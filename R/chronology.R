chronology <- function(
  dat,
  details = FALSE,
  nparam = NULL,
  test = FALSE,
  minclutch = TRUE,
  rel.flex = c(0, 0),
  nact = TRUE,
  lsc = NULL
) {
  dat <- dat[order(dat$nest_id, dat$date, dat$visit_id), ] # added 2012-11-06 because only using the chronology function gave different results than active.nest with unordered data (which is presumably done within active.nest)
  if (is.data.table(dat)) {
    dat <- as.data.frame(dat)
  } #maybe not useful check in active.nest
  #if(!all(sort(dat$date)==dat$date)){return(c(NA,NA,"weird dates or visit id and dates not matching"))} # this thing here needs the visit_id column in the original data check nest 322498
  ### associating status code with stage code
  #browser()
  if (is.null(lsc)) {
    lsc <- list(
      nest = c("AB", "FB", "MB", "PB", "N1", "N2", "N3", "N4", "NC", "NL"),
      laying = c("FR"),
      #incubation=c("DE","WA"),    # check nest 66546
      hatch = c("HA", "PI"),
      fledge = c("RF", "SL", "SY", "YC", "LB", "ON", "EX")
    )
  }
  if (!is.null(lsc)) {
    dat$stage_code <- sub_list(dat$status_code, lsc)
  }
  ###

  #browser()

  dat$stage_code <- id_stage(dat, nact = nact)
  #print(subset(dat,select=c(eggs,young,stage_code)))
  rownames(dat) <- 1:nrow(dat) # to create stage_code in dattest
  dattest <- dat # to have the stage codes for the test version
  #print(dat)####################################################################################

  #setting the upperbound from nest observations
  if (any(dat$stage_code == "nest")) {
    temp <- as.Date(max(dat$date[dat$stage_code == "nest"]))
    wtemp <- which(dat$date == temp)
    if (any(dat$stage_code[wtemp] != "nest")) {
      # for when on a given day there is a nest observation and an egg observation
      upperbound <- temp
    } else {
      upperbound <- temp + 1
    }
  } else {
    upperbound <- NULL
  }

  #browser()

  dat <- dat[!dat$stage_code %in% c("nest", "unknown"), ]
  if (nrow(dat) == 0) {
    return(c(NA, NA, "no eggs or young", NA, NA))
  }
  #browser()
  ## info on nesting parameters
  if (
    !all(
      c("clutch", "cmin", "cmax", "incub", "start", "rearing", "laying") %in%
        names(dat)
    )
  ) {
    #print("11111111")
    #species<-read.csv("species2precocial.csv",header=T,stringsAsFactors=FALSE)
    species <- nestingparameters
    if (!is.null(nparam)) {
      species <- nparam
    }
    spinfo <- which(species$sp == dat$species_code[1])
    if (length(spinfo) != 1) {
      return(c(
        NA,
        NA,
        "species not in database or multiple species codes matching",
        NA,
        NA
      ))
    }
    if (is.na(species[spinfo, "clutch"])) {
      return(c(NA, NA, "no info for this species", NA, NA))
    }
    if (
      any(is.na(species[
        spinfo,
        c("clutch", "cmin", "cmax", "incub", "start", "rearing", "laying")
      ]))
    ) {
      return(c(NA, NA, "some missing info", NA, NA))
    }
    rname <- rownames(dat) # keep rownames to fit with dattest cause merge renames ans sorts
    #browser()
    dat <- merge(
      dat,
      species[spinfo, ],
      by = "species_code",
      all = TRUE,
      sort = FALSE
    )
    rownames(dat) <- rname # keep rownames
  } else {
    species <- dat[
      1,
      c("clutch", "cmin", "cmax", "incub", "start", "rearing", "laying")
    ]
    spinfo <- 1
  }
  unknown <- FALSE
  #print(dat)
  if (any(c(dat$eggs, dat$young) < 0)) {
    if (all(c(dat$eggs, dat$young) %in% c(0, -1))) {
      unknown <- TRUE
    }
    predat <- unknown_nbeggs(dat)
    dat <- predat$dat
    unknown <- predat$unknown
  }
  #print(dat)
  if (is.character(dat)) {
    return(c(NA, NA, dat, NA, NA))
  }
  #careful! adjust egg start for case where found in laying with uncompleted clutches and maybe give the modal clutch?
  dat$date <- as.Date(dat$date)
  #browser()
  # from eggs observations, save only lines with status codes meaning laying

  #browser()

  if (!is.null(lsc)) {
    # this to keep lines with laying status_codes in the eggs observations because they will be ordered first
    temp_ordre <- sub_list(dat$status_code, lsc)
    temp_ordre <- ifelse(
      !is.na(temp_ordre),
      ifelse(temp_ordre != "laying", NA, temp_ordre),
      NA
    )
    if (any(which(temp_ordre == "laying"))) {
      dat <- dat[order(dat$date, dat$visit_id, temp_ordre, na.last = TRUE), ]
    }
  }

  #save only higher order observations
  ordre <- c("nest", "active", "eggs", "young", "hatch", "fledge")
  order_stage <- match(dat$stage_code, ordre)
  dat <- cbind(dat, order_stage, stringsAsFactors = FALSE)
  dat <- dat[order(dat$date, -dat$order_stage), ]
  dat <- dat[!duplicated(dat$date), ]
  dat <- dat[
    !duplicated(
      dat$stage_code,
      incomparables = c("nest", "active", "eggs", "young", "hatch")
    ),
  ] # to keep only the first date with code fledge
  if (nact) {
    progobs <- max(apply(
      matrix(c(dat$eggs, dat$young), ncol = 2),
      1,
      function(i) {
        #print(i)
        if (i[1] == (-1) && i[2] == (-1)) {
          1
        } else {
          ifelse(identical(i[1], -1), 1, i[1]) +
            ifelse(identical(i[2], -1), 1, i[2])
        }
      }
    ))
    #print(progobs)
  } else {
    progobs <- max(
      ifelse(dat$eggs == (-1), 1, dat$eggs) +
        ifelse(dat$young == (-1), 1, dat$young),
      na.rm = TRUE
    )
  }
  if (
    !unknown |
      progobs > dat$clutch[1] |
      any(apply(
        subset(
          dat,
          dat$stage_code %in% c("hatch", "young", "fledge"),
          select = c(eggs, young)
        ),
        1,
        function(i) {
          all(i >= 0)
        }
      ))
  ) {
    prog <- progobs
  } else {
    prog <- dat$clutch[1]
  }
  if (minclutch & prog < dat$cmin[1] & all(dat$stage_code == "eggs")) {
    lran <- range(dat$date[dat$stage_code == "eggs"])
    #oe<-unique(dat$eggs[dat$stage_code=="eggs"])
    #if(length(oe)==1 & min(oe)<dat$cmin[1]

    lran <- as.Date(range(dat$date[
      dat$stage_code == "eggs" & dat$eggs == prog
    ]))
    if ((abs(as.numeric(difftime(lran[1], lran[2]))) + 1) <= dat$laying[1]) {
      prog <- dat$clutch[1]
    }
  }

  if (!is.null(lsc)) {
    ### add 2013-11-07 for fresh eggs code (FR) and laying status codes
    codes <- sub_list(dat$status_code, lsc) ### if the last observation is a fresh egg, completes the laying sequence up to the modal clutch and changes prog
    wcodes <- which(codes == "laying") ### if a species has 3 fresh eggs and a laing interval more than 1 days, problem it won't go up to the modal clutch, but really difficult to consider
    if (any(wcodes)) {
      nb <- dat$eggs[max(wcodes)]
      if (nb == prog & max(wcodes) == nrow(dat)) {
        prog <- max(dat$clutch[1], prog) # chooses the max if fresh egg not the max
      }
    }
  }

  #browser()

  if (dat$start[1] == 999) {
    egg_start <- prog
  }
  if (dat$start[1] == 998) {
    egg_start <- prog - 1
  }
  if (dat$start[1] < 998) {
    egg_start <- min(dat$start[1], prog)
  } #edit pour une BANO avec 1 de prog
  #if(dat$start[1]==999){egg_start<-max(ifelse(dat$eggs==(-1),1,dat$eggs)+ifelse(dat$young==(-1),1,dat$young),na.rm=T)}
  #if(dat$start[1]==998){egg_start<-max(ifelse(dat$eggs==(-1),1,dat$eggs)+ifelse(dat$young==(-1),1,dat$young),na.rm=T)-1}
  #if(dat$start[1]<998){egg_start<-dat$start[1]}
  egg_start <- ifelse(egg_start <= 0, 1, egg_start)
  dat <- data.frame(
    dat,
    egg_start = rep(egg_start, nrow(dat)),
    stringsAsFactors = FALSE
  )
  #if more than one visit in the same day, program here to keep the latest (or something else)
  #maximum number of progeny

  lay <- (dat$egg_start[1] - 1) * dat$laying[1]
  inc <- dat$incub[1]
  egg <- lay + inc # for nests that only have unknown numbers
  rear <- dat$rearing[1] # attention ce rearing est dans le species info il ne faut pas le changer avec la confusion possible avec le young

  ######when nests are only considered active, maube put in id_stage fucntion
  #dattest$stage_code[which(dattest$eggs<0 & dattest$young<0 & dattest$stage_code!="unknown")]<-"active"
  #browser()
  #egg stage id
  weggs <- which(dat$stage_code == "eggs")
  if (any(weggs) && !unknown) {
    temp <- dat$eggs[weggs]
    dat$stage_code[weggs] <- egg_stage_id(x = temp, beg = dat$egg_start[1])
  }
  # this part to get modified stage_code from dat to dattest
  mat <- match(rownames(dattest), rownames(dat))
  dattest$stage_code[ifelse(!is.na(mat), TRUE, FALSE)] <- dat$stage_code[mat[
    !is.na(mat)
  ]]
  #
  # gather observations
  layingdetect <- FALSE
  layobs <- if (unknown) {
    list(period = NULL, beg_end = NULL)
  } else {
    stage_obs(
      dat,
      period = "laying",
      prog = prog,
      upperbound = upperbound,
      lsc = lsc
    )
  }
  incobs <- stage_obs(
    dat,
    period = "incubation",
    prog = prog,
    unknown = unknown,
    lsc = lsc
  )
  eggobs <- stage_obs(dat, period = "eggs", prog = prog, lsc = lsc)
  rearobs <- stage_obs(dat, period = "young", prog = prog, lsc = lsc)
  if (!is.null(layobs$period)) {
    layingdetect <- TRUE
  }

  #browser()

  #fit incubation and laying
  winc <- which(names(layobs$period) == "incubation")
  if (any(winc)) {
    incobs$period <- c(layobs$period[winc], incobs$period)
    incobs$beg_end <- c(layobs$beg_end[winc], incobs$beg_end)
    incobs <- lapply(incobs, function(i) {
      i[!duplicated(incobs$period)]
    })
    incobs <- lapply(incobs, function(i) {
      i[order(incobs$period)]
    })
    incobs$period <- c(incobs$period, filldate(incobs$period))
    incobs$beg_end <- c(
      incobs$beg_end,
      rep("U", length(filldate(incobs$period)))
    )
    incobs <- lapply(incobs, function(i) {
      i[!duplicated(incobs$period)]
    })
    if (winc[1] == 1) {
      layobs$period <- NULL
      layobs$beg_end <- NULL
    } else {
      layobs$period <- layobs$period[-winc]
      layobs$beg_end <- layobs$beg_end[-winc]
    }
  }
  #print("hello")
  #add probablement mettre ce passage dans la fonction stage_obs ferait plus de sens
  # pour ajouter une journ\u00e9e d'incubation avant l'atteinte du prog si L,incubation d\u00e9bute avant le dernier oeuf
  if (
    !layingdetect &&
      dat$start[1] < 999 &&
      !is.null(incobs$period) &&
      !unknown &&
      prog > 1
  ) {
    y <- length(sort(rep(egg_start:(prog - 1), dat$laying[1])))
    if (!is.null(upperbound)) {
      dy <- span(upperbound, incobs$period[1])
      y <- min(dy - 1, y)
    }
    incobs$period <- filldate(c(
      as.Date(incobs$period[1], origin = "1970-01-01") - y,
      as.Date(incobs$period, origin = "1970-01-01")
    ))
    incobs$beg_end <- c(rep("U", y), incobs$beg_end)
  }

  if (!layingdetect && !is.null(eggobs$period) && unknown) {
    # 2011-10-04 v\u00e9rifier les unknown, eggobs vs incobs avec le r\u00e9sultats du unknown***
    w <- which(dat$eggs != 0 & dat$stage_code == "eggs") #ici c'\u00e9tait un dat$eggs>0 pr\u00e9 5 mars 2012
    if (any(w)) {
      date2 <- dat$date[w[1]]
      names(date2) <- names(eggobs$period)[1]
      y <- (dat$eggs[w[1]] - 1) * dat$laying[1]
      if (!is.null(upperbound)) {
        dy <- span(upperbound, eggobs$period[1])
        y <- min(dy - 1, y)
      }
      if (abs(as.numeric(difftime(date2, eggobs$period[1]))) < y) {
        eggobs$period <- filldate(c(date2 - y, eggobs$period))
        eggobs$beg_end <- c(rep("U", y), eggobs$beg_end)
      }
    }
  }

  #endadd

  if (!all(dat$stage_code == "active")) {
    lobs <- list(
      period = as.Date(
        c(layobs$period, incobs$period, eggobs$period, rearobs$period),
        origin = "1970-01-01"
      ),
      beg_end = c(
        layobs$beg_end,
        incobs$beg_end,
        eggobs$beg_end,
        rearobs$beg_end
      )
    )
    lon <- length(lobs$period)
  }
  #modifying nesting parameters according to observations

  #browser()

  # lay adjustement  #see ex6.txt and maybe modify here
  if (layingdetect) {
    lay <- length(layobs$period) # this will be ok even if there is an upperbound cause it is considered in the laying function
  } else {
    if (!is.null(upperbound)) {
      if (!is.null(incobs$period)) {
        temp <- span(incobs$period[1], upperbound) - 1
        if (lay > temp) {
          lay <- temp
        }
      } else {
        if (!is.null(rearobs$period)) {
          temp <- span(rearobs$period[1], upperbound) - 1
          if (lay > temp) {
            lay <- temp
          }
        }
      }
    }
  }

  if (all(c("B", "E") %in% incobs$beg_end) | inc < length(incobs$period)) {
    inc <- length(incobs$period)
  }
  if (all(c("B", "E") %in% rearobs$beg_end) | rear < length(rearobs$period)) {
    rear <- length(rearobs$period)
  }
  if (all(egg < length(eggobs$period))) {
    egg <- length(eggobs$period)
    inc <- egg - lay
  }
  egg <- inc + lay #rajouter le 5 mars 2012 because of nest 642382

  if (!is.null(incobs$period) && !is.null(rearobs$period)) {
    temp <- span(c(incobs$period, rearobs$period))

    if (temp > (inc + rear)) {
      disp <- dispatch(
        inct = dat$incub[1],
        reart = dat$rearing[1],
        incmin = length(incobs$period),
        rearmin = length(rearobs$period),
        increar = temp,
        rel.flex = rel.flex
      )
      inc <- disp[1]
      rear <- disp[2]
    }
  }

  # the following part to adjust nesting periods when a nest stage code is given
  if (!is.null(upperbound)) {
    if (any(dat$stage_code %in% c("hatch", "young", "fledge"))) {
      #print(upperbound+lay)
      #print(min(dat$date[dat$stage_code%in%c("young","fledge")]))
      temp <- abs(as.numeric(difftime(
        upperbound + lay,
        min(dat$date[dat$stage_code %in% c("hatch", "young", "fledge")])
      )))
      if (inc > temp) {
        inc <- temp
      }
      temp <- abs(as.numeric(difftime(
        upperbound,
        min(dat$date[dat$stage_code %in% c("hatch", "young", "fledge")])
      )))
      if (egg > temp) {
        egg <- temp
      }
    }

    if (any(dat$stage_code == "fledge")) {
      temp <- abs(as.numeric(difftime(
        upperbound + lay,
        min(dat$date[dat$stage_code == "fledge"])
      ))) +
        1
      if ((inc + rear) != (temp)) {
        if (!any(dat$stage_code == "incubation")) {
          incmin <- 0
        } else {
          #incmin<-abs(as.numeric(difftime(upperbound+lay,max(dat$date[dat$stage_code=="incubation"]))))+1
          incmin <- span(incobs$period)
        }
        rearmin <- abs(as.numeric(difftime(
          min(dat$date[dat$stage_code %in% c("hatch", "young", "fledge")]),
          max(dat$date[dat$stage_code %in% c("hatch", "young", "fledge")])
        ))) +
          1
        if (layingdetect || temp < inc + rear) {
          disp <- dispatch(
            inct = dat$incub[1],
            reart = dat$rearing[1],
            incmin = incmin,
            rearmin = rearmin,
            increar = temp,
            rel.flex = rel.flex
          )
        } else {
          disp <- dispatch(
            inct = dat$incub[1],
            reart = dat$rearing[1],
            incmin = incmin,
            rearmin = rearmin,
            increar = inc + rear,
            rel.flex = rel.flex
          )
        }
        inc <- disp[1]
        rear <- disp[2]
        egg <- lay + inc
      } else {
        temp <- abs(as.numeric(difftime(
          upperbound + lay + inc - 1,
          min(dat$date[dat$stage_code == "fledge"])
        )))
        if (rear > temp) {
          rear <- temp
        }
        if (rear == 0 & dat$rearing[1] > 0) {
          return(c(
            NA,
            NA,
            "nb of days for young for this altricial species is < 1 day",
            NA,
            NA
          ))
        }
      }
    }
  }

  if (any(dat$stage_code == "active")) {
    emin <- span(dat$date[which(dat$stage_code == "eggs")])
    ymin <- span(dat$date[which(
      dat$stage_code %in% c("hatch", "young", "fledge")
    )])
    if (egg < emin) {
      egg <- emin
    }
    inc <- egg - lay
    if (rear < ymin) {
      rear <- ymin
    }

    if (
      all(dat$stage_code == "active") |
        all(dat$stage_code %in% c("eggs", "active")) |
        all(dat$stage_code %in% c("young", "active"))
    ) {
      lact <- span(dat$date)
      if (lact > (lay + inc + rear)) {
        disp <- dispatch(
          inct = dat$incub[1],
          reart = dat$rearing[1],
          incmin = max(emin - lay, 0),
          rearmin = ymin,
          increar = lact - lay,
          rel.flex = rel.flex
        )
        inc <- disp[1]
        egg <- inc + lay
        rear <- disp[2]
      }
      dif <- lay + inc + rear - lact
      x1 <- dat$date[1] - ceiling(dif / 2)
      if (!is.null(upperbound) && upperbound > x1) {
        x1 <- upperbound
      }
      x2 <- x1 + lay + inc + rear - 1
      active <- seq.Date(x1, x2, by = 1)
      names(active) <- c(
        rep("laying", lay),
        rep("incubation", inc),
        rep("young", rear)
      )

      #if egg observations overflow
      weggs <- which(dat$stage_code == "eggs")
      if (any(weggs)) {
        overeggs <- dat$date[max(weggs)]
        maxinc <- active[max(which(names(active) == "incubation"))]
        if (overeggs > maxinc) {
          active <- active + (span(overeggs, maxinc) - 1)
        }
      }

      #if young observations overflow
      wyoung <- which(dat$stage_code %in% c("hatch", "young", "fledge"))
      if (any(wyoung)) {
        overyoung <- dat$date[min(wyoung)]
        minyou <- active[min(which(
          names(active) %in% c("hatch", "young", "fledge")
        ))]
        if (overyoung < minyou) {
          active <- active - (span(overyoung, minyou) - 1)
        }
      }
      test.dat <- testchronology(
        info = species[spinfo, ],
        data = list(dat, dattest),
        active = active,
        layingdetect = layingdetect
      )
      if (test) {
        return(test.dat)
      } ###### temp test mode
      if (details) {
        return(active)
      } else {
        return(c(
          range(active),
          NA,
          test.dat$len_active[1],
          test.dat$span_error[1]
        ))
      }
    } else {
      dat <- dat[!dat$stage_code == "active", ]
    }
  }

  # create sequence and phenology
  l <- if (!unknown) {
    1:(lay + inc + rear)
  } else {
    1:(egg + rear)
  }
  names(l) <- if (!unknown) {
    c(rep("laying", lay), rep("incubation", inc), rep("young", rear))
  } else {
    c(rep("eggs", egg), rep("young", rear))
  }

  #browser()

  if (layingdetect) {
    #potential problem with too long incubation+rearing period (too big gap between two observations, but no detection of too long period for any period)
    x1 <- lobs$period[1]
    incmin <- length(incobs$period)
    rearmin <- length(rearobs$period)
    if (!is.null(rearobs$period)) {
      if (is.null(incobs$period)) {
        #rajout de ce ifelse car si on ne reach pas l'incubation pendant le laying et que incobs est vide m\u00eame s'il y a laying d\u00e9tect\u00e9
        inc <- 0
        temprear <- span(max(layobs$period) + 1, rearobs$period)
        if (temprear > rear) {
          rear <- temprear
        } else {
          if (any(rearobs$beg_end == "E") && temprear < rear) {
            rear <- temprear
          }
        }
      } else {
        #print("what")
        inc <- min(span(incobs$period[1], rearobs$period[1] - 1), inc)
        if (
          any(rearobs$beg_end == "E") ||
            span(min(incobs$period), max(rearobs$period)) > inc + rear
        ) {
          #if(rearobs$beg_end[length(rearobs$period)]=="E"){
          increar <- span(incobs$period[1], max(rearobs$period))
          #}else{
          #increar<-inc+rear
          #}
          disp <- dispatch(
            inct = dat$incub[1],
            reart = dat$rearing[1],
            incmin = incmin,
            rearmin = rearmin,
            increar = increar,
            rel.flex = rel.flex
          )
          inc <- disp[1]
          rear <- disp[2]
        }
      }
    }
    x2 <- x1 + lay + inc + rear - 1
    active <- seq.Date(x1, x2, by = 1)
    #overlap<-length(intersect(seq.Date(x1,x1+lay+inc-1,by=1),rearobs$period)) # vieux overlap!!! if incubation overlaps rearing, this "buffs" incubation
    #overlap<-length(which(seq.Date(x1,x1+lay+inc-1,by=1)>=rearobs$period[1]))
    #if(overlap>=inc){return(c(NA,NA,"possible renesting or inconsistent observations"))}
    #x2<-if(lobs$beg_end[lon]=="E"){lobs$period[lon]}else{max(lobs$period[lon],x1+length(l)-1-overlap)}
    #active<-seq.Date(x1,x2,by=1)
    #nbrear<-as.numeric(difftime(x2,x1))+1-lay-inc+overlap
    #if(nbrear<1){return(c(NA,NA,"possible renesting or inconsistent observations"))}
    #increar<-inc-overlap+nbrear
    #print(lobs)
    #print(increar)
    #print(inc)
    #print(overlap)
    #print(nbrear)
    #inc<-ifelse(increar>inc+rear,round(increar*(inc/(inc+rear))),inc-overlap) #to make period augmentation proportional in both period
    #inc<-ifelse(increar>inc+rear,dispatch(inct=dat$incub[1],reart=dat$rearing[1],incmin=inc,rearmin=rear,increar=increar)[1],inc-overlap) # 2011-10-07 new version with dispatch
    #rear<-increar-inc
    names(active) <- c(
      rep("laying", lay),
      rep("incubation", inc),
      rep("young", rear)
    )
    test.dat <- testchronology(
      info = species[spinfo, ],
      data = list(dat, dattest),
      active = active,
      layingdetect = layingdetect
    )
    if (test) {
      return(test.dat)
    } ###### temp test mode
    if (details) {
      return(active)
    } else {
      return(c(
        range(active),
        NA,
        test.dat$len_active[1],
        test.dat$span_error[1]
      ))
    }
  }
  m <- match("E", lobs$beg_end)
  estage <- if (unknown) {
    "eggs"
  } else {
    "incubation"
  }

  if (!is.na(m)) {
    if (names(lobs$period)[m] == estage) {
      x1 <- lobs$period[m] - ifelse(unknown, egg, lay + inc) + 1
      if (!is.null(upperbound)) {
        if (upperbound > x1) {
          x1 <- upperbound
        }
      }
      x2 <- if (lobs$beg_end[lon] == "E" && names(lobs$period)[m] == "young") {
        lobs$period[m]
      } else {
        x1 + length(l) - 1
      }
      active <- seq.Date(x1, x2, by = 1)
      names(active) <- c(
        rep("laying", lay),
        rep("incubation", inc),
        rep("young", length(active) - lay - inc)
      )
    }
    if (names(lobs$period)[m] == "young") {
      ww <- which(names(lobs$period) == estage)
      if (any(ww)) {
        ww <- ww[length(ww)]
        ll <- length(seq.Date(lobs$period[ww] + 1, lobs$period[m], by = 1))
        if (ll < rear) {
          rear <- ll
        }
        increar <- abs(as.numeric(difftime(
          min(lobs$period),
          max(lobs$period)
        ))) +
          1 -
          ifelse(unknown, lay, 0)
        #print(inc)
        #print(rear)
        #if(!is.null(upperbound) & increar<(inc+rear)){
        #     inct<-dat$incub[1]
        #     reart<-dat$rearing[1]
        #     incmin<-length(ww)-ifelse(unknown,lay,0)
        #     rearmin<-length(which(names(lobs$period)=="young"))
        #     disp<-dispatch(inct=inct,reart=reart,incmin=incmin,rearmin=rearmin,increar=increar)
        #     inc<-disp[1]
        #     rear<-disp[2]
        #     egg<-lay+inc
        #     }
      }
      x1 <- lobs$period[m] - ifelse(unknown, egg, lay + inc) - rear + 1
      if (!is.null(upperbound)) {
        if (upperbound > x1) {
          x1 <- upperbound
        }
      }
      x2 <- lobs$period[m]
      active <- seq.Date(x1, x2, by = 1)
      names(active) <- c(
        rep("laying", lay),
        rep("incubation", inc),
        rep("young", rear)
      )
    }
    test.dat <- testchronology(
      info = species[spinfo, ],
      data = list(dat, dattest),
      active = active,
      layingdetect = layingdetect
    )
    if (test) {
      return(test.dat)
    } ###### temp test mode
    if (details) {
      return(active)
    } else {
      return(c(
        range(active),
        NA,
        test.dat$len_active[1],
        test.dat$span_error[1]
      ))
    }
  }
  if (is.na(m)) {
    if (identical(unique(names(lobs$period)), c(estage, "young"))) {
      #if(all(unique(names(lobs$period))==c(estage,"young"))){  #old version
      #L<-as.numeric(difftime(max(lobs$period),min(lobs$period)))+1  #old version
      L <- span(lobs$period)
      if (L <= ifelse(unknown, egg + rear, inc + rear)) {
        if (!unknown) {
          x1 <- as.Date(center(
            iobs = incobs$period,
            robs = rearobs$period,
            itheo = inc,
            rtheo = rear
          )) -
            lay
        } else {
          x1 <- as.Date(center(
            iobs = eggobs$period,
            robs = rearobs$period,
            itheo = egg,
            rtheo = rear
          ))
          inc <- egg - lay
        }
        if (!is.null(upperbound)) {
          if (upperbound > x1) {
            x1 <- upperbound
          }
        }
        x2 <- x1 + length(l) - 1
        active <- seq.Date(x1, x2, by = 1)
        names(active) <- c(
          rep("laying", lay),
          rep("incubation", inc),
          rep("young", rear)
        )
      } else {
        x1 <- min(lobs$period) - ifelse(unknown, 0, lay)
        if (!is.null(upperbound)) {
          if (upperbound > x1) {
            x1 <- upperbound
          }
        }
        x2 <- max(lobs$period)
        active <- seq.Date(x1, x2, by = 1)
        if (estage == "incubation") {
          nbinc <- as.integer(round(
            L * (dat$incub[1] / (dat$rearing[1] + dat$incub[1])),
            0
          ))
          nbrear <- L - nbinc # this assumes that within the more lengthy inc+rear period, the proportion of both periods is proportional to the theoretical one
        } else {
          nbinc <- as.integer(round(
            (L - lay) * (dat$incub[1] / (dat$rearing[1] + dat$incub[1])),
            0
          )) # this might contradict observations if hatching is not between the observed periods, maybe adjust with lobs
          nbrear <- (L - lay) - nbinc #here, laying is assumed to take the normal amount of time despite other periods being abnormally long
        }
        if (inc > nbinc) {
          nbrear <- nbrear - (inc - nbinc)
          nbinc <- inc
        }
        if (rear > nbrear) {
          nbinc <- nbinc - (rear - nbrear)
          nbrear <- rear
        }

        names(active) <- c(
          rep("laying", lay),
          rep("incubation", nbinc),
          rep("young", nbrear)
        )
      }
    }
    if (all(unique(names(lobs$period)) %in% estage)) {
      ranobs <- range(lobs$period[names(lobs$period) == estage])
      midobs <- median(ranobs)
      mid <- floor(median(l[names(l) == estage]))
      cons <- ifelse(
        (as.numeric(difftime(ranobs[2], ranobs[1])) + 1) %%
          2 &&
          !ifelse(unknown, lay + inc, inc) %% 2,
        0,
        1
      ) #to choose earliest when not central
      x1 <- midobs - (mid - cons)
      if (!is.null(upperbound)) {
        if (upperbound > x1) {
          x1 <- upperbound
        }
      }
      x2 <- x1 + length(l) - 1
      active <- seq.Date(x1, x2, by = 1)
      names(active) <- c(
        rep("laying", lay),
        rep("incubation", inc),
        rep("young", rear)
      ) # <a verifier si ca marche pour un cas d'egg inconnu et que la longueur de inc fit avec les obs (voir la separation des deux cas dans le centering
    }
    if (all(unique(names(lobs$period)) %in% c("young"))) {
      ranobs <- range(lobs$period[names(lobs$period) == "young"])
      midobs <- median(ranobs)
      mid <- floor(median(l[names(l) == "young"]))
      cons <- ifelse(
        (as.numeric(difftime(ranobs[2], ranobs[1])) + 1) %% 2 && !rear %% 2,
        0,
        1
      ) #to choose earliest when not central
      x1 <- midobs - (mid - cons)
      if (!is.null(upperbound)) {
        if (upperbound > x1) {
          x1 <- upperbound
        }
      }
      x2 <- x1 + length(l) - 1
      active <- seq.Date(x1, x2, by = 1)
      names(active) <- c(
        rep("laying", lay),
        rep("incubation", inc),
        rep("young", rear)
      )
    }
  }
  test.dat <- testchronology(
    info = species[spinfo, ],
    data = list(dat, dattest),
    active = active,
    layingdetect = layingdetect
  )
  if (test) {
    return(test.dat)
  } ###### temp test mode
  if (details) {
    return(active)
  } else {
    return(c(
      range(active),
      NA,
      test.dat$len_active[1],
      test.dat$span_error[1]
    ))
  }
}
