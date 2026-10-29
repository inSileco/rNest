laying <- function(dat, prog, result = TRUE, upperbound = NULL, lsc = lsc) {
  if (!any(dat$stage_code %in% c("eggs", "laying", "incubation"))) {
    if (result) {
      return(list(period = c(period = NULL), beg_end = c(NULL)))
    } else {
      NULL
    }
  }

  if (prog == 1) {
    codes <- sub_list(dat$status_code, lsc)
    wcodes <- which(codes == "laying")
    if (any(wcodes)) {
      temp <- dat$date[wcodes[1]]
      if (result) {
        return(list(period = c(incubation = as.Date(temp)), beg_end = c("B")))
      } else {
        c(incubation = as.Date(temp))
      }
    }
  }

  w <- which(
    dat$eggs == prog & dat$stage_code %in% c("eggs", "laying", "incubation")
  ) #version originale pr\u00e9 2012-02-07
  #w<-which((dat$eggs==prog & dat$stage_code%in%c("eggs","laying","incubation")) | dat$stage_code%in%c("young","fledge","incubation")) # version post 2012-02-07
  ww <- which(dat$stage_code %in% c("eggs", "laying", "incubation"))
  if (any(w)) {
    ww <- ww[ww <= w[1]]
  }

  if (!any(ww)) {
    if (result) {
      return(list(period = c(period = NULL), beg_end = c(NULL)))
    } else {
      NULL
    }
  }
  if (length(ww) == 1 && any(w)) {
    if (ww == w[1]) {
      if (!is.null(lsc)) {
        if (!any(which(sub_list(dat$status_code[ww], lsc) == "laying"))) {
          if (result) {
            return(list(period = c(period = NULL), beg_end = c(NULL)))
          } else {
            NULL
          }
        }
      }
    }
  }

  obs <- dat$date[ww]
  names(obs) <- dat$eggs[ww]
  eth <- c(sort(rep(1:(prog - 1), dat$laying[1])), prog)
  val <- as.numeric(unique(names(obs)))
  if (length(val) == 1) {
    ntimes <- (as.numeric(max(obs) - min(obs)) + 1) - dat$laying[1]
    if (ntimes > 0) {
      eth <- sort(c(eth, rep(val, ntimes)))
    }
  }
  # watch out si on a une seule valeur d'oeuf en laying et que l'Obs est inf\u00e9reuiure en dur\u00e9e au laying interval et que l'incubation survient rop rapidement, ceci ira dans le pas de fit de th et je ne sais pas ce que ca va donner tdans le th1 th2
  # watch out lorsqu'il n'y a qu'une seule valeur mais que la p\u00e9riode entre cette valeur et l'incubation est trop courte ex 1037307 \u00e0 modifier absolument
  bloc <- which(dat$stage_code %in% c("hatch", "young", "fledge"))
  bloc <- if (any(bloc)) {
    dat$date[bloc[1]]
  } else {
    NA
  }

  incomplete <- FALSE
  if (!is.na(bloc)) {
    wmax <- max(which(eth == names(obs)[length(obs)]))
    aj <- unique(c(1:wmax, wmax:(wmax + span(max(obs), bloc) - 2)))
    if (length(aj) < length(eth)) {
      eth <- eth[aj]
      incomplete <- TRUE
    }
  }

  www <- which(eth == names(obs)[1])
  temp <- obs[1] - www[length(www)] + 1

  if (!is.null(upperbound)) {
    if (temp <= upperbound) {
      th <- seq.Date(upperbound, along.with = eth, by = 1)
    } else {
      th <- seq.Date(temp, along.with = eth, by = 1)
    }
  } else {
    th <- seq.Date(temp, along.with = eth, by = 1)
  }
  names(th) <- eth
  deb <- NULL
  # && ifelse(is.na(bloc),TRUE,max(th)<bloc)
  while (th[1] <= obs[1]) {
    ss <- sapply(obs, function(i) {
      m <- which(th == i)
      if (any(m)) {
        m
      } else {
        NA
      }
    })
    if (any(w) && is.na(ss[length(ss)])) {
      ss <- ss[-length(ss)]
    }
    if (any(is.na(ss))) {
      # c'est pour corriiger si la s\u00e9quence d'observation contient le prog car dans ce cas le NA est \u00e0 ignorer
      th <- th + 1
      next
    }
    if (all(names(obs)[1:length(ss)] == names(th)[ss])) {
      # pour adapter la longuer de obs en fonction de ss
      deb <- c(deb, as.character(th[1]))
    }
    th <- th + 1
  }
  #browser()
  if (!is.null(deb)) {
    ans <- seq.Date(median(as.Date(deb)), length.out = length(th), by = 1)
    names(ans) <- c(
      rep("laying", length(ans) - 1),
      ifelse(incomplete, "laying", "incubation")
    )

    beginc <- which(eth >= dat$egg_start[1])[1]
    if (!is.na(beginc)) {
      if (result) {
        names(ans)[beginc:length(ans)] <- "incubation"
      }
    }
  } else {
    obs1 <- obs[1]
    #if(length(obs)==1 & ifelse(any(w),1,0)==1){browser()}
    obs2 <- obs[length(obs) - ifelse(any(w) & length(obs) > 1, 1, 0)] # because fresh eggs make prog and laying together check nest_id 404015
    #if(length(obs2)==0){browser()}
    difobs <- span(obs2, obs1)
    difth <- (as.numeric(names(obs2)) - as.numeric(names(obs1)) + 1) *
      dat$laying[1]
    eth1 <- eth[eth <= as.numeric(names(obs1))]
    eth2 <- eth[eth >= as.numeric(names(obs2))]
    th1 <- seq.Date(from = obs1 - length(eth1) + 1, along.with = eth1, by = 1)
    th2 <- seq.Date(
      from = obs[length(obs)] - ifelse(any(w), length(eth2) - 1, 0),
      along.with = eth2,
      by = 1
    ) # le ifelse c'est pour s'il n'y a pas de prog dans la s\u00e9qence d'oeufs, le th2 commence avec le d\u00e9but \u00e0 la fin de s\u00e9quence des observations au lieu de partir du prog
    #cas si une seule valeur et cas si on commence par un 1 ou si laying interval est un 1
    names(th1) <- eth1
    names(th2) <- eth2
    nbmatch1 <- nbmatch2 <- NULL
    deb1 <- deb2 <- NULL
    ss1 <- names(th1)[th1 %in% intersect(th1, obs)] ==
      names(obs)[obs %in% intersect(th1, obs)]

    while (all(ss1)) {
      deb1 <- c(deb1, as.character(th1[1]))
      nbmatch1 <- c(nbmatch1, length(ss1))
      th1 <- th1 + 1
      ss1 <- names(th1)[th1 %in% intersect(th1, obs)] ==
        names(obs)[obs %in% intersect(th1, obs)]
    }
    ss2 <- names(th2)[th2 %in% intersect(th2, obs)] ==
      names(obs)[obs %in% intersect(th2, obs)]
    while (all(ss2)) {
      deb2 <- c(deb2, as.character(th2[length(th2)]))

      nbmatch2 <- c(
        nbmatch2,
        length(intersect(th2[names(th2) != as.character(prog)], obs))
      )
      #print(deb2)
      #print(nbmatch2)
      #print(th2)
      #cat("\n","\n")

      if (
        (length(th2) == 1 && length(deb2) == 1) ||
          (length(th2) == nbmatch2[length(nbmatch2)])
      ) {
        break
      } # check bug test_20120209_094244 et suivant
      th2 <- th2 - 1
      ss2 <- names(th2)[th2 %in% intersect(th2, obs)] ==
        names(obs)[obs %in% intersect(th2, obs)]
      #if(length(nbmatch2)>50){print(dat)}
    }
    # on conserve les positions pour lesquelles on a un nombre maximum de match dans les observations d'oeufs

    deb1 <- deb1[nbmatch1 == max(nbmatch1)]
    deb2 <- if (is.null(nbmatch2) && is.null(deb2)) {
      th2[length(th2)]
    } else {
      deb2[nbmatch2 == max(nbmatch2)]
    } # si il y a une pndaison trop rapide entre le dernier oeuf et les pr\u00e9c\u00e9dents dans la s\u00e9quence finaled

    if (difobs > difth) {
      # ans<-seq.Date(as.Date(ifelse(names(obs1)=="1",min(deb1),max(deb1))),as.Date(ifelse((span(max(obs),bloc)-2)==0,max(deb2),min(deb2))),by=1)    # used to be ifelse(names(obs1)=="1",min(deb1),max(deb1)) but don't remember why
      ans <- seq.Date(
        as.Date(ifelse(names(obs1) == "1", min(deb1), max(deb1))),
        as.Date(min(deb2)),
        by = 1
      )
    } else {
      ans <- seq.Date(as.Date(min(deb1)), as.Date(max(deb2)), by = 1)
    }

    names(ans) <- c(
      rep("laying", length(ans) - 1),
      ifelse(incomplete, "laying", "incubation")
    )

    if (result) {
      th1 <- seq.Date(from = ans[1], along.with = eth1, by = 1)
      th2 <- seq.Date(
        from = ans[length(ans)] - length(eth2) + 1,
        along.with = eth2,
        by = 1
      )
      names(th1) <- eth1
      names(th2) <- eth2
      temp <- if (!is.null(nbmatch2) && !is.null(deb2)) {
        sort(c(th1, obs, th2))
      } else {
        sort(c(th1, obs))
      } #that is because in this case th2 can't fit the data, so weird of empty data prior to incubation
      temp <- temp[!duplicated(temp)]
      dinc <- temp[which(as.numeric(names(temp)) >= dat$egg_start[1])[1]]

      if (!is.na(dinc)) {
        names(ans)[which(ans == dinc):length(ans)] <- "incubation"
      }
    }
  }
  if (!is.null(upperbound)) {
    if (upperbound > ans[1]) {
      ans <- ans[ans >= upperbound]
    }
  }

  if (result) {
    beg_end <- rep("U", length(ans))
    beg_end[1] <- "B"
    #which(names(ans)=="incubation")
    beg_end[which(names(ans) == "incubation")[1]] <- "B"
    temp <- which(names(ans) == "laying")
    if (length(temp) >= 2) {
      beg_end[temp[length(temp)]] <- "E"
    }
    list(period = ans, beg_end = beg_end)
  } else {
    ans
  }
}
