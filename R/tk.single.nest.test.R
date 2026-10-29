tk.single.nest.test <-
  function(dates = c("2000-05-01", "2000-05-30"), list_lang) {
    tclServiceMode(FALSE)

    list_lang <- list_lang
    rnest <- new.env()
    assign("list_lang", list_lang, envir = rnest)
    for (i in seq_along(list_lang)) {
      assign(names(list_lang)[i], list_lang[[i]], envir = rnest)
    }

    tt <- tktoplevel()
    tktitle(tt) <- get("titlesntlabl", envir = rnest)
    deslab <- tklabel(tt, text = get("deslabl", envir = rnest))
    tkconfigure(deslab, foreground = "blue")
    incublab <- tklabel(tt, text = get("incublabl", envir = rnest))
    rearlab <- tklabel(tt, text = get("rearlabl", envir = rnest))
    laylab <- tklabel(tt, text = get("laylabl", envir = rnest))
    beglab <- tklabel(tt, text = get("beglabl", envir = rnest))
    clutchlab <- tklabel(tt, text = get("clutchlabl", envir = rnest))
    clutchminlab <- tklabel(tt, text = get("clutchminlabl", envir = rnest))
    clutchmaxlab <- tklabel(tt, text = get("clutchmaxlabl", envir = rnest))

    useminclutchlab <- tklabel(
      tt,
      text = get("useminclutchlabl", envir = rnest)
    )
    incflexplab <- tklabel(tt, text = get("incflexplabl", envir = rnest))
    incflexmlab <- tklabel(tt, text = get("incflexmlabl", envir = rnest))

    incub <- tkentry(tt, width = 5, textvariable = tclVar("10"))
    rear <- tkentry(tt, width = 5, textvariable = tclVar("12"))
    lay <- tkentry(tt, width = 5, textvariable = tclVar("1"))
    beg <- tkentry(tt, width = 5, textvariable = tclVar("999"))
    clutch <- tkentry(tt, width = 5, textvariable = tclVar("4"))
    clutchmin <- tkentry(tt, width = 5, textvariable = tclVar("3"))
    clutchmax <- tkentry(tt, width = 5, textvariable = tclVar("6"))

    useminclutch <- tkentry(tt, width = 5, textvariable = tclVar("T"))
    incflexp <- tkentry(tt, width = 5, textvariable = tclVar(""))
    incflexm <- tkentry(tt, width = 5, textvariable = tclVar(""))

    date <- tklabel(tt, text = "dates")
    eggs <- tklabel(tt, text = get("eggslabl", envir = rnest))
    young <- tklabel(tt, text = get("younglabl", envir = rnest))
    codes <- tklabel(tt, text = "codes")
    separator <- "============================================================================"
    dateini <- as.Date(dates[1])
    n <- as.numeric(abs(difftime(as.Date(dates[2]), dateini))) + 1
    if (n < 10) {
      stop(get("period10labl", envir = rnest))
    }
    for (i in 1:n) {
      datetemp <- as.character(dateini + i - 1)
      assign(paste("date", i, sep = ""), tklabel(tt, text = datetemp))
    }
    for (i in 1:n) {
      assign(
        paste("eggs", i, sep = ""),
        tkentry(tt, width = 5, textvariable = tclVar(""))
      )
    }
    for (i in 1:n) {
      assign(
        paste("young", i, sep = ""),
        tkentry(tt, width = 5, textvariable = tclVar(""))
      )
    }
    for (i in 1:n) {
      assign(
        paste("codes", i, sep = ""),
        tkentry(tt, width = 8, textvariable = tclVar(""))
      )
    }
    tkgrid(date, eggs, young, codes, row = 3)
    for (i in 1:n) {
      tkgrid(get(paste("date", i, sep = "")), row = i + 3, column = 0)
    }
    for (i in 1:n) {
      tkgrid(get(paste("eggs", i, sep = "")), row = i + 3, column = 1)
    }
    for (i in 1:n) {
      tkgrid(get(paste("young", i, sep = "")), row = i + 3, column = 2)
    }
    for (i in 1:n) {
      tkgrid(get(paste("codes", i, sep = "")), row = i + 3, column = 3)
    }
    ff <- function() {
      eg <- sapply(paste("eggs", seq_len(n), sep = ""), function(i) {
        tclvalue(tkget(get(i)))
      })
      yo <- sapply(paste("young", 1:n, sep = ""), function(i) {
        tclvalue(tkget(get(i)))
      })
      co <- sapply(paste("codes", 1:n, sep = ""), function(i) {
        tclvalue(tkget(get(i)))
      })
      w <- which(!(eg == yo & eg == ""))
      eg <- as.numeric(ifelse(eg[w] == "", "0", eg[w]))
      yo <- as.numeric(ifelse(yo[w] == "", "0", yo[w]))
      co <- ifelse(
        !co[w] %in% c("hatch", "fledge", "nest", "incubation", "laying"),
        NA,
        co[w]
      )
      da <- as.character(seq.Date(from = dateini, to = dateini + n - 1, by = 1)[
        w
      ])
      dat <- data.frame(
        species_code = rep("TEST", length(da)),
        nest_id = rep("test", length(da)),
        visit_id = 1:length(da),
        status_code = co,
        stage_code = NA,
        date = da,
        eggs = eg,
        young = yo,
        initiation = NA,
        fledging = NA,
        warning = NA,
        len_active = NA,
        span_error = NA,
        stringsAsFactors = FALSE
      )
      nparam <- data.frame(
        clutch = as.numeric(tclvalue(tkget(clutch))),
        cmin = as.numeric(tclvalue(tkget(clutchmin))),
        cmax = as.numeric(tclvalue(tkget(clutchmax))),
        laying = as.numeric(tclvalue(tkget(lay))),
        incub = as.numeric(tclvalue(tkget(incub))),
        start = as.numeric(tclvalue(tkget(beg))),
        rearing = as.numeric(tclvalue(tkget(rear))),
        sp = "TEST",
        species_code = "TEST",
        stringsAsFactors = FALSE
      )
      minclutch <- as.logical(tclvalue(tkget(useminclutch)))
      inc1 <- tclvalue(tkget(incflexp))
      inc2 <- tclvalue(tkget(incflexm))
      inc1 <- ifelse(inc1 == "", FALSE, as.numeric(inc1))
      inc2 <- ifelse(inc2 == "", FALSE, as.numeric(inc2))
      rel.flex <- if (is.logical(inc1) || is.logical(inc2)) {
        FALSE
      } else {
        c(as.numeric(inc1), as.numeric(inc2))
      }
      lsc <- list(
        nest = c("nest"),
        laying = c("laying"),
        hatch = c("hatch"),
        fledge = c("fledge")
      )
      assign("lll", list(don = dat, nparam = nparam), envir = .GlobalEnv) #remove this part eventually just for testing
      ans <- chronology(
        dat,
        nparam = nparam,
        test = TRUE,
        minclutch = minclutch,
        rel.flex = rel.flex,
        lsc = lsc
      )
      cat("\n", "\n", "\n", "\n")
      print(ans)
    }
    chdate <- function() {
      ttt <- tktoplevel()
      chdatelab1 <- tklabel(ttt, text = get("chdatelab1labl", envir = rnest))
      chdatelab2 <- tklabel(ttt, text = get("chdatelab2labl", envir = rnest))
      chdate1 <- tkentry(ttt, width = 10, textvariable = tclVar(""))
      chdate2 <- tkentry(ttt, width = 10, textvariable = tclVar(""))
      newin <- function() {
        d1 <- tclvalue(tkget(chdate1))
        d2 <- tclvalue(tkget(chdate2))
        if ((abs(as.numeric(difftime(as.Date(d1), as.Date(d2)))) + 1) < 14) {
          stop(get("newinlabl", envir = rnest))
        }
        tkdestroy(tt)
        tkdestroy(ttt)
        tk.single.nest.test(dates = c(d1, d2))
      }
      refresh <- tkbutton(
        ttt,
        text = get("refreshlabl", envir = rnest),
        command = newin
      )
      tkpack(chdatelab1, chdate1)
      tkpack(chdatelab2, chdate2)
      tkpack(refresh)
    }
    writebug <- function() {
      eg <- sapply(paste("eggs", 1:n, sep = ""), function(i) {
        tclvalue(tkget(get(i)))
      })
      yo <- sapply(paste("young", 1:n, sep = ""), function(i) {
        tclvalue(tkget(get(i)))
      })
      co <- sapply(paste("codes", 1:n, sep = ""), function(i) {
        tclvalue(tkget(get(i)))
      })
      w <- which(!(eg == yo & eg == ""))
      eg <- as.numeric(ifelse(eg[w] == "", "0", eg[w]))
      yo <- as.numeric(ifelse(yo[w] == "", "0", yo[w]))
      co <- ifelse(
        !co[w] %in% c("hatch", "fledge", "nest", "incubation", "laying"),
        NA,
        co[w]
      )
      da <- as.character(seq.Date(from = dateini, to = dateini + n - 1, by = 1)[
        w
      ])
      dat <- data.frame(
        species_code = rep("TEST", length(da)),
        nest_id = rep("test", length(da)),
        status_code = co,
        stage_code = rep(NA, length(da)),
        date = da,
        eggs = eg,
        young = yo,
        stringsAsFactors = FALSE
      )
      nparam <- data.frame(
        clutch = as.numeric(tclvalue(tkget(clutch))),
        cmin = as.numeric(tclvalue(tkget(clutchmin))),
        cmax = as.numeric(tclvalue(tkget(clutchmax))),
        laying = as.numeric(tclvalue(tkget(lay))),
        incub = as.numeric(tclvalue(tkget(incub))),
        start = as.numeric(tclvalue(tkget(beg))),
        rearing = as.numeric(tclvalue(tkget(rear))),
        sp = "TEST",
        stringsAsFactors = FALSE
      )
      ans <- merge(dat, nparam)
      curpath <- getwd()
      nameans <- paste(
        "test",
        gsub(":", "", gsub(" ", "_", gsub("-", "", Sys.time()))),
        sep = "_"
      )
      write.table(ans, file = nameans, row.names = FALSE)
      curpath
      setwd(curpath)
      print(nameans)
    }

    bouton1 <- tkbutton(
      tt,
      text = get("bouton1sntlabl", envir = rnest),
      command = ff
    )
    bouton3 <- tkbutton(
      tt,
      text = get("bouton3sntlabl", envir = rnest),
      command = writebug
    )
    bouton2 <- tkbutton(
      tt,
      text = get("bouton2sntlabl", envir = rnest),
      command = chdate
    )
    tkgrid(tklabel(tt, text = ""), row = 0, columnspan = 100)
    tkgrid(deslab, row = 1, columnspan = 100)
    tkgrid(tklabel(tt, text = ""), row = 2, columnspan = 100)
    tkgrid(incublab, row = 4, column = 4)
    tkgrid(incub, row = 4, column = 5)
    tkgrid(rearlab, row = 5, column = 4)
    tkgrid(rear, row = 5, column = 5)
    tkgrid(laylab, row = 6, column = 4)
    tkgrid(lay, row = 6, column = 5)
    tkgrid(beglab, row = 7, column = 4)
    tkgrid(beg, row = 7, column = 5)
    tkgrid(clutchlab, row = 8, column = 4)
    tkgrid(clutch, row = 8, column = 5)
    tkgrid(clutchminlab, row = 9, column = 4)
    tkgrid(clutchmin, row = 9, column = 5)
    tkgrid(clutchmaxlab, row = 10, column = 4)
    tkgrid(clutchmax, row = 10, column = 5)

    tkgrid(useminclutchlab, row = 11, column = 4)
    tkgrid(useminclutch, row = 11, column = 5)
    tkgrid(incflexplab, row = 12, column = 4)
    tkgrid(incflexp, row = 12, column = 5)
    tkgrid(incflexmlab, row = 13, column = 4)
    tkgrid(incflexm, row = 13, column = 5)

    tkgrid(bouton1, row = floor(((n - 7) / 2) + 8), column = 4, columnspan = 2)
    tkgrid(
      bouton3,
      row = floor(((n - 7) / 2) + 8) + 1,
      column = 4,
      columnspan = 2
    )
    tkgrid(bouton2, row = n + 3 - 1, column = 4, columnspan = 2)
    tclServiceMode(TRUE)
  }
