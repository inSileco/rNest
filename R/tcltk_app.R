# The rNest Tcl/Tk user interface: entry points rNest()/rNid(), the main
# window tk.nesting.calendar(), the single-nest windows and their helpers.

#' Calling rNest's User Interface
#'
#' These functions call the user tcltk interface as well as the nestwatch data,
#' nesting parameters and bird grouping characteristics
#'
#' The function `rNest` is used to call the english interface and
#' `rNid` is used to call the french interface.
#'
#' Information on how to use the interface is partly contained in the different
#' menus and buttons.
#'
#' When using the `rNest` function, the NestWatch data
#' ([nestwatch]) is called first, then nesting parameters
#' ([nestingparameters]) and bird groupings
#' ([birdgroupings]) and finally the user interface appears with
#' its different menus. Both functions use the `rNest_tcltk` function
#' internal to the package.
#'
#' @return Opens the tcltk interface of rNest
#' @note Currently, a bug in tcltk causes R and the user interface to crash
#' when regions are selected interactively on a plot and the plot is
#' prematurely closed before ending the selection. To prevent the crash, always
#' end any selection by using the right button of the mouse and choosing the
#' stop option displayed. Similarly, interrupting a long calculation is
#' currently causing the same type of problem.
#' @author Francois Rousseu <francoisrousseu@@hotmail.com>
#' @seealso To be described
#' @export
rNest <- function() rNest_tcltk(lang = "en")

#' @rdname rNest
#' @export
rNid <- function() rNest_tcltk(lang = "fr")


rNest_tcltk <- function(lang = "en") {
  msg <- i18n("rNest_tcltk", lang)
  tkmessageBox(
    title = msg$warninglabl,
    message = paste(msg$warningtextlabl, collapse = "\n\n"),
    icon = "info",
    type = "ok"
  )
  cat("\n  ", msg$loadingmsglabl, "\n")
  flush.console()
  data(nestwatch, package = "rNest")
  data(nestingparameters, package = "rNest")
  data(birdgroupings, package = "rNest")
  data(ecodistrictscsv, package = "rNest")
  data(predictions, package = "rNest")
  data(specodistricts, package = "rNest")
  x <- dlply(ecodistrictscsv, .(group), function(i) {
    Polygon(i[c(nrow(i), 1:nrow(i)), 1:2], hole = i$hole[1])
  })
  x <- sapply(unique(ecodistrictscsv$id), function(i) {
    res <- x[
      names(x) %in% ecodistrictscsv$group[which(ecodistrictscsv$id == i)]
    ]
    Polygons(res, ID = i)
  })
  dat <- ecodistrictscsv[match(names(x), ecodistrictscsv$id), ]
  # stringsAsFactors = TRUE was the default before R 4.0
  dat <- as.data.frame(
    do.call("rbind", strsplit(dat$id, "_")),
    stringsAsFactors = TRUE
  )
  names(dat) <- c(
    "PROVINCE",
    "ecodistrict",
    "breeding_zone2",
    "breeding_zone",
    "provincesEN",
    "provincesFR"
  )
  dat <- dat[, -1]
  dat$ecodistrict <- as.numeric(as.character(dat$ecodistrict))
  x <- SpatialPolygons(
    x,
    proj4string = CRS(
      "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
    )
  )
  x <- SpatialPolygonsDataFrame(x, data = dat, match.ID = FALSE)
  ecodistricts <- x
  tk.nesting.calendar(ecodistricts = ecodistricts, lang = lang)
}


tk.nesting.calendar <- function(
  grid = FALSE,
  ecodistricts = ecodistricts,
  lang = "en"
) {
  # the app state: every object of this function, shared with the app_*() functions
  app <- environment()
  list_lang <- i18n("tk.nesting.calendar", lang)
  rnest <- new.env()
  assign("usershp", NULL, envir = rnest) ### to put the name in the environment
  assign("usershppath", FALSE, envir = rnest) ### to put the name in the environment
  assign("predict", FALSE, envir = rnest)
  assign("rejected_status", FALSE, envir = rnest)
  n <- 6
  groups <- species_groups()
  d <- nestwatch
  sg <- birdgroupings
  sg <- subset(
    sg,
    select = c(
      Species_ID,
      MBCA_2011_Final,
      Species_gr1,
      Species_gr2,
      Habitat1,
      Habitat2,
      Habitat3,
      Habitat4,
      Nest_Type1,
      Nest_Type2,
      Nest_Type3,
      model
    )
  )

  x <- unique(unlist(subset(sg, select = -Species_ID)))
  x <- x[!is.na(x)]

  sg2 <- subset(sg, select = Species_ID)
  for (i in names(groups)) {
    grp <- groups[[i]]
    for (j in seq_along(grp$values)) {
      ans <- apply(sg[, grp$columns, drop = FALSE], 2, function(k) {
        ans <- k == grp$values[j]
        ans <- ifelse(is.na(ans), FALSE, ans)
        ans
      })
      ans <- apply(ans, 1, any)
      sg2 <- cbind(sg2, as.integer(ans), stringsAsFactors = FALSE)
      names(sg2)[ncol(sg2)] <- paste(i, grp$labels[j], sep = "_")
    }
  }
  sg <- sg2
  rm(grp)

  old_mode <- tclServiceMode(FALSE)
  on.exit(tclServiceMode(old_mode), add = TRUE)
  tt <- tktoplevel()
  # no fixed geometry: let Tk size the window from its content (fonts scale
  # with the screen DPI), but never larger than the screen
  tcl(
    "wm",
    "maxsize",
    tt,
    tclvalue(tcl("winfo", "screenwidth", tt)),
    tclvalue(tcl("winfo", "screenheight", tt))
  )
  for (i in 0:36) {
    tkgrid.rowconfigure(tt, i, weight = 1, minsize = 12)
  }
  for (i in 0:20) {
    tkgrid.columnconfigure(tt, i, weight = 1)
  }
  tkgrid.columnconfigure(tt, 0, weight = 1, minsize = 110)
  tkgrid.columnconfigure(tt, 1, weight = 1)
  tkgrid.columnconfigure(tt, 2, weight = 1, minsize = 20)
  tkgrid.columnconfigure(tt, 5, weight = 0, minsize = 30)
  #tkgrid.columnconfigure(tt,4,weight=1,minsize=45)
  tkgrid.columnconfigure(tt, 6, weight = 0)
  tkgrid.columnconfigure(tt, 7, weight = 0)
  tkgrid.rowconfigure(tt, 36, weight = 1, minsize = 40)
  for (i in c(10, 12, 14, 16, 18, 20)) {
    tkgrid.columnconfigure(tt, i, minsize = 17, weight = 1)
  }
  for (i in c(4, 9, 11, 13, 15, 17, 19)) {
    tkgrid.columnconfigure(tt, i, minsize = 50, weight = 1)
  }
  tktitle(tt) <- list_lang$rNestname

  header1 <- tklabel(tt, text = list_lang$header1labl)
  tkconfigure(header1, foreground = "blue")
  header2 <- tklabel(tt, text = list_lang$header2labl)
  tkconfigure(header2, foreground = "blue")
  header3 <- tklabel(tt, text = list_lang$header3labl)
  tkconfigure(header3, foreground = "blue")

  intchoice <- tkcheckbutton(tt)
  intchoiceval <- tclVar("0")
  tkconfigure(intchoice, variable = intchoiceval, text = "")
  intchoicelab <- tklabel(tt, text = list_lang$intchoicelabl)
  intchoiceq <- tkbutton(
    tt,
    pady = 0,
    bg = "grey85",
    text = "?",
    command = function() {
      tk_info(list_lang$intchoiceqlabl)
    }
  )

  zoom <- tkcheckbutton(tt)
  zoomval <- tclVar("0")
  tkconfigure(zoom, variable = zoomval, text = "")
  zoomlab <- tklabel(tt, text = list_lang$zoomlabl)
  zoomq <- tkbutton(
    tt,
    pady = 0,
    bg = "grey85",
    text = "?",
    command = function() {
      tk_info(list_lang$zoomqlabl)
    }
  )

  backsp <- tkcheckbutton(tt)
  backspval <- tclVar("0")
  tkconfigure(backsp, variable = backspval, text = "")
  backsplab <- tklabel(tt, text = list_lang$backsplabl)
  backspq <- tkbutton(
    tt,
    pady = 0,
    bg = "grey85",
    text = "?",
    command = function() {
      tk_info(list_lang$backspqlabl)
    }
  )

  shp <- tkentry(tt, width = 26, textvariable = tclVar(""))
  shpb <- tkbutton(
    tt,
    text = list_lang$shpblabl,
    command = function() app_chooseshp(app),
    pady = 0,
    width = 11,
    bg = "grey85"
  )
  shpq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(paste0(
        list_lang$tkmb29[1],
        getwd(),
        list_lang$tkmb29[2],
        tclvalue(tkget(shp)),
        list_lang$tkmb29[3],
        ifelse(
          !is.null(get("usershp", envir = rnest)),
          proj4string(get("usershp", envir = rnest)),
          ""
        )
      ))
    }
  )

  id <- tkentry(tt, width = 26, textvariable = tclVar(""))
  idb <- tkbutton(
    tt,
    text = list_lang$idblabl,
    command = function() app_chooseid(app),
    pady = 0,
    width = 11,
    bg = "grey85"
  )
  idq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(paste0(list_lang$tkmb30, tclvalue(tkget(id))))
    }
  )

  region <- tkentry(tt, width = 26, textvariable = tclVar(""))
  regionb <- tkbutton(
    tt,
    text = list_lang$regionblabl,
    command = function() app_chooseregion(app),
    pady = 0,
    width = 11,
    bg = "grey85"
  )
  regionq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(paste0(
        list_lang$tkmb31,
        tclvalue(tkget(region))
      ))
    }
  )

  #specieslab<-tklabel(tt,text="species")
  species <- tkentry(tt, width = 26, textvariable = tclVar(""))
  speciesb <- tkbutton(
    tt,
    text = list_lang$speciesblabl,
    command = function() app_choosesp(app),
    pady = 0,
    width = 11,
    bg = "grey85"
  )
  speciesq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(paste0(
        list_lang$tkmb32,
        tclvalue(tkget(species))
      ))
    }
  )

  yearlab <- tkbutton(
    tt,
    text = list_lang$yearlabl,
    pady = 0,
    width = 11,
    bg = "grey85"
  )
  year <- tkentry(tt, width = 26, textvariable = tclVar(""))
  yearq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(paste0(list_lang$tkmb33, tclvalue(tkget(year))))
    }
  )

  cal <- tclVar("sp")
  spcal <- tkradiobutton(tt)
  yearcal <- tkradiobutton(tt)
  regioncal <- tkradiobutton(tt)
  tkconfigure(
    spcal,
    variable = cal,
    value = "sp",
    text = list_lang$spcallabl
  )
  tkconfigure(
    yearcal,
    variable = cal,
    value = "year",
    text = list_lang$yearcallabl
  )
  tkconfigure(
    regioncal,
    variable = cal,
    value = "region",
    text = list_lang$regioncallabl
  )

  ordercal <- tclVar("spgr")
  orderspgrcal <- tkradiobutton(tt)
  orderecocal <- tkradiobutton(tt)
  tkconfigure(
    orderspgrcal,
    variable = ordercal,
    value = "spgr",
    text = list_lang$orderspgrcallabl
  )
  tkconfigure(
    orderecocal,
    variable = ordercal,
    value = "eco",
    text = list_lang$orderecocallabl
  )

  nameback <- tkentry(tt, width = 12, textvariable = tclVar("estimates"))
  namebackq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(list_lang$namebackqtext)
    }
  )
  namerawcal <- tkentry(tt, width = 12, textvariable = tclVar("calendar"))
  namerawcalq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(list_lang$namerawcalqtext)
    }
  )

  butcal <- tkbutton(
    tt,
    text = list_lang$butcallabl,
    bg = "grey55",
    width = 27,
    command = function() {
      if (get("predict", envir = rnest)) {
        app_calendarpred(app)
      } else {
        app_calendarobs(app)
      }
    }
  )
  butcalq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(list_lang$butcalqtext)
    }
  )

  datelab <- tklabel(tt, text = "dates")
  datebeg <- tkentry(tt, width = 7, textvariable = tclVar("02-01"))
  dateend <- tkentry(tt, width = 7, textvariable = tclVar("09-30"))

  propbut <- tkbutton(
    tt,
    text = list_lang$propbutlabl,
    width = 11,
    command = function() app_propfun(app),
    pady = 0,
    bg = "grey85"
  )
  prop <- tkentry(tt, width = 25, textvariable = tclVar(""))
  propq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() app_propfun(app)
  )

  colobut <- tkbutton(
    tt,
    text = list_lang$colobutlabl,
    width = 11,
    command = function() app_showcol(app),
    pady = 0,
    bg = "grey85"
  )
  colo <- tkentry(tt, width = 25, textvariable = tclVar(""))
  coloq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() app_showcol(app)
  )

  showpropbut <- tkbutton(
    tt,
    text = list_lang$showpropbutlabl,
    width = 11,
    command = function() {},
    pady = 0,
    bg = "grey85"
  )
  showpropnest <- tkentry(tt, width = 4, textvariable = tclVar("10"))
  showpropcolo <- tkentry(tt, width = 18, textvariable = tclVar(""))
  showpropq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() app_showpropcol(app)
  )

  # obs mode
  usp <- tkradiobutton(tt)
  ugr <- tkradiobutton(tt)
  spgr <- tclVar("usp")
  tkconfigure(
    usp,
    variable = spgr,
    value = "usp",
    text = list_lang$usplabl
  )
  tkconfigure(
    ugr,
    variable = spgr,
    value = "ugr",
    text = list_lang$ugrlabl
  )
  # predict mode
  uallsp <- tkradiobutton(tt)
  uselectsp <- tkradiobutton(tt)
  ugroups <- tkradiobutton(tt)
  uspgroup <- tclVar("uselectsp")
  tkconfigure(
    uallsp,
    variable = uspgroup,
    value = "uallsp",
    text = list_lang$uallsplabl
  )
  tkconfigure(
    uselectsp,
    variable = uspgroup,
    value = "uselectsp",
    text = list_lang$uselectsplabl
  )
  tkconfigure(
    ugroups,
    variable = uspgroup,
    value = "ugroups",
    text = list_lang$ugroupslabl
  )

  ll <- sg$Species_ID[sg$Species_ID %in% unique(d$species_code)]

  scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) {
    tkyview(listid, ...)
  })
  listid <- tklistbox(
    tt,
    height = min(50, length(ll)),
    width = 10,
    selectmode = "multiple",
    background = "white",
    yscrollcommand = function(...) {
      tkset(scr, ...)
    }
  )
  listidlab <- tklabel(tt, text = "           ")
  for (i in 1:length(ll)) {
    tkinsert(listid, "end", ll[i])
  }
  tkgrid(listidlab, row = 3, rowspan = 32, column = 3)
  tkgrid(
    tklabel(tt, text = list_lang$tkmb37),
    row = 2,
    rowspan = 1,
    column = 4,
    sticky = "nw"
  )
  tkgrid(listid, row = 3, rowspan = 32, column = 4, sticky = "ns")
  tkgrid(scr, row = 3, rowspan = 32, column = 5, sticky = "nsw")

  #tkgrid(tkbutton(tt,text="clear all",width=10,pady=0,bg="grey85",command=function(i){assign(paste(lgroups[[2]],"val",sep=""),tclVar("0"))}),row=17,column=7)

  groupq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(list_lang$groupqtext)
    }
  )

  lgroups <- lapply(groups, function(x) x$labels)
  lgroupsl <- list_lang$lgroupsl

  ### first set
  pos <- 3
  for (i in 1:(length(lgroups) - 1)) {
    pos <- pos + 2
    tkgrid(
      tkbutton(
        tt,
        text = gsub("_", " ", names(lgroupsl)[i]),
        width = 12,
        bg = "grey85",
        command = local({
          i <- i
          r <- pos
          s <- length(lgroups[[i]])
          function() {
            old_mode <- tclServiceMode(FALSE)
            on.exit(tclServiceMode(old_mode), add = TRUE)
            app_butmask(app, i, r, s, predict = get("predict", envir = rnest))
            app_relistsp(app, sg, lgroups)
          }
        })
      ),
      row = pos,
      column = 6,
      rowspan = 1,
      sticky = "w"
    )
    for (j in 1:length(lgroups[[i]])) {
      pos <- pos + 1
      assign(
        lgroups[[i]][j],
        tkcheckbutton(
          tt,
          command = local({
            i <- i
            j <- j
            function() {
              app_relistsp(app, sg, lgroups)
              if (i == 5 && j == 2) {
                assign("rejected_status", TRUE, envir = rnest)
              }
            }
          })
        ),
        envir = rnest
      )
      assign(
        paste(names(lgroups)[i], lgroups[[i]][j], "val", sep = "_"),
        tclVar(ifelse(
          get("predict", envir = rnest) &&
            paste(names(lgroups)[i], lgroups[[i]][j], "val", sep = "_") ==
              "model_output_rejected_val",
          "0",
          "1"
        )),
        envir = rnest
      )
      tkconfigure(
        get(lgroups[[i]][j], envir = rnest),
        variable = get(
          paste(names(lgroups)[i], lgroups[[i]][j], "val", sep = "_"),
          envir = rnest
        ),
        text = lgroupsl[[i]][j]
      )
      tkgrid(
        get(lgroups[[i]][j], envir = rnest),
        row = pos,
        column = 6,
        rowspan = 1,
        sticky = "w"
      )
    }
  }

  ### second set
  pos <- 18
  co <- 9
  for (i in length(lgroups)) {
    tkgrid(
      tkbutton(
        tt,
        text = gsub("_", " ", names(lgroupsl)[i]),
        width = 12,
        bg = "grey85",
        command = local({
          i <- i
          function() {
            old_mode <- tclServiceMode(FALSE)
            on.exit(tclServiceMode(old_mode), add = TRUE)
            app_butmaskgroups(app, i)
            app_relistsp(app, sg, lgroups)
          }
        })
      ),
      row = pos,
      column = co,
      columnspan = 3,
      rowspan = 1,
      sticky = "we"
    )
    pos <- pos + 1
    tkgrid(
      tkbutton(
        tt,
        text = list_lang$uncheckall,
        width = 12,
        bg = "grey85",
        command = local({
          i <- i
          function() {
            old_mode <- tclServiceMode(FALSE)
            on.exit(tclServiceMode(old_mode), add = TRUE)
            app_butmaskgroups(app, i, val = "0")
            app_relistsp(app, sg, lgroups)
          }
        })
      ),
      row = pos,
      column = co,
      columnspan = 3,
      rowspan = 1,
      sticky = "we"
    )
    pos <- pos + 1
    for (j in 1:length(lgroups[[i]])) {
      assign(
        lgroups[[i]][j],
        tkcheckbutton(tt, command = function(x) {
          app_relistsp(app, sg, lgroups)
        }),
        envir = rnest
      )
      assign(
        paste(names(lgroups)[i], lgroups[[i]][j], "val", sep = "_"),
        tclVar("1"),
        envir = rnest
      )
      tkconfigure(
        get(lgroups[[i]][j], envir = rnest),
        variable = get(
          paste(names(lgroups)[i], lgroups[[i]][j], "val", sep = "_"),
          envir = rnest
        ),
        text = lgroupsl[[i]][j]
      )
      tkgrid(
        get(lgroups[[i]][j], envir = rnest),
        row = pos,
        column = co,
        columnspan = 4,
        rowspan = 1,
        sticky = "w"
      )
      pos <- pos + 1
      if (pos == 35) {
        pos <- 18
        co <- co + 4
      }
    }
  }

  tkbind(listid, "<Button-3>", function() {
    app_relistsp(app, sg, lgroups)
  })
  tkbind(listid, "<Double-Button-1>", function() {
    tkselection.set(listid, 0, "end")
  })

  group1 <- tkentry(
    tt,
    width = 12,
    textvariable = tclVar(list_lang$group1labl)
  )
  groupscr1 <- tkscrollbar(tt, repeatinterval = 5, command = function(...) {
    tkyview(groupm1, ...)
  })
  groupm1 <- tklistbox(
    tt,
    height = 9,
    width = 12,
    selectmode = "multiple",
    background = "white",
    yscrollcommand = function(...) {
      tkset(groupscr1, ...)
    }
  )
  groupb1 <- tkbutton(
    tt,
    text = list_lang$groupb1labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_selection(app, 1)
  )
  groupc1 <- tkbutton(
    tt,
    text = list_lang$groupc1labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 1)
  )
  groupcall1 <- tkbutton(
    tt,
    text = list_lang$groupcall1labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 1, all = TRUE)
  )
  groupsp1 <- tkbutton(
    tt,
    text = list_lang$groupsp1labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_group2species(app, no = 1)
  )
  tkgrid(group1, row = 3, column = 9, columnspan = 2, sticky = "ew")
  tkgrid(
    groupb1,
    row = 4,
    rowspan = 1,
    column = 9,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupc1,
    row = 5,
    rowspan = 1,
    column = 9,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupcall1,
    row = 6,
    rowspan = 1,
    column = 9,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupsp1,
    row = 7,
    rowspan = 1,
    column = 9,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(groupm1, row = 8, rowspan = 9, column = 9, sticky = "s")
  tkgrid(groupscr1, row = 8, rowspan = 9, column = 10, sticky = "ns")
  group2 <- tkentry(
    tt,
    width = 12,
    textvariable = tclVar(list_lang$group2labl)
  )
  groupscr2 <- tkscrollbar(tt, repeatinterval = 5, command = function(...) {
    tkyview(groupm2, ...)
  })
  groupm2 <- tklistbox(
    tt,
    height = 9,
    width = 12,
    selectmode = "multiple",
    background = "white",
    yscrollcommand = function(...) {
      tkset(groupscr2, ...)
    }
  )
  groupb2 <- tkbutton(
    tt,
    text = list_lang$groupb2labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_selection(app, 2)
  )
  groupc2 <- tkbutton(
    tt,
    text = list_lang$groupc2labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 2)
  )
  groupcall2 <- tkbutton(
    tt,
    text = list_lang$groupcall2labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 2, all = TRUE)
  )
  groupsp2 <- tkbutton(
    tt,
    text = list_lang$groupsp2labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_group2species(app, no = 2)
  )
  tkgrid(group2, row = 3, column = 11, columnspan = 2, sticky = "ew")
  tkgrid(
    groupb2,
    row = 4,
    rowspan = 1,
    column = 11,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupc2,
    row = 5,
    rowspan = 1,
    column = 11,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupcall2,
    row = 6,
    rowspan = 1,
    column = 11,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupsp2,
    row = 7,
    rowspan = 1,
    column = 11,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(groupm2, row = 8, rowspan = 9, column = 11, sticky = "s")
  tkgrid(groupscr2, row = 8, rowspan = 9, column = 12, sticky = "ns")
  group3 <- tkentry(
    tt,
    width = 12,
    textvariable = tclVar(list_lang$group3labl)
  )
  groupscr3 <- tkscrollbar(tt, repeatinterval = 5, command = function(...) {
    tkyview(groupm3, ...)
  })
  groupm3 <- tklistbox(
    tt,
    height = 9,
    width = 12,
    selectmode = "multiple",
    background = "white",
    yscrollcommand = function(...) {
      tkset(groupscr3, ...)
    }
  )
  groupb3 <- tkbutton(
    tt,
    text = list_lang$groupb3labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_selection(app, 3)
  )
  groupc3 <- tkbutton(
    tt,
    text = list_lang$groupc3labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 3)
  )
  groupcall3 <- tkbutton(
    tt,
    text = list_lang$groupcall3labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 3, all = TRUE)
  )
  groupsp3 <- tkbutton(
    tt,
    text = list_lang$groupsp3labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_group2species(app, no = 3)
  )
  tkgrid(group3, row = 3, column = 13, columnspan = 2, sticky = "ew")
  tkgrid(
    groupb3,
    row = 4,
    rowspan = 1,
    column = 13,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupc3,
    row = 5,
    rowspan = 1,
    column = 13,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupcall3,
    row = 6,
    rowspan = 1,
    column = 13,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupsp3,
    row = 7,
    rowspan = 1,
    column = 13,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(groupm3, row = 8, rowspan = 9, column = 13, sticky = "s")
  tkgrid(groupscr3, row = 8, rowspan = 9, column = 14, sticky = "ns")
  group4 <- tkentry(
    tt,
    width = 12,
    textvariable = tclVar(list_lang$group4labl)
  )
  groupscr4 <- tkscrollbar(tt, repeatinterval = 5, command = function(...) {
    tkyview(groupm4, ...)
  })
  groupm4 <- tklistbox(
    tt,
    height = 9,
    width = 12,
    selectmode = "multiple",
    background = "white",
    yscrollcommand = function(...) {
      tkset(groupscr4, ...)
    }
  )
  groupb4 <- tkbutton(
    tt,
    text = list_lang$groupb4labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_selection(app, 4)
  )
  groupc4 <- tkbutton(
    tt,
    text = list_lang$groupc4labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 4)
  )
  groupcall4 <- tkbutton(
    tt,
    text = list_lang$groupcall4labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 4, all = TRUE)
  )
  groupsp4 <- tkbutton(
    tt,
    text = list_lang$groupsp4labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_group2species(app, no = 4)
  )
  tkgrid(group4, row = 3, column = 15, columnspan = 2, sticky = "ew")
  tkgrid(
    groupb4,
    row = 4,
    rowspan = 1,
    column = 15,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupc4,
    row = 5,
    rowspan = 1,
    column = 15,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupcall4,
    row = 6,
    rowspan = 1,
    column = 15,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupsp4,
    row = 7,
    rowspan = 1,
    column = 15,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(groupm4, row = 8, rowspan = 9, column = 15, sticky = "s")
  tkgrid(groupscr4, row = 8, rowspan = 9, column = 16, sticky = "ns")
  group5 <- tkentry(
    tt,
    width = 12,
    textvariable = tclVar(list_lang$group5labl)
  )
  groupscr5 <- tkscrollbar(tt, repeatinterval = 5, command = function(...) {
    tkyview(groupm5, ...)
  })
  groupm5 <- tklistbox(
    tt,
    height = 9,
    width = 12,
    selectmode = "multiple",
    background = "white",
    yscrollcommand = function(...) {
      tkset(groupscr5, ...)
    }
  )
  groupb5 <- tkbutton(
    tt,
    text = list_lang$groupb5labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_selection(app, 5)
  )
  groupc5 <- tkbutton(
    tt,
    text = list_lang$groupc5labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 5)
  )
  groupcall5 <- tkbutton(
    tt,
    text = list_lang$groupcall5labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 5, all = TRUE)
  )
  groupsp5 <- tkbutton(
    tt,
    text = list_lang$groupsp5labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_group2species(app, no = 5)
  )
  tkgrid(group5, row = 3, column = 17, columnspan = 2, sticky = "ew")
  tkgrid(
    groupb5,
    row = 4,
    rowspan = 1,
    column = 17,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupc5,
    row = 5,
    rowspan = 1,
    column = 17,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupcall5,
    row = 6,
    rowspan = 1,
    column = 17,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupsp5,
    row = 7,
    rowspan = 1,
    column = 17,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(groupm5, row = 8, rowspan = 9, column = 17, sticky = "s")
  tkgrid(groupscr5, row = 8, rowspan = 9, column = 18, sticky = "ns")
  group6 <- tkentry(
    tt,
    width = 12,
    textvariable = tclVar(list_lang$group6labl)
  )
  groupscr6 <- tkscrollbar(tt, repeatinterval = 5, command = function(...) {
    tkyview(groupm6, ...)
  })
  groupm6 <- tklistbox(
    tt,
    height = 9,
    width = 12,
    selectmode = "multiple",
    background = "white",
    yscrollcommand = function(...) {
      tkset(groupscr6, ...)
    }
  )
  groupb6 <- tkbutton(
    tt,
    text = list_lang$groupb6labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_selection(app, 6)
  )
  groupc6 <- tkbutton(
    tt,
    text = list_lang$groupc6labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 6)
  )
  groupcall6 <- tkbutton(
    tt,
    text = list_lang$groupcall6labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_clearselection(app, 6, all = TRUE)
  )
  groupsp6 <- tkbutton(
    tt,
    text = list_lang$groupsp6labl,
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() app_group2species(app, no = 6)
  )
  tkgrid(group6, row = 3, column = 19, columnspan = 2, sticky = "ew")
  tkgrid(
    groupb6,
    row = 4,
    rowspan = 1,
    column = 19,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupc6,
    row = 5,
    rowspan = 1,
    column = 19,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupcall6,
    row = 6,
    rowspan = 1,
    column = 19,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(
    groupsp6,
    row = 7,
    rowspan = 1,
    column = 19,
    columnspan = 2,
    sticky = "ew"
  )
  tkgrid(groupm6, row = 8, rowspan = 9, column = 19, sticky = "s")
  tkgrid(groupscr6, row = 8, rowspan = 9, column = 20, sticky = "ns")

  useprecalc <- tkcheckbutton(tt)
  useprecalcval <- tclVar("1")
  tkconfigure(
    useprecalc,
    variable = useprecalcval,
    text = list_lang$useprecalclabl
  )

  assign("path1", "", envir = rnest)
  assign("path2", "", envir = rnest)
  assign("path3", "", envir = rnest)
  assign("path4", "", envir = rnest)

  topMenu <- tkmenu(tt)
  tkconfigure(tt, menu = topMenu)
  fileMenu <- tkmenu(topMenu, tearoff = FALSE)
  tkadd(
    topMenu,
    "cascade",
    label = list_lang$menufilelabl,
    menu = fileMenu
  )
  tkadd(
    fileMenu,
    "command",
    label = list_lang$menuloaddatalabl,
    command = function() {
      print("Not implemented yet")
      app_loaddata(app)
    }
  )

  tkadd(
    fileMenu,
    "command",
    label = list_lang$menuquitlabl,
    command = function() tkdestroy(tt)
  )

  optionsMenu <- tkmenu(topMenu, tearoff = FALSE)
  tkadd(topMenu, "cascade", label = "Options", menu = optionsMenu)
  tkadd(
    optionsMenu,
    "command",
    label = list_lang$menuretrolabl,
    command = function() print("Not implemented yet")
  )
  testMenu <- tkmenu(topMenu, tearoff = FALSE)
  tkadd(
    topMenu,
    "cascade",
    label = list_lang$menutestlabl,
    menu = testMenu
  )
  tkadd(
    testMenu,
    "command",
    label = list_lang$menuranlabl,
    command = function() {
      tk.single.nest(list_lang = list_lang)
    }
  )
  tkadd(
    testMenu,
    "command",
    label = list_lang$menuficlabl,
    command = function() {
      tk.single.nest.test(
        dates = c("2000-05-01", "2000-05-30"),
        list_lang = list_lang
      )
    }
  )
  #tkfocus(tt)

  ### parts for the predict mode
  #regionlab<-tklabel(tt,text="region")
  ecodistrict <- tkentry(tt, width = 25, textvariable = tclVar(""))
  ecodistrictb <- tkbutton(
    tt,
    text = list_lang$ecodistrictblabl,
    command = function() app_chooseecodistrict(app),
    pady = 0,
    width = 11,
    bg = "grey85"
  )
  ecodistrictq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(paste(
        list_lang$tkmb41,
        "{ ",
        tclvalue(tkget(ecodistrict)),
        " }"
      ))
    }
  )

  tkgrid(tklabel(tt, text = "       "), column = 5, row = 0)
  tkgrid(tklabel(tt, text = "   "), column = 7, row = 0)
  tkgrid(tklabel(tt, text = "       "), column = n + 8 + 1, row = 0)

  header4 <- tklabel(tt, text = list_lang$header4labl)
  tkconfigure(header4, foreground = "blue")
  tkgrid(header4, row = 1, column = 9, columnspan = n * 2, sticky = "we")
  grouplab <- tklabel(tt, text = list_lang$grouplabl)

  fontmode <- tkfont.create(size = 14, weight = "bold")

  butpred <- tkbutton(
    tt,
    text = list_lang$butpredlabl,
    bg = "grey55",
    bg = "lightblue",
    fg = "red",
    font = fontmode,
    command = function() {
      old_mode <- tclServiceMode(FALSE)
      on.exit(tclServiceMode(old_mode), add = TRUE)
      app_widget(app, predict = FALSE)
      app_rejected(app, fromobsbut = FALSE)
      app_relistsp(app, sg, lgroups)
    }
  )
  butobs <- tkbutton(
    tt,
    text = list_lang$butobslabl,
    bg = "grey55",
    bg = "lightgreen",
    fg = "red",
    font = fontmode,
    command = function() {
      old_mode <- tclServiceMode(FALSE)
      on.exit(tclServiceMode(old_mode), add = TRUE)
      app_widget(app, predict = TRUE)
      app_rejected(app, fromobsbut = TRUE)
      app_relistsp(app, sg, lgroups)
    }
  )

  butrun <- tkbutton(
    tt,
    text = list_lang$butrunlabl,
    bg = "grey55",
    width = 27,
    command = function() app_run(app)
  )
  butrunq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(list_lang$butrunqtext)
    }
  )
  butmap <- tkbutton(
    tt,
    text = list_lang$butmaplabl,
    bg = "grey55",
    width = 27,
    command = function() app_mapnest(app)
  )
  butmapq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(list_lang$butmapqtext)
    }
  )
  buteco <- tkbutton(
    tt,
    text = list_lang$butecolabl,
    bg = "grey55",
    width = 27,
    command = function() app_mapeco(app)
  )
  butecoq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(list_lang$butecoqtext)
    }
  )
  butsp <- tkbutton(
    tt,
    text = list_lang$butsplabl,
    bg = "grey55",
    width = 27,
    command = function() app_mapsp(app)
  )
  butspq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tk_info(list_lang$butspqtext)
    }
  )

  app_widget(app, predict = FALSE)

  if (grid) {
    size <- rev(as.numeric(tkgrid.size(tt)))
    print(size)
    for (i in 1:size[1]) {
      for (j in 1:size[2]) {
        a <- tkgrid(
          tklabel(tt, text = paste(i - 1, ",", j - 1, sep = "")),
          row = i - 1,
          column = j - 1
        )
      }
    }
  }
  invisible(app)
}


app_getopt <- function(app) {
  species1 <- tclvalue(tkget(app$species))
  species1 <- if (species1 == "") {
    FALSE
  } else {
    unlist(strsplit(species1, ","))
  }
  years1 <- tclvalue(tkget(app$year))
  years1 <- if (years1 == "") {
    FALSE
  } else {
    as.numeric(unlist(strsplit(years1, ",")))
  }
  years1 <- if (length(years1) == 1) {
    years1
  } else {
    years1[1]:years1[2]
  }
  shp1 <- tclvalue(tkget(app$shp))
  shp1 <- if (shp1 == "") {
    FALSE
  } else {
    shp1
  }
  if (!identical(shp1, get("usershppath", envir = app$rnest))) {
    assign("usershppath", shp1, envir = app$rnest)
    if (!is.logical(shp1)) {
      tkconfigure(app$tt, cursor = "watch")
      on.exit(tkconfigure(app$tt, cursor = "arrow"))
      arg <- breakshapepath(shp1)
      usershp <- read_layer(dsn = arg[1], layer = arg[2])
      assign("usershp", usershp, envir = app$rnest)
    } else {
      assign("usershp", NULL, envir = app$rnest)
      assign("usershp", app$ecodistricts, envir = app$rnest)
    }
  }
  id1 <- tclvalue(tkget(app$id))
  id1 <- if (id1 == "") {
    FALSE
  } else {
    unlist(strsplit(id1, ","))
  } #mettre le ID \u00e0 FALSE ici
  region1 <- tclvalue(tkget(app$region))
  region1 <- if (region1 == "") {
    FALSE
  } else {
    unlist(strsplit(region1, ","))
  }
  intchoice1 <- as.logical(as.numeric(tclvalue(app$intchoiceval)))
  zoom1 <- as.logical(as.numeric(tclvalue(app$zoomval)))
  backsp1 <- as.logical(as.numeric(tclvalue(app$backspval)))
  useprecalc1 <- as.logical(as.numeric(tclvalue(app$useprecalcval)))
  if (tclvalue(app$spgr) == "usp") {
    spgroup1 <- NULL
  } else {
    spgroup1 <- app_getgroups(app)
    spgroup1 <- spgroup1[
      !sapply(spgroup1, function(i) {
        identical(NA, i)
      })
    ]
    species1 <- FALSE
  }
  uspgroup1 <- tclvalue(app$uspgroup)
  ecodistrict1 <- tclvalue(tkget(app$ecodistrict))
  ecodistrict1 <- if (identical(ecodistrict1, "")) {
    ""
  } else {
    unlist(strsplit(ecodistrict1, ","))
  }
  l <- list(
    sp = species1,
    year = years1,
    layer = shp1,
    region = region1,
    id = id1,
    int.choice = intchoice1,
    zoom = zoom1,
    backsp = backsp1,
    locations = FALSE,
    spgroup = spgroup1,
    useprecalc = useprecalc1,
    uspgroup = uspgroup1,
    ecodistrict = ecodistrict1,
    progress = TRUE
  )
  return(l)
}


app_mapnest <- function(app) {
  tkconfigure(app$tt, cursor = "watch")
  on.exit(tkconfigure(app$tt, cursor = "arrow"))
  l <- app_getopt(app)
  #y<-x ### sub 2014-01-17
  temp <- TRUE
  if (!is.logical(l$sp)) {
    x <- app$d[app$d[, "species_code"] %in% l$sp, ]
    temp <- FALSE
  }
  if (!is.null(l$spgroup)) {
    x <- app$d[app$d[, "species_code"] %in% unlist(l$spgroup), ]
    temp <- FALSE
  }
  if (temp) {
    tk_info(app$list_lang$tkmb1)
    return()
  }
  if (nrow(x) == 0) {
    tk_info(app$list_lang$tkmb2)
    return()
  }
  x <- unique(x[, c(
    "species_code",
    "nest_id",
    "statprov_code",
    "date",
    "lon",
    "lat"
  )])
  keep <- rep(TRUE, nrow(x))
  if (!is.logical(l$year)) {
    temp <- as.numeric(substr(x$date, 1, 4)) %in% l$year
    keep <- ifelse(!temp & keep, FALSE, keep)
  }
  #region subsetting
  temp <- !(is.na(x$lon) | is.na(x$lat))
  keep <- ifelse(!temp & keep, FALSE, keep)
  x <- x[keep, ]
  if (nrow(x) == 0) {
    tk_info(app$list_lang$tkmb3)
    return()
  }
  if (!is.logical(l$layer)) {
    pol <- get("usershp", envir = app$rnest)
    #pol<-readShapeSpatial(l$layer)
  } else {
    pol <- app$ecodistricts
  }
  xx <- x
  coordinates(xx) <- cbind(xx$lon, xx$lat, stringsAsFactors = FALSE)
  slot(pol, "data") <- data.frame(
    reg = apply(
      slot(pol, "data")[, l$id, drop = FALSE],
      1,
      paste,
      collapse = "_"
    ),
    stringsAsFactors = FALSE
  )
  if (!identical(l$region, FALSE)) {
    proj4string(xx) <- CRS(proj4string(pol))
    reg <- over(xx, pol)
  }
  #reg<-reg[,l$id,drop=F]
  #ici il faudrait trouver le moyen d'int\u00e9grer le polygone trac\u00e9 pour r\u00e9duire le temps du overlay tr\u00e8s important
  #reg<-apply(reg,1,paste,collapse="_")
  x11()
  par(mar = c(2, 0, 0, 0))
  plot(pol, col = ifelse(identical(l$region, FALSE), "white", "white"))
  if (!identical(l$region, FALSE)) {
    plot(pol[pol$reg %in% l$region, ], col = "lightgoldenrod", add = TRUE)
    x <- cbind(x, reg = reg[, 1, drop = FALSE], stringsAsFactors = FALSE)
    x <- x[x$reg %in% l$region, ]
    if (nrow(x) == 0) {
      tk_info(app$list_lang$tkmb4)
      return()
    }
  }
  points(x$lon, x$lat, col = "red", pch = 16)
}


app_mapeco <- function(app) {
  tkconfigure(app$tt, cursor = "watch")
  on.exit(tkconfigure(app$tt, cursor = "arrow"))
  l <- app_getopt(app)
  w <- which(app$ecodistricts@data[, "ecodistrict"] %in% l$ecodistrict)
  if (any(w)) {
    x11()
    par(mar = c(2, 0, 0, 0))
    plot(app$ecodistricts[w, ], col = "white", border = "white")
  } else {
    tk_info(app$list_lang$tkmb5)
    return()
  }
  plot(app$ecodistricts, add = TRUE)
  if (!is.logical(l$layer)) {
    fshp <- get("usershp", envir = app$rnest)
    plot(fshp, add = TRUE)
  }
  plot(
    app$ecodistricts[w, ],
    col = ifelse(
      !(app$ecodistricts@data[w, "ecodistrict"] %in% predictions$ecodistrict),
      gray(0.9),
      "lightgoldenrod"
    ),
    add = TRUE
  )
  coo <- coordinates(app$ecodistricts[w, ])
  text(
    coo[, 1],
    coo[, 2],
    app$ecodistricts@data[w, "ecodistrict"],
    cex = 0.7,
    font = 2
  )
}


app_mapsp <- function(app) {
  tkconfigure(app$tt, cursor = "watch")
  on.exit(tkconfigure(app$tt, cursor = "arrow"))
  l <- app_getopt(app)
  if (is.logical(l$sp)) {
    tk_info(app$list_lang$tkmb6)
    return()
  }
  w <- app$ecodistricts@data[, "ecodistrict"] %in%
    specodistricts$ecodis[specodistricts[, which(
      names(specodistricts) == l$sp[1]
    )]]
  if (any(w)) {
    x11()
    par(mar = c(2, 0, 0, 0))
    plot(app$ecodistricts)
    mtext(l$sp[1], side = 1, outer = FALSE, font = 2, cex = 1.5)
    plot(
      app$ecodistricts[w, ],
      col = "lightgoldenrod",
      border = "black",
      add = TRUE
    )
    if (!is.logical(l$layer)) {
      fshp <- get("usershp", envir = app$rnest)
      plot(fshp, add = TRUE)
    }
  } else {
    tk_info(app$list_lang$tkmb7)
    return()
  }
}


app_mapspbackground <- function(app, warnsp = TRUE) {
  tkconfigure(app$tt, cursor = "watch")
  on.exit(tkconfigure(app$tt, cursor = "arrow"))
  l <- app_getopt(app)
  if (is.logical(l$sp)) {
    return()
  }
  if (length(l$sp) > 1 && warnsp) {
    tk_info(app$list_lang$tkmb8)
  }
  w <- app$ecodistricts@data[, "ecodistrict"] %in%
    specodistricts$ecodis[specodistricts[, which(
      names(specodistricts) == l$sp[1]
    )]]
  if (any(w)) {
    plot(app$ecodistricts[w, ], col = "grey85", add = TRUE)
    return()
  }
}


app_run <- function(app) {
  tkconfigure(app$tt, cursor = "watch")
  on.exit(tkconfigure(app$tt, cursor = "arrow"))
  l <- app_getopt(app)
  assign("l", l, envir = app$rnest)
  nameback1 <- tclvalue(tkget(app$nameback))
  if (
    (is.logical(l$sp) && tclvalue(app$spgr) == "usp") ||
      (is.null(l$spgroup) && tclvalue(app$spgr) == "ugr")
  ) {
    tk_info(app$list_lang$tkmb9)
    return()
  }
  ans <- active.nest(
    app$d,
    sp = l$sp,
    year = l$year,
    layer = if (is.logical(l$layer)) {
      app$ecodistricts
    } else {
      get("usershp", envir = app$rnest)
    },
    region = l$region,
    id = l$id,
    int.choice = l$int.choice,
    zoom = l$zoom,
    locations = FALSE,
    spgroup = l$spgroup,
    useprecalc = l$useprecalc,
    progress = TRUE,
    lang = app$lang,
    fromtk = TRUE
  )
  if (nameback1 != "") {
    assign(nameback1, ans, envir = .GlobalEnv)
  }
  assign("ans", ans, envir = app$rnest)
}


app_calendarobs <- function(app) {
  tkconfigure(app$tt, cursor = "watch")
  on.exit(tkconfigure(app$tt, cursor = "arrow"))
  l <- app_getopt(app)
  if (
    (is.logical(l$sp) && tclvalue(app$spgr) == "usp") ||
      (is.null(l$spgroup) && tclvalue(app$spgr) == "ugr")
  ) {
    tk_info(app$list_lang$tkmb10)
    return()
  }
  if (
    !exists("l", envir = app$rnest) ||
      !identical(get("l", envir = app$rnest), l)
  ) {
    answer <- tkmessageBox(
      title = "",
      default = "no",
      message = app$list_lang$tkmb11,
      icon = "info",
      type = "yesno"
    )
    if (tclvalue(answer) == "yes") {
      app_run(app)
    }
    if (tclvalue(answer) == "no") {
      return()
    }
  }
  on.exit(tkconfigure(app$tt, cursor = "arrow"))
  namerawcal1 <- tclvalue(tkget(app$namerawcal))
  res <- nesting.activity(
    get("ans", envir = app$rnest),
    n = TRUE,
    dates = FALSE,
    by = tclvalue(app$cal),
    chronology = FALSE
  )
  if (namerawcal1 != "") {
    assign(namerawcal1, res, envir = .GlobalEnv)
  }
  prop <- tclvalue(tkget(app$prop))
  color <- tclvalue(tkget(app$colo))
  if (prop == "" || color == "") {
    if (prop != "" || color != "") {
      tk_info(app$list_lang$tkmb12)
    }
    prop <- c(0.000000000000, 0.01, 0.05, 0.10, 0.2, 0.4, 0.6, 0.8, 1)
    color <- c(
      "grey85",
      "grey65",
      brewer.pal(length(prop) - 4, "YlOrRd"),
      "darkred"
    )
  } else {
    prop <- as.numeric(unlist(strsplit(prop, ","))) / 100
    prop <- c(0.000000000000, prop[-1])
    color <- unlist(strsplit(color, ","))
  }
  colcons <- as.character(tclvalue(tkget(app$showpropcolo)))
  colcons <- ifelse(colcons == "", "deepskyblue", colcons)
  res <- res[, c("date", rev(names(res)[-1]))]
  nesting.calendar(
    res,
    color.scale = TRUE,
    dates = c(tclvalue(tkget(app$datebeg)), tclvalue(tkget(app$dateend))),
    type = tclvalue(app$cal),
    color = color,
    prop = prop,
    cons = as.numeric(tclvalue(tkget(app$showpropnest))),
    colcons = colcons,
    list_lang = app$list_lang
  )
}


app_calendarpred <- function(app) {
  tkconfigure(app$tt, cursor = "watch")
  on.exit(tkconfigure(app$tt, cursor = "arrow"))
  l <- app_getopt(app)
  leco <- l$ecodistrict
  if (identical(leco, "")) {
    tk_info(app$list_lang$tkmb13)
    return()
  }
  if (l$uspgroup == "uallsp") {
    lgro <- list(allsp = unique(app$sg[, "Species_ID"]))
  }
  if (l$uspgroup == "uselectsp") {
    if (is.logical(l$sp)) {
      tk_info(app$list_lang$tkmb14)
      return()
    }
    if (!any(l$sp %in% predictions$species_code)) {
      tk_info(app$list_lang$tkmb15)
      return()
    }
    lgro <- lapply(l$sp, function(i) {
      i
    })
    names(lgro) <- sapply(lgro, function(i) {
      i
    })
  }
  if (l$uspgroup == "ugroups") {
    lgro <- app_getgroups(app)
    if (is.null(lgro)) {
      tk_info(app$list_lang$tkmb16)
      return()
    }
  }
  if (!any(predictions[, "ecodistrict"] %in% leco)) {
    tk_info(app$list_lang$tkmb17)
    return()
  }
  cal <- nesting.pred(
    predictions,
    leco,
    lgro,
    order.by = tclvalue(app$ordercal)
  )
  if (ncol(cal) == 1) {
    tk_info(app$list_lang$tkmb18)
    return()
  }
  if ((ncol(cal) - 1) > 50) {
    tk_info(paste0(app$list_lang$tkmb19, ncol(cal) - 1))
    return()
  }
  #names(cal)<-gsub("\\.","___",names(cal))
  prop <- tclvalue(tkget(app$prop))
  color <- tclvalue(tkget(app$colo))
  if (prop == "" || color == "") {
    if (prop != "" || color != "") {
      tk_info(app$list_lang$tkmb20)
    }
    prop <- c(0.000000000000, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1)
    color <- c("white", brewer.pal(length(prop) - 3, "YlOrRd"), "darkred")
  } else {
    prop <- as.numeric(unlist(strsplit(prop, ","))) / 100
    prop <- c(0.000000000000, prop[-1])
    color <- unlist(strsplit(color, ","))
  }
  names(cal) <- gsub("___", "  ", names(cal))
  nesting.calendar.pred(
    cal,
    lebox = 35,
    name = "Ecodistrict",
    prop = prop,
    color = color,
    dates = c(tclvalue(tkget(app$datebeg)), tclvalue(tkget(app$dateend))),
    color.scale = ifelse(l$uspgroup == "uselectsp", FALSE, TRUE),
    list_lang = app$list_lang
  )
}


app_showshp <- function(app, info = "id") {
  l <- app_getopt(app)
  if (is.logical(l$layer)) {
    stop(app$list_lang$tkmb21)
  }
  fshp <- get("usershp", envir = app$rnest)
  #fshp<-readShapeSpatial(x)
  if (info == "id") {
    cat(
      "\n",
      "\n",
      paste(app$list_lang$tkmb22, app$x),
      "\n",
      "\n",
      "\n"
    )
    print(head(fshp@data))
    return(names(fshp@data))
  }
  if (info == "region") {
    l <- app_getopt(app)
    if (is.logical(l$id)) {
      stop(app$list_lang$tkmb23)
    }
    temp <- fshp@data[, l$id, drop = FALSE]
    temp <- apply(temp, 1, paste, collapse = "_")
    return(list(region = temp, shape = fshp))
  }
}


app_chooseshp <- function(app) {
  tkconfigure(app$tt, cursor = "watch")
  on.exit(tkconfigure(app$tt, cursor = "arrow"))
  x <- choose_file(filters = matrix(c("Shapefile", ".shp"), ncol = 2))
  if (is.null(x)) {
    return(invisible())
  }
  tkconfigure(app$shp, textvariable = tclVar(x))
  tkgrid(app$shp, row = 10, column = 1, sticky = "w") #adjust with final location row=5
  l <- app_getopt(app) # to load the shapefile and bring modifications to rnest envir
  #arg<-breakshapepath(x)
  #usershp<-readOGR(dsn=arg[1],layer=arg[2])
  #assign("usershp",usershp,envir=rnest)
}


app_choosesp <- function(app) {
  listsp <- as.character(tkget(app$listid, 0, "end"))
  x <- as.numeric(tkcurselection(app$listid)) + 1
  x <- paste(listsp[x], collapse = ",")
  tkconfigure(app$species, textvariable = tclVar(x))
  tkgrid(app$species, row = 3, column = 1, sticky = "w")
}


app_chooseid <- function(app) {
  ll <- app_showshp(app, info = "id")
  ttt <- tktoplevel()
  tcl("wm", "transient", ttt, app$tt)
  tktitle(ttt) <- "Attributes"
  listid <- tklistbox(
    ttt,
    height = ifelse(length(ll) < 3, 3, min(length(ll), 10)),
    selectmode = "multiple",
    background = "white",
    width = 30
  )
  listidlab <- tklabel(ttt, text = "Attributes")
  for (i in 1:length(ll)) {
    tkinsert(listid, "end", ll[i])
  }
  choice <- function() {
    x <- as.numeric(tkcurselection(listid)) + 1
    x <- paste(ll[x], collapse = ",")
    tkconfigure(app$id, textvariable = tclVar(x))
    tkgrid(app$id, row = 11, column = 1, sticky = "w")
    tkdestroy(ttt)
  }
  listidbut <- tkbutton(
    ttt,
    text = "OK",
    pady = 0,
    bg = "grey85",
    command = choice
  )
  scroll <- tkscrollbar(ttt, repeatinterval = 5, command = function(...) {
    tkyview(listid, ...)
  })
  tkgrid(tklabel(ttt, text = ""))
  tkgrid(listidlab, row = 0, column = 1)
  tkgrid(listid, row = 0, column = 2)
  tkgrid(listidbut, row = 0, column = 4)
  tkgrid(scroll, row = 0, rowspan = 1, column = 3, sticky = "ns")
  tkgrid(tklabel(ttt, text = ""))
}


app_chooseregion <- function(app) {
  ll <- app_showshp(app, info = "region")
  ttt <- tktoplevel()
  tcl("wm", "transient", ttt, app$tt)
  tktitle(ttt) <- "Regions"

  choixderegion <- unique(ll$region) # car on veut conserver le nb total pour faciliter le subsettage
  listregion <- tklistbox(
    ttt,
    height = 10,
    selectmode = "multiple",
    background = "white",
    width = 70
  )
  listregionlab <- tklabel(ttt, text = "Regions")
  for (i in 1:length(choixderegion)) {
    tkinsert(listregion, "end", choixderegion[i])
  }
  choice <- function() {
    x <- as.numeric(tkcurselection(listregion)) + 1
    x <- paste(choixderegion[x], collapse = ",")
    tkconfigure(app$region, textvariable = tclVar(x))
    tkgrid(app$region, row = 12, column = 1, sticky = "w")
    tkdestroy(ttt)
  }
  mapchoice <- function() {
    tkconfigure(app$tt, cursor = "watch")
    on.exit(tkconfigure(app$tt, cursor = "arrow"))
    x11()
    par(mar = c(2, 0, 0, 0))
    plot(ll$shape)
    l <- app_getopt(app)
    if (l$zoom) {
      mtext(
        app$list_lang$tkmb24,
        side = 1,
        outer = FALSE,
        font = 2,
        cex = 1.1
      )
      pol <- getpoly(quiet = TRUE)
      pol <- SpatialPolygons(list(Polygons(
        list(Polygon(pol[c(1:nrow(pol), 1), ])),
        ID = 1
      )))
      plot(
        ll$shape,
        xlim = range(bbox(pol)[1, ]),
        ylim = range(bbox(pol)[2, ])
      )
    }
    mtext(
      app$list_lang$tkmb25,
      side = 1,
      outer = FALSE,
      font = 2,
      cex = 1.1
    )
    polid <- NULL
    repeat {
      loc <- locator(1)
      if (is.null(loc)) {
        break
      }
      locx <- data.frame(x = loc$x, y = loc$y, stringsAsFactors = FALSE)
      coordinates(locx) <- cbind(locx$x, locx$y)
      proj4string(locx) <- CRS(proj4string(ll$shape))
      temp <- over(ll$shape, locx, returnList = FALSE)
      temp <- which(!is.na(temp[, 1]))
      if (!any(temp)) {
        next
      }
      temp <- temp[1]
      w <- which(ll$region == ll$region[temp])
      if (all(w %in% polid)) {
        polid <- polid[!polid %in% w]
        couleur <- "white"
      } else {
        polid <- c(polid, w)
        couleur <- "lightgoldenrod"
      }
      plot(ll$shape[w, ], add = TRUE, col = couleur)
    }
    polid <- sort(unique(polid))
    x <- paste(unique(ll$region[polid]), collapse = ",")
    tkconfigure(app$region, textvariable = tclVar(x))
    tkgrid(app$region, row = 12, column = 1, sticky = "w")
    tkdestroy(ttt)
  }
  listregionbut <- tkbutton(
    ttt,
    text = "OK",
    pady = 0,
    width = 5,
    bg = "grey85",
    command = choice
  )
  clickregionbut <- tkbutton(
    ttt,
    text = app$list_lang$tkmb26,
    pady = 0,
    width = 5,
    bg = "grey85",
    command = mapchoice
  )
  scroll <- tkscrollbar(ttt, repeatinterval = 5, command = function(...) {
    tkyview(listregion, ...)
  })
  tkgrid(tklabel(ttt, text = ""))
  tkgrid(listregionlab, row = 1, rowspan = 2, column = 0)
  tkgrid(listregion, row = 1, rowspan = 2, column = 1)
  tkgrid(listregionbut, row = 1, column = 3, sticky = "s")
  tkgrid(clickregionbut, row = 2, column = 3, sticky = "n")
  tkgrid(tklabel(ttt, text = ""))
  tkgrid(scroll, row = 1, rowspan = 2, column = 2, sticky = "ns")
}


app_chooseecodistrict <- function(app) {
  tkconfigure(app$tt, cursor = "watch")
  on.exit(tkconfigure(app$tt, cursor = "arrow"))
  l <- app_getopt(app)
  x11()
  par(mar = c(2, 0, 0, 0))
  plot(app$ecodistricts)
  warnsp <- TRUE
  if (l$backsp) {
    app_mapspbackground(app, warnsp = TRUE)
    warnsp <- FALSE
  }
  if (!is.logical(l$layer)) {
    fshp <- get("usershp", envir = app$rnest)
    plot(fshp, add = TRUE)
  }
  if (l$zoom) {
    mtext(
      app$list_lang$tkmb24,
      side = 1,
      outer = FALSE,
      font = 2,
      cex = 1.1
    )
    pol <- getpoly(quiet = TRUE)
    pol <- SpatialPolygons(
      list(Polygons(list(Polygon(pol[c(1:nrow(pol), 1), ])), ID = 1)),
      proj4string = CRS(proj4string(app$ecodistricts))
    )
    plot(
      app$ecodistricts,
      xlim = range(bbox(pol)[1, ]),
      ylim = range(bbox(pol)[2, ])
    )
  }
  if (l$backsp) {
    app_mapspbackground(app, warnsp = warnsp)
  }
  if (exists("fshp")) {
    plot(fshp, add = TRUE)
  }
  if (l$int.choice) {
    mtext(
      app$list_lang$tkmb27,
      side = 1,
      outer = FALSE,
      font = 2,
      cex = 1.1
    )
    pol <- getpoly(quiet = TRUE)
    polygon(pol, lwd = 4, border = "darkgreen")
    pol <- SpatialPolygons(
      list(Polygons(list(Polygon(pol[c(1:nrow(pol), 1), ])), ID = 1)),
      proj4string = CRS(proj4string(app$ecodistricts))
    )
    o <- over(pol, app$ecodistricts, returnList = TRUE)
    plot(app$ecodistricts[o[[1]], ], add = TRUE, col = "lightgoldenrod")
    if (exists("fshp")) {
      plot(fshp, add = TRUE)
    }
    plot(pol, lwd = 4, border = "darkgreen", add = TRUE)
    x <- paste(
      sort(unique(app$ecodistricts$ecodistrict[o[[1]]])),
      collapse = ","
    )
    tkconfigure(app$ecodistrict, textvariable = tclVar(x))
    tkgrid(app$ecodistrict, row = 11, column = 1, sticky = "w")
  } else {
    polid <- NULL
    repeat {
      mtext(
        app$list_lang$tkmb28,
        side = 1,
        outer = FALSE,
        font = 2,
        cex = 1.1
      )
      loc <- locator(1)
      if (is.null(loc)) {
        break
      }
      locx <- data.frame(x = loc$x, y = loc$y, stringsAsFactors = FALSE)
      coordinates(locx) <- cbind(locx$x, locx$y)
      proj4string(locx) <- CRS(proj4string(app$ecodistricts))
      temp <- over(locx, app$ecodistricts, returnList = FALSE)[,
        "ecodistrict",
        drop = TRUE
      ]
      if (is.na(temp)) {
        next
      }
      w <- which(app$ecodistricts$ecodistrict == temp)
      if (all(w %in% polid)) {
        polid <- polid[!polid %in% w]
        couleur <- "white"
      } else {
        polid <- c(polid, w)
        couleur <- "lightgoldenrod"
      }
      plot(app$ecodistricts[w, ], add = TRUE, col = couleur)
    }
    if (exists("fshp")) {
      plot(fshp, add = TRUE)
    }
    polid <- sort(unique(polid))
    x <- paste(unique(app$ecodistricts$ecodistrict[polid]), collapse = ",")
    tkconfigure(app$ecodistrict, textvariable = tclVar(x))
    tkgrid(app$ecodistrict, row = 11, column = 1, sticky = "w")
  }
}


app_getgroups <- function(app, nn = app$n) {
  members <- lapply(1:nn, function(i) {
    x <- as.character(tkget(
      get(paste("groupm", i, sep = ""), envir = app),
      0,
      "end"
    ))
    if (length(x) == 0) {
      NA
    } else {
      x
    }
  })
  group <- sapply(1:nn, function(i) {
    x <- as.character(tkget(get(paste("group", i, sep = ""), envir = app)))
    if (length(x) == 0) {
      NA
    } else {
      x
    }
  })
  if (all(is.na(group)) || all(is.na(unlist(members)))) {
    return(NULL)
  }
  names(members) <- group
  members <- members[!is.na(names(members))]
  #members<-members[sapply(members,function(i){!any(is.na(i))})]
  members
}


app_propfun <- function(app) {
  tk_info(paste0(app$list_lang$tkmb34, tclvalue(tkget(app$prop))))
}


app_showcol <- function(app) {
  tk_info(paste0(app$list_lang$tkmb35, tclvalue(tkget(app$colo))))
}


app_showpropcol <- function(app) {
  tk_info(paste0(app$list_lang$tkmb36))
}


app_butmask <- function(
  app,
  n = 1,
  r = 1,
  s = 1,
  remove = FALSE,
  predict = get("predict", envir = app$rnest)
) {
  old_mode <- tclServiceMode(FALSE)
  on.exit(tclServiceMode(old_mode), add = TRUE)
  if (remove) {
    tkgrid.remove(get(paste(names(app$lgroups)[n], "mask"), envir = app$rnest))
    for (j in 1:(length(app$lgroups[[n]]))) {
      assign(
        app$lgroups[[n]][j],
        tkcheckbutton(app$tt, command = function(x) {
          app_relistsp(app, app$sg, app$lgroups)
        }),
        envir = app$rnest
      )
      assign(
        paste(names(app$lgroups)[n], app$lgroups[[n]][j], "val", sep = "_"),
        tclVar(ifelse(
          predict &&
            paste(
              names(app$lgroups)[n],
              app$lgroups[[n]][j],
              "val",
              sep = "_"
            ) ==
              "model_output_rejected_val",
          "0",
          "1"
        )),
        envir = app$rnest
      )
      tkconfigure(
        get(app$lgroups[[n]][j], envir = app$rnest),
        variable = get(
          paste(names(app$lgroups)[n], app$lgroups[[n]][j], "val", sep = "_"),
          envir = app$rnest
        ),
        text = app$lgroupsl[[n]][j]
      )
      tkgrid(
        get(app$lgroups[[n]][j], envir = app$rnest),
        row = r + j,
        column = 6,
        rowspan = 1,
        sticky = "w"
      )
    }
  } else {
    for (j in 1:(length(app$lgroups[[n]]))) {
      assign(
        app$lgroups[[n]][j],
        tkcheckbutton(app$tt, command = function(x) {
          app_relistsp(app, app$sg, app$lgroups)
        }),
        envir = app$rnest
      )
      assign(
        paste(names(app$lgroups)[n], app$lgroups[[n]][j], "val", sep = "_"),
        tclVar(ifelse(
          predict &&
            paste(
              names(app$lgroups)[n],
              app$lgroups[[n]][j],
              "val",
              sep = "_"
            ) ==
              "model_output_rejected_val",
          "0",
          "1"
        )),
        envir = app$rnest
      )
      tkconfigure(
        get(app$lgroups[[n]][j], envir = app$rnest),
        variable = get(
          paste(names(app$lgroups)[n], app$lgroups[[n]][j], "val", sep = "_"),
          envir = app$rnest
        ),
        text = app$lgroupsl[[n]][j]
      )
      tkgrid(
        get(app$lgroups[[n]][j], envir = app$rnest),
        row = r + j,
        column = 6,
        rowspan = 1,
        sticky = "w"
      )
    }
    assign(
      paste(names(app$lgroups)[n], "mask"),
      tkbutton(
        app$tt,
        text = gsub("_", " ", names(app$lgroupsl)[n]),
        width = 12,
        command = function() {
          tkgrid.remove(get(
            paste(names(app$lgroups)[n], "mask"),
            envir = app$rnest
          ))
          app_relistsp(app, app$sg, app$lgroups)
        }
      ),
      envir = app$rnest
    )
    tkgrid(
      get(paste(names(app$lgroups)[n], "mask"), envir = app$rnest),
      row = r,
      column = 6,
      rowspan = s + 1,
      sticky = "nws"
    )
  }
}


app_rejected <- function(app, fromobsbut = FALSE) {
  if (fromobsbut) {
    assign(
      "model_output_rejected_val",
      tclVar(ifelse(get("predict", envir = app$rnest), "0", "0")),
      envir = app$rnest
    )
    tkconfigure(
      get("rejected", envir = app$rnest),
      variable = get("model_output_rejected_val", envir = app$rnest),
      text = app$lgroupsl[[5]][2]
    )
    tkgrid(
      get("rejected", envir = app$rnest),
      row = 31,
      column = 6,
      rowspan = 1,
      sticky = "w"
    )
  } else {
    if (!get("rejected_status", envir = app$rnest)) {
      assign(
        "model_output_rejected_val",
        tclVar(ifelse(!get("predict", envir = app$rnest), "1", "0")),
        envir = app$rnest
      )
      tkconfigure(
        get("rejected", envir = app$rnest),
        variable = get("model_output_rejected_val", envir = app$rnest),
        text = app$lgroupsl[[5]][2]
      )
      tkgrid(
        get("rejected", envir = app$rnest),
        row = 31,
        column = 6,
        rowspan = 1,
        sticky = "w"
      )
    }
  }
}


app_butmaskgroups <- function(app, n = 6, val = "1", remove = FALSE) {
  old_mode <- tclServiceMode(FALSE)
  on.exit(tclServiceMode(old_mode), add = TRUE)
  pos <- 20
  co <- 9
  if (remove) {
    tkgrid.remove(get(paste(names(app$lgroups)[n], "mask"), envir = app$rnest))
    for (j in 1:length(app$lgroups[[n]])) {
      assign(
        app$lgroups[[n]][j],
        tkcheckbutton(app$tt, command = function(x) {
          app_relistsp(app, app$sg, app$lgroups)
        }),
        envir = app$rnest
      )
      assign(
        paste(names(app$lgroups)[n], app$lgroups[[n]][j], "val", sep = "_"),
        tclVar(val),
        envir = app$rnest
      )
      tkconfigure(
        get(app$lgroups[[n]][j], envir = app$rnest),
        variable = get(
          paste(names(app$lgroups)[n], app$lgroups[[n]][j], "val", sep = "_"),
          envir = app$rnest
        ),
        text = app$lgroupsl[[n]][j]
      )
      tkgrid(
        get(app$lgroups[[n]][j], envir = app$rnest),
        row = pos,
        column = co,
        columnspan = 4,
        rowspan = 1,
        sticky = "w"
      )
      pos <- pos + 1
      if (pos == 35) {
        pos <- 18
        co <- co + 4
      }
    }
  } else {
    for (j in 1:length(app$lgroups[[n]])) {
      assign(
        app$lgroups[[n]][j],
        tkcheckbutton(app$tt, command = function(x) {
          app_relistsp(app, app$sg, app$lgroups)
        }),
        envir = app$rnest
      )
      assign(
        paste(names(app$lgroups)[n], app$lgroups[[n]][j], "val", sep = "_"),
        tclVar(val),
        envir = app$rnest
      )
      tkconfigure(
        get(app$lgroups[[n]][j], envir = app$rnest),
        variable = get(
          paste(names(app$lgroups)[n], app$lgroups[[n]][j], "val", sep = "_"),
          envir = app$rnest
        ),
        text = app$lgroupsl[[n]][j]
      )
      tkgrid(
        get(app$lgroups[[n]][j], envir = app$rnest),
        row = pos,
        column = co,
        columnspan = 4,
        rowspan = 1,
        sticky = "w"
      )
      pos <- pos + 1
      if (pos == 35) {
        pos <- 18
        co <- co + 4
      }
    }
    if (val == "1") {
      assign(
        paste(names(app$lgroups)[n], "mask"),
        tkbutton(
          app$tt,
          text = gsub("_", " ", names(app$lgroupsl)[n]),
          width = 12,
          command = function() {
            tkgrid.remove(get(
              paste(names(app$lgroups)[n], "mask"),
              envir = app$rnest
            ))
            app_relistsp(app, app$sg, app$lgroups)
          }
        ),
        envir = app$rnest
      )
      tkgrid(
        get(paste(names(app$lgroups)[n], "mask"), envir = app$rnest),
        row = 18,
        column = 9,
        rowspan = 17,
        columnspan = 12,
        sticky = "nwse"
      )
    }
  }
}


app_getcheckbut <- function(app, lg = app$lgroups) {
  ans <- llply(names(lg), function(i) {
    res <- paste(i, lg[[i]], sep = "_")
    res <- sapply(res, function(j) {
      as.logical(as.numeric(tclvalue(get(
        paste(j, "val", sep = "_"),
        envir = app$rnest
      ))))
    })
    res
  })
  names(ans) <- names(lg)
  ans
}


app_relistsp <- function(app, sg, lg) {
  ans <- app_getcheckbut(app)
  if (!any(unlist(ans))) {
    tkdelete(app$listid, 0, "end")
    return()
  }
  old_mode <- tclServiceMode(FALSE)
  on.exit(tclServiceMode(old_mode), add = TRUE)
  temp <- list()
  for (i in 1:length(ans)) {
    temp[[i]] <- apply(
      sg[, -1, drop = FALSE][, names(ans[[i]])[ans[[i]]], drop = FALSE],
      1,
      function(i) {
        any(as.logical(i))
      }
    )
  }
  temp <- do.call("cbind", temp)
  temp <- apply(temp, 1, all)
  subsp <- sg[, 1][sg[, 1] %in% sg[temp, 1]]
  if (get("predict", envir = app$rnest)) {
    subsp <- subsp[subsp %in% predictions$species_code]
  } else {
    subsp <- subsp[subsp %in% app$d$species_code]
  }
  tkdelete(app$listid, 0, "end")
  if (length(subsp) > 0) {
    for (j in 1:length(subsp)) {
      tkinsert(app$listid, "end", subsp[j])
    }
    tkgrid(app$listid, row = 3, rowspan = 32, column = 4, sticky = "ns")
  }
}


app_selection <- function(app, k = 1) {
  #x<-ll[as.numeric(tkcurselection(listid))+1]

  listsp <- as.character(tkget(app$listid, 0, "end"))
  x <- listsp[as.numeric(tkcurselection(app$listid)) + 1]

  xm <- as.character(tkget(
    get(paste("groupm", k, sep = ""), envir = app),
    0,
    "end"
  ))
  if (length(xm) == 0) {
    xm <- NA
  }
  x <- setdiff(x, xm)
  if (length(x)) {
    for (j in 1:length(x)) {
      tkinsert(get(paste("groupm", k, sep = ""), envir = app), "end", x[j])
    }
    tkselection.clear(app$listid, 0, "end")
  }
}


app_clearselection <- function(app, k = 1, all = FALSE) {
  x <- as.character(tkget(
    get(paste("groupm", k, sep = ""), envir = app),
    0,
    "end"
  ))
  y <- as.integer(tkcurselection(get(
    paste("groupm", k, sep = ""),
    envir = app
  ))) +
    1
  if (length(y) > 0) {
    x <- x[-y]
  }
  tkdelete(get(paste("groupm", k, sep = ""), envir = app), 0, "end")
  if (length(x) == 0 || all) {
    return()
  }
  for (j in seq(x)) {
    tkinsert(get(paste("groupm", k, sep = ""), envir = app), "end", x[j])
  }
}


app_group2species <- function(app, no) {
  g <- app_getgroups(app)
  x <- g[[no]]
  y <- as.integer(tkcurselection(get(
    paste("groupm", no, sep = ""),
    envir = app
  ))) +
    1
  if (length(y) > 0) {
    x <- x[y]
  }
  x <- paste(x, collapse = ",")
  tkconfigure(app$species, textvariable = tclVar(x))
  tkgrid(app$species, row = 3, column = 1, sticky = "w")
}


app_loaddata <- function(app) {
  tt <- tktoplevel()
  tktitle(tt) <- app$list_lang$loaddatalabl
  lab1 <- tklabel(tt, text = app$list_lang$lab1labl)
  lab2 <- tklabel(tt, text = app$list_lang$lab2labl)
  lab3 <- tklabel(tt, text = app$list_lang$lab3labl)
  lab4 <- tklabel(tt, text = app$list_lang$lab4labl)
  ent1 <- tkentry(
    tt,
    width = 50,
    textvariable = tclVar(get("path1", envir = app$rnest))
  )
  ent2 <- tkentry(
    tt,
    width = 50,
    textvariable = tclVar(get("path2", envir = app$rnest))
  )
  ent3 <- tkentry(
    tt,
    width = 50,
    textvariable = tclVar(get("path3", envir = app$rnest))
  )
  ent4 <- tkentry(
    tt,
    width = 50,
    textvariable = tclVar(get("path4", envir = app$rnest))
  )

  onok <- function() {
    x1 <- as.character(tclvalue(tkget(ent1)))
    path1 <- tclVar(x1)
    assign("nestdata", x1, envir = app$rnest)
    assign("path1", x1, envir = app$rnest)
    x2 <- tclvalue(tkget(ent2))
    path2 <- tclVar(x2)
    assign("nestparam", x2, envir = app$rnest)
    assign("path2", x2, envir = app$rnest)
    x3 <- tclvalue(tkget(ent3))
    path3 <- tclVar(x3)
    assign("neststatus", x3, envir = app$rnest)
    assign("path3", x3, envir = app$rnest)
    x4 <- tclvalue(tkget(ent4))
    path4 <- tclVar(x4)
    #if(x4==""){stop("No bird groups provided")}
    #x4<-read.csv(x4,header=T,stringsAsFactors=FALSE)
    assign("birdgroups", x4, envir = app$rnest)
    assign("path4", x4, envir = app$rnest)
    #print(ls(rnest))
    tkdestroy(tt)
  }

  browse <- function(num) {
    x <- choose_file()
    if (is.null(x)) {
      return(invisible())
    }
    tkconfigure(get(paste("ent", num, sep = "")), textvariable = tclVar(x))
    tkgrid(get(paste("ent", num, sep = "")), row = num * 2, column = 0)
  }

  but1 <- tkbutton(
    tt,
    text = app$list_lang$butbrowselabl,
    pady = 0,
    width = 10,
    bg = "grey85",
    command = function() browse(1)
  )
  but2 <- tkbutton(
    tt,
    text = app$list_lang$butbrowselabl,
    pady = 0,
    width = 10,
    bg = "grey85",
    command = function() browse(2)
  )
  but3 <- tkbutton(
    tt,
    text = app$list_lang$butbrowselabl,
    pady = 0,
    width = 10,
    bg = "grey85",
    command = function() browse(3)
  )
  but4 <- tkbutton(
    tt,
    text = app$list_lang$butbrowselabl,
    pady = 0,
    width = 10,
    bg = "grey85",
    command = function() browse(4)
  )
  ok <- tkbutton(
    tt,
    text = "OK",
    pady = 0,
    width = 10,
    bg = "grey85",
    command = onok
  )
  tkgrid(tklabel(tt, text = "     "), row = 0, column = 0)
  tkgrid(lab1, sticky = "w", row = 1)
  tkgrid(ent1, but1, row = 2)
  tkgrid(lab2, sticky = "w", row = 3)
  tkgrid(ent2, but2, row = 4)
  tkgrid(lab3, sticky = "w", row = 5)
  tkgrid(ent3, but3, row = 6)
  tkgrid(lab4, sticky = "w", row = 7)
  tkgrid(ent4, but4, row = 8)
  tkgrid(ok, columnspan = 2, row = 9)
}


app_widget <- function(app, predict = FALSE) {
  assign("predict", predict, envir = app$rnest)
  if (!predict) {
    #tkconfigure(tt,bg="lightgreen")
    tkgrid(tklabel(app$tt, text = ""), columnspan = 1, row = 0)
    tkgrid(app$header1, columnspan = 4, row = 1)
    tkgrid(
      tklabel(app$tt, text = "                                   "),
      column = 0,
      row = 2
    )
    tkgrid(app$speciesb, row = 3, column = 0)
    tkgrid(app$species, row = 3, column = 1, sticky = "w")
    tkgrid(app$speciesq, row = 3, column = 2)
    tkgrid(app$yearlab, row = 4, column = 0)
    tkgrid(app$year, row = 4, column = 1, sticky = "w")
    tkgrid(app$yearq, row = 4, column = 2)
    tkgrid(app$usp, row = 5, column = 1, sticky = "w")
    tkgrid(app$ugr, row = 6, column = 1, sticky = "w")
    tkgrid(tklabel(app$tt, text = ""), row = 5)
    tkgrid(tklabel(app$tt, text = ""), row = 6)
    tkgrid(tklabel(app$tt, text = ""), row = 7)
    tkgrid(app$header2, columnspan = 4, row = 8)
    tkgrid(tklabel(app$tt, text = ""), row = 9)
    tkgrid(app$shpb, row = 10, column = 0)
    tkgrid(app$shp, row = 10, column = 1, sticky = "w")
    tkgrid(app$shpq, row = 10, column = 2)
    tkgrid(app$idb, row = 11, column = 0)
    tkgrid(app$id, row = 11, column = 1, sticky = "w")
    tkgrid(app$idq, row = 11, column = 2)
    tkgrid(app$regionb, row = 12, column = 0)
    tkgrid(app$region, row = 12, column = 1, sticky = "w")
    tkgrid(app$regionq, row = 12, column = 2)
    tkgrid(app$intchoicelab, row = 13, column = 0)
    tkgrid(app$intchoice, row = 13, column = 1, sticky = "w")
    tkgrid(app$intchoiceq, row = 13, column = 2)
    tkgrid(app$zoomlab, row = 14, column = 0)
    tkgrid(app$zoom, row = 14, column = 1, sticky = "w")
    tkgrid(app$zoomq, row = 14, column = 2)
    tkgrid(app$backsplab, row = 15, column = 0)
    tkgrid(app$backsp, row = 15, column = 1, sticky = "w")
    tkgrid(app$backspq, row = 15, column = 2)
    tkgrid(tklabel(app$tt, text = ""), row = 16)
    #tkgrid(tklabel(tt,text=""),row=17)
    tkgrid(app$butrun, column = 0, row = 17, columnspan = 4)
    tkgrid(app$butrunq, column = 2, row = 17)
    tkgrid(app$butmap, column = 0, row = 18, columnspan = 4)
    tkgrid(app$butmapq, column = 2, row = 18)
    #tkgrid(tklabel(tt,text=""),row=18)
    tkgrid(app$useprecalc, row = 20, column = 1, sticky = "w")
    tkgrid(app$nameback, row = 20, column = 0, sticky = "e")
    tkgrid(app$namebackq, row = 20, column = 2)
    tkgrid(tklabel(app$tt, text = ""), row = 21)
    tkgrid(app$header3, columnspan = 4, row = 22)
    tkgrid(tklabel(app$tt, text = ""), row = 23)
    tkgrid(app$datelab, row = 24, sticky = "e", column = 0)
    tkgrid(app$datebeg, row = 25, sticky = "e", column = 0)
    tkgrid(app$dateend, row = 26, sticky = "e", column = 0)
    tkgrid(app$yearcal, row = 26, column = 1, sticky = "w")
    tkgrid(app$spcal, row = 24, column = 1, sticky = "w")
    tkgrid(app$regioncal, row = 25, column = 1, sticky = "w")
    tkgrid(tklabel(app$tt, text = ""), row = 27)
    tkgrid(app$propbut, row = 28, column = 0)
    tkgrid(app$prop, row = 28, column = 1, sticky = "e")
    tkgrid(app$propq, row = 28, column = 2)
    tkgrid(app$colobut, row = 29, column = 0)
    tkgrid(app$colo, row = 29, column = 1, sticky = "e")
    tkgrid(app$coloq, row = 29, column = 2)
    tkgrid(app$showpropbut, row = 30)
    tkgrid(app$showpropcolo, row = 30, column = 1, sticky = "w")
    tkgrid(app$showpropnest, row = 30, column = 1, sticky = "e")
    tkgrid(app$showpropq, row = 30, column = 2)
    tkgrid(tklabel(app$tt, text = ""), row = 31)
    tkgrid(app$butcal, row = 32, columnspan = 4)
    tkgrid(app$butcalq, row = 32, column = 2)
    tkgrid(tklabel(app$tt, text = ""), row = 33)
    tkgrid(app$namerawcal, row = 34, column = 0, sticky = "e")
    tkgrid(app$namerawcalq, row = 34, column = 2)
    tkgrid(tklabel(app$tt, text = ""), row = 35)
    tkgrid(tklabel(app$tt, text = "   "), row = 0, column = app$n + 8 + 1)
    tkgrid(app$groupq, row = 34, column = 6, sticky = "w")
    tkgrid(app$butobs, column = 0, row = 36, columnspan = 100, sticky = "we")
    #tkgrid(tklabel(tt,text=""),row=37)
    tkgrid.remove(app$butsp, app$butspq)
    tkgrid.remove(app$butpred)
    tkgrid.remove(app$buteco, app$butecoq)
    tkgrid.remove(app$ecodistrictb, app$ecodistrict, app$ecodistrictq)
    tkgrid.remove(app$uselectsp, app$uallsp, app$ugroups)
    tkgrid.remove(app$orderspgrcal, app$orderecocal)
    #l<-getopt()
    #if(is.logical(l$layer)){assign("usershp",ecodistricts,envir=rnest)}
  } else {
    #tkconfigure(tt,bg="lightblue")
    tkgrid.remove(app$yearlab, app$year, app$yearq)
    tkgrid.remove(app$usp, app$ugr)
    tkgrid.remove(app$idb, app$id, app$idq)
    tkgrid.remove(app$regionb, app$region, app$regionq)
    tkgrid.remove(app$butrun, app$butrunq)
    tkgrid.remove(app$butmap, app$butmapq)
    tkgrid.remove(app$useprecalc)
    tkgrid.remove(app$nameback, app$namebackq)
    tkgrid(app$orderspgrcal, row = 25, column = 1, sticky = "w")
    tkgrid(app$orderecocal, row = 26, column = 1, sticky = "w")
    tkgrid(app$uselectsp, row = 5, column = 1, sticky = "w")
    tkgrid(app$uallsp, row = 4, column = 1, sticky = "w")
    tkgrid(app$ugroups, row = 6, column = 1, sticky = "w")
    tkgrid(app$ecodistrictb, row = 11, column = 0)
    tkgrid(app$ecodistrict, row = 11, column = 1, sticky = "w")
    tkgrid(app$ecodistrictq, row = 11, column = 2)
    tkgrid(app$butsp, column = 0, row = 17, columnspan = 4)
    tkgrid(app$butspq, column = 2, row = 17)
    tkgrid(app$buteco, column = 0, row = 18, columnspan = 4)
    tkgrid(app$butecoq, column = 2, row = 18)
    tkgrid(app$butpred, column = 0, row = 36, columnspan = 100, sticky = "we")
    tkgrid.remove(app$butobs)
    tkgrid.remove(app$yearcal, app$spcal, app$regioncal)
    #l<-getopt()
    #if(is.logical(l$layer)){assign("usershp",NULL,envir=rnest)}
  }
}


tk.single.nest <- function(list_lang) {
  old_mode <- tclServiceMode(FALSE)
  on.exit(tclServiceMode(old_mode), add = TRUE)

  tt <- tktoplevel()
  tktitle(tt) <- list_lang$titlesnlabl
  header1 <- tklabel(tt, text = list_lang$header1snlabl)
  tkconfigure(header1, foreground = "blue")
  header2 <- tklabel(tt, text = list_lang$header2snlabl)
  header22 <- tklabel(tt, text = list_lang$header22snlabl)
  titre1 <- tklabel(tt, text = list_lang$titre1snlabl)
  titre2 <- tklabel(tt, text = list_lang$titre2snlabl)
  titre3 <- tklabel(tt, text = list_lang$titre3snlabl)
  valeur1 <- tkentry(tt, width = 10, textvariable = tclVar(""))
  valeur2 <- tkentry(tt, width = 10, textvariable = tclVar(""))
  valeur3 <- tkentry(tt, width = 10, textvariable = tclVar(""))
  name1 <- tkentry(tt, width = 20, textvariable = tclVar(""))
  d <- nestwatch

  # add button to print raw data or not
  but1 <- function() {
    y <- d[d$nest_id == sample(unique(d$nest_id), 1), ]
    ans <- chronology(y, test = TRUE)
    temp <- tclvalue(tkget(name1))
    if (temp != "") {
      assign(temp, ans, envir = .GlobalEnv)
    }
    cat_brs(4)
    print(ans)
    if (length(ans) == 3) {
      cat("\n")
      print(y)
    }
  }
  but2 <- function() {
    x <- tclvalue(tkget(valeur2))
    if (!any(d$species_code == x)) {
      stop(list_lang$tkmb39)
    }
    y <- d[d$nest_id == sample(unique(d$nest_id[d$species_code == x]), 1), ]
    ans <- chronology(y, test = TRUE)
    temp <- tclvalue(tkget(name1))
    if (temp != "") {
      assign(temp, ans, envir = .GlobalEnv)
    }
    cat_brs(4)
    print(ans)
    if (length(ans) == 3) {
      cat("\n")
      print(y)
    }
  }
  but3 <- function() {
    x <- tclvalue(tkget(valeur3))
    if (!any(d$nest_id == x)) {
      stop(list_lang$tkmb40)
    }
    y <- d[d$nest_id == x, ]
    ans <- chronology(y, test = TRUE)
    temp <- tclvalue(tkget(name1))
    if (temp != "") {
      assign(temp, ans, envir = .GlobalEnv)
    }
    cat_brs(4)
    print(ans)
    if (length(ans) == 3) {
      cat_brs()
      print(y)
    }
  }
  bouton1 <- tkbutton(tt, text = list_lang$bouton, command = but1)
  bouton2 <- tkbutton(tt, text = list_lang$bouton, command = but2)
  bouton3 <- tkbutton(tt, text = list_lang$bouton, command = but3)
  boutonc <- tkbutton(
    tt,
    text = list_lang$boutonc,
    command = function() {
      tkdestroy(tt)
    }
  )
  #tkwm.geometry(tt, "245x300")
  tkwm.resizable(tt, FALSE, FALSE)
  tkgrid(tklabel(tt, text = ""), row = 0, column = 0, columnspan = 2)
  tkgrid(header1, row = 1, columnspan = 4)
  tkgrid(tklabel(tt, text = ""), row = 2, column = 0, columnspan = 2)
  tkgrid(titre1, valeur1)
  tkgrid(titre2, valeur2)
  tkgrid(titre3, valeur3)
  tkgrid(bouton1, row = 3, column = 2, columnspan = 2)
  tkgrid(bouton2, row = 4, column = 2, columnspan = 2)
  tkgrid(bouton3, row = 5, column = 2, columnspan = 2)
  tkgrid(tklabel(tt, text = ""), columnspan = 2)
  tkgrid(header2, columnspan = 4)
  tkgrid(header22, columnspan = 4)
  tkgrid(tklabel(tt, text = ""), columnspan = 2)
  tkgrid(name1, columnspan = 4)
  tkgrid(tklabel(tt, text = ""), columnspan = 2)
  tkgrid(boutonc, columnspan = 4)
  tkgrid(tklabel(tt, text = ""), columnspan = 2)
}


tk.single.nest.test <- function(
  dates = c("2000-05-01", "2000-05-30"),
  list_lang
) {
  old_mode <- tclServiceMode(FALSE)
  on.exit(tclServiceMode(old_mode), add = TRUE)

  tt <- tktoplevel()
  tktitle(tt) <- list_lang$titlesntlabl
  deslab <- tklabel(tt, text = list_lang$deslabl)
  tkconfigure(deslab, foreground = "blue")
  incublab <- tklabel(tt, text = list_lang$incublabl)
  rearlab <- tklabel(tt, text = list_lang$rearlabl)
  laylab <- tklabel(tt, text = list_lang$laylabl)
  beglab <- tklabel(tt, text = list_lang$beglabl)
  clutchlab <- tklabel(tt, text = list_lang$clutchlabl)
  clutchminlab <- tklabel(tt, text = list_lang$clutchminlabl)
  clutchmaxlab <- tklabel(tt, text = list_lang$clutchmaxlabl)

  useminclutchlab <- tklabel(
    tt,
    text = list_lang$useminclutchlabl
  )
  incflexplab <- tklabel(tt, text = list_lang$incflexplabl)
  incflexmlab <- tklabel(tt, text = list_lang$incflexmlabl)

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
  eggs <- tklabel(tt, text = list_lang$eggslabl)
  young <- tklabel(tt, text = list_lang$younglabl)
  codes <- tklabel(tt, text = "codes")
  separator <- "============================================================================"
  dateini <- as.Date(dates[1])
  n <- as.numeric(abs(difftime(as.Date(dates[2]), dateini))) + 1
  if (n < 10) {
    stop(list_lang$period10labl)
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
  # one row per date: date, eggs, young and codes in columns 0 to 3
  for (i in 1:n) {
    tkgrid(
      get(paste("date", i, sep = "")),
      get(paste("eggs", i, sep = "")),
      get(paste("young", i, sep = "")),
      get(paste("codes", i, sep = "")),
      row = i + 3
    )
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
    ans <- chronology(
      dat,
      nparam = nparam,
      test = TRUE,
      minclutch = minclutch,
      rel.flex = rel.flex,
      lsc = lsc
    )
    cat_brs(4)
  }
  chdate <- function() {
    ttt <- tktoplevel()
    chdatelab1 <- tklabel(ttt, text = list_lang$chdatelab1labl)
    chdatelab2 <- tklabel(ttt, text = list_lang$chdatelab2labl)
    chdate1 <- tkentry(ttt, width = 10, textvariable = tclVar(""))
    chdate2 <- tkentry(ttt, width = 10, textvariable = tclVar(""))
    newin <- function() {
      d1 <- tclvalue(tkget(chdate1))
      d2 <- tclvalue(tkget(chdate2))
      if ((abs(as.numeric(difftime(as.Date(d1), as.Date(d2)))) + 1) < 14) {
        stop(list_lang$newinlabl)
      }
      tkdestroy(tt)
      tkdestroy(ttt)
      tk.single.nest.test(dates = c(d1, d2), list_lang = list_lang)
    }
    refresh <- tkbutton(
      ttt,
      text = list_lang$refreshlabl,
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
    text = list_lang$bouton1sntlabl,
    command = ff
  )
  bouton3 <- tkbutton(
    tt,
    text = list_lang$bouton3sntlabl,
    command = writebug
  )
  bouton2 <- tkbutton(
    tt,
    text = list_lang$bouton2sntlabl,
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
}


cat_brs <- function(n = 1) {
  cat(rep("\n", n))
}


# Shows an information message box.
# Asks for one file with the Tk file dialog (file.choose() prompts in the
# console on Unix). Returns the path with forward slashes, or NULL if cancelled.
choose_file <- function(...) {
  x <- tk_choose.files(multi = FALSE, ...)
  if (length(x) == 0 || identical(x, "")) {
    return(NULL)
  }
  gsub("\\\\", "/", x)
}


tk_info <- function(message) {
  tkmessageBox(title = "", message = message, icon = "info", type = "ok")
}


# Reads the species groupings offered in the interface.
species_groups <- function() {
  file <- system.file("extdata", "species_groups.yml", package = "rNest")
  lapply(yaml::yaml.load_file(file), function(x) lapply(x, unlist))
}
