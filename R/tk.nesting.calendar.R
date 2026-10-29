tk.nesting.calendar <- function(
  grid = FALSE,
  ecodistricts = ecodistricts,
  lang = "en"
) {
  list_lang <- i18n("tk.nesting.calendar", lang)
  rnest <- new.env()
  assign("list_lang", list_lang, envir = rnest)
  for (i in seq_along(list_lang)) {
    assign(names(list_lang)[i], list_lang[[i]], envir = rnest)
  }
  assign("usershp", NULL, envir = rnest) ### to put the name in the environment
  assign("usershppath", FALSE, envir = rnest) ### to put the name in the environment
  assign("predict", FALSE, envir = rnest)
  assign("rejected_status", FALSE, envir = rnest)
  n <- 6

  #species grouping info

  lg <- list(
    bird_status = c("Yes", "No"),
    bird_type = c("Landbirds", "Waterbirds", "Waterfowl", "Shorebirds"),
    habitat_type = c("tf", "to", "we"),
    nest_location = c("gro", "wat", "bot", "cav", "man", "cli", "col"),
    model_output = c(1, 0),
    bird_group = c(
      "Geese",
      "Swans",
      "Dabbling",
      "Diving",
      "Grouses and allies",
      "Loons and Grebes",
      "Petrels and allies",
      "Cormorants",
      "Pelicans",
      "Herons and allies",
      "Hawks and allies",
      "Rails and Cranes",
      "Shorebirds",
      "Gulls",
      "Terns",
      "Jaegers",
      "Auks and allies",
      "Doves and allies",
      "Cuckoos",
      "Owls and allies",
      "Nightjars",
      "Swallows and allies",
      "Hummingbirds",
      "Kingfishers",
      "Woodpeckers",
      "Flychatchers and allies",
      "Shrikes",
      "Vireos",
      "Corvids",
      "Pipits and allies",
      "Chickadees and allies",
      "Nuthatches and Creepers",
      "Wrens and Dipper",
      "Kinglets and allies",
      "Others",
      "Thrushes",
      "Thrashers and allies",
      "Urban birds",
      "Waxwings",
      "Longspurs",
      "Wood-Warblers",
      "Sparrows and allies",
      "Cardinals and allies",
      "Blackbirds and allies",
      "Finches",
      "Crossbills",
      "Redpolls"
    )
  )

  lg2 <- list(
    bird_status = c("federal", "provincial"),
    bird_type = c("landbirds", "waterbirds", "waterfowl", "shorebirds"),
    habitat_type = c("forest", "open", "wetland"),
    nest_location = c(
      "ground",
      "water",
      "vegetation",
      "cavity",
      "man-made",
      "cliff",
      "colony"
    ),
    model_output = c("accepted", "rejected"),
    bird_group = c(
      "geese",
      "swans",
      "dabbling ducks",
      "diving ducks",
      "grouses and allies",
      "loons and grebes",
      "petrels and allies",
      "cormorants",
      "pelicans",
      "herons and allies",
      "hawks and allies",
      "rails and cranes",
      "shorebirds",
      "gulls",
      "terns",
      "jaegers",
      "auks and allies",
      "doves and allies",
      "cuckoos",
      "owls and allies",
      "nightjars",
      "swallows and allies",
      "hummingbirds",
      "kingfishers",
      "woodpeckers",
      "flychatchers and allies",
      "shrikes",
      "vireos",
      "corvids",
      "pipits and allies",
      "chickadees and allies",
      "nuthatches and creepers",
      "wrens and dipper",
      "kinglets and allies",
      "others",
      "thrushes",
      "thrashers and allies",
      "urban birds",
      "waxwings",
      "longspurs",
      "warblers",
      "sparrows and allies",
      "cardinals and allies",
      "blackbirds and allies",
      "finches",
      "crossbills",
      "redpolls"
    )
  )

  d <- nestwatch
  #sg<-read.csv("Species_grouping2.csv",header=T,stringsAsFactors=F,na.strings="")
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
  for (i in 1:length(lg)) {
    if (names(lg)[i] == "bird_status") {
      col <- "MBCA_2011_Final"
    }
    if (names(lg)[i] == "bird_type") {
      col <- "Species_gr1"
    }
    if (names(lg)[i] == "habitat_type") {
      col <- c("Habitat1", "Habitat2", "Habitat3")
    }
    if (names(lg)[i] == "nest_location") {
      col <- c("Nest_Type1", "Nest_Type2", "Nest_Type3")
    }
    if (names(lg)[i] == "model_output") {
      col <- c("model")
    }
    if (names(lg)[i] == "bird_group") {
      col <- c("Species_gr2")
    }
    for (j in 1:length(lg[[i]])) {
      ans <- apply(sg[, col, drop = FALSE], 2, function(k) {
        ans <- k == lg[[i]][j]
        ans <- ifelse(is.na(ans), FALSE, ans)
        ans
      })
      ans <- apply(ans, 1, any)
      sg2 <- cbind(sg2, as.integer(ans), stringsAsFactors = FALSE)
      names(sg2)[ncol(sg2)] <- paste(names(lg2)[i], lg2[[i]][j], sep = "_")
    }
  }
  sg <- sg2
  rm(lg)

  tclServiceMode(FALSE)
  tt <- tktoplevel()
  tkwm.geometry(tt, "950x600")
  tcl("wm", "attributes", tt, topmost = TRUE)
  #tkwm.resizable(tt, TRUE, TRUE)
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
  #fontHeading <- tkfont.create(family="times",size=24,weight="bold",slant="italic")
  #tkwm.title(tt,"rNest",font=fontHeading)
  tktitle(tt) <- get("rNestname", envir = rnest)

  header1 <- tklabel(tt, text = get("header1labl", envir = rnest))
  tkconfigure(header1, foreground = "blue")
  header2 <- tklabel(tt, text = get("header2labl", envir = rnest))
  tkconfigure(header2, foreground = "blue")
  header3 <- tklabel(tt, text = get("header3labl", envir = rnest))
  tkconfigure(header3, foreground = "blue")

  intchoice <- tkcheckbutton(tt)
  intchoiceval <- tclVar("0")
  tkconfigure(intchoice, variable = intchoiceval, text = "")
  intchoicelab <- tklabel(tt, text = get("intchoicelabl", envir = rnest))
  intchoiceq <- tkbutton(
    tt,
    pady = 0,
    bg = "grey85",
    text = "?",
    command = function() {
      tkmessageBox(
        title = "",
        message = get("intchoiceqlabl", envir = rnest),
        icon = "info",
        type = "ok"
      )
    }
  )

  zoom <- tkcheckbutton(tt)
  zoomval <- tclVar("0")
  tkconfigure(zoom, variable = zoomval, text = "")
  zoomlab <- tklabel(tt, text = get("zoomlabl", envir = rnest))
  zoomq <- tkbutton(
    tt,
    pady = 0,
    bg = "grey85",
    text = "?",
    command = function() {
      tkmessageBox(
        title = "",
        message = get("zoomqlabl", envir = rnest),
        icon = "info",
        type = "ok"
      )
    }
  )

  backsp <- tkcheckbutton(tt)
  backspval <- tclVar("0")
  tkconfigure(backsp, variable = backspval, text = "")
  backsplab <- tklabel(tt, text = get("backsplabl", envir = rnest))
  backspq <- tkbutton(
    tt,
    pady = 0,
    bg = "grey85",
    text = "?",
    command = function() {
      tkmessageBox(
        title = "",
        message = get("backspqlabl", envir = rnest),
        icon = "info",
        type = "ok"
      )
    }
  )

  getopt <- function() {
    species1 <- tclvalue(tkget(species))
    species1 <- if (species1 == "") {
      FALSE
    } else {
      unlist(strsplit(species1, ","))
    }
    years1 <- tclvalue(tkget(year))
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
    shp1 <- tclvalue(tkget(shp))
    shp1 <- if (shp1 == "") {
      FALSE
    } else {
      shp1
    } #attention, les fonctions de bases veulent le shp dans le r\u00e9pertoire courant
    if (!identical(shp1, get("usershppath", envir = rnest))) {
      assign("usershppath", shp1, envir = rnest)
      if (!is.logical(shp1)) {
        tkconfigure(tt, cursor = "watch")
        on.exit(tkconfigure(tt, cursor = "arrow"))
        arg <- breakshapepath(shp1)
        usershp <- readOGR(dsn = arg[1], layer = arg[2])
        assign("usershp", usershp, envir = rnest)
      } else {
        assign("usershp", NULL, envir = rnest)
        assign("usershp", ecodistricts, envir = rnest)
      }
    }
    id1 <- tclvalue(tkget(id))
    id1 <- if (id1 == "") {
      FALSE
    } else {
      unlist(strsplit(id1, ","))
    } #mettre le ID \u00e0 FALSE ici
    region1 <- tclvalue(tkget(region))
    region1 <- if (region1 == "") {
      FALSE
    } else {
      unlist(strsplit(region1, ","))
    }
    intchoice1 <- as.logical(as.numeric(tclvalue(intchoiceval)))
    zoom1 <- as.logical(as.numeric(tclvalue(zoomval)))
    backsp1 <- as.logical(as.numeric(tclvalue(backspval)))
    useprecalc1 <- as.logical(as.numeric(tclvalue(useprecalcval)))
    if (tclvalue(spgr) == "usp") {
      spgroup1 <- NULL
    } else {
      spgroup1 <- getgroups()
      spgroup1 <- spgroup1[
        !sapply(spgroup1, function(i) {
          identical(NA, i)
        })
      ] ### add 2014-01-18
      species1 <- FALSE
    }
    uspgroup1 <- tclvalue(uspgroup)
    ecodistrict1 <- tclvalue(tkget(ecodistrict))
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

  mapnest <- function() {
    tkconfigure(tt, cursor = "watch")
    on.exit(tkconfigure(tt, cursor = "arrow"))
    l <- getopt()
    #y<-x ### sub 2014-01-17
    temp <- TRUE
    if (!is.logical(l$sp)) {
      x <- d[d[, "species_code"] %in% l$sp, ]
      temp <- FALSE
    }
    if (!is.null(l$spgroup)) {
      x <- d[d[, "species_code"] %in% unlist(l$spgroup), ]
      temp <- FALSE
    }
    if (temp) {
      tkmessageBox(
        title = "",
        message = get("tkmb1", envir = rnest),
        icon = "info",
        type = "ok"
      )
      return()
    }
    if (nrow(x) == 0) {
      tkmessageBox(
        title = "",
        message = get("tkmb2", envir = rnest),
        icon = "info",
        type = "ok"
      )
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
    temp <- !(is.na(x$lon) | is.na(x$lat)) ### add 2014-01-17
    keep <- ifelse(!temp & keep, FALSE, keep) ### add 2014-01-17
    #if(!identical(l$layer,FALSE) || l$int.choice){temp<-!(is.na(x$lon) | is.na(x$lat));keep<-ifelse(!temp & keep,FALSE,keep)}  ### sub 2014-01-17
    x <- x[keep, ]
    if (nrow(x) == 0) {
      tkmessageBox(
        title = "",
        message = get("tkmb3", envir = rnest),
        icon = "info",
        type = "ok"
      )
      return()
    }
    if (!is.logical(l$layer)) {
      pol <- get("usershp", envir = rnest)
      #pol<-readShapeSpatial(l$layer)
    } else {
      pol <- ecodistricts
    }
    xx <- x
    coordinates(xx) <- cbind(xx$lon, xx$lat, stringsAsFactors = FALSE)
    slot(pol, "data") <- data.frame(
      reg = apply(
        slot(pol, "data")[, l$id, drop = F],
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
        tkmessageBox(
          title = "",
          message = get("tkmb4", envir = rnest),
          icon = "info",
          type = "ok"
        )
        return()
      }
    }
    points(x$lon, x$lat, col = "red", pch = 16)
  }

  mapeco <- function() {
    tkconfigure(tt, cursor = "watch")
    on.exit(tkconfigure(tt, cursor = "arrow"))
    l <- getopt()
    w <- which(ecodistricts@data[, "ecodistrict"] %in% l$ecodistrict)
    if (any(w)) {
      x11()
      par(mar = c(2, 0, 0, 0))
      plot(ecodistricts[w, ], col = "white", border = "white")
    } else {
      tkmessageBox(
        title = "",
        message = get("tkmb5", envir = rnest),
        icon = "info",
        type = "ok"
      )
      return()
    }
    plot(ecodistricts, add = TRUE)
    if (!is.logical(l$layer)) {
      fshp <- get("usershp", envir = rnest)
      #fshp<-readShapeSpatial(x)
      plot(fshp, add = TRUE)
    }
    plot(
      ecodistricts[w, ],
      col = ifelse(
        !(ecodistricts@data[w, "ecodistrict"] %in% predictions$ecodistrict),
        gray(0.9),
        "lightgoldenrod"
      ),
      add = TRUERUE
    )
    coo <- coordinates(ecodistricts[w, ])
    text(
      coo[, 1],
      coo[, 2],
      ecodistricts@data[w, "ecodistrict"],
      cex = 0.7,
      font = 2
    )
  }

  mapsp <- function() {
    tkconfigure(tt, cursor = "watch")
    on.exit(tkconfigure(tt, cursor = "arrow"))
    l <- getopt()
    if (is.logical(l$sp)) {
      tkmessageBox(
        title = "",
        message = get("tkmb6", envir = rnest),
        icon = "info",
        type = "ok"
      )
      return()
    }
    w <- ecodistricts@data[, "ecodistrict"] %in%
      specodistricts$ecodis[specodistricts[, which(
        names(specodistricts) == l$sp[1]
      )]]
    if (any(w)) {
      x11()
      par(mar = c(2, 0, 0, 0))
      plot(ecodistricts)
      mtext(l$sp[1], side = 1, outer = FALSE, font = 2, cex = 1.5)
      plot(
        ecodistricts[w, ],
        col = "lightgoldenrod",
        border = "black",
        add = TRUE
      )
      if (!is.logical(l$layer)) {
        fshp <- get("usershp", envir = rnest)
        #fshp<-readShapeSpatial(x)
        plot(fshp, add = TRUE)
      }
    } else {
      tkmessageBox(
        title = "",
        message = get("tkmb7", envir = rnest),
        icon = "info",
        type = "ok"
      )
      return()
    }
  }

  mapspbackground <- function(warnsp = TRUE) {
    tkconfigure(tt, cursor = "watch")
    on.exit(tkconfigure(tt, cursor = "arrow"))
    l <- getopt()
    if (is.logical(l$sp)) {
      return()
    }
    if (length(l$sp) > 1 && warnsp) {
      tkmessageBox(
        title = "",
        message = get("tkmb8", envir = rnest),
        type = "ok"
      )
    }
    w <- ecodistricts@data[, "ecodistrict"] %in%
      specodistricts$ecodis[specodistricts[, which(
        names(specodistricts) == l$sp[1]
      )]]
    if (any(w)) {
      plot(ecodistricts[w, ], col = "grey85", add = TRUE)
      return()
    }
  }

  run <- function() {
    tkconfigure(tt, cursor = "watch")
    on.exit(tkconfigure(tt, cursor = "arrow"))
    l <- getopt()
    assign("l", l, envir = rnest)
    nameback1 <- tclvalue(tkget(nameback))
    if (
      (is.logical(l$sp) && tclvalue(spgr) == "usp") ||
        (is.null(l$spgroup) && tclvalue(spgr) == "ugr")
    ) {
      tkmessageBox(
        title = "",
        message = get("tkmb9", envir = rnest),
        icon = "info",
        type = "ok"
      )
      return()
    }
    ans <- active.nest(
      d,
      sp = l$sp,
      year = l$year,
      layer = if (is.logical(l$layer)) {
        ecodistricts
      } else {
        get("usershp", envir = rnest)
      },
      region = l$region,
      id = l$id,
      int.choice = l$int.choice,
      zoom = l$zoom,
      locations = FALSE,
      spgroup = l$spgroup,
      useprecalc = l$useprecalc,
      progress = TRUE,
      lang = lang,
      fromtk = TRUE
    )
    if (nameback1 != "") {
      assign(nameback1, ans, envir = .GlobalEnv)
    }
    assign("ans", ans, envir = rnest)
  }

  calendarobs <- function() {
    tkconfigure(tt, cursor = "watch")
    on.exit(tkconfigure(tt, cursor = "arrow"))
    l <- getopt()
    if (
      (is.logical(l$sp) && tclvalue(spgr) == "usp") ||
        (is.null(l$spgroup) && tclvalue(spgr) == "ugr")
    ) {
      tkmessageBox(
        title = "",
        message = get("tkmb10", envir = rnest),
        icon = "info",
        type = "ok"
      )
      return()
    }
    if (!exists("l", envir = rnest) || !identical(get("l", envir = rnest), l)) {
      answer <- tkmessageBox(
        title = "",
        default = "no",
        message = get("tkmb11", envir = rnest),
        icon = "info",
        type = "yesno"
      )
      if (tclvalue(answer) == "yes") {
        run()
      }
      if (tclvalue(answer) == "no") {
        return()
      }
    }
    on.exit(tkconfigure(tt, cursor = "arrow"))
    namerawcal1 <- tclvalue(tkget(namerawcal))
    res <- nesting.activity(
      get("ans", envir = rnest),
      n = TRUE,
      dates = FALSE,
      by = tclvalue(cal),
      chronology = FALSE
    )
    if (namerawcal1 != "") {
      assign(namerawcal1, res, envir = .GlobalEnv)
    }
    prop <- tclvalue(tkget(prop))
    color <- tclvalue(tkget(colo))
    if (prop == "" || color == "") {
      if (prop != "" || color != "") {
        tkmessageBox(
          title = "",
          message = get("tkmb12", envir = rnest),
          icon = "info",
          type = "ok"
        )
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
    colcons <- as.character(tclvalue(tkget(showpropcolo)))
    colcons <- ifelse(colcons == "", "deepskyblue", colcons)
    res <- res[, c("date", rev(names(res)[-1]))]
    nesting.calendar(
      res,
      color.scale = TRUE,
      dates = c(tclvalue(tkget(datebeg)), tclvalue(tkget(dateend))),
      type = tclvalue(cal),
      color = color,
      prop = prop,
      cons = as.numeric(tclvalue(tkget(showpropnest))),
      colcons = colcons,
      list_lang = list_lang
    )
  }

  calendarpred <- function() {
    tkconfigure(tt, cursor = "watch")
    on.exit(tkconfigure(tt, cursor = "arrow"))
    l <- getopt()
    leco <- l$ecodistrict
    if (identical(leco, "")) {
      tkmessageBox(
        title = "",
        message = get("tkmb13", envir = rnest),
        icon = "info",
        type = "ok"
      )
      return()
    }
    if (l$uspgroup == "uallsp") {
      lgro <- list(allsp = unique(sg[, "Species_ID"]))
    }
    if (l$uspgroup == "uselectsp") {
      if (is.logical(l$sp)) {
        tkmessageBox(
          title = "",
          message = get("tkmb14", envir = rnest),
          icon = "info",
          type = "ok"
        )
        return()
      }
      if (!any(l$sp %in% predictions$species_code)) {
        tkmessageBox(
          title = "",
          message = get("tkmb15", envir = rnest),
          icon = "info",
          type = "ok"
        )
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
      lgro <- getgroups()
      if (is.null(lgro)) {
        tkmessageBox(
          title = "",
          message = get("tkmb16", envir = rnest),
          icon = "info",
          type = "ok"
        )
        return()
      }
    }
    if (!any(predictions[, "ecodistrict"] %in% leco)) {
      tkmessageBox(
        title = "",
        message = get("tkmb17", envir = rnest),
        icon = "info",
        type = "ok"
      )
      return()
    }
    cal <- data.frame(
      date = c(
        "nb_nest",
        substr(
          seq.Date(as.Date("2001-01-01"), as.Date("2001-12-31"), by = 1)[
            1:365
          ],
          6,
          10
        )
      ),
      stringsAsFactors = FALSE
    )
    for (i in seq_along(leco)) {
      xeco <- predictions[which(predictions[, "ecodistrict"] == leco[i]), ]
      for (j in seq_along(lgro)) {
        x <- xeco[xeco$species_code %in% lgro[[j]], ]
        if (nrow(x) == 0) {
          next
        }
        a1 <- lapply(
          dlply(x[x[, "type"] == "initiation", ], .(ecodistrict)),
          function(k) {
            c(nrow(k), calendarprop(k, column = "jul"))
          }
        )
        a2 <- lapply(
          dlply(x[x[, "type"] == "fledging", ], .(ecodistrict)),
          function(k) {
            c(nrow(k), calendarprop(k, column = "jul"))
          }
        )
        a <- as.data.frame(matrix(
          do.call("c", a1) - do.call("c", a2),
          nrow = 366
        ))
        a[1, ] <- sapply(a1, function(k) {
          k[1]
        })
        a <- as.list(a)
        names(a) <- names(a1)
        cal <- cbind(cal, temp = a, stringsAsFactors = FALSE)
        names(cal)[ncol(cal)] <- paste(leco[i], names(lgro)[j], sep = "___")
      }
    }
    if (ncol(cal) == 1) {
      tkmessageBox(
        title = "",
        message = get("tkmb18", envir = rnest),
        icon = "info",
        type = "ok"
      )
      return()
    }
    if ((ncol(cal) - 1) > 50) {
      tkmessageBox(
        title = "",
        message = paste0(get("tkmb19", envir = rnest), ncol(cal) - 1),
        icon = "info",
        type = "ok"
      )
      return()
    }
    #names(cal)<-gsub("\\.","___",names(cal))
    prop <- tclvalue(tkget(prop))
    color <- tclvalue(tkget(colo))
    if (prop == "" || color == "") {
      if (prop != "" || color != "") {
        tkmessageBox(
          title = "",
          message = get("tkmb20", envir = rnest),
          icon = "info",
          type = "ok"
        )
      }
      prop <- c(0.000000000000, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1)
      color <- c("white", brewer.pal(length(prop) - 3, "YlOrRd"), "darkred")
    } else {
      prop <- as.numeric(unlist(strsplit(prop, ","))) / 100
      prop <- c(0.000000000000, prop[-1])
      color <- unlist(strsplit(color, ","))
    }
    if (tclvalue(ordercal) == "eco") {
      cal <- cal[, c(1, rev(2:ncol(cal)))]
    }
    if (tclvalue(ordercal) == "spgr") {
      lo <- strsplit(names(cal)[-1], "___")
      lo1 <- sapply(lo, function(i) {
        i[1]
      })
      lo2 <- sapply(lo, function(i) {
        i[2]
      })
      namescal2 <- names(cal)[-1]
      cal <- cal[, c(
        "date",
        rev(namescal2[order(match(lo2, names(lgro)), match(lo1, leco))])
      )]
    }
    names(cal) <- gsub("___", "  ", names(cal))
    nesting.calendar.pred(
      cal,
      lebox = 35,
      name = "Ecodistrict",
      prop = prop,
      color = color,
      dates = c(tclvalue(tkget(datebeg)), tclvalue(tkget(dateend))),
      color.scale = ifelse(l$uspgroup == "uselectsp", FALSE, TRUE),
      list_lang = list_lang
    )
  }

  showshp <- function(info = "id") {
    l <- getopt()
    if (is.logical(l$layer)) {
      stop(get("tkmb21", envir = rnest))
    }
    fshp <- get("usershp", envir = rnest)
    #fshp<-readShapeSpatial(x)
    if (info == "id") {
      cat(
        "\n",
        "\n",
        paste(get("tkmb22", envir = rnest), x),
        "\n",
        "\n",
        "\n"
      )
      print(head(fshp@data))
      return(names(fshp@data))
    }
    if (info == "region") {
      l <- getopt()
      if (is.logical(l$id)) {
        stop(get("tkmb23", envir = rnest))
      }
      temp <- fshp@data[, l$id, drop = FALSE]
      temp <- apply(temp, 1, paste, collapse = "_")
      return(list(region = temp, shape = fshp))
    }
  }

  chooseshp <- function() {
    tkconfigure(tt, cursor = "watch")
    on.exit(tkconfigure(tt, cursor = "arrow"))
    tcl("wm", "attributes", tt, topmost = FALSE)
    x <- gsub("\\\\", "/", file.choose())
    tkconfigure(shp, textvariable = tclVar(x))
    tkgrid(shp, row = 10, column = 1, sticky = "w") #adjust with final location row=5
    l <- getopt() # to load the shapefile and bring modifications to rnest envir
    #arg<-breakshapepath(x)
    #usershp<-readOGR(dsn=arg[1],layer=arg[2])
    #assign("usershp",usershp,envir=rnest)
    tcl("wm", "attributes", tt, topmost = TRUE)
  }

  choosesp <- function() {
    listsp <- as.character(tkget(listid, 0, "end"))
    x <- as.numeric(tkcurselection(listid)) + 1
    x <- paste(listsp[x], collapse = ",")
    tkconfigure(species, textvariable = tclVar(x))
    tkgrid(species, row = 3, column = 1, sticky = "w")
  }

  chooseid <- function() {
    ll <- showshp(info = "id")
    ttt <- tktoplevel()
    tcl("wm", "attributes", ttt, topmost = TRUE)
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
      tkconfigure(id, textvariable = tclVar(x))
      tkgrid(id, row = 11, column = 1, sticky = "w")
      tkdestroy(ttt)
      tcl("wm", "attributes", tt, topmost = TRUE)
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

  chooseregion <- function() {
    ll <- showshp(info = "region")
    ttt <- tktoplevel()
    tcl("wm", "attributes", ttt, topmost = TRUE)
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
      tkconfigure(region, textvariable = tclVar(x))
      tkgrid(region, row = 12, column = 1, sticky = "w")
      tkdestroy(ttt)
      tcl("wm", "attributes", tt, topmost = TRUE)
    }
    mapchoice <- function() {
      tkconfigure(tt, cursor = "watch")
      on.exit(tkconfigure(tt, cursor = "arrow"))
      x11()
      par(mar = c(2, 0, 0, 0))
      plot(ll$shape)
      l <- getopt()
      if (l$zoom) {
        mtext(
          get("tkmb24", envir = rnest),
          side = 1,
          outer = FALSE,
          font = 2,
          cex = 1.1
        )
        pol <- getpoly(quiet = T)
        #pol<-SpatialPolygons(list(Polygons(list(Polygon(pol[c(1:nrow(pol),1),])),ID=1)),proj4string=CRS(proj4string(ecodistricts)))
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
        get("tkmb25", envir = rnest),
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
        plot(ll$shape[w, ], add = T, col = couleur)
      }
      polid <- sort(unique(polid))
      x <- paste(unique(ll$region[polid]), collapse = ",")
      tkconfigure(region, textvariable = tclVar(x))
      tkgrid(region, row = 12, column = 1, sticky = "w")
      tkdestroy(ttt)
      tcl("wm", "attributes", tt, topmost = TRUE)
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
      text = get("tkmb26", envir = rnest),
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

  chooseecodistrict <- function() {
    tkconfigure(tt, cursor = "watch")
    on.exit(tkconfigure(tt, cursor = "arrow"))
    l <- getopt()
    x11()
    par(mar = c(2, 0, 0, 0))
    plot(ecodistricts)
    warnsp <- TRUE
    if (l$backsp) {
      mapspbackground(warnsp = TRUE)
      warnsp <- FALSE
    }
    if (!is.logical(l$layer)) {
      fshp <- get("usershp", envir = rnest)
      #fshp<-readShapeSpatial(x)
      plot(fshp, add = TRUE)
    }
    if (l$zoom) {
      mtext(
        get("tkmb24", envir = rnest),
        side = 1,
        outer = FALSE,
        font = 2,
        cex = 1.1
      )
      pol <- getpoly(quiet = T)
      pol <- SpatialPolygons(
        list(Polygons(list(Polygon(pol[c(1:nrow(pol), 1), ])), ID = 1)),
        proj4string = CRS(proj4string(ecodistricts))
      )
      plot(
        ecodistricts,
        xlim = range(bbox(pol)[1, ]),
        ylim = range(bbox(pol)[2, ])
      )
    }
    if (l$backsp) {
      mapspbackground(warnsp = warnsp)
    }
    if (exists("fshp")) {
      plot(fshp, add = TRUE)
    }
    if (l$int.choice) {
      mtext(
        get("tkmb27", envir = rnest),
        side = 1,
        outer = FALSE,
        font = 2,
        cex = 1.1
      )
      pol <- getpoly(quiet = T)
      polygon(pol, lwd = 4, border = "darkgreen")
      #int<-inout(cbind(x=d$lon,y=d$lat),pol)
      pol <- SpatialPolygons(
        list(Polygons(list(Polygon(pol[c(1:nrow(pol), 1), ])), ID = 1)),
        proj4string = CRS(proj4string(ecodistricts))
      )
      o <- over(pol, ecodistricts, returnList = TRUE)
      plot(ecodistricts[o[[1]], ], add = TRUE, col = "lightgoldenrod")
      if (exists("fshp")) {
        plot(fshp, add = TRUE)
      }
      plot(pol, lwd = 4, border = "darkgreen", add = TRUERUE)
      x <- paste(
        sort(unique(ecodistricts$ecodistrict[o[[1]]])),
        collapse = ","
      )
      tkconfigure(ecodistrict, textvariable = tclVar(x))
      tkgrid(ecodistrict, row = 11, column = 1, sticky = "w")
    } else {
      polid <- NULL
      repeat {
        mtext(
          get("tkmb28", envir = rnest),
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
        proj4string(locx) <- CRS(proj4string(ecodistricts))
        temp <- over(locx, ecodistricts, returnList = FALSE)[,
          "ecodistrict",
          drop = TRUE
        ]
        if (is.na(temp)) {
          next
        }
        w <- which(ecodistricts$ecodistrict == temp)
        if (all(w %in% polid)) {
          polid <- polid[!polid %in% w]
          couleur <- "white"
        } else {
          polid <- c(polid, w)
          couleur <- "lightgoldenrod"
        }
        plot(ecodistricts[w, ], add = TRUE, col = couleur)
      }
      if (exists("fshp")) {
        plot(fshp, add = TRUE)
      }
      polid <- sort(unique(polid))
      x <- paste(unique(ecodistricts$ecodistrict[polid]), collapse = ",")
      tkconfigure(ecodistrict, textvariable = tclVar(x))
      tkgrid(ecodistrict, row = 11, column = 1, sticky = "w")
    }
  }

  getgroups <- function(nn = n) {
    members <- lapply(1:nn, function(i) {
      x <- as.character(tkget(get(paste("groupm", i, sep = "")), 0, "end"))
      if (length(x) == 0) {
        NA
      } else {
        x
      }
    })
    group <- sapply(1:nn, function(i) {
      x <- as.character(tkget(get(paste("group", i, sep = ""))))
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

  shp <- tkentry(tt, width = 26, textvariable = tclVar(""))
  shpb <- tkbutton(
    tt,
    text = get("shpblabl", envir = rnest),
    command = chooseshp,
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
      tkmessageBox(
        title = "",
        message = paste0(
          get("tkmb29", envir = rnest)[1],
          getwd(),
          get("tkmb29", envir = rnest)[2],
          tclvalue(tkget(shp)),
          get("tkmb29", envir = rnest)[3],
          ifelse(
            !is.null(get("usershp", envir = rnest)),
            proj4string(get("usershp", envir = rnest)),
            ""
          )
        ),
        icon = "info",
        type = "ok"
      )
    }
  )

  #idlab<-tklabel(tt,text="id attribute")
  id <- tkentry(tt, width = 26, textvariable = tclVar(""))
  idb <- tkbutton(
    tt,
    text = get("idblabl", envir = rnest),
    command = chooseid,
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
      tkmessageBox(
        title = "",
        message = paste0(get("tkmb30", envir = rnest), tclvalue(tkget(id))),
        icon = "info",
        type = "ok"
      )
    }
  )

  #regionlab<-tklabel(tt,text="region")
  region <- tkentry(tt, width = 26, textvariable = tclVar(""))
  regionb <- tkbutton(
    tt,
    text = get("regionblabl", envir = rnest),
    command = chooseregion,
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
      tkmessageBox(
        title = "",
        message = paste0(
          get("tkmb31", envir = rnest),
          tclvalue(tkget(region))
        ),
        icon = "info",
        type = "ok"
      )
    }
  )

  #specieslab<-tklabel(tt,text="species")
  species <- tkentry(tt, width = 26, textvariable = tclVar(""))
  speciesb <- tkbutton(
    tt,
    text = get("speciesblabl", envir = rnest),
    command = choosesp,
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
      tkmessageBox(
        title = "",
        message = paste0(
          get("tkmb32", envir = rnest),
          tclvalue(tkget(species))
        ),
        icon = "info",
        type = "ok"
      )
    }
  )

  yearlab <- tkbutton(
    tt,
    text = get("yearlabl", envir = rnest),
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
      tkmessageBox(
        title = "",
        message = paste0(get("tkmb33", envir = rnest), tclvalue(tkget(year))),
        icon = "info",
        type = "ok"
      )
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
    text = get("spcallabl", envir = rnest)
  )
  tkconfigure(
    yearcal,
    variable = cal,
    value = "year",
    text = get("yearcallabl", envir = rnest)
  )
  tkconfigure(
    regioncal,
    variable = cal,
    value = "region",
    text = get("regioncallabl", envir = rnest)
  )

  ordercal <- tclVar("spgr")
  orderspgrcal <- tkradiobutton(tt)
  orderecocal <- tkradiobutton(tt)
  tkconfigure(
    orderspgrcal,
    variable = ordercal,
    value = "spgr",
    text = get("orderspgrcallabl", envir = rnest)
  )
  tkconfigure(
    orderecocal,
    variable = ordercal,
    value = "eco",
    text = get("orderecocallabl", envir = rnest)
  )

  nameback <- tkentry(tt, width = 12, textvariable = tclVar("estimates"))
  namebackq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tkmessageBox(
        title = "",
        message = get("namebackqtext", envir = rnest),
        icon = "info",
        type = "ok"
      )
    }
  )
  namerawcal <- tkentry(tt, width = 12, textvariable = tclVar("calendar"))
  namerawcalq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tkmessageBox(
        title = "",
        message = get("namerawcalqtext", envir = rnest),
        icon = "info",
        type = "ok"
      )
    }
  )

  #disres<-tklabel(tt,text="display results ?")
  #discal<-tklabel(tt,text="display raw calendar ?")
  #objectres<-tklabel(tt,text="object name")
  #objectcal<-tklabel(tt,text="object name")

  butcal <- tkbutton(
    tt,
    text = get("butcallabl", envir = rnest),
    bg = "grey55",
    width = 27,
    command = function() {
      if (get("predict", envir = rnest)) {
        calendarpred()
      } else {
        calendarobs()
      }
    }
  )
  butcalq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tkmessageBox(
        title = "",
        message = get("butcalqtext", envir = rnest),
        icon = "info",
        type = "ok"
      )
    }
  )

  datelab <- tklabel(tt, text = "dates")
  datebeg <- tkentry(tt, width = 7, textvariable = tclVar("02-01"))
  dateend <- tkentry(tt, width = 7, textvariable = tclVar("09-30"))

  propfun <- function() {
    tkmessageBox(
      title = "",
      message = paste0(get("tkmb34", envir = rnest), tclvalue(tkget(prop))),
      icon = "info",
      type = "ok"
    )
  }
  showcol <- function() {
    tkmessageBox(
      title = "",
      message = paste0(get("tkmb35", envir = rnest), tclvalue(tkget(colo))),
      icon = "info",
      type = "ok"
    )
  }
  showpropcol <- function() {
    tkmessageBox(
      title = "",
      message = paste0(get("tkmb36", envir = rnest)),
      icon = "info",
      type = "ok"
    )
  }

  propbut <- tkbutton(
    tt,
    text = get("propbutlabl", envir = rnest),
    width = 11,
    command = propfun,
    pady = 0,
    bg = "grey85"
  )
  prop <- tkentry(tt, width = 25, textvariable = tclVar(""))
  propq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = propfun
  )

  colobut <- tkbutton(
    tt,
    text = get("colobutlabl", envir = rnest),
    width = 11,
    command = showcol,
    pady = 0,
    bg = "grey85"
  )
  colo <- tkentry(tt, width = 25, textvariable = tclVar(""))
  coloq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = showcol
  )

  showpropbut <- tkbutton(
    tt,
    text = get("showpropbutlabl", envir = rnest),
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
    command = showpropcol
  )

  # obs mode
  usp <- tkradiobutton(tt)
  ugr <- tkradiobutton(tt)
  spgr <- tclVar("usp")
  tkconfigure(
    usp,
    variable = spgr,
    value = "usp",
    text = get("usplabl", envir = rnest)
  )
  tkconfigure(
    ugr,
    variable = spgr,
    value = "ugr",
    text = get("ugrlabl", envir = rnest)
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
    text = get("uallsplabl", envir = rnest)
  )
  tkconfigure(
    uselectsp,
    variable = uspgroup,
    value = "uselectsp",
    text = get("uselectsplabl", envir = rnest)
  )
  tkconfigure(
    ugroups,
    variable = uspgroup,
    value = "ugroups",
    text = get("ugroupslabl", envir = rnest)
  )

  #makegroup<-function(tt=tt,nn=n){

  #ll<-unique(d$species_code)

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
    tklabel(tt, text = get("tkmb37", envir = rnest)),
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
      tkmessageBox(
        title = "",
        message = get("groupqtext", envir = rnest),
        icon = "info",
        type = "ok"
      )
    }
  )

  lgroups <- lgroups <- list(
    bird_status = c("federal", "provincial"),
    bird_type = c("landbirds", "waterbirds", "waterfowl", "shorebirds"),
    habitat_type = c("forest", "open", "wetland"),
    nest_location = c(
      "ground",
      "water",
      "vegetation",
      "cavity",
      "man-made",
      "cliff",
      "colony"
    ),
    model_output = c("accepted", "rejected"),
    bird_group = c(
      "geese",
      "swans",
      "dabbling ducks",
      "diving ducks",
      "grouses and allies",
      "loons and grebes",
      "petrels and allies",
      "cormorants",
      "pelicans",
      "herons and allies",
      "hawks and allies",
      "rails and cranes",
      "shorebirds",
      "gulls",
      "terns",
      "jaegers",
      "auks and allies",
      "doves and allies",
      "cuckoos",
      "owls and allies",
      "nightjars",
      "swallows and allies",
      "hummingbirds",
      "kingfishers",
      "woodpeckers",
      "flychatchers and allies",
      "shrikes",
      "vireos",
      "corvids",
      "pipits and allies",
      "chickadees and allies",
      "nuthatches and creepers",
      "wrens and dipper",
      "kinglets and allies",
      "others",
      "thrushes",
      "thrashers and allies",
      "urban birds",
      "waxwings",
      "longspurs",
      "warblers",
      "sparrows and allies",
      "cardinals and allies",
      "blackbirds and allies",
      "finches",
      "crossbills",
      "redpolls"
    )
  )
  lgroupsl <- get("lgroupsl", envir = rnest)

  butmask <- function(
    n = 1,
    r = 1,
    s = 1,
    remove = FALSE,
    predict = get("predict", envir = rnest)
  ) {
    tclServiceMode(FALSE)
    if (remove) {
      tkgrid.remove(get(paste(names(lgroups)[n], "mask"), envir = rnest))
      for (j in 1:(length(lgroups[[n]]))) {
        assign(
          lgroups[[n]][j],
          tkcheckbutton(tt, command = function(x) {
            relistsp(sg, lgroups)
          }),
          envir = rnest
        )
        assign(
          paste(names(lgroups)[n], lgroups[[n]][j], "val", sep = "_"),
          tclVar(ifelse(
            predict &&
              paste(names(lgroups)[n], lgroups[[n]][j], "val", sep = "_") ==
                "model_output_rejected_val",
            "0",
            "1"
          )),
          envir = rnest
        )
        tkconfigure(
          get(lgroups[[n]][j], envir = rnest),
          variable = get(
            paste(names(lgroups)[n], lgroups[[n]][j], "val", sep = "_"),
            envir = rnest
          ),
          text = lgroupsl[[n]][j]
        )
        tkgrid(
          get(lgroups[[n]][j], envir = rnest),
          row = r + j,
          column = 6,
          rowspan = 1,
          sticky = "w"
        )
      }
    } else {
      for (j in 1:(length(lgroups[[n]]))) {
        assign(
          lgroups[[n]][j],
          tkcheckbutton(tt, command = function(x) {
            relistsp(sg, lgroups)
          }),
          envir = rnest
        )
        assign(
          paste(names(lgroups)[n], lgroups[[n]][j], "val", sep = "_"),
          tclVar(ifelse(
            predict &&
              paste(names(lgroups)[n], lgroups[[n]][j], "val", sep = "_") ==
                "model_output_rejected_val",
            "0",
            "1"
          )),
          envir = rnest
        )
        tkconfigure(
          get(lgroups[[n]][j], envir = rnest),
          variable = get(
            paste(names(lgroups)[n], lgroups[[n]][j], "val", sep = "_"),
            envir = rnest
          ),
          text = lgroupsl[[n]][j]
        )
        tkgrid(
          get(lgroups[[n]][j], envir = rnest),
          row = r + j,
          column = 6,
          rowspan = 1,
          sticky = "w"
        )
      }
      assign(
        paste(names(lgroups)[n], "mask"),
        tkbutton(
          tt,
          text = gsub("_", " ", names(lgroupsl)[n]),
          width = 12,
          command = function() {
            tkgrid.remove(get(
              paste(names(lgroups)[n], "mask"),
              envir = rnest
            ))
            relistsp(sg, lgroups)
          }
        ),
        envir = rnest
      )
      tkgrid(
        get(paste(names(lgroups)[n], "mask"), envir = rnest),
        row = r,
        column = 6,
        rowspan = s + 1,
        sticky = "nws"
      )
    }
    tclServiceMode(TRUE)
  }

  rejected <- function(fromobsbut = FALSE) {
    if (fromobsbut) {
      assign(
        "model_output_rejected_val",
        tclVar(ifelse(get("predict", envir = rnest), "0", "0")),
        envir = rnest
      )
      tkconfigure(
        get("rejected", envir = rnest),
        variable = get("model_output_rejected_val", envir = rnest),
        text = lgroupsl[[5]][2]
      )
      tkgrid(
        get("rejected", envir = rnest),
        row = 31,
        column = 6,
        rowspan = 1,
        sticky = "w"
      )
    } else {
      if (!get("rejected_status", envir = rnest)) {
        assign(
          "model_output_rejected_val",
          tclVar(ifelse(!get("predict", envir = rnest), "1", "0")),
          envir = rnest
        )
        tkconfigure(
          get("rejected", envir = rnest),
          variable = get("model_output_rejected_val", envir = rnest),
          text = lgroupsl[[5]][2]
        )
        tkgrid(
          get("rejected", envir = rnest),
          row = 31,
          column = 6,
          rowspan = 1,
          sticky = "w"
        )
      }
    }
  }

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
            tclServiceMode(FALSE)
            butmask(i, r, s, predict = get("predict", envir = rnest))
            relistsp(sg, lgroups)
            tclServiceMode(TRUE)
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
              relistsp(sg, lgroups)
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

  butmaskgroups <- function(n = 6, val = "1", remove = FALSE) {
    tclServiceMode(FALSE)
    pos <- 20
    co <- 9
    if (remove) {
      tkgrid.remove(get(paste(names(lgroups)[n], "mask"), envir = rnest))
      for (j in 1:length(lgroups[[n]])) {
        assign(
          lgroups[[n]][j],
          tkcheckbutton(tt, command = function(x) {
            relistsp(sg, lgroups)
          }),
          envir = rnest
        )
        assign(
          paste(names(lgroups)[n], lgroups[[n]][j], "val", sep = "_"),
          tclVar(val),
          envir = rnest
        )
        tkconfigure(
          get(lgroups[[n]][j], envir = rnest),
          variable = get(
            paste(names(lgroups)[n], lgroups[[n]][j], "val", sep = "_"),
            envir = rnest
          ),
          text = lgroupsl[[n]][j]
        )
        tkgrid(
          get(lgroups[[n]][j], envir = rnest),
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
      for (j in 1:length(lgroups[[n]])) {
        assign(
          lgroups[[n]][j],
          tkcheckbutton(tt, command = function(x) {
            relistsp(sg, lgroups)
          }),
          envir = rnest
        )
        assign(
          paste(names(lgroups)[n], lgroups[[n]][j], "val", sep = "_"),
          tclVar(val),
          envir = rnest
        )
        tkconfigure(
          get(lgroups[[n]][j], envir = rnest),
          variable = get(
            paste(names(lgroups)[n], lgroups[[n]][j], "val", sep = "_"),
            envir = rnest
          ),
          text = lgroupsl[[n]][j]
        )
        tkgrid(
          get(lgroups[[n]][j], envir = rnest),
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
          paste(names(lgroups)[n], "mask"),
          tkbutton(
            tt,
            text = gsub("_", " ", names(lgroupsl)[n]),
            width = 12,
            command = function() {
              tkgrid.remove(get(
                paste(names(lgroups)[n], "mask"),
                envir = rnest
              ))
              relistsp(sg, lgroups)
            }
          ),
          envir = rnest
        )
        tkgrid(
          get(paste(names(lgroups)[n], "mask"), envir = rnest),
          row = 18,
          column = 9,
          rowspan = 17,
          columnspan = 12,
          sticky = "nwse"
        )
      }
    }
    tclServiceMode(TRUE)
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
            tclServiceMode(FALSE)
            butmaskgroups(i)
            relistsp(sg, lgroups)
            tclServiceMode(TRUE)
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
        text = get("uncheckall", envir = rnest),
        width = 12,
        bg = "grey85",
        command = local({
          i <- i
          function() {
            tclServiceMode(FALSE)
            butmaskgroups(i, val = "0")
            relistsp(sg, lgroups)
            tclServiceMode(TRUE)
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
          relistsp(sg, lgroups)
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

  getcheckbut <- function(lg = lgroups) {
    ans <- llply(names(lg), function(i) {
      res <- paste(i, lg[[i]], sep = "_")
      res <- sapply(res, function(j) {
        as.logical(as.numeric(tclvalue(get(
          paste(j, "val", sep = "_"),
          envir = rnest
        ))))
      })
      res
    })
    names(ans) <- names(lg)
    ans
  }

  relistsp <- function(sg, lg) {
    ans <- getcheckbut()
    if (!any(unlist(ans))) {
      tkdelete(listid, 0, "end")
      return()
    }
    tclServiceMode(FALSE)
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
    if (get("predict", envir = rnest)) {
      subsp <- subsp[subsp %in% predictions$species_code]
    } else {
      subsp <- subsp[subsp %in% d$species_code]
    }
    tkdelete(listid, 0, "end")
    if (length(subsp) > 0) {
      for (j in 1:length(subsp)) {
        tkinsert(listid, "end", subsp[j])
      }
      tkgrid(listid, row = 3, rowspan = 32, column = 4, sticky = "ns")
    }
    tclServiceMode(TRUE)
  }

  tkbind(listid, "<Button-3>", function() {
    relistsp(sg, lgroups)
  })
  tkbind(listid, "<Double-Button-1>", function() {
    tkselection.set(listid, 0, "end")
  })

  selection <- function(k = 1) {
    #x<-ll[as.numeric(tkcurselection(listid))+1]

    listsp <- as.character(tkget(listid, 0, "end"))
    x <- listsp[as.numeric(tkcurselection(listid)) + 1]

    xm <- as.character(tkget(get(paste("groupm", k, sep = "")), 0, "end"))
    if (length(xm) == 0) {
      xm <- NA
    }
    x <- setdiff(x, xm)
    if (length(x)) {
      for (j in 1:length(x)) {
        tkinsert(get(paste("groupm", k, sep = "")), "end", x[j])
      }
      tkselection.clear(listid, 0, "end")
    }
  }

  clearselection <- function(k = 1, all = FALSE) {
    x <- as.character(tkget(get(paste("groupm", k, sep = "")), 0, "end"))
    y <- as.integer(tkcurselection(get(paste("groupm", k, sep = "")))) + 1
    if (length(y) > 0) {
      x <- x[-y]
    }
    tkdelete(get(paste("groupm", k, sep = "")), 0, "end")
    if (length(x) == 0 || all) {
      return()
    }
    for (j in seq(x)) {
      tkinsert(get(paste("groupm", k, sep = "")), "end", x[j])
    }
  }

  group2species <- function(no) {
    g <- getgroups()
    x <- g[[no]]
    y <- as.integer(tkcurselection(get(paste("groupm", no, sep = "")))) + 1
    if (length(y) > 0) {
      x <- x[y]
    }
    x <- paste(x, collapse = ",")
    tkconfigure(species, textvariable = tclVar(x))
    tkgrid(species, row = 3, column = 1, sticky = "w")
  }

  group1 <- tkentry(
    tt,
    width = 12,
    textvariable = tclVar(get("group1labl", envir = rnest))
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
    text = get("groupb1labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() selection(1)
  )
  groupc1 <- tkbutton(
    tt,
    text = get("groupc1labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(1)
  )
  groupcall1 <- tkbutton(
    tt,
    text = get("groupcall1labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(1, all = TRUE)
  )
  groupsp1 <- tkbutton(
    tt,
    text = get("groupsp1labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() group2species(no = 1)
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
    textvariable = tclVar(get("group2labl", envir = rnest))
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
    text = get("groupb2labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() selection(2)
  )
  groupc2 <- tkbutton(
    tt,
    text = get("groupc2labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(2)
  )
  groupcall2 <- tkbutton(
    tt,
    text = get("groupcall2labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(2, all = TRUE)
  )
  groupsp2 <- tkbutton(
    tt,
    text = get("groupsp2labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() group2species(no = 2)
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
    textvariable = tclVar(get("group3labl", envir = rnest))
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
    text = get("groupb3labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() selection(3)
  )
  groupc3 <- tkbutton(
    tt,
    text = get("groupc3labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(3)
  )
  groupcall3 <- tkbutton(
    tt,
    text = get("groupcall3labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(3, all = TRUE)
  )
  groupsp3 <- tkbutton(
    tt,
    text = get("groupsp3labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() group2species(no = 3)
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
    textvariable = tclVar(get("group4labl", envir = rnest))
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
    text = get("groupb4labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() selection(4)
  )
  groupc4 <- tkbutton(
    tt,
    text = get("groupc4labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(4)
  )
  groupcall4 <- tkbutton(
    tt,
    text = get("groupcall4labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(4, all = TRUE)
  )
  groupsp4 <- tkbutton(
    tt,
    text = get("groupsp4labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() group2species(no = 4)
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
    textvariable = tclVar(get("group5labl", envir = rnest))
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
    text = get("groupb5labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() selection(5)
  )
  groupc5 <- tkbutton(
    tt,
    text = get("groupc5labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(5)
  )
  groupcall5 <- tkbutton(
    tt,
    text = get("groupcall5labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(5, all = TRUE)
  )
  groupsp5 <- tkbutton(
    tt,
    text = get("groupsp5labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() group2species(no = 5)
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
    textvariable = tclVar(get("group6labl", envir = rnest))
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
    text = get("groupb6labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() selection(6)
  )
  groupc6 <- tkbutton(
    tt,
    text = get("groupc6labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(6)
  )
  groupcall6 <- tkbutton(
    tt,
    text = get("groupcall6labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() clearselection(6, all = TRUE)
  )
  groupsp6 <- tkbutton(
    tt,
    text = get("groupsp6labl", envir = rnest),
    width = 12,
    pady = 0,
    bg = "grey85",
    command = function() group2species(no = 6)
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
    text = get("useprecalclabl", envir = rnest)
  )

  assign("path1", "", envir = rnest)
  assign("path2", "", envir = rnest)
  assign("path3", "", envir = rnest)
  assign("path4", "", envir = rnest)

  loaddata <- function() {
    tt <- tktoplevel()
    tktitle(tt) <- get("loaddatalabl", envir = rnest)
    lab1 <- tklabel(tt, text = get("lab1labl", envir = rnest))
    lab2 <- tklabel(tt, text = get("lab2labl", envir = rnest))
    lab3 <- tklabel(tt, text = get("lab3labl", envir = rnest))
    lab4 <- tklabel(tt, text = get("lab4labl", envir = rnest))
    ent1 <- tkentry(
      tt,
      width = 50,
      textvariable = tclVar(get("path1", envir = rnest))
    )
    ent2 <- tkentry(
      tt,
      width = 50,
      textvariable = tclVar(get("path2", envir = rnest))
    )
    ent3 <- tkentry(
      tt,
      width = 50,
      textvariable = tclVar(get("path3", envir = rnest))
    )
    ent4 <- tkentry(
      tt,
      width = 50,
      textvariable = tclVar(get("path4", envir = rnest))
    )

    onok <- function() {
      x1 <- as.character(tclvalue(tkget(ent1)))
      path1 <- tclVar(x1)
      #if(x1==""){stop("No nest obervations provided")}
      #x1<-read.table(x1,header=T,stringsAsFactors=FALSE)
      assign("nestdata", x1, envir = rnest)
      assign("path1", x1, envir = rnest)
      x2 <- tclvalue(tkget(ent2))
      path2 <- tclVar(x2)
      #if(x2==""){stop("No nesting parameters provided")}
      #x2<-read.csv(x2,header=T,stringsAsFactors=FALSE)
      assign("nestparam", x2, envir = rnest)
      assign("path2", x2, envir = rnest)
      x3 <- tclvalue(tkget(ent3))
      path3 <- tclVar(x3)
      #if(x3==""){stop("No status codes provided")}
      #x3<-read.csv(x3,header=T,stringsAsFactors=FALSE)
      assign("neststatus", x3, envir = rnest)
      assign("path3", x3, envir = rnest)
      x4 <- tclvalue(tkget(ent4))
      path4 <- tclVar(x4)
      #if(x4==""){stop("No bird groups provided")}
      #x4<-read.csv(x4,header=T,stringsAsFactors=FALSE)
      assign("birdgroups", x4, envir = rnest)
      assign("path4", x4, envir = rnest)
      #print(ls(rnest))
      tkdestroy(tt)
    }

    browse <- function(num) {
      x <- gsub("\\\\", "/", file.choose())
      tkconfigure(get(paste("ent", num, sep = "")), textvariable = tclVar(x))
      tkgrid(get(paste("ent", num, sep = "")), row = num * 2, column = 0)
    }

    but1 <- tkbutton(
      tt,
      text = get("butbrowselabl", envir = rnest),
      pady = 0,
      width = 10,
      bg = "grey85",
      command = function() browse(1)
    )
    but2 <- tkbutton(
      tt,
      text = get("butbrowselabl", envir = rnest),
      pady = 0,
      width = 10,
      bg = "grey85",
      command = function() browse(2)
    )
    but3 <- tkbutton(
      tt,
      text = get("butbrowselabl", envir = rnest),
      pady = 0,
      width = 10,
      bg = "grey85",
      command = function() browse(3)
    )
    but4 <- tkbutton(
      tt,
      text = get("butbrowselabl", envir = rnest),
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

  topMenu <- tkmenu(tt)
  tkconfigure(tt, menu = topMenu)
  fileMenu <- tkmenu(topMenu, tearoff = FALSE)
  tkadd(
    topMenu,
    "cascade",
    label = get("menufilelabl", envir = rnest),
    menu = fileMenu
  )
  tkadd(
    fileMenu,
    "command",
    label = get("menuloaddatalabl", envir = rnest),
    command = function() {
      print("Not implemented yet")
      loaddata()
    }
  )

  tkadd(
    fileMenu,
    "command",
    label = get("menuquitlabl", envir = rnest),
    command = function() tkdestroy(tt)
  )

  optionsMenu <- tkmenu(topMenu, tearoff = FALSE)
  tkadd(topMenu, "cascade", label = "Options", menu = optionsMenu)
  tkadd(
    optionsMenu,
    "command",
    label = get("menuretrolabl", envir = rnest),
    command = function() print("Not implemented yet")
  )
  testMenu <- tkmenu(topMenu, tearoff = FALSE)
  tkadd(
    topMenu,
    "cascade",
    label = get("menutestlabl", envir = rnest),
    menu = testMenu
  )
  tkadd(
    testMenu,
    "command",
    label = get("menuranlabl", envir = rnest),
    command = function() {
      tk.single.nest(list_lang = list_lang)
    }
  )
  tkadd(
    testMenu,
    "command",
    label = get("menuficlabl", envir = rnest),
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
    text = get("ecodistrictblabl", envir = rnest),
    command = chooseecodistrict,
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
      tkmessageBox(
        title = "",
        message = paste(
          get("tkmb41", envir = rnest),
          "{ ",
          tclvalue(tkget(ecodistrict)),
          " }"
        ),
        icon = "info",
        type = "ok"
      )
    }
  )

  tkgrid(tklabel(tt, text = "       "), column = 5, row = 0)
  tkgrid(tklabel(tt, text = "   "), column = 7, row = 0)
  tkgrid(tklabel(tt, text = "       "), column = n + 8 + 1, row = 0)

  header4 <- tklabel(tt, text = get("header4labl", envir = rnest))
  tkconfigure(header4, foreground = "blue")
  tkgrid(header4, row = 1, column = 9, columnspan = n * 2, sticky = "we")
  grouplab <- tklabel(tt, text = get("grouplabl", envir = rnest))
  #tkgrid(grouplab,row=2,column=9,sticky="nw")

  fontmode <- tkfont.create(size = 14, weight = "bold")

  widget <- function(predict = FALSE) {
    assign("predict", predict, envir = rnest)
    if (!predict) {
      #tkconfigure(tt,bg="lightgreen")
      tkgrid(tklabel(tt, text = ""), columnspan = 1, row = 0)
      tkgrid(header1, columnspan = 4, row = 1)
      tkgrid(
        tklabel(tt, text = "                                   "),
        column = 0,
        row = 2
      )
      tkgrid(speciesb, row = 3, column = 0)
      tkgrid(species, row = 3, column = 1, sticky = "w")
      tkgrid(speciesq, row = 3, column = 2)
      tkgrid(yearlab, row = 4, column = 0)
      tkgrid(year, row = 4, column = 1, sticky = "w")
      tkgrid(yearq, row = 4, column = 2)
      tkgrid(usp, row = 5, column = 1, sticky = "w")
      tkgrid(ugr, row = 6, column = 1, sticky = "w")
      tkgrid(tklabel(tt, text = ""), row = 5)
      tkgrid(tklabel(tt, text = ""), row = 6)
      tkgrid(tklabel(tt, text = ""), row = 7)
      tkgrid(header2, columnspan = 4, row = 8)
      tkgrid(tklabel(tt, text = ""), row = 9)
      tkgrid(shpb, row = 10, column = 0)
      tkgrid(shp, row = 10, column = 1, sticky = "w")
      tkgrid(shpq, row = 10, column = 2)
      tkgrid(idb, row = 11, column = 0)
      tkgrid(id, row = 11, column = 1, sticky = "w")
      tkgrid(idq, row = 11, column = 2)
      tkgrid(regionb, row = 12, column = 0)
      tkgrid(region, row = 12, column = 1, sticky = "w")
      tkgrid(regionq, row = 12, column = 2)
      tkgrid(intchoicelab, row = 13, column = 0)
      tkgrid(intchoice, row = 13, column = 1, sticky = "w")
      tkgrid(intchoiceq, row = 13, column = 2)
      tkgrid(zoomlab, row = 14, column = 0)
      tkgrid(zoom, row = 14, column = 1, sticky = "w")
      tkgrid(zoomq, row = 14, column = 2)
      tkgrid(backsplab, row = 15, column = 0)
      tkgrid(backsp, row = 15, column = 1, sticky = "w")
      tkgrid(backspq, row = 15, column = 2)
      tkgrid(tklabel(tt, text = ""), row = 16)
      #tkgrid(tklabel(tt,text=""),row=17)
      tkgrid(butrun, column = 0, row = 17, columnspan = 4)
      tkgrid(butrunq, column = 2, row = 17)
      tkgrid(butmap, column = 0, row = 18, columnspan = 4)
      tkgrid(butmapq, column = 2, row = 18)
      #tkgrid(tklabel(tt,text=""),row=18)
      tkgrid(useprecalc, row = 20, column = 1, sticky = "w")
      tkgrid(nameback, row = 20, column = 0, sticky = "e")
      tkgrid(namebackq, row = 20, column = 2)
      tkgrid(tklabel(tt, text = ""), row = 21)
      tkgrid(header3, columnspan = 4, row = 22)
      tkgrid(tklabel(tt, text = ""), row = 23)
      tkgrid(datelab, row = 24, sticky = "e", column = 0)
      tkgrid(datebeg, row = 25, sticky = "e", column = 0)
      tkgrid(dateend, row = 26, sticky = "e", column = 0)
      tkgrid(yearcal, row = 26, column = 1, sticky = "w")
      tkgrid(spcal, row = 24, column = 1, sticky = "w")
      tkgrid(regioncal, row = 25, column = 1, sticky = "w")
      tkgrid(tklabel(tt, text = ""), row = 27)
      tkgrid(propbut, row = 28, column = 0)
      tkgrid(prop, row = 28, column = 1, sticky = "e")
      tkgrid(propq, row = 28, column = 2)
      tkgrid(colobut, row = 29, column = 0)
      tkgrid(colo, row = 29, column = 1, sticky = "e")
      tkgrid(coloq, row = 29, column = 2)
      tkgrid(showpropbut, row = 30)
      tkgrid(showpropcolo, row = 30, column = 1, sticky = "w")
      tkgrid(showpropnest, row = 30, column = 1, sticky = "e")
      tkgrid(showpropq, row = 30, column = 2)
      tkgrid(tklabel(tt, text = ""), row = 31)
      tkgrid(butcal, row = 32, columnspan = 4)
      tkgrid(butcalq, row = 32, column = 2)
      tkgrid(tklabel(tt, text = ""), row = 33)
      tkgrid(namerawcal, row = 34, column = 0, sticky = "e")
      tkgrid(namerawcalq, row = 34, column = 2)
      tkgrid(tklabel(tt, text = ""), row = 35)
      tkgrid(tklabel(tt, text = "   "), row = 0, column = n + 8 + 1)
      tkgrid(groupq, row = 34, column = 6, sticky = "w")
      tkgrid(butobs, column = 0, row = 36, columnspan = 100, sticky = "ns")
      #tkgrid(tklabel(tt,text=""),row=37)
      tkgrid.remove(butsp, butspq)
      tkgrid.remove(butpred)
      tkgrid.remove(buteco, butecoq)
      tkgrid.remove(ecodistrictb, ecodistrict, ecodistrictq)
      tkgrid.remove(uselectsp, uallsp, ugroups)
      tkgrid.remove(orderspgrcal, orderecocal)
      #l<-getopt()
      #if(is.logical(l$layer)){assign("usershp",ecodistricts,envir=rnest)}
    } else {
      #tkconfigure(tt,bg="lightblue")
      tkgrid.remove(yearlab, year, yearq)
      tkgrid.remove(usp, ugr)
      tkgrid.remove(idb, id, idq)
      tkgrid.remove(regionb, region, regionq)
      tkgrid.remove(butrun, butrunq)
      tkgrid.remove(butmap, butmapq)
      tkgrid.remove(useprecalc)
      tkgrid.remove(nameback, namebackq)
      tkgrid(orderspgrcal, row = 25, column = 1, sticky = "w")
      tkgrid(orderecocal, row = 26, column = 1, sticky = "w")
      tkgrid(uselectsp, row = 5, column = 1, sticky = "w")
      tkgrid(uallsp, row = 4, column = 1, sticky = "w")
      tkgrid(ugroups, row = 6, column = 1, sticky = "w")
      tkgrid(ecodistrictb, row = 11, column = 0)
      tkgrid(ecodistrict, row = 11, column = 1, sticky = "w")
      tkgrid(ecodistrictq, row = 11, column = 2)
      tkgrid(butsp, column = 0, row = 17, columnspan = 4)
      tkgrid(butspq, column = 2, row = 17)
      tkgrid(buteco, column = 0, row = 18, columnspan = 4)
      tkgrid(butecoq, column = 2, row = 18)
      tkgrid(butpred, column = 0, row = 36, columnspan = 100, sticky = "ns")
      tkgrid.remove(butobs)
      tkgrid.remove(yearcal, spcal, regioncal)
      #l<-getopt()
      #if(is.logical(l$layer)){assign("usershp",NULL,envir=rnest)}
    }
  }

  butpred <- tkbutton(
    tt,
    text = get("butpredlabl", envir = rnest),
    bg = "grey55",
    width = 115,
    bg = "lightblue",
    fg = "red",
    font = fontmode,
    command = function() {
      tclServiceMode(FALSE)
      widget(predict = FALSE)
      rejected(fromobsbut = FALSE)
      relistsp(sg, lgroups)
      tclServiceMode(TRUE)
    }
  )
  butobs <- tkbutton(
    tt,
    text = get("butobslabl", envir = rnest),
    bg = "grey55",
    width = 115,
    bg = "lightgreen",
    fg = "red",
    font = fontmode,
    command = function() {
      tclServiceMode(FALSE)
      widget(predict = TRUE)
      rejected(fromobsbut = TRUE)
      relistsp(sg, lgroups)
      tclServiceMode(TRUE)
    }
  )

  butrun <- tkbutton(
    tt,
    text = get("butrunlabl", envir = rnest),
    bg = "grey55",
    width = 27,
    command = run
  )
  butrunq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tkmessageBox(
        title = "",
        message = get("butrunqtext", envir = rnest),
        icon = "info",
        type = "ok"
      )
    }
  )
  butmap <- tkbutton(
    tt,
    text = get("butmaplabl", envir = rnest),
    bg = "grey55",
    width = 27,
    command = mapnest
  )
  butmapq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tkmessageBox(
        title = "",
        message = get("butmapqtext", envir = rnest),
        icon = "info",
        type = "ok"
      )
    }
  )
  buteco <- tkbutton(
    tt,
    text = get("butecolabl", envir = rnest),
    bg = "grey55",
    width = 27,
    command = mapeco
  )
  butecoq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tkmessageBox(
        title = "",
        message = get("butecoqtext", envir = rnest),
        icon = "info",
        type = "ok"
      )
    }
  )
  butsp <- tkbutton(
    tt,
    text = get("butsplabl", envir = rnest),
    bg = "grey55",
    width = 27,
    command = mapsp
  )
  butspq <- tkbutton(
    tt,
    text = "?",
    pady = 0,
    bg = "grey85",
    command = function() {
      tkmessageBox(
        title = "",
        message = get("butspqtext", envir = rnest),
        icon = "info",
        type = "ok"
      )
    }
  )

  widget(predict = FALSE)

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
  invisible(tclServiceMode(TRUE))
}
