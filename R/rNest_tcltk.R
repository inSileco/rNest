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
  data(nestwatch)
  data(nestingparameters)
  data(birdgroupings)
  data(ecodistrictscsv)
  data(predictions)
  data(specodistricts)
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
  dat <- as.data.frame(do.call("rbind", strsplit(dat$id, "_")))
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
  #winMenuAddItem("rNest","single nest","rNesttest:::tk.single.nest()")
  #winMenuAddItem("rNest","single nest test","rNesttest:::tk.single.nest.test()")
  #winMenuAddItem("rNest","nesting calendar","rNesttest:::tk.nesting.calendar()")
  #winMenuAddItem("rNest","explore data","rNesttest:::tk.data()")
}
