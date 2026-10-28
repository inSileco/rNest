active.nest <-
  function(
    d,
    useprecalc = FALSE,
    error = FALSE,
    nparam = NULL,
    sp = FALSE,
    year = FALSE,
    layer = FALSE,
    region = FALSE,
    id = FALSE,
    int.choice = FALSE,
    zoom = TRUE,
    locations = FALSE,
    progress = TRUE,
    lsc = NULL,
    spgroup = NULL,
    lang = "english",
    fromtk = FALSE,
    ...
  ) {
    if (lang == "english") {
      me1 <- "No nests found for these species."
      me2 <- "Some species not found in the database:"
      me3 <- "No nests found for those years/species."
      me4 <- "No nests found for the years/species combination."
      me5 <- "Some species not found in the database for those years:"
      me6 <- "Species not found in the nesting parameters:"
      me7 <- "No species found in the nesting parameters."
      me8 <- "Missing nesting information for the following species:"
      me9 <- "No layer or interactive choice specified."
      me10 <- "No data."
      me11 <- "Do not use NA as labels for attributes. Some were found in"
      me12 <- "No nests in the regions given."
      me13 <- "No nests found in the layer or first/id attribute with NA labels."
      me14 <- "Plot a region to zoom in using left button\nUse right-click and stop to end selection"
      me15 <- "Select a region using left button\nUse right-click and stop to end selection"
      me16 <- "selected region"
      me17 <- c("No nest found for the", "region")
    } else {
      me1 <- "Aucun nid trouv? pour les esp?ces s?lectionn?es."
      me2 <- "Certaines esp?ces n'ont pas ?t? trouv?es dans les donn?es:"
      me3 <- "Aucun nid trouv? pour les ann?es/esp?ces s?lectionn?es."
      me4 <- "Aucun nid trouv? pour cette combinaison ann?es/esp?ces."
      me5 <- "Des nids de certaines esp?ces n'ont pas ?t? trouv?es pour les ann?es sp?cifi?es:"
      me6 <- "Certaines esp?ces absentes des param?tres de nidification:"
      me7 <- "Aucune esp?ce pr?sente dans les param?tres de nidification."
      me8 <- "Param?tres de nidification manquant pour les esp?ces suivantes:"
      me9 <- "Aucun fichier de forme fourni ou aucune r?gion interactive fournie."
      me10 <- "Aucune donn?e fournie."
      me11 <- "Ne pas utiliser la valeur NA pour les attributs du fichier de forme. Certains ont ?t? trouv?s dans les colonnes:"
      me12 <- "Aucun nid trouv? dans les r?gions sp?cifi?es."
      me13 <- "Aucun nid trouv? dans la r?gion sp?cifi?e ou premi?re colonne des attributs du fichier de forme contenant des NAs."
      me14 <- "Tracez une r?gion ? agrandir en utilisant le bouton de gauche\nUtilisez le bouton de droite pour terminer la s?lection"
      me15 <- "S?lectionnez les r?gions en utilisant le bouton de gauche\nUtilisez le bouton de droite pour terminer la s?lection"
      me16 <- "r?gion s?lectionn?e"
      me17 <- c("Aucun nid trouv? pour les r?gions", "")
    }

    d <- subset(
      d,
      select = c(
        nest_id,
        species_code,
        visit_id,
        date,
        eggs,
        young,
        status_code,
        stage_code,
        lat,
        lon,
        initiation,
        fledging,
        warning,
        len_active,
        span_error
      )
    ) #add-on to subet years, has to be modified it is temporary for bruno
    #d<-d[,c("nest_id","species_code","lat","lon","status_code","visit_id","date","eggs","young","stage_code")]
    if (is.character(sp)) {
      d <- d[d$species_code %in% sp, ]
    }
    if (!is.null(spgroup)) {
      d <- d[d$species_code %in% unlist(spgroup), ]
    }
    if (nrow(d) == 0) {
      if (fromtk) {
        tkmessageBox(title = "", message = me1, icon = "info", type = "ok")
      }
      stop(me1)
    }
    if (is.character(sp)) {
      m <- match(sp, d$species_code)
      #m<-ifelse(is.na(m),FALSE,TRUE)
      if (any(is.na(m))) {
        if (fromtk) {
          tkmessageBox(
            title = "",
            message = paste(me2, sp[is.na(m)]),
            icon = "info",
            type = "ok"
          )
        }
        warning(paste(me2, sp[is.na(m)]), call. = T, immediate. = TRUE)
      }
    }
    if (is.numeric(year)) {
      d <- d[as.numeric(substr(d$date, 1, 4)) %in% year, ]
    } #add-on to subet years, has to be modified it is temporary for bruno
    if (nrow(d) == 0) {
      if (fromtk) {
        tkmessageBox(title = "", message = me3, icon = "info", type = "ok")
      }
      stop(me3)
    } #add-on to subet years, has to be modified it is temporary for bruno
    # peut donner un warning si une des esp?ces n,est pas dans la database
    if (is.character(sp)) {
      d <- d[d$species_code %in% sp, ]
    }
    if (nrow(d) == 0) {
      if (fromtk) {
        tkmessageBox(title = "", message = me4, icon = "info", type = "ok")
      }
      stop(me4)
    } #add-on to subet years, has to be modified it is temporary for bruno
    if (is.character(sp)) {
      m <- match(sp, d$species_code)
      #m<-ifelse(is.na(m),FALSE,TRUE)
      if (any(is.na(m))) {
        if (fromtk) {
          tkmessageBox(
            title = "",
            message = paste(me5, sp[is.na(m)]),
            icon = "info",
            type = "ok"
          )
        }
        warning(paste(me5, sp[is.na(m)]), call. = T, immediate. = TRUE)
      }
    }

    ### info on nesting parameters
    #species<-read.csv("species2precocial.csv",header=T,stringsAsFactors=FALSE)

    #species<-nestingparameters
    if (!is.null(nparam)) {
      species <- nparam
    } else {
      species <- nestingparameters
    }

    if (!all(unique(d$species_code) %in% species$species_code)) {
      w <- which(!d$species_code %in% species$species_code)
      if (any(w)) {
        if (fromtk) {
          tkmessageBox(
            title = "",
            message = paste(
              me6,
              paste(unique(d$species_code[w]), collapse = " ")
            ),
            icon = "info",
            type = "ok"
          )
        }
        warning(
          paste(me6, paste(unique(d$species_code[w]), collapse = " ")),
          immediate. = TRUE
        )
      }
      d <- d[-w, ]
      if (nrow(d) == 0) {
        if (fromtk) {
          tkmessageBox(title = "", message = me7, icon = "info", type = "ok")
        }
        stop(me7)
      }
    }
    #rname<-rownames(d) # see if warranted !! keep rownames to fit with dattest cause merge renames ans sorts
    d <- merge(d, species, all.x = T, sort = FALSE)
    #rownames(d)<-rname # keep rownames
    info <- apply(
      d[, c("clutch", "cmin", "cmax", "incub", "start", "rearing", "laying")],
      1,
      function(i) {
        any(is.na(i))
      }
    )
    if (any(info)) {
      if (fromtk) {
        tkmessageBox(
          title = "",
          message = paste(
            me8,
            paste(unique(d$species_code[info]), collapse = " ")
          ),
          icon = "info",
          type = "ok"
        )
      }
      warning(
        paste(me8, paste(unique(d$species_code[info]), collapse = " ")),
        immediate. = TRUE
      )
      d <- d[!info, ]
    }
    ### associating status code with stage code
    if (is.null(lsc)) {
      lsc <- list(
        nest = c("AB", "FB", "MB", "PB", "N1", "N2", "N3", "N4", "NC", "NL"),
        laying = c("FR"),
        #incubation=c("DE","WA"),  # check nest 66546
        hatch = c("HA", "PI"),
        fledge = c("RF", "SL", "SY", "YC", "LB", "ON", "EX")
      )
    }
    if (!is.null(lsc)) {
      d$stage_code <- sub_list(d$status_code, lsc)
    }

    if (!is.logical(region) && is.logical(layer) && !int.choice) {
      if (fromtk) {
        tkmessageBox(title = "", message = me9, icon = "info", type = "ok")
      }
      stop(me9)
    }
    if (!is.logical(layer) || int.choice) {
      d <- d[!(is.na(d$lon) | is.na(d$lat)), ]
      if (nrow(d) == 0) {
        if (fromtk) {
          tkmessageBox(title = "", message = me10, icon = "info", type = "ok")
        }
        stop(me10)
      }
      if (!int.choice) {
        if (!is.logical(layer)) {
          if (is.character(layer)) {
            arg <- breakshapepath(layer)
            reg <- readOGR(dsn = arg[1], layer = arg[2])
          }
          if (inherits(layer, "SpatialPolygonsDataFrame")) {
            reg <- layer
          }
          if (!is.logical(id) && length(id) > 1) {
            slot(reg, "data") <- slot(reg, "data")[, id, drop = FALSE]
            newatt <- paste(id, collapse = "_")
            slot(reg, "data") <- data.frame(
              newatt = apply(slot(reg, "data"), 1, paste, collapse = "_"),
              stringsAsFactors = FALSE
            )
          } else {
            if (is.logical(id)) {
              slot(reg, "data") <- slot(reg, "data")[, 1, drop = FALSE]
              if (any(is.na(slot(reg, "data")[, 1]))) {
                if (fromtk) {
                  tkmessageBox(
                    title = "",
                    message = paste(me11, names(slot(reg, "data"))[1]),
                    icon = "info",
                    type = "ok"
                  )
                }
                warning(
                  paste(me11, names(slot(reg, "data"))[1]),
                  immediate. = TRUE
                )
              }
            } else {
              slot(reg, "data") <- slot(reg, "data")[, id, drop = FALSE]
              if (any(is.na(slot(reg, "data")[, 1]))) {
                if (fromtk) {
                  tkmessageBox(
                    title = "",
                    message = paste(me11, id),
                    icon = "info",
                    type = "ok"
                  )
                }
                warning(paste(me11, id), immediate. = TRUE)
              }
            }
          }
          x <- d
          coordinates(x) = cbind(x$lon, x$lat, stringsAsFactors = FALSE)
          proj4string(x) <- CRS(proj4string(reg))
          spa <- over(x, reg)
          if (is.factor(spa[, 1])) {
            spa[, 1] <- as.character(spa[, 1])
          } # sinon overlay convertit tou en factor siboire!
          if (!is.logical(region)) {
            w <- which(spa[, 1] %in% region)
            if (!any(w)) {
              if (fromtk) {
                tkmessageBox(
                  title = "",
                  message = me12,
                  icon = "info",
                  type = "ok"
                )
              }
              stop(me12)
            }
            d <- cbind(
              d[w, ],
              spa[w, 1, drop = FALSE],
              stringsAsFactors = FALSE
            )
          } else {
            d <- cbind(d, spa[, 1, drop = FALSE], stringsAsFactors = FALSE)
          }
          names(d)[ncol(d)] <- "region"
          d <- d[!is.na(d$region), ]
          if (nrow(d) == 0) {
            if (fromtk) {
              tkmessageBox(
                title = "",
                message = me13,
                icon = "info",
                type = "ok"
              )
            }
            stop(me13)
          }
        }
      }
      if (int.choice) {
        if (is.logical(layer)) {
          data(worldHiresMapEnv)
          par(mar = c(2, 0, 0, 0))
          map(
            'worldHires',
            fill = T,
            bg = "cyan",
            col = "lightgoldenrod",
            xpd = FALSE
          )
          mtext("Zoom region using 4 clicks", side = 3, font = 2, cex = 1.1)
          ran <- locator(4)
          par(mar = c(2, 0, 0, 0))
          map(
            'worldHires',
            ylim = range(ran$y),
            xlim = range(ran$x),
            fill = T,
            bg = "cyan",
            col = "lightgoldenrod",
            xpd = FALSE
          )
          mtext(
            "Use right-click and stop to end selection",
            side = 1,
            outer = FALSE,
            font = 2,
            cex = 1.1
          )
          points(d$lon, d$lat, pch = 16, col = "red", cex = 0.6)
          pol <- getpoly(quiet = T)
          polygon(pol, lwd = 4, border = "darkgreen")
        } else {
          # maybe here the region should be subsetted according to shapefile, region AND selection?
          if (is.character(layer)) {
            arg <- breakshapepath(layer)
            reg <- readOGR(dsn = arg[1], layer = arg[2])
          }
          if (inherits(layer, "SpatialPolygonsDataFrame")) {
            reg <- layer
          }
          x11()
          par(mar = c(2, 0, 0, 0))
          plot(reg)
          if (zoom) {
            mtext(me14, side = 1, outer = FALSE, font = 2, cex = 1.1)
            points(d$lon, d$lat, pch = 16, col = "red", cex = 0.6)
            pol <- getpoly(quiet = T)
            pol <- SpatialPolygons(
              list(Polygons(list(Polygon(pol[c(1:nrow(pol), 1), ])), ID = 1)),
              proj4string = CRS(proj4string(layer))
            )
            plot(
              reg,
              xlim = range(bbox(pol)[1, ]),
              ylim = range(bbox(pol)[2, ])
            )
          }
          mtext(me15, side = 1, outer = FALSE, font = 2, cex = 1.1)
          points(d$lon, d$lat, pch = 16, col = "red", cex = 0.6)
          pol <- getpoly(quiet = T)
          polygon(pol, lwd = 4, border = "darkgreen")
        }
        int <- inout(cbind(x = d$lon, y = d$lat), pol)
        d <- cbind(d, region = rep(me16, nrow(d)), stringsAsFactors = FALSE)
        d <- d[int, ]
        if (locations) {
          points(d$lon, d$lat, pch = 16, col = "red", cex = 0.6)
        }
      }
      if (nrow(d) == 0) {
        if (fromtk) {
          tkmessageBox(
            title = "",
            message = paste(
              me17[1],
              if (!is.logical(region)) {
                region
              },
              me17[2]
            ),
            icon = "info",
            type = "ok"
          )
        }
        stop(paste(
          me17[1],
          if (!is.logical(region)) {
            region
          },
          me17[2]
        ))
      }
      if (locations & int.choice) {
        points(d$lon, d$lat, pch = 16, col = "red", cex = 0.6)
      } #maybe remove or adapt?
      if (locations & !int.choice) {
        plot(reg)
        points(d$lon, d$lat, pch = 16, col = "red", cex = 0.6)
      } #maybe remove or adapt?
    }
    if (is.logical(layer) && !int.choice) {
      d <- cbind(d, region = rep(NA, nrow(d)), stringsAsFactors = FALSE)
    }
    if (nrow(d) == 0) {
      if (fromtk) {
        tkmessageBox(
          title = "",
          message = paste(
            me17[1],
            if (!is.logical(region)) {
              region
            },
            me17[2]
          ),
          icon = "info",
          type = "ok"
        )
      }
      stop(paste(
        me17[1],
        if (!is.logical(region)) {
          region
        },
        me17[2]
      ))
    }
    d <- d[order(d$nest_id, d$date), ]
    id <- unique(d$nest_id)
    respre <- unique(subset(
      d,
      select = c(
        species_code,
        nest_id,
        region,
        initiation,
        fledging,
        warning,
        len_active,
        span_error
      )
    ))
    #res<-data.frame(sp=res$species_code,id=res$nest_id,region=res$region,initiation=as.Date(rep(NA,nrow(res))),fledging=as.Date(rep(NA,nrow(res))),warning=rep(NA,nrow(res)),stringsAsFactors=FALSE)
    res <- data.frame(
      sp = respre$species_code,
      id = respre$nest_id,
      region = respre$region,
      stringsAsFactors = FALSE
    )
    res2 <- matrix(NA, nrow = nrow(res), ncol = 5)
    d <- data.table(d)
    setkey(d, "nest_id")
    error_id <- NULL
    assign("error_id", error_id, envir = .GlobalEnv)
    #browser()
    if (useprecalc) {
      res2 <- respre[, c(
        "initiation",
        "fledging",
        "warning",
        "len_active",
        "span_error"
      )]
    } else {
      for (i in 1:nrow(res)) {
        x <- d[J(res$id[i])]
        #x<-d[d$nest_id==res$id[i],]

        if (
          class(try(
            ans <- chronology(x, details = FALSE, test = FALSE, ...),
            silent = TRUE
          )) ==
            "try-error"
        ) {
          if (error) {
            print(paste("nest_id", id[i]))
            ans <- chronology(x, details = FALSE, test = FALSE, ...)
          } else {
            error_id <- c(error_id, id[i])
            ans <- c(NA, NA, "nest generated an error message", NA, NA)
            assign("error_id", error_id, envir = .GlobalEnv)
          }
          #traceback()
          #stop(geterrmessage())
        }
        #ans<-chronology(x,details=FALSE,test=FALSE,...)
        #print(ans)
        res2[i, ] <- ans
        #res$initiation[i]<-ans[1]
        #res$fledging[i]<-ans[2]
        #res$warning[i]<-ans[3]
        if (progress && !(i %% 5)) {
          progress(i, nrow(res))
        }
      }
      res2 <- data.frame(
        initiation = as.Date(as.numeric(res2[, 1]), origin = "1970-01-01"),
        fledging = as.Date(as.numeric(res2[, 2]), origin = "1970-01-01"),
        warning = res2[, 3],
        len_active = res2[, 4],
        span_error = res2[, 5],
        stringsAsFactors = FALSE
      )
    }
    #print(paste("error_id:",error_id))
    assign("error_id", error_id, envir = .GlobalEnv)
    res <- data.frame(res, res2, stringsAsFactors = FALSE)
    #if(!is.logical(year)){
    #     if(is.numeric(year)){
    #          res<-res[as.numeric(substr(res$initiation,1,4))%in%year,]
    #          }
    #     if(is.character(year)){
    #          res<-res[as.character(res$initiation)%in%year | as.character(res$fledging)%in%year,]
    #          }
    #     }
    # watch ou here for nests that don't have dates because of no eggs or young ever but visits with dates
    # watch out with year subsetting when a nest overlaps 01-01, now supposed to be ok
    # but really inefficient to subset here and not earlier to reduce the number of iterations, maybe find another way?
    if (!is.null(spgroup)) {
      if (any(duplicated(unlist(spgroup)))) {
        res2 <- list()
        for (i in 1:length(spgroup)) {
          temp <- res[res$sp %in% spgroup[[i]], ]
          temp$sp <- sub_list(temp$sp, spgroup[i])
          res2[[i]] <- temp
        }
        res <- do.call("rbind", res2)
      } else {
        res$sp <- sub_list(res$sp, spgroup)
      }
      res <- res[order(match(res$sp, names(spgroup))), ] ### add 2014-01-20 keep order of groups
    }
    res
  }
