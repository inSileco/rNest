#' A Function to Infer Nesting Chronology from Nest Observations to Several
#' Nests
#'
#' Infers nest chronology from nest observations
#'
#' Details of the algorithm are described in the Rnest manual.
#'
#' @param d A `data.frame` containing observations for one or several
#' nests. An object of class `data.frame` with column names?: nest_id,
#' species_code, date, eggs, young and optionally status_code, lon and lat.
#' @param useprecalc When used with the `nestwatch` database,
#' precalculated data can be used to speed up the process whent set to
#' `TRUE`.
#' @param error When set to `TRUE`, nest ids generating an error (either
#' due to erroneous data or a bug in Rnest) will be displayed.
#' @param nparam A `data.frame` containing nesting parameters for the
#' species. An object of class `data.frame` with column names?:
#' species_code, cmin, cmax, clutch, laying, start, incub and rearing.
#' @param sp A set of species for which the nesting chronology has to be
#' inferred. All species are included when the argument is set to `FALSE`.
#' Otherwise, a `vector` with the species code has to be given.
#' @param year Used to subset nests to be included based on their date of
#' initiation. All nests are included when the argument is set to `FALSE`.
#' Otherwise, either a `vector` containing dates (in character format) or
#' years (in numeric/integer format) to be included. Years and dates given can
#' precede or exceed values observed in the database. When the nesting
#' chronology of a nest encompasses two years and years are given as an
#' argument, it will be included if its date of initiation corresponds to a
#' year given in the subset. If the argument is a set of dates, nests for which
#' either the initiation or the fledging date correspond to one of the dates
#' given will be included.
#' @param layer Name of a layer (ESRI shapefile) used to restrict the subset of
#' nests studied to polygons contained in the layer. All nests are used when
#' layer is set to `FALSE`, unless the argument `int.choice` is set
#' to `TRUE`. Otherwise, either an `sf` object of polygons (an `sp`
#' `SpatialPolygonsDataFrame` is also accepted and converted with
#' [sf::st_as_sf()]) or a character string giving the path of a layer
#' (without the extension) containing the region of interest has to be given.
#' A path without a directory refers to the R working directory, which must
#' also contain the other files needed to interpret the layer. The layer has to be in
#' geographic coordinates with the NAD83 system. Used in conjonction with
#' `region` and `int.choice`.
#' @param region Regions to which the nests should be restricted. The two
#' possible values are `FALSE` or a `vector` of the names of regions
#' of interest. When `int.choice` is `TRUE`, the name of the first
#' region will be given to the selected polygon and a warning will be given if
#' more than one region is specified. When `int.choice` is `FALSE`, a
#' layer containing the names of the regions in its attribute table has to be given,
#' otherwise, an error message will be returned.
#' @param id Names of the columns in the attribute table of the shapefile to be
#' used to create the regions of interest. A character `vector` with the
#' names of columns in the attribute table. When set to `FALSE`, the first
#' column is used to describe the regions.
#' @param int.choice Let's the user draw a polygon containing the region of
#' interest. When set to `TRUE`, the user is asked to draw a polygon in
#' the graphic device. If a layer is specified, the polygon is drawn in a plot
#' showing the layer. If layer is set to `FALSE`, a world map is plotted
#' and the user is asked for a polygon. The first 4 clicks on the graphic
#' device will be used to rescale the map to get a greater accuracy in the
#' drawing of the polygon.
#' @param zoom Used when `int.choice` is `TRUE` and a layer is given.
#' When set to `TRUE`, the user first draws a polygon around the area to
#' zoom in on, then draws the region of interest.
#' @param locations Used to map the locations of the nests within the region
#' studied. When set to `TRUE`, nest locations contained in the regions of
#' interest are plotted.
#' @param progress Displays the progression of the function (number of nests
#' for which the chronology has been estimated). The progression is displayed
#' when progress is set to `TRUE`. This argument uses the function
#' [svMisc::progress()] from the svMisc package.
#' @param lsc A named `list` with stages as names and each element of the
#' `list` has to be status codes to be interpreted as such.
#' @param spgroup A named `list` with group names as names. Each element
#' of the `list` has to be a `vector` of characters with the species
#' 4-letters AOU codes.
#' @param lang Used within the tcltk interface to choose the language used.
#' Default value `"en"`, otherwise `"fr"` is used.
#' @param fromtk Used when the function is called within the tcltk interface to
#' generate error and warning messages with [tcltk::tkmessageBox()]. Default
#' value `FALSE`.
#' @param ... Further arguments to be passed to the [chronology()]
#' function.
#' @return A [data.frame()] with the following columns:
#' * `sp`: species 4-letters AOU code
#' * `id`: nest id
#' * `region`: region where the nest is found
#' * `initiation`: estimated initiation date (character)
#' * `fledging`: estimated fledging date or of departure from the nest date
#'   (character)
#' * `warning`: warning encountered when backcalculating
#'
#' @note To be described
#' @author Francois Rousseu <francoisrousseu@@hotmail.com>
#' @seealso [chronology()], [nesting.activity()]
#' @export
active.nest <- function(
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
  lang = "en",
  fromtk = FALSE,
  ...
) {
  msg <- i18n("active.nest", lang)

  d <- d |>
    dplyr::select(dplyr::all_of(c(
      "nest_id",
      "species_code",
      "visit_id",
      "date",
      "eggs",
      "young",
      "status_code",
      "stage_code",
      "lat",
      "lon",
      "initiation",
      "fledging",
      "warning",
      "len_active",
      "span_error"
    )))
  if (is.character(sp)) {
    d <- d[d$species_code %in% sp, ]
  }
  if (!is.null(spgroup)) {
    d <- d[d$species_code %in% unlist(spgroup), ]
  }
  if (nrow(d) == 0) {
    if (fromtk) {
      tkmessageBox(
        title = "",
        message = msg$no_nests_species,
        icon = "info",
        type = "ok"
      )
    }
    stop(msg$no_nests_species)
  }
  if (is.character(sp)) {
    m <- match(sp, d$species_code)
    if (any(is.na(m))) {
      if (fromtk) {
        tkmessageBox(
          title = "",
          message = paste(msg$species_not_found, sp[is.na(m)]),
          icon = "info",
          type = "ok"
        )
      }
      warning(
        paste(msg$species_not_found, sp[is.na(m)]),
        call. = TRUE,
        immediate. = TRUE
      )
    }
  }
  if (is.numeric(year)) {
    d <- d[as.numeric(substr(d$date, 1, 4)) %in% year, ]
  } #add-on to subet years, has to be modified it is temporary for bruno
  if (nrow(d) == 0) {
    if (fromtk) {
      tkmessageBox(
        title = "",
        message = msg$no_nests_years_species,
        icon = "info",
        type = "ok"
      )
    }
    stop(msg$no_nests_years_species)
  } #add-on to subet years, has to be modified it is temporary for bruno
  # peut donner un warning si une des esp?ces n,est pas dans la database
  if (is.character(sp)) {
    d <- d[d$species_code %in% sp, ]
  }
  if (nrow(d) == 0) {
    if (fromtk) {
      tkmessageBox(
        title = "",
        message = msg$no_nests_years_species_combination,
        icon = "info",
        type = "ok"
      )
    }
    stop(msg$no_nests_years_species_combination)
  } #add-on to subet years, has to be modified it is temporary for bruno
  if (is.character(sp)) {
    m <- match(sp, d$species_code)
    #m<-ifelse(is.na(m),FALSE,TRUE)
    if (any(is.na(m))) {
      if (fromtk) {
        tkmessageBox(
          title = "",
          message = paste(msg$species_not_found_years, sp[is.na(m)]),
          icon = "info",
          type = "ok"
        )
      }
      warning(
        paste(msg$species_not_found_years, sp[is.na(m)]),
        call. = TRUE,
        immediate. = TRUE
      )
    }
  }

  ### info on nesting parameters
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
            msg$species_not_in_nparam,
            paste(unique(d$species_code[w]), collapse = " ")
          ),
          icon = "info",
          type = "ok"
        )
      }
      warning(
        paste(
          msg$species_not_in_nparam,
          paste(unique(d$species_code[w]), collapse = " ")
        ),
        immediate. = TRUE
      )
    }
    d <- d[-w, ]
    if (nrow(d) == 0) {
      if (fromtk) {
        tkmessageBox(
          title = "",
          message = msg$no_species_in_nparam,
          icon = "info",
          type = "ok"
        )
      }
      stop(msg$no_species_in_nparam)
    }
  }

  d <- merge(d, species, all.x = TRUE, sort = FALSE)
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
          msg$missing_nesting_info,
          paste(unique(d$species_code[info]), collapse = " ")
        ),
        icon = "info",
        type = "ok"
      )
    }
    warning(
      paste(
        msg$missing_nesting_info,
        paste(unique(d$species_code[info]), collapse = " ")
      ),
      immediate. = TRUE
    )
    d <- d[!info, ]
  }
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
      tkmessageBox(
        title = "",
        message = msg$no_layer_or_choice,
        icon = "info",
        type = "ok"
      )
    }
    stop(msg$no_layer_or_choice)
  }
  if (!is.logical(layer) || int.choice) {
    d <- d[!(is.na(d$lon) | is.na(d$lat)), ]
    if (nrow(d) == 0) {
      if (fromtk) {
        tkmessageBox(
          title = "",
          message = msg$no_data,
          icon = "info",
          type = "ok"
        )
      }
      stop(msg$no_data)
    }
    if (!int.choice) {
      if (!is.logical(layer)) {
        reg <- as_layer(layer)
        att <- sf::st_drop_geometry(reg)
        if (!is.logical(id) && length(id) > 1) {
          att <- att[, id, drop = FALSE]
          newatt <- paste(id, collapse = "_")
          att <- data.frame(
            newatt = apply(att, 1, paste, collapse = "_"),
            stringsAsFactors = FALSE
          )
        } else {
          if (is.logical(id)) {
            att <- att[, 1, drop = FALSE]
            if (any(is.na(att[, 1]))) {
              if (fromtk) {
                tkmessageBox(
                  title = "",
                  message = paste(
                    msg$na_attribute_labels,
                    names(att)[1]
                  ),
                  icon = "info",
                  type = "ok"
                )
              }
              warning(
                paste(msg$na_attribute_labels, names(att)[1]),
                immediate. = TRUE
              )
            }
          } else {
            att <- att[, id, drop = FALSE]
            if (any(is.na(att[, 1]))) {
              if (fromtk) {
                tkmessageBox(
                  title = "",
                  message = paste(msg$na_attribute_labels, id),
                  icon = "info",
                  type = "ok"
                )
              }
              warning(paste(msg$na_attribute_labels, id), immediate. = TRUE)
            }
          }
        }
        spa <- att[
          first_hit(layer_points(d$lon, d$lat, reg), reg),
          ,
          drop = FALSE
        ]
        if (is.factor(spa[, 1])) {
          spa[, 1] <- as.character(spa[, 1])
        } # sinon overlay convertit tou en factor siboire!
        if (!is.logical(region)) {
          w <- which(spa[, 1] %in% region)
          if (!any(w)) {
            if (fromtk) {
              tkmessageBox(
                title = "",
                message = msg$no_nests_regions,
                icon = "info",
                type = "ok"
              )
            }
            stop(msg$no_nests_regions)
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
              message = msg$no_nests_layer,
              icon = "info",
              type = "ok"
            )
          }
          stop(msg$no_nests_layer)
        }
      }
    }
    if (int.choice) {
      if (is.logical(layer)) {
        par(mar = c(2, 0, 0, 0))
        map(
          "mapdata::worldHires",
          fill = TRUE,
          bg = "cyan",
          col = "lightgoldenrod",
          xpd = FALSE
        )
        mtext("Zoom region using 4 clicks", side = 3, font = 2, cex = 1.1)
        ran <- locator(4)
        par(mar = c(2, 0, 0, 0))
        map(
          "mapdata::worldHires",
          ylim = range(ran$y),
          xlim = range(ran$x),
          fill = TRUE,
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
        pol <- draw_poly()
        polygon(pol, lwd = 4, border = "darkgreen")
      } else {
        # maybe here the region should be subsetted according to shapefile, region AND selection?
        reg <- as_layer(layer)
        x11()
        par(mar = c(2, 0, 0, 0))
        plot_layer(reg)
        if (zoom) {
          mtext(
            msg$zoom_instructions,
            side = 1,
            outer = FALSE,
            font = 2,
            cex = 1.1
          )
          points(d$lon, d$lat, pch = 16, col = "red", cex = 0.6)
          pol <- draw_poly()
          if (nrow(pol) >= 2) {
            plot_layer(reg, xlim = range(pol[, 1]), ylim = range(pol[, 2]))
          }
        }
        mtext(
          msg$select_instructions,
          side = 1,
          outer = FALSE,
          font = 2,
          cex = 1.1
        )
        points(d$lon, d$lat, pch = 16, col = "red", cex = 0.6)
        pol <- draw_poly()
        polygon(pol, lwd = 4, border = "darkgreen")
      }
      int <- in_polygon(d$lon, d$lat, pol)
      d <- cbind(
        d,
        region = rep(msg$selected_region, nrow(d)),
        stringsAsFactors = FALSE
      )
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
            msg$no_nests_region[1],
            if (!is.logical(region)) {
              region
            },
            msg$no_nests_region[2]
          ),
          icon = "info",
          type = "ok"
        )
      }
      stop(paste(
        msg$no_nests_region[1],
        if (!is.logical(region)) {
          region
        },
        msg$no_nests_region[2]
      ))
    }
    if (locations & int.choice) {
      points(d$lon, d$lat, pch = 16, col = "red", cex = 0.6)
    } #maybe remove or adapt?
    if (locations & !int.choice) {
      plot_layer(reg)
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
          msg$no_nests_region[1],
          if (!is.logical(region)) {
            region
          },
          msg$no_nests_region[2]
        ),
        icon = "info",
        type = "ok"
      )
    }
    stop(paste(
      msg$no_nests_region[1],
      if (!is.logical(region)) {
        region
      },
      msg$no_nests_region[2]
    ))
  }
  d <- d[order(d$nest_id, d$date), ]
  id <- unique(d$nest_id)
  respre <- d |>
    dplyr::select(dplyr::all_of(c(
      "species_code",
      "nest_id",
      "region",
      "initiation",
      "fledging",
      "warning",
      "len_active",
      "span_error"
    ))) |>
    unique() # not dplyr::distinct(), which drops the row names returned
  #res<-data.frame(sp=res$species_code,id=res$nest_id,region=res$region,initiation=as.Date(rep(NA,nrow(res))),fledging=as.Date(rep(NA,nrow(res))),warning=rep(NA,nrow(res)),stringsAsFactors=FALSE)
  res <- tibble::tibble(
    sp = respre$species_code,
    id = respre$nest_id,
    region = respre$region
  )
  res2 <- matrix(NA, nrow = nrow(res), ncol = 5)
  by_nest <- split(d, d$nest_id)
  error_id <- NULL
  assign("error_id", error_id, envir = .GlobalEnv)
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
      x <- by_nest[[as.character(res$id[i])]]

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
      }
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
  assign("error_id", error_id, envir = .GlobalEnv)
  # back to a plain data.frame, which active.nest() returns
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
