breakshapepath <- function(path) {
  x <- unlist(strsplit(path, "/"))
  if (length(x) == 1) {
    dsn <- getwd()
    layername <- x[1]
  } else {
    dsn <- paste0(x[1:(length(x) - 1)], collapse = "/")
    layername <- x[length(x)]
  }
  ext <- substr(layername, nchar(layername) - 3, nchar(layername) - 3)
  if (ext == ".") {
    layername <- substr(layername, 1, nchar(layername) - 4)
  }
  c(dsn, layername)
}


# Reads a layer of polygons as an sf object, with text columns as factors,
# as rgdal::readOGR() did on R 3.2.
read_layer <- function(dsn, layer) {
  sf::st_read(dsn, layer, stringsAsFactors = TRUE, quiet = TRUE)
}


# Returns a layer as an sf object: a path to a shapefile is read, an sp object
# (Spatial*DataFrame) is converted, and an sf object is returned as is.
as_layer <- function(layer) {
  if (is.character(layer)) {
    arg <- breakshapepath(layer)
    layer <- read_layer(dsn = arg[1], layer = arg[2])
  }
  if (inherits(layer, "Spatial")) {
    layer <- sf::st_as_sf(layer)
  }
  if (!inherits(layer, "sf")) {
    stop("`layer` must be a path to a shapefile or an sf object.")
  }
  layer
}


# Index of the first polygon of `polys` that contains each point of `pts`
# (NA if none), as sp::over() did. Planar geometry is used (no s2), as sp did
# for coordinates in longitude/latitude.
first_hit <- function(pts, polys) {
  s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2)))
  hit <- suppressMessages(sf::st_intersects(pts, sf::st_geometry(polys)))
  vapply(hit, function(i) if (length(i)) i[1L] else NA_integer_, integer(1))
}


# Points (sf) from x and y coordinates, in the coordinate system of `layer`.
layer_points <- function(x, y, layer) {
  sf::st_sfc(
    lapply(seq_along(x), function(i) sf::st_point(c(x[i], y[i]))),
    crs = sf::st_crs(layer)
  )
}


# Polygon (sfc) from a 2-column matrix of vertices, such as the one
# draw_poly() returns; the ring is closed.
poly_from_xy <- function(m, crs = sf::NA_crs_) {
  m <- as.matrix(m[, 1:2])
  sf::st_sfc(sf::st_polygon(list(m[c(seq_len(nrow(m)), 1), ])), crs = crs)
}


# Plots the geometry of a layer (sf or sfc), without its attributes.
plot_layer <- function(x, ...) {
  plot(sf::st_geometry(x), ...)
}


# Opens a plot device for a calendar. The requested size can be hundreds of
# inches; windows() clamped such a window to the screen, so the width and the
# height are each capped, at max_width and max_height inches.
calendar_device <- function(width, height, max_width = 16, max_height = 10) {
  dev.new(width = min(width, max_width), height = min(height, max_height))
}


calendarprop <- function(x, jul = 1:365, column) {
  dates <- x[, column] # change quantile !!!
  r <- range(dates, na.rm = TRUE)
  sapply(jul, function(i) {
    sum(dates <= i, na.rm = TRUE) / sum(!is.na(dates))
  })
}


center <- function(iobs, robs, itheo, rtheo) {
  iout <- itheo - length(iobs)
  rout <- rtheo - length(robs)
  if (iout == 0) {
    return(as.Date(min(iobs), origin = "1970-01-01"))
  }
  if (rout == 0) {
    return(as.Date(min(robs) - itheo, origin = "1970-01-01"))
  }
  obs <- c(iobs, robs)
  temp <- seq.Date(max(robs) - itheo - rtheo + 1, max(robs), by = 1)
  names(temp)[1:itheo] <- names(iobs)[1]
  names(temp)[(itheo + 1):(itheo + rtheo)] <- names(robs)[1]
  if (temp[1] == iobs[1]) {
    return(temp[1])
  }
  dpourc <- date1 <- NULL

  while (temp[1] <= iobs[1]) {
    m <- match(obs, temp)
    if (any(is.na(m)) || !all(names(obs) == names(temp)[m], na.rm = TRUE)) {
      dpourc <- c(dpourc, NA)
      date1 <- c(date1, NA)
      temp <- temp + 1
    } else {
      pourci <- abs(as.numeric(difftime(min(temp), min(iobs)))) / iout
      pourcr <- abs(as.numeric(difftime(max(temp), max(robs)))) / rout
      diffpourc <- abs(pourci - pourcr)
      dpourc <- c(dpourc, diffpourc)
      date1 <- c(date1, temp[1])
      temp <- temp + 1
    }
  }
  w <- which.min(dpourc) #the first date is taken in case of a tie which makes the active period earlier

  as.Date(date1[[w]], origin = "1970-01-01")
}


dispatch <- function(
  inct = 10,
  reart = 15,
  incmin = 12,
  rearmin = 6,
  increar = 32,
  rel.flex = c(0, 0)
) {
  r <- (increar - incmin):rearmin
  i <- incmin:(incmin + length(r) - 1)
  m <- matrix(c(i, r), ncol = 2)
  if (is.logical(rel.flex)) {
    dif <- apply(m, 1, function(x) {
      abs(x[1] / increar - (inct / (inct + reart)))
    })
  } else {
    if (increar > inct + reart) {
      dif <- apply(m, 1, function(x) {
        abs(
          x[1] /
            increar -
            ((inct + (rel.flex[1] * (increar - (inct + reart)))) / increar)
        )
      })
    } else {
      dif <- apply(m, 1, function(x) {
        abs(
          x[1] /
            increar -
            ((inct + (rel.flex[2] * (increar - (inct + reart)))) / increar)
        )
      })
    }
  }
  return(m[which.min(dif), ])
}


egg_stage_id <- function(x, beg) {
  l <- which(x < beg)
  if (!any(l)) {
    return(rep("incubation", length(x)))
  }
  wi <- which(x >= beg)[1]
  if (is.na(wi)) {
    return(rep("laying", length(x)))
  } #this line was added because of nest_id 54946, verify if warranted
  res <- rep(NA, length(x))
  res[wi:length(res)] <- "incubation"
  if (wi > 1) {
    res[is.na(res)] <- "laying"
  }
  return(res)
}


filldate <- function(x) {
  y <- as.Date(x)
  ans <- seq.Date(min(y, na.rm = TRUE), max(y, na.rm = TRUE), by = 1)
  if (!is.null(names(y))) {
    if (length(unique(names(y))) == 1) {
      names(ans)[1:length(ans)] <- names(y)[1]
    } else {
      names(ans)[1:length(ans)] <- sapply(ans, function(i) {
        w <- which(x == i)
        if (any(w)) {
          names(x)[w]
        } else {
          NA
        }
      })
    }
  }
  return(ans)
}


hatching <- function(x) any(x[1] < x) && x[1] > (-1)


span <- function(x, y) {
  if (!length(x)) {
    return(0)
  } else {
    stopifnot(inherits(x, "Date"))
    if (missing(y)) {
      r <- range(x, na.rm = TRUE)
    } else {
      stopifnot(inherits(y, "Date"))
      r <- range(c(x, y), na.rm = TRUE)
    }
    abs(as.numeric(difftime(r[1], r[2]))) + 1
  }
}

sub_list <- function(x, l) {
  if (is.null(names(l))) {
    stop("Not a named list")
  }
  namesl <- rep(names(l), sapply(l, length))
  l <- unlist(l, use.names = FALSE)
  if (any(duplicated(l))) {
    stop("Some codes used more than once")
  }
  names(l) <- namesl
  m <- match(x, l)
  names(l)[m]
}


# Builds the layer of ecodistricts (sf, NAD83 longitude/latitude) from the
# rings stored in ecodistrictscsv: one feature per id, made of the rings of its
# groups, each hole being attached to the outer ring that contains it.
build_ecodistricts <- function(csv) {
  s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2)))
  ring <- function(r) {
    m <- as.matrix(r[, 1:2])
    dimnames(m) <- NULL
    if (any(m[1, ] != m[nrow(m), ])) {
      m <- rbind(m, m[1, ])
    }
    m
  }
  ids <- unique(csv$id)
  rings <- split(csv, factor(csv$group, levels = unique(csv$group)))
  ring_id <- vapply(rings, function(r) r$id[1], "")
  geoms <- lapply(split(rings, factor(ring_id, levels = ids)), function(rs) {
    hole <- vapply(rs, function(r) r$hole[1], NA)
    outer <- lapply(rs[!hole], function(r) list(ring(r)))
    if (any(hole)) {
      outer_sfc <- sf::st_sfc(lapply(outer, sf::st_polygon))
      for (r in rs[hole]) {
        h <- ring(r)
        w <- suppressMessages(
          sf::st_intersects(sf::st_point(h[1, ]), outer_sfc)
        )[[1]]
        if (length(w)) outer[[w[1]]] <- c(outer[[w[1]]], list(h))
      }
    }
    sf::st_multipolygon(outer)
  })
  dat <- csv[match(ids, csv$id), ]
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
  sf::st_sf(dat, geometry = sf::st_sfc(unname(geoms), crs = 4269))
}


# Points at which to label the polygons of a layer (2-column matrix).
label_points <- function(x) {
  s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2)))
  suppressWarnings(sf::st_coordinates(
    sf::st_point_on_surface(sf::st_geometry(x))
  ))[, 1:2, drop = FALSE]
}


# Indices of the polygons of `polys` that intersect the geometry `g`.
intersecting <- function(g, polys) {
  s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2)))
  suppressMessages(sf::st_intersects(g, sf::st_geometry(polys)))[[1]]
}


# Lets the user draw a polygon on the current plot by clicking its vertices
# (finish with the right button); the polygon is closed on the plot. Returns
# the vertices as a 2-column matrix (no rows if none were given), as
# splancs::getpoly(quiet = TRUE) did.
draw_poly <- function() {
  p <- locator(type = "l")
  if (is.null(p)) {
    return(matrix(numeric(0), ncol = 2))
  }
  lines(p$x[c(1, length(p$x))], p$y[c(1, length(p$y))])
  cbind(p$x, p$y)
}


# Whether the points (x, y) are inside the polygon given by the 2-column
# matrix of its vertices (closed or not), with the nonzero winding rule, as
# splancs::inout() did. Works with self-intersecting polygons. Points exactly
# on an edge may be classified differently than by splancs.
in_polygon <- function(x, y, poly) {
  wn <- integer(length(x))
  n <- NROW(poly)
  if (n < 3) {
    return(wn != 0)
  }
  px <- poly[, 1]
  py <- poly[, 2]
  j <- n
  for (i in seq_len(n)) {
    # > 0 when (x, y) is left of the edge from vertex j to vertex i
    side <- (px[i] - px[j]) * (y - py[j]) - (x - px[j]) * (py[i] - py[j])
    wn <- wn +
      (py[j] <= y & py[i] > y & side > 0) -
      (py[j] > y & py[i] <= y & side < 0)
    j <- i
  }
  wn != 0
}
