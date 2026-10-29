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


cat_brs <- function(n = 1) {
  cat(rep("\n", n))
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
