span <-
  function(x, y) {
    if (length(x) == 0) {
      return(0)
    }
    stopifnot(inherits(x, "Date"))
    if (missing(y)) {
      r <- range(x, na.rm = TRUE)
    } else {
      stopifnot(inherits(y, "Date"))
      r <- range(c(x, y), na.rm = TRUE)
    }
    ans <- abs(as.numeric(difftime(r[1], r[2]))) + 1
    ans
  }
