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
