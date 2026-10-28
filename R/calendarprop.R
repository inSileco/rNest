calendarprop <-
  function(x, jul = 1:365, column) {
    dates <- x[, column] # change quantile !!!
    r <- range(dates, na.rm = TRUE)
    p <- sapply(jul, function(i) {
      sum(dates <= i, na.rm = TRUE) / sum(!is.na(dates))
    })
    p
  }
