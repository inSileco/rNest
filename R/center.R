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
