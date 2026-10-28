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
