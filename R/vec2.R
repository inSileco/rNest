vec2 <-
  function(vec, val) {
    if (all(vec < 0)) {
      return(c(floor(val / 2), ceiling(val / 2)))
    }
    if (vec[1] == 0 & vec[2] < 0) {
      return(c(0, val))
    }
    if (vec[1] == -1 & vec[2] == 0) {
      return(c(val, 0))
    }
    if (vec[1] > 0 & vec[2] < 0) {
      return(c(vec[1], ifelse(vec[1] >= val, 1, val - vec[1])))
    }
    if (vec[1] < 0 & vec[2] > 0) {
      return(c(ifelse(vec[2] >= val, 1, val - vec[2]), vec[2]))
    }
    if (all(vec >= 0)) {
      return(vec)
    }
  }
