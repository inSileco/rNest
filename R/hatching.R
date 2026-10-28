hatching <-
  function(x) {
    if (any(x[1] < x) && x[1] > (-1)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
