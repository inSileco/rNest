rep1 <-
  function(x, val) {
    t(apply(x, 1, vec2, val = val))
  }
