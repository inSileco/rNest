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
