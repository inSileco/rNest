sub_list <-
  function(x, l) {
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
