breakshapepath <-
  function(path) {
    x <- unlist(strsplit(path, "/"))
    if (length(x) == 1) {
      dsn <- getwd()
      layername <- x[1]
    } else {
      dsn <- paste0(x[1:(length(x) - 1)], collapse = "/")
      layername <- x[length(x)]
    }
    ext <- substr(layername, nchar(layername) - 3, nchar(layername) - 3)
    if (ext == ".") {
      layername <- substr(layername, 1, nchar(layername) - 4)
    }
    c(dsn, layername)
  }
