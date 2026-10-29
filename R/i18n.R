i18n <- function(section, lang = "english") {
  file <- system.file(
    "i18n",
    if (lang == "english") "en.yml" else "fr.yml",
    package = "rNest"
  )
  # old yaml versions do not mark the strings they read as UTF-8
  mark <- function(x) {
    if (is.list(x)) {
      x <- lapply(x, mark)
    }
    if (is.character(x)) {
      Encoding(x) <- "UTF-8"
    }
    if (!is.null(names(x))) {
      Encoding(names(x)) <- "UTF-8"
    }
    x
  }
  mark(yaml::yaml.load_file(file)[[section]])
}
