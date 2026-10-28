#!/usr/bin/env Rscript
# Replace every non-ASCII character in R source files by its \uxxxx escape so
# that the package sources are pure ASCII (see "Writing R Extensions", 1.6.4).
# Files that are not valid UTF-8 are assumed to be Windows-1252 (latin1).
## Usage: Rscript inst/escape_non_ascii.R [file_or_dir ...]   (default: R/)

escape_non_ascii <- function(path) {
  x <- readLines(path, encoding = "UTF-8", warn = FALSE)
  bad <- !validUTF8(x)
  x[bad] <- iconv(x[bad], from = "CP1252", to = "UTF-8")
  m <- gregexpr("[^\\x01-\\x7F]", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), stringi::stri_escape_unicode)
  writeLines(x, path, useBytes = TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  args <- "R"
}
files <- unlist(lapply(args, function(p) {
  if (dir.exists(p)) {
    list.files(p, "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
  } else {
    p
  }
}))
invisible(lapply(files, escape_non_ascii))
