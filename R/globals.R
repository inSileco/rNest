# Names that R CMD check can't resolve: the datasets the app reads from the
# global environment (loaded by data() in rNest_tcltk()). Columns are referred
# to with the .data pronoun or as strings, so they need no declaration.
utils::globalVariables(c(
  "birdgroupings",
  "ecodistrictscsv",
  "nestingparameters",
  "nestwatch",
  "predictions",
  "specodistricts"
))
