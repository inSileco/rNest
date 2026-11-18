# Names that R CMD check can't resolve: the datasets the app reads from the
# global environment (loaded by data() in rNest_tcltk()), the columns used in
# subset() and data.table expressions, and data.table's J().
utils::globalVariables(c(
  # datasets
  "birdgroupings",
  "ecodistrictscsv",
  "nestingparameters",
  "nestwatch",
  "predictions",
  "specodistricts",
  # columns
  "ecodistrict",
  "eggs",
  "fledging",
  "group",
  "Habitat1",
  "Habitat2",
  "Habitat3",
  "Habitat4",
  "initiation",
  "lat",
  "len_active",
  "lon",
  "MBCA_2011_Final",
  "model",
  "nest_id",
  "Nest_Type1",
  "Nest_Type2",
  "Nest_Type3",
  "span_error",
  "species_code",
  "Species_gr1",
  "Species_gr2",
  "Species_ID",
  "stage_code",
  "status_code",
  "visit_id",
  "young",
  # data.table
  "J"
))
