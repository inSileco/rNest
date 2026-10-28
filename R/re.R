re <- function(x, n) {
  subset(
    x,
    nest_id %in% n,
    select = c(
      species_code,
      nest_id,
      visit_id,
      date,
      eggs,
      young,
      stage_code,
      status_code
    )
  )
}
