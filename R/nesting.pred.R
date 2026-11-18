# Builds the nesting calendar of the predicted nests: one column per
# ecodistrict and group of species, named "<ecodistrict>___<group>". The
# first row holds the number of nests, the next 365 the proportion of nests
# active on each day of the year.
#
# predictions: the predictions dataset; ecodistricts: ecodistrict numbers;
# groups: a named list of species codes; order.by: "eco" or "spgr" to order
# the columns by ecodistrict or by group of species, anything else to keep
# them in the order they are built.
nesting.pred <- function(predictions, ecodistricts, groups, order.by = "") {
  cal <- data.frame(
    date = c(
      "nb_nest",
      substr(
        seq.Date(as.Date("2001-01-01"), as.Date("2001-12-31"), by = 1)[
          1:365
        ],
        6,
        10
      )
    ),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(ecodistricts)) {
    xeco <- predictions[
      which(predictions[, "ecodistrict"] == ecodistricts[i]),
    ]
    for (j in seq_along(groups)) {
      x <- xeco[xeco$species_code %in% groups[[j]], ]
      if (nrow(x) == 0) {
        next
      }
      a1 <- x |> dplyr::filter(.data$type == "initiation") |> split_props()
      a2 <- x |> dplyr::filter(.data$type == "fledging") |> split_props()
      a <- as.data.frame(matrix(
        do.call("c", a1) - do.call("c", a2),
        nrow = 366
      ))
      a[1, ] <- sapply(a1, function(k) {
        k[1]
      })
      a <- as.list(a)
      names(a) <- names(a1)
      cal <- cbind(cal, temp = a, stringsAsFactors = FALSE)
      names(cal)[ncol(cal)] <- paste(
        ecodistricts[i],
        names(groups)[j],
        sep = "___"
      )
    }
  }
  if (ncol(cal) == 1) {
    return(cal)
  }
  if (order.by == "eco") {
    cal <- cal[, c(1, rev(2:ncol(cal)))]
  }
  if (order.by == "spgr") {
    lo <- strsplit(names(cal)[-1], "___")
    lo1 <- sapply(lo, function(i) {
      i[1]
    })
    lo2 <- sapply(lo, function(i) {
      i[2]
    })
    namescal2 <- names(cal)[-1]
    cal <- cal[, c(
      "date",
      rev(namescal2[order(match(lo2, names(groups)), match(lo1, ecodistricts))])
    )]
  }
  cal
}


# For each ecodistrict of the predicted nests x: the number of nests followed
# by the proportion of nests whose date is passed, for each day of the year.
split_props <- function(x) {
  split(x, x$ecodistrict) |>
    lapply(\(k) c(nrow(k), calendarprop(k, column = "jul")))
}
