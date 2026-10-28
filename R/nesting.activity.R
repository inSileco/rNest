nesting.activity <-
  function(
    dat,
    dates = FALSE,
    groups = FALSE,
    by = "sp",
    n = TRUE,
    chronology = FALSE,
    ...
  ) {
    if (chronology) {
      res <- active.nest(dat, ...)
    } else {
      res <- dat
    }
    res <- res[
      !(is.na(res$initiation) | is.na(res$fledging) | !is.na(res$warning)),
    ]
    res <- res[substr(res$initiation, 1, 4) == substr(res$fledging, 1, 4), ] #ici j'enl\u00e8ve ce qui est \u00e0 cheval sur le changement d'ann\u00e9e
    res$initiation <- as.Date(res$initiation)
    res$fledging <- as.Date(res$fledging)
    if (is.logical(dates)) {
      temp <- substr(c(res$initiation, res$fledging), 6, 10)
      date <- c(min(temp), max(temp))
    } else {
      date <- dates
    }
    if (!is.logical(groups)) {
      ll <- unlist(groups, use.names = FALSE)
      w <- duplicated(ll)
      if (any(w)) {
        warning(
          "The following species are part of more than one group: ",
          paste(unique(ll[w]), collapse = ", "),
          ". The first occurence was selected",
          immediate. = TRUE
        )
      }
      l <- lapply(groups, function(i) {
        which(res$sp %in% i)
      })
      for (i in 1:length(l)) {
        if (!any(l[[i]])) {
          next
        }
        res$sp[l[[i]]] <- names(l)[i]
      }
    }
    date <- as.Date(paste("2008-", date, sep = ""))
    date <- substr(seq.Date(date[1], date[2], by = 1), 6, 10)

    beg <- substr(res$initiation, 6, 10)
    end <- substr(res$fledging, 6, 10)
    YO <- substr(res$initiation, 1, 4) != substr(res$fledging, 1, 4) # to verify if the nesting period covers more than one year
    if (any(YO)) {
      res <- cbind(res, YO, stringsAsFactors = FALSE)
    }
    res <- cbind(res, beg, end, stringsAsFactors = FALSE)
    y <- if (by == "year") {
      substr(res[, "initiation"], 1, 4)
    } else {
      res[, by]
    }
    #watch out when a nest is active on different years (overlapping january first) adjustement needed
    calendar <- as.data.frame(
      matrix(NA, nrow = length(date), ncol = length(unique(y)) + 1),
      stringsAsFactors = FALSE
    )
    names(calendar) <- c("date", unique(y))
    calendar$date <- date
    nb_nest <- NULL
    for (i in 2:ncol(calendar)) {
      x <- res[y == names(calendar)[i], ]
      nb_nest <- c(nb_nest, nrow(x))
      calendar[, i] <- sapply(date, function(k) {
        temp <- x$beg <= k & x$end >= k
        if (any(x$YO)) {
          temp[which(x$YO)] <- apply(
            x[which(x$YO), c("initiation", "fledging")],
            1,
            function(j) {
              dateseq <- seq.Date(as.Date(j[1]), as.Date(j[2]), by = 1)
              k %in% substr(dateseq, 6, 10)
            }
          )
        }
        #round(length(which(temp))/nrow(x),3)
        length(which(temp)) / nrow(x)
      })
      progress(i - 1, ncol(calendar) - 1)
    }
    if (n) {
      #nest<-data.frame("nb_nest",nb_nest)
      nest <- calendar[1, ]
      nest[1, 1] <- "nb_nest"
      nest[1, 2:ncol(nest)] <- nb_nest
      names(nest) <- names(calendar)
      calendar <- rbind(nest, calendar)
    }
    if (by == "year") {
      calendar <- calendar[, order(names(calendar), decreasing = T)]
    }
    calendar
  }
