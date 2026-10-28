stage_obs <-
  function(
    dat,
    period,
    prog = prog,
    upperbound = NULL,
    unknown = FALSE,
    lsc = lsc
  ) {
    wyo <- which(dat$stage_code %in% c("hatch", "young", "fledge"))
    w <- if (period == "young") {
      wyo
    } else {
      which(dat$stage_code == period)
    }

    #browser()

    if (period == "incubation" && unknown) {
      return(list(period = c(period = NULL), beg_end = c(NULL)))
    }
    if (
      period %in%
        c("incubation", "eggs") &&
        !any(w) &&
        !(any(dat$stage_code == "incubation") && period == "eggs")
    ) {
      if (any(dat$stage_code == "hatch")) {
        ans <- dat$date[which(dat$stage_code == "hatch")[1]] - 1
        names(ans)[1:length(ans)] <- period
        return(list(period = ans, beg_end = "E"))
      }
    }
    if (!any(w) && period != "laying") {
      return(list(period = c(period = NULL), beg_end = c(NULL)))
    }
    if (period == "laying") {
      # the function always infer the initiation from laying
      ans <- laying(
        dat,
        prog = prog,
        result = T,
        upperbound = upperbound,
        lsc = lsc
      )
      return(ans)
    }
    if (period != "laying") {
      ans <- dat$date[w]
      names(ans)[1:length(ans)] <- period
      ma <- max(ans) + 1
      mi <- min(ans) - 1
      #to modifiy if the nb of eegs never goes over 1 for species that lay only 1 egg
      e <- if (any(dat$date == ma)) {
        "E"
      } else {
        "U"
      }
      b <- if (any(dat$date == mi)) {
        "B"
      } else {
        "U"
      }
      if (period == "young") {
        b <- if (dat$stage_code[w[1]] == "hatch") {
          "B"
        } else {
          "U"
        }
      }
      if (period == "young") {
        e <- if (dat$stage_code[w[length(w)]] == "fledge") {
          "E"
        } else {
          "U"
        }
      } #last fledge considered
      if (period %in% c("eggs", "incubation")) {
        if (any(wyo)) {
          if (dat$stage_code[wyo[1]] == "hatch") {
            e <- "E"
            ans <- unique(c(ans, dat$date[wyo[1]] - 1))
            names(ans)[1:length(ans)] <- period
          }
        }
      }

      #kept only first fledge in chronology on 8 november 2011 because of nest 1039866
      ans <- filldate(ans)
      return(list(
        period = ans,
        beg_end = if (length(ans) == 1) {
          if (dat$stage_code[w[length(w)]] == "fledge") {
            "E"
          } else {
            min(c(e, b))
          }
        } else {
          c(b, rep("U", length(ans) - 2), e)
        }
      ))
    }
  }
