testchronology <- function(info, data, active, layingdetect = layingdetect) {
  if (all(is.na(active[1:2]))) {
    return(active)
  }
  add <- data.frame(
    STAGE = names(active),
    DATE = as.character(active),
    stringsAsFactors = FALSE
  )
  dattest <- data[[2]]
  dattest$date <- as.character(dattest$date) # dates in active can be represented by fractions so the same date can be represented by 2 different numbers so ar not equal so conversion to its character representation
  dat <- data[[1]]
  dat$date <- as.character(dat$date)
  temp <- merge(dattest, add, by.x = "date", by.y = "DATE", all = TRUE)
  res2 <- merge(dat, add, by.x = "date", by.y = "DATE", all = TRUE)
  res2 <- res2[, c(
    "species_code",
    "nest_id",
    "date",
    "eggs",
    "young",
    "stage_code",
    "STAGE"
  )]
  res2$stage_code <- ifelse(is.na(res2$stage_code), "-", res2$stage_code)
  names(res2) <- c(
    "sp_code",
    "nest_id",
    "date",
    "eggs",
    "young",
    "STAGE_OBS",
    "STAGE"
  )
  inf <- ifelse(
    temp$STAGE == "laying",
    info$laying[1],
    ifelse(
      temp$STAGE == "incubation",
      info$incub[1],
      ifelse(temp$STAGE == "young", info$rearing[1], NA)
    )
  )
  seqtemp <- c(
    rep(
      paste("B", info$start, sep = ""),
      sum(temp$STAGE == "laying", na.rm = TRUE)
    ),
    as.numeric(factor(temp$date[which(temp$STAGE == "incubation")])),
    as.numeric(factor(temp$date[which(temp$STAGE == "young")]))
  )
  seq <- rep(NA, nrow(temp))
  seq[!is.na(temp$STAGE)] <- seqtemp
  res <- cbind(temp, inf, seq)
  res <- res[, c(
    "species_code",
    "nest_id",
    "date",
    "eggs",
    "young",
    "stage_code",
    "STAGE",
    "inf",
    "seq"
  )]
  res$nest_id <- ifelse(is.na(res$nest_id), "-", res$nest_id)
  res$species_code <- ifelse(is.na(res$species_code), "-", res$species_code)
  res$eggs <- ifelse(is.na(res$eggs), "", res$eggs)
  res$young <- ifelse(is.na(res$young), "", res$young)
  res$stage_code <- ifelse(is.na(res$stage_code), "-", res$stage_code)
  names(res) <- c(
    "sp_code",
    "nest_id",
    "date",
    "eggs",
    "young",
    "STAGE_OBS",
    "STAGE",
    "nest_param",
    "NB_DAYS"
  )
  rownames(res) <- paste(
    rownames(res),
    ifelse(res$sp_code == "TEST", "obs", ""),
    sep = ""
  )

  ### computing the error
  #elim<-!((duplicated(res$date) | duplicated(res$date,fromLast=TRUE))& res$STAGE_OBS=="eggs")
  #res2<-res[!duplicated(res$date),] #to get rid of duplicated eggs observations, here it assumes the duplictaed observation is always to be deleted and eggs observation are always classified last
  w <- which(res$STAGE_OBS == "nest")
  if (any(w)) {
    #add nest observations which are deleted from dat to estimate chronology but used to estimate error here
    res2 <- rbind(res[w, names(res2)], res2)
  }
  obs <- res2[, "STAGE_OBS"]
  obs <- ifelse(obs == "unknown", "-", obs)
  stage <- res2[, "STAGE"]
  jul <- substr(
    seq.Date(as.Date("2008-01-01"), as.Date("2008-12-31"), by = 1),
    6,
    10
  )
  jul <- match(substr(res2[, "date"], 6, 10), jul)
  w <- which(obs == "nest")
  lnest <- ifelse(any(w), jul[max(w)], NA)
  maxerror <- 999
  if (!any(which(stage == "incubation"))) {
    # for nest 1733 without incubation period
    res$len_active <- ifelse(
      info$start > 900,
      info$clutch - (999 - info$start + 1),
      info$start - 1
    ) *
      info$laying +
      info$incub +
      info$rearing
    res$span_error <- NA
    return(res)
  }
  if (all(obs %in% c("-", "active"))) {
    x <- jul[which(obs == "active")]
    x <- max(x) - min(x) + 1
    r <- jul[range(which(!is.na(stage)))]
    maxerror <- r[2] - r[1] + 1 - x + 1
  } else {
    if (any(obs %in% c("laying", "hatch", "fledge")) || layingdetect) {
      maxerror <- 1
    } else {
      if (all(obs %in% c("-", "young"))) {
        x <- jul[which(obs == "young")]
        x <- max(x) - min(x) + 1
        maxerror <- info$rearing[1] - x + 1
      } else {
        if (all(obs %in% c("-", "incubation"))) {
          x <- jul[which(obs == "incubation")]
          x <- max(x) - min(x) + 1
          maxerror <- info$incub[1] - x + 1
        } else {
          if (all(obs %in% c("-", "eggs"))) {
            x <- jul[which(obs == "eggs")]
            x <- max(x) - min(x) + 1
            r <- jul[range(which(stage %in% c("laying", "incubation")))]
            maxerror <- r[2] - r[1] + 1 - x + 1
          } else {
            act <- which(obs == "active")
            nact <- which(!obs %in% c("active", "-"))
            if (any(act)) {
              w <- which(act > min(nact) & act < max(nact))
              if (any(w)) {
                obs[act[w]] <- "-"
              }
            }
            if (all(obs %in% c("-", "nest", "incubation", "young"))) {
              winc <- which(obs == "incubation")
              wyou <- which(obs == "young")
              if (any(winc)) {
                x1 <- jul[winc[1]] - jul[which(stage == "incubation")[1]]
              } else {
                x1 <- 999
              }
              if (any(wyou)) {
                x2 <- jul[wyou[1]] -
                  jul[max(which(stage == "incubation"))] -
                  1
              } else {
                x2 <- 999
              }
              forward <- min(x1, x2)
              if (any(wyou)) {
                x1 <- jul[max(which(stage == "young"))] - jul[max(wyou)]
              } else {
                x1 <- 999
              }
              if (any(winc)) {
                x2 <- jul[match("young", stage)] - jul[max(winc)] - 1
              } else {
                x2 <- 999
              }
              x3 <- ifelse(
                !is.na(lnest),
                jul[which(!is.na(stage))[1]] - lnest - 1,
                999
              )
              backward <- min(x1, x2, x3)
              maxerror <- forward + backward + 1
            }
            if (all(obs %in% c("-", "nest", "eggs", "young"))) {
              #
              wegg <- which(obs == "eggs")
              wyou <- which(obs == "young")
              if (any(wegg)) {
                x1 <- jul[wegg[1]] -
                  jul[which(stage %in% c("incubation", "laying"))[1]]
              } else {
                x1 <- 999
              }
              if (any(wyou)) {
                x2 <- jul[wyou[1]] -
                  jul[max(which(stage %in% c("incubation", "laying")))] -
                  1
              } else {
                x2 <- 999
              }
              forward <- min(x1, x2)
              if (any(wyou)) {
                x1 <- jul[max(which(stage == "young"))] - jul[max(wyou)]
              } else {
                x1 <- 999
              }
              if (any(wegg)) {
                x2 <- jul[match("young", stage)] - jul[max(wegg)] - 1
              } else {
                x2 <- 999
              }
              x3 <- ifelse(
                !is.na(lnest),
                jul[which(!is.na(stage))[1]] - lnest - 1,
                999
              )
              backward <- min(x1, x2, x3)
              maxerror <- forward + backward + 1
              #
            }
            if (all(obs %in% c("-", "nest", "incubation", "active"))) {
              winc <- which(obs == "incubation")
              wact <- which(obs == "active")
              if (any(winc)) {
                x1 <- jul[winc[1]] - jul[which(stage %in% c("incubation"))[1]]
              } else {
                x1 <- 999
              }
              x2 <- 999
              forward <- min(x1, x2)
              if (any(wact)) {
                x1 <- jul[max(which(stage == "young"))] - jul[max(wact)]
              } else {
                x1 <- 999
              }
              if (any(winc)) {
                x2 <- jul[match("young", stage)] - jul[max(winc)] - 1
              } else {
                x2 <- 999
              }
              x3 <- ifelse(
                !is.na(lnest),
                jul[which(!is.na(stage))[1]] - lnest - 1,
                999
              )
              backward <- min(x1, x2, x3)
              maxerror <- forward + backward + 1
            }
            if (all(obs %in% c("-", "nest", "eggs", "active"))) {
              wegg <- which(obs == "eggs")
              wact <- which(obs == "active")
              if (any(wegg)) {
                x1 <- jul[wegg[1]] -
                  jul[which(stage %in% c("incubation", "laying"))[1]]
              } else {
                x1 <- 999
              }
              x2 <- 999
              forward <- min(x1, x2)
              if (any(wact)) {
                x1 <- jul[max(which(stage == "young"))] - jul[max(wact)]
              } else {
                x1 <- 999
              }
              if (any(wegg)) {
                x2 <- jul[match("young", stage)] - jul[max(wegg)] - 1
              } else {
                x2 <- 999
              }
              x3 <- ifelse(
                !is.na(lnest),
                jul[which(!is.na(stage))[1]] - lnest - 1,
                999
              )
              backward <- min(x1, x2, x3)
              maxerror <- forward + backward + 1
            }
            if (all(obs %in% c("-", "nest", "active", "young"))) {
              wact <- which(obs == "active")
              wyou <- which(obs == "young")
              if (any(wact)) {
                x1 <- jul[wact[1]] -
                  jul[which(stage %in% c("laying", "incubation"))[1]]
              } else {
                x1 <- 999
              }
              if (any(wyou)) {
                x2 <- jul[wyou[1]] -
                  jul[max(which(stage %in% c("laying", "incubation")))] -
                  1
              } else {
                x2 <- 999
              }
              forward <- min(x1, x2)
              if (any(wyou)) {
                x1 <- jul[max(which(stage == "young"))] - jul[max(wyou)]
              } else {
                x1 <- 999
              }
              #if(any(wact)){
              #	x2<-jul[match("young",stage)]-jul[max(wact)]-1
              #}else{
              x2 <- 999
              #}
              x3 <- ifelse(
                !is.na(lnest),
                jul[which(!is.na(stage))[1]] - lnest - 1,
                999
              )
              backward <- min(x1, x2, x3)
              maxerror <- forward + backward + 1
            }
            if (all(obs %in% c("-", "nest", "active"))) {
              wact <- which(obs == "active")
              x1 <- jul[wact[1]] - jul[which(!is.na(stage))[1]]
              forward <- x1
              x1 <- jul[max(which(!is.na(stage)))] - jul[max(wact)]
              x2 <- ifelse(
                !is.na(lnest),
                jul[which(!is.na(stage))[1]] - lnest - 1,
                999
              )
              backward <- min(x1, x2)
              maxerror <- forward + backward + 1
            }
          }
        }
      }
    }
  }
  ### calculate theoretical active sequence with mean clutch size
  res$len_active <- ifelse(
    info$start > 900,
    info$clutch - (999 - info$start + 1),
    info$start - 1
  ) *
    info$laying +
    info$incub +
    info$rearing
  ###
  if (maxerror <= 0) {
    maxerror <- 1
  }
  res$span_error <- maxerror
  res
}
