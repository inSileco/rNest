id_stage <-
  function(dat, nact = nact) {
    r <- c("1nest", "2eggs", "3hatch", "4young", "5fledge")
    if (all(dat$eggs == 0 & dat$young == 0)) {
      return(rep("nest", nrow(dat)))
    }
    st <- "1nest"
    for (i in 1:nrow(dat)) {
      if (dat$eggs[i] < 0 && dat$young[i] < 0) {
        ############ rajout
        st <- c(st, "0active")
        next
      }
      if (any(st == "6unknown")) {
        ############ rajout
        st <- c(st, rep("6unknown", nrow(dat) + 1 - length(st)))
        break
      }
      if (!is.na(dat$stage_code[i])) {
        if (dat$stage_code[i] == "fledge") {
          st <- c(st, "5fledge") #here used to be next right after this line
          st <- c(st, rep("6unknown", length(i:nrow(dat)) - 1))
          break
        }
        if (dat$stage_code[i] == "hatch") {
          if (any(st == "3hatch")) {
            st <- c(st, "4young")
          } else {
            st <- c(st, "3hatch")
          } #here used to be next right after this line
          next
        }
        if (
          !(dat$stage_code[i] == "nest" &
            (dat$eggs[i] != 0 | dat$young[i] != 0))
        ) {
          if (!max(st) == "1nest") {
            st <- c(st, max(r[grep(dat$stage_code[i], r)], st)) # need to eliminate visit where a lower status code (ex nest) is given despite a higher stage has been reache (see nest_id 22564)
            next
          }
        }
      }
      if (dat$eggs[i] != 0 && dat$young[i] == 0) {
        if (max(st) == "4young") {
          st <- c(st, max(c("6unknown", st)))
          next
        } else {
          st <- c(st, max(c("2eggs", st)))
          next
        }
      }
      if (dat$eggs[i] != 0 && dat$young[i] != 0) {
        st <- c(st, max(c("4young", st)))
        next
      }
      if (dat$eggs[i] == 0 && dat$young[i] != 0) {
        st <- c(st, max(c("4young", st)))
        next
      }
      if (dat$eggs[i] == 0 && dat$young[i] == 0) {
        if (max(st) == "1nest" && all(st != "0active")) {
          st <- c(st, max(c("1nest", st)))
          next
        } else {
          st <- c(st, max(c("6unknown", st)))
          next
        }
      }
    }
    #############

    if (nact && any(dat$eggs < 0 & dat$young < 0)) {
      wact <- which(dat$eggs < 0 & dat$young < 0 & st[-1] != "6unknown") + 1
      st[wact] <- "0active"
      wafty <- which(st %in% c("3hatch", "4young"))[1] #effacer les active qui surviennent apr\u00e8s les jeunes
      if (any(wact > wafty, na.rm = TRUE)) {
        st[wact[wact > wafty]] <- "4young"
      }
      wbefe <- which(st == "2eggs") #effacer les active qui surviennent dans le stage eggs ici problem si on a un status code fresh qui fait identifier une obs en laying avant de passer dans le egg stage id
      wbefe <- ifelse(length(wbefe) > 0, wbefe[length(wbefe)], NA)
      if (any(wact < wbefe, na.rm = TRUE)) {
        st[wact[wact < wbefe]] <- "2eggs"
      }

      wunk <- match("6unknown", st[-1]) + 1 # add this 2012-08-24 to
      if (!is.na(wunk)) {
        st[wunk:length(st)] <- rep("6unknown", length(wunk:length(st)))
      } # add this 2012-08-24
    }

    if (!any(st == "3hatch")) {
      wy <- which(st[-1] %in% c("4young", "5fledge"))
      if (any(wy)) {
        if (hatching(dat$young[wy])) {
          st[wy[1] + 1] <- "3hatch"
        }
      }
    }
    return(substr(st, 2, nchar(st))[-1])
  }
