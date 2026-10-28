nesting.calendar <-
  function(
    d,
    sp = FALSE,
    cons = 10,
    colcons = "grey75",
    dates = FALSE,
    groups = FALSE,
    type = "Species",
    color.scale = TRUE,
    prop = c(0.0000000000, 0.01, 0.02, 0.1, 0.2, 0.5, 0.7, 1),
    color = c(
      rev(heat.colors(length(prop) - 3, alpha = 1)),
      "brown",
      "darkred"
    ),
    list_lang = NULL
  ) {
    #browser()

    list_lang <- list_lang
    rnest <- new.env()
    assign("list_lang", list_lang, envir = rnest)
    if (!is.null(list_lang)) {
      for (i in seq_along(list_lang)) {
        assign(names(list_lang)[i], list_lang[[i]], envir = rnest)
      }
    }

    if (!is.logical(sp)) {
      d <- d[, names(d) %in% c(sp, "date")]
    }
    if (!is.logical(dates)) {
      d <- d[(d$date >= dates[1] & d$date <= dates[2]) | d$date == "nb_nest", ]
    } #### harmoniser ceci avec le rajout plus bas
    #if((length(prop)-1)!=length(color)){warning("Too many colors specified")}
    color <- tail(color, length(prop) - 1)
    if ((length(prop) - 1) != length(color)) {
      stop(paste("Need", length(prop) - 1, "colors"))
    }
    if (!is.logical(dates)) {
      crap <- data.frame(
        date = substr(
          seq.Date(
            as.Date(paste("2008", dates[1], sep = "-")),
            as.Date(paste("2008", dates[2], sep = "-")),
            by = 1
          ),
          6,
          10
        ),
        stringsAsFactors = FALSE
      )
      d <- merge(d, crap, all = TRUE)
      d <- d[c(nrow(d), 1:(nrow(d) - 1)), ]
    }
    for (i in 2:ncol(d)) {
      d[, i] <- ifelse(is.na(d[, i]), 0, d[, i])
    }
    if (!is.null(list_lang)) {
      month <- get("monthlabl", envir = rnest)
    } else {
      month <- c(
        "january",
        "february",
        "march",
        "april",
        "may",
        "june",
        "july",
        "august",
        "september",
        "october",
        "november",
        "december"
      )
    }
    mnum <- sprintf("%02d", 1:12)
    x <- nrow(d)
    sx <- 1:x
    y <- ncol(d)
    sy <- y:2
    windows(
      length(sx) * 10,
      (length(sy) + 3.5 + 1 + ifelse(color.scale, 3, 0)) * 30
    )
    par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
    plot(x, y, type = "n")
    upbox <- 3.5
    lobox <- if (color.scale) {
      -3
    } else {
      0
    }
    lebox <- (max(c(
      strwidth(
        c(
          type,
          ifelse(
            !is.null(list_lang),
            get("leboxlabl", envir = rnest),
            "(nb nests)"
          )
        ),
        font = 2
      ),
      strwidth(paste(names(d)[-1], "  (", d[1, -1], ") ", sep = ""), cex = 0.7)
    )) +
      1) *
      (-1.5)
    dev.off()
    windows(
      length(sx) * 10,
      (length(sy) + 3.5 + 1 + ifelse(color.scale, 3, 0)) * 30
    )
    #browser()
    par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
    plot(
      x,
      y,
      type = "n",
      xlim = c(lebox, x),
      ylim = c(lobox, y + upbox),
      yaxt = "n",
      xaxt = "n",
      bty = "n",
      xlab = "",
      ylab = ""
    )
    for (j in sy) {
      for (i in sx[-length(sx)]) {
        val <- d[-1, j][i]
        colo <- if (d[1, j] < cons & val > 0.0000000000001) {
          colcons
        } else {
          color[
            if (val < 0.0000000000001) {
              "white"
            } else {
              ma <- max(which(prop <= val))
              if (ma == length(prop)) {
                ma - 1
              } else {
                ma
              }
            }
          ]
        }
        rect(
          xleft = i,
          xright = i + 1,
          ytop = j,
          ybottom = j - 1,
          col = colo,
          border = "black"
        )
      }
    }
    for (i in sx[-length(sx)]) {
      dates <- substr(d$date[i], 4, 5)
      dates <- ifelse(dates %in% c("05", "10", "15", "20", "25"), dates, "")
      mo <- substr(d$date[i], 4, 5) == "01"
      if (dates != "" && !i %in% c(2, 3, x - 1, x)) {
        lines(
          c(i - 0.5, i - 0.5),
          c(y, y + 0.4),
          col = "black",
          lwd = 2,
          xpd = TRUE
        )
        lines(
          c(i - 0.5, i - 0.5),
          c(1, 1 - 0.15),
          col = "black",
          lwd = 2,
          xpd = TRUE
        )
        text(
          i - 0.5,
          max(sy) + (0.285 * upbox),
          ifelse(dates %in% c("05", "10", "15", "20", "25", "30"), dates, ""),
          cex = 0.8,
          xpd = T,
          adj = c(0.5, 0.5)
        )
      }
    }
    for (i in sy) {
      if (i != sy[1]) {
        lines(c(lebox, x), c(i, i), col = "black", lwd = 1)
      }
      text(
        (lebox + 1) / 2,
        i - 0.5,
        paste(names(d)[i], "  (", d[1, i], ") ", sep = ""),
        adj = c(0.5, 0.4),
        cex = 0.7,
        xpd = T,
        font = 2
      )
    }
    for (i in sx[-length(sx)]) {
      dates <- substr(d$date[i], 4, 5)
      dates <- ifelse(dates %in% c("05", "10", "15", "20", "25"), dates, "")
      mo <- substr(d$date[i], 4, 5) == "01" || i %in% c(2, x)
      if (mo) {
        lines(c(i - 1, i - 1), c(y, y + upbox), col = "black", lwd = 2, xpd = T)
        lines(c(i - 1, i - 1), c(1, y), col = "black", lwd = 2, xpd = T)
      }
    }
    m <- unique(substr(d$date, 1, 2))
    for (i in 1:length(m)) {
      w <- which(substr(d$date, 1, 2) == m[i])
      diff <- max(w) - min(w)
      if (diff >= 13) {
        text(
          min(w) + diff / 2,
          y + (0.73 * upbox),
          month[which(mnum == m[i])],
          font = 2,
          adj = c(0.5, 0.5)
        )
      }
    }
    text((lebox + 1) / 2, (upbox * 0.7) + y, type, font = 2, adj = c(0.5, 0.5))
    text(
      (lebox + 1) / 2,
      (upbox * 0.3) + y,
      ifelse(
        !is.null(list_lang),
        get("leboxlabl", envir = rnest),
        "(nb nests)"
      ),
      font = 2,
      adj = c(0.5, 0.5)
    )
    lines(c(lebox, x), c(y, y), lwd = 1)
    lines(c(x, x), c(1, y), col = "black", lwd = 1)
    rect(
      xleft = lebox,
      xright = x + 1 - 0.85,
      ytop = y + upbox,
      ybottom = lobox,
      border = "black",
      lwd = 4,
      xpd = TRUE
    )
    lines(c(lebox, x + 1 - 0.85), c(1, 1), lwd = 1, xpd = TRUE)
    if (color.scale) {
      l <- ((x - 0.85 - (lebox)) * 0.3) / 2
      s <- seq(lebox + l, x - 0.85 - l, length.out = length(color) + 1)
      for (i in 1:(length(s) - 1)) {
        rect(
          xleft = s[i],
          xright = s[i + 1],
          ytop = 0.333 * lobox,
          ybottom = 0.666 * lobox,
          col = color[i],
          border = "black"
        )
        prop2 <- prop * 100
        text(
          median(c(s[i], s[i + 1])),
          lobox * 0.1,
          paste(
            if (i == 1) {
              ">"
            },
            prop2[i],
            " - ",
            prop2[i + 1],
            sep = ""
          ),
          cex = 0.8
        )
      }
    }
  }
