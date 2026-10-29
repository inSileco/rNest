nesting.calendar.pred <- function(
  d,
  lebox = 10,
  name = "test",
  sp = FALSE,
  cons = 10,
  colcons = gray(0.8),
  dates = FALSE,
  groups = FALSE,
  type = "Species",
  color.scale = TUE,
  prop = c(0.0000000000, 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, 1),
  color = c("white", brewer.pal(length(prop) - 3, "YlOrRd"), "darkred"),
  list_lang
) {
  list_lang <- list_lang
  rnest <- new.env()
  assign("list_lang", list_lang, envir = rnest)
  for (i in seq_along(list_lang)) {
    assign(names(list_lang)[i], list_lang[[i]], envir = rnest)
  }

  if (!is.logical(sp)) {
    d <- d[, names(d) %in% c(sp, "date")]
  }
  #if(is.logical(dates)){
  #	dates<-range(c(d$date[-1],d2$date[-1]))
  #	}
  if (!is.logical(dates)) {
    d <- d[(d$date >= dates[1] & d$date <= dates[2]) | d$date == "nb_nest", ]
  } #### harmoniser ceci avec le rajout plus bas
  #if((length(prop)-1)!=length(color)){warning("Too many colors specified")}
  color <- tail(color, length(prop) - 1)
  if ((length(prop) - 1) != length(color)) {
    stop(paste(
      get("tkmb38", envir = rnest)[1],
      length(prop) - 1,
      get("tkmb38", envir = rnest)[2]
    ))
  }
  if (!is.logical(dates)) {
    crap <- data.frame(
      date = substr(
        seq.Date(
          as.Date(paste("2001", dates[1], sep = "-")),
          as.Date(paste("2001", dates[2], sep = "-")),
          by = 1
        ),
        6,
        10
      ),
      stringsAsFactors = FALSE
    )
    d <- merge(d, crap, all = TRUE)
  }
  d <- d[c(nrow(d), 1:(nrow(d) - 1)), ]
  for (i in 2:ncol(d)) {
    d[, i] <- ifelse(is.na(d[, i]), 0, d[, i])
  }
  #d<-d[,c("date",sort(names(d)[-1]))]
  month <- get("monthlabl", envir = rnest)
  mnum <- sprintf("%02d", 1:12)
  x <- nrow(d)
  sx <- 1:x
  y <- ncol(d)
  sy <- y:2
  windows(
    length(sx) * 10,
    (length(sy) + 3.5 + 1 + ifelse(color.scale, 3, 0)) * 30
  )
  #windows(17,10)
  par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
  upbox <- 3.5
  lobox <- if (color.scale) {
    -3
  } else {
    0
  }
  lebox <- lebox * (-1)
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
  lebox <- (max(c(
    strwidth(c(type, get("leboxlabl", envir = rnest)), font = 2),
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
  #print((length(sy)+3.5+1+ifelse(color.scale,3,0))*30)
  #pdf("temp.pdf",length(sx)*10,(length(sy)+3.5+1+ifelse(color.scale,3,0))*30)
  #pdf("temp.pdf",17,10)
  #layout(matrix(c(rep(1,11*3),rep(c(2,3,4),5),rep(c(5,5,5),4)), 20, 3, byrow = TRUE))
  ## show the regions that have been allocated to each plot
  #layout.show(4)
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
  rect(
    xleft = 1,
    xright = sx,
    ytop = 1,
    ybottom = y,
    col = gray(0.8),
    border = gray(0.8)
  )

  for (i in sy) {
    if (i != sy[1]) {
      lines(c(lebox, x), c(i, i), col = gray(0.6), lwd = 1)
      #points(lebox,i)
    }
    text(
      (lebox + 1) / 2,
      i - 0.5,
      paste(names(d)[i], "  (", d[1, i], ") ", sep = ""),
      adj = c(0.5, 0.4),
      cex = 0.8,
      xpd = TRUERUE,
      font = 2
    )
  }

  for (i in sx[-length(sx)]) {
    dates <- substr(d$date[i], 4, 5)
    dates <- ifelse(dates %in% c("05", "10", "15", "20", "25"), dates, "")
    mo <- substr(d$date[i], 4, 5) == "01" || i %in% c(2, x)
    if (mo) {
      lines(
        c(i - 1, i - 1),
        c(y, y + upbox),
        col = "black",
        lwd = 2,
        xpd = TRUE
      )
      lines(c(i - 1, i - 1), c(1, y), col = "black", lwd = 1, xpd = TRUE)
    }
  }

  ### all
  for (j in sy) {
    for (i in sx[-length(sx)]) {
      val <- d[-1, j][i]
      #val<-val-val2
      #browser()
      colo <- if (val > 998) {
        "deepskyblue3"
      } else {
        if (val < 1e-13) {
          next
        } else {
          ma <- max(which(prop <= val))
          color[
            if (ma == length(prop)) {
              ma - 1
            } else {
              ma
            }
          ]
        }
      }
      #rect(xleft=i,xright=i+1,ytop=j-0.10,ybottom=j-0.5,col=colo,border=gray(0.8),lwd=1)
      rect(
        xleft = i,
        xright = i + 1,
        ytop = j - 0.1,
        ybottom = j - 0.9,
        col = colo,
        border = gray(0.8),
        lwd = 1
      )
    }
  }

  ###
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
        xpd = TRUE,
        adj = c(0.5, 0.5)
      )
    }
  }

  m <- unique(substr(d$date, 1, 2))
  for (i in 1:length(m)) {
    w <- which(substr(d$date, 1, 2) == m[i])
    diff <- max(w) - min(w)
    if (diff >= 13) {
      text(
        min(w) + diff / 2,
        y + (0.70 * upbox),
        month[which(mnum == m[i])],
        font = 2,
        adj = c(0.5, 0.5),
        cex = 0.9
      )
    }
  }
  text(
    (lebox + 1) / 2,
    (upbox * 0.85) + y,
    name,
    font = 2,
    adj = c(0.5, 0.5),
    cex = 0.9
  )
  text(
    (lebox + 1) / 2,
    (upbox * 0.525) + y,
    get("lebox2labl", envir = rnest),
    font = 2,
    adj = c(0.5, 0.5),
    cex = 0.9
  )
  text(
    (lebox + 1) / 2,
    (upbox * 0.20) + y,
    get("leboxlabl", envir = rnest),
    font = 2,
    adj = c(0.5, 0.5),
    cex = 0.9
  )

  lines(c(lebox, x), c(y, y), lwd = 1)
  lines(c(x, x), c(1, y), col = "black", lwd = 1)
  rect(
    xleft = lebox,
    xright = x + 1 - 0.85,
    ytop = y + upbox,
    ybottom = lobox,
    border = "black",
    lwd = 3,
    xpd = TRUE
  )
  lines(c(lebox, x + 1 - 0.85), c(1, 1), lwd = 1, xpd = TRUE)
  if (color.scale) {
    l <- ((x - (lebox)) * 0.15) / 2
    s <- seq(lebox + l - lebox, x - l, length.out = (length(color)) + 1)
    rect(
      xleft = s[1] - 1.5,
      xright = s[length(s)] + 1.5,
      ytop = 0.3 * lobox,
      ybottom = 0.7 * lobox,
      col = gray(0.8),
      border = gray(0.8)
    )
    for (i in 1:(length(s) - 1)) {
      rect(
        xleft = s[i],
        xright = s[i + 1],
        ytop = 0.35 * lobox,
        ybottom = 0.65 * lobox,
        col = color[i],
        border = gray(0.8)
      )
      prop2 <- prop * 100
      tt <- c(paste(prop2[-length(prop2)], prop2[-1], sep = " - "))
      tt[1] <- paste(">", tt[1], sep = "")
      text(median(c(s[i], s[i + 1])), lobox * (0.125), tt[i], cex = 0.8)
    }
  }
}
