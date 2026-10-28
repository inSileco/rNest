tk.data <-
  function() {
    tt <- tktoplevel()
    tktitle(tt) <- "INSPECT DATA"

    header3 <- tklabel(tt, text = "INSPECT DATABASE:")
    tkconfigure(header3, foreground = "blue")

    nest <- tkentry(tt, width = 13, textvariable = tclVar(""))
    nestlab <- tklabel(tt, text = "nest id")

    loclab <- tkentry(tt, width = 13, textvariable = tclVar(""))
    loclablab <- tklabel(tt, text = "loc label")

    braw <- function() {
      x <- tclvalue(tkget(nest))
      ydata <- ddata[ddata$nest_id == x, ]
      yheader <- dheader[dheader$nest_id == x, ]
      ystatus <- dstatus[dstatus$nest_id == x, ]
      ans <- list(DATA = ydata, HEADER = yheader, STATUS = ystatus)
      cat("\n", "\n", "\n", "\n")
      print(ans)
      cat("\n", "\n")
    }

    bdata <- function() {
      x <- tclvalue(tkget(nest))
      ans <- d[d$nest_id == x, ]
      cat("\n", "\n", "\n", "\n")
      print(ans)
      cat("\n", "\n")
    }

    bget <- function() {
      x <- tclvalue(tkget(nest))
      if (!any(d$nest_id == x)) {
        stop("nest id not in database")
      }
      y <- d[d$nest_id == x, ]
      ans <- chronology(y, test = T)
      cat("\n", "\n", "\n", "\n")
      print(ans)
      if (length(ans) == 3) {
        cat("\n")
        print(y)
      }
    }
    #
    brawl <- function() {
      x <- tclvalue(tkget(loclab))
      yheader <- dheader[which(dheader$loc_label == x), ]
      if (nrow(yheader) == 0) {
        ydata <- ddata[0, ]
        ystatus <- dstatus[0, ]
      } else {
        ydata <- ddata[which(ddata$nest_id == yheader$nest_id[1]), ]
        ystatus <- dstatus[which(dstatus$nest_id == yheader$nest_id[1]), ]
      }
      ans <- list(DATA = ydata, HEADER = yheader, STATUS = ystatus)
      cat("\n", "\n", "\n", "\n")
      print(ans)
      cat("\n", "\n")
    }

    bdatal <- function() {
      x <- tclvalue(tkget(loclab))
      ans <- d[which(d$loc_label == x), ]
      cat("\n", "\n", "\n", "\n")
      print(ans)
      cat("\n", "\n")
    }

    bgetl <- function() {
      x <- tclvalue(tkget(loclab))
      y <- d[which(d$loc_label == x), ]
      if (nrow(d) == 0) {
        stop("loc label not in database")
      }
      ans <- chronology(y, test = T)
      cat("\n", "\n", "\n", "\n")
      print(ans)
      if (length(ans) == 3) {
        cat("\n")
        print(y)
      }
    }

    butraw <- tkbutton(tt, text = "raw data", command = braw, width = 8)
    butdata <- tkbutton(tt, text = "data", command = bdata, width = 8)
    butget <- tkbutton(tt, text = "estimate", command = bget, width = 8)

    butrawl <- tkbutton(tt, text = "raw data", command = brawl, width = 8)
    butdatal <- tkbutton(tt, text = "data", command = bdatal, width = 8)
    butgetl <- tkbutton(tt, text = "estimate", command = bgetl, width = 8)

    tkgrid(tklabel(tt, text = ""), row = 0, column = 0, columnspan = 2)
    tkgrid(header3, row = 1, columnspan = 4)
    tkgrid(tklabel(tt, text = ""), row = 2, column = 0, columnspan = 2)

    tkgrid(nestlab, row = 3, column = 0)
    tkgrid(nest, row = 4, column = 0)
    tkgrid(butraw, row = 5, column = 0)
    tkgrid(butdata, row = 6, column = 0)
    tkgrid(butget, row = 7, column = 0)

    tkgrid(loclablab, row = 3, column = 1)
    tkgrid(loclab, row = 4, column = 1)
    tkgrid(butrawl, row = 5, column = 1)
    tkgrid(butdatal, row = 6, column = 1)
    tkgrid(butgetl, row = 7, column = 1)

    tkgrid(tklabel(tt, text = ""), row = 8, column = 0, columnspan = 2)
  }
