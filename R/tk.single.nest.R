tk.single.nest <-
  function(list_lang) {
    tclServiceMode(FALSE)

    list_lang <- list_lang
    rnest <- new.env()
    assign("list_lang", list_lang, envir = rnest)
    for (i in seq_along(list_lang)) {
      assign(names(list_lang)[i], list_lang[[i]], envir = rnest)
    }

    tt <- tktoplevel()
    tktitle(tt) <- get("titlesnlabl", envir = rnest)
    header1 <- tklabel(tt, text = get("header1snlabl", envir = rnest))
    tkconfigure(header1, foreground = "blue")
    header2 <- tklabel(tt, text = get("header2snlabl", envir = rnest))
    header22 <- tklabel(tt, text = get("header22snlabl", envir = rnest))
    titre1 <- tklabel(tt, text = get("titre1snlabl", envir = rnest))
    titre2 <- tklabel(tt, text = get("titre2snlabl", envir = rnest))
    titre3 <- tklabel(tt, text = get("titre3snlabl", envir = rnest))
    valeur1 <- tkentry(tt, width = 10, textvariable = tclVar(""))
    valeur2 <- tkentry(tt, width = 10, textvariable = tclVar(""))
    valeur3 <- tkentry(tt, width = 10, textvariable = tclVar(""))
    name1 <- tkentry(tt, width = 20, textvariable = tclVar(""))
    d <- nestwatch
    #separator<-"============================================================================"
    # add button to print raw data or not
    but1 <- function() {
      y <- d[d$nest_id == sample(unique(d$nest_id), 1), ]
      ans <- chronology(y, test = T)
      temp <- tclvalue(tkget(name1))
      if (temp != "") {
        assign(temp, ans, envir = .GlobalEnv)
      }
      cat_brs(4)
      print(ans)
      if (length(ans) == 3) {
        cat("\n")
        print(y)
      }
    }
    but2 <- function() {
      x <- tclvalue(tkget(valeur2))
      if (!any(d$species_code == x)) {
        stop(get("tkmb39", envir = rnest))
      }
      y <- d[d$nest_id == sample(unique(d$nest_id[d$species_code == x]), 1), ]
      ans <- chronology(y, test = T)
      temp <- tclvalue(tkget(name1))
      if (temp != "") {
        assign(temp, ans, envir = .GlobalEnv)
      }
      #print(y)
      cat_brs(4)
      print(ans)
      if (length(ans) == 3) {
        cat("\n")
        print(y)
      }
    }
    but3 <- function() {
      x <- tclvalue(tkget(valeur3))
      if (!any(d$nest_id == x)) {
        stop(get("tkmb40", envir = rnest))
      }
      y <- d[d$nest_id == x, ]
      ans <- chronology(y, test = T)
      temp <- tclvalue(tkget(name1))
      if (temp != "") {
        assign(temp, ans, envir = .GlobalEnv)
      }
      #print(y)
      cat_brs(4)
      print(ans)
      if (length(ans) == 3) {
        cat_brs()
        print(y)
      }
    }
    bouton1 <- tkbutton(tt, text = get("bouton", envir = rnest), command = but1)
    bouton2 <- tkbutton(tt, text = get("bouton", envir = rnest), command = but2)
    bouton3 <- tkbutton(tt, text = get("bouton", envir = rnest), command = but3)
    boutonc <- tkbutton(
      tt,
      text = get("boutonc", envir = rnest),
      command = function() {
        tkdestroy(tt)
      }
    )
    #tkwm.geometry(tt, "245x300")
    tkwm.resizable(tt, FALSE, FALSE)
    tkgrid(tklabel(tt, text = ""), row = 0, column = 0, columnspan = 2)
    tkgrid(header1, row = 1, columnspan = 4)
    tkgrid(tklabel(tt, text = ""), row = 2, column = 0, columnspan = 2)
    tkgrid(titre1, valeur1)
    tkgrid(titre2, valeur2)
    tkgrid(titre3, valeur3)
    tkgrid(bouton1, row = 3, column = 2, columnspan = 2)
    tkgrid(bouton2, row = 4, column = 2, columnspan = 2)
    tkgrid(bouton3, row = 5, column = 2, columnspan = 2)
    tkgrid(tklabel(tt, text = ""), columnspan = 2)
    tkgrid(header2, columnspan = 4)
    tkgrid(header22, columnspan = 4)
    tkgrid(tklabel(tt, text = ""), columnspan = 2)
    tkgrid(name1, columnspan = 4)
    tkgrid(tklabel(tt, text = ""), columnspan = 2)
    tkgrid(boutonc, columnspan = 4)
    tkgrid(tklabel(tt, text = ""), columnspan = 2)
    tclServiceMode(TRUE)
  }
