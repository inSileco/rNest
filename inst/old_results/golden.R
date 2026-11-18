# golden_r320.rds was recorded with R 3.2.0 (rocker/r-ver:3.2.0), from commit
# 4292255 (rNest v0.2.2).
#
# Records rNest outputs, to compare R versions.
# Usage (needs an X display): Rscript golden.R <out.rds>
out <- commandArgs(TRUE)[1]
suppressMessages(library(rNest))
library(tcltk)
library(sp)
cat("R", R.version.string, "\n")
ns <- asNamespace("rNest")
try_ <- function(expr) tryCatch(expr, error = function(e) structure(conditionMessage(e), class = "golden_error"))
res <- list()

# message boxes would wait for a click: log them and answer "yes"
MSGS <- character()
tkmessageBox <- function(...) { MSGS <<- c(MSGS, as.character(list(...)$message)[1]); tclVar("yes") }
# once tcltk is imported (not attached), rNest finds it in its imports first
imp <- parent.env(ns)
if (exists("tkmessageBox", envir = imp, inherits = FALSE)) {
  unlockBinding("tkmessageBox", imp)
  assign("tkmessageBox", tkmessageBox, envir = imp)
}
cap <- list()
suppressMessages({
  trace("tk.nesting.calendar", exit = quote(assign("APP", environment(), globalenv())), where = ns, print = FALSE)
  trace("nesting.calendar.pred", quote(cap[[length(cap) + 1]] <<- list(kind = "pred", d = d, color.scale = color.scale, prop = prop, color = color, dates = dates)), where = ns, print = FALSE)
  trace("nesting.calendar", quote(cap[[length(cap) + 1]] <<- list(kind = "obs", d = d, prop = prop, color = color, dates = dates, type = type, cons = cons)), where = ns, print = FALSE)
})

# the app, which also builds the ecodistricts layer
rNest:::rNest_tcltk("en")
tcl("update")
eco <- APP$ecodistricts
res$ecodistricts <- list(data = eco@data, n = length(eco), bbox = bbox(eco))

# a shapefile of a few ecodistricts, for the "read a layer from a file" cases
shpdir <- file.path(tempdir(), "shp"); dir.create(shpdir)
sub <- eco[eco@data$ecodistrict %in% c(565, 571, 429, 431), ]
if (requireNamespace("rgdal", quietly = TRUE)) {
  rgdal::writeOGR(sub, shpdir, "eco4", driver = "ESRI Shapefile")
} else {
  sf::st_write(sf::st_as_sf(sub), file.path(shpdir, "eco4.shp"), quiet = TRUE)
}
shp <- file.path(shpdir, "eco4.shp")

# functions
nw <- nestwatch; np <- nestingparameters
res$chronology <- lapply(unique(nw$nest_id), function(i) try_(chronology(nw[nw$nest_id == i, ], nparam = np)))
res$chronology_test <- try_(chronology(nw[nw$nest_id == 43359, ], nparam = np, test = TRUE))
an <- list(
  all = quote(active.nest(nw, nparam = np, progress = FALSE)),
  precalc = quote(active.nest(nw, nparam = np, useprecalc = TRUE, progress = FALSE)),
  sp = quote(active.nest(nw, sp = c("WAVI", "MAWR"), nparam = np, progress = FALSE)),
  year = quote(active.nest(nw, year = 1974, nparam = np, progress = FALSE)),
  layer = quote(active.nest(nw, layer = eco, nparam = np, progress = FALSE)),
  region = quote(active.nest(nw, layer = eco, region = c(565, 571), nparam = np, progress = FALSE)),
  id2 = quote(active.nest(nw, layer = eco, id = c("ecodistrict", "breeding_zone"), nparam = np, progress = FALSE)),
  file = quote(active.nest(nw, layer = shp, nparam = np, progress = FALSE)),
  file_region = quote(active.nest(nw, layer = shp, region = 565, nparam = np, progress = FALSE)),
  unknown_sp = quote(active.nest(nw, sp = "ZZZZ", nparam = np, progress = FALSE))
)
res$active.nest <- lapply(an, function(e) try_(suppressWarnings(eval(e))))
ok <- Filter(function(x) !inherits(x, "golden_error"), res$active.nest)
res$nesting.activity <- lapply(ok, function(a) list(
  default = try_(nesting.activity(a)),
  year = try_(nesting.activity(a, by = "year")),
  region = try_(nesting.activity(a, by = "region")),
  app = try_(nesting.activity(a, n = TRUE, dates = FALSE, by = "sp", chronology = FALSE))
))
groups <- list(a = c("COLO", "CAGO"), b = "YWAR")
res$nesting.pred <- lapply(c("", "eco", "spgr"), function(o) try_(rNest:::nesting.pred(predictions, c(429, 431), groups, o)))
res$species_groups <- rNest:::species_groups()

# the app: press its buttons
setentry <- function(w, v) { tcl(w, "delete", 0, "end"); tcl(w, "insert", 0, v) }
press <- function(w) tryCatch({ tcl(w, "invoke"); "ok" }, error = function(e) paste("ERROR", conditionMessage(e)))
log <- character(); step <- function(name, w) log <<- c(log, paste(name, press(w)))
MSGS <- character()
tclvalue(APP$spgr) <- "usp"; setentry(APP$species, "WAVI,MAWR")
setentry(APP$nameback, "nb_out"); setentry(APP$namerawcal, "raw_out")
step("run", APP$butrun)
step("calendar obs", APP$butcal)
nb1 <- if (exists("nb_out")) nb_out; raw1 <- if (exists("raw_out")) raw_out
# observation mode with a layer read from a file
setentry(APP$shp, shp); setentry(APP$nameback, "nb_shp")
step("run with shapefile", APP$butrun)
nb_shp1 <- if (exists("nb_shp")) nb_shp
setentry(APP$shp, "")
step("predict mode", APP$butobs)
setentry(APP$species, "COLO,CAGO,YWAR"); setentry(APP$ecodistrict, "429,431")
step("calendar pred", APP$butcal)
tclvalue(APP$ordercal) <- "spgr"; step("calendar pred spgr", APP$butcal)
tclvalue(APP$ordercal) <- "eco"; step("calendar pred eco", APP$butcal)
tclvalue(APP$uspgroup) <- "uallsp"; step("calendar pred all sp", APP$butcal)
setentry(APP$ecodistrict, ""); step("calendar pred no eco", APP$butcal)
step("obs mode", APP$butpred)
res$app <- list(cap = cap, msgs = MSGS, log = log, nb = nb1, raw = raw1, nb_shp = nb_shp1)

saveRDS(res, out, version = 2)
cat(log, sep = "\n")
cat("errors:", sum(sapply(unlist(res[c("chronology", "active.nest")], recursive = FALSE), inherits, "golden_error")),
  " calendars:", length(cap), " messages:", length(MSGS), "\n")
