# About the rNest app

rNest ships a desktop app, built with tcltk, for building **bird nesting
calendars** for Canada. You give it species and a location, and it draws a
chart showing the times of year when nests are likely to be active.

It is meant to help people plan activities so they don't disturb nests
protected under the Migratory Birds Convention Act, as the disclaimer shown at
startup explains.

## Launching

```r
rNest()  # English interface
rNid()   # French interface
```

Both call `rNest_tcltk()` (`R/tcltk_app.R`), which:

1. shows the disclaimer in a message box;
2. loads the bundled datasets: `nestwatch`, `nestingparameters`,
   `birdgroupings`, `ecodistrictscsv`, `predictions` and `specodistricts`;
3. turns `ecodistrictscsv` into a `SpatialPolygonsDataFrame` of ecodistricts
   (NAD83 longlat);
4. opens the main window, `tk.nesting.calendar()`
   (`R/tcltk_app.R`).

## Main window

The window has three sections, **Species**, **Location** and **Calendar**,
and two modes you switch between with a large button.

### Observation mode

Works from real nest records in the bundled Project NestWatch data.

- **Species:** pick species codes such as `AMRO,WCSP`, or groups of species.
  Groups can be built from bird type (landbirds, waterfowl and so on),
  habitat, nest location (ground, cavity, cliff…), federal or provincial
  status, or about 47 taxonomic groups. You can also define up to six custom
  groups.
- **Location:** use the built-in ecodistricts, load your own shapefile (it
  must be in NAD83 longlat), or draw a polygon on a map. You can also zoom
  and limit the data to certain years.
- **What it computes:** `active.nest()` works out each nest's chronology,
  `nesting.activity()` turns that into the share of nests active on each
  date, and `nesting.calendar()` plots the calendar.
- **Options:** pool the results by species/group, year or region; set your
  own percentage breaks and colours; and set a minimum number of nests, below
  which a combination is drawn in one flat colour.

### Prediction mode

Uses model predictions per ecodistrict (the `predictions` data) rather than
raw observations.

- You pick ecodistricts on a map, optionally with a species range map behind
  them.
- You can leave out species whose model was "rejected".
- `nesting.calendar.pred()` draws a calendar with one row per
  ecodistrict × species/group combination, up to 50 rows.

### Saving results

In both modes you can type a name in a box to save the intermediate results
(the nest chronologies, or the raw calendar table) to your global
environment, so you can keep working with them in R.

## Menus

- **File:** load your own data (nest observations, nesting parameters, status
  codes, bird groups) and quit.
- **Test:**
  - *Random observations* (`tk.single.nest()`) estimates the chronology of one
    randomly chosen NestWatch nest.
  - *Fictitious observations* (`tk.single.nest.test()`) lets you enter
    made-up observations to check how chronologies are estimated.
- **Options → Retrocalculation:** a placeholder that prints "Not implemented
  yet".

## Caveats

NestWatch observations are concentrated in southern, inhabited regions of
Canada, and a few species are overrepresented. Calendars built in observation
mode can therefore be unreliable where data are sparse, or when using groups
of species. Prediction mode is the main way to fill those gaps.

## Code map: UI vs. non-UI

The non-UI code is the computation pipeline plus the two plotting functions.
The core is the four exported functions other than `rNest()`/`rNid()`; the
internal functions they call are listed below them. None of these files calls
tcltk, except for the error dialogs in `active.nest()`.

### Pipeline, from raw observations to a calendar

| Layer | Functions | Files |
|---|---|---|
| Chronology of a single nest | `chronology()` (exported) | `R/chronology.R` |
| ↳ helpers it calls | `stage_obs()`, `laying()`, `id_stage()`, `testchronology()`, `unknown_nbeggs()` | each in its own file in `R/` |
| ↳ internals in `zzz.R` | `center()`, `dispatch()`, `egg_stage_id()`, `filldate()`, `hatching()`, `span()`, `sub_list()` | `R/zzz.R` |
| Selecting nests and running chronologies | `active.nest()` (exported) | `R/active.nest.R` |
| ↳ helper | `breakshapepath()` | `R/zzz.R` |
| Share of nests active on each date | `nesting.activity()` (exported) | `R/nesting.activity.R` |
| Plotting the calendar | `nesting.calendar()` (exported), `nesting.calendar.pred()` | `R/nesting.calendar.R`, `R/nesting.calendar.pred.R` |
| Translation helper | `i18n()` | `R/i18n.R` |

### Grey zones

- **`active.nest()`** mixes computation and UI in two ways. It has about 16
  `if (fromtk) tkmessageBox(...)` branches for error messages, which aren't
  reached when it is called from the console with `fromtk = FALSE`. It also
  has an `int.choice`/`zoom` mode that lets you draw a region on a plot. To
  make it pure computation, have it always `stop()`/`warning()` and let the
  GUI catch those errors and show them.
- **`calendarprop()`** in `R/zzz.R` isn't UI itself, but only
  `tk.nesting.calendar()` calls it.
- **`nesting.calendar()` and `nesting.calendar.pred()`** take a `list_lang`
  argument meant for the GUI, so the GUI's i18n setup leaks into them.
- **Prediction mode's calendar table** is built by `nesting.pred()`
  (`R/nesting.pred.R`) from the `predictions` data, the chosen ecodistricts and
  groups of species; `app_calendarpred()` only reads the inputs and plots it.

### Pure UI

`rNest()`, `rNid()`, `rNest_tcltk()`, `tk.nesting.calendar()`,
`tk.single.nest()`, `tk.single.nest.test()`, `cat_brs()`, `tk_info()` and the
`app_*()` functions of the main window, which take its state, `app`, as first
argument.
`rNest_tcltk()` also builds the ecodistrict polygons, and that step could move
into a data-preparation function.

### Where to start

For non-UI fixes, start with `chronology()` and its helpers, which is where
the estimation logic lives. Then work on `active.nest()`,
`nesting.activity()` and the two calendar plotting functions.




> For the non-UI work you asked about earlier, the useful part of tk.single.nest.test.R is the code that builds dat and nparam. Moving that into a plain function would give you a way to write testthat cases for chronology() without opening the GUI.
