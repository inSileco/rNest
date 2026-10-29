\name{active.nest}
\alias{active.nest}
\title{
A Function to Infer Nesting Chronology from Nest Observations to Several Nests
}
\description{
Infers nest chronology from nest observations
}
\usage{
active.nest(d, useprecalc = FALSE, error = FALSE, nparam = NULL, sp = FALSE, year = FALSE, layer = FALSE, region = FALSE, id = FALSE,
int.choice = FALSE, locations = FALSE, progress = TRUE, lsc = NULL, spgroup = NULL, lang = "en", fromtk = FALSE ...)
}
\arguments{
  \item{d}{
A \code{data.frame} containing observations for one or several nests. An object of class \code{data.frame} with column names?: nest_id, species_code,
date, eggs, young and optionally status_code, lon and lat.
}
  \item{useprecalc}{
When used with the \code{nestwatch} database, precalculated data can be used to speed up the process whent set to \code{TRUE}.
}
  \item{error}{
When set to \code{TRUE}, nest ids generating an error (either due to erroneous data or a bug in Rnest) will be displayed.
}
  \item{nparam}{
A \code{data.frame} containing nesting parameters for the species. An object of class \code{data.frame} with column names?: species_code, cmin,
cmax, clutch, laying, start, incub and rearing.
}
  \item{sp}{
A set of species for which the nesting chronology has to be inferred. All species are included when the argument is set to \code{FALSE}.
Otherwise, a \code{vector} with the species code has to be given.
}
  \item{year}{
Used to subset nests to be included based on their date of initiation. All nests are included when the argument is set to \code{FALSE}.
Otherwise, either a \code{vector} containing dates (in character format) or years (in numeric/integer format) to be included. Years and
dates given can precede or exceed values observed in the database. When the nesting chronology of a nest encompasses two years and
years are given as an argument, it will be included if its date of initiation corresponds to a year given in the subset. If the
argument is a set of dates, nests for which either the initiation or the fledging date correspond to one of the dates given will
be included.
}
  \item{layer}{
Name of a layer (ESRI shapefile) used to restrict the subset of nests studied to polygons contained in the layer. All nests are
used when layer is set to \code{FALSE}, unless the argument \code{int.choice} is set to \code{TRUE}. Otherwise, a character string giving the name of
a layer (without the extension) containing the region of interest has to be given. The file has to be located in the R working
directory along with other necessary files to interpret the layer. The shapefile has to be in geographic coordinates with the
NAD83 system. Used in conjonction with \code{region} and \code{int.choice}.
}
  \item{region}{
Regions to which the nests should be restricted. The two possible values are \code{FALSE} or a \code{vector} of the names of regions of interest.
When \code{int.choice} is \code{TRUE}, the name of the first region will be given to the selected polygon and a warning will be given if more than
one region is specified. When \code{int.choice} is \code{FALSE}, a layer containing the names of the regions in its data slot has to be given,
otherwise, an error message will be returned.
}
  \item{id}{
Names of the columns in the attribute table of the shapefile to be used to create the regions of interest. A character \code{vector} with
the names of columns in the attribute table. When set to \code{FALSE}, the first column is used to describe the regions.
}
  \item{int.choice}{
Let's the user draw a polygon containing the region of interest. When set to \code{TRUE}, the user is asked to draw a polygon in the graphic
device. If a layer is specified, the polygon is drawn in a plot showing the layer. If layer is set to \code{FALSE}, a world map is plotted
and the user is asked for a polygon. The first 4 clicks on the graphic device will be used to rescale the map to get a greater accuracy
in the drawing of the polygon.
}
  \item{locations}{
Used to map the locations of the nests within the region studied. When set to \code{TRUE}, nest locations contained in the regions of interest
are plotted.
}
  \item{progress}{
Displays the progression of the function (number of nests for which the chronology has been estimated). The progression is displayed
when progress is set to \code{TRUE}. This argument uses the function \code{\link{progress}} from the svMisc package.
}
  \item{lsc}{
A named \code{list} with stages as names and each element of the \code{list} has to be status codes to be interpreted as such.
}
  \item{spgroup}{
A named \code{list} with group names as names. Each element of the \code{list} has to be a \code{vector} of characters with the species 4-letters AOU codes.
}
  \item{lang}{
Used within the tcltk interface to choose the language used. Default value \code{"en"}, otherwise \code{"fr"} is used.
}
  \item{fromtk}{
Used when the function is called within the tcltk interface to generate error and warning messages with \code{\link{tkmessageBox}}. Default value \code{FALSE}.
}
  \item{\dots}{
Further arguments to be passed to the \code{\link{chronology}} function.
}
}
\details{
Details of the algorithm are described in the Rnest manual.
}
\value{
A \code{\link{data.frame}} with the following columns:

  \item{sp}{species 4-letters AOU code}
  \item{id}{nest id}
  \item{region}{region where the nest is found}
  \item{initiation}{estimated initiation date (character)}
  \item{fledging}{estimated fledging date or of departure from the nest date (character)}
  \item{warning}{warning encountered when backcalculating}
}
\author{
Francois Rousseu <francoisrousseu@hotmail.com>}
\note{
To be described
}
\seealso{
\code{\link{chronology}}, \code{\link{nesting.activity}}
}
\examples{
To be described.
}
