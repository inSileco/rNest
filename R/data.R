# Documentation of the datasets in data/.

#' Bird Characteristics
#'
#' Displays several characteristics of birds breeding in Canada.
#'
#' Used to build bird groups
#'
#' @format A data frame with 436 rows and 49 variables:
#' \describe{
#'   \item{Species_ID}{a character vector}
#'   \item{oldSpecies_ID}{a character vector}
#'   \item{English_Name}{a character vector}
#'   \item{French_Name}{a character vector}
#'   \item{Scientific_Name}{a character vector}
#'   \item{MBCA_2011_Final}{a character vector}
#'   \item{mclutch}{a numeric vector}
#'   \item{cavity}{a numeric vector}
#'   \item{Species_gr1}{a character vector}
#'   \item{Species_gr2}{a character vector}
#'   \item{miggroup}{a character vector}
#'   \item{Habitat1}{a character vector}
#'   \item{Habitat2}{a character vector}
#'   \item{Habitat3}{a character vector}
#'   \item{Habitat4}{a logical vector}
#'   \item{Nest_Type1}{a character vector}
#'   \item{Nest_Type2}{a character vector}
#'   \item{Nest_Type3}{a character vector}
#'   \item{Mig_W}{a character vector}
#'   \item{Mig_C}{a character vector}
#'   \item{Mig_E}{a character vector}
#'   \item{size}{a character vector}
#'   \item{CAV}{a numeric vector}
#'   \item{COL}{a numeric vector}
#'   \item{Nest_Reused}{a logical vector}
#'   \item{BCR03}{a logical vector}
#'   \item{BCR04}{a logical vector}
#'   \item{BCR05}{a logical vector}
#'   \item{BCR06}{a logical vector}
#'   \item{BCR07}{a logical vector}
#'   \item{BCR08}{a logical vector}
#'   \item{BCR09}{a logical vector}
#'   \item{BCR10}{a logical vector}
#'   \item{BCR11}{a logical vector}
#'   \item{BCR12}{a logical vector}
#'   \item{BCR14}{a logical vector}
#'   \item{TF}{a numeric vector}
#'   \item{TO}{a numeric vector}
#'   \item{WE}{a numeric vector}
#'   \item{SS}{a numeric vector}
#'   \item{WAT}{a numeric vector}
#'   \item{GRO}{a numeric vector}
#'   \item{BOT}{a numeric vector}
#'   \item{CLI}{a numeric vector}
#'   \item{MAN}{a numeric vector}
#'   \item{Family_Sci}{a character vector}
#'   \item{Family_CmName_EN}{a character vector}
#'   \item{AOU_Sort_Order}{a numeric vector}
#'   \item{model}{a numeric vector}
#' }
#' @references To be described
#' @source To be described
"birdgroupings"

#' Ecodistrict database
#'
#' Ecodistricts database
#'
#' ...
#'
#' @format A data frame with 928874 rows and 7 variables:
#' \describe{
#'   \item{long}{a numeric vector}
#'   \item{lat}{a numeric vector}
#'   \item{order}{a numeric vector}
#'   \item{hole}{a logical vector}
#'   \item{piece}{a numeric vector}
#'   \item{group}{a character vector}
#'   \item{id}{a character vector}
#' }
#' @references ...
#' @source ...
#' @examples
#' data(ecodistrictscsv)
"ecodistrictscsv"

#' Canadian Bird Nesting Parameters
#'
#' Nesting parameters of birds nesting in Canada
#'
#' Values were adapted from the species BNA accounts (Birds of North America).
#'
#' @format A data frame with 7 rows and 15 variables:
#' \describe{
#'   \item{common_name}{a character vector}
#'   \item{clutch}{a numeric vector}
#'   \item{cmin}{a numeric vector}
#'   \item{cmax}{a numeric vector}
#'   \item{laying}{a numeric vector}
#'   \item{incub}{a numeric vector}
#'   \item{start}{a numeric vector}
#'   \item{rearing}{a numeric vector}
#'   \item{sp}{a character vector}
#'   \item{scientific_name}{a character vector}
#'   \item{nom_commun}{a character vector}
#'   \item{species_code}{a character vector}
#'   \item{data}{a character vector}
#'   \item{AOUsort}{a numeric vector}
#'   \item{remarks}{a character vector}
#' }
#' @references The Birds of North America Online (A. Poole, Ed.). Ithaca:
#' Cornell Laboratory of Ornithology; Retrieved from The Birds of North America
#' Online database, URL: http://bna.birds.cornell.edu/BNA/, AUG 2005.
#' @source To be described
"nestingparameters"

#' Bird observation data
#'
#' A small excerpt from the NestWatch database (BSC)
#'
#' The database presented here has been filtered and modified to be usable by
#' rNest.
#'
#' @format A data frame with 24 rows and 46 variables:
#' \describe{
#'   \item{nest_id}{a numeric vector}
#'   \item{status_code}{a character vector}
#'   \item{visit_id}{a numeric vector}
#'   \item{month}{a numeric vector}
#'   \item{day}{a numeric vector}
#'   \item{time}{a numeric vector}
#'   \item{eggs}{a numeric vector}
#'   \item{young}{a numeric vector}
#'   \item{deadyg}{a numeric vector}
#'   \item{bhco_eggs}{a numeric vector}
#'   \item{bhco_young}{a numeric vector}
#'   \item{bhco_deadyg}{a numeric vector}
#'   \item{nest_no}{a numeric vector}
#'   \item{user_id}{a numeric vector}
#'   \item{year}{a numeric vector}
#'   \item{species_code}{a character vector}
#'   \item{species_code2}{a character vector}
#'   \item{statprov_code}{a character vector}
#'   \item{county_code}{a character vector}
#'   \item{post_code}{a character vector}
#'   \item{square_id}{a character vector}
#'   \item{loc_label}{a character vector}
#'   \item{loc_type}{a character vector}
#'   \item{name}{a character vector}
#'   \item{description}{a character vector}
#'   \item{coordinates_source}{a character vector}
#'   \item{coordinates_type}{a character vector}
#'   \item{lat}{a numeric vector}
#'   \item{lon}{a numeric vector}
#'   \item{utm_zone}{a numeric vector}
#'   \item{utm_easting}{a numeric vector}
#'   \item{utm_northing}{a numeric vector}
#'   \item{altitude}{a character vector}
#'   \item{data_source}{a character vector}
#'   \item{date}{a character vector}
#'   \item{type_nw_code}{a character vector}
#'   \item{stage_code}{a character vector}
#'   \item{outcome}{a character vector}
#'   \item{nbvisit}{a numeric vector}
#'   \item{sp}{a character vector}
#'   \item{region}{a logical vector}
#'   \item{initiation}{a character vector}
#'   \item{fledging}{a character vector}
#'   \item{warning}{a character vector}
#'   \item{len_active}{a numeric vector}
#'   \item{span_error}{a numeric vector}
#' }
#' @references BSC. 2011. Project NestWatch. Bird Studies Canada / Études
#' d'oiseaux Canada, URL: http://www.bsc-eoc.org
#' @source To be described
"nestwatch"

#' Model predictions
#'
#' Model predictions
#'
#' ...
#'
#' @format A data frame with 239074 rows and 4 variables:
#' \describe{
#'   \item{ecodistrict}{a numeric vector}
#'   \item{type}{a character vector}
#'   \item{species_code}{a character vector}
#'   \item{jul}{a numeric vector}
#' }
#' @references ...
#' @source ...
#' @examples
#' data(predictions)
"predictions"

#' Species ranges
#'
#' Species ecodistricts association
#'
#' ...
#'
#' @format A data frame with 1021 rows and 374 variables:
#' \describe{
#'   \item{ecodis}{a numeric vector}
#'   \item{GWFG}{a logical vector}
#'   \item{SNGO}{a logical vector}
#'   \item{ROGO}{a logical vector}
#'   \item{BRAN}{a logical vector}
#'   \item{CAGO}{a logical vector}
#'   \item{MUSW}{a logical vector}
#'   \item{TRUS}{a logical vector}
#'   \item{TUSW}{a logical vector}
#'   \item{WODU}{a logical vector}
#'   \item{GADW}{a logical vector}
#'   \item{AMWI}{a logical vector}
#'   \item{ABDU}{a logical vector}
#'   \item{MALL}{a logical vector}
#'   \item{BWTE}{a logical vector}
#'   \item{CITE}{a logical vector}
#'   \item{NSHO}{a logical vector}
#'   \item{NOPI}{a logical vector}
#'   \item{GWTE}{a logical vector}
#'   \item{CANV}{a logical vector}
#'   \item{REDH}{a logical vector}
#'   \item{RNDU}{a logical vector}
#'   \item{GRSC}{a logical vector}
#'   \item{LESC}{a logical vector}
#'   \item{KIEI}{a logical vector}
#'   \item{COEI}{a logical vector}
#'   \item{SUSC}{a logical vector}
#'   \item{WWSC}{a logical vector}
#'   \item{BLSC}{a logical vector}
#'   \item{LTDU}{a logical vector}
#'   \item{BUFF}{a logical vector}
#'   \item{COGO}{a logical vector}
#'   \item{BAGO}{a logical vector}
#'   \item{HOME}{a logical vector}
#'   \item{COME}{a logical vector}
#'   \item{RBME}{a logical vector}
#'   \item{RUDU}{a logical vector}
#'   \item{CAQU}{a logical vector}
#'   \item{NOBO}{a logical vector}
#'   \item{RUGR}{a logical vector}
#'   \item{GRSG}{a logical vector}
#'   \item{SPGR}{a logical vector}
#'   \item{WIPT}{a logical vector}
#'   \item{ROPT}{a logical vector}
#'   \item{WTPT}{a logical vector}
#'   \item{DUGR}{a logical vector}
#'   \item{STGR}{a logical vector}
#'   \item{WITU}{a logical vector}
#'   \item{RTLO}{a logical vector}
#'   \item{PALO}{a logical vector}
#'   \item{COLO}{a logical vector}
#'   \item{PBGR}{a logical vector}
#'   \item{HOGR}{a logical vector}
#'   \item{RNGR}{a logical vector}
#'   \item{EAGR}{a logical vector}
#'   \item{WEGR}{a logical vector}
#'   \item{DCCO}{a logical vector}
#'   \item{AWPE}{a logical vector}
#'   \item{AMBI}{a logical vector}
#'   \item{LEBI}{a logical vector}
#'   \item{GBHE}{a logical vector}
#'   \item{GREG}{a logical vector}
#'   \item{CAEG}{a logical vector}
#'   \item{GRHE}{a logical vector}
#'   \item{BCNH}{a logical vector}
#'   \item{TUVU}{a logical vector}
#'   \item{OSPR}{a logical vector}
#'   \item{BAEA}{a logical vector}
#'   \item{NOHA}{a logical vector}
#'   \item{SSHA}{a logical vector}
#'   \item{COHA}{a logical vector}
#'   \item{NOGO}{a logical vector}
#'   \item{RSHA}{a logical vector}
#'   \item{BWHA}{a logical vector}
#'   \item{SWHA}{a logical vector}
#'   \item{RTHA}{a logical vector}
#'   \item{FEHA}{a logical vector}
#'   \item{RLHA}{a logical vector}
#'   \item{GOEA}{a logical vector}
#'   \item{AMKE}{a logical vector}
#'   \item{MERL}{a logical vector}
#'   \item{GYRF}{a logical vector}
#'   \item{PEFA}{a logical vector}
#'   \item{PRFA}{a logical vector}
#'   \item{YERA}{a logical vector}
#'   \item{KIRA}{a logical vector}
#'   \item{VIRA}{a logical vector}
#'   \item{SORA}{a logical vector}
#'   \item{COMO}{a logical vector}
#'   \item{AMCO}{a logical vector}
#'   \item{SACR}{a logical vector}
#'   \item{BBPL}{a logical vector}
#'   \item{AMGP}{a logical vector}
#'   \item{SEPL}{a logical vector}
#'   \item{PIPL}{a logical vector}
#'   \item{KILL}{a logical vector}
#'   \item{BNST}{a logical vector}
#'   \item{AMAV}{a logical vector}
#'   \item{SPSA}{a logical vector}
#'   \item{SOSA}{a logical vector}
#'   \item{WATA}{a logical vector}
#'   \item{GRYE}{a logical vector}
#'   \item{WILL}{a logical vector}
#'   \item{LEYE}{a logical vector}
#'   \item{UPSA}{a logical vector}
#'   \item{WHIM}{a logical vector}
#'   \item{LBCU}{a logical vector}
#'   \item{HUGO}{a logical vector}
#'   \item{MAGO}{a logical vector}
#'   \item{RUTU}{a logical vector}
#'   \item{SURF}{a logical vector}
#'   \item{REKN}{a logical vector}
#'   \item{SAND}{a logical vector}
#'   \item{SESA}{a logical vector}
#'   \item{LESA}{a logical vector}
#'   \item{WRSA}{a logical vector}
#'   \item{BASA}{a logical vector}
#'   \item{PESA}{a logical vector}
#'   \item{PUSA}{a logical vector}
#'   \item{DUNL}{a logical vector}
#'   \item{STSA}{a logical vector}
#'   \item{BBSA}{a logical vector}
#'   \item{SBDO}{a logical vector}
#'   \item{LBDO}{a logical vector}
#'   \item{COSN}{a logical vector}
#'   \item{AMWO}{a logical vector}
#'   \item{WIPH}{a logical vector}
#'   \item{RNPH}{a logical vector}
#'   \item{REPH}{a logical vector}
#'   \item{SAGU}{a logical vector}
#'   \item{BOGU}{a logical vector}
#'   \item{BHGU}{a logical vector}
#'   \item{LIGU}{a logical vector}
#'   \item{FRGU}{a logical vector}
#'   \item{MEGU}{a logical vector}
#'   \item{RBGU}{a logical vector}
#'   \item{CAGU}{a logical vector}
#'   \item{HERG}{a logical vector}
#'   \item{GLGU}{a logical vector}
#'   \item{GBBG}{a logical vector}
#'   \item{CATE}{a logical vector}
#'   \item{BLTE}{a logical vector}
#'   \item{COTE}{a logical vector}
#'   \item{ARTE}{a logical vector}
#'   \item{FOTE}{a logical vector}
#'   \item{PAJA}{a logical vector}
#'   \item{LTJA}{a logical vector}
#'   \item{BLGU}{a logical vector}
#'   \item{RODO}{a logical vector}
#'   \item{MODO}{a logical vector}
#'   \item{YBCU}{a logical vector}
#'   \item{BBCU}{a logical vector}
#'   \item{FLOW}{a logical vector}
#'   \item{EASO}{a logical vector}
#'   \item{GHOW}{a logical vector}
#'   \item{SNOW}{a logical vector}
#'   \item{NHOW}{a logical vector}
#'   \item{BUOW}{a logical vector}
#'   \item{BDOW}{a logical vector}
#'   \item{GGOW}{a logical vector}
#'   \item{LEOW}{a logical vector}
#'   \item{SEOW}{a logical vector}
#'   \item{BOOW}{a logical vector}
#'   \item{NSWO}{a logical vector}
#'   \item{CONI}{a logical vector}
#'   \item{CWWI}{a logical vector}
#'   \item{WPWI}{a logical vector}
#'   \item{CHSW}{a logical vector}
#'   \item{RTHU}{a logical vector}
#'   \item{CAHU}{a logical vector}
#'   \item{RUHU}{a logical vector}
#'   \item{BEKI}{a logical vector}
#'   \item{RHWO}{a logical vector}
#'   \item{RBWO}{a logical vector}
#'   \item{YBSA}{a logical vector}
#'   \item{RNSA}{a logical vector}
#'   \item{RBSA}{a logical vector}
#'   \item{DOWO}{a logical vector}
#'   \item{HAWO}{a logical vector}
#'   \item{ATTW}{a logical vector}
#'   \item{BBWO}{a logical vector}
#'   \item{NOFL}{a logical vector}
#'   \item{PIWO}{a logical vector}
#'   \item{OSFL}{a logical vector}
#'   \item{WEWP}{a logical vector}
#'   \item{EAWP}{a logical vector}
#'   \item{YBFL}{a logical vector}
#'   \item{ACFL}{a logical vector}
#'   \item{ALFL}{a logical vector}
#'   \item{WIFL}{a logical vector}
#'   \item{LEFL}{a logical vector}
#'   \item{HAFL}{a logical vector}
#'   \item{DUFL}{a logical vector}
#'   \item{COFL}{a logical vector}
#'   \item{EAPH}{a logical vector}
#'   \item{SAPH}{a logical vector}
#'   \item{GCFL}{a logical vector}
#'   \item{WEKI}{a logical vector}
#'   \item{EAKI}{a logical vector}
#'   \item{LOSH}{a logical vector}
#'   \item{NSHR}{a logical vector}
#'   \item{WEVI}{a logical vector}
#'   \item{YTVI}{a logical vector}
#'   \item{BHVI}{a logical vector}
#'   \item{HUVI}{a logical vector}
#'   \item{WAVI}{a logical vector}
#'   \item{PHVI}{a logical vector}
#'   \item{REVI}{a logical vector}
#'   \item{GRAJ}{a logical vector}
#'   \item{STJA}{a logical vector}
#'   \item{BLJA}{a logical vector}
#'   \item{CLNU}{a logical vector}
#'   \item{BBMA}{a logical vector}
#'   \item{AMCR}{a logical vector}
#'   \item{NOCR}{a logical vector}
#'   \item{CORA}{a logical vector}
#'   \item{SKLA}{a logical vector}
#'   \item{HOLA}{a logical vector}
#'   \item{PUMA}{a logical vector}
#'   \item{TRES}{a logical vector}
#'   \item{VGSW}{a logical vector}
#'   \item{NRWS}{a logical vector}
#'   \item{BANS}{a logical vector}
#'   \item{CLSW}{a logical vector}
#'   \item{BARS}{a logical vector}
#'   \item{BCCH}{a logical vector}
#'   \item{MOCH}{a logical vector}
#'   \item{CBCH}{a logical vector}
#'   \item{BOCH}{a logical vector}
#'   \item{TUTI}{a logical vector}
#'   \item{BUSH}{a logical vector}
#'   \item{RBNU}{a logical vector}
#'   \item{WBNU}{a logical vector}
#'   \item{PYNU}{a logical vector}
#'   \item{BRCR}{a logical vector}
#'   \item{ROWR}{a logical vector}
#'   \item{CANW}{a logical vector}
#'   \item{CARW}{a logical vector}
#'   \item{BEWR}{a logical vector}
#'   \item{HOWR}{a logical vector}
#'   \item{WIWR}{a logical vector}
#'   \item{SEWR}{a logical vector}
#'   \item{MAWR}{a logical vector}
#'   \item{BGGN}{a logical vector}
#'   \item{AMDI}{a logical vector}
#'   \item{GCKI}{a logical vector}
#'   \item{RCKI}{a logical vector}
#'   \item{NOWH}{a logical vector}
#'   \item{EABL}{a logical vector}
#'   \item{WEBL}{a logical vector}
#'   \item{MOBL}{a logical vector}
#'   \item{TOSO}{a logical vector}
#'   \item{VEER}{a logical vector}
#'   \item{GCTH}{a logical vector}
#'   \item{BITH}{a logical vector}
#'   \item{SWTH}{a logical vector}
#'   \item{HETH}{a logical vector}
#'   \item{WOTH}{a logical vector}
#'   \item{AMRO}{a logical vector}
#'   \item{VATH}{a logical vector}
#'   \item{GRCA}{a logical vector}
#'   \item{NOMO}{a logical vector}
#'   \item{BRTH}{a logical vector}
#'   \item{EUST}{a logical vector}
#'   \item{AMPI}{a logical vector}
#'   \item{SPPI}{a logical vector}
#'   \item{BOWA}{a logical vector}
#'   \item{CEDW}{a logical vector}
#'   \item{LALO}{a logical vector}
#'   \item{CCLO}{a logical vector}
#'   \item{SMLO}{a logical vector}
#'   \item{MCLO}{a logical vector}
#'   \item{SNBU}{a logical vector}
#'   \item{OVEN}{a logical vector}
#'   \item{LOWA}{a logical vector}
#'   \item{NOWA}{a logical vector}
#'   \item{GWWA}{a logical vector}
#'   \item{BWWA}{a logical vector}
#'   \item{BAWW}{a logical vector}
#'   \item{PROW}{a logical vector}
#'   \item{TEWA}{a logical vector}
#'   \item{OCWA}{a logical vector}
#'   \item{NAWA}{a logical vector}
#'   \item{CONW}{a logical vector}
#'   \item{MGWA}{a logical vector}
#'   \item{MOWA}{a logical vector}
#'   \item{COYE}{a logical vector}
#'   \item{HOWA}{a logical vector}
#'   \item{AMRE}{a logical vector}
#'   \item{CMWA}{a logical vector}
#'   \item{CERW}{a logical vector}
#'   \item{NOPA}{a logical vector}
#'   \item{MAWA}{a logical vector}
#'   \item{BBWA}{a logical vector}
#'   \item{BLBW}{a logical vector}
#'   \item{YWAR}{a logical vector}
#'   \item{CSWA}{a logical vector}
#'   \item{BLPW}{a logical vector}
#'   \item{BTBW}{a logical vector}
#'   \item{PAWA}{a logical vector}
#'   \item{PIWA}{a logical vector}
#'   \item{YRWA}{a logical vector}
#'   \item{PRAW}{a logical vector}
#'   \item{BTYW}{a logical vector}
#'   \item{TOWA}{a logical vector}
#'   \item{BTNW}{a logical vector}
#'   \item{CAWA}{a logical vector}
#'   \item{WIWA}{a logical vector}
#'   \item{YBCH}{a logical vector}
#'   \item{SPTO}{a logical vector}
#'   \item{EATO}{a logical vector}
#'   \item{ATSP}{a logical vector}
#'   \item{CHSP}{a logical vector}
#'   \item{CCSP}{a logical vector}
#'   \item{BRSP}{a logical vector}
#'   \item{FISP}{a logical vector}
#'   \item{VESP}{a logical vector}
#'   \item{LASP}{a logical vector}
#'   \item{LARB}{a logical vector}
#'   \item{SAVS}{a logical vector}
#'   \item{GRSP}{a logical vector}
#'   \item{BAIS}{a logical vector}
#'   \item{HESP}{a logical vector}
#'   \item{LCSP}{a logical vector}
#'   \item{NSTS}{a logical vector}
#'   \item{FOSP}{a logical vector}
#'   \item{SOSP}{a logical vector}
#'   \item{LISP}{a logical vector}
#'   \item{SWSP}{a logical vector}
#'   \item{WTSP}{a logical vector}
#'   \item{HASP}{a logical vector}
#'   \item{WCSP}{a logical vector}
#'   \item{GCSP}{a logical vector}
#'   \item{DEJU}{a logical vector}
#'   \item{SCTA}{a logical vector}
#'   \item{WETA}{a logical vector}
#'   \item{NOCA}{a logical vector}
#'   \item{RBGR}{a logical vector}
#'   \item{BHGR}{a logical vector}
#'   \item{LAZB}{a logical vector}
#'   \item{INBU}{a logical vector}
#'   \item{DICK}{a logical vector}
#'   \item{BOBO}{a logical vector}
#'   \item{RWBL}{a logical vector}
#'   \item{EAME}{a logical vector}
#'   \item{WEME}{a logical vector}
#'   \item{YHBL}{a logical vector}
#'   \item{RUBL}{a logical vector}
#'   \item{BRBL}{a logical vector}
#'   \item{COGR}{a logical vector}
#'   \item{BHCO}{a logical vector}
#'   \item{OROR}{a logical vector}
#'   \item{BUOR}{a logical vector}
#'   \item{BAOR}{a logical vector}
#'   \item{GCRF}{a logical vector}
#'   \item{PIGR}{a logical vector}
#'   \item{PUFI}{a logical vector}
#'   \item{CAFI}{a logical vector}
#'   \item{HOFI}{a logical vector}
#'   \item{RECR}{a logical vector}
#'   \item{WWCR}{a logical vector}
#'   \item{CORE}{a logical vector}
#'   \item{HORE}{a logical vector}
#'   \item{PISI}{a logical vector}
#'   \item{AMGO}{a logical vector}
#'   \item{EVGR}{a logical vector}
#'   \item{HOSP}{a logical vector}
#'   \item{HADU}{a logical vector}
#'   \item{GRAP}{a logical vector}
#'   \item{RNEP}{a logical vector}
#'   \item{LESP}{a logical vector}
#'   \item{BANO}{a logical vector}
#'   \item{GFLY}{a logical vector}
#'   \item{EYWA}{a logical vector}
#' }
#' @references ...
#' @source ...
#' @examples
#' data(specodistricts)
"specodistricts"
