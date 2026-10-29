unknown_nbeggs <- function(dat) {
  if (all(dat$eggs >= 0 & dat$young >= 0)) {
    list(dat = dat, unknown = FALSE)
  }
  if (nrow(dat) == 1 || all(dat$eggs %in% c(-1, 0) & dat$young %in% c(-1, 0))) {
    return(list(dat = dat, unknown = TRUE))
  }
  egg <- dat$eggs[dat$stage_code == "eggs"]
  if (length(egg) == 0 || all(egg >= 0)) {
    return(list(dat = dat, unknown = FALSE))
  }
  prog <- max(
    ifelse(dat$eggs == (-1), 1, dat$eggs) +
      ifelse(dat$young == (-1), 1, dat$young)
  )
  if (all(egg == (-1))) {
    #dat$eggs[dat$stage_code=="eggs"]<-prog
    return(list(dat = dat, unknown = TRUE))
  }

  w <- which(dat$eggs > 0 & dat$stage_code == "eggs")[1]
  if (dat$eggs[w] == prog) {
    return(list(dat = dat, unknown = TRUE))
  } #(2011/10/03 this unknown was changed to F because of nest 1040923)
  if (dat$eggs[w] < prog) {
    ww <- match(-1, dat$eggs)
    #if(ww<w){  #removed check bug test_20120301_122656
    #     if((as.numeric(difftime(dat$date[w],dat$date[ww]))-1)>(dat$laying[1]*(dat$eggs[w]-1))){dat$eggs[ww]<-1}
    #     }
    dat <- dat[!(dat$eggs == (-1) & dat$stage_code == "eggs"), ]
    return(list(dat = dat[dat$eggs >= 0, ], unknown = FALSE)) # anciennement un FALSE ici check bug test_20120301_130733 watch out, ici on ne conserve que les observations d'oeufs non n\u00e9gatives car \u00e7a fuck lorsqu'on a de l'incubation
    #return(list(dat=dat,unknown=FALSE))  # TEST on ne conserve que les observations d'oeufs non n\u00e9gatives car \u00e7a fuck lorsqu'on a de l'incubation
  }
  return(list(dat = "negative numbers; not implemented yet", unknown = FALSE))
}
