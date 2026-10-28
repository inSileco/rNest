warnparam <-
  function(x = x, param = nparam, prog = 4, warn = warn) {
    theosta <- c(
      nparam$cmax,
      nparam$laying * prog,
      nparam$incubation,
      nparam$rearing
    )
    sta <- names(x)
    laying <- length(which(sta == "laying"))
    incubation <- length(which(sta == "incubation"))
    rearing <- length(which(sta == "young"))
    obs <- c(prog, laying, incubation, rearing)
    if (warn$type == "percentage") {
      varobs <- c(prog - theosta[1], (obs[-1] / theosta[-1]) - 1)
    } else {
      #if warn$type=="days"
      varobs <- c(prog - theosta[1], obs[-1] - theosta[-1])
    }
    print(varobs)
    varobs <- c(
      ifelse(varobs[1] > 0L, TRUE, FALSE),
      varobs[-1] > warn$over | varobs[-1] < (warn$under * (-1))
    )
    names(varobs) <- c("clutch", "laying", "incubation", "young")
    return(list(warn = varobs, reject = any(varobs)))
  }
