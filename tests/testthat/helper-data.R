# Bundled datasets, loaded here rather than in the global environment.
nw <- get(data(nestwatch, package = "rNest", envir = environment()))
np <- get(data(nestingparameters, package = "rNest", envir = environment()))
