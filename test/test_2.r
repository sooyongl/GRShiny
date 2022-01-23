for (i in fs::dir_ls("R")) {source(i)}

ipar <- genIRTpar(4, ncat = 3, 1)
eta <- MASS::mvrnorm(500, rep(0, 1), matrix(c(1), ncol = 1))
orddata <- genData(eta, ipar)

lav.model <- genLavSyn(orddata)
cat(lav.model)


runGRM(dat = orddata, lav.syntax = lav.model, estimator = "ML")
runGRM(orddata, lav.model, "WL")
