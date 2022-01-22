rm(list = ls())
library(tidyverse)
library(lavaan)
library(mirt)
library(sirt)

library(MplusAutomation)
for (i in fs::dir_ls("R")) {
  source(i)
}

lvmodel <- "grm"
ipar <- genIRTpar(4, ncat = 3, 1, lvmodel)
eta <- MASS::mvrnorm(500, rep(0, 1), matrix(c(1), ncol = 1))
orddata <- genIRTdt(lvmodel, eta, ipar)
orddata <- data.frame(orddata)
names(orddata) <- paste0("a", 1:4)

write_csv(orddata, "test/grm_dt.csv", col_names = F)
runModels("test/grm_ML.inp")
runModels("test/grm_WL.inp")
grm.ml.mplus <- readModels("test/grm_ML.out")
grm.ml.mplus$parameters$unstandardized

grm.wl.mplus <- readModels("test/grm_WL.out")
grm.wl.mplus$parameters$unstandardized
file.show("test/grm_WL.out")
lav.model <- "
    f1 =~ NA*a1 + a2 + a3 + a4

    f1 ~~ 1*f1
    f1 ~ 0*1

    a1 | t1;
    a1 | t2;

    a2 | t1;
    a2 | t2;

    a3 | t1;
    a3 | t2;

    a4 | t1;
    a4 | t2;

     "
grm.lav <- cfa(
  model = lav.model,
  data = orddata,
  ordered = c("a1", "a2", "a3", "a4"),
  parameterization = "delta",
  # delta
  estimator = "WLSMV" #"ML"
)
summary(grm.lav, fit.measures = T)


grm.mirt <- mirt(
  data = orddata,
  method = 'EM',
  model = paste0("F = 1-", ncol(orddata)),
  itemtype = "graded"
)
# summary(grm.mirt)
est.irt.mirt <- coef(grm.mirt, simplify = T, IRTpars = T)
est.mirt <- coef(grm.mirt, simplify = T, IRTpars = F)

L  <- est.mirt$items[,"a1"]
T1 <- est.mirt$items[,"d1"]
T2 <- est.mirt$items[,"d2"]
FV <- est.mirt$cov
FM <- est.mirt$means

DIS <- c()
D1 <- c()
D2 <- c()
for(i in 1:length(L)) {

  l <- L[i]
  t1 <- T1[i]
  t2 <- T2[i]

  DIS[i] <- l*sqrt(FV) / sqrt((1 - (l)^2))
  D1[i] <- (t1 - l*FM) / l*sqrt(FV)
  D2[i] <- (t2 - l*FM) / l*sqrt(FV)
}


grm.sirt <-
  lavaan2mirt(orddata,
              lav.model,
              est.mirt = TRUE,
              poly.itemtype = "graded")
# inspect coefficients
grm.sirt$mirt.pars
mirt.wrapper.coef(grm.sirt$mirt)

grm.sirt <-
  lavaan2mirt(orddata,
              lav.model,
              est.mirt = F,
              poly.itemtype = "graded")
cat(grm.sirt$mirt.syntax)
# inspect coefficients
mirt.wrapper.coef(grm.sirt$mirt)
