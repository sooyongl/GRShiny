rm(list = ls())
library(tidyverse)
library(lavaan)
library(mirt)
library(sirt)

library(MplusAutomation)
for (i in fs::dir_ls("R")) {source(i)}
ls()
lvmodel <- "grm"
ipar <- genIRTpar(4, ncat = 3, 1)
eta <- MASS::mvrnorm(500, rep(0, 1), matrix(c(1), ncol = 1))
orddata <- genData(eta, ipar)

write_csv(orddata, "test/grm_dt.csv", col_names = F)

runModels("test/grm_ML.inp")
runModels("test/grm_WL.inp")
grm.ml.mplus <- readModels("test/grm_ML.out")
grm.ml.mplus$parameters$unstandardized
file.show("test/grm_ML.out")
grm.wl.mplus <- readModels("test/grm_WL.out")
grm.wl.mplus$parameters$unstandardized
file.show("test/grm_WL.out")
# https://groups.google.com/g/lavaan/c/AicOjPPmtPk



lav.model <- "
    f1 =~ NA*a1 + l1*a1 + l2*a2 + l3*a3 + l4*a4

    f1 ~~ 1*f1
    f1 ~ 0*1

    a1 | t11*t1;
    a1 | t12*t2;

    a2 | t21*t1;
    a2 | t22*t2;

    a3 | t31*t1;
    a3 | t32*t2;

    a4 | t41*t1;
    a4 | t42*t2;


    fv := 1;
    fm := 0;

    DIS1 := ((l1)*(sqrt(fv))) / sqrt(1 - ((l1)^2));
    DIS2 := ((l2)*(sqrt(fv))) / sqrt(1 - ((l2)^2));
    DIS3 := ((l3)*(sqrt(fv))) / sqrt(1 - ((l3)^2));
    DIS4 := ((l4)*(sqrt(fv))) / sqrt(1 - ((l4)^2));

    D1i1 := (t11 - l1*fm) / l1*sqrt(fv)
    D2i1 := (t12 - l1*fm) / l1*sqrt(fv)

    D1i2 := (t21 - l2*fm) / l2*sqrt(fv)
    D2i2 := (t22 - l2*fm) / l2*sqrt(fv)

    D1i3 := (t21 - l3*fm) / l3*sqrt(fv)
    D2i3 := (t22 - l3*fm) / l3*sqrt(fv)

    D1i4 := (t41 - l4*fm) / l4*sqrt(fv)
    D2i4 := (t42 - l4*fm) / l4*sqrt(fv)
     "
grm.lav <- cfa(
  model = lav.model,
  data = orddata,
  ordered = names(orddata),
  parameterization = "delta",
  # delta
  estimator = "WLSMV" #"ML"
)
warnings()
summary(grm.lav, fit.measures = T)

grm.est <- lavInspect(grm.lav, what = "est")

Lam <- data.frame(grm.est$lambda)
Thre <- grm.est$tau
n_thre <- nrow(Thre)/nrow(Lam)
Thre <- matrix(Thre, ncol = n_thre, byrow = T)

FV <- grm.est$psi
FM <- grm.est$alpha
estimator = "WL"
# n_lam <- lavaan::parameterEstimates(grm.lav) %>%
#   filter(str_detect(op, "=~")) %>%
#   pull(lhs) %>%
#   unique() %>%
#   length()
#
# n_thre <- lavaan::parameterEstimates(grm.lav) %>%
#   filter(str_detect(rhs, "^t")) %>%
#   mutate(n_thre = as.numeric(str_remove(rhs, "t"))) %>%
#   pull(n_thre) %>%
#   max()
#
# Lam <- lavaan::parameterEstimates(grm.lav) %>%
#   filter(str_detect(label, "^l")) %>%
#   pull(est)
#
# Thre <- lavaan::parameterEstimates(grm.lav) %>%
#   filter(str_detect(label, "^t")) %>%
#   pull(est) %>%
#   matrix(., ncol = n_thre, byrow = T)

grm.mirt <- mirt(
  data = orddata,
  method = 'EM',
  model = paste0("F = 1-", ncol(orddata)),
  itemtype = "graded"
)
# summary(grm.mirt)
est.irt.mirt <- coef(grm.mirt, simplify = T, IRTpars = T)
est.mirt <- coef(grm.mirt, simplify = T, IRTpars = F)

item.est <- data.frame(est.mirt$items)
Lam  <- item.est[grep("a", names(item.est))]
Thre <- item.est[grep("d", names(item.est))]
FV <- est.mirt$cov
FM <- est.mirt$means
estimator = "ML"

trans_to_grm <- function(grm.fit) {

  if(class(grm.fit) != "lavaan") {
    est.mirt <- coef(grm.fit, simplify = T, IRTpars = F)

    item.est <- data.frame(est.mirt$items)
    Lam  <- item.est[grep("a", names(item.est))]
    Thre <- item.est[grep("d", names(item.est))]
    FV <- est.mirt$cov
    FM <- est.mirt$means
    estimator = "ML"

  } else {
    est.lav <- lavInspect(grm.fit, what = "est")

    Lam <- data.frame(est.lav$lambda)
    Thre <- est.lav$tau
    n_thre <- nrow(Thre)/nrow(Lam)
    Thre <- data.frame(matrix(Thre, ncol = n_thre, byrow = T))

    FV <- est.lav$psi
    FM <- est.lav$alpha
    estimator = "WL"
  }

  Disc <- matrix(rep(0, nrow(Lam)*ncol(Lam)), ncol = ncol(Lam))
  Diff <- matrix(rep(0, nrow(Lam)*ncol(Thre)), ncol = ncol(Thre))
  for(i in 1:nrow(Lam)) {

    lam <- Lam[i,]
    thre <- Thre[i,]

    for(j in 1:ncol(Lam)) {

      if (estimator == "WL") {
        Disc[i,j] <- lam*sqrt(FV) / sqrt((1 - (lam)^2))
      } else {
        # if ML
        Disc[i,j] <- lam*sqrt(FV) / 1.7
      }
    }

    for(j in 1:ncol(Thre)) {
      Diff[i,j] <- (thre[,j] - lam*FM) / lam*sqrt(FV)
    }
  }
  if(class(grm.fit) == "lavaan") {
    Diff <- -Diff
  }
  colnames(Diff) <- paste0("d", 1:ncol(Diff))
  data.frame(Disc, Diff)
}

lavForgrm <- function(dat, lav.syntax, estimator) {

  if(estimator != "ML") {
    # WLE
    grm.fit <- cfa(
      model = lav.syntax,
      data = dat,
      ordered = names(dat),
      parameterization = "delta",# theta
      estimator = "WLSMV" #"ML" is not working for now in lavaan
    )

    grm.par <- trans_to_grm(grm.fit = grm.fit)

  } else {
    # ML
    grm.sirt <- lavaan2mirt(dat,lav.syntax,est.mirt = TRUE,poly.itemtype = "graded")

    grm.fit <- grm.sirt$mirt
    grm.par <- trans_to_grm(grm.fit = grm.fit)
  }


  return(list(grm.fit, grm.par))
}
lavForgrm(orddata, lav.model, "ML")
lavForgrm(orddata, lav.model, "WL")
# inspect coefficients



trans_to_grm(grm.fit = grm.lav)
trans_to_grm(grm.fit = grm.mirt)


# grm.sirt <-
#   lavaan2mirt(orddata,
#               lav.model,
#               est.mirt = TRUE,
#               poly.itemtype = "graded")
# # inspect coefficients
# grm.sirt$mirt.pars
# mirt.wrapper.coef(grm.sirt$mirt)
#
# grm.sirt <-
#   lavaan2mirt(orddata,
#               lav.model,
#               est.mirt = F,
#               poly.itemtype = "graded")
# cat(grm.sirt$mirt.syntax)
# # inspect coefficients
# mirt.wrapper.coef(grm.sirt$mirt)
