rm(list = ls())
library(tidyverse)
library(lavaan)
library(mirt)
library(sirt)

orddata <- read_csv("test/grm_dt.csv", col_names = F)
names(orddata) <- paste0("a",1:4)



# WLSMV in lavaan ------------------------------------------------------
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
  estimator = "WLSMV" #"ML"
)


# ML in mirt ----------------------------------------------------------
grm.mirt <- mirt(
  data = orddata,
  method = 'EM',
  model = paste0("F = 1-", ncol(orddata)),
  itemtype = "graded"
)
# summary(grm.mirt)
est.irt.mirt <- coef(grm.mirt, simplify = T, IRTpars = T)
est.irt.mirt
est.mirt <- coef(grm.mirt, simplify = T, IRTpars = F)
est.mirt
