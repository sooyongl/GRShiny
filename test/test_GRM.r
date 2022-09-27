library(lavaan)
library(data.table)

dat <- fread("test/ITEMS.DAT")

lav.syntax <- "
F1 =~ NA*V1+l1*V1+l2*V2+l3*V3+l4*V4+l5*V5+l6*V6+l7*V7+l8*V8+l9*V9+l10*V10+l11*V11+l12*V12+l13*V13+l14*V14+l15*V15+l16*V16+l17*V17+l18*V18+l19*V19+l20*V20


F1~~ 1*F1
F1~ 0*1
V1 | t11*t1;
V2 | t21*t1;
V3 | t31*t1;
V4 | t41*t1;
V5 | t51*t1;
V6 | t61*t1;
V7 | t71*t1;
V8 | t81*t1;
V9 | t91*t1;
V10 | t101*t1;
V11 | t111*t1;
V12 | t121*t1;
V13 | t131*t1;
V14 | t141*t1;
V15 | t151*t1;
V16 | t161*t1;
V17 | t171*t1;
V18 | t181*t1;
V19 | t191*t1;
V20 | t201*t1;
V1 | t12*t2;
V2 | t22*t2;
V3 | t32*t2;
V4 | t42*t2;
V5 | t52*t2;
V6 | t62*t2;
V7 | t72*t2;
V8 | t82*t2;
V9 | t92*t2;
V10 | t102*t2;
V11 | t112*t2;
V12 | t122*t2;
V13 | t132*t2;
V14 | t142*t2;
V15 | t152*t2;
V16 | t162*t2;
V17 | t172*t2;
V18 | t182*t2;
V19 | t192*t2;
V20 | t202*t2;
V1 | t13*t3;
V2 | t23*t3;
V3 | t33*t3;
V4 | t43*t3;
V5 | t53*t3;
V6 | t63*t3;
V7 | t73*t3;
V8 | t83*t3;
V9 | t93*t3;
V10 | t103*t3;
V11 | t113*t3;
V12 | t123*t3;
V13 | t133*t3;
V14 | t143*t3;
V15 | t153*t3;
V16 | t163*t3;
V17 | t173*t3;
V18 | t183*t3;
V19 | t193*t3;
V20 | t203*t3;
"

grm.fit <- cfa(
  model = lav.syntax,
  data = dat,
  ordered = names(dat),
  parameterization = "delta",# theta
  estimator = "WLSMV" #"ML" is not working for now in lavaan
)
Fit <- fitMeasures(fit)

c(Fit["chisq.scaled"],
  Fit["df.scaled"],
  Fit["pvalue.scaled"],

  Fit["cfi.scaled"],
  Fit["tli.scaled"],
  Fit["rmsea.scaled"],
  Fit["rmsea.ci.lower.scaled"],
  Fit["rmsea.ci.upper.scaled"],
  Fit["rmsea.pvalue.scaled"],
  Fit["srmr"]

)


summary(grm.fit)
