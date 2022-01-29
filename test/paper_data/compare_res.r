# devtools::install_github("sooyongl/GRShiny")
library(GRShiny)
library(data.table)

paperdt <- data.table::fread("test/paper_data/BYI_DEMO.DAT")[,-c(1:4)]

lav_model <- genLavSyn(paperdt, nfac = 1)

fit.wl <- runGRM(paperdt, lav_model, estimator = "WL")
fit.ml <- runGRM(paperdt, lav_model, estimator = "ML")

extract_est(fit.wl); fit.wl$grm.par
extract_est(fit.ml); fit.ml$grm.par

extract_fit(fit.wl)
extract_fit(fit.ml)

GRShiny::ICCplot(fit.wl, 1:3, plot.ps = F)
GRShiny::ICCplot(fit.wl, 1:3, plot.ps = T)
GRShiny::ESplot(fit.wl, 1:3)
GRShiny::FSplot(fit.wl, type = "histogram")
GRShiny::infoPlot(fit.wl, 1:3)
GRShiny::infoPlot(fit.wl, type = "tcc")
