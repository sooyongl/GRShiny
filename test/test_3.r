library(GRShiny)
library(lavaan)
library(tidyverse)
library(data.table)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(bslib)
library(lavaan)
library(mirt)
library(sirt)
library(MASS)
library(DT)
library(officer)
library(flextable)

for (i in sort(fs::dir_ls("R"))) {source(i)}

orddata <- data.table::fread("test/paper_data/BYI_DEMO.DAT")[,-c(1:4)]
names(orddata) <- LETTERS[1:20]
# write_csv(orddata, "test/paper_data/grm_dt.csv")

orddata <- data.table::fread("test/BYI1.DAT",header = T)

unique(orddata$V5)

startGRshiny()

lav.model <- genLavSyn(orddata)
cat(lav.model)

a1 <- runGRM(dat = orddata, lav.syntax = lav.model, estimator = "WLS")
coef(a1$mirt.fit, simplify = T, IRTpars = F)$items
coef(a1$mirt.fit, simplify = T, IRTpars = T)$items
extract_est(a1)
extract_fit(a1)
a1$grm.par

a1 <- runGRM(orddata, lav.model, "WL")
summary(a1$lav.fit)
extract_fit(a1)
output_cleaning(a1)
a1$grm.par

# fs_scores <- calFS(a1)
font_size = 16
linesize = 2
colour_option = "D"


ICCplot(fit = a1, selected_item = 1, theta = seq(-2, 6, .1),
        plot.occ = FALSE,base_size = font_size, line_size = linesize, cal_option = "D")

theta_range <- seq(-4, 4, .1)

res     <- a1

ICCplot(
  fit = res,
  selected_item = c(1),
  theta = theta_range,
  plot.occ = T,
  base_size = 10,
  line_size = linesize,
  cal_option = "D",
  addlabel = T
)


itemplot(a1$mirt.fit, 2)
plot(
  a1$mirt.fit,
  type = 'trace',
  which.items = 1:2,
  theta_lim = c(-4, 4),
  facet_items = FALSE
)

ICCplot(a1,1:2,seq(-6, 6, .1), plot.ps = T, font_size, addlabel = F)
ICCplot(a1,1:2,seq(-6, 6, .1), plot.ps = T, font_size, addlabel = T)


ESplot(a1,selected_item = 1,seq(-4, 4, .1))

plot(a1$mirt.fit,type = 'score',
     which.items = 1,
     theta_lim =c(-4,4), facet_items = FALSE)

infoPlot(a1,selected_item = 1,
         theta = seq(-4, 4, .1),
         type = "icc")
plot(
  a1$mirt.fit,
  type = 'infotrace',
  which.items = 1,
  theta_lim = c(-4, 4),
  facet_items = FALSE
)
plot(
  a1$mirt.fit,
  type = 'info',
  which.items = 1,
  theta_lim = c(-4, 4),
  facet_items = FALSE
)




infoPlot(a1,type = "tcc",theta = seq(-4, 4, .1))

FSplot(a1,type = "histogram",hist_bins = 20)
FSplot(a1,type = "density",hist_bins = 20)
plot(
  a1$mirt.fit,
  type = 'EAPsum',
  which.items = 1,
  theta_lim = c(-4, 4),
  facet_items = FALSE
)
