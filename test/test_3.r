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
library(gt)

for (i in sort(fs::dir_ls("R"))) {source(i)}

orddata <- data.table::fread("test/paper_data/BYI_DEMO.DAT")[,-c(1:4)]
names(orddata) <- LETTERS[1:20]
# write_csv(orddata, "test/paper_data/grm_dt.csv")

# orddata <- data.table::fread("test/BYI1.DAT",header = T)
orddata <- data.table::fread("test/grm_dt.csv",header = F)
# orddata <- data.table::fread("test/ITEMS.DAT",header = F)

unique(orddata$V5)

# startGRshiny()

lav.model <- genLavSyn(orddata)
cat(lav.model)

rownames(a1$grm.par)

a1 <- runGRM(dat = orddata, lav.syntax = lav.model, estimator = "ML")
coef(a1$mirt.fit, simplify = T, IRTpars = F)$items
coef(a1$mirt.fit, simplify = T, IRTpars = T)$items

fittable <- extract_fit(a1)

if(any(str_detect(fittable[[1]], "SABIC"))) {
  fittable %>%
    gt() %>%
    tab_header(
      title = md("**MODEL FIT INFORMATION**")
    ) %>%
    tab_row_group(
      label = md("**Information Criteria**"),
      rows = c(5:7)
    ) %>%
    tab_row_group(
      label = md("**Chi-Square Test of Model Fit**"),
      rows = c(2:4)
    ) %>%
    tab_row_group(
      label = md("**Loglikelihood**"),
      rows = 1
    )

} else {
  fittable %>%
    gt() %>%
    tab_header(
      title = md("**MODEL FIT INFORMATION**")
    ) %>%
    tab_row_group(
      label = md("**SRMR (Standardized Root Mean Square Residual)**"),
      rows = c(10)
    ) %>%
    tab_row_group(
      label = md("**RMSE**"),
      rows = c(6:9)
    ) %>%
    tab_row_group(
      label = md("**CFI/TLI**"),
      rows = 4:5
    ) %>%
    tab_row_group(
      label = md("**Chi-Square Test of Model Fit**"),
      rows = c(1:3)
    )
}



est_results <- extract_est(a1) %>%
  mutate_if(is.numeric, ~ round(., 3))

fl <- str_which(est_results$op, "=~")
thre <- str_which(est_results$rhs, "^t")

est_results %>%
  gt() %>%
  tab_header(
    title = md("**MODEL RESULTS**")
  ) %>%
  tab_row_group(
    label = md("**Thresholds**"),
    rows = c(thre)
  ) %>%
  tab_row_group(
    label = md("**Item Slope**"),
    rows = c(fl)
  )


a1$grm.par %>%
  gt() %>%
  tab_style(
    locations = cells_column_labels(columns = gt::everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)),
      #Make text bold
      cell_text(weight = "bold")
    )
  ) %>%
  opt_all_caps() %>%
  #Use the Chivo font
  #Note the great 'google_font' function in 'gt' that removes the need to pre-load fonts
  opt_table_font(
    font = list(
      google_font("Chivo"),
      default_fonts()
    )
  )



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

infoPlot(a1,selected_item = c(9,20),
         theta = seq(-4, 4, .1),
         type = "icc")
plot(
  a1$mirt.fit,
  type = 'infotrace',
  which.items = c(9,20),
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


infoPlot(a1,selected_item = 1,
         type = "icc",theta = seq(-4, 4, .1))

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
