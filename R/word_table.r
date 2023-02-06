word_out3 <- function(filename,
                      reportTables) {
  # reportTables <- list()
  # reportTables$fit.dt <- extract_fit(a1)
  # reportTables$est.dt <- extract_est(a1)
  # reportTables$grmest.dt <- a1$grm.par

  my.doc <- read_docx()

  fit.dt    <- reportTables$fit.dt %>% mutate_if(is.numeric, ~ round(.x, 3))
  est.dt    <- reportTables$est.dt %>% mutate_if(is.numeric, ~ round(.x, 3)) %>%
    dplyr::select(lhs, op, rhs, est,se, pvalue)
  grmest.dt <- reportTables$grmest.dt %>% mutate_if(is.numeric, ~ round(.x, 3)) %>%
    mutate(Variable = rownames(reportTables$grmest.dt), .before = a1)



  fit.dt <- flextable(fit.dt) %>%
    width(width = 1.5) %>%
    height(height = .3, part = "body") %>%
    padding(padding = 0.1) %>%
    hrule(., rule = "exact") %>%
    set_caption(caption = "Table. Fix indices")

  est.dt <- flextable(est.dt) %>%
    height(height = .3, part = "body") %>%
    padding(padding = 0.1) %>%
    hrule(., rule = "exact") %>%
    set_caption(caption = "Table. Parameter estimates (SEM specification)")

  grmest.dt <- flextable(grmest.dt) %>%
    height(height = .3, part = "body") %>%
    padding(padding = 0.1) %>%
    hrule(., rule = "exact") %>%
    set_caption(caption = "Table. Parameter estimates (GRM specification)")

  table_add(my.doc,fit.dt)
  body_add_break(my.doc, pos = "after")
  table_add(my.doc,est.dt)
  body_add_break(my.doc, pos = "after")
  table_add(my.doc,grmest.dt)

  print(my.doc, target = filename)
  # print(my.doc, target = "test.docx")
}


table_add <- function(x.doc, x.tb) {
  flextable::body_add_flextable(
    x.doc,
    value = x.tb,
    align = "left"
  )

  body_add_par(x.doc, " ")
}
