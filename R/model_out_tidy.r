mirt_out_cleaning <- function(mirt.fit) {
  mirt.param <- coef(mirt.fit, simplify = F, printSE=TRUE)

  EST <- lapply(mirt.param[-length(mirt.param)], function(x) x["par",]) %>%
    do.call("rbind",.) %>%
    data.frame(., varname = rownames(.)) %>%
    gather("parname","est", -varname)
  SE <- lapply(mirt.param[-length(mirt.param)], function(x) x["SE",]) %>%
    do.call("rbind",.) %>%
    data.frame(., varname = rownames(.)) %>%
    gather("parname","se", -varname)

  EST %>%
    left_join(SE, by=c("varname","parname")) %>%
    mutate(z = est / se,
           pvalue = 2*pnorm(abs(z), mean = 0, sd = 1, lower.tail = F)) %>%
    mutate(
      lhs = if_else(str_detect(parname, "a"), "F", varname),
      op  = if_else(str_detect(parname, "a"), "=~", "|"),
      rhs = if_else(str_detect(parname, "a"), varname, parname),
      rhs = str_replace(rhs, "d","t")
    ) %>%
    select(lhs, op, rhs, everything()) %>%
    select(-varname, -parname) %>%
    mutate_if(is.numeric, ~ round(., 3))

}

lavaan_out_cleaning <- function(lav.fit) {
  parameterestimates(lav.fit) %>%
    filter(str_detect(op, "=~|\\|")) %>%
    select(-starts_with("ci"))
}

