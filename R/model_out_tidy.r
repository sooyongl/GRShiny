#' @include GRShiny-package.r
NULL

#' Clean output to look like Mplus
#'
#' @param fit an object from \code{\link{runGRM}}
#'
#' @details This tidies the results in the CFA framework.
#'
#' @export
extract_est <- function(fit) {

  fit <- fit[grep("fit",names(fit))][[1]]

  if(class(fit) == "lavaan") {
    cleaned_out <- lavaan_output_cleaning(fit)
  } else {
    cleaned_out <- mirt_output_cleaning(fit)
  }
  return(cleaned_out)
}

#' Extract model fit
#'
#' @param fit an object from \code{\link{runGRM}}
#'
#' @details This extacts model fit.
#'
#' @export
extract_fit <- function(fit) {
  # fit = a1
  fit <- fit[grep("fit",names(fit))][[1]]

  if(class(fit) == "lavaan") {
    Fit <- fitMeasures(fit)

    fit.dt <- data.frame(
      FixIndex = c("chisq","df","chisq_p","CFI","TLI","RMSEA","RMSE.ci.lower","RMSE.ci.upper","RMSE_p","SRMR"),

      value = c(Fit["chisq.scaled"],
                Fit["df.scaled"],
                Fit["pvalue.scaled"],

                Fit["cfi.scaled"],
                Fit["tli.scaled"],
                Fit["rmsea.scaled"],
                Fit["rmsea.ci.lower.scaled"],
                Fit["rmsea.ci.upper.scaled"],
                Fit["rmsea.pvalue.scaled"],
                Fit["srmr"]

      ))

    rownames(fit.dt)<-NULL

  } else {
    Fit <- fit@Fit
    fit.dt <- data.frame(
      FixIndex = c("logLik","value","df","pvalue","AIC","BIC","SABIC"),
      value= c(Fit$logLik,Fit$G2,Fit$df,Fit$p,Fit$AIC,Fit$BIC,Fit$SABIC))
  }

  fit.dt$value <- round(fit.dt$value, 3)
  rownames(fit.dt)<-NULL

  return(fit.dt)
}


#' clean mirt class
#'
#' @param mirt.fit an obejct from \code{\link{mirt}} package
#' @noRd
mirt_output_cleaning <- function(mirt.fit) {

  `.` <- varname <- parname <- est <- z <- rhs <- lhs <- op <- se <- NULL

  mirt.param <- coef(mirt.fit, simplify = F, printSE=TRUE)

  EST <- lapply(mirt.param[-length(mirt.param)], function(x) x["par",]) %>%
    do.call("rbind",.) %>%
    data.frame(., varname = rownames(.)) %>%
    gather("parname","est", -varname) %>%
    mutate(est = ifelse(str_detect(parname, "d"), -est, est))

  SE <- lapply(mirt.param[-length(mirt.param)], function(x) x["SE",]) %>%
    do.call("rbind",.) %>%
    data.frame(., varname = rownames(.)) %>%
    gather("parname","se", -varname)

  EST <- EST %>%
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
    # select(-varname, -parname) %>%
    mutate_if(is.numeric, ~ round(., 3))

  new_lhs <- EST %>% filter(str_detect(parname, "a")) %>% pull(parname) %>%
    parse_number(.) %>% paste0("F",.) %>%
    c(.,EST %>% filter(!str_detect(parname, "a")) %>% pull(parname))

  EST$lhs <- new_lhs

  EST %>% filter(!is.na(se))
}

#' clean lavaan class
#'
#' @param lav.fit an obejct from \code{\link{lavaan}} package
#' @noRd
lavaan_output_cleaning <- function(lav.fit) {
  op <- NULL

  parameterestimates(lav.fit) %>%
    filter(str_detect(op, "=~|\\|")) %>%
    dplyr::select(-matches("^ci"))
}
