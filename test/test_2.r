rm(list=ls())
for (i in fs::dir_ls("R")) {source(i)}

library(lavaan)
library(mirt)
library(sirt)
library(tidyverse)
ipar <- genIRTpar(4, ncat = 3, 1)
eta <- MASS::mvrnorm(500, rep(0, 1), matrix(c(1), ncol = 1))
orddata <- genData(eta, ipar)


# write_csv(orddata, "test/grm_dt.csv", col_names = F)
# write_tsv(orddata, "test/grm_dt.dat", col_names = F)
#
# fread("test/grm_dt.dat")

orddata <- fread("test/paper_data/BYI_DEMO.DAT")[,-c(1:4)]


lav.model <- genLavSyn(orddata)
cat(lav.model)

# lav.model <- str_replace_all(lav.model, "\\+l[0-9]\\*|\\+l[0-9][0-9]\\*|\\+l[0-9][0-9][0-9]\\*", "+")
#
# lav.model <- str_remove_all(lav.model, " t[0-9][0-9]\\*| t[0-9][0-9][0-9]\\*| t[0-9][0-9][0-9][0-9]\\*")
# cat(lav.model)

lav.model <- "F =~ NA*y1+y2+y3+y4

 F ~~ 1*F;
 F ~ 0*1;

y1 | t1;
y2 | t1;
y3 | t1;
y4 | t1;
y1 | t2;
y2 | t2;
y3 | t2;
y4 | t2;"

modeltext <- paste(c("F1 by y1*",
                     "y2-y4;",
                     "F1@1;",
                     "[F1@0];"), collapse = "\n")

modeltext <- readLines("test/paper_data/mplus run_wlsmv.inp")
modeltext <- paste(modeltext, collapse = "\n")
cat(modeltext)

lav.model <- mplus2lavaan.modelSyntax(strsplit(modeltext, "\n")[[1]])
lav.model <- str_replace(lav.model, "start\\(1\\)", "NA")
cat(lav.model)
# first_item <- strsplit(strsplit(lav.model, "NA\\*")[[1]][2], " \\+ ")[[1]][1]


a1 <- runGRM(dat = orddata, lav.syntax = lav.model, estimator = "ML")
a1[[1]]
summary(a1$mirt.fit)

output_cleaning(a1)
extract_fit(a1)

extract_fit <- function(fit) {
        # fit = a1
        fit <- fit[grep("fit",names(fit))][[1]]

        if(class(fit) == "lavaan") {

                Fit <- fitMeasures(fit)

                fit.dt <- data.frame(
                        fname = c("logLik","df","CFI","TLI","RMSEA","AIC","BIC"),
                        value = c(Fit["logl"],Fit["df"],Fit["cfi"],Fit["tli"],Fit["cfi"],Fit["rmsea"],Fit["aic"],Fit["bic"])
                )


        } else {
                Fit <- fit@Fit
                fit.dt <- data.frame(
                        fname = c("logLik","df","CFI","TLI","RMSEA","AIC","BIC"),
                        value= c(Fit$logLik,Fit$df,Fit$CFI,Fit$TLI,Fit$RMSEA,Fit$AIC,Fit$BIC))
        }


        return(fit.dt)
}
mirt_out_cleaning(a1$mirt.fit)
a1$grm.par

a1 <- runGRM(orddata, lav.model, "WL")
output_cleaning(fit = a1)
extract_fit(a1)
a1$grm.par
















