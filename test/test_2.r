rm(list=ls())
for (i in fs::dir_ls("R")) {source(i)}

ipar <- genIRTpar(4, ncat = 3, 1)
eta <- MASS::mvrnorm(500, rep(0, 1), matrix(c(1), ncol = 1))
orddata <- genData(eta, ipar)


# write_csv(orddata, "test/grm_dt.csv", col_names = F)
# write_tsv(orddata, "test/grm_dt.dat", col_names = F)
#
# fread("test/grm_dt.dat")


lav.model <- genLavSyn(orddata)
cat(lav.model)


lav.model <- str_replace_all(lav.model, "\\+l[0-9]\\*|\\+l[0-9][0-9]\\*|\\+l[0-9][0-9][0-9]\\*", "+")

lav.model <- str_remove_all(lav.model, " t[0-9][0-9]\\*| t[0-9][0-9][0-9]\\*| t[0-9][0-9][0-9][0-9]\\*")

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

lav.model <- mplus2lavaan.modelSyntax(strsplit(modeltext, "\n")[[1]])
lav.model <- str_replace(lav.model, "start\\(1\\)", "NA")
# first_item <- strsplit(strsplit(lav.model, "NA\\*")[[1]][2], " \\+ ")[[1]][1]


a1 <- runGRM(dat = orddata, lav.syntax = lav.model, estimator = "ML")
a1[[1]]

summary(a1$mirt.fit$mirt)


mirt_out_cleaning(a1$mirt.fit)


a2 <- runGRM(orddata, lav.model, "WL")


lavaan_out_cleaning(a2$lav.fit)



summary(a2[[1]])
