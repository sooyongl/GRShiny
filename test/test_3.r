rm(list=ls())
for (i in fs::dir_ls("R")) {source(i)}

orddata <- fread("test/paper_data/BYI_DEMO.DAT")[,-c(1:4)]

write_csv(orddata, "test/paper_data/grm_dt.csv")

lav.model <- genLavSyn(orddata)
cat(lav.model)

a1 <- runGRM(dat = orddata, lav.syntax = lav.model, estimator = "ML")
coef(a1$mirt.fit, simplify = T)
mirt_out_cleaning(a1$mirt.fit)
a1$grm.par




a2 <- runGRM(orddata, lav.model, "WL")
summary(a2$lav.fit)
lavaan_out_cleaning(a2$lav.fit)
a2$grm.par



