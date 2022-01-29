library(hexSticker)
library(GRShiny)
library(ggplot2)

# ipar <- genIRTpar(10, ncat = 4, 1)
# eta <- genTheta(200, 1)
# orddata <- genData(eta, ipar)
orddata <- data.table::fread("test/paper_data/BYI_DEMO.DAT")[,-c(1:4)]

names(orddata)[1:2] <- c("item1","item2")

lav.model <- genLavSyn(orddata)
a1 <- runGRM(dat = orddata, lav.syntax = lav.model, estimator = "ML")
p1 <- ICCplot(a1, 1:2,seq(-6, 6, .1), plot.ps = F, 16, addlabel = F)
p1 <- p1 + labs(title = "") + theme_transparent() + theme_void()

# str(p1)
p1 <- p1 + theme(legend.position = "none")
#
# p1$layers[[2]] <- NULL
# p1$layers[[2]] <- NULL
#
# str(p1)
#
# p2 <- p1

sticker(p1, package="GRShiny",
        p_size=20, s_x=1, s_y=.8, s_width=1, s_height=1,
        filename="inst/figures/hextile.png")
