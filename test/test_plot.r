rm(list=ls())
for (i in fs::dir_ls("R")) {source(i)}

ipar <- genIRTpar(10, ncat = 3, 1)
eta <- MASS::mvrnorm(500, rep(0, 1), matrix(c(1), ncol = 1))
orddata <- genData(eta, ipar)
lav.model <- genLavSyn(orddata)
cat(lav.model)

a1 <- runGRM(dat = orddata, lav.syntax = lav.model, estimator = "ML")
a1[[1]]
summary(a1$mirt.fit)

sempars <- coef(a1$mirt.fit, simplify = T, IRTpars = F)$items
ipar <- grmpars <- coef(a1$mirt.fit, simplify = T, IRTpars = T)$items

mirt_out_cleaning(a1$mirt.fit)
a1$grm.par

calFS(fit=a1)
calFS <- function(fit) {
  res.fit <- fit[grep("fit",names(fit))]
  if(class(res.fit) == "lavaan") {
    fs_scores <- lavaan::lavPredict(res.fit)
  } else {
    fs_scores <- mirt::fscores(res.fit)
  }

  fs_scores

}





a2 <- runGRM(orddata, lav.model, "WL")
lavaan_out_cleaning(a2$lav.fit)
summary(a2[[1]])
a2$grm.par

itemplot(a1$mirt.fit, 1)
plot_GR(ipar = grmpars,
        selected_item = 1,
        theta = seq(-6, 6, .1),
        plot.ps = F)


Information_GR(ipar = grmpars,
               selected_item = 1:4,
               theta = seq(-3, 3, .1))
plot(a1$mirt.fit, type = 'infotrace', theta_lim =c(-4,4), facet_items = FALSE)


# tes ICC or IIC TIC
# https://solomonkurz.netlify.app/post/2021-06-29-make-icc-plots-for-your-brms-irt-models/
# https://gist.github.com/Nygator8/5f87085dfaa5689d97f3d284050f7d6f


## Calculate probability ---------------------------------------------------
calProb <- function(ipar, theta = seq(-3, 3, .1)) {

  ipar <- data.frame(ipar)

  a <- unlist(ipar[grep("a", names(ipar))])
  b <- unlist(ipar[grep("^b", names(ipar))])

  nq = length(theta)
  ncat = length(b) + 1
  prob = matrix(NA, nq, ncat)
  ps = matrix(NA, nq, ncat + 1)
  ps[, 1] = 1
  ps[, ncat + 1] = 0
  for (k in 1:(ncat - 1)) {
    ps[, k + 1] = (1 + exp(-a * (theta - b[k])))^-1
  }
  for (k in 1:ncat) {
    prob[, k] = ps[, k] - ps[, k + 1]
  }

  colnames(prob) <- paste0("k", 1:ncol(prob))
  colnames(ps) <- c("z",paste0("b", 1:(ncol(ps)-2)),"zz")

  return(list(prob = prob, ps = ps))
}

ICCplot <- function(ipar, selected_item, theta = seq(-3, 3, .1), plot.ps = FALSE, base_size = 16) {

  ipar <- data.frame(ipar)
  b <- ipar[grep("^b", names(ipar))]

  prob_dt <- ipar %>%
    data.frame(iid = paste0("item", 1:nrow(ipar))) %>%
    slice(selected_item) %>%
    group_split(iid) %>%
    map(., ~ calProb(.x, theta)) %>%
    set_names(paste0("item", selected_item))

  if (plot.ps) {

    prob_dt <- prob_dt %>%
      map(., ~ .x$ps %>%
            data.frame(theta = theta) %>%
            select(-z, -zz) %>%
            gather("step","prob+", -theta)) %>%
      bind_rows(.id = "item")

    # x_breaks = seq(from = min(theta), to = max(theta), by = 0.5)
    x_breaks = round(seq(from = min(theta), to = max(theta), length.out = 10),2)

    used_b <- round(b[selected_item,],2)
    used_b$item <- paste0("item", 1:nrow(used_b))
    used_b <- used_b %>% gather("b","val", -item)
    used_b <- used_b %>% mutate(ye = 0.5, ys = 0, xs = 0)

    yseg <- used_b %>%
      group_by(item) %>%
      filter(val ==max(val)) %>%
      mutate(ys = 0.5, xs = min(theta)) %>% ungroup()

    p <- prob_dt %>%
      ggplot(aes(x = theta, y = `prob+`, color = step)) +
      geom_line() +
      facet_wrap(~item) +
      # geom_vline(xintercept = b) +
      geom_segment(
        data = used_b,
        aes(x=val, xend = val, y = ys, yend = ye),
        lty = "dashed",
        colour = "grey30"
      ) +
      geom_segment(
        data = yseg,
        aes(x=xs, xend = val, y = ys, yend = ye),
        lty = "dashed",
        colour = "grey30"
      ) +

      scale_color_viridis_d(option = "H") +
      labs(title = "Prob+ curve",
           # subtitle = "Each curve is for the probs of each category",
           x = expression(theta~('ability on the logit scale')),
           y = expression(italic(p)),
           colour = "") #expression(italic(p)(y==1))) +
    scale_x_continuous(breaks = x_breaks)

  } else {
    prob_dt <- prob_dt %>%
      map(., ~ .x$prob %>%
            data.frame() %>%
            gather("cate","p")) %>%
      bind_rows(.id = "item")

    x_breaks = round(seq(from = min(theta), to = max(theta), length.out = 10),2)

    p <- prob_dt %>%
      data.frame(theta = theta) %>%
      ggplot(aes(x = theta, y = p, color = cate)) +
      geom_line() +
      scale_color_viridis_d(option = "H") +
      facet_wrap(~item) +
      labs(title = "ICC",
           # subtitle = "Each curve is for the probs of each category",
           x = expression(theta~('ability on the logit scale')),
           y = expression(italic(p)),
           colour = "") +
      scale_x_continuous(breaks = x_breaks)
  }

  p + theme_classic(base_size = base_size)
}

# Expected scores --------------------------------------------------------
calES = function(ipar,theta = seq(-3, 3, .1)) {

  ipar <- data.frame(ipar)
  a <- unlist(ipar[grep("a", names(ipar))])
  b <- unlist(ipar[grep("^b", names(ipar))])

  prob = calProb(ipar, theta = theta)$prob
  ES = as.vector(prob %*% matrix(0:length(b), length(b) + 1))
  ES
}

ESplot <- function(ipar, selected_item, theta, base_size = 16) {

  ipar <- data.frame(ipar)
  exscore_dt <- ipar %>%
    data.frame(iid = paste0("item", 1:nrow(ipar))) %>%
    slice(selected_item) %>%
    group_split(iid) %>%
    map(., ~ calES(.x, theta)) %>%
    set_names(paste0("item", selected_item)) %>%
    do.call("cbind", .) %>%
    data.frame(theta = theta) %>%
    gather("item","score", -theta)

  x_breaks = round(seq(from = min(theta), to = max(theta), length.out = 10),2)

  exscore_dt %>%
    ggplot(aes(x = theta, y = score, colour = item)) +
    geom_line() +
    scale_color_viridis_d(option = "H") +
    labs(title = "Expected score",
         x = expression(theta~('ability on the logit scale')),
         y = expression(italic(Expected)~ italic(Score)),
         colour = "") +
    scale_x_continuous(breaks = x_breaks) +
    theme_classic(base_size = base_size)
}
ExScore_GR(ipar, c(1,2,3,4))

# Item information --------------------------------------------------------
calInfo = function(ipar, theta = seq(-3, 3, .1)) {

  ipar <- data.frame(ipar)
  a <- unlist(ipar[grep("a", names(ipar))])
  b <- unlist(ipar[grep("^b", names(ipar))])

  nq = length(theta)
  ncat = length(b) + 1
  ps = calProb(ipar, theta = theta)$ps
  info = numeric(nq)
  for (k in 1:ncat) {
    info = info + (ps[, k] - ps[, k + 1]) * (1 - ps[, k] - ps[, k + 1])^2
  }
  info = a^2 * info
  return(info)
}

infoPlot <- function(ipar, selected_item, theta, base_size = 16) {

  ipar <- data.frame(ipar)
  info_dt <- ipar %>%
    data.frame(iid = paste0("item", 1:nrow(ipar))) %>%
    slice(selected_item) %>%
    group_split(iid) %>%
    map(., ~ calInfo(.x, theta)) %>%
    set_names(paste0("item", selected_item)) %>%
    do.call("cbind", .) %>%
    data.frame(theta = theta) %>%
    gather("item","info", -theta)

  x_breaks = round(seq(from = min(theta), to = max(theta), length.out = 10),2)

  info_dt %>%
    ggplot(aes(x = theta, y = info, colour = item)) +
    geom_line() +
    scale_color_viridis_d(option = "H") +
    labs(title = "Information",
         x = expression(theta~('ability on the logit scale')),
         y = expression(italic(I)~(theta)),
         colour = "") +
    scale_x_continuous(breaks = x_breaks) +
    theme_classic(base_size = base_size)
}



# -------------------------------------------------------------------------
# cINFO_GR = function(a, b, theta = seq(-3, 3, .1), plot.I = FALSE, title = "") {
#   nq = length(theta)
#   ncat = length(b) + 1
#   pp = calProb(a, b, theta = theta)
#   prob = pp$prob
#   ps = pp$ps
#   info = INFO_GR(a, b, theta = theta)
#   cInfo = matrix(0, nq, ncat)
#   for (k in 1:ncat) {
#     cInfo[, k] = a * (ps[, k] * (1 - ps[, k]) - ps[, k + 1] * (1 - ps[, k + 1]))^2 / prob[, k]
#   }
#   if (plot.I) {
#     plot(theta, info, xlab = "theta", ylab = "Info", type = "l", ylim = c(0, max(info)), main = title)
#     for (k in 1:(length(b) + 1)) {
#       lines(theta, cInfo[, k], col = k)
#     }
#   } else {
#     return(cInfo)
#   }
# }
#
# par(mfrow = c(1, 2))
# cINFO_GR(1, c(-1, 0, 1), theta = seq(-3, 3, .1), plot.I = TRUE, title = "a = 1")
# cINFO_GR(2, c(-1, 0, 1), theta = seq(-3, 3, .1), plot.I = TRUE, title = "a = 2")

# -------------------------------------------------------------------------

# function(theta_range = NULL, pick_item = NULL, oneplot = T){}

# post <- post %>%
#   expand(nesting(iter, b_eta_Intercept, b_logalpha_Intercept, item, xi, logalpha),
#          theta = seq(from = -6, to = 6, length.out = 100)) %>%
#   # note the difference in the equation
#   mutate(p = inv_logit_scaled(exp(b_logalpha_Intercept + logalpha) * (b_eta_Intercept + theta + xi))) %>%
#   group_by(theta, item) %>%
#   summarise(p = mean(p))
#
# ## ICC
# post %>%
#   ggplot(aes(x = theta, y = p, color = item)) +
#   geom_line() +
#   scale_color_viridis_d(option = "H") +
#   labs(title = "ICCs for the 1PL",
#        subtitle = "Each curve is based on the posterior mean.",
#        x = expression(theta~('ability on the logit scale')),
#        y = expression(italic(p)(y==1))) +
#   theme_classic()
#
# ## IIC
# # these wrangling steps are all the same as before
# posterior_samples(irt2) %>%
#   select(b_eta_Intercept, b_logalpha_Intercept, starts_with("r_item")) %>%
#   mutate(iter = 1:n()) %>%
#   pivot_longer(starts_with("r_item")) %>%
#   mutate(item      = str_extract(name, "\\d+"),
#          parameter = ifelse(str_detect(name, "eta"), "xi", "logalpha")) %>%
#   select(-name) %>%
#   pivot_wider(names_from = parameter, values_from = value) %>%
#   expand(nesting(iter, b_eta_Intercept, b_logalpha_Intercept, item, xi, logalpha),
#          theta = seq(from = -6, to = 6, length.out = 200)) %>%
#   mutate(p = inv_logit_scaled(exp(b_logalpha_Intercept + logalpha) * (b_eta_Intercept + theta + xi))) %>%
#
#   # again, here's the new part
#   mutate(i = p * (1 - p)) %>%
#   group_by(theta, item) %>%
#   summarise(i = median(i)) %>%
#
#   # now plot!
#   ggplot(aes(x = theta, y = i, color = item)) +
#   geom_line() +
#   scale_color_viridis_d(option = "H") +
#   labs(title = "IICs for the 2PL",
#        subtitle = "Each curve is based on the posterior median.",
#        x = expression(theta~('ability on the logit scale')),
#        y = "information") +
#   theme_classic()
#
# ## TIC
# posterior_samples(irt1) %>%
#   select(b_Intercept, starts_with("r_item")) %>%
#   mutate(iter = 1:n()) %>%
#   pivot_longer(starts_with("r_item"), names_to = "item", values_to = "xi") %>%
#   mutate(item = str_extract(item, "\\d+")) %>%
#   expand(nesting(iter, b_Intercept, item, xi),
#          theta = seq(from = -6, to = 6, length.out = 200)) %>%
#   mutate(p = inv_logit_scaled(b_Intercept + xi + theta)) %>%
#   mutate(i = p * (1 - p)) %>%
#
#   # this is where the TIC magic happens
#   group_by(theta, iter) %>%
#   summarise(sum_i = sum(i)) %>%
#   group_by(theta) %>%
#   summarise(i = median(sum_i)) %>%
#
#   # we plot
#   ggplot(aes(x = theta, y = i)) +
#   geom_line() +
#   labs(title = "The test information curve for the 1PL",
#        subtitle = "The curve is based on the posterior median.",
#        x = expression(theta~('ability on the logit scale')),
#        y = "information") +
#   theme_classic()
