fit = a1
selected_item = 1
theta <- seq(-4, 4, .1)

ESplot
function(fit, selected_item, theta = seq(-4, 4, 0.1), base_size = 16, line_size=1, cal_option = "D") {

  iid <- `.` <- score <- item <- F1 <- NULL

  ipar <- fit$grm.par
  selected_id <- rownames(ipar)[selected_item]

  ipar <- data.frame(ipar)
  exscore_dt <- ipar %>%
    data.frame(iid = rownames(ipar)) %>%
    slice(selected_item) %>%
    group_split(iid)


  calES <- function(ipar,theta = seq(-4, 4, 0.1)) {
    # ipar = exscore_dt[[1]]
    ipar <- data.frame(ipar)
    a <- unlist(ipar[grep("a", names(ipar))])
    b <- unlist(ipar[grep("^b", names(ipar))])


    calProb <-  function(ipar, theta = seq(-4, 4, 0.1)) {

      # ipar <- data.frame(ipar)
      #
      # a <- unlist(ipar[grep("a", names(ipar))])
      # b <- unlist(ipar[grep("^b", names(ipar))])

      nq   <- length(theta)
      ncat <- length(b) + 1

      ps <- sapply(1:(ncat - 1), function(k) {(1 + exp(-a * (theta - b[k])))^-1})
      ps   <- cbind(1, ps ,0)

      prob <- sapply(1:(ncat), function(k) {ps[, k] - ps[, k + 1]})

      colnames(prob) <- paste0("k", 1:ncol(prob))
      colnames(ps) <- c("z",paste0("b", 1:(ncol(ps)-2)),"zz")

      return(list(prob = prob, ps = ps))
    }

    prob = calProb(ipar, theta = theta)$prob
    ES = as.vector(prob %*% matrix(0:length(b), length(b) + 1))
    ES
  }
  exscore_dt %>%
    map(., ~ calES(.x, theta)) %>%
    set_names(selected_id) %>%
    do.call("cbind", .) %>%
    data.frame(theta = theta) %>%
    gather("item","score", -theta)

  x_breaks = c(0,round(seq(from = min(theta), to = max(theta), length.out = 9),1))

  exscore_dt %>%
    ggplot(aes(x = theta, y = score, colour = item)) +
    geom_line(size = line_size) +
    scale_color_viridis_d(option = cal_option) +
    labs(title = "Expected score",
         x = expression(theta~('ability on the logit scale')),
         y = expression(italic(Expected)~ italic(Score)),
         colour = "") +
    scale_x_continuous(breaks = x_breaks) +
    theme_bw(base_size = base_size)
}

