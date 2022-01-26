# ref: https://aidenloe.github.io/irtplots.html

## Calculate Factor score --------------------------------------------------
calFS <- function(fit) {
  res.fit <- fit[grep("fit",names(fit))][[1]]
  if(class(res.fit) == "lavaan") {
    fs_scores <- lavaan::lavPredict(res.fit)
  } else {
    fs_scores <- mirt::fscores(res.fit)
  }
  data.frame(fs_scores)
}

# FSplot(fs_scores, type = "histogram", hist_bins = 20, base_size = 16)
# FSplot(fs_scores, type = "density", hist_bins = 20, base_size = 16)
FSplot <- function(fit, type = "histogram", hist_bins = 20, base_size = 16) {

  fs_scores <- calFS(fit)


  if(!is.data.frame(fs_scores))
    fs_scores <- data.frame(fs_scores)

  names(fs_scores) <- paste0("F", 1:ncol(fs_scores))

  if(grepl("hist", tolower(type))) {
    p <- fs_scores %>%
      ggplot(aes(x = F1)) +
      geom_histogram(bins = hist_bins, fill = "grey70", colour = "white")

  } else {
    p <- fs_scores %>%
      ggplot(aes(x = F1)) +
      geom_density(fill = "grey70", alpha = 1)
  }

  p +
    theme_bw(base_size = base_size)
}


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

# ICCplot(ipar, 1, seq(-3, 3, .1), plot.ps = FALSE, base_size = 16)
# ICCplot(ipar, selected_item=2:4, theta=seq(-3, 3, .1), plot.ps = T, base_size = 16)
ICCplot <- function(fit, selected_item, theta = seq(-3, 3, .1), plot.ps = FALSE, addlabel = F, base_size = 16) {
  # plot.ps=F : Categorical response curve
  # plot.ps=T : operating characteristic curve plot

  ipar <- fit$grm.par

  ipar <- data.frame(ipar)
  b <- ipar[grep("^b", names(ipar))]

  selected_ipar <- ipar %>%
    data.frame(iid = paste0("item", 1:nrow(ipar))) %>%
    slice(selected_item)

  prob_dt <- selected_ipar %>%
    group_split(iid) %>%
    map(., ~ calProb(.x, theta)) %>%
    set_names(paste0("item", selected_item))

  if (plot.ps) {

    selected_ipar <- selected_ipar %>%
      mutate_if(is.numeric, ~ round(., 2)) %>%
      rename("item"="iid") %>%
      gather("b", "val", -item) %>%
      filter(str_detect(b, "b"))


    prob_dt <- prob_dt %>%
      map(., ~ .x$ps %>%
            data.frame(theta = theta) %>%
            select(-z, -zz) %>%
            gather("step","prob+", -theta)) %>%
      bind_rows(.id = "item")

    # x_breaks = seq(from = min(theta), to = max(theta), by = 0.5)
    x_breaks = round(seq(from = min(theta), to = max(theta), length.out = 10),2)

    used_b <- round(b[selected_item,],2)
    used_b$item <- paste0("item", parse_number(rownames(used_b)))
    used_b <- used_b %>% gather("b","val", -item)
    used_b <- used_b %>% mutate(ye = 0.5, ys = 0, xs = 0)

    yseg <- used_b %>%
      group_by(item) %>%
      filter(val ==max(val)) %>%
      mutate(ys = 0.5, xs = min(theta)) %>% ungroup()

    p <- prob_dt %>%
      ggplot(aes(x = theta, y = `prob+`)) +
      geom_line(aes(color = step)) +
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
      )

    if(addlabel) {
      p <- p +
        geom_label(data = selected_ipar,
                   aes(label = val, x = val, y = 0))
    }

    p <-  p +
      scale_color_viridis_d(option = "H") +
      labs(title = "Operating characteristic curve",
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

ESplot <- function(fit, selected_item, theta, base_size = 16) {

  ipar <- fit$grm.par

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

# infoPlot(ipar, selected_item=1:4, type = "icc", base_size = 16)
# infoPlot(ipar, selected_item=1:2, type = "tcc", base_size = 16)
infoPlot <- function(fit, selected_item, type = "icc", theta = seq(-3, 3, .1), base_size = 16) {

  ipar <- fit$grm.par

  if(tolower(type) == "icc") {
    ipar <- data.frame(ipar) %>%
      data.frame(iid = paste0("item", 1:nrow(ipar))) %>%
      slice(selected_item)
  } else {
    ipar <- data.frame(ipar) %>%
      data.frame(iid = paste0("item", 1:nrow(ipar)))
    selected_item <- 1:nrow(ipar)
  }

  info_dt <- ipar %>%
    group_split(iid) %>%
    map(., ~ calInfo(.x, theta)) %>%
    set_names(paste0("item", selected_item)) %>%
    do.call("cbind", .) %>%
    data.frame(theta = theta) %>%
    gather("item","info", -theta)

  x_breaks = round(seq(from = min(theta), to = max(theta), length.out = 10),2)

  if(tolower(type) == "icc") {
    p <- info_dt %>%
      ggplot(aes(x = theta, y = info, colour = item)) +
      geom_line() +
      labs(title = "Information",
           x = expression(theta~('ability on the logit scale')),
           y = expression(italic(I)~(theta)),
           colour = "")
  } else {

    p <- info_dt %>%
      group_by(theta) %>%
      summarise(total.info = sum(info)) %>%
      ggplot(aes(x = theta, y = total.info)) +
      geom_line() +
      labs(title = "Total Information",
           x = expression(theta~('ability on the logit scale')),
           y = expression(italic(TI)~(theta)),
           colour = "")
  }
  p  +
    scale_color_viridis_d(option = "H") +
    scale_x_continuous(breaks = x_breaks) +
    theme_classic(base_size = base_size)
}
