#' @include GRShiny-package.r
NULL

# ref: https://aidenloe.github.io/irtplots.html

#' Calculate Factor score
#'
#' @param fit an object from \code{\link{runGRM}}
#' @noRd
calFS <- function(fit) {
  res.fit <- fit[grep("fit",names(fit))][[1]]


  if(inherits(res.fit, "lavaan")) {
    fs_scores <- lavaan::lavPredict(res.fit)
  } else {
    fs_scores <- mirt::fscores(res.fit)
  }
  data.frame(fs_scores)
}

#' Plot Factor score
#'
#' @param fit an object from \code{\link{runGRM}}
#' @param type a character indicating the type of plots
#' \itemize{
#' \item{\code{histogram}} Histogram plot
#' \item{\code{density}} Density plot
#' }
#' @param hist_bins a numeric indicating the number of bins for the histogram
#' @param fill_colour a character indicating the color (default = \code{grey70})
#' @param base_size a numeric indicating the base font size
#'
#' @return a \code{\link{ggplot}} object.
#'
#' @details This makes either histogram or density plot for individual factor
#'  scores.
#'
#' @examples
#' grm_dt <- genData(eta = genTheta(nsample = 500, nfac = 1),
#'                  ipar = genIRTpar(nitem = 10, ncat = 3, nfac = 1))
#'
#' fit <- runGRM(dat = grm_dt,
#'             lav.syntax = genLavSyn(dat = grm_dt, nfac = 1),
#'             estimator = "WL")
#' FSplot(fit, type = "histogram", hist_bins = 20, base_size = 16)
#'
#' @export
FSplot <- function(fit, type = "histogram", hist_bins = 20, fill_colour = "grey70", base_size = 16) {
  F1 <- NULL

  fs_scores <- calFS(fit)


  if(!is.data.frame(fs_scores))
    fs_scores <- data.frame(fs_scores)

  names(fs_scores) <- paste0("F", 1:ncol(fs_scores))

  if(grepl("hist", tolower(type))) {
    p <- fs_scores %>%
      ggplot(aes(x = F1)) +
      geom_histogram(bins = hist_bins, fill = fill_colour, colour = "white")

  } else {
    p <- fs_scores %>%
      ggplot(aes(x = F1)) +
      geom_density(fill = fill_colour, alpha = 1)
  }

  p +
    theme_bw(base_size = base_size)
}


#' Calculate probability
#'
#' @param ipar a data frame containing estimated GRM parameters
#' @param theta a numeric indicating the range of theta
#' @noRd
calProb <- function(ipar, theta = seq(-4, 4, 0.1)) {

  ipar <- data.frame(ipar)

  a <- unlist(ipar[grep("a", names(ipar))])
  b <- unlist(ipar[grep("^b", names(ipar))])

  nq   <- length(theta)
  ncat <- length(b) + 1

  ps <- sapply(1:(ncat - 1), function(k) {(1 + exp(-a * (theta - b[k])))^-1})
  ps   <- cbind(1, ps ,0)

  prob <- sapply(1:(ncat), function(k) {ps[, k] - ps[, k + 1]})

  colnames(prob) <- paste0("c", 1:ncol(prob))
  colnames(ps) <- c("z",paste0("b", 1:(ncol(ps)-2)),"zz")

  return(list(prob = prob, ps = ps))
}

#' Plot ICC or OCC
#'
#' @param fit an object from \code{\link{runGRM}}
#' @param selected_item a numeric indicating for what items the function makes plots
#' @param theta a numeric indicating latent traits
#' @param plot.occ a logical. If TURE, OCC is made instead of ICC
#' @param addlabel a logical indicating whether to add the b parameter as labels
#' @param base_size a numeric indicating the base font size
#' @param line_size a numeric indicating the size of line
#' @param cal_option a character indicating the plot color specified in
#'  \code{\link{scale_color_viridis_d}} (default = \code{D})
#'
#' @return a \code{\link{ggplot}} object.
#'
#' @details This makes either item characteristic curve plots or
#' operating characteristic curve plots
#'
#' @examples
#'
#' grm_dt <- genData(eta = genTheta(nsample = 500, nfac = 1),
#'                  ipar = genIRTpar(nitem = 10, ncat = 3, nfac = 1))
#'
#' fit <- runGRM(dat = grm_dt,
#'             lav.syntax = genLavSyn(dat = grm_dt, nfac = 1),
#'             estimator = "WL")
#' ICCplot(fit, 1, seq(-3, 3, .1), plot.occ = FALSE, base_size = 16)
#'
#' @export
ICCplot <- function(fit, selected_item, theta = seq(-4, 4, 0.1), plot.occ = FALSE, addlabel = F, base_size = 16, line_size = 1, cal_option = "D") {
  # plot.occ=F : Categorical response curve
  # plot.occ=T : operating characteristic curve plot

  iid <- `.` <- score <- item <- val <- `prob+` <- step <- ys <- ye <- xs <- cate <- NULL

  if(inherits(fit[[1]], "SingleGroupClass")) {
    vname <- names(coef(fit$mirt.fit))
    vname <- vname[-length(vname)]
  } else {

    vname <- rownames(lavInspect(fit$lav.fit)$lambda)
  }

  ipar <- fit$grm.par
  rownames(ipar) <- vname
  # ipar <- fit$grm.par

  ipar <- data.frame(ipar)
  b <- ipar[grep("^b", names(ipar))]

  selected_ipar <- ipar %>%
    data.frame(iid = rownames(ipar)) %>%
    slice(selected_item) #%>%

  selected_ipar <- selected_ipar %>%
    mutate(iid = factor(iid, levels = unique(selected_ipar$iid)))
    # arrange(iid)
  selected_id <- selected_ipar$iid

  prob_dt <- selected_ipar %>%
    group_split(iid) %>%
    map(., ~ calProb(.x, theta)) %>%
    set_names(selected_id)

  if (plot.occ) {

    selected_ipar <- selected_ipar %>%
      mutate_if(is.numeric, ~ round(., 2)) %>%
      rename("item"="iid") %>%
      gather("b", "val", -item) %>%
      filter(str_detect(b, "b"))

    prob_dt <- prob_dt %>%
      map(., ~ .x$ps %>%
            data.frame(theta = theta) %>%
            dplyr::select(-z, -zz) %>%
            gather("step","prob+", -theta)) %>%
      bind_rows(.id = "item")

    # x_breaks = seq(from = min(theta), to = max(theta), by = 0.5)
    x_breaks = c(0,round(seq(from = min(theta), to = max(theta), length.out = 9),1))

    used_b <- round(b[selected_item,],2)
    used_b$item <- selected_id

    used_b <- used_b %>% gather("b","val", -item)
    used_b <- used_b %>% mutate(ye = 0.5, ys = 0, xs = 0)

    yseg <- used_b %>%
      group_by(item) %>%
      filter(val ==max(val)) %>%
      mutate(ys = 0.5, xs = min(theta)) %>% ungroup()

    p <- prob_dt %>%
      ggplot(aes(x = theta, y = `prob+`)) +
      geom_line(aes(color = step, linetype = step), linewidth = line_size) +
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
      facet_wrap(~item)

    if(addlabel) {
      p <- p +
        geom_label(data = selected_ipar,
                   aes(label = val, x = val, y = 0))
    }

    p <-  p +
      scale_color_viridis_d(option = cal_option) +
      labs(title = "Operating characteristic curve",
           x = expression(theta~('ability on the logit scale')),
           y = expression(italic(p)),
           colour = "",
           linetype = "") + #expression(italic(p)(y==1))) +
      scale_x_continuous(breaks = x_breaks)

  } else {
    prob_dt <- prob_dt %>%
      map(., ~ .x$prob %>%
            data.frame() %>%
            gather("cate","p")) %>%
      bind_rows(.id = "item")

    x_breaks = c(0,round(seq(from = min(theta), to = max(theta), length.out = 9),1))

    p <- prob_dt %>%
      data.frame(theta = theta) %>%
      ggplot(aes(x = theta, y = p)) +
      geom_line(aes(color = cate, linetype = cate), linewidth = line_size) +
      scale_color_viridis_d(option = cal_option) +
      facet_wrap(~item) +
      labs(title = "ICC",
           # subtitle = "Each curve is for the probs of each category",
           x = expression(theta~('ability on the logit scale')),
           y = expression(italic(p)),
           colour = "",
           linetype = "") +
      scale_x_continuous(breaks = x_breaks)
  }

  if(length(selected_item) == 1)
    p <- p + facet_null()

  p + theme_bw(base_size = base_size)
}

#' Calculate expected scores
#'
#' @param ipar a data frame containing estimated GRM parameters
#' @param theta a numeric indicating the range of theta
#' @noRd
calES = function(ipar,theta = seq(-4, 4, 0.1)) {

  ipar <- data.frame(ipar)
  a <- unlist(ipar[grep("a", names(ipar))])
  b <- unlist(ipar[grep("^b", names(ipar))])

  prob = calProb(ipar, theta = theta)$prob
  ES = as.vector(prob %*% matrix(0:length(b), length(b) + 1))
  ES
}

#' Plot expected scores by items
#'
#' @param fit an object from \code{\link{runGRM}}
#' @param selected_item a numeric indicating for what items the function makes plots
#' @param theta a numeric indicating latent traits
#' @param base_size a numeric indicating the base font size
#' @param line_size a numeric indicating the size of line
#' @param cal_option a character indicating the plot color specified in
#'  \code{\link{scale_color_viridis_d}} (default = \code{D})
#'
#' @return a \code{\link{ggplot}} object.
#'
#' @details This makes a expected score plot
#'
#' @examples
#'
#' grm_dt <- genData(eta = genTheta(nsample = 500, nfac = 1),
#'                  ipar = genIRTpar(nitem = 10, ncat = 3, nfac = 1))
#'
#' fit <- runGRM(dat = grm_dt,
#'             lav.syntax = genLavSyn(dat = grm_dt, nfac = 1),
#'             estimator = "WL")
#' ESplot(fit, 1)
#'
#' @export
ESplot <- function(fit, selected_item, theta = seq(-4, 4, 0.1), base_size = 16, line_size=1, cal_option = "D") {

  iid <- `.` <- score <- item <- F1 <- NULL

  if(inherits(fit[[1]], "SingleGroupClass")) {
    vname <- names(coef(fit$mirt.fit))
    vname <- vname[-length(vname)]
  } else {

    vname <- rownames(lavInspect(fit$lav.fit)$lambda)

  }

  ipar <- fit$grm.par
  rownames(ipar) <- vname

  selected_id <- rownames(ipar)[selected_item]

  ipar <- data.frame(ipar)
  exscore_dt <- ipar %>%
    data.frame(iid = rownames(ipar)) %>%
    slice(selected_item) %>%
    group_split(iid) %>%
    map(., ~ calES(.x, theta)) %>%
    set_names(selected_id) %>%
    do.call("cbind", .) %>%
    data.frame(theta = theta) %>%
    gather("item","score", -theta)

  x_breaks = c(0,round(seq(from = min(theta), to = max(theta), length.out = 9),1))

  exscore_dt %>%
    ggplot(aes(x = theta, y = score, colour = item)) +
    geom_line(linewidth = line_size) +
    scale_color_viridis_d(option = cal_option) +
    labs(title = "Expected score",
         x = expression(theta~('ability on the logit scale')),
         y = expression(italic(Expected)~ italic(Score)),
         colour = "") +
    scale_x_continuous(breaks = x_breaks) +
    theme_bw(base_size = base_size)
}

#' Calculate item information
#'
#' @inheritParams calES
#' @noRd
calInfo = function(ipar, theta = seq(-4, 4, 0.1)) {

  ipar <- data.frame(ipar)
  a <- unlist(ipar[grep("a", names(ipar))])
  b <- unlist(ipar[grep("^b", names(ipar))])

  nq   <- length(theta)
  ncat <- length(b) + 1
  ps   <- calProb(ipar, theta = theta)$ps

  info <- rowSums(
    sapply(1:ncat, function(k) {
      (ps[, k] - ps[, k + 1]) * (1 - ps[, k] - ps[, k + 1])^2 }))

  info <- a^2 * info
  return(info)
}

#' Calculate item information
#'
#' @param fit an object from \code{\link{runGRM}}
#' @param selected_item a numeric indicating for what items the function makes plots
#' @param type a character indicating the type of plots
#' \itemize{
#' \item{\code{icc}} Test information
#' \item{\code{tcc}} Total Test information
#' }
#' @param theta a numeric indicating latent traits
#' @param base_size a numeric indicating the base font size
#' @param line_size a numeric indicating the size of line
#' @param cal_option a character indicating the plot colour  specified in
#'  \code{\link{scale_color_viridis_d}} (default = \code{D})
#' @param facet a logical. If TRUE, the plot is faceted by items.
#' (default = \code{FALSE}).
#'
#' @return a \code{\link{ggplot}} object.
#'
#' @details This makes either item information plots or
#' total information plot
#'
#' @examples
#'
#' grm_dt <- genData(eta = genTheta(nsample = 500, nfac = 1),
#'                  ipar = genIRTpar(nitem = 10, ncat = 3, nfac = 1))
#'
#' fit <- runGRM(dat = grm_dt,
#'             lav.syntax = genLavSyn(dat = grm_dt, nfac = 1),
#'             estimator = "WL")
#' infoPlot(fit, selected_item=1:4, type = "icc", base_size = 16)
#'
#' @export
infoPlot <- function(fit, selected_item, type = "icc", theta = seq(-4, 4, 0.1), base_size = 16, line_size=1, cal_option = "D", facet = FALSE) {

  iid <- `.` <- info <- item <- total.info <- NULL

  if(inherits(fit[[1]], "SingleGroupClass")) {
    vname <- names(coef(fit$mirt.fit))
    vname <- vname[-length(vname)]
  } else {
    vname <- rownames(lavInspect(fit$lav.fit)$lambda)
  }

  ipar <- fit$grm.par
  rownames(ipar) <- vname

  if(!inherits(fit[[1]], "lavaan")) {
    ipar[,grep("a",names(ipar))] <-
      ipar[,grep("a",names(ipar))]*1.7
  }

  varname <- rownames(ipar)

  if(tolower(type) == "icc") {
    ipar <- data.frame(ipar) %>%
      data.frame(iid = varname) %>%
      slice(selected_item)

    ipar <- ipar %>%
      mutate(iid = factor(iid, levels = unique(ipar$iid)))

    selected_id <- ipar$iid
  } else {
    ipar <- data.frame(ipar) %>%
      data.frame(iid = varname)

    ipar <- ipar %>%
      mutate(iid = factor(iid, levels = unique(ipar$iid)))

    selected_item <- 1:nrow(ipar)
    selected_id <- ipar$iid
  }

  info_dt <- ipar %>%
    group_split(iid) %>%
    map(., ~ calInfo(.x, theta)) %>%
    set_names(selected_id) %>%
    do.call("cbind", .) %>%
    data.frame(theta = theta) %>%
    gather("item","info", -theta)

  x_breaks = c(0,round(seq(from = min(theta), to = max(theta), length.out = 9),1))

  if(tolower(type) == "icc") {
    p <- info_dt %>%
      ggplot(aes(x = theta, y = info, colour = item)) +
      geom_line(linewidth = line_size) +
      labs(title = "Information",
           x = expression(theta~('ability on the logit scale')),
           y = expression(italic(I)~(theta)),
           colour = "")

    if(facet)
      p + facet_wrap(item)

  } else {

    p <- info_dt %>%
      group_by(theta) %>%
      summarise(total.info = sum(info)) %>%
      ggplot(aes(x = theta, y = total.info)) +
      geom_line(linewidth = line_size) +
      labs(title = "Total Information",
           x = expression(theta~('ability on the logit scale')),
           y = expression(italic(TI)~(theta)),
           colour = "")
  }
  p  +
    scale_color_viridis_d(option = cal_option) +
    scale_x_continuous(breaks = x_breaks) +
    theme_bw(base_size = base_size)
}
