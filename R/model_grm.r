#' Transform SEM params into GRM params
#'
trans_to_grm <- function(grm.fit) {

  if(class(grm.fit) != "lavaan") {
    est.mirt <- coef(grm.fit, simplify = T, IRTpars = F)

    item.est <- data.frame(est.mirt$items)
    Lam  <- item.est[grep("a", names(item.est))]
    Thre <- item.est[grep("d", names(item.est))]
    FV <- est.mirt$cov
    FM <- est.mirt$means
    estimator = "ML"

  } else {
    est.lav <- lavInspect(grm.fit, what = "est")

    Lam <- data.frame(est.lav$lambda)
    Thre <- est.lav$tau
    n_thre <- nrow(Thre)/nrow(Lam)
    Thre <- data.frame(matrix(Thre, ncol = n_thre, byrow = T))

    FV <- est.lav$psi
    FM <- est.lav$alpha
    estimator = "WL"
  }

  Disc <- matrix(rep(0, nrow(Lam)*ncol(Lam)), ncol = ncol(Lam))
  Diff <- matrix(rep(0, nrow(Lam)*ncol(Thre)), ncol = ncol(Thre))
  for(i in 1:nrow(Lam)) {

    lam <- Lam[i,]
    thre <- Thre[i,]

    for(j in 1:ncol(Lam)) {

      if (toupper(estimator) == "ML") {
        # if ML
        Disc[i,j] <- lam*sqrt(FV) / 1.7
      } else {
        # if WL
        Disc[i,j] <- lam*sqrt(FV) / sqrt((1 - (lam)^2))
      }
    }

    for(j in 1:ncol(Thre)) {
      Diff[i,j] <- (thre[,j] - lam*FM) / lam*sqrt(FV)
    }
  }
  if(class(grm.fit) == "lavaan") {
    Diff <- -Diff
  }
  colnames(Disc) <- paste0("a", 1:ncol(Disc))
  colnames(Diff) <- paste0("d", 1:ncol(Diff))
  data.frame(Disc, Diff)
}

#' Run graded response model
#'
runGRM <- function(dat, lav.syntax, estimator) {

  if(toupper(estimator) != "ML") {
    # WLE
    grm.fit <- cfa(
      model = lav.syntax,
      data = dat,
      ordered = names(dat),
      parameterization = "delta",# theta
      estimator = "WLSMV" #"ML" is not working for now in lavaan
    )

    grm.par <- trans_to_grm(grm.fit = grm.fit)

  } else {
    # ML
    lav.syntax <- str_replace_all(lav.syntax, "\\+l[0-9]\\*|\\+l[0-9][0-9]\\*|\\+l[0-9][0-9][0-9]\\*", "+")

    lav.syntax <- str_remove_all(lav.syntax, " t[0-9][0-9]\\*| t[0-9][0-9][0-9]\\*| t[0-9][0-9][0-9][0-9]\\*")

    grm.sirt <- lavaan2mirt(dat,
                            lav.syntax,
                            est.mirt = T,
                            poly.itemtype = "graded",
                            SE = T, verbose = F)

    # grm.fit <-
    #   mirt::mirt(grm.sirt$dat,
    #              model = grm.sirt$mirt.model,
    #              pars = grm.sirt$mirt.pars,
    #              itemtype = "graded", SE = T, verbose = F)

    grm.fit <- grm.sirt$mirt
    grm.par <- trans_to_grm(grm.fit = grm.fit)
  }

  res <- list(fit = grm.fit, grm.par = grm.par)
  names(res)[1] <- ifelse(estimator != "ML", "lav.fit", "mirt.fit")

  return(res)
}

#' Generate lavaan syntax
#'
genLavSyn <- function(dat) {

  n_thre <- max(dat)
  usevars <- names(dat)
  nvars <- length(usevars)

  var_name <- c(paste0(usevars[1:(nvars-1)], "+"),usevars[nvars])
  lam_label <- paste0("l", 1:nvars,"*")
  fac_syn <- paste0(lam_label[1], var_name[1])
  for(i in 2:nvars) {
    fac_syn <- paste0(fac_syn,paste0(lam_label[i], var_name[i]))
  }

  first <- paste0("NA*",usevars[1],"+")

  fac_syn <- paste0("F =~ ",first,fac_syn,"\n\n")

  fac_m_v <- "F ~~ 1*F; \n F ~ 0*1;\n"

  thre_name <- rep(paste0("t",1:n_thre,";\n"), each = nvars)
  thre_label <- paste0("t", apply(expand.grid(1:nvars, 1:n_thre), 1, function(x) paste0(x, collapse = "")))
  thre_label <- paste0(thre_label, "*")
  var_name <- paste0(rep(usevars, n_thre), " | ")

  thre_syn <- sapply(1:length(thre_name),function(x) {
    paste0(var_name[x],thre_label[x],thre_name[x])
  })
  thre_syn <- paste0("\n",paste(thre_syn, collapse = ""))

  lav_syn <- paste(fac_syn, fac_m_v, thre_syn, collapse = "\n")

  lav_syn
}
