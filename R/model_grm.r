#' @include GRShiny-package.r
NULL

#' Run graded response model
#'
#' @param dat a data frame containing graded response model data
#' @param lav.syntax a character indicating \pkg{lavaan} syntax
#' @param estimator a character indicating the type of estimator.
#' \itemize{
#' \item{\code{ML}} Maximum likelihood estimation
#' \item{\code{WL}} Weighted least squares mean and variance
#' }
#'
#' @return a list containing GRM results as follows:
#' \itemize{
#'   \item{\code{fit}} an object from either \code{\linkS4class[mirt]{SingleGroupClass}}
#'    from \pkg{mirt} or \code{\linkS4class[lavaan]{lavaan}} from from
#'     \pkg{lavaan}.
#'   \item{\code{grm.par}} a data frame indicating graded response parameters.
#' }
#'
#' @details This conducts GRM. The second element of the return indicates
#' the graded response parameters converted from the CFA parameters
#'
#' @export runGRM
runGRM <- function(dat, lav.syntax, estimator) {

  varname <- names(dat)

  if(toupper(estimator) != "ML") {
    # WLE
    grm.fit <- cfa(
      model = lav.syntax,
      data = dat,
      ordered = names(dat),
      parameterization = "delta",# theta
      estimator = "WLSMV" #"ML" is not working for now in lavaan
    )

    if(grm.fit@pta$nfac == 1) {

      grm.par <- trans_to_grm(grm.fit = grm.fit)
      # grm.par <- grm.par %>%
      #   mutate(variable = varname, .before = a1)

    } else {

      grm.par <- "traditional parameterization of GRM not working with more than 2 factors"
      print(grm.par)

      grm.par <- trans_to_grm(grm.fit = grm.fit)
      # grm.par <- grm.par %>%
      #   mutate(variable = varname, .before = a1)
    }

  } else {
    # ML
    lav.syntax <- str_replace_all(lav.syntax, "\\+l[0-9]\\*|\\+l[0-9][0-9]\\*|\\+l[0-9][0-9][0-9]\\*", "+")

    lav.syntax <- str_remove_all(lav.syntax, " t[0-9][0-9]\\*| t[0-9][0-9][0-9]\\*| t[0-9][0-9][0-9][0-9]\\*")

    grm.sirt <- lavaan2mirt(dat,
                            lav.syntax,
                            est.mirt = T,
                            poly.itemtype = "graded",
                            SE = T, verbose = F)

    grm.fit <- grm.sirt$mirt
    if(ncol(grm.fit@Fit$F) == 1) {

      grm.par <- trans_to_grm(grm.fit = grm.fit)
      # rownames(grm.par) <- varname
      # grm.par <- grm.par %>%
      #   mutate(variable = varname, .before = a1)

    } else {

      print(grm.par)

      grm.par <- trans_to_grm(grm.fit = grm.fit)
      # rownames(grm.par) <- varname

      # grm.par <- grm.par %>%
      #   mutate(variable = varname, .before = a1)
    }
  }

  res <- list(fit = grm.fit, grm.par = grm.par)
  names(res)[1] <- ifelse(estimator != "ML", "lav.fit", "mirt.fit")

  return(res)
}

#' Generate lavaan syntax
#'
#' @param dat a data frame containing graded response data
#' @param nfac a numeric indicating the number of factors
#'
#' @return a string indicating \code{\link[lavaan]{lavaan}} syntax.
#'
#' @details This generates \code{\link[lavaan]{lavaan}}  syntax
#'
#' @export genLavSyn
genLavSyn <- function(dat, nfac=1) {

  gen_a <- function(nitem, nfac) {
    idx_ <- rep(floor(nitem / nfac),nfac)
    idx_[length(idx_)] <- nitem - sum(idx_[-length(idx_)])
    idx_c <- c(0,cumsum(idx_))
    a_idx <- matrix(rep(0, nitem*nfac), ncol=nfac)
    for(j in 1:nfac) { # j=1
      a_idx[(idx_c[j]+1):idx_c[(j+1)],j] <- 1
    }
    a_idx
  }

  n_thre <- max(dat)
  nvars <- ncol(dat)

  oname <- names(dat)
  fname <- paste0("F",1:nfac)

  a_list <- gen_a(nvars, nfac); svar <- 1; num_po <- 0
  fac_m_v <- c(); lav_syn <- c()
  for(j in 1:nfac) { # j = 1

    picked <- which(a_list[, j] == 1)
    picked_len <- length(picked)
    num_po <- num_po + picked_len
    picked_ovar <- oname[picked]
    picked_fvar <- fname[j]

    var_name <- c(paste0(picked_ovar[1:(picked_len-1)], "+"),picked_ovar[picked_len])
    lam_label <- paste0("l", svar:num_po,"*")
    indi_syn <- paste0(lam_label[1], var_name[1])
    for(i in 2:picked_len) { # i = 1
      indi_syn <- paste0(indi_syn,paste0(lam_label[i], var_name[i]))
    }
    firstid <- paste0("NA*",picked_ovar[1],"+")
    indi_syn <- paste0(picked_fvar, " =~ ",firstid,indi_syn,"\n\n")

    lav_syn <- paste0(lav_syn, "\n", indi_syn)

    fac_m_v <- paste0(fac_m_v, "\n",paste(c(paste0(picked_fvar,"~~ 1*",picked_fvar ),paste0(picked_fvar,"~ 0*1" )), collapse = "\n"))

    svar <- svar + num_po
  }

  fac_c <- c()
  if(nfac > 1){
    for(j in 2:nfac) { # j = 2
      fac_c <- paste0(fac_c, "\n",paste0(fname[(j-1)],"~~",
                                         paste(fname[(j):nfac], collapse = "+") ))
    }}

  thre_name <- rep(paste0("t",1:n_thre,";\n"), each = nvars)
  thre_label <- paste0("t", apply(expand.grid(1:nvars, 1:n_thre), 1, function(x) paste0(x, collapse = "")))
  thre_label <- paste0(thre_label, "*")
  var_name <- paste0(rep(oname, n_thre), " | ")

  thre_syn <- sapply(1:length(thre_name),function(x) {
    paste0(var_name[x],thre_label[x],thre_name[x])
  })
  thre_syn <- paste0("\n",paste(thre_syn, collapse = ""))

  lav_syn <- paste(lav_syn, fac_m_v, fac_c, thre_syn, collapse = "\n")

  cat(lav_syn)

  lav_syn
}

#' Transform SEM params into GRM params
#'
#' @param grm.fit a matrix containing the estimates of GRM
#' @noRd
trans_to_grm <- function(grm.fit) {

  if(!inherits(grm.fit, "lavaan")) {
    est.mirt <- coef(grm.fit, simplify = T, IRTpars = F)

    # varname <- rownames(est.mirt$items)

    item.est <- data.frame(est.mirt$items)
    Lam  <- item.est[grep("a", names(item.est))]
    Thre <- -item.est[grep("d", names(item.est))]
    FV <- est.mirt$cov
    FM <- est.mirt$means
    estimator = "ML"

  } else {
    est.lav <- lavInspect(grm.fit, what = "est")

    # varname <- rownames(est.lav$lambda)

    Lam <- data.frame(est.lav$lambda)
    Thre <- est.lav$tau
    n_thre <- nrow(Thre)/nrow(Lam)
    Thre <- data.frame(matrix(Thre, ncol = n_thre, byrow = T))

    FV <- est.lav$psi
    FM <- est.lav$alpha
    estimator = "WL"
  }

  Lam <- matrix(rowSums(Lam), byrow = T)
  FM <- c(FM)
  FV <- diag(FV)

  Disc <- matrix(rep(0, nrow(Lam)*ncol(Lam)), ncol = ncol(Lam))
  Diff <- matrix(rep(0, nrow(Lam)*ncol(Thre)), ncol = ncol(Thre))
  for(i in 1:nrow(Lam)) {

    lam <- Lam[i,]
    thre <- Thre[i,]

    for(j in 1:ncol(Lam)) {

      fv <- FV[j]
      fm <- FM[j]

      if (toupper(estimator) == "ML") {
        # if ML
        Disc[i,j] <- lam*sqrt(fv) / 1.7
      } else {
        # if WL
        Disc[i,j] <- lam*sqrt(fv) / sqrt((1 - (lam)^2))
      }
    }

    for(j in 1:ncol(Thre)) {
      Diff[i,j] <- (thre[,j] - lam*fm) / lam*sqrt(fv)
    }
  }


  if(inherits(grm.fit, "lavaan")) {
    Diff <- Diff
  }
  colnames(Disc) <- paste0("a", 1:ncol(Disc))
  colnames(Diff) <- paste0("b", 1:ncol(Diff))
  out <- data.frame(Disc, Diff)

  # row_name <- rownames(out)

  out
}
