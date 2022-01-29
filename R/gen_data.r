#' Generate Item parameters
#'
#' @param nitem a numeric indicating the number of items
#' @param ncat  a numeric indicating the number of categories
#' @param nfac  a numeric indicating the number of factors
#'
#' @examples
#' genIRTpar(20, 4, 3)
#' @export
genIRTpar <- function(nitem=25, ncat=4, nfac=3) {

  if(ncat <= 1) {
    stop("The number of cateories should be at least 2")
  }

  gen_a <- function(nitem, nfac) {
    idx_ <- rep(floor(nitem / nfac),nfac)
    idx_[length(idx_)] <- nitem - sum(idx_[-length(idx_)])
    idx_c <- c(0,cumsum(idx_))
    a    <- matrix(rep(0, nitem*nfac), ncol=nfac)
    a_idx <- matrix(rep(0, nitem*nfac), ncol=nfac)
    for(j in 1:nfac) { # j=1
      a_idx[(idx_c[j]+1):idx_c[(j+1)],j] <- 1
      a[(idx_c[j]+1):idx_c[(j+1)],j] <- c(1, matrix(rlnorm((idx_[(j)]-1), .2, .3))) #the first 1 here is the recommended constraint
    }
    colnames(a) <- paste0("a",1:ncol(a))

    list(a_idx = a_idx, a = a)
  }

  a_list <- gen_a(nitem, nfac)
  a <- a_list$a
  a_idx <- a_list$a_idx

  diffs <- t(apply(matrix(runif(nitem * (ncat-1), .3, 1), nitem), 1, cumsum))
  d <- -(diffs - rowMeans(diffs))

  colnames(d) <- paste0("d",1:ncol(d))

  ipar <- data.frame(a, d)

  return(ipar)
}

#' Generate individual true latent traits
#'
#' @param nsample a numeric indicating the number of people
#' @param nfac a numeric indicating the number of factors
#' @param l.cov a matrix containing latent covariances
#' @export
genTheta <- function(nsample, nfac, l.cov = NULL) {

  if(is.null(l.cov)) {
    l.cov <- diag(nfac)
    covs <- sample(c(-.2, 0, .2), sum(lower.tri(l.cov)), prob = c(0.25, 0.5, 0.25))
    l.cov[lower.tri(l.cov)] <- covs
    l.cov[upper.tri(l.cov)] <- covs
  }

  MASS::mvrnorm(nsample, rep(0, nfac), l.cov)
}

#' Generate IRT data
#'
#' @param eta a matrix indicating individual true latent traits
#' @param ipar a dataframe containing item parameters
#'
#' @examples
#'
#' ipar <- genIRTpar(20, ncat = 3, 2)
#' eta <- genTheta(50, 2)
#' genData(eta, ipar)
#'
#' @export
genData <- function(eta, ipar) {

  stopifnot(is.data.frame(ipar))

  N    <- nrow(eta)
  nfac <- ncol(eta)

  a <- ipar[grep("a",names(ipar))]
  d <- ipar[grep("d",names(ipar))]
  guess <- 0

  stopifnot(ncol(a) == ncol(eta))

  dat <- mirt::simdata(
    a = as.matrix(a),
    d = as.matrix(d),
    guess = as.vector(guess),
    N = N,
    Theta = as.matrix(eta),
    itemtype = "graded")

  dat <- data.frame(dat)
  names(dat) <- paste0("y", 1:ncol(dat))

  return(dat)
}
