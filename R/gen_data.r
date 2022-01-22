#' Generate Item parameters
#'
#' @examples
#' genIRTpar(20, 4, 3, "grm")
#' genIRTpar(20, 4, 3, "gpcm")
#' genIRTpar(20, 2, 3, "1pl")
#' genIRTpar(20, 2, 3, "2pl")
#' genIRTpar(20, 2, 3, "3pl")
#'
genIRTpar <- function(nitem=25, ncat=4, nfac=3, lvmodel) {

  lvmodel <- tolower(lvmodel)

  if(ncat <= 1) {
    stop("the number of cateories should be at least 2")
  } else if(ncat == 2 & lvmodel %in% c("grm","gpcm")) {
    stop("For GRM and GPCM, cateories should be at least 3")
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

  if(lvmodel %in% c("grm","gpcm")) {

    if(lvmodel == "grm"){ # grm
      # for the graded model, ensure that there is enough space between the intercepts,
      # otherwise closer categories will not be selected often
      # (minimum distance of 0.3 here)
      diffs <- t(apply(matrix(runif(nitem * (ncat-1), .3, 1), nitem), 1, cumsum))
      d <- -(diffs - rowMeans(diffs))
    }
    else { # gpcm
      d <- matrix(rnorm(nitem * (ncat)), nitem)
      d[,1] <- 0 #the first 0 here is the recommended constraint for gpcm
    }

    colnames(d) <- paste0("d",1:ncol(d))

    ipar <- data.frame(a, d)

  }
  else if(lvmodel %in% c("rasch","1pl","2pl","3pl")) {

    g <- 0
    d <- rnorm(nitem)

    if(lvmodel %in% c("1pl","rasch")) {
      a[which(a_idx == 1)] <- 1
    }
    else if(lvmodel == "3pl") {
      g = runif(nitem, 0, 0.2)
    }

    ipar <- data.frame(a, d, g)
  }

  return(ipar)
}

#' Generate IRT data
#'
#' @examples
#'
#'lvmodel <- "gpcm"
#'ipar <- genIRTpar(20, ncat = 3, 2, lvmodel)
#'eta <- MASS::mvrnorm(100, rep(0, 2), matrix(c(1,0,0,1),ncol=2))
#'genIRTdt(lvmodel, eta, ipar)
#'
genIRTdt <- function(lvmodel, eta, ipar) {

  stopifnot(is.data.frame(ipar))

  N    <- nrow(eta)
  nfac <- ncol(eta)

  lvmodel <- switch(lvmodel,
                    "rasch" = "dich",
                    "2pl" = "dich",
                    "3pl" = "dich",
                    "gpcm" = "gpcm",
                    "grm" = "graded")

  if(lvmodel == "dich") {
    a <- ipar[grep("a",names(ipar))]
    d <- ipar[grep("d",names(ipar))]
    guess <- ipar[,grep("g",names(ipar))]

  }

  if(lvmodel %in% c("gpcm","graded")) {
    a <- ipar[grep("a",names(ipar))]
    d <- ipar[grep("d",names(ipar))]
    guess <- 0
  }

  stopifnot(ncol(a) == ncol(eta))

  dat <- mirt::simdata(
    a = as.matrix(a),
    d = as.matrix(d),
    guess = as.vector(guess),
    N = N,
    Theta = as.matrix(eta),
    itemtype = lvmodel)

  return(dat)
}
