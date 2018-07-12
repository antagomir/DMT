# 'Computation is a new microscope for studying massive data sets.'
# 	      	   - @antagomir 2010

#' @title Probabilistic Factor Analysis 
#' @description Probabilistic factor analysis.
#' @param X X
#' @param Y Y
#' @param zDimension zDimension
#' @param includeData includeData
#' @param calculateZ calculateZ
#' @param priors priors
#' @return pfa model
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("dmt").
#' @keywords utilities
#' @examples #
#' @export
pfa <- function (X, Y = NULL,
                 zDimension = NULL,
                 includeData = TRUE,
                 calculateZ = TRUE, priors = NULL) {

  # Probabilistic factorial analysis model as proposed in
  # EM Algorithms for ML Factoral Analysis, Rubin D. and 
  # Thayer D. 1982

  # Assumption: R = I

  dat <- check.data(X, Y, zDimension)
  X <- dat$X
  Y <- dat$Y
  zDimension <- dat$zDimension
  method <- "pFA"
  params <- list(marginalCovariances = "diagonal", zDimension = zDimension)

  if (nrow(X) == 1 && (nrow(Y) == 1 || is.null(Y)) ) {

    score <- Inf
    res <- list(W = dat, phi = list(X = 0, Y = 0))
    if (is.null(Y)) {res$phi$Y <- res$phi$X <- NULL; res$phi$total <- 0}    

  } else {

    res <- calc.pfa(X, Y, zDimension, priors)
    score <- dependency.score( res )  

  }

  model <- new("DependencyModel",
               W = res$W, phi = res$phi,
               score = score,
               method = method,
               params = params)
  if ( includeData ) model@data <- list(X = X, Y = Y)

  if ( calculateZ ) {
    if (nrow(X) == 1 && (nrow(Y) == 1 || is.null(Y)) ) {
      model@z <- X
    } else {
      model@z <- z.expectation(model, X, Y)
    }
  }

  model

}

calc.pfa <- function (X, Y, zDimension, priors = NULL) {

  # Y.rubin is Y in (Rubin & Thayer, 1982)
  # Variables on columns and samples on rows
  # W corresponds to t(beta)
  if (is.null(Y)){
    Y.rubin <- t(X)
    # Factor loading matrix
    Wt <- t(eigen(cov(t(X)))$vectors[, 1:zDimension])
  } else {
    Y.rubin <- cbind(t(X), t(Y))
    # Use different initialization for Wt when data has inequal dimensionalities
    if (nrow(X) != nrow(Y)) {
      Wt <- t(eigen(cov(Y.rubin))$vectors[,1:zDimension])
    } else {
      init <- initialize2(X, Y, zDimension, marginalCovariances = "diagonal")
      # Factor loading matrix
      Wt <- t(init$W$total[,1:zDimension])
    }
  }
  
  epsilon <- 1e-3
  colnames(Wt) <- colnames(Y.rubin)
  tau2 <- diag(ncol(Y.rubin))

  Cyy <- cov(Y.rubin)
  delta <- 1e12
  # EM
  while(delta > epsilon){

    Wt.old <- Wt
    tau2.old <- tau2

    # E-step
    invtau2 <- solve(tau2)
    binv <- Wt%*%invtau2
    tbb <- invtau2 - (invtau2%*%t(Wt))%*%solve(diag(zDimension) + binv%*%t(Wt))%*%(binv)
    d <- tbb%*%t(Wt)
    D <- diag(zDimension) - Wt%*%d
    cyd   <- Cyy%*%d
    
    # M-step
    # Update W
    # FIXME: combine calc.pca, calc.pfa and calc.cca into one uniform model?
    if (is.null(priors)) {
      #message("Analytical optimization")      
      Wt  <- solve(t(d)%*%cyd + D)%*%t(cyd) # WORKS    
      # Also obtained with numerical optimization:
      # Wt <- update_W_singledata(Wt, X, tau2)
    } else if (!is.null(priors$W) && priors$W > 0) {
      #message("Numerical optimization")
      Wt <- update_W_singledata(Wt, X, tau2, priors)
    }
    
    # Update margin/s 
    tau2  <- diag(diag(Cyy - cyd%*%solve(t(d)%*%cyd + D)%*%t(cyd)))
    # Check cost function convergence
    delta <- max(sum(abs(tau2 - tau2.old)), sum(abs(Wt - Wt.old)))

  }

  # Convert names as same in other methods
  if ( is.null(Y) ){
      W <- list(total = t(Wt))
    phi <- list(total = tau2)
  } else {
      W <- list(X = as.matrix(t(Wt)[(1:nrow(X)),]), Y = as.matrix(t(Wt)[-(1:nrow(X)),]), total = t(Wt))
    phi <- list(X = tau2[1:nrow(X),1:nrow(X)], Y = tau2[-(1:nrow(X)),-(1:nrow(X))], total = tau2)                
  }
  
  list(W = W, phi = phi)

}

