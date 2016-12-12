# "Where a calculator on the ENIAC is equipped with 18,000 vacuum tubes
# and weighs 30 tons, computers in the future may have only 1,000
# vaccuum tubes and perhaps weigh 1.5 tons."
# - unknown, Popular Mechanics, March 1949

initialize2 <- function (X, Y, zDim = NULL, marginalCovariances) {

  zDim <- ifelse(is.null(zDim), min(nrow(X), nrow(Y)), zDim)

  Nsamples <- ncol(X)
  Dim <- list(X = nrow(X), Y = nrow(Y), Z = zDim)
  nullmat  <- matrix(0, nrow = Dim$X, ncol = min(Dim$X, Dim$Y))

  if (marginalCovariances == "isotropic") {
    # Scalar values
    phi <- list()
    phi$X <- diag(var(as.vector(X)), Dim$X)
    if (!is.null(Y)) {
      phi$Y <- diag(var(as.vector(Y)), Dim$Y)
    }
    phi$total <- diag(c(diag(phi$X), diag(phi$Y)))
  } else if (marginalCovariances == "identical isotropic") {
    # Scalar values phix = phiy
    phi <- list()
    if (!is.null(Y)) {
      phi.est <- var(c(as.vector(X), as.vector(Y)))
    } else {
      phi.est <- var(as.vector(X))
    }
    phi$X <- diag(phi.est, Dim$X)
    phi$Y <- diag(phi.est, Dim$Y)
    phi$total <- diag(c(diag(phi$X), diag(phi$Y)))    
    #nullmat <- 0
  } else {
    # diagonal matrices
    # initialize with scalar diagonal noise on the marginals
    # (shared by all features)
    if (!is.null(Y)) {      
      phi <- list(X = diag(var(as.vector(X)), Dim$X), 
                Y = diag(var(as.vector(Y)), Dim$Y))
    } else {
      phi <- list(X = diag(var(as.vector(X)), Dim$X))
    }

    if (!is.null(Y)) {
      phi$total <- rbind(cbind(phi$X,nullmat), cbind(t(nullmat), phi$Y))
      #phi$total <- rbind(cbind(phi$X,nullmat), cbind(nullmat, phi$Y))      
    } else {
      phi$total <- cbind(phi$X,nullmat)
    }
  }

  # FIXME: if phi$Y is scalar (as in segmented/mir case) we can speed up here. Do later.

  phi.inv  <- list()
  phi.inv$X <- solve(phi$X)
  phi.inv$Y <- solve(phi$Y)

  Dcov <- list()
  Dcov$X <- cov(t(X), use = "pairwise.complete.obs")
  Dcov$Y <- cov(t(Y), use = "pairwise.complete.obs")
  Dcov$xy <- cov(t(X), t(Y), use = "pairwise.complete.obs")
  Dcov$yx <- t(Dcov$xy)
  Dcov$total <- rbind(cbind(Dcov$X, Dcov$xy), cbind(Dcov$yx, Dcov$Y))
  if (nrow(X) == nrow(Y)) {
    Dcov$sum <- Dcov$X + Dcov$Y + Dcov$xy + Dcov$yx
  }

  # It is possible that covariances calculated with pairwise complete
  # observations are not positive semi-definite.
  # Check this. If not pos.sem.def, then replace with the closest
  # pos. semidefinite matrix
  if (any(eigen(Dcov$total)$values < 0)) {

    Dcov$X  <- as.matrix(nearPD(Dcov$X)$mat)
    Dcov$Y  <- as.matrix(nearPD(Dcov$Y)$mat)
    Dcov$total <- rbind(cbind(Dcov$X, Dcov$xy), cbind(Dcov$yx, Dcov$Y))
    if (nrow(X) == nrow(Y)) {
      Dcov$sum <- Dcov$X + Dcov$Y + Dcov$xy + Dcov$yx
    }
  }

  # Initialize W's
  W <- list()
  W$X <- as.matrix(eigen(Dcov$X)$vectors[, 1:Dim$Z])
  W$Y <- as.matrix(eigen(Dcov$Y)$vectors[, 1:Dim$Z])	
  W$total <- rbind(W$X, W$Y) 
  
  list(phi = phi, W = W, Dcov = Dcov, Dim = Dim, nullmat = nullmat, Nsamples = Nsamples)

}

