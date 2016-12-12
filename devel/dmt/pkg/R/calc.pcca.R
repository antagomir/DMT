calc.pcca <- function (X, Y, zDimension) {

  Dcov <- list()
  Dcov$X <- cov(t(X))
  Dcov$Y <- cov(t(Y))

  # Solve W (solve.w utilizes Archambeau06 equations from PCA for shortcut)
  # FIXME: compare accuracy and speed to direct EM update scheme?
  W <- solve.w(t(X), t(Y), Dcov$X, Dcov$Y, zDimension)

  # Then take only the zDimension first components if defined
  W$X <- as.matrix(W$X)
  W$Y <- as.matrix(W$Y)
  W$total <- rbind(W$X, W$Y)
        
  # estimate
  phi <- list()
  phi$X <- Dcov$X - W$X%*%t(W$X)
  phi$Y <- Dcov$Y - W$Y%*%t(W$Y)

  # Retrieve principal canonical components from the prob.CCA model
  # assuming that W and phi are known (at least in the full-rank case)
  # U <- solve.archambeau(X, Y, W$X, W$Y, phi$X, phi$Y)

  list(W = W, phi = phi)
  
}



