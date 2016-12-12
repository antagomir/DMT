#' @title Toy Data Generation
#' 
#' @description Generate simulated data which follows the
#'  distributional assumptions of the model.
#' 
#' @details Assuming normally distributed latent variables for shared
#' component Z, and
#' data-specific components Zx, Zy. These follow standard multivariate normal
#' distribution N(0, I). The observations X and Y are obtained as X = Wx*Z +
#' Bx*Zx, Y = Wy*Z + By*Zy.
#' 
#' @param N Sample size.
#' @param zDim Dimensionality of the latent variable.
#' @param xDim Dimensionality of X data set.
#' @param yDim Dimensionality of Y data set.
#' @param marginal.covariances "full": full covariance matrices for marginal
#' noise (assumed by pCCA); "diagonal": diagonal covariances for marginal noise
#' (pFA); "isotropic": isotropic covariances (pPCA).
#' @param priors Set priors for toydata generation. Use as in
#' \code{\link{fit.dependency.model}}.
#' @return List with the following components: \item{Z, Zx, Zy }{Shared and
#' data-set specific latent variables.} \item{Wx, Wy, Bx, By }{ Transformation
#' matrices. } \item{X, Y }{ Data sets. }
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("dmt") for references.
#' @keywords utilities
#' @export
#' @examples
#'  toy <- generate.toydata(N = 100, 
#'                    zDim = 1, xDim = 3, yDim = 3, 
#'                    marginal.covariances = "full") 
#' 
generate.toydata <- function (N = 100, zDim = 2, xDim = 3, yDim = 3, marginal.covariances = "full", priors = NULL) {

  # FIXME: add Wx ~ Wy or Wy = TWx option
  # FIXME: add non-full marginal noises (started, not finished)

  #########################################################
  
  # LATENT VARIABLES

  Z <- matrix( rnorm(zDim * N), nrow = zDim )

  # marginal noise dimensionality equals data dimensionality
  
  zxDim <- xDim
  Zx <- matrix(rnorm(N*zxDim), nrow = zxDim)

  zyDim <- yDim
  Zy <- matrix(rnorm(N*zyDim), nrow = zyDim)  

  
  ######################################################

  if (is.null(priors$W)) {

    Wx <- matrix(rnorm(zDim*xDim), nrow = xDim)
    Wy <- matrix(rnorm(zDim*yDim), nrow = yDim)

  } else if (priors$W > 0) {

    Wx <- matrix(rexp(zDim*xDim, rate = 1), nrow = xDim)
    Wy <- matrix(rexp(zDim*yDim, rate = 1), nrow = yDim)
  
  }
  
  if (!is.null(priors$Nm.wx.wy.sigma) && priors$Nm.wx.wy.sigma == 0) {
     Wy <- Wx
  }

  if (marginal.covariances == "full") {

    Bx <- matrix(rnorm(zxDim*xDim), nrow = xDim)
    By <- matrix(rnorm(zyDim*yDim), nrow = yDim)

  } else if (marginal.covariances == "diagonal") {

    Bx <- diag(rnorm(xDim))
    By <- diag(rnorm(yDim))

  } else if (marginal.covariances == "isotropic") {

    Bx <- rnorm(1)*diag(xDim)
    By <- rnorm(1)*diag(yDim)

  } else if (marginal.covariances == "identical isotropic") {

    const <- rnorm(1)
    Bx <- const*diag(xDim)
    By <- const*diag(yDim)

  }  

  # Marginal noise
  nx <- Bx%*%Zx
  ny <- By%*%Zy    

  X <- Wx%*%Z + nx
  Y <- Wy%*%Z + ny
		      
  list(Z = Z, X = X, Y = Y, Wx = Wx, Wy = Wy, Bx = Bx, By = By, Zx = Zx, Zy = Zy)

}

  


#' @title Measuring Model Accuracy
#' 
#' @description Compare estimated parameters and the original parameters used to generate the toydata.
#' 
#' @details Given original data (including the original parameters) and
#' model learned based on the data, compare the learned parameters to the
#' original ones.
#' 
#' @param res DependencyModel object.
#' @param toy Toydata which was used to learn the model.
#' @return Vector with following elements \item{wtw.x, wtw.y}{Correlation
#' between the original and estimated values for W*t(W) for X and Y,
#' respectively.} \item{phi.x, phi.y}{Correlation between the original and
#' estimated values for phi for X and Y, respectively.}
#' @note Additional tests added later.
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @seealso \code{\link{generate.toydata}}
#' @references See citation("dmt") for references.
#' @keywords utilities
#' @examples
#'  
#' # toy <- generate.toydata(N = N, 
#' #           zDim = zdim, xDim = xdim, yDim = ydim,
#' #           marginal.covariances = marginalCovariances,
#' #           priors = priors)
#' # res <- fit.dependency.model(toy$X, toy$Y,
#' #                      zDimension = zdim,
#' #              marginalCovariances = marginalCovariances,
#' #		      priors = priors,
#' #                     matched = FALSE)
#' # vec <- compare.estimate.and.truth(res, toy)
compare.estimate.and.truth <- function (res, toy) {
  
  # res: output from fit.dependency.model
  # toy: toydata object containing the toydata used to train the model 
  #      (including true parameters used to generate the toydata)
  
  wtw.x.estimated <- res@W$X%*%t(res@W$X)
  wtw.x.true <- toy$Wx%*%t(toy$Wx)
  
  wtw.y.estimated <- res@W$Y%*%t(res@W$Y)
  wtw.y.true <- toy$Wy%*%t(toy$Wy)  
  
  phiX.estimated <- res@phi$X
  phiX.true <- toy$Bx%*%t(toy$Bx)

  phiY.estimated <- res@phi$Y
  phiY.true <- toy$By%*%t(toy$By)

  corsx <- cor(as.vector(wtw.x.estimated), as.vector(wtw.x.true))
  corsy <- cor(as.vector(wtw.y.estimated), as.vector(wtw.y.true))
  
  cormx <- cor(as.vector(phiX.estimated), as.vector(phiX.true))
  cormy <- cor(as.vector(phiY.estimated), as.vector(phiY.true))  

  unlist(list(wtw.x = corsx, wtw.y = corsy, phi.x = cormx, phi.y = cormy))

}




