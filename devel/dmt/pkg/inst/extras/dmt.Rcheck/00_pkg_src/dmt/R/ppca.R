#' @title Probabilistic PCA
#' @description Probabilistic PCA.
#' @param X Data set (variables x samples)
#' @param Y The second data set is optional.
#' @param zDimension Dimensionality of the shared latent variable.
#' @param includeData Logical indicating whether the original data is included
#' to the model output. Using \code{FALSE} can be used to save memory.
#' @param calculateZ Logical indicating whether an expectation of the latent
#' variable Z is included in the model output. Otherwise the expectation can be
#' calculated with \code{getZ} or \code{z.expectation}. Using \code{FALSE}
#' speeds up the calculation of the dependency model.
#' @return \linkS4class{DependencyModel}
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references 
#' A Probabilistic Interpretation of Canonical Correlation Analysis, Bach
#' Francis R. and Jordan Michael I. 2005 Technical Report 688. Department of
#' Statistics, University of California, Berkley.
#'
#' Probabilistic Principal Component Analysis, Tipping Michael E. and Bishop
#' Christopher M. 1999. \emph{Journal of the Royal Statistical Society}, Series
#' B, \bold{61}, Part 3, pp. 611--622.
#' 
#' EM Algorithms for ML Factorial Analysis, Rubin D. and Thayer D. 1982.
#' \emph{Psychometrika}, \bold{vol. 47}, no. 1.
#' @keywords math
#' @examples # ppca()
ppca <- function (X, Y = NULL, zDimension = NULL, includeData = TRUE, calculateZ = TRUE) {

  dat <- check.data(X, Y, zDimension)
  X <- dat$X  
  Y <- dat$Y
  zDimension <- dat$zDimension

  res <- calc.ppca(X, Y, zDimension)
  
  method <- "pPCA"	
  params <- list(marginalCovariances = "isotropic", zDimension = zDimension)
  score <- dependency.score( res )
  model <- new("DependencyModel", W = res$W, phi = res$phi, score = score, method = method, params = params)
  if (includeData) { model@data <- list(X = X, Y = Y) }
  if (calculateZ) { model@z <- z.expectation(model, X, Y) }
  model
 	    
}
