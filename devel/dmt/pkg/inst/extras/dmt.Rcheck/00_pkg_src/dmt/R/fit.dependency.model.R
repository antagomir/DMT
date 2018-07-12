# "Computer Science is no more about computers than astronomy is about
#  telescopes." 
# - E. W. Dijkstra

#' Fit dependency model between two data sets.
#' 
#' Fit generative latent variable model (see vignette for model specification)
#' on two data sets. Regularize the solutions with priors, including
#' constraints on marginal covariance structures, the structure of W, latent
#' dimensionality etc. Probabilistic versions of PCA, factor analysis and CCA
#' are available as special cases.
#' 
#' The \code{fit.dependency.model} function fits the dependency model X = N(W$X
#' * Z, phi$X); Y = N(W$Y * Z, phi$Y) with the possibility to tune the model
#' structure and parameter priors.
#' 
#' In particular, the dataset-specific covariance structure phi can be defined;
#' non-negative priors for W are possible; the relation between W$X and W$Y can
#' be tuned. For a comprehensive set of examples, see the example scripts in
#' the tests/ directory of this package.
#' 
#' Special cases of the model, obtained with particular prior assumptions,
#' include probabilistic canonical correlation analysis (\code{pcca};
#' \cite{Bach & Jordan 2005}), probabilistic principal component analysis
#' (\code{ppca}; \cite{Tipping & Bishop 1999}), probabilistic factor analysis
#' (\code{pfa}; \cite{Rubin & Thayer 1982}), and a regularized version of
#' canonical correlation analysis (pSimCCA; \cite{Lahti et al. 2009}).
#' 
#' The standard probabilistic PCA and factor analysis are methods for a single
#' data set (X ~ N(WZ, phi)), with isotropic and diagonal covariance (phi) for
#' pPCA and pFA, respectively. Analogous models for two data sets are obtained
#' by concatenating the two data sets, and performing pPCA or pFA.
#' 
#' Such special cases are obtained with the following choices in the
#' \code{fit.dependency.model} function:
#' 
#' \describe{
#' 
#' \item{pPCA}{ \code{marginalCovariances = "identical isotropic"}
#' (\cite{Tipping & Bishop 1999}) }
#' 
#' \item{pFA}{ \code{marginalCovariances = "diagonal"} (\cite{Rubin & Thayer
#' 1982}) }
#' 
#' \item{pCCA}{ \code{marginalCovariances = "full"} (\cite{Bach & Jordan 2005})
#' }
#' 
#' \item{pSimCCA}{ \code{marginaCovariances = "full", priors =
#' list(Nm.wxwy.mean = I, Nm.wxwy.sigma = 0)}. This is the default method,
#' corresponds to the case with W$X = W$Y.  (\cite{Lahti et al. 2009}) }
#' 
#' \item{pSimCCA with T prior}{ \code{marginalCovariances = "isotropic", priors
#' = list(Nm.wxwy.mean = 1, Nm.wx.wy.sigma = 1} (\cite{Lahti et al. 2009}) }}
#' 
#' To avoid computational singularities, the covariance matrix phi is
#' regularised by adding a small constant to the diagonal.
#' 
#' @param X,Y Data set/s X and Y. 'Variables x samples'. The second data set
#' (\code{Y}) is optional.
#' @param zDimension Dimensionality of the shared latent variable.
#' @param marginalCovariances Structure of marginal covariances, assuming
#' multivariate Gaussian distributions for the dataset-specific effects.
#' Options: \code{"identical isotropic"}, \code{"isotropic"}, \code{"diagonal"}
#' and \code{"full"}. The difference between isotropic and identical isotropic
#' options is that in isotropic model, phi$X != phi$Y in general, whereas with
#' isotropic model phi$X = phi$Y.
#' @param epsilon Convergence limit.
#' @param priors Prior parameters for the model. A list, which can contain some
#' of the following elements: \describe{ \item{W}{Rate parameter for
#' exponential distribution (should be positive). Used to specify the prior for
#' Wx and Wy in the dependency model. The exponential prior is used to produce
#' non-negative solutions for W; small values of the rate parameter correspond
#' to an uninformative prior distribution.} \item{Nm.wxwy.mean}{ Mean of the
#' matrix normal prior distribution for the transformation matrix T. Must be a
#' matrix of size (variables in first data set) x (variables in second data
#' set). If value is \code{1}, \code{Nm.wxwy.mean} will be made identity matrix
#' of appropriate size.} \item{Nm.wxwy.sigma}{ Variance parameter for the
#' matrix normal prior distribution of the transformation matrix \code{T}.
#' Described the allowed deviation scale of the transformation matrix \code{T}
#' from the mean matrix \code{Nm.wxwy.mean}.} }
#' @param matched Logical indicating if the variables (dimensions) are matched
#' between X and Y. Applicable only when dimX = dimY. Affects the results only
#' when prior on the relationship Wx ~ Wy is set, i.e. when
#' priors$Nm.wx.wy.sigma < Inf.
#' @param includeData Logical indicating whether the original data is included
#' to the model output. Using \code{FALSE} can be used to save memory.
#' @param calculateZ Logical indicating whether an expectation of the latent
#' variable Z is included in the model output. Otherwise the expectation can be
#' calculated with \code{getZ} or \code{z.expectation}. Using \code{FALSE}
#' speeds up the calculation of the dependency model.
#' @param verbose Follow procedure by intermediate messages.
#' @return \linkS4class{DependencyModel}
#' @author Olli-Pekka Huovilainen \email{ohuovila@@gmail.com} and Leo Lahti
#' \email{leo.lahti@@iki.fi}
#' @seealso Output class for this function: \linkS4class{DependencyModel}.
#' Special cases: \code{ppca}, \code{pfa}, \code{pcca}
#' @references Dependency Detection with Similarity Constraints, Lahti et al.,
#' 2009 Proc. MLSP'09 IEEE International Workshop on Machine Learning for
#' Signal Processing, \url{http://arxiv.org/abs/1101.5919}
#' 
#' A Probabilistic Interpretation of Canonical Correlation Analysis, Bach
#' Francis R. and Jordan Michael I. 2005 Technical Report 688. Department of
#' Statistics, University of California, Berkley.
#' \url{http://www.di.ens.fr/~fbach/probacca.pdf}
#' 
#' Probabilistic Principal Component Analysis, Tipping Michael E. and Bishop
#' Christopher M. 1999. \emph{Journal of the Royal Statistical Society}, Series
#' B, \bold{61}, Part 3, pp. 611--622.
#' \url{http://research.microsoft.com/en-us/um/people/cmbishop/downloads/Bishop-PPCA-JRSS.pdf}
#' 
#' EM Algorithms for ML Factorial Analysis, Rubin D. and Thayer D. 1982.
#' \emph{Psychometrika}, \bold{vol. 47}, no. 1.
#' @keywords math
#' @examples
#' #  data(modelData) # Load example data X, Y
#' #  # probabilistic CCA
#' #  model <- pcca(X, Y)
#' #  # dependency model with priors (W>=0; Wx = Wy; full marginal covariances)
#' #  model <- fit.dependency.model(X, Y, zDimension = 1, 
#' #      	 	      priors = list(W = 1e-3, Nm.wx.wy.sigma = 0), 
#' # 			      marginalCovariances = "full")
#' #  # Getting the latent variable Z when it has been calculated with the model
#' #  # getZ(model)
#' 
#' @export
fit.dependency.model <- function (X, Y,
          zDimension = 1,
          marginalCovariances = "full",
          epsilon = 1e-3,
          priors = list(),
	  matched = TRUE,
          includeData = TRUE,
	  calculateZ = TRUE,
	  verbose = FALSE)
{

  # Fits the generative model
  # X = Wx * z + epsx
  # Y = Wy * z + epsy
  # with various modeling assumptions

  if ( verbose ) { cat("Checking data\n") }

  dat <- check.data(X, Y, zDimension)
  X <- dat$X
  Y <- dat$Y
  zDimension <- dat$zDimension
  intercept <- dat$intercept
  
  # FIXME store/return intercepts as well; further dependency models
  # including intercepts
  if (!is.null(Y) && (nrow(X) < nrow(Y)) ) {
    stop("If the two data matrices do not have equal dimensionality, place the smaller one in Y.")
  } # FIXME automate
  
  if ( !is.null(Y) && !nrow(X) == nrow(Y) ) {
    if ( matched ) { stop("Cannot use matched methods for nonmatched data.") }
  }

  if ( verbose ) { cat("Checking inputs\n") }
  if ( !is.null(priors$Nm.wxwy.sigma ) && priors$Nm.wxwy.sigma == Inf) { matched <- FALSE; message("priors$Nm.wxwy.sigma == Inf; Wx ~ Wy independendent i.e. matched = FALSE") }  
  if ( epsilon == 0 )  { epsilon <- 1e-3 } # avoid numerical overflows
  res <- NA; method <- ""

  if (!matched) {

    if (verbose) { cat("Model for non-matched case\n") }

    if ( is.null(priors$Nm.wxwy.sigma )) {
      priors$Nm.wxwy.sigma <- Inf
    }

    if ( is.null(priors$W) ) { 

      if ( verbose ) { cat("Wx ~ Wy free. No regularization for W.\n") }
      if ( verbose ) { cat(marginalCovariances); cat("\n") }
      
      if ( marginalCovariances == "full" ) { # standard pCCA

        method <- "pCCA"
        res <- calc.pcca(X, Y, zDimension)

      } else if (marginalCovariances == "diagonal") { 

        # pFA for two data sets corresponds to 
        # standard pFA for concatenated data

        res <- calc.pfa(X, Y, zDimension)    
        method <- "pFA"

      } else if (marginalCovariances == "isotropic") {
      
        # phiX != phiY in general
        # FIXME: add tests

        res <- calc.pcca.with.isotropic.margins(X, Y, zDimension)

        # force these scalars into diagonal matrices                  
        res$phi$X <- diag(res$phi$X, nrow(X))
        res$phi$Y <- diag(res$phi$Y, nrow(Y))           
        res$phi$total <- diag(c(diag(res$phi$X),diag(res$phi$Y)), (nrow(X) + nrow(Y)))

        method <- "pCCA with isotropic margins"
	
      } else if (marginalCovariances == "identical isotropic") {
      
        # FIXME: add tests for this
        # pPCA for two data sets corresponds to 
        # standard pPCA for concatenated data

        res <- calc.ppca(X, Y, zDimension)
        method <- "pPCA"
	
      } else { stop("Erroneous marginalCovariances parameter provided!") }

    } else if ( !is.null(priors$W) ) { 
      
      if ( verbose ) { cat("Wx ~ Wy free; exponential (nonnegative) prior for W.\n") }

      # Prior for W is given -> no analytical solution to EM
      # Exponential prior for W,
      # Used to force positive W with exponential distribution.
      # priors$W is the rate parameter of the exponential. 
      # The smaller, the flatter tail.

      # TODO: implement sparsity prior W ~ N(0, sd*I)
      res <- optimize.parameters(X, Y, zDim = zDimension, priors = priors,
                                   marginalCovariances = marginalCovariances,           
                                   epsilon = epsilon, convergence.steps = 3, verbose = verbose)
      method <- "Free Wx ~ Wy with exponential priors for Wx and Wy. Check marginal covariances from parameters."
	
    }
    
  } else if (matched) {

    if ( verbose ) { cat("Matched features case\n") }
      
    # Matrix normal distribution variance not specified
    if ( is.null(priors$Nm.wxwy.sigma) ) {
      message("Matched variables but priors$Nm.wxwy.sigma not given, using strong matching with Wx = Wy.")
      priors$Nm.wxwy.sigma <- 0
    }
      
    # Matrix normal distribution mean matrix not specified
    if ( is.null(priors$Nm.wxwy.mean) || is.na(priors$Nm.wxwy.mean)) {
      message("The matrix Nm.wxwy.mean is not specified. Using identity matrix.")
      priors$Nm.wxwy.mean <- 1
    }    

    method <- "pSimCCA"
        
    # Case IIa: fully constrained case Wx = Wy
    if ( priors$Nm.wxwy.sigma == 0 ) { #Wx = Wy        

      if ( verbose ) { cat("Assuming Wx = Wy\n") }
	
      #  SimCCA with full covariances with constraint Wx = Wy
      #  "probsimcca.full" = aucs.simcca.full      
      #  Denoting Wy = T*Wx = TW; W = Wx this fits the case T = I with
      #  full-rank Wx, Wy, Sigmax, Sigmay: (dxd-matrices where d equals to
      #  number of features in X and Y)
      # If prior for W is given, we must optimize W (no analytical
      # solution to EM)
          
      # Regularization for W (W > 0 implemented)
      if (!is.null(priors$W)) {

	if ( verbose ) { cat("Wx = Wy with regularized W (W>=0)\n") }
	if ( verbose ) { cat(marginalCovariances); cat("\n") }	

        res <- optimize.parameters(X, Y, zDim = zDimension, priors = priors, 
	       			   marginalCovariances = marginalCovariances, 
				   epsilon = epsilon, convergence.steps = 3, verbose = verbose)

        method <- "pCCA with W prior"

      } else if (is.null(priors$W)) {

	if ( verbose ) { cat("Wx = Wy; free W.\n") }

          # mlsp'09 simcca
          # message("Case Wx = Wy. No regularization for W.")
	  
	  # use this for full W (EM algorithm, unstable for n ~ p)

         res <- optimize.parameters(X, Y, zDim = zDimension, priors = priors, 
                                   marginalCovariances = marginalCovariances,           
                                   epsilon = epsilon, convergence.steps = 3,
                                   verbose = verbose)


          method <- "matched case Wx = Wy with unconstrained W. Check covariances from parameters."
          # FIXME: speeups possible here when Wx = Wy but not yet implemented with other than full covs
      }
      
    } else if ( priors$Nm.wxwy.sigma > 0 ) {
      # Case IIb: partially constrained Wx ~ Wy
                
      if ( verbose ) { cat("partially constrained Wx ~ Wy.\n") }
		
      if ( !is.null(priors$W) ) {
        if ( verbose ) {cat("regularized W.\n")}
        # FIXME: consider adding fast option with simply nonnegative W but no distributional priors
        stop("Not implemented regularization for W with partially constrained Wx ~ Wy.")
      } else if (is.null(priors$W)) {
        if ( verbose ) { cat("Partially constrained Wx ~ Wy. No regularization for W.\n") }
        if ( verbose ) { cat(marginalCovariances); cat("\n") }            		  
			  
        if ( marginalCovariances == 'isotropic' ) {
	  # message("SimCCA with isotropic covariances and regularized H (through sigmas).")
	
          # FIXME: consider later adding other covariance structures if needed?
	  # note that the computation is slow then          		

         res <- optimize.parameters(X, Y, zDim = zDimension, priors = priors,
	     			       marginalCovariances = marginalCovariances,                                   
				       epsilon = epsilon, convergence.steps = 3, verbose = verbose)
                                                                    
         method <- "constrained Wx~Wy with matrix normal distribution prior"


        } else if ( !marginalCovariances == 'isotropic' ) {
          stop("Only isotropic marginal covariances implemented with constrained Wx ~ Wy in the general case.")
        }
      } 
    }
  }


  if ( verbose ) { cat("Checking the model..\n") }

  # Test whether model exists for given arguments
  if ( any(is.na(unlist(res))) ) {
    stop("Error with model parameters.")
  } else {
    params <- list(marginalCovariances = marginalCovariances, Nm.wxwy.mean = priors$Nm.wxwy.mean, Nm.wxwy.sigma = priors$Nm.wxwy.sigma, zDimension = zDimension, epsilon = epsilon)
    score <- dependency.score( res )
  }
  
  if ( verbose ) {cat("Creating DependencyModel object..\n")}
  
  model <- new("DependencyModel", W = res$W, phi = res$phi, score = score, method = method, params = params)	
  if ( includeData ) model@data <- list(X = X, Y = Y)
  if ( calculateZ ) model@z <- z.expectation(model, X, Y)
  
  if ( verbose ) { cat("fit.dependency.model OK.\n") }
  
  model
}




# FIXME: now calc.ppca used only in one-data case by function ppca
# either remove two-data case, or test and compare with fit.dependency.model and take into use
calc.ppca <- function (X, Y, zDimension) {

  # Replaces function solve.CCA

  # if zDimension = NULL then full-rank solution is computed
  
  # Probabilistic PCA
  # (See Tipping and Bishop 1999)

  # ML estimates W, sigma for probability model
  # X ~ N(Wz, sigma*I)
  # i.e. latent variable model with isotropic noise

  # If only X is given in the argument, compute
  # pPCA for X

  # If both X and Y are given in the argument, compute
  # pPCA for concatenated [X; Y]
  # Assuming isotropic and identical marginal noise, the
  # principal subspace will capture the dependencies between X and Y.
  # This corresponds to the model (sigmax = sigmay = sigma)
  # X ~ N(Wx*z, sigma*I); Y ~ N(Wy*z, sigma*I)
  # This provides a simple comparison method for more
  # detailed dependency models.

  if ( is.null(Y) ) {
    res <- ppca.calculate(X, zDimension)
    phi <- list(total = diag(res$phi, nrow(X)))
    rownames(res$W) <- rownames(X)
    colnames(phi$total) <- rownames(phi$total) <- rownames(X)
    W <- list(total = res$W)    
  } else {
    # If second argument (Y) given, compute
    # pPCA with two concatenated data sets
    res <- ppca.calculate(rbind(X,Y), zDimension)

    # Make phi diagonal matrix
    phitotal <- diag(res$phi,(nrow(X) + nrow(Y)))

    # Variable names to W and phi
    rownames(res$W) <- c(rownames(X),rownames(Y))
    rownames(phitotal) <- c(rownames(X),rownames(Y))
    colnames(phitotal) <- rownames(phitotal)

    # Divide W and phi to X and Y parts
    phi <- list(X = phitotal[(1:nrow(X)),(1:nrow(X))], 
                Y = phitotal[-(1:nrow(X)),-(1:nrow(X))],
    	        total = phitotal)
    W <- list(X = as.matrix(res$W[(1:nrow(X)),]), 
              Y = as.matrix(res$W[-(1:nrow(X)),]), 
	      total = res$W)

  }
  # Note that if X, Y given then phi$X = phi$Y in the pCCA model
  # Here W corresponds to W$total of other functions when X, Y both given
  list(W = W, phi = phi)
}


ppca.calculate <- function (X, zDimension) {

 # FIXME: ensure that X is zero-mean

  # Probabilistic PCA
  # (See Tipping and Bishop 1999 / 3.2)

  # ML estimates W, sigma for probability model
  # X ~ N(Wz, sigma*I)
  # i.e. latent variable model with isotropic noise

  # Use full-rank if dimensionality is not specified
  zDimension <- ifelse(is.null(zDimension), nrow(X), zDimension)

  # eigenvalues D and eigenvectors U       
  duv <- svd(X)
  U <- duv$u                                                 
  D <- sort(duv$d, decreasing = TRUE)  

  # ML estimate for phi (in pPCA)
  phi <- sum(D[-seq(zDimension)])/(nrow(duv$u) - zDimension)

  # ML estimate for W, given phi (in pPCA)
  # Here set R <- I (R is an arbitrary orthogonal rotation matrix)  
  W <- as.matrix(U[, 1:zDimension])%*%sqrt(diag(D)[seq(zDimension), seq(zDimension)] - 
       		      phi * diag(1, zDimension, zDimension))

  if (zDimension == nrow(X)) {

    # If W is full-rank then the isotropic error term will disappear, assuming data X is gaussian
    # then X%*%t(X) i.e. cov(t(X)) approximates W%*%t(W) (since X ~ Wz and z ~ N(0,I))

    # Note rotational ambiguity for W, Z 
    cat("Full-rank PCA calculated, isotropic error term is zero.\n")
    W <- matrix.sqrt(cov(t(X)))
    phi <- 0
  }
  
  list(W = W, phi = phi)
}



Pcca.with.isotropic.margins <- function (X, Y, zDimension = 1, epsilon = 1e-6, delta = 1e6) {

  # epsilon and delta are convergence parameters
  # zDimension determines the dimensionality of the shared latent variable Z

  #  Dependency model
  #  X ~ N(Wx*z, sigmax*I)
  #  y ~ N(Wy*z, sigmay*I)
  #  i.e. isotropic marginals but in general  sigmax != sigmay
  # This is a special case of probabilistic factor analysis model

  # FIXME: ensure that X, Y have zero-mean (shift if necessary);
  # alternatively add mean parameter in the model

  res <- calc.pcca.with.isotropic.margins(X, Y, zDimension, epsilon = epsilon, delta = delta)
  phi <- res$phi  
    W <- res$W   


  colnames(phi$X) <- rownames(phi$X) <- rownames(X)
  colnames(phi$Y) <- rownames(phi$Y) <- rownames(Y)
  colnames(phi$total) <- rownames(phi$total) <- c(rownames(X), rownames(Y))
  
  list(phi = phi, W = W)

  # FIXME provide here proper DependencyModel object as in pcca, pfa and ppca
}


calc.pcca.with.isotropic.margins <- function (X, Y, zDimension, epsilon = 1e-6, delta = 1e6) {

  dat <- check.data(X, Y, zDimension)
  X <- dat$X
  Y <- dat$Y
  zDimension <- dat$zDimension
  
  # initialize
     inits <- initialize2(X, Y, zDimension, marginalCovariances = "isotropic")
      Dcov <- inits$Dcov
       Dim <- inits$Dim
         W <- inits$W  

  # FIXME: ensure that X, Y have zero-mean (shift if necessary);
  # alternatively add mean parameter in the model
  phi <- list(X = 1, Y = 1)
  
  # iterate until convergence:
  while (delta > epsilon) {

    W.old <- W
          
    ##########################################

    # Update Phi
    phi$X <- update_phi_isotropic(Dcov$X, W$X, phi$X, Dim$X) 
    phi$Y <- update_phi_isotropic(Dcov$Y, W$Y, phi$Y, Dim$Y)
          
    #######################################

    # Full CCA update for W 

        phi.inv <- list()
        phi.inv$X <- diag(rep(1/phi$X, Dim$X))
        phi.inv$Y <- diag(rep(1/phi$Y, Dim$Y))
        phi.inv$total <- diag(c(rep(1/phi$X, Dim$X), rep(1/phi$Y, Dim$Y)))

          M <- set.M.full2(W, phi.inv) # modified for G in Bishop's book	  
       beta <- set.beta.fullcov(M, W$total, phi.inv$total)
    W$total <- W.cca.EM(Dcov, M, beta)
        W$X <- matrix(W$total[1:Dim$X,], nrow = Dim$X)
        W$Y <- matrix(W$total[-(1:Dim$X),], nrow = Dim$Y)

    ########################################
          
    # check convergence (enough to check W)
    delta <- max(abs(as.vector(W$total - W.old$total)))
          
  }

  # FIXME: add 'intercept' field to DependencyModel?
  list(W = W, phi = phi)

}

