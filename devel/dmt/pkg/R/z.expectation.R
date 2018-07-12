# "I am among those who think that science has great beauty. A scientist
# in his laboratory is not only a technician: he is also a child placed
# before natural phenomena which impress him like a fairy tale."
# - Marie Curie

#' @title Expectation of the latent variable.
#' @description Calculates expectation of the latent variable,
#'  given data and model parameters.
#' @param model \linkS4class{DependencyModel}
#' @param X Data set.
#' @param Y Optional, second data set. Used in dependency models where two
#' co-occurring observations are assumed to stem from a shared latent variable.
#' @return Returns a matrix (latent features x samples).
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("dmt") for references.
#' @keywords utilities
#' @examples
#' \dontrun{ 
#'   library(dmt)
#'   data(modelData)
#'   res <- pfa(X, Y, zDimension = 2)
#' }
z.expectation <- function (model, X, Y = NULL) {

    W <- getW(model)
  phi <- getPhi(model)

  # Center data
  X <- t(centerData(t(X), rm.na = TRUE))
  
  if (!is.null(Y)){

    Y <- t(centerData(t(Y), rm.na = TRUE))

    phi.inv <- list(X = solve(phi$X), Y = solve(phi$Y))
    S <- set.M.full2(W, phi.inv)

    return( S%*%(t(W$X)%*%phi.inv$X%*%X + t(W$Y)%*%phi.inv$Y%*%Y) )
  } else {
    phi.inv <- solve(phi$total)
    S <- set.M.full(W$total, phi.inv)
    return(S %*% (t(W$total) %*% phi.inv %*% X))
  }
}
