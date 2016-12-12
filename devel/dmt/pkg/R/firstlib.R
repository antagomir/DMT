#' @import methods
#' @import mvtnorm
#' @import MASS
#' @importFrom Matrix image
#' @importFrom Matrix cov2cor
#' @importFrom Matrix update
#' @importFrom Matrix nearPD
#' @import stats
#' @import graphics
.onAttach <- function(lib, pkg)
{
packageStartupMessage("\ndmt Copyright (C) 2008-2016 Leo Lahti and Olli-Pekka Huovilainen.")
}

# "Science may set limits to knowledge, but should not set limits to
# imagination."
# - Bertrand Russell
