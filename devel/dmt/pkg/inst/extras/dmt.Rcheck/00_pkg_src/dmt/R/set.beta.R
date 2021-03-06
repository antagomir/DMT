set.beta.isotropic <- function (M, W, phi) {
  # assuming isotropic marginal covariance
  M%*%t(W)/phi

}


set.beta.fullcov <- function (M, W, phi.inv) {
  # assuming full marginal covariance
  # as in section 4.1 EM algorithm from BachJordan probCCA paper
  M%*%t(W)%*%phi.inv

}


