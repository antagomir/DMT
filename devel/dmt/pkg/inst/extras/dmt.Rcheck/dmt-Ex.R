pkgname <- "dmt"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "dmt-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('dmt')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("DependencyModel-class")
### * DependencyModel-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: DependencyModel-class
### Title: Class DependencyModel
### Aliases: DependencyModel-class getModelMethod
###   getModelMethod,DependencyModel-method getParams
###   getParams,DependencyModel-method getPhi getPhi,DependencyModel-method
###   getScore getScore,DependencyModel-method getW
###   getW,DependencyModel-method getWindowSize
###   getWindowSize,DependencyModel-method getZ getZ,DependencyModel-method
### Keywords: classes

### ** Examples

  #data(modelData) # Load example data X, Y
  #model <- fit.dependency.model(X, Y)
  ## Getting the latent variable Z when it has been calculated with the model
  ## getZ(model)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("DependencyModel-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("centerData")
### * centerData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: centerData
### Title: Center Data Matrix
### Aliases: centerData
### Keywords: maths utilities

### ** Examples


#centerData(X)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("centerData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("compare.estimate.and.truth")
### * compare.estimate.and.truth

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: compare.estimate.and.truth
### Title: Measuring Model Accuracy
### Aliases: compare.estimate.and.truth
### Keywords: utilities

### ** Examples

 
# toy <- generate.toydata(N = N, 
#           zDim = zdim, xDim = xdim, yDim = ydim,
#           marginal.covariances = marginalCovariances,
#           priors = priors)
# res <- fit.dependency.model(toy$X, toy$Y,
#                      zDimension = zdim,
#              marginalCovariances = marginalCovariances,
#		      priors = priors,
#                     matched = FALSE)
# vec <- compare.estimate.and.truth(res, toy)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("compare.estimate.and.truth", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dependency.score")
### * dependency.score

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dependency.score
### Title: Dependency Score
### Aliases: dependency.score
### Keywords: maths utilities

### ** Examples

#dependency.score(model)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dependency.score", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("drCCAcombine")
### * drCCAcombine

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: drCCAcombine
### Title: A function to combine several data sets
### Aliases: drCCAcombine
### Keywords: multivariate

### ** Examples



    # data(expdata1)
    # data(expdata2)
    # drCCAcombine(list(expdata1,expdata2),0,2,3)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("drCCAcombine", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("expdata")
### * expdata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: expdata
### Title: Example data for drCCA
### Aliases: expdata expdata1 expdata2
### Keywords: datasets

### ** Examples

#data(expdata1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("expdata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit.dependency.model")
### * fit.dependency.model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit.dependency.model
### Title: Fit dependency model between two data sets.
### Aliases: fit.dependency.model
### Keywords: math

### ** Examples

#  data(modelData) # Load example data X, Y
#  # probabilistic CCA
#  model <- pcca(X, Y)
#  # dependency model with priors (W>=0; Wx = Wy; full marginal covariances)
#  model <- fit.dependency.model(X, Y, zDimension = 1, 
#      	 	      priors = list(W = 1e-3, Nm.wx.wy.sigma = 0), 
# 			      marginalCovariances = "full")
#  # Getting the latent variable Z when it has been calculated with the model
#  # getZ(model)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit.dependency.model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("generate.toydata")
### * generate.toydata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: generate.toydata
### Title: Toy Data Generation
### Aliases: generate.toydata
### Keywords: utilities

### ** Examples

 toy <- generate.toydata(N = 100, 
                   zDim = 1, xDim = 3, yDim = 3, 
                   marginal.covariances = "full") 




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("generate.toydata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("modelData")
### * modelData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: modelData
### Title: An example data set
### Aliases: X Y modelData
### Keywords: datasets

### ** Examples

#data(modelData)
#model <- fit.dependency.model(X, Y)
#model





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("modelData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pcca")
### * pcca

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pcca
### Title: Probabilistic CCA
### Aliases: pcca
### Keywords: math

### ** Examples

# pcca()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pcca", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pfa")
### * pfa

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pfa
### Title: Probabilistic Factor Analysis
### Aliases: pfa
### Keywords: utilities

### ** Examples

#



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pfa", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pfa.neg.log.likelihood")
### * pfa.neg.log.likelihood

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pfa.neg.log.likelihood
### Title: Likelihood for the models.
### Aliases: pfa.neg.log.likelihood
### Keywords: utilities

### ** Examples


library(dmt)
# Generate toydata
N <- 100
xdim <- 10
zdim <- 3
toy <- generate.toydata(N = N, zDim = zdim, xDim = xdim, yDim = xdim, 
               marginal.covariances = "diagonal")
# Estimate model parameters
res <- pfa(toy$X, zDimension = zdim)
W <- res@W$total
phi <- res@phi$total
# wtw <- crossprod(t(W)) # is the same as W * t(W)
# Calculate negative log-likelihood for the model
L <- pfa.neg.log.likelihood(W, phi,toy$X)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pfa.neg.log.likelihood", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotVar")
### * plotVar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plotVar
### Title: Data-specific and shared variance for several dimensionalities
### Aliases: plotVar
### Keywords: multivariate

### ** Examples



#       data(expdata1)
#       data(expdata2)
#       r <- regCCA(list(expdata1,expdata2))#

#       plotVar(list(expdata1,expdata2),r,c(1:2),4)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotVar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ppca")
### * ppca

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ppca
### Title: Probabilistic PCA
### Aliases: ppca
### Keywords: math

### ** Examples

# ppca()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ppca", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("regCCA")
### * regCCA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: regCCA
### Title: Generalized Canonical Correlation Analysis
### Aliases: regCCA
### Keywords: multivariate

### ** Examples



#     data(expdata1)
#     data(expdata2)

     #performing regCCA
#    test <- regCCA(list(expdata1,expdata2),0) #list of result is stored in test
 
#     test$eigval #generalized canonical correlations
#     test$eigvecs #gCCA components
#     test$proj #projection of data onto gCCA components
#     test$meanvec #array of columnwise mean vectors for each matrix
#     test$white # array of whitening matrix



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("regCCA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("sharedVar")
### * sharedVar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: sharedVar
### Title: Shared variation retained in the combined drCCA representation
### Aliases: sharedVar
### Keywords: multivariate

### ** Examples



  #     data(expdata1)
  #     data(expdata2)
  #     r <- regCCA(list(expdata1,expdata2))

  #     sharedVar(list(expdata1,expdata2),r,4)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("sharedVar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("specificVar")
### * specificVar

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: specificVar
### Title: Data-specific variation retained in the combined drCCA
###   representation
### Aliases: specificVar
### Keywords: multivariate

### ** Examples



#       data(expdata1)
#       data(expdata2)
#       r <- regCCA(list(expdata1,expdata2))

#       specificVar(list(expdata1,expdata2),r,4)






base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("specificVar", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("z.expectation")
### * z.expectation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: z.expectation
### Title: Expectation of the latent variable.
### Aliases: z.expectation
### Keywords: utilities

### ** Examples

## Not run: 
##D  
##D   library(dmt)
##D   data(modelData)
##D   res <- pfa(X, Y, zDimension = 2)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("z.expectation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
