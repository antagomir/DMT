#' @title Class DependencyModel
#' @description A Dependency model for one or two data sets.
#' @name DependencyModel-class
#' @aliases DependencyModel-class getW getPhi getScore getModelMethod getParams
#' getWindowSize getZ getW,DependencyModel-method getPhi,DependencyModel-method
#' getScore,DependencyModel-method getParams,DependencyModel-method
#' getWindowSize,DependencyModel-method getModelMethod,DependencyModel-method
#' getZ,DependencyModel-method
#' @docType class
#' @section Objects from the Class: Returned by
#' \code{\link{fit.dependency.model}}, \code{\link{ppca}}, \code{\link{pfa}},
#' and \code{\link{pcca}} functions. % and \code{\link{pcca.isotropic}}
#' @author Olli-Pekka Huovilainen \email{ohuovila@@gmail.com}
#' @keywords classes
#' @examples
#'   #data(modelData) # Load example data X, Y
#'   #model <- fit.dependency.model(X, Y)
#'   ## Getting the latent variable Z when it has been calculated with the model
#'   ## getZ(model)
NULL



#' Example data for drCCA
#' 
#' Randomly generated data set for the examples.  Data matrix with 2000 rows
#' and 6 columns.  Rows corresponds to the samples and each column corresponds
#' to a particular features.
#' 
#' A randomly generated data set for the example purpose only.
#' 
#' @name expdata
#' @aliases expdata1 expdata2
#' @docType data
#' @format A data matrix with 2000 rows and 6 columns. Column names and row
#' names are ommitted.
#' @keywords datasets
#' @examples
#' #data(expdata1)
NULL





#' An example data set
#' 
#' Preprocessed gene expression and gene copy number levels of 51 patients in
#' chromosome 17 for 10 genes. 
#' @name modelData
#' @aliases X Y
#' @docType data
#' @format \describe{ \item{X}{Gene expression levels in matrix form. Genes are
#' in columns and samples in rows.}
#' 
#' \item{Y}{Gene copy number levels in matrix form. Genes are in columns and
#' samples in rows.} }
#' @source Integrated gene copy number and expression microarray analysis of
#' gastric cancer highlights potential target genes.  Myllykangas et al.,
#' \emph{International Journal of Cancer}, vol. \bold{123}, \bold{no. 4}, pp.
#' 817--25, 2008.
#' @keywords datasets
#' @examples 
#' #data(modelData)
#' #model <- fit.dependency.model(X, Y)
#' #model
#' 
#' 
NULL



