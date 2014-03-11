\name{svm.slearner}
\alias{svm.slearner}

\title{Fit a SVM Classifier Using a Deep Network Basis .}

\description{
  A convenience wrapper around \code{\link[LiblineaR]{LiblineaR}} using a deep neural basis grown using \code{\link{makeBasis.slearner}}.
}

\usage{
  svm.slearner(x, y, widths, train.ind, type, lambdas = 2^(1:6),
                 folds = 2, svd.method='exact', ...)
}

\arguments{
  \item{x}{A numeric design matrix.}
  \item{y}{The dependent variable. Assumed to be a two class factor.}
  \item{widths}{A numeric vector with the number of nodes in each layer of the deep neural architecture.}
  \item{train.ind}{An optional logical vector marking observations to be used for growing the basis.
}
\item{type}{Controls the type of validation. See Details.}
  \item{lambdas}{A numeric vector of values of the svm regularization parameter passed as the \emph{cost} parameter in \code{\link[LiblineaR]{LiblineaR}}}.
  \item{svd.method}{Passed to \code{\link{makeBasis.slearner}}.}
  \item{folds}{The number of cross validation folds in case \code{type="cross"}.}
  \item{\dots}{Further arguments passed to \code{\link[LiblineaR]{LiblineaR}}.}
  }


\details{
The function initially calls \code{\link{makeBasis.slearner}} to augment an initial design matrix with a "deep" architecture. 
If \code{type="fix"} is set, a train-test validation will be performed with \emph{train.ind} marking the training and test data. If missing, a random half-split will be used.
If \code{type="cross"}, a cross validation of the error will be performed with the number of folds specified in \code{folds}.}

\value{
A list including:
  \item{call}{The function call.}
  \item{fit}{A \code{\link[LiblineaR]{LiblineaR}} class object with the fitted model.}
  \item{lambdas}{The lambda values considered.}
  \item{type}{Train-test or cross validated.}
  \item{lambda}{The selected tunning parameter value.}
  \item{CV.misclass}{The test error rate.}
  \item{widths.requested}{The desired network architechture.}
  \item{widths.returned}{The fitted network architechture.}
  \item{makeBasis}{A function class object inteded to build the fitted network on new X matrices}
}

\references{
For the basis augmentation:
  Livni, Roi, Shai Shalev-Shwartz, and Ohad Shamir. "A Provably Efficient Algorithm for Training Deep Networks." arXiv:1304.7045 

For the LiblineaR:
  R.-E. Fan, K.-W. Chang, C.-J. Hsieh, X.-R. Wang, and C.-J. Lin. LIBLINEAR: A library for large linear classification Journal of Machine Learning Research 9(2008), 1871-1874.
}
\author{
Jonathan Rosenblatt
}
\note{
  An extension of the learning algorithm can easilly be extended by construcring the \code{X} matrix using \code{\link{makeBasis.slearner}} and then plugging it in any other learning strategy.
}

\seealso{
  \code{\link{makeBasis.slearner}}, \code{\link{summary.slearner}}, \code{\link{predict.slearner}}
}

\examples{
  data('test.data')
  widths<- c(10,10)
  lambdas<-  2^seq(-10,3,length=50) 
  train.ind<- rep(FALSE, nrow(test.data$X))
  train.ind[1:250]<- TRUE
  slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
                            type="fix", lambdas=lambdas, widths=widths)
  summary(slearner.fit)
  predict(slearner.fit, newx=test.data$X)
  
  \dontrun{
    rm(list=ls())
    load_mnist(dirname='../Data/mnist/')
    train.ind<- as.logical(rbinom(nrow(train$x), 1, 0.7))
    widths<- c(50,600,600)
    lambdas<-  2^seq(-2,6,length=50) 
    slearner.fit<- svm.slearner(x=train$x, y=train$y, train.ind=train.ind,
                                type="fix", lambdas=lambdas, widths=widths)
    predict(slearner.fit, newx=train$x)
    summary(slearner.fit)
  }
}

\keyword{ neural }
\keyword{ regression }
