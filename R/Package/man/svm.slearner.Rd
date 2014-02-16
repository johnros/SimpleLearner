\name{svm.slearner}
\alias{svm.slearner}

\title{
  Fitting an svm classifier with augmented basis.
}

\description{
  This is a convenience wrapper around \code{\link[e1071]{svm}} using a deep neural basis grown using \code{\link{makeBasis.slearner}}.
}
\usage{
  svm.slearner(x, y, widths, train.ind, lambdas = 2^(1:6), 
  tunecontrol=tune.control(sampling="fix"), ...)
}

\arguments{
  \item{x}{
A numeric design matrix.
}
  \item{y}{
The dependent variable. Assumed to be a two class factor.
}
  \item{widths}{
A numeric vector with the number of nodes in each layer of the deep neural architecture.
}
  \item{train.ind}{
An optional logical vector marking observations to be used for growing the basis.
}
  \item{lambdas}{
A numeric vector of values of the svm regularization parameter passed as the \emph{cost} parameter in \code{\link[e1071]{tune.svm}}.
}
\item{tunecontrol}{
A control object created by \code{link[e1071]{tune.control}} and passed to \code{\link[e1071]{tune.svm}}
}
  \item{\dots}{
Further arguments passed to \code{\link[e1071]{tune.svm}}
}
}

\details{
The function initially calls \code{\link{makeBasis.slearner}} to augment the initial design matrix with a deep neural architecture. 
If \emph{train.ind} is not specified, a random half of the data is used as a training set when growing the network.
The returned 
}
\value{
A list including:
  \item{fit}{An object of class \emph{tune} which is the output of \code{\link[e1071]{tune.svm}} called with the augmented basis.}
  \item{makeBasis}{A function object. Used by \code{\link{predict.slearner}}.}
}

\references{
For the basis augmentation:
  Livni, Roi, Shai Shalev-Shwartz, and Ohad Shamir. "A Provably Efficient Algorithm for Training Deep Networks." arXiv:1304.7045 


For the svm implementation:
  D. Meyer. Support Vector Machines: The interface to libsvm in Package
    e1071. Techische Universitat Wien, Austria, 2004.
}
\author{
Jonathan Rosenblatt
}
\note{
  This simple function can easily be extended to other types of supervised learning problems.  
}

\seealso{
  \code{\link{makeBasis.slearner}}
}

\examples{
  data('test.data')
  widths<- c(10,10)
  lambdas<-  2^seq(-10,3,length=50) 
  train.ind<- rep(FALSE, nrow(test.data$X))
  train.ind[1:250]<- TRUE
  slearner.fit<- svm.slearner(x=test.data$X, y=test.data$Y, train.ind=train.ind,
                               lambdas=lambdas, widths=widths)
  summary(slearner.fit)
  predict(slearner.fit)
}

\keyword{ neural }
\keyword{ regression }
