\name{predict.slearner}
\alias{predict.slearner}
\alias{summary.slearner}

\title{
  Predict method for slearner class objects.
}

\description{
  Convenience wrappers of common methods for \emph{slearner} class objects.
}

\usage{
  \method{predict}{slearner}(object, newdata, ...)
  
  \method{summary}{slearner}(object, ...)
}

\arguments{
  \item{object}{
 An \emph{slearner} class object encapsulating some fitted model with an existing \code{\link{predict}} method.
}
  \item{newdata}{
  A numeric design matrix. The number of columns and their ordering has to be the exact same as in the matrix used when fitting the predictor.
}
  \item{\dots}{
  Further arguments to be passed to appropriate methods.
}
}

\details{
  Convenience wrappers around of common methods for \emph{slearner} class objects.
  The behaviour and output depends on the type of fitted object.
  
  If \code{predict} is called on the output of \code{\link{svm.slearner}}, then \code{\link[e1071]{predict.svm}} will be called.
  
  If \code{summary} is called on the output of \code{\link{svm.slearner}}, then \code{\link[e1071]{summary.svm}} will be called.
  
}

\value{
  The output of a the appropriate method. 
}

\author{
 Jonathan Rosenblatt
}




\seealso{
  \code{\link[e1071]{predict.svm}}, \code{\link[e1071]{summary.svm}}
}

\examples{
## See under svm.sleaner()
}

