\name{load_mnist}
\alias{load_mnist}
\alias{show_digit}

\title{
  Load and visualize the MNIST digit recognition dataset.
}
\description{
  Load and visualize the digit recognition dataset in \url{http://yann.lecun.com/exdb/mnist/}. 
}
\usage{
  load_mnist(dirname)

  show_digit(arr784, col = gray(12:1/12), ...)
}

\arguments{
  \item{dirname}{
  The directory name where the .idx files reside, relative to the current working directory.
}
\item{arr784}{
  A row of the feature matrix. See example.
}
  \item{col}{
  A color palette.
}
  \item{\dots}{
  Further arguments passed to \code{image}.
}
}


\details{
  \code{load_mnist} loads the four \emph{.edx} files containing the MNIST data set from a local directory. It is assumed the files have been downloaded and decompressed manually.
  \emph{train-images-idx3-ubyte} is an example of the assumed file naming format.
  
  \code{show_digit} visualizes a particular digit from the data.
}

\value{
  \code{load_mnist} will create a \emph{test} and \emph{train} objects in the global environment. 
  These will have the features and the class of each digit in the dataset.
}

\references{
Y. LeCun, L. Bottou, Y. Bengio, and P. Haffner. "Gradient-based learning applied to document recognition." Proceedings of the IEEE, 86(11):2278-2324, November 1998. 
}

\author{
  Brendan O'Connor
}


\examples{
\donotrun{
  load_mnist(dirname='../Data/mnist/')
  show_digit(train$x[5,])
}
}

