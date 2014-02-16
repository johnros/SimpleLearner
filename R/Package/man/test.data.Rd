\name{test.data}
\alias{test.data}
\docType{data}
\title{
  sleaner test data.
}
\description{
  Synthetic test data to demonstrate performance of the basis learner algorithm in the references.
}

\usage{data('test.data')}

\format{
  The format is:
List of 2
 $ X: num [1:500, 1:15] -0.341 0.678 -0.866 -0.106 0.369 ...
 $ Y: Factor w/ 2 levels "-1","1": 2 2 1 2 1 1 2 1 2 1 ...
 - attr(*, "header")=List of 3
  ..$ description: chr "MATLAB 5.0 MAT-file, Platform: PCWIN64, Created on: Mon Apr 15 17:12:07 2013                                                "
  ..$ version    : chr "5"
  ..$ endian     : chr "little"
}

\references{
  Livni, Roi, Shai Shalev-Shwartz, and Ohad Shamir. "A Provably Efficient Algorithm for Training Deep Networks." arXiv:1304.7045 
}

\keyword{datasets}
