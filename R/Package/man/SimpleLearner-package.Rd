\name{SimpleLearner-package}
\alias{SimpleLearner-package}
\alias{SimpleLearner}
\docType{package}
\title{ Basis learner for deep neural networks}

\description{ Augments a given basis with a data driven deep neural network architechture described in the reference.}
\details{
The package implements the basis augmentation algorithm in the reference. 
This is done in the \code{\link{makeBasis.slearner}} function.
The augmented basis, essentially a matrix object, can then be used for any learning algorith.
Currently, a two class svm learner is implemented by wrapping around \code{\link[e1071]{svm}}.
}

\author{
Jonathan Rosenblatt<john.ros.work@gmail.com> based on Matlab code by Ohad Shamir.

Maintainer: Jonathan Rosenblatt <john.ros.work@gmail.com>
}
\references{
  Livni, Roi, Shai Shalev-Shwartz, and Ohad Shamir. "A Provably Efficient Algorithm for Training Deep Networks." arXiv:1304.7045 
}
