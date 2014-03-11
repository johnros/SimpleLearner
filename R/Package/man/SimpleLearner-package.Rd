\name{SimpleLearner-package}
\alias{SimpleLearner-package}
\alias{SimpleLearner}
\docType{package}

\title{ Basis Learner Algorithm for Deep Networks}

\description{ Augments a given basis with a data driven deep neural architechture described in the reference.}

\details{
The package implements the basis augmentation algorithm in the reference. 
This is done in the \code{\link{makeBasis.slearner}} function.
The augmented basis, essentially a matrix object, can then be used for any learning algorith.
A convenience wrapper for SVM is implemented in \code{\link{svm.slearner}}.}

\author{
Jonathan Rosenblatt<john.ros.work@gmail.com> based on Matlab code by Ohad Shamir.

Maintainer: Jonathan Rosenblatt <john.ros.work@gmail.com>
}
\references{
  Livni, Roi, Shai Shalev-Shwartz, and Ohad Shamir. "A Provably Efficient Algorithm for Training Deep Networks." arXiv:1304.7045 
}
