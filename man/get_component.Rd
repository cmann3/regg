% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_components.R
\name{get_component}
\alias{get_component}
\alias{get_cooks}
\alias{get_df}
\alias{get_fitted}
\alias{get_hat}
\alias{get_model_matrix}
\alias{get_offset}
\alias{get_resids}
\alias{get_weights}
\alias{get_x}
\alias{get_y}
\alias{has_intercept}
\title{Obtain & Extract Regression Model Components}
\usage{
get_cooks(x)

get_df(x, write = TRUE)

get_fitted(x)

get_hat(x)

get_model_matrix(x, write = TRUE)

get_offset(x, write = TRUE)

get_resids(x)

get_weights(x, write = TRUE)

get_x(x, write = TRUE)

get_y(x, write = TRUE)

has_intercept(x, write = TRUE)
}
\arguments{
\item{x}{\code{rgo_model} object}

\item{write}{should the requested object be inserted in \code{x}, if it not found}

\item{...}{objects passed to methods}
}
\value{
the requested object, usually numeric vector or matrix
}
\description{
These functions are intended to be used inside of methods to extract regression
components - such as the X or y matrices - for fitting.
}
\section{Functions}{
\itemize{
\item \code{get_cooks()}: Obtain the cook's distance.

\item \code{get_df()}: Obtain the number of degrees of freedom of the residual

\item \code{get_fitted()}: Obtain the regression fitted values

\item \code{get_hat()}: Obtain the leverage values for the regression.

\item \code{get_model_matrix()}: Obtain the model matrix associated with \code{\link{reg_select}}.

\item \code{get_offset()}: Obtain the regression offset

\item \code{get_resids()}: Obtain the regression residuals.

\item \code{get_weights()}: Obtain the vector of weights

\item \code{get_x()}: Obtain the X matrix of independent variables

\item \code{get_y()}: Obtain the y matrix or vector of dependent variables

\item \code{has_intercept()}: Does the model contain an intercept?

}}
