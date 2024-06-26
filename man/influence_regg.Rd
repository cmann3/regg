% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_influence.R, R/get_reg.R
\name{influence_regg}
\alias{influence_regg}
\alias{cooks.distance.rgo_model}
\alias{dfbeta.rgo_model}
\alias{dfbetas.rgo_model}
\alias{hatvalues.rgo_model}
\alias{influence.rgo_model}
\alias{rstandard.rgo_model}
\alias{rstudent.rgo_model}
\alias{deviance.rgo_model}
\title{regg Deletion Diagnostics & Influence Measures}
\usage{
\method{cooks.distance}{rgo_model}(model, write = TRUE, ...)

\method{dfbeta}{rgo_model}(model, write = TRUE, ...)

\method{dfbetas}{rgo_model}(model, write = TRUE, ...)

\method{hatvalues}{rgo_model}(model, write = TRUE, ...)

\method{influence}{rgo_model}(model, write = TRUE, ...)

\method{rstandard}{rgo_model}(model, write = TRUE, type = c("sd.1", "predictive"), ...)

\method{rstudent}{rgo_model}(model, write = TRUE, ...)

\method{deviance}{rgo_model}(object, write = FALSE, ...)
}
\arguments{
\item{model}{\code{rgo_model} object}

\item{write}{should the requested object be inserted in \code{x}, if it not there}

\item{...}{objects passed to methods}
}
\value{
the object, extracted from \code{x}
}
\description{
Compute leave-one-out deletion diagnostics for \code{regg} models. See
\code{\link[stats]{influence.measures}} from the \code{stats} package.
}
