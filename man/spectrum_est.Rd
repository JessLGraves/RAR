% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectrum_est.r
\name{spectrum_est}
\alias{spectrum_est}
\title{Spectrum estimation}
\usage{
spectrum_est(df, method = c("pss", "whittle"), log_transform = c(TRUE,
  FALSE), ...)
}
\arguments{
\item{df}{dataframe containing residuals from  extended cosine model from RAR() and date and time.}

\item{method}{specifies if smooth or penalized spline should be used. Default is smooth.}

\item{log_transform}{specifies if log power spectrum should be calculated. TRUE is default}

\item{...}{additional parameters from astsa::mvspec() or gss::gssanova()}
}
\description{
Internal function used to estimate the power spectrum of residuals estimated from the RAR().
}
\author{
Jessica Graves
}
