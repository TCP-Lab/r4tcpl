% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hedmad.R
\name{ccat}
\alias{ccat}
\title{Combine cat}
\usage{
ccat(..., sep = "", end = "\\n", vecsep = ", ", vecstart = "(", vecend = ")")
}
\arguments{
\item{...}{Values to concatenate then print.}

\item{sep}{Passed onto \code{paste0}. Defaults to an empty string (no separation).}

\item{end}{Character to add to the end of the pasted string. Defaults to \verb{\\\\n}}

\item{vecsep}{Separator of vector elements. Defaults to "\verb{, }"}

\item{vecstart}{Starting character(s) of vector elements. Defaults to "\code{(}".}

\item{vecend}{Ending character(s) of vector elements. Defaults to "\verb{)}".}
}
\description{
Combine then print to stdout the input variables.
}
\details{
This similar to running \code{cat(paste0(...))}. By default adds a newline
at the end of the concatenated string, but it can be overridden through the
\code{end} argument.
Gracefully handles printing vectors, by default showing them as comma-separated
values, e.g. "\verb{(a , b, c, ...)}".
}
