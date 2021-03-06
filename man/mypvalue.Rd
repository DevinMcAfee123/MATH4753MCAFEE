% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mypvalue.R
\name{mypvalue}
\alias{mypvalue}
\title{P value}
\usage{
mypvalue(t0, xmax = 4, n = 20, alpha = 0.05)
}
\arguments{
\item{t0}{t calc}

\item{xmax}{the limit of the plot}

\item{n}{sample size}

\item{alpha}{for a 100(1-alpha)% confidence interval}
}
\value{
p value and the quantile defining the rejection regions. Creates a plot showing the distribution of T along with the rejection regions and a shaded area representing the pvalue.
}
\description{
Title
}
\examples{
\dontrun{mypvalue(tcalc,n=30,alpha=0.05)}
}
