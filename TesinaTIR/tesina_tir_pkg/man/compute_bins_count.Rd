% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_bins_count.R
\name{compute_bins_count}
\alias{compute_bins_count}
\title{funzione che calcola le frequenze contenute nei bins}
\usage{
compute_bins_count(in_data, bins, step)
}
\arguments{
\item{in_data}{vettore che contiene la durata di ogni flusso}

\item{bins}{bins della distribuzione}

\item{step}{dimensione dei bins}
}
\value{
array con frequenze dei bins
}
\description{
funzione che calcola le frequenze contenute nei bins
}
\examples{
library(TesinaTIR)
step<-1
durations<-c(1,2,2,3,3,4)
bins<-seq(1,4,step)
compute_bins_count(durations,bins,step)
}

