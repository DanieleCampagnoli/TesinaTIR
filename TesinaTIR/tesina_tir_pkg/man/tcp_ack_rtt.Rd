% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcp_ack_rtt.R
\name{tcp_ack_rtt}
\alias{tcp_ack_rtt}
\title{estraggo valori ack rtt del tcp nell'intervallo temporale [tmin,tmax] su tutti i flussi}
\usage{
tcp_ack_rtt(tmin, tmax, filename)
}
\arguments{
\item{tmin}{tempo minimo}

\item{tmax}{tempo massimo}

\item{filename:}{nome del file}
}
\value{
array con gli rtt
}
\description{
estraggo valori ack rtt del tcp nell'intervallo temporale [tmin,tmax] su tutti i flussi
}
\examples{
library(TesinaTIR)
t<-1
step<-1
filename<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
rtt_values<-tcp_ack_rtt(t,t+step,filename)
}

