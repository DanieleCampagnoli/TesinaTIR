% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_streams_id.R
\name{get_streams_id}
\alias{get_streams_id}
\title{genera tutti gli stream id di un pcap e di un protocollo. \cr}
\usage{
get_streams_id(max_stream_id)
}
\arguments{
\item{max_stream_id}{stream_id massimo}
}
\value{
array che contiene gli stream id del pcap
}
\description{
Lo stream id è un id di flusso in wireshark.
}
\examples{
# 10 è lo stream id massimo
streams<-get_streams_id(10)
}

