#' genera tutti gli stream id di un pcap e di un protocollo. \cr
#' 
#' Lo stream id è un id di flusso in wireshark.
#parameters
#'@param max_stream_id stream_id massimo
#'@return array che contiene gli stream id del pcap
#'@export
#'@examples
#' # 10 è lo stream id massimo
#' streams<-get_streams_id(10)
get_streams_id<-function(max_stream_id){
  return(seq(0,max_stream_id,1))
}