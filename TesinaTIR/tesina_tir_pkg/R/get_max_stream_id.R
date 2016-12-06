#' stream id massimo del pcap \cr
#' 
#'stream id va da 0 a 'MAX(tcp.stream)tcp.stream'
#parameters
#'@param filename: nome del file di input
#'@param protocol: protocollo
#'@return stream_id massimo del pcap
#'@export
#'@examples
#'library(TesinaTIR)
#'filename<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'max_stream_id_tcp<-get_max_stream_id(filename,'tcp')
#'max_stream_id_udp<-get_max_stream_id(filename,'udp')

get_max_stream_id<-function(filename,protocol){
  
  if(is.na(match(protocol,c('tcp','udp')))){
    stop("protocol must be 'tcp' or 'udp'")
  }    
  s<-paste("tshark -r" ,filename,  "-z io,stat,0,'MAX(",protocol,".stream)",protocol,".stream' -q |grep '<>' | sed 's/[|<>]//g' ",sep="")  
  max_stream_id<-system(paste("tshark -r " ,filename,  " -z io,stat,0,'MAX(",protocol,".stream)",protocol,".stream' -q |grep '<>' | sed 's/[|<>]//g' ",sep=""),intern=TRUE)
  #eliminazione degli spazi successivi e precedenti
  max_stream_id<-stringr::str_trim(max_stream_id)
  #split sugli spazi interni 
  max_stream_id<-strsplit(max_stream_id, " ")[[1]]
  #memorizzazione ultimo campo
  max_stream_id<-as.numeric(max_stream_id[[length(max_stream_id)]])
  return(max_stream_id)
}
