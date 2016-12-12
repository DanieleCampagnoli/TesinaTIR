#' reperisce le tracce relative ad un flusso e ne calcola il tempo relativo rispetto all'inizio del flusso.
#parameters:
#'@param stream_id id del flusso
#'@param protocol protocollo
#'@param filename nome del file da leggere
#'@return array con i tempi relativi all'inizio del flusso delle tracce del flusso identificato da stream_id
#'@export
#'@examples
#'library(TesinaTIR)
#'stream<-1
#'protocol<-'tcp'
#'filename<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'trace_tcp<-get_conv_timeRelative_frameLen(stream,protocol,filename)
#'stream<-1
#'protocol<-'udp'
#'filename<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'trace_udp<-get_conv_timeRelative_frameLen(stream,protocol,filename)
get_conv_timeRelative_frameLen<-function(stream_id,protocol,filename){
  scmd<-paste("tshark -r ",filename) 
  scmd_time_relative<-paste(scmd," -Y '",protocol,".stream==",stream_id,"' -Tfields -e frame.time_relative",sep="")
  time_relative<-system(scmd_time_relative,intern=TRUE)
  time_relative<-as.numeric(time_relative)
  
  scmd_start_time<-paste(scmd," -z io,stat,0,'MIN(frame.time_relative)frame.time_relative 
                         and ",protocol,".stream==",stream_id,"' -q | grep '<>' | sed 's/<>//g' |sed 's/|//g'",sep="")
  start_time<-system(scmd_start_time,intern=TRUE)
  start_time<-stringr::str_trim(start_time)
  #split sugli spazi interni 
  start_time<-strsplit(start_time, " ")[[1]]
  #memorizzazione ultimo campo
  start_time<-as.numeric(start_time[[length(start_time)]])
  
  time_relative<-time_relative-start_time
  return(time_relative)
}