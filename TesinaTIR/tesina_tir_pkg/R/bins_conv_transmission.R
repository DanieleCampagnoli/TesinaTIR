#source('./R/get_conv_timeRelative_frameLen.R')

#' quante volte i flussi di tipo protocol hanno trasmesso nei bin temporali indicati da bins
#parameters:
#'@param streams_id  array dei flussi da considerare
#'@param protocol protocollo dei flussi
#'@param bins array dei bin temporali
#'@param step_size dimensione dei bin
#'@return array che contiene le frequenze dei bin 
#'@seealso \code{\link[TesinaTIR]{get_conv_timeRelative_frameLen}} la funzione dipende da questa funzione
#'@seealso \code{\link[TesinaTIR]{compute_bins_count}} la funzione dipende da questa funzione
#'@export

#'@examples
#'library(TesinaTIR)
#'filename<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'step_size<-1
#'#stream id massimi di udp e tcp
#'max_stream_id_tcp<-get_max_stream_id(filename,'tcp')
#'max_stream_id_udp<-get_max_stream_id(filename,'udp')
#'
#'#generazione di tutti gli stream id udp e tcp
#'streams_id_tcp<-get_streams_id(max_stream_id_tcp)
#'streams_id_udp<-get_streams_id(max_stream_id_udp)
#'
#'#calcolo durata massima conversazioni udp e tcp
#'max_duration_tcp<-max(get_conv_durations_TcpUdp(filename,'tcp'))
#'max_duration_udp<-max(get_conv_durations_TcpUdp(filename,'udp'))
#'
#'#calcolo bin temporali udp e tcp
#'bins_tcp=seq(0,max_duration_tcp,step_size)
#'bins_udp=seq(0,max_duration_udp,step_size)
#'#valori bin udp e tcp
#'bins_conv_transmission_tcp<-bins_conv_transmission(streams_id_tcp,'tcp',filename,bins_tcp,step_size)
#'bins_conv_transmission_udp<-bins_conv_transmission(streams_id_udp,'udp',filename,bins_udp,step_size)


bins_conv_transmission<-function(streams_id,protocol,filename,bins,step_size){
  total_bins_stream_data<-array(0,length(bins))
  for(stream in streams_id){
    print(paste('processo stream id numero: ',stream))
    
    stream_data<-TesinaTIR::get_conv_timeRelative_frameLen(stream,protocol,filename)
    bins_stream_data<-TesinaTIR::compute_bins_count(stream_data,bins,step_size)
    total_bins_stream_data<-total_bins_stream_data+bins_stream_data
  }
  return(total_bins_stream_data)
}