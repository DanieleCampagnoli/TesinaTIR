#' andamento flussi tcp e udp nell'intervallo temporale [tmin,tmax]
#params:
#'@param pcapName file pcap da leggere
#'@param tmin tempo minimo
#'@param tmax tempo massimo
#'@param step dimensione bins
#'@return dataframe composto dalle seguenti colonne: \cr
#'l_time: valori temporali dei bins \cr
#'l_tcp_frame: numero di frame tcp \cr
#'l_udp_frame: numero di frame udp \cr
#'l_udp_data: byte inviati da flussi udp \cr
#'l_tcp_data: byte inviati da flussi tcp \cr
#'l_tcp_duplicate_ack: numero di duplicate ack del tcp
#'@export
#'@examples
#'library(TesinaTIR)
#'pcapName<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'tmax<-5
#'step_size<-1
#'campTcpUdp<-time_values_TcpUdp(pcapName,0,tmax,step_size)

#ex estraiDatiTcpUdp
time_values_TcpUdp<-function(pcapName,tmin,tmax,step){
  
  #dimensionamento array
  l_time<-array(dim=tmax)
  l_tcp_frame<-array(dim=tmax)
  l_tcp_data<-array(dim=tmax)
  l_udp_frame<-array(dim=tmax)
  l_udp_data<-array(dim=tmax)
  l_tcp_duplicateAck<-array(dim=tmax)
  
  s_step<-gsub("\\.",",",as.character(step))
  
  tcmd<-"tshark -r "
  tcmd<-paste(tcmd, pcapName)
  tcmd<-paste(tcmd, " -z  io,stat,'",s_step,"','tcp','udp'",sep="")
  tcmd<-paste(tcmd, ",'COUNT(tcp.analysis.duplicate_ack)tcp.analysis.duplicate_ack'",sep="")
  tcmd<-paste(tcmd, " -q | grep '<>'")
  
  tshark_out<-system(tcmd,intern=TRUE)
  
  #ogni step secondi
  sequenza = seq(tmin,tmax,step)
  for (pos in sequenza){
    l_time[pos+1]<-pos
    dati = tshark_out[pos+1]
    dati_split =strsplit(dati, "[|]")
    
    l_tcp_frame[pos+1]<-as.numeric(dati_split[[1]][3])
    l_tcp_data[pos+1]<-as.numeric(dati_split[[1]][4])
    l_udp_frame[pos+1]<-as.numeric(dati_split[[1]][5])
    l_udp_data[pos+1]<-as.numeric(dati_split[[1]][6])
    l_tcp_duplicateAck[pos+1]<-as.numeric(dati_split[[1]][7])
  }
  
  return(data.frame(l_time,l_tcp_frame,l_tcp_data,l_udp_frame,l_udp_data,l_tcp_duplicateAck))
}
