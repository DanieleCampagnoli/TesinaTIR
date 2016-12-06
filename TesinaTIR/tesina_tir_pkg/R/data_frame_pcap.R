#' dati e frame inviati tcp
#parameters:
#'@param pcapName: nome del file pcap
#'@param fluxType: protocollo del flusso
#'@return list in cui list[0]= numero frame list[1]= byte inviati
#'@export
#'
#'@examples
#'library(TesinaTIR)
#'#dati e frame inviati tcp
#'pcapName<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'res<-dataFrame(pcapName,'tcp')
#'tcp_frame<-res[[1]]
#'tcp_data<-res[[2]]
#'print('totale frame tcp:')
#'print(tcp_frame)
#'print('totale dati tcp:')
#'print(tcp_data)
#'#dati e frame inviati udp
#'res<-dataFrame(pcapName,'udp')
#'udp_frame<-res[[1]]
#'udp_data<-res[[2]]
#'print('totale frame udp:')
#'print(udp_frame)
#'print('totale dati udp:')
#'print(udp_data)

dataFrame<-function(pcapName,fluxType){
  #costruzione filtro
  #time_filter<-'<=frame.time_relative<='
  #time_filter<-paste(as.character(time_start),time_filter)
  #time_filter<-paste(time_filter,as.character(time_end))
  
  tcmd<-"tshark -r "
  tcmd<-paste(tcmd, pcapName)
  #tcmd<-paste(tcmd, " -z io,stat,0,'",fluxType," and ",time_filter,"'", sep = "")
  tcmd<-paste(tcmd, " -z io,stat,0,'",fluxType,"'", sep = "")
  tcmd<-paste(tcmd, " -q |grep '<>' | sed 's/[<>]//g'| tr '|' '\n' | xargs")
  
  dump<-system(tcmd, intern=TRUE) 
  dump<- strsplit(dump, " ")[[1]]
  
  frame<-dump[[length(dump)-1]] 
  frame<-as.numeric(frame)
  
  data<- dump[[length(dump)]] 
  data<-as.numeric(data)
  
  return(list(frame,data))
}
