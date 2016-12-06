#'conteggio flussi tcp
#parametri:
#'@param pcapName: nome del pcap
#'@param fluxType: protocollo del flusso
#'@return numero di flussi del protocollo fluxType
#'@export
#'@examples
#'library(TesinaTIR)
#'pcapName<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'n_tcp_flux<-fluxCount(pcapName, 'tcp')
#'n_udp_flux<-fluxCount(pcapName, 'udp')

fluxCount<-function(pcapName, fluxType){
  #costruzione filtro
  #time_filter<-'<=frame.time_relative<='
  #time_filter<-paste(as.character(time_start),time_filter)
  #time_filter<-paste(time_filter,as.character(time_end))
  
  #preparazione comando raggruppamento conversazioni nel range
  tcmd<-"tshark -r "
  tcmd<-paste(tcmd, pcapName)
  #tcmd<-paste(tcmd, " -z conv,",fluxType,",'", time_filter, "'", sep = "")
  tcmd<-paste(tcmd, " -z conv,",fluxType, sep = "")
  tcmd<-paste(tcmd, " -q | grep '<->' | wc -l")
  
  #escuzione comando
  sn_flux<-system(tcmd,intern=TRUE) 
  
  #conversione a numero
  n_flux<-as.numeric(sn_flux)
  
  return (n_flux)
}
