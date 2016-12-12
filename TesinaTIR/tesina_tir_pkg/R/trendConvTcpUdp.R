#' andamento numero flussi tcp e udp nel tempo
#params:
#'@param pcapName file pcap da leggere
#'@param tmin tempo minimo
#'@param tmax tempo massimo
#'@param step dimensione bins
#'@return dataframe composto dalle seguenti colonne: \cr
#'l_time: valori temporali dei bins \cr
#'l_tcp_flux: numero di frame tcp \cr
#'l_udp_flux: numero di frame tcp \cr
#'@export
#'@examples
#'library(TesinaTIR)
#'pcapName<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'tmax<-5
#'step_size<-1
#'n_flux<-trendConvTcpUdp(pcapName,0,tmax,step_size)
trendConvTcpUdp<-function(pcapName,tmin,tmax,step){
  
  #dimensionamento array
  l_time<-array(dim=tmax)
  l_tcp_flux<-array(dim=tmax)
  l_udp_flux<-array(dim=tmax)
  
  #ogni step secondi
  for ( t in seq(tmin,tmax,step)){
    time_filter<-'<=frame.time_relative<='
    time_filter<-paste(as.character(t),time_filter)
    time_filter<-paste(time_filter,as.character(t+step))
    
    tcmd_tcp<-"tshark -r "
    tcmd_tcp<-paste(tcmd_tcp, pcapName)
    tcmd_tcp<-paste(tcmd_tcp, " -z  conv,tcp,'",time_filter,"'", sep="")
    tcmd_tcp<-paste(tcmd_tcp, " -q |grep '<->' | wc -l")
    
    tcmd_udp<-"tshark -r "
    tcmd_udp<-paste(tcmd_udp, pcapName)
    tcmd_udp<-paste(tcmd_udp, " -z conv,udp,'",time_filter,"'", sep="")
    tcmd_udp<-paste(tcmd_udp, " -q |grep '<->' | wc -l")
    
    stcp_flux<-system(tcmd_tcp,intern=TRUE)
    tcp_flux<-as.numeric(stcp_flux)
    
    sudp_flux<-system(tcmd_udp,intern=TRUE)
    udp_flux<-as.numeric(sudp_flux)
    
    l_tcp_flux[t+1]<-tcp_flux
    l_udp_flux[t+1]<-udp_flux
    l_time[t+1]<-t    
  }
  return(data.frame(l_time,l_tcp_flux,l_udp_flux))
}