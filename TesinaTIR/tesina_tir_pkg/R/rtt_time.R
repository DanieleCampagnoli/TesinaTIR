#source('./R/tcp_ack_rtt.R')

#' metriche ack rtt del tcp sull'intervallo temporale [tmin,tmax] 
#' 
#'Questa funzione calcola media,varianza, valore massimo e minimo di ack rtt.\cr
#params: 
#'@param tmin tempo minimo
#'@param tmax tempo massimo
#'@param step dimensione dei bin
#'@param filename nome del file
#'@return dataframe che contiene le seguenti colonne \cr
#' l_time:valore temporale del bin \cr
#' l_max: tempo massimo nel bin \cr
#' l_min: valore minimo nel bin \cr
#' l_avg: valore medio nel bin \cr
#' l_var; varianza nel bin 
#'@seealso \code{\link[TesinaTIR]{tcp_ack_rtt}} la funzione dipende da questa funzione
#'@export
#'@examples
#'library(TesinaTIR)
#'tmax<-5
#'bin_size<-1
#'filename<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'rtt_tcp_intervals<-rtt_time(0,tmax,bin_size,filename)
rtt_time<-function (tmin,tmax,step,filename){
  #contiene i bin
  l_time_bins=seq(tmin,tmax,step)
  #metriche, non posso sapere in anticipo la dimensione 
  #perchè alcuni bin non hanno nessuna osservazione e ho scelto
  #di non avere valori null
  l_avg=c()
  l_min=c()
  l_max=c()
  l_var=c()
  l_time=c()
  
  for ( t in l_time_bins ){
    rtts<-TesinaTIR::tcp_ack_rtt(t,t+step,filename)
    #se non ho valori di rtts passo alla prossima iterazione
    if(length(rtts)==0){
      next  
    }
    # rtt medio nel tempo di tutti i flussi
    l_avg<-c(l_avg,mean(rtts))
    # rtt max
    l_max<-c(l_max,max(rtts))
    #rtt min
    l_min<-c(l_min,min(rtts))
    #varianza
    l_var<-c(l_var,var(rtts))
    #bin
    l_time=c(l_time,t)
  }
  
  #ritorno un dataframe con i dati calcolati
  return(data.frame(l_time,l_avg,l_min,l_max,l_var))
}
