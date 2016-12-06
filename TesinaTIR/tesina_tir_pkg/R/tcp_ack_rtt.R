#require(stringr)
#' estraggo valori ack rtt del tcp nell'intervallo temporale [tmin,tmax] su tutti i flussi
#params:
#'@param tmin tempo minimo
#'@param tmax tempo massimo
#'@param filename: nome del file
#'@return array con gli rtt
#'@export
#'@examples
#'library(TesinaTIR)
#'t<-1
#'step<-1
#'filename<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'rtt_values<-tcp_ack_rtt(t,t+step,filename)
tcp_ack_rtt<-function(tmin,tmax,filename){
  #preparazione filtro sul tempo relativo
  time_filter<-'<=frame.time_relative<='
  time_filter<-paste(as.character(tmin),time_filter)
  time_filter<-paste(time_filter,as.character(tmax))
  #preparazione stringa che contiene il comando
  #questo comando preleva i valori di tcp.analysis.ack_rtt in un intervallo temporale
  #esempio: tshark -r GazzettaDiModena_20161120.pcap  -Y 'tcp and 0<=frame.time_relative<=1 and tcp.analysis.ack_rtt' -T fields  -e tcp.analysis.ack_rtt
  #questo comando restituisce un exit code 1 se non ci nel pcap che soddisfano il filtro , quindi è necessario aggiungere i seguenti comandi per gestire questo
  #caso particolare:
  #grep -v -e '^$' || echo 'NO_DATA'
  #se tshark ha un exit code 1 viene restituito 'NO_DATA'
  tcmd<-paste("tshark -r ",filename,"-Y 'tcp and ",time_filter," and tcp.analysis.ack_rtt' -T fields  -e tcp.analysis.ack_rtt  | grep -v -e '^$' || echo 'NO_DATA'")
  ret<-system(tcmd,intern = TRUE)
  #gestione del caso in cui non ci siano rilevazioni di tcp.analysis.ack_rtt
  if(ret[1]=='NO_DATA'){
    return (c())
  }
  #conversione rtt in numeric
  rtts<-as.numeric(ret)
  return (rtts)
}
