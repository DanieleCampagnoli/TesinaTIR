#' durata di tutti i flussi udp/tcp:
#parameters:
#'@param filename file da leggere
#'@param protocol protocollo 
#'@return array che contiene la durata di tutti i flussi del protocollo specificato 
#'@export
#'@examples
#'library(TesinaTIR)
#'filename<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'#durate tcp
#'durations_tcp<-get_conv_durations_TcpUdp(filename,'tcp')
#'#durate udp 
#'durations_udp<-get_conv_durations_TcpUdp(filename,'udp')
get_conv_durations_TcpUdp<-function(filename,protocol){
  
  if(is.na(match(protocol,c('tcp','udp')))){
    stop("protocol must be 'tcp' or 'udp'")
  }

  #tshark -r GazzettaDiModena_20161120.pcap -q -z conv,tcp
  # con grep '<->' seleziono le righe che contengono il simbolo '<->'
  # con sed 's/<->//g' vado ad eliminare il simbolo '<->'
  # con tr -s vado a formattare l'output in modo che i campi siano divisi da uno spazio
   
  dump<-system(paste("tshark -r ",filename," -q -z conv,",protocol," |grep '<->' | sed 's/<->//g' |  tr -s ' '",sep=""),intern=TRUE)
  #contiene le durate dei flussi, ogni elemento rappresenta la durata di un flusso diverso
  durations<-array(dim=(length(dump)))
  #indice di durations
  i<-1
  #per ogni liena di output del comando
  for(d in dump){
    #eseguo split sullo spazio
    dump_split <- strsplit(d, " ")[[1]]
    #prelevo ultimo campo (duration)
    s<-dump_split[[length(dump_split)]]
    #sostituisco la virgola con il punto per il casting
    s<-gsub(",",".",s)
    #casting da stringa a numeric
    v<-as.numeric(s)
    #aggiorno il vettore durations
    durations[i]<- v
    i<-i+1
  }
  return(durations)
}
