#'terminazione flusso con reset
#'
#'Questa funzione calcola se i flussi in input sono stati terminati da reset o meno.\cr
#'Un flusso è terminato da reset se ha almeno un reset al suo interno e non ha due fin. \cr
#'Comando di wireshark di prova:\cr
#'tshark -r GazzettaDiModena_20161120.pcap  -z io,stat,0,'COUNT(tcp.flags.reset)tcp.flags.reset and tcp.flags.reset==1 and tcp.stream==11' -q \cr
#'@param filename nome del file pcap
#'@param streams_id id delle conversazioni del pcap
#'@param streams_id per ogni conversazione abbiamo TRUE se la conversazione \cr
#' ha due fin al suo interno o FALSE viceversa
#'@retun array che contiene per ogni stream tcp una variabile booleana: \cr
#'FALSE: flusso non terminato da reset
#'TRUE: flusso terminato da reset
#'@examples
#'library(TesinaTIR)
#'filename<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'#tempo massimo relativo del pcap
#'tmax<-tmaxPcap(filename)
#'#estrazione flussi sottoforma di stream id
#'#stream id va da 0 a 'MAX(tcp.stream)tcp.stream'
#'#stream id massimo
#'max_stream_id<-get_max_stream_id(filename,'tcp')
#'#genero tutti gli stream id
#'streams_id<-get_streams_id(max_stream_id)
#'#terminazione flusso con fin
#'fin<-get_TcpConv_correct_fwh(filename,streams_id)
#'#terminazione flusso con reset
#'l_reset<-get_TcpConv_reset(filename,streams_id,fin)



#'@export
get_TcpConv_reset<-function(filename,streams_id,fin){
  #posizione array reset
  pos<-1 
  #contiene i reset per flussi
  l_reset<-array(FALSE,dim=length(streams_id))
  #assegno valori a l_reset
  for(stream_id in streams_id){
    dim_reset<-system(paste("tshark -r" ,filename,  "-z io,stat,0,'COUNT(tcp.flags.reset)tcp.flags.reset and tcp.flags.reset==1 
                                                     and tcp.stream==",stream_id,"' -q |grep '<>' | sed 's/[|<>]//g'"),intern=TRUE)
    
    dim_reset<-stringr::str_trim(dim_reset)
    dim_reset <- strsplit(dim_reset, " ")[[1]]
    dim_reset<- as.numeric(dim_reset[[length(dim_reset)]])
    #se ho almeno un reset e non ho due fin il flusso è stato terminato dal reset
    if(dim_reset>0 && fin[pos]!=TRUE){
      l_reset[pos]<-TRUE
    }
    pos<-pos+1
  }
  return(l_reset)
}
