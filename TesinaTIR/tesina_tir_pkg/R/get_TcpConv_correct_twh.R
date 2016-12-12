#'presenza three way handshake nei flussi
#'
#'Questa funzione calcola la presenza o meno del twh nei flussi di input. \cr
#'il filtro di wireshark utilizzato è composto dalle seguenti parti: \cr
#' * tcp.flags.syn==1 cattura i primi due pacchetti \cr
#' * tcp.seq==1 and tcp.ack==1  and tcp.analysis.initial_rtt cattura il terzo pacchetto, \cr
#'   perchè wireshark calcola tcp.analysis.initial_rtt ogni volta che viene instaurata \cr
#'   una connessione tcp ed è applicato a tutte le tracce successive all'instaurazione \cr
#'   della connessione \cr
#'comando per testare questo filtro \cr
#'tshark -r  GazzettaDiModena_20161120.pcap -Y 'tcp.flags.syn==1 or (tcp.seq==1 and tcp.ack==1  and tcp.analysis.initial_rtt) and tcp.stream==11' \cr
#'prendo le due tracce iniziali: syn, syn-ack \cr
#'tshark -r  GazzettaDiModena_20161120.pcap -Y 'tcp.flags.syn==1 and tcp.seq==0 and tcp.stream==11' \cr
#'prendo la terza traccia: ack \cr
#'tshark -r  GazzettaDiModena_20161120.pcap -Y 'tcp.seq==1 and tcp.ack==1  and tcp.analysis.initial_rtt and tcp.stream==11' \cr
#'se ho almeno due tracce per le tracce iniziali e almento una traccia per qella finale allora il three way handshake è instaurato con successo \cr
#' @param filename nome del file pcap
#' @param streams_id id degli stream tcp del pcap
#' @return array che contiene per ogni stream tcp una variabile booleana: \cr
#' #' FALSE: twh non eseguito
#' #' TRUE: twh eseguito
#'@export
#'@examples
#'library(TesinaTIR)
#'filename<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'#tempo massimo relativo del pcap
#'tmax<-tmaxPcap(filename)
#'
#'#estrazione flussi sottoforma di stream id
#'#stream id va da 0 a 'MAX(tcp.stream)tcp.stream'
#'#stream id massimo
#'max_stream_id<-get_max_stream_id(filename,'tcp')
#'
#'#genero tutti gli stream id
#'streams_id<-get_streams_id(max_stream_id)
#'#calcolo i flussi che hanno eseguito il three way hanshake 
#'three_way_handshake<-get_TcpConv_correct_twh(filename,streams_id)


get_TcpConv_correct_twh<-function(filename,streams_id){
  #indice array three_way_handshake
  pos<-0 
  #array che indica se il flusso corrispondente ha eseguti o meno 
  #il three way handshake correttamente
  three_way_handshake<-array(FALSE,dim=length(streams_id))
  #assegno valori al vettore three_way_handshake 
  for(stream_id in streams_id){
    #conto tracce del three way handshake del flusso di syn e syn-ack 
    dim_tree_way_handshake_first<-system(paste("tshark -r " ,filename,  "-z io,stat,0,'COUNT(frame.number)frame.number 
                                               and tcp.flags.syn==1 
                                               and tcp.stream==",stream_id,"' -q |grep '<>' | sed 's/[|<>]//g'"),intern=TRUE)
    
    #elimino gli spazi precedenti e successivi
    dim_tree_way_handshake_first<-stringr::str_trim(dim_tree_way_handshake_first)
    #split sullo spazio
    dim_tree_way_handshake_first <- strsplit(dim_tree_way_handshake_first, " ")[[1]]
    #prendo utimo campo
    dim_tree_way_handshake_first<- as.numeric(dim_tree_way_handshake_first[[length(dim_tree_way_handshake_first)]])
    
    
    #conto tracce del three way handshake del flusso di ack, cioè l'ultimo pacchetto   
    dim_tree_way_handshake_second<-system(paste("tshark -r " ,filename,  "-z io,stat,0,'COUNT(frame.number)frame.number 
                                                     and tcp.seq==1 and tcp.ack==1 and tcp.analysis.initial_rtt 
                                                     and tcp.stream==",stream_id,"' -q |grep '<>' | sed 's/[|<>]//g'"),intern=TRUE)
    
    #elimino gli spazi precedenti e successivi
    dim_tree_way_handshake_second<-stringr::str_trim(dim_tree_way_handshake_second)
    #split sullo spazio
    dim_tree_way_handshake_second <- strsplit(dim_tree_way_handshake_second, " ")[[1]]
    #prendo utimo campo
    dim_tree_way_handshake_second<- as.numeric(dim_tree_way_handshake_second[[length(dim_tree_way_handshake_second)]])
    
    #controllo se il three way handshake è stato eseguito correttamente
    if(dim_tree_way_handshake_first>=2 && dim_tree_way_handshake_second>=1 ){
      three_way_handshake[pos]<-TRUE
    }
    pos<-pos+1
  }
  return(three_way_handshake)
}