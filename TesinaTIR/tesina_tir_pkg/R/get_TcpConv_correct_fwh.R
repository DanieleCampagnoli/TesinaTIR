#'flussi terminati con fin \cr
#'
#'Questa funzione si occupa di calcolare se i flussi in input sono stati
#'terminati da due fin o meno.\cr
#'comando di prova \cr
#'tshark -r GazzettaDiModena_20161120.pcap  -z io,stat,0,'COUNT(tcp.flags.fin)tcp.flags.fin and tcp.flags.fin==1 and tcp.stream==11' -q \cr
#'@param filename nome del file pcap
#'@param streams_id vettore che continene gli id di tutti gli stream del pcap
#'@return array che contiene per ogni stream tcp una variabile booleana: \cr
#'FALSE: flusso non terminato da fin
#'TRUE: flusso terminato da fin
#'@export
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



get_TcpConv_correct_fwh<-function(filename,streams_id){
  pos<-1 
  #contiene fin per flussi
  fin<-array(FALSE,dim=length(streams_id))
  #assegno valori a fin
  for(stream_id in streams_id){
    dim_fin<-system(paste("tshark -r" ,filename,  "-z io,stat,0,'COUNT(tcp.flags.fin)tcp.flags.fin and tcp.flags.fin==1 
                                                     and tcp.stream==",stream_id,"' -q |grep '<>' | sed 's/[|<>]//g'"),intern=TRUE)
    #elimino spazi precedenti e successivi
    dim_fin<-stringr::str_trim(dim_fin)
    #split sugli spazi
    dim_fin <- strsplit(dim_fin, " ")[[1]]
    #converto in numerico
    dim_fin<- as.numeric(dim_fin[[length(dim_fin)]])
    #se ho almeno un fin
    if(dim_fin>=2){
      fin[pos]<-TRUE
    }
    pos<-pos+1
  }
  return(fin)
}
