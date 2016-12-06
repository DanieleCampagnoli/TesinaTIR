#require(stringr)

#'tempo massimo relativo del pcap
#params:
#'@param pcapName nome del file pcap
#'@retun tempo massimo relativo del pcap
#'@seealso \code{\link[stringr]{str_trim}} la funzione dipende da questa funzione
#'@export 
#'@examples
#'library(TesinaTIR)
#'pcapName<-system.file("extdata", "dump.pcap", package = "TesinaTIR")
#'tmax<-tmaxPcap(pcapName)
tmaxPcap <- function(pcapName){
  #preparazione comando per il recupero del tmax
  cmd_tshark<-"tshark -r "
  cmd_tshark<-paste(cmd_tshark, pcapName)
  cmd_tshark<-paste(cmd_tshark, " -z io,stat,0,'MAX(frame.time_relative)frame.time_relative' -q |grep '<>' | sed 's/[|<>]//g'")
  
  #lancio il comando 
  stmax<-system(cmd_tshark,intern=TRUE)
  #elimino gli spazi iniziali e finali
  stmax<-stringr::str_trim(stmax)
  #split sullo spazio
  dump_split <- strsplit(stmax, " ")[[1]]
  #recupero l'ultimo campo
  #stmax_val<-dump_split[[11]]
  stmax_val<-dump_split[[length(dump_split)]] 
  #conversione a numero
  tmax<-as.numeric(stmax_val)
  
  return(tmax)
}
