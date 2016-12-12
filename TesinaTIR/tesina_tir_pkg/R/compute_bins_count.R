#'funzione che calcola le frequenze contenute nei bins
#parameters
#'@param in_data vettore che contiene la durata di ogni flusso
#'@param bins bins della distribuzione
#'@param step dimensione dei bins
#'@return array con frequenze dei bins
#'@export
#'@examples
#'library(TesinaTIR)
#'step<-1
#'durations<-c(1,2,2,3,3,4)
#'bins<-seq(1,4,step)
#'compute_bins_count(durations,bins,step)
compute_bins_count<-function(in_data,bins,step){
  #conteggio dei bin, ogni elemento inizializzato a zero
  bins_count=array(0,dim=length(bins))
  
  #per ogni bin in bins
  for(val in in_data){
    pos_bin<-floor(val/step)+1
    bins_count[pos_bin]<-bins_count[pos_bin]+1   
  }
  return (bins_count)
}
