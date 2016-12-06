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
  
  #indice di bins_count
  i<-1
  #per ogni bin in bins
  for(bin in bins){
    #per ogni valore in in_data
    for(val in in_data){
      #se il valore è contenuto nel range assegnato al bin aggiorna il conteggio del bin
      if(bin<=val && val<bin+step){
        bins_count[i]<-bins_count[i]+1
      }
    }
    i<-i+1
  }
  return (bins_count)
}
