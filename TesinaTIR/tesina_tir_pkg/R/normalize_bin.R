#' funzione che normalizza il valore dei bins.
#parameters:
#'@param bins_count vettore che contiene i conteggio dei bins
#'@return array con i valori dei bin normalizzati
#'@export
#'@examples
#'library(TesinaTIR)
#'bins_count<-c(8,1,1)
#'normalize_bin(bins_count) 
normalize_bin<-function(bins_count){
  #calcolo il totale dei bins
  tot_bins_count<-0
  for(bin in bins_count){
    tot_bins_count<-tot_bins_count+bin
  }
  #creo un nuovo array con i conteggi normalizzati
  bins_norm=array(0,dim=length(bins_count))
  i<-1
  for(bin in bins_count){
    bins_norm[i]<-(bin/tot_bins_count)
    i<-i+1
  } 
  return (bins_norm)
}
