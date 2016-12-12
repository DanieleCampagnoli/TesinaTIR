#'@export
count_conv<-function (filename,protocol,step_size){
  
  f<-system(paste("tshark -r ",filename," -Y '",protocol,"' -E separator=, -Tfields -e frame.time_relative -e ",protocol,".stream",sep=""),intern=TRUE)
  tmax<-TesinaTIR::tmaxPcap(filename)
  step_size<-1
  bins<-seq(0,tmax,step_size)
  time_rel<-array(dim=length(f))
  stream_id<-array(dim=length(f))
  i<-1
  for(line in f){
    s_line<-strsplit(line, ",")[[1]]
    time_rel[i]<-as.numeric(s_line[1])
    stream_id[i]<-as.numeric(s_line[2])
    i<-i+1
  }
  
  compute_bins_count_conv<-function(in_data_time,in_data_values,bins,step){
    #conteggio dei bin, ogni elemento inizializzato a zero
    bins_count=array(list(),dim=(length(bins)))
    
    #per ogni bin in bins
    for(i in seq(1,length(in_data_time),1)){
      val_time<-in_data_time[[i]]
      val<-in_data_values[[i]]
      pos_bin<-floor(val_time/step)+1
      bins_count[[pos_bin]]<- unique(c(bins_count[[pos_bin]],val))
    }
    for(i in seq(1,length(bins_count),1)){
      bins_count[[i]]<-length(bins_count[[i]])
    }
    
    return (bins_count)
  }
  
  
  
  #numero di flussi attivi in un certo istante
  active_conv<-compute_bins_count_conv(time_rel,stream_id,bins,step_size)
  return(active_conv)
}