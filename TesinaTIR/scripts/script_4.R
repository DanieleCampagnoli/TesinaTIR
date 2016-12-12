library(TesinaTIR)

description<-"

metriche calcolate:
-distibuzione nel tempo delle trasmissioni dei flussi tcp e udp.
calcola la probabilità che un flusso trasmetta in un dato instante 
nell'intervallo temporale in cui è in vita.

VARIABILI DI OUTPUT-> workspace.RData
max_stream_id_tcp: stream id massimo tcp
streams_id_tcp: array con tutti gli stream id tcp del pcap
max_duration_tcp: durata massima flussi tcp
count_bins: conteggio dei bin non normalizzati
env_stream_tmin: tmin per ogni flusso
env_stream_tmax: tmax per ogni flusso
count_duration_norm_bins: conteggio dei bin normalizzati per la durata

GRAFICI DI OUTPUT:
transmission_tcp.pdf -> distibuzione trasmissione flussi tcp
transmission_zoom_tcp.pdf -> zoom negli istanti iniziali
transmission_normDuration_tcp.pdf -> grafico della normalizzazione
"

arguments<-matrix(c("--bin_size","dimensione del bin per il campionamento di campTcpUdp","1"), 
                  ncol=3,nrow=1)
arguments<-rbind(arguments,c("--norm_bins","numero di bin per la normalizzazione della durata del flusso","100"))

argv<-get_argv(description,arguments)


filename<-argv$input_file
output_folder<-argv$output_directory

max_duration_tcp<-max(get_conv_durations_TcpUdp(filename,'tcp'))
max_stream_id_tcp<-get_max_stream_id(filename,'tcp')
step_size<-as.numeric(argv$bin_size)
#normalizzazione durata: divido la durata in 100 parti
num_duration_norm_bins<-as.numeric(argv$norm_bins)



f<-system(paste("tshark -r ",filename," -Y 'tcp' -E separator=, -Tfields -e frame.time_relative -e tcp.stream",sep=""),intern=TRUE)

time_rel<-array(dim=length(f))
stream_id<-array(dim=length(f))
i<-1
for(line in f){
  s_line<-strsplit(line, ",")[[1]]
  time_rel[i]<-as.numeric(s_line[1])
  stream_id[i]<-as.numeric(s_line[2])
  i<-i+1
}



#tmin per ogni flusso
env_stream_tmin<- new.env(hash = TRUE)
#tmax per il flusso
env_stream_tmax<-new.env(hash = TRUE)


count_tmin_tmax<-0
for(i in seq(1,length(time_rel),1)){
  stream<-stream_id[i]
  t<-time_rel[i]
  stream_tmin<-env_stream_tmin[[as.character(stream)]]
  if(is.null(stream_tmin)){
    env_stream_tmin[[as.character(stream)]]<-t
  }else{
    stream_tmax<-env_stream_tmax[[as.character(stream)]]
    if((is.null(stream_tmax)) || stream_tmax<t){
      env_stream_tmax[[as.character(stream)]]<-t
    }
  }
}

#conto le frequenze dei bins

#contiene i conteggi dei bin
bins<-seq(0,max_duration_tcp,step_size)
count_bins<-array(list(),dim=length(bins))

for(i in seq(1,length(time_rel),1)){
  stream<-stream_id[i]
  t<-time_rel[i]
  tmin<-env_stream_tmin[[as.character(stream)]]
  t_rel_conv<-t-tmin
  if(t_rel_conv!=0){
    pos_bin<-floor(t_rel_conv/step_size)+1
    count_bins[[pos_bin]]<-unique(c(count_bins[[pos_bin]],stream))
  }else{
    count_bins[[1]]<-unique(c(count_bins[[1]],stream))
  }
}

for(i in seq(1,length(count_bins),1)){
  count_bins[[i]]<-length(count_bins[[i]])/(max_stream_id_tcp+1)
}


#grafico
pdf(paste(output_folder,"/transmission_tcp.pdf",sep=""))

plot(bins,count_bins,col="blue",type="l",ann=FALSE,xaxt="n")
axis(1, at =seq(1,length(bins),3000), las=2)
box()
title(main="distibuzione trasmissione flussi tcp", col.main="red", font.main=4)
title(xlab="tempo relativo all'inizio del flusso [s]", col.lab=rgb(0,0.5,0))
title(ylab="Probabilità di trasmissione", col.lab=rgb(0,0.5,0))
#salvo grafico
dev.off()

#grafico
pdf(paste(output_folder,"/transmission_zoom_tcp.pdf",sep=""))

plot(bins[1:80],count_bins[1:80],col="blue",type="l",ann=FALSE,xaxt="n")
axis(1, at =seq(1,length(bins[1:80]),3), las=2)
box()
title(main="distibuzione trasmissione flussi tcp", col.main="red", font.main=4)
title(xlab="tempo relativo all'inizio del flusso [s]", col.lab=rgb(0,0.5,0))
title(ylab="Probabilità di trasmissione", col.lab=rgb(0,0.5,0))
#salvo grafico
dev.off()

#conteggi dei bins durata normalizzata

count_duration_norm_bins<-array(list(),dim=num_duration_norm_bins)

for(i in seq(1,length(time_rel),1)){
  stream<-stream_id[i]
  t<-time_rel[i]
  tmin<-env_stream_tmin[[as.character(stream)]]
  tmax<-env_stream_tmax[[as.character(stream)]]
  
  bin_size_stream_duration<-(tmax-tmin)/(num_duration_norm_bins-1)
  t_rel_conv<-t-tmin
  
  if(t_rel_conv!=0){
    pos_bin<-floor(t_rel_conv/bin_size_stream_duration)+1
    count_duration_norm_bins[[pos_bin]]<-
      unique(c(count_duration_norm_bins[[pos_bin]],stream))
  }else{
    count_duration_norm_bins[[1]]<-
      unique(c(count_duration_norm_bins[[1]],stream))
  }
}

for(i in seq(1,length(count_duration_norm_bins),1)){
  count_duration_norm_bins[[i]]<-
    length(count_duration_norm_bins[[i]])/(max_stream_id_tcp+1)
}


#grafico
pdf(paste(output_folder,"/transmission_normDuration_tcp.pdf",sep=""))

plot(seq(1,num_duration_norm_bins,1),
     count_duration_norm_bins,col="blue",type="l",ann=FALSE,xaxt="n")
axis(1, at =seq(1,num_duration_norm_bins,10), las=2)
box()
title(main="trasmissione tcp con durata normalizzata(100 parti)", col.main="red", font.main=4)
title(xlab="parti", col.lab=rgb(0,0.5,0))
title(ylab="Probabilità di trasmissione", col.lab=rgb(0,0.5,0))

#salvo grafico
dev.off()

#salvataggio workspace 
save.image(file=paste(output_folder,"/workspace.RData",sep=""))