
library(TesinaTIR)


#require(stringr)
#source('./functions/tmax_pcap.R')
#source('./functions/get_conv_durations_TcpUdp.R')
#source('./functions/compute_bins_count.R')
#source('./functions/normalize_bin.R')
#source('./functions/cmd_parser.R')
#source('./functions/bins_conv_transmission.R')
#source('./functions/get_max_stream_id.R')
#source('./functions/get_streams_id.R')

#parsing dei parametri

description<-"

metriche calcolate:
-distibuzione nel tempo delle trasmissioni dei flussi tcp e udp.
calcola la probabilità che un flusso trasmetta in un dato instante 
nell'intervallo temporale in cui è in vita.

VARIABILI DI OUTPUT-> workspace.RData
max_stream_id_tcp: stream id massimo tcp
max_stream_id_udp: stream id massimo udp
streams_id_tcp: array con tutti gli stream id tcp del pcap
streams_id_udp: array con tutti gli stream id udp del pcap
max_duration_tcp: durata massima flussi tcp
max_duration_udp: durata massima flussi udp
bins_tcp: bin temporali tcp
bins_udp: bin temporali udp
bins_conv_transmission_tcp: valori bin contenuti in bins_tcp
bins_conv_transmission_udp: valori bin contenuti in bins_udp

GRAFICI DI OUTPUT:
transmission_tcp.pdf -> distibuzione trasmissione flussi tcp
transmission_udp.pdf -> distibuzione trasmissione flussi udp
"

arguments<-matrix(c("--bin_size",
                    "dimensione del bin per il campionamento di campTcpUdp","1"), 
                  ncol=3,nrow=1)

argv<-get_argv(description,arguments)

#ouput folder
output_folder<-argv$output_directory
#nome del pcap di input
filename<-argv$input_file

#ampiezza dei bin sul tempo 
step_size<-as.numeric(argv$bin_size)



#stream id massimi di udp e tcp
max_stream_id_tcp<-get_max_stream_id(filename,'tcp')
max_stream_id_udp<-get_max_stream_id(filename,'udp')

#generazione di tutti gli stream id udp e tcp
streams_id_tcp<-get_streams_id(max_stream_id_tcp)
streams_id_udp<-get_streams_id(max_stream_id_udp)

#calcolo durata massima conversazioni udp e tcp
max_duration_tcp<-max(get_conv_durations_TcpUdp(filename,'tcp'))
max_duration_udp<-max(get_conv_durations_TcpUdp(filename,'udp'))

#calcolo bin temporali udp e tcp
bins_tcp=seq(0,max_duration_tcp,step_size)
bins_udp=seq(0,max_duration_udp,step_size)
#valori bin udp e tcp
bins_conv_transmission_tcp<-bins_conv_transmission(streams_id_tcp,'tcp',filename,bins_tcp,step_size)
bins_conv_transmission_udp<-bins_conv_transmission(streams_id_udp,'udp',filename,bins_udp,step_size)

#normalizzo i bins
bins_norm_conv_transmission_tcp<-normalize_bin(bins_conv_transmission_tcp)
bins_norm_conv_transmission_udp<-normalize_bin(bins_conv_transmission_udp)

#grafico della distribuzione della durata dei flussi tcp
pdf(paste(output_folder,"/transmission_tcp.pdf",sep=""))
plot(bins_tcp,bins_norm_conv_transmission_tcp,col="blue",type="l",ann=FALSE,xaxt="n")
axis(1, at =bins_tcp[seq(1,length(bins_tcp),20)], las=2)
box()
title(main="distibuzione trasmissione flussi tcp", col.main="red", font.main=4)
title(xlab="tempo relativo all'inizio del flusso [s]", col.lab=rgb(0,0.5,0))
title(ylab="Probabilità di trasmissione", col.lab=rgb(0,0.5,0))
#salvataggio grafico duration_tcp
dev.off()


#grafico della distribuzione della durata dei flussi tcp
pdf(paste(output_folder,"/transmission_udp.pdf",sep=""))
plot(bins_udp,bins_norm_conv_transmission_udp,col="blue",type="o",ann=FALSE,xaxt="n")
axis(1, at =bins_udp[seq(1,length(bins_udp),20)], las=2)
box()
title(main="distibuzione trasmissione flussi udp", col.main="red", font.main=4)
title(xlab="tempo relativo all'inizio del flusso [s]", col.lab=rgb(0,0.5,0))
title(ylab="Probabilità di trasmissione", col.lab=rgb(0,0.5,0))
#salvataggio grafico duration_tcp
dev.off()

#salvataggio workspace 
save.image(file=paste(output_folder,"/workspace.RData",sep=""))

#pulizia generale
rm(list=ls())

