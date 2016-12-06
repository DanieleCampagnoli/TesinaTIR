#---------------DESCRIZIONE-----------
#distibuzione durata flussi tcp e udp
#parametri dello script
#bins: bins della distribuzione
#filename: nome del file pcap
#tmax: tempo massimo del pcap
#step: dimensione dei bin
#-------VARIABILI DI OUTPUT----------
#sono presenti nello scope dopo l'esecuzione dello script:
#bins_count_durations: valore dei bin delle durate
#norm_bins_count: valore dei bin delle durate normalizzato

#funzioni esterne

#source('./functions/tmax_pcap.R')
#source('./functions/cmd_parser.R')
#source('./functions/get_conv_durations_TcpUdp.R')
#source('./functions/normalize_bin.R')
#source('./functions/compute_bins_count.R')

library(TesinaTIR)

#parsing dei parametri

description<-"

metriche calcolate:
   -distibuzione della durata dei flussi tcp e udp

VARIABILI DI OUTPUT -> workspace.RData
durations_tcp: durate dei flussi tcp
durations_udp:durate dei flussi udp
bins_count_durations_tcp: valore dei bin delle durate dei flussi tcp
bins_count_durations_dup: valore dei bin delle durate dei flussi udp
bins_norm_durations_tcp: valore dei bin delle durate dei flussi tcp normalizzato
bins_norm_durations_udp: valore dei bin delle durate dei flussi udp normalizzato

GRAFICI DI OUTPUT:
duration_tcp.pdf -> durata flussi tcp
duration_udp.pdf -> durata flussi udp
"

arguments<-matrix(c("--bin_size",
                    "dimensione del bin per il campionamento di campTcpUdp","1"), 
                  ncol=3,nrow=1)

argv<-get_argv(description,arguments)

#filename
filename<-argv$input_file

#ouput folder
output_folder<-argv$output_directory

#tempo massimo relativo del pcap
tmax<-tmaxPcap(filename)

#dimensione dei bins
step<-as.numeric(argv$bin_size)
#array che contiene tutti i bins, un bin inizia con il suo 
#valore e finisce al valore dell'elemento successivo dell'array
bins<-seq(0,tmax,step)

durations_tcp<-get_conv_durations_TcpUdp(filename,'tcp')
durations_udp<-get_conv_durations_TcpUdp(filename,'udp')

#eseguo il conteggio dei bin per le durate
bins_count_durations_tcp<-compute_bins_count(durations_tcp,bins,step)
bins_count_durations_udp<-compute_bins_count(durations_udp,bins,step)

#normalizzazione dei bin della durata del flusso
bins_norm_durations_tcp<-normalize_bin(bins_count_durations_tcp)
bins_norm_durations_udp<-normalize_bin(bins_count_durations_udp)

#grafico della distribuzione della durata dei flussi tcp
pdf(paste(output_folder,"/duration_tcp.pdf",sep=""))
plot(bins,bins_norm_durations_tcp,col="blue",type="o",ann=FALSE,xaxt="n")
axis(1, at =seq(0,tmax,5), las=2)
box()
title(main="distibuzione durata flussi tcp", col.main="red", font.main=4)
title(xlab="Time [s]", col.lab=rgb(0,0.5,0))
title(ylab="Probabilità", col.lab=rgb(0,0.5,0))
#salvataggio grafico duration_tcp
dev.off()

#grafico della distribuzione della durata dei flussi udp
pdf(paste(output_folder,"/duration_udp.pdf",sep=""))
plot(bins,bins_norm_durations_udp,col="blue",type="o",ann=FALSE,xaxt="n")
axis(1, at =seq(0,tmax,5), las=2)
box()
title(main="distibuzione durata flussi udp", col.main="red", font.main=4)
title(xlab="Time [s]", col.lab=rgb(0,0.5,0))
title(ylab="Probabilità", col.lab=rgb(0,0.5,0))
#salvataggio grafico duration_udp
dev.off()

#salvataggio workspace 
save.image(file=paste(output_folder,"/workspace.RData",sep=""))

#pulizia generale
rm(list=ls())


