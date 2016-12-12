#!/usr/bin/env Rscript

library(TesinaTIR)
#parsing dei parametri
description<-"
 metriche calcolate:
  -numero di flussi tcp e udp
  -dati inviati/ricevuti tcp e udp
  -frame inviati/ricevuti tcp e udp
  -andamento duplicate ack nel tempo
  -andamento numero flussi nel tempo 

VARIABILI DI OUTPUT -> workspace.RData  
n_tcp_flux: numero flussi tcp
n_udp_flux: numero flussi udp
tcp_frame: frame inviati tcp
udp_frame: frame inviati udp
tcp_data: byte inviati tcp
udp_data: byte inviati udp
campTcpUdp: dataframe che contiene i seguenti campi:
            l_time: valori temporali dei bins
            l_tcp_frame: numero di frame tcp
            l_udp_frame: numero di frame udp
            l_udp_data: byte inviati da flussi udp
            l_tcp_data: byte inviati da flussi tcp
            l_tcp_duplicate_ack: numero di duplicate ack del tcp

GRAFICI DI OUTPUT:
frame_time.pdf -> numero di frame inviato nel tempo
tcp_duplicateAck.pdf -> andamento duplicate ack nel tempo
tcp_udp_data.pdf -> dati inviati nel tempo
trend_conv_tcp_udp -> flussi udp e tcp nel tempo 
"
arguments<-matrix(c("--bin_size",
                   "dimensione del bin per il campionamento di campTcpUdp","1"), 
                  ncol=3,nrow=1)

argv<-get_argv(description,arguments)

#parametri
pcapName <-argv$input_file
output_folder<-argv$output_directory

#calcolo tempo massimo del pcap
tmax<-tmaxPcap(pcapName)

#dimensione bin delle analisi
step_size<-as.numeric(argv$bin_size)

#conteggio flussi tcp da 0s a tmax s
n_tcp_flux<-fluxCount(pcapName, 'tcp', 0, tmax)
n_tcp_flux<-fluxCount(pcapName, 'tcp')
print('totale flussi tcp:')
print(n_tcp_flux)

#totale flussi udp
n_udp_flux<-fluxCount(pcapName, 'udp', 0, tmax)
n_udp_flux<-fluxCount(pcapName, 'udp')
print('totale flussi udp:')
print(n_udp_flux)

#dati e frame inviati tcp
res<-dataFrame('tcp',0,tmax)
res<-dataFrame(pcapName,'tcp')
tcp_frame<-res[[1]]
tcp_data<-res[[2]]
print('totale frame tcp:')
print(tcp_frame)
print('totale dati tcp:')
print(tcp_data)

#dati e frame inviati udp
res<-dataFrame('udp',0,tmax)
res<-dataFrame(pcapName,'udp')
udp_frame<-res[[1]]
udp_data<-res[[2]]
print('totale frame udp:')
print(udp_frame)
print('totale dati udp:')
print(udp_data)


#campionamento numero flussi tcp e udp (ogni secondo)
bins=seq(0,tmax,1)
print('conteggio udp')
conv_tot_udp<-count_conv(pcapName,"udp",step_size)
print('conteggio tcp')
conv_tot_tcp<-count_conv(pcapName,"tcp",step_size)
#grafici campionamento numero flussi tcp e udp 
pdf(paste(output_folder,"/trend_conv_tcp_udp.pdf",sep=""))


plot(bins,conv_tot_tcp, col="black",type="l",ann=FALSE)
lines(bins,conv_tot_udp, type="l", pch=22, lty=2, col="green")
legend("top","(x,y)", c("tcp","udp"), cex=0.8,col=c("black","green"), pch=21:23, lty=1:2)
box()
title(main="flussi udp e tcp nel tempo", col.main="red", font.main=4)
title(xlab="Time [s]", col.lab=rgb(0,0.5,0))
title(ylab="Flussi totali", col.lab=rgb(0,0.5,0))

#salvataggio grafico trend_conv_tcp_udp
dev.off()



#campionamento flussi tcp e udp 
campTcpUdp<-time_values_TcpUdp(pcapName,0,tmax,step_size)

#grafico tcp/udp frame


pdf(paste(output_folder,"/frame_time.pdf",sep=""))


plot(campTcpUdp[['l_time']],campTcpUdp[['l_tcp_frame']], col="blue",type="l",ann=FALSE)
lines(campTcpUdp[['l_udp_frame']], type="l", pch=22, lty=2, col="red")
legend("top","(x,y)", c("tcp_frame","udp_frame"), cex=0.8,col=c("blue","red"), pch=21:23, lty=1:2)
box()
title(main="numero di segmenti inviati nel tempo", col.main="red", font.main=4)
title(xlab="Time [s]", col.lab=rgb(0,0.5,0))
title(ylab="numero di segmenti", col.lab=rgb(0,0.5,0))

#salvataggio grafico tcp/udp frame
dev.off()

#grafico tcp/udp data
pdf(paste(output_folder,"/tcp_udp_data.pdf",sep=""))

plot(campTcpUdp[['l_time']],campTcpUdp[['l_tcp_data']], col="blue",type="l",ann=FALSE)
lines(campTcpUdp[['l_udp_data']], type="l", pch=22, lty=2, col="red")
legend("top","(x,y)", c("tcp_data","udp_data"), cex=0.8,col=c("blue","red"), pch=21:23, lty=1:2)
box()
title(main="dati inviati nel tempo", col.main="red", font.main=4)
title(xlab="Time [s]", col.lab=rgb(0,0.5,0))
title(ylab="byte", col.lab=rgb(0,0.5,0))

#salvataggio grafico tcp/udp data
dev.off()

#grafico tcp_duplicateAck
pdf(paste(output_folder,"/tcp_duplicateAck.pdf",sep=""))

plot(campTcpUdp[['l_time']],campTcpUdp[['l_tcp_duplicateAck']], col="blue",type="l",ann=FALSE)
legend("top","(x,y)", c("l_tcp_duplicateAck"), cex=0.8,col=c("blue"), pch=21:23, lty=1:2)
box()
title(main="andamento duplicate ack nel tempo", col.main="red", font.main=4)
title(xlab="Time [s]", col.lab=rgb(0,0.5,0))
title(ylab="numero duplicate ack", col.lab=rgb(0,0.5,0))

#salvataggio grafico tcp_duplicateAck
dev.off()

#salvataggio workspace
save.image(file=paste(output_folder,"/workspace.RData",sep=""))


