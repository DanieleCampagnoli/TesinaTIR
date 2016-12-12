#!/usr/bin/env Rscript

#source('./functions/tmax_pcap.R')
#source('./functions/cmd_parser.R')
#source('./functions/rtt_time.R')

library(TesinaTIR)

#parsing dei parametri
description<-"

metriche calcolate:
-andamento del round trip time medio su tutti i flussi tcp
 round trip time= tempo trascorso tra l'invio di un sequence number e la sua conferma tramite ack

VARIABILI DI OUTPUT -> workspace.RData
rtt_tcp_intervals: contiene: contiene rtt medio, rtt max e rtt min di ogni bin

GRAFICI DI OUTPUT:
rtt_tcp.pdf -> andamento rtt nel tempo
"
arguments<-matrix(c("--bin_size",
                    "dimensione del bin per il campionamento di campTcpUdp","1"), 
                  ncol=3,nrow=1)

argv<-get_argv(description,arguments)
#filename
filename<-argv$input_file
#cartella di output
output_folder<-argv$output_directory
#ampiezze dei bin
bin_size<-as.numeric(argv$bin_size)
#tmax pcap
tmax<-tmaxPcap(filename)

#valori rtt in intervallo temporale
rtt_tcp_intervals<-rtt_time(0,tmax,bin_size,filename)

rtt_tcp_intervals[is.na(rtt_tcp_intervals)]<-0

#grafico andamento rtt tcp
pdf(paste(output_folder,"/rtt_tcp.pdf",sep="")) 

plot(rtt_tcp_intervals[['l_time']],rtt_tcp_intervals[['l_avg']], 
     col="blue",type="o",ann=FALSE)

lines(rtt_tcp_intervals[['l_time']],rtt_tcp_intervals[['l_min']], type="o", pch=22, lty=2, col="red")
lines(rtt_tcp_intervals[['l_time']],rtt_tcp_intervals[['l_max']], type="o", pch=22, lty=2, col="green")

legend("topright","(x,y)", c("avg","max","min"), cex=0.8, 
       col=c("blue","green","red"), pch=21:23, lty=1:2)
box()
title(main="rtt tcp", col.main="red", font.main=4)
title(xlab="Time [s]", col.lab=rgb(0,0.1,0))
title(ylab="Rtt[s]", col.lab=rgb(0,0.5,0))

#salvataggio grafico rtt tcp
dev.off()

#grafico andamento varianza nel tempo
pdf(paste(output_folder,"/rtt_var_tcp.pdf",sep="")) 

plot(rtt_tcp_intervals[['l_time']],rtt_tcp_intervals[['l_var']], 
     col="blue",type="o",ann=FALSE)

legend("topright","(x,y)", c("avg","max","min"), cex=0.8, 
       col=c("blue","green","red"), pch=21:23, lty=1:2)
box()
title(main=" var rtt tcp", col.main="red", font.main=4)
title(xlab="Time [s]", col.lab=rgb(0,0.1,0))
title(ylab="var Rtt[s]", col.lab=rgb(0,0.5,0))

#salvataggio grafico var rtt tcp
dev.off()





#salvataggio workspace 
save.image(file=paste(output_folder,"/workspace.RData",sep=""))

#pulizia generale
rm(list=ls())

