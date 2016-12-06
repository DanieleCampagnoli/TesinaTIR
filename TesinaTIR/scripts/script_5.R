#-------DESCRIZIONE----------------------------------------------
#questo script va a studiare l'instaurazione e la terminazione dei flussi tcp
#in particolare andiamo a calcolare tutti i flussi tcp sottoforma di tcp.stream che è un id associato al flusso da wireshark nel contesto di un pcap
#------VARIABILI DI OUTPUT-----:
#stmax: tempo massimo del pcap
#streams_id: stream id  di tutti i flussi del pcap
#max_stream_id: stream id massimo
#three_way_handshake: indica se il flusso ha eseguito correttamente il three way handshake valori:{TRUE | FALSE}
#fin: indica se il flusso è terminato con un fin valori:{TRUE | FALSE}
#l_reset: indica se il flusso è terminato con un reset valori:{TRUE | FALSE}
#counts_fin: numero di true e false in fin
#counts_twh: numero di true e false in three_way_handshake
#counts_reset: numero di true e false in l_reset
#bins_reset: conteggio del numero di reset nei bin di bins
#bins: intervalli temporali per l'andamento nel tempo dei reset
#step: dimensione dei bin di bins_reset

library(TesinaTIR)

#moduli esterni
require(stringr)
#source('./functions/get_max_stream_id.R')
#source('./functions/get_streams_id.R')
#source('./functions/tmax_pcap.R')
#source('./functions/cmd_parser.R')

#parsing dei parametri

description<-"

metriche calcolate:
   -numero di flussi tcp con three way handshake corretto
   -numero di flussi tcp terminati da fin
   -numero di flussi tcp terminati da reset
   -grafico andamento dei reset nel tempo

VARIABILI DI OUTPUT -> workspace.RData
stmax: tempo massimo del pcap
streams_id: stream id  di tutti i flussi del pcap
max_stream_id: stream id massimo
three_way_handshake: indica se il flusso ha eseguito correttamente il three way handshake valori:{TRUE | FALSE}
fin: indica se il flusso è terminato con un fin valori:{TRUE | FALSE}
l_reset: indica se il flusso è terminato con un reset valori:{TRUE | FALSE}
counts_fin: numero di true e false in fin
counts_twh: numero di true e false in three_way_handshake
counts_reset: numero di true e false in l_reset
bins_reset: conteggio del numero di reset nei bin di bins
bins: intervalli temporali per l'andamento nel tempo dei reset
step: dimensione dei bin di bins_reset

GRAFICI DI OUTPUT:
reset.pdf -> numero di flussi terminati da reset 
fin.pdf -> numero di flussi terminati da fin
twh.pdf -> numero di flussi con three way handshake corretto
"

arguments<-matrix(c("--bin_size",
                    "dimensione del bin per il campionamento di campTcpUdp","1"), 
                  ncol=3,nrow=1)

argv<-get_argv(description,arguments) 
#ouput folder
output_folder<-argv$output_directory

#nome del pcap di input
filename<-argv$input_file


#tempo massimo relativo del pcap
tmax<-tmaxPcap(filename)

#estrazione flussi sottoforma di stream id
#stream id va da 0 a 'MAX(tcp.stream)tcp.stream'
#stream id massimo
max_stream_id<-get_max_stream_id(filename,'tcp')


#genero tutti gli stream id
streams_id<-get_streams_id(max_stream_id)
#calcolo i flussi che hanno eseguito il three way hanshake 
three_way_handshake<-get_TcpConv_correct_twh(filename,streams_id)

#terminazione flusso con fin
fin<-get_TcpConv_correct_fwh(filename,streams_id)
#terminazione flusso con reset

l_reset<-get_TcpConv_reset(filename,streams_id,fin)

#creazione barpolt reset
pdf(paste(output_folder,"/reset.pdf",sep=""))
counts_reset<-table(l_reset)
barplot(counts_reset)
box()
title(main="numero flussi terminati da reset", col.main="red", font.main=4)
title(xlab="true:flusso terminato da reset", col.lab=rgb(0,0.5,0))
#salvataggio grafico reset
dev.off()


#creazione barpolt three_way_handshake
pdf(paste(output_folder,"/twh.pdf",sep=""))
counts_twh<-table(three_way_handshake)

barplot(counts_twh)
box()
title(main="numero flussi con three way handshake", col.main="red", font.main=4)
title(xlab="true:three way handshake eseguito correttamente", col.lab=rgb(0,0.5,0))
#salvataggio grafico twh
dev.off()

#creazione barpolt fin
pdf(paste(output_folder,"/fin.pdf",sep=""))
counts_fin<-table(fin)
barplot(counts_fin)
box()
title(main="numero flussi terminati da fin", col.main="red", font.main=4)
title(xlab="true:flusso terminato da fin", col.lab=rgb(0,0.5,0))
#salvataggio grafico fin
dev.off()

#distibuzione del numero di reset nel tempo

step<-as.numeric(argv$bin_size)
#array che contiene tutti i bins, un bin inizia con il suo 
#valore e finisce al valore dell'elemento successivo dell'array
bins<-seq(0,tmax,step)
#contiene il numero di reset avvenuti nel bin
bins_reset=array(0,dim=length(bins))


for (pos in seq(1,length(l_reset))){
  if(l_reset[[pos]]==TRUE){
    s<-paste("tshark -r ",filename," -Y 'tcp and tcp.flags.reset==1 and tcp.stream==",streams_id[[pos]],"' -T fields -e frame.time_relative")
    time_resets<-system(s,intern=TRUE)
    time_resets<-gsub(",",".",time_resets)
    time_resets<-as.numeric(time_resets)
    for(time_reset in time_resets){
      pos_bin<-1
      for(bin in bins){
        if(bin<=time_reset && time_reset<bin+step){
          bins_reset[[pos_bin]]<-bins_reset[[pos_bin]]+1
          break
        }
        pos_bin<-pos_bin+1
      }
    }
  }
}
pdf(paste(output_folder,"/reset_tcp.pdf",sep="")) 
plot(bins,bins_reset,col="blue",type="o",ann=FALSE,xaxt="n")
axis(1, at =seq(0,tmax,5), las=2)
box()
title(main="numbero di reset tcp nel tempo", col.main="red", font.main=4)
title(xlab="Time [s]", col.lab=rgb(0,0.5,0))
title(ylab="numero reset", col.lab=rgb(0,0.5,0))
#salvataggio grafico rtt tcp
dev.off()

#salvataggio workspace 
save.image(file=paste(output_folder,"/workspace.RData",sep=""))
#pulizia generale
rm(list=ls())


