library(DBI)
library(TesinaTIR)

description<-"

metriche calcolate:
-var,avg della percentuale di dati inviati in una parte.
 La durata di un flusso viene suddivisa in norm_bins di eguale dimensione.
 Dopo questa operazione si va a calcolare la percentuale di dati inviati in una parte per ogni flusso.
 Infine si calcola la media e la varianza delle percentuali di dati inviati in una parte.

VARIABILI DI OUTPUT-> workspace.RData
bins_metrics: var,avg della percentuale di dati inviati in una parte 

GRAFICI DI OUTPUT:
avg_transmission_normDuration_tcp.pdf -> varianza e media della percentuale di dati inviati

"

arguments<-matrix(c("--norm_bins","numero di bin per la normalizzazione della durata del flusso","10"), 
                  ncol=3,nrow=1)

argv<-get_argv(description,arguments)


filename<-argv$input_file
num_duration_norm_bins<-as.numeric(argv$norm_bins)
output_folder<-argv$output_directory

random_string<-paste(LETTERS[sample(1:26,20)],collapse="")
out_csv<-paste(random_string,".csv",sep="")
out_sqlite<-paste(random_string,".sqlite",sep="")

db <- dbConnect(RSQLite::SQLite(), out_sqlite)
dbSendQuery(conn = db,
            "CREATE TABLE pcap
            (stream INTEGER,
              frame_len INTEGER,
              time_relative FLOAT )")
dbSendQuery(conn = db,
            "CREATE TABLE bins
            (bin INTEGER)")


for(i in seq(0,num_duration_norm_bins-1,1)){
  dbSendQuery(conn=db,paste("insert into bins values(",i,")",sep=""))
} 



scmd<-paste("tshark -r ",filename," -Y 'tcp' -Tfields -e tcp.stream -e frame.len -e frame.time_relative -E separator=, > ",out_csv)

system(scmd,inter=TRUE)

dbWriteTable(conn = db, name = "pcap", value = out_csv,
             row.names = FALSE, header = FALSE,append=TRUE)



stream_durations<-dbGetQuery(conn = db,"select stream,max(time_relative),min(time_relative) from pcap group by stream;")

dbSendQuery(conn = db,
            "CREATE TABLE bins_value_sum
            (stream INTEGER,bin INTEGER, val INTEGER,percentage FLOAT)")


for(i in seq(1,nrow(stream_durations),1)){
  stream_id <-stream_durations[[i,1]]
  t_max <-stream_durations[[i,2]]
  t_min<-stream_durations[[i,3]]
  
  bin_size_stream_duration<-(t_max-t_min)/(num_duration_norm_bins-1)
  
  dbSendQuery(conn = db,paste(" insert into bins_value_sum (stream,bin,val)
                                select case when (t.stream is null) then",stream_id," else t.stream end as stream,
                                       bins.bin as bin,      
                                       case when (t.val is null) then 0 else t.val end as val                  
                                from
                                bins left join   
                                (select stream,cast(t.time_in_conv/",bin_size_stream_duration," as int)  bin,sum(t.frame_len)  val
                                from (select stream,frame_len,(time_relative-",t_min,") as time_in_conv from pcap where stream=",stream_id,") as t
                                group by stream,bin) as t on (bins.bin=t.bin) 
                                 "))
  
  

}


dbSendQuery(conn=db,";WITH table_percentage as( 
                     select t.stream,bv.bin,(cast (bv.val as float)/cast (t.tot_val as float)) as percentage
                     from (select bv.stream,sum(bv.val) as tot_val
                           from bins_value_sum bv
                           group by bv.stream
                          ) as t join bins_value_sum bv on (t.stream=bv.stream)
                     )
                     UPDATE bins_value_sum
                     SET 
                     percentage=(select tp.percentage from table_percentage as tp 
                                 where bins_value_sum.bin=tp.bin and bins_value_sum.stream=tp.stream);  
                ")


dbSendQuery(conn = db,
            "CREATE TABLE bins_metrics
            (bin INTEGER, data_avg FLOAT,data_var FLOAT)")


dbSendQuery(conn=db,paste("insert into bins_metrics (bin,data_avg,data_var)
                          select t.bin as bin,
                          t.data_avg as data_avg,
                          SUM((bv.percentage-t.data_avg)*(bv.percentage-t.data_avg))/",nrow(stream_durations)-1," as data_var   
                           
                          from (select bv.bin,AVG(bv.percentage) as data_avg   
                          from bins_value_sum bv
                          group by bv.bin) as t join bins_value_sum bv on (t.bin=bv.bin)
                          group by t.bin  ",sep=""))



bins_metrics<-dbGetQuery(conn=db,"select * from bins_metrics")

sum_avg<-sum(bins_metrics[['data_avg']])


#grafico
pdf(paste(output_folder,"/avg_transmission_normDuration_tcp.pdf",sep=""))

par(mfrow=c(2,1)) 

plot(seq(1,num_duration_norm_bins,1),
     bins_metrics[["data_avg"]],col="blue",type="l",ann=FALSE,xaxt="n")
axis(1, at =seq(1,num_duration_norm_bins,5), las=2)
box()
title(main="avg(percentuale dati) con durata normalizzata(100 parti)", col.main="red", font.main=4)
title(xlab="parti", col.lab=rgb(0,0.5,0))
title(ylab="avg(percentuale dati)", col.lab=rgb(0,0.5,0))

plot(seq(1,num_duration_norm_bins,1),
     bins_metrics[["data_var"]],col="blue",type="l",ann=FALSE,xaxt="n")
axis(1, at =seq(1,num_duration_norm_bins,5), las=2)
box()
title(main="var(percentuale dati) con durata normalizzata(100 parti)", col.main="red", font.main=4)
title(xlab="parti", col.lab=rgb(0,0.5,0))
title(ylab="var(percentuale dati)", col.lab=rgb(0,0.5,0))



#salvo grafico
dev.off()

save.image(file=paste(output_folder,"/workspace.RData",sep=""))

#pulizia file temporanei
system(paste("rm ",out_csv))
system(paste("rm ",out_sqlite))

