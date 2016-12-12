#library(argparser, quietly=TRUE)
#require(stringr)

#'parser dei parametri passati dalla command line
#'
#'parametri di input dello script di default: \cr
#' -i: input file, default: dump.pcap \cr
#' -o: output directory, default: ./output \cr
#' -h: help\cr
#' Tutti gli altri parametri devono essere specificati con il parametro arguments.
#'@seealso \code{\link{argparser}} la funzione utilizza questa libreria
#'@param description descrizione dello script
#'@param arguments argomenti accettati dallo script in un array con i seguenti elementi:\cr 
#'arguments[i]=c(argument,description,default_value) \cr
#'dove c() è il comando di r per creare un array
#'@return argv validato
#'@export
#'@examples
#'library(TesinaTIR)
#' description<-"
#' descrizione dello script
#'"
#' #argomenti aggiuntivi
#' arguments<-matrix(c("--bin_size",
#'                    "dimensione del bin per il campionamento di campTcpUdp","1"), 
#'                  ncol=3,nrow=1)
#' #argv contiene i parametri passati da command line
#'argv<-get_argv(description,arguments)

#crea il parser e valida i parametri
#pamameters:
#description: descrizione dello script
get_argv<-function(description,arguments){
  #creazione parser
  p <- argparser::arg_parser(description)
  p <- argparser::add_argument(p, "--input_file", help="input file",
                               default=system.file("extdata", "dump.pcap", package = "TesinaTIR"))
  p <- argparser::add_argument(p, "--output_directory", help="output directory",
                               default="./output")
  
  if(length(arguments)!=0){
    for(r in seq(1,nrow(arguments),1) ){
      p <- argparser::add_argument(p, arguments[[r,1]], help=arguments[[r,2]],default=arguments[[r,3]])
    }
  }
  
  #lettura parametri di input
  argv <- argparser::parse_args(p)

  #validazione parametri
  if(!file.exists(argv$input_file)){
    print(paste("file di input inesistente: ",argv$input_file,"\n"))
    stop()
  }
  if(!file.exists(argv$output_directory)){
    dir.create(file.path(argv$output_directory))
    print(paste("file salvati nella cartella: ",argv$output_directory,"\n"))
  }
  return(argv)
}


