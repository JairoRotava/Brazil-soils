import("utils")
import(stringi)
import(dplyr)
import(sp)
import(geobr)
import(ggplot2)
#soil <- modules::use("soil.R")

export("read_file")
read_file <- function(x) {
  data <- read.csv(x, sep=";", quote="\"", 
                   header = TRUE, skip = 2, fileEncoding="utf-8")
  
  #data <- read.csv(x, sep=";", quote="\"", 
  #                 header = TRUE, skip = 2, fileEncoding="latin1")
  
  # Faz limpeza do cabecalho
  # Cabecalho nao pode comecar com numero, por isso adiciona um x na frente.
  # Espacos e caracteres invalidos sao subsituidos por ponto (.).
  
  # Remove acentos
  colnames(data) <- stri_trans_general(str = colnames(data), id = "Latin-ASCII")
  # transforma para lowercase
  colnames(data) <- tolower(colnames(data))
  # Remove . repetidos
  colnames(data) <- gsub("([\\.])\\1+", "\\1",colnames(data))
  # Remove qualquer caracter que nao seja 0-9a-zA-Z e .
  colnames(data) <- gsub("[^a-zA-Z0-9\\.]", "", colnames(data))
  # Remove pontos no inicio ou no final do nome
  colnames(data) <- gsub("^\\.|\\.$", "", colnames(data))
  
  
  return(data)
}

export("get_header")
get_header <- function(X) {
  df <- read_file(X)
  return(colnames(df))
}

export("read_all_files")
read_all_files <- function(x) {
  files_embrapa = list.files(path=x, pattern=NULL, all.files=FALSE,
                             full.names=TRUE)
  df_appended = data.frame()
  
  for(file_name in files_embrapa) {
    print(paste("Reading file:",toString(file_name)))
    df <- read_file(file_name)
    df_appended <- rbind(df_appended, df)
  }
  return(df_appended)
  
}

export("convert_deg_to_decimal")
convert_deg_to_decimal <- function(d, m , s, h) {
  # TODO: converter para SUL o LEste de acorto com arquivo de entrar
  # TODO: consertar isso para conseguir lidar com casos onde falta 
  #     o hemisferio
  h <- tolower(h)
  #if (any(h != "sul" & h != "norte" & 
  #        h != "leste" & h != "oeste")) {
  if (any(!h %in% c("norte","sul","leste","oeste"))) {
    stop("Hemisferio deve ser norte, sul, leste ou oeste")
  }
  
  h[tolower(h)=="sul"] <- "S"
  h[tolower(h)=="norte"] <- "N"
  h[tolower(h)=="oeste"] <- "W"
  h[tolower(h)=="leste"] <- "E"
  h[tolower(h)==""] <- "S"
  
  coord <- as.numeric(char2dms(paste0(d,"d",m,"'",s,"\"",h)))
  return(coord)
}


export("plot_sites")
plot_sites <- function (lon, lat) {
  # carrega shape dos estados do brasil
  states <- read_state()
  
  
  # Plota pontos
  sites <- data.frame(longitude = lon[,1], latitude = lat[,1])
  
  
  
  ggplot() +
    geom_sf(data=states) +
    geom_point(data = sites, aes(x = longitude, y = latitude), size=1)
  
}


