import("utils")
import(stringi)
import(dplyr)

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


SWAT_HEADER = c("MUID", "SEQN", "SNAM", "S5ID", "CMPPCT", "NLAYERS", "HYDGRP", "SOL_ZMX", "ANION_EXCL", "SOL_CRK", "TEXTURE", 
                "SOL_Z1", "SOL_BD1", "SOL_AWC1", "SOL_K1", "SOL_CBN1", "CLAY1", "SILT1", "SAND1", "ROCK1", "SOL_ALB1", "USLE_K1", "SOL_EC1", 
                "SOL_Z2", "SOL_BD2", "SOL_AWC2", "SOL_K2", "SOL_CBN2", "CLAY2", "SILT2", "SAND2", "ROCK2", "SOL_ALB2", "USLE_K2", "SOL_EC2", 
                "SOL_Z3", "SOL_BD3", "SOL_AWC3", "SOL_K3", "SOL_CBN3", "CLAY3", "SILT3", "SAND3", "ROCK3", "SOL_ALB3", "USLE_K3", "SOL_EC3", 
                "SOL_Z4", "SOL_BD4", "SOL_AWC4", "SOL_K4", "SOL_CBN4", "CLAY4", "SILT4", "SAND4", "ROCK4", "SOL_ALB4", "USLE_K4", "SOL_EC4", 
                "SOL_Z5", "SOL_BD5", "SOL_AWC5", "SOL_K5", "SOL_CBN5", "CLAY5", "SILT5", "SAND5", "ROCK5", "SOL_ALB5", "USLE_K5", "SOL_EC5", 
                "SOL_Z6", "SOL_BD6", "SOL_AWC6", "SOL_K6", "SOL_CBN6", "CLAY6", "SILT6", "SAND6", "ROCK6", "SOL_ALB6", "USLE_K6", "SOL_EC6", 
                "SOL_Z7", "SOL_BD7", "SOL_AWC7", "SOL_K7", "SOL_CBN7", "CLAY7", "SILT7", "SAND7", "ROCK7", "SOL_ALB7", "USLE_K7", "SOL_EC7", 
                "SOL_Z8", "SOL_BD8", "SOL_AWC8", "SOL_K8", "SOL_CBN8", "CLAY8", "SILT8", "SAND8", "ROCK8", "SOL_ALB8", "USLE_K8", "SOL_EC8", 
                "SOL_Z9", "SOL_BD9", "SOL_AWC9", "SOL_K9", "SOL_CBN9", "CLAY9", "SILT9", "SAND9", "ROCK9", "SOL_ALB9", "USLE_K9", "SOL_EC9", 
                "SOL_Z10", "SOL_BD10", "SOL_AWC10", "SOL_K10", "SOL_CBN10", "CLAY10", "SILT10", "SAND10", "ROCK10", "SOL_ALB10", "USLE_K10", "SOL_EC10")

export("convert_to_swat")
convert_to_swat <- function(x) {
  soil_df <- data.frame(matrix(ncol = length(SWAT_HEADER), nrow = 1))
  colnames(soil_df) <- SWAT_HEADER
  # Obtem ID no ponto
  soil_df$MUID <- x[1,"codigo.pa"]
  
  # Checa se não não existe valor NA como profundidade
  if (anyNA(x[c("profundidade.inferior","profundidade.superior")]))
  {
    stop(paste("Valores NA para profundida para ID", soil_df$MUID))
    
  }
  
  # Coloca em ordem crescente com profundidade
  x <- arrange(x, profundidade.superior)
  # numero de camadas
  n_layers <- nrow(x)
  if (n_layers > 10) {
    warning(paste("Truncando para 10 camadas de solo para ID", soil_df$MUID))
    n_layers <- 10
  }
  # Verifica se espessuras de solo estao de acordo e atualiza
  z = 0
  # Trunca numero de camadas para 10 - limite SWAT
  for (i in seq_len(n_layers)) {
    if (x[i,"profundidade.superior"] != z) {
      stop(paste("Erro de continuidade de camadas para ID",soil_df$MUID))
    }
    e <- x[i,"profundidade.inferior"] - x[i,"profundidade.superior"]
    z <- z + e
    soil_df[paste0("SOL_Z",i)] <- z
    
  }
  soil_df$SOL_ZMX <- z
  
  return(soil_df)
}
