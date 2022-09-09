library(stringi)

read_embrapa <- function(x) {
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
  
  return(data)
}

get_header_embrapa <- function(X) {
  df <- read_embrapa(X)
  return(colnames(df))
}


read_all_embrapa <- function(x) {
  files_embrapa = list.files(path=x, pattern=NULL, all.files=FALSE,
                             full.names=TRUE)
  df_appended = data.frame()
  
  for(file_name in files_embrapa) {
    print(paste("Reading file:",toString(file_name)))
    df <- read_embrapa(file_name)
    df_appended <- rbind(df_appended, df)
  }
  return(df_appended)

}

