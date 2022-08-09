library(stringi)

read_embrapa <- function(x) {
  data <- read.csv(x, sep=";", quote="\"", 
                   header = TRUE, skip = 2, fileEncoding="latin1")
  
  # Faz limpeza do cabecalho
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