# Pre processamento dos dados
source("embrapa_util.R")

# Salva dados para uso posterior
solos <- readRDS("./data/embrapa_soil_raw.Rda")


# Processa dados e converte para SWAT
################################################
# TODO: Verificar problema de compatiilidade com dplyr
#library(tidyverse)

# Cria dataframe com colunas utilizadas pelo SWAT
COL_NAMES_FILE <- "swat_soil_colnames.csv"

convert_soil <- function(x, df_header) {
  soil_df <- data.frame(matrix(ncol = length(df_header), nrow = 1))
  colnames(soil_df) <- c(df_header)
  # Obtem ID no ponto
  soil_df$MUID <- x[1,"codigo.pa"]
  
  # Checa se não não existe valor NA como profundidade
  if (anyNA(df[c("profundidade.inferior","profundidade.superior")]))
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

library(futile.logger)
library(tryCatchLog)


# Obtem ID unicos dos pontos da coluna "codigo.pa"
ids <- unique(solos$codigo.pa)

#ids = c(7204, 72222, 7204, 72222)
swat_soil <- read.csv(COL_NAMES_FILE)
header <- colnames(swat_soil)

# Arquivo para log dos erros de conversao
LOG_FILE_NAME <- paste0("./logs/",
                        format(Sys.time(), "%Y%m%dT%H%M%S"),
                        "_swat_convertion.log")


# tryLog insere muitas linhas no fim da mensagem
# foi criado um novo appender no futile.logger para remover as linhas extras
# do final. Se tiver mais de 3 fim de linha, troca por 1
my_appender <- function(filename)
{
  function(line) {
    appender.tee(filename)(gsub("\n\n\n+$", "\n", line) )
  } 
}

flog.appender(my_appender(LOG_FILE_NAME))

for (id in ids) {
  df = solos %>% filter(codigo.pa == id)

  #TODO: tryLog adiciona \n\n no final de cada erro
  # deixando o arquivo cheio de espaco vazio. Ver como corrigir isso.
  tryLog(
    {
      converted_soil <- convert_soil(df, header)
      swat_soil <- rbind(swat_soil, converted_soil)
    },
    include.compact.call.stack = FALSE,
    include.full.call.stack = FALSE,
    
  )
}

# Salva arquivo de solos para SWAT
FILE_OUT = "soil_brazil.csv"
write.csv(swat_soil,FILE_OUT, quote = FALSE, row.names = FALSE)


################################################
# Mostra pontos no mapa

# Remove pontos sem coordenada GPS
solos = solos[complete.cases(solos[,c("latitude.graus", 
                                      "longitude.graus")]),]
# Altera NA em coordenads GPS para 0, pois alguns pontos não tem
# segundos definidos
solos$latitude.graus[is.na(solos$latitude.graus)] <-0
solos$latitude.minutos[is.na(solos$latitude.minutos)] <-0
solos$latitude.segundos[is.na(solos$latitude.segundos)] <-0
solos$longitude.graus[is.na(solos$longitude.graus)] <-0
solos$longitude.minutos[is.na(solos$longitude.minutos)] <-0
solos$longitude.segundos[is.na(solos$longitude.segundos)] <-0

# Seta hemisferio para sul ou oeste sempre que tiver indefinido
solos$longitude.hemisferio <- tolower(solos$longitude.hemisferio)
solos$latitude.hemisferio <- tolower(solos$latitude.hemisferio)

solos$longitude.hemisferio[!(solos$longitude.hemisferio %in% 
                               c("leste", "oeste"))] <- "oeste" 
solos$latitude.hemisferio[!(solos$latitude.hemisferio %in% 
                              c("norte", "sul"))] <- "sul" 
# Como o brasil fica sempre a oeste, seta hemisferio
solos$longitude.hemisferio <- "oeste"


# Remove pontos com coordenadas GPS fora do range
solos = subset(solos, solos$longitude.graus <= 180 
               & solos$longitude.graus >= -180)
solos = subset(solos, solos$latitude.graus <= 90 
               & solos$latitude.graus >= -90)


solos$lat_dec <- mapply(convert_deg_to_decimal,
                        solos["latitude.graus"],
                        solos["latitude.minutos"],
                        solos["latitude.segundos"],
                        solos["latitude.hemisferio"])


solos$lon_dec <- mapply(convert_deg_to_decimal,
                        solos["longitude.graus"],
                        solos["longitude.minutos"],
                        solos["longitude.segundos"],
                        solos["longitude.hemisferio"])

# Converte latitudes positivas em negativas 
#(erro no arquivo da embrapa), 
#solos$lon_dec <- - abs(solos$lon_dec)

# Carrega mapa do Brasil para plotar os pontos de dados
plot_sites(solos$lon_dec, solos$lat_dec)

# Total de pontos - dados com mesmo numero de identificação - mesmo buraco
sites <- unique(solos$codigo.pa)
