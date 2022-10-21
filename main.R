library(dplyr)
library(futile.logger)
library(tryCatchLog)
library(dplyr)
lib <- modules::use("R")

PRE_PROCESSED_FILE = "./results/embrapa_soil_raw.Rda"

# Gera arquivo pre processado se ele não existe
if (!file.exists(PRE_PROCESSED_FILE)) {
  solos <- lib$embrapa$read_all_embrapa("./data/embrapa")
  saveRDS(solos, PRE_PROCESSED_FILE)
  
}

# Le dados pré processador
solos <- readRDS(PRE_PROCESSED_FILE)


# Processa dados e converte para SWAT
################################################

# Obtem ID unicos dos pontos da coluna "codigo.pa"
ids <- unique(solos$codigo.pa)

ids = c(7064, 7066)
swat_soil <- data.frame()


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
  
  #try({
  #  converted_soil <- lib$embrapa$convert_to_swat(df)
  #  swat_soil <- rbind(swat_soil, converted_soil)
  #})
  
  tryLog(
    {
      converted_soil <- lib$embrapa$convert_to_swat(df)
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
