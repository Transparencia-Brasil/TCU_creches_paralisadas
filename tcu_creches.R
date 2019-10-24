#Analise de obras do TCU V2
# Tutorial do deflateBR https://fmeireles.com/blog/rstats/deflacionar-series-no-r-deflatebr/

library(dplyr)
library(data.table)
library(janitor)
library(httr)
library(jsonlite)
library(xlsx)
library(stringr)
library(deflateBR)
library(readxl)
library(ggmap)

key <- "AIzaSyBqpO3elcy7_DbZuw6y5gjYnFo-AR4iPjc"

register_google(key)

como_data <- function(x) {
  
  stopifnot(require(dplyr))
  x <- gsub(" .*", "", x)
  y <- gsub(".*/", "", x)
  x <- if_else((nchar(y)==4), as.Date(x, format="%d/%m/%Y"),
               as.Date(x, format="%d/%m/%y"))
  
}


# Importando as obrs de creches do TCU

tipo_projetos <- c("Escola com projeto elaborado pelo concedente",
                   "Escola com Projeto elaborado pelo proponente",
                   "Escola de Educação Infantil Tipo A",
                   "Escola de Educação Infantil Tipo B",
                   "Escola de Educação Infantil Tipo C",
                   "Espaço Educativo - 01 Sala", 
                   "Espaço Educativo - 02 Salas",
                   "Espaço Educativo - 04 Salas",
                   "Espaço Educativo - 06 Salas",
                   "Espaço Educativo - 08 Salas",
                   "Espaço Educativo - 12 Salas",
                   "MI - Escola de Educação Infantil Tipo B",
                   "MI - Escola de Educação Infantil Tipo C",
                   "Projeto 1 Convencional",
                   "Projeto 2 Convencional",
                   "Projeto Tipo B - Bloco Estrutural",  
                   "Projeto Tipo C - Bloco Estrutural")

tcu_creches_original <- read_excel("C:/Users/coliv/Documents/R-Projects/FMCSV/bancos_originais/TCU - obras paralisadas.xlsx", sheet = "MEC - creches etc")

tcu_creches <- tcu_creches_original %>%
  clean_names() %>%
  filter(tipologia %in% tipo_projetos) %>%
  mutate(data_de_inicio_da_execucao = como_data(data_de_inicio_da_execucao),
         data_de_termino_da_execucao = como_data(data_de_termino_da_execucao),
         valor_contrato_deflacionado = deflate(valor_contrato, data_de_inicio_da_execucao, "09/2019", "ipca") )
                       
tcu_creches <- tcu_creches %>%
  mutate(end = paste(municipio, uf, "Brasil", sep=" , "))

ends <- tcu_creches1 %>%
  group_by(end) %>%
  summarise(total = n())
lat_lon <- geocode(ends$end)

ends <- bind_cols(ends, lat_lon)

tcu_creches <- tcu_creches %>%
  left_join(ends, by=c("end")) %>%
  select(-c(total, end)) %>%
  filter(programa != "Brasil Profissionalizado",
         esfera == "Municipal")

setwd("C:/Users/coliv/Documents/R-Projects/FMCSV/resultados")
save(tcu_creches , file="tcu_creches.Rdata")
