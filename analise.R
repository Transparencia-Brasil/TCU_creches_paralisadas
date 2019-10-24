#Analise

library(dplyr)
library(ggplot2)
library(janitor)
library(xlsx)
library(hrbrthemes)
library(googledrive)


drive_find(n_max=10) 

setwd("C:/Users/coliv/Documents/R-Projects/FMCSV/resultados")
load("tcu_creches.Rdata")

#Endereços das obras (mapa)

ends <- tcu_creches %>%
  distinct(municipio, uf, situacao_da_obra, lat, lon)

fwite(ends, file="ends.csv")


# Tabela 1:

motivos <- c("Problemas de Infraestrutura", "Embargos")

tbl1 <- tcu_creches %>%
  filter(situacao_da_obra == "Paralisada") %>%
  mutate(tipo_de_paralisacao = ifelse(is.na(tipo_de_paralisacao), "Não informado", tipo_de_paralisacao),
         dificuldade_de_retomada = ifelse(tipo_de_paralisacao %in% motivos, "Alta", "Baixa")) %>%
  group_by(dificuldade_de_retomada, tipo_de_paralisacao) %>%
  summarise(obras_paralisadas = n()) %>%
  arrange(desc(dificuldade_de_retomada), desc(obras_paralisadas)) %>%
  adorn_totals("row") %>%
  select(dificuldade_de_retomada, tipo_de_paralisacao, obras_paralisadas) 

colnames(tbl1) <- c("Dificuldade de retomada", "Motivo da paralisação", "Quantidade de obras paralisadas")

setwd("C:/Users/coliv/Documents/R-Projects/FMCSV/resultados")
write.xlsx(tbl1, file="tbl1.xlsx")

#Quanto custaram essas obras

tcu_creches %>%
  filter(situacao_da_obra == "Paralisada",
   !tipo_de_paralisacao %in% motivos) %>%
  summarise(valor_contrato_total = sum(valor_contrato, na.rm=TRUE)) #833.959.664

#Quanto custaram todas as obras:

tcu_creches %>%
  summarise(valor_total = sum(valor_contrato , na.rm=TRUE)) #1.579.420.443 ou R$ 1,5 bilhão

833959664 / 1579420443
  

#Distribuição do percentual de execução.

x <- tcu_creches %>%
  mutate(probabilidade_para_retomada = ifelse(tipo_de_paralisacao %in% motivos |
                                              situacao_da_obra == "Inacabada", "Menos provável",
                                              "Mais provável"),
         executado = gsub("%", "", percent_executado ),
         executado = as.numeric(executado),
         executado = ifelse(is.na(executado), 0, executado),
         status_executado = ifelse(executado < 10.01, "Até 10%",
                                   ifelse(executado > 10 & executado < 30.01, "Entre 10% e 30%",
                                          ifelse(executado > 30 & executado < 50.01, "Entre 30% e 50%",
                                                 ifelse(executado > 50 & executado < 80.01, "Entre 50% e 80%",
                                                         ifelse(executado > 80, "Acima de 80%", "Erro")))))) %>%
   group_by(probabilidade_para_retomada, status_executado) %>%
   summarise(total_obras = n(),
             perc = total_obras/1398)

#Reordenando
x$status_executado <- factor(x$status_executado, levels = c("Até 10%" ,"Entre 10% e 30%", "Entre 30% e 50%", "Entre 50% e 80%", "Acima de 80%" ))
x$probabilidade_para_retomada <- factor(x$probabilidade_para_retomada, levels = c("Menos provável", "Mais provável"))

ggplot(data=x, aes(x=status_executado, y=total_obras, fill=probabilidade_para_retomada)) +
  geom_bar(stat="identity") + theme_ipsum() +
  geom_text(aes(label = total_obras), position = position_stack(vjust = 0.5)) +
  labs(fill = "Probabilidade de retomada da obra" , 
       title = "Quantidade de obras x Percentual executado") +
  xlab("Percentual executado") +  ylab("Qtde obras") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

#Anexo 1

anexo1 <- tcu_creches %>%
  filter(situacao_da_obra == "Paralisada",
         !tipo_de_paralisacao %in% motivos) %>%
  mutate(tipo_de_paralisacao = ifelse(is.na(tipo_de_paralisacao), "Não informado", tipo_de_paralisacao)) %>%
  select(-c(lon, lat, valor_contrato_deflacionado))

colnames(anexo1) <- c("Uf", "Município", "Unidade implantadora", "Data de início da execução", 
                      "Data de interrupmento da execução", "Situação da obra", "Tipo de paralisação",
                      "Percentual executado", "Programa", "Fonte", "Esfera", "Tipologia", "Valor contrato",
                      "Valor previsto")

write.xlsx(as.data.frame(anexo1), file="anexo1.xlsx",
           sheetName="Anexo 1",col.names=TRUE, row.names=FALSE, append=FALSE, showNA=FALSE)

drive_upload(
  "anexo1.xlsx",
  path="~/TB/2019/FMCSV",
  name = "Anexo 1 -  Obras com maior probabilidade de retomada",
  type = "spreadsheet")


#Anexo 2 - Motivos da paralisação obras com menor viabilidade de serem retomadas

tbl2 <- tcu_creches %>%
  filter(situacao_da_obra == "Inacabada"|
          tipo_de_paralisacao %in% motivos) %>%
  mutate(tipo_de_paralisacao = ifelse(is.na(tipo_de_paralisacao), "Não informado", tipo_de_paralisacao)) %>%
  group_by(situacao_da_obra, tipo_de_paralisacao) %>%
  summarise(obras = n()) %>%
  arrange(desc(situacao_da_obra),desc(obras)) %>%
  adorn_totals("row")

colnames(tbl2) <- c("Situação da obra", "Motivo da interrupção", "Quantidade de obras")

setwd("C:/Users/coliv/Documents/R-Projects/FMCSV/resultados")
write.xlsx(tbl2, file="tbl2.xlsx")
