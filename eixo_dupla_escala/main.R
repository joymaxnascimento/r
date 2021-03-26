# Title     : Gráfico com duplo eixo em escalas diferentes
# Objective : Estudo
# Created by: Joymax
# Created on: 10/06/2020
'Importações-----------------------------------------------------------------------------------------------------------'
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(tidyverse)
library(lubridate)
'---------------------------------------------------------------------------------------------------Fim das importações'

'Definindo diretório de trabalho---------------------------------------------------------------------------------------'
setwd("D:/Documents/GitHub/r/eixo_dupla_escala")
getwd()
'-----------------------------------------------------------------------------Fim da definição do diretório de trabalho'

'Leitura dos arquivos--------------------------------------------------------------------------------------------------'
bovespa <- read.csv("~/GitHub/r/eixo_dupla_escala/bovespa.csv", encoding="UTF-8") +
  mutate(data=as.Date(bovespa$X.U.FEFF.Data,format("%d.%m.%Y")),
         indiceBovespa=as.numeric(str_replace(str_replace(bovespa$"Último","\\.",""),",","\\."))
) +
select(data,indiceBovespa)
'--------------------------------------------------------------------------------------------Fim da leitura dos aquivos'


dir <- '/home/mercel/mega/work/conteudo/ggplot2Axis/'
arq <- 'dadosDolar2.csv'
arq2 <- 'bovespa.csv'

completo <- paste0(dir,arq)
completo2 <- paste0(dir,arq2)

#########################################
####  Lendo e processando os dados  #####
#########################################

bovespa <- read.csv(completo2) %>%
  mutate(data=as.Date(Data,format('%d.%m.%Y')),
         indiceBovespa=as.numeric(str_replace(str_replace(Último,"\\.",""),",","\\."))
) %>%
select(data,indiceBovespa)

dolar  <- read.csv(completo) %>%
mutate(data=as.Date(Data,format('%d.%m.%Y')),
dolar=as.numeric(gsub(",","\\.",Último))
) %>%
select(data,dolar)

#########################################
####  Juntando os data.frames       #####
#########################################

dados <- merge(dolar,bovespa)

#########################################
####  Plotando os dados             #####
#########################################

#definindo o tema
theme_set(theme_economist())

#plotando os dados
p1 <- ggplot(dados,aes(x=data))+
geom_line(aes(y=dolar,colour="Dólar"))

p2 <- p1 + geom_line(aes(y=indiceBovespa/37497,col="Bovespa"))

# Ajustando escalas e legendas
p2 <- p2 + scale_y_continuous(name="Valor do Dólar (R$)",
sec.axis = sec_axis(~.*37497,name = "Índice Bovespa"))

p2 <- p2 + scale_colour_manual(values = c("#F57E6B", "#0D9ED7"))
p2 <- p2 + labs(title="Dolar X Bovespa",colour = NULL,x=NULL)

p2 <- p2 + theme(legend.position = c(0.9, 0.2),
plot.title = element_text(family = 'Avenir Next Condensed', hjust=0,size=13,margin=margin(-7,0,10,0))
)

#########################################
####  Salvando a figura             #####
#########################################

ggsave(filename = paste0(dir,"/figs/dolar_bovespa.png"),
plot = p2)


loc <-  Sys.getlocale()




