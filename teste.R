# Title     : TODO
# Objective : TODO
# Created by: joyma
# Created on: 10/06/2020
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(grid)
library(extrafont)
library(tidyverse)
library(lubridate)
library(cowplot) # 1.0.0
library(egg) # 0.4.5

setwd("D:/Documents/R")
forecast <- read.csv("forecast_2.csv",encoding = "UTF-8",sep = ";")
sapply(forecast,summary)
'----------------------------------------------------------------------------------------------------------------------'
#https://stackoverflow.com/questions/54413787/how-to-plot-a-2-y-axis-chart-
# with-bars-side-by-side-without-re-scaling-the-data/54426831#54426831

scaleFactor1 <-  max(forecast$monetario) / max(forecast$X.U.FEFF.metros)
scaleFactor2 <-  max(forecast$monetario)/ max(forecast$num_sondagem)

ggplot(forecast, aes(x=month(data),  width=.4)) +
  geom_col(aes(y=monetario, colour="R$"), position = position_nudge(x = -.4)) +
  geom_col(aes(y=X.U.FEFF.metros * scaleFactor1, colour="m")) +
  scale_y_continuous(name="R$", sec.axis=sec_axis(~./scaleFactor1, name="m"))  +
  scale_x_discrete(breaks = seq(1, 12, 1)) +
  labs(title = "Forecast Mensal", xlab("Mes"))




'----------------------------------------------------------------------------------------------------------------------'

scaleFactor1 <-  max(forecast$monetario) / max(forecast$X.U.FEFF.metros)
scaleFactor2 <-  max(forecast$monetario)/ max(forecast$num_sondagem)

ggplot(forecast, aes(x=month(data),  width=.4)) +
  geom_col(aes(y=monetario,fill="R$"), position = position_nudge(x = -.4)) +
  geom_col(aes(y=X.U.FEFF.metros * scaleFactor1, fill="m")) +
  scale_y_continuous(name="R$", sec.axis=sec_axis(~./scaleFactor1, name="m") )  +
  geom_line(aes(y=num_sondagem * scaleFactor2, fill="qtd")) +
  scale_x_discrete(breaks = seq(1, 12, 1)) +
  #labs(title = "Forecast Mensal", fill="")+
  theme(legend.position=c(0.1,1),legend.direction="horizontal")
axis(2,at = seq(50,150,20),labels = TRUE, cex.axis = .4, lwd.ticks = 1)

'----------------------------------------------------------------------------------------------------------------------'
p <- ggplot(forecast,  width=.4, aes(x=month(data)) )
+ geom_bar(aes(y = monetario/350420,col="monetario"),stat = "identity", position = position_nudge(x = -.4))
p2 <- p + geom_bar(aes(y = X.U.FEFF.metros,col="metros"), stat = "identity") +  geom_col(position="dodge")
p3 <- p2 + scale_y_continuous(sec.axis = sec_axis(~.*350420))

p <- ggplot(forecast) + aes(x=data) + geom_line(aes(y = monetario/350420,col="monetario"))
p2 <- p + geom_line(aes(y = X.U.FEFF.metros,col="metros"))
p3 <- p2 + scale_y_continuous(sec.axis = sec_axis(~.*350420))


ggplot(forecast, aes(x=data, y=monetario/350420)) + aes(x=data,y = Sondagem) +
  geom_col(position="dodge") +
  labs(x="data", y="monetario", fill="sondagem")
'----------------------------------------------------------------------------------------------------------------------'
df <- data.frame(DateTime = ymd("2010-07-01") + c(0:8760) * hours(2), series1 = rnorm(8761), series2 = rnorm(8761, 100))

plot1 <- df %>%
  select(DateTime, series1) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = DateTime, y = series1), size = 0.5, alpha = 0.75) +
  ylab("Red dots / m") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

plot2 <- df %>%
  select(DateTime, series2) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = DateTime, y = series2), size = 0.5, alpha = 0.75) +
  ylab("Blue drops / L") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

'----------------------------------------------------------------------------------------------------------------------'


df <- data.frame(DateTime = ymd("2010-07-01") + c(0:8760) * hours(2),
                 series1 = rnorm(8761),
                 series2 = rnorm(8761, 100))

#' Create the two plots.
plot1 <- df %>%
  select(DateTime, series1) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = DateTime, y = series1), size = 0.5, alpha = 0.75) +
  ylab("Red dots / m") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())

plot2 <- df %>%
  select(DateTime, series2) %>%
  na.omit() %>%
  ggplot() +
  geom_point(aes(x = DateTime, y = series2), size = 0.5, alpha = 0.75) +
  ylab("Blue drops / L") +
  theme_minimal() +
  theme(axis.title.x = element_blank())

# Draw the two plot aligned vertically, with the top plot 1/3 of the height
# of the bottom plot
cowplot::plot_grid(plot1, plot2, align = "v", ncol = 1, rel_heights = c(0.25, 0.75))
egg::ggarrange(plot1, plot2, heights = c(0.25, 0.75))


'----------------------------------------------------------------------------------------------------------------------'


#Barra - Tipo de hospedagem por grupo de vizinhança
barplot(forecast,
        main = "Tipo de Hospedagem por Grupo de Vizinhança",
        ylab = "%",
        xlab = "Vizinhança",
        ylim = c(0, 35),
        lwd = 1,
        axis.lty = 1,
        cex.axis = .45, cex.main = .45, cex.lab = .45, cex.names = .45,
        col = c("snow4", "chartreuse4"),
        beside = TRUE
)
legend(x=2.35,y=40, xpd=TRUE, ncol=2, legend=c("Ap/Casa inteira", "Quarto Privativo"),
       fill=c(col=alpha("snow4"), col=alpha("chartreuse4")), bty="n", cex = .4
  ,yjust = .5
)
