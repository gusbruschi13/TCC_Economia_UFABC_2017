## CAPÍTULO 1 ##

library(pastecs)
library(moments)
library(readr)
library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggthemes)
library(RColorBrewer)
library(extrafont)
library(reshape)
library(zoo)
library(forecast)
font_import()
loadfonts(device="win") 
fonts()

############# TAXA DE CRESCIMENTO DO PIB CHINÊs NO LONGO PRAZO (1952-2014) EM % ##############

CH_PIB <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/NBS/PIB_1952_2014.csv")
PIB <- ts(CH_PIB$PIB_R)

options(scipen=100)
options(digits=2)
CHINA_PIB1 <- data.frame(DATA = as.Date(CH_PIB$ANO, format = "%d/%m/%Y"),
                         PIB = PIB,
                         TX_PIB = c(NA,exp(diff(log(PIB)))-1))

options(scipen=100)
options(digits=2)
CHINA_PIB2 <- data.frame(DATA = CHINA_PIB1$DATA,
                         PIB = CHINA_PIB1$PIB,
                         TX_PIB = CHINA_PIB1$TX_PIB)



CHINA_VA1 <- data.frame(DATE = CH_PIB$ANO,
                        VA_INDUST_PRIMARIA = CH_PIB$INDUST_PRIMARIA,
                        VA_INDUST_SECUNDARIA = CH_PIB$INDUST_SECUNDARIA,
                        VA_INDUST_TERCIARIA = CH_PIB$INDUST_TERCIARIA)
CHINA_VA2 <- rename(CHINA_VA1, c(DATE = "DATE", VA_INDUST_PRIMARIA = "INDÚSTRIA PRIMÁRIA", VA_INDUST_SECUNDARIA = "INDÚSTRIA SECUNDÁRIA", VA_INDUST_TERCIARIA = "INDÚSTRIA TERCIÁRIA"))
CHINA_VA3 <- melt(CHINA_VA2)
CHINA_VA4 <- data.frame(DATA = as.Date(CH_PIB$ANO, format = "%d/%m/%Y"),
                        variable = CHINA_VA3$variable,
                        value = CHINA_VA3$value)

# PLOTAR GRÁFICOS:

CHN_PIB <- ggplot(CHINA_PIB2, aes(x = DATA)) + geom_line(aes(y = TX_PIB),colour = "black",linetype = "longdash",size = 0.7) + geom_point(aes(y = TX_PIB),size = 1.3) + geom_smooth(aes(y = TX_PIB),colour="darkred",linetype="solid",size=1,method="loess",fill="darksalmon",level=0.95) + geom_hline(yintercept = 0) + scale_x_date(limits = as.Date(c('1952-01-01','2014-01-01')),date_breaks ="4 year", date_labels = "%Y") + scale_y_continuous(limits = c(-0.4,0.4), breaks = c(-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4)) + ylab(" % ")  + theme_hc() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.line.x = element_blank(),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))                                                                                                                                                                                                                                                                                                                      


CHN_PIB




LEGENDA_VA <- c("INDÚSTRIA PRIMÁRIA" = "red","INDÚSTRIA SECUNDÁRIA" = "green3","INDÚSTRIA TERCIÁRIA" = "royalblue3")

CHN_VA <- ggplot(CHINA_VA4, aes(x = DATA, y = value, fill = variable)) + geom_area() + scale_x_date(limits = as.Date(c('1952-01-01','2014-01-01')),date_breaks = "4 year", date_labels = "%Y") + scale_y_continuous(breaks = c(0,500000,1000000,1500000,2000000,2500000,3000000,3500000,4000000,4500000,5000000,5500000,6000000,6500000,7000000)) + scale_fill_manual(values = LEGENDA_VA) + ylab("V.A. PIB / INDÚSTRIA \n EM BI ¥") + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                               legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                               plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                                                                                                                                                               axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                                                                                                                               panel.grid.major = element_blank(), panel.grid.minor = element_blank())                                                                                                                                                                                                                                                                                                                      

CHN_VA




####################### ACUMULAÇÃO DE RESERVAS  #################

ACUM_RESERVAS <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/SAFE/RESERVAS.csv")

CHINA_ACUM_RESERVAS <- data.frame(DATA = as.Date(ACUM_RESERVAS$ANO, format = "%d/%m/%Y"),
                                  RESERVAS = ts(ACUM_RESERVAS$RESERVAS),
                                  TX_ACUM = ACUM_RESERVAS$TX_ACUM)

# PLOTAR GRÁFICOS:

CHN_RESERVAS <- ggplot(CHINA_ACUM_RESERVAS, aes(x = DATA)) + geom_bar(aes(y = RESERVAS),stat='identity',position='dodge',width=80) + scale_x_date(limits = as.Date(c('1990-01-01','2015-01-01')),date_breaks ="3 year", date_labels = "%Y") + ylab(" 100 MI US$ ") + ggtitle("Acumulação de Reservas Cambiais na China\n (1990-2015)")  + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                         legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                                                         plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                                                         axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                         panel.grid.major = element_blank(), panel.grid.minor = element_blank())                                                                                                                                                                                                                                                                                                                      


CHN_RESERVAS



