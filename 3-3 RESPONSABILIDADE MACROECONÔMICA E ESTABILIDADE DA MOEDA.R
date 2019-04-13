## "RESPONSABILIDADE" MACROECONÔMICA E ESTABILIDADE DA MOEDA ##

library(pastecs)
library(moments)
library(readr)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(ggthemes)
library(lattice)
library(RColorBrewer)
library(extrafont)
library(reshape)
font_import()
loadfonts(device="win") 
fonts()

####### TRADE-OFF ENTRE INFLAÇÃO E PRODUTO (1997-2014) EM % #######

# ABRIR BASES E REALIZAR AS TRANSFORMAÇÕES: 

CH_PIB1 <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/NBS/PIB_1952_2014.csv")
CH_INFL1 <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/NBS/INFLACAO.csv")


CH_PIB2 <- data.frame(DATA = as.Date(CH_PIB1$ANO, format = "%d/%m/%Y"),
                      PIB = (CH_PIB1$PIB_R)/10,
                      TX_PIB = c(NA,exp(diff(log(CH_PIB1$PIB)))-1))

CH_INFL2 <- data.frame(DATA = as.Date(CH_INFL1$ANO, format = "%d/%m/%Y"),
                       TX_INFL = c(NA,exp(diff(log(CH_INFL1$INFL)))-1),
                       CPI = CH_INFL1$CPI,
                       INFL = CH_INFL1$INFL)

attach(CH_PIB2)
CH_PIB3 <- CH_PIB2[which(DATA >= '1996-01-01'),]
detach(CH_PIB2)
attach(CH_INFL2)
CH_INFL3 <- CH_INFL2[which(DATA >= '1996-01-01'),]
detach(CH_INFL2)

CH_TRADEOFF1 <- data.frame(DATA = CH_PIB3$DATA,
                          TX_PIB = ts(CH_PIB3$TX_PIB),
                          TX_INFL = ts(CH_INFL3$TX_INFL))

options(scipen=100)
options(digits=2)
attach(CH_TRADEOFF1)
CH_TRADEOFF2 <- CH_TRADEOFF1[which(DATA >= '1997-01-01'),]
detach(CH_TRADEOFF1)

CH_TRADEOFF2 <- rename(CH_TRADEOFF2,
                       c(ANOS = "DATA",
                         TX_PIB = "TAXA CRESC. PIB",
                         TX_INFL = "TAXA CRESC. INFLAÇÃO"))

CH_TRADEOFF3 <- melt(CH_TRADEOFF2,id=c("DATA"))

CH_TRADEOFF4 <- data.frame(DATA = as.Date(CH_TRADEOFF3$DATA, format = "%d/%m/%Y"),
                             value = (CH_TRADEOFF3$value)*100,
                             variable = CH_TRADEOFF3$variable)

# PLOTAR GRÁFICOS:

TRADEOFF <- ggplot(CH_TRADEOFF4, aes(x = DATA, y = value, group = variable, colour = variable)) + geom_line(aes(linetype = variable), size = 0.75) + geom_point(shape=21,fill="white",size=2.2) + geom_hline(yintercept = 0) + scale_x_date(limits = as.Date(c('1997-01-01','2014-01-01')),date_breaks ="1 year", date_labels = "%Y") + ylab(" % ") + theme_hc() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_text(size=10,color="black"), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                                         legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                                                                         plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                                                                         axis.ticks = element_blank(), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_blank(),
                                                                                                                                                                                                                                                                                                                                                                         panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))                                                                                                                                                                                                                                                                                                                      

TRADEOFF
