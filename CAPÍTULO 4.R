## CAPÍTULO 4 ##

library(pastecs)
library(moments)
library(readr)
library(stringr)
library(plyr)
library(dplyr)
library(scales)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(ggthemes)
library(lattice)
library(RColorBrewer)
library(extrafont)
library(reshape)
library(zoo)
font_import()
loadfonts(device="win") 
fonts()

############### LIQUIDAÇÃO DO COMÉRCIO EXTERIOR EM RMB ####################

CHINA_EXP_IMP1 <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/NBS/CHINA_IMPORTS_EXPORTS.csv")

RMB_TRADE_SETTLE <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/Wind/RMB_Trade_Settle.csv")

CHN_RMB_TS1 <- data.frame(DATA = RMB_TRADE_SETTLE$DATA,
                         RMB_TS = RMB_TRADE_SETTLE$RMB_SETTLE)

CHN_RMB_TS2 <- rename(CHN_RMB_TS1, c(DATA = "DATA",RMB_TS = "FATURAÇÃO DO COMÉRCIO EM RMB"))

CHN_RMB_TS3 <- melt(CHN_RMB_TS2)

CHN_RMB_TS4 <- data.frame(DATA = as.Date(CHN_RMB_TS3$DATA, format = "%d/%m/%Y"),
                          value = CHN_RMB_TS3$value,
                          variable = CHN_RMB_TS3$variable)

# PLOTAR GRÁFICOS:

RMB_TS <- ggplot(CHN_RMB_TS4, aes(x = DATA, y = value, fill = variable)) + geom_bar(stat = "identity", width=50) + scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") + ylab(" 100 MI ¥ ")  + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=7.5,color="black",angle = 60,vjust=0.50), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                        legend.position = "none",
                                                                                                                                                                                                                                                                                                                        plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                        axis.ticks = element_blank(), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

RMB_TS


############### LIQUIDAÇÃO DO COMÉRCIO EXTERIOR EM RMB ####################

CHINA_INFLOW_OUTFLOW <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/IMF/ENTR_SAIDA_INVEST.csv")

ENTR_CAP1 <- data.frame(DATA = CHINA_INFLOW_OUTFLOW$DATA,
                       ENTR_IED = CHINA_INFLOW_OUTFLOW$ENTR_FDI/10^9,
                       ENTR_PORT = CHINA_INFLOW_OUTFLOW$ENTR_PORTFOLIO/10^9,
                       ENTR_OUTROS = CHINA_INFLOW_OUTFLOW$ENTR_OUTROS/10^9)

SAIDA_CAP1 <- data.frame(DATA = CHINA_INFLOW_OUTFLOW$DATA,
                        SAIDA_IED = CHINA_INFLOW_OUTFLOW$SAIDA_FDI/10^9,
                        SAIDA_PORT = CHINA_INFLOW_OUTFLOW$SAIDA_PORTFOLIO/10^9,
                        SAIDA_OUTROS = CHINA_INFLOW_OUTFLOW$SAIDA_OUTROS/10^9)

ENTR_CAP2 <- rename(ENTR_CAP1,
                    c(DATA = "DATA",
                      ENTR_IED = "FLUXO DE IED",
                      ENTR_PORT = "INVESTIMENTOS EM PORTFÓLIO",
                      ENTR_OUTROS = "OUTROS INVESTIMENTOS"))

SAIDA_CAP2 <- rename(SAIDA_CAP1,
                    c(DATA = "DATA",
                      SAIDA_IED = "FLUXO DE IED",
                      SAIDA_PORT = "INVESTIMENTOS EM PORTFÓLIO",
                      SAIDA_OUTROS = "OUTROS INVESTIMENTOS"))


ENTR_CAP3 <- melt(ENTR_CAP2)

ENTR_CAP4 <- data.frame(DATA = as.Date(ENTR_CAP3$DATA, format = "%d/%m/%Y"),
                          value = ENTR_CAP3$value,
                          variable = ENTR_CAP3$variable)

SAIDA_CAP3 <- melt(SAIDA_CAP2)

SAIDA_CAP4 <- data.frame(DATA = as.Date(SAIDA_CAP3$DATA, format = "%d/%m/%Y"),
                        value = SAIDA_CAP3$value,
                        variable = SAIDA_CAP3$variable)

# PLOTAR GRÁFICOS:

CORES_FLOWS <- c("FLUXO DE IED" = "royalblue4",
                 "INVESTIMENTOS EM PORTFÓLIO" = "red",
                 "OUTROS INVESTIMENTOS" = "hotpink3")

PLOT_ENTR_CAP <- ggplot(ENTR_CAP4, aes(x = DATA, y = value, fill = variable)) + geom_bar(stat = "identity",position='stack',width=100) + geom_hline(yintercept = 0) + scale_x_date(date_breaks ="1 years", date_labels = "%Y") + ylab(" 1 BI US$ ") + ggtitle(" ENTRADA BRUTA DE CAPITAIS ") + theme_minimal() + scale_fill_manual(values = c(CORES_FLOWS)) + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=8,color="black",angle = 60,vjust=0.50), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                   legend.position = c("top"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"), legend.title = element_blank(),
                                                                                                                                                                                                                                                                                                                                                                                                                   plot.title = element_text(hjust = 0.01, size = 10),
                                                                                                                                                                                                                                                                                                                                                                                                                   axis.ticks = element_blank(), axis.line.y = element_blank(), axis.line.x = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                                                                                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

PLOT_ENTR_CAP


PLOT_SAIDA_CAP <- ggplot(SAIDA_CAP4, aes(x = DATA, y = value, fill = variable)) + geom_bar(stat = "identity",position='stack',width=100) + geom_hline(yintercept = 0) + scale_x_date(date_breaks ="1 years", date_labels = "%Y") + ylab(" 1 BI US$ ") + ggtitle(" SAÍDA BRUTA DE CAPITAIS ") + theme_minimal() + scale_fill_manual(values = c(CORES_FLOWS)) + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=8,color="black",angle = 60,vjust=0.50), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                   legend.position = c("top"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"), legend.title = element_blank(),
                                                                                                                                                                                                                                                                                                                                                                                                                   plot.title = element_text(hjust = 0.01, size = 10),
                                                                                                                                                                                                                                                                                                                                                                                                                   axis.ticks = element_blank(), axis.line.y = element_blank(), axis.line.x = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                                                                                   panel.grid.major = element_blank(), panel.grid.minor = element_blank())

PLOT_SAIDA_CAP


# JUNTAR GRÁFICOS

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(PLOT_ENTR_CAP)

PLOT_FLOWS <- grid.arrange(arrangeGrob(PLOT_ENTR_CAP + theme(legend.position="none"),
                                       PLOT_SAIDA_CAP + theme(legend.position="none"),
                                       ncol=1, nrow=2,heights=c(0.5, 0.5)),
                           mylegend, heights=c(0.85, 0.15))

PLOT_FLOWS



############### RESERVAS CAMBIAIS OFICIAIS - % POR MOEDAS ####################

RESERVAS_OFICIAIS <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/IMF/RESERVAS_OFICIAIS.csv")

RESERVAS_OFICIAIS <- rename(RESERVAS_OFICIAIS,
                    c(ANOS = "DATA",
                      DOLAR = "DÓLAR (US$)",
                      LIBRA = "LIBRA ESTERLINA (£)",
                      EURO = "EURO (???)",
                      YEN = "YEN JAPÔNES (¥)",
                      MARCO.ALEMAO = "MARCO ALEMÃO (DEM)",
                      FRANCO.FRANCES = "FRANCO FRANCÊS (FF)",
                      FRANCO.SUICO = "FRANCO SUÍÇO (CHF)"))

RESERVAS_DATASET1 <- melt(RESERVAS_OFICIAIS)

RESERVAS_DATASET2 <- data.frame(DATA = as.Date(RESERVAS_DATASET1$DATA, format = "%d/%m/%Y"),
                                value = RESERVAS_DATASET1$value,
                                variable = RESERVAS_DATASET1$variable)


# PLOTAR GRÁFICOS:

COR_MOEDAS <- c("DÓLAR (US$)" = "tomato4",
                "LIBRA ESTERLINA (£)" = "limegreen",
                "EURO (???)" = "tan4",
                "YEN JAPÔNES (¥)" = "darkorchid4",
                "MARCO ALEMÃO (DEM)" = "black",
                "FRANCO FRANCÊS (FF)" = "cyan2",
                "FRANCO SUÍÇO (CHF)" = "orchid1")


PLOT_RESERVAS <- ggplot(RESERVAS_DATASET2, aes(x = DATA, y = value, group = variable, color = variable)) + geom_line(aes(linetype = variable),size = 0.75) + geom_point(size=1.2) + scale_x_date(limits = as.Date(c('1956-01-01','2014-01-01')),date_breaks ="2 year", date_labels = "%Y") + ylab(" % ") + theme_hc() + scale_color_manual(values = c(COR_MOEDAS)) + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                                           legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                                                                           plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                                                                           axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                                           panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))                                                                                                                                                                                                                                                                                                                       




PLOT_RESERVAS

