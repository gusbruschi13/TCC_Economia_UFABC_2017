## TAMANHO E DESENVOLVIMENTOS COM MERCADOS FINANCEIROS ##

library(pastecs)
library(moments)
library(readr)
library(stringr)
library(plyr)
library(dplyr)
library(scales)
library(ggplot2)
library(ggrepel)
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

######### COMPOSIÇÃO DO SISTEMA BANCÁRIO (2001-2014) EM % ###################

CBRC_DATA <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/CBRC/CBRC_DATA.csv")

PIB <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/NBS/PIB_1952_2014.csv")

EDIT_PIB1 <- data.frame(DATA = as.Date(PIB$ANO, format = "%d/%m/%Y"),
                        PIB = (PIB$PIB))
attach(EDIT_PIB1)
EDIT_PIB2 <- EDIT_PIB1[which(DATA >= '2003-01-01'),]
detach(EDIT_PIB1)

# COMPOSIÇÃO DOS ATIVOS POR CATEGORIA DE BANCO (2004,2007,2011,2014)

options(scipen=100)
options(digits=2)
COMP_ATIV_BANC1 <- data.frame(DATA = CBRC_DATA$DATA,
                             POL_CDB_BANC = (CBRC_DATA$ATIVOS.Policy.Banks...CDB),
                             BIG_COM_BANC = (CBRC_DATA$ATIVOS.Grandes.Bancos.Comerciais),
                             BANC_MIST_CAP = (CBRC_DATA$ATIVOS.Bancos.Comerciais.de.Capital.Misto),
                             BANC_COMERC_CID = (CBRC_DATA$ATIVOS.Bancos.Comerciais.das.Cidades),
                             BANC_COMERC_RUR = (CBRC_DATA$ATIVOS.Bancos.Comerciais.Rurais),
                             BANC_COOP_RUR = (CBRC_DATA$ATIVOS.Bancos.das.Cooperativas.Rurais),
                             CRED_COOP_URB = (CBRC_DATA$ATIVOS.Cooperativas.de.Credito.Urbanas),
                             CRED_COOP_RUR = (CBRC_DATA$ATIVOS.Cooperativas.de.Credito.Rurais),
                             IF_N_BANC = (CBRC_DATA$ATIVOS.Instituicoes.Financeiras.Nao.Bancarias),
                             ESTR_BANC = (CBRC_DATA$ATIVOS.Bancos.Estrangeiros),
                             NV_IF_RUR = (CBRC_DATA$ATIVOS.Novas.Instituicoees.Financeiras.Rurais.e.Bancos.de.Poupanca.Rural))

COMP_ATIV_BANC2 <- rename(COMP_ATIV_BANC1, c(DATA = "DATA",
                                             POL_CDB_BANC = "POLICY BANKS E CDB",
                                             BIG_COM_BANC = "GRANDES BANCOS COMERCIAIS",
                                             BANC_MIST_CAP = "BANCOS COMERCIAIS DE CAPITAL MISTO",
                                             BANC_COMERC_CID = "BANCOS COMERCIAIS URBANOS",
                                             BANC_COMERC_RUR = "BANCOS COMERCIAIS RURAIS",
                                             BANC_COOP_RUR = "BANCOS COOPERATIVAS DE CRÉDITO RURAIS",
                                             CRED_COOP_URB = "COOPERATIVAS DE CRÉDITO URBANO",
                                             CRED_COOP_RUR = "COOPERATIVAS DE CRÉDITO RURAL",
                                             IF_N_BANC = "INSTITUIÇÕES FINANCEIRAS NÃO-BANCÁRIAS",
                                             ESTR_BANC = "BANCOS ESTRANGEIROS",
                                             NV_IF_RUR = "NOVAS INSTITUIÇÕES FINANCEIRAS E DE POUPNAÇA RURAL"))

attach(COMP_ATIV_BANC2)
COMP_ATIV_BANC_2004 <- COMP_ATIV_BANC2[which(DATA == '01/01/2004'),]
detach(COMP_ATIV_BANC2)

COMP_ATIV_BANC_2004A <- melt(COMP_ATIV_BANC_2004,id=c("DATA"))


attach(COMP_ATIV_BANC2)
COMP_ATIV_BANC_2007 <- COMP_ATIV_BANC2[which(DATA == '01/01/2007'),]
detach(COMP_ATIV_BANC2)

COMP_ATIV_BANC_2007A <- melt(COMP_ATIV_BANC_2007,id=c("DATA"))


attach(COMP_ATIV_BANC2)
COMP_ATIV_BANC_2011 <- COMP_ATIV_BANC2[which(DATA == '01/01/2011'),]
detach(COMP_ATIV_BANC2)

COMP_ATIV_BANC_2011A <- melt(COMP_ATIV_BANC_2011,id=c("DATA"))

attach(COMP_ATIV_BANC2)
COMP_ATIV_BANC_2014 <- COMP_ATIV_BANC2[which(DATA == '01/01/2014'),]
detach(COMP_ATIV_BANC2)

COMP_ATIV_BANC_2014A <- melt(COMP_ATIV_BANC_2014,id=c("DATA"))


# RELAÇÃO DOS DEPÓSITOS E EMPRÉSTIMO SOBRE ATIVOS

options(scipen=100)
options(digits=2)
DEP_EMPR_BANC1 <- data.frame(DATA = as.Date(CBRC_DATA$DATA, format = "%d/%m/%Y"),
                             DEP_BANC = (CBRC_DATA$Depositos.Totais)/(CBRC_DATA$ATIVOS.Instituicoes.Bancarias),
                             EMPR_BANC = (CBRC_DATA$Emprestimos.Totais)/(CBRC_DATA$ATIVOS.Instituicoes.Bancarias))

DEP_EMPR_BANC2 <- rename(DEP_EMPR_BANC1, c(DATA = "DATA", EMPR_BANC = "EMPRÉSTIMOS", DEP_BANC = "DEPÓSITOS"))

DEP_EMPR_BANC3 <- melt(DEP_EMPR_BANC2,id=c("DATA"))

DEP_EMPR_BANC4 <- data.frame(DATA = as.Date(DEP_EMPR_BANC3$DATA, format = "%d/%m/%Y"),
                             value = DEP_EMPR_BANC3$value,
                             variable = DEP_EMPR_BANC3$variable)

# RELAÇÃO DOS EMPRÉSTIMOS SOBRE DEPÓSITOS

options(scipen=100)
options(digits=2)
DEP_EMPR_RATIO1 <- data.frame(DATA = as.Date(CBRC_DATA$DATA, format = "%d/%m/%Y"),
                             RATIO = (CBRC_DATA$Emprestimos.Totais)/(CBRC_DATA$Depositos.Totais))


### PLOTAR GRÁFICOS:

# COMPOSIÇÃO DOS ATIVOS POR CATEGORIA DE BANCO (2004,2007,2011,2014)

# ADICÕES PARA O "DONUT PLOT"
COMP_ATIV_BANC_2004A$fraction = COMP_ATIV_BANC_2004A$value/sum(COMP_ATIV_BANC_2004A$value)
COMP_ATIV_BANC_2004A$perc = percent(c(round(prop.table(COMP_ATIV_BANC_2004A$fraction), 3)))
COMP_ATIV_BANC_2004A$ymax = cumsum(COMP_ATIV_BANC_2004A$fraction)
COMP_ATIV_BANC_2004A$ymin = c(0, head(COMP_ATIV_BANC_2004A$ymax, n = -1))

COMP_ATIV_BANC_2007A$fraction = COMP_ATIV_BANC_2007A$value/sum(COMP_ATIV_BANC_2007A$value)
COMP_ATIV_BANC_2007A$perc = percent(c(round(prop.table(COMP_ATIV_BANC_2007A$fraction), 3)))
COMP_ATIV_BANC_2007A$ymax = cumsum(COMP_ATIV_BANC_2007A$fraction)
COMP_ATIV_BANC_2007A$ymin = c(0, head(COMP_ATIV_BANC_2007A$ymax, n = -1))

COMP_ATIV_BANC_2011A$fraction = COMP_ATIV_BANC_2011A$value/sum(COMP_ATIV_BANC_2011A$value)
COMP_ATIV_BANC_2011A$perc = percent(c(round(prop.table(COMP_ATIV_BANC_2011A$fraction), 3)))
COMP_ATIV_BANC_2011A$ymax = cumsum(COMP_ATIV_BANC_2011A$fraction)
COMP_ATIV_BANC_2011A$ymin = c(0, head(COMP_ATIV_BANC_2011A$ymax, n = -1))

COMP_ATIV_BANC_2014A$fraction = COMP_ATIV_BANC_2014A$value/sum(COMP_ATIV_BANC_2014A$value)
COMP_ATIV_BANC_2014A$perc = percent(c(round(prop.table(COMP_ATIV_BANC_2014A$fraction), 3)))
COMP_ATIV_BANC_2014A$ymax = cumsum(COMP_ATIV_BANC_2014A$fraction)
COMP_ATIV_BANC_2014A$ymin = c(0, head(COMP_ATIV_BANC_2014A$ymax, n = -1))

# PLOTAR

ATV_BANC_2004 <- ggplot(COMP_ATIV_BANC_2004A, aes(fill = variable, ymax = ymax, ymin = ymin, xmax = 100, xmin = 70)) + geom_rect(colour = "black", show.legend = TRUE) + coord_polar(theta = "y") + xlim(c(0, 100))

ATV_BANC_2004 <- ATV_BANC_2004 + theme_minimal() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(),
                                                         panel.border = element_blank(), panel.grid = element_blank(),
                                                         legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=7.5, color="gray10"),legend.title = element_blank(),
                                                         plot.title = element_text(size=14, face="bold"))

ATV_BANC_2004_F <- ATV_BANC_2004 + geom_label_repel(aes(label = paste(perc,"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, size = 3.3) + annotate("text", x = 0, y = 0, size = 12, label = "2004")
ATV_BANC_2004_F

ATV_BANC_2007 <- ggplot(COMP_ATIV_BANC_2007A, aes(fill = variable, ymax = ymax, ymin = ymin, xmax = 100, xmin = 70)) + geom_rect(colour = "black", show.legend = TRUE) + coord_polar(theta = "y") + xlim(c(0, 100))

ATV_BANC_2007 <- ATV_BANC_2007 + theme_minimal() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(),
                                                         panel.border = element_blank(), panel.grid = element_blank(),
                                                         legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=7, color="gray10"),legend.title = element_blank(),
                                                         plot.title = element_text(size=14, face="bold"))

ATV_BANC_2007_F <- ATV_BANC_2007 + geom_label_repel(aes(label = paste(perc,"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, size = 3.3) + annotate("text", x = 0, y = 0, size = 12, label = "2007")
ATV_BANC_2007_F


ATV_BANC_2011 <- ggplot(COMP_ATIV_BANC_2011A, aes(fill = variable, ymax = ymax, ymin = ymin, xmax = 100, xmin = 70)) + geom_rect(colour = "grey30", show.legend = TRUE) + coord_polar(theta = "y") + xlim(c(0, 100))

ATV_BANC_2011 <- ATV_BANC_2011 + theme_minimal() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(),
                                                         panel.border = element_blank(), panel.grid = element_blank(),
                                                         legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=7, color="gray10"),legend.title = element_blank(),
                                                         plot.title = element_text(size=14, face="bold"))

ATV_BANC_2011_F <- ATV_BANC_2011 + geom_label_repel(aes(label = paste(perc,"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, size = 3.3) + annotate("text", x = 0, y = 0, size = 12, label = "2011")
ATV_BANC_2011_F


ATV_BANC_2014 <- ggplot(COMP_ATIV_BANC_2014A, aes(fill = variable, ymax = ymax, ymin = ymin, xmax = 100, xmin = 70)) + geom_rect(colour = "grey30", show.legend = TRUE) + coord_polar(theta = "y") + xlim(c(0, 100))

ATV_BANC_2014 <- ATV_BANC_2014 + theme_minimal() + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(),
                                                         panel.border = element_blank(), panel.grid = element_blank(),
                                                         legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=7, color="gray10"),legend.title = element_blank(),
                                                         plot.title = element_text(size=14, face="bold"))

ATV_BANC_2014_F <- ATV_BANC_2014 + geom_label_repel(aes(label = paste(perc,"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, size = 3.3) + annotate("text", x = 0, y = 0, size = 12, label = "2014")
ATV_BANC_2014_F

# JUNTAR E PLOTAR GRÁFICOS:

legend_ATV <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

A_legend_ATV<-legend_ATV(ATV_BANC_2007)

PLOT_ATV_BANC <- grid.arrange(arrangeGrob(ATV_BANC_2004_F + theme(legend.position="none") ,
                                          ATV_BANC_2007_F + theme(legend.position="none"),
                                          ATV_BANC_2011_F + theme(legend.position="none"),
                                          ATV_BANC_2014_F + theme(legend.position="none"), nrow=1),
                              A_legend_ATV, nrow = 2, heights=c(0.75, 0.25),
                              top = textGrob("Composição dos Ativos no Sistema Financeiro Chinês \n Anos Selecionados", gp=gpar(fontfamily = "Arial", fontface="bold", fontsize = 14)))

PLOT_ATV_BANC


# RELAÇÃO DOS DEPÓSITOS SOBRE ATIVOS

CORES_BANC <- c("EMPRÉSTIMOS" = "firebrick4", "DEPÓSITOS" = "steelblue4")

DEP_EMPR_BANC <- ggplot(DEP_EMPR_BANC4, aes(x = DATA, y = value, group = variable, colour = variable)) + geom_line(aes(linetype = variable),size = 0.75) + geom_point(shape=21, fill= "white", color= "black", size=2.2) + scale_x_date(limits = as.Date(c('2003-01-01','2014-01-01')),date_breaks ="1 years", date_labels = "%Y") + ggtitle("Depósitos e Empréstimos sobre Ativos Totais \n do Sistema Bancário Chinês (2003 - 2014)") + ylab(" % ") + theme_hc() + scale_colour_manual(values = CORES_BANC) + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))                                                                                                                                                                                                                                                                                                                      

DEP_EMPR_BANC

# RELAÇÃO DOS EMPRÉSTIMOS SOBRE DEPÓSITOS

RATIO_BANC <- ggplot(DEP_EMPR_RATIO1, aes(x = DATA, y = RATIO)) + geom_line(size = 0.75) + geom_point(shape=21, fill= "white", color= "black", size=2.2) + scale_x_date(limits = as.Date(c('2003-01-01','2014-01-01')),date_breaks ="1 years", date_labels = "%Y") + ggtitle("Depósitos sobre Empréstimos \n no Sistema Bancário Chinês (2003 - 2014)") + ylab(" % ") + theme_hc() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                      legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                      plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                      axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                      panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))                                                                                                                                                                                                                                                                                                                      

RATIO_BANC


######### EMISSÃO DE TÍTULOS (2004-2016) EM % ###################

CHINA_BONDS1 <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/Asian Bonds/CN-Issuance_Volume_USD.csv")

CHINA_BONDS2 <- data.frame(DATA = as.Date(CHINA_BONDS1$DATA, format = "%d/%m/%Y"),
                           GOV_BONDS = CHINA_BONDS1$Govt..in.USD.Billions.,
                           CORP_BONDS = CHINA_BONDS1$Corp..in.USD.Billions.)

options(scipen=100)
options(digits=2)
CHINA_BONDS2 <- rename(CHINA_BONDS2,
                       c(DATA = "DATA", 
                         GOV_BONDS = "TÍTULOS DO GOVERNO", 
                         CORP_BONDS = "TÍTULOS DE CORPORAÇÕES")) 

attach(CHINA_BONDS2)
CHINA_BONDS3 <- CHINA_BONDS2[which(DATA >= '2004-03-01'),]
detach(CHINA_BONDS2)

CHINA_BONDS4 <- melt(CHINA_BONDS3, id.vars = "DATA")

CHINA_BONDS5 <- data.frame(DATA = as.Date(CHINA_BONDS4$DATA, format = "%d/%m/%Y"),
                         value = CHINA_BONDS4$value,
                         variable = CHINA_BONDS4$variable)

# PLOTAR GRÁFICOS:

fill <- c("#5F9EA0", "#E1B378")

PLOT_BONDS <- ggplot(CHINA_BONDS5, aes(x = DATA, y = value, fill = variable)) + geom_area() + scale_x_date(limits = as.Date(c('2004-03-01','2016-09-01')), date_breaks ="1 years", date_labels = "%b-%Y") + ylab(" US$ 1BI. ") + theme_hc() + scale_fill_manual(values=fill) + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=10,color="black",vjust=0.50), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                     legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"), legend.title = element_blank(),
                                                                                                                                                                                                                                                                                     plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                     axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                     panel.grid.major = element_blank(), panel.grid.minor = element_blank())

PLOT_BONDS



######### EMISSÃO DE TÍTULOS NA CHINA (2003-2015) EM % ###################

CHINA_DMK1 <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/WFE/dmk.csv", sep = ';')

CHINA_DMK2 <- rename(CHINA_DMK1, c(DATA = "DATA", SHSE = "Shangai Stock Exchange", SZSE = "Shenzen Stock Exchange")) 



CHINA_DMK3 <- melt(CHINA_DMK2)

CHINA_DMK4 <- data.frame(DATA = as.Date(CHINA_DMK3$DATA, format = "%d/%m/%Y"),
                         value = CHINA_DMK3$value,
                         variable = CHINA_DMK3$variable)

# PLOTAR GRÁFICOS:

PLOT_DMK <- ggplot(CHINA_DMK4, aes(x = DATA, y = value, group = variable, color = variable)) + geom_line(aes(linetype = variable),size = 0.75) + scale_x_date(limits = as.Date(c('2003-01-01','2015-12-01')),date_breaks ="18 months", date_labels = "%b-%Y") + ylab(" Mi US$ ") + theme_hc() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                                                            legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                                                                                            plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                                                                                            axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                                                            panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))                                                                                                                                                                                                                                                                                                                       




PLOT_DMK




######### COMPOSIÇÃO DE NPLs (2003-2015) EM % ###################

CHINA_NLP1 <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/CBRC/CBRC_NPL.csv")

CHINA_NLP2 <- rename(CHINA_NLP1, c(Ano = "DATA", ABAIXO_PADRAO = "ABAIXO DO PADRÃO", DUVIDOS = "DUVIDOSO", PERDAS = "PERDAS")) 

CHINA_NLP3 <- melt(CHINA_NLP2)

CHINA_NLP4 <- data.frame(DATA = as.Date(CHINA_NLP3$DATA, format = "%d/%m/%Y"),
                         value = CHINA_NLP3$value,
                         variable = CHINA_NLP3$variable)

# PLOTAR GRÁFICOS:

PLOT_NPL <- ggplot(CHINA_NLP4, aes(x = DATA, y = value, fill = variable)) + geom_bar(stat = "identity",position='stack',width=100) + scale_x_date(date_breaks ="1 years", date_labels = "%Y") + ylab(" ¥100 MI. ") + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=10,color="black",vjust=0.50), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                          legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"), legend.title = element_blank(),
                                                                                                                                                                                                                                                                                                                                          plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                                          axis.ticks = element_blank(), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())

PLOT_NPL

