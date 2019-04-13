## TAMANHO DA ECONOMIA E INTEGRAÇÃO COM A ECONOMIA MUNDIAL ##

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
library(foreign)
font_import()
loadfonts(device="win") 
fonts()


####### TAXA DE CRESCIMENTO DOS COMPONENTES DO PIB CHINÊS (1997-2014) EM % #######

# ABRIR BASES E REALIZAR AS TRANSFORMAÇÕES: 

CH_PIB1 <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/NBS/PIB_1952_2014.csv")
COMP_PIB1 <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/NBS/COMPONENTES DO PIB.csv")


COMP_CH_PIB1 <- data.frame(DATA = as.Date(CH_PIB1$ANO, format = "%d/%m/%Y"),
                      PIB = CH_PIB1$PIB,
                      VA_INDUST_PRIMARIA = CH_PIB1$INDUST_PRIMARIA,
                      VA_INDUST_SECUNDARIA = CH_PIB1$INDUST_SECUNDARIA,
                      VA_INDUST_TERCIARIA = CH_PIB1$INDUST_TERCIARIA,
                      CONS_FAM = COMP_PIB1$CONS_FAM,
                      CONS_GOV = COMP_PIB1$GOV_CONS,
                      CAP_FOR = COMP_PIB1$CAP_BRU_FORM,
                      NX = COMP_PIB1$EXP_LIQ)



COMP_CH_PIB2 <- data.frame(DATA = COMP_CH_PIB1$DATA,
                        PIB = COMP_CH_PIB1$PIB,
                        VA_INDUST_PRIMARIA = CH_PIB1$INDUST_PRIMARIA,
                        VA_INDUST_SECUNDARIA = CH_PIB1$INDUST_SECUNDARIA,
                        VA_INDUST_TERCIARIA = CH_PIB1$INDUST_TERCIARIA,
                        CONS_FAM = COMP_CH_PIB1$CONS_FAM,
                        CONS_GOV = COMP_CH_PIB1$CONS_GOV,
                        CAP_FOR = COMP_CH_PIB1$CAP_FOR,
                        NX = COMP_CH_PIB1$NX,
                        PIB_CONS_FAM = (COMP_CH_PIB1$CONS_FAM)/(COMP_CH_PIB1$PIB),
                        PIB_CONS_GOV = (COMP_CH_PIB1$CONS_GOV)/(COMP_CH_PIB1$PIB),
                        PIB_CAP_FOR = (COMP_CH_PIB1$CAP_FOR)/(COMP_CH_PIB1$PIB),
                        PIB_NX = (COMP_CH_PIB1$NX)/(COMP_CH_PIB1$PIB))



TX_COMP_CH_PIB1 <- data.frame(DATA = COMP_CH_PIB2$DATA,
                        TX_CONS_FAM = c(NA,exp(diff(log(COMP_CH_PIB2$PIB_CONS_FAM)))-1),
                        TX_CONS_GOV = c(NA,exp(diff(log(COMP_CH_PIB2$PIB_CONS_GOV)))-1),
                        TX_CAP_FOR = c(NA,exp(diff(log(COMP_CH_PIB2$PIB_CAP_FOR)))-1),
                        TX_NX = c(NA,exp(diff(log(COMP_CH_PIB2$PIB_NX)))-1),
                        TX_PIB = c(NA,exp(diff(log(COMP_CH_PIB2$PIB)))-1))


TX_COMP_CH_PIB2 <- data.frame(DATA = TX_COMP_CH_PIB1$DATA,
                       TX_PIB_CONS_FAM = ((TX_COMP_CH_PIB1$TX_CONS_FAM)/(TX_COMP_CH_PIB1$TX_PIB))*100,
                       TX_PIB_CONS_GOV = ((TX_COMP_CH_PIB1$TX_CONS_GOV)/(TX_COMP_CH_PIB1$TX_PIB))*100,
                       TX_PIB_CAP_FOR = ((TX_COMP_CH_PIB1$TX_CAP_FOR)/(TX_COMP_CH_PIB1$TX_PIB))*100,
                       TX_PIB_NX = ((TX_COMP_CH_PIB1$TX_NX)/(TX_COMP_CH_PIB1$TX_PIB))*100)

options(scipen=100)
options(digits=2)
attach(TX_COMP_CH_PIB2)
TX_COMP_CH_PIB3 <- TX_COMP_CH_PIB2[which(DATA >= '1995-01-01'),]
detach(TX_COMP_CH_PIB2)

# PLOTAR GRÁFICOS:

CONS_FAM <- ggplot(TX_COMP_CH_PIB3, aes(x = DATA, y = TX_PIB_CONS_FAM)) + geom_line(size = 0.75, linetype = "solid") + geom_point(shape=21, fill= "white", color= "black", size=2.2) + geom_hline(yintercept = 0) + scale_x_date(limits = as.Date(c('1997-01-01','2014-01-01')),date_breaks ="1 years", date_labels = "%Y") + ylab("CRESC. CONS. FAMÍLIAS NO CRESC. PIB ( % )") + theme_hc() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                 legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                 plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                 axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.line.x = element_blank(),
                                                                                                                                                                                                                                                                                                                 panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))                                                                                                                                                                                                                                                                                                                      

CONS_GOV <- ggplot(TX_COMP_CH_PIB3, aes(x = DATA, y = TX_PIB_CONS_GOV)) + geom_line(size = 0.75, linetype = "solid") + geom_point(shape=21, fill= "white", color= "black", size=2.2)+ geom_hline(yintercept = 0) + scale_x_date(limits = as.Date(c('1997-01-01','2014-01-01')),date_breaks ="1 years", date_labels = "%Y") + ylab("CRESC. CONS. GOVERNO NO CRESC. PIB ( % )") + theme_hc() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.line.x = element_blank(),
                                                                                                                                                                                                                                                                                                                panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))

CAP_FOR <- ggplot(TX_COMP_CH_PIB3, aes(x = DATA, y = TX_PIB_CAP_FOR)) + geom_line(size = 0.75, linetype = "solid") + geom_point(shape=21, fill= "white", color= "black", size=2.2) + geom_hline(yintercept = 0) + scale_x_date(limits = as.Date(c('1997-01-01','2014-01-01')),date_breaks ="1 years", date_labels = "%Y") + ylab("CRESC. FORM. CAPITAL NO CRESC. PIB ( % )") + theme_hc() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                              legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                              plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                              axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5), axis.line.x = element_blank(),
                                                                                                                                                                                                                                                                                                              panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))

NX <- ggplot(TX_COMP_CH_PIB3, aes(x = DATA, y = TX_PIB_NX)) + geom_line(size = 0.75, linetype = "solid") + geom_point(shape=21, fill= "white", color= "black", size=2.2) + geom_hline(yintercept = 0) + scale_x_date(limits = as.Date(c('1997-01-01','2014-01-01')),date_breaks ="1 years", date_labels = "%Y") + ylab("CRESC. EXP. LÍQUIDAS NO CRESC. PIB ( % )") + theme_hc() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                    legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                    plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                    axis.ticks = element_blank(), axis.line.y = element_line(color = "black", size = 0.5), axis.line.x = element_blank(),
                                                                                                                                                                                                                                                                                                    panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))
PLOT_COMP_PIB <- grid.arrange(CONS_FAM, CONS_GOV, CAP_FOR, NX, ncol=2, nrow=2)

PLOT_COMP_PIB



########################## COMPOSIÇÃO EMPREGO ################################





########################## ANALISE COMPOSIÇÃO EMPRESAS ################################

# ABRIR BASES E REALIZAR AS TRANSFORMAÇÕES: 

CHINA_INFO_EMPR <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/NBS/INFO_EMPRESAS.csv")

aggr <- read.dta("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/z_Growing Like China/aggregates.dta")


CHINA_INFO_EMPR1 <- data.frame(DATA = as.Date(CHINA_INFO_EMPR$ANOS, format = "%d/%m/%Y"),
                             SOE_LUCROS = CHINA_INFO_EMPR$LUCROS_SOE,
                             SOE_ATIVOS = CHINA_INFO_EMPR$ATIVOS_FIXOS_SOE,
                             SOE_LUCROS_ATIVOS = CHINA_INFO_EMPR$LUCROS_SOE/CHINA_INFO_EMPR$ATIVOS_FIXOS_SOE,
                             PE_LUCROS = CHINA_INFO_EMPR$LUCROS_PE,
                             PE_ATIVOS = CHINA_INFO_EMPR$ATIVOS_FIXOS_PE,
                             PE_LUCROS_ATIVOS = CHINA_INFO_EMPR$LUCROS_PE/CHINA_INFO_EMPR$ATIVOS_FIXOS_PE,
                             FE_LUCROS = CHINA_INFO_EMPR$LUCROS_FE,
                             FE_ATIVOS = CHINA_INFO_EMPR$ATIVOS_FIXOS_FE,
                             FE_LUCROS_ATIVOS = CHINA_INFO_EMPR$LUCROS_FE/CHINA_INFO_EMPR$ATIVOS_FIXOS_FE)


options(scipen=100)
options(digits=2)
attach(CHINA_INFO_EMPR1)
CHINA_INFO_EMPR2 <- CHINA_INFO_EMPR1[which(DATA >= '2000-01-01'),]
detach(CHINA_INFO_EMPR1)
attach(CHINA_INFO_EMPR2)
CHINA_INFO_EMPR3 <- CHINA_INFO_EMPR2[which(DATA <= '2014-01-01'),]
detach(CHINA_INFO_EMPR2)

CHINA_RATIO1 <- data.frame(DATA = CHINA_INFO_EMPR3$DATA,
                           SOE_RATIO = CHINA_INFO_EMPR3$SOE_LUCROS_ATIVOS,
                           PE_RATIO = CHINA_INFO_EMPR3$PE_LUCROS_ATIVOS,
                           FE_RATIO = CHINA_INFO_EMPR3$FE_LUCROS_ATIVOS)

CHINA_RATIO2 <- rename(CHINA_RATIO1, c(DATA = "DATA", SOE_RATIO = "EMPRESAS ESTATAIS", PE_RATIO = "EMPRESAS PRIVADAS", FE_RATIO = "EMPRESAS ESTRANGEIRAS"))

CHINA_RATIO3 <- melt(CHINA_RATIO2,id=c("DATA"))

CHINA_RATIO4 <- data.frame(DATA = as.Date(CHINA_RATIO3$DATA, format = "%d/%m/%Y"),
                           variable = CHINA_RATIO3$variable,
                           value = CHINA_RATIO3$value*100)

INV_1 <- data.frame(DATA = aggr$year,
                    SOE_INV = aggr$inv_ef_soe,
                    PE_INV = aggr$inv_ef_dpe,
                    FE_INV = aggr$inv_ef_fe)

INV_1 <- rename(INV_1, c(DATA = "DATA", SOE_INV = "EMPRESAS ESTATAIS", PE_INV = "EMPRESAS PRIVADAS", FE_INV = "EMPRESAS ESTRANGEIRAS"))

attach(INV_1)
INV_2 <- INV_1[which(DATA >= '1997'),]
detach(INV_1)
attach(INV_2)
INV_3 <- INV_2[which(DATA <= '2003'),]
detach(INV_3)

INV_4 <- melt(INV_3, id.vars = "DATA")

INV_5 <- data.frame(DATA = ts(INV_4$DATA),
                    value = INV_4$value,
                    variable = INV_4$variable)

# PLOTAR GRÁFICOS:

CORES_RATIO <- c("EMPRESAS ESTATAIS" = "firebrick4", "EMPRESAS PRIVADAS" = "steelblue4", "EMPRESAS ESTRANGEIRAS" = "springgreen3")

CHN_RATIO1 <- ggplot(CHINA_RATIO4, aes(x = DATA, y = value, group = variable, colour = variable)) + geom_line(aes(linetype = variable),size = 0.75) + geom_point(shape=21,fill="white",size=2.2) + scale_x_date(limits = as.Date(c('2000-01-01','2014-01-01')),date_breaks ="1 years", date_labels = "%Y") + ylab(" % ") + theme_hc() + scale_colour_manual(values = CORES_RATIO) + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))                                                                                                                                                                                                                                                                                                                       

CHN_RATIO1


CHN_INV1 <- ggplot(INV_5, aes(x = DATA, y = value, group = variable, colour = variable)) + geom_line(aes(linetype = variable),size = 0.75) + scale_x_continuous(breaks = c(1997,1998,1999,2000,2001,2002,2003,2004)) + geom_point(shape=21,fill="white",size=2.2) + ylab(" % ") + theme_hc() + scale_colour_manual(values = CORES_RATIO) + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                 legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                                                 plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                                                 axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                 panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))                                                                                                                                                                                                                                                                                                                       

CHN_INV1


########################## BALANÇA COMERCIAL ######################################


# ABRIR BASES E REALIZAR AS TRANSFORMAÇÕES: 

CHINA_EXP_IMP1 <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/NBS/CHINA_IMPORTS_EXPORTS.csv")
PIB <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/NBS/PIB_1952_2014.csv")

EDIT_PIB1 <- data.frame(DATA = as.Date(PIB$ANO, format = "%d/%m/%Y"),
                       PIB = (PIB$PIB_R))
attach(EDIT_PIB1)
EDIT_PIB2 <- EDIT_PIB1[which(DATA >= '1980-01-01'),]
detach(EDIT_PIB1)

EDIT_PIB3 <- data.frame(DATA = as.Date(CHINA_EXP_IMP1$DATA, format = "%d/%m/%Y"),
                        PIB = EDIT_PIB2$PIB)

options(scipen=100)
options(digits=2)
EXP_IMP_FINAL1 <- data.frame(DATA = as.Date(CHINA_EXP_IMP1$DATA, format = "%d/%m/%Y"),
                             EXP_PIB = CHINA_EXP_IMP1$EXP/EDIT_PIB3$PIB,
                             IMP_PIB = CHINA_EXP_IMP1$IMP/EDIT_PIB3$PIB)

EXP_IMP_FINAL2 <- rename(EXP_IMP_FINAL1, c(DATA = "DATA", EXP_PIB = "EXPORTAÇÕES", IMP_PIB = "IMPORTAÇÕES"))

EXP_IMP_FINAL3 <- melt(EXP_IMP_FINAL2, id = c("DATA"), na.rm = TRUE)

EXP_IMP_FINAL4 <- data.frame(DATA = as.Date(EXP_IMP_FINAL3$DATA, format = "%d/%m/%Y"),
                             value = EXP_IMP_FINAL3$value*100,
                             variable = EXP_IMP_FINAL3$variable)



CHINA_CONTA_CORR1 <- read.csv("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/SAFE/CONTA_CORRENTE.csv")
DATA <- as.Date(CHINA_CONTA_CORR$ANOS, format = "%d/%m/%Y")

CHINA_CONTA_CORR2 <- data.frame(DATA = as.Date(CHINA_CONTA_CORR1$ANOS, format = "%d/%m/%Y"),
                        BS_TOTAL = ts(CHINA_CONTA_CORR1$BENS_SERVIÇOS_TOTAL),        
                        BS_ATIVO = ts(CHINA_CONTA_CORR1$BENS_SERVIÇOS_ATIVOS),
                        BS_PASSIVO = ts(CHINA_CONTA_CORR1$BENS_SERVIÇOS_PASSIVOS),
                        CC_TOTAL = ts(CHINA_CONTA_CORR1$CONTA_CORRENTE_TOTAL),
                        CC_ATIVO = ts(CHINA_CONTA_CORR1$CONTA_CORRENTE_ATIVOS),
                        CC_ATIVO = ts(CHINA_CONTA_CORR1$CONTA_CORRENTE_PASSIVOS))



# PLOTAR GRÁFICOS:

CORES_BAL_COMER <- c("EXPORTAÇÕES" = "tomato4", "IMPORTAÇÕES" = "paleturquoise4")
LINHAS_BAL_COMER <- c("EXPORTAÇÕES" = "dashed", "IMPORTAÇÕES" = "solid")


PLOT_BAL_COMERC1 <- ggplot(EXP_IMP_FINAL4, aes(x = DATA, y = value, group = variable, colour = variable)) + geom_line(aes(linetype = variable),size=0.75) + geom_point(shape=21,fill="white",size=2.2) + scale_x_date(limits = as.Date(c('1997-01-01','2014-01-01')),date_breaks ="1 years", date_labels = "%Y") + ylab(" % ") + theme_hc() + scale_color_manual(values = c(CORES_BAL_COMER)) + scale_linetype_manual(values = c(LINHAS_BAL_COMER)) + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9,color="black"), axis.text.y = element_text(size=9,color="black"), axis.title.x = element_blank(), axis.title.y = element_text(size=10,color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    legend.position = c("bottom"), legend.direction = "horizontal", legend.title = element_blank(), legend.text = element_text(size=10, color="black"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    plot.title = element_text(hjust = 0.5, size = 14),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    axis.ticks = element_blank(), axis.line = element_line(color = "black", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    panel.grid.major = element_blank(), panel.grid.minor = element_line(size=0.2,linetype="solid",color="grey90"))                                                                                                                                                                                                                                                                                                                       


PLOT_BAL_COMERC1

 

