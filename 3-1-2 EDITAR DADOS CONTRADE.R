### BAIXAR PACOTES ###

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

### BAIXAR ARQUIVOS ###

setwd("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/UN-COMTRADE")

LISTA_DADOS <- list.files(path ="C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/UN-COMTRADE", pattern = ".csv", all.files = TRUE, full.names = TRUE)

for(f in 1:length(LISTA_DADOS)) 
  {file_name <- str_sub(string = LISTA_DADOS[f], start = 46, end = -5)
  file_df <- read_csv(LISTA_DADOS[f])  
  assign(x = file_name, value = file_df, envir = .GlobalEnv)}

### LIMPAR VARIÁVEIS ##



### DADOS AFRICA ###

AFRICA1 <- rbind(`DADOS/UN-COMTRADE/AFRICA1_2001_2005`,
                `DADOS/UN-COMTRADE/AFRICA1_2006_2010`,
                `DADOS/UN-COMTRADE/AFRICA1_2011_2015`,
                `DADOS/UN-COMTRADE/AFRICA2_2001_2005`,
                `DADOS/UN-COMTRADE/AFRICA2_2006_2010`,
                `DADOS/UN-COMTRADE/AFRICA2_2011_2015`)

afr_trade_value = (AFRICA1$`Trade Value (US$)`/10e+7)

AFRICA2 <- data.frame(Year = AFRICA1$Year,
                      Partner = AFRICA1$Partner,
                      Trade_Flow = revalue(AFRICA1$`Trade Flow`, c("Import"="Importação", 
                                                                 "Export"="Exportação")),
                      Commodity_Code = AFRICA1$`Commodity Code`,
                      Commodity = revalue(AFRICA1$Commodity, c("Food and beverages"="Alimentos e Bebidas",
                                                               "Industrial supplies nes"="Suprimentos industriais",
                                                               "Fuels and lubricants"="Combustiveis e Lubrificantes",
                                                               "Capital goods (except transport equipment), and parts and accessories thereof"="Bens de capital(partes e acessórios) - excepto equipamento de transporte",
                                                               "Transport equipment, and parts and accessories thereof"="Equipamento de transporte (partes e acessórios)",
                                                               "Consumption goods nes"="Bens de Consumo",
                                                               "Goods nes"="Bens")),
                      Trade_Value = afr_trade_value)

index <- AFRICA2$Trade_Flow == "Importação"
AFRICA2$Trade_Value[index] = -abs(AFRICA2$Trade_Value[index])

attach(AFRICA2)
AFRICA3 <- AFRICA2[order(Year,Partner,Trade_Flow,Commodity_Code),]
detach(AFRICA2)


### DADOS ASEAN+3 ###

ASEAN1 <- rbind(`DADOS/UN-COMTRADE/CHINA_BRUNEI_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_BRUNEI_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_BRUNEI_2011_2015`,
                 `DADOS/UN-COMTRADE/CHINA_CAMBOJA_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_CAMBOJA_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_CAMBOJA_2011_2015`,
                 `DADOS/UN-COMTRADE/CHINA_COREIA_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_COREIA_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_COREIA_2011_2015`,
                 `DADOS/UN-COMTRADE/CHINA_FILIPINAS_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_FILIPINAS_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_FILIPINAS_2011_2015`,
                 `DADOS/UN-COMTRADE/CHINA_INDONESIA_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_INDONESIA_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_INDONESIA_2011_2015`,
                 `DADOS/UN-COMTRADE/CHINA_JAPAO_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_JAPAO_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_JAPAO_2011_2015`,
                 `DADOS/UN-COMTRADE/CHINA_LAOS_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_LAOS_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_LAOS_2011_2015`,
                 `DADOS/UN-COMTRADE/CHINA_MALASIA_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_MALASIA_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_MALASIA_2011_2015`,
                 `DADOS/UN-COMTRADE/CHINA_MIANMAR_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_MIANMAR_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_MIANMAR_2011_2015`,
                 `DADOS/UN-COMTRADE/CHINA_SINGAPURA_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_SINGAPURA_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_SINGAPURA_2011_2015`,
                 `DADOS/UN-COMTRADE/CHINA_TAILANDIA_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_TAILANDIA_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_TAILANDIA_2011_2015`,
                 `DADOS/UN-COMTRADE/CHINA_VIETNA_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_TAILANDIA_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_TAILANDIA_2011_2015`)

asn_trade_value = (ASEAN1$`Trade Value (US$)`/10e+7)

ASEAN2 <- data.frame(Year = ASEAN1$Year,
                      Partner = ASEAN1$Partner,
                      Trade_Flow = revalue(ASEAN1$`Trade Flow`, c("Import"="Importação", 
                                                                  "Export"="Exportação")),
                      Commodity_Code = ASEAN1$`Commodity Code`,
                      Commodity = revalue(ASEAN1$Commodity, c("Food and beverages"="Alimentos e Bebidas",
                                                      "Industrial supplies nes"="Suprimentos industriais",
                                                      "Fuels and lubricants"="Combustiveis e Lubrificantes",
                                                      "Capital goods (except transport equipment), and parts and accessories thereof"="Bens de capital(partes e acessórios) - excepto equipamento de transporte",
                                                      "Transport equipment, and parts and accessories thereof"="Equipamento de transporte (partes e acessórios)",
                                                      "Consumption goods nes"="Bens de Consumo",
                                                      "Goods nes"="Bens")),
                      Trade_Value = asn_trade_value)

index <- ASEAN2$Trade_Flow == "Importação"
ASEAN2$Trade_Value[index] = -abs(ASEAN2$Trade_Value[index])

attach(ASEAN2)
ASEAN3 <- ASEAN2[order(Year,Partner,Trade_Flow,Commodity_Code),]
detach(ASEAN2)


### DADOS BRICS ###

BRICS1 <- rbind(`DADOS/UN-COMTRADE/CHINA_BRICS_2001_2005`,
                 `DADOS/UN-COMTRADE/CHINA_BRICS_2006_2010`,
                 `DADOS/UN-COMTRADE/CHINA_BRICS_2011_2015`)


BRICS2 <- data.frame(Year = BRICS1$Year,
                      Partner = BRICS1$Partner,
                      Trade_Flow = revalue(BRICS1$`Trade Flow`, c("Import"="Importação", 
                                                                 "Export"="Exportação")),
                      Commodity_Code = BRICS1$`Commodity Code`,
                      Commodity = revalue(BRICS1$Commodity, c("Food and beverages"="Alimentos e Bebidas",
                                                      "Industrial supplies nes"="Suprimentos industriais",
                                                      "Fuels and lubricants"="Combustiveis e Lubrificantes",
                                                      "Capital goods (except transport equipment), and parts and accessories thereof"="Bens de capital(partes e acessórios) - excepto equipamento de transporte",
                                                      "Transport equipment, and parts and accessories thereof"="Equipamento de transporte (partes e acessórios)",
                                                      "Consumption goods nes"="Bens de Consumo",
                                                      "Goods nes"="Bens")),
                      Trade_Value = BRICS1$`Trade Value (US$)`)

index <- BRICS2$Trade_Flow == "Importação"
BRICS2$Trade_Value[index] = -abs(BRICS2$Trade_Value[index])

attach(BRICS2)
BRICS3 <- BRICS2[order(Year,Partner,Trade_Flow,Commodity_Code),]
detach(BRICS2)


### DADOS EUROZONE ###

EURO1 <- rbind(`DADOS/UN-COMTRADE/EU1_2001_2005`,
                `DADOS/UN-COMTRADE/EU1_2006_2010`,
                `DADOS/UN-COMTRADE/EU1_2011_2015`,
                `DADOS/UN-COMTRADE/EU2_2001_2005`,
                `DADOS/UN-COMTRADE/EU2_2006_2010`,
                `DADOS/UN-COMTRADE/EU2_2011_2015`,
                `DADOS/UN-COMTRADE/EU3_2001_2005`,
                `DADOS/UN-COMTRADE/EU3_2006_2010`,
                `DADOS/UN-COMTRADE/EU3_2011_2015`,
                `DADOS/UN-COMTRADE/EU4_2001_2005`,
                `DADOS/UN-COMTRADE/EU4_2006_2010`,
                `DADOS/UN-COMTRADE/EU4_2011_2015`)

euro_trade_value = (EURO1$`Trade Value (US$)`/10e+7)

EURO2 <- data.frame(Year = EURO1$Year,
                     Partner = EURO1$Partner,
                     Trade_Flow = revalue(EURO1$`Trade Flow`, c("Import"="Importação", 
                                                                "Export"="Exportação")),
                     Commodity_Code = EURO1$`Commodity Code`,
                     Commodity = revalue(EURO1$Commodity, c("Food and beverages"="Alimentos e Bebidas",
                                                     "Industrial supplies nes"="Suprimentos industriais",
                                                     "Fuels and lubricants"="Combustiveis e Lubrificantes",
                                                     "Capital goods (except transport equipment), and parts and accessories thereof"="Bens de capital(partes e acessórios) - excepto equipamento de transporte",
                                                     "Transport equipment, and parts and accessories thereof"="Equipamento de transporte (partes e acessórios)",
                                                     "Consumption goods nes"="Bens de Consumo",
                                                     "Goods nes"="Bens")),
                     Trade_Value = euro_trade_value)

index <- EURO2$Trade_Flow == "Importação"
EURO2$Trade_Value[index] = -abs(EURO2$Trade_Value[index])

attach(EURO2)
EURO3 <- EURO2[order(Year,Partner,Trade_Flow,Commodity_Code),]
detach(EURO2)

### DADOS EUA ###

EUA1 <- rbind(`DADOS/UN-COMTRADE/CHINA_EUA_2001_2005`,
                `DADOS/UN-COMTRADE/CHINA_EUA_2006_2010`,
                `DADOS/UN-COMTRADE/CHINA_EUA_2011_2015`)

eua_trade_value = (EUA1$`Trade Value (US$)`/10e+7)


EUA2 <- data.frame(Year = EUA1$Year,
                     Partner = EUA1$Partner,
                     Trade_Flow = revalue(EUA1$`Trade Flow`, c("Import"="Importação", 
                                                              "Export"="Exportação")),
                     Commodity_Code = EUA1$`Commodity Code`,
                     Commodity = revalue(EUA1$Commodity, c("Food and beverages"="Alimentos e Bebidas",
                                                   "Industrial supplies nes"="Suprimentos industriais",
                                                   "Fuels and lubricants"="Combustiveis e Lubrificantes",
                                                   "Capital goods (except transport equipment), and parts and accessories thereof"="Bens de capital (partes e acessórios) - exceto equipamento de transporte",
                                                   "Transport equipment, and parts and accessories thereof"="Equipamento de transporte (partes e acessórios)",
                                                   "Consumption goods nes"="Bens de Consumo",
                                                   "Goods nes"="Bens")),
                     Trade_Value = eua_trade_value)

index <- EUA2$Trade_Flow == "Importação"
EUA2$Trade_Value[index] = -abs(EUA2$Trade_Value[index])
transf <- EUA2$Trade_Value 

attach(EUA2)
EUA3 <- EUA2[order(Year,Partner,Trade_Flow,Commodity_Code),]
detach(EUA2)

######################################################################################

# "COMÉRCIO DE BENS - CHINA E PARCEIROS EM 100 MI US$" #

CHN_AFRICA1 <- ggplot(AFRICA3, aes(x = Year, y = Trade_Value, fill = Trade_Flow)) + geom_bar(stat = "identity", position=position_dodge(), width=0.5) + geom_hline(yintercept = 0) + scale_x_continuous(breaks = c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)) + ylim(-400,400) + ggtitle("AFRICA") + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9), axis.text.y = element_text(size=9), axis.title.x = element_blank(), axis.title.y = element_text(size=12),
                                                                                                                                                                                                                                                                                                                                                                legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"),
                                                                                                                                                                                                                                                                                                                                                                plot.title = element_text(hjust = 0.5, size = 12),
                                                                                                                                                                                                                                                                                                                                                                axis.ticks = element_blank(), axis.line.x = element_line(color = "gray40", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                                panel.grid.major = element_blank()) 

CHN_AFRICA2 <- CHN_AFRICA1 + theme(axis.title.y = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white', colour = 'white'))
CHN_AFRICA2


CHN_ASEAN1 <- ggplot(ASEAN3, aes(x = Year, y = Trade_Value, fill = Trade_Flow)) + geom_bar(stat = "identity", position=position_dodge(), width=0.5) + geom_hline(yintercept = 0) + scale_x_continuous(breaks = c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)) + ylim(-1000,1000) + ggtitle("ASEAN") + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9), axis.text.y = element_text(size=9), axis.title.x = element_blank(), axis.title.y = element_text(size=12),
                                                                                                                                                                                                                                                                                                                                                             legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"),
                                                                                                                                                                                                                                                                                                                                                             plot.title = element_text(hjust = 0.5, size = 12),
                                                                                                                                                                                                                                                                                                                                                             axis.ticks = element_blank(), axis.line.x = element_line(color = "gray40", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                             panel.grid.major = element_blank()) 


CHN_ASEAN2 <- CHN_ASEAN1 + theme(axis.title.y = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white', colour = 'white'))
CHN_ASEAN2


CHN_EURO1 <- ggplot(EURO3, aes(x = Year, y = Trade_Value, fill = Trade_Flow)) + geom_bar(stat = "identity", position=position_dodge(), width=0.5) + geom_hline(yintercept = 0) + scale_x_continuous(breaks = c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)) + ylim(-400,400) + ggtitle("ZONA DO EURO") + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9), axis.text.y = element_text(size=9), axis.title.x = element_blank(), axis.title.y = element_text(size=12),
                                                                                                                                                                                                                                                                                                                                                                legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"),
                                                                                                                                                                                                                                                                                                                                                                plot.title = element_text(hjust = 0.5, size = 12),
                                                                                                                                                                                                                                                                                                                                                                axis.ticks = element_blank(), axis.line.x = element_line(color = "gray40", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                                panel.grid.major = element_blank()) 


CHN_EURO2 <- CHN_EURO1 + theme(axis.title.y = element_blank(),panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white', colour = 'white'))
CHN_EURO2


CHN_EUA1 <- ggplot(EUA3, aes(x = Year, y = Trade_Value, fill = Trade_Flow)) + geom_bar(stat = "identity", position=position_dodge(), width=0.5) + geom_hline(yintercept = 0) + scale_x_continuous(breaks = c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)) + ylim(-2000,2000) + ggtitle("EUA") + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9), axis.text.y = element_text(size=9), axis.title.x = element_blank(), axis.title.y = element_text(size=12),
                                                                                                                                                                                                                                                                                                                                                         legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"),
                                                                                                                                                                                                                                                                                                                                                         plot.title = element_text(hjust = 0.5, size = 12),
                                                                                                                                                                                                                                                                                                                                                         axis.ticks = element_blank(), axis.line.x = element_line(color = "gray40", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                         panel.grid.major = element_blank()) 


CHN_EUA2 <- CHN_EUA1 + theme(axis.title.y = element_blank(), panel.grid.minor = element_blank(),  panel.background = element_rect(fill = 'white', colour = 'white'), legend.title = element_blank())
CHN_EUA2

######################################################################################

# JUNTAR E PLOTAR GRÁFICOS:

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(CHN_EUA2)

PLOT_IMP_EXP <- grid.arrange(arrangeGrob(CHN_EUA2 + theme(legend.position="none"),
                                      CHN_EURO2 + theme(legend.position="none"),
                                      CHN_ASEAN2 + theme(legend.position="none"),
                                      CHN_AFRICA2 + theme(legend.position="none"), nrow=2),
                          mylegend, nrow = 2, heights=c(0.95, 0.05))

PLOT_IMP_EXP




######################################################################################

# "TIPOS DE PRODUTOS - CHINA E PARCEIROS EM 100 MI US$" #

# "Alimentos e Bebidas"
"Suprimentos industriais"
"Combustiveis e Lubrificantes"
"Bens de capital(partes e acessórios)(exceto equipamento de transporte)"
"Equipamento de transporte (partes e acessórios)"
"Bens de Consumo"
"Bens" #

CHN_AFRICA_C1 <- ggplot(AFRICA3, aes(x = Year, y = Trade_Value, fill = Commodity)) + geom_bar(stat = "identity", position=position_dodge(), width=0.5) + geom_hline(yintercept = 0) + scale_x_continuous(breaks = c(2001,2005,2010,2015)) + ggtitle("AFRICA") + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9), axis.text.y = element_text(size=9), axis.title.x = element_blank(), axis.title.y = element_text(size=12),
                                                                                                                                                                                                                                                                                                                                                               legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"),
                                                                                                                                                                                                                                                                                                                                                               plot.title = element_text(hjust = 0.5, size = 12),
                                                                                                                                                                                                                                                                                                                                                               axis.ticks = element_blank(), axis.line.x = element_line(color = "gray40", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                               panel.grid.major = element_blank()) 

CHN_AFRICA_C2 <- CHN_AFRICA_C1 + theme(axis.title.y = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white', colour = 'white'))
CHN_AFRICA_C2


CHN_ASEAN_C1 <- ggplot(ASEAN3, aes(x = Year, y = Trade_Value, fill = Commodity)) + geom_bar(stat = "identity", position=position_dodge(), width=0.5) + geom_hline(yintercept = 0) + scale_x_continuous(breaks = c(2001,2005,2010,2015)) + ggtitle("ASEAN") + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9), axis.text.y = element_text(size=9), axis.title.x = element_blank(), axis.title.y = element_text(size=12),
                                                                                                                                                                                                                                                                                                                                                             legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"),
                                                                                                                                                                                                                                                                                                                                                             plot.title = element_text(hjust = 0.5, size = 12),
                                                                                                                                                                                                                                                                                                                                                             axis.ticks = element_blank(), axis.line.x = element_line(color = "gray40", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                             panel.grid.major = element_blank()) 

CHN_ASEAN_C2<- CHN_ASEAN_C1 + theme(axis.title.y = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white', colour = 'white'))
CHN_ASEAN_C2



CHN_EURO_C1 <- ggplot(EURO3, aes(x = Year, y = Trade_Value, fill = Commodity)) + geom_bar(stat = "identity", position=position_dodge(), width=0.5) + geom_hline(yintercept = 0) + scale_x_continuous(breaks = c(2001,2005,2010,2015)) + ggtitle("ZONA DO EURO") + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9), axis.text.y = element_text(size=9), axis.title.x = element_blank(), axis.title.y = element_text(size=12),
                                                                                                                                                                                                                                                                                                                                                                 legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"),
                                                                                                                                                                                                                                                                                                                                                                 plot.title = element_text(hjust = 0.5, size = 12),
                                                                                                                                                                                                                                                                                                                                                                 axis.ticks = element_blank(), axis.line.x = element_line(color = "gray40", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                                 panel.grid.major = element_blank()) 


CHN_EURO_C2 <- CHN_EURO_C1 + theme(axis.title.y = element_blank(),panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'white', colour = 'white'))
CHN_EURO_C2


CHN_EUA_C1 <- ggplot(EUA3, aes(x = Year, y = Trade_Value, fill = Commodity)) + geom_bar(stat = "identity", position=position_dodge(), width=0.5) + geom_hline(yintercept = 0) + scale_x_continuous(breaks = c(2001,2005,2010,2015)) + ggtitle("EUA") + theme_minimal() + theme(text = element_text(family = "Arial"), axis.text.x = element_text(size=9), axis.text.y = element_text(size=9), axis.title.x = element_blank(), axis.title.y = element_text(size=12),
                                                                                                                                                                                                                                                                                                                                                        legend.position = c("bottom"), legend.direction = "horizontal", legend.text = element_text(size=10, color="gray10"),
                                                                                                                                                                                                                                                                                                                                                        plot.title = element_text(hjust = 0.5, size = 12),
                                                                                                                                                                                                                                                                                                                                                        axis.ticks = element_blank(), axis.line.x = element_line(color = "gray40", size = 0.5),
                                                                                                                                                                                                                                                                                                                                                        panel.grid.major = element_blank()) 


CHN_EUA_C2 <- CHN_EUA_C1 + theme(axis.title.y = element_blank(), panel.grid.minor = element_blank(),  panel.background = element_rect(fill = 'white', colour = 'white'), legend.title = element_blank())
CHN_EUA_C2

######################################################################################

# JUNTAR E PLOTAR GRÁFICOS:

b_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

minelegend <- b_legend(CHN_EUA_C2)

PLOT_PROD <- grid.arrange(arrangeGrob(CHN_EUA_C2 + theme(legend.position="none") ,
                                    CHN_EURO_C2 + theme(legend.position="none"),
                                    CHN_ASEAN_C2+ theme(legend.position="none"),
                                    CHN_AFRICA_C2 + theme(legend.position="none"), nrow=4),
                        minelegend, nrow = 2, heights=c(0.92, 0.08))
                        
PLOT_PROD