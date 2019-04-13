### BAIXAR PACOTES ###

library(pastecs)
library(moments)
library(readr)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggthemes)
library(RColorBrewer)
library(extrafont)
library(ggrepel)
library(reshape)
library(reshape2)
font_import()
loadfonts(device="win") 
fonts()

### BAIXAR ARQUIVOS ###

setwd("C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/Wind")

LISTA_DADOS <- list.files(path ="C:/Users/Gustavo Bruschi/Downloads/TCC/TCC - DADOS/Wind", pattern = ".csv", all.files = TRUE, full.names = TRUE)

for(f in 1:length(LISTA_DADOS)) 
{file_name <- str_sub(string = LISTA_DADOS[f], start = 46, end = -5)
file_df <- read_csv(LISTA_DADOS[f])  
assign(x = file_name, value = file_df, envir = .GlobalEnv)}

### LIMPAR VARIÁVEIS ##

DADOS_AGREGADOS1 <- rbind(`DADOS/Wind/FI_LOANS_DEPOSITS`)

DADOS_AGREGADOS2 <- data.frame(DATA = as.Date(DADOS_AGREGADOS1$DATA, format = "%d/%m/%Y"),
                               TOTAL_LOANS = (DADOS_AGREGADOS1$`FIs: Total Loans`)+(DADOS_AGREGADOS1$`FIs: Foreign Currency Loans`),
                               TOTAL_DEPS = (DADOS_AGREGADOS1$`FIs: Total Deposits`)+(DADOS_AGREGADOS1$`FIs: Foreign Currency Deposits`))


DADOS_LOANS_AGR1 <- rbind(`DADOS/Wind/LOANS_AGREGADOS`)

DADOS_LOANS_AGR2 <- data.frame(DATA = as.Date(DADOS_LOANS_AGR1$DATA, format = "%d/%m/%Y"),
                               SMLs = DADOS_LOANS_AGR1$SMLs,
                               NPLs = DADOS_LOANS_AGR1$NPLs,
                               NL = DADOS_LOANS_AGR1$`Normal Loans`,
                               SMLs_NPLs_RATIO = (DADOS_LOANS_AGR1$SMLs)+(DADOS_LOANS_AGR1$NPLs)/(DADOS_LOANS_AGR1$`Normal Loans`))


DADOS_BANCOS1 <- rbind(`DADOS/Wind/BANKS Non-performing Loans`,
                    `DADOS/Wind/BANKS Total Deposits`,
                    `DADOS/Wind/BANKS Total Loans`)

DADOS_BANCOS2 <- data.frame(FLAG = DADOS_BANCOS1$FLAG, 
                           DATA = DADOS_BANCOS1$DATA,
                           Beijing = DADOS_BANCOS1$`Bank of Beijing`,
                           ChongqingRural = DADOS_BANCOS1$`Chongqing Rural Commercial Bank`,
                           HuaXia = DADOS_BANCOS1$`Hua Xia Bank`,
                           BoCommu = DADOS_BANCOS1$`Bank of Communications`,
                           Nanjing = DADOS_BANCOS1$`Bank of Nanjing`,
                           Ningbo = DADOS_BANCOS1$`Bank of Ningbo`,
                           PingAn = DADOS_BANCOS1$`Ping An Bank`,
                           Wuxi = DADOS_BANCOS1$`Wuxi Rural Commercial Bank`,
                           ShangaiDev = DADOS_BANCOS1$`Shanghai Pudong Development Bank`,
                           Industrial = DADOS_BANCOS1$`Industrial Bank`,
                           Merchants = DADOS_BANCOS1$`China Merchants Bank`,
                           IndustComm = DADOS_BANCOS1$`Industrial and Commercial Bank of China`,
                           Everbright = DADOS_BANCOS1$`China Everbright Bank`,
                           Construction = DADOS_BANCOS1$`China Construction Bank`,
                           Minsheng = DADOS_BANCOS1$`China Minsheng Banking`,
                           CITIC = DADOS_BANCOS1$`China CITIC Bank`,
                           BOC = DADOS_BANCOS1$`Bank of China`,
                           AgrBank = DADOS_BANCOS1$`Agricultural Bank of China`)

### DADOS AGREGADOS ###   

attach(DADOS_AGREGADOS2)
DADOS_AGR2013 <- rbind(DADOS_AGREGADOS2[which(DATA == '2013-10-01'),],
                DADOS_AGREGADOS2[which(DATA == '2013-11-01'),],
                DADOS_AGREGADOS2[which(DATA == '2013-12-01'),])
detach(DADOS_AGREGADOS2)

attach(DADOS_AGREGADOS2)
DADOS_AGR2014 <- rbind(DADOS_AGREGADOS2[which(DATA == '2014-10-01'),],
                       DADOS_AGREGADOS2[which(DATA == '2014-11-01'),],
                       DADOS_AGREGADOS2[which(DATA == '2014-12-01'),])
detach(DADOS_AGREGADOS2)


attach(DADOS_AGREGADOS2)
DADOS_AGR2015 <- rbind(DADOS_AGREGADOS2[which(DATA == '2015-10-01'),],
                       DADOS_AGREGADOS2[which(DATA == '2015-11-01'),],
                       DADOS_AGREGADOS2[which(DATA == '2015-12-01'),])
detach(DADOS_AGREGADOS2)

DADOS_AGR2013 <- data.frame(DATA = DADOS_AGR2013$DATA,
                            TOTAL_LOANS = sum(DADOS_AGR2013$TOTAL_LOANS),
                            TOTAL_DEPS = sum(DADOS_AGR2013$TOTAL_DEPS))
DADOS_AGR2014 <- data.frame(DATA = DADOS_AGR2014$DATA,
                            TOTAL_LOANS = sum(DADOS_AGR2014$TOTAL_LOANS),
                            TOTAL_DEPS = sum(DADOS_AGR2014$TOTAL_DEPS))
DADOS_AGR2015 <- data.frame(DATA = DADOS_AGR2015$DATA,
                            TOTAL_LOANS = sum(DADOS_AGR2015$TOTAL_LOANS),
                            TOTAL_DEPS = sum(DADOS_AGR2015$TOTAL_DEPS))

DADOS_AGR1 <- rbind(DADOS_AGR2013,
                   DADOS_AGR2014,
                   DADOS_AGR2015)


### DADOS NPL ###                   
                           
attach(DADOS_BANCOS2)
DADOS_NPL2013 <- DADOS_BANCOS2[which(DATA == '01/12/2013' & FLAG == 'NPL'),]
detach(DADOS_BANCOS2)

attach(DADOS_BANCOS2)
DADOS_NPL2014 <- DADOS_BANCOS2[which(DATA == '01/12/2014' & FLAG == 'NPL'),]
detach(DADOS_BANCOS2)

attach(DADOS_BANCOS2)
DADOS_NPL2015 <- DADOS_BANCOS2[which(DATA == '01/12/2015' & FLAG == 'NPL'),]
detach(DADOS_BANCOS2)

DATA_NPL1 <- rbind(DADOS_NPL2013,
                  DADOS_NPL2014,
                  DADOS_NPL2015)
DATA_NPL2 <- data.frame(DATA = DATA_NPL1$DATA,
                        Beijing = DATA_NPL1$Beijing,
                        ChongqingRural = DATA_NPL1$ChongqingRural,
                        HuaXia = DATA_NPL1$HuaXia,
                        BoCommu = DATA_NPL1$BoCommu,
                        Nanjing = DATA_NPL1$Nanjing,
                        Ningbo = DATA_NPL1$Ningbo,
                        PingAn = DATA_NPL1$PingAn,
                        Wuxi = DATA_NPL1$Wuxi,
                        ShangaiDev = DATA_NPL1$ShangaiDev,
                        Industrial = DATA_NPL1$Industrial,
                        Merchants = DATA_NPL1$Merchants,
                        IndustComm = DATA_NPL1$IndustComm,
                        Everbright = DATA_NPL1$Everbright,
                        Construction = DATA_NPL1$Construction,
                        Minsheng = DATA_NPL1$Minsheng,
                        CITIC = DATA_NPL1$CITIC,
                        BOC = DATA_NPL1$BOC,
                        AgrBank = DATA_NPL1$AgrBank)

DATA_NPL3 <- melt(DATA_NPL2)
DATA_NPL4 <- data.frame(DATA = as.Date(DATA_NPL3$DATA, format = "%d/%m/%Y"),
                        value_NPL = DATA_NPL3$value,
                        variable = DATA_NPL3$variable)

### DADOS DEPOSTIS ###
  
attach(DADOS_BANCOS2)
DADOS_DEP2013 <- DADOS_BANCOS2[which(DATA == '01/12/2013' & FLAG == 'DEPOSIT'),]
detach(DADOS_BANCOS2)

attach(DADOS_BANCOS2)
DADOS_DEP2014 <- DADOS_BANCOS2[which(DATA == '01/12/2014' & FLAG == 'DEPOSIT'),]
detach(DADOS_BANCOS2)

attach(DADOS_BANCOS2)
DADOS_DEP2015 <- DADOS_BANCOS2[which(DATA == '01/12/2015' & FLAG == 'DEPOSIT'),]
detach(DADOS_BANCOS2)

DATA_DEP1 <- rbind(DADOS_DEP2013,
                   DADOS_DEP2014,
                   DADOS_DEP2015)
DATA_DEP2 <- data.frame(DATA = DATA_DEP1$DATA,
                        Beijing = DATA_DEP1$Beijing,
                        ChongqingRural = DATA_DEP1$ChongqingRural,
                        HuaXia = DATA_DEP1$HuaXia,
                        BoCommu = DATA_DEP1$BoCommu,
                        Nanjing = DATA_DEP1$Nanjing,
                        Ningbo = DATA_DEP1$Ningbo,
                        PingAn = DATA_DEP1$PingAn,
                        Wuxi = DATA_DEP1$Wuxi,
                        ShangaiDev = DATA_DEP1$ShangaiDev,
                        Industrial = DATA_DEP1$Industrial,
                        Merchants = DATA_DEP1$Merchants,
                        IndustComm = DATA_DEP1$IndustComm,
                        Everbright = DATA_DEP1$Everbright,
                        Construction = DATA_DEP1$Construction,
                        Minsheng = DATA_DEP1$Minsheng,
                        CITIC = DATA_DEP1$CITIC,
                        BOC = DATA_DEP1$BOC,
                        AgrBank = DATA_DEP1$AgrBank)

DATA_DEP3 <- melt(DATA_DEP2)
DATA_DEP4 <- data.frame(DATA = as.Date(DATA_DEP3$DATA, format = "%d/%m/%Y"),
                        value_DEP = DATA_DEP3$value,
                        variable = DATA_DEP3$variable)

### DADOS LOANS ###
  
attach(DADOS_BANCOS2)
DADOS_LOANS2013 <- DADOS_BANCOS2[which(DATA == '01/12/2013' & FLAG == 'LOANS'),]
detach(DADOS_BANCOS2)

attach(DADOS_BANCOS2)
DADOS_LOANS2014 <- DADOS_BANCOS2[which(DATA == '01/12/2014' & FLAG == 'LOANS'),]
detach(DADOS_BANCOS2)

attach(DADOS_BANCOS2)
DADOS_LOANS2015 <- DADOS_BANCOS2[which(DATA == '01/12/2015' & FLAG == 'LOANS'),]
detach(DADOS_BANCOS2)

DATA_LOANS1 <- rbind(DADOS_LOANS2013,
                   DADOS_LOANS2014,
                   DADOS_LOANS2015)
DATA_LOANS2 <- data.frame(DATA = DATA_LOANS1$DATA,
                        Beijing = DATA_LOANS1$Beijing,
                        ChongqingRural = DATA_LOANS1$ChongqingRural,
                        HuaXia = DATA_LOANS1$HuaXia,
                        BoCommu = DATA_LOANS1$BoCommu,
                        Nanjing = DATA_LOANS1$Nanjing,
                        Ningbo = DATA_LOANS1$Ningbo,
                        PingAn = DATA_LOANS1$PingAn,
                        Wuxi = DATA_LOANS1$Wuxi,
                        ShangaiDev = DATA_LOANS1$ShangaiDev,
                        Industrial = DATA_LOANS1$Industrial,
                        Merchants = DATA_LOANS1$Merchants,
                        IndustComm = DATA_LOANS1$IndustComm,
                        Everbright = DATA_LOANS1$Everbright,
                        Construction = DATA_LOANS1$Construction,
                        Minsheng = DATA_LOANS1$Minsheng,
                        CITIC = DATA_LOANS1$CITIC,
                        BOC = DATA_LOANS1$BOC,
                        AgrBank = DATA_LOANS1$AgrBank)

DATA_LOANS3 <- melt(DATA_LOANS2)
DATA_LOANS4 <- data.frame(DATA = as.Date(DATA_LOANS3$DATA, format = "%d/%m/%Y"),
                        value_LOANS = DATA_LOANS3$value,
                        variable = DATA_LOANS3$variable)

### JUNTAR OS DADOS ###

AGREGADAO <- data.frame(DATA = DATA_DEP4$DATA,
                        value_NPL = DATA_NPL4$value_NPL,
                        value_DEP = DATA_DEP4$value_DEP,
                        value_LOANS = DATA_LOANS4$value_LOANS,
                        NPL_Ratio = (DATA_NPL4$value_NPL),
                        variable = DATA_LOANS4$variable)

### TESTE PLOTAR ###

DEP_NPL_2013 <- ggplot(data = subset(AGREGADAO, DATA =="2015-12-01"),aes(x = value_DEP, y = value_NPL)) + geom_point(shape = 1, size = 7) + geom_text_repel(aes(label=variable), size = 2)
                                                                                                                                                                                                                                                                                                                 
DEP_NPL_2013


