## SCRIPT QUE CALCULA O GINI PARA RENDA DO TRABALHO E RENDA EFETOVA DOMICILIAR 
## PER CAPITA ENTRE 2012 E 2021, PARA A AMOSTRA ENTRE 15 E 64 ANOS
## LEVANDO EM CONTA O DESENHO AMOSTRAL DA PNAD

################## Pacotes ##############
library(survey) # uso e modelos de dados de pesquisas amostrais
library(convey)
library(PNADcIBGE) # leitura de dados da PNADC
library(tidyverse) # manipulação de dados
library(ggplot2)
library(ggpubr)
library(reshape2)

## mudando o diretório (caso seja necessário)
getwd()
setwd("/Users/vinicius/Desktop/Artigos/PET/Artigo_PNADC_RIF/PNADC")

#para apagar todas as variáveis
rm(list = ls())
# Clear plots
dev.off()
# Clear console
cat("\014")

################## Leitura dos Dados ##############

# variáveis a serem lidas
vars <- c("Ano","Trimestre","UF","UPA","Estrato","posest","V1027","V1028",
          "V1029", "V1008", 'V1014',"V2005","V2007","V2009","V2010","VD3005",
          "VD2003","VD4020","VD4035"
        )

## lendo os dados
# 2012 a 2021 (alguns anos só serão usados pra calcular o gini)
# Fonte: https://www.ibge.gov.br/estatisticas/sociais/trabalho/17270-pnad-continua.html?=&t=downloads
nums <- c(2:10)
data <- c("PNADC_012012.txt","PNADC_012013.txt","PNADC_012014.txt",
          "PNADC_012015.txt","PNADC_012016.txt","PNADC_012017.txt",
          "PNADC_012018.txt","PNADC_012019.txt",
          "PNADC_012020.txt","PNADC_012021.txt")

ler_pnad <- function(){
  print(1)
  df1 <- read_pnadc(microdata = data[1], input = "input_PNADC_trimestral.txt", vars = vars)
  for (i in nums){
    ## lendo os dados
    print(i)
    df2 <- read_pnadc(microdata = data[i], input = "input_PNADC_trimestral.txt", vars = vars)
    # unindo vários dfs
    print("Unindo...")
    df1 <- rbind(df1,df2)
  }
  return(df1)
}

## lendo todos os dados
df.tot <- ler_pnad()

## TESTE DOS WEIGHTS COM IDENTIFICADOR DE ANOS NOS ESTRATOS####
# df11 <- read_pnadc(microdata = data[1], input = "input_PNADC_trimestral.txt", vars = vars)
# df11 <- pnadc_deflator(df11, "deflator_PNADC_2021_trimestral_010203.xls")
# df11$VD4020 <- df11$VD4020*df11$Habitual
# 
# df2 <- read_pnadc(microdata = data[2], input = "input_PNADC_trimestral.txt", vars = vars)
# df2 <- pnadc_deflator(df2, "deflator_PNADC_2021_trimestral_010203.xls")
# df2$VD4020 <- df2$VD4020*df2$Habitual
# 
# df3 <- read_pnadc(microdata = data[3], input = "input_PNADC_trimestral.txt", vars = vars)
# df3 <- pnadc_deflator(df3, "deflator_PNADC_2021_trimestral_010203.xls")
# df3$VD4020 <- df3$VD4020*df3$Habitual
# 
# df <- rbind(df11,df2)
# df <- rbind(df,df3)
# 
# df11 <- pnadc_design(df11)
# df2 <- pnadc_design(df2)
# df3 <- pnadc_design(df3)
# df4 <- pnadc_design(df)
# 
# ## identificando os anos
# df$posest <- as.character(paste(df$Ano,df$posest, sep = ""))
# df$UPA <- as.character(paste(df$Ano,df$UPA, sep = ""))
# df$Estrato <- as.character(paste(df$Ano,df$Estrato, sep = ""))
# 
# df <- pnadc_design(df)
# 
# ## vendo se as probs batem (df1 + df2 = df)
# sum(df11$prob)+sum(df2$prob)+sum(df3$prob)
# sum(df4$prob)
# sum(df$prob)
# 
# ## vendo os weights
# sum(weights(df11))
# sum(weights(df2))
# sum(weights(df11))+sum(weights(df2))+sum(weights(df3))
# sum(weights(df4))
# sum(weights(df))
# 
# ## vendo a renda média
# svyby(~VD4020, by = ~Ano, df, svymean, na.rm  =  TRUE)
# svymean(~VD4020, df11,na.rm  =  TRUE, ci=TRUE,vartype="ci")
# svymean(~VD4020, df2,na.rm  =  TRUE, ci=TRUE,vartype="ci")
# svymean(~VD4020, df3,na.rm  =  TRUE, ci=TRUE,vartype="ci")
# svyby(~VD4020, by = ~Ano, df4, svymean, na.rm  =  TRUE)
# 
# df11 <- NULL
# df2 <- NULL
# df3 <- NULL
# df4 <- NULL
# df <- NULL

## Identificação dos indivíduos/domicílios ####
# UPA; V1008:número do domicílio; V1014: painel; V2003: ordenação das pessoas no domicílio
df.tot$iddom <- as.double(paste(df.tot$UPA, df.tot$V1008, df.tot$V1014, sep = ""))
df.tot$idind <- as.double(paste(df.tot$UPA, df.tot$V1008, df.tot$V1014, df.tot$V2003, sep = ""))

# criando coluna de data
df.tot$Trimestre <- as.integer(df.tot$Trimestre)
df.tot$Ano <- as.integer(df.tot$Ano)
df.tot$data <- as.integer(df.tot$Trimestre*10000 + df.tot$Ano)

# anos de educ
df.tot$educ <- as.integer(df.tot$VD3005)

# 2021 tem um número bem menor de observações e uma taxa de "participação" menor
table(df.tot$Ano)
table(df.tot$Ano[!is.na(df.tot$VD4020)])
table(df.tot$Ano[!is.na(df.tot$VD4020)])/table(df.tot$Ano)

# considerando individuos entre 15 e 64 anos, eram 378.729 em 2019 e 213.871 com trabalho
# de novo, 2020 e 2021 tem muito menos!
table(df.tot$Ano[df.tot$V2009 >= 15 & df.tot$V2009 < 65])
table(df.tot$Ano[df.tot$V2009 >= 15 & df.tot$V2009 < 65 & !is.na(df.tot$VD4020)])
table(df.tot$Ano[df.tot$V2009 >= 15 & df.tot$V2009 < 65 & !is.na(df.tot$VD4020)])/table(df.tot$Ano[df.tot$V2009 >= 15 & df.tot$V2009 < 65])

################## Manipulações Gerais e Renda ##############
## deflacionando (usando o CO1 para termos reais do último ano (2021))
df.tot <- pnadc_deflator(df.tot, "deflator_PNADC_2021_trimestral_010203.xls")
# deflacionando rendimentos efetivos
df.tot$VD4020 <- df.tot$VD4020*df.tot$Efetivo

#### renda domiciliar (de todos os trabalhos)
df.tot$renda.trab.efet.dom <- ave(df.tot$VD4020, df.tot$iddom, df.tot$data, FUN = function(x) sum(x, na.rm = TRUE))
df.tot$renda.efet.dom.pc <- df.tot$renda.trab.efet.dom/df.tot$VD2003

## renda por hora (VD4031 e VD4035 são horas por semana)
# arrumando caso haja uma hora efetiva = 0
df.tot$VD4035 <- ifelse(df.tot$VD4035 < 1, 1, df.tot$VD4035)
df.tot$sal.efet.hora <- df.tot$VD4020/(df.tot$VD4035*365/(7*12))

# por permitir a logaritmização, opta-se pela renda habitual de todos os trabalhos
# (já deflacionada pelo deflator CO1)
names(df.tot)[names(df.tot) == 'VD4020'] <- 'sal.efet'

## idade e experiencia minceriana
names(df.tot)[names(df.tot) == 'V2009'] <- 'idade'
df.tot$idade <- as.integer(df.tot$idade)

## gênero
names(df.tot)[names(df.tot) == 'V2007'] <- 'sexo'
# substituindo
df.tot$sexo[df.tot$sexo == 2] <- 'mulher'
df.tot$sexo[df.tot$sexo == 1] <- 'homem'
# colocando como fator (homem como base)
df.tot$sexo <- factor(df.tot$sexo)
# vendo os totais
table(df.tot$sexo) 

## cor
names(df.tot)[names(df.tot) == 'V2010'] <- 'cor'
index <- c(1,2,3,4,5,9)
subs <- c('branca','preta','amarela','parda','indigena',NA)
# substituindo
for (i in index) {
  ifelse(i == 9,
         df.tot$cor[df.tot$cor == 9] <- NA,
         df.tot$cor[df.tot$cor == i] <- subs[i]
  )
}
# colocando como fator
df.tot$cor <- factor(df.tot$cor)
table(df.tot$cor) # vendo os totais

# criando uma coluna ppi/branco
df.tot$cor2 <- ifelse(
  df.tot$cor == 'branca' | df.tot$cor == 'amarela',
  'branca',
  'ppi'
)
# colocando como fator
df.tot$cor2 <- factor(df.tot$cor2)
# vendo os totais
table(df.tot$cor2) 

## deixando apenas as colunas que vamos usar
uteis <- c('Ano','UPA','Estrato','posest','idade','V1027','V1028','V1029','sexo',
           'cor','cor2','sal.efet','renda.efet.dom.pc')
df.tot <- df.tot[,uteis]

# lendo apenas os de 2012,2015,2020,2021 na idade ativa e com renda e cor não-nula
# (mesma amostra usada nas análises do arquivo pnad_pet.R)
df.tot1 <- df.tot[df.tot$Ano %in% c(2012,2015,2020,2021),]
df.tot1 <- df.tot1[df.tot1$idade >= 15 & df.tot1$idade < 64 & !is.na(df.tot1$sal.efet) & !is.na(df.tot1$cor), ]

#### Survey: design próprio (levando em conta os anos na UPA, posest e Estrato) ####
# ver https://rdrr.io/cran/PNADcIBGE/src/R/pnadc_design.R para mais detalhes
df.tot <- arrange(df.tot, Ano)

#df.tot <- df1
## levando em conta os anos nas colunas específicas do survey
# se quiser usar a análise sem isso, só comentar as linhas abaixo :)

df.tot$posest <- as.character(paste(df.tot$Ano,df.tot$posest, sep = ""))
## vendo se bate o numero de dominios de projeção e o número de projeções (agora bate!)
length(unique(df.tot$posest))
length(unique(df.tot$V1029))

df.tot$UPA <- as.character(paste(df.tot$Ano,df.tot$UPA, sep = ""))
df.tot$Estrato <- as.character(paste(df.tot$Ano,df.tot$Estrato, sep = ""))

## fazendo o survey object na mão
df.tot <- pnadc_design(df.tot)
df.tot <- convey_prep(df.tot)

################## Rendas Médias e Medianas do Trabalho ##############
## rendas médias e medianas
Sys.time()
media.sal <- svyby(~sal.efet, by = ~Ano, 
                   subset(df.tot, idade >= 15 & idade < 65 & !is.na(sal.efet) & !is.na(cor)),
                   svymean, na.rm = TRUE, ci = TRUE, vartype="ci")

# mudando para um dataframe e mudando os nomes
media.sal <- as.data.frame(media.sal)
names(media.sal) <- c('Ano','media.sal','ci_l.media.sal','ci_u.media.sal')
media.sal

Sys.time()
mediana.sal <- svyby(~sal.efet, by = ~Ano,
                     subset(df.tot, idade >= 15 & idade < 65 & !is.na(sal.efet) & !is.na(cor)),
                     svyquantile, quantiles=0.5, na.rm = TRUE, ci = TRUE,vartype="ci")

# mudando para um dataframe e mudando os nomes
mediana.sal <- as.data.frame(mediana.sal)
names(mediana.sal) <- c('Ano','mediana.sal','ci_l.mediana.sal','ci_u.mediana.sal')
mediana.sal

## unindo tudo
renda <- merge(media.sal, mediana.sal, by = "Ano")

# alterando para ficar legível para o ggplot
ci_l.renda <- renda[, c("Ano","ci_l.media.sal","ci_l.mediana.sal")]
names(ci_l.renda) <- c("Ano","media.sal","mediana.sal")
ci_l.renda <- melt(ci_l.renda, id = c("Ano"))

ci_u.renda <- renda[, c("Ano","ci_u.media.sal","ci_u.mediana.sal")]
names(ci_u.renda) <- c("Ano","media.sal","mediana.sal")
ci_u.renda <- melt(ci_u.renda, id = c("Ano"))

renda <- renda[, c("Ano","media.sal","mediana.sal")]
renda <- melt(renda, id = c("Ano"))

renda <- merge(renda, ci_l.renda, by = c("Ano","variable"))
renda <- merge(renda, ci_u.renda, by = c("Ano","variable"))
names(renda) <- c("Ano","variable","value","ci.l","ci.u")
renda

################## Rendas Médias e Medianas Efetivas Domiciliares Per Capita ##############
Sys.time()
media.pc <- svyby(~renda.efet.dom.pc, by = ~Ano, df.tot,
                   svymean, na.rm = TRUE, ci = TRUE, vartype="ci")
Sys.time()

# mudando para um dataframe e mudando os nomes
media.pc <- as.data.frame(media.pc)
names(media.pc) <- c('Ano','media.pc','ci_l.media.pc','ci_u.media.pc')
media.pc

## calculando a mediana
Sys.time()
mediana.pc <- svyby(~renda.efet.dom.pc, by = ~Ano, df.tot,
                     svyquantile, quantiles=0.5, 
                    na.rm = TRUE, ci = TRUE,vartype="ci")
Sys.time()

# mudando para um dataframe e mudando os nomes
mediana.pc <- as.data.frame(mediana.pc)
names(mediana.pc) <- c('Ano','mediana.pc','ci_l.mediana.pc','ci_u.mediana.pc')
mediana.pc

## unindo tudo
renda.pc <- merge(media.pc, mediana.pc, by = "Ano")

# alterando para ficar legível para o ggplot
ci_l.renda <- renda.pc[, c("Ano","ci_l.media.pc","ci_l.mediana.pc")]
names(ci_l.renda) <- c("Ano","media.pc","mediana.pc")
ci_l.renda <- melt(ci_l.renda, id = c("Ano"))

ci_u.renda <- renda.pc[, c("Ano","ci_u.media.pc","ci_u.mediana.pc")]
names(ci_u.renda) <- c("Ano","media.pc","mediana.pc")
ci_u.renda <- melt(ci_u.renda, id = c("Ano"))

renda.pc <- renda.pc[, c("Ano","media.pc","mediana.pc")]
renda.pc <- melt(renda.pc, id = c("Ano"))

renda.pc <- merge(renda.pc, ci_l.renda, by = c("Ano","variable"))
renda.pc <- merge(renda.pc, ci_u.renda, by = c("Ano","variable"))

names(renda.pc) <- c("Ano","variable","value","ci.l","ci.u")
renda.pc

################## Gini ##############
## os índices calculados aqui serão diferentes do IBGE em virtude das restrições de idade
# e de usarmos apenas a renda do trabalho

# gini da renda do trabalho por ano
Sys.time()
gini.trab <- svyby(~sal.efet, by = ~Ano, 
                   subset(df.tot, idade >= 15 & idade < 65 & !is.na(sal.efet) & !is.na(cor)),
                   svygini, na.rm = TRUE, ci = TRUE, vartype="ci")
Sys.time()

# mudando para um dataframe e mudando os nomes
gini.trab <- as.data.frame(gini.trab)
names(gini.trab) <- c('Ano','gini.trab','ci_l.gini.trab','ci_u.gini.trab')
gini.trab

# gini da renda domiciliar per capita total por ano
gini.tot <-svyby(~renda.efet.dom.pc, by = ~Ano, 
                 df.tot, 
                 svygini, na.rm = T, ci = T,vartype="ci")
Sys.time()

# mudando para um dataframe e mudando os nomes
gini.tot <- as.data.frame(gini.tot)
names(gini.tot) <- c('Ano','gini.tot','ci_l.gini.tot','ci_u.gini.tot')
gini.tot

## unindo
gini <- merge(gini.trab, gini.tot, by = "Ano")
gini

# alterando para ficar legível para o ggplot
ci_l.gini <- gini[, c("Ano","ci_l.gini.trab","ci_l.gini.tot")]
names(ci_l.gini) <- c("Ano","gini.trab","gini.tot")
ci_l.gini <- melt(ci_l.gini, id = c("Ano"))

ci_u.gini <- gini[, c("Ano","ci_u.gini.trab","ci_u.gini.tot")]
names(ci_u.gini) <- c("Ano","gini.trab","gini.tot")
ci_u.gini <- melt(ci_u.gini, id = c("Ano"))

gini <- gini[, c("Ano","gini.trab","gini.tot")]
gini <- melt(gini, id = c("Ano"))

gini <- merge(gini, ci_l.gini, by = c("Ano","variable"))
gini <- merge(gini, ci_u.gini, by = c("Ano","variable"))
names(gini) <- c("Ano","variable","value","ci.l","ci.u")
gini

#### Gráficos! ####
### gini
## em ambos os casos, 2015 é o ponto mínimo
# logo após a crise, houve aumento da desigualdade, sendo que o mesmo ocorreu em 2021 com a pandemia
# corroborando os achados do TD 2610 do IPEA (o pouco crescimento que teve foi apropriado apenas pelos mais ricos)
# e com os dados de HECKSHER (2018), que mostram a contribuição principalmente dos 5% para a desigualdade elevada
# como mostram MEDEIROS e SOUZA (2016), a desigualdade brasileira é notadamente presente no topo
# (e subestimada pelos dados da PNAD); progresso nulo (e negativo na RDPC) nos últimos 10 anos

## análise interessante: comparar os dados de gini com a amostra da PEA toda x retirando os 0,01% mais ricos;
# dá pra fazer um link com SOUZA, MEDEIROS (2016) e elaborar sobre a concentração de renda no topo extremo da distribuição
grafico.gini <- gini %>% 
  ggplot(aes(x=Ano)) +
  geom_point(
    aes(y = value, colour = variable), 
    shape = 18, size = 2
  ) +
  geom_text(
    aes(y = value, label = format(round(value,3), decimal.mark = ",")), 
    nudge_y = 0.01, size = 3
  ) +
  geom_line(aes(y = value, colour = variable), size = 1) +
  geom_ribbon(aes(ymin = ci.l, ymax = ci.u, fill = variable), alpha = 0.3, show.legend = F) +
  scale_colour_manual(name = "",
                      labels = c('Renda do Trabalho',"RTDPC"),
                      values = c("black","gray40")) + 
  scale_fill_manual(values=c("black","gray40")) + 
  labs(x = 'Ano', y = 'Gini') +
  scale_x_continuous(breaks = seq(2012,2021, by = 1)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.background = element_rect(fill = NA)
  )
grafico.gini
ggsave("gini_efet.pdf", dpi=1200)

### renda
# renda média aumentando e mediana estável: recuperação apenas dos mais ricos
# reparar na queda da renda mediana em 2021! pandemia afetando desproporcionalmente mais os mais pobres
grafico.renda <- renda %>% 
  ggplot(aes(x=Ano)) +
  geom_point(
    aes(y = value, colour = variable), 
    shape = 18, size = 2
    ) +
  geom_text(
    aes(y = value, label = format(round(value), big.mark = ".", decimal.mark = ",")), 
    nudge_y = 125, size = 3
    ) +
  geom_line(aes(y = value, colour = variable), size = 1) +
  geom_ribbon(aes(ymin = ci.l, ymax = ci.u, fill = variable), alpha = 0.3, show.legend = F) +
  scale_colour_manual(name = "",
                      labels = c('Média',"Mediana"),
                      values = c("black","gray40")) + 
  scale_fill_manual(values=c("black","gray40")) + 
  labs(x = 'Ano', y = 'Renda do Trabalho (R$ 2021)') +
  scale_x_continuous(breaks = seq(2012,2021, by = 1)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.background = element_rect(fill = NA)
    )
grafico.renda
ggsave("renda_media_mediana_efet.pdf", dpi = 1200)

## gráfico de renda domiciliar per capita
grafico.renda.pc <- renda.pc %>% 
  ggplot(aes(x=Ano)) +
  geom_point(
    aes(y = value, colour = variable), 
    shape = 18, size = 2
  ) +
  geom_text(
    aes(y = value, label = format(round(value), big.mark = ".", decimal.mark = ",")), 
    nudge_y = 60, size = 3
  ) +
  geom_line(aes(y = value, colour = variable), size = 1) +
  geom_ribbon(aes(ymin = ci.l, ymax = ci.u, fill = variable), alpha = 0.3, show.legend = F) +
  scale_colour_manual(name = "",
                      labels = c('Média',"Mediana"),
                      values = c("black","gray40")) + 
  scale_fill_manual(values=c("black","gray40")) + 
  labs(x = 'Ano', y = 'Renda do Trabalho Domiciliar per Capita (R$ 2021)') +
  scale_x_continuous(breaks = seq(2012,2021, by = 1)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.background = element_rect(fill = NA)
  )
grafico.renda.pc
ggsave("renda_media_mediana_pc_efet.pdf", dpi = 1200)

#### Exportando para Excel ####
write.xlsx(gini, file = "estatisticas_rendas_ginis.xlsx", 
           sheetName = "gini_efet", col.names = T, row.names = F, append = T)

write.xlsx(renda, file = "estatisticas_rendas_ginis.xlsx", 
           sheetName = "renda_efet", col.names = T, row.names = F, append = T)

write.xlsx(renda.pc, file = "estatisticas_rendas_ginis.xlsx", 
           sheetName = "renda_pc_efet", col.names = T, row.names = F, append = T)
