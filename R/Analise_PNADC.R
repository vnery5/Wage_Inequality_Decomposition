## Script que modifica a PNADC de anos selecionados, fazendo análises descritivas
## e regressões com pesos amostrais. Além disso, exporta um .dta que servirá de base
## para as análises RIF feitas no STATA.

#### Pacotes e Diretório ####
library(survey) # uso e modelos de dados de pesquisas amostrais (com pesos pós-estratificados e correção de variância)
library(convey)
library(PNADcIBGE) # leitura de dados da PNADC
library(tidyverse) # manipulação de dados
library(magrittr)
library(ggpubr)
library(ggdist)
library(reshape2)
library(psych) # describe (instead of summary)
library(oaxaca) # oaxaca-blinder 
library(dineq) # regressões RIF
library(DescTools) # weighted gini
library(foreign) # exporting to and reading .dta files
library(stargazer) # for cute tables
library(sandwich) # for robust SEs
library(performance)
library(reldist)

# Para apagar todas as variáveis
rm(list = ls())
# Limpar gráficos
dev.off()
# Limpar console
cat("\014")

## Mudando o diretório (caso seja necessário)
getwd()
setwd("/Users/vinicius/Desktop/Artigos/PET/Artigo_PNADC_RIF/PNADC")

t1 <- Sys.time()

#### Leitura dos Dados ##############

# Variáveis a serem lidas
vars <- c("Ano","Trimestre","UF","UPA","Estrato","V1027","V1028","V1029","posest",
          "V1008","V1014","V1022","V2001","V2003","V2005","V2007","V2009","V2010",
          "VD2002","VD2003","VD3004","VD3005","VD3006","VD4001","VD4002","VD4003",
          "VD4005","VD4008","VD4009","VD4010","VD4016","VD4017","VD4019",
          "VD4020","VD4031","VD4035","VD4036","VD4037")

## Lendo os dados
# Anos específicos (2012, 2015, 2020, 2021)
# Fonte: https://www.ibge.gov.br/estatisticas/sociais/trabalho/17270-pnad-continua.html?=&t=downloads
nums <- c(2, 3, 4)
data_compressed <- c("PNADC_012012_20211130.zip","PNADC_012015_20211130.zip",
                     "PNADC_012020_20211130.zip","PNADC_012021_20211130.zip")
data <- c("PNADC_012012.txt","PNADC_012015.txt",
          "PNADC_012020.txt","PNADC_012021.txt")
data.csv <- c("PNADC_012012.csv","PNADC_012015.csv",
              "PNADC_012020.csv","PNADC_012021.csv")

# Função para ler e concantenar múltiplos anos
ler_pnad <- function(){
  print(1)
  df1 <- read_pnadc(microdata = data[1], 
                    input = "input_PNADC_trimestral.txt", vars = vars)
  for (i in nums){
    ## Lendo os dados
    print(i)
    df2 <- read_pnadc(microdata = data[i], 
                      input = "input_PNADC_trimestral.txt", vars = vars)
    # Unindo os dfs
    print("Unindo...")
    df1 <- rbind(df1,df2)
  }
  return(df1)
}

## Versão com csvs
ler_pnad_csv <- function(){
  print(1)
  df1 <- read.csv(data.csv[1])
  for (i in nums){
    ## Lendo os dados
    print(i)
    df2 <- read.csv(data.csv[i])
    # Unindo os dfs
    print("Unindo...")
    df1 <- rbind(df1,df2)
  }
  return(df1)
}

## Lendo os dados
df <- ler_pnad()
#df <- ler_pnad_csv()

# Alternativamente (com uma boa internet), 
# df <- get_pnadc(year = 2019, interview = 1, vars = vars)

## Modificando o diretório de volta
setwd("/Users/vinicius/Desktop/Artigos/PET/Artigo_PNADC_RIF")

#### Identificação dos indivíduos/domicílios ####
## UPA: unidade primária de amostragem (contém 14 domicílios); V1008: número do domicílio; 
## V1014: painel; V2003: ordenação das pessoas no domicílio

# OBS: como dito no webinar do Rafael Osorio (pynad), o número de ordem das pessoas
# pode mudar, sendo possível, preferencialmente, usar dia, mês e ano de nascimento.
# Contudo, há pessoas que não declaram sua data de aniversário, o que se torna um problema.
# Além disso, há problemas com mudança na condição de domicílio, o que altera V2003 e V2005

# Economizando memória
df$UPA <- as.integer(df$UPA)
df$V1008 <- as.integer(df$V1008)
df$V1014 <- as.integer(df$V1014)
df$V2003 <- as.integer(df$V2003)

# Criando ids de domicílios e indivíduos
df$iddom <- as.double(paste(df$UPA, df$V1008, df$V1014, sep = ""))
df$idind <- as.double(paste(df$UPA, df$V1008, df$V1014, df$V2003, sep = ""))

# Criando coluna de data
df$Trimestre <- as.integer(df$Trimestre)
df$Ano <- as.integer(df$Ano)
df$data <- as.integer(df$Trimestre*10000 + df$Ano)

## 2021 (e principalmente 2021) tem um número bem menor de observações
## e uma "taxa de participação" bem menor (2021: < 40%);
## 2021 tem 320.000 observações e 120.000 que trabalham; muito menor que ambos
## 2021 tem tanto um número bem menor de observações quanto uma taxa de "participação" menor
table(df$Ano)
table(df$Ano[!is.na(df$VD4019)])
table(df$Ano[!is.na(df$VD4019)])/table(df$Ano)

# considerando individuos entre 15 e 64 anos, a mesma análise é válida
# Das meras 115.235 observações em idade ativa, 52% trabalham
# máximo: 2012 (224.633); mínimo: 2021 (115.235)
table(df$Ano[df$V2009 >= 15 & df$V2009 < 65])
table(df$Ano[df$V2009 >= 15 & df$V2009 < 65 & !is.na(df$VD4019)])
table(df$Ano[df$V2009 >= 15 & df$V2009 < 65 & !is.na(df$VD4019)])/table(df$Ano[df$V2009 >= 15 & df$V2009 < 65])

## vendo a taxa de desocupação; crescente ao longo do tempo e principalmente em 2020 e 2021 (12 e 14,36%)
# não esquecer de mencionar a menor taxa de participação em virtude da pandemia!
table(df$Ano[df$V2009 >= 15 & df$V2009 < 65 & df$VD4002 == 2])/table(df$Ano[df$V2009 >= 15 & df$V2009 < 65 & df$VD4001 == 1])

#### Manipulações Gerais ##############
# para acessar várias colunas
select(df, iddom, idind)

# para mudar o tipo de uma coluna  (aqui, anos de educação)
table(df$VD3005)  #value_counts
df$educ <- as.integer(df$VD3005)
typeof(df$educ)
df$VD3005 <- NULL

## Criando educ^2, ^3 e ^4
df$educ2 <- as.integer(df$educ**2)
df$educ3 <- as.integer(df$educ**3)
df$educ4 <- as.integer(df$educ**4)

## deflacionando (usando o CO1 para termos reais do último ano (2021))
df <- pnadc_deflator(df, "deflator_PNADC_2021_trimestral_070809.xls")
# deflacionando rendimentos habituais
df$VD4019 <- df$VD4019*df$Habitual
# deflacionando rendimentos efetivos
df$VD4020 <- df$VD4020*df$Efetivo

# mudando o tipo da variável de condição no domicilio
df$V2005 <- as.integer(df$V2005)

## ordenando o dataframe
df <- arrange(df, iddom, Ano, V2005)

## dummy de filho e filho na primeira infância
df$filho <-  ifelse(df$V2005 %in% c(4,5,6), 1, 0)
#se tem idade <= 5 e tá nas condições de filho
df$filho05 <-  ifelse(df$V2009 <= 5 & df$V2005 %in% c(4,5,6), 1, 0)

## calculando o total de filhos
# somar filhos agrupando por iddom e data
df$num.filhos <- ave(df$filho, df$iddom, df$data, FUN = sum)
df$num.filhos05 <- ave(df$filho05, df$iddom, df$data, FUN = sum)

## deixando os valores apenas para chefes e conjugues
df$num.filhos <- ifelse(df$V2005 > 3, 0, df$num.filhos)
df$num.filhos05 <- ifelse(df$V2005 > 3, 0, df$num.filhos05)
## a maioria da amostra (66,5%) não possui filhos; o máximo que temos é de TREZE filhos!
table(df$num.filhos)

# removendo as colunas de filhos
df$filho <- NULL
df$filho05 <- NULL

# a mediana de renda habitual é de 1.328,70, sendo que não há valores iguais a 0
# na amostra como um todo, o máximo auferido foi de R$ 300.000 e 40,33% possuem rendimento do trabalho
summary(df[, c("VD4019", "VD4020")])
summary(df[, c("VD4031", "VD4035")])

## renda por hora (VD4031 e VD4035 são horas por semana)
# arrumando caso haja uma hora efetiva = 0
# table(df$VD4035)
# df$VD4035 <- ifelse(df$VD4035 < 1, 1, df$VD4035)

df$sal.hab.hora <- df$VD4019/(df$VD4031*365/(7*12))
df$sal.efet.hora <- df$VD4020/(df$VD4035*365/(7*12))
df$sal.efet.hora <- ifelse(is.infinite(df$sal.efet.hora), NA, df$sal.efet.hora)
summary(df[,c("sal.hab.hora", "sal.efet.hora")])

## renda domiciliar (de todos os trabalhos)
df$renda.trab.hab.dom <- ave(df$VD4019, df$iddom, df$data, FUN = function(x) sum(x, na.rm = TRUE))
df$renda.hab.dom.pc <- df$renda.trab.hab.dom/df$VD2003

## renomeando e logaritmizando
# Habitual
names(df)[names(df) == 'VD4019'] <- 'sal.hab'
df$lsal <- log(df$sal.hab)
df$lsalh <- log(df$sal.hab.hora)

# Efetivo
names(df)[names(df) == 'VD4020'] <- 'sal.efet'
df$lsal.efet <- log(df$sal.efet)
df$lsal.efet[which(!is.finite(df$lsal.efet) & !is.na(df$lsal.efet))] <- 0

df$lsalh.efet <- log(df$sal.efet.hora)
df$lsalh.efet[which(!is.finite(df$lsalh.efet) & !is.na(df$lsalh.efet))] <- 0

summary(df[,c("sal.efet", "lsal.efet","sal.efet.hora","lsalh.efet")])

## idade e experiencia minceriana
names(df)[names(df) == 'V2009'] <- 'idade'
df$idade <- as.integer(df$idade)

df$exper <- as.integer(ifelse(df$educ < 9, df$idade - 15, df$idade - df$educ - 6))
df$exper <- ifelse(df$exper < 0, 0, df$exper)

## Criando exper2,exper3, exper4
df$exper2 <- as.integer(df$exper**2)
df$exper3 <- as.integer(df$exper**3)
df$exper4 <- as.integer(df$exper**4)

###### Variáveis Categóricas ####

## criando a dummy de casado
# criando uma coluna "atrasada"
df$V2005 <- as.integer(df$V2005)
df <- df %>%
  mutate(V2005_1 = lead(V2005))

# ifelse para criar a dummy
# se for casado (2,3) ou é chefe de domicílio (1) com conjugue (2,3)
df$casado <- ifelse(df$V2005 %in% c(2,3) | (df$V2005 == 1 & df$V2005_1 %in% c(2,3)), 1, 0)
# vendo os resultados
select(df, iddom, idind, data, V2005, V2005_1, casado)

# removendo a coluna auxiliar
df$V2005_1 <- NULL

## dummy de chefe de domicílio
df$chefe.dom <- ifelse(df$V2005 == 1, 1, 0)

## rural
names(df)[names(df) == 'V1022'] <- 'rural'
# substituindo
df$rural[df$rural == 2] <- 'rural'
df$rural[df$rural == 1] <- 'urbano'
# colocando como fator
df$rural <- factor(df$rural)
# vendo os totais: 73% da amostra é urbana
table(df$rural)/nrow(df)

## região
df$UF <- as.integer(df$UF)

index <- c(1,2,3,4,5)
subs <- c("N","NE","SE","S","CO")
df$regiao <- subs[floor(df$UF/10)]

# colocando como fator (CO como base)
df$regiao <- factor(df$regiao)
contrasts(df$regiao)
# vendo os totais: 33% da amostra é do nordeste!
table(df$regiao)/nrow(df)

## gênero
names(df)[names(df) == 'V2007'] <- 'sexo'
# substituindo
df$sexo[df$sexo == 2] <- 'feminino'
df$sexo[df$sexo == 1] <- 'masculino'
# colocando como fator (homem como base)
df$sexo <- factor(df$sexo)
# vendo os totais: amostra bem equilibrada
table(df$sexo)/nrow(df)

## cor
names(df)[names(df) == 'V2010'] <- 'cor'
index <- c(1, 2, 3, 4, 5, 9)
subs <- c('branca','preta','amarela','parda','indigena',NA)
# substituindo
for (i in index) {
  ifelse(i == 9,
         df$cor[df$cor == 9] <- NA,
         df$cor[df$cor == i] <- subs[i]
         )
}
# colocando como fator
df$cor <- factor(df$cor)
# vendo os totais: 
  # amarela: 0,3%; indigena: 0,3%
  # branca: 40%; parda: 51%; preta: 7,5%
table(df$cor)/nrow(df)

# criando uma coluna ppi/branco
df$cor2 <- ifelse(
  df$cor == 'branca' | df$cor == 'amarela',
  'branca',
  'ppi'
)
# colocando como fator
df$cor2 <- factor(df$cor2)
# vendo os totais: ppi são 59,42% da amostra
table(df$cor2)/nrow(df)

## grau de educação
names(df)[names(df) == 'VD3004'] <- 'grau.educ'
index <- c(1,2,3,4,5,6,7)
subs <- c('si','fund.inc','fund.comp','med.inc','med.comp','sup.inc','sup.comp')

# substituindo
for (i in index) {
  df$grau.educ[df$grau.educ == i] <- subs[i]
}
# colocando como fator
df$grau.educ <- factor(df$grau.educ)
# vendo o grupo base
contrasts(df$grau.educ)

# alterando o grupo base (para si)
df <- df %>%
  mutate(grau.educ = relevel(grau.educ, ref = "si"))
contrasts(df$grau.educ)

# vendo os totais: 54% da amostra tem ≤ fund. completo; 30% tem >= med comp.
table(df$grau.educ)/nrow(df)*100

## ocupação
names(df)[names(df) == 'VD4009'] <- 'pos.ocupacao'
df$pos.ocupacao <- as.integer(df$pos.ocupacao)
index <- c(1:10)
subs <- c('formal.priv','informal.priv','formal.dom','informal.dom','formal.pub','informal.pub','militar','empregador','conta.propria','familiar')
# substituindo
for (i in index) {
  df$pos.ocupacao[df$pos.ocupacao == i] <- subs[i]
}
# colocando como fator
df$pos.ocupacao <- factor(df$pos.ocupacao)
contrasts(df$pos.ocupacao)

# alterando o grupo base (para formal público)
df <- df %>%
  mutate(pos.ocupacao = relevel(pos.ocupacao, ref = "formal.pub"))
contrasts(df$pos.ocupacao)

typeof(df$pos.ocupacao)

## formalidade
df$formalidade <- ifelse(
  df$pos.ocupacao %in% c('formal.priv','formal.dom','formal.pub','militar','empregador'),
  'formal',
  'informal'
)

# colocando como fator
df$formalidade <- factor(df$formalidade)

# alterando o grupo base (para informal)
df <- df %>%
  mutate(formalidade = relevel(formalidade, ref = "informal"))
contrasts(df$formalidade)

## setor e setor simplificado
names(df)[names(df) == 'VD4010'] <- 'setor'
df$setor <- as.integer(df$setor)
df$setor.simp <- df$setor

index <- c(1:12)
subs <- c('agro','industria','construcao','comercio','transporte','aloj.alim','servicos.fin','adm.publica','educ.saude','outros.servicos','servicos.domesticos','ativ.mal.definidas')
subs.simp <- c('agro','industria','construcao','servicos','servicos','servicos','servicos','servicos','servicos','servicos','servicos','servicos')

# substituindo
for (i in index) {
  df$setor[df$setor == i] <- subs[i]
  df$setor.simp[df$setor.simp == i] <- subs.simp[i]
}
# colocando como fator (adm.publica como base)
df$setor <- factor(df$setor)

## desocupados, desalentados e força de trabalho
df$VD4001[df$VD4001 == 1] <- "dentro.ft"
df$VD4001[df$VD4001 == 2] <- "fora.ft"
df$VD4001 <- factor(df$VD4001)

df$VD4002[df$VD4002 == 1] <- "ocupado"
df$VD4002[df$VD4002 == 2] <- "desocupado"
df$VD4002 <- factor(df$VD4002)

df$VD4003[df$VD4003 == 1] <- "fora.ft.dentro.ft.potencial"
df$VD4003[df$VD4003 == 2] <- "fora.ft.fora.ft.potencial"
df$VD4003 <- factor(df$VD4003)

#### Data para Objeto Survey e para o Stata ####

## Selecionado apenas as colunas que vamos usar
uteis <- c('Ano','UPA','Estrato','posest','idade','V1027','V1028','V1029','sexo',
           'cor','cor2','sal.hab','sal.hab.hora','lsal','lsalh','renda.hab.dom.pc',
           'V2005','VD4001','VD4002','VD4003','setor','setor.simp','pos.ocupacao',
           'formalidade','grau.educ','regiao','rural','chefe.dom', 'casado',
           'num.filhos','num.filhos05','idind','iddom', 'VD4031', 'VD4035',
           'sal.efet','sal.efet.hora','lsal.efet','lsalh.efet',
           'educ','educ2','educ3','educ4','exper','exper2','exper3','exper4')

## Restringindo as colunas
df <- df[, uteis]

## Ordenando por ano
df <- arrange(df, Ano)

## Alterando alguns nomes
names(df)[names(df) == 'cor'] <- 'Cor'
names(df)[names(df) == 'cor2'] <- 'Cor2'
names(df)[names(df) == 'sexo'] <- 'Sexo'
df$Cor.Sexo <- paste(df$Cor, df$Sexo, sep = ".")
df$Cor2.Sexo <- paste(df$Cor2, df$Sexo, sep = ".")

## Identificando os anos na UPA, posest e Estrato 
# Fazemos isso para permitir correto desenho amostral quando usamos dados de 
# vários anos/trimestres
df$posest <- as.character(paste(df$Ano, df$posest, sep=""))
df$UPA <- as.character(paste(df$Ano, df$UPA, sep=""))
df$Estrato <- as.character(paste(df$Ano, df$Estrato, sep=""))

## Criando o objeto survey e preparando-o para análises de desigualdade (se necessário)
df.svy <- pnadc_design(df)
# df.svy <- convey_prep(df.svy)

## Exportando para dta
# write.dta(df, "df_pnad_pet.dta")

## Restringindo a subpopulação de interesse usando o subset
# Isso torna os pesos das observações não desejadas iguais a 0 (e as probs = inf),
# o que permite calcular as variâncias corretamente (dropar é errado!)
df.svy <- subset(df.svy, idade >= 15 & idade < 65 & !is.na(sal.hab) & !is.na(Cor))

## Droppando as observações do df original (sem o desenho amostral)
# Usando o df apenas com as observações em idade ativa (15 a 64) e com renda do 
# trabalho não-nula; de 1.947.690 obs, caimos para 756.540 e para 463.137 domicilios
df <- df[df$idade >= 15 & df$idade < 65 & 
           !is.na(df$sal.hab) & 
           !is.na(df$Cor), ]

# Vendo estatísticas
length(unique(df$iddom))
table(df$Ano)
summary(df[, c("sal.efet.hora","lsalh.efet")])

## Pegando os pesos amostrais (V1028)
df$pesos <- weights(df.svy)[weights(df.svy) > 0]

# vendo o tempo que demorou o código
T1 <- Sys.time() - t1
T1

# Lembrando: os pesos são pós-estratificados (V1028) e a probabilidade de
# uma pessoa de uma pessoa na amostra é Prob  = 1 / Peso
df$prob <- 1 / df$pesos
select(df, pesos, V1028, prob)

# Mais informações sobre a reponderação da PNADC: 
# https://guilhermejacob.github.io/2021/12/pnadc-raking-bootstrap/

#### Lendo a base filtrada ####
df <- read.dta("df_pnad_pet.dta")

#### Curvas de Lorenz ####
quantis <- c(seq(0,.01,.001), seq(.01,0.09,0.01), seq(0.1,0.9,0.1), seq(0.91,.99,0.01), seq(.991,1, 0.001))

# ## 1% ganha menos de 100, enquanto os 99% mais ricos ganham até 15.784
# quantile(df$sal.hab, probs = quantis, na.rm = FALSE, names = TRUE)
# 
# ## calculando as curvas de lorenz para cada ano
# lorenz <- rbind(
#   as.data.frame(
#     svylorenz(
#       ~sal.hab, design = subset(df.svy, Ano == 2012), quantiles = quantis, plot = F
#     )$quantiles
#   ),
#   as.data.frame(
#       svylorenz(
#       ~sal.hab, design = subset(df.svy, Ano == 2015), quantiles = quantis, plot = F
#     )$quantiles
#   ),
#   as.data.frame(
#     svylorenz(
#       ~sal.hab, design = subset(df.svy, Ano == 2020), quantiles = quantis, plot = F
#     )$quantiles
#   ),
#   as.data.frame(
#     svylorenz(
#       ~sal.hab, design = subset(df.svy, Ano == 2021), quantiles = quantis, plot = F
#     )$quantiles
#   )
# )
# 
# lorenz$Ano <- factor(c(2012,2015,2020,2021))
# 
# ## Pivotando
# lorenz %<>% 
#   pivot_longer(!c(Ano), names_to = "quantil", values_to = 'values')
# 
# lorenz$quantil <- as.numeric(lorenz$quantil)
# 
# ## plottando as curvas!
# # 2012 - 2015
# lorenz[lorenz$Ano %in% c(2012, 2015),] %>% 
#   ggplot(aes(x = quantil, y = values, colour = Ano)) + 
#   geom_line(size = .75) + 
#   labs(x= "Quantil", y = "% da Renda Acumulada") + 
#   scale_x_continuous(breaks = seq(0, 1, .1),
#                      labels=function(x) format(paste(100*x,"º", sep = ""))) +
#   scale_y_continuous(labels = scales::percent) +
#   theme_bw() + 
#   theme(
#     legend.position = 'top',
#     legend.direction = 'horizontal',
#     legend.box = 'horizontal',
#     legend.text = element_text(size = 10),
#     axis.title = element_text(size = 10),
#     legend.title = element_blank(),
#     legend.background = element_rect(fill = NA)
#   )
# ggsave("lorenz1215.pdf", dpi = 1200)
# 
# ## excluindo os 0,1% mais pobres e mais ricos
# pobre <- quantile(df$sal.hab, probs = c(.001), na.rm = FALSE, names = TRUE)
# rico <- quantile(df$sal.hab, probs = c(.999), na.rm = FALSE, names = TRUE)
# 
# df <- df[df$sal.hab >= pobre & df$sal.hab <= rico, ]
# df.svy <- subset(df.svy, sal.hab >= pobre & sal.hab <= rico)

#### Ginis, Rendas Médias e Quantis ####
## para ter os erros padrão corretos, o certo é utilizar o pacote survey;
## como os cálculos das matrizes de covariância são demorados, aqui se utiliza
## os pesos para calcular as estimativas centrais (que serão identicas à do survey)

## Gini por ano
ginis <- df %>% 
  group_by(Ano) %>% 
  summarise(n = n(), pop = sum(pesos), gini = gini.wtd(sal.hab, pesos))
# ginis.se <- svyby(~sal.hab, by = ~Ano, df.svy1, svygini, quantiles = c(.1,.5,.9), na.rm = T)
# Gini(df$sal.efet[df$Ano == 2021], n = df$pesos[df$Ano == 2021], unbiased = T, 
#      conf.level = T, R = 50, type = "bca", na.rm = F)

## renda média e pesos por ano
rendas <- df %>% 
  group_by(Ano) %>% 
  summarise(n = n(),
            ocupados = sum(pesos), 
            media.hab.simples = mean(sal.hab),
            media.hab.pond = sum(pesos*sal.hab)/sum(pesos),
            media.efet.simples = mean(sal.efet),
            media.efet.pond = sum(pesos*sal.efet)/sum(pesos),
            media.horas.hab.simples = mean(VD4031),
            media.horas.hab.pond = sum(pesos*VD4031)/sum(pesos),
            media.horas.efet.simples = mean(VD4035),
            media.horas.efet.pond = sum(pesos*VD4035)/sum(pesos),
            media.hab.hora.pond = sum(pesos*sal.hab.hora)/sum(pesos),
            media.efet.hora.pond = sum(pesos*sal.efet.hora)/sum(pesos),
            )
rendas
# media.renda.se <- svyby(~sal.hab, by = ~Ano, df.svy, svymean, na.rm = T)

## quantis por ano
lista.quantis <- c(seq(0,.01,.001), seq(.01,0.09,0.01), seq(0.1,0.9,0.1), seq(0.91,.99,0.01), seq(.991,1, 0.001))

quantis <- df %>% 
  group_by(Ano) %>% 
  summarise(n = n(), pop = sum(pesos),
            p10 = wtd.quantile(sal.hab, q = .1, weight = pesos),
            p50 = wtd.quantile(sal.hab, q = .5, weight = pesos),
            p90 = wtd.quantile(sal.hab, q = .9, weight = pesos),
            p10.efet = wtd.quantile(sal.efet, q = .1, weight = pesos),
            p50.efet = wtd.quantile(sal.efet, q = .5, weight = pesos),
            p90.efet = wtd.quantile(sal.efet, q = .9, weight = pesos),
  )
# quantis.se <- svyby(~sal.hab, by = ~Ano, df.svy, svyquantile, quantiles = c(.1,.5,.9,.99,.999), na.rm = T)

#### Funções de formatação das colunas ####
## antes das análises, vai ser útil ter uma função que deixa as primeiras letras das strings em caps
Proper <- function(x) {
  paste(toupper(substring(x,1,1)), substring(x,2), sep = "")
}

## e uma que formata as porcentagens
Perc <- function(x) {
  paste(format(round(x,1), decimal.mark = ",", scientific = F), '%', sep = "")
}
## Formatando grau de educ
educ.labels <- c("Sem\nInstrução","Fundamental\nIncompleto","Fundamental\nCompleto","Médio\nIncompleto",
                 "Médio\nCompleto","Superior\nIncompleto","Superior\nCompleto")
orig <- c('si','fund.inc','fund.comp','med.inc','med.comp','sup.inc','sup.comp')

df$grau.educ <- factor(df$grau.educ, levels = orig)
levels(df$grau.educ) <- gsub("\n"," ", educ.labels)

#### Análises por Cor ####

color.labels <- c('Amarela','Branca','Indígena','Parda','Preta')

## dataframe com algumas informações agrupadas por cor (sem divisão de ano)
cor <- df %>%
  dplyr::group_by(Cor) %>%
  dplyr::summarise(n=n(), renda_media = mean(sal.hab),
                   pop = sum(pesos)/1000, renda = weighted.mean(sal.hab, pesos)) %>%
  dplyr::mutate(freq = n/sum(n), freq.pop = pop/sum(pop))
# formatando as labels
cor$Cor <- cor$Cor %>%
  as.factor() %>%
  Proper()
cor

### gráficos de contagem
## na amostra
cor %>%
  ggplot(aes(x= Cor, y = n, fill = Cor)) +
  geom_text(aes(label = paste0(format(round(100*freq,1),decimal.mark = ","),'%')), 
            nudge_y = 15000, size = 4) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=c("gold2","gray90","tan3","tan4","black")) +
  scale_x_discrete(labels = color.labels) +
  scale_y_continuous(labels=function(x) format(
    x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Cor', y = 'Observações') +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.background = element_rect(fill = NA))
ggsave("obs_cor.pdf", dpi = 1200)

## na população
cor %>%
  ggplot(aes(x= Cor, y = pop, fill = Cor)) +
  geom_text(aes(label = paste0(format(round(100*freq.pop,1),decimal.mark = ","),'%')), 
            nudge_y = 6000, size = 4) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=c("gold2","gray90","tan3","tan4","black")) +
  scale_x_discrete(labels = color.labels) +
  scale_y_continuous(labels=function(x) format(
    x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Cor', y = 'População (em milhares)') +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.background = element_rect(fill = NA))
ggsave("pop_cor.pdf", dpi = 1200)

## Distribuições de Renda
# apenas o boxplot capped em 5.000
df[df$sal.hab <= 5000,] %>%
  ggplot(aes(x= Cor, y = sal.hab, fill = Cor)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values=c("gold2","gray90","tan3","tan4","black")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Cor', y = 'Renda (R$ 2021)') +
  scale_x_discrete(labels = color.labels) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.background = element_rect(fill = NA))
ggsave("boxplot_renda5000_cor.pdf", dpi = 2400)

# Raincloud visualization
df[df$sal.hab <= 5000,] %>%
  ggplot(aes(x= Cor, y = sal.hab, fill = Cor)) +
  ggdist::stat_halfeye( # visualização da distribuição (metade de um violinplot)
    adjust = .3, # mais ou menos smooth
    justification = -.2,
    .width = 0, # retirando linha preta
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .2,
    alpha = .8,
    outlier.shape = NA
  ) + 
  scale_fill_manual(values=c("gold2","gray90","tan3","tan4","black")) +
  scale_x_discrete(labels = color.labels) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Cor', y = 'Renda (R$ 2021)') +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.background = element_rect(fill = NA)) + 
  coord_flip()
ggsave("raincloud_renda5000_cor.pdf", dpi = 2400)

## Distribuições de Educ
# amarelos e brancos possuem distribuição superior; ppis similares entre si
df %>%
    ggplot(aes(x= Cor, y = educ, fill = Cor)) +
    geom_boxplot(outlier.shape = NA) +
    scale_fill_manual(values=c("gold2","gray90","tan3","tan4","black")) +
    scale_y_continuous(breaks = seq(0,16, by = 4),
                       labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = 'Cor', y = 'Anos de Educação') +
    scale_x_discrete(labels = color.labels) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 11),
          legend.background = element_rect(fill = NA))
ggsave('boxplots_educ_cor.pdf', dpi = 2400)

# Raincloud visualization
df %>%
  ggplot(aes(x= Cor, y = educ, fill = Cor)) +
  ggdist::stat_halfeye( # visualização da distribuição (metade de um violinplot)
    adjust = .3, # mais ou menos smooth
    justification = -.2,
    .width = 0, # retirando linha preta
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .2,
    alpha = .8,
    outlier.shape = NA
  ) + 
  scale_fill_manual(values=c("gold2","gray90","tan3","tan4","black")) +
  scale_x_discrete(labels = color.labels) +
  scale_y_continuous(
    breaks = seq(0,16, by = 4),
    labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)
  ) +
  labs(x = 'Cor', y = 'Anos de Educação') +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.background = element_rect(fill = NA)) + 
  coord_flip()
ggsave("raincloud_educ_cor.pdf", dpi = 2400)

#### Cor e Grau de Educ ####
## Contagens
agrup <- df %>%
  dplyr::group_by(grau.educ, Cor) %>%
  dplyr::summarise(n=n(), renda_media = mean(sal.hab),
                   pop = sum(pesos), renda = weighted.mean(sal.hab, pesos)) %>%
  dplyr::mutate(freq = n/sum(n), freq.pop = pop/sum(pop))

# formatando as labels
agrup$Cor <- agrup$Cor %>%
  as.factor() %>%
  Proper()
agrup

agrup.educ <- df %>%
  dplyr::group_by(grau.educ) %>%
  dplyr::summarise(n=n(), renda_media = mean(sal.hab),
                   pop = sum(pesos), renda = weighted.mean(sal.hab, pesos)) %>%
  dplyr::mutate(freq = n/sum(n), freq.pop = pop/sum(pop))
agrup.educ

## contagem do número de observações
absolute <- agrup %>%
  ggplot(aes(x= grau.educ, y = n)) +
  geom_text(
    data = agrup.educ,
    aes(x = grau.educ, y=n, label = paste0(format(round(100*freq,1), decimal.mark = ","),'%')), nudge_y = 14000, size = 3) +
  geom_bar(aes(fill = Cor), stat='identity') +
  scale_fill_manual(name = "", values=c("gold2","gray90","tan3","tan4","black")) +
  scale_x_discrete(labels = educ.labels) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = '', y = 'Número de Observações') +
  theme_bw() +
  theme(legend.position = 'top',
       legend.direction = 'horizontal',
       legend.box = 'horizontal',
       legend.background = element_rect(fill = NA),
       plot.caption = element_text(hjust = 0))
absolute

## relativa (em que cada barra = 100%)
relative <- agrup %>%
  ggplot(aes(x= grau.educ, y = freq, fill = Cor)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values=c("gold2","gray90","tan3","tan4","black")) +
  scale_x_discrete(labels = educ.labels) +
  scale_y_continuous(labels=function(x) format(paste0(100*x,'%'), big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Grau de Educação', y = '% de Observações',
       caption = "Fonte: Microdados da PNADC.\nO número de observações diz respeito ao total de indivíduos na amostra com idade entre 15 e 64 anos
que declararam rendimento habitual positivo no momento da entrevista.") +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))
relative

# quanto maior o nivel de educação, maior a proporção de brancos e amarelos
# also, muita gente com médio completo e fundamental incompleto
graficos <- ggarrange(absolute, relative, nrow = 2)
graficos
ggsave("obs_cor_grau_educ.pdf", dpi = 300)

## equivalentes populacionais
ggarrange(
  agrup %>%
    ggplot(aes(x= grau.educ, y = pop)) +
    geom_text(
      data = agrup.educ,
      aes(x = grau.educ, y= pop, label = paste0(format(round(100*freq.pop,1), decimal.mark = ","),'%')), nudge_y = 10000000, size = 3) +
    geom_bar(aes(fill = Cor), stat='identity') +
    scale_fill_manual(name = "", values=c("gold2","gray90","tan3","tan4","black")) +
    scale_x_discrete(labels = educ.labels) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = '', y = 'População') +
    theme_bw() +
    theme(legend.position = 'top',
          legend.direction = 'horizontal',
          legend.box = 'horizontal',
          legend.background = element_rect(fill = NA),
          plot.caption = element_text(hjust = 0))
  ,
  agrup %>%
    ggplot(aes(x= grau.educ, y = freq.pop, fill = Cor)) +
    geom_bar(stat='identity') +
    scale_fill_manual(values=c("gold2","gray90","tan3","tan4","black")) +
    scale_x_discrete(labels = educ.labels) +
    scale_y_continuous(labels=function(x) format(paste0(100*x,'%'), big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = 'Grau de Educação', y = '% da População',
         caption = "Fonte: Microdados da PNADC.\nA população entre 15 e 64 anos e com rendimento habitual positivo no momento da entrevista foi
inferida a partir dos pesos pós-estratificados disponíveis na pesquisa.") +
    theme_bw() +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0))
  , nrow = 2
)
ggsave("pop_cor_grau_educ.pdf", dpi = 300)

## Renda
# retornos não-lineares (curva exponencial)
# diferencial de raça aumenta conforme o grau educacional aumenta
# (maior dispersão das rendas)
agrup %>%
  ggplot(aes(x= grau.educ, y = renda, fill = Cor)) +
  geom_bar(stat='identity', position = position_dodge(), colour = 'white') +
  scale_fill_manual(name = "", values=c("gold2","gray90","tan3","tan4","black")) +
  scale_x_discrete(labels = educ.labels) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Grau de Educação', y = 'Renda Média (R$ 2021)',
       caption = "Fonte: Microdados da PNADC.\nO cálculo da renda média levou em conta os pesos pós-estratificados disponíveis na pesquisa.") +
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0))
ggsave("renda_cor_grau_educ.pdf", dpi = 300)

#### Análises por Gênero ####

sex.labels <- c('Feminino','Masculino')

## dataframe com algumas informações agrupadas por cor (sem divisão de ano)
sexo <- df %>%
  dplyr::group_by(Sexo) %>%
  dplyr::summarise(n=n(), renda_media = mean(sal.hab),
                   pop = sum(pesos), renda = weighted.mean(sal.hab, pesos)) %>%
  dplyr::mutate(freq = n/sum(n), freq.pop = pop/sum(pop))
# formatando as labels
sexo$Sexo <- sexo$Sexo %>%
  as.factor() %>%
  Proper()
sexo

### gráficos de contagem
## na amostra
# mais homens na força de trabalho do que mulheres
sexo %>%
  ggplot(aes(x= Sexo, y = n, fill = Sexo)) +
  geom_text(aes(label = paste0(format(round(100*freq,1), decimal.mark = ","),'%')), nudge_y = 10000, size = 3) +
  geom_bar(stat='identity') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Sexo', y = 'Número de Observações (2012, 2015, 2020 e 2021)',
       caption = "Fonte: Microdados da PNADC.\nO número de observações diz respeito ao total de indivíduos na amostra com idade entre 15 e 64 anos
que declararam rendimento habitual positivo no momento da entrevista.") +
  theme_bw() +
  theme(legend.position = "none",plot.caption = element_text(hjust = 0))
ggsave("obs_sexo.pdf",dpi=300)

## na população
sexo %>%
  ggplot(aes(x= Sexo, y = pop, fill = Sexo)) +
  geom_text(aes(label = paste0(format(round(100*freq.pop,1),decimal.mark = ","),'%')), nudge_y = 5000000, size = 3) +
  geom_bar(stat='identity') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Sexo', y = 'População (2012, 2015, 2020 e 2021)',
       caption = "Fonte: Microdados da PNADC.\nA população entre 15 e 64 anos e com rendimento habitual positivo no momento da entrevista foi
inferida a partir dos pesos pós-estratificados disponíveis na pesquisa.") +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))
ggsave("pop_sexo.pdf",dpi=300)

## Distribuições de Renda
# distribuicao de homens superior a de mulheres
ggarrange(
  df %>%
    ggplot(aes(x= Sexo, y = sal.hab, fill = Sexo)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(
      data = as.data.frame(df %>% dplyr::group_by(Sexo) %>% dplyr::summarise(value = max(sal.hab))),
      aes(y = value),
      shape = 18, size = 2
    ) +
    scale_x_discrete(labels = sex.labels) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = '', y = 'Renda (R$ 2021)') +
    theme_bw() +
    theme(legend.position = "none",plot.title = element_text(hjust = 0.5)),
  df[df$sal.hab <= 4000,] %>%
    ggplot(aes(x= Sexo, y = sal.hab, fill = Sexo)) +
    geom_boxplot(outlier.shape = NA) +
    scale_x_discrete(labels = sex.labels) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = 'Cor', y = 'Renda (R$ 2021)',
         caption = "Fonte: Microdados da PNADC.\nO gráfico superior diz respeito a toda a distribuição de renda da amostra; o inferior, por sua vez, tem sua
escala limitada em R$ 4.000 para permitir uma melhor visualização das distribuiçõe amostrais.") +
    theme_bw() +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0)),
  nrow = 2)
ggsave('boxplots_renda_sexo.pdf')

## Distribuições de Educ
# apesar de terem menor renda, mulheres possuem uma distribuição superior de educ
df %>%
  ggplot(aes(x= Sexo, y = educ, fill = Sexo)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(breaks = seq(0,16, by = 4),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Sexo', y = 'Anos de Educação',
       caption = "Fonte: Microdados da PNADC.\nDados dizem respeito apenas à distribuição da amostra.") +
  scale_x_discrete(labels = sex.labels) +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))
ggsave('boxplots_educ_sexo.pdf')

#### Sexo e Grau de Educ ####
## Contagens
agrup <- df %>%
  dplyr::group_by(grau.educ, Sexo) %>%
  dplyr::summarise(n=n(), renda_media = mean(sal.hab),
                   pop = sum(pesos), renda = weighted.mean(sal.hab, pesos)) %>%
  dplyr::mutate(freq = n/sum(n), freq.pop = pop/sum(pop))
# formatando as labels
agrup$Sexo <- agrup$Sexo %>%
  as.factor() %>%
  Proper()
agrup

## contagem do número de observações
absolute <- agrup %>%
  ggplot(aes(x= grau.educ, y = n)) +
  geom_text(
    data = agrup.educ,
    aes(x = grau.educ, y=n, label = paste0(format(round(100*freq,1), decimal.mark = ","),'%')), nudge_y = 14000, size = 3) +
  geom_bar(aes(fill = Sexo), stat='identity') +
  scale_x_discrete(labels = educ.labels) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = '', y = 'Número de Observações') +
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0))
absolute

## relativa (em que cada barra = 100%)
relative <- agrup %>%
  ggplot(aes(x= grau.educ, y = freq, fill = Sexo)) +
  geom_bar(stat='identity') +
  scale_x_discrete(labels = educ.labels) +
  scale_y_continuous(labels=function(x) format(paste0(100*x,'%'), big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Grau de Educação', y = '% de Observações',
       caption = "Fonte: Microdados da PNADC.\nO número de observações diz respeito ao total de indivíduos na amostra com idade entre 15 e 64 anos
que declararam rendimento habitual positivo no momento da entrevista.") +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))
relative

# quanto maior o nivel de educação, maior a proporção de mulheres!!!
graficos <- ggarrange(absolute, relative, nrow = 2)
graficos
ggsave("obs_sexo_grau_educ.pdf", dpi = 300)

## equivalentes populacionais
ggarrange(
  agrup %>%
    ggplot(aes(x= grau.educ, y = pop)) +
    geom_text(
      data = agrup.educ,
      aes(x = grau.educ, y= pop, label = paste0(format(round(100*freq.pop,1), decimal.mark = ","),'%')), nudge_y = 10000000, size = 3) +
    geom_bar(aes(fill = Sexo), stat='identity') +
    scale_x_discrete(labels = educ.labels) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = '', y = 'População') +
    theme_bw() +
    theme(legend.position = 'top',
          legend.direction = 'horizontal',
          legend.box = 'horizontal',
          legend.title = element_blank(),
          legend.background = element_rect(fill = NA),
          plot.caption = element_text(hjust = 0))
  ,
  agrup %>%
    ggplot(aes(x= grau.educ, y = freq.pop, fill = Sexo)) +
    geom_bar(stat='identity') +
    scale_x_discrete(labels = educ.labels) +
    scale_y_continuous(labels=function(x) format(paste0(100*x,'%'), big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = 'Grau de Educação', y = '% da População',
         caption = "Fonte: Microdados da PNADC.\nA população entre 15 e 64 anos e com rendimento habitual positivo no momento da entrevista foi
inferida a partir dos pesos pós-estratificados disponíveis na pesquisa.") +
    theme_bw() +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0))
  , nrow = 2
)
ggsave("pop_sexo_grau_educ.pdf", dpi = 300)

## Renda
# retornos não-lineares (curva exponencial)
# diferencial de genero aumenta conforme o grau educacional aumenta (apesar de as mulheres terem mais educação que os homens,
# o que sugere um efeito estrutural/discriminatório)
agrup %>%
  ggplot(aes(x= grau.educ, y = renda, fill = Sexo)) +
  geom_bar(stat='identity', position = position_dodge(), colour = 'white') +
  scale_x_discrete(labels = educ.labels) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Grau de Educação', y = 'Renda Média (R$ 2021)',
       caption = "Fonte: Microdados da PNADC.\nO cálculo da renda média levou em conta os pesos pós-estratificados disponíveis na pesquisa.") +
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0))
ggsave("renda_sexo_grau_educ.pdf", dpi = 300)

#### Análises por Cor e Sexo ####

sex.labels <- c('Mulher Branca','Homem Branco','Mulher PPI','Homem PPI')

## dataframe com algumas informações agrupadas por cor (sem divisão de ano)
sexo <- df %>%
  dplyr::group_by(Cor2.Sexo) %>%
  dplyr::summarise(n=n(), renda_media = mean(sal.hab),
                   pop = sum(pesos), renda = weighted.mean(sal.hab, pesos)) %>%
  dplyr::mutate(freq = n/sum(n), freq.pop = pop/sum(pop))
# formatando as labels
sexo$Cor2.Sexo <- sexo$Cor2.Sexo %>%
  as.factor() %>%
  Proper()
sexo

### gráficos de contagem
## na amostra
# mais homens na força de trabalho do que mulheres (especialmente analisando os PPIs)
sexo %>%
  ggplot(aes(x= Cor2.Sexo, y = n, fill = Cor2.Sexo)) +
  geom_text(aes(label = paste0(format(round(100*freq,1), decimal.mark = ","),'%')), nudge_y = 10000, size = 3) +
  geom_bar(stat='identity') +
  scale_x_discrete(labels = sex.labels) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Grupo', y = 'Número de Observações (2012, 2015, 2020 e 2021)',
       caption = "Fonte: Microdados da PNADC.\nO número de observações diz respeito ao total de indivíduos na amostra com idade entre 15 e 64 anos
que declararam rendimento habitual positivo no momento da entrevista.") +
  theme_bw() +
  theme(legend.position = "none",plot.caption = element_text(hjust = 0))
ggsave("obs_cor_sexo.pdf",dpi=300)

## na população
sexo %>%
  ggplot(aes(x= Cor2.Sexo, y = pop, fill = Cor2.Sexo)) +
  geom_text(aes(label = paste0(format(round(100*freq.pop,1),decimal.mark = ","),'%')), nudge_y = 5000000, size = 3) +
  geom_bar(stat='identity') +
  scale_x_discrete(labels = sex.labels) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Sexo', y = 'População (2012, 2015, 2020 e 2021)',
       caption = "Fonte: Microdados da PNADC.\nA população entre 15 e 64 anos e com rendimento habitual positivo no momento da entrevista foi
inferida a partir dos pesos pós-estratificados disponíveis na pesquisa.") +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))
ggsave("pop_cor_sexo.pdf",dpi=300)

## Distribuições de Renda
# distribuicao de homens superior a de mulheres e de brancos superior a de PPIs
ggarrange(
  df %>%
    ggplot(aes(x= Cor2.Sexo, y = sal.hab, fill = Cor2.Sexo)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(
      data = as.data.frame(df %>% dplyr::group_by(Cor2.Sexo) %>% dplyr::summarise(value = max(sal.hab))),
      aes(y = value),
      shape = 18, size = 2
    ) +
    scale_x_discrete(labels = sex.labels) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = '', y = 'Renda (R$ 2021)') +
    theme_bw() +
    theme(legend.position = "none",plot.title = element_text(hjust = 0.5)),
  df[df$sal.hab <= 4000,] %>%
    ggplot(aes(x= Cor2.Sexo, y = sal.hab, fill = Cor2.Sexo)) +
    geom_boxplot(outlier.shape = NA) +
    scale_x_discrete(labels = sex.labels) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = 'Grupo', y = 'Renda (R$ 2021)',
         caption = "Fonte: Microdados da PNADC.\nO gráfico superior diz respeito a toda a distribuição de renda da amostra; o inferior, por sua vez, tem sua
escala limitada em R$ 4.000 para permitir uma melhor visualização das distribuiçõe amostrais.") +
    theme_bw() +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0)),
  nrow = 2)
ggsave('boxplots_renda_cor_sexo.pdf')

## Distribuições de Educ
# distribuição superior de mulheres é concentrada entre as que se declaram branca/amarela
df %>%
  ggplot(aes(x= Cor2.Sexo, y = educ, fill = Cor2.Sexo)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(breaks = seq(0,16, by = 4),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Grupo', y = 'Anos de Educação',
       caption = "Fonte: Microdados da PNADC.\nDados dizem respeito apenas à distribuição da amostra.") +
  scale_x_discrete(labels = sex.labels) +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))
ggsave('boxplots_educ_cor_sexo.pdf')

#### Cor_Sexo e Grau de Educ ####
## Contagens
agrup <- df %>%
  dplyr::group_by(grau.educ, Cor2.Sexo) %>%
  dplyr::summarise(n=n(), renda_media = mean(sal.hab),
                   pop = sum(pesos), renda = weighted.mean(sal.hab, pesos)) %>%
  dplyr::mutate(freq = n/sum(n), freq.pop = pop/sum(pop),
                Cor2.Sexo = factor(sex.labels, ordered = TRUE, levels = sex.labels))

## contagem do número de observações
absolute <- agrup %>%
  ggplot(aes(x= grau.educ, y = n)) +
  geom_text(
    data = agrup.educ,
    aes(x = grau.educ, y=n, label = paste0(format(round(100*freq,1), decimal.mark = ","),'%')), nudge_y = 14000, size = 3) +
  geom_bar(aes(fill = Cor2.Sexo), stat='identity') +
  scale_x_discrete(labels = educ.labels) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = '', y = 'Número de Observações') +
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0))
absolute

## relativa (em que cada barra = 100%)
relative <- agrup %>%
  ggplot(aes(x= grau.educ, y = freq, fill = Cor2.Sexo)) +
  geom_bar(stat='identity') +
  scale_x_discrete(labels = educ.labels) +
  scale_y_continuous(labels=function(x) format(paste0(100*x,'%'), big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Grau de Educação', y = '% de Observações',
       caption = "Fonte: Microdados da PNADC.\nO número de observações diz respeito ao total de indivíduos na amostra com idade entre 15 e 64 anos
que declararam rendimento habitual positivo no momento da entrevista.") +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))
relative

# quanto maior o nivel de educação, maior a proporção de mulheres brancas
# homens brancos vão tomando o lugar de homens ppis
graficos <- ggarrange(absolute, relative, nrow = 2)
graficos
ggsave("obs_cor_sexo_grau_educ.pdf", dpi = 300)

## equivalentes populacionais
ggarrange(
  agrup %>%
    ggplot(aes(x= grau.educ, y = pop)) +
    geom_text(
      data = agrup.educ,
      aes(x = grau.educ, y= pop, label = paste0(format(round(100*freq.pop,1), decimal.mark = ","),'%')), nudge_y = 10000000, size = 3) +
    geom_bar(aes(fill = Cor2.Sexo), stat='identity') +
    scale_x_discrete(labels = educ.labels) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = '', y = 'População') +
    theme_bw() +
    theme(legend.position = 'top',
          legend.direction = 'horizontal',
          legend.box = 'horizontal',
          legend.title = element_blank(),
          legend.background = element_rect(fill = NA),
          plot.caption = element_text(hjust = 0))
  ,
  agrup %>%
    ggplot(aes(x= grau.educ, y = freq.pop, fill = Cor2.Sexo)) +
    geom_bar(stat='identity') +
    scale_x_discrete(labels = educ.labels) +
    scale_y_continuous(labels=function(x) format(paste0(100*x,'%'), big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = 'Grau de Educação', y = '% da População',
         caption = "Fonte: Microdados da PNADC.\nA população entre 15 e 64 anos e com rendimento habitual positivo no momento da entrevista foi
inferida a partir dos pesos pós-estratificados disponíveis na pesquisa.") +
    theme_bw() +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0))
  , nrow = 2
)
ggsave("pop_cor_sexo_grau_educ.pdf", dpi = 300)

## Renda
# retornos não-lineares (curva exponencial)
# mesmo sendo o grupo de menor educação, homens ppis superam as mulheres (brancas e PPIs)
agrup %>%
  ggplot(aes(x= grau.educ, y = renda, fill = Cor2.Sexo)) +
  geom_bar(stat='identity', position = position_dodge(), colour = 'white') +
  scale_x_discrete(labels = educ.labels) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Grau de Educação', y = 'Renda Média (R$ 2021)',
       caption = "Fonte: Microdados da PNADC.\nO cálculo da renda média levou em conta os pesos pós-estratificados disponíveis na pesquisa.") +
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0))
ggsave("renda_cor_sexo_grau_educ.pdf", dpi = 300)

#### Distribuição de lsalh pelo grau de educação ####

## distribuição no ensino superior é bem menos concentrada e com maior variação;
# como esse grupo tem rendimentos menores, essa maior dispersão tende a aumentar a desigualdade
# quando a força de trabalho se escolariza (quando há um efeito composição positivo)
# (omitiu-se o grupo dos sem instrução)
df[df$grau.educ != "Sem Instrução",] %>%
  ggplot(aes(x = lsalh, weight = pesos)) +
  geom_histogram(aes(y = ..density..)) + 
  geom_density(alpha = .1) +
  labs(x = 'Log(Salário Habitual/Hora) (R$ 2021)', y = "Densidade") +
  facet_wrap(~grau.educ) +
  xlim(-3,6) +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0))
ggsave("dist_grau_educ.pdf", dpi = 1200)

# vendo estatísticas: superior completo possui o DOBRO do desvio padrão
tapply(df$sal.hab,df$grau.educ, psych::describe)

# Raincloud visualization
df[df$grau.educ != "Sem Instrução",]  %>%
  ggplot(aes(x = grau.educ, y = lsalh, fill = grau.educ, weight = pesos)) +
  ggdist::stat_halfeye( # visualização da distribuição (metade de um violinplot)
    adjust = .3, # mais ou menos smooth
    justification = -.2,
    .width = 0, # retirando linha preta
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .2,
    alpha = .8,
    outlier.shape = NA
  ) + 
  scale_x_discrete(labels = educ.labels[-1]) +
  scale_y_continuous(
    labels=function(x) format(x, big.mark = ".", decimal.mark = ",")
  ) +
  labs(x = 'Grau de Educação', y = 'Log(Salário Habitual/Hora) (R$ 2021)') +
  ylim (0, 5) + 
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.background = element_rect(fill = NA)) + 
  coord_flip()
ggsave("raincloud_renda_grau_educ.pdf", dpi = 2400)

#### Anos ####

## dataframe com algumas informações agrupadas por cor (sem divisão de ano)
ano <- df %>%
  dplyr::group_by(Ano) %>%
  dplyr::summarise(n=n(), renda_media = mean(sal.hab),
                   pop = sum(pesos), renda = weighted.mean(sal.hab, pesos)) %>%
  dplyr::mutate(freq = n/sum(n), freq.pop = pop/sum(pop))
# formatando as labels
ano$Ano <- ano$Ano %>%
  as.factor() %>%
  Proper()
ano

ano$Ano <- factor(ano$Ano)

### gráficos de contagem
## na amostra
# taxa de ocupação muito menor em 2021 e 2020
ano %>%
  ggplot(aes(x= Ano, y = n, fill = Ano)) +
  geom_text(aes(label = paste0(format(round(100*freq,1), decimal.mark = ","),'%')), 
            nudge_y = 12000, size = 4) +
  geom_bar(stat='identity') +
  #scale_fill_manual(values=c("lightblue","dodgerblue","dodgerblue4","blue4")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Ano', y = 'Número de Observações') +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.background = element_rect(fill = NA))
ggsave("obs_ano.pdf", dpi=1200)

## na população
# dada a menor amostra, atribuiu-se maior peso as observações colhidas em 2021
# o que ameniza o menor número de observações neste ano (e em 2020 também)
ano %>%
  ggplot(aes(x= Ano, y = pop, fill = Ano)) +
  geom_text(aes(label = paste0(format(round(100*freq.pop,1),decimal.mark = ","),'%')), 
            nudge_y = 5000000, size = 4) +
  geom_bar(stat='identity') +
  #scale_fill_manual(values=c("lightblue","dodgerblue","dodgerblue4","blue4")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Ano', y = 'População') +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.background = element_rect(fill = NA))
ggsave("pop_ano.pdf", dpi = 1200)

## Distribuições de Renda
# distribuicao de 2020 e 2021 "igual", mas mediana deste ano caiu, indicando aumento da renda dos mais ricos
# (ou que os mais pobres saíram da força de trabalho, o que faz sentido dada a natureza da crise)
# estagnação da renda real desde 20215
ggarrange(
  df %>%
    ggplot(aes(x= factor(Ano), y = sal.hab)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(
      data = as.data.frame(df %>% dplyr::group_by(Ano) %>% dplyr::summarise(value = max(sal.hab))),
      aes(y = value),
      shape = 18, size = 2
    ) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = '', y = 'Renda (R$ 2021)') +
    theme_bw() +
    theme(legend.position = "none",plot.title = element_text(hjust = 0.5)),
  df[df$sal.hab <= 4000,] %>%
    ggplot(aes(x= factor(Ano), y = sal.hab)) +
    geom_boxplot(outlier.shape = NA) +
    scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
    labs(x = 'Ano', y = 'Renda (R$ 2021)',
         caption = "Fonte: Microdados da PNADC.\nO gráfico superior diz respeito a toda a distribuição de renda da amostra; o inferior, por sua vez, tem sua
escala limitada em R$ 4.000 para permitir uma melhor visualização das distribuiçõe amostrais.") +
    theme_bw() +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0)),
  nrow = 2)
ggsave('boxplots_renda_ano.pdf')

#### Distribuições de Educ e Exper por Ano ####
## Boxplots de educ
# força de trabalho foi ficando mais escolarizada ao longo dos anos (efeito composição)
# distribuição de 2021 ligeiramente superior a de 2020, mas com a mesma mediana
df %>%
  ggplot(aes(x = factor(Ano), y = educ, fill = factor(Ano))) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(breaks = seq(0,16, by = 4),
                     labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Ano', y = 'Anos de Educação') + 
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.background = element_rect(fill = NA))
ggsave('boxplots_educ_ano.pdf')

# Raincloud visualization
df %>%
  ggplot(aes(x = factor(Ano), y = educ, fill = factor(Ano))) +
  ggdist::stat_halfeye( # visualização da distribuição (metade de um violinplot)
    adjust = .3, # mais ou menos smooth
    justification = -.2,
    .width = 0, # retirando linha preta
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .2,
    alpha = .8,
    outlier.shape = NA
  ) + 
  # trocando a ordem do eixo
  scale_x_discrete(limits = rev(levels(factor(df$Ano)))) + 
  scale_y_continuous(
    breaks = seq(0,16, by = 4),
    labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)
  ) +
  labs(x = 'Cor', y = 'Anos de Educação') +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.background = element_rect(fill = NA)) + 
  coord_flip()
ggsave("raincloud_educ_ano.pdf", dpi = 2400)

## Distplot de Educação
# 2020 e 2021 possuem uma força de trabalho mais educada
df %>%
  ggplot(aes(x = educ, colour = factor(Ano))) +
  # geom_density(alpha = .1) +
  stat_ecdf() +
  #scale_colour_manual(values=c("lightblue","dodgerblue","dodgerblue4","blue4")) +
  scale_x_continuous(breaks = seq(0,16, by = 4)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Anos de Educação', y = "Densidade Acumulada") +
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12))
ggsave('distacum_educ_ano.pdf', dpi = 2400)

## Boxplots de exper
# força de trabalho foi ficando ligeiramente mais experiente
df %>%
  ggplot(aes(x = factor(Ano), y = exper)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Ano', y = 'Anos de Experiência Potencial',
       caption = "Fonte: Microdados da PNADC.\nDados dizem respeito apenas à distribuição da amostra.") +
  theme_bw() +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0))
ggsave('boxplots_exper_ano.pdf')

## Distplot de Exper
# força de trabalho foi ficando ligeiramente mais experiente
df %>%
  ggplot(aes(x = exper, colour = factor(Ano))) +
  stat_ecdf() +
  #scale_colour_manual(values=c("lightblue","dodgerblue","dodgerblue4","blue4")) +
  scale_x_continuous(breaks = seq(0,50, by = 5)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Anos de Experiência Potencial', y = "Densidade Acumulada") +
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 11),
        legend.text = element_text(size = 12))
ggsave('distacum_exper_ano.pdf', dpi = 2400)

#### Ano e Grau de Educ ####
## Contagens
agrup <- df %>%
  dplyr::group_by(Ano, grau.educ) %>%
  dplyr::summarise(n=n(), renda_media = mean(sal.hab),
                   pop = sum(pesos), renda = weighted.mean(sal.hab, pesos)) %>%
  dplyr::mutate(freq = n/sum(n), freq.pop = pop/sum(pop))
agrup

agrup2 <- df %>%
  dplyr::group_by(grau.educ, Ano) %>%
  dplyr::summarise(n=n(), renda_media = mean(sal.hab),
                   pop = sum(pesos), renda = weighted.mean(sal.hab, pesos)) %>%
  dplyr::mutate(freq = n/sum(n), freq.pop = pop/sum(pop))
agrup2

## contagem relativa (em que cada barra = 100%)
# ao longo do tempo, mais pessoas cumpriram o ensino superior
relative <- agrup %>%
  ggplot(aes(x = factor(Ano), y = freq, fill = grau.educ)) +
  geom_bar(stat='identity') +
  scale_y_continuous(labels=function(x) format(paste0(100*x,'%'), big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Ano', y = '% de Observações',
       caption = "Fonte: Microdados da PNADC.\nO número de observações diz respeito ao total de indivíduos na amostra com idade entre 15 e 64 anos
que declararam rendimento habitual positivo no momento da entrevista.") +
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0))
relative
ggsave("obs_grau_educ_ano.pdf", dpi = 300)

## equivalentes populacionais
relative <- agrup %>%
  ggplot(aes(x = factor(Ano), y = freq.pop, fill = grau.educ)) +
  geom_bar(stat='identity') +
  scale_y_continuous(labels=function(x) format(paste0(100*x,'%'), big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Ano', y = '% da População',
       caption = "Fonte: Microdados da PNADC.\nA população entre 15 e 64 anos e com rendimento habitual positivo no momento da entrevista foi
inferida a partir dos pesos pós-estratificados disponíveis na pesquisa.") +
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0))
relative
ggsave("pop_grau_educ_ano.pdf", dpi = 300)

## Renda
# retornos não-lineares (curva exponencial)
# como mais e mais pessoas estão se qualificando mais, o prêmio pago pelo mercado diminui ao longo do tempo
agrup2 %>%
  ggplot(aes(x= grau.educ, y = renda, fill = factor(Ano))) +
  geom_bar(stat='identity', position = position_dodge(), colour = 'white') +
  scale_x_discrete(labels = educ.labels) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = F)) +
  labs(x = 'Grau de Educação', y = 'Renda Média (R$ 2021)',
       caption = "Fonte: Microdados da PNADC.\nO cálculo da renda média levou em conta os pesos pós-estratificados disponíveis na pesquisa.") +
  theme_bw() +
  theme(legend.position = 'top',
        legend.direction = 'horizontal',
        legend.box = 'horizontal',
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA),
        plot.caption = element_text(hjust = 0))
ggsave("renda_ano_grau_educ.pdf", dpi = 300)

#### Tabela de Médias por Ano ####
## Se precisar mudar a tabela para considerar apenas a amostra, rodar:
# df$pesos <- 1

medias <- df %>%
  group_by(Ano) %>%
  summarise(
    "Renda.Hab" = weighted.mean(sal.hab, pesos),
    "log(Renda Hab.)" = weighted.mean(lsal, pesos),
    "Renda.Hab.Hora" = weighted.mean(sal.hab.hora, pesos),
    "log(Renda Hab./Hora)" = weighted.mean(lsalh, pesos),
    "Renda.Efet" = weighted.mean(sal.efet, pesos),
    "log(Renda Efet.)" = weighted.mean(lsal.efet, pesos),
    "Renda.Efet.Hora" = weighted.mean(sal.efet.hora[!is.na(sal.efet.hora)], 
                                      pesos[!is.na(sal.efet.hora)]),
    "log(Renda Efet./Hora)" = weighted.mean(lsalh.efet[!is.na(lsalh.efet)], 
                                            pesos[!is.na(lsalh.efet)]),
    "Educação" = weighted.mean(educ, pesos),
    "Experiência Potencial" = weighted.mean(exper, pesos)
  ) %>%
  t() %>% # transpondo
  as.data.frame() %>% # colocando como df normal (e não grouped)
  mutate(across(everything(), round, 2)) # arredondando

## deixando os anos no cabeçalho e excluindo a sua linha
names(medias) <- medias[1,]
medias <- medias[-1,]
medias

### Sexo
categ <- df %>%
  group_by(Ano, Sexo) %>%
  summarise(pop = sum(pesos)) %>%
  mutate(freq.pop = pop/sum(pop)) %>%
  pivot_wider(id_cols = Sexo, names_from = Ano, values_from = freq.pop) %>%
  as.data.frame() # colocando como df normal (e não grouped)

## deixando os nomes das variáveis no index
row.names(categ) <- paste(toupper(substring(categ$Sexo,1,1)), substring(categ$Sexo,2), sep = "")
categ <- categ[-1]
categ <- mutate(categ, across(everything(), round, 3)) # arredondando

## union
medias <- rbind(medias, categ)

### Sexo com filhos
df$Sexo.filhos <- ifelse(
  df$Sexo == "masculino" & df$num.filhos05 == 0,
  "masculino0",
  ifelse(
    df$Sexo == "feminino" & df$num.filhos05 == 0,
    "feminino0",
    ifelse(
      df$Sexo == "masculino" & df$num.filhos05 > 0,
      "masculino.filhos",
      "feminino.filhos"
    )
  )
)

categ <- df %>%
  group_by(Ano, Sexo.filhos) %>%
  summarise(pop = sum(pesos)) %>%
  mutate(freq.pop = pop/sum(pop)) %>%
  pivot_wider(id_cols = Sexo.filhos, names_from = Ano, values_from = freq.pop) %>%
  as.data.frame() # colocando como df normal (e não grouped)

## deixando os nomes das variáveis no index
row.names(categ) <- paste(toupper(substring(categ$Sexo,1,1)), substring(categ$Sexo,2), sep = "")
categ <- categ[-1]
categ <- mutate(categ, across(everything(), round, 3)) # arredondando

## union
medias <- rbind(medias, categ)

### Cor2
categ <- df %>%
  group_by(Ano, Cor2) %>%
  summarise(pop = sum(pesos)) %>%
  mutate(freq.pop = pop/sum(pop)) %>%
  pivot_wider(id_cols = Cor2, names_from = Ano, values_from = freq.pop) %>%
  as.data.frame() # colocando como df normal (e não grouped)

## deixando os nomes das variáveis no index
row.names(categ) <- paste(toupper(substring(categ$Cor2,1,1)), substring(categ$Cor2,2), sep = "")
categ <- categ[-1]
categ <- mutate(categ, across(everything(), round, 3)) # arredondando
categ

## union
medias <- rbind(medias, categ)

### Rural
categ <- df %>%
  group_by(Ano, rural) %>%
  summarise(pop = sum(pesos)) %>%
  mutate(freq.pop = pop/sum(pop)) %>%
  pivot_wider(id_cols = rural, names_from = Ano, values_from = freq.pop) %>%
  as.data.frame() # colocando como df normal (e não grouped)

## deixando os nomes das variáveis no index
row.names(categ) <- paste(toupper(substring(categ$rural,1,1)), substring(categ$rural,2), sep = "")
categ <- categ[-1]
categ <- mutate(categ, across(everything(), round, 3)) # arredondando
categ

## union
medias <- rbind(medias, categ)

### Formalidade
categ <- df %>%
  group_by(Ano, formalidade) %>%
  summarise(pop = sum(pesos)) %>%
  mutate(freq.pop = pop/sum(pop)) %>%
  pivot_wider(id_cols = formalidade, names_from = Ano, values_from = freq.pop) %>%
  as.data.frame() # colocando como df normal (e não grouped)

## deixando os nomes das variáveis no index
row.names(categ) <- paste(toupper(substring(categ$formalidade,1,1)), substring(categ$formalidade,2), sep = "")
categ <- categ[-1]
categ <- mutate(categ, across(everything(), round, 3)) # arredondando
categ

## union
medias <- rbind(medias, categ)

### Regiao
categ <- df %>%
  group_by(Ano, regiao) %>%
  summarise(pop = sum(pesos)) %>%
  mutate(freq.pop = pop/sum(pop)) %>%
  pivot_wider(id_cols = regiao, names_from = Ano, values_from = freq.pop) %>%
  as.data.frame() # colocando como df normal (e não grouped)

## deixando os nomes das variáveis no index
row.names(categ) <- categ$regiao
categ <- categ[-1]
categ <- mutate(categ, across(everything(), round, 3)) # arredondando
categ

## union
medias <- rbind(medias, categ)

### Setor
categ <- df %>%
  group_by(Ano, setor.simp) %>%
  summarise(pop = sum(pesos)) %>%
  mutate(freq.pop = pop/sum(pop)) %>%
  pivot_wider(id_cols = setor.simp, names_from = Ano, values_from = freq.pop) %>%
  as.data.frame() # colocando como df normal (e não grouped)

## deixando os nomes das variáveis no index
row.names(categ) <- paste(toupper(substring(categ$setor.simp,1,1)), substring(categ$setor.simp,2), sep = "")
categ <- categ[-1]
categ <- mutate(categ, across(everything(), round, 3)) # arredondando
categ

## union
medias <- rbind(medias, categ)

###### MÉDIAS POR ANO ####

# Renda média aumentou principalmente entre 2012 e 2015, mas estagnou desde então
# Força de Trabalho cada vez mais escolarizada (sendo que a média chega perto do ensino médio em 2021,
# o que pode estar ocorrendo em virtude do efeito da crise sobre os mais desfavorecidos)
# Há uma maior participação feminina, mas que parece ter se estagnado no último ano; mesma análise para PPIs
# SALTO DE INFORMALIDADE DESDE 2015 (e em 2021 também), contrariando a tendência vista nos dados de FERREIRA, 2021 para
# o período de 2003-2012. Essa tendência segue o aumento dos empregos no setor de serviços
# - o mais afetado pela crise da Covid-19
medias

## exportando para o latex
stargazer(medias, summary = F, decimal.mark = ",", digit.separator = ".")

#### Fórmulas e Categorias-Base ####
## formula1 (com setores de atividade)
formula.setor <- lsalh ~ educ + educ2 + educ3 + educ4 + exper + exper2 + exper3 + exper4 + Sexo + Cor2 + rural + formalidade + regiao + setor
formula.setor21 <-  lsalh.efet ~ educ + educ2 + educ3 + educ4 + exper + exper2 + exper3 + exper4 + Sexo + Cor2 + rural + formalidade + regiao + setor

## arrumando as categorias-base
# Sexo
df.svy$variables <- df.svy$variables %>%
  mutate(Sexo = relevel(Sexo, ref = "masculino"))
df <- df %>%
  mutate(Sexo = relevel(Sexo, ref = "masculino"))

# Cor2
df.svy$variables <- df.svy$variables %>%
  mutate(Cor2 = relevel(Cor2, ref = "branca"))
df <- df %>%
  mutate(Cor2 = relevel(Cor2, ref = "branca"))

# Rural
df.svy$variables <- df.svy$variables %>%
  mutate(rural = relevel(rural, ref = "urbano"))
df <- df %>%
  mutate(rural = relevel(rural, ref = "urbano"))

# Formalidade
df.svy$variables <- df.svy$variables %>%
  mutate(formalidade = relevel(formalidade, ref = "informal"))
df <- df %>%
  mutate(formalidade = relevel(formalidade, ref = "informal"))

# Regiao
df.svy$variables <- df.svy$variables %>%
  mutate(regiao = relevel(regiao, ref = "CO"))
df <- df %>%
  mutate(regiao = relevel(regiao, ref = "CO"))

# Setor
df.svy$variables <- df.svy$variables %>%
  mutate(setor = relevel(setor, ref = "servicos.fin"))
df <- df %>%
  mutate(setor = relevel(setor, ref = "servicos.fin"))

#### Regressões (OLSs) por ano ####
## modelos para 2012
ols2012.setor <- lm(formula.setor, subset(df, Ano == 2012), weights = pesos)
cov <- vcovHC(ols2012.setor, type = "HC1")
robust.se.ols2012.setor <- sqrt(diag(cov))

## modelos para 2015
ols2015.setor <- lm(formula.setor, subset(df, Ano == 2015), weights = pesos)
cov <- vcovHC(ols2015.setor, type = "HC1")
robust.se.ols2015.setor <- sqrt(diag(cov))

## modelos para 2020
ols2020.setor <- lm(formula.setor, subset(df, Ano == 2020), weights = pesos)
cov <- vcovHC(ols2020.setor, type = "HC1")
robust.se.ols2020.setor <- sqrt(diag(cov))

ols2020.setor.efet <- lm(formula.setor21, subset(df, Ano == 2020), weights = pesos)
cov <- vcovHC(ols2020.setor.efet, type = "HC1")
robust.se.ols2020.setor.efet <- sqrt(diag(cov))

## modelos para 2021
ols2021.setor <- lm(formula.setor21, subset(df, Ano == 2021), weights = pesos)
cov <- vcovHC(ols2021.setor, type = "HC1")
robust.se.ols2021.setor <- sqrt(diag(cov))

## Tabelas do stargazer para o latex
# Modelos Setoriais
stargazer(ols2012.setor, ols2015.setor, ols2020.setor, ols2020.setor.efet, ols2021.setor, 
          title = "Comparação Anual dos Modelos Simples: Equação Minceriana",
          se = list(robust.se.ols2012.setor, robust.se.ols2015.setor,
                    robust.se.ols2020.setor, robust.se.ols2020.setor.efet,
                    robust.se.ols2021.setor),
          decimal.mark = ",", digit.separator = ".",
          align = T, no.space = T, column.sep.width = "2pt")

###### Retorno da educação e da experiência ao longo dos anos ####
r.setor <- rbind(ols2012.setor$coefficients[2:9], 
                ols2015.setor$coefficients[2:9], 
                ols2020.setor$coefficients[2:9], 
                ols2020.setor.efet$coefficients[2:9], 
                ols2021.setor$coefficients[2:9])
rownames(r.setor) <- c("2012", "2015", "2020", "2020E", "2021E")

## Calculando os retornos por ano
retornos <- as.data.frame(
  rbind(
    cbind("2012", 'Educação', seq(0,16, 1), r.setor[1,1], r.setor[1,2], r.setor[1,3], r.setor[1,4]),
    cbind("2015", 'Educação', seq(0,16, 1), r.setor[2,1], r.setor[2,2], r.setor[2,3], r.setor[2,4]),
    cbind("2020", 'Educação', seq(0,16, 1), r.setor[3,1], r.setor[3,2], r.setor[3,3], r.setor[3,4]),
    cbind("2020E", 'Educação', seq(0,16, 1), r.setor[4,1], r.setor[4,2], r.setor[4,3], r.setor[4,4]),
    cbind("2021E", 'Educação', seq(0,16, 1), r.setor[5,1], r.setor[5,2], r.setor[5,3], r.setor[5,4]),
    cbind("2012", 'Experiência', seq(0,50, 1), r.setor[1,5], r.setor[1,6], r.setor[1,7], r.setor[1,8]),
    cbind("2015", 'Experiência', seq(0,50, 1), r.setor[2,5], r.setor[2,6], r.setor[2,7], r.setor[2,8]),
    cbind("2020", 'Experiência', seq(0,50, 1), r.setor[3,5], r.setor[3,6], r.setor[3,7], r.setor[3,8]),
    cbind("2020E", 'Experiência', seq(0,50, 1), r.setor[4,5], r.setor[4,6], r.setor[4,7], r.setor[4,8]),
    cbind("2021E", 'Experiência', seq(0,50, 1), r.setor[5,5], r.setor[5,6], r.setor[5,7], r.setor[5,8])
  )
)

# alterando os nomes e os tipos das colunas
names(retornos) <- c('Ano','Variável','Anos','var','var2','var3','var4')
cols <- c('Anos','var','var2','var3','var4')
retornos[cols] <- sapply(retornos[cols],as.numeric)

# Calculando os Retornos
retornos <- retornos %>% 
  mutate(Retornos = Anos*var + Anos**2*var2 + 
           Anos**3*var3 + Anos**4*var4)

## Plottando
# Retorno da educação é não-linear (há um grande prêmio a partir do EM/ES completo)
# Como a FT tem ficado cada vez mais escolarizada, o prêmio educacional tem diminuído

# Função concâva que começa a declinar a partir dos ≈ 40 anos de experiência (≈55 de idade)
# (o que vai de encontro com a teoria do ciclo de vida de Modigliani!)
retornos %>% 
  ggplot(aes(x = Anos, y = Retornos, colour = Ano)) +
  geom_line(size = .75) +
  labs(x = 'Anos de Educação/Experiência', y = 'Retorno para a log(Renda/Hora)') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  facet_wrap(~Variável, scales = 'free') + 
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )
ggsave("retornos_educ_exper_hora.pdf", dpi = 2400)

## Plots separados

# Educação
retornos[retornos$Variável == "Educação", ] %>% 
  ggplot(aes(x = Anos, y = Retornos, colour = Ano)) +
  geom_line(size = .75) +
  labs(x = 'Anos de Educação', y = 'Retorno para log(Renda/Hora)') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )
ggsave("retornos_educ_hora.pdf", dpi = 2400)

# Experiência
retornos[retornos$Variável == "Experiência", ] %>% 
  ggplot(aes(x = Anos, y = Retornos, colour = Ano)) +
  geom_line(size = .75) +
  labs(x = 'Anos de Experiência', y = 'Retorno para log(Renda/Hora)') +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 10)
  )
ggsave("retornos_exper_hora.pdf", dpi = 2400)

#### Regressões (SVYGLMs) por ano ####
## modelos para 2012
mod2012.setor <- svyglm(formula.setor, subset(df.svy, Ano == 2012))

## modelos para 2015
mod2015.setor <- svyglm(formula.setor, subset(df.svy, Ano == 2015))

## modelos para 2020
mod2020.setor <- svyglm(formula.setor, subset(df.svy, Ano == 2020))

## modelos para 2021
mod2021.setor <- svyglm(formula.setor, subset(df.svy, Ano == 2021))

## Exportando para o Latex
# Modelos Setoriais
stargazer(mod2012.setor, mod2015.setor, mod2020.setor, mod2021.setor,
          title = "Comparação Anual dos Modelos Ponderados: Equação Minceriana",
          decimal.mark = ",", digit.separator = ".",
          align = T, no.space = T)

#### Decomposições (RIFs FFL/Oaxaca) ###################
### Aqui, usaremos RIF e, para a decomposição, usaremos o pacote oaxaca
### Como uma regressão RIF é apenas uma OLS de X sobre RIF, podemos encontrar as
### decomposições (para a AMOSTRA) usando a RIF como variável dependente na funçào
### oaxaca, encontrando, assim, o efeito composição e estrutural.
### (isso funciona porque a esperança de uma IF é 0; como a RIF é definida usando
### v(Y) + IF(y, v(Y)), onde v(Y) é a função de interesse, a esperança de uma RIF é v(Y))

## Para isso, usaremos a two-fold decomposition (onde explained = composição e
## unexplained = estrutural) de Neumark (group.weight = -1, ou seja, a referência
## é uma pooled OLS com os dois anos).

## Antes, porém, é preciso estimar a RIF de cada estatística PARA CADA ANO SEPARADAMENTE.
## Queremos encontrar as seguintes estatísticas:
# Renda Média (oaxaca normal)
# Gini
# p1
# p10
# p50
# p90
# p99

## As estimativas foram feitas no Stata por comodidade e permitir o uso de um
## desenho de pesquisa amostral :). 

## Caso se queira fazer o mesmo exercício sem os pesos, basta calcular a respectiva RIF
## usando a função rif da biblioteca dineq. Apesar de ela permitir o uso de pesos,
## a função oaxaca não permite.

## Para comparação, aqui estima-se a decomposição da renda media da amostra entre 2012 e 2015
df1215 <- df[df$Ano %in% c(2012,2015),]

## Apagando o df.svy da memória
rm(df.svy)

## Coluna para identificar os grupos; 2012 será o grupo B
df1215$doze <- ifelse(df1215$Ano == 2012, T, F)

oaxaca1215 <- oaxaca(formula = lsalh ~ educ + educ2 + educ3 + educ4 + 
                      + exper + exper2 + exper3 + exper4 + 
                      + Sexo + Cor2 + rural + formalidade + regiao | doze,
                     data  = df1215, R = 100)

## vendo a média por ano
oaxaca1215$y

## vendo os resultados agregados (explained = composição; unexplained = estrutural)
# estamos interessados na linha de group.weight = -1
oaxaca1215$twofold$overall

plot(oaxaca1215, decomposition = "twofold", group.weight = -1, unexplained.split = F)
