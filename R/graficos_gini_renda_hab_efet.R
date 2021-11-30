#### SCRIPT QUE PLOTTA GRÁFICOS DE GINI E RENDAS MÉDIAS PRO PERÍODO 12-21
#### SEM TER QUE CALCULÁ-LOS COM O PACOTE SURVEY (dados pré-calculados
#### usando os scripts gini_efet e gini_hab)

library(tidyverse)
library(magrittr)
library(xlsx)

#para apagar todas as variáveis
rm(list = ls())
# Clear plots
dev.off()
# Clear console
cat("\014")

## mudando o diretório (caso seja necessário)
getwd()
setwd("/Users/vinicius/Desktop/Artigos/PET/Artigo_PNAD")
# setwd("/Users/vinicius/Desktop/Artigo_PNAD")

#### FUNÇÃO DOS GRÁFICOS ####

#### Gini ####

## Lendo o arquivo
sheets <- c("gini_efet","renda_efet","renda_pc_efet",
            "gini_hab","renda_hab","renda_pc_hab")

# Gini dos Rendimentos Efetivos
gini_efet <- read.xlsx(file = "estatisticas_rendas_ginis.xlsx", 
                       sheetName = "gini_efet", as.data.frame = TRUE)

gini_efet$rendimento <- "Rendimentos Efetivos"

# Gini dos Rendimentos Habituais
gini_hab <- read.xlsx(file = "estatisticas_rendas_ginis.xlsx", 
                      sheetName = "gini_hab", as.data.frame = TRUE)

gini_hab$rendimento <- "Rendimentos Habituais"

## Juntando
gini <- rbind(gini_efet, gini_hab)

gini$Ano <- gini$Ano - 2000

## Plottando
gini %>% 
  ggplot(aes(x = Ano)) +
  geom_point(
    aes(y = value, colour = variable), 
    shape = 18, size = 2
  ) +
  geom_text(
    data = gini[gini$Ano %in% c(18),],
    mapping = aes(y = value, label = format(round(value,3), decimal.mark = ",")), 
    nudge_y = 0.01, size = 3
  ) +
  geom_text(
    data = gini[gini$Ano %in% c(15, 20),],
    mapping = aes(y = value, label = format(round(value,3), decimal.mark = ",")), 
    nudge_y = -0.01, size = 3
  ) +
  geom_text(
    data = gini[gini$Ano == 12,],
    mapping = aes(y = value, label = format(round(value,3), decimal.mark = ",")), 
    nudge_y = 0.01, nudge_x = .15, size = 3
  ) +
  geom_text(
    data = gini[gini$Ano == 21,],
    mapping = aes(y = value, label = format(round(value,3), decimal.mark = ",")), 
    nudge_y = 0.01, nudge_x = -.15, size = 3
  ) +
  geom_line(aes(y = value, colour = variable), size = 1) +
  geom_ribbon(aes(ymin = ci.l, ymax = ci.u, fill = variable), alpha = 0.3, show.legend = F) +
  scale_colour_manual(name = "",
                      labels = c('Renda do Trabalho',"RTDPC"),
                      values = c("black","gray40")) + 
  scale_fill_manual(values=c("black","gray40")) + 
  labs(x = 'Ano', y = 'Gini') +
  scale_x_continuous(breaks = seq(12,21, by = 1)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.background = element_rect(fill = NA)
  ) + 
  facet_grid(cols = vars(rendimento), scales = 'free_y', switch = 'y')
ggsave("gini_hab_efet.pdf", dpi = 2400)

#### Renda Individual do Trabalho ####

## Lendo o arquivo
# renda dos Rendimentos Efetivos
renda_efet <- read.xlsx(file = "estatisticas_rendas_ginis.xlsx", 
                       sheetName = "renda_efet", as.data.frame = TRUE)

renda_efet$rendimento <- "Rendimentos Efetivos"

# renda dos Rendimentos Habituais
renda_hab <- read.xlsx(file = "estatisticas_rendas_ginis.xlsx", 
                      sheetName = "renda_hab", as.data.frame = TRUE)

renda_hab$rendimento <- "Rendimentos Habituais"

## Juntando
renda <- rbind(renda_efet, renda_hab)
renda$Ano <- renda$Ano - 2000

## Plottando
renda %>% 
  ggplot(aes(x=Ano)) +
  geom_point(
    aes(y = value, colour = variable), 
    shape = 18, size = 2
  ) +
  geom_text(
    data = renda[renda$Ano %in% c(12, 15, 18, 20),],
    mapping = aes(y = value, label = format(round(value, 0), decimal.mark = ",")), 
    nudge_y = 90, size = 3
  ) +
  geom_text(
    data = renda[renda$Ano == 21,],
    mapping = aes(y = value, label = format(round(value, 0), decimal.mark = ",")), 
    nudge_y = -60, size = 3
  ) +
  geom_line(aes(y = value, colour = variable), size = 1) +
  geom_ribbon(aes(ymin = ci.l, ymax = ci.u, fill = variable), alpha = 0.3, show.legend = F) +
  scale_colour_manual(name = "",
                      labels = c('Média',"Mediana"),
                      values = c("black","gray40")) + 
  scale_fill_manual(values=c("black","gray40")) + 
  labs(x = 'Ano', y = 'Renda do Trabalho (R$ 2021)') +
  scale_x_continuous(breaks = seq(12,21, by = 1)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.background = element_rect(fill = NA)
  ) + 
  facet_grid(cols = vars(rendimento), scales = 'free_y', switch = 'y')
ggsave("renda_hab_efet.pdf", dpi = 2400)


#### Renda do Trabalho Domiciliar per Capita ####
# Renda Domiciliar per Capita dos Rendimentos Efetivos
renda_pc_efet <- read.xlsx(file = "estatisticas_rendas_ginis.xlsx", 
                        sheetName = "renda_pc_efet", as.data.frame = TRUE)

renda_pc_efet$rendimento <- "Rendimentos Efetivos"

# Renda Domiciliar per Capita dos Rendimentos Habituais
renda_pc_hab <- read.xlsx(file = "estatisticas_rendas_ginis.xlsx", 
                       sheetName = "renda_pc_hab", as.data.frame = TRUE)

renda_pc_hab$rendimento <- "Rendimentos Habituais"

## Juntando
renda.pc <- rbind(renda_pc_efet, renda_pc_hab)
renda.pc$Ano <- renda.pc$Ano - 2000

## Plottando
renda.pc %>% 
  ggplot(aes(x=Ano)) +
  geom_point(
    aes(y = value, colour = variable), 
    shape = 18, size = 2
  ) +
  geom_text(
    data = renda.pc[renda.pc$Ano %in% c(12, 15, 18, 20),],
    mapping = aes(y = value, label = format(round(value, 0), decimal.mark = ",")), 
    nudge_y = 40, size = 3
  ) +
  geom_text(
    data = renda.pc[renda.pc$Ano == 21,],
    mapping = aes(y = value, label = format(round(value, 0), decimal.mark = ",")), 
    nudge_y = -30, size = 3
  ) +
  geom_line(aes(y = value, colour = variable), size = 1) +
  geom_ribbon(aes(ymin = ci.l, ymax = ci.u, fill = variable), alpha = 0.3, show.legend = F) +
  scale_colour_manual(name = "",
                      labels = c('Média',"Mediana"),
                      values = c("black","gray40")) + 
  scale_fill_manual(values=c("black","gray40")) + 
  labs(x = 'Ano', y = 'Renda do Trabalho Domiciliar per Capita (R$ 2021)') +
  scale_x_continuous(breaks = seq(12,21, by = 1)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    legend.background = element_rect(fill = NA)
  ) + 
  facet_grid(cols = vars(rendimento), scales = 'free_y', switch = 'y')
ggsave("renda_hab_efet_pc.pdf", dpi = 2400)
