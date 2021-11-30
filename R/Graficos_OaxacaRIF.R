## SCRIPT QUE, A PARTIR DAS ESTIMAÇÕES NO STATA PARA AS DECOMPOSIÇÕES EM CADA
## PERCENTIL, ELABORA GRÁFICOS PARA MELHOR VISUALIZAÇÃO

## Importante: os dados foram gerados a partir do comando esttab do STATA e, por
## isso, precisaram ser tratados manualmente no Excel para ficarem no formato
## adequado de tabela (como números e sem linhas em branco)

## Pacotes Necessários ####
library(tidyverse)
library(tidylog, warn.conflicts = F)
library(magrittr)
library(xlsx)
library(data.table)
library(stringr)
library(wesanderson)

#para apagar todas as variáveis
rm(list = ls())
# Clear plots
dev.off()
# Clear console
cat("\014")

## mudando o diretório (caso seja necessário)
getwd()
setwd("/Users/vinicius/Desktop/Artigos/PET/Artigo_PNADC_RIF")

## Percentis ####

## Lendo o arquivo
file <- "Decomp_Percentis_Hora.xlsm" # _Mes para rendas mensais; _Hora c.c.
sheet <- "R_2020_2021efet CI" # ou R_2020_2021hab para 2020-21 habituais
percentis <- read.xlsx(file = file, 
                       sheetName = sheet, as.data.frame = TRUE)

## Pivotando
percentis %<>% 
  pivot_longer(!c(label, effect, years), names_to = "quantil", values_to = 'values')

## Formatando os quantis e os anos
percentis$quantil <- as.integer(substring(percentis$quantil, 2))
percentis$years <- gsub("_", "-", percentis$years)

## colocando os anos como fatores
percentis$years <- factor(percentis$years, 
                          levels = c('2012-2015','2015-2020','2012-2020','2020-2021'))

## Criando um df sem os efeitos gerais (e que vai servir pros gráficos)
perc.gg <- percentis[percentis$effect != 'Geral',]
perc.geral <- percentis[percentis$effect == 'Geral' 
                        & !(percentis$label %in% c('group_1','group_2')),]

## Modificando o dataframe geral caso haja intervalos de confiança
if (sheet == "R_2020_2021efet CI"){
  #### SPECIFIC DECOMPOSITON
  perc.gg.values <- percentis[percentis$effect != 'Geral'
                       & !(percentis$label %like% "ci"), ]
  # Lower CIs
  perc.gg.lower <- percentis[percentis$effect != 'Geral'
                              & (percentis$label %like% "lower_ci"), ]
  
  # Matching labels
  perc.gg.lower$label <- str_sub(perc.gg.lower$label, 1, -10)
  
  # Upper CIs
  perc.gg.upper <- percentis[percentis$effect != 'Geral'
                             & (percentis$label %like% "upper_ci"), ]
  
  # Matching labels
  perc.gg.upper$label <- str_sub(perc.gg.upper$label, 1, -10)
  
  # Changing Column Names
  colnames(perc.gg.lower)[5] <- "lower.ci"
  colnames(perc.gg.upper)[5] <- "upper.ci"
  
  # Joining
  perc.gg <- left_join(perc.gg.values, perc.gg.lower, 
                          by=c("label", "effect", "years", "quantil"))
  perc.gg <- left_join(perc.gg, perc.gg.upper, 
                          by=c("label", "effect", "years", "quantil"))
  
  
  #### GENERAL DECOMPOSITION
  perc.geral.values <- percentis[percentis$effect == 'Geral' 
                                 & !(percentis$label %in% c('group_1','group_2'))
                                 & !(percentis$label %like% "ci"),]
  # Lower CIs
  perc.geral.lower <- percentis[percentis$effect == 'Geral' 
                                & !(percentis$label %in% c('group_1','group_2'))
                                & (percentis$label %like% "lower_ci"),]
  # Matching labels
  perc.geral.lower$label <- str_sub(perc.geral.lower$label, 1, -10)
  
  # Upper CIs
  perc.geral.upper <- percentis[percentis$effect == 'Geral' 
                                & !(percentis$label %in% c('group_1','group_2'))
                                & (percentis$label %like% "upper_ci"),]
  # Matching labels
  perc.geral.upper$label <- str_sub(perc.geral.upper$label, 1, -10)
  
  # Changing Column Names
  colnames(perc.geral.lower)[5] <- "lower.ci"
  colnames(perc.geral.upper)[5] <- "upper.ci"
  
  # Joining
  perc.geral <- left_join(perc.geral.values, perc.geral.lower, 
                          by=c("label", "effect", "years", "quantil"))
  perc.geral <- left_join(perc.geral, perc.geral.upper, 
                          by=c("label", "effect", "years", "quantil"))
}

## Plottando a decomposição detalhada

## Fittando as Loess na mão (pra poder preencher)
perc.gg <- perc.gg %>% 
  group_by(label, effect, years) %>% 
  arrange(label, effect, years, quantil) %>%
  mutate(loess = predict(loess(values ~ quantil, span = .2,),
                         data.frame(quantil = seq(min(quantil), max(quantil), 1))),
         loess.lower = predict(loess(lower.ci ~ quantil, span = .2,),
                               data.frame(quantil = seq(min(quantil), max(quantil), 1))),
         loess.upper = predict(loess(upper.ci ~ quantil, span = .2,),
                               data.frame(quantil = seq(min(quantil), max(quantil), 1))))

## 2012 a 2015:
  # FT mais escolarizada, mas a queda no prêmio educacional compensou esses efeitos
  # (com exceção dos 5% mais pobres)
  # Houve uma ligeira queda no prêmio da formalidade para os estratos inferiores,
  # mas acompanhado de um efeito composição positivo, indicando a formalização
  # dessa parcela da população. Ressalta-se também o efeito composição positivo 
  # dos setores nesses estratos.
  # Redução no prêmio da escolariade e da experiência, principalmente nos estratos superiores;
  # como visto na decomposição do Gini, esse foi o principal fator que reduziu
  # a desigualdade no período.
  # Intercepto: outros fatores estruturais do mercado de trabalho explicam
  # o aumento de renda dos mais pobres (como o preço de habilidades não-observadas
  # e os aumento do salário mínimo, especialmente após o grande crescimento de 2010),
  # o que ocorre devido a queda no prêmio de habilidades observáveis.
  # Não houve muitas mudanças na discriminação de raça/gênero, o que se estende
  # aos outros períodos analisados.

  # De modo geral, os ganhos foram concentrados na base da distribuição, mas
  # todos os estratos tiveram ganhos. O crescimento foi inclusivo, com a curva
  # de incidência do crescimento negativamente inclinada graças ao maior prêmio
  # de habilidades não-observadas OU do efeito do salário mínimo (BRITO et al, 2013).
  # (na parte de baixo da distribuição) e a redução do prêmio salarial/
  # de experiência (entre os mais ricos)
  # O crescimento inclusivo encontrado corrobora os achados de (BARBOSA, SOUZA, SOARES, 2020),
  # bem como a perda de renda dos mais pobres/estagnação dos demais estratos
  # no período 2015-2020.

## 2015 a 2020:
  # FT bem mais escolarizada, especialmente no topo da distribuição
  # Maior informalidade no 20% mais pobres; aumento do prêmio mitigou os efeitos
  # aumento do prêmio de habilidades observáveis (educ) e queda no de não-obs.:
  # efeitos pesam mais para o lado das habilidades não-observáveis, prejudicando
  # principalmente os 20% mais pobres. Mais provável que isso é a interrupção
  # no ciclo de alta do salário-mínimo (que afeta apenas os formais),
  # causada pela recessão/estagnação.

  # O período foi marcado por um declínio dos 20% mais pobres (em virtude do
  # intercepto) e uma estagnação no resto da pirâmide.

## 2012 a 2020:
  # FT MUITO escolarizada, principalmente no topo; contrabalanceada pela redução
  # do prêmio salarial nos estratos superiores. Na base, a educação contribuiu
  # para o aumento da renda, o que foi ligeiramente contrabalanceado pela
  # diminuição no prêmio de habilidades não-observáveis (ACEMOGLU, 2002) ou, 
  # mais provavelmente, pela parada nos avanços do salário mínimo em virtude
  # da recessão e do baixo crescimento que se seguiu, o que fez com que o paga-
  # mento ficasse com quase nenhum reajuste real.

  # O período foi, portanto, marcado pelo mesmo padrão de 2012-2015 (crescimento
  # inclusivo, ainda que com a perda em 2015-2020 e a inclusão concentrada nos
  # três primeiros anos iniciais). O principal fator que contribuiu para a 
  # queda da desigualdade foi a redução do prêmio salarial da educação de um
  # topo cada vez mais escolarizado.
  # Vale ressaltar também o comportamento do prêmio educacional na base da 
  # distribuição (inclusivo em 2015-2020 e no período como um todo; ao contrário
  # de 2012-15, o intercepto teve efeito negativo, o que pode ser atribuído ao
  # fim do ciclo de alta real do salário mínimo no período 2015-2020).

## 2020 a 2021:
  ## Rendimentos efetivos por hora:
    # Chama a atenção a redução no prêmio salarial da educação para a parte de 
    # baixo da pirâmide, o que indica que pessoas escolarizadas (mas pobres) estão
    # sendo forçadas a aceitar trabalhos com menor remuneração e mais precários.
    # O efeito do intercepto não é muito claro, podendo ser parte da explicação
    # o fato de que muitas pessoas nesses estratos não possuírem telefone e,
    # consequentemente, não terem sido entrevistadas pelo IBGE, o que ainda
    # não era um empecilho no ano anterior. O resultado também pode estar ligado
    # a maior volatilidade de renda dos estratos inferiores (FIRPO, PORTELLA, 2021);
    # Além disso, ressalta-se a melhor composição da FT, em linha com o que se
    # espera em crises; o fenômeno é maior na parte de cima da distribuição.

    # De modo geral, vê-se que, DENTRE OS QUE TRABALHAM, houve um ganho no 1º decil,
    # o que pode ser atribuído ao fato de que pessoas menos bem remuneradas em 2020
    # foram expulsas da força de trabalho, o que fez com que os mais pobres na 
    # força de trabalho em 2021 sejam "menos pobres" que 2020.
    # Ao longo de toda o restante da distribuição, houve uma perda de renda, o
    # que se intensifica nos estratos mais ricos (por que?)

# The palette with black:
Palette <- c("Gray", "#000000", "#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

perc.gg %>% 
  ggplot(aes(x = quantil/10, y = values, colour = label, fill = label)) + 
  # geom_smooth(se = F, size = .5, span = .2) + 
  # geom_line(size = .5) +
  # geom_ribbon(aes(ymin=lower.ci, ymax=upper.ci), linetype=0, show.legend=F, alpha=0.3) +
  geom_line(aes(y = loess), size = .5) +
  geom_ribbon(aes(ymin=loess.lower, ymax=loess.upper), linetype=0, show.legend=F, alpha=0.3) +
  labs(x = 'Decis', y = 'Diferença dos Logs') +
  scale_colour_manual(
    name = "",
    values = Palette,
    labels = c("Intercepto", "Educação", "Experiência", "Gênero", "Formalidade",
               "Cor/Etnia","Região", "Rural", "Setor")
  ) + 
  scale_fill_manual(name = "", values = Palette, guide = "none") +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", 
                                               decimal.mark = ",", 
                                               scientific = FALSE)) +
  theme_bw() + 
  theme(
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.box = 'horizontal',
      legend.title = element_blank(),
      legend.background = element_rect(fill = NA),
      legend.text = element_text(size = 12)
  ) + 
  guides(color = guide_legend(nrow = 2)) + 
  facet_grid(rows = vars(effect), cols = vars(years),
             scales = 'free_y', switch = 'y')
ggsave('decomp_detalhada_CI_loess_quantis.pdf', dpi = 12000)
# ggsave('decomp_detalhada_CI_quantis.pdf', dpi = 6000)

## Decomposição Geral
# A diferença observada pode ser interpretada como a curva de incidência do crescimento

## Fittando as Loess na mão (pra poder preencher)
perc.geral <- perc.geral %>% 
  group_by(label, years) %>% 
  arrange(label, years, quantil) %>%
  mutate(loess = predict(loess(values ~ quantil, span = .2,),
                         data.frame(quantil = seq(min(quantil), max(quantil), 1))),
         loess.lower = predict(loess(lower.ci ~ quantil, span = .2,),
                               data.frame(quantil = seq(min(quantil), max(quantil), 1))),
         loess.upper = predict(loess(upper.ci ~ quantil, span = .2,),
                               data.frame(quantil = seq(min(quantil), max(quantil), 1))))

Palette2 <- c("blue4", "green4", "red2")
Palette2 <- wes_palette("Cavalcanti1")
Palette2 <- wes_palette("Darjeeling1")

perc.geral %>% 
  ggplot(aes(x = quantil/10, y = values, colour = label, fill = label)) + 
  # geom_line(size = .5) +
  # geom_ribbon(aes(ymin=lower.ci, ymax=upper.ci), linetype=0, show.legend=F, alpha=0.5) +
  geom_line(aes(y = loess), size = .5) +
  geom_ribbon(aes(ymin=loess.lower, ymax=loess.upper), linetype=0, show.legend=F, alpha=0.5) +
  # geom_smooth(se = F, size = .5, span = .2, show.legend = F) +
  geom_hline(yintercept = 0, linetype = 'longdash', size = .5, colour = 'black', show.legend = F) + 
  labs(x = 'Decis', y = 'Diferença dos Logs') +
  scale_colour_manual(
    name = "",
    values = Palette2,
    labels = c("Diferença Observada", "Composição da Força de Trabalho", 
               "Estrutura do Mercado de Trabalho")
  ) + 
  scale_fill_manual(name = "", values = Palette2, guide = "none") +
  scale_x_continuous(breaks = seq(0,10, by = 1)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ".", 
                                               decimal.mark = ",", 
                                               scientific = FALSE)) +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    plot.caption = element_text(hjust = 0),
    legend.text = element_text(size = 12)
  ) + 
  guides(color = guide_legend(nrow = 1)) + 
  facet_grid(cols = vars(years))

ggsave('decomp_geral_CI_loess_quantis.pdf', dpi = 12000)
# ggsave('decomp_geral_CI_quantis.pdf', dpi = 6000)

## Decomposições Totais Gini e Renda Média ####

## Lendo o arquivo
file.media.gini <- "Decomp_Medias_Gini.xlsx"
sheet.media.gini <- "R"
media.gini <- read.xlsx(file = file.media.gini, 
                       sheetName = sheet.media.gini, as.data.frame = TRUE)

## Pivotando
media.gini %<>% 
  pivot_longer(!c(label, effect, years), 
               names_to = "estatistica", 
               values_to = 'values'
  )

## arrumando o ponto decimal e convertendo pra número
media.gini$values <- gsub(",",".", media.gini$values)
media.gini$values <- as.numeric(media.gini$values)

# removendo valores nulos
media.gini <- media.gini[!is.na(media.gini$values), ]

# Reordenando e renomeando as variáveis
nova.ordem <- c("Intercepto", "Educação", "Experiência", "Gênero (Feminino)",
                "Formalização", "Etnia (PPIs)", "Região",
                "Local de Domicílio (Rural)", "Setor Econômico")
novas.labels <- c("Intercepto", "Educação", "Experiência", "Gênero", "Formalidade",
                           "Cor/Etnia","Região", "Rural", "Setor")
media.gini$label <- factor(media.gini$label, levels = nova.ordem)
levels(media.gini$label) <- novas.labels


# Reordenando os anos
media.gini$years <- factor(media.gini$years, 
                          levels = c('2012-2015','2015-2020','2012-2020','2020-2021E'))

## Plottando a média
Palette <- c("Gray", "#000000", "#E69F00", "#56B4E9", "#009E73", 
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
Palette2 <- c("#009E73", "#D55E00", "#000000")

media.gini[media.gini$estatistica == "media", ] %>% 
  ggplot(aes(x = label, y = values, fill = label)) + 
  geom_bar(stat = "identity", alpha = .8, position = "dodge") + 
  geom_hline(yintercept = 0, linetype = 'longdash', size = .3, colour = 'blue4', show.legend = F) + 
  # valores positivos maiores que 0.01
  geom_text(
    data = media.gini[media.gini$estatistica == "media" & media.gini$values >= .01, ],
    mapping = aes(label = format(round(values,2), decimal.mark = ",")), 
    nudge_y = 0.02, size = 2
  ) +
  # valores negativos menores que -0.01
  geom_text(
    data = media.gini[media.gini$estatistica == "media" & media.gini$values <= -.01, ],
    mapping = aes(label = format(round(values,2), decimal.mark = ",")), 
    nudge_y = -0.02, size = 2
  ) +
  labs(x = 'Variáveis', y = 'Impacto na Diferença dos Logs(Salário/Hora)') +
  scale_fill_manual(name = "", values = Palette) +
  scale_y_continuous(
    breaks = seq(-0.15,0.15, by = 0.05),
    labels=function(x) format(round(x,2), big.mark = ".", decimal.mark = ",", scientific = F)
  ) +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)
  ) + 
  guides(fill = guide_legend(nrow = 1)) + 
  facet_grid(rows = vars(effect), cols = vars(years), switch = "y")
ggsave("decomp_medias.pdf", dpi = 24000)

## Plottando o Gini
media.gini[media.gini$estatistica == "gini", ] %>% 
  ggplot(aes(x = label, y = values, fill = label)) + 
  geom_bar(stat = "identity", alpha = .8, position = "dodge") + 
  geom_hline(yintercept = 0, linetype = 'longdash', size = .3, colour = 'blue4', show.legend = F) + 
  # valores positivos maiores que 0.1
  geom_text(
    data = media.gini[media.gini$estatistica == "gini" & media.gini$values >= .1, ],
    mapping = aes(label = format(round(values,2), decimal.mark = ",")), 
    nudge_y = 0.75, size = 2
  ) +
  # valores negativos menores que -0.1
  geom_text(
    data = media.gini[media.gini$estatistica == "gini" & media.gini$values <= -.1, ],
    mapping = aes(label = format(round(values,2), decimal.mark = ",")), 
    nudge_y = -0.75, size = 2
  ) +
  labs(x = 'Variáveis', y = 'Impacto na Diferença dos Índices de Gini') +
  scale_fill_manual(name = "", values = Palette) +
  scale_y_continuous(
    breaks = seq(-7.5,7.5, by = 2.5),
    labels=function(x) format(x, big.mark = ".", decimal.mark = ",",  scientific = F)
  ) +
  theme_bw() + 
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal',
    legend.box = 'horizontal',
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)
  ) + 
  guides(fill = guide_legend(nrow = 1)) + 
  facet_grid(rows = vars(effect), cols = vars(years), switch = "y")
ggsave("decomp_gini.pdf", dpi = 24000)
