**** Do file da estimação da decomposição RIF, levando em conta o desenho amostral da PNAD

* Importante: o arquivo .dta é gerado no script Analise_PNADC.R


* Limpando a memória e facilitando a visualização dos outputs
clear all    
eststo clear
cls
set more off
set dp comma

* Mudando o diretório e lendo os dados
cd "/Users/vinicius/Desktop/Artigos/PET/Artigo_PNADC_RIF"
use df_pnad_pet

**** Modificando o dataset
*** Criando dummies de cada ano
gen doze = 1 if Ano == 2012
replace doze = 0 if doze == .

gen quinze = 1 if Ano == 2015
replace quinze = 0 if quinze == .

gen vinte = 1 if Ano == 2020
replace vinte = 0 if vinte == .

gen vinteum = 1 if Ano == 2021
replace vinteum = 0 if vinteum == .

*** Criando dummies para ver se a pessoa está em idade ativa
gen ativo = 1 if idade >= 15 & idade < 65
replace ativo = 0 if ativo == .

** formatando as funções de educ e exper para seus coeficientes ficarem visíveis
replace educ2 = educ2/100
replace educ3 = educ3/1000
replace educ4 = educ4/10000

replace exper2 = exper2/100
replace exper3 = exper3/1000
replace exper4 = exper4/10000

* Alternativamente, podemos usar o seguinte comando, que 
* gera dummies para os n-1 grupos: xi i.Ano

** Alternando os grupos-base
* Para ver quais são esses grupos, reg y x, baselevels
* Os grupos-base seguiram os que foram definidos no script do R

* Homem para Sexo
fvset base 2 Sexo
* Urbano para rural
rename rural local_dom
fvset base 2 local_dom
* Serviços financeiros para setor
fvset base 11 setor

* criando a lista de variáveis dependentes e independentes
global y lsalh

global x_setor educ educ2 educ3 educ4 exper exper2 exper3 exper4 i.Sexo i.Cor2 i.local_dom i.formalidade i.regiao i.setor

* regredindo com erros robustos e os pesos para o ano de 2012 e para as pessoas em idade ativa
reg $y $x_setor if Ano == 2012 & idade >= 15 & idade < 65 [pweight = V1028], vce(robust)

**** Criando o desenho amostral da PNADC
** Postweight: populações de cada posest
* svyset UPA [pweight = V1027], strata(Estrato) poststrata(posest) postweight(V1029) singleunit(scaled)
svyset UPA [pweight = V1028], strata(Estrato) singleunit(scaled)

** Vendo estatísticas
svy: mean lsalh, over(Ano)
svy: mean lsalh_efet, over(Ano)

* regredindo com erros robustos e os pesos para o ano de 2012 e para as pessoas em idade ativa
svy, subpop(ativo if doze == 1): reg $y $x_setor
eststo reg2012

* para os demais anos
svy, subpop(ativo if quinze == 1): reg $y $x_setor
eststo reg2015

svy, subpop(ativo if vinte == 1): reg $y $x_setor
eststo reg2020

svy, subpop(ativo if vinte == 1): reg lsalh_efet $x_setor
eststo reg2020_efet

svy, subpop(ativo if vinteum == 1): reg lsalh_efet $x_setor
eststo reg2021

** Exportando para o Latex
* Educação: queda consistente do prêmio ao longo dos anos (ver gráfico R)
* Experiência: queda entre 2012 e 2015 (um dos principais drivers da queda da desigualdade) e aumento entre 2015 e 2020; tendência geral indefinida
* Feminino: redução da discriminação ao longo do tempo (o que coincide com a maior participação feminina no mercado de trabalho)
* PPI: redução da discriminação entre 2012 e 2015, mas aumento entre 2015-2020/21 para patamares superiores ao de 2012
* Rural: pessoas nesses ambientes tem conseguido, em média, diminuir o gap frente ao ambiente urbano, o que pode ser atribuído ao boom da agricultura frente a estagnação da economia brasileira em 2015-2020/21 (principalmente no período 2020-2021)
* Formalidade: prêmio da formalidade diminuiu ligeiramente entre 2012 e 2015, mas aumentou com o aumento dos informais e da precarização no pós-crise.
* Região (frente ao CO, principalmente DF): N aumentou o gap; NE diminuiu entre 2012 e 2015, mas aumentou novamente com a crise. S fez esse gap zerar e SE diminuiu bastante.
* Setor (frente a serviços financeiros): Adm. Pública aumentou seu prêmio e agro diminuiu; gap do comércio aumentou com a pandemia. Chama atenção o setor de construção, que havia zerado o gap em 2015 e, com o advento dos escandâlos de corrupção descobertos pela Lava-Jato, viu seu prêmio salarial cair novamente. Vale mencionar também a melhora dos serviços domésticos, apesar da perda parcial do avanço com a pandemia.
cls
esttab reg2012 reg2015 reg2020 reg2020_efet reg2021 using regs.tex, b(4) se(4) r2(3) ///
title("\label{tab1}Estimações (com desenho amostral) por ano") ///
nonumbers mtitles("2012" "2015" "2020" "2020_efet" "2021") ///
label brackets compress nobaselevels varwidth(22)


**** RIFs!
** o comando não funciona usando fatores (i.), então deveremos cria-los
* Sexo (base: homens)
xi, noomit i.Sexo
drop _ISexo_2
rename _ISexo_1 feminino

* Rural (base: urbanos)
xi, noomit i.local_dom
drop _Ilocal_dom_2
rename _Ilocal_dom_1 rural

* Setor (base: adm.publica; usaremos como base servicos financeiros (== 11))
xi, noomit i.setor
rename (_Isetor_1 _Isetor_2 _Isetor_3 _Isetor_4 _Isetor_5 _Isetor_6 _Isetor_7 _Isetor_8 _Isetor_9 _Isetor_10 _Isetor_11 _Isetor_12) (adm_publica agro aloj_alim ativ_mal_definidas comercio construcao educ_saude industria outros_servicos servicos_domesticos servicos_fin transporte)

*Cor2 (base: brancos/amarelos)
xi i.Cor2
rename _ICor2_2 ppi

*Formalidade (base: informais)
xi i.formalidade
rename _Iformalida_2 formal

*Região (base: CO)
xi, noomit i.regiao
rename (_Iregiao_1 _Iregiao_2 _Iregiao_3 _Iregiao_4 _Iregiao_5) (CO N NE S SE)

** Criando as variáveis dependentes na variável global xlist_oaxaca
* Aqui, agregamos os resultados de educ e exper e normalizamos as variáveis
* categóricas (já que os resultados dependem da categoria base)
global xlist_oaxaca (Educação: educ educ2 educ3 educ4) (Experiência: exper exper2 exper3 exper4) feminino ppi rural formal (Região: normalize(b.CO N NE S SE)) (Setor: normalize(b.servicos_fin adm_publica agro aloj_alim ativ_mal_definidas comercio construcao educ_saude industria outros_servicos servicos_domesticos transporte))

* variáveis para serem usadas no processo de reweighting (iguais às de cima, mas desagrupadas, e com a adição de chefe_dom, casado e num_filhos05)
global xlist_rwt educ educ2 educ3 educ4 exper exper2 exper3 exper4 feminino ppi rural formal N NE S SE adm_publica agro aloj_alim ativ_mal_definidas comercio construcao educ_saude industria outros_servicos servicos_domesticos transporte chefe_dom casado num_filhos05

*******************************************************************************
*** 2012 e 2015 ***
** fazendo uma decomposição do log da renda média por hora entre 2012 e 2015
** entre os ativos (15 <= idade < 65)
* Usando como referência os coeficientes do grupo 2 (2012, wgt(0))
quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2015) [pw = V1028], rif(mean) by(doze) wgt(0) robust
eststo media_1215

** Gini da renda habitual total
quietly oaxaca_rif sal_hab $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2015) [pw = V1028], rif(gini) by(doze) wgt(0) robust scale(100)
eststo gini_1215

/** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis
foreach i in 1 10 50 90 99 {
	display `i'
	quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2015) [pw = V1028], rif(q(`i')) by(doze) wgt(0) robust
	eststo p`i'_1215
}

esttab media_1215 gini_1215 p1_1215 p10_1215 p50_1215 p90_1215 p99_1215, ///
title("Resultados da Decomposição de Várias Estatísticas") ///
nonumbers mtitles("Média" "Gini" "1º" "10º" "Mediana" "90º" "99º") ///
addnote("Grupo 1: 2015; Grupo 2: 2012") ///
se(4) b(4) brackets varwidth(12) compress
*/


*** 2015 e 2020 ***
** Média
quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2015 | Ano == 2020) [pw = V1028], rif(mean) by(quinze) wgt(0) robust
eststo media_1520

** Gini da renda habitual total
quietly oaxaca_rif sal_hab $xlist_oaxaca if ativo == 1 & (Ano == 2015 | Ano == 2020) [pw = V1028], rif(gini) by(quinze) wgt(0) robust scale(100) 
eststo gini_1520

/** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis 
foreach i in 1 10 50 90 99 {
	display `i'
	quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2015 | Ano == 2020) [pw = V1028], rif(q(`i')) by(quinze) wgt(0) robust
	eststo p`i'_1520
}

esttab media_1520 gini_1520 p1_1520 p10_1520 p50_1520 p90_1520 p99_1520, ///
title("Resultados da Decomposição de Várias Estatísticas") ///
nonumbers mtitles("Média" "Gini" "1º" "10º" "Mediana" "90º" "99º") ///
addnote("Grupo 1: 2020; Grupo 2: 2015") ///
se b(5) t(3) brackets varwidth(12) compress
*/


*** 2012 e 2020 ***
** Média
quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2020) [pw = V1028], rif(mean) by(doze) wgt(0) robust
eststo media_1220

** Gini da renda habitual total
quietly oaxaca_rif sal_hab $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2020) [pw = V1028], rif(gini) by(doze) wgt(0) robust scale(100)
eststo gini_1220

/** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis 
foreach i in 1 10 50 90 99 {
	display `i'
	quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2020) [pw = V1028], rif(q(`i')) by(doze) wgt(0) robust
	eststo p`i'_1220
}

esttab media_1220 gini_1220 p1_1220 p10_1220 p50_1220 p90_1220 p99_1220, ///
title("Resultados da Decomposição de Várias Estatísticas") ///
nonumbers mtitles("Média" "Gini" "1º" "10º" "Mediana" "90º" "99º") ///
addnote("Grupo 1: 2020; Grupo 2: 2012") ///
se b(5) t(3) brackets varwidth(12) compress
*/


*** 2020 e 2021 ***
** Média
quietly oaxaca_rif lsalh_efet $xlist_oaxaca if ativo == 1 & (Ano == 2021 | Ano == 2020) [pw = V1028], rif(mean) by(vinte) wgt(0) robust
eststo media_2021

** Gini da renda habitual total
quietly oaxaca_rif sal_efet $xlist_oaxaca if ativo == 1 & (Ano == 2021 | Ano == 2020) [pw = V1028], rif(gini) by(vinte) wgt(0) robust scale(100)
eststo gini_2021

/** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis 
foreach i in 1 10 50 90 99 {
	display `i'
	quietly oaxaca_rif lsalh_efet $xlist_oaxaca if ativo == 1 & (Ano == 2021 | Ano == 2020) [pw = V1028], rif(q(`i')) by(vinte) wgt(0) robust
	eststo p`i'_2021
}

esttab media_2021 gini_2021 p1_2021 p10_2021 p50_2021 p90_2021 p99_2021, ///
title("Resultados da Decomposição de Várias Estatísticas") ///
nonumbers mtitles("Média" "Gini" "1º" "10º" "Mediana" "90º" "99º") ///
addnote("Grupo 1: 2021; Grupo 2: 2020") ///
se b(3) se(3) brackets varwidth(12) compress
*/

*******************************************************************************
**** Fazendo as tabelas por estatísticas ***
cls
** Média
esttab media_1215 media_1520 media_1220 media_2021, ///
title("Resultados da Decomposição das Médias") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(3) b(3) brackets varwidth(13) compress wide
* 2012-2015, COMPOSIÇÃO: o aumento da renda média foi igualmente alavancado pelos efeitos composição e estrutural. Chama a atenção a melhora da composição educacional e o aumento da experiência. Feminino e PPI tem sinal negativo porque houve uma maior inserção desses grupos menos remunerados na força de trabalho, o que contribuiu negativamente para o efeito composição. Houve uma ligeira formalização da FT nesse periodo, bem como a entrada em setores mais bem-remunerados
* 2012-2015 ESTRUTURAL: Os principais fatores estruturais do mercado de trabalho que reduziram a renda média foram a queda nos prêmios educacionais e de experiência - que, como afetam os trabalhadores mais-bem remunerados, diminuem a renda média e a desigualdade. Contudo, o maior driver positivo foi o intercepto, que capta o prêmio das habilidades não-observáveis (que tende a subir com a redução do prêmio educacional) e os grandes avanços reais do salário-mínimo.

* 2015-2020, COMPOSIÇÃO: a composição da força de trabalho foi o que impediu que a renda diminuísse no período, sendo o efeito quase inteiramente em virtude da maior escolaridade da FT. Resalta-se o efeito negativo da formalização, mostrando que o trabalho se tornou mais informal e precário, apesar de ainda haver mais PPIs e mulheres no mercado de trabalho.
* 2015-2020, ESTRUTURAL: a estrutura do mercado de trabalho foi a responsável pela estagnação da renda do trabalho: apesar de um maior prêmio da experiência e da formalização, a queda no prêmio dos efeitos não-observáveis/fim do ciclo de aumento real do salário-mínimo fizeram com que a renda estagnasse, afetando principalmente os estratos inferiores da distribuição (gráfico R). 

* 2012-2020, COMPOSIÇÃO: Mesmo os avanços concentrados em 2012-2015, é interessante analisar o período como um todo vendo como, por exemplo, a FT se tornou bastante escolarizada, principalmente no topo, o que contribuiu para o efeito composição da educação. A maior experiência também contribuiu para a elevação da renda, o que foi contrabalanceado pela inserção de PPIs e mulheres no mercado de trabalho. No período como um todo, houve uma maior informalidade, fenômeno concentrado no período de 2015-2020.
* 2012-2020, ESTRUTURAL: A redução no prêmio educacional - principalmente no 1º período - puxou a renda média para baixo, o que foi parcialmente contrabalanceado pela menor discriminação feminina, menos desigualdade interregional e maior prêmio de formalidade.

* 2020-2021, COMPOSIÇÃO: Em linha com o IQT, a FT ficou mais escolarizada (69,17% do efeito total) e experiente (25,4%) (principalmente no topo da distribuição), haja vista a perda de postos mais precários. Educação e experiência são, juntos, responsáveis por 94,6% do efeito composição. Os demais 5,4% são distribuídos entre uma FT menos diversa (já que mulheres, PPIs e sua interação foram os mais afetados e possuiam menores rendimentos). Dado que a agricultura foi o setor menos afetado, houve uma maior importância relativa destes postos de trabalho na distribuição; como eles tem renda média menor, há o sinal negativo.
*2020-2021, ESTRUTURAL: Houve uma GIGANTESCA redução do prêmio salarial, fenômeno que ficou concentrado na base da distribuição: indivíduos mais escolarizados foram forçados a, por necessidade, ocuparem postos mais precários e menos bem-remunerados, diminuindo sua renda e piorando a produtividade de curto e de longo prazo do país (histerese). Na contrapartida, há um grande aumento do prêmio de outros fatores não-observados, os quais não são inteiramente claros. Além disso, uma redução no retorno experiência causou uma queda na renda média, provavelmente através do mesmo mecanismo da educação ou em virtude da adoção de novas tecnologias que favorecem os mais jovens ("age-biased technical change hypothesis", BEHAGHEL e GREENAN, 2010). O sinal de região mostra que os efeitos da crise prejudicaram mais as regiões mais pobres.


** Gini
esttab gini_1215 gini_1520 gini_1220 gini_2021, ///
title("Resultados da Decomposição dos Ginis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(3) b(2) brackets varwidth(13) compress wide
* 2012-2015, COMPOSIÇÃO: A composição da FT contribuiu para um aumento da desigualdade no período, sendo grande parte em virtude do "paradoxo do progresso" (FERREIRA et al, 2017 e 2021): como os retornos da educação são convexos e a FT se tornou mais escolarizada, aumenta a dispersão dos salários, o que aumenta a desigualdade. O mesmo ocorre, em certa medida, com a experiência, apesar de os seus retornos serem côncavos. A maior formalização da FT afetou mais os estratos inferiores, colaborando para uma redução da desigualdade. Além disso, a mudança para postos de trabalho em setores mais bem remunerados também foi importante para reduzir a desigualdade, bem como a inserção de PPIs e mulheres - que ganham menores salários - na força de trabalho.
* 2012-2015, ESTRUTURAL: A estrutura do mercado de trabalho mais que compensou o efeito composicão, sendo o principal driver a queda no prêmio educacional e, principalmente, da experiência, em linha com os achados de FERREIRA et al (2021). Isso se a uma série de fatores, como o "age-biased technical change" (BEHAGHEL, GREENAN, 2010), uma vez que a revolução computacional/técnica que beneficia os mais novos, que tradicionalmente estão na parte de baixo da pirâmide salarial, e causa rápida obsolescência de habilidades. Além disso, como as gerações mais novas são mais escolarizadas (havendo um "atraso" para entrar na FT), elas podem entrar na força de trabalho recebendo um salário maior. Além disso, FIRPO (2019) também pontua o efeito de haver formas e incentivos para pessoas bem-remuneradas aposentarem mais cedo.
* Ressalta-se que houve um aumento da desigualdade inter-regional no período, sendo os demais efeitos insignificantes.

* 2015-2020, COMPOSIÇÃO: No período, houve um aumento da desigualdade para um patamar semelhante ao de 2012, explicado majoritariamente pelo efeito composição. Nele, vê-se uma notável participação da educação (97% do total), especialmente entre os estratos superiores, o que aumenta a desigualdade. Além disso, uma FT mais experiente contribui para uma maior desigualdade, o que foi contrabalanceado pela maior inserção de PPIs e mulheres no mercado de trabalho. Vê-se que a formalização também foi importante para explicar a desigualdade, uma vez que houve perda de postos desta modalidade e maior informalidade e precarização no período, principalmente ente os mais pobres (ver gráfico R). Vê-se também uma importante contribuição da melhora nas regiões e da ocupação de postos de trabalho em setores com melhor remuneração.
* 2015-2020, ESTRUTURAL: A redução no prêmio da educação nos estratos superiores e o aumento nos estratos inferiores foi marcante e muito inclusiva, responsável por 700% do efeito total. Ela é contrabalanceada, contudo, pelo intercepto, que representa a desvalorização de habilidades não-observáveis na base da distribuição e, mais importante, o fim do ciclo de avanços reais no salário mínimo. Isso está consistente com SALTIEL e URZUA (2017), que argumentam que os efeitos do salário mínimo dependem da situação macroeconômica, sendo progressivos apenas em situações de crescimento - que induzem maior formalidade e emprego. Isso está consistente com o fato de que, entre 1981 e 1999, o salário mínimo foi regressivo (MENEZES-FILHO, RODRIGUES, 2009), enquanto foi muito progressivo na década de 00-10 (BRITO et al, 2013; FERREIRA, FIRPO, MESSINA, 2017), onde se viu um cenário internacinal majoritariamente favorável. Além disso, o salário mínimo chegou a 70% da remuneração mediana no 3ºtri/2018, o que reduz seu potencial para compressão salarial (FIRPO, PORTELLA, 2019).
* Com exceção de região, os demais termos são insignificantes. Pode-se argumentar que o fim da redução do prêmio da experiência foi um das fatores responsáveis pela estagnação da desigualdade, já que seu papel redistributivo notado desde 2003 (FERREIRA et al, 2017/21) foi interrompido com a crise.

* 2012-2020, COMPOSIÇÃO: Responsável por uma distribuição mais desigual, ressalta-se o papel da maior escolaridade da FT, cuja curva de incidência do crescimento é muito íngrime. Além disso, uma FT mais experiente no geral contribuiu para a desigualdade, efeito parcialmente mitigado pela entrada de PPIs e mulheres, além de uma maior inserção rural. A formalização foi um grande driver de desigualdade, haja vista a informalidade vivida pelo mercado de trabalho a partir de 2015. A ocupação em postos de setores mais bem-remunerados contribuiu para mitigar a desigualdade total no período.
* 2012-2020, ESTRUTURAL: O efeito estrutural foi redistributivo. Os maiores prêmios educacionais na parcela inferior da distribuição e menores na parte superior - um fenômeno notável de 2015-2020 - fez com que essa variável fosse a maior responsável pela redução da desigualdade no período, o que foi contrabalanceado pelos fenômenos do intercepto descritos no período de 2015-2020. Ressalta-se também o importante prêmio de experiência, cujos efeitos distributivos se concentram no primeiro triênio. A redução da discriminação contra PPIs se torna significante quando se analisa todo o período, o que também reduz a desigualdade haja vista a menor remuneração desse grupo. Contudo, as desigualdades regionais são muito acentuadas no período e contribuíram para um aumento de 1 p.p. no Gini.

* 2020-2021, COMPOSIÇÃO: Como em toda crise (IQT IPEA, 2021), a FT ficou mais escolarizada, o que reflete o fato de que os mais vulneráveis, menos escolarizados e menos experientes foram os mais afetados pela pandemia. Além disso, vê-se que a FT ficou menos diversas, uma vez que houve uma redução da proporção de mulheres (não-significativa estatisticamente) e de PPIs. A maior parcela de pessoas rurais também faz com que haja uma maior desigualdade.
*Combinando essas informações, vê-se que o grupo mais afetado foi o de mulheres negras (MADE, 2020/2021). Apesar disso, não há indícios estatísticos de que a força de trabalho ficou mais informal no período.
* 2020-2021, ESTRUTURAL: Houve um efeito bastante acentuado da redução do prêmio educacional na base da distribuição, o que mostra que muitas pessoas escolarizadas tiveram de mudar para postos com menor rendimento, o que precariza e diminui a produtividade da economia brasileira, podendo ter inclusive consequências de longo prazo em virtude da histerese. Contudo, os efeitos estruturais descritos não são significantes, o que pode ser um sintoma do menor número de amostras usadas em virtude da dificuldade de coleta por parte do IBGE em virtude da pandemia.
* 2020-2021, ESTRUTURAL: não-significante e diz respeito principalmente à valorização dos trabalhadores rurais, haja vista o miniboom de commodities.

** 1º Percentil
esttab p1_1215 p1_1520 p1_1220 p1_2021, ///
title("Resultados da Decomposição dos 1º Percentis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide

** 1º Decil
esttab p10_1215 p10_1520 p10_1220 p10_2021, ///
title("Resultados da Decomposição dos 1º Decis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide

** Mediana
esttab p50_1215 p50_1520 p50_1220 p50_2021, ///
title("Resultados da Decomposição das Medianas") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide

** 9º Decil
esttab p90_1215 p90_1520 p90_1220 p90_2021, ///
title("Resultados da Decomposição dos 9º Decis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide

** 99º Percentil
esttab p99_1215 p99_1520 p99_1220 p99_2021, ///
title("Resultados da Decomposição dos 99º Percentis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide
*/


****************** Decomposição por Percentis ************************
*** 2012_2015
** limpando a memória das outras estimações
eststo clear

** Loop sobre todos os quantis para a decomposição
forvalues i = 1(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2015) [pw = V1028], rif(q(`i')) by(doze) wgt(0)
}
set dp period
*esttab using 2012_2015.csv
esttab using 2012_2015.csv, ci(4) wide



*** 2015_2020
** limpando a memória das outras estimações
eststo clear

** Loop sobre todos os quantis para a decomposição
forvalues i = 1(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2015 | Ano == 2020) [pw = V1028], rif(q(`i')) by(quinze) wgt(0)
}
set dp period
*esttab using 2015_2020.csv
esttab using 2015_2020.csv, ci(4) wide



*** 2012_2020
** limpando a memória das outras estimações
eststo clear

** Loop sobre todos os quantis para a decomposição
forvalues i = 1(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2020) [pw = V1028], rif(q(`i')) by(doze) wgt(0)
}
set dp period
*esttab using 2012_2020.csv
esttab using 2012_2020.csv, ci(4) wide


*** 2020_2021
** limpando a memória das outras estimações
eststo clear

** Loop sobre todos os quantis para a decomposição
forvalues i = 1(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif lsalh_efet $xlist_oaxaca if ativo == 1 & (Ano == 2020 | Ano == 2021) [pw = V1028], rif(q(`i')) by(vinte) wgt(0) relax
}
set dp period
*esttab using 2020_2021.csv
esttab using 2020_2021.csv, ci(4) wide

*/

/* ******************* Tabelas com Reweights ************************
*** 2012-2015
** estimando com um reweighting factor (ver RIOS-AVILA (2020, p. 75)) a fim de corrigir para o fato de que a obtenção de um contrafactual pode ser identificado incorretamente se o modelo estiver mal-especificado
* aqui, usaremos um logit para estimar w(X), de modo que o contrafactual é a distribuição do grupo 2 (2012) multiplicada pelo novo peso
* os resultados mostram que os erros de especificação (usado para ver a qualidade do modelo e da aproximação RIF) e de reweighting (usado para ver a qualidade da identificação do contrafactual) são insignificantes. VITÓRIA!
* Média
oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2015) [pw = V1028], rif(mean) by(doze) wgt(0) robust rwlogit($xlist_rwt)
eststo media_1215_rwt

* Gini
oaxaca_rif sal_hab $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2015) [pw = V1028], rif(gini) by(doze) wgt(0) robust scale(100) rwlogit($xlist_rwt)
eststo gini_1215_rwt

** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis 
foreach i in 1 10 50 90 99 {
	display `i'
	quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2015) [pw = V1028], rif(q(`i')) by(doze) wgt(0) robust rwlogit($xlist_rwt)
	eststo p`i'_1215_rwt
}

*** 2015-2020
* Média
quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2015 | Ano == 2020) [pw = V1028], rif(mean) by(quinze) wgt(0) robust rwlogit($xlist_rwt)
eststo media_1520_rwt

* Gini
quietly oaxaca_rif sal_hab $xlist_oaxaca if ativo == 1 & (Ano == 2015 | Ano == 2020) [pw = V1028], rif(gini) by(quinze) wgt(0) robust scale(100) rwlogit($xlist_rwt)
eststo gini_1520_rwt

** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis 
foreach i in 1 10 50 90 99 {
	display `i'
	quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2015 | Ano == 2020) [pw = V1028], rif(q(`i')) by(quinze) wgt(0) robust rwlogit($xlist_rwt)
	eststo p`i'_1520_rwt
}

*** 2012-2020
* Média
quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2020) [pw = V1028], rif(mean) by(doze) wgt(0) robust rwlogit($xlist_rwt)
eststo media_1220_rwt

* Reweighting
quietly oaxaca_rif sal_hab $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2020) [pw = V1028], rif(gini) by(doze) wgt(0) robust scale(100) rwlogit($xlist_rwt)
eststo gini_1220_rwt


** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis 
foreach i in 1 10 50 90 99 {
	display `i'
	quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2020) [pw = V1028], rif(q(`i')) by(doze) wgt(0) robust rwlogit($xlist_rwt)
	eststo p`i'_1220_rwt
}

*** 2020-2021
* Média
quietly oaxaca_rif lsalh_efet $xlist_oaxaca if ativo == 1 & (Ano == 2021 | Ano == 2020) [pw = V1028], rif(mean) by(vinte) wgt(0) robust rwlogit($xlist_rwt)
eststo media_2021_rwt

* Gini
quietly oaxaca_rif sal_efet $xlist_oaxaca if ativo == 1 & (Ano == 2021 | Ano == 2020) [pw = V1028], rif(gini) by(vinte) wgt(0) robust scale(100) rwlogit($xlist_rwt)
eststo gini_2021_rwt

** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis 
foreach i in 1 10 50 90 99 {
	display `i'
	quietly oaxaca_rif lsalh_efet $xlist_oaxaca if ativo == 1 & (Ano == 2021 | Ano == 2020) [pw = V1028], rif(q(`i')) by(vinte) wgt(0) robust rwlogit($xlist_rwt)
	eststo p`i'_2021_rwt
}


** Médias
esttab media_1215_rwt media_1520_rwt media_1220_rwt media_2021_rwt, ///
title("Resultados da Decomposição das Médias") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide

** Ginis
esttab gini_1215_rwt gini_1520_rwt gini_1220_rwt gini_2021_rwt, ///
title("Resultados da Decomposição dos Ginis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide


** 1º Percentil
esttab p1_1215_rwt p1_1520_rwt p1_1220_rwt p1_2021_rwt, ///
title("Resultados da Decomposição dos 1º Percentis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide

** 1º Decil
esttab p10_1215_rwt p10_1520_rwt p10_1220_rwt p10_2021_rwt, ///
title("Resultados da Decomposição dos 1º Decis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide

** Mediana
esttab p50_1215_rwt p50_1520_rwt p50_1220_rwt p50_2021_rwt, ///
title("Resultados da Decomposição das Medianas") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide

** 9º Decil
esttab p90_1215_rwt p90_1520_rwt p90_1220_rwt p90_2021_rwt, ///
title("Resultados da Decomposição dos 9º Decis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide

** 99º Percentil
esttab p99_1215_rwt p99_1520_rwt p99_1220_rwt p99_2021_rwt, ///
title("Resultados da Decomposição dos 99º Percentis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se(5) b(5) brackets varwidth(13) compress wide
*/


/* Teste do decil mais rico em 20-21 usando rendimento habituais e efetivos
cls
eststo clear

* Efetivo com weights
forvalues i = 92(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif lsalh_efet $xlist_oaxaca if ativo == 1 & (Ano == 2020 | Ano == 2021) [pw = V1028], rif(q(`i')) by(vinte) wgt(0)
}

esttab, ///
title("Resultados da Decomposição dos 10% mais ricos") ///
nonumbers addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
ci(5) b(5) brackets varwidth(13) compress 

eststo clear

* Efetivo sem weights
forvalues i = 92(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif lsalh_efet $xlist_oaxaca if ativo == 1 & (Ano == 2020 | Ano == 2021), rif(q(`i')) by(vinte) wgt(0)
}

esttab, ///
title("Resultados da Decomposição dos 10% mais ricos") ///
nonumbers addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
ci(5) b(5) brackets varwidth(13) compress

eststo clear

* Habitual com weights
forvalues i = 92(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif lsalh $xlist_oaxaca if ativo == 1 & (Ano == 2020 | Ano == 2021) [pw = V1028], rif(q(`i')) by(vinte) wgt(0)
}

esttab, ///
title("Resultados da Decomposição dos 10% mais ricos") ///
nonumbers addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
ci(5) b(5) brackets varwidth(13) compress

eststo clear

* Habitual sem weights
forvalues i = 92(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif lsalh $xlist_oaxaca if ativo == 1 & (Ano == 2020 | Ano == 2021), rif(q(`i')) by(vinte) wgt(0)
}

esttab, ///
title("Resultados da Decomposição dos 10% mais ricos") ///
nonumbers addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
ci(5) b(5) brackets varwidth(13) compress
*/

