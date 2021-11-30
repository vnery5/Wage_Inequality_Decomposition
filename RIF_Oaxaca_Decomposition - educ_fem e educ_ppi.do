**** Do file da estimação da decomposição RIF, levando em conta o desenho
**** amostral da PNAD

* Creating a log file to store output
* log using stata_output.txt, text replace


* Limpando a memória e facilitando a visualização dos outputs
clear all    
eststo clear
cls
set more off
set dp comma

* Mudando o diretório e lendo os dados
cd "/Users/vinicius/Desktop/Artigo_PNAD"
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

* Criando interações de educ com ppi
gen educ_ppi = educ if Cor2 == 2
replace educ_ppi = 0 if educ_ppi == .

gen educ2_ppi = educ2 if Cor2 == 2
replace educ2_ppi = 0 if educ2_ppi == .

gen educ3_ppi = educ3 if Cor2 == 2
replace educ3_ppi = 0 if educ3_ppi == .

gen educ4_ppi = educ4 if Cor2 == 2
replace educ4_ppi = 0 if educ4_ppi == .

* e de educ com feminino
gen educ_fem = educ if Sexo == 1
replace educ_fem = 0 if educ_fem == .

gen educ2_fem = educ2 if Sexo == 1
replace educ2_fem = 0 if educ2_fem == .

gen educ3_fem = educ3 if Sexo == 1
replace educ3_fem = 0 if educ3_fem == .

gen educ4_fem = educ4 if Sexo == 1
replace educ4_fem = 0 if educ4_fem == .

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

global x_setor educ educ2 educ3 educ4 educ_ppi educ2_ppi educ3_ppi educ4_ppi educ_fem educ2_fem educ3_fem educ4_fem exper exper2 exper3 exper4 i.Sexo i.Cor2 i.local_dom i.formalidade i.regiao i.setor

* regredindo com erros robustos e os pesos para o ano de 2012 e para as pessoas em idade ativa
reg $y $x_setor if Ano == 2012 & idade >= 15 & idade < 65 [pweight = V1028], vce(robust)

**** Criando o desenho amostral da PNADC
** Postweight: populações de cada posest
* svyset UPA [pweight = V1027], strata(Estrato) poststrata(posest) postweight(V1029) singleunit(scaled)
svyset UPA [pweight = V1028], strata(Estrato) singleunit(scaled)

* regredindo com erros robustos e os pesos para o ano de 2012 e para as pessoas em idade ativa
svy, subpop(ativo if doze == 1): quietly reg $y $x_setor
eststo reg2012

* para os demais anos
svy, subpop(ativo if quinze == 1): quietly reg $y $x_setor
eststo reg2015

svy, subpop(ativo if vinte == 1): quietly reg $y $x_setor
eststo reg2020

svy, subpop(ativo if vinteum == 1): quietly reg $y $x_setor
eststo reg2021

** Exportando para o Latex: esttab e estout
cls
esttab reg2012 reg2015 reg2020 reg2021, b(4) se(4) r2(3) ar2(3)  ///
title("\label{tab1}Estimações (com desenho amostral) por ano") ///
nonumbers mtitles("2012" "2015" "2020" "2021") ///
refcat("2.regiao" "Região" "1.setor" "\rule{0pt}{4ex}Setor", nolabel) ///
label wide brackets compress nobaselevels varwidth(20)


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

** Criando as variáveis dependentes na variável global xlist
* Aqui, agregamos os resultados de educ e exper e normalizamos as variáveis
* categóricas (já que os resultados dependem da categoria base)
global xlist_oaxaca (Educação: educ educ2 educ3 educ4) (Educação_PPI: educ_ppi educ2_ppi educ3_ppi educ4_ppi) (Educação_Fem: educ_fem educ2_fem educ3_fem educ4_fem) (Experiência: exper exper2 exper3 exper4) feminino ppi rural formal (Região: normalize(b.CO N NE S SE)) (Setor: normalize(b.servicos_fin adm_publica agro aloj_alim ativ_mal_definidas comercio construcao educ_saude industria outros_servicos servicos_domesticos transporte))


*** 2012 e 2015 ***
** fazendo uma decomposição do log da renda média por hora entre 2012 e 2015
** entre os ativos (15 <= idade < 65)
* Usando como referência os coeficientes do grupo 2 (2012, wgt(0))
quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2015) [pw = V1028], rif(mean) by(doze) wgt(0) robust
eststo media_1215

** Gini da renda habitual total
quietly oaxaca_rif sal_hab $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2015) [pw = V1028], rif(gini) by(doze) wgt(0) robust scale(100)
eststo gini_1215

** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis 
foreach i in 1 10 50 90 99 {
	display `i'
	quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2015) [pw = V1028], rif(q(`i')) by(doze) wgt(0) robust
	eststo p`i'_1215
}

esttab media_1215 gini_1215 p1_1215 p10_1215 p50_1215 p90_1215 p99_1215, ///
title("Resultados da Decomposição de Várias Estatísticas") ///
nonumbers mtitles("Média" "Gini" "1º" "10º" "Mediana" "90º" "99º") ///
addnote("Grupo 1: 2015; Grupo 2: 2012") ///
se b(5) t(3) brackets varwidth(12) compress



*** 2015 e 2020 ***
quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2015 | Ano == 2020) [pw = V1028], rif(mean) by(quinze) wgt(0) robust
eststo media_1520

** Gini da renda habitual total
quietly oaxaca_rif sal_hab $xlist_oaxaca if ativo == 1 & (Ano == 2015 | Ano == 2020) [pw = V1028], rif(gini) by(quinze) wgt(0) robust scale(100)
eststo gini_1520

** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis 
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



*** 2012 e 2020 ***
quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2020) [pw = V1028], rif(mean) by(doze) wgt(0) robust
eststo media_1220

** Gini da renda habitual total
quietly oaxaca_rif sal_hab $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2020) [pw = V1028], rif(gini) by(doze) wgt(0) robust scale(100)
eststo gini_1220

** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis 
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



*** 2020 e 2021 ***
quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2021 | Ano == 2020) [pw = V1028], rif(mean) by(vinte) wgt(0) robust
eststo media_2021

** Gini da renda habitual total
quietly oaxaca_rif sal_hab $xlist_oaxaca if ativo == 1 & (Ano == 2021 | Ano == 2020) [pw = V1028], rif(gini) by(vinte) wgt(0) robust scale(100)
eststo gini_2021

** Loop sobre o 1º, 10º, mediana, 90º e 99º quantis 
foreach i in 1 10 50 90 99 {
	display `i'
	quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2021 | Ano == 2020) [pw = V1028], rif(q(`i')) by(vinte) wgt(0) robust
	eststo p`i'_2021
}

esttab media_2021 gini_2021 p1_2021 p10_2021 p50_2021 p90_2021 p99_2021, ///
title("Resultados da Decomposição de Várias Estatísticas") ///
nonumbers mtitles("Média" "Gini" "1º" "10º" "Mediana" "90º" "99º") ///
addnote("Grupo 1: 2021; Grupo 2: 2020") ///
se b(5) t(3) brackets varwidth(12) compress



**** Fazendo as tabelas por estatísticas ***
** Média
esttab media_1215 media_1520 media_1220 media_2021, ///
title("Resultados da Decomposição das Médias") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se b(5) t(3) brackets varwidth(12) compress

** Gini
esttab gini_1215 gini_1520 gini_1220 gini_2021, ///
title("Resultados da Decomposição dos Ginis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se b(5) t(3) brackets varwidth(12) compress

** 1º Percentil
esttab p1_1215 p1_1520 p1_1220 p1_2021, ///
title("Resultados da Decomposição dos 1º Percentis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se b(5) t(3) brackets varwidth(12) compress

** 1º Decil
esttab p10_1215 p10_1520 p10_1220 p10_2021, ///
title("Resultados da Decomposição dos 1º Decis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se b(5) t(3) brackets varwidth(12) compress

** Mediana
esttab p50_1215 p50_1520 p50_1220 p50_2021, ///
title("Resultados da Decomposição das Medianas") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se b(5) t(3) brackets varwidth(12) compress

** 9º Decil
esttab p90_1215 p90_1520 p90_1220 p90_2021, ///
title("Resultados da Decomposição dos 9º Decis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se b(5) t(3) brackets varwidth(12) compress

** 99º Percentil
esttab p99_1215 p99_1520 p99_1220 p99_2021, ///
title("Resultados da Decomposição dos 99º Percentis") ///
nonumbers mtitles("2012-2015" "2015-2020" "2012-2020" "2020-2021") ///
addnote("Grupo 1: 2º Ano; Grupo 2: 1º Ano") ///
se b(5) t(3) brackets varwidth(12) compress



/*
**** DECOMPOSICAO POR PERCENTIL
*** 2012_2015
** limpando a memória das outras estimações
eststo clear

** Loop sobre todos os quantis para a decomposição
forvalues i = 1(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2015) [pw = V1028], rif(q(`i')) by(doze) wgt(0) nose
}
set dp period
esttab using 2012_2015.csv


*** 2015_2020
** limpando a memória das outras estimações
eststo clear

** Loop sobre todos os quantis para a decomposição
forvalues i = 1(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2015 | Ano == 2020) [pw = V1028], rif(q(`i')) by(quinze) wgt(0) nose
}
set dp period
esttab using 2015_2020.csv


*** 2012_2020
** limpando a memória das outras estimações
eststo clear

** Loop sobre todos os quantis para a decomposição
forvalues i = 1(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2012 | Ano == 2020) [pw = V1028], rif(q(`i')) by(doze) wgt(0) nose
}
set dp period
esttab using 2012_2020.csv


*** 2020_2021
** limpando a memória das outras estimações
eststo clear

** Loop sobre todos os quantis para a decomposição
forvalues i = 1(1)99 {
	display "Calculando para o " `i' "º percentil (" `i' "%)..."
	eststo: quietly oaxaca_rif $y $xlist_oaxaca if ativo == 1 & (Ano == 2020 | Ano == 2021) [pw = V1028], rif(q(`i')) by(vinte) wgt(0) nose
}
set dp period
esttab using 2020_2021.csv
*/
