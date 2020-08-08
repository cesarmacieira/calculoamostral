Definições
==========

Poder do teste
--------------

Os testes estatísticos procuram evidências para rejeitar a hipótese nula
e concluir que existe diferença entre os grupos. No entanto, há sempre a
possibilidade de encontrar uma diferença entre os grupos quando na
verdade não existe. Isso é chamado de erro de tipo I. Da mesma forma, é
possível existir uma diferença e o teste não ser capaz de identificá-la.
Este tipo de erro é chamado de erro do tipo II. O poder do teste é a
probabilidade de encontrar uma diferença significativa quando realmente
essa diferença existe. Em outras palavras, o poder é a probabilidade de
você rejeitar a hipótese nula quando realmente se deve rejeita-la. É
geralmente aceito que o poder do teste seja igual ou maior que 0,8.

Nível de significância
----------------------

O nível de significância é o que conhecemos como erro tipo I, na ciência
da saude, assim como na maiorias das ciências, adota-se um *α* = 5%.

Tamanho do efeito
-----------------

O tamanho do efeito é dado por: $d=\\frac{\\mu\_1-\\mu\_2}{\\sigma}$

Sendo, *μ*<sub>1</sub> média do primeiro grupo, *μ*<sub>2</sub> média do
segundo grupo e σ desvio padrão comum entre os grupos. Segundo (J. Cohen
1988), pode-se definir como:

-   Efeito pequeno: *d* = 0, 2;
-   Efeito médio: *d* = 0, 5;
-   Efeito grande: *d* = 0, 8.

Correlação Intraclasse
----------------------

Os indivíduos que apresentaram as piores pontuações na primeira medida
tendem a apresentar as piores pontuações na segunda medida, o que
evidencia uma correlação entre as medidas realizadas no mesmo indivíduo.
Nesse caso, pode-se pensar na correlação restrita ao intervalo 0 e 1.
Sendo que uma correlação igual à zero indicaria que não existe nenhuma
relação entre as medidas no mesmo indivíduo, ao passo que uma correlação
igual a um indicaria que todos os indivíduos mantiveram a mesma
pontuação.

Estimação para proporção
========================

Populações finitas
------------------

Para estimação de proporções para populações finitas, (Bolfarine e
Bussab 2005), a expressão para o tamanho da amostra é dada por:

$n=\\frac{N}{\\frac{(N-1)E^2}{p(1-p)z^2\_\\alpha}+1}$

<p>
Em que *z*<sub>*α*</sub> é o percentil da distribuição normal
correspondente ao nível de significância *α*, *E* a margem de erro, *N*
o tamanho da população e *p* alguma proporção de interesse provinda do
instrumento de pesquisa.
</p>
<p>
Para possibilitar o cálculo do tamanho da amostra para as diferentes
variáveis com os níveis especificados de significância e margem de erro,
foi utilizado um p de 50%, uma vez que o tamanho da amostra obtido sobre
esta suposição é máximo, suficiente para qualquer possível resultado que
venha a ocorrer (Hulley, et al. 2006).
</p>

``` r
EstPropPopFinita <- function(populacao,proporcao){
  erro <- c(0.005,0.01,0.02,0.03,0.04,0.05,0.06,0.07)
  N5 <- populacao/((((populacao-1)*(erro^2))/(proporcao*(1-proporcao)*(qnorm(1-(0.05/2))^2)))+1)
  N10 <- populacao/((((populacao-1)*(erro^2))/(proporcao*(1-proporcao)*(qnorm(1-(0.10/2))^2)))+1)
  tabela <- data.frame(erro=c("0.5%","1%","2%","3%","4%","5%","6%","7%"),
                       tamanhoamostral5=N5, tamanhoamostral10=N10)
  colnames(tabela) <- c("Erro", "Amostra - 5% de significância","Amostra - 10% de significância")
  return(tabela)
}
```

**Exemplo:** Supondo *N = 231*, pode-se verificar o tamanho da amostra
condicionado ao nível de significância e margem de erro.

``` r
EstPropPopFinita(231,0.5)
```

    ##   Erro Amostra - 5% de significância Amostra - 10% de significância
    ## 1 0.5%                      229.6252                      229.05281
    ## 2   1%                      225.5971                      223.40334
    ## 3   2%                      210.8055                      203.34202
    ## 4   3%                      190.0385                      176.87077
    ## 5   4%                      167.0055                      149.60480
    ## 6   5%                      144.4896                      124.85768
    ## 7   6%                      124.0487                      103.85975
    ## 8   7%                      106.2796                       86.63988

<p>
Considerando um *Erro = 5%*, têm-se que *n = 144* (como no exemplo
anterior). Isto posto, realizando 1000 simulações é possível perceber
que o intervalo com 95% de confiança têm a margem de erro esperada
</p>

``` r
Amostra <- c()
p <-rbinom(231,1,0.5)
for (i in 1:1000){
  Amostra[i] <- mean(sample(p,144,0))
}
paste0("Proporção estimada: ",round(mean(Amostra),2),"; I.C.(95%) = ","[",round(quantile(Amostra,0.025),2),";",round(quantile(Amostra,0.975),2),"]")
```

    ## [1] "Proporção estimada: 0.47; I.C.(95%) = [0.42;0.52]"

<p>
Transformando a fórmula para o cálculo do tamanho amostral, é possível
verificar se os erros reais correspondem aos erros estimados pela
fórmula:
</p>

$n=\\frac{N}{\\frac{(N-1)E^2}{p(1-p)z^2\_\\alpha}+1} \\therefore E = Erro = \\sqrt\\frac{\\frac{N}{n}-1}{\\frac{(N-1)}{p(1-p)z^2\_\\alpha}}$

``` r
N <- 231
p = aux5 = aux10 = c()
erro5 = erro10 = matrix(nrow=10,ncol=8)
for(i in 1:10){
  p[i] <- mean(rbinom(384, 1, 0.3))
  aux5 <- sqrt(((N/EstPropPopFinita(231,0.5)[,2])-1)/((N-1)/(p[i]*(1-p[i])*(qnorm(1-(0.05/2))^2))))*100
  aux10 <- sqrt(((N/EstPropPopFinita(231,0.5)[,3])-1)/((N-1)/(p[i]*(1-p[i])*(qnorm(1-(0.1/2))^2))))*100
  for(j in 1:8){
    erro5[i,j] <- aux5[j]
    erro10[i,j] <- aux10[j]
  }
}
media5 = media10 = c()

for(k in 1:8){
  media5[k] <- mean(erro5[,k])
  media10[k] <- mean(erro10[,k])
}
tabela <- data.frame(erroreal=c("0.5","1","2","3","4","5","6","7"),
                     mediaerro5=c(media5),
                     amostra5=EstPropPopFinita(231,0.5)[,2],
                     amostra10=EstPropPopFinita(231,0.5)[,3])
colnames(tabela) <- c("Erro real (%)", "Erro estimado (%)","Amostra - 5% de sig","Amostra - 10% de sig")
rownames(tabela) <- NULL
tabela
```

    ##   Erro real (%) Erro estimado (%) Amostra - 5% de sig Amostra - 10% de sig
    ## 1           0.5         0.4606626            229.6252            229.05281
    ## 2             1         0.9213252            225.5971            223.40334
    ## 3             2         1.8426505            210.8055            203.34202
    ## 4             3         2.7639757            190.0385            176.87077
    ## 5             4         3.6853010            167.0055            149.60480
    ## 6             5         4.6066262            144.4896            124.85768
    ## 7             6         5.5279514            124.0487            103.85975
    ## 8             7         6.4492767            106.2796             86.63988

Populações infinitas
--------------------

<p>
A expressão para o tamanho da amostra para estimação de proporções para
populações infinitas dada por:
</p>

$n=\\frac{p(1-p)z^2\_\\alpha}{E^2}$

<p>
Em que *z*<sub>*α*</sub> é o percentil da distribuição normal
correspondente ao nível de significância *α*, p é a proporção e *E* a
margem de erro.
</p>

``` r
EstPropPopInfinita <- function(proporcao){
  erro <- c(0.005,0.01,0.02,0.03,0.04,0.05,0.06,0.07)
  N5 <- (proporcao*(1-proporcao)*(qnorm(1-(0.05/2))^2))/(erro^2)
  N10 <- (proporcao*(1-proporcao)*(qnorm(1-(0.10/2))^2))/(erro^2)
  tabela <- data.frame(erro=c("0,5%","1%","2%","3%","4%","5%","6%","7%"),
                       tamanhoamostral5=N5, tamanhoamostral10=N10)
  colnames(tabela) <- c("Erro", "Amostra - 5% de significância","Amostra - 10% de significância")
  return(tabela)
}
```

**Exemplo:** Supondo *p* = 10, 5/1000.

``` r
EstPropPopInfinita(10.5/1000)
```

    ##   Erro Amostra - 5% de significância Amostra - 10% de significância
    ## 1 0,5%                   1596.471871                    1124.396804
    ## 2   1%                    399.117968                     281.099201
    ## 3   2%                     99.779492                      70.274800
    ## 4   3%                     44.346441                      31.233245
    ## 5   4%                     24.944873                      17.568700
    ## 6   5%                     15.964719                      11.243968
    ## 7   6%                     11.086610                       7.808311
    ## 8   7%                      8.145265                       5.736718

**Exemplo:** Supondo o maior tamanho amostral possível (*p* = 0.5).

``` r
EstPropPopInfinita(0.5)
```

    ##   Erro Amostra - 5% de significância Amostra - 10% de significância
    ## 1 0,5%                    38414.5882                     27055.4345
    ## 2   1%                     9603.6471                      6763.8586
    ## 3   2%                     2400.9118                      1690.9647
    ## 4   3%                     1067.0719                       751.5398
    ## 5   4%                      600.2279                       422.7412
    ## 6   5%                      384.1459                       270.5543
    ## 7   6%                      266.7680                       187.8850
    ## 8   7%                      195.9928                       138.0379

<p>
Considerando um *E**r**r**o* = 5%, têm-se que *n* = 385, pelo exemplo
anterior. Isto posto, realizando 1000 simulações é possível perceber que
o intervalo com 95% de confiança têm a margem de erro esperada.
</p>

``` r
Amostra <- c()
for (i in 1:1000){
  Amostra[i] <- mean(rbinom(231,1,0.5))
}
paste0("Proporção estimada: ",round(mean(Amostra),2),"; I.C.(95%) = ","[",round(quantile(Amostra,0.025),2),";",round(quantile(Amostra,0.975),2),"]")
```

    ## [1] "Proporção estimada: 0.5; I.C.(95%) = [0.44;0.56]"

<p>
Transformando a fórmula para o cálculo do tamanho amostral para
proporção de populações infinitas, é possível verificar se os erros
reais correspondem aos erros estimados pela fórmula:
</p>

$n=\\frac{p(1-p)z^2\_\\alpha}{E^2} \\therefore E = Erro = \\sqrt\\frac{p(1-p)z^2\_\\alpha}{n}$

``` r
p = aux5 = aux10 = c()
erro5 = erro10 = matrix(nrow=10,ncol=8)
for(i in 1:10){
  p[i] <- mean(rbinom(1000, 1, 0.0105))
  aux5 <- (sqrt((p[i]*(1-p[i])*(qnorm(1-(0.05/2))^2))/EstPropPopInfinita(10.5/1000)[,2]))*100
  aux10 <- (sqrt((p[i]*(1-p[i])*(qnorm(1-(0.05/2))^2))/EstPropPopInfinita(10.5/1000)[,3]))*100
  for(j in 1:8){
    erro5[i,j] <- aux5[j]
    erro10[i,j] <- aux10[j]
  }
}
media5 = media10 = c()
for(k in 1:8){
  media5[k] <- mean(erro5[,k])
  media10[k] <- mean(erro10[,k])
}
tabela <- data.frame(erroreal=c("0.5","1","2","3","4","5","6","7"),
                     mediaerro5=c(media5),
                     amostra5=EstPropPopInfinita(10.5/1000)[,2],
                     amostra10=EstPropPopInfinita(10.5/1000)[,3])
colnames(tabela) <- c("Erro real (%)", "Erro estimado (%)","Amostra - 5% de sig","Amostra - 10% de sig")
rownames(tabela) <- NULL
tabela
```

    ##   Erro real (%) Erro estimado (%) Amostra - 5% de sig Amostra - 10% de sig
    ## 1           0.5         0.4562045         1596.471871          1124.396804
    ## 2             1         0.9124091          399.117968           281.099201
    ## 3             2         1.8248181           99.779492            70.274800
    ## 4             3         2.7372272           44.346441            31.233245
    ## 5             4         3.6496362           24.944873            17.568700
    ## 6             5         4.5620453           15.964719            11.243968
    ## 7             6         5.4744544           11.086610             7.808311
    ## 8             7         6.3868634            8.145265             5.736718

Comparação de proporções
========================

Transversal
-----------

Para calcular o tamanho amostral necessário para comparar duas
proporções, foi utilizada a metodologia proposta por Fleiss, 1981, em
que a quantidade total de indivíduos em cada grupo é dada por:

$N=\\frac{\[z\_\\alpha 2(\\overline{pq})^\\frac{1}{2} + 2z\_\\beta (p1q1 + p2q2)^\\frac{1}{2}\]^2}{(p1-p2)^2}$

<p>
Onde:
</p>

-   *p*1 é a proporção do evento no grupo 1.
-   *p*2 é a proporção do evento no grupo 2.(Vale ressaltar que *p*2 é
    definido a partir do tamanho do efeito e de *p*1)
-   *q*1 = 1 − *p*1;
-   *q*2 = 1 − *p*2;
-   $\\overline{p} = \\frac{(p1+p2)}{2}$;
-   $\\overline{q}$ = $1-\\overline{p}$;
-   *z*<sub>*α*</sub> é o percentil da distribuição normal
    correspondente ao nível de significância.
-   *z*<sub>*β*</sub> é o percentil da distribuição normal
    correspondente ao poder do teste.

``` r
AmostraCompPropTransversal <- function (p1,TamanhodeEfeito,Significancia){
  p2 <- 1 
  estimativaefeito <- 0
  while( abs(TamanhodeEfeito-estimativaefeito) > 0.0001 ){
    estimativaefeito <- abs(2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2)))
    p2 <- p2-0.00001
  }
  q1 <- 1-p1
  q2 <- 1-p2
  pbarra <- (p1+p2)/2
  qbarra <- 1-pbarra
  zalfa <- qnorm(1-(Significancia/2))
  zbeta <- qnorm(seq(0.80,0.96, 0.05))
  numerador <- ( (zalfa * sqrt(2 * pbarra*qbarra) + zbeta * sqrt(p1*q1 + p2*q2) )^2) 
  denominador <-  ( (p1-p2)^2 )
  N <- numerador/denominador
  tabela <- (data.frame(poder=c("80,00%","85,00%","90,00%","95,00%"),tamanhoamostral=N))
  colnames(tabela) <- c("Poder", "Tamanho amostral (N)")
  return(tabela)
}
```

**Exemplo:** Supondo tamanho do efeito médio (*d*) = 0, 5, nível de
significância (*α*) = 5% e *p*1 = 0, 50.

``` r
AmostraCompPropTransversal(0.5,0.5,0.05)
```

    ##    Poder Tamanho amostral (N)
    ## 1 80,00%             63.16415
    ## 2 85,00%             72.04866
    ## 3 90,00%             84.05766
    ## 4 95,00%            103.55870

Longitudinal
------------

<p>
De acordo (J. Cohen 1988), pode-se definir o tamanho do efeito para
comparações de proporções como:
</p>

$h={2arcsen(\\sqrt{p1})-2arcsen(\\sqrt{p2})}$

-   Efeito pequeno: *h* = 0,2;
-   Efeito médio: *h* = 0,5;
-   Efeito grande: *h* = 0,8.

<p>
Para calcular o tamanho da amostra para verificar a diferença da
proporção entre os grupos 1 e 2 persistentes ao longo do tempo, foi
utilizada a metodologia proposta por (Diggle, et al. 2002), para
comparações de dois grupos em respostas binárias para dados dependentes.
</p>
<p>
Em que a quantidade total de indivíduos em cada grupo é dada por:
</p>

$N=\\frac{\[z\_\\alpha 2(\\overline{pq})^\\frac{1}{2} + 2z\_\\beta (p1q1 + p2q2)^\\frac{1}{2}\]^2 (1+(n-1)\\rho)}{n(p1-p2)^2}$

<p>
Onde:
</p>

-   *p*1 é a proporção do evento no grupo 1.
-   *p*2 é a proporção do evento no grupo 2.
-   *q*1 = 1 − *p*1;
-   *q*2 = 1 − *p*2;
-   $\\overline{p} = \\frac{(p1+p2)}{2}$;
-   $\\overline{q}$ = $1-\\overline{p}$;
-   *ρ* é a correlação intraclasse.
-   *n* é número de medidas no mesmo indivíduo.
-   *z*<sub>*α*</sub> é o percentil da distribuição normal
    correspondente ao nível de significância.
-   *z*<sub>*β*</sub> é o percentil da distribuição normal
    correspondente ao poder do teste.

``` r
AmostraCompPropLongitudinal <- function (p1,TamanhodeEfeito,Significancia,CorrIntraclasse,MedidasRealizadas){
  p2 <- 1 
  estimativaefeito <- 0
  while( abs(TamanhodeEfeito-estimativaefeito) > 0.0001 ){
    estimativaefeito <- abs(2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2)))
    p2 <- p2-0.00001
  }
  q1 <- 1-p1;
  q2 <- 1-p2;
  pbarra <- (p1+p2)/2;
  qbarra <- 1-pbarra
  zalfa <- qnorm(1-(Significancia/2));zbeta <- qnorm(seq(0.80,0.96, 0.05))
  numerador <- ( (zalfa * sqrt(2 * pbarra*qbarra) + zbeta * sqrt(p1*q1 + p2*q2) )^2) * (1+(MedidasRealizadas-1)*CorrIntraclasse)
  denominador <- MedidasRealizadas * ( (p1-p2)^2 )
  N <- numerador/denominador
  tabela <- (data.frame(poder=c("80,00%","85,00%","90,00%","95,00%"),tamanhoamostral=N))
  colnames(tabela) <- c("Poder", "Tamanho amostral (N)")
  return(tabela)
}
```

**Exemplo:** Supondo Tamanho do efeito médio (*h*) = 0, 5, Correlação
Intraclasse(*ρ*) = 0, 5, *p*1 = 0, 10, nível de significância (*α*) = 5%
e número de medidas no mesmo indivíduo (*n*) = 5.

``` r
AmostraCompPropLongitudinal(0.1,0.5,0.05,0.5,5)
```

    ##    Poder Tamanho amostral (N)
    ## 1 80,00%             39.26544
    ## 2 85,00%             44.79289
    ## 3 90,00%             52.26461
    ## 4 95,00%             64.39844

Comparação de médias
====================

2 grupos ou mais grupos - Amostras independentes
------------------------------------------------

<p>
Para calcular o tamanho amostral, foi utilizada a metodologia proposta
por (Fleiss 1986) para comparações de dois grupos em respostas contínuas
para amostras independentes. Caso haja mais de dois grupos, o nível de
significância foi corrigido, pelo método de bonferroni (Miller 1991),
para realizar as comparações dois-a-dois.
</p>
<p>
A quantidade total de indivíduos em cada grupo considerando somente 2
grupos é dada por:
</p>

$N= \\frac{2\\left(z\_{\\alpha}+z\_\\beta\\right)^2}{d^2}$

<p>
A quantidade total de indivíduos em cada grupo considerando mais 2
grupos é dada por:
</p>

$N= \\frac{2\\left(z\_{\\frac{\\alpha}{g}}+z\_\\beta\\right)^2}{d^2}$

<p>
Onde:
</p>

-   *z*<sub>*α*</sub> é o percentil da distribuição normal
    correspondente ao nível de significância;
-   *z*<sub>*β*</sub> é o percentil da distribuição normal
    correspondente ao poder do teste;
-   *d* é o tamanho do efeito;
-   *g* é o número de grupos.

<p>
Para utilizar a metodologia acima, está se fazendo duas suposições:
distribuição normal e o desvio padrão populacional entre grupos são os
mesmos. Caso essas suposições sejam violadas, pode-se usar para comparar
os grupos um teste não paramétrico correspondente. Dessa forma, sendo N
o tamanho amostral necessário para o teste paramétrico garantir um poder
adequado na comparação dos grupos em amostras independentes, o tamanho
da amostra necessário para o teste não paramétrico correspondente será,
de acordo com (Lehmann 1975):
</p>

$N\*=\\frac{N}{0,955}$

``` r
AmostraCompMedias2ouMaisGruposIndependentes <- function(TamanhodeEfeito,Significancia,NumeroGrupos){
  if(NumeroGrupos==2){
    zalfa <- qnorm(1-(Significancia/2))
  }else if(NumeroGrupos>2){
    zalfa <- qnorm(1-(Significancia/(2*NumeroGrupos)))
  }
  zbeta <- qnorm(seq(0.80,0.96, 0.05))
  N <- (2*((zalfa+zbeta)^2))/(TamanhodeEfeito^2)
  Ncorrigido <- N/0.955
  tabela <- (data.frame(poder=c("80,00%","85,00%","90,00%","95,00%"),
                        tamanhoamostralparametrico=N,
                        tamanhoamostralnaoparametrico=Ncorrigido))
  colnames(tabela) <- c("Poder", "Amostra por grupo - Paramétrico",
                        "Amostra por grupo -  Não paramétrico")
  return(tabela)
}
```

**Exemplo:** Supondo tamanho do efeito médio (*d*) = 0, 5 e nível de
significância (*α*) = 5%.

``` r
AmostraCompMedias2ouMaisGruposIndependentes(0.5,0.05,2)
```

    ##    Poder Amostra por grupo - Paramétrico Amostra por grupo -  Não paramétrico
    ## 1 80,00%                        62.79104                             65.74978
    ## 2 85,00%                        71.82718                             75.21170
    ## 3 90,00%                        84.05938                             88.02030
    ## 4 95,00%                       103.95768                            108.85621

1 grupo - 2 medidas (Antes e depois) (dependentes) (T=1)
--------------------------------------------------------

<p>
Para calcular o tamanho das amostras foi utilizada a metodologia
proposta por Dupont e Plummer (1990) para comparações de dois grupos em
respostas contínuas para amostras pareadas.
</p>
<p>
A quantidade total de indivíduos em cada grupo é dada por:
</p>

$N=\\frac{ \\left(t\_{n-1,\\frac{\\alpha}{2}}+t\_{n-1,\\frac{\\beta}{2}}\\right)}{d^2}$

<p>
Onde:
</p>

-   $t\_{n-1,\\frac{\\alpha}{2}}$ é o percentil da distribuição t de
    Student com *n* − 1 graus de liberdade correspondente ao nível de
    significância;
-   $t\_{n-1,\\frac{\\beta}{2}}$ é o percentil da distribuição *t* de
    Student com *n* − 1 graus de liberdade correspondente ao poder do
    teste;
-   *d* é o tamanho do efeito.

<p>
A equação é então solucionada através de métodos iterativos, dado que
“n” aparece em ambos os lados da equação.
</p>
<p>
Para utilizar a metodologia acima duas suposições são feitas:
distribuição normal e o desvio padrão populacional é o mesmo. Caso essas
suposições sejam violadas, utiliza-se na comparação dos dois grupos um
teste não paramétrico correspondente. Dessa forma, sendo N o tamanho
amostral necessário para o teste paramétrico garantir um poder adequado
na comparação de dois grupos em amostras pareadas, o tamanho da amostra
necessário para o teste não paramétrico correspondente será, de acordo
com Lehmann (1975):
</p>

$N\*=\\frac{N}{0,955}$

``` r
AmostraCompMedias2GruposDependentes <- function(TamanhodeEfeito,Significancia){
  if(!require(pwr)){install.packages("pwr"); require(pwr)}
  Nparametrico <- c(pwr.t.test(d=TamanhodeEfeito , sig.level = Significancia, power =0.80 , type = c("paired"))$n,
                    pwr.t.test(d=TamanhodeEfeito , sig.level = Significancia, power =0.85 , type = c("paired"))$n,
                    pwr.t.test(d=TamanhodeEfeito , sig.level = Significancia, power =0.90 , type = c("paired"))$n,
                    pwr.t.test(d=TamanhodeEfeito , sig.level = Significancia, power =0.95 , type = c("paired"))$n)
  Ncorrigido <- Nparametrico/0.955
  tabela <- (data.frame(poder=c("80,00%","85,00%","90,00%","95,00%"),
                        tamanhoamostralparametrico=Nparametrico,
                        tamanhoamostralnaoparametrico=Ncorrigido))
  colnames(tabela) <- c("Poder", "Amostra por grupo - Paramétrico",
                        "Amostra por grupo -  Não paramétrico")
  return(tabela)
}
```

**Exemplo:** Supondo tamanho do efeito grande (*d*) = 0, 8 e nível de
significância (*α*) = 5%.

``` r
AmostraCompMedias2GruposDependentes(0.8,0.05)
```

    ##    Poder Amostra por grupo - Paramétrico Amostra por grupo -  Não paramétrico
    ## 1 80,00%                        14.30276                             14.97672
    ## 2 85,00%                        16.06292                             16.81981
    ## 3 90,00%                        18.44624                             19.31543
    ## 4 95,00%                        22.32455                             23.37650

2 grupos ou mais - Amostras dependentes (T\>=2)
-----------------------------------------------

<p>
Para calcular o tamanho amostral para possibilitar a comparação entre os
grupos, foi utilizada a metodologia proposta por (Diggle, et al. 2002)
para comparações de dois grupos em respostas contínuas para amostras
dependentes (ao longo do tempo). Supondo que existe três grupos, o nível
de significância foi corrigido, pelo método de bonferroni (Miller 1991),
para realizar as comparações dois-a-dois.
</p>
<p>
Para dois grupos, a quantidade total de indivíduos em cada grupo é dada
por:
</p>

$N=\\frac{2\\left(z\_{\\alpha}+z\_\\beta\\right)^2(1+(n-1)\\rho)}{n\\left(\\frac{\\mu\_1-\\mu\_2}{\\sigma}\\right)^2}=\\frac{2\\left(z\_{\\alpha}+z\_\\beta\\right)^2 (1+(n-1)\\rho)}{nd^2}$

<p>
Para mais de dois grupos, a quantidade total de indivíduos em cada grupo
é dada por:
</p>

$N=\\frac{2\\left(z\_{\\frac{\\alpha}{g}}+z\_\\beta\\right)^2(1+(n-1)\\rho)}{n\\left(\\frac{\\mu\_1-\\mu\_2}{\\sigma}\\right)^2}=\\frac{2\\left(z\_{\\frac{\\alpha}{g}}+z\_\\beta\\right)^2 (1+(n-1)\\rho)}{nd^2}$

<p>
Onde:
</p>

-   *z*<sub>*α*</sub> é o percentil da distribuição normal
    correspondente ao nível de significância;
-   *z*<sub>*β*</sub> é o percentil da distribuição normal
    correspondente ao poder do teste;
-   *n* é o número de medidas realizadas no mesmo indivíduo;
-   *ρ* é a correlação esperada entre as medidas do mesmo indivíduo;
-   *g* é o número de grupos.
-   *μ*<sub>1</sub> é a média populacional no grupo 1;
-   *μ*<sub>2</sub> é a média populacional no grupo 2;
-   *d* é o tamanho do efeito.

``` r
AmostraCompMedias2ouMaisGruposDependentes <- function(TamanhodeEfeito,Correlacao,MedidasRealizadas,Significancia,NumeroGrupos){
  if(NumeroGrupos==2){
    zalfa <- qnorm(1-(Significancia/2))
  }else if(NumeroGrupos>2){
    zalfa <- qnorm(1-(Significancia/(2*NumeroGrupos)))
  }
  zbeta <- qnorm(seq(0.80,0.96, 0.05))
  numerador <- (2*((zalfa+zbeta)^2)) * (1+((MedidasRealizadas-1)*Correlacao) )  
  denominador <- (MedidasRealizadas*TamanhodeEfeito^2)
  N <- numerador/denominador
  tabela <- (data.frame(poder=c("80,00%","85,00%","90,00%","95,00%"),
                        tamanhoamostral=N))
  colnames(tabela) <- c("Poder", "Tamanho amostral por grupo")
  return(tabela)
}
```

**Exemplo:** Supondo tamanho do efeito grande (*d*) = 0, 8, correlação
(*ρ*) = 0, 5, medidas realizadas (*n*) = 2, grupos (*g*) = 3 e nível de
significância (*α*) = 5%.

``` r
AmostraCompMedias2ouMaisGruposDependentes(0.8,0.5,2,0.05,3)
```

    ##    Poder Tamanho amostral por grupo
    ## 1 80,00%                   24.53699
    ## 2 85,00%                   27.58063
    ## 3 90,00%                   31.66296
    ## 4 95,00%                   38.23166

Correlação
==========

<p>
Para calcular o tamanho da amostra para realizar um teste de correlação
de forma a verificar a relação linear existente foi utilizada a seguinte
fórmula (Hulley, et al., 2013):
<p/>

$N=\\left(\\frac{z\_\\alpha+z\_\\beta}{0,5 \\log\\left(\\frac{1+r}{1-r}\\right)}\\right)^2 + 3$

<p>
Onde:
</p>

-   *z*<sub>*α*</sub> é o percentil da distribuição normal
    correspondente ao nível de significância;
-   *z*<sub>*β*</sub> é o percentil da distribuição normal
    correspondente ao poder do teste;
-   *r* é o tamanho do efeito.

``` r
AmostraCorrelacao <- function (TamanhodeEfeito,Significancia){
  if(!require(pwr)){install.packages("pwr"); require(pwr)}
  N <- rbind(pwr.r.test(r = TamanhodeEfeito, sig.level = Significancia, power = 0.80)$n,
             pwr.r.test(r = TamanhodeEfeito, sig.level = Significancia, power = 0.85)$n,
             pwr.r.test(r = TamanhodeEfeito, sig.level = Significancia, power = 0.90)$n,
             pwr.r.test(r = TamanhodeEfeito, sig.level = Significancia, power = 0.95)$n)
  tabela <- data.frame(poder=c("80,00%","85,00%","90,00%","95,00%"),
                        tamanhoamostralparametrico=N)
  colnames(tabela) <- c("Poder", "Tamanho da amostra")
  return(tabela)
}
```

**Exemplo:** Supondo tamanho de efeito (*d*) = 0, 3 e nível de
significância (*α*) = 5%.

``` r
AmostraCorrelacao(0.3,0.05)
```

    ##    Poder Tamanho da amostra
    ## 1 80,00%           84.07364
    ## 2 85,00%           95.85551
    ## 3 90,00%          111.80684
    ## 4 95,00%          137.75868

Simulações
==========

Proporções
----------

``` r
set.seed(13)
n <- 384
propreal <- 0.3
p <- sum(rbinom(n,1,propreal))/n

SimulacaoProp <- function (propreal,n){
  p <- sum(rbinom(n,1,propreal))/n
  ic90real <- paste0("[",round(propreal-0.1,2),";",round(propreal+0.1,2),"]")
  ic95real <- paste0("[",round(propreal-0.05,2),";",round(propreal+0.05,2),"]")
  ic99real <- paste0("[",round(propreal-0.01,2),";",round(propreal+0.01,2),"]")
  ic90 <- paste0("[",round(0.3-abs(qnorm(0.05))*sqrt(p*(1-p)/n),2),";",round(0.3+abs(qnorm(0.95))*sqrt(p*(1-p)/n),2),"]")
  ic95 <- paste0("[",round(0.3-abs(qnorm(0.025))*sqrt(p*(1-p)/n),2),";",round(0.3+abs(qnorm(0.975))*sqrt(p*(1-p)/n),2),"]")
  ic99 <- paste0("[",round(0.3-abs(qnorm(0.005))*sqrt(p*(1-p)/n),2),";",round(0.3+abs(qnorm(0.995))*sqrt(p*(1-p)/n),2),"]")
  tabela <- data.frame(poder=c("90,00%","95,00%","99,00%"),
                       reais=rbind(ic90real,ic95real,ic99real),
                       intervalos=rbind(ic90,ic95,ic99))
  colnames(tabela) <- c("Nível de confiança", "I.C. Real","I.C. Estimado")
  rownames(tabela) <- NULL
  return(tabela)
}
```

**Exemplo:** Supondo o tamanho amostral (*n*) = 384 e proporção
(*p*) = 0.3.

``` r
SimulacaoProp(0.3,384)
```

    ##   Nível de confiança   I.C. Real I.C. Estimado
    ## 1             90,00%   [0.2;0.4]   [0.26;0.34]
    ## 2             95,00% [0.25;0.35]   [0.25;0.35]
    ## 3             99,00% [0.29;0.31]   [0.24;0.36]

Médias
------

``` r
set.seed(13)
SimulacaoMedia <- function(nsim,n1,media1,dp1,n2,media2,dp2){
  valoresp <- vector(length = nsim)
  significancia20 = significancia15 = significancia10 = significancia5 = significancia1 = vector(length = nsim)
  for(i in 1:nsim){
    amostra1 <- rnorm(n1,media1,dp1)
    amostra2 <- rnorm(n2,media2,dp2)
    valoresp[i] <- t.test(amostra1,amostra2)$p.value
    significancia20[i] <- ifelse(valoresp[i]<0.20,1,0)
    significancia15[i] <- ifelse(valoresp[i]<0.15,1,0)
    significancia10[i] <- ifelse(valoresp[i]<0.10,1,0)
    significancia5[i] <- ifelse(valoresp[i]<0.050,1,0)
    significancia1[i] <- ifelse(valoresp[i]<0.01,1,0)
  }
  poder1 <- sum(significancia20)/nsim
  poder2 <- sum(significancia15)/nsim
  poder3 <- sum(significancia10)/nsim
  poder4 <- sum(significancia5)/nsim
  poder5 <- sum(significancia1)/nsim
  print(paste0("O tamanho de efeito é: ",abs((media1-media2)/dp1)))
  tabela <- data.frame(significancia=c("20%","15%","10%","5%","1%"),
                       poder=rbind(poder1*100,poder2*100,poder3*100,poder4*100,poder5*100))
  colnames(tabela) <- c("Nível de significância", "Poder (%)")
  rownames(tabela) <- NULL
  return(tabela)
}
```

**Exemplo:** Supondo número de simulações = 3000, tamanho amostral
(*n*1) = (*n*2) = 30, média da primeira amostra (*μ*<sub>1</sub>) = 3,
desvio padrão da primeira amostra (*σ*<sub>1</sub><sup>2</sup>) = 1,
média da segunda amostra (*μ*<sub>2</sub>) = 4 e desvio padrão da
segunda amostra (*σ*<sub>2</sub><sup>2</sup>) = 1.

``` r
SimulacaoMedia(3000,30,3,1,30,4,1)
```

    ## [1] "O tamanho de efeito é: 1"

    ##   Nível de significância Poder (%)
    ## 1                    20%  99.30000
    ## 2                    15%  98.96667
    ## 3                    10%  98.40000
    ## 4                     5%  96.60000
    ## 5                     1%  87.83333

Referências
===========

<p>
Bolfarine, Heleno, e Wilton O. Bussab. Elementos de Amostragem. São
Paulo: Blucher, 2005.
</p>
<p>
Cohen, J. Statistical power analysis for the behavioral sciences (2nd
ed.). Hillsdale, NJ: Lawrence Earlbaum Associates, 1988.
</p>
<p>
Diggle, P.J., P.J. Heagerty, K Liang, e S.L. Zeger. Analysis of
longitudinal data. Oxford Statistical Science Series, 2002.
</p>
<p>
Dupont, W. D., & Plummer, W. D. Power and sample size calculations: a
review and computer program. Controlled clinical trials, 11(2), 116-128,
1990.
</p>
<p>
Fleiss, J. L. The design and analysis of clinical experiments. New York:
J. Wiley & Sons, 1986.
</p>
<p>
Hulley, SB, SR Cummings, WS Browner, D Grady, N Hearst, e TB Newman.
Delineando a Pesquisa Clínica: Uma abordagem epidemiológica. Porto
Alegre: Artmed, 2006.
</p>
<p>
Hulley, S. B. et al. Designing clinical research. Lippincott Williams &
Wilkins, 2013.
</p>
<p>
Lehmann, E. L. Nonparametrics. Statistical methods based on ranks. San
Francisco: Holden- Day, 1975.
</p>
<p>
Miller, R. G. Jr. Simultaneous Statistical Inference. New York:
Springer-Verlag, 1991.
</p>
