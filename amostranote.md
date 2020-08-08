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

O tamanho do efeito é dado por:

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

    EstPropPopFinita <- function(populacao,proporcao){
      erro <- c(0.005,0.01,0.02,0.03,0.04,0.05,0.06,0.07)
      N5 <- populacao/((((populacao-1)*(erro^2))/(proporcao*(1-proporcao)*(qnorm(1-(0.05/2))^2)))+1)
      N10 <- populacao/((((populacao-1)*(erro^2))/(proporcao*(1-proporcao)*(qnorm(1-(0.10/2))^2)))+1)
      tabela <- data.frame(erro=c("0.5%","1%","2%","3%","4%","5%","6%","7%"),
                           tamanhoamostral5=N5, tamanhoamostral10=N10)
      colnames(tabela) <- c("Erro", "Amostra - 5% de significância","Amostra - 10% de significância")
      return(tabela)
    }

**Exemplo TD086:** Supondo *N* = 231, pode-se verificar o tamanho da
amostra condicionado ao nível de significância e margem de erro.

    EstPropPopFinita(231,0.5)

    ##   Erro Amostra - 5% de significância Amostra - 10% de significância
    ## 1 0.5%                      229.6252                      229.05281
    ## 2   1%                      225.5971                      223.40334
    ## 3   2%                      210.8055                      203.34202
    ## 4   3%                      190.0385                      176.87077
    ## 5   4%                      167.0055                      149.60480
    ## 6   5%                      144.4896                      124.85768
    ## 7   6%                      124.0487                      103.85975
    ## 8   7%                      106.2796                       86.63988
