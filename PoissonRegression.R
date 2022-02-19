### Trabalho Modelos Lineares Generalizados

#### Importação dos dados #####
library(haven)

data <- read_sav("DCoronaria.sav")

##### Descrição dos dados ######

# imc 4: ı́ndice de massa corporal agrupado em quartis
# cs 4: colesterol sérico agrupado em quartis
# pad 4: pressão arterial diastólica agrupada em quartis
# homem: indicatriz para o sexo masculino
# idade gp: 45 se idade ≤ 45, 50 se 46 ≤ idade ≤ 50, 55 se 51 ≤ idade ≤ 55, 60 se 56 ≤ idade ≤ 60, . . . ,
# 80 se 76 ≤ idade ≤ 80, 81 se idade > 80.

# Para cada padrão de covariaveis temos:

# p-yrs: número de pessoas-anos (person-years), correspondendo à soma dos tempos de seguimento dos indivı́duos
# desse padrão de covariáveis
# dc: número de casos de doença coronária observados.

# Para clarificar a interpretação dos dados, esclarece-se que, por exemplo, um paciente que entra no estudo com 42
# anos de idade e tem o primeiro evento de doença coronária aos 47 anos e nessa altura sai do estudo, contribui para o
# ficheiro com:
# 4 pessoa-anos de follow-up para o seu padrão de covariáveis com idade no grupo 41 − 45
# 2 pessoa-anos de follow-up para o seu padrão de covariáveis com idade no grupo 46 − 50
# 1 evento de doença coronária para o seu padrão de covariáveis com idade no grupo 46 − 50.

##### Pre-processamento #####

# Retirar os NAs
data <- na.omit(data)

# vamos ver os unicos casos das colunas divididas em quartis
lapply(data[c("imc_4","pad_4","cs_4")], unique)
# ha aqui um problema, aparece o primeiro quartil de imc_4 duas vezes
# Algum problema com as casas decimais aqui, por isso arredondamos a coluna para 1 casa decimal, para ficar uniforme
data$imc_4 <- round(data$imc_4,1)

lapply(data[c("imc_4","pad_4","cs_4")], unique) # agora está corrigido

# Fatorizar variaveis categoricas, male nao precisa ja que é binario
data[c("imc_4","pad_4","cs_4","idade_gp")] <- lapply(data[c("imc_4","pad_4","cs_4","idade_gp")], factor)

#' O gráfico representa, em cada uma das categorias de idade designadas neste trabalho, a taxa de ocorrência
#' de CHD por cada 1000 pessoas, separando esta representação também pelo sexo. Este gráfico é indicativo
#' que a taxa de CHD aumenta em geral quando se avança no grupo de idades, com a exceção de indíviduos
#' com mais de 80 anos. A separação pelo sexo permite ver que os individuos do sexo masculino tem, em média, mais 
#' episódios de CHD que individuos do sexo feminino, independentemente da classe de idade associada.

#' Temos de considerar o logaritmo do "person_years" como offset como usual

# Modelo a para interpretar a figura apresentar - a ver se faz sentido
mod_a <- glm(dc ~ idade_gp + male +
             offset(log(p_yrs)),
             family = poisson(),
             data = data)
summary(mod_a)

#' Vemos que todos os coeficientes são estatísticamente significativos, sendo assim podemos
#' rejeitar que os coeficientes na populacao é zero (teste de wald), e vemos pela estatistica de teste e pelos coeficientes na
#' amostra que os valores positivos indicam que tem uma relação crescente com a taxa de 
#' ocorrência de CHD.

# Vamos ajustar primeiro um modelo inicial com todas as variaveis explicativas do dataset, e seguir
# fazemos diferentes modelos explorando interaçoes e depois decidimos com base na sua significancia
# do teste de Wald e a diferença de desviancias com o modelo considerado anterior, para ver se a diferença
# de desviancias é significativa na populacao. Temos em cuidado o problema da convergencia da desviancia
# ao longo do trabalho

# Modelo 1
# Modelo com todas as variáveis
mod_1 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                 idade_gp + male + offset(log(p_yrs)),
             family = poisson(),
             data = data)
summary(mod_1) # todos os coeficientes significativos

#' Vemos pelo grafico da figura no (a) e pelo facto do coeficiente ser basicamente
#' o mesmo que podemos agrupar a idade 60-65 e 65-70 juntos. Vamos fazer
#' isso porque assim é menos um coeficiente e vemos o resultado de seguida.
#' Vamos entao juntar estes 2 padroes de covariaveis

# Vamos clonar o nosso dataset ja que nao e muito grande, e vamos modificar, assim
# podemos voltar para o original caso necessario
data2 <- data

# Aqui fazemos um loop para iterar em cada uma das linhas do dataset
for(i in 1:nrow(data2)) { # percorrendo todas as linhas
    if (i > nrow(data2)) {
        break
    }  # necessario esta condicao, visto que vamos eliminar linhas mais a frente
    if (data2[i,4] == 65 & data2[i+1,4] == 70) { # se o valor da coluna yr for 65 e a proxima ser 70
        data2[i,4] = as.factor(70) # vamos mudar o valor de 65 para 70
        data2[i,5] = data2[i,5] + data2[i+1,5] # adicionamos o pr_years dos dois
        data2[i,6] = data2[i,6] + data2[i+1,6] # adicionamos os eventos de dc nestes dois padroes
        data2 <- data2[-c(i+1),] # retiramos a linha a seguir, onde retiramos a informacao para a linha anterior, thus making it redundant
    }
} # end for

# Voltamos a ajustar o modelo
mod_2 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                 idade_gp + male + offset(log(p_yrs)),
             family = poisson(),
             data = data2)
summary(mod_2) # todos os coeficientes significativos

# Vamos comparar com um criterio de informacao, e neste caso como o sample size e diferente
# nos dois usamos BIC

extractAIC(mod_1,k = log(nrow(data))) # 2991.842
extractAIC(mod_2,k = log(nrow(data2))) # 2668.571
# o criterio de info baixou consideravelmente, pelo que quanto menor este valor, melhor


# Agora vamos explorar o modelo mas com interacoes

# Modelo mas com interação de grupo de idade com sexo
mod_3 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                 idade_gp + male + idade_gp * male +
             offset(log(p_yrs)),
             family = poisson(),
             data = data2)
summary(mod_3)

## Primeiro vamos so conferir algumas propriedades sobre os dados
# comparacao da desviancia residual para uma distribuicao qui-quadrado.
# Esta estatistica so será valida se o valor de contagem esperado em todos os padroes
# de covariaveis for em geral maior que 5

# Precismaos de ver a representatividade dos dados, por isso vamos ver o n de cada
# padrao de covariavel a ver se podemos fazer sentido das estatisticas que vamos
# tirar das desviancias

length(which(data2$p_yrs <= 5)) # apenas 41 no total de todos os padroes
# contem um numero abaixo de 5, ja que todas as variaveis explicativas sao categoricas
# + este facto, podemos considerar que a desviancia segue assintoticamente uma qui-quadrado
# embora temos sempre em consideração que esta convergencia é fraca, vamos ter em atenção
# e fazer decisoes apenas com p-values muito altos ou muito baixos


# Vamos comparar entao a diferenca de desviancias
anova(mod_2,mod_3) # diferença das desviancias de 16.654
# Vamos ver se o p-value é MUITO, caso não for, não vamos considerar a interação
# a diferencia de parametros dos dos modelos e 8, por isso segue quiquadrado(8)
1 - pchisq(16.654,7) # o p-value é significativo a um nível de significancia de .05
# contudo, não vamos considerar esta interacao no modelo, pois o p-value nao é assim
# tao baixo.

# Agora vamos sempre fazendo com as restantes interacoes, a ver o que dá, fazemos
# comparacao de desviancias sempre com o melhor modelo ate agora considerado, no
# nosso caso o mod_1

# Modelo com interação imc_4 e pad_4
mod_4 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                 idade_gp + male + pad_4 * imc_4 +
             offset(log(p_yrs)),
             family = poisson(),
             data = data2)

summary(mod_4)

anova(mod_2,mod_4) # não significativo logo por comparação

# interacao imc_4 e male
mod_5 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                 idade_gp + male + imc_4 * male +
             offset(log(p_yrs)),
             family = poisson(),
             data = data2)

summary(mod_5)

anova(mod_2,mod_5) # nao muda em quase nada a desviancia

# interacao cs_4 e imc_4
mod_6 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                 idade_gp + male + cs_4 * imc_4 +
             offset(log(p_yrs)),
             family = poisson(),
             data = data2)

summary(mod_6)

anova(mod_2,mod_6) # nada significativo

# interacao cs_4 e idade
mod_7 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                 idade_gp + male + cs_4 * idade_gp +
             offset(log(p_yrs)),
             family = poisson(),
             data = data2)

summary(mod_7)

anova(mod_2,mod_7)
1 - pchisq(35.022,21) # nao vamos considerar interacao

# interacao cs_4 e male
mod_8 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                 idade_gp + male +
                 cs_4 * male +
             offset(log(p_yrs)),
             family = poisson(),
             data = data2)

summary(mod_8)

anova(mod_2,mod_8) # nada

# idade_gp e pad_4
mod_9 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                 idade_gp + male +
                 idade_gp * pad_4 +
             offset(log(p_yrs)),
             family = poisson(),
             data = data2)

summary(mod_9)

anova(mod_2,mod_9) # nada significativo


# idade_gp e imc_4
mod_10 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                 idade_gp + male + 
                 idade_gp * imc_4 +
             offset(log(p_yrs)),
             family = poisson(),
             data = data2)

summary(mod_9)

anova(mod_2,mod_10) # 0.03 de p-value tambem

# pad_4 e cs_4
mod_11 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                 idade_gp + male +
                 pad_4 * cs_4 +
             offset(log(p_yrs)),
             family = poisson(),
             data = data2)

summary(mod_11)

anova(mod_2,mod_11) # nada tambem


# Chegamos ao fim e consideramos o modelo completo entao sem os termos de interacao
summary(mod_2)


# Analises de GOF (goodness of fit)

# Primeiro vamos ver se o nosso modelo tem um GOF (goodness of fit) melhor que o modelo nulo 
# e um GOF abaixo do modelo saturado.
# Podemos ver que na amostra a desviancia residual (portanto do nosso modelo) tem um valor de 1145.7
# e a desviancia do modelo nulo é 2035.1. Vamos tentar passar a diferenca para a populacao, assumindo
# que a difereca de desviancias converge assintoticamente para uma distribuicao qui quadrado.

# Comparacao M0 (modelo nulo) e M (nosso mod_1). Estamos a espera de um valor de 0 ou mesmo perto de 0.
# H0: GOF(M0) == GOF(M); H1: GOF(M0) != GOF(M) 
dif_desv <- 1909.7 - 1020.5
dif_df <- 1005 - 988

# calculo do p-value
1 - pchisq(dif_desv, dif_df) # 0 tal como presumido, pois pchisq(0.95, dif_df) = 28.8693
# Podemos entao rejeitar a hipotese nula de que os dois modelos tem a mesma qualidade
# de ajustamento, e claro como M é o modelo mais completo dos dois então a sua GOF é maior

# Comparacao com o Ms (modelo saturado) e M
# Aqui, esta comparacao é basicamente vermos se a desviancia e zero na populacao,
# pois a desviancia do modelo saturado é naturalmente zero
# H0: D(M) == 0; H1: D(M) != 0

summary(mod_2)
# calculo do p-value
1 - pchisq(1020.5, 1005)
# Com o p-value de 0.36 não podemos rejeitar H0 e dizer que a qualidade do ajustamento
# e diferente de que a do modelo saturado. O que queriamos era rejeitar que M0 tem GOF
# igual a M (feito) e não rejeitar que Ms tem o mesmo GOF que o nosso modelo. Por isso
# estamos numa boa posicao.

# Pseudo R-squared statistic: da a percentagem de desviancia explicada pelo modelo
r_squared <- 1 - (1020.5/1909.7)
r_squared # 0.47, temos normalmente valores mais baixos do que em regressao linear,
# tendo isso em conta, este resultado é bom


# Analises de diagnostico

# Começemos por dar plot aos principais graficos de diagnostico
par(mfrow = c(2,3))
plot(mod_2, which = 1:6)

# Ver independencia de observacoes:
# Vemos alguns outliers, mas nenhum padrao em concreto
plot(mod_2$residuals) 

# Verificacao que a media e igual a variancia
lambdahat <- fitted(mod_2)

par(mfrow=c(1,2), pty="s")
plot(lambdahat,(data2$dc-lambdahat)^2,
     xlab=expression(hat(lambda)), ylab=expression((y-hat(lambda))^2))
plot(lambdahat, resid(mod_2,type="deviance"), 
     xlab=expression(hat(lambda)), ylab="Deviance Residuals") 

# Aqui vemos que de facto a variancia nao e igual a media, e por isso uma das assumptions
# da regressao de Poisson é aqui violada

plot(log(fitted(mod_2)), log((data2$dc - fitted(mod_2))^2),
     xlab=expression(hat(mu)),ylab=expression((y - hat(mu))^2))
abline(0,1)

# Analise de outliers.
# Nota: Previamente vimos pelo grafico de Cook, que nenhum ponto tem valor superior a 1

library(faraway)
halfnorm(rstandard(mod_2)) # observacoes 771 e 492 como outliers

# Ver os residuos estandardizados da desviancia cujo o valor esta fora do range de -2 a 2, para grouped data
length(which(rstandard(mod_2) < -2 | rstandard(mod_2) > 2)) # Temos 31 residuos fora deste range
which(rstandard(mod_2) < -2 | rstandard(mod_2) > 2) 
outliers <- which(rstandard(mod_2) < -2 | rstandard(mod_2) > 2)

# Devemos remove-los?
# Vamos ve-los primeiro
data2[c(492,771),]
# Nao parecem "impossiveis" de acontecer num contexto real

# calculating the high leverage values
high_leverage_values <- which(hatvalues(mod_2) >= (2 * (17+1))/nrow(data2)) # 1531
outliers[outliers %in% high_leverage_values] # estes outliers detetados pelos residuos da desviancia
# estao no conjunto de pontos calculados de leverage, incluindo o 492

# Olhar para os outliers ja que nao sao tantos assim
print(data2[outliers,], n=31)

# calculating cook's distance for all points (therefore also having the high leverage points calculated)
cook_d<- cooks.distance(mod_2)

# We want to see if any point has a cook's distance above 1
plot(cook_d, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# nenhum ponto tem cook's distance acima de 1
# 
# Por causa destes resultados, decidimos nao remover outliers

## Residuos da desviancia estandardizados
plot(resid(mod_2))
abline(h=0, col="red")

qqnorm(resid(mod_2))
qqline(resid(mod_2)) # estes residuos dizem respeito aos residuos da desviancia

summary(mod_2)

#' o teste wald é o que segue no summary do mod_11, sendo que a estatistica de teste assintoticamente segue uma
#' distribuicao standard normal.
summary(mod_2)

# T = 0.67731 / 0.05399 = 12.5451 (confere!)

# alternativamente podemos fazer LRT (likelihood ratio test) para obter entao
# resultados de o beta de male é igual a zero ou nao
# por likelihood ratio test

library(lmtest)

# Aqui nao temos de ajustar outro modelo sem o "male" para comparar, podemos
# pedir ao lrtest do r especificacoes para ver em concreto o modelo ajustado com e
# sem o coeficiente

lrtest(mod_2, data="male") # podemos ver que o resultado e muito significativo

#' Caso clinico:
#' Homens (vamos generalizar para 100 individuos seguidos em 1 ano)
#' entre os 60 e 70 anos;
#' no segundo quartil de imc_4
#' no segundo quartil de pad_4
#' no segundo quartil de cs_4

mod_2
newdata <- data.frame(imc_4 = 25.2, pad_4 = 80, cs_4 = 225, idade_gp = 70, p_yrs = 100, male = 1)
newdata[c("imc_4","pad_4","cs_4","idade_gp")] <- lapply(newdata[c("imc_4","pad_4","cs_4","idade_gp")], factor)
b = predict(mod_2,newdata)
exp(b)

# Para 100 observaces de homens de 80 anos, no espaco de 1 ano, com um indice de massa corporal no segundo
# quartil, pressao arterial diastolica no segundo quartil, e colesterol serico no segundo quartil
# pressao arterial diastolica de 80 e um coletrol serico de 225, preve-se que 
# ocorram em media 2.18 eventos.

mod_2
newdata <- data.frame(imc_4 = 22.8, pad_4 = 74, cs_4 = 197, idade_gp = 45, p_yrs = 100, male = 0)
newdata[c("imc_4","pad_4","cs_4","idade_gp")] <- lapply(newdata[c("imc_4","pad_4","cs_4","idade_gp")], factor)
b = predict(mod_2,newdata)
exp(b)

newdata <- data.frame(imc_4 = 29, pad_4 = 91, cs_4 = 256, idade_gp = 81, p_yrs = 100, male = 1)
newdata[c("imc_4","pad_4","cs_4","idade_gp")] <- lapply(newdata[c("imc_4","pad_4","cs_4","idade_gp")], factor)
b1 = predict(mod_2,newdata)
exp(b1)

#Pelos coeficientes do modelo, presume-se que homens com idade superior a 80,
#press?o arterial dist?lica de 91, colestrol s?rico de 256 e ?ndice de massa
#corporal de 29, prev?-se que obtenham o maior n?mero de eventos.
#Por outro lado, mulheres com idade inferior ou igual a 45, press?o arterial
#de 74 e colestrol a 197, prev?-se que demonstrem o menor n?mero de eventos.

prop.table(table(data2$dc==0))
prop.table(table(mod_2$fitted.values<0.5))
# Pelos valores das tabelas de proporção, verifica-se a ocorrência de 30.72% respostas nulas
# esperadas para 41.25% respostas nulas efetivamente observadas nos dados. Observa-se, assim, uma
# menor quantidade de respostas nulas no modelo do que na realidade ocorreram.

#Isto permite aferir que o modelo estima menos respostas nulas que as que foram 
#efetivamente observadas nos dados. 

# Sobre o fenomeno de sobre-dispersao
#' A variabilidade nos dados pode ser maior que a variabilidade prevista pelo modelo de Poisson
#' Vamos verificar o fi estimado

# by deviance
fi_deviance <- mod_2$deviance / mod_2$df.residual
fi_deviance

# by pearson statistic
fi_pearson  <- sum(residuals(mod_2,type="pearson")^2)/mod_2$df.residual
fi_pearson

#' o modelo em questao nao esta com sobre-dispersao, mas underdispersion...
#' Vamos na mesma re-fazer modelos com quasipoisson e binomial negativa
#' para ver se conseguimos um fi de ~ 1

library(MASS)
library(magrittr)

mod_12 <- glm(dc ~ imc_4 + pad_4 + cs_4 +
                  idade_gp + male +
                  offset(log(p_yrs)),
              family = "quasipoisson",
              data = data2)

confint(mod_12)

# desviancia
fi_deviance <- mod_12$deviance / mod_12$df.residual
fi_deviance

# by pearson statistic
fi_pearson  <- sum(residuals(mod_12,type="pearson")^2)/mod_12$df.residual
fi_pearson

# mesma situacao, vamos tentar a binomial negativa

mod_13 <- glm.nb(dc ~ imc_4 + pad_4 + cs_4 +
                     idade_gp + male + offset(log(p_yrs)),
                 data = data2)

# desviancia
fi_deviance <- mod_13$deviance / mod_13$df.residual
fi_deviance

# by pearson statistic
fi_pearson  <- sum(residuals(mod_13,type="pearson")^2)/mod_13$df.residual
fi_pearson

prop.table(table(data2$dc==0))
prop.table(table(mod_13$fitted.values<0.5))
