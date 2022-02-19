library(readr)

setwd("~/Desktop/Trabalhos_Estatistica")

library(readxl)
dados_mel_2 <- read_excel("dados_mel.xlsx",skip = 1)
dim(dados_mel_2) # 112 39
str(dados_mel_2)

##### Pre-processamento #####
# MDA analysis 
apply(dados_mel_2,2, function(x) {any(is.na(x) | is.infinite(x) | is.nan(x))})
# no missings

library(dplyr)

dados_mel_2 <- dados_mel_2 %>%
    mutate_at(vars(13:33), ~replace(.,is.na(.), 0))

#

# categorizar
dados_mel_2[c(3:33)] <- lapply(dados_mel_2[c(3:33)], factor)

library(dplyr)
# retirar numero
dados_mel_2 <- dados_mel_2 %>% 
    dplyr::select(-c(1,2,7,9,13,14,15,16,17))

# processar as colunas de data, para apenas ter horas
library(lubridate)
dados_mel_2[c(4)] <- lapply(dados_mel_2[c(4)], hour)

# Recodificacao de variaveis
dados_mel_2$t_Viagem <- ifelse(dados_mel_2$t_Viagem > 4, 1, 0)
dados_mel_2$t_Abegoaria <- ifelse(dados_mel_2$t_Abegoaria == 2, 1, 0)
dados_mel_2$P <- ifelse(dados_mel_2$P == 2, 1, 0)
dados_mel_2$ALIMENTAÇÃO <- ifelse(dados_mel_2$ALIMENTAÇÃO == "Alimentada", 1, 0)
dados_mel_2$LAM <- ifelse(dados_mel_2$LAM == "Moderadas", 1, 0)

# particao do dataset para analises de pH e depois das lesoes
dados_mel_pH <- dados_mel_2[complete.cases(dados_mel_2),]
dados_mel_LPM <- dados_mel_2[,-c(25:30)]


##### Analises Descritivas ######

# Peso
library(ggplot2)
library(cowplot)

# P
# Barplot
p1 <- ggplot(as.data.frame(table(dados_mel_LPM$P)), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat="identity", width = 0.45) +
    labs(x=" ", y="Frequência", fill="Peso") +
    scale_x_discrete(labels=c("0" = "<90kg", "1" = ">=90kg")) +
    scale_fill_manual(labels = c("1", "2"),values=c("#999999", "#3A3B3C")) +
    theme(text = element_text(size = 20), legend.position = "none")
p1

ggsave("P.png", dpi = 200)
# Relative Frequency Table
table(dados_mel_LPM$P)
# Absolute Frequency Table (%)
round(prop.table(table(dados_mel_LPM$P))*100,2)




# Labels EXPL
# Barplot
p1 <- ggplot(as.data.frame(table(dados_mel_LPM$EXPL)), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat="identity", width = 0.35) +
    labs(x=" ", y="Frequency", fill="EXPL")

p1
# Relative Frequency Table
table(dados_mel$EXPL)
# Absolute Frequency Table (%)
round(prop.table(table(dados_mel$EXPL))*100,2)


# Origem
# Barplot
p1 <- ggplot(as.data.frame(table(dados_mel_LPM$O)), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat="identity", width = 0.35) +
    labs(x=" ", y="Frequência", fill="Origem") +
    scale_x_discrete(labels=c("Centro" = "Centro", "Esp" = "Espanha", "Sul" = "Sul")) +
    scale_fill_manual(labels = c("Centro", "Espanha", "Sul"),values=c("#4b3832","#854442", "#be9b7b")) +
    theme(text = element_text(size = 20),legend.position = "none")

p1

ggsave("O.png", dpi = 300)
# Relative Frequency Table
table(dados_mel_LPM$O)
# Absolute Frequency Table (%)
round(prop.table(table(dados_mel_LPM$O))*100,2)

# Tempo de viagem
# Barplot
p1 <- ggplot(as.data.frame(table(dados_mel_LPM$t_Viagem)), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat="identity", width = 0.35) +
    labs(x=" ", y="Frequência", fill="Tempo de Viagem") +
    scale_x_discrete(labels=c("0" = "<4h", "1" = ">=4h")) +
    scale_fill_manual(labels = c("0", "1"),values=c("#999999", "#3A3B3C")) +
    theme(text = element_text(size = 20),legend.position = "none")

p1

ggsave("t_Viagem.png", dpi = 300)
# Relative Frequency Table
table(dados_mel$Tempo_viagem)
# Absolute Frequency Table (%)
round(prop.table(table(dados_mel$Tempo_viagem))*100,2)


# Tempo de abegoaria
# Barplot
p1 <- ggplot(as.data.frame(table(dados_mel_LPM$t_Abegoaria)), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat="identity", width = 0.35) +
    labs(x=" ", y="Frequência", fill="Tempo de Abegoaria") +
    scale_x_discrete(labels=c("0" = "<12h", "1" = ">=12h")) +
    scale_fill_manual(labels = c("0", "1"),values=c("#999999", "#3A3B3C")) +
    theme(text = element_text(size = 20),legend.position = "none")

p1

ggsave("t_Abegoaria.png", dpi = 300)
# Relative Frequency Table
table(dados_mel_LPM$t_Abegoaria)
# Absolute Frequency Table (%)
round(prop.table(table(dados_mel_LPM$t_Abegoaria))*100,2)


# t_Abegoaria
# Barplot
p1 <- ggplot(as.data.frame(table(dados_mel$t_Abegoaria)), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat="identity", width = 0.35) +
    labs(x=" ", y="Frequency", fill="Tempo de Abegoaria")

p1
# Relative Frequency Table
table(dados_mel$t_Abegoaria)
# Absolute Frequency Table (%)
round(prop.table(table(dados_mel$t_Abegoaria))*100,2)


# PAT
# Barplot
p1 <- ggplot(as.data.frame(table(dados_mel$PAT)), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat="identity", width = 0.35) +
    labs(x=" ", y="Frequency", fill="PAT")

p1
# Relative Frequency Table
table(dados_mel$t_Abegoaria)
# Absolute Frequency Table (%)
round(prop.table(table(dados_mel$t_Abegoaria))*100,2)


# ALIMENTACAO
p1 <- ggplot(as.data.frame(table(dados_mel_LPM$ALIMENTAÇÃO)), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat="identity", width = 0.35) +
    labs(x=" ", y="Frequência", fill="Alimentação") +
    scale_x_discrete(labels=c("0" = "Alimentado", "1" = "Não Alimentado")) +
    scale_fill_manual(labels = c("0", "1"),values=c("#999999", "#3A3B3C")) +
    theme(text = element_text(size = 20),legend.position = "none")

p1

ggsave("alimentacao.png", dpi = 300)
# Relative Frequency Table
table(dados_mel_LPM$ALIMENTAÇÃO)
# Absolute Frequency Table (%)
round(prop.table(table(dados_mel_LPM$ALIMENTAÇÃO))*100,2)

# LAM
# Barplot
p1 <- ggplot(as.data.frame(table(dados_mel_LPM$LAM)), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat="identity", width = 0.35) +
    labs(x=" ", y="Frequência", fill="LAM") +
    scale_x_discrete(labels=c("0" = "Leves", "1" = "Moderadas")) +
    scale_fill_manual(labels = c("0", "1"),values=c("#999999", "#3A3B3C")) +
    theme(text = element_text(size = 20),legend.position = "none")

p1

ggsave("LAM.png", dpi = 300)
# Relative Frequency Table
table(dados_mel_LPM$LAM)
# Absolute Frequency Table (%)
round(prop.table(table(dados_mel_LPM$ALIMENTAÇÃO))*100,2)

# Sexo
# Barplot
p1 <- ggplot(as.data.frame(table(dados_mel_LPM$Sexo)), aes(x=Var1, y=Freq, fill=Var1)) + 
    geom_bar(stat="identity", width = 0.35) +
    labs(x=" ", y="Frequência", fill="Sexo") +
    scale_x_discrete(labels=c("F" = "Fêmea", "MC" = "Macho Castrado", "MI" = "Macho Inteiro")) +
    scale_fill_manual(labels = c("F", "MC", "MI"),values=c("#D291BC","#779ECB", "#AEC6CF")) +
    theme(text = element_text(size = 20),legend.position = "none")

p1

ggsave("Sexo.png", dpi = 300)
# Relative Frequency Table
table(dados_mel_LPM$Sexo)
# Absolute Frequency Table (%)
round(prop.table(table(dados_mel_LPM$Sexo))*100,2)


# pH 45.1
# Histogram
p1<-ggplot(dados_mel_pH, aes(x=pH45.1)) +
    geom_histogram(fill="#000000", color="#000000", alpha=0.5, bins = 10) + 
    labs(x=" ", y="Frequência") + 
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) 
# Boxplot
p2<-ggplot(dados_mel_pH, aes(x=pH45.1)) +
    geom_boxplot(fill="#000000", alpha = 0.5, outlier.colour="#999999", outlier.shape=8, outlier.size=2) +
    theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    labs(x="pH 45 pernil")
# Pair Plot
cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(2, 1),align = 'v', axis = 'lr', labels="AUTO")  
ggsave("pH45_pernil.png")

round(mean(dados_mel_pH$pH45.1),1) # media
round(sd(dados_mel_pH$pH45.1),2) # desvio padrao


# pH 45.2
# Histogram
p1<-ggplot(dados_mel_pH, aes(x=pH45.2)) +
    geom_histogram(fill="#000000", color="#000000", alpha=0.5, bins = 10) + 
    labs(x=" ", y="Frequency") + 
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) 
# Boxplot
p2<-ggplot(dados_mel_pH, aes(x=pH45.2)) +
    geom_boxplot(fill="#000000", alpha = 0.5, outlier.colour="#999999", outlier.shape=8, outlier.size=2) +
    theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    labs(x="pH 45 dorsal")
# Pair Plot
cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(2, 1),align = 'v', axis = 'lr', labels="AUTO")  
ggsave("pH45_cabeça.png")

round(mean(dados_mel_pH$pH45.2),1) # media
round(sd(dados_mel_pH$pH45.2),2) # desvio padrao


# pH 45.3
# Histogram
p1<-ggplot(dados_mel_pH, aes(x=pH45.3)) +
    geom_histogram(fill="#000000", color="#000000", alpha=0.5, bins = 10) + 
    labs(x=" ", y="Frequency") + 
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) 
# Boxplot
p2<-ggplot(dados_mel_pH, aes(x=pH45.3)) +
    geom_boxplot(fill="#000000", alpha = 0.5, outlier.colour="#999999", outlier.shape=8, outlier.size=2) +
    theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    labs(x="pH45 cabeça")
# Pair Plot
cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(2, 1),align = 'v', axis = 'lr', labels="AUTO")  
ggsave("pH45_cabeca.png")

round(mean(dados_mel_pH$pH45.3),1) # media
round(sd(dados_mel_pH$pH45.3),2) # desvio padrao


# pH U.1
# Histogram
p1<-ggplot(dados_mel_pH, aes(x=pHU.1)) +
    geom_histogram(fill="#000000", color="#000000", alpha=0.5, bins = 10) + 
    labs(x=" ", y="Frequência") + 
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) 
# Boxplot
p2<-ggplot(dados_mel_pH, aes(x=pHU.1)) +
    geom_boxplot(fill="#000000", alpha = 0.5, outlier.colour="#999999", outlier.shape=8, outlier.size=2) +
    theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    labs(x="pH 22-24h pernil")
# Pair Plot
cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(2, 1),align = 'v', axis = 'lr', labels="AUTO")  
ggsave("pH24_pernil.png")

round(mean(dados_mel_pH$pHU.1),1) # media
round(sd(dados_mel_pH$pHU.1),2) # desvio padrao


# pH 45.2
# Histogram
p1<-ggplot(dados_mel_pH, aes(x=pHU.2)) +
    geom_histogram(fill="#000000", color="#000000", alpha=0.5, bins = 10) + 
    labs(x=" ", y="Frequência") + 
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) 
# Boxplot
p2<-ggplot(dados_mel_pH, aes(x=pH45.2)) +
    geom_boxplot(fill="#000000", alpha = 0.5, outlier.colour="#999999", outlier.shape=8, outlier.size=2) +
    theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    labs(x="pH 22-24h dorsal")
# Pair Plot
cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(2, 1),align = 'v', axis = 'lr', labels="AUTO")  
ggsave("pHU_dorsal.png")

round(mean(dados_mel_pH$pHU.2),1) # media
round(sd(dados_mel_pH$pHU.2),2) # desvio padrao


# pH 45.3
# Histogram
p1<-ggplot(dados_mel_pH, aes(x=pHU.3)) +
    geom_histogram(fill="#000000", color="#000000", alpha=0.5, bins = 10) + 
    labs(x=" ", y="Frequência") + 
    theme(axis.text.x=element_blank(),axis.ticks.x=element_blank()) 
# Boxplot
p2<-ggplot(dados_mel_pH, aes(x=pH45.3)) +
    geom_boxplot(fill="#000000", alpha = 0.5, outlier.colour="#999999", outlier.shape=8, outlier.size=2) +
    theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
    labs(x="pH 22-24h cabeça")
# Pair Plot
cowplot::plot_grid(p1, p2, ncol = 1, rel_heights = c(2, 1),align = 'v', axis = 'lr', labels="AUTO")  
ggsave("pHU_cabeca.png")

round(mean(dados_mel_pH$pHU.3),1) # media
round(sd(dados_mel_pH$pHU.3),2) # desvio padrao


library(reshape2)
library(tidyr)

dados_F <- pivot_longer(dados_mel_LPM[,c("F1","F2","F3","F4")], c("F1","F2","F3","F4"), names_to = "variable", values_to = "values")

p1 <- ggplot(as.data.frame(table(dados_F)), aes(x=variable, y=Freq, fill=values)) + 
    geom_bar(stat="identity", position = "dodge", width = 0.70) +
    labs(x=" ", y="Frequência", fill="F1") +
    scale_fill_manual(name = "Forma", labels = c("0","D","L","R","Ro","V"),values=c("#A9A9A9", "#bed1d8","#aec6cf","#9ebbc6","#8eafbc","#7ea4b3")) +
    theme(text = element_text(size = 20))

p1

ggsave("F.png")


dados_T <- pivot_longer(dados_mel_LPM[,c("T1","T2","T3","T4")], c("T1","T2","T3","T4"), names_to = "variable", values_to = "values")

p1 <- ggplot(as.data.frame(table(dados_T)), aes(x=variable, y=Freq, fill=values)) + 
    geom_bar(stat="identity", position = "dodge", width = 0.70) +
    labs(x=" ", y="Frequência") +
    scale_fill_manual(name = "Tamanho", labels = c("0","1","2","3","4"),values=c("#A9A9A9", "#aec6cf","#9ebbc6","#8eafbc","#7ea4b3")) +
    theme(text = element_text(size = 20))

p1

ggsave("T.png")


dados_S <- pivot_longer(dados_mel_LPM[,c("S1","S2","S3","S4")], c("S1","S2","S3","S4"), names_to = "variable", values_to = "values")

p1 <- ggplot(as.data.frame(table(dados_S)), aes(x=variable, y=Freq, fill=values)) + 
    geom_bar(stat="identity", position = "dodge", width = 0.70) +
    labs(x=" ", y="Frequência") +
    scale_fill_manual(name = "Severidade", labels = c("0","1","2","3","4"),values=c("#A9A9A9", "#aec6cf","#9ebbc6","#8eafbc","#7ea4b3")) +
    theme(text = element_text(size = 20))

p1

ggsave("S.png")


dados_C <- pivot_longer(dados_mel_LPM[,c("C1","C2","C3","C4")], c("C1","C2","C3","C4"), names_to = "variable", values_to = "values")

p1 <- ggplot(as.data.frame(table(dados_C)), aes(x=variable, y=Freq, fill=values)) + 
    geom_bar(stat="identity", position = "dodge", width = 0.70) +
    labs(x=" ", y="Frequência") +
    scale_fill_manual(name = "Cor", labels = c("0","1","2","3"),values=c("#A9A9A9","#9ebbc6","#8eafbc","#7ea4b3")) +
    theme(text = element_text(size = 20))

p1

ggsave("C.png")

##### Analises diagnostico iniciais ######

cor(dados_mel[c(32:37)], method = "pearson") # correlacoes elevadas em geral entre os pHs entre 
# separadamente por 45 mins e 24h, i.e., o pH varia de forma muito igual independentemente da
# localizacao onde foi tirado?

# Testes univariados
t.test(dados_mel$pH45.1, dados_mel$pH45.2, mu = 0, paired = FALSE, conf.level = .95)
t.test(dados_mel$pH45.1, dados_mel$pH45.3, mu = 0, paired = FALSE, conf.level = .95)
t.test(dados_mel$pH45.2, dados_mel$pH45.3, mu = 0, paired = FALSE, conf.level = .95)

t.test(dados_mel$pH45.1, dados_mel$pHU.1, mu = 0, paired = TRUE, conf.level = .95)

chisq.test(table(dados_mel$P,dados_mel$O)) # sao independentes
fisher.test(table(dados_mel$P,dados_mel$Tempo_viagem)) # independentes
chisq.test(table(dados_mel_pH$O,dados_mel_pH$t_Viagem)) # dependentes
chisq.test(table(dados_mel_pH$O,dados_mel_pH$t_Abegoaria)) # dependentes
fisher.test(table(dados_mel_pH$O,dados_mel_pH$LAM)) # muito dependentes
chisq.test(table(dados_mel$O,dados_mel$LAM)) # dependentes
fisher.test(table(dados_mel_pH$O,dados_mel_pH$Sexo)) # dependentes
fisher.test(table(dados_mel$t_Abegoaria,dados_mel$Sexo)) # dependentes

chisq.test(table(dados_mel_LPM$O,dados_mel_LPM$t_Viagem))
fisher.test(table(dados_mel_LPM$O,dados_mel_LPM$Sexo))
chisq.test(table(dados_mel_LPM$O,dados_mel_LPM$t_Abegoaria))
chisq.test(table(dados_mel_LPM$O,dados_mel_LPM$ALIMENTAÇÃO))
chisq.test(table(dados_mel_LPM$t_Viagem,dados_mel_LPM$t_Abegoaria))
fisher.test(table(dados_mel_LPM$Sexo,dados_mel_LPM$t_Abegoaria))
chisq.test(table(dados_mel_LPM$t_Viagem,dados_mel_LPM$t_Abegoaria))

library(GGally)
ggpairs(dados_mel_pH)

# Peso vs pH
boxplot(pH45.1 ~ P, data = dados_mel_pH)

t.test(pH45.1 ~ P, paired = FALSE, data = dados_mel_pH)
wilcox.test(pH45.1 ~ P, paired = FALSE, data = dados_mel_pH)

# Tempo viagem vs pH
boxplot(pH45.1 ~ Tempo_viagem, data = dados_mel)
# anova
summary(aov(pH45.1 ~ Tempo_viagem, data = dados_mel)) # significativo

# Tempo abegoaria vs pH
boxplot(pH45.1 ~ t_Abegoaria, data = dados_mel_pH)
# anova
summary(aov(pH45.1 ~ t_Abegoaria, data = dados_mel)) # nao significativo

table(dados_mel$t_Abegoaria)
# testes
t.test(pH45.1 ~ t_Abegoaria, paired = FALSE, data = dados_mel_pH)
wilcox.test(pH45.1 ~ t_Abegoaria, paired = FALSE, data = dados_mel) # nao significativo

# ALIMENTACAO vs pH
boxplot(pH45.1 ~ ALIMENTACAO, data = dados_mel)
table(dados_mel$ALIMENTACAO)

# testes
t.test(pH45.1 ~ ALIMENTACAO, paired = FALSE, data = dados_mel)
wilcox.test(pH45.1 ~ ALIMENTACAO, paired = FALSE, data = dados_mel) # significativo


# LAM vs pH
boxplot(pH45.1 ~ LAM, data = dados_mel)
table(dados_mel$LAM)

# testes
t.test(pH45.1 ~ LAM, paired = FALSE, data = dados_mel)
wilcox.test(pH45.1 ~ LAM, paired = FALSE, data = dados_mel) # nao significativo



# Peso vs Sexo
boxplot(pH45.1 ~ Sexo, data = dados_mel_pH)

hist(dados_mel$pH45.1[dados_mel$Sexo == "F"])
boxplot(dados_mel$pH45.1[dados_mel$Sexo == "F"])
hist(dados_mel$pH45.1[dados_mel$Sexo == "MC"])
boxplot(dados_mel$pH45.1[dados_mel$Sexo == "MC"])
hist(dados_mel$pH45.1[dados_mel$Sexo == "MI"])
boxplot(dados_mel$pH45.1[dados_mel$Sexo == "MI"])

t.test(dados_mel$pH45.1[dados_mel$Sexo == "F"], dados_mel$pH45.1[dados_mel$Sexo == "MC"], paired = FALSE)
t.test(dados_mel$pH45.1[dados_mel$Sexo == "F"], dados_mel$pH45.1[dados_mel$Sexo == "MI"], paired = FALSE)
t.test(dados_mel$pH45.1[dados_mel$Sexo == "MI"], dados_mel$pH45.1[dados_mel$Sexo == "MC"], paired = FALSE)
wilcox.test(dados_mel$pH45.1[dados_mel$Sexo == "F"], dados_mel$pH45.1[dados_mel$Sexo == "MC"], paired = FALSE)
wilcox.test(dados_mel$pH45.1[dados_mel$Sexo == "F"], dados_mel$pH45.1[dados_mel$Sexo == "MI"], paired = FALSE)
wilcox.test(dados_mel$pH45.1[dados_mel$Sexo == "MI"], dados_mel$pH45.1[dados_mel$Sexo == "MC"], paired = FALSE)


# exploracao do step
model_null <- lm(pHU.2 ~ 1, dados_mel_pH)
model_complete <- lm(pHU.2 ~ P + O + t_Viagem + t_Abegoaria + ALIMENTAÇÃO + LAM + I(Sexo) +
                         I(F1) + I(T1) + I(S1) + I(C1), dados_mel_pH)

# backwards selection
step.backward <- step(model_complete, 
                      scope = list(lower = model_null, upper = model_complete),
                      trace = TRUE,
                      direction="backward")

summary(step.backward)
# Origem, C, Sexo e t_Abegoaria

# forward selection
step.forward <- step(model_null, 
                     scope = list(lower = model_null, upper = model_complete),
                     trace = TRUE,
                     direction="forward")

summary(step.forward)
# Sexo e C

# forward selection
step.stepwise <- step(model_null, 
                      scope = list(lower = model_null, upper = model_complete),
                      trace = TRUE,
                      direction="both")

summary(step.stepwise)
# Sexo e C

# Regressao linear pH45.1

mod_1 <- lm(pH45.1 ~ I(O) + I(Sexo) + t_Abegoaria + I(C1), data = dados_mel_pH)
summary(mod_1)
anova(mod_1)

confint(mod_1)

alias(mod_1)

library(car)
car::vif(mod_1)



mod_2 <- lm(pH45.2 ~ I(O) + t_Abegoaria + I(Sexo), data = dados_mel_pH)
summary(mod_2)
anova(mod_2)

confint(mod_2)

alias(mod_2)

library(car)
car::vif(mod_2)

par(mfrow = c(2,2))
plot(mod_2)

library(faraway)
halfnorm(rstandard(mod_2))

hist(rstandard(mod_2))

dados_mel_pH[which(rstandard(mod_2) > 3.3 | rstandard(mod_2) < -3.3)]


mod_3 <- lm(pH45.3 ~ I(O) + t_Abegoaria + I(Sexo) + I(F3) + I(S3), data = dados_mel_pH)
summary(mod_3)
anova(mod_3)

confint(mod_3)

alias(mod_3)

library(car)
car::vif(mod_3)


# Com 24 h
mod_u1 <- lm(pHU.1 ~ I(O)+ t_Viagem + t_Abegoaria+ I(C1) + I(F1), data = dados_mel_pH)
summary(mod_u1)
anova(mod_u1)

confint(mod_u1)

alias(mod_u1)

library(car)
vif(mod_u1)


# Com 24 h
mod_u2 <- lm(pHU.2 ~ O + t_Abegoaria, data = dados_mel_pH)
summary(mod_u2)
anova(mod_u2)

confint(mod_u2)

alias(mod_u2)

library(car)
car::vif(mod_u2)


# Com 24 h
mod_u3 <- lm(pHU.3 ~ O + t_Viagem + Sexo + I(T3), data = dados_mel_pH)
summary(mod_u3)
anova(mod_u3)

confint(mod_u3)

alias(mod_u3)

library(car)
vif(mod_u3)

mod_all <- lm(cbind(pH45.1,pH45.2,pH45.3,pHU.1,pHU.2,pHU.3) ~ P + O + t_Abegoaria + t_Viagem + LAM + ALIMENTAÇÃO + Sexo + C1 + T1 + S1 + F1 + C2 + F2 + T2 + S2 + C3 + F3 + T3 + S3, data = dados_mel_pH)
summary(mod_all)
anova(mod_all)

library(car)
car::Anova(mod_all)

confint(mod_all)

alias(mod_all)

library(car)
vif(mod_all)



mod_F1_D <- glm(F1 ~ P + I(O) + t_Viagem + t_Abegoaria + LAM + I(Sexo), family = binomial, data = dados_mel_LPM[dados_mel_LPM$F1 == "D" | dados_mel_LPM$F1 == "0",])
summary(mod_F1_D)
anova(mod_1)

exp(coef(mod_F1))

confint(mod_1)

alias(mod_1)

library(car)
vif(mod_1)


require(nnet)

mod_F1_null <- multinom(F1 ~ 1, data = dados_mel_LPM[dados_mel_LPM$F1 != "Ro" & dados_mel_LPM$F1 != "R",])
summary(mod_F1_null)
mod_F1 <- multinom(F1 ~ I(O) + t_Viagem + t_Abegoaria + LAM + I(Sexo), data = dados_mel_LPM[dados_mel_LPM$F1 != "Ro" & dados_mel_LPM$F1 != "R",])
summary(mod_F1)

anova(mod_F1_null, mod_F1, test = "Chisq")

z <- summary(mod_F1)$coefficients/summary(mod_F1)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mod_F1))


mod_F2_null <- multinom(F2 ~ 1, data = dados_mel_LPM[dados_mel_LPM$F2 != "Ro" & dados_mel_LPM$F2 != "V" & dados_mel_LPM$F2 != "D",])
summary(mod_F2_null)
mod_F2 <- multinom(F2 ~ I(O) + I(Sexo), data = dados_mel_LPM[dados_mel_LPM$F2 != "Ro" & dados_mel_LPM$F2 != "V" & dados_mel_LPM$F2 != "D",])
summary(mod_F2)

anova(mod_F2_null, mod_F2, test = "Chisq")

z <- summary(mod_F2)$coefficients/summary(mod_F2)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mod_F2))


mod_F3_null <- glm(F3 ~ 1, family = binomial, data = dados_mel_LPM[dados_mel_LPM$F1 == "V" | dados_mel_LPM$F1 == "0",])
summary(mod_F3_null)
mod_F3 <- glm(F3 ~ I(O) + t_Abegoaria, family = binomial, data = dados_mel_LPM[dados_mel_LPM$F1 == "V" | dados_mel_LPM$F1 == "0",])
summary(mod_F3)

anova(mod_F3_null, mod_F3, test = "Chisq")


mod_F4_null <- glm(F4 ~ 1, family = binomial, data = dados_mel_LPM[dados_mel_LPM$F4 == "V" | dados_mel_LPM$F4 == "0",])
summary(mod_F4_null)
mod_F4 <- glm(F4 ~ I(O) + LAM, family = binomial, data = dados_mel_LPM[dados_mel_LPM$F4 == "V" | dados_mel_LPM$F4 == "0",])
summary(mod_F4)

anova(mod_F4_null, mod_F4, test = "Chisq")



mod_T1_null <- multinom(T1 ~ 1, data = dados_mel_LPM[dados_mel_LPM$T1 != "4",])
summary(mod_T1_null)
mod_T1 <- multinom(T1 ~ I(O) + I(Sexo) + LAM + t_Viagem, data = dados_mel_LPM[dados_mel_LPM$T1 != "4",])
summary(mod_T1)

anova(mod_T1_null, mod_T1, test = "Chisq")

z <- summary(mod_T1)$coefficients/summary(mod_T1)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mod_T1))






mod_T2_null <- multinom(T2 ~ 1, data = dados_mel_LPM)
summary(mod_T2_null)
mod_T2 <- multinom(T2 ~ I(O) + I(Sexo) + t_Viagem, data = dados_mel_LPM)
summary(mod_T2)

anova(mod_T2_null, mod_T2, test = "Chisq")

z <- summary(mod_T2)$coefficients/summary(mod_T2)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mod_T2))








mod_T3_null <- multinom(T3 ~ 1, data = dados_mel_LPM[dados_mel_LPM$T3 != "4" & dados_mel_LPM$T3 != "3",])
summary(mod_T3_null)
mod_T3 <- multinom(T3 ~ I(O) + I(Sexo), data = dados_mel_LPM[dados_mel_LPM$T3 != "4" & dados_mel_LPM$T3 != "3",])
summary(mod_T3)

anova(mod_T3_null, mod_T3, test = "Chisq")

z <- summary(mod_T3)$coefficients/summary(mod_T3)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mod_T3))





mod_T4_null <- multinom(T4 ~ 1, data = dados_mel_LPM[dados_mel_LPM$T4 != "4",])
summary(mod_T4_null)
mod_T4 <- multinom(T4 ~ I(O) + I(Sexo), data = dados_mel_LPM[dados_mel_LPM$T4 != "4",])
summary(mod_T4)

anova(mod_T4_null, mod_T4, test = "Chisq")

z <- summary(mod_T4)$coefficients/summary(mod_T4)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mod_T4))



mod_S1_null <- multinom(S1 ~ 1, data = dados_mel_LPM[dados_mel_LPM$S1 != "4",])
summary(mod_S1_null)
mod_S1 <- multinom(S1 ~ I(O) + I(Sexo) + t_Viagem, data = dados_mel_LPM[dados_mel_LPM$S1 != "4",])
summary(mod_S1)

anova(mod_S1_null, mod_S1, test = "Chisq")

z <- summary(mod_S1)$coefficients/summary(mod_S1)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mod_S1))


mod_S2_null <- multinom(S2 ~ 1, data = dados_mel_LPM[dados_mel_LPM$S2 != "3",])
summary(mod_S2_null)
mod_S2 <- multinom(S2 ~ I(O) + t_Viagem, data = dados_mel_LPM[dados_mel_LPM$S2 != "3",])
summary(mod_S2)

anova(mod_S2_null, mod_S2, test = "Chisq")

z <- summary(mod_S2)$coefficients/summary(mod_S2)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mod_S2))



mod_S3_null <- multinom(S3 ~ 1, data = dados_mel_LPM[dados_mel_LPM$S3 != "1" & dados_mel_LPM$S3 != "4",])
summary(mod_S3_null)
mod_S3 <- multinom(S3 ~ I(O) + I(Sexo), data = dados_mel_LPM[dados_mel_LPM$S3 != "1" & dados_mel_LPM$S3 != "4",])
summary(mod_S3)

anova(mod_S3_null, mod_S3, test = "Chisq")

z <- summary(mod_S3)$coefficients/summary(mod_S3)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mod_S3))



mod_S4_null <- multinom(S4 ~ 1, data = dados_mel_LPM[dados_mel_LPM$S4 != "4",])
summary(mod_S4_null)
mod_S4 <- multinom(S4 ~ I(O) + I(Sexo), data = dados_mel_LPM[dados_mel_LPM$S4 != "4",])
summary(mod_S4)

anova(mod_S4_null, mod_S4, test = "Chisq")

z <- summary(mod_S4)$coefficients/summary(mod_S4)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

exp(coef(mod_S4))


mod_C1_null <- glm(C1 ~ 1, family = binomial, data = dados_mel_LPM[dados_mel_LPM$C1 == "1" | dados_mel_LPM$C1 == "0",])
summary(mod_C1_null)
mod_C1 <- glm(C1 ~ I(Sexo) + t_Viagem, family = binomial, data = dados_mel_LPM[dados_mel_LPM$C1 == "1" | dados_mel_LPM$C1 == "0",])
summary(mod_C1)

exp(coef(mod_C1))

anova(mod_C1_null, mod_C1, test = "Chisq")

mod_C2_null <- glm(C2 ~ 1, family = binomial, data = dados_mel_LPM[dados_mel_LPM$C2 == "1" | dados_mel_LPM$C2 == "0",])
summary(mod_C2_null)
mod_C2 <- glm(C2 ~ I(O) + t_Viagem, family = binomial, data = dados_mel_LPM[dados_mel_LPM$C2 == "1" | dados_mel_LPM$C2 == "0",])
summary(mod_C2)

exp(coef(mod_C2))

anova(mod_C2_null, mod_C2, test = "Chisq")


mod_C3_null <- glm(C3 ~ 1, family = binomial, data = dados_mel_LPM[dados_mel_LPM$C3 == "1" | dados_mel_LPM$C3 == "0",])
summary(mod_F4_null)
mod_C3 <- glm(C3 ~ I(O) + t_Abegoaria, family = binomial, data = dados_mel_LPM[dados_mel_LPM$C3 == "1" | dados_mel_LPM$C3 == "0",])
summary(mod_C3)

exp(coef(mod_C3))

anova(mod_C3_null, mod_C3, test = "Chisq")

mod_C4_null <- glm(C4 ~ 1, family = binomial, data = dados_mel_LPM[dados_mel_LPM$C4 == "1" | dados_mel_LPM$C4 == "0",])
summary(mod_C4_null)
mod_C4 <- glm(C4 ~ I(O) + LAM, family = binomial, data = dados_mel_LPM[dados_mel_LPM$C4 == "1" | dados_mel_LPM$C4 == "0",])
summary(mod_C4)

exp(coef(mod_C4))

anova(mod_C4_null, mod_C4, test = "Chisq")
