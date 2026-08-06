## Teste de modelagem Letícia Gontijo
## modelo linear de feitos mistos


######################################################################################################################
# 1. Instalação e Carregamento de Pacotes
if (!require("lme4")) install.packages("lme4")
if (!require("performance")) install.packages("performance")
if (!require("sjPlot")) install.packages("sjPlot")

#####################################################################################################################
#Disponibilizando bibliotecas
library(lme4)
library(performance)
library(sjPlot)

#######################################################################################################################
#Importando bibliotecas
df <- read.csv("/home/leticia-gontijo/Documents/projeto_gui_mestrado/base_dados_brutos_tratados.csv", header = TRUE, stringsAsFactors = FALSE)
#######################################################################################################################
#transformações de dados
df$npf <- as.factor(df$npf)
df$nome_sessao <- as.factor(df$nome_sessao)
df$mando <- as.factor(df$mando)
df$posicao <- as.factor(df$posicao)
df$quarto <- as.factor(df$quarto)
######################################################################################################################
#MODELOS

#modelo 1
mixed_model1 <- lmer(
  dmi ~ mando + posicao + interv_dias + classificacao + quarto + idade + dif + 
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)



#modelo 2 
mixed_model2 <- lmer(
  dmi ~ mando + posicao + interv_dias + quarto + classificacao + idade + 
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)


####################################################################################################################
#compare modelos
compare_performance(mixed_model1, mixed_model2, rank = TRUE)
####################################################################################################################
#Análise de variância dos modelos
anova(mixed_model1, mixed_model2)
####################################################################################################################
#Verificação de colinearidade
check_collinearity(mixed_model1)

###################################################################################################################
#Visualização dos Resultados
tab_model(mixed_model1, mixed_model2, show.re.var = TRUE)

###################################################################################################################


#analise leticia gontijo
#parece que a relaçao intervalo de dias e dmi nao é linear
# testarei Modelos Aditivos Generalizados Mistos (GAMM)


library(mgcv)# biblioteca
# O s() indica a função de suavização para a variável não linear
gam_model <- gamm(dmi ~ s(interv_dias) + mando + posicao + quarto + idade,
                  random = list(npf = ~1, nome_sessao = ~1),
                  data = df)
summary(gam_model$gam)
plot(gam_model$gam) # Gera a curva não linear se houver
summary(gam_model$gam)
#o GAMM é um modelo "mais honesto" com os dados, capturando 6% da variância total com os efeitos fixos