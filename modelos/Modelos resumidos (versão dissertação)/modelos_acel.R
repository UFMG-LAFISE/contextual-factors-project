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
df <- read.csv("C:/Users/Usuário/Downloads/base_dados_derivada_min.csv")
#######################################################################################################################
#transformações de dados
df$npf <- as.factor(df$npf)
df$nome_sessao <- as.factor(df$nome_sessao)
df$mando <- as.factor(df$mando)
df$posicao <- as.factor(df$posicao)
df$dif_duo <- as.factor(df$dif_duo)
df$idade <- as.integer (df$idade)
df$tempo <- as.numeric (df$tempo)
df$quarto_jg <- as.integer (df$quarto_jg)
df$interv_dias <- as.integer (df$interv_dias)
df$classificacao <- as.integer (df$classificacao)
df$trimp <- as.numeric(df$trimp)
######################################################################################################################
# Centralizando as variaveis pela média

df$idade <- scale(df$idade, center = TRUE, scale = FALSE)
df$dif <- scale(df$dif, center = TRUE, scale = FALSE)
df$interv_dias <- scale(df$interv_dias, center = TRUE, scale = FALSE)


#MODELOS

#modelo 1
mixed_model1 <- lmer(
  ac_total_min ~ posicao + quarto_jg + idade + tempo + mando + classificacao + dif_duo + interv_dias +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = TRUE
)

#modelo 2 
mixed_model2 <- lmer(
  acel2_min ~ posicao + quarto_jg + idade + tempo + mando + classificacao + dif_duo + interv_dias +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = TRUE
)

#modelo 3
mixed_model3 <- lmer(
  acmi_min ~ posicao + quarto_jg + idade + tempo + mando + classificacao + dif_duo + interv_dias +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = TRUE
)

#modelo 4
mixed_model4 <- lmer(
  aci_min ~ posicao + quarto_jg + idade + tempo + mando + classificacao + dif_duo + interv_dias +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = TRUE
)


####################################################################################################################
#compare modelos
compare_performance(mixed_model1, mixed_model2, mixed_model3, mixed_model4, rank = TRUE)
####################################################################################################################
#Análise de variância dos modelos
anova(mixed_model1, mixed_model2, mixed_model3, mixed_model4)
####################################################################################################################
#Verificação de colinearidade
check_collinearity(mixed_model1)
check_collinearity(mixed_model2)
check_collinearity(mixed_model3)
check_collinearity(mixed_model4)
###################################################################################################################
#Visualização dos Resultados
tab_model(
  mixed_model1, mixed_model2, mixed_model3, mixed_model4,
  show.re.var = TRUE,           # Mostra a variância dos efeitos aleatórios (atleta)
  show.icc = TRUE,              # Coeficiente de Correlação Intraclasse (importante no doutorado)
  show.ngroups = FALSE,
  p.style = "numeric_stars",    # Mostra p-value e estrelas (* p < 0.05)
  p.threshold = c(0.05, 0.01, 0.001),
  dv.labels = c("Acel. Total", "Acel. Moderada", "Acel. Moderada a Intensa", "Acel. Intensa"),
  string.pred = "Preditores",
  string.est = "Estimativa (Beta)",
  title = "Tabela. Modelos Mistos para Número de acelerações e Fatores Contextuais"
)
