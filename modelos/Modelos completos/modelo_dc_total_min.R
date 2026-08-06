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
df$quarto <- as.factor(df$quarto)
df$interv_duo <- as.factor(df$interv_duo)
df$dif_duo <- as.factor(df$dif_duo)
######################################################################################################################
# Centralizando as variaveis pela média

df$idade <- scale(df$idade, center = TRUE, scale = FALSE)
df$dif <- scale(df$dif, center = TRUE, scale = FALSE)
df$interv_dias <- scale(df$interv_dias, center = TRUE, scale = FALSE)


#MODELOS

#modelo 1
mixed_model1 <- lmer(
  dc_total_min ~ posicao + interv_dias + quarto_jg + idade + tempo +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)

#modelo 2 
mixed_model2 <- lmer(
  dc_total_min ~ posicao + interv_dias + quarto_jg + idade + tempo + mando +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)

#modelo 3
mixed_model3 <- lmer(
  dc_total_min ~ posicao + interv_dias + quarto_jg + idade + tempo + mando + classificacao +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)

#modelo 4
mixed_model4 <- lmer(
  dc_total_min ~ posicao + interv_dias + quarto_jg + idade + tempo + mando + classificacao + dif +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)


#modelo 5
mixed_model5 <- lmer(
  dc_total_min ~ posicao + interv_duo + quarto_jg + idade + tempo + mando + classificacao + dif_duo + interv_dias +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)


####################################################################################################################
#compare modelos
compare_performance(mixed_model1, mixed_model2, mixed_model3, mixed_model4, mixed_model5, rank = TRUE)
####################################################################################################################
#Análise de variância dos modelos
anova(mixed_model1, mixed_model2, mixed_model3, mixed_model4, mixed_model5)
####################################################################################################################
#Verificação de colinearidade
check_collinearity(mixed_model1)

###################################################################################################################
#Visualização dos Resultados
tab_model(
  mixed_model1, mixed_model2, mixed_model3, mixed_model4, mixed_model5,
  show.re.var = TRUE,           # Mostra a variância dos efeitos aleatórios (atleta)
  show.icc = TRUE,              # Coeficiente de Correlação Intraclasse (importante no doutorado)
  show.stat = TRUE,             # Mostra a estatística t
  show.aic = TRUE,
  p.style = "numeric_stars",    # Mostra p-value e estrelas (* p < 0.05)
  p.threshold = c(0.05, 0.01, 0.001),
  dv.labels = c("modelo 1 AC_Total_min", "modelo 2 AC_Total_min", "modelo 3 AC_Total_min", "modelo 4 AC_Total_min", "modelo 5 AC_Total_min"),
  string.pred = "Preditores",
  string.est = "Estimativa (Beta)",
  title = "Tabela. Modelos Mistos para Acelerações Totais por Minuto Jogado e Fatores Contextuais"
)
