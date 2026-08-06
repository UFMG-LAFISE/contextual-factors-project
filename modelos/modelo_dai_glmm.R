
#Disponibilizando bibliotecas
library(lme4)
library(performance)
library(sjPlot)
library(DHARMa)
library(glmmTMB)
library(lmerTest)
library(lmtest)
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


#MODELO "glmmTMB"

modelo4_teste <- glmmTMB(dai_min ~
                           posicao +
                           quarto_jg +
                           idade +
                           tempo +
                           mando +
                           classificacao +
                           dif_duo +
                           interv_dias +
                           (1 | npf) +
                           (1 | nome_sessao),
                         ziformula = ~1,
                         family = tweedie(link = 'log'),
                         data = df
)



sim_res <- simulateResiduals(modelo4_teste, n = 50)

testZeroInflation(sim_res)

plot(sim_res)

testUniformity(sim_res)
testDispersion(sim_res)
testZeroInflation(sim_res)

###################################################################################################################
#Visualização dos Resultados
tab_model(
  modelo4_teste,
  show.re.var = TRUE,           # Mostra a variância dos efeitos aleatórios (atleta)
  show.icc = TRUE,              # Coeficiente de Correlação Intraclasse (importante no doutorado)
  show.ngroups = FALSE,
  p.style = "numeric_stars",    # Mostra p-value e estrelas (* p < 0.05)
  p.threshold = c(0.05, 0.01, 0.001),
  dv.labels = c("Dist. Alta Intensidade"),
  string.pred = "Preditores",
  string.est = "Estimativa (Beta)",
  title = "Tabela. Modelos Mistos para Distâncias Percorridas e Fatores Contextuais"
)
