# arquivo teste

#Disponibilizando bibliotecas
library(lme4)
library(performance)
library(sjPlot)
library(broom.mixed)
library(ggplot2)

#Importando base de dados
df <- read.csv('/home/leticia-gontijo/Documents/artigo-gui-mestrado/projeto_gui_mestrado/modelos/modelagem-artigo/base_dados_derivada_min (correção quartos).csv')

#filtro: remove participações muito curtas (tempo em quadra <= 1 min)
df <- df[df$playing_time > 1, ]

#transformações de dados
df$player <- as.factor(df$player)
df$match<- as.factor(df$match)
df$qt_match<- as.factor(df$qt_match)
df$playng_venue <- as.factor(df$playng_venue)
df$position <- as.factor(df$position)
df$quarter <- as.factor(df$quarter)

#Centralizacao
df$age_c <- scale(df$age, scale = FALSE)
df$interval_c <- scale(df$interval, scale = FALSE)
df$playing_time_c <- scale(df$playing_time, scale = FALSE)
df$opponent_level_c <- scale(df$opponent_level, scale = FALSE)
df$score_dif_c <- scale(df$score_dif, scale = FALSE)


#reconstrução da variável bruta (tot_dist já vem como taxa dist_total/tempo no tratamento)
df$dist_total <- df$tot_dist * df$playing_time

#modelo 1
mixed_model1 <- lmer(
  dist_total ~ position + interval_c + quarter + age_c + playing_time_c + playng_venue + opponent_level_c + score_dif_c +
    (1 | player)  + (1 | match/qt_match),
  data = df, REML = FALSE
)

#Visualização dos Resultados
tab_model(
  mixed_model1,
  show.re.var = TRUE,           # Mostra a variância dos efeitos aleatórios (atleta)
  show.icc = TRUE,              # Coeficiente de Correlação Intraclasse (importante no doutorado)
  show.stat = TRUE,             # Mostra a estatística t
  p.style = "numeric_stars",    # Mostra p-value e estrelas (* p < 0.05)
  p.threshold = c(0.05, 0.01, 0.001),
  dv.labels = c("modelo 1 Total Distance"),
  string.pred = "Preditores",
  string.est = "Estimativa (Beta)",
  title = "Tabela. Modelos Mistos para Total Distance e Fatores Contextuais"
)