#Variável: Moderate intense acceleration


# 1. Instalação e Carregamento de Pacotes
if (!require("lme4")) install.packages("lme4")
if (!require("performance")) install.packages("performance")
if (!require("sjPlot")) install.packages("sjPlot")
if (!require("broom.mixed")) install.packages("broom.mixed")
if (!require("ggplot2")) install.packages("ggplot2")

#Disponibilizando bibliotecas
library(lme4)
library(performance)
library(sjPlot)
library(broom.mixed)
library(ggplot2)

#Importando base de dados
df <- read.csv('/home/leticia-gontijo/Documents/artigo-gui-mestrado/projeto_gui_mestrado/contextual-factors-project/modelos/modelagem-artigo/base_dados_derivada_min (correção quartos).csv')


#filtro: remove participações muito curtas (tempo em quadra <= 1 min)
df <- df[df$playing_time > 1, ]

#transformações de dados
df$player <- as.factor(df$player)
df$match<- as.factor(df$match)
df$qt_match<- as.factor(df$qt_match)
df$playng_venue <- as.factor(df$playng_venue)
df$position <- as.factor(df$position)
df$quarter_lin <- as.numeric(df$quarter) #quarto como variável contínua, usado só no slope aleatório
df$quarter <- as.factor(df$quarter)

#reconstrução da variável bruta (miac já vem como taxa ACMI/tempo no tratamento)
df$miac_bruto <- df$miac * df$playing_time

#centralização das variáveis numéricas contínuas
df$interval_c <- as.numeric(scale(df$interval, scale = FALSE))
df$age_c <- as.numeric(scale(df$age, scale = FALSE))
df$playing_time_c <- as.numeric(scale(df$playing_time, scale = FALSE))
df$opponent_level_c <- as.numeric(scale(df$opponent_level, scale = FALSE))
df$score_dif_c <- as.numeric(scale(df$score_dif, scale = FALSE))

#MODELOS

#modelo 1
mixed_model1 <- lmer(
  miac_bruto ~ position + interval_c + quarter + age_c + playing_time_c +
    (1 + quarter_lin | player) + (1 | match/qt_match) ,
  data = df, REML = FALSE
)

#modelo 2
mixed_model2 <- lmer(
  miac_bruto ~ position + interval_c + quarter + age_c + playing_time_c + playng_venue +
    (1 + quarter_lin | player) + (1 | match/qt_match) ,
  data = df, REML = FALSE
)

#modelo 3
mixed_model3 <- lmer(
  miac_bruto ~ position + interval_c + quarter + age_c + playing_time_c + playng_venue + opponent_level_c +
    (1 + quarter_lin | player) + (1 | match/qt_match) ,
  data = df, REML = FALSE
)

#modelo 4
mixed_model4 <- lmer(
  miac_bruto ~ position + interval_c + quarter + age_c + playing_time_c + playng_venue + opponent_level_c + score_dif_c +
    (1 + quarter_lin | player) + (1 | match/qt_match) ,
  data = df, REML = FALSE
)


#compare modelos
compare_performance(mixed_model1, mixed_model2, mixed_model3, mixed_model4,rank = TRUE)


#Visualização dos Resultados
tab_model(
  mixed_model1, mixed_model2, mixed_model3, mixed_model4,
  show.re.var = TRUE,           # Mostra a variância dos efeitos aleatórios (atleta)
  show.icc = TRUE,              # Coeficiente de Correlação Intraclasse (importante no doutorado)
  show.stat = TRUE,             # Mostra a estatística t
  p.style = "numeric_stars",    # Mostra p-value e estrelas (* p < 0.05)
  p.threshold = c(0.05, 0.01, 0.001),
  dv.labels = c("modelo 1 Miac", "modelo 2 Miac", "modelo 3 Miac", "modelo 4 Miac"),
  string.pred = "Preditores",
  string.est = "Estimativa (Beta)",
  title = "Tabela. Modelos Mistos para Miac (Moderate Intense Acceleration) e Fatores Contextuais"
)



#Gráficos do modelo 4 (modelo final)

#cores (paleta diverging: azul = positivo, vermelho = negativo; cinza = neutro/individual)
cor_positivo <- "#2a78d6"
cor_negativo <- "#e34948"
cor_neutro   <- "#898781"

#1. EFEITOS FIXOS - forest plot
efeitos_fixos <- broom.mixed::tidy(mixed_model4, effects = "fixed", conf.int = TRUE)
efeitos_fixos <- efeitos_fixos[efeitos_fixos$term != "(Intercept)", ]
efeitos_fixos$sinal <- ifelse(efeitos_fixos$estimate > 0, "Positivo", "Negativo")

grafico_efeitos_fixos <- ggplot(efeitos_fixos, aes(x = estimate, y = reorder(term, estimate), color = sinal)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = cor_neutro) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.6) +
  scale_color_manual(values = c("Positivo" = cor_positivo, "Negativo" = cor_negativo)) +
  labs(x = "Estimativa (Beta)", y = NULL, color = NULL,
       title = "Efeitos fixos - Modelo 4 (Miac)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(grafico_efeitos_fixos)

#2. VARIAÇÃO INDIVIDUAL - trajetória prevista por atleta ao longo dos quartos
ranef_player <- ranef(mixed_model4)$player
ranef_player$player <- rownames(ranef_player)

quartos <- 1:4
fixef_model <- fixef(mixed_model4)
media_quarto <- c(0, unname(fixef_model["quarter2"]), unname(fixef_model["quarter3"]), unname(fixef_model["quarter4"]))
names(media_quarto) <- as.character(1:4)

trajetorias <- do.call(rbind, lapply(seq_len(nrow(ranef_player)), function(i) {
  data.frame(
    player = ranef_player$player[i],
    quarter = quartos,
    trimp_previsto = unname(fixef_model["(Intercept)"]) + ranef_player[i, "(Intercept)"] +
      media_quarto[as.character(quartos)] + ranef_player[i, "quarter_lin"] * quartos
  )
}))

media_populacao <- data.frame(
  quarter = quartos,
  trimp_previsto = unname(fixef_model["(Intercept)"]) + media_quarto[as.character(quartos)]
)

grafico_trajetorias <- ggplot() +
  geom_line(data = trajetorias, aes(x = quarter, y = trimp_previsto, group = player),
            color = cor_neutro, alpha = 0.35, linewidth = 0.4) +
  geom_line(data = media_populacao, aes(x = quarter, y = trimp_previsto),
            color = cor_positivo, linewidth = 1.4) +
  scale_x_continuous(breaks = quartos) +
  labs(x = "Quarto", y = "Miac previsto",
       title = "Trajetória individual de Miac por quarto (modelo 4)",
       subtitle = "Linhas cinza: atletas | Linha azul: média da população") +
  theme_minimal(base_size = 12)

print(grafico_trajetorias)

#2b. CATERPILLAR PLOT - efeitos aleatórios por atleta (intercepto e slope)
ranef_tidy <- broom.mixed::tidy(mixed_model4, effects = "ran_vals")
ranef_tidy <- ranef_tidy[ranef_tidy$group == "player", ]
ranef_tidy$sinal <- ifelse(ranef_tidy$estimate > 0, "Positivo", "Negativo")

grafico_caterpillar <- ggplot(ranef_tidy, aes(x = estimate, y = reorder(level, estimate), color = sinal)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = cor_neutro) +
  geom_pointrange(aes(xmin = estimate - std.error, xmax = estimate + std.error), size = 0.3) +
  scale_color_manual(values = c("Positivo" = cor_positivo, "Negativo" = cor_negativo)) +
  facet_wrap(~ term, scales = "free_x") +
  labs(x = "Desvio em relação à média", y = "Atleta", color = NULL,
       title = "Efeitos aleatórios por atleta - Modelo 4 (Miac)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 6), legend.position = "top")

print(grafico_caterpillar)

#3. DIAGNÓSTICO - pressupostos do modelo 4
check_model(mixed_model4)
