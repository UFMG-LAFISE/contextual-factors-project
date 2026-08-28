#Variável: High intense acceleration (hiac)

# 1. Instalação e Carregamento de Pacotes
if (!require("glmmTMB")) install.packages("glmmTMB")
if (!require("performance")) install.packages("performance")
if (!require("sjPlot")) install.packages("sjPlot")
if (!require("broom.mixed")) install.packages("broom.mixed")
if (!require("ggplot2")) install.packages("ggplot2")

#Disponibilizando bibliotecas
library(glmmTMB)
library(performance)
library(sjPlot)
library(broom.mixed)
library(ggplot2)

#patch: R 4.3.3 nao tem %||% nativo (so a partir do R 4.4), e o glmmTMB assume que existe
#(necessario pra print()/VarCorr() de objetos glmmTMB nao quebrarem)
`%||%` <- function(a, b) if (is.null(a)) b else a

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
df$quarter <- as.factor(df$quarter)

#reconstrução da contagem bruta
df$hiac_count <- round(df$hiac * df$playing_time)

#exclusão dos zeros - não são eventos reais 
df <- df[df$hiac_count > 0, ]

#centralização das variáveis numéricas contínuas - EXCETO playing_time, que entra
#sem centralizar, na escala natural, dentro do offset(log(playing_time))
df$interval_c <- as.numeric(scale(df$interval, scale = FALSE))
df$age_c <- as.numeric(scale(df$age, scale = FALSE))
df$opponent_level_c <- as.numeric(scale(df$opponent_level, scale = FALSE))
df$score_dif_c <- as.numeric(scale(df$score_dif, scale = FALSE))

#--------------------------------------------------------------------------
# TESTE DE DISTRIBUIÇÃO: Poisson vs. binomial negativa
#--------------------------------------------------------------------------

fx_teste <- "hiac_count ~ position + interval_c + quarter + age_c + playng_venue + opponent_level_c + score_dif_c + offset(log(playing_time))"
re_teste <- "+ (1 | player) + (1 | match/qt_match)"

teste_poisson <- glmmTMB(as.formula(paste(fx_teste, re_teste)), data = df, family = poisson(link = "log"))
teste_nbinom  <- glmmTMB(as.formula(paste(fx_teste, re_teste)), data = df, family = nbinom2(link = "log"))

cat("=== Comparação de distribuição: Poisson vs. Binomial Negativa ===\n")
cat("AIC Poisson:", AIC(teste_poisson), "\n")
cat("AIC Binomial Negativa:", AIC(teste_nbinom), "\n")
cat("\nTeste de superdispersão (Poisson):\n")
print(check_overdispersion(teste_poisson))
cat("\nConclusão: se a Binomial Negativa tiver AIC menor e/ou o teste de\n")
cat("superdispersão do Poisson indicar razão > 1 com p<0.05, a Binomial\n")
cat("Negativa é a distribuição adequada. Com os zeros (não-genuínos) excluídos,\n")
cat("a Binomial Negativa comum já resolve a dispersão sem precisar de\n")
cat("componente de inflação de zero - os 4 modelos abaixo usam nbinom2.\n")

#--------------------------------------------------------------------------
#MODELOS (Binomial Negativa, offset = log(playing_time))
#--------------------------------------------------------------------------

#modelo 1
mixed_model1 <- glmmTMB(
  hiac_count ~ position + interval_c + quarter + age_c + offset(log(playing_time)) +
    (1 | player) + (1 | match/qt_match) ,
  data = df, family = nbinom2(link = "log")
)

#modelo 2
mixed_model2 <- glmmTMB(
  hiac_count ~ position + interval_c + quarter + age_c + playng_venue + offset(log(playing_time)) +
    (1 | player) + (1 | match/qt_match) ,
  data = df, family = nbinom2(link = "log")
)

#modelo 3
mixed_model3 <- glmmTMB(
  hiac_count ~ position + interval_c + quarter + age_c + playng_venue + opponent_level_c + offset(log(playing_time)) +
    (1 | player) + (1 | match/qt_match) ,
  data = df, family = nbinom2(link = "log")
)

#modelo 4
mixed_model4 <- glmmTMB(
  hiac_count ~ position + interval_c + quarter + age_c + playng_venue + opponent_level_c + score_dif_c + offset(log(playing_time)) +
    (1 | player) + (1 | match/qt_match) ,
  data = df, family = nbinom2(link = "log")
)


#compare modelos
compare_performance(mixed_model1, mixed_model2, mixed_model3, mixed_model4, rank = TRUE)


#Visualização dos Resultados
#transform="exp" reporta os coeficientes como IRR (incidence rate ratio) -
#a escala natural de interpretação de um modelo log-linear de contagem
tab_model(
  mixed_model1, mixed_model2, mixed_model3, mixed_model4,
  transform = "exp",
  show.re.var = TRUE,           # Mostra a variância dos efeitos aleatórios (atleta)
  show.icc = TRUE,              # Coeficiente de Correlação Intraclasse (importante no doutorado)
  show.stat = TRUE,             # Mostra a estatística t
  p.style = "numeric_stars",    # Mostra p-value e estrelas (* p < 0.05)
  p.threshold = c(0.05, 0.01, 0.001),
  dv.labels = c("modelo 1 hiac", "modelo 2 hiac", "modelo 3 hiac", "modelo 4 hiac"),
  string.pred = "Preditores",
  string.est = "IRR (exp(Beta))",
  title = "Tabela. Modelos Mistos de Contagem (Binomial Negativa) para hiac (High Intense Acceleration) e Fatores Contextuais"
  ,
  file = if (exists("EXPORT_DIR")) file.path(EXPORT_DIR, paste0("tabela_", "hiac", ".html")) else NULL
)

#Gráficos do modelo 4 (modelo final)

#cores (paleta diverging: azul = positivo, vermelho = negativo; cinza = neutro/individual)
cor_positivo <- "#2a78d6"
cor_negativo <- "#e34948"
cor_neutro   <- "#898781"

#1. EFEITOS FIXOS - forest plot (escala IRR, exp(estimate); referência em 1, não em 0)
efeitos_fixos <- broom.mixed::tidy(mixed_model4, effects = "fixed", conf.int = TRUE)
efeitos_fixos <- efeitos_fixos[efeitos_fixos$term != "(Intercept)", ]
efeitos_fixos$irr <- exp(efeitos_fixos$estimate)
efeitos_fixos$irr_low <- exp(efeitos_fixos$conf.low)
efeitos_fixos$irr_high <- exp(efeitos_fixos$conf.high)
efeitos_fixos$sinal <- ifelse(efeitos_fixos$irr > 1, "Positivo", "Negativo")

grafico_efeitos_fixos <- ggplot(efeitos_fixos, aes(x = irr, y = reorder(term, irr), color = sinal)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = cor_neutro) +
  geom_pointrange(aes(xmin = irr_low, xmax = irr_high), size = 0.6) +
  scale_color_manual(values = c("Positivo" = cor_positivo, "Negativo" = cor_negativo)) +
  labs(x = "IRR (razão de taxas de incidência)", y = NULL, color = NULL,
       title = "Efeitos fixos - Modelo 4 (hiac, Binomial Negativa)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(grafico_efeitos_fixos)
if (exists("EXPORT_DIR")) ggsave(file.path(EXPORT_DIR, "graficos", "hiac_efeitos_fixos.png"), grafico_efeitos_fixos, width = 8, height = 6, dpi = 300)

#2. CATERPILLAR PLOT - efeitos aleatórios por atleta (apenas intercepto - modelo nao tem slope)
ranef_tidy <- broom.mixed::tidy(mixed_model4, effects = "ran_vals")
ranef_tidy <- ranef_tidy[ranef_tidy$group == "player", ]
ranef_tidy$sinal <- ifelse(ranef_tidy$estimate > 0, "Positivo", "Negativo")

grafico_caterpillar <- ggplot(ranef_tidy, aes(x = estimate, y = reorder(level, estimate), color = sinal)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = cor_neutro) +
  geom_pointrange(aes(xmin = estimate - std.error, xmax = estimate + std.error), size = 0.3) +
  scale_color_manual(values = c("Positivo" = cor_positivo, "Negativo" = cor_negativo)) +
  facet_wrap(~ term, scales = "free_x") +
  labs(x = "Desvio em relação à média (escala log)", y = "Atleta", color = NULL,
       title = "Efeitos aleatórios por atleta - Modelo 4 (hiac, Binomial Negativa)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 6), legend.position = "top")

print(grafico_caterpillar)
if (exists("EXPORT_DIR")) ggsave(file.path(EXPORT_DIR, "graficos", "hiac_caterpillar.png"), grafico_caterpillar, width = 8, height = 10, dpi = 300)

#--------------------------------------------------------------------------
#3. DIAGNÓSTICO - pressupostos ADEQUADOS A DADOS DE CONTAGEM
#--------------------------------------------------------------------------

cat("\n=== check_overdispersion (modelo 4) ===\n")
print(check_overdispersion(mixed_model4))

cat("\n=== check_zeroinflation (modelo 4) ===\n")
print(check_zeroinflation(mixed_model4))

cat("\n=== check_singularity (modelo 4) ===\n")
print(check_singularity(mixed_model4))

cat("\n=== check_model (modelo 4) ===\n")
diagnostico_modelo4 <- check_model(mixed_model4)
print(diagnostico_modelo4)
if (exists("EXPORT_DIR")) {
  png(file.path(EXPORT_DIR, "pressupostos", "hiac_check_model.png"), width = 1600, height = 1400, res = 150)
  print(diagnostico_modelo4)
  dev.off()
}

