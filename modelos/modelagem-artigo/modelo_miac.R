#Variável: Moderate intense acceleration (miac)
#
#miac é um DADO DE CONTAGEM (número de acelerações em zona moderada-intensa),
#não uma variável contínua — a versão "por minuto" na base é só a contagem bruta
#dividida pelo tempo em quadra. Por isso, em vez de reconstruir o valor bruto e
#modelar com lmer() (como as outras 4 variáveis), aqui:
#   - a contagem bruta é reconstruída (miac * playing_time, que recupera um inteiro)
#   - o modelo é um GLMM de contagem (não LMM), com offset = log(playing_time)
#     representando a exposição (tempo em quadra), no lugar de playing_time como
#     covariável comum
#   - playing_time entra SEM centralizar (offsets em modelos de contagem usam a
#     escala natural da exposição, não faz sentido centralizar)
#
#Testamos Poisson vs. binomial negativa (ver nota antes dos modelos) - binomial
#negativa venceu com folga por causa de superdispersão real nos dados.


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

#reconstrução da contagem bruta (miac já vem como taxa ACMI/tempo no tratamento;
#multiplicando de volta por playing_time recupera a contagem original, que é
#inteira a menos de erro de ponto flutuante - por isso o round())
df$miac_count <- round(df$miac * df$playing_time)

#centralização das variáveis numéricas contínuas - EXCETO playing_time, que entra
#sem centralizar, na escala natural, dentro do offset(log(playing_time))
df$interval_c <- as.numeric(scale(df$interval, scale = FALSE))
df$age_c <- as.numeric(scale(df$age, scale = FALSE))
df$opponent_level_c <- as.numeric(scale(df$opponent_level, scale = FALSE))
df$score_dif_c <- as.numeric(scale(df$score_dif, scale = FALSE))

#--------------------------------------------------------------------------
# TESTE DE DISTRIBUIÇÃO: Poisson vs. binomial negativa
#--------------------------------------------------------------------------
#Poisson assume media = variancia. Testamos o modelo mais completo (equivalente
#ao modelo 4) nas duas familias e comparamos por AIC + teste de superdispersao.
#(Binomial "comum" não é adequada aqui: exige um número fixo de tentativas/
#ensaios, que não existe para uma contagem de acelerações sem teto natural -
#por isso a alternativa correta a testar contra Poisson é a binomial negativa,
#não a binomial.)

fx_teste <- "miac_count ~ position + interval_c + quarter + age_c + playng_venue + opponent_level_c + score_dif_c + offset(log(playing_time))"
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
cat("Negativa é a distribuição adequada (foi o resultado obtido: dispersão ~3.3,\n")
cat("AIC bem menor na Binomial Negativa) - os 4 modelos abaixo usam nbinom2.\n")

#--------------------------------------------------------------------------
#MODELOS (Binomial Negativa, offset = log(playing_time))
#--------------------------------------------------------------------------

#modelo 1
mixed_model1 <- glmmTMB(
  miac_count ~ position + interval_c + quarter + age_c + offset(log(playing_time)) +
    (1 | player) + (1 | match/qt_match) ,
  data = df, family = nbinom2(link = "log")
)

#modelo 2
mixed_model2 <- glmmTMB(
  miac_count ~ position + interval_c + quarter + age_c + playng_venue + offset(log(playing_time)) +
    (1 | player) + (1 | match/qt_match) ,
  data = df, family = nbinom2(link = "log")
)

#modelo 3
mixed_model3 <- glmmTMB(
  miac_count ~ position + interval_c + quarter + age_c + playng_venue + opponent_level_c + offset(log(playing_time)) +
    (1 | player) + (1 | match/qt_match) ,
  data = df, family = nbinom2(link = "log")
)

#modelo 4
mixed_model4 <- glmmTMB(
  miac_count ~ position + interval_c + quarter + age_c + playng_venue + opponent_level_c + score_dif_c + offset(log(playing_time)) +
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
  dv.labels = c("modelo 1 Miac", "modelo 2 Miac", "modelo 3 Miac", "modelo 4 Miac"),
  string.pred = "Preditores",
  string.est = "IRR (exp(Beta))",
  title = "Tabela. Modelos Mistos de Contagem (Binomial Negativa) para Miac (Moderate Intense Acceleration) e Fatores Contextuais"
  ,
  file = if (exists("EXPORT_DIR")) file.path(EXPORT_DIR, paste0("tabela_", "miac", ".html")) else NULL
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
       title = "Efeitos fixos - Modelo 4 (Miac, Binomial Negativa)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(grafico_efeitos_fixos)
if (exists("EXPORT_DIR")) ggsave(file.path(EXPORT_DIR, "graficos", "miac_efeitos_fixos.png"), grafico_efeitos_fixos, width = 8, height = 6, dpi = 300)

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
       title = "Efeitos aleatórios por atleta - Modelo 4 (Miac, Binomial Negativa)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.y = element_text(size = 6), legend.position = "top")

print(grafico_caterpillar)
if (exists("EXPORT_DIR")) ggsave(file.path(EXPORT_DIR, "graficos", "miac_caterpillar.png"), grafico_caterpillar, width = 8, height = 10, dpi = 300)

#--------------------------------------------------------------------------
#3. DIAGNÓSTICO - pressupostos ADEQUADOS A DADOS DE CONTAGEM
#--------------------------------------------------------------------------
#Não faz sentido usar check_normality()/check_heteroscedasticity() aqui - essas
#checagens assumem resíduos gaussianos contínuos, o que não se aplica a um GLMM
#de contagem. No lugar, os pressupostos relevantes são:
#   - superdispersão (a binomial negativa já modela isso via theta, mas vale
#     conferir se ainda sobra dispersão não capturada)
#   - inflação de zeros (excesso de zeros além do que a distribuição prevê)
#   - singularidade da estrutura de efeitos aleatórios
#   - check_model() do pacote performance já se adapta automaticamente a modelos
#     de contagem (troca os painéis de normalidade/homocedasticidade por
#     verificação de dispersão e resíduos simulados/quantílicos)

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
  png(file.path(EXPORT_DIR, "pressupostos", "miac_check_model.png"), width = 1600, height = 1400, res = 150)
  print(diagnostico_modelo4)
  dev.off()
}

