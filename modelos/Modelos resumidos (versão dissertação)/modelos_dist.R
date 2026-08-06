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
  dits_total_min ~ posicao + quarto_jg + idade + tempo + mando + classificacao + dif_duo + interv_dias +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = TRUE
)

#modelo 2 
mixed_model2 <- lmer(
  zv3_min ~ posicao + quarto_jg + idade + tempo + mando + classificacao + dif_duo + interv_dias +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = TRUE
)

#modelo 3
mixed_model3 <- lmer(
  dmi_min ~ posicao + quarto_jg + idade + tempo + mando + classificacao + dif_duo + interv_dias +
    (1 | npf) + (1 | nome_sessao),
  data = df, REML = TRUE
)

#modelo 4
mixed_model4 <- lmer(
  dai_min ~ posicao + quarto_jg + idade + tempo + mando + classificacao + dif_duo + interv_dias +
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
  dv.labels = c("Dist. Total", "Dist. Intensidade Moderada", "Dist. Moderada a Alta", "Dist. Alta Intensidade"),
  string.pred = "Preditores",
  string.est = "Estimativa (Beta)",
  title = "Tabela. Modelos Mistos para Distâncias Percorridas e Fatores Contextuais"
)

####################################################################################################################
## Análise dos pressupostos do modelo linear de efeitos mistos

### Análise da normalidade e homocedasticidade dos resíduos e dos efeitos aleatórios

run_model_diagnostics <- function(model_object, model_name) {
  
  # Sugestão do gemini para não retornar notação científica
  old_opts <- options(scipen = 999, digits = 4)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    options(old_opts)
    par(old_par)
  })
  
  cat(paste0("\n--- Diagnostics for: ", model_name, " ---\n"))
  cat("\n--- Checking Residuals ---\n")
  model_residuals <- residuals(model_object)
  
  if (length(model_residuals) > 5000) {
    cat("Note: Shapiro-Wilk test is not recommended for n > 5000. Using Lilliefors test.\n")
    shapiro_available <- FALSE
  } else if (length(model_residuals) < 3) {
    cat("Warning: Not enough data points for Shapiro-Wilk test.\n")
    shapiro_available <- FALSE
  } else {
    shapiro_available <- TRUE
  }
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  
  par(mfrow = c(2, 2), mar = c(3, 3, 2, 1), mgp = c(2, 0.7, 0)) 
  
  hist(model_residuals,
       main = paste("Histogram of Residuals (", model_name, ")"),
       xlab = "Residuals",
       col = "lightblue",
       border = "black")
  
  qqnorm(model_residuals,
         main = paste("Q-Q Plot of Residuals (", model_name, ")"),
         xlab = "Theoretical Quantiles",
         ylab = "Sample Quantiles")
  qqline(model_residuals, col = "red")
  
  plot(fitted(model_object), model_residuals,
       main = paste("Residuals vs. Fitted (", model_name, ")"),
       xlab = "Fitted Values",
       ylab = "Residuals",
       pch = 19,
       col = "darkgreen",
       cex = 0.8)
  abline(h = 0, col = "red", lty = 2) 
  if (shapiro_available) {
    cat("\nShapiro-Wilk Test for Residuals:\n")
    print(shapiro.test(model_residuals))
  }
  cat("\nLilliefors (Kolmogorov-Smirnov) Test for Residuals:\n")
  print(lillie.test(model_residuals))

  # --- RANDOM EFFECTS DIAGNOSTICS ---
  cat("\n--- Checking Random Effects (Player_id Intercepts) ---\n")
  random_effects_player <- tryCatch({
    ranef(model_object)$npf$`(Intercept)`
  }, error = function(e) {
    message("Could not extract random effects for Player_id. Error: ", e$message)
    return(NULL)
  })
  
  if (!is.null(random_effects_player) && length(random_effects_player) > 1) {
    # Check for sufficient data for Shapiro-Wilk test on random effects
    if (length(random_effects_player) > 5000) {
      cat("Note: Shapiro-Wilk test is not recommended for n > 5000 for random effects. Using Lilliefors test.\n")
      shapiro_re_available <- FALSE
    } else if (length(random_effects_player) < 3) {
      cat("Warning: Not enough data points for Shapiro-Wilk test on random effects.\n")
      shapiro_re_available <- FALSE
    } else {
      shapiro_re_available = TRUE
    }
    par(mfrow = c(2, 2), mar = c(3, 3, 2, 1), mgp = c(2, 0.7, 0)) # 1 row, 2 columns for RE plots
    hist(random_effects_player,
         main = paste("Histogram of Random Effects (", model_name, ")"),
         xlab = "Random Intercepts",
         col = "lightgreen",
         border = "black")
    qqnorm(random_effects_player,
           main = paste("Q-Q Plot of Random Effects (", model_name, ")"),
           xlab = "Theoretical Quantiles",
           ylab = "Sample Quantiles")
    qqline(random_effects_player, col = "red")
    
    # Run Normality Tests for Random Effects
    if (shapiro_re_available) {
      cat("\nShapiro-Wilk Test for Random Effects:\n")
      print(shapiro.test(random_effects_player))
    }
    cat("\nLilliefors (Kolmogorov-Smirnov) Test for Random Effects:\n")
    print(lillie.test(random_effects_player))
    
  } else {
    cat("Skipping random effects diagnostics: No random effects found or insufficient data.\n")
  }
  cat(paste0("\n--- End Diagnostics for: ", model_name, " ---\n"))
}

models_to_process <- list(
  mixed_model1 = mixed_model1,
  mixed_model2 = mixed_model2,
  mixed_model3 = mixed_model3, 
  mixed_model4 = mixed_model4
)

for (name in names(models_to_process)) {
  model_obj <- models_to_process[[name]]
  run_model_diagnostics(model_obj, name)
}
###############################################################################################################################

#pot hoc para compararação posições

#modelo1
emm_posição_mm1 <- emmeans(mixed_model1, "posicao", pbkrtest.limit = 4561)
summary(emm_posição_mm1)
pos_pairs_mm1 <- contrast(emm_posição_mm1, method = "pairwise", adjust = "tukey")
summary(pos_pairs_mm1)

#modelo2
emm_posição_mm2 <- emmeans(mixed_model2, "posicao", pbkrtest.limit = 4561)
summary(emm_posição_mm2)
pos_pairs_mm2 <- contrast(emm_posição_mm2, method = "pairwise", adjust = "tukey")
summary(pos_pairs_mm2)

#modelo3
emm_posição_mm3 <- emmeans(mixed_model3, "posicao", pbkrtest.limit = 4561)
summary(emm_posição_mm3)
pos_pairs_mm3 <- contrast(emm_posição_mm3, method = "pairwise", adjust = "tukey")
summary(pos_pairs_mm3)

#modelo4
emm_posição_mm4 <- emmeans(mixed_model4, "posicao", pbkrtest.limit = 4561)
summary(emm_posição_mm4)
pos_pairs_mm4 <- contrast(emm_posição_mm4, method = "pairwise", adjust = "tukey")
summary(pos_pairs_mm4)


