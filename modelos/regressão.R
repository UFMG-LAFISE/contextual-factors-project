
# Formatação de tabelas
options(digits = 3)
set_flextable_defaults(decimal.mark = ",", big.mark = " ")
theme_set(theme_minimal())

library(readxl)
df <- read_excel("C:/Users/Usuário/Downloads/BAS_base_dados.xlsx")
nrow(df)


df <- df %>%
  filter(temp == "22/23" | temp == "23/24" | temp == "24/25")%>%
  filter(Competição == "NBB") %>%
  filter(quarto >= 1 & quarto <= 4) %>%
  na.omit(interv_dias)
df$interv_dias = as.integer (df$interv_dias)
nrow(df)

df$tempo <- as.numeric(df$tempo)

df <- df %>%
  filter(tempo >= 1)
summary(df$tempo)
nrow(df)


predictor_vars <- c("classificacao", "mando", "posicao", "interv_dias", "dif","dif_modolo", "quarto")
absolut_vars <- c("dits_total", "dmi", "dai", "trimp")
distance_vars_min <- c("dits_total_min", "zv1_min", "zv2_min", "zv3_min", "zv4_min","zv5_min", "dmi_min", "dai_min")
acel_vars_min <- c("dcel4_min", "dcel3_min", "dcel2_min", "dcel1_min", "acel1_min", "acel2_min", "acel3_min", "acel4_min")
hr_vars_min <- c("fc1_min", "fc2_min", "fc3_min", "fc4_min","fc5_min", "trimp_min")

df <- df %>%
  drop_na(classificacao )%>%
  mutate(across(all_of(distance_vars_min), as.numeric)) %>%
  mutate(across(all_of(acel_vars_min), as.numeric)) %>%
  mutate(across(all_of(hr_vars_min), as.numeric)) %>%
  mutate(across(all_of(absolut_vars), as.numeric)
  )

df <- df %>%
  mutate(interv_dias = as.integer(interv_dias)) %>%
  drop_na(interv_dias)
df$classificacao <- as.integer(df$classificacao)
df$mando <- as.factor(df$mando)
df$posicao <- as.factor(df$posicao)
df$npf <- as.factor(df$npf)
df$temp <- as.factor(df$temp)
df$dif <- as.integer(df$dif)
df$idade <- as.integer(df$idade)
df$nome_sessao <- as.factor(df$nome_sessao)
df$tempo <- as.numeric(df$tempo)

df <- df %>%
  filter(tempo > 1) %>%
  filter(temp == "22/23" | temp == "23/24" | temp == "24/25")%>%
  drop_na(dmi_min, mando, posicao, interv_dias, classificacao, quarto, idade, dif, tempo, npf, nome_sessao)
nrow(df)

#MODEL 1
mixed_model1 <- lmer(
  dmi_min ~ mando + posicao + interv_dias + classificacao + quarto + idade + dif + (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)
summary(mixed_model1)

r2_mixed_model1 <- r2(mixed_model1)
print(r2_mixed_model1)
VarCorr(mixed_model1)
performance::check_collinearity(mixed_model1)

#MODEL 2
mixed_model2 <- lmer(
  dmi_min ~ mando + posicao + interv_dias + quarto + classificacao + idade + (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)
summary(mixed_model2)

r2_mixed_model2 <- r2(mixed_model2)
print(r2_mixed_model2)
VarCorr(mixed_model2)
performance::check_collinearity(mixed_model2)

#MODEL 3
mixed_model3 <- lmer(
  dmi_min ~ mando + posicao + interv_dias + quarto + idade + (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)
summary(mixed_model3)

r2_mixed_model3 <- r2(mixed_model3)
print(r2_mixed_model3)
VarCorr(mixed_model3)
performance::check_collinearity(mixed_model3)

#MODEL 4
mixed_model4 <- lmer(
  dmi_min ~ mando + posicao + quarto + idade + (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)
summary(mixed_model4)

r2_mixed_model4 <- r2(mixed_model4)
print(r2_mixed_model4)
VarCorr(mixed_model4)
performance::check_collinearity(mixed_model4)


#MODEL 5
mixed_model5 <- lmer(
  dmi_min ~ posicao + quarto + idade + (1 | npf) + (1 | nome_sessao),
  data = df, REML = FALSE
)
summary(mixed_model5)

r2_mixed_model5 <- r2(mixed_model5)
print(r2_mixed_model5)
VarCorr(mixed_model5)
performance::check_collinearity(mixed_model5)
