# contextual-factors-project

Análise de fatores contextuais sobre a carga física de atletas de basquete profissional — dissertação de mestrado, dados de GPS/frequência cardíaca (Polar) do time Minas Tênis Clube no NBB, temporadas 2022/23 a 2025/26.

O objetivo é modelar como variáveis de contexto (posição, tempo de descanso, mando de quadra, força do adversário, diferença de placar, tempo em quadra) se associam a diferentes métricas de carga física — distância percorrida, aceleração/desaceleração por zona de intensidade, e TRIMP (training impulse) — usando modelos lineares mistos (LMM), com dados no nível de atleta × jogo × quarto.

## Estrutura do repositório

```
.
├── dados/                    # bases brutas e tratadas
│   ├── dataset-bruto/        # CSVs por temporada + base geral consolidada
│   └── dataset-tratado/      # bases já processadas, prontas para análise
├── tratamento-de-dados/      # notebooks de ETL (limpeza, tipagem, derivação de variáveis)
├── analise-de-dados/         # notebooks de análise exploratória e gráficos da dissertação
├── modelos/                  # scripts de modelagem estatística
│   └── modelagem-artigo/     # pipeline de modelagem atual (ver abaixo)
└── escrita/                  # documentos de texto da dissertação (objetivos, tabelas, resultados)
```

## Dados

Granularidade: uma linha por **atleta × jogo × quarto** (1º a 4º). Cada linha traz, entre outras:

- Distância por zona de velocidade (`zv1`-`zv5`) e variáveis agregadas (`lid`, `mid`, `mhid`, `hid`, `tot_dist`)
- Aceleração/desaceleração por zona de intensidade (`miac`, `mhiac`, `hiac`, `tot_ac`; `midc`, `mhidc`, `hidc`, `tot_dc`)
- Frequência cardíaca por zona (`fc1`-`fc5`) e `trimp`
- Covariáveis de contexto: `position`, `interval` (dias de descanso), `playng_venue` (mando), `opponent_level`, `score_dif`, `age`, `playing_time`

Importante: `tot_dist`, `mid`, `miac`, `midc` (e as demais variáveis derivadas por minuto) já vêm calculadas como **taxa** (`valor bruto / tempo em quadra`) no pipeline de tratamento — ver seção de modelagem sobre como isso é tratado.

## Modelagem (`modelos/modelagem-artigo/`)

Um script R por variável dependente, todos seguindo a mesma estrutura:

| Script | Variável dependente |
|---|---|
| `modelo_trimp.R` | TRIMP |
| `modelo-tot-dist.R` | Distância total |
| `modelo_mid.R` | Distância moderada-intensa |
| `modelo_miac.R` | Aceleração moderada-intensa |
| `modelo_midc.R` | Desaceleração moderada-intensa |

Cada script ajusta 4 modelos mistos aninhados (adicionando efeitos fixos progressivamente) e reporta o modelo 4 (mais completo) em tabela (`sjPlot::tab_model`) e gráficos (efeitos fixos, trajetória individual por quarto, efeitos aleatórios por atleta, diagnóstico de pressupostos).

### Estrutura do modelo

```r
DV ~ position + interval_c + quarter + age_c + playing_time_c +
     playng_venue + opponent_level_c + score_dif_c +
     (1 | player) + (1 | match/qt_match)
```

- **Efeitos aleatórios cruzados**: `player` e `match` — um atleta passa por vários jogos, um jogo tem vários atletas.
- **Efeito aleatório aninhado**: `qt_match` (identificador único jogo×quarto) dentro de `match` — captura variação específica de cada quarto daquele jogo, além do que `match` já explica.
- **Sem slope aleatório**: o modelo assume que a tendência entre quartos é igual para todos os atletas (decisão de padronização entre os 5 scripts — testes de razão de verossimilhança indicaram slope estatisticamente justificado para `trimp`, `miac` e `midc`, mas não para `tot_dist`/`mid`).
- **Variáveis contínuas centralizadas** (`interval_c`, `age_c`, `playing_time_c`, `opponent_level_c`, `score_dif_c`): o intercepto representa a previsão para um atleta nas condições médias da amostra, não em valores como idade=0 ou tempo=0.
- **Variáveis derivadas por-minuto reconstruídas em valor bruto** (`dist_total`, `mid_bruto`, `miac_bruto`, `midc_bruto` = taxa × `playing_time`), com `playing_time` entrando como efeito fixo em vez de divisor — evita a instabilidade estatística de dividir por tempo em quadra muito curto.

### Requisitos

```r
install.packages(c("lme4", "performance", "sjPlot", "broom.mixed", "ggplot2"))
```

### Limitações conhecidas

- Resíduos com heterocedasticidade e não-normalidade detectadas em todos os modelos (mais acentuado em `dist_total`); testes com famílias alternativas (Gamma via `glmer`/`glmmTMB`, log-transformação, regressão robusta) não resolveram de forma conclusiva.
- Autocorrelação residual entre quartos consecutivos do mesmo atleta no mesmo jogo (não modelada pelo `lme4`).
- `score_dif` entra como variável única (sem decomposição entre-jogo/dentro-do-jogo) — o coeficiente reflete uma mistura dos dois efeitos.
