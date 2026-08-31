# contextual-factors-project

Análise de fatores contextuais sobre a carga física de atletas de basquete profissional — dissertação de mestrado, dados de GPS/frequência cardíaca (Polar) do time Minas Tênis Clube no NBB, temporadas 2022/23 a 2025/26.

O objetivo é modelar como variáveis de contexto (posição, tempo de descanso, mando de quadra, força do adversário, diferença de placar, tempo em quadra) se associam a diferentes métricas de carga física — distância percorrida, aceleração/desaceleração por zona de intensidade, e TRIMP (training impulse) — usando modelos mistos, com dados no nível de atleta × jogo × quarto.

## Estrutura do repositório

```
.
├── dados/                    # bases brutas e tratadas
│   ├── dataset-bruto/        # CSVs por temporada + base geral consolidada
│   └── dataset-tratado/      # bases já processadas, prontas para análise
├── tratamento-de-dados/      # notebooks de ETL (limpeza, tipagem, derivação de variáveis)
├── analise-de-dados/         # notebooks de análise exploratória e gráficos da dissertação
├── modelos/
│   ├── modelagem-artigo/     # pipeline de modelagem ATUAL, usada para o artigo (ver abaixo)
│   │   └── modelos-dependente-min/  # variante com DVs em taxa (por minuto), sem reconstrução ao valor bruto
│   ├── Resultados-artigo/    # tabelas e gráficos exportados da pipeline atual (ver abaixo)
│   ├── Modelos completos/            # pipeline mais antiga (nomes de coluna em PT, sem tratamento de contagem/offset) — mantida como referência
│   └── Modelos resumidos (versão dissertação)/  # versão condensada da pipeline antiga, agrupada por família de variável
└── escrita/                  # documentos de texto da dissertação (objetivos, tabelas, resultados)
```

> `Modelos completos/` e `Modelos resumidos (versão dissertação)/` usam nomes de coluna diferentes (`posicao`, `idade`, `npf`, `nome_sessao` etc.) e tratam toda variável de aceleração/desaceleração como contínua — não passaram pela revisão de distribuição (Poisson/binomial negativa) feita para o artigo. Não documentei o conteúdo em detalhe aqui porque não foram trabalhadas nesta sessão; me avise se quiser que eu descreva essa pipeline também.

## Dados

Granularidade: uma linha por **atleta × jogo × quarto** (1º a 4º). Cada linha traz, entre outras:

- Distância por zona de velocidade (`zv1`-`zv5`) e variáveis agregadas (`lid`, `mid`, `mhid`, `hid`, `tot_dist`)
- Aceleração/desaceleração por zona de intensidade (`miac`, `mhiac`, `hiac`, `tot_ac`; `midc`, `mhidc`, `hidc`, `tot_dc`)
- Frequência cardíaca por zona (`fc1`-`fc5`) e `trimp`
- Covariáveis de contexto: `position`, `interval` (dias de descanso), `playng_venue` (mando), `opponent_level`, `score_dif`, `age`, `playing_time`

Importante: `tot_dist`, `mid`, `midc`, `miac`, `hiac`, `hidc`, `tot_dc` (e as demais variáveis derivadas por minuto) já vêm calculadas como **taxa** (`valor bruto / tempo em quadra`) no pipeline de tratamento — ver seção de modelagem sobre como isso é tratado.

**Coluna sem script de modelagem**: `tot_ac` (aceleração total, todas as intensidades) existe na base mas não tem um `modelo_tot_ac.R` correspondente em `modelagem-artigo/` — hoje só `tot_dc` (desaceleração total) está modelado no par "total". Vale conferir se o artigo precisa desse modelo também.

## Modelagem (`modelos/modelagem-artigo/`)

Um script R por variável dependente. Todas as 8 variáveis usam o valor **bruto** (reconstruído a partir da taxa × `playing_time`), nunca a taxa diretamente — ver "Estrutura do modelo" abaixo.

### Modelos contínuos (LMM via `lme4::lmer`)

| Script | Variável dependente |
|---|---|
| `modelo_trimp.R` | TRIMP |
| `modelo-tot-dist.R` | Distância total |
| `modelo_mid.R` | Distância moderada-intensa |
| `modelo_midc.R` | Desaceleração moderada-intensa (distância) |

### Modelos de contagem (GLMM via `glmmTMB`, família binomial negativa)

| Script | Variável dependente |
|---|---|
| `modelo_miac.R` | Nº de acelerações moderada-intensas |
| `modelo_hiac.R` | Nº de acelerações de alta intensidade |
| `modelo_hidc.R` | Nº de desacelerações de alta intensidade |
| `modelo_tot_dc.R` | Nº de desacelerações totais |

`miac`, `hiac`, `hidc`, `tot_dc` são **contagens** (número de eventos por quarto), não variáveis contínuas — por isso usam GLMM com `offset(log(playing_time))` no lugar de `playing_time` como covariável, e distribuição binomial negativa em vez de gaussiana (ver "Distribuição e zero-inflação" abaixo).

Cada script ajusta 4 modelos aninhados (adicionando efeitos fixos progressivamente) e reporta o modelo 4 (mais completo) em tabela (`sjPlot::tab_model`) e gráficos (efeitos fixos, efeitos aleatórios por atleta, diagnóstico de pressupostos; os 4 modelos contínuos também têm gráfico de trajetória individual por quarto, que depende do slope aleatório — ver abaixo).

### Estrutura do modelo

**Contínuos** (`trimp`, `tot_dist`, `mid`, `midc`):

```r
DV_bruto ~ position + interval_c + quarter + age_c + playing_time_c +
           playng_venue + opponent_level_c + score_dif_c +
           (1 + quarter_lin | player) + (1 | match/qt_match)
```

**Contagem** (`miac`, `hiac`, `hidc`, `tot_dc`):

```r
DV_count ~ position + interval_c + quarter + age_c +
           playng_venue + opponent_level_c + score_dif_c +
           offset(log(playing_time)) +
           (1 | player) + (1 | match/qt_match)
family = nbinom2(link = "log")
```

- **Efeitos aleatórios cruzados**: `player` e `match` — um atleta passa por vários jogos, um jogo tem vários atletas.
- **Efeito aleatório aninhado**: `qt_match` (identificador único jogo×quarto) dentro de `match` — captura variação específica de cada quarto daquele jogo, além do que `match` já explica. Idêntico nos dois grupos de modelo.
- **Slope aleatório de quarto por atleta** (`quarter_lin`, versão numérica 1-4 de `quarter`, usada só dentro do termo aleatório) — presente **apenas nos 4 modelos contínuos**. Permite que cada atleta tenha sua própria inclinação de carga entre os quartos, além do intercepto próprio. Nos 4 modelos de contagem o slope foi testado (no `miac`) e **removido**: gerava correlação intercepto-slope no limite do espaço paramétrico (-0.888, ajuste singular), apesar do teste de razão de verossimilhança favorecer sua inclusão (p=5.1e-6) — decisão de estabilidade numérica sobre o modelo maximal, aplicada por analogia aos outros 3 modelos de contagem.
- **Variáveis contínuas centralizadas** (`interval_c`, `age_c`, `opponent_level_c`, `score_dif_c`, e `playing_time_c` nos modelos contínuos): o intercepto representa a previsão para um atleta nas condições médias da amostra. `playing_time` **não** é centralizado nos modelos de contagem — entra em escala natural dentro do `offset(log(...))`.
- **Variáveis reconstruídas em valor bruto** a partir da taxa (`valor_bruto = taxa × playing_time`): nos contínuos, `playing_time` entra como efeito fixo; nos de contagem, a contagem reconstruída é arredondada (`round()`) e `playing_time` entra como offset. Em ambos os casos evita a instabilidade estatística de modelar direto a taxa (razão) ou dividir por tempo em quadra muito curto.

### Distribuição e zero-inflação (modelos de contagem)

Para cada uma das 4 variáveis de contagem, testamos Poisson vs. binomial negativa comparando AIC e o teste de superdispersão (`performance::check_overdispersion`) num modelo com todos os efeitos fixos do modelo 4. Em todos os 4 casos a binomial negativa venceu.

- **`hiac`, `hidc`, `tot_dc`**: apresentavam zero-inflação aparente. Investigamos os casos com contagem = 0 e constatamos que o tempo em quadra dessas linhas era substancial (mediana de ~6-8 min) — implausível zerar completamente um quarto inteiro de ações de alta intensidade. Tratamos como dado inválido (não como zero genuíno): **as linhas com contagem = 0 são excluídas** antes de ajustar os modelos, e a binomial negativa comum (sem componente de inflação de zero) já resolve a dispersão remanescente.
- **`miac`**: **não** passou por essa exclusão — o `check_zeroinflation` do modelo 4 aponta zero-inflação estatisticamente significativa (12 zeros observados vs. 0 previstos, p<0.001), mas ainda não investigamos se são zeros genuínos ou o mesmo tipo de artefato encontrado nas outras 3 variáveis. **Pendência**: decidir se aplica a mesma exclusão.

### Requisitos

```r
install.packages(c("lme4", "glmmTMB", "performance", "sjPlot", "broom.mixed", "ggplot2"))
```

### Exportando tabelas e gráficos (`Resultados-artigo/`)

`modelos/modelagem-artigo/gerar_resultados.R` roda os 8 scripts em sequência e exporta os resultados do modelo 4 de cada um para `modelos/Resultados-artigo/`:

```
Resultados-artigo/
├── tabela_{variável}.html      # tabela sjPlot::tab_model dos 4 modelos aninhados
├── graficos/
│   ├── {variável}_efeitos_fixos.png   # forest plot (todas as 8)
│   ├── {variável}_trajetorias.png     # trajetória individual por quarto (só as 4 contínuas, que têm slope)
│   └── {variável}_caterpillar.png     # efeitos aleatórios por atleta (todas as 8)
└── pressupostos/
    └── {variável}_check_model.png     # diagnóstico completo (performance::check_model)
```

```r
# a partir de modelos/modelagem-artigo/
Rscript gerar_resultados.R
```

Rodar um script individual normalmente (fora deste driver) não muda em nada — a exportação só acontece quando a variável `EXPORT_DIR` existe no ambiente, o que só ocorre dentro do `gerar_resultados.R`.

### Limitações conhecidas

**Modelos contínuos:**
- Resíduos com heterocedasticidade e não-normalidade detectadas em todos os modelos (mais acentuado em `tot_dist`); testes com famílias alternativas (Gamma via `glmer`/`glmmTMB`, log-transformação, regressão robusta) não resolveram de forma conclusiva. O slope aleatório não altera a forma dos resíduos — só redistribui variância entre os efeitos aleatórios.
- Autocorrelação residual entre quartos consecutivos do mesmo atleta no mesmo jogo (não modelada pelo `lme4`).

**Modelos de contagem:**
- `miac` ainda não teve a zero-inflação investigada/tratada (ver seção acima).
- `tot_dc` apresentou 8 avisos de não-convergência (`max|grad|` entre 0.003-0.01, pouco acima da tolerância) durante o diagnóstico `check_model()`; `check_singularity` deu `FALSE` e a dispersão do modelo final bateu com execuções anteriores, então não parece indicar um ajuste não confiável, mas vale reavaliar se a estrutura for alterada.
- `check_outliers()` (parte do `check_model()`) ainda não tem suporte a objetos `glmmTMB` no pacote `performance` — esse painel fica ausente no diagnóstico dos 4 modelos de contagem.

**Geral:**
- `score_dif` entra como variável única (sem decomposição entre-jogo/dentro-do-jogo) — o coeficiente reflete uma mistura dos dois efeitos.
- Falta um `modelo_tot_ac.R` para a coluna `tot_ac` (aceleração total) — hoje só o par de desaceleração (`tot_dc`) tem modelo "total" equivalente.
