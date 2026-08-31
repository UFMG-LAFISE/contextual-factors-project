#Script driver: roda os 8 modelos (variaveis dependentes BRUTAS) e exporta
#tabelas de resultados + graficos para a pasta Resultados-artigo.
#Nao deve ser fonte de verdade dos modelos - so orquestra a exportacao.
#
#Uso: Rscript gerar_resultados.R  (ou "Source" no RStudio a partir desta pasta)
#
#Cada script de modelagem só exporta arquivo quando a variável EXPORT_DIR
#existe no ambiente em que ele é rodado - por isso rodar um script individual
#normalmente (fora deste driver) continua funcionando exatamente como antes,
#sem gerar nenhum arquivo extra.

#localiza a pasta deste script (funciona com Rscript e com "Source" no RStudio);
#se nenhum dos dois for detectado, assume que o diretório de trabalho já é
#modelos/modelagem-artigo/
BASE <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
  if (length(file_arg) == 1) return(dirname(normalizePath(file_arg)))
  if (!is.null(sys.calls())) {
    ctxt <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
    if (!is.null(ctxt)) return(dirname(normalizePath(ctxt)))
  }
  getwd()
})
EXPORT_DIR <<- file.path(dirname(BASE), "Resultados-artigo")

dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(EXPORT_DIR, "graficos"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(EXPORT_DIR, "pressupostos"), showWarnings = FALSE, recursive = TRUE)

scripts <- c(
  "modelo_trimp.R",
  "modelo_mid.R",
  "modelo_midc.R",
  "modelo-tot-dist.R",
  "modelo_miac.R",
  "modelo_hiac.R",
  "modelo_hidc.R",
  "modelo_tot_dc.R"
)

grDevices::pdf(NULL) #evita que print()/plot() abram janela ou gerem Rplots.pdf

resultados <- list()

for (s in scripts) {
  cat("\n\n#####################################################\n")
  cat("### Rodando:", s, "\n")
  cat("#####################################################\n")
  env <- new.env(parent = globalenv())
  assign("EXPORT_DIR", EXPORT_DIR, envir = env)
  ok <- tryCatch({
    source(file.path(BASE, s), local = env, print.eval = TRUE)
    TRUE
  }, error = function(e) {
    cat("ERRO em", s, ":", conditionMessage(e), "\n")
    FALSE
  })
  resultados[[s]] <- ok
}

grDevices::dev.off()

cat("\n\n=== RESUMO ===\n")
print(unlist(resultados))
