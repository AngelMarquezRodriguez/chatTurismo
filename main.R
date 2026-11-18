library(plumber)

# Recalcular la minería (scraping, grafos, PLN) SOLO en tu PC:
#   en R:  Sys.setenv(RECALCULAR_MINERIA = "1"); source("main.R")
# En la nube (Render) nunca se ejecuta, así no hace falta instalar rvest, igraph, etc.
EN_NUBE <- nzchar(Sys.getenv("RENDER")) || nzchar(Sys.getenv("PORT"))
RECALCULAR_MINERIA <- identical(Sys.getenv("RECALCULAR_MINERIA"), "1") && !EN_NUBE

if (RECALCULAR_MINERIA) {
  source("R_scripts/mod1_crawling_scraping.R", encoding = "UTF-8")
  source("R_scripts/mod2_estructura_enlaces.R", encoding = "UTF-8")
  source("R_scripts/mod3_contenido_pln_clustering.R", encoding = "UTF-8")
  source("R_scripts/mod4_google_trends.R", encoding = "UTF-8")
  source("R_scripts/mod5_mineria_uso_logs.R", encoding = "UTF-8")
}

cat("\n======================================================\n")
cat("  CHATBOT WAYRA ACTIVO EN: http://127.0.0.1:8000\n")
cat("  Abre ahora 'web/index.html' en tu navegador (Chrome o Edge para usar el micrófono)\n")
cat("======================================================\n")

pr <- plumb("R_scripts/mod6_servidor_api.R")
pr <- pr_static(pr, "/", "web")
pr$run(host = "0.0.0.0", port = as.integer(Sys.getenv("PORT", "8000")))
