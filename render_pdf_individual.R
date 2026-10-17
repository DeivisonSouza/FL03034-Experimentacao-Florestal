# ============================================================
# Converter apenas o HTML atual em PDF
# ============================================================

# Estamos em:
# .../Slides/08-TCM

# Pasta raiz: Slides
# slides_dir <- normalizePath(
#  "..",
#  winslash = "/",
#  mustWork = TRUE
#)

slides_dir <- normalizePath(
  "C:/GitHub/Course/FL03034-Experimentacao-Florestal/Slides",
  winslash = "/",
  mustWork = TRUE
)

# Arquivo HTML
# html_rel <- "08-TCM/08-Teste-Comparacao-Medias.html"
html_rel <- "05-DBC/05-DBC.html"

html_abs <- normalizePath(
  file.path(slides_dir, html_rel),
  winslash = "/",
  mustWork = TRUE
)

# PDF de saída
pdf <- sub(
  "\\.html$",
  ".pdf",
  html_abs,
  ignore.case = TRUE
)

# ============================================================
# Servidor
# ============================================================

server <- callr::r_bg(
  function(dir) {
    servr::httd(
      dir = dir,
      port = 4322,
      browser = FALSE,
      daemon = FALSE
    )
  },
  args = list(slides_dir)
)

Sys.sleep(5)

# Confirma se o servidor continua ativo
server$is_alive()

# URL
html_url <- paste0(
  "http://127.0.0.1:4322/",
  html_rel
)

cat("\nAbrindo:\n", html_url, "\n")

# ============================================================
# Converter para PDF
# ============================================================

pagedown::chrome_print(
  input = html_url,
  output = pdf,
  wait = 10,
  timeout = 600
)

# ============================================================
# Encerrar servidor
# ============================================================

server$kill()

cat(
  "\nPDF criado em:\n",
  pdf,
  "\n"
)
