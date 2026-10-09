# ============================================================
# Converter todos os HTML existentes em PDF
# Sem renderizar nenhum arquivo Rmd
# ============================================================

# Instalar uma vez, se necessário:
# install.packages(c("pagedown", "servr", "callr"))


# ============================================================
# 1. Pasta raiz dos slides
# ============================================================

slides_dir <- normalizePath(
  "Slides",
  winslash = "/",
  mustWork = TRUE
)


# ============================================================
# 2. Encontrar todos os arquivos HTML
# ============================================================

html_files <- list.files(
  path = slides_dir,
  pattern = "\\.html$",
  recursive = TRUE,
  full.names = TRUE
)

# Ignorar HTML de dependências/pastas auxiliares
html_files <- html_files[
  !grepl(
    "_files|_cache|/libs/|/assets/",
    gsub("\\\\", "/", html_files)
  )
]

cat("\nHTML encontrados:\n")
print(html_files)


# ============================================================
# 3. Iniciar servidor usando Slides como raiz
# ============================================================

server <- callr::r_bg(
  function(dir) {
    servr::httd(
      dir = dir,
      port = 4321,
      browser = FALSE,
      daemon = FALSE
    )
  },
  args = list(slides_dir)
)

# Aguarda o servidor iniciar
Sys.sleep(3)


# ============================================================
# 4. Converter cada HTML para PDF
# ============================================================

for (html in html_files) {

  # Caminho absoluto normalizado
  html_abs <- normalizePath(
    html,
    winslash = "/",
    mustWork = TRUE
  )

  # Caminho relativo à pasta Slides
  html_rel <- substring(
    html_abs,
    nchar(slides_dir) + 2
  )

  # URL do arquivo no servidor
  html_url <- paste0(
    "http://127.0.0.1:4321/",
    html_rel
  )

  # PDF na mesma pasta do HTML
  pdf <- sub(
    "\\.html$",
    ".pdf",
    html_abs,
    ignore.case = TRUE
  )

  cat(
    "\n============================================================\n",
    "Convertendo:\n",
    html_rel,
    "\n->\n",
    pdf,
    "\n============================================================\n"
  )

  tryCatch(

    pagedown::chrome_print(
      input = html_url,
      output = pdf,
      wait = 3,
      timeout = 180
    ),

    error = function(e) {
      message(
        "\nERRO ao converter:\n",
        html_rel,
        "\n",
        e$message
      )
    }
  )
}


# ============================================================
# 5. Encerrar servidor
# ============================================================

server$kill()


cat(
  "\n============================================================\n",
  "CONVERSÃO CONCLUÍDA\n",
  "============================================================\n"
)
