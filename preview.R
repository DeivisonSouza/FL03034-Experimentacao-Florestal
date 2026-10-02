# ============================================================
# Visualização dinâmica das aulas em xaringan
# Projeto: Experimentação Florestal
# ============================================================

preview_aula <- function(aula = NULL) {

  # Verifica se a pasta Slides existe
  if (!dir.exists("Slides")) {
    stop(
      "A pasta 'Slides' não foi encontrada.\n",
      "Execute este script a partir da raiz do projeto."
    )
  }

  # Localiza todos os arquivos R Markdown dentro de Slides
  arquivos <- list.files(
    path = "Slides",
    pattern = "\\.Rmd$",
    recursive = TRUE,
    full.names = TRUE
  )

  # Remove arquivos que não interessam, se houver
  arquivos <- arquivos[
    !grepl("README\\.Rmd$", arquivos, ignore.case = TRUE)
  ]

  if (length(arquivos) == 0) {
    stop("Nenhum arquivo .Rmd foi encontrado dentro de 'Slides/'.")
  }

  # Ordena alfabeticamente
  arquivos <- sort(arquivos)

  # ----------------------------------------------------------
  # Se nenhum nome for informado, abre menu interativo
  # ----------------------------------------------------------

  if (is.null(aula)) {

    nomes <- sub(
      "\\.Rmd$",
      "",
      basename(arquivos)
    )

    pastas <- dirname(
      sub("^Slides/", "", arquivos)
    )

    opcoes <- paste0(
      nomes,
      "  [",
      pastas,
      "]"
    )

    escolha <- menu(
      opcoes,
      title = "Selecione a aula que deseja visualizar:"
    )

    if (escolha == 0) {
      message("Visualização cancelada.")
      return(invisible(NULL))
    }

    arquivo <- arquivos[escolha]

  } else {

    # --------------------------------------------------------
    # Permite procurar a aula pelo nome
    # Ex.: preview_aula("AED")
    # --------------------------------------------------------

    encontrados <- arquivos[
      grepl(
        aula,
        arquivos,
        ignore.case = TRUE,
        fixed = TRUE
      )
    ]

    if (length(encontrados) == 0) {
      stop(
        "Nenhuma aula encontrada contendo: '",
        aula,
        "'."
      )
    }

    if (length(encontrados) > 1) {

      opcoes <- sub("^Slides/", "", encontrados)

      escolha <- menu(
        opcoes,
        title = paste0(
          "Foram encontradas várias aulas contendo '",
          aula,
          "':"
        )
      )

      if (escolha == 0) {
        message("Visualização cancelada.")
        return(invisible(NULL))
      }

      arquivo <- encontrados[escolha]

    } else {

      arquivo <- encontrados
    }
  }

  message("")
  message("Abrindo apresentação:")
  message("  ", arquivo)
  message("")

  # Inicia o Infinite Moon Reader
  xaringan::inf_mr(
    arquivo,
    cast_from = "Slides"
  )
}


# Ao executar source("preview.R"), abre automaticamente o menu
if (interactive()) {
  preview_aula()
}
