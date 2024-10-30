source("R/get_parameter.R")

correlacao_parametros <- function() {
  # Verifica se o parâmetro foi escolhido
  if (is.null(parametro_escolhido_global)) {
    cat("Nenhum parâmetro foi escolhido.\n")
    return()
  }
  
  # Acessa os dados e faz a correlação
  cat("Parâmetro escolhido para correlação:", parametro_escolhido_global, "\n")
  
  # Padrão para localizar o parâmetro escolhido
  padrao_nome <- paste0("\\b", parametro_escolhido_global, "\\b")
  
  # Inicializa uma lista para armazenar as correlações
  todas_correlacoes <- list()
  
  for (i in seq_along(dfs)) {
    # Verifica se o parâmetro está presente no dataframe
    indice_parametro <- grep(padrao_nome, names(dfs[[i]]), ignore.case = TRUE)
    
    if (length(indice_parametro) == 1) {
      # Calcula correlações com o parâmetro escolhido
      correlacoes <- cor(dfs[[i]], use = "complete.obs")  # Calcula correlações para o dataframe
      todas_correlacoes[[names(dfs)[i]]] <- correlacoes[indice_parametro, ]  # Armazena apenas a linha correspondente ao parâmetro escolhido
    } else {
      cat("Parâmetro", parametro_escolhido_global, "não encontrado no dataframe", names(dfs)[i], ".\n")
    }
  }
  
  # Exibe as correlações
  for (df_name in names(todas_correlacoes)) {
    cat("\nCorrelações no dataframe:", df_name, "\n")
    print(todas_correlacoes[[df_name]])
  }
}

correlacao_parametros()