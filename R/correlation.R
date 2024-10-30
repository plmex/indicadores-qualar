source("R/get_parameter.R", echo = FALSE)

correlacao_parametros <- function(parametro_escolhido) {
  indice_parametro <- grep(parametro_escolhido, names(dfs), ignore.case = TRUE)
  
  if (length(indice_parametro) == 0) {
    cat("Parâmetro não encontrado.\n")
    return()
  }
  
  df_parametro <- dfs[[indice_parametro]]
  correlacoes <- c()
  dias_parametro <- paste(df_parametro$dia, df_parametro$mes, sep = "-")
  
 
  for (i in seq_along(dfs)) {
    if (i == indice_parametro) {
      next
    }
  
    if ("valor_diario" %in% names(dfs[[i]])) {
      df_comparacao <- dfs[[i]]
      dias_comuns <- intersect(dias_parametro, paste(df_comparacao$dia, df_comparacao$mes, sep = "-"))
      
      if (length(dias_comuns) > 0) {
        dados_parametro <- df_parametro[with(df_parametro, dias_parametro %in% dias_comuns), "valor_diario"]
        dados_comparacao <- df_comparacao[with(df_comparacao, paste(dia, mes, sep = "-") %in% dias_comuns), "valor_diario"]
       
        cor_value <- tryCatch({
          cor(dados_parametro, dados_comparacao, use = "complete.obs")
        }, error = function(e) {
          NA
        })
        
        correlacoes <- c(correlacoes, cor_value)
      } else {
        correlacoes <- c(correlacoes, NA)
      }
    }
  }
  names(correlacoes) <- names(dfs)[-indice_parametro] 
  cat(sprintf("Para o parâmetro '%s', as correlações são:\n", names(dfs)[indice_parametro]))
  for (i in seq_along(correlacoes)) {
    cat(sprintf("%s: %f\n", names(correlacoes)[i], correlacoes[i]))}
}
