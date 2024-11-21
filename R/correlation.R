source("R/process_air_quality_parameters.R")

correlation_test <- function(dfs) {
  results_list <- lapply(seq_along(dfs), function(index) {
    df <- dfs[[index]]

    df[, data := as.Date(paste(ano, mes, dia, sep = "-"))]

    results <- data.frame(Parametro = character(),
                          Correlacao = numeric(),
                          P_Valor = numeric(),
                          stringsAsFactors = FALSE)
    
    # Calcular a correlação do valor_diario com outros dataframes
    for (j in seq_along(dfs)) {
      if (j != index) {
        other_df <- dfs[[j]]
        other_df[, data := as.Date(paste(ano, mes, dia, sep = "-"))]
        combined_data <- merge(df[, .(data, valor_diario)], 
                               other_df[, .(data, valor_diario)], 
                               by = "data", 
                               suffixes = c("_base", "_other"))
        
        if (nrow(combined_data) > 1) {
          cor_test <- cor.test(combined_data$valor_diario_base, combined_data$valor_diario_other, method = "pearson")
          results <- rbind(results, data.frame(
            Parametro = names(dfs)[j],
            Correlacao = cor_test$estimate,
            P_Valor = cor_test$p.value,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    results$Parametro_Base <- names(dfs)[index]
    return(results)
  })
  names(results_list) <- names(dfs)
  return(results_list)
}


cor_test_results <- correlation_test(dfs)