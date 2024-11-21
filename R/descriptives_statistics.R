source("R/process_air_quality_parameters.R")


descriptive_stats <- function(dfs) {
  stats_list <- lapply(dfs, function(df) {
    if (!is.data.table(df)) {
      df <- as.data.table(df)
    }
    
    numeric_cols <- names(df)[sapply(df, is.numeric)]

    stats <- df[, lapply(.SD, function(col) {
      c(
        Media = mean(col, na.rm = TRUE),
        Mediana = median(col, na.rm = TRUE),
        DesvioPadrao = sd(col, na.rm = TRUE),
        Minimo = min(col, na.rm = TRUE),
        Maximo = max(col, na.rm = TRUE),
        Q1 = quantile(col, 0.25, na.rm = TRUE)
      )
    }), .SDcols = numeric_cols]

    result <- data.table(
      Estatistica = rep(c("Media", "Mediana", "DesvioPadrao", "Minimo", "Maximo", "Q1"), times = length(numeric_cols)),
      Parametro = rep(numeric_cols, each = 6),
      Resultados = as.vector(as.matrix(stats))
    )
    
    final_result <- dcast(result, Estatistica ~ Parametro, value.var = "Resultados")
    return(final_result)
  })
  
  return(stats_list) 
}

descriptives_statistics <- descriptive_stats(dfs)