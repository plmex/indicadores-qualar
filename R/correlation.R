library(data.table)
library(corrplot)
library(Hmisc)

source("R/process_air_quality_parameters.R")
source("R/plots.R")


calcular_correlacao <- function(df1, df2, var) {
  dados_comum <- merge(df1, df2, by = "data", suffixes = c("_df1", "_df2"))
  dados_comum <- dados_comum[complete.cases(dados_comum[[paste0(var, "_df1")]], dados_comum[[paste0(var, "_df2")]]), ]
  
  if (identical(df1, df2)) {
    return(tibble(correlacao = 1, p_value = NA))
  }
  
  resultado <- cor.test(dados_comum[[paste0(var, "_df1")]], dados_comum[[paste0(var, "_df2")]], method = "pearson")
  return(tibble(correlacao = round(resultado$estimate, 3), p_value = round(resultado$p.value, 3)))
}

limpar_nome_parametro <- function(nome) {
  nome_sem_numeros <- gsub("^\\d+ - ", "", nome)
  nome_sem_parênteses <- gsub(" \\(.*\\)", "", nome_sem_numeros)
  return(nome_sem_parênteses)
}

calcular_todas_correlacoes <- function(dfs) {
  variaveis <- c("valor_diario")
  resultados <- tibble(correlacao = numeric(), p_value = numeric(), parametro = character(), parametro_relacionado = character())
  
  for (i in 1:length(dfs)) {
    for (j in 1:length(dfs)) {
      for (var in variaveis) {
        resultado <- calcular_correlacao(dfs[[i]], dfs[[j]], var)
        resultados <- resultados %>%
          bind_rows(tibble(correlacao = resultado$correlacao, 
                           p_value = resultado$p_value, 
                           parametro = limpar_nome_parametro(names(dfs)[i]), 
                           parametro_relacionado = limpar_nome_parametro(names(dfs)[j])))
      }
    }
  }
  
  return(resultados)
}

dfs <- process_parameters(lines)
resultados_correlacao <- calcular_todas_correlacoes(dfs)
plot_correlograma(resultados_correlacao)



