source("R/process_air_quality_parameters.R", echo = FALSE)

parametro_escolhido <- NULL

estatisticas_descritivas <- function() {
  
  parametros <- c(
    "MP10 - Partículas inaláveis",
    "NO2 - Dióxido de Nitrogênio",
    "CO - Monóxido de Carbono",
    "NO - Monóxido de Nitrogênio",
    "NOx - Óxidos de Nitrogênio",
    "VV - Velocidade do Vento",
    "TEMP - Temperatura",
    "RADG - Radiação Global",
    "UR - Umidade Relativa",
    "PRESS - Pressão Atmosférica",
    "RADUV - Radiação Ultravioleta",
    "MP2.5 - Partículas Inaláveis Finas",
    "O3 - Ozônio"
  )
  
  cat("Escolha um parâmetro de qualidade do ar para calcular as estatísticas:\n")
  for (i in seq_along(parametros)) {
    cat(i, "-", parametros[i], "\n")
  }
  
  opcao <- as.integer(readline(prompt = "Digite o número correspondente ao parâmetro (ou 0 para SAIR): "))
  
  if (opcao == 0) {
    cat("Obrigado! Encerrando a consulta.\n")
    return() 
  }
  
  if (opcao < 1 || opcao > length(parametros)) {
    cat("Opção inválida! Você pode sair digitando 0.\n")
    return() 
  }
  
  parametro_escolhido <<- unlist(strsplit(parametros[opcao], " - "))[1]
  cat("O parâmetro escolhido foi:", parametro_escolhido, "\n")
  cat("As estatísticas descritivas são:\n")
  Sys.sleep(3)
  
  padrao_nome <- paste0("\\b", parametro_escolhido, "\\b")
  indice_parametro <- grep(padrao_nome, names(dfs), ignore.case = TRUE)
  
  if (length(indice_parametro) == 1) {
    estatisticas <- summary(dfs[[indice_parametro]])
    print(estatisticas)
    return(estatisticas)  # Adicionando o retorno das estatísticas
  } else if (length(indice_parametro) == 0) {
    cat("Parâmetro", parametro_escolhido, "não encontrado na lista `dfs`.\n")
  } else {
    cat("Múltiplos dataframes encontrados para o parâmetro:", parametro_escolhido, "\n")
  }
  
  cat("Deseja trocar de parâmetro?\n")
  cat("1- Para sim\n")
  cat("2- Para não\n")
  continuar <- as.integer(readline(prompt = "Opção: "))
  
  if (continuar != 1) {
    cat("Obrigado!\n")
    return() 
  } else {
    estatisticas_descritivas()  
  }
}
