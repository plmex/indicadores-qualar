source("R/extract_pdf_text.R")

library(data.table)

process_parameters <- function(lines) {
  parameter_indices <- which(str_detect(lines, "Parâmetro"))
  parameter_names <- vector("character", length(parameter_indices)) # Para armazenar os nomes dos parâmetros
  
  
  # Lista de padrões desnecessários a serem removidos
  unnecessary_patterns <- c("Média Aritmética", "Máxima", "Mínima", "N. dias", 
                            "UGRHI", "Estação", "Unidade de Medida", "PQAR", 
                            "Data Geração", "Sistema de Informações", 
                            "RELATÓRIO DE VALORES DIÁRIOS", "Horário de", 
                            "Número de", "Data", "Valor Diário", "Qualidade do Ar", "Ocorrência", "Amostras")
  
  dataframes <- list()
  
  for (i in seq_along(parameter_indices)) {
    start_index <- parameter_indices[i]
    end_index <- ifelse(i < length(parameter_indices), parameter_indices[i + 1] - 1, length(lines))
    
    parameter_table <- lines[(start_index + 1):end_index]
    
    # # Remove linhas desnecessárias e vazias
    # parameter_table <- parameter_table[!str_detect(parameter_table, paste(unnecessary_patterns, collapse = "|"))]
    # parameter_table <- parameter_table[parameter_table != ""]

    parameter_table <- parameter_table[str_detect(parameter_table, "^\\d{2}/\\d{2}/\\d{4}")]
    parameter_name <- str_extract(lines[start_index], "(?<=Parâmetro: ).*")
    parameter_names[i] <- str_trim(parameter_name)  
  
    parameter_table_df <- data.table(parameter_table)
    colnames(parameter_table_df) <- "Data"

    split_data <- tstrsplit(parameter_table_df$Data, "\\s{2,}", fill = NA)
    
    # Verifica se há pelo menos 5 colunas e atribui
    if (length(split_data) >= 5) {
      parameter_table_df[, c("data", "valor_diario", "horario_ocorrencia", "num_amostras", "qualidade_ar") := split_data[1:5]]
    } else {
      parameter_table_df[, c("data", "valor_diario", "horario_ocorrencia", "num_amostras", "qualidade_ar") := list(NA, NA, NA, NA, NA)]
    }
    
    # Separa a coluna data em dia, mês e ano
    parameter_table_df[, c("dia", "mes", "ano") := tstrsplit(data, "/", fixed = TRUE)]

    parameter_table_df[, valor_diario := as.numeric(valor_diario)]
    parameter_table_df[, num_amostras := as.numeric(num_amostras)]
    
    # Remove a coluna original de data
    if ("data" %in% colnames(parameter_table_df)) {
      parameter_table_df[, data := NULL]
    }
    if ("Data" %in% colnames(parameter_table_df)) {
      parameter_table_df[, Data := NULL]
    }
    
    setcolorder(parameter_table_df, c("dia", "mes", "ano", "valor_diario", "horario_ocorrencia", "num_amostras", "qualidade_ar"))
    
    # Adiciona o dataframe processado à lista
    dataframes[[parameter_names[i]]] <- parameter_table_df
  }
  
  return(dataframes)
}


dfs <- process_parameters(lines)
