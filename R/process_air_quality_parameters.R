source("R/extract_pdf_text.R")



process_parameters <- function(lines) {
  parameter_indices <- which(str_detect(lines, "Parâmetro"))
  parameter_names <- vector("character", length(parameter_indices))
  
  # Lista de padrões desnecessários a serem removidos
  unnecessary_patterns <- c("Média Aritmética", "Máxima", "Mínima", "N. dias", 
                            "UGRHI", "Estação", "Unidade de Medida", "PQAR", 
                            "Data Geração", "Sistema de Informações", 
                            "RELATÓRIO DE VALORES DIÁRIOS", "Horário de", 
                            "Número de", "Data", "Valor Diário", "Qualidade do Ar", "Ocorrência", "Amostras")
  
  dataframes <- list()
  
  clean_numeric_column <- function(column) {
    column <- gsub("\\s", "", column)
    column <- gsub(",", ".", column)
    suppressWarnings(as.numeric(column))
  }
  
  for (i in seq_along(parameter_indices)) {
    start_index <- parameter_indices[i]
    end_index <- ifelse(i < length(parameter_indices), parameter_indices[i + 1] - 1, length(lines))
    
    parameter_table <- lines[(start_index + 1):end_index]
    
    
    parameter_table <- str_trim(parameter_table)  
    parameter_table <- parameter_table[str_detect(parameter_table, "^\\d{2}/\\d{2}/\\d{4}")] 
    
    if (length(parameter_table) == 0) {
      next
    }
    
    parameter_table_df <- data.table(parameter_table)
    colnames(parameter_table_df) <- "Data"
    
    split_data <- tstrsplit(parameter_table_df$Data, "\\s{2,}", fill = NA)
    

    if (length(split_data) >= 5) {
      parameter_table_df[, c("data", "valor_diario", "horario_ocorrencia", "num_amostras", "qualidade_ar") := split_data[1:5]]
    } else {
      parameter_table_df[, c("data", "valor_diario", "horario_ocorrencia", "num_amostras", "qualidade_ar") := list(NA, NA, NA, NA, NA)]
    }
    

    parameter_table_df[, c("dia", "mes", "ano") := {
      if (!all(is.na(data)) && length(data) > 0) tstrsplit(data, "/", fixed = TRUE) else list(NA, NA, NA)
    }]
    
    parameter_table_df[, valor_diario := clean_numeric_column(valor_diario)]
    parameter_table_df[, num_amostras := clean_numeric_column(num_amostras)]
    
    # Remove as colunas "Data" e "data"
    parameter_table_df[, c("Data", "data") := NULL]
    
    setcolorder(parameter_table_df, c("dia", "mes", "ano", "valor_diario", "horario_ocorrencia", "num_amostras", "qualidade_ar"))
    
    parameter_name <- str_extract(lines[start_index], "(?<=Parâmetro: ).*")
    parameter_names[i] <- str_trim(parameter_name)
    dataframes[[parameter_names[i]]] <- parameter_table_df
  }
  
  return(dataframes)
}


dfs <- process_parameters(lines)
