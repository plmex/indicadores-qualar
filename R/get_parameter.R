library(tidyverse)
library(pdftools)
library(data.table)

split_by_parameter <- function(pdf_file_path) {
  raw_text <- pdf_text(pdf_file_path)
  raw_text <- paste(raw_text, collapse = "\n")
  lines <- unlist(str_split(raw_text, "\n"))
  parameter_indices <- which(str_detect(lines, "Parâmetro"))
  dataframes <- list()
  
  # Padrões que indicam informações desnecessárias ou cabeçalhos repetidos
  unnecessary_patterns <- c("Média Aritmética", "Máxima", "Mínima", "N. dias", "UGRHI", 
                            "Estação", "Unidade de Medida", "PQAR", "Data Geração", 
                            "Sistema de Informações", "RELATÓRIO DE VALORES DIÁRIOS",
                            "Horário de", "Número de", "Data", "Valor Diário", "Qualidade do Ar", "Ocorrência", "Amostras")
  
  # Função para verificar se a linha contém uma data válida
  is_valid_date <- function(string) {
    !is.na(as.Date(string, format = "%d/%m/%Y"))
  }
  
  for (i in seq_along(parameter_indices)) {
    start_index <- parameter_indices[i]
    end_index <- ifelse(i < length(parameter_indices), parameter_indices[i + 1] - 1, length(lines))
    
    # Extrai a tabela de parâmetros após a linha "Parâmetro"
    parameter_table <- lines[(start_index + 1):end_index]
    parameter_table <- parameter_table[!str_detect(parameter_table, paste(unnecessary_patterns, collapse = "|"))]
    parameter_table <- parameter_table[parameter_table != ""]
    
    # Remover qualquer linha que não tenha uma data válida no início
    parameter_table <- parameter_table[str_detect(parameter_table, "^\\d{2}/\\d{2}/\\d{4}")]
    
    # Converte em data.table para facilitar a manipulação
    parameter_table_df <- data.table(parameter_table)
    colnames(parameter_table_df) <- "Data"
    
    # Separa os dados em 5 colunas com base em múltiplos espaços
    split_data <- tstrsplit(parameter_table_df$Data, "\\s{2,}", fill = NA)
    
    # Verifica se há pelo menos 5 colunas e atribui as colunas corretamente
    if (length(split_data) >= 5) {
      parameter_table_df[, c("data", "valor_diario", "horario_ocorrencia", "num_amostras", "qualidade_ar") := split_data[1:5]]
    } else {
      # Preenche com NA se houver menos colunas
      parameter_table_df[, c("data", "valor_diario", "horario_ocorrencia", "num_amostras", "qualidade_ar") := list(NA, NA, NA, NA, NA)]
    }
    
    # Remove a coluna original que foi dividida
    parameter_table_df[, Data := NULL]
    
    # Adiciona o dataframe formatado à lista
    dataframes[[i]] <- parameter_table_df
  }
  
  return(dataframes)
}


rename_dataframes <- function(parameter_dataframes) {
  parameter_names <- sapply(parameter_dataframes, function(x) str_extract(x[1, 1], "Parâmetro: (.+)"))
  cleaned_dataframes <- setNames(parameter_dataframes, parameter_names)
  
  return(cleaned_dataframes)
}

dataframes <- rename_dataframes(split_by_parameter("file/relValoresDiarios.pdf"))




