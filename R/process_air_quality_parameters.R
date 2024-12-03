source("R/extract_pdf_text.R")


process_parameters <- function(lines) {
  parameter_indices <- which(str_detect(lines, "Parâmetro"))
  parameter_names <- vector("character", length(parameter_indices))
  
  dataframes <- list()
  
  clean_numeric_column <- function(column) {
    column <- gsub("\\s", "", column)
    column <- gsub(",", ".", column)
    suppressWarnings(as.numeric(column))
  }
  
  for (i in seq_along(parameter_indices)) {
    start_index <- parameter_indices[i]
    end_index <- if (i < length(parameter_indices)) parameter_indices[i + 1] - 1 else length(lines)
    
    parameter_table <- lines[(start_index + 1):end_index]
    
    parameter_table <- str_trim(parameter_table)  
    parameter_table <- parameter_table[str_detect(parameter_table, "^\\d{2}/\\d{2}/\\d{4}")] 
    
    if (length(parameter_table) == 0) {
      next
    }
    
    parameter_table_df <- data.table(parameter_table)
    colnames(parameter_table_df) <- "Data"
    
    split_data <- tstrsplit(parameter_table_df$Data, "\\s{2,}", fill = NA)
    
    parameter_table_df[, c("data", "valor_diario", "horario_ocorrencia", "num_amostras", "qualidade_ar") := 
                         .(split_data[[1]], split_data[[2]], split_data[[3]], split_data[[4]], split_data[[5]])]
    
    parameter_table_df[, c("dia", "mes", "ano") := tstrsplit(data, "/", fixed = TRUE)]
    
    parameter_table_df[, valor_diario := clean_numeric_column(valor_diario)]
    parameter_table_df[, num_amostras := clean_numeric_column(num_amostras)]
    
    setcolorder(parameter_table_df, c("dia", "mes", "ano", "valor_diario", "horario_ocorrencia", "num_amostras", "qualidade_ar"))
    
    parameter_name <- str_extract(lines[start_index], "(?<=Parâmetro: ).*")
    parameter_names[i] <- str_trim(parameter_name)
    dataframes[[parameter_names[i]]] <- parameter_table_df
  }
  
  return(dataframes)
}
