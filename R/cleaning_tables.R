source("R/get_parameter.R")


split_by_parameter <- function(pdf_file_path) {
  raw_text <- pdf_text(pdf_file_path)
  raw_text <- paste(raw_text, collapse = "\n")
  lines <- unlist(str_split(raw_text, "\n"))
  parameter_indices <- which(str_detect(lines, "Parâmetro"))
  dataframes <- list()
  
  for (i in seq_along(parameter_indices)) {
    start_index <- parameter_indices[i]
    end_index <- ifelse(i < length(parameter_indices), parameter_indices[i + 1] - 1, length(lines))
    parameter_table <- lines[start_index:end_index]
    parameter_table_df <- data.table(parameter_table[parameter_table != ""])
    setnames(parameter_table_df, "V1", "Data")
    
    # Separa os dados em 5 colunas
    parameter_table_df[, c("Data", "ValorDiario", "Horario", "Ocorrencias", "QualidadeDoAr")] <- 
      tstrsplit(parameter_table_df$Data, "\\s{2,}", fill = NA)
    
    # Remove a coluna original
    parameter_table_df[, Data := NULL]
    
    dataframes[[i]] <- parameter_table_df
  }
  
  return(dataframes)
}

dataframes <- separar_dados(dataframes)


