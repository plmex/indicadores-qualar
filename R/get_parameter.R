library(tidyverse)
library(pdftools)


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
    parameter_table_df <- as.data.frame(parameter_table[parameter_table != ""])
    colnames(parameter_table_df) <- "Data"
    
    # Adiciona o dataframe à lista
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




