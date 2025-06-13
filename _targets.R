library(targets)

tar_option_set(packages = c("dplyr", "stringr", "data.table" ,"pdftools", "purrr", "glue", "ggplot2", "lubridate"))


source("R/extract_pdf_text.R")
source("R/process_air_quality_parameters.R")
source("R/bind_data.R")


list(
  
  # Dados de São José dos Campos
  tar_target(sao_jose_raw, map(dir("files/sao_jose_dos_campos/", pattern = "\\.pdf$", full.names = TRUE), read_pdf)),
  tar_target(sao_jose_list, lapply(sao_jose_raw, process_parameters)),
  tar_target(sao_jose, bind_ind(sao_jose_list))
  
  
  # Dados de Jacareí
  
)