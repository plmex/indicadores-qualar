library(targets)

tar_option_set(packages = c("dplyr", "stringr", "data.table" ,"pdftools", "fs" ,"purrr", "glue", "ggplot2", "lubridate"))


source("R/extract_pdf_text.R")
source("R/process_air_quality_parameters.R")
source("R/bind_data.R")


list(
  
  # Dados de São José dos Campos
  tar_target(sao_jose_raw, map(dir("files/sao_jose_dos_campos/", pattern = "\\.pdf$", full.names = TRUE), read_pdf)),
  tar_target(sao_jose_list, lapply(sao_jose_raw, process_parameters)),
  tar_target(sao_jose, bind_ind(sao_jose_list)),
  
  
  # Dados de Jacareí
  tar_target(jacarei_raw, map(dir("files/jacarei/", pattern = "\\.pdf$", full.names = TRUE), read_pdf)),
  tar_target(jacarei_list, lapply(jacarei_raw, process_parameters)),
  tar_target(jacarei, bind_ind(jacarei_list)),
  
  # Dados de Guaratinguetá
  tar_target(guaratingueta_raw, map(dir("files/guaratingueta/", pattern = "\\.pdf$", full.names = TRUE), read_pdf)),
  tar_target(guaratingueta_list, lapply(guaratingueta_raw, process_parameters)),
  tar_target(guaratingueta, bind_ind(guaratingueta_list)),
  
  # Dados de Taubate
  tar_target(taubate_raw, map(dir("files/taubate", pattern = "\\.pdf$", full.names = TRUE), read_pdf)),
  tar_target(taubate_list, lapply(taubate_raw, process_parameters)),
  tar_target(taubate, bind_ind(taubate_list))

)
