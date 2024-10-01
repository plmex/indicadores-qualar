library(pdftools)

get_parameter <- function(start_pattern) {
  raw_text <- pdf_text("file/relValoresDiarios.pdf")
  data_vec <- c() 
  capturing <- FALSE
  
  for (page_number in seq_along(raw_text)) {
    page_split <- strsplit(raw_text[page_number], "\n")[[1]]
    for (line in page_split) {
      if (grepl(start_pattern, line)) {
        capturing <- TRUE
      }
      
      if (capturing) {
        data_vec <- c(data_vec, line) 
        if (grepl("31/12/2023", line)) {
          capturing <- FALSE
          break 
        }
      }
    }
    if (!capturing) {
      break
    }
  }
  
  data_vec <- trimws(data_vec)
  data_cleaned <- data_vec[data_vec != ""]
  
  
  header_index <- grep("Data", data_cleaned)
  data_cleaned <- data_cleaned[!grepl("Tipo|Rede|Pág.|Geração", data_cleaned)]
  data_split <- strsplit(data_cleaned, "\\s{2,}") 
  
  # Filtra apenas linhas que têm o número correto de colunas
  valid_rows <- lapply(data_split, function(x) {
    if (length(x) == 5) {
      return(x)
    } else {
      return(NULL)
    }
  })

  valid_rows <- Filter(Negate(is.null), valid_rows)
  
  df <- do.call(rbind, valid_rows)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  colnames(df) <- c("data", "valor_diario", 
                    "horario_de_ocorrencia", 
                    "numero_de_amostras", 
                    "qualidade_do_ar")
  
  df <- df |>
    tidyr::separate(data, into = c("dia", "mes", "ano"), sep = "/", convert = TRUE) |>
    mutate(numero_de_amostras = as.integer(numero_de_amostras),
           valor_diario = as.integer(valor_diario))
  return(df)
}