library(tidyverse)
library(pdftools)

read_pdf <- function(pdf_file_path) {
 
  raw_text <- pdf_text(pdf_file_path)
  raw_text <- paste(raw_text, collapse = "\n")
  lines <- unlist(str_split(raw_text, "\n"))
  
  return(lines)
}

lines <- read_pdf("file/relValoresDiarios.pdf")
