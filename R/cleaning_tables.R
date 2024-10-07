source("R/get_parameter.R")


separar_dados <- function(dataframes) {
  dataframes_separados <- lapply(dataframes, function(df) {
    df_clean <- df[-(1:4), ]
    colnames(df_clean) <- as.character(df_clean[1, ])
    df_clean <- df_clean[-1, ]
    df_clean <- df_clean %>%
      mutate(across(everything(), ~ trimws(.))) |>  
      mutate(across(-Data, as.numeric))
    
    return(df_clean)
  })
  
  return(dataframes_separados)
}

dataframes <- separar_dados(dataframes)


