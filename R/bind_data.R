

bind_ind <- function(lista_anos) {
  
  todos_indicadores <- unique(unlist(map(lista_anos, names)))
  
  resultado <- map(
    todos_indicadores,
    function(ind) {
      dfs <- map(lista_anos, ~ .x[[ind]])  
      dfs <- compact(dfs)  
      reduce(dfs, dplyr::bind_rows)            
    }
  )
  
  names(resultado) <- todos_indicadores
  return(resultado)
  
}