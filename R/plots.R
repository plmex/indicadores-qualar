library(ggplot2)
library(dplyr)
library(showtext)
library(extrafont)
library(RColorBrewer)

plot_monthly_values <- function(data, parameter_name) {
  
  font_add_google("Lato", "lato")
  showtext_auto()
  
  month_names <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                   "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
  
  for (month in 1:12) {
    
    monthly_data <- data |>
      filter(mes == sprintf("%02d", month))
    
    p <- ggplot(monthly_data, aes(x = as.numeric(dia), y = valor_diario)) +
      geom_line(color = "#4A90E2", size = 0.7) +
      geom_point(color = "#4A90E2", size = 2) +
      geom_text(aes(label = valor_diario), vjust = -1.2, size = 10, color = "#333333") +  
      scale_x_continuous(breaks = seq(1, 31, by = 1)) +  
      labs(title = paste("Valores Diários -", month_names[month]),
           x = NULL,
           y = "Valor Diário") +
      theme_minimal(base_family = "lato", base_size = 16) +  
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 25, color = "#333333"),  
        axis.title = element_text(size = 16), 
        axis.text = element_text(size = 22), 
        panel.grid.major = element_line(color = "grey80", size = 0.5, linetype = "dotted"),  
        panel.grid.minor = element_blank(),  
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    file_path <- file.path("plots", parameter_name, paste0("grafico_", month_names[month], ".png"))
    
    ggsave(file_path, plot = p, width = 12, height = 6, dpi = 300)
  }
}