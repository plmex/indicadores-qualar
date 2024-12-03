library(ggplot2)
library(showtext)
library(dplyr)
library(data.table)
library(gridExtra)

plot_monthly_values <- function(data, parameter_name) {
  font_add_google("Lato", "lato")
  showtext_auto()
  
  month_names <- c(
    "Janeiro",
    "Fevereiro",
    "Março",
    "Abril",
    "Maio",
    "Junho",
    "Julho",
    "Agosto",
    "Setembro",
    "Outubro",
    "Novembro",
    "Dezembro"
  )
  
  parameter_dir <- file.path("plots", parameter_name)
  if (!dir.exists(parameter_dir)) {
    dir.create(parameter_dir, recursive = TRUE)
  }
  
  # Criação de pares de gráficos
  for (month in seq(1, 12, by = 2)) {
    p1 <- ggplot(data |> dplyr::filter(mes == sprintf("%02d", month)),
                 aes(x = as.numeric(dia), y = valor_diario)) +
      geom_line(color = "#4A90E2", size = 1.2) +
      geom_point(color = "#4A90E2", size = 2.5) +
      geom_text(
        aes(label = valor_diario),
        vjust = -1.5,
        size = 6,
        color = "#333333",
        fontface = "bold",
        angle = 45,
        hjust = -0.5
      ) +
      scale_x_continuous(breaks = seq(1, 31, by = 1)) +
      labs(
        title = paste("Valores diários -", month_names[month]),
        x = NULL,
        y = "Valor Diário"
      ) +
      theme_minimal(base_family = "lato", base_size = 16) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 28,
          color = "#333333"
        ),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16),
        panel.grid.major = element_line(
          color = "grey80",
          size = 0.3,
          linetype = "dotted"
        ),
        panel.grid.minor = element_line(
          color = "grey80",
          size = 0.2,
          linetype = "dotted"
        ),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    p2 <- ggplot(data |> dplyr::filter(mes == sprintf("%02d", month + 1)),
                 aes(x = as.numeric(dia), y = valor_diario)) +
      geom_line(color = "#4A90E2", size = 1.2) +
      geom_point(color = "#4A90E2", size = 2.5) +
      geom_text(
        aes(label = valor_diario),
        vjust = -1.5,
        size = 6,
        color = "#333333",
        fontface = "bold",
        angle = 45,
        hjust = -0.5
      ) +
      scale_x_continuous(breaks = seq(1, 31, by = 1)) +
      labs(
        title = paste("Valores diários -", month_names[month + 1]),
        x = NULL,
        y = "Valor Diário"
      ) +
      theme_minimal(base_family = "lato", base_size = 16) +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold",
          size = 28,
          color = "#333333"
        ),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16),
        panel.grid.major = element_line(
          color = "grey80",
          size = 0.3,
          linetype = "dotted"
        ),
        panel.grid.minor = element_line(
          color = "grey80",
          size = 0.2,
          linetype = "dotted"
        ),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    
    combined_plot <- grid.arrange(p1, p2, ncol = 2)
    
    
    file_path <- file.path(parameter_dir,
                           paste0("grafico_", month_names[month], "_e_", month_names[month + 1], ".png"))
    ggsave(
      file_path,
      plot = combined_plot,
      width = 24,
      height = 12,
      dpi = 300
    )
  }
}



plot_correlograma <- function(resultados_correlacao) {
  correlacoes <- resultados_correlacao |>
    mutate(i = match(parametro, unique(parametro)),
           j = match(parametro_relacionado, unique(parametro_relacionado)))
  
  p <- ggplot(correlacoes, aes(x = i, y = j, fill = correlacao)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "#1f77b4",
      mid = "#ffffff",
      high = "#d62728",
      midpoint = 0,
      limit = c(-1, 1)
    ) +
    geom_text(aes(label = round(correlacao, 2)), color = "black", size = 4) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 12
      ),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 12)
    ) +
    coord_fixed() +
    theme(legend.position = "none") +
    labs(x = "Variável", y = "Variável") +
    scale_x_continuous(
      breaks = 1:length(unique(resultados_correlacao$parametro)),
      labels = unique(resultados_correlacao$parametro)
    ) +
    scale_y_continuous(
      breaks = 1:length(unique(resultados_correlacao$parametro)),
      labels = unique(resultados_correlacao$parametro)
    )
  
  ggsave(
    "plots/correlograma_quadrado.png",
    plot = p,
    width = 10,
    height = 8
  )
}