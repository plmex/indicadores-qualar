source("R/process_air_quality_parameters.R")
source("R/plots.R")


plot_all_parameters <- function(dfs) {
  walk2(names(dfs), dfs, ~plot_monthly_values(.y, .x))
}

dfs <- process_parameters(lines)
plot_all_parameters(dfs)

