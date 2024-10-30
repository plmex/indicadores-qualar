source("R/process_air_quality_parameters.R")
source("R/plots.R")

radg <- dfs[[8]]

plot_monthly_values(data = radg, parameter_name = "radg_plots")


