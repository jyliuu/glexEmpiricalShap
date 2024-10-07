setwd("simulation")
library(tidyverse)


bench_times_background_shap <- read_csv("res/bench_times_true.csv")
bench_times_background_shap2 <- read_csv("res/bench_times_true2.csv")
bench_times_background_shap <-  bind_rows(bench_times_background_shap1, bench_times_background_shap2)

# Create a vector of file names
file_names <- paste0("res/bench_res_glex/bench_times_glex", 1:8, ".rds")

# Read all .rds files and combine them into one tibble
bench_times_glex <- file_names %>%
  map_dfr(readRDS)

bench_times_glex

r1 <- bench_times_background_shap |>
  select(N, `Mean Time`) |>
  rename(median = `Mean Time`) |>
  mutate(method = "Tree SHAP-Int.")

r2 <- bench_times_glex |>
  select(N, median) |>
  mutate(method = "FastPD")

to_plot <- rbind(r1, r2)


tikzDevice::tikz(file = "figures/runtime.tex", width = 4, height = 4)
p1 <- ggplot(to_plot, aes(x = N, y = median, color = method)) +
  geom_point() +
  geom_line(aes(group = method)) +
  labs(
    x = "$n_b = n_f$",
    y = "Time (s)",
    # title = "Time complexity for background = foreground",
    color = "Method"  # Set the legend title
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  # Set the legend position to bottom
p1
dev.off()
p1
ggsave("figures/plot_runtime.pdf", plot = p1, width = 4, height = 4)
p1


