setwd("simulation")
library(tidyverse)


bench_times_background_shap1 <- read_csv("res/bench_times_true.csv")
bench_times_background_shap2 <- read_csv("res/bench_times_true2100.csv")
bench_times_background_shap <- bind_rows(bench_times_background_shap1, bench_times_background_shap2)
bench_times_glex <- readRDS("res/bench_times_glex.rds")

as.character(bench_times_glex$expression)

r1 <- bench_times_background_shap |>
  select(N, `Mean Time`) |>
  rename(median = `Mean Time`) |>
  mutate(method = "background shap")

r2 <- bench_times_glex |>
  select(N, median) |>
  mutate(method = "glex")

to_plot <- rbind(r1, r2)

ggplot(to_plot, aes(x = N, y = median, color = method)) +
  geom_point() +
  geom_line(aes(group = method)) +
  labs(
    x = "N",
    y = "Time (s)",
    title = "Time complexity for N = background = foreground",
  ) +
  theme_minimal()
