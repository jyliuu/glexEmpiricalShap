library(patchwork)
library(reshape2)
library(tidyverse)
source("main_loop.r")
theme_set(theme_bw(base_size = 8))
theme_update(
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, size = 0.6),
  legend.title = element_text(size = 8),
  legend.key.size = unit(0.4, "cm"),
  legend.text = element_text(size = 6)  # Adjust legend text size here
)


BoxPlot <- function(
  res,
  settings,
  coord,
  MSEtype,
  MSEtypeIsComponents = FALSE) {
  # Plots boxplots of different types of MSE for different settings
  # res: result of simulation
  # settings: settings for which to plot box plot
  # group: variable on x-axis
  # coord: coordinate to show MSE for
  # MSEtype: type of MSE to use
  # MSEtypeIsComponents: set to TRUE if the MSEtype is component MSE
  data <- NULL
  mse_type_listnumber <- ifelse(MSEtypeIsComponents, 2, 1)
  for (i in settings) {
    estType <- c("TreeSHAP-Path.", "FastPD", "FastPD-50", "FastPD-100$^\\ast$")
    if (as.numeric(res[[i]]$params[1]) > 500) {
      estType <- c(estType, "FastPD-500")
    }
    for (j in 1:length(res[[i]]$sim_res)) {
      glex_objs <- res[[i]]$sim_res[[j]]$glex_objs
      stats <- res[[i]]$sim_res[[j]]$stats

      # stats[[1]] <- obtain_shap_MSEs(
      #   true_shap_fun = function(...) 1,
      #   glex_true_p = glex_objs[[1]],
      #   to_explain = glex_objs[-1]
      # )
      table <- cbind(res[[i]]$params, stats[[mse_type_listnumber]][[MSEtype]], Method = estType, row.names = NULL)
      data <- rbind(data, table)
    }
  }
  data$Method <- factor(data$Method, levels = c("FastPD", "FastPD-500", "FastPD-100$^\\ast$", "FastPD-50", "TreeSHAP-Path."))

  library(RColorBrewer)

  # Define the palette for the reds and greens
  reds <- brewer.pal(9, "Reds")   # 3 shades of red
  greens <- brewer.pal(9, "Greens") # 2 shades of green


  p <- ggplot(data, aes_string(x = "as.factor(n)", y = coord, fill = "Method")) +
    scale_y_log10() +
    geom_boxplot(width = 0.8, lwd = 0.1, outlier.size = 0.5) +
    labs(x = "$n$", y = "Mean squared error") +
    scale_fill_manual(values = c(
    "FastPD" = reds[7],        # Light red
    "FastPD-500" = reds[6],    # Dark red
    "FastPD-50" = reds[4],     # Medium red
    "FastPD-100$^\\ast$" = greens[7],  # Darker green
    "TreeSHAP-Path." = greens[4] # Lighter green
    )) +
    theme(
      legend.position = "right",
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 6)  # Adjust legend text size here
  )
  # ggsave(paste0("figures/6boxplot", coord, ".pdf"), plot = p, width = 5.5, height = 3.5, dpi = 1200)
  return(p)
}


plotComponentIters <- function(res, setting, coord, titles, true_function = NULL) {
  # Plots the components of the glex object for all simulations together
  # res: result of simulation
  # setting: setting for which to plot
  # coord: coordinate to plot components for
  # tiles: titles of the plots
  # true_function: insert function in plots. Set to NULL if length(coords)>2
  p <- list(NULL)
  for (i in 1:length(res[[setting]]$sim_res[[1]]$glex_objs)) {
    data <- NULL
    cat("calculating components for", c("\\hat{m}p", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500")[i], "\n")
    for (j in 1:100) {
      data <- rbind(data, data.frame(
        x = res[[setting]]$sim_res[[j]]$glex_objs[[i]]$x[[coord]],
        m = res[[setting]]$sim_res[[j]]$glex_objs[[i]]$m[[coord]],
        It = as.factor(j)
      ))
    }
    p[[i]] <- ggplot(data, aes(x = x, y = m)) +
      geom_line(aes(group = It), alpha = 0.05) +
      labs(x = coord, y = paste0("\\hat{m}", substring(coord, 2, 2)), title = titles[i]) +
      stat_function(fun = true_function, col = "red")
  }
  cat("plotting...", "\n")
  plot <- p[[1]]
  for (i in 2:length(p)) {
    plot <- plot + p[[i]]
  }
  ggsave(paste0("figures/compItersS", setting, coord, ".png"), plot = plot, width = 10, height = 8, dpi = 300)
  return(plot)
}



plotMedianShap <- function(res, setting, MSEtype, coord, titles) {
  # Plots the shap of the glex object with median MSE
  # res: result of simulation
  # setting: setting for which to plot
  # MSEtype: type of MSE for which to calculate median MSE. Number in list c("TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500")
  # coord: coordinate to plot shap values for
  # tiles: titles of the plots
  shapMSE <- NULL
  for (i in 1:length(res[[setting]]$sim_res)) {
    shapMSE[i] <- res[[setting]]$sim_res[[i]]$stats[[1]]$B_shap_mse[MSEtype, coord]
  }
  medianIndex <- which(shapMSE == min(shapMSE[which(shapMSE >= median(shapMSE))]))
  p <- list(NULL)
  x <- res[[setting]]$sim_res[[medianIndex]]$dataset$x
  data <- data.frame(x,
    shapA = x[, 1] + x[, 1] * x[, 2] - 0.3,
    shapB = res[[setting]]$sim_res[[medianIndex]]$glex_objs[[1]]$shap[[coord]]
  )
  plot <- ggplot(reshape2::melt(data, id = colnames(x)), aes_string(x = coord, y = "value", color = "variable")) +
    geom_point(alpha = 0.1) +
    scale_color_manual(values = c("red", "blue")) +
    labs(y = "shap", title = titles[1]) +
    theme(legend.position = "none")
  for (i in 2:length(res[[setting]]$sim_res[[medianIndex]]$glex_objs)) {
    plotdata <- data.frame(data, shapVar = res[[setting]]$sim_res[[medianIndex]]$glex_objs[[i]]$shap[[coord]])
    colnames(plotdata)[length(plotdata)] <- "See title" # titles[i]
    plotdata <- reshape2::melt(plotdata, id = colnames(x))
    p[[i]] <- ggplot(plotdata, aes_string(x = coord, y = "value", color = "variable")) +
      geom_point(alpha = 0.1) +
      scale_color_manual(values = c("red", "blue", "green")) +
      labs(y = "shap", title = titles[i], color = "SHAP type")
    plot <- plot + p[[i]]
  }
  plot <- plot + plot_layout(guides = "collect")
  return(plot)
}


plotMedianShapModel <- function(res, setting, MSEtype, coord) {
  # Plots the shap of the glex object with median MSE
  # res: result of simulation
  # setting: setting for which to plot
  # MSEtype: type of MSE for which to calculate median MSE. Number in list c("TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500")
  # coord: coordinate to plot shap values for
  # tiles: titles of the plots
  shapMSE <- NULL
  for (i in 1:length(res[[setting]]$sim_res)) {
    shapMSE[i] <- res[[setting]]$sim_res[[i]]$stats[[1]]$B_shap_mse[MSEtype, coord]
  }
  medianIndex <- which(shapMSE == min(shapMSE[which(shapMSE >= median(shapMSE))]))
  x <- res[[setting]]$sim_res[[medianIndex]]$dataset$x
  data <- tibble(x1 = x[,1], x2 = x[,2],
    "Model SHAP" = res[[setting]]$sim_res[[medianIndex]]$glex_objs[[1]]$shap[[coord]]
  ) 
  # Create the left plot with shapB and treeshap
  data_left <- tibble(data, Estimate = res[[setting]]$sim_res[[medianIndex]]$glex_objs[[2]]$shap[[coord]])
  data_left <- reshape2::melt(data_left, id = colnames(x))
  data_left <- rename(data_left, Variable = variable)
  plot_left <- ggplot(data_left, aes_string(x = coord)) +
    geom_point(aes(y = value, color = Variable), alpha = 0.3) +
    scale_color_manual(values = c("red", "blue")) +
    labs(y = "$\\phi_1(x)$", x = "$x_1$", title = "Tree SHAP-Path", color = "Type")

  data_right <- tibble(data, Estimate = res[[setting]]$sim_res[[medianIndex]]$glex_objs[[3]]$shap[[coord]])
  data_right <- reshape2::melt(data_right, id = colnames(x))
  data_right <- rename(data_right, Variable = variable)
  plot_right <- ggplot(data_right, aes_string(x = coord)) +
    geom_point(aes(y = value, color = Variable), alpha = 0.3) +
    scale_color_manual(values = c("red", "blue")) +
    labs(y = NULL, x = "$x_1$", title = "FastPD", color = "Type")
  combined_plot <- plot_left + plot_right + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  # ggsave(paste0("figures/shap_fastpd_vs_treeshap", coord, ".pdf"), plot = combined_plot, width = 3.25, height = 2.6, dpi = 300)
  # return(plot_right)
  combined_plot
}



plotMedianComponents <- function(
    res, setting, MSEtype, coord, titles,
    xy_labs = c(
      substitute(x[s], list(s = coord)),
      substitute(hat(m)[s], list(s = coord))
    ),
    true_function = NULL) {
  # Plots the components of the glex object with median MSE
  # res: result of simulation
  # setting: setting for which to plot
  # MSEtype: type of MSE for which to calculate median MSE. Number in list c("\\hat{m}p", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500")
  # coord: coordinate to plot components for
  # tiles: titles of the plots
  # true_function: insert function in plots. Set to NULL if length(coords)>2
  coordsString <- paste0("x", coord)
  glexList <- list(NULL)
  compMSE <- NULL
  for (i in 1:length(res[[setting]]$sim_res)) {
    glexList[[i]] <- res[[setting]]$sim_res[[i]]$glex_objs
    compMSE[i] <- res[[setting]]$sim_res[[i]]$stats[[2]]$B_m_mse[MSEtype, coordsString] # stats 2 for components
  }
  medianIndex <- which.min(abs(compMSE - median(compMSE)))
  p <- list()

  glex_list_median <- glexList[[medianIndex]]
  glex_obj_B <- glex_list_median[[1]]
  glex_list_median <- glex_list_median[-1]
  for (i in seq_along(glex_list_median)) {
    glex_obj <- glex_list_median[[i]]
    out <- as.data.frame(cbind(glex_obj$x[[coord]], glex_obj$m[[coord]], glex_obj_B$m[[coord]])) |>
      rename(x = "V1", "Estimated component" = "V2", "Model component" = "V3") |>
      pivot_longer(
        cols = c("Estimated component", "Model component"), # Columns to pivot into longer format
        names_to = "type", # New column for variable names (o1_m, o2_m)
        values_to = "value" # New column for values from o1_m and o2_m
      )

    plt <- ggplot(out, aes_string(x = "x", y = "value", color = "type")) +
      labs(
        x = xy_labs[[1]],
        y = xy_labs[[2]]
      ) +
      geom_line() +
      ggtitle(titles[i]) +
      stat_function(fun = true_function, aes(color = "Ground truth")) +
      scale_color_manual(
        name = xy_labs[[2]],
        values = c("Estimated component" = "black", "Model component" = ggplot_colors[1], "Ground truth" = ggplot_colors[2]),
      )
    p[[i]] <- plt
  }

  # Determine limits for a specific group
  get_limits <- function(plots, axis) {
    range(c(sapply(plots, function(i) ggplot_build(p[[i]])$layout$panel_params[[1]][[paste0(axis, ".range")]])))
  }

  # Group plots into two lists
  group1 <- c(1, 2)
  group2 <- c(3, 4, 5)

  # Calculate limits for each group
  x_limits_group1 <- get_limits(group1, "x")
  y_limits_group1 <- get_limits(group1, "y")
  x_limits_group2 <- get_limits(group2, "x")
  y_limits_group2 <- get_limits(group2, "y")

  # Apply limits to each plot in group 1
  for (i in group1) {
    p[[i]] <- p[[i]] +
      scale_x_continuous(limits = x_limits_group1) +
      scale_y_continuous(limits = y_limits_group1) +
      theme(legend.position = "none") # Remove legend from individual plots
  }

  # Apply limits to each plot in group 2
  for (i in group2) {
    p[[i]] <- p[[i]] +
      scale_x_continuous(limits = x_limits_group2) +
      scale_y_continuous(limits = y_limits_group2) +
      theme(legend.position = "none") # Remove legend from individual plots
  }

  # Combine plots with adjusted layout
  combined_plot <- (p[[1]] | p[[2]]) /
    (p[[3]] + p[[4]] + p[[5]]) +
    plot_layout(heights = c(1.5, 1), guides = "collect") &
    theme(legend.position = "bottom")

  cat("plotting for simulation", medianIndex, "\n")
  ggsave(paste0("figures/comps", coordsString, ".pdf"), plot = combined_plot, width = 9, height = 8, dpi = 300)
  return(combined_plot)
}



plotMedianComponentsOnePlot <- function(
    res, setting, MSEtype, coord, titles,
    true_function = NULL) {
  # Plots the components of the glex object with median MSE
  # res: result of simulation
  # setting: setting for which to plot
  # MSEtype: type of MSE for which to calculate median MSE. Number in list c("\\hat{m}p", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500")
  # coord: coordinate to plot components for
  # tiles: titles of the plots
  # true_function: insert function in plots. Set to NULL if length(coords)>2
  ggplot_colors <- scales::hue_pal()(5) # Generates the first two default ggplot colors
  coordsString <- paste0("x", coord)
  glexList <- list(NULL)
  compMSE <- NULL
  for (i in 1:length(res[[setting]]$sim_res)) {
    glexList[[i]] <- res[[setting]]$sim_res[[i]]$glex_objs
    compMSE[i] <- res[[setting]]$sim_res[[i]]$stats[[2]]$B_m_mse[MSEtype, coordsString] # stats 2 for components
  }
  medianIndex <- which.min(abs(compMSE - median(compMSE)))
  p <- list()

  glex_list_median <- glexList[[medianIndex]]
  glex_obj_B <- glex_list_median[[1]]
  glex_list_median <- glex_list_median[-1]

  to_plot <- data.frame(x = glex_obj_B$x[[coord]], Model = glex_obj_B$m[[coord]])
  names(to_plot) <- c("x", "$\\hat{m}_1(x)$")
  for (i in c(1, 2, 4)) {
    to_plot[[titles[i]]] <- glex_list_median[[i]]$m[[coord]]
  }

  to_plot_long <- to_plot |>
    pivot_longer(
      cols = -x, # Columns to pivot (excluding 'x')
      names_to = "type", # New column for variable names
      values_to = "value" # New column for values
    )

  plt <- ggplot(to_plot_long, aes(x = x, y = value, color = type, linetype = type)) +
    geom_line() +
    labs(
      x = "$x_1$",
      y = "Estimated $m_1(x)$"
    ) +
    stat_function(fun = true_function, aes(color = "$m_1^\\ast(x)$", linetype = "$m_1^\\ast(x)$"))  +
    scale_color_manual(
      name = "Line",
      values = c(
      "Path-dep." = ggplot_colors[4],
      "FastPD" = "#282424",
      "FastPD-100" = "#b739d3",
      "$\\hat{m}_1(x)$" = ggplot_colors[1],
      "$m_1^\\ast(x)$" = ggplot_colors[3]),
      limits = c("FastPD", "FastPD-100", "Path-dep.", "$\\hat{m}_1(x)$", "$m_1^\\ast(x)$")
    ) +
    scale_linetype_manual(
      name = "Line",
      values = c(
        "Path-dep." = "twodash",
        "FastPD" = "twodash",
        "FastPD-100" = "twodash",
        "$\\hat{m}_1(x)$" = "solid",
        "$m_1^\\ast(x)$" = "solid"
      ),
      limits = c("FastPD", "FastPD-100", "Path-dep.", "$\\hat{m}_1(x)$", "$m_1^\\ast(x)$")
    ) +
    theme(legend.position = "right")

  cat("plotting for simulation", medianIndex, "\n")
  # ggsave(paste0("figures/comps_one_plot", coordsString, ".pdf"), plot = plt, width = 5, height = 3, dpi = 300)
  return(plt)
}

x <-plotMedianComponentsOnePlot(
  res = complete_res, setting = 3, MSEtype = 2, coord = 1,
  titles = c("Path-dep.", "FastPD", "FastPD-50", "FastPD-100", "Fast-PD500"),
  true_function = function(x) {
    x - 2 * 0.3
  }
)
setwd("simulation")
load("./res/complete_res.RData")
for (i in 1:3) {
  names(complete_res[[i]]) <- c("params", "sim_res")
  complete_res[[i]]$params <- data.frame(n = complete_res[[i]]$params[1], c = complete_res[[i]]$params[2], s = complete_res[[i]]$params[3])
  rownames(complete_res[[i]]$params) <- NULL
}

plotMedianComponents(
  res = complete_res, setting = 3, MSEtype = 2, coord = 1,
  titles = c("Path-dependent TreeSHAP", "EmpAll", "Emp50", "Emp100", "Emp500"),
  true_function = function(x) {
    x - 2 * 0.3
  }
)
# plotComponentIters(res = complete_res, setting = 1, coord = "x1", titles = c("Reference", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500"), true_function = function(x) x)


save_tex_decorator <- function(method, file_name, width = 4, height = 5) {
  function(...) {
    tikzDevice::tikz(
      file = paste0("figures/", file_name, ".tex"),
      width = width,
      height = height
    )
    print(method(...))
    dev.off()
  }
}


x <- plotMedianShapModel(res = complete_res, setting = 2, MSEtype = 2, coord = "x1")

plot_save_median_shap <- save_tex_decorator(
  method = plotMedianShapModel,
  file_name = "shap_fastpd_vs_treeshap",
  width = 3.25,
  height = 2.6
)
plot_save_median_shap(res = complete_res, setting = 2, MSEtype = 2, coord = "x1")


plot_save_boxplot <- save_tex_decorator(
  method = BoxPlot,
  file_name = "boxplotx1",
  width = 3.25,
  height = 2.2
)
plot_save_boxplot(res = complete_res, settings = c(2, 3), coord = "x1", MSEtype = "B_shap_mse", MSEtypeIsComponents = F)

x <-BoxPlot(res = complete_res, settings = c(2, 3), coord = "x1", MSEtype = "B_shap_mse", MSEtypeIsComponents = F)

plot_save_median_comp <- save_tex_decorator(
  plotMedianComponentsOnePlot,
  file_name = "comps_one_plot_x1",
  width = 3.25,
  height = 1.95
)

plot_save_median_comp(
  res = complete_res, setting = 3, MSEtype = 2, coord = 1,
  titles = c("Path-dep.", "FastPD", "FastPD-50", "FastPD-100", "Fast-PD500"),
  true_function = function(x) {
    x - 2 * 0.3
  }
)
dev.off()
