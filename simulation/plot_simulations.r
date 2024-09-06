library(patchwork)
library(reshape2)
library(tidyverse)
theme_set(theme_bw())
theme_update(
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, size = 0.6)
)
ggplot_colors <- scales::hue_pal()(3) # Generates the first two default ggplot colors


BoxPlot <- function(res, settings, group = n, coord, MSEtype, MSEtypeIsComponents = FALSE) {
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
    estType <- c("TreeSHAP", "Emp", "Emp50", "Emp100")
    if (as.numeric(complete_res[[i]]$params[1]) > 500) {
      estType <- c(estType, "Emp500")
    }
    for (j in 1:length(res[[i]]$sim_res)) {
      table <- cbind(res[[i]]$params, res[[i]]$sim_res[[j]]$stats[[mse_type_listnumber]][[MSEtype]], type = estType, row.names = NULL)
      data <- rbind(data, table)
    }
  }
  group_plotvar <- paste0("as.factor(", group, ")")
  p <- ggplot(data, aes_string(x = group_plotvar, y = coord, fill = "type")) +
    geom_boxplot(width = 0.5) +
    labs(x = group, y = MSEtype)
  ggsave(paste0("simulation/boxplot", coord, ".png"), plot = p, width = 10, height = 8, dpi = 300)
  return(p)
}


# BoxPlot(res = complete_res, settings = c(2, 3), group = "n", coord = "x1", MSEtype = "A_shap_mse", MSEtypeIsComponents = F)


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
  ggsave(paste0("figures/shaps", coord, ".png"), plot = plot, width = 10, height = 8, dpi = 300)
  return(plot)
}



load("simulation/res/complete_res.RData")
for (i in 1:3) {
  names(complete_res[[i]]) <- c("params", "sim_res")
  complete_res[[i]]$params <- data.frame(n = complete_res[[i]]$params[1], c = complete_res[[i]]$params[2], s = complete_res[[i]]$params[3])
  rownames(complete_res[[i]]$params) <- NULL
}

plotMedianComponents(
  res = complete_res, setting = 3, MSEtype = 1, coord = 1,
  titles = c("Path-dependent TreeSHAP", "EmpAll", "Emp50", "Emp100", "Emp500"),
  true_function = function(x) {
    x - 2 * 0.3
  }
)
# plotMedianShap(res = complete_res, setting = 3, MSEtype = 2, coord = "x1", titles = c("Reference", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500"))
# plotComponentIters(res = complete_res, setting = 1, coord = "x1", titles = c("Reference", "TreeSHAP", "Emp", "Emp50", "Emp100", "Emp500"), true_function = function(x) x)
