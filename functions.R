#------------------------------------------------------------------------------#
# Name: functions
# Goal: Script to create functions
# Date of creation: 21-12-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez
#------------------------------------------------------------------------------#


# 1. Color palettes ----
colors_cluster <- c("#4477AA", "#FFDF00", "#882255", "#FF7300")
blue_gradient_pal <- c("#d7d7d7", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C")

# 2. NMF ----
  ## 2.1 Bootstrap silhouette widths (for large datasets) ----
  bootstrap_silhouette <- function(data, clusters, boot = 100, sample_size = NULL, ns = 2) {
    # If no sample is specified, use half the data
    if (is.null(sample_size)) {
      sample_size <- floor(nrow(data) / ns)
    }
    
    # Initialize a vector to store the silhouette widths
    silhouette_widths <- numeric(boot)
    
    # Perform bootstrap
    for (i in 1:boot) {
      # Generate a random subsample 
      subsample_indices <- sample(1:nrow(data), sample_size)
      subsample_data <- data[subsample_indices, ]
      subsample_clusters <- clusters[subsample_indices]
      
      # Calculate silhouette
      sil.dist = 1-stats::cor(t(subsample_data))
      silhouette_output <- cluster::silhouette(as.numeric(subsample_clusters), dmatrix = sil.dist)
      silhouette_widths[i] <- mean(aggregate(silhouette_output[,3], list(silhouette_output[,1]), mean)[,2])
    }
    
    # Calculate the mean of the bootstrap silhouette widths
    mean_width <- mean(silhouette_widths)
    
    # Calculate the 95% confidence interval 
    ci <- quantile(silhouette_widths, c(0.05, 0.95))
    
    # Return list
    return(list(mean = mean_width, ci = ci))
  }

  ## 2.2 Summary of results as data.frame ----
  summary.nmf <- function(estim.list) {
    summary.df <-  data.frame (
      rank = ranks,
      sil_basis = unlist(lapply(estim.list, function(x) x$sil_feat$mean)),
      sil_basis_lci = unlist(lapply(estim.list, function(x) x$sil_feat$ci[1])),
      sil_basis_uci = unlist(lapply(estim.list, function(x) x$sil_feat$ci[2])),
      sil_con = unlist(lapply(estim.list, function(x) mean(x$sil_con[,3]))),
      sil_sample = unlist(lapply(estim.list, function(x) mean(x$sil_sample[,3]))),
      coph = unlist(lapply(estim.list, function(x) mean(x$coph))),
      rss = unlist(lapply(estim.list, function(x) mean(x$rss))),
      spar_basis = unlist(lapply(estim.list, function(x) mean(x$spar_basis))),
      spar_coef = unlist(lapply(estim.list, function(x) mean(x$spar_coef))),
      dispersion = unlist(lapply(estim.list, function(x) mean(x$dispersion)))
    )
    return(summary.df)
  }


  ## 2.3 Summary plots ----
  summary_nmf_plots <- function(data) { 
    
    # Silhouette plot
    sil.plot <- ggplot(data = data, aes(x= as.factor(rank))) +
      geom_line(aes(y= sil_basis, group = 1, color = "Basis"), position = position_dodge(width = 0.2), size = 0.5) +
      geom_point(aes(y= sil_basis, group = 1, color =  "Basis"), position = position_dodge(width = 0.2), size = 1.5) +
      geom_errorbar(
        aes(ymin = sil_basis_lci, ymax = sil_basis_uci),
        position = position_dodge(width = 0.2),
        width = 0.2
      ) +
      geom_line(aes(y = sil_con, group = 2, color =  "Consensus"), position = position_dodge(width = 0.2), size = 0.5) +
      geom_point(aes(y = sil_con, group = 2, color =  "Consensus"), position = position_dodge(width = 0.2), size = 1.5) +
      geom_line(aes(y = sil_sample, group = 3, color =  "Coef."), position = position_dodge(width = 0.2), size = 0.5) +
      geom_point(aes(y = sil_sample, group = 3, color =  "Coef."), position = position_dodge(width = 0.2), size = 1.5) +
      labs(title = "Silhouette", x = "Ranks", y = "", color = "") +
      scale_color_manual(values = c("black", "red", "purple"), labels = c("Basis", "Coef.", "Consensus")) +
      theme_minimal() +
      theme(text = element_text(family = "serif"))
    
    # Cophonetic plot
    coph.plot <- ggplot(data = data, aes(x= as.factor(rank))) +
      geom_line(aes(y= coph, group = 1, color = "Consensus"), position = position_dodge(width = 0.2), size = 0.5) +
      geom_point(aes(y= coph, group = 1, color =  "Consensus"), position = position_dodge(width = 0.2), size = 1.5) +
      labs(title = "Cophonetic", x = "Ranks", y = "", color = "") +
      scale_color_manual(values = c("purple"), labels = c("Consensus")) +
      theme_minimal() +
      theme(text = element_text(family = "serif"))
    
    # Sparseness 
    spar.plot <- ggplot(data = data, aes(x= as.factor(rank))) +
      geom_line(aes(y= spar_basis, group = 1, color = "Basis"), position = position_dodge(width = 0.2), size = 0.5) +
      geom_point(aes(y= spar_basis, group = 1, color =  "Basis"), position = position_dodge(width = 0.2), size = 1.5) +
      geom_line(aes(y = spar_coef, group = 2, color =  "Coef."), position = position_dodge(width = 0.2), size = 0.5) +
      geom_point(aes(y = spar_coef, group = 2, color =  "Coef."), position = position_dodge(width = 0.2), size = 1.5) +
      labs(title = "Sparseness", x = "Ranks", y = "", color = "") +
      scale_color_manual(values = c("black", "red"), labels = c("Basis", "Coef.")) +
      theme_minimal() +
      theme(text = element_text(family = "serif"))
    
    # RSS
    rss.plot <- ggplot(data = data, aes(x= as.factor(rank))) +
      geom_line(aes(y= rss, group = 1, color = "Best fit"), position = position_dodge(width = 0.2), size = 0.5) +
      geom_point(aes(y= rss, group = 1, color =  "Best fit"), position = position_dodge(width = 0.2), size = 1.5) +
      labs(title = "RSS", x = "Ranks", y = "", color = "") +
      scale_color_manual(values = c("blue"), labels = c("Best fit")) +
      theme_minimal() +
      theme(text = element_text(family = "serif"))
    
    # Dispersion
    dispersion.plot <- ggplot(data = data, aes(x= as.factor(rank))) +
      geom_line(aes(y= dispersion, group = 1, color = "Best fit"), position = position_dodge(width = 0.2), size = 0.5) +
      geom_point(aes(y= dispersion, group = 1, color =  "Best fit"), position = position_dodge(width = 0.2), size = 1.5) +
      labs(title = "Dispersion", x = "Ranks", y = "", color = "") +
      scale_color_manual(values = c("blue"), labels = c("Best fit")) +
      theme_minimal() +
      theme(text = element_text(family = "serif"))
    
    # Silhouette basis, coph and sparseness coefs
    sel.plot <- ggplot(data = data, aes(x= as.factor(rank))) +
      geom_line(aes(y= sil_basis, group = 1, color = "Silhouette"), position = position_dodge(width = 0.2), size = 0.5) +
      geom_point(aes(y= sil_basis, group = 1, color =  "Silhouette"), position = position_dodge(width = 0.2), size = 1.5) +
      geom_errorbar(
        aes(ymin = sil_basis_lci, ymax = sil_basis_uci),
        position = position_dodge(width = 0.2),
        width = 0.2
      ) +
      geom_line(aes(y= coph, group = 2, color = "Cophonetic"), position = position_dodge(width = 0.2), size = 0.5) +
      geom_point(aes(y= coph, group = 2, color =  "Cophonetic"), position = position_dodge(width = 0.2), size = 1.5) +
      geom_line(aes(y = spar_coef, group = 3, color =  "Sparseness"), position = position_dodge(width = 0.2), size = 0.5) +
      geom_point(aes(y = spar_coef, group = 3, color =  "Sparseness"), position = position_dodge(width = 0.2), size = 1.5) +
      labs(title = "Rank selection", x = "Ranks", y = "", color = "Stat") +
      scale_color_manual(values = c("purple", "black", "red"), labels = c("Cophonetic", "Silhouette", "Sparseness")) +
      theme_minimal() +
      theme(text = element_text(family = "serif"))
    
    # Plot all together
    list.plots <- list()
    list.plots[["all"]] <- gridExtra::arrangeGrob(sil.plot, coph.plot, spar.plot, rss.plot, dispersion.plot, ncol = 3)
    list.plots[["sel"]] <- sel.plot
    return(list.plots)
  }


# Maps
map_variables <- function(data, variable, legend_name, add_breaks = FALSE, br) {
  var <- variable
  if (add_breaks == TRUE) {
    nbreaks <- length(br) - 1
    category_colors <- grDevices::palette.colors(nbreaks)
    labs <- round(br, 1)
    stop <- length(labs) - 1
    labs_plot <- paste0("[", labs[1:stop], "- ", labs[2:length(labs)], ")")
    labs_plot[[length(labs_plot)]] <- paste0("[", labs[(length(labs)-1)], "- ", labs[length(labs)], "]")
    
    data[[var]] <- as.factor(cut(data[[var]], breaks = br, labels  = FALSE, include.lowest = TRUE))

    discrete.scale <- TRUE

    fill_options = scale_fill_manual(
      values = blue_gradient_pal,
      labels = labs_plot
    )
    
    # fill_options = scale_fill_brewer(
    #   labels = labs_plot
    # )
    
  } else {
    
    data[[var]] <- data[[var]]
    discrete.scale <- FALSE
    fill_options = NULL
    
    if (class(data[[var]]) == "factor") {
      nbreaks <- length(unique(data[[var]]))
      category_colors <- palette.colors(nbreaks)
      discrete.scale <- TRUE
      fill_options = scale_fill_manual(
        values = category_colors
        #limits = c(0,1)
      )
    }
  }
  
  p <- ggplot() +
    geom_sf(data = other_sub, fill = "gray70", color = NA, alpha = 0.05) +
    geom_sf(data = water_sub, fill = "gray50", color = NA, alpha = 0.05) +
    geom_sf(data = built_sub, fill = "gray30", color = NA, alpha = 0.05) +
    geom_sf(data = roads_sub, fill = "gray10", color = NA, alpha = 0.05) +
    geom_sf(data = metaggl_sf, fill = NA, color = "black", alpha = 0.8, linewidth = 0.1) +
    geom_sf(data = data[!is.na(data[[var]]),], aes(fill = !!sym(var)), 
            color = NA, size = 0
    ) +
    
    # Labs
    fill_options +
    labs(fill= legend_name) +
    
    # Theme
    theme_minimal() +
    theme(
      plot.caption = element_text(size= 3, face = "italic"),
      plot.subtitle = element_text(size= 8, vjust = -8.5, hjust = 0.075),
      legend.position = "right",
      legend.text = element_text(size = 15), legend.title = element_text(size = 15),
      legend.key.size = unit(0.45, "cm"),
      legend.box.margin = margin(l = -12),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      rect = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(c(-0.25,-3.5,0,-3.5), "cm")
    ) +
    
    # Arrow and scale bar 
    ggspatial::annotation_scale(
      location = 'br', 
      width_hint = 0.12, 
      style = "ticks", 
      pad_x = unit(1.5, "cm"), 
      pad_y = unit(1, "cm"),
      line_width = 1.5, 
      text_pad = unit(0.15, "cm"), 
      tick_height = 1, 
      text_cex = 0.5,
      height = unit(0.2, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = 'tr', 
      which_north = TRUE, 
      height = unit(1, "cm"),
      width = unit(1, "cm"),
      pad_x = unit(1.5, "cm"),
      pad_y = unit(1.5, "cm"),
      style = ggspatial::north_arrow_orienteering(text_size = 3.5)
    )
  
  return(p)
}

# Create polygons based on grid cell data
grid_to_polygon <- function(data, grid_size = 100) {
  if (class(data)[1] != "data.table") {
    data <- as.data.table(data)
  }
  
  if (grid_size == 100) {
    polygons_df <- data.table(VIERKANT100M = unique(data[,VIERKANT100M]))
    polygons_df[, `:=`(X = as.numeric(paste0(substr(VIERKANT100M, 2, 5),"50")),
                       Y = as.numeric(paste0(substr(VIERKANT100M, 7, 10),"50")))]
    polygons_df <- as.data.frame(polygons_df)
    
    polygons_sf <- sf::st_sf(
      geometry = sf::st_sfc(
        lapply(1:nrow(polygons_df), function(i){
          sf::st_polygon(list(rbind(
            c(polygons_df$X[i] - 50, polygons_df$Y[i] + 50),
            c(polygons_df$X[i] + 50, polygons_df$Y[i] + 50),
            c(polygons_df$X[i] + 50, polygons_df$Y[i] - 50),
            c(polygons_df$X[i] - 50, polygons_df$Y[i] - 50),
            c(polygons_df$X[i] - 50, polygons_df$Y[i] + 50)
          )))
        }),
        crs = 28992
      ),
      ID = polygons_df$VIERKANT100M
    )
    
  } else {
    polygons_df <- data.table(VIERKANT100M = unique(data[,VIERKANT500M]))
    polygons_df[, `:=`(X = (as.numeric(substr(VIERKANT500M, 2, 5)) * 100 + 250),
                       Y = (as.numeric(substr(VIERKANT500M, 7, 10)) * 100 + 250))]
    polygons_df <- as.data.frame(polygons_df)
    
    polygons_sf <- sf::st_sf(
      geometry = sf::st_sfc(
       lapply(1:nrow(polygons_df), function(i){
         sf::st_polygon(list(rbind(
            c(polygons_df$X[i] - 250, polygons_df$Y[i] + 250),
            c(polygons_df$X[i] + 250, polygons_df$Y[i] + 250),
            c(polygons_df$X[i] + 250, polygons_df$Y[i] - 250),
            c(polygons_df$X[i] - 250, polygons_df$Y[i] - 250),
            c(polygons_df$X[i] - 250, polygons_df$Y[i] + 250)
          )))
        }),
        crs = 28992
      ), 
      ID = polygons_df$VIERKANT500M
    )
    
  }
  return(polygons_sf)
}