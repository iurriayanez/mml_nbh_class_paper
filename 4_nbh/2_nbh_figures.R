#------------------------------------------------------------------------------#
# Name: 3_nbh_figures_tables.R
# Goal: Create figures and tables
# Date of creation: 04-12-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/nbh/clusters_scales_k[nclust]_long_metaggl_[met_aggl].csv
#                  data/grid/grid_[year].rds   
#                  data/nbh/clusters_basis_matrix_k[nclust]_long_metaggl_[met_aggl].csv
#                  data/nbh/clusters_coef_matrix_k[nclust]_long_metaggl_[met_aggl].csv 
#                  output/tables/rank_stats_NMF_scales_nostd_long_metaggl_[met_aggl].csv
# Output: output/figures/sel_rank_stats_NMF_scales_nostd_long_metaggl_[met_aggl].pdf
#                  output/figures/sel_rank_stats_NMF_scales_nostd_long_metaggl_[met_aggl].eps
#                  output/figures/coefmap_NMF_scales_nostd_k[nclust]_long_metaggl_[met_aggl].pdf
#                  output/figures/coefmap_NMF_scales_nostd_k[nclust]_long_metaggl_[met_aggl].eps
#                  output/figures/coefmap_NMF_scales_nostd_k[nclust]_long_metaggl_[met_aggl].bmp
#                  output/figures/clusters-maps/clusters_NMF_k[nclust]_scale_[scale]_[year]_metaggl_[met_aggl].pdf
#                  output/figures/clusters-maps/clusters_NMF_k[nclust]_scale_[scale]_[year]_metaggl_[met_aggl].eps
#                  output/figures/alluvial_plot_[selected_scale]_metaggl_[met_aggl].pdf
#                  output/figures/alluvial_plot_[selected_scale]_metaggl_[met_aggl].eps
#                  output/figures/clusters_size_scale_years_metaggl_[met_aggl].pdf
#------------------------------------------------------------------------------#

#--------------------#
# 1. Import data ----
#--------------------#  

  ## 1.1 Clusters ----
    cluster_csv <- paste0("clusters_scales_k", nclust, "_long_metaggl_", met_aggl, ".csv")
    df <- fread(file.path(path_nbh, cluster_csv))
    
    df$CLUSTER <- factor(df$CLUSTER, labels = clust_labs)
    df <- df %>% mutate(SCALE = SCALE + 1)

  ## 1.2 Coef matrix  ---- 
    coef_csv <- paste0("clusters_coef_matrix_k", nclust, "_long_metaggl_", met_aggl, ".csv")
    coef_matrix <- fread(file.path(path_nbh, coef_csv))    
    
  ## 1.3 Basis matrix ----
    basis_csv <- paste0("clusters_basis_matrix_k", nclust, "_long_metaggl_", met_aggl, ".csv")
    basis_matrix <- fread(file.path(path_nbh, basis_csv))
    
    # Add basis components
    df <- cbind(df, basis_matrix)

    # Create basis variable (value of basis of the corresponding cluster) 
    df$BASIS <- do.call(pmax, df[, paste0("CLUSTER", 1:nclust), with = FALSE])
  
  ## 1.4 Gray values of cells for maps ----
  # We need to gray out cells with less than 5 individuals and less than 3 households

    # Only grid cells in the metropolitan agglomeration in 2022
      grid <- read_rds(file.path(path_grid,"grid_2022.rds"))
      grid <- grid %>% 
        select(VIERKANT100M, METAGGL) %>% 
        filter(METAGGL == met_aggl)
    
      grid_long = data.table()
      for (i in years) {
        file_rds  <- paste0("grid_",i,".rds")
        grid_year <- read_rds(file.path(path_grid,file_rds))
        setDT(grid_year)
        grid_year <- grid_year[, c('VIERKANT100M', 'TOT_IND',
                                   'TOT_HH'), with = FALSE]
        grid_year <- merge(grid, grid_year, by = 'VIERKANT100M', all.x = T)
        grid_year <- grid_year[METAGGL == met_aggl,]
        grid_year[, YEAR := as.integer(i)]
        grid_long = rbind(grid_long, grid_year)
        rm(grid_year)
      }
    

    # Create sample identifier
      grid_long[, SAMPLE := 0]
      grid_long[TOT_IND >= min_ind & TOT_HH >= min_hh, SAMPLE := 1]
      grid_long[, .(sample = sum(SAMPLE)), by = YEAR]
    
    # Add information to original dataframe 
    df <- merge(df, grid_long, by = c('VIERKANT100M', 'YEAR'), all.x = T)
    
    df %>% 
      filter(SCALE == 1) %>% 
      group_by(YEAR) %>% 
      summarise(sample = sum(SAMPLE))
    
    # Gray out cluster variable 
    df$CLUSTER_MAP <- as.factor(ifelse(df$SAMPLE == 0, NA, df$CLUSTER))
    df$CLUSTER1_MAP <- as.numeric(ifelse(df$SAMPLE == 0, NA, df$CLUSTER1))
    df$CLUSTER2_MAP <- as.numeric(ifelse(df$SAMPLE == 0, NA, df$CLUSTER2))
    df$CLUSTER3_MAP <- as.numeric(ifelse(df$SAMPLE == 0, NA, df$CLUSTER3))
    df$CLUSTER4_MAP <- as.numeric(ifelse(df$SAMPLE == 0, NA, df$CLUSTER4))
    #df$CLUSTER5_MAP <- as.numeric(ifelse(df$SAMPLE == 0, NA, df$CLUSTER5))
    
  ## 1.5 Import shapefiles ----
    gem_sf <- sf::st_read(file.path(path_utilities, "Tools","GISHulpbestanden", "Gemeentewijkbuurt", "2021", "gm_2021.shp"))
    metaggl_sf <- sf::st_read(file.path("raw", paper,  
                                    "CBS_grootstedelijke_agglomeratie_2015_NL", 
                                    "CBS_grootstedelijke_agglomeratie_2015_NL.shp")) %>% 
      select(statcode) %>% 
      filter(statcode == paste0("GA",met_aggl))
    land_sf <- sf::st_read(file.path(path_utilities, "Tools","GISHulpbestanden", "Bodemstatistiek", "2015", "BBG2015_Publicatiebestand.shp"))
    water <- subset(land_sf, BG2015 >= 70 & BG2015 < 90)
    roads <- subset(land_sf, BG2015 >= 10 & BG2015 < 20)
    built <- subset(land_sf, BG2015 >= 20 & BG2015 < 40)
    other <- subset(land_sf, BG2015 >= 40 & BG2015 < 70)

  ## 1.6 Create polygons of grid cells ----
    polygons_sf <- grid_to_polygon(df)

  ## 1.7 Create data.frame with variables and geometry ----
    clusters_sf <- sf::st_as_sf(merge(df, polygons_sf, by.y = 'ID', by.x = 'VIERKANT100M', all.x = TRUE))
    
    # Subset land use 
      bbox_list <- sf::st_bbox(metaggl_sf)
      y_min <- bbox_list[["ymin"]] - 100
      y_max <- bbox_list[["ymax"]] + 100
      x_min <- bbox_list[["xmin"]] - 100
      x_max <- bbox_list[["xmax"]] + 100
      
      box_coords <- tibble(x = c(x_min, x_max), y = c(y_min, y_max)) %>% 
        sf::st_as_sf(coords = c("x", "y")) %>% 
        sf::st_set_crs(sf::st_crs(land_sf))
      bbox <- sf::st_bbox(box_coords) %>% sf::st_as_sfc()
      
      land_sf_sub <- sf::st_intersection(land_sf,bbox)
      water_sub <- sf::st_intersection(water,bbox)
      roads_sub <- sf::st_intersection(roads,bbox)
      built_sub <- sf::st_intersection(built,bbox)
      other_sub <- sf::st_intersection(other,bbox)

    # Database with variables and geometry by year and by scale
      for (i in years) {
        for (k in seq_along(scale_labs)) {
          clusters_scales <- paste("clusters_sf",i, k, sep = "_")
          assign(clusters_scales, clusters_sf[clusters_sf$YEAR == i & clusters_sf$SCALE == k,]) 
        }
      }
    
  ## 1.8 Wide format ----
    # IMPORTANT TO ORDER BY VIERKANT100M and YEAR!
      df_wide <- df %>%
        select(VIERKANT100M, SCALE, CLUSTER, YEAR) %>% 
        arrange(., VIERKANT100M, YEAR, SCALE) %>% 
        tidyr::pivot_wider(names_from = SCALE, values_from = CLUSTER) %>% 
        relocate(`1`, .before = `2`) 
      
      colnames(df_wide)[3:(2+length(scale_labs_save))] <- scale_labs_save
      
#-------------------------------------------#
# Figure 2: NMF Optimal Rank selection  ----
#-------------------------------------------# 
  # Selection rank stats
    # Silhouette basis, coph and sparseness coefs
    read_file <- paste0("rank_stats_NMF_scales_nostd_long_metaggl_", met_aggl, ".csv")
    rank_stats <- fread(file = file.path(path_results,"tables",read_file))   
      
    sel.plot <- ggplot(data = rank_stats, aes(x= as.factor(rank))) +
        geom_vline(xintercept = (nclust -1), color = "grey70", linetype = "dashed", alpha = 0.5) +
        geom_line(
          aes(y= sil_basis, group = 1, color = "Silhouette"), 
          position = position_dodge(width = 0.2), 
          linewidth = 0.5
        ) +
        geom_point(
          aes(y= sil_basis, group = 1, color =  "Silhouette"), 
          position = position_dodge(width = 0.2), 
          size = 1.5, 
          shape = 15
        ) +
        geom_errorbar(
          aes(ymin = sil_basis_lci, ymax = sil_basis_uci),
          position = position_dodge(width = 0.2),
          width = 0.2
        ) +
        geom_line(
          aes(y= coph, group = 2, color = "Cophonetic"), 
          position = position_dodge(width = 0.2), 
          linewidth = 0.5
        ) +
        geom_point(
          aes(y= coph, group = 2, color =  "Cophonetic"),
          position = position_dodge(width = 0.2), 
          size = 1.5, 
          shape = 16
        ) +
        geom_line(
          aes(y = spar_coef, group = 3, color =  "Sparseness"), 
          position = position_dodge(width = 0.2), 
          linewidth = 0.5
        ) +
        geom_point(
          aes(y = spar_coef, group = 3, color =  "Sparseness"), 
          position = position_dodge(width = 0.2), 
          size = 1.5, 
          shape =17
        ) +
        labs(x = "Ranks", y = "", color = "") +
        scale_color_manual(
          values = c("#4477AA", "black", "#882255"), 
          labels = c("Cophonetic", "Silhouette", "Sparseness")
        ) +
        theme(
          panel.background = element_blank(),
          #text = element_text(family = "serif"),
          panel.grid.minor.x = element_blank(),
          axis.line = element_line(),
          legend.position = c(0.95,0.25),
          legend.justification = c(1,0),
          legend.text = element_text(size = 11),
          legend.key.size = unit(0.95, "cm")
        )
      
      ggsave(file.path(path_results, "figures", paste0("sel_rank_stats_NMF_scales_nostd_long_metaggl_", met_aggl, ".pdf")), plot = sel.plot, width = 7, height = 4, units = "in", dpi = 400)
      ggsave(file.path(path_results, "figures", paste0("sel_rank_stats_NMF_scales_nostd_long_metaggl_", met_aggl, ".eps")), plot = sel.plot, width = 7, height = 4, units = "in", dpi = 400)
      
#-----------------------------#
# Figure 4: NMF H Matrix  ----
#-----------------------------# 

  # Column names 
    cols_names <- c(
      # Ethnic background
      'PERC_ETHNL',
      'PERC_ETHEUR',
      'PERC_ETHTUR',
      'PERC_ETHMOR',
      'PERC_ETHSUR',
      'PERC_ETHCAR',
      'PERC_ETHINDO',
      'PERC_ETHOAF',
      'PERC_ETHOAS',
      'PERC_ETHOAM',
      # Age groups 
      'PERC_AGE_GROUP_7',
      'PERC_AGE_GROUP_6',
      'PERC_AGE_GROUP_5',
      'PERC_AGE_GROUP_4',
      'PERC_AGE_GROUP_3',
      'PERC_AGE_GROUP_2',
      'PERC_AGE_GROUP_1',
      # Students
      'PERC_STUDENTS',
      # Household type:
      'PERC_HH_TYPE_1', 
      'PERC_HH_TYPE_2', 
      'PERC_HH_TYPE_3', 
      'PERC_HH_TYPE_4',
      # Household income
      'PERC_HH_INC_Q5',
      'PERC_HH_INC_Q4',
      'PERC_HH_INC_Q3',
      'PERC_HH_INC_Q2',
      'PERC_HH_INC_Q1',
      # Social benefits 
      'PERC_SOCBENEF',
      # Tenure
      'PERC_OBJ_OWNED'
    )
      
    row_labels <- c(
      'Mig. background: Netherlands',
      'Mig. background: Europe',
      'Mig. background: Turkey',
      'Mig. background: Morocco',
      'Mig. background: Suriname',
      'Mig. background: Dutch Caribbean',
      'Mig. background: Indonesia',
      'Mig. background: Other Africa',
      'Mig. background: Other Asia',
      'Mig. background: Other America & Oceania',
      'Age: 65+',
      'Age: 56-65',
      'Age: 46-55',
      'Age: 36-45',
      'Age: 26-35',
      'Age: 18-25',
      'Age: 0-18',
      'Students (>18)',
      'Household type: Single person',
      'Household type: Couples no children',
      'Household type: Couples with children',
      'Household type: Single-parents',
      'Std. household income: 81-100%',
      'Std. household income: 61-80%',
      'Std. household income: 41-60%',
      'Std. household income: 21-40%',
      'Std. household income: 0-20%',
      'Social benefit recipients',
      'Owner-occupied residential units'
  )
     
  coef_plot <- coef_matrix %>% 
        # Scale to sum 1 by column 
        mutate(across(everything(), ~./sum(.))) %>% 
        tidyr::gather(key = "var") %>%
        mutate(rank = rep(1:nclust, length(cols_names))) %>% 
        ggplot() +
        geom_tile(
          aes(x = rank, y = var, fill = value, width = 1, height = 1), 
          color = "gray9"
        ) +
        scale_fill_viridis_c(
          option = "rocket",
          direction = -1, 
          limits = c(0,1),
          breaks = seq(from = 0, to = 1, by = 0.2),
          guide = guide_colorbar(frame.colour = "gray9", ticks.colour = NA)
        ) +
        labs(x = "Clusters", fill = "Value") +
        scale_y_discrete(limits = rev(cols_names), labels = rev(row_labels)) +
        theme_minimal() +
        theme(axis.ticks = element_blank(),
              rect = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_blank(),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 6),
              axis.title.y = element_blank(),
              axis.title.x = element_text(margin = margin(t = 10)))   
   
  ggsave(
    file.path(path_results, "figures", paste0("coefmap_NMF_scales_nostd_k", nclust, "_long_metaggl_", met_aggl, ".bmp")), 
    plot = coef_plot,
    width = 5,
    height = 7,
    units = "in"
  )
  
  ggsave(
    file.path(path_results, "figures", paste0("coefmap_NMF_scales_nostd_k", nclust, "_long_metaggl_", met_aggl, ".pdf")), 
    plot = coef_plot,
    width = 5,
    height = 7,
    units = "in",
    dpi = 400
  )
  
  ggsave(
    file.path(path_results, "figures", paste0("coefmap_NMF_scales_nostd_k", nclust, "_long_metaggl_", met_aggl, ".eps")), 
    plot = coef_plot,
    width = 5,
    height = 7,
    units = "in",
    dpi = 400
  )
  

#----------------------------#
# Figure 5: Map clusters ----
#----------------------------# 

  ## Plot aesthetics ----
    clustlabs <- scale_fill_manual(
      values = colors_cluster,
      labels = clust_labs
    )

  ## Individual plots (by year and by scale) ----
    for (i in years) {
      for (k in seq_along(scale_labs)) {
        plot.data <- get(paste("clusters_sf",i, k, sep = "_"))
        plot.clust <- map_variables(plot.data, "CLUSTER_MAP", "", add_breaks = FALSE)
        plot.clust <- plot.clust + 
            clustlabs +
            labs(subtitle = paste(scale_labs[[k]], "-", i, sep = " "))
        #ggsave(file.path(path_results, "figures", "clusters-maps", paste0("clusters_NMF_k", nclust, "_scale",scale_labs_save[[k]], "_", i, "_metaggl_", met_aggl, ".bmp")), plot = plot.clust, width = 3.5, height = 3.5, units = "in", dpi = 400)
        ggsave(file.path(path_results, "figures", "clusters-maps", paste0("clusters_NMF_k", nclust, "_scale",scale_labs_save[[k]], "_", i, "_metaggl_", met_aggl, ".pdf")), plot = plot.clust, width = 8, height = 8, units = "in", dpi = 500)
        ggsave(file.path(path_results, "figures", "clusters-maps", paste0("clusters_NMF_k", nclust, "_scale",scale_labs_save[[k]], "_", i, "_metaggl_", met_aggl, ".eps")), plot = plot.clust, width = 8, height = 8, units = "in", dpi = 500)
        
      }
    }
    
  

#-----------------------------#
# Figure 6: Alluvial plot ---- 
#-----------------------------#
  library(ggalluvial)
  
  ## Plot aesthetics ----
  clustlabs <- scale_fill_manual(
    values = colors_cluster,
    labels = clust_labs
  )
  
  ## For every scale  over the years ---- 
  for (k in scale_labs_save) {

    # Select scale 
    selected_scale <- k

    # Select scale
    df_alluvial <- df_wide %>% 
      select(VIERKANT100M, YEAR, !!sym(selected_scale)) %>% 
      rename(Cluster = !!sym(selected_scale))
    
    # Plot 
    p <- ggplot(df_alluvial,
                aes(x = YEAR, stratum = Cluster, 
                    alluvium = VIERKANT100M, 
                    fill = factor(Cluster), 
                )) +
      geom_flow(alpha = 0.5, color = "gray20") +
      geom_stratum()+
      scale_x_continuous(breaks = unique(df_alluvial$YEAR)) +
      clustlabs +
      theme(panel.background = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(),
            legend.title = element_blank(),
            legend.text = element_text(size = 12),
            legend.key.size = unit(1,"cm"),
            axis.title.x = element_blank()) +
      ylab("Cluster Size (100x100m grid cells)") 
    
    #ggsave(file.path(path_results, "figures", paste0("alluvial_plot", selected_scale, "_metaggl_", met_aggl, ".bmp")), plot = p, width = 8, height = 5, units = "in", dpi = 500)
    ggsave(file.path(path_results, "figures", paste0("alluvial_plot", selected_scale, "_metaggl_", met_aggl, ".pdf")), plot = p, width = 8, height = 5, units = "in", dpi = 500)
    ggsave(file.path(path_results, "figures", paste0("alluvial_plot", selected_scale, "_metaggl_", met_aggl, ".eps")), plot = p, width = 8, height = 5, units = "in", dpi = 500)
  }
  
 detach("package:ggalluvial", unload = TRUE)
  
#-----------------------------#
# Figure A1: Cluster size ---- 
#-----------------------------#
  
    p <- df %>%
    group_by(YEAR, SCALE, CLUSTER) %>%
    summarise(n_clust = n()) %>% 
    ggplot() +
    geom_line(aes(x = YEAR, y = n_clust, color = as.factor(CLUSTER)),
              size = 0.5) +
    geom_point(aes(x = YEAR, y = n_clust, color = as.factor(CLUSTER)),
              size = 1.5) +
    facet_wrap(~SCALE, scales = 'free', labeller = as_labeller(c(`1` = "100x100m grid cells",
                                                  `2` = "100m radius",
                                                  `3` = "200m radius",
                                                  `4` = "400m radius",
                                                  `5` = "800m radius",
                                                  `6` = "1600m radius",
                                                  `7` = "3200m radius"))) +
    scale_color_manual(values= colors_cluster) +
    ggthemes::theme_tufte() +
    theme(panel.background = element_blank(),
            text = element_text(family = "serif"),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(),
            legend.title = element_blank(),
            legend.position = c(0.9,0),
            legend.justification = c(1,0),
            legend.text = element_text(size = 12),
            legend.key.size = unit(1,"cm"),
            axis.title.x = element_blank()) +
    ylab("Cluster Size (100x100m grid cells)") +
    xlab("Year") +
    scale_x_continuous(breaks=as.numeric(years)) +
    scale_y_continuous(lim = c(0,4500))

    #ggsave(file.path(path_results, "figures", paste0("clusters_size_scale_years_metaggl_", met_aggl, ".bmp")), plot = p, width = 12, height = 7, units = "in", dpi = 300)
    ggsave(file.path(path_results, "figures", paste0("clusters_size_scale_years_metaggl_", met_aggl, ".pdf")), plot = p, width = 12, height = 7, units = "in", dpi = 300)
    ggsave(file.path(path_results, "figures", paste0("clusters_size_scale_years_metaggl_", met_aggl, ".eps")), plot = p, width = 12, height = 7, units = "in", dpi = 300)
    
