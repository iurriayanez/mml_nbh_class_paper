#------------------------------------------------------------------------------#
# Name: 1_nbh_cluster_scales_long.R
# Goal: Create neighbourhood classification with grid's aggregate info across multiple scales and multiple years simultaneously
# Date of creation: 04-12-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/grid/grid_var_scales_met_aggl_[metaggl]_[year].rds
# Output: data/nbh/clusters_scales_k[nclust]_long_metaggl_[met_aggl].csv
#                   data/nbh/clusters_basis_matrix_k[nclust]_long_metaggl_[met_aggl].csv
#                   data/nbh/clusters_coef_matrix_k[nclust]_long_metaggl_[met_aggl].csv
#
#                   output/tables/rank_stats_NMF_scales_nostd_long_metaggl_[met_aggl].csv
#                   output/figures/all_rank_stats_NMF_scales_nostd_long_metaggl_[met_aggl].bmp
#                   output/figures/sel_rank_stats_NMF_scales_nostd_long_metaggl_[met_aggl].bmp
#------------------------------------------------------------------------------#

#---------------#
# 1. Import ----
#---------------#

  ## 1.1 Import database ----
  
    # Create data.table to import all years in one dataset
    df_long = data.table()
  
    # Import the grid_var_scales dataset for selected years
    for (i in years) {
      
      file_rds <- paste0("grid_var_scales_metaggl_", met_aggl, "_", i, ".rds")
      df  <- read_rds(file.path(path_grid,file_rds))
      setDT(df)

      # Create year variable
      df[, YEAR := i]
      
      # Append
      df_long = rbind(df_long, df)
      rm(df)
      
    }
    
    df1 = df_long
    
  ## 1.2 Relevant variables ----
    # IDs
    cols_ids  <- c(paste0('VIERKANT', grid_size,'M'),
                   'METAGGL', 
                   'YEAR')
    
    # Demographic variables 
    cols_demo <- c(
      # Ethnic background
      c(grep("^PERC_ETHNL", names(df1), value = TRUE)),
      c(grep("^PERC_ETHEUR", names(df1), value = TRUE)),
      c(grep("^PERC_ETHTUR", names(df1), value = TRUE)),
      c(grep("^PERC_ETHMOR", names(df1), value = TRUE)),
      c(grep("^PERC_ETHSUR", names(df1), value = TRUE)),
      c(grep("^PERC_ETHCAR", names(df1), value = TRUE)),
      c(grep("^PERC_ETHINDO", names(df1), value = TRUE)),
      c(grep("^PERC_ETHOAF", names(df1), value = TRUE)),
      c(grep("^PERC_ETHOAS", names(df1), value = TRUE)),
      c(grep("^PERC_ETHOAM", names(df1), value = TRUE)),
      # Age groups 
      c(grep("^PERC_AGE_GROUP_1", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_2", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_3", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_4", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_5", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_6", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_7", names(df1), value = TRUE)),
      # Household type:
      c(grep("^PERC_HH_TYPE_1", names(df1), value = TRUE)), 
      c(grep("^PERC_HH_TYPE_2", names(df1), value = TRUE)), 
      c(grep("^PERC_HH_TYPE_3", names(df1), value = TRUE)), 
      c(grep("^PERC_HH_TYPE_4", names(df1), value = TRUE)),
      # Students 
      c(grep("^PERC_STUDENTS", names(df1), value = TRUE)) 
    ) 
    
    # Socioeconomic variables 
    cols_inc <- c(# Household income
      c(grep("^PERC_HH_INC_Q1", names(df1), value = TRUE)),
      c(grep("^PERC_HH_INC_Q2", names(df1), value = TRUE)),
      c(grep("^PERC_HH_INC_Q3", names(df1), value = TRUE)),
      c(grep("^PERC_HH_INC_Q4", names(df1), value = TRUE)),
      c(grep("^PERC_HH_INC_Q5", names(df1), value = TRUE)),
      # Social benefits and unemployment
      c(grep("^PERC_SOCBENEF", names(df1), value = TRUE))
    )
  
    # Housing variables 
    cols_hous <- c(
      c(grep("^PERC_OBJ_OWNED", names(df1), value = TRUE))
    )
    
    # All columns
    cols <- c( cols_ids, cols_demo, cols_inc, cols_hous)
    
    # Numeric columns
    cols_num <- c(cols_demo, cols_inc, cols_hous)
    
    # Data with selected variables 
    df1 <- df1[, cols, with = FALSE]

  ## 1.3 Preprocessing ---- 
    # Remove missing values 
      summary(df1)
      length(unique(df1$VIERKANT100M))
      df1 = na.omit(df1)
      length(unique(df1$VIERKANT100M))
  
    # Keep grids without NAs throughout the window 
      df1[, COUNT_VIERKANT := .N, by = VIERKANT100M]
      df1 = df1[COUNT_VIERKANT == length(years),]
      df1[, COUNT_VIERKANT := NULL]
      summary(df1)
      length(unique(df1$VIERKANT100M))

  ## 1.4 Normalise variables to the metropolitan agglomeration average ----
    df2 <- copy(df1)
    df1[, (cols_num) := lapply(.SD, function(x) ((x-mean(x))/sd(x))), by = METAGGL, .SDcols = cols_num ]

#---------------------#
# 2. Wide to long ----
#---------------------#
  # Set columns as lists 
    list_cols_num <- list(
      c(grep("^PERC_ETHNL", names(df1), value = TRUE)),
      c(grep("^PERC_ETHEUR", names(df1), value = TRUE)),
      c(grep("^PERC_ETHTUR", names(df1), value = TRUE)),
      c(grep("^PERC_ETHMOR", names(df1), value = TRUE)),
      c(grep("^PERC_ETHSUR", names(df1), value = TRUE)),
      c(grep("^PERC_ETHCAR", names(df1), value = TRUE)),
      c(grep("^PERC_ETHINDO", names(df1), value = TRUE)),
      c(grep("^PERC_ETHOAF", names(df1), value = TRUE)),
      c(grep("^PERC_ETHOAS", names(df1), value = TRUE)),
      c(grep("^PERC_ETHOAM", names(df1), value = TRUE)),
      # Age groups 
      c(grep("^PERC_AGE_GROUP_1", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_2", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_3", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_4", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_5", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_6", names(df1), value = TRUE)),
      c(grep("^PERC_AGE_GROUP_7", names(df1), value = TRUE)),
      # Household type:
      c(grep("^PERC_HH_TYPE_1", names(df1), value = TRUE)), 
      c(grep("^PERC_HH_TYPE_2", names(df1), value = TRUE)), 
      c(grep("^PERC_HH_TYPE_3", names(df1), value = TRUE)), 
      c(grep("^PERC_HH_TYPE_4", names(df1), value = TRUE)),
      # Students 
      c(grep("^PERC_STUDENTS", names(df1), value = TRUE)),
      # Household income
      c(grep("^PERC_HH_INC_Q1", names(df1), value = TRUE)),
      c(grep("^PERC_HH_INC_Q2", names(df1), value = TRUE)),
      c(grep("^PERC_HH_INC_Q3", names(df1), value = TRUE)),
      c(grep("^PERC_HH_INC_Q4", names(df1), value = TRUE)),
      c(grep("^PERC_HH_INC_Q5", names(df1), value = TRUE)),
      # Social benefits and unemployment
      c(grep("^PERC_SOCBENEF", names(df1), value = TRUE)),
      # Tenure
      c(grep("^PERC_OBJ_OWNED", names(df1), value = TRUE))
    )
    
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
      'PERC_AGE_GROUP_1',
      'PERC_AGE_GROUP_2',
      'PERC_AGE_GROUP_3',
      'PERC_AGE_GROUP_4',
      'PERC_AGE_GROUP_5',
      'PERC_AGE_GROUP_6',
      'PERC_AGE_GROUP_7',
      # Household type:
      'PERC_HH_TYPE_1', 
      'PERC_HH_TYPE_2', 
      'PERC_HH_TYPE_3', 
      'PERC_HH_TYPE_4',
      # Students 
      'PERC_STUDENTS',
      # Household income
      'PERC_HH_INC_Q1',
      'PERC_HH_INC_Q2',
      'PERC_HH_INC_Q3',
      'PERC_HH_INC_Q4',
      'PERC_HH_INC_Q5',
      # Social benefits and unemployment
      'PERC_SOCBENEF',
      # Tenure
      'PERC_OBJ_OWNED'
    )
    
  # Wide to long    
    df1_long <- melt(df1, id.vars = cols_ids, measure.vars = list_cols_num, 
                     variable.name = "SCALE", value.name = cols_names)
    
    df2_long <- melt(df2, id.vars = cols_ids, measure.vars = list_cols_num, 
                     variable.name = "SCALE", value.name = cols_names)  
  
  # Last scale is grid level 
    df1_long[, SCALE := ifelse(SCALE == 7, 0, SCALE)]
    df2_long[, SCALE := ifelse(SCALE == 7, 0, SCALE)] 
    
  # Scaling again
    df1_long <- copy(df2_long)
    df1_long[, (cols_names) := lapply(.SD, function(x) ((x-min(x))/(max(x)-min(x)))), by = SCALE, .SDcols = cols_names ]
    
  # Subset scales 
    df2_long_clust <- df2_long[SCALE %in% clust_scales, ]
    df1_long_clust <- df1_long[SCALE %in% clust_scales, ]

#------------#
# 3. NMF ----
#------------#
  library(NMF)
  ## 3.1 Create database ----
    df_nmf = df2_long_clust[, cols_names, with = FALSE]
    summary(df_nmf)

  ## 3.2 Optimal rank selection ----
    # Silhouette cannot be calculated in C with long vectors --> Bootstrap
    # Silhouette of NMF cluster classification of original dataset rows (grid cells)
    log <- file(file.path(path_log_nbh,"1_nbh_cluster_scales_long_rank_select.txt"))
    sink(log, append = TRUE)
    sink(log, append = TRUE, type = "message")
    ranks <- 2:15
    estim <- lapply(ranks, function(r){
      print(r)
      fit <- NMF::nmf(df_nmf, r, nrun = 50, method = 'lee', seed = "random", .opt = 'vp')
      w <- fit@fit@W
      clust <- NMF::predict(fit, what = 'rows')
      sil.feat <- bootstrap_silhouette(w, clust, ns = 40, boot = 1000)     # clust: clusters database; ns: bootstrap sample = total/ns ; boot: bootstrap repetitions
      sparse <- NMF::sparseness(fit)
      list(fit = fit, coph = NMF::cophcor(fit), rss = NMF::rss(fit, df_nmf), 
           spar_basis = sparse[1], spar_coef = sparse[2], sil_con = silhouette(fit, what = 'consensus'),
           sil_sample = silhouette(fit, what = 'samples'), sil_feat = sil.feat,
           dispersion = NMF::dispersion(fit))
    })
    names(estim) <- paste0('rank', ranks)

    # As data frame 
    summary_estim <- summary.nmf(estim)
  
    # Summary plots
    sum_plots_estim <- summary_nmf_plots(summary_estim)

    # Save results 
    save_file <- paste0("rank_stats_NMF_scales_nostd_long_metaggl_", met_aggl, ".csv")
    fwrite(summary_estim, file = file.path(path_results,"tables",save_file), row.names = TRUE)   
    p1 <- sum_plots_estim$all
    p2 <- plot(sum_plots_estim$sel)
    ggsave(file.path(path_results, "figures", paste0("all_rank_stats_NMF_scales_nostd_long_metaggl_", met_aggl, ".bmp")), plot = p1, width = 15, height = 7, units = "in")
    ggsave(file.path(path_results, "figures", paste0("sel_rank_stats_NMF_scales_nostd_long_metaggl_", met_aggl, ".bmp")), plot = p2, width = 8, height = 7, units = "in")
    sink()
    sink(type = "message")
    
  ## 3.3 Final NMF ----
    # Optimal number of clusters 
    nclust = 4
    res.nmf <- NMF::nmf(df_nmf, nclust, nrun = 100, method = 'lee', seed = "random", .opt = 'vp')
    df2_long_clust$CLUSTER <- as.factor(NMF::predict(res.nmf, 'rows'))
    df1_long_clust$CLUSTER <- df2_long_clust$CLUSTER
    
    detach("package:NMF", unload = TRUE)
    
    # Export database 
    save_file <- paste0("clusters_scales_k", nclust, "_long_metaggl_", met_aggl, ".csv")
    fwrite(df2_long_clust, file = file.path(path_nbh, save_file), row.names = FALSE) 
    
    # Export coef and basis matrix 
    basis_matrix <- as.data.frame(res.nmf@fit@W)
    coef_matrix <- as.data.frame(res.nmf@fit@H)
  
    colnames(basis_matrix) <- paste0("CLUSTER", 1:nclust) 
    
    save_file <- paste0("clusters_basis_matrix_k", nclust, "_long_metaggl_", met_aggl, ".csv")
    fwrite(basis_matrix, file = file.path(path_nbh, save_file), row.names = FALSE) 
    
    save_file <- paste0("clusters_coef_matrix_k", nclust, "_long_metaggl_", met_aggl, ".csv")
    fwrite(coef_matrix, file = file.path(path_nbh, save_file), row.names = FALSE) 
