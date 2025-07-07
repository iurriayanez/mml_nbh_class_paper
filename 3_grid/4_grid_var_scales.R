#------------------------------------------------------------------------------#
# Name: 5_grid_var_scales.R
# Goal: Calculate variables across scales for all grid cells
# Date of creation: 19-12-2023

# R Version: R version 4.2.3 (2023-03-15 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Author: Ignacio Urria Yanez

# Input databases: data/grid/grid_[year].rds ; data/grid/euclidean_dist_[radius]m_[city]_part_[i].csv 
# Output databases: data/grid/grid_var_scales_[part].csv
#------------------------------------------------------------------------------#

#-------------------#    
# 1. Setup loop ----
#-------------------# 

  ## 1.1 All columns to keep from grid_year ----
    cols_ids  <- c('VIERKANT100M')
    
    cols_demo <- c(# Number of residents
      'TOT_IND',
      # Ethnic background
      'ETHNL',
      'ETHEUR',
      'ETHTUR',
      'ETHMOR',
      'ETHSUR',
      'ETHCAR',
      'ETHINDO',
      'ETHOAF', 
      'ETHOAS', 
      'ETHOAM',
      # Age groups 
      'AGE_GROUP_1',
      'AGE_GROUP_2',
      'AGE_GROUP_3',
      'AGE_GROUP_4',
      'AGE_GROUP_5',
      'AGE_GROUP_6',
      'AGE_GROUP_7',
      # Household type:
      'HH_TYPE_1', 
      'HH_TYPE_2', 
      'HH_TYPE_3', 
      'HH_TYPE_4',
      'HH_TYPE_5',
      # Students 
      'STUDENTS'
    ) 
    
    cols_inc <- c(# Household income
      'HH_INC_Q1',
      'HH_INC_Q2',
      'HH_INC_Q3',
      'HH_INC_Q4',
      'HH_INC_Q5',
      # Social benefits 
      'NOSOCBENEF', 
      'SOCBENEF'
    )
    
    cols_hous <- c(
      # Tenure
      'OBJ_OWNED',
      'OBJ_RENTED'
    )

    cols <- c( cols_ids, cols_demo, cols_inc, cols_hous)
    cols_nbh <- c(cols_demo, cols_inc, cols_hous)

for (year in start_year:inc_year) {
      
  print(year)
      
  #------------------------------------#    
  # 2. Import grid info data frame ----
  #------------------------------------# 
      
    grid_year <- read_rds(file.path(path_grid,paste0("grid_",year,".rds")))
    setDT(grid_year)
    grid_year <- grid_year[, (cols), with = FALSE]
    
  #-----------------------------------#    
  # 3. Import distance data frame ----
  #-----------------------------------# 
      
    for (k in (1:num_chunks)) {
      
      print(k)
  
    ## 3.1 Import ----
      df = read_rds(file.path(path_grid, paste0("euclidean_dist_", radius, "m_", city,"_part_", k, ".rds")))
      setDT(df)
      
    ## 3.2 Add rows with distance 0 for each unique origin_node_id 
      aux <- df[, .(destination_node_id = origin_node_id, euclidean_distance = 0), by = origin_node_id]
      nrows <- nrow(aux)
      df <- rbind(df, aux)
      rm(aux)
    
    ## 3.3 Conversion table 
      conv_table <- fread(file.path(path_grid, paste0("conv_table_euclidean_", radius, "m_", city, ".csv")))
      df <- merge(df, conv_table, by.x= 'origin_node_id', by.y = 'INT_VIERKANT100M', all.x = TRUE)
      df[, 'origin_node_id' := NULL]
      setnames(df, c('VIERKANT100M'), c('origin_node_id'))
      df <- merge(df, conv_table, by.x= 'destination_node_id', by.y = 'INT_VIERKANT100M', all.x = TRUE)
      df[, 'destination_node_id' := NULL]
      setnames(df, c('VIERKANT100M'), c('destination_node_id'))
      rm(conv_table)
  
  #---------------------------------#    
  # 4. Create variables by year ----
  #---------------------------------# 
    
      ## 4.1 Add info of destination grid cell
        df_year <- merge(df, grid_year, by.x = 'destination_node_id', by.y = 'VIERKANT100M', all.x = TRUE)
        rm(df)
        
      ## 4.2 Calculation by scales ----
        for (scales in scale_values) { 
          names <- paste(cols_nbh, scales, sep = "_")
          df_year[euclidean_distance <= scales, (names) := lapply(.SD, function(x) (sum(x, na.rm = TRUE))), by = origin_node_id, .SDcols = cols_nbh ]
        }
        
      ## 4.3 Keep one observation by grid ----
        df_year <- df_year[euclidean_distance == 0, ]
        
      ## 4.4 Calculate percentages by scale ---- 
        
        for (scales in scale_values) { 
          
          # Age (7 age groups)
            cols_perc <- c(paste('AGE_GROUP',1:7,scales,sep = "_"))
            names <- paste("PERC", cols_perc, sep = "_")
            cols_denom <- paste( 'TOT_IND', scales, sep = '_')
            df_year[, (names) := lapply(.SD, function(x) (x/ get(cols_denom) )), .SDcols = cols_perc ]
          
          # Ethnic background (10 groups)
           cols_perc <- c(paste('ETHNL',scales,sep = "_"), 
                           paste('ETHEUR',scales,sep = "_"),
                           paste('ETHTUR',scales,sep = "_"),
                           paste('ETHMOR',scales,sep = "_"),
                           paste('ETHSUR',scales,sep = "_"),
                           paste('ETHCAR',scales,sep = "_"),
                           paste('ETHINDO',scales,sep = "_"),
                           paste('ETHOAF',scales,sep = "_"),
                           paste('ETHOAS',scales,sep = "_"),
                           paste('ETHOAM',scales,sep = "_")
                          )
           
            names <- paste("PERC", cols_perc,  sep = "_")
            cols_denom1 <- paste('ETHNL',scales, sep = '_')
            cols_denom2 <- paste('ETHEUR',scales, sep = '_')
            cols_denom3 <- paste('ETHTUR',scales,sep = "_")
            cols_denom4 <- paste('ETHMOR',scales,sep = "_")
            cols_denom5 <- paste('ETHSUR',scales,sep = "_")
            cols_denom6 <- paste('ETHCAR',scales,sep = "_")
            cols_denom7 <- paste('ETHINDO',scales,sep = "_")
            cols_denom8 <- paste('ETHOAF',scales,sep = "_")
            cols_denom9 <- paste('ETHOAS',scales,sep = "_")
            cols_denom10 <- paste('ETHOAM',scales,sep = "_")
            
            df_year[, (names) := lapply(.SD, function(x) (x/(get(cols_denom1) + get(cols_denom2) + get(cols_denom3)
                                                             + get(cols_denom4) + get(cols_denom5) + get(cols_denom6)
                                                             + get(cols_denom7) + get(cols_denom8) + get(cols_denom9)
                                                             + get(cols_denom10)))), .SDcols = cols_perc ]
            
        
          # Household type 
            cols_perc <- c(paste('HH_TYPE',(1:5),scales,sep = "_"))
            names <- paste("PERC", cols_perc, sep = "_")
            cols_denom1 <- paste( 'HH_TYPE_1', scales, sep = '_')
            cols_denom2 <- paste( 'HH_TYPE_2', scales, sep = '_')
            cols_denom3 <- paste( 'HH_TYPE_3', scales, sep = '_')
            cols_denom4 <- paste( 'HH_TYPE_4', scales, sep = '_')
            cols_denom5 <- paste( 'HH_TYPE_5', scales, sep = '_')

            df_year[, (names) := lapply(.SD, function(x) (x/(get(cols_denom1) + get(cols_denom2) + get(cols_denom3)
                                                             + get(cols_denom4) + get(cols_denom5) ))), .SDcols = cols_perc ]
            
            
          # Household income 
            cols_perc <- c(paste0('HH_INC_Q',(1:5),"_",scales))
            names <- paste("PERC", cols_perc, sep = "_")

            cols_denom1 <- paste( 'HH_INC_Q1', scales, sep = '_')
            cols_denom2 <- paste( 'HH_INC_Q2', scales, sep = '_') 
            cols_denom3 <- paste( 'HH_INC_Q3', scales, sep = '_')
            cols_denom4 <- paste( 'HH_INC_Q4', scales, sep = '_')
            cols_denom5 <- paste( 'HH_INC_Q5', scales, sep = '_')
            
            df_year[, (names) := lapply(.SD, function(x) (x/(get(cols_denom1) + get(cols_denom2) + get(cols_denom3) + get(cols_denom4) + get(cols_denom5) ))), .SDcols = cols_perc ]
            
            
          # Students
            cols_perc <- c(paste('STUDENTS',scales,sep = "_"))
            names <- paste("PERC", cols_perc, sep = "_")
            cols_denom <- paste( 'TOT_IND', scales, sep = '_')
            df_year[, (names) := lapply(.SD, function(x) (x/get(cols_denom) )), .SDcols = cols_perc ]
          
          # Tenure
            cols_perc <- c(paste('OBJ_OWNED',scales,sep = "_"), paste('OBJ_RENTED',scales,sep = "_"))
            names <- paste("PERC", cols_perc, sep = "_")
            cols_denom1 <- paste( 'OBJ_OWNED', scales, sep = '_') 
            cols_denom2 <- paste( 'OBJ_RENTED', scales, sep = '_')
            df_year[, (names) := lapply(.SD, function(x) (x/(get(cols_denom1) + get(cols_denom2) ))), .SDcols = cols_perc ]              
            
          # Social benefits 
            cols_perc <- c(paste('SOCBENEF',scales,sep = "_"))
            names <- paste("PERC", cols_perc, sep = "_")
            cols_denom1 <- paste( 'SOCBENEF', scales, sep = '_') 
            cols_denom2 <- paste( 'NOSOCBENEF', scales, sep = '_')
            df_year[, (names) := lapply(.SD, function(x) (x/(get(cols_denom1) + get(cols_denom2) ))), .SDcols = cols_perc ]              
            
        }  
        
        ## 4.5 Calculate percentages at the grid cell level (no scale) ----  
        
          # Age
            cols_perc <- c(paste0('AGE_GROUP_', (1:7)))
            names <- paste("PERC", cols_perc, sep = "_")
            df_year[, (names) := lapply(.SD, function(x) (x/TOT_IND)), .SDcols = cols_perc ]
            
          # Ethnic background
            cols_perc <- c('ETHNL',
                           'ETHEUR',
                           'ETHTUR',
                           'ETHMOR',
                           'ETHSUR',
                           'ETHCAR',
                           'ETHINDO',
                           'ETHOAF', 
                           'ETHOAS', 
                           'ETHOAM')
            names <- paste("PERC", cols_perc, sep = "_")
            df_year[, (names) := lapply(.SD, function(x) (x/(ETHNL + ETHEUR + ETHTUR + 
            ETHMOR + ETHSUR + ETHCAR + ETHINDO + ETHOAF + ETHOAS + ETHOAM))), .SDcols = cols_perc ]
            
          # Household type
            cols_perc <- c(paste0('HH_TYPE_', (1:5)))
            names <- paste("PERC", cols_perc, sep = "_")
            df_year[, (names) := lapply(.SD, function(x) (x/(HH_TYPE_1 + HH_TYPE_2 + HH_TYPE_3 + HH_TYPE_4 + HH_TYPE_5 ))), .SDcols = cols_perc ]
            
          # Students
            cols_perc <- c('STUDENTS')
            names <- paste("PERC", cols_perc, sep = "_")
            df_year[, (names) := lapply(.SD, function(x) (x/(TOT_IND))), .SDcols = cols_perc ]
            
          # Household income
            cols_perc <- c(paste0('HH_INC_Q',(1:5)))
            names <- paste("PERC", cols_perc, sep = "_")
            df_year[, (names) := lapply(.SD, function(x) (x/(HH_INC_Q1 + HH_INC_Q2 + HH_INC_Q3 + HH_INC_Q4 + HH_INC_Q5))), .SDcols = cols_perc ]
            
          # Tenure
            cols_perc <- c('OBJ_OWNED', 'OBJ_RENTED')
            names <- paste("PERC", cols_perc, sep = "_")
            df_year[, (names) := lapply(.SD, function(x) (x/(OBJ_OWNED + OBJ_RENTED))), .SDcols = cols_perc ]
            
          # Social benefits
            cols_perc <- c('SOCBENEF')
            names <- paste("PERC", cols_perc, sep = "_")
            df_year[, (names) := lapply(.SD, function(x) (x/(SOCBENEF + NOSOCBENEF))), .SDcols = cols_perc ]
            
            
        ## 4.6 Relevant variables ----
          setnames(df_year, "origin_node_id", "VIERKANT100M")
          cols_perc <- c(grep("^PERC_", names(df_year), value = TRUE))
          cols_final <- c('VIERKANT100M',
                          cols_perc)
          df_year <- df_year[, (cols_final), with = FALSE]
        
    #---------------#    
    # 5. Export ----
    #---------------# 
          
          save_file <- paste0("grid_var_scales_", year, "_part_", k,".csv")
          fwrite(df_year, file = file.path(path_grid,save_file))   
          rm(df_year)
      }
    
  rm(grid_year)  
  
}
