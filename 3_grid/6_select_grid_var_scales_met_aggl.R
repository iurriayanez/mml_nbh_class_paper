#------------------------------------------------------------------------------#
# Name: clean_amsterdam
# Goal: Script to select final sample from grid_var_scales database
# Date of creation: 04-12-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases:  data/grid/grid_var_scales_[year].rds
#                  
# Output databases: data/grid/grid_var_scales_metaggl_10_[year].rds
#                   
#
#------------------------------------------------------------------------------#
# RUN MASTER BEFORE RUNNING 

# Import grid cell database to add Metropolitan agglomeration and gemeente codes
grid <- read_rds(file.path(path_grid,"grid_2022.rds"))
setDT(grid)

# Import the grid_var_scales dataset for selected years
for (i in start_year:inc_year) {
 
  file_rds <- paste0("grid_var_scales_",i,".rds")
  df  <- read_rds(file.path(path_grid,file_rds))
  setDT(df)
  
  # Add and filter by metropolitan agglomeration
  df<- merge(df, grid[, c('VIERKANT100M', 'METAGGL')], by = 'VIERKANT100M', all.x = TRUE)
  df = df[METAGGL == met_aggl]
  
  # Export 
  save_file <- paste0("grid_var_scales_metaggl_", met_aggl, "_", i, ".rds")
  write_rds(df, file = file.path(path_grid,save_file), compress = "gz")   
  rm(df)

}