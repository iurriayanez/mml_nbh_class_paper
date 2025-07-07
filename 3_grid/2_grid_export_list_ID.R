#------------------------------------------------------------------------------#
# Name: 3_grid_export_list_ID.R
# Goal: Create list with all the grid cells with available information between 1999 - 2022
# Date of creation: 12-12-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases:  grid_[year].rds
# Output database: data/grid/list_VIERKANT[grid_size]M_1999_2022.csv    
#------------------------------------------------------------------------------#

df_list = data.table()

for (year in start_year:inc_year) { 
  print(year)
  df = read_rds(file.path(path_grid,paste0("grid_",year,".rds")))
  setDT(df)
  df = df[, VIERKANT100M]
  df_list = rbind(df_list, df)
  df_list = unique(df_list)
  rm(df)
}

# Export 
setnames(df_list, 'x', 'VIERKANT100M')
fwrite(df_list, file = file.path(path_grid,vierkantlistname))   

rm(grid_df, ind_df, hh_df, obj_df)

  
    
  
 
