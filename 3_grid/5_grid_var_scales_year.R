#------------------------------------------------------------------------------#
# Name: 6_grid_var_scales_year.R
# Goal: Consolidate chunks to create final data base with grid cell info by scales
# Date of creation: 03-01-2024

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/grid/grid_var_scales_[year]_[part].csv
# Output databases: data/grid/grid_var_scales_[year].rds
#------------------------------------------------------------------------------#

for (year in (start_year:inc_year)) {
  
  print(year)
  
  # 1. Import part 1 of grid_var_scale ---- 
    df <- fread(file.path(path_grid, paste0("grid_var_scales_", year, "_part_1", ".csv")))
  
  # 2. Append subsequent parts of grid_var_scale ----
    for (k in (2:num_chunks)) {
      print(k)
      df1 <- fread(file.path(path_grid, paste0("grid_var_scales_", year, "_part_", k, ".csv")))
      df <- rbind(df, df1)
      rm(df1)
    }
    
  # 3. Export ----
    save_file <- paste0("grid_var_scales_", year,".rds")
    write_rds(df, file = file.path(path_grid,save_file), compress = "gz")   
    rm(df)
    
  # 4. Remove chunks 
    for (k in 1:num_chunks) {
      csv_file <- paste0("grid_var_scales_", year, "_part_", k, ".csv")
      #file.remove(file.path(path_grid,csv_file))
    }
}
