#------------------------------------------------------------------------------#
# Name: 3_obj_year.R
# Goal: Create yearly databases (1999-2023) with address object information: spatial location and tenure/WOZ value
# Date of creation: 04-10-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases:  data/ind/ind_spa/obj_tenure_[year].csv ; data/ind/ind_spa/obj_spatial_[year].csv
# Output databases: data/ind/ind_spa/obj_[year].rds
#------------------------------------------------------------------------------#

# Loop
pb = txtProgressBar(min = 0 , max = length(start_year:end_year), style = 3)       

for (year in start_year:end_year) { 
          
  #---------------------------------#
  # 1. Merge spatial and tenure ----
  #---------------------------------# 
          
    ## 1.1 Import spatial database ----
      spa_csv <- paste0("obj_spatial_",year,".csv")
      obj_spa <- fread(file.path(path_obj,spa_csv))
      
    ## 1.2 Import tenure database ----
      tenure_csv <- paste0("obj_tenure_",year,".csv")
      if (file.exists(file.path(path_obj,tenure_csv)) == TRUE) {
      obj_tenure <- fread(file.path(path_obj,tenure_csv))      
       
    ## 1.3 Merge ---- 
      df <- merge(obj_spa, obj_tenure, by = 'OBJ_ID', all.x = TRUE)
      rm(obj_spa, obj_tenure)
      } else {
        df <- obj_spa
        rm(obj_spa)
      }
      
  #---------------#
  # 2. Export ----
  #---------------#    
    
    ## 2.1 Export ----
      save_file <- paste0("obj_",year,".rds")
      write_rds(df, file = file.path(path_obj,save_file), compress = "gz")
      
      # Remove dataframe
      rm(df)
      
  #----------------------------------#
  # 3. Remove intermediate files ----
  #----------------------------------#
    
    ## 3.1 Spatial ----  
      csv_file <- paste0("obj_spatial_",year,".csv")
      #file.remove(file.path(path_obj,csv_file))
      
    ## 3.2 Tenure ----  
      csv_file <- paste0("obj_tenure_",year,".csv")
      if (file.exists(file.path(path_obj,csv_file)) == TRUE) {
      #file.remove(file.path(path_obj,csv_file))
      }
      
  # Update progress bar 
  setTxtProgressBar(pb, year - start_year + 1)     
}
close(pb)    
          