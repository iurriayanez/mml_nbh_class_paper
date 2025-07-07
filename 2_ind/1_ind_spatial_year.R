#------------------------------------------------------------------------------#
# Name: 1_ind_spatial_year
# Goal: Create yearly databases (1999-2023) with individual's address object info: address object ID, grid IDm, admin. spatial unit codes, tenure and WOZ value.
# Date of creation: 04-10-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases:  GBAADRESOBJECTBUS ; data/obj/obj_[year].rds
# Output databases: data/ind/ind_spatial_[year].csv
#------------------------------------------------------------------------------#

#------------------------------------------------------#    
# 1. Individual address object (GBAADRESOBJECTBUS) ----
#------------------------------------------------------#     
  
  ## 1.1 Import the database ----
    ind_add = read_spss(gbaadressobject,
                        col_select = c('RINPERSOON','RINPERSOONS', 
                                       'GBADATUMAANVANGADRESHOUDING', 'GBADATUMEINDEADRESHOUDING',
                                       'RINOBJECTNUMMER', 'SOORTOBJECTNUMMER'))
    setDT(ind_add)
    
  ## 1.2 ID ----
    # Persons
    ind_add[, IND_ID := paste0(RINPERSOON, RINPERSOONS)] 
    ind_add[, (c('RINPERSOON','RINPERSOONS')) := NULL]
    
    # Objects (address)
    ind_add[, OBJ_ID := paste0(RINOBJECTNUMMER, SOORTOBJECTNUMMER)] 
   
  ## 1.3 Date format ---- 
    cols = c('GBADATUMAANVANGADRESHOUDING', 'GBADATUMEINDEADRESHOUDING')
    ind_add[, (cols) := lapply(.SD, function(x) as.IDate(as.character(x), "%Y%m%d")), .SDcols = cols ]
    
  ## 1.4 Check data ----
    summary(ind_add)                                                              # General info
    any(duplicated(ind_add, by= 'OBJ_ID'))       # Duplicates
    
    table(ind_add$SOORTOBJECTNUMMER)                                              # Check unique values of SOORTOBJECTNUMMER
    ind_add[, (c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')) := NULL]

#-------------------------------#            
# 2. Merge with object info ----
#-------------------------------# 
  
  # We consider the address where the individual lives on January 1st each year  
      
  # Loop through each year  
    for (year in start_year:end_year) {
      # Print year
      print(year)
        
  ## 2.1 Filter by dates the grid database ----
      df = ind_add[
        
        # Start date before or on January 1st 
        ind_add$GBADATUMAANVANGADRESHOUDING <= as.IDate(paste0(year, "0101"),"%Y%m%d") &               
          
        # End date after or on January 1st
        ind_add$GBADATUMEINDEADRESHOUDING >= as.IDate(paste0(year, "0101"),"%Y%m%d")
        
      ]
      
  ## 2.2 Merge individual address with object info ----
    
    # Import object info 
      rds_file <- paste0("obj_",year,".rds")
      df_obj = read_rds(file.path(path_obj,rds_file))
      setDT(df_obj)
      
    # Merge   
      df = merge(df, df_obj, by='OBJ_ID', all.x = TRUE)
    
    # Change names
      setnames(df, c('GBADATUMAANVANGADRESHOUDING','GBADATUMEINDEADRESHOUDING'),
                   c('STARTADDRESS', 'ENDADDRESS'))
      
  ## 2.3 Export ----
      save_file <- paste0("ind_spatial_",year,".csv")
      fwrite(df, file = file.path(path_ind,save_file))
      
    # Remove dataframe 
      rm(df, df_obj)
    }
rm(ind_add)
