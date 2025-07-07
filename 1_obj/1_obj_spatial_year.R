#------------------------------------------------------------------------------#
# Name: 1_obj_spatial_year
# Goal: Create yearly databases (1999-2023) with address objects spatial info: address object ID, grid ID, admin. spatial unit codes
# Date of creation: 20-11-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases:  VSLVIERKANTTAB ; VSLGWBTAB ; GIN2018  
# Output databases: data/obj/obj_spatial_[year].csv
#------------------------------------------------------------------------------#

#------------------------------------#      
# 1. Grid info (VSLVIERKANTTAB) ---- 
#------------------------------------#   
      
  ## 1.1 Import grid info ----
    grid = read_spss(vslvierkanttab)

    # Data.table format 
      grid = setDT(grid)

    # Remove labelled values of variables 
      cols = c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')
      grid[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]

  ## 2.2 ID ----
    # Objects (address)
      grid[, OBJ_ID := paste0(RINOBJECTNUMMER, SOORTOBJECTNUMMER)] 
      
  ## 2.3 Check data ----
    summary(grid)                                                                  # General info 
    
    any(duplicated(grid, by= 'OBJ_ID'))                                            # No duplicates. Key variables.
    
    table(grid$SOORTOBJECTNUMMER)
    # B: From the Basic Registration of Addresseses (Basisregistratie Adressen en Gebouwen). Objects from 1-1-2012. 
    # H: Historical origin. Objects from before 1-1-2012. 
    # D: Origin from the municipal population registers. Not B nor H. 
    
    grid[, (c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')) := NULL]
    
#------------------------------------------------------# 
# 2. Municipal and neighbourhood codes (VSLGWBTAB) ----
#------------------------------------------------------# 
    
  ## 2.1 Import administrative location information with municipality codes (vslgwbtab). ----
    add_admin = read_spss(vslgwbtab) 

    # Data.table format 
      add_admin = setDT(add_admin)

    # Remove labelled values of variables 
      cols = c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')
      add_admin[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]
    
    # As integers 
      cols = names(add_admin)[grep("^gem", names(add_admin))]
      add_admin[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]
      add_admin[, (cols) := lapply(.SD, function(x) (as.integer(gsub("[^0-9]", "", x)))), .SDcols = cols]
      
      cols = names(add_admin)[grep("^wc", names(add_admin))]
      add_admin[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]
      add_admin[, (cols) := lapply(.SD, function(x) (as.integer(gsub("[^0-9]", "", x)))), .SDcols = cols]
      
      cols = names(add_admin)[grep("^bc", names(add_admin))]
      add_admin[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]
      add_admin[, (cols) := lapply(.SD, function(x) (as.integer(gsub("[^0-9]", "", x)))), .SDcols = cols]
      
  ## 2.2 ID ----
    # Objects (address)
      add_admin[, OBJ_ID := paste0(RINOBJECTNUMMER, SOORTOBJECTNUMMER)] 
      add_admin[, (c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')) := NULL]

#------------------------------------------------------------------------------------#      
# 3. Add Metropolitan agglomeration codes to Municipal codes (GIN2018 database) ----
#------------------------------------------------------------------------------------#   
      
  # We match the 2018 agglomeration codes with the 2018 gemeente codes

  ## 3.1 Import GIN database ----
    gin2018 = read_spss(gin2018tab, col_select = c('gemeente', 'grstedagg'))
    setDT(gin2018)

  ## 3.2 Change type of variable for merge afterwards ----
    gin2018[, (colnames(gin2018)) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = colnames(gin2018)]
    gin2018[, (colnames(gin2018)) := lapply(.SD, function(x) (as.integer(x))), .SDcols = colnames(gin2018)]

  ## 3.3 Merge ----
    add_admin = merge(add_admin,gin2018, by.x = 'gem2018', by.y = 'gemeente', all.x = TRUE)

  ## 3.4 Check ----
    # We check with gem2021 because it is the last year available with income information. 
    # We assume that the agglomerations in 2018 remained the same in 2021.   
    tab = unique(add_admin[,c('gem2021','grstedagg')], by = c('gem2021','grstedagg'))

    # Some objects have a missing value in gem2018 but are from Rotterdam in 2021. We input the agglomeration of Rotterdam (14) to these objects
    add_admin[gem2021 == 599 & is.na(grstedagg), grstedagg := 14]

    # Some objects are in one municipality in 2018 (corresponding to one agglomeration) but in other in 2021 (in another agglomeration). We follow the gem2021 code to define the agglomeration code in these cases.  
    add_admin[gem2021 == 1621 & gem2018 == 599 & grstedagg == 14, grstedagg := 0]   
    add_admin[gem2021 == 1945 & gem2018 == 268 & grstedagg == 7, grstedagg := 0] 
    add_admin[gem2021 == 1949 & gem2018 == 80 & grstedagg == 2, grstedagg := 0]  

#-------------------------#      
# 4. Yearly database ----
#-------------------------#   
    
    pb = txtProgressBar(min = 0 , max = length(start_year:end_year), style = 3)
    
    # Loop through each year  
    for (year in start_year:end_year) {
      
      # Print year
      print(year)
      
      ## 4.1 Define the variable from the vslgwbtab (spatial unit codes) to merge ----
        # We use the codes from that year and the codes from last year (with income available) as a fixed municipality code
        if (year != 2021) {
          col_names = c(paste0("gem",year),paste0("wc",year),paste0("bc",year),"gem2021","wc2021","bc2021", "grstedagg")  
        } else {
          col_names = c(paste0("gem",year),paste0("wc",year),paste0("bc",year), "grstedagg") 
        }
      
      ## 4.2 Merge grid info with admin. spatial unit codes by year ----
      df = merge(grid, add_admin[,c('OBJ_ID',col_names), with = FALSE], by='OBJ_ID')
      
      ## 4.4 Relevant variables ----
      cols <- c(# Object ID
                  'OBJ_ID',
                # GRID ID
                  'VIERKANT100M','VIERKANT500M',
                # Admin. spatial units
                  col_names
                )
      
      df <- df[, ..cols, with = FALSE]
      
      setnames(df, c('grstedagg'),
                   c('METAGGL'))
      
      ## 4.5 Export ----
      save_file <- paste0("obj_spatial_",year,".csv")
      #fwrite(df, file = file.path(path_obj,save_file))
      
      ### Remove dataframe ----
      rm(df)
      
      # Update progress bar 
      setTxtProgressBar(pb, year - start_year + 1)
    }
close(pb)    
rm(grid, gin2018, add_admin)   