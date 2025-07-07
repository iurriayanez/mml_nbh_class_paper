#------------------------------------------------------------------------------#
# Name: 2_obj_tenure_year.R
# Goal: Create yearly databases (1999-2023) with address object information: tenure and WOZ values
# Date of creation: 04-10-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases:  OBJECTWONINGTAB ; EIGENDOMWOZTAB ; EIGENDOMWOZBAGTAB ; BAGWOZTAB ; EIGENDOMTAB
# Output databases: data/obj/obj_tenure_[year].csv
#------------------------------------------------------------------------------#

  # TENURE: 
  # 1999 - 2005: OBJECTWONINGTAB
  # 2006 - 2011: EIGENDOMWOZTAB
  # 2012 - 2023: EIGENDOMTAB
  
  pb = txtProgressBar(min = 0 , max = length(start_year:end_year), style = 3)
  
  #----------------------------#      
  # 1. Tenure - 1999-2005  ---- 
  #----------------------------# 

  for (year in 1999:2005) { 
    
    # TENURE in OBJECTWONINGTAB
    
    ## 1.1 Import object info ----
      
        # Import
          df = read_spss(file.path(objectwoningtab,year,paste0("140924 OBJECTWONINGTAB ", year, "V1.sav")))
          
        # Set relevant columns
          setnames(df, toupper(names(df))) 
          cols = c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER','HUURKOOP')
          
        # Data.table format 
          df = setDT(df)
          
        # Remove labels    
          df[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]  
        
        # Set names 
          setnames(df,cols,
                   c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER','OBJ_RENTED')
          )
          
    ## 1.2 Recode variables ----
      
      # ID: Objects (address)
        df[, OBJ_ID := paste0(RINOBJECTNUMMER, SOORTOBJECTNUMMER)] 
        df[, (c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')) := NULL]

      # Tenure: 1 rented, 0 owned
        df[, OBJ_RENTED := as.integer(OBJ_RENTED)]
        df[, OBJ_RENTED := as.integer(ifelse(OBJ_RENTED == 1, 0, ifelse(OBJ_RENTED == 2, 1, NA)))]
          
    ## 1.3 Export ----
      save_file <- paste0("obj_tenure_",year,".csv")
      fwrite(df, file = file.path(path_obj,save_file))
          
      # Remove dataframe 
      rm(df) 
      
      # Update progress bar 
      setTxtProgressBar(pb, year - 1999 + 1)
  }
  

  #----------------------------#      
  # 2. Tenure - 2006-2011  ---- 
  #----------------------------# 
  
  for (year in 2006:2011) { 
    
    # TENURE in EIGENDOMWOZTAB
    
    ## 2.1 Import object info ----
      
      df = read_spss(file.path(eigendomwoztab,year,paste0("140930 EIGENDOMWOZTAB", year, "V1.sav")),
                     col_select = c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER', 'VBOEIGENDOM','WOZGEBRUIKSCODE', 'WOONRUIMTECODE'))
      
      # Set relevant columns
        setnames(df, toupper(names(df))) 
        cols = c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER', 'VBOEIGENDOM', 'WOZGEBRUIKSCODE', 'WOONRUIMTECODE')
        
      # Data.table format 
        df = setDT(df)
      
      # Remove labels    
        df[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]  
      
      # Set names 
        setnames(df,cols,
                 c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER','OBJ_RENTED', 'OBJ_HOME', 'OBJ_HOME2')
        )
    
    ## 2.2 Recode variables ----
      
      # ID: Objects (address)
        df[, OBJ_ID := paste0(RINOBJECTNUMMER, SOORTOBJECTNUMMER)] 
        df[, (c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')) := NULL]
        
      # Tenure: 1 rented, 0 owned  
        df[, OBJ_RENTED := as.integer(ifelse(OBJ_RENTED == 'E', 0, ifelse(OBJ_RENTED == 'H', 1, NA)))]   
      
      # Type of dwelling: 1 Home, 0 Others (non-residential and farms)
        df[, OBJ_HOME := as.integer(OBJ_HOME)]
        df[, OBJ_HOME := as.integer(ifelse(OBJ_HOME == 10 | OBJ_HOME == 11 | OBJ_HOME == 12, 1, 
                                         ifelse(OBJ_HOME == 20 | OBJ_HOME == 21 |
                                                  OBJ_HOME == 30 | OBJ_HOME == 31 |
                                                  OBJ_HOME == 40, 0, NA)))]
        
        df[, OBJ_HOME2 := as.integer(OBJ_HOME2)]
        df[, OBJ_HOME2 := as.integer(ifelse(OBJ_HOME2 == 1 | OBJ_HOME2 == 4, 1, 
                                           ifelse(OBJ_HOME2 == 5 | OBJ_HOME2 == 6, 0, NA)))]
        
      # Keep only residential units 
        df = df[OBJ_HOME == 1 | OBJ_HOME2 == 1,]
        df[, c('OBJ_HOME', 'OBJ_HOME2') := NULL]
        
    ## 2.3 Export ----
      save_file <- paste0("obj_tenure_",year,".csv")
      fwrite(df, file = file.path(path_obj,save_file))
      
      # Remove dataframe 
        rm(df) 
        
      # Update progress bar 
        setTxtProgressBar(pb, year - 1999 + 1)
  } 
          
  
  #----------------------------#      
  # 3. Tenure - 2012-2018  ---- 
  #----------------------------# 
  
  for (year in 2012:2018) { 
    
    # Residential units defintion in EIGENDOMWOZBAGTAB
    # TENURE in EIGENDOMTAB
    
    ## 3.1 Residential unit info ----

      # Define the path 
       assign("eigendomwozbagtab", get(paste0("eigendomwozbagtab",year)), envir = .GlobalEnv)
        
      # Import
        cols = c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER', 'WOZGEBRUIKSCODEBAG')
        df = read_spss(eigendomwozbagtab) %>% 
          setnames(., toupper(names(.))) %>% 
          select(all_of(cols))
        
      # Data.table format 
        df = setDT(df)
        
      # Remove labels    
        df[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]  
        
      # Set names 
        setnames(df,cols,
                 c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER', 'OBJ_HOME')
        )
    
      # ID: Objects (address)
        df[, OBJ_ID := paste0(RINOBJECTNUMMER, SOORTOBJECTNUMMER)] 
        df[, (c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')) := NULL]
    
    ## 3.2 Import TENURE info ----
      # Define the path 
        assign("eigendomtab", get(paste0("eigendomtab",year)), envir = .GlobalEnv)
        
      # Import
        cols = c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER', 'TYPEEIGENDOM')
        df_eigen = read_spss(eigendomtab) %>% 
          setnames(., toupper(names(.))) %>% 
          select(all_of(cols))
        
      # Data.table format 
        df_eigen = setDT(df_eigen)
        
      # Remove labels    
        df_eigen[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]  
      
      # Set names 
        setnames(df_eigen,cols,
                 c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER','OBJ_RENTED')
        )
        
      # ID: Objects (address)
        df_eigen[, OBJ_ID := paste0(RINOBJECTNUMMER, SOORTOBJECTNUMMER)] 
        df_eigen[, (c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')) := NULL]
    
    ## 3.3 Residential info and TENURE info merged ----
      df = merge(df, df_eigen, by = 'OBJ_ID', all = TRUE)

    ## 3.4 Recode variables ----
      
      # Tenure: 1 rented, 0 owned  
      df[, OBJ_RENTED := as.integer(ifelse(OBJ_RENTED == 'E', 0, ifelse(OBJ_RENTED == 'H', 1, NA)))]   
      
      # Type of dwelling: 1 Home, 0 Others (non-residential and farms)
      df[, OBJ_HOME := as.integer(OBJ_HOME)]
      df[, OBJ_HOME := as.integer(ifelse(OBJ_HOME == 10 | OBJ_HOME == 11 | OBJ_HOME == 12, 1, 
                                         ifelse(OBJ_HOME == 20 | OBJ_HOME == 21 |
                                                  OBJ_HOME == 30 | OBJ_HOME == 31 |
                                                  OBJ_HOME == 40, 0, NA)))]
      
      
      # Keep only residential units
      # We assume missing is residential because in BAG all units are essentially residential. 
      # However, we still do some filtering because we try to avoid considering the WOZ value of a big object (health complex center with 1 VBO residential unit)
        df = df[OBJ_HOME == 1 | is.na(OBJ_HOME),]
        df[, c('OBJ_HOME') := NULL]
      
      ## 3.5 Export ----
        save_file <- paste0("obj_tenure_",year,".csv")
        fwrite(df, file = file.path(path_obj,save_file))
        
        # Remove dataframe 
        rm(df, df_eigen)
        
        # Update progress bar 
        setTxtProgressBar(pb, year - 1999 + 1)
  }        
 
  #----------------------------#      
  # 4. Tenure - 2019-2023  ---- 
  #----------------------------# 
  
  for (year in 2019:2023) { 
    
    # Residential unit info in BAGWOZTAB
    # TENURE in EIGENDOMTAB
    
    ## 4.1 Import residential info info ----
    
      # Define the path 
        assign("bagwoztab", get(paste0("bagwoztab",year)), envir = .GlobalEnv)
    
      # Import
        cols = c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER', 'GEBRUIKSCODEWOZ')
        df = read_spss(bagwoztab) %>% 
          setnames(., toupper(names(.))) %>% 
          select(all_of(cols))

      # Data.table format 
        df = setDT(df)
      
      # Remove labels    
        df[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]  
      
      # Set names 
        setnames(df,cols,
               c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER', 'OBJ_HOME')
        )
      
      # ID: Objects (address)
        df[, OBJ_ID := paste0(RINOBJECTNUMMER, SOORTOBJECTNUMMER)] 
        df[, (c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')) := NULL]
    
    ## 4.2 Import TENURE info ----
      # Define the path 
        assign("eigendomtab", get(paste0("eigendomtab",year)), envir = .GlobalEnv)
      
      # Import
        cols = c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER', 'TYPEEIGENDOM')
        df_eigen = read_spss(eigendomtab) %>% 
          setnames(., toupper(names(.))) %>% 
          select(all_of(cols))
      
      # Data.table format 
        df_eigen = setDT(df_eigen)
      
      # Remove labels    
        df_eigen[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]  
      
      # Set names 
        setnames(df_eigen,cols,
                 c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER','OBJ_RENTED')
        )
      
      # ID: Objects (address)
        df_eigen[, OBJ_ID := paste0(RINOBJECTNUMMER, SOORTOBJECTNUMMER)] 
        df_eigen[, (c('SOORTOBJECTNUMMER', 'RINOBJECTNUMMER')) := NULL]
    
    ## 4.3 WOZ and TENURE info merged ----
      df = merge(df, df_eigen, by = 'OBJ_ID', all = TRUE)
    
    
    ## 4.4 Recode variables ----
    
      # Tenure: 1 rented, 0 owned  
        df[, OBJ_RENTED := as.integer(ifelse(OBJ_RENTED == 'E', 0, ifelse(OBJ_RENTED == 'H', 1, NA)))]   

      # Type of dwelling: 1 Home, 0 Others (non-residential and farms)
        df[, OBJ_HOME := as.integer(OBJ_HOME)]
        df[, OBJ_HOME := as.integer(ifelse(OBJ_HOME == 10 | OBJ_HOME == 11 | OBJ_HOME == 12, 1, 
                                         ifelse(OBJ_HOME == 20 | OBJ_HOME == 21 |
                                                  OBJ_HOME == 30 | OBJ_HOME == 31 |
                                                  OBJ_HOME == 40, 0, NA)))]
    
    
      # Keep only residential units
      # We assume missing is residential because in BAG all units are essentially residential. 
      # However, we still do some filtering because we try to avoid considering the WOZ value of a big object (health complex center with 1 VBO residential unit)
        df = df[OBJ_HOME == 1 | is.na(OBJ_HOME),]
        df[, c('OBJ_HOME') := NULL]
    
    ## 4.5 Export ----
      save_file <- paste0("obj_tenure_",year,".csv")
      fwrite(df, file = file.path(path_obj,save_file))
      
      # Remove dataframe 
      rm(df, df_eigen)
      
      # Update progress bar 
      setTxtProgressBar(pb, year - start_year + 1)
  }          

close(pb)  
      