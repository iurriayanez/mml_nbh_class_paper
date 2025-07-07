#------------------------------------------------------------------------------#
# Name: 5_ind_year.R
# Goal: Create yearly databases (1999-2023) with individual's info: spatial (location and housing), demographic, educational and economic.
# Date of creation: 20-11-2023 

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/ind/ind_spatial_[year].csv ; ind_demo_[year].csv ; ind_inc_[year].csv ; ind_educ_[year].csv
# Output databases: data/ind/ind_[year].rds
#------------------------------------------------------------------------------#

# Import CPI database (European Harmonized Consumer Price Index 2015 = 100)
  cpi <- fread(cpitab)
  
# Loop
  
for (year in start_year:end_year) { 
  
  print(year)
  
  #---------------------------------------#
  # 1. Merge spatial and demographics ----
  #---------------------------------------# 
  
    ## 1.1 Import spatial database ----
      spa_csv <- paste0("ind_spatial_",year,".csv")
      ind_spa <- fread(file.path(path_ind,spa_csv))
      ind_spa[, IND_ID := sprintf("%09d%s",as.numeric(substring(IND_ID,1, nchar(IND_ID)-1)),substr(IND_ID,nchar(IND_ID),nchar(IND_ID)))]
      
    ## 1.2 Import demographic database ----
      demo_csv <- paste0("ind_demo_",year,".csv")
      ind_demo <- fread(file.path(path_ind,demo_csv))      
      ind_demo[, IND_ID := sprintf("%09d%s",as.numeric(substring(IND_ID,1, nchar(IND_ID)-1)),substr(IND_ID,nchar(IND_ID),nchar(IND_ID)))]
      
    ## 1.3 Merge ---- 
      df <- merge(ind_spa, ind_demo, by = 'IND_ID', all.x = TRUE)
      rm(ind_spa, ind_demo)
    
  #---------------------------------------------------#
  # 2. Merge with income and socioeconomic status ----
  #---------------------------------------------------#
    
    ## 2.1 Import and merge income and socioeconomic status database ----    
      inc_csv <- paste0("ind_inc_",year,".csv")
      sec_csv  <- paste0("ind_sec_",year,".csv")
      
      if (file.exists(file.path(path_ind,inc_csv)) == TRUE) {
        ind_inc <- fread(file.path(path_ind,inc_csv))
        ind_inc[, IND_ID := sprintf("%09d%s",as.numeric(substring(IND_ID,1, nchar(IND_ID)-1)),substr(IND_ID,nchar(IND_ID),nchar(IND_ID)))]
        df <- merge(df, ind_inc, by = 'IND_ID', all.x = TRUE)
        rm(ind_inc)
      }
      
      if (file.exists(file.path(path_ind,sec_csv)) == TRUE) {
        ind_sec <- fread(file.path(path_ind,sec_csv))
        ind_sec[, IND_ID := sprintf("%09d%s",as.numeric(substring(IND_ID,1, nchar(IND_ID)-1)),substr(IND_ID,nchar(IND_ID),nchar(IND_ID)))]
        df <- merge(df, ind_sec, by = 'IND_ID', all.x = TRUE)      
        rm(ind_sec)
      }

  #-----------------------------#        
  # 3. Additional variables ---- 
  #-----------------------------#   
    
    if (file.exists(file.path(path_ind,inc_csv)) == TRUE) {
      # These variables can only be created with the income variable, so we need to check that the file exists for that year
    
      ## 3.1 Remove students (we don't count their income)
        df[, STUD := as.integer(MONTHS_STUD>= 1 & MONTHS_STUD<=12 & !is.na(MONTHS_STUD))]

        cols <- c('GROSS_ANNUAL_INCOME', 'GROSS_ANNUAL_INCOME_WORK', 'GROSS_ANNUAL_INCOME_WORK_SB')
        df[, (cols) := lapply(.SD, function(x) (ifelse(STUD == 1, NA, x))), .SDcols = cols]
        
      ## 3.2 Working age ---- 
        df[, WORK_AGE := as.integer(AGE >= 20 & AGE<= 65)]
  
      ## 3.3 Household income ---- 
        # Sum of individual income from work, social benefits and pensions of people who are not students
          aux = df[!is.na(GROSS_ANNUAL_INCOME) & !is.na(HH_REFPER),
                   .(HH_GROSS_ANNUAL_INCOME = sum(GROSS_ANNUAL_INCOME)), by = HH_ID]
          df <- merge(df, aux, by = 'HH_ID', all.x = TRUE)
          rm(aux)
        
        # Equivalised household income (CBS equivalence factors)
          # Number of adults and children 
            df[, ADULTS := sum(AGE >= 18, na.rm = TRUE), by = 'HH_ID']
            df[, KIDS   := sum(AGE <  18, na.rm = TRUE), by = 'HH_ID']

          # Equivalence factors
            if (year > 2017) {
              df[, EQ_FACTOR := fcase(
                ADULTS == 1 & KIDS == 0 , 1,
                ADULTS == 2 & KIDS == 0 , 1.4,
                ADULTS == 3 & KIDS == 0 , 1.78,
                ADULTS == 4 & KIDS == 0 , 2.02,
                ADULTS == 1 & KIDS == 1 , 1.32,
                ADULTS == 2 & KIDS == 1 , 1.69,
                ADULTS == 3 & KIDS == 1 , 2.00,
                ADULTS == 4 & KIDS == 1 , 2.19,
                ADULTS == 1 & KIDS == 2 , 1.52,
                ADULTS == 2 & KIDS == 2 , 1.91,
                ADULTS == 3 & KIDS == 2 , 2.16,
                ADULTS == 4 & KIDS == 2 , 2.37,
                ADULTS == 1 & KIDS == 3 , 1.73,
                ADULTS == 2 & KIDS == 3 , 2.09,
                ADULTS == 3 & KIDS == 3 , 2.32,
                ADULTS == 4 & KIDS == 3 , 2.53,
                ADULTS == 1 & KIDS == 4 , 1.93,
                ADULTS == 2 & KIDS == 4 , 2.28,
                ADULTS == 3 & KIDS == 4 , 2.49,
                ADULTS == 4 & KIDS == 4 , 2.68,
                ADULTS >  4  | KIDS > 4 , (ADULTS + KIDS*0.8)^0.5
              )]
            } else {
              df[, EQ_FACTOR := fcase(
                ADULTS == 1 & KIDS == 0 , 1,
                ADULTS == 2 & KIDS == 0 , 1.37,
                ADULTS == 3 & KIDS == 0 , 1.73,
                ADULTS == 4 & KIDS == 0 , 2.00,
                ADULTS == 1 & KIDS == 1 , 1.33,
                ADULTS == 2 & KIDS == 1 , 1.67,
                ADULTS == 3 & KIDS == 1 , 1.95,
                ADULTS == 4 & KIDS == 1 , 2.19,
                ADULTS == 1 & KIDS == 2 , 1.51,
                ADULTS == 2 & KIDS == 2 , 1.88,
                ADULTS == 3 & KIDS == 2 , 2.14,
                ADULTS == 4 & KIDS == 2 , 2.37,
                ADULTS == 1 & KIDS == 3 , 1.76,
                ADULTS == 2 & KIDS == 3 , 2.06,
                ADULTS == 3 & KIDS == 3 , 2.32,
                ADULTS == 4 & KIDS == 3 , 2.53,
                ADULTS == 1 & KIDS == 4 , 1.95,
                ADULTS == 2 & KIDS == 4 , 2.28,
                ADULTS == 3 & KIDS == 4 , 2.49,
                ADULTS == 4 & KIDS == 4 , 2.68,
                ADULTS >  4  | KIDS > 4 , (ADULTS + KIDS*0.8)^0.5
              )]
            }
            
          # Equivalised income 
            df[, HH_EQ_GROSS_ANNUAL_INCOME := (HH_GROSS_ANNUAL_INCOME / EQ_FACTOR)]
            
            # There are some private households with EQ_FACTOR missing, but it is because these are households of underage individuals (not considering them).
            
      ## 3.4 Inflation correction ----    
        # CPI value of that year (European Harmonized Consumer Price Index 2015 = 100)
          cpi_val <- as.numeric(cpi[YEAR == year, 2]/100)
          
        # Correct for inflation 
          df[, GROSS_ANNUAL_INCOME_CPI       := GROSS_ANNUAL_INCOME * cpi_val]
          df[, GROSS_ANNUAL_INCOME_WORK_CPI  := GROSS_ANNUAL_INCOME_WORK * cpi_val]
          df[, HH_GROSS_ANNUAL_INCOME_CPI    := HH_GROSS_ANNUAL_INCOME * cpi_val]
          df[, HH_EQ_GROSS_ANNUAL_INCOME_CPI := HH_EQ_GROSS_ANNUAL_INCOME * cpi_val]

      ## 3.5 Low and high income ---- 
        
        # Sample selection for individual national distribution: working age individuals (between 20 and 65 years old) who did not study that year
        # Sample selection for household national distribution: private households - reference person must not be a student (HH_REFPER == 1 & STUD != 1)
          
        # Private households: Must have one reference person and that reference person must not be a student
          df[, aux := sum(HH_REFPER == 1 & (STUD == 0 | is.na(STUD)), na.rm = TRUE), by = HH_ID]
          df[, PRIV_HH := as.integer(aux == 1)]
          df[, aux := NULL]
          
        # Income quintiles: 
          df[WORK_AGE == 1, INCOME_QUANT := cut(df[WORK_AGE == 1, GROSS_ANNUAL_INCOME_WORK_SB], 
                                                quantile(df[WORK_AGE == 1, GROSS_ANNUAL_INCOME_WORK_SB], probs = seq(0, 1, 0.2), na.rm = TRUE), 
                                                labels = FALSE, include.lowest = TRUE)]
          
          df[PRIV_HH == 1, HH_INCOME_QUANT := cut(df[PRIV_HH == 1, HH_EQ_GROSS_ANNUAL_INCOME], 
                                                  quantile(df[PRIV_HH == 1, HH_EQ_GROSS_ANNUAL_INCOME], probs = seq(0, 1, 0.2), na.rm = TRUE), 
                                                         labels = FALSE, include.lowest = TRUE)]
          
        # Low income: Income below the 40% of the median of the national distribution (risk of poverty)
          median_value_ind <- median(df[WORK_AGE == 1, GROSS_ANNUAL_INCOME_WORK_SB], na.rm = TRUE)    # Earned income
          median_value_hh  <- median(df[PRIV_HH == 1 , HH_EQ_GROSS_ANNUAL_INCOME], na.rm = TRUE)      # Household income (ref person must not be a student)
          
          df[WORK_AGE == 1, LOWINC := as.integer(GROSS_ANNUAL_INCOME_WORK_SB <= (0.4 * median_value_ind))]
          df[PRIV_HH == 1,HH_LOWINC := as.integer(HH_EQ_GROSS_ANNUAL_INCOME <= (0.4 * median_value_hh))]
          
        # High income: Income above the 75th percentile of the income distribution      
          p75_ind <- quantile(df[WORK_AGE == 1, GROSS_ANNUAL_INCOME_WORK_SB], probs = 0.75, na.rm = TRUE)    # Earned income
          p75_hh  <- quantile(df[PRIV_HH == 1 , HH_EQ_GROSS_ANNUAL_INCOME], probs = 0.75, na.rm = TRUE)      # Household income
          
          df[WORK_AGE == 1, HIGHINC := as.integer(GROSS_ANNUAL_INCOME_WORK_SB >= p75_ind)]
          df[PRIV_HH == 1,HH_HIGHINC := as.integer(HH_EQ_GROSS_ANNUAL_INCOME >= p75_hh)]
    }
        
    ## 3.6 Age groups ---- 
      breaks <- c(-1,18,25,35,45,55,65,Inf)
      df[, AGE_GROUP :=  cut(AGE, breaks = breaks, labels = FALSE)]

  #---------------#        
  # 4. Export ---- 
  #---------------# 
    
    setnames(df, toupper(names(df)))  
    save_file <- paste0("ind_",year,".rds")
    write_rds(df, file = file.path(path_ind,save_file), compress = "gz")  
      
  #-----------------------------------------------------#        
  # 5. Remove intermediate databases from directory ---- 
  #-----------------------------------------------------#            
    
    ## 5.1 Spatial ----  
      csv_file <- paste0("ind_spatial_",year,".csv")
      #file.remove(file.path(path_ind,csv_file))

    ## 5.2 Demo ----  
      csv_file <- paste0("ind_demo_",year,".csv")
      #file.remove(file.path(path_ind,csv_file))
      
    ## 5.3 Econ ----  
      csv_file <- paste0("ind_inc_",year,".csv")
      if (file.exists(file.path(path_ind,csv_file)) == TRUE) {
        #file.remove(file.path(path_ind,csv_file))
      }
      
      csv_file <- paste0("ind_sec_",year,".csv")
      if (file.exists(file.path(path_ind,csv_file)) == TRUE) {
        #file.remove(file.path(path_ind,csv_file))
      }

    ## 5.4 Educ ----  
      csv_file <- paste0("ind_educ_",year,".csv")
      #file.remove(file.path(path_ind,csv_file))
      
    ## 5.5 Remove dataframe from memory to restart with the next year ----
      rm(df)     
} 
   