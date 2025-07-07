#------------------------------------------------------------------------------#
# Name: 1_grid_year.R
# Goal: Create yearly databases (1999-2023) with grid's aggregate info: demographic, socioeconomic and housing composition
# Date of creation: 27-11-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/ind/ind_[year].rds
# Output databases: data/grid/grid_[year].rds
#------------------------------------------------------------------------------#

# Loop
      
pb = txtProgressBar(min = 0 , max = length(start_year:inc_year), style = 3)
      
for (year in start_year:inc_year) { 
  
  print(year)
  
  #----------------------------------#
  # 1.Import individual database ----
  #----------------------------------#  
  
  ind_rds <- paste0("ind_",year,".rds")
  ind_df  <- read_rds(file.path(path_ind,ind_rds))
  setDT(ind_df)
  
  # Remove missing grid ID 
  ind_df <- ind_df[nchar(VIERKANT100M) == 10 & VIERKANT100M != '----------',]
  
  # Household database 
  hh_df <- ind_df[HH_REFPER == 1,]

  # Object database 
  obj_df <- ind_df[, .(VIERKANT100M = first(VIERKANT100M), 
                       METAGGL = first(METAGGL), 
                       OBJ_RENTED   = first(OBJ_RENTED)), by = OBJ_ID ]

  #-------------------------#
  # 2. Total population ----
  #-------------------------#
  
  # Individuals  
  grid_df <- ind_df[, .(TOT_IND = .N, GEM2021 = min(GEM2021), METAGGL = min(METAGGL)), by = .(VIERKANT100M) ]
  
  #-------------------#
  # 3. Age groups ----
  #-------------------#    
    
    # Population: All individuals 
  
    ## 3.1 Counts ----
      aux <- dcast(ind_df, VIERKANT100M ~ AGE_GROUP, fun.aggregate = length)
      setnames(aux, c('1','2','3','4','5','6','7'), paste0('AGE_GROUP_',1:7), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      rm(aux)
    
    ## 3.2 Percentages ----  
      cols <- c(paste0('AGE_GROUP_',1:7))
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/TOT_IND)), .SDcols = cols ]
      
  #--------------------------#
  # 4. Ethnic background ----
  #--------------------------#    
    
    # Population: All individuals
    # CBS variable: 1 NL; 2 Europe; 3 Turkey; 4 Morocco; 5 Suriname; 6 Dutch Caribbean; 7 Indonesia; 8 Other Africa; 9 Other Asia; 10 Other America and Oceania

    ## 4.1 Counts ----
      aux <- dcast(ind_df, VIERKANT100M ~ ETHNEWCBS, fun.aggregate = length)
      setnames(aux, c('1','2','3','4','5','6','7', '8','9','10'), c('ETHNL','ETHEUR','ETHTUR',
                                                                         'ETHMOR', 'ETHSUR', 'ETHCAR', 'ETHINDO',
                                                                         'ETHOAF', 'ETHOAS', 'ETHOAM'), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      rm(aux)
      
      ## 4.2 Percentages ----  
      cols <- c('ETHNL','ETHEUR','ETHTUR',
                'ETHMOR', 'ETHSUR', 'ETHCAR', 'ETHINDO',
                'ETHOAF', 'ETHOAS', 'ETHOAM')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(ETHNL + ETHEUR +
                                                       ETHTUR + ETHMOR + ETHSUR + 
                                                       ETHCAR + ETHINDO +
                                                       ETHOAF + ETHOAS + ETHOAM))), .SDcols = cols ]     

  #-------------------------------------------#
  # 5. Low/high income private households ----
  #-------------------------------------------#   
    
    # Population: Private non-student households with low or high household income.
      # Private non-student household: Only one reference person who is not a student that year (PRIV_HH == 1 & HHREFPER == 1)
      # Household income: Sum of income from work, social benefits and pensions of people who are not students
      
    ## 5.1 Counts ----
      # Number of private and non-private households 
        aux <- dcast(hh_df, VIERKANT100M ~ PRIV_HH, fun.aggregate = length)
        setnames(aux, c('0','1'), c('HH_NOPRIV','HH_PRIV'), skip_absent = TRUE)
        grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
        rm(aux)
      
      # Households 
        grid_df[, AUX_TOT_HH := HH_NOPRIV + HH_PRIV]
        grid_df[, TOT_HH := sum(AUX_TOT_HH), by = VIERKANT100M]
        grid_df[, AUX_TOT_HH := NULL]

      # Household income by quantile
        hh_df[PRIV_HH == 1 & is.na(HH_INCOME_QUANT), HH_INCOME_QUANT := 6]
        aux <- dcast(hh_df, VIERKANT100M ~ HH_INCOME_QUANT, fun.aggregate = length)
        setnames(aux, c('NA','1', '2', '3', '4', '5','6'), c('HH_NOPRIV_INC','HH_INC_Q1','HH_INC_Q2','HH_INC_Q3','HH_INC_Q4','HH_INC_Q5','HH_MISSINGINC'), skip_absent = TRUE)
        grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
        rm(aux)
        
    ## 5.2 Percentages ----  
        cols <- c('HH_INC_Q1','HH_INC_Q2','HH_INC_Q3','HH_INC_Q4','HH_INC_Q5')
        names <- paste("PERC", cols, sep = "_")
        grid_df[, (names) := lapply(.SD, function(x) (x/(HH_INC_Q1 + HH_INC_Q2 + HH_INC_Q3 + HH_INC_Q4 + HH_INC_Q5))), .SDcols = cols ]
        
        cols <- c('HH_MISSINGINC')
        names <- paste("PERC", cols, sep = "_")
        grid_df[, (names) := lapply(.SD, function(x) (x/(HH_MISSINGINC + HH_INC_Q1 + HH_INC_Q2 + HH_INC_Q3 + HH_INC_Q4 + HH_INC_Q5))), .SDcols = cols ]
        
        cols <- c('HH_PRIV')
        names <- paste("PERC", cols, sep = "_")
        grid_df[, (names) := lapply(.SD, function(x) (x/(HH_NOPRIV + HH_PRIV))), .SDcols = cols ]
        
  #-------------------------#
  # 6. Social benefits ----
  #-------------------------#   
      
    # Population: Working age non-students individuals who received social benefits at least one month during the year
      
    ## 6.1 Counts ----
      ind_df[WORK_AGE == 1 & (STUD == 0 | is.na(STUD)), aux := ifelse(MONTHS_SOCBENEF >= 1 & MONTHS_SOCBENEF <= 12, 1, 
                                                                      ifelse(MONTHS_SOCBENEF == 0 | is.na(MONTHS_SOCBENEF), 0, NA))]
      ind_df[WORK_AGE == 1 & (STUD == 0 | is.na(STUD)) & is.na(MONTHS_SOCBENEF), aux := 0]
      aux <- dcast(ind_df, VIERKANT100M ~ aux, fun.aggregate = length)
      setnames(aux, c('NA','0','1'), c('MISSINGSOCBENEF','NOSOCBENEF', 'SOCBENEF'), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      ind_df[, aux := NULL]
      rm(aux)
      
    ## 6.2 Percentages ----  
      cols <- c('NOSOCBENEF','SOCBENEF')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(NOSOCBENEF + SOCBENEF))), .SDcols = cols ]
      cols <- c('MISSINGSOCBENEF')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(MISSINGSOCBENEF + NOSOCBENEF + SOCBENEF))), .SDcols = cols ]
   
  #------------------#
  # 7. Students ----
  #------------------#   
      
    # Population: Individuals (>18) who studied at least one month during the year
      
    ## 7.1 Counts ----
      ind_df[, STUD_18 := 0]
      ind_df[STUD == 1 & AGE_GROUP > 1, STUD_18 := 1]
      aux <- dcast(ind_df, VIERKANT100M ~ STUD_18, fun.aggregate = length)
      setnames(aux, c('0','1'), c('NOSTUDENT', 'STUDENTS'), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      rm(aux)
      
    ## 7.2 Percentages ----  
      cols <- c('STUDENTS')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(NOSTUDENT + STUDENTS))), .SDcols = cols ]
      
  #---------------------------#
  # 8. Type of household ----
  #---------------------------#    
    
    # Population: Private households (CBS definition) (HHREFPER == 1)
    # 1: Single, 2: Couple without children, 3: Couple with children, 4: Single-parent households, 5: Others
    ## 8.1 Counts ----
      hh_df[, HH_TYPE := ifelse(HH_TYPE == 1, 1,
                                ifelse(HH_TYPE == 2 | HH_TYPE == 3, 2, 
                                       ifelse(HH_TYPE == 4 | HH_TYPE == 5, 3,
                                              ifelse(HH_TYPE == 6, 4, 
                                                     ifelse(HH_TYPE == 7, 5, 
                                                            ifelse(HH_TYPE == 8, 6, NA))))))]
      aux <- dcast(hh_df, VIERKANT100M ~ HH_TYPE, fun.aggregate = length)
      setnames(aux, c('1','2','3','4','5'), paste0('HH_TYPE_',1:5), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      rm(aux)
      
    ## 8.2 Percentages ----  
      cols <- c(paste0('HH_TYPE_',1:5))
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(HH_TYPE_1 + HH_TYPE_2 + HH_TYPE_3 + HH_TYPE_4 + HH_TYPE_5 ))), .SDcols = cols ]   
    
  #----------------#
  # 9. Tenure ----
  #----------------#   
        
    # Population: All residential address objects.
        
    ## 9.1 Counts ----
      aux <- dcast(obj_df, VIERKANT100M ~ OBJ_RENTED, fun.aggregate = length)
      setnames(aux, c('NA','0','1'), c('OBJ_MISSINGTENURE','OBJ_OWNED', 'OBJ_RENTED'), skip_absent = TRUE)
      grid_df <- merge(grid_df, aux, by = 'VIERKANT100M', all.x = TRUE)
      rm(aux)
          
    ## 9.2 Percentages ----  
      cols <- c('OBJ_OWNED', 'OBJ_RENTED')
      names <- paste("PERC", cols, sep = "_")
      grid_df[, (names) := lapply(.SD, function(x) (x/(OBJ_OWNED + OBJ_RENTED))), .SDcols = cols ]

        
  #------------------------------------#
  # 10. Export table of attributes ----
  #------------------------------------#  
    
    save_file <- paste0("grid_",year,".rds")
    write_rds(grid_df, file = file.path(path_grid,save_file), compress = "gz")   
    rm(grid_df, ind_df, hh_df, obj_df)

    # Update progress bar 
    setTxtProgressBar(pb, year - start_year + 1)
}
close(pb)