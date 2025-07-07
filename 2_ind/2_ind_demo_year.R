#------------------------------------------------------------------------------#
# Name: 2_ind_demo_year_[version]
# Goal: Create yearly databases (1999-2023) with individual's demographic info: gender, age and migration background.
# Date of creation: 06-10-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases:  GBAPERSOONTAB ; LANDAKTUEELREF ; GBAHUISHOUDENSBUS
# Output databases: data/ind/ind_spa_demo_econ_educ/ind_demo_[year].csv
#------------------------------------------------------------------------------#

#-----------------------------------------------------------# 
# 1. Individual demographic information (GBAPERSOONTAB) ----
#-----------------------------------------------------------# 

  ## 1.1 Import the database ----
    ind_demo = read_spss(gbapersoontab, 
                         col_select = c('RINPERSOON','RINPERSOONS', 
                                        'GBAGEBOORTELANDNL', 'GBAGESLACHT',
                                        'GBAAANTALOUDERSBUITENLAND', 'GBAGENERATIE',
                                        'GBAGEBOORTEJAAR','GBAGEBOORTEMAAND','GBAGEBOORTEDAG',
                                        'GBAHERKOMSTLAND'
                                        ))
    
    setDT(ind_demo)
    cols = c('RINPERSOON','RINPERSOONS', 
             'GBAGEBOORTELANDNL', 'GBAGESLACHT',
             'GBAAANTALOUDERSBUITENLAND', 'GBAGENERATIE',
             'GBAGEBOORTEJAAR','GBAGEBOORTEMAAND','GBAGEBOORTEDAG',
             'GBAHERKOMSTLAND')
    ind_demo[, (cols) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols]  
    
    cols = c('GBAHERKOMSTLAND','GBAGESLACHT')
    ind_demo[, (cols) := lapply(.SD, function(x) (as.numeric(x))), .SDcols = cols] 
    
  ## 1.2 ID ----
    # Persons
    ind_demo[, IND_ID := paste0(RINPERSOON, RINPERSOONS)] 
    ind_demo[, (c('RINPERSOON','RINPERSOONS')) := NULL]
    
  ## 1.3 Check data ----
    summary(ind_demo)                                                              # General info
    any(duplicated(ind_demo, by= 'IND_ID'))                  # No duplicates
    
  ## 1.4 Gender ----
    ind_demo[, FEMALE := ifelse(GBAGESLACHT == 1, 0, ifelse(GBAGESLACHT == 2, 1, NA))]
    ind_demo[, GBAGESLACHT := NULL ]
  
  ## 1.5 Birth date 
    ind_demo[, BIRTHDATE := as.Date(paste0(GBAGEBOORTEJAAR,"-",GBAGEBOORTEMAAND, "-", GBAGEBOORTEDAG))]

#--------------------------------------------# 
# 2. Ethnic background (LANDAKTUEELREF) -----
#--------------------------------------------#     
 
  ## 2.1 Import conversion file -----
    cols = c(# Country code
             'LAND', 
             # Western/Non-western (old classification) 
             'LANDTYPE',
             # New classification (10 groups)
             'LANDTIENDELING'
    )
    ethnic_ref = read_spss(landaktueelref,
                           col_select = all_of(cols))
    ethnic_ref = setDT(ethnic_ref)
  
  ## 2.2 Check data ----
    summary(ethnic_ref)                                                           # General info
    any(duplicated(ethnic_ref, by= 'LAND'))                                       # No duplicates
 
#-----------------------------------------------------------------------#     
# 3. Merge individual demographic info with ethnic group references ----
#-----------------------------------------------------------------------#    
    
    # The key variable is the country of origin in the individual demographic information database (GBAPERSOONTAB): GBAHERKOMSTLAND
    # and the country variable in the ethnic background reference classification database (LANDAKTUEELREF): LAND
    
  ## 3.1 Select columns from ethnic background reference classification database ----
      # As numeric values
      ethnic_ref[, (cols) := lapply(.SD, function(x) (as.numeric(zap_labels(x)))), .SDcols = cols]
      summary(ethnic_ref)
    
  ## 3.2 Merge ----
      ind_demo = merge(ind_demo, ethnic_ref, by.x = "GBAHERKOMSTLAND", by.y = "LAND", all.x = TRUE)
    
  ## 3.3 Check ----
      table(ind_demo$GBAGEBOORTELANDNL, ind_demo$LANDTIENDELING)
      table(ind_demo$GBAAANTALOUDERSBUITENLAND, ind_demo$LANDTIENDELING)
    
  ## 3.4 Creation of ethnic background ---- 
    # LANDTYPE (old classification): 1 Western background, 2 Non-western background
      ind_demo[, ETHNONWEST := ifelse(LANDTYPE == 1, 0, ifelse(LANDTYPE == 2, 1, NA))]
    # LANDTIENDELING (new classification): 1 NL, 2 Europe (excluding NL), 3 Turkey, 4 Morocco, 5 Surinam, 6 Former Dutch Caribbean, 7 Indonesia, 8 Other Africa, 9 Other Asia, 10 Others America and Oceania
    
  ## 3.5 Relevant variables ---- 
      cols = c(# ID
                "IND_ID",
                # Gender
                "FEMALE",
                # Date of birth 
                "BIRTHDATE",
                # Country of origin and ethnic background classification (old and new and ours)
                "GBAHERKOMSTLAND", "ETHNONWEST", "LANDTIENDELING",
                # MIGRATION GENERATION
                'GBAGENERATIE'
                )
      
      ind_demo = ind_demo[,cols, with = FALSE]
      setnames(ind_demo, c('GBAHERKOMSTLAND','LANDTIENDELING','GBAGENERATIE'), c('COUNTRYORIGIN','ETHNEWCBS','MIGGEN'))

#------------------------------------------------------#       
# 4. Add household information (GBAHUISHOUDENSBUS) ----
#------------------------------------------------------#   
      
  ## 4.1 Import the database ----
    ind_hh = read_spss(gbahuishoudens, 
                       col_select = c('RINPERSOON','RINPERSOONS','HUISHOUDNR','DATUMAANVANGHH','DATUMEINDEHH','TYPHH','REFPERSOONHH'))
    setDT(ind_hh)
    
  ## 4.2 Date format ----
    cols = c('DATUMAANVANGHH', 'DATUMEINDEHH')
    ind_hh[, (cols) := lapply(.SD, function(x) as.IDate(as.character(x), "%Y%m%d")), .SDcols = cols]
             
  ## 4.3 ID ----
    # Persons
    ind_hh[, IND_ID := paste0(RINPERSOON, RINPERSOONS)] 
    ind_hh[, (c('RINPERSOON','RINPERSOONS')) := NULL]
    
    # To obtain a unique HH we need two variables: 'HUISHOUDNR' and 'DATUMAANVANGHH' 
    ind_hh[, HH_ID := as.character(paste(HUISHOUDNR, as.numeric(DATUMAANVANGHH), sep = "_"))] 
    ind_hh[, (c('HUISHOUDNR')) := NULL]

#----------------------------------------------#       
# 5. Yearly database with demographic info ----
#----------------------------------------------#
  
  for (year in start_year:end_year) {    
  
    print(year)
    
  ## 5.1 Select household type on Januart 1st of that year ----
    
    df = ind_hh[
      
      # Start date before or on January 1st 
      ind_hh$DATUMAANVANGHH <= as.IDate(paste0(year, "0101"),"%Y%m%d") &               
      
      # End date after or on January 1st
      ind_hh$DATUMEINDEHH >= as.IDate(paste0(year, "0101"),"%Y%m%d")
    
    ]
    
  ## 5.2 Merge with demographic database ---- 
    ind_demo_final = merge(ind_demo, df[, c('IND_ID','HH_ID','TYPHH','REFPERSOONHH')], by = 'IND_ID', all.x = TRUE)
    setnames(ind_demo_final,c('TYPHH','REFPERSOONHH'),c('HH_TYPE','HH_REFPER'))
    
  ## 5.3 Individual age on January 1st that year ---- 
    ind_demo_final[, AGE := floor(as.numeric(difftime(as.Date(paste0(year, "-01-01")),
                                                              BIRTHDATE, units = "days")) / 365.25)]
    
  ## 5.4 Export ----
    save_file <- paste0("ind_demo_",year,".csv")
    fwrite(ind_demo_final, file = file.path(path_ind,save_file))
    
    # Remove dataframe 
    rm(ind_demo_final, df)
  } 
    
# Remove remaining dataframes    
rm(ind_demo, ethnic_ref, ind_hh)      