#------------------------------------------------------------------------------#
# Name: 3_ind_econ_year_[version]
# Goal: Create yearly databases (1999-2023) with individual's socioeconomic info: annual income and socioecon. status.
# Date of creation: 19-10-2023

# R Version: R version 4.2.3 (2023-03-15 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Author: Ignacio Urria Yanez

# Input databases:  SECM databases
# Output databases: data/ind/ind_sec_[year].csv; data/ind/ind_inc_[year].csv
#------------------------------------------------------------------------------#

#--------------------------------------#
# 1.Socioeconomic status (SECMBUS) ----
#--------------------------------------#

  ## 1.1 Import database ----
    cols_secm <- c('RINPERSOON', 'RINPERSOONS', 'AANVSECM', 'EINDSECM',
                   # Worker status
                   'XKOPPELWERKNSECM', 'XKOPPELDGASECM', 'XKOPPELZELFSTSECM', 'XKOPPELOVACTIEFSECM',
                   # Student status
                   'XKOPPELSCHOLSTUDSECM',
                   # Unemployment status
                   'XKOPPELWERKLUITKSECM',
                   # Social benefits
                   'XKOPPELBIJSTANDSECM','XKOPPELSOCVOORZOVSECM','XKOPPELZIEKTEAOSECM',
                   # Pension
                   'XKOPPELPENSIOENSECM')

  # Import as data.table
    secm = read_spss(secmbus,
                     col_select = all_of(cols_secm))
    setDT(secm)
    secm[, (cols_secm) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = cols_secm]

  # Individual ID
    secm[, IND_ID := paste0(RINPERSOON, RINPERSOONS)]

  # Remove those who don't work, don't study, don't receive unemployment or social benefits or any pensions
    secm = secm[(XKOPPELWERKNSECM == 1 | XKOPPELDGASECM == 1 | XKOPPELZELFSTSECM == 1 | XKOPPELOVACTIEFSECM == 1 |
                 XKOPPELSCHOLSTUDSECM == 1 | XKOPPELWERKLUITKSECM == 1 | XKOPPELBIJSTANDSECM == 1 |
                 XKOPPELSOCVOORZOVSECM == 1 | XKOPPELZIEKTEAOSECM == 1 | XKOPPELPENSIOENSECM == 1)]

  # Remove observations with RINPERSOON missing (RINPERSOONS = O)
    secm[, 'RINPERSOON' := lapply(.SD, function(x) (as.integer(x))), .SDcols = 'RINPERSOON']
    secm[, .(sum(is.na(RINPERSOON))), by = RINPERSOONS]
    secm = secm[!is.na(RINPERSOON)]
    secm[, (c('RINPERSOON','RINPERSOONS')) := NULL]

  # Set as date variables
    cols = c('AANVSECM', 'EINDSECM')
    secm[, (cols) := lapply(.SD, function(x) as.IDate(as.character(x), "%Y%m%d")), .SDcols = cols]

## 1.2 Months worked and studied by year ----

  for (year in start_year:inc_year) {

    print(year)

    # Select observations only for that year
      df = secm[year(AANVSECM) <= year & year(EINDSECM) >= year]

    # Count the number of working months during that year
      df[, work := as.integer(XKOPPELWERKNSECM == 1 | XKOPPELDGASECM == 1 | XKOPPELZELFSTSECM == 1 | XKOPPELOVACTIEFSECM == 1)]
      df[, aux_months_work := 0]
      df[year(AANVSECM) < year & year(EINDSECM) > year & work == 1 , aux_months_work := 12 ]
      df[year(AANVSECM) == year & year(EINDSECM) > year & work == 1 , aux_months_work := 13 - month(AANVSECM)]
      df[year(AANVSECM) == year & year(EINDSECM) == year & work == 1 , aux_months_work := (year(EINDSECM)*12 + month(EINDSECM)) - (year(AANVSECM)*12 + month(AANVSECM)) + 1]
      df[year(AANVSECM) < year & year(EINDSECM) == year & work == 1 , aux_months_work := month(EINDSECM)]

    # Count the number of studying months during that situation
      df[, aux_months_stud := 0]
      df[year(AANVSECM) < year & year(EINDSECM) > year & XKOPPELSCHOLSTUDSECM == 1, aux_months_stud := 12 ]
      df[year(AANVSECM) == year & year(EINDSECM) > year & XKOPPELSCHOLSTUDSECM == 1, aux_months_stud := 13 - month(AANVSECM)]
      df[year(AANVSECM) == year & year(EINDSECM) == year & XKOPPELSCHOLSTUDSECM == 1, aux_months_stud := (year(EINDSECM)*12 + month(EINDSECM)) - (year(AANVSECM)*12 + month(AANVSECM)) +1]
      df[year(AANVSECM) < year & year(EINDSECM) == year & XKOPPELSCHOLSTUDSECM == 1, aux_months_stud := month(EINDSECM)]

    # Count the number of unemployed months during that situation
      df[, aux_months_unempl := 0]
      df[year(AANVSECM) < year & year(EINDSECM) > year & XKOPPELWERKLUITKSECM == 1 , aux_months_unempl := 12 ]
      df[year(AANVSECM) == year & year(EINDSECM) > year & XKOPPELWERKLUITKSECM == 1 , aux_months_unempl := 13 - month(AANVSECM)]
      df[year(AANVSECM) == year & year(EINDSECM) == year & XKOPPELWERKLUITKSECM == 1 , aux_months_unempl := (year(EINDSECM)*12 + month(EINDSECM)) - (year(AANVSECM)*12 + month(AANVSECM)) +1]
      df[year(AANVSECM) < year & year(EINDSECM) == year & XKOPPELWERKLUITKSECM == 1 , aux_months_unempl := month(EINDSECM)]

    # Count the number of months receiving social assistance and social security benefits
      df[, socbenef := as.integer(XKOPPELBIJSTANDSECM == 1 | XKOPPELSOCVOORZOVSECM == 1 | XKOPPELZIEKTEAOSECM == 1)]
      df[, aux_months_socbenef := 0]
      df[year(AANVSECM) < year & year(EINDSECM) > year & socbenef == 1 , aux_months_socbenef := 12 ]
      df[year(AANVSECM) == year & year(EINDSECM) > year & socbenef == 1 , aux_months_socbenef := 13 - month(AANVSECM)]
      df[year(AANVSECM) == year & year(EINDSECM) == year & socbenef == 1 , aux_months_socbenef := (year(EINDSECM)*12 + month(EINDSECM)) - (year(AANVSECM)*12 + month(AANVSECM)) +1]
      df[year(AANVSECM) < year & year(EINDSECM) == year & socbenef == 1 , aux_months_socbenef := month(EINDSECM)]

    # Count the number of pension months during that situation
      df[, aux_months_pension := 0]
      df[year(AANVSECM) < year & year(EINDSECM) > year & XKOPPELPENSIOENSECM == 1 , aux_months_pension := 12 ]
      df[year(AANVSECM) == year & year(EINDSECM) > year & XKOPPELPENSIOENSECM == 1 , aux_months_pension := 13 - month(AANVSECM)]
      df[year(AANVSECM) == year & year(EINDSECM) == year & XKOPPELPENSIOENSECM == 1 , aux_months_pension := (year(EINDSECM)*12 + month(EINDSECM)) - (year(AANVSECM)*12 + month(AANVSECM)) +1]
      df[year(AANVSECM) < year & year(EINDSECM) == year & XKOPPELPENSIOENSECM == 1 , aux_months_pension := month(EINDSECM)]
    
    # Aggregation by individual 
      df[, months_work := sum(aux_months_work), by = IND_ID]
      df[, months_stud := sum(aux_months_stud), by = IND_ID]
      df[, months_unempl := sum(aux_months_unempl), by = IND_ID]
      df[, months_socbenef := sum(aux_months_socbenef), by = IND_ID]
      df[, months_pension := sum(aux_months_pension), by = IND_ID]
      
    ## 1.3 Final data base by year ----
      cols = c('IND_ID', 'months_work', 'months_stud', 'months_unempl', 'months_socbenef', 'months_pension')
      df = df[, ..cols, with = FALSE]
      df = unique(df, by = 'IND_ID')
      setnames(df, toupper(names(df)))

    ## 1.4 Export data base ----
      # Export
      save_file <- paste0("ind_sec_",year,".csv")
      fwrite(df, file = file.path(path_ind,save_file))

      # Remove dataframe
      rm(df)
  }
    
rm(secm)

#------------------------------------------------#
# 2.Annual income amount (by type of income) ----
#------------------------------------------------#

  # To extract the name of each element of the database list (prefix for the final database)
  i = 1

  # Loop through all SECM relevant databases

  for (dfsecm in secmdata) {

    namesecm <- names(secmdata[i])
    print(namesecm)

    ## 2.1 Import databases ----

      # Import as data.table
        df = read_spss(dfsecm)
        setDT(df)

        df[, (colnames(df)) := lapply(.SD, function(x) (zap_labels(x))), .SDcols = colnames(df)]

      # Rename date variables
        setnames(df, old = c(grep("^AANV", names(df), value = TRUE), grep("^EIND", names(df), value = TRUE)), new = c('AANVSECM', 'EINDSECM'))

      # Rename income variable
        setnames(df, old = c(grep("MNDBEDRAG$", names(df), value = TRUE)), new = c('MNDBEDRAG'))

      # Set as date variables
        cols = c('AANVSECM', 'EINDSECM')
        df[, (cols) := lapply(.SD, function(x) as.IDate(as.character(x), "%Y%m%d")), .SDcols = cols]

      # Individual ID
        df[, IND_ID := paste0(RINPERSOON,RINPERSOONS)]
        df[, (c('RINPERSOON','RINPERSOONS')) := NULL]

    ## 2.2 Annual income amount by year ----

      for (year in start_year:inc_year) {

        print(year)

        # Select observations only for that year
          dfyear = df[year(AANVSECM) <= year & year(EINDSECM) >= year]

        # Count the number of working months during that year
          dfyear[, aux_months_work := 0]
          dfyear[year(AANVSECM) < year & year(EINDSECM) > year & !is.na(MNDBEDRAG), aux_months_work := 12 ]
          dfyear[year(AANVSECM) == year & year(EINDSECM) > year & !is.na(MNDBEDRAG), aux_months_work := 13 - month(AANVSECM)]
          dfyear[year(AANVSECM) == year & year(EINDSECM) == year & !is.na(MNDBEDRAG), aux_months_work := (year(EINDSECM)*12 + month(EINDSECM)) - (year(AANVSECM)*12 + month(AANVSECM)) + 1]
          dfyear[year(AANVSECM) < year & year(EINDSECM) == year & !is.na(MNDBEDRAG), aux_months_work := month(EINDSECM)]

        # Total amount of that working situation (monthly amount times the number of months)
          dfyear[, aux_income_work := aux_months_work * MNDBEDRAG]
          dfyear[, GROSS_YEAR := sum(aux_income_work), by = IND_ID]
          
        # Final data base by year
          cols = c('IND_ID', 'GROSS_YEAR')
          dfyear = dfyear[, ..cols, with = FALSE]
          dfyear = unique(dfyear, by = 'IND_ID')

        # Export data base
          save_file <- paste0("SECM_",namesecm,"_",year,".csv")
          fwrite(dfyear, file = file.path(path_ind,save_file))

        # Remove dataframe
          rm(dfyear)

      }

      rm(df)

      # Update value of i for calling SECM database name
      i <- i + 1
  }

#------------------------------------------------------------------------#       
# 3.Annual income from work and social benefits (all SECM databases) ----
#------------------------------------------------------------------------#   
  
  for (year in start_year:end_year) { 
    
    print(year)
    
    ## 3.1 Import relevant databases ----  
      dfwerkndag   <- fread(file.path(path_ind,paste0("SECM_werkndga_",year,".csv")))       # Income of employees and managers
      dfzlf        <- fread(file.path(path_ind,paste0("SECM_zlf_",year,".csv")))            # Income of self-employed
      dfovact      <- fread(file.path(path_ind,paste0("SECM_ovact_",year,".csv")))          # Income of other employment (from 2001)
      
      dfwerkl      <- fread(file.path(path_ind,paste0("SECM_werkl_",year,".csv")))          # Income of unemployment benefit
      dfbijst      <- fread(file.path(path_ind,paste0("SECM_bijst_",year,".csv")))          # Income of social security benefits
      dfsocvoorzov <- fread(file.path(path_ind,paste0("SECM_socvoorzov_",year,".csv")))     # Income of other security benefits
      dfziekteao   <- fread(file.path(path_ind,paste0("SECM_ziekteao_",year,".csv")))       # Income of sick leave
      
      dfpensioen   <- fread(file.path(path_ind,paste0("SECM_pensioen_",year,".csv")))       # Income of pensions
    
    ## 3.2 Append databases - All types of income ----
      df <- rbindlist(list(dfwerkndag, dfzlf, dfovact, dfwerkl, dfbijst, dfsocvoorzov, dfziekteao, dfpensioen))
    
    ## 3.3 Append databases - Income from work ----
      df_work <- rbindlist(list(dfwerkndag, dfzlf, dfovact))
    
    ## 3.4 Append databases - Income from work and social benefits ----
      df_work_sb <- rbindlist(list(dfwerkndag, dfzlf, dfovact, dfwerkl, dfbijst, dfsocvoorzov, dfziekteao))
      
    ## 3.5 Annual gross income ----
      
      # All types of income
      dfyear <- df[, .(GROSS_ANNUAL_INCOME = sum(GROSS_YEAR)), by = IND_ID]
    
      # Income from work 
      dfyear_work <- df_work[, .(GROSS_ANNUAL_INCOME_WORK = sum(GROSS_YEAR)), by = IND_ID]
      
      # Income from work and social benef 
      dfyear_work_sb <- df_work_sb[, .(GROSS_ANNUAL_INCOME_WORK_SB = sum(GROSS_YEAR)), by = IND_ID]
      
      # Merge database 
      dfyear = merge(dfyear, dfyear_work, by = 'IND_ID', all = TRUE)
      dfyear = merge(dfyear, dfyear_work_sb, by = 'IND_ID', all = TRUE)
      
    ## 3.4 Save yearly annual income database ---- 
      save_file <- paste0("ind_inc_",year,".csv")
      fwrite(dfyear, file = file.path(path_ind,save_file))

    ## 3.5 Remove dataframe ----
      rm(dfyear,df,dfyear_work,df_work,df_work_sb,dfyear_work_sb,dfwerkndag, dfzlf, dfovact, dfwerkl, dfbijst, dfsocvoorzov, dfziekteao, dfpensioen)
      
  }

#------------------------------------------------#       
# 4.Remove intermediate files from directory ----
#------------------------------------------------#    
      
  # To extract the name of each element of the database list (prefix for the final database)
    i = 1
      
  # Loop through all SECM relevant databases 
      
    for (dfsecm in secmdata) {
        
      namesecm <- names(secmdata[i])
      print(namesecm)
      
      # Loop through all years 
        
        for (year in start_year:end_year) { 
          
          print(year)
          
          save_file <- paste0("SECM_",namesecm,"_",year,".csv")
          
          #file.remove(file.path(path_ind,save_file))
        
        }
      
      # Update value of i for calling SECM database name
        i <- i + 1  
      
    }  