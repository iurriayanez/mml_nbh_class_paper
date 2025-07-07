#------------------------------------------------------------------------------#
# Name: 0_master
# Goal: Master script of all processes
# Date of creation: 21-12-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez
#------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------------------------------------------------------#
# 0. Setup ----
## 0.1 Packages ---- 
pacman::p_unload(pacman::p_loaded(), character.only = T)
library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(data.table)

## 0.2 Parameters ----
# Seed 
set.seed(1409)

# Grid 
grid_size <- 100                          # VIERKANT100M = 100; VIERKANT500M = 500

# Euclidean distance   
filter_area   <- FALSE                    # If TRUE: filter grids within specific area
city          <- paste0("vk",grid_size,"_1999_2022")        # City/area under analysis. For file name
radius        <- 3500                     # Search radius for nearest neighbours (in meters)
export_chunks <- TRUE                     # If TRUE: export by chunks
num_chunks    <- 30                       # Number of chunks to export final database

# VIERKANT list file name
vierkantlistname = paste0("list_VIERKANT",grid_size, "M_1999_2022.csv")

# Scales (in meters)
scale_values      <- c(100,200,400,800,1600,3200) # Scale to agregate variables (in meters)
clust_scales      <- c(0,1,2,3,4,5,6)             # Subset scales for cluster analysis (numeric value for the corresponding scale)

# Metropolitan agglomeration (cluster analysis)
met_aggl = 10 

# Years 
start_year = 1999
end_year = 2023
inc_year = 2022
years    = c('1999','2003','2007','2011','2015', '2019', '2022')           # Years for longitudinal classification 

# Sequence analysis parameters                                                            
clust_labs <- c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4')
scale_labs <- c("100x100m grid cells","100m radius","200m radius","400m radius","800m radius","1600m radius","3200m radius") 
scale_labs_save <- c("Grid","100m","200m","400m","800m","1600m","3200m") 

# Check if it is the correct number of NMF clusters
nclust = 4 

# Minimum number of individuals and households to show on maps (by grid cell)
min_ind <- 5
min_hh <- 3


## 0.3 Paths  ----
# Directory 
path         = "H:/Ignacio"
paper        = "paper1"                                             # Change according to the paper name
setwd(path)

# Main folders 
microdata    = "G:"                                                 # Change according to the microdata location 
path_raw     = file.path(microdata)
path_data    = file.path(path, "data")
path_results = file.path(path, "output", paper)
path_code    = file.path(path, "code", paper)
path_utilities = file.path("K:","Utilities")

# Process subfolders 
path_ind              = file.path(path_data, "ind")
path_obj              = file.path(path_data, "obj")
path_grid             = file.path(path_data, "grid")
path_nbh              = file.path(path_data, "nbh")

subpaths = list(path_ind,path_obj,path_grid,path_nbh,path_log_obj,path_log_ind,path_log_grid,path_log_nbh)

# Check existance of subfolders to export the data
for (p in subpaths) {
  if (file.exists(p) == FALSE) {
    dir.create(p)
  }
}

## 0.4 Databases ----
  ### 0.4.1 Object ----
    # Spatial 
    vslvierkanttab        = file.path(path_raw,"BouwenWonen","VSLVIERKANTTAB","VRLVSLVIERKANTTABV2023031.sav")
    vslgwbtab             = file.path(path_raw,"BouwenWonen","VSLGWBTAB", "VSLGWB2023TAB03V1.sav")
    gin2018tab            = file.path(path_utilities,'HULPbestanden','GebiedeninNederland','GIN2018V1.sav')
    
    # Tenure
    eigendomwoztab        = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZTAB")
    
    objectwoningtab       = file.path(path_raw,"BouwenWonen", "OBJECTWONINGTAB")
    
    eigendomwozbagtab2012 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2012", "EIGENDOMWOZBAGTAB 2012V1.sav")
    eigendomwozbagtab2013 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2013", "EIGENDOMWOZBAGTAB 2013V1.sav")
    eigendomwozbagtab2014 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2014", "EIGENDOMWOZBAGTAB 2014V2.sav")
    eigendomwozbagtab2015 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2015", "EIGENDOMWOZBAG2015TABV1.sav")
    eigendomwozbagtab2016 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2016", "EIGENDOMWOZBAG2016TABV1.sav")
    eigendomwozbagtab2017 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2017", "EIGENDOMWOZBAG2017TABV2.sav")
    eigendomwozbagtab2018 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2018", "EIGENDOMWOZBAG2018TABV1.sav")
    eigendomwozbagtab2019 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2019", "EIGENDOMWOZBAG2019TABV1.sav")
    eigendomwozbagtab2020 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2020", "EIGENDOMWOZBAG2020TABV1.sav")
    eigendomwozbagtab2021 = file.path(path_raw,"BouwenWonen", "EIGENDOMWOZBAGTAB", "2021", "EIGENDOMWOZBAG2021TABV1.sav")
    
    eigendomtab2012       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2012TABV6.sav")
    eigendomtab2013       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2013TABV6.sav")
    eigendomtab2014       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2014TABV6.sav")
    eigendomtab2015       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2015TABV6.sav")
    eigendomtab2016       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2016TABV6.sav")
    eigendomtab2017       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2017TABV5.sav")
    eigendomtab2018       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2018TABV3.sav")
    eigendomtab2019       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2019TABV3.sav")
    eigendomtab2020       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2020TABV3.sav")
    eigendomtab2021       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2021TABV2.sav")
    eigendomtab2022       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2022TABV2.sav")
    eigendomtab2023       = file.path(path_raw,"BouwenWonen", "EIGENDOMTAB", "EIGENDOM2023TABV1.sav")
    
    bagwoztab2019         = file.path(path_raw,"BouwenWonen", "BAGWOZTAB", "BAGWOZ2019TABV2.sav")
    bagwoztab2020         = file.path(path_raw,"BouwenWonen", "BAGWOZTAB", "BAGWOZ2020TABV2.sav")
    bagwoztab2021         = file.path(path_raw,"BouwenWonen", "BAGWOZTAB", "BAGWOZ2021TABV2.sav")
    bagwoztab2022         = file.path(path_raw,"BouwenWonen", "BAGWOZTAB", "BAGWOZ2022TABV2.sav")
    bagwoztab2023         = file.path(path_raw,"BouwenWonen", "BAGWOZTAB", "BAGWOZ2023TABV1.sav")     
    
  ### 0.4.2 Individuals ----
    # Spatial 
    gbaadressobject   = file.path(path_raw,"Bevolking","GBAADRESOBJECTBUS", "GBAADRESOBJECT2023BUSV1.sav")
    
    # Demo
    gbapersoontab  = file.path(path_raw,"Bevolking","GBAPERSOONTAB","2023", "GBAPERSOON2023TABV1.sav")
    landaktueelref = file.path(path_utilities,"Code_Listings","SSBreferentiebestanden","LANDAKTUEELREFV13.sav")
    gbahuishoudens = file.path(path_raw,"Bevolking","GBAHUISHOUDENSBUS", "GBAHUISHOUDENS2023BUSV1.sav")
    
    # Econ
    secmbus    = file.path(path_raw,"InkomenBestedingen","SECMBUS","SECMBUS2022V1.sav")
    werkndga   = file.path(path_raw,"InkomenBestedingen","SECMWERKNDGAMNDBEDRAGBUS","SECMWERKNDGAMNDBEDRAGBUSV20221.sav")
    beurs      = file.path(path_raw,"InkomenBestedingen","SECMBEURSMNDBEDRAGBUS","SECMBEURSMNDBEDRAGBUSV20221.sav")
    bijst      = file.path(path_raw,"InkomenBestedingen","SECMBIJSTMNDBEDRAGBUS","SECMBIJSTMNDBEDRAGBUSV20221.sav")
    ovact      = file.path(path_raw,"InkomenBestedingen","SECMOVACTMNDBEDRAGBUS","SECMOVACTMNDBEDRAGBUSV20221.sav")
    pensioen   = file.path(path_raw,"InkomenBestedingen","SECMPENSIOENMNDBEDRAGBUS","SECMPENSIOENMNDBEDRAGBUSV20221.sav")
    socvoorzov = file.path(path_raw,"InkomenBestedingen","SECMSOCVOORZOVMNDBEDRAGBUS","SECMSOCVOORZOVMNDBEDRAGBUSV20221.sav")
    werkl      = file.path(path_raw,"InkomenBestedingen","SECMWERKLMNDBEDRAGBUS","SECMWERKLMNDBEDRAGBUSV20221.sav")
    ziekteao   = file.path(path_raw,"InkomenBestedingen","SECMZIEKTEAOMNDBEDRAGBUS","SECMZIEKTEAOMNDBEDRAGBUSV20221.sav")
    zlf        = file.path(path_raw,"InkomenBestedingen","SECMZLFMNDBEDRAGBUS","SECMZLFMNDBEDRAGBUSV20221.sav")
    
    secmdata   = list(werkndga = werkndga, beurs = beurs, bijst = bijst, ovact = ovact, pensioen = pensioen, 
                      socvoorzov = socvoorzov, werkl = werkl, ziekteao = ziekteao, zlf = zlf)

    # Ind
    cpitab = file.path(path,'raw', 'cpiEU_NL_1999_2022_2015100.csv') 
  
## 0.5 Processes ----
# Functions 
source(file.path(path_code, "functions.R"), echo = F)        


#---------------------------------------------------------------------------------------------------------------------------------------#

# 1. Obj
source(file.path("1_obj", "1_obj_spatial_year.R"), echo = TRUE, max.deparse.length = 10000)    
source(file.path("1_obj", "2_obj_tenure_year.R"), echo = TRUE, max.deparse.length = 10000)    
source(file.path("1_obj", "3_obj_year.R"), echo = TRUE, max.deparse.length = 10000)   
 
# 2. Ind 
source(file.path("2_ind", "1_ind_spatial_year.R"), echo = TRUE, max.deparse.length = 10000)      
source(file.path("2_ind", "2_ind_demo_year.R"), echo = TRUE, max.deparse.length = 10000)  
source(file.path("2_ind", "3_ind_econ_year.R"), echo = TRUE, max.deparse.length = 10000)  
source(file.path("2_ind", "4_ind_year.R"), echo = TRUE, max.deparse.length = 10000) 

# 3. Grid 
source(file.path("3_grid", "1_grid_year.R"), echo = TRUE, max.deparse.length = 10000)   
source(file.path("3_grid", "2_grid_export_list_ID.R"), echo = TRUE, max.deparse.length = 10000)  
source(file.path("3_grid", "3_grid_distances.R"), echo = TRUE, max.deparse.length = 10000)  
source(file.path("3_grid", "4_grid_var_scales.R"), echo = TRUE, max.deparse.length = 10000)  
source(file.path("3_grid", "5_grid_var_scales_year.R"), echo = TRUE, max.deparse.length = 10000)  
source(file.path("3_grid", "6_select_grid_var_scales_met_aggl.R"), echo = TRUE, max.deparse.length = 10000)  

# 4. Neighbourhood class. & final figures/tables
source(file.path("4_nbh", "1_nbh_cluster_scales_long.R"), echo = TRUE, max.deparse.length = 10000) 
source(file.path("4_nbh", "2_nbh_figures.R"), echo = TRUE, max.deparse.length = 10000) 
source(file.path("4_nbh", "3_nbh_tables.R"), echo = TRUE, max.deparse.length = 10000) 