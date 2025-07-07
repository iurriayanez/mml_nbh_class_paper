#------------------------------------------------------------------------------#
# Name: 3_nbh_tables.R
# Goal: Create figures and tables
# Date of creation: 04-12-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/nbh/clusters_scales_k[nclust]_long_metaggl_[met_aggl].csv
#                  data/grid/grid_[year].rds
#                  data/nbh/clusters_basis_matrix_k[nclust]_long_metaggl_[met_aggl].csv
#                  data/nbh/clusters_coef_matrix_k[nclust]_long_metaggl_[met_aggl].csv 
# Output: output/tables/clust_size_selected_scales_[year]_[met_aggl].csv
#------------------------------------------------------------------------------#
#--------------------#
# 1. Import data ----
#--------------------#  

## 1.1 Clusters ----
cluster_csv <- paste0("clusters_scales_k", nclust, "_long_metaggl_", met_aggl, ".csv")
df <- fread(file.path(path_nbh, cluster_csv))

df$CLUSTER <- factor(df$CLUSTER, labels = clust_labs)
df <- df %>% mutate(SCALE = SCALE + 1)

## 1.2 Coef matrix  ---- 
coef_csv <- paste0("clusters_coef_matrix_k", nclust, "_long_metaggl_", met_aggl, ".csv")
coef_matrix <- fread(file.path(path_nbh, coef_csv))    

## 1.3 Basis matrix ----
basis_csv <- paste0("clusters_basis_matrix_k", nclust, "_long_metaggl_", met_aggl, ".csv")
basis_matrix <- fread(file.path(path_nbh, basis_csv))

# Add basis components
df <- cbind(df, basis_matrix)

# Create basis variable (value of basis of the corresponding cluster) 
df$BASIS <- do.call(pmax, df[, paste0("CLUSTER", 1:nclust), with = FALSE])

## 1.4 Gray values of cells for maps ----
# We need to gray out cells with less than 5 individuals and less than 3 households

# Only grid cells in the metropolitan agglomeration in 2022
grid <- read_rds(file.path(path_grid,"grid_2022.rds"))
grid <- grid %>% 
  select(VIERKANT100M, METAGGL) %>% 
  filter(METAGGL == met_aggl)

grid_long = data.table()
for (i in years) {
  file_rds  <- paste0("grid_",i,".rds")
  grid_year <- read_rds(file.path(path_grid,file_rds))
  setDT(grid_year)
  grid_year <- grid_year[, c('VIERKANT100M', 'TOT_IND',
                             'TOT_HH'), with = FALSE]
  grid_year <- merge(grid, grid_year, by = 'VIERKANT100M', all.x = T)
  grid_year <- grid_year[METAGGL == met_aggl,]
  grid_year[, YEAR := as.integer(i)]
  grid_long = rbind(grid_long, grid_year)
  rm(grid_year)
}


# Create sample identifier
grid_long[, SAMPLE := 0]
grid_long[TOT_IND >= min_ind & TOT_HH >= min_hh, SAMPLE := 1]
grid_long[, .(sample = sum(SAMPLE)), by = YEAR]

# Add information to original dataframe 
df <- merge(df, grid_long, by = c('VIERKANT100M', 'YEAR'), all.x = T)

df %>% 
  filter(SCALE == 1) %>% 
  group_by(YEAR) %>% 
  summarise(sample = sum(SAMPLE))

# Gray out cluster variable 
df$CLUSTER_MAP <- as.factor(ifelse(df$SAMPLE == 0, NA, df$CLUSTER))
df$CLUSTER1_MAP <- as.numeric(ifelse(df$SAMPLE == 0, NA, df$CLUSTER1))
df$CLUSTER2_MAP <- as.numeric(ifelse(df$SAMPLE == 0, NA, df$CLUSTER2))
df$CLUSTER3_MAP <- as.numeric(ifelse(df$SAMPLE == 0, NA, df$CLUSTER3))
df$CLUSTER4_MAP <- as.numeric(ifelse(df$SAMPLE == 0, NA, df$CLUSTER4))
#df$CLUSTER5_MAP <- as.numeric(ifelse(df$SAMPLE == 0, NA, df$CLUSTER5))


#---------------------#
# 2. Create table ----
#---------------------#  

clust_size <- df %>%
  group_by(YEAR, SCALE, CLUSTER) %>%
  summarise(n_clust = n())

# Create custom table 
clusters <- paste("Cluster", 1:nclust)
years <- c(1999, 2011, 2022)
final_table <- tibble::as_tibble(years)
colnames(final_table) <- "year"
for (clust in clusters){
  for (i in c(1,5,7)) {
    x <- clust_size[clust_size$YEAR %in% c(1999,2011,2022) & clust_size$CLUSTER == clust & clust_size$SCALE == i, 4] 
    colnames(x) <- paste(clust,i, sep = "_")
    final_table <- cbind(final_table, x)
  }
}


#---------------#
# 3. Export ----
#---------------#  
save_file <- paste0("clust_size_selected_scales_", years, "_", met_aggl, ".csv")
fwrite(final_table, file = file.path(path_results,"tables",save_file), row.names = TRUE)   