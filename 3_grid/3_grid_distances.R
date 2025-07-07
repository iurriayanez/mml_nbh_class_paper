#------------------------------------------------------------------------------#
# Name: 4_grid_distances.R
# Goal: Calculate euclidean distances between grid cells
# Date of creation: 19-12-2023

# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Platform: x86_64-w64-mingw32/x64
# Author: Ignacio Urria Yanez

# Input databases: data/grid/list_VIERKANT[grid_size]M_1999_2022.csv    
# Output databases: data/grid/euclidean_dist_[radius]m_[city]_part_[i].csv

#------------------------------------------------------------------------------#

#----------------------#    
# 1. Import layers ----
#----------------------# 
  
  # Filter grids for which we have information 
    grids <- fread(file.path(path_grid, vierkantlistname))

  # Numeric VIERKANT ID
    grids$INT_VIERKANT100M <- seq_len(nrow(grids))
    
  # Import study area 
    if (filter_area == TRUE) {
      area <- sf::st_read(area_gpkg)
    }
 
#-------------------------#    
# 2. Create centroids ----
#-------------------------# 

  # Create centroids from ID 
  X <- as.numeric(paste0(substr(grids$VIERKANT100M, 2, 5),"50"))
  Y <- as.numeric(paste0(substr(grids$VIERKANT100M, 7, 10),"50"))
  grids_centroids <- data.frame(X,Y)
  coords <- as.matrix(grids_centroids)
  rm(X,Y, grids_centroids)
  
#--------------------#    
# 3. Filter area ----
#--------------------#  
  
  # Buffer 
    if (filter_area == TRUE) {
      area <- area[, c("id", "geom")]
      area <- sf::st_convex_hull(area)
      sf::st_crs(area)$units              # In meters
      area_buffer <- sf::st_buffer(area, dist = radius + 1000)
   
  
  # Filter 
      filter_grid <- sf::st_intersection(grids_centroids, area_buffer)
      filter_grid <- filter_grid[, c("VIERKANT100M", "geom")]
      head(filter_grid)
    }
  
#---------------------------#    
# 4. Euclidean distance ----
#---------------------------#
    
  # Find the indices and euclidean distance of centroids within radius
    nbrs <- dbscan::frNN(coords, eps = radius, sort = FALSE)
   
  # Count the total number of neighbors within radiues for all points in the current chunk 
    total_neighbors <- sum(sapply(nbrs$id, function(x) sum(x != 0)))
      
  # Preallocate the vectors for the current chunk 
    origin_node_id <- integer(total_neighbors)
    destination_node_id <- integer(total_neighbors)
    euclidean_distance <- numeric(total_neighbors)
      
  # Initialise a counter for the current row 
    row_counter <- 1
  
  # Initialise progress bar
    pb <- txtProgressBar(min = 0, max = length(nbrs$id), style = 3)
    
  # Calculate the distances for the current chunk 
    for (i in seq_along(nbrs$id)) {
      k <- 1
      for (j in nbrs$id[[i]]) {
        origin_node_id[row_counter] <- i
        destination_node_id[row_counter] <- j
        euclidean_distance[row_counter] <- nbrs$dist[[i]][[k]]
        row_counter <- row_counter + 1
        k <- k + 1
      }
      #Update progress bar 
      setTxtProgressBar(pb, i)
    }
    
  # Convert the results vectors to a data frame 
    results <- data.frame(origin_node_id = origin_node_id, destination_node_id = destination_node_id, euclidean_distance = euclidean_distance)
    setDT(results)
    #results <- results[euclidean_distance >0, ]
    #results <- unique(results)
    #head(results)
    rm(nbrs, coords, origin_node_id, destination_node_id, euclidean_distance)
    
#---------------#    
# 5. Export ----
#---------------#
  # By chunks 
    if (export_chunks == TRUE) {
      chunk_size <- ceiling(max(grids$INT_VIERKANT100M) / num_chunks)
      chunks <- lapply(1:num_chunks, function(i) {
        start_id <- (i - 1) * chunk_size + 1
        end_id <- i * chunk_size 
        results[origin_node_id >= start_id & origin_node_id <= end_id]
      })
      
      # Export each chunk to a CSV file 
      for (i in seq_along(chunks)) {
        chunk <- chunks[[i]]
        filename <- paste0("euclidean_dist_", radius, "m_", city, "_part_", i, ".rds")
        write_rds(chunk, file.path(path_grid, filename), compress = "gz")
      }
      rm(chunk, chunks)
      
    } else {
      # Export results 
        write_rds(results, file.path(path_grid, paste0("euclidean_dist_", radius, "m_", city, ".rds")), compress = "gz")
    }
    
    # Export conversion table
      fwrite(grids, file.path(path_grid, paste0("conv_table_euclidean_", radius, "m_", city, ".csv")), row.names = FALSE) 
    
      rm(results, grids)
    
    
  
  
