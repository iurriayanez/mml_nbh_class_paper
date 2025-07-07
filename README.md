# Analysis code for "The Spatio-Temporal Evolution of Social Inequalities in Cities: A Multidimensional, Multiscalar and Longitudinal Approach for Neighbourhood Classification"

This repository contains the code and data processing scripts necessary to reproduce the analysis in [The spatio-temporal evolution of social inequalities in cities: a multidimensional, multiscalar and longitudinal approach for neighbourhood classification](https://doi.org/10.1016/j.cities.2025.106089) by [Ignacio Urria](https://orcid.org/0009-0005-0912-7627), [Ana Petrović](https://orcid.org/0000-0002-6337-964X), [Maarten van Ham](https://orcid.org/0000-0002-2106-0702) and [David Manley](https://orcid.org/0000-0003-1864-5141). 

Please refer to the original paper for a detailed description of the methodology and findings.

> **Note: The authors do not have permission to share data. However, under certain conditions, these data are accessible for statistical and scientific research. For further information: [microdata@cbs.nl](mailto:microdata@cbs.nl).**

## Table of Contents

- [1. Overview](#1-overview)
- [2. Repository Structure](#2-repository-structure)
- [3. Usage](#3-usage)
- [4. Data](#4-data)
- [5. Reproducibility](#5-reproducibility)
- [6. Dependencies](#6-dependencies)
- [7. License](#7-license)
- [8. Contact](#8-contact)
- [9. Citation](#9-citation)
- [10. Acknowledgements](#10-acknowledgements)

---

## 1. Overview

This repository provides the R scripts and functions used to create a cluster classification of multiscale bespoke neighbourhoods delineated around 100 by 100 metre grid cells in Amsterdam using a Non-Negative Matrix Factorisation (NMF). 

The approach is multidimensional, multiscalar, and longitudinal, focusing on neighbourhood classification using various socieconomic, demographic, and economic indicators across multiple time periods.

The code is organised to facilitate reproducibility and transparency, enabling other researchers to replicate, adapt, or extend the analysis.

---

## 2. Repository Structure

The repository is structured as follows:
- **master.R**: Main script to run the full analysis pipeline.
- **functions.R**: Common functions used throughout the analysis.
- **1_obj/**: Scripts for processing data on residential objects (i.e., residential units).
    - **1_obj_spatial_year.R**: Create yearly databases (1999-2023) with the spatial information of residential objects: address object ID, grid ID, and administrative spatial unit codes.
    - **2_obj_tenure_year.R**: Create yearly databases (1999-2023) with tenure and housing value (WOZ) information of residential objects.
    - **3_obj_year.R**: Create yearly databases (1999-2023) merging the spatial, tenure and housing value information of residential objects calculated in the previous 2 scripts.
- **2_ind/**: Scripts for processing individual-level data.
    - **1_ind_spatial_year.R**: Create yearly databases (1999-2023) with the individual's residential object information: address object ID, grid ID, administrative spatial unit codes, tenure and WOZ value.
    - **2_ind_demo_year.R**: Create yearly databases (1999-2023) with the individual's demographic information: gender, age and migration background.
    - **3_ind_econ_year.R**: Create yearly databases (1999-2023) with the individual's socioeconomic information: annual income and socioeconomic status.
    - **4_ind_year.R**: Create yearly databases (1999-2023) merging the spatial, demographic and socioeconomic information of individuals calculated in the previous 3 scripts.
- **3_grid/**: Scripts for aggregating individual and residential data at the 100x100m grid cell level and create the bespoke neighbourhoods.
    - **1_grid_year.R**: Create yearly databases (1999-2023) with the aggregated information of 100x100m grid cells: demographic, socioeconomic and housing composition.
    - **2_grid_export_list_ID.R**: Create list with all the grid cells with available information between 1999 - 2022.
    - **3_grid_distances.R**: Calculate euclidean distances between the grid cells obtained in the previous script.
    - **4_grid_var_scales.R**: Calculate the variables across scales for all grid cells by chunks.
    - **5_grid_var_scales_year.R**: Consolidate the chunks to create the final data base with grid cell information across all scales.
    - **6_select_grid_var_scales_met_aggl.R**: Select the final universe of grid cells in the corresponding study area from the database created in the previous script.
- **4_nbh/**: Scripts for clustering, and creation of outputs (figures, maps and tables).
    - **1_nbh_cluster_scales_long.R**: Create the cluster classification of multiscale bespoke neighbourhoods delineated around 100 by 100 metre grid cells in Amsterdam using a Non-Negative Matrix Factorisation (NMF).
    - **2_nbh_figures.R**: Create figures for the paper.
    - **3_nbh_tables.R**: Create tables for the paper.

---

## 3. Usage

All scripts should be run in the order specified in the `master.R` file. Each script is designed to be self-contained, but they might depend on the outputs of previous scripts.

To run the entire analysis, execute the `master.R` script in R or RStudio. Ensure that you have set the correct working directory.

---

## 4. Data 

### 4.1 Data sources:
Microdata from Statistics Netherlands (CBS) between 1999 and 2023. The final dataset is restricted to the metropolitan agglomeration of Amsterdam between 1999 and 2022, but the code allows to use the whole country or any other region in the Netherlands between 1999 and 2023. 

### 4.2 Data structure:

For every script, the input files sourced from CBS microdata and the corresponding output files generated by the script are listed below.

- **1_obj/**:
    - **1_obj_spatial_year.R**: 
        - **Input**: 
            1. [VRLVSLVIERKANTTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/vslvierkanttab-coordinaten-vierkanten-verblijfsobject): Link between the residential object ID and grid cells (100x100m and 500x500m) ID
            2. [VSLGWBTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/vslgwbtab-gwbcode-van-verblijfsobject-niet-gecoord--): All objects from the Basisregistratie Adressen en Gebouwen (BAG) and their spatial unit codes (buurt, wijk and gemeente).
            3. GIN2018: The GIN2018 file contains the code of the corresponding metropolitan agglomeration of the municipality. 
        
        - **Output**:
            1. `data/obj/obj_spatial_[year].csv`: Yearly database with the spatial information of residential objects: address object ID, grid ID, and administrative spatial unit codes.

    - **2_obj_tenure_year.R**:
        - **Input**:   
            1. [OBJECTWONINGTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/objectwoningtab-bewoonde-woningen-naar-eigendom-en-woz-waarde): Tenure and housing value (WOZ) information of residential objects (1999-2005).
            2. [EIGENDOMWOZTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/eigendomwoztab-woonruimten-naar-eigendom-en-woz-waarde--vervanger-van-woningtab-woonruimteeigendomtab-en-opgevolgd-door-eigendomwozbagtab--): Tenure and housing value (WOZ) information of residential objects (2006 - 2011).
            3. [EIGENDOMWOZBAGTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/eigendomwozbagtab-woz-waarde-en-eigendom): Housing value (WOZ) of residential objects  (2012 - 2018).
            4. [BAGWOZTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/bagwoztab-woz-waarde-van-woningen): Housing value (WOZ) of residential objects  (2019 - 2023).
            5. [EIGENDOMTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/eigendomtab-eigendom-en-eigenaar-van-woningen): Tenure information of residential objects (2012 - 2023).
        - **Output**:
            1. `data/obj/obj_tenure_[year].csv`: Yearly database with tenure and housing value (WOZ) information of residential objects.
    - **3_obj_year.R**:
        - **Input**: 
            1. `data/obj/obj_spatial_[year].csv`
            2. `data/obj/obj_tenure_[year].csv`
        - **Output**:
            1. `data/obj/obj_[year].rds`: Yearly database merging the spatial, tenure and housing value information of residential objects calculated in the previous 2 scripts.
- **2_ind/**:
    - **1_ind_spatial_year.R**:
        - **Input**: 
            1. `data/obj/obj_[year].csv`
            2. [GBAADRESOBJECTBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/gbaadresobjectbus-gba-adreskenmerken-van-personen): Link between the individual and the residential object ID.
        - **Output**:
            1. `ind_spatial_[year].csv`: Yearly database with the individual's residential object information: address object ID, grid ID, administrative spatial unit codes, tenure and WOZ value.
    - **2_ind_demo_year.R**:
        - **Input**: 
            1. [GBAPERSOONTAB](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/gbapersoontab-persoonskenmerken-van-personen-in-de-brp): Demographic information of individuals.
            2. LANDAKTUEELREF: Listing with country codes.
            3. [GBAHUISHOUDENSBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/gbahuishoudensbus-huishoudenskenmerken): Household composition of individuals.
        - **Output**:
            1. `ind_demo_[year].csv`: Yearly database with the individual's demographic information.
    - **3_ind_econ_year.R**:
        - **Input**:
            1. [SECMBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmbus-personen-sociaaleconomische-categorie): Socioeconomic status of individuals. 
            2. [SECMWERKNDGAMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmwerkndgamndbedragbus-personen-met-loon-maand): Monthly income of individuals from employment. 
            3. [SECMZLFMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmzlfmndbedragbus-winstbedrag-van-zelfstandigen): Monthly income of individuals from self-employment.
            4. [SECMOVACTMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmovactmndbedragbus-personen-overige-arbeid): Monthly income of individuals from other work-related activities.
            5. [SECMBEURSMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmbeursmndbedragbus-personen-met-studiebeurs): Monthly income of individuals from student benefits.
            6. [SECMBIJSTMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmbijstmndbedragbus-personen-bijstand-verslagmaand): Monthly income of individuals from social assistance (_Bijstand_ in Dutch).
            7. [SECMSOCVOORZOVMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmsocvoorzovmndbedragbus-soc-econ-cat-ov-soc-voorzien-): Monthly income of individuals from other social assistance benefits.
            8. [SECMPENSIOENMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmpensioenmndbedragbus-personen-met-pensioenbedragen): Monthly income of individuals from pensions.
            9. [SECMWERKLMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmwerklmndbedragbus-personen-werkloosheidsuitkering): Monthly income of individuals from unemployment benefits.
            10. [SECMZIEKTEAOMNDBEDRAGBUS](https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/secmziekteaomndbedragbus-personen-uitkering-bij-ziekte): Monthly income of individuals from sickness benefits.
        - **Output**:
            1. `data/ind/ind_sec_[year].csv`: Yearly database with the individual's socioeconomic status.
            2. `data/ind/ind_inc_[year].csv`: Yearly database with the individual's annual income.
    - **4_ind_year.R**:
        - **Input**:   
            1. `data/ind/ind_spatial_[year].csv`
            2. `data/ind/ind_demo_[year].csv`
            3. `data/ind/ind_sec_[year].csv`
            4. `data/ind/ind_inc_[year].csv`
            5. cpiEU_NL_1999_2022_2015100.csv [(available online here)](https://opendata.cbs.nl/#/CBS/en/dataset/83131ENG/table?ts=1751906408417): Consumer Price Index (CPI) data for the Netherlands between 1999 and 2022 (2015 = 100), used to adjust income values.
        - **Output**:
            1. `data/ind/ind_[year].rds`: Yearly database merging the spatial, demographic and socioeconomic information of individuals calculated in the previous 3 scripts.
- **3_grid/**:
    - **1_grid_year.R**:
        - **Input**: 
            1. `ind_[year].rds`: Yearly database merging the spatial, demographic and socioeconomic information of individuals.
            2. `obj_[year].csv`: Yearly database merging the spatial, tenure and housing value information of residential objects.
        - **Output**:
            1. `data/grid/grid_[year].rds`: Yearly database with the aggregated information of 100x100m grid cells: demographic, socioeconomic and housing composition.
    - **2_grid_export_list_ID.R**:
        - **Input**:
            1. `data/grid/grid_[year].rds`
        - **Output**:
            1. `data/grid/list_VIERKANT100M_1999_2022.csv`: List with all the grid cells with available information between 1999 - 2022.
    - **3_grid_distances.R**:
        - **Input**:
            1. `data/grid/list_VIERKANT100M_1999_2022.csv`
        - **Output**:
            1. `data/grid/euclidean_dist_[max_radius]m_[city]_part_[i].csv`: Euclidean distances between the grid cells obtained in the previous script with a max search radius of `max_radius`, for the study area `city` (can be the whole country) and the chunk `i`.
    - **4_grid_var_scales.R**:
        - **Input**:
            1. `data/grid/grid_[year].rds`
            2. `data/grid/euclidean_dist_[max_radius]m_[city]_part_[i].csv`
        - **Output**:
            1. `data/grid/grid_var_scales_[part].csv`: Variables calculated across scales for all grid cells by chunks, where `part` is the chunk number.
    - **5_grid_var_scales_year.R**:
        - **Input**:
            1. `data/grid/grid_var_scales_[part].csv`
        - **Output**:
            1. `data/grid/grid_var_scales_[year].rds`: Consolidated database with grid cell information across all scales.
    - **6_select_grid_var_scales_met_aggl.R**:
        - **Input**:
            1. `data/grid/grid_var_scales_[year].rds`
        - **Output**:
            1. `data/grid/grid_var_scales_metaggl_10_[year].rds`: Final universe of grid cells in the corresponding study area (MAA or metaggl_10) from the database created in the previous script.
- **4_nbh/**:
    - **1_nbh_cluster_scales_long.R**: 
        - **Input**:
            1. `data/grid/grid_var_scales_metaggl_10_[year].rds`
        - **Output**:
            1. `data/nbh/clusters_scales_k[nclust]_long_metaggl_[met_aggl].csv`: Cluster assignments (nclust = 4) for each grid cell across scales for the metropolitan agglomeration of Amsterdam (met_aggl = 10).
            2. `data/nbh/clusters_basis_matrix_k[nclust]_long_metaggl_[met_aggl].csv`: NMF basis matrix for the clusters.
            3. `data/nbh/clusters_coef_matrix_k[nclust]_long_metaggl_[met_aggl].csv`: NMF coefficient matrix for the clusters.
            4. `output/tables/rank_stats_NMF_scales_nostd_long_metaggl_[met_aggl].csv`: NMF rank statistics across scales.
            5. `output/figures/all_rank_stats_NMF_scales_nostd_long_metaggl_[met_aggl].bmp`: Plots with statistics for clusters different cluster solutions.
            6. `output/figures/sel_rank_stats_NMF_scales_nostd_long_metaggl_[met_aggl].bmp`: Plots with selected statistics for clusters different cluster solutions.
    - **2_nbh_figures.R**:
        - **Input**:
            1. `data/nbh/clusters_scales_k[nclust]_long_metaggl_[met_aggl].csv`
            2. `data/grid/grid_[year].rds`
            3. `data/nbh/clusters_basis_matrix_k[nclust]_long_metaggl_[met_aggl].csv`
            4. `data/nbh/clusters_coef_matrix_k[nclust]_long_metaggl_[met_aggl].csv`
            5. `output/tables/rank_stats_NMF_scales_nostd_long_metaggl_[met_aggl].csv`
        - **Output**:
            1. `output/figures/sel_rank_stats_NMF_scales_nostd_long_metaggl_[met_aggl].pdf`: Selected NMF rank statistics plots (Figure 2 of the paper).
            2. `output/figures/coefmap_NMF_scales_nostd_k[nclust]_long_metaggl_[met_aggl].pdf`: Coefficient map for NMF clusters (Figure 4 of the paper).
            3. `output/figures/clusters-maps/clusters_NMF_k[nclust]_scale_[scale]_[year]_metaggl_[met_aggl].pdf`: Cluster maps by scale and year (Figure 5 and all other remaining scales and years).
            4. `output/figures/alluvial_plot_[selected_scale]_metaggl_[met_aggl].pdf`: Alluvial plot for selected scale (Figures 6 and B.2 of the paper).  
            5. `output/figures/clusters_size_scale_years_metaggl_[met_aggl].pdf`: Cluster size by scale and years (Figure B.1 of the paper).
    - **3_nbh_tables.R**:
        - **Input**:
            1. `data/nbh/clusters_scales_k[nclust]_long_metaggl_[met_aggl].csv`
            2. `data/grid/grid_[year].rds`
            3. `data/nbh/clusters_basis_matrix_k[nclust]_long_metaggl_[met_aggl].csv`
            4. `data/nbh/clusters_coef_matrix_k[nclust]_long_metaggl_[met_aggl].csv`
        - **Output**:
            1. `output/tables/clust_size_selected_scales_[year]_[met_aggl].csv`: Cluster size (Table 1 of the paper).
---

## 5. Reproducibility

- All scripts are designed for reproducibility.
- Set random seeds where applicable.

---

## 6. Dependencies

The analysis requires R (version 4.0 or higher) and the following R packages:

- haven
- dplyr
- tidyr
- readr
- ggplot2
- ggthemes
- sf
- data.table
- NMF
- cluster
- tibble
- gridExtra
- ggspatial
- FactoMineR
- grDevices
- stats 


---

## 7. License

This software is licensed under the [MIT License](https://opensource.org/licenses/MIT).

---

## 8. Contact

For questions or further information, please contact:

- Ignacio Urria Yáñez, TU Delft [(i.a.urriayanez@tudelft.nl)](mailto:i.a.urriayanez@tudelft.nl)

---

## 9. Citation

If you use this code or analysis in your research, please cite the paper:

Ignacio Urria, Ana Petrović, Maarten van Ham, David Manley.  
"The spatio-temporal evolution of social inequalities in cities: a multidimensional, multiscalar and longitudinal approach for neighbourhood classification."  
*Cities*, Volume 165, 2025, 106089.  
[https://doi.org/10.1016/j.cities.2025.106089](https://doi.org/10.1016/j.cities.2025.106089) 


---

## 10. Acknowledgements

This research was funded by the Delft Technology Fellowship awarded to Ana Petrović. 

The authors gratefully acknowledge the support of the microdata team from Statistics Netherlands (CBS). 

Part of the code in this repository is based on code originally developed by Ana Petrović.
