# Nationwide serological reconstruction reveals spatial heterogeneity in dengue transmission and case reporting across Taiwan
The repository contains three main R scripts corresponding to the descriptive, genetic, and spatial analyses.

| R Script | Analysis | Input Data |
| --- | --- | --- |
| Demographic Analysis (`demographic_analysis.R`) | Descriptive and statistical analyses of demographic characteristics and dengue serostatus. | **Input files:** <br>1. Individual-level Taiwan Biobank data containing demographic information and dengue ELISA serology results (`sero_elisa_all.xlsx`).
| Genetic Analysis (`genetic_analysis.R`) | Downstream genetic analysis after quality control, including SNP selection, correlation and multi-SNP regression analysis. | **Input files:** <br>1. Individual-level Taiwan Biobank data containing demographic information and dengue ELISA serology results (`biobank_data_individual_district.xlsx`). <br>2. Genetic principal component and covariate data(`final_remove_o_gwas_pca_5_1%_age_sex.txt`).<br>3. Taiwan Biobank PLINK genotype dataset (`final_100_remove_o.bed, .bim, .fam`).|
| Spatial Analysis and Modelling (`spatial_analysis.R`) | Spatial analysis and modelling, including force of infection (FOI) estimation, past seroprevalence reconstruction, reporting rate and number of expected infection estimation | **Input files:** <br>1. Individual-level Taiwan Biobank data containing demographic information and dengue ELISA serology results (`biobank_data_individual_district.xlsx`). <br>2. Taiwan boundary shapefile (`TOWN_MOI_1120317.shp`). <br>3. Township-level covariate data, including adult mosquito indices, mobility, population density, towncode, town and county names (`integrated_town_data.xlsx`). <br>4. Township-level dengue daily reported cases excluding imported cases (`Dengue_cases_town_city_by_year_without_imported.xlsx`). <br>5. Population data by age group at the township and county level (`鄉鎮市區人數按年齡_age_group.xlsx`). | 

## Software and package versions

**Demographic analysis**<br>
All analyses were conducted using R version 4.4.1. 
| R package | version |
| --- | --- |
| dplyr | 1.1.4 | 
| openxlsx | 4.2.8 | 
| broom | 1.0.10 |

**Spatial analysis**<br>
All analysis are run using R version 4.4.1.
| R package | version |
| --- | --- |
| INLA | 24.12.11 | 
| dplyr | 1.1.4 | 
| ggplot2 | 4.0.1 |
| ggthemes | 5.1.0 | 
| sf | 1.0.16 | 
| corrplot | 0.94 | 
| car | 3.1.2 | 
| openxlsx | 4.2.8| 
| readxl | 1.4.5 | 
| ggspatial | 1.1.10 | 
| spdep | 1.3.10 |
| Hmisc | 5.2.3 | 
| tidyr | 1.3.1 | 
| cowplot | 1.2.0 |
| patchwork | 1.3.2 |
| ggbreak | 0.1.7 |

**Genetic analysis** <br>
Genetic analysis are run using R version 4.4.1 and PLINK v1.90b6.27.
| R package | version |
| --- | --- |
| data.table | 1.17.0 |
| pscl | 1.5.9 | 
| ggplot2 | 4.0.1 |
| openxlsx | 4.2.8| 

**All required R packages can be installed with the following command:**<br> install.packages(c("dplyr","openxlsx","broom","data.table","pscl","ggplot2","ggthemes","sf","corrplot","car","readxl","ggspatial","spdep","Hmisc","tidyr","cowplot","patchwork","ggbreak"))<br>
**For R-INLA:** <br> install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)<br>
**PLINK 1.9** can be downloaded from the official PLINK website:<br> https://www.cog-genomics.org/plink/

## Data availability
The Taiwan Biobank data used in this study are not publicly available due to data access restrictions. Access to these data may be obtained through application to the Taiwan Biobank and is subject to its approval procedures. 

Mobility data were obtained from Far EasTone Telecommunications and are not publicly available due to data access restrictions. 

Other datasets and variables used in this study are publicly available from their respective sources. The sources of these datasets are described in the manuscript.

