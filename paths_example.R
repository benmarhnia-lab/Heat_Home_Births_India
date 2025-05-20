library(here)

# Paths for the project on the shared drive
path_project <- "path/to/your/project/directory" # Replace with your project path
path_processed <- here(path_project, "data/processed_data/")
path_outputs <- here(path_project, "outputs/")

# Paths for datasets from the common-datasets folder
dhs_temp_dir <- "path/to/your/dhs-and-temperature-datasets/" # Replace with your local directory for DHS and temperature datasets

## DHS datasets
path_dhs_india_IR <- here(dhs_temp_dir, "dhs-raw-datasets/India/2019-21/individual-recode/IAIR7EDT/")
path_dhs_india_shp <- here(dhs_temp_dir, "dhs-raw-datasets/India/2019-21/geographic-data/IAGE7AFL/")

## Temperature datasets
path_wbgt_max_raw <- here(dhs_temp_dir, "climate-datasets/global/wbgt-brimicombe/wbgt-max/")
path_db_max_era5 <- here(dhs_temp_dir, "climate-datasets/global/t2m-era5/tmax/")
