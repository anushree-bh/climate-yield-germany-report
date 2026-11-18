# =============================================================================
# FMCASS – Inspecting the German county-level crop yield dataset (1979–2024)
# =============================================================================

library(tidyverse)

# Load the data
# district_no is read as text so leading zeros are kept ("01001", not 1001)
crops <- read_csv("Final_data_2024.csv",
                  col_types = cols(district_no = col_character()))

head(crops)


# --- 1. Check the available measurements -------------------------------------
unique(crops$measure)


# --- 2. Check the available crops --------------------------------------------
unique(crops$var)


# --- 3. Check the time period ------------------------------------------------
range(crops$year, na.rm = TRUE)


# --- 4. Count observations by measurement type -------------------------------
library(dplyr)
crops %>%
  count(measure)

# Same, but split into available (non-missing) and missing values
crops %>%
  group_by(measure) %>%
  summarise(total     = n(),
            available = sum(!is.na(value)),
            missing   = sum(is.na(value)))


# --- 5. Check winter wheat yield observations --------------------------------
ww_yield <- crops %>%
  filter(var == "ww", measure == "yield")

nrow(ww_yield)                       # total rows
sum(!is.na(ww_yield$value))          # rows with a yield value
n_distinct(ww_yield$district_no)     # number of districts
summary(ww_yield$value)              # yield distribution (t/ha)

# Observations with a yield value, per year
ww_yield %>%
  group_by(year) %>%
  summarise(n_obs = sum(!is.na(value))) %>%
  print(n = Inf)
#How many unique districts are there?
n_distinct(crops$district_no)

#How many districts have winter wheat yield observations in 2024?
ww_yield %>%
  filter(year == 2024, !is.na(value)) %>%
  summarise(n_districts = n_distinct(district_no))

#Check the number of missing winter wheat yield values
ww_yield %>%
  summarise(
    total_observations = n(),
    missing_values = sum(is.na(value)),
    available_values = sum(!is.na(value))
  )

#Check the outlier variable
table(ww_yield$outlier, useNA = "ifany")

#Check the number of available districts each year
ww_yield %>%
  group_by(year) %>%
  summarise(
    available_districts = n_distinct(district_no[!is.na(value)])
  ) %>%
  print(n = Inf)

#Check whether the missing values are concentrated in particular districts
ww_yield %>%
  group_by(district_no, district) %>%
  summarise(
    available_years = sum(!is.na(value)),
    missing_years = sum(is.na(value)),
    .groups = "drop"
  ) %>%
  arrange(available_years) %>%
  print(n = 20)

#Inspect the observation flagged as an outlier
ww_yield %>%
  filter(outlier == 1)

#Check 1: Confirm the number of unique districts in the winter wheat dataset.
n_distinct(ww_yield$district_no)

ww_yield %>%
  distinct(district_no, district) %>%
  count(district_no) %>%
  filter(n > 1)

ww_yield %>%
  group_by(district_no) %>%
  summarise(
    first_year = min(year),
    last_year = max(year),
    number_of_years = n_distinct(year),
    .groups = "drop"
  ) %>%
  arrange(number_of_years) %>%
  print(n = 20)

ww_yield %>%
  filter(year == 2024, !is.na(value)) %>%
  select(district_no, district, nuts_id, value) %>%
  arrange(district_no) %>%
  print(n = 30)


#List the 72 districts with different names
ww_yield %>%
  distinct(district_no, district, year) %>%
  group_by(district_no) %>%
  summarise(
    number_of_names = n_distinct(district),
    names = paste(sort(unique(district)), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(number_of_names > 1) %>%
  print(n = 20)

#B. Examine two district identifiers over time
ww_yield %>%
  filter(district_no %in% c("01003", "16056")) %>%
  distinct(district_no, district, nuts_id, year) %>%
  arrange(district_no, year) %>%
  print(n = 100)

#C. Check whether NUTS codes also change
ww_yield %>%
  group_by(district_no) %>%
  summarise(
    number_of_nuts_ids = n_distinct(nuts_id),
    nuts_ids = paste(sort(unique(nuts_id)), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(number_of_nuts_ids > 1)

#A small R exercise: inspect the district-year panel
ww_yield %>%
  count(district_no, year) %>%
  filter(n > 1)
#Then check the distribution of available years per district:
ww_yield %>%
  group_by(district_no) %>%
  summarise(
    available_years = sum(!is.na(value)),
    .groups = "drop"
  ) %>%
  summarise(
    minimum = min(available_years),
    median = median(available_years),
    maximum = max(available_years)
  )


#separate dataset for winter wheat
ww_panel <- ww_yield %>%
  filter(!is.na(value)) %>%
  select(
    district_no,
    district,
    nuts_id,
    year,
    yield = value
  )


glimpse(ww_panel)

dim(ww_panel)

summary(ww_panel$yield)

#Then calculate average yield by year:
annual_yield <- ww_panel %>%
  group_by(year) %>%
  summarise(
    mean_yield = mean(yield),
    median_yield = median(yield),
    sd_yield = sd(yield),
    n_districts = n(),
    .groups = "drop"
  )

#display the results
print(annual_yield, n = 46)

#first graph
library(ggplot2)

ggplot(annual_yield, aes(x = year, y = mean_yield)) +
  geom_line() +
  labs(
    title = "Average Winter Wheat Yield in Germany (1979–2024)",
    x = "Year",
    y = "Average yield"
  ) +
  theme_minimal()

#compare early and recent yields
ww_panel %>%
  mutate(
    period = case_when(
      year >= 1979 & year <= 1989 ~ "1979–1989",
      year >= 1990 & year <= 1999 ~ "1990–1999",
      year >= 2000 & year <= 2009 ~ "2000–2009",
      year >= 2010 & year <= 2024 ~ "2010–2024"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    mean_yield = mean(yield),
    median_yield = median(yield),
    observations = n(),
    .groups = "drop"
  )
#Robustness check
complete_districts <- ww_panel %>%
  group_by(district_no) %>%
  summarise(
    years_available = n_distinct(year),
    .groups = "drop"
  ) %>%
  filter(years_available == 46) %>%
  pull(district_no)

length(complete_districts)

ww_panel %>%
  filter(district_no %in% complete_districts) %>%
  mutate(
    period = case_when(
      year <= 1989 ~ "1979–1989",
      year <= 1999 ~ "1990–1999",
      year <= 2009 ~ "2000–2009",
      TRUE ~ "2010–2024"
    )
  ) %>%
  group_by(period) %>%
  summarise(
    mean_yield = mean(yield),
    median_yield = median(yield),
    observations = n(),
    .groups = "drop"
  )

###new climate data
install.packages(c("terra", "sf", "exactextractr", "dplyr", "tidyr"))
library(terra)
library(sf)
library(exactextractr)
library(dplyr)
library(tidyr)

#checking if everything loaded
packageVersion("terra")
packageVersion("sf")
packageVersion("exactextractr")

#checking the extracted folder
districts <- st_read(
  "/Users//Downloads/EXTRACTED_FOLDER/VG250_eng.shp"
)

districts <- st_read(
  "vg250_ebenen_0101/VG250_KRS.shp"
)

names(districts)
head(st_drop_geometry(districts))
table(districts$GF)

# inspecting the names and types of the 401 features:
districts %>%
  st_drop_geometry() %>%
  filter(GF == 4) %>%
  count(BEZ, sort = TRUE)

#checking whether ARS and RS are unique:
districts %>%
  st_drop_geometry() %>%
  filter(GF == 4) %>%
  summarise(
    total = n(),
    unique_ARS = n_distinct(ARS),
    unique_RS = n_distinct(RS)
  )

#Comparing the shapefile with your agricultural data
districts %>%
  st_drop_geometry() %>%
  filter(GF == 4) %>%
  summarise(
    matching_districts = sum(ARS %in% crops$district_no),
    unmatched_districts = sum(!ARS %in% crops$district_no)
  )

#Identifyong the four unmatched districts
unmatched_districts <- districts %>%
  st_drop_geometry() %>%
  filter(
    GF == 4,
    !ARS %in% crops$district_no
  )

unmatched_districts %>%
  select(ARS, GEN, BEZ, NUTS)

#Preparing our matching boundary dataset
districts_397 <- districts %>%
  filter(
    GF == 4,
    ARS %in% crops$district_no
  )

#verifying
nrow(districts_397)

# Districts in agricultural data missing from the shapefile
setdiff(
  unique(crops$district_no),
  districts_397$ARS
)

# Districts in shapefile missing from agricultural data
setdiff(
  districts_397$ARS,
  unique(crops$district_no)
)

character(0)


## saving our prepared district boundaries so we don't lose this work:
st_write(
  districts_397,
  "districts_397.gpkg",
  delete_dsn = TRUE
)

save.image("FMCASS_research.RData")

#Installing and loading the NetCDF package
install.packages("ncdf4")
library(ncdf4)

#download the data
download.file(
  url = "https://opendata.dwd.de/climate_environment/CDC/grids_germany/daily/hyras_de/air_temperature_mean/tas_hyras_1_2020_v6-1_de.nc",
  destfile = "temperature_2020.nc",
  mode = "wb"
)

#open the file
temp <- nc_open("temperature_2020.nc")

#inspecting variables
names(temp$var)

#dimensions as well
names(temp$dim)

##Checking the temperature data
#dimensions
ncvar_get(temp, "tas") |> dim()

#Checking the temperature units
ncatt_get(temp, "tas", "units")

#Checking the time range
length(temp$dim$time$vals)

#checking the time units
ncatt_get(temp, "time", "units")

#Checking the spatial coordinates
temp$dim$x$vals[1:5]
temp$dim$y$vals[1:5]

ncatt_get(temp, "crs", "spatial_ref")

#convert the time values into dates
time_values <- temp$dim$time$vals

time_units <- as.Date("1881-01-01")

temperature_dates <- as.POSIXct("1881-01-01 00:00:00", tz = "UTC") +
  time_values * 3600

head(temperature_dates)
tail(temperature_dates)

#checking the district coordination system
st_crs(districts_397)

#Loading the temperature raster
temp_raster <- rast("temperature_2020.nc", subds = "tas")

temp_raster


#Checking the raster
crs(temp_raster)
nlyr(temp_raster)
res(temp_raster)

#Transforming the district boundaries
districts_laea <- st_transform(districts_397, crs(temp_raster))
  
#checking
st_crs(districts_laea)

#Selecting the winter wheat growing season
# Select January to July 2020
temp_jan_jul <- temp_raster[[1:212]]

# Calculate average daily temperature for each grid cell
temp_mean_jan_jul <- app(temp_jan_jul, mean, na.rm = TRUE)

#Extracting average temperatures for each district
district_temp_2020 <- exact_extract(
  temp_mean_jan_jul,
  districts_laea,
  "mean"
)

#Adding the results to your district data
districts_laea$temp_jan_jul_2020 <- district_temp_2020

district_temp_table <- districts_laea %>%
  st_drop_geometry() %>%
  select(ARS, GEN, temp_jan_jul_2020)

head(district_temp_table)
summary(district_temp_table$temp_jan_jul_2020)

#checking the number of districts
length(district_temp_2020)
sum(is.na(district_temp_2020))

#saving the 2020 temp. table
write.csv(
  district_temp_table,
  "district_temperature_2020.csv",
  row.names = FALSE
)

#Downloading the 2020 monthly files
# Monthly mean temperature
download.file(
  "https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/hyras_de/air_temperature_mean/tas_hyras_1_2020_v6-1_de_monmean.nc",
  "temperature_monthly_2020.nc",
  mode = "wb"
)

# Monthly precipitation totals
download.file(
  "https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/hyras_de/precipitation/pr_hyras_1_2020_v6-1_de_monsum.nc",
  "precipitation_monthly_2020.nc",
  mode = "wb"
)

#loading both files
temp_monthly <- rast(
  "temperature_monthly_2020.nc",
  subds = "tas"
)

prec_monthly <- rast(
  "precipitation_monthly_2020.nc",
  subds = "pr"
)

#then inspecting
temp_monthly
prec_monthly

nlyr(temp_monthly)
nlyr(prec_monthly)

crs(temp_monthly)
crs(prec_monthly)

#Calculate the 2020 growing-season indicators
# January to July 2020: layers 1 to 7
temp_jan_jul_monthly <- temp_monthly[[1:7]]
prec_jan_jul_monthly <- prec_monthly[[1:7]]

# Number of days in each month of 2020
days <- c(31, 29, 31, 30, 31, 30, 31)

# Calculate day-weighted mean temperature
temp_jan_jul_mean <- (
  temp_jan_jul_monthly[[1]] * days[1] +
    temp_jan_jul_monthly[[2]] * days[2] +
    temp_jan_jul_monthly[[3]] * days[3] +
    temp_jan_jul_monthly[[4]] * days[4] +
    temp_jan_jul_monthly[[5]] * days[5] +
    temp_jan_jul_monthly[[6]] * days[6] +
    temp_jan_jul_monthly[[7]] * days[7]
) / sum(days)

# Sum precipitation from January to July
prec_jan_jul_sum <- app(prec_jan_jul_monthly, sum, na.rm = TRUE)

#extracting district level values
temp_district_2020 <- exact_extract(
  temp_jan_jul_mean,
  districts_laea,
  "mean"
)

prec_district_2020 <- exact_extract(
  prec_jan_jul_sum,
  districts_laea,
  "mean"
)
#check the results
summary(temp_district_2020)
summary(prec_district_2020)

sum(is.na(temp_district_2020))
sum(is.na(prec_district_2020))

# Download October–December 2019 temperature data
download.file(
  "https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/hyras_de/air_temperature_mean/tas_hyras_1_2019_v6-1_de_monmean.nc",
  "temperature_monthly_2019.nc",
  mode = "wb"
)

# Download October–December 2019 precipitation data
download.file(
  "https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/hyras_de/precipitation/pr_hyras_1_2019_v6-1_de_monsum.nc",
  "precipitation_monthly_2019.nc",
  mode = "wb"
)

#loading them
temp_2019 <- rast(
  "temperature_monthly_2019.nc",
  subds = "tas"
)

prec_2019 <- rast(
  "precipitation_monthly_2019.nc",
  subds = "pr"
)

temp_2019
prec_2019

##now calculating the complete October 2019–July 2020 growing season
# Combine the 3 months from 2019 and 7 months from 2020
temp_growing_season <- c(
  temp_2019[[10:12]],
  temp_monthly[[1:7]]
)

prec_growing_season <- c(
  prec_2019[[10:12]],
  prec_monthly[[1:7]]
)
#calculating growing season temperature
days_growing <- c(31, 30, 31, 31, 29, 31, 30, 31, 30, 31)

#calculating the day weighted mean
temp_growing_mean <- (
  temp_growing_season[[1]] * days_growing[1] +
    temp_growing_season[[2]] * days_growing[2] +
    temp_growing_season[[3]] * days_growing[3] +
    temp_growing_season[[4]] * days_growing[4] +
    temp_growing_season[[5]] * days_growing[5] +
    temp_growing_season[[6]] * days_growing[6] +
    temp_growing_season[[7]] * days_growing[7] +
    temp_growing_season[[8]] * days_growing[8] +
    temp_growing_season[[9]] * days_growing[9] +
    temp_growing_season[[10]] * days_growing[10]
) / sum(days_growing)

#Calculating total growing-season precipitation
prec_growing_sum <- app(
  prec_growing_season,
  sum,
  na.rm = TRUE
)
#Extracting district-level climate indicators
temp_district_2020_full <- exact_extract(
  temp_growing_mean,
  districts_laea,
  "mean"
)

prec_district_2020_full <- exact_extract(
  prec_growing_sum,
  districts_laea,
  "mean"
)

#Creating the final 2020 climate table
climate_2020 <- data.frame(
  district_no = districts_laea$ARS,
  district = districts_laea$GEN,
  year = 2020,
  temperature = temp_district_2020_full,
  precipitation = prec_district_2020_full
)

head(climate_2020)
summary(climate_2020)

#saving 2020 dataset
write.csv(
  climate_2020,
  "climate_2020.csv",
  row.names = FALSE
)

#testing the download of 2000 temp.
download.file(
  "https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/hyras_de/air_temperature_mean/tas_hyras_1_2000_v6-1_de_monmean.nc",
  "temperature_monthly_2000.nc",
  mode = "wb"
)
#testing the download of 2000 precipitation
download.file(
  "https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/hyras_de/precipitation/pr_hyras_1_2000_v6-1_de_monsum.nc",
  "precipitation_monthly_2000.nc",
  mode = "wb"
)

####Creating a reusable function
process_climate_year <- function(harvest_year, districts) {
  
  # Helper: download a file only if it does not already exist
  get_file <- function(url, filename) {
    if (!file.exists(filename)) {
      download.file(url, filename, mode = "wb", quiet = TRUE)
    }
    if (!file.exists(filename) || file.info(filename)$size == 0) {
      stop("File download failed: ", filename)
    }
  }
  
  # Previous year and harvest year
  previous_year <- harvest_year - 1
  
  # File names
  temp_prev_file <- paste0("temperature_monthly_", previous_year, ".nc")
  temp_curr_file <- paste0("temperature_monthly_", harvest_year, ".nc")
  prec_prev_file <- paste0("precipitation_monthly_", previous_year, ".nc")
  prec_curr_file <- paste0("precipitation_monthly_", harvest_year, ".nc")
  
  # Download temperature files
  get_file(
    paste0("https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/hyras_de/air_temperature_mean/tas_hyras_1_", previous_year, "_v6-1_de_monmean.nc"),
    temp_prev_file
  )
  
  get_file(
    paste0("https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/hyras_de/air_temperature_mean/tas_hyras_1_", harvest_year, "_v6-1_de_monmean.nc"),
    temp_curr_file
  )
  
  # Download precipitation files
  get_file(
    paste0("https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/hyras_de/precipitation/pr_hyras_1_", previous_year, "_v6-1_de_monsum.nc"),
    prec_prev_file
  )
  
  get_file(
    paste0("https://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/hyras_de/precipitation/pr_hyras_1_", harvest_year, "_v6-1_de_monsum.nc"),
    prec_curr_file
  )
  
  # Load monthly rasters
  temp_prev <- rast(temp_prev_file, subds = "tas")
  temp_curr <- rast(temp_curr_file, subds = "tas")
  
  prec_prev <- rast(prec_prev_file, subds = "pr")
  prec_curr <- rast(prec_curr_file, subds = "pr")
  
  # October-December of previous year + January-July of harvest year
  temp_season <- c(temp_prev[[10:12]], temp_curr[[1:7]])
  prec_season <- c(prec_prev[[10:12]], prec_curr[[1:7]])
  
  # Calculate the actual number of days in each month
  month_starts <- seq(
    as.Date(paste0(previous_year, "-10-01")),
    by = "month",
    length.out = 11
  )
  
  days <- diff(as.numeric(month_starts))
  
  # Day-weighted mean temperature
  temp_mean <- app(
    temp_season,
    function(x) weighted.mean(x, days, na.rm = TRUE)
  )
  
  # Total growing-season precipitation
  prec_sum <- app(
    prec_season,
    sum,
    na.rm = TRUE
  )
  
  # Extract district-level averages
  temp_values <- exact_extract(temp_mean, districts, "mean")
  prec_values <- exact_extract(prec_sum, districts, "mean")
  
  # Return a district-level dataset
  result <- data.frame(
    district_no = districts$ARS,
    district = districts$GEN,
    year = harvest_year,
    temperature = temp_values,
    precipitation = prec_values
  )
  
  return(result)
}
#testing the function for 2001
climate_2001 <- process_climate_year(
  harvest_year = 2001,
  districts = districts_laea
)

head(climate_2001)
summary(climate_2001)

#Checking the number of districts and missing values:
nrow(climate_2001)

sum(is.na(climate_2001$temperature))
sum(is.na(climate_2001$precipitation))

######Processing 2000–2024######
# Define analysis years
years <- 2000:2024

# Create an empty list for results
climate_results <- list()

# Process each year
for (yr in years) {
  
  message("Processing harvest year: ", yr)
  
  climate_results[[as.character(yr)]] <-
    process_climate_year(
      harvest_year = yr,
      districts = districts_laea
    )
  
  # Save progress after each year
  saveRDS(
    climate_results,
    "climate_progress.rds"
  )
  
  gc()
}

# Combine all years into one dataset
climate_all <- dplyr::bind_rows(climate_results)

# Save final dataset
write.csv(
  climate_all,
  "climate_2000_2024.csv",
  row.names = FALSE
)

saveRDS(
  climate_all,
  "climate_2000_2024.rds"
)

##check the final dataset
dim(climate_all)

range(climate_all$year)

length(unique(climate_all$district_no))

sum(is.na(climate_all$temperature))
sum(is.na(climate_all$precipitation))

##Inspect the climate indicators
summary(climate_all$temperature)

summary(climate_all$precipitation)

# Merge winter wheat yields with climate data
ww_climate <- ww_panel %>%
  inner_join(
    climate_all,
    by = c("district_no", "year")
  )

# Inspect the merged dataset
dim(ww_climate)

head(ww_climate)

summary(ww_climate)

# Number of unique districts and years
length(unique(ww_climate$district_no))
range(ww_climate$year)

# Check missing values
colSums(is.na(ww_climate))


##Cleaning up the merged dataset
ww_climate <- ww_climate %>%
  select(
    district_no,
    district = district.x,
    nuts_id,
    year,
    yield,
    temperature,
    precipitation
  )

#Checking whether observations are balanced across years
ww_climate %>%
  count(year) %>%
  print(n = 25)

##Plot 1: Temperature and wheat yield
install.packages("ggplot2")
library(ggplot2)

ggplot(ww_climate, aes(x = temperature, y = yield)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Winter Wheat Yield and Growing-Season Temperature",
    x = "Growing-season temperature (°C)",
    y = "Winter wheat yield (tonnes/ha)"
  ) +
  theme_minimal()

##Plot 2: Precipitation and wheat yield
ggplot(ww_climate, aes(x = precipitation, y = yield)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Winter Wheat Yield and Growing-Season Precipitation",
    x = "Growing-season precipitation (mm)",
    y = "Winter wheat yield (tonnes/ha)"
  ) +
  theme_minimal()

##linegraph
annual_yield <- ww_climate %>%
  group_by(year) %>%
  summarise(
    mean_yield = mean(yield, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(annual_yield, aes(x = year, y = mean_yield)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Winter Wheat Yield in Germany (2000–2024)",
    x = "Year",
    y = "Average yield (tonnes/ha)"
  ) +
  theme_minimal()

##second line graph, showing how temperature and precipitation changed over the same period.
annual_climate <- ww_climate %>%
  group_by(year) %>%
  summarise(
    mean_temperature = mean(temperature, na.rm = TRUE),
    mean_precipitation = mean(precipitation, na.rm = TRUE),
    .groups = "drop"
  )
##
ggplot(annual_climate, aes(x = year, y = mean_temperature)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Growing-Season Temperature (2000–2024)",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  theme_minimal()
##
ggplot(annual_climate, aes(x = year, y = mean_precipitation)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Growing-Season Precipitation (2000–2024)",
    x = "Year",
    y = "Precipitation (mm)"
  ) +
  theme_minimal()

#########
getwd()
file.exists("FMCASS_research.RData")

folders <- c(
  "~/Desktop",
  "~/Documents",
  "~/Downloads"
)

for (folder in folders) {
  cat("\nSearching:", folder, "\n")
  print(list.files(
    path = folder,
    pattern = "FMCASS_research.RData|climate_2000_2024.rds|districts_397.gpkg",
    recursive = TRUE,
    full.names = TRUE
  ))
}


setwd("~/Documents/FMCASS-project")

getwd()

load("FMCASS_research.RData")

ls()

climate_all <- readRDS("climate_2000_2024.rds")

dim(climate_all)

library(dplyr)
library(fixest)
#####regression model######
ww_climate <- ww_panel %>%
  inner_join(
    climate_all,
    by = c("district_no", "year")
  ) %>%
  select(
    district_no,
    district = district.x,
    nuts_id,
    year,
    yield,
    temperature,
    precipitation
  ) %>%
  mutate(
    precipitation_100 = precipitation / 100
  )

dim(ww_climate)

model1 <- feols(
  yield ~ temperature + precipitation_100 |
    district_no + year,
  cluster = ~district_no,
  data = ww_climate
)

summary(model1)

save.image("FMCASS_research.RData")



###Model 2: squared climate variables
ww_climate <- ww_climate %>%
  mutate(
    temperature_c = temperature - mean(temperature),
    precipitation_c = precipitation_100 - mean(precipitation_100)
  )

model2 <- feols(
  yield ~ temperature_c + I(temperature_c^2) +
    precipitation_c + I(precipitation_c^2) |
    district_no + year,
  cluster = ~district_no,
  data = ww_climate
)

summary(model2)

# Calculate average climate conditions
mean_temp <- mean(ww_climate$temperature)
mean_precip <- mean(ww_climate$precipitation)

# Calculate turning points
temp_turning_point <- mean_temp -
  coef(model2)["temperature_c"] /
  (2 * coef(model2)["I(temperature_c^2)"])

precip_turning_point <- mean_precip +
  100 * (
    -coef(model2)["precipitation_c"] /
      (2 * coef(model2)["I(precipitation_c^2)"])
  )

# Display results
temp_turning_point
precip_turning_point

# Check observed ranges
range(ww_climate$temperature)
range(ww_climate$precipitation)

####This will show how the estimated relationship changes at different climate conditions
# Temperature marginal association
temp_marginal <- function(x) {
  coef(model2)["temperature_c"] +
    2 * coef(model2)["I(temperature_c^2)"] *
    (x - mean_temp)
}

# Precipitation marginal association per 100 mm
precip_marginal <- function(x) {
  coef(model2)["precipitation_c"] +
    2 * coef(model2)["I(precipitation_c^2)"] *
    ((x / 100) - mean_precip)
}

# Evaluate at selected climate values
temp_marginal(c(6, 8, 10, 12))

precip_marginal(c(400, 500, 600, 800, 1000))

summary(ww_climate$temperature)
summary(ww_climate$precipitation)


quantile(
  ww_climate$temperature,
  probs = c(0.05, 0.25, 0.50, 0.75, 0.95)
)

quantile(
  ww_climate$precipitation,
  probs = c(0.05, 0.25, 0.50, 0.75, 0.95)
)

mean_precip_100 <- mean(ww_climate$precipitation_100)

precip_marginal <- function(x) {
  coef(model2)["precipitation_c"] +
    2 * coef(model2)["I(precipitation_c^2)"] *
    ((x / 100) - mean_precip_100)
}

precip_marginal(c(400, 500, 600, 800, 1000))

wald(model2, keep = "temperature_c|precipitation_c")

##Now testing whether the quadratic terms improve the model
wald(
  model2,
  keep = "I\\(temperature_c\\^2\\)|I\\(precipitation_c\\^2\\)"
)
#inspecting the observations with the highest precipitation values.
ww_climate %>%
  arrange(desc(precipitation)) %>%
  select(district, year, precipitation, yield) %>%
  head(10)

summary(ww_climate$precipitation)

quantile(
  ww_climate$precipitation,
  probs = c(0.90, 0.95, 0.99, 0.995, 1),
  na.rm = TRUE
)
##running the sensitivity analysis
precip_cutoff <- quantile(
  ww_climate$precipitation,
  0.99,
  na.rm = TRUE
)

ww_climate_trimmed <- ww_climate %>%
  filter(precipitation <= precip_cutoff)

nrow(ww_climate)
nrow(ww_climate_trimmed)

model2_trimmed <- feols(
  yield ~ temperature_c + I(temperature_c^2) +
    precipitation_c + I(precipitation_c^2) |
    district_no + year,
  cluster = ~district_no,
  data = ww_climate_trimmed
)

etable(model2, model2_trimmed)

##making table of regression results
library(modelsummary)

models <- list(
  "Model 1: Linear" = model1,
  "Model 2: Quadratic" = model2
)

modelsummary(
  models,
  statistic = "({std.error})",
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  coef_omit = "Intercept",
  output = "regression_results.docx"
)
##Step 1. Temperature and winter wheat yield
library(ggplot2)

temp_plot_data <- data.frame(
  temperature = seq(
    min(ww_climate$temperature),
    max(ww_climate$temperature),
    length.out = 100
  )
)

temp_plot_data$predicted_yield <-
  coef(model2)["temperature_c"] *
  (temp_plot_data$temperature - mean(ww_climate$temperature)) +
  coef(model2)["I(temperature_c^2)"] *
  (temp_plot_data$temperature - mean(ww_climate$temperature))^2

ggplot(temp_plot_data, aes(x = temperature, y = predicted_yield)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Temperature and Winter Wheat Yield",
    subtitle = "Estimated quadratic association, precipitation held at its mean",
    x = "Growing-season mean temperature (°C)",
    y = "Estimated yield association (tonnes/ha)"
  ) +
  theme_minimal()

#Step 2. Precipitation and winter wheat yield
precip_plot_data <- data.frame(
  precipitation = seq(
    min(ww_climate$precipitation),
    max(ww_climate$precipitation),
    length.out = 100
  )
)

precip_plot_data$predicted_yield <-
  coef(model2)["precipitation_c"] *
  ((precip_plot_data$precipitation / 100) -
     mean(ww_climate$precipitation_100)) +
  coef(model2)["I(precipitation_c^2)"] *
  ((precip_plot_data$precipitation / 100) -
     mean(ww_climate$precipitation_100))^2

ggplot(precip_plot_data,
       aes(x = precipitation, y = predicted_yield)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Precipitation and Winter Wheat Yield",
    subtitle = "Estimated quadratic association, temperature held at its mean",
    x = "Growing-season precipitation (mm)",
    y = "Estimated yield association (tonnes/ha)"
  ) +
  theme_minimal()

##sensitivity anaysis confirmation
etable(
  model2,
  model2_trimmed,
  headers = c("Original model", "Restricted sample"),
  fitstat = c("n", "r2", "ar2", "wr2", "rmse")
)

#making table

models_sensitivity <- list(
  "Original model" = model2,
  "Restricted sample" = model2_trimmed
)

modelsummary(
  models_sensitivity,
  coef_map = c(
    "temperature_c" = "Temperature (centred)",
    "I(temperature_c^2)" = "Temperature² (centred)",
    "precipitation_c" = "Precipitation (centred)",
    "I(precipitation_c^2)" = "Precipitation² (centred)"
  ),
  estimate = "{estimate}{stars}",
  statistic = "({std.error})",
  stars = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
  gof_map = c(
    "nobs" = "Observations",
    "r.squared" = "R²",
    "adj.r.squared" = "Adjusted R²",
    "rmse" = "RMSE"
  ),
  add_rows = data.frame(
    term = c(
      "District fixed effects",
      "Year fixed effects",
      "Within R²"
    ),
    `Original model` = c("Yes", "Yes", "0.0560"),
    `Restricted sample` = c("Yes", "Yes", "0.0656"),
    check.names = FALSE
  ),
  output = "table2_sensitivity.docx"
)