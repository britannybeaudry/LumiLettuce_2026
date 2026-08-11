# -------------------------
# Load libraries
# -------------------------
# install packages 
library(tidyverse)
library(here)
library(tools)
library(readxl)
library(stringr)

# -----------------------------------
# Create raw data path for microbiome
# -----------------------------------
# getwd()
## "C:/Users/brita/OneDrive/Desktop/UAlg/thesis/LumiLettuce"
data_raw_path <- here::here("data/raw/microbiome_results") # use function here()
data_raw_path
list.files(data_raw_path)

folder_list <- list.files(path = data_raw_path, pattern = "\\.tsv$", full.names = TRUE)
length(folder_list) # there are 89 files available
folder_list # this is to get the name of all the files

# -----------------------------------------
# Combine all .tsv files into one big file
# -----------------------------------------
microbiome <- folder_list |> 
  map_df(~ {
    # 1. Read the current file
    df <- read_tsv(.x, show_col_types = FALSE)
    
    # 2. Grab the unique sample name from the 2nd column header
    sample_name <- names(df)[2]
    
    # 3. Process and return the data frame
    df |> 
      rename(tax = 1, count = 2) |> 
      mutate(sample_id = sample_name) |> 
      select(sample_id, tax, count) # Explicitly order the columns
  })
View(microbiome)

# -------------------------
# Inspect the microbiome data set
# -------------------------
# View(microbiome)
dplyr::glimpse(microbiome) # nrow = 18,988
colnames(microbiome) # sample_id, tax, count
head(microbiome)
tail(microbiome)
summary(microbiome)

# -------------------------
# Clean the column 'tax' 
# -------------------------
microbiome |> # this tells me how many elements are delimited 
  mutate(num_elements = str_count(tax, ";") + 1) |>
  count(num_elements)
#    num_elements     n
# 1      7          18862 
# 2      8           126
# Some have 8 levels which means some might have a species variable (which we do not want)

microbiome <- microbiome |>
  mutate(tax = str_trim(tax)) |> 
  separate_wider_delim(
    cols = tax,
    delim = ";",
    # We only define the 7 columns we want to keep
    names = c("superkingdom", "kingdom", "phylum", "class", "order", "family", "genus"),
    too_few = "align_start", 
    too_many = "drop"  # allows us to ignore the 8th level (Species) 
  )

# Now check if it worked
colnames(microbiome)
head(microbiome)

unique(microbiome$sample_id) # CM is for community standard, water samples end with "w"
unique(microbiome$superkingdom) # "Bacteria"     "Unclassified" "Eukaryota"
unique(microbiome$kingdom) 
unique(microbiome$phylum)
unique(microbiome$class)
unique(microbiome$order)
unique(microbiome$family)
unique(microbiome$genus)


# The sample_id column has values ending with "Bd" and others with "N" which i want to remove to make it more consistent across datasets.
microbiome <- microbiome %>%
  mutate(
    sample_id = str_remove(sample_id, "Bd$"),  # Removes "Bd" only at the end of the string
    sample_id = str_remove(sample_id, "N$")    # Removes "N" only at the end of the string
  )

# -----------------------------------------
# Assign the correct variable type to each 
# -----------------------------------------
microbiome <- microbiome |>
  dplyr::mutate(
    sample_id          = as.character(sample_id),
    superkingdom       = as.character(superkingdom),
    kingdom            = as.character(kingdom),
    phylum             = as.character(phylum),
    class              = as.character(class),
    order              = as.character(order),
    family             = as.character(family),
    genus              = as.character(genus),
    count              = as.numeric(count)
  )
dplyr::glimpse(microbiome)

# ----------------------------------
# Create raw data path for metadata
# ----------------------------------
metadata_raw <- readxl::read_excel("data/raw/metadata_raw.xlsx") 
View(metadata_raw)

# rename "field_sample_id" column to "sample_id" to be consistent with the microbiome data set

metadata_raw <- dplyr::rename(metadata_raw, sample_id=field_sample_id)

# -----------------------------------------
# Assign the correct variable type to each 
# -----------------------------------------

metadata <- metadata_raw |>
  dplyr::mutate(
    collection_date      = as.Date(collection_date),
    season               = as.character(season),
    diel_phase           = factor(diel_phase, levels = c("Midnight", "Dawn", "Noon", "Dusk")),
    site_name            = as.character(site_name),
    sample_id            = as.character(sample_id),
    sample_type          = as.character(sample_type),
    morphotype           = as.character(morphotype),
    host_species         = as.character(host_species),
    tide_state           = factor(tide_state, levels = c("High", "Low", "Ebb", "Flood")),
    sky_condition        = factor(sky_condition, levels = c("Clear", "Partly cloudy", "Overcast", "Foggy")),
    precipitation        = factor(precipitation, levels = c("None", "Drizzle", "Heavy rain", "Storm")),
    temp_degC            = as.numeric(temp_degC),
    dissolved_oxygen_mg_L= as.numeric(dissolved_oxygen_mg_L),
    pH                   = as.numeric(pH),
    salinity_psu         = as.numeric(salinity_psu),
    PAR_umol_photons_m2_s= as.numeric(PAR_umol_photons_m2_s)
    
  )

# -------------------------
# Complete data set
# -------------------------

# Let's merge the metadata and microbiome into one complete data set by using the sample_id as the variable in commun

full_data <- microbiome |>
  left_join(metadata, by = "sample_id")
View(full_data)

# -------------------------
# Export tidy data to disk
# -------------------------

microbiome_tidy <- microbiome
metadata_tidy <- metadata

data_tidy_path <- here::here("data/processed")

readr::write_csv(microbiome_tidy, file=file.path(data_tidy_path, "microbiome_tidy.csv"))
saveRDS(microbiome_tidy, file=file.path(data_tidy_path, "microbiome_tidy.rds"))

readr::write_csv(metadata_tidy, file=file.path(data_tidy_path, "metadata_tidy.csv"))
saveRDS(metadata_tidy, file=file.path(data_tidy_path, "metadata_tidy.rds"))

readr::write_csv(full_data, file=file.path(data_tidy_path, "full_data.csv"))
saveRDS(full_data, file=file.path(data_tidy_path, "full_data.rds"))








