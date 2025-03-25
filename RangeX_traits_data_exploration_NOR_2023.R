
# RangeX data exploration demographic and functional traits ------------

## Data used: RangeX_clean_functional_traits_NOR_2023.csv
##            RangeX_clean_yearly_size_2021_2022_2023_NOR.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      24.03.25
## Author:    Nadine Arzt
## Purpose:   Combine functional traits with demographic traits and
##            explore the data


# questions / comments ----------------------------------------------------
# keep only the 597 plants where we have functional and demographic traits

# keep date in data cleaning 23

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(FactoMineR)
library(factoextra)

theme_set(theme_bw())

# import data -------------------------------------------------------------
# functional traits 2023
functional_traits <- read.csv("Data/RangeX_clean_functional_traits_NOR_2023.csv")

functional_traits <- functional_traits |> 
  select(-X)
head(functional_traits)

# demographic traits 2021-23
demo_traits <- read.csv("Data/RangeX_clean_yearly_size_2021_2022_2023_NOR.csv")

# filter only 2023
demographic_traits <- demo_traits |> 
  filter(year == 2023)

# combine functional and demographic traits -------------------------------
traits <- left_join(demographic_traits, functional_traits,
                    by = c("unique_plant_ID", "species"))


# import meta data --------------------------------------------------------
metadata <- read.csv("Data/RangeX_metadata_focal_NOR.csv")
head(metadata)
dput(colnames(metadata))


# merge trait data with meta data -----------------------------------------
traits_23 <- left_join(metadata, traits, by = c("unique_plant_ID", "species"))
nrow(traits_23)

traits_23 <- traits_23 |> 
  mutate(combined_treatment = paste(site, treat_warming, treat_competition, sep = "_"))


# species names -----------------------------------------------------------
traits_23 <- traits_23 |> 
  mutate(species = case_when(
    species == "cyncri" ~ "Cynosurus cristatus",
    species == "sucpra" ~ "Succisa pratensis",
    species == "hypmac" ~ "Hypericum maculatum",
    species == "cennig" ~ "Centaurea nigra",
    species == "leuvul" ~ "Leucanthemum vulgare",
    species == "sildio" ~ "Silene dioica",
    species == "plalan" ~ "Plantago lanceolata",
    species == "luzmul" ~ "Luzula multiflora",
    species == "tripra" ~ "Trifolium pratense",
    species == "pimsax" ~ "Pimpinella saxifraga",
    TRUE ~ species  # Keep other species unchanged
  ))


# plotting functional traits ----------------------------------------------
# SLA ---------------------------------------------------------------------
ggplot(traits_23, aes(combined_treatment, SLA, fill = combined_treatment))+
  geom_boxplot()+
  facet_wrap(vars(species), ncol = 5)+
  labs(x = "Treatment", y = "SLA (mm^2/mg)")+
  theme(legend.position = "bottom")


# LDMC ---------------------------------------------------------------------
ggplot(traits_23, aes(combined_treatment, LDMC, fill = combined_treatment))+
  geom_boxplot()+
  facet_wrap(vars(species), ncol = 5)+
  labs(x = "Treatment", y = "LDMC (mg g-1)")+
  theme(legend.position = "bottom")












# delete samples without functional trait measurements -------------------------------------
traits_NOR_23 <- traits_23 |> 
  filter(!is.na(wet_mass) | !is.na(dry_mass) | !is.na(leaf_area) 
         | !is.na(leaf_thickness) | !is.na(SLA) | !is.na(LDMC))

length(traits_NOR_23$region) # 576 oh no, why not 597?




# PCA ---------------------------------------------------------------------
# Convert integer columns to numeric
traits_NOR_23 <- traits_NOR_23 |> 
  mutate(across(where(is.integer), as.numeric))

# Fill missing values with the mean of each column
traits_NOR_23_PCA <- traits_NOR_23 |> 
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE))))

# Perform PCA


# Select numerical columns for PCA
numerical_columns <- traits_NOR_23_PCA |> 
  select(height_vegetative_str, height_reproductive_str,
         height_vegetative, height_reproductive,
         leaf_length1, leaf_width, 
         petiole_length, 
         number_leaves, number_tillers, number_flowers)

# ok, so you cant out in block ... 

# Perform PCA
pca_result <- PCA(numerical_columns, scale.unit = TRUE, ncp = 2, graph = FALSE)

# Visualize PCA results - species
fviz_pca_ind(pca_result, geom.ind = "point", pointshape = 21, 
             pointsize = 2, fill.ind = traits_NOR_23_PCA$species, 
             palette = "jco", addEllipses = TRUE, 
             label = "var", col.var = "black", repel = TRUE) +
  theme_minimal()

# treatment
fviz_pca_ind(pca_result, geom.ind = "point", pointshape = 21, 
             pointsize = 2, fill.ind = traits_NOR_23_PCA$combined_treatment, 
             palette = "jco", addEllipses = TRUE, 
             label = "var", col.var = "black", repel = TRUE) +
  theme_minimal()

