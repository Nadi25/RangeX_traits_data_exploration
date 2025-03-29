
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

# check in CHE:
# very high SLA > 60
# high leaf area but low dry mass
# deleted for now the whole leaf
# CHE.hi.warm.bare.wf.09.23.1 scacol
# CHE.hi.ambi.vege.wf.05.16.1 brapin 
# CHE.hi.ambi.bare.wf.09 hypper
# CHE.hi.ambi.vege.wf.10 hypper
# CHE.hi.ambi.bare.wf.09.05.1 hypper

# LDMC
# delete?
# too high
# CHE.hi.ambi.vege.wf.10.16.1 brapin
# CHE.hi.ambi.vege.wf.06.01.1 brapin
# too low
# CHE.lo.ambi.bare.wf.09.16.2 brapin


# check in NOR:
# NOR SLA > 60
# wm 19.61, dm 2.13, lt 0.21200000, la 128.5, sla 60.32864, ldmc 108.6181
# NOR.hi.warm.vege.wf.04.13 sildio hi 4 a b5
# delete for now

# NOR.lo.ambi.bare.wf.04.11 very low LDMC value in tripra
# delete for now

# what is wrong here?? NOR.lo.ambi.bare.wf.07.30 cenig
# delete for now


# make decision about nathan height 

# load library ------------------------------------------------------------
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)
library(tidyverse)
library(broom)
library(lmerTest)
library(ggsignif)
library(plotly)
conflicts_prefer(plotly::layout)
library(colorspace)
library(scales)
library(ggradar)
library(fmsb)
library(vegan)

library(FactoMineR)
library(factoextra)

theme_set(theme_bw(base_size = 20))

# define colors for each treatment
define_colors <- c(
  "hi ambi vege" = "turquoise4",
  "hi ambi bare" = lighten("turquoise4", 0.7),
  "hi warm vege" = "darkred",
  "hi warm bare" = lighten("darkred", 0.7),
  "lo ambi vege" = "grey34",
  "lo ambi bare" = lighten("grey34", 0.7))

# import data -------------------------------------------------------------
# functional traits 2023 NOR ----------------------------------------------
functional_traits_NOR <- read.csv("Data/RangeX_clean_functional_traits_NOR_2023.csv")

functional_traits_NOR <- functional_traits_NOR |> 
  select(-X)
head(functional_traits_NOR)

# change name to date_collection
functional_traits_NOR <- functional_traits_NOR |> 
  rename("date_collection" = "date_collected") |> 
  mutate(date_collection = as.Date(date_collection))



# functional traits CHE ---------------------------------------------------
functional_traits_CHE <- read.csv("Data/RangeX_clean_LeafTraits_2022_2023_CHE.csv")

# filter 2023
functional_traits_CHE$date_collection <- as.Date(functional_traits_CHE$date_collection)

functional_traits_CHE <- functional_traits_CHE |> 
  filter(format(date_collection, "%Y") == "2023")

functional_traits_CHE <- functional_traits_CHE |> 
  select(-c(C_N, C13))


# demographic traits 2021-23 NOR ------------------------------------------
demo_traits_NOR <- read.csv("Data/RangeX_clean_yearly_size_2021_2022_2023_NOR.csv")

# filter only 2023
demographic_traits_NOR <- demo_traits_NOR |> 
  filter(year == 2023)

# species names demo NOR
demographic_traits_NOR <- demographic_traits_NOR |> 
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

demographic_traits_NOR <- demographic_traits_NOR |> 
  select(-c(year, vegetative_length))


# demographic traits 2021-23 CHE ------------------------------------------
demo_traits_CHE <- read.csv("Data/RangeX_clean_YearlyDemographics_2021_2023_CHE.csv")

# filter only 2023
demo_traits_CHE$date_measurement <- as.Date(demo_traits_CHE$date_measurement)

demographic_traits_CHE <- demo_traits_CHE |> 
  filter(format(date_measurement, "%Y") == "2023")

# filter relevant columns
# this can change later when I update demo NOR data cleaning 
demographic_traits_CHE <- demographic_traits_CHE |> 
  select(-c(functional_group, date_measurement, date_planting, survival,
            height_nathan))

# species names
demographic_traits_CHE <- demographic_traits_CHE |> 
  mutate(species = case_when(
    species == "brapin" ~ "Brachypodium pinnatum",
    species == "daucar" ~ "Daucus carota",
    species == "broere" ~ "Bromus erectus",
    species == "hypper" ~ "Hypericum perforatum",
    species == "scacol" ~ "Scabiosa columbaria",
    species == "medlup" ~ "Medicago lupulina",
    species == "plamed" ~ "Plantago media",
    species == "silvul" ~ "Silene vulgaris",
    species == "cenjac" ~ "Centaurea jacea",
    species == "salpra" ~ "Salvia pratensis",
    TRUE ~ species  # Keep other species unchanged
  ))


# import meta data --------------------------------------------------------
# NOR
metadata_NOR <- read.csv("Data/RangeX_metadata_focal_NOR.csv")
head(metadata_NOR)
dput(colnames(metadata_NOR))
# CHE
metadata_CHE <- read.csv("Data/RangeX_clean_MetadataFocal_CHE.csv")
head(metadata_CHE)
dput(colnames(metadata_CHE))


# merge functional trait data with meta data NOR -----------------------------
traits_23_NOR <- left_join(metadata_NOR, functional_traits_NOR, by = c("unique_plant_ID", "species"))
nrow(traits_23_NOR)

traits_23_NOR <- traits_23_NOR |> 
  mutate(combined_treatment = paste(site, treat_warming, treat_competition, sep = " "))

traits_23_NOR <- traits_23_NOR |> 
  select(-X)

traits_23_NOR <- traits_23_NOR |> 
  filter(!unique_plant_ID %in% c("NOR.hi.warm.vege.wf.04.13", 
                                 "NOR.lo.ambi.bare.wf.04.11",
                                 "NOR.lo.ambi.bare.wf.07.30"))

# NOR.lo.ambi.bare.wf.04.11 dont delete?


# species names -----------------------------------------------------------
traits_23_NOR <- traits_23_NOR |> 
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

# merge functional trait data with meta data CHE ----------------------------
traits_23_CHE <- left_join(metadata_CHE, functional_traits_CHE, by = c("unique_plant_ID", "species"))
nrow(traits_23_CHE)

traits_23_CHE <- traits_23_CHE |> 
  mutate(combined_treatment = paste(site, treat_warming, treat_competition, sep = " "))


# delete leaves with too high SLA or LDMC-----------------------------------
traits_23_CHE <- traits_23_CHE |> 
  filter(!unique_plant_ID %in% c("CHE.hi.warm.bare.wf.09.23.1", 
                                 "CHE.hi.ambi.vege.wf.05.16.1",
                                 "CHE.hi.ambi.vege.wf.10.18.1",
                                 "CHE.hi.ambi.bare.wf.09.05.1",
                                 "CHE.hi.ambi.vege.wf.10.16.1",
                                 "CHE.hi.ambi.vege.wf.06.01.1",
                                 "CHE.lo.ambi.bare.wf.09.16.2"))

# species names
traits_23_CHE <- traits_23_CHE |> 
  mutate(species = case_when(
    species == "brapin" ~ "Brachypodium pinnatum",
    species == "daucar" ~ "Daucus carota",
    species == "broere" ~ "Bromus erectus",
    species == "hypper" ~ "Hypericum perforatum",
    species == "scacol" ~ "Scabiosa columbaria",
    species == "medlup" ~ "Medicago lupulina",
    species == "plamed" ~ "Plantago media",
    species == "silvul" ~ "Silene vulgaris",
    species == "cenjac" ~ "Centaurea jacea",
    species == "salpra" ~ "Salvia pratensis",
    TRUE ~ species  # Keep other species unchanged
  ))

# combine functional NOR with CHE ----------------------------------------
names(traits_23_NOR)
names(traits_23_CHE)

traits_23_CHE <- traits_23_CHE |> 
  mutate(plot_ID_original = as.character(plot_ID_original),
         position_ID_original = as.character(position_ID_original))

traits_NOR_CHE <- bind_rows(traits_23_NOR, traits_23_CHE)

traits_NOR_CHE <- traits_NOR_CHE |> 
  mutate(region = case_when(region == "CHE" ~ "Switzerland", 
                            region == "NOR" ~ "Norway"))

na_counts <- colSums(is.na(traits_NOR_CHE))
na_counts

# filter interesting species CHE NOR ------------------------------
# leuvul
# both hypericums 
# both plantagos
# cynosurus
# bromus
# medicago

# checking CHE outlier ----------------------------------------------------
ggplot(traits_23_CHE, aes(combined_treatment, SLA, fill = combined_treatment)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
  facet_wrap(vars(species), ncol = 5) +
  labs(x = "Treatment", y = "SLA (mm^2/mg)") +
  theme(legend.position = "bottom") +
  geom_jitter(alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", 
               shape = 20, size = 2, color = "red")
# very high SLA > 60
# high leaf area but low dry mass
# CHE.hi.warm.bare.wf.09.23.1 scacol
# CHE.hi.ambi.vege.wf.05.16.1 brapin 
# CHE.hi.ambi.bare.wf.09 hypper
# CHE.hi.ambi.vege.wf.10 hypper

# NOR.hi.warm.vege.wf.04 sildio NOR


# combine functional and demographic traits NOR ------------------------------
traits_fun_demo_NOR <- left_join(demographic_traits_NOR, 
                                 traits_23_NOR,
                                 by = c("unique_plant_ID", "species"))


# combine functional and demographic traits CHE ------------------------------
traits_fun_demo_CHE <- left_join(demographic_traits_CHE, 
                                 traits_23_CHE,
                                 by = c("unique_plant_ID", "species"))



# combine NOR CHE functional and demographic traits -----------------------
names(traits_fun_demo_NOR)
names(traits_fun_demo_CHE)
traits_fun_demo_NOR_CHE <- bind_rows(traits_fun_demo_NOR, 
                                     traits_fun_demo_CHE)



