
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
  filter(unique_plant_ID != "NOR.hi.warm.vege.wf.04.13")

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




# plotting functional traits NOR -------------------------------------------
# SLA ---------------------------------------------------------------------
ggplot(traits_23_NOR, aes(combined_treatment, SLA, fill = combined_treatment))+
  geom_boxplot()+
  facet_wrap(vars(species), ncol = 5)+
  labs(x = "Treatment", y = "SLA (mm^2/mg)")+
  theme(legend.position = "bottom")+
  geom_jitter(alpha = 0.5)+
  stat_summary(fun = mean, geom = "point", shape = 20, 
               size = 2, color = "red")


# Perform ANOVA for each species
results_anova <- traits_23_NOR |> 
  group_by(species) |> 
  do(tidy(aov(SLA ~ combined_treatment, data = .)))
print(results_anova)


# significance all species together ---------------------------------------
# all species together
# Fit the mixed-effects model
model_SLA <- lmerTest::lmer(SLA ~ combined_treatment + (1 | species) + 
                (1 | block_ID_original),
              data = traits_23_NOR)

# View the summary of the model
summary(model_SLA)
# species explains some variance: species = 24.937, block = 0.213
# hi_ambi_vege, hi_warm_vege, and lo_ambi_vege have significant effects on SLA, as indicated by their low p-values 
# SLA significantly higher in vegetated plots compared to bare


# Calculate means and confidence intervals for each treatment
summary_data <- traits_23_NOR |> 
  group_by(combined_treatment) |> 
  summarise(
    mean_SLA = mean(SLA, na.rm = TRUE),
    ci_lower = mean_SLA - qt(0.975, df = n() - 1) * sd(SLA, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_SLA + qt(0.975, df = n() - 1) * sd(SLA, na.rm = TRUE) / sqrt(n())
  )

# Define significance comparisons
comparisons <- list(
  c("hi ambi vege", "hi ambi bare"),
  c("hi warm vege", "hi warm bare"),
  c("lo ambi vege", "lo ambi bare"),
  c("hi ambi bare", "hi warm bare"),
  c("hi ambi vege", "hi warm vege"),
  c("hi ambi bare", "hi warm bare"),
  c("hi ambi vege", "lo ambi vege")
)

# Define y-positions for the significance bars
y_positions <- c(40, 40, 40, 44, 48, 51, 54)

# plot all species together
SLA_all <- ggplot(traits_23_NOR, aes(combined_treatment, SLA, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_data, aes(x = combined_treatment, y = mean_SLA), color = "red", size = 3) +
  labs(x = "Treatment", y = expression(SLA (mm^2/mg)))+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  scale_fill_manual(values = define_colors)
SLA_all

ggsave(filename = "RangeX_SLA_per_treat_NOR.png", 
       plot = SLA_all, 
       path = "Graphs", 
       width = 10, height = 6)

# plot per species
SLA_species <- ggplot(traits_23_NOR, aes(combined_treatment, SLA, fill = combined_treatment)) +
  geom_boxplot() +
  labs(x = "Treatment", y = "SLA (mm^2/mg)")+
  theme(legend.position = "bottom")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  scale_fill_manual(values = define_colors)+
  facet_wrap(vars(species), ncol = 5)
SLA_species

ggsave(filename = "RangeX_SLA_separate_species.png", 
       plot = SLA_species, 
       path = "Graphs", 
       width = 18, height = 10)

# LDMC ---------------------------------------------------------------------
ggplot(traits_23_NOR, aes(combined_treatment, LDMC, fill = combined_treatment))+
  geom_boxplot()+
  facet_wrap(vars(species), ncol = 5)+
  labs(x = "Treatment", y = "LDMC (mg g-1)")+
  theme(legend.position = "bottom")+
  stat_summary(fun = mean, geom = "point", shape = 20, 
               size = 2, color = "red")

# significance all species together ---------------------------------------
# all species together
# Fit the mixed-effects model
model_LDMC <- lmerTest::lmer(LDMC ~ combined_treatment + (1 | species) + 
                              (1 | block_ID_original),
                            data = traits_23_NOR)

# View the summary of the model
summary(model_LDMC)
# species explains some variance: species = 2781.77, block = 30.71
# 

# Calculate means and confidence intervals for each treatment
summary_LDMC <- traits_23_NOR |> 
  group_by(combined_treatment) |> 
  summarise(
    mean_LDMC = mean(LDMC, na.rm = TRUE),
    ci_lower = mean_LDMC - qt(0.975, df = n() - 1) * sd(LDMC, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_LDMC + qt(0.975, df = n() - 1) * sd(LDMC, na.rm = TRUE) / sqrt(n())
  )

# Define y-positions for the significance bars
y_positions <- c(400, 420, 440, 460, 480, 520)

# plot all species together
LDMC_all <- ggplot(traits_23_NOR, aes(combined_treatment, LDMC, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_LDMC, aes(x = combined_treatment, y = mean_LDMC), color = "red", size = 3) +
  labs(x = "Treatment", y = "LDMC (mg g-1)")+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  scale_fill_manual(values = define_colors)
LDMC_all

ggsave(filename = "RangeX_LDMC_all_species.png", 
       plot = LDMC_all, 
       path = "Graphs", 
       width = 10, height = 6)

LDMC_species <- ggplot(traits_23_NOR, aes(combined_treatment, LDMC, fill = combined_treatment)) +
  geom_boxplot() +
  labs(x = "Treatment", y = "LDMC (mg g-1)")+
  theme(legend.position = "bottom")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  scale_fill_manual(values = define_colors)+
  facet_wrap(vars(species), ncol = 5)
LDMC_species

ggsave(filename = "RangeX_LDMC_separate_species.png", 
       plot = LDMC_species, 
       path = "Graphs", 
       width = 20, height = 10)


# leaf thickness -------------------------------------------------------------
ggplot(traits_23_NOR, aes(combined_treatment, leaf_thickness, fill = combined_treatment))+
  geom_boxplot()+
  facet_wrap(vars(species), ncol = 5)+
  labs(x = "Treatment", y = "Leaf thickness (mm)")+
  theme(legend.position = "bottom")+
  stat_summary(fun = mean, geom = "point", shape = 20, 
               size = 2, color = "red")


# significance all species together ---------------------------------------
# all species together
# Fit the mixed-effects model
model_leaf_thickness <- lmerTest::lmer(leaf_thickness ~ combined_treatment +
                                         (1 | species) + 
                               (1 | block_ID_original),
                             data = traits_23_NOR)

# View the summary of the model
summary(model_leaf_thickness)
# species explains very little variance
# 

# Calculate means and confidence intervals for each treatment
summary_leaf_thickness <- traits_23_NOR |> 
  group_by(combined_treatment) |> 
  summarise(
    mean_leaf_thickness = mean(leaf_thickness, na.rm = TRUE),
    ci_lower = mean_leaf_thickness- qt(0.975, df = n() - 1) * sd(leaf_thickness, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_leaf_thickness + qt(0.975, df = n() - 1) * sd(leaf_thickness, na.rm = TRUE) / sqrt(n())
  )

# Define y-positions for the significance bars
y_positions <- c(0.59, 0.59, 0.6, 0.66, 0.7, 0.77)

# plot all species together
leaf_thickness_all <- ggplot(traits_23_NOR, aes(combined_treatment, leaf_thickness, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_leaf_thickness, aes(x = combined_treatment, y = mean_leaf_thickness), color = "red", size = 3) +
  labs(x = "Treatment", y = "leaf_thickness (mm)")+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  scale_fill_manual(values = define_colors)
leaf_thickness_all

ggsave(filename = "RangeX_leaf_thickness_all_species.png", 
       plot = leaf_thickness_all, 
       path = "Graphs", 
       width = 10, height = 6)


leaf_thickness_species <- ggplot(traits_23_NOR, aes(combined_treatment, leaf_thickness, fill = combined_treatment)) +
  geom_boxplot() +
  labs(x = "Treatment", y = "Leaf thickness (mm)")+
  theme(legend.position = "bottom")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  scale_fill_manual(values = define_colors)+
  facet_wrap(vars(species), ncol = 5)
leaf_thickness_species

ggsave(filename = "RangeX_leaf_thickness_separate_species.png", 
       plot = leaf_thickness_species, 
       path = "Graphs", 
       width = 18, height = 10)


# SLA CHE -----------------------------------------------------------------

ggplot(traits_23_CHE, aes(combined_treatment, SLA, fill = combined_treatment))+
  geom_boxplot()+
  labs(x = "Treatment", y = "SLA (mm^2/mg)")+
  theme(legend.position = "bottom")+
  geom_jitter(alpha = 0.5)+
  stat_summary(fun = mean, geom = "point", shape = 20, 
               size = 2, color = "red")

# SLA NOR and CHE species ----------------------------------------------------
SLA_NOR_CHE_species <- ggplot(traits_NOR_CHE, aes(combined_treatment, SLA, fill = combined_treatment))+
  geom_boxplot()+
  facet_wrap(vars(species), ncol = 5)+
  labs(x = "Treatment", y = expression(SLA (mm^2/mg)))+
  theme(legend.position = "bottom")+
  stat_summary(fun = mean, geom = "point", shape = 20, 
               size = 2, color = "red")+
  scale_fill_manual(values = define_colors)+
  theme(legend.position = "none")+
  facet_wrap(vars(species), ncol = 5, scales = "free")
SLA_NOR_CHE_species

ggsave(filename = "RangeX_SLA_per_species_NOR_CHE.png", 
       plot = SLA_NOR_CHE_species, 
       path = "Graphs", 
       width = 20, height = 18)

# SLA NOR and CHE per treatment ---------------------------------------------
# Calculate means and confidence intervals for each treatment per country
summary_data_traits_NOR_CHE <- traits_NOR_CHE |> 
  group_by(region, combined_treatment) |> 
  summarise(
    mean_SLA = mean(SLA, na.rm = TRUE),
    ci_lower = mean_SLA - qt(0.975, df = n() - 1) * sd(SLA, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_SLA + qt(0.975, df = n() - 1) * sd(SLA, na.rm = TRUE) / sqrt(n())
  )


# Define significance comparisons
comparisons <- list(
  c("hi ambi vege", "hi ambi bare"),
  c("hi warm vege", "hi warm bare"),
  c("lo ambi vege", "lo ambi bare"),
  c("hi ambi bare", "hi warm bare"),
  c("hi ambi vege", "hi warm vege"),
  c("hi ambi bare", "hi warm bare"),
  c("hi ambi vege", "lo ambi vege")
)

# Define y-positions for the significance bars
y_positions <- c(40, 40, 40, 44, 48, 51, 54)

# plot all species together
SLA_NOR_CHE <- ggplot(traits_NOR_CHE, aes(combined_treatment, SLA, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_data_traits_NOR_CHE, aes(x = combined_treatment, y = mean_SLA), color = "red", size = 3) +
  labs(x = "Treatment", y = expression(SLA (mm^2/mg)))+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  facet_wrap(vars(region))+
  scale_fill_manual(values = define_colors)
SLA_NOR_CHE

ggsave(filename = "RangeX_SLA_per_treat_NOR_CHE.png", 
       plot = SLA_NOR_CHE, 
       path = "Graphs", 
       width = 19, height = 8)


# LDMC NOR and CHE per treatment ---------------------------------------------
# Calculate means and confidence intervals for each treatment per country
summary_data_traits_NOR_CHE_LDMC <- traits_NOR_CHE |> 
  group_by(region, combined_treatment) |> 
  summarise(
    mean_LDMC = mean(LDMC, na.rm = TRUE),
    ci_lower = mean_LDMC - qt(0.975, df = n() - 1) * sd(LDMC, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_LDMC + qt(0.975, df = n() - 1) * sd(LDMC, na.rm = TRUE) / sqrt(n())
  )

y_positions <- c(550, 550, 550, 570, 600, 630, 670)

# plot all species together
LDMC_NOR_CHE <- ggplot(traits_NOR_CHE, aes(combined_treatment, LDMC, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_data_traits_NOR_CHE_LDMC, aes(x = combined_treatment, y = mean_LDMC), color = "red", size = 3) +
  labs(x = "Treatment", y = expression(LDMC (mg~g^{-1})))+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  facet_wrap(vars(region))+
  scale_fill_manual(values = define_colors)
LDMC_NOR_CHE

ggsave(filename = "RangeX_LDMC_per_treat_NOR_CHE.png", 
       plot = LDMC_NOR_CHE, 
       path = "Graphs", 
       width = 19, height = 8)

# LDMC NOR and CHE species ----------------------------------------------
LDMC_NOR_CHE_species <- ggplot(traits_NOR_CHE, aes(combined_treatment, LDMC, 
                           fill = combined_treatment))+
  geom_boxplot()+
  facet_wrap(vars(species), ncol = 5, scales = "free")+
  labs(x = "Treatment", y = expression(LDMC (mg~g^{-1})))+
  theme(legend.position = "none")+
  stat_summary(fun = mean, geom = "point", shape = 20, 
               size = 2, color = "red")
LDMC_NOR_CHE_species

ggsave(filename = "RangeX_LDMC_per_species_NOR_CHE.png", 
       plot = LDMC_NOR_CHE_species, 
       path = "Graphs", 
       width = 19, height = 18)





# spiderweb NOR--------------------------------------------
# prepare data to have one value per leaf
# has to be numeric
# no NAs
traits_NOR_radar <- traits_23_NOR |> 
  mutate(group = combined_treatment) |> 
  mutate(across(c(LDMC, SLA, leaf_thickness, wet_mass, dry_mass,
                  leaf_area), as.numeric)) |> 
  mutate(across(c(LDMC, SLA, leaf_thickness, wet_mass, dry_mass,
                  leaf_area), rescale)) |> 
  select(group, LDMC, SLA, leaf_thickness, wet_mass, dry_mass,
         leaf_area) |> 
  na.omit() |> # delete rows with NAs
  distinct()

# calculate mean per treatment
radar_data_NOR <- traits_NOR_radar |> 
  group_by(group) |> 
  summarise(across(c(LDMC, SLA, leaf_thickness, wet_mass, dry_mass,
                     leaf_area), mean)) |> 
  ungroup()

max_min <- data.frame(
  group = c("Max", "Min"),
  LDMC = c(max(radar_data_NOR$LDMC), min(radar_data_NOR$LDMC)),
  SLA = c(max(radar_data_NOR$SLA), min(radar_data_NOR$SLA)),
  leaf_thickness = c(max(radar_data_NOR$leaf_thickness), min(radar_data_NOR$leaf_thickness)),
  wet_mass = c(max(radar_data_NOR$wet_mass), min(radar_data_NOR$wet_mass)),
  dry_mass = c(max(radar_data_NOR$dry_mass), min(radar_data_NOR$dry_mass)),
  leaf_area = c(max(radar_data_NOR$leaf_area), min(radar_data_NOR$leaf_area))
)

radar_data_NOR <- rbind(max_min, radar_data_NOR)

ggradar(radar_data_NOR)



# spiderweb functional traits NOR--------------------------------------------
rescale_custom <- function(x) {
  scales::rescale(x, to = c(0.2, 1))
}

radar_data_NOR <- traits_23_NOR |> 
  mutate(group = combined_treatment) |> 
  mutate(across(c(LDMC, SLA, leaf_thickness, wet_mass, dry_mass, leaf_area), as.numeric)) |> 
  mutate(across(c(LDMC, SLA, leaf_thickness, wet_mass, dry_mass, leaf_area), rescale_custom)) |> 
  select(group, LDMC, SLA, leaf_thickness, wet_mass, dry_mass, leaf_area) |> 
  na.omit() |> 
  distinct()

# Calculate mean per treatment
radar_data_NOR <- radar_data_NOR |> 
  group_by(group) |> 
  summarise(across(everything(), mean)) |> 
  ungroup()

# Create radar plot without min-max rows
ggradar(radar_data_NOR,
        grid.min = 0.2,            # Start from 20% to avoid clustering
        grid.mid = 0.4,            # Midpoint for better spread
        grid.max = 0.6,              # Max remains 1
        plot.extent.x.sf = 1.5,    # Increase plot area
        plot.extent.y.sf = 1.2,    # Adjust aspect ratio
        group.line.width = 1.2,    # Thicker lines for visibility
        group.point.size = 4,      # Larger points
        background.circle.colour = "white", # Clean background
        legend.position = "bottom") # Adjust legend placement



# spiderweb functional and demographic traits NOR ----------------------------
rescale_custom <- function(x) {
  scales::rescale(x, to = c(0.2, 1))
}

traits_fun_demo_NOR |> 
  summarise(across(everything(), ~sum(is.na(.))))

# select traits where as many plants as possible have values
radar_fun_demo_NOR <- traits_fun_demo_NOR |> 
  mutate(group = combined_treatment) |> 
  mutate(across(c(LDMC, SLA, leaf_thickness, wet_mass, dry_mass, leaf_area,
                  height_vegetative, height_vegetative_str, 
                  number_leaves, number_flowers), as.numeric)) |> 
  mutate(across(c(LDMC, SLA, leaf_thickness, wet_mass, dry_mass, leaf_area,
                  height_vegetative, height_vegetative_str, 
                  number_leaves, number_flowers), rescale_custom)) |> 
  select(group, LDMC, SLA, leaf_thickness, wet_mass, dry_mass, leaf_area,
         height_vegetative, height_vegetative_str, 
         number_leaves, number_flowers) |> 
  #na.omit() |> 
  #distinct() |> 
  select(group, everything())

# delete the rows where one trait has an NA
radar_fun_demo_NOR <- radar_fun_demo_NOR |> na.omit()


# Calculate mean per treatment
radar_fun_demo_NOR <- radar_fun_demo_NOR |> 
  group_by(group) |> 
  summarise(across(everything(), mean)) |> 
  ungroup()

# Create radar plot without min-max rows
ggradar(radar_fun_demo_NOR,
        grid.min = 0.2,            # Start from 20% to avoid clustering
        grid.mid = 0.4,            # Midpoint for better spread
        grid.max = 0.6,              # Max remains 1
        plot.extent.x.sf = 1.5,    # Increase plot area
        plot.extent.y.sf = 1.2,    # Adjust aspect ratio
        group.line.width = 1.2,    # Thicker lines for visibility
        group.point.size = 4,      # Larger points
        background.circle.colour = "white", # Clean background
        legend.position = "bottom") # Adjust legend placement

#













# 3D plot NOR ----------------------------------------------------------------
traits_23_NOR_3d <- traits_23_NOR |> 
  mutate(treatment_label = as.factor(combined_treatment))

# Create the 3D scatter plot
plot_3d <- plot_ly(traits_23_NOR_3d, x = ~SLA, y = ~LDMC, z = ~leaf_thickness, color = ~treatment_label, colors = "Set1") |> 
  add_markers() |> 
  plotly::layout(scene = list(
    xaxis = list(title = "SLA (mm^2/mg)"),
    yaxis = list(title = "LDMC"),
    zaxis = list(title = "Leaf Thickness")
  ))

plot_3d
#

# 3d mean per species per treatment ---------------------------------------
# Calculate the mean SLA, LDMC, and leaf thickness for each species and treatment combination
mean_traits <- traits_23_NOR_3d |> 
  group_by(species, combined_treatment) |> 
  summarise(
    mean_SLA = mean(SLA, na.rm = TRUE),
    mean_LDMC = mean(LDMC, na.rm = TRUE),
    mean_leaf_thickness = mean(leaf_thickness, na.rm = TRUE)
  ) |> 
  ungroup()

plot_3d_mean <- plot_ly(mean_traits, x = ~mean_SLA, y = ~mean_LDMC, z = ~mean_leaf_thickness, color = ~combined_treatment, colors = "Set1",
                        text = ~species) |> 
  add_markers() |> 
  layout(scene = list(
    xaxis = list(title = "Mean SLA (mm^2/mg)"),
    yaxis = list(title = "Mean LDMC"),
    zaxis = list(title = "Mean Leaf Thickness"),
    legend = list(title = list(text = "Treatment"))
  ))

plot_3d_mean
#

# add lines between species in same treat
plot_ly(mean_traits, x = ~mean_SLA, y = ~mean_LDMC, z = ~mean_leaf_thickness, color = ~combined_treatment, colors = "Set1") |> 
  add_markers(size = 1) |> 
  layout(scene = list(
    xaxis = list(title = "Mean SLA (mm^2/mg)"),
    yaxis = list(title = "Mean LDMC"),
    zaxis = list(title = "Mean Leaf Thickness"),
    legend = list(size = 1, x = 0.1, y = 0.9, title = list(text = "Treatment"))  # Adjust legend position
  )) |> 
  add_trace(type = "scatter3d", mode = "markers+lines", x = ~mean_SLA, y = ~mean_LDMC, z = ~mean_leaf_thickness, color = ~combined_treatment, colors = "Set1", marker = list(size = 1), line = list(width = 2), showlegend = FALSE)


# 3d mean per treatment ---------------------------------------
# Calculate the mean SLA, LDMC, and leaf thickness for each  treatment combination
mean_traits_treat <- traits_23_NOR_3d |> 
  group_by(combined_treatment) |> 
  summarise(
    mean_SLA = mean(SLA, na.rm = TRUE),
    mean_LDMC = mean(LDMC, na.rm = TRUE),
    mean_leaf_thickness = mean(leaf_thickness, na.rm = TRUE)
  ) |> 
  ungroup()

plot_3d_mean_treat <- plot_ly(mean_traits_treat, x = ~mean_SLA, y = ~mean_LDMC, z = ~mean_leaf_thickness, color = ~combined_treatment, 
                              colors = define_colors) |> 
  add_markers(marker = list(size = 18)) |> 
  layout(scene = list(
    xaxis = list(title = "Mean SLA (mm^2/mg)"),
    yaxis = list(title = "Mean LDMC (mg g-1)"),
    zaxis = list(title = "Mean leaf thickness (mm)"),
    legend = list(title = list(text = "Treatment")) 
  ))
plot_3d_mean_treat
#
# kind of nice to see that the points for bare vs vege are far apart
# while warm vs ambi are close to each other

# move legend closer
plot_3d_mean_treat <- plot_3d_mean_treat|> 
  layout(legend = list(x = 0.1, y = 0.9))
plot_3d_mean_treat


# 3d mean per species ---------------------------------------
# Calculate the mean SLA, LDMC, and leaf thickness for each species just to check per treatment
mean_traits_species <- traits_23_NOR_3d |> 
  group_by(species, combined_treatment) |> 
  summarise(
    mean_SLA = mean(SLA, na.rm = TRUE),
    mean_LDMC = mean(LDMC, na.rm = TRUE),
    mean_leaf_thickness = mean(leaf_thickness, na.rm = TRUE)
  ) |> 
  ungroup()

plot_3d_mean_species <- plot_ly(mean_traits_species, x = ~mean_SLA, y = ~mean_LDMC, z = ~mean_leaf_thickness, color = ~species, colors = "Set1",
                        text = ~species) |> 
  add_markers() |> 
  layout(scene = list(
    xaxis = list(title = "Mean SLA (mm^2/mg)"),
    yaxis = list(title = "Mean LDMC (mg g-1)"),
    zaxis = list(title = "Mean leaf thickness (mm)"),
    legend = list(title = list(text = "Treatment"))
  ))
plot_3d_mean_species
#
# ok species seem to be clustered a bit




# 3d NOR and CHE ----------------------------------------------------------
traits_NOR_CHE <- traits_NOR_CHE |> 
  mutate(treatment_label = as.factor(combined_treatment))

# 3d mean per treatment ---------------------------------------
# Calculate the mean SLA, LDMC, and leaf thickness for each  treatment combination
mean_traits_treat_NOR_CHE <- traits_NOR_CHE |> 
  group_by(region, combined_treatment) |> 
  summarise(
    mean_SLA = mean(SLA, na.rm = TRUE),
    mean_LDMC = mean(LDMC, na.rm = TRUE),
    mean_leaf_thickness = mean(leaf_thickness, na.rm = TRUE)
  ) |> 
  ungroup()

# define the shapes for each region
shape_mapping <- c("NOR" = "circle", "CHE" = "diamond")

# plot
NOR_CHE_plot_3d_mean_treat <- plot_ly(mean_traits_treat_NOR_CHE, x = ~mean_SLA, y = ~mean_LDMC, z = ~mean_leaf_thickness, color = ~combined_treatment, 
                                      colors = define_colors, symbol = ~region, symbols = shape_mapping) |> 
  add_markers(marker = list(size = 18)) |> 
  layout(scene = list(
    xaxis = list(title = "Mean SLA (mm^2/mg)"),
    yaxis = list(title = "Mean LDMC (mg g-1)"),
    zaxis = list(title = "Mean leaf thickness (mm)"),
    legend = list(title = list(text = "Treatment"))
  ))
NOR_CHE_plot_3d_mean_treat
#
# move legend closer
NOR_CHE_plot_3d_mean_treat <- NOR_CHE_plot_3d_mean_treat|> 
  layout(legend = list(x = 0.9, y = 0.8))
NOR_CHE_plot_3d_mean_treat
#


# 3d functional traits CHE and NOR ----------------------------------------
# make plot with better legend
NOR_CHE_plot_3d_mean_treat <- plot_ly() |> 
  # Add CHE points with legend visible
  add_markers(data = mean_traits_treat_NOR_CHE |> filter(region == "CHE"),
              x = ~mean_SLA, y = ~mean_LDMC, z = ~mean_leaf_thickness,
              color = ~combined_treatment, colors = define_colors,
              symbol = ~region, symbols = shape_mapping,
              marker = list(size = 18),
              showlegend = FALSE) |> 
  
  # Add NOR points with legend hidden
  add_markers(data = mean_traits_treat_NOR_CHE |> filter(region == "NOR"),
              x = ~mean_SLA, y = ~mean_LDMC, z = ~mean_leaf_thickness,
              color = ~combined_treatment, colors = define_colors,
              symbol = ~region, symbols = shape_mapping,
              marker = list(size = 18),
              showlegend = TRUE) |> 
  
  # Layout adjustments for axis labels
  layout(scene = list(
    xaxis = list(title = "Mean SLA (mm^2/mg)"),
    yaxis = list(title = "Mean LDMC (mg g-1)"),
    zaxis = list(title = "Mean leaf thickness (mm)")
  ))

NOR_CHE_plot_3d_mean_treat <- NOR_CHE_plot_3d_mean_treat|> 
  layout(legend = list(x = 0.8, y = 0.8))


# Add a separate annotation for shape legend without title
NOR_CHE_plot_3d_mean_treat <- NOR_CHE_plot_3d_mean_treat |> 
  layout(annotations = list(
    list(
      x = 0.9, y = 0.4,  # Position outside the plot
      xref = "paper", yref = "paper",
      text = "◊ = CHE<br>● = NOR",
      showarrow = FALSE,
      font = list(size = 18)
    )
  ))

NOR_CHE_plot_3d_mean_treat








# delete samples without functional trait measurements NOR -------------------------------------
traits_NOR_23 <- traits_23_NOR |> 
  filter(!is.na(wet_mass) | !is.na(dry_mass) | !is.na(leaf_area) 
         | !is.na(leaf_thickness) | !is.na(SLA) | !is.na(LDMC))

length(traits_NOR_23$region) # 596 now




# PCA NOR -------------------------------------------------------------------
# Convert integer columns to numeric
traits_NOR_23 <- traits_NOR_23 |> 
  mutate(across(where(is.integer), as.numeric))

# Fill missing values with the mean of each column
traits_NOR_23_PCA <- traits_NOR_23 |> 
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE))))

# Perform PCA


# Select numerical columns for PCA
numerical_columns <- traits_NOR_23_PCA |> 
  select(leaf_thickness, leaf_area, wet_mass, dry_mass, SLA, LDMC)

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



# prepare PCA - delete samples without functional trait measurements NOR -------------------------------------
traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR |> 
  filter(!is.na(wet_mass) | !is.na(dry_mass) | !is.na(leaf_area) 
         | !is.na(leaf_thickness) | !is.na(SLA) | !is.na(LDMC))

length(traits_fun_demo_NOR_PCA$region) # 575 now


# PCA NOR -------------------------------------------------------------------
# Convert integer columns to numeric
traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR_PCA |> 
  mutate(across(where(is.integer), as.numeric))

# # Fill missing values with the mean of each column
# traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR_PCA |> 
#   mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE))))

# Select numerical columns for PCA
numerical_columns_NOR <- traits_fun_demo_NOR_PCA |> 
  select(leaf_thickness, leaf_area, wet_mass, dry_mass, SLA, LDMC,
          height_vegetative_str, height_vegetative, leaf_length1,
          number_leaves, number_flowers)


# Perform PCA
pca_result_NOR <- PCA(numerical_columns_NOR, scale.unit = TRUE, ncp = 2, graph = FALSE)

# Visualize PCA results - species
fviz_pca_ind(pca_result_NOR, geom.ind = "point", pointshape = 21, 
             pointsize = 2, fill.ind = traits_fun_demo_NOR_PCA$species, 
             palette = "jco", addEllipses = TRUE, 
             label = "var", col.var = "black", repel = TRUE)

# PCA treatment
fviz_pca_ind(pca_result_NOR, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = traits_fun_demo_NOR_PCA$combined_treatment, 
             palette = "jco", addEllipses = TRUE, 
             label = "var", col.var = "black", repel = TRUE) 




