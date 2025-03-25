
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
library(broom)
library(lmerTest)
library(ggsignif)
library(plotly)
conflicts_prefer(plotly::layout)

library(FactoMineR)
library(factoextra)

theme_set(theme_bw())

# define colors for each treatment
define_colors <- c(
  "hi_ambi_vege" = "turquoise",
  "hi_ambi_bare" = "lightblue",
  "hi_warm_vege" = "red",
  "hi_warm_bare" = "orange",
  "lo_ambi_vege" = "grey34",
  "lo_ambi_bare" = "grey"
)

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
  theme(legend.position = "bottom")+
  geom_jitter(alpha = 0.5)+
  stat_summary(fun = mean, geom = "point", shape = 20, 
               size = 2, color = "red")


# Perform ANOVA for each species
results_anova <- traits_23 |> 
  group_by(species) |> 
  do(tidy(aov(SLA ~ combined_treatment, data = .)))
print(results_anova)


# significance all species together ---------------------------------------
# all species together
# Fit the mixed-effects model
model_SLA <- lmerTest::lmer(SLA ~ combined_treatment + (1 | species) + 
                (1 | block_ID_original),
              data = traits_23)

# View the summary of the model
summary(model_SLA)
# species explains some variance: species = 24.937, block = 0.213
# hi_ambi_vege, hi_warm_vege, and lo_ambi_vege have significant effects on SLA, as indicated by their low p-values 
# SLA significantly higher in vegetated plots compared to bare


# Calculate means and confidence intervals for each treatment
summary_data <- traits_23 |> 
  group_by(combined_treatment) |> 
  summarise(
    mean_SLA = mean(SLA, na.rm = TRUE),
    ci_lower = mean_SLA - qt(0.975, df = n() - 1) * sd(SLA, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_SLA + qt(0.975, df = n() - 1) * sd(SLA, na.rm = TRUE) / sqrt(n())
  )

# Define significance comparisons
comparisons <- list(
  c("hi_ambi_vege", "hi_ambi_bare"),
  c("hi_warm_vege", "hi_warm_bare"),
  c("lo_ambi_vege", "lo_ambi_bare"),
  c("hi_ambi_vege", "hi_warm_vege"),
  c("hi_ambi_bare", "hi_warm_bare"),
  c("hi_ambi_vege", "lo_ambi_vege")
)

# Define y-positions for the significance bars
y_positions <- c(40, 42, 44, 46, 48, 52)

# plot all species together
SLA_all <- ggplot(traits_23, aes(combined_treatment, SLA, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_data, aes(x = combined_treatment, y = mean_SLA), color = "red", size = 3) +
  labs(x = "Treatment", y = "SLA (mm^2/mg)")+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  scale_fill_manual(values = define_colors)
SLA_all

ggsave(filename = "RangeX_SLA_all_species.png", 
       plot = SLA_all, 
       path = "Graphs", 
       width = 10, height = 6)

# plot per species
SLA_species <- ggplot(traits_23, aes(combined_treatment, SLA, fill = combined_treatment)) +
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
ggplot(traits_23, aes(combined_treatment, LDMC, fill = combined_treatment))+
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
                            data = traits_23)

# View the summary of the model
summary(model_LDMC)
# species explains some variance: species = 2781.77, block = 30.71
# 

# Calculate means and confidence intervals for each treatment
summary_LDMC <- traits_23 |> 
  group_by(combined_treatment) |> 
  summarise(
    mean_LDMC = mean(LDMC, na.rm = TRUE),
    ci_lower = mean_LDMC - qt(0.975, df = n() - 1) * sd(LDMC, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_LDMC + qt(0.975, df = n() - 1) * sd(LDMC, na.rm = TRUE) / sqrt(n())
  )

# Define y-positions for the significance bars
y_positions <- c(400, 420, 440, 460, 480, 520)

# plot all species together
LDMC_all <- ggplot(traits_23, aes(combined_treatment, LDMC, fill = combined_treatment)) +
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

LDMC_species <- ggplot(traits_23, aes(combined_treatment, LDMC, fill = combined_treatment)) +
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
ggplot(traits_23, aes(combined_treatment, leaf_thickness, fill = combined_treatment))+
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
                             data = traits_23)

# View the summary of the model
summary(model_leaf_thickness)
# species explains very little variance
# 

# Calculate means and confidence intervals for each treatment
summary_leaf_thickness <- traits_23 |> 
  group_by(combined_treatment) |> 
  summarise(
    mean_leaf_thickness = mean(leaf_thickness, na.rm = TRUE),
    ci_lower = mean_leaf_thickness- qt(0.975, df = n() - 1) * sd(leaf_thickness, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_leaf_thickness + qt(0.975, df = n() - 1) * sd(leaf_thickness, na.rm = TRUE) / sqrt(n())
  )

# Define y-positions for the significance bars
y_positions <- c(0.59, 0.59, 0.6, 0.66, 0.7, 0.77)

# plot all species together
leaf_thickness_all <- ggplot(traits_23, aes(combined_treatment, leaf_thickness, fill = combined_treatment)) +
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


leaf_thickness_species <- ggplot(traits_23, aes(combined_treatment, leaf_thickness, fill = combined_treatment)) +
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



# 3D plot -----------------------------------------------------------------
traits_23_3d <- traits_23 |> 
  mutate(treatment_label = as.factor(combined_treatment))

# Create the 3D scatter plot
plot_3d <- plot_ly(traits_23_3d, x = ~SLA, y = ~LDMC, z = ~leaf_thickness, color = ~treatment_label, colors = "Set1") |> 
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
mean_traits <- traits_23_3d |> 
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
mean_traits_treat <- traits_23_3d |> 
  group_by(combined_treatment) |> 
  summarise(
    mean_SLA = mean(SLA, na.rm = TRUE),
    mean_LDMC = mean(LDMC, na.rm = TRUE),
    mean_leaf_thickness = mean(leaf_thickness, na.rm = TRUE)
  ) |> 
  ungroup()

plot_3d_mean_treat <- plot_ly(mean_traits_treat, x = ~mean_SLA, y = ~mean_LDMC, z = ~mean_leaf_thickness, color = ~combined_treatment, 
                              colors = define_colors) |> 
  add_markers() |> 
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



# 3d mean per specie ---------------------------------------
# Calculate the mean SLA, LDMC, and leaf thickness for each species just to check
mean_traits_species <- traits_23_3d |> 
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
    yaxis = list(title = "Mean LDMC"),
    zaxis = list(title = "Mean Leaf Thickness"),
    legend = list(title = list(text = "Treatment"))
  ))
plot_3d_mean_species
#
# ok species seem to be clustered a bit





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

