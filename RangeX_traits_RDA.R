
# RangeX demographic and functional traits - RDA plots ------------

## Data used: RangeX_clean_functional_traits_NOR_2023.csv
##            RangeX_clean_yearly_size_2021_2022_2023_NOR.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      30.03.25
## Author:    Nadine Arzt
## Purpose:   Create RDA plots


# load library ------------------------------------------------------------
library(ggvegan)
library(ggrepel)
library(stringr)

source("RangeX_demographic_functional_traits_data_preparation_CHE_NOR.R")


# NOR ---------------------------------------------------------------------
# prepare PCA - delete samples without functional trait measurements NOR -------------------------------------
traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR |> 
  filter(!is.na(wet_mass) | !is.na(dry_mass) | !is.na(leaf_area) 
         | !is.na(leaf_thickness) | !is.na(SLA) | !is.na(LDMC))

length(traits_fun_demo_NOR_PCA$region) # 574 now

traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR_PCA |> 
  select(leaf_thickness, leaf_area, wet_mass, dry_mass, SLA, LDMC,
         height_vegetative_str, height_vegetative, leaf_length1,
         number_leaves, number_flowers, species, combined_treatment)

traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR_PCA |>
  na.omit()

length(traits_fun_demo_NOR_PCA$leaf_thickness) # 451 leaves


# PCA NOR -------------------------------------------------------------------
# Convert integer columns to numeric
traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR_PCA |> 
  mutate(across(where(is.integer), as.numeric))

# this one! perform rda and plot it -----------------------------------------
# prepare PCA - delete samples without functional trait measurements NOR 
traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR |> 
  filter(!is.na(wet_mass) | !is.na(dry_mass) | !is.na(leaf_area) 
         | !is.na(leaf_thickness) | !is.na(SLA) | !is.na(LDMC))

length(traits_fun_demo_NOR_PCA$region) # 558 now

traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR_PCA |> 
  select(leaf_thickness, leaf_area, wet_mass, dry_mass, SLA, LDMC,
         height_vegetative_str, height_vegetative, leaf_length1,
         number_leaves, number_flowers, species, combined_treatment)

traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR_PCA |>
  na.omit()
#451

# if you import data with read_cvs you would get a tibble and 
# would not need drop = FALSE
species_data <- traits_fun_demo_NOR_PCA[, "species", drop = FALSE]
trait_data <- traits_fun_demo_NOR_PCA |> select(leaf_thickness: number_flowers)
meta_data <- traits_fun_demo_NOR_PCA |> 
  select(species, combined_treatment)

rda_NOR <- rda(trait_data ~ combined_treatment + Condition(species), data = meta_data, scale = TRUE)

rda_NOR |> anova()

summary(rda_NOR)

fortify(rda_NOR, display = "cn")

rda_NOR_cn <- fortify(rda_NOR, display = "cn") |> 
  mutate(label = str_remove(label, "combined_treatment"))

# plot
autoplot(rda_NOR) 

# zoom in 
autoplot(rda_NOR) + coord_equal(ylim = c(-2,2))


# find outlier
rda_scores <- fortify(rda_NOR, display = "sites")

# Convert to data frame for easier handling
rda_scores_df <- as.data.frame(rda_scores)

# Identify the outlier (e.g., the point with the minimum value on RDA1 axis)
outlier_index <- which.min(rda_scores_df$RDA1)

# Print the outlier index and corresponding scores
print(outlier_index)
print(rda_scores_df[outlier_index, ])

# Match the outlier index to the original data
outlier_data <- traits_fun_demo_NOR_PCA[outlier_index, ]
print(outlier_data)

# NOR.lo.ambi.bare.wf.07.30 cenig deleted above
# not sure what is wrong but should check



# make nicer plot ---------------------------------------------------------
# Extract site scores
site_scores <- as.data.frame(fortify(rda_NOR, display = "sites"))
site_scores$Site <- rownames(site_scores)

# Extract treat scores
site_scores$combined_treatment <- traits_fun_demo_NOR_PCA$combined_treatment 

# Extract species scores
# means traits
fortify(rda_NOR, display = "species")

species_scores <- as.data.frame(fortify(rda_NOR, display = "species"))
species_scores$Species <- rownames(species_scores)

# Extract trait loadings (constraints)
trait_loadings <- as.data.frame(fortify(rda_NOR, display = "bp"))
trait_loadings$Trait <- rownames(trait_loadings)

trait_loadings <- trait_loadings |> 
  mutate(label = str_remove(label, "combined_treatment"))

# plot
RDA_NOR <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = RDA1, y = RDA2, color = combined_treatment), 
             size = 4, alpha = 0.5) +
  
  # Species points in red
  geom_point(data = species_scores, 
             aes(x = RDA1, y = RDA2), 
             color = "red", size = 3, alpha = 0.5) +
  
  # Ellipses for treatment groups
  stat_ellipse(data = site_scores, 
               aes(x = RDA1, y = RDA2, color = combined_treatment), 
               size = 1) +
  
  # Species labels
  # geom_text(data = species_scores, 
  #           aes(x = RDA1, y = RDA2, label = label), 
  #           vjust = -1, hjust = 1) +  # color = "blue", size = 7
  geom_text_repel(data = species_scores, 
                  aes(x = RDA1, y = RDA2, label = label, 
                      size = 7, color = "blue")) + # 11 unlabeled data points
  
  # Arrows
  geom_segment(data = trait_loadings, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "blue", size = 1) +
  
  # Trait labels with ggrepel to avoid overlap
  geom_text_repel(data = trait_loadings, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  color = "blue", size = 6) +
  
  # Customize plot
  labs(x = "RDA1", 
       y = "RDA2", 
       color = "Treatment") +
  theme(legend.position = "right")+
  scale_color_manual(values = define_colors)
RDA_NOR

# ggsave(filename = "RangeX_RDA_NOR.png", 
#        plot = RDA_NOR, 
#        path = "Graphs", 
#        width = 15, height = 15)


# final plot RDA NOR ------------------------------------------------------
RDA_NOR <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = RDA1, y = RDA2, color = combined_treatment), 
             size = 4, alpha = 0.5) +
  
  # Species points in red
  geom_point(data = species_scores, 
             aes(x = RDA1, y = RDA2), 
             color = "red", size = 3, alpha = 0.5) +
  
  # Ellipses for treatment groups
  stat_ellipse(data = site_scores, 
               aes(x = RDA1, y = RDA2, color = combined_treatment), 
               size = 1) +
  
  # Species labels with no size legend
  geom_text_repel(data = species_scores, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  #color = "red", 
                  size = 7,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 23) +
  
  # Arrows for trait loadings
  geom_segment(data = trait_loadings, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "blue", size = 1) +
  
  # Trait labels with better spacing
  geom_text_repel(data = trait_loadings, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  color = "blue", 
                  size = 8,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 15) +
  
  # Customize plot labels and theme
  labs(x = "RDA1", y = "RDA2", color = "Treatment") +
  theme(legend.position = "right") +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  coord_cartesian(xlim = c(-1.3, 2), ylim = c(-4, 2))  # Adjust as needed

RDA_NOR

# ggsave(filename = "RangeX_RDA_NOR.png", 
#        plot = RDA_NOR, 
#        path = "Graphs", 
#        width = 15, height = 15)



# RDA NOR plot for poster -------------------------------------------------
RDA_NOR <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = RDA1, y = RDA2, color = combined_treatment), 
             size = 4, alpha = 0.5) +
  
  # centroids
  geom_point(data = species_scores, 
             aes(x = RDA1, y = RDA2), 
             color = "red", size = 6, alpha = 0.5) +
  
  # Ellipses for treatment groups
  stat_ellipse(data = site_scores, 
               aes(x = RDA1, y = RDA2, color = combined_treatment), 
               size = 1) +
  
  # Species labels with no size legend
  geom_text_repel(data = species_scores, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  #color = "red", 
                  size = 7,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 23)+
  
  # Customize plot labels and theme
  labs(x = "RDA1 (72.5%)", y = "RDA2 (22.0%)", color = "Treatment") +
  theme(legend.position = "right") +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  coord_cartesian(xlim = c(-1.3, 2), ylim = c(-4, 2))  # Adjust as needed

RDA_NOR

ggsave(filename = "RangeX_RDA_NOR_poster.png", 
       plot = RDA_NOR, 
       path = "Graphs", 
       width = 15, height = 15)


# CHE ---------------------------------------------------------------------
# prepare PCA - delete samples without functional trait measurements NOR 
traits_fun_demo_CHE_PCA <- traits_fun_demo_CHE |> 
  filter(!is.na(wet_mass) | !is.na(dry_mass) | !is.na(leaf_area) 
         | !is.na(leaf_thickness) | !is.na(SLA) | !is.na(LDMC))

length(traits_fun_demo_CHE_PCA$region) # 519 now

traits_fun_demo_CHE_PCA <- traits_fun_demo_CHE_PCA |> 
  select(leaf_thickness, leaf_area, wet_mass, dry_mass, SLA, LDMC,
         height_vegetative_str, height_vegetative, leaf_length1,
         number_leaves, number_flowers, species, combined_treatment)

traits_fun_demo_CHE_PCA <- traits_fun_demo_CHE_PCA |>
  na.omit()

length(traits_fun_demo_CHE_PCA$leaf_thickness) # 223


# if you import data with read_cvs you would get a tibble and 
# would not need drop = FALSE
species_data <- traits_fun_demo_CHE_PCA |> 
  select(species)
trait_data <- traits_fun_demo_CHE_PCA |> select(leaf_thickness: number_flowers)
meta_data <- traits_fun_demo_CHE_PCA |> 
  select(species, combined_treatment)

rda_CHE <- rda(trait_data ~ combined_treatment + Condition(species), data = meta_data, scale = TRUE)

rda_CHE |> anova()

summary(rda_CHE)
# 7.9% of variation is explained by treat -> small but significant
# 60.2% by species but we take that out with condition
# RDA1 (first axis) explains 58.6% of the constrained variance
# axis 2 explains 35.5%
# RDA1 + RDA2 together explain 94.1%,

autoplot(rda_CHE)

# make nicer plot CHE ------------------------------------------------------
# Extract site scores
site_scores <- as.data.frame(fortify(rda_CHE, display = "sites"))
site_scores$Site <- rownames(site_scores)

# Extract treat scores
site_scores$combined_treatment <- traits_fun_demo_CHE_PCA$combined_treatment 

# Extract species scores
# means traits
species_scores <- as.data.frame(fortify(rda_CHE, display = "species"))
species_scores$Species <- rownames(species_scores)

# Extract trait loadings (constraints)
trait_loadings <- as.data.frame(fortify(rda_CHE, display = "bp"))
trait_loadings$Trait <- rownames(trait_loadings)

trait_loadings <- trait_loadings |> 
  mutate(label = str_remove(label, "combined_treatment"))


# plot RDA CHE ------------------------------------------------------------
RDA_CHE <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = RDA1, y = RDA2, color = combined_treatment), 
             size = 4, alpha = 0.5) +
  
  # centroids
  geom_point(data = species_scores, 
             aes(x = RDA1, y = RDA2), 
             color = "red", size = 6, alpha = 0.5) +
  
  # Ellipses for treatment groups
  stat_ellipse(data = site_scores, 
               aes(x = RDA1, y = RDA2, color = combined_treatment), 
               size = 1) +
  
  # Species labels with no size legend
  geom_text_repel(data = species_scores, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  #color = "red", 
                  size = 7,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 23)+
  
  # Customize plot labels and theme
  labs(x = "RDA1 (58.6%)", y = "RDA2 (35.5%)", color = "Treatment") +
  theme(legend.position = "right") +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")
RDA_CHE

ggsave(filename = "RangeX_RDA_CHE_poster.png", 
       plot = RDA_CHE, 
       path = "Graphs", 
       width = 15, height = 15)





# NOR + CHE ------------------------------------------------------------------
# prepare PCA - delete samples without functional trait measurements NOR -------------------------------------
traits_fun_demo_NOR_CHE_PCA <- traits_fun_demo_NOR_CHE |> 
  filter(!is.na(wet_mass) | !is.na(dry_mass) | !is.na(leaf_area) 
         | !is.na(leaf_thickness) | !is.na(SLA) | !is.na(LDMC))

length(traits_fun_demo_NOR_CHE_PCA$region) # 1123 now

traits_fun_demo_NOR_CHE_PCA <- traits_fun_demo_NOR_CHE_PCA |> 
  select(leaf_thickness, leaf_area, wet_mass, dry_mass, SLA, LDMC,
         height_vegetative_str, height_vegetative, leaf_length1,
         number_leaves,  species, combined_treatment, region)

traits_fun_demo_NOR_CHE_PCA <- traits_fun_demo_NOR_CHE_PCA |> 
  mutate(combined_treatment_region = paste(region, combined_treatment, sep = " "))

na_counts <- colSums(is.na(traits_fun_demo_NOR_CHE_PCA))
na_counts

# number_flowers has 301 NAs --> maybe take it out?
# gives 960 leaves

traits_fun_demo_NOR_CHE_PCA <- traits_fun_demo_NOR_CHE_PCA |>
  na.omit()

length(traits_fun_demo_NOR_CHE_PCA$leaf_thickness) #960, but 674 leaves with #flowers



# species, trait, meta data -----------------------------------------------
species_data <- traits_fun_demo_NOR_CHE_PCA |> 
  select(species)
trait_data <- traits_fun_demo_NOR_CHE_PCA |> select(leaf_thickness: number_leaves)
meta_data <- traits_fun_demo_NOR_CHE_PCA |> 
  select(species, region, combined_treatment)

meta_data <- meta_data |> 
  mutate(region = factor(region), 
         combined_treatment = factor(combined_treatment))

meta_data <- meta_data |> 
  mutate(combined_treatment_region = paste(combined_treatment, region, sep = " "))


rda_NOR_CHE <- rda(trait_data ~ combined_treatment_region  + Condition(species), data = meta_data, scale = TRUE)

rda_NOR_CHE |> anova()


# Fortify -----------------------------------------------------------------
# Extract site scores
site_scores <- as.data.frame(fortify(rda_NOR_CHE, display = "sites"))
site_scores$Site <- rownames(site_scores)

# Extract treat scores
site_scores$combined_treatment_region <- traits_fun_demo_NOR_CHE_PCA$combined_treatment_region 

site_scores$region <- traits_fun_demo_NOR_CHE_PCA$region  


# Extract species scores
# means traits
fortify(rda_NOR_CHE, display = "species")

species_scores <- as.data.frame(fortify(rda_NOR_CHE, display = "species"))
species_scores$Species <- rownames(species_scores)

# Extract trait loadings (constraints)
trait_loadings <- as.data.frame(fortify(rda_NOR_CHE, display = "bp"))
trait_loadings$Trait <- rownames(trait_loadings)

trait_loadings <- trait_loadings |> 
  mutate(label = str_remove(label, "combined_treatment_region"))

# final plot RDA NOR ------------------------------------------------------
RDA_NOR_CHE <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = RDA1, y = RDA2, color = combined_treatment_region), 
             size = 4, alpha = 0.5) +
  
  # Species points in red
  geom_point(data = species_scores, 
             aes(x = RDA1, y = RDA2), 
             color = "red", size = 3, alpha = 0.5) +
  
  # Ellipses for treatment groups
  stat_ellipse(data = site_scores, 
               aes(x = RDA1, y = RDA2, color = combined_treatment_region), 
               size = 1) +
  
  # Species labels with no size legend
  geom_text_repel(data = species_scores, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  #color = "red", 
                  size = 7,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 23) +
  
  # Arrows for trait loadings
  geom_segment(data = trait_loadings, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "blue", size = 1) +
  
  # Trait labels with better spacing
  geom_text_repel(data = trait_loadings, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  color = "blue", 
                  size = 8,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 15) +
  
  # Customize plot labels and theme
  labs(x = "RDA1", y = "RDA2", color = "Treatment") +
  theme(legend.position = "right") +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  coord_cartesian(xlim = c(-1.3, 2), ylim = c(-4, 2))  # Adjust as needed

RDA_NOR_CHE


ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = RDA1, y = RDA2, color = combined_treatment_region), 
             size = 4, alpha = 0.5) +
  
  # Species points in red
  geom_point(data = species_scores, 
             aes(x = RDA1, y = RDA2), 
             color = "red", size = 3, alpha = 0.5) +
  
  # Ellipses for regions (CHE and NOR)
  stat_ellipse(data = site_scores, 
               aes(x = RDA1, y = RDA2, color = combined_treatment_region),  # Group by region
               size = 1)



















RDA_NOR_CHE <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = RDA1, y = RDA2, color = combined_treatment_region), 
             size = 4, alpha = 0.5) +
  
  # Species points in red
  geom_point(data = species_scores, 
             aes(x = RDA1, y = RDA2), 
             color = "red", size = 3, alpha = 0.5) +
  
  # Ellipses for regions (CHE and NOR)
  stat_ellipse(data = site_scores, 
               aes(x = RDA1, y = RDA2, color = combined_treatment_region),  # Group by region
               size = 1) +
  
  # Species labels
  geom_text_repel(data = species_scores, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  size = 7,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 23) +
  
  # Arrows for trait loadings
  geom_segment(data = trait_loadings, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "blue", size = 1) +
  
  # Trait labels
  geom_text_repel(data = trait_loadings, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  color = "blue", 
                  size = 8,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 15) +
  
  # Customize plot labels and theme
  labs(x = "RDA1", y = "RDA2", color = "Treatment / Region") +
  theme(legend.position = "right") +
  #scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  #coord_cartesian(xlim = c(-1.3, 2), ylim = c(-4, 2))  # Adjust as needed

RDA_NOR_CHE

