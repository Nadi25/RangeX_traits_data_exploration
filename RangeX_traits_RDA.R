
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


# Convert integer columns to numeric
traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR_PCA |> 
  mutate(across(where(is.integer), as.numeric))

# NOR this one! perform rda and plot it -----------------------------------------
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


# plot RDA NOR ------------------------------------------------------
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



# RDA NOR plot -------------------------------------------------
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


# RDA NOR plot poster margins same as in CHE ----------------------------
RDA_NOR_ <- ggplot() +
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
  coord_cartesian(xlim = c(-2.5, 2), ylim = c(-4, 4))

RDA_NOR_

ggsave(filename = "RangeX_RDA_NOR_poster_zoom.png", 
       plot = RDA_NOR_, 
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


# RDA CHE plot poster margins same as in NOR ---------------------
RDA_CHE_ <- ggplot() +
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
  theme(legend.position = "right",
        legend.text = element_text(size = 16),  # Adjust legend text size
        axis.title = element_text(size = 14)) +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # 
  coord_cartesian(xlim = c(-2.5, 2), ylim = c(-4, 4))
RDA_CHE_

ggsave(filename = "RangeX_RDA_CHE_poster_zoom.png", 
       plot = RDA_CHE_, 
       path = "Graphs", 
       width = 15, height = 15)



# RDA final without wet mass ------------------------------------------------
# dont use wet mas because it is closely linked to dry mass
# NOR ---------------------------------------------------------------------
traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR |> 
  filter(!is.na(wet_mass) | !is.na(dry_mass) | !is.na(leaf_area) 
         | !is.na(leaf_thickness) | !is.na(SLA) | !is.na(LDMC))

length(traits_fun_demo_NOR_PCA$region) # 594 now

traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR_PCA |> 
  select(leaf_thickness, leaf_area, dry_mass, SLA, LDMC,
         height_vegetative_str, height_vegetative, leaf_length1,
         number_leaves, number_flowers, species, combined_treatment,
         treat_warming, treat_competition)

traits_fun_demo_NOR_PCA <- traits_fun_demo_NOR_PCA |>
  na.omit()

length(traits_fun_demo_NOR_PCA$leaf_thickness) # 451 now



species_data <- traits_fun_demo_NOR_PCA |> 
  select(species)
trait_data <- traits_fun_demo_NOR_PCA |> select(leaf_thickness: number_flowers)
meta_data <- traits_fun_demo_NOR_PCA |> 
  select(species, combined_treatment, treat_warming, treat_competition)

rda_NOR <- rda(trait_data ~ combined_treatment + Condition(species), data = meta_data, scale = TRUE)

rda_NOR |> anova()

summary(rda_NOR)


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
trait_loadings <- as.data.frame(fortify(rda_NOR, display = "cn"))
trait_loadings$Trait <- rownames(trait_loadings)

trait_loadings <- trait_loadings |> 
  mutate(label = str_remove(label, "combined_treatment"))


RDA_NOR_ <- ggplot() +
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
  labs(x = "RDA1 (13.47%)", y = "RDA2 (4.29%)", color = "Treatment") +
  theme(legend.position = "right") +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  coord_cartesian(xlim = c(-2.5, 2), ylim = c(-4, 4))
RDA_NOR_

# make axis labels bigger
RDA_NOR_ <- RDA_NOR_ +
  theme(axis.title = element_text(size = 28))
RDA_NOR_
# legend text bigger doesnt work
RDA_NOR_ <- RDA_NOR_ +
  theme(legend.text = element_text(size = 16))
RDA_NOR_

ggsave(filename = "RangeX_RDA_NOR_poster_zoom_without_wetmass_final.png", 
       plot = RDA_NOR_, 
       path = "Graphs", 
       width = 15, height = 15)

# NOR RDA with arrows for traits ------------------------------------------
# flipped the x axis around with -RDA1 to make it better comparable with NOR
RDA_NOR_ <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = RDA1, y = RDA2, color = combined_treatment), 
             size = 4, alpha = 0.5) +
  
  # centroids
  geom_point(data = trait_loadings, 
             aes(x = RDA1, y = RDA2), 
             color = "red", size = 7, shape = 18) +
  
  # Ellipses for treatment groups
  stat_ellipse(data = site_scores, 
               aes(x = RDA1, y = RDA2, color = combined_treatment), 
               size = 1) +
  
  # trait arrows
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "blue", size = 1)+
  # Trait labels
  geom_text_repel(data = species_scores, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  color = "blue", 
                  size = 6,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 18)+
  
  # Customize plot labels and theme
  labs(x = "RDA1 (13.47%)", y = "RDA2 (4.29%)", color = "Treatment") +
  theme(legend.position = "none") +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  coord_equal()
RDA_NOR_

ggsave(filename = "RangeX_RDA_NOR_poster_zoom_without_wetmass_with_arrows_final.png", 
       plot = RDA_NOR_, 
       path = "Graphs", 
       width = 15, height = 15)

RDA_NOR_ <- RDA_NOR_ +
  coord_equal(xlim = c(-2.5, 3), ylim = c(-6, 4))
RDA_NOR_

ggsave(filename = "RangeX_RDA_NOR_poster_zoom_without_wetmass_with_arrows_larger.png", 
       plot = RDA_NOR_, 
       path = "Graphs", 
       width = 15, height = 15)

# CHE ---------------------------------------------------------------------
traits_fun_demo_CHE_PCA <- traits_fun_demo_CHE |> 
  filter(!is.na(wet_mass) | !is.na(dry_mass) | !is.na(leaf_area) 
         | !is.na(leaf_thickness) | !is.na(SLA) | !is.na(LDMC))

length(traits_fun_demo_CHE_PCA$region) # 527 now

traits_fun_demo_CHE_PCA <- traits_fun_demo_CHE_PCA |> 
  select(leaf_thickness, leaf_area, dry_mass, SLA, LDMC,
         height_vegetative_str, height_vegetative, leaf_length1,
         number_leaves, number_flowers, species, combined_treatment,
         treat_warming, treat_competition)

traits_fun_demo_CHE_PCA <- traits_fun_demo_CHE_PCA |>
  na.omit()

length(traits_fun_demo_CHE_PCA$leaf_thickness) # 223 now


# get species, trait and meta data
species_data <- traits_fun_demo_CHE_PCA |> 
  select(species)
trait_data <- traits_fun_demo_CHE_PCA |> select(leaf_thickness: number_flowers)
meta_data <- traits_fun_demo_CHE_PCA |> 
  select(species, combined_treatment, treat_warming, treat_competition)

rda_CHE <- rda(trait_data ~ combined_treatment + Condition(species), data = meta_data, scale = TRUE)

rda_CHE |> anova()

summary(rda_CHE)
# rda1 = 0.1192 
# rda2 = 0.07232 


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
trait_loadings <- as.data.frame(fortify(rda_CHE, display = "cn"))
trait_loadings$Trait <- rownames(trait_loadings)

trait_loadings <- trait_loadings |> 
  mutate(label = str_remove(label, "combined_treatment"))


RDA_CHE_ <- ggplot() +
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
  labs(x = "RDA1 (11.92%)", y = "RDA2 (7.23%)", color = "Treatment") +
  theme(legend.position = "right") +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  coord_cartesian(xlim = c(-2.5, 2), ylim = c(-4, 4))
RDA_CHE_

RDA_CHE_ <- RDA_CHE_ +
  theme(axis.title = element_text(size = 28))
RDA_CHE_

ggsave(filename = "RangeX_RDA_CHE_poster_zoom_without_wetmass_final.png", 
       plot = RDA_CHE_, 
       path = "Graphs", 
       width = 15, height = 15)



# CHE RDA with arrows for traits ------------------------------------------
# flipped the x axis around with -RDA1 to make it better comparable with NOR
RDA_CHE_ <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = -RDA1, y = RDA2, color = combined_treatment), 
             size = 4, alpha = 0.5) +
  
  # centroids
  geom_point(data = trait_loadings, 
             aes(x = -RDA1, y = RDA2), 
             color = "red", size = 7, shape = 18) +
  
  # Ellipses for treatment groups
  stat_ellipse(data = site_scores, 
               aes(x = -RDA1, y = RDA2, color = combined_treatment), 
               size = 1) +
  
  # trait arrows
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = -RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "blue", size = 1)+
  # Trait labels
  geom_text_repel(data = species_scores, 
                  aes(x = -RDA1, y = RDA2, label = label), 
                  color = "blue", 
                  size = 6,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 18)+
  
  # Customize plot labels and theme
  labs(x = "RDA1 (11.92%)", y = "RDA2 (7.23%)", color = "Treatment") +
  theme(legend.position = "none") +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  coord_equal()
RDA_CHE_

ggsave(filename = "RangeX_RDA_CHE_poster_zoom_without_wetmass_with_arrows_final.png", 
       plot = RDA_CHE_, 
       path = "Graphs", 
       width = 15, height = 15)

RDA_CHE_ <- RDA_CHE_ +
  coord_equal(xlim = c(-2.5, 3), ylim = c(-6, 4))
RDA_CHE_

ggsave(filename = "RangeX_RDA_CHE_poster_zoom_without_wetmass_with_arrows_larger.png", 
       plot = RDA_CHE_, 
       path = "Graphs", 
       width = 15, height = 15)




# NOR hist ----------------------------------------------------------------
# pivot long
traits_long_NOR <- traits_fun_demo_NOR_PCA |> 
  pivot_longer(cols = c(leaf_thickness, leaf_area, dry_mass, SLA, LDMC,
                        height_vegetative_str, height_vegetative, leaf_length1,
                        number_leaves, number_flowers),
               names_to = "Trait", values_to = "Value")

# Histogram plot
ggplot(traits_long_NOR, aes(x = (Value), fill = combined_treatment)) +
  geom_histogram(color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(vars(Trait), scales = "free") + 
  scale_fill_manual(values = define_colors) +
  labs(x = "Trait Value", y = "Count", fill = "Treatment") 


# all traits except LDMC should be log transformed
# number leaves and flowrs needs log1p 


# NOR log transform -------------------------------------------------------
# this plots all traits against each other to check weird patterns
GGally::ggpairs(traits_fun_demo_NOR_PCA)

# log transform all except LDMC
traits_fun_demo_NOR_RDA_log <- traits_fun_demo_NOR_PCA |> 
  mutate(across(c(leaf_thickness, leaf_area, dry_mass, SLA, height_vegetative_str, 
                  height_vegetative, leaf_length1), log),
         across(c(number_leaves, number_flowers), log1p)) 


species_data <- traits_fun_demo_NOR_RDA_log |> 
  select(species)
trait_data <- traits_fun_demo_NOR_RDA_log |> select(leaf_thickness: number_flowers)
meta_data <- traits_fun_demo_NOR_RDA_log |> 
  select(species, combined_treatment, treat_warming, treat_competition)

rda_NOR <- rda(trait_data ~ combined_treatment + Condition(species), data = meta_data, scale = TRUE)

rda_NOR |> anova()

summary(rda_NOR)
# RDA1 0.1637 
# RDA2 0.0324 

# Extract site scores
site_scores <- as.data.frame(fortify(rda_NOR, display = "sites"))
site_scores$Site <- rownames(site_scores) # not needed only gives number

# add treat to site scores
site_scores$combined_treatment <- traits_fun_demo_NOR_RDA_log$combined_treatment 

# Extract species scores
# means traits
species_scores <-fortify(rda_NOR, display = "species")
# species_scores$Species <- rownames(species_scores)

# Extract trait loadings (constraints)
# cn means centroids
trait_loadings <- fortify(rda_NOR, display = "cn")
# trait_loadings$Trait <- rownames(trait_loadings)
# remove combined_treatment in label
trait_loadings <- trait_loadings |> 
  mutate(label = str_remove(label, "combined_treatment"))

RDA_NOR_ <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = RDA1, y = RDA2, color = combined_treatment), 
             size = 4, alpha = 0.5) +
  
  # centroids
  geom_point(data = trait_loadings, 
             aes(x = RDA1, y = RDA2), 
             color = "red", size = 7, shape = 18) +
  
  # Ellipses for treatment groups
  stat_ellipse(data = site_scores, 
               aes(x = RDA1, y = RDA2, color = combined_treatment), 
               size = 1) +
  
  # trait arrows
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "blue", size = 1)+
  # Trait labels
  geom_text_repel(data = species_scores, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  color = "blue", 
                  size = 6,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 18)+
  
  # Customize plot labels and theme
  labs(x = "RDA1 (16.37%)", y = "RDA2 (3.24%)", color = "Treatment") +
  theme(legend.position = "none") +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  coord_equal()
RDA_NOR_

ggsave(filename = "RangeX_RDA_NOR_log_transformed.png", 
       plot = RDA_NOR_, 
       path = "Graphs", 
       width = 15, height = 15)

# this looks better because outlier in bottom left are gone
# but veg height str = 0 in species_scores
# that is weird
# what happens if we exclude veg height str?


# Final NOR log transform without veg height str -------------------------------------------------------
# dont use veg height str
# and rename leaf_lenght1
traits_fun_demo_NOR_RDA <- traits_fun_demo_NOR_PCA |> 
  select(-height_vegetative_str) |> 
  rename(leaf_length = leaf_length1)

# log transform all except LDMC
traits_fun_demo_NOR_RDA_log <- traits_fun_demo_NOR_RDA |> 
  mutate(across(c(leaf_thickness, leaf_area, dry_mass, SLA,
                  height_vegetative, leaf_length), log),
         across(c(number_leaves, number_flowers), log1p)) 


species_data <- traits_fun_demo_NOR_RDA_log |> 
  select(species)
trait_data <- traits_fun_demo_NOR_RDA_log |> select(leaf_thickness: number_flowers)
meta_data <- traits_fun_demo_NOR_RDA_log |> 
  select(species, combined_treatment, treat_warming, treat_competition)

rda_NOR <- rda(trait_data ~ combined_treatment + Condition(species), data = meta_data, scale = TRUE)

rda_NOR |> anova()

summary(rda_NOR)
# RDA1 0.1637 
# RDA2 0.0324 
# is it supposed to have the same values as without veg height str?

# Extract site scores
site_scores <- as.data.frame(fortify(rda_NOR, display = "sites"))

# add treat to site scores
site_scores$combined_treatment <- traits_fun_demo_NOR_RDA_log$combined_treatment 

# Extract species scores
# means traits
species_scores <-fortify(rda_NOR, display = "species")
# dry mass looks like it could be 0 in the plot but it's just small

# Extract trait loadings (constraints)
# cn means centroids
trait_loadings <- fortify(rda_NOR, display = "cn")
# remove combined_treatment in label
trait_loadings <- trait_loadings |> 
  mutate(label = str_remove(label, "combined_treatment"))

RDA_NOR_ <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = RDA1, y = RDA2, color = combined_treatment), 
             size = 4, alpha = 0.5) +
  
  # centroids
  geom_point(data = trait_loadings, 
             aes(x = RDA1, y = RDA2), 
             color = "red", size = 7, shape = 18) +
  
  # Ellipses for treatment groups
  stat_ellipse(data = site_scores, 
               aes(x = RDA1, y = RDA2, color = combined_treatment), 
               size = 1) +
  
  # trait arrows
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "blue", size = 1)+
  # Trait labels
  geom_text_repel(data = species_scores, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  color = "blue", 
                  size = 6,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 18)+
  
  # Customize plot labels and theme
  labs(x = "RDA1 (16.37%)", y = "RDA2 (3.24%)", color = "Treatment") +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 20)) +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  coord_equal()
RDA_NOR_

ggsave(filename = "RangeX_RDA_NOR_log_transformed.png", 
       plot = RDA_NOR_, 
       path = "Graphs", 
       width = 18, height = 15)


# CHE hist ----------------------------------------------------------------
# pivot long
traits_long_CHE <- traits_fun_demo_CHE_PCA |> 
  pivot_longer(cols = c(leaf_thickness, leaf_area, dry_mass, SLA, LDMC,
                        height_vegetative_str, height_vegetative, leaf_length1,
                        number_leaves, number_flowers),
               names_to = "Trait", values_to = "Value")

# Histogram plot
ggplot(traits_long_CHE, aes(x = (Value), fill = combined_treatment)) +
  geom_histogram(color = "black", bins = 30, alpha = 0.7) +
  facet_wrap(vars(Trait), scales = "free") + 
  scale_fill_manual(values = define_colors) +
  labs(x = "Trait Value", y = "Count", fill = "Treatment") 

# CHE log transform -------------------------------------------------------
# this plots all traits against each other to check weird patterns
# GGally::ggpairs(traits_fun_demo_CHE_PCA)

# make it consistent with NOR and exclude veg height str
traits_fun_demo_CHE_RDA <- traits_fun_demo_CHE_PCA |> 
  select(-height_vegetative_str) |> 
  rename(leaf_length = leaf_length1)

# log transform all except LDMC
# log1p handles 0 better e.g. for number of flowers
traits_fun_demo_CHE_RDA_log <- traits_fun_demo_CHE_RDA |> 
  mutate(across(c(leaf_thickness, leaf_area, dry_mass, SLA,
                  height_vegetative, leaf_length), log),
         across(c(number_leaves, number_flowers), log1p)) 


species_data <- traits_fun_demo_CHE_RDA_log |> 
  select(species)
trait_data <- traits_fun_demo_CHE_RDA_log |> select(leaf_thickness: number_flowers)
meta_data <- traits_fun_demo_CHE_RDA_log |> 
  select(species, combined_treatment, treat_warming, treat_competition)

rda_CHE <- rda(trait_data ~ combined_treatment + Condition(species), data = meta_data, scale = TRUE)

rda_CHE |> anova()

summary(rda_CHE)
# RDA1 0.1901
# RDA2 0.06994
# 26% of the varaince is explained by treatment?

# Extract site scores
site_scores <- as.data.frame(fortify(rda_CHE, display = "sites"))

# add treat to site scores
site_scores$combined_treatment <- traits_fun_demo_CHE_RDA_log$combined_treatment 

# Extract species scores
# means traits
species_scores <-fortify(rda_CHE, display = "species")

# Extract trait loadings (constraints)
# cn means centroids
trait_loadings <- fortify(rda_CHE, display = "cn")
# remove combined_treatment in label
trait_loadings <- trait_loadings |> 
  mutate(label = str_remove(label, "combined_treatment"))

# plot with flipped x axis by -RDA1
RDA_CHE_ <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = -RDA1, y = RDA2, color = combined_treatment), 
             size = 4, alpha = 0.5) +
  
  # centroids
  geom_point(data = trait_loadings, 
             aes(x = -RDA1, y = RDA2), 
             color = "red", size = 7, shape = 18) +
  
  # Ellipses for treatment groups
  stat_ellipse(data = site_scores, 
               aes(x = -RDA1, y = RDA2, color = combined_treatment), 
               size = 1) +
  
  # trait arrows
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = -RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "blue", size = 1)+
  # Trait labels
  geom_text_repel(data = species_scores, 
                  aes(x = -RDA1, y = RDA2, label = label), 
                  color = "blue", 
                  size = 6,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 18)+
  
  # Customize plot labels and theme
  labs(x = "RDA1 (19.01%)", y = "RDA2 (6.99%)", color = "Treatment") +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 20)) +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  coord_equal()
RDA_CHE_

ggsave(filename = "RangeX_RDA_CHE_log_transformed.png", 
       plot = RDA_CHE_, 
       path = "Graphs", 
       width = 18, height = 15)

# plot without flipped x axis
RDA_CHE_ <- ggplot() +
  # Site points with colors by treatment
  geom_point(data = site_scores, 
             aes(x = RDA1, y = RDA2, color = combined_treatment), 
             size = 4, alpha = 0.5) +
  
  # centroids
  geom_point(data = trait_loadings, 
             aes(x = RDA1, y = RDA2), 
             color = "red", size = 7, shape = 18) +
  
  # Ellipses for treatment groups
  stat_ellipse(data = site_scores, 
               aes(x = RDA1, y = RDA2, color = combined_treatment), 
               size = 1) +
  
  # trait arrows
  geom_segment(data = species_scores, 
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "blue", size = 1)+
  # Trait labels
  geom_text_repel(data = species_scores, 
                  aes(x = RDA1, y = RDA2, label = label), 
                  color = "blue", 
                  size = 6,
                  box.padding = 0.5,
                  point.padding = 0.3,
                  max.overlaps = 18)+
  
  # Customize plot labels and theme
  labs(x = "RDA1 (19.01%)", y = "RDA2 (6.99%)", color = "Treatment") +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.text = element_text(size = 20)) +
  scale_color_manual(values = define_colors) +
  
  # Remove size legend 
  guides(size = "none")+
  
  # Zoom in on central data
  coord_equal()
RDA_CHE_

ggsave(filename = "RangeX_RDA_CHE_log_transformed_not_flipped_xaxis.png", 
       plot = RDA_CHE_, 
       path = "Graphs", 
       width = 18, height = 15)


