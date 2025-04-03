
# RangeX plot leaf thickness ------------

## Data used: RangeX_clean_functional_traits_NOR_2023.csv
##            RangeX_clean_yearly_size_2021_2022_2023_NOR.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      24.03.25
## Author:    Nadine Arzt
## Purpose:   Plot leaf thickness


# comments ----------------------------------------------------------------
# leaf thickness CHE: outlier in Brachypodium and Daucus--> check!!
# CHE.hi.ambi.bare.wf.08.16.1 = 14.6 must be typo --> exclude for now
# CHE.hi.ambi.vege.wf.09.13.2 = 6.8 must be typo --> exclude for now

source("RangeX_demographic_functional_traits_data_preparation_CHE_NOR.R")

# leaf thickness NOR --------------------------------------------------------
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


# leaf thickness CHE -------------------------------------------------------------
ggplot(traits_23_CHE, aes(combined_treatment, leaf_thickness, fill = combined_treatment))+
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
                                       data = traits_23_CHE)

# View the summary of the model
summary(model_leaf_thickness)
# species explains very little variance
# 

# Calculate means and confidence intervals for each treatment
summary_leaf_thickness <- traits_23_CHE |> 
  group_by(combined_treatment) |> 
  summarise(
    mean_leaf_thickness = mean(leaf_thickness, na.rm = TRUE),
    ci_lower = mean_leaf_thickness- qt(0.975, df = n() - 1) * sd(leaf_thickness, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_leaf_thickness + qt(0.975, df = n() - 1) * sd(leaf_thickness, na.rm = TRUE) / sqrt(n())
  )

# Define y-positions for the significance bars
y_positions <- c(0.59, 0.59, 0.6, 0.66, 0.7, 0.77)

# plot all species together
leaf_thickness_all <- ggplot(traits_23_CHE, aes(combined_treatment, leaf_thickness, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_leaf_thickness, aes(x = combined_treatment, y = mean_leaf_thickness), color = "red", size = 3) +
  labs(x = "Treatment", y = "leaf_thickness (mm)")+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  scale_fill_manual(values = define_colors)
leaf_thickness_all

ggsave(filename = "RangeX_leaf_thickness_all_species_CHE.png", 
       plot = leaf_thickness_all, 
       path = "Graphs", 
       width = 10, height = 6)


leaf_thickness_species <- ggplot(traits_23_CHE, aes(combined_treatment, leaf_thickness, fill = combined_treatment)) +
  geom_boxplot() +
  labs(x = "Treatment", y = "Leaf thickness (mm)")+
  theme(legend.position = "bottom")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  scale_fill_manual(values = define_colors)+
  facet_wrap(vars(species), ncol = 5)
leaf_thickness_species

# Leaf thickness NOR and CHE per treatment ---------------------------------------------
# Calculate means and confidence intervals for each treatment per country
summary_data_traits_NOR_CHE <- traits_NOR_CHE |> 
  group_by(region, combined_treatment) |> 
  summarise(
    mean_leaf_thick = mean(leaf_thickness, na.rm = TRUE),
    ci_lower = mean_leaf_thick - qt(0.975, df = n() - 1) * sd(leaf_thickness, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_leaf_thick + qt(0.975, df = n() - 1) * sd(leaf_thickness, na.rm = TRUE) / sqrt(n())
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
y_positions <- c(0.8, 0.8, 0.8, 0.9, 1, 1.1, 1.2)

# plot all species together
leaf_thick_NOR_CHE <- ggplot(traits_NOR_CHE, aes(combined_treatment, leaf_thickness, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_data_traits_NOR_CHE, aes(x = combined_treatment, y = mean_leaf_thick), color = "red", size = 3) +
  labs(x = "Treatment", y = "leaf thickness (mm)")+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  facet_wrap(vars(region))+
  scale_fill_manual(values = define_colors)
leaf_thick_NOR_CHE

ggsave(filename = "RangeX_leaf_thickness_per_treat_NOR_CHE.png", 
       plot = leaf_thick_NOR_CHE, 
       path = "Graphs", 
       width = 19, height = 8)




