
# RangeX demographic and functional traits - SLA plots ------------

## Data used: RangeX_clean_functional_traits_NOR_2023.csv
##            RangeX_clean_yearly_size_2021_2022_2023_NOR.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      30.03.25
## Author:    Nadine Arzt
## Purpose:   Create SLA plots

source("RangeX_demographic_functional_traits_data_preparation_CHE_NOR.R")


# SLA all species NOR and CHE ---------------------------------------------
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


# SLA NOR and CHE species subset --------------------------------------------
species_subset <- species_subset |> 
  filter(!is.na(SLA))

SLA_NOR_CHE_species_subset <- ggplot(species_subset, aes(combined_treatment, SLA, fill = combined_treatment))+
  geom_boxplot()+
  labs(x = "Treatment", y = expression(SLA (mm^2/mg)))+
  theme(legend.position = "bottom")+
  stat_summary(fun = mean, geom = "point", shape = 20, 
               size = 2, color = "red")+
  scale_fill_manual(values = define_colors)+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8, vjust = 0.5, hjust = 0.5))+
  scale_x_discrete(labels = wrap_format(10)) +
  facet_wrap(vars(species), ncol = 4, scales = "free")
  # geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)
SLA_NOR_CHE_species_subset

ggsave(filename = "RangeX_SLA_per_species_subset_NOR_CHE.png", 
       plot = SLA_NOR_CHE_species_subset, 
       path = "Graphs", 
       width = 20, height = 13)


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
y_positions <- c(40, 40, 40, 43, 47, 51, 54)

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












