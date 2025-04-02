
# RangeX demographic and functional traits - number of flowers plots ------------

## Data used: RangeX_clean_functional_traits_NOR_2023.csv
##            RangeX_clean_yearly_size_2021_2022_2023_NOR.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      30.03.25
## Author:    Nadine Arzt
## Purpose:   Create number of flowers plots


source("RangeX_demographic_functional_traits_data_preparation_CHE_NOR.R")


head(traits_fun_demo_NOR_CHE)


# exclude NAs combined treat --------------------------------------------------
# not good to do that --> needs to be fixed before in data preparation
traits_fun_demo_NOR_CHE <- traits_fun_demo_NOR_CHE |> 
  filter(!is.na(combined_treatment))


flowers_NOR_CHE_species <- ggplot(traits_fun_demo_NOR_CHE, aes(combined_treatment, number_flowers, fill = combined_treatment))+
  geom_boxplot()+
  labs(x = "Treatment", y = "Number of flowers")+
  theme(legend.position = "bottom")+
  stat_summary(fun = mean, geom = "point", shape = 20, 
               size = 2, color = "red")+
  scale_fill_manual(values = define_colors)+
  theme(legend.position = "none")+
  facet_wrap(vars(species), ncol = 5, scales = "free")
flowers_NOR_CHE_species

ggsave(filename = "RangeX_flowers_per_species_NOR_CHE.png", 
       plot = flowers_NOR_CHE_species, 
       path = "Graphs", 
       width = 20, height = 18)


# filter species ----------------------------------------------------------
# tripra is doing better at high site --> winner
# leucanthemum same



# number flwoers NOR and CHE per treatment ---------------------------------------------
traits_fun_demo_NOR_CHE <- traits_fun_demo_NOR_CHE |> 
  filter(!is.na(number_flowers))


# Calculate means and confidence intervals for each treatment per country
summary_data_traits_NOR_CHE <- traits_fun_demo_NOR_CHE |> 
  group_by(region, combined_treatment) |> 
  summarise(
    mean_number_flowers = mean(number_flowers, na.rm = TRUE),
    ci_lower = mean_number_flowers - qt(0.975, df = n() - 1) * sd(number_flowers, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_number_flowers + qt(0.975, df = n() - 1) * sd(number_flowers, na.rm = TRUE) / sqrt(n())
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
number_flowers_NOR_CHE <- ggplot(traits_fun_demo_NOR_CHE, aes(combined_treatment, number_flowers, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_data_traits_NOR_CHE, aes(x = combined_treatment, y = mean_number_flowers), color = "red", size = 3) +
  labs(x = "Treatment", y = "Mean number of flowers")+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  facet_wrap(vars(region), , scales = "free")+
  scale_fill_manual(values = define_colors)
number_flowers_NOR_CHE

ggsave(filename = "RangeX_SLA_per_treat_NOR_CHE.png", 
       plot = SLA_NOR_CHE, 
       path = "Graphs", 
       width = 19, height = 8)









