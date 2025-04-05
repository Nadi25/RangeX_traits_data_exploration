
# RangeX plot height vegetative ------------

## Data used: RangeX_clean_functional_traits_NOR_2023.csv
##            RangeX_clean_yearly_size_2021_2022_2023_NOR.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      24.03.25
## Author:    Nadine Arzt
## Purpose:   Plot height vegetative

source("RangeX_demographic_functional_traits_data_preparation_CHE_NOR.R")

traits_fun_demo_NOR_CHE

names(traits_fun_demo_NOR_CHE)

traits_fun_demo_NOR_CHE <- traits_fun_demo_NOR_CHE |> 
  filter(!is.na(height_vegetative)) |> 
  filter(!is.na(combined_treatment))


traits_fun_demo_NOR_CHE <- traits_fun_demo_NOR_CHE |> 
  mutate(region = case_when(
    region == "CHE" ~ "Switzerland", 
    region == "NOR" ~ "Norway"
  ))

# veg height NOR and CHE per treatment ---------------------------------------------
# Calculate means and confidence intervals for each treatment per country
summary_data_traits_NOR_CHE <- traits_fun_demo_NOR_CHE |> 
  group_by(region, combined_treatment) |> 
  summarise(
    mean_height_veg = mean(height_vegetative, na.rm = TRUE),
    ci_lower = mean_height_veg - qt(0.975, df = n() - 1) * sd(height_vegetative, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_height_veg + qt(0.975, df = n() - 1) * sd(height_vegetative, na.rm = TRUE) / sqrt(n())
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
height_veg_NOR_CHE <- ggplot(traits_fun_demo_NOR_CHE, aes(combined_treatment, height_vegetative, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_data_traits_NOR_CHE, aes(x = combined_treatment, y = mean_height_veg), color = "red", size = 3) +
  labs(x = "Treatment", y = "height vegetative (mm)")+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  facet_wrap(vars(region), scale = "free")+
  scale_fill_manual(values = define_colors)
height_veg_NOR_CHE




# only NOR ----------------------------------------------------------------
traits_fun_demo_NOR <- traits_fun_demo_NOR_CHE |> 
  filter(region == "Norway")

# Calculate means and confidence intervals for each treatment per country
summary_data_traits_NOR <- traits_fun_demo_NOR |> 
  group_by(region, combined_treatment) |> 
  summarise(
    mean_height_veg = mean(height_vegetative, na.rm = TRUE),
    ci_lower = mean_height_veg - qt(0.975, df = n() - 1) * sd(height_vegetative, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_height_veg + qt(0.975, df = n() - 1) * sd(height_vegetative, na.rm = TRUE) / sqrt(n())
  )
# Define y-positions for the significance bars
y_positions <- c(40, 40, 40, 43, 47, 51, 54)

height_veg_NOR <- ggplot(traits_fun_demo_NOR, aes(combined_treatment, height_vegetative, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_data_traits_NOR, aes(x = combined_treatment, y = mean_height_veg), color = "red", size = 3) +
  labs(x = "Treatment", y = "height vegetative (cm)")+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  facet_wrap(vars(region), scale = "free")+
  scale_fill_manual(values = define_colors)
height_veg_NOR



# only CHE ----------------------------------------------------------------
traits_fun_demo_CHE <- traits_fun_demo_NOR_CHE |> 
  filter(region == "Switzerland")

traits_fun_demo_CHE <- traits_fun_demo_CHE |> 
  mutate(height_vegetative = height_vegetative/10)

# Calculate means and confidence intervals for each treatment per country
summary_data_traits_CHE <- traits_fun_demo_CHE |> 
  group_by(region, combined_treatment) |> 
  summarise(
    mean_height_veg = mean(height_vegetative, na.rm = TRUE),
    ci_lower = mean_height_veg - qt(0.975, df = n() - 1) * sd(height_vegetative, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_height_veg + qt(0.975, df = n() - 1) * sd(height_vegetative, na.rm = TRUE) / sqrt(n())
  )
# Define y-positions for the significance bars

height_veg_CHE <- ggplot(traits_fun_demo_CHE, aes(combined_treatment, height_vegetative, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_data_traits_CHE, aes(x = combined_treatment, y = mean_height_veg), color = "red", size = 3) +
  labs(x = "Treatment", y = "height veg")+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  facet_wrap(vars(region), scale = "free")+
  scale_fill_manual(values = define_colors)
height_veg_CHE




# combine NOR and CHE -----------------------------------------------------

traits_fun_demo <- rbind(traits_fun_demo_CHE, traits_fun_demo_NOR)



summary_data_traits <- traits_fun_demo |> 
  group_by(region, combined_treatment) |> 
  summarise(
    mean_height_veg = mean(height_vegetative, na.rm = TRUE),
    ci_lower = mean_height_veg - qt(0.975, df = n() - 1) * sd(height_vegetative, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_height_veg + qt(0.975, df = n() - 1) * sd(height_vegetative, na.rm = TRUE) / sqrt(n())
  )
# Define y-positions for the significance bars
y_positions <- c(60, 60, 60, 63, 67, 71, 74)

theme_set(theme_bw(base_size = 30))

height_veg <- ggplot(traits_fun_demo, aes(combined_treatment, height_vegetative, fill = combined_treatment)) +
  geom_boxplot() +
  geom_point(data = summary_data_traits, aes(x = combined_treatment, y = mean_height_veg), color = "red", size = 3) +
  labs(x = "Treatment", y = "Height vegetative (cm)")+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)+
  facet_wrap(vars(region), scale = "free")+
  scale_fill_manual(values = define_colors)+
  guides(x = guide_axis(n.dodge = 2))
height_veg


ggsave(filename = "RangeX_height_veg_NOR_CHE.png", 
       plot = height_veg, 
       path = "Graphs", 
       width = 22, height = 11)




# CHE and NOR in same plot no facet ---------------------------------------
traits_fun_demo <- traits_fun_demo |> 
  mutate(combined_treatment_region = paste(region, combined_treatment))

comparisons <- list(
  c("Norway hi ambi vege", "Norway hi ambi bare"),
  c("Norway hi warm vege", "Norway hi warm bare"),
  c("Norway lo ambi vege", "Norway lo ambi bare"),
  c("Norway hi ambi bare", "Norway hi warm bare"),
  c("hNorway i ambi vege", "Norway hi warm vege"),
  c("Norway hi ambi bare", "Norway hi warm bare"),
  c("Norway hi ambi vege", "Norway lo ambi vege")
)

summary_data_traits <- traits_fun_demo |> 
  group_by(combined_treatment_region) |> 
  summarise(
    mean_height_veg = mean(height_vegetative, na.rm = TRUE),
    ci_lower = mean_height_veg - qt(0.975, df = n() - 1) * sd(height_vegetative, na.rm = TRUE) / sqrt(n()),
    ci_upper = mean_height_veg + qt(0.975, df = n() - 1) * sd(height_vegetative, na.rm = TRUE) / sqrt(n())
  )
# Define y-positions for the significance bars
y_positions <- c(60, 60, 60, 63, 67, 71, 74)

height_veg <- ggplot(traits_fun_demo, aes(combined_treatment_region, height_vegetative, fill = combined_treatment_region)) +
  geom_boxplot() +
  geom_point(data = summary_data_traits, aes(x = combined_treatment_region, y = mean_height_veg), color = "red", size = 3) +
  labs(x = "Treatment", y = "height vegetative (cm)")+
  theme(legend.position = "none")+
  geom_signif(comparisons = comparisons, map_signif_level = TRUE, y_position = y_positions)
  scale_fill_manual(values = define_colors)
height_veg



