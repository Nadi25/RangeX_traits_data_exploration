

# Vegetative height vs number of leaves -----------------------------------



source("RangeX_demographic_functional_traits_data_preparation_CHE_NOR.R")

traits_fun_demo_NOR_CHE

names(traits_fun_demo_NOR_CHE)



ggplot(traits_fun_demo_NOR_CHE, aes(x = number_leaves, y = height_vegetative, shape = region, color = combined_treatment)) +
  geom_point() +
  labs(title = "Vegetative Height vs Number of Leaves by Region",
       x = "Number of Leaves",
       y = "Vegetative Height")+
  geom_smooth(method = "lm", se = FALSE)
