
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










