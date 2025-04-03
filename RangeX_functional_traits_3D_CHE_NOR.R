
# RangeX demographic and functional traits 3D plots ------------

## Data used: RangeX_clean_functional_traits_NOR_2023.csv
##            RangeX_clean_yearly_size_2021_2022_2023_NOR.csv
##            RangeX_metadata_focal_NOR.csv
## Date:      24.03.25
## Author:    Nadine Arzt
## Purpose:   Create 3D plots

source("RangeX_demographic_functional_traits_data_preparation_CHE_NOR.R")


# 3D plot NOR ---------------------------------------------------------------
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
shape_mapping <- c("Norway" = "circle", "Switzerland" = "diamond")

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


# 3d functional traits CHE and NOR final plot --------------------------------
# make plot with better legend
NOR_CHE_plot_3d_mean_treat <- plot_ly() |> 
  # Add CHE points with legend visible
  add_markers(data = mean_traits_treat_NOR_CHE |> 
                filter(region == "Switzerland"),
              x = ~mean_SLA, y = ~mean_LDMC, z = ~mean_leaf_thickness,
              color = ~combined_treatment, colors = define_colors,
              symbol = ~region, symbols = shape_mapping,
              marker = list(size = 18),
              showlegend = FALSE) |> 
  
  # Add NOR points with legend hidden
  add_markers(data = mean_traits_treat_NOR_CHE |> filter(region == "Norway"),
              x = ~mean_SLA, y = ~mean_LDMC, z = ~mean_leaf_thickness,
              color = ~combined_treatment, colors = define_colors,
              symbol = ~region, symbols = shape_mapping,
              marker = list(size = 20),
              showlegend = TRUE) |> 
  
  # Layout adjustments for axis labels
  layout(scene = list(
    xaxis = list(title = "Mean SLA (mm\u00B2/mg)", titlefont  = list(size = 21)),
    yaxis = list(title = "Mean LDMC (mg g\u207B\u00B9)", titlefont = list(size = 21)),
    zaxis = list(title = "Mean leaf thickness (mm)", titlefont = list(size = 21))))

NOR_CHE_plot_3d_mean_treat <- NOR_CHE_plot_3d_mean_treat|> 
  layout(legend = list(x = 0.8, y = 0.8))


# Add a separate annotation for shape legend without title
NOR_CHE_plot_3d_mean_treat <- NOR_CHE_plot_3d_mean_treat |> 
  layout(annotations = list(
    list(
      x = 0.9, y = 0.4,  # Position outside the plot
      xref = "paper", yref = "paper",
      text = "◊ Switzerland<br>● Norway",
      showarrow = FALSE,
      titlefont = list(size = 22)
    )
  ))

NOR_CHE_plot_3d_mean_treat 



