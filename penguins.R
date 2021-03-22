## ----setup, include=FALSE-----------------------------------------------------
library(tidyverse)
library(here)
library(palmerpenguins)
library(kableExtra)
library(patchwork)
library(gt)
library(GGally)
library(recipes)
library(shadowtext)
library(broom)
knitr::opts_chunk$set(echo = TRUE)

theme_set(theme_minimal())


## ---- echo = FALSE------------------------------------------------------------
overview_compare <- tribble(
  ~Feature, ~iris, ~penguins,
  "Year(s) collected", "1935", "2007 - 2009",
  "Dimensions (col x row)", "5 x 150", "8 x 344",
  "Documentation", "minimal", "complete metadata",
  "Variable classes", "double (4), factor (1)", "double (2), int (3), factor (3)",
  "Missing values?", "no (n = 0; 0.0%)", "yes (n = 19; 0.7%)"
)

overview_compare %>% 
  kable(caption = "Overview comparison of penguins and iris dataset features and characteristics.")


## ---- echo = FALSE------------------------------------------------------------
iris_counts <- iris %>% 
  count(Species) %>% 
  rename(`Iris species` = Species)

penguin_counts <- penguins %>% 
  mutate(species = as.character(species)) %>% 
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie", 
    species == "Chinstrap" ~ "chinstrap",
    species == "Gentoo" ~ "gentoo",
    TRUE ~ species
  )) %>% 
  mutate(sex = str_to_title(sex)) %>% 
  group_by(species, sex) %>% 
  tally() %>% 
  pivot_wider(names_from = sex, values_from = n) %>% 
  replace_na(list(`NA` = 0)) %>% 
  as.data.frame() %>% 
  rename(`Penguin species` = species)


iris_penguin_n <- cbind(iris_counts, penguin_counts) %>%
   rename(`Sample size` = n)

iris_penguin_n %>% 
   kable(caption = "Grouped sample size for iris (by species; n = 150 total) and penguins (by species and sex; n = 344 total). Penguins can be further grouped by variables for island and study year.", align = "lclccc") %>% 
  add_header_above(c("Iris sample size (by species)" = 2, "Penguin sample size (by species and sex)" = 4 ))



## ---- penguins-pairs, fig.cap = "Distribution and correlations for numeric variables in the penguins data (flipper length (mm), body mass, (g) bill length (mm) and bill depth (mm)) for the three observed species: gentoo (green, triangles); chinstrap (blue, circles); and Adélie (orange, squares). Correlations are Pearson's r (*p < 0.05; **p < 0.01; ***p < 0.001).", warning = FALSE, message = FALSE, echo = FALSE----
penguin_pairs <- penguins %>%
  mutate(species = as.character(species)) %>% 
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie", 
    TRUE ~ species
  )) %>% 
  select(species, where(is.numeric)) %>% 
  ggpairs(aes(color = species, shape = species),
          columns = c("flipper_length_mm", "body_mass_g", 
                      "bill_length_mm", "bill_depth_mm"),
          columnLabels = c("Flipper length (mm)","Body mass (g)", "Bill length (mm)", "Bill depth (mm)"),
          upper = list(continuous = wrap("cor", size = 2.7)),
          lower = list(continuous = wrap(ggally_points, size = 1.3))) +
  scale_fill_manual(values = c("darkorange","#0072b2","#009e73")) +
  scale_color_manual(values = c("darkorange","#0072b2","#009e73")) +
  scale_shape_manual(values = c(15,16,17)) +
  theme_minimal() +
  theme(
        text = element_text(size = 9),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "gray80", fill = NA)
        )

penguin_pairs


## ---- iris-pairs, fig.cap = "Distribution and correlations for numeric variables in the iris data (petal length (cm), petal width (cm), sepal length (cm) and sepal width (cm)) for the three included Iris species: setosa (light gray, circles); versicolor (dark gray, triangles); and virginica (black, squares). Correlations are Pearson's r (*p < 0.05; **p < 0.01; ***p < 0.001).", warning = FALSE, message = FALSE, echo = FALSE----
iris_pairs <- iris %>%
  ggpairs(aes(color = Species, shape = Species),
          columns = c("Petal.Length", "Petal.Width", 
                      "Sepal.Length", "Sepal.Width"),
          columnLabels = c("Petal length (cm)","Petal width (cm)", "Sepal length (cm)", "Sepal width (cm)"),
          upper = list(continuous = wrap("cor", size = 2.7, color = "black"))) +
  scale_colour_manual(values = c("gray70","gray40","black")) +
  scale_fill_manual(values = c("gray70","gray40","black")) +
  theme_minimal() +
  theme(
        text = element_text(size = 9),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = "gray80")
        )

iris_pairs



## ---- linear-example, fig.cap = "Representative linear relationships for penguin flipper length (mm) and body mass (g) for Adélie (orange circles), chinstrap (blue triangles), and gentoo (green squares) penguins (A), and iris petal length (cm) and width (cm) for setosa (light gray circles), versicolor (dark gray triangles) and virginica (black squares) irises (B). Within-species linear model is visualized for each penguin or iris species.", echo = FALSE, out.width = '6in', fig.align='center', fig.pos='htbp'----

knitr::include_graphics('fig/linear_example.png')



## ---- simpsons, fig.cap = "Trends for penguin bill dimensions (bill length and bill depth, millimeters) if the ‘species’ variable is excluded (A) or included (B), illustrating Simpson’s Paradox. Note: linear regression for bill dimensions without including species in (A) is ill-advised; the linear trendline is only included to visualize trend reversal for Simpson’s Paradox when compared to (B).", echo = FALSE, out.width = '6in', fig.align='center', fig.pos='htbp'----

knitr::include_graphics('fig/simpson_gg.png')



## ---- pca, fig.cap = "Principal component analysis biplots and scree plots for structural size measurements in penguins (A,C) and iris (B,D), revealing similarities in multivariate patterns, variable loadings, and variance explained by each component. For penguins, variables are flipper length (mm), body mass (g), bill length (mm) and bill depth (mm); groups are visualized by species (Adélie = orange circles, chinstrap = blue triangles, gentoo = green squares). For iris, variables are petal length (cm), petal width (cm), sepal length (cm) and sepal width (cm); groups are visualized by species (setosa = light gray circles, versicolor = dark gray triangles, virginica = black squares). Values above scree plot columns (C,D) indicate percent of total variance explained by each of the four principal components.", echo = FALSE, out.width = '6in', fig.align='center', fig.pos='htbp'----

knitr::include_graphics('fig/pca_plots.png')



## ---- include = FALSE, echo = FALSE-------------------------------------------
# Including so kable table can be created here

# TWO VARIABLE k-means comparison
# Penguins: Bill length vs. bill depth
# Iris: petal length vs. petal width

pb_species <- penguins %>% 
  select(species, starts_with("bill")) %>% 
  drop_na()

pb_nospecies <- pb_species %>% 
  select(-species) %>% 
  recipe() %>% 
  step_normalize(all_numeric()) %>% 
  prep() %>% 
  juice()
  
# Perform k-means on penguin bill dimensions (k = 3, w/20 centroid starts)
# save augmented data
pb_clust <- 
  pb_nospecies %>% 
  kmeans(centers = 3, nstart = 20) %>% 
  broom::augment(pb_species)

# Plot clusters
pb_kmeans_gg <- 
  pb_clust %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_text(aes(label = .cluster, color = species),
            key_glyph = draw_key_rect) +
  scale_color_manual(values = c("darkorange","#0072b2","#009e73"))+
  theme(legend.position = "bottom",
         panel.border = element_rect(fill = NA, color = "gray70"))

# Get counts in each cluster by species
pb_clust_n <- pb_clust %>% 
  count(species, .cluster) %>% 
  pivot_wider(names_from = species, values_from = n, names_sort = TRUE) %>% 
  arrange(.cluster)

### Iris k-means

ip_species <- iris %>% 
  select(Species, starts_with("Petal"))

# remove species factor & scale petal dimensions: 
ip_nospecies <- ip_species %>% 
  select(-Species) %>% 
  recipe() %>% 
  step_normalize(all_numeric()) %>% 
  prep() %>% 
  juice()

# Perform k-means on iris petal dimensions (k = 3, w/20 centroid starts)
ip_clust <- 
  ip_nospecies %>% 
  kmeans(centers = 3, nstart = 20) %>% 
  broom::augment(ip_species)

# Plot clusters
ip_kmeans_gg <- 
  ip_clust %>% 
  ggplot(aes(x = Petal.Length, y = Petal.Width)) +
  geom_text(aes(label = .cluster, color = Species), 
            key_glyph = draw_key_rect) +
  theme(legend.position = "bottom",
         panel.border = element_rect(fill = NA, color = "gray70")) +
  scale_color_manual(values = c("gray70","gray50","black")) 

# Get counts in each cluster by species
ip_clust_n <- ip_clust %>% 
  count(Species, .cluster) %>% 
  pivot_wider(names_from = Species, values_from = n, names_sort = TRUE) %>% 
  arrange(.cluster)

# Combine kmeans plots for penguins & iris
(pb_kmeans_gg | ip_kmeans_gg) + plot_annotation(tag_levels = "A")

# ggsave(here("fig","kmeans.png"), width = 8, height = 4.5)

kmeans_2var_table <- cbind(pb_clust_n, ip_clust_n) %>% 
  kable(caption = "Grouped sample size for iris (by species; n = 150 total) and penguins (by species and sex; n = 344 total). Penguins can be further grouped by variables for island and study year.", align = "lclccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  column_spec(c(5:8), background = "gainsboro") %>% 
  add_header_above(c("Penguins cluster assignments" = 4, "Iris cluster assignments" = 4))


## ---- kmeans, fig.cap = "K-means clustering outcomes for penguin bill dimensions (A) and iris petal dimensions (B). Numbers indicate the cluster to which an observation was assigned, revealing a high degree of separation between species for both penguins and iris.", echo = FALSE, out.width = '6in', fig.align='center', fig.pos='htbp'----

knitr::include_graphics('fig/kmeans.png')


## ---- echo = FALSE------------------------------------------------------------
kmeans_2var_table <- cbind(pb_clust_n, ip_clust_n) %>% 
  kable(col.names = c("Cluster", "Adélie", "chinstrap", "gentoo", "Cluster", "setosa", "versicolor", "virginica"),
        caption = "K-means cluster assignments by species based on penguin bill length (mm) and depth (mm), and iris petal length (cm) and width (cm).", align = "cccccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  add_header_above(c("Penguins cluster assignments" = 4, "Iris cluster assignments" = 4))

kmeans_2var_table


## ---- Rlogo, echo=FALSE, fig.cap='The logo of R.', out.width='2in', fig.align='center', fig.pos='htbp'----
knitr::include_graphics('palmerpenguins-logo.png')


## -----------------------------------------------------------------------------
x <- 1:10
x

