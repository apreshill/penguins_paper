## ---- penguin-pairs ---------------------------------------------------------
### Pairs plots for penguins and iris (with ggpairs)

# Penguin pairs plot:

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
  scale_color_paletteer_d("colorblindr::OkabeIto") +
  scale_fill_paletteer_d("colorblindr::OkabeIto") +
  scale_shape_manual(values = c(15,16,17)) +
  theme_minimal() +
  theme(
        text = element_text(size = 9),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "gray80", fill = NA)
        )
penguin_pairs

## ---- iris-pairs ---------------------------------------------------------

# Iris pairs plot:

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


## ---- linear-example ---------------------------------------------------------

# Penguin linear relationships (flipper length versus body mass):

penguin_flip_mass_scatter <-
  penguins %>%
  mutate(species = as.character(species)) %>%
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie",
    TRUE ~ species
  )) %>%
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species), size = 1.5) +
  geom_smooth(method = "lm", aes(group = species, color = species), se = FALSE, show.legend = FALSE) +
  theme_minimal() +
  scale_color_paletteer_d("colorblindr::OkabeIto") +
  theme(legend.position = c(0.2,0.85)) +
  labs(x = "Flipper length (mm)",
       y = "Body mass (g)") +
  theme(plot.title.position = "plot",
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray70"),
        legend.title = element_blank())

# To compare without penguin species as a variable (not in manuscript):
penguin_flip_mass_scatter_all <- ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(color = "gray50", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(x = "Flipper length (mm)",
       y = "Body mass (g)") +
  theme(plot.title.position = "plot",
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray70"))

# Iris linear relationships (petal dimensions):
iris_petal_scatter <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color = Species, size = Species, shape = Species), size = 1.5) +
  geom_smooth(aes(group = Species, color = Species), method = "lm", se = FALSE, show.legend = FALSE) +
  theme_minimal() +
  scale_color_manual(values = c("gray70","gray40","black")) +
  theme(legend.position = c(0.2, 0.85)) +
  labs(x = "Petal length (cm)",
       y = "Petal width (cm)") +
  theme(plot.title.position = "plot",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, color = "gray70"),
        legend.title = element_blank())

# To compare without iris species as a variable (not in manuscript):
iris_petal_scatter_all <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(color = "gray50", size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  labs(x = "Petal length (cm)",
       y = "Petal width (cm)") +
  theme(plot.title.position = "plot",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, color = "gray70"))

# Combine and save image:
linear_example <- (penguin_flip_mass_scatter + iris_petal_scatter) + plot_annotation(tag_levels = 'A')
linear_example

# ggsave(here("fig","linear_example.png"), width = 6, height = 3)


## ---- simpsons -------------------------------------------------------------------------------
# Simpson's Paradox example (bill dimensions, omitting species):
simpson_nospecies <- ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(color = "gray40", alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, color = "gray70")) +
  labs(x = "Bill length (mm)", y = "Bill depth (mm)")

# Bill dimensions, including species:
simpson_wspecies <-
  penguins %>%
  mutate(species = as.character(species)) %>%
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie",
    TRUE ~ species)
    ) %>%
    ggplot(aes(x = bill_length_mm, y = bill_depth_mm, group = species)) +
  geom_point(aes(color = species, shape = species), size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, aes(color = species), show.legend = FALSE) +
  scale_color_paletteer_d("colorblindr::OkabeIto") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, color = "gray70")) +
  labs(x = "Bill length (mm)", y = "Bill depth (mm)") +
  guides(color = guide_legend("Species"),
           shape = guide_legend("Species"))

# Combining into a compound figure:
simpson_gg <- (simpson_nospecies | simpson_wspecies) + plot_annotation(tag_levels = 'A')
simpson_gg
#ggsave(here("fig","simpson_gg.png"), width = 6, height = 2.5, dpi = 500)


## ---- pca ---------------------------------------------------------
# From Alison Hill's post for palmerpenguins pkgdown site

### PENGUINS PCA:

# Omit year
penguins_noyr <- penguins %>%
  select(-year) %>%
  mutate(species = as.character(species)) %>%
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie",
    TRUE ~ species
  )) %>%
  mutate(species = as.factor(species))

penguin_recipe <-
  recipe(~., data = penguins_noyr) %>%
  update_role(species, island, sex, new_role = "id") %>%
  step_naomit(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>%
  prep()

penguin_pca <-
  penguin_recipe %>%
  tidy(id = "pca")

penguin_percvar <- penguin_recipe %>%
  tidy(id = "pca", type = "variance") %>%
  dplyr::filter(terms == "percent variance")

# Make the penguins PCA biplot:

# Get pca loadings into wider format
pca_wider <- penguin_pca %>%
  tidyr::pivot_wider(names_from = component, id_cols = terms)

# define arrow style:
arrow_style <- arrow(length = unit(.05, "inches"),
                     type = "closed")

# Make the penguins PCA biplot:
pca_plot <-
  juice(penguin_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = species, shape = species),
             alpha = 0.7,
             size = 2) +
  scale_color_paletteer_d("colorblindr::OkabeIto") +
  guides(color = guide_legend("Species"),
        shape = guide_legend("Species"))

penguins_biplot <- pca_plot +
  geom_segment(data = pca_wider,
               aes(xend = PC1, yend = PC2),
               x = 0,
               y = 0,
               arrow = arrow_style) +
  geom_shadowtext(data = pca_wider,
            aes(x = PC1, y = PC2, label = terms),
            nudge_x = c(0.7,0.7,1.7,1.2),
            nudge_y = c(-0.1,-0.2,0.1,-0.1),
            size = 4,
            color = "black",
            bg.color = "white") +
  theme(legend.position = "bottom",
        panel.border = element_rect(color = "gray70", fill = NA))

# For positioning (above):
# 1: bill_length
# 2: bill_depth
# 3: flipper length
# 4: body mass

penguin_screeplot <- penguin_percvar %>%
  ggplot(aes(x = component, y = value)) +
  geom_col(fill = "gray50") +
  scale_x_continuous(limits = c(0, 5), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  ylab("% of total variance") +
  geom_text(aes(label = round(value,2)), vjust=-0.25) +
  theme(panel.border = element_rect(color = "gray70", fill = NA))

### IRIS PCA:

iris_recipe <-
  recipe(~., data = iris) %>%
  update_role(Species, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>%
  prep()
iris_pca <-
  iris_recipe %>%
  tidy(id = "pca")

### Iris PCA biplot:

# Get pca loadings into wider format
iris_wider <- iris_pca %>%
  tidyr::pivot_wider(names_from = component, id_cols = terms)

# define arrow style:
arrow_style <- arrow(length = unit(.05, "inches"),
                     type = "closed")

# Make the iris PCA biplot:
iris_pca_plot <-
  juice(iris_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = Species, shape = Species),
             alpha = 0.8,
             size = 2) +
  scale_colour_manual(values = c("gray70","gray40","black"))
iris_biplot <- iris_pca_plot +
  geom_segment(data = iris_wider,
               aes(xend = PC1, yend = PC2),
               x = 0,
               y = 0,
               arrow = arrow_style) +
  geom_shadowtext(data = iris_wider,
            aes(x = PC1, y = PC2, label = terms),
            nudge_x = c(0.5,0.3,1,1.2),
            nudge_y = c(-0.1,-0.2,0.1,-0.1),
            size = 4,
            color = "black",
            bg.color = "white") +
  theme(panel.background = element_rect(fill = NA, color = "gray70"),
        legend.position = "bottom")

iris_percvar <- iris_recipe %>%
  tidy(id = "pca", type = "variance") %>%
  dplyr::filter(terms == "percent variance")

# Iris screeplot:

iris_screeplot <- iris_percvar %>%
  ggplot(aes(x = component, y = value)) +
  geom_col(fill = "gray50") +
  scale_x_continuous(limits = c(0, 5), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  ylab("% of total variance") +
  geom_text(aes(label = round(value,2)), vjust=-0.25) +
  theme(panel.border = element_rect(color = "gray70", fill = NA))

# Combine biplots and screeplots for both iris & penguins:

(penguins_biplot | iris_biplot) / (penguin_screeplot | iris_screeplot) + plot_annotation(tag_levels = 'A')

#ggsave(here("fig","pca_plots.png"), width = 8, height = 8, dpi = 500)


## ---- kmeans ---------------------------------------------------------

# TWO VARIABLE k-means comparison
# Penguins: Bill length vs. bill depth
pb_species <- penguins %>%
  select(species, starts_with("bill")) %>%
  drop_na() %>%
  mutate(species = as.character(species)) %>%
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie",
    TRUE ~ species
  )) %>%
  mutate(species = as.factor(species))

# Prep penguins for k-means:
pb_nospecies <- pb_species %>%
  select(-species) %>%
  recipe() %>%
  step_normalize(all_numeric()) %>%
  prep() %>%
  juice()

# Perform k-means on penguin bill dimensions (k = 3, w/20 centroid starts)

# Save augmented data
set.seed(100)
pb_clust <-
  pb_nospecies %>%
  kmeans(centers = 3, nstart = 20) %>%
  broom::augment(pb_species)

# Plot penguin k-means clusters:
pb_kmeans_gg <-
  pb_clust %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_text(aes(label = .cluster, color = species),
            key_glyph = draw_key_rect) +
  scale_color_paletteer_d("colorblindr::OkabeIto") +
  theme(legend.position = "bottom",
         panel.border = element_rect(fill = NA, color = "gray70")) +
  labs(x = "Bill length (mm)",
       y = "Bill depth (mm)",
       color = "Species")

# Get counts in each cluster by species
pb_clust_n <- pb_clust %>%
  count(species, .cluster) %>%
  pivot_wider(names_from = species, values_from = n, names_sort = TRUE) %>%
  arrange(.cluster) %>%
  replace_na(list(`Adelie` = 0))

### Iris k-means
# Iris: petal length vs. petal width

# Select only petal dimensions:
ip_species <- iris %>%
  select(Species, starts_with("Petal"))

# Remove species factor & scale petal dimensions:
ip_nospecies <- ip_species %>%
  select(-Species) %>%
  recipe() %>%
  step_normalize(all_numeric()) %>%
  prep() %>%
  juice()

# Perform k-means on iris petal dimensions (k = 3, w/20 centroid starts):
set.seed(100)
ip_clust <-
  ip_nospecies %>%
  kmeans(centers = 3, nstart = 20) %>%
  broom::augment(ip_species)

# Plot iris k-means clusters:
ip_kmeans_gg <-
  ip_clust %>%
  ggplot(aes(x = Petal.Length, y = Petal.Width)) +
  geom_text(aes(label = .cluster, color = Species),
            key_glyph = draw_key_rect) +
  theme(legend.position = "bottom",
         panel.border = element_rect(fill = NA, color = "gray70")) +
  scale_color_manual(values = c("gray70","gray50","black"))  +
  labs(x = "Petal length (cm)",
       y = "Petal width (cm)")

# Get iris counts in each cluster by species:
ip_clust_n <- ip_clust %>%
  count(Species, .cluster) %>%
  pivot_wider(names_from = Species, values_from = n, names_sort = TRUE) %>%
  arrange(.cluster) %>%
  replace_na(list(`setosa` = 0, `versicolor` = 0, `virginica` = 0))

# Combine k-means plots for penguins & iris:
(pb_kmeans_gg | ip_kmeans_gg) + plot_annotation(tag_levels = "A")

# Save image:
# ggsave(here("fig","kmeans.png"), width = 8, height = 4.5, dpi = 500)

