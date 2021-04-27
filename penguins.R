## ----overview-tbl-------------------------------------------------------------
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


## ----counts-tbl---------------------------------------------------------------
iris_counts <- iris %>% 
  count(Species) %>% 
  rename(`Iris species` = Species)

penguin_counts <- penguins %>% 
  mutate(species = as.character(species)) %>% 
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie", 
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



## ---- penguin-pairs, width = 6, height = 4.5, fig.cap = "Distributions and correlations for numeric variables in the penguins data (flipper length (mm), body mass (g), bill length (mm) and bill depth (mm)) for the three observed species: Gentoo (green, triangles); Chinstrap (blue, circles); and Adélie (orange, squares). Significance levels indicated for bivariate correlations are: *p < 0.05; **p < 0.01; ***p < 0.001."----


## ---- iris-pairs, width = 6, height = 4.5, fig.cap = "Distributions and correlations for numeric variables in the iris data (petal length (cm), petal width (cm), sepal length (cm) and sepal width (cm)) for the three included Iris species: setosa (light gray, circles); versicolor (dark gray, triangles); and virginica (black, squares). Significance levels indicated for bivariate correlations are: *p < 0.05; **p < 0.01; ***p < 0.001."----


## ---- linear-example, width = 6, height = 3, fig.cap = "Representative linear relationships for (A): penguin flipper length (mm) and body mass (g) for Adélie (orange circles), Chinstrap (blue triangles), and Gentoo (green squares) penguins; (B): iris petal length (cm) and width (cm) for setosa (light gray circles), versicolor (dark gray triangles) and virginica (black squares) irises. Within-species linear model is visualized for each penguin or iris species.", fig.pos='htbp'----


## ---- simpsons, width = 6, height = 2.5, fig.cap = "Trends for penguin bill dimensions (bill length and bill depth, millimeters) if the species variable is excluded (A) or included (B), illustrating Simpson’s Paradox. Note: linear regression for bill dimensions without including species in (A) is ill-advised; the linear trendline is only included to visualize trend reversal for Simpson’s Paradox when compared to (B).", fig.pos='htbp'----


## ---- pca, width = 8, height = 8, fig.cap = "Principal component analysis biplots and screeplots for structural size measurements in penguins (A,C) and iris (B,D), revealing similarities in multivariate patterns, variable loadings, and variance explained by each component. For penguins, variables are flipper length (mm), body mass (g), bill length (mm) and bill depth (mm); groups are visualized by species (Adélie = orange circles, Chinstrap = blue triangles, Gentoo = green squares). For iris, variables are petal length (cm), petal width (cm), sepal length (cm) and sepal width (cm); groups are visualized by species (setosa = light gray circles, versicolor = dark gray triangles, virginica = black squares). Values above screeplot columns (C,D) indicate percent of total variance explained by each of the four principal components.", fig.pos='htbp'----


## -----------------------------------------------------------------------------
# Included to create kable table of cluster assignment
# k-means comparison
# Penguins: Bill length vs. bill depth
# Iris: petal length vs. petal width

pb_species <- penguins %>% 
  select(species, starts_with("bill")) %>% 
  drop_na() %>% 
  mutate(species = as.character(species)) %>% 
  mutate(species = case_when(
    species == "Adelie" ~ "Adélie",
    TRUE ~ species
  )) %>% 
  mutate(species = as.factor(species))

pb_nospecies <- pb_species %>% 
  select(-species) %>% 
  recipe() %>% 
  step_normalize(all_numeric()) %>% 
  prep() %>% 
  juice()
  
# Perform k-means on penguin bill dimensions (k = 3, w/20 centroid starts)
# save augmented data
set.seed(100)
pb_clust <- 
  pb_nospecies %>% 
  kmeans(centers = 3, nstart = 20) %>% 
  broom::augment(pb_species)

# Get counts in each cluster by species
pb_clust_n <- pb_clust %>% 
  count(species, .cluster) %>% 
  pivot_wider(names_from = species, values_from = n, names_sort = TRUE) %>% 
  arrange(.cluster) %>% 
  replace_na(list(`Adélie` = 0)) 

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
set.seed(100)
ip_clust <- 
  ip_nospecies %>% 
  kmeans(centers = 3, nstart = 20) %>% 
  broom::augment(ip_species)

# Get counts in each cluster by species
ip_clust_n <- ip_clust %>% 
  count(Species, .cluster) %>% 
  pivot_wider(names_from = Species, values_from = n, names_sort = TRUE) %>% 
  arrange(.cluster) %>% 
  replace_na(list(`setosa` = 0, `versicolor` = 0, `virginica` = 0)) 

# Making cluster assignments table
kmeans_2var_table <- cbind(pb_clust_n, ip_clust_n) %>% 
  kable(col.names = c("Cluster", "Adélie", "Chinstrap", "Gentoo", "Cluster", "setosa", "versicolor", "virginica"),
        caption = "K-means cluster assignments by species based on penguin bill length (mm) and depth (mm), and iris petal length (cm) and width (cm).", align = "cccccc") %>% 
  kable_styling(full_width = FALSE) %>% 
  add_header_above(c("Penguins cluster assignments" = 4, "Iris cluster assignments" = 4))


## ---- kmeans, width = 8, height = 4.5, fig.cap = "K-means clustering outcomes for penguin bill dimensions (A) and iris petal dimensions (B). Numbers indicate the cluster to which an observation was assigned, revealing a high degree of separation between species for both penguins and iris.", fig.pos='htbp'----


## -----------------------------------------------------------------------------
kmeans_2var_table


## ---- echo = FALSE------------------------------------------------------------
overview_penguins_raw <- tribble(
  ~Feature, ~penguins_raw,
  "Year(s) collected", "2007 - 2009",
  "Dimensions (col x row)", "17 x 344",
  "Documentation", "complete metadata",
  "Variable classes", "character (9), Date (1), numeric (7)",
  "Missing values?", "yes (n = 336; 5.7%)"
)

overview_penguins_raw %>% 
  kable()


## ---- Rlogo, echo=FALSE, fig.cap='The logo of R.', out.width='2in', fig.align='center', fig.pos='htbp'----
knitr::include_graphics('palmerpenguins-logo.png')


## -----------------------------------------------------------------------------
x <- 1:10
x

