
library(readxl)
library(tidyverse)

# download data from Bay Adapt Github
# url <- "https://github.com/BCDC-GIS/Bay-Adapt-Currents/blob/e8e4ec0ad1ee6888070975a138a3d7d075c1a40f/Sea%20Level%20Rise%20in%20General%20Plans/Cities.xlsx?raw=true"

# download.file(url, destfile = "data/Cities_Simplified.xlsx")

cities_data <- read_excel("data/Cities_Simplified.xlsx")[,1:41]

colnames(cities_data) <- c('city',
                           'county', 
                           'doc_type',
                           'year',
                           'element',
                           'policy',
                           'program',
                           'text',
                           'aff_housing',
                           'beach',
                           'building_code',
                           'community_engage',
                           'contamination',
                           'coordination',
                           'cult_hist_pres',
                           'development',
                           'econ_jobs',
                           'education_outreach',
                           'emergency',
                           'erosion',
                           'expenditures',
                           'funding',
                           'planning',
                           'habitat_env_qual',
                           'infrastructure',
                           'managed_retreat',
                           'mapping',
                           'levees_seawall',
                           'nature_based',
                           'open_space',
                           'public_health',
                           'research_monitor',
                           'training',
                           'transport_mobility',
                           'utilities',
                           'vulnerable_populations',
                           'water_quality',
                           'wetlands',
                           'flood_stormwater',
                           'groundwater',
                           'slr_mentioned'
) 

colnames(cities_data) <- c(
  'city', 'county', 'doc_type', 'year', 'element', 'policy', 'program', 'text',
  'aff_housing', 'beach', 'building_code', 'community_engage', 'contamination',
  'coordination', 'cult_hist_pres', 'development', 'econ_jobs', 'education_outreach',
  'emergency', 'erosion', 'expenditures', 'funding', 'planning', 'habitat_env_qual',
  'infrastructure', 'managed_retreat', 'mapping', 'levees_seawall', 'nature_based',
  'open_space', 'public_health', 'research_monitor', 'training', 'transport_mobility',
  'utilities', 'vulnerable_populations', 'water_quality', 'wetlands',
  'flood_stormwater', 'groundwater', 'slr_mentioned'
)

# --- 2. AGGREGATION (New Code) ---

# come back to how these correlate with one another via cluster analysis, etc

#policy and program differences need to be looked at as well

# check for city completeness

# 1. Built Environment
vars_built <- c('infrastructure', 'transport_mobility', 'utilities', 
                'building_code', 'development', 'levees_seawall', 'flood_stormwater')

# 2. Natural Resources
vars_nature <- c('wetlands', 'nature_based', 'habitat_env_qual', 
                 'open_space', 'beach', 'erosion', 'water_quality', 
                 'groundwater', 'contamination')

# 3. Social Equity
vars_social <- c('vulnerable_populations', 'aff_housing', 'public_health', 
                 'cult_hist_pres', 'econ_jobs', 'community_engage', 'education_outreach')

# 4. Governance & Process
vars_gov <- c('planning', 'coordination', 'funding', 'expenditures', 
              'research_monitor', 'mapping', 'training', 'emergency', 'managed_retreat')

cities_clean <- cities_data |>
  mutate(across(aff_housing:slr_mentioned, 
                ~ case_when(. == "1" ~ 1, TRUE ~ 0))) |>
  mutate(year = as.numeric(as.character(year))) |>
  rowwise() |>
  mutate(
    sys_built   = sum(c_across(all_of(vars_built))),
    sys_nature  = sum(c_across(all_of(vars_nature))),
    sys_social  = sum(c_across(all_of(vars_social))),
    sys_gov     = sum(c_across(all_of(vars_gov))),
  ) |>
  ungroup() |>
  mutate(across(aff_housing:slr_mentioned, as.factor)) |>
  mutate(across(city:element, as.factor)) |> 
  select(city, element, sys_built, sys_nature, sys_social, sys_gov, year, text, everything())

double_counts <- cities_clean |> 
  filter(sys_built > 1 | sys_nature > 1 | sys_social > 1 | sys_gov > 1) |> 
  select(city, text, sys_built, sys_nature, sys_social, sys_gov)

#removing distinctions between elements, eyar, and text
cities_agg <- cities_clean |>
  group_by(city) |>
  summarise(
    sys_built = sum(sys_built),
    sys_nature = sum(sys_nature),
    sys_social = sum(sys_social),
    sys_gov = sum(sys_gov),
  ) 

# |>
#   mutate(
#     sys_built_b = ifelse(sys_built > 0, 1, 0),
#     sys_nature_b = ifelse(sys_nature > 0, 1, 0),
#     sys_social_b = ifelse(sys_social > 0, 1, 0),
#     sys_gov_b = ifelse(sys_gov > 0, 1, 0)
  # )

# --- 3. VISUALIZATION ---


# 1. Convert to Long Format
plot_data_long <- cities_agg |>
  pivot_longer(
    cols = c(sys_built, sys_nature, sys_social, sys_gov),
    names_to = "System_Type",
    values_to = "Count"
  ) |>
  # Clean up the names for the legend
  mutate(System_Type = recode(System_Type,
                              "sys_built" = "Built Environment",
                              "sys_nature" = "Nature-Based",
                              "sys_social" = "Social Equity",
                              "sys_gov" = "Governance"))

# 2. Calculate Total Volume per City (for sorting)
city_totals <- plot_data_long |>
  group_by(city) |>
  summarise(Total_Policies = sum(Count)) |>
  arrange(desc(Total_Policies))

# --- VISUALIZATION 1: The "System Dominance" Boxplot ---
# Question: Across all cities, which systems are getting the most attention?

ggplot(plot_data_long, aes(x = System_Type, y = Count, fill = System_Type)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) + # Adds dots for individual cities
  theme_minimal() +
  labs(
    title = "Distribution of Policy Focus Across Bay Area Cities",
    subtitle = "Points represent individual cities; Boxes show the regional median",
    x = "",
    y = "Number of Policy Mentions"
  ) +
  theme(legend.position = "none") +
  coord_flip() # Flips sideways for easier reading


# --- VISUALIZATION 2: The "Strategic Mix" (Top 20 Cities) ---
# Question: Do the most active cities prioritize different things?
# We filter to the top 20 cities by volume to keep the chart readable.

top_20_cities <- city_totals$city[1:20]

plot_data_long |>
  filter(city %in% top_20_cities) |>
  ggplot(aes(x = reorder(city, Count), y = Count, fill = System_Type)) +
  geom_col(position = "fill") + # "fill" makes it a 100% stacked bar
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Policy Composition of Top 20 Most Active Cities",
    subtitle = "Relative proportion of policy types (normalized to 100%)",
    x = "",
    y = "Proportion of Total Policies",
    fill = "System Category"
  ) +
  scale_fill_brewer(palette = "Spectral")


# --- VISUALIZATION 3: The "Nature vs. Concrete" Scatter Plot ---
# Question: Are cities that build 'hard' infrastructure also investing in 'nature'?
# Or is it an either/or trade-off?

ggplot(cities_agg, aes(x = log(sys_built), y = log(sys_nature))) +
  geom_point(aes(size = log(sys_social), color = log(sys_gov)), alpha = 0.8) +
  geom_text(aes(label = as.character(city)),
            vjust = 2, 
            size = 2, 
            position='jitter',
            check_overlap = TRUE) +
  theme_light() +
  scale_color_viridis_c(option = "magma") +
  labs(
    title = "Typologies: Hard Infrastructure vs. Nature-Based Solutions",
    subtitle = "Size = Social Equity Focus | Color = Governance Intensity",
    x = "Built Environment Score (log Count)",
    y = "Nature-Based Score (log Count)",
    color = "Gov Score",
    size = "Social Score"
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") # Line of equality

