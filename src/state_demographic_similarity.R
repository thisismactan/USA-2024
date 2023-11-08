source("src/library.R")
source("src/census-mining/api_key.R")

# You should have your own Census API key; either assign it to api_key in an R script at "src/census-mining/api_key.R"
# or directly set it in the working environment using Sys.setenv()
Sys.setenv(CENSUS_KEY = api_key)
Sys.getenv("CENSUS_KEY")

# For looking up variable codes
# acs_variables <- load_variables(2022, "acs1")
# acs_variables %>%
#   filter(concept == "Total Population")

varstoget <- c("population" = "B01003_001", "population_over_15" = "B12001_001", "population_over_15_male" = "B12001_002",
               "population_over_15_female" = "B12001_011", "population_over_15_male_married" = "B12001_004", 
               "population_over_15_female_married" = "B12001_013", "white" = "B03002_003", "black" = "B02001_003",
               "hispanic" = "B03002_012", "asian" = "B02001_005", "pacific" = "B02001_006", "imm" = "B05002_013",
               "educ_associate" = "B15003_021", "educ_bachelor" = "B15003_022", "educ_master" = "B15003_023",
               "educ_pro" = "B15003_024", "educ_doc" = "B15003_025", "median_hhinc" = "B19013_001", 
               "median_hhinc_white" = "B19013H_001", "poverty" = "B17001_002", "poverty_white" = "B17001H_001",
               "median_age" = "B01002_001")

state_demographics_2021 <- get_acs(geography = "state", variables = varstoget, year = 2021) %>%
  dplyr::select(-moe) %>%
  spread(variable, estimate)

cd_demographics_2021 <- get_acs(geography = "congressional district", variables = varstoget, year = 2021) %>%
  dplyr::select(-moe) %>%
  spread(variable, estimate) %>%
  filter(grepl("Nebraska|Maine", NAME)) %>%
  mutate(NAME = c("Maine's 1st congressional district",
                  "Maine's 2nd congressional district",
                  "Nebraska's 1st congressional district",
                  "Nebraska's 2nd congressional district",
                  "Nebraska's 3rd congressional district"))

state_area <- read_csv("data/auxiliary-demographics/state_land_area.csv", lazy = FALSE)
state_religion <- read_csv("data/auxiliary-demographics/state_religion_2020.csv", lazy = FALSE)

state_dem_margins <- read_csv("data/presidential_election_results_by_state.csv", lazy = FALSE) %>%
  filter(year %in% 2016:2020) %>%
  group_by(year, state) %>%
  mutate(two_party_pct = votes / sum(votes)) %>%
  ungroup() %>%
  dplyr::select(-candidate, -votes) %>%
  spread(party, two_party_pct) %>%
  mutate(margin = Democratic - Republican) %>%
  dplyr::select(-Democratic, -Republican) %>%
  spread(year, margin) %>%
  dplyr::select(NAME = state, dem_2020 = `2020`, dem_2016 = `2016`)

state_features <- state_demographics_2021 %>%
  bind_rows(cd_demographics_2021) %>%
  left_join(state_area, by = "NAME") %>%
  mutate(pop_density = population / land_area) %>%
  left_join(state_religion, by = c("NAME")) %>%
  left_join(state_dem_margins, by = "NAME") %>%
  mutate(pct_white = white / population,
         pct_black = black / population,
         pct_hispanic = hispanic / population,
         pct_aapi = (asian + pacific) / population,
         pct_college = (educ_associate + educ_bachelor + educ_doc + educ_master + educ_pro) / population,
         pct_married = (population_over_15_male_married + population_over_15_female_married) / population_over_15,
         pct_imm = imm / population,
         poverty_rate = poverty / population,
         white_poverty_rate = poverty_white / white) %>%
  filter(NAME != "Puerto Rico") %>%
  arrange(NAME)

# Compute principal components
state_pca <- prcomp(~ pct_white + pct_black + pct_hispanic + pct_aapi + pct_college + pct_married + poverty_rate +
                      white_poverty_rate + median_age + pop_density + pct_imm + pct_religious, 
                    data = state_features, scale = TRUE)

# Grab principal component scores and compute covariance matrix
state_pcs <- state_pca$x
state_eigenvalues <- state_pca$sdev^2
state_cor <- round(cov.wt(t(state_pcs), wt = state_eigenvalues, cor = TRUE)$cor, 6)
rownames(state_cor) <- colnames(state_cor) <- state_features$NAME

polling_error_sd <- 0.05

state_cov <- state_cor * (polling_error_sd^2)
