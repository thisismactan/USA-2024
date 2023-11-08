source("src/library.R")

regions <- read_csv("data/state_divisions.csv", lazy = FALSE)

historical_results <- read_csv("data/presidential_election_results_by_state.csv", lazy = FALSE) %>%
  left_join(read_csv("data/state_total_votes_by_year.csv", lazy = FALSE), by = c("year", "state")) %>%
  group_by(year, state) %>%
  mutate(pct = votes / total_votes,
         two_party_pct = votes / sum(votes)) %>%
  ungroup() %>%
  dplyr::select(-candidate) %>%
  mutate(national_winner = case_when(year %in% 1992:1996 ~ "Bill Clinton",
                                     year %in% 2000:2004 ~ "George W. Bush",
                                     year %in% 2008:2012 ~ "Barack Obama",
                                     year == 2016 ~ "Donald Trump",
                                     year == 2020 ~ "Joe Biden"))

third_party_performance <- historical_results %>%
  group_by(year, state, total_votes) %>%
  summarise(votes_3p = mean(total_votes) - sum(votes)) %>%
  ungroup() %>%
  mutate(pct_3p = votes_3p / total_votes) %>%
  group_by(year) %>%
  mutate(natl_pct_3p = sum(votes_3p * !grepl("congressional", state)) / sum(total_votes * !grepl("congressional", state))) %>%
  ungroup() %>%
  mutate(ratio_3p = pct_3p / natl_pct_3p)

third_party_ratio <- third_party_performance %>%
  filter(!(state == "Texas" & year < 2000),
         !(state == "New Mexico" & year %in% 2012:2016),
         !(state == "Utah" & year == 2016)) %>%
  group_by(state) %>%
  summarise(min_3p = min(ratio_3p), 
            avg_3p = mean(ratio_3p), 
            max_3p = max(ratio_3p))

third_party_propensity_model <- lmer(log_ratio_3p ~ natl_pct_3p + (1+natl_pct_3p|state), 
                                     data = third_party_performance %>% 
                                       filter(ratio_3p > 0) %>%
                                       mutate(log_ratio_3p = log(ratio_3p)), REML = FALSE)

third_party_propensity_coefficients <- coefficients(third_party_propensity_model)$state %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(state = rownames(coefficients(third_party_propensity_model)$state)) %>%
  as_tibble()

national_historical_results <- historical_results %>%
  filter(!grepl("congressional", state)) %>%
  group_by(year, party) %>%
  summarise(national_votes = sum(votes)) %>%
  mutate(two_party_pct_natl = national_votes / sum(national_votes)) %>%
  dplyr::select(-national_votes) %>%
  spread(party, two_party_pct_natl) %>%
  mutate(two_party_margin_natl = Democratic - Republican) %>%
  dplyr::select(year, two_party_margin_natl) %>%
  ungroup()

two_party_margin_2020 <- national_historical_results %>%
  filter(year == 2020) %>%
  pull(two_party_margin_natl)

state_two_party_margins_2020 <- historical_results %>%
  filter(year == 2020) %>%
  dplyr::select(-year, -votes, -national_winner) %>%
  spread(party, two_party_pct) %>%
  mutate(state_two_party_margin = Democratic - Republican) %>%
  dplyr::select(-Democratic, -Republican)

# Shape to changes
incumbent_running_results <- historical_results %>%
  dplyr::select(-votes) %>%
  spread(party, two_party_pct) %>%
  arrange(national_winner, state, year) %>%
  mutate(two_party_margin = Democratic - Republican) %>%
  left_join(national_historical_results, by = "year") %>%
  group_by(national_winner, state) %>%
  mutate(last_two_party_margin = lag(two_party_margin),
         two_party_margin_natl_change = two_party_margin_natl - lag(two_party_margin_natl)) %>%
  na.omit() %>%
  left_join(regions, by = "state")

# Linear regression
two_party_margin_change_model <- lm(I(two_party_margin - last_two_party_margin) ~ two_party_margin_natl_change, data = incumbent_running_results)
summary(two_party_margin_change_model)

# With regions
two_party_margin_change_model_regions <- lmer(I(two_party_margin - last_two_party_margin) ~ two_party_margin_natl_change + (1|region), 
                                              data = incumbent_running_results)
fixed_effect_coefficients <- as.matrix(coefficients(two_party_margin_change_model_regions)$region) %>%
  apply(MARGIN = 2, FUN = mean)

# Variance components
regional_sd <- sqrt(as.numeric(VarCorr(two_party_margin_change_model_regions)))
residual_sd <- attributes(VarCorr(two_party_margin_change_model_regions))$sc

# ggplot(incumbent_running_results, aes(x = last_two_party_margin, y = two_party_margin, col = factor(year))) +
#   facet_wrap(~year) +
#   geom_abline(slope = 1, intercept = 0, col = "#888888", size = 1, linetype = 2) +
#   geom_smooth(method = "lm", show.legend = FALSE, se = FALSE) +
#   geom_text(aes(label = abbrev, size = ), size = 3, show.legend = FALSE) +
#   scale_colour_manual(name = "Year", values = c("1996" = "red", "2004" = "green4", "2012" = "blue")) +
#   scale_x_continuous(labels = scales::percent) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
#   labs(title = "State-level presidential election results vs. previous year's results",
#        x = "Two-party Democratic margin in last election", y = "Two-party Democratic margin")
