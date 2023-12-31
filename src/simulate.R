source("src/poll_averages.R")
source("src/state_partisan_priors.R")
if(!exists("state_cov")) {
  source("src/state_demographic_similarity.R")
}

set.seed(2024)

n_sims <- 5e4

# Simulated national popular vote based on national polling
poll_pcts <- rmvn(n_sims, mu = president_poll_covariance_3p$center, 
                  president_poll_covariance_3p$cov + diag(c(0.035^2, 0.035^2, 0.01^2)))
colnames(poll_pcts) <- c("biden", "trump", "kennedy")
undecided_pct <- 1 - rowSums(poll_pcts)

# Undecided split
undecided_candidate_frac <- rdirichlet(n_sims, c(9, 9, 3))
undecided_distribution <- undecided_candidate_frac * undecided_pct

national_popular_vote_sims <- (poll_pcts + undecided_distribution) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(sim_id = 1:n()) %>%
  dplyr::select(sim_id, biden, trump, kennedy) %>%
  mutate(national_two_party_margin = (biden - trump) / (biden + trump))

national_popular_vote_sims %>%
  as.data.frame() %>%
  mutate(biden_win = (biden > trump) & (biden > kennedy),
         trump_win = (trump > biden) & (trump > kennedy),
         kennedy_win = (kennedy > biden) & (kennedy > trump)) %>%
  summarise_at(vars(contains("_win")), mean)

# Regional deviations
region_names <- unique(regions$region)
regional_deviations <- expand.grid(region = region_names,
                                   sim_id = 1:n_sims) %>%
  as.tbl() %>%
  mutate(region_deviation = rnorm(n(), 0, regional_sd))

# Residual_deviations
state_names <- regions %>%
  pull(state) %>%
  unique()

state_deviations <- expand.grid(state = state_names,
                                sim_id = 1:n_sims) %>%
  as.tbl() %>%
  mutate(state_deviation = rnorm(n(), 0, residual_sd))

# Joining onto national results
total_deviations <- regions %>%
  dplyr::select(state, region, electoral_votes) %>%
  left_join(regional_deviations, by = "region") %>%
  left_join(state_deviations, by = c("state", "sim_id")) %>%
  arrange(sim_id, state) %>%
  na.locf() %>%
  mutate(total_deviation = region_deviation + state_deviation) 

# Simulate states' propensities to vote third party
state_priors <- total_deviations %>%
  left_join(national_popular_vote_sims, by = "sim_id") %>%
  left_join(state_two_party_margins_2020, by = "state") %>%
  mutate(two_party_margin_natl_change = national_two_party_margin - two_party_margin_2020,
         prior_predicted_two_party_margin = state_two_party_margin + fixed_effect_coefficients["(Intercept)"] + 
           two_party_margin_natl_change * fixed_effect_coefficients["two_party_margin_natl_change"] + total_deviation,
         prior_predicted_two_party_margin = pmax(prior_predicted_two_party_margin, -1),
         prior_predicted_two_party_margin = pmin(prior_predicted_two_party_margin, 1)) %>%
  dplyr::rename(kennedy_natl = kennedy) %>%
  left_join(third_party_propensity_coefficients, by = "state") %>%
  mutate(log_ratio_3p = `(Intercept)` + pmin(natl_pct_3p, 0) * kennedy_natl + rnorm(n(), 0, summary(third_party_propensity_model)$sigma),
         kennedy = exp(log_ratio_3p) * kennedy_natl,
         biden = (1 - kennedy) / 2 + prior_predicted_two_party_margin / 2,
         trump = (1 - kennedy) / 2 - prior_predicted_two_party_margin / 2) %>%
  
  # Floor major-party candidates to 5% and then normalize so everything sums to 1
  mutate(biden = pmax(biden, 0.05),
         trump = pmax(trump, 0.05),
         total_pct = biden + trump + kennedy,
         biden = biden / total_pct,
         trump = trump / total_pct,
         kennedy = kennedy / total_pct) %>%
  dplyr::select(sim_id, state, region, electoral_votes, total_deviation, biden, trump, kennedy) %>%
  melt(measure.vars = c("biden", "trump", "kennedy"), variable.name = "candidate", value.name = "prior_pct") %>%
  as_tibble()

# State prior probabilities
state_prior_probabilities <- state_priors %>%
  group_by(sim_id, state) %>%
  filter(prior_pct == max(prior_pct)) %>%
  group_by(state, candidate) %>%
  summarise(prob = n() / n_sims) %>%
  spread(candidate, prob, fill = 0)

# State prior mean and variance
state_prior_summary_stats <- state_priors %>%
  group_by(candidate, state) %>%
  summarise(prior_mean = mean(prior_pct),
            prior_var = var(prior_pct)) %>%
  mutate(state = case_when(grepl("'s 1st congressional district", state) ~ gsub("'s 1st congressional district", " CD-1", state),
                           grepl("'s 2nd congressional district", state) ~ gsub("'s 2nd congressional district", " CD-2", state),
                           grepl("'s 3rd congressional district", state) ~ gsub("'s 3rd congressional district", " CD-3", state),
                           !grepl("congress", state) ~ state)) %>%
  ungroup()

# Updating with state-level polling
state_poll_averages_today <- president_averages %>%
  filter(median_date == today(), state != "National")

polling_variation <- state_poll_averages_today %>%
  right_join(state_prior_summary_stats, by = c("state", "candidate")) %>%
  mutate(polled = !is.na(var),
         var = replace_na_zero(var) / eff_n) %>%
  dplyr::select(state, candidate, polled, poll_avg = avg, poll_var = var) %>%
  ungroup()

state_cov_adj <- state_cov + (polling_variation %>%
  filter(candidate != "kennedy") %>%
  group_by(state) %>% 
  summarise(poll_var = sum(poll_var)) %>%
  arrange(state) %>%
  mutate(poll_var = replace_na_zero(poll_var)) %>%
  pull(poll_var) %>%
  diag())

while(!is.positive.definite(state_cov_adj)) {
  diag(state_cov_adj) <- diag(state_cov_adj) + 1e-6
}

# Weights for average
poll_weights <- polling_variation %>%
  dplyr::select(-poll_var) %>%
  spread(candidate, poll_avg) %>%
  mutate(poll_weight = polled / diag(state_cov_adj)) %>%
  dplyr::select(state, poll_weight)

prior_weights <- state_prior_summary_stats %>%
  dplyr::select(-prior_mean) %>%
  spread(candidate, prior_var) %>%
  mutate(prior_weight = 2 / (biden + trump)) %>%
  dplyr::select(state, prior_weight)

pres_simulation_weights <- prior_weights %>%
  left_join(poll_weights, by = "state") %>%
  mutate(poll_weight = case_when(is.na(poll_weight) ~ 0,
                                 !is.na(poll_weight) ~ poll_weight),
         weight_sum = prior_weight + poll_weight,
         poll_weight = poll_weight / weight_sum,
         prior_weight = prior_weight / weight_sum,
         state = case_when(grepl("CD-1", state) ~ gsub(" CD-1", "'s 1st congressional district", state),
                           grepl("CD-2", state) ~ gsub(" CD-2", "'s 2nd congressional district", state),
                           grepl("CD-3", state) ~ gsub(" CD-3", "'s 3rd congressional district", state),
                           !grepl("CD-", state) ~ state)) %>%
  dplyr::select(state, prior_weight, poll_weight)

# Simulate state poll distribution draws
state_polling_error_sims <- rmvn(n_sims, mu = rep(0, nrow(state_cov_adj)), sigma = state_cov_adj) %>%
  as.data.frame()
names(state_polling_error_sims) <- colnames(state_cov_adj)

pres_state_sims <- state_polling_error_sims %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "state", value.name = "error") %>%
  left_join(state_poll_averages_today %>% dplyr::select(state, candidate, avg) %>% spread(candidate, avg), by = "state") %>%
  mutate(biden_poll = biden + (error * (1 - kennedy)) / 2,
         trump_poll = trump - (error * (1 - kennedy)) / 2) %>%
  left_join(polling_variation %>% filter(candidate == "kennedy", polled) %>% dplyr::select(state, kennedy_var = poll_var), 
            by = "state") %>%
  mutate(kennedy_error = rnorm(n(), 0, sqrt(kennedy_var)),
         kennedy_poll = kennedy + kennedy_error,
         state = case_when(grepl("CD-1", state) ~ gsub(" CD-1", "'s 1st congressional district", state),
                           grepl("CD-2", state) ~ gsub(" CD-2", "'s 2nd congressional district", state),
                           grepl("CD-3", state) ~ gsub(" CD-3", "'s 3rd congressional district", state),
                           !grepl("CD-", state) ~ state)) %>%
  dplyr::select(sim_id, state, biden, trump, kennedy) %>%
  melt(id.vars = c("sim_id", "state"), variable.name = "candidate", value.name = "poll") %>%
  right_join(state_priors %>% dplyr::select(sim_id, state, electoral_votes, candidate, prior = prior_pct), 
             by = c("sim_id", "state", "candidate")) %>%
  mutate(poll = replace_na_zero(poll)) %>%
  left_join(pres_simulation_weights, by = "state") %>%
  left_join(undecided_candidate_frac %>%
              as.data.frame() %>%
              mutate(sim_id = 1:n_sims) %>%
              dplyr::select(sim_id, biden = V1, trump = V2, kennedy = V3) %>%
              melt(id.vars = "sim_id", variable.name = "candidate", value.name = "undecided_frac"), 
            by = c("sim_id", "candidate")) %>%
  mutate(pct = prior * prior_weight + poll * poll_weight) %>%
  group_by(sim_id, state) %>%
  mutate(undecided = 1 - sum(pct)) %>%
  ungroup() %>%
  mutate(pct = pct + undecided_frac * undecided) %>%
  dplyr::select(sim_id, state, electoral_votes, candidate, pct) %>%
  as.tbl()

# Probabilities
pres_win_probabilities <- pres_state_sims %>%
  group_by(sim_id, state) %>%
  filter(pct == max(pct)) %>%
  group_by(sim_id, candidate) %>%
  summarise(electoral_votes = sum(electoral_votes)) %>%
  spread(candidate, electoral_votes, fill = 0) %>%
  mutate(candidate = case_when(biden >= 270 ~ "biden",
                               trump >= 270 ~ "trump",
                               kennedy >= 270 ~ "kennedy",
                               TRUE ~ "none")) %>%
  group_by(state = "National", candidate) %>%
  summarise(prob = n() / n_sims)

pres_state_probabilities <- pres_state_sims %>%
  group_by(sim_id, state) %>%
  filter(pct == max(pct)) %>%
  group_by(state, candidate) %>%
  summarise(prob = n() / n_sims) %>%
  spread(candidate, prob, fill = 0)

presidential_forecast_probabilities_today <- pres_state_probabilities %>%
  melt(id.vars = "state", variable.name = "candidate", value.name = "prob") %>% 
  bind_rows(pres_win_probabilities) %>%
  arrange(state, candidate) %>%
  mutate(date = today()) %>%
  dplyr::select(date, state, candidate, prob)

# Write this to an output file
if(!("presidential_forecast_probabilities_history.csv" %in% list.files("output"))) {
  write_csv(presidential_forecast_probabilities_today, "output/presidential_forecast_probabilities_history.csv")
}

presidential_forecast_probabilities_history <- read_csv("output/presidential_forecast_probabilities_history.csv") %>%
  bind_rows(presidential_forecast_probabilities_today) %>%
  group_by(date, state, candidate) %>%
  dplyr::slice(n()) %>%
  ungroup()

write_csv(presidential_forecast_probabilities_history, "output/presidential_forecast_probabilities_history.csv")

# Cleanup
rm(list = c("regional_deviations", "state_deviations", "state_polling_error_sims", "total_deviations", "biden_undecided_frac",
            "biden_undecided_pct", "trump_undecided_frac", "trump_undecided_pct", "undecided_pct"))

# For the House ####
pres_generic_ballot_data <- national_president_polls_adj %>%
  dplyr::select(poll_id, question_id, candidate, pct) %>%
  spread(candidate, pct) %>%
  mutate(pres_margin = biden - trump) %>%
  dplyr::select(poll_id, question_id, pres_margin) %>%
  inner_join(generic_ballot_polls_adj %>%
              dplyr::select(poll_id, question_id, loess_weight, candidate, pct) %>%
              spread(candidate, pct) %>%
              mutate(house_margin = dem - rep) %>%
              dplyr::select(poll_id, loess_weight, house_margin),
            by = "poll_id")

pres_generic_ballot_lm <- lm(house_margin ~ pres_margin, data = pres_generic_ballot_data, weight = loess_weight)
generic_ballot_sigma <- sqrt(sum(pres_generic_ballot_lm$residuals^2) / pres_generic_ballot_lm$df.residual)
pres_generic_ballot_r2 <- summary(pres_generic_ballot_lm)$r.squared

# House popular vote sims: predict national House vote from presidential sims and add appropriate noise to ensure correlation
# matches generic ballot
house_two_party_sims <- national_popular_vote_sims %>%
  dplyr::select(sim_id, pres_margin = national_two_party_margin) %>%
  mutate(expected_house_two_party_margin = predict(pres_generic_ballot_lm, newdata = .),
         dev = rnorm(n(), 0, generic_ballot_sigma),
         house_two_party_margin = expected_house_two_party_margin + dev)

simulated_house_r2 <- var(house_two_party_sims$expected_house_two_party_margin) / var(house_two_party_sims$house_two_party_margin)
r2_ratio <- simulated_house_r2 / pres_generic_ballot_r2

house_two_party_sims <- national_popular_vote_sims %>%
  dplyr::select(sim_id, pres_margin = national_two_party_margin) %>%
  mutate(house_two_party_margin = predict(pres_generic_ballot_lm, newdata = .) + rnorm(n(), 0, r2_ratio * generic_ballot_sigma))

# Center and rescale so that mean and standard deviation match two-party generic ballot
generic_ballot_2party_avg <- generic_ballot_averages_adj %>%
  filter(median_date == today())

house_margin_mean <- -diff(generic_ballot_2party_avg$avg)
house_margin_var <- sum(generic_ballot_2party_avg$var) + abs(2 * generic_ballot_poll_covariance$cov[1, 2])
house_margin_eff_n <- mean(generic_ballot_2party_avg$eff_n)
house_margin_scale_ratio <- sqrt(house_margin_var) / sd(house_two_party_sims$house_two_party_margin)

house_two_party_sims <- national_popular_vote_sims %>%
  dplyr::select(sim_id, pres_margin = national_two_party_margin) %>%
  mutate(house_two_party_margin = predict(pres_generic_ballot_lm, newdata = .) + rnorm(n(), 0, r2_ratio * generic_ballot_sigma)) %>%
  mutate(house_margin_rescaled = house_two_party_margin * house_margin_scale_ratio,
         house_margin_rescaled = house_margin_rescaled - mean(house_margin_rescaled) + house_margin_mean + rnorm(n(), 0, 0.06) +
           rnorm(n(), 0, sqrt(house_margin_var / house_margin_eff_n))) %>%
  dplyr::select(sim_id, natl_margin = house_margin_rescaled) 


# Fit House model if it doesn't exist yet
if(!exists("house_lm")) {
  source("src/house_modeling.R")
}

# Shape House dataset
house_2024_data <- read_csv("data/house_candidates.csv", na = character(), lazy = FALSE) %>%
  filter(candidate_firstname != "None") %>%
  dplyr::select(state, seat_number, district_abbr, incumbent_running, candidate_party) %>%
  group_by(state, seat_number) %>%
  mutate(dem_running = any(candidate_party == "DEM"),
         rep_running = any(candidate_party == "REP")) %>%
  dplyr::select(-candidate_party) %>%
  distinct() %>%
  left_join(house_results_2party %>%
              filter(year == 2022) %>%
              mutate(margin = DEM - REP) %>%
              dplyr::select(state, seat_number, region, incumbent_running_last = incumbent_running, 
                            democrat_running_last = democrat_running, republican_running_last = republican_running, 
                            last_margin = margin),
            by = c("state", "seat_number")) %>%
  mutate(incumbent_running_last = ifelse(grepl("redistricting", incumbent_running_last), "None", incumbent_running_last),
         incumbency_change = paste(incumbent_running_last, incumbent_running, sep = " to "),
         last_natl_margin = national_house_results %>% filter(year == 2022) %>% pull(natl_margin)) %>%
  # left_join(read_csv("data/nc_redistricted.csv") %>% dplyr::select(state, seat_number, lean), by = c("state", "seat_number")) %>%
  left_join(read_csv("data/presidential_results_by_2022_cd.csv") %>%
              mutate(pres_2party_2020 = (biden_2020 - trump_2020) / (biden_2020 + trump_2020)), 
            by = c("state", "seat_number")) %>%
  ungroup() %>%
  dplyr::select(state, seat_number, district_abbr, region, incumbency_change, dem_running, rep_running, last_margin, last_natl_margin)

# Generate district simulations
if(exists("house_district_sims")) {
  rm(house_district_sims)
}
gc()

house_n_sims <- n_sims

## State and regional-level deviations
house_region_deviations <- regions %>%
  dplyr::select(region) %>%
  distinct() %>%
  dplyr::slice(rep(1:n(), each = house_n_sims)) %>%
  mutate(sim_id = rep(1:house_n_sims, 10),
         region_dev = rnorm(n(), 0, region_sd))

# house_state_deviations <- regions %>%
#   dplyr::select(state) %>%
#   dplyr::slice(rep(1:n(), each = house_n_sims)) %>%
#   mutate(sim_id = rep(1:house_n_sims, 56),
#          state_dev = rnorm(n(), 0, state_sd))

## District-level polling simulations
n_districts_polled <- nrow(district_averages)

district_poll_sims <- district_averages %>%
  dplyr::slice(rep(1:n(), each = house_n_sims)) %>%
  mutate(sim_id = rep(1:house_n_sims, n_districts_polled),
         poll_margin = poll_margin + rnorm(n(), 0, sqrt(poll_var))) %>%
  dplyr::select(-poll_var)

house_district_sims <- house_2024_data %>%
  dplyr::slice(rep(1:n(), each = house_n_sims)) %>%
  mutate(sim_id = rep(1:house_n_sims, 435)) %>%
  left_join(house_two_party_sims, by = "sim_id") %>%
  left_join(house_region_deviations, by = c("region", "sim_id")) %>%
  mutate(sim_margin = predict(house_lmer, newdata = .) + region_dev + rnorm(n(), 0.0, residual_sd),
         sim_margin = case_when(!dem_running ~ -1,
                                !rep_running ~ 1,
                                dem_running & rep_running ~ sim_margin)) %>%
  group_by(state, seat_number) %>%
  mutate(prior_variance = var(sim_margin)) %>%
  ungroup() %>%
  mutate(prior_weight = 1 / prior_variance) %>%
  dplyr::select(sim_id, state, seat_number, prior_margin = sim_margin, prior_weight) %>%
  left_join(district_poll_sims, by = c("state", "seat_number", "sim_id")) %>%
  mutate(poll_margin = ifelse(is.na(poll_margin), 0, poll_margin),
         poll_weight = ifelse(is.na(poll_weight), 0, poll_weight),
         prior_weight = ifelse(prior_weight == Inf, 99999, prior_weight),
         margin = (prior_margin * prior_weight + poll_margin * poll_weight) / (prior_weight + poll_weight)) %>%
  dplyr::select(sim_id, state, seat_number, margin)

## Timeline for the House forecast
house_forecast_probability_today <- house_district_sims %>%
  mutate(party = ifelse(margin > 0, "Democrats", "Republicans")) %>%
  group_by(sim_id, party) %>%
  summarise(seats = n()) %>%
  group_by(sim_id) %>%
  filter(seats == max(seats)) %>%
  group_by(party) %>%
  summarise(prob = n() / house_n_sims) %>%
  mutate(date = today()) %>%
  dplyr::select(date, party, prob)

house_states_won <- house_district_sims %>%
  group_by(sim_id, state) %>%
  summarise(frac_dem_seats_won = mean(margin > 0)) %>%
  group_by(sim_id) %>%
  summarise(dem_states_won = sum(frac_dem_seats_won > 0.5),
            rep_states_won = sum(frac_dem_seats_won < 0.5))

pres_sim_results <- pres_state_sims %>%
  group_by(sim_id, state) %>%
  filter(pct == max(pct)) %>%
  group_by(sim_id, candidate) %>%
  summarise(ev = sum(electoral_votes)) %>%
  left_join(house_states_won %>% 
              mutate(contingent_win = case_when(dem_states_won > 25 ~ "biden",
                                                rep_states_won > 25 ~ "trump",
                                                TRUE ~ "No majority")) %>%
              dplyr::select(sim_id, contingent_win),
            by = "sim_id") %>%
  spread(candidate, ev) %>%
  mutate(contingent_win = ifelse(!is.na(contingent_win), contingent_win, "trump"),
         winner = case_when(biden >= 270 ~ "biden",
                            trump >= 270 ~ "trump",
                            biden <= 269 & contingent_win == "biden" ~ "biden",
                            trump <= 269 & contingent_win == "trump" ~ "trump",
                            TRUE ~ "Something else crazy happens"))

# Write this to an output file
if(!("house_forecast_probability_history.csv" %in% list.files("output"))) {
  write_csv(house_forecast_probability_today, "output/house_forecast_probability_history.csv")
}

house_forecast_probability_history <- read_csv("output/house_forecast_probability_history.csv") %>%
  bind_rows(house_forecast_probability_today) %>%
  group_by(date, party) %>%
  dplyr::slice(n()) %>%
  ungroup()

write_csv(house_forecast_probability_history, "output/house_forecast_probability_history.csv")

rm(district_poll_sims)
gc()

# Fit Senate model if it doesn't exist yet
if(!exists("senate_lm")) {
  source("src/senate_modeling.R")
}

senate_region_deviations <- expand.grid(region = region_names,
                                        sim_id = 1:n_sims) %>%
  as.tbl() %>%
  mutate(region_deviation = rnorm(n(), 0, senate_region_sd))

senate_2024_prior_sims <- read_csv("data/senate_candidates.csv", lazy = FALSE) %>%
  dplyr::select(state, seat_name, incumbent_running, dem_statewide_elected, rep_statewide_elected) %>%
  mutate(class = case_when(seat_name == "Class I" ~ 1,
                           seat_name == "Class II" ~ 2,
                           seat_name == "Class III" ~ 3)) %>%
  dplyr::distinct() %>%
  left_join(pres_state_sims %>% 
              spread(candidate, pct) %>% 
              mutate(state_margin = (biden - trump) / (biden + trump)) %>% 
              dplyr::select(sim_id, state, state_margin), 
            by = "state") %>%
  left_join(last_senate_results %>% dplyr::select(state, class, last_margin = margin), by = c("state", "class")) %>%
  left_join(regions %>% dplyr::select(state, region), by = "state") %>%
  left_join(senate_region_deviations, by = c("region", "sim_id")) %>%
  mutate(incumbent_running = ifelse(incumbent_running == "IND", NA, incumbent_running),
         residual_error = rnorm(n(), 0, senate_residual_sd)) %>%
  mutate(prior_margin = case_when(
    !state %in% c("Vermont", "Maine", "Arizona") ~ predict(senate_lm, newdata = .) + region_deviation + residual_error,
    state == "Vermont" ~ rnorm(n_sims, 0.5, 0.05) + region_deviation + residual_error,
    state == "Maine" ~ rnorm(n_sims, 0.3, 0.05) + region_deviation + residual_error,
    state == "Arizona" ~ rnorm(n_sims, 0, 0.1) + region_deviation + residual_error)
    ) %>%
  group_by(state, seat_name) %>%
  mutate(prior_margin = ifelse(class == 2, -1, prior_margin),
         prior_weight = ifelse(var(prior_margin) > 0, 1 / var(prior_margin), 1)) %>%
  dplyr::select(sim_id, state, seat_name, class, prior_margin, prior_weight)

senate_2024_states_polled <- unique(senate_average_margins$state)

senate_state_cov <- state_cov[senate_2024_states_polled, senate_2024_states_polled]
diag(senate_state_cov) <- diag(senate_state_cov) + senate_average_margins$var

while(!is.positive.definite(senate_state_cov)) {
  diag(senate_state_cov) <- diag(senate_state_cov) + 1e-6
}

senate_poll_errors <- rmvn(n_sims, mu = rep(0, length(senate_2024_states_polled)), sigma = senate_state_cov)
colnames(senate_poll_errors) <- senate_2024_states_polled
senate_poll_errors <- as.data.frame(senate_poll_errors) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "state", value.name = "error") %>%
  as.tbl()

senate_poll_sims <- senate_poll_errors %>%
  left_join(senate_average_margins %>% dplyr::select(state, seat_name, avg), by = "state") %>%
  mutate(poll_margin = avg + error) %>%
  left_join(senate_undecided_pct, by = c("state", "seat_name")) %>%
  mutate(dem_undecided_frac = rbeta(n(), 15, 15),
         poll_margin = poll_margin + undecided * (2 * dem_undecided_frac - 1)) %>%
  group_by(state, seat_name) %>%
  mutate(poll_weight = 1 / var(poll_margin)) %>%
  ungroup() %>%
  dplyr::select(sim_id, state, seat_name, poll_margin, poll_weight)

senate_3p_states <- senate_3p_averages$state
if(length(senate_3p_states) == 1) {
  senate_3p_poll_sims <- tibble(
    sim_id = 1:n_sims,
    state = senate_3p_states,
    pct_3p = rnorm(n_sims, senate_3p_averages$avg, senate_3p_averages$poll_var)
  )
} else if(length(senate_3p_states) > 1) {
  senate_3p_poll_sims <- rmvn(n_sims, senate_3p_averages$avg, diag(senate_3p_averages$poll_var))
  colnames(senate_3p_poll_sims) <- senate_3p_states
  senate_3p_poll_sims <- senate_3p_poll_sims %>%
    as.data.frame() %>%
    mutate(sim_id = 1:n_sims) %>%
    melt(id.vars = "sim_id", variable.name = "state", value.name = "pct_3p") %>%
    as_tibble()
} else {
  senate_3p_poll_sims <- tibble(
    sim_id = 1:n_sims,
    state = "",
    pct_3p = 0
  )
}

senate_state_sims <- senate_2024_prior_sims %>%
  left_join(senate_poll_sims, by = c("sim_id", "state", "seat_name")) %>%
  mutate_at(vars(c("poll_margin", "poll_weight")), replace_na_zero) %>%
  ungroup() %>%
  mutate(margin = (prior_margin * prior_weight + poll_margin * poll_weight) / (prior_weight + poll_weight)) %>%
  left_join(senate_3p_poll_sims, by = c("sim_id", "state")) %>%
  mutate(pct_3p = replace_na_zero(pct_3p),
         pct_dem = (1 - pct_3p) / 2 + margin / 2,
         pct_rep = (1 - pct_3p) / 2 - margin / 2) %>%
  mutate(party = case_when(
    (pct_dem > pct_3p) & (pct_dem > pct_rep) ~ "Democrats",
    (pct_rep > pct_3p) & (pct_rep > pct_dem) ~ "Republicans",
    (pct_3p > pct_dem) & (pct_3p > pct_rep) ~ "Independents"
  )) %>%
  dplyr::select(sim_id, state, seat_name, class, margin, pct_3p, party)

## Timeline for the Senate forecast
current_dem_senate_seats <- 28
current_rep_senate_seats <- 38

senate_seat_distribution <- senate_state_sims %>%
  group_by(sim_id, party) %>%
  summarise(seats_won = n()) %>%
  ungroup() %>%
  mutate(seats_held = case_when(party == "Democrats" ~ seats_won + current_dem_senate_seats,
                                party == "Republicans" ~ seats_won + current_rep_senate_seats)) %>%
  left_join(pres_sim_results %>% dplyr::select(sim_id, pres_winner = winner), by = "sim_id") 

senate_majority_winners <- senate_seat_distribution %>%
  filter(party == "Democrats") %>%
  mutate(majority = case_when(seats_held > 50 ~ "Democrats",
                              seats_held < 50 ~ "Republicans",
                              seats_held == 50 & pres_winner == "biden" ~ "Democrats",
                              seats_held == 50 & pres_winner == "trump" ~ "Republicans"))

senate_majority_probability_today <- senate_seat_distribution %>%
  left_join(senate_majority_winners %>% dplyr::select(sim_id, majority), by = "sim_id") %>%
  group_by(date = today(), state = "National", party) %>%
  summarise(prob = mean(party == majority)) %>%
  ungroup()

senate_state_probabilities <- senate_state_sims %>%
  group_by(state, seat_name) %>%
  summarise(Democrats = mean(party == "Democrats")) %>%
  mutate(Republicans = 1 - Democrats) %>%
  melt(id.vars = c("state", "seat_name"), variable.name = "party", value.name = "prob") %>%
  mutate(date = today()) %>%
  arrange(state, seat_name, party) %>%
  dplyr::select(date, state, seat_name, party, prob)

senate_forecast_probabilities_today <- bind_rows(senate_state_probabilities, senate_majority_probability_today) %>%
  arrange(state, seat_name, party)

# Write this to an output file
if(!("senate_forecast_probability_history.csv" %in% list.files("output"))) {
  write_csv(senate_forecast_probabilities_today, "output/senate_forecast_probability_history.csv")
}

senate_forecast_probability_history <- read_csv("output/senate_forecast_probability_history.csv") %>%
  bind_rows(senate_forecast_probabilities_today) %>%
  group_by(date, state, seat_name, party) %>%
  dplyr::slice(n()) %>%
  ungroup()

write_csv(senate_forecast_probability_history, "output/senate_forecast_probability_history.csv")

rm(senate_poll_sims)
gc()
