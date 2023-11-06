source("src/shape_polls.R")

poll_dates <- seq(from = as.Date("2023-01-01"), to = today(), by = 1)
n_days <- length(poll_dates)

national_president_average_list <- vector("list", n_days)

# Presidential polls ####
for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute averages and standard errors
  suppressMessages(national_president_average_list[[i]] <- national_president_polls %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = (age <= 30) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0) %>%
    group_by(candidate, state) %>%
    summarise(avg = wtd.mean(pct, weight),
              var = n() * wtd.var(pct, 1e6 * weight) / (n() - 1),
              eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date))
}

national_president_averages <- bind_rows(national_president_average_list)

# Computing house effects
candidate_pcts_vs_natl <- national_president_polls %>%
  left_join(national_president_averages, by = c("candidate", "state", "median_date"))

biden_house_effect_model <- lmer(pct ~ (1|pop) + (1|pollster), 
                                 data = candidate_pcts_vs_natl %>% filter(candidate == "biden"), 
                                 weights = loess_weight)

trump_house_effect_model <- lmer(pct ~ (1|pop) + (1|pollster), 
                                 data = candidate_pcts_vs_natl %>% filter(candidate == "trump"), 
                                 weights = loess_weight)

kennedy_house_effect_model <- lmer(pct ~ (1|pop) + (1|pollster), 
                                   data = candidate_pcts_vs_natl %>% filter(candidate == "kennedy"),
                                   weights = loess_weight)

# Converting to a mergeable data frame
biden_house_effects <- ranef(biden_house_effect_model)$pollster %>%
  as.data.frame()

trump_house_effects <- ranef(trump_house_effect_model)$pollster %>%
  as.data.frame()

kennedy_house_effects <- ranef(kennedy_house_effect_model)$pollster %>%
  as.data.frame()

house_effects <- biden_house_effects %>%
  mutate(pollster = rownames(biden_house_effects)) %>%
  dplyr::select(pollster, biden = `(Intercept)`) %>%
  left_join(trump_house_effects %>%
              mutate(pollster = rownames(trump_house_effects)) %>%
              dplyr::select(pollster, trump = `(Intercept)`),
            by = "pollster") %>%
  left_join(kennedy_house_effects %>%
              mutate(pollster = rownames(kennedy_house_effects)) %>%
              dplyr::select(pollster, kennedy = `(Intercept)`),
            by = "pollster") %>%
  mutate(kennedy = ifelse(is.na(kennedy), 0, kennedy)) %>%
  melt(id.vars = "pollster", variable.name = "candidate", value.name = "house") %>%
  as_tibble() 

# Bias due to RV
rv_bias <- tibble(candidate = c("biden", "trump", "kennedy"),
                  rv_bias = c(ranef(biden_house_effect_model)$pop["rv", 1],
                              ranef(trump_house_effect_model)$pop["rv", 1],
                              ranef(kennedy_house_effect_model)$pop["rv", 1]))

# Adjusted national polls
national_president_polls_adj <- national_president_polls %>%
  left_join(house_effects, by = c("pollster", "candidate")) %>%
  left_join(rv_bias, by = "candidate") %>%
  mutate(house = ifelse(is.na(house), 0, house)) %>%
  mutate(pct_adj = pct - house - rv_bias * (pop != "lv"))

# Covariance matrix for current polls
president_poll_matrix <- national_president_polls_adj %>%
  mutate(weight = (age <= 30) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
  filter(weight > 0) %>%
  dplyr::select(weight, poll_id, question_id, candidate, pct) %>% 
  spread(candidate, pct) %>%
  dplyr::select(weight, biden, trump, kennedy)

president_poll_covariance_2p <- cov.wt(as.matrix(president_poll_matrix %>% filter(is.na(kennedy)) %>% dplyr::select(biden, trump)), 
                                    wt = president_poll_matrix %>% filter(is.na(kennedy)) %>% pull(weight))

weight_sum_2p <- sum(president_poll_covariance_2p$wt)

president_poll_covariance_3p <- cov.wt(as.matrix(president_poll_matrix %>% filter(!is.na(kennedy)) %>% dplyr::select(biden, trump, kennedy)), 
                                      wt = president_poll_matrix %>% filter(!is.na(kennedy)) %>% pull(weight))

weight_sum_3p <- sum(president_poll_covariance_3p$wt)

president_poll_covariance_2p_matrix <- rbind(president_poll_covariance_2p$cov, "kennedy" = c(0, 0)) %>%
  cbind("kennedy" = c(0, 0, president_poll_covariance_2p$cov %>% diag() %>% sum()))

president_poll_covariance <- weight_sum_3p * president_poll_covariance_3p$cov + 
  weight_sum_2p * president_poll_covariance_2p_matrix

# Recompute with house effect-adjusted polls
national_president_average_adj_list <- vector("list", n_days)
 
# Presidential polls
for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute averages and standard errors
  suppressMessages(national_president_average_adj_list[[i]] <- national_president_polls_adj %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = (age <= 30) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0) %>%
    group_by(candidate, state) %>%
    summarise(avg = wtd.mean(pct, weight),
              var = n() * wtd.var(pct, 1e6 * weight) / (n() - 1),
              eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date))
}

# Averages
national_president_averages_adj <- bind_rows(national_president_average_adj_list)

# Time trend-adjusting state polls
state_president_poll_leans <- state_president_polls %>%
  left_join(national_president_averages_adj %>% dplyr::select(-state), by = c("candidate", "median_date")) %>%
  left_join(house_effects, by = c("pollster", "candidate")) %>%
  left_join(rv_bias, by = "candidate") %>%
  mutate(house = ifelse(is.na(house), 0, house)) %>%
  mutate(pct_adj = pct - house - rv_bias * (pop != "lv"),
         state_lean = pct_adj - avg)

state_president_average_list <- vector("list", n_days)

for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute averages and standard errors
  suppressMessages(state_president_average_list[[i]] <- state_president_poll_leans %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = 100 * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0) %>%
    group_by(candidate, state) %>%
    summarise(avg_lean = wtd.mean(state_lean, weight),
              lean_var = wtd.var(state_lean, 126 * weight),
              lean_eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date))
}

state_president_averages <- bind_rows(state_president_average_list) %>%
  arrange(state, median_date, candidate) %>%
  left_join(national_president_averages_adj %>% dplyr::select(-state), by = c("candidate", "median_date")) %>%
  mutate(state_avg = avg + avg_lean,
         state_var = lean_var + var,
         state_eff_n = lean_eff_n) %>%
  dplyr::select(candidate, state, avg = state_avg, var = state_var, eff_n = state_eff_n, median_date) %>%
  arrange(state, candidate, median_date) %>%
  group_by(state, candidate) %>%
  na.locf()
  
president_averages <- bind_rows(national_president_averages_adj, state_president_averages)

# Smoothed averages
president_averages_smoothed <- president_averages %>%
  group_by(state, candidate) %>%
  arrange(state, candidate, median_date) %>%
  mutate(avg = rollmeanr(avg, 5, na.pad = TRUE),
         var = rollmeanr(var, 5, na.pad = TRUE),
         eff_n = rollmeanr(eff_n, 5, na.pad = TRUE))


# House polls ####
generic_ballot_average_list <- vector("list", n_days)

for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute averages and standard errors
  generic_ballot_average_list[[i]] <- generic_ballot_polls %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = (age <= 30) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0) %>%
    group_by(candidate) %>%
    summarise(avg = wtd.mean(pct, weight),
              var = n() * wtd.var(pct, weight) / (n() - 1),
              eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date)
}

generic_ballot_averages <- bind_rows(generic_ballot_average_list)

# Computing house effects
dem_generic_ballot_leads <- generic_ballot_polls %>%
  filter(candidate %in% c("dem", "rep")) %>%
  left_join(generic_ballot_averages, by = c("candidate", "median_date")) %>%
  arrange(poll_id, question_id, candidate) %>%
  group_by(poll_id, question_id) %>%
  mutate(dem_lead = pct - lead(pct),
         avg_dem_lead = avg - lead(avg),
         diff = dem_lead - avg_dem_lead) %>%
  filter(!is.na(dem_lead), !is.na(avg_dem_lead)) %>%
  dplyr::select(poll_id, question_id, median_date, pollster, pop, sponsor_party, loess_weight, dem_lead, avg_dem_lead, diff)

generic_ballot_house_effect_model <- lmer(diff ~ (1|pop) + (1|pollster), data = dem_generic_ballot_leads, weights = loess_weight)

# Converting to a mergeable data frame
generic_ballot_house_effects <- ranef(generic_ballot_house_effect_model)$pollster %>%
  as.data.frame()
generic_ballot_house_effects <- generic_ballot_house_effects %>% 
  mutate(pollster = rownames(generic_ballot_house_effects)) %>%
  dplyr::select(pollster, house = `(Intercept)`) %>%
  as.tbl()

# Bias due to RV
generic_ballot_rv_bias <- ranef(generic_ballot_house_effect_model)$pop["rv", 1]

# Adjusted national polls
generic_ballot_polls_adj <- generic_ballot_polls %>%
  left_join(generic_ballot_house_effects, by = c("pollster")) %>%
  mutate(pct_adj = case_when(candidate == "dem" ~ pct - house / 2 - generic_ballot_rv_bias,
                             candidate == "rep" ~ pct + house / 2 + generic_ballot_rv_bias,
                             !(candidate %in% c("dem", "rep")) ~ pct))


# Covariance matrix for current polls
generic_ballot_poll_matrix <- generic_ballot_polls_adj %>%
  mutate(weight = (age <= 30) * (age >= 0) * loess_weight / exp((age + 2)^0.5)) %>%
  filter(weight > 0) %>%
  dplyr::select(weight, poll_id, question_id, candidate, pct) %>% 
  spread(candidate, pct) %>%
  dplyr::select(weight, dem, rep)

generic_ballot_poll_covariance <- cov.wt(as.matrix(generic_ballot_poll_matrix %>% dplyr::select(dem, rep)), 
                                         wt = generic_ballot_poll_matrix$weight)

# Recompute with house effect-adjusted polls
generic_ballot_average_adj_list <- vector("list", n_days)

# Generic ballot polls
for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute averages and standard errors
  suppressMessages(generic_ballot_average_adj_list[[i]] <- generic_ballot_polls_adj %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = (age <= 30) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0) %>%
    group_by(candidate) %>%
    summarise(avg = wtd.mean(pct, weight),
              var = n() * wtd.var(pct, 1e6 * weight) / (n() - 1),
              eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date))
}

# Averages
generic_ballot_averages_adj <- bind_rows(generic_ballot_average_adj_list)

# Smoothed averages
generic_ballot_averages_smoothed <- generic_ballot_averages_adj %>%
  group_by(candidate) %>%
  arrange(candidate, median_date) %>%
  mutate(avg = rollmeanr(avg, 5, na.pad = TRUE),
         var = rollmeanr(var, 5, na.pad = TRUE),
         eff_n = rollmeanr(eff_n, 5, na.pad = TRUE)) %>%
  ungroup()

# Time trend-adjusting district-level polls
district_poll_leans <- house_district_polls %>%
  left_join(generic_ballot_averages_adj %>% mutate(candidate = toupper(as.character(candidate))), 
            by = c("candidate_party" = "candidate", "median_date" = "median_date")) %>%
  left_join(generic_ballot_house_effects, by = "pollster") %>%
  mutate(weight = 100 * loess_weight / exp((age + 1)^0.5),
         house = case_when(is.na(house) ~ 0,
                           !is.na(house) ~ house),
         pct = case_when(candidate_party == "DEM" ~ pct - house / 2 - generic_ballot_rv_bias + 0.02 * (sponsor_party == "REP") - 0.02 * (sponsor_party == "DEM"),
                         candidate_party == "REP" ~ pct + house / 2 + generic_ballot_rv_bias - 0.02 * (sponsor_party == "REP") + 0.02 * (sponsor_party == "DEM"),
                         !(candidate %in% c("DEM", "REP")) ~ pct),
         district_lean = pct - avg) %>% 
  group_by(question_id, weight, candidate_party) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  filter(!is.na(avg))

district_poll_leans_simp <- district_poll_leans %>%
  dplyr::select(question_id, weight, candidate_party, district_lean) %>%
  spread(candidate_party, district_lean) %>%
  filter(!is.na(DEM), !is.na(REP))

district_poll_cov <- cov.wt(district_poll_leans_simp %>% dplyr::select(DEM, REP),
                            wt = district_poll_leans_simp$weight)$cov[1,2]

district_averages <- district_poll_leans %>%
  left_join(generic_ballot_averages_adj %>% 
              mutate(candidate_party = toupper(as.character(candidate))) %>%
              filter(median_date == today()) %>%
              dplyr::select(candidate_party, avg_today = avg, var_today = var, eff_n_today = eff_n), by = c("candidate_party")) %>%
  group_by(state, seat_number, candidate_party, var_today, eff_n_today) %>%
  summarise(district_respondents = sum(n),
            district_avg = wtd.mean(district_lean + avg_today, weight),
            district_var = wtd.var(district_lean, weight),
            district_eff_n = sum(weight)^2 / sum(weight^2)) %>%
  mutate(district_var = district_var + 0.25 / sum(district_respondents) + var_today / eff_n_today + 0.05^2 / district_eff_n) %>%
  group_by(state, seat_number) %>%
  dplyr::mutate(poll_margin = district_avg - lead(district_avg),
                poll_var = sum(district_var),
                poll_weight = 1 / poll_var) %>%
  ungroup() %>%
  dplyr::select(state, seat_number, poll_margin, poll_var, poll_weight) %>%
  na.omit()

# Senate polls ####
senate_poll_leans <- senate_polls %>%
  left_join(generic_ballot_averages_adj %>% mutate(candidate = toupper(as.character(candidate))), 
            by = c("candidate_party" = "candidate", "median_date" = "median_date")) %>%
  left_join(generic_ballot_house_effects, by = "pollster") %>%
  mutate(weight = loess_weight / exp((age + 1)^0.5),
         house = case_when(is.na(house) ~ 0,
                           !is.na(house) ~ house),
         pct = case_when(candidate_party == "DEM" ~ pct - house / 2 - generic_ballot_rv_bias + 0.02 * (sponsor_party == "REP") - 0.02 * (sponsor_party == "DEM"),
                         candidate_party == "REP" ~ pct + house / 2 + generic_ballot_rv_bias - 0.02 * (sponsor_party == "REP") + 0.02 * (sponsor_party == "DEM"),
                         !(candidate %in% c("DEM", "REP")) ~ pct),
         state_lean = pct - avg)

senate_average_list <- vector("list", n_days)

for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute averages and standard errors
  suppressMessages(senate_average_list[[i]] <- senate_poll_leans %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = 100 * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0) %>%
    group_by(candidate, candidate_party, state, seat_name) %>%
    summarise(avg_lean = wtd.mean(state_lean, weight),
              lean_var = wtd.var(state_lean, 1e6 * weight),
              lean_eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date))
}

senate_averages <- bind_rows(senate_average_list) %>%
  arrange(state, seat_name, median_date, candidate_party, candidate) %>%
  left_join(generic_ballot_averages_adj %>%
              mutate(candidate = toupper(candidate)), by = c("candidate_party" = "candidate", "median_date")) %>%
  mutate(state_avg = avg + avg_lean,
         state_var = lean_var + var,
         state_eff_n = lean_eff_n) %>%
  dplyr::select(candidate, candidate_party, state, seat_name, avg = state_avg, var = state_var, eff_n = state_eff_n, median_date) %>%
  arrange(state, seat_name, candidate, median_date) %>%
  group_by(state, seat_name, candidate) %>%
  na.locf()

# Adjusted national polls
senate_polls_adj <- senate_polls %>%
  left_join(generic_ballot_house_effects, by = c("pollster")) %>%
  mutate(house = ifelse(is.na(house), 0, house),
         pct_adj = case_when(candidate_party == "DEM" ~ pct - house / 2 - generic_ballot_rv_bias + 0.02 * (sponsor_party == "REP") - 0.02 * (sponsor_party == "DEM"),
                             candidate_party == "REP" ~ pct + house / 2 + generic_ballot_rv_bias - 0.02 * (sponsor_party == "REP") + 0.02 * (sponsor_party == "DEM"),
                             !(candidate %in% c("DEM", "REP")) ~ pct))


# Covariance matrix for current polls
senate_poll_matrix <- senate_polls_adj %>%
  mutate(weight = (age <= 30) * (age >= 0) * loess_weight / exp((age + 2)^0.5)) %>%
  filter(weight > 0) %>%
  dplyr::select(weight, poll_id, question_id, candidate_party, pct) %>% 
  spread(candidate_party, pct) %>%
  dplyr::select(weight, DEM, REP)

senate_poll_covariance <- cov.wt(as.matrix(senate_poll_matrix %>% dplyr::select(DEM, REP)), 
                                 wt = senate_poll_matrix$weight)

# Recompute with house effect-adjusted polls
senate_average_adj_list <- vector("list", n_days)

# Generic ballot polls
for(i in 1:n_days) {
  current_date <- poll_dates[i]
  
  # Compute averages and standard errors
  suppressMessages(senate_average_adj_list[[i]] <- senate_polls_adj %>%
    mutate(age = as.numeric(current_date - median_date),
           weight = 100 * (age <= 60) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
    filter(weight > 0) %>%
    group_by(candidate, candidate_party, state, seat_name) %>%
    summarise(avg = wtd.mean(pct, weight),
              var = n() * wtd.var(pct, 1e6 * weight) / (n() - 1),
              eff_n = sum(weight)^2 / sum(weight^2)) %>%
    mutate(median_date = current_date))
}

# Averages
senate_averages_adj <- bind_rows(senate_average_adj_list) %>%
  arrange(state, seat_name, candidate_party, candidate)

senate_average_margins <- senate_polls_adj %>%
  mutate(age = as.numeric(today() - median_date),
         weight = (age <= 60) * (age >= 0) * loess_weight / exp((age + 1)^0.5)) %>%
  filter(weight > 0) %>%
  dplyr::select(-candidate, -pct) %>%
  spread(candidate_party, pct_adj) %>%
  mutate(margin = DEM - REP) %>%
  dplyr::select(state, seat_name, weight, margin) %>%
  group_by(state, seat_name) %>%
  summarise(avg = wtd.mean(margin, weight),
            var = n() * wtd.var(margin, 1e6 * weight) / (n() - 1),
            eff_n = sum(weight)^2 / sum(weight^2)) %>%
  mutate(var = case_when(var == Inf | is.na(var) ~ 0.05^2 + 0.03^2,
                         var < Inf ~ (var + 0.03^2) / eff_n))

current_senate_averages <- senate_averages_adj %>%
  group_by(state, seat_name, candidate) %>%
  dplyr::slice(n()) %>%
  mutate(poll_var = ifelse(is.na(var), 0.05^2, var) / eff_n + 0.03^2) 

senate_undecided_pct <- current_senate_averages %>%
  group_by(state, seat_name) %>%
  summarise(undecided = 1 - sum(avg))

# Smoothed averages
senate_averages_smoothed <- senate_averages_adj %>%
  group_by(state, seat_name, candidate) %>%
  arrange(state, seat_name, candidate, median_date) %>%
  mutate(avg = rollmeanr(avg, 5, na.pad = TRUE),
         var = rollmeanr(var, 5, na.pad = TRUE),
         eff_n = rollmeanr(eff_n, 5, na.pad = TRUE)) %>%
  ungroup()

# Clean up after yourself
rm(list = grep("_list", ls(), value = TRUE))

