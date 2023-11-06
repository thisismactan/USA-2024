pres_state_sim_results <- pres_state_sims %>%
  mutate(biden_win = biden > trump) %>%
  dplyr::select(sim_id, state, biden, trump, biden_win) %>%
  filter(sim_id %in% 1:20000)

cartprod <- pres_state_sim_results %>%
  full_join(pres_state_sim_results, by = "sim_id")

joint_prob <- cartprod %>%
  group_by(state.x) %>%
  mutate(prob_x = mean(biden_win.x)) %>%
  group_by(state.y) %>%
  mutate(prob_y = mean(biden_win.y)) %>%
  group_by(state.x, prob_x, state.y, prob_y) %>%
  summarise(joint_biden_prob = mean(biden_win.x * biden_win.y)) %>%
  mutate(non_independence = joint_biden_prob - prob_x * prob_y) %>%
  ungroup()

cond_prob <- cartprod %>%
  mutate(winner = ifelse(biden_win.x, "Biden", "Trump")) %>%
  group_by(winner, state.x, state.y) %>%
  summarise(cond_prob_y_x = mean(biden_win.y)) %>%
  spread(winner, cond_prob_y_x, fill = 0) %>%
  mutate(Trump = 1 - Trump) %>%
  left_join(pres_state_probabilities, by = c("state.y" = "state")) %>%
  mutate(biden_improv = Biden - biden,
         trump_improv = Trump - trump)

# Slope coefficients of one twixt the other
pres_state_sims_wide <- pres_state_sims %>% 
  filter(!grepl("congress", state)) %>%
  mutate(margin = biden - trump) %>%
  dplyr::select(sim_id, state, margin) %>%
  spread(state, margin) 

# Example plots
pres_state_sims_wide %>%
  dplyr::select(sim_id, Michigan, Illinois, Nevada) %>%
  melt(id.vars = c("sim_id", "Michigan"), variable.name = "state_2", value.name = "margin") %>%
  group_by(state_2) %>%
  dplyr::slice(1:10000) %>%
  ggplot(aes(x = 100 * Michigan, y = 100 * margin, col = state_2)) +
  facet_wrap(~state_2) +
  geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = "lm", col = "black") +
  scale_colour_manual(name = "Second state", values = c("green4", "purple")) +
  lims(x = c(-20, 50), y = c(-20, 50)) +
  labs(title = "Biden margin elasticity with respect to Michigan margin",
       x = "Biden margin in Michigan (pp)", y = "Biden margin in second state (pp)")

state_cov_matrix <- pres_state_sims_wide %>%
  dplyr::select(-sim_id) %>%
  cov()

state_variances <- pres_state_sims_wide %>%
  dplyr::select(-sim_id) %>% 
  apply(2, var)

pairwise_regression_coefficients <- as.data.frame(state_cov_matrix / state_variances) %>%
  mutate(state = names(state_variances)) %>%
  left_join(regions %>% dplyr::select(state, abbrev), by = "state") %>%
  dplyr::select(state_1 = state, abbrev, everything()) %>%
  melt(id.vars = c("state_1", "abbrev"), variable.name = "state_2", value.name = "b")

pairwise_regression_coefficients <- as.data.frame(state_cov_matrix / state_variances) %>%
  mutate(state = names(state_variances)) %>%
  left_join(regions %>% dplyr::select(state, abbrev), by = "state") %>%
  dplyr::select(state_1 = state, abbrev, everything()) %>%
  melt(id.vars = c("state_1", "abbrev"), variable.name = "state_2", value.name = "b") %>%
  as.tbl()

# Ranked
pairwise_regression_coefficients %>%
  filter(state_1 != state_2) %>%
  arrange(desc(b)) %>%
  print(n = 100)

pairwise_regression_coefficients %>%
  ggplot(aes(x = state_1, y = state_2, fill = b)) +
  geom_tile() +
  coord_flip() +
  scale_fill_gradient(name = "Coefficient", low = "white", high = "green4", limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "State pairwise elasticity matrix",
       x = "Predictor state", y = "Outcome state")

# Competitive states only
pairwise_regression_coefficients %>%
  filter(state_1 %in% comp_states, state_2 %in% comp_states) %>%
  ggplot(aes(x = state_1, y = state_2, fill = b)) +
  geom_tile() +
  coord_flip() +
  scale_fill_gradient(name = "Coefficient", low = "white", high = "green4", limits = c(0, 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "State pairwise elasticity matrix", subtitle = "Competitive states only",
       x = "Predictor state", y = "Outcome state")

# Matrices
## Biden's improvement
conditional_margins <- cartprod %>%
  mutate(margin.x = biden.x - trump.x,
         margin.y = biden.y - trump.y) %>%
  left_join(national_popular_vote_sims %>% dplyr::select(sim_id, national_two_party_margin), by = "sim_id") %>%
  dplyr::select(sim_id, state.x, margin.x, biden_win.x, state.y, margin.y, biden_win.y, national_two_party_margin) %>%
  group_by(state.y) %>%
  mutate(avg_margin.y = mean(margin.y)) %>%
  group_by(state.x, state.y, biden_win.x, unconditional_margin = avg_margin.y) %>%
  summarise(conditional_avg_margin = mean(margin.y)) %>%
  mutate(relative_conditional_avg_margin = conditional_avg_margin - conditional_avg_natl_margin)

conditional_margins %>%
  group_by(state.x, state.y) %>%
  mutate(diff = relative_conditional_avg_margin - lag(relative_conditional_avg_margin)) %>%
  dplyr::select(state.x, state.y, diff) %>%
  na.omit() %>%
  spread(state.y, diff) %>%
  filter(state.x %in% comp_states) %>%
  dplyr::select(comp_states)

state_cor <- pres_state_sims %>%
  filter(!grepl("congress", state)) %>%
  left_join(regions %>% dplyr::select(state, abbrev), by = "state") %>%
  mutate(dem_margin = biden - trump) %>%
  dplyr::select(sim_id, abbrev, dem_margin) %>%
  spread(abbrev, dem_margin) %>%
  dplyr::select(-sim_id) %>%
  cor()

diag(state_cor) <- rep(as.numeric(NA), nrow(state_cor))

ggcorrplot(state_cor)
