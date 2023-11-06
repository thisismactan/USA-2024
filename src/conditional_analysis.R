condition_sim_ids <- pres_state_sims %>%
  filter(state == "Michigan", trump >= biden) %>%
  pull(sim_id)

condition_sim_ids_2 <- pres_sim_results %>%
  pull(sim_id)

condition_sim_ids <- base::intersect(condition_sim_ids, condition_sim_ids_2)
n_condition_sims <- length(condition_sim_ids)

condition_sims <- pres_state_sims %>%
  filter(sim_id %in% condition_sim_ids)

condition_sims %>%
  group_by(sim_id) %>%
  summarise(biden_ev = sum(electoral_votes * (biden > trump))) %>%
  group_by(biden_wins = biden_ev >= 270) %>%
  summarise(n = n() / n_condition_sims)

conditional_state_probabilities <- condition_sims %>%
  group_by(state) %>%
  summarise(biden_prob = mean(biden > trump)) %>%
  mutate(trump = 1 - biden_prob) %>%
  dplyr::select(state, biden = biden_prob, trump)

national_popular_vote_sims %>%
  mutate(winner = ifelse(national_two_party_margin > 0, "biden", "trump")) %>%
  filter(sim_id %in% condition_sim_ids) %>%
  ggplot(aes(x = national_two_party_margin, y = ..count.. / n_condition_sims, fill = winner)) +
  geom_histogram(breaks = seq(from = -0.2, to = 0.2, by = 0.01), col = "black") +
  scale_fill_manual(name = "Popular vote winner", values = candidate_colors, labels = candidate_fullnames) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "National popular vote margin forecast", subtitle = "Conditional on Trump winning MI, PA, and WI",
       x = "Biden margin over Trump", y = "Probability")
