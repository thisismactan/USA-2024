source("src/library.R")

# Download polls from FiveThirtyEight
download.file("https://projects.fivethirtyeight.com/polls/data/president_polls.csv", destfile = "data/president_polls.csv")
download.file("https://projects.fivethirtyeight.com/polls/data/senate_polls.csv", destfile = "data/senate_polls.csv")
download.file("https://projects.fivethirtyeight.com/polls/data/house_polls.csv", destfile = "data/house_district_polls.csv")
download.file("https://projects.fivethirtyeight.com/polls/data/generic_ballot_polls.csv", destfile = "data/generic_ballot_polls.csv")

# President
president_polls <- read_csv("data/president_polls.csv", lazy = FALSE) %>%
  dplyr::select(poll_id, state, pollster, question_id, start_date, end_date, n = sample_size, pop = population, mode = methodology, 
                sponsor_party = partisan, tracking, candidate = candidate_name, pct) %>%
  mutate(mode = ifelse(is.na(mode), "IVR", mode)) %>%
  mutate(pct = pct / 100,
         start_date = as.Date(start_date, format = "%m/%d/%y"),
         end_date = as.Date(end_date, format = "%m/%d/%y"),
         spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + round(spread / 2),
         age = as.numeric(today() - median_date),
         state = case_when(is.na(state) ~ "National",
                           !is.na(state) ~ state),
         sponsor_party = case_when(!is.na(sponsor_party) ~ sponsor_party,
                                   is.na(sponsor_party) ~ "None"),
         sponsor_party = case_when(grepl("McLaughlin", pollster) ~ "REP",
                                   !grepl("McLaughlin", pollster) ~ sponsor_party),
         loess_weight = (n^0.25) * ifelse(spread == 1, 1, 5) * ifelse(grepl("IVR|Automated", mode), 1, 2) * 
           ifelse(is.na(mode), 1, 2) * ifelse(pop == "lv", 3, 1) * ifelse(mode == "Live Phone", 2, 1) * 
           ifelse(sponsor_party == "None", 4, 1) * ifelse(is.na(tracking), 1, 1 / spread) *
           ifelse(pollster == "USC Dornsife/Los Angeles Times", 2, 1) / sqrt(abs(spread - 4) + 2)) %>%
  group_by(poll_id, question_id) %>%
  mutate(biden_v_trump = any(grepl("Biden", candidate)) & any(grepl("Trump", candidate)),
         has_kennedy = any(grepl("Kennedy", candidate)),
         has_3p = any(!grepl("Biden|Trump", candidate)),
         has_4p = sum(!grepl("Biden|Trump", candidate)) >= 2) %>%
  ungroup() %>%
  filter(biden_v_trump, pop %in% c("lv", "rv", "v"), grepl("Biden|Trump|Kennedy", candidate), has_kennedy, !has_4p) %>%
  mutate(candidate = case_when(grepl("Kennedy", candidate) ~ "kennedy",
                               grepl("Biden", candidate) ~ "biden",
                               grepl("Trump", candidate) ~ "trump")) %>%
  group_by() %>%
  mutate(n_questions = n_distinct(question_id)) %>%
  ungroup() %>%
  mutate(loess_weight = loess_weight / (n_questions))

## National vs. state polls
national_president_polls <- president_polls %>% 
  filter(state == "National")

state_president_polls <- president_polls %>%
  filter(state != "National")

# House generic ballot
generic_ballot_polls <- read_csv("data/generic_ballot_polls.csv", lazy = FALSE) %>%
  filter(is.na(state), population %in% c("rv", "lv")) %>%
  dplyr::select(poll_id, pollster, question_id, start_date, end_date, n = sample_size, pop = population, mode = methodology, 
                sponsor_party = partisan, tracking, dem, rep) %>%
  melt(measure.vars = c("dem", "rep"), variable.name = "candidate", value.name = "pct") %>%
  mutate(pct = pct / 100,
         start_date = as.Date(start_date, format = "%m/%d/%y"),
         end_date = as.Date(end_date, format = "%m/%d/%y"),
         spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + round(spread / 2),
         age = as.numeric(today() - median_date),
         sponsor_party = case_when(!is.na(sponsor_party) ~ sponsor_party,
                                   is.na(sponsor_party) ~ "None"),
         sponsor_party = case_when(grepl("McLaughlin", pollster) ~ "REP",
                                   !grepl("McLaughlin", pollster) ~ sponsor_party),
         loess_weight = (n^0.25) * ifelse(spread == 1, 1, 5) * ifelse(grepl("IVR|Automated", mode)|is.na(mode), 1, 2) * ifelse(pop == "lv", 3, 1) *
           ifelse(mode == "Live Phone", 2, 1) * ifelse(sponsor_party == "None", 4, 1) * ifelse(is.na(tracking), 1, 1 / spread) / sqrt(abs(spread - 4) + 2)) %>%
  group_by(poll_id, question_id) %>%
  ungroup()

# Individual House district polls
house_district_polls <- read_csv("data/house_district_polls.csv", lazy = FALSE) %>%
  filter(population %in% c("rv", "lv", "v"), cycle == 2024) %>%
  dplyr::select(poll_id, state, seat_number, pollster, question_id, start_date, end_date, n = sample_size, pop = population, mode = methodology, 
                sponsor_party = sponsor_candidate_party, tracking, candidate_party = party, candidate = candidate_name, pct) %>%
  mutate(pct = pct / 100,
         start_date = as.Date(start_date, format = "%m/%d/%y"),
         end_date = as.Date(end_date, format = "%m/%d/%y"),
         spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + round(spread / 2),
         age = as.numeric(today() - median_date),
         sponsor_party = case_when(!is.na(sponsor_party) ~ sponsor_party,
                                   is.na(sponsor_party) ~ "None"),
         sponsor_party = case_when(grepl("McLaughlin", pollster) ~ "REP",
                                   !grepl("McLaughlin", pollster) ~ sponsor_party),
         mode = case_when(is.na(mode) ~ "IVR",
                          !is.na(mode) ~ mode),
         n = case_when(is.na(n) ~ 300,
                       !is.na(n) ~ n),
         loess_weight = (n^0.25) * ifelse(spread == 1, 1, 5) * ifelse(grepl("IVR|Automated", mode), 1, 2) * ifelse(pop == "lv", 3, 1) *
           ifelse(mode == "Live Phone", 2, 1) * ifelse(sponsor_party == "None", 6, 1) * ifelse(is.na(tracking), 1, 1 / spread) / sqrt(abs(spread - 4) + 2)) 

# Senate
senate_polls_all <- read_csv("data/senate_polls.csv", lazy = FALSE) %>%
  filter(!is.na(state), population %in% c("rv", "lv", "v"), cycle == 2024) %>%
  dplyr::select(poll_id, state, seat_name, pollster, question_id, start_date, end_date, n = sample_size, pop = population, mode = methodology, 
                sponsor_party = sponsor_candidate_party, tracking, candidate_party = party, candidate = candidate_name, pct) %>%
  mutate(pct = pct / 100,
         start_date = as.Date(start_date, format = "%m/%d/%y"),
         end_date = as.Date(end_date, format = "%m/%d/%y"),
         spread = as.numeric(end_date - start_date) + 1,
         median_date = start_date + round(spread / 2),
         age = as.numeric(today() - median_date),
         sponsor_party = case_when(!is.na(sponsor_party) ~ sponsor_party,
                                   is.na(sponsor_party) ~ "None"),
         sponsor_party = case_when(grepl("McLaughlin", pollster) ~ "REP",
                                   !grepl("McLaughlin", pollster) ~ sponsor_party),
         mode = case_when(is.na(mode) ~ "IVR",
                          !is.na(mode) ~ mode),
         loess_weight = (n^0.25) * ifelse(spread == 1, 1, 5) * ifelse(grepl("IVR|Automated", mode), 1, 2) * ifelse(pop == "lv", 3, 1) *
           ifelse(mode == "Live Phone", 2, 1) * ifelse(sponsor_party == "None", 4, 1) * ifelse(is.na(tracking), 1, 1 / spread) / sqrt(abs(spread - 4) + 2))

## Candidates
senate_candidates <- read_csv("data/senate_candidates.csv", lazy = FALSE) %>%
  dplyr::select(state, seat_name, candidate_party, candidate_fullname) %>%
  spread(candidate_party, candidate_fullname)

## Filter polls to those with the appropriate candidate matchups
senate_poll_candidates <- senate_polls_all %>%
  dplyr::select(state, seat_name, question_id, candidate_party, candidate) %>%
  group_by(question_id) %>%
  filter(any(candidate_party == "DEM"), any(candidate_party == "REP")) %>%
  ungroup() %>%
  spread(candidate_party, candidate) %>%
  filter(state != "Arizona" | !is.na(IND))

senate_question_ids <- senate_candidates %>%
  left_join(senate_poll_candidates, by = c("state", "seat_name", "DEM", "REP")) %>%
  filter(!is.na(question_id)) %>%
  pull(question_id)

senate_polls <- senate_polls_all %>%
  filter(question_id %in% senate_question_ids)
