makeActiveBinding(".", function() .Last.value, env = globalenv())


library(tidyverse)
d = read_csv("data/intermediate/waves_merged.csv")

#### select media used in content analysis ####
codebook = read_csv("data/raw/codebook.csv") |>
  mutate(var=str_c(variable, "_", value))

# (1) media_channel_weights: How often is news consumed through each channel?
weight_columns = codebook |>
  filter(variable=="I1") |> 
  mutate(label=str_remove(label, " .*"),
         channel=case_match(label, 
                            "Television" ~ "TV",
                            "Newspapers" ~ "newspaper",
                            "Online" ~ "website",
                            "News" ~ "app")) |>
  filter(!is.na(channel)) |>
  select(var, channel)

media_channel_weights = d |> select(iisID, wave, I1_1:I1_9) |> 
  pivot_longer(-iisID:-wave, names_to="var", values_to = "weight") |>
  inner_join(weight_columns) |> 
  filter(!is.na(weight), weight>0) |>
  mutate(weight_consumption=weight/7) |>
  select(iisID, wave, channel, weight_consumption)

# (2) Which media (brands) have people consumed?
media_cols <- codebook |>
  filter(variable %in% c("I2", "I3", "I4", "I7")) |> 
  mutate(label=str_remove_all(label, "de | Handelsblad|\\.nl"),
         label=case_match(label,
                          "Algemeen Dagblad (AD)"~"AD", 
                          "NOS" ~ "NOS_app",
                          "NOS Journaal" ~ "NOS_journaal",
                          .default = label)) |>
  filter(label %in% c("AD", "Telegraaf", "Volkskrant", "NRC", "Trouw", "Nu", "NOS_app", "NOS_journaal", "Nieuwsuur")) |>
  mutate(channel=case_match(variable,
                            "I2" ~ "newspaper",
                            "I3" ~ "TV",
                            "I4" ~ "website",
                            "I7" ~ "app")) |>
  select(var, channel, medium=label)

media_consumption <- d |>
  select(iisID, wave, matches("I[2,3,4,7]_\\d")) |>
  pivot_longer(-iisID:-wave, names_to = "var", values_to = "consumed") |>
  inner_join(media_cols) |> 
  filter(!is.na(consumed))

media_consumption |> filter(iisID == "558689")
media_channel_weights |> filter(iisID == "558689")

# (3) combine consumption and weights
media_consumption_weighted <- media_consumption |>
  inner_join(media_channel_weights) |> 
  group_by(iisID, wave, medium) |>
  summarize(weight_consumption=max(weight_consumption), .groups = "drop") |>
  filter(wave != "pre-wave") |>
  mutate(wave = str_remove(wave, "wave ") |> as.numeric())

# Next, get list of dates representing media in between waves for each respondent:

# (1) Create tibble containing all possible respondent x day combinations in the study period
all_respondents = d |> select(iisID) |> unique()
all_days = tibble(date=seq.Date(from=as.Date("2021-01-13"), to=as.Date("2021-03-17"), by = "day")) |>
  cross_join(all_respondents)

# (2) Create tibble attributing each day for each respondent to a specific wave  
date_waves <- d |>
  # 1. Keep only end date as date (so without time) and convert wave to integer
  select(iisID, wave, date=end_date) |>
  mutate(date=as.Date(date),
         wave=wave |> str_replace_all("pre-wave", "wave 0") |> str_remove_all("wave ") |> as.numeric()) |>
  # (Copy wave column to make checking results easier. Can we dropped if we're happy)
  mutate(survey=wave) |>
  # Join with all date x respondent combinations, sort by respondent and date, and fill up waves
  full_join(all_days, by=c("date", "iisID")) |>
  arrange(iisID, date) |>
  fill(wave, .direction="up") |>
  filter(wave != 0) |> 
  group_by(iisID, wave) |>
  mutate(days_to_survey=as.numeric(max(date) - date),
         weight_decay=.5^(days_to_survey/7)) |> 
  select(iisID, wave, date, weight_decay)

# Check for two respondents:
# date_waves |>filter(iisID %in% c(558719, 558789)) |> View()

## Combine consumption data with dates
# check that all respondents with consumption have dates
if (media_consumption_weighted |> anti_join(date_waves) |> nrow() > 0) stop("!")

# there are respondents that filled in which channels they use, but not which media
# What to do?
# date_waves |> anti_join(media_consumption_weighted) |> select(iisID, wave) |> unique()
# e.g. d |> filter(iisID == 558666) |> select(matches("^I\\d")) |> View()

# For now, we drop them, but maybe it would be better to impute something?
media_per_respondent = inner_join(date_waves, media_consumption_weighted, relationship='many-to-many')


#### Compute media content variables per day/medium ####

net = readxl::read_xlsx("data/raw-private-encrypted/NETcoderingenALLcontrole_final.xlsx") |> 
  select(publisher, date, type, src, subject, object, dir) |>
  filter(!is.na(type)) |>
  mutate(value=case_match(dir, "p"~1, "n"~ -1, "o" ~ 0))

parties = c("VVD", "CDA", "FvD", "PVV", "D66", "GroenLinks", "PvdA", "SP", "CU", "JA21", "PvdD", "SGP", "50Plus", "VOLT", "BIJ1", "DENK", "BBB")

# SF Success and failure: all reality statements towards a party
media_statements_sf = net |> filter(type == "SF") |> 
  select(publisher, date, type, party=object, value)


# SC form other own party, other parties, media, society
# (should probably collapse some categories)
# Also include eva statements as source -> subject

other_politics = c("LKrol", "parlement", "rechtsePartijen", "linksePartijen", "party")
media = c("media", "journalist", "webmedia", "rtv")

media_statements_cc = bind_rows(
  filter(net, type == "EVA") |> select(-object) |> rename(subject=src, object=subject),
  filter(net, type == "CC")
  ) |> 
  filter(object %in% parties) |>
  mutate(type=case_when(
    subject == object ~ "CC_own",
    subject %in% c(parties, other_politics) ~ "CC_parties",
    is.na(subject) | subject %in% media ~ "CC_media",
    T ~ "CC_other"
  )) |>
  select(publisher, date, type, party=object, value)

# REA: positive REA statements are positive for incumbents, negative for opposition
# Also taking CAU, as it seems to mostly descibe that X is causing Y, 
#  not that it would potentially cause Y
incumbent <- c("VVD", "CDA", "CU", "D66")
rea_values <- tibble(party=parties) |>
  mutate(rea_value=if_else(party %in% incumbent, 1, -1))
media_statements_rea <- net |> filter(type %in% c("REA", "CAU")) |> 
  mutate(type="REA", 
         value=value * if_else(subject == "coronaverspreiding", -1, 1)) |>
  cross_join(rea_values) |>
  mutate(value = value * rea_value) |>
  select(publisher, date, type, party, value)

# OWNED ISSUES  

issues <- read_csv("data/raw/2021_issues.csv") |> na.omit()

media_statements_iss <- bind_rows(select(net, publisher, date, issue=subject),
                                  select(net, publisher, date, issue=object)) |> 
  inner_join(issues) |> add_column(type="ISS", value=1) |>
  select(publisher, date, type, party, value)

# All media statements per publisher/date/party

media_statements <- bind_rows(
  media_statements_cc,
  media_statements_sf,
  media_statements_rea,
  media_statements_iss,
) |> 
  rename(medium=publisher) |> 
  mutate(medium=case_match(medium, 
                             "Algemeen Dagblad" ~ "AD",
                             "De Telegraaf" ~ "Telegraaf",
                             "De Volkskrant" ~ "Volkskrant",
                             "Nieuwsuur" ~ "Nieuwsuur",
                             "NOS Journaal 20:00" ~ "NOS_journaal",
                             "NOS liveblog" ~ "NOS_app",
                             "NOS nieuws" ~ "NOS_app",
                             "NU.nl" ~ "Nu",
                             "Trouw" ~ "Trouw"))

#### Combine media statements with respondents

media_per_respondent$medium |> table()

media_statements$publisher |> table()

content_per_respondent <- inner_join(media_per_respondent, media_statements, relationship = "many-to-many")
content_per_respondent |> 
  group_by(iisID, wave, party, type) |> 
  summarize(value=sum(value*weight_decay*weight_consumption))

x = tibble(days_to_survey=0:10)
x |> mutate(decay_weight=.5^(days_to_survey/7))
