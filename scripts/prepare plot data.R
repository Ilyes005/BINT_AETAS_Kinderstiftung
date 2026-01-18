## --- Prepare analysis-ready datasets for plotting ------------------------
# This section creates the final data subsets used directly for figures and
# visualizations based on the previously cleaned member-specific datasets.


####MEMBER 1 --------------------------------------------------------------------------------------------------------------
##stress level and time since event--------------------------------------------------------------------
data_with_phase <- selected.data %>%
  mutate(
    time_num = as.numeric(`F11 Wie lange zurück`),
    phase = case_when(
      time_num %in% 1:2 ~ "0–1 years",
      time_num %in% 3:5 ~ "2–4 years",
      time_num %in% 6:10 ~ "5–9 years",
      time_num >= 11     ~ "10+ years"
    ),
    phase = factor(phase,
                   levels = c("0–1 years", "2–4 years", "5–9 years", "10+ years"))
  )

summary_by_time <- data_with_phase %>%
  group_by(phase) %>%
  summarise(
    med = median(current_stress_level, na.rm = TRUE),
    mean = mean(current_stress_level,   na.rm = TRUE),
    q1  = quantile(current_stress_level, 0.25, na.rm = TRUE),
    q3  = quantile(current_stress_level, 0.75, na.rm = TRUE)
  )



##stress level and cause of death---------------------------------------------------------------------
cause_of_death_data <- selected.data %>%
  filter(!is.na(cause_of_death)) %>%
  pivot_longer(
    cols = c(early_stress_level, current_stress_level),
    names_to = "timepoint",
    values_to = "stress_level"
  ) %>%
  mutate(
    timepoint = recode(timepoint,
                       "early_stress_level"   = "Early stress",
                       "current_stress_level" = "Current stress"),
    timepoint = factor(timepoint, levels = c("Early stress", "Current stress"))
  )



##stress level and social demographic status----------------------------------------------------------
data_for_social <- selected.data %>%
  mutate(Geschlecht = factor(Geschlecht,
                             levels = c(1, 2, 3),
                             labels = c("Male", "Female", "divers")),
         age_group = cut(as.numeric(Alter),
                         breaks = c(0, 30, 45, 60, 120),
                         labels = c("≤30", "31–45", "46–60", "60+"),
                         right = TRUE),
         education = factor(Bildungsabschluss,
                            levels = c(1, 2, 3, 4, 5, 7),
                            labels = c("No school degree", "Primary/Lower secondary school", "Intermediate school", "High school", "Vocational training", "University degree"))) %>%
  select(early_stress_level, Geschlecht, age_group, education)

social_long <- data_for_social %>%
  pivot_longer(cols = c(Geschlecht, age_group, education),
               names_to = "variable",
               values_to = "category") %>%
  mutate(variable = factor(variable,
                           levels = c("Geschlecht", "age_group", "education"),
                           labels = c("Gender", "Age Group", "Education Level"))) %>%
  filter(!is.na(category))


##stress level and support received-------------------------------------------------------------------
data_for_support <- selected.data %>%
  filter(`F18 Betreuung unklar` != 2) %>%
  mutate(support = recode(support, `1` = "Without support", `2` = "With support")) %>% 
  mutate(stress_change = early_stress_level - current_stress_level)




###MEMBER2------------------------------------------------------------------------------------------------------------------------
#who informed genral
Who_informed_child_data <- d  %>%
  drop_na() %>%
  count(`How was the kid informed`) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2),
    Frequency  = n,
    `How was the kid informed` =
      fct_reorder(`How was the kid informed`, Frequency, .desc = TRUE)
  )

#who informed + cause of death
plot_data <-clean_data %>%
  count(`How was the kid informed`, cause_of_death)%>%
  group_by(`How was the kid informed`)%>%
  mutate(percent = n / sum(n) )

plot_data <- plot_data %>%
  ungroup() %>%
  mutate(
    `How was the kid informed`= factor(
      `How was the kid informed`,
      levels = c("Parent", "Police", "Self found", "Present", "Friend", "Grandparent", "Sibling", "Teacher")
    )
  )

#who informed + age
plot2.data <- clean_data %>%
  count(
    `How was the kid informed`,
    child_age_group = cut(
      child_age,
      breaks = c(0, 5, 11, 17),
      labels = c("0–5", "6–11", "12–17"),
      include.lowest = TRUE
    )
  ) %>%
  group_by(`How was the kid informed`) %>%
  mutate(percent = n / sum(n))

plot2.data$child_age_group <- factor(
  plot2.data$child_age_group,
  levels = c("0–5", "6–11", "12–17")
)

plot2.data <- plot2.data %>%
  mutate(`How was the kid informed` =
           factor(`How was the kid informed`,
                  levels = c("Parent", "Police", "Self found", "Present", "Friend", "Grandparent", "Sibling", "Teacher")))

#who recommended x who informed
plot_heat <-clean_data %>%
  filter(!is.na(`How was the kid informed`))%>%
  count(who_recommended, `How was the kid informed`) %>%
  group_by(who_recommended) %>%
  mutate(percent = n / sum(n))

plot_heat <- plot_heat %>%
  mutate(
    who_recommended = factor(
      who_recommended,
      levels = c("None","AETAS","Emergency Support", "Therapeutic", "Others")    
    ),
    `How was the kid informed` = factor(
      `How was the kid informed`,
      levels = c("Parent", "Grandparent", "Friend", "Sibling", "Teacher", "Police", "Self found", "Present")
    )
  )
plot_heat <- plot_heat %>%
  filter(!is.na(who_recommended),
         !is.na(`How was the kid informed`))


#second question
#how did they find the words?
plot_q2.1 <- clean_data %>%
  select(help_words, provider_words_other, which_info_source) %>%
  filter(
    !is.na(help_words),
    !is.na(provider_words_other),
    !is.na(which_info_source)
  ) %>%
  count(help_words, provider_words_other, which_info_source) %>%
  filter(n > 0) %>%
  mutate(
    provider_words_other = factor(
      provider_words_other,
      levels = c("AETAS", "Therapeutic", "Others", "None")
    ),
    which_info_source = factor(
      which_info_source, levels = c("Internet", "Literature", "Psychological/Medical counseling", "Multiple sources", "Other", "No Info" )
    ),
    help_words = if_else(
      provider_words_other %in% c("Help", "None"),
      "No Help",
      "Help"
    ),
    help_words = factor(help_words, levels = c("No Help", "Help"))
  )



#####MEMBER 3---------------------------------------------------------------------------------------------------------------
##plot1---------------------------------------------------------------------------------------------------

# 1) Reshape provider variables to long format (one row = one mention of support)
plot1_data <- selected.data %>%
  select(
    provider_words_main,
    provider_words_other,
    provider_watch,
    provider_good
  ) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "source",
    values_to = "provider_raw"
  )
plot1_data <- filter(
  plot1_data,
  !is.na(provider_raw) & provider_raw != ""
)


# 2) Classify providers into meaningful categories and label support domains
plot1_data <- plot1_data %>%
  mutate(
    # ---- Provider category (who gave the support?) ----
    provider_cat = case_when(
      # Closed codes from X003 (main provider for finding words)
      source == "provider_words_main" & provider_raw == "1"                ~ "PSNV / KIT",
      source == "provider_words_main" & provider_raw == "2"                ~ "Other person",
      source == "provider_words_main" & provider_raw %in% c("3","-1","-9") ~ "Unclassified",
      
      # Open text patterns – we search for keywords in the text
      str_detect(provider_raw, "(?i)aetas|tita kern|aetas kinder")                 ~ "AETAS",
      str_detect(provider_raw, "(?i)kit|notfallseelsorge|krisenintervention")      ~ "PSNV / KIT",
      str_detect(provider_raw, "(?i)psycholog|therapeut|kinderpsycholog|hebamme") ~ "Therapist / Health",
      str_detect(provider_raw, "(?i)verein der trauernden kinder|arche|primi passi") ~ "Other organisations",
      str_detect(provider_raw, "(?i)betroffene|selbst")                            ~ "Informal / Peer",
      TRUE ~ "Unclassified"
    ),
    
    # ---- Support domain (what kind of support?) ----
    support_domain = case_when(
      source %in% c("provider_words_main", "provider_words_other") ~ 
        "Support with finding words about the cause",
      source == "provider_watch" ~ 
        "Support about what to watch for in the child",
      source == "provider_good"  ~ 
        "Support about what is good/helpful for the child",
      TRUE ~ NA_character_
    )
  )
plot1_data<-filter(plot1_data,
                   !is.na(support_domain))




# 3) Count how many entries we could not classify into a provider category
unknown_count <- sum(plot1_data$provider_cat == "Unclassified", na.rm = TRUE)

# 4) Keep only classified providers and enforce a consistent order of domains and provider categories
plot1_filtered <- plot1_data %>%
  filter(!is.na(provider_cat) & provider_cat != "Unclassified") %>%   
  mutate(
    support_domain = factor(
      support_domain,
      levels = c(
        "Support with finding words about the cause",
        "Support about what to watch for in the child",
        "Support about what is good/helpful for the child"
      )
    ),
    provider_cat = factor(
      provider_cat,
      levels = c(
        "AETAS",
        "PSNV / KIT",
        "Therapist / Health",
        "Other organisations",
        "Other person",
        "Informal / Peer"
      )
    )
  )


##plot2--------------------------------------------------------------------------------------------------------
## 1) Build a long dataset: one row = one Parent x domain -----------------

plot2_data <- bind_rows(
  # ---- Domain 1: support with finding words about the cause (Q10) ----
  selected.data %>%
    transmute(
      domain      = "Finding words\nabout the cause",
      timing_code = as.numeric(timing_words),
      timely      = as.numeric(timely_words)
    ),
  
  # ---- Domain 2: support about what to watch for in the child (Q13) ----
  selected.data %>%
    transmute(
      domain      = "What to watch for\nin the childs´behaviour",
      timing_code = as.numeric(timing_watch),
      timely      = as.numeric(timely_watch)
    ),
  
  # ---- Domain 3: support about what is good/helpful for the child (Q32) ----
  selected.data %>%
    transmute(
      domain      = "What is good/helpful\nfor the child",
      timing_code = as.numeric(timing_good),
      timely      = as.numeric(timely_good)
    )
) %>%
  mutate(
    timing = case_when(
      timing_code == 1 ~ "Immediately",
      timing_code == 2 ~ "Within hours",
      timing_code == 3 ~ "Within days",
      timing_code == 4 ~ "Later",
      timing_code == 5 ~ "Not at all",
      TRUE             ~ NA_character_
    ),
    timely = ifelse(timely %in% c(-1, -9), NA_real_, timely)
  ) %>%
  filter(!is.na(timing), !is.na(timely)) %>%
  mutate(
    timing = factor(
      timing,
      levels = c("Immediately", "Within hours", "Within days", "Later", "Not at all")
    ),
    domain = factor(
      domain,
      levels = c(
        "Finding words\nabout the cause",
        "What to watch for\nin the childs´behaviour",
        "What is good/helpful\nfor the child"
      )
    )
  )


##plot3---------------------------------------------------------------------------------------------------------
# 1) Put all three domains into one long table:
#    one row = one caregiver x domain x (timely, helpful)
help_quality <- bind_rows(
  # ---- Domain 1: support with finding words about the cause ----
  selected.data %>%
    transmute(
      domain    = "Finding words\nabout the cause",
      help_rcvd = as.numeric(help_words),        # 1 = yes, 2 = no
      timely    = as.numeric(timely_words),      # 1–10 (rechtzeitig genug?)
      helpful   = as.numeric(helpful_words)      # 1–10 (hilfreich?)
    ),
  
  # ---- Domain 2: support about what to watch for in the child ----
  selected.data %>%
    transmute(
      domain    = "What to watch for\nin the childs´behaviour",
      help_rcvd = as.numeric(help_watch),
      timely    = as.numeric(timely_watch),
      helpful   = as.numeric(helpful_watch)
    ),
  
  # ---- Domain 3: support about what is good/helpful for the child ----
  selected.data %>%
    transmute(
      domain    = "What is good/helpful\nfor the child",
      help_rcvd = as.numeric(help_good),
      timely    = as.numeric(timely_good),
      helpful   = as.numeric(helpful_good)
    )
) %>%
  # Keep only caregivers who actually received support in that domain
  filter(help_rcvd == 1) %>%
  # Remove special codes -1 / -9 from the 1–10 scales
  mutate(
    timely  = ifelse(timely  %in% c(-1, -9), NA_real_, timely),
    helpful = ifelse(helpful %in% c(-1, -9), NA_real_, helpful)
  ) %>%
  # Drop rows with missing scores
  filter(!is.na(timely), !is.na(helpful)) %>%
  mutate(
    domain = factor(
      domain,
      levels = c(
        "Finding words\nabout the cause",
        "What to watch for\nin the childs´behaviour",
        "What is good/helpful\nfor the child"
      )
    )
  )


##plot4----------------------------------------------------------------------------------------
# 1) Long data: one row = one caregiver x domain, with help & wish
burden2_data <- bind_rows(
  # ---- Domain 1: finding words about the cause ----
  selected.data %>%
    transmute(
      domain = "Finding words\nabout the cause",
      help   = as.numeric(help_words),
      wish   = as.numeric(wish_words)
    ),
  # ---- Domain 2: what to watch for in the child ----
  selected.data %>%
    transmute(
      domain = "What to watch for\nin the childs´behaviour",
      help   = as.numeric(help_watch),
      wish   = as.numeric(wish_watch)
    ),
  # ---- Domain 3: what is good/helpful for the child ----
  selected.data %>%
    transmute(
      domain = "What is good/helpful\nfor the child",
      help   = as.numeric(help_good),
      wish   = as.numeric(wish_good)
    )
) %>%
  # classify support experience in each domain
  mutate(
    support_status = case_when(
      help == 1                    ~ "Support received",
      help == 2 & wish >= 7        ~ "Wanted support but did not receive",
      help == 2 & wish <  7        ~ "No support needed",
      TRUE                         ~ NA_character_
    )
  ) %>%
  filter(!is.na(support_status)) %>%   # drop missing / unclear
  mutate(
    domain = factor(
      domain,
      levels = c(
        "Finding words\nabout the cause",
        "What to watch for\nin the childs´behaviour",
        "What is good/helpful\nfor the child"
      )
    ),
    support_status = factor(
      support_status,
      levels = c(
        "No support needed",
        "Support received",
        "Wanted support but did not receive"
      )
    )
  )

# 2) Summaries per domain: convert to percentages
burden_summary <- burden2_data %>%
  group_by(domain, support_status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(domain) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()



#####MEMBER4-----------------------------------------------------------------------------------------------------------------------------------
#Preparation of intermediate summary tables for subsequent plots.
Stress <- cleaned_data %>% select(
  CASE,
  `mehrfach betroffen`,
  `F10 Todesursache`,
  `F11 Wie lange zurück`,
  `X039_01 Wie hoch eig. Belastung?`,
  `X039_02 Wie hoch jetzt?` ) %>% rename(
    "Early stress" = `X039_01 Wie hoch eig. Belastung?`,
    "Current stress" = `X039_02 Wie hoch jetzt?`,
    affected_multiple_times = `mehrfach betroffen`,
    how_long_ago = `F11 Wie lange zurück`,
    cause_of_death = `F10 Todesursache`
  )  %>% pivot_longer(
    cols = c("Early stress","Current stress"),
    names_to = "time",
    values_to = "stress") %>% drop_na() %>% mutate(
      affected_multiple_times = recode(affected_multiple_times,
                                       `1` = "Multiple",
                                       `2` = "Single"),
      cause_of_death = recode(cause_of_death,
                              `1` = "Suicide" ,
                              `2` = "Homicide")
    )

Received_support <- cleaned_data %>% 
  select(
    CASE,
    `F18 Betreuung Nein`,
    `F18 Betreuung Nennung`,
    `F18 Betreuung nach Ereignis`,
    `F19 Betreuung Belastung Kinder`,
    `F19 Betreuung eigene Belastung`,
    `F19 Betreuung organisatorisch`
  ) %>% 
  rename(
    Support = `F18 Betreuung Nein`,
    Support_name = `F18 Betreuung Nennung`,
    Support_after_accident = `F18 Betreuung nach Ereignis`
  ) %>%
  mutate(
    Support_after_accident = case_when(
      Support_after_accident == 1 ~ "No",
      Support_after_accident == 2 ~ "Yes",
      TRUE ~ as.character(Support_after_accident)
    ),
    Support = recode(
      Support,
      `1` = "Without support",
      `2` = "With support"
    )
  )

Information_seeking <- cleaned_data %>% select(
  CASE,
  `X047 andere Infoquellen?`,
  `X048_01 wenn ja, welche?`,
  `X049 wann?`,
  `X050_01 wie hilfreich?`,
  `X051_01 wenn nein, was abgehalten?`,
  `X052_01 rechtzeitig?`
)

Coping_guidance <- cleaned_data %>% select(
  CASE,
  `X030_01 Tipps eig. Belastung- rechtzeitig?`,
  `X030_02 Tipps eig. Belastung -hilfreich?`,
  `X020_01_ Tipps eig. Belastung - Wer?`,
  `X025 Tipps Umgang eig. Belastung - wann?`,
  `X043 Tips eig. Belastung`,
  `X034_01 Tipps eig. Belastung_ wenn nein, gewünscht?`
)

Advice_for_child_needs <- cleaned_data %>% select(
  CASE,`X017 was gut &hilfreich?`,
  `X018_01 Erklärt gut & hilfreich für Kind. Wer?`,
  `X023 Erklärt was gut & hilfreich - wann?`,
  `X028_01 Erklärung gut & hilfreich - rechtzeitig?`,
  `X028_02 Erklärung gut & hilfreich - Hilfreich?`,
  `X033_01 Erklärung gut & hilfreich_wenn nein, gewünscht?`
)


Wished_support <- cleaned_data %>% select(
  CASE,
  `X044_01 Wunsch für gute Versorgung?`,
  `X051_01 wenn nein, was abgehalten?`,
  `NF16_01 Wenn nein, Wunsch?`,
  `N116_01 Wenn nein, Wunsch?`
)

Other_feedback <- cleaned_data %>% select(
  CASE,
  `X040 Was noch hilfreich?`,
  `X040_01 wenn ja, was?`,
  `X041 was nicht hilfreich?`,
  `X041_01 was?`)

stress_information_seeking_data <- Stress %>% 
  left_join(Information_seeking) %>% 
  select(
    CASE,
    time,
    stress,
    `X048_01 wenn ja, welche?`,
    how_long_ago,
    `X047 andere Infoquellen?`
  ) %>% 
  mutate(
    Inf_seeking = recode(
      `X047 andere Infoquellen?`, 
      `1` = "YES",
      `2` = "NO"
    ),
    time = factor(time, levels = c("Early stress", "Current stress")),
    `X048_01 wenn ja, welche?` = ifelse(
      is.na(`X048_01 wenn ja, welche?`), 
      "Nothing", 
      `X048_01 wenn ja, welche?`
    ),
    stress_cat = cut(
      stress,
      breaks = c(-Inf, 4, 7, 10),
      labels = c(
        "Low Stress Level", 
        "Moderate Stress Level", 
        "High Stress Level"
      )
    )
  ) %>% 
  drop_na()



#--------------------------------------------------------------------
#4:answer the question using plots
#• Welche Hilfe und Unterstützung hätten Sie sich eventuell gewünscht?
#
#Schwerpunkt:
#Informationsverhalten und Bewältigung:
#Untersuchung des Zusammenhangs zwischen Stressniveau, psychosozialer Unterstützung und dem 
#Suchverhalten nach Informationen sowie dem Erhalt von Bewältigungstipps. 
#-------------------------------------------------------------------------------------------
#plotting 
#-------------------------------------------------------------------------------------------
##Data for wished support vs stress plots
Wished_Support_data <- Wished_support %>% select(
  `X044_01 Wunsch für gute Versorgung?`
)  %>% drop_na() %>% group_by(
  `X044_01 Wunsch für gute Versorgung?`) %>% count() %>% arrange(n) %>% rename(
    "wished support" = `X044_01 Wunsch für gute Versorgung?`
  )

Wished_support_vs_stress_data <- Stress %>% left_join(
  Wished_support
) %>% select(
  CASE,
  time,
  stress,
  `X044_01 Wunsch für gute Versorgung?`
) %>% pivot_wider(
  names_from = time,
  values_from = stress
) %>% drop_na()

medians_Now <- tapply(
  Wished_support_vs_stress_data$`Current stress`,
  Wished_support_vs_stress_data$`X044_01 Wunsch für gute Versorgung?`,
  median,
  na.rm = TRUE
)

Wished_support_vs_stress_data$`X044_01 Wunsch für gute Versorgung?` <- factor(Wished_support_vs_stress_data$`X044_01 Wunsch für gute Versorgung?`,
                                                                              levels = names(sort(medians_Now)))
medians_then <- tapply(
  Wished_support_vs_stress_data$`Early stress`,
  Wished_support_vs_stress_data$`X044_01 Wunsch für gute Versorgung?`,
  median,
  na.rm = TRUE
)

Wished_support_vs_stress_data$`X044_01 Wunsch für gute Versorgung?` <- factor(Wished_support_vs_stress_data$`X044_01 Wunsch für gute Versorgung?`,
                                                                              levels = names(sort(medians_then)))

wished_stress_long <- Wished_support_vs_stress_data %>%
  pivot_longer(
    cols = c("Early stress", "Current stress"),
    names_to = "Time",
    values_to = "Stress"
  )


wished_stress_long$Time <- factor(wished_stress_long$Time, levels = c("Early stress", "Current stress"))

#--------------------------------------------------------------------------------------------
##Data for support vs stress plots
Stress_level_vs_Support_data <- stress_information_seeking_data %>% 
  left_join(Received_support)

Stress_level_vs_Support_mean_data <- stress_information_seeking_data %>% 
  pivot_wider(
    names_from = time,
    values_from = stress
  ) %>% 
  left_join(Received_support, by = "CASE") %>% 
  select(
    CASE,
    Support,
    Inf_seeking,
    "Early stress",
    "Current stress") %>% 
  group_by(Support) %>% 
  drop_na() %>% 
  summarise(
    Mean_Then = mean("Early stress", na.rm = TRUE),
    Mean_Now = mean("Current stress", na.rm = TRUE)
  ) %>% 
  mutate(
    Mean_Then = round(Mean_Then, 2),
    Mean_Now = round(Mean_Now, 2)
  )

Type_Of_Support_VS._Stress_data <- Received_support %>% select(
  CASE,
  `F19 Betreuung Belastung Kinder`,
  `F19 Betreuung eigene Belastung`,
  `F19 Betreuung organisatorisch`
) %>% left_join(
  Stress) %>% rename (
    "Organisational issues" = `F19 Betreuung organisatorisch`,
    "Own emotional burden" = `F19 Betreuung eigene Belastung`,
    "Child´s emotional burden" = `F19 Betreuung Belastung Kinder`
  ) %>%pivot_longer(
    cols = c(`Organisational issues`, `Own emotional burden`, `Child´s emotional burden`),
    names_to = "Type",
    values_to = "Intensity"
  ) %>% select(
    CASE,
    stress,
    time,
    Type,
    Intensity
  ) %>% drop_na() %>% mutate(
    Intensity = Intensity - 1,
    Support_Group = cut(
      Intensity,
      breaks = c(- Inf, 33, 66, 100),
      labels = c("Low", "Moderate", "High")
    )
  )
#--------------------------------------------------------------------------------------------
##Data for information seeking vs stress plots
Information_Seeking_vs_Stress_Now_data <- Stress %>%
  left_join(Information_seeking) %>% 
  select(
    CASE,
    time,
    stress,
    `X048_01 wenn ja, welche?`,
    how_long_ago,
    `X047 andere Infoquellen?`
  ) %>% 
  mutate(
    Inf_seeking = recode(
      `X047 andere Infoquellen?`, 
      `1` = "YES",
      `2` = "NO"
    ),
    time = factor(time, levels = c("Early stress", "Current stress")),
    `X048_01 wenn ja, welche?` = ifelse(
      is.na(`X048_01 wenn ja, welche?`), 
      "Nothing", 
      `X048_01 wenn ja, welche?`
    ),
    stress_cat = cut(
      stress,
      breaks = c(-Inf, 4, 7, 10),
      labels = c(
        "Low Stress Level", 
        "Moderate Stress Level", 
        "High Stress Level"
      )
    )
  ) %>% 
  rename(Source = `X048_01 wenn ja, welche?`) %>% 
  drop_na() %>% 
  mutate(
    Source = fct_infreq(Source)
  ) %>% 
  filter(time == "Current stress")

Information_Seeking_vs_Stress_Then_data <- Stress %>%
  left_join(Information_seeking) %>% 
  select(
    CASE,
    time,
    stress,
    `X048_01 wenn ja, welche?`,
    how_long_ago,
    `X047 andere Infoquellen?`
  ) %>% 
  mutate(
    Inf_seeking = recode(
      `X047 andere Infoquellen?`, 
      `1` = "YES",
      `2` = "NO"
    ),
    time = factor(time, levels = c("Early stress", "Current stress")),
    `X048_01 wenn ja, welche?` = ifelse(
      is.na(`X048_01 wenn ja, welche?`), 
      "Nothing", 
      `X048_01 wenn ja, welche?`
    ),
    stress_cat = cut(
      stress,
      breaks = c(-Inf, 4, 7, 10),
      labels = c(
        "Low Stress Level", 
        "Moderate Stress Level", 
        "High Stress Level"
      )
    )
  ) %>% 
  rename(Source = `X048_01 wenn ja, welche?`) %>% 
  drop_na() %>% 
  mutate(
    Source = fct_infreq(Source)
  ) %>% 
  filter(time == "Early stress")
plot_combined <- bind_rows(
  Information_Seeking_vs_Stress_Then_data %>% mutate(Time = "Early stress"),
  Information_Seeking_vs_Stress_Now_data  %>% mutate(Time = "Current stress")
) %>% drop_na()


plot_combined$Time <- factor(plot_combined$Time, levels = c("Early stress", "Current stress"))
#--------------------------------------------------------------------------------------------
##Data for coping guidance vs stress plots
Stress_Levels_With_Coping_Guidance_data <- Stress %>% left_join(
  Coping_guidance,
  by = "CASE"
) %>% filter(
  `X043 Tips eig. Belastung` == 1
) %>% select(
  CASE,
  time,
  stress,
  how_long_ago
) %>% mutate(
  time_group = case_when(
    how_long_ago %in% c(1,2) ~ "0-1 Year",
    how_long_ago %in% c(3,4,5) ~ "2-4 Years",
    how_long_ago %in% c(6,7,8,9) ~ "5-9 Years",
    how_long_ago %in% c(10,11,12) ~ "10+ Years"
  ),
  time_group = factor(time_group , levels = c ("0-1 Year", "2-4 Years", "5-9 Years", "10+ Years"))
) %>% 
  filter(time == "Current stress")

Stress_Levels_Without_Coping_Guidance_data <- Stress %>% left_join(
  Coping_guidance,
  by = "CASE"
)%>% filter(
  `X043 Tips eig. Belastung` != 1
) %>% select(
  CASE,
  time,
  stress,
  how_long_ago
) %>% mutate(
  time_group = case_when(
    how_long_ago %in% c(1,2) ~ "0-1 Year",
    how_long_ago %in% c(3,4,5) ~ "2-4 Years",
    how_long_ago %in% c(6,7,8,9) ~ "5-9 Years",
    how_long_ago %in% c(10,11,12) ~ "10+ Years"
  ),
  time_group = factor(time_group , levels = c ("0-1 Year", "2-4 Years", "5-9 Years", "10+ Years"))
) %>% 
  filter(time == "Current stress")
stress_combined <- bind_rows(
  Stress_Levels_Without_Coping_Guidance_data %>% mutate(Coping = "Without Guidance"),
  Stress_Levels_With_Coping_Guidance_data    %>% mutate(Coping = "With Guidance")
)

stress_combined$Coping <- factor(stress_combined$Coping, levels = c("Without Guidance", "With Guidance"))

