##importing raw data
raw.data <- read_excel("data/raw/AETAS - BereinigterDatensatz2511.xlsx")
##-------------------------------------------------------------------------------------------------------------------------
## --- Member 1 & 3: Variable selection and cleaning ----------------------------
# This section extracts the variables assigned to member 1 an member 3 from the survey dataset.
#extracting the needed columns
selected.data <- raw.data %>%
  select(c(CASE, "Alter","Geschlecht","Bildungsabschluss", "Stadt", "Land", "Deutschland", "Österreich",
           "Schweiz", "anderes Land", "Nennung anderes Land", "F9 Verhältnis Verstorbene", "F9 anderes Verhältnis",
           "F10 Todesursache", "F8 eigenes Verhältnis zum Kind", "F8 anderes Verhältnis",
           "F11 Wie lange zurück", "F12 Erfahren von Tod", "F12 erfahren von wem", "F12 Sonstiges",
           "F18 Betreuung nach Ereignis", "F18 Betreuung ja", "F18 Betreuung durch wen", "F18 Betreuung Nennung",
           "F18 Betreuung Nein", "F18 Betreuung unklar", "F19 Betreuung organisatorisch", "F19 Betreuung eigene Belastung",
           "F19 Betreuung Belastung Kinder", "F13 Alter Kind_1", "F15_K1 wie erfahren", "F15.2_K1 Empfehlung Überbringen",
           "F15.2_K1 ja", "F15.2_K1 Wer", "F15.2_K1 Wer Eingabe", "F15.2_K1 Nein", "F15.2_K1 weiß nicht",
           "F17.2_K1_Empfehlung ob & von wem?", "F17.2_K1 Ja", "F17.2_K1 Wer", "F17.2_K1 Nennung", "F17.2_K1 Nein",
           "F17.2_K1 weiß nicht", "X043 Tips eig. Belastung", "X025 Tipps Umgang eig. Belastung - wann?",
           "X030_01 Tipps eig. Belastung- rechtzeitig?", "X030_02 Tipps eig. Belastung -hilfreich?",
           "X034_01 Tipps eig. Belastung_ wenn nein, gewünscht?", "X039_01 Wie hoch eig. Belastung?",
           "X039_02 Wie hoch jetzt?", "X040 Was noch hilfreich?","X040_01 wenn ja, was?",
           "X041 was nicht hilfreich?", "X041_01 was?", "X044_01 Wunsch für gute Versorgung?",
           `X011_01 Hilfe konkrete Worte - wenn nein, gewünscht?`,
           `X014_01 Erklärung, worauf achten - Wenn nein, gewünscht?`,
           `X033_01 Erklärung gut & hilfreich_wenn nein, gewünscht?`,
           
           # --- Help finding words ---
           `X007 Hilfe entsprechnder Worte (Todesursache)`,
           `X003 Hilfe entsprechender Worte - Wenn ja, wer?`,
           `X003_02 Andere Person`,
           `X015 Hilfe konkrete Worte Todesursache - wann?`,
           `X016_02 Hilfe konkrete Worte -hilfreich?`,
           `X016_01 Hilfe konkrete Worte - rechtzeitig?`,
           
           # --- Explain what to watch for ---
           `X012 Worauf achten?`,
           `X042_01 Worauf konkret achten - Wer?`,
           `X009 Worauf achten bei Kind - wann?`,
           `X010_01 Erklärung konkret achten - rechtzeitig genug?`,
           `X010_02_Erklärung konkret achten - hilfreich?`,
           
           # --- Explain what is good/helpful ---
           `X017 was gut &hilfreich?`,
           `X018_01 Erklärt gut & hilfreich für Kind. Wer?`,
           `X023 Erklärt was gut & hilfreich - wann?`,
           `X028_01 Erklärung gut & hilfreich - rechtzeitig?`,
           `X028_02 Erklärung gut & hilfreich - Hilfreich?`))



#cleaning the data------------------------------------------------------------------------------------
selected.data <- selected.data %>%
  slice(-1)
selected.data <- selected.data %>%
  rename(early_stress_level = `X039_01 Wie hoch eig. Belastung?`,
         current_stress_level = `X039_02 Wie hoch jetzt?`,
         support = `F18 Betreuung ja`,
         cause_of_death = `F10 Todesursache`,
         wish_words = `X011_01 Hilfe konkrete Worte - wenn nein, gewünscht?`,
         wish_watch = `X014_01 Erklärung, worauf achten - Wenn nein, gewünscht?`,
         wish_good  = `X033_01 Erklärung gut & hilfreich_wenn nein, gewünscht?`,
         
         # --- Help finding words (Todesursache) ---
         help_words = `X007 Hilfe entsprechnder Worte (Todesursache)`,
         provider_words_main = `X003 Hilfe entsprechender Worte - Wenn ja, wer?`,
         provider_words_other = `X003_02 Andere Person`,
         timing_words = `X015 Hilfe konkrete Worte Todesursache - wann?`,
         helpful_words = `X016_02 Hilfe konkrete Worte -hilfreich?`,
         timely_words = `X016_01 Hilfe konkrete Worte - rechtzeitig?`,
         
         # --- Explain what to watch for (warning signs) ---
         help_watch = `X012 Worauf achten?`,
         provider_watch = `X042_01 Worauf konkret achten - Wer?`,
         timing_watch = `X009 Worauf achten bei Kind - wann?`,
         timely_watch = `X010_01 Erklärung konkret achten - rechtzeitig genug?`,
         helpful_watch = `X010_02_Erklärung konkret achten - hilfreich?`,
         
         # --- Explain what is good/helpful for the child ---
         help_good = `X017 was gut &hilfreich?`,
         provider_good = `X018_01 Erklärt gut & hilfreich für Kind. Wer?`,
         timing_good = `X023 Erklärt was gut & hilfreich - wann?`,
         timely_good = `X028_01 Erklärung gut & hilfreich - rechtzeitig?`,
         helpful_good = `X028_02 Erklärung gut & hilfreich - Hilfreich?`)

selected.data <- selected.data %>%
  mutate(cause_of_death = recode(cause_of_death, `1` = "Suicide", `2` = "Homicide"))



## --- Member 4: Variable selection and cleaning ----------------------------
# This section extracts the variables assigned to member 4 from the survey dataset.
# IMPORTANT: The cleaned variables produced here are reused in the subsequent
# Member 2 block
cleaned_data <- raw.data
cleaned_data$STARTED <-as.POSIXct(
  (cleaned_data$STARTED - 25569) * 86400,
  origin = "1970-01-01",
  tz = "UTC")

#extract the relevant variables for Information-Seeking & Coping
cleaned_data <- cleaned_data[-1,] %>% select(
  CASE,
  STARTED,
  Alter,
  Geschlecht,
  Bildungsabschluss,
  Deutschland,
  Bezugsperson,
  `mehrfach betroffen`,
  `F8 eigenes Verhältnis zum Kind`,
  `F8 anderes Verhältnis`,
  `F10 Todesursache`,
  `F11 Wie lange zurück`,
  `F12 erfahren von wem`,
  `F12 Sonstiges`,
  `F15_K1 wie erfahren`,
  `F15_K1 andere`,
  `F15_K1 Sonstiges`,
  `F18 Betreuung ja`,
  `F18 Betreuung Nein`,
  `F18 Betreuung unklar`,
  `F18 Betreuung durch wen`,
  `F18 Betreuung nach Ereignis`,
  `F18 Betreuung Nennung`,
  `F19 Betreuung Belastung Kinder`,
  `F19 Betreuung eigene Belastung`,
  `F19 Betreuung organisatorisch`,
  `F8 eigenes Verhältnis zum Kind`,
  `F8 anderes Verhältnis`,
  `F9 Verhältnis Verstorbene`,
  `F9 anderes Verhältnis`,
  `NF16_01 Wenn nein, Wunsch?`,
  `N116_01 Wenn nein, Wunsch?`,
  `X003_02 Andere Person`,
  `X007 Hilfe entsprechnder Worte (Todesursache)`,
  `X017 was gut &hilfreich?`,
  `X018_01 Erklärt gut & hilfreich für Kind. Wer?`,
  `X023 Erklärt was gut & hilfreich - wann?`,
  `X020_01_ Tipps eig. Belastung - Wer?`,
  `X025 Tipps Umgang eig. Belastung - wann?`,
  `X028_01 Erklärung gut & hilfreich - rechtzeitig?`,
  `X028_02 Erklärung gut & hilfreich - Hilfreich?`,
  `X030_01 Tipps eig. Belastung- rechtzeitig?`,
  `X030_02 Tipps eig. Belastung -hilfreich?`,
  `X033_01 Erklärung gut & hilfreich_wenn nein, gewünscht?`,
  `X034_01 Tipps eig. Belastung_ wenn nein, gewünscht?`,
  `X039_01 Wie hoch eig. Belastung?`,
  `X039_02 Wie hoch jetzt?`,
  `X040 Was noch hilfreich?`,
  `X040_01 wenn ja, was?`,
  `X041 was nicht hilfreich?`,
  `X041_01 was?`,
  `X043 Tips eig. Belastung`,
  `X044_01 Wunsch für gute Versorgung?`,
  `X047 andere Infoquellen?`,
  `X048_01 wenn ja, welche?`,
  `X049 wann?`,
  `X050_01 wie hilfreich?`,
  `X051_01 wenn nein, was abgehalten?`,
  `X052_01 rechtzeitig?`) %>% rename(
    Land = Deutschland)

#clean data                                                         
#standardize columns                                                  
#merging all spelling variants/descriptions into a unified form/label
cleaned_data <- cleaned_data %>% mutate(across(c(
  `F8 anderes Verhältnis`,
  `F9 anderes Verhältnis`,
  `F12 erfahren von wem`,
  `X020_01_ Tipps eig. Belastung - Wer?`,
  `X048_01 wenn ja, welche?`,
  `X044_01 Wunsch für gute Versorgung?`,
  `X051_01 wenn nein, was abgehalten?`,
  `F18 Betreuung Nennung`,
  `X018_01 Erklärt gut & hilfreich für Kind. Wer?`), tolower)
) 

cleaned_data <- cleaned_data %>%mutate(
  `F8 anderes Verhältnis` = case_when(
    str_detect(`F8 anderes Verhältnis`, regex("freund", ignore_case = TRUE)) ~ "Freunde",
    str_detect(`F8 anderes Verhältnis`, regex("tante", ignore_case = TRUE)) ~ "Tante",
    TRUE ~ `F8 anderes Verhältnis`
  ),
  
  `F9 anderes Verhältnis` = case_when(
    str_detect(`F9 anderes Verhältnis`, regex("bekannte der familie|eltern bester freund", ignore_case = TRUE)) ~ "Bekannte_Der_Famlie",
    str_detect(`F9 anderes Verhältnis`, regex("cousin|cousine", ignore_case = TRUE)) ~ "Cousin-e",
    str_detect(`F9 anderes Verhältnis`, regex("freunde", ignore_case = TRUE)) ~ "Freunde",
    str_detect(`F9 anderes Verhältnis`, regex("freund|freundin", ignore_case = TRUE)) ~ "Freund-in",
    str_detect(`F9 anderes Verhältnis`, regex("mitschüler|mitschülerin", ignore_case = TRUE)) ~ "Mitschüler-in",
    str_detect(`F9 anderes Verhältnis`, regex("onkel", ignore_case = TRUE)) ~ "Onkel",
    str_detect(`F9 anderes Verhältnis`, regex("tante", ignore_case = TRUE)) ~ "Tante",
    TRUE ~ `F9 anderes Verhältnis`
  ),
  
  `F12 erfahren von wem` = case_when(
    str_detect(`F12 erfahren von wem`, regex(" von einem guten freund", ignore_case = TRUE)) ~ "Freunde",
    str_detect(`F12 erfahren von wem`, regex("anruf", ignore_case = TRUE)) ~ "Telefon",
    str_detect(`F12 erfahren von wem`, regex("caritas", ignore_case = TRUE)) ~ "Caritas",
    str_detect(`F12 erfahren von wem`, regex("dermutter der getöteten kinder|mutter des verstorbenen", ignore_case = TRUE)) ~ "Opfers_Familie",
    str_detect(`F12 erfahren von wem`, regex("familie", ignore_case = TRUE)) ~ "Familie",
    str_detect(`F12 erfahren von wem`, regex("polizei|kriminalpolizei|kripo|polizisten|poizeil|polizeibeamten|wohnung versiegelt", ignore_case = TRUE)) ~ "Police",
    str_detect(`F12 erfahren von wem`, regex("notfallseelsorge", ignore_case = TRUE)) ~ "Notfallseelsorge",
    str_detect(`F12 erfahren von wem`, regex("nachbarschaft", ignore_case = TRUE)) ~ "Nachbarschaft",
    str_detect(`F12 erfahren von wem`, regex("partnerin", ignore_case = TRUE)) ~ "Partner-in",
    str_detect(`F12 erfahren von wem`, regex("mitschüler|mitschülerin", ignore_case = TRUE)) ~ "Mitschüler-in",
    str_detect(`F12 erfahren von wem`, regex("klinikpsychologin", ignore_case = TRUE)) ~ "Klinikpsychologin",
    str_detect(`F12 erfahren von wem`, regex("meinem bruder|vater|meinen töchtern|meiner tante|schwager|schwägerin|oma der kinder|mutter|meine Eltern", ignore_case = TRUE)) ~ "Bezugpersons_Familie",
    str_detect(`F12 Sonstiges`, regex("kit", ignore_case = TRUE)) ~ "KIT",
    TRUE ~ `F12 erfahren von wem`
  ),
  
  `F12 Sonstiges` = case_when(
    str_detect(`F12 Sonstiges`, regex("Indizienprozess", ignore_case = TRUE)) ~ "Gericht",
  ),
  
  `X020_01_ Tipps eig. Belastung - Wer?` = case_when(
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("aetas|aeteas|tita kern", ignore_case = TRUE)) ~ "AETAS",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("andere suizid-betroffene", ignore_case = TRUE)) ~ "Andere_Suizid-betroffene",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("arche", ignore_case = TRUE)) ~ "",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("diverse", ignore_case = TRUE)) ~ "Diverse",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("freunde", ignore_case = TRUE)) ~ "Freunde",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("kit|krisendienst|krisen dienst", ignore_case = TRUE)) ~ "KIT",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("schwägerin", ignore_case = TRUE)) ~ "Familie",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("notfallseelsorger", ignore_case = TRUE)) ~ "Notfallseelsorge",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("polizei", ignore_case = TRUE)) ~ "Police",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("psychologe", ignore_case = TRUE)) ~ "Psychologe-in",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("psychotherapeutin|therapeutin", ignore_case = TRUE)) ~ "Psychotherapeutin",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("trauerbegleiterin", ignore_case = TRUE)) ~ "Trauerbegleiter-in",
    str_detect(`X020_01_ Tipps eig. Belastung - Wer?`, regex("verein der trauernden kinder", ignore_case = TRUE)) ~ "Verein_Der_Trauernden_Kinder",
    TRUE ~ `X020_01_ Tipps eig. Belastung - Wer?`
  ),
  
  `X044_01 Wunsch für gute Versorgung?` = case_when(
    str_detect(`X044_01 Wunsch für gute Versorgung?`, regex("fokus lag ausschließlich auf den kindern|trauernde menschen im stich gelassen|keine trauergruppe|jemand, der mich", ignore_case = TRUE)) ~ "More focus on the primary caregiver",
    str_detect(`X044_01 Wunsch für gute Versorgung?`, regex("regelmäßige ambulante unterstützung|war sie alleine da und haben erst vor Ort|sobald wie möglich unterstützen", ignore_case = TRUE)) ~ "Immediate and regular support",
    str_detect(`X044_01 Wunsch für gute Versorgung?`, regex("direkt über|noch frühere info über aetas|erstes gespräch mit aetas sofort|vermittlung an selbsthilfegruppe|viel mehr aufklärung und konkrete nennung", ignore_case = TRUE)) ~ "Support recommendation",
    str_detect(`X044_01 Wunsch für gute Versorgung?`, regex("direkten beistand bei überbringung der todesnachricht|direkter kontakt zum lebenden geschwisterkind|betroffene geschwister", ignore_case = TRUE)) ~ "Sibling support",
    str_detect(`X044_01 Wunsch für gute Versorgung?`, regex("die anschlussbetreuung|ein netzwerk,|nummern für psychiatrische", ignore_case = TRUE)) ~ "Coordination with therapists",
    str_detect(`X044_01 Wunsch für gute Versorgung?`, regex("papierkramm|unterstützung bei allen organisatorische", ignore_case = TRUE)) ~ "Administrative support",
    str_detect(`X044_01 Wunsch für gute Versorgung?`, regex("persönlichen ansprechpartne|schriftliche informationen, telefonnummern ", ignore_case = TRUE)) ~ "Contact persons",
    str_detect(`X044_01 Wunsch für gute Versorgung?`, regex("schnelle telefonische erreichbarkeit", ignore_case = TRUE)) ~ "Telephone availability",
    str_detect(`X044_01 Wunsch für gute Versorgung?`, regex(" kind einen platz", ignore_case = TRUE)) ~ "Support for the kid",
  ),
  
  `X048_01 wenn ja, welche?` = case_when(
    str_detect(`X048_01 wenn ja, welche?`, regex("aetas", ignore_case = TRUE)) ~ "AETAS",
    str_detect(`X048_01 wenn ja, welche?`, regex("bücher|literatur", ignore_case = TRUE)) ~ "Literatur",
    str_detect(`X048_01 wenn ja, welche?`, regex("kindergärtnerin", ignore_case = TRUE)) ~ "Kindergarten",
    str_detect(`X048_01 wenn ja, welche?`, regex("psychologische beratung", ignore_case = TRUE)) ~ "Psychological advice",
    str_detect(`X048_01 wenn ja, welche?`, regex("mein sohn war damals 19", ignore_case = TRUE)) ~ "Mother",
    str_detect(`X048_01 wenn ja, welche?`, regex("ibternet|internet", ignore_case = TRUE)) ~ "Internet",
    str_detect(`X048_01 wenn ja, welche?`, regex("gespräche ", ignore_case = TRUE)) ~ "Discussions",
    str_detect(`X048_01 wenn ja, welche?`, regex("pfarrer", ignore_case = TRUE)) ~ "Pope",
    str_detect(`X048_01 wenn ja, welche?`, regex("neurologen", ignore_case = TRUE)) ~ "Victims´doctor",
    TRUE ~ `X048_01 wenn ja, welche?`
  ),
  
  `X051_01 wenn nein, was abgehalten?` = case_when(
    str_detect(`X051_01 wenn nein, was abgehalten?`, regex("eigene trauer|schock|schockzustand|keine kapazitäten|phychisch nicht in der lage|trauer", ignore_case = TRUE)) ~ "Überlastung",
    str_detect(`X051_01 wenn nein, was abgehalten?`, regex(" mich gut betreut|unserer betreuerin|gut betreut|bestens erklärt", ignore_case = TRUE)) ~ "Gute_Betreuung",
    str_detect(`X051_01 wenn nein, was abgehalten?`, regex(" wollte nicht", ignore_case = TRUE)) ~ "Vermeidungsverhalten_des_Kindes",
    str_detect(`X051_01 wenn nein, was abgehalten?`, regex("ich war selbst das kind", ignore_case = TRUE)) ~ "Zu_jung",
    str_detect(`X051_01 wenn nein, was abgehalten?`, regex("meine eigene lebenserfahrung und hintergrund", ignore_case = TRUE)) ~ "Kein_Bedürfnis",
    str_detect(`X051_01 wenn nein, was abgehalten?`, regex("unfrage funktioniert nicht", ignore_case = TRUE)) ~ "Systemproblem",
  ),
  
  `X018_01 Erklärt gut & hilfreich für Kind. Wer?`= case_when(
    str_detect(`X018_01 Erklärt gut & hilfreich für Kind. Wer?`, regex("arche", ignore_case = TRUE)) ~ "Arche",
    str_detect(`X018_01 Erklärt gut & hilfreich für Kind. Wer?`, regex("aetas", ignore_case = TRUE)) ~ "AETAS",
    str_detect(`X018_01 Erklärt gut & hilfreich für Kind. Wer?`, regex("psychotherapeut|therapeutin", ignore_case = TRUE)) ~ "Psychotherapeut-in",
    str_detect(`X018_01 Erklärt gut & hilfreich für Kind. Wer?`, regex("psychologe", ignore_case = TRUE)) ~ "Psychologe-in",
    str_detect(`X018_01 Erklärt gut & hilfreich für Kind. Wer?`, regex("kit", ignore_case = TRUE)) ~ "KIT",
    str_detect(`X018_01 Erklärt gut & hilfreich für Kind. Wer?`, regex("hebamme", ignore_case = TRUE)) ~ "Hebamme",
    str_detect(`X018_01 Erklärt gut & hilfreich für Kind. Wer?`, regex("notfallseelsorger", ignore_case = TRUE)) ~ "Notfallseelsorge",
    str_detect(`X018_01 Erklärt gut & hilfreich für Kind. Wer?`, regex("verein der trauernden kinder", ignore_case = TRUE)) ~ "Verein_Der_Trauernden_Kinder",
  ),
  
  `F18 Betreuung Nennung` = case_when(
    str_detect(`F18 Betreuung Nennung`, regex("aetas", ignore_case = TRUE)) ~ "AETAS",
    str_detect(`F18 Betreuung Nennung`, regex("kit|krisenintervention|krisen intervention|krisenintrventionsteam|krisenitervention", ignore_case = TRUE)) ~ "KIT",
    str_detect(`F18 Betreuung Nennung`, regex("hebamme", ignore_case = TRUE)) ~ "Hebamme",
    str_detect(`F18 Betreuung Nennung`, regex("notfall seelsorge|notfallseelsorge", ignore_case = TRUE)) ~ "Notfallseelsorge",
    str_detect(`F18 Betreuung Nennung`, regex("psychologen", ignore_case = TRUE)) ~ "Psychologen",
    str_detect(`F18 Betreuung Nennung`, regex("arbeiter-samariter-bund", ignore_case = TRUE)) ~ "ASB",
    str_detect(`F18 Betreuung Nennung`, regex("arche", ignore_case = TRUE)) ~ "Arche",
  )) %>% mutate(
    `F18 Betreuung Nein` = ifelse(
      is.na(`F18 Betreuung Nennung`),
      1,
      2)
  )


## --- Member 2: Variable selection and cleaning ----------------------------
# This section extracts the variables assigned to member 2 from the survey dataset.
# This section partly relies on variables cleaned in the Member 4 section.
clean_data <- raw.data %>%
  slice(-1) %>% 
  select(
    CASE, `F15_K1 wie erfahren`, `F15_K1 andere`,`F13 Alter Kind_1`,
    `F15.2_K1 ja`, `F15.2_K1 Wer Eingabe`,`F15.2_K1 Nein`,`F10 Todesursache`,
    `X007 Hilfe entsprechnder Worte (Todesursache)`, `X003 Hilfe entsprechender Worte - Wenn ja, wer?`,`X003_02 Andere Person`,
    `X015 Hilfe konkrete Worte Todesursache - wann?`,
    `X047 andere Infoquellen?`, `X048_01 wenn ja, welche?`, no_recommendation = `F15.2_K1 Nein`
    
  )
clean_data <- clean_data %>%
  rename(child_informed_by = `F15_K1 wie erfahren`, other_informers = `F15_K1 andere`,
         child_age = `F13 Alter Kind_1`, Recommendation = `F15.2_K1 ja`,who_recommended = `F15.2_K1 Wer Eingabe`)


clean_data <- clean_data %>%
  mutate(child_informed_by = recode(child_informed_by, "1" = "Self found" , "2" = "Present" , "3" = "Parent", "4"= "Others"))



clean_data <- clean_data %>%
  rename (cause_of_death ='F10 Todesursache')
clean_data <- clean_data %>%
  mutate(cause_of_death =recode (cause_of_death, '1' = "Suicide", '2' = "Homicide"))


clean_data <- clean_data %>%
  rename(help_words = `X007 Hilfe entsprechnder Worte (Todesursache)` ,
         provider_words_main = `X003 Hilfe entsprechender Worte - Wenn ja, wer?`,
         provider_words_other = `X003_02 Andere Person`, help_timing = `X015 Hilfe konkrete Worte Todesursache - wann?`, other_information_sources = `X047 andere Infoquellen?`,
         which_info_source = `X048_01 wenn ja, welche?`)

clean_data <- clean_data %>%
  mutate(p= recode(help_words, "1" = "Help", "2" = "No Help"))

clean_data$child_informed_by[str_detect(str_to_lower(clean_data$`other_informers`), "polizei")] <- "Police"
clean_data$child_informed_by[str_detect(str_to_lower(clean_data$`other_informers`), "mutter|vater|großvater|eltern")] <- "Relatives"

clean_data <- clean_data %>%
  mutate(
    Recommendation = as.numeric(Recommendation),
    Recommendation = if_else(!is.na(who_recommended) & Recommendation == 1 , 2, Recommendation)
  )
clean_data <- clean_data %>%
  filter(!(Recommendation == "1" & no_recommendation == "1"))
clean_data <- clean_data %>%
  mutate(Recommendation= recode(Recommendation, "1" = "No", "2" = "Yes"))

clean_data$who_recommended[str_detect(str_to_lower(clean_data$`who_recommended`), "aetas")] <- "AETAS"
clean_data$who_recommended[str_detect(str_to_lower(clean_data$`who_recommended`), "psychotherapeut|kindertherapeutin")] <- "Therapeutic"
clean_data$who_recommended[str_detect(str_to_lower(clean_data$`who_recommended`), "psu|kit|notfallseelsorger|kriseninterventionsteam")] <- "Emergency Support"

clean_data$who_recommended[
  !is.na(clean_data$who_recommended) &
    !str_detect(clean_data$who_recommended, "AETAS")&
    !str_detect(clean_data$who_recommended, "Therapeutic")&
    !str_detect(clean_data$who_recommended, "Emergency Support")
] <- "Others"


clean_data$provider_words_other[str_detect(str_to_lower(clean_data$`provider_words_other`), "aetas")] <- "AETAS"
clean_data$provider_words_other[str_detect(str_to_lower(clean_data$`provider_words_other`), "psychotherapeut|kindertherapeutin")] <- "Therapeutic"

clean_data$provider_words_other[str_detect(str_to_lower(clean_data$`provider_words_other`), "psu|kit|notfallseelsorger|kriseninterventionsteam")] <- "Emergency Support"

clean_data$provider_words_other[
  !is.na(clean_data$provider_words_other) &
    !str_detect(clean_data$provider_words_other, "AETAS")&
    !str_detect(clean_data$provider_words_other, "Therapeutic")&
    !str_detect(clean_data$provider_words_other, "Emergency Support")
] <- "Others"  
clean_data <- clean_data %>%
  mutate(
    who_recommended = if_else(
      Recommendation == "Yes" & is.na(who_recommended),
      "Others",
      who_recommended
    )
  )
clean_data <- clean_data %>%
  mutate(
    who_recommended = if_else(
      is.na(who_recommended),
      "None",
      who_recommended
    )
  )
clean_data <- clean_data %>%
  mutate(
    provider_words_other = if_else(
      help_words == "Help" & is.na(provider_words_other),
      "Others",
      provider_words_other
    )
  )
clean_data <- clean_data %>%
  mutate(
    provider_words_other = if_else(
      is.na(provider_words_other),
      "None",
      provider_words_other
    )
  )

clean_data <- clean_data %>%
  mutate(help_timing= recode(help_timing, "1" = "Immediately", "2" = "Within hours", "3" = "Within days", "4" = "Later", "5" = "Not at all"))
clean_data <- clean_data %>%
  mutate(other_information_sources = recode(other_information_sources, "1" = "Yes", "2" = "No"))


clean_data <- clean_data %>%
  mutate(
    which_info_source = str_to_lower(which_info_source),
    
    Internet = str_detect(which_info_source, "internet"),
    Literature = str_detect(which_info_source, "bücher|literatur"),
    Aetas = str_detect(which_info_source, "aetas"),
    Agus = str_detect(which_info_source, "agus"),
    psych_med = str_detect(which_info_source, "psychologische|neurologen"),
    
    # Count how many categories matched
    n_matches = Internet + Literature + Aetas + Agus + psych_med ,
    
    which_info_source = case_when(
      is.na(which_info_source) ~ NA_character_ ,
      n_matches > 1 ~ "Multiple sources",
      Internet ~ "Internet",
      Literature ~ "Literature",
      Aetas ~ "AETAS material",
      Agus ~ "AGUS",
      psych_med ~ "Psychological/Medical counseling",
      TRUE ~ "Other"
    )
  ) %>%
  select(-Internet, -Literature, -Aetas, -Agus, -psych_med, -n_matches)

clean_data <- clean_data %>%
  mutate(
    which_info_source = if_else(
      other_information_sources == "No" & is.na(which_info_source),
      "No Info",
      which_info_source
    )
  )

d <- cleaned_data %>%    # Dependency: this relies on variables prepared in Member 4 cleaning block (cleaned_data).
  select(
    CASE,
    `F8 eigenes Verhältnis zum Kind`,
    `F8 anderes Verhältnis`,
    `F15_K1 wie erfahren`,
    `F15_K1 andere`,
    `F15_K1 Sonstiges`
  ) %>%
  mutate(
    `F8 eigenes Verhältnis zum Kind` = recode(
      as.character(`F8 eigenes Verhältnis zum Kind`),
      "1" = "Parent",
      "2" = "Sibling",
      "3" = "Other",
      "4" = "Grandparent",
      .default = NA_character_
    ),
    `F15_K1 wie erfahren` = case_when(
      `F15_K1 wie erfahren` == 1 ~ "Self found",
      `F15_K1 wie erfahren` == 2 ~ "Present",
      `F15_K1 wie erfahren` == 3 ~ `F8 eigenes Verhältnis zum Kind`,
      `F15_K1 wie erfahren` %in% c(4, 5) ~ "Other",
      TRUE ~ NA_character_
    ),
    `F15_K1 andere` = case_when(
      str_detect(`F15_K1 andere`, regex("Eltern|Mutter|Vater", ignore_case = TRUE)) ~ "Parent",
      str_detect(`F15_K1 andere`, regex("Polizei", ignore_case = TRUE)) ~ "Police",
      str_detect(`F15_K1 andere`, regex("Freund(in)?", ignore_case = TRUE)) ~ "Friend",
      str_detect(`F15_K1 andere`, regex("Lehrer(in)?", ignore_case = TRUE)) ~ "Teacher",
      str_detect(`F15_K1 andere`, regex("Großvater|Großmutter", ignore_case = TRUE)) ~ "Grandparent",
      TRUE ~ `F15_K1 andere`
    ),
    `F15_K1 Sonstiges` = case_when(
      str_detect(`F15_K1 Sonstiges`, regex("Das Kind befand sich im Haus", ignore_case = TRUE)) ~ "Present",
      str_detect(`F15_K1 Sonstiges`, regex("von mir", ignore_case = TRUE)) ~ `F8 eigenes Verhältnis zum Kind`,
      TRUE ~ NA_character_
    ),
    `How was the kid informed` = case_when(
      `F15_K1 wie erfahren` != "Other" ~ `F15_K1 wie erfahren`,
      `F15_K1 wie erfahren` == "Other" & !is.na(`F15_K1 Sonstiges`) ~ `F15_K1 Sonstiges`,
      `F15_K1 wie erfahren` == "Other" & !is.na(`F15_K1 andere`) ~ `F15_K1 andere`,
      TRUE ~ NA_character_
    )
  ) %>%
  select(CASE, `How was the kid informed`)

clean_data <-  clean_data %>% left_join(d)

clean_data <- clean_data %>%
  filter(!is.na(`How was the kid informed`),
         !is.na(cause_of_death),
         !is.na(child_age))


