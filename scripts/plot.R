
###MEMBER1-------------------------------------------------------------------------------------------------------------------------------------------------
##stress level vs time since event------------------------------------------------------------------------------------------------------------------

#Trend via Quartile Lines-----------------------------------------------------------------------------------------

ggplot(summary_by_time, aes(x = phase, y = med, group = 1)) +
  geom_line(aes(y = med, color = "Median"), size = 1.3) +
  geom_point(aes(y = med, color = "Median"), size = 3) +
  geom_ribbon(aes(ymin = q1, ymax = q3), alpha = 0.2) +
  geom_line(aes(y = mean, color = "Mean"),
            linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = mean, color = "Mean"),
             size = 2, shape = 18) +
  scale_color_manual(
    name   = "Statistic",
    values = c("Median" = deep.blue, "Mean" = teal)
  ) +
  scale_y_continuous(
    limits = c(1, 10),
    breaks = 1:10
  ) +
  labs(
    title = "Current Stress Trajectory Over Time Since the Event",
    x = "Time since event",
    y = "Stress level",
  ) +
  annotation_custom(
    rectGrob(
      x = unit(1.00, "npc"),
      y = unit(0.72, "npc"),
      width  = unit(0.28, "npc"),
      height = unit(0.12, "npc"),
      just = "left",
      gp = gpar(
        fill = "grey90",
        col = NA
      )
    )
  ) +
  annotation_custom(
    textGrob(
      "GREY AREA:\ninterquartile range\n(25th–75th percentile)",
      x = unit(1.005, "npc"),
      y = unit(0.72, "npc"),
      just = "left",
      gp = gpar(
        fontsize = 9,
        col = "grey40",
        fontface = "bold"
      )
    )
  ) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(10, 350, 10, 10)
  ) +
  theme_minimal(base_size = 16)  +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text.x       = element_text(size = 11, angle = 0, hjust = 0.5),
    legend.position   = "right",
    legend.title      = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank()
  )


##stress level vs cause of death-------------------------------------------------------------------------------------------------------

#beeswarm-----------------------------------------------------------------
ggplot(cause_of_death_data,
       aes(x = cause_of_death,
           y = stress_level,)) +
  geom_beeswarm(size = 2, alpha = 0.6) +
  facet_wrap(~ timepoint) +
  scale_y_continuous(
    limits = c(1, 10),
    breaks = 1:10
  ) +
  labs(
    title = "Stress Level by Cause of Death Across Timepoints",
    x = "Cause of death",
    y = "Stress level",
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x       = element_text(size = 11, angle = 0, hjust = 0.5),
    legend.position   = "none",
    panel.grid.major.x = element_blank()
  )




##stress level vs social demographic status----------------------------------------------------------------------------------------------------------------------------

#box plot and jitter-------------------------------------------------------------------------
ggplot(social_long, aes(x = category, y = early_stress_level)) +
  geom_boxplot(alpha = 0.4, color = "black") +
  geom_jitter(color = light.cyan.mist, alpha = 0.4, width = 0) +
  facet_wrap(~ variable, scales = "free_x") +
  scale_y_continuous(
    limits = c(1, 10),
    breaks = 1:10
  ) +
  labs(
    title = "Early Stress Level by Sociodemographic Factors",
    subtitle = "The grey (blackish) dots represent the outliers",
    x = "Category",
    y = "Early stress level"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text.x       = element_text(size = 11, angle = 45, hjust = 1),
    legend.position   = "right",
    legend.title      = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines"),
    panel.grid.major.x = element_blank()
  )



##stress level vs support received------------------------------------------------------------------------------------------------------------

#half violin, boxplot, jitter-------------------------------------------------------------------------
ggplot(data_for_support, aes(x = support, y = current_stress_level)) +
  geom_half_violin(side = "l", alpha = 0.6) +
  geom_half_boxplot(side = "r", alpha = 0.5, width = 0.3) +
  scale_y_continuous(
    limits = c(1, 10),
    breaks = 1:10
  ) +
  labs(
    title = "Current Stress Level by Psychosocial Support Status",
    x = "Psychosocial support",
    y = "Stress level",
  ) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "none") +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11),
    axis.text.x       = element_text(size = 11, hjust = 0.5),
    legend.position   = "none",
    panel.grid.major.x = element_blank()
  )


##does support reduce stress level--------------------------------------------------------------------------------------------
ggplot(
  data_for_support,
  aes(x = support, y = stress_change)
) +
  geom_boxplot(
    alpha = 0.5,
    outlier.shape = NA
  ) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  geom_hline(
    yintercept = 0,
    color = "red",
    linewidth = 0.6,
    linetype = "dashed",
    alpha = 0.5
  ) +
  labs(
    title = "Change in Stress by Psychosocial Support Status",
    subtitle = "Change = early stress − current stress",
    x = "Psychosocial Support",
    y = "Stress Change"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title         = element_text(face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 11, hjust = 0.5),
    axis.text.x        = element_text(size = 11),
    panel.grid.major.x = element_blank()
  )




###MEMBER2--------------------------------------------------------------------------------------------------------------------
#who informed genral
ggplot(Who_informed_child_data, aes(x = `How was the kid informed`, y = Frequency)) +
  geom_col(fill = "#225522") +
  scale_y_continuous(
    breaks = seq(0, 100, 5)
  ) +
  labs(
    title = "Who Informed the Child",
    x = "Informer",
    y = "Frequency of Children"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x        = element_text(size = 11),
    legend.position    = "none",
    panel.grid.major.x = element_blank()
  )

#who informed + cause of death
ggplot(plot_data, aes(x = `How was the kid informed`, y = percent, fill = cause_of_death)) +
  geom_col(position = "stack") +
  scale_fill_manual(
    name = "Cause of death",
    values = c(
      "Homicide" = "#A23B3B",
      "Suicide"  = "#4E79A7"
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.10),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Who Informed the Child by Cause of Death",
    x = "Informer",
    y = "Percentage of Children"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x        = element_text(size = 11),
    legend.position    = "right",
    panel.grid.major.x = element_blank()
  )

#who informed + age
ggplot(plot2.data, aes(x = `How was the kid informed`, y = percent, fill = fct_rev(child_age_group))) +
  geom_col(color = "black", size = 0.7) +
  scale_fill_manual(values = c(
    "0–5"   = lightblue,
    "6–11"  = steelblue,
    "12–17" = navy
  )) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.10),
    labels = percent_format(1)
  ) +
  labs(
    title = "Who informed the child by age group",
    x = "Informer",
    y = "Percentage of children",
    fill = "Child age group"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x        = element_text(size = 11, hjust = 0.5),
    legend.position    = "right",
    legend.title       = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank()
  )

#who recommended x who informed
ggplot(plot_heat,
       aes(x = `How was the kid informed`,
           y = who_recommended,
           fill = percent)) +
  geom_tile(color = "white") +
  geom_label(
    aes(label = percent(percent, accuracy = 1)),
    fill = alpha("white", 0.55),
    label.size = 0,
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_gradient(
    low = "#E9F7EF",
    high = "#145A32",
    name = "% Within recommender",
    limits = c(0, 1),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "External Recommendations vs Who Informed the Child",
    x = "Source That Informed the Child",
    y = "Source That Recommended The Child´s Informer"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x        = element_text(size = 11, hjust = 0.5, angle = 45, vjust = 0.7),
    legend.position    = "right",
    legend.title       = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank()
  ) + coord_flip()


#second question
#first plot: how did they find the words?
ggplot(plot_q2.1,
       aes(axis2 = provider_words_other,
           axis3 = which_info_source,
           y = n)) +
  geom_alluvium(aes(fill = help_words), width = 0.25, alpha = 0.8) +
  geom_stratum(width = 0.3, color = "grey30") +
  geom_label(stat = "stratum",
             aes(label = after_stat(stratum)),
             size = 3) +
  scale_x_discrete(limits = c("Helper", "Info source"),
                   labels = c("Helper", "Info source")) +
  labs(
    title = "Pathways to Finding Words for the Child",
    x = "",
    y = "Number of caregivers",
    fill = "Help with wording"
  ) +
  scale_fill_manual(
    values = c(
      "Help" = "#5B6BD5",
      "No Help" = "#D9795F"
    )
  )+
  theme_minimal(base_size = 16) +
  theme(
    plot.title      = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(size = 11, hjust = 0.5),
    axis.text.x     = element_text(size = 10, angle = 0, vjust = 1),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.position = "right",
  )

#####MEMBER 3----------------------------------------------------------------------------------------------
##plot: who provided support in each domain?
ggplot(plot1_filtered, aes(x = support_domain, fill = provider_cat)) +
  geom_bar(
    position = position_dodge(width = 0.8),
    width    = 0.7,
    colour   = "grey20"
  ) +
  scale_fill_manual(
    name = "Provider category",
    values = c(
      "AETAS"               = "#44AA99",
      "PSNV / KIT"          = "#EE6677",
      "Therapist / Health"  = "#6699CC",
      "Other organisations" = "#AA4499",
      "Other person"        = "#88AA44",
      "Informal / Peer"     = "#DDCC55"
    )
  ) +
  scale_x_discrete(
    labels = c(
      "Support with finding words about the cause"      = "Support with\nfinding words\nabout the cause",
      "Support about what to watch for in the child"    = "Support about\nwhat to watch\nfor in the child",
      "Support about what is good/helpful for the child" = "Support about\nwhat is good /\nhelpful for the child"
    )
  ) +
  labs(
    title    = "Who provided psychosocial support in each domain",
    subtitle = paste("Note:", sum(plot1_data$provider_cat == "Unclassified", na.rm = TRUE),
                     "unclassified entries not shown"),
    x        = "Support domain",
    y        = "Number of mentions"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title      = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(size = 11, hjust = 0.5),
    axis.text.x     = element_text(size = 10, angle = 0, vjust = 1),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.position = "right",
  )


##plot: timeliness (1–10) by timing category, for each domain -----

ggplot(plot2_data, aes(x = timing, y = timely)) +
  geom_boxplot(
    outlier.alpha = 0.6,
    width         = 0.6
  ) +
  geom_jitter(colour = "#332288", alpha = 0.4, width = 0.2) +
  facet_wrap(~ domain, nrow = 1) +
  coord_cartesian(ylim = c(1, 10)) +
  labs(
    title    = "Perceived Timeliness of Support by Timing and Domain",
    subtitle = "Timeliness = how well-timed the support was perceived (score 1–10)",
    x        = "When was the support",
    y        = "Timeliness score "
  ) +
  theme_minimal(base_size = 16) +
  theme(
    strip.text         = element_text(size = 11, face = "bold"),
    legend.position    = "none",
    plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 11, hjust = 0.5),
    panel.grid.major.x = element_blank()
  ) +
  coord_flip()


##plot: timely caregivers perceived the support they received across different timing categories

ggplot(help_quality, aes(x = timely, y = helpful)) + 
  geom_jitter(
    width  = 0.1,
    height = 0.1,
    alpha  = 0.3,
    size   = 2,
    colour = slate
  ) +
  
  stat_summary(
    fun  = median,
    geom = "line",
    linewidth = 1.2,
    colour = forest.green,
    group = 1
  ) +
  
  stat_summary(
    fun  = median,
    geom = "point",
    size = 3,
    colour = forest.green,
    group = 1
  ) +
  facet_wrap(~ domain, nrow = 1) +
  coord_cartesian(xlim = c(1, 10), ylim = c(1, 10)) +
  labs(
    title    = "Relationship between timeliness and helpfulness of support",
    subtitle = "Helpfulness = how useful the support was perceived (score 1–10)",
    x        = "Timeliness score",
    y        = "Helpfulness score"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    strip.text         = element_text(size = 11, face = "bold"),
    plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title       = element_text(size = 12, face = "bold"), 
    plot.subtitle      = element_text(size = 11, hjust = 0.5),
    panel.grid.major.x = element_blank()
  )


##plot: the relationship between the timeliness of support and how helpful caregivers perceived that support to be.

ggplot(burden_summary, aes(x = domain, y = pct, fill = support_status)) +
  geom_col(color = "white", linewidth = 0.3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c(
      "No support needed"                  = Neutral.Gray,
      "Support received"                   = cyan,
      "Wanted support but did not receive" = Reddish.purple
    ),
    name = "Support status"
  ) +
  geom_text(
    data = subset(burden_summary),
    aes(label = percent(pct, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    colour = "black",
    size = 3.5
  ) +
  
  labs(
    title    = "Gaps and Coverage in Caregiver Support Across Domains",
    x        = "Support domain",
    y        = "Percentage of caregivers"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text.x       = element_text(size = 11, angle = 0, hjust = 0.5),
    legend.position   = "right",
    legend.title      = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank()
  )

###MEMBER4----------------------------------------------------------------------------------------------------------------
#Wished support vs stress plots
#-------------------------------------------------------------------------------------------
#Wished_Support_Plot  (Nour will present this plot it falls under her axis)

Wished_Support_data %>%
  ggplot(aes(
    x = reorder(`wished support`, n),
    y = n
  )) +
  geom_col(fill = "#2295B8", color = "grey30", width = 0.9, alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Wished Support",
    x = "Wished support",
    y = "Number of caregivers",
    fill = "Wished support"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle      = element_text(size = 11),
    axis.text.x        = element_text(size = 11, angle = 0, hjust = 0.5),
    axis.text.y        = element_text(size = 11),
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

#Wished_Support_VS._Stress_Now_and_then
ggplot(wished_stress_long, aes(x = `X044_01 Wunsch für gute Versorgung?`, y = Stress)) +
  geom_jitter(alpha = 0.6, width = 0) +
  coord_flip() +
  facet_wrap(~Time, nrow = 1) +
  theme_minimal(base_size = 16) +
  labs(
    title = "Caregiver Stress in Relation to Desired Support Types",
    x = "Type of support wished for",
    y = "Stress level"
  ) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x        = element_text(size = 11, hjust = 0.5),
    axis.text.y        = element_text(size = 11),
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(size = 13, face = "bold")
  )


#--------------------------------------------------------------------------------------------
#Support vs stress plots
#--------------------------------------------------------------------------------------------
#Type_Of_Support_VS._Stress_Plot
Type_Of_Support_VS._Stress_data %>%
  ggplot(aes(
    x = Support_Group,
    y = stress,
    fill = Support_Group
  )) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 0.6, color = "red") +
  stat_summary(fun = mean, geom = "point", alpha = 0.9) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, alpha = 0.45, fun.args = list(mult = 1)) +
  facet_grid(time ~ Type) +
  labs(
    x = "Support intensity category",
    y = "Mean stress level",
    title = "Stress In Relation To Support Intensity",
    subtitle = "Error bars represent mean ± standard deviation (SD)\nSupport intensity categories: Low = 0–33, Medium = 34–66, High = 67–100"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle     = element_text(size = 11, hjust = 0.5),
    axis.text.x       = element_text(size = 11),
    legend.position   = "none",
    strip.text        = element_text(size = 13, face = "bold"),
    panel.grid.major.x = element_blank(),
    plot.caption      = element_markdown(hjust = 0.5, size = 11)
  )


#--------------------------------------------------------------------------------------------
#Information seeking vs stress plots
#--------------------------------------------------------------------------------------------
#information_seeking_vs_stress_then_and_now
ggplot(plot_combined, aes(x = Source, fill = stress_cat)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c(
    "Low Stress Level" = light_raspberry, 
    "Moderate Stress Level" = medium_raspberry, 
    "High Stress Level" = deep_raspberry
  )) +
  labs(
    title = "Stress Levels and Information Source",
    x = "Information source",
    y = "Share of participants",
    fill = "Stress category",
    subtitle = "Stress categories: Low = 1–4, Moderate = 4–7, High = 8–10"
  ) +
  facet_wrap(~Time, nrow = 1) +  
  coord_flip() +
  theme_minimal(base_size = 16) +
  theme(
    plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x        = element_text(size = 11, hjust = 0.5),
    axis.text.y        = element_text(size = 11),
    legend.position    = "right",
    plot.subtitle = element_text(hjust = 0.5, size = 12 ),
    legend.title       = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank()
  )


#--------------------------------------------------------------------------------------------
#Coping guidance vs stress plots 
#--------------------------------------------------------------------------------------------
#stress_level_with_and_without_coping_guidance
ggplot(stress_combined, aes(x = time_group, y = stress)) +
  geom_boxplot(width = 0.4,
               box.linewidth = 0.1,
               whisker.linewidth = 0.1,
               median.linewidth = 0.7,
               colour = "grey",
               median.colour = "black"
  ) +
  geom_jitter(alpha = 0.4, width = 0) +
  facet_wrap(~Coping, nrow = 1) +
  theme_minimal(base_size = 16) +
  labs(
    title = "Current Stress by Time Since the Death and Coping Guidance",
    x = "Time since the event",
    y = "Stress level"
  ) +
  theme(
    plot.title        = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x        = element_text(size = 11, hjust = 0.5),
    axis.text.y        = element_text(size = 11),
    legend.position    = "none",
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(size = 13, face = "bold")
  )
