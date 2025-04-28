# analysis_functions.R

# load required packages
library(brms)
library(maps)
library(dplyr)
library(mapdata)
library(ggrepel)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(ggcorrplot)

# function to create state voting map
create_state_voting_map <- function(votes_data) {
  state_summary <- votes_data %>%
    group_by(STNAME) %>%
    summarize(
      mean_gop = mean(per_gop),
      mean_depression = mean(Crude.Prevalence.Estimate),
      mean_race = mean(race),
      n_counties = n()
    )
  
  us_states <- map_data("state")
  state_summary$state_lower <- tolower(state_summary$STNAME)
  
  map_data <- left_join(us_states, state_summary, by = c("region" = "state_lower"))
  
  ggplot(map_data, aes(x = long, y = lat, group = group, fill = mean_gop)) +
    geom_polygon(color = "white", linewidth = 0.2) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_gradient2(
      name = "Republican vote (%)",
      low = "blue", mid = "white", high = "red",
      midpoint = 50,
      limits = c(min(state_summary$mean_gop), max(state_summary$mean_gop))
    ) +
    labs(title = "Average republican vote percentage by state",
         subtitle = "2024 US Presidential Election") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}

# function to create depression map
create_depression_map <- function(votes_data) {
  state_summary <- votes_data %>%
    group_by(STNAME) %>%
    summarize(
      mean_gop = mean(per_gop),
      mean_depression = mean(Crude.Prevalence.Estimate),
      mean_race = mean(race),
      n_counties = n()
    )
  
  us_states <- map_data("state")
  state_summary$state_lower <- tolower(state_summary$STNAME)
  
  map_data <- left_join(us_states, state_summary, by = c("region" = "state_lower"))
  
  ggplot(map_data, aes(x = long, y = lat, group = group, fill = mean_depression)) +
    geom_polygon(color = "white", linewidth = 0.2) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    scale_fill_viridis_c(
      name = "Depression rate (%)",
      option = "plasma"
    ) +
    labs(title = "Average depression rate by state",
         subtitle = "Based on self-reported data") +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
}

# function to create correlation plot
create_correlation_plot <- function(votes_data) {
  corr_matrix <- cor(votes_data[, c("per_gop", "Crude.Prevalence.Estimate", "race", "TOT_POP", "TOT_MALE", "TOT_FEMALE")])
  
  melted_corr <- melt(corr_matrix)
  names(melted_corr) <- c("Var1", "Var2", "value")
  
  ggplot(melted_corr, aes(Var1, Var2, fill = value)) + 
    geom_tile() +
    scale_fill_gradient2(low = "navy", mid = "white", high = "firebrick", 
                         midpoint = 0, limits = c(-1, 1), name = "Corr") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Correlation between voting, depression, race and demographics\nin recent US presidential election",
         x = "", y = "") +
    geom_text(aes(label = round(value, 2)), size = 3)
}

# function to create state effects plot
create_state_effects_plot <- function(model_hier) {
  ranef_summary <- brms::ranef(model_hier, summary = TRUE)
  state_effects_df <- data.frame(
    state = rownames(ranef_summary$STNAME),
    estimate = ranef_summary$STNAME[, "Estimate", "Intercept"],
    lower = ranef_summary$STNAME[, "Q2.5", "Intercept"],
    upper = ranef_summary$STNAME[, "Q97.5", "Intercept"]
  )
  
  state_effects_df <- state_effects_df[order(state_effects_df$estimate), ]
  state_effects_df$state <- factor(state_effects_df$state, levels = state_effects_df$state)
  
  ggplot(state_effects_df, aes(x = estimate, y = state, color = estimate > 0)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_pointrange(aes(xmin = lower, xmax = upper)) +
    scale_color_manual(values = c("blue", "red"), guide = "none") +
    labs(
      title = "State-level effects on Republican voting",
      subtitle = "After controlling for depression rates and racial demographics",
      x = "Effect on Republican vote % (compared to national average)",
      y = "State"
    ) +
    theme_minimal()
}

# function to create fixed effects plot
create_fixed_effects_plot <- function(model_hier) {
  fixed_effects <- fixef(model_hier)
  fixed_effects_df <- data.frame(
    term = rownames(fixed_effects),
    estimate = fixed_effects[, 1],
    lower = fixed_effects[, 3],  # 2.5% quantile
    upper = fixed_effects[, 4]   # 97.5% quantile
  )
  
  ggplot(fixed_effects_df[-1, ], aes(x = estimate, y = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_pointrange(aes(xmin = lower, xmax = upper)) +
    labs(
      title = "Fixed effects",
      subtitle = "Estimated impact on Republican vote %",
      x = "Effect size",
      y = ""
    ) +
    theme_minimal()
}