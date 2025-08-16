# Detailed Ranking Analysis: Why NYC Data Analyst Won
# Author: S
# Purpose: Deep dive into the scoring methodology and rankings

library(dplyr)
library(ggplot2)
library(knitr)

# ============================================================================
# COMPREHENSIVE RANKING: ALL CITY-JOB COMBINATIONS
# ============================================================================

# Calculate detailed scores for ALL combinations
comprehensive_ranking <- job_data %>%
  group_by(city, job_title) %>%
  summarise(
    total_jobs = sum(job_count),
    avg_jobs_per_month = mean(job_count),
    avg_salary = mean(avg_salary),
    .groups = 'drop'
  ) %>%
  # Calculate normalized scores (0-1 scale)
  mutate(
    # Job volume scores
    job_score = (total_jobs - min(total_jobs)) / (max(total_jobs) - min(total_jobs)),
    jobs_percentile = round(rank(total_jobs) / n() * 100, 1),
    
    # Salary scores  
    salary_score = (avg_salary - min(avg_salary)) / (max(avg_salary) - min(avg_salary)),
    salary_percentile = round(rank(avg_salary) / n() * 100, 1),
    
    # Combined score (equal weighting)
    combined_score = (job_score + salary_score) / 2,
    combined_percentile = round(rank(combined_score) / n() * 100, 1),
    
    # Format for readability
    avg_salary_formatted = paste0("$", format(round(avg_salary), big.mark = ","))
  ) %>%
  arrange(desc(combined_score))

# ============================================================================
# QUESTION 1: First Place vs Second Place
# ============================================================================

cat("=== TOP 10 CITY-JOB COMBINATIONS ===\n")
top_10 <- comprehensive_ranking %>%
  select(city, job_title, total_jobs, avg_salary_formatted, combined_score, combined_percentile) %>%
  head(10)

print(top_10)

# Focus on the winner
winner <- comprehensive_ranking[1, ]
second_place <- comprehensive_ranking[2, ]

cat("\n=== WINNER ANALYSIS ===\n")
cat("🥇 WINNER:", winner$city, winner$job_title, "\n")
cat("   Combined Score:", round(winner$combined_score, 3), "\n")
cat("   Ranking: Top", winner$combined_percentile, "percentile\n")
cat("   Total Jobs:", winner$total_jobs, "\n")
cat("   Average Salary:", winner$avg_salary_formatted, "\n\n")

cat("🥈 SECOND PLACE:", second_place$city, second_place$job_title, "\n")
cat("   Combined Score:", round(second_place$combined_score, 3), "\n")
cat("   Ranking: Top", second_place$combined_percentile, "percentile\n")
cat("   Total Jobs:", second_place$total_jobs, "\n")
cat("   Average Salary:", second_place$avg_salary_formatted, "\n\n")

# Calculate the advantage
score_advantage <- (winner$combined_score - second_place$combined_score) / second_place$combined_score * 100
cat("📊", winner$city, winner$job_title, "beats second place by",  round(score_advantage, 1), "% in combined score\n")


# ============================================================================
# QUESTION 2: What Drove the Win?
# ============================================================================

cat("\n=== WHAT DROVE THE WIN? ===\n")
cat(winner$city, winner$job_title, "breakdown:\n")
cat("• Job Volume Score:", round(winner$job_score, 3), "(", winner$jobs_percentile, "th percentile)\n")
cat("• Salary Score:", round(winner$salary_score, 3), "(", winner$salary_percentile, "th percentile)\n")
cat("• Combined Score:", round(winner$combined_score, 3), "\n\n")

# Compare to overall averages
avg_job_score <- mean(comprehensive_ranking$job_score)
avg_salary_score <- mean(comprehensive_ranking$salary_score)

cat("Compared to all combinations:\n")
if (winner$job_score > winner$salary_score) {
  cat("🎯 PRIMARY DRIVER: Job Volume (", round(winner$job_score, 3), " vs avg ", round(avg_job_score, 3), ")\n")
  cat("📈 Secondary factor: Salary (", round(winner$salary_score, 3), " vs avg ", round(avg_salary_score, 3), ")\n")
} else {
  cat("🎯 PRIMARY DRIVER: Salary (", round(winner$salary_score, 3), " vs avg ", round(avg_salary_score, 3), ")\n")
  cat("📈 Secondary factor: Job Volume (", round(winner$job_score, 3), " vs avg ", round(avg_job_score, 3), ")\n")
}

# ============================================================================
# QUESTION 3: Strategic Comparisons
# ============================================================================

cat("\n=== DATA ANALYST OPPORTUNITIES: YOUR TARGET NE CITIES ===\n")

# Compare Data Analyst roles across your Northeast target cities
northeast_data_analyst <- comprehensive_ranking %>%
  filter(
    city %in% c("Philadelphia", "Boston", "New York", "Chicago"), # Note: Chicago added as major city
    job_title == "Data Analyst"
  ) %>%
  select(city, total_jobs, avg_salary_formatted, combined_score, combined_percentile) %>%
  arrange(desc(combined_score)) %>%
  mutate(rank = row_number())

print(northeast_data_analyst)

cat("\nKey insights for Data Analyst roles:\n")
best_ne_da <- northeast_data_analyst[1, ]
worst_ne_da <- northeast_data_analyst[nrow(northeast_data_analyst), ]

cat("🥇 Best NE option:", best_ne_da$city, "- Score:", round(best_ne_da$combined_score, 3), "\n")
cat("🥉 Weakest NE option:", worst_ne_da$city, "- Score:", round(worst_ne_da$combined_score, 3), "\n")

score_gap_ne <- (best_ne_da$combined_score - worst_ne_da$combined_score) / worst_ne_da$combined_score * 100
cat("📊 Gap between best and worst NE cities:", round(score_gap_ne, 1), "%\n")

cat("\n=== JOB TYPE COMPARISON: YOUR TARGET NE CITIES ===\n")

# Compare different job types within your target Northeast cities
northeast_job_comparison <- comprehensive_ranking %>%
  filter(
    city %in% c("Philadelphia", "Boston", "New York", "Chicago"),
    job_title %in% c("Data Analyst", "Product Manager", "UX Researcher", "Program Manager", "Policy Analyst")
  ) %>%
  select(city, job_title, total_jobs, avg_salary_formatted, combined_score) %>%
  arrange(city, desc(combined_score))

print(northeast_job_comparison)

# Find the best job type in each city
best_by_city <- northeast_job_comparison %>%
  group_by(city) %>%
  slice_max(combined_score, n = 1) %>%
  ungroup()

cat("\n=== BEST JOB TYPE IN EACH NE CITY ===\n")
print(best_by_city)

# Summary insights
cat("\nStrategic insights:\n")
for(i in 1:nrow(best_by_city)) {
  city_data <- best_by_city[i, ]
  cat("• ", city_data$city, ": Best opportunity is ", city_data$job_title, 
      " (score: ", round(city_data$combined_score, 3), ")\n", sep = "")
}

# ============================================================================
# RANKINGS BY CITY (Your Target Cities)
# ============================================================================

cat("\n=== RANKINGS WITHIN YOUR TARGET CITIES ===\n")
target_cities_ranking <- comprehensive_ranking %>%
  filter(city %in% c("Philadelphia", "Boston", "New York", "Chicago")) %>%
  select(city, job_title, total_jobs, avg_salary_formatted, combined_score) %>%
  arrange(desc(combined_score))

print(target_cities_ranking)

# ============================================================================
# VISUALIZATION: Score Breakdown
# ============================================================================

# Create a score breakdown plot for top 10
plot_data <- comprehensive_ranking %>%
  head(10) %>%
  mutate(
    city_job = paste(city, job_title, sep = "\n"),
    city_job = factor(city_job, levels = rev(city_job))  # Reverse for plotting
  )

score_breakdown_plot <- plot_data %>%
  select(city_job, job_score, salary_score) %>%
  tidyr::pivot_longer(cols = c(job_score, salary_score), 
                      names_to = "score_type", 
                      values_to = "score") %>%
  mutate(
    score_type = case_when(
      score_type == "job_score" ~ "Job Volume",
      score_type == "salary_score" ~ "Salary"
    )
  ) %>%
  ggplot(aes(x = city_job, y = score, fill = score_type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("Job Volume" = "steelblue", "Salary" = "darkgreen")) +
  labs(
    title = "What Drives Success? Job Volume vs Salary Scores",
    subtitle = "Top 10 city-job combinations",
    x = "City + Job Type",
    y = "Normalized Score (0-1)",
    fill = "Score Component"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )

print(score_breakdown_plot)

# Save the plot
ggsave("outputs/figures/score_breakdown_analysis.png", score_breakdown_plot, 
       width = 10, height = 8, dpi = 300)

# ============================================================================
# EXPORT DETAILED RESULTS
# ============================================================================

# Save comprehensive ranking
write_csv(comprehensive_ranking, "data/processed/comprehensive_ranking.csv")
write_csv(target_cities_ranking, "data/processed/target_cities_ranking.csv")

cat("\n=== SUMMARY FOR PORTFOLIO ===\n")
cat("🎯 Winner: NYC Data Analyst (Top", winner$combined_percentile, "percentile)\n")
cat("📊 Advantage: Beats #2 by", round(score_advantage, 1), "%\n")
cat("🔍 Driver:", ifelse(winner$job_score > winner$salary_score, "Job Volume", "Salary"), "was the primary factor\n")
cat("📈 Alternative: Philadelphia Data Analyst ranked #", which(comprehensive_ranking$city == "Philadelphia" & comprehensive_ranking$job_title == "Data Analyst"), "\n")
cat("💼 Data: Analyzed", nrow(comprehensive_ranking), "city-job combinations\n")