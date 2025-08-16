# Timing Analysis: Should I Focus My NYC Data Analyst Search Now?
# Author: S
# Purpose: Combine city/job analysis with seasonal timing for actionable insights

library(dplyr)
library(ggplot2)
library(lubridate)

# ============================================================================
# ANALYSIS: NYC Data Analyst Jobs by Month
# ============================================================================

# Focus specifically on your target: NYC Data Analyst (top choice among NE cities)
nyc_data_analyst <- job_data %>%
  filter(
    city == "New York",
    job_title == "Data Analyst"
  ) %>%
  mutate(
    month_name = month.name[month(month)],
    month_num = month(month),
    year = year(month)
  )

cat("=== NYC DATA ANALYST: MONTHLY BREAKDOWN ===\n")
monthly_breakdown <- nyc_data_analyst %>%
  group_by(month_name, month_num) %>%
  summarise(
    avg_jobs = mean(job_count),
    avg_salary = mean(avg_salary),
    .groups = 'drop'
  ) %>%
  arrange(month_num)

print(monthly_breakdown)

# Find the best and worst months
best_month <- monthly_breakdown[which.max(monthly_breakdown$avg_jobs), ]
worst_month <- monthly_breakdown[which.min(monthly_breakdown$avg_jobs), ]

cat("\nBEST month for NYC Data Analyst jobs:", best_month$month_name, 
    "with", round(best_month$avg_jobs, 1), "jobs\n")
cat("WORST month for NYC Data Analyst jobs:", worst_month$month_name, 
    "with", round(worst_month$avg_jobs, 1), "jobs\n")

# Calculate September advantage specifically for your target
september_advantage <- monthly_breakdown %>%
  filter(month_num == 9) %>%
  pull(avg_jobs)

other_months_avg <- monthly_breakdown %>%
  filter(month_num != 9) %>%
  pull(avg_jobs) %>%
  mean()

nyc_da_september_boost <- (september_advantage - other_months_avg) / other_months_avg * 100

cat("\nFor NYC Data Analyst specifically:\n")
cat("September:", round(september_advantage, 1), "jobs vs Other months average:", round(other_months_avg, 1), "jobs\n")
cat("That's a", round(nyc_da_september_boost, 1), "% increase in September!\n")

# ============================================================================
# TIMING ANALYSIS: Current Opportunity
# ============================================================================

# What's your expected job search timeline?
current_date <- Sys.Date()
current_month <- month(current_date)

cat("\n=== TIMING ANALYSIS ===\n")
cat("Today's date:", as.character(current_date), "\n")
cat("Current month:", month.name[current_month], "\n")

# Calculate opportunities in different scenarios
if (current_month == 9) {
  cat("\n🎯 PERFECT TIMING! You're already in September - the peak month!\n")
  cat("Expected NYC Data Analyst jobs this month:", round(september_advantage, 1), "\n")
  
} else if (current_month == 8) {
  cat("\n⏰ GREAT TIMING! September (peak month) starts soon!\n")
  cat("Expected jobs in September:", round(september_advantage, 1), "\n")
  cat("vs Expected jobs if you wait until October:", 
      round(monthly_breakdown$avg_jobs[monthly_breakdown$month_num == 10], 1), "\n")
  
} else if (current_month < 9) {
  months_until_sep <- 9 - current_month
  cat("\n📅 STRATEGY: September peak is", months_until_sep, "months away\n")
  cat("You could start networking now and intensify applications in September\n")
  
} else {
  months_until_next_sep <- 12 - current_month + 9
  cat("\n📅 STRATEGY: Next September peak is", months_until_next_sep, "months away\n")
  cat("Consider the January surge (", 
      round(monthly_breakdown$avg_jobs[monthly_breakdown$month_num == 1], 1), 
      " jobs) as your next target\n")
}

# ============================================================================
# VISUALIZATION: NYC Data Analyst Monthly Pattern
# ============================================================================

nyc_da_plot <- ggplot(monthly_breakdown, aes(x = reorder(month_name, month_num), y = avg_jobs)) +
  geom_col(aes(fill = month_num == 9), alpha = 0.8) +
  scale_fill_manual(values = c("steelblue", "red"), guide = "none") +
  labs(
    title = "NYC Data Analyst Jobs: When to Search",
    subtitle = paste("September shows a", round(nyc_da_september_boost, 1), "% increase"),
    x = "Month",
    y = "Average Job Postings",
    caption = "Red bar = September peak month"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_text(aes(label = round(avg_jobs, 1)), vjust = -0.3, size = 3)

print(nyc_da_plot)

# Save the plot
ggsave("outputs/figures/nyc_data_analyst_timing.png", nyc_da_plot, 
       width = 10, height = 6, dpi = 300)

# ============================================================================
# ACTIONABLE RECOMMENDATIONS
# ============================================================================

cat("\n=== ACTIONABLE RECOMMENDATIONS ===\n")

# Calculate opportunity cost of waiting
if (current_month <= 9) {
  sep_jobs <- round(september_advantage, 1)
  current_month_jobs <- round(monthly_breakdown$avg_jobs[monthly_breakdown$month_num == current_month], 1)
  
  if (current_month < 9) {
    cat("1. 🎯 TIMING: Start preparing now, go intensive in September\n")
    cat("   - September has", sep_jobs, "jobs vs", current_month_jobs, "this month\n")
    cat("   - Use August to polish resume, research companies, build network\n")
    cat("   - Launch heavy application push in September\n\n")
  }
  
  cat("2. 💼 STRATEGY: Focus exclusively on NYC Data Analyst roles\n")
  cat("   - This combination scored highest in your analysis\n")
  cat("   - September gives you", round(nyc_da_september_boost, 1), "% more opportunities\n\n")
  
  cat("3. 📊 BACKUP PLAN: If September doesn't work out\n")
  cat("   - January is your second-best timing option\n")
  cat("   - January jobs:", round(monthly_breakdown$avg_jobs[monthly_breakdown$month_num == 1], 1), "\n")
  cat("   - Use October-December to get feedback and improve applications\n\n")
}

# ============================================================================
# PORTFOLIO STORY SUMMARY
# ============================================================================

cat("=== YOUR PORTFOLIO STORY ===\n")
cat("Problem: I'm unemployed and considering multiple cities - where and when should I focus?\n\n")
cat("Analysis: I analyzed job market data across 8 cities and 6 job types over 20 months\n\n")
cat("Key Findings:\n")
cat("• September shows 42.6% more job postings overall\n")
cat("• NYC offers the best opportunities for Data Analyst roles\n")
cat("• For NYC Data Analyst specifically, September has", round(nyc_da_september_boost, 1), "% more jobs\n\n")
cat("Action: Focus my job search on NYC Data Analyst roles, with intensive effort in September\n\n")
cat("Impact: Data-driven job search strategy instead of random applications\n")

# Save this analysis
write_csv(monthly_breakdown, "data/processed/nyc_data_analyst_timing.csv")
write_csv(nyc_data_analyst, "data/processed/nyc_data_analyst_full_data.csv")