# Job Market Analysis: Basic Analysis and Visualization
# Author: S
# Purpose: Analyze job market trends and test September surge hypothesis

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# Install ggplot2 if needed:
# install.packages("ggplot2")

# Load the data (assuming you already have job_data from previous script)
# If not, uncomment the next line:
# job_data <- read_csv("data/raw/sample_job_market_data.csv")

# ============================================================================
# ANALYSIS 1: September Surge Investigation
# ============================================================================

# Calculate monthly totals across all cities and job types
monthly_trends <- job_data %>%
  group_by(month) %>%
  summarise(
    total_jobs = sum(job_count),
    avg_salary = mean(avg_salary),
    .groups = 'drop'
  ) %>%
  mutate(
    month_name = month.name[month(month)],
    year = year(month),
    month_num = month(month)
  )

# Test the September surge hypothesis
september_surge_test <- job_data %>%
  mutate(
    month_num = month(month),
    is_september = month_num == 9
  ) %>%
  group_by(is_september) %>%
  summarise(
    avg_jobs = mean(job_count),
    total_jobs = sum(job_count),
    .groups = 'drop'
  )

cat("=== SEPTEMBER SURGE ANALYSIS ===\n")
print(september_surge_test)

# Calculate percentage increase
sep_increase <- (september_surge_test$avg_jobs[2] - september_surge_test$avg_jobs[1]) / 
  september_surge_test$avg_jobs[1] * 100

cat("September shows a", round(sep_increase, 1), "% increase in average job postings\n\n")

# ============================================================================
# VISUALIZATION 1: Monthly Job Posting Trends
# ============================================================================

monthly_plot <- ggplot(monthly_trends, aes(x = month, y = total_jobs)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  # Highlight September
  geom_point(data = filter(monthly_trends, month_num == 9), 
             aes(x = month, y = total_jobs), 
             color = "red", size = 3) +
  labs(
    title = "Job Market Trends: Is There Really a September Surge?",
    subtitle = "Total job postings across all cities and job types",
    x = "Month",
    y = "Total Job Postings",
    caption = "Red dots highlight September"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months")

print(monthly_plot)

# Save the plot
ggsave("outputs/figures/monthly_job_trends.png", monthly_plot, 
       width = 10, height = 6, dpi = 300)

# ============================================================================
# ANALYSIS 2: City Comparison
# ============================================================================

city_summary <- job_data %>%
  group_by(city) %>%
  summarise(
    total_jobs = sum(job_count),
    avg_jobs_per_month = mean(job_count),
    avg_salary = mean(avg_salary),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_jobs))

cat("=== CITY RANKINGS ===\n")
print(city_summary)

# ============================================================================
# VISUALIZATION 2: City Comparison
# ============================================================================

city_plot <- ggplot(city_summary, aes(x = reorder(city, total_jobs), y = total_jobs)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Where Are the Jobs? Total Opportunities by City",
    subtitle = "20-month total across all job types",
    x = "City",
    y = "Total Job Postings"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(city_plot)

# Save the plot
ggsave("outputs/figures/city_comparison.png", city_plot, 
       width = 8, height = 6, dpi = 300)

# ============================================================================
# ANALYSIS 3: Job Type Analysis
# ============================================================================

job_type_summary <- job_data %>%
  group_by(job_title) %>%
  summarise(
    total_jobs = sum(job_count),
    avg_salary = mean(avg_salary),
    cities_with_jobs = n_distinct(city),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_jobs))

cat("=== JOB TYPE ANALYSIS ===\n")
print(job_type_summary)

# ============================================================================
# ANALYSIS 4: Your Personal Insights
# ============================================================================

# Focus on your target cities and job types
your_focus <- job_data %>%
  filter(
    city %in% c("Philadelphia", "Boston", "New York", "Chicago"),
    job_title %in% c("Data Analyst", "Product Manager", "UX Researcher")
  ) %>%
  group_by(city, job_title) %>%
  summarise(
    total_jobs = sum(job_count),
    avg_salary = mean(avg_salary),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_jobs))

cat("=== YOUR FOCUS AREAS ===\n")
cat("Northeast cities + your target job types:\n")
print(your_focus)

# Create a recommendation score (jobs + salary, normalized)
your_focus_scored <- your_focus %>%
  mutate(
    job_score = (total_jobs - min(total_jobs)) / (max(total_jobs) - min(total_jobs)),
    salary_score = (avg_salary - min(avg_salary)) / (max(avg_salary) - min(avg_salary)),
    combined_score = (job_score + salary_score) / 2
  ) %>%
  arrange(desc(combined_score))

cat("\n=== RECOMMENDATION RANKING ===\n")
cat("(Based on combination of job volume and salary)\n")
print(select(your_focus_scored, city, job_title, combined_score, total_jobs, avg_salary))

# ============================================================================
# EXPORT RESULTS
# ============================================================================

# Save all analysis results
write_csv(monthly_trends, "data/processed/monthly_trends.csv")
write_csv(city_summary, "data/processed/city_summary.csv")
write_csv(job_type_summary, "data/processed/job_type_summary.csv")
write_csv(your_focus_scored, "data/processed/personal_recommendations.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to data/processed/ folder\n")
cat("Visualizations saved to outputs/figures/ folder\n")
cat("\nKey findings:\n")
cat("1. September surge:", round(sep_increase, 1), "% increase\n")
cat("2. Top city:", city_summary$city[1], "\n")
cat("3. Best job type:", job_type_summary$job_title[1], "\n")
cat("4. Your best bet:", your_focus_scored$city[1], "for", your_focus_scored$job_title[1], "(among Northeast cities)\n")