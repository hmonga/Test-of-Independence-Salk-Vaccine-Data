# HW14 – Test of Independence: Salk Vaccine Data

# Step 1: Contingency Table (Vaccination Status vs Polio Status)
vaccine_table <- matrix(c(
  200688, 24, 33,
  201087, 27, 115
), nrow = 2, byrow = TRUE)

rownames(vaccine_table) <- c("Vaccinated", "Placebo")
colnames(vaccine_table) <- c("NoPolio", "Nonparalytic", "Paralytic")

# Step 2: Chi-Square Test
chi_result <- chisq.test(vaccine_table)
cat("Chi-Square Test Statistic:", round(chi_result$statistic, 4), "\n")
cat("Chi-Square Test p-value:", chi_result$p.value, "\n\n")

# Step 3: Permutation Test with 5000 reshuffles
set.seed(12345)

# Convert table to vectors
group <- c(rep("Vaccinated", sum(vaccine_table[1, ])),
           rep("Placebo", sum(vaccine_table[2, ])))

outcome <- c(
  rep("NoPolio", vaccine_table[1, 1]),
  rep("Nonparalytic", vaccine_table[1, 2]),
  rep("Paralytic", vaccine_table[1, 3]),
  rep("NoPolio", vaccine_table[2, 1]),
  rep("Nonparalytic", vaccine_table[2, 2]),
  rep("Paralytic", vaccine_table[2, 3])
)

trial_data <- data.frame(Group = group, Outcome = outcome)

# Observed test statistic
obs_table <- table(trial_data$Group, trial_data$Outcome)
expected <- outer(rowSums(obs_table), colSums(obs_table)) / sum(obs_table)
obs_stat <- sum((obs_table - expected)^2 / expected)

# Permutation loop (fast version)
perm_stats <- replicate(5000, {
  shuffled <- sample(trial_data$Group)
  perm_table <- table(shuffled, trial_data$Outcome)
  expected_perm <- outer(rowSums(perm_table), colSums(perm_table)) / sum(perm_table)
  sum((perm_table - expected_perm)^2 / expected_perm)
})

perm_p_value <- mean(perm_stats >= obs_stat)
cat("Permutation Test p-value:", perm_p_value, "\n")
