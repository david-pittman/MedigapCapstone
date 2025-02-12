##############################
##Date: Feb. 11, 2025
##Name: David Pittman
##Purpose: # Running our DiD 
# Comparing Medigap premiums, enrollment and offerings in Idaho to comparable states
##############################

#load libraries
library(data.table)
library(magrittr)
library(dplyr)
library(stringdist)
library(ggplot2)
library(sf)
library(maps)
library(did)
library(tidyr)
library(stringr)
library(broom)


##########################
# EVENT STUDY
##########################

# Convert to data.table
setDT(DiD_overall)  

# Create event time
DiD_overall[, event_time := year - 2021]

# Need to combine 2017 and 2018 into one year
DiD_overall[, event_time := fcase(
  event_time <= -3, "-3",
  event_time == -2, "-2",
  event_time == -1, "-1",
  event_time == 0, "0",
  event_time == 1, "1",
  event_time >= 2, "2"
)]

# The factor(year) * treated term estimates the treatment effect separately for each year relative to the treatment year.
DiD_event_study <- glm(weighted_premiums_overall ~ factor(event_time) + treated + factor(event_time):treated, 
                      data = DiD_overall)

# Extract coefficients
event_study_results <- tidy(DiD_event_study) 

# Filter relevant coefficients
event_study_results <- event_study_results[grepl("factor\\(event_time\\)", event_study_results$term), ]

# Convert to data.table
setDT(event_study_results)

# Extract event_time from the 'term' column
event_study_results[, `:=` (
  event_time = as.numeric(sub("factor\\(event_time\\)", "", sub(":treated", "", term))),  
  treated = ifelse(grepl(":treated", term), 1, 0)  
)]

# Plot results
# Plot 1 is combined; both plots together
ggplot(event_study_results, aes(x = event_time, y = estimate, color = factor(treated), group = treated)) +
  geom_point(position = position_dodge(width = 0.2)) +  # Slight shift to separate overlapping points
  geom_line() +  # Connect points for better trend visualization
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), 
                width = 0.2, position = position_dodge(width = 0.2)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("Control", "Treated")) +
  scale_x_continuous(breaks = seq(min(event_study_results$event_time, na.rm = TRUE), 
                                  max(event_study_results$event_time, na.rm = TRUE), by = 1)) +
  labs(title = "Event Study: Medigap Premiums in Idaho",
       x = "Years Since Policy Change",
       y = "Estimated Effect on Premiums",
       color = "Group") +
  theme_minimal()

#Plot 2 is separate plots 
p_control <- ggplot(event_study_results[treated == 0], aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(min(event_study_results$event_time, na.rm = TRUE), 
                                  max(event_study_results$event_time, na.rm = TRUE), by = 1)) +
  labs(title = "Event Study: Control Group",
       x = "Years Since Policy Change",
       y = "Estimated Effect on Premiums") +
  theme_minimal()

p_treated <- ggplot(event_study_results[treated == 1], aes(x = event_time, y = estimate)) +
  geom_point(color = "red") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = seq(min(event_study_results$event_time, na.rm = TRUE), 
                                  max(event_study_results$event_time, na.rm = TRUE), by = 1)) +
  labs(title = "Event Study: Treated Group",
       x = "Years Since Policy Change",
       y = "Estimated Effect on Premiums") +
  theme_minimal()

# Arrange the plots side by side
library(gridExtra)
grid.arrange(p_control, p_treated, ncol = 2)

# Compute the Difference Between Treated and Control
event_study_diff <- event_study_results[, .(estimate = estimate[treated == 1] - estimate[treated == 0],
                                            std.error = sqrt(std.error[treated == 1]^2 + std.error[treated == 0]^2)), 
                                        by = event_time]

# Plot the Difference Over Time
ggplot(event_study_diff, aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_line() +  # Connect the points
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Zero baseline
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Policy change line
  scale_x_continuous(breaks = seq(min(event_study_diff$event_time, na.rm = TRUE), 
                                  max(event_study_diff$event_time, na.rm = TRUE), by = 1)) +
  labs(title = "Event Study: Difference-in-Differences Estimate",
       x = "Years Since Policy Change",
       y = "Difference in Estimated Effects (Treated - Control)") +
  theme_minimal()

# Save out data 
fwrite(DiD_overall, "/Users/davidpittman/Desktop/Capstone/data/DiD_overall.csv")
fwrite(event_study_results, "/Users/davidpittman/Desktop/Capstone/data/event_study_results.csv")
fwrite(event_study_diff, "/Users/davidpittman/Desktop/Capstone/data/event_study_diff.csv")
