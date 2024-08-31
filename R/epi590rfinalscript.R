# EPI590R Final Project 
# Daniel Herrera 

# Package loading 
library(pacman)
p_load(ggplot2, gtsummary, here, janitor, medicaldata, naniar, tidyverse)

# Reading-in the dataset. It comes from the package medicaldata
# https://github.com/higgi13425/medicaldata/tree/master/data
fapraw <- medicaldata::polyps

# Saving the data as an original raw file 
write_rds(fapraw, here::here("data", "fapraw.rds"))

# Calling the stored data for analysis
fap <- read_rds (here::here("data", "fapraw.rds"))

# Removing observations with missing data
fapnona <- na.omit(fap)

# Creating data clean folder and saving the dataset without missing data  
if (!dir.exists(here::here("data", "clean"))) {
  dir.create(here::here("data", "clean"))
}

write_rds(fapnona, here::here("data", "clean", "fapnona.rds"))

# Creating the descriptive statistics table including the overall number of
# observations, t and chi square tests and additional statistics where 
# applicable. "fap" data set was used to report two missing observations  
table1 <- tbl_summary(fap, 
            by = treatment,
            include = c(sex, age, baseline, number3m, number12m),
            label = list(
              sex ~ "Sex",
              age ~ "Age (yr)",
              baseline ~ "Base-line no. of polyps", 
              number3m ~ "No. of polyps at 3 months",
              number12m ~ "No. of polyps at 12 months"
            ),
            missing_text = "Missing", 
            statistic = list(
              sex ~ "{n} ({p})",
              age ~ "{mean} ({sd})",
              baseline ~ "{median} ({min}, {max})",
              number3m ~ "{median} ({min}, {max})",
              number12m ~ "{median} ({min}, {max})"
            ), 
            digits = everything() ~ style_sigfig 
            ) |>
  add_p(test = list(
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"
  )) |> 
  add_overall(col_label = "**Total** N = {N}") |> 
  bold_labels() |> 
  modify_header(label = "**Variable**", p.value = "***p***") 
  table1


# Creating a multiple linear regression model to evaluate the relationship
# between the number of polyps at baseline and the sex and age variables 
mlr <- lm(baseline ~ age + sex, data = fap)
regression1 <- tbl_regression(mlr,
                              intercept = FALSE,
                              label = list(sex ~ "Sex",
                                           age ~ "Age (yr)"
                                           )
                              )
regression1

# Creating a box plot to compare the number of polyps at baseline, 3 months, and 
# 12 months 
fap_long <- fap |> 
  pivot_longer(cols = c(baseline, number3m, number12m),
               names_to = "time_point",
               values_to = "num_polyps")

# Filtering the rows with missing values in num_polyps
fap_long_clean <- fap_long  |> 
  filter(!is.na(num_polyps))

# Creating the box plot
figure1 <- ggplot(fap_long_clean, aes(x = factor(time_point, 
                                      levels = c("baseline", 
                                                 "number3m", 
                                                 "number12m"),
                                      labels = c("Baseline", 
                                                 "3 months", 
                                                 "12 months")),
                           y = num_polyps, fill = treatment)) +
  geom_boxplot() +
  labs(title = "Comparison of Number of Polyps by Treatment Group",
       x = "Time Point",
       y = "Number of Polyps",
       fill = "Treatment Group") +
  scale_fill_discrete(labels = c("sulindac" = "Sulindac", 
                                 "placebo" = "Placebo")) +
  scale_fill_manual(values = c("sulindac" = "#FF7F50", "placebo" = "lightblue")) +
  theme_gray()
figure1

# Saving the figure on the project folder under a "figures" folder 
# Create the figures directory if it doesn't exist
if (!dir.exists(here::here("figures"))) {
  dir.create(here::here("figures"))
}

# Saving the plot
ggsave(filename = here::here("figures", "polyps_comparison.png"), 
       plot = figure1, 
       width = 8, height = 6, dpi = 600)

# Creating a function and using it with the data 
sdmanual <- function(data, variable) {
  var_data <- data[[variable]]
  if (!is.numeric(var_data)) {
    stop("Error! The specified variable is not continuous (numeric).")
  }
  var_data <- var_data[!is.na(var_data)]
  mean_value <- mean(var_data)
  squared_deviations <- sum((var_data - mean_value) ^ 2)
  sd_value <- sqrt(squared_deviations / (length(var_data) - 1))
  return(sd_value)
}

# Testing the new function
sdmanual(fap, "age")