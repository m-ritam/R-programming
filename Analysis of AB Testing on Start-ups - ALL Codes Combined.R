#Table 1 - Panel A
pacman::p_load(tidyverse, haven, estimatr, fixest,
               modelsummary)
setwd("C:/Users/Ritam/OneDrive/Desktop/DSC/Data")

data01 <- read_dta("ab_data.dta")

print(names(data01))

library(knitr)
library(kableExtra)

# Extract summary statistics for each column
summary_using_ab_only <- summary(data01$using_ab_only)[c("Mean", "Median", "Min.", "Max.")]
summary_log1p_pageviews <- summary(data01$log1p_pageviews)[c("Mean", "Median", "Min.", "Max.")]
summary_log1p_stack2 <- summary(data01$log1p_stack2)[c("Mean", "Median", "Min.", "Max.")]

# Combine the summary statistics into a data frame
summary_table <- data.frame(
  Column = c("using_ab_only", "log1p_pageviews", "log1p_stack2"),
  Mean = c(summary_using_ab_only[1], summary_log1p_pageviews[1], summary_log1p_stack2[1]),
  Median = c(summary_using_ab_only[2], summary_log1p_pageviews[2], summary_log1p_stack2[2]),
  Min = c(summary_using_ab_only[3], summary_log1p_pageviews[3], summary_log1p_stack2[3]),
  Max = c(summary_using_ab_only[4], summary_log1p_pageviews[4], summary_log1p_stack2[4])
)

# Print the summary table using kable
kable(summary_table, align = "c") %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Panel A: Start-up–week level" = 5))


#Table 1 - Panel B
data02 <- read_dta("ab_data.dta")

print(names(data02))

# Not angel/VC funded

not_angel_vc_funded <- data02 %>%
  filter(firm_tag & !sum_funded) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )

not_angel_vc_funded

# Angel/VC funded

angel_vc_funded <- data02 %>%
  filter(firm_tag & sum_funded) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
angel_vc_funded

founded_2012_13 <- data02 %>%
  filter(firm_tag & sum_1213) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
founded_2012_13

founded_2010_11 <- data02 %>%
  filter(firm_tag & sum_1011) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
founded_2010_11


founded_2008_09 <- data02 %>%
  filter(firm_tag & sum_0809) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
founded_2008_09


outside_us <-  data02 %>%
  filter(firm_tag & sum_foreign & !is.na(sum_foreign)) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
outside_us


in_us_outside_bay <- data02 %>%
  filter(firm_tag & sum_us & !is.na(sum_foreign)) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
in_us_outside_bay


bay_area<- data02 %>%
  filter(firm_tag & sum_bay & !sum_foreign) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
bay_area

small_1_10_employees <- data02 %>%
  filter(firm_tag & sum_small) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
small_1_10_employees


large_11_employees <- data02 %>%
  filter(firm_tag & sum_large) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
large_11_employees


fewer_than_1500_weekly <- data02 %>%
  filter(firm_tag & sum_views_low) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
fewer_than_1500_weekly


more_than_1500_weekly <- data02 %>%
  filter(firm_tag & sum_views_high) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
more_than_1500_weekly


commerce_shopping <- data02 %>%
  filter(commerce & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
commerce_shopping


advertising <- data02 %>%
  filter(advertising & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
advertising

internet_services <- data02 %>%
  filter(internet & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
internet_services


software <- data02 %>%
  filter(software & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
software

data_analysis <- data02 %>%
  filter(data & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
data_analysis

apps <- data02 %>%
  filter(apps & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
apps

content_publishing <- data02 %>%
  filter(content & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
content_publishing

financial_services <- data02 %>%
  filter(financial & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
financial_services

education <- data02 %>%
  filter(education & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
education

information_tech <- data02 %>%
  filter(information & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
information_tech

healthcare <- data02 %>%
  filter(health & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
healthcare

hardware <- data02 %>%
  filter(hardware & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )
hardware



# Other (Last)

other <- data02 %>%
  filter(other & firm_tag) %>%
  summarize(
    Number_of_startups = n(),
    Percentage_AB_testing = mean(ab_max)
  )

other

Panel_B <- bind_rows(
  tibble(Condition = "Not_angel_vc_funded", not_angel_vc_funded),
  tibble(Condition = "Angel_vc_funded", angel_vc_funded),
  tibble(Condition = "Founded_2012_13", founded_2012_13),
  tibble(Condition = "Founded_2010_11", founded_2010_11),
  tibble(Condition = "Founded_2008_09", founded_2008_09),
  tibble(Condition = "Outside_us", outside_us),
  tibble(Condition = "In_us_outside_bay", in_us_outside_bay),
  tibble(Condition = "bay_area", bay_area),
  tibble(Condition = "Small_1_10_employees", small_1_10_employees),
  tibble(Condition = "Large_11_employees", large_11_employees),
  tibble(Condition = "Fewer_than_1500_weekly", fewer_than_1500_weekly),
  tibble(Condition = "More_than_1500_weekly", more_than_1500_weekly),
  tibble(Condition = "Commerce_Shopping", commerce_shopping),
  tibble(Condition = "Advertising", advertising),
  tibble(Condition = "Internet_Services", internet_services),
  tibble(Condition = "Software", software),
  tibble(Condition = "Data_Analysis", data_analysis),
  tibble(Condition = "Apps", apps),
  tibble(Condition = "Content_Publishing", content_publishing),
  tibble(Condition = "Financial_Services", financial_services),
  tibble(Condition = "Education", education),
  tibble(Condition = "Information_Tech", information_tech),
  tibble(Condition = "Healthcare", healthcare),
  tibble(Condition = "Hardware", hardware),
  tibble(Condition = "Other", other)
)
library(knitr)
library(kableExtra)

kable(Panel_B, align = "c") %>%
  kable_styling(full_width = FALSE, font_size = 12) %>%
  add_header_above(c("Panel B: Start-up level" = 3))

#Table 2

m01 <- feols(
  log1p_pageviews ~ using_ab_only | week_id,
  cluster = data01$firm_id,
  data = data01
)
modelsummary(m01)

m02 <- feols(
  log1p_pageviews ~ using_ab_only + log1p_stack2 | firm_id,
  cluster = data01$firm_id,
  data = data01
)
summary(m02)

m03 <- feols(
  log1p_pageviews ~ using_ab_only | week_id + firm_id,
  cluster = data01$firm_id,
  data = data01
)
summary(m03)

m04 <- feols(
  log1p_pageviews ~ using_ab_only + log1p_stack2 | 
    week_id + firm_id,
  cluster = data01$firm_id,
  data = data01
)
summary(m04)

modelsummary(list(m01, m02, m03, m04), stars = T)

#Table 2 Figure 1
#model 1
baseline <- feols(
  log1p_pageviews ~ using_ab_only + log1p_stack2 | week_id + firm_id,
  cluster = ~firm_id,
  data = data01
)


#model 2
slope <- feols(
  log1p_pageviews ~ using_ab_only + log1p_stack2 | week_id + week_id:firm_id,
  cluster = ~firm_id,
  data = data01
)

#model 3
event_study <- feols(
  log1p_pageviews ~ using_ab_only + log1p_stack2 | week_id + firm_id,
  cluster = ~firm_id,
  data = subset(data01, ab_switcher == 1)  # Assuming ab_switcher is a binary variable
)

#model 4
event_study_slope <- feols(
  log1p_pageviews ~ using_ab_only + log1p_stack2 + week_id:firm_id | week_id + week_id:firm_id,
  cluster = ~firm_id,
  data = subset(data01, ab_switcher == 1)  # Assuming ab_switcher is a binary variable
)

#model 5
asinh_baseline <- feols(
  asinh_pageviews ~ using_ab_only + log1p_stack2 | week_id + firm_id,
  cluster = ~firm_id,
  data = data01
)

#model 6 
asinh_slope <- feols(
  asinh_pageviews ~ using_ab_only + log1p_stack2 | week_id + week_id:firm_id,
  cluster = ~firm_id,
  data = data01
)


# Model 7
asinh_event_study <- feols(
  asinh_pageviews ~ using_ab_only + log1p_stack2 + week_id:firm_id | week_id + week_id:firm_id,
  cluster = ~firm_id,
  data = subset(data01, ab_switcher == 1)  # Assuming ab_switcher is a binary variable
)


# Model 8
asinh_event_study_slope <- feols(
  asinh_pageviews ~ using_ab_only + log1p_stack2 + week_id:firm_id | week_id + week_id:firm_id,
  cluster = ~firm_id,
  data = subset(data01, ab_switcher == 1)  # Assuming ab_switcher is a binary variable
)

# Model 9
censored_baseline <- feols(
  log1p_pageviews ~ using_ab_only + log1p_stack2 | week_id + firm_id,
  cluster = ~firm_id,
  data = data01[data01$pageviews > 0, ]  # Filter for pageviews > 0
)

#model10
censored_slope <- feols(
  log1p_pageviews ~ using_ab_only + log1p_stack2 | week_id + week_id:firm_id,
  cluster = ~firm_id,
  data = data01[data01$pageviews > 0, ]
)

#model 11
censored_event_study <- feols(
  asinh_pageviews ~ using_ab_only + log1p_stack2 | week_id + firm_id,
  cluster = ~firm_id,
  data = subset(data01[data01$pageviews > 0, ], ab_switcher == 1)  # Assuming ab_switcher is a binary variable
)


#model 12
censored_event_study_slope <- feols(
  asinh_pageviews ~ using_ab_only + log1p_stack2 | week_id + week_id:firm_id,
  cluster = ~firm_id,
  data = subset(data01[data01$pageviews > 0, ], ab_switcher == 1)  # Assuming ab_switcher is a binary variable
)

nums <- c(26, 52, 208)

# Iterate over the numbers
foreach(num = nums) %do% {
  # Filter data for specific week_id values
  filtered_data <- data01 %>%
    filter(week_id %in% c(1, num))
  
  # Run regression with high-dimensional fixed effects
  model <- felm(log1p_pageviews ~ using_ab_only + log1p_stack2 | week_id + firm_id | 0 | firm_id, 
                data = filtered_data, cluster = "firm_id")
  
  # Store estimates
  assign(paste0("normal_m", num), model, envir = .GlobalEnv)
}

# Iterate over the numbers
foreach(num = nums) %do% {
  # Filter data for specific week_id values and ab_switcher
  filtered_data <- data01 %>%
    filter(week_id %in% c(1, num) & ab_switcher)
  
  # Run regression with high-dimensional fixed effects
  model <- felm(log1p_pageviews ~ using_ab_only + log1p_stack2 | week_id + firm_id | 0 | firm_id, 
                data = filtered_data, cluster = "firm_id")
  
  # Store estimates
  assign(paste0("event_m", num), model, envir = .GlobalEnv)
}

# Define the models to plot
models <- list(
  "baseline" = baseline,
  "asinh_baseline" = asinh_baseline,
  "censored_baseline" = censored_baseline,
  "slope" = slope,
  "asinh_slope" = asinh_slope,
  "censored_slope" = censored_slope,
  "normal_m26" = normal_m26,
  "normal_m52" = normal_m52,
  "normal_m208" = normal_m208,
  "event_study" = event_study,
  "asinh_event_study" = asinh_event_study,
  "censored_event_study" = censored_event_study,
  "event_study_slope" = event_study_slope,
  "asinh_event_study_slope" = asinh_event_study_slope,
  "censored_event_study_slope" = censored_event_study_slope,
  "event_m26" = event_m26,
  "event_m52" = event_m52,
  "event_m208" = event_m208
)


# Plotting coefficient plot
modelplot(models, yline = 0, vertical = TRUE, legend = TRUE,
          ytitle = "A/B Testing Coefficient", xlabel = NULL,
          title = "Average impact of A/B testing on a startup's logged page visits")




#Table 2 fig 2
data02 <- read_dta("ab_data.dta")

# Drop ab_max ab_switcher
data02 <- data02 %>%            
  select(-ab_max, -ab_switcher)

# Generating ab_max and ab_min by firm_id
data02 <- data02 %>%
  group_by(firm_id) %>%
  mutate(ab_max = max(using_ab_only),
         ab_min = min(using_ab_only))

# Generating ab_switcher
data02 <- data02 %>%
  mutate(ab_switcher = ab_max - ab_min)

# Generating monthly data
data02 <- data02 %>%
  filter(ab_switcher==1) %>%
  group_by(firm_id, week_id) %>%
  mutate(switch = using_ab_only != lag(using_ab_only)) %>%
  group_by(firm_id) %>%
  mutate(sum_switch = sum(switch)) %>%
  filter(sum_switch == 2)


# Assuming week_date is in "YYYY-MM-DD" format
data02 <- data02 %>%
  mutate(week_date = as.Date(week_date),
         month_date = as.Date(format(week_date, "%m-%d")))


# Collapsing data
data02_summary <- data02 %>%
  group_by(firm_id, week_date) %>%
  summarize(using_ab_only_max = max(using_ab_only),
            log1p_stack2_max = max(log1p_stack2),
            log1p_pageviews_mean = mean(log1p_pageviews))


# Setting up panel data
data02_panel <- data02_summary %>%
  mutate(month_date = as.Date(week_date)) %>%
  ungroup() %>%
  select(-c(log1p_stack2_max)) %>%
  spread(key = "variable", value = "value") %>%
  arrange(firm_id, week_date) %>%
  mutate(week_date = as.yearmon(week_date)) %>%
  rename_at(vars(starts_with("log1p")), ~paste0("log1p_", gsub("log1p_", "", .)))

# Generating using_off and using_on
data02_panel <- data02_panel %>%
  group_by(firm_id) %>%
  mutate(using_off = (using_ab_only == 1) & (lag(using_ab_only) == 0),
         using_on = (using_ab_only == 1) & (lead(using_ab_only) == 0)) %>%
  ungroup()

# Generating adopter and disadopter
data02_panel <- data02_panel %>%
  group_by(firm_id) %>%
  mutate(adopter = max(using_on),
         disadopter = max(using_off)) %>%
  ungroup()

# Dropping rows with no adoption or disadoption
data02_panel <- data02_panel %>%
  filter(adopter == 1 | disadopter == 1)

# Generating event_date and months_from_event    ????
data02_panel <- data02_panel %>%
  group_by(firm_id) %>%
  mutate(event_date = max(month_date[using_on == 1 | using_off == 1]),
         months_from_event = ifelse(adopter == 1, month_date - event_date, event_date - month_date)) %>%
  ungroup()

# Generating lag_log1p_pageviews
data02_panel <- data02_panel %>%
  group_by(firm_id) %>%
  mutate(lag_log1p_pageviews = lag(log1p_pageviews)) %>%
  ungroup()

# Dropping NAs in lag_log1p_pageviews
data02_panel <- data02_panel %>%
  filter(!is.na(lag_log1p_pageviews))


# 18 month
library(plm)
library(fixest)

# Convert 'data02_panel' to a panel data frame
data_panel <- pdata.frame(data02_panel, index = c("firm_id", "month_date"))

# Run event study regression with 18-month window
event_study <- felm(log1p_pageviews ~ log1p_stack2 | firm_id + as.factor(month_date) |
                      0 | firm_id + as.factor(months_from_event) | data_panel,
                    data = data_panel, leads = 18, lags = 18)

# Plotting the coefficients
plot(event_study, xlab = "Months from Event", ylab = "Event Study Coefficient",
     main = "(A) 18-month window", xlim = c(-18, 18))


#Figure 4
data03 <- read_dta("ab_wayback.dta")


# Run the regression with fixed effects and clustered standard errors
m1 <- felm(
  log_lines_changed ~ using_ab_only + log1p_stack2 + log_lag_lines | firm_id2 + lag_month + months_diff | 0 | firm_id2, data = data03, cluster = "firm_id2")

summary(m1)

m2 <- felm(
  big_change ~ using_ab_only + log1p_stack2 + log_lag_lines | firm_id2 + lag_month + months_diff | 0 | firm_id2, data = data03, cluster = "firm_id2")

summary(m2)

m6 <- felm(
  structural_dsim ~ using_ab_only + log1p_stack2 + log_lag_lines | firm_id2 + lag_month + months_diff | 0 | firm_id2, data = data03, cluster = "firm_id2")

summary(m6)

m7 <- felm(
  style_dsim ~ using_ab_only + log1p_stack2 + log_lag_lines | firm_id2 + lag_month + months_diff | 0 | firm_id2, data = data03, cluster = "firm_id2")

summary(m7)


data04 <- read_dta("ab_product_metrics.dta")

m12 <- felm(product_weeks_cumulative ~ using_ab_only + log1p_stack2 | week_id + firm_id | 0 | firm_id, data = data04[data04$funded_from_start == TRUE, ], cluster = "firm_id")

summary(m12)

library(coefplot)

# Assuming you have stored the regression results in objects m1, m2, m6, m7, and m12

# Create a list of regression results
regression_results <- list(m1, m6, m7, m2, m12)

# Create a list of labels for each regression result
regression_labels <- c("Log(Lines of code changed + 1)",
                       "Relative change in HTML structure",
                       "Relative change in CSS style",
                       "Major code change (Top 5%)",
                       "Products launched")

# Plot the coefficients
modelplot(regression_results, 
          by.label = regression_labels, 
          by.label.lab = regression_labels, 
          ylabel = "", 
          keep = "using_ab_only", 
          xline = 0, 
          grid = FALSE, 
          xtitle = "A/B Testing Coefficient", 
          ytitle = "Impact", 
          title = "Impact of A/B testing on a startup's product strategy", 
          xlabsize = "small", 
          ylabsize = "small", 
          title.size = "small", 
          xlabel = seq(-0.05, 0.15, by = 0.05))


#Figure 5

data05 <- read_dta("ab_data.dta")

# Model 1
m1 <- plm(using_ab_only ~ x0 + log1p_stack2, data = data05, index = c("week_id", "firm_id"), model = "within", effect = "twoways", cluster = "group")
m1est <- summary(m1)

# Model 2
m2 <- plm(using_ab_only ~ x1 + log1p_stack2, data = data05, index = c("week_id", "firm_id"), model = "within", effect = "twoways", cluster = "group")
m2est <- summary(m2)

# Model 3
m3 <- plm(using_ab_only ~ x500 + log1p_stack2, data = data05, index = c("week_id", "firm_id"), model = "within", effect = "twoways", cluster = "group")
m3est <- summary(m3)

# Model 4
m4 <- plm(using_ab_only ~ x5k + log1p_stack2, data = data05, index = c("week_id", "firm_id"), model = "within", effect = "twoways", cluster = "group")
m4est <- summary(m4)

# Model 5
m5 <- plm(using_ab_only ~ x50k + log1p_stack2, data = data05, index = c("week_id", "firm_id"), model = "within", effect = "twoways", cluster = "group")
m5est <- summary(m5)


models <- list(m1est, m2est, m3est, m4est, m5est)
labels <- c("0 Visits", "1-499 Visits", "500-4,999 Visits", "5,000-49,999 Visits", "50,000+ Visits")

# Create coefficient plot
coefplot(models, bylabel = labels, bycoefs = TRUE, xline = 0, ylabel = list(size = "small"), main = "Startups that A/B test are more likely to scale or fail", ylab = "Visits")
