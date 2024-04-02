#load the required libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(fpc)
options(digits = 12)

# Load the sample csv exported from the GPS company (in this case catapult)
# Time and velocity variables selected that I am going to need in the analysis
# The sample csv contains data from a single player from a training session and two games
df <- read.csv("sample_csv.csv")

# data preparation for the analysis. "ts" variable is the timestamp seconds and "cs" variable is the timestamp centiseconds
# We want to create a new column called "time" calculating the seconds with 0 being the first data point recorded for each session/game
df <- df %>%
  group_by(activ_id) %>%
  mutate(sec = ts - min(ts)) %>%
  mutate(cs = cs / 100) %>%
  mutate(time = sec + cs) %>%
  mutate(acceleration  =  c(NA, diff(velocity) / diff(time))) %>%
  ungroup() %>%
  select(acceleration, velocity)

# remove all NAs
df <- na.omit(df, cols = "acceleration")

# As we are not interested in deceleration we remove all negative values from acceleration column
df <- df %>%
  mutate(acceleration = as.numeric(acceleration)) %>%
  filter(acceleration > 0)


######### OUTLIER DETECTION ###############

# First step is to remove any outliers (artefacts or measurement errors) based on the calculation below.
# Identification of misuse errors. 10.93 - 10.93/10.5 * speed: mean + 3 * std (see paper) 
df_clean <- df %>%
  mutate(outlier = ifelse(acceleration >= 10.93 - 10.93/10.5 * velocity, "yes", "no")) %>%
  filter(outlier == "no") %>%
  select(!outlier)

## Second step to run the DBSCAN alogithm to detect any data point without "neighbours" and hence classify it as outlier
# Because the number of data points is large we'll remove all data points less than 2 to allow the algorithm to run faster.
indexes_less_than2 <- which(df_clean$acceleration < 2)
df_less_than2 <- df_clean[indexes_less_than2,]
df_greater_than2 <- df_clean[-indexes_less_than2,]

# run the DBSCAN algorithm in the df containing values greater than 2 m/s2 only.
dbscan_result <- dbscan(df_greater_than2[,c("acceleration", "velocity")], eps = 0.5, MinPts = 3)

# Access the cluster assignments and outliers
cluster_assignments <- dbscan_result$cluster
outliers <- which(cluster_assignments == 0)

## create a dataframe by removing the outliers if applicable
if (length(outliers) > 0) {
  df_clean_greater_than2 <- df_greater_than2[-outliers,]
  df_outliers <- df_greater_than2[outliers,]
} else {
  df_clean_greater_than2 <- df_greater_than2
  df_outliers <- data.frame(velocity = NA, acceleration = NA)
}

df_clean <- rbind(df_clean_greater_than2, df_less_than2)

# visualize the data to inspect the outliers removed by the algorithm
ggplot(df_clean_greater_than2, aes(x= velocity, y = acceleration)) +
  geom_point(col = "tomato", alpha = 0.3, size = 2) +
  geom_point(data = df_outliers, aes(x = velocity, y = acceleration), col = "black", alpha = 0.7, size = 2) +
  geom_point(data = df_less_than2, aes(x = velocity, y = acceleration), col = "tomato", alpha = 0.3, size = 2) +
  see::theme_modern() +
  labs(y = "Acceleration", x = "Velocity", title = "Outlier identification")

#### FITTING THE REGRESSION LINE ###########

# Calculate the two highest acceleration values within each velocity 0.2 interval
velocity_intervals <- seq(df_clean$velocity[which.max(df_clean$acceleration)], max(df_clean$velocity), by = 0.2)
regression_data <- data.frame()

for (vel in velocity_intervals) {
  index_vel <- vel + 0.2
  
  if (index_vel > max(df_clean$velocity)) {
    break
  }
  
  df_reg <- df_clean %>%
    filter(velocity >= vel & velocity < index_vel) %>%
    arrange(desc(acceleration)) %>%
    head(2) %>%
    select(acceleration, velocity)
  
  regression_data <- rbind(regression_data, df_reg)
}

model <- lm(acceleration ~ velocity, data = regression_data)
summary(model)
intercept <- coef(model)[1]
slope <- coef(model)[2]
r_squared <- summary(model)$adj.r.squared

# Calculate the theoretical V0
v0 <- -intercept / slope
# Predict y for x = 0 which corresponds to theoretical A0
x_0 <- data.frame(velocity = 0)
A0 <- predict(model, newdata = x_0)

# these are required for the visualization
equation <- paste0("Accel = ", round(intercept, 2), " - ", abs(round(slope, 2)), " * Vel")
r_squared_text <- bquote(italic(R)^2 == .(round(r_squared, digits = 2)))

# final visual of the AS profile
ggplot(df_clean, aes(x= velocity, y = acceleration)) +
  geom_point(col = "tomato", alpha = 0.3, size = 2) +
  geom_point(data = regression_data, aes(x = velocity, y = acceleration), col = "black", alpha = 0.7, size = 3) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,9), breaks = seq(0, 9, by = 2)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,10.5), breaks = seq(0, 10.5, by = 2)) +
  annotate("text", x = 2.2, y = max(regression_data$acceleration) + 1.7, label = equation) +
  annotate("text", x = 2.2, y = max(regression_data$acceleration) + 1.2, label = r_squared_text) +
  geom_abline(intercept = intercept, slope = slope, linetype = "dashed") +
  see::theme_modern() +
  labs(y = expression("Acceleration (m/s"^2*")"), x = "Velocity (m/s)", "Model fit")


