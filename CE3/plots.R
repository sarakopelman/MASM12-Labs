# Load required libraries
library(ggplot2)
library(corrplot)

# Overview of dataset
str(AllDat)
summary(AllDat)

# Identify numeric and categorical variables
numeric_vars <- sapply(AllDat, is.numeric)
categorical_vars <- sapply(AllDat, is.factor)

# 1. Histograms for numeric variables
for (var in names(AllDat)[numeric_vars]) {
  print(
    ggplot(AllDat, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      theme_minimal() +
      ggtitle(paste("Histogram of", var))
  )
}

# 2. Boxplots for numeric vs categorical variables
for (num_var in names(AllDat)[numeric_vars]) {
  for (cat_var in names(AllDat)[categorical_vars]) {
    print(
      ggplot(AllDat, aes_string(x = cat_var, y = num_var)) +
        geom_boxplot(fill = "lightgreen") +
        theme_minimal() +
        ggtitle(paste("Boxplot of", num_var, "by", cat_var))
    )
  }
}

# 3. Scatterplots for first 4 numeric variables
numeric_names <- names(AllDat)[numeric_vars]
if(length(numeric_names) >= 2){
  pairs(AllDat[, numeric_names[1:min(4, length(numeric_names))]],
        main = "Pairwise Scatterplots")
}

# 4. Example ggplot scatterplot of first two numeric variables
if(length(numeric_names) >= 2){
  ggplot(AllDat, aes_string(x = numeric_names[1], y = numeric_names[2])) +
    geom_point(alpha = 0.6, color = "darkred") +
    theme_minimal() +
    ggtitle(paste("Scatterplot:", numeric_names[1], "vs", numeric_names[2]))
}

# 5. Correlation heatmap for numeric variables
if(sum(numeric_vars) >= 2){
  cor_matrix <- cor(AllDat[, numeric_vars], use = "complete.obs")
  corrplot(cor_matrix, method = "color", addCoef.col = "black",
           tl.cex = 0.8, number.cex = 0.7)
}

# 6. Barplots for categorical variables
for (cat_var in names(AllDat)[categorical_vars]) {
  print(
    ggplot(AllDat, aes_string(x = cat_var)) +
      geom_bar(fill = "skyblue") +
      theme_minimal() +
      ggtitle(paste("Barplot of", cat_var))
  )
}

# 7. Density plots for numeric variables
for (num_var in names(AllDat)[numeric_vars]) {
  print(
    ggplot(AllDat, aes_string(x = num_var)) +
      geom_density(fill = "lightblue", alpha = 0.5) +
      theme_minimal() +
      ggtitle(paste("Density plot of", num_var))
  )
}


library(ggplot2)
library(dplyr)
library(lubridate)

# Assume t is a datetime column and yTi1 is numeric
# Extract hour from t
AllDat <- AllDat %>%
  mutate(hour = hour(t))  # lubridate::hour extracts hour (0-23)

# Plot all yTi1 series against hour
ggplot(AllDat, aes(x = hour, y = yTi1, group = 1)) +
  geom_line(alpha = 0.5, color = "steelblue") +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(x = "Hour of the Day", y = "Temperature (yTi1)", 
       title = "Temperature vs Hour of Day")


library(ggplot2)
library(lubridate)

# Basic line plot over datetime
ggplot(AllDat, aes(x = date, y = yTi4)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_line(aes(group = 1), alpha = 0.3, color = "steelblue") +
  theme_minimal() +
  labs(x = "Date", y = "Temperature (yTi1)",
       title = "Temperature vs Date")


library(ggplot2)
library(dplyr)
library(lubridate)

# Extract hour and month
AllDat <- AllDat %>%
  mutate(hour = hour(date),
         month = month(date, label = TRUE))  # month as factor with names

# Option 1: Show points for all temperatures by hour, colored by month
ggplot(AllDat, aes(x = hour, y = yTi1, color = month)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(x = "Hour of the Day", y = "Temperature (yTi1)", 
       color = "Month",
       title = "Temperature by Hour Colored by Month")

# Option 2: Connect points for each day with lines, colored by month
ggplot(AllDat, aes(x = hour, y = yTi1, group = as.factor(date), color = month)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(x = "Hour of the Day", y = "Temperature (yTi1)", 
       color = "Month",
       title = "Temperature by Hour for Each Day Colored by Month")



library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# Extract hour and month
AllDat <- AllDat %>%
  mutate(hour = hour(date),
         month = month(date, label = TRUE))

# Reshape to long format for multiple temperature columns
# Replace yTi1, yTi2, yTi3 with your actual temperature columns
AllDat_long <- AllDat %>%
  pivot_longer(cols = starts_with("yTi"), 
               names_to = "temperature", 
               values_to = "value")

# Facet plot: one facet per month (columns), one facet per temperature (rows)
ggplot(AllDat_long, aes(x = hour, y = value)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  facet_grid(temperature ~ month) +  # rows: temperature, columns: month
  theme_minimal() +
  labs(x = "Hour of the Day", y = "Temperature",
       title = "Temperature by Hour for Each Month and Each Temperature")


# Select temperature columns
temp_cols <- grep("^yTi", names(AllDat), value = TRUE)

# Initialize a dataframe to store RMSE
rmse_results <- data.frame(Room = temp_cols, RMSE = NA)

# Loop over each temperature column
for (col in temp_cols){
  
  # Arrange by time
  data_room <- AllDat %>% arrange(t)
  
  temps <- data_room[[col]]
  
  # Naive prediction: previous value
  pred <- dplyr::lag(temps, 1)
  
  # Remove first NA
  valid_idx <- !is.na(pred)
  pred <- pred[valid_idx]
  actual <- temps[valid_idx]
  
  # Compute RMSE manually
  rmse_val <- sqrt(mean((actual - pred)^2))
  
  # Store result
  rmse_results$RMSE[rmse_results$Room == col] <- rmse_val
}

# Show RMSE for each room
rmse_results





# Extract hour
# Extract hour and month
AllDat <- AllDat %>%
  mutate(hour = hour(date),
         month = month(date, label = TRUE))

# Extract hour
AllDat <- AllDat %>%
  mutate(hour = hour(t))  # replace t with your datetime column

# Example for one temperature column: yTi1

# Plot 1: Temperature vs Hour
p_temp <- ggplot(AllDat, aes(x = hour, y = yTi1)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  theme_minimal() +
  labs(x = "Hour", y = "Temperature", title = "Temperature vs Hour")

# Plot 2: Gv vs Hour
p_gv <- ggplot(AllDat, aes(x = hour, y = Gv)) +
  geom_point(alpha = 0.7, color = "orange") +
  theme_minimal() +
  labs(x = "Hour", y = "Gv", title = "Gv vs Hour")

# Plot 3: Ambient Temperature Ta vs Hour
p_ta <- ggplot(AllDat, aes(x = hour, y = Ta)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  theme_minimal() +
  labs(x = "Hour", y = "Ta", title = "Ambient Temperature vs Hour")

# Combine side by side
p_temp + p_gv + p_ta










# Extract hour
AllDat <- AllDat %>%
  mutate(hour = hour(t))  # replace t with your datetime column

# Reshape temperature columns to long format
temp_cols <- grep("^yTi", names(AllDat), value = TRUE)
AllDat_long <- AllDat %>%
  pivot_longer(cols = all_of(temp_cols),
               names_to = "Room",
               values_to = "Temperature")

# Plot 1: Temperature vs Hour, faceted by Room
p_temp <- ggplot(AllDat_long, aes(x = hour, y = Temperature)) +
  geom_point(alpha = 0.7, color = "steelblue") +
  facet_wrap(~ Room, ncol = 2) +  # adjust ncol for layout
  theme_minimal() +
  labs(x = "Hour", y = "Temperature", title = "Temperature vs Hour for All Rooms")

# Plot 2: Gv vs Hour (same for all rooms)
p_gv <- ggplot(AllDat, aes(x = hour, y = Gv)) +
  geom_point(alpha = 0.7, color = "orange") +
  theme_minimal() +
  labs(x = "Hour", y = "Gv", title = "Gv vs Hour")

# Plot 3: Ambient Temperature Ta vs Hour
p_ta <- ggplot(AllDat, aes(x = hour, y = Ta)) +
  geom_point(alpha = 0.7, color = "darkgreen") +
  theme_minimal() +
  labs(x = "Hour", y = "Ta", title = "Ambient Temperature vs Hour")

# Combine side by side
p_temp + p_gv + p_ta



library(ggplot2)
library(dplyr)
library(lubridate)

# Make sure month is extracted
AllDat <- AllDat %>%
  mutate(month = month(t, label = TRUE))  # replace t with your datetime column


# Faceted density plot
ggplot(AllDat, aes(x = yTi1)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  facet_wrap(~ month, nrow = 2) +  # only specify nrow, columns auto-adjust
  theme_minimal() +
  labs(x = "yTi1 Temperature", y = "Density",
       title = "Density of yTi1 by Month (2 rows)")




library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Extract month


# Select temperature columns (rooms)
temp_cols <- grep("^yTi", names(AllDat), value = TRUE)

# Reshape to long format
AllDat_long <- AllDat %>%
  pivot_longer(cols = all_of(temp_cols),
               names_to = "Room",
               values_to = "Temperature")

# Create a density plot for each room with facets for each month
for (room_name in unique(AllDat_long$Room)) {
  
  room_data <- AllDat_long %>% filter(Room == room_name)
  
  p <- ggplot(room_data, aes(x = Temperature)) +
    geom_density(fill = "lightblue", alpha = 0.5) +
    facet_wrap(~ month, nrow = 2) +  # adjust nrow/ncol as needed
    theme_minimal() +
    labs(x = "Temperature", y = "Density",
         title = paste("Density of Temperatures by Month -", room_name))
  
  print(p)
}



library(ggplot2)
library(dplyr)
library(tidyr)

# Select only numeric columns
numeric_vars <- sapply(AllDat, is.numeric)
AllDat_num <- AllDat[, numeric_vars]

# Compute correlation matrix
corr_mat <- cor(AllDat_num, use = "complete.obs")

# Convert to long format for ggplot
corr_long <- as.data.frame(as.table(corr_mat))
names(corr_long) <- c("Var1", "Var2", "Correlation")

# Heatmap
ggplot(corr_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap", x = "", y = "")



# Select temperature columns
temp_cols <- grep("^yTi", names(AllDat), value = TRUE)

# Reference room
ref_room <- "yTi1"

# Compute differences
diffs <- AllDat[, temp_cols] %>%
  select(-ref_room) %>%
  sweep(AllDat[, ref_room], 1, FUN = "-")  # subtract yTi1

# Compute correlations
correlations <- sapply(diffs, function(x) cor(AllDat[[ref_room]], x, use = "complete.obs"))

# Print correlations
print(round(correlations, 2))





# Room columns
rooms <- c("YTi1", "YTi2", "YTi3", "YTi4")  # adjust to all your columns
n <- length(rooms)

# Dataframe to store correlations
cor_results <- data.frame(Room = character(),
                          OtherRoom = character(),
                          Correlation = numeric())

# Compute correlations
for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) {
      diff_ij <- AllDat[[rooms[i]]] - AllDat[[rooms[j]]]
      corr_ij <- cor(AllDat[[rooms[i]]], diff_ij)
      cor_results <- rbind(cor_results, data.frame(Room = rooms[i],
                                                   OtherRoom = rooms[j],
                                                   Correlation = corr_ij))
    }
  }
}

# Plot as bar plot
library(ggplot2)
ggplot(cor_results, aes(x = Room, y = Correlation, fill = OtherRoom)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Correlation between Room Temperature and Differences with Other Rooms",
       y = "Correlation",
       x = "Room")







