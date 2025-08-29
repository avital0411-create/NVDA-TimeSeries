### Final Project: Regression Analysis ###

# Installing and loading required packages
library(readr)
library(ggplot2)
library(dplyr)
library(car)
library(lmtest)
library(reshape2)
library(GGally)

# Importing the dataset after removing non-significant variables
#filtered_df <- read.csv("C:/Users/avita/Desktop/R/selected_table.csv", header = TRUE, stringsAsFactors = FALSE)#

# Reading data
df <- read_csv("C:/Users/avita/Downloads/house_sales.csv")
dim(df)
head(df)
tail(df)

# Removing irrelevant variables
newdf <- df[, !names(df) %in% c("DocumentDate", "PropertyID")]

# Handling missing values
missing_values <- colSums(is.na(newdf)) 

# Function to identify outliers
find_outliers <- function(data) {
  lapply(data, function(col) {
    if (is.numeric(col)) {
      Q <- quantile(col, probs = c(0.25, 0.75), na.rm = TRUE)
      IQR <- Q[2] - Q[1]
      lower <- Q[1] - 1.5 * IQR
      upper <- Q[2] + 1.5 * IQR
      which(col < lower | col > upper)
    } else {
      NULL
    }
  })
}

outlier_indices <- find_outliers(newdf)

# Removing outliers from SalePrice
remove_outliers <- function(x) {
  if (is.numeric(x)) {
    q <- quantile(x, probs=c(0.25, 0.75), na.rm=TRUE)
    lower <- q[1] - 1.5 * IQR(x, na.rm=TRUE)
    upper <- q[2] + 1.5 * IQR(x, na.rm=TRUE)
    x[x < lower | x > upper] <- NA
  }
  x
}

newdf$SalePrice <- remove_outliers(newdf$SalePrice)
newdf <- na.omit(newdf)
summary(newdf)

# Calculating variance and standard deviation for numeric variables
numeric_df <- newdf[, sapply(newdf, is.numeric)]
apply(numeric_df, 2, var, na.rm = TRUE)
apply(numeric_df, 2, sd, na.rm = TRUE)

# Running plots on the data after removing outliers
# Distribution of Property Types
ggplot(newdf, aes(x = "", fill = factor(PropertyType))) +
  geom_bar(width = 1) + coord_polar("y") + theme_void() +
  labs(title = "Distribution of Property Types")

# New Construction Status
ggplot(newdf, aes(x = "", fill = factor(NewConstruction))) +
  geom_bar(width = 1) + coord_polar("y") + theme_void() +
  labs(title = "New Construction Status", subtitle = "0 = No, 1 = Yes")

# Traffic Noise Level
ggplot(newdf, aes(x = factor(TrafficNoise))) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Traffic Noise Level", x = "Traffic Noise", y = "Count") + theme_minimal()

# Visualization of Building Grade Distribution
ggplot(newdf, aes(x = factor(BldgGrade))) +
  geom_bar(fill = "lightblue", color = "black") + 
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) + 
  labs(title = "Building Grade Distribution", 
       x = "Building Grade", 
       y = "Count") + 
  theme_minimal()

# Visualization of the Number of Bathrooms
ggplot(newdf, aes(x = factor(Bathrooms))) +
  geom_bar(fill = "lightblue", color = "black") + 
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) + 
  labs(title = "Distribution of the Number of Bathrooms", 
       x = "Bathrooms", 
       y = "Count") + 
  theme_minimal()

# Visualization of the Number of Bedrooms
ggplot(newdf, aes(x = factor(Bedrooms))) +
  geom_bar(fill = "cornsilk", color = "black") + 
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) + 
  labs(title = "Distribution of the Number of Bedrooms", 
       x = "Bedrooms", 
       y = "Count") + 
  theme_minimal()

# Visualizing numerical variables
# Adjusted Sale Price Distribution
ggplot(newdf, aes(x = AdjSalePrice)) +
  geom_histogram(binwidth = 50000, fill = "skyblue") +
  labs(title = "Adjusted Sale Price Distribution") + theme_minimal()

# Density of Total Living Area
ggplot(newdf, aes(x = SqFtTotLiving)) +
  geom_density(fill = "skyblue") +
  labs(title = "Density of Total Living Area") + theme_minimal()

# Density plot showing the distribution of land values
ggplot(newdf, aes(x = LandVal)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Land Value Distribution", 
       x = "Land Value ($)", 
       y = "Density") +
  theme_minimal()

# Histogram showing the distribution of improvements values
ggplot(newdf, aes(x = ImpsVal)) +
  geom_histogram(binwidth = 50000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Improvements Value", 
       x = "Improvements Value ($)", 
       y = "Frequency") +
  theme_minimal()

# Histogram displaying the distribution of lot sizes
ggplot(newdf, aes(x = SqFtLot)) +
  geom_histogram(binwidth = 200, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Lot Size", 
       x = "Lot Size (SqFt)", 
       y = "Frequency") +
  theme_minimal()

# Histogram showing the distribution of finished basement areas
ggplot(newdf, aes(x = SqFtFinBasement)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Finished Basement Area", 
       x = "Finished Basement Area (SqFt)", 
       y = "Frequency") +
  theme_minimal()

# Histogram illustrating the distribution of the year the houses were built
ggplot(newdf, aes(x = YrBuilt)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Year Built", 
       x = "Year Built", 
       y = "Frequency") +
  theme_minimal()



## Testing for correlations
ggpairs(df)

# Correlation Heatmap
cor_matrix <- cor(newdf[, sapply(newdf, is.numeric)], use = "complete.obs")
melted_cor <- melt(cor_matrix)

ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() + geom_text(aes(label = round(value, 2))) +
  scale_fill_gradient(low = "cornsilk", high = "brown1") +
  labs(title = "Correlation Heatmap") +
  theme_minimal()

# Creating interaction terms
interaction_vars <- c("SqFtTotLiving", "Bathrooms", "Bedrooms", "BldgGrade", "LandVal", "ImpsVal")
dummy_vars <- c("NewConstruction", "PropertyType")

newdf[dummy_vars] <- lapply(newdf[dummy_vars], function(x) as.numeric(as.factor(x)))
for (dummy_var in dummy_vars) {
  for (numeric_var in interaction_vars) {
    interaction_name <- paste0(dummy_var, "_by_", numeric_var)
    newdf[[interaction_name]] <- newdf[[dummy_var]] * newdf[[numeric_var]]
  }
}


# Full regression model
full_model <- lm(SalePrice ~ ., data = newdf)
summary(full_model)

### Stepwise Regression: Manual Removal of Non-Significant Variables and Selection of the Final Regression Model ###

model1 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms, data = newdf)
summary(model1)

model2 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade, data = newdf)
summary(model2)

model3 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade
             - NewConstruction_by_LandVal, data = newdf)
summary(model3)

model4 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
             - NewConstruction_by_LandVal - PropertyType_by_Bathrooms, data = newdf)
summary(model4)

model5 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
             - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade, data = newdf)
summary(model5)

model6 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
             - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
             - PropertyType_by_Bedrooms, data = newdf)
summary(model6)

model7 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
             - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
             - PropertyType_by_Bedrooms - PropertyType_by_SqFtTotLiving, data = newdf)
summary(model7)

model8 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
             - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
             - PropertyType_by_Bedrooms - PropertyType_by_SqFtTotLiving - PropertyType_by_ImpsVal, data = newdf)
summary(model8)

model9 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
             - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
             - PropertyType_by_Bedrooms - PropertyType_by_SqFtTotLiving - PropertyType_by_ImpsVal - PropertyType_by_LandVal, data = newdf)
summary(model9)

model10 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
              - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
              - PropertyType_by_Bedrooms - PropertyType_by_SqFtTotLiving - PropertyType_by_ImpsVal 
              - PropertyType_by_LandVal - PropertyType, data = newdf)
summary(model10)

model11 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
              - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
              - PropertyType_by_Bedrooms - PropertyType_by_SqFtTotLiving - PropertyType_by_ImpsVal 
              - PropertyType_by_LandVal - PropertyType - zhvi_idx, data = newdf)
summary(model11)

model12 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
              - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
              - PropertyType_by_Bedrooms - PropertyType_by_SqFtTotLiving - PropertyType_by_ImpsVal 
              - PropertyType_by_LandVal - PropertyType - zhvi_idx - NbrLivingUnits, data = newdf)
summary(model12)

model13 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
              - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
              - PropertyType_by_Bedrooms - PropertyType_by_SqFtTotLiving - PropertyType_by_ImpsVal 
              - PropertyType_by_LandVal - PropertyType - zhvi_idx - NbrLivingUnits - SqFtFinBasement, data = newdf)
summary(model13) 

model14 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
              - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
              - PropertyType_by_Bedrooms - PropertyType_by_SqFtTotLiving - PropertyType_by_ImpsVal 
              - PropertyType_by_LandVal - PropertyType - zhvi_idx - NbrLivingUnits 
              - SqFtFinBasement - YrBuilt, data = newdf)
summary(model14)              

model15 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
              - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
              - PropertyType_by_Bedrooms - PropertyType_by_SqFtTotLiving - PropertyType_by_ImpsVal 
              - PropertyType_by_LandVal - PropertyType - zhvi_idx - NbrLivingUnits 
              - SqFtFinBasement - YrBuilt - YrRenovated, data = newdf)
summary(model15)              

model16 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
              - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
              - PropertyType_by_Bedrooms - PropertyType_by_SqFtTotLiving - PropertyType_by_ImpsVal 
              - PropertyType_by_LandVal - PropertyType - zhvi_idx - NbrLivingUnits 
              - SqFtFinBasement - YrBuilt - YrRenovated - TrafficNoise, data = newdf)
summary(model16) 

model17 <- lm(SalePrice ~ . - NewConstruction_by_Bathrooms - NewConstruction_by_BldgGrade 
              - NewConstruction_by_LandVal - PropertyType_by_Bathrooms - PropertyType_by_BldgGrade 
              - PropertyType_by_Bedrooms - PropertyType_by_SqFtTotLiving - PropertyType_by_ImpsVal 
              - PropertyType_by_LandVal - PropertyType - zhvi_idx - NbrLivingUnits 
              - SqFtFinBasement - YrBuilt - YrRenovated - TrafficNoise - ZipCode, data = newdf)
summary(model17)

# Optimized model with selected variables 
final_model_17 <- lm(SalePrice ~ ym + zhvi_px + AdjSalePrice + SqFtLot + SqFtTotLiving + 
    Bathrooms + Bedrooms + BldgGrade + LandVal + ImpsVal + NewConstruction + 
    NewConstruction_by_SqFtTotLiving + NewConstruction_by_Bedrooms + 
    NewConstruction_by_ImpsVal, data = newdf)
summary(final_model_17)



