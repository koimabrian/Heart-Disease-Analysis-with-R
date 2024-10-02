# Install required libraries (if not already installed)
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("corrplot")) install.packages("corrplot")
library(tidyverse)
library(corrplot)

# Load the heart disease dataset from URL
data_url <- 'https://raw.githubusercontent.com/koimabrian/Heart-Disease-Analysis-with-R/refs/heads/main/heart.csv'
data <- read.csv(data_url, comment = "#")  # Handle potential comments in the CSV

# Display the first few rows (head) and last few rows (tail) of the data
head(data)
tail(data)

# Get a brief overview of the data structure (including variable types)
glimpse(data)

# Determine the number of columns (variables)
ncol(data)

# Determine the number of rows (observations)
nrow(data)

# Get the names of the variables
colnames(data)

# Summarize the data (descriptive statistics)
summary(data)

# Create a transformed dataset with more descriptive variable labels
data2 <- data %>%
  mutate(
    sex = ifelse(sex == 1, "MALE", "FEMALE"),  # Convert sex to factor
    fbs = ifelse(fbs == 1, ">120", "<=120"),    # Convert fbs to binary factor
    exang = ifelse(exang == 1, "YES", "NO"),     # Convert exang to factor
    cp = ifelse(
      cp == 1, "ATYPICAL ANGINA",
      ifelse(cp == 2, "NON-ANGINAL PAIN", "ASYMPTOMATIC")  # Convert cp to factor levels
    ),
    restecg = ifelse(
      restecg == 0, "NORMAL",
      ifelse(restecg == 1, "ABNORMALITY", "PROBABLE OR DEFINITE")  # Convert restecg to factor levels
    ),
    slope = as.factor(slope),  # Convert slope to factor
    ca = as.factor(ca),        # Convert ca to factor
    thal = as.factor(thal),    # Convert thal to factor
    target = ifelse(target == 1, "YES", "NO")  # Convert target to factor for disease presence
  ) %>%
  mutate_if(is.character, as.factor) %>%  # Convert remaining character variables to factors
  dplyr::select(target, sex, fbs, exang, cp, restecg, slope, ca, thal, everything())  # Select all variables

# Bar plot to visualize the distribution of heart disease presence/absence
ggplot(data2, aes(x = target)) +
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Presence & Absence of Heart Disease") +
  scale_fill_discrete(name = 'Heart Disease', labels = c("Absence", "Presence"))

# Proportion of individuals with and without heart disease
prop.table(table(data2$target))

# Analyze age distribution (excluding rows with count less than 10 to avoid noise)
data2 %>%
  group_by(age) %>%  # Group by age (corrected variable name)
  count() %>%
  filter(n > 10) %>%  # Filter out rows with low counts
  ggplot() +
  geom_col(aes(age, n), fill = 'green') +  # Plot age vs. count (corrected variable name)
  ggtitle("Age Analysis") +
  xlab("Age") +
  ylab("Count")

# Compare blood pressure (trestbps) across chest pain types (cp) with boxplots
data2 %>%
  ggplot(aes(x = sex, y = trestbps)) +
  geom_boxplot(fill = 'purple') +
  xlab('Sex') +
  ylab('Blood Pressure') +
  facet_grid(~ cp)  # Facet by chest pain type

# Compare cholesterol (chol) across chest pain types (cp) with boxplots
data2 %>%
  ggplot(aes(x = sex, y = chol)) +
  geom_boxplot(fill = 'orange') +
  xlab('Sex') +
  ylab('Cholesterol') +
  facet_grid(~ cp)

# Create a correlation matrix
cor_heart <- cor(data2[, 10:14])
cor_heart

# Visualize the correlation matrix
corrplot(cor_heart, method = 'square', type = 'upper')