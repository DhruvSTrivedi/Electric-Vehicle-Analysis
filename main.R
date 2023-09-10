# Load necessary libraries
library(ggplot2)
library(scales)

# Read the data
data <- read.csv("Electric_Vehicle_Population_Data.csv")

# Histogram for Model Year (Standard Frequency)
ggplot(data, aes(x=Model.Year)) +
  geom_histogram(binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Histogram of Model Year (Standard Frequency)",
       x="Model Year", y="Frequency")

# Histogram for Model Year (Relative Frequency)
ggplot(data, aes(x=Model.Year)) +
  geom_histogram(aes(y=..count../sum(..count..)), binwidth=1, fill="blue", color="black", alpha=0.7) +
  labs(title="Histogram of Model Year (Relative Frequency)",
       x="Model Year", y="Relative Frequency")

# Bar Chart for Binned Electric Range with Adjusted y-axis

# Create bins for Electric Range
data$ElectricRangeBin <- cut(data$Electric.Range, breaks=c(0, 50, 100, 150, 200, 250, 300, max(data$Electric.Range)), 
                             labels=c("0-50", "50-100", "100-150", "150-200", "200-250", "250-300", "300+"), 
                             include.lowest=TRUE)

# Create the bar chart for Electric Range bins
ggplot(data, aes(x=ElectricRangeBin)) +
  geom_bar(fill="purple", color="black", alpha=0.7) +
  labs(title="Distribution of Binned Electric Range",
       x="Electric Range (miles)", y="Count") +
  scale_y_continuous(labels = comma) + # This line adjusts the y-axis format
  theme_minimal()

# Bar Chart for Binned Base MSRP with Adjusted y-axis

# Create bins for Base MSRP
data$BaseMSRPBin <- cut(data$Base.MSRP, breaks=c(0, 20000, 40000, 60000, 80000, 100000, max(data$Base.MSRP)), 
                        labels=c("0-20k", "20k-40k", "40k-60k", "60k-80k", "80k-100k", "100k+"), 
                        include.lowest=TRUE)

# Create the bar chart for Base MSRP bins
ggplot(data, aes(x=BaseMSRPBin)) +
  geom_bar(fill="pink", color="black", alpha=0.7) +
  labs(title="Distribution of Binned Base MSRP",
       x="Base MSRP ($)", y="Count") +
  scale_y_continuous(labels = comma) + # This line adjusts the y-axis format
  theme_minimal()
