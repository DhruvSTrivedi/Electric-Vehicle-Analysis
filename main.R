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


# Pie Chart for Electric Vehicle Type

# Calculate counts for each type
vehicle_type_counts <- table(data$Electric.Vehicle.Type)

# Create the pie chart
pie(vehicle_type_counts, main="Distribution of Electric Vehicle Types", 
    col=rainbow(length(vehicle_type_counts)), 
    labels=names(vehicle_type_counts))





# Calculate counts for each brand
brand_counts <- table(data$Make)

# Convert the table into a data frame for ggplot
brand_df <- as.data.frame(brand_counts)

# Calculate percentages for each brand
brand_df$Percentage <- (brand_df$Freq / sum(brand_df$Freq)) * 100

# Create labels for the pie chart slices
brand_df$label <- ifelse(brand_df$Percentage > 5, paste0(brand_df$Var1, "\n", round(brand_df$Percentage, 1), "%"), "")

# Create the pie chart using ggplot2
ggplot(brand_df, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title="Market Share of Electric Vehicle Brands",
       fill="Brand") +
  theme_minimal() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  theme(legend.position="bottom", 
        legend.box = "horizontal",
        legend.key.size = unit(0.5, "cm")) +
  guides(fill = guide_legend(title = "Brands", nrow = 2, keywidth = 0.5, keyheight = 0.5))


# Empirical CDF for Electric Range

ggplot(data, aes(x=Electric.Range)) +
  stat_ecdf(geom = "step", color="red") +
  labs(title="Empirical CDF of Electric Range",
       x="Electric Range", y="Cumulative Probability") +
  theme_minimal()



# ECDF of Electric Range differentiated by Electric Vehicle Type
ggplot(data, aes(x = Electric.Range, color = Electric.Vehicle.Type)) +
  stat_ecdf(geom = "step") +
  labs(title = "ECDF of Electric Range by Vehicle Type",
       x = "Electric Range", y = "Cumulative Probability",
       color = "Vehicle Type") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Scatter Plot for Model Year vs Electric Range
ggplot(data, aes(x=Model.Year, y=Electric.Range)) +
  geom_point(aes(color=Make), alpha=0.6) +
  labs(title="Scatter Plot of Electric Range by Model Year",
       x="Model Year", y="Electric Range") +
  theme_minimal() +
  theme(legend.position="bottom") +
  scale_color_discrete(name="Make")
