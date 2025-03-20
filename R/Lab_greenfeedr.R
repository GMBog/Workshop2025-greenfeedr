
################################################################################
#                                                                              #
#                        GreenFeed Workshop 2025                               #
#                 Instructor: Guillermo Martinez-Boggio                        #
#                                                                              #
#       greenfeedr: An R Package for Processing & Reporting GreenFeed Data     #
#                                                                              #
################################################################################


# Step 0. Installation and Loading Packages -------------------------------------
 
# Install released version from CRAN repository (https://cran.r-project.org/web/packages/greenfeedr/index.html)
if (!requireNamespace("greenfeedr", quietly = TRUE)) {
  install.packages("greenfeedr")
}
library(greenfeedr)
packageVersion("greenfeedr")

# Install development version from GitHub (optional) (https://github.com/GMBog/greenfeedr)
#if (!requireNamespace("remotes", quietly = TRUE)) {
#  install.packages("remotes")
#}
#remotes::install_github("GMBog/greenfeedr", force = TRUE)
#library(greenfeedr)
#packageVersion("greenfeedr")

library(dplyr)
library(ggplot2)

# -------------------------------------------------------------------------------

# Step 1. Downloading Data ------------------------------------------------------

# Get help on function usage
?get_gfdata   # Shows documentation if the package is loaded
??get_gfdata  # Searches for get_gfdata in all installed packages


# Set directory
setwd("/Users/GuillermoMartinez/Documents/Courses/GreenFeed/Workshop 2025/Lab/")

# Download raw data ('feedtimes', 'rfids', or 'commands')
Start_time <- Sys.time()  # Start time
get_gfdata(user = Sys.getenv("GF_USER"), 
           pass = Sys.getenv("GF_PASS"), 
           d = "feed",                       #"rfid", #"cmds"
           exp = "EXP1", 
           unit = c(304,305),                #"212", "592,593", c("592","593")
           start_date = "02/12/2024",        #DD/MM/YYYY: "02/12/2024", "2024-12-02"
           end_date = "06/02/2025",          #DD/MM/YYYY: "06/02/2025", "2025-02-06"
           save_dir = "Results/")
elapsed_time <- round(Sys.time() - Start_time, 2)  # Running time
message("Download completed in ", elapsed_time, " seconds.\n", 
        "Note: The web interface takes ~16 minutes (~960 seconds) for the same period.")

# Download Preliminary Data
Start_time <- Sys.time()  # Start time
get_gfdata(user = Sys.getenv("GF_USER"), 
           pass = Sys.getenv("GF_PASS"), 
           #d = "visits",         #By default the function download prelim data ("visits")
           exp = "EXP1", 
           unit = 592,
           start_date = "2025-02-02", 
           end_date = "2025-02-06", 
           save_dir = "Results/")
elapsed_time <- round(Sys.time() - Start_time, 2)  # Running time
message("Download completed in ", elapsed_time, " seconds.")



# How to Declare the Order of Arguments in Functions

## Option 1: Named Arguments (Order Does Not Matter)
get_gfdata(start_date = "2025-02-02", 
           end_date = "2025-02-06",
           user = Sys.getenv("GF_USER"), 
           pass = Sys.getenv("GF_PASS"), 
           #d = "visits",  # By default, the function downloads the emissions ("visits")
           unit = 592,
           save_dir = "Results/",
           exp = "EXP1")

## Option 2: Positional arguments (Order Matters!)
?get_gfdata # Check the correct order first
get_gfdata(Sys.getenv("GF_USER"), 
           Sys.getenv("GF_PASS"), 
           "visits",   # Try commenting out this argument to see an error message due to incorrect positioning
           "EXP1", 
           592,
           "2025-02-02", 
           "2025-02-06", 
           "Results/")


# -------------------------------------------------------------------------------

# Step 2. Reporting data --------------------------------------------------------

?report_gfdata

# Generate a Final Report in PDF

# Set the working directory
setwd("/Users/GuillermoMartinez/Documents/Courses/GreenFeed/Workshop 2025/Lab/")

# Run function report gf_data using the finalized data
report_gfdata(input_type = "final",
              exp = "EXP1", 
              unit = c(10, 11), 
              start_date = "2011-03-13",
              end_date = "2011-06-21", 
              save_dir = "Results/", 
              plot_opt = "All", 
              rfid_file = "EXP1_EID.xlsx", 
              file_path = "GreenFeed_Summarized_fData.xlsx")



# Create a Daily Report in PDF 

## Downloading and Reporting Preliminary Data from a running study

# Set the working directory
setwd("/Users/GuillermoMartinez/Documents/Projects/Project_UW_GCIGreenFeed/Methane/Studies/FP720/")

# Run function report_gfdata using the preliminary data
report_gfdata(input_type = "prelim",
              exp = "EXP1",
              unit = 716,
              start_date = "2025-02-10", 
              #end_date = Sys.Date(),
              save_dir = "/Users/GuillermoMartinez/Downloads/",
              plot_opt = "All",
              rfid_file = "FP720_EID.xlsx",
              user = Sys.getenv("GF_USER"), 
              pass = Sys.getenv("GF_PASS"))


## Downloading and Reporting Preliminary Data for multiple running studies

# Creating a list of studies to run function report_gfdata in loop
FID <- list(
  
  list(
    exp = "EXP1",
    unit = c(579,648),
    start_date = "2025-02-21",
    dir = "/Users/GuillermoMartinez/Downloads/"
  ),
  
  list(
    exp = "EXP2",
    unit = c(592,593),
    start_date = "2025-02-20",
    dir = "/Users/GuillermoMartinez/Downloads/"
  )
  
)

for (element in FID) {
  report_gfdata(input_type = "prelim",
                exp = element$exp,
                unit = element$unit,
                start_date = element$start_date,
                end_date = Sys.Date(),
                save_dir = element$dir,
                plot_opt = "CH4",
                #rfid_file = element$EID,
                user = Sys.getenv("GF_USER"),
                pass = Sys.getenv("GF_PASS"))
  }
  

# -------------------------------------------------------------------------------

# Step 3. Processing & Analyzing data -------------------------------------------

?process_gfdata

# Set the working directory
setwd("/Users/GuillermoMartinez/Documents/Courses/GreenFeed/Workshop 2025/Lab/")

# Load the finalized GreenFeed data
Summarized_Data <- readxl::read_excel("GreenFeed_Summarized_fData.xlsx")
head(Summarized_Data)             # Display the first few rows of the dataset   
summary(Summarized_Data[,1:10])   # Generate summary statistics for the first 10 columns

# Load the RFID file containing animal IDs
EID <- readxl::read_excel("EXP1_EID.xlsx")
head(EID)                         # Display the first few rows of the RFID data  

# Evaluate what are the best parameters to filter data
eval <- eval_gfparam(data = Summarized_Data,
                     start_date = "2011-04-13",
                     end_date = "2011-06-21")

head(eval, n=20)                  # Display the first 20 rows of the RFID data 
parameters <- eval[1,]           # Choose your parameters

# Process data using a set of parameters for a specific period
data <- process_gfdata(data = Summarized_Data,
                       start_date = "2011-04-13",
                       end_date = "2011-06-21",
                       param1 = parameters$param1,
                       param2 = parameters$param2,
                       min_time = parameters$min_time
                       )

# Function returns 3 data frames:
#1. Summarized data filtered (outliers and low airflow)
filtered_data <- data$filtered_data
head(filtered_data)

#2. Daily averages
daily_data <- data$daily_data
head(daily_data)

#3. Weekly averages
weekly_data <- data$weekly_data
head(weekly_data)

# Merge weekly averages with animal information to filter out animals not in the study
weekly_data <- weekly_data %>%
  dplyr::inner_join(EID, by = "RFID") %>%
  dplyr::relocate(FarmName, RFID, Parity, DIM, .before = week)

# Get a description of the records per animal and week
table(weekly_data$RFID, weekly_data$week)
tapply(weekly_data$nRecords, list(weekly_data$RFID, weekly_data$week), sum)

# Count number of animals keep in data
length(unique(weekly_data$FarmName))

# Description of animals and records per treatment
weekly_data %>%
  dplyr::group_by(RFID) %>%
  dplyr::summarise(TRT = first(TRT),
                   sum_nRecords = sum(nRecords),
                   .groups = 'drop') %>%
  dplyr::group_by(TRT) %>%
  dplyr::summarise(n = n_distinct(RFID),
                   total_nRecords = sum(sum_nRecords),
                   .groups = 'drop')


# Fit a linear mixed-effects model
model <- lme4::lmer(CH4GramsPerDay ~ TRT + Parity + DIM + (1 | RFID), data = weekly_data)

# Summarize the model
summary(model)

# Check for differences between groups (TRT)
emmeans_results <- emmeans::emmeans(model, pairwise ~ TRT)
emmeans_results

# Perform ANOVA
anova_result <- stats::aov(CH4GramsPerDay ~ TRT, data = weekly_data)

# Perform Tukey's HSD test
tukey_result <- stats::TukeyHSD(anova_result)

# Extract the TukeyHSD results for plotting
tukey_data <- as.data.frame(tukey_result$TRT)
tukey_data$Comparison <- rownames(tukey_data) # Add comparison labels
rownames(tukey_data) <- NULL

# Plotting using ggplot2
ggplot(tukey_data, aes(x = Comparison, y = diff, fill = Comparison)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "black") +
  theme_minimal() +
  labs(
    title = "TukeyHSD Results for CH4GramsPerDay",
    x = "Treatment Comparison",
    y = "Mean Difference (CH4GramsPerDay)"
  ) +
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", 
                               "#3B3B3BFF", "#8F7700FF", "#D2691EFF", "#56B4E9", "#009E73")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# -------------------------------------------------------------------------------

# Step 4. Processing Intakes and Visits ----------------------------------------

rm(list = ls()) # initialization

# Set the working directory
setwd("/Users/GuillermoMartinez/")

##Download raw data and Process pellet intakes
pellin(user = Sys.getenv("GF_USER"), 
       pass = Sys.getenv("GF_PASS"), 
       unit = c(304,305), 
       gcup = c(42,43,35,36),   #Specify the gcup for each FoodType
       start_date = "2025-01-02", 
       end_date = "2025-03-06", 
       save_dir = "Downloads/",
       rfid_file = "Documents/Projects/Project_UW_GCIGreenFeed/Methane/Studies/RL06/RL06_EID.xlsx")


# Set the working directory
setwd("/Users/GuillermoMartinez/Documents/Courses/GreenFeed/Workshop 2025/Lab/")

##Process pellet intakes from file
pellin(unit = c(304,305), 
       gcup = 43, 
       start_date = "02/12/2024",        
       end_date = "06/02/2025",
       save_dir = "/Users/GuillermoMartinez/Downloads/",
       file_path = "Results/EXP1_feedtimes.csv")


rm(list = ls()) # initialization

# Set the working directory
setwd("/Users/GuillermoMartinez/Documents/Projects/Project_UW_GCIGreenFeed/Methane/Studies/HCM718/")

# Checking animal visitation to the GreenFeed
data1 <- viseat(user = Sys.getenv("GF_USER"), 
               pass = Sys.getenv("GF_PASS"),
               unit = c(592,593),
               start_date = "2025-02-21",
               end_date = "2025-03-14",
               rfid_file = "HCM718_EID.xlsx"
              )


# -------------------------------------------------------------------------------

