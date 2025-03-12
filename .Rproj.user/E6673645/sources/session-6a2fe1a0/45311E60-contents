
# Installation

##Install from CRAN
install.packages("greenfeedr")

##Install from GitHub
#install.packages("remotes")
remotes::install_github("GMBog/greenfeedr", force = TRUE)

# Load libraries
library(greenfeedr)
library(dplyr)

# Initialization
rm(list = ls())

# Set directory
setwd("/Users/GuillermoMartinez/Documents/Projects/Project_UW_GCIGreenFeed/Methane/Studies/FP712/")

# Processing GreenFeed data using process_gfdata()
file_path <- "FP712_EID.xlsx"
EID <- readxl::read_excel(file_path, col_types = c("text", "text", "numeric", "text"))

#1. Read GreenFeed Data (Preliminary or Finalized)
file_path <- "GreenFeed_Summarized_Data_716_2024_09_16_to_2024_11_08.xlsx"
Summarized_Data <- readxl::read_excel(file_path)

Summarized_Data$RFID <- gsub("^0+", "", Summarized_Data$RFID) # Remove leading zeros from RFID column

Summarized_Data$RFID[Summarized_Data$RFID == "840003234513845"] <- "840003287325030"

#2. Evaluate all possible combination of parameters using eval_gfparam()
Start_time <- Sys.time()  # Start time
eval <- eval_gfparam(data = Summarized_Data,
                     start_date = "2024-09-16",
                     end_date = "2024-11-08")
round(Sys.time() - Start_time, 2)  # Running time

##Find the "best" combination of parameters
eval <- eval %>%
  dplyr::mutate(
    cow_retention = round(N / max(N, na.rm = TRUE), 2),
    optimal = case_when(
      CV_wCH4 <= median(CV, na.rm = TRUE) & cow_retention >= 0.80 ~ "Optimal",
      TRUE ~ "Not Optimal"
    )
  )


#3. Process GreenFeed data using process_gfdata()
Start_time <- Sys.time()  # Start time
Summarized_Data <- process_gfdata(
  data = Summarized_Data,
  start_date = "2024-09-16",
  end_date = "2024-11-08",
  param1 = 1,
  param2 = 1,
  min_time = 2
)
round(Sys.time() - Start_time, 2)  # Running time

filtered_data <- Summarized_Data$filtered_data
daily_data <- Summarized_Data$daily_data
weekly_gases_per_cow <- Summarized_Data$weekly_data

weekly_gases_per_cow <- weekly_gases_per_cow %>%
  dplyr::inner_join(EID, by = "RFID") %>%
  dplyr::relocate(FarmName, RFID, Parity, DIM, .before = week)

table(weekly_gases_per_cow$FarmName, weekly_gases_per_cow$week)
tapply(weekly_gases_per_cow$nRecords, list(weekly_gases_per_cow$FarmName, weekly_gases_per_cow$week), sum)


# Methane production across the day

#tiff("~/Downloads/FigMEPday.tiff", units = "cm", width = 22, height = 10, res = 300)
ggplot(filtered_data[filtered_data$HourOfDay <= 23, ], aes(x = HourOfDay, y = CH4GramsPerDay, color = CH4GramsPerDay)) +
  geom_point() +
  geom_smooth(color = "red") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 0, size = 9, family = "Times New Roman"),
    axis.text.y = element_text(angle = 0, size = 9, family = "Times New Roman"),
    legend.position = "none",
    axis.title.y = element_text(size = 15, family = "Times New Roman", face = "bold"),
    axis.title.x = element_text(size = 15, family = "Times New Roman", face = "bold")
  ) +
  coord_cartesian(y = c(0, 800)) +
  scale_y_continuous(breaks = seq(0, 800, 50)) +
  labs(title = "", x = "", y = "MeP (g/d)") +
  scale_x_continuous(breaks = seq(0, 23), labels = c(
    "12 AM", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
    "12 PM", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"
  )) +
  scale_color_gradient(low = "grey70", high = "grey70")

#dev.off()



#Extra
# Set directory
setwd("/Users/GuillermoMartinez/Documents/Projects/Project_UW_GCIGreenFeed/Methane/Studies/LFF682/")

# Read EIDs
EID <- readr::read_table("LFF682_EID.csv", col_types = cols(FarmName = col_character(), RFID = col_character()))

# Change treatment (TRT) names
EID <- EID %>%
  dplyr::mutate(
    Parity = dplyr::case_when(
      Parity == 2 ~ "2",
      Parity == 3 ~ "3",
      Parity >= 4 ~ "4",
      TRUE ~ NA_character_  # Handle unexpected values
    )) %>%
  dplyr::mutate(TRT = case_when(
    TRT == "LSCON" ~ "A",
    TRT == "LSDFM" ~ "B",
    TRT == "NSCON" ~ "C",
    TRT == "NSDFM" ~ "D",
    TRUE ~ TRT # Keep the original value if it doesn't match any of the above
  ))


openxlsx::write.xlsx(EID, file = "~/Downloads/EXP1_EID.xlsx")


