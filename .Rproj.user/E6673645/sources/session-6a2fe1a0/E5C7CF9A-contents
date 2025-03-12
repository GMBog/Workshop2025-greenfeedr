
################################################################################
#                                                                              #
#                        GreenFeed Workshop 2025                               #
#                              Laboratory                                      #
#                                                                              #
#       greenfeedr: An R Package for Processing & Reporting GreenFeed Data     #
#                                                                              #
################################################################################

# Installation

##Install from CRAN
install.packages("greenfeedr")

##Install from GitHub
#install.packages("remotes")
remotes::install_github("GMBog/greenfeedr", force = TRUE)

# Load libraries
library(greenfeedr)
library(dplyr)
library(emmeans)
library(ggplot2)
library(lme4)
library(purrr)
library(readr)
library(readxl)

# Set directory
setwd("/Users/GuillermoMartinez/Documents/Courses/GreenFeed/Workshop 2025/Lab/")


## Step 1. Downloading data #####################################################

# Download Raw Data (e.g., feedtimes, rfids, commands)
get_gfdata(user = Sys.getenv("GF_USER"), 
           pass = Sys.getenv("GF_PASS"), 
           d = "feed", #"rfid", #"cmds"
           exp = "EXP1", 
           unit = c(832,833), #"212"
           start_date = "2025-02-02", 
           end_date = "2025-02-06", 
           save_dir = "Results/")

# Download Preliminary Data
get_gfdata(user = Sys.getenv("GF_USER"), 
           pass = Sys.getenv("GF_PASS"), 
           #d = "visits",  
           exp = "EXP1", 
           unit = c(592,593),
           start_date = "2025-02-25", 
           end_date = "2025-03-05", 
           save_dir = "Results/")


#################################################################################

## Step 2. Reporting data #######################################################



#################################################################################

## Step 3. Processing data ######################################################

# Read 'finalized' GreenFeed data
Summarized_Data <- readxl::read_excel("GreenFeed_Summarized_Data.xlsx")

# Change RFID of cows that were changed or lost during the study
Summarized_Data$RFID <- gsub("^0+", "", Summarized_Data$RFID)
Summarized_Data$RFID[Summarized_Data$RFID == "840003211748001"] <- "840003268256430"
Summarized_Data$RFID[Summarized_Data$RFID == "840003211747974"] <- "840003268256431"
Summarized_Data$RFID[Summarized_Data$RFID == "840003148277091"] <- "840003268256437"
Summarized_Data$RFID[Summarized_Data$RFID == "840003148277127"] <- "840003268256436"
Summarized_Data$RFID[Summarized_Data$RFID == "840003205049445"] <- "840003268256439"



eval <- eval_gfparam(data = data1,
                     start_date = "2025-01-17",
                     end_date = "2025-03-07")


# Description of the results

## How much affect the decision on the number of records per day (param1)?
scale_factor <- max(data$mean_dCH4) / max(data$CV_dCH4)
ggplot(data, aes(x = param1)) +
  # Primary y-axis: daily mean
  geom_point(aes(y = mean_dCH4, size = records_d, color = "Daily Mean")) +
  # Secondary y-axis: daily CV (scaled)
  geom_point(aes(y = CV_dCH4 * scale_factor, size = records_d, color = "Daily CV")) +
  # Labels and axes
  labs(
    title = "",
    x = "Parameter 1",
    y = "Daily Mean CH4",
    size = "Records",
    color = "Metric"
  ) +
  scale_y_continuous(
    name = "Daily Mean CH4",
    sec.axis = sec_axis(~ . / scale_factor, name = "Daily CV CH4")
  ) +
  # Customize theme
  theme_minimal() +
  theme(
    strip.text = element_text(colour = "white", size = 10),
    strip.background = element_rect(fill = "grey20", color = "grey20", linewidth = 1),
    panel.background = element_rect(fill = "white", colour = "grey20"),
    legend.position = "bottom",  # Ensures size legend remains visible
    axis.title.y = element_text(size = 12, vjust = 1.5, face = "bold", family = "Times New Roman"),
    axis.title.y.right = element_text(size = 12, vjust = 1.5, face = "bold", family = "Times New Roman"),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 7, family = "Times New Roman"),
    axis.text.y.right = element_text(angle = 0, hjust = 0.5, size = 7, family = "Times New Roman"),
    axis.title.x = element_text(size = 12, vjust = -0.5, face = "bold", family = "Times New Roman"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7, family = "Times New Roman")
  )

## How much affect the decision on the number of records per day (param1) and days with records (param2)?
ggplot(data, aes(x = param1, y = CV_wCH4, group = param2, color = as.factor(param2))) +
  # Add line and point layers
  geom_line() +
  geom_point(aes(size = records_d)) +  # Size indicates the number of records
  # Facet the plot by 'min_time' and 'param2'
  facet_grid(min_time ~ param2) +
  # Add labels (excluding param2 legend)
  labs(
    title = "",
    x = "Parameter 1",
    y = "CV Weekly CH4",
    size = "Records"
  ) +
  # Remove the color legend for param2
  scale_color_discrete(guide = "none") +
  # Apply minimal theme and customize
  theme_minimal() +
  theme(
    strip.text = element_text(colour = "white", size = 10),
    strip.background = element_rect(fill = "grey20", color = "grey20", linewidth = 1),
    panel.background = element_rect(fill = "white", colour = "grey20"),
    panel.grid.major = element_line(color = "grey80", size = 0.25),  # Enable major gridlines
    panel.grid.minor = element_line(color = "white", size = 0),  # Enable minor gridlines
    legend.position = "bottom",  # Ensures size legend remains visible
    axis.title.y = element_text(size = 12, vjust = 1.5, face = "bold", family = "Times New Roman"),
    axis.text.y = element_text(angle = 0, hjust = 0.5, size = 7, family = "Times New Roman"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 7, family = "Times New Roman")
  )



# Now, we can run the process_gfdata with the parameter we decided to use
Summarized_Data <- greenfeedr::process_gfdata(
  data = Summarized_Data,
  start_date = "2023-07-13",
  end_date = "2023-10-20",
  param1 = 2,
  param2 = 3,
  min_time = 2
)

daily_data <- Summarized_Data$daily_data
weekly_data <- Summarized_Data$weekly_data

# Join weekly averages with animal information
weekly_data <- weekly_data %>%
  dplyr::inner_join(list_cows, by = "RFID") %>%
  dplyr::relocate(FarmName, RFID, Parity, DIM, TRT, CAN, .before = week)

# Description of records across weeks
table(weekly_data$FarmName, weekly_data$week)
tapply(weekly_data$nRecords, list(weekly_data$FarmName, weekly_data$week), sum)

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
model <- lmer(CH4GramsPerDay ~ TRT + Parity + DIM + (1 | RFID), data = weekly_data)

# Summarize the model
summary(model)

# Check for differences between groups (TRT)
emmeans_results <- emmeans::emmeans(model, pairwise ~ TRT)
emmeans_results

# Perform ANOVA
anova_result <- aov(CH4GramsPerDay ~ TRT, data = weekly_data)

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(anova_result)

# Extract the TukeyHSD results for plotting
tukey_data <- as.data.frame(tukey_result$TRT)
tukey_data$Comparison <- rownames(tukey_data) # Add comparison labels
rownames(tukey_data) <- NULL

# Plotting using ggplot2
ggplot(tukey_data, aes(x = Comparison, y = diff)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  theme_minimal() +
  labs(
    title = "TukeyHSD Results for CH4GramsPerDay",
    x = "Treatment Comparison",
    y = "Mean Difference (CH4GramsPerDay)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# FILE FOR COMBINING GREENFEED AND SMARTFEED DATA
# M. Harrison (9/21/24)


# BROUWER EQUATION
#HP (kJ) = (16.18 x O2 consumption in L) + (5.02 * CO2 Prod in L) - (2.17 x CH4 prod in L)


### FILE SET-UP

# LOAD PACKAGES
library(openxlsx)
library(tidyr)
library(dplyr)
library(readxl)
library(data.table)
library(tidyverse)



# CONVERSION FACTORS
cup <- 35                     # cup mass in grams from GreenFeed user
kg <- 2.2                     # conversion for pound to kilo

ch4.molc.wt <-  16.04         # gas molecular weight for conversion to liters
co2.molc.wt <- 44.01
o2.molc.wt <- 32.00
g_mol <- 22.4                 # conversion factor for g/mol


# USER DEFINED PARAMETERS
fac <- 1.5                       # IQR multiplying factor used "1.5 * IQR"
period <- 19                    # number of visits needed for inclusion in analysis



### READ EMISSIONS DATA
##
df <- read_excel("/Users/GuillermoMartinez/Documents/Projects/Project_UW_GCIGreenFeed/Methane/Studies/FP707/GreenFeed_Summarized_Data_592_593_2024_09_01_to_2024_10_25.xlsx",
                 sheet = 1)    # use read xl for dates
head(df); tail(df)                                                                                # view first and last 6 rows

# prepare data
colnames(df)=c('RFID','AnimalName','FeederID','StartTime','EndTime','GoodDataDuration','Hour',    # rename columns
               'CO2GramsPerDay','CH4GramsPerDay','O2GramsPerDay','H2GramsPerDay','H2SGramsPerDay',
               'AirflowLitersPerSec','AirflowCf','WindSpeedMetersPerSec','WindDirDeg','WindCf',
               'WasInterrupted','InterruptingTags','MidSinceLast', 'MidUntilNext',
               'StandDevCH4Base', 'PipeTempDegC','GasTempDegC','RID')
head(df)                                                                                          # quick check

df$Date= as.Date(df$StartTime)                                                                    # create new column date
df$Duration= format(as.POSIXct(df$GoodDataDuration), format = "%H:%M:%S"); str(df)                # create new column duration

# define animals as factor based on RFID
(df$EID= substr(df$RFID, nchar(df$RFID) - 4 + 1, nchar(df$RFID)))                             # define EID as last 4 digits of RFID
df$EID=as.factor(df$EID); unique(df$EID)                                                      # create EID as a factor and see how many unique ids
dim(df)                                                                                       # total number of gas observations for all animals; (r X c)
df$FeederID=as.factor(df$FeederID); unique(df$FeederID)                                       # create FID as a factor and see how many unique ids
str(df)                                                                                       # data structure



### ASSESS OUTLIERS AND SUMMARIZE DATA
# Replace value -999 (denotes sensor error) with NA -- Note all greenfeeds don't have o2 and h2
df <- df %>%
    mutate(CO2GramsPerDay  = ifelse(CO2GramsPerDay  == -999, NA, CO2GramsPerDay),
           CH4GramsPerDay  = ifelse(CH4GramsPerDay  == -999, NA, CH4GramsPerDay),
           O2GramsPerDay  = ifelse(O2GramsPerDay  == -999, NA, O2GramsPerDay),
           H2GramsPerDay  = ifelse(H2GramsPerDay  == -999, NA, H2GramsPerDay))


# summarize the data set
unique(df$EID)                                                                          # number of unique animals with methane measurement
length(df$EID)                                                                          # total number of gas observations for all animals
dim(df)
summary(df$CH4GramsPerDay); summary(df$CO2GramsPerDay); summary(df$O2GramsPerDay)      # 5 number summary for CH4, CO2, and O2 visits


# visualize data using simple boxplot and set quantiles
boxplot(df$CH4GramsPerDay, main = "Boxplot CH4", ylab = "(g/d)")$out                   # "$out" outliers from the boxplot listed in output
(Q.ch4 <- quantile(df$CH4GramsPerDay, probs=c(.25, .75), na.rm = TRUE))                # set quantiles for outliers

boxplot(df$CO2GramsPerDay, main = "Boxplot CO2", ylab = "(g/d)")$out
(Q.co2 <- quantile(df$CO2GramsPerDay, probs=c(.25, .75), na.rm = TRUE))

boxplot(df$O2GramsPerDay, main = "Boxplot O2", ylab = "(g/d)")$out
(Q.o2 <- quantile(df$O2GramsPerDay, probs=c(.25, .75), na.rm = TRUE))

boxplot(df$H2GramsPerDay, main = "Boxplot H2", ylab = "(g/d)")$out
(Q.h2 <- quantile(df$O2GramsPerDay, probs=c(.25, .75), na.rm = TRUE))

# Histograms
hist(df$CH4GramsPerDay, main = "Histogram CH4", xlab = "CH4 (g/d)")
hist(df$CO2GramsPerDay, main = "Histogram CO2", xlab = "CO2 (g/d)")                                                                                           # Ch4 histogram
hist(df$O2GramsPerDay, main = "Histogram O2", xlab = "O2 (g/d)")
hist(df$O2GramsPerDay, main = "Histogram O2", xlab = "H2 (g/d)")

#


### BEGIN OUTLIER ASSESSING
# calculate IQR
(IQR.ch4 <- IQR(df$CH4GramsPerDay, na.rm = TRUE))
(IQR.co2 <- IQR(df$CO2GramsPerDay, na.rm = TRUE))
(IQR.o2 <- IQR(df$O2GramsPerDay, na.rm = TRUE))


# find outliers using IQR and configured fac and subset new data--2 OPTIONS

# OPTION 1- delete entire visit if anything is outlier
no_out <- subset(df, df$CO2GramsPerDay > (Q.co2[1] - fac*IQR.co2) &  df$CO2GramsPerDay < (Q.co2[2] + fac*IQR.co2))
no_out <- subset(no_out, no_out$CH4GramsPerDay > (Q.ch4[1] - fac*IQR.ch4) &  no_out$CH4GramsPerDay < (Q.ch4[2] + fac*IQR.ch4))
no_out <- subset(no_out, no_out$O2GramsPerDay > (Q.o2[1] - fac*IQR.o2) &  no_out$O2GramsPerDay < (Q.o2[2] + fac*IQR.o2))

dim(no_out)                             # deminsions of new reduced sized data set no out
hist(no_out$CH4GramsPerDay)
boxplot(no_out$CH4GramsPerDay)$out


# OPTION 2- outlier converted to NA to keep other gasses that were not an outlier
no_out2 <- df %>%
  mutate(CO2GramsPerDay  = ifelse(CO2GramsPerDay > (Q.co2[1] - fac*IQR.co2) & CO2GramsPerDay < (Q.co2[2] + fac*IQR.co2),  CO2GramsPerDay, NA),
        CH4GramsPerDay  = ifelse(CH4GramsPerDay  > (Q.ch4[1] - fac*IQR.ch4) & CH4GramsPerDay < (Q.ch4[2] + fac*IQR.ch4), CH4GramsPerDay, NA),
        O2GramsPerDay  = ifelse(O2GramsPerDay  > (Q.o2[1] - fac*IQR.o2) &  O2GramsPerDay < (Q.o2[2] + fac*IQR.o2), O2GramsPerDay, NA))

dim(no_out2)
hist(no_out2$CH4GramsPerDay)
boxplot(no_out2$CH4GramsPerDay)$out

# chose an option I am using option 2 to retain as much data as possible


## Visualize and summarize
# methane
ggplot(df,aes(x=Date,y=CH4GramsPerDay))+
  geom_point(aes(color = FeederID)) +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(~AnimalName)+
  ggtitle('CH4 Observations by GreenFeed Unit')+
  ylab('CH4 g/d')+  ylim(0,700)+
  theme_light()+
  theme(axis.text.x = element_text(angle = -45))

#carbon dioxide
ggplot(df,aes(x=Date,y=CO2GramsPerDay))+
  geom_point(aes(color = FeederID)) +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(~AnimalName)+
  ggtitle('CO2 Observations by GreenFeed Unit')+
  ylab('CO2 g/d')+  ylim(6000,22000)+
  theme_light()+
  theme(axis.text.x = element_text(angle = -45))



# AVERAGE EMISSIONS DATA (with no outliers)

# creating new data frame with period averages for gas event data
em_avg <- no_out2 %>% group_by(AnimalName) %>%
  summarise(Count = n(),
            unique_days = n_distinct(Date),
            Mean.CH4 = mean(CH4GramsPerDay, na.rm = TRUE),
            sd.CH4 = sd(CH4GramsPerDay, na.rm = TRUE),
            Mean.CO2 = mean(CO2GramsPerDay, na.rm = TRUE),
            sd.CO2 = sd(CO2GramsPerDay, na.rm = TRUE),
            Mean.O2 = mean(O2GramsPerDay, na.rm = TRUE),
            sd.O2 = sd(O2GramsPerDay, na.rm = TRUE),
  )

head(em_avg); dim(em_avg)
boxplot(em_avg$Mean.CH4)$out

# generate file all animals and days
write.xlsx(em_avg, 'Ross All Bulls.xlsx')




# quick stats on visitation
mean(em_avg$Count); sd(em_avg$Count)        # average visitation per animal
summary(em_avg$Count)                       # average visitation all animals
boxplot(em_avg$Count)$out


# GENERATE FINAL DATA
# filter by number of total visits, mist have >20
em_pheno <- subset(em_avg, em_avg$Count > period)        # subset by number of vis; FINAL DATA SET
head(em_pheno) ; unique(em_pheno$AnimalName)             # number of head that used it versus used it enough

hist(em_pheno$Mean.CH4, main = "Histogram of Average CH4 by Animal", xlab= "g/d")
boxplot(em_pheno$Mean.CH4, main = "CH4 Average by Animal")$out

hist(em_pheno$Mean.CO2, main = "Histogram of Average CO2 by Animal", xlab= "g/d")
boxplot(em_pheno$Mean.CO2, main = "CO2 Average by Animal")$out

hist(em_pheno$Mean.O2, main = "Histogram of Average O2 by Animal", xlab= "g/d")
boxplot(em_pheno$Mean.O2, main = "O2 Average by Animal")$out


# calculate as L per day for RQ and HP
(em_pheno$CH4_L = (em_pheno$Mean.CH4 * g_mol)/ ch4.molc.wt)
(em_pheno$CO2_L = (em_pheno$Mean.CO2 * g_mol)/ co2.molc.wt)
(em_pheno$O2_L = (em_pheno$Mean.O2 * g_mol)/ o2.molc.wt)
(em_pheno$RQ = em_pheno$CO2_L/em_pheno$O2_L)
em_pheno$HP = (16.18*em_pheno$O2_L) + (5.02 *em_pheno$CO2_L) - (2.17*em_pheno$CH4_L)






