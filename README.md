# greenfeedr R-Package Training Session

This repository provides training on handling and analyzing GreenFeed data using the **greenfeedr** R-package. The course is designed for individuals with prior experience with the GreenFeed system who want to master the usage of the **greenfeedr** package and gain hands-on training in processing GreenFeed data.

## Course Information

### Introduction to GreenFeedR
**Dates**: 18-21 March, 2025  
**Lecture**: 10 AM – 11:30 AM, Tuesday  
**Lab**: 10 AM – 11:30 AM, Friday

**Instructor**:  
Guillermo Martinez-Boggio  
[Email: guillermo.martinezboggio@wisc.edu](mailto:guillermo.martinezboggio@wisc.edu)

### Required Software
- R
- RStudio

### Course Objectives:
- Master the usage of the greenfeedr package.
- Gain hands-on training for efficient GreenFeed data handling.

### Expectations of Students:
- Install R and RStudio before class time.
- Students should have prior experience with the GreenFeed system.

### Modes of Instruction:
- Virtual classes.

## Program Overview

This program focuses on how to effectively use the **greenfeedr** package to handle and analyze GreenFeed data. It covers functions to download, report, and process daily and summarized GreenFeed data. Note that this program does **not** cover GreenFeed theory, operating principles, mechanical design, or maintenance.

### 1. Downloading Data

The **greenfeedr** package has two key functions: `get_gfdata` and `report_gfdata`, which allow users to download their processed gas records daily.

**GreenFeed Data Overview**:
- **Raw Data**: Processed daily by the C-Lock processor with a one-day delay.
- **Summarized Data**: The same data as raw, but with an additional manual check by C-Lock.

**Important Terms**:
- **Visits**: When an animal enters the unit, receives food drops, and exits.
- **Records**: A visit is translated into a record in the data based on certain criteria, such as visits longer than 2 minutes.

### 2. Reporting Data

The **report_gfdata** function helps generate a PDF report for daily or summarized GreenFeed data. This report includes key metrics and plots, helping users understand their trial's progress and performance. It is essential for tracking anomalies and ensuring proper functioning of the GreenFeed system.

### 3. Processing Data

The **process_gfdata** function is used to process both preliminary and finalized GreenFeed data. This function handles:
- **Removing** records with low airflow, 'Unknown ID', or outliers (defined as values outside ±3 standard deviations).
- **Filtering** based on:
  - **Param1**: Number of records per day.
  - **Param2**: Number of days with records in a week.
  - **Min_time**: Minimum visit duration for an animal.

The best combination of these parameters depends on the trial's characteristics and should be adjusted accordingly.

