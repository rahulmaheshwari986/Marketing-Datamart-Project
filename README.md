# Marketing-Datamart-Project

This is an academic project as part the course 'Business Analytics Tools - Open Source(R)' under the MSc. program in Big Data Analytics for Business at IÉSEG School of Management. 

The aim is to create a marketing datamart using multiple raw datasets from bwin on demographics and product-specific transactions for 40k+ users. This involved combining and aggregating the gambling activity, and designing and calculating marketing metrics for each user. A pdf report manual with insights and visualizations on the gambling behavior, and details on the metrics has been created programmatically. 

Tools used - R v3.2.3 (packages - data.table, dplyr, readxl, ggplot2, rmarkdown, knitr) and RStudio

Main R script files - Datamart.R, Datamart_Report.Rmd

Output files - Datamart.csv, Datamart_Report.pdf

Input Data files (from bwin.com) - AnalyticDataInternetGambling.csv, RawDataIDemographics.csv, RawDataIIIPokerChipConversions.csv, RawDataIIUserDailyAggregation.csv, Codes.xlsx

The RMD script takes Codes.xlsx and Datamart_temp.csv as input, which are generated after running the Datamart.R