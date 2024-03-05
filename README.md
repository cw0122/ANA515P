ANA 515P Data Preparation Project
Author: Tiger Wang
Dataset Used: Presidential Polling from 2020 (source FiveThirtyEight)

Necessary R packages that must be installed to run the code include "tidyverse", "readxl", and "janitor".

This project is a data preparation project where two datasets including information from FiveThrityEight on the 2020 presidential polls are cleaned. The datasets are combined into one final dataset in preparation for further analysis.

We begin cleaning by merging the datasets and deleting rows where there are more than two missing values. This guarantees the quality of the data in further analysis and sets a threshold.

Next, we check for and delete duplicate rows.

Lastly, we perform data cleaning on a column by column basis - the code is written in chunks where each chunk pertains to the cleaning of a particular column or variable. 
We look for missing values, anomaly values using frequency distributions, and inspect the data type. In some cases we decide to use a mean, median, or mode value to replace missing values and anomalies.
In other cases, we determine the missing value is unlikely to influence our data analysis and leave the values as is.
In one particular case, we decide to give missing values a "False" value based on our understanding that the column is a binary True or False variable.
And in another particular case, we decide to delete a column after inspecting that the majority of values in that column is missing, and could not be reasonable replaced otherwise.
