# Title: Austin Cultural Centers Analysis
# Author: Alexander Zakrzeski
# Date: January 20, 2025

# Part 1: Setup and Configuration

# Load to import, clean, and wrangle data
import pandas as pd

# Part 2: Data Preprocessing

# Load the data from the CSV file, rename columns, and drop certain columns
df_qual = pd.read_csv("Austin-Cultural-Centers-Qualitative-Data.csv")
df_qual = df_qual.rename(columns = {"Facility": "facility", 
                                    "Survey Item": "prompt", 
                                    "Response": "response", 
                                    "Auditor-assigned Category": "label", 
                                    "Re-assigned response?": "reassigned",
                                    "Translated?": "translated",
                                    "Original Language": "language"})
df_qual = df_qual.drop(columns = ["reassigned", "translated", "language"])

# Create new columns, modify values of existing columns, and sort the rows
df_qual["alias"] = df_qual["facility"].replace( 
  {"African American Cultural and Heritage Facility": "AACHF", 
   "Asian American Resource Center": "AARC",  
   "George Washington Carver Museum": "Carver Museum", 
   "Emma S. Barrientos Mexican American Cultural Center": "ESB-MACC"} 
  ) 
df_qual["prompt"] = df_qual["prompt"].replace( 
  {r".*facilities.*": "1-Facilities",  
   r".*staff.*": "2-Staff", 
   r".*fees.*": "3-Fees",
   r".*programs.*": "4-Programs"}, 
  regex = True  
  )
df_qual["response"] = df_qual["response"].str.replace(r"\s{2,}", " ", 
                                                      regex = True)  
df_qual = df_qual.sort_values(by = ["prompt", "facility", "response"])
df_qual["id"] = range(1, len(df_qual) + 1)

# Filter, reset the index, drop a column, and modify the values in a column
df_qual = df_qual[~((df_qual["label"].isna()) | 
                    (df_qual["label"] == "Suggestion") |  
                    (df_qual["id"].isin([30, 41, 68, 76, 89, 91, 98, 102, 131,
                                         152, 159, 171, 183, 197, 210, 224, 
                                         237, 257, 272, 275, 276, 292, 345, 
                                         380, 384, 391, 409, 434, 435, 448, 
                                         455, 484, 488, 490, 493, 516, 536, 
                                         598, 615, 644, 648, 678, 705, 715, 
                                         732, 739, 795, 832, 839, 848])))] 
df_qual = df_qual.reset_index(drop = True) 
df_qual = df_qual.drop(columns = "id")
df_qual["prompt"] = df_qual["prompt"].str[2:] 

# Change the order of the columns
df_qual.insert(0, "prompt", df_qual.pop("prompt"))
df_qual.insert(2, "alias", df_qual.pop("alias"))

# Part 3: Sentiment Analysis

# Part 4: Data Visualization                                             
