
#################################################################################
# OFB_Pre-processing.R                                                         ##
# Extracts original neighborhood survery into factor, boolean, and text fields ##
#################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

#rm(OFB_wide_original)

OFB_wide_original <- read.csv("OFB_2023/OFB_neighbour_survey_v1.0/neighbour_survey_clean-2024-06-14.csv", stringsAsFactors = FALSE)
#OFB_wide_original_m <- read.csv("m_neighbour_survey_clean-2024-07-28.csv", stringsAsFactors = FALSE)


#names(OFB_wide_original_m)

#create index

#OFB_wide <- tibble::rowid_to_column(OFB_wide_original_m, "ID") 

#colnames(OFB_wide)[colnames(OFB_wide) == 'ID'] <- 'id'

#glimpse(OFB_wide)
## Change id to numeric index

OFB_wide <- OFB_wide_original |> 
  mutate(id = as.integer(substring(id,3,8)))

### Checking for duplicates

paste("No Duplicates: ", !anyDuplicated(OFB_wide))
paste("rows", nrow(OFB_wide))
paste("columns", ncol(OFB_wide))

print("Duplicate Ids:")
print(OFB_wide$id[duplicated(OFB_wide$id)])

### Delete Duplicates

# Deletes NA values in key id field

glimpse(OFB_wide)

OFB_wide <- OFB_wide |> 
  filter(!is.na(id)) 

OFB_wide <- OFB_wide[!duplicated(OFB_wide$id), ]

print("Duplicates Deleted")

glimpse(OFB_wide)

### Verifying duplicates deleted

paste("No Duplicates: ", !anyDuplicated(OFB_wide$id))
paste("rows", nrow(OFB_wide))
paste("columns", ncol(OFB_wide))

### Checking for NAs in id field

paste(sum(is.na(OFB_wide[, c("id")])),"NAs in id field")

### Reshape from wide to long 

glimpse(OFB_wide)

OFB_long <- OFB_wide %>%
  pivot_longer(
    cols = starts_with("q0"),
    names_to = "Question",              
    values_to = "Response"                
  )



OFB_long |> arrange(Question)
#print(OFB_long)

### Fix inconsistencies in responses

library(stringr)
OFB_long_fixed <- OFB_long |> 
  mutate(Response = ifelse(str_detect(Response, "^[:upper:]+$"), Response,str_to_title(Response))) 


### Based on the format of the question number, we are creating factor, boolean, 
### and text field datasets. The factor and boolean datasets are merged and the 
### text fields are left out for privacy and processing speed reasons for now. 

### Extracting and converting into factor, boolean, and text fields


## Identify text fields from table and create vector

text_fields = c("q011b","q012"
                ,"q014h"
                ,"q016f"
                ,"q017k"
                ,"q018"
                ,"q021"
                ,"q023f"
                ,"q024i"
                ,"q029"
                ,"q030"
                ,"q031"
                ,"q032"
                ,"q035b"
                ,"q036"
                ,"q038k"
                ,"q039i"
                ,"q040b"
                ,"q042"
)

# Normally all the factor fields have 4 char length question numbers, however there
#are exceptions

# Creates vector of fields that are factors (excemptions) rather than boolean fields

factor_fields <- c("q011a"
                   ,"q035a"
                   ,"q036a"
                   ,"q040a"
) #the exceptions


# Creating OFB_long_text dataset

OFB_long_text <- OFB_long_fixed |>
  filter(Question %in% text_fields
  )

# Creates table with only Boolean values minus text and factor fields

OFB_long_boolean <- OFB_long_fixed |>
  filter(str_count(trimws(Question)) > 4 ) |>
  filter(!(Question %in% text_fields)) |>
  filter(!(Question %in% factor_fields)) 

# Extracts all boolean responses and converts them to boolean values

OFB_long_boolean <- OFB_long_boolean |>
  mutate(Response=ifelse(!is.na(Response), 1, NA ))

OFB_long_boolean <- OFB_long_boolean |> mutate(Response = as.integer(Response))

# Extracts all factor fields and factors the fields

OFB_long_factor <- OFB_long_fixed |> 
  filter(str_count(trimws(Question)) <= 4 | Question %in% factor_fields ) |>
  filter(!(Question %in% text_fields))


# OFB_long_factor <- OFB_long_factor |> 
  # mutate(Response = as.factor(Response))

# Extracts all comment(text) fields

OFB_long_text <- OFB_long_fixed |>
  filter(Question %in% text_fields)

OFB_wide_boolean <- OFB_long_boolean %>%
  pivot_wider(names_from = Question, values_from = Response)

OFB_wide_factor <-  OFB_long_factor %>%
  pivot_wider(names_from = Question, values_from = Response)

### Merging OFB_wide_factor and OFB_wide_boolean 

# Convert all OFB_longs to wide then cbind them together

## Convert Factor fields to wide data frame format


OFB_wide_factor <- OFB_long_factor %>%
  pivot_wider(names_from = Question, values_from = Response)

### Convert Boolean to wide

# Convert Boolean NA's to 0's

OFB_long_boolean <- OFB_long_boolean |>
  mutate(Response=ifelse(is.na(Response), 0, Response))

OFB_wide_boolean <- OFB_long_boolean %>%
  pivot_wider(names_from = Question, values_from = Response)


OFB_wide <- merge(OFB_wide_factor, OFB_wide_boolean)

paste("OFB Pre-processing Completed")

