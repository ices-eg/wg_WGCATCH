# Selection methods by hierarchy and year 

rm(list=ls())


# library -----------------------------------------------------------------

library(tidyverse)
# library(RDBEScore)

# library(remotes)
# remove.packages(RDBEScore)
# install_github("ices-tools-dev/RDBEScore@dev")




# Load all data -----------------------------------------------------------
hier <- paste0("H", seq(1, 13, by = 1))
inputH <- paste0("./CS_data/", hier, "/")


# READ all files in the folders 
files <- list()


for(i in 1:length(unique(inputH))){
  log_temp <- list.files(path = inputH[[i]], pattern=".csv", full.names = TRUE) 
  files[[i]] <- lapply(log_temp, function(x) {read.csv(x, stringsAsFactors = FALSE, check.names = FALSE)})
  
}

pattern <- "selectionMethod"
exclusion_pattern <- "Cluster" 

unique_values <- lapply(seq_along(files), function(list_index) {
  file_list <- files[[list_index]]
  
  lapply(file_list, function(df) {
    # Find columns that match the pattern but do not contain the exclusion pattern
    cols <- grep(pattern, names(df), value = TRUE)
    cols <- cols[!grepl(exclusion_pattern, cols)]  # Exclude columns that match the exclusion pattern
    
    # Check if there are any matching columns after exclusion
    if (length(cols) > 0) {
      # Convert each column to character to avoid data type mismatches
      df %>%
        select(all_of(cols)) %>%
        mutate(across(everything(), as.character)) %>%
        pivot_longer(cols = everything(), names_to = "column", values_to = "unique_value") %>%
        count(column, unique_value) %>% # TODO  Check - Ok matches the number of rows 
        mutate(hierarchy = list_index)  # Add list index as a separate column
    } else {
      NULL
    }
  })
})

# Filter out NULL values to remove data frames with no matching columns
unique_values <- unique_values[!sapply(unique_values, is.null)]

# Combine all counts into a single data frame
final_df <- bind_rows(unique_values, .id = "file_index") %>%
  select(hierarchy, column, unique_value, n) %>%
  arrange(hierarchy, column, unique_value) %>%
  rename(unique_value_count = n)

final_df <- mutate(final_df, SelectionMethod = ifelse(unique_value %in% c("CENSUS", "UPSWOR", "UPSWR",  "SRSWOR", "SYSS", "SRSWR"), "P", "NP"))

# Get out number of sampling schemes by hierarchy 


nsamschemes <- lapply(seq_along(files), function(hierarchy) {
  file_list <- files[[hierarchy]][[2]] # the second table is design? yes but not robust as it is by position 
  file_list %>%
    select(DEsamplingScheme)    %>%
    mutate(across(everything(), as.character)) 
})

nsamschemes1 <- bind_rows(nsamschemes, .id = "file_index")





ff <- final_df %>%
  group_by( column, hierarchy) %>%
  mutate(tot = sum(unique_value_count)) %>%
  group_by(SelectionMethod, column, hierarchy) %>%
  summarise(totS = sum(unique_value_count),
            fin = round((totS/tot)*100, digits  = 0))

ff1 <- filter(ff, !column %in% "BVselectionMethod") %>%
  distinct()
ggplot(ff1, aes(column, fin, fill = SelectionMethod)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~hierarchy) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(ff1, aes(column, fin, fill = SelectionMethod)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~hierarchy, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ff2 <- filter(final_df, !column %in% "BVselectionMethod")
ggplot(ff2, aes(column, unique_value_count, fill = SelectionMethod)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~hierarchy, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


