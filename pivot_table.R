# code to combine GSI data from 2015-2019 and 2020-2023 to create a pivot table

library(tidyverse)
library(readxl)
library(writexl)

data_2019 <- read_excel("Biological_Data_With_Results_2015-2019.xlsx", sheet = "Biological_Data_With")
data_2023 <- read_excel("Biological_Data_With_Results_2020-2023.xlsx", sheet = "Biological_Data_With")

gsi_2015_2023 <- bind_rows(data_2019, data_2023)

# export the combined data to Excel
write_xlsx(gsi_2015_2023, "gsi_2015_2023.xlsx")

# Go to Excel, create a pivot table