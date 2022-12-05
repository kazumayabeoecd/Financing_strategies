### Financing Strategy base data

source("~/Desktop/R_projects/source_file.R")

#All donors 
#CRS_ODA <- CRS_raw %>%
#  filter(flow_code %in% ODA_flow_codes)
#write_parquet(CRS_ODA, "./CRS_ODA.parquet")
CRS_ODA <- read_parquet("~/Desktop/R_projects/R_CRS/CRS_ODA.parquet")

#Certain recipient 2010-2020
rec <- CRS_ODA %>%
  filter(year >= 2010,
         recipient_name == "Mozambique")

### Aggregate ### 

agg_gross <- rec %>%
  group_by(year) %>%
  summarise(value = sum(usd_disbursement_defl, na.rm = T)) 

oda_net <- read_parquet("~/Desktop/R_projects/R_DAC2a (net)/DAC2a.parquet")

agg_net <- oda_net %>%
  filter(year >= 2010,
         amount_type == "Constant Prices (2020 USD millions)",
         aid_type == "ODA: Total Net",
         donor_2 == "Official Donors, Total",
         recipient_2 == "Mozambique") %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = T))

### HDP ###
purpose <- read_xlsx("~/Desktop/R_projects/Core_Data/DAC-CRS-CODES.xlsx", sheet = "Purpose codes") %>%
  slice(-c(1,3), ) %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  rename(purpose_code = crs,
         purpose_name = description) %>%
  select(2,4)

hdp <- purpose %>%
  select(purpose_code) %>%
  na.omit() %>%
  mutate(hdp_code = ifelse(
    purpose_code %in% c(peace_codes),
    "Peace",
    ifelse(purpose_code %in% c(hum_codes),
           "Humanitarian", "Development"))) %>%
  mutate(purpose_code = as.numeric(purpose_code))

hdp_oda <- rec %>%
  left_join(hdp, "purpose_code") %>%
  group_by(year, hdp_code) %>%
  summarise(value = sum(usd_disbursement_defl, na.rm = T)) %>%
  group_by(year) %>%
  mutate(`proportion per year (%)` = value / sum(value, na.rm = T)*100)

### By donor 2010-2020 timeseries ### 

donor <- rec %>%
  group_by(donor_code, donor_name, year) %>%
  summarise(value = sum(usd_disbursement_defl, na.rm = T)) %>%
  group_by(year) %>%
  mutate(`proportion per year (%)` = value / sum(value, na.rm = T)*100) %>%
  mutate(type = ifelse(
    donor_code %in% DACdonors_codes, "DAC", 
    ifelse(
      donor_code %in% non_DAC_donors_codes, "Non-DAC bilateral", "Multilateral")))

### Channel code 2010-2020 timeseries ### 

channel <- rec %>%
  mutate(channel = ifelse(
    parent_channel_code == 11000, "Donor Government",
    ifelse(
      parent_channel_code == 12000, "Recipient Govenrment",
      ifelse(
        parent_channel_code == 13000, "Third Country Government",
        ifelse(
          parent_channel_code == 21000, "International NGO",
          ifelse(
            parent_channel_code == 22000, "Donor country-based NGO",
            ifelse(
              parent_channel_code == 23000, "Developing country-based NGO",
              ifelse(
                parent_channel_code == 31000, "Public-Private Partnership (PPP)",
                ifelse(
                  parent_channel_code == 32000, "Network",
                  ifelse(
                    parent_channel_code %in% c(41000:41600), "United Nations (UN)",
                    ifelse(
                      parent_channel_code == 42000, "European Union Institution (EU)",
                      ifelse(
                        parent_channel_code == 43000, "International Monetary Fund (IMF)",
                        ifelse(
                          parent_channel_code == 44000, "World Bank Group (WB)",
                          ifelse(
                            parent_channel_code == 46000, "Regional Development Bank",
                            ifelse(
                              parent_channel_code == 47000, "Other multilateral institution",
                              ifelse(
                                parent_channel_code == 51000, "University, college or other teaching institution, research institute or think‑tank",
                                ifelse(
                                  parent_channel_code == 61000, "Private sector in provider country",
                                  ifelse(
                                    parent_channel_code == 62000, "Private sector in recipient country",
                                    ifelse(
                                      parent_channel_code == 63000, "Private sector in third country", 
                                      ifelse(
                                        parent_channel_code == 10000, "Public sector institutions (unspecified)",
                                        ifelse(
                                          parent_channel_code == 20000, "NGOs and civil society (unspecified)",
                                          ifelse(
                                            parent_channel_code == 30000, "Public-Private Partnership (PPP) and Networks (unspecified)",
                                            ifelse(
                                              parent_channel_code == 40000, "Multilateral organisations (unspecified)",
                                              ifelse(
                                                parent_channel_code == 60000, "Private sector institution (unspecified)", "Other"))))))))))))))))))))))))

channel$channel[is.na(channel$channel)] <- "NA"

channel_timeseries <- channel %>%
  group_by(channel, year) %>%
  summarise(value = sum(usd_disbursement_defl, na.rm = T)) %>%
  group_by(year) %>%
  mutate(`proportion per year (%)` = value / sum(value, na.rm = T)*100)

### Sectors ###

sector <- rec %>%
  group_by(sector_code, sector_name, year) %>%
  summarise(value = sum(usd_disbursement_defl, na.rm = T)) %>%
  group_by(year) %>%
  mutate(`proportion per year (%)` = value / sum(value, na.rm = T)*100)

sector$sector_name[is.na(sector$sector_code)] <- "NA"

### Environmental ODA ### 

#ODA_DAC <- CRS_ODA %>%
#  filter(donor_code %in% DACdonors_codes) 
#write_parquet(ODA_DAC, "./ODA_DAC.parquet")
ODA_DAC <- read_parquet("~/Desktop/R_projects/R_CRS/ODA_DAC.parquet")

rec_bilateral_allocable <- ODA_DAC %>%
  filter(year >= 2010,
         recipient_name == "Mozambique",
         aid_t %in% bilateral_allocable)

env_oda_dac <- rec_bilateral_allocable %>%
  filter(environment %in% c(1,2) | biodiversity %in% c(1,2) | climate_mitigation %in% c(1,2)|
           climate_adaptation %in% c(1,2)| desertification %in% c(1,2)) 

aggregate_env <- env_oda_dac %>%
  group_by(year) %>%
  summarise(`Aggregate environmental ODA per year` = sum(usd_commitment_defl, na.rm = T)) 

biodiversity <- env_oda_dac %>%
  filter(biodiversity %in% c(1,2)) %>%
  group_by(year) %>%
  summarise(value = sum(usd_commitment_defl, na.rm = T)) %>%
  mutate(marker = "Biodiversity")

mitigation <- env_oda_dac %>%
  filter(climate_mitigation %in% c(1,2)) %>%
  group_by(year) %>%
  summarise(value = sum(usd_commitment_defl, na.rm = T)) %>%
  mutate(marker = "Climate migigation")

adaptation <- env_oda_dac %>%
  filter(climate_adaptation %in% c(1,2)) %>%
  group_by(year) %>%
  summarise(value = sum(usd_commitment_defl, na.rm = T)) %>%
  mutate(marker = "Climate adaptation")

desertification <- env_oda_dac %>%
  filter(desertification %in% c(1,2)) %>%
  group_by(year) %>%
  summarise(value = sum(usd_commitment_defl, na.rm = T)) %>%
  mutate(marker = "Desertification")

env_merged <- biodiversity %>%
  rbind(mitigation, adaptation, desertification) %>%
  left_join(aggregate_env, "year") %>%
  mutate(`proportion per year (%)` = value/`Aggregate environmental ODA per year`*100) %>%
  select(1,3,2,5,4)

### Gender marker ###

rec_bilateral_allocable$gender[is.na(rec_bilateral_allocable$gender)] <- 99

oda_gender <- rec_bilateral_allocable %>%
  mutate(significance = ifelse(
    gender == 2, "Gender-focused aid, principal objective", 
    ifelse(
      gender == 1, "Gender-focused aid, significant objective",
      ifelse(
        gender == 0, "Screened, not targeted", "Not screened"
      )))) %>%
  group_by(year, significance) %>%
  summarise(value = sum(usd_commitment_defl, na.rm = T)) %>%
  group_by(year) %>%
  mutate(`proportion per year (%)` = value / sum(value, na.rm = T)*100)


### OOF (net) ###

#CRS_OOF <- CRS_raw %>%
#  filter(flow_code == 14)
#write_parquet(CRS_OOF, "./CRS_OOF.parquet")
#CRS_OOF <- read_parquet("~/Desktop/R_projects/R_CRS/CRS_OOF.parquet")

oof <- fread("~/Desktop/R_projects/R_DAC2b (OOF)/Data/Table2b_Data.csv") %>%
  clean_names()

#Excluding debt relief and keep only constant prices

oof2 <- oof %>%
  filter(recipient_2 == "Mozambique",
         year >= 2010,
         aidtype %!in% c(295,298),
         amount_type == "Constant Prices (2020 USD millions)") 

oof_agg <- oof2 %>%
  filter(donor_2 == "Official Donors, Total") %>%
  group_by(year) %>%
  summarise(value = sum(value, na.rm = T)) 

oof_donor <- oof2 %>%
  group_by(year, donor_2) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  group_by(year) %>%
  mutate(`proportion per year (%)` = value / sum(value, na.rm = T)*100)

### Private flows ###

CRS_raw <- read_parquet("~/Desktop/R_projects/R_CRS/CRS_raw.parquet")

private <- CRS_raw %>%
  filter(year >= 2010,
         recipient_name == "Mozambique",
         donor_code %in% private_donors_codes)

private_agg <- private %>%
  group_by(year) %>%
  summarise(value = sum(usd_disbursement_defl, na.rm = T)) 

private_donor <- private %>%
  group_by(year, donor_name) %>%
  summarise(value = sum(usd_disbursement_defl, na.rm = T)) %>%
  mutate(`proportion per year (%)` = value / sum(value, na.rm = T)*100)

private_sector <- private %>%
  group_by(year, sector_code, sector_name) %>%
  summarise(value = sum(usd_disbursement_defl, na.rm = T)) %>%
  group_by(year) %>%
  mutate(`proportion per year (%)` = value / sum(value, na.rm = T)*100)
  

list <- list("Aggregate (gross)" = agg_gross,
             "Aggregate (net)" = agg_net,
             "HDP" = hdp_oda,
             "Donor" = donor,
             "Channel" = channel_timeseries,
             "Sector" = sector,
             "Environmental ODA" = env_merged,
             "Gender ODA" = oda_gender,
             "OOF aggregate (net)" = oof_agg,
             "OOF donor (net)" = oof_donor,
             "Private aggregate" = private_agg,
             "Private donor" = private_donor,
             "Private sector" = private_sector)

write_xlsx(list, "Output/Financing strategy data package Mozambique.xlsx")
  
  
