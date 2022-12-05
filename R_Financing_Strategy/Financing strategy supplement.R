### Financing strategy supplement

donor <- rec %>%
  group_by(donor_name, year) %>%
  summarise(value = sum(usd_disbursement_defl, na.rm = T)) %>%
  mutate(`proportion (%)` = value / sum(value, na.rm = T)*100) %>%
  mutate(type = ifelse(
    donor_name %in% DACdonors_names, "DAC", "Non-DAC"))

agg_gross$value <- format(round(agg_gross$value,1),nsmall=1)
agg_net$value <- format(round(agg_net$value,1),nsmall=1)

hdp_oda$value <- format(round(hdp_oda$value,1),nsmall=1)
hdp_oda$`proportion (%)` <- format(round(hdp_oda$`proportion (%)`,1),nsmall=1)

donor$value <- format(round(donor$value,1),nsmall=1)
donor$`proportion (%)` <- format(round(donor$`proportion (%)`,1),nsmall=1)

channel_agg$value <- format(round(channel_agg$value,1),nsmall=1)
channel_agg$`proportion (%)` <- format(round(channel_agg$`proportion (%)`,1),nsmall=1)

sector$value <-  format(round(sector$value,1),nsmall=1)
sector$`proportion (%)` <- format(round(sector$`proportion (%)`,1),nsmall=1)

oof_agg$value <-  format(round(oof_agg$value,1),nsmall=1)
oof_donor$value <-  format(round(oof_donor$value,1),nsmall=1)

private_agg$value <-  format(round(private_agg$value,1),nsmall=1)
private_donor$value <-  format(round(private_donor$value,2),nsmall=2)

private_sector$value <-  format(round(private_sector$value,2),nsmall=2)
private_sector$`proportion (%)` <-  format(round(private_sector$`proportion (%)`,2),nsmall=2)


ifelse(
  parent_channel_code == 41300, "Other UN",
  ifelse(
    parent_channel_code == 41400, "UN inter-agency pooled funds",
    ifelse(
      parent_channel_code == 41500, "UN single-agency thematic funds",
      ifelse(
        parent_channel_code == 41600, "Existing UN channels not included in Standard I - UN entity- of the UN Data Cube reporting framework",