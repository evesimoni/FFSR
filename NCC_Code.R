# Load Libraries
library(haven)
library(dplyr)
library(magrittr)
library(tidyverse)
library(knitr)
library(fixest)
library(estimatr)
library(stargazer)
library(xtable)
library(miceadds)
library(modelsummary)
library(texreg)
library(ggplot2)
library(tidyr)
library(stats)
library(readxl)
library(lubridate)
library(vtable)
library(naniar)
library(survival)
library(survminer)
library(MALDIquant)
library(slider)
library(WriteXLS)
library(gplots)
library(lfe)
library(ggrepel)
library(stringr)
library(openxlsx)
library(gridExtra)
library(cowplot)

## ------------------------------------------------------------------------------------------------------------------
# Set working directory to folder with downloaded data
# setwd()

## ------------------------------------------------------------------------------------------------------------------
ffsr <- read_excel("ffsr.xlsx") %>%
  mutate(date = as.Date(date))

# Add list of names of countries
list_names <- c("Algeria","Angola","Azerbaijan","Bahrain","Ecuador","Egypt","Indonesia",
                "Iran","Iraq","Kuwait","Libya","Malaysia","Myanmar","Nigeria","Oman",
                "Qatar","Saudi_Arabia","Sudan","Trinidad and Tobago","UAE","Venezuela")

# create limits for plots
lims <- as.Date(c("2000-01-01 00:00", "2023-12-01 00:00"), format = "%Y-%m-%d")

# calculate the mean benchmark gap for all countries
mean_bmgap <- ffsr %>% group_by(date) %>% summarise(mean_bmgap = mean(bm_gap_2015, na.rm=TRUE))

# plot and save all countries timelines
figure6 <- ggplot() +
  geom_vline(xintercept = as.Date("2016-01-01"), color="darkgreen", size=1) +
  geom_hline(data=ffsr, aes(x=date, yintercept = 0), alpha=0.8) +
  geom_line(data=ffsr, aes(x=date, y=bm_gap_2015, group=country), alpha=0.3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y",
               limits = lims, expand = c(0,0)) +
  scale_y_continuous(limits = c(-1,1.7),
                     breaks = seq(-1,1.5,0.5)) +
  geom_line(data = mean_bmgap, aes(x=date, y=mean_bmgap), color="#C93312", size=1.1) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
  labs(title = "",
       caption = "Each line represents a country’s tax or subsidy on a liter of gasoline in constant 2015 dollars.\n The red line represents the mean value.\n The sample includes all countries that maintained net subsidies from January 2003 to January 2015",
       x="", y="") 

figure6
ggsave("figure6.pdf", plot = figure6)

# Create table 1 Changes in Net Taxes from 2016 to 2023.
table1 <- list()
list_names2 <- list_names[list_names != "Venezuela"]
  # not inlcuding Venezuela due to lack of ERs.

for (each_country in list_names2) {
  temp <- filter(ffsr, country == each_country & date >= "2016-01-01" & date <= "2016-03-01")
  table1$mean_firstq[[each_country]] <- mean(temp$bm_gap_2015, na.rm=TRUE)
  temp <- filter(ffsr, country == each_country & date >= "2023-10-01" & date <= "2023-12-01")
  table1$mean_lastq[[each_country]] <- mean(temp$bm_gap_2015, na.rm=TRUE)
}

table2 <- as.data.frame(do.call(cbind, table1)) %>%
  mutate(mean_firstq = as.numeric(mean_firstq),
         mean_lastq = as.numeric(mean_lastq),
         diff = (-1)*(as.numeric(mean_lastq)-as.numeric(mean_firstq))) %>%
  arrange(desc(diff))

# Rename the row name "Saudi_Arabia" to "Saudi Arabia" in table2
rownames(table2) <- gsub("Saudi_Arabia", "Saudi Arabia", rownames(table2))

table1 <- kable(table2,
                format = "latex",
                caption = "",
                digits = 2,
                col.names = c("Taxes Q1-2016","Taxes Q4-2023","Difference"))

print(table1)
cat(table1)

### FIXITY CODING ####
# Create New Variable for % Changes in monthly prices
ffsr <- ffsr %>% group_by(country) %>% 
  mutate_each(funs(chg_price_pct = ((.-lag(.))/lag(.))*100), price) %>%
  mutate_each(funs(chg_price_pct3 = ((.-lag(.,3))/lag(.,3))*100), price) %>%
  # Create a Dummy for Cases of % changes higher than 10
  mutate(reform = ifelse(chg_price_pct >= 10, 1, 0)) %>%
  # Delete NAs in dummy variable
  mutate(reform = coalesce(reform, 0))

# Remove Special Case Myanmar
ffsr$reform[ffsr$country == "Myanmar" & ffsr$date == "2012-01-01"] <- 0
ffsr$reform[ffsr$country == "Myanmar" & ffsr$date == "2021-05-01"] <- 0

## ------------------------------------------------------------------------------------------------------------------
# Import the "updated" excel files and merge
# List all Excel files with the pattern "fixity_*.xlsx" in a specific directory
file_list <- list.files(pattern = "fixity_.*\\.xlsx$",
                        full.names = TRUE)

# Read each Excel file into a list of data frames
data_list <- lapply(file_list, read.xlsx)

# Bind all data frames together
ffsr1 <- bind_rows(data_list) %>%
  select(country_date, floating)

# Merge with original DF
ffsr <- merge(ffsr, ffsr1, by = c("country_date"))

# Create new fixity variable
ffsr <- ffsr %>%  
  # Create fixity_reform and end_fixity_reform variables
  group_by(country) %>%
  mutate_each(funs(floating_chg = ((.-lag(.)))), floating) %>%
  # start fixity reform when it goes from fixed to floating
  mutate(start_fixity_reform = ifelse(floating_chg == 1, 1, ifelse(is.na(floating_chg), 1, 0)),
         # end fixity reform when it goes from floating to fixed
         end_fixity_reform = if_else(floating_chg == -1, 1, 0)) %>%
  # Delete NAs in dummy variable
  mutate(start_fixity_reform = coalesce(start_fixity_reform, 0),
         end_fixity_reform = coalesce(end_fixity_reform, 0)) %>%
  # Delete column
  select(-floating_chg)


## ------------------------------------------------------------------------------------------------------------------
# New Reform Variable: if prices are floating, then reform is always 0. 
# If prices are fixed, then new_reform is equal to price reform (10% increase in nominal domestic prices)
ffsr <- ffsr %>%
  mutate(new_reform = ifelse(floating == 1, 0, reform)) %>%
  # Delete NAs
  mutate(new_reform = coalesce(new_reform, 0))

# Remove Special Case Myanmar
ffsr$new_reform[ffsr$country == "Myanmar" & ffsr$date == "2012-01-01"] <- 0
ffsr$new_reform[ffsr$country == "Myanmar" & ffsr$date == "2021-05-01"] <- 0

# Subset and Create Year
ffsr_increases <- ffsr %>% filter(new_reform == 1)  %>% arrange((country)) %>%
  # create year
  mutate(year = substr(date,1,4))

# Plot and save onset of fixity reforms per country
figure8 <- ggplot(ffsr_increases, aes(x=date, y=factor(country, rev(sort(list_names))),
                           color=postparis)) +
  geom_point(size=0.9) +
  theme_minimal() +
  labs(x = "", y = "", title = "") +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y",
               limits = lims, expand = c(0,0)) +
  scale_color_manual(values = c("#D69C4E","#046C9A"), limits = c("pre", "post")) 

figure8
ggsave("figure8.pdf", plot = figure8)


## ------------------------------------------------------------------------------------------------------------------
# create variable for "all reform" (either price or fixity reform)
ffsr <- ffsr %>%
  mutate(all_reform = ifelse(new_reform == 1 | start_fixity_reform == 1, 1, 0))  %>%
  mutate(type_all_reform = ifelse(new_reform == 1 & start_fixity_reform == 1, "both",
                                  ifelse(new_reform == 1 & start_fixity_reform == 0, "price",
                                         ifelse(new_reform == 0 & start_fixity_reform == 1, "fixity",
                                                "none")))) 

# Subset DF and Create Year
ffsr_increases <- ffsr %>% filter(all_reform == 1) %>% 
  arrange((country)) %>%
  # create year
  mutate(year = substr(date,1,4))

# Plot and save All Reforms per country
figure10 <- ggplot(ffsr_increases, 
       aes(x=date, y=factor(country, rev(sort(list_names))), color=postparis)) +
  geom_point(size=0.9) +
  theme_minimal() +
  labs(x = "", y = "", title = "") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y", limits = lims, expand = c(0,0)) +
  scale_color_manual(values = c("#D69C4E","#046C9A"), limits = c("pre","post")) 

figure10
ggsave("figure10.pdf", plot = figure10)


## --------------------------------------------------------------------------------
# Create Plot Count of Reforms per year

figure2 <- ffsr %>% 
  group_by(year = lubridate::floor_date(date, "year")) %>%
  summarise(count_new_reform = sum(new_reform),
            count_start_fixity_reform = sum(start_fixity_reform)) %>%
  pivot_longer(cols = starts_with("count"), names_to = "type", values_to = "count") %>%
  mutate(postparis = if_else(year >= "2016-01-01", "post", "pre"),
         type = recode(type, "count_new_reform" = "Price Reform", 
                       "count_start_fixity_reform" = "Fixity Reform"),
         interaction_type = interaction(type, postparis, sep = " - ")) %>%
  mutate(interaction_type = factor(interaction_type, 
                                   levels = c("Fixity Reform - pre",
                                              "Price Reform - pre", 
                                              "Fixity Reform - post",
                                              "Price Reform - post"))) %>%
  ggplot(aes(x = year, y = count, fill = interaction_type)) +
  geom_col(position = "stack") +
  scale_x_date(date_breaks = "1 year", date_labels = "%y", limits = lims, expand = c(0,0)) +
  scale_y_continuous(labels = function(x) floor(x)) +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "",
       legend = "", fill = "") +
  scale_fill_manual(values = c(
      "Price Reform - pre" = "#D69C4E",
      "Fixity Reform - pre" = "#eac48d",
      "Price Reform - post" = "#046C9A",
      "Fixity Reform - post" = "#6baacf"),
    labels = c("Pre Paris - Fixity Reform",
               "Pre Paris - Price Reform",
               "Post Paris - Fixity Reform",
               "Post Paris - Price Reform")) +
 theme(legend.position = "bottom",
       legend.text = element_text(size = 8),
       legend.title = element_text(size = 8)) 

figure2
ggsave("figure2.pdf", plot = figure2, width = 10, height = 15)


## ------------------------------------------------------------------------------------------------------------------
# SURVIVAL CURVES

# Create lag Variables
ffsr <- ffsr %>% 
  mutate(price.1 = lag(price),
         usd.1 = lag(price_usd_2015),
         gap.1 = lag(bm_gap_2015))

# Creating reform-only list
monthly_reforms <- list()
nom_reforms <- list()
usd_reforms <- list()
gap_reforms <- list()
ffsr_select <- ffsr[,c("country","date",
                       "price","price.1",
                       "price_usd_2015","usd.1",
                       "bm_gap_2015","gap.1",
                       "new_reform")] 

###### PRICE REFORMS #########

## NOMINAL PRICE, REAL USD, BM_GAP ADJ ##
for(i in 1:sum(ffsr_select$new_reform)) {
  # Identifying reforms and storing each reform as its own dataframe
  monthly_reforms[[i]] <- ffsr_select[ffsr_select$new_reform==1,][i,]
  # Adding to each reform dataframe all months that follow
  monthly_reforms[[i]] <- rbind(monthly_reforms[[i]],
                                    ffsr_select[ffsr_select$country == monthly_reforms[[i]]$country &
                                                  ffsr_select$date > monthly_reforms[[i]]$date,])
  # Create Reform date var for each reform
  monthly_reforms[[i]]$reform_start <- monthly_reforms[[i]]$date[1] 
  # Add Pre-reform price for each reform: nominal, real usd, and bm gap adj
  monthly_reforms[[i]]$pre.ref.price <- monthly_reforms[[i]]$price.1[1]
  monthly_reforms[[i]]$pre.ref.usd <- monthly_reforms[[i]]$usd.1[1]
  monthly_reforms[[i]]$pre.ref.gap <- monthly_reforms[[i]]$gap.1[1]
  # Add dummy for the month in which the reform failed 
  monthly_reforms[[i]]$ref_nom_fail <- 1*(monthly_reforms[[i]]$price <= 
                                            monthly_reforms[[i]]$pre.ref.price)
  
  monthly_reforms[[i]]$ref_usd_fail <- 1*(monthly_reforms[[i]]$price_usd_2015 <= 
                                            monthly_reforms[[i]]$pre.ref.usd)
  
  monthly_reforms[[i]]$ref_gap_fail <- 1*(monthly_reforms[[i]]$bm_gap_2015 <= 
                                            monthly_reforms[[i]]$pre.ref.gap)
  # Spell Lengtzh
  monthly_reforms[[i]]$spell <- as.numeric(difftime(monthly_reforms[[i]]$date, 
                                                    monthly_reforms[[i]]$reform_start, 
                                                    units = "days"))/365.25

  # NOM: Reducing df to one row
  nom_reforms[[i]] <- monthly_reforms[[i]]
  # Status = 0 if reform did not failed, and = 1 if reform failed
  if(sum(nom_reforms[[i]]$ref_nom_fail, na.rm = TRUE) > 0) {
    nom_reforms[[i]] <- nom_reforms[[i]][nom_reforms[[i]]$ref_nom_fail==1,][1,]
    nom_reforms[[i]]$status <- 1} else {
      nom_reforms[[i]] <- nom_reforms[[i]][1,]
      nom_reforms[[i]]$spell <- (as.numeric(as.Date("2023-12-01") - nom_reforms[[i]]$reform_start)/365.25)
      nom_reforms[[i]]$status <- 0}

  # USD: Reducing df to one row
  usd_reforms[[i]] <- monthly_reforms[[i]]
  # Status = 0 if reform did not failed, and = 1 if reform failed
  if(sum(usd_reforms[[i]]$ref_usd_fail, na.rm = TRUE) > 0) {
    usd_reforms[[i]] <- usd_reforms[[i]][usd_reforms[[i]]$ref_usd_fail==1,][1,]
    usd_reforms[[i]]$status <- 1} else {
      usd_reforms[[i]] <- usd_reforms[[i]][1,]
      usd_reforms[[i]]$spell <- (as.numeric(as.Date("2023-12-01") - usd_reforms[[i]]$reform_start)/365.25)
      usd_reforms[[i]]$status <- 0}
  
  # GAP: Reducing df to one row
  gap_reforms[[i]] <- monthly_reforms[[i]]
  # Status = 0 if reform did not failed, and = 1 of reform failed
  if(sum(gap_reforms[[i]]$ref_gap_fail, na.rm = TRUE) > 0) {
    gap_reforms[[i]] <- gap_reforms[[i]][gap_reforms[[i]]$ref_gap_fail==1,][1,]
    gap_reforms[[i]]$status <- 1} else {
      gap_reforms[[i]] <- gap_reforms[[i]][1,]
      gap_reforms[[i]]$spell <- (as.numeric(as.Date("2023-12-01") - gap_reforms[[i]]$reform_start)/365.25)
      gap_reforms[[i]]$status <- 0}
}

# Bind all Reforms
nom_reforms <- bind_rows(nom_reforms) %>% 
  transmute(country = country,
            date_ref_start = reform_start,
            ref_nom_fail = ref_nom_fail,
            spell_nom = spell)

usd_reforms <- bind_rows(usd_reforms) %>%
  transmute(country = country,
            date_ref_start = reform_start,
            ref_usd_fail = ref_usd_fail,
            spell_usd = spell)

gap_reforms <- bind_rows(gap_reforms) %>%
  transmute(country = country,
            date_ref_start = reform_start,
            ref_gap_fail = ref_gap_fail,
            spell_gap = spell)

all_reforms_full_10 <- merge(nom_reforms, usd_reforms,
                             by=c("country","date_ref_start"))
all_reforms_full_10 <- merge(all_reforms_full_10, gap_reforms,
                             by=c("country","date_ref_start"))

# Calculate Survival Estimate
nom_fit <- survfit(Surv(spell_nom, ref_nom_fail) ~ 1, data = nom_reforms)
usd_fit <- survfit(Surv(spell_usd, ref_usd_fail) ~ 1, data = usd_reforms)
bmg_fit <- survfit(Surv(spell_gap, ref_gap_fail) ~ 1, data = gap_reforms)

# Storing Survival Estimates:
# Create DF and store in a CSV file for each
nom_fit_df <- rbind(c(0,summary(nom_fit)$n,0,0,1,NA),as.data.frame(summary(nom_fit)[2:7]))
write.csv(nom_fit_df, file = "new_reform_10_nominal.csv", row.names = F)
usd_fit_df <- rbind(c(0,summary(usd_fit)$n,0,0,1,NA),as.data.frame(summary(usd_fit)[2:7]))
write.csv(usd_fit_df, file = "new_reform_10_usd.csv", row.names = F)
bmg_fit_df <- rbind(c(0,summary(bmg_fit)$n,0,0,1,NA),as.data.frame(summary(bmg_fit)[2:7]))
write.csv(bmg_fit_df, file = "new_reform_10_bmg.csv", row.names = F)


######## PRE PARIS ##################################

# Calculate Survival Estimate 
nom_fit <- survfit(Surv(spell_nom, ref_nom_fail) ~ 1, 
                   data = filter(nom_reforms, date_ref_start <= "2015-12-01"))
usd_fit <- survfit(Surv(spell_usd, ref_usd_fail) ~ 1, 
                   data = filter(usd_reforms, date_ref_start <= "2015-12-01"))
bmg_fit <- survfit(Surv(spell_gap, ref_gap_fail) ~ 1, 
                   data = filter(gap_reforms, date_ref_start <= "2015-12-01"))

# Storing Survival Estimates:
# Create DF and store in a CSV file for each
nom_fit_df <- rbind(c(0,summary(nom_fit)$n,0,0,1,NA),as.data.frame(summary(nom_fit)[2:7]))
write.csv(nom_fit_df, file = "new_reform_10_nominal_preparis.csv", row.names = F)
usd_fit_df <- rbind(c(0,summary(usd_fit)$n,0,0,1,NA),as.data.frame(summary(usd_fit)[2:7]))
write.csv(usd_fit_df, file = "new_reform_10_usd_preparis.csv", row.names = F)
bmg_fit_df <- rbind(c(0,summary(bmg_fit)$n,0,0,1,NA),as.data.frame(summary(bmg_fit)[2:7]))
write.csv(bmg_fit_df, file = "new_reform_10_bmg_preparis.csv", row.names = F)


######## POST PARIS ##################################

# Calculate Survival Estimate 
nom_fit <- survfit(Surv(spell_nom, ref_nom_fail) ~ 1, 
                   data = filter(nom_reforms, date_ref_start >= "2016-01-01"))
usd_fit <- survfit(Surv(spell_usd, ref_usd_fail) ~ 1, 
                   data = filter(usd_reforms, date_ref_start >= "2016-01-01"))
bmg_fit <- survfit(Surv(spell_gap, ref_gap_fail) ~ 1, 
                   data = filter(gap_reforms, date_ref_start >= "2016-01-01"))

# Storing Survival Estimates:
# Create DF and store in a CSV file for each
nom_fit_df <- rbind(c(0,summary(nom_fit)$n,0,0,1,NA),as.data.frame(summary(nom_fit)[2:7]))
write.csv(nom_fit_df, file = "new_reform_10_nominal_postparis.csv", row.names = F)
usd_fit_df <- rbind(c(0,summary(usd_fit)$n,0,0,1,NA),as.data.frame(summary(usd_fit)[2:7]))
write.csv(usd_fit_df, file = "new_reform_10_usd_postparis.csv", row.names = F)
bmg_fit_df <- rbind(c(0,summary(bmg_fit)$n,0,0,1,NA),as.data.frame(summary(bmg_fit)[2:7]))
write.csv(bmg_fit_df, file = "new_reform_10_bmg_postparis.csv", row.names = F)



##########3# FIXITY REFORMS #############

# Creating reform-only list
monthly_fixity_reforms <- list()
fixity_reforms <- list()
ffsr_select <- ffsr[,c("country","date","start_fixity_reform","end_fixity_reform")]

for(i in 1:sum(ffsr_select$start_fixity_reform)) {
  # Identifying reforms and storing each reform as its own dataframe
  monthly_fixity_reforms[[i]] <- ffsr_select[ffsr_select$start_fixity_reform==1,][i,]
  # Adding to each reform dataframe all months that follow
  monthly_fixity_reforms[[i]] <- rbind(monthly_fixity_reforms[[i]],
                                       ffsr_select[ffsr_select$country == monthly_fixity_reforms[[i]]$country &
                                                ffsr_select$date > monthly_fixity_reforms[[i]]$date,])
  # Create Reform date var for each reform
  monthly_fixity_reforms[[i]]$ref_fix_start <- monthly_fixity_reforms[[i]]$date[1] 
  # Add dummy for the month in which the reform failed 
  monthly_fixity_reforms[[i]]$ref_fix_fail <- 1*(monthly_fixity_reforms[[i]]$end_fixity_reform)
  
  # Spell Length
  monthly_fixity_reforms[[i]]$spell <- as.numeric(difftime(monthly_fixity_reforms[[i]]$date, monthly_fixity_reforms[[i]]$ref_fix_start, units = "days")) / 365.25

  # Reducing df to one row: start and end based
  fixity_reforms[[i]] <- monthly_fixity_reforms[[i]]
  # Status = 0 if reform did not failed, and = 1 if reform failed
  if(sum(fixity_reforms[[i]]$ref_fix_fail, na.rm = TRUE) > 0) {
    fixity_reforms[[i]] <- fixity_reforms[[i]][fixity_reforms[[i]]$ref_fix_fail==1,][1,]
    fixity_reforms[[i]]$status <- 1} else {
      fixity_reforms[[i]] <- fixity_reforms[[i]][1,]
      fixity_reforms[[i]]$spell <- (as.numeric(as.Date("2023-12-01") - fixity_reforms[[i]]$ref_fix_start)/365.25)
      fixity_reforms[[i]]$status <- 0}
}

# Bind all Reforms
all_reforms_fixity <- bind_rows(fixity_reforms) %>%
  transmute(country = country,
            date_ref_start = ref_fix_start,
            date_ref_fail = if_else(ref_fix_fail == 1,
                                    as.Date(date), as.Date("2024-01-01")),
            ref_fix_fail = ref_fix_fail,
            spell_fixity = spell)

# Estimate Survival
fix_fit <- survfit(Surv(spell_fixity, ref_fix_fail) ~ 1, data = all_reforms_fixity)

# Create DF and store in a CSV file for each
fix_fit_df <- rbind(c(0,summary(fix_fit)$n,0,0,1,NA),as.data.frame(summary(fix_fit)[2:7]))
write.csv(fix_fit_df, file = "fixity.csv", row.names = F)

# Ensure date columns are properly converted to Date class
fixity_all <- all_reforms_fixity %>% 
  mutate(date_ref_start = as.Date(date_ref_start),
         date_end = date_ref_fail,
         postparis = if_else(date_ref_start >= as.Date("2016-01-01"), "post", "pre"))

# Plot Survival Curves
figure7 <- ggplot(fixity_all, aes(y=country, 
                       yend=country,
                       x=date_ref_start,
                       xend=date_end,
                       color=postparis)) +
  geom_segment(size=1.5) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_x_date(date_breaks = "2 years",
               date_labels = "%Y",
               limits = as.Date(c("2000-01-01","2024-01-01")),
               expand = c(0,0.1)) +
  labs(x = "", y = "", title = "") +
  scale_color_manual(values = c("#D69C4E","#046C9A"),
                     labels = c("Reform began before Paris","Reform began after Paris"),
                     breaks = c("pre","post"))

ggsave("figure7.pdf", plot = figure7)

# Read and Bind All Survival Estimates
csv_names <- list.files(pattern = "*.csv", 
                        full.names = TRUE) 

csv_names2 <- data.frame(period = csv_names, 
                         id = as.character(1:length(csv_names)))

estimates <- csv_names %>% 
  lapply(read_csv) %>% bind_rows(.id = "id") %>% left_join(csv_names2) %>%
  mutate(period = str_remove(period, "./")) %>%
  mutate(period = str_remove(period, ".csv")) %>%
  arrange(desc(period)) %>%
  mutate(period2 = ifelse(str_detect(period, "pre"), "pre", 
                          ifelse(str_detect(period, "post"), "post",
                                 "full")),
         measure = ifelse(str_detect(period, "nominal"), "nominal",
                       ifelse(str_detect(period, "usd"), "usd",
                              ifelse(str_detect(period, "bmg"), "bmg",
                                     "fixity"))),
         
         type_reform = ifelse(str_detect(period, "new_reform_10"), "new_reform_10",
                              ifelse(str_detect(period, "new_reform_25"), "new_reform_25",
                                     ifelse(str_detect(period, "tax_reform_10"), "tax_reform_10",
                                            ifelse(str_detect(period, "tax_reform_25"), "tax_reform_25",
                                     "fixity")))))


# Plot BM_GAP PRE & POST PARIS
figure4 <- estimates %>%
  filter(type_reform == "new_reform_10") %>%
  filter(period2 == "pre" | period2 == "post") %>%
  filter(measure == "bmg") %>% 
  ggplot() +
  geom_line(aes(x=time, 
                y=surv, 
                group=id, 
                linetype=period2, 
                color=period2), size = 1.1) + 
  theme_light() + 
  labs(title = "",
       x = "", y = "Duration in years", 
       linetype="", color="",
       legend="")  +
  scale_x_continuous(breaks = seq(0,5,1), limits = c(0,5),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1),
                     expand = c(0,0)) +
  scale_linetype_manual(values = c(1,2)) +
  guides(linetype = "none") +
  scale_color_manual(limits = c("pre", "post"),
                     values = c("#D69C4E","#046C9A"),
                     labels = c("Pre Paris","Post Paris")) +
  theme(legend.position = "bottom") 

figure4
ggsave("figure4.pdf", plot = figure4)

## PLOT PRICE VS FIXITY
figure5 <- estimates %>%
  filter(period == "new_reform_10_bmg" | 
           period == "fixity") %>%
  ggplot() +
  geom_line(aes(x=time, 
                y=surv,
                group=period, 
                linetype=period, 
                color=period), 
            size = 1.1) + 
  theme_light() + 
  labs(title = "",
       x = "", y = "Duration in years", 
       linetype="", color="",
       legend="")  +
  scale_x_continuous(breaks = seq(0,5,1), limits = c(0,5),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1),
                     expand = c(0,0)) +
  scale_linetype_manual(values = c(1,2)) +
  guides(linetype = "none") +
  scale_color_manual(limits = c("new_reform_10_bmg", "fixity"),
                     values = c("#D69C4E","#046C9A"),
                     labels = c("Price Reform","Fixity Reform")) +
  theme(legend.position = "bottom") 

figure5
ggsave("figure5.pdf", plot = figure5)



##### PLOT NIGERIA #######

nigeria <- filter(ffsr, country == "Nigeria") %>%
  mutate(price_reform = "Price Reform")

# Nominal Price
plot_price <- ggplot(nigeria, aes(x=date, y=price)) +
                       geom_line() +
                       scale_x_date(breaks = "2 years", 
                                    date_labels = "%y",
                                    limits = lims, 
                                    expand = c(0,0)) +
                       labs(x="", y="",
                            title="") +
                       theme_minimal() 

plot_price
ggsave("nigeria1.pdf", plot = plot_price, width = 12, height = 5)

# USD Price
plot_usd <- ggplot(nigeria, aes(x=date, y=price_usd)) +
                       geom_line() +
                       scale_x_date(breaks = "2 years", 
                                    date_labels = "%y",
                                    limits = lims, 
                                    expand = c(0,0)) +
                       labs(x="", y="",
                            title="") +
                       theme_minimal() 

plot_usd
ggsave("nigeria2.pdf", plot = plot_usd, width = 12, height = 5)

# USD 2015 Price
plot_usd_2015 <- ggplot(nigeria, aes(x=date, y=price_usd_2015)) +
                       geom_line() +
                       scale_x_date(breaks = "2 years", 
                                    date_labels = "%y",
                                    limits = lims, 
                                    expand = c(0,0)) +
                       labs(x="", y="",
                            title="") +
                       theme_minimal() 

plot_usd_2015
ggsave("nigeria3.pdf", plot = plot_usd_2015, width = 12, height = 5)

# Benchmark Gap
price_bmg <- ggplot(nigeria, aes(x=date, y=bm_gap_2015)) +
  geom_hline(yintercept = 0, size = 0.3) +
  geom_line() +
  scale_x_date(breaks = "2 years", date_labels = "%y",
               limits = lims, expand = c(0,0)) +
  labs(x="", y="",
       title="") +
  theme_minimal() 

price_bmg
ggsave("nigeria4.pdf", plot = price_bmg, width = 12, height = 5)


# Price Reforms
plot_reforms <- ggplot(filter(nigeria, new_reform == 1), 
                       aes(x=date, y=price_reform)) +
  geom_point(size=3) +
  scale_x_date(breaks = "2 years", date_labels = "%y",
               limits = lims, expand = c(0,0)) +
  labs(x="", y="",
       title="") +
  theme_minimal() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) 

plot_reforms
ggsave("nigeria5.pdf", plot = plot_reforms, width = 12, height = 2)

# Plot Fixity
# Create the rolling average using dplyr and lag
ffsr <- ffsr %>%
  arrange(date) %>% 
  group_by(country) %>%
  mutate(
    bm_gap_lag1 = lag(bm_gap_2015, 1),
    bm_gap_lag2 = lag(bm_gap_2015, 2),
    rolling_avg_bm_gap_2015 = (bm_gap_2015 + bm_gap_lag1 + bm_gap_lag2) / 3,
    subsidizing_floater = ifelse(floating == 1 & rolling_avg_bm_gap_2015 < -0.2, 1, 0),
    non_subsidizing_floater = ifelse(floating == 1 & rolling_avg_bm_gap_2015 >= -0.2, 1, 0)
  ) %>%
  ungroup()

# Identify continuous periods
plot_fixity <- ffsr %>%
  filter(country == "Nigeria") %>%
  arrange(date) %>%
  mutate(period_group = cumsum(non_subsidizing_floater != lag(non_subsidizing_floater, 
                                                              default = 0))) %>%
  filter(non_subsidizing_floater == 1) %>%
  transmute(date = date,
            period_group = period_group,
            fixity_reform = "Fixity Reform") %>%
  ggplot(aes(x = date, y = fixity_reform, group = period_group)) + 
  geom_line(size = 1.8) +
  theme_minimal() +
  scale_x_date(breaks = "2 years",
               date_labels = "%y",
               limits = lims,
               expand = c(0,0))  +
  labs(x = "", y = "", title="") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) 

plot_fixity
ggsave("nigeria6.pdf", plot = plot_fixity, width = 12, height = 2)





# Bind Instances of Reforms DF (Price and Fixity)
all_reforms_full_10 <- all_reforms_full_10 %>%
  mutate(type = "price",
         "ref_fix_fail" = NA,
         "spell_fixity" = NA)

all_reforms_fixity <- all_reforms_fixity %>%
  select(-date_ref_fail) %>%
  mutate(type = "fixity",
         ref_nom_fail = NA,
         spell_nom = NA,
         ref_usd_fail = NA,
         spell_usd = NA,
         ref_gap_fail = NA,
         spell_gap = NA)

all_reforms <- bind_rows(all_reforms_full_10, all_reforms_fixity) %>%
  mutate(
    date_ref_start = as.Date(date_ref_start, format = "%Y-%m-%d"),
    failed_any = if_else(
      coalesce(ref_nom_fail, 0) == 1 | 
      coalesce(ref_usd_fail, 0) == 1 | 
      coalesce(ref_gap_fail, 0) == 1 | 
      coalesce(ref_fix_fail, 0) == 1, 
      "yes", "no")) %>%
  rowwise() %>%
  mutate(spell_min = min(c_across(c(spell_nom, spell_usd, spell_gap, spell_fixity)), na.rm = TRUE),
         date_ref_end = as.Date(date_ref_start) + (spell_min * 365.25)) %>%
  ungroup() %>%  # It is important to ungroup after using rowwise to avoid issues with the following operations
  mutate(date_ref_end = if_else(failed_any == "yes", 
                                date_ref_end, 
                                as.Date("2024-01-01", format = "%Y-%m-%d")),
         period = ifelse(date_ref_start >= "2016-01-01", "post", "pre"))

# Plot and Save Instances and Type of Reforms per Year
figure3 <- all_reforms %>% 
  filter(failed_any == "yes") %>%
  mutate(year = floor_date(date_ref_end, "year")) %>%
  group_by(year) %>%
  summarise(count_price = sum(type == "price"),
            count_fixity = sum(type == "fixity")) %>%
  pivot_longer(cols = starts_with("count"), 
               names_to = "type", 
               values_to = "count") %>%
  mutate(postparis = if_else(year >= as.Date("2016-01-01"), "post", "pre"),
         type = recode(type, 
                       "count_price" = "Price Reform", 
                       "count_fixity" = "Fixity Reform"),
         interaction_type = interaction(type, postparis, sep = " - ")) %>%
  mutate(interaction_type = factor(interaction_type, 
                                   levels = c("Fixity Reform - pre",
                                              "Price Reform - pre", 
                                              "Fixity Reform - post",
                                              "Price Reform - post"))) %>%
  ggplot(aes(x = year, y = count, fill = interaction_type)) +
  geom_col(position = "stack") +
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%y", 
               limits = lims, 
               expand = c(0,0)) +
  scale_y_continuous(labels = function(x) floor(x)) +
  theme_minimal() +
  labs(x = "", y = "", 
       title = "",
       legend = "", fill = "") +
  scale_fill_manual(values = c(
      "Price Reform - pre" = "#D69C4E",
      "Fixity Reform - pre" = "#eac48d",
      "Price Reform - post" = "#046C9A",
      "Fixity Reform - post" = "#6baacf"),
    labels = c("Pre Paris - Fixity Reform",
               "Pre Paris - Price Reform",
               "Post Paris - Fixity Reform",
               "Post Paris - Price Reform")) +
 theme(legend.position = "bottom",
       legend.text = element_text(size = 8),
       legend.title = element_text(size = 8)) 

figure3
ggsave("figure3.pdf", plot = figure3)


# which way reforms ended?
all_reforms <- all_reforms %>%
  mutate(
    type_fail = case_when(
      failed_any == "no" ~ NA_character_,
      spell_min == spell_nom ~ "backsliding",
      spell_min == spell_fixity ~ "backsliding",
      TRUE ~ "erosion"))


# Plot Density Graphs of why reforms failed.
figure13 <- ggplot(filter(all_reforms, !is.na(type_fail)), 
                   aes(x=date_ref_end, fill=type_fail, color=type_fail)) +
  geom_density(alpha=0.3) +
  theme_minimal() +
  labs(x="", y="", title="") +
  scale_x_date(date_breaks = "2 years",
               date_labels = "%Y",
               expand = c(0,0)) +
  scale_fill_manual(name =NULL, values = c("#D69C4E", "#046C9A")) +
  scale_color_manual(name = NULL, values = c("#D69C4E", "#046C9A")) +
  theme(legend.position = "bottom")

figure13

ggsave("figure13.pdf", plot = figure13)






