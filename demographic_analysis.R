library(dplyr)
library(readxl)    
library(writexl)  
library(openxlsx)


#Data input: sero_elisa_all.xlsx = biobank individual-level dataset

#define region
north <- c("臺北市", "新北市", "基隆市", "桃園市", "新竹市", "新竹縣","宜蘭縣")
central <- c("苗栗縣", "臺中市", "彰化縣", "南投縣", "雲林縣")
south <- c("嘉義市", "嘉義縣", "臺南市", "高雄市", "屏東縣")
east <- c("花蓮縣", "臺東縣")

#data cleaning 
sero_elisa_all_clean <- sero_elisa_all %>%
  mutate(elisa_bin = if_else(elisa == "POS", 1, 0),
    SEX = factor(SEX,levels = c(1, 2),labels = c("Male", "Female")),
    EDUCATION_cat = case_when(
      EDUCATION %in% 1:5 ~ "Below college",
      EDUCATION %in% 6:7 ~ "College+",
      TRUE ~ NA_character_),
    EDUCATION_cat = factor(EDUCATION_cat),
    PLACE_REGION = if_else(is.na(PLACE_LGST) | PLACE_LGST == "",PLACE_CURR,PLACE_LGST),
    county = sub("\\[(.*?)\\].*", "\\1", PLACE_REGION),
    region = case_when(
      county %in% north ~ "North",
      county %in% central ~ "Central",
      county %in% south ~ "South",
      county %in% east ~ "East",
      TRUE ~ NA_character_),
    region = factor(region))

#define reference groups
sero_elisa_all_clean$region <- relevel(sero_elisa_all_clean$region, ref = "North")
sero_elisa_all_clean$SEX <- relevel(sero_elisa_all_clean$SEX, ref = "Male")
sero_elisa_all_clean$EDUCATION_cat <- relevel(sero_elisa_all_clean$EDUCATION_cat, ref = "Below college")

#create age groups
sero_elisa_all_clean <- sero_elisa_all_clean %>%
  mutate(age_group = cut(AGE,breaks = c(30, 40, 50, 60, 70, Inf),right = FALSE,labels = c("30-39", "40-49", "50-59", "60-69", "70+")))
sero_elisa_all_clean$age_group <- relevel(sero_elisa_all_clean$age_group, ref = "30-39")


#Table 1: Descrtiptive statistics

total_n <- nrow(sero_elisa_all_clean)
age_group_summary <- sero_elisa_all_clean  %>%
  group_by(age_group) %>%
  summarise(n = n()) %>%
  mutate(perc = round(n / total_n * 100, 1),
         label = paste0(n, " (", perc, "%)"))
sex_summary <- sero_elisa_all_clean  %>%
  group_by(SEX) %>%
  summarise(n = n()) %>%
  mutate(perc = round(n / total_n * 100, 1),
         label = paste0(n, " (", perc, "%)"))
edu_summary <-sero_elisa_all_clean  %>%
  group_by(EDUCATION_cat) %>%
  summarise(n = n()) %>%
  mutate(perc = round(n / total_n * 100, 1),
         label = paste0(n, " (", perc, "%)"))
region_summary <- sero_elisa_all_clean  %>%
  group_by(region) %>%
  summarise(n = n()) %>%
  mutate(perc = round(n / total_n * 100, 1),
         label = paste0(n, " (", perc, "%)"))
elisa_summary <- sero_elisa_all_clean  %>%
  group_by(elisa_bin) %>%
  summarise(n = n()) %>%
  mutate(label = case_when(
    elisa_bin == 1 ~ paste0(n, " (", round(n / total_n * 100, 1), "%)"),
    elisa_bin == 0 ~ paste0(n, " (", round(n / total_n * 100, 1), "%)")))

descriptive_stats <- list(
  age_group = age_group_summary %>% dplyr::select(age_group, label),
  sex = sex_summary %>% dplyr::select(SEX, label),
  education = edu_summary %>% dplyr::select(EDUCATION_cat, label),
  region = region_summary %>% dplyr::select(region, label),
  elisa = elisa_summary %>% dplyr::select(elisa_bin, label))

for (name in names(descriptive_stats)) {
  write.csv(descriptive_stats[[name]], paste0("descriptive_", name, ".csv"), row.names = FALSE)
}


#Table 1: serostatus by demographic characteristics 
age_group_elisa <- sero_elisa_all_clean%>%
  group_by(age_group, elisa_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(age_group) %>%
  mutate(
    perc = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", perc, "%)")) %>%
  dplyr::select(age_group, elisa_bin, label)


sex_elisa <- sero_elisa_all_clean %>%
  group_by(SEX, elisa_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SEX) %>%
  mutate(
    perc = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", perc, "%)")) %>%
  dplyr::select(SEX, elisa_bin, label)


edu_elisa <- sero_elisa_all_clean %>%
  group_by(EDUCATION_cat, elisa_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(EDUCATION_cat) %>%
  mutate(
    perc = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", perc, "%)")) %>%
  dplyr::select(EDUCATION_cat, elisa_bin, label)


region_elisa <- sero_elisa_all_clean %>%
  group_by(region, elisa_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(region) %>%
  mutate(
    perc = round(n / sum(n) * 100, 1),
    label = paste0(n, " (", perc, "%)")) %>%
  dplyr::select(region, elisa_bin, label)


#Table 1: univariable regression
vars_group <- c("age_group", "SEX", "EDUCATION_cat", "region")
run_univ <- function(var) {
  formula <- as.formula(paste("elisa_bin ~", var))
  model <- glm(formula, data = sero_elisa_all_clean, family = binomial)
  broom::tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(variable = var) %>%
    dplyr::select(variable, term, estimate, conf.low, conf.high, p.value)
}
univ_group_results <- bind_rows(lapply(vars_group, run_univ)) %>%
  dplyr::select(variable, term, estimate, conf.low, conf.high, p.value)



#Table 1: multivariable regression
fit_multi_group <- glm(elisa_bin ~ age_group + SEX + EDUCATION_cat + region,
                       data = sero_elisa_all_clean, family = binomial)
multi_group_results <- fit_multi_group %>%
  broom::tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  dplyr::select(term, estimate, conf.low, conf.high, p.value)




