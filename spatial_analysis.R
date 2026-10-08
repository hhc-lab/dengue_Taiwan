#packages
library(raster)
library(INLA)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(maps)
library(sp)
library(sf)
library(openxlsx)
library(scales)
library(forcats)
library(corrplot)
library(car)
library(readxl)
library(binom)
library(writexl)
library(viridis)
library(ggspatial)
rm(list=ls())
conflicts()   

#Input data
#df_real_test <- read.xlsx("biobank_data_individual_district.xlsx") #biobank data including elisa results
#town.shp<-read_sf('TOWN_MOI_1120317.shp') #Taiwan shape file
#pop_data_town = read.xlsx("integrated_town_data.xlsx") #town-level data including population density, mosquito index, proportion away from county, imported cases, towncode, and county-town names


#Define known outbreak years and calculate number of outbreaks exposed
year_outbreak = c(1930:1940, 1981, 1987, 1988, 2001, 2002, 2007, 2014, 2015)

get_n_outbreaks = function(birth, survey, outbreak_years, survey_month) {
  if (survey %in% outbreak_years && survey_month < 7) {
    return(sum(outbreak_years %in% (birth:(survey - 1))))
  } else {
    return(sum(outbreak_years %in% (birth:survey)))
  }
}

df_real_test$n_outbreaks = apply(df_real_test, 1, function(row) {
  get_n_outbreaks(as.numeric(row["BIRTH_YEAR_combined"]),
                  as.numeric(row["SURVEY_YEAR"]),
                  year_outbreak,
                  as.numeric(row["SURVEY_MONTH"]))
})

#remove cities outside main island
remove_list= c("[澎湖縣]七美鄉", "[澎湖縣]湖西鄉", "[澎湖縣]白沙鄉", "[澎湖縣]西嶼鄉", "[澎湖縣]馬公市",
               "[連江縣]北竿鄉", "[連江縣]南竿鄉", "[金門縣]金城鎮", "[金門縣]金湖鎮", "[臺東縣]綠島鄉","[屏東縣]琉球鄉")

df_real_test= df_real_test[!df_real_test$PLACE_COMBINED %in% remove_list,]


#Break down multipolygon into several polygons
town.shp = st_cast(town.shp,"POLYGON")
twn_nb = spdep::poly2nb(town.shp) #List of all neighbouring districts
town.shp = town.shp %>% mutate(has_neighbours = sapply(twn_nb, function(x) sum(x==0)==0))
table(town.shp$has_neighbours) #360 polygons have neighbours
town.shp = town.shp %>% filter(has_neighbours) %>% mutate(unique_loc = paste0("[",COUNTYNAME,"]",TOWNNAME))
town.shp %>% ggplot()+geom_sf()

#remove remaining islands
coords = st_coordinates(st_centroid(town.shp))
below_120_long = coords[,1]<120
town.shp = town.shp[!below_120_long,]

#Reunite polygons into multipolygons
town.shp = lapply(unique(town.shp$unique_loc), function(town){
  multipol = town.shp %>% filter(unique_loc==town)
  vect = st_drop_geometry(multipol)
  multipol = st_union(multipol)
  vect = vect[1,] %>% mutate(geometry=multipol) %>% st_as_sf(sf_column_name = "geometry")
}) %>% do.call(what=rbind)
town.shp %>% ggplot()+geom_sf()

#Supplementary figure 1 :regional classification of Taiwan
#Define four geographic regions
North <- c("臺北市", "新北市", "基隆市", "桃園市","新竹市", "新竹縣", "宜蘭縣")
Central <- c("苗栗縣", "臺中市", "彰化縣", "南投縣", "雲林縣")
South <- c("嘉義市", "嘉義縣", "臺南市", "高雄市", "屏東縣")
East <- c("花蓮縣", "臺東縣")
county_eng <- c(
  "臺北市" = "Taipei City","新北市" = "New Taipei City","基隆市" = "Keelung City","桃園市" = "Taoyuan City","新竹市" = "Hsinchu City","新竹縣" = "Hsinchu County","宜蘭縣" = "Yilan County",
  "苗栗縣" = "Miaoli County","臺中市" = "Taichung City","彰化縣" = "Changhua County","南投縣" = "Nantou County","雲林縣" = "Yunlin County",
  "嘉義市" = "Chiayi City","嘉義縣" = "Chiayi County","臺南市" = "Tainan City","高雄市" = "Kaohsiung City","屏東縣" = "Pingtung County",
  "花蓮縣" = "Hualien County","臺東縣" = "Taitung County")

town.shp$COUNTYENG <- county_eng[town.shp$COUNTYNAME]
town.shp$region <- dplyr::case_when(
  town.shp$COUNTYNAME %in% North ~ "North",
  town.shp$COUNTYNAME %in% Central ~ "Central",
  town.shp$COUNTYNAME %in% South ~ "South",
  town.shp$COUNTYNAME %in% East ~ "East",
  TRUE ~ NA_character_)

county.shp <- town.shp %>%group_by(COUNTYNAME, COUNTYENG, region) %>%
  summarise(geometry = st_union(geometry),.groups = "drop") %>%
  st_make_valid() %>%
  st_cast("POLYGON") %>%
  group_by(COUNTYNAME, COUNTYENG, region) %>%
  slice_max(st_area(geometry), n = 1, with_ties = FALSE) %>%
  ungroup()

county_proj <- st_transform(county.shp, 3826)
county_proj$region <- factor(county_proj$region,levels = c("North", "Central", "South", "East"))
county_labels <- st_point_on_surface(county_proj)

supp_fig_one<-ggplot() +
  geom_sf(data = county_proj,aes(fill = region),color = "white", linewidth = 0.45) +
  geom_sf_text(data = county_labels,aes(label = COUNTYENG),size = 3.0) +
  scale_fill_manual(values = c("North"   = "#6BAED6","Central" = "#A6A6A6","South"   = "#E07A7A","East"    = "#9E8AC3")) +
  labs(fill = "Region") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.key.height = unit(0.6, "cm"),
    plot.margin = margin(5, 5, 5, 5))+  
  ggspatial::annotation_scale(
    bar_cols = c("gray50", "gray80"),
    line_width = 0.1,
    text_cex = 1)


#Seroprevalence 
Hmisc::binconf(x = 261, n = 6385)

#Figure 1
df_by_town = df_real_test %>% group_by(city) %>% summarise(seroprevalence=mean(seropos), npos = sum(seropos), ntot = n()) 
town_plot <- town.shp %>%
  left_join(df_by_town, by = join_by(unique_loc == city)) %>%
  mutate(
    seroprevalence = ifelse(ntot < 5, NA, seroprevalence))
df_by_town %>%filter(ntot < 5) %>%arrange(ntot)

fig_one <- town_plot %>%ggplot(aes(fill = seroprevalence)) +
  geom_sf() + ggthemes::theme_map() +
  scale_fill_gradientn(colours = c("gray90","wheat1","peachpuff","orange","darkorange2","darkorange4"),transform = "sqrt") +
  labs(fill = "Seroprevalence") +
  theme(legend.position = c(1.05, 0.2),legend.justification = c(0, 0.5)) +
  ggspatial::annotation_scale(
    bar_cols = c("gray50", "gray80"),
    line_width = 0.1,
    text_cex = 1)


#Supplementary figure 2: seropositivity by age
df_real_test %>%group_by(age_cat) %>%
  summarise(n_people = n(), mean_seropos = mean(seropos),seropos_sum = sum(seropos), .groups = "drop")

supp_fig_two<-df_real_test %>%
  mutate(
    age_cat_plot = as.character(age_cat),
    age_cat_plot = ifelse(age_cat_plot %in% c("(75,80]", "(80,85]"), "(75,85]", age_cat_plot)) %>%
  group_by(age_cat_plot) %>%
  summarise(
    mean_seropos = mean(seropos),
    mean_age = mean(age),
    lo = Hmisc::binconf(n = n(), x = sum(seropos))[,2],
    hi = Hmisc::binconf(n = n(), x = sum(seropos))[,3],.groups = "drop") %>%
  ggplot(aes(x = mean_age, y = mean_seropos, ymin = lo, ymax = hi)) +
  geom_pointrange() +xlab("Age") + ylab("Seroprevalence") +
  theme_minimal()

#Merge township level data
town.shp = town.shp %>%mutate(TOWNCODE = as.numeric(TOWNCODE)) %>%left_join(pop_data_town_2, by="TOWNCODE")

#Running the INLA model----------------------------------------------------
df_real_test= df_real_test %>%
  left_join(town.shp%>%st_drop_geometry()%>%dplyr::select(TOWNCODE,AIAeg,AIAlb,ppl_density,COUNTYNAME,away_from_county,imported))

#Supplementary figure 3 and supplementary table s1: correlations
cor_matrix<-cor(df_real_test[, c("AIAeg","AIAlb","ppl_density","away_from_county")],use = "complete.obs", method = "spearman")
colnames(cor_matrix) <- rownames(cor_matrix) <- c("Aedes aegypti", "Aedes albopictus", "Population density", "Proportion away from county")
corrplot(cor_matrix, method = "color", type = "upper",addCoef.col = "black", tl.col = "black", tl.srt = 45,title = "")
lm_test <- lm(seropos ~ AIAeg + AIAlb + ppl_density +away_from_county, data=df_real_test)
car::vif(lm_test) 

#Create adjacency matrix frome shape file (Taiwan)------------------
twn_nb = spdep::poly2nb(town.shp) #List of all neighbouring districts
spdep::nb2INLA("adj_mat.graph",twn_nb) #Directly cerates the file
adj_mat = inla.read.graph("adj_mat.graph") #Load it into the environment
image(inla.graph2matrix(adj_mat), xlab = "", ylab = "") #Plot adjacency matrix

df_real_test$district = sapply(df_real_test$city, function(city){
  dist = which(town.shp$unique_loc==city)
  ifelse(length(dist)>0,dist,NA)
})

formula = seroposenr ~ -1 + Intercept + offset(log(n_outbreaks)) + AIAeg + AIAlb + ppl_density + away_from_county+f(district, model="bym2", graph=adj_mat)

stack.est = inla.stack(data=list(seroposenr=df_real_test$seropos),
                       A=list(1),
                       effects=list(c(Intercept=1, df_real_test[,which(names(df_real_test)!= "seropos")])), 
                       tag='stdata')

output = inla(formula,
              data =inla.stack.data(stack.est),
              family = "binomial",
              control.family = list(link="cloglog"),
              control.predictor=list(A=inla.stack.A(stack.est), compute = F),
              control.compute = list(dic = TRUE,waic = TRUE,cpo = TRUE),
              verbose=T)

#Model outputs----------------------------------------
summary(output)

#Get coeffs on estimates

summary_output_stats <- output$summary.fixed %>%
  mutate(
    covariate = c("Intercept","Aedes aegypti","Aedes albopictus","Population density (per 10,000 people)","Proportion away from county"),
    multiplier = c(1, 1, 1, 10000, 1),
    OR = exp(mean * multiplier),
    OR_lower = exp(`0.025quant` * multiplier),
    OR_upper = exp(`0.975quant` * multiplier)) %>%
  filter(covariate != "Intercept") %>%
  dplyr::select(covariate, OR, OR_lower, OR_upper) #population density unit to per 10,000 people 

#Retrieve predicted FOI at mesh nodes
spat_field = output$summary.random$district[1:349,]
town.shp$mean_field = spat_field$mean

town.shp = town.shp %>% mutate(foi = exp(output$summary.fixed$mean[1] + 
                                           AIAeg * output$summary.fixed$mean[2] + 
                                           output$summary.fixed$mean[3] * AIAlb  + 
                                           output$summary.fixed$mean[4] * ppl_density + 
                                           away_from_county * output$summary.fixed$mean[5] + 
                                           mean_field)) 

#Figure 2: foi by town
county_border <- town.shp %>%
  group_by(County_eng) %>%
  summarise(geometry = st_union(geometry),.groups = "drop") %>%
  st_make_valid() %>%
  st_cast("POLYGON") %>%
  group_by(County_eng) %>%
  slice_max(st_area(geometry), n = 1, with_ties = FALSE) %>%ungroup()
fig <- town.shp %>%ggplot(aes(fill = foi)) +
  geom_sf() +
  geom_sf(data = county_border,fill = NA,color = "gray25",linewidth = 0.5) +
  ggthemes::theme_map() +
  scale_fill_gradient2(name = "FOI",low = "darkgreen",mid = "gray80",high = "darkred",midpoint = 0.003089975,transform = "sqrt") +
  theme(legend.position = c(0.85, 0.15)) +
  ggspatial::annotation_scale(
    bar_cols = c("gray50", "gray80"),
    line_width = 0.1,
    text_cex = 1)

#township-level estimated foi table
estimated_foi_table<-town.shp %>% 
  st_drop_geometry() %>%
  dplyr::select(TOWNCODE, COUNTYNAME, unique_loc, Town_eng,TOWNENG,County_eng,
                AIAeg, AIAlb, ppl_density, away_from_county,  mean_field, foi)                                       



#Reconstruct individual seroprevalence-----
#Get the operator that projects field values from the mesh nodes onto the individual locations from the original dataset and then do the projection
df_real_test$foi = exp(spat_field$mean[df_real_test$district] + output$summary.fixed$mean[1] +
                         output$summary.fixed$mean[2]*df_real_test$AIAeg +
                         output$summary.fixed$mean[3]*df_real_test$AIAlb +
                         output$summary.fixed$mean[4]*df_real_test$ppl_density +
                         output$summary.fixed$mean[5]*df_real_test$away_from_county)

df_real_test$prob_seropositive = 1-exp(-df_real_test$foi*df_real_test$n_outbreaks)

#Supplmentary figure 5: Visualise observed vs predicted-----------
size_check<-df_real_test %>% 
  mutate(pred_cat = cut(prob_seropositive, seq(0, 1, by=0.01))) %>%
  group_by(pred_cat) %>%summarise(n = n()) %>%arrange(desc(n))  
size_check %>% pull(n) %>% summary()

fig <- df_real_test %>% 
  mutate(pred_cat = cut(prob_seropositive, seq(0, 1, by=0.01))) %>%
  group_by(pred_cat) %>%
  summarise(Observed = mean(seropos),
            Predicted = mean(prob_seropositive),
            n = n()) %>%
  filter(!is.na(Observed) & !is.na(Predicted)) %>%
  ggplot(aes(x = Predicted, y = Observed, size = n)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  expand_limits(x = 0, y = 0) + 
  scale_size_continuous(
    name = "Sample size",
    breaks = c(10, 50, 100, 500, 1000, 1500)) +
  theme_minimal()



#Prob of seropositivity and number infected----------
get_n_outbreaks = function(birth, survey, year_outbreak){
  return(sum(year_outbreak %in% (birth:survey)))
}

year_outbreak= c(1930:1940, 1981, 1987, 1988, 2001, 2002, 2007, 2014, 2015)

# proportion of people getting infected
n_outbreaks_age= sapply((2021:(2021-100)), function(x) get_n_outbreaks(x, 2021,year_outbreak )) #use age range 0:100, the situation in 2021
age_data_town = read.xlsx("鄉鎮市區人數按年齡_age_group.xlsx") #year 109 pop data
age_data_town<-age_data_town %>% 
  rename(TOWNCODE=towncode,age=age_group, population=pop)
summary(town.shp$TOWNCODE %in% age_data_town$TOWNCODE)#349
correct_order <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                   "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                   "75-79","80-84","85-89","90-94","95-99","100+")
age_data_town$age <- factor(age_data_town$age, levels = correct_order)
age_data_town <- age_data_town %>% arrange(TOWNCODE, age)
list_town <- unique(age_data_town$TOWNCODE)
common_towns <- intersect(list_town, town.shp$TOWNCODE)
age_data_town <- age_data_town %>%filter(TOWNCODE %in% common_towns)
summary(town.shp$TOWNCODE %in% age_data_town$TOWNCODE)#349
list_town = unique(age_data_town$TOWNCODE)
list_age = age_data_town$age[age_data_town$TOWNCODE == list_town[1]]
age_data_town$prop = NA
for (i in 1:length(list_town)){
  temp = age_data_town[age_data_town$TOWNCODE == list_town[i],]
  age_data_town$prop[age_data_town$TOWNCODE == list_town[i]] = temp$population/sum(temp$population)
}

#build prop_age matrix 
prop_age = matrix(NA, nrow=101, ncol=length(list_town))
list_age = age_data_town$age[age_data_town$TOWNCODE == list_town[1]]
for (i in 1:length(list_town)){
  for (j in 1:length(list_age)){
    if (j<21) prop_age[((j-1)*5+1):(j*5),i] = age_data_town$prop[age_data_town$TOWNCODE==list_town[i] & age_data_town$age==list_age[j]]/5
    else prop_age[((j-1)*5+1),i] = age_data_town$prop[age_data_town$TOWNCODE==list_town[i] & age_data_town$age==list_age[j]]
  }
}

town.shp$prob_seropositive = NA
for (i in 1:nrow(town.shp)){
  town.shp$prob_seropositive[i] = weighted.mean((1-exp(-town.shp$foi[i]*n_outbreaks_age)),prop_age[, which(list_town == town.shp$TOWNCODE[i])])
}


town.shp$number_infected = town.shp$prob_seropositive*town.shp$ppl_num

#estmated number of infected and prob seropostive table
town_prob_serop_number_infected <- town.shp %>%
  st_drop_geometry() %>%
  dplyr::select(COUNTYNAME, TOWNCODE,County_eng,Town_eng, TOWNNAME,ppl_num, foi, prob_seropositive, number_infected)

#Figure 3: Plot number infected and prob seropositve
region_order <- c(
  "Keelung City", "Yilan County","Taipei City", "New Taipei City", "Taoyuan City",
  "Hsinchu City", "Hsinchu County", 
  "Miaoli County", "Taichung City", "Changhua County",
  "Nantou County", "Yunlin County",
  "Chiayi City", "Chiayi County", "Tainan City",
  "Kaohsiung City", "Pingtung County",
  "Hualien County", "Taitung County")

region_df <- data.frame(County_eng = region_order,region = c(rep("North", 7),rep("Central", 5),rep("South", 5),rep("East", 2)))
region_levels <- c("North", "Central", "South", "East")
region_cols <- c("North"= "#4C78A8","Central" = "#7A7A7A","South"= "#C44E52","East"= "#8172B2")

plot_data <- town.shp %>%
  st_drop_geometry() %>%
  dplyr::select(County_eng,TOWNCODE,prob_seropositive,number_infected) %>%
  tidyr::pivot_longer(cols = c(prob_seropositive, number_infected),names_to = "variable",values_to = "value") %>%
  mutate(variable = case_when(
    variable == "prob_seropositive" ~ "Proportion seropositive",
    variable == "number_infected" ~ "Number of seropositives",
    TRUE ~ variable)) %>%
  left_join(region_df, by = "County_eng") %>%
  mutate(County_eng = factor(County_eng, levels = region_order),
         region = factor(region, levels = region_levels))

p1 <- plot_data %>%
  filter(variable == "Number of seropositives") %>%
  ggplot(aes(x = County_eng, y = value)) +
  geom_boxplot(fill = "lightgray",outlier.shape = NA) +
  geom_jitter(width = 0.15,size = 1.5,alpha = 0.5,aes(color = region)) +
  scale_color_manual(values = region_cols) +
  labs(x = "County",y = "Number of seropositives") +
  theme_minimal(base_size = 22) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1,size = 14,color = "black"),
        axis.text.y = element_text(size = 14,color = "black"),
        axis.title = element_text(size = 18,color = "black"),
        legend.position = "none")

p2 <- plot_data %>%
  filter(variable == "Proportion seropositive") %>%
  ggplot(aes(x = County_eng, y = value)) +
  geom_boxplot(fill = "lightgray",outlier.shape = NA) +
  geom_jitter(width = 0.15,size = 1.5,alpha = 0.5,aes(color = region)) +
  scale_color_manual( values = region_cols,breaks = region_levels,name = "County region") +
  labs(x = "County",y = "Proportion seropositive") +
  theme_minimal(base_size = 22) +
  theme(axis.text.x = element_text(angle = 45,hjust = 1,size = 14,color = "black"),
        axis.text.y = element_text(size = 14,color = "black"),
        axis.title = element_text(size = 18,color = "black"),
        legend.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 14, color = "black"),
        legend.position = "right")

fig_three <- cowplot::plot_grid(p1, p2,ncol = 2,labels = c("A", "B"),label_size = 18)


#Regional summary (North vs South) 
north_counties <- c("臺北市", "新北市", "基隆市", "桃園市", "新竹市", "新竹縣","宜蘭縣")
south_counties <- c("嘉義縣", "嘉義市", "臺南市", "高雄市", "屏東縣")

town.shp <- town.shp %>%
  mutate(region = case_when(
    COUNTYNAME %in% north_counties ~ "North",
    COUNTYNAME %in% south_counties ~ "South",
    TRUE ~ "Other"))

region_summary <- town.shp %>%
  st_drop_geometry() %>%
  filter(region %in% c("North","South")) %>%
  group_by(region) %>%
  summarise(
    weighted_seroprev = sum(prob_seropositive * ppl_num, na.rm=TRUE) / sum(ppl_num, na.rm=TRUE),
    total_infected = sum(number_infected, na.rm=TRUE),
    total_population = sum(ppl_num, na.rm=TRUE)) #north vs south



#reporting rates ----------------------------

all(list_town == colnames(prop_age))
get_n_outbreaks_pre <- function(birth, survey, year_outbreak){
  sum(year_outbreak %in% (birth:(survey-1)))
}
report_year <- c(2001, 2002, 2007, 2014, 2015)#outbreak years after 1998
results_list <- list()
for (k in seq_along(report_year)) {
  yr <- report_year[k]
  n_outbreaks_age_pre <- sapply((yr:(yr - 100)), function(x)
    get_n_outbreaks_pre(x, yr, year_outbreak))
  town.shp$expected_i_1 <- NA
  town.shp$expected_i_2 <- NA
  for (i in 1:nrow(town.shp)) {
    town.shp$expected_i_1[i] = weighted.mean(exp(-n_outbreaks_age_pre*town.shp$foi[i])*(town.shp$foi[i]), prop_age[, which (list_town==town.shp$TOWNCODE[i])])*town.shp$ppl_num[i]  
    town.shp$expected_i_2[i] = weighted.mean(exp(-(n_outbreaks_age_pre-1)*town.shp$foi[i])*(1-exp(-town.shp$foi[i]))*(town.shp$foi[i]), prop_age[, which (list_town==town.shp$TOWNCODE[i])])*town.shp$ppl_num[i]
  }
  tmp <- town.shp %>%
    mutate(TOWNCODE =TOWNCODE, year = yr,expected_cases = expected_i_1 + expected_i_2, expected_primary = expected_i_1,
           expected_secondary = expected_i_2) %>%
    dplyr::select(TOWNCODE,TOWNNAME,TOWNENG,COUNTYNAME,year,expected_cases,County_eng, expected_primary,
                  expected_secondary)
  results_list[[k]] <- tmp
}
expected_all <- bind_rows(results_list)


#town reporting rate
valid_counties <- unique(town.shp$COUNTYNAME)
town_cases <- read_xlsx("Dengue_cases_town_city_by_year_without_imported.xlsx") %>% 
  dplyr::select(-County_Township,-Township) %>% 
  tidyr::pivot_longer(cols = -c( County,County_mandarin,Township_mandarin),names_to = "year",values_to = "reported_cases") %>%
  mutate(year = as.numeric(year)) %>%
  filter(County_mandarin %in% valid_counties,County != "None",year %in% report_year)
#town reporting rate (town-year)
compare_town <- expected_all %>%
  left_join(town_cases,by = c("TOWNNAME"="Township_mandarin","COUNTYNAME"="County_mandarin","year")) %>%
  mutate(reporting_rate = reported_cases / expected_cases)
compare_town_df <- sf::st_drop_geometry(compare_town)


#county reporting rate by county-year
county_compare<- compare_town_df %>%
  group_by(County_eng, year) %>%
  summarise(
    reported_cases = sum(reported_cases, na.rm = TRUE),
    expected_cases  = sum(expected_cases, na.rm = TRUE),
    reporting_rate  = reported_cases / expected_cases,
    expected_primary = sum(expected_primary, na.rm = TRUE),
    expected_secondary = sum(expected_secondary, na.rm = TRUE),
    secondary_prop = expected_secondary / expected_cases,
    .groups = "drop")
sum(county_compare$expected_cases[county_compare$County_eng=="Kaohsiung City"&county_compare$year=="2014"])


#overall reporting rate for each county
county_overall <- compare_town %>%
  st_drop_geometry() %>%
  group_by(County_eng) %>%
  summarise(
    reported_cases = sum(reported_cases, na.rm = TRUE),
    expected_cases = sum(expected_cases, na.rm = TRUE),
    overall_rate = reported_cases / expected_cases,.groups = "drop")
 
county_overall %>%
  mutate(region = case_when(
    County_eng %in% c("Taipei City", "New Taipei City", "Keelung City","Taoyuan City", "Hsinchu City", "Hsinchu County","Yilan County") ~ "North",
    County_eng %in% c( "Miaoli County","Taichung City","Changhua County","Nantou County","Yunlin County") ~ "Central",
    County_eng %in% c("Chiayi City","Chiayi County","Tainan City","Kaohsiung City","Pingtung County") ~ "South",
    County_eng %in% c( "Hualien County","Taitung County") ~ "East")) %>%
  group_by(region) %>%
  summarise(
    min_reporting_rate = min(overall_rate * 100, na.rm = TRUE),
    max_reporting_rate = max(overall_rate * 100, na.rm = TRUE),
    .groups = "drop")

#Figure 5A: Plot county-level overall reporting rate
county_map <- town.shp %>%
  group_by(County_eng) %>%summarise(geometry = sf::st_union(geometry))

county_map_plot <- county_map %>%left_join(county_overall, by = "County_eng")
county_map_plot %>%
  st_drop_geometry() %>%
  dplyr::select(County_eng, overall_rate) %>%
  arrange(desc(overall_rate))
summary(county_map_plot$overall_rate)

county_map_plot <- county_map_plot %>%
  mutate(overall_pct = overall_rate * 100,
         rate_cat = cut(overall_pct,
                        breaks = c(0, 0.2, 0.5, 1, 5, 50),
                        labels = c("≤0.2%","0.2–0.5%","0.5–1%","1–5%",">5%"),include.lowest = TRUE))

p_county<-ggplot(county_map_plot) +
  geom_sf(aes(fill = rate_cat),color = "white",linewidth = 0.3) +
  scale_fill_brewer(palette = "Blues",drop = FALSE) +
  labs(fill = "Overall reporting rate (%)") +
  ggthemes::theme_map() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"))+
  ggspatial::annotation_scale(bar_cols = c("gray50", "gray80"),line_width = 0.1,text_cex = 1)


#Figure 5B: Plot town-level overall reporting rate
town_summary2 <- compare_town_df %>%
  group_by(TOWNNAME, COUNTYNAME) %>%
  summarise(
    has_data = any(!is.na(reported_cases)),
    total_reported = sum(reported_cases, na.rm = TRUE),
    total_expected = sum(expected_cases, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(rho_weighted = case_when(
    !has_data ~ NA_real_,total_expected == 0 ~ NA_real_,TRUE ~ total_reported / total_expected))


town_map <- town.shp %>%left_join(town_summary2,by = c("TOWNNAME", "COUNTYNAME"))
setdiff(town.shp$TOWNNAME, town_summary2$TOWNNAME)
town_map <- town_map %>%mutate(
  rho_class = case_when(
    is.na(rho_weighted) ~ "NA",
    rho_weighted == 0 ~ "0",
    rho_weighted < 0.005 ~ "<0.5%",
    rho_weighted < 0.01 ~ "0.5–1%",
    rho_weighted < 0.02 ~ "1–2%",
    rho_weighted < 0.05 ~ "2–5%",
    rho_weighted < 0.1 ~ "5–10%",
    rho_weighted < 0.5 ~ "10–50%",TRUE ~ "50–100%"))
town_map <- town_map %>%
  mutate(rho_class = factor(rho_class,
                            levels = c( "0", "<0.5%","0.5–1%","1–2%", "2–5%", "5–10%", "10–50%", "50–100%","NA")))
table(town_map$rho_class)
county_sf <- town.shp %>%group_by(COUNTYNAME) %>%summarise(geometry = st_union(geometry))
p_town<-ggplot(town_map) +
  geom_sf(aes(fill = rho_class), color = "white",linewidth = 0) +
  geom_sf(data = county_sf, fill = NA, color = "grey30", linewidth = 0.1) +
  scale_fill_manual(
    values = c(
      "0" = "#f7fbff",
      "<0.5%" = "#deebf7",
      "0.5–1%" = "#c6dbef",
      "1–2%" = "#9ecae1",
      "2–5%" = "#6baed6",
      "5–10%" = "#2171b5",
      "10–50%" = "#08306b",
      "50–100%" = "#4d0000",
      "NA" = "grey80"),drop = FALSE) +
  ggthemes::theme_map() +
  labs(fill = "Overall reporting rate (%)")+
  theme(
    legend.position = "right",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(0.4, "cm"))+
  ggspatial::annotation_scale(bar_cols = c("gray50", "gray80"),line_width = 0.1,text_cex = 1)


fig_five_A_B <- (p_county | p_town) +
  patchwork::plot_annotation(tag_levels = list(c("A", "B")),theme = theme(plot.tag = element_text(face = "bold", size = 14)))


# Supplementary figure 6: Distribution of overall township-level reporting rates in Taiwan
supp_fig_six<-ggplot(town_summary2, aes(x = rho_weighted)) +geom_histogram(bins = 40, color = "white") +theme_minimal() +labs(x = "Overall reporting rate (per township)",y = "Number of townships") 

#fold difference in overall reporting within county
town_summary2 %>% filter(rho_weighted > 0) %>%
  group_by(COUNTYNAME) %>%
  summarise(min_rate = min(rho_weighted),max_rate = max(rho_weighted),fold_diff = max_rate / min_rate) 



#Estimated infections by outbreak and county-----

region_order <- c("Keelung City","Yilan County","Taipei City","New Taipei City","Taoyuan City","Hsinchu City","Hsinchu County",
                  "Miaoli County","Taichung City","Changhua County","Nantou County","Yunlin County",
                  "Chiayi City","Chiayi County","Tainan City","Kaohsiung City","Pingtung County",
                  "Hualien County","Taitung County")

region_df <- data.frame(County_eng = region_order,region = c(rep("North", 7),rep("Central", 5),rep("South", 5),rep("East", 2)))
region_levels <- c("North","Central","South","East")
region_cols <- c("North"   = "#4C78A8","Central" = "#7A7A7A","South"   = "#C44E52","East"    = "#8172B2")


county_average <- county_compare %>%
  group_by(County_eng) %>%
  summarise(
    mean_estimated_infections_per_outbreak =mean(expected_cases, na.rm = TRUE),
    mean_estimated_secondary_infections_per_outbreak = mean(expected_secondary, na.rm = TRUE),
    mean_reported_cases_per_outbreak =mean(reported_cases, na.rm = TRUE),
    .groups = "drop")

# Secondary infections by county and outbreak 
secondary_by_county_year <- compare_town_df %>%
  group_by(County_eng, year) %>%
  summarise(
    expected_secondary =sum(expected_secondary, na.rm = TRUE),
    expected_cases =sum(expected_cases, na.rm = TRUE),
    secondary_prop =expected_secondary / expected_cases,
    .groups = "drop")

#Average secondary infection per outbreak by county
secondary_county <- secondary_by_county_year %>%
  group_by(County_eng) %>%
  summarise(
    mean_secondary_per_outbreak =mean(expected_secondary, na.rm = TRUE),
    mean_secondary_prop =mean(secondary_prop, na.rm = TRUE),
    .groups = "drop")


# estimated expected severe secondary
# Published estimate: 7.8% of secondary dengue infections were severe
p_severe_secondary <- 0.078
secondary_county <- secondary_county %>%
  mutate(
    mean_severe_secondary_per_outbreak =mean_secondary_per_outbreak *p_severe_secondary,
    severe_secondary_prop =mean_secondary_prop*p_severe_secondary)
# Secondary infections
secondary_range <- secondary_county %>%
  summarise(
    min_secondary =min(mean_secondary_per_outbreak, na.rm = TRUE),
    max_secondary =max(mean_secondary_per_outbreak, na.rm = TRUE))
# Severe secondary infections per outbreak
severe_secondary_range <- secondary_county %>%
  summarise(
    min_severe_secondary =min(mean_severe_secondary_per_outbreak, na.rm = TRUE),
    max_severe_secondary =max(mean_severe_secondary_per_outbreak, na.rm = TRUE))

#Expected primary infections
primary_by_county_year <- compare_town_df %>%
  group_by(County_eng, year) %>%
  summarise(
    expected_primary = sum(expected_primary, na.rm = TRUE),
    expected_cases = sum(expected_cases, na.rm = TRUE),
    primary_prop = expected_primary / expected_cases,
    .groups = "drop")

#Mean primary infections per outbreak by county
primary_county <- primary_by_county_year %>%
  group_by(County_eng) %>%
  summarise(
    mean_primary_per_outbreak = mean(expected_primary, na.rm = TRUE),
    mean_primary_prop = mean(primary_prop, na.rm = TRUE),
    .groups = "drop")


#Estimated severe primary infection
p_severe_primary <- 0.038
primary_county <- primary_county %>%
  mutate(
    mean_severe_primary_per_outbreak =mean_primary_per_outbreak * p_severe_primary,
    severe_primary_prop =mean_primary_prop * p_severe_primary)

#primaryinfections per outbreak 
primary_range <- primary_county %>%
  summarise(
    min_primary = min(mean_primary_per_outbreak, na.rm = TRUE),
    max_primary = max(mean_primary_per_outbreak, na.rm = TRUE))

#severe primary infections per outbreak
severe_primary_range <- primary_county %>%
  summarise(
    min_severe_primary =min(mean_severe_primary_per_outbreak, na.rm = TRUE),
    max_severe_primary =max(mean_severe_primary_per_outbreak, na.rm = TRUE))

#Figure 4: mean number of expected infections
county_average_plot <- county_average %>%
  left_join(region_df, by = "County_eng") %>%
  mutate(County_eng = factor(County_eng,levels = region_order),region = factor(region,levels = region_levels)) %>%
  arrange(County_eng)

secondary_county_plot <- secondary_county %>%
  left_join(region_df, by = "County_eng") %>%
  mutate(County_eng = factor(County_eng,levels = region_order),region = factor(region,levels = region_levels)) %>%
  arrange(County_eng)

p_total <- ggplot(
  county_average_plot,
  aes(x = County_eng,y = mean_estimated_infections_per_outbreak,fill = region)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = region_cols,breaks = region_levels,name = "Region") +
  ggbreak::scale_y_break(c(7000, 10000),scales = 0.5) +
  labs(x = "County",y = "Mean expected infections per outbreak") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),legend.position = "none")

p_secondary <- ggplot(secondary_county_plot,
                      aes(x = County_eng,y = mean_secondary_per_outbreak,fill = region)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = region_cols,breaks = region_levels,name = "Region") +
  ggbreak::scale_y_break(c(100, 700),scales = 0.5) +
  labs(x = "County", y = "Mean expected secondary infections per outbreak") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),legend.position = "right")

figure_four <- (p_total | p_secondary) +
  patchwork::plot_annotation(tag_levels = "A",theme = theme(plot.tag = element_text(face = "bold",size = 14)))



#Exploratory analysis-------

cor_matrix<-cor(df_real_test[, c("AIAeg","AIAlb","ppl_density","away_from_county","imported")],
                use = "complete.obs", method = "spearman")
colnames(cor_matrix) <- rownames(cor_matrix) <- c("Aedes aegypti", "Aedes albopictus", "Population density", "Proportion away from county","imported")
corrplot(cor_matrix, method = "color", type = "upper",addCoef.col = "black", tl.col = "black", tl.srt = 45,title = "")


formula = seroposenr ~ -1 + Intercept + offset(log(n_outbreaks)) + 
  AIAeg + AIAlb + ppl_density + away_from_county+ imported+
  f(district, model="bym2", graph=adj_mat)

stack.est = inla.stack(data=list(seroposenr=df_real_test$seropos),
                       A=list(1),
                       effects=list(c(Intercept=1, df_real_test[,which(names(df_real_test)!= "seropos")])), 
                       tag='stdata')

output = inla(formula,
              data =inla.stack.data(stack.est),
              family = "binomial",
              control.family = list(link="cloglog"),
              control.predictor=list(A=inla.stack.A(stack.est), compute = F),
              control.compute = list(dic = TRUE,waic = TRUE,cpo = TRUE),
              verbose=T)

#Model outputs
summary(output)

#Plotting the spatial field
#Retrieve predicted FOI at mesh nodes
spat_field = output$summary.random$district[1:349,]
town.shp$mean_field = spat_field$mean

town.shp = town.shp %>% mutate(foi = exp(output$summary.fixed$mean[1] + 
                                           AIAeg * output$summary.fixed$mean[2] + 
                                           output$summary.fixed$mean[3] * AIAlb  + 
                                           output$summary.fixed$mean[4] * ppl_density + 
                                           away_from_county * output$summary.fixed$mean[5] + 
                                           imported * output$summary.fixed$mean[6]+
                                           mean_field)) 

#Reconstruct individual seroprevalence
#Get the operator that projects field values from the mesh nodes onto the individual locations from the original dataset and then do the projection
df_real_test$foi = exp(spat_field$mean[df_real_test$district] + output$summary.fixed$mean[1] +
                         output$summary.fixed$mean[2]*df_real_test$AIAeg +
                         output$summary.fixed$mean[3]*df_real_test$AIAlb +
                         output$summary.fixed$mean[4]*df_real_test$ppl_density +
                         output$summary.fixed$mean[5]*df_real_test$away_from_county+
                         output$summary.fixed$mean[6]*df_real_test$imported)

df_real_test$prob_seropositive = 1-exp(-df_real_test$foi*df_real_test$n_outbreaks)

#Supplementary figure 4
size_check<-df_real_test %>% 
  mutate(pred_cat = cut(prob_seropositive, seq(0, 1, by=0.01))) %>%
  group_by(pred_cat) %>%summarise(n = n()) %>%arrange(desc(n))  
size_check %>% pull(n) %>% summary()

supp_fig_four <- df_real_test %>% 
  mutate(pred_cat = cut(prob_seropositive, seq(0, 1, by=0.01))) %>%
  group_by(pred_cat) %>%
  summarise(Observed = mean(seropos),
            Predicted = mean(prob_seropositive),
            n = n()) %>%
  filter(!is.na(Observed) & !is.na(Predicted)) %>%
  ggplot(aes(x = Predicted, y = Observed, size = n)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  expand_limits(x = 0, y = 0) + 
  scale_size_continuous(
    name = "Sample size",
    breaks = c(10, 50, 100, 500, 1000, 1500)) +
  theme_minimal()



#Interaction test (AIAeg:imported)
formula = seroposenr ~ -1 + Intercept + offset(log(n_outbreaks)) + 
  AIAeg + AIAlb + ppl_density + away_from_county+ imported+ AIAeg:imported+
  f(district, model="bym2", graph=adj_mat)

stack.est = inla.stack(data=list(seroposenr=df_real_test$seropos),
                       A=list(1),
                       effects=list(c(Intercept=1, df_real_test[,which(names(df_real_test)!= "seropos")])), 
                       tag='stdata')
output = inla(formula,
              data =inla.stack.data(stack.est),
              family = "binomial",
              control.family = list(link="cloglog"),
              control.predictor=list(A=inla.stack.A(stack.est), compute = F),
              control.compute = list(dic = TRUE,waic = TRUE,cpo = TRUE),
              verbose=T)
#Model outputs
summary(output)
