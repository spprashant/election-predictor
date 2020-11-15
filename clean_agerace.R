library(data.table)
library(dplyr)

setwd("~/Documents/Grad School/election-predictor-main/data")

age08 <- fread("2008_age.csv") %>% .[, "Year" := 2008]
race08 <- fread("2008_race.csv") %>% .[, "Year" := 2008]

age12 <- fread("2012_age.csv") %>% .[, "Year" := 2012]
race12 <- fread("2012_race.csv") %>% .[, "Year" := 2012]

age16 <- fread("2016_age.csv") %>% .[, "Year" := 2016]
race16 <- fread("2016_race.csv") %>% .[, "Year" := 2016]

age18 <- fread("2018_age.csv") %>% .[, "Year" := 2018]
race18 <- fread("2018_race.csv") %>% .[, "Year" := 2018]

race.names <- c("STATE","Sex, Race, and Hispanic-Origin","Total population","Total citizen population",
                "Total registered","Total voted","Year")

age.names <- c("STATE","Age","Total population","Total citizen population","Total registered","Total voted","Year")

race.comb <- merge(race08[, .SD, .SDcols = race.names], race12[, .SD, .SDcols = race.names], by = names(race.names), all = T) %>%
  merge(., race16[, .SD, .SDcols = race.names], by = race.names, all = T) %>%
  merge(., race18[, .SD, .SDcols = race.names], by = race.names, all = T) %>%
  .[startsWith(`Sex, Race, and Hispanic-Origin`,"."), 
    "Sex, Race, and Hispanic-Origin" := substr(`Sex, Race, and Hispanic-Origin`,2,nchar(`Sex, Race, and Hispanic-Origin`))] %>%
  .[startsWith(`Sex, Race, and Hispanic-Origin`,"."), 
    "Sex, Race, and Hispanic-Origin" := substr(`Sex, Race, and Hispanic-Origin`,2,nchar(`Sex, Race, and Hispanic-Origin`))] %>%
  .[STATE == "All", "STATE" := "US"] %>% .[`Sex, Race, and Hispanic-Origin` == "Total", "Sex, Race, and Hispanic-Origin" := "Total - Race/Gender"]

age.comb <- merge(age08[, .SD, .SDcols = age.names], age12[, .SD, .SDcols = age.names], by = names(age.names), all = T) %>%
  merge(., age16[, .SD, .SDcols = age.names], by = age.names, all = T) %>%
  merge(., age18[, .SD, .SDcols = age.names], by = age.names, all = T) %>%
  .[startsWith(Age,"."), "Age" := substr(Age,2,nchar(Age))] %>%
  .[Age == "Total", "Age" := "Total - Age"]

state.names <- fread("state_names.csv")

for (i in 1:51) {
  age.comb <- age.comb[startsWith(STATE,state.names$state_name[i]), "STATE" := state.names$state_abv[i]]
  race.comb <- race.comb[startsWith(STATE,state.names$state_name[i]), "STATE" := state.names$state_abv[i]]
}

names.f <- c("STATE","Metric","Total population","Total citizen population","Total registered","Total voted","Year")

names(age.comb) <- names.f
names(race.comb) <- names.f

comb.data <- merge(age.comb, race.comb, by = names.f, all = T) %>%
  .[, "Total population" := as.numeric(gsub(",","",`Total population`))] %>%
  .[, "Total citizen population" := as.numeric(gsub(",","",`Total citizen population`))] %>%
  .[, "Total registered" := as.numeric(gsub(",","",`Total registered`))] %>%
  .[, "Total voted" := as.numeric(gsub(",","",`Total voted`))]

comb.temp <- melt(comb.data, id.vars = c("STATE","Year","Metric"), variable.name = "populations") %>%
  dcast(., STATE + Year + populations ~ Metric, value.var = "value") %>%
  .[, "Under 45" := rowSums(.SD, na.rm = T), .SDcols = c("18 to 24","18 to 44","25 to 34","25 to 44","35 to 44")] %>%
  .[, "Over 45" := rowSums(.SD, na.rm = T), .SDcols = c("45 to 54","45 to 64","55 to 64","65 to 74","65+","75+")] %>%
  .[populations == "Total registered"] %>%
  .[, "pct_reg_under45" := `Under 45` / `Total - Age`] %>%
  .[, "pct_reg_black" := `Black alone or in combination` / `Total - Race/Gender`] %>%
  .[, "pct_reg_hispanic" := `Hispanic (of any race)` / `Total - Race/Gender`] %>%
  .[, c("STATE","Year","pct_reg_under45","pct_reg_black","pct_reg_hispanic")] %>%
  .[Year == 2018, "Year" := 2020]

names(comb.temp) <- c("state_po","year","pct_reg_under45","pct_reg_black","pct_reg_hispanic")
  
write.csv(comb.temp, "race_age_data.csv", row.names = F, na = "0")
  
  
  
  
  .[, c("STATE","Year","populations","Under 45","Over 45","Black alone or in combination","Hispanic (of any race)")] %>%
  melt(., id.vars = c("STATE","Year","populations"), variable.name = "Metric") %>%
  dcast(., STATE + Year + Metric ~ populations, value.var = "value") %>%
  .[, "Percent registered" := `Total registered` / `Total citizen population`] %>%
  .[, "Percent voted" := `Total voted` / `Total registered`] %>%
  .[, c("STATE","Year","Metric","Percent registered","Percent voted")] %>%
  melt(., id.vars = c("STATE","Year","Metric")) %>%
  .[Metric == "Under 45" & variable == "Percent registered", "Label" := "pct_under45_registered"] %>%
  .[Metric == "Under 45" & variable == "Percent voted", "Label" := "pct_under45_voted"] %>%
  .[Metric == "Black alone or in combination" & variable == "Percent registered", "Label" := "pct_black_registered"] %>%
  .[Metric == "Black alone or in combination" & variable == "Percent voted", "Label" := "pct_black_voted"] %>%
  .[Metric == "Hispanic (of any race)" & variable == "Percent registered", "Label" := "pct_black_registered"] %>%
  .[Metric == "Hispanic (of any race)" & variable == "Percent voted", "Label" := "pct_black_voted"]
  
  
