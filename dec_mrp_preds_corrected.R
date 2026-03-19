all_objects <- ls(envir = .GlobalEnv)
if (exists("pred_master")) {objects_to_remove <- all_objects[all_objects != "pred_master"]}
if (!exists("pred_master")) {objects_to_remove <- all_objects}
rm(list = objects_to_remove)


install.packages(c("brms", "rstan", "posterior"))

listOfPackages <- c("tictoc","tibble","tidyr","brms","dplyr",
                    "readr","data.table","rstan","future",
                    "future.apply","stringr")

for (i in 1:length(listOfPackages)) {
  package <- listOfPackages[i]
  if(!package %in% installed.packages()){install.packages(listOfPackages[i], dependencies = TRUE)} 
}

library(tictoc)
library(tibble)
library(tidyr)
library(brms)
library(dplyr)
library(readr)
library(data.table)
library(rstan)
library(future)
library(future.apply)
library(stringr)


# url to digital ocean space where model is stored
# eng_fit <- "https://mrp-psf.ams3.digitaloceanspaces.com/models/eng-2025-09-16_sep2025elex.rds"
# scot_fit <- "https://mrp-psf.ams3.digitaloceanspaces.com/models/scot-2025-09-15_sep2025elex.rds"
# wales_fit <- "https://mrp-psf.ams3.digitaloceanspaces.com/models/wales-2025-09-15_sep2025elex.rds"


     
 eng_fit <- "eng-2025-12-17_dec2025elex.rds"
 scot_fit <- "scot-2025-12-16_dec2025elex.rds"
wales_fit <- "wales-2025-12-16_dec2025elex.rds"

# eng_fit <- "models/model_eng.RDS"
# scot_fit <- "models/model_scot.RDS"
# wales_fit <- "models/model_wales.RDS"



# log_day <- read_rds(eng_fit)
# log_day <- log_day$data
# log_day <- max(log_day$log_day)

# url to digital ocean space where psf is stored
psf_location <- "https://mrp-psf.ams3.digitaloceanspaces.com/psf-with-ge2024"

ndraws <- 500

#### loading and prepping ####

aux <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/const_data_full_new.csv")
aux <- aux %>% mutate(const_name = gsub("Ynys Mon", "Ynys Môn", const_name))

elex_results <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/ge2024_results.csv")%>%
  mutate(const_name = gsub("Ynys M\xf4n", "Ynys Môn", const_name, useBytes = TRUE),
         const_name = gsub("Ynys Mon", "Ynys Môn", const_name, useBytes = TRUE)) %>% 
  select(const_name,turnout,ge_con,ge_lab,ge_libdem,ge_ref,ge_green,ge_snp,ge_pc,ge_oth)

vote_cols <- c("ge_con", "ge_lab", "ge_libdem", "ge_ref", 
               "ge_green", "ge_snp", "ge_pc", "ge_oth")

# Then multiply by (1 - turnout) so they sum to the remaining proportion
elex_results[vote_cols] <- elex_results[vote_cols] * (elex_results$turnout)
elex_results <- elex_results %>% mutate(turnout = 1-turnout)













aux <- aux %>% left_join(elex_results,by='const_name')

const_names <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/order_for_preds.csv") %>% select(const_name)

const_to_region <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/Const%20to%20region.csv")
const_names <- left_join(const_names, const_to_region)
if (!exists("scot_fit")) {const_names <- const_names %>% filter(region != "Scotland")}
if (!exists("wales_fit")) {const_names <- const_names %>% filter(region != "Wales")}
if (!exists("eng_fit")) {const_names <- const_names %>% filter(region != "East Midlands")}
if (!exists("eng_fit")) {const_names <- const_names %>% filter(region != "East of England")}
if (!exists("eng_fit")) {const_names <- const_names %>% filter(region != "Greater London")}
if (!exists("eng_fit")) {const_names <- const_names %>% filter(region != "North East England")}
if (!exists("eng_fit")) {const_names <- const_names %>% filter(region != "North West England")}
if (!exists("eng_fit")) {const_names <- const_names %>% filter(region != "South East England")}
if (!exists("eng_fit")) {const_names <- const_names %>% filter(region != "South West England")}
if (!exists("eng_fit")) {const_names <- const_names %>% filter(region != "West Midlands")}
if (!exists("eng_fit")) {const_names <- const_names %>% filter(region != "Yorkshire and the Humber")}

const_names <- data.frame(const_name = const_names$const_name)

## load electorate size data and make sure constituency names are consistent
esize <- read_csv("/Users/david/Dropbox/zain_asaf/September MRP Vote/electorate_size_const.csv") %>%
  setNames(c("const_name", "electorate", "n_voters"))

esize$const_name <- ifelse(grepl("Montgomeryshire",esize$const_name),"Montgomeryshire and Glyndwr", esize$const_name)
esize$const_name <- ifelse(grepl("Ynys",esize$const_name),"Ynys Môn", esize$const_name)

# const_names <- const_names %>% slice(1:25)

library(doFuture)
options('doFuture.rng.onMisuse' = "ignore")
registerDoFuture()
# wrk <- parallel::detectCores()-2
wrk <- 8
plan(multisession, workers=wrk)


## Look at Islington 
const_names %>% mutate(row = row_number()) %>%  filter(grepl("Islington", const_name))
## assign row number of constituency
n <- 391
## block and run from const_name_i <- const_names$const_name[n] to just before the write.csv()

try(dir.create("overall"), silent=TRUE)
try(dir.create("age"), silent=TRUE)
try(dir.create("ethnicity"), silent=TRUE)
try(dir.create("sex"), silent=TRUE)
try(dir.create("degree"), silent=TRUE)
try(dir.create("age_sex"), silent=TRUE)

res <- foreach(n = 1:nrow(const_names), .packages=c("brms", "dplyr", "stringr"), 
               .errorhandling = "pass") %dopar% {
                 const_name_i <- const_names$const_name[n]
                 const_formatted <- str_replace_all(const_name_i, "ô", "%C3%B4")
                 const_formatted <- str_replace_all(const_formatted, " ", "%20")
                 psf_file <- paste0("https://mrp-psf.ams3.digitaloceanspaces.com/psf-with-ge2024/psf-", const_formatted,".csv")
                 data <- read.csv(psf_file)
                 names(data)[names(data) == 'welsh_level'] <- 'welshlang'
                 ## join in electorate size and number of voters
                 data <- data %>% left_join(esize) %>% 
                   ## calculate expected number of voters in each cell assuming equal turnout rates across groups. 
                   mutate(voters_per_cell = n_voters*perc, 
                          electorate_per_cell = electorate*perc)
                 region <- data$region[1]
                 if (region == "Scotland") {fit <- readRDS(scot_fit)}
                 if (region == "Wales") {fit <- readRDS(wales_fit)}
                 if (region != "Wales" & region != "Scotland") {fit <- readRDS(eng_fit)}
                 if (const_name_i == "South Holland and The Deepings") {data$region <- "East Midlands"}
                 if (const_name_i == "Queen's Park and Maida Vale") {data$region <- "Greater London"}
                 data <- data %>% select(-c(X))
                 data <- data %>% left_join(aux, by = 'const_name')
                 options(timeout = 600)
                 pred <- posterior_epred(fit, newdata = data, allow_new_levels = TRUE, ndraws = ndraws) 
                 pred <- as.data.frame(pred)
                 data <- data %>% dplyr::mutate(id = as.character(row_number()))
                 pred <- t(pred)
                 pred <- rownames_to_column(data.frame(pred), var = "names")
                 pred <- pred %>%
                   rename_with(~str_replace(., "X", "pred"), everything()) %>%
                   separate(names,
                            into = c("num", "level"),
                            extra = "merge") %>%
                   dplyr::mutate(level = str_replace_all(level, "[.]", " ")) %>%
                   dplyr::mutate(id = str_replace(num, "X", "")) %>% 
                   full_join(data, pred, by = "id")
                 overall_pred <- pred %>%
                   select(c(num, level, const_name, perc, voters_per_cell, electorate_per_cell, starts_with("pred"))) %>% 
                   dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
                   dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
                   group_by(const_name,level) %>% 
                   dplyr::summarise(across(starts_with("pred"), sum), 
                                    voters_per_cell = sum(voters_per_cell), 
                                    electorate_per_cell = sum(electorate_per_cell)) %>% 
                   rowwise() %>%
                   dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE), 
                                 voters_per_cell = voters_per_cell * mean, 
                                 electorate_per_cell = electorate_per_cell * mean) %>%
                   select(const_name,level,mean, voters_per_cell, electorate_per_cell)
                 ## keep ethn_level or whatever other demographic variable you want to group by
                 eth_pred <- pred  %>%
                   select(c(num, level, const_name, ethn_level, perc, voters_per_cell, electorate_per_cell, starts_with("pred"))) %>% 
                   dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
                   dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
                   ## keep demographic grouping variable and group by it
                   group_by(const_name,level, ethn_level) %>%
                   ## also keep voterss_per_cell summary
                   dplyr::summarise(across(starts_with("pred"), sum), 
                                    voters_per_cell = sum(voters_per_cell), 
                                    electorate_per_cell = sum(electorate_per_cell)) %>% 
                   rowwise() %>%
                   dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
                   ## keep voters_per_cell, too
                   select(const_name,level,ethn_level, mean, voters_per_cell, electorate_per_cell) %>% 
                   ## group_by grouping variable as well
                   group_by(const_name, ethn_level) %>% 
                   ## calculate the within-group percentages 
                   mutate(group_pct = mean/sum(mean), 
                          ## calculate voters per cell for the grouping variable
                          voters_per_cell = voters_per_cell*group_pct, 
                          electorate_per_cell = electorate_per_cell*group_pct)
                 
                 age_pred <- pred  %>%
                   select(c(num, level, const_name, age_level, perc, voters_per_cell, electorate_per_cell, starts_with("pred"))) %>% 
                   dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
                   dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
                   group_by(const_name,level, age_level) %>%
                   dplyr::summarise(across(starts_with("pred"), sum), 
                                    voters_per_cell = sum(voters_per_cell), 
                                    electorate_per_cell = sum(electorate_per_cell)) %>% 
                   rowwise() %>%
                   dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
                   select(const_name,level,age_level, mean, voters_per_cell, electorate_per_cell) %>% 
                   group_by(const_name, age_level) %>% 
                   mutate(group_pct = mean/sum(mean), 
                          voters_per_cell = voters_per_cell*group_pct, 
                          electorate_per_cell = electorate_per_cell*group_pct)
                 
                 sex_pred <- pred  %>%
                   select(c(num, level, const_name, sex_level, perc, voters_per_cell, electorate_per_cell, starts_with("pred"))) %>% 
                   dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
                   dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
                   group_by(const_name,level, sex_level) %>%
                   dplyr::summarise(across(starts_with("pred"), sum), 
                                    voters_per_cell = sum(voters_per_cell), 
                                    electorate_per_cell = sum(electorate_per_cell)) %>% 
                   rowwise() %>%
                   dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
                   select(const_name,level,sex_level, mean, voters_per_cell, electorate_per_cell) %>% 
                   group_by(const_name, sex_level) %>% 
                   mutate(group_pct = mean/sum(mean), 
                          voters_per_cell = voters_per_cell*group_pct, 
                          electorate_per_cell = electorate_per_cell*group_pct)
                 
                 edu_pred <- pred  %>%
                   select(c(num, level, const_name, edu_level, perc, voters_per_cell, electorate_per_cell, starts_with("pred"))) %>% 
                   dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
                   dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
                   group_by(const_name,level, edu_level) %>%
                   dplyr::summarise(across(starts_with("pred"), sum), 
                                    voters_per_cell = sum(voters_per_cell), 
                                    electorate_per_cell = sum(electorate_per_cell)) %>% 
                   rowwise() %>%
                   dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
                   select(const_name,level,edu_level, mean, voters_per_cell, electorate_per_cell) %>% 
                   group_by(const_name, edu_level) %>% 
                   mutate(group_pct = mean/sum(mean), 
                          voters_per_cell = voters_per_cell*group_pct, 
                          electorate_per_cell = electorate_per_cell*group_pct)
                 
                 age_sex_pred <- pred  %>%
                   select(c(num, level, const_name, age_level, sex_level, perc, voters_per_cell, electorate_per_cell, starts_with("pred"))) %>% 
                   dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
                   dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
                   group_by(const_name,level, age_level, sex_level) %>%
                   dplyr::summarise(across(starts_with("pred"), sum), 
                                    voters_per_cell = sum(voters_per_cell), 
                                    electorate_per_cell = sum(electorate_per_cell)) %>% 
                   rowwise() %>%
                   dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
                   select(const_name,level,age_level, sex_level, mean, voters_per_cell, electorate_per_cell) %>% 
                   group_by(const_name, age_level, sex_level) %>% 
                   mutate(group_pct = mean/sum(mean), 
                          voters_per_cell = voters_per_cell*group_pct, 
                          electorate_per_cell = electorate_per_cell*group_pct)
                 
                 write.csv(overall_pred,paste0("overall/preds_",const_name_i,".csv"))
                 write.csv(age_pred,paste0("age/preds_",const_name_i,".csv"))
                 write.csv(eth_pred,paste0("ethnicity/preds_",const_name_i,".csv"))
                 write.csv(sex_pred,paste0("sex/preds_",const_name_i,".csv"))
                 write.csv(edu_pred,paste0("degree/preds_",const_name_i,".csv"))
                 write.csv(age_sex_pred,paste0("age_sex/preds_",const_name_i,".csv"))
               }


pred_files <- list.files(path="overall/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master

all_preds %>%
  select(const_name, level, voters_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "voters_per_cell") -> voters_master

all_preds %>%
  select(const_name, level, electorate_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "electorate_per_cell") -> electorate_master

write.csv(pred_master, "overall/dec2025_mrp_preds.csv")
write.csv(voters_master, "overall/dec2025_mrp_voters.csv")
write.csv(electorate_master, "overall/dec2025_mrp_electorate.csv")

pred_files <- list.files(path="age/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, age_level,  mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master

all_preds %>%
  select(const_name, level, age_level, voters_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "voters_per_cell") -> voters_master

all_preds %>%
  select(const_name, level, age_level, electorate_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "electorate_per_cell") -> electorate_master

all_preds %>%
  select(const_name, level, age_level, group_pct) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "group_pct") -> group_master

write.csv(pred_master, "age/dec2025_mrp_preds.csv")
write.csv(voters_master, "age/dec2025_mrp_voters.csv")
write.csv(electorate_master, "age/dec2025_mrp_electorate.csv")
write.csv(group_master, "age/dec2025_mrp_group.csv")

pred_files <- list.files(path="sex/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, sex_level,  mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master

all_preds %>%
  select(const_name, level, sex_level, voters_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "voters_per_cell") -> voters_master

all_preds %>%
  select(const_name, level, sex_level, electorate_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "electorate_per_cell") -> electorate_master

all_preds %>%
  select(const_name, level, sex_level, group_pct) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "group_pct") -> group_master

write.csv(pred_master, "sex/dec2025_mrp_preds.csv")
write.csv(voters_master, "sex/dec2025_mrp_voters.csv")
write.csv(electorate_master, "sex/dec2025_mrp_electorate.csv")
write.csv(group_master, "sex/dec2025_mrp_group.csv")

pred_files <- list.files(path="ethnicity/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, ethn_level,  mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master

all_preds %>%
  select(const_name, level, ethn_level, voters_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "voters_per_cell") -> voters_master

all_preds %>%
  select(const_name, level, ethn_level, electorate_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "electorate_per_cell") -> electorate_master

all_preds %>%
  select(const_name, level, ethn_level, group_pct) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "group_pct") -> group_master

write.csv(pred_master, "ethnicity/dec2025_mrp_preds.csv")
write.csv(voters_master, "ethnicity/dec2025_mrp_voters.csv")
write.csv(electorate_master, "ethnicity/dec2025_mrp_electorate.csv")
write.csv(group_master, "ethnicity/dec2025_mrp_group.csv")

pred_files <- list.files(path="degree/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, edu_level,  mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master

all_preds %>%
  select(const_name, level, edu_level, voters_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "voters_per_cell") -> voters_master

all_preds %>%
  select(const_name, level, edu_level, electorate_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "electorate_per_cell") -> electorate_master

all_preds %>%
  select(const_name, level, edu_level, group_pct) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "group_pct") -> group_master

write.csv(pred_master, "degree/dec2025_mrp_preds.csv")
write.csv(voters_master, "degree/dec2025_mrp_voters.csv")
write.csv(electorate_master, "degree/dec2025_mrp_electorate.csv")
write.csv(group_master, "degree/dec2025_mrp_group.csv")


pred_files <- list.files(path="age_sex/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, age_level, sex_level,  mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master

all_preds %>%
  select(const_name, level, age_level, sex_level, voters_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "voters_per_cell") -> voters_master

all_preds %>%
  select(const_name, level, age_level, sex_level, electorate_per_cell) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "electorate_per_cell") -> electorate_master

all_preds %>%
  select(const_name, level, age_level, sex_level, group_pct) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "group_pct") -> group_master

write.csv(pred_master, "age_sex/dec2025_mrp_preds.csv")
write.csv(voters_master, "age_sex/dec2025_mrp_voters.csv")
write.csv(electorate_master, "age_sex/dec2025_mrp_electorate.csv")
write.csv(group_master, "age_sex/dec2025_mrp_group.csv")



const_to_region <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/Const%20to%20region.csv")

age_v <- read_csv("age/dec2025_mrp_voters.csv")
age_v <- age_v %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))

age_v <- age_v %>% group_by(age_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) 
total_v <- age_v %>% pivot_longer(-c(age_level, country), names_to="party", values_to = "votes") %>% group_by(country) %>% summarise(votes = sum(votes))
total_vw<- total_v %>% bind_rows(total_v %>% summarise(country="GB", votes = sum(votes))) %>% pivot_wider(names_from = "country", values_from = "votes")
age_v_cell <- age_v %>% 
  group_by(age_level) %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) %>%
  bind_cols(total_vw)%>% 
  mutate(across(c(Conservative, Labour, `Liberal Democrat`, Other, `Reform UK`, `The Green Party`, `WNV`), ~.x/GB), 
         `Plaid Cymru` = `Plaid Cymru`/Wales, 
         `Scottish National Party (SNP)` = `Scottish National Party (SNP)`/Scotland) %>%
  select(-c(England, Scotland, Wales, GB)) %>% 
  mutate(vbl = "Age", .before="age_level") %>% rename("category" = "age_level")

age_v_cell3 <- age_v %>% 
  left_join(total_v)%>% 
  mutate(across(c(Conservative:`Scottish National Party (SNP)`), ~.x/votes)) %>%
  select(-votes) %>% 
  mutate(vbl = "Age", .before="age_level") %>% rename("category" = "age_level") %>% 
  arrange(country, category)

age_p <- read_csv("age/dec2025_mrp_preds.csv")
age_p <- age_p %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))
age_p_cell <- age_p %>% group_by(age_level) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Age", .before="age_level") %>% rename("category" = "age_level") %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x))) %>% 
  arrange(category)

age_p_cell3 <- age_p %>% group_by(age_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Age", .before="age_level") %>% rename("category" = "age_level") %>% 
  arrange(country, category) %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x)))


age_g <- read_csv("age/dec2025_mrp_group.csv")
age_g <- age_g %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))
age_g_cell <- age_g%>% group_by(age_level) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE))) %>% 
  mutate(vbl = "Age", .before="age_level") %>% rename("category" = "age_level") %>% 
  arrange(category)

age_g_cell3 <- age_g %>% group_by(age_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Age", .before="age_level") %>% rename("category" = "age_level") %>% 
  arrange(country, category) %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x)))


sex_v <- read_csv("sex/dec2025_mrp_voters.csv")
sex_v <- sex_v %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))

sex_v <- sex_v %>% group_by(sex_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) 
total_v <- sex_v %>% pivot_longer(-c(sex_level, country), names_to="party", values_to = "votes") %>% group_by(country) %>% summarise(votes = sum(votes))
total_vw<- total_v %>% bind_rows(total_v %>% summarise(country="GB", votes = sum(votes))) %>% pivot_wider(names_from = "country", values_from = "votes")
sex_v_cell <- sex_v %>% 
  group_by(sex_level) %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) %>%
  bind_cols(total_vw)%>% 
  mutate(across(c(Conservative, Labour, `Liberal Democrat`, Other, `Reform UK`, `The Green Party`, `WNV`), ~.x/GB), 
         `Plaid Cymru` = `Plaid Cymru`/Wales, 
         `Scottish National Party (SNP)` = `Scottish National Party (SNP)`/Scotland) %>%
  select(-c(England, Scotland, Wales, GB)) %>% 
  mutate(vbl = "Sex", .before="sex_level") %>% rename("category" = "sex_level")

sex_v_cell3 <- sex_v %>% 
  left_join(total_v)%>% 
  mutate(across(c(Conservative:`Scottish National Party (SNP)`), ~.x/votes)) %>%
  select(-votes) %>% 
  mutate(vbl = "Sex", .before="sex_level") %>% rename("category" = "sex_level") %>% 
  arrange(country, category)

sex_p <- read_csv("sex/dec2025_mrp_preds.csv")
sex_p <- sex_p %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))
sex_p_cell <- sex_p %>% group_by(sex_level) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Sex", .before="sex_level") %>% rename("category" = "sex_level") %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x))) %>% 
  arrange(category)

sex_p_cell3 <- sex_p %>% group_by(sex_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Sex", .before="sex_level") %>% rename("category" = "sex_level") %>% 
  arrange(country, category) %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x)))


sex_g <- read_csv("sex/dec2025_mrp_group.csv")
sex_g <- sex_g %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))
sex_g_cell <- sex_g%>% group_by(sex_level) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE))) %>% 
  mutate(vbl = "Sex", .before="sex_level") %>% rename("category" = "sex_level") %>% 
  arrange(category)

sex_g_cell3 <- sex_g %>% group_by(sex_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Sex", .before="sex_level") %>% rename("category" = "sex_level") %>% 
  arrange(country, category) %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x)))

edu_v <- read_csv("degree/dec2025_mrp_voters.csv")
edu_v <- edu_v %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))

edu_v <- edu_v %>% group_by(edu_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) 
total_v <- edu_v %>% pivot_longer(-c(edu_level, country), names_to="party", values_to = "votes") %>% group_by(country) %>% summarise(votes = sum(votes))
total_vw<- total_v %>% bind_rows(total_v %>% summarise(country="GB", votes = sum(votes))) %>% pivot_wider(names_from = "country", values_from = "votes")
edu_v_cell <- edu_v %>% 
  group_by(edu_level) %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) %>%
  bind_cols(total_vw)%>% 
  mutate(across(c(Conservative, Labour, `Liberal Democrat`, Other, `Reform UK`, `The Green Party`, `WNV`), ~.x/GB), 
         `Plaid Cymru` = `Plaid Cymru`/Wales, 
         `Scottish National Party (SNP)` = `Scottish National Party (SNP)`/Scotland) %>%
  select(-c(England, Scotland, Wales, GB)) %>% 
  mutate(vbl = "Education", .before="edu_level") %>% rename("category" = "edu_level")

edu_v_cell3 <- edu_v %>% 
  left_join(total_v)%>% 
  mutate(across(c(Conservative:`Scottish National Party (SNP)`), ~.x/votes)) %>%
  select(-votes) %>% 
  mutate(vbl = "Education", .before="edu_level") %>% rename("category" = "edu_level") %>% 
  arrange(country, category)

edu_p <- read_csv("degree/dec2025_mrp_preds.csv")
edu_p <- edu_p %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))
edu_p_cell <- edu_p %>% group_by(edu_level) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Education", .before="edu_level") %>% rename("category" = "edu_level") %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x))) %>% 
  arrange(category)

edu_p_cell3 <- edu_p %>% group_by(edu_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Education", .before="edu_level") %>% rename("category" = "edu_level") %>% 
  arrange(country, category) %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x)))


edu_g <- read_csv("degree/dec2025_mrp_group.csv")
edu_g <- edu_g %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))
edu_g_cell <- edu_g%>% group_by(edu_level) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE))) %>% 
  mutate(vbl = "Education", .before="edu_level") %>% rename("category" = "edu_level") %>% 
  arrange(category)

edu_g_cell3 <- edu_g %>% group_by(edu_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Education", .before="edu_level") %>% rename("category" = "edu_level") %>% 
  arrange(country, category) %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x)))

ethn_v <- read_csv("ethnicity/dec2025_mrp_voters.csv")
ethn_v <- ethn_v %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))

ethn_v <- ethn_v %>% group_by(ethn_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) 
total_v <- ethn_v %>% pivot_longer(-c(ethn_level, country), names_to="party", values_to = "votes") %>% group_by(country) %>% summarise(votes = sum(votes))
total_vw<- total_v %>% bind_rows(total_v %>% summarise(country="GB", votes = sum(votes))) %>% pivot_wider(names_from = "country", values_from = "votes")
ethn_v_cell <- ethn_v %>% 
  group_by(ethn_level) %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) %>%
  bind_cols(total_vw)%>% 
  mutate(across(c(Conservative, Labour, `Liberal Democrat`, Other, `Reform UK`, `The Green Party`, `WNV`), ~.x/GB), 
         `Plaid Cymru` = `Plaid Cymru`/Wales, 
         `Scottish National Party (SNP)` = `Scottish National Party (SNP)`/Scotland) %>%
  select(-c(England, Scotland, Wales, GB)) %>% 
  mutate(vbl = "Ethnicity", .before="ethn_level") %>% rename("category" = "ethn_level")

ethn_v_cell3 <- ethn_v %>% 
  left_join(total_v)%>% 
  mutate(across(c(Conservative:`Scottish National Party (SNP)`), ~.x/votes)) %>%
  select(-votes) %>% 
  mutate(vbl = "Ethnicity", .before="ethn_level") %>% rename("category" = "ethn_level") %>% 
  arrange(country, category)

ethn_p <- read_csv("ethnicity/dec2025_mrp_preds.csv")
ethn_p <- ethn_p %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))
ethn_p_cell <- ethn_p %>% group_by(ethn_level) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Ethnicity", .before="ethn_level") %>% rename("category" = "ethn_level") %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x))) %>% 
  arrange(category)

ethn_p_cell3 <- ethn_p %>% group_by(ethn_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Ethnicity", .before="ethn_level") %>% rename("category" = "ethn_level") %>% 
  arrange(country, category) %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x)))


ethn_g <- read_csv("ethnicity/dec2025_mrp_group.csv")
ethn_g <- ethn_g %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))
ethn_g_cell <- ethn_g%>% group_by(ethn_level) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE))) %>% 
  mutate(vbl = "Ethnicity", .before="ethn_level") %>% rename("category" = "ethn_level") %>% 
  arrange(category)

ethn_g_cell3 <- ethn_g %>% group_by(ethn_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Ethnicity", .before="ethn_level") %>% rename("category" = "ethn_level") %>% 
  arrange(country, category) %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x)))

age_sex_v <- read_csv("age_sex/dec2025_mrp_voters.csv")
age_sex_v <- age_sex_v %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))

age_sex_v <- age_sex_v %>% group_by(age_level, sex_level, country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) 
total_v <- age_sex_v %>% pivot_longer(-c(age_level, sex_level, country), names_to="party", values_to = "votes") %>% group_by(country) %>% summarise(votes = sum(votes))
total_vw<- total_v %>% bind_rows(total_v %>% summarise(country="GB", votes = sum(votes))) %>% pivot_wider(names_from = "country", values_from = "votes")
age_sex_v_cell <- age_sex_v %>% 
  group_by(age_level, sex_level) %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) %>%
  bind_cols(total_vw)%>% 
  mutate(across(c(Conservative, Labour, `Liberal Democrat`, Other, `Reform UK`, `The Green Party`, `WNV`), ~.x/GB), 
         `Plaid Cymru` = `Plaid Cymru`/Wales, 
         `Scottish National Party (SNP)` = `Scottish National Party (SNP)`/Scotland) %>%
  select(-c(England, Scotland, Wales, GB)) %>% 
  mutate(age_level = paste(sex_level, age_level, sep=": ")) %>% 
  select(-sex_level) %>% 
  arrange(age_level) %>% 
  mutate(vbl = "Sex-Age", .before="age_level") %>% rename("category" = "age_level")

age_sex_v_cell3 <- age_sex_v %>% 
  left_join(total_v)%>% 
  mutate(across(c(Conservative:`Scottish National Party (SNP)`), ~.x/votes)) %>%
  select(-votes) %>% 
  mutate(age_level = paste(sex_level, age_level, sep=": ")) %>% 
  ungroup %>% 
  select(-sex_level) %>% 
  arrange(age_level) %>% 
  mutate(vbl = "Sex-Age", .before="age_level") %>% rename("category" = "age_level") %>% 
  arrange(country, category)

age_sex_p <- read_csv("age_sex/dec2025_mrp_preds.csv")
age_sex_p <- age_sex_p %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))
age_sex_p_cell <- age_sex_p %>% 
  group_by(age_level, sex_level) %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(age_level = paste(sex_level, age_level, sep=": ")) %>% 
  ungroup %>% 
  select(-sex_level) %>% 
  arrange(age_level) %>% 
  mutate(vbl = "Sex-Age", .before="age_level") %>% rename("category" = "age_level") %>% 
  arrange(category)

age_sex_p_cell3 <- age_sex_p %>% 
  group_by(age_level, sex_level, country) %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(age_level = paste(sex_level, age_level, sep=": ")) %>% 
  ungroup %>% 
  select(-sex_level) %>% 
  arrange(age_level) %>% 
  mutate(vbl = "Sex-Age", .before="age_level") %>% rename("category" = "age_level") %>% 
  arrange(country, category) %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x)))


age_sex_g <- read_csv("age_sex/dec2025_mrp_group.csv")
age_sex_g <- age_sex_g %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))
age_sex_g_cell <- age_sex_g%>% 
  group_by(age_level, sex_level) %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE))) %>% 
  mutate(age_level = paste(sex_level, age_level, sep=": ")) %>% 
  ungroup %>% 
  select(-sex_level) %>% 
  arrange(age_level) %>% 
  mutate(vbl = "Sex-Age", .before="age_level") %>% rename("category" = "age_level") %>% 
  arrange(category)

age_sex_g_cell3 <- age_sex_g %>% 
  group_by(age_level, sex_level, country) %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(age_level = paste(sex_level, age_level, sep=": ")) %>% 
  ungroup %>% 
  select(-sex_level) %>% 
  mutate(vbl = "Sex-Age", .before="age_level") %>% rename("category" = "age_level") %>% 
  arrange(country, category) %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x)))


overall_v <- read_csv("overall/dec2025_mrp_voters.csv")
overall_v <- overall_v %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))

overall_v <- overall_v %>% group_by(country) %>% summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) 
total_v <- overall_v %>% pivot_longer(-c(country), names_to="party", values_to = "votes") %>% group_by(country) %>% summarise(votes = sum(votes))
total_vw<- total_v %>% bind_rows(total_v %>% summarise(country="GB", votes = sum(votes))) %>% pivot_wider(names_from = "country", values_from = "votes")
overall_v_cell <- overall_v %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~sum(.x, na.rm=TRUE))) %>%
  bind_cols(total_vw)%>% 
  mutate(across(c(Conservative, Labour, `Liberal Democrat`, Other, `Reform UK`, `The Green Party`, `WNV`), ~.x/GB), 
         `Plaid Cymru` = `Plaid Cymru`/Wales, 
         `Scottish National Party (SNP)` = `Scottish National Party (SNP)`/Scotland) %>%
  select(-c(England, Scotland, Wales, GB)) %>% 
  mutate(vbl = "Overall", category = "Overall", .before="Conservative")

overall_v_cell3 <- overall_v %>% 
  left_join(total_v)%>% 
  mutate(across(c(Conservative:`Scottish National Party (SNP)`), ~.x/votes)) %>%
  select(-votes) %>% 
  mutate(vbl = "Overall", category="Overall", .before="country") %>% 
  arrange(country, category)

overall_p <- read_csv("overall/dec2025_mrp_preds.csv")
overall_p <- overall_p %>% left_join(const_to_region) %>% mutate(country = case_when(region == "Scotland" ~ "Scotland", region == "Wales" ~ "Wales", TRUE ~ "England"))
overall_p_cell <- overall_p %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Overall", category="Overall", .before="Conservative") %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x))) 

overall_p_cell3 <- overall_p %>% 
  group_by(country) %>% 
  summarise(across(c(Conservative:`Scottish National Party (SNP)`), ~mean(.x, na.rm=TRUE)))  %>% 
  mutate(vbl = "Overall", category="Overall", .before="country") %>% 
  arrange(country, category) %>% 
  mutate(across(Conservative:`Scottish National Party (SNP)`, ~ifelse(!is.finite(.x), 0, .x)))

overall_g_cell <- overall_p_cell
overall_g_cell3 <- overall_p_cell3

two_blank <- data.frame(vbl = c("", ""), category = c("", ""), Conservative = c(NA, NA), Labour = c(NA, NA), `Liberal Democrat` = c(NA, NA), Other = c(NA, NA), `Reform UK` = c(NA, NA), `The Green Party` = c(NA, NA), WNV = c(NA, NA), `Plaid Cymru` = c(NA, NA), `Scottish National Party (SNP)` = c(NA, NA))
bind_rows(overall_v_cell, age_v_cell, sex_v_cell, edu_v_cell, ethn_v_cell, age_sex_v_cell) %>% print(n=100)

library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Overall")
addWorksheet(wb, "Age")
addWorksheet(wb, "Sex")
addWorksheet(wb, "Education")
addWorksheet(wb, "Ethnicity")
addWorksheet(wb, "Age-Sex")


writeData(wb, sheet = "Overall", x = overall_v_cell, startRow = 1, startCol = 1)
writeData(wb, sheet = "Age", x = age_v_cell, startRow = 1, startCol = 1)
writeData(wb, sheet = "Sex", x = sex_v_cell, startRow = 1, startCol = 1)
writeData(wb, sheet = "Education", x = edu_v_cell, startRow = 1, startCol = 1)
writeData(wb, sheet = "Ethnicity", x = ethn_v_cell, startRow = 1, startCol = 1)
writeData(wb, sheet = "Age-Sex", x = age_sex_v_cell, startRow = 1, startCol = 1)

try(dir.create("summaries"))
saveWorkbook(wb, file = "summaries/voter_shares_GB.xlsx", overwrite = TRUE)

wb2 <- createWorkbook()
addWorksheet(wb2, "Overall")
addWorksheet(wb2, "Age")
addWorksheet(wb2, "Sex")
addWorksheet(wb2, "Education")
addWorksheet(wb2, "Ethnicity")
addWorksheet(wb2, "Age-Sex")


writeData(wb2, sheet = "Overall", x = overall_v_cell3, startRow = 1, startCol = 1)
writeData(wb2, sheet = "Age", x = age_v_cell3, startRow = 1, startCol = 1)
writeData(wb2, sheet = "Sex", x = sex_v_cell3, startRow = 1, startCol = 1)
writeData(wb2, sheet = "Education", x = edu_v_cell3, startRow = 1, startCol = 1)
writeData(wb2, sheet = "Ethnicity", x = ethn_v_cell3, startRow = 1, startCol = 1)
writeData(wb2, sheet = "Age-Sex", x = age_sex_v_cell3, startRow = 1, startCol = 1)

saveWorkbook(wb2, file = "summaries/voter_shares_by_country.xlsx", overwrite = TRUE)


wb3 <- createWorkbook()
addWorksheet(wb3, "Overall")
addWorksheet(wb3, "Age")
addWorksheet(wb3, "Sex")
addWorksheet(wb3, "Education")
addWorksheet(wb3, "Ethnicity")
addWorksheet(wb3, "Age-Sex")


writeData(wb3, sheet = "Overall", x = overall_p_cell3, startRow = 1, startCol = 1)
writeData(wb3, sheet = "Age", x = age_p_cell3, startRow = 1, startCol = 1)
writeData(wb3, sheet = "Sex", x = sex_p_cell3, startRow = 1, startCol = 1)
writeData(wb3, sheet = "Education", x = edu_p_cell3, startRow = 1, startCol = 1)
writeData(wb3, sheet = "Ethnicity", x = ethn_p_cell3, startRow = 1, startCol = 1)
writeData(wb3, sheet = "Age-Sex", x = age_sex_p_cell3, startRow = 1, startCol = 1)

saveWorkbook(wb3, file = "summaries/predicted_shares_by_country.xlsx", overwrite = TRUE)

wb4 <- createWorkbook()
addWorksheet(wb4, "Overall")
addWorksheet(wb4, "Age")
addWorksheet(wb4, "Sex")
addWorksheet(wb4, "Education")
addWorksheet(wb4, "Ethnicity")
addWorksheet(wb4, "Age-Sex")


writeData(wb4, sheet = "Overall", x = overall_p_cell, startRow = 1, startCol = 1)
writeData(wb4, sheet = "Age", x = age_p_cell, startRow = 1, startCol = 1)
writeData(wb4, sheet = "Sex", x = sex_p_cell, startRow = 1, startCol = 1)
writeData(wb4, sheet = "Education", x = edu_p_cell, startRow = 1, startCol = 1)
writeData(wb4, sheet = "Ethnicity", x = ethn_p_cell, startRow = 1, startCol = 1)
writeData(wb4, sheet = "Age-Sex", x = age_sex_p_cell, startRow = 1, startCol = 1)

saveWorkbook(wb4, file = "summaries/predicted_shares_GB.xlsx", overwrite = TRUE)

wb5 <- createWorkbook()
addWorksheet(wb5, "Overall")
addWorksheet(wb5, "Age")
addWorksheet(wb5, "Sex")
addWorksheet(wb5, "Education")
addWorksheet(wb5, "Ethnicity")
addWorksheet(wb5, "Age-Sex")


writeData(wb5, sheet = "Overall", x = overall_g_cell3, startRow = 1, startCol = 1)
writeData(wb5, sheet = "Age", x = age_g_cell3, startRow = 1, startCol = 1)
writeData(wb5, sheet = "Sex", x = sex_g_cell3, startRow = 1, startCol = 1)
writeData(wb5, sheet = "Education", x = edu_g_cell3, startRow = 1, startCol = 1)
writeData(wb5, sheet = "Ethnicity", x = ethn_g_cell3, startRow = 1, startCol = 1)
writeData(wb5, sheet = "Age-Sex", x = age_sex_g_cell3, startRow = 1, startCol = 1)

saveWorkbook(wb5, file = "summaries/predicted_shares_within_group_by_country.xlsx", overwrite = TRUE)

wb6 <- createWorkbook()
addWorksheet(wb6, "Overall")
addWorksheet(wb6, "Age")
addWorksheet(wb6, "Sex")
addWorksheet(wb6, "Education")
addWorksheet(wb6, "Ethnicity")
addWorksheet(wb6, "Age-Sex")


writeData(wb6, sheet = "Overall", x = overall_g_cell, startRow = 1, startCol = 1)
writeData(wb6, sheet = "Age", x = age_g_cell, startRow = 1, startCol = 1)
writeData(wb6, sheet = "Sex", x = sex_g_cell, startRow = 1, startCol = 1)
writeData(wb6, sheet = "Education", x = edu_g_cell, startRow = 1, startCol = 1)
writeData(wb6, sheet = "Ethnicity", x = ethn_g_cell, startRow = 1, startCol = 1)
writeData(wb6, sheet = "Age-Sex", x = age_sex_g_cell, startRow = 1, startCol = 1)

saveWorkbook(wb6, file = "summaries/predicted_shares_within_group_GB.xlsx", overwrite = TRUE)
