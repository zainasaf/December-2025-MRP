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

##eng_fit <- "model_eng.RDS"
##scot_fit <- "model_scot.RDS"
##wales_fit <- "model_wales.RDS"

##eng_fit <- readRDS(here("models", "model_eng.RDS"))
##scot_fit <- readRDS(here("models", "model_scot.RDS"))
##wales_fit <- readRDS(here("models", "model_wales.RDS"))


##eng_fit <- "eng-2025-12-19.rds"
##scot_fit <- "scot-2025-12-18.rds"
##wales_fit <- "wales-2025-12-18.rds"



 eng_fit <- "eng-2025-12-17_dec2025elex.rds"
scot_fit <- "scot-2025-12-16_dec2025elex.rds"
wales_fit <- "wales-2025-12-16_dec2025elex.rds"



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
elex_results[vote_cols] <- elex_results[vote_cols] * (1 - elex_results$turnout)

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
esize <- read_csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/electorate_size_const.csv") %>%
  setNames(c("const_name", "electorate", "n_voters"))

esize$const_name <- ifelse(grepl("Montgomeryshire",esize$const_name),"Montgomeryshire and Glyndwr", esize$const_name)
esize$const_name <- ifelse(grepl("Ynys",esize$const_name),"Ynys Môn", esize$const_name)

# const_names <- const_names %>% slice(1:25)

library(doFuture)
options('doFuture.rng.onMisuse' = "ignore")
registerDoFuture()
wrk <- parallel::detectCores()-2
plan(multisession, workers=wrk)


## Look at Islington 
const_names %>% mutate(row = row_number()) %>%  filter(grepl("Islington", const_name))
## assign row number of constituency
n <- 567
## block and run from const_name_i <- const_names$const_name[n] to just before the write.csv()

try(dir.create("overall"), silent=TRUE)
try(dir.create("age"), silent=TRUE)
try(dir.create("ethnicity"), silent=TRUE)
try(dir.create("sex"), silent=TRUE)
try(dir.create("degree"), silent=TRUE)
try(dir.create("age_sex"), silent=TRUE)

## for single run 126-264

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
                 ## keep ethn_level or whatever other demographic variableu want to group by
                 ## the code for the demographics is doing this: keeping constituency levels info
                 ## multiply each prediction the proportion that demographic cell represents within the constituency
                 ## groups by constituency and demographic
                 ##aggregates weighted predictions within each group
                 
                 eth_pred <- pred  %>%
                   select(c(num, level, const_name, ethn_level, perc, voters_per_cell, electorate_per_cell, starts_with("pred"))) %>% 
                   dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
                   dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    
                   group_by(const_name,level, ethn_level) %>%
                   ## also keep voters_per_cell summary
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
