#########################################################################################
# Prepared for Bruno and Maevas Data Analysis
# DA3 Assignment 2 : Finding fast growing firms
####################################################



# ------------------------------------------------------------------------------------------------------
#### SET UP

# CLEAR MEMORY
rm(list=ls())


# Import libraries
library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)

library(rattle)
library(caret)
library(pROC)
library(ranger)
library(rpart)
library(partykit)
library(rpart.plot)
library(tidyr)
library(readr)
library(dplyr)

# set working directory
data_dir <- paste0(getwd(),"/DA3_A2_Bruno_Maeva")
SourceFunctions <- paste0("https://raw.githubusercontent.com/Maeva2408/DA3_A2_Bruno_Maeva/main/code/functions/")

# load theme and functions
source(paste0(SourceFunctions,"theme_bg.R"))
source(paste0(SourceFunctions,"da_helper_functions.R"))

data_in <- paste(data_dir,"data/clean/", sep = "/")
output <- paste0(data_dir,"output/")
create_output_if_doesnt_exist(output)
data_out <- paste(data_dir,"data/clean/", sep = "/")



###########################################################
# Import data
###########################################################
dataraw <- read_csv("https://raw.githubusercontent.com/Maeva2408/DA3_A2_Bruno_Maeva/main/data/clean/cs_bisnode_panel.csv")
data <- dataraw

# drop variables with many NAs
data <- data %>%
  select(-c(COGS, finished_prod, net_dom_sales, net_exp_sales, wages)) %>%
  filter(year !=2016)

###########################################################
# label engineering
###########################################################

# add all missing year and comp_id combinations -
# originally missing combinations will have NAs in all other columns
data <- data %>% complete(year, comp_id)

# generate status_alive; if sales larger than zero and not-NA, then firm is alive
data  <- data %>%
  mutate(status_alive = sales > 0 & !is.na(sales) %>% as.numeric(.))

# defaults in two years if there are sales in this year but no sales two years later
data <- data %>% group_by(comp_id) %>%
  mutate(default = ((status_alive == 1) & (lead(status_alive, 2) == 0)) %>%
           as.numeric(.),
         CAGR = ifelse( ( (is.na(lead(sales, 2))) & (status_alive == 1) ),
                             0,((lead(sales, 2) / sales )^(1/2) - 1)*100) ) %>% 
  ungroup()

data <- data %>% mutate(
  HyperGrowth = ifelse(CAGR >= 30, 1,0)) %>% 
  filter(year <=2013)

# Size and growth
summary(data$sales) # There will be NAs, we'll drop them soon

# assets can't be negative. Change them to 0 and add a flag.
data <-data  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))

data <- data %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))

data <- data %>%
  mutate(sales = ifelse(sales < 0, 1, sales),
         ln_sales  = ifelse(sales > 0, log(sales), 0),
         sales_mil = sales/1000000,
         sales_mil_log   = ifelse(sales > 0, log(sales_mil), 0),
         total_assets_bs = intang_assets + curr_assets + fixed_assets)

# Extra Calculated Fields: 
    #   Wk.CapTO = Use of Investment within the year As ratio of Total Sales -> more the better
    #   EBITDA -> Closest proxy for Cash Flow -> Future Re-investment Opportunities
data <- data %>% mutate(
  working_capital_TO =  (curr_assets - curr_liab)/ sales,
  EBITDA = inc_bef_tax + amort)

# D1s : Inventories, total_assets, Amort,EBITDA
data <- data %>% 
  group_by(comp_id) %>% 
  mutate(d1_sales_mil_log      = sales_mil_log - Lag(sales_mil_log, 1),
         d1_inventories = (inventories - lag(inventories,1))/sales,
         d1_total_assets_bs = (total_assets_bs - lag(total_assets_bs,1))/sales,
         d1_amort = (amort - lag(amort,1))/sales ) %>%
  ungroup()

# Extra YOY changes: -> % terms
  # Total Assets
  # Inventories
  # EBITDA


# replace w 0 for new firms + add dummy to capture it
data <- data %>%
  mutate(age = ifelse(is.na(year - founded_year),0,year - founded_year) %>%
           ifelse(. < 0, 0, .),
         new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .),
         d1_sales_mil_log = ifelse(new == 1, 0, d1_sales_mil_log),
         new = ifelse(is.na(d1_sales_mil_log), 1, new),
         d1_sales_mil_log = ifelse(is.na(d1_sales_mil_log), 0, d1_sales_mil_log))



###########################################################
 # sample design
###########################################################

# look at cross section
df <- data %>%
  filter((year == 2012) & (status_alive == 1)) %>% 
  # look at firms below 10m euro revenues and above 10000 euros
  filter(!(sales_mil > 10)) %>%
  filter(!(sales_mil < 0.01)) %>% 
  filter(age >= 1) %>% 
  filter(!(is.na(HyperGrowth)))

###########################
#   Feature engineering   #
###########################

# change some industry category codes
df <- df %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .))

table(df$ind2_cat)

# Firm characteristics
df <- df %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))

###########################################################
# look at more financial variables, create ratios
###########################################################

pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp","EBITDA")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

# divide all pl_names elements by sales and create new column for it
df <- df %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
df <- df %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))

########################################################################
# creating flags, and winsorizing tails
########################################################################

## To winsorize : liq_assets_bs, curr_liab_bs, intang_assets_bs,
    #     personnel_exp_pl, profit_loss_year_pl, material_exp_pl, inventories_pl, inc_bef_tax_pl,
    #     extra_profit_loss_pl, extra_inc_pl, working_capital_TO, EBITDA

# Visual Checks
lapply(df %>% select(-matches("begin|end|gender|origin|region|status_alive|founded|exit")) %>% 
         select(-D) %>% colnames(),function(x) {
  bounds <- quantile(df[[c(x)]], c(0,1), na.rm = T)
  print(bounds)
  df %>% ggplot(aes_string(x = x)) + 
    geom_histogram(bins = 50) + 
    xlim(bounds[1],bounds[2]) +theme_tufte()
})


ggplot(data =  df, aes(x=sales_mil_log, y=as.numeric(HyperGrowth))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), color=color[4], se = F, size=1)+
  geom_smooth(method="loess", se=F, colour=color[5], size=1.5, span=0.9) +
  labs(x = "sales_mil_log",y = "HyperGrowth") +
  theme_bg()

ggplot(data =  df, aes(x=d1_working_capital_TO, y=HyperGrowth)) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
#  geom_smooth(method = "lm", formula = y ~ poly(x,2), color=color[4], se = F, size=1)+
  geom_smooth(method="loess", se=F, colour=color[5], size=1.5, span=0.9) +
  labs(x = "Working Capital TO",y = "Hyp.Growth") +
  theme_bg()

ggplot(data =  df, aes(x=d1_total_assets_bs, y=as.numeric(HyperGrowth))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), color=color[4], se = F, size=1)+
  geom_smooth(method="loess", se=F, colour=color[5], size=1.5, span=0.9) +
  ylim(0,1) +
  labs(x = "Current_assets",y = "Hyp.Growth") +
  theme_bg()


# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

df <- df %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))



table(round(df$d1_inventories,0)) # should be betwwen +/- 5
# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_loss_year_pl", "share_eq_bs", 
          "EBITDA_pl","d1_amort","d1_inventories")

df <- df %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))


# dropping flags with no variation
variances<- df %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

df <- df %>%
  select(-one_of(names(variances)[variances]))

########################################################################
# additional
# including some imputation
########################################################################

# CEO age
df <- df %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

df <- df %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

# number emp, very noisy measure
df <- df %>%
  mutate(labor_avg_mod = ifelse(is.na(labor_avg), mean(labor_avg, na.rm = TRUE), labor_avg),
         flag_miss_labor_avg = as.numeric(is.na(labor_avg))) %>%
  select(-labor_avg)

# create factors
df <- df %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(df$ind2_cat)))) %>%
  mutate(HyperGrowth_f = factor(HyperGrowth, levels = c(0,1)) %>%
           recode(., `0` = 'no_Hyp.Growth', `1` = "Hyp.Growth"))

########################################################################
 # sales 
########################################################################

df <- df %>%
  mutate(sales_mil_log_sq=sales_mil_log^2)


ggplot(data =  df, aes(x=sales_mil_log, y=as.numeric(HyperGrowth))) +
  geom_point(size=2,  shape=20, stroke=2, fill="blue", color="blue") +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), color=color[4], se = F, size=1)+
  geom_smooth(method="loess", se=F, colour=color[5], size=1.5, span=0.9) +
  labs(x = "sales_mil_log",y = "HyperGrowth") +
  theme_bg()


ols_s <- lm(default~sales_mil_log+sales_mil_log_sq,
                data = df)
summary(ols_s)

########################################################################
# sales change
########################################################################
# Note: graphs not in book

# lowess
Hmisc::describe(df$d1_sales_mil_log) # no missing

d1sale_1<-ggplot(data = df, aes(x=d1_sales_mil_log, y=as.numeric(HyperGrowth))) +
  geom_point(size=0.1,  shape=20, stroke=2, fill=color[2], color=color[2]) +
  geom_smooth(method="loess", se=F, colour=color[1], size=1.5, span=0.9) +
 labs(x = "Growth rate (Diff of ln sales)",y = "HyperGrowth") +
 theme_bg() +
 scale_x_continuous(limits = c(-6,10), breaks = seq(-5,10, 5))
d1sale_1
save_fig("ch17-extra-1", output, "small")

# generate variables ---------------------------------------------------
# Winsorize - d1_total_assets_bs & Sales changes

df <- df %>%
  mutate(flag_low_d1_sales_mil_log = ifelse(d1_sales_mil_log < -1.5, 1, 0),
         flag_high_d1_sales_mil_log = ifelse(d1_sales_mil_log > 1.5, 1, 0),
         d1_sales_mil_log_mod = ifelse(d1_sales_mil_log < -1.5, -1.5,
                                       ifelse(d1_sales_mil_log > 1.5, 1.5, d1_sales_mil_log)),
         d1_sales_mil_log_mod_sq = d1_sales_mil_log_mod^2,
         flag_low_d1_total_assets_bs  = ifelse(d1_total_assets_bs < -5,1,0),
         flag_high_d1_total_assets_bs = ifelse(d1_total_assets_bs >  5,1,0),
         d1_total_assets_bs_mod       = ifelse(d1_total_assets_bs < -5,-5,
                                               ifelse(d1_total_assets_bs >  5,5,d1_total_assets_bs))
         )

# no more imputation, drop obs if key vars missing
df <- df %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign), !is.na(ind))

# drop missing
df <- df %>%
  filter(!is.na(age),!is.na(foreign), !is.na(material_exp_pl), !is.na(m_region_loc),
         !is.na(d1_total_assets_bs),!is.na(d1_inventories),!is.na(d1_amort))

Hmisc::describe(df$age)



# drop unused factor levels
df <- df %>%
  mutate_at(vars(colnames(df)[sapply(df, is.factor)]), funs(fct_drop))

d1sale_2<-ggplot(data = df, aes(x=d1_sales_mil_log_mod, y=as.numeric(HyperGrowth))) +
  geom_point(size=0.1,  shape=20, stroke=2, fill=color[2], color=color[2]) +
  geom_smooth(method="loess", se=F, colour=color[1], size=1.5, span=0.9) +
  labs(x = "Growth rate (Diff of ln sales)",y = "HyperGrowth") +
  theme_bg() +
  scale_x_continuous(limits = c(-1.5,1.5), breaks = seq(-1.5,1.5, 0.5))
d1sale_2
save_fig("ch17-extra-2", output, "small")

d1sale_3<-ggplot(data = df, aes(x=d1_sales_mil_log, y=d1_sales_mil_log_mod)) +
  geom_point(size=0.1,  shape=20, stroke=2, fill=color[2], color=color[2]) +
  labs(x = "Growth rate (Diff of ln sales) (original)",y = "Growth rate (Diff of ln sales) (winsorized)") +
  theme_bg() +
  scale_x_continuous(limits = c(-5,5), breaks = seq(-5,5, 1)) +
scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3, 1))
d1sale_3
save_fig("ch17-extra-3", output, "small")

# N / % of Firms HyperGrowth
table(df$HyperGrowth)
prop.table(table(df$HyperGrowth))*100

write_csv(df,paste0(data_out,"bisnode_firms_clean.csv"))
write_rds(df,paste0(data_out,"bisnode_firms_clean.rds"))



