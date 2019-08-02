# GRE study
# Data exploration / descriptive analysis
#
# Amanda Irish
# Last updated 7/18/19

library(tidyverse)
library(RColorBrewer)

setwd("/Users/amandairish/Desktop/GRE study")

# Load data
dat <- read.csv("admin_data.csv")

# Basic dataset exploration
str(dat)
summary(dat)
names(dat)
head(dat)

# Number of missings overall
#rowSums(is.na(dat)) # Number of missing per row - a lot of output!
mean(rowSums(is.na(dat))) # Average number of missing per row
colSums(is.na(dat))

# number of duplicate applicant IDs
sum(duplicated(dat$applicant_id))
#1709 applicant IDs w/ 1+ application

# Extract duplicate obs applicant ID numbers
duplicates <- dat$applicant_id[duplicated(dat$applicant_id)]

# Create recoded outcome variable
levels(dat$status)

dat$status_new <- "NA"
dat$status_new[dat$status=="Application Started" | dat$status=="Application Fee Paid" | 
                 dat$status=="Application Submitted"] <- "Not considered"
dat$status_new[dat$status=="Application Complete" | dat$status=="At Reader" | dat$status=="Under Review" |
                 dat$status=="Interview Invited" | dat$status=="Interview Scheduled" | dat$status==
                 "Decline to Interview" | dat$status=="Withdrawal"] <- "Withdrawal" 
dat$status_new[dat$status=="Rejected" | dat$status=="Waitlisted" | dat$status=="Waitlist Pending" |
                 dat$status=="Rejection Pending"] <- "Rejected"
dat$status_new[dat$status=="Offered Admission" | dat$status=="Offer Declined"] <- "Offer declined"
dat$status_new[dat$status=="Deferred"] <- "Deferred"
dat$status_new[dat$status=="Offer Accepted"] <- "Offer accepted"

dat$status_new <- as.factor(dat$status_new)
levels(dat$status_new)
any(is.na(dat$status_new)) # Check if we've exhausted all the options for dat$status in recoding
summary(dat$status_new) # see how many are in each category


# Code a new URM variable from "ethnic_background" and "us_citizen" variables
dat$ethnic_background <- as.factor(as.character(dat$ethnic_background))
dat$urm_new <- ifelse(((dat$us_citizen=="US" | dat$us_citizen=="PR") &
                  (grepl("Hispanic|Black|Chicano|Vietnamese|Filipino|Alaskan|Hawaiian", 
                         dat$ethnic_background))), "Yes",
           ifelse(is.na(dat$us_citizen) | is.na(dat$ethnic_background), NA, "No"))

dat$urm_new <- as.factor(dat$urm_new)

# Check to make sure this worked correctly
table(dat$urm_new, dat$us_citizen)

View(dat %>%
       filter(us_citizen=="US" | us_citizen=="PR") %>%
       group_by(urm_new, ethnic_background) %>%
       summarize(n()))

# Compare urm_new to original urm var
dat %>%
  filter(us_citizen=="US" | us_citizen=="PR") %>%
  group_by(urm_new, urm) %>%
  summarize(n())             


# Create indicator var for complete/not complete
dat$app_complete <- ifelse((dat$status=="Application Started" | dat$status=="Application Fee Paid" | 
                             dat$status=="Application Submitted"), "Not complete", "Complete")
dat$app_complete <- as.factor(dat$app_complete)

table(dat$app_complete, dat$status)
table(dat$app_complete, dat$status_new)

# Create indicator var for UCSF accepted/not accepted, *among completed applications*
dat$accepted <- ifelse((dat$status=="Offer Accepted" | dat$status=="Offer Declined" | 
                          dat$status=="Offered Admission"), "Accepted",
                      ifelse(dat$status_new=="Withdrawal" | dat$status_new=="Rejected", "Not accepted", NA))
# NA = not completed. Coded as NA b/c only want to evaluate for those that have completed apps, and
# NAs will force drop out of those
dat$accepted <- as.factor(dat$accepted)

table(dat$status, dat$accepted)
table(dat$status_new, dat$accepted)
table(dat$app_complete, dat$accepted)


# Create age variable
dat$age <- dat$app_year - dat$Year.of.birth
dat %>% filter(age < 18)
# Unique.ID 5204 - 14 (2002)
#           9355 - 1  (2016)
#          13106 - 1  (2017)
#          13394 - 1  (2017)

# For now, set age==NA if age < 17
dat$age <- ifelse(dat$age <17, NA, dat$age)
dat %>% filter(age < 18)
summary(dat$age)

###################
# Missing data by whether application is complete
dat %>%
  filter(app_complete=="Complete") %>%
  summarize_each(funs(sum(is.na(.))))

###################
# Investigate duplicated data

# Create df with only duplicate applicant IDs
duplicates_with_data <- dat %>%
  filter(applicant_id %in% duplicates)

# Table of appID & year
#table(duplicates_with_data$applicant_id, duplicates_with_data$app_year)

# saved this table as a dataframe
dup_app_by_year <- as.data.frame(table(duplicates_with_data$applicant_id, duplicates_with_data$app_year))
# note this doesn't quite do what I want - saves as appid/year/freq (long rather than wide)

# Number of years duplicates applied in
no_years_dup <- duplicates_with_data %>%
  group_by(applicant_id) %>%
  summarize(no_years = length(unique(app_year)))
#no_years_dup

mean_no_years_dup <- mean(no_years_dup$no_years)
mean_no_years_dup

med_no_years_dup <- median(no_years_dup$no_years)
med_no_years_dup

# how many applied 1, 2, 3, 4 years?
no_years_dup %>%
  group_by(no_years) %>%
  summarize(no_applicants = length(applicant_id))

# Number of programs duplicates applied to
no_programs_dup <- duplicates_with_data %>%
  group_by(applicant_id) %>%
  summarize(no_programs = length(unique(program_descr)))
no_programs_dup

mean_no_prog_dup <- mean(no_programs_dup$no_programs)
mean_no_prog_dup

med_no_prog_dup <- median(no_programs_dup$no_programs)
med_no_prog_dup


# Number of duplicates by categories of status_new
no_dup_by_status <- duplicates_with_data %>%
  group_by(status_new) %>%
  summarize(no_applicants = length(applicant_id))
no_dup_by_status

# Number of dupliates by URM
dat %>%
  group_by(urm_new) %>%
  summarize(no_applicants = length(applicant_id))

##################

# Create a dataset with unique applicant IDs
# note that the default for this is to take the first row for IDs that have multiple entries

dat_unique <- dat[!duplicated(dat$applicant_id), ]


###########
#Summarize important variables
summary(dat_unique$urm_new)
summary(dat_unique$status_new)
summary(dat_unique$app_complete)
summary(dat_unique$accepted)
summary(dat_unique$ETS_GRE_quant_percent)
summary(dat_unique$ETS_GRE_verbal_percent)
summary(dat_unique$ETS_GRE_writing_percent)


##################
# Histograms of GRE percentiles - unique applicants
hist(dat_unique$ETS_GRE_quant_percent)
hist(dat_unique$ETS_GRE_verbal_percent)
hist(dat_unique$ETS_GRE_writing_percent)

vline_dat_1 <- dat_unique %>%
  filter(!is.na(ETS_GRE_quant_percent) & !is.na(ETS_GRE_verbal_percent) & !is.na(ETS_GRE_writing_percent)) %>%
  group_by(status_new) %>%
  summarize(quant_med_pct = median(ETS_GRE_quant_percent), verbal_med_pct = median(ETS_GRE_verbal_percent),
            write_med_pct = median(ETS_GRE_writing_percent))

# Histograms of GRE percentiles by categories of status_new
gre_quant_status <- ggplot(dat_unique, aes(ETS_GRE_quant_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=quant_med_pct), data=vline_dat_1, color = "red") +
  labs(title = "GRE quantitative percentile by application status") +
  facet_wrap(vars(status_new))
gre_quant_status

gre_verbal_status <- ggplot(dat_unique, aes(ETS_GRE_verbal_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=verbal_med_pct), data=vline_dat_1, color = "red") +
  labs(title = "GRE verbal percentile by application status") +
  facet_wrap(vars(status_new))
gre_verbal_status

gre_write_status <- ggplot(dat_unique, aes(ETS_GRE_writing_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=write_med_pct), data=vline_dat_1, color = "red") +
  labs(title = "GRE writing percentile by application status") +
  facet_wrap(vars(status_new))
gre_write_status


vline_dat_2 <- dat_unique %>%
  filter(!is.na(ETS_GRE_quant_percent) & !is.na(ETS_GRE_verbal_percent) & !is.na(ETS_GRE_writing_percent)) %>%
  group_by(urm_new) %>%
  summarize(quant_med_pct = median(ETS_GRE_quant_percent), verbal_med_pct = median(ETS_GRE_verbal_percent),
            write_med_pct = median(ETS_GRE_writing_percent))

# Histograms of GRE percentiles by urm
gre_quant_urm <- ggplot(dat_unique, aes(ETS_GRE_quant_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=quant_med_pct), data=vline_dat_2, color = "red") +
  labs(title = "GRE quantitative percentile by URM status") +
  facet_wrap(vars(urm_new))
gre_quant_urm

gre_verbal_urm <- ggplot(dat_unique, aes(ETS_GRE_verbal_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=verbal_med_pct), data=vline_dat_2, color = "red") +
  labs(title = "GRE verbal percentile by URM status") +
  facet_wrap(vars(urm_new))
gre_verbal_urm

gre_write_urm <- ggplot(dat_unique, aes(ETS_GRE_writing_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=write_med_pct), data=vline_dat_2, color = "red") +
  labs(title = "GRE writing percentile by URM status") +
  facet_wrap(vars(urm_new))
gre_write_urm

###### By 2 categories
vline_dat_3 <- dat_unique %>%
  filter(!is.na(ETS_GRE_quant_percent) & !is.na(ETS_GRE_verbal_percent) & !is.na(ETS_GRE_writing_percent)) %>%
  group_by(urm_new, app_complete) %>%
  summarize(quant_med_pct = median(ETS_GRE_quant_percent), verbal_med_pct = median(ETS_GRE_verbal_percent),
            write_med_pct = median(ETS_GRE_writing_percent))

# By categories of app_complete and urm
# Quant
gre_quant_complete_urm <- ggplot(dat_unique, aes(ETS_GRE_quant_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=quant_med_pct), data=vline_dat_3, color = "red") +
  facet_grid(rows = vars(urm_new), cols = vars(app_complete)) +
  labs(title = "GRE quantitative percentile, by URM and application completion")
gre_quant_complete_urm

# Verbal
gre_verbal_complete_urm <- ggplot(dat_unique, aes(ETS_GRE_verbal_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=verbal_med_pct), data=vline_dat_3, color = "red") +
  facet_grid(rows = vars(urm_new), cols = vars(app_complete)) +
  labs(title = "GRE verbal percentile, by URM and application completion")
gre_verbal_complete_urm

# Write
gre_write_complete_urm <- ggplot(dat_unique, aes(ETS_GRE_writing_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=write_med_pct), data=vline_dat_3, color = "red") +
  facet_grid(rows = vars(urm_new), cols = vars(app_complete)) +
  labs(title = "GRE writing percentile, by URM and application completion")
gre_write_complete_urm


vline_dat_4 <- dat_unique %>%
  filter(!is.na(ETS_GRE_quant_percent) & !is.na(ETS_GRE_verbal_percent) & !is.na(ETS_GRE_writing_percent)) %>%
  group_by(urm_new, accepted) %>%
  summarize(quant_med_pct = median(ETS_GRE_quant_percent), verbal_med_pct = median(ETS_GRE_verbal_percent),
            write_med_pct = median(ETS_GRE_writing_percent))

# By categories of accepted and urm
# Quant
gre_quant_accept_urm <- ggplot(dat_unique, aes(ETS_GRE_quant_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=quant_med_pct), data=vline_dat_4, color = "red") +
  facet_grid(rows = vars(urm_new), cols = vars(accepted)) +
  labs(title = "GRE quantitative percentile, by URM and accepted")
gre_quant_accept_urm

# Verbal
gre_verbal_accept_urm <- ggplot(dat_unique, aes(ETS_GRE_verbal_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=verbal_med_pct), data=vline_dat_4, color = "red") +
  facet_grid(rows = vars(urm_new), cols = vars(accepted)) +
  labs(title = "GRE verbal percentile, by URM and accepted")
gre_verbal_accept_urm

# Writing
gre_write_accept_urm <- ggplot(dat_unique, aes(ETS_GRE_writing_percent)) +
  geom_histogram(binwidth = 5) +
  geom_vline(aes(xintercept=write_med_pct), data=vline_dat_4, color = "red") +
  facet_grid(rows = vars(urm_new), cols = vars(accepted)) +
  labs(title = "GRE writing percentile, by URM and accepted")
gre_write_accept_urm


#########
#par(mfrow=c(2,2))
#hist(dat_unique$ETS_GRE_quant_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="Yes"],
#     main = "Quant GRE for URM, rejected",
#     xlab = "Quant GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_quant_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="Yes"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
#hist(dat_unique$ETS_GRE_quant_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="No"],
#     main = "Quant GRE for non-URM, rejected",
#     xlab = "Quant GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_quant_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="No"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
#hist(dat_unique$ETS_GRE_quant_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="Yes"],
#     main = "Quant GRE for URM, accepted",
#     xlab = "Quant GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_quant_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="Yes"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
#hist(dat_unique$ETS_GRE_quant_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="No"],
#     main = "Quant GRE for non-URM, accepted",
#     xlab = "Quant GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_quant_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="No"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
# For quantitative GRE, within URM group, median GRE score across accepted/rejected is not meaningfully different.
# Across URM groups, median GRE scores were meaningfully different.


# Verbal
#hist(dat_unique$ETS_GRE_verbal_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="Yes"],
#     main = "Verbal GRE for URM, rejected",
#     xlab = "Verbal GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_verbal_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="Yes"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
#hist(dat_unique$ETS_GRE_verbal_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="No"],
#     main = "Verbal GRE for non-URM, rejected",
#     xlab = "Verbal GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_verbal_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="No"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
#hist(dat_unique$ETS_GRE_verbal_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="Yes"],
#     main = "Verbal GRE for URM, accepted",
#     xlab = "Verbal GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_verbal_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="Yes"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
#hist(dat_unique$ETS_GRE_verbal_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="No"],
#     main = "Verbal GRE for non-URM, accepted",
#     xlab = "Verbal GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_verbal_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="No"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
# Opposite association as with quantitative score above - associated w/ accepted/rejected but not
# as much with URM


# Writing
#hist(dat_unique$ETS_GRE_writing_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="Yes"],
#     main = "Writing GRE for URM, rejected",
#     xlab = "Writing GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_writing_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="Yes"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
#hist(dat_unique$ETS_GRE_writing_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="No"],
#     main = "Writing GRE for non-URM, rejected",
#     xlab = "Writing GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_writing_percent[dat_unique$status_new=="Rejected" & dat_unique$urm_new=="No"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
#hist(dat_unique$ETS_GRE_writing_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="Yes"],
#     main = "Writing GRE for URM, accepted",
#     xlab = "Writing GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_writing_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="Yes"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)
#hist(dat_unique$ETS_GRE_writing_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="No"],
#     main = "Writing GRE for non-URM, accepted",
#     xlab = "Writing GRE percentile")
#abline(v = median(dat_unique$ETS_GRE_writing_percent[dat_unique$status_new=="Offer accepted" & dat_unique$urm_new=="No"], na.rm = TRUE),
#       col = "red",
#       lwd = 2)


# Table of status_new by URM and median percentile GRE scores
dat_unique %>%
  filter(!is.na(ETS_GRE_quant_percent) & !is.na(ETS_GRE_verbal_percent) & !is.na(ETS_GRE_writing_percent)) %>%
  group_by(urm_new, status_new) %>%
  summarize(quant_med_pct = median(ETS_GRE_quant_percent), verbal_med_pct = median(ETS_GRE_verbal_percent),
            write_med_pct = median(ETS_GRE_writing_percent))

# Table of app_complete by URM and median percentile GRE scores
dat_unique %>%
  filter(!is.na(ETS_GRE_quant_percent) & !is.na(ETS_GRE_verbal_percent) & !is.na(ETS_GRE_writing_percent)) %>%
  group_by(urm_new, app_complete) %>%
  summarize(quant_med_pct = median(ETS_GRE_quant_percent), verbal_med_pct = median(ETS_GRE_verbal_percent),
            write_med_pct = median(ETS_GRE_writing_percent))

# Table of accepted by URM and median percentile GRE scores
dat_unique %>%
  filter(!is.na(ETS_GRE_quant_percent) & !is.na(ETS_GRE_verbal_percent) & !is.na(ETS_GRE_writing_percent)) %>%
  group_by(urm_new, accepted) %>%
  summarize(quant_med_pct = median(ETS_GRE_quant_percent), verbal_med_pct = median(ETS_GRE_verbal_percent),
            write_med_pct = median(ETS_GRE_writing_percent))


# Boxplot of status_new for URM vs. not
p <- ggplot(dat_unique) +
  geom_bar(aes(x = urm_new, fill = status_new), position = "dodge") +
  scale_fill_brewer(palette = "Dark2")
p


# Table of URM and status_new
urm_status_table <- dat_unique %>%
  group_by(urm_new, status_new) %>%
  summarize(n = n()) %>%
  mutate(percent = n / sum(n))
urm_status_table

# Boxplot of status_new for URM vs. not - proportions
q <- ggplot(urm_status_table) +
  geom_bar(aes(x=urm_new, y=percent, fill=status_new), position = 'dodge', stat = "identity") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "URM status", y = "Proportion within URM group", title = "Application status by URM group")
q


# Boxplot of URM by status_new - proprotions
r <- ggplot(urm_status_table) +
  geom_bar(aes(x=status_new, y=percent, fill=urm_new), position = 'dodge', stat = "identity") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Application status", y = "Proportion within application status group", 
       title = "URM group by application status")
r



# Table for app completion by URM status
dat_unique %>%
  filter(!is.na(urm_new)) %>%
  group_by(app_complete, urm_new) %>%
  summarize(n = n()) %>%
  mutate(percent = n / sum(n))

# Tables for whether GRE is included in application / med GRE score for those with GRE, by app completion & URM status
# GRE score missing, by app complete
dat_unique %>%
  group_by(app_complete) %>%
  summarize(n = n(),
            na_quant = sum(is.na(ETS_GRE_quant_percent)),
            na_verbal = sum(is.na(ETS_GRE_verbal_percent)),
            na_write = sum(is.na(ETS_GRE_verbal_percent))) %>%
  mutate(prop_na = na_quant/n) # since each test has same number of missings

# GRE score missing, by app complete/URM
dat_unique %>%
  filter(!is.na(urm_new)) %>%
  group_by(app_complete, urm_new) %>%
  summarize(n = n(),
            na_quant = sum(is.na(ETS_GRE_quant_percent)),
            na_verbal = sum(is.na(ETS_GRE_verbal_percent)),
            na_write = sum(is.na(ETS_GRE_verbal_percent))) %>%
  mutate(prop_na = na_quant/n) # since each test has same number of missings

# GRE score median, by app complete/URM
dat_unique %>%
  filter(!is.na(urm_new)) %>%
  group_by(urm_new, app_complete) %>%
  summarize(n=n(),
            quant_med_pct = median(ETS_GRE_quant_percent, na.rm = TRUE),
            verbal_med_pct = median(ETS_GRE_verbal_percent, na.rm = TRUE),
            write_med_pct = median(ETS_GRE_writing_percent, na.rm = TRUE))


# Demographic characteristics by app_complete
# Categorical var
cat_demog_complete <- dat_unique %>%
  gather(variable, value, urm_new, gender_identity, CA_Resident, parent1_edu_level, parent2_edu_level) %>%
  group_by(app_complete, variable, value) %>%
  summarise (n = n()) %>%
  mutate(prop = n / sum(n))
cat_demog_complete %>% 
  print(n = nrow(.))

# Mean age
dat_unique %>%
  group_by(app_complete) %>%
  summarize(n=n(),
            mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE))


# Demographic characteristics by accepted
# Categorical var
cat_demog_accepted <- dat_unique %>%
  gather(variable, value, urm_new, gender_identity, CA_Resident, parent1_edu_level, parent2_edu_level) %>%
  group_by(accepted, variable, value) %>%
  summarise (n = n()) %>%
  mutate(prop = n / sum(n))
cat_demog_accepted %>% 
  print(n = nrow(.))


# Mean age
dat_unique %>%
  group_by(accepted) %>%
  summarize(n=n(),
            mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE))

dat_unique %>%
  filter(!is.na(ETS_GRE_quant_percent) & !is.na(ETS_GRE_verbal_percent) & !is.na(ETS_GRE_writing_percent)) %>%
  group_by(urm_new) %>%
  summarize(quant_med_pct = median(ETS_GRE_quant_percent), 
            verbal_med_pct = median(ETS_GRE_verbal_percent),
            write_med_pct = median(ETS_GRE_writing_percent))


# Summary stats for GRE by group of URM - median/IQR
tapply(dat_unique$ETS_GRE_quant_percent, dat_unique$urm_new, summary)
tapply(dat_unique$ETS_GRE_verbal_percent, dat_unique$urm_new, summary)
tapply(dat_unique$ETS_GRE_writing_percent, dat_unique$urm_new, summary)

# Significance testing for medians calculated above
# GRE quant by URM
wilcox.test(dat_unique$ETS_GRE_quant_percent[dat_unique$urm_new=="Yes"],
            dat_unique$ETS_GRE_quant_percent[dat_unique$urm_new=="No"],
            paired=FALSE)

# GRE verbal by URM
wilcox.test(dat_unique$ETS_GRE_verbal_percent[dat_unique$urm_new=="Yes"],
            dat_unique$ETS_GRE_verbal_percent[dat_unique$urm_new=="No"],
            paired=FALSE)

# GRE writing by URM
wilcox.test(dat_unique$ETS_GRE_writing_percent[dat_unique$urm_new=="Yes"],
            dat_unique$ETS_GRE_writing_percent[dat_unique$urm_new=="No"],
            paired=FALSE)

# Summary stats for app status by group of URM
tapply(dat_unique$app_complete, dat_unique$urm_new, summary)
tapply(dat_unique$status_new, dat_unique$urm_new, summary)
tapply(dat_unique$accepted, dat_unique$urm_new, summary)


# Summary stats for accepted & URM - median/IQR
accepted <- filter(dat_unique, accepted=="Accepted")
not_accepted <- filter(dat_unique, accepted=="Not accepted")

tapply(accepted$ETS_GRE_quant_percent, accepted$urm_new, summary)
tapply(accepted$ETS_GRE_verbal_percent, accepted$urm_new, summary)
tapply(accepted$ETS_GRE_writing_percent, accepted$urm_new, summary)

tapply(not_accepted$ETS_GRE_quant_percent, not_accepted$urm_new, summary)
tapply(not_accepted$ETS_GRE_verbal_percent, not_accepted$urm_new, summary)
tapply(not_accepted$ETS_GRE_writing_percent, not_accepted$urm_new, summary)

# Significance testing for medians calculated above
# GRE quant by accepted/URM
wilcox.test(accepted$ETS_GRE_quant_percent[accepted$urm_new=="Yes"],
            accepted$ETS_GRE_quant_percent[accepted$urm_new=="No"],
            paired=FALSE)

# GRE verbal by accepted/URM
wilcox.test(accepted$ETS_GRE_verbal_percent[accepted$urm_new=="Yes"],
            accepted$ETS_GRE_verbal_percent[accepted$urm_new=="No"],
            paired=FALSE)

# GRE writing by accepted/URM
wilcox.test(accepted$ETS_GRE_writing_percent[accepted$urm_new=="Yes"],
            accepted$ETS_GRE_writing_percent[accepted$urm_new=="No"],
            paired=FALSE)
###

# GRE quant by not accepted/URM
wilcox.test(not_accepted$ETS_GRE_quant_percent[not_accepted$urm_new=="Yes"],
            not_accepted$ETS_GRE_quant_percent[not_accepted$urm_new=="No"],
            paired=FALSE)

# GRE verbal by not accepted/URM
wilcox.test(not_accepted$ETS_GRE_verbal_percent[not_accepted$urm_new=="Yes"],
            not_accepted$ETS_GRE_verbal_percent[not_accepted$urm_new=="No"],
            paired=FALSE)

# GRE writing by not accepted/URM
wilcox.test(not_accepted$ETS_GRE_writing_percent[not_accepted$urm_new=="Yes"],
            not_accepted$ETS_GRE_writing_percent[not_accepted$urm_new=="No"],
            paired=FALSE)


