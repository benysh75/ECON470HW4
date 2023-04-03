## Title: ECON 470 HW4
## Author: Ben Yang
## Date Created: 3/22/2023
## Date Edited: 4/3/2023
## Descriptions: This file renders/runs all relevant R code for the assignment

## Preliminaries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr,
               knitr, data.table, kableExtra, tinytex, scales,
               lubridate, stringr, gdata,
               readxl, 
               rdrobust, rddensity, estimatr,
               modelsummary, fixest, AER)

## Read data and set workspace for knitr ---------------------------------------

ma.data <- read_rds("data/output/final_ma_data.rds")

## Question 1 Distribution of plan counts by county over time ------------------

ma.data.selected <- ma.data %>%
  filter(snp == "No" & (planid < 800 | planid >= 900) & partd == "No")

q1.data <- ma.data.selected %>%
  group_by(year, county) %>%
  summarise(count = length(planid))

q1.data.sum <- q1.data %>%
  group_by(year) %>% 
  summarise(`Mean` = mean(count),
            `Max` = max(count),
            `Q2` = quantile(count, 0.25),
            `Median` = median(count),
            `Q3` = quantile(count, 0.75),
            `Min` = min(count))

q1.plot <- q1.data %>%
  ggplot(aes(x = factor(year), y = log(count))) +
  geom_boxplot(size = 0.5, color = "dodgerblue4", fill = "dodgerblue1", alpha = 0.25, outlier.color = "dodgerblue4", outlier.shape = 16) +
  # stat_summary(fun.y = "mean", geom = "point", shape = 16, size = 4, color = "dodgerblue") +
  labs(x = "Year", y = "Log Plan Counts by County", Title = "Distribution of Plan Counts by County from 2007 to 2015") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 2 Distribution of star ratings in 2009, 2012, and 2015 -------------

q2.data <- ma.data %>% 
  filter(year %in% c(2009, 2012, 2015) & !is.na(avg_enrollment) & !is.na(partc_score)) %>%
  group_by(year) %>%
  summarise(`1.5` = sum(Star_Rating == "1.5"), `2` = sum(Star_Rating == "2"),
            `2.5` = sum(Star_Rating == "2.5"), `3` = sum(Star_Rating == "3"),
            `3.5` = sum(Star_Rating == "3.5"), `4` = sum(Star_Rating == "4"),
            `4.5` = sum(Star_Rating == "4.5"), `5` = sum(Star_Rating == "5")) %>%
  pivot_longer(-year, names_to = "Star_Rating", values_to = "count")

q2.plot <- q2.data %>%
  ggplot(aes(x = as.factor(Star_Rating), y = count, fill = factor(year))) +
  geom_col(width = 0.75, position = position_dodge(0.8)) +
  scale_fill_manual(name = "year", values = c("dodgerblue1", "dodgerblue3", "dodgerblue4")) +
  geom_text(aes(label = count, y = count + 250), size = 3, check_overlap = TRUE, position = position_dodge(width = 0.8)) +
  ylim(0,9000) +
  labs(x = "Star Rating", y = "Count of Plans", Title = paste0("Frequency Distribution of Star Ratings in 2009, 2012, and 2015")) +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

q2.plot2 <-  q2.data %>%
  ggplot(aes(x = as.factor(Star_Rating), y = count)) +
  geom_col(fill = "dodgerblue4") +
  geom_text(aes(label = count, y = count + 250), size = 3, check_overlap = TRUE) +
  facet_wrap(vars(factor(year))) +
  ylim(0,9000) +
  labs(x = "Star Rating", y = "Count of Plans", Title = paste0("Frequency Distribution of Star Ratings in 2009, 2012, and 2015")) +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

for (i in c(2009, 2012, 2015)){
  ma.data.year <- ma.data %>% 
    filter(year==i & !is.na(avg_enrollment) & !is.na(partc_score))
  
  q2.plot.year <- ma.data.year %>% 
    ggplot(aes(x = as.factor(Star_Rating))) + 
    geom_bar(width = 0.5, fill = "dodgerblue4") +
    labs(x = "Star Rating", y = "Count of Plans", Title = paste0("Frequency Distribution of Star Ratings in ", i)) +
    theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
    theme(
      plot.title = element_text(size = 12, color = "black", hjust = 0.5),
      legend.title = element_text(size = 10, color = "black"),
      legend.position = "top",
      axis.title = element_text(size = 10, color = "black"),
      axis.text.x = element_text(size = 10, angle = 0, color = "black"),
      axis.text.y = element_text(size = 10, angle = 0, color = "black"))
  
  assign(paste0("ma.data.", i), ma.data.year)
  assign(paste0("q2.plot.", i), q2.plot.year)
}

## Question 3 Avg Benchmark Payment 2009-2015 ----------------------------------

q3.data <- ma.data %>%
  filter(year %in% 2009:2015) %>%
  group_by(year) %>%
  summarise(avg_payment_partc = mean(payment_partc, na.rm = TRUE))

q3.plot <- q3.data %>% 
  ggplot(aes(x = factor(year), y = avg_payment_partc)) +
  geom_col(width = 0.5, fill = "dodgerblue4") +
  geom_text(label = round(q3.data$avg_payment_partc,1), size = 3, nudge_x = 0, nudge_y = 25, check_overlap = TRUE) +
  ylim(0,800) +
  labs(x = "Year", y = "Average Benchmark Payment (Part C)", Title = "Average Benchmark Payment from 2009 to 2015") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 4 Avg share of Medicare Advantage 2009-2015 ------------------------

q4.data <- ma.data %>%
  filter(year %in% 2009:2015) %>%
  group_by(year) %>%
  summarise(all_enroll = sum(avg_enrollment, na.rm = TRUE),
            all_eligible = sum(avg_eligibles, na.rm = TRUE),
            share = all_enroll/all_eligible * 100)

q4.plot <- q4.data %>%
  ggplot(aes(x = factor(year), y = share)) +
  geom_col(width = 0.5, fill = "dodgerblue4") +
  geom_text(label = paste0(round(q4.data$share,2), "%"), size = 3, nudge_x = 0, nudge_y = 0.025, check_overlap = TRUE) +
  ylim(0,1) +
  labs(x = "Year", y = "Share (%)", Title = "Average Share of Medicare Advantage (Relative to All Medicare Eligibles) from 2009 to 2015") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

## Question 5 Running Variable Underlying the Star Rating ----------------------

ma.data.clean <- ma.data %>%
  filter(!is.na(avg_enrollment) & year==2009 & !is.na(partc_score))

ma.data.clean <- ma.data.clean %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diabetes_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess,
          hospital_followup,depression_followup,nodelays,carequickly,
          overallrating_care,overallrating_plan,calltime,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,antidepressant,bloodpressure,ra_manage,
          copd_test,betablocker,bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T)) 

q5.data <- ma.data.clean %>% 
  group_by(factor(Star_Rating)) %>%
  summarise(count = length(contractid))
colnames(q5.data) <- c("Star Ratings", "Count")

## Question 6 RD, bandwidth 0.125, effect of 3-star vs 2.5 star on enrollments -
## Question 7 Bandwidths 0.1, 0.12, 0.13, 0.14, and 0.15 -----------------------

bandwidth <- c(0.125, 0.1, 0.12, 0.13, 0.14, 0.15)
starRating <- c(3, 3.5, 4)
ma.rd.ls <- list()
for (j in length(bandwidth)){
  ma.rd.ls2 <- list()
  for (i in length(starRating)){
    ma.rd.ls3 <- list()
    ma.rd <- ma.data.clean %>% filter(Star_Rating == starRating[i]-0.5 | Star_Rating == starRating[i])
    ma.rd.dist <- ma.rd %>% ggplot(aes(x = raw_rating)) + geom_bar(width = .025) + theme_bw() +
      labs(x = "Running Variable", y = "Number of Plans", title = "Distribution of Raw Scores") 
    
    ma.rd <-ma.rd %>%
      mutate(score = raw_rating - (starRating[i]-0.25),
             treat = (score >= 0),
             window1 = (score >= -.175 & score <= .175),
             window2 = (score >= -.125 & score <= .125),
             enrollment = avg_enrollment,
             score_treat = score*treat)
    
    ma.rd.plot <- rdplot(y = ma.rd$enrollment, x = ma.rd$score, binselect = "es",
                         title = "RD Plot: Enrollment", x.label = "Summary Score", y.label = "Enrollment", masspoints = "off")
    ma.rd.est <- rdrobust(y = ma.rd$enrollment, x = ma.rd$score, c = 0, h = bandwidth[j], p = 1,
                          kernel = "uniform", vce = "hc0", masspoints = "off")
    
    ma.rd.bin.avg <- as_tibble(ma.rd.plot$vars_bins)
    ma.rd.plot.bin <- ma.rd.bin.avg %>% ggplot(aes(x = rdplot_mean_x, y = rdplot_mean_y)) + 
      geom_point() + theme_bw() +
      geom_vline(aes(xintercept = 1), linetype = 'dashed') +
      scale_x_continuous(breaks = c(.5, 1.5), label = c("Untreated", "Treated")) +
      xlab("Running Variable") + ylab("Outcome")
    
    ma.rd.ls3[[1]] <- ma.rd
    ma.rd.ls3[[2]] <- ma.rd.dist
    ma.rd.ls3[[3]] <- ma.rd.plot
    ma.rd.ls3[[4]] <- ma.rd.est
    ma.rd.ls3[[5]] <- ma.rd.bin.avg
    ma.rd.ls3[[6]] <- ma.rd.plot.bin
    names(ma.rd.ls3) <- c(paste0("ma.rd_", i), paste0("ma.rd.dist_", i), paste0("ma.rd.plot_", i), paste0("ma.rd.est_", i), paste0("ma.rd.bin.avg_", i), paste0("ma.rd.plot.bin_", i))
    ma.rd.ls2[[i]] <- ma.rd.ls3
  }
  names(ma.rd.ls2) <- paste0(rep("ma.rd.ls_SR", length(starRating)), starRating)
  ma.rd.ls[[j]] <- ma.rd.ls2
}
names(ma.rd.ls) <-paste0(rep("ma.rd.ls_BW", length(bandwidth)), bandwidth)

ma.rd.ls[[6]]

q6.data <- cbind(ma.rd.est_3$coef, ma.rd.est_3.5$coef, ma.rd.est_4$coef)
colnames(q6.data) <- c("3-Star", "3.5-Star", "4-Star")

## Question 8 Manipulate the Running Variable ----------------------------------

ma.rd_3_dens <- rddensity(ma.rd_3$score, c=0)
q8.plot <- rdplotdensity(ma.rd_3_dens, ma.rd_3$score)

## Question 9 HMO and Part D status as Plan Characteristics --------------------

m <- lm(formula = avg_enrollment ~ factor(Star_Rating) + factor(partd) + factor(plan_type), data = ma.data.clean)

## Save data for markdown ------------------------------------------------------

colnames(ma.data)
rm(list=c("ma.data"))
save.image("Hwk4_workspace.Rdata")
levels(factor(ma.data.clean$plan_type))

# %>%
# select(contractid, planid, fips, avg_enrollment, first_enrolled, last_enrolled, 
#        state, county, raw_rating, partc_score,
#        avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
#        bid, avg_ffscost, ma_rate)