## Title: ECON 470 HW4
## Author: Ben Yang
## Date Created: 3/22/2023
## Date Edited: 4/10/2023
## Descriptions: This file renders/runs all relevant R code for the assignment

## Preliminaries ---------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, ggpubr, cobalt, dplyr, broom, cobalt, MatchIt,
               knitr, data.table, kableExtra, tinytex, scales,  
               lubridate, stringr, gdata,
               readxl, 
               rdrobust, rddensity, estimatr,
               modelsummary, fixest, AER)

## Read data and set workspace for knitr ---------------------------------------

ma.data <- read_rds("data/output/final_ma_data.rds")

## modelsummary for rdrobust ---------------------------------------------------

rd_ms <- function(rd.est) {
  ti <- data.frame(
    term = c("RD Linear"),
    estimate = c(-1 * rd.est$coef[1]),
    std.error = c(rd.est$se[1])
  )
  
  gl <- data.frame(
    N1 = rd.est$M[1],
    N2 = rd.est$M[2]
  )
  
  mod <- list(
    tidy = ti,
    glance = gl
  )
  
  class(mod) <- "modelsummary_list"
  return(mod)
}

## Question 1 Distribution of plan counts by county over time ------------------

q1.data <- ma.data %>%
  group_by(year, county) %>%
  summarise(count = length(planid))

q1.data.sum <- q1.data %>%
  group_by(factor(year)) %>% 
  summarise(`Mean` = mean(count),
            `Max` = max(count),
            `Q2` = quantile(count, 0.25),
            `Median` = median(count),
            `Q3` = quantile(count, 0.75),
            `Min` = min(count))

q1.plot <- q1.data %>%
  ggplot(aes(x = factor(year), y = count)) +
  geom_boxplot(size = 0.5, color = "dodgerblue4", fill = "dodgerblue1", alpha = 0.25, outlier.color = "dodgerblue4", outlier.shape = NA) +
  stat_summary(fun.y = "mean", geom = "point", shape = 16, size = 4, color = "dodgerblue") +
  labs(x = "Year", y = "Plan Counts by County", Title = "Distribution of Plan Counts by County from 2007 to 2015") +
  ylim(0,100) +
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
  summarise(avg_ma_rate = mean(ma_rate, na.rm = TRUE))

q3.plot <- q3.data %>% 
  ggplot(aes(x = factor(year), y = avg_ma_rate)) +
  geom_col(width = 0.5, fill = "dodgerblue4") +
  geom_text(label = round(q3.data$avg_ma_rate,1), size = 3, nudge_x = 0, nudge_y = 25, check_overlap = TRUE) +
  ylim(0,1000) +
  labs(x = "Year", y = "Average Benchmark Payment", Title = "Average Benchmark Payment from 2009 to 2015") +
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
  group_by(fips, year) %>%
  summarise(all_enroll = first(avg_enrolled),
            all_eligible = first(avg_eligibles),
            bench = mean(ma_rate, na.rm = TRUE),
            share = all_enroll/all_eligible) %>%
  group_by(year) %>%
  summarise(avg_share = mean(share, na.rm = TRUE))

q4.plot <- q4.data %>%
  ggplot(aes(x = year, y = avg_share)) +
  geom_line(size = 1, color = "dodgerblue4") +
  scale_x_continuous(breaks = c(2009:2015)) +
  geom_text(label = round(q4.data$avg_share,2), size = 3, nudge_x = 0, nudge_y = 0.0025, check_overlap = TRUE) +
  labs(x = "Year", y = "Average Share", Title = "Average Share of Medicare Advantage (Relative to All Medicare Eligibles) from 2009 to 2015") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

cor_bench_share <- cor(q3.data$avg_ma_rate, q4.data$avg_share)

## Question 5 Running Variable Underlying the Star Rating ----------------------

ma.data.clean <- ma.data %>% ungroup() %>%
  filter(!is.na(avg_enrollment) & year==2009 & !is.na(partc_score)) %>%
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
    na.rm=T)) %>%
  select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate, plan_type, partd) %>%
  mutate(HMO = str_detect(plan_type, "HMO"))

q5.data <- ma.data.clean %>% 
  mutate(rounded_30 = ifelse(raw_rating >= 2.75 & raw_rating < 3.00 & Star_Rating == 3.0, 1, 0),
         rounded_35 = ifelse(raw_rating >= 3.25 & raw_rating < 3.50 & Star_Rating == 3.5, 1, 0),
         rounded_40 = ifelse(raw_rating >= 3.75 & raw_rating < 4.00 & Star_Rating == 4.0, 1, 0),
         rounded_45 = ifelse(raw_rating >= 4.25 & raw_rating < 4.50 & Star_Rating == 4.5, 1, 0),
         rounded_50 = ifelse(raw_rating >= 4.75 & raw_rating < 5.00 & Star_Rating == 5.0, 1, 0),
         ) %>%
  group_by(factor(Star_Rating)) %>%
  filter(Star_Rating %in% c (3, 3.5, 4, 4.5, 5)) %>%
  summarise(count_30 = sum(rounded_30), count_35 = sum(rounded_35), 
            count_40 = sum(rounded_40), count_45 = sum(rounded_45), 
            count_50 = sum(rounded_50),
            count = count_30 + count_35 + count_40 + count_45 + count_50) %>%
  select(`factor(Star_Rating)`, count)

colnames(q5.data) <- c("Star Ratings", "Count")

## Question 6, 7, 8, 9 ---------------------------------------------------------
### Q6: RD estimate of the Effect of Receiving Stars at 3.0-Star, 3.5-Star, 4.0-Star on Enrollment, bandwidth 0.125
### Q7: Bandwidths 0.1, 0.12, 0.13, 0.14, 0.15 ---------------------------------

bandwidth <- c(0.125, 0.1, 0.12, 0.13, 0.14, 0.15)
starRating <- c(3.0, 3.5, 4.0)

for (i in 1:length(bandwidth)){
  for (j in 1:length(starRating)){
    # Data for RD Model
    cutoff <- starRating[j] - 0.25
    ma.rd.data <- ma.data.clean %>%
      filter(raw_rating >= (cutoff - bandwidth[i]),
             raw_rating <= (cutoff + bandwidth[i]),
             Star_Rating %in% c(starRating[j] - 0.5, starRating[j])) %>%
      mutate(treat = (Star_Rating == starRating[j]),
             score = raw_rating - cutoff)
    
    # RD Model
    ma.rd.est <- lm(avg_enrollment ~ treat + score, data = ma.rd.data)
    ma.rd.est.coef <- tidy(ma.rd.est, conf.int = TRUE) %>% mutate(r = starRating[j], bw = bandwidth[i])
    
    # Density Plot
    ma.rd_dens <- rddensity(ma.rd.data$score, c = 0)
    ma.rd_dens_plot <- rdplotdensity(ma.rd_dens, ma.rd.data$score)
    
    # Love Plot
    lp.vars <- ma.data.clean %>%
      filter((raw_rating >= (cutoff - bandwidth[i]) & Star_Rating == starRating[j] - 0.5) |
               (raw_rating <= (cutoff + bandwidth[i]) & Star_Rating == starRating[j])) %>%
      mutate(rounded = (Star_Rating == starRating[j])) %>% 
      select(HMO, partd, rounded) %>%
      filter(complete.cases(.))
    lp.covs <- lp.vars %>% select(HMO, partd)
    ma.rd_lp <- love.plot(bal.tab(lp.covs, treat = lp.vars$rounded), colors = "dodgerblue4") +
      theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
    
    # Assign Variable
    assign(paste0("ma.rd.est_", bandwidth[i], "_", starRating[j]), ma.rd.est)
    assign(paste0("ma.rd.est.coef_", bandwidth[i], "_", starRating[j]), ma.rd.est.coef)
    assign(paste0("ma.rd_dens_plot_", bandwidth[i], "_", starRating[j]), ma.rd_dens_plot)
    assign(paste0("ma.rd_lp_", bandwidth[i], "_", starRating[j]), ma.rd_lp)
  }
}

q7.data <- 
  rbind(ma.rd.est.coef_0.1_3, ma.rd.est.coef_0.1_3.5, ma.rd.est.coef_0.1_4,
        ma.rd.est.coef_0.12_3, ma.rd.est.coef_0.12_3.5, ma.rd.est.coef_0.12_4,
        ma.rd.est.coef_0.13_3, ma.rd.est.coef_0.13_3.5, ma.rd.est.coef_0.13_4,
        ma.rd.est.coef_0.14_3, ma.rd.est.coef_0.14_3.5, ma.rd.est.coef_0.14_4,
        ma.rd.est.coef_0.15_3, ma.rd.est.coef_0.15_3.5, ma.rd.est.coef_0.15_4
        ) %>%
  select(term, estimate, std.error, r, bw) %>%
  filter(term == "treatTRUE") %>%
  mutate(r = factor(r), bw = factor(bw))

q7.plot <- q7.data %>%
  ggplot(aes(x = bw, y = estimate, group = r, color = r)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_errorbar(aes(x = bw, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = .1, position = position_dodge(width = 0.5)) +
  scale_color_manual(name = "Star Rating", values = c("dodgerblue1", "dodgerblue3", "dodgerblue4")) +
  labs(x = "Bandwidth", y = "Estimate of Treatment and 95 Percent Confidence Interval", Title = "RD Estimates of the Effect of Receiving a Star Rating on Enrollment with Different Bandwidths") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

q7.data2 <- 
  rbind(ma.rd.est.coef_0.1_3, ma.rd.est.coef_0.1_3.5, ma.rd.est.coef_0.1_4,
        ma.rd.est.coef_0.12_3, ma.rd.est.coef_0.12_3.5, ma.rd.est.coef_0.12_4,
        ma.rd.est.coef_0.13_3, ma.rd.est.coef_0.13_3.5, ma.rd.est.coef_0.13_4,
        ma.rd.est.coef_0.14_3, ma.rd.est.coef_0.14_3.5, ma.rd.est.coef_0.14_4,
        ma.rd.est.coef_0.15_3, ma.rd.est.coef_0.15_3.5, ma.rd.est.coef_0.15_4
  ) %>%
  select(term, estimate, std.error, r, bw) %>%
  filter(term == "score") %>%
  mutate(r = factor(r), bw = factor(bw))

q7.plot2 <- q7.data2 %>%
  ggplot(aes(x = bw, y = estimate, group = r, color = r)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_errorbar(aes(x = bw, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = .1, position = position_dodge(width = 0.5)) +
  scale_color_manual(name = "Star Rating", values = c("dodgerblue1", "dodgerblue3", "dodgerblue4")) +
  labs(x = "Bandwidth", y = "Estimate of Score and 95 Percent Confidence Interval", Title = "RD Estimates of the Effect of Receiving a Star Rating on Enrollment with Different Bandwidths") +
  theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(
    plot.title = element_text(size = 12, color = "black", hjust = 0.5),
    legend.title = element_text(size = 10, color = "black"),
    legend.position = "top",
    axis.title = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, angle = 0, color = "black"),
    axis.text.y = element_text(size = 10, angle = 0, color = "black"))

### Q8: Manipulate the Running Variable ----------------------------------------

q8.plot <- ggarrange(ma.rd_dens_plot_0.125_3$Estplot, ma.rd_dens_plot_0.125_3.5$Estplot, ma.rd_dens_plot_0.125_4$Estplot,
                     labels = c("0.125-BW, 3.0-Star", "0.125-BW, 3.5-Star", "0.125-BW, 4.0-Star"),
                     font.label = list(size = 12, color = "black"),
                     hjust = 0,
                     ncol = 1, nrow = 3)

q8.plot2 <- ggarrange(ma.rd_dens_plot_0.1_3$Estplot, ma.rd_dens_plot_0.1_3.5$Estplot, ma.rd_dens_plot_0.1_4$Estplot,
                      ma.rd_dens_plot_0.12_3$Estplot, ma.rd_dens_plot_0.12_3.5$Estplot, ma.rd_dens_plot_0.12_4$Estplot,
                      ma.rd_dens_plot_0.13_3$Estplot, ma.rd_dens_plot_0.13_3.5$Estplot, ma.rd_dens_plot_0.13_4$Estplot,
                      ma.rd_dens_plot_0.14_3$Estplot, ma.rd_dens_plot_0.14_3.5$Estplot, ma.rd_dens_plot_0.14_4$Estplot,
                      ma.rd_dens_plot_0.15_3$Estplot, ma.rd_dens_plot_0.15_3.5$Estplot, ma.rd_dens_plot_0.15_4$Estplot,
                      ncol = 3, nrow = 5)

### Q9: HMO and Part D status as Plan Characteristics --------------------------

q9.plot <- ggarrange(ma.rd_lp_0.125_3, ma.rd_lp_0.125_3.5, ma.rd_lp_0.125_4,
                     labels = c("0.125-BW, 3.0-Star", "0.125-BW, 3.5-Star", "0.125-BW, 4.0-Star"),
                     font.label = list(size = 12, color = "black"),
                     hjust = 0,
                     ncol = 1, nrow = 3)

## Question 6, 7, 8, 9 Alternative ---------------------------------------------

bandwidth <- c(0.125, 0.1, 0.12, 0.13, 0.14, 0.15)
starRating <- c(3.0, 3.5, 4.0)
for (i in 1:length(bandwidth)){
  for (j in 1:length(starRating)){
    cutoff <- starRating[j] - 0.25
    ma.rd.data <- ma.data.clean %>%
      filter(Star_Rating == starRating[j]-0.5 | Star_Rating == starRating[j]) %>%
      mutate(score = raw_rating - cutoff)
    
    ma.rd.dist <- ma.rd.data %>% ggplot(aes(x = raw_rating)) + geom_bar(width = .025) + theme_bw() +
      labs(x = "Running Variable", y = "Number of Plans", title = "Distribution of Raw Ratings") 
    
    ma.rd.plot <- rdplot(y = ma.rd.data$avg_enrollment, x = ma.rd.data$score,
                         binselect = "es",
                         title = "RD Plot: Enrollment", x.label = "Star Rating", y.label = "Enrollment", masspoints = "off")
    ma.rd.est <- rdrobust(y = ma.rd.data$avg_enrollment, x = ma.rd.data$score,
                          c = 0, h = bandwidth[i], p = 1,
                          kernel = "uniform", vce = "hc0", masspoints = "off")
    
    ma.rd.bin.avg <- as_tibble(ma.rd.plot$vars_bins)
    ma.rd.plot.bin <- ma.rd.bin.avg %>% ggplot(aes(x = rdplot_mean_x, y = rdplot_mean_y)) + 
      geom_point() + theme_bw() +
      geom_vline(aes(xintercept = 1), linetype = 'dashed') +
      scale_x_continuous(breaks = c(.5, 1.5), label = c("Untreated", "Treated")) +
      xlab("Running Variable") + ylab("Outcome")
    
    ma.rd_dens <- rddensity(ma.rd.data$score, c = 0)
    ma.rd_dens_plot <- rdplotdensity(ma.rd_dens, ma.rd.data$score)
    
    assign(paste0("ma.rd.est_", bandwidth[i], "_", starRating[j]), ma.rd.est)
    assign(paste0("ma.rd_dens_plot_", bandwidth[i], "_", starRating[j]), ma.rd_dens_plot)
  }
}

q6.data <- cbind(ma.rd.est_0.125_3$coef, ma.rd.est_0.125_3.5$coef, ma.rd.est_0.125_4$coef)
colnames(q6.data) <- c("3-Star", "3.5-Star", "4-Star")

q7.data <- cbind(ma.rd.est_0.1_3$coef, ma.rd.est_0.1_3.5$coef, ma.rd.est_0.1_4$coef,
                 ma.rd.est_0.12_3$coef, ma.rd.est_0.12_3.5$coef, ma.rd.est_0.12_4$coef,
                 ma.rd.est_0.13_3$coef, ma.rd.est_0.13_3.5$coef, ma.rd.est_0.13_4$coef,
                 ma.rd.est_0.14_3$coef, ma.rd.est_0.14_3.5$coef, ma.rd.est_0.14_4$coef,
                 ma.rd.est_0.15_3$coef, ma.rd.est_0.15_3.5$coef, ma.rd.est_0.15_4$coef)
colnames(q7.data) <- rep(c("3-Star", "3.5-Star", "4-Star"), 5)

ma.rd_match.dat <- matchit(treat ~ HMO + partd, data = ma.rd.data, method = NULL, distance = "mahalanobis")
ma.rd_lp <- love.plot(ma.rd_match.dat, stats = c("mean.diffs"), abs = TRUE, binary = "std", var.order = "unadjusted")

## Save data for markdown ------------------------------------------------------

rm(list=c("ma.data"))
save.image("Hwk4_workspace.Rdata")
