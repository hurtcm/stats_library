################################################################
#####             Repeated Measures 2 way Anova            #####
###
##  Author: Carl Hurt
##  Date: 06/28/2025
#
################################################################
#

#   Clear environment and memory
rm(list = ls(all.names = TRUE))
gc()


#   Libraries RequIRed
library(rstatix) # For Anova and sphericity test
library(tidyverse) # For data wrangling and cleaning
library(ggpubr) # For graphics showing significants labeling
library(datarium) # For the data set selfesteem2

#   Load Data
selfesteem2
str(selfesteem2)


#   Clean Data - Check for NAs and blank cells ("")
selfesteem2[rowSums(is.na(selfesteem2)), ] # No NAs
which(is.na(selfesteem2)) # No NAs

selfesteem2[ , 3:5] == " " # Returns all False for space
which(selfesteem2[ , 3:5] == " ") # Returns 0 space found

selfesteem2[ , 3:5] == "" # Returns all false for blanks
which(selfesteem2[ , 3:5] == "") # Returns 0

#   Wrangle Data - Convert to tidy format. 
selfesteem2 |> head()
selfesteem2 |> sample_n_by(treatment, 2) # Returns two 
#   treatments 
#   Convert to long format
selfesteem2_lg <- selfesteem2 |> pivot_longer(
    cols = 3:5,
    names_to = "time",
    values_to = "score"
)
selfesteem2_lg

#   Descriptive Statistics
selfesteem2_lg |> 
    group_by(treatment) |> 
    summarize(
        n = n(),
        mean = mean(score),
        median = median(score),
        sd = sd(score),
        se = sd/sqrt(n),
    )
selfesteem2_lg |> 
    ungroup()


selfesteem2_lg |> 
    group_by(treatment, time) |>
    summarize(
    n = n(),
    mean = mean(score),
    median = median(score),
    sd = sd(score),
    se = sd/sqrt(n) 
    )
selfesteem2_lg |> 
    ungroup()

################################################################
####    R base alternative to the above. Consider sub-setting it 
####    Need to fix: Descriptive stat for each treatment*time
selfesteem2_lg |> tail()
index_ctr <- selfesteem2_lg[ , "treatment" ] == "ctr" 
index_Diet <- selfesteem2_lg[ , "treatment"] == "Diet"
summary(selfesteem2_lg$score[index_ctr])

################################################################
#

#   Visualization of data
selfesteem2_lg |> 
    ggplot(aes(time, score, fill = treatment)) +
    geom_boxplot()
#   Alternative using ggboxplot() function. 
bxplt <- ggboxplot(selfesteem2_lg, x = "time", y = "score", 
                 add = "point")
bxplt


#   Identify outliers
selfesteem2_lg |> 
    group_by(treatment, time) |> 
    identify_outliers(score)|> 
    ungroup()
#   No outliers identifed.

#   Normality Evaluation
#   Shapiro test
selfesteem2_lg |> 
    group_by(treatment, time) |> 
    shapiro_test(score) # All but control t1 are normal. 

#   QQplots for Normality 
ggqqplot(selfesteem2_lg, "score", ggtheme = theme_bw()) +
    facet_grid(time ~ treatment, labeller = "label_both")

#   Two way anova analysis using aov() function
reslt_aov <-
    aov(score ~ treatment * time + Error(id/(treatment*time)), 
    data = selfesteem2_lg)
reslt_aov

#   Summary of two way anova summarize 
summary(reslt_aov) # standard summary
anova_summary(reslt_aov, detailed = TRUE) # rstatix 
#   anova_summary returns sphericity test if significant. 

get_anova_table(reslt_aov) # rstatix detailed results table
#   without sphericity results

#   Two way anova using anova_test from rstatix package. 
reslts_aov1 <- anova_test(score ~ treatment * time +
                   Error(id/(treatment*time)), 
                   data = selfesteem2_lg)

reslts_aov1 # Provides the anova results with sphericity test 
#   results as default. 

summary(reslts_aov1) # Returns anova output structure without
#       anova analysis results. It is not compatible with 
#       rstatix::anova_test().

get_anova_table(reslts_aov1) # rstatix detailed results table.
#       as a short summary 

################################################################
##  Anova results show significant interaction between the 
#       time and treatment factors on scores. 
#   The Maulchy W test for sphericity shows that time alone has 
#       lacks some sphericity
#   Post Anova analysis test will make pairwise comparisons. 
#   Approached by doing two seperate one-way anovas cotnroling 
#   for variable one second time 

#   Examine the effect of treatment on score while controlling 
#       for each time (point). Then the reverse is examining 
#       the effect of time on score while controlling for each
#       treatment (level).

#   One-way anova with repeated measures with bonferroni
#       adjustment added.
#   Effect of treatment while controlling time.
one_way_trt <- selfesteem2_lg |>
    group_by(time) |> 
    anova_test(dv =score, wid = id, within = treatment) |> 
    get_anova_table() |> 
    adjust_pvalue(method = "bonferroni")
 
#   Pairwise t-test with rstatix::pairwise_t_test() function.
#   Examining the effect of treatment at different time points.
pr_ws_trt <- selfesteem2_lg |> 
    group_by(time) |> 
    pairwise_t_test(score ~ treatment, paired = TRUE,
                    p.adjust.method = "bonferroni")


#   One-way anova with repeated measures with bonferroni
#       adjustment added.
#   Effect of time while controlling for treatment at different
#       levels. 
summary(aov(score ~ time + Error(id/time), data = selfesteem2_lg))

one_way_time <- selfesteem2_lg |> 
    group_by(treatment) |> 
    anova_test(dv = score, wid = id, within = time) |> 
    get_anova_table() |> 
    adjust_pvalue(method = "bonferroni")
one_way_time

### Using R base formula works the same. 
one_way_time <- selfesteem2_lg |> 
    group_by(treatment) |> 
    anova_test(score ~ time + Error(id/time)) |> 
    get_anova_table() |> 
    adjust_pvalue(method = "bonferroni")
one_way_time 

pr_ws_time <- selfesteem2_lg |> 
    group_by(treatment) |> 
    pairwise_t_test(score ~ time, paired = TRUE,
                    p.adjust.method = "bonferroni")
pr_ws_time |> view()

### Visualize Results using ggpubr package.
pr_ws_trt <- pr_ws_trt %>% add_xy_position(x = "time")
pr_ws_trt |> view()

bxplt + 
    stat_pvalue_manual(pr_ws_trt, tip.length = 0, 
                       hide.ns = TRUE) +
    labs(
        subtitle = get_test_label(reslts_aov1, detailed = TRUE),
        caption = get_pwc_label(pr_ws_trt)
    )



