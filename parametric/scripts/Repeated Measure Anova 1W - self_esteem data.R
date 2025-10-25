################################################################
#####          Anova -  1 way Repeated Measures            #####
###
##  Author: Carl M. Hurt
##  Date: 06/27/2025
################################################################
#

#   Clear enviroment and memory
rm(list = ls(all.names = TRUE))
gc()

#   Libraries Required:
library(rstatix) # Anova analysis and sphericity
library(tidyverse) # Data wrangling and presentation
library(ggpubr) # Creates publication quality figures 
library(datarium) # Contains the data set datarium

#   Data set: selfesteem
?selfesteem #  10 individuals' self-esteem score on three 
#       time points during a specific diet to determine 
#       whether their self-esteem improved.
str(selfesteem)
selfesteem

#   Data wrangling - Make data tidy
self_estm <- selfesteem |> 
    pivot_longer(cols = c(2:4),
                 names_to = "time", 
                 values_to = "scores")

#    Descriptive Analysis
self_estm |> 
    group_by(time) |> 
    summarize(
        n = n(),
        mean = mean(scores),
        sd = sd(scores),
        se = sd(scores)/n
    )

#   Boxplot of data set
self_estm |> 
    ggplot(aes(x = time, y = scores, fill = time)) +
    geom_boxplot() + 
    coord_cartesian(ylim = c(0,11)) +
    theme_bw()

#   Identify outliers
self_estm |> 
    group_by(time) |> 
    identify_outliers(scores)
#   Alternatively, using r base function apply
apply(selfesteem[ , c("t1", "t2", "t3")], 2, shapiro_test)

#   No extreme outliers found.

#   QQPlot of data set.
par(mfrow = c(2,2))
qqnorm(selfesteem$t1, conf.level = 0.95)
qqline(selfesteem$t1)

qqnorm(selfesteem$t2)
qqline(selfesteem$t2)

qqnorm(selfesteem$t3)
qqline(selfesteem$t3)
par(mfrow = c(1,1))

#   Repeted measure two way anova 
aov_self_estm <- self_estm |> 
    anova_test(scores ~ time + Error(id/(time)))

aov_self_estm # 

get_anova_table(aov_self_estm)


pwt_t <- self_estm |> 
    pairwise_t_test(
        scores ~ time,
        paired = TRUE, 
        p.adjust.method = "bonferroni"
    )
pwt_t

#   Graphical Representation - Simple Boxplot
self_estm |> 
    ggplot(aes(time, scores, fill = time)) +
    geom_boxplot()  + 
    theme_bw()


#   ggpubr generated boxplot.
bxplt <- ggboxplot(self_estm, x = "time", y = "scores", 
                   add = "point")
bxplt

#   Add stats and labels to boxplot  
pwt_t <- pwt_t |> add_xy_position(x = "time") 

bxplt + 
    stat_pvalue_manual(pwt_t) +
    labs(
        subtitle = get_test_label(aov_self_estm, 
        detailed = TRUE),
        caption = get_pwt_label(pwt_t)
    )


