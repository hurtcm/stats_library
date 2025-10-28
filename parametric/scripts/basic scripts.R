################################################################
#####           Run Through of Basic statistics            #####
###

#   Clear memory and environment
rm(list = ls(all.names = TRUE))
gc()

#   Libraries needed
library(tidyverse)
library(multcomp)
library(car) 
library(rstatix)

#   Set the random number generator
set.seed(318)

#   Create a data set (data frame) for analysis
df <- data.frame(subj = paste("subject", 1:30, sep = "_"),
                 cont = rnorm(30, 7, 3),
                 treat1 = rnorm(30, 8.5, 3),
                 treat2 = rnorm(30, 9.25, 3))
df
str(df)

##  R base - Descriptive statistic method      
df_n <- apply(df[ , 2:4], 2, length)
df_mean <- apply(df[ , 2:4], 2, mean)
df_median <- apply(df[ ,2:4], 2 ,median)
df_sd <- apply(df[ , 2:4], 2, sd)
df_se <- (apply(df[ , 2:4], 2, sd)/sqrt(df_n))
rbind(df_n, df_mean, df_median, df_sd,df_se)
apply(df[ , 2:4], 2, summary)

#   Wrangle data to tidy format
df |> head()
df_long <- df |> 
    pivot_longer(
        col = 2:4,
        names_to = "treatment", 
        values_to = "effect"
    )
head(df_long)

##   R Tidy descriptive statistic method.
df_long|> 
    group_by(treatment) |> 
        summarize(
            count = n(),
            mean_val = mean (effect, na.rm = TRUE), 
            median_val = median(effect, na.rm = TRUE), 
            sd_val  = sd(effect, na.rm = TRUE),
            se_val  = sd_val/sqrt(count)
     )

##   Newer Alternative R Tidy descriptive statistic method. 
#       The method uses across and  list() functions to reduce 
#       the number of lines of code 
df_long |>  
    group_by(treatment) |> 
    summarize(across(effect,
        list(count = length, mean_val = mean, sd_val = sd)
    ))
##  Note: The across() function allows for multiple dependent 
#       values to be analyzed. Note it also labels the dependent 
#       values to distinguished between dependent variables.
###

### Graphical presentation of data
##  Boxplot
df_long |> 
    ggplot(aes(x = treatment, y = effect, fill = treatment)) +
    geom_boxplot() +
    theme_minimal()

##  Histogram (composite)
df_long |> 
    ggplot(aes(x = effect, color = treatment, fill = treatment))+
    geom_histogram(alpha = 0.3, bin = 20) +
    theme_minimal()

##  Density plot (Composite)
df_long |> 
    ggplot(aes(x = effect, color = treatment, fill = treatment))+
    geom_density(alpha = 0.3) +
    theme_minimal()


#   One-Way Anova analysis - Test statistic of data 
model_df_aov <- aov(effect ~ treatment, df_long)
summary(model_df_aov)

#   Diagnostic graphs examining of the model analysis 
par(mfrow = c(2, 2))
par(mar = c(3, 2, 2, 2))
plot(model_df_aov, which = 1:4)
plot(model_df_aov, which = 5:6)


#   Reset margins to default settings
#   Margin default is c(5.1, 4.1, 4.1, 2.1).
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mfrow = c(1,1))

#   Post-Hoc comparisons between groups
tukey_model <- TukeyHSD(model_df_aov)
tukey_model

plot(tukey_model, las = 2)


################################################################
#   Plot 1 - Residuals vs Fitted Residuals: homoscedasticity ~
#       relationship between response and predictors.Points 
#       should be randomly distributed around the zero line and
#       fit should be at zero (horizontal).

#   Plot2 - Q-Q Plot: normality ~ Points fit the line for 
#       quantile vs 



##################################################################

head(df_long)
################################################################
# plotting methods:
#interaction.plot(independent variable1, independent variable 2, 
#                dependent variable, main = "###", xlab = "##",
#                  ylab = " ", trace.label = "### treatment")
################################################################
#
#
###                 Two-Way ANOVA

#   Data creation for two-way factorial ANOVA
df1 <- data.frame(subj = (rep(paste("subject", 1:50, sep = "_"),
                             time = 2)),
                 conds = rep(c("cond1", "cond2"), c(50, 50)),
                 cont  = rnorm(100, 7, 2),
                 treat1 = rnorm(100, 8.5, 2.5),
                 treat2 = rnorm(100, 10.5, 3)
                 )

df1 |> head()
df1 |> tail()

df_long1 <- pivot_longer(df1, 
    cols = 3:5,
    names_to = "treatments", 
    values_to = "scores"
)

head(df_long1)
tail(df_long1)

ggplot(data = df_long1, aes(
    x = treatments, y = scores, fill = conds)) + 
    geom_boxplot() +
    theme_minimal()

###     Two-Way ANOVA (Test with interaction)
model_aov1 <- aov(scores ~ conds * treatments + 
        Error(subj/(conds * treatments)), df_long1)

summary(model_aov1)
str(model_aov1)
model_aov1$`subj:conds:treatments`[1]

ggplot(data = df_long1, aes(x = treatments, y = scores), 
       fill = conds) +
    geom_bar(stats = "identity")
    

ggplot(data = df_long1, aes(x = treatments, y = scores)) +
    geom_col(aes(fill = conds)) +
    theme_minimal()


###  Dunnett test
#glht(model_aov, linfct = mcp(cond = "Dunnett"))
#summary(glht(model_aov, linfct = mcp(cond = "Dunnett")))
#plot(glht(model_aov, linfct = mcp(cond = "Dunnett")))






################################################################

