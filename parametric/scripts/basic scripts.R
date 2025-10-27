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

#   Create a data set (data frame) for analysis
df <- data.frame(subj = paste("subject", 1:30, sep = "_"),
                 cont = rnorm(30, 7, 3),
                 treat1 = rnorm(30, 8.5, 3),
                 treat2 = rnorm(30, 9.25, 3))
df
str(df)

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

#   Summary Stats using tidy data set
df_long|> 
    group_by(treatment) |> 
        summarize(
            count = n(),
            mean_val = mean (effect, na.rm = TRUE), 
            median_val = median(effect, na.rm = TRUE), 
            sd_val  = sd(effect, na.rm = TRUE),
            se_val  = sd_val/sqrt(count)
     )

#   Newer method using list() functions to reduce the number
#       of line of code 
df_long |>  
    group_by(treatment) |> 
    summarize(across(effect,
        list(count = length, mean_val = mean, sd_val = sd)
    ))
#   The across() allows for multiple dependent values  to be
#       analyzed. Note it labels the dependent values to be 
#       distinguished.
###

# Graphical presentation of the data




#   One-Way Anova analysis of data 
model_df_aov <- aov(effect ~ treatment, df_long)
summary(model_df_aov)

#   Diagnostic graphs examining of the model analysis 
par(mfrow = c(2, 2))
par(mar = c(3, 2,2,2))
plot(model_df_aov, which = 1:4)

#   Plot 1 - Residuals vs Fitted Residuals: homoscedasticity ~
#       relationship between response and predictors.Points 
#       should be randomly distributed around the zero line and
#       fit should be at zero (horizontal).

#   Plot2 - Q-Q Plot: normality ~ Points fit the line for 
#       quantile vs 




plot(model_df_aov, which = 5:6)


#   Margin default is c(5.1, 4.1, 4.1, 2.1).
# Create data set for standard two way anova 



# ploting methods:
#interaction.plot(independent variable1, independent variable 2, 
#                dependent variable, main = "###", xlab = "##",
#                  ylab = " ", trace.label = "### treatment")

#
#

#   Data creation for two-way factorial ANOVA
df1 <- data.frame(subj = (rep(paste("subject", 1:50, sep = "_"),
                             time = 2)),
                 conds = rep(c("cond1", "cond2"), c(50, 50)),
                 cont = rnorm(100, 7, 3),
                 treat1 = rnorm(100, 8.5, 3),
                 treat2 = rnorm(100, 9.25, 3))
df1 |> head()

df_long1 <- pivot_longer(df1, 
    cols = 3:5,
    names_to = "treatments", 
    values_to = "scores"
)
head(df_long1)
ggplot(data = df_long1, aes(
    x = treatments, y = scores, fill = conds, )) + 
    geom_boxplot()
tail(df1)
model_aov1 <- aov(scores ~ conds * treatments + 
        Error(subj/(conds * treatments)), df_long1)

summary(model_aov1)
str(model_aov1)

model_aov1$`subj:conds:treatments`[1]

rep(1:4, each = 2) # Returns: 1 1 2 2 3 3 4 4
rep(1:4, 2) # Retuns: 1 2 3 4 1 2 3 4
rep(1:4, c(2, 2, 2, 2)) # Returns:  1 1 2 2 3 3 4 4
rep(1:4, c(2,3,4, 5)) # Returns: 1 1 2 2 2 3 3 3 3 4 4 4 4 4
rep(1:4, each = 2, times = 2)
                    # Returns: 1 1 2 2 3 3 4 4 1 1 2 2 3 3 4 4
rep(1:4, each = 2, length.out = 4) 
                    # Returns only the 4 digits 1 1 2 2

rep(1, 40*(1-.8)) # Returns:  1 1 1 1 1 1 1 ; 7 items returned not 8
rep(1, 40*(1-.8)+1e-7) # Returns: 1 1 1 1 1 1 1 1; 8 items.       
######

df <- matrix(rnorm(200, 10, 5), nrow = 50)
df <- as.data.frame(df)
class(df)
summary(df)
apply(df, 2, sd)
colnames(df) <- c("cont", "t1", "t2", "t3")
df
boxplot(df)

df_long <- pivot_longer(df,
             cols = 1:4,
             names_to = "cond",
             values_to = "effect")
df_long$cond <- as.factor(df_long$cond)

ggplot(df_long, aes(x = cond, y = effect)) +
    geom_boxplot(aes(fill = cond)) +
    theme_minimal()

model_aov <- aov(effect ~ cond, df_long)
summary(model_aov)
hist(df_long$effect)
hist(df$t3)
plot(model_aov, which = 3)
Tuk_mod_aov <- TukeyHSD(model_aov)
Tuk_mod_aov

plot(Tuk_mod_aov)


##  Dunnett test
glht(model_aov, linfct = mcp(cond = "Dunnett"))
summary(glht(model_aov, linfct = mcp(cond = "Dunnett")))
plot(glht(model_aov, linfct = mcp(cond = "Dunnett")))


################################################################

set.seed(123)
subject <- factor(1:10)
condition <- factor(rep(c("A", "B", "C"), each = 10))
score <- c(rnorm(10, mean = 15), rnorm(10, mean = 16), 
           rnorm(10, mean = 17))

# Creating a data frame
data <- data.frame(subject = rep(subject, 3), 
                   condition = condition, score = score)

paste("subject", 1:10, sep =  "_")






