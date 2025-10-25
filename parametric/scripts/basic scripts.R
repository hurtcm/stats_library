library(tidyverse)
library(multcomp)
library(car) 
library(rstatix)

df <- data.frame(subj = paste("subject", 1:100, sep = "_"),
                 cont = rnorm(100, 7, 3),
                 treat1 = rnorm(100, 8.5, 3),
                 treat2 = rnorm(100, 9.25, 3))



df |> head()
df_long <- df |> 
    pivot_longer(
        col = 2:4,
        names_to = "treatment", 
        values_to = "effect"
    )
head(df_long)
model_df_aov <- aov(effect ~ treatment, df_long)
summary(model_df_aov)
par(mfrow = c(2, 2))
plot(model_df_aov, which = 1:4)
plot(model_df_aov, which = 5:6)

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






