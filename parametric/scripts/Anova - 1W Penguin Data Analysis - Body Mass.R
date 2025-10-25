################################################################
#####                 Penguin Data Analysis                #####
##                          Body Mass                         ##
#                                                              #
################################################################
#
# Analysis of Body Mass across species (Adelie, Chinstrap, 
#       #& Gentoo)
#

#   Clear environment and memory
rm(list = ls(all.names = TRUE))
gc()

##  Libraries for analysis
library(lattice)
library(palmerpenguins)
library(tidyverse)
#
# The data on penguins comes from palmer penguins library
#
?penguins # Size measurements for adult foraging penguins near 
#   Palmer Station, Antarctica. Includes measurements for penguin
#   species, island in Palmer Archipelago, size (flipper length, 
#   body mass, bill dimensions), and sex.
#
penguins # 344 observations x 8 variables data frame.
names(penguins) # Variables: species, island, bill_length_mm, 
#   bill_depth_mm, flipper_length_mm, body_mass_g, sex, and year.   


##  Descriptive analysis of of the measurements of penguins, 
summary(penguins) # Count of penguin species:  
# Adelie   :152    
# Chinstrap: 68  
# Gentoo   :124

##  Data set is unbalanced in number of penguins per species.
#     To make balanced 65 random samples per species could be 
#     used after removing any missing values, NAs. 
#
##  Addressing missing values, NAs, present in the data set:

penguins[rowSums(is.na(penguins)) > 0, ] # Indexing method to
#   identify rows with missing values.
####################################################################
na.omit(penguins) # Returns data that have rows removed with 
#                     missing values. 
# na.fail(penguins) # Error occurs if there is missing values.  
#                   Quick way of checking if data set is complete.
# na.pass(penguins) # Returns the data set unchanged. 
##################################################################

##  Evaluation of missing values.

na_penguins <- penguins[rowSums(is.na(penguins)) > 0, ]
View(na_penguins) 
head(na_penguins)
# 
#     6 Adelie penguins have missing values (5 lack sex
#         determinations and 1 lacks all measurements). 
#     5 Gentoo penguins (4 sex determination and 1 lacks 
#       all measurements).

# Complete data set without NAs.
mv_penguins <- penguins |> 
  na.omit()
mv_penguins

## Distributions of Body Mass across penguin species.
# Histogram of each species body masses measured
mv_penguins |> 
  ggplot(aes(x = body_mass_g, fill = species)) +
  geom_histogram(binwidth = 250) +
  facet_wrap(~ species)+ 
  labs(
    x = "Body Mass (mm)",
    y = " Counts",
    title = "Body Mass of Penguin Species")

# Box plot of each penguin species body mass distribution.
#
mv_penguins |> 
  ggplot(aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() + 
  labs(
    x = "Species",
    y = "Body Mass (mm)", 
    title = "Penguin Species Body Mass ")

# Normal distribution test of penguins species body mass  
# Graphical analysis using Q-Q normal plots.
# Need to create separate species subsets. 
# 
par(mfrow = c(2,2))
mv_Adel <- mv_penguins |> 
  filter(species == "Adelie")
mv_Adel

mv_Chin <- mv_penguins |> 
  filter(species == "Chinstrap")
mv_Chin

mv_Gent <- mv_penguins |> 
  filter(species == "Gentoo")
mv_Gent

par(mfrow = c(2,2))
with(mv_penguins, tapply(body_mass_g, species, qqnorm))
mv_penguins$species
unique(mv_penguins$species)
# qqnorm() function used to test normal distribution
# qqline() function used to draw a line through point 
#   of qqnorm plot.
# Adelie penguins Q-Q Normal Plot
par(mfrow = c(2,2))
qqnorm(mv_Adel$body_mass_g, 
       main = "Adelie Penguins Body Mass: Q-Q Normal Plot",
       ylab = "Body Mass (g)")
qqline(mv_Adel$body_mass_g)

# Chinstrap penguins Q-Q Normal Plot
qqnorm(mv_Chin$body_mass_g,
       main = "Chinstrap Penguins Body Mass: Q-Q Normal Plot",
       ylab = "Body Mass (g)")
qqline(mv_Chin$body_mass_g)

# Gentoo penguins Q-Q Normal Plot
qqnorm(mv_Gent$body_mass_g, 
       main = "Gentoo Penguins Body Mass: Q-Q Normal Plot",
       ylab = "Body Mass(g)")
qqline(mv_Gent$body_mass_g)

## Normal distribution statistical determination of Penguin 
#   species body mass
# Using the Shapiro-Wilk's Test.
# All three species run simultaneously using the with() and 
#   tapply() functions.
with(mv_penguins, tapply(body_mass_g, species, shapiro.test))

# Results: Both Chinstrap and Gentoo are not significantly 
#   different than the normal distributions.
#   However, the Adelie penguins are barely significantly 
#     different at p = 0.04232

# Will still use since anova is fairly robust test.
# Note: To ensure that the groups have comparable variances,
#   will use the Levene's test of variance across groups. 
# Require the car package to be loadedl
library(car)
?leveneTest() # test for homogeneity of variance across groups.
# Ho: Null hypothesis - variances are equal between  groups
#   (p > 0.5, not significant test result)
# H1: Alternative Hypothesis - variances are different between 
#   groups (p < 0.05, signficant test result)
leveneTest(body_mass_g ~ species, data =  mv_penguins)
# Returned a p value of 0.006367, significantly different groups.
# Test with aov() and older anova.test which corrects for unequal 
#   variances. 

?aov
aov_mv_peng <- aov(body_mass_g ~ species, mv_penguins)
aov_mv_peng
summary(aov_mv_peng)
TukeyHSD(aov_mv_peng)
tukey_mv_peng <- TukeyHSD((aov_mv_peng))

par(mfrow = c(1,1))
plot(tukey_mv_peng)

library(effsize)
?cohenD.test?
  
  
  
# Verification of effect using older one way anova test
#   correcting for unequal variances between groups. 

?oneway.test
oneway.test(body_mass_g ~ species, mv_penguins)
oneway.test(body_mass_g ~ species, mv_penguins, var.equal = FALSE)
oneway.test(body_mass_g ~ species, mv_penguins, var.equal = TRUE)


?TukeyHSD

library(effsize)

?cohen.d

cohen.d(mv_Adel$body_mass_g, mv_Chin$body_mass_g)

cohen.d(mv_Adel$body_mass_g, mv_Gent$body_mass_g)

cohen.d(mv_Chin$body_mass_g, mv_Gent$body_mass_g)
        
#
summary(mv_penguins)
#
sum_stats <- cbind(with(mv_penguins,
      tapply(body_mass_g, species, FUN = length)), 
      with(mv_penguins, tapply(body_mass_g, species, FUN = median)),
      with(mv_penguins, tapply(body_mass_g, species, FUN = mean)),
      with(mv_penguins, tapply(body_mass_g, species, FUN = sd)))

#
sum_stats <- data.frame(sum_stats)
str(sum_stats)

colnames(sum_stats) <- c("n", "medaian", "mean", "sd")
sum_stats

sum_stats$se <- sum_stats$sd/sqrt(sum_stats$n)
sum_stats

sum_stats <- rownames_to_column(sum_stats, var = "species")
sum_stats

#
sum_stats |> 
    ggplot() +
    geom_col(aes(species, mean), 
            fill = "skyblue")  +
    geom_errorbar(aes(species, ymin = mean - sd, 
            ymax = mean +sd), size = 0.8, width = 0.2) + 
    coord_cartesian(ylim = c(0,7000))

#
mv_penguins |> 
    ggplot(aes(species, body_mass_g, fill = species)) +
    geom_boxplot()

#   Non-parametric comparison given Levine's test failed 
#       (found to be significantly different than normal).

kruskal.test(body_mass_g ~ species, mv_penguins)
#   Returns a Chi-Squared of 212.09, df = 2 p < 2.2e=16. 
#       The same value as the one-way anova.












