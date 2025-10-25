#######################################################################
#####                  Penguin Data Analysis                      #####
##                                                                   ##
#######################################################################

## Analysis of Penguin populations on 3 different islands.

##  Libraries needed for work. 

library(lattice)
library(palmerpenguins)
library(tidyverse)

##  Data set to be used: Penguins

?penguins  #  Size measurements for adult foraging penguins near 
#               Palmer Station, Antarctic

glimpse(penguins) # Tibble of 8 x344
head(penguins)  
names(penguins)
View(penguins)
summary(penguins) # Provides a quick summary of the data 

# Multiple missing values are present as NAs. 
# The variable sex has 11 missing values, and the others have 2 
#   missing values each (bill_length and depth, flipper_length and 
#   body_mass).

?qqnorm() #  is a generic function the default method of which 
#    produces a normal QQ plot of the values, quantile-quantile 
#    plot which passes through the probs quantiles, by default 
#    the first and third quantiles.
?qqline # adds a line to a “theoretical”, by default normal, 
#    quantile-quantile plot which passes through the probs 
#    quantiles, by default the first and third quatiles.

# Removing NAs with na.omit() function and creating a new object.
summary(penguins) # Has NAs in measured variables. 
comp_penguins <- na.omit(penguins)
nrow(penguins) # Returns 344 rows
nrow(comp_penguins) # Returns 333 rows

names(comp_penguins)  # Variable names
glimpse(comp_penguins) # Tibble of 8 x 333 ; 11 rows removed for NAs

summary(comp_penguins)

# Continuing working with the data subset - comp_penguins. 
###   By species: Adelie, Chinstrap, and Gentoo. 
###     Examine the measured features of the penguins:
#         1)  bill_length_mm
#         2)  bill_depth_mm
#         3)  flipper_length_mm
#         4)  body_mass_g
names(comp_penguins)

##  Initial Visual inspection of the data by histogram, qqplots,
##    boxplot, and Shapiro.Wilks testing for normal data 
##    distributions.
?hist() # simple graphing tools.

##  Adelie penguin data set.

par(mfrow = c(2,2))
with(comp_penguins, tapply(bill_length_mm, species, qqnorm))




comp_Adelie <- comp_penguins |> 
  filter(species == "Adelie") 

comp_Adelie
summary(comp_penguins)

## 1) Adelie penguin bill_length_mm
mean(comp_Adelie$bill_length_mm)
median(comp_Adelie$bill_length_mm)



histogram(comp_Adelie$bill_length_mm, 
    main = "Adelie Bill Length", xlab = "Bill Length (mm)")
#
qqnorm(comp_Adelie$bill_length_mm, 
    main = "Normal Q-Q plot of Adelie Bill Length", 
    ylab = "Bill length Quantiles")
#
qqline(comp_Adelie$bill_length_mm)
# 
boxplot(comp_Adelie$bill_length_mm, main = "Adelie Bill Length",
        ylab = "Bill Length (mm)")#
shapiro.test(comp_Adelie$bill_length_mm) # Not significant so 
#       normal distribution. 
#

### 2)  bill_depth_mm 
mean(comp_Adelie$bill_depth_mm)
median(comp_Adelie$bill_depth_mm)
histogram(comp_Adelie$bill_depth_mm, 
          main = "Adelie Bill Depth", xlab = "Bill Depth (mm)")
#
qqnorm(comp_Adelie$bill_depth_mm, 
       main = "Normal Q-Q Plot of Adelie Bill Depth",
       ylab = "Bill Depth Quantiles")
qqline(comp_Adelie$bill_depth_mm)

# 
boxplot(comp_Adelie$bill_depth_mm, main = "Adelie Bill Depth", 
        ylab = "Bill Depth (mm)")


#
shapiro.test(comp_Adelie$bill_depth_mm) # Not significant so 
#       normal distribution.


### 3) Adelie flipper_length_mm
mean(comp_Adelie$flipper_length_mm)
median(comp_Adelie$flipper_length_mm)
histogram(comp_Adelie$flipper_length_mm, 
    main = "Adelie Flipper Length", xlab = "Flipper Length (mm)")

#
qqnorm(comp_Adelie$flipper_length_mm, 
    main = "Normal Q-Q Plot of Adelie Flipper Length",
    ylab = "Flipper Length Quantiles")
qqline(comp_Adelie$flipper_length_mm)

#
boxplot(comp_Adelie$flipper_length_mm, 
        main = "Adelie Flipper Length", 
        xlab = "Adelie Flipper Length")
#
shapiro.test(comp_Adelie$flipper_length_mm) # No significant so 
#       normal distribution.

### 4) Adelie body_mass_g
mean(comp_Adelie$body_mass_g)
median(comp_Adelie$body_mass_g)
histogram(comp_Adelie$body_mass_g, main = "Adelie Body Mass", 
          xlab = "Body Mass (g)")
#
qqnorm(comp_Adelie$body_mass_g, 
       main = "Normal Q-Q Plot of Adelie Body Mass", 
       ylab = "Body Mass Quantiles")
qqline(comp_Adelie$body_mass_g)

#
boxplot(comp_Adelie$body_mass_g, main = "Adelie Body Mass",
        ylab = "Body Mass (g)")
#
shapiro.test(comp_Adelie$body_mass_g) # Significant so not 
#       normal distribution. Slightly skewed to the left seen. 

#####################################################
##  Species Comparison of measured indices. 
# Using the complete data set, comp_penguins, that lacks NAs.
?histogram # For quick plots
#
par(mfrow = c(2,2))
comp_penguins
histogram(bill_length_mm ~ species, xlab = "Species",
          ylab = "Count", type = "count", 
          main = "Penguins Bill Length Across Species",
          data = comp_penguins)
histogram(bill_depth_mm ~ species, xlab = "Species", 
          ylab = "Count", type = "count", 
          main = "Penguins Bill Depth Across Species",
          data = comp_penguins)
histogram(flipper_length_mm ~ species, xlab = "Species",
          ylab = "Count", type = "count", 
          main = "Penguin Flipper Length Across Species",
          data = comp_penguins)
histogram(body_mass_g ~ species, xlab = "Species", 
          ylab = "Count", type = "count",
          main = "Penguins Body Mass Across Species",
          data = comp_penguins)

# Examination of Bill Length across species.
### 1) bill_length_mm 

comp_Adelie <- comp_penguins |> 
  filter(species == "Adelie") 

comp_Adelie

summary(comp_penguins)
head(comp_penguins)
# Count by species:
#   1) Adelie: 146
#   2)  Chinstrap: 68
#   3)  Gentoo

# For final analysis 65 samples from each species will be used 
#   for mean comparisons and anova. 


histogram(comp_Adelie$bill_length_mm) 
#
par(mfrow = c(1, 1))
qqnorm(comp_Adelie$bill_length_mm) # plots the bill_length_mm
#   against qqnorm distribution
qqline(comp_Adelie$bill_length_mm) # Draws line through qqnorm 
#               point on plot ==> Note points mainly along line.
#
boxplot(comp_Adelie$bill_length_mm)
#
?shapiro.test
shapiro.test(comp_Adelie$bill_length_mm) # Test for normal 
#   distribution. 


comp_Chinstrap <- penguins |> 
  na.omit() |> 
  filter(species == "Chinstrap")

comp_Chinstrap
summary(comp_Chinstrap)

qqnorm(comp_Chinstrap$bill_length_mm)
qqline(comp_Chinstrap$bill_length_mm)

histogram(comp_Chinstrap$bill_length_mm, xlab = "Chinstrap Penguins",
          ylabs = "Bill Length (mm)", main = "Chinstrap Bill Length")

boxplot(comp_Chinstrap$bill_length_mm, xlab = "Chinstrap Penguins", 
        ylab = "Bill Length (mm)",main = "Chinstrap Bill Length")

shapiro.test(comp_Chinstrap$bill_length_mm)

comp_Gentoo <- comp_penguins |> 
  filter(species == "Gentoo")

comp_Gentoo
summary(comp_Gentoo)

qqnorm(comp_Gentoo$bill_length_mm)
qqline(comp_Gentoo$bill_length_mm)

histogram(comp_Gentoo$bill_length_mm, xlab = "Gentoo Penguins", 
          ylab = "Bill Length (mm)", main = "Gentoo Bill Length",
          data = comp_Gentoo)

boxplot(comp_Gentoo$bill_length_mm, xlab = "Gentoo Penguins",
          ylab = "Bill Length (mm)", main = "Gentoo Bill Length",
          data = comp_Gentoo)

shapiro.test(comp_Gentoo$bill_length_mm) # Significant so Gentoo
#       bill length is not a normal distribution

# All three shapiro tests done in one line of code. 
with(comp_penguins, tapply(bill_length_mm, species, shapiro.test))

with(comp_penguins, tapply(bill_length_mm, species, qqnorm))


############## Unnecessary to create equal sets ##############
# Creates a subset of data with each species of penguins having
#   65 observations each. Slice_sample() will randomly pick 65 
#   observations from each data set.

comp_Adel_bill_len <- comp_penguins |> 
  filter(species == "Adelie") |> 
  select(bill_length_mm) |> 
  slice_sample(n = 65)

comp_Chin_bill_len <- comp_penguins |> 
  filter(species == "Chinstrap") |> 
  select(bill_length_mm) |> 
  slice_sample(n = 65)

comp_Gen_bill_len <- comp_penguins |> 
  filter(species == "Gentoo") |> 
  select(bill_length_mm) |> 
  slice_sample(n = 65)


species_bill_length <- 
  data.frame(comp_Adel_bill_len,
  comp_Chin_bill_len,comp_Gen_bill_len)


?rename
#
species_bill_length <- species_bill_length |> 
  rename(Adelie = bill_length_mm,
         Chinstrap = bill_length_mm.1,
         Gentoo = bill_length_mm.2)


glimpse(species_bill_length)
str(species_bill_length)

summary(species_bill_length)

#
?pivot_longer() #  "lengthens" data, increasing the number 
#    of rows and decreasing the number of columns.

species_bill_length <- pivot_longer(species_bill_length,
             cols = c(1:3),
             names_to = "species",
             values_to = "bill_length_mm"
             )
rm(species_bill_len)
head(species_bill_length)

species_bill_length$species <- factor(species_bill_length$species)
str(species_bill_length)

Adelie_temp<- comp_penguins |> 
    filter(species == "Adelie") |> 
    select(3:6)
apply(Adelie_temp, 2, mean)

Gentoo_temp<- comp_penguins |> 
    filter(species == "Gentoo") |> 
    select(3:6)
apply(Gentoo_temp, 2, mean)


Chinstrap_temp<- comp_penguins |> 
    filter(species == "Chinstrap") |> 
    select(3:6)
apply(Chinstrap_temp, 2, mean)
###############################################################

aov(bill_length_mm ~ species, 
                       data = comp_penguins)
#
summary(aov(bill_length_mm ~ species, data = comp_penguins))
#
aov_model_bill_len <- aov(bill_length_mm ~ species,
                          data = comp_penguins)

aov_model_bill_len
#
summary(aov_model_bill_len)

model.tables(aov_model_bill_len, type = "effect") # Returns difference of 
#   each factor mean - overall mean  
model.tables(aov_model_bill_len, type = "means")  # Returns the overall
#   mean and the mean of each factor. 
?aov()
#
?TukeyHSD()
#
TukeyHSD(aov_model_bill_len, conf.level = 0.95)  

comparisons_bill_len <- TukeyHSD(aov_model_bill_len,
                                 conf.level = 0.95)
plot(comparisons_bill_len)

compar_bill_len <- TukeyHSD((aov_model_bill_len))
plot(compar_bill_len)

aov(body_mass_g ~ species, comp_penguins)
#

summary(comp_penguins)


####################################################################
# Skipping the need for making a comp_penguins for data sub-setting. 
penguins |> 
  na.omit() |> 
  select(species, bill_length_mm) |> 
  filter(species == "Adelie") |> 
  slice_sample(n = 65) 
####################################################################

## Bill Length differences across species 
glimpse(comp_penguins)
summary(comp_penguins)

Adelie_65 <- penguins |> 
  na.omit() |> 
  filter(species == "Adelie") |> 
  slice_sample(n = 65)

Chinstrap_65 <- penguins |> 
  na.omit() |> 
  filter(species == "Chinstrap") |> 
  slice_sample(n = 65)

Gentoo_65 <- penguins |> 
  na.omit() |> 
  filter(species == "Gentoo") |> 
  slice_sample(n = 65)

Species_65 <- rbind(Adelie_65, Chinstrap_65, Gentoo_65)

###################################################################

Species_65 |> 
  filter(species == "Adelie") |> 
  select(species, bill_depth_mm) |> 
  rename(Adel_bill_dep = "bill_depth_mm")

####################################################################

Species_65 |> 
  filter(species == "Adelie") |> 
  ggplot(aes(x = bill_depth_mm)) +
  geom_histogram(binwidth = 0.25) +
  labs(
    title = "Adelie Penguin's Bill Depth",
    x = "Length (mm)", y = "Counts")

Species_65 |> 
  filter(species == "Chinstrap") |> 
  ggplot(aes(x = bill_depth_mm)) +
  geom_histogram(binwidth = 0.2) +
  labs(
    title = "Chinstrap Penguin's Bill Depth",
    x = "length (mm)", y = "Counts")

Species_65 |> 
  filter(species == "Gentoo") |> 
  ggplot(aes(x = bill_depth_mm)) +
  geom_histogram()+
  labs(
    title = "Gentoo Penguin's Bill Depth",
    x = "Length (mm)", y = "Counts"
  )

Species_65 |> 
  ggplot(aes(x = species, y = bill_depth_mm, fill = species)) +
  geom_boxplot()+
  labs(
    title = "Penguin Species Bill Depth ", 
    x = "Species", y = "Bill Depth (mm)"
  )

################################################################
#####             Further analysis Two Way Anova
###
#
# Two way anova: comp_penguin$bill_length::comp_penguins$sex 

library(car)
leveneTest(comp_penguins$bill_length_mm ~ comp_penguins$species *
               comp_penguins$sex)

tw_aov_bill_length <- 
    aov(comp_penguins$bill_length_mm ~ comp_penguins$species * 
        comp_penguins$sex)
summary(tw_aov_bill_length)

tw_aov_no_ia_bill_length <- 
    aov(comp_penguins$bill_length_mm ~ comp_penguins$species + 
            comp_penguins$sex)
summary(tw_aov_no_ia_bill_length)

TukeyHSD(tw_aov_bill_length)
plot(TukeyHSD(tw_aov_bill_length))

TukeyHSD(tw_aov_no_ia_bill_length)
plot(TukeyHSD(tw_aov_no_ia_bill_length))




#################################################################

tw_aov_bill_depth <- 
    aov(comp_penguins$bill_depth_mm ~ comp_penguins$species * 
            comp_penguins$sex)
summary(tw_aov_bill_depth)

tw_aov_no_ia_bill_depth <- 
    aov(comp_penguins$bill_depth_mm ~ comp_penguins$species + 
            comp_penguins$sex)
summary(tw_aov_no_ia_bill_depth)
plot(tw_aov_no_ia_bill_depth, which = 2)
plot(tw_aov_no_ia_bill_depth, which = 3)

TukeyHSD(tw_aov_bill_depth)
plot(TukeyHSD(tw_aov_bill_depth))

TukeyHSD(tw_aov_no_ia_bill_depth)

plot(TukeyHSD(tw_aov_no_ia_bill_depth))





#################################################################

tw_aov_body_mass <- 
    aov(comp_penguins$body_mass_g ~ comp_penguins$species * 
            comp_penguins$sex)
summary(tw_aov_body_mass)

tw_aov_no_ia_bill_depth <- 
    aov(comp_penguins$body_mass_g ~ comp_penguins$species + 
            comp_penguins$sex)
summary(tw_aov_no_ia_bill_depth)

TukeyHSD(tw_aov_body_mass)
plot(TukeyHSD(tw_aov_body_mass))

TukeyHSD(tw_aov_no_ia_bill_depth)

plot(TukeyHSD(tw_aov_no_ia_bill_depth))


   