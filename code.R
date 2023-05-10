# Call all libraries
library(tidyverse)     
library(readxl)        
library(dplyr)        
library(car)          
library(multcomp)     
library(readr)        
library(ggplot2)      
library(here)         
library(patchwork)    
library(ggpubr)       

## Data retrieval ##

# Site of data in Github project repo
url <- "https://github.com/molly1902/psy_6422/raw/main/s2.xlsx"


# Download the Excel file from GitHub
download.file(url, destfile = "s2.xlsx", mode = "wb")


# Read in the Excel file
s2 <- read_excel("s2.xlsx")

## Data wrangling ##

# Remove the columns not used in analysis
# Leaves father and participant age in years, 3 song conditions
st2 <- subset(s2, select=-c(aged, mom, female, root, bird, political, quarterback, olddays, feelold, computer, diner, cond))


# The 3 song conditions (potato, when64, kalimba) are coded '1' if listened to
# and 0 if not listened to
# Replace all '1' values with the participant age value in the same row
# This also retains corresponding father age per song
stu2 <- st2 %>%
  mutate(potato = replace(potato, potato == "1", aged365[potato=="1"])) %>% 
  mutate(when64 = replace(when64, when64 == "1", aged365[when64=="1"])) %>%
  mutate(kalimba = replace(kalimba, kalimba == "1", aged365[kalimba=="1"])) %>% 
  mutate(dad)


#Remove all 0 values from song columns
new1 <- stu2 %>%
  filter(!if_any(starts_with("p"), ~ . == 0))

new2 <- stu2 %>% 
  filter(!if_any(starts_with("w"), ~ . == 0))

new3 <- stu2 %>% 
  filter(!if_any(starts_with("k"), ~ . == 0))


# Index to further remove unwanted columns
new1pot <- new1[,-3:-5]

new2when <- new2[,-2,-4,-5]

new3kal <- new3[,-2,-3,-5]


# Create new column song type with the same number of values
# as are in the existing column, for each song
song1 <- c("Potato", "Potato", "Potato", "Potato", "Potato", 
           "Potato", "Potato", "Potato", "Potato", "Potato", 
           "Potato", "Potato", "Potato", "Potato")

song2 <- c("When", "When", "When", "When", "When", 
           "When", "When", "When", "When", "When", 
           "When")

song3 <- c("Kalimba", "Kalimba", "Kalimba", "Kalimba",
           "Kalimba", "Kalimba", "Kalimba", "Kalimba",
           "Kalimba")

# Add new column to each existing corresponding data frame 
# containing participant age and father age
newpot <- new1pot %>% 
  mutate(song1)

newwhen <- new2when %>% 
  mutate(song2)

newkal <- new3kal %>% 
  mutate(song3)


# Create a data frame containing columns: father age, participant age, song type
datafra <- data.frame(dadage = c(newpot$dad, newwhen$dad, newkal$dad),
                      pptage = c(newpot$potato, newwhen$when64, newkal$kalimba),
                      song = rep(c(newpot$song1, newwhen$song2, newkal$song3)))


# Convert song type column from a character to a factor for statistical analyses
factor_song <- as.factor(datafra$song)
class(factor_song)


# Replace the character column with the new factor column in a data frame
df1 <- datafra
df1$factor_song <- factor_song
df1 <- df1[, -3]


## Statistical analyses ##

# Descriptive statistics for participant and father age by song
descriptive <- df1 %>%
  group_by(factor_song) %>%
  summarise(mean_age = mean(pptage),
            sd_age = sd(pptage),
            mean_dad = mean(dadage),
            sd_dad = sd(dadage))
summary(descriptive)


# ANCOVA model
# Response variable = participant age
# Group variable = song 
# Covariate = father age
ancova_ppt <- aov(pptage ~ factor_song + dadage, data = df1)
ancova_pptage <- Anova(ancova_ppt, type="III")
summary(ancova_ppt)
summary(ancova_pptage)



# Test for Homogeneity
leveneTest(pptage~factor_song, data = df1)
#   p=0.56, test was not significant so assumption met


# Test for Independence of covariate and group
m1 <- lm(pptage ~ factor_song + dadage, data=df1)
m2 <- lm(pptage ~ factor_song * dadage, data=df1)
anova(m1, m2)
#   p=0.13, test was not significant so assumption met

# This ANCOVA meets statistical assumptions


# ANCOVA model
# Response variable = father age
# Group variable = song 
# Covariate = participant age
ancova_dad <- aov(dadage ~ factor_song + pptage, data = df1)
ancova_dadage <- Anova(ancova_dad, type="III")
summary(ancova_dad)
summary(ancova_dadage)



# Test for Homogeneity
leveneTest(dadage~factor_song,data= df1)
#   p=0.37, test was not significant so assumption met


# Test for Independence of covariate and group
n1 <- lm(dadage ~ factor_song + pptage, data=df1)
n2 <- lm(dadage ~ factor_song * pptage, data=df1)
anova(n1, n2)
#   p=0.46, test was not significant so assumption met

# This ANCOVA meets statistical assumptions


# Post Hoc analyses on both ANCOVA models
# Anlayses within group differences for significance
posthoc_ppt <- glht(ancova_ppt, linfct = mcp(factor_song = "Tukey"))
summary(posthoc_ppt)
posthoc_dad <- glht(ancova_dad, linfct = mcp(factor_song = "Tukey"))
summary(posthoc_dad)


## Data wrangling of statistical analyses results ##

# Extract p values from Post Hoc analyses
# Round to 3 significant figures
summary_posthoc_ppt <- summary(posthoc_ppt)
p_values_ppt <- summary_posthoc_ppt$test$pvalues
pptpvalues <- round(p_values_ppt, 3)

summary_posthoc_dad <- summary(posthoc_dad)
p_values_dad <- summary_posthoc_dad$test$pvalues
dadpvalues <- round(p_values_dad, 3)


# Calculate mean age of participant per song
mean_age_ppt <- aggregate(df1$pptage, by=list(df1$factor_song), FUN=mean)


# Create columns containing the difference in mean age between songs 
kalpot <- 21.17169 - 20.57143
whenkal <- 21.17169 - 20.33524
whenpot <- 20.57143 - 20.33524
songdiffs <- c(kalpot, whenkal, whenpot)
songs <- c("Kalimba-Potato", "Kalimba-When", "Potato-When")


# Add columns to a data frame for participant age
mean_age_ppt <- mean_age_ppt %>% 
  mutate(songdiffs) %>% 
  mutate(songs)


# Calculate mean age of father per song
mean_age_dad <- aggregate(df1$dadage, by=list(df1$factor_song), FUN=mean)


# Create columns containing the difference in mean age between songs 
kalpot2 <- 55.07143 - 49.88889
whenkal2 <- 52.09091 - 49.88889
whenpot2 <- 55.07143 - 52.09091
songdiffs2 <- c(kalpot2, whenkal2, whenpot2)


# Add columns to a data frame for father age
mean_age_dad <- mean_age_dad %>% 
  mutate(songdiffs2) %>% 
  mutate(songs)


# Create columns for whether the age is of father or participant,
# which songs the difference comes from,
# the difference in mean age for participant and father,
# and the p values for each within group difference from participant and father
Who<- c('Participant','Participant', 'Participant', 'Father','Father','Father')

songminussong <- c('Kalimba-Potato','Kalimba-When64','Potato-When64',
                   'Kalimba-Potato','Kalimba-When64','Potato-When64')

songsdiffs <- c(mean_age_ppt$songdiffs, mean_age_dad$songdiffs2)  

p_values <- c(pptpvalues,dadpvalues)


# Create a data frame combining these columns
df2 <- data.frame(songsdiffs,Who,songminussong,p_values)


## Plotting the results of the ANCOVAs

# Bar chart with song difference pairs on the x axis,
# mean age difference on the y axis, grouped by who's mean age is shown-
# participant or father, with corresponding p values on the bars
# Display and save the plot
plot1 <- ggplot(df2, aes(x = songminussong, y = songsdiffs, color = Who)) +
  geom_bar(stat = "identity", color = 'black') +
  geom_text(aes(label = paste("p =", p_values)), size = 4, 
            position = position_dodge(width = 0), vjust = -0.6, hjust = 0.5) +
  labs(x = "Post Hoc analysis of songs", y = "Difference in age in years",
       title = "ANCOVA results of participant or father age by song listened to",
       subtitle = "Covariate was father or participant age",
       caption = "Source: Simmons et al., (2011)") +
  guides(fill = none) +
  theme(panel.background = element_rect(fill = 'white'),
        axis.line = element_line(color = "grey"))+
  scale_y_continuous(breaks = seq(0,6,1))
plot1
ggsave(filename = file.path("graphs","ANCOVAs.png"), 
       width = 13, height = 15, units = "cm")
