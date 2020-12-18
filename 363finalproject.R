#these are our required libraries
library(dplyr)
library(ggplot2)
library(knitr)
library(maps)


#first we need to read in our file. 
scores <- read.csv("C:/Users/Om/Desktop/FALL2020/DATA 363/Data 363 Project 1/school_scores.csv", header=TRUE)
View(scores)

#here, we're making checks to make sure we have what we need and our data has been read in properly 
#without any surprises. 
scores%>% summarize(distinct = n_distinct(State.Name))
scores %>% summarize(distinct = n_distinct(Year))
nrow(scores)


#From this point forward, we are collecting the country avg GPA in all the subjects 
#from 2005-2015

#Average GPA of the country over the years 2005-2015 in arts and music
average_gpa_arts_music <- mean(scores$Academic.Subjects.Arts.Music.Average.GPA)
average_gpa_arts_music

#Average GPA of the country over the years 2005-2015 in English
average_gpa_english <- mean(scores$Academic.Subjects.English.Average.GPA)
average_gpa_english

#Average GPA of the country over the years 2005-2015 in Foreign Languages
average_gpa_foreign_lang <- mean(scores$Academic.Subjects.Foreign.Languages.Average.GPA)
average_gpa_foreign_lang 

#Average GPA of the country over the years 2005-2015 in Mathematics
average_gpa_math <- mean(scores$Academic.Subjects.Mathematics.Average.GPA)
average_gpa_math

#Average GPA of the country over the years 2005-2015 in Natural Sciences
average_gpa_nat_sciences <- mean(scores$Academic.Subjects.Natural.Sciences.Average.GPA)
average_gpa_nat_sciences

#Average GPA of the country over the years 2005-2015 in Social Sciences
average_gpa_social_sciences <- mean(scores$Academic.Subjects.Social.Sciences.History.Average.GPA)
average_gpa_social_sciences

#now we will make the plot
# Create the data for the chart
avg_subject_gpas <- c(3.822704,3.500953,3.453345,3.310312,3.41818, 3.522166)
subjects <- c("Arts","English","Foreign Languages","Math","Natural Sciences", "Social Sciences")
sd(c(3.822704,3.500953,3.453345,3.310312,3.41818, 3.522166))

# Give the chart file a name
png(file = "barchart_months_revenue.png")

# Plot the bar chart 
barplot(avg_subject_gpas,names.arg=subjects,xlab="Subjects",ylab="GPA",col="maroon",
        main="Subjectwise Avg. GPA over 2005-2015",border="blue", las=3,cex.axis=0.70, cex.names=0.70 )

# Save the file
dev.off()


#now we will look at the country average total scores from 2005-2015 
#we are looking at the total of math and verbal, and it is important
#to note that in the dataset, verbal includes only reading, not writing. 
average_total_score <- mean(scores$Total.Math) + mean(scores$Total.Verbal)
average_total_score

#here,we make a new column of average total scores and add it to the df. 
scores_1 <- scores %>% mutate (Average_Total_Score =Total.Math + Total.Verbal)
standard_deviation <- sd(scores_1$Average_Total_Score)
standard_deviation

#histogram of average total sat scores. 
format <- theme(plot.title = element_text(face = "bold", hjust = 0.5), 
            axis.title = element_text(size = rel(1)),
            legend.position = "bottom")
ggplot(data = scores_1) +
  geom_histogram(mapping = aes(x = Average_Total_Score), bins = 20) + 
  labs(title = "Histogram of Average SAT Scores", x = "Average Total Score", y = "Frequency") + format


#histogram of average total sat scores over years
ggplot(data = scores_1) +
  geom_histogram(mapping = aes(x = Average_Total_Score), bins = 20) + 
  labs(title = "Histogram of Average SAT Scores By Year", x = "Average Total Score", y = "Frequency") + 
  facet_wrap(~Year) + format

#NoW WE WILL LOOK AT INCOME DATA

#average number of test-takers from this income bracket state-wise over 2005-2015
avg_tt_20below <- mean(scores$Family.Income.Less.than.20k.Test.takers) 
avg_tt_20below

#average number of test-takers from this income bracket state-wise over 2005-2015 
avg_tt_20_40 <- mean(scores$Family.Income.Between.20.40k.Test.takers) 
avg_tt_20_40

#average number of test-takers from this income bracket state-wise over 2005-2015
avg_tt_40_60 <- mean(scores$Family.Income.Between.40.60k.Test.takers) 
avg_tt_40_60

#average number of test-takers from this income bracket state-wise over 2005-2015
avg_tt_60_80 <- mean(scores$Family.Income.Between.60.80k.Test.takers) 
avg_tt_60_80

#average number of test-takers from this income bracket state-wise over 2005-2015
avg_tt_80_100 <- mean(scores$Family.Income.Between.80.100k.Test.takers) 
avg_tt_80_100

#average number of test-takers from this income bracket state-wise over 2005-2015
avg_tt_100above <- mean(scores$Family.Income.More.than.100k.Test.takers) 
avg_tt_100above

# Give the chart file a name
png(file = "barchart_avg_tt_income_brackets.png")
brackets <- c("below 20k","20k-40k","40k-60k","60k-80k","80k-100k", "above 100k")

sd_brackets = sd(c(avg_tt_20below,avg_tt_20_40,avg_tt_40_60,avg_tt_60_80,avg_tt_80_100,avg_tt_100above))
sd_brackets
# Plot the bar chart 
barplot(c(avg_tt_20below,avg_tt_20_40,avg_tt_40_60,avg_tt_60_80,avg_tt_80_100,avg_tt_100above),names.arg=brackets,xlab="Income Brackets",ylab="Average Number of Students",col="purple",
        main="Income-wise average number of test-takers",border="black", las=3,cex.axis=0.70, cex.names=0.70 )

# Save the file
dev.off()

#now we will look at the average scores according to income brackets. 
#below 20k
average_total_score_below20 <- mean(scores$Family.Income.Less.than.20k.Math) + mean(scores$Family.Income.Less.than.20k.Verbal)
average_total_score_below20 

#20k-40k
average_total_score_20_40 <- mean(scores$Family.Income.Between.20.40k.Math) + mean(scores$Family.Income.Between.20.40k.Verbal)
average_total_score_20_40

#40k-60k
average_total_score_40_60 <- mean(scores$Family.Income.Between.40.60k.Math) + mean(scores$Family.Income.Between.40.60k.Verbal)
average_total_score_40_60 

#60k-80k
average_total_score_60_80 <- mean(scores$Family.Income.Between.60.80k.Math) + mean(scores$Family.Income.Between.60.80k.Verbal)
average_total_score_60_80 

#80k-100k
average_total_score_80_100 <- mean(scores$Family.Income.Between.80.100k.Math) + mean(scores$Family.Income.Between.80.100k.Verbal)
average_total_score_80_100 

#above 100k
average_total_score_above100 <- mean(scores$Family.Income.More.than.100k.Math) + mean(scores$Family.Income.More.than.100k.Verbal)
average_total_score_above100

# Give the chart file a name
png(file = "barchart_avg_score_income_brackets.png")
brackets <- c("below 20k","20k-40k","40k-60k","60k-80k","80k-100k", "above 100k")

sd_brackets = sd(c(average_total_score_below20 ,average_total_score_20_40,average_total_score_40_60 ,average_total_score_60_80 ,average_total_score_80_100 ,average_total_score_above100))
sd_brackets
# Plot the bar chart 
barplot(c(average_total_score_below20 ,average_total_score_20_40,average_total_score_40_60 ,average_total_score_60_80 ,average_total_score_80_100 ,average_total_score_above100),names.arg=brackets,xlab="Income Brackets",ylab="Average Total Score",col="yellow",
        main="Income-wise average total SAT score",border="black", las=3,cex.axis=0.70, cex.names=0.70 )

# Save the file
dev.off()

#correlation between number of years of arts education vs sat score. 
scores$Year <- factor (scores$Year)
correlation2 <- scores_1 %>% select(Academic.Subjects.Arts.Music.Average.Years, Average_Total_Score, Year)
correlation2 %>% summarize(correlation = cor(Academic.Subjects.Arts.Music.Average.Years, Average_Total_Score))

ggplot(data = correlation2, mapping = aes(x=Academic.Subjects.Arts.Music.Average.Years, y=Average_Total_Score, col = Year)) +
  geom_point(alpha = 0.8, position = "jitter") + 
  geom_smooth(method = "lm") +
  labs(title = "Correlation between Arts Education and SAT Score", x = "Number of Years of Arts Education", y = "Total SAT Score") + format

#correlation between number of years of english vs sat score. 
scores$Year <- factor (scores$Year)
correlation2 <- scores_1 %>% select(Academic.Subjects.English.Average.Years, Average_Total_Score, Year)
correlation2 %>% summarize(correlation = cor(Academic.Subjects.English.Average.Years,Average_Total_Score))

ggplot(data = correlation2, mapping = aes(x=Academic.Subjects.English.Average.Years, y=Average_Total_Score, col = Year)) +
  geom_point(alpha = 0.8, position = "jitter") + 
  geom_smooth(method = "lm") +
  labs(title = "Correlation between English Education and SAT Score", x = "Number of Years of English Education", y = "Total SAT Score") + format

#correlation between number of years of foreign languages vs sat score. 
scores$Year <- factor (scores$Year)
correlation2 <- scores_1 %>% select(Academic.Subjects.Foreign.Languages.Average.Years, Average_Total_Score, Year)
correlation2 %>% summarize(correlation = cor(Academic.Subjects.Foreign.Languages.Average.Years,Average_Total_Score))

ggplot(data = correlation2, mapping = aes(x=Academic.Subjects.Foreign.Languages.Average.Years, y=Average_Total_Score, col = Year)) +
  geom_point(alpha = 0.8, position = "jitter") + 
  geom_smooth(method = "lm") +
  labs(title = "Correlation between Foreign Language Education and SAT Score", x = "Number of Years of Foreign Language Education", y = "Total SAT Score") + format


#correlation between number of years of Math vs sat score. 
scores$Year <- factor (scores$Year)
correlation2 <- scores_1 %>% select(Academic.Subjects.Mathematics.Average.Years, Average_Total_Score, Year)
correlation2 %>% summarize(correlation = cor(Academic.Subjects.Mathematics.Average.Years,Average_Total_Score))

ggplot(data = correlation2, mapping = aes(x=Academic.Subjects.Mathematics.Average.Years, y=Average_Total_Score, col = Year)) +
  geom_point(alpha = 0.8, position = "jitter") + 
  geom_smooth(method = "lm") +
  labs(title = "Correlation between Mathematics Education and SAT Score", x = "Number of Years of Mathematics Education", y = "Total SAT Score") + format


#correlation between number of years of Natural Sciences vs sat score. 
scores$Year <- factor (scores$Year)
correlation2 <- scores_1 %>% select(Academic.Subjects.Natural.Sciences.Average.Years, Average_Total_Score, Year)
correlation2 %>% summarize(correlation = cor(Academic.Subjects.Natural.Sciences.Average.Years,Average_Total_Score))

ggplot(data = correlation2, mapping = aes(x=Academic.Subjects.Natural.Sciences.Average.Years, y=Average_Total_Score, col = Year)) +
  geom_point(alpha = 0.8, position = "jitter") + 
  geom_smooth(method = "lm") +
  labs(title = "Correlation between Natural Sciences Education and SAT Score", x = "Number of Years of Natural Science Education", y = "Total SAT Score") + format


#correlation between number of years of Social Sciences vs sat score. 
scores$Year <- factor (scores$Year)
correlation2 <- scores_1 %>% select(Academic.Subjects.Social.Sciences.History.Average.Years, Average_Total_Score, Year)
correlation2 %>% summarize(correlation = cor(Academic.Subjects.Social.Sciences.History.Average.Years,Average_Total_Score))

ggplot(data = correlation2, mapping = aes(x=Academic.Subjects.Social.Sciences.History.Average.Years, y=Average_Total_Score, col = Year)) +
  geom_point(alpha = 0.8, position = "jitter") + 
  geom_smooth(method = "lm") +
  labs(title = "Correlation between Social Sciences Education and SAT Score", x = "Number of Years of Social Science Education", y = "Total SAT Score") + format


#average number of testtakers by state for 20k-40k
statewise_tt_20_40 <- scores_1 %>% 
  filter(Year == "2005") %>%
  select(Family.Income.More.than.100k.Test.takers, State.Name)
statewise_tt_20_40 <- statewise_tt_20_40[-c(9, 40, 48), ] #removes DC, Puerto Rico, and Virgin Islands
statewise_tt_20_40$State.Name = tolower(statewise_tt_20_40$State.Name)
colnames(statewise_tt_20_40)[colnames(statewise_tt_20_40) == 'State.Name'] <- 'region'
kable(statewise_tt_20_40 %>% arrange(desc(Family.Income.More.than.100k.Test.takers), region))

#making the map
map_state <- map_data("state")
combined_data <- map_state %>% left_join(statewise_tt_20_40, by = "region")
map_theme <- theme(
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  panel.background = element_rect(fill = "white")
)

ggplot() +
  geom_polygon(data = combined_data, 
               mapping = aes(x = long, y = lat, group = region, fill = Family.Income.More.than.100k.Test.takers)) +
  geom_polygon(data = map_state,
               mapping = aes(x = long, y = lat, group = group), fill = NA, col= "black") +
  scale_fill_gradient(low = "red", high = "blue") +
  coord_quickmap() + map_theme +
  labs(title = "Number of Test Takers from the Income Bracket above 100k for 2015") + format


