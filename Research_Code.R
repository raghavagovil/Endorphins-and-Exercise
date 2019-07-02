
# Downloading Data and Data Wrangling

#Reading the files in
library(readr)
ls1 <- read_csv("101b_data - LS1.csv")
ls2 <- read_csv("101b_data - LS2.csv")
ls3 <- read_csv("101b_data - LS3.csv")
ls4 <- read_csv("101b_data - LS4.csv")
ls5 <- read_csv("101b_data - LS5.csv")


#Adding a missing value
ls3[5,5] <- 72.1


# Testting First Hypothesis

#Calculating endorphin difference
ls1$`Endorphins Diff` <- ls1$`Endorphins - After` - ls1$`Endorphins - Before`
ls2$`Endorphins Diff` <- ls2$`Endorphins - After` - ls2$`Endorphins - Before`
ls3$`Endorphins Diff` <- ls3$`Endorphins - After` - ls3$`Endorphins - Before`
ls4$`Endorphins Diff` <- ls4$`Endorphins - After` - ls4$`Endorphins - Before`
ls5$`Endorphins Diff` <- ls5$`Endorphins - After` - ls5$`Endorphins - Before`
final_data <- rbind(ls1,ls2,ls3,ls4,ls5)


#write.csv(final_data, file = "final_data.csv")

#Converting all numeric to factors to get a valid model
cols <- c("Row", "Column", "Excercise")
ls1[cols] <- lapply(ls1[cols], factor)
ls2[cols] <- lapply(ls2[cols], factor)
ls3[cols] <- lapply(ls3[cols], factor)
ls4[cols] <- lapply(ls4[cols], factor)
ls5[cols] <- lapply(ls5[cols], factor)

## Initial Linear Models

#Making Linear Models for each Latin Square
m1 <- lm(`Endorphins Diff` ~ Row + Column + Excercise, data = ls1)
m2 <- lm(`Endorphins Diff` ~ Row + Column + Excercise, data = ls2)
m3 <- lm(`Endorphins Diff` ~ Row + Column + Excercise, data = ls3)
m4 <- lm(`Endorphins Diff` ~ Row + Column + Excercise, data = ls4)
m5 <- lm(`Endorphins Diff` ~ Row + Column + Excercise, data = ls5)

## Inital Diagnostic plots

#Plotting all 5 models 
par(mfrow=c(2,2)) 
plot(m1)
plot(m2)
plot(m3)
plot(m4)
plot(m5)

## Bad Leverage Removal

#Calculating bad leverage points for LS1 and removing them
d1 <- cooks.distance(m1)
r <- rstandard(m1)
a <- cbind.data.frame(rownames(ls1), d1, r)
inf <- a[d1 > 4/(25-(3+1)) & abs(r) > 2, ]
inf
dim(inf)[1]
inf[,1] <- as.numeric(inf[,1])
ls1_copy <- ls1[-inf[,1],]
#2 bad leverage points found



#Calculating bad leverage points for LS2 and removing them
d1 <- cooks.distance(m2)
r <- rstandard(m2)
a <- cbind.data.frame(rownames(ls2), d1, r)
inf <- a[d1 > 4/(25-(3+1)) & abs(r) > 2, ]
inf
dim(inf)[1]
inf[,1] <- as.numeric(inf[,1])
ls2_copy <- ls2[-inf[,1],]
#1 bad leverage point found


#Calculating bad leverage points for LS3 and removing them
d1 <- cooks.distance(m3)
r <- rstandard(m3)
a <- cbind.data.frame(rownames(ls3), d1, r)
inf <- a[d1 > 4/(25-(3+1)) & abs(r) > 2, ]
inf
dim(inf)[1]
ls3_copy <- ls3
#No bad leverage points found


#Calculating bad leverage points for LS4 and removing them
d1 <- cooks.distance(m4)
r <- rstandard(m4)
a <- cbind.data.frame(rownames(ls4), d1, r)
inf <- a[d1 > 4/(25-(3+1)) & abs(r) > 2, ]
inf
dim(inf)[1]
inf[,1] <- as.numeric(inf[,1])
ls4_copy <- ls4[-inf[,1],]
#1 bad leverage point found


#Calculating bad leverage points for LS5 and removing them
d1 <- cooks.distance(m5)
r <- rstandard(m5)
a <- cbind.data.frame(rownames(ls5), d1, r)
inf <- a[d1 > 4/(25-(3+1)) & abs(r) > 2, ]
inf
dim(inf)[1]
inf[,1] <- as.numeric(inf[,1])
ls5_copy <- ls5[-inf[,1],]
#2 bad leverage points found


## New Linear Models without Bad Leverage

#Creating new models with the bad leverage points removed
m1_copy <- lm(`Endorphins Diff` ~ Row + Column + Excercise, data = ls1_copy)
m2_copy <- lm(`Endorphins Diff` ~ Row + Column + Excercise, data = ls2_copy)
m3_copy <- lm(`Endorphins Diff` ~ Row + Column + Excercise, data = ls3_copy)
m4_copy <- lm(`Endorphins Diff` ~ Row + Column + Excercise, data = ls4_copy)
m5_copy <- lm(`Endorphins Diff` ~ Row + Column + Excercise, data = ls5_copy)


## New Diagnostic Plots

#The new diagnostic plots
par(mfrow=c(2,2)) 
plot(m1_copy)
plot(m2_copy)
plot(m3_copy)
plot(m4_copy)
plot(m5_copy)



#The dataset combining all 5 latin squares (without the leverage points)
all <- rbind(ls1_copy,ls2_copy,ls3_copy,ls4_copy,ls5_copy)


## Diagnostic Plots for Combined Model

#Creating the overall liner model for all 5 latin squares
m_all <- lm(`Endorphins Diff` ~ Row + Column + Excercise, data = all)
plot(m_all)


## ANOVA Tables

#Anova Tables
anova(m1_copy)
anova(m2_copy)
anova(m3_copy)
anova(m4_copy)
anova(m5_copy)
anova(m_all)



#Diagnostic plots for combined latin square for use in final paper
par(mfrow=c(2,2)) 
plot(m_all)


## Boxplots for Data Visualisation

#Boxplots for each LS - used in final paper
library(ggplot2)
g1 <- ggplot(ls1_copy, aes(x=Excercise, y=`Endorphins Diff`, fill=Excercise))+
  geom_boxplot() + 
  ggtitle("Endorphin Differences per Exercise, Latin Square 1") +
  ylab ("Endorphin Difference (pg/mL)") +
  xlab("Exercise") +
  scale_x_discrete(breaks=c("1","2","3","4","5"), labels=c("100-m Run", "1-km Run", "5-km Run", "50-m Swim", "200-m Swim")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

g2 <- ggplot(ls2_copy, aes(x=Excercise, y=`Endorphins Diff`, fill=Excercise))+
  geom_boxplot() + 
  ggtitle("Endorphin Differences per Exercise, Latin Square 2") +
  ylab ("Endorphin Difference (pg/mL)") +
  xlab("Exercise") +
  scale_x_discrete(breaks=c("1","2","3","4","5"), labels=c("100-m Run", "1-km Run", "5-km Run", "50-m Swim", "200-m Swim")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

g3 <- ggplot(ls3_copy, aes(x=Excercise, y=`Endorphins Diff`, fill=Excercise))+
  geom_boxplot() + 
  ggtitle("Endorphin Differences per Exercise, Latin Square 3") +
  ylab ("Endorphin Difference (pg/mL)") +
  xlab("Exercise") +
  scale_x_discrete(breaks=c("1","2","3","4","5"), labels=c("100-m Run", "1-km Run", "5-km Run", "50-m Swim", "200-m Swim")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

g4 <- ggplot(ls4_copy, aes(x=Excercise, y=`Endorphins Diff`, fill=Excercise))+
  geom_boxplot() + 
  ggtitle("Endorphin Differences per Exercise, Latin Square 4") +
  ylab ("Endorphin Difference (pg/mL)") +
  xlab("Exercise") +
  scale_x_discrete(breaks=c("1","2","3","4","5"), labels=c("100-m Run", "1-km Run", "5-km Run", "50-m Swim", "200-m Swim")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

g5 <- ggplot(ls5_copy, aes(x=Excercise, y=`Endorphins Diff`, fill=Excercise))+
  geom_boxplot() + 
  ggtitle("Endorphin Differences per Exercise, Latin Square 5") +
  ylab ("Endorphin Difference (pg/mL)") +
  xlab("Exercise") +
  scale_x_discrete(breaks=c("1","2","3","4","5"), labels=c("100-m Run", "1-km Run", "5-km Run", "50-m Swim", "200-m Swim")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

g6 <- ggplot(all, aes(x=Excercise, y=`Endorphins Diff`, fill=Excercise))+
  geom_boxplot() + 
  ggtitle("Endorphin Differences per Exercise, All Latin Squares Combined") +
  ylab ("Endorphin Difference (pg/mL)") +
  xlab("Exercise") +
  scale_x_discrete(breaks=c("1","2","3","4","5"), labels=c("100-m Run", "1-km Run", "5-km Run", "50-m Swim", "200-m Swim")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))



#Displaying the Boxplots
g1 
g2
g3
g4
g5
g6


#Saving Boxplots to local computer
ggsave(filename="g1.png", plot=g1)
ggsave(filename="g2.png", plot=g2)
ggsave(filename="g3.png", plot=g3)
ggsave(filename="g4.png", plot=g4)
ggsave(filename="g5.png", plot=g5)
ggsave(filename="g6.png", plot=g6)


## Testing Second Hypothesis: Relationship between endorphin exercise and time taken to complete exercise

#Linear model plot to study relationship between endorphin increase and time taken to complete exercise
lmplot <- ggplot(all, aes(x = Time, y=(`Endorphins Diff`))) +
  geom_point(aes(color=factor(Excercise))) +
  geom_smooth(method=lm, color="black")+ 
  ggtitle("Relationship Between Endorphin Difference and Time") + 
  ylab ("Endorphin Difference (pg/mL)") +
  xlab("Time (seconds)") +
  #scale_color_manual(values=c('#F8766D', '#C49A00', '#53B400', '#00C094', '#00B6EB')) +
  scale_color_discrete(name = "Exercise", labels = c("100-m Run", "1-km Run", "5-km Run", "50-m Swim", "200-m Swim"))
lmplot


#Saving plot to local computer
ggsave(lmplot, filename = "lmplot.png")


## Linear Model

#Linear model to study relationship between endorphin increase and time taken to complete exercise
initial_lm <- lm(`Endorphins Diff` ~ Time, data = all)
summary(initial_lm)
#Diagnostic plots 
par(mfrow=c(2,2))
plot(initial_lm)


## Inverse Reverse Plot for determing the optimal transformation

#Inverse reverse plot to determine the optimal transformation for time taken to complete exercise
library(leaps)
library(car)
inverseResponsePlot(initial_lm)


## Final Transformed Model

#Final trasnformed model to study relationship between endorphin increase and time taken to complete exercise
time_lm <- lm((`Endorphins Diff`)^3 ~ Time, data = all)
summary(time_lm)
#Diagnostic plots
par(mfrow=c(2,2))
plot(time_lm)


## Table with Means

#Table with Means for endorphin difference and exercise
library(dplyr)
means <- all %>% group_by(Excercise) %>% summarise(mean = mean(`Endorphins Diff`, na.rm = TRUE))
means$Excercise <- c("100-m Run", "1-km Run", "5-km Run", "50-m Swim", "200-m Swim")
arrange(means, mean)



#Table with Means for endorphin difference and time
means2 <- all %>% group_by(Excercise) %>% summarise(mean = mean(Time, na.rm = TRUE))
means2$Excercise <- c("100-m Run", "1-km Run", "5-km Run", "50-m Swim", "200-m Swim")
arrange(means2, mean)


## T-Test

#T-Test for time based on exercise type
all$ExcerciseType <- ifelse (all$Excercise %in% c(1,2,3), "Non-Resistant", "Resistant")
t.test(Time ~ ExcerciseType, data = all)


## Third Hypothesis Tested: Resistant vs Non-Resistant

#Grouping exercise by resistant and non-resistant
all$ExcerciseType <- ifelse (all$Excercise %in% c(1,2,3), "Non-Resistant", "Resistant")


## Boxplot for Visualising difference in means

#Boxplot to visualise difference in mean endorphin increase for resistant and non-resistant exercise
boxplots2 <- ggplot(all, aes(x=ExcerciseType, y=`Endorphins Diff`, fill=ExcerciseType))+
  geom_boxplot() + 
  ggtitle("Endorphin Differences per Exercise Type, All Latin Squares Combined") +
  ylab ("Endorphin Difference (pg/mL)") +
  xlab("Exercise Type") +
  #scale_x_discrete(breaks=c("1","2","3","4","5"), labels=c("100-m Run", "1-km Run", "5-km Run", "50-m Swim", "200-m Swim")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))
boxplots2



#Saving boxplot to local computer
ggsave(boxplots2, filename = "boxplots2.png")



#Boxplot to visualise difference in mean time for each exercise
boxplots2_1 <- ggplot(all, aes(x=ExcerciseType, y=Time, fill=ExcerciseType))+
  geom_boxplot() + 
  ggtitle("Time per Exercise Type, All Latin Squares Combined") +
  ylab ("Time (seconds)") +
  xlab("Exercise Type") +
  #scale_x_discrete(breaks=c("1","2","3","4","5"), labels=c("100-m Run", "1-km Run", "5-km Run", "50-m Swim", "200-m Swim")) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(boxplots2_1, filename = "boxplots2_1.png")


## T-Test

#T-test to test hypothesis of whethere the endorphin increase differs with resistant and non-resistant exercise
t.test(`Endorphins Diff` ~ ExcerciseType, data = all)
