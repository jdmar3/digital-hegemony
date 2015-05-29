# Clear all data

rm(list = ls())

############################################

# SWITCH TO MANUALLY-CODED DATA

############################################

h <- read.csv(file="./hegemony-manual.csv",header=T, na.strings=c("","NA"))

h$Race <- as.factor(h$Race)
h$Gender <- as.factor(h$Gender)
h$PIgender <- as.factor(h$PIgender)

# Summary of Race and Gender variables
summary(as.factor(h$Gender))
summary(as.factor(h$Race))
summary(as.factor(h$PIgender))

summary(as.factor(h$PIgender))/657*100



# Crosstab of Race and Gender variables
table(as.factor(h$Gender),as.factor(h$Race))


h$Total <- numeric(length=656)

h$Total <- (as.numeric(gsub("[^0-9]","",h$AwardOutright)) + as.numeric(gsub("[^0-9]","",h$AwardMatching)) + as.numeric(gsub("[^0-9]","",h$SupplementAmount)))/100

SummaryRaceAwards <- as.data.frame(rbind(
  summary(h.race$Total),summary(h.race$Total[h.race$Race %in% "Asian"]),summary(h.race$Total[h.race$Race %in% "Black"]),
  summary(h.race$Total[h.race$Race %in% "Latino"]),summary(h.race$Total[h.race$Race %in% "Native American"]),
  summary(h.race$Total[h.race$Race %in% "Pacific Islander"]),summary(h.race$Total[h.race$Race %in% "White"]),
  summary(h.race$Total[h.race$Race %in% "Multiple"]),summary(h.race$Total[h.race$Race %in% "Other"])
  ))

rownames(SummaryRaceAwards) <- c("Total",paste(levels(h.race$Race),sep=","))

SummaryRaceAwards

SummaryRaceCounts <- as.data.frame(rbind(summary(h.race$Race),round(summary(h.race$Race)/length(h.race$Race)*100,1)))

rownames(SummaryRaceCounts) <- c("Count","Percentage")

SummaryRaceCounts

SummaryGenderAwards <- as.data.frame(rbind(summary(h.gender$Total),
    summary(h.gender$Gender[h.gender$Gender %in% "Men"]),
    summary(h.gender$Gender[h.gender$Gender %in% "Women"]),
    summary(h.gender$Gender[h.gender$Gender %in% "Transgender"]),
    summary(h.gender$Gender[h.gender$Gender %in% "Multiple"])))

rownames(SummaryGenderAwards) <- c("Total",paste(levels(h.gender$Gender),sep=","))

SummaryGenderAwards

SummaryGenderCounts <- as.data.frame(rbind(summary(h.gender$Gender),round(summary(h.gender$Gender)/length(h.gender$Gender)*100,1)))

rownames(SummaryGenderCounts) <- c("Count","Percentage")

SummaryGenderCounts

SummaryPIgenderAwards <- as.data.frame(rbind(summary(h.pigender$Total),
                                             summary(h.pigender$PIgender[h.pigender$PIgender %in% "male"]),
                                             summary(h.pigender$PIgender[h.pigender$PIgender %in% "female"])))

rownames(SummaryPIgenderAwards) <- c("Total",paste(levels(h.pigender$PIgender),sep=","))

SummaryPIgenderAwards

SummaryPIgenderCounts <- as.data.frame(rbind(summary(h.pigender$PIgender),round(summary(h.pigender$PIgender)/length(h.pigender$PIgender)*100,1)))

rownames(SummaryPIgenderCounts) <- c("Count","Percentage")

SummaryPIgenderCounts

# Make pretty pictures
library(ggplot2)
library(scales)

h.race <- h[!is.na(h$Race),]
print(levels(h.race$Race)) 
h.race$Race <- factor(h.race$Race,levels(h.race$Race)[c(1:3,5,7,8,4,6)])

h.gender <- h[!is.na(h$Gender),]
print(levels(h.gender$Gender))
h.gender$Gender <- factor(h.gender$Gender,levels(h.gender$Gender)[c(1,4,3,2)])

h.pigender <- h[!is.na(h$PIgender),]

ggplot(h.race, aes(x = Race, y = Total)) + 
  geom_violin(outlier.shape = NA) + 
#  coord_flip() + 
  scale_y_continuous(limits = c(0, 400000),labels = comma) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", color="red") +
  xlab("Racial / Ethnic Focus of Project") + ylab("Total Grant Award") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./plot.race.award.png", width=6, height=4, dpi=300)

ggplot(h.race, aes(x = Race)) + geom_histogram() +
  xlab("Racial / Ethnic Focus of Project") + ylab("Number of Grants Awarded") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("./plot.race.count.png", width=6, height=4, dpi=300)

ggplot(h.gender, aes(x = Gender, y = Total)) + geom_boxplot() + 
  coord_flip() + 
  scale_y_continuous(limits = c(0, 400000),labels = comma) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", color="red") +
  xlab("Gender Focus of Project") + ylab("Total Grant Award")

ggsave("./plot.gender.award.png", width=6, height=4, dpi=300)

ggplot(h.gender, aes(x = Gender)) + geom_histogram() + coord_flip() +
  xlab("Gender Focus of Project") + ylab("Number of Grants Awarded")

ggsave("./plot.gender.count.png", width=6, height=4, dpi=300)

ggplot(h.pigender, aes(x = PIgender, y = Total)) + geom_violin() + 
  coord_flip() + 
  scale_y_continuous(limits = c(0, 400000),labels = comma) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1) +
  stat_summary(fun.data=mean_sdl, mult=1, 
               geom="pointrange", color="red") +
  xlab("PI Gender") + ylab("Total Grant Award")

ggsave("./plot.pigender.award.png", width=6, height=4, dpi=300)

ggplot(h.pigender, aes(x = PIgender)) + geom_histogram() + coord_flip() +
  xlab("PI Gender") + ylab("Number of Grants Awarded")

ggsave("./plot.pigender.count.png", width=6, height=4, dpi=300)

print(levels(h$ProgramName))

ggplot(h, aes(x = ProgramName)) + geom_histogram() + 
  coord_flip() +
  xlab("NEH Grant Program") + ylab("Number of Grants Awarded") 

ggsave("./plot.grants awarded.png", width=6, height=4, dpi=300)
