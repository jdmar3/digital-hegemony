# Load dataset 
h <- read.csv(file="./hegemony.csv")

# Set match lists
match.b <- c("african","jim crow","slave","segregation","caribbean")
match.w <- c("anglo","european","jew","armenian","byzantine","dutch","mediterranean","caucasian","greek","roman","spain","appalachian","jefferson","whitman","founding")
match.a <- c("asian","japan","pakistan","middle east","afghanistan","iran","caucasus","china","korea","chinese","vietnam","mesopotamia","buddh","syriac","assyria","sanskrit")
match.l <- c("hispanic","latino","mexic","mesoamerican","chicano","puerto rico")
match.n <- c("blackfoot","native","indian","alaska","cahokia","athabascan","kwak")
match.p <- c("hawaii","samoa","philippin","papua")
match.o <- c("sahel","ethiopia","mali","libya","egypt","copt","kenya","camaroon")
#match.m <- c("","")
match.f <- c("women","feminist","feminism","lesbian","girl")
match.t <- c("transgender","genderqueer")
match.g <- c("gay","lesbian","lgb")

# Extract race
h$Race[grep(paste(match.b,collapse="|"), h$ProjectDesc, ignore.case=TRUE)] <- "Black"
h$Race[grep(paste(match.w,collapse="|"), h$ProjectDesc, ignore.case=TRUE)] <- "White"
h$Race[grep(paste(match.a,collapse="|"), h$ProjectDesc, ignore.case=TRUE)] <- "Asian"
h$Race[grep(paste(match.l,collapse="|"), h$ProjectDesc, ignore.case=TRUE)] <- "Latino"
h$Race[grep(paste(match.n,collapse="|"), h$ProjectDesc, ignore.case=TRUE)] <- "Native American"
h$Race[grep(paste(match.p,collapse="|"), h$ProjectDesc, ignore.case=TRUE)] <- "Pacific Islander"
h$Race[grep(paste(match.o,collapse="|"), h$ProjectDesc, ignore.case=TRUE)] <- "Other"

# Extract women
h$Gender[grep(paste(match.f,collapse="|"), h$ProjectDesc, ignore.case=TRUE)] <- "Women"

# Extract Trans
h$Gender[grep(paste(match.t,collapse="|"), h$ProjectDesc, ignore.case=TRUE)] <- "Transgender"

# Extract orientation
h$Orientation <- ""
h$Orientation[grep(paste(match.g,collapse="|"), h$ProjectDesc, ignore.case=TRUE)] <- "LBG"

# Extract gender of PI
library(splitstackshape)
library(genderizeR) 
library(gender)

# Strip out names only
h$Participants <- gsub("\\s\\([^\\)]*\\)","",h$Participants)
# Split Multiple names into individual columns
h <- cSplit(h, "Participants", "; ")

# Identify gender
hPart_1 <- gsub("[A-Z]\\.\\s","",as.character(h$Participants_1))
hPart_1 <- gsub("\\s.*","",as.character(hPart_1))
hGender_1 <- gender(as.character(hPart_1))
hGender_1 <- do.call(rbind.data.frame, hGender_1)
hGender_1

length(hGender_1$gender)

h$PIgender <- hGender_1$gender

# Save as RData file
save(h,file="hegemony-auto.Rdata")

# Export data to CSV
write.csv(h,file="hegemony-auto.csv")
