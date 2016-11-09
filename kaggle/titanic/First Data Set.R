# Load raw data

train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Add a "Survived" varible to the test set to allow for combining data sets
# data.frame creates new data frames / type ?data.frame in console for more help
# add a varible called survived and make test 11 varibles like "train" set
# rep = repeat the value of "none" 418 times (the # of rows in test) and 
# assign it to the "Survived" varible
# then combine that varible with the "test" varible, leave blank to use entire data frame
test.Survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train,test.Survived)

# Take a look at the string types in data set
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

# Look at gross survival rates
table(data.combined$Survived)

# Dist across classes
table(data.combined$Pclass)

# Load up ggplot package to use for visualization
library(ggplot2)

# Hypothesis: rich folks suvived at a higher rate
train$Pclass <- as.factor(train$Pclass)

# the lib you're using + the data frame + an x axis corresponding to pclass +
# want a fill color (optional) + color code based on the Survived varible (converted to a factor) 
# + plot out a histogram + add an x  & y lab + add to the fill for Survived
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Examine first few names in trainig data set
head(as.character(train$Name))

# How many unique names are there across the train and test set?
#                          (grab name from d.c. data frame) 1309 values
#           (convert them to a char str)
#     find out how many of them are unique 
# then tell me
length(unique(as.character(data.combined$Name)))

# Take a closer look at the 2 dups
# First get dups names and store them as a vector
#                                                                    (grab names from set)
#                                                       (convert those to char str)
#                                            (then use duplicated func on that to give rows of dups)
#                        (from this set, grab the 'Name' but only the dups)    
#            ( take all that and make it a char called 'dup.Names')
dup.Names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Next take a look at the records in the combined data set
# Look at the dups and see if they are the same ppl or diff ppl with same name, by pulling out
# data that match those records
#                  (go thru all names in d.c. set)   
#                                     (and if these names are a dup...)
#            (which = pull that record out)
#                                                    (return all)
data.combined[which(data.combined$Name %in% dup.Names),]

# What is up with the Miss and Mr. thing?
library(stringr)

# Any correlation with other varibles (e.g., 'sibsp')?
#                                       (grab all names in d.c. set)
#                           (and detect if 'Miss.' is in string)
#                      (and if so, grab every single record in data set where 'Miss.' is in the name)
#                                                                   (return all)
# (store results in 'misses' varible)
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
# look at the first 5
misses[1:5,]

# Let's look at Mrs. now
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses [1:5,]

# Check to see if pattern cont with the males
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

# Expand upon the r.ships b/t 'Survived' and 'Pclass' by adding the new 'Title' varible to the
# data set and then Explore a potential 3D r.ship

# Create a utility function to help with title extraction 
extractTitle <- function(Name){
  Name <- as.character(Name) # Convert name into char
  
  # If it recognizes 'Miss' w/i a name, and length > 0, then return 'Miss'
  if (length(grep("Miss.", Name)) > 0) {
    return("Miss.")
  } else if (length(grep("Master.", Name)) > 0) {
    return("Master.")
  } else if (length(grep("Mrs.", Name)) > 0) {
    return("Mrs.")
  } else if (length(grep("Mr.", Name)) > 0) {
    return("Mr.")
  } else {
    return("Other")
  }
}

Titles <- NULL
# Loop over all the values in d.c.and call the 'c' function, grab the name, pass it into the func
# take w/e comes out of func and add it to titles
for (i in 1:nrow(data.combined)) {
  Titles <- c(Titles,extractTitle(data.combined[i,"Name"]))
}
# Add in a new varible called title but conver to a factor first
data.combined$Title <- as.factor(Titles)

# Since we only have survived lables for the train set, only use first 891 rows
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) + 
  geom_bar(width = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

data.combined$title <- NULL

# What's the dist of females to males across test and train?
table(data.combined$Sex)

# Visualize the 3-way r.ship of sex, pclass, and survival, compare to title analysis
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Look at age dist over entire set
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

# To be thourough, look at Survival rates broken out by sex, class, & age
# DNU this one, instead use next ggplot
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) + 
  geom_bar(width = 10) +
  xlab("Age") +
  ylab("Total Count")

ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

# Validate that "Master" is a good proxy for male children
boys <- data.combined[which(data.combined$Title == "Master."),]
data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

# "Miss" is a lil more complicated, let's examine closer
misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x= Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") +
  xlab("Age") +
  ylab("Total Count")

# OK, it appears female kids have a differnt survival rate, could be a CANDIDATE
# for FEATURE ENGINEERING later
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0), ]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

# Move on to the sibsp varible, summarize varible
summary(data.combined$SibSp)

# Can we treat as a factor?
length(unique(data.combined$SibSp))

data.combined$SipSp <- as.factor(data.combined$SibSp)

# We believe Title is predictive. 
# Visualize survival rates by sibsp, pclass, & title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Treat 'Parch' variable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar(width = 1) +
  stat_count(width = 1) +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Let's try to do some feature engineering. Let's create a "family size" feature
# We are using test and train b/c we want int values and not the factors
temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)

data.combined$Fam.Size <- as.factor(temp.SibSp + temp.Parch + 1)
FamSize <- NULL
data.combined$FamSize<- NULL

# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = Fam.Size, fill = Survived)) +
  geom_bar(width = 1) +
  stat_count(width = 1) +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") +
  xlab("Fam.Size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#################PREVIOUS WORK BEFORE EXIT############################

# REMEMBER TO SET WORKING DIRECTORY

# Take a look at ticket variable
str(data.combined$Ticket)

# Turn tic varible into a string
# Display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each
#                                                     (grab part of the string, the 1st char)
#                    (substr call is wrapped in 'ifelse')
#                       (check to see if the 1st char in Ticket is an empty string)
#                       (& if it finds empty string, put a space in its place)
#                       (otherwise, give results of 1st char in string)
ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))

# Give me all the 1st chars in from all the tics :)
unique(ticket.first.char)

# Now make a factor out of variable for analysis and viz purposes
data.combined$ticket.first.char <- as.factor(ticket.first.char)

# First, a hi-lvl plot of the data
ggplot(data.combined[1:891,], aes(x= ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by ticket.first.char") + 
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Tics seem interesting, let's dog in some more
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Pclass") +
  facet_wrap(~Pclass) +
  xlab("ticket.first.char") + #quotes for labels plz#
  ylab("Total Count") + 
  ylim(0,150) +
  labs(fill = "Survived")

# See about a combo of Pclass and Title
ggplot(data.combined[1:891,], aes(x = ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Pclass, Title") +
  facet_wrap(~Pclass + Title) +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0, 200) + 
  labs( fill = "Survived")

# Did it myself and got it right before looking at example :)

# Next up - fares passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))

# Can't make fare a factor (too large), treat as numeric and viz with histogram
ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

# Let's see of Fare as predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill = "Survived")

# Analysis of cabin varible
str(data.combined$Cabin)

# Cabin is not a factor, make a string and display first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

# Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

# Take a look at Cabin first letter as a factor
cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

# Add to combined data set and plot
data.combined$cabin.first.char <- cabin.first.char

# Hi lvl plot
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") + 
  ylim(0,750) +
  labs(fill = "Survived")


# Could have some predictive power, drill in
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Does this feature improve upon pclass + title?
ggplot(data.combined[1:891,], aes(x = cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# What about folks with multiple cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab('Total Count') +
  ylim(0,350)
  labs(fill = "Survived")
  
# Does survivability depend on where you boarded the Titanic?
str(data.combined$PassengerId)  
str(data.combined$Embarked)
levels(data.combined$Embarked)

# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() + 
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title")
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
  
  #==============================================================================
  #
  # Video #4 - Exploratory Modeling
  #
  #============================================================================== 
  
  
  library(randomForest)
  
  # Train a Random Forest with the default parameters using pclass & title
  rf.train.1 <- data.combined[1:891, c("pclass", "title")]
  rf.label <- as.factor(train$survived)
  
  set.seed(1234)
  rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
  rf.1
  varImpPlot(rf.1)
  
  #Hi I really hope Git syncs with RStudio this time :)