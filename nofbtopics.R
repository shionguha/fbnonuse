library(dplyr)
library(mallet)
library(ggplot2)
library(reshape2)

n.topics <- 25 # this might need to be, and probably should be, adjusted based on the total number of questions whose responses are being included as documents
num_long_words = 25 # the number of words to use for "long" topic labels
num.runs = 10 # number of total topic model solution runs

which.data <- 33 # select either 33, 66, or 99 day surveys

setwd("/Users/ericpsb/Dropbox/Work/Projects/99Days/NoFBSurvey/")

if (which.data == 33)
{
	col.types <- rep("factor", 44)
	col.types[1] <- "character"
	col.types[2] <- "character"
	col.types[5] <- "character"
	col.types[10] <- "character"
	col.types[12] <- "character"
	col.types[13] <- "character"
	col.types[14] <- "character"
	col.types[15] <- "character"
	col.types[21] <- "character"
	col.types[22] <- "character"
	col.types[23] <- "character"
	col.types[24] <- "character"
	col.types[25] <- "character"
	col.types[29] <- "character"
	
	data <- read.csv("surveys_33_epsb.csv", colClasses=col.types, quote="\"\"")
	
	data <- subset(data, data$survey_id != "")
	#data <- subset(data, data$X != "")
	
	data$ConsideredBreak <- as.numeric(levels(data$ConsideredBreak))[data$ConsideredBreak]
	
	data$FeelDuringExp <- gsub("\\n", " ", data$FeelDuringExp)
	#data$FeelDuringExp <- gsub("missing out", "missing_out", data$FeelDuringExp)
	
	data$FriendsReactExp <- gsub("\\n", " ", data$FriendsReactExp)
	#data$FriendsReactExp <- gsub("missing out", "missing_out", data$FriendsReactExp)
	
	data$BestThingExp <- gsub("\\n", " ", data$BestThingExp)
	#data$BestThingExp <- gsub("missing out", "missing_out", data$BestThingExp)
	
	data$WorstThingExp <- gsub("\\n", " ", data$WorstThingExp)
	#data$WorstThingExp <- gsub("missing out", "missing_out", data$WorstThingExp)
	
	data$MotivationJoinExp <- gsub("\\n", " ", data$MotivationJoinExp)
	#data$MotivationJoinExp <- gsub("missing out", "missing_out", data$MotivationJoinExp)
	
	data$FeelAboutReturning <- gsub("\\n", " ", data$FeelAboutReturning)
	#data$FeelAboutReturning <- gsub("missing out", "missing_out", data$FeelAboutReturning)
	
	data$FriendsReactReturning <- gsub("\\n", " ", data$FriendsReactReturning)
	#data$FriendsReactReturning <- gsub("missing out", "missing_out", data$FriendsReactReturning)
	# data$FriendsReactReturning <- gsub("no one", "no_one", data$FriendsReactReturning)
	
	data$MissMostAboutFB <- gsub("\\n", " ", data$MissMostAboutFB)
	#data$MissMostAboutFB <- gsub("missing out", "missing_out", data$MissMostAboutFB)
	# data$MissMostAboutFB <- gsub("no one", "no_one", data$MissMostAboutFB)
	
	data$MostValuableFB <- gsub("\\n", " ", data$MostValuableFB)
	
	data$MostNervousNext33Days <- gsub("\\n", " ", data$MostNervousNext33Days)
	
	data$FunPlansNext33Days <- gsub("\\n", " ", data$FunPlansNext33Days)
	
	data$Gender <- tolower(data$Gender)
	data[ data$Gender == "mujer", ]$Gender <- "f"
	data[ data$Gender == "", ]$Gender <- "d"
	data[ grep("hombre", data$Gender), ]$Gender <- "m"
	data[ grep("enski", data$Gender), ]$Gender <- "f"
	data[ grep("boy", data$Gender), ]$Gender <- "m"
	data[ grep("^n", data$Gender), ]$Gender <- "m"
	data[ grep("^y", data$Gender), ]$Gender <- "d"
	data[ grep("human being", data$Gender), ]$Gender <- "d"
	data[ grep("alien", data$Gender), ]$Gender <- "d"
	data[ grep("26", data$Gender), ]$Gender <- "d"
	data[ grep("cis-female", data$Gender), ]$Gender <- "f"
	data[ grep("happily female", data$Gender), ]$Gender <- "f"
	data[ grep("^w", data$Gender), ]$Gender <- "f"
	data[ grep("^v", data$Gender), ]$Gender <- "f"
	data[ grep("^g", data$Gender), ]$Gender <- "f"
	data[ grep("^f", data$Gender), ]$Gender <- "f"
	data[ grep("^m", data$Gender), ]$Gender <- "m"
	data[ grep("_", data$Gender), ]$Gender <- "d"
	data[ grep("-", data$Gender), ]$Gender <- "d"
	data$Gender <- as.factor(data$Gender)
	
	# I tried to use survey_id for the document id, but I kept getting an odd internal java error. might be because the survey_id's are non-sequential, not sure.
	documents <- rbind(data.frame(id=data$id, question="MostValuableFB", text=data$MostValuableFB, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="MotivationJoinExp", text=data$MotivationJoinExp, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="FeelDuringExp", text=data$FeelDuringExp, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="FriendsReactExp", text=data$FriendsReactExp, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="BestThingExp", text=data$BestThingExp, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="WorstThingExp", text=data$WorstThingExp, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
	#				   data.frame(id=data$id, question="FeelAboutReturning", text=data$FeelAboutReturning, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
	#				   data.frame(id=data$id, question="FriendsReactReturning", text=data$FriendsReactReturning, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="MissMostAboutFB", text=data$MissMostAboutFB, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
	#				   data.frame(id=data$id, question="FunPlansNext33Days", text=data$FunPlansNext33Days, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="MostNervousNext33Days", text=data$MostNervousNext33Days, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F))

} # end if 33 day

else if (which.data == 66)
{
	col.types <- rep("factor", 40)
	col.types[1] <- "character"
	col.types[2] <- "character"
	col.types[13] <- "character"
	col.types[14] <- "character"
	col.types[15] <- "character"
	col.types[16] <- "character"
	col.types[20] <- "character"
	col.types[23] <- "character"
	col.types[24] <- "character"
	col.types[25] <- "character"
	col.types[27] <- "character"
	col.types[30] <- "character"
	col.types[34] <- "character"
	
	data <- read.csv("surveys_66_epsb.csv", colClasses=col.types, quote="\"\"")
	
	data <- subset(data, data$survey_id != "")
	
	data$RelationshipFriendsFamilyChanged <- gsub("\\n", " ", data$RelationshipFriendsFamilyChanged)
	data$DailyRoutineChanged <- gsub("\\n", " ", data$DailyRoutineChanged)
	data$BestThingSinceLastSurvey <- gsub("\\n", " ", data$BestThingSinceLastSurvey)
	data$WorstThingSinceLastSurvey <- gsub("\\n", " ", data$WorstThingSinceLastSurvey)
	data$FeelAboutReturning <- gsub("\\n", " ", data$FeelAboutReturning)
	data$FriendsReactReturning <- gsub("\\n", " ", data$FriendsReactReturning)
	data$HowSpendTimeUsedToOnFB <- gsub("\\n", " ", data$HowSpendTimeUsedToOnFB)
	data$WhyOrNotPlanReturn <- gsub("\\n", " ", data$WhyOrNotPlanReturn)
	data$MostNervousNext33Days <- gsub("\\n", " ", data$MostNervousNext33Days)
	data$BestThingExpBrought <- gsub("\\n", " ", data$BestThingExpBrought)
	
	# I tried to use survey_id for the document id, but I kept getting an odd internal java error. might be because the survey_id's are non-sequential, not sure.
	documents <- rbind(data.frame(id=data$id, question="RelationshipFriendsFamilyChanged", text=data$RelationshipFriendsFamilyChanged, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="DailyRoutineChanged", text=data$DailyRoutineChanged, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="BestThingSinceLastSurvey", text=data$BestThingSinceLastSurvey, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="WorstThingSinceLastSurvey", text=data$WorstThingSinceLastSurvey, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="HowSpendTimeUsedToOnFB", text=data$HowSpendTimeUsedToOnFB, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="WhyOrNotPlanReturn", text=data$WhyOrNotPlanReturn, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="BestThingExpBrought", text=data$BestThingExpBrought, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="MostNervousNext33Days", text=data$MostNervousNext33Days, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F))

} # end if 66 days

else if (which.data == 99)
{
	col.types <- rep("factor", 40)
	col.types[1] <- "character"
	col.types[2] <- "character"
	col.types[15] <- "character"
	col.types[16] <- "character"
	col.types[20] <- "character"
	col.types[22] <- "character"
	col.types[23] <- "character"
	col.types[24] <- "character"
	col.types[26] <- "character"
	col.types[29] <- "character"
	col.types[32] <- "character"
	col.types[33] <- "character"
	col.types[34] <- "character"
	
	data <- read.csv("surveys_99_epsb.csv", colClasses=col.types, quote="\"\"")
	
	data <- subset(data, data$survey_id != "")
	
	data$RelationshipFriendsFamilyChanged <- gsub("\\n", " ", data$RelationshipFriendsFamilyChanged)
	data$DailyRoutineChanged <- gsub("\\n", " ", data$DailyRoutineChanged)
	data$BestThingSinceLastSurvey <- gsub("\\n", " ", data$BestThingSinceLastSurvey)
	data$WorstThingSinceLastSurvey <- gsub("\\n", " ", data$WorstThingSinceLastSurvey)
	data$FeelAboutReturning <- gsub("\\n", " ", data$FeelAboutReturning)
	data$FriendsReactReturning <- gsub("\\n", " ", data$FriendsReactReturning)
	data$HowSpendTimeUsedToOnFB <- gsub("\\n", " ", data$HowSpendTimeUsedToOnFB)
	data$WhyOrNotPlanReturn <- gsub("\\n", " ", data$WhyOrNotPlanReturn)
	data$BestThingExpBrought <- gsub("\\n", " ", data$BestThingExpBrought)
	data$QuittingOtherHabits <- gsub("\\n", " ", data$QuittingOtherHabits)
	data$PersonalInsightsGained <- gsub("\\n", " ", data$PersonalInsightsGained)
	
	# I tried to use survey_id for the document id, but I kept getting an odd internal java error. might be because the survey_id's are non-sequential, not sure.
	documents <- rbind(data.frame(id=data$id, question="RelationshipFriendsFamilyChanged", text=data$RelationshipFriendsFamilyChanged, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="DailyRoutineChanged", text=data$DailyRoutineChanged, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="BestThingSinceLastSurvey", text=data$BestThingSinceLastSurvey, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="WorstThingSinceLastSurvey", text=data$WorstThingSinceLastSurvey, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="HowSpendTimeUsedToOnFB", text=data$HowSpendTimeUsedToOnFB, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="WhyOrNotPlanReturn", text=data$WhyOrNotPlanReturn, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
#					   data.frame(id=data$id, question="QuittingOtherHabits", text=data$QuittingOtherHabits, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="PersonalInsightsGained", text=data$PersonalInsightsGained, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F),
					   data.frame(id=data$id, question="BestThingExpBrought", text=data$BestThingExpBrought, howhard=data$HowHardNotUsing, return=data$ReturnedBool, stringsAsFactors=F))
} # end if 99 days

# only include documents with 3 or more words and without NA's
documents <- subset(documents, sapply(gregexpr("\\W+", text), length) + 1 > 3)

#documents <- rbind(data.frame(id=data$X, text=data$MissMostAboutFB, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBin, stringsAsFactors=F))

#documents <- rbind(data.frame(id=data$X, text=data$MotivationJoinExp, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBin, consider=data$ConsideredBreak, stringsAsFactors=F))

'''
documents <- rbind(data.frame(id=data$survey_id, text=data$FeelAboutReturning, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBin, stringsAsFactors=F),
				   data.frame(id=data$survey_id, text=data$FriendsReactReturning, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBin, stringsAsFactors=F))
'''

'''
documents <- rbind(data.frame(id=data$X, text=data$FeelDuringExp, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
				   data.frame(id=data$X, text=data$FriendsReactExp, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F))

documents <- rbind(data.frame(id=data$X, text=paste(data$FeelDuringExp, data$FriendsReactExp, sep=" ||| "), gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F))

documents <- rbind(data.frame(id=data$X, text=data$BestThingExp, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F),
                   data.frame(id=data$X, text=data$WorstThingExp, gender=data$Gender, howhard=data$HowHardNotUsing, consider=data$ConsideredBreak, return=data$ReturnedBool, stringsAsFactors=F))
'''



mallet.instances <- mallet.import(documents$id, documents$text, "nofb.stop", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

all.topics <- matrix(nrow = n.topics * num.runs, ncol = num_long_words)
all.topic.labels <- character(n.topics * num.runs)

for (run_num in 0:(num.runs - 1))
{

## Create a topic trainer object.
topic.model <- MalletLDA(num.topics=n.topics)

## Load our documents. We could also pass in the filename of a 
##  saved instance list file that we build from the command-line tools.
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
#vocabulary <- topic.model$getVocabulary()
#word.freqs <- mallet.word.freqs(topic.model)

## Optimize hyperparameters every 20 iterations, 
##  after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Now train a model.
##  We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(1000)
doc.topics <- 0.1 * mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- 0.1 * mallet.topic.words(topic.model, smoothed=T, normalized=T)

for (sample.iter in 1:9) {
	topic.model$train(100)
	doc.topics <- doc.topics + 0.1 * mallet.doc.topics(topic.model, smoothed=T, normalized=T)
	topic.words <- topic.words + 0.1 * mallet.topic.words(topic.model, smoothed=T, normalized=T)
}

#topic.model$maximize(50)

#doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
#topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
# mallet.top.words(topic.model, topic.words[1,], 25)

#topics.labels <- gsub("\\W", "_", mallet.topic.labels(topic.model, topic.words, 5))
topics.labels <- vector(length=n.topics)
for (topic.i in 1:n.topics) 
{
	topics.labels[topic.i] <- gsub("\\W", "_", paste(as.vector(mallet.top.words(topic.model, topic.words[topic.i,], 5)[,1]), collapse="_"))
}
#topics.long.labels <- mallet.topic.labels(topic.model, topic.words, num.top.words=50)
topics.long.labels <- vector(length=n.topics)
for (topic.i in 1:n.topics)
{
	topics.long.labels[topic.i] <- paste(as.vector(mallet.top.words(topic.model, topic.words[topic.i,], 25)[,1]), collapse = " ")
}

#library("sets") # NB: masks %>% from dplyr

topic.sets <- matrix(nrow=n.topics, ncol=num_long_words)
for (topic.i in 1:n.topics)
{
	topic.sets[topic.i,] <- as.vector(mallet.top.words(topic.model, topic.words[topic.i,], 25)[,1])
}


doc.topics.frame <- data.frame(doc.topics)
#names(doc.topics.frame) <- paste("Topic", 1:n.topics, sep="")
names(doc.topics.frame) <- topics.labels
docs.and.topics <- cbind(documents, doc.topics.frame)


if (which.data == 33)
{
	melted.docs.and.topics <- melt(docs.and.topics, id.vars=c("id", "question", "text", "gender", "return", "consider", "howhard"), variable.name="topic")
}
if (which.data == 66)
{
	melted.docs.and.topics <- melt(docs.and.topics, id.vars=c("id", "question", "text", "return", "howhard"), variable.name="topic")
}
if (which.data == 99)
{
	melted.docs.and.topics <- melt(docs.and.topics, id.vars=c("id", "question", "text", "return", "howhard"), variable.name="topic")
}

#ggplot(melted.docs.and.topics %>% group_by(question, topic) %>% summarize(mean = mean(value)), aes(question, topic, fill=mean)) + geom_tile() + theme(axis.text.x=element_text(angle=-90))
#ggsave("/Users/mimno/Documents/Data/NoFBSurvey/question_topic.pdf")

#ggplot(melted.docs.and.topics %>% group_by(return, topic) %>% summarize(mean = mean(value)), aes(return, topic, fill=mean)) + geom_tile() + theme(axis.text.x=element_text(angle=-90))
#ggsave("/Users/mimno/Documents/Data/NoFBSurvey/returned_topic.pdf")

# exclude topics with non-English words
mask = grep("_tiempo_|_mais_|_mas_|_com_|_uma_|_mis_|_con_|_bem_|_het_|_niet_|_dat_|_vontade_|_muito_|_sinto_|_entrar_|_amigos_|_desafio_|_minha_|_bien_|_porque_|_cuenta_|_saber_|_cuanda_|_coisas_|_tempo_|_fejsbuk_|_telefoon_|_niet_|_estudio_",topics.labels) * -1

if (which.data == 33)
{
	wide.data <- dcast(melted.docs.and.topics %>% group_by(id, gender, return, consider, howhard, topic) %>% summarise(value = mean(log(value))), id + gender + return + consider + howhard ~ topic)
	wide.data[,6:30] <- wide.data[,6:30] - (rowSums(wide.data[,6:30]) / n.topics)
	formula.string <- paste(names(wide.data)[6:30][mask], collapse=" + ")
	user.topics <- as.matrix(wide.data[,6:30])
}
if (which.data == 66)
{
	wide.data <- dcast(melted.docs.and.topics %>% group_by(id, return, howhard, topic) %>% summarise(value = mean(log(value))), id + return + howhard ~ topic)
	wide.data[,4:28] <- wide.data[,4:28] - (rowSums(wide.data[,4:28]) / n.topics)
	formula.string <- paste(names(wide.data)[4:28][mask], collapse=" + ")
	user.topics <- as.matrix(wide.data[,4:28])
}
if (which.data == 99)
{
	wide.data <- dcast(melted.docs.and.topics %>% group_by(id, return, howhard, topic) %>% summarise(value = mean(log(value))), id + return + howhard ~ topic)
	wide.data[,4:28] <- wide.data[,4:28] - (rowSums(wide.data[,4:28]) / n.topics)
	formula.string <- paste(names(wide.data)[4:28][mask], collapse=" + ")
	user.topics <- as.matrix(wide.data[,4:28])
}

summary(glm(paste("return", " ~ ", formula.string), data=wide.data, family=binomial()))

for (i in 1:n.topics)
{
	all.topics[i + n.topics * run_num,] <- topic.sets[i,]
}

all.topic.labels[(1 + n.topics * run_num):(n.topics + n.topics * run_num)] <- topics.labels[1:25]

} # end for (run_num in 0:(num.runs - 1))

rownames(all.topics) <- all.topic.labels

topic.sim = matrix(nrow = n.topics * num.runs, ncol = n.topics * num.runs)
for (i in 1:(n.topics * num.runs))
{
    for (j in 1:(n.topics * num.runs))
    {
        topic.sim[i,j] = 1 - length(intersect(all.topics[i,], all.topics[j,])) / length(union(all.topics[i,], all.topics[j,]))
    }
}
#names(topic.sim) <- all.topic.labels
names(topic.sim) <- paste(all.topic.labels, as.character(rep(c(1:10), each=25)), as.character(rep(c(1:25), 10)), sep=" - ")
#rownames(topic.sim) <- all.topic.labels
rownames(topic.sim) <- paste(all.topic.labels, as.character(rep(c(1:10), each=25)), as.character(rep(c(1:25), 10)), sep=" - ")

hcl <- hclust(as.dist(topic.sim))
dendr <- dendro_data(hcl, type="rectangle") 
ggplot() + 
  geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=1) +
  coord_flip() + scale_y_reverse(expand=c(1.0, 0)) + 
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())



library(ggdendro)
library(ggplot2)

hcl <- hclust(dist(t(user.topics)))
dendr <- dendro_data(hcl, type="rectangle") 

ggplot() + 
  geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=5) +
  coord_flip() + scale_y_reverse(expand=c(1.0, 0)) + 
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())







# test which topics predict returning
frm <- paste('return ~ log(', paste(topics.labels[mask], collapse=") + log("), ')', sep='')
# result <- do.call("glm", list(as.formula(frm), data=docs.and.topics, family=binomial()))
# summary(result) # this provides lots of data spew--not sure how to get just a summary of the model

if (FALSE)
{
find.mode <- function(data=NULL)
{
	temp <- table(as.vector(data))
	mode <- as.numeric(names(temp[temp == max(temp)]))
	
	return(mode)
}
}

if (FALSE)
{
# doesn't work, can't figure out how to pass topic_var properly
test.indiv.topic <- function(topic_name, docs.and.topics)
{
	print(topic_name)
	# mode <- find.mode(docs.and.topics[topic_name])
	# model <- glm(return ~ eval(parse(text=topic_name)), family=binomial(),
			#data=subset(docs.and.topics, docs.and.topics[topic_name] > mode))
	model <- glm(return ~ eval(parse(text=topic_name)) > 0.1, family=binomial(),
			data=subset(docs.and.topics))
	summary(model)
}
}

# for each topic, floor out those below the estimated "mode"
# temps <- apply(docs.and.topics, 2, function(x) table(as.vector(x)))
# threshs <-


