library(dplyr)
library(mallet)
library(ggplot2)
library(reshape2)
library(ggdendro)

n.topics <- 10 # this might need to be, and probably should be, adjusted based on the total number of questions whose responses are being included as documents
num_long_words = 25 # the number of words to use for "long" topic labels
num.runs = 10 # number of total topic model solution runs
top.responses = 50 # how many top responses to list for each topic

setwd("/Users/ericpsb/Dropbox/Work/Projects/99Days/NoFBSurvey/")

# 33 day data

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

data33 <- read.csv("surveys_33_epsb.csv", colClasses=col.types, quote="\"\"")

data33 <- subset(data33, data33$survey_id != "")

data33$ConsideredBreak <- as.numeric(levels(data33$ConsideredBreak))[data33$ConsideredBreak]

data33$FeelDuringExp <- gsub("\\n", " ", data33$FeelDuringExp)
data33$FriendsReactExp <- gsub("\\n", " ", data33$FriendsReactExp)
data33$BestThingExp <- gsub("\\n", " ", data33$BestThingExp)
data33$WorstThingExp <- gsub("\\n", " ", data33$WorstThingExp)
data33$MotivationJoinExp <- gsub("\\n", " ", data33$MotivationJoinExp)
data33$FeelAboutReturning <- gsub("\\n", " ", data33$FeelAboutReturning)
data33$FriendsReactReturning <- gsub("\\n", " ", data33$FriendsReactReturning)
data33$MissMostAboutFB <- gsub("\\n", " ", data33$MissMostAboutFB)
data33$MostValuableFB <- gsub("\\n", " ", data33$MostValuableFB)
data33$MostNervousNext33Days <- gsub("\\n", " ", data33$MostNervousNext33Days)
data33$FunPlansNext33Days <- gsub("\\n", " ", data33$FunPlansNext33Days)

data33$Gender <- tolower(data33$Gender)
data33[ data33$Gender == "mujer", ]$Gender <- "f"
data33[ data33$Gender == "", ]$Gender <- "d"
data33[ grep("hombre", data33$Gender), ]$Gender <- "m"
data33[ grep("enski", data33$Gender), ]$Gender <- "f"
data33[ grep("boy", data33$Gender), ]$Gender <- "m"
data33[ grep("^n", data33$Gender), ]$Gender <- "m"
data33[ grep("^y", data33$Gender), ]$Gender <- "d"
data33[ grep("human being", data33$Gender), ]$Gender <- "d"
data33[ grep("alien", data33$Gender), ]$Gender <- "d"
data33[ grep("26", data33$Gender), ]$Gender <- "d"
data33[ grep("cis-female", data33$Gender), ]$Gender <- "f"
data33[ grep("happily female", data33$Gender), ]$Gender <- "f"
data33[ grep("^w", data33$Gender), ]$Gender <- "f"
data33[ grep("^v", data33$Gender), ]$Gender <- "f"
data33[ grep("^g", data33$Gender), ]$Gender <- "f"
data33[ grep("^f", data33$Gender), ]$Gender <- "f"
data33[ grep("^m", data33$Gender), ]$Gender <- "m"
data33[ grep("_", data33$Gender), ]$Gender <- "d"
data33[ grep("-", data33$Gender), ]$Gender <- "d"
data33$Gender <- as.factor(data33$Gender)

# end 33 day data

# 66 day data

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

data66 <- read.csv("surveys_66_epsb.csv", colClasses=col.types, quote="\"\"")

data66 <- subset(data66, data66$survey_id != "")

data66$RelationshipFriendsFamilyChanged <- gsub("\\n", " ", data66$RelationshipFriendsFamilyChanged)
data66$DailyRoutineChanged <- gsub("\\n", " ", data66$DailyRoutineChanged)
data66$BestThingSinceLastSurvey <- gsub("\\n", " ", data66$BestThingSinceLastSurvey)
data66$WorstThingSinceLastSurvey <- gsub("\\n", " ", data66$WorstThingSinceLastSurvey)
data66$FeelAboutReturning <- gsub("\\n", " ", data66$FeelAboutReturning)
data66$FriendsReactReturning <- gsub("\\n", " ", data66$FriendsReactReturning)
data66$HowSpendTimeUsedToOnFB <- gsub("\\n", " ", data66$HowSpendTimeUsedToOnFB)
data66$WhyOrNotPlanReturn <- gsub("\\n", " ", data66$WhyOrNotPlanReturn)
data66$MostNervousNext33Days <- gsub("\\n", " ", data66$MostNervousNext33Days)
data66$BestThingExpBrought <- gsub("\\n", " ", data66$BestThingExpBrought)

# end 66 day data

# 99 day data

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

data99 <- read.csv("surveys_99_epsb.csv", colClasses=col.types, quote="\"\"")

data99 <- subset(data99, data99$survey_id != "")

data99$RelationshipFriendsFamilyChanged <- gsub("\\n", " ", data99$RelationshipFriendsFamilyChanged)
data99$DailyRoutineChanged <- gsub("\\n", " ", data99$DailyRoutineChanged)
data99$BestThingSinceLastSurvey <- gsub("\\n", " ", data99$BestThingSinceLastSurvey)
data99$WorstThingSinceLastSurvey <- gsub("\\n", " ", data99$WorstThingSinceLastSurvey)
data99$FeelAboutReturning <- gsub("\\n", " ", data99$FeelAboutReturning)
data99$FriendsReactReturning <- gsub("\\n", " ", data99$FriendsReactReturning)
data99$HowSpendTimeUsedToOnFB <- gsub("\\n", " ", data99$HowSpendTimeUsedToOnFB)
data99$WhyOrNotPlanReturn <- gsub("\\n", " ", data99$WhyOrNotPlanReturn)
data99$BestThingExpBrought <- gsub("\\n", " ", data99$BestThingExpBrought)
data99$QuittingOtherHabits <- gsub("\\n", " ", data99$QuittingOtherHabits)
data99$PersonalInsightsGained <- gsub("\\n", " ", data99$PersonalInsightsGained)

# end 99 day data

# create documents across all 3 data sets

documents <- rbind(data.frame(id=data33$id, question="FeelAboutReturning", text=data33$FeelAboutReturning, howhard=data33$HowHardNotUsing, return=data33$ReturnedBool, morality=data33$Morality, stringsAsFactors=F),
				   data.frame(id=data33$id, question="FriendsReactReturning", text=data33$FriendsReactReturning, howhard=data33$HowHardNotUsing, return=data33$ReturnedBool, morality=data33$Morality, stringsAsFactors=F),
				   data.frame(id=data66$id, question="FeelAboutReturning", text=data66$FeelAboutReturning, howhard=data66$HowHardNotUsing, return=data66$ReturnedBool, morality=data66$Morality, stringsAsFactors=F),
				   data.frame(id=data66$id, question="FriendsReactReturning", text=data66$FriendsReactReturning, howhard=data66$HowHardNotUsing, return=data66$ReturnedBool, morality=data66$Morality, stringsAsFactors=F),
				   data.frame(id=data99$id, question="FeelAboutReturning", text=data99$FeelAboutReturning, howhard=data99$HowHardNotUsing, return=data99$ReturnedBool, morality=data99$Morality, stringsAsFactors=F),
				   data.frame(id=data99$id, question="FriendsReactReturning", text=data99$FriendsReactReturning, howhard=data99$HowHardNotUsing, return=data99$ReturnedBool, morality=data99$Morality, stringsAsFactors=F))

# only include documents with 3 or more words and without NA's
documents <- subset(documents, sapply(gregexpr("\\W+", text), length) + 1 > 3)



mallet.instances <- mallet.import(documents$id, documents$text, "nofb.stop", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

# these keep track of topics across multiple runs
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
	topics.long.labels[topic.i] <- paste(as.vector(mallet.top.words(topic.model, topic.words[topic.i,], num_long_words)[,1]), collapse = " ")
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

melted.docs.and.topics <- melt(docs.and.topics, id.vars=c("id", "question", "text", "return", "howhard"), variable.name="topic")

#ggplot(melted.docs.and.topics %>% group_by(question, topic) %>% summarize(mean = mean(value)), aes(question, topic, fill=mean)) + geom_tile() + theme(axis.text.x=element_text(angle=-90))
#ggsave("/Users/mimno/Documents/Data/NoFBSurvey/question_topic.pdf")

#ggplot(melted.docs.and.topics %>% group_by(return, topic) %>% summarize(mean = mean(value)), aes(return, topic, fill=mean)) + geom_tile() + theme(axis.text.x=element_text(angle=-90))
#ggsave("/Users/mimno/Documents/Data/NoFBSurvey/returned_topic.pdf")

# exclude topics with non-English words
mask = grep("_tiempo_|_mais_|_mas_|_com_|_uma_|_mis_|_con_|_bem_|_het_|_niet_|_dat_|_vontade_|_muito_|_sinto_|_entrar_|_amigos_|_desafio_|_minha_|_bien_|_porque_|_cuenta_|_saber_|_cuanda_|_coisas_|_tempo_|_fejsbuk_|_telefoon_|_niet_|_estudio_",topics.labels) * -1

wide.data <- dcast(melted.docs.and.topics %>% group_by(id, return, howhard, topic) %>% summarize(value = mean(log(value))), id + return + howhard ~ topic)
wide.data[,topics.labels] <- wide.data[,topics.labels] - (rowSums(wide.data[,topics.labels]) / n.topics)
formula.string <- paste(names(wide.data)[4:(3+n.topics)][mask], collapse = " + ")
user.topics <- as.matrix(wide.data[,topics.labels])

# this only made sense when we were predicting likelihood of returning
# summary(glm(paste("return", " ~ ", formula.string), data=wide.data, family=binomial()))

for (i in 1:n.topics)
{
	all.topics[i + n.topics * run_num,] <- topic.sets[i,]
}

all.topic.labels[(1 + n.topics * run_num):(n.topics + n.topics * run_num)] <- topics.labels[1:n.topics]

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
names(topic.sim) <- paste(all.topic.labels, as.character(rep(c(1:num.runs), each=n.topics)), as.character(rep(c(1:n.topics), num.runs)), sep=" - ")
#rownames(topic.sim) <- all.topic.labels
rownames(topic.sim) <- paste(all.topic.labels, as.character(rep(c(1:num.runs), each=n.topics)), as.character(rep(c(1:n.topics), num.runs)), sep=" - ")


# generate the dendrogram of topics clustered across solutions
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



# treat the nth solution as the reference solution. for each topic in the reference solution, list the 50 responses with the highest proportion for that topic.
for (t in topics.labels)
{
	# sort by topic t
	sorted.by.t <- docs.and.topics[do.call(order, list(docs.and.topics[, match(t, names(docs.and.topics))], decreasing=TRUE)),]
	# list the responses with the 50 highest scores for topic t
	writeLines(sprintf("*** Top %d responses for: %s ***\n", top.responses, t))
	print(sorted.by.t$text[1:top.responses])
	writeLines("\n")
}






# test which topics predict returning
#frm <- paste('return ~ log(', paste(topics.labels[mask], collapse=") + log("), ')', sep='')
# result <- do.call("glm", list(as.formula(frm), data=docs.and.topics, family=binomial()))
# summary(result) # this provides lots of data spew--not sure how to get just a summary of the model
