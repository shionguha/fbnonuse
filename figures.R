melted.docs.and.topics <- read.csv("/Users/mimno/Documents/Data/NoFBSurvey/melted.docs.and.topics")

ggplot(melted.docs.and.topics %>% group_by(question, topic) %>% summarize(mean = mean(value)), aes(question, topic, fill=mean)) + geom_tile() + theme(axis.text.x=element_text(angle=-90))
ggsave("/Users/mimno/Documents/Data/NoFBSurvey/question_topic.pdf")

ggplot(melted.docs.and.topics %>% group_by(return, topic) %>% summarize(mean = mean(value)), aes(return, topic, fill=mean)) + geom_tile() + theme(axis.text.x=element_text(angle=-90))
ggsave("/Users/mimno/Documents/Data/NoFBSurvey/returned_topic.pdf")

wide.data <- dcast(melted.docs.and.topics %>% group_by(id, gender, return, consider, howhard, topic) %>% summarise(value = mean(log(value))), id + gender + return + consider + howhard ~ topic)

wide.data[,6:30] <- wide.data[,6:30] - (rowSums(wide.data[,6:30]) / 25)

formula.string <- paste(names(wide.data)[6:30], collapse=" + ")
summary(glm(paste("return", " ~ ", formula.string), data=wide.data, family=binomial()))

user.topics <- as.matrix(wide.data[,6:30])
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
