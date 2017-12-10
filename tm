library(dplyr)
library(tm)
library(stm)
# load the file
survey <- read.csv("tm.csv", head=T, sep=",",quote = "\"",stringsAsFactors=FALSE)

head(processed)

str(survey)
survey$comment <- gsub("phone", "", survey$comment, ignore.case=T)
survey$comment <- gsub("use", "", survey$comment, ignore.case=T)
survey$comment <- gsub("plan", "", survey$comment, ignore.case=T)
survey$comment <- gsub("package", "", survey$comment, ignore.case=T)

processed <- textProcessor(survey$comment, metadata=survey)
# associate text with metadata
out<-prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh=15)
# variables setting for convenience
docs <- out$documents
vocabs <- out$vocab
meta <-out$meta
# estimate the structural topic model 
PrevFit <- stm(docs, vocabs, K = 3, prevalence = ~Id,  max.em.its = 75, data = meta, init.type = "Spectral")

labelTopics(PrevFit)
# plot the topics
plot.STM(PrevFit, type="summary")



thought2 <- findThoughts(PrevFit, texts=out$meta$comment, topics=2, n=10)$docs[[1]]
thought3 <- findThoughts(PrevFit, texts=out$meta$comment, topics=3, n=10)$docs[[1]]
thought1 <- findThoughts(PrevFit, texts=out$meta$comment, topics=1, n=10)$docs[[1]]

par(mfrow = c(1, 2),mar = c(.5, .5, 1, .5))

plotQuote(thought2, width = 100, main = "Topic 2")
cloud(PrevFit, topic=2)
plotQuote(thought3, width = 100, main = "Topic 3") 
cloud(PrevFit, topic=3)
plotQuote(thought1, width = 100, main = "Topic 1")
cloud(PrevFit, topic=1)


