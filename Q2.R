library(XML)
library(data.table)

# Tags analysis

docTags <- xmlTreeParse("./Tags.xml", useInternal = TRUE)
rootNodeTags <- xmlRoot(docTags)

count <- unname(xpathSApply(rootNodeTags, "//row/@Count"))
class(count) <- "numeric"

sortedCount <- sort(count, decreasing = TRUE)
fraction5 <- sortedCount[5] / sum(count)

cat(sprintf("The 5th most popular tag has a fraction %s", fraction5))


# Post scores analysis

docPosts <- xmlTreeParse("./Posts.xml", useInternal = TRUE)
rootNodePosts <- xmlRoot(docPosts)

questionScores <- unname(xpathSApply(rootNodePosts, "//row[@PostTypeId='1']/@Score"))
class(questionScores) <- "numeric"

answerScores <- unname(xpathSApply(rootNodePosts, "//row[@PostTypeId='2']/@Score"))
class(answerScores ) <- "numeric"

ans <- mean(answerScores) / mean(questionScores)

cat(sprintf("The average answer's score than the average question's is higher by %s", 1/ans))


# Pearson's correlation for users

docUsers <- xmlTreeParse("./Users.xml", useInternal = TRUE)
rootNodeUsers <- xmlRoot(docUsers)

reputation <- unname(xpathSApply(rootNodeUsers, "//row/@Reputation"))
class(reputation) <- "numeric"

upVotes <- unname(xpathSApply(rootNodeUsers, "//row/@UpVotes"))
class(upVotes) <- "numeric"
downVotes <- unname(xpathSApply(rootNodeUsers, "//row/@DownVotes"))
class(downVotes) <- "numeric"
totalScore <- upVotes - downVotes

pearson <- cor(reputation[-1], totalScore[-1], method = "pearson")
cat(sprintf("Pearson's correlation between a user's reputation and total score from posts is %s", pearson))

# Upvotes between the average answer and the average question


# Time of the day for best accepted response
# Take data from the previous steps

questions <- xpathApply(rootNodePosts, "//row[@PostTypeId='1']", function(x){
  attrs <- xmlAttrs(x)
  qDate <- sapply(attrs['CreationDate'],gsub,pattern="T",replacement=" ")
  qDate <- as.POSIXlt(as.character(qDate), format="%Y-%m-%d %H:%M:%S")
  data.frame(qId = attrs['Id'], acceptedId = attrs["AcceptedAnswerId"], 
             qDate = qDate, stringsAsFactors = FALSE)
})
qDF <- do.call(rbind.data.frame, questions)

answers <- xpathApply(rootNodePosts, "//row[@PostTypeId='2']", function(x){
  attrs <- xmlAttrs(x)
  aDate <- sapply(attrs['CreationDate'],gsub,pattern="T",replacement=" ")
  aDate <- as.POSIXlt(as.character(aDate), format="%Y-%m-%d %H:%M:%S")
  data.frame(ansId = attrs['Id'], aDate = aDate, stringsAsFactors = FALSE)
})
ansDF <- do.call(rbind.data.frame, answers)

mergedDF <- merge(qDF, ansDF, by.x = "acceptedId", by.y = "ansId")

qHours <- as.POSIXlt(mergedDF$qDate, format="%Y-%m-%d %H:%M:%S")$hour

diff <- difftime(as.POSIXct(mergedDF$aDate), as.POSIXct(mergedDF$qDate), units="hours")
ans = data.frame(qHour = qHours, diff = diff)

result <- aggregate(ans$diff, by=list(ans$qHour), FUN=median)

medianDiff <- max(unlist(result[2])) - min(unlist(result[2]))
cat(sprintf("Difference between the largest and smallest median response times is %s hours", medianDiff))


# Probability

qUser <- xpathApply(rootNodePosts, "//row[@PostTypeId='1'][@OwnerUserId]", function(x){
  attrs <- xmlAttrs(x)
  date <- sapply(attrs['CreationDate'],gsub,pattern="T",replacement=" ")
  date <- as.POSIXlt(as.character(date), format="%Y-%m-%d %H:%M:%S")
  data.frame(userId = attrs['OwnerUserId'], date = date, stringsAsFactors = FALSE)
})
df1 <- data.frame(rbindlist(qUser))

aUser <- xpathApply(rootNodePosts, "//row[@PostTypeId='2'][@OwnerUserId]", function(x){
  attrs <- xmlAttrs(x)
  date <- sapply(attrs['CreationDate'],gsub,pattern="T",replacement=" ")
  date <- as.POSIXlt(as.character(date), format="%Y-%m-%d %H:%M:%S")
  data.frame(userId = attrs['OwnerUserId'], date = date, stringsAsFactors = FALSE)
})
df2 <- data.frame(rbindlist(aUser))

docCom <- xmlTreeParse("./Comments.xml", useInternal = TRUE)
rootNodeCom <- xmlRoot(docCom)

cUser <- xpathApply(rootNodeCom, "//row[@UserId]", function(x){
  attrs <- xmlAttrs(x)
  date <- sapply(attrs['CreationDate'],gsub,pattern="T",replacement=" ")
  date <- as.POSIXlt(as.character(date), format="%Y-%m-%d %H:%M:%S")
  data.frame(userId = attrs['UserId'], date = date, stringsAsFactors = FALSE)
})
df3 <- data.frame(rbindlist(cUser))


# type = 1 - question
# type = 2 - answer
# type = 3 - comment

UserFilter <- function(user, df, type=0){
  dates <- df[df$userId == user,]['date']
  types <- rep(type, each=length(unlist(dates)))
  resultDf <- data.frame(type = types, date = dates)
  return (resultDf)
}

UserEvents <- function(user){
  events <- data.frame(type=numeric(), 
                           date=as.POSIXlt(as.character(), format="%Y-%m-%d %H:%M:%S"))
  events1 <- rbind(events, UserFilter(user, df1, 1))
  events2 <- rbind(events, UserFilter(user, df2, 2))
  events3 <- rbind(events, UserFilter(user, df3, 3))
  events <- rbind(events1, events2, events3)
  events <- events[with(events, order(date)), ] # sorting events chronologically 
  return (events)
}

CondEventCount <- function(userEvents, firstEvent, secondEvent){
  count = 0
  if (nrow(userEvents) == 1) return(0)
  for (event in 1:(nrow(userEvents)-1)){
    if ((userEvents$type[event] == firstEvent) && (userEvents$type[event+1] == secondEvent)){
      count = count + 1
    }    
  }
  return (count)
}

users <- c(df1['userId'], df2['userId'], df3['userId'])
usersList <- as.list(unique(unlist(users, use.names = FALSE)))

probs <- data.frame(matrix(nrow=0, ncol=13))
names(probs) <- c('user', 'p1', 'p2', 'p3', 'p11', 'p12', 
                  'p13', 'p21', 'p22', 'p23', 'p31', 'p32', 'p33')
for (num in 1:length(usersList)){
  newEntry <- vector("list", 13)
  userEvents <- UserEvents(usersList[[num]])
  newEntry[1] = usersList[num]
  for (i in 1:3){
    probCount <- nrow(userEvents[userEvents$type == i,]) / nrow(userEvents)
    newEntry[1 + i] = probCount

    for (j in 1:3){
      condProbCount <- CondEventCount(userEvents, i, j) / (nrow(userEvents)-1)
      newEntry[1 + 3*i + j] = condProbCount
    }
  }
  
  probs[num,] <- newEntry
}

quotients <- data.frame(matrix(nrow=0, ncol=3))
names(quotients) <- c(1, 2, 3)

for (row in 1:nrow(probs)){
  newrow <- vector("list", 3)
  for(i in 1:3){
    maxProb <- max(probs[row, 4+i], probs[row, 7+i], probs[row, 10+i])
    newrow[i] <- maxProb / probs[row, 1 + i]
  }
  
  quotients[row,] <- newrow
}

MadMax <- max(quotients, na.rm=TRUE)
cat(sprintf("The largest quotient is %s", MadMax)) # = 2


