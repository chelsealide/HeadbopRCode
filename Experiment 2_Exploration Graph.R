### In order to create a stacked bar chart wih ggplot2, the data needed to be substantially rearranged...

### Workflow: 

  # 1). Subset head and hand touches for each participant by condition 
  # 2). Sum head and hand touches within conditions 
  # 3). Take those sums, and repeat the condition name for the number of head touches, 
       # then condition name for number of hand touches. Put them into a vector (essentially, big long list of cond. tags for *every* touch)
  # 4). Repeat #3, but with the string "Head" for x number of head touches and "Hand" for x number of hand touches. Put them into a vector
  # 5). Melt those long vectors into a data frame 
  # 6). Graph! 


require(reshape2)
require(ggplot2)

# 1). --------
raw <- read.csv("CleanData.csv")

c4 <- subset(raw, Condition == "Exposed, Dax To", select = c(Head.Touches, Hand.Touches))
c3 <- subset(raw, Condition == "Exposed, Dax", select = c(Head.Touches, Hand.Touches))
c2 <- subset(raw, Condition == "Occupied, Dax To", select = c(Head.Touches, Hand.Touches))
c1 <- subset(raw, Condition == "Occupied, Dax", select = c(Head.Touches, Hand.Touches))

# 2). --------

a <- colSums(c4)
b <- colSums(c3)
c <- colSums(c2)
d <- colSums(c1)

# 3). --------

c4.cond <- c(rep.int("Exposed, Dax To", times =29), rep.int("Exposed, Dax To", times = 71))
c3.cond <- c(rep.int("Exposed, Dax", times =19), rep.int("Exposed, Dax", times = 66))
c2.cond <- c(rep.int("Occupied, Dax To", times =23), rep.int("Occupied, Dax To", times = 72))
c1.cond <- c(rep.int("Occupied, Dax", times =22), rep.int("Occupied, Dax", times = 59))


Conditions <- c(c4.cond, c3.cond, c2.cond, c1.cond)

# 4). --------

c4.head.hand <- c(rep.int("Head", times =29), rep.int("Hand", times = 71))
c3.head.hand <- c(rep.int("Head", times =19), rep.int("Hand", times = 66))
c2.head.hand <- c(rep.int("Head", times =23), rep.int("Hand", times = 72))
c1.head.hand <- c(rep.int("Head", times =22), rep.int("Hand", times = 59))

Responses <- c(c4.head.hand, c3.head.hand, c2.head.hand, c1.head.hand)

# 5). --------

df <- melt(data.frame(Conditions,Responses))
colnames(df) <- c("Conditons", "Responses")
df

# 6). -------


df$Responses <- factor(df$Responses, levels = rev(levels(df$Responses)))

p <- ggplot(data=df, aes(x=Conditions, fill=Responses)) + geom_bar() + theme_light() + 
  ylab("Count") + theme(axis.title.y=element_text(size = 13, face = "bold")) + labs(fill="Response Type") + ggtitle("Exploration Behaviors") + 
  theme(plot.title=element_text(vjust=1.5, face="bold")) + xlab("\n Condition") +theme(axis.title.x=element_text(vjust=.1, size = 13, face = "bold")) 
bp <- p + scale_x_discrete(limits=c("Exposed, Dax To","Exposed, Dax","Occupied, Dax To", "Occupied, Dax"), 
                           labels = c("Hands Exposed \n Manner Language","Hands Exposed \n Outcome Language","Hands Occupied \n Manner Language", "Hands Occupied \n Outcome Language"))
bp + scale_fill_manual(values=c("deepskyblue3", "chartreuse3"), 
                      breaks=c("Head", "Hand"),
                      labels=c("Head Touch", "Hand Touch")) 

