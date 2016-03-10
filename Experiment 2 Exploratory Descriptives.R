### Experiment 2 Exploratory Descriptives 
### Chelsea Lide
### Harvard University, 2016


require(ggplot2)

### Table of Contents ----

  # 1. Age per condition 
  # 2. MCDI per condition
  # 3. Durations per condition 
        # a. Total Trial 
        # b. Warm Up 
        # c. Demonstration
        # d. First Response 
        # e. Exploration 
        # f. Engagement
        # f. IBAs
  # 4. Total Number of Responses per condition 


### Load in and recode data ----

  data <- read.csv("CleanData.csv")
  
# Recode conditions (groups) as numeric 
  
  data$Condition<-as.numeric(data$Condition)
  
# Added intermediary step to preserve recoding values 
# Exposed, Dax To = 4; Exposed, Dax = 3; Occupied, Dax To = 2; Occupied Dax = 1 
  
  data$Condition[data$Condition==2]<-0
  data$Condition[data$Condition==4]<-2
  data$Condition[data$Condition==0]<-4
  data$Condition[data$Condition==1]<-9
  data$Condition[data$Condition==3]<-1
  data$Condition[data$Condition==9]<-3
  
attach(data)
  

### 1. Age per condition ----
  
  # Subset group and age 
  
  age <- data$Days.Old
  group <- data$Condition
  dat = data.frame(age = age, group = factor(group))
  
  # ANOVA

  fit = lm(age ~ group, dat)
  anova(fit)
  
  # Graph
  
  ggplot(dat, aes(x=group, y=age, fill=group)) + geom_boxplot() + xlab("Condition") + ylab("Age (days old) \n") + 
    ggtitle("Age Distribution per Condition \n") + scale_fill_discrete(name="\n Condition",
                          breaks=c("4", "3", "2", "1"),
                          labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))
  
  
  
  
  

### 2. MCDI per condition ----
  
  # Subset group and MCDI 
  
  MCDI <- data$MCDI
  group <- data$Condition
  dat = data.frame(MCDI = MCDI, group = factor(group))
  
  # ANOVA
  
  fit1 = lm(MCDI ~ group, dat)
  anova(fit1)
  
  # Graph
  
  ggplot(dat, aes(x=group, y=MCDI, fill=group)) + geom_boxplot() + xlab("Condition") + ylab("Vocabulary Score") + 
    ggtitle("MCDI Score Distribution per Condition") + 
    scale_fill_discrete(name="Condition", 
           breaks=c("4", "3", "2", "1"), 
           labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax"))
  

### 3. Durations per condition ----
  
  # a. Total Trial 
  
      # Subset group and total trial duration 
  
         total <- data$Total.Dur
         group <- data$Condition
         dat = data.frame(total = total, group = factor(group))
  
       # ANOVA
  
         fit2 = lm(total ~ group, dat)
         anova(fit2)
         
         # Graph
         
         ggplot(dat, aes(x=group, y=total, fill=group)) + geom_boxplot() + xlab("Condition") + ylab("Duration (s)") + coord_flip() + 
           ggtitle("Total Trial Duration Distribution per Condition") + 
           scale_fill_discrete(name="Condition", breaks=c("4", "3", "2", "1"), labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")) 
           
  
  # b. Warm Up 
  
      # Subset group and warmup duration 
  
          warmup <- data$Warmup.Dur
          group <- data$Condition
          dat = data.frame(warmup = warmup, group = factor(group))
  
       # ANOVA
   
          fit3 = lm(warmup ~ group, dat)
          anova(fit3)
  
      # Graph
          
          ggplot(dat, aes(x=group, y=warmup, fill=group)) + geom_boxplot() + xlab("Condition") + ylab("Duration (s)") + coord_flip() + 
            ggtitle("Warm-Up Duration Distribution per Condition") + 
            scale_fill_discrete(name="Condition", breaks=c("4", "3", "2", "1"), labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")) 
          

  # c. Demonstration
  
        # Subset group and demonstration duration 
  
          demo <- data$Demo.Dur
          group <- data$Condition
          dat = data.frame(demo = demo, group = factor(group))
  
        # ANOVA
  
          fit4 = lm(demo ~ group, dat)
          anova(fit4)
          summary(fit4)
          
        # Graph 
          
          ggplot(dat, aes(x=group, y=demo, fill=group)) + geom_boxplot() + theme_linedraw() + coord_flip() +
            xlab("Condition") + theme(axis.title.x=element_text(vjust=.1, size = 13, face = "bold")) +
            ylab("Duration (s)") + theme(axis.title.y=element_text(size = 13, face = "bold")) +
            ggtitle("Duration of Experimenter's Demonstration \n") + theme(plot.title=element_text(vjust=1.5, face="bold", size = 15)) +
            scale_fill_discrete(name="Condition", breaks=c("4", "3", "2", "1"), labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")) +
            theme(axis.ticks = element_blank(), axis.text.y = element_blank())
          
  # Checking means of each group 
          
          tapply(demo, group, mean) 
          # Doesn't seem that different... 
          
          # Take out huge outliers in group 4 to test if it has an effect...
            temp.data <- data[(data$Demo.Dur < 55),]
          # Rerun analyses 
            demo.test <- temp.data$Demo.Dur
            group <- temp.data$Condition
            dat = data.frame(demo = demo.test, group = factor(group))
          # ANOVA
            testfit = lm(demo ~ group, dat)
            anova(testfit)
          
          
  
  # d. First Response 
  
        # Subset group and FR window duration 
  
          fr <- data$FirstResponse.Dur
          group <- data$Condition
          dat = data.frame(fr = fr, group = factor(group))
  
        # ANOVA
  
          fit5 = lm(fr ~ group, dat)
          anova(fit5)
          
        # Graph
          
          ggplot(dat, aes(x=group, y=fr, fill=group)) + geom_boxplot() + xlab("Condition") + ylab("Duration (s)") + coord_flip() + 
            ggtitle("First Response Window Duration Distribution per Condition") + 
            scale_fill_discrete(name="Condition", breaks=c("4", "3", "2", "1"), labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")) 
        
  
  
  # e. Exploration 
  
        # Subset group and exploration duration 
  
          explore <- data$Exploration.Dur
          group <- data$Condition
          dat = data.frame(explore = explore, group = factor(group))
  
        # ANOVA
  
          fit6 = lm(explore ~ group, dat)
          anova(fit6)
          
        # Graph 
          
          ggplot(dat, aes(x=group, y=explore, fill=group)) + geom_boxplot() + xlab("Condition") + ylab("Duration (s)") + coord_flip() + 
            ggtitle("Exploration Period Duration Distribution per Condition") + 
            scale_fill_discrete(name="Condition", breaks=c("4", "3", "2", "1"), labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")) 
          
  # f. Engagement
          
          # Subset group and Engagement duration 
          
          engagement <- data$Engagement.Duration
          group <- data$Condition
          dat = data.frame(demo = engagement, group = factor(group))
          
          # ANOVA
          
          fita = lm(engagement ~ group, dat)
          anova(fita)
          summary(fita)
          
          # Graph 
          
          ggplot(dat, aes(x=group, y=engagement, fill=group)) + geom_boxplot() + xlab("Condition") + ylab("Duration (s)") + coord_flip() + 
            ggtitle("Engagement Duration Distribution per Condition") + 
            scale_fill_discrete(name="Condition", breaks=c("4", "3", "2", "1"), labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")) 
          
  # g. IBAs
          
          # Subset group and IBA duration 
          
          iba <- data$IBA.Duration
          group <- data$Condition
          dat = data.frame(demo = iba, group = factor(group))
          
          # ANOVA
          
          fitb = lm(iba ~ group, dat)
          anova(fitb)
          summary(fitb)
          
          # Graph 
          
          ggplot(dat, aes(x=group, y=iba, fill=group)) + geom_boxplot() + theme_linedraw() + coord_flip() +
            xlab("Condition") + theme(axis.title.x=element_text(vjust=.1, size = 13, face = "bold")) +
            ylab("Duration (s)") + theme(axis.title.y=element_text(size = 13, face = "bold")) +
            ggtitle("IBA Duration \n") + theme(plot.title=element_text(vjust=1.5, face="bold", size = 15)) +
            scale_fill_discrete(name="Condition", breaks=c("4", "3", "2", "1"), labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")) +
            theme(axis.ticks = element_blank(), axis.text.y = element_blank())
          
          
          
          ggplot(dat, aes(x=group, y=iba, fill=group)) + geom_boxplot() + xlab("Condition") + ylab("Duration (s)") + coord_flip() + 
            ggtitle("IBA Duration Distribution per Condition") + theme(plot.title=element_text(face="bold")) +
            scale_fill_discrete(name="Condition", breaks=c("4", "3", "2", "1"), labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")) 
          
          
          
          
          
          
### 4. Total Number of Responses per condition ----
          
    # Subset group and total responses 
          
        responses <- data$Total.Responses
        group <- data$Condition
        dat = data.frame(responses = responses, group = factor(group))
          
    # ANOVA
          
        fit7 = lm(responses ~ group, dat)
        anova(fit7)
        
    # Graph 
        
        ggplot(dat, aes(x=group, y=responses, fill=group)) + geom_boxplot() + xlab("Condition") + ylab("Duration (s)") +
          ggtitle("Distribution of Response Counts per Condition") + 
          scale_fill_discrete(name="Condition", breaks=c("4", "3", "2", "1"), labels=c("Exposed, Dax To", "Exposed, Dax", "Occupied, Dax To", "Occupied, Dax")) 
        

        
detach(data)
        
        
        