# CDS

library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(tidytext)
library(tidyr)

df <- read_excel("CDS initiatives - lasts.xlsx")
df$num <- 1
d2 <- df %>% select(ori_name_ap, outcome)

df$axis[df$axis==1] <- "Defense Policies"
df$axis[df$axis==2] <- "Military Cooperation, Humanitarian Action, and Peace Operations"
df$axis[df$axis==3] <- "Defense Industry"
df$axis[df$axis==4] <- "Education and Capacity Building"

df$proposition[df$proposition == "n/a"] <- NA
df$proposition <- as.numeric(df$proposition)
df$proposition <- as.Date(df$proposition, origin = "1899-12-30")

df$year <- year(df$proposition)

## Graph 1 - Leader

leader <- separate_rows(df,leader, sep="; ")
ord <- aggregate(leader$num,list(leader$leader), sum, na.rm=T)
leader <- aggregate(
  leader$num, list(leader$leader, leader$axis), sum, na.rm=T)
leader <- leader %>% left_join(ord, by="Group.1")
leader %>% filter(Group.1 != "n/a" & Group.1 != "Pro Tempore Presidency" &
                    Group.2 != "CBM" & Group.2 != "TP") %>%
  ggplot(aes(x= reorder(Group.1, -x.y), x.x, fill=Group.2))+
  geom_bar(stat="identity") + 
  labs(x="Country", y="Total Activities", fill="Axis") + 
  theme(panel.background = element_rect(fill='white', colour='black'), 
        legend.position = "bottom")
install.packages("dygraphs")
## Graph 2 - Axis/year

axis <- aggregate(num ~ axis + first_ap, data=df, sum) %>%
  complete(axis, first_ap = 2010:2016, fill = list(num=0))
axis %>% ggplot(aes(first_ap, num, colour=axis)) + geom_point(aes(shape=axis)) +
  geom_line(aes(linetype=axis)) + 
  labs(x="Year", y="Total Activities", colour="Axis", shape="Axis", 
       linetype="Axis") + 
  scale_y_continuous(breaks=c(0, 3, 6, 9, 12)) + 
  theme(panel.background = element_rect(fill='white', colour='black'), 
        legend.position = "bottom")

## Graph 3 - Implementation/axis

axisimp <- aggregate(num ~ axis + result, data=df, sum)
axisimp %>% filter(axis != "CEED" & axis != "AMC" & axis != "HAI" & 
                     axis != "TP" & result != "Continuous") %>%
  ggplot(aes(x= reorder(axis, -num), num, fill=factor(result)))+
  geom_bar(stat="identity") + 
  labs(x="Country", y="Total Activities", fill="Result") + 
  theme(panel.background = element_rect(fill='white', colour='black'), 
        legend.position = "bottom") +
  scale_x_discrete(name="Axis", 
                   labels=c("Defense Policies" = "Defense Policies", 
                            "Education and Training" = "Education and Capacity Building",
                            "Military Cooperation, Humanitarian Action, and Peace Operations" = "Mil. Coop., Hum. action, and Peace Op.", 
                            "Defense Industry and Technology" = "Defense Industry",
                            "CBM" = "Confidence Building Measures"))

## Graph 4 - Issue

ord2 <- aggregate(num ~ issue, data=df, sum)
issue <- aggregate(num ~ issue + result, data=df, sum)
issue <- issue %>% left_join(ord2, by="issue")
issue$issue[issue$issue == "Mutual Confidence Measures"] <- "Confidence Building"
issue %>% filter(result != "Continuous") %>% 
  ggplot(aes(x=reorder(issue, num.y), num.x, fill=factor(result))) +
  geom_bar(stat="identity") + coord_flip() +
  labs(x="Issue", y="Total initiatives", fill="Result") + 
  theme(panel.background = element_rect(fill='white', colour='black'), 
        legend.position = "bottom")

## Graph 5 - Outcomes

out <- aggregate(num ~ outcome + result, data=df, sum)
ord3 <- aggregate(num~outcome, data=df, sum)
out <- out %>% left_join(ord3, by="outcome")
out %>% filter(result != "Continuous") %>% 
  ggplot(aes(x=reorder(outcome, num.y), num.x, fill=factor(result)))+
  geom_bar(stat="identity") + coord_flip() +
  labs(x="Outcome", y="Total initiatives", fill="Result") + 
  theme(panel.background = element_rect(fill='white', colour='black'), 
        legend.position = "bottom")

## Graph 6 - Issues/Outcomes

df2 <- df %>% filter(result=="Implemented")
ord3 <- aggregate(num ~ issue, data=df2, sum)
issueout <- aggregate(num ~ issue + outcome, data=df2, sum)
issueout <- issueout %>% left_join(ord3, by="issue")
issueout$issue[issueout$issue == "Mutual Confidence Measures"] <- "Confidence Building"
issueout %>%
  ggplot(aes(x=reorder(issue, num.y), num.x, fill=factor(outcome))) +
  geom_bar(stat="identity") + coord_flip() +
  labs(x="Issue", y="Total initiatives", fill="Outcome") +
  theme(panel.background = element_rect(fill='white', colour='black'), 
        legend.position = "bottom")

## Appendix 1 - Issues' codification
library(xlsx)
df2 <- df %>% select(ori_name_ap, issue)
issues <- plyr::ddply(df2, "issue", summarize, initiatives = paste(ori_name_ap, collapse=";"))
write.xlsx(issues, "issues.xlsx")
?write.xlsx
write.csv(issues, "issues.csv")

## Appendix 2 - Outcomes' codification

df3 <- df %>% select(ori_name_ap, outcome)
outcomes <- plyr::ddply(df3, "outcome", summarize, initiatives = paste(ori_name_ap, collapse=","))
write.xlsx(outcomes, "outcomes.xlsx")
