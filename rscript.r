library(rstudioapi) # load it
library(stringr) 

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
library("data.table")
library("bit64")
library(dplyr)
library("ggplot2")
library(Rmisc)


data <- fread('result.csv')
agents_3 <- data[data[,grep(3,num_agents)],]
agents_4 <- data[data[,grep(4,num_agents)],]
agents_5 <- data[data[,grep(5,num_agents)],]
agents_6 <- data[data[,grep(6,num_agents)],]
agents_7 <- data[data[,grep(7,num_agents)],]
agents_8 <- data[data[,grep(8,num_agents)],]
agents_9 <- data[data[,grep(9,num_agents)],]

agents_3$util_agent <- str_remove(agents_3$util_agent, "\\[")
agents_3$util_agent <- str_remove(agents_3$util_agent, "\\]")
agents_3$util_agent_splitted <- strsplit(agents_3$util_agent, ",")
agents_3_utils <- data.frame(matrix(unlist(agents_3$util_agent_splitted), nrow=length(agents_3$util_agent_splitted), byrow=T))
agents_3_utils_1 <- data.frame(y=as.numeric(paste(agents_3_utils$X1)))
agents_3_utils_2 <- data.frame(y=as.numeric(paste(agents_3_utils$X2)))
agents_3_utils_3 <- data.frame(y=as.numeric(paste(agents_3_utils$X3)))
agents_3_utils_1$row <- seq.int(nrow(agents_3_utils_1))
agents_3_utils_2$row <- seq.int(nrow(agents_3_utils_2))
agents_3_utils_3$row <- seq.int(nrow(agents_3_utils_3))
agents_3_utils_1$x <- 1
agents_3_utils_2$x <- 2
agents_3_utils_3$x <- 3

agents_3_thresholdA_60 <- agents_3[agents_3[,grep(60,threshold_approve)],]
agents_3_thresholdA_60_D20 <- agents_3_thresholdA_60[agents_3_thresholdA_60[,grep(20,threshold_disapprove)],]


agents_3_thresholdA_70 <- agents_3[agents_3[,grep(70,threshold_approve)],]
agents_3_thresholdA_80 <- agents_3[agents_3[,grep(80,threshold_approve)],]

agents_3_thresholdD_20 <- agents_3[agents_3[,grep(20,threshold_disapprove)],]
agents_3_thresholdD_30 <- agents_3[agents_3[,grep(30,threshold_disapprove)],]
agents_3_thresholdD_40 <- agents_3[agents_3[,grep(40,threshold_disapprove)],]


agents_3_all_utils <- rbind(agents_3_utils_1, agents_3_utils_2, agents_3_utils_3)

agents_3_utils_1_avg <- colMeans(agents_3_utils_1)
agents_3_utils_2_avg <- colMeans(agents_3_utils_2)
agents_3_utils_3_avg <- colMeans(agents_3_utils_3)

agents_3_all_utils_avg <- data.frame(rbind(agents_3_utils_1_avg, agents_3_utils_2_avg, agents_3_utils_3_avg))


ggplot(data=agents_3_all_utils, aes(x=x, y=y, group=row, scale_size(guide = "none"))) +
  geom_line(linetype="solid", color="black", alpha=0.1) +
  xlim(1, 3) +
  ylim(0.5, 0.9) + labs(x = "position in the game", y="utily") +
  geom_line(data=agents_3_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) + 
  theme(legend.position="right")


agents_4$util_agent <- str_remove(agents_4$util_agent, "\\[")
agents_4$util_agent <- str_remove(agents_4$util_agent, "\\]")
agents_4$util_agent_splitted <- strsplit(agents_4$util_agent, ",")

agents_5$util_agent <- str_remove(agents_5$util_agent, "\\[")
agents_5$util_agent <- str_remove(agents_5$util_agent, "\\]")
agents_5$util_agent_splitted <- strsplit(agents_5$util_agent, ",")

agents_6$util_agent <- str_remove(agents_6$util_agent, "\\[")
agents_6$util_agent <- str_remove(agents_6$util_agent, "\\]")
agents_6$util_agent_splitted <- strsplit(agents_6$util_agent, ",")

agents_7$util_agent <- str_remove(agents_7$util_agent, "\\[")
agents_7$util_agent <- str_remove(agents_7$util_agent, "\\]")
agents_7$util_agent_splitted <- strsplit(agents_7$util_agent, ",")

agents_8$util_agent <- str_remove(agents_8$util_agent, "\\[")
agents_8$util_agent <- str_remove(agents_8$util_agent, "\\]")
agents_8$util_agent_splitted <- strsplit(agents_8$util_agent, ",")

agents_9$util_agent <- str_remove(agents_9$util_agent, "\\[")
agents_9$util_agent <- str_remove(agents_9$util_agent, "\\]")
agents_9$util_agent_splitted <- strsplit(agents_9$util_agent, ",")




plot(agents_3$util_agent_splitted)
