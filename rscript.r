library(rstudioapi) # load it
library(stringr) 

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
library("data.table")
library("bit64")
library(dplyr)
library("ggplot2")
library(Rmisc)
library(dplyr)


data <- fread('result3_9.csv')
data2 <- fread('result10_16.csv')
agents_3 <- data[data[,grep(3,num_agents)],]
agents_4 <- data[data[,grep(4,num_agents)],]
agents_5 <- data[data[,grep(5,num_agents)],]
agents_6 <- data[data[,grep(6,num_agents)],]
agents_7 <- data[data[,grep(7,num_agents)],]
agents_8 <- data[data[,grep(8,num_agents)],]
agents_9 <- data[data[,grep(9,num_agents)],]
agents_16 <- data2[data2[,grep(16,num_agents)],]


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

agents_3_all_utils <- rbind(agents_3_utils_1, agents_3_utils_2, agents_3_utils_3)

agents_3_utils_1_avg <- colMeans(agents_3_utils_1)
agents_3_utils_2_avg <- colMeans(agents_3_utils_2)
agents_3_utils_3_avg <- colMeans(agents_3_utils_3)

agents_3_all_utils_avg <- data.frame(rbind(agents_3_utils_1_avg, agents_3_utils_2_avg, agents_3_utils_3_avg))
agents_3_all_utils_avg

#APPROVE 60
agents_3_thresholdA_60 <- agents_3[agents_3[,grep(60,threshold_approve)],]
agents_3_thresholdA_60_D20 <- agents_3_thresholdA_60[agents_3_thresholdA_60[,grep(20,threshold_disapprove)],]
agents_3_thresholdA_60_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_60_D20$util_agent_splitted))), nrow=length(agents_3_thresholdA_60_D20$util_agent_splitted), byrow=T))
agents_3_thresholdA_60_D20_utils_avg <- colMeans(agents_3_thresholdA_60_D20_utils)
agents_3_thresholdA_60_D20_utils_avg <- data.frame(y = agents_3_thresholdA_60_D20_utils_avg)
agents_3_thresholdA_60_D20_utils_avg$x <- seq.int(nrow(agents_3_thresholdA_60_D20_utils_avg))
agents_3_thresholdA_60_D20_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_60_D20_utils_avg_2$y[1] <- agents_3_thresholdA_60_D20_utils_avg$y[1]
agents_3_thresholdA_60_D20_utils_avg_2$y[2] <- agents_3_thresholdA_60_D20_utils_avg$y[2]
agents_3_thresholdA_60_D20_utils_avg_2$y[3] <- agents_3_thresholdA_60_D20_utils_avg$y[3]

agents_3_thresholdA_60_D30 <- agents_3_thresholdA_60[agents_3_thresholdA_60[,grep(30,threshold_disapprove)],]
agents_3_thresholdA_60_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_60_D30$util_agent_splitted))), nrow=length(agents_3_thresholdA_60_D30$util_agent_splitted), byrow=T))
agents_3_thresholdA_60_D30_utils_avg <- colMeans(agents_3_thresholdA_60_D30_utils)
agents_3_thresholdA_60_D30_utils_avg <- data.frame(y = agents_3_thresholdA_60_D30_utils_avg)
agents_3_thresholdA_60_D30_utils_avg$x <- seq.int(nrow(agents_3_thresholdA_60_D30_utils_avg))
agents_3_thresholdA_60_D30_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_60_D30_utils_avg_2$y[1] <- agents_3_thresholdA_60_D30_utils_avg$y[1]
agents_3_thresholdA_60_D30_utils_avg_2$y[2] <- agents_3_thresholdA_60_D30_utils_avg$y[2]
agents_3_thresholdA_60_D30_utils_avg_2$y[3] <- agents_3_thresholdA_60_D30_utils_avg$y[3]

agents_3_thresholdA_60_D40 <- agents_3_thresholdA_60[agents_3_thresholdA_60[,grep(40,threshold_disapprove)],]
agents_3_thresholdA_60_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_60_D40$util_agent_splitted))), nrow=length(agents_3_thresholdA_60_D40$util_agent_splitted), byrow=T))
agents_3_thresholdA_60_D40_utils_avg <- colMeans(agents_3_thresholdA_60_D40_utils)
agents_3_thresholdA_60_D40_utils_avg <- data.frame(y = agents_3_thresholdA_60_D40_utils_avg)
agents_3_thresholdA_60_D40_utils_avg$x <- seq.int(nrow(agents_3_thresholdA_60_D40_utils_avg))
agents_3_thresholdA_60_D40_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_60_D40_utils_avg_2$y[1] <- agents_3_thresholdA_60_D40_utils_avg$y[1]
agents_3_thresholdA_60_D40_utils_avg_2$y[2] <- agents_3_thresholdA_60_D40_utils_avg$y[2]
agents_3_thresholdA_60_D40_utils_avg_2$y[3] <- agents_3_thresholdA_60_D40_utils_avg$y[3]

agents_3_threshold_A60_avg <- data.frame(rbind(agents_3_thresholdA_60_D20_utils_avg_2, agents_3_thresholdA_60_D30_utils_avg_2,agents_3_thresholdA_60_D40_utils_avg_2))
agents_3_threshold_A60_avg$x <- seq.int(nrow(agents_3_threshold_A60_avg))

#APPROVE 70
agents_3_thresholdA_70 <- agents_3[agents_3[,grep(70,threshold_approve)],]
agents_3_thresholdA_70_D20 <- agents_3_thresholdA_70[agents_3_thresholdA_70[,grep(20,threshold_disapprove)],]
agents_3_thresholdA_70_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_70_D20$util_agent_splitted))), nrow=length(agents_3_thresholdA_70_D20$util_agent_splitted), byrow=T))
agents_3_thresholdA_70_D20_utils_avg <- colMeans(agents_3_thresholdA_70_D20_utils)
agents_3_thresholdA_70_D20_utils_avg <- data.frame(y = agents_3_thresholdA_70_D20_utils_avg)
agents_3_thresholdA_70_D20_utils_avg$x <- seq.int(nrow(agents_3_thresholdA_70_D20_utils_avg))
agents_3_thresholdA_70_D20_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_70_D20_utils_avg_2$y[1] <- agents_3_thresholdA_70_D20_utils_avg$y[1]
agents_3_thresholdA_70_D20_utils_avg_2$y[2] <- agents_3_thresholdA_70_D20_utils_avg$y[2]
agents_3_thresholdA_70_D20_utils_avg_2$y[3] <- agents_3_thresholdA_70_D20_utils_avg$y[3]

agents_3_thresholdA_70_D30 <- agents_3_thresholdA_70[agents_3_thresholdA_70[,grep(30,threshold_disapprove)],]
agents_3_thresholdA_70_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_70_D30$util_agent_splitted))), nrow=length(agents_3_thresholdA_70_D30$util_agent_splitted), byrow=T))
agents_3_thresholdA_70_D30_utils_avg <- colMeans(agents_3_thresholdA_70_D30_utils)
agents_3_thresholdA_70_D30_utils_avg <- data.frame(y = agents_3_thresholdA_70_D30_utils_avg)
agents_3_thresholdA_70_D30_utils_avg$x <- seq.int(nrow(agents_3_thresholdA_70_D30_utils_avg))
agents_3_thresholdA_70_D30_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_70_D30_utils_avg_2$y[1] <- agents_3_thresholdA_70_D30_utils_avg$y[1]
agents_3_thresholdA_70_D30_utils_avg_2$y[2] <- agents_3_thresholdA_70_D30_utils_avg$y[2]
agents_3_thresholdA_70_D30_utils_avg_2$y[3] <- agents_3_thresholdA_70_D30_utils_avg$y[3]

agents_3_thresholdA_70_D40 <- agents_3_thresholdA_70[agents_3_thresholdA_70[,grep(40,threshold_disapprove)],]
agents_3_thresholdA_70_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_70_D40$util_agent_splitted))), nrow=length(agents_3_thresholdA_70_D40$util_agent_splitted), byrow=T))
agents_3_thresholdA_70_D40_utils_avg <- colMeans(agents_3_thresholdA_70_D40_utils)
agents_3_thresholdA_70_D40_utils_avg <- data.frame(y = agents_3_thresholdA_70_D40_utils_avg)
agents_3_thresholdA_70_D40_utils_avg$x <- seq.int(nrow(agents_3_thresholdA_70_D40_utils_avg))
agents_3_thresholdA_70_D40_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_70_D40_utils_avg_2$y[1] <- agents_3_thresholdA_70_D40_utils_avg$y[1]
agents_3_thresholdA_70_D40_utils_avg_2$y[2] <- agents_3_thresholdA_70_D40_utils_avg$y[2]
agents_3_thresholdA_70_D40_utils_avg_2$y[3] <- agents_3_thresholdA_70_D40_utils_avg$y[3]

agents_3_threshold_A70_avg <- data.frame(rbind(colMeans(agents_3_thresholdA_70_D20_utils_avg_2), colMeans(agents_3_thresholdA_70_D30_utils_avg_2), colMeans(agents_3_thresholdA_70_D40_utils_avg_2)))
agents_3_threshold_A70_avg$x <- seq.int(nrow(agents_3_threshold_A70_avg))

#APPROVE 80
agents_3_thresholdA_80 <- agents_3[agents_3[,grep(80,threshold_approve)],]
agents_3_thresholdA_80_D20 <- agents_3_thresholdA_80[agents_3_thresholdA_80[,grep(20,threshold_disapprove)],]
agents_3_thresholdA_80_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_80_D20$util_agent_splitted))), nrow=length(agents_3_thresholdA_80_D20$util_agent_splitted), byrow=T))
agents_3_thresholdA_80_D20_utils_avg <- colMeans(agents_3_thresholdA_80_D20_utils)
agents_3_thresholdA_80_D20_utils_avg <- data.frame(y = agents_3_thresholdA_80_D20_utils_avg)
agents_3_thresholdA_80_D20_utils_avg$x <- seq.int(nrow(agents_3_thresholdA_80_D20_utils_avg))
agents_3_thresholdA_80_D20_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_80_D20_utils_avg_2$y[1] <- agents_3_thresholdA_80_D20_utils_avg$y[1]
agents_3_thresholdA_80_D20_utils_avg_2$y[2] <- agents_3_thresholdA_80_D20_utils_avg$y[2]
agents_3_thresholdA_80_D20_utils_avg_2$y[3] <- agents_3_thresholdA_80_D20_utils_avg$y[3]

agents_3_thresholdA_80_D30 <- agents_3_thresholdA_80[agents_3_thresholdA_80[,grep(30,threshold_disapprove)],]
agents_3_thresholdA_80_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_80_D30$util_agent_splitted))), nrow=length(agents_3_thresholdA_80_D30$util_agent_splitted), byrow=T))
agents_3_thresholdA_80_D30_utils_avg <- colMeans(agents_3_thresholdA_80_D30_utils)
agents_3_thresholdA_80_D30_utils_avg <- data.frame(y = agents_3_thresholdA_80_D30_utils_avg)
agents_3_thresholdA_80_D30_utils_avg$x <- seq.int(nrow(agents_3_thresholdA_80_D30_utils_avg))
agents_3_thresholdA_80_D30_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_80_D30_utils_avg_2$y[1] <- agents_3_thresholdA_80_D30_utils_avg$y[1]
agents_3_thresholdA_80_D30_utils_avg_2$y[2] <- agents_3_thresholdA_80_D30_utils_avg$y[2]
agents_3_thresholdA_80_D30_utils_avg_2$y[3] <- agents_3_thresholdA_80_D30_utils_avg$y[3]

agents_3_thresholdA_80_D40 <- agents_3_thresholdA_80[agents_3_thresholdA_80[,grep(40,threshold_disapprove)],]
agents_3_thresholdA_80_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_80_D40$util_agent_splitted))), nrow=length(agents_3_thresholdA_80_D40$util_agent_splitted), byrow=T))
agents_3_thresholdA_80_D40_utils_avg <- colMeans(agents_3_thresholdA_80_D40_utils)
agents_3_thresholdA_80_D40_utils_avg <- data.frame(y = agents_3_thresholdA_80_D40_utils_avg)
agents_3_thresholdA_80_D40_utils_avg$x <- seq.int(nrow(agents_3_thresholdA_80_D40_utils_avg))
agents_3_thresholdA_80_D40_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_80_D40_utils_avg_2$y[1] <- agents_3_thresholdA_80_D40_utils_avg$y[1]
agents_3_thresholdA_80_D40_utils_avg_2$y[2] <- agents_3_thresholdA_80_D40_utils_avg$y[2]
agents_3_thresholdA_80_D40_utils_avg_2$y[3] <- agents_3_thresholdA_80_D40_utils_avg$y[3]

agents_3_threshold_A80_avg <- data.frame(rbind(colMeans(agents_3_thresholdA_80_D20_utils_avg_2), colMeans(agents_3_thresholdA_80_D30_utils_avg_2), colMeans(agents_3_thresholdA_80_D40_utils_avg_2)))
agents_3_threshold_A80_avg$x <- seq.int(nrow(agents_3_threshold_A80_avg))

agents_3_thresholdD_20 <- agents_3[agents_3[,grep(20,threshold_disapprove)],]
agents_3_thresholdD_30 <- agents_3[agents_3[,grep(30,threshold_disapprove)],]
agents_3_thresholdD_40 <- agents_3[agents_3[,grep(40,threshold_disapprove)],]


ggplot(data=agents_3_all_utils, aes(x=x, y=y, group=row, scale_size(guide = "none"))) +
  geom_line(linetype="solid", color="black", alpha=0.04) +
  xlim(1, 3) +
  ylim(0.5, 0.9) + labs(x = "position in the game", y="utily") +
  #geom_line(data=agents_3_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) +
  geom_line(data=agents_3_threshold_A60_avg,aes(x=x,y=y),color="#0077ff",size=2,alpha=0.8) +
  geom_line(data=agents_3_threshold_A70_avg,aes(x=x,y=y),color="#0053b3",size=2,alpha=0.8) +
  geom_line(data=agents_3_threshold_A80_avg,aes(x=x,y=y),color="#002880",size=2,alpha=0.8) +
  #geom_line(data=agents_3_thresholdA_60_D20_utils_avg_2,aes(x=x,y=y),color="#0077ff",size=1,alpha=0.8) +
  #geom_line(data=agents_3_thresholdA_60_D30_utils_avg_2,aes(x=x,y=y),color="#0053b3",size=1,alpha=0.8) +
  #geom_line(data=agents_3_thresholdA_60_D40_utils_avg_2,aes(x=x,y=y),color="#3b96ff",size=1,alpha=0.8) +
  #geom_line(data=agents_3_thresholdA_70_D20_utils_avg_2,aes(x=x,y=y),color="#00ff00",size=1,alpha=0.8) +
  #geom_line(data=agents_3_thresholdA_70_D30_utils_avg_2,aes(x=x,y=y),color="#00a800",size=1,alpha=0.8) +
  #geom_line(data=agents_3_thresholdA_70_D40_utils_avg_2,aes(x=x,y=y),color="#80ff80",size=1,alpha=0.8) +
  #geom_line(data=agents_3_thresholdA_80_D20_utils_avg_2,aes(x=x,y=y),color="#ff0000",size=1,alpha=0.8) +
  #geom_line(data=agents_3_thresholdA_80_D30_utils_avg_2,aes(x=x,y=y),color="#8c0000",size=1,alpha=0.8) +
  #geom_line(data=agents_3_thresholdA_80_D40_utils_avg_2,aes(x=x,y=y),color="#ff6969",size=1,alpha=0.8) +
  theme(legend.position="right")


###########################################################################
###########################################################################
###                                                                     ###
###                                 AGENTS 4                            ###
###                                                                     ###
###########################################################################
###########################################################################
agents_4$util_agent <- str_remove(agents_4$util_agent, "\\[")
agents_4$util_agent <- str_remove(agents_4$util_agent, "\\]")
agents_4$util_agent_splitted <- strsplit(agents_4$util_agent, ",")


###########################################################################
###########################################################################
###                                                                     ###
###                                 AGENTS 5                            ###
###                                                                     ###
###########################################################################
###########################################################################
agents_5$util_agent <- str_remove(agents_5$util_agent, "\\[")
agents_5$util_agent <- str_remove(agents_5$util_agent, "\\]")
agents_5$util_agent_splitted <- strsplit(agents_5$util_agent, ",")


###########################################################################
###########################################################################
###                                                                     ###
###                                 AGENTS 6                            ###
###                                                                     ###
###########################################################################
###########################################################################
agents_6$util_agent <- str_remove(agents_6$util_agent, "\\[")
agents_6$util_agent <- str_remove(agents_6$util_agent, "\\]")
agents_6$util_agent_splitted <- strsplit(agents_6$util_agent, ",")
agents_6_utils <- data.frame(matrix(unlist(agents_6$util_agent_splitted), nrow=length(agents_6$util_agent_splitted), byrow=T))
agents_6_utils_1 <- data.frame(y=as.numeric(paste(agents_6_utils$X1)))
agents_6_utils_2 <- data.frame(y=as.numeric(paste(agents_6_utils$X2)))
agents_6_utils_3 <- data.frame(y=as.numeric(paste(agents_6_utils$X3)))
agents_6_utils_4 <- data.frame(y=as.numeric(paste(agents_6_utils$X4)))
agents_6_utils_5 <- data.frame(y=as.numeric(paste(agents_6_utils$X5)))
agents_6_utils_6 <- data.frame(y=as.numeric(paste(agents_6_utils$X6)))
agents_6_utils_1$row <- seq.int(nrow(agents_6_utils_1))
agents_6_utils_2$row <- seq.int(nrow(agents_6_utils_2))
agents_6_utils_3$row <- seq.int(nrow(agents_6_utils_3))
agents_6_utils_4$row <- seq.int(nrow(agents_6_utils_4))
agents_6_utils_5$row <- seq.int(nrow(agents_6_utils_5))
agents_6_utils_6$row <- seq.int(nrow(agents_6_utils_6))
agents_6_utils_1$x <- 1
agents_6_utils_2$x <- 2
agents_6_utils_3$x <- 3
agents_6_utils_4$x <- 4
agents_6_utils_5$x <- 5
agents_6_utils_6$x <- 6

agents_6_all_utils <- rbind(agents_6_utils_1, agents_6_utils_2, agents_6_utils_3, agents_6_utils_4, agents_6_utils_5, agents_6_utils_6)

agents_6_utils_1_avg <- colMeans(agents_6_utils_1)
agents_6_utils_2_avg <- colMeans(agents_6_utils_2)
agents_6_utils_3_avg <- colMeans(agents_6_utils_3)
agents_6_utils_4_avg <- colMeans(agents_6_utils_4)
agents_6_utils_5_avg <- colMeans(agents_6_utils_5)
agents_6_utils_6_avg <- colMeans(agents_6_utils_6)

agents_6_all_utils_avg <- data.frame(rbind(agents_6_utils_1_avg, agents_6_utils_2_avg, agents_6_utils_3_avg, agents_6_utils_4_avg, agents_6_utils_5_avg, agents_6_utils_6_avg))
agents_6_all_utils_avg

#APPROVE 60
agents_6_thresholdA_60 <- agents_6[agents_6[,grep(60,threshold_approve)],]
agents_6_thresholdA_60_D20 <- agents_6_thresholdA_60[agents_6_thresholdA_60[,grep(20,threshold_disapprove)],]
agents_6_thresholdA_60_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdA_60_D20$util_agent_splitted))), nrow=length(agents_6_thresholdA_60_D20$util_agent_splitted), byrow=T))
agents_6_thresholdA_60_D20_utils_avg <- colMeans(agents_6_thresholdA_60_D20_utils)
agents_6_thresholdA_60_D20_utils_avg <- data.frame(y = agents_6_thresholdA_60_D20_utils_avg)
agents_6_thresholdA_60_D20_utils_avg$x <- seq.int(nrow(agents_6_thresholdA_60_D20_utils_avg))
agents_6_thresholdA_60_D20_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_60_D20_utils_avg_2$y[1] <- agents_6_thresholdA_60_D20_utils_avg$y[1]
agents_6_thresholdA_60_D20_utils_avg_2$y[2] <- agents_6_thresholdA_60_D20_utils_avg$y[2]
agents_6_thresholdA_60_D20_utils_avg_2$y[3] <- agents_6_thresholdA_60_D20_utils_avg$y[3]
agents_6_thresholdA_60_D20_utils_avg_2$y[4] <- agents_6_thresholdA_60_D20_utils_avg$y[4]
agents_6_thresholdA_60_D20_utils_avg_2$y[5] <- agents_6_thresholdA_60_D20_utils_avg$y[5]
agents_6_thresholdA_60_D20_utils_avg_2$y[6] <- agents_6_thresholdA_60_D20_utils_avg$y[6]

agents_6_thresholdA_60_D30 <- agents_6_thresholdA_60[agents_6_thresholdA_60[,grep(30,threshold_disapprove)],]
agents_6_thresholdA_60_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdA_60_D30$util_agent_splitted))), nrow=length(agents_6_thresholdA_60_D30$util_agent_splitted), byrow=T))
agents_6_thresholdA_60_D30_utils_avg <- colMeans(agents_6_thresholdA_60_D30_utils)
agents_6_thresholdA_60_D30_utils_avg <- data.frame(y = agents_6_thresholdA_60_D30_utils_avg)
agents_6_thresholdA_60_D30_utils_avg$x <- seq.int(nrow(agents_6_thresholdA_60_D30_utils_avg))
agents_6_thresholdA_60_D30_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_60_D30_utils_avg_2$y[1] <- agents_6_thresholdA_60_D30_utils_avg$y[1]
agents_6_thresholdA_60_D30_utils_avg_2$y[2] <- agents_6_thresholdA_60_D30_utils_avg$y[2]
agents_6_thresholdA_60_D30_utils_avg_2$y[3] <- agents_6_thresholdA_60_D30_utils_avg$y[3]
agents_6_thresholdA_60_D30_utils_avg_2$y[4] <- agents_6_thresholdA_60_D30_utils_avg$y[4]
agents_6_thresholdA_60_D30_utils_avg_2$y[5] <- agents_6_thresholdA_60_D30_utils_avg$y[5]
agents_6_thresholdA_60_D30_utils_avg_2$y[6] <- agents_6_thresholdA_60_D30_utils_avg$y[6]


agents_6_thresholdA_60_D40 <- agents_6_thresholdA_60[agents_6_thresholdA_60[,grep(40,threshold_disapprove)],]
agents_6_thresholdA_60_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdA_60_D40$util_agent_splitted))), nrow=length(agents_6_thresholdA_60_D40$util_agent_splitted), byrow=T))
agents_6_thresholdA_60_D40_utils_avg <- colMeans(agents_6_thresholdA_60_D40_utils)
agents_6_thresholdA_60_D40_utils_avg <- data.frame(y = agents_6_thresholdA_60_D40_utils_avg)
agents_6_thresholdA_60_D40_utils_avg$x <- seq.int(nrow(agents_6_thresholdA_60_D40_utils_avg))
agents_6_thresholdA_60_D40_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_60_D40_utils_avg_2$y[1] <- agents_6_thresholdA_60_D40_utils_avg$y[1]
agents_6_thresholdA_60_D40_utils_avg_2$y[2] <- agents_6_thresholdA_60_D40_utils_avg$y[2]
agents_6_thresholdA_60_D40_utils_avg_2$y[3] <- agents_6_thresholdA_60_D40_utils_avg$y[3]
agents_6_thresholdA_60_D40_utils_avg_2$y[4] <- agents_6_thresholdA_60_D40_utils_avg$y[4]
agents_6_thresholdA_60_D40_utils_avg_2$y[5] <- agents_6_thresholdA_60_D40_utils_avg$y[5]
agents_6_thresholdA_60_D40_utils_avg_2$y[6] <- agents_6_thresholdA_60_D40_utils_avg$y[6]

agents_6_threshold_A60_avg <- data.frame(rbind(agents_6_thresholdA_60_D20_utils_avg_2, agents_6_thresholdA_60_D30_utils_avg_2,agents_6_thresholdA_60_D40_utils_avg_2))
agents_6_threshold_A60_avg$x <- seq.int(nrow(agents_6_threshold_A60_avg))

#APPROVE 70
agents_6_thresholdA_70 <- agents_6[agents_6[,grep(70,threshold_approve)],]
agents_6_thresholdA_70_D20 <- agents_6_thresholdA_70[agents_6_thresholdA_70[,grep(20,threshold_disapprove)],]
agents_6_thresholdA_70_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdA_70_D20$util_agent_splitted))), nrow=length(agents_6_thresholdA_70_D20$util_agent_splitted), byrow=T))
agents_6_thresholdA_70_D20_utils_avg <- colMeans(agents_6_thresholdA_70_D20_utils)
agents_6_thresholdA_70_D20_utils_avg <- data.frame(y = agents_6_thresholdA_70_D20_utils_avg)
agents_6_thresholdA_70_D20_utils_avg$x <- seq.int(nrow(agents_6_thresholdA_70_D20_utils_avg))
agents_6_thresholdA_70_D20_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_70_D20_utils_avg_2$y[1] <- agents_6_thresholdA_70_D20_utils_avg$y[1]
agents_6_thresholdA_70_D20_utils_avg_2$y[2] <- agents_6_thresholdA_70_D20_utils_avg$y[2]
agents_6_thresholdA_70_D20_utils_avg_2$y[3] <- agents_6_thresholdA_70_D20_utils_avg$y[3]
agents_6_thresholdA_70_D20_utils_avg_2$y[4] <- agents_6_thresholdA_70_D20_utils_avg$y[4]
agents_6_thresholdA_70_D20_utils_avg_2$y[5] <- agents_6_thresholdA_70_D20_utils_avg$y[5]
agents_6_thresholdA_70_D20_utils_avg_2$y[6] <- agents_6_thresholdA_70_D20_utils_avg$y[6]

agents_6_thresholdA_70_D30 <- agents_6_thresholdA_70[agents_6_thresholdA_70[,grep(30,threshold_disapprove)],]
agents_6_thresholdA_70_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdA_70_D30$util_agent_splitted))), nrow=length(agents_6_thresholdA_70_D30$util_agent_splitted), byrow=T))
agents_6_thresholdA_70_D30_utils_avg <- colMeans(agents_6_thresholdA_70_D30_utils)
agents_6_thresholdA_70_D30_utils_avg <- data.frame(y = agents_6_thresholdA_70_D30_utils_avg)
agents_6_thresholdA_70_D30_utils_avg$x <- seq.int(nrow(agents_6_thresholdA_70_D30_utils_avg))
agents_6_thresholdA_70_D30_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_70_D30_utils_avg_2$y[1] <- agents_6_thresholdA_70_D30_utils_avg$y[1]
agents_6_thresholdA_70_D30_utils_avg_2$y[2] <- agents_6_thresholdA_70_D30_utils_avg$y[2]
agents_6_thresholdA_70_D30_utils_avg_2$y[3] <- agents_6_thresholdA_70_D30_utils_avg$y[3]
agents_6_thresholdA_70_D30_utils_avg_2$y[4] <- agents_6_thresholdA_70_D30_utils_avg$y[4]
agents_6_thresholdA_70_D30_utils_avg_2$y[5] <- agents_6_thresholdA_70_D30_utils_avg$y[5]
agents_6_thresholdA_70_D30_utils_avg_2$y[6] <- agents_6_thresholdA_70_D30_utils_avg$y[6]


agents_6_thresholdA_70_D40 <- agents_6_thresholdA_70[agents_6_thresholdA_70[,grep(40,threshold_disapprove)],]
agents_6_thresholdA_70_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdA_70_D40$util_agent_splitted))), nrow=length(agents_6_thresholdA_70_D40$util_agent_splitted), byrow=T))
agents_6_thresholdA_70_D40_utils_avg <- colMeans(agents_6_thresholdA_70_D40_utils)
agents_6_thresholdA_70_D40_utils_avg <- data.frame(y = agents_6_thresholdA_70_D40_utils_avg)
agents_6_thresholdA_70_D40_utils_avg$x <- seq.int(nrow(agents_6_thresholdA_70_D40_utils_avg))
agents_6_thresholdA_70_D40_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_70_D40_utils_avg_2$y[1] <- agents_6_thresholdA_70_D40_utils_avg$y[1]
agents_6_thresholdA_70_D40_utils_avg_2$y[2] <- agents_6_thresholdA_70_D40_utils_avg$y[2]
agents_6_thresholdA_70_D40_utils_avg_2$y[3] <- agents_6_thresholdA_70_D40_utils_avg$y[3]
agents_6_thresholdA_70_D40_utils_avg_2$y[4] <- agents_6_thresholdA_70_D40_utils_avg$y[4]
agents_6_thresholdA_70_D40_utils_avg_2$y[5] <- agents_6_thresholdA_70_D40_utils_avg$y[5]
agents_6_thresholdA_70_D40_utils_avg_2$y[6] <- agents_6_thresholdA_70_D40_utils_avg$y[6]

agents_6_threshold_A70_avg <- data.frame(rbind(agents_6_thresholdA_70_D20_utils_avg_2, agents_6_thresholdA_70_D30_utils_avg_2,agents_6_thresholdA_70_D40_utils_avg_2))
agents_6_threshold_A70_avg$x <- seq.int(nrow(agents_6_threshold_A70_avg))

#APPROVE 80
agents_6_thresholdA_80 <- agents_6[agents_6[,grep(80,threshold_approve)],]
agents_6_thresholdA_80_D20 <- agents_6_thresholdA_80[agents_6_thresholdA_80[,grep(20,threshold_disapprove)],]
agents_6_thresholdA_80_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdA_80_D20$util_agent_splitted))), nrow=length(agents_6_thresholdA_80_D20$util_agent_splitted), byrow=T))
agents_6_thresholdA_80_D20_utils_avg <- colMeans(agents_6_thresholdA_80_D20_utils)
agents_6_thresholdA_80_D20_utils_avg <- data.frame(y = agents_6_thresholdA_80_D20_utils_avg)
agents_6_thresholdA_80_D20_utils_avg$x <- seq.int(nrow(agents_6_thresholdA_80_D20_utils_avg))
agents_6_thresholdA_80_D20_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_80_D20_utils_avg_2$y[1] <- agents_6_thresholdA_80_D20_utils_avg$y[1]
agents_6_thresholdA_80_D20_utils_avg_2$y[2] <- agents_6_thresholdA_80_D20_utils_avg$y[2]
agents_6_thresholdA_80_D20_utils_avg_2$y[3] <- agents_6_thresholdA_80_D20_utils_avg$y[3]
agents_6_thresholdA_80_D20_utils_avg_2$y[4] <- agents_6_thresholdA_80_D20_utils_avg$y[4]
agents_6_thresholdA_80_D20_utils_avg_2$y[5] <- agents_6_thresholdA_80_D20_utils_avg$y[5]
agents_6_thresholdA_80_D20_utils_avg_2$y[6] <- agents_6_thresholdA_80_D20_utils_avg$y[6]

agents_6_thresholdA_80_D30 <- agents_6_thresholdA_80[agents_6_thresholdA_80[,grep(30,threshold_disapprove)],]
agents_6_thresholdA_80_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdA_80_D30$util_agent_splitted))), nrow=length(agents_6_thresholdA_80_D30$util_agent_splitted), byrow=T))
agents_6_thresholdA_80_D30_utils_avg <- colMeans(agents_6_thresholdA_80_D30_utils)
agents_6_thresholdA_80_D30_utils_avg <- data.frame(y = agents_6_thresholdA_80_D30_utils_avg)
agents_6_thresholdA_80_D30_utils_avg$x <- seq.int(nrow(agents_6_thresholdA_80_D30_utils_avg))
agents_6_thresholdA_80_D30_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_80_D30_utils_avg_2$y[1] <- agents_6_thresholdA_80_D30_utils_avg$y[1]
agents_6_thresholdA_80_D30_utils_avg_2$y[2] <- agents_6_thresholdA_80_D30_utils_avg$y[2]
agents_6_thresholdA_80_D30_utils_avg_2$y[3] <- agents_6_thresholdA_80_D30_utils_avg$y[3]
agents_6_thresholdA_80_D30_utils_avg_2$y[4] <- agents_6_thresholdA_80_D30_utils_avg$y[4]
agents_6_thresholdA_80_D30_utils_avg_2$y[5] <- agents_6_thresholdA_80_D30_utils_avg$y[5]
agents_6_thresholdA_80_D30_utils_avg_2$y[6] <- agents_6_thresholdA_80_D30_utils_avg$y[6]


agents_6_thresholdA_80_D40 <- agents_6_thresholdA_80[agents_6_thresholdA_80[,grep(40,threshold_disapprove)],]
agents_6_thresholdA_80_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdA_80_D40$util_agent_splitted))), nrow=length(agents_6_thresholdA_80_D40$util_agent_splitted), byrow=T))
agents_6_thresholdA_80_D40_utils_avg <- colMeans(agents_6_thresholdA_80_D40_utils)
agents_6_thresholdA_80_D40_utils_avg <- data.frame(y = agents_6_thresholdA_80_D40_utils_avg)
agents_6_thresholdA_80_D40_utils_avg$x <- seq.int(nrow(agents_6_thresholdA_80_D40_utils_avg))
agents_6_thresholdA_80_D40_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_80_D40_utils_avg_2$y[1] <- agents_6_thresholdA_80_D40_utils_avg$y[1]
agents_6_thresholdA_80_D40_utils_avg_2$y[2] <- agents_6_thresholdA_80_D40_utils_avg$y[2]
agents_6_thresholdA_80_D40_utils_avg_2$y[3] <- agents_6_thresholdA_80_D40_utils_avg$y[3]
agents_6_thresholdA_80_D40_utils_avg_2$y[4] <- agents_6_thresholdA_80_D40_utils_avg$y[4]
agents_6_thresholdA_80_D40_utils_avg_2$y[5] <- agents_6_thresholdA_80_D40_utils_avg$y[5]
agents_6_thresholdA_80_D40_utils_avg_2$y[6] <- agents_6_thresholdA_80_D40_utils_avg$y[6]

agents_6_threshold_A80_avg <- data.frame(rbind(agents_6_thresholdA_80_D20_utils_avg_2, agents_6_thresholdA_80_D30_utils_avg_2,agents_6_thresholdA_80_D40_utils_avg_2))
agents_6_threshold_A80_avg$x <- seq.int(nrow(agents_6_threshold_A80_avg))

agents_6_thresholdD_20 <- agents_6[agents_6[,grep(20,threshold_disapprove)],]
agents_6_thresholdD_30 <- agents_6[agents_6[,grep(30,threshold_disapprove)],]
agents_6_thresholdD_40 <- agents_6[agents_6[,grep(40,threshold_disapprove)],]


ggplot(data=agents_6_all_utils, aes(x=x, y=y, group=row, scale_size(guide = "none"))) +
  geom_line(linetype="solid", color="black", alpha=0.04) +
  xlim(1, 6) +
  ylim(0.5, 0.9) + labs(x = "position in the game", y="utily") +
  #geom_line(data=agents_6_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) +
  geom_line(data=agents_6_threshold_A60_avg,aes(x=x,y=y),color="#0077ff",size=2,alpha=0.8) +
  geom_line(data=agents_6_threshold_A70_avg,aes(x=x,y=y),color="#0053b3",size=2,alpha=0.8) +
  geom_line(data=agents_6_threshold_A80_avg,aes(x=x,y=y),color="#002880",size=2,alpha=0.8) +
  #geom_line(data=agents_6_thresholdA_60_D20_utils_avg_2,aes(x=x,y=y),color="#0077ff",size=1,alpha=0.8) +
  #geom_line(data=agents_6_thresholdA_60_D30_utils_avg_2,aes(x=x,y=y),color="#0053b3",size=1,alpha=0.8) +
  #geom_line(data=agents_6_thresholdA_60_D40_utils_avg_2,aes(x=x,y=y),color="#3b96ff",size=1,alpha=0.8) +
  #geom_line(data=agents_6_thresholdA_70_D20_utils_avg_2,aes(x=x,y=y),color="#00ff00",size=1,alpha=0.8) +
  #geom_line(data=agents_6_thresholdA_70_D30_utils_avg_2,aes(x=x,y=y),color="#00a800",size=1,alpha=0.8) +
  #geom_line(data=agents_6_thresholdA_70_D40_utils_avg_2,aes(x=x,y=y),color="#80ff80",size=1,alpha=0.8) +
  #geom_line(data=agents_6_thresholdA_80_D20_utils_avg_2,aes(x=x,y=y),color="#ff0000",size=1,alpha=0.8) +
  #geom_line(data=agents_6_thresholdA_80_D30_utils_avg_2,aes(x=x,y=y),color="#8c0000",size=1,alpha=0.8) +
  #geom_line(data=agents_6_thresholdA_80_D40_utils_avg_2,aes(x=x,y=y),color="#ff6969",size=1,alpha=0.8) +
  theme(legend.position="right")


###########################################################################
###########################################################################
###                                                                     ###
###                                 AGENTS 7                            ###
###                                                                     ###
###########################################################################
###########################################################################
agents_7$util_agent <- str_remove(agents_7$util_agent, "\\[")
agents_7$util_agent <- str_remove(agents_7$util_agent, "\\]")
agents_7$util_agent_splitted <- strsplit(agents_7$util_agent, ",")

###########################################################################
###########################################################################
###                                                                     ###
###                                 AGENTS 8                            ###
###                                                                     ###
###########################################################################
###########################################################################
agents_8$util_agent <- str_remove(agents_8$util_agent, "\\[")
agents_8$util_agent <- str_remove(agents_8$util_agent, "\\]")
agents_8$util_agent_splitted <- strsplit(agents_8$util_agent, ",")






###########################################################################
###########################################################################
###                                                                     ###
###                                 AGENTS 9                            ###
###                                                                     ###
###########################################################################
###########################################################################
agents_9$util_agent <- str_remove(agents_9$util_agent, "\\[")
agents_9$util_agent <- str_remove(agents_9$util_agent, "\\]")
agents_9$util_agent_splitted <- strsplit(agents_9$util_agent, ",")

agents_9_utils <- data.frame(matrix(unlist(agents_9$util_agent_splitted), nrow=length(agents_9$util_agent_splitted), byrow=T))
agents_9_utils_1 <- data.frame(y=as.numeric(paste(agents_9_utils$X1)))
agents_9_utils_2 <- data.frame(y=as.numeric(paste(agents_9_utils$X2)))
agents_9_utils_3 <- data.frame(y=as.numeric(paste(agents_9_utils$X3)))
agents_9_utils_4 <- data.frame(y=as.numeric(paste(agents_9_utils$X4)))
agents_9_utils_5 <- data.frame(y=as.numeric(paste(agents_9_utils$X5)))
agents_9_utils_6 <- data.frame(y=as.numeric(paste(agents_9_utils$X6)))
agents_9_utils_7 <- data.frame(y=as.numeric(paste(agents_9_utils$X7)))
agents_9_utils_8 <- data.frame(y=as.numeric(paste(agents_9_utils$X8)))
agents_9_utils_9 <- data.frame(y=as.numeric(paste(agents_9_utils$X9)))
agents_9_utils_1$row <- seq.int(nrow(agents_9_utils_1))
agents_9_utils_2$row <- seq.int(nrow(agents_9_utils_2))
agents_9_utils_3$row <- seq.int(nrow(agents_9_utils_3))
agents_9_utils_4$row <- seq.int(nrow(agents_9_utils_4))
agents_9_utils_5$row <- seq.int(nrow(agents_9_utils_5))
agents_9_utils_6$row <- seq.int(nrow(agents_9_utils_6))
agents_9_utils_7$row <- seq.int(nrow(agents_9_utils_7))
agents_9_utils_8$row <- seq.int(nrow(agents_9_utils_8))
agents_9_utils_9$row <- seq.int(nrow(agents_9_utils_9))
agents_9_utils_1$x <- 1
agents_9_utils_2$x <- 2
agents_9_utils_3$x <- 3
agents_9_utils_4$x <- 4
agents_9_utils_5$x <- 5
agents_9_utils_6$x <- 6
agents_9_utils_7$x <- 7
agents_9_utils_8$x <- 8
agents_9_utils_9$x <- 9

agents_9_all_utils <- rbind(agents_9_utils_1, agents_9_utils_2, agents_9_utils_3, agents_9_utils_4, agents_9_utils_5, agents_9_utils_6, agents_9_utils_7, agents_9_utils_8, agents_9_utils_9)

agents_9_utils_1_avg <- colMeans(agents_9_utils_1)
agents_9_utils_2_avg <- colMeans(agents_9_utils_2)
agents_9_utils_3_avg <- colMeans(agents_9_utils_3)
agents_9_utils_4_avg <- colMeans(agents_9_utils_4)
agents_9_utils_5_avg <- colMeans(agents_9_utils_5)
agents_9_utils_6_avg <- colMeans(agents_9_utils_6)
agents_9_utils_7_avg <- colMeans(agents_9_utils_7)
agents_9_utils_8_avg <- colMeans(agents_9_utils_8)
agents_9_utils_9_avg <- colMeans(agents_9_utils_9)

agents_9_all_utils_avg <- data.frame(rbind(agents_9_utils_1_avg, agents_9_utils_2_avg, agents_9_utils_3_avg, agents_9_utils_4_avg, agents_9_utils_5_avg, agents_9_utils_6_avg, agents_9_utils_7_avg, agents_9_utils_8_avg, agents_9_utils_9_avg))
agents_9_all_utils_avg

#APPROVE 60
agents_9_thresholdA_60 <- agents_9[agents_9[,grep(60,threshold_approve)],]
agents_9_thresholdA_60_D20 <- agents_9_thresholdA_60[agents_9_thresholdA_60[,grep(20,threshold_disapprove)],]
agents_9_thresholdA_60_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdA_60_D20$util_agent_splitted))), nrow=length(agents_9_thresholdA_60_D20$util_agent_splitted), byrow=T))
agents_9_thresholdA_60_D20_utils_avg <- colMeans(agents_9_thresholdA_60_D20_utils)
agents_9_thresholdA_60_D20_utils_avg <- data.frame(y = agents_9_thresholdA_60_D20_utils_avg)
agents_9_thresholdA_60_D20_utils_avg$x <- seq.int(9)
agents_9_thresholdA_60_D20_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdA_60_D20_utils_avg_2$y[1] <- agents_9_thresholdA_60_D20_utils_avg$y[1]
agents_9_thresholdA_60_D20_utils_avg_2$y[2] <- agents_9_thresholdA_60_D20_utils_avg$y[2]
agents_9_thresholdA_60_D20_utils_avg_2$y[3] <- agents_9_thresholdA_60_D20_utils_avg$y[3]
agents_9_thresholdA_60_D20_utils_avg_2$y[4] <- agents_9_thresholdA_60_D20_utils_avg$y[4]
agents_9_thresholdA_60_D20_utils_avg_2$y[5] <- agents_9_thresholdA_60_D20_utils_avg$y[5]
agents_9_thresholdA_60_D20_utils_avg_2$y[6] <- agents_9_thresholdA_60_D20_utils_avg$y[6]
agents_9_thresholdA_60_D20_utils_avg_2$y[7] <- agents_9_thresholdA_60_D20_utils_avg$y[7]
agents_9_thresholdA_60_D20_utils_avg_2$y[8] <- agents_9_thresholdA_60_D20_utils_avg$y[8]
agents_9_thresholdA_60_D20_utils_avg_2$y[9] <- agents_9_thresholdA_60_D20_utils_avg$y[9]

agents_9_thresholdA_60_D30 <- agents_9_thresholdA_60[agents_9_thresholdA_60[,grep(30,threshold_disapprove)],]
agents_9_thresholdA_60_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdA_60_D30$util_agent_splitted))), nrow=length(agents_9_thresholdA_60_D30$util_agent_splitted), byrow=T))
agents_9_thresholdA_60_D30_utils_avg <- colMeans(agents_9_thresholdA_60_D30_utils)
agents_9_thresholdA_60_D30_utils_avg <- data.frame(y = agents_9_thresholdA_60_D30_utils_avg)
agents_9_thresholdA_60_D30_utils_avg$x <- seq.int(9)
agents_9_thresholdA_60_D30_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdA_60_D30_utils_avg_2$y[1] <- agents_9_thresholdA_60_D30_utils_avg$y[1]
agents_9_thresholdA_60_D30_utils_avg_2$y[2] <- agents_9_thresholdA_60_D30_utils_avg$y[2]
agents_9_thresholdA_60_D30_utils_avg_2$y[3] <- agents_9_thresholdA_60_D30_utils_avg$y[3]
agents_9_thresholdA_60_D30_utils_avg_2$y[4] <- agents_9_thresholdA_60_D30_utils_avg$y[4]
agents_9_thresholdA_60_D30_utils_avg_2$y[5] <- agents_9_thresholdA_60_D30_utils_avg$y[5]
agents_9_thresholdA_60_D30_utils_avg_2$y[6] <- agents_9_thresholdA_60_D30_utils_avg$y[6]
agents_9_thresholdA_60_D30_utils_avg_2$y[7] <- agents_9_thresholdA_60_D30_utils_avg$y[7]
agents_9_thresholdA_60_D30_utils_avg_2$y[8] <- agents_9_thresholdA_60_D30_utils_avg$y[8]
agents_9_thresholdA_60_D30_utils_avg_2$y[9] <- agents_9_thresholdA_60_D30_utils_avg$y[9]

agents_9_thresholdA_60_D40 <- agents_9_thresholdA_60[agents_9_thresholdA_60[,grep(40,threshold_disapprove)],]
agents_9_thresholdA_60_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdA_60_D40$util_agent_splitted))), nrow=length(agents_9_thresholdA_60_D40$util_agent_splitted), byrow=T))
agents_9_thresholdA_60_D40_utils_avg <- colMeans(agents_9_thresholdA_60_D40_utils)
agents_9_thresholdA_60_D40_utils_avg <- data.frame(y = agents_9_thresholdA_60_D40_utils_avg)
agents_9_thresholdA_60_D40_utils_avg$x <- seq.int(9)
agents_9_thresholdA_60_D40_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdA_60_D40_utils_avg_2$y[1] <- agents_9_thresholdA_60_D40_utils_avg$y[1]
agents_9_thresholdA_60_D40_utils_avg_2$y[2] <- agents_9_thresholdA_60_D40_utils_avg$y[2]
agents_9_thresholdA_60_D40_utils_avg_2$y[3] <- agents_9_thresholdA_60_D40_utils_avg$y[3]
agents_9_thresholdA_60_D40_utils_avg_2$y[4] <- agents_9_thresholdA_60_D40_utils_avg$y[4]
agents_9_thresholdA_60_D40_utils_avg_2$y[5] <- agents_9_thresholdA_60_D40_utils_avg$y[5]
agents_9_thresholdA_60_D40_utils_avg_2$y[6] <- agents_9_thresholdA_60_D40_utils_avg$y[6]
agents_9_thresholdA_60_D40_utils_avg_2$y[7] <- agents_9_thresholdA_60_D40_utils_avg$y[7]
agents_9_thresholdA_60_D40_utils_avg_2$y[8] <- agents_9_thresholdA_60_D40_utils_avg$y[8]
agents_9_thresholdA_60_D40_utils_avg_2$y[9] <- agents_9_thresholdA_60_D40_utils_avg$y[9]

agents_9_threshold_A60_avg <- data.frame(rbind(agents_9_thresholdA_60_D20_utils_avg_2, agents_9_thresholdA_60_D30_utils_avg_2,agents_9_thresholdA_60_D40_utils_avg_2))
agents_9_threshold_A60_avg_result <- agents_9_threshold_A60_avg[FALSE,]
agents_9_threshold_A60_avg_result <- data.frame(t(colMeans(subset(agents_9_threshold_A60_avg, x == 1))))
agents_9_threshold_A60_avg_result <- rbind(agents_9_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A60_avg, x == 2)))))
agents_9_threshold_A60_avg_result <- rbind(agents_9_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A60_avg, x == 3)))))
agents_9_threshold_A60_avg_result <- rbind(agents_9_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A60_avg, x == 4)))))
agents_9_threshold_A60_avg_result <- rbind(agents_9_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A60_avg, x == 5)))))
agents_9_threshold_A60_avg_result <- rbind(agents_9_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A60_avg, x == 6)))))
agents_9_threshold_A60_avg_result <- rbind(agents_9_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A60_avg, x == 7)))))
agents_9_threshold_A60_avg_result <- rbind(agents_9_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A60_avg, x == 8)))))
agents_9_threshold_A60_avg_result <- rbind(agents_9_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A60_avg, x == 9)))))
agents_9_threshold_A60_avg_result

#APPROVE 70
agents_9_thresholdA_70 <- agents_9[agents_9[,grep(70,threshold_approve)],]
agents_9_thresholdA_70_D20 <- agents_9_thresholdA_70[agents_9_thresholdA_70[,grep(20,threshold_disapprove)],]
agents_9_thresholdA_70_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdA_70_D20$util_agent_splitted))), nrow=length(agents_9_thresholdA_70_D20$util_agent_splitted), byrow=T))
agents_9_thresholdA_70_D20_utils_avg <- colMeans(agents_9_thresholdA_70_D20_utils)
agents_9_thresholdA_70_D20_utils_avg <- data.frame(y = agents_9_thresholdA_70_D20_utils_avg)
agents_9_thresholdA_70_D20_utils_avg$x <- seq.int(9)
agents_9_thresholdA_70_D20_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdA_70_D20_utils_avg_2$y[1] <- agents_9_thresholdA_70_D20_utils_avg$y[1]
agents_9_thresholdA_70_D20_utils_avg_2$y[2] <- agents_9_thresholdA_70_D20_utils_avg$y[2]
agents_9_thresholdA_70_D20_utils_avg_2$y[3] <- agents_9_thresholdA_70_D20_utils_avg$y[3]
agents_9_thresholdA_70_D20_utils_avg_2$y[4] <- agents_9_thresholdA_70_D20_utils_avg$y[4]
agents_9_thresholdA_70_D20_utils_avg_2$y[5] <- agents_9_thresholdA_70_D20_utils_avg$y[5]
agents_9_thresholdA_70_D20_utils_avg_2$y[6] <- agents_9_thresholdA_70_D20_utils_avg$y[6]
agents_9_thresholdA_70_D20_utils_avg_2$y[7] <- agents_9_thresholdA_70_D20_utils_avg$y[7]
agents_9_thresholdA_70_D20_utils_avg_2$y[8] <- agents_9_thresholdA_70_D20_utils_avg$y[8]
agents_9_thresholdA_70_D20_utils_avg_2$y[9] <- agents_9_thresholdA_70_D20_utils_avg$y[9]

agents_9_thresholdA_70_D30 <- agents_9_thresholdA_70[agents_9_thresholdA_70[,grep(30,threshold_disapprove)],]
agents_9_thresholdA_70_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdA_70_D30$util_agent_splitted))), nrow=length(agents_9_thresholdA_70_D30$util_agent_splitted), byrow=T))
agents_9_thresholdA_70_D30_utils_avg <- colMeans(agents_9_thresholdA_70_D30_utils)
agents_9_thresholdA_70_D30_utils_avg <- data.frame(y = agents_9_thresholdA_70_D30_utils_avg)
agents_9_thresholdA_70_D30_utils_avg$x <- seq.int(9)
agents_9_thresholdA_70_D30_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdA_70_D30_utils_avg_2$y[1] <- agents_9_thresholdA_70_D30_utils_avg$y[1]
agents_9_thresholdA_70_D30_utils_avg_2$y[2] <- agents_9_thresholdA_70_D30_utils_avg$y[2]
agents_9_thresholdA_70_D30_utils_avg_2$y[3] <- agents_9_thresholdA_70_D30_utils_avg$y[3]
agents_9_thresholdA_70_D30_utils_avg_2$y[4] <- agents_9_thresholdA_70_D30_utils_avg$y[4]
agents_9_thresholdA_70_D30_utils_avg_2$y[5] <- agents_9_thresholdA_70_D30_utils_avg$y[5]
agents_9_thresholdA_70_D30_utils_avg_2$y[6] <- agents_9_thresholdA_70_D30_utils_avg$y[6]
agents_9_thresholdA_70_D30_utils_avg_2$y[7] <- agents_9_thresholdA_70_D30_utils_avg$y[7]
agents_9_thresholdA_70_D30_utils_avg_2$y[8] <- agents_9_thresholdA_70_D30_utils_avg$y[8]
agents_9_thresholdA_70_D30_utils_avg_2$y[9] <- agents_9_thresholdA_70_D30_utils_avg$y[9]

agents_9_thresholdA_70_D40 <- agents_9_thresholdA_70[agents_9_thresholdA_70[,grep(40,threshold_disapprove)],]
agents_9_thresholdA_70_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdA_70_D40$util_agent_splitted))), nrow=length(agents_9_thresholdA_70_D40$util_agent_splitted), byrow=T))
agents_9_thresholdA_70_D40_utils_avg <- colMeans(agents_9_thresholdA_70_D40_utils)
agents_9_thresholdA_70_D40_utils_avg <- data.frame(y = agents_9_thresholdA_70_D40_utils_avg)
agents_9_thresholdA_70_D40_utils_avg$x <- seq.int(9)
agents_9_thresholdA_70_D40_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdA_70_D40_utils_avg_2$y[1] <- agents_9_thresholdA_70_D40_utils_avg$y[1]
agents_9_thresholdA_70_D40_utils_avg_2$y[2] <- agents_9_thresholdA_70_D40_utils_avg$y[2]
agents_9_thresholdA_70_D40_utils_avg_2$y[3] <- agents_9_thresholdA_70_D40_utils_avg$y[3]
agents_9_thresholdA_70_D40_utils_avg_2$y[4] <- agents_9_thresholdA_70_D40_utils_avg$y[4]
agents_9_thresholdA_70_D40_utils_avg_2$y[5] <- agents_9_thresholdA_70_D40_utils_avg$y[5]
agents_9_thresholdA_70_D40_utils_avg_2$y[6] <- agents_9_thresholdA_70_D40_utils_avg$y[6]
agents_9_thresholdA_70_D40_utils_avg_2$y[7] <- agents_9_thresholdA_70_D40_utils_avg$y[7]
agents_9_thresholdA_70_D40_utils_avg_2$y[8] <- agents_9_thresholdA_70_D40_utils_avg$y[8]
agents_9_thresholdA_70_D40_utils_avg_2$y[9] <- agents_9_thresholdA_70_D40_utils_avg$y[9]

agents_9_threshold_A70_avg <- data.frame(rbind(agents_9_thresholdA_70_D20_utils_avg_2, agents_9_thresholdA_70_D30_utils_avg_2,agents_9_thresholdA_70_D40_utils_avg_2))
agents_9_threshold_A70_avg_result <- agents_9_threshold_A70_avg[FALSE,]
agents_9_threshold_A70_avg_result <- data.frame(t(colMeans(subset(agents_9_threshold_A70_avg, x == 1))))
agents_9_threshold_A70_avg_result <- rbind(agents_9_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A70_avg, x == 2)))))
agents_9_threshold_A70_avg_result <- rbind(agents_9_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A70_avg, x == 3)))))
agents_9_threshold_A70_avg_result <- rbind(agents_9_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A70_avg, x == 4)))))
agents_9_threshold_A70_avg_result <- rbind(agents_9_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A70_avg, x == 5)))))
agents_9_threshold_A70_avg_result <- rbind(agents_9_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A70_avg, x == 6)))))
agents_9_threshold_A70_avg_result <- rbind(agents_9_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A70_avg, x == 7)))))
agents_9_threshold_A70_avg_result <- rbind(agents_9_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A70_avg, x == 8)))))
agents_9_threshold_A70_avg_result <- rbind(agents_9_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A70_avg, x == 9)))))
agents_9_threshold_A70_avg_result

#APPROVE 80
agents_9_thresholdA_80 <- agents_9[agents_9[,grep(80,threshold_approve)],]
agents_9_thresholdA_80_D20 <- agents_9_thresholdA_80[agents_9_thresholdA_80[,grep(20,threshold_disapprove)],]
agents_9_thresholdA_80_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdA_80_D20$util_agent_splitted))), nrow=length(agents_9_thresholdA_80_D20$util_agent_splitted), byrow=T))
agents_9_thresholdA_80_D20_utils_avg <- colMeans(agents_9_thresholdA_80_D20_utils)
agents_9_thresholdA_80_D20_utils_avg <- data.frame(y = agents_9_thresholdA_80_D20_utils_avg)
agents_9_thresholdA_80_D20_utils_avg$x <- seq.int(9)
agents_9_thresholdA_80_D20_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdA_80_D20_utils_avg_2$y[1] <- agents_9_thresholdA_80_D20_utils_avg$y[1]
agents_9_thresholdA_80_D20_utils_avg_2$y[2] <- agents_9_thresholdA_80_D20_utils_avg$y[2]
agents_9_thresholdA_80_D20_utils_avg_2$y[3] <- agents_9_thresholdA_80_D20_utils_avg$y[3]
agents_9_thresholdA_80_D20_utils_avg_2$y[4] <- agents_9_thresholdA_80_D20_utils_avg$y[4]
agents_9_thresholdA_80_D20_utils_avg_2$y[5] <- agents_9_thresholdA_80_D20_utils_avg$y[5]
agents_9_thresholdA_80_D20_utils_avg_2$y[6] <- agents_9_thresholdA_80_D20_utils_avg$y[6]
agents_9_thresholdA_80_D20_utils_avg_2$y[7] <- agents_9_thresholdA_80_D20_utils_avg$y[7]
agents_9_thresholdA_80_D20_utils_avg_2$y[8] <- agents_9_thresholdA_80_D20_utils_avg$y[8]
agents_9_thresholdA_80_D20_utils_avg_2$y[9] <- agents_9_thresholdA_80_D20_utils_avg$y[9]

agents_9_thresholdA_80_D30 <- agents_9_thresholdA_80[agents_9_thresholdA_80[,grep(30,threshold_disapprove)],]
agents_9_thresholdA_80_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdA_80_D30$util_agent_splitted))), nrow=length(agents_9_thresholdA_80_D30$util_agent_splitted), byrow=T))
agents_9_thresholdA_80_D30_utils_avg <- colMeans(agents_9_thresholdA_80_D30_utils)
agents_9_thresholdA_80_D30_utils_avg <- data.frame(y = agents_9_thresholdA_80_D30_utils_avg)
agents_9_thresholdA_80_D30_utils_avg$x <- seq.int(9)
agents_9_thresholdA_80_D30_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdA_80_D30_utils_avg_2$y[1] <- agents_9_thresholdA_80_D30_utils_avg$y[1]
agents_9_thresholdA_80_D30_utils_avg_2$y[2] <- agents_9_thresholdA_80_D30_utils_avg$y[2]
agents_9_thresholdA_80_D30_utils_avg_2$y[3] <- agents_9_thresholdA_80_D30_utils_avg$y[3]
agents_9_thresholdA_80_D30_utils_avg_2$y[4] <- agents_9_thresholdA_80_D30_utils_avg$y[4]
agents_9_thresholdA_80_D30_utils_avg_2$y[5] <- agents_9_thresholdA_80_D30_utils_avg$y[5]
agents_9_thresholdA_80_D30_utils_avg_2$y[6] <- agents_9_thresholdA_80_D30_utils_avg$y[6]
agents_9_thresholdA_80_D30_utils_avg_2$y[7] <- agents_9_thresholdA_80_D30_utils_avg$y[7]
agents_9_thresholdA_80_D30_utils_avg_2$y[8] <- agents_9_thresholdA_80_D30_utils_avg$y[8]
agents_9_thresholdA_80_D30_utils_avg_2$y[9] <- agents_9_thresholdA_80_D30_utils_avg$y[9]

agents_9_thresholdA_80_D40 <- agents_9_thresholdA_80[agents_9_thresholdA_80[,grep(40,threshold_disapprove)],]
agents_9_thresholdA_80_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdA_80_D40$util_agent_splitted))), nrow=length(agents_9_thresholdA_80_D40$util_agent_splitted), byrow=T))
agents_9_thresholdA_80_D40_utils_avg <- colMeans(agents_9_thresholdA_80_D40_utils)
agents_9_thresholdA_80_D40_utils_avg <- data.frame(y = agents_9_thresholdA_80_D40_utils_avg)
agents_9_thresholdA_80_D40_utils_avg$x <- seq.int(9)
agents_9_thresholdA_80_D40_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdA_80_D40_utils_avg_2$y[1] <- agents_9_thresholdA_80_D40_utils_avg$y[1]
agents_9_thresholdA_80_D40_utils_avg_2$y[2] <- agents_9_thresholdA_80_D40_utils_avg$y[2]
agents_9_thresholdA_80_D40_utils_avg_2$y[3] <- agents_9_thresholdA_80_D40_utils_avg$y[3]
agents_9_thresholdA_80_D40_utils_avg_2$y[4] <- agents_9_thresholdA_80_D40_utils_avg$y[4]
agents_9_thresholdA_80_D40_utils_avg_2$y[5] <- agents_9_thresholdA_80_D40_utils_avg$y[5]
agents_9_thresholdA_80_D40_utils_avg_2$y[6] <- agents_9_thresholdA_80_D40_utils_avg$y[6]
agents_9_thresholdA_80_D40_utils_avg_2$y[7] <- agents_9_thresholdA_80_D40_utils_avg$y[7]
agents_9_thresholdA_80_D40_utils_avg_2$y[8] <- agents_9_thresholdA_80_D40_utils_avg$y[8]
agents_9_thresholdA_80_D40_utils_avg_2$y[9] <- agents_9_thresholdA_80_D40_utils_avg$y[9]

agents_9_threshold_A80_avg <- data.frame(rbind(agents_9_thresholdA_80_D20_utils_avg_2, agents_9_thresholdA_80_D30_utils_avg_2,agents_9_thresholdA_80_D40_utils_avg_2))
agents_9_threshold_A80_avg_result <- agents_9_threshold_A80_avg[FALSE,]
agents_9_threshold_A80_avg_result <- data.frame(t(colMeans(subset(agents_9_threshold_A80_avg, x == 1))))
agents_9_threshold_A80_avg_result <- rbind(agents_9_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A80_avg, x == 2)))))
agents_9_threshold_A80_avg_result <- rbind(agents_9_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A80_avg, x == 3)))))
agents_9_threshold_A80_avg_result <- rbind(agents_9_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A80_avg, x == 4)))))
agents_9_threshold_A80_avg_result <- rbind(agents_9_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A80_avg, x == 5)))))
agents_9_threshold_A80_avg_result <- rbind(agents_9_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A80_avg, x == 6)))))
agents_9_threshold_A80_avg_result <- rbind(agents_9_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A80_avg, x == 7)))))
agents_9_threshold_A80_avg_result <- rbind(agents_9_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A80_avg, x == 8)))))
agents_9_threshold_A80_avg_result <- rbind(agents_9_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_A80_avg, x == 9)))))
agents_9_threshold_A80_avg_result

#DISSAPROVE 20
agents_9_thresholdD_20 <- agents_9[agents_9[,grep(20,threshold_disapprove)],]
agents_9_thresholdD_20_A60 <- agents_9_thresholdD_20[agents_9_thresholdD_20[,grep(60,threshold_approve)],]
agents_9_thresholdD_20_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdD_20_A60$util_agent_splitted))), nrow=length(agents_9_thresholdD_20_A60$util_agent_splitted), byrow=T))
agents_9_thresholdD_20_A60_utils_avg <- colMeans(agents_9_thresholdD_20_A60_utils)
agents_9_thresholdD_20_A60_utils_avg <- data.frame(y = agents_9_thresholdD_20_A60_utils_avg)
agents_9_thresholdD_20_A60_utils_avg$x <- seq.int(9)
agents_9_thresholdD_20_A60_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdD_20_A60_utils_avg_2$y[1] <- agents_9_thresholdD_20_A60_utils_avg$y[1]
agents_9_thresholdD_20_A60_utils_avg_2$y[2] <- agents_9_thresholdD_20_A60_utils_avg$y[2]
agents_9_thresholdD_20_A60_utils_avg_2$y[3] <- agents_9_thresholdD_20_A60_utils_avg$y[3]
agents_9_thresholdD_20_A60_utils_avg_2$y[4] <- agents_9_thresholdD_20_A60_utils_avg$y[4]
agents_9_thresholdD_20_A60_utils_avg_2$y[5] <- agents_9_thresholdD_20_A60_utils_avg$y[5]
agents_9_thresholdD_20_A60_utils_avg_2$y[6] <- agents_9_thresholdD_20_A60_utils_avg$y[6]
agents_9_thresholdD_20_A60_utils_avg_2$y[7] <- agents_9_thresholdD_20_A60_utils_avg$y[7]
agents_9_thresholdD_20_A60_utils_avg_2$y[8] <- agents_9_thresholdD_20_A60_utils_avg$y[8]
agents_9_thresholdD_20_A60_utils_avg_2$y[9] <- agents_9_thresholdD_20_A60_utils_avg$y[9]

agents_9_thresholdD_20_A70 <- agents_9_thresholdD_20[agents_9_thresholdD_20[,grep(70,threshold_approve)],]
agents_9_thresholdD_20_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdD_20_A70$util_agent_splitted))), nrow=length(agents_9_thresholdD_20_A70$util_agent_splitted), byrow=T))
agents_9_thresholdD_20_A70_utils_avg <- colMeans(agents_9_thresholdD_20_A70_utils)
agents_9_thresholdD_20_A70_utils_avg <- data.frame(y = agents_9_thresholdD_20_A70_utils_avg)
agents_9_thresholdD_20_A70_utils_avg$x <- seq.int(9)
agents_9_thresholdD_20_A70_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdD_20_A70_utils_avg_2$y[1] <- agents_9_thresholdD_20_A70_utils_avg$y[1]
agents_9_thresholdD_20_A70_utils_avg_2$y[2] <- agents_9_thresholdD_20_A70_utils_avg$y[2]
agents_9_thresholdD_20_A70_utils_avg_2$y[3] <- agents_9_thresholdD_20_A70_utils_avg$y[3]
agents_9_thresholdD_20_A70_utils_avg_2$y[4] <- agents_9_thresholdD_20_A70_utils_avg$y[4]
agents_9_thresholdD_20_A70_utils_avg_2$y[5] <- agents_9_thresholdD_20_A70_utils_avg$y[5]
agents_9_thresholdD_20_A70_utils_avg_2$y[6] <- agents_9_thresholdD_20_A70_utils_avg$y[6]
agents_9_thresholdD_20_A70_utils_avg_2$y[7] <- agents_9_thresholdD_20_A70_utils_avg$y[7]
agents_9_thresholdD_20_A70_utils_avg_2$y[8] <- agents_9_thresholdD_20_A70_utils_avg$y[8]
agents_9_thresholdD_20_A70_utils_avg_2$y[9] <- agents_9_thresholdD_20_A70_utils_avg$y[9]

agents_9_thresholdD_20_A80 <- agents_9_thresholdD_20[agents_9_thresholdD_20[,grep(80,threshold_approve)],]
agents_9_thresholdD_20_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdD_20_A80$util_agent_splitted))), nrow=length(agents_9_thresholdD_20_A80$util_agent_splitted), byrow=T))
agents_9_thresholdD_20_A80_utils_avg <- colMeans(agents_9_thresholdD_20_A80_utils)
agents_9_thresholdD_20_A80_utils_avg <- data.frame(y = agents_9_thresholdD_20_A80_utils_avg)
agents_9_thresholdD_20_A80_utils_avg$x <- seq.int(9)
agents_9_thresholdD_20_A80_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdD_20_A80_utils_avg_2$y[1] <- agents_9_thresholdD_20_A80_utils_avg$y[1]
agents_9_thresholdD_20_A80_utils_avg_2$y[2] <- agents_9_thresholdD_20_A80_utils_avg$y[2]
agents_9_thresholdD_20_A80_utils_avg_2$y[3] <- agents_9_thresholdD_20_A80_utils_avg$y[3]
agents_9_thresholdD_20_A80_utils_avg_2$y[4] <- agents_9_thresholdD_20_A80_utils_avg$y[4]
agents_9_thresholdD_20_A80_utils_avg_2$y[5] <- agents_9_thresholdD_20_A80_utils_avg$y[5]
agents_9_thresholdD_20_A80_utils_avg_2$y[6] <- agents_9_thresholdD_20_A80_utils_avg$y[6]
agents_9_thresholdD_20_A80_utils_avg_2$y[7] <- agents_9_thresholdD_20_A80_utils_avg$y[7]
agents_9_thresholdD_20_A80_utils_avg_2$y[8] <- agents_9_thresholdD_20_A80_utils_avg$y[8]
agents_9_thresholdD_20_A80_utils_avg_2$y[9] <- agents_9_thresholdD_20_A80_utils_avg$y[9]

agents_9_threshold_D20_avg <- data.frame(rbind(agents_9_thresholdD_20_A60_utils_avg_2, agents_9_thresholdD_20_A70_utils_avg_2,agents_9_thresholdD_20_A80_utils_avg_2))
agents_9_threshold_D20_avg_result <- agents_9_threshold_D20_avg[FALSE,]
agents_9_threshold_D20_avg_result <- data.frame(t(colMeans(subset(agents_9_threshold_D20_avg, x == 1))))
agents_9_threshold_D20_avg_result <- rbind(agents_9_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D20_avg, x == 2)))))
agents_9_threshold_D20_avg_result <- rbind(agents_9_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D20_avg, x == 3)))))
agents_9_threshold_D20_avg_result <- rbind(agents_9_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D20_avg, x == 4)))))
agents_9_threshold_D20_avg_result <- rbind(agents_9_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D20_avg, x == 5)))))
agents_9_threshold_D20_avg_result <- rbind(agents_9_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D20_avg, x == 6)))))
agents_9_threshold_D20_avg_result <- rbind(agents_9_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D20_avg, x == 7)))))
agents_9_threshold_D20_avg_result <- rbind(agents_9_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D20_avg, x == 8)))))
agents_9_threshold_D20_avg_result <- rbind(agents_9_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D20_avg, x == 9)))))
agents_9_threshold_D20_avg_result

#disapprove 30
agents_9_thresholdD_30 <- agents_9[agents_9[,grep(30,threshold_disapprove)],]
agents_9_thresholdD_30_D20 <- agents_9_thresholdD_30[agents_9_thresholdD_30[,grep(60,threshold_approve)],]
agents_9_thresholdD_30_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdD_30_D20$util_agent_splitted))), nrow=length(agents_9_thresholdD_30_D20$util_agent_splitted), byrow=T))
agents_9_thresholdD_30_A60_utils_avg <- colMeans(agents_9_thresholdD_30_A60_utils)
agents_9_thresholdD_30_A60_utils_avg <- data.frame(y = agents_9_thresholdD_30_A60_utils_avg)
agents_9_thresholdD_30_A60_utils_avg$x <- seq.int(9)
agents_9_thresholdD_30_A60_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdD_30_A60_utils_avg_2$y[1] <- agents_9_thresholdD_30_A60_utils_avg$y[1]
agents_9_thresholdD_30_A60_utils_avg_2$y[2] <- agents_9_thresholdD_30_A60_utils_avg$y[2]
agents_9_thresholdD_30_A60_utils_avg_2$y[3] <- agents_9_thresholdD_30_A60_utils_avg$y[3]
agents_9_thresholdD_30_A60_utils_avg_2$y[4] <- agents_9_thresholdD_30_A60_utils_avg$y[4]
agents_9_thresholdD_30_A60_utils_avg_2$y[5] <- agents_9_thresholdD_30_A60_utils_avg$y[5]
agents_9_thresholdD_30_A60_utils_avg_2$y[6] <- agents_9_thresholdD_30_A60_utils_avg$y[6]
agents_9_thresholdD_30_A60_utils_avg_2$y[7] <- agents_9_thresholdD_30_A60_utils_avg$y[7]
agents_9_thresholdD_30_A60_utils_avg_2$y[8] <- agents_9_thresholdD_30_A60_utils_avg$y[8]
agents_9_thresholdD_30_A60_utils_avg_2$y[9] <- agents_9_thresholdD_30_A60_utils_avg$y[9]

agents_9_thresholdD_30_A70 <- agents_9_thresholdD_30[agents_9_thresholdD_30[,grep(70,threshold_approve)],]
agents_9_thresholdD_30_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdD_30_A70$util_agent_splitted))), nrow=length(agents_9_thresholdD_30_A70$util_agent_splitted), byrow=T))
agents_9_thresholdD_30_A70_utils_avg <- colMeans(agents_9_thresholdD_30_A70_utils)
agents_9_thresholdD_30_A70_utils_avg <- data.frame(y = agents_9_thresholdD_30_A70_utils_avg)
agents_9_thresholdD_30_A70_utils_avg$x <- seq.int(9)
agents_9_thresholdD_30_A70_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdD_30_A70_utils_avg_2$y[1] <- agents_9_thresholdD_30_A70_utils_avg$y[1]
agents_9_thresholdD_30_A70_utils_avg_2$y[2] <- agents_9_thresholdD_30_A70_utils_avg$y[2]
agents_9_thresholdD_30_A70_utils_avg_2$y[3] <- agents_9_thresholdD_30_A70_utils_avg$y[3]
agents_9_thresholdD_30_A70_utils_avg_2$y[4] <- agents_9_thresholdD_30_A70_utils_avg$y[4]
agents_9_thresholdD_30_A70_utils_avg_2$y[5] <- agents_9_thresholdD_30_A70_utils_avg$y[5]
agents_9_thresholdD_30_A70_utils_avg_2$y[6] <- agents_9_thresholdD_30_A70_utils_avg$y[6]
agents_9_thresholdD_30_A70_utils_avg_2$y[7] <- agents_9_thresholdD_30_A70_utils_avg$y[7]
agents_9_thresholdD_30_A70_utils_avg_2$y[8] <- agents_9_thresholdD_30_A70_utils_avg$y[8]
agents_9_thresholdD_30_A70_utils_avg_2$y[9] <- agents_9_thresholdD_30_A70_utils_avg$y[9]


agents_9_thresholdD_30_A80 <- agents_9_thresholdD_30[agents_9_thresholdD_30[,grep(80,threshold_approve)],]
agents_9_thresholdD_30_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdD_30_A80$util_agent_splitted))), nrow=length(agents_9_thresholdD_30_A80$util_agent_splitted), byrow=T))
agents_9_thresholdD_30_A80_utils_avg <- colMeans(agents_9_thresholdD_30_A80_utils)
agents_9_thresholdD_30_A80_utils_avg <- data.frame(y = agents_9_thresholdD_30_A80_utils_avg)
agents_9_thresholdD_30_A80_utils_avg$x <- seq.int(9)
agents_9_thresholdD_30_A80_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdD_30_A80_utils_avg_2$y[1] <- agents_9_thresholdD_30_A80_utils_avg$y[1]
agents_9_thresholdD_30_A80_utils_avg_2$y[2] <- agents_9_thresholdD_30_A80_utils_avg$y[2]
agents_9_thresholdD_30_A80_utils_avg_2$y[3] <- agents_9_thresholdD_30_A80_utils_avg$y[3]
agents_9_thresholdD_30_A80_utils_avg_2$y[4] <- agents_9_thresholdD_30_A80_utils_avg$y[4]
agents_9_thresholdD_30_A80_utils_avg_2$y[5] <- agents_9_thresholdD_30_A80_utils_avg$y[5]
agents_9_thresholdD_30_A80_utils_avg_2$y[6] <- agents_9_thresholdD_30_A80_utils_avg$y[6]
agents_9_thresholdD_30_A80_utils_avg_2$y[7] <- agents_9_thresholdD_30_A80_utils_avg$y[7]
agents_9_thresholdD_30_A80_utils_avg_2$y[8] <- agents_9_thresholdD_30_A80_utils_avg$y[8]
agents_9_thresholdD_30_A80_utils_avg_2$y[9] <- agents_9_thresholdD_30_A80_utils_avg$y[9]

agents_9_threshold_D30_avg <- data.frame(rbind(agents_9_thresholdD_30_A60_utils_avg_2, agents_9_thresholdD_30_A70_utils_avg_2,agents_9_thresholdD_30_A80_utils_avg_2))
agents_9_threshold_D30_avg_result <- agents_9_threshold_D30_avg[FALSE,]
agents_9_threshold_D30_avg_result <- data.frame(t(colMeans(subset(agents_9_threshold_D30_avg, x == 1))))
agents_9_threshold_D30_avg_result <- rbind(agents_9_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D30_avg, x == 2)))))
agents_9_threshold_D30_avg_result <- rbind(agents_9_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D30_avg, x == 3)))))
agents_9_threshold_D30_avg_result <- rbind(agents_9_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D30_avg, x == 4)))))
agents_9_threshold_D30_avg_result <- rbind(agents_9_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D30_avg, x == 5)))))
agents_9_threshold_D30_avg_result <- rbind(agents_9_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D30_avg, x == 6)))))
agents_9_threshold_D30_avg_result <- rbind(agents_9_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D30_avg, x == 7)))))
agents_9_threshold_D30_avg_result <- rbind(agents_9_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D30_avg, x == 8)))))
agents_9_threshold_D30_avg_result <- rbind(agents_9_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D30_avg, x == 9)))))
agents_9_threshold_D30_avg_result

#disapprove 40
agents_9_thresholdD_40 <- agents_9[agents_9[,grep(40,threshold_disapprove)],]
agents_9_thresholdD_40_D20 <- agents_9_thresholdD_40[agents_9_thresholdD_40[,grep(60,threshold_approve)],]
agents_9_thresholdD_40_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdD_40_D20$util_agent_splitted))), nrow=length(agents_9_thresholdD_40_D20$util_agent_splitted), byrow=T))
agents_9_thresholdD_40_A60_utils_avg <- colMeans(agents_9_thresholdD_40_A60_utils)
agents_9_thresholdD_40_A60_utils_avg <- data.frame(y = agents_9_thresholdD_40_A60_utils_avg)
agents_9_thresholdD_40_A60_utils_avg$x <- seq.int(9)
agents_9_thresholdD_40_A60_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdD_40_A60_utils_avg_2$y[1] <- agents_9_thresholdD_40_A60_utils_avg$y[1]
agents_9_thresholdD_40_A60_utils_avg_2$y[2] <- agents_9_thresholdD_40_A60_utils_avg$y[2]
agents_9_thresholdD_40_A60_utils_avg_2$y[3] <- agents_9_thresholdD_40_A60_utils_avg$y[3]
agents_9_thresholdD_40_A60_utils_avg_2$y[4] <- agents_9_thresholdD_40_A60_utils_avg$y[4]
agents_9_thresholdD_40_A60_utils_avg_2$y[5] <- agents_9_thresholdD_40_A60_utils_avg$y[5]
agents_9_thresholdD_40_A60_utils_avg_2$y[6] <- agents_9_thresholdD_40_A60_utils_avg$y[6]
agents_9_thresholdD_40_A60_utils_avg_2$y[7] <- agents_9_thresholdD_40_A60_utils_avg$y[7]
agents_9_thresholdD_40_A60_utils_avg_2$y[8] <- agents_9_thresholdD_40_A60_utils_avg$y[8]
agents_9_thresholdD_40_A60_utils_avg_2$y[9] <- agents_9_thresholdD_40_A60_utils_avg$y[9]

agents_9_thresholdD_40_A70 <- agents_9_thresholdD_40[agents_9_thresholdD_40[,grep(70,threshold_approve)],]
agents_9_thresholdD_40_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdD_40_A70$util_agent_splitted))), nrow=length(agents_9_thresholdD_40_A70$util_agent_splitted), byrow=T))
agents_9_thresholdD_40_A70_utils_avg <- colMeans(agents_9_thresholdD_40_A70_utils)
agents_9_thresholdD_40_A70_utils_avg <- data.frame(y = agents_9_thresholdD_40_A70_utils_avg)
agents_9_thresholdD_40_A70_utils_avg$x <- seq.int(9)
agents_9_thresholdD_40_A70_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdD_40_A70_utils_avg_2$y[1] <- agents_9_thresholdD_40_A70_utils_avg$y[1]
agents_9_thresholdD_40_A70_utils_avg_2$y[2] <- agents_9_thresholdD_40_A70_utils_avg$y[2]
agents_9_thresholdD_40_A70_utils_avg_2$y[3] <- agents_9_thresholdD_40_A70_utils_avg$y[3]
agents_9_thresholdD_40_A70_utils_avg_2$y[4] <- agents_9_thresholdD_40_A70_utils_avg$y[4]
agents_9_thresholdD_40_A70_utils_avg_2$y[5] <- agents_9_thresholdD_40_A70_utils_avg$y[5]
agents_9_thresholdD_40_A70_utils_avg_2$y[6] <- agents_9_thresholdD_40_A70_utils_avg$y[6]
agents_9_thresholdD_40_A70_utils_avg_2$y[7] <- agents_9_thresholdD_40_A70_utils_avg$y[7]
agents_9_thresholdD_40_A70_utils_avg_2$y[8] <- agents_9_thresholdD_40_A70_utils_avg$y[8]
agents_9_thresholdD_40_A70_utils_avg_2$y[9] <- agents_9_thresholdD_40_A70_utils_avg$y[9]


agents_9_thresholdD_40_A80 <- agents_9_thresholdD_40[agents_9_thresholdD_40[,grep(80,threshold_approve)],]
agents_9_thresholdD_40_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_9_thresholdD_40_A80$util_agent_splitted))), nrow=length(agents_9_thresholdD_40_A80$util_agent_splitted), byrow=T))
agents_9_thresholdD_40_A80_utils_avg <- colMeans(agents_9_thresholdD_40_A80_utils)
agents_9_thresholdD_40_A80_utils_avg <- data.frame(y = agents_9_thresholdD_40_A80_utils_avg)
agents_9_thresholdD_40_A80_utils_avg$x <- seq.int(9)
agents_9_thresholdD_40_A80_utils_avg_2 <- agents_9_all_utils_avg
agents_9_thresholdD_40_A80_utils_avg_2$y[1] <- agents_9_thresholdD_40_A80_utils_avg$y[1]
agents_9_thresholdD_40_A80_utils_avg_2$y[2] <- agents_9_thresholdD_40_A80_utils_avg$y[2]
agents_9_thresholdD_40_A80_utils_avg_2$y[3] <- agents_9_thresholdD_40_A80_utils_avg$y[3]
agents_9_thresholdD_40_A80_utils_avg_2$y[4] <- agents_9_thresholdD_40_A80_utils_avg$y[4]
agents_9_thresholdD_40_A80_utils_avg_2$y[5] <- agents_9_thresholdD_40_A80_utils_avg$y[5]
agents_9_thresholdD_40_A80_utils_avg_2$y[6] <- agents_9_thresholdD_40_A80_utils_avg$y[6]
agents_9_thresholdD_40_A80_utils_avg_2$y[7] <- agents_9_thresholdD_40_A80_utils_avg$y[7]
agents_9_thresholdD_40_A80_utils_avg_2$y[8] <- agents_9_thresholdD_40_A80_utils_avg$y[8]
agents_9_thresholdD_40_A80_utils_avg_2$y[9] <- agents_9_thresholdD_40_A80_utils_avg$y[9]

agents_9_threshold_D40_avg <- data.frame(rbind(agents_9_thresholdD_40_A60_utils_avg_2, agents_9_thresholdD_40_A70_utils_avg_2,agents_9_thresholdD_40_A80_utils_avg_2))
agents_9_threshold_D40_avg_result <- agents_9_threshold_D40_avg[FALSE,]
agents_9_threshold_D40_avg_result <- data.frame(t(colMeans(subset(agents_9_threshold_D40_avg, x == 1))))
agents_9_threshold_D40_avg_result <- rbind(agents_9_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D40_avg, x == 2)))))
agents_9_threshold_D40_avg_result <- rbind(agents_9_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D40_avg, x == 3)))))
agents_9_threshold_D40_avg_result <- rbind(agents_9_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D40_avg, x == 4)))))
agents_9_threshold_D40_avg_result <- rbind(agents_9_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D40_avg, x == 5)))))
agents_9_threshold_D40_avg_result <- rbind(agents_9_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D40_avg, x == 6)))))
agents_9_threshold_D40_avg_result <- rbind(agents_9_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D40_avg, x == 7)))))
agents_9_threshold_D40_avg_result <- rbind(agents_9_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D40_avg, x == 8)))))
agents_9_threshold_D40_avg_result <- rbind(agents_9_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_9_threshold_D40_avg, x == 9)))))
agents_9_threshold_D40_avg_result

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
ggplot(data=agents_9_all_utils, aes(x=x, y=y, group=row, scale_size(guide = "none"))) +
  geom_line(linetype="solid", color="black", alpha=0.04) +
  #xlim(1, 9) +
  ylim(0.55, 0.7) + labs(x = "position in the game", y="utily") +
  #geom_line(data=agents_9_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) +
  geom_line(data=agents_9_threshold_A60_avg_result,aes(x=x,y=y),color="#00de3b",size=1,alpha=0.8) +
  geom_line(data=agents_9_threshold_A70_avg_result,aes(x=x,y=y),color="#00b831",size=1,alpha=0.8) +
  geom_line(data=agents_9_threshold_A80_avg_result,aes(x=x,y=y),color="#008223",size=1,alpha=0.8) +
  geom_line(data=agents_9_threshold_D20_avg_result,aes(x=x,y=y),color="#e33900",size=1,alpha=0.8) +
  geom_line(data=agents_9_threshold_D30_avg_result,aes(x=x,y=y),color="#ad2c00",size=1,alpha=0.8) +
  geom_line(data=agents_9_threshold_D40_avg_result,aes(x=x,y=y),color="#701d00",size=1,alpha=0.8) +
  scale_x_continuous(breaks = int_breaks) +
  #geom_line(data=agents_9_thresholdA_60_D20_utils_avg_2,aes(x=x,y=y),color="#0077ff",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_60_D30_utils_avg_2,aes(x=x,y=y),color="#0053b3",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_60_D40_utils_avg_2,aes(x=x,y=y),color="#3b96ff",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_70_D20_utils_avg_2,aes(x=x,y=y),color="#00ff00",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_70_D30_utils_avg_2,aes(x=x,y=y),color="#00a800",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_70_D40_utils_avg_2,aes(x=x,y=y),color="#80ff80",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_80_D20_utils_avg_2,aes(x=x,y=y),color="#ff0000",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_80_D30_utils_avg_2,aes(x=x,y=y),color="#8c0000",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_80_D40_utils_avg_2,aes(x=x,y=y),color="#ff6969",size=1,alpha=0.8) +
  theme(legend.position="right")




plot(agents_9$util_agent_splitted)





###########################################################################
###########################################################################
###                                                                     ###
###                                 AGENTS 16                           ###
###                                                                     ###
###########################################################################
###########################################################################
agents_16$util_agent <- str_remove(agents_16$util_agent, "\\[")
agents_16$util_agent <- str_remove(agents_16$util_agent, "\\]")
agents_16$util_agent_splitted <- strsplit(agents_16$util_agent, ",")

agents_16_utils <- data.frame(matrix(unlist(agents_16$util_agent_splitted), nrow=length(agents_16$util_agent_splitted), byrow=T))
agents_16_utils_1 <- data.frame(y=as.numeric(paste(agents_16_utils$X1)))
agents_16_utils_2 <- data.frame(y=as.numeric(paste(agents_16_utils$X2)))
agents_16_utils_3 <- data.frame(y=as.numeric(paste(agents_16_utils$X3)))
agents_16_utils_4 <- data.frame(y=as.numeric(paste(agents_16_utils$X4)))
agents_16_utils_5 <- data.frame(y=as.numeric(paste(agents_16_utils$X5)))
agents_16_utils_6 <- data.frame(y=as.numeric(paste(agents_16_utils$X6)))
agents_16_utils_7 <- data.frame(y=as.numeric(paste(agents_16_utils$X7)))
agents_16_utils_8 <- data.frame(y=as.numeric(paste(agents_16_utils$X8)))
agents_16_utils_9 <- data.frame(y=as.numeric(paste(agents_16_utils$X9)))
agents_16_utils_10 <- data.frame(y=as.numeric(paste(agents_16_utils$X10)))
agents_16_utils_11 <- data.frame(y=as.numeric(paste(agents_16_utils$X11)))
agents_16_utils_12 <- data.frame(y=as.numeric(paste(agents_16_utils$X12)))
agents_16_utils_13 <- data.frame(y=as.numeric(paste(agents_16_utils$X13)))
agents_16_utils_14 <- data.frame(y=as.numeric(paste(agents_16_utils$X14)))
agents_16_utils_15 <- data.frame(y=as.numeric(paste(agents_16_utils$X15)))
agents_16_utils_16 <- data.frame(y=as.numeric(paste(agents_16_utils$X16)))
agents_16_utils_1$row <- seq.int(nrow(agents_16_utils_1))
agents_16_utils_2$row <- seq.int(nrow(agents_16_utils_2))
agents_16_utils_3$row <- seq.int(nrow(agents_16_utils_3))
agents_16_utils_4$row <- seq.int(nrow(agents_16_utils_4))
agents_16_utils_5$row <- seq.int(nrow(agents_16_utils_5))
agents_16_utils_6$row <- seq.int(nrow(agents_16_utils_6))
agents_16_utils_7$row <- seq.int(nrow(agents_16_utils_7))
agents_16_utils_8$row <- seq.int(nrow(agents_16_utils_8))
agents_16_utils_9$row <- seq.int(nrow(agents_16_utils_9))
agents_16_utils_10$row <- seq.int(nrow(agents_16_utils_10))
agents_16_utils_11$row <- seq.int(nrow(agents_16_utils_11))
agents_16_utils_12$row <- seq.int(nrow(agents_16_utils_12))
agents_16_utils_13$row <- seq.int(nrow(agents_16_utils_13))
agents_16_utils_14$row <- seq.int(nrow(agents_16_utils_14))
agents_16_utils_15$row <- seq.int(nrow(agents_16_utils_15))
agents_16_utils_16$row <- seq.int(nrow(agents_16_utils_16))
agents_16_utils_1$x <- 1
agents_16_utils_2$x <- 2
agents_16_utils_3$x <- 3
agents_16_utils_4$x <- 4
agents_16_utils_5$x <- 5
agents_16_utils_6$x <- 6
agents_16_utils_7$x <- 7
agents_16_utils_8$x <- 8
agents_16_utils_9$x <- 9
agents_16_utils_10$x <- 10
agents_16_utils_11$x <- 11
agents_16_utils_12$x <- 12
agents_16_utils_13$x <- 13
agents_16_utils_14$x <- 14
agents_16_utils_15$x <- 15
agents_16_utils_16$x <- 16

agents_16_all_utils <- rbind(agents_16_utils_1, agents_16_utils_2, agents_16_utils_3, agents_16_utils_4, agents_16_utils_5, agents_16_utils_6, agents_16_utils_7, agents_16_utils_8, agents_16_utils_9, agents_16_utils_10, agents_16_utils_11, agents_16_utils_12, agents_16_utils_13, agents_16_utils_14, agents_16_utils_15, agents_16_utils_16)

agents_16_utils_1_avg <- colMeans(agents_16_utils_1)
agents_16_utils_2_avg <- colMeans(agents_16_utils_2)
agents_16_utils_3_avg <- colMeans(agents_16_utils_3)
agents_16_utils_4_avg <- colMeans(agents_16_utils_4)
agents_16_utils_5_avg <- colMeans(agents_16_utils_5)
agents_16_utils_6_avg <- colMeans(agents_16_utils_6)
agents_16_utils_7_avg <- colMeans(agents_16_utils_7)
agents_16_utils_8_avg <- colMeans(agents_16_utils_8)
agents_16_utils_9_avg <- colMeans(agents_16_utils_9)
agents_16_utils_10_avg <- colMeans(agents_16_utils_10)
agents_16_utils_11_avg <- colMeans(agents_16_utils_11)
agents_16_utils_12_avg <- colMeans(agents_16_utils_12)
agents_16_utils_13_avg <- colMeans(agents_16_utils_13)
agents_16_utils_14_avg <- colMeans(agents_16_utils_14)
agents_16_utils_15_avg <- colMeans(agents_16_utils_15)
agents_16_utils_16_avg <- colMeans(agents_16_utils_16)

agents_16_all_utils_avg <- data.frame(rbind(agents_16_utils_1_avg, agents_16_utils_2_avg, agents_16_utils_3_avg, agents_16_utils_4_avg, agents_16_utils_5_avg, agents_16_utils_6_avg, agents_16_utils_7_avg, agents_16_utils_8_avg, agents_16_utils_9_avg, agents_16_utils_10_avg, agents_16_utils_11_avg, agents_16_utils_12_avg, agents_16_utils_13_avg, agents_16_utils_14_avg, agents_16_utils_15_avg, agents_16_utils_16_avg))
agents_16_all_utils_avg

#APPROVE 60
agents_16_thresholdA_60 <- agents_16[agents_16[,grep(60,threshold_approve)],]
agents_16_thresholdA_60_D20 <- agents_16_thresholdA_60[agents_16_thresholdA_60[,grep(20,threshold_disapprove)],]
agents_16_thresholdA_60_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdA_60_D20$util_agent_splitted))), nrow=length(agents_16_thresholdA_60_D20$util_agent_splitted), byrow=T))
agents_16_thresholdA_60_D20_utils_avg <- colMeans(agents_16_thresholdA_60_D20_utils)
agents_16_thresholdA_60_D20_utils_avg <- data.frame(y = agents_16_thresholdA_60_D20_utils_avg)
agents_16_thresholdA_60_D20_utils_avg$x <- seq.int(16)
agents_16_thresholdA_60_D20_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdA_60_D20_utils_avg_2$y[1] <- agents_16_thresholdA_60_D20_utils_avg$y[1]
agents_16_thresholdA_60_D20_utils_avg_2$y[2] <- agents_16_thresholdA_60_D20_utils_avg$y[2]
agents_16_thresholdA_60_D20_utils_avg_2$y[3] <- agents_16_thresholdA_60_D20_utils_avg$y[3]
agents_16_thresholdA_60_D20_utils_avg_2$y[4] <- agents_16_thresholdA_60_D20_utils_avg$y[4]
agents_16_thresholdA_60_D20_utils_avg_2$y[5] <- agents_16_thresholdA_60_D20_utils_avg$y[5]
agents_16_thresholdA_60_D20_utils_avg_2$y[6] <- agents_16_thresholdA_60_D20_utils_avg$y[6]
agents_16_thresholdA_60_D20_utils_avg_2$y[7] <- agents_16_thresholdA_60_D20_utils_avg$y[7]
agents_16_thresholdA_60_D20_utils_avg_2$y[8] <- agents_16_thresholdA_60_D20_utils_avg$y[8]
agents_16_thresholdA_60_D20_utils_avg_2$y[9] <- agents_16_thresholdA_60_D20_utils_avg$y[9]
agents_16_thresholdA_60_D20_utils_avg_2$y[10] <- agents_16_thresholdA_60_D20_utils_avg$y[10]
agents_16_thresholdA_60_D20_utils_avg_2$y[11] <- agents_16_thresholdA_60_D20_utils_avg$y[11]
agents_16_thresholdA_60_D20_utils_avg_2$y[12] <- agents_16_thresholdA_60_D20_utils_avg$y[12]
agents_16_thresholdA_60_D20_utils_avg_2$y[13] <- agents_16_thresholdA_60_D20_utils_avg$y[13]
agents_16_thresholdA_60_D20_utils_avg_2$y[14] <- agents_16_thresholdA_60_D20_utils_avg$y[14]
agents_16_thresholdA_60_D20_utils_avg_2$y[15] <- agents_16_thresholdA_60_D20_utils_avg$y[15]
agents_16_thresholdA_60_D20_utils_avg_2$y[16] <- agents_16_thresholdA_60_D20_utils_avg$y[16]

agents_16_thresholdA_60_D30 <- agents_16_thresholdA_60[agents_16_thresholdA_60[,grep(30,threshold_disapprove)],]
agents_16_thresholdA_60_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdA_60_D30$util_agent_splitted))), nrow=length(agents_16_thresholdA_60_D30$util_agent_splitted), byrow=T))
agents_16_thresholdA_60_D30_utils_avg <- colMeans(agents_16_thresholdA_60_D30_utils)
agents_16_thresholdA_60_D30_utils_avg <- data.frame(y = agents_16_thresholdA_60_D30_utils_avg)
agents_16_thresholdA_60_D30_utils_avg$x <- seq.int(16)
agents_16_thresholdA_60_D30_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdA_60_D30_utils_avg_2$y[1] <- agents_16_thresholdA_60_D30_utils_avg$y[1]
agents_16_thresholdA_60_D30_utils_avg_2$y[2] <- agents_16_thresholdA_60_D30_utils_avg$y[2]
agents_16_thresholdA_60_D30_utils_avg_2$y[3] <- agents_16_thresholdA_60_D30_utils_avg$y[3]
agents_16_thresholdA_60_D30_utils_avg_2$y[4] <- agents_16_thresholdA_60_D30_utils_avg$y[4]
agents_16_thresholdA_60_D30_utils_avg_2$y[5] <- agents_16_thresholdA_60_D30_utils_avg$y[5]
agents_16_thresholdA_60_D30_utils_avg_2$y[6] <- agents_16_thresholdA_60_D30_utils_avg$y[6]
agents_16_thresholdA_60_D30_utils_avg_2$y[7] <- agents_16_thresholdA_60_D30_utils_avg$y[7]
agents_16_thresholdA_60_D30_utils_avg_2$y[8] <- agents_16_thresholdA_60_D30_utils_avg$y[8]
agents_16_thresholdA_60_D30_utils_avg_2$y[9] <- agents_16_thresholdA_60_D30_utils_avg$y[9]
agents_16_thresholdA_60_D30_utils_avg_2$y[10] <- agents_16_thresholdA_60_D30_utils_avg$y[10]
agents_16_thresholdA_60_D30_utils_avg_2$y[11] <- agents_16_thresholdA_60_D30_utils_avg$y[11]
agents_16_thresholdA_60_D30_utils_avg_2$y[12] <- agents_16_thresholdA_60_D30_utils_avg$y[12]
agents_16_thresholdA_60_D30_utils_avg_2$y[13] <- agents_16_thresholdA_60_D30_utils_avg$y[13]
agents_16_thresholdA_60_D30_utils_avg_2$y[14] <- agents_16_thresholdA_60_D30_utils_avg$y[14]
agents_16_thresholdA_60_D30_utils_avg_2$y[15] <- agents_16_thresholdA_60_D30_utils_avg$y[15]
agents_16_thresholdA_60_D30_utils_avg_2$y[16] <- agents_16_thresholdA_60_D30_utils_avg$y[16]

agents_16_thresholdA_60_D40 <- agents_16_thresholdA_60[agents_16_thresholdA_60[,grep(40,threshold_disapprove)],]
agents_16_thresholdA_60_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdA_60_D40$util_agent_splitted))), nrow=length(agents_16_thresholdA_60_D40$util_agent_splitted), byrow=T))
agents_16_thresholdA_60_D40_utils_avg <- colMeans(agents_16_thresholdA_60_D40_utils)
agents_16_thresholdA_60_D40_utils_avg <- data.frame(y = agents_16_thresholdA_60_D40_utils_avg)
agents_16_thresholdA_60_D40_utils_avg$x <- seq.int(16)
agents_16_thresholdA_60_D40_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdA_60_D40_utils_avg_2$y[1] <- agents_16_thresholdA_60_D40_utils_avg$y[1]
agents_16_thresholdA_60_D40_utils_avg_2$y[2] <- agents_16_thresholdA_60_D40_utils_avg$y[2]
agents_16_thresholdA_60_D40_utils_avg_2$y[3] <- agents_16_thresholdA_60_D40_utils_avg$y[3]
agents_16_thresholdA_60_D40_utils_avg_2$y[4] <- agents_16_thresholdA_60_D40_utils_avg$y[4]
agents_16_thresholdA_60_D40_utils_avg_2$y[5] <- agents_16_thresholdA_60_D40_utils_avg$y[5]
agents_16_thresholdA_60_D40_utils_avg_2$y[6] <- agents_16_thresholdA_60_D40_utils_avg$y[6]
agents_16_thresholdA_60_D40_utils_avg_2$y[7] <- agents_16_thresholdA_60_D40_utils_avg$y[7]
agents_16_thresholdA_60_D40_utils_avg_2$y[8] <- agents_16_thresholdA_60_D40_utils_avg$y[8]
agents_16_thresholdA_60_D40_utils_avg_2$y[9] <- agents_16_thresholdA_60_D40_utils_avg$y[9]
agents_16_thresholdA_60_D40_utils_avg_2$y[10] <- agents_16_thresholdA_60_D40_utils_avg$y[10]
agents_16_thresholdA_60_D40_utils_avg_2$y[11] <- agents_16_thresholdA_60_D40_utils_avg$y[11]
agents_16_thresholdA_60_D40_utils_avg_2$y[12] <- agents_16_thresholdA_60_D40_utils_avg$y[12]
agents_16_thresholdA_60_D40_utils_avg_2$y[13] <- agents_16_thresholdA_60_D40_utils_avg$y[13]
agents_16_thresholdA_60_D40_utils_avg_2$y[14] <- agents_16_thresholdA_60_D40_utils_avg$y[14]
agents_16_thresholdA_60_D40_utils_avg_2$y[15] <- agents_16_thresholdA_60_D40_utils_avg$y[15]
agents_16_thresholdA_60_D40_utils_avg_2$y[16] <- agents_16_thresholdA_60_D40_utils_avg$y[16]


agents_16_threshold_A60_avg <- data.frame(rbind(agents_16_thresholdA_60_D20_utils_avg_2, agents_16_thresholdA_60_D30_utils_avg_2,agents_16_thresholdA_60_D40_utils_avg_2))
agents_16_threshold_A60_avg_result <- agents_16_threshold_A60_avg[FALSE,]
agents_16_threshold_A60_avg_result <- data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 1))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 2)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 3)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 4)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 5)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 6)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 7)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 8)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 9)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 10)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 11)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 12)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 13)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 14)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 15)))))
agents_16_threshold_A60_avg_result <- rbind(agents_16_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A60_avg, x == 16)))))

agents_16_threshold_A60_avg_result

#APPROVE 70
agents_16_thresholdA_70 <- agents_16[agents_16[,grep(70,threshold_approve)],]
agents_16_thresholdA_70_D20 <- agents_16_thresholdA_70[agents_16_thresholdA_70[,grep(20,threshold_disapprove)],]
agents_16_thresholdA_70_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdA_70_D20$util_agent_splitted))), nrow=length(agents_16_thresholdA_70_D20$util_agent_splitted), byrow=T))
agents_16_thresholdA_70_D20_utils_avg <- colMeans(agents_16_thresholdA_70_D20_utils)
agents_16_thresholdA_70_D20_utils_avg <- data.frame(y = agents_16_thresholdA_70_D20_utils_avg)
agents_16_thresholdA_70_D20_utils_avg$x <- seq.int(16)
agents_16_thresholdA_70_D20_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdA_70_D20_utils_avg_2$y[1] <- agents_16_thresholdA_70_D20_utils_avg$y[1]
agents_16_thresholdA_70_D20_utils_avg_2$y[2] <- agents_16_thresholdA_70_D20_utils_avg$y[2]
agents_16_thresholdA_70_D20_utils_avg_2$y[3] <- agents_16_thresholdA_70_D20_utils_avg$y[3]
agents_16_thresholdA_70_D20_utils_avg_2$y[4] <- agents_16_thresholdA_70_D20_utils_avg$y[4]
agents_16_thresholdA_70_D20_utils_avg_2$y[5] <- agents_16_thresholdA_70_D20_utils_avg$y[5]
agents_16_thresholdA_70_D20_utils_avg_2$y[6] <- agents_16_thresholdA_70_D20_utils_avg$y[6]
agents_16_thresholdA_70_D20_utils_avg_2$y[7] <- agents_16_thresholdA_70_D20_utils_avg$y[7]
agents_16_thresholdA_70_D20_utils_avg_2$y[8] <- agents_16_thresholdA_70_D20_utils_avg$y[8]
agents_16_thresholdA_70_D20_utils_avg_2$y[9] <- agents_16_thresholdA_70_D20_utils_avg$y[9]
agents_16_thresholdA_70_D20_utils_avg_2$y[10] <- agents_16_thresholdA_70_D20_utils_avg$y[10]
agents_16_thresholdA_70_D20_utils_avg_2$y[11] <- agents_16_thresholdA_70_D20_utils_avg$y[11]
agents_16_thresholdA_70_D20_utils_avg_2$y[12] <- agents_16_thresholdA_70_D20_utils_avg$y[12]
agents_16_thresholdA_70_D20_utils_avg_2$y[13] <- agents_16_thresholdA_70_D20_utils_avg$y[13]
agents_16_thresholdA_70_D20_utils_avg_2$y[14] <- agents_16_thresholdA_70_D20_utils_avg$y[14]
agents_16_thresholdA_70_D20_utils_avg_2$y[15] <- agents_16_thresholdA_70_D20_utils_avg$y[15]
agents_16_thresholdA_70_D20_utils_avg_2$y[16] <- agents_16_thresholdA_70_D20_utils_avg$y[16]


agents_16_thresholdA_70_D30 <- agents_16_thresholdA_70[agents_16_thresholdA_70[,grep(30,threshold_disapprove)],]
agents_16_thresholdA_70_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdA_70_D30$util_agent_splitted))), nrow=length(agents_16_thresholdA_70_D30$util_agent_splitted), byrow=T))
agents_16_thresholdA_70_D30_utils_avg <- colMeans(agents_16_thresholdA_70_D30_utils)
agents_16_thresholdA_70_D30_utils_avg <- data.frame(y = agents_16_thresholdA_70_D30_utils_avg)
agents_16_thresholdA_70_D30_utils_avg$x <- seq.int(16)
agents_16_thresholdA_70_D30_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdA_70_D30_utils_avg_2$y[1] <- agents_16_thresholdA_70_D30_utils_avg$y[1]
agents_16_thresholdA_70_D30_utils_avg_2$y[2] <- agents_16_thresholdA_70_D30_utils_avg$y[2]
agents_16_thresholdA_70_D30_utils_avg_2$y[3] <- agents_16_thresholdA_70_D30_utils_avg$y[3]
agents_16_thresholdA_70_D30_utils_avg_2$y[4] <- agents_16_thresholdA_70_D30_utils_avg$y[4]
agents_16_thresholdA_70_D30_utils_avg_2$y[5] <- agents_16_thresholdA_70_D30_utils_avg$y[5]
agents_16_thresholdA_70_D30_utils_avg_2$y[6] <- agents_16_thresholdA_70_D30_utils_avg$y[6]
agents_16_thresholdA_70_D30_utils_avg_2$y[7] <- agents_16_thresholdA_70_D30_utils_avg$y[7]
agents_16_thresholdA_70_D30_utils_avg_2$y[8] <- agents_16_thresholdA_70_D30_utils_avg$y[8]
agents_16_thresholdA_70_D30_utils_avg_2$y[9] <- agents_16_thresholdA_70_D30_utils_avg$y[9]
agents_16_thresholdA_70_D30_utils_avg_2$y[10] <- agents_16_thresholdA_70_D30_utils_avg$y[10]
agents_16_thresholdA_70_D30_utils_avg_2$y[11] <- agents_16_thresholdA_70_D30_utils_avg$y[11]
agents_16_thresholdA_70_D30_utils_avg_2$y[12] <- agents_16_thresholdA_70_D30_utils_avg$y[12]
agents_16_thresholdA_70_D30_utils_avg_2$y[13] <- agents_16_thresholdA_70_D30_utils_avg$y[13]
agents_16_thresholdA_70_D30_utils_avg_2$y[14] <- agents_16_thresholdA_70_D30_utils_avg$y[14]
agents_16_thresholdA_70_D30_utils_avg_2$y[15] <- agents_16_thresholdA_70_D30_utils_avg$y[15]
agents_16_thresholdA_70_D30_utils_avg_2$y[16] <- agents_16_thresholdA_70_D30_utils_avg$y[16]


agents_16_thresholdA_70_D40 <- agents_16_thresholdA_70[agents_16_thresholdA_70[,grep(40,threshold_disapprove)],]
agents_16_thresholdA_70_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdA_70_D40$util_agent_splitted))), nrow=length(agents_16_thresholdA_70_D40$util_agent_splitted), byrow=T))
agents_16_thresholdA_70_D40_utils_avg <- colMeans(agents_16_thresholdA_70_D40_utils)
agents_16_thresholdA_70_D40_utils_avg <- data.frame(y = agents_16_thresholdA_70_D40_utils_avg)
agents_16_thresholdA_70_D40_utils_avg$x <- seq.int(16)
agents_16_thresholdA_70_D40_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdA_70_D40_utils_avg_2$y[1] <- agents_16_thresholdA_70_D40_utils_avg$y[1]
agents_16_thresholdA_70_D40_utils_avg_2$y[2] <- agents_16_thresholdA_70_D40_utils_avg$y[2]
agents_16_thresholdA_70_D40_utils_avg_2$y[3] <- agents_16_thresholdA_70_D40_utils_avg$y[3]
agents_16_thresholdA_70_D40_utils_avg_2$y[4] <- agents_16_thresholdA_70_D40_utils_avg$y[4]
agents_16_thresholdA_70_D40_utils_avg_2$y[5] <- agents_16_thresholdA_70_D40_utils_avg$y[5]
agents_16_thresholdA_70_D40_utils_avg_2$y[6] <- agents_16_thresholdA_70_D40_utils_avg$y[6]
agents_16_thresholdA_70_D40_utils_avg_2$y[7] <- agents_16_thresholdA_70_D40_utils_avg$y[7]
agents_16_thresholdA_70_D40_utils_avg_2$y[8] <- agents_16_thresholdA_70_D40_utils_avg$y[8]
agents_16_thresholdA_70_D40_utils_avg_2$y[9] <- agents_16_thresholdA_70_D40_utils_avg$y[9]
agents_16_thresholdA_70_D40_utils_avg_2$y[10] <- agents_16_thresholdA_70_D40_utils_avg$y[10]
agents_16_thresholdA_70_D40_utils_avg_2$y[11] <- agents_16_thresholdA_70_D40_utils_avg$y[11]
agents_16_thresholdA_70_D40_utils_avg_2$y[12] <- agents_16_thresholdA_70_D40_utils_avg$y[12]
agents_16_thresholdA_70_D40_utils_avg_2$y[13] <- agents_16_thresholdA_70_D40_utils_avg$y[13]
agents_16_thresholdA_70_D40_utils_avg_2$y[14] <- agents_16_thresholdA_70_D40_utils_avg$y[14]
agents_16_thresholdA_70_D40_utils_avg_2$y[15] <- agents_16_thresholdA_70_D40_utils_avg$y[15]
agents_16_thresholdA_70_D40_utils_avg_2$y[16] <- agents_16_thresholdA_70_D40_utils_avg$y[16]


agents_16_threshold_A70_avg <- data.frame(rbind(agents_16_thresholdA_70_D20_utils_avg_2, agents_16_thresholdA_70_D30_utils_avg_2,agents_16_thresholdA_70_D40_utils_avg_2))
agents_16_threshold_A70_avg_result <- agents_16_threshold_A70_avg[FALSE,]
agents_16_threshold_A70_avg_result <- data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 1))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 2)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 3)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 4)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 5)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 6)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 7)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 8)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 9)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 10)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 11)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 12)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 13)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 14)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 15)))))
agents_16_threshold_A70_avg_result <- rbind(agents_16_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A70_avg, x == 16)))))

agents_16_threshold_A70_avg_result

#APPROVE 80
agents_16_thresholdA_80 <- agents_16[agents_16[,grep(80,threshold_approve)],]
agents_16_thresholdA_80_D20 <- agents_16_thresholdA_80[agents_16_thresholdA_80[,grep(20,threshold_disapprove)],]
agents_16_thresholdA_80_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdA_80_D20$util_agent_splitted))), nrow=length(agents_16_thresholdA_80_D20$util_agent_splitted), byrow=T))
agents_16_thresholdA_80_D20_utils_avg <- colMeans(agents_16_thresholdA_80_D20_utils)
agents_16_thresholdA_80_D20_utils_avg <- data.frame(y = agents_16_thresholdA_80_D20_utils_avg)
agents_16_thresholdA_80_D20_utils_avg$x <- seq.int(16)
agents_16_thresholdA_80_D20_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdA_80_D20_utils_avg_2$y[1] <- agents_16_thresholdA_80_D20_utils_avg$y[1]
agents_16_thresholdA_80_D20_utils_avg_2$y[2] <- agents_16_thresholdA_80_D20_utils_avg$y[2]
agents_16_thresholdA_80_D20_utils_avg_2$y[3] <- agents_16_thresholdA_80_D20_utils_avg$y[3]
agents_16_thresholdA_80_D20_utils_avg_2$y[4] <- agents_16_thresholdA_80_D20_utils_avg$y[4]
agents_16_thresholdA_80_D20_utils_avg_2$y[5] <- agents_16_thresholdA_80_D20_utils_avg$y[5]
agents_16_thresholdA_80_D20_utils_avg_2$y[6] <- agents_16_thresholdA_80_D20_utils_avg$y[6]
agents_16_thresholdA_80_D20_utils_avg_2$y[7] <- agents_16_thresholdA_80_D20_utils_avg$y[7]
agents_16_thresholdA_80_D20_utils_avg_2$y[8] <- agents_16_thresholdA_80_D20_utils_avg$y[8]
agents_16_thresholdA_80_D20_utils_avg_2$y[9] <- agents_16_thresholdA_80_D20_utils_avg$y[9]
agents_16_thresholdA_80_D20_utils_avg_2$y[10] <- agents_16_thresholdA_80_D20_utils_avg$y[10]
agents_16_thresholdA_80_D20_utils_avg_2$y[11] <- agents_16_thresholdA_80_D20_utils_avg$y[11]
agents_16_thresholdA_80_D20_utils_avg_2$y[12] <- agents_16_thresholdA_80_D20_utils_avg$y[12]
agents_16_thresholdA_80_D20_utils_avg_2$y[13] <- agents_16_thresholdA_80_D20_utils_avg$y[13]
agents_16_thresholdA_80_D20_utils_avg_2$y[14] <- agents_16_thresholdA_80_D20_utils_avg$y[14]
agents_16_thresholdA_80_D20_utils_avg_2$y[15] <- agents_16_thresholdA_80_D20_utils_avg$y[15]
agents_16_thresholdA_80_D20_utils_avg_2$y[16] <- agents_16_thresholdA_80_D20_utils_avg$y[16]

agents_16_thresholdA_80_D30 <- agents_16_thresholdA_80[agents_16_thresholdA_80[,grep(30,threshold_disapprove)],]
agents_16_thresholdA_80_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdA_80_D30$util_agent_splitted))), nrow=length(agents_16_thresholdA_80_D30$util_agent_splitted), byrow=T))
agents_16_thresholdA_80_D30_utils_avg <- colMeans(agents_16_thresholdA_80_D30_utils)
agents_16_thresholdA_80_D30_utils_avg <- data.frame(y = agents_16_thresholdA_80_D30_utils_avg)
agents_16_thresholdA_80_D30_utils_avg$x <- seq.int(16)
agents_16_thresholdA_80_D30_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdA_80_D30_utils_avg_2$y[1] <- agents_16_thresholdA_80_D30_utils_avg$y[1]
agents_16_thresholdA_80_D30_utils_avg_2$y[2] <- agents_16_thresholdA_80_D30_utils_avg$y[2]
agents_16_thresholdA_80_D30_utils_avg_2$y[3] <- agents_16_thresholdA_80_D30_utils_avg$y[3]
agents_16_thresholdA_80_D30_utils_avg_2$y[4] <- agents_16_thresholdA_80_D30_utils_avg$y[4]
agents_16_thresholdA_80_D30_utils_avg_2$y[5] <- agents_16_thresholdA_80_D30_utils_avg$y[5]
agents_16_thresholdA_80_D30_utils_avg_2$y[6] <- agents_16_thresholdA_80_D30_utils_avg$y[6]
agents_16_thresholdA_80_D30_utils_avg_2$y[7] <- agents_16_thresholdA_80_D30_utils_avg$y[7]
agents_16_thresholdA_80_D30_utils_avg_2$y[8] <- agents_16_thresholdA_80_D30_utils_avg$y[8]
agents_16_thresholdA_80_D30_utils_avg_2$y[9] <- agents_16_thresholdA_80_D30_utils_avg$y[9]
agents_16_thresholdA_80_D30_utils_avg_2$y[10] <- agents_16_thresholdA_80_D30_utils_avg$y[10]
agents_16_thresholdA_80_D30_utils_avg_2$y[11] <- agents_16_thresholdA_80_D30_utils_avg$y[11]
agents_16_thresholdA_80_D30_utils_avg_2$y[12] <- agents_16_thresholdA_80_D30_utils_avg$y[12]
agents_16_thresholdA_80_D30_utils_avg_2$y[13] <- agents_16_thresholdA_80_D30_utils_avg$y[13]
agents_16_thresholdA_80_D30_utils_avg_2$y[14] <- agents_16_thresholdA_80_D30_utils_avg$y[14]
agents_16_thresholdA_80_D30_utils_avg_2$y[15] <- agents_16_thresholdA_80_D30_utils_avg$y[15]
agents_16_thresholdA_80_D30_utils_avg_2$y[16] <- agents_16_thresholdA_80_D30_utils_avg$y[16]


agents_16_thresholdA_80_D40 <- agents_16_thresholdA_80[agents_16_thresholdA_80[,grep(40,threshold_disapprove)],]
agents_16_thresholdA_80_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdA_80_D40$util_agent_splitted))), nrow=length(agents_16_thresholdA_80_D40$util_agent_splitted), byrow=T))
agents_16_thresholdA_80_D40_utils_avg <- colMeans(agents_16_thresholdA_80_D40_utils)
agents_16_thresholdA_80_D40_utils_avg <- data.frame(y = agents_16_thresholdA_80_D40_utils_avg)
agents_16_thresholdA_80_D40_utils_avg$x <- seq.int(16)
agents_16_thresholdA_80_D40_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdA_80_D40_utils_avg_2$y[1] <- agents_16_thresholdA_80_D40_utils_avg$y[1]
agents_16_thresholdA_80_D40_utils_avg_2$y[2] <- agents_16_thresholdA_80_D40_utils_avg$y[2]
agents_16_thresholdA_80_D40_utils_avg_2$y[3] <- agents_16_thresholdA_80_D40_utils_avg$y[3]
agents_16_thresholdA_80_D40_utils_avg_2$y[4] <- agents_16_thresholdA_80_D40_utils_avg$y[4]
agents_16_thresholdA_80_D40_utils_avg_2$y[5] <- agents_16_thresholdA_80_D40_utils_avg$y[5]
agents_16_thresholdA_80_D40_utils_avg_2$y[6] <- agents_16_thresholdA_80_D40_utils_avg$y[6]
agents_16_thresholdA_80_D40_utils_avg_2$y[7] <- agents_16_thresholdA_80_D40_utils_avg$y[7]
agents_16_thresholdA_80_D40_utils_avg_2$y[8] <- agents_16_thresholdA_80_D40_utils_avg$y[8]
agents_16_thresholdA_80_D40_utils_avg_2$y[9] <- agents_16_thresholdA_80_D40_utils_avg$y[9]
agents_16_thresholdA_80_D40_utils_avg_2$y[10] <- agents_16_thresholdA_80_D40_utils_avg$y[10]
agents_16_thresholdA_80_D40_utils_avg_2$y[11] <- agents_16_thresholdA_80_D40_utils_avg$y[11]
agents_16_thresholdA_80_D40_utils_avg_2$y[12] <- agents_16_thresholdA_80_D40_utils_avg$y[12]
agents_16_thresholdA_80_D40_utils_avg_2$y[13] <- agents_16_thresholdA_80_D40_utils_avg$y[13]
agents_16_thresholdA_80_D40_utils_avg_2$y[14] <- agents_16_thresholdA_80_D40_utils_avg$y[14]
agents_16_thresholdA_80_D40_utils_avg_2$y[15] <- agents_16_thresholdA_80_D40_utils_avg$y[15]
agents_16_thresholdA_80_D40_utils_avg_2$y[16] <- agents_16_thresholdA_80_D40_utils_avg$y[16]

agents_16_threshold_A80_avg <- data.frame(rbind(agents_16_thresholdA_80_D20_utils_avg_2, agents_16_thresholdA_80_D30_utils_avg_2,agents_16_thresholdA_80_D40_utils_avg_2))
agents_16_threshold_A80_avg_result <- agents_16_threshold_A80_avg[FALSE,]
agents_16_threshold_A80_avg_result <- data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 1))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 2)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 3)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 4)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 5)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 6)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 7)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 8)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 9)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 10)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 11)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 12)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 13)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 14)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 15)))))
agents_16_threshold_A80_avg_result <- rbind(agents_16_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_A80_avg, x == 16)))))

agents_16_threshold_A80_avg_result

#DISSAPROVE 20
agents_16_thresholdD_20 <- agents_16[agents_16[,grep(20,threshold_disapprove)],]
agents_16_thresholdD_20_A60 <- agents_16_thresholdD_20[agents_16_thresholdD_20[,grep(60,threshold_approve)],]
agents_16_thresholdD_20_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdD_20_A60$util_agent_splitted))), nrow=length(agents_16_thresholdD_20_A60$util_agent_splitted), byrow=T))
agents_16_thresholdD_20_A60_utils_avg <- colMeans(agents_16_thresholdD_20_A60_utils)
agents_16_thresholdD_20_A60_utils_avg <- data.frame(y = agents_16_thresholdD_20_A60_utils_avg)
agents_16_thresholdD_20_A60_utils_avg$x <- seq.int(16)
agents_16_thresholdD_20_A60_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdD_20_A60_utils_avg_2$y[1] <- agents_16_thresholdD_20_A60_utils_avg$y[1]
agents_16_thresholdD_20_A60_utils_avg_2$y[2] <- agents_16_thresholdD_20_A60_utils_avg$y[2]
agents_16_thresholdD_20_A60_utils_avg_2$y[3] <- agents_16_thresholdD_20_A60_utils_avg$y[3]
agents_16_thresholdD_20_A60_utils_avg_2$y[4] <- agents_16_thresholdD_20_A60_utils_avg$y[4]
agents_16_thresholdD_20_A60_utils_avg_2$y[5] <- agents_16_thresholdD_20_A60_utils_avg$y[5]
agents_16_thresholdD_20_A60_utils_avg_2$y[6] <- agents_16_thresholdD_20_A60_utils_avg$y[6]
agents_16_thresholdD_20_A60_utils_avg_2$y[7] <- agents_16_thresholdD_20_A60_utils_avg$y[7]
agents_16_thresholdD_20_A60_utils_avg_2$y[8] <- agents_16_thresholdD_20_A60_utils_avg$y[8]
agents_16_thresholdD_20_A60_utils_avg_2$y[9] <- agents_16_thresholdD_20_A60_utils_avg$y[9]
agents_16_thresholdD_20_A60_utils_avg_2$y[10] <- agents_16_thresholdD_20_A60_utils_avg$y[10]
agents_16_thresholdD_20_A60_utils_avg_2$y[11] <- agents_16_thresholdD_20_A60_utils_avg$y[11]
agents_16_thresholdD_20_A60_utils_avg_2$y[12] <- agents_16_thresholdD_20_A60_utils_avg$y[12]
agents_16_thresholdD_20_A60_utils_avg_2$y[13] <- agents_16_thresholdD_20_A60_utils_avg$y[13]
agents_16_thresholdD_20_A60_utils_avg_2$y[14] <- agents_16_thresholdD_20_A60_utils_avg$y[14]
agents_16_thresholdD_20_A60_utils_avg_2$y[15] <- agents_16_thresholdD_20_A60_utils_avg$y[15]
agents_16_thresholdD_20_A60_utils_avg_2$y[16] <- agents_16_thresholdD_20_A60_utils_avg$y[16]


agents_16_thresholdD_20_A70 <- agents_16_thresholdD_20[agents_16_thresholdD_20[,grep(70,threshold_approve)],]
agents_16_thresholdD_20_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdD_20_A70$util_agent_splitted))), nrow=length(agents_16_thresholdD_20_A70$util_agent_splitted), byrow=T))
agents_16_thresholdD_20_A70_utils_avg <- colMeans(agents_16_thresholdD_20_A70_utils)
agents_16_thresholdD_20_A70_utils_avg <- data.frame(y = agents_16_thresholdD_20_A70_utils_avg)
agents_16_thresholdD_20_A70_utils_avg$x <- seq.int(16)
agents_16_thresholdD_20_A70_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdD_20_A70_utils_avg_2$y[1] <- agents_16_thresholdD_20_A70_utils_avg$y[1]
agents_16_thresholdD_20_A70_utils_avg_2$y[2] <- agents_16_thresholdD_20_A70_utils_avg$y[2]
agents_16_thresholdD_20_A70_utils_avg_2$y[3] <- agents_16_thresholdD_20_A70_utils_avg$y[3]
agents_16_thresholdD_20_A70_utils_avg_2$y[4] <- agents_16_thresholdD_20_A70_utils_avg$y[4]
agents_16_thresholdD_20_A70_utils_avg_2$y[5] <- agents_16_thresholdD_20_A70_utils_avg$y[5]
agents_16_thresholdD_20_A70_utils_avg_2$y[6] <- agents_16_thresholdD_20_A70_utils_avg$y[6]
agents_16_thresholdD_20_A70_utils_avg_2$y[7] <- agents_16_thresholdD_20_A70_utils_avg$y[7]
agents_16_thresholdD_20_A70_utils_avg_2$y[8] <- agents_16_thresholdD_20_A70_utils_avg$y[8]
agents_16_thresholdD_20_A70_utils_avg_2$y[9] <- agents_16_thresholdD_20_A70_utils_avg$y[9]
agents_16_thresholdD_20_A70_utils_avg_2$y[10] <- agents_16_thresholdD_20_A70_utils_avg$y[10]
agents_16_thresholdD_20_A70_utils_avg_2$y[11] <- agents_16_thresholdD_20_A70_utils_avg$y[11]
agents_16_thresholdD_20_A70_utils_avg_2$y[12] <- agents_16_thresholdD_20_A70_utils_avg$y[12]
agents_16_thresholdD_20_A70_utils_avg_2$y[13] <- agents_16_thresholdD_20_A70_utils_avg$y[13]
agents_16_thresholdD_20_A70_utils_avg_2$y[14] <- agents_16_thresholdD_20_A70_utils_avg$y[14]
agents_16_thresholdD_20_A70_utils_avg_2$y[15] <- agents_16_thresholdD_20_A70_utils_avg$y[15]
agents_16_thresholdD_20_A70_utils_avg_2$y[16] <- agents_16_thresholdD_20_A70_utils_avg$y[16]


agents_16_thresholdD_20_A80 <- agents_16_thresholdD_20[agents_16_thresholdD_20[,grep(80,threshold_approve)],]
agents_16_thresholdD_20_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdD_20_A80$util_agent_splitted))), nrow=length(agents_16_thresholdD_20_A80$util_agent_splitted), byrow=T))
agents_16_thresholdD_20_A80_utils_avg <- colMeans(agents_16_thresholdD_20_A80_utils)
agents_16_thresholdD_20_A80_utils_avg <- data.frame(y = agents_16_thresholdD_20_A80_utils_avg)
agents_16_thresholdD_20_A80_utils_avg$x <- seq.int(16)
agents_16_thresholdD_20_A80_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdD_20_A80_utils_avg_2$y[1] <- agents_16_thresholdD_20_A80_utils_avg$y[1]
agents_16_thresholdD_20_A80_utils_avg_2$y[2] <- agents_16_thresholdD_20_A80_utils_avg$y[2]
agents_16_thresholdD_20_A80_utils_avg_2$y[3] <- agents_16_thresholdD_20_A80_utils_avg$y[3]
agents_16_thresholdD_20_A80_utils_avg_2$y[4] <- agents_16_thresholdD_20_A80_utils_avg$y[4]
agents_16_thresholdD_20_A80_utils_avg_2$y[5] <- agents_16_thresholdD_20_A80_utils_avg$y[5]
agents_16_thresholdD_20_A80_utils_avg_2$y[6] <- agents_16_thresholdD_20_A80_utils_avg$y[6]
agents_16_thresholdD_20_A80_utils_avg_2$y[7] <- agents_16_thresholdD_20_A80_utils_avg$y[7]
agents_16_thresholdD_20_A80_utils_avg_2$y[8] <- agents_16_thresholdD_20_A80_utils_avg$y[8]
agents_16_thresholdD_20_A80_utils_avg_2$y[9] <- agents_16_thresholdD_20_A80_utils_avg$y[9]
agents_16_thresholdD_20_A80_utils_avg_2$y[10] <- agents_16_thresholdD_20_A80_utils_avg$y[10]
agents_16_thresholdD_20_A80_utils_avg_2$y[11] <- agents_16_thresholdD_20_A80_utils_avg$y[11]
agents_16_thresholdD_20_A80_utils_avg_2$y[12] <- agents_16_thresholdD_20_A80_utils_avg$y[12]
agents_16_thresholdD_20_A80_utils_avg_2$y[13] <- agents_16_thresholdD_20_A80_utils_avg$y[13]
agents_16_thresholdD_20_A80_utils_avg_2$y[14] <- agents_16_thresholdD_20_A80_utils_avg$y[14]
agents_16_thresholdD_20_A80_utils_avg_2$y[15] <- agents_16_thresholdD_20_A80_utils_avg$y[15]
agents_16_thresholdD_20_A80_utils_avg_2$y[16] <- agents_16_thresholdD_20_A80_utils_avg$y[16]


agents_16_threshold_D20_avg <- data.frame(rbind(agents_16_thresholdD_20_A60_utils_avg_2, agents_16_thresholdD_20_A70_utils_avg_2,agents_16_thresholdD_20_A80_utils_avg_2))
agents_16_threshold_D20_avg_result <- agents_16_threshold_D20_avg[FALSE,]
agents_16_threshold_D20_avg_result <- data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 1))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 2)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 3)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 4)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 5)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 6)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 7)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 8)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 9)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 10)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 11)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 12)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 13)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 14)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 15)))))
agents_16_threshold_D20_avg_result <- rbind(agents_16_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D20_avg, x == 16)))))

agents_16_threshold_D20_avg_result

#disapprove 30
agents_16_thresholdD_30 <- agents_16[agents_16[,grep(30,threshold_disapprove)],]
agents_16_thresholdD_30_D20 <- agents_16_thresholdD_30[agents_16_thresholdD_30[,grep(60,threshold_approve)],]
agents_16_thresholdD_30_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdD_30_D20$util_agent_splitted))), nrow=length(agents_16_thresholdD_30_D20$util_agent_splitted), byrow=T))
agents_16_thresholdD_30_A60_utils_avg <- colMeans(agents_16_thresholdD_30_A60_utils)
agents_16_thresholdD_30_A60_utils_avg <- data.frame(y = agents_16_thresholdD_30_A60_utils_avg)
agents_16_thresholdD_30_A60_utils_avg$x <- seq.int(16)
agents_16_thresholdD_30_A60_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdD_30_A60_utils_avg_2$y[1] <- agents_16_thresholdD_30_A60_utils_avg$y[1]
agents_16_thresholdD_30_A60_utils_avg_2$y[2] <- agents_16_thresholdD_30_A60_utils_avg$y[2]
agents_16_thresholdD_30_A60_utils_avg_2$y[3] <- agents_16_thresholdD_30_A60_utils_avg$y[3]
agents_16_thresholdD_30_A60_utils_avg_2$y[4] <- agents_16_thresholdD_30_A60_utils_avg$y[4]
agents_16_thresholdD_30_A60_utils_avg_2$y[5] <- agents_16_thresholdD_30_A60_utils_avg$y[5]
agents_16_thresholdD_30_A60_utils_avg_2$y[6] <- agents_16_thresholdD_30_A60_utils_avg$y[6]
agents_16_thresholdD_30_A60_utils_avg_2$y[7] <- agents_16_thresholdD_30_A60_utils_avg$y[7]
agents_16_thresholdD_30_A60_utils_avg_2$y[8] <- agents_16_thresholdD_30_A60_utils_avg$y[8]
agents_16_thresholdD_30_A60_utils_avg_2$y[9] <- agents_16_thresholdD_30_A60_utils_avg$y[9]
agents_16_thresholdD_30_A60_utils_avg_2$y[10] <- agents_16_thresholdD_30_A60_utils_avg$y[10]
agents_16_thresholdD_30_A60_utils_avg_2$y[11] <- agents_16_thresholdD_30_A60_utils_avg$y[11]
agents_16_thresholdD_30_A60_utils_avg_2$y[12] <- agents_16_thresholdD_30_A60_utils_avg$y[12]
agents_16_thresholdD_30_A60_utils_avg_2$y[13] <- agents_16_thresholdD_30_A60_utils_avg$y[13]
agents_16_thresholdD_30_A60_utils_avg_2$y[14] <- agents_16_thresholdD_30_A60_utils_avg$y[14]
agents_16_thresholdD_30_A60_utils_avg_2$y[15] <- agents_16_thresholdD_30_A60_utils_avg$y[15]
agents_16_thresholdD_30_A60_utils_avg_2$y[16] <- agents_16_thresholdD_30_A60_utils_avg$y[16]


agents_16_thresholdD_30_A70 <- agents_16_thresholdD_30[agents_16_thresholdD_30[,grep(70,threshold_approve)],]
agents_16_thresholdD_30_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdD_30_A70$util_agent_splitted))), nrow=length(agents_16_thresholdD_30_A70$util_agent_splitted), byrow=T))
agents_16_thresholdD_30_A70_utils_avg <- colMeans(agents_16_thresholdD_30_A70_utils)
agents_16_thresholdD_30_A70_utils_avg <- data.frame(y = agents_16_thresholdD_30_A70_utils_avg)
agents_16_thresholdD_30_A70_utils_avg$x <- seq.int(16)
agents_16_thresholdD_30_A70_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdD_30_A70_utils_avg_2$y[1] <- agents_16_thresholdD_30_A70_utils_avg$y[1]
agents_16_thresholdD_30_A70_utils_avg_2$y[2] <- agents_16_thresholdD_30_A70_utils_avg$y[2]
agents_16_thresholdD_30_A70_utils_avg_2$y[3] <- agents_16_thresholdD_30_A70_utils_avg$y[3]
agents_16_thresholdD_30_A70_utils_avg_2$y[4] <- agents_16_thresholdD_30_A70_utils_avg$y[4]
agents_16_thresholdD_30_A70_utils_avg_2$y[5] <- agents_16_thresholdD_30_A70_utils_avg$y[5]
agents_16_thresholdD_30_A70_utils_avg_2$y[6] <- agents_16_thresholdD_30_A70_utils_avg$y[6]
agents_16_thresholdD_30_A70_utils_avg_2$y[7] <- agents_16_thresholdD_30_A70_utils_avg$y[7]
agents_16_thresholdD_30_A70_utils_avg_2$y[8] <- agents_16_thresholdD_30_A70_utils_avg$y[8]
agents_16_thresholdD_30_A70_utils_avg_2$y[9] <- agents_16_thresholdD_30_A70_utils_avg$y[9]
agents_16_thresholdD_30_A70_utils_avg_2$y[10] <- agents_16_thresholdD_30_A70_utils_avg$y[10]
agents_16_thresholdD_30_A70_utils_avg_2$y[11] <- agents_16_thresholdD_30_A70_utils_avg$y[11]
agents_16_thresholdD_30_A70_utils_avg_2$y[12] <- agents_16_thresholdD_30_A70_utils_avg$y[12]
agents_16_thresholdD_30_A70_utils_avg_2$y[13] <- agents_16_thresholdD_30_A70_utils_avg$y[13]
agents_16_thresholdD_30_A70_utils_avg_2$y[14] <- agents_16_thresholdD_30_A70_utils_avg$y[14]
agents_16_thresholdD_30_A70_utils_avg_2$y[15] <- agents_16_thresholdD_30_A70_utils_avg$y[15]
agents_16_thresholdD_30_A70_utils_avg_2$y[16] <- agents_16_thresholdD_30_A70_utils_avg$y[16]



agents_16_thresholdD_30_A80 <- agents_16_thresholdD_30[agents_16_thresholdD_30[,grep(80,threshold_approve)],]
agents_16_thresholdD_30_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdD_30_A80$util_agent_splitted))), nrow=length(agents_16_thresholdD_30_A80$util_agent_splitted), byrow=T))
agents_16_thresholdD_30_A80_utils_avg <- colMeans(agents_16_thresholdD_30_A80_utils)
agents_16_thresholdD_30_A80_utils_avg <- data.frame(y = agents_16_thresholdD_30_A80_utils_avg)
agents_16_thresholdD_30_A80_utils_avg$x <- seq.int(16)
agents_16_thresholdD_30_A80_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdD_30_A80_utils_avg_2$y[1] <- agents_16_thresholdD_30_A80_utils_avg$y[1]
agents_16_thresholdD_30_A80_utils_avg_2$y[2] <- agents_16_thresholdD_30_A80_utils_avg$y[2]
agents_16_thresholdD_30_A80_utils_avg_2$y[3] <- agents_16_thresholdD_30_A80_utils_avg$y[3]
agents_16_thresholdD_30_A80_utils_avg_2$y[4] <- agents_16_thresholdD_30_A80_utils_avg$y[4]
agents_16_thresholdD_30_A80_utils_avg_2$y[5] <- agents_16_thresholdD_30_A80_utils_avg$y[5]
agents_16_thresholdD_30_A80_utils_avg_2$y[6] <- agents_16_thresholdD_30_A80_utils_avg$y[6]
agents_16_thresholdD_30_A80_utils_avg_2$y[7] <- agents_16_thresholdD_30_A80_utils_avg$y[7]
agents_16_thresholdD_30_A80_utils_avg_2$y[8] <- agents_16_thresholdD_30_A80_utils_avg$y[8]
agents_16_thresholdD_30_A80_utils_avg_2$y[9] <- agents_16_thresholdD_30_A80_utils_avg$y[9]
agents_16_thresholdD_30_A80_utils_avg_2$y[10] <- agents_16_thresholdD_30_A80_utils_avg$y[10]
agents_16_thresholdD_30_A80_utils_avg_2$y[11] <- agents_16_thresholdD_30_A80_utils_avg$y[11]
agents_16_thresholdD_30_A80_utils_avg_2$y[12] <- agents_16_thresholdD_30_A80_utils_avg$y[12]
agents_16_thresholdD_30_A80_utils_avg_2$y[13] <- agents_16_thresholdD_30_A80_utils_avg$y[13]
agents_16_thresholdD_30_A80_utils_avg_2$y[14] <- agents_16_thresholdD_30_A80_utils_avg$y[14]
agents_16_thresholdD_30_A80_utils_avg_2$y[15] <- agents_16_thresholdD_30_A80_utils_avg$y[15]
agents_16_thresholdD_30_A80_utils_avg_2$y[16] <- agents_16_thresholdD_30_A80_utils_avg$y[16]


agents_16_threshold_D30_avg <- data.frame(rbind(agents_16_thresholdD_30_A60_utils_avg_2, agents_16_thresholdD_30_A70_utils_avg_2,agents_16_thresholdD_30_A80_utils_avg_2))
agents_16_threshold_D30_avg_result <- agents_16_threshold_D30_avg[FALSE,]
agents_16_threshold_D30_avg_result <- data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 1))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 2)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 3)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 4)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 5)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 6)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 7)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 8)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 9)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 10)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 11)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 12)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 13)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 14)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 15)))))
agents_16_threshold_D30_avg_result <- rbind(agents_16_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D30_avg, x == 15)))))

agents_16_threshold_D30_avg_result

#disapprove 40
agents_16_thresholdD_40 <- agents_16[agents_16[,grep(40,threshold_disapprove)],]
agents_16_thresholdD_40_D20 <- agents_16_thresholdD_40[agents_16_thresholdD_40[,grep(60,threshold_approve)],]
agents_16_thresholdD_40_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdD_40_D20$util_agent_splitted))), nrow=length(agents_16_thresholdD_40_D20$util_agent_splitted), byrow=T))
agents_16_thresholdD_40_A60_utils_avg <- colMeans(agents_16_thresholdD_40_A60_utils)
agents_16_thresholdD_40_A60_utils_avg <- data.frame(y = agents_16_thresholdD_40_A60_utils_avg)
agents_16_thresholdD_40_A60_utils_avg$x <- seq.int(16)
agents_16_thresholdD_40_A60_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdD_40_A60_utils_avg_2$y[1] <- agents_16_thresholdD_40_A60_utils_avg$y[1]
agents_16_thresholdD_40_A60_utils_avg_2$y[2] <- agents_16_thresholdD_40_A60_utils_avg$y[2]
agents_16_thresholdD_40_A60_utils_avg_2$y[3] <- agents_16_thresholdD_40_A60_utils_avg$y[3]
agents_16_thresholdD_40_A60_utils_avg_2$y[4] <- agents_16_thresholdD_40_A60_utils_avg$y[4]
agents_16_thresholdD_40_A60_utils_avg_2$y[5] <- agents_16_thresholdD_40_A60_utils_avg$y[5]
agents_16_thresholdD_40_A60_utils_avg_2$y[6] <- agents_16_thresholdD_40_A60_utils_avg$y[6]
agents_16_thresholdD_40_A60_utils_avg_2$y[7] <- agents_16_thresholdD_40_A60_utils_avg$y[7]
agents_16_thresholdD_40_A60_utils_avg_2$y[8] <- agents_16_thresholdD_40_A60_utils_avg$y[8]
agents_16_thresholdD_40_A60_utils_avg_2$y[9] <- agents_16_thresholdD_40_A60_utils_avg$y[9]
agents_16_thresholdD_40_A60_utils_avg_2$y[10] <- agents_16_thresholdD_40_A60_utils_avg$y[10]
agents_16_thresholdD_40_A60_utils_avg_2$y[11] <- agents_16_thresholdD_40_A60_utils_avg$y[11]
agents_16_thresholdD_40_A60_utils_avg_2$y[12] <- agents_16_thresholdD_40_A60_utils_avg$y[12]
agents_16_thresholdD_40_A60_utils_avg_2$y[13] <- agents_16_thresholdD_40_A60_utils_avg$y[13]
agents_16_thresholdD_40_A60_utils_avg_2$y[14] <- agents_16_thresholdD_40_A60_utils_avg$y[14]
agents_16_thresholdD_40_A60_utils_avg_2$y[15] <- agents_16_thresholdD_40_A60_utils_avg$y[15]
agents_16_thresholdD_40_A60_utils_avg_2$y[16] <- agents_16_thresholdD_40_A60_utils_avg$y[16]


agents_16_thresholdD_40_A70 <- agents_16_thresholdD_40[agents_16_thresholdD_40[,grep(70,threshold_approve)],]
agents_16_thresholdD_40_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdD_40_A70$util_agent_splitted))), nrow=length(agents_16_thresholdD_40_A70$util_agent_splitted), byrow=T))
agents_16_thresholdD_40_A70_utils_avg <- colMeans(agents_16_thresholdD_40_A70_utils)
agents_16_thresholdD_40_A70_utils_avg <- data.frame(y = agents_16_thresholdD_40_A70_utils_avg)
agents_16_thresholdD_40_A70_utils_avg$x <- seq.int(16)
agents_16_thresholdD_40_A70_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdD_40_A70_utils_avg_2$y[1] <- agents_16_thresholdD_40_A70_utils_avg$y[1]
agents_16_thresholdD_40_A70_utils_avg_2$y[2] <- agents_16_thresholdD_40_A70_utils_avg$y[2]
agents_16_thresholdD_40_A70_utils_avg_2$y[3] <- agents_16_thresholdD_40_A70_utils_avg$y[3]
agents_16_thresholdD_40_A70_utils_avg_2$y[4] <- agents_16_thresholdD_40_A70_utils_avg$y[4]
agents_16_thresholdD_40_A70_utils_avg_2$y[5] <- agents_16_thresholdD_40_A70_utils_avg$y[5]
agents_16_thresholdD_40_A70_utils_avg_2$y[6] <- agents_16_thresholdD_40_A70_utils_avg$y[6]
agents_16_thresholdD_40_A70_utils_avg_2$y[7] <- agents_16_thresholdD_40_A70_utils_avg$y[7]
agents_16_thresholdD_40_A70_utils_avg_2$y[8] <- agents_16_thresholdD_40_A70_utils_avg$y[8]
agents_16_thresholdD_40_A70_utils_avg_2$y[9] <- agents_16_thresholdD_40_A70_utils_avg$y[9]
agents_16_thresholdD_40_A70_utils_avg_2$y[10] <- agents_16_thresholdD_40_A70_utils_avg$y[10]
agents_16_thresholdD_40_A70_utils_avg_2$y[11] <- agents_16_thresholdD_40_A70_utils_avg$y[11]
agents_16_thresholdD_40_A70_utils_avg_2$y[12] <- agents_16_thresholdD_40_A70_utils_avg$y[12]
agents_16_thresholdD_40_A70_utils_avg_2$y[13] <- agents_16_thresholdD_40_A70_utils_avg$y[13]
agents_16_thresholdD_40_A70_utils_avg_2$y[14] <- agents_16_thresholdD_40_A70_utils_avg$y[14]
agents_16_thresholdD_40_A70_utils_avg_2$y[15] <- agents_16_thresholdD_40_A70_utils_avg$y[15]
agents_16_thresholdD_40_A70_utils_avg_2$y[16] <- agents_16_thresholdD_40_A70_utils_avg$y[16]



agents_16_thresholdD_40_A80 <- agents_16_thresholdD_40[agents_16_thresholdD_40[,grep(80,threshold_approve)],]
agents_16_thresholdD_40_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_16_thresholdD_40_A80$util_agent_splitted))), nrow=length(agents_16_thresholdD_40_A80$util_agent_splitted), byrow=T))
agents_16_thresholdD_40_A80_utils_avg <- colMeans(agents_16_thresholdD_40_A80_utils)
agents_16_thresholdD_40_A80_utils_avg <- data.frame(y = agents_16_thresholdD_40_A80_utils_avg)
agents_16_thresholdD_40_A80_utils_avg$x <- seq.int(16)
agents_16_thresholdD_40_A80_utils_avg_2 <- agents_16_all_utils_avg
agents_16_thresholdD_40_A80_utils_avg_2$y[1] <- agents_16_thresholdD_40_A80_utils_avg$y[1]
agents_16_thresholdD_40_A80_utils_avg_2$y[2] <- agents_16_thresholdD_40_A80_utils_avg$y[2]
agents_16_thresholdD_40_A80_utils_avg_2$y[3] <- agents_16_thresholdD_40_A80_utils_avg$y[3]
agents_16_thresholdD_40_A80_utils_avg_2$y[4] <- agents_16_thresholdD_40_A80_utils_avg$y[4]
agents_16_thresholdD_40_A80_utils_avg_2$y[5] <- agents_16_thresholdD_40_A80_utils_avg$y[5]
agents_16_thresholdD_40_A80_utils_avg_2$y[6] <- agents_16_thresholdD_40_A80_utils_avg$y[6]
agents_16_thresholdD_40_A80_utils_avg_2$y[7] <- agents_16_thresholdD_40_A80_utils_avg$y[7]
agents_16_thresholdD_40_A80_utils_avg_2$y[8] <- agents_16_thresholdD_40_A80_utils_avg$y[8]
agents_16_thresholdD_40_A80_utils_avg_2$y[9] <- agents_16_thresholdD_40_A80_utils_avg$y[9]
agents_16_thresholdD_40_A80_utils_avg_2$y[10] <- agents_16_thresholdD_40_A80_utils_avg$y[10]
agents_16_thresholdD_40_A80_utils_avg_2$y[11] <- agents_16_thresholdD_40_A80_utils_avg$y[11]
agents_16_thresholdD_40_A80_utils_avg_2$y[12] <- agents_16_thresholdD_40_A80_utils_avg$y[12]
agents_16_thresholdD_40_A80_utils_avg_2$y[13] <- agents_16_thresholdD_40_A80_utils_avg$y[13]
agents_16_thresholdD_40_A80_utils_avg_2$y[14] <- agents_16_thresholdD_40_A80_utils_avg$y[14]
agents_16_thresholdD_40_A80_utils_avg_2$y[15] <- agents_16_thresholdD_40_A80_utils_avg$y[15]
agents_16_thresholdD_40_A80_utils_avg_2$y[16] <- agents_16_thresholdD_40_A80_utils_avg$y[16]


agents_16_threshold_D40_avg <- data.frame(rbind(agents_16_thresholdD_40_A60_utils_avg_2, agents_16_thresholdD_40_A70_utils_avg_2,agents_16_thresholdD_40_A80_utils_avg_2))
agents_16_threshold_D40_avg_result <- agents_16_threshold_D40_avg[FALSE,]
agents_16_threshold_D40_avg_result <- data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 1))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 2)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 3)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 4)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 5)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 6)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 7)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 8)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 9)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 10)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 11)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 12)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 13)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 14)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 15)))))
agents_16_threshold_D40_avg_result <- rbind(agents_16_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_16_threshold_D40_avg, x == 16)))))

agents_16_threshold_D40_avg_result

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
ggplot(data=agents_16_all_utils, aes(x=x, y=y, group=row, scale_size(guide = "none"))) +
  geom_line(linetype="solid", color="black", alpha=0.05) +
  #xlim(1, 9) +
  ylim(0.55, 0.7) + labs(x = "position in the game", y="utily") +
  #geom_line(data=agents_16_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) +
  geom_line(data=agents_16_threshold_A60_avg_result,aes(x=x,y=y),color="#00de3b",size=1,alpha=0.8) +
  geom_line(data=agents_16_threshold_A70_avg_result,aes(x=x,y=y),color="#00b831",size=1,alpha=0.8) +
  geom_line(data=agents_16_threshold_A80_avg_result,aes(x=x,y=y),color="#008223",size=1,alpha=0.8) +
  geom_line(data=agents_16_threshold_D20_avg_result,aes(x=x,y=y),color="#e33900",size=1,alpha=0.8) +
  geom_line(data=agents_16_threshold_D30_avg_result,aes(x=x,y=y),color="#ad2c00",size=1,alpha=0.8) +
  geom_line(data=agents_16_threshold_D40_avg_result,aes(x=x,y=y),color="#701d00",size=1,alpha=0.8) +
  scale_x_continuous(breaks = int_breaks) +
  #geom_line(data=agents_16_thresholdA_60_D20_utils_avg_2,aes(x=x,y=y),color="#0077ff",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_60_D30_utils_avg_2,aes(x=x,y=y),color="#0053b3",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_60_D40_utils_avg_2,aes(x=x,y=y),color="#3b96ff",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_70_D20_utils_avg_2,aes(x=x,y=y),color="#00ff00",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_70_D30_utils_avg_2,aes(x=x,y=y),color="#00a800",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_70_D40_utils_avg_2,aes(x=x,y=y),color="#80ff80",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_80_D20_utils_avg_2,aes(x=x,y=y),color="#ff0000",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_80_D30_utils_avg_2,aes(x=x,y=y),color="#8c0000",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_80_D40_utils_avg_2,aes(x=x,y=y),color="#ff6969",size=1,alpha=0.8) +
  theme(legend.position="right")


