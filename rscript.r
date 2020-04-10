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
agents_12 <- data2[data2[,grep(12,num_agents)],]
agents_16 <- data2[data2[,grep(16,num_agents)],]


###########################################################################
###########################################################################
###                                                                     ###
###                                 AGENTS 3                            ###
###                                                                     ###
###########################################################################
###########################################################################
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
agents_3
agents_3[agents_3[,grep(60,threshold_approve)],]
agents_3_thresholdA_60 <- agents_3[agents_3[,grep(60,threshold_approve)],]
agents_3_thresholdA_60_D20 <- agents_3_thresholdA_60[agents_3_thresholdA_60[,grep(20,threshold_disapprove)],]
agents_3_thresholdA_60_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_60_D20$util_agent_splitted))), nrow=length(agents_3_thresholdA_60_D20$util_agent_splitted), byrow=T))
agents_3_thresholdA_60_D20_utils_avg <- colMeans(agents_3_thresholdA_60_D20_utils)
agents_3_thresholdA_60_D20_utils_avg <- data.frame(y = agents_3_thresholdA_60_D20_utils_avg)
agents_3_thresholdA_60_D20_utils_avg$x <- seq.int(3)
agents_3_thresholdA_60_D20_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_60_D20_utils_avg_2$y[1] <- agents_3_thresholdA_60_D20_utils_avg$y[1]
agents_3_thresholdA_60_D20_utils_avg_2$y[2] <- agents_3_thresholdA_60_D20_utils_avg$y[2]
agents_3_thresholdA_60_D20_utils_avg_2$y[3] <- agents_3_thresholdA_60_D20_utils_avg$y[3]

agents_3_thresholdA_60_D30 <- agents_3_thresholdA_60[agents_3_thresholdA_60[,grep(30,threshold_disapprove)],]
agents_3_thresholdA_60_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_60_D30$util_agent_splitted))), nrow=length(agents_3_thresholdA_60_D30$util_agent_splitted), byrow=T))
agents_3_thresholdA_60_D30_utils_avg <- colMeans(agents_3_thresholdA_60_D30_utils)
agents_3_thresholdA_60_D30_utils_avg <- data.frame(y = agents_3_thresholdA_60_D30_utils_avg)
agents_3_thresholdA_60_D30_utils_avg$x <- seq.int(3)
agents_3_thresholdA_60_D30_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_60_D30_utils_avg_2$y[1] <- agents_3_thresholdA_60_D30_utils_avg$y[1]
agents_3_thresholdA_60_D30_utils_avg_2$y[2] <- agents_3_thresholdA_60_D30_utils_avg$y[2]
agents_3_thresholdA_60_D30_utils_avg_2$y[3] <- agents_3_thresholdA_60_D30_utils_avg$y[3]

agents_3_thresholdA_60_D40 <- agents_3_thresholdA_60[agents_3_thresholdA_60[,grep(40,threshold_disapprove)],]
agents_3_thresholdA_60_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_60_D40$util_agent_splitted))), nrow=length(agents_3_thresholdA_60_D40$util_agent_splitted), byrow=T))
agents_3_thresholdA_60_D40_utils_avg <- colMeans(agents_3_thresholdA_60_D40_utils)
agents_3_thresholdA_60_D40_utils_avg <- data.frame(y = agents_3_thresholdA_60_D40_utils_avg)
agents_3_thresholdA_60_D40_utils_avg$x <- seq.int(3)
agents_3_thresholdA_60_D40_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_60_D40_utils_avg_2$y[1] <- agents_3_thresholdA_60_D40_utils_avg$y[1]
agents_3_thresholdA_60_D40_utils_avg_2$y[2] <- agents_3_thresholdA_60_D40_utils_avg$y[2]
agents_3_thresholdA_60_D40_utils_avg_2$y[3] <- agents_3_thresholdA_60_D40_utils_avg$y[3]


agents_3_threshold_A60_avg <- data.frame(rbind(agents_3_thresholdA_60_D20_utils_avg_2, agents_3_thresholdA_60_D30_utils_avg_2,agents_3_thresholdA_60_D40_utils_avg_2))
agents_3_threshold_A60_avg_result <- agents_3_threshold_A60_avg[FALSE,]
agents_3_threshold_A60_avg_result <- data.frame(t(colMeans(subset(agents_3_threshold_A60_avg, x == 1))))
agents_3_threshold_A60_avg_result <- rbind(agents_3_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_A60_avg, x == 2)))))
agents_3_threshold_A60_avg_result <- rbind(agents_3_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_A60_avg, x == 3)))))

agents_3_threshold_A60_avg_result

#APPROVE 70
agents_3_thresholdA_70 <- agents_3[agents_3[,grep(70,threshold_approve)],]
agents_3_thresholdA_70_D20 <- agents_3_thresholdA_70[agents_3_thresholdA_70[,grep(20,threshold_disapprove)],]
agents_3_thresholdA_70_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_70_D20$util_agent_splitted))), nrow=length(agents_3_thresholdA_70_D20$util_agent_splitted), byrow=T))
agents_3_thresholdA_70_D20_utils_avg <- colMeans(agents_3_thresholdA_70_D20_utils)
agents_3_thresholdA_70_D20_utils_avg <- data.frame(y = agents_3_thresholdA_70_D20_utils_avg)
agents_3_thresholdA_70_D20_utils_avg$x <- seq.int(3)
agents_3_thresholdA_70_D20_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_70_D20_utils_avg_2$y[1] <- agents_3_thresholdA_70_D20_utils_avg$y[1]
agents_3_thresholdA_70_D20_utils_avg_2$y[2] <- agents_3_thresholdA_70_D20_utils_avg$y[2]
agents_3_thresholdA_70_D20_utils_avg_2$y[3] <- agents_3_thresholdA_70_D20_utils_avg$y[3]


agents_3_thresholdA_70_D30 <- agents_3_thresholdA_70[agents_3_thresholdA_70[,grep(30,threshold_disapprove)],]
agents_3_thresholdA_70_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_70_D30$util_agent_splitted))), nrow=length(agents_3_thresholdA_70_D30$util_agent_splitted), byrow=T))
agents_3_thresholdA_70_D30_utils_avg <- colMeans(agents_3_thresholdA_70_D30_utils)
agents_3_thresholdA_70_D30_utils_avg <- data.frame(y = agents_3_thresholdA_70_D30_utils_avg)
agents_3_thresholdA_70_D30_utils_avg$x <- seq.int(3)
agents_3_thresholdA_70_D30_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_70_D30_utils_avg_2$y[1] <- agents_3_thresholdA_70_D30_utils_avg$y[1]
agents_3_thresholdA_70_D30_utils_avg_2$y[2] <- agents_3_thresholdA_70_D30_utils_avg$y[2]
agents_3_thresholdA_70_D30_utils_avg_2$y[3] <- agents_3_thresholdA_70_D30_utils_avg$y[3]


agents_3_thresholdA_70_D40 <- agents_3_thresholdA_70[agents_3_thresholdA_70[,grep(40,threshold_disapprove)],]
agents_3_thresholdA_70_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_70_D40$util_agent_splitted))), nrow=length(agents_3_thresholdA_70_D40$util_agent_splitted), byrow=T))
agents_3_thresholdA_70_D40_utils_avg <- colMeans(agents_3_thresholdA_70_D40_utils)
agents_3_thresholdA_70_D40_utils_avg <- data.frame(y = agents_3_thresholdA_70_D40_utils_avg)
agents_3_thresholdA_70_D40_utils_avg$x <- seq.int(3)
agents_3_thresholdA_70_D40_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_70_D40_utils_avg_2$y[1] <- agents_3_thresholdA_70_D40_utils_avg$y[1]
agents_3_thresholdA_70_D40_utils_avg_2$y[2] <- agents_3_thresholdA_70_D40_utils_avg$y[2]
agents_3_thresholdA_70_D40_utils_avg_2$y[3] <- agents_3_thresholdA_70_D40_utils_avg$y[3]


agents_3_threshold_A70_avg <- data.frame(rbind(agents_3_thresholdA_70_D20_utils_avg_2, agents_3_thresholdA_70_D30_utils_avg_2,agents_3_thresholdA_70_D40_utils_avg_2))
agents_3_threshold_A70_avg_result <- agents_3_threshold_A70_avg[FALSE,]
agents_3_threshold_A70_avg_result <- data.frame(t(colMeans(subset(agents_3_threshold_A70_avg, x == 1))))
agents_3_threshold_A70_avg_result <- rbind(agents_3_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_A70_avg, x == 2)))))
agents_3_threshold_A70_avg_result <- rbind(agents_3_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_A70_avg, x == 3)))))

agents_3_threshold_A70_avg_result

#APPROVE 80
agents_3_thresholdA_80 <- agents_3[agents_3[,grep(80,threshold_approve)],]
agents_3_thresholdA_80_D20 <- agents_3_thresholdA_80[agents_3_thresholdA_80[,grep(20,threshold_disapprove)],]
agents_3_thresholdA_80_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_80_D20$util_agent_splitted))), nrow=length(agents_3_thresholdA_80_D20$util_agent_splitted), byrow=T))
agents_3_thresholdA_80_D20_utils_avg <- colMeans(agents_3_thresholdA_80_D20_utils)
agents_3_thresholdA_80_D20_utils_avg <- data.frame(y = agents_3_thresholdA_80_D20_utils_avg)
agents_3_thresholdA_80_D20_utils_avg$x <- seq.int(3)
agents_3_thresholdA_80_D20_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_80_D20_utils_avg_2$y[1] <- agents_3_thresholdA_80_D20_utils_avg$y[1]
agents_3_thresholdA_80_D20_utils_avg_2$y[2] <- agents_3_thresholdA_80_D20_utils_avg$y[2]
agents_3_thresholdA_80_D20_utils_avg_2$y[3] <- agents_3_thresholdA_80_D20_utils_avg$y[3]

agents_3_thresholdA_80_D30 <- agents_3_thresholdA_80[agents_3_thresholdA_80[,grep(30,threshold_disapprove)],]
agents_3_thresholdA_80_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_80_D30$util_agent_splitted))), nrow=length(agents_3_thresholdA_80_D30$util_agent_splitted), byrow=T))
agents_3_thresholdA_80_D30_utils_avg <- colMeans(agents_3_thresholdA_80_D30_utils)
agents_3_thresholdA_80_D30_utils_avg <- data.frame(y = agents_3_thresholdA_80_D30_utils_avg)
agents_3_thresholdA_80_D30_utils_avg$x <- seq.int(3)
agents_3_thresholdA_80_D30_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_80_D30_utils_avg_2$y[1] <- agents_3_thresholdA_80_D30_utils_avg$y[1]
agents_3_thresholdA_80_D30_utils_avg_2$y[2] <- agents_3_thresholdA_80_D30_utils_avg$y[2]
agents_3_thresholdA_80_D30_utils_avg_2$y[3] <- agents_3_thresholdA_80_D30_utils_avg$y[3]


agents_3_thresholdA_80_D40 <- agents_3_thresholdA_80[agents_3_thresholdA_80[,grep(40,threshold_disapprove)],]
agents_3_thresholdA_80_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdA_80_D40$util_agent_splitted))), nrow=length(agents_3_thresholdA_80_D40$util_agent_splitted), byrow=T))
agents_3_thresholdA_80_D40_utils_avg <- colMeans(agents_3_thresholdA_80_D40_utils)
agents_3_thresholdA_80_D40_utils_avg <- data.frame(y = agents_3_thresholdA_80_D40_utils_avg)
agents_3_thresholdA_80_D40_utils_avg$x <- seq.int(3)
agents_3_thresholdA_80_D40_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdA_80_D40_utils_avg_2$y[1] <- agents_3_thresholdA_80_D40_utils_avg$y[1]
agents_3_thresholdA_80_D40_utils_avg_2$y[2] <- agents_3_thresholdA_80_D40_utils_avg$y[2]
agents_3_thresholdA_80_D40_utils_avg_2$y[3] <- agents_3_thresholdA_80_D40_utils_avg$y[3]

agents_3_threshold_A80_avg <- data.frame(rbind(agents_3_thresholdA_80_D20_utils_avg_2, agents_3_thresholdA_80_D30_utils_avg_2,agents_3_thresholdA_80_D40_utils_avg_2))
agents_3_threshold_A80_avg_result <- agents_3_threshold_A80_avg[FALSE,]
agents_3_threshold_A80_avg_result <- data.frame(t(colMeans(subset(agents_3_threshold_A80_avg, x == 1))))
agents_3_threshold_A80_avg_result <- rbind(agents_3_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_A80_avg, x == 2)))))
agents_3_threshold_A80_avg_result <- rbind(agents_3_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_A80_avg, x == 3)))))

agents_3_threshold_A80_avg_result

#DISSAPROVE 20
agents_3_thresholdD_20 <- agents_3[agents_3[,grep(20,threshold_disapprove)],]
agents_3_thresholdD_20_A60 <- agents_3_thresholdD_20[agents_3_thresholdD_20[,grep(60,threshold_approve)],]
agents_3_thresholdD_20_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdD_20_A60$util_agent_splitted))), nrow=length(agents_3_thresholdD_20_A60$util_agent_splitted), byrow=T))
agents_3_thresholdD_20_A60_utils_avg <- colMeans(agents_3_thresholdD_20_A60_utils)
agents_3_thresholdD_20_A60_utils_avg <- data.frame(y = agents_3_thresholdD_20_A60_utils_avg)
agents_3_thresholdD_20_A60_utils_avg$x <- seq.int(3)
agents_3_thresholdD_20_A60_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdD_20_A60_utils_avg_2$y[1] <- agents_3_thresholdD_20_A60_utils_avg$y[1]
agents_3_thresholdD_20_A60_utils_avg_2$y[2] <- agents_3_thresholdD_20_A60_utils_avg$y[2]
agents_3_thresholdD_20_A60_utils_avg_2$y[3] <- agents_3_thresholdD_20_A60_utils_avg$y[3]


agents_3_thresholdD_20_A70 <- agents_3_thresholdD_20[agents_3_thresholdD_20[,grep(70,threshold_approve)],]
agents_3_thresholdD_20_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdD_20_A70$util_agent_splitted))), nrow=length(agents_3_thresholdD_20_A70$util_agent_splitted), byrow=T))
agents_3_thresholdD_20_A70_utils_avg <- colMeans(agents_3_thresholdD_20_A70_utils)
agents_3_thresholdD_20_A70_utils_avg <- data.frame(y = agents_3_thresholdD_20_A70_utils_avg)
agents_3_thresholdD_20_A70_utils_avg$x <- seq.int(3)
agents_3_thresholdD_20_A70_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdD_20_A70_utils_avg_2$y[1] <- agents_3_thresholdD_20_A70_utils_avg$y[1]
agents_3_thresholdD_20_A70_utils_avg_2$y[2] <- agents_3_thresholdD_20_A70_utils_avg$y[2]
agents_3_thresholdD_20_A70_utils_avg_2$y[3] <- agents_3_thresholdD_20_A70_utils_avg$y[3]


agents_3_thresholdD_20_A80 <- agents_3_thresholdD_20[agents_3_thresholdD_20[,grep(80,threshold_approve)],]
agents_3_thresholdD_20_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdD_20_A80$util_agent_splitted))), nrow=length(agents_3_thresholdD_20_A80$util_agent_splitted), byrow=T))
agents_3_thresholdD_20_A80_utils_avg <- colMeans(agents_3_thresholdD_20_A80_utils)
agents_3_thresholdD_20_A80_utils_avg <- data.frame(y = agents_3_thresholdD_20_A80_utils_avg)
agents_3_thresholdD_20_A80_utils_avg$x <- seq.int(3)
agents_3_thresholdD_20_A80_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdD_20_A80_utils_avg_2$y[1] <- agents_3_thresholdD_20_A80_utils_avg$y[1]
agents_3_thresholdD_20_A80_utils_avg_2$y[2] <- agents_3_thresholdD_20_A80_utils_avg$y[2]
agents_3_thresholdD_20_A80_utils_avg_2$y[3] <- agents_3_thresholdD_20_A80_utils_avg$y[3]


agents_3_threshold_D20_avg <- data.frame(rbind(agents_3_thresholdD_20_A60_utils_avg_2, agents_3_thresholdD_20_A70_utils_avg_2,agents_3_thresholdD_20_A80_utils_avg_2))
agents_3_threshold_D20_avg_result <- agents_3_threshold_D20_avg[FALSE,]
agents_3_threshold_D20_avg_result <- data.frame(t(colMeans(subset(agents_3_threshold_D20_avg, x == 1))))
agents_3_threshold_D20_avg_result <- rbind(agents_3_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_D20_avg, x == 2)))))
agents_3_threshold_D20_avg_result <- rbind(agents_3_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_D20_avg, x == 3)))))

agents_3_threshold_D20_avg_result

#disapprove 30
agents_3_thresholdD_30 <- agents_3[agents_3[,grep(30,threshold_disapprove)],]
agents_3_thresholdD_30_A60 <- agents_3_thresholdD_30[agents_3_thresholdD_30[,grep(60,threshold_approve)],]
agents_3_thresholdD_30_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdD_30_A60$util_agent_splitted))), nrow=length(agents_3_thresholdD_30_A60$util_agent_splitted), byrow=T))
agents_3_thresholdD_30_A60_utils_avg <- colMeans(agents_3_thresholdD_30_A60_utils)
agents_3_thresholdD_30_A60_utils_avg <- data.frame(y = agents_3_thresholdD_30_A60_utils_avg)
agents_3_thresholdD_30_A60_utils_avg$x <- seq.int(3)
agents_3_thresholdD_30_A60_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdD_30_A60_utils_avg_2$y[1] <- agents_3_thresholdD_30_A60_utils_avg$y[1]
agents_3_thresholdD_30_A60_utils_avg_2$y[2] <- agents_3_thresholdD_30_A60_utils_avg$y[2]
agents_3_thresholdD_30_A60_utils_avg_2$y[3] <- agents_3_thresholdD_30_A60_utils_avg$y[3]


agents_3_thresholdD_30_A70 <- agents_3_thresholdD_30[agents_3_thresholdD_30[,grep(70,threshold_approve)],]
agents_3_thresholdD_30_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdD_30_A70$util_agent_splitted))), nrow=length(agents_3_thresholdD_30_A70$util_agent_splitted), byrow=T))
agents_3_thresholdD_30_A70_utils_avg <- colMeans(agents_3_thresholdD_30_A70_utils)
agents_3_thresholdD_30_A70_utils_avg <- data.frame(y = agents_3_thresholdD_30_A70_utils_avg)
agents_3_thresholdD_30_A70_utils_avg$x <- seq.int(3)
agents_3_thresholdD_30_A70_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdD_30_A70_utils_avg_2$y[1] <- agents_3_thresholdD_30_A70_utils_avg$y[1]
agents_3_thresholdD_30_A70_utils_avg_2$y[2] <- agents_3_thresholdD_30_A70_utils_avg$y[2]
agents_3_thresholdD_30_A70_utils_avg_2$y[3] <- agents_3_thresholdD_30_A70_utils_avg$y[3]



agents_3_thresholdD_30_A80 <- agents_3_thresholdD_30[agents_3_thresholdD_30[,grep(80,threshold_approve)],]
agents_3_thresholdD_30_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdD_30_A80$util_agent_splitted))), nrow=length(agents_3_thresholdD_30_A80$util_agent_splitted), byrow=T))
agents_3_thresholdD_30_A80_utils_avg <- colMeans(agents_3_thresholdD_30_A80_utils)
agents_3_thresholdD_30_A80_utils_avg <- data.frame(y = agents_3_thresholdD_30_A80_utils_avg)
agents_3_thresholdD_30_A80_utils_avg$x <- seq.int(3)
agents_3_thresholdD_30_A80_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdD_30_A80_utils_avg_2$y[1] <- agents_3_thresholdD_30_A80_utils_avg$y[1]
agents_3_thresholdD_30_A80_utils_avg_2$y[2] <- agents_3_thresholdD_30_A80_utils_avg$y[2]
agents_3_thresholdD_30_A80_utils_avg_2$y[3] <- agents_3_thresholdD_30_A80_utils_avg$y[3]


agents_3_threshold_D30_avg <- data.frame(rbind(agents_3_thresholdD_30_A60_utils_avg_2, agents_3_thresholdD_30_A70_utils_avg_2,agents_3_thresholdD_30_A80_utils_avg_2))
agents_3_threshold_D30_avg_result <- agents_3_threshold_D30_avg[FALSE,]
agents_3_threshold_D30_avg_result <- data.frame(t(colMeans(subset(agents_3_threshold_D30_avg, x == 1))))
agents_3_threshold_D30_avg_result <- rbind(agents_3_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_D30_avg, x == 2)))))
agents_3_threshold_D30_avg_result <- rbind(agents_3_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_D30_avg, x == 3)))))

agents_3_threshold_D30_avg_result

#disapprove 40
agents_3_thresholdD_40 <- agents_3[agents_3[,grep(40,threshold_disapprove)],]
agents_3_thresholdD_40_A60 <- agents_3_thresholdD_40[agents_3_thresholdD_40[,grep(60,threshold_approve)],]
agents_3_thresholdD_40_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdD_40_A60$util_agent_splitted))), nrow=length(agents_3_thresholdD_40_A60$util_agent_splitted), byrow=T))
agents_3_thresholdD_40_A60_utils_avg <- colMeans(agents_3_thresholdD_40_A60_utils)
agents_3_thresholdD_40_A60_utils_avg <- data.frame(y = agents_3_thresholdD_40_A60_utils_avg)
agents_3_thresholdD_40_A60_utils_avg$x <- seq.int(3)
agents_3_thresholdD_40_A60_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdD_40_A60_utils_avg_2$y[1] <- agents_3_thresholdD_40_A60_utils_avg$y[1]
agents_3_thresholdD_40_A60_utils_avg_2$y[2] <- agents_3_thresholdD_40_A60_utils_avg$y[2]
agents_3_thresholdD_40_A60_utils_avg_2$y[3] <- agents_3_thresholdD_40_A60_utils_avg$y[3]


agents_3_thresholdD_40_A70 <- agents_3_thresholdD_40[agents_3_thresholdD_40[,grep(70,threshold_approve)],]
agents_3_thresholdD_40_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdD_40_A70$util_agent_splitted))), nrow=length(agents_3_thresholdD_40_A70$util_agent_splitted), byrow=T))
agents_3_thresholdD_40_A70_utils_avg <- colMeans(agents_3_thresholdD_40_A70_utils)
agents_3_thresholdD_40_A70_utils_avg <- data.frame(y = agents_3_thresholdD_40_A70_utils_avg)
agents_3_thresholdD_40_A70_utils_avg$x <- seq.int(3)
agents_3_thresholdD_40_A70_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdD_40_A70_utils_avg_2$y[1] <- agents_3_thresholdD_40_A70_utils_avg$y[1]
agents_3_thresholdD_40_A70_utils_avg_2$y[2] <- agents_3_thresholdD_40_A70_utils_avg$y[2]
agents_3_thresholdD_40_A70_utils_avg_2$y[3] <- agents_3_thresholdD_40_A70_utils_avg$y[3]



agents_3_thresholdD_40_A80 <- agents_3_thresholdD_40[agents_3_thresholdD_40[,grep(80,threshold_approve)],]
agents_3_thresholdD_40_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_3_thresholdD_40_A80$util_agent_splitted))), nrow=length(agents_3_thresholdD_40_A80$util_agent_splitted), byrow=T))
agents_3_thresholdD_40_A80_utils_avg <- colMeans(agents_3_thresholdD_40_A80_utils)
agents_3_thresholdD_40_A80_utils_avg <- data.frame(y = agents_3_thresholdD_40_A80_utils_avg)
agents_3_thresholdD_40_A80_utils_avg$x <- seq.int(3)
agents_3_thresholdD_40_A80_utils_avg_2 <- agents_3_all_utils_avg
agents_3_thresholdD_40_A80_utils_avg_2$y[1] <- agents_3_thresholdD_40_A80_utils_avg$y[1]
agents_3_thresholdD_40_A80_utils_avg_2$y[2] <- agents_3_thresholdD_40_A80_utils_avg$y[2]
agents_3_thresholdD_40_A80_utils_avg_2$y[3] <- agents_3_thresholdD_40_A80_utils_avg$y[3]


agents_3_threshold_D40_avg <- data.frame(rbind(agents_3_thresholdD_40_A60_utils_avg_2, agents_3_thresholdD_40_A70_utils_avg_2,agents_3_thresholdD_40_A80_utils_avg_2))
agents_3_threshold_D40_avg_result <- agents_3_threshold_D40_avg[FALSE,]
agents_3_threshold_D40_avg_result <- data.frame(t(colMeans(subset(agents_3_threshold_D40_avg, x == 1))))
agents_3_threshold_D40_avg_result <- rbind(agents_3_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_D40_avg, x == 2)))))
agents_3_threshold_D40_avg_result <- rbind(agents_3_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_3_threshold_D40_avg, x == 3)))))

agents_3_threshold_D40_avg_result

integer_breaks <- function(n = 1, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
ggplot(data=agents_3_all_utils, aes(x=x, y=y, group=row, scale_size(guide = "none"))) +
  geom_line(linetype="solid", color="black", alpha=0.05) +
  #xlim(1, 9) +
  ylim(0.55, 0.82) + labs(x = "position in the game", y="utily") +
  scale_x_discrete(name ="position in the game", limits=c("1","2","3")) +
  #geom_line(data=agents_3_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) +
  ggtitle("3 Agents") +
  geom_line(data=agents_3_threshold_A60_avg_result,aes(x=x,y=y),color="#00de3b",size=1,alpha=0.8) +
  geom_line(data=agents_3_threshold_A70_avg_result,aes(x=x,y=y),color="#00b831",size=1,alpha=0.8) +
  geom_line(data=agents_3_threshold_A80_avg_result,aes(x=x,y=y),color="#008223",size=1,alpha=0.8) +
  geom_line(data=agents_3_threshold_D20_avg_result,aes(x=x,y=y),color="#e33900",size=1,alpha=0.8) +
  geom_line(data=agents_3_threshold_D30_avg_result,aes(x=x,y=y),color="#ad2c00",size=1,alpha=0.8) +
  geom_line(data=agents_3_threshold_D40_avg_result,aes(x=x,y=y),color="#701d00",size=1,alpha=0.8)
#scale_x_continuous(breaks = integer_breaks)
#geom_line(data=agents_3_thresholdA_60_D20_utils_avg_2,aes(x=x,y=y),color="#0077ff",size=1,alpha=0.8) +
#geom_line(data=agents_3_thresholdA_60_D30_utils_avg_2,aes(x=x,y=y),color="#0053b3",size=1,alpha=0.8) +
#geom_line(data=agents_3_thresholdA_60_D40_utils_avg_2,aes(x=x,y=y),color="#3b96ff",size=1,alpha=0.8) +
#geom_line(data=agents_3_thresholdA_70_D20_utils_avg_2,aes(x=x,y=y),color="#00ff00",size=1,alpha=0.8) +
#geom_line(data=agents_3_thresholdA_70_D30_utils_avg_2,aes(x=x,y=y),color="#00a800",size=1,alpha=0.8) +
#geom_line(data=agents_3_thresholdA_70_D40_utils_avg_2,aes(x=x,y=y),color="#80ff80",size=1,alpha=0.8) +
#geom_line(data=agents_3_thresholdA_80_D20_utils_avg_2,aes(x=x,y=y),color="#ff0000",size=1,alpha=0.8) +
#geom_line(data=agents_3_thresholdA_80_D30_utils_avg_2,aes(x=x,y=y),color="#8c0000",size=1,alpha=0.8) +
#geom_line(data=agents_3_thresholdA_80_D40_utils_avg_2,aes(x=x,y=y),color="#ff6969",size=1,alpha=0.8) +
#theme(legend.position = c(0, 1),legend.justification = c(0, 1))




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

agents_4_utils <- data.frame(matrix(unlist(agents_4$util_agent_splitted), nrow=length(agents_4$util_agent_splitted), byrow=T))
agents_4_utils_1 <- data.frame(y=as.numeric(paste(agents_4_utils$X1)))
agents_4_utils_2 <- data.frame(y=as.numeric(paste(agents_4_utils$X2)))
agents_4_utils_3 <- data.frame(y=as.numeric(paste(agents_4_utils$X3)))
agents_4_utils_4 <- data.frame(y=as.numeric(paste(agents_4_utils$X4)))
agents_4_utils_1$row <- seq.int(nrow(agents_4_utils_1))
agents_4_utils_2$row <- seq.int(nrow(agents_4_utils_2))
agents_4_utils_3$row <- seq.int(nrow(agents_4_utils_3))
agents_4_utils_4$row <- seq.int(nrow(agents_4_utils_4))
agents_4_utils_1$x <- 1
agents_4_utils_2$x <- 2
agents_4_utils_3$x <- 3
agents_4_utils_4$x <- 4

agents_4_all_utils <- rbind(agents_4_utils_1, agents_4_utils_2, agents_4_utils_3, agents_4_utils_4)

agents_4_utils_1_avg <- colMeans(agents_4_utils_1)
agents_4_utils_2_avg <- colMeans(agents_4_utils_2)
agents_4_utils_3_avg <- colMeans(agents_4_utils_3)
agents_4_utils_4_avg <- colMeans(agents_4_utils_4)

agents_4_all_utils_avg <- data.frame(rbind(agents_4_utils_1_avg, agents_4_utils_2_avg, agents_4_utils_3_avg, agents_4_utils_4_avg))
agents_4_all_utils_avg

#APPROVE 60
agents_4_thresholdA_60 <- agents_4[agents_4[,grep(60,threshold_approve)],]
agents_4_thresholdA_60_D20 <- agents_4_thresholdA_60[agents_4_thresholdA_60[,grep(20,threshold_disapprove)],]
agents_4_thresholdA_60_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdA_60_D20$util_agent_splitted))), nrow=length(agents_4_thresholdA_60_D20$util_agent_splitted), byrow=T))
agents_4_thresholdA_60_D20_utils_avg <- colMeans(agents_4_thresholdA_60_D20_utils)
agents_4_thresholdA_60_D20_utils_avg <- data.frame(y = agents_4_thresholdA_60_D20_utils_avg)
agents_4_thresholdA_60_D20_utils_avg$x <- seq.int(4)
agents_4_thresholdA_60_D20_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdA_60_D20_utils_avg_2$y[1] <- agents_4_thresholdA_60_D20_utils_avg$y[1]
agents_4_thresholdA_60_D20_utils_avg_2$y[2] <- agents_4_thresholdA_60_D20_utils_avg$y[2]
agents_4_thresholdA_60_D20_utils_avg_2$y[3] <- agents_4_thresholdA_60_D20_utils_avg$y[3]
agents_4_thresholdA_60_D20_utils_avg_2$y[4] <- agents_4_thresholdA_60_D20_utils_avg$y[4]

agents_4_thresholdA_60_D30 <- agents_4_thresholdA_60[agents_4_thresholdA_60[,grep(30,threshold_disapprove)],]
agents_4_thresholdA_60_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdA_60_D30$util_agent_splitted))), nrow=length(agents_4_thresholdA_60_D30$util_agent_splitted), byrow=T))
agents_4_thresholdA_60_D30_utils_avg <- colMeans(agents_4_thresholdA_60_D30_utils)
agents_4_thresholdA_60_D30_utils_avg <- data.frame(y = agents_4_thresholdA_60_D30_utils_avg)
agents_4_thresholdA_60_D30_utils_avg$x <- seq.int(4)
agents_4_thresholdA_60_D30_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdA_60_D30_utils_avg_2$y[1] <- agents_4_thresholdA_60_D30_utils_avg$y[1]
agents_4_thresholdA_60_D30_utils_avg_2$y[2] <- agents_4_thresholdA_60_D30_utils_avg$y[2]
agents_4_thresholdA_60_D30_utils_avg_2$y[3] <- agents_4_thresholdA_60_D30_utils_avg$y[3]
agents_4_thresholdA_60_D30_utils_avg_2$y[4] <- agents_4_thresholdA_60_D30_utils_avg$y[4]

agents_4_thresholdA_60_D40 <- agents_4_thresholdA_60[agents_4_thresholdA_60[,grep(40,threshold_disapprove)],]
agents_4_thresholdA_60_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdA_60_D40$util_agent_splitted))), nrow=length(agents_4_thresholdA_60_D40$util_agent_splitted), byrow=T))
agents_4_thresholdA_60_D40_utils_avg <- colMeans(agents_4_thresholdA_60_D40_utils)
agents_4_thresholdA_60_D40_utils_avg <- data.frame(y = agents_4_thresholdA_60_D40_utils_avg)
agents_4_thresholdA_60_D40_utils_avg$x <- seq.int(4)
agents_4_thresholdA_60_D40_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdA_60_D40_utils_avg_2$y[1] <- agents_4_thresholdA_60_D40_utils_avg$y[1]
agents_4_thresholdA_60_D40_utils_avg_2$y[2] <- agents_4_thresholdA_60_D40_utils_avg$y[2]
agents_4_thresholdA_60_D40_utils_avg_2$y[3] <- agents_4_thresholdA_60_D40_utils_avg$y[3]
agents_4_thresholdA_60_D40_utils_avg_2$y[4] <- agents_4_thresholdA_60_D40_utils_avg$y[4]


agents_4_threshold_A60_avg <- data.frame(rbind(agents_4_thresholdA_60_D20_utils_avg_2, agents_4_thresholdA_60_D30_utils_avg_2,agents_4_thresholdA_60_D40_utils_avg_2))
agents_4_threshold_A60_avg_result <- agents_4_threshold_A60_avg[FALSE,]
agents_4_threshold_A60_avg_result <- data.frame(t(colMeans(subset(agents_4_threshold_A60_avg, x == 1))))
agents_4_threshold_A60_avg_result <- rbind(agents_4_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_A60_avg, x == 2)))))
agents_4_threshold_A60_avg_result <- rbind(agents_4_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_A60_avg, x == 3)))))
agents_4_threshold_A60_avg_result <- rbind(agents_4_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_A60_avg, x == 4)))))

agents_4_threshold_A60_avg_result

#APPROVE 70
agents_4_thresholdA_70 <- agents_4[agents_4[,grep(70,threshold_approve)],]
agents_4_thresholdA_70_D20 <- agents_4_thresholdA_70[agents_4_thresholdA_70[,grep(20,threshold_disapprove)],]
agents_4_thresholdA_70_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdA_70_D20$util_agent_splitted))), nrow=length(agents_4_thresholdA_70_D20$util_agent_splitted), byrow=T))
agents_4_thresholdA_70_D20_utils_avg <- colMeans(agents_4_thresholdA_70_D20_utils)
agents_4_thresholdA_70_D20_utils_avg <- data.frame(y = agents_4_thresholdA_70_D20_utils_avg)
agents_4_thresholdA_70_D20_utils_avg$x <- seq.int(4)
agents_4_thresholdA_70_D20_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdA_70_D20_utils_avg_2$y[1] <- agents_4_thresholdA_70_D20_utils_avg$y[1]
agents_4_thresholdA_70_D20_utils_avg_2$y[2] <- agents_4_thresholdA_70_D20_utils_avg$y[2]
agents_4_thresholdA_70_D20_utils_avg_2$y[3] <- agents_4_thresholdA_70_D20_utils_avg$y[3]
agents_4_thresholdA_70_D20_utils_avg_2$y[4] <- agents_4_thresholdA_70_D20_utils_avg$y[4]


agents_4_thresholdA_70_D30 <- agents_4_thresholdA_70[agents_4_thresholdA_70[,grep(30,threshold_disapprove)],]
agents_4_thresholdA_70_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdA_70_D30$util_agent_splitted))), nrow=length(agents_4_thresholdA_70_D30$util_agent_splitted), byrow=T))
agents_4_thresholdA_70_D30_utils_avg <- colMeans(agents_4_thresholdA_70_D30_utils)
agents_4_thresholdA_70_D30_utils_avg <- data.frame(y = agents_4_thresholdA_70_D30_utils_avg)
agents_4_thresholdA_70_D30_utils_avg$x <- seq.int(4)
agents_4_thresholdA_70_D30_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdA_70_D30_utils_avg_2$y[1] <- agents_4_thresholdA_70_D30_utils_avg$y[1]
agents_4_thresholdA_70_D30_utils_avg_2$y[2] <- agents_4_thresholdA_70_D30_utils_avg$y[2]
agents_4_thresholdA_70_D30_utils_avg_2$y[3] <- agents_4_thresholdA_70_D30_utils_avg$y[3]
agents_4_thresholdA_70_D30_utils_avg_2$y[4] <- agents_4_thresholdA_70_D30_utils_avg$y[4]


agents_4_thresholdA_70_D40 <- agents_4_thresholdA_70[agents_4_thresholdA_70[,grep(40,threshold_disapprove)],]
agents_4_thresholdA_70_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdA_70_D40$util_agent_splitted))), nrow=length(agents_4_thresholdA_70_D40$util_agent_splitted), byrow=T))
agents_4_thresholdA_70_D40_utils_avg <- colMeans(agents_4_thresholdA_70_D40_utils)
agents_4_thresholdA_70_D40_utils_avg <- data.frame(y = agents_4_thresholdA_70_D40_utils_avg)
agents_4_thresholdA_70_D40_utils_avg$x <- seq.int(4)
agents_4_thresholdA_70_D40_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdA_70_D40_utils_avg_2$y[1] <- agents_4_thresholdA_70_D40_utils_avg$y[1]
agents_4_thresholdA_70_D40_utils_avg_2$y[2] <- agents_4_thresholdA_70_D40_utils_avg$y[2]
agents_4_thresholdA_70_D40_utils_avg_2$y[3] <- agents_4_thresholdA_70_D40_utils_avg$y[3]
agents_4_thresholdA_70_D40_utils_avg_2$y[4] <- agents_4_thresholdA_70_D40_utils_avg$y[4]


agents_4_threshold_A70_avg <- data.frame(rbind(agents_4_thresholdA_70_D20_utils_avg_2, agents_4_thresholdA_70_D30_utils_avg_2,agents_4_thresholdA_70_D40_utils_avg_2))
agents_4_threshold_A70_avg_result <- agents_4_threshold_A70_avg[FALSE,]
agents_4_threshold_A70_avg_result <- data.frame(t(colMeans(subset(agents_4_threshold_A70_avg, x == 1))))
agents_4_threshold_A70_avg_result <- rbind(agents_4_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_A70_avg, x == 2)))))
agents_4_threshold_A70_avg_result <- rbind(agents_4_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_A70_avg, x == 3)))))
agents_4_threshold_A70_avg_result <- rbind(agents_4_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_A70_avg, x == 4)))))

agents_4_threshold_A70_avg_result

#APPROVE 80
agents_4_thresholdA_80 <- agents_4[agents_4[,grep(80,threshold_approve)],]
agents_4_thresholdA_80_D20 <- agents_4_thresholdA_80[agents_4_thresholdA_80[,grep(20,threshold_disapprove)],]
agents_4_thresholdA_80_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdA_80_D20$util_agent_splitted))), nrow=length(agents_4_thresholdA_80_D20$util_agent_splitted), byrow=T))
agents_4_thresholdA_80_D20_utils_avg <- colMeans(agents_4_thresholdA_80_D20_utils)
agents_4_thresholdA_80_D20_utils_avg <- data.frame(y = agents_4_thresholdA_80_D20_utils_avg)
agents_4_thresholdA_80_D20_utils_avg$x <- seq.int(4)
agents_4_thresholdA_80_D20_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdA_80_D20_utils_avg_2$y[1] <- agents_4_thresholdA_80_D20_utils_avg$y[1]
agents_4_thresholdA_80_D20_utils_avg_2$y[2] <- agents_4_thresholdA_80_D20_utils_avg$y[2]
agents_4_thresholdA_80_D20_utils_avg_2$y[3] <- agents_4_thresholdA_80_D20_utils_avg$y[3]
agents_4_thresholdA_80_D20_utils_avg_2$y[4] <- agents_4_thresholdA_80_D20_utils_avg$y[4]

agents_4_thresholdA_80_D30 <- agents_4_thresholdA_80[agents_4_thresholdA_80[,grep(30,threshold_disapprove)],]
agents_4_thresholdA_80_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdA_80_D30$util_agent_splitted))), nrow=length(agents_4_thresholdA_80_D30$util_agent_splitted), byrow=T))
agents_4_thresholdA_80_D30_utils_avg <- colMeans(agents_4_thresholdA_80_D30_utils)
agents_4_thresholdA_80_D30_utils_avg <- data.frame(y = agents_4_thresholdA_80_D30_utils_avg)
agents_4_thresholdA_80_D30_utils_avg$x <- seq.int(4)
agents_4_thresholdA_80_D30_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdA_80_D30_utils_avg_2$y[1] <- agents_4_thresholdA_80_D30_utils_avg$y[1]
agents_4_thresholdA_80_D30_utils_avg_2$y[2] <- agents_4_thresholdA_80_D30_utils_avg$y[2]
agents_4_thresholdA_80_D30_utils_avg_2$y[3] <- agents_4_thresholdA_80_D30_utils_avg$y[3]
agents_4_thresholdA_80_D30_utils_avg_2$y[4] <- agents_4_thresholdA_80_D30_utils_avg$y[4]


agents_4_thresholdA_80_D40 <- agents_4_thresholdA_80[agents_4_thresholdA_80[,grep(40,threshold_disapprove)],]
agents_4_thresholdA_80_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdA_80_D40$util_agent_splitted))), nrow=length(agents_4_thresholdA_80_D40$util_agent_splitted), byrow=T))
agents_4_thresholdA_80_D40_utils_avg <- colMeans(agents_4_thresholdA_80_D40_utils)
agents_4_thresholdA_80_D40_utils_avg <- data.frame(y = agents_4_thresholdA_80_D40_utils_avg)
agents_4_thresholdA_80_D40_utils_avg$x <- seq.int(4)
agents_4_thresholdA_80_D40_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdA_80_D40_utils_avg_2$y[1] <- agents_4_thresholdA_80_D40_utils_avg$y[1]
agents_4_thresholdA_80_D40_utils_avg_2$y[2] <- agents_4_thresholdA_80_D40_utils_avg$y[2]
agents_4_thresholdA_80_D40_utils_avg_2$y[3] <- agents_4_thresholdA_80_D40_utils_avg$y[3]
agents_4_thresholdA_80_D40_utils_avg_2$y[4] <- agents_4_thresholdA_80_D40_utils_avg$y[4]

agents_4_threshold_A80_avg <- data.frame(rbind(agents_4_thresholdA_80_D20_utils_avg_2, agents_4_thresholdA_80_D30_utils_avg_2,agents_4_thresholdA_80_D40_utils_avg_2))
agents_4_threshold_A80_avg_result <- agents_4_threshold_A80_avg[FALSE,]
agents_4_threshold_A80_avg_result <- data.frame(t(colMeans(subset(agents_4_threshold_A80_avg, x == 1))))
agents_4_threshold_A80_avg_result <- rbind(agents_4_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_A80_avg, x == 2)))))
agents_4_threshold_A80_avg_result <- rbind(agents_4_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_A80_avg, x == 3)))))
agents_4_threshold_A80_avg_result <- rbind(agents_4_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_A80_avg, x == 4)))))

agents_4_threshold_A80_avg_result

#DISSAPROVE 20
agents_4_thresholdD_20 <- agents_4[agents_4[,grep(20,threshold_disapprove)],]
agents_4_thresholdD_20_A60 <- agents_4_thresholdD_20[agents_4_thresholdD_20[,grep(60,threshold_approve)],]
agents_4_thresholdD_20_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdD_20_A60$util_agent_splitted))), nrow=length(agents_4_thresholdD_20_A60$util_agent_splitted), byrow=T))
agents_4_thresholdD_20_A60_utils_avg <- colMeans(agents_4_thresholdD_20_A60_utils)
agents_4_thresholdD_20_A60_utils_avg <- data.frame(y = agents_4_thresholdD_20_A60_utils_avg)
agents_4_thresholdD_20_A60_utils_avg$x <- seq.int(4)
agents_4_thresholdD_20_A60_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdD_20_A60_utils_avg_2$y[1] <- agents_4_thresholdD_20_A60_utils_avg$y[1]
agents_4_thresholdD_20_A60_utils_avg_2$y[2] <- agents_4_thresholdD_20_A60_utils_avg$y[2]
agents_4_thresholdD_20_A60_utils_avg_2$y[3] <- agents_4_thresholdD_20_A60_utils_avg$y[3]
agents_4_thresholdD_20_A60_utils_avg_2$y[4] <- agents_4_thresholdD_20_A60_utils_avg$y[4]


agents_4_thresholdD_20_A70 <- agents_4_thresholdD_20[agents_4_thresholdD_20[,grep(70,threshold_approve)],]
agents_4_thresholdD_20_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdD_20_A70$util_agent_splitted))), nrow=length(agents_4_thresholdD_20_A70$util_agent_splitted), byrow=T))
agents_4_thresholdD_20_A70_utils_avg <- colMeans(agents_4_thresholdD_20_A70_utils)
agents_4_thresholdD_20_A70_utils_avg <- data.frame(y = agents_4_thresholdD_20_A70_utils_avg)
agents_4_thresholdD_20_A70_utils_avg$x <- seq.int(4)
agents_4_thresholdD_20_A70_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdD_20_A70_utils_avg_2$y[1] <- agents_4_thresholdD_20_A70_utils_avg$y[1]
agents_4_thresholdD_20_A70_utils_avg_2$y[2] <- agents_4_thresholdD_20_A70_utils_avg$y[2]
agents_4_thresholdD_20_A70_utils_avg_2$y[3] <- agents_4_thresholdD_20_A70_utils_avg$y[3]
agents_4_thresholdD_20_A70_utils_avg_2$y[4] <- agents_4_thresholdD_20_A70_utils_avg$y[4]


agents_4_thresholdD_20_A80 <- agents_4_thresholdD_20[agents_4_thresholdD_20[,grep(80,threshold_approve)],]
agents_4_thresholdD_20_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdD_20_A80$util_agent_splitted))), nrow=length(agents_4_thresholdD_20_A80$util_agent_splitted), byrow=T))
agents_4_thresholdD_20_A80_utils_avg <- colMeans(agents_4_thresholdD_20_A80_utils)
agents_4_thresholdD_20_A80_utils_avg <- data.frame(y = agents_4_thresholdD_20_A80_utils_avg)
agents_4_thresholdD_20_A80_utils_avg$x <- seq.int(4)
agents_4_thresholdD_20_A80_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdD_20_A80_utils_avg_2$y[1] <- agents_4_thresholdD_20_A80_utils_avg$y[1]
agents_4_thresholdD_20_A80_utils_avg_2$y[2] <- agents_4_thresholdD_20_A80_utils_avg$y[2]
agents_4_thresholdD_20_A80_utils_avg_2$y[3] <- agents_4_thresholdD_20_A80_utils_avg$y[3]
agents_4_thresholdD_20_A80_utils_avg_2$y[4] <- agents_4_thresholdD_20_A80_utils_avg$y[4]


agents_4_threshold_D20_avg <- data.frame(rbind(agents_4_thresholdD_20_A60_utils_avg_2, agents_4_thresholdD_20_A70_utils_avg_2,agents_4_thresholdD_20_A80_utils_avg_2))
agents_4_threshold_D20_avg_result <- agents_4_threshold_D20_avg[FALSE,]
agents_4_threshold_D20_avg_result <- data.frame(t(colMeans(subset(agents_4_threshold_D20_avg, x == 1))))
agents_4_threshold_D20_avg_result <- rbind(agents_4_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_D20_avg, x == 2)))))
agents_4_threshold_D20_avg_result <- rbind(agents_4_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_D20_avg, x == 3)))))
agents_4_threshold_D20_avg_result <- rbind(agents_4_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_D20_avg, x == 4)))))

agents_4_threshold_D20_avg_result

#disapprove 30
agents_4_thresholdD_30 <- agents_4[agents_4[,grep(30,threshold_disapprove)],]
agents_4_thresholdD_30_D20 <- agents_4_thresholdD_30[agents_4_thresholdD_30[,grep(60,threshold_approve)],]
agents_4_thresholdD_30_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdD_30_D20$util_agent_splitted))), nrow=length(agents_4_thresholdD_30_D20$util_agent_splitted), byrow=T))
agents_4_thresholdD_30_A60_utils_avg <- colMeans(agents_4_thresholdD_30_A60_utils)
agents_4_thresholdD_30_A60_utils_avg <- data.frame(y = agents_4_thresholdD_30_A60_utils_avg)
agents_4_thresholdD_30_A60_utils_avg$x <- seq.int(4)
agents_4_thresholdD_30_A60_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdD_30_A60_utils_avg_2$y[1] <- agents_4_thresholdD_30_A60_utils_avg$y[1]
agents_4_thresholdD_30_A60_utils_avg_2$y[2] <- agents_4_thresholdD_30_A60_utils_avg$y[2]
agents_4_thresholdD_30_A60_utils_avg_2$y[3] <- agents_4_thresholdD_30_A60_utils_avg$y[3]
agents_4_thresholdD_30_A60_utils_avg_2$y[4] <- agents_4_thresholdD_30_A60_utils_avg$y[4]


agents_4_thresholdD_30_A70 <- agents_4_thresholdD_30[agents_4_thresholdD_30[,grep(70,threshold_approve)],]
agents_4_thresholdD_30_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdD_30_A70$util_agent_splitted))), nrow=length(agents_4_thresholdD_30_A70$util_agent_splitted), byrow=T))
agents_4_thresholdD_30_A70_utils_avg <- colMeans(agents_4_thresholdD_30_A70_utils)
agents_4_thresholdD_30_A70_utils_avg <- data.frame(y = agents_4_thresholdD_30_A70_utils_avg)
agents_4_thresholdD_30_A70_utils_avg$x <- seq.int(4)
agents_4_thresholdD_30_A70_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdD_30_A70_utils_avg_2$y[1] <- agents_4_thresholdD_30_A70_utils_avg$y[1]
agents_4_thresholdD_30_A70_utils_avg_2$y[2] <- agents_4_thresholdD_30_A70_utils_avg$y[2]
agents_4_thresholdD_30_A70_utils_avg_2$y[3] <- agents_4_thresholdD_30_A70_utils_avg$y[3]
agents_4_thresholdD_30_A70_utils_avg_2$y[4] <- agents_4_thresholdD_30_A70_utils_avg$y[4]



agents_4_thresholdD_30_A80 <- agents_4_thresholdD_30[agents_4_thresholdD_30[,grep(80,threshold_approve)],]
agents_4_thresholdD_30_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdD_30_A80$util_agent_splitted))), nrow=length(agents_4_thresholdD_30_A80$util_agent_splitted), byrow=T))
agents_4_thresholdD_30_A80_utils_avg <- colMeans(agents_4_thresholdD_30_A80_utils)
agents_4_thresholdD_30_A80_utils_avg <- data.frame(y = agents_4_thresholdD_30_A80_utils_avg)
agents_4_thresholdD_30_A80_utils_avg$x <- seq.int(4)
agents_4_thresholdD_30_A80_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdD_30_A80_utils_avg_2$y[1] <- agents_4_thresholdD_30_A80_utils_avg$y[1]
agents_4_thresholdD_30_A80_utils_avg_2$y[2] <- agents_4_thresholdD_30_A80_utils_avg$y[2]
agents_4_thresholdD_30_A80_utils_avg_2$y[3] <- agents_4_thresholdD_30_A80_utils_avg$y[3]
agents_4_thresholdD_30_A80_utils_avg_2$y[4] <- agents_4_thresholdD_30_A80_utils_avg$y[4]


agents_4_threshold_D30_avg <- data.frame(rbind(agents_4_thresholdD_30_A60_utils_avg_2, agents_4_thresholdD_30_A70_utils_avg_2,agents_4_thresholdD_30_A80_utils_avg_2))
agents_4_threshold_D30_avg_result <- agents_4_threshold_D30_avg[FALSE,]
agents_4_threshold_D30_avg_result <- data.frame(t(colMeans(subset(agents_4_threshold_D30_avg, x == 1))))
agents_4_threshold_D30_avg_result <- rbind(agents_4_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_D30_avg, x == 2)))))
agents_4_threshold_D30_avg_result <- rbind(agents_4_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_D30_avg, x == 3)))))
agents_4_threshold_D30_avg_result <- rbind(agents_4_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_D30_avg, x == 4)))))

agents_4_threshold_D30_avg_result

#disapprove 40
agents_4_thresholdD_40 <- agents_4[agents_4[,grep(40,threshold_disapprove)],]
agents_4_thresholdD_40_D20 <- agents_4_thresholdD_40[agents_4_thresholdD_40[,grep(60,threshold_approve)],]
agents_4_thresholdD_40_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdD_40_D20$util_agent_splitted))), nrow=length(agents_4_thresholdD_40_D20$util_agent_splitted), byrow=T))
agents_4_thresholdD_40_A60_utils_avg <- colMeans(agents_4_thresholdD_40_A60_utils)
agents_4_thresholdD_40_A60_utils_avg <- data.frame(y = agents_4_thresholdD_40_A60_utils_avg)
agents_4_thresholdD_40_A60_utils_avg$x <- seq.int(4)
agents_4_thresholdD_40_A60_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdD_40_A60_utils_avg_2$y[1] <- agents_4_thresholdD_40_A60_utils_avg$y[1]
agents_4_thresholdD_40_A60_utils_avg_2$y[2] <- agents_4_thresholdD_40_A60_utils_avg$y[2]
agents_4_thresholdD_40_A60_utils_avg_2$y[3] <- agents_4_thresholdD_40_A60_utils_avg$y[3]
agents_4_thresholdD_40_A60_utils_avg_2$y[4] <- agents_4_thresholdD_40_A60_utils_avg$y[4]


agents_4_thresholdD_40_A70 <- agents_4_thresholdD_40[agents_4_thresholdD_40[,grep(70,threshold_approve)],]
agents_4_thresholdD_40_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdD_40_A70$util_agent_splitted))), nrow=length(agents_4_thresholdD_40_A70$util_agent_splitted), byrow=T))
agents_4_thresholdD_40_A70_utils_avg <- colMeans(agents_4_thresholdD_40_A70_utils)
agents_4_thresholdD_40_A70_utils_avg <- data.frame(y = agents_4_thresholdD_40_A70_utils_avg)
agents_4_thresholdD_40_A70_utils_avg$x <- seq.int(4)
agents_4_thresholdD_40_A70_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdD_40_A70_utils_avg_2$y[1] <- agents_4_thresholdD_40_A70_utils_avg$y[1]
agents_4_thresholdD_40_A70_utils_avg_2$y[2] <- agents_4_thresholdD_40_A70_utils_avg$y[2]
agents_4_thresholdD_40_A70_utils_avg_2$y[3] <- agents_4_thresholdD_40_A70_utils_avg$y[3]
agents_4_thresholdD_40_A70_utils_avg_2$y[4] <- agents_4_thresholdD_40_A70_utils_avg$y[4]



agents_4_thresholdD_40_A80 <- agents_4_thresholdD_40[agents_4_thresholdD_40[,grep(80,threshold_approve)],]
agents_4_thresholdD_40_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_4_thresholdD_40_A80$util_agent_splitted))), nrow=length(agents_4_thresholdD_40_A80$util_agent_splitted), byrow=T))
agents_4_thresholdD_40_A80_utils_avg <- colMeans(agents_4_thresholdD_40_A80_utils)
agents_4_thresholdD_40_A80_utils_avg <- data.frame(y = agents_4_thresholdD_40_A80_utils_avg)
agents_4_thresholdD_40_A80_utils_avg$x <- seq.int(4)
agents_4_thresholdD_40_A80_utils_avg_2 <- agents_4_all_utils_avg
agents_4_thresholdD_40_A80_utils_avg_2$y[1] <- agents_4_thresholdD_40_A80_utils_avg$y[1]
agents_4_thresholdD_40_A80_utils_avg_2$y[2] <- agents_4_thresholdD_40_A80_utils_avg$y[2]
agents_4_thresholdD_40_A80_utils_avg_2$y[3] <- agents_4_thresholdD_40_A80_utils_avg$y[3]
agents_4_thresholdD_40_A80_utils_avg_2$y[4] <- agents_4_thresholdD_40_A80_utils_avg$y[4]


agents_4_threshold_D40_avg <- data.frame(rbind(agents_4_thresholdD_40_A60_utils_avg_2, agents_4_thresholdD_40_A70_utils_avg_2,agents_4_thresholdD_40_A80_utils_avg_2))
agents_4_threshold_D40_avg_result <- agents_4_threshold_D40_avg[FALSE,]
agents_4_threshold_D40_avg_result <- data.frame(t(colMeans(subset(agents_4_threshold_D40_avg, x == 1))))
agents_4_threshold_D40_avg_result <- rbind(agents_4_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_D40_avg, x == 2)))))
agents_4_threshold_D40_avg_result <- rbind(agents_4_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_D40_avg, x == 3)))))
agents_4_threshold_D40_avg_result <- rbind(agents_4_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_4_threshold_D40_avg, x == 4)))))

agents_4_threshold_D40_avg_result

integer_breaks <- function(n = 1, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
ggplot(data=agents_4_all_utils, aes(x=x, y=y, group=row, scale_size(guide = "none"))) +
  geom_line(linetype="solid", color="black", alpha=0.05) +
  #xlim(1, 9) +
  ylim(0.55, 0.82) + labs(x = "position in the game", y="utily") +
  scale_x_discrete(name ="position in the game", limits=c("1","2","3","4")) +
  #geom_line(data=agents_4_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) +
  ggtitle("4 Agents") +
  geom_line(data=agents_4_threshold_A60_avg_result,aes(x=x,y=y),color="#00de3b",size=1,alpha=0.8) +
  geom_line(data=agents_4_threshold_A70_avg_result,aes(x=x,y=y),color="#00b831",size=1,alpha=0.8) +
  geom_line(data=agents_4_threshold_A80_avg_result,aes(x=x,y=y),color="#008223",size=1,alpha=0.8) +
  geom_line(data=agents_4_threshold_D20_avg_result,aes(x=x,y=y),color="#e33900",size=1,alpha=0.8) +
  geom_line(data=agents_4_threshold_D30_avg_result,aes(x=x,y=y),color="#ad2c00",size=1,alpha=0.8) +
  geom_line(data=agents_4_threshold_D40_avg_result,aes(x=x,y=y),color="#701d00",size=1,alpha=0.8)
#scale_x_continuous(breaks = integer_breaks)
#geom_line(data=agents_4_thresholdA_60_D20_utils_avg_2,aes(x=x,y=y),color="#0077ff",size=1,alpha=0.8) +
#geom_line(data=agents_4_thresholdA_60_D30_utils_avg_2,aes(x=x,y=y),color="#0053b3",size=1,alpha=0.8) +
#geom_line(data=agents_4_thresholdA_60_D40_utils_avg_2,aes(x=x,y=y),color="#3b96ff",size=1,alpha=0.8) +
#geom_line(data=agents_4_thresholdA_70_D20_utils_avg_2,aes(x=x,y=y),color="#00ff00",size=1,alpha=0.8) +
#geom_line(data=agents_4_thresholdA_70_D30_utils_avg_2,aes(x=x,y=y),color="#00a800",size=1,alpha=0.8) +
#geom_line(data=agents_4_thresholdA_70_D40_utils_avg_2,aes(x=x,y=y),color="#80ff80",size=1,alpha=0.8) +
#geom_line(data=agents_4_thresholdA_80_D20_utils_avg_2,aes(x=x,y=y),color="#ff0000",size=1,alpha=0.8) +
#geom_line(data=agents_4_thresholdA_80_D30_utils_avg_2,aes(x=x,y=y),color="#8c0000",size=1,alpha=0.8) +
#geom_line(data=agents_4_thresholdA_80_D40_utils_avg_2,aes(x=x,y=y),color="#ff6969",size=1,alpha=0.8) +
#theme(legend.position = c(0, 1),legend.justification = c(0, 1))



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

agents_5_utils <- data.frame(matrix(unlist(agents_5$util_agent_splitted), nrow=length(agents_5$util_agent_splitted), byrow=T))
agents_5_utils_1 <- data.frame(y=as.numeric(paste(agents_5_utils$X1)))
agents_5_utils_2 <- data.frame(y=as.numeric(paste(agents_5_utils$X2)))
agents_5_utils_3 <- data.frame(y=as.numeric(paste(agents_5_utils$X3)))
agents_5_utils_4 <- data.frame(y=as.numeric(paste(agents_5_utils$X4)))
agents_5_utils_5 <- data.frame(y=as.numeric(paste(agents_5_utils$X5)))
agents_5_utils_1$row <- seq.int(nrow(agents_5_utils_1))
agents_5_utils_2$row <- seq.int(nrow(agents_5_utils_2))
agents_5_utils_3$row <- seq.int(nrow(agents_5_utils_3))
agents_5_utils_4$row <- seq.int(nrow(agents_5_utils_4))
agents_5_utils_5$row <- seq.int(nrow(agents_5_utils_5))
agents_5_utils_1$x <- 1
agents_5_utils_2$x <- 2
agents_5_utils_3$x <- 3
agents_5_utils_4$x <- 4
agents_5_utils_5$x <- 5

agents_5_all_utils <- rbind(agents_5_utils_1, agents_5_utils_2, agents_5_utils_3, agents_5_utils_4, agents_5_utils_5)

agents_5_utils_1_avg <- colMeans(agents_5_utils_1)
agents_5_utils_2_avg <- colMeans(agents_5_utils_2)
agents_5_utils_3_avg <- colMeans(agents_5_utils_3)
agents_5_utils_4_avg <- colMeans(agents_5_utils_4)
agents_5_utils_5_avg <- colMeans(agents_5_utils_5)

agents_5_all_utils_avg <- data.frame(rbind(agents_5_utils_1_avg, agents_5_utils_2_avg, agents_5_utils_3_avg, agents_5_utils_4_avg, agents_5_utils_5_avg))
agents_5_all_utils_avg

#APPROVE 60
agents_5_thresholdA_60 <- agents_5[agents_5[,grep(60,threshold_approve)],]
agents_5_thresholdA_60_D20 <- agents_5_thresholdA_60[agents_5_thresholdA_60[,grep(20,threshold_disapprove)],]
agents_5_thresholdA_60_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdA_60_D20$util_agent_splitted))), nrow=length(agents_5_thresholdA_60_D20$util_agent_splitted), byrow=T))
agents_5_thresholdA_60_D20_utils_avg <- colMeans(agents_5_thresholdA_60_D20_utils)
agents_5_thresholdA_60_D20_utils_avg <- data.frame(y = agents_5_thresholdA_60_D20_utils_avg)
agents_5_thresholdA_60_D20_utils_avg$x <- seq.int(5)
agents_5_thresholdA_60_D20_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdA_60_D20_utils_avg_2$y[1] <- agents_5_thresholdA_60_D20_utils_avg$y[1]
agents_5_thresholdA_60_D20_utils_avg_2$y[2] <- agents_5_thresholdA_60_D20_utils_avg$y[2]
agents_5_thresholdA_60_D20_utils_avg_2$y[3] <- agents_5_thresholdA_60_D20_utils_avg$y[3]
agents_5_thresholdA_60_D20_utils_avg_2$y[4] <- agents_5_thresholdA_60_D20_utils_avg$y[4]
agents_5_thresholdA_60_D20_utils_avg_2$y[5] <- agents_5_thresholdA_60_D20_utils_avg$y[5]

agents_5_thresholdA_60_D30 <- agents_5_thresholdA_60[agents_5_thresholdA_60[,grep(30,threshold_disapprove)],]
agents_5_thresholdA_60_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdA_60_D30$util_agent_splitted))), nrow=length(agents_5_thresholdA_60_D30$util_agent_splitted), byrow=T))
agents_5_thresholdA_60_D30_utils_avg <- colMeans(agents_5_thresholdA_60_D30_utils)
agents_5_thresholdA_60_D30_utils_avg <- data.frame(y = agents_5_thresholdA_60_D30_utils_avg)
agents_5_thresholdA_60_D30_utils_avg$x <- seq.int(5)
agents_5_thresholdA_60_D30_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdA_60_D30_utils_avg_2$y[1] <- agents_5_thresholdA_60_D30_utils_avg$y[1]
agents_5_thresholdA_60_D30_utils_avg_2$y[2] <- agents_5_thresholdA_60_D30_utils_avg$y[2]
agents_5_thresholdA_60_D30_utils_avg_2$y[3] <- agents_5_thresholdA_60_D30_utils_avg$y[3]
agents_5_thresholdA_60_D30_utils_avg_2$y[4] <- agents_5_thresholdA_60_D30_utils_avg$y[4]
agents_5_thresholdA_60_D30_utils_avg_2$y[5] <- agents_5_thresholdA_60_D30_utils_avg$y[5]

agents_5_thresholdA_60_D40 <- agents_5_thresholdA_60[agents_5_thresholdA_60[,grep(40,threshold_disapprove)],]
agents_5_thresholdA_60_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdA_60_D40$util_agent_splitted))), nrow=length(agents_5_thresholdA_60_D40$util_agent_splitted), byrow=T))
agents_5_thresholdA_60_D40_utils_avg <- colMeans(agents_5_thresholdA_60_D40_utils)
agents_5_thresholdA_60_D40_utils_avg <- data.frame(y = agents_5_thresholdA_60_D40_utils_avg)
agents_5_thresholdA_60_D40_utils_avg$x <- seq.int(5)
agents_5_thresholdA_60_D40_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdA_60_D40_utils_avg_2$y[1] <- agents_5_thresholdA_60_D40_utils_avg$y[1]
agents_5_thresholdA_60_D40_utils_avg_2$y[2] <- agents_5_thresholdA_60_D40_utils_avg$y[2]
agents_5_thresholdA_60_D40_utils_avg_2$y[3] <- agents_5_thresholdA_60_D40_utils_avg$y[3]
agents_5_thresholdA_60_D40_utils_avg_2$y[4] <- agents_5_thresholdA_60_D40_utils_avg$y[4]
agents_5_thresholdA_60_D40_utils_avg_2$y[5] <- agents_5_thresholdA_60_D40_utils_avg$y[5]


agents_5_threshold_A60_avg <- data.frame(rbind(agents_5_thresholdA_60_D20_utils_avg_2, agents_5_thresholdA_60_D30_utils_avg_2,agents_5_thresholdA_60_D40_utils_avg_2))
agents_5_threshold_A60_avg_result <- agents_5_threshold_A60_avg[FALSE,]
agents_5_threshold_A60_avg_result <- data.frame(t(colMeans(subset(agents_5_threshold_A60_avg, x == 1))))
agents_5_threshold_A60_avg_result <- rbind(agents_5_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A60_avg, x == 2)))))
agents_5_threshold_A60_avg_result <- rbind(agents_5_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A60_avg, x == 3)))))
agents_5_threshold_A60_avg_result <- rbind(agents_5_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A60_avg, x == 4)))))
agents_5_threshold_A60_avg_result <- rbind(agents_5_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A60_avg, x == 5)))))

agents_5_threshold_A60_avg_result

#APPROVE 70
agents_5_thresholdA_70 <- agents_5[agents_5[,grep(70,threshold_approve)],]
agents_5_thresholdA_70_D20 <- agents_5_thresholdA_70[agents_5_thresholdA_70[,grep(20,threshold_disapprove)],]
agents_5_thresholdA_70_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdA_70_D20$util_agent_splitted))), nrow=length(agents_5_thresholdA_70_D20$util_agent_splitted), byrow=T))
agents_5_thresholdA_70_D20_utils_avg <- colMeans(agents_5_thresholdA_70_D20_utils)
agents_5_thresholdA_70_D20_utils_avg <- data.frame(y = agents_5_thresholdA_70_D20_utils_avg)
agents_5_thresholdA_70_D20_utils_avg$x <- seq.int(5)
agents_5_thresholdA_70_D20_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdA_70_D20_utils_avg_2$y[1] <- agents_5_thresholdA_70_D20_utils_avg$y[1]
agents_5_thresholdA_70_D20_utils_avg_2$y[2] <- agents_5_thresholdA_70_D20_utils_avg$y[2]
agents_5_thresholdA_70_D20_utils_avg_2$y[3] <- agents_5_thresholdA_70_D20_utils_avg$y[3]
agents_5_thresholdA_70_D20_utils_avg_2$y[4] <- agents_5_thresholdA_70_D20_utils_avg$y[4]
agents_5_thresholdA_70_D20_utils_avg_2$y[5] <- agents_5_thresholdA_70_D20_utils_avg$y[5]


agents_5_thresholdA_70_D30 <- agents_5_thresholdA_70[agents_5_thresholdA_70[,grep(30,threshold_disapprove)],]
agents_5_thresholdA_70_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdA_70_D30$util_agent_splitted))), nrow=length(agents_5_thresholdA_70_D30$util_agent_splitted), byrow=T))
agents_5_thresholdA_70_D30_utils_avg <- colMeans(agents_5_thresholdA_70_D30_utils)
agents_5_thresholdA_70_D30_utils_avg <- data.frame(y = agents_5_thresholdA_70_D30_utils_avg)
agents_5_thresholdA_70_D30_utils_avg$x <- seq.int(5)
agents_5_thresholdA_70_D30_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdA_70_D30_utils_avg_2$y[1] <- agents_5_thresholdA_70_D30_utils_avg$y[1]
agents_5_thresholdA_70_D30_utils_avg_2$y[2] <- agents_5_thresholdA_70_D30_utils_avg$y[2]
agents_5_thresholdA_70_D30_utils_avg_2$y[3] <- agents_5_thresholdA_70_D30_utils_avg$y[3]
agents_5_thresholdA_70_D30_utils_avg_2$y[4] <- agents_5_thresholdA_70_D30_utils_avg$y[4]
agents_5_thresholdA_70_D30_utils_avg_2$y[5] <- agents_5_thresholdA_70_D30_utils_avg$y[5]


agents_5_thresholdA_70_D40 <- agents_5_thresholdA_70[agents_5_thresholdA_70[,grep(40,threshold_disapprove)],]
agents_5_thresholdA_70_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdA_70_D40$util_agent_splitted))), nrow=length(agents_5_thresholdA_70_D40$util_agent_splitted), byrow=T))
agents_5_thresholdA_70_D40_utils_avg <- colMeans(agents_5_thresholdA_70_D40_utils)
agents_5_thresholdA_70_D40_utils_avg <- data.frame(y = agents_5_thresholdA_70_D40_utils_avg)
agents_5_thresholdA_70_D40_utils_avg$x <- seq.int(5)
agents_5_thresholdA_70_D40_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdA_70_D40_utils_avg_2$y[1] <- agents_5_thresholdA_70_D40_utils_avg$y[1]
agents_5_thresholdA_70_D40_utils_avg_2$y[2] <- agents_5_thresholdA_70_D40_utils_avg$y[2]
agents_5_thresholdA_70_D40_utils_avg_2$y[3] <- agents_5_thresholdA_70_D40_utils_avg$y[3]
agents_5_thresholdA_70_D40_utils_avg_2$y[4] <- agents_5_thresholdA_70_D40_utils_avg$y[4]
agents_5_thresholdA_70_D40_utils_avg_2$y[5] <- agents_5_thresholdA_70_D40_utils_avg$y[5]


agents_5_threshold_A70_avg <- data.frame(rbind(agents_5_thresholdA_70_D20_utils_avg_2, agents_5_thresholdA_70_D30_utils_avg_2,agents_5_thresholdA_70_D40_utils_avg_2))
agents_5_threshold_A70_avg_result <- agents_5_threshold_A70_avg[FALSE,]
agents_5_threshold_A70_avg_result <- data.frame(t(colMeans(subset(agents_5_threshold_A70_avg, x == 1))))
agents_5_threshold_A70_avg_result <- rbind(agents_5_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A70_avg, x == 2)))))
agents_5_threshold_A70_avg_result <- rbind(agents_5_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A70_avg, x == 3)))))
agents_5_threshold_A70_avg_result <- rbind(agents_5_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A70_avg, x == 4)))))
agents_5_threshold_A70_avg_result <- rbind(agents_5_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A70_avg, x == 5)))))

agents_5_threshold_A70_avg_result

#APPROVE 80
agents_5_thresholdA_80 <- agents_5[agents_5[,grep(80,threshold_approve)],]
agents_5_thresholdA_80_D20 <- agents_5_thresholdA_80[agents_5_thresholdA_80[,grep(20,threshold_disapprove)],]
agents_5_thresholdA_80_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdA_80_D20$util_agent_splitted))), nrow=length(agents_5_thresholdA_80_D20$util_agent_splitted), byrow=T))
agents_5_thresholdA_80_D20_utils_avg <- colMeans(agents_5_thresholdA_80_D20_utils)
agents_5_thresholdA_80_D20_utils_avg <- data.frame(y = agents_5_thresholdA_80_D20_utils_avg)
agents_5_thresholdA_80_D20_utils_avg$x <- seq.int(5)
agents_5_thresholdA_80_D20_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdA_80_D20_utils_avg_2$y[1] <- agents_5_thresholdA_80_D20_utils_avg$y[1]
agents_5_thresholdA_80_D20_utils_avg_2$y[2] <- agents_5_thresholdA_80_D20_utils_avg$y[2]
agents_5_thresholdA_80_D20_utils_avg_2$y[3] <- agents_5_thresholdA_80_D20_utils_avg$y[3]
agents_5_thresholdA_80_D20_utils_avg_2$y[4] <- agents_5_thresholdA_80_D20_utils_avg$y[4]
agents_5_thresholdA_80_D20_utils_avg_2$y[5] <- agents_5_thresholdA_80_D20_utils_avg$y[5]

agents_5_thresholdA_80_D30 <- agents_5_thresholdA_80[agents_5_thresholdA_80[,grep(30,threshold_disapprove)],]
agents_5_thresholdA_80_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdA_80_D30$util_agent_splitted))), nrow=length(agents_5_thresholdA_80_D30$util_agent_splitted), byrow=T))
agents_5_thresholdA_80_D30_utils_avg <- colMeans(agents_5_thresholdA_80_D30_utils)
agents_5_thresholdA_80_D30_utils_avg <- data.frame(y = agents_5_thresholdA_80_D30_utils_avg)
agents_5_thresholdA_80_D30_utils_avg$x <- seq.int(5)
agents_5_thresholdA_80_D30_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdA_80_D30_utils_avg_2$y[1] <- agents_5_thresholdA_80_D30_utils_avg$y[1]
agents_5_thresholdA_80_D30_utils_avg_2$y[2] <- agents_5_thresholdA_80_D30_utils_avg$y[2]
agents_5_thresholdA_80_D30_utils_avg_2$y[3] <- agents_5_thresholdA_80_D30_utils_avg$y[3]
agents_5_thresholdA_80_D30_utils_avg_2$y[4] <- agents_5_thresholdA_80_D30_utils_avg$y[4]
agents_5_thresholdA_80_D30_utils_avg_2$y[5] <- agents_5_thresholdA_80_D30_utils_avg$y[5]


agents_5_thresholdA_80_D40 <- agents_5_thresholdA_80[agents_5_thresholdA_80[,grep(40,threshold_disapprove)],]
agents_5_thresholdA_80_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdA_80_D40$util_agent_splitted))), nrow=length(agents_5_thresholdA_80_D40$util_agent_splitted), byrow=T))
agents_5_thresholdA_80_D40_utils_avg <- colMeans(agents_5_thresholdA_80_D40_utils)
agents_5_thresholdA_80_D40_utils_avg <- data.frame(y = agents_5_thresholdA_80_D40_utils_avg)
agents_5_thresholdA_80_D40_utils_avg$x <- seq.int(5)
agents_5_thresholdA_80_D40_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdA_80_D40_utils_avg_2$y[1] <- agents_5_thresholdA_80_D40_utils_avg$y[1]
agents_5_thresholdA_80_D40_utils_avg_2$y[2] <- agents_5_thresholdA_80_D40_utils_avg$y[2]
agents_5_thresholdA_80_D40_utils_avg_2$y[3] <- agents_5_thresholdA_80_D40_utils_avg$y[3]
agents_5_thresholdA_80_D40_utils_avg_2$y[4] <- agents_5_thresholdA_80_D40_utils_avg$y[4]
agents_5_thresholdA_80_D40_utils_avg_2$y[5] <- agents_5_thresholdA_80_D40_utils_avg$y[5]

agents_5_threshold_A80_avg <- data.frame(rbind(agents_5_thresholdA_80_D20_utils_avg_2, agents_5_thresholdA_80_D30_utils_avg_2,agents_5_thresholdA_80_D40_utils_avg_2))
agents_5_threshold_A80_avg_result <- agents_5_threshold_A80_avg[FALSE,]
agents_5_threshold_A80_avg_result <- data.frame(t(colMeans(subset(agents_5_threshold_A80_avg, x == 1))))
agents_5_threshold_A80_avg_result <- rbind(agents_5_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A80_avg, x == 2)))))
agents_5_threshold_A80_avg_result <- rbind(agents_5_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A80_avg, x == 3)))))
agents_5_threshold_A80_avg_result <- rbind(agents_5_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A80_avg, x == 4)))))
agents_5_threshold_A80_avg_result <- rbind(agents_5_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_A80_avg, x == 5)))))

agents_5_threshold_A80_avg_result

#DISSAPROVE 20
agents_5_thresholdD_20 <- agents_5[agents_5[,grep(20,threshold_disapprove)],]
agents_5_thresholdD_20_A60 <- agents_5_thresholdD_20[agents_5_thresholdD_20[,grep(60,threshold_approve)],]
agents_5_thresholdD_20_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdD_20_A60$util_agent_splitted))), nrow=length(agents_5_thresholdD_20_A60$util_agent_splitted), byrow=T))
agents_5_thresholdD_20_A60_utils_avg <- colMeans(agents_5_thresholdD_20_A60_utils)
agents_5_thresholdD_20_A60_utils_avg <- data.frame(y = agents_5_thresholdD_20_A60_utils_avg)
agents_5_thresholdD_20_A60_utils_avg$x <- seq.int(5)
agents_5_thresholdD_20_A60_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdD_20_A60_utils_avg_2$y[1] <- agents_5_thresholdD_20_A60_utils_avg$y[1]
agents_5_thresholdD_20_A60_utils_avg_2$y[2] <- agents_5_thresholdD_20_A60_utils_avg$y[2]
agents_5_thresholdD_20_A60_utils_avg_2$y[3] <- agents_5_thresholdD_20_A60_utils_avg$y[3]
agents_5_thresholdD_20_A60_utils_avg_2$y[4] <- agents_5_thresholdD_20_A60_utils_avg$y[4]
agents_5_thresholdD_20_A60_utils_avg_2$y[5] <- agents_5_thresholdD_20_A60_utils_avg$y[5]


agents_5_thresholdD_20_A70 <- agents_5_thresholdD_20[agents_5_thresholdD_20[,grep(70,threshold_approve)],]
agents_5_thresholdD_20_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdD_20_A70$util_agent_splitted))), nrow=length(agents_5_thresholdD_20_A70$util_agent_splitted), byrow=T))
agents_5_thresholdD_20_A70_utils_avg <- colMeans(agents_5_thresholdD_20_A70_utils)
agents_5_thresholdD_20_A70_utils_avg <- data.frame(y = agents_5_thresholdD_20_A70_utils_avg)
agents_5_thresholdD_20_A70_utils_avg$x <- seq.int(5)
agents_5_thresholdD_20_A70_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdD_20_A70_utils_avg_2$y[1] <- agents_5_thresholdD_20_A70_utils_avg$y[1]
agents_5_thresholdD_20_A70_utils_avg_2$y[2] <- agents_5_thresholdD_20_A70_utils_avg$y[2]
agents_5_thresholdD_20_A70_utils_avg_2$y[3] <- agents_5_thresholdD_20_A70_utils_avg$y[3]
agents_5_thresholdD_20_A70_utils_avg_2$y[4] <- agents_5_thresholdD_20_A70_utils_avg$y[4]
agents_5_thresholdD_20_A70_utils_avg_2$y[5] <- agents_5_thresholdD_20_A70_utils_avg$y[5]


agents_5_thresholdD_20_A80 <- agents_5_thresholdD_20[agents_5_thresholdD_20[,grep(80,threshold_approve)],]
agents_5_thresholdD_20_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdD_20_A80$util_agent_splitted))), nrow=length(agents_5_thresholdD_20_A80$util_agent_splitted), byrow=T))
agents_5_thresholdD_20_A80_utils_avg <- colMeans(agents_5_thresholdD_20_A80_utils)
agents_5_thresholdD_20_A80_utils_avg <- data.frame(y = agents_5_thresholdD_20_A80_utils_avg)
agents_5_thresholdD_20_A80_utils_avg$x <- seq.int(5)
agents_5_thresholdD_20_A80_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdD_20_A80_utils_avg_2$y[1] <- agents_5_thresholdD_20_A80_utils_avg$y[1]
agents_5_thresholdD_20_A80_utils_avg_2$y[2] <- agents_5_thresholdD_20_A80_utils_avg$y[2]
agents_5_thresholdD_20_A80_utils_avg_2$y[3] <- agents_5_thresholdD_20_A80_utils_avg$y[3]
agents_5_thresholdD_20_A80_utils_avg_2$y[4] <- agents_5_thresholdD_20_A80_utils_avg$y[4]
agents_5_thresholdD_20_A80_utils_avg_2$y[5] <- agents_5_thresholdD_20_A80_utils_avg$y[5]


agents_5_threshold_D20_avg <- data.frame(rbind(agents_5_thresholdD_20_A60_utils_avg_2, agents_5_thresholdD_20_A70_utils_avg_2,agents_5_thresholdD_20_A80_utils_avg_2))
agents_5_threshold_D20_avg_result <- agents_5_threshold_D20_avg[FALSE,]
agents_5_threshold_D20_avg_result <- data.frame(t(colMeans(subset(agents_5_threshold_D20_avg, x == 1))))
agents_5_threshold_D20_avg_result <- rbind(agents_5_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D20_avg, x == 2)))))
agents_5_threshold_D20_avg_result <- rbind(agents_5_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D20_avg, x == 3)))))
agents_5_threshold_D20_avg_result <- rbind(agents_5_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D20_avg, x == 4)))))
agents_5_threshold_D20_avg_result <- rbind(agents_5_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D20_avg, x == 5)))))

agents_5_threshold_D20_avg_result

#disapprove 30
agents_5_thresholdD_30 <- agents_5[agents_5[,grep(30,threshold_disapprove)],]
agents_5_thresholdD_30_D20 <- agents_5_thresholdD_30[agents_5_thresholdD_30[,grep(60,threshold_approve)],]
agents_5_thresholdD_30_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdD_30_D20$util_agent_splitted))), nrow=length(agents_5_thresholdD_30_D20$util_agent_splitted), byrow=T))
agents_5_thresholdD_30_A60_utils_avg <- colMeans(agents_5_thresholdD_30_A60_utils)
agents_5_thresholdD_30_A60_utils_avg <- data.frame(y = agents_5_thresholdD_30_A60_utils_avg)
agents_5_thresholdD_30_A60_utils_avg$x <- seq.int(5)
agents_5_thresholdD_30_A60_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdD_30_A60_utils_avg_2$y[1] <- agents_5_thresholdD_30_A60_utils_avg$y[1]
agents_5_thresholdD_30_A60_utils_avg_2$y[2] <- agents_5_thresholdD_30_A60_utils_avg$y[2]
agents_5_thresholdD_30_A60_utils_avg_2$y[3] <- agents_5_thresholdD_30_A60_utils_avg$y[3]
agents_5_thresholdD_30_A60_utils_avg_2$y[4] <- agents_5_thresholdD_30_A60_utils_avg$y[4]
agents_5_thresholdD_30_A60_utils_avg_2$y[5] <- agents_5_thresholdD_30_A60_utils_avg$y[5]


agents_5_thresholdD_30_A70 <- agents_5_thresholdD_30[agents_5_thresholdD_30[,grep(70,threshold_approve)],]
agents_5_thresholdD_30_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdD_30_A70$util_agent_splitted))), nrow=length(agents_5_thresholdD_30_A70$util_agent_splitted), byrow=T))
agents_5_thresholdD_30_A70_utils_avg <- colMeans(agents_5_thresholdD_30_A70_utils)
agents_5_thresholdD_30_A70_utils_avg <- data.frame(y = agents_5_thresholdD_30_A70_utils_avg)
agents_5_thresholdD_30_A70_utils_avg$x <- seq.int(5)
agents_5_thresholdD_30_A70_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdD_30_A70_utils_avg_2$y[1] <- agents_5_thresholdD_30_A70_utils_avg$y[1]
agents_5_thresholdD_30_A70_utils_avg_2$y[2] <- agents_5_thresholdD_30_A70_utils_avg$y[2]
agents_5_thresholdD_30_A70_utils_avg_2$y[3] <- agents_5_thresholdD_30_A70_utils_avg$y[3]
agents_5_thresholdD_30_A70_utils_avg_2$y[4] <- agents_5_thresholdD_30_A70_utils_avg$y[4]
agents_5_thresholdD_30_A70_utils_avg_2$y[5] <- agents_5_thresholdD_30_A70_utils_avg$y[5]



agents_5_thresholdD_30_A80 <- agents_5_thresholdD_30[agents_5_thresholdD_30[,grep(80,threshold_approve)],]
agents_5_thresholdD_30_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdD_30_A80$util_agent_splitted))), nrow=length(agents_5_thresholdD_30_A80$util_agent_splitted), byrow=T))
agents_5_thresholdD_30_A80_utils_avg <- colMeans(agents_5_thresholdD_30_A80_utils)
agents_5_thresholdD_30_A80_utils_avg <- data.frame(y = agents_5_thresholdD_30_A80_utils_avg)
agents_5_thresholdD_30_A80_utils_avg$x <- seq.int(5)
agents_5_thresholdD_30_A80_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdD_30_A80_utils_avg_2$y[1] <- agents_5_thresholdD_30_A80_utils_avg$y[1]
agents_5_thresholdD_30_A80_utils_avg_2$y[2] <- agents_5_thresholdD_30_A80_utils_avg$y[2]
agents_5_thresholdD_30_A80_utils_avg_2$y[3] <- agents_5_thresholdD_30_A80_utils_avg$y[3]
agents_5_thresholdD_30_A80_utils_avg_2$y[4] <- agents_5_thresholdD_30_A80_utils_avg$y[4]
agents_5_thresholdD_30_A80_utils_avg_2$y[5] <- agents_5_thresholdD_30_A80_utils_avg$y[5]


agents_5_threshold_D30_avg <- data.frame(rbind(agents_5_thresholdD_30_A60_utils_avg_2, agents_5_thresholdD_30_A70_utils_avg_2,agents_5_thresholdD_30_A80_utils_avg_2))
agents_5_threshold_D30_avg_result <- agents_5_threshold_D30_avg[FALSE,]
agents_5_threshold_D30_avg_result <- data.frame(t(colMeans(subset(agents_5_threshold_D30_avg, x == 1))))
agents_5_threshold_D30_avg_result <- rbind(agents_5_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D30_avg, x == 2)))))
agents_5_threshold_D30_avg_result <- rbind(agents_5_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D30_avg, x == 3)))))
agents_5_threshold_D30_avg_result <- rbind(agents_5_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D30_avg, x == 4)))))
agents_5_threshold_D30_avg_result <- rbind(agents_5_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D30_avg, x == 5)))))

agents_5_threshold_D30_avg_result

#disapprove 40
agents_5_thresholdD_40 <- agents_5[agents_5[,grep(40,threshold_disapprove)],]
agents_5_thresholdD_40_D20 <- agents_5_thresholdD_40[agents_5_thresholdD_40[,grep(60,threshold_approve)],]
agents_5_thresholdD_40_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdD_40_D20$util_agent_splitted))), nrow=length(agents_5_thresholdD_40_D20$util_agent_splitted), byrow=T))
agents_5_thresholdD_40_A60_utils_avg <- colMeans(agents_5_thresholdD_40_A60_utils)
agents_5_thresholdD_40_A60_utils_avg <- data.frame(y = agents_5_thresholdD_40_A60_utils_avg)
agents_5_thresholdD_40_A60_utils_avg$x <- seq.int(5)
agents_5_thresholdD_40_A60_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdD_40_A60_utils_avg_2$y[1] <- agents_5_thresholdD_40_A60_utils_avg$y[1]
agents_5_thresholdD_40_A60_utils_avg_2$y[2] <- agents_5_thresholdD_40_A60_utils_avg$y[2]
agents_5_thresholdD_40_A60_utils_avg_2$y[3] <- agents_5_thresholdD_40_A60_utils_avg$y[3]
agents_5_thresholdD_40_A60_utils_avg_2$y[4] <- agents_5_thresholdD_40_A60_utils_avg$y[4]
agents_5_thresholdD_40_A60_utils_avg_2$y[5] <- agents_5_thresholdD_40_A60_utils_avg$y[5]


agents_5_thresholdD_40_A70 <- agents_5_thresholdD_40[agents_5_thresholdD_40[,grep(70,threshold_approve)],]
agents_5_thresholdD_40_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdD_40_A70$util_agent_splitted))), nrow=length(agents_5_thresholdD_40_A70$util_agent_splitted), byrow=T))
agents_5_thresholdD_40_A70_utils_avg <- colMeans(agents_5_thresholdD_40_A70_utils)
agents_5_thresholdD_40_A70_utils_avg <- data.frame(y = agents_5_thresholdD_40_A70_utils_avg)
agents_5_thresholdD_40_A70_utils_avg$x <- seq.int(5)
agents_5_thresholdD_40_A70_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdD_40_A70_utils_avg_2$y[1] <- agents_5_thresholdD_40_A70_utils_avg$y[1]
agents_5_thresholdD_40_A70_utils_avg_2$y[2] <- agents_5_thresholdD_40_A70_utils_avg$y[2]
agents_5_thresholdD_40_A70_utils_avg_2$y[3] <- agents_5_thresholdD_40_A70_utils_avg$y[3]
agents_5_thresholdD_40_A70_utils_avg_2$y[4] <- agents_5_thresholdD_40_A70_utils_avg$y[4]
agents_5_thresholdD_40_A70_utils_avg_2$y[5] <- agents_5_thresholdD_40_A70_utils_avg$y[5]



agents_5_thresholdD_40_A80 <- agents_5_thresholdD_40[agents_5_thresholdD_40[,grep(80,threshold_approve)],]
agents_5_thresholdD_40_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_5_thresholdD_40_A80$util_agent_splitted))), nrow=length(agents_5_thresholdD_40_A80$util_agent_splitted), byrow=T))
agents_5_thresholdD_40_A80_utils_avg <- colMeans(agents_5_thresholdD_40_A80_utils)
agents_5_thresholdD_40_A80_utils_avg <- data.frame(y = agents_5_thresholdD_40_A80_utils_avg)
agents_5_thresholdD_40_A80_utils_avg$x <- seq.int(5)
agents_5_thresholdD_40_A80_utils_avg_2 <- agents_5_all_utils_avg
agents_5_thresholdD_40_A80_utils_avg_2$y[1] <- agents_5_thresholdD_40_A80_utils_avg$y[1]
agents_5_thresholdD_40_A80_utils_avg_2$y[2] <- agents_5_thresholdD_40_A80_utils_avg$y[2]
agents_5_thresholdD_40_A80_utils_avg_2$y[3] <- agents_5_thresholdD_40_A80_utils_avg$y[3]
agents_5_thresholdD_40_A80_utils_avg_2$y[4] <- agents_5_thresholdD_40_A80_utils_avg$y[4]
agents_5_thresholdD_40_A80_utils_avg_2$y[5] <- agents_5_thresholdD_40_A80_utils_avg$y[5]


agents_5_threshold_D40_avg <- data.frame(rbind(agents_5_thresholdD_40_A60_utils_avg_2, agents_5_thresholdD_40_A70_utils_avg_2,agents_5_thresholdD_40_A80_utils_avg_2))
agents_5_threshold_D40_avg_result <- agents_5_threshold_D40_avg[FALSE,]
agents_5_threshold_D40_avg_result <- data.frame(t(colMeans(subset(agents_5_threshold_D40_avg, x == 1))))
agents_5_threshold_D40_avg_result <- rbind(agents_5_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D40_avg, x == 2)))))
agents_5_threshold_D40_avg_result <- rbind(agents_5_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D40_avg, x == 3)))))
agents_5_threshold_D40_avg_result <- rbind(agents_5_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D40_avg, x == 4)))))
agents_5_threshold_D40_avg_result <- rbind(agents_5_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_5_threshold_D40_avg, x == 5)))))

agents_5_threshold_D40_avg_result

integer_breaks <- function(n = 1, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
ggplot(data=agents_5_all_utils, aes(x=x, y=y, group=row, scale_size(guide = "none"))) +
  geom_line(linetype="solid", color="black", alpha=0.05) +
  #xlim(1, 9) +
  ylim(0.55, 0.82) + labs(x = "position in the game", y="utily") +
  scale_x_discrete(name ="position in the game", limits=c("1","2","3","4","5")) +
  ggtitle("5 Agents") +
  #geom_line(data=agents_5_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) +
  geom_line(data=agents_5_threshold_A60_avg_result,aes(x=x,y=y),color="#00de3b",size=1,alpha=0.8) +
  geom_line(data=agents_5_threshold_A70_avg_result,aes(x=x,y=y),color="#00b831",size=1,alpha=0.8) +
  geom_line(data=agents_5_threshold_A80_avg_result,aes(x=x,y=y),color="#008223",size=1,alpha=0.8) +
  geom_line(data=agents_5_threshold_D20_avg_result,aes(x=x,y=y),color="#e33900",size=1,alpha=0.8) +
  geom_line(data=agents_5_threshold_D30_avg_result,aes(x=x,y=y),color="#ad2c00",size=1,alpha=0.8) +
  geom_line(data=agents_5_threshold_D40_avg_result,aes(x=x,y=y),color="#701d00",size=1,alpha=0.8)
#scale_x_continuous(breaks = integer_breaks)
#geom_line(data=agents_5_thresholdA_60_D20_utils_avg_2,aes(x=x,y=y),color="#0077ff",size=1,alpha=0.8) +
#geom_line(data=agents_5_thresholdA_60_D30_utils_avg_2,aes(x=x,y=y),color="#0053b3",size=1,alpha=0.8) +
#geom_line(data=agents_5_thresholdA_60_D40_utils_avg_2,aes(x=x,y=y),color="#3b96ff",size=1,alpha=0.8) +
#geom_line(data=agents_5_thresholdA_70_D20_utils_avg_2,aes(x=x,y=y),color="#00ff00",size=1,alpha=0.8) +
#geom_line(data=agents_5_thresholdA_70_D30_utils_avg_2,aes(x=x,y=y),color="#00a800",size=1,alpha=0.8) +
#geom_line(data=agents_5_thresholdA_70_D40_utils_avg_2,aes(x=x,y=y),color="#80ff80",size=1,alpha=0.8) +
#geom_line(data=agents_5_thresholdA_80_D20_utils_avg_2,aes(x=x,y=y),color="#ff0000",size=1,alpha=0.8) +
#geom_line(data=agents_5_thresholdA_80_D30_utils_avg_2,aes(x=x,y=y),color="#8c0000",size=1,alpha=0.8) +
#geom_line(data=agents_5_thresholdA_80_D40_utils_avg_2,aes(x=x,y=y),color="#ff6969",size=1,alpha=0.8) +
#theme(legend.position = c(0, 1),legend.justification = c(0, 1))



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
agents_6_thresholdA_60_D20_utils_avg$x <- seq.int(6)
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
agents_6_thresholdA_60_D30_utils_avg$x <- seq.int(6)
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
agents_6_thresholdA_60_D40_utils_avg$x <- seq.int(6)
agents_6_thresholdA_60_D40_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_60_D40_utils_avg_2$y[1] <- agents_6_thresholdA_60_D40_utils_avg$y[1]
agents_6_thresholdA_60_D40_utils_avg_2$y[2] <- agents_6_thresholdA_60_D40_utils_avg$y[2]
agents_6_thresholdA_60_D40_utils_avg_2$y[3] <- agents_6_thresholdA_60_D40_utils_avg$y[3]
agents_6_thresholdA_60_D40_utils_avg_2$y[4] <- agents_6_thresholdA_60_D40_utils_avg$y[4]
agents_6_thresholdA_60_D40_utils_avg_2$y[5] <- agents_6_thresholdA_60_D40_utils_avg$y[5]
agents_6_thresholdA_60_D40_utils_avg_2$y[6] <- agents_6_thresholdA_60_D40_utils_avg$y[6]


agents_6_threshold_A60_avg <- data.frame(rbind(agents_6_thresholdA_60_D20_utils_avg_2, agents_6_thresholdA_60_D30_utils_avg_2,agents_6_thresholdA_60_D40_utils_avg_2))
agents_6_threshold_A60_avg_result <- agents_6_threshold_A60_avg[FALSE,]
agents_6_threshold_A60_avg_result <- data.frame(t(colMeans(subset(agents_6_threshold_A60_avg, x == 1))))
agents_6_threshold_A60_avg_result <- rbind(agents_6_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A60_avg, x == 2)))))
agents_6_threshold_A60_avg_result <- rbind(agents_6_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A60_avg, x == 3)))))
agents_6_threshold_A60_avg_result <- rbind(agents_6_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A60_avg, x == 4)))))
agents_6_threshold_A60_avg_result <- rbind(agents_6_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A60_avg, x == 5)))))
agents_6_threshold_A60_avg_result <- rbind(agents_6_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A60_avg, x == 6)))))

agents_6_threshold_A60_avg_result

#APPROVE 70
agents_6_thresholdA_70 <- agents_6[agents_6[,grep(70,threshold_approve)],]
agents_6_thresholdA_70_D20 <- agents_6_thresholdA_70[agents_6_thresholdA_70[,grep(20,threshold_disapprove)],]
agents_6_thresholdA_70_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdA_70_D20$util_agent_splitted))), nrow=length(agents_6_thresholdA_70_D20$util_agent_splitted), byrow=T))
agents_6_thresholdA_70_D20_utils_avg <- colMeans(agents_6_thresholdA_70_D20_utils)
agents_6_thresholdA_70_D20_utils_avg <- data.frame(y = agents_6_thresholdA_70_D20_utils_avg)
agents_6_thresholdA_70_D20_utils_avg$x <- seq.int(6)
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
agents_6_thresholdA_70_D30_utils_avg$x <- seq.int(6)
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
agents_6_thresholdA_70_D40_utils_avg$x <- seq.int(6)
agents_6_thresholdA_70_D40_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_70_D40_utils_avg_2$y[1] <- agents_6_thresholdA_70_D40_utils_avg$y[1]
agents_6_thresholdA_70_D40_utils_avg_2$y[2] <- agents_6_thresholdA_70_D40_utils_avg$y[2]
agents_6_thresholdA_70_D40_utils_avg_2$y[3] <- agents_6_thresholdA_70_D40_utils_avg$y[3]
agents_6_thresholdA_70_D40_utils_avg_2$y[4] <- agents_6_thresholdA_70_D40_utils_avg$y[4]
agents_6_thresholdA_70_D40_utils_avg_2$y[5] <- agents_6_thresholdA_70_D40_utils_avg$y[5]
agents_6_thresholdA_70_D40_utils_avg_2$y[6] <- agents_6_thresholdA_70_D40_utils_avg$y[6]


agents_6_threshold_A70_avg <- data.frame(rbind(agents_6_thresholdA_70_D20_utils_avg_2, agents_6_thresholdA_70_D30_utils_avg_2,agents_6_thresholdA_70_D40_utils_avg_2))
agents_6_threshold_A70_avg_result <- agents_6_threshold_A70_avg[FALSE,]
agents_6_threshold_A70_avg_result <- data.frame(t(colMeans(subset(agents_6_threshold_A70_avg, x == 1))))
agents_6_threshold_A70_avg_result <- rbind(agents_6_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A70_avg, x == 2)))))
agents_6_threshold_A70_avg_result <- rbind(agents_6_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A70_avg, x == 3)))))
agents_6_threshold_A70_avg_result <- rbind(agents_6_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A70_avg, x == 4)))))
agents_6_threshold_A70_avg_result <- rbind(agents_6_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A70_avg, x == 5)))))
agents_6_threshold_A70_avg_result <- rbind(agents_6_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A70_avg, x == 6)))))

agents_6_threshold_A70_avg_result

#APPROVE 80
agents_6_thresholdA_80 <- agents_6[agents_6[,grep(80,threshold_approve)],]
agents_6_thresholdA_80_D20 <- agents_6_thresholdA_80[agents_6_thresholdA_80[,grep(20,threshold_disapprove)],]
agents_6_thresholdA_80_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdA_80_D20$util_agent_splitted))), nrow=length(agents_6_thresholdA_80_D20$util_agent_splitted), byrow=T))
agents_6_thresholdA_80_D20_utils_avg <- colMeans(agents_6_thresholdA_80_D20_utils)
agents_6_thresholdA_80_D20_utils_avg <- data.frame(y = agents_6_thresholdA_80_D20_utils_avg)
agents_6_thresholdA_80_D20_utils_avg$x <- seq.int(6)
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
agents_6_thresholdA_80_D30_utils_avg$x <- seq.int(6)
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
agents_6_thresholdA_80_D40_utils_avg$x <- seq.int(6)
agents_6_thresholdA_80_D40_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdA_80_D40_utils_avg_2$y[1] <- agents_6_thresholdA_80_D40_utils_avg$y[1]
agents_6_thresholdA_80_D40_utils_avg_2$y[2] <- agents_6_thresholdA_80_D40_utils_avg$y[2]
agents_6_thresholdA_80_D40_utils_avg_2$y[3] <- agents_6_thresholdA_80_D40_utils_avg$y[3]
agents_6_thresholdA_80_D40_utils_avg_2$y[4] <- agents_6_thresholdA_80_D40_utils_avg$y[4]
agents_6_thresholdA_80_D40_utils_avg_2$y[5] <- agents_6_thresholdA_80_D40_utils_avg$y[5]
agents_6_thresholdA_80_D40_utils_avg_2$y[6] <- agents_6_thresholdA_80_D40_utils_avg$y[6]

agents_6_threshold_A80_avg <- data.frame(rbind(agents_6_thresholdA_80_D20_utils_avg_2, agents_6_thresholdA_80_D30_utils_avg_2,agents_6_thresholdA_80_D40_utils_avg_2))
agents_6_threshold_A80_avg_result <- agents_6_threshold_A80_avg[FALSE,]
agents_6_threshold_A80_avg_result <- data.frame(t(colMeans(subset(agents_6_threshold_A80_avg, x == 1))))
agents_6_threshold_A80_avg_result <- rbind(agents_6_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A80_avg, x == 2)))))
agents_6_threshold_A80_avg_result <- rbind(agents_6_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A80_avg, x == 3)))))
agents_6_threshold_A80_avg_result <- rbind(agents_6_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A80_avg, x == 4)))))
agents_6_threshold_A80_avg_result <- rbind(agents_6_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A80_avg, x == 5)))))
agents_6_threshold_A80_avg_result <- rbind(agents_6_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_A80_avg, x == 6)))))

agents_6_threshold_A80_avg_result

#DISSAPROVE 20
agents_6_thresholdD_20 <- agents_6[agents_6[,grep(20,threshold_disapprove)],]
agents_6_thresholdD_20_A60 <- agents_6_thresholdD_20[agents_6_thresholdD_20[,grep(60,threshold_approve)],]
agents_6_thresholdD_20_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdD_20_A60$util_agent_splitted))), nrow=length(agents_6_thresholdD_20_A60$util_agent_splitted), byrow=T))
agents_6_thresholdD_20_A60_utils_avg <- colMeans(agents_6_thresholdD_20_A60_utils)
agents_6_thresholdD_20_A60_utils_avg <- data.frame(y = agents_6_thresholdD_20_A60_utils_avg)
agents_6_thresholdD_20_A60_utils_avg$x <- seq.int(6)
agents_6_thresholdD_20_A60_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdD_20_A60_utils_avg_2$y[1] <- agents_6_thresholdD_20_A60_utils_avg$y[1]
agents_6_thresholdD_20_A60_utils_avg_2$y[2] <- agents_6_thresholdD_20_A60_utils_avg$y[2]
agents_6_thresholdD_20_A60_utils_avg_2$y[3] <- agents_6_thresholdD_20_A60_utils_avg$y[3]
agents_6_thresholdD_20_A60_utils_avg_2$y[4] <- agents_6_thresholdD_20_A60_utils_avg$y[4]
agents_6_thresholdD_20_A60_utils_avg_2$y[5] <- agents_6_thresholdD_20_A60_utils_avg$y[5]
agents_6_thresholdD_20_A60_utils_avg_2$y[6] <- agents_6_thresholdD_20_A60_utils_avg$y[6]


agents_6_thresholdD_20_A70 <- agents_6_thresholdD_20[agents_6_thresholdD_20[,grep(70,threshold_approve)],]
agents_6_thresholdD_20_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdD_20_A70$util_agent_splitted))), nrow=length(agents_6_thresholdD_20_A70$util_agent_splitted), byrow=T))
agents_6_thresholdD_20_A70_utils_avg <- colMeans(agents_6_thresholdD_20_A70_utils)
agents_6_thresholdD_20_A70_utils_avg <- data.frame(y = agents_6_thresholdD_20_A70_utils_avg)
agents_6_thresholdD_20_A70_utils_avg$x <- seq.int(6)
agents_6_thresholdD_20_A70_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdD_20_A70_utils_avg_2$y[1] <- agents_6_thresholdD_20_A70_utils_avg$y[1]
agents_6_thresholdD_20_A70_utils_avg_2$y[2] <- agents_6_thresholdD_20_A70_utils_avg$y[2]
agents_6_thresholdD_20_A70_utils_avg_2$y[3] <- agents_6_thresholdD_20_A70_utils_avg$y[3]
agents_6_thresholdD_20_A70_utils_avg_2$y[4] <- agents_6_thresholdD_20_A70_utils_avg$y[4]
agents_6_thresholdD_20_A70_utils_avg_2$y[5] <- agents_6_thresholdD_20_A70_utils_avg$y[5]
agents_6_thresholdD_20_A70_utils_avg_2$y[6] <- agents_6_thresholdD_20_A70_utils_avg$y[6]


agents_6_thresholdD_20_A80 <- agents_6_thresholdD_20[agents_6_thresholdD_20[,grep(80,threshold_approve)],]
agents_6_thresholdD_20_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdD_20_A80$util_agent_splitted))), nrow=length(agents_6_thresholdD_20_A80$util_agent_splitted), byrow=T))
agents_6_thresholdD_20_A80_utils_avg <- colMeans(agents_6_thresholdD_20_A80_utils)
agents_6_thresholdD_20_A80_utils_avg <- data.frame(y = agents_6_thresholdD_20_A80_utils_avg)
agents_6_thresholdD_20_A80_utils_avg$x <- seq.int(6)
agents_6_thresholdD_20_A80_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdD_20_A80_utils_avg_2$y[1] <- agents_6_thresholdD_20_A80_utils_avg$y[1]
agents_6_thresholdD_20_A80_utils_avg_2$y[2] <- agents_6_thresholdD_20_A80_utils_avg$y[2]
agents_6_thresholdD_20_A80_utils_avg_2$y[3] <- agents_6_thresholdD_20_A80_utils_avg$y[3]
agents_6_thresholdD_20_A80_utils_avg_2$y[4] <- agents_6_thresholdD_20_A80_utils_avg$y[4]
agents_6_thresholdD_20_A80_utils_avg_2$y[5] <- agents_6_thresholdD_20_A80_utils_avg$y[5]
agents_6_thresholdD_20_A80_utils_avg_2$y[6] <- agents_6_thresholdD_20_A80_utils_avg$y[6]


agents_6_threshold_D20_avg <- data.frame(rbind(agents_6_thresholdD_20_A60_utils_avg_2, agents_6_thresholdD_20_A70_utils_avg_2,agents_6_thresholdD_20_A80_utils_avg_2))
agents_6_threshold_D20_avg_result <- agents_6_threshold_D20_avg[FALSE,]
agents_6_threshold_D20_avg_result <- data.frame(t(colMeans(subset(agents_6_threshold_D20_avg, x == 1))))
agents_6_threshold_D20_avg_result <- rbind(agents_6_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D20_avg, x == 2)))))
agents_6_threshold_D20_avg_result <- rbind(agents_6_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D20_avg, x == 3)))))
agents_6_threshold_D20_avg_result <- rbind(agents_6_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D20_avg, x == 4)))))
agents_6_threshold_D20_avg_result <- rbind(agents_6_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D20_avg, x == 5)))))
agents_6_threshold_D20_avg_result <- rbind(agents_6_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D20_avg, x == 6)))))

agents_6_threshold_D20_avg_result

#disapprove 30
agents_6_thresholdD_30 <- agents_6[agents_6[,grep(30,threshold_disapprove)],]
agents_6_thresholdD_30_D20 <- agents_6_thresholdD_30[agents_6_thresholdD_30[,grep(60,threshold_approve)],]
agents_6_thresholdD_30_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdD_30_D20$util_agent_splitted))), nrow=length(agents_6_thresholdD_30_D20$util_agent_splitted), byrow=T))
agents_6_thresholdD_30_A60_utils_avg <- colMeans(agents_6_thresholdD_30_A60_utils)
agents_6_thresholdD_30_A60_utils_avg <- data.frame(y = agents_6_thresholdD_30_A60_utils_avg)
agents_6_thresholdD_30_A60_utils_avg$x <- seq.int(6)
agents_6_thresholdD_30_A60_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdD_30_A60_utils_avg_2$y[1] <- agents_6_thresholdD_30_A60_utils_avg$y[1]
agents_6_thresholdD_30_A60_utils_avg_2$y[2] <- agents_6_thresholdD_30_A60_utils_avg$y[2]
agents_6_thresholdD_30_A60_utils_avg_2$y[3] <- agents_6_thresholdD_30_A60_utils_avg$y[3]
agents_6_thresholdD_30_A60_utils_avg_2$y[4] <- agents_6_thresholdD_30_A60_utils_avg$y[4]
agents_6_thresholdD_30_A60_utils_avg_2$y[5] <- agents_6_thresholdD_30_A60_utils_avg$y[5]
agents_6_thresholdD_30_A60_utils_avg_2$y[6] <- agents_6_thresholdD_30_A60_utils_avg$y[6]


agents_6_thresholdD_30_A70 <- agents_6_thresholdD_30[agents_6_thresholdD_30[,grep(70,threshold_approve)],]
agents_6_thresholdD_30_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdD_30_A70$util_agent_splitted))), nrow=length(agents_6_thresholdD_30_A70$util_agent_splitted), byrow=T))
agents_6_thresholdD_30_A70_utils_avg <- colMeans(agents_6_thresholdD_30_A70_utils)
agents_6_thresholdD_30_A70_utils_avg <- data.frame(y = agents_6_thresholdD_30_A70_utils_avg)
agents_6_thresholdD_30_A70_utils_avg$x <- seq.int(6)
agents_6_thresholdD_30_A70_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdD_30_A70_utils_avg_2$y[1] <- agents_6_thresholdD_30_A70_utils_avg$y[1]
agents_6_thresholdD_30_A70_utils_avg_2$y[2] <- agents_6_thresholdD_30_A70_utils_avg$y[2]
agents_6_thresholdD_30_A70_utils_avg_2$y[3] <- agents_6_thresholdD_30_A70_utils_avg$y[3]
agents_6_thresholdD_30_A70_utils_avg_2$y[4] <- agents_6_thresholdD_30_A70_utils_avg$y[4]
agents_6_thresholdD_30_A70_utils_avg_2$y[5] <- agents_6_thresholdD_30_A70_utils_avg$y[5]
agents_6_thresholdD_30_A70_utils_avg_2$y[6] <- agents_6_thresholdD_30_A70_utils_avg$y[6]



agents_6_thresholdD_30_A80 <- agents_6_thresholdD_30[agents_6_thresholdD_30[,grep(80,threshold_approve)],]
agents_6_thresholdD_30_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdD_30_A80$util_agent_splitted))), nrow=length(agents_6_thresholdD_30_A80$util_agent_splitted), byrow=T))
agents_6_thresholdD_30_A80_utils_avg <- colMeans(agents_6_thresholdD_30_A80_utils)
agents_6_thresholdD_30_A80_utils_avg <- data.frame(y = agents_6_thresholdD_30_A80_utils_avg)
agents_6_thresholdD_30_A80_utils_avg$x <- seq.int(6)
agents_6_thresholdD_30_A80_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdD_30_A80_utils_avg_2$y[1] <- agents_6_thresholdD_30_A80_utils_avg$y[1]
agents_6_thresholdD_30_A80_utils_avg_2$y[2] <- agents_6_thresholdD_30_A80_utils_avg$y[2]
agents_6_thresholdD_30_A80_utils_avg_2$y[3] <- agents_6_thresholdD_30_A80_utils_avg$y[3]
agents_6_thresholdD_30_A80_utils_avg_2$y[4] <- agents_6_thresholdD_30_A80_utils_avg$y[4]
agents_6_thresholdD_30_A80_utils_avg_2$y[5] <- agents_6_thresholdD_30_A80_utils_avg$y[5]
agents_6_thresholdD_30_A80_utils_avg_2$y[6] <- agents_6_thresholdD_30_A80_utils_avg$y[6]


agents_6_threshold_D30_avg <- data.frame(rbind(agents_6_thresholdD_30_A60_utils_avg_2, agents_6_thresholdD_30_A70_utils_avg_2,agents_6_thresholdD_30_A80_utils_avg_2))
agents_6_threshold_D30_avg_result <- agents_6_threshold_D30_avg[FALSE,]
agents_6_threshold_D30_avg_result <- data.frame(t(colMeans(subset(agents_6_threshold_D30_avg, x == 1))))
agents_6_threshold_D30_avg_result <- rbind(agents_6_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D30_avg, x == 2)))))
agents_6_threshold_D30_avg_result <- rbind(agents_6_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D30_avg, x == 3)))))
agents_6_threshold_D30_avg_result <- rbind(agents_6_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D30_avg, x == 4)))))
agents_6_threshold_D30_avg_result <- rbind(agents_6_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D30_avg, x == 5)))))
agents_6_threshold_D30_avg_result <- rbind(agents_6_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D30_avg, x == 6)))))

agents_6_threshold_D30_avg_result

#disapprove 40
agents_6_thresholdD_40 <- agents_6[agents_6[,grep(40,threshold_disapprove)],]
agents_6_thresholdD_40_D20 <- agents_6_thresholdD_40[agents_6_thresholdD_40[,grep(60,threshold_approve)],]
agents_6_thresholdD_40_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdD_40_D20$util_agent_splitted))), nrow=length(agents_6_thresholdD_40_D20$util_agent_splitted), byrow=T))
agents_6_thresholdD_40_A60_utils_avg <- colMeans(agents_6_thresholdD_40_A60_utils)
agents_6_thresholdD_40_A60_utils_avg <- data.frame(y = agents_6_thresholdD_40_A60_utils_avg)
agents_6_thresholdD_40_A60_utils_avg$x <- seq.int(6)
agents_6_thresholdD_40_A60_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdD_40_A60_utils_avg_2$y[1] <- agents_6_thresholdD_40_A60_utils_avg$y[1]
agents_6_thresholdD_40_A60_utils_avg_2$y[2] <- agents_6_thresholdD_40_A60_utils_avg$y[2]
agents_6_thresholdD_40_A60_utils_avg_2$y[3] <- agents_6_thresholdD_40_A60_utils_avg$y[3]
agents_6_thresholdD_40_A60_utils_avg_2$y[4] <- agents_6_thresholdD_40_A60_utils_avg$y[4]
agents_6_thresholdD_40_A60_utils_avg_2$y[5] <- agents_6_thresholdD_40_A60_utils_avg$y[5]
agents_6_thresholdD_40_A60_utils_avg_2$y[6] <- agents_6_thresholdD_40_A60_utils_avg$y[6]


agents_6_thresholdD_40_A70 <- agents_6_thresholdD_40[agents_6_thresholdD_40[,grep(70,threshold_approve)],]
agents_6_thresholdD_40_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdD_40_A70$util_agent_splitted))), nrow=length(agents_6_thresholdD_40_A70$util_agent_splitted), byrow=T))
agents_6_thresholdD_40_A70_utils_avg <- colMeans(agents_6_thresholdD_40_A70_utils)
agents_6_thresholdD_40_A70_utils_avg <- data.frame(y = agents_6_thresholdD_40_A70_utils_avg)
agents_6_thresholdD_40_A70_utils_avg$x <- seq.int(6)
agents_6_thresholdD_40_A70_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdD_40_A70_utils_avg_2$y[1] <- agents_6_thresholdD_40_A70_utils_avg$y[1]
agents_6_thresholdD_40_A70_utils_avg_2$y[2] <- agents_6_thresholdD_40_A70_utils_avg$y[2]
agents_6_thresholdD_40_A70_utils_avg_2$y[3] <- agents_6_thresholdD_40_A70_utils_avg$y[3]
agents_6_thresholdD_40_A70_utils_avg_2$y[4] <- agents_6_thresholdD_40_A70_utils_avg$y[4]
agents_6_thresholdD_40_A70_utils_avg_2$y[5] <- agents_6_thresholdD_40_A70_utils_avg$y[5]
agents_6_thresholdD_40_A70_utils_avg_2$y[6] <- agents_6_thresholdD_40_A70_utils_avg$y[6]



agents_6_thresholdD_40_A80 <- agents_6_thresholdD_40[agents_6_thresholdD_40[,grep(80,threshold_approve)],]
agents_6_thresholdD_40_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_6_thresholdD_40_A80$util_agent_splitted))), nrow=length(agents_6_thresholdD_40_A80$util_agent_splitted), byrow=T))
agents_6_thresholdD_40_A80_utils_avg <- colMeans(agents_6_thresholdD_40_A80_utils)
agents_6_thresholdD_40_A80_utils_avg <- data.frame(y = agents_6_thresholdD_40_A80_utils_avg)
agents_6_thresholdD_40_A80_utils_avg$x <- seq.int(6)
agents_6_thresholdD_40_A80_utils_avg_2 <- agents_6_all_utils_avg
agents_6_thresholdD_40_A80_utils_avg_2$y[1] <- agents_6_thresholdD_40_A80_utils_avg$y[1]
agents_6_thresholdD_40_A80_utils_avg_2$y[2] <- agents_6_thresholdD_40_A80_utils_avg$y[2]
agents_6_thresholdD_40_A80_utils_avg_2$y[3] <- agents_6_thresholdD_40_A80_utils_avg$y[3]
agents_6_thresholdD_40_A80_utils_avg_2$y[4] <- agents_6_thresholdD_40_A80_utils_avg$y[4]
agents_6_thresholdD_40_A80_utils_avg_2$y[5] <- agents_6_thresholdD_40_A80_utils_avg$y[5]
agents_6_thresholdD_40_A80_utils_avg_2$y[6] <- agents_6_thresholdD_40_A80_utils_avg$y[6]


agents_6_threshold_D40_avg <- data.frame(rbind(agents_6_thresholdD_40_A60_utils_avg_2, agents_6_thresholdD_40_A70_utils_avg_2,agents_6_thresholdD_40_A80_utils_avg_2))
agents_6_threshold_D40_avg_result <- agents_6_threshold_D40_avg[FALSE,]
agents_6_threshold_D40_avg_result <- data.frame(t(colMeans(subset(agents_6_threshold_D40_avg, x == 1))))
agents_6_threshold_D40_avg_result <- rbind(agents_6_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D40_avg, x == 2)))))
agents_6_threshold_D40_avg_result <- rbind(agents_6_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D40_avg, x == 3)))))
agents_6_threshold_D40_avg_result <- rbind(agents_6_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D40_avg, x == 4)))))
agents_6_threshold_D40_avg_result <- rbind(agents_6_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D40_avg, x == 5)))))
agents_6_threshold_D40_avg_result <- rbind(agents_6_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_6_threshold_D40_avg, x == 6)))))

agents_6_threshold_D40_avg_result

integer_breaks <- function(n = 1, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
ggplot(data=agents_6_all_utils, aes(x=x, y=y, group=row, scale_size(guide = "none"))) +
  geom_line(linetype="solid", color="black", alpha=0.05) +
  #xlim(1, 9) +
  ylim(0.55, 0.82) + labs(x = "position in the game", y="utily") +
  scale_x_discrete(name ="position in the game", limits=c("1","2","3","4","5","6")) +
  ggtitle("6 Agents") +
  #geom_line(data=agents_6_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) +
  geom_line(data=agents_6_threshold_A60_avg_result,aes(x=x,y=y),color="#00de3b",size=1,alpha=0.8) +
  geom_line(data=agents_6_threshold_A70_avg_result,aes(x=x,y=y),color="#00b831",size=1,alpha=0.8) +
  geom_line(data=agents_6_threshold_A80_avg_result,aes(x=x,y=y),color="#008223",size=1,alpha=0.8) +
  geom_line(data=agents_6_threshold_D20_avg_result,aes(x=x,y=y),color="#e33900",size=1,alpha=0.8) +
  geom_line(data=agents_6_threshold_D30_avg_result,aes(x=x,y=y),color="#ad2c00",size=1,alpha=0.8) +
  geom_line(data=agents_6_threshold_D40_avg_result,aes(x=x,y=y),color="#701d00",size=1,alpha=0.8)
#scale_x_continuous(breaks = integer_breaks)
#geom_line(data=agents_6_thresholdA_60_D20_utils_avg_2,aes(x=x,y=y),color="#0077ff",size=1,alpha=0.8) +
#geom_line(data=agents_6_thresholdA_60_D30_utils_avg_2,aes(x=x,y=y),color="#0053b3",size=1,alpha=0.8) +
#geom_line(data=agents_6_thresholdA_60_D40_utils_avg_2,aes(x=x,y=y),color="#3b96ff",size=1,alpha=0.8) +
#geom_line(data=agents_6_thresholdA_70_D20_utils_avg_2,aes(x=x,y=y),color="#00ff00",size=1,alpha=0.8) +
#geom_line(data=agents_6_thresholdA_70_D30_utils_avg_2,aes(x=x,y=y),color="#00a800",size=1,alpha=0.8) +
#geom_line(data=agents_6_thresholdA_70_D40_utils_avg_2,aes(x=x,y=y),color="#80ff80",size=1,alpha=0.8) +
#geom_line(data=agents_6_thresholdA_80_D20_utils_avg_2,aes(x=x,y=y),color="#ff0000",size=1,alpha=0.8) +
#geom_line(data=agents_6_thresholdA_80_D30_utils_avg_2,aes(x=x,y=y),color="#8c0000",size=1,alpha=0.8) +
#geom_line(data=agents_6_thresholdA_80_D40_utils_avg_2,aes(x=x,y=y),color="#ff6969",size=1,alpha=0.8) +
#theme(legend.position = c(0, 1),legend.justification = c(0, 1))




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

integer_breaks <- function(n = 1, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
ggplot(data=agents_9_all_utils, aes(x=x, y=y, group=row, scale_size(guide = "none"))) +
  geom_line(linetype="solid", color="black", alpha=0.05) +
  #xlim(1, 9) +
  ylim(0.55, 0.82) + labs(x = "position in the game", y="utily") +
  scale_x_discrete(name ="position in the game", limits=c("1","2","3","4","5","6","7","8","9")) +
  ggtitle("9 Agents") +
  #geom_line(data=agents_9_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) +
  geom_line(data=agents_9_threshold_A60_avg_result,aes(x=x,y=y),color="#00de3b",size=1,alpha=0.8) +
  geom_line(data=agents_9_threshold_A70_avg_result,aes(x=x,y=y),color="#00b831",size=1,alpha=0.8) +
  geom_line(data=agents_9_threshold_A80_avg_result,aes(x=x,y=y),color="#008223",size=1,alpha=0.8) +
  geom_line(data=agents_9_threshold_D20_avg_result,aes(x=x,y=y),color="#e33900",size=1,alpha=0.8) +
  geom_line(data=agents_9_threshold_D30_avg_result,aes(x=x,y=y),color="#ad2c00",size=1,alpha=0.8) +
  geom_line(data=agents_9_threshold_D40_avg_result,aes(x=x,y=y),color="#701d00",size=1,alpha=0.8)
  #scale_x_continuous(breaks = integer_breaks)
  #geom_line(data=agents_9_thresholdA_60_D20_utils_avg_2,aes(x=x,y=y),color="#0077ff",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_60_D30_utils_avg_2,aes(x=x,y=y),color="#0053b3",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_60_D40_utils_avg_2,aes(x=x,y=y),color="#3b96ff",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_70_D20_utils_avg_2,aes(x=x,y=y),color="#00ff00",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_70_D30_utils_avg_2,aes(x=x,y=y),color="#00a800",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_70_D40_utils_avg_2,aes(x=x,y=y),color="#80ff80",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_80_D20_utils_avg_2,aes(x=x,y=y),color="#ff0000",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_80_D30_utils_avg_2,aes(x=x,y=y),color="#8c0000",size=1,alpha=0.8) +
  #geom_line(data=agents_9_thresholdA_80_D40_utils_avg_2,aes(x=x,y=y),color="#ff6969",size=1,alpha=0.8) +
  #theme(legend.position = c(0, 1),legend.justification = c(0, 1))




###########################################################################
###########################################################################
###                                                                     ###
###                                 AGENTS 12                           ###
###                                                                     ###
###########################################################################
###########################################################################
agents_12$util_agent <- str_remove(agents_12$util_agent, "\\[")
agents_12$util_agent <- str_remove(agents_12$util_agent, "\\]")
agents_12$util_agent_splitted <- strsplit(agents_12$util_agent, ",")

agents_12_utils <- data.frame(matrix(unlist(agents_12$util_agent_splitted), nrow=length(agents_12$util_agent_splitted), byrow=T))
agents_12_utils_1 <- data.frame(y=as.numeric(paste(agents_12_utils$X1)))
agents_12_utils_2 <- data.frame(y=as.numeric(paste(agents_12_utils$X2)))
agents_12_utils_3 <- data.frame(y=as.numeric(paste(agents_12_utils$X3)))
agents_12_utils_4 <- data.frame(y=as.numeric(paste(agents_12_utils$X4)))
agents_12_utils_5 <- data.frame(y=as.numeric(paste(agents_12_utils$X5)))
agents_12_utils_6 <- data.frame(y=as.numeric(paste(agents_12_utils$X6)))
agents_12_utils_7 <- data.frame(y=as.numeric(paste(agents_12_utils$X7)))
agents_12_utils_8 <- data.frame(y=as.numeric(paste(agents_12_utils$X8)))
agents_12_utils_9 <- data.frame(y=as.numeric(paste(agents_12_utils$X9)))
agents_12_utils_10 <- data.frame(y=as.numeric(paste(agents_12_utils$X10)))
agents_12_utils_11 <- data.frame(y=as.numeric(paste(agents_12_utils$X11)))
agents_12_utils_12 <- data.frame(y=as.numeric(paste(agents_12_utils$X12)))
agents_12_utils_1$row <- seq.int(nrow(agents_12_utils_1))
agents_12_utils_2$row <- seq.int(nrow(agents_12_utils_2))
agents_12_utils_3$row <- seq.int(nrow(agents_12_utils_3))
agents_12_utils_4$row <- seq.int(nrow(agents_12_utils_4))
agents_12_utils_5$row <- seq.int(nrow(agents_12_utils_5))
agents_12_utils_6$row <- seq.int(nrow(agents_12_utils_6))
agents_12_utils_7$row <- seq.int(nrow(agents_12_utils_7))
agents_12_utils_8$row <- seq.int(nrow(agents_12_utils_8))
agents_12_utils_9$row <- seq.int(nrow(agents_12_utils_9))
agents_12_utils_10$row <- seq.int(nrow(agents_12_utils_10))
agents_12_utils_11$row <- seq.int(nrow(agents_12_utils_11))
agents_12_utils_12$row <- seq.int(nrow(agents_12_utils_12))
agents_12_utils_1$x <- 1
agents_12_utils_2$x <- 2
agents_12_utils_3$x <- 3
agents_12_utils_4$x <- 4
agents_12_utils_5$x <- 5
agents_12_utils_6$x <- 6
agents_12_utils_7$x <- 7
agents_12_utils_8$x <- 8
agents_12_utils_9$x <- 9
agents_12_utils_10$x <- 10
agents_12_utils_11$x <- 11
agents_12_utils_12$x <- 12

agents_12_all_utils <- rbind(agents_12_utils_1, agents_12_utils_2, agents_12_utils_3, agents_12_utils_4, agents_12_utils_5, agents_12_utils_6, agents_12_utils_7, agents_12_utils_8, agents_12_utils_9, agents_12_utils_10, agents_12_utils_11, agents_12_utils_12)

agents_12_utils_1_avg <- colMeans(agents_12_utils_1)
agents_12_utils_2_avg <- colMeans(agents_12_utils_2)
agents_12_utils_3_avg <- colMeans(agents_12_utils_3)
agents_12_utils_4_avg <- colMeans(agents_12_utils_4)
agents_12_utils_5_avg <- colMeans(agents_12_utils_5)
agents_12_utils_6_avg <- colMeans(agents_12_utils_6)
agents_12_utils_7_avg <- colMeans(agents_12_utils_7)
agents_12_utils_8_avg <- colMeans(agents_12_utils_8)
agents_12_utils_9_avg <- colMeans(agents_12_utils_9)
agents_12_utils_10_avg <- colMeans(agents_12_utils_10)
agents_12_utils_11_avg <- colMeans(agents_12_utils_11)
agents_12_utils_12_avg <- colMeans(agents_12_utils_12)

agents_12_all_utils_avg <- data.frame(rbind(agents_12_utils_1_avg, agents_12_utils_2_avg, agents_12_utils_3_avg, agents_12_utils_4_avg, agents_12_utils_5_avg, agents_12_utils_6_avg, agents_12_utils_7_avg, agents_12_utils_8_avg, agents_12_utils_9_avg, agents_12_utils_10_avg, agents_12_utils_11_avg, agents_12_utils_12_avg))
agents_12_all_utils_avg

#APPROVE 60
agents_12_thresholdA_60 <- agents_12[agents_12[,grep(60,threshold_approve)],]
agents_12_thresholdA_60_D20 <- agents_12_thresholdA_60[agents_12_thresholdA_60[,grep(20,threshold_disapprove)],]
agents_12_thresholdA_60_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdA_60_D20$util_agent_splitted))), nrow=length(agents_12_thresholdA_60_D20$util_agent_splitted), byrow=T))
agents_12_thresholdA_60_D20_utils_avg <- colMeans(agents_12_thresholdA_60_D20_utils)
agents_12_thresholdA_60_D20_utils_avg <- data.frame(y = agents_12_thresholdA_60_D20_utils_avg)
agents_12_thresholdA_60_D20_utils_avg$x <- seq.int(12)
agents_12_thresholdA_60_D20_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdA_60_D20_utils_avg_2$y[1] <- agents_12_thresholdA_60_D20_utils_avg$y[1]
agents_12_thresholdA_60_D20_utils_avg_2$y[2] <- agents_12_thresholdA_60_D20_utils_avg$y[2]
agents_12_thresholdA_60_D20_utils_avg_2$y[3] <- agents_12_thresholdA_60_D20_utils_avg$y[3]
agents_12_thresholdA_60_D20_utils_avg_2$y[4] <- agents_12_thresholdA_60_D20_utils_avg$y[4]
agents_12_thresholdA_60_D20_utils_avg_2$y[5] <- agents_12_thresholdA_60_D20_utils_avg$y[5]
agents_12_thresholdA_60_D20_utils_avg_2$y[6] <- agents_12_thresholdA_60_D20_utils_avg$y[6]
agents_12_thresholdA_60_D20_utils_avg_2$y[7] <- agents_12_thresholdA_60_D20_utils_avg$y[7]
agents_12_thresholdA_60_D20_utils_avg_2$y[8] <- agents_12_thresholdA_60_D20_utils_avg$y[8]
agents_12_thresholdA_60_D20_utils_avg_2$y[9] <- agents_12_thresholdA_60_D20_utils_avg$y[9]
agents_12_thresholdA_60_D20_utils_avg_2$y[10] <- agents_12_thresholdA_60_D20_utils_avg$y[10]
agents_12_thresholdA_60_D20_utils_avg_2$y[11] <- agents_12_thresholdA_60_D20_utils_avg$y[11]
agents_12_thresholdA_60_D20_utils_avg_2$y[12] <- agents_12_thresholdA_60_D20_utils_avg$y[12]

agents_12_thresholdA_60_D30 <- agents_12_thresholdA_60[agents_12_thresholdA_60[,grep(30,threshold_disapprove)],]
agents_12_thresholdA_60_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdA_60_D30$util_agent_splitted))), nrow=length(agents_12_thresholdA_60_D30$util_agent_splitted), byrow=T))
agents_12_thresholdA_60_D30_utils_avg <- colMeans(agents_12_thresholdA_60_D30_utils)
agents_12_thresholdA_60_D30_utils_avg <- data.frame(y = agents_12_thresholdA_60_D30_utils_avg)
agents_12_thresholdA_60_D30_utils_avg$x <- seq.int(12)
agents_12_thresholdA_60_D30_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdA_60_D30_utils_avg_2$y[1] <- agents_12_thresholdA_60_D30_utils_avg$y[1]
agents_12_thresholdA_60_D30_utils_avg_2$y[2] <- agents_12_thresholdA_60_D30_utils_avg$y[2]
agents_12_thresholdA_60_D30_utils_avg_2$y[3] <- agents_12_thresholdA_60_D30_utils_avg$y[3]
agents_12_thresholdA_60_D30_utils_avg_2$y[4] <- agents_12_thresholdA_60_D30_utils_avg$y[4]
agents_12_thresholdA_60_D30_utils_avg_2$y[5] <- agents_12_thresholdA_60_D30_utils_avg$y[5]
agents_12_thresholdA_60_D30_utils_avg_2$y[6] <- agents_12_thresholdA_60_D30_utils_avg$y[6]
agents_12_thresholdA_60_D30_utils_avg_2$y[7] <- agents_12_thresholdA_60_D30_utils_avg$y[7]
agents_12_thresholdA_60_D30_utils_avg_2$y[8] <- agents_12_thresholdA_60_D30_utils_avg$y[8]
agents_12_thresholdA_60_D30_utils_avg_2$y[9] <- agents_12_thresholdA_60_D30_utils_avg$y[9]
agents_12_thresholdA_60_D30_utils_avg_2$y[10] <- agents_12_thresholdA_60_D30_utils_avg$y[10]
agents_12_thresholdA_60_D30_utils_avg_2$y[11] <- agents_12_thresholdA_60_D30_utils_avg$y[11]
agents_12_thresholdA_60_D30_utils_avg_2$y[12] <- agents_12_thresholdA_60_D30_utils_avg$y[12]

agents_12_thresholdA_60_D40 <- agents_12_thresholdA_60[agents_12_thresholdA_60[,grep(40,threshold_disapprove)],]
agents_12_thresholdA_60_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdA_60_D40$util_agent_splitted))), nrow=length(agents_12_thresholdA_60_D40$util_agent_splitted), byrow=T))
agents_12_thresholdA_60_D40_utils_avg <- colMeans(agents_12_thresholdA_60_D40_utils)
agents_12_thresholdA_60_D40_utils_avg <- data.frame(y = agents_12_thresholdA_60_D40_utils_avg)
agents_12_thresholdA_60_D40_utils_avg$x <- seq.int(12)
agents_12_thresholdA_60_D40_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdA_60_D40_utils_avg_2$y[1] <- agents_12_thresholdA_60_D40_utils_avg$y[1]
agents_12_thresholdA_60_D40_utils_avg_2$y[2] <- agents_12_thresholdA_60_D40_utils_avg$y[2]
agents_12_thresholdA_60_D40_utils_avg_2$y[3] <- agents_12_thresholdA_60_D40_utils_avg$y[3]
agents_12_thresholdA_60_D40_utils_avg_2$y[4] <- agents_12_thresholdA_60_D40_utils_avg$y[4]
agents_12_thresholdA_60_D40_utils_avg_2$y[5] <- agents_12_thresholdA_60_D40_utils_avg$y[5]
agents_12_thresholdA_60_D40_utils_avg_2$y[6] <- agents_12_thresholdA_60_D40_utils_avg$y[6]
agents_12_thresholdA_60_D40_utils_avg_2$y[7] <- agents_12_thresholdA_60_D40_utils_avg$y[7]
agents_12_thresholdA_60_D40_utils_avg_2$y[8] <- agents_12_thresholdA_60_D40_utils_avg$y[8]
agents_12_thresholdA_60_D40_utils_avg_2$y[9] <- agents_12_thresholdA_60_D40_utils_avg$y[9]
agents_12_thresholdA_60_D40_utils_avg_2$y[10] <- agents_12_thresholdA_60_D40_utils_avg$y[10]
agents_12_thresholdA_60_D40_utils_avg_2$y[11] <- agents_12_thresholdA_60_D40_utils_avg$y[11]
agents_12_thresholdA_60_D40_utils_avg_2$y[12] <- agents_12_thresholdA_60_D40_utils_avg$y[12]


agents_12_threshold_A60_avg <- data.frame(rbind(agents_12_thresholdA_60_D20_utils_avg_2, agents_12_thresholdA_60_D30_utils_avg_2,agents_12_thresholdA_60_D40_utils_avg_2))
agents_12_threshold_A60_avg_result <- agents_12_threshold_A60_avg[FALSE,]
agents_12_threshold_A60_avg_result <- data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 1))))
agents_12_threshold_A60_avg_result <- rbind(agents_12_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 2)))))
agents_12_threshold_A60_avg_result <- rbind(agents_12_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 3)))))
agents_12_threshold_A60_avg_result <- rbind(agents_12_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 4)))))
agents_12_threshold_A60_avg_result <- rbind(agents_12_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 5)))))
agents_12_threshold_A60_avg_result <- rbind(agents_12_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 6)))))
agents_12_threshold_A60_avg_result <- rbind(agents_12_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 7)))))
agents_12_threshold_A60_avg_result <- rbind(agents_12_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 8)))))
agents_12_threshold_A60_avg_result <- rbind(agents_12_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 9)))))
agents_12_threshold_A60_avg_result <- rbind(agents_12_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 10)))))
agents_12_threshold_A60_avg_result <- rbind(agents_12_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 11)))))
agents_12_threshold_A60_avg_result <- rbind(agents_12_threshold_A60_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A60_avg, x == 12)))))

agents_12_threshold_A60_avg_result

#APPROVE 70
agents_12_thresholdA_70 <- agents_12[agents_12[,grep(70,threshold_approve)],]
agents_12_thresholdA_70_D20 <- agents_12_thresholdA_70[agents_12_thresholdA_70[,grep(20,threshold_disapprove)],]
agents_12_thresholdA_70_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdA_70_D20$util_agent_splitted))), nrow=length(agents_12_thresholdA_70_D20$util_agent_splitted), byrow=T))
agents_12_thresholdA_70_D20_utils_avg <- colMeans(agents_12_thresholdA_70_D20_utils)
agents_12_thresholdA_70_D20_utils_avg <- data.frame(y = agents_12_thresholdA_70_D20_utils_avg)
agents_12_thresholdA_70_D20_utils_avg$x <- seq.int(12)
agents_12_thresholdA_70_D20_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdA_70_D20_utils_avg_2$y[1] <- agents_12_thresholdA_70_D20_utils_avg$y[1]
agents_12_thresholdA_70_D20_utils_avg_2$y[2] <- agents_12_thresholdA_70_D20_utils_avg$y[2]
agents_12_thresholdA_70_D20_utils_avg_2$y[3] <- agents_12_thresholdA_70_D20_utils_avg$y[3]
agents_12_thresholdA_70_D20_utils_avg_2$y[4] <- agents_12_thresholdA_70_D20_utils_avg$y[4]
agents_12_thresholdA_70_D20_utils_avg_2$y[5] <- agents_12_thresholdA_70_D20_utils_avg$y[5]
agents_12_thresholdA_70_D20_utils_avg_2$y[6] <- agents_12_thresholdA_70_D20_utils_avg$y[6]
agents_12_thresholdA_70_D20_utils_avg_2$y[7] <- agents_12_thresholdA_70_D20_utils_avg$y[7]
agents_12_thresholdA_70_D20_utils_avg_2$y[8] <- agents_12_thresholdA_70_D20_utils_avg$y[8]
agents_12_thresholdA_70_D20_utils_avg_2$y[9] <- agents_12_thresholdA_70_D20_utils_avg$y[9]
agents_12_thresholdA_70_D20_utils_avg_2$y[10] <- agents_12_thresholdA_70_D20_utils_avg$y[10]
agents_12_thresholdA_70_D20_utils_avg_2$y[11] <- agents_12_thresholdA_70_D20_utils_avg$y[11]
agents_12_thresholdA_70_D20_utils_avg_2$y[12] <- agents_12_thresholdA_70_D20_utils_avg$y[12]


agents_12_thresholdA_70_D30 <- agents_12_thresholdA_70[agents_12_thresholdA_70[,grep(30,threshold_disapprove)],]
agents_12_thresholdA_70_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdA_70_D30$util_agent_splitted))), nrow=length(agents_12_thresholdA_70_D30$util_agent_splitted), byrow=T))
agents_12_thresholdA_70_D30_utils_avg <- colMeans(agents_12_thresholdA_70_D30_utils)
agents_12_thresholdA_70_D30_utils_avg <- data.frame(y = agents_12_thresholdA_70_D30_utils_avg)
agents_12_thresholdA_70_D30_utils_avg$x <- seq.int(12)
agents_12_thresholdA_70_D30_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdA_70_D30_utils_avg_2$y[1] <- agents_12_thresholdA_70_D30_utils_avg$y[1]
agents_12_thresholdA_70_D30_utils_avg_2$y[2] <- agents_12_thresholdA_70_D30_utils_avg$y[2]
agents_12_thresholdA_70_D30_utils_avg_2$y[3] <- agents_12_thresholdA_70_D30_utils_avg$y[3]
agents_12_thresholdA_70_D30_utils_avg_2$y[4] <- agents_12_thresholdA_70_D30_utils_avg$y[4]
agents_12_thresholdA_70_D30_utils_avg_2$y[5] <- agents_12_thresholdA_70_D30_utils_avg$y[5]
agents_12_thresholdA_70_D30_utils_avg_2$y[6] <- agents_12_thresholdA_70_D30_utils_avg$y[6]
agents_12_thresholdA_70_D30_utils_avg_2$y[7] <- agents_12_thresholdA_70_D30_utils_avg$y[7]
agents_12_thresholdA_70_D30_utils_avg_2$y[8] <- agents_12_thresholdA_70_D30_utils_avg$y[8]
agents_12_thresholdA_70_D30_utils_avg_2$y[9] <- agents_12_thresholdA_70_D30_utils_avg$y[9]
agents_12_thresholdA_70_D30_utils_avg_2$y[10] <- agents_12_thresholdA_70_D30_utils_avg$y[10]
agents_12_thresholdA_70_D30_utils_avg_2$y[11] <- agents_12_thresholdA_70_D30_utils_avg$y[11]
agents_12_thresholdA_70_D30_utils_avg_2$y[12] <- agents_12_thresholdA_70_D30_utils_avg$y[12]


agents_12_thresholdA_70_D40 <- agents_12_thresholdA_70[agents_12_thresholdA_70[,grep(40,threshold_disapprove)],]
agents_12_thresholdA_70_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdA_70_D40$util_agent_splitted))), nrow=length(agents_12_thresholdA_70_D40$util_agent_splitted), byrow=T))
agents_12_thresholdA_70_D40_utils_avg <- colMeans(agents_12_thresholdA_70_D40_utils)
agents_12_thresholdA_70_D40_utils_avg <- data.frame(y = agents_12_thresholdA_70_D40_utils_avg)
agents_12_thresholdA_70_D40_utils_avg$x <- seq.int(12)
agents_12_thresholdA_70_D40_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdA_70_D40_utils_avg_2$y[1] <- agents_12_thresholdA_70_D40_utils_avg$y[1]
agents_12_thresholdA_70_D40_utils_avg_2$y[2] <- agents_12_thresholdA_70_D40_utils_avg$y[2]
agents_12_thresholdA_70_D40_utils_avg_2$y[3] <- agents_12_thresholdA_70_D40_utils_avg$y[3]
agents_12_thresholdA_70_D40_utils_avg_2$y[4] <- agents_12_thresholdA_70_D40_utils_avg$y[4]
agents_12_thresholdA_70_D40_utils_avg_2$y[5] <- agents_12_thresholdA_70_D40_utils_avg$y[5]
agents_12_thresholdA_70_D40_utils_avg_2$y[6] <- agents_12_thresholdA_70_D40_utils_avg$y[6]
agents_12_thresholdA_70_D40_utils_avg_2$y[7] <- agents_12_thresholdA_70_D40_utils_avg$y[7]
agents_12_thresholdA_70_D40_utils_avg_2$y[8] <- agents_12_thresholdA_70_D40_utils_avg$y[8]
agents_12_thresholdA_70_D40_utils_avg_2$y[9] <- agents_12_thresholdA_70_D40_utils_avg$y[9]
agents_12_thresholdA_70_D40_utils_avg_2$y[10] <- agents_12_thresholdA_70_D40_utils_avg$y[10]
agents_12_thresholdA_70_D40_utils_avg_2$y[11] <- agents_12_thresholdA_70_D40_utils_avg$y[11]
agents_12_thresholdA_70_D40_utils_avg_2$y[12] <- agents_12_thresholdA_70_D40_utils_avg$y[12]


agents_12_threshold_A70_avg <- data.frame(rbind(agents_12_thresholdA_70_D20_utils_avg_2, agents_12_thresholdA_70_D30_utils_avg_2,agents_12_thresholdA_70_D40_utils_avg_2))
agents_12_threshold_A70_avg_result <- agents_12_threshold_A70_avg[FALSE,]
agents_12_threshold_A70_avg_result <- data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 1))))
agents_12_threshold_A70_avg_result <- rbind(agents_12_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 2)))))
agents_12_threshold_A70_avg_result <- rbind(agents_12_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 3)))))
agents_12_threshold_A70_avg_result <- rbind(agents_12_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 4)))))
agents_12_threshold_A70_avg_result <- rbind(agents_12_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 5)))))
agents_12_threshold_A70_avg_result <- rbind(agents_12_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 6)))))
agents_12_threshold_A70_avg_result <- rbind(agents_12_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 7)))))
agents_12_threshold_A70_avg_result <- rbind(agents_12_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 8)))))
agents_12_threshold_A70_avg_result <- rbind(agents_12_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 9)))))
agents_12_threshold_A70_avg_result <- rbind(agents_12_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 10)))))
agents_12_threshold_A70_avg_result <- rbind(agents_12_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 11)))))
agents_12_threshold_A70_avg_result <- rbind(agents_12_threshold_A70_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A70_avg, x == 12)))))

agents_12_threshold_A70_avg_result

#APPROVE 80
agents_12_thresholdA_80 <- agents_12[agents_12[,grep(80,threshold_approve)],]
agents_12_thresholdA_80_D20 <- agents_12_thresholdA_80[agents_12_thresholdA_80[,grep(20,threshold_disapprove)],]
agents_12_thresholdA_80_D20_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdA_80_D20$util_agent_splitted))), nrow=length(agents_12_thresholdA_80_D20$util_agent_splitted), byrow=T))
agents_12_thresholdA_80_D20_utils_avg <- colMeans(agents_12_thresholdA_80_D20_utils)
agents_12_thresholdA_80_D20_utils_avg <- data.frame(y = agents_12_thresholdA_80_D20_utils_avg)
agents_12_thresholdA_80_D20_utils_avg$x <- seq.int(12)
agents_12_thresholdA_80_D20_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdA_80_D20_utils_avg_2$y[1] <- agents_12_thresholdA_80_D20_utils_avg$y[1]
agents_12_thresholdA_80_D20_utils_avg_2$y[2] <- agents_12_thresholdA_80_D20_utils_avg$y[2]
agents_12_thresholdA_80_D20_utils_avg_2$y[3] <- agents_12_thresholdA_80_D20_utils_avg$y[3]
agents_12_thresholdA_80_D20_utils_avg_2$y[4] <- agents_12_thresholdA_80_D20_utils_avg$y[4]
agents_12_thresholdA_80_D20_utils_avg_2$y[5] <- agents_12_thresholdA_80_D20_utils_avg$y[5]
agents_12_thresholdA_80_D20_utils_avg_2$y[6] <- agents_12_thresholdA_80_D20_utils_avg$y[6]
agents_12_thresholdA_80_D20_utils_avg_2$y[7] <- agents_12_thresholdA_80_D20_utils_avg$y[7]
agents_12_thresholdA_80_D20_utils_avg_2$y[8] <- agents_12_thresholdA_80_D20_utils_avg$y[8]
agents_12_thresholdA_80_D20_utils_avg_2$y[9] <- agents_12_thresholdA_80_D20_utils_avg$y[9]
agents_12_thresholdA_80_D20_utils_avg_2$y[10] <- agents_12_thresholdA_80_D20_utils_avg$y[10]
agents_12_thresholdA_80_D20_utils_avg_2$y[11] <- agents_12_thresholdA_80_D20_utils_avg$y[11]
agents_12_thresholdA_80_D20_utils_avg_2$y[12] <- agents_12_thresholdA_80_D20_utils_avg$y[12]

agents_12_thresholdA_80_D30 <- agents_12_thresholdA_80[agents_12_thresholdA_80[,grep(30,threshold_disapprove)],]
agents_12_thresholdA_80_D30_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdA_80_D30$util_agent_splitted))), nrow=length(agents_12_thresholdA_80_D30$util_agent_splitted), byrow=T))
agents_12_thresholdA_80_D30_utils_avg <- colMeans(agents_12_thresholdA_80_D30_utils)
agents_12_thresholdA_80_D30_utils_avg <- data.frame(y = agents_12_thresholdA_80_D30_utils_avg)
agents_12_thresholdA_80_D30_utils_avg$x <- seq.int(12)
agents_12_thresholdA_80_D30_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdA_80_D30_utils_avg_2$y[1] <- agents_12_thresholdA_80_D30_utils_avg$y[1]
agents_12_thresholdA_80_D30_utils_avg_2$y[2] <- agents_12_thresholdA_80_D30_utils_avg$y[2]
agents_12_thresholdA_80_D30_utils_avg_2$y[3] <- agents_12_thresholdA_80_D30_utils_avg$y[3]
agents_12_thresholdA_80_D30_utils_avg_2$y[4] <- agents_12_thresholdA_80_D30_utils_avg$y[4]
agents_12_thresholdA_80_D30_utils_avg_2$y[5] <- agents_12_thresholdA_80_D30_utils_avg$y[5]
agents_12_thresholdA_80_D30_utils_avg_2$y[6] <- agents_12_thresholdA_80_D30_utils_avg$y[6]
agents_12_thresholdA_80_D30_utils_avg_2$y[7] <- agents_12_thresholdA_80_D30_utils_avg$y[7]
agents_12_thresholdA_80_D30_utils_avg_2$y[8] <- agents_12_thresholdA_80_D30_utils_avg$y[8]
agents_12_thresholdA_80_D30_utils_avg_2$y[9] <- agents_12_thresholdA_80_D30_utils_avg$y[9]
agents_12_thresholdA_80_D30_utils_avg_2$y[10] <- agents_12_thresholdA_80_D30_utils_avg$y[10]
agents_12_thresholdA_80_D30_utils_avg_2$y[11] <- agents_12_thresholdA_80_D30_utils_avg$y[11]
agents_12_thresholdA_80_D30_utils_avg_2$y[12] <- agents_12_thresholdA_80_D30_utils_avg$y[12]


agents_12_thresholdA_80_D40 <- agents_12_thresholdA_80[agents_12_thresholdA_80[,grep(40,threshold_disapprove)],]
agents_12_thresholdA_80_D40_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdA_80_D40$util_agent_splitted))), nrow=length(agents_12_thresholdA_80_D40$util_agent_splitted), byrow=T))
agents_12_thresholdA_80_D40_utils_avg <- colMeans(agents_12_thresholdA_80_D40_utils)
agents_12_thresholdA_80_D40_utils_avg <- data.frame(y = agents_12_thresholdA_80_D40_utils_avg)
agents_12_thresholdA_80_D40_utils_avg$x <- seq.int(12)
agents_12_thresholdA_80_D40_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdA_80_D40_utils_avg_2$y[1] <- agents_12_thresholdA_80_D40_utils_avg$y[1]
agents_12_thresholdA_80_D40_utils_avg_2$y[2] <- agents_12_thresholdA_80_D40_utils_avg$y[2]
agents_12_thresholdA_80_D40_utils_avg_2$y[3] <- agents_12_thresholdA_80_D40_utils_avg$y[3]
agents_12_thresholdA_80_D40_utils_avg_2$y[4] <- agents_12_thresholdA_80_D40_utils_avg$y[4]
agents_12_thresholdA_80_D40_utils_avg_2$y[5] <- agents_12_thresholdA_80_D40_utils_avg$y[5]
agents_12_thresholdA_80_D40_utils_avg_2$y[6] <- agents_12_thresholdA_80_D40_utils_avg$y[6]
agents_12_thresholdA_80_D40_utils_avg_2$y[7] <- agents_12_thresholdA_80_D40_utils_avg$y[7]
agents_12_thresholdA_80_D40_utils_avg_2$y[8] <- agents_12_thresholdA_80_D40_utils_avg$y[8]
agents_12_thresholdA_80_D40_utils_avg_2$y[9] <- agents_12_thresholdA_80_D40_utils_avg$y[9]
agents_12_thresholdA_80_D40_utils_avg_2$y[10] <- agents_12_thresholdA_80_D40_utils_avg$y[10]
agents_12_thresholdA_80_D40_utils_avg_2$y[11] <- agents_12_thresholdA_80_D40_utils_avg$y[11]
agents_12_thresholdA_80_D40_utils_avg_2$y[12] <- agents_12_thresholdA_80_D40_utils_avg$y[12]

agents_12_threshold_A80_avg <- data.frame(rbind(agents_12_thresholdA_80_D20_utils_avg_2, agents_12_thresholdA_80_D30_utils_avg_2,agents_12_thresholdA_80_D40_utils_avg_2))
agents_12_threshold_A80_avg_result <- agents_12_threshold_A80_avg[FALSE,]
agents_12_threshold_A80_avg_result <- data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 1))))
agents_12_threshold_A80_avg_result <- rbind(agents_12_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 2)))))
agents_12_threshold_A80_avg_result <- rbind(agents_12_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 3)))))
agents_12_threshold_A80_avg_result <- rbind(agents_12_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 4)))))
agents_12_threshold_A80_avg_result <- rbind(agents_12_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 5)))))
agents_12_threshold_A80_avg_result <- rbind(agents_12_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 6)))))
agents_12_threshold_A80_avg_result <- rbind(agents_12_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 7)))))
agents_12_threshold_A80_avg_result <- rbind(agents_12_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 8)))))
agents_12_threshold_A80_avg_result <- rbind(agents_12_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 9)))))
agents_12_threshold_A80_avg_result <- rbind(agents_12_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 10)))))
agents_12_threshold_A80_avg_result <- rbind(agents_12_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 11)))))
agents_12_threshold_A80_avg_result <- rbind(agents_12_threshold_A80_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_A80_avg, x == 12)))))

agents_12_threshold_A80_avg_result

#DISSAPROVE 20
agents_12_thresholdD_20 <- agents_12[agents_12[,grep(20,threshold_disapprove)],]
agents_12_thresholdD_20_A60 <- agents_12_thresholdD_20[agents_12_thresholdD_20[,grep(60,threshold_approve)],]
agents_12_thresholdD_20_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdD_20_A60$util_agent_splitted))), nrow=length(agents_12_thresholdD_20_A60$util_agent_splitted), byrow=T))
agents_12_thresholdD_20_A60_utils_avg <- colMeans(agents_12_thresholdD_20_A60_utils)
agents_12_thresholdD_20_A60_utils_avg <- data.frame(y = agents_12_thresholdD_20_A60_utils_avg)
agents_12_thresholdD_20_A60_utils_avg$x <- seq.int(12)
agents_12_thresholdD_20_A60_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdD_20_A60_utils_avg_2$y[1] <- agents_12_thresholdD_20_A60_utils_avg$y[1]
agents_12_thresholdD_20_A60_utils_avg_2$y[2] <- agents_12_thresholdD_20_A60_utils_avg$y[2]
agents_12_thresholdD_20_A60_utils_avg_2$y[3] <- agents_12_thresholdD_20_A60_utils_avg$y[3]
agents_12_thresholdD_20_A60_utils_avg_2$y[4] <- agents_12_thresholdD_20_A60_utils_avg$y[4]
agents_12_thresholdD_20_A60_utils_avg_2$y[5] <- agents_12_thresholdD_20_A60_utils_avg$y[5]
agents_12_thresholdD_20_A60_utils_avg_2$y[6] <- agents_12_thresholdD_20_A60_utils_avg$y[6]
agents_12_thresholdD_20_A60_utils_avg_2$y[7] <- agents_12_thresholdD_20_A60_utils_avg$y[7]
agents_12_thresholdD_20_A60_utils_avg_2$y[8] <- agents_12_thresholdD_20_A60_utils_avg$y[8]
agents_12_thresholdD_20_A60_utils_avg_2$y[9] <- agents_12_thresholdD_20_A60_utils_avg$y[9]
agents_12_thresholdD_20_A60_utils_avg_2$y[10] <- agents_12_thresholdD_20_A60_utils_avg$y[10]
agents_12_thresholdD_20_A60_utils_avg_2$y[11] <- agents_12_thresholdD_20_A60_utils_avg$y[11]
agents_12_thresholdD_20_A60_utils_avg_2$y[12] <- agents_12_thresholdD_20_A60_utils_avg$y[12]


agents_12_thresholdD_20_A70 <- agents_12_thresholdD_20[agents_12_thresholdD_20[,grep(70,threshold_approve)],]
agents_12_thresholdD_20_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdD_20_A70$util_agent_splitted))), nrow=length(agents_12_thresholdD_20_A70$util_agent_splitted), byrow=T))
agents_12_thresholdD_20_A70_utils_avg <- colMeans(agents_12_thresholdD_20_A70_utils)
agents_12_thresholdD_20_A70_utils_avg <- data.frame(y = agents_12_thresholdD_20_A70_utils_avg)
agents_12_thresholdD_20_A70_utils_avg$x <- seq.int(12)
agents_12_thresholdD_20_A70_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdD_20_A70_utils_avg_2$y[1] <- agents_12_thresholdD_20_A70_utils_avg$y[1]
agents_12_thresholdD_20_A70_utils_avg_2$y[2] <- agents_12_thresholdD_20_A70_utils_avg$y[2]
agents_12_thresholdD_20_A70_utils_avg_2$y[3] <- agents_12_thresholdD_20_A70_utils_avg$y[3]
agents_12_thresholdD_20_A70_utils_avg_2$y[4] <- agents_12_thresholdD_20_A70_utils_avg$y[4]
agents_12_thresholdD_20_A70_utils_avg_2$y[5] <- agents_12_thresholdD_20_A70_utils_avg$y[5]
agents_12_thresholdD_20_A70_utils_avg_2$y[6] <- agents_12_thresholdD_20_A70_utils_avg$y[6]
agents_12_thresholdD_20_A70_utils_avg_2$y[7] <- agents_12_thresholdD_20_A70_utils_avg$y[7]
agents_12_thresholdD_20_A70_utils_avg_2$y[8] <- agents_12_thresholdD_20_A70_utils_avg$y[8]
agents_12_thresholdD_20_A70_utils_avg_2$y[9] <- agents_12_thresholdD_20_A70_utils_avg$y[9]
agents_12_thresholdD_20_A70_utils_avg_2$y[10] <- agents_12_thresholdD_20_A70_utils_avg$y[10]
agents_12_thresholdD_20_A70_utils_avg_2$y[11] <- agents_12_thresholdD_20_A70_utils_avg$y[11]
agents_12_thresholdD_20_A70_utils_avg_2$y[12] <- agents_12_thresholdD_20_A70_utils_avg$y[12]


agents_12_thresholdD_20_A80 <- agents_12_thresholdD_20[agents_12_thresholdD_20[,grep(80,threshold_approve)],]
agents_12_thresholdD_20_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdD_20_A80$util_agent_splitted))), nrow=length(agents_12_thresholdD_20_A80$util_agent_splitted), byrow=T))
agents_12_thresholdD_20_A80_utils_avg <- colMeans(agents_12_thresholdD_20_A80_utils)
agents_12_thresholdD_20_A80_utils_avg <- data.frame(y = agents_12_thresholdD_20_A80_utils_avg)
agents_12_thresholdD_20_A80_utils_avg$x <- seq.int(12)
agents_12_thresholdD_20_A80_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdD_20_A80_utils_avg_2$y[1] <- agents_12_thresholdD_20_A80_utils_avg$y[1]
agents_12_thresholdD_20_A80_utils_avg_2$y[2] <- agents_12_thresholdD_20_A80_utils_avg$y[2]
agents_12_thresholdD_20_A80_utils_avg_2$y[3] <- agents_12_thresholdD_20_A80_utils_avg$y[3]
agents_12_thresholdD_20_A80_utils_avg_2$y[4] <- agents_12_thresholdD_20_A80_utils_avg$y[4]
agents_12_thresholdD_20_A80_utils_avg_2$y[5] <- agents_12_thresholdD_20_A80_utils_avg$y[5]
agents_12_thresholdD_20_A80_utils_avg_2$y[6] <- agents_12_thresholdD_20_A80_utils_avg$y[6]
agents_12_thresholdD_20_A80_utils_avg_2$y[7] <- agents_12_thresholdD_20_A80_utils_avg$y[7]
agents_12_thresholdD_20_A80_utils_avg_2$y[8] <- agents_12_thresholdD_20_A80_utils_avg$y[8]
agents_12_thresholdD_20_A80_utils_avg_2$y[9] <- agents_12_thresholdD_20_A80_utils_avg$y[9]
agents_12_thresholdD_20_A80_utils_avg_2$y[10] <- agents_12_thresholdD_20_A80_utils_avg$y[10]
agents_12_thresholdD_20_A80_utils_avg_2$y[11] <- agents_12_thresholdD_20_A80_utils_avg$y[11]
agents_12_thresholdD_20_A80_utils_avg_2$y[12] <- agents_12_thresholdD_20_A80_utils_avg$y[12]


agents_12_threshold_D20_avg <- data.frame(rbind(agents_12_thresholdD_20_A60_utils_avg_2, agents_12_thresholdD_20_A70_utils_avg_2,agents_12_thresholdD_20_A80_utils_avg_2))
agents_12_threshold_D20_avg_result <- agents_12_threshold_D20_avg[FALSE,]
agents_12_threshold_D20_avg_result <- data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 1))))
agents_12_threshold_D20_avg_result <- rbind(agents_12_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 2)))))
agents_12_threshold_D20_avg_result <- rbind(agents_12_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 3)))))
agents_12_threshold_D20_avg_result <- rbind(agents_12_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 4)))))
agents_12_threshold_D20_avg_result <- rbind(agents_12_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 5)))))
agents_12_threshold_D20_avg_result <- rbind(agents_12_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 6)))))
agents_12_threshold_D20_avg_result <- rbind(agents_12_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 7)))))
agents_12_threshold_D20_avg_result <- rbind(agents_12_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 8)))))
agents_12_threshold_D20_avg_result <- rbind(agents_12_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 9)))))
agents_12_threshold_D20_avg_result <- rbind(agents_12_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 10)))))
agents_12_threshold_D20_avg_result <- rbind(agents_12_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 11)))))
agents_12_threshold_D20_avg_result <- rbind(agents_12_threshold_D20_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D20_avg, x == 12)))))

agents_12_threshold_D20_avg_result

#disapprove 30
agents_12_thresholdD_30 <- agents_12[agents_12[,grep(30,threshold_disapprove)],]
agents_12_thresholdD_30_D20 <- agents_12_thresholdD_30[agents_12_thresholdD_30[,grep(60,threshold_approve)],]
agents_12_thresholdD_30_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdD_30_D20$util_agent_splitted))), nrow=length(agents_12_thresholdD_30_D20$util_agent_splitted), byrow=T))
agents_12_thresholdD_30_A60_utils_avg <- colMeans(agents_12_thresholdD_30_A60_utils)
agents_12_thresholdD_30_A60_utils_avg <- data.frame(y = agents_12_thresholdD_30_A60_utils_avg)
agents_12_thresholdD_30_A60_utils_avg$x <- seq.int(12)
agents_12_thresholdD_30_A60_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdD_30_A60_utils_avg_2$y[1] <- agents_12_thresholdD_30_A60_utils_avg$y[1]
agents_12_thresholdD_30_A60_utils_avg_2$y[2] <- agents_12_thresholdD_30_A60_utils_avg$y[2]
agents_12_thresholdD_30_A60_utils_avg_2$y[3] <- agents_12_thresholdD_30_A60_utils_avg$y[3]
agents_12_thresholdD_30_A60_utils_avg_2$y[4] <- agents_12_thresholdD_30_A60_utils_avg$y[4]
agents_12_thresholdD_30_A60_utils_avg_2$y[5] <- agents_12_thresholdD_30_A60_utils_avg$y[5]
agents_12_thresholdD_30_A60_utils_avg_2$y[6] <- agents_12_thresholdD_30_A60_utils_avg$y[6]
agents_12_thresholdD_30_A60_utils_avg_2$y[7] <- agents_12_thresholdD_30_A60_utils_avg$y[7]
agents_12_thresholdD_30_A60_utils_avg_2$y[8] <- agents_12_thresholdD_30_A60_utils_avg$y[8]
agents_12_thresholdD_30_A60_utils_avg_2$y[9] <- agents_12_thresholdD_30_A60_utils_avg$y[9]
agents_12_thresholdD_30_A60_utils_avg_2$y[10] <- agents_12_thresholdD_30_A60_utils_avg$y[10]
agents_12_thresholdD_30_A60_utils_avg_2$y[11] <- agents_12_thresholdD_30_A60_utils_avg$y[11]
agents_12_thresholdD_30_A60_utils_avg_2$y[12] <- agents_12_thresholdD_30_A60_utils_avg$y[12]


agents_12_thresholdD_30_A70 <- agents_12_thresholdD_30[agents_12_thresholdD_30[,grep(70,threshold_approve)],]
agents_12_thresholdD_30_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdD_30_A70$util_agent_splitted))), nrow=length(agents_12_thresholdD_30_A70$util_agent_splitted), byrow=T))
agents_12_thresholdD_30_A70_utils_avg <- colMeans(agents_12_thresholdD_30_A70_utils)
agents_12_thresholdD_30_A70_utils_avg <- data.frame(y = agents_12_thresholdD_30_A70_utils_avg)
agents_12_thresholdD_30_A70_utils_avg$x <- seq.int(12)
agents_12_thresholdD_30_A70_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdD_30_A70_utils_avg_2$y[1] <- agents_12_thresholdD_30_A70_utils_avg$y[1]
agents_12_thresholdD_30_A70_utils_avg_2$y[2] <- agents_12_thresholdD_30_A70_utils_avg$y[2]
agents_12_thresholdD_30_A70_utils_avg_2$y[3] <- agents_12_thresholdD_30_A70_utils_avg$y[3]
agents_12_thresholdD_30_A70_utils_avg_2$y[4] <- agents_12_thresholdD_30_A70_utils_avg$y[4]
agents_12_thresholdD_30_A70_utils_avg_2$y[5] <- agents_12_thresholdD_30_A70_utils_avg$y[5]
agents_12_thresholdD_30_A70_utils_avg_2$y[6] <- agents_12_thresholdD_30_A70_utils_avg$y[6]
agents_12_thresholdD_30_A70_utils_avg_2$y[7] <- agents_12_thresholdD_30_A70_utils_avg$y[7]
agents_12_thresholdD_30_A70_utils_avg_2$y[8] <- agents_12_thresholdD_30_A70_utils_avg$y[8]
agents_12_thresholdD_30_A70_utils_avg_2$y[9] <- agents_12_thresholdD_30_A70_utils_avg$y[9]
agents_12_thresholdD_30_A70_utils_avg_2$y[10] <- agents_12_thresholdD_30_A70_utils_avg$y[10]
agents_12_thresholdD_30_A70_utils_avg_2$y[11] <- agents_12_thresholdD_30_A70_utils_avg$y[11]
agents_12_thresholdD_30_A70_utils_avg_2$y[12] <- agents_12_thresholdD_30_A70_utils_avg$y[12]



agents_12_thresholdD_30_A80 <- agents_12_thresholdD_30[agents_12_thresholdD_30[,grep(80,threshold_approve)],]
agents_12_thresholdD_30_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdD_30_A80$util_agent_splitted))), nrow=length(agents_12_thresholdD_30_A80$util_agent_splitted), byrow=T))
agents_12_thresholdD_30_A80_utils_avg <- colMeans(agents_12_thresholdD_30_A80_utils)
agents_12_thresholdD_30_A80_utils_avg <- data.frame(y = agents_12_thresholdD_30_A80_utils_avg)
agents_12_thresholdD_30_A80_utils_avg$x <- seq.int(12)
agents_12_thresholdD_30_A80_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdD_30_A80_utils_avg_2$y[1] <- agents_12_thresholdD_30_A80_utils_avg$y[1]
agents_12_thresholdD_30_A80_utils_avg_2$y[2] <- agents_12_thresholdD_30_A80_utils_avg$y[2]
agents_12_thresholdD_30_A80_utils_avg_2$y[3] <- agents_12_thresholdD_30_A80_utils_avg$y[3]
agents_12_thresholdD_30_A80_utils_avg_2$y[4] <- agents_12_thresholdD_30_A80_utils_avg$y[4]
agents_12_thresholdD_30_A80_utils_avg_2$y[5] <- agents_12_thresholdD_30_A80_utils_avg$y[5]
agents_12_thresholdD_30_A80_utils_avg_2$y[6] <- agents_12_thresholdD_30_A80_utils_avg$y[6]
agents_12_thresholdD_30_A80_utils_avg_2$y[7] <- agents_12_thresholdD_30_A80_utils_avg$y[7]
agents_12_thresholdD_30_A80_utils_avg_2$y[8] <- agents_12_thresholdD_30_A80_utils_avg$y[8]
agents_12_thresholdD_30_A80_utils_avg_2$y[9] <- agents_12_thresholdD_30_A80_utils_avg$y[9]
agents_12_thresholdD_30_A80_utils_avg_2$y[10] <- agents_12_thresholdD_30_A80_utils_avg$y[10]
agents_12_thresholdD_30_A80_utils_avg_2$y[11] <- agents_12_thresholdD_30_A80_utils_avg$y[11]
agents_12_thresholdD_30_A80_utils_avg_2$y[12] <- agents_12_thresholdD_30_A80_utils_avg$y[12]


agents_12_threshold_D30_avg <- data.frame(rbind(agents_12_thresholdD_30_A60_utils_avg_2, agents_12_thresholdD_30_A70_utils_avg_2,agents_12_thresholdD_30_A80_utils_avg_2))
agents_12_threshold_D30_avg_result <- agents_12_threshold_D30_avg[FALSE,]
agents_12_threshold_D30_avg_result <- data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 1))))
agents_12_threshold_D30_avg_result <- rbind(agents_12_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 2)))))
agents_12_threshold_D30_avg_result <- rbind(agents_12_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 3)))))
agents_12_threshold_D30_avg_result <- rbind(agents_12_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 4)))))
agents_12_threshold_D30_avg_result <- rbind(agents_12_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 5)))))
agents_12_threshold_D30_avg_result <- rbind(agents_12_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 6)))))
agents_12_threshold_D30_avg_result <- rbind(agents_12_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 7)))))
agents_12_threshold_D30_avg_result <- rbind(agents_12_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 8)))))
agents_12_threshold_D30_avg_result <- rbind(agents_12_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 9)))))
agents_12_threshold_D30_avg_result <- rbind(agents_12_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 10)))))
agents_12_threshold_D30_avg_result <- rbind(agents_12_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 11)))))
agents_12_threshold_D30_avg_result <- rbind(agents_12_threshold_D30_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D30_avg, x == 12)))))

agents_12_threshold_D30_avg_result

#disapprove 40
agents_12_thresholdD_40 <- agents_12[agents_12[,grep(40,threshold_disapprove)],]
agents_12_thresholdD_40_D20 <- agents_12_thresholdD_40[agents_12_thresholdD_40[,grep(60,threshold_approve)],]
agents_12_thresholdD_40_A60_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdD_40_D20$util_agent_splitted))), nrow=length(agents_12_thresholdD_40_D20$util_agent_splitted), byrow=T))
agents_12_thresholdD_40_A60_utils_avg <- colMeans(agents_12_thresholdD_40_A60_utils)
agents_12_thresholdD_40_A60_utils_avg <- data.frame(y = agents_12_thresholdD_40_A60_utils_avg)
agents_12_thresholdD_40_A60_utils_avg$x <- seq.int(12)
agents_12_thresholdD_40_A60_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdD_40_A60_utils_avg_2$y[1] <- agents_12_thresholdD_40_A60_utils_avg$y[1]
agents_12_thresholdD_40_A60_utils_avg_2$y[2] <- agents_12_thresholdD_40_A60_utils_avg$y[2]
agents_12_thresholdD_40_A60_utils_avg_2$y[3] <- agents_12_thresholdD_40_A60_utils_avg$y[3]
agents_12_thresholdD_40_A60_utils_avg_2$y[4] <- agents_12_thresholdD_40_A60_utils_avg$y[4]
agents_12_thresholdD_40_A60_utils_avg_2$y[5] <- agents_12_thresholdD_40_A60_utils_avg$y[5]
agents_12_thresholdD_40_A60_utils_avg_2$y[6] <- agents_12_thresholdD_40_A60_utils_avg$y[6]
agents_12_thresholdD_40_A60_utils_avg_2$y[7] <- agents_12_thresholdD_40_A60_utils_avg$y[7]
agents_12_thresholdD_40_A60_utils_avg_2$y[8] <- agents_12_thresholdD_40_A60_utils_avg$y[8]
agents_12_thresholdD_40_A60_utils_avg_2$y[9] <- agents_12_thresholdD_40_A60_utils_avg$y[9]
agents_12_thresholdD_40_A60_utils_avg_2$y[10] <- agents_12_thresholdD_40_A60_utils_avg$y[10]
agents_12_thresholdD_40_A60_utils_avg_2$y[11] <- agents_12_thresholdD_40_A60_utils_avg$y[11]
agents_12_thresholdD_40_A60_utils_avg_2$y[12] <- agents_12_thresholdD_40_A60_utils_avg$y[12]


agents_12_thresholdD_40_A70 <- agents_12_thresholdD_40[agents_12_thresholdD_40[,grep(70,threshold_approve)],]
agents_12_thresholdD_40_A70_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdD_40_A70$util_agent_splitted))), nrow=length(agents_12_thresholdD_40_A70$util_agent_splitted), byrow=T))
agents_12_thresholdD_40_A70_utils_avg <- colMeans(agents_12_thresholdD_40_A70_utils)
agents_12_thresholdD_40_A70_utils_avg <- data.frame(y = agents_12_thresholdD_40_A70_utils_avg)
agents_12_thresholdD_40_A70_utils_avg$x <- seq.int(12)
agents_12_thresholdD_40_A70_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdD_40_A70_utils_avg_2$y[1] <- agents_12_thresholdD_40_A70_utils_avg$y[1]
agents_12_thresholdD_40_A70_utils_avg_2$y[2] <- agents_12_thresholdD_40_A70_utils_avg$y[2]
agents_12_thresholdD_40_A70_utils_avg_2$y[3] <- agents_12_thresholdD_40_A70_utils_avg$y[3]
agents_12_thresholdD_40_A70_utils_avg_2$y[4] <- agents_12_thresholdD_40_A70_utils_avg$y[4]
agents_12_thresholdD_40_A70_utils_avg_2$y[5] <- agents_12_thresholdD_40_A70_utils_avg$y[5]
agents_12_thresholdD_40_A70_utils_avg_2$y[6] <- agents_12_thresholdD_40_A70_utils_avg$y[6]
agents_12_thresholdD_40_A70_utils_avg_2$y[7] <- agents_12_thresholdD_40_A70_utils_avg$y[7]
agents_12_thresholdD_40_A70_utils_avg_2$y[8] <- agents_12_thresholdD_40_A70_utils_avg$y[8]
agents_12_thresholdD_40_A70_utils_avg_2$y[9] <- agents_12_thresholdD_40_A70_utils_avg$y[9]
agents_12_thresholdD_40_A70_utils_avg_2$y[10] <- agents_12_thresholdD_40_A70_utils_avg$y[10]
agents_12_thresholdD_40_A70_utils_avg_2$y[11] <- agents_12_thresholdD_40_A70_utils_avg$y[11]
agents_12_thresholdD_40_A70_utils_avg_2$y[12] <- agents_12_thresholdD_40_A70_utils_avg$y[12]



agents_12_thresholdD_40_A80 <- agents_12_thresholdD_40[agents_12_thresholdD_40[,grep(80,threshold_approve)],]
agents_12_thresholdD_40_A80_utils <- data.frame(matrix(as.numeric(paste(unlist(agents_12_thresholdD_40_A80$util_agent_splitted))), nrow=length(agents_12_thresholdD_40_A80$util_agent_splitted), byrow=T))
agents_12_thresholdD_40_A80_utils_avg <- colMeans(agents_12_thresholdD_40_A80_utils)
agents_12_thresholdD_40_A80_utils_avg <- data.frame(y = agents_12_thresholdD_40_A80_utils_avg)
agents_12_thresholdD_40_A80_utils_avg$x <- seq.int(12)
agents_12_thresholdD_40_A80_utils_avg_2 <- agents_12_all_utils_avg
agents_12_thresholdD_40_A80_utils_avg_2$y[1] <- agents_12_thresholdD_40_A80_utils_avg$y[1]
agents_12_thresholdD_40_A80_utils_avg_2$y[2] <- agents_12_thresholdD_40_A80_utils_avg$y[2]
agents_12_thresholdD_40_A80_utils_avg_2$y[3] <- agents_12_thresholdD_40_A80_utils_avg$y[3]
agents_12_thresholdD_40_A80_utils_avg_2$y[4] <- agents_12_thresholdD_40_A80_utils_avg$y[4]
agents_12_thresholdD_40_A80_utils_avg_2$y[5] <- agents_12_thresholdD_40_A80_utils_avg$y[5]
agents_12_thresholdD_40_A80_utils_avg_2$y[6] <- agents_12_thresholdD_40_A80_utils_avg$y[6]
agents_12_thresholdD_40_A80_utils_avg_2$y[7] <- agents_12_thresholdD_40_A80_utils_avg$y[7]
agents_12_thresholdD_40_A80_utils_avg_2$y[8] <- agents_12_thresholdD_40_A80_utils_avg$y[8]
agents_12_thresholdD_40_A80_utils_avg_2$y[9] <- agents_12_thresholdD_40_A80_utils_avg$y[9]
agents_12_thresholdD_40_A80_utils_avg_2$y[10] <- agents_12_thresholdD_40_A80_utils_avg$y[10]
agents_12_thresholdD_40_A80_utils_avg_2$y[11] <- agents_12_thresholdD_40_A80_utils_avg$y[11]
agents_12_thresholdD_40_A80_utils_avg_2$y[12] <- agents_12_thresholdD_40_A80_utils_avg$y[12]


agents_12_threshold_D40_avg <- data.frame(rbind(agents_12_thresholdD_40_A60_utils_avg_2, agents_12_thresholdD_40_A70_utils_avg_2,agents_12_thresholdD_40_A80_utils_avg_2))
agents_12_threshold_D40_avg_result <- agents_12_threshold_D40_avg[FALSE,]
agents_12_threshold_D40_avg_result <- data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 1))))
agents_12_threshold_D40_avg_result <- rbind(agents_12_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 2)))))
agents_12_threshold_D40_avg_result <- rbind(agents_12_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 3)))))
agents_12_threshold_D40_avg_result <- rbind(agents_12_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 4)))))
agents_12_threshold_D40_avg_result <- rbind(agents_12_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 5)))))
agents_12_threshold_D40_avg_result <- rbind(agents_12_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 6)))))
agents_12_threshold_D40_avg_result <- rbind(agents_12_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 7)))))
agents_12_threshold_D40_avg_result <- rbind(agents_12_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 8)))))
agents_12_threshold_D40_avg_result <- rbind(agents_12_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 9)))))
agents_12_threshold_D40_avg_result <- rbind(agents_12_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 10)))))
agents_12_threshold_D40_avg_result <- rbind(agents_12_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 11)))))
agents_12_threshold_D40_avg_result <- rbind(agents_12_threshold_D40_avg_result, data.frame(t(colMeans(subset(agents_12_threshold_D40_avg, x == 12)))))

agents_12_threshold_D40_avg_result

integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
ggplot(data=agents_12_all_utils, aes(x=x, y=y, group=row, scale_size(guide = "none"))) +
  geom_line(linetype="solid", color="black", alpha=0.05) +
  #xlim(1, 9) +
  ylim(0.55, 0.82) + labs(x = "position in the game", y="utily") +
  ggtitle("12 Agents") +
  #geom_line(data=agents_12_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) +
  scale_x_discrete(name ="position in the game", limits=c("1","2","3","4","5","6","7","8","9","10","11","12")) +
  geom_line(data=agents_12_threshold_A60_avg_result,aes(x=x,y=y),color="#00de3b",size=1,alpha=0.8) +
  geom_line(data=agents_12_threshold_A70_avg_result,aes(x=x,y=y),color="#00b831",size=1,alpha=0.8) +
  geom_line(data=agents_12_threshold_A80_avg_result,aes(x=x,y=y),color="#008223",size=1,alpha=0.8) +
  geom_line(data=agents_12_threshold_D20_avg_result,aes(x=x,y=y),color="#e33900",size=1,alpha=0.8) +
  geom_line(data=agents_12_threshold_D30_avg_result,aes(x=x,y=y),color="#ad2c00",size=1,alpha=0.8) +
  geom_line(data=agents_12_threshold_D40_avg_result,aes(x=x,y=y),color="#701d00",size=1,alpha=0.8) +
  #scale_x_continuous(breaks = int_breaks) +
  #geom_line(data=agents_12_thresholdA_60_D20_utils_avg_2,aes(x=x,y=y),color="#0077ff",size=1,alpha=0.8) +
  #geom_line(data=agents_12_thresholdA_60_D30_utils_avg_2,aes(x=x,y=y),color="#0053b3",size=1,alpha=0.8) +
  #geom_line(data=agents_12_thresholdA_60_D40_utils_avg_2,aes(x=x,y=y),color="#3b96ff",size=1,alpha=0.8) +
  #geom_line(data=agents_12_thresholdA_70_D20_utils_avg_2,aes(x=x,y=y),color="#00ff00",size=1,alpha=0.8) +
  #geom_line(data=agents_12_thresholdA_70_D30_utils_avg_2,aes(x=x,y=y),color="#00a800",size=1,alpha=0.8) +
  #geom_line(data=agents_12_thresholdA_70_D40_utils_avg_2,aes(x=x,y=y),color="#80ff80",size=1,alpha=0.8) +
  #geom_line(data=agents_12_thresholdA_80_D20_utils_avg_2,aes(x=x,y=y),color="#ff0000",size=1,alpha=0.8) +
  #geom_line(data=agents_12_thresholdA_80_D30_utils_avg_2,aes(x=x,y=y),color="#8c0000",size=1,alpha=0.8) +
  #geom_line(data=agents_12_thresholdA_80_D40_utils_avg_2,aes(x=x,y=y),color="#ff6969",size=1,alpha=0.8) +
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))




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
  ylim(0.55, 0.82) + labs(x = "position in the game", y="utily") +
  ggtitle("16 Agents") +
  scale_x_discrete(name ="position in the game", limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")) +
  #geom_line(data=agents_16_all_utils_avg,aes(x=x,y=y),color="red",size=2,alpha=0.7) +
  geom_line(data=agents_16_threshold_A60_avg_result,aes(x=x,y=y),color="#00de3b",size=1,alpha=0.8) +
  geom_line(data=agents_16_threshold_A70_avg_result,aes(x=x,y=y),color="#00b831",size=1,alpha=0.8) +
  geom_line(data=agents_16_threshold_A80_avg_result,aes(x=x,y=y),color="#008223",size=1,alpha=0.8) +
  geom_line(data=agents_16_threshold_D20_avg_result,aes(x=x,y=y),color="#e33900",size=1,alpha=0.8) +
  geom_line(data=agents_16_threshold_D30_avg_result,aes(x=x,y=y),color="#ad2c00",size=1,alpha=0.8) +
  geom_line(data=agents_16_threshold_D40_avg_result,aes(x=x,y=y),color="#701d00",size=1,alpha=0.8) +
  #scale_x_continuous(breaks = integer_breaks) +
  #geom_line(data=agents_16_thresholdA_60_D20_utils_avg_2,aes(x=x,y=y),color="#0077ff",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_60_D30_utils_avg_2,aes(x=x,y=y),color="#0053b3",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_60_D40_utils_avg_2,aes(x=x,y=y),color="#3b96ff",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_70_D20_utils_avg_2,aes(x=x,y=y),color="#00ff00",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_70_D30_utils_avg_2,aes(x=x,y=y),color="#00a800",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_70_D40_utils_avg_2,aes(x=x,y=y),color="#80ff80",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_80_D20_utils_avg_2,aes(x=x,y=y),color="#ff0000",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_80_D30_utils_avg_2,aes(x=x,y=y),color="#8c0000",size=1,alpha=0.8) +
  #geom_line(data=agents_16_thresholdA_80_D40_utils_avg_2,aes(x=x,y=y),color="#ff6969",size=1,alpha=0.8) +
  theme(legend.position = c(0, 1),legend.justification = c(0, 1))
  