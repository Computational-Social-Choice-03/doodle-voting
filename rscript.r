library(rstudioapi) # load it
library(stringr) 

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))
library("data.table")
library("bit64")
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
agents_3_utils_1 <- data.frame(y=agents_3_utils$X1)
agents_3_utils_2 <- data.frame(y=agents_3_utils$X2)
agents_3_utils_3 <- data.frame(y=agents_3_utils$X3)
agents_3_utils_1$row <- seq.int(nrow(agents_3_utils_1))
agents_3_utils_2$row <- seq.int(nrow(agents_3_utils_2))
agents_3_utils_3$row <- seq.int(nrow(agents_3_utils_3))
agents_3_utils_1$x <- 1
agents_3_utils_2$x <- 2
agents_3_utils_3$x <- 3

all_agents_3_utils <- rbind(agents_3_utils_1, agents_3_utils_2, agents_3_utils_3)

ggplot(all_agents_3_utils, aes(x = x, y = y, colour = row, linetype = "dashed"))

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
