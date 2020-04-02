import numpy as np
import time
import progressbar

# Total number of agents is 90
NUM_AGENTS_SOCIAL_QUICK = 10
NUM_AGENTS_SOCIAL_AVERAGE = 10
NUM_AGENTS_SOCIAL_LATE = 10
NUM_AGENTS_AVERAGE_QUICK = 10
NUM_AGENTS_AVERAGE_AVERAGE = 10
NUM_AGENTS_AVERAGE_LATE = 10
NUM_AGENTS_STRICT_QUICK = 10
NUM_AGENTS_STRICT_AVERAGE = 10
NUM_AGENTS_STRICT_LATE = 10

NUM_OPTIONS = 10

THRESHOLD_SOCIAL = 25
THRESHOLD_AVERGE = 50
THRESHOLD_STRICT = 75

def partition(n, d, depth=0):
    if d == depth:
        return [[]]
    return [
        item + [i]
        for i in range(n+1)
        for item in partition(n-i, d, depth=depth+1)
		]

n = 9
d = 9
Agents_lst = [[n-sum(p)] + p for p in partition(n, d-1)]

class Agent:
    def __init__(self, strategy, speed, num_options):
        self.strategy = strategy
        self.speed = speed
        self.utilities = np.random.randint(101, size=num_options)

    def accepts_options(self):
        if self.strategy == "social":
            return [i for i, util in enumerate(self.utilities) if util > THRESHOLD_SOCIAL]
        if self.strategy == "average":
            return [i for i, util in enumerate(self.utilities) if util > THRESHOLD_AVERGE]
        else:
            return [i for i, util in enumerate(self.utilities) if util > THRESHOLD_STRICT]


def option_utility_agents(agents, num_options):
    option_util = [0] * num_options
    for i in range(0, num_options):
        for agent in agents:
            option_util[i] += agent.utilities[i]
    return option_util


def generate_agents(num_options):
    agents = []
    for i in range(0, NUM_AGENTS_SOCIAL_QUICK):
        agents.append(Agent("social", "quick", num_options))
    for i in range(0, NUM_AGENTS_SOCIAL_AVERAGE):
        agents.append(Agent("social", "average", num_options))
    for i in range(0, NUM_AGENTS_SOCIAL_LATE):
        agents.append(Agent("social", "late", num_options))
    for i in range(0, NUM_AGENTS_AVERAGE_QUICK):
        agents.append(Agent("average", "quick", num_options))
    for i in range(0, NUM_AGENTS_AVERAGE_AVERAGE):
        agents.append(Agent("average", "average", num_options))
    for i in range(0, NUM_AGENTS_AVERAGE_LATE):
        agents.append(Agent("average", "late", num_options))
    for i in range(0, NUM_AGENTS_STRICT_QUICK):
        agents.append(Agent("strict", "quick", num_options))
    for i in range(0, NUM_AGENTS_STRICT_AVERAGE):
        agents.append(Agent("strict", "average", num_options))
    for i in range(0, NUM_AGENTS_STRICT_LATE):
        agents.append(Agent("strict", "late", num_options))
    return agents


def run_test():
	agents = generate_agents(NUM_OPTIONS)
	votes = [0]*NUM_OPTIONS
	for agent in [a for a in agents if a.speed == "quick"]:
		for choice in agent.accepts_options():
			votes[choice]+=1
	for agent in [a for a in agents if a.speed == "average"]:
		for choice in agent.accepts_options():
			votes[choice]+=1
	for agent in [a for a in agents if a.speed == "late"]:
		for choice in agent.accepts_options():
			votes[choice]+=1
		# print(votes)
		# print("Selected option is: " + str(votes.index(max(votes))))

if __name__ == "__main__":
	print(Agents_lst[0][0])
	bar = progressbar.ProgressBar(max_value=len(Agents_lst)*100)
	for i in range(0, len(Agents_lst)):	
		NUM_AGENTS_SOCIAL_QUICK = Agents_lst[i][0] * 10
		NUM_AGENTS_SOCIAL_AVERAGE = Agents_lst[i][1] * 10
		NUM_AGENTS_SOCIAL_LATE = Agents_lst[i][2] * 10
		NUM_AGENTS_AVERAGE_QUICK = Agents_lst[i][3] * 10
		NUM_AGENTS_AVERAGE_AVERAGE = Agents_lst[i][4] * 10
		NUM_AGENTS_AVERAGE_LATE = Agents_lst[i][5] * 10
		NUM_AGENTS_STRICT_QUICK = Agents_lst[i][6] * 10
		NUM_AGENTS_STRICT_AVERAGE = Agents_lst[i][7] * 10
		NUM_AGENTS_STRICT_LATE = Agents_lst[i][8] * 10
		for j in range(0, 100):
			run_test()
		bar.update(i)
