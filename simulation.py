import numpy as np

NUM_AGENTS_SOCIAL_QUICK = 1
NUM_AGENTS_SOCIAL_AVERAGE = 1
NUM_AGENTS_SOCIAL_LATE = 1
NUM_AGENTS_AVERAGE_QUICK = 1
NUM_AGENTS_AVERAGE_AVERAGE = 1
NUM_AGENTS_AVERAGE_LATE = 1
NUM_AGENTS_STRICT_QUICK = 1
NUM_AGENTS_STRICT_AVERAGE = 1
NUM_AGENTS_STRICT_LATE = 1
NUM_OPTIONS = 10
THRESHOLD_SOCIAL = 25
THRESHOLD_AVERGE = 50
THRESHOLD_STRICT = 75

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

if __name__ == "__main__":
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
	print(votes)
	print("Selected option is: " + str(votes.index(max(votes))))