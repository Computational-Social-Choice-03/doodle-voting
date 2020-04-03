import numpy as np
import progressbar
import random

NUM_OPTIONS = 12

THRESHOLD_MINIMUM = 25
THRESHOLD_ACCEPT = 75
# This should be equal or smaller than NUMOPTIONS - 2, we should probably vary this
SOCIAL_BONUS_CAP = 6
NUM_AGENTS = 6
LEXI = True


class Agent:
	def __init__(self, num_options):
		self.utilities = np.random.randint(101, size=num_options)

	def accepts_options(self, vote_count, agents_total, agents_voted):
		possible_options = [i for i, util in enumerate(self.utilities) if (
			util > THRESHOLD_MINIMUM) & (util < THRESHOLD_ACCEPT)]
		accepted_options = [i for i, util in enumerate(
			self.utilities) if util > THRESHOLD_ACCEPT]
		
		second_max = sorted(vote_count, reverse=True)[1]
		max_votes = max(vote_count)

		# Everything that can no longer win is added
		safe_options = set([i for i in possible_options if (max_votes - vote_count[i]) > (agents_total - agents_voted)])
		
		# If the winner is known, add all options
		# For random tie breaking the difference needs to be bigger
		if (max_votes - second_max) > (agents_total - agents_voted + 1):
			safe_options.add(possible_options)
		# For lexicographic tie breaking, the options can have equal values, as long as the other comes later
		if ((max_votes - second_max) > (agents_total - agents_voted)) & LEXI:
			safe_options.add([i for i in possible_options if i > vote_count.index(max_votes)])
		
		accepted_options.extend(safe_options)
		possible_options = [i for i in possible_options if i not in accepted_options]

		# If there is still a social bonus to be acquired, add options with the least votes
		while (len(accepted_options) < SOCIAL_BONUS_CAP) & (len(possible_options) > 0):
			pos_min = possible_options[0]
			val_min = vote_count[pos_min]
			for pos in possible_options:
				if val_min > vote_count[pos]:
					pos_min = pos
					val_min = vote_count[pos]
			accepted_options.append(pos_min)
			del possible_options[possible_options.index(pos_min)]
		return accepted_options


def option_utility_agents(agents, num_options):
	option_util = [0] * num_options
	for i in range(0, num_options):
		for agent in agents:
			option_util[i] += agent.utilities[i]
	return option_util

def utility_loss_per_agent(agents, winning_option):
	util_loss = []
	for agent in agents:
		util_loss.append(max(agent.utilities)-agent.utilities[winning_option])
	return util_loss

def generate_agents(num_options, num_agents):
	agents = []
	for i in range(0, num_agents):
		agents.append(Agent(num_options))
	return agents


def run_test(num_options, num_agents):
	agents = generate_agents(num_options, num_agents)
	votes = [0] * num_options
	agents_voted = 0
	for agent in agents:
		for choice in agent.accepts_options(votes, num_agents, agents_voted):
			votes[choice] += 1
		agents_voted += 1
	print(votes)
	if LEXI:
		winning_option = votes.index(max(votes))
	else:
		winning_option = random.choice([i for i in votes if i == max(votes)])
	total_utilities = option_utility_agents(agents, num_options)
	print("Selected option is: " + str(winning_option))
	print("Votes for selected option is: " + str(votes[winning_option]))
	print("Total utility missed: " + str(max(total_utilities) - total_utilities[winning_option]))
	print("Missed utility per agent: " + str(utility_loss_per_agent(agents, winning_option)))


if __name__ == "__main__":
	trials = 1
	# bar = progressbar.ProgressBar(max_value=trials)
	for i in range(0, trials):
		run_test(NUM_OPTIONS, NUM_AGENTS)
	# bar.update(i)
