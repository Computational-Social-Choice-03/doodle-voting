import numpy as np
import progressbar
import random

NUM_OPTIONS = 12

THRESHOLD_DISAPPROVE = 25
THRESHOLD_APPROVE = 75
# This should be equal or smaller than NUMOPTIONS - 2, we should probably vary this
SOCIAL_BONUS_CAP = 6
NUM_AGENTS = 6


class Agent:
	def __init__(self, num_options):
		self.utilities = np.random.randint(101, size=num_options)

	def accepts_options(self, vote_count, agents_total, agents_voted, lexi):
		possible_options = [i for i, util in enumerate(self.utilities) if (
			util > THRESHOLD_DISAPPROVE) & (util < THRESHOLD_APPROVE)]
		accepted_options = [i for i, util in enumerate(
			self.utilities) if util > THRESHOLD_APPROVE]
		
		second_max = sorted(vote_count, reverse=True)[1]
		max_votes = max(vote_count)

		# Everything that can no longer win is added
		safe_options = set([i for i in possible_options if (max_votes - vote_count[i]) > (agents_total - agents_voted)])
		
		# If the winner is known, add all options
		# For random tie breaking the difference needs to be bigger
		if (max_votes - second_max) > (agents_total - agents_voted + 1):
			safe_options.update(possible_options)
		# For lexicographic tie breaking, the options can have equal values, as long as the other comes later
		if ((max_votes - second_max) > (agents_total - agents_voted)) & lexi:
			safe_options.update([i for i in possible_options if i > vote_count.index(max_votes)])
		
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
		util_loss.append(agent.utilities[winning_option]/max(agent.utilities))
	return util_loss

def generate_agents(num_options, num_agents):
	agents = []
	for i in range(0, num_agents):
		agents.append(Agent(num_options))
	return agents


def run_test(num_options, num_agents, lexi):
	agents = generate_agents(num_options, num_agents)
	votes = [0] * num_options
	agents_voted = 0
	for agent in agents:
		for choice in agent.accepts_options(votes, num_agents, agents_voted, lexi):
			votes[choice] += 1
		agents_voted += 1
	if lexi:
		winning_option = votes.index(max(votes))
	else:
		winning_option = random.choice([i for i in votes if i == max(votes)])
	total_utilities = option_utility_agents(agents, num_options)
	# print("Votes for selected option is: " + str(votes[winning_option]))
	# print("Utility acquired: " + str(total_utilities[winning_option]/max(total_utilities)))
	# print("Utility acquired per agent: " + str(utility_loss_per_agent(agents, winning_option)))
	return {'votes': votes[winning_option], 'util_total': total_utilities[winning_option]/max(total_utilities), 'util_agent':utility_loss_per_agent(agents, winning_option)}

def avg_util_agents(util_agent):
	avg_utils =[]
	for i in range(0,len(util_agent[0])):
		total_util = 0
		for j in range(0, len(util_agent)):
			total_util += util_agent[j][i]
		avg_utils.append(total_util/len(util_agent))
	return avg_utils

def run_tests(num_options, num_agents, trials, lexi):
	votes = []
	util_total = []
	util_agent = []
	for i in range(0, trials):
		result = run_test(num_options, num_agents, lexi)
		votes.append(result['votes'])
		util_total.append(result['util_total'])
		util_agent.append(result['util_agent'])
	avg_util_agent = avg_util_agents(util_agent)
	return {'votes': np.mean(votes), 'util_total': np.mean(util_total), 'util_agent':avg_util_agent}

if __name__ == "__main__":
	trials = 10000
	# bar = progressbar.ProgressBar(max_value=trials)
	for lexi in [True, False]:
		print(run_tests(NUM_OPTIONS, NUM_AGENTS, trials, lexi))
	

