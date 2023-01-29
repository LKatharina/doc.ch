import csv, numpy as np, random as rnd
from collections.abc import Iterable
from pathlib import Path # Requires Python v3.4 or newer

# ==========================================================================
# Contents
#   This file contains the setup (phases, stimuli, blocks) of the experiment
#   
#   Change the sections 1., 2., and 3. below
# ==========================================================================
# CHANGE THIS
# 1. General setup ----------------------------------------------------------
phases = ["familiarization", "stimuli"]
"""String: Name or names of phases to change the stimulus material (see below) """

blocks = [1, 1]  # 1,1,3,3,3,3
"""Numbers, how often to repeat the stimulus material in a phase (= blocks)?
   1       = don't repeat, show the stimuli 1 x per phase (one block)
   1,2,3   = show stimuli of the first phase 1 x, the second 2 x, the third 3 x
"""

trials = [1]
"""How many trials to repeat each stimulus within each block in each phase?
   1 = Don't repeat, show each stimulis 1 time per block (1 trial)
   1,10,2 = in the first phase 1, in the second phase 10, in the third phase 2
"""

numactions = [2]
"""How many actions or choice options are there in each phase
   2      = two options in all phases
   2,1,5  = in phase one 2 options, in the next phase 1, in the next phase 5 
"""

numfeatures = [[2, 2]]
"""How many features do the stimuli have per phase?
   [[2,2]]       = both stimuli have two features in all phases
   [[2,2],[1,1]] = in the first phase two features, in the second phase one
"""

""" PHASES
   * A phase consists of a block of one (or more) blocks of stimuli.
   * Some or all phases can be shown in random order (see below)
   * The names of the phases tells the experiment which stimuli to show.
   * Stimuli must be in a .csv file which has one row per stimulus
   * Stimuli .csv files should be in the folder 'static/stimuli/'
   
   For example if the stimuli are stored as 'static/stimuli/choice_stimuli.csv' then you must use phases = ["choice_stimuli"]
"""



# OPTIONALLY CHANGE THIS
# 2. Bonus setup ------------------------------------------------------------
bonus_trials = [0, 1]
"""How many trials are drawn for the bonus in each phase?"""

bonus_blocks = [None]
"""How many blocks are drawn for the bonus in each phase?
   None    = no bonus blocks
   1,0,0   = one block from the first phase of 3
"""



# OPTIONALLY CHANGE THIS
# Randomization setup ------------------------------------------------------- 1,2,3
shuffle_phases = [] #[1, 3, 5]
"""Number, which 'phases' to show in random order (start counting at 0)?
   []    = Do not shuffle, keep the order of 'phases' as entered above
   [0,3] = shuffle among the frist and fourth phases
   [2,3] = shuffle the third and fourth phases
"""

randomize_feature = ['appearance/trial']
"""String, what to randomize in features
   '' = no randomization
   'appearance/once'
   'appearance/block'
   'appearance/trial' = feature appearance randomized for each trial
   'position/once'
   'position/trial'
   'position/block'
"""

randomize_action = ['position/trial']
"""List with strings what to randomize in features
   'color/once' = action color at the beginning
   'color/phase' = action color each phase
   'color/block' = action color each block
   'color/trial' = action color each trial
   'position/once' = action position randomized once at the start
   'position/phase' = action position randomized each phase
   'position/block' = action position randomized each block
   'position/trial' = action position randomized each trial
"""

randomize_stimulus_order = 'block'
"""String containing how to randomize randomize in features
   '' = No randomization
   'once'
   'phase'
   'block' = shuffle stimuli at the start of each block
"""


# Automatic Setups
# DO NOT CHANGE THE NEXT LINES
# =========================================================================
# Loads the environments from the csv files in 'filepath'
def load_choice_environment(filepath):
  with open(filepath) as csvfile:
    next(csvfile)
    file = csv.reader(csvfile, delimiter=',', quoting=csv.QUOTE_NONNUMERIC)
    environment = [[row[ :4], row[4:8], row[8: ]] for row in file]
  return environment
# File paths to the current file
foldername = Path(__file__).parent.name
filepaths = [foldername + "/static/stimuli/" + i + ".csv" for i in phases]
# Loads the stimuli by  phases fro csv
stimuli_by_phase = [load_choice_environment(x) for x in filepaths]
stimuli = [len(e) for e in stimuli_by_phase]
# Compute total number of rounds and phases etc.
num_phases = len(phases)
if num_phases > 1:
  if len(bonus_trials) == 1:
    bonus_trials = np.repeat(bonus_trials, num_phases)
  if len(numactions) == 1:
    numactions = np.repeat(numactions, num_phases)
  if len(trials) == 1:
    trials = np.repeat(trials, num_phases)
  if len(blocks) == 1:
    blocks = np.repeat(blocks, num_phases)
  if len(numfeatures) == 1:
    numfeatures = numfeatures * num_phases
trials_per_phase = [s * b * t for s, b, t in zip(stimuli, blocks, trials)]
num_rounds = int(sum(trials_per_phase))

counts_from_one = 1
"""NOT IMPLEMENTED. DONT CHANGE. 1 if trials and block counts should start at 1, 0 otherwise"""

# Checks
if min(blocks) < 1:
  print("\n\n'blocks' must be greater than 0.\n")



#
# Objects to manage the phase and the layout
# --------------------------------------------------------------------------
# 'Phasemanager' handles only the phases, it does not handle any randomization
# Randomition see the below 'Appearancemanager'
class Phasemanager:
  def __init__(self, phases, stimuli, blocks, trials):
    print("\n")
    self.doc = "Manage phases object holding the phases"
    phaseN = range(len(phases))
    # Randomization of the display order of the phases
    phase_dict = dict(zip(shuffle_phases, rnd.sample(shuffle_phases, k = len(shuffle_phases))))
    phase_order = list(map(phase_dict.get, phaseN, phaseN))

    self.phases = [phases[i] for i in phase_order]
    self.blocks = [blocks[i] for i in phase_order]
    self.trials = [trials[i] for i in phase_order]
    self.stimuli = [stimuli[i] for i in phase_order]
    self.trials_per_phase = [trials_per_phase[i] for i in phase_order]

    # Draw which block will be the bonus block
    bonus_in_block = [rnd.sample(range(self.trials_per_phase[i]), k = bonus_trials[i]) for i in phase_order]
    is_bonus_trial = [[False] * i for i in self.trials_per_phase]
    for i in phase_order:
      for j in bonus_in_block[i]:
        is_bonus_trial[i][j] = True
    self.is_bonus_trial = [i for s in is_bonus_trial for i in s]
    print(bonus_in_block)

    # Internal lookup table prior to randomization
    self.lookup = np.column_stack((
      # round number 0,1,2,...,N
      range(num_rounds),
      # phase number 0,0,0,0,1,1,1,...
      np.repeat(phase_order, self.trials_per_phase),
      # block number/phase 0,0,1,1,0,0,1,1,...
      [i for s in [np.repeat(list(range(b)), s) for s, b in zip(self.stimuli,self.blocks)] for i in s],
      # stimulus number per block per phase 0,1,0,1,0,1,2,3
      [i for s in [list(range(s))*b for s, b in zip(self.stimuli, self.blocks)] for i in s],
      # decision number per phase, 0,1,2,3, 0, 1,2,3,4
      [i for s in [range(x) for x in self.trials_per_phase] for i in s]
      ))
    self.phase_order = phase_order

    print("\n\n\nNEW STATUS FROM SETUP IN FILE exp.py",
    "\n---------------------------------------------------",
    "\nLoaded the following numbers of stimuli per phase:",
    "\n  Phase Name\tStimuli\tRepeatitions\n\n ",
    "\n  ".join("{}:\t{}\t{}".format(x, y, z) for x,y,z in zip(self.phases, self.stimuli, self.blocks)), "\n\n")

  def get_phaseN(self, round_number):
    return(int(self.lookup[round_number-1, 1]))
  def get_phaseL(self, round_number):
    return(phases[self.get_phaseN(round_number)])
  def get_block(self, round_number):
    return(int(self.lookup[round_number-1, 2] + counts_from_one))
  def get_num_trials_in_phase(self, round_number):
    return(int(self.trials_per_phase[self.get_phaseN(round_number)]))
  def get_bonus_rounds(self):
    return(self.lookup[self.is_bonus_trial, 0] + 1)
  def get_instruction_rounds(self):
    return([i + 1 for i in np.cumsum(self.trials_per_phase)])
  def get_decision_number_in_phase(self, round_number):
    return(int(self.lookup[round_number-1, 4] + counts_from_one))


# 'Appearancemanager' class takes care of all the randomization and shuffling
class Appearancemanager:
  def __init__(self, PM, filepaths, numfeatures, numactions, randomize_feature, randomize_action, randomize_stimulus_order):
    # Load the environment/s
    self.environments = stimuli_by_phase
    
    # Randomizations
    # Implement phase randomization for numactions according  to PM.phaseN
    numactions =  [numactions[i]  for i in PM.phase_order]
    numfeatures = [numfeatures[i] for i in PM.phase_order]

    # Stimuli: Display order of the stimuli
    self.stimulus_order = np.repeat([range(x) for x in PM.stimuli], PM.blocks, axis = 0).tolist()
    if (randomize_stimulus_order == 'block'):
      self.stimulus_order = [rnd.sample(x, k=len(x)) for x in self.stimulus_order]
    self.stimulus_order = [item for sublist in self.stimulus_order for item in sublist]
    
    # Action: Position of the action buttons (= stimuli) or of the keys
    num_rounds_per_phase = PM.trials_per_phase
    num_phases = len(phases)
    self.action_positions = [list(range(x)) for x in np.repeat(numactions, num_rounds_per_phase)]
    if ('position/trial' in randomize_action):
      self.action_positions = [rnd.sample(x, k=len(x)) for x in self.action_positions]
    
    # Determine position and appearance of features ---------------------------
    numactions_long = np.repeat(numactions, num_rounds_per_phase)
    numfeatures_long = [[numfeatures[i]] * num_rounds_per_phase[i] for i in range(num_phases)]
    numfeatures_long = [item for sublist in numfeatures_long for item in sublist] # this is just to flatten it to one vector
    self.feature_appearances = [[list(range(numfeatures_long[i][0]))] * numactions_long[i] for i in range(num_rounds)] # todo: this only works if feature.0 and feature.1 have the same number of values!!
    if ('appearance/trial' in randomize_feature):
      self.feature_appearances = [[rnd.sample(range(numfeatures_long[i][0]), k = numfeatures_long[i][0])] * numactions_long[i] for i in range(num_rounds)]

  def get_stimuli(self, round_number, phase_number):
    i = self.stimulus_order[round_number-1]
    return(self.environments[phase_number][i])
  def get_action_position(self, round_number):
    return(self.action_positions[round_number-1])
  def get_feature_appearance(self, round_number):
    return(self.feature_appearances[round_number-1])