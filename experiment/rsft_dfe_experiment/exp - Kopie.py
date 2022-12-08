import csv, numpy as np, random as rnd
from collections.abc import Iterable
from pathlib import Path # Requires Python v3.4 or newer
import os
# ==========================================================================
# Contents
#   This file contains the setup (phases, stimuli, blocks) of the experiment
#   
#   Change the sections 1., 2., and 3. below
# ==========================================================================
# CHANGE THIS
# 1. General setup ----------------------------------------------------------
phases = ["familiarization", "stimuli_easy", "attentioncheck_easy", "stimuli_medium", "attentioncheck_medium", "stimuli_hard", "attentioncheck_hard"]
"""String: Name or names of phases to change the stimulus material (see below) """

blocks = [1, 1, 1, 1, 1, 1, 1]  # 1,1,3,3,3,3
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
bonus_trials = [0, 3, 0, 1, 0, 1, 0]
"""How many trials are drawn for the bonus in each phase?"""

bonus_blocks = [None]
"""How many blocks are drawn for the bonus in each phase?
   None    = no bonus blocks
   1,0,0   = one block from the first phase of 3
"""



# OPTIONALLY CHANGE THIS
# Randomization setup ------------------------------------------------------- 1,2,3
shuffle_phases = [1, 3, 5]
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
    # Ein quasi verschachtelter Array, der alle die Einzelnen Choice Tasks einer Phase speichert
  return environment

# File paths to the current file
foldername = Path(__file__).parent.name 
filepaths = [os.getcwd() +"/static/stimuli/" + i + ".csv" for i in phases] # Generierung der Dateinamen zum Laden der Stimuli

# Loads the stimuli by  phases fro csv
stimuli_by_phase = [load_choice_environment(x) for x in filepaths] # Array enthält Stimuli unterteilt nach Phasen
stimuli = [len(e) for e in stimuli_by_phase] # Array enthält Anzahl Stimuli pro Phase

# Compute total number of rounds and phases etc.
num_phases = len(phases)
if num_phases > 1:
  if len(bonus_trials) == 1:
    bonus_trials = np.repeat(bonus_trials, num_phases) #Verlängerung der Array, falls sie nicht den Anzahl Phasen entspricht
  if len(numactions) == 1:
    numactions = np.repeat(numactions, num_phases)  # Verlängerung Array auf Anzahl Phasen
  if len(trials) == 1:
    trials = np.repeat(trials, num_phases) # Verlängerung Array auf Anzahl Phasen
  if len(blocks) == 1:
    blocks = np.repeat(blocks, num_phases)  # Verlängerung Array auf Anzahl Phasen
  if len(numfeatures) == 1:
    numfeatures = numfeatures * num_phases  # Verlängerung Array auf Anzahl Phasen

trials_per_phase = [s * b * t for s, b, t in zip(stimuli, blocks, trials)] # Array pro Phase: Stimuli*Blocks*trials
num_rounds = int(sum(trials_per_phase)) # Summe

counts_from_one = 1

"""NOT IMPLEMENTED. DONT CHANGE. 1 if trials and block counts should start at 1, 0 otherwise"""

# Checks
if min(blocks) < 1:
  print("\n\n'blocks' must be greater than 0.\n")


# Phasenmanager ==============================================================================================
phaseN = range(len(phases)) # range(0-6)

# Randomization of the display order of the phases -----------------------------------------------------------
phase_dict = dict(zip(shuffle_phases, rnd.sample(shuffle_phases, k = len(shuffle_phases))))
    # Randomisierung der Phasen --> Anstatt, welche Phase wird welche gezeigt.
    # Output z.b. {1:5,3:3,5:1} --> Anstatt 1 wird 5 gezeigt, anstatt 5 wird 1 gezeigt

# Phasenreihenfolge nach der Randomisierung    
phase_order = list(map(phase_dict.get, phaseN, phaseN))
    # mit get wird nach der phaseN (von 0 - 6) in phase_dict gesucht, falls die Zahl als Key vorhanden ist,
    # wird der Value eingesetzt, falls nicht wird der ursprüngliche Wert von phaseN eingesetzt. Dies wird wiederholt
    # für alle Werte der Range PhaseN

# Umordnung nach der neuen Phasenreihenfolge ---------------------------------------------------------------
phases1 = [phases[i] for i in phase_order] # Zieht anhand der Phasenordnung die Namen der Phase aus der Phasenliste und erstellt eine Liste
blocks1 = [blocks[i] for i in phase_order] # Zieht anhand der Phasenordnung die Anzahl Blocks für die Phasen
trials1 = [trials[i] for i in phase_order] # Zieht anhand der Phasenordnung die Anzahl Trials für die Phasen
stimuli1 = [stimuli[i] for i in phase_order] # Zieht anhand der Phasenordnung die Anzahl Stimuli für jede Phase
trials_per_phase1 = [trials_per_phase[i] for i in phase_order] # Zieht anhand der Phasenordnung die Anzahl Trials für jede Phase
bonus_in_block = [rnd.sample(range(trials_per_phase1[i]), k = bonus_trials[i]) for i in phase_order]


# Bonustrials: true vs false
is_bonus_trial = [[False] * i for i in trials_per_phase1] # Setzt alle Werte auf false
for i in phase_order:
 for j in bonus_in_block[i]:
  is_bonus_trial[i][j] = True
is_bonus_trial = [i for s in is_bonus_trial for i in s] # Nochmals anschauen

# Eine Tabelle mit folgenden Spalten (Tabelle gibt Reihenfolge an, welche randomisiert ist) ------------------
# Runde (0:Anzahl Runden-1) --> Choice Task NR
# Phase (zu welcher Phase gehört die Runde)
# Blocknr pro Phase (Wenn man die Blöcke nicht wiederholt, dann immer 0)
# Choice Task NR pro Block und Phase
# Choice Task NR pro Phase

lookup = np.column_stack((
 # round number 0,1,2,...,N
 range(num_rounds),
 # phase number 0,0,0,0,1,1,1,...
 np.repeat(phase_order, trials_per_phase1),
 # block number/phase 0,0,1,1,0,0,1,1,...
 [i for s in [np.repeat(list(range(b)), s) for s, b in zip(stimuli1,blocks1)] for i in s],
 # stimulus number per block per phase 0,1,0,1,0,1,2,3
 [i for s in [list(range(s))*b for s, b in zip(stimuli1, blocks1)] for i in s],
 # decision number per phase, 0,1,2,3, 0, 1,2,3,4
 [i for s in [range(x) for x in trials_per_phase1] for i in s]
))

print(lookup)

# METHODEN --------------------------------------------------------------------------------------------------
# Welche Phasennr (unrandomisierte NR) wird aber aus dem randomisierten LookupTabelle gezogen
def get_phaseN(round_number):
    return(int(lookup[round_number-1, 1]))

# Wie heisst die Phase
def get_phaseL(round_number):
    return(phases[get_phaseN(round_number)])

def get_block(round_number):
    return(int(lookup[round_number-1, 2] + counts_from_one)) #Blocknr --> Wenn Block nicht wiederholt dann 0

def get_num_trials_in_phase(round_number):
    return(int(trials_per_phase1[get_phaseN(round_number)]))

def get_bonus_rounds():
    return(lookup[is_bonus_trial, 0] + 1)

def get_instruction_rounds():
    return([i + 1 for i in np.cumsum(trials_per_phase1)]) #Vor diesen Runden kommen Instruktionen?
    #Hier [2, 6, 7, 11, 12, 16, 17]
    # 1 erhält schon instruktion, 2 --> Eigentlicher Stimuli (2-5), 6 Attentioncheck, 7 --> Eigentliche Stimuli (7-10)
    # 11 Attentioncheck --> 12 Eigentliche Stimuli (12-15), 16 --> Attentioncheck. Was passiert mit 17?

# Choice Task nr pro Phase
def get_decision_number_in_phase(round_number):
    return(int(lookup[round_number-1, 4] + counts_from_one))

# Appearance Manager =============================================================================
environments = stimuli_by_phase # Unrandomisiert
numactions =  [numactions[i]  for i in phase_order] # [2, 2, 2, 2, 2, 2, 2] --> In jeder Phase gibt es pro Choice Task 2 Optionen
numfeatures = [numfeatures[i] for i in phase_order] # [[2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2]] --> In jeder Phase gibt es pro Choice Task und pro Option 2 Features 


# Stimuli: Display order of the stimuli ----------------------------------------------------------
stimulus_order = np.repeat([range(x) for x in stimuli1], blocks1, axis = 0).tolist()
# [range(0, 1), range(0, 4), range(0, 1), range(0, 4), range(0, 1), range(0, 4), range(0, 1)]

if (randomize_stimulus_order == 'block'):
    stimulus_order = [rnd.sample(x, k=len(x)) for x in stimulus_order] # Jede range() wird randomisiert
    
stimulus_order = [item for sublist in stimulus_order for item in sublist]
# Erstellen eines Vektors [0, 1, 2, 3, 0, 0, 2, 0, 3, 1, 0, 2, 3, 1, 0, 0]
print("stimulus_order:",stimulus_order)

# Action: Position of the action buttons (= stimuli) or of the keys ---------------------------------
num_rounds_per_phase = trials_per_phase1
num_phases = len(phases)

for x in np.repeat(numactions, num_rounds_per_phase):
    action_positions = [list(range(x)) for x in np.repeat(numactions, num_rounds_per_phase)] 
    # np.repeat(numactions, num_rounds_per_phase)] --> [2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2]
    # wiederholt range(2) --> [0,1]
    # [[0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1], [0, 1]]


if ('position/trial' in randomize_action):
    action_positions = [rnd.sample(x, k=len(x)) for x in action_positions] # Randomisierung der Actions
    # [[0, 1], [0, 1], [1, 0], [0, 1], [0, 1], [1, 0], [0, 1], [1, 0], [1, 0], [0, 1], [1, 0], [1, 0], [0, 1], [0, 1], [0, 1], [0, 1]]

# Features randomisieren --> Wie werden die Outcomes angezeigt ------------------------------------------
# Anzeige der Outcomes innerhalb der Optionen --> pro Option 2 Möglichkeiten
numactions_long = np.repeat(numactions, num_rounds_per_phase) # Verlängerung für jeden Stimuli (Choice Task) --> [2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2]
numfeatures_long = [[numfeatures[i]] * num_rounds_per_phase[i] for i in range(num_phases)] # Für jeden Stimuli --> [[2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2], [2, 2]]
numfeatures_long = [item for sublist in numfeatures_long for item in sublist] # this is just to flatten it to one vector

# Zeigt die Anordung der Outcomes für jeden Choice Task --> hier noch unrandomisiert
feature_appearances = [[list(range(numfeatures_long[i][0]))] * numactions_long[i] for i in range(num_rounds)] # todo: this only works if feature.0 and feature.1 have the same number of values!!
# [[[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]]]

# Randomisierung der Outcomes in jedem Choice Task
if ('appearance/trial' in randomize_feature):
 feature_appearances = [[rnd.sample(range(numfeatures_long[i][0]), k = numfeatures_long[i][0])] * numactions_long[i] for i in range(num_rounds)]
# [[[1, 0], [1, 0]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[0, 1], [0, 1]], [[1, 0], [1, 0]], [[1, 0], [1, 0]], [[1, 0], [1, 0]], [[0, 1], [0, 1]], [[1, 0], [1, 0]], [[0, 1], [0, 1]], [[1, 0], [1, 0]], [[0, 1], [0, 1]]]

# Methoden ======================================================================================================================

# Gibt Choice Task zurück
def get_stimuli(round_number, phase_number):
    i = stimulus_order[round_number-1] #gibt an welcher Stimuli pro Phase zurück gegeben wird
    return(environments[phase_number][i]) # phase_number gibt die Phase an

def get_action_position(round_number):
    return(action_positions[round_number-1])

def get_feature_appearance(round_number):
    return(feature_appearances[round_number-1])

# init__ ======================================================================================================================
### init Beispiel für round_number 1 = Familarization Choice Task

# Funktionen ------------------------------------------------------------------------------------------------------------------
def draw_outcomes(action, size):
  # print(action)
  probs = action[2: ][1]
  # print(probs)
  indices = np.random.binomial(n=1, p=probs, size=size)
  # print(indices)
  #indices = [0, 1, 0, 1, 1, 0, 1, 0, 1, 1]
  x = action[ :2]
  res = [x[i] for i in indices]
  # print(res)
  return res

def concat_stimulus(i, stimuli):
  y = "_".join( str(x) for x in ['%.0f' % stimuli[i][0], '%.0f' % (stimuli[i][2] * 100), '%.0f' % stimuli[i][1]] )
  return(y)

# Konstanten
NAME_IN_URL = 'study' # how the experiment is called in the browser URL
PLAYERS_PER_GROUP = None # do we group players? Usually None means not
NUM_FAMILIARIZATION_ROUNDS = 1 # how many times rounds are familiarization?
NUM_REPETITIONS = 1 # how many repetitions of stimuli?
NUM_SAMPLES_PER_OPTION = 3 #how manys amples per option
NUM_TRIALS = 3 # in the RSFT-choice, how many trials?
NUM_ONESHOT = 0 # how many 1-choice trials?
NUM_MULTITRIAL = NUM_REPETITIONS + NUM_FAMILIARIZATION_ROUNDS  
NUM_ROUNDS = int(num_rounds) # exp.py calculates number of rounds -->
  # Anzahl Stimuli (ohne wiederholung) pro Block * Anzahl Wiederholungen innerhalb eines Blocks * Anzahl Blöcke pro Phase
  # Das wird für alle Phasen berechnet und dann aufsummiert.
INITIAL_STATE = 0
NUM_ACTIONS = 2
DURATION = 55
BONUS_AMOUNT = 0.25


# Variablen ---------------------------------------------------------------------------------------------------------------------
round_number = 4 # Erste Entscheidungsaufgabe --> Familiarizierung
phase_number = get_phaseN(round_number) # Phasennr unrandomisiert
phase = get_phaseL(round_number) # phase label bsp: familiarization (wenn 1 dann)
stimuli = get_stimuli(round_number, phase_number) # [[3.0, 9.0, 0.6, 0.4], [7.0, 5.0, 0.2, 0.8], [29.0, 0.0]]
stimulus_position = get_action_position(round_number) # Output [0, 1]
print("stimulus originale Anordnung:",stimuli)
print("stimulus position:", stimulus_position)

feature_color = get_feature_appearance(round_number)[0] # Output [[0, 1], [0, 1]]

# Store variables
# p.block = subsession.round_number
budget = stimuli[2][0]  # [[3.0, 9.0, 0.6, 0.4], [7.0, 5.0, 0.2, 0.8], [29.0, 0.0]] --> Hier 29
stimulus0 = concat_stimulus(0, stimuli) #Name für Stimuli z.B. 3_60_9 (Risky) unrandomisiert
stimulus1 = concat_stimulus(1, stimuli) #Name für Stimuli z.B. 7_20_5 (Safe) unrandomisiert
state = stimuli[2][1] # [[3.0, 9.0, 0.6, 0.4], [7.0, 5.0, 0.2, 0.8], [29.0, 0.0]] --> Hier 0
    # this is only if we have one-shot trials in there
    # if (phase in ['critical']):
    #   p.trial = stimuli[2][2]
    # else:
trial = 1
successes = 0

# Do and store the randomizations
layout_featurecolor = '_'.join(''.join(str(y) for y in x) for x in [feature_color]) #gibt an wie Outcomes gezeigt werden: 01 oder 10 (gleich beide Optionen)
layout_stimulusposition_01 = ''.join(str(x) for x in stimulus_position) # gibt an welche Option links und welche rechts gezeigt wird: 01 (risky-safe) oder 10 (safe-risky)


# Initialisierung der Variablen, Containers --------------------------------------------------------------------------------------------
n = int(num_rounds + 1) #hier 17 (in 0 wird nichts gespeichert?)
stimulus_position_v = [None] * n #[None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None]
img1_v = [None] * n
img2_v = [None] * n
max_earnings_v = [None] * n
block_number_v = [None] * n
#participant.num_blocks = [None] * n
decision_number_v = [None] * n
outcomes_v = [None] * n
sampling_outcomes_v = [None] * n
num_samples_per_option_v = 3
stimulus_position_v[round_number] = stimulus_position # Einfüllen des Stimulus


# Define Sprites ------------------------------------------------------------------------------------------------------------------------
# Define the names of the sprites for the images
css_img_orig_position = [
  'sprite_' + stimulus0 + '_featurecolor' + layout_featurecolor,
  'sprite_' + stimulus1 + '_featurecolor' + layout_featurecolor
    ]
# ['sprite_3_60_9_featurecolor10', 'sprite_7_20_5_featurecolor10']

# Hier wird anhand der Randomisierung, die in stimulus_position gespeichert ist bestummen, wo welcher Stimulus gezeigt wird.
img1_v[round_number] = css_img_orig_position[stimulus_position[0]] # Wenn Stimulus position [0,1] dann Stimulus0 img1, wenn [1,0] dann Stimulus1 auf img1
img2_v[round_number] = css_img_orig_position[stimulus_position[1]] # Wenn Stimulus position [0,1] dann Stimulus1 img2, wenn [1,0] dann Stimulus0 auf img2


# Outcomes ziehen -------------------------------------------------------------------------------------------------------------------------

outcomes_orig_position = [draw_outcomes(x, NUM_TRIALS) for x in get_stimuli(round_number, phase_number)[ :2]] #Nur Stimuli [[3.0, 9.0, 0.6, 0.4], [7.0, 5.0, 0.2, 0.8]]
# Es wird aus dem ersten Stimulus und dann aus dem zweiten Stimulus num_trials outcomes gezogen (num_trials = Anzahl outcomes die gezogen werden)
# Ergibt einen Vektor (verschachtelt) Liste, bei der die erste Ebene die Stimuli angeben und die 2 die gezogenen Outcomes
# Output --> z.B. [[3.0, 9.0, 3.0], [5.0, 5.0, 5.0]]

# Anordnung der gezogenen Outcomes nach Stimulus position
outcomes_v[round_number] = [outcomes_orig_position[i] for i in stimulus_position]
print("Outcomes nach Stimulus position geordnet:", outcomes_v[round_number])

# Sampling -------------------------------------------------------------------------------------------------------------------------------
sampling_outcomes_orig_position = [draw_outcomes(x, NUM_SAMPLES_PER_OPTION) for x in get_stimuli(round_number, phase_number)[ :2]] # Ziehen der Outcomes für das Sampling
sampling_outcomes_v[round_number] = [sampling_outcomes_orig_position[i] for i in stimulus_position]


# Process Bar -----------------------------------------------------------------------------------------------------------------------------
maxx = max([max(list(map(abs, stimuli[i]))) for i in [0,1]]) # sucht grösster Outcome
max_earnings = max(maxx * (NUM_TRIALS), budget)

# select overall maximum points to be earned to determine the width of the progress bar
# if (round_number == 1):
#     last_max_earnings = int(max_earnings)
# else:
#     last_max_earnings = max_earnings_v[(round_number - 1)]

# max_earnings = max(last_max_earnings, max_earnings)
# max_earnings_v = [max_earnings] * n
decision_number_v[round_number] = get_decision_number_in_phase(round_number)
print(decision_number_v)
if (round_number == 1):
    successes = 0
      

def make_q(key = None):
  # Enter quiz questions in here formatted with 1 being true:
    # q_name = ["Question text?",
    #   [1, "True answer"], [2, "Wrong answer"], [3, "Wrong answer"]]
  quiz = dict(
    q_task = ['What will your task be?',
      [1, 'Choose the best option to reach a threshold.'],
      [2, 'Choose the best threshold among two.'],
      [3, 'Choose the best of 3 options A, B, C.']],
    q_trials = ['How often can you choose until your point total should have reached the threshold?',
      [1, str(NUM_TRIALS) + " choices."],
      [2, "10 choices."],
      [3, str(DURATION) + " choices."]],
    q_options = ['What are the options like?',
      [1, "Options give points with a probability."],
      [2, "Options give points with certainty."],
      [3, "Options never give any points."]],
    q_outcomes = ["How many points does the shown option offer?",
      [1, "7 points or 5 points."],
      [2, "3 points or 6 points"],
      [3, "10 points or -4 points"]],
    q_samples = ["What happens upon clicking one option?",
      [1, "The option returns points that are drawn according to their probability."],
      [2, "The option's left outcome is drawn with a constant probability of 45%."],
      [3, "The option changes the points that it offers according to their probability."]],
    q_view = ["What will you do when <u>first</u> viewing the options?",
      [1, "Familiarize yourself with the options without accumulating points."],
      [2, "Reach the threshold by accumulating points."],
      [3, "Learn about the threshold without accumulating points."]],
    q_reach = ["What will you do on the <u>next</u> page?  ",
      [1, "Accumulate points by clicking on one of two options."],
      [2, "Clicking an option leads to a draw of points that I accumulate."],
      [3, "Clicking an option leads to familiarization with the option."]]
    )
  if key is None:
    return quiz
  else:
    ans = quiz[key][1:]
    rnd.shuffle(ans)
    label = quiz[key][0]
    choices = ans
    return label
print(make_q(1))
# Objects to manage the phase and the layout
# --------------------------------------------------------------------------
# 'Phasemanager' handles only the phases, it does not handle any randomization
# Randomition see the below 'Appearancemanager'
"""
class Phasemanager:
  def __init__(self, phases, stimuli, blocks, trials):
    print("\n")
    self.doc = "Manage phases object holding the phases"
    phaseN = range(len(phases)) # range(0-6)
    
    # Randomization of the display order of the phases
    # Randomisierung der Phasen --> Anstatt, welche Phase wird welche gezeigt.
    # Output z.b. {1:5,3:3,5:1} --> Anstatt 1 wird 5 gezeigt, anstatt 5 wird 1 gezeigt
    phase_dict = dict(zip(shuffle_phases, rnd.sample(shuffle_phases, k = len(shuffle_phases))))
 
    phase_order = list(map(phase_dict.get, phaseN, phaseN))
    # mit get wird nach der phaseN (von 0 - 6) in phase_dict gesucht, falls die Zahl als Key vorhanden ist,
    # wird der Value eingesetzt, falls nicht wird der ursprüngliche Wert von phaseN eingesetzt. Dies wird wiederholt
    # für alle Werte der Range PhaseN

    self.phases = [phases[i] for i in phase_order] # Zieht anhand der Phasenordnung die Namen der Phase aus der Phasenliste und erstellt eine Liste
    self.blocks = [blocks[i] for i in phase_order] # Zieht anhand der Phasenordnung die Anzahl Blocks für die Phasen
    self.trials = [trials[i] for i in phase_order] # Zieht anhand der Phasenordnung die Anzahl Trials für die Phasen
    self.stimuli = [stimuli[i] for i in phase_order] # Zieht anhand der Phasenordnung die Anzahl Stimuli für jede Phase
    self.trials_per_phase = [trials_per_phase[i] for i in phase_order] # Zieht anhand der Phasenordnung die Anzahl Trials für jede Phase

    # Draw which block will be the bonus block
    bonus_in_block = [rnd.sample(range(self.trials_per_phase[i]), k = bonus_trials[i]) for i in phase_order]
    # Zieht die Aufgaben, welche Bonusaufgaben sind --> Achtung Anzahl trials per Phase wurde 2x geordnet, d.h wenn die Phasen eine unterschiedliche Anzahl
    # An Trials total haben, dann werden jenachdem nicht aus allen Trials gezogen --> Man darf nicht 2x ordnen
    # Überprüfen wenn Phasen, die randomisiert werden eine unterschiedliche Anzahl Trials hat.
    
    is_bonus_trial = [[False] * i for i in self.trials_per_phase] # Setzt alle Werte auf false
    for i in phase_order:
      for j in bonus_in_block[i]:
        is_bonus_trial[i][j] = True
    self.is_bonus_trial = [i for s in is_bonus_trial for i in s] # Nochmals anschauen
    print(bonus_in_block)

    # Internal lookup table prior to randomization
    self.lookup = np.column_stack((
      # round number 0,1,2,...,N
      range(num_rounds),
      # phase number 0,0,0,0,1,1,1,... --> In welcher Phase kommt diese Choice Task
      np.repeat(phase_order, self.trials_per_phase),
      # block number/phase 0,0,1,1,0,0,1,1,... --> Gibt an ob ein Block wiederholt wird in einer Phase
      [i for s in [np.repeat(list(range(b)), s) for s, b in zip(self.stimuli,self.blocks)] for i in s],
      # stimulus number per block per phase 0,1,0,1,0,1,2,3 --> Number Choice Task pro Block
      [i for s in [list(range(s))*b for s, b in zip(self.stimuli, self.blocks)] for i in s],
      # decision number per phase, 0,1,2,3, 0, 1,2,3,4 --> Nummer der Choice Task in einer Phase 
      [i for s in [range(x) for x in self.trials_per_phase] for i in s]
      ))
    self.phase_order = phase_order

    print("\n\n\nNEW STATUS FROM SETUP IN FILE exp.py",
    "\n---------------------------------------------------",
    "\nLoaded the following numbers of stimuli per phase:",
    "\n  Phase Name\tStimuli\tRepeatitions\n\n ",
    "\n  ".join("{}:\t{}\t{}".format(x, y, z) for x,y,z in zip(self.phases, self.stimuli, self.blocks)), "\n\n")

  def get_phaseN(self, round_number):
    return(int(self.lookup[round_number-1, 1])) # Spalte 2 in der Tabelle --> Phasennr
  def get_phaseL(self, round_number):
    return(phases[self.get_phaseN(round_number)]) # Benennen der Phasennr anhand dem Wert in der Tabelle
  def get_block(self, round_number):
    return(int(self.lookup[round_number-1, 2] + counts_from_one)) #Blocknr --> Wenn Block nicht wiederholt dann 0
  def get_num_trials_in_phase(self, round_number):
    return(int(self.trials_per_phase[self.get_phaseN(round_number)]))
  def get_bonus_rounds(self):
    return(self.lookup[self.is_bonus_trial, 0] + 1) # Zieht aus der Tabelle alle Choice Tasks bei denen bonus = True
# liest in der Spalte die NR (beginnend bei 0) und addiert 1 dazu, damit Choice Tasks bei 1 beginnen
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
    numactions =  [numactions[i]  for i in PM.phase_order] # Ordnen der Actions nach Phasen (array länge = Anzahl Phasen)
    numfeatures = [numfeatures[i] for i in PM.phase_order] # Ordnen der Features nach Phasen

    # Stimuli: Display order of the stimuli
    # Stimulus_order --> Liste welcher Ranges von 0-Anzahl Stimuli pro Block enthält
    self.stimulus_order = np.repeat([range(x) for x in PM.stimuli], PM.blocks, axis = 0).tolist() #Axis wird eine Zahl dazu addiert?
    if (randomize_stimulus_order == 'block'):
      self.stimulus_order = [rnd.sample(x, k=len(x)) for x in self.stimulus_order] #Liste mit Range der Stimuli unterteilt nach Blocks
    self.stimulus_order = [item for sublist in self.stimulus_order for item in sublist] # Unterteilung nach Blocks auflösen
    
    # Action: Position of the action buttons (= stimuli) or of the keys
    num_rounds_per_phase = PM.trials_per_phase # Array enthält pro Phase die Anzahl Stimuli
    num_phases = len(phases)
    
    # Verlängerung Array, sodass jede Choice Task (Stimuli) in jeder Phase die numActions (Anzahl Optionen) zugeordnet bekommt
    # Dann wird Array so umgeändert, das jede Choice Task eine Range von Actions enthält, also hier 0 bis 1 für jeden Choice Task
    self.action_positions = [list(range(x)) for x in np.repeat(numactions, num_rounds_per_phase)]
    
    if ('position/trial' in randomize_action):
      self.action_positions = [rnd.sample(x, k=len(x)) for x in self.action_positions] #Randomisierung der Actions
    # Verschachtelter Array [[1,0],[0,1],...] --> Speichert für jede Choice Task, ob die Optionen links oder rechts angezeigt werden
    
    # Determine position and appearance of features ---------------------------
    numactions_long = np.repeat(numactions, num_rounds_per_phase) # Verlängerung für jeden Stimuli (Choice Task)
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
"""
