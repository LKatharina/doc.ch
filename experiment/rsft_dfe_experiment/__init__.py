from otree.api import *
import math, csv, itertools, random as rnd, numpy as np
from gettext import gettext as _
from . import exp

author = 'Mathias Durrer, Laura Marbacher, Dr. Jana B. Jarecki'
doc = """
  Risk sensitive foraging in decisions from experience format
"""

# Conceptual overview: =======================================================
# 1) Apps are subessions
# -----------------------------------------------------------------------------
# |                            session                                       |
# |       subsession (app1)       |              subsession (app2)           |
# |  page | page | page | page    |    page  |  page                         |
# -----------------------------------------------------------------------------
# |                           participant                                    |
# |           player              |                  player                  |
# -----------------------------------------------------------------------------
# player = instance of a participant in a subsession
# participant can be player 2 in one subsession, player 1 in the next, etc.
#
# 2) Object hierarchy
# Session
#   Subsession
#     Group
#       Player
# End Conceptual overview =====================================================


class C(BaseConstants):
  NAME_IN_URL = 'study' # how the experiment is called in the browser URL
  PLAYERS_PER_GROUP = None # do we group players? Usually None means not
  NUM_FAMILIARIZATION_ROUNDS = 1 # how many times rounds are familiarization?
  NUM_REPETITIONS = 1 # how many repetitions of stimuli?
  NUM_SAMPLES_PER_OPTION = [3,10] #how many samples per option by sampling condition
  NUM_TRIALS = 3 # in the RSFT-choice, how many trials?
  NUM_ONESHOT = 0 # how many 1-choice trials?
  NUM_MULTITRIAL = NUM_REPETITIONS + NUM_FAMILIARIZATION_ROUNDS  
  NUM_ROUNDS = int(exp.num_rounds) # exp.py calculates number of rounds
  INITIAL_STATE = 0
  NUM_ACTIONS = 2
  DURATION = 55
  BONUS_AMOUNT = 0.25
  ATTENTION_FAIL_ERROR = _("Nicht ganz richtig")
  PAGE_TEMPLATE = __name__ + "/template.html"

class Subsession(BaseSubsession):
  pass

def creating_session(subsession):
  # rounds are subsessions: a round is a subsession
  # conditions: dfe (small-no probs (0), large-no probs (1), small-uncertainty (2), large-uncertainty (3)), dfd (4)
  conditions = itertools.cycle([0, 1, 2, 3, 4])
  sampling_conditions =  [C.NUM_SAMPLES_PER_OPTION[0],
                                          C.NUM_SAMPLES_PER_OPTION[1],
                                          C.NUM_SAMPLES_PER_OPTION[0],
                                          C.NUM_SAMPLES_PER_OPTION[1],
                                          0]
  format_conditions = ["dfe_np", "dfe_np", "dfe", "dfe", "dfd"] # dfe_np = hidden probabilities, dfe = hidden probabilities and outcomes,  dfd = probabilities and outcomes described
  if subsession.round_number == 1:
    for p in subsession.get_players():
      participant = p.participant
      # for testing set this to true
      participant.condition = next(conditions)
      participant.num_samples = sampling_conditions[participant.condition]
      participant.format_condition = format_conditions[participant.condition]
      participant.num_blocks = 999
      participant.PM = exp.Phasemanager(exp.phases, exp.stimuli, exp.blocks, exp.trials)
      participant.AM = exp.Appearancemanager(participant.PM, exp.filepaths, exp.numfeatures, exp.numactions, exp.randomize_feature, exp.randomize_action, exp.randomize_stimulus_order)
  for p in subsession.get_players():
    participant = p.participant
    round_number = subsession.round_number
    phase_number = participant.PM.get_phaseN(round_number)
    phase = participant.PM.get_phaseL(round_number) # phase label
    stimuli = participant.AM.get_stimuli(round_number, phase_number)
    stimulus_position = participant.AM.get_action_position(round_number)
    feature_color = participant.AM.get_feature_appearance(round_number)[0]
    # Store variables
    p.phase = participant.PM.get_phaseL(round_number)
    p.block = subsession.round_number
    p.budget = stimuli[2][0]
    p.stimulus0 = concat_stimulus(0, stimuli)
    p.stimulus1 = concat_stimulus(1, stimuli)
    p.state = stimuli[2][1]
    # this is only if we have one-shot trials in there
    # if (phase in ['critical']):
    #   p.trial = stimuli[2][2]
    # else:
    p.trial = 1
    p.successes = 0

    # Do and store the randomizations
    p.layout_featurecolor = '_'.join(''.join(str(y) for y in x) for x in [feature_color])
    p.layout_stimulusposition_01 = ''.join(str(x) for x in stimulus_position)

    # Initialize variable containers for the blocks
    n = int(C.NUM_ROUNDS + 1)
    if (subsession.round_number == 1):
      subsession.session.vars['instruction_rounds'] = participant.PM.get_instruction_rounds()
      participant.bonus_rounds = participant.PM.get_bonus_rounds()
      participant.stimulus_position = [None] * n
      participant.img1 = [None] * n
      participant.img2 = [None] * n
      participant.max_earnings = [None] * n
      participant.block_number = [None] * n
      #participant.num_blocks = [None] * n
      participant.decision_number = [None] * n
      participant.outcomes = [None] * n
      participant.sampling_outcomes = [None] * n

    participant.stimulus_position[round_number] = stimulus_position

    # Define the names of the sprites for the images
    css_img_orig_position = [
      'sprite_' + p.stimulus0 + '_featurecolor' + p.layout_featurecolor,
      'sprite_' + p.stimulus1 + '_featurecolor' + p.layout_featurecolor
        ]
    participant.img1[round_number] = css_img_orig_position[stimulus_position[0]]
    participant.img2[round_number] = css_img_orig_position[stimulus_position[1]]
    if (phase in exp.phases):
      outcomes_orig_position = [draw_outcomes(x, C.NUM_TRIALS) for x in participant.AM.get_stimuli(round_number, phase_number)[ :2]]
      participant.outcomes[round_number] = [outcomes_orig_position[i] for i in stimulus_position]
      sampling_outcomes_orig_position = [draw_outcomes(x, participant.num_samples) for x in participant.AM.get_stimuli(round_number, phase_number)[ :2]]
      participant.sampling_outcomes[round_number] = [sampling_outcomes_orig_position[i] for i in stimulus_position]
    maxx = max([max(list(map(abs, stimuli[i]))) for i in [0,1]])
    max_earnings = max(maxx * (C.NUM_TRIALS), p.budget)
    # select overall maximum points to be earned to determine the width of the progress bar
    if (round_number == 1):
      last_max_earnings = int(max_earnings)
    else:
      last_max_earnings = participant.max_earnings[(round_number - 1)]
    max_earnings = max(last_max_earnings, max_earnings)
    participant.max_earnings = [max_earnings] * n
    participant.decision_number[round_number] = participant.PM.get_decision_number_in_phase(round_number)
    if (round_number == 1):
      p.successes = 0

def concat_stimulus(i, stimuli):
  y = "_".join( str(x) for x in ['%.0f' % stimuli[i][0], '%.0f' % (stimuli[i][2] * 100), '%.0f' % stimuli[i][1]] )
  return(y)

class Group(BaseGroup):
  pass

# These functions 'make_..._field' generate the (hidden) html input fields
#   which will store the responses of participants
def make_choice_field(trial, blank = False):
  return models.IntegerField(blank = blank)

def make_sample_field(trial):
  return models.IntegerField(
    blank=True,
    doc = "Sampled stimulus in trial" +str(trial) +", this is the stimulus, not the shown stimulus position")

def make_state_field(trial):
  return models.IntegerField(
    doc = "Point state at the beginning of trial" +str(trial))

def make_sample_state_field(trial):
  return models.IntegerField(
    blank=True,
    doc = "Point state at the beginning of trial" +str(trial))

def make_rt_field(trial):
  return models.FloatField(
    doc = "Reaction time  in ms from the end of the page load until the choice, in trial" +str(trial) +" or until submit, in case of instruction pages.")

def make_sample_rt_field(trial):
  return models.FloatField(
    blank=True,
    doc = "Reaction time  in ms from the end of the page load until the choice, in trial" +str(trial) +" or until submit, in case of instruction pages.")


# Attention check quiz
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
      [1, str(C.NUM_TRIALS) + " choices."],
      [2, "10 choices."],
      [3, str(C.DURATION) + " choices."]],
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
    return models.IntegerField(
      label = quiz[key][0],
      choices = ans,
      widget = widgets.RadioSelectHorizontal)

# Answer options switch based on 
def q_task_choices(player):
  choices = make_q()['q_task'][1:]
  if player.participant.format_condition == "dfe_np":
    choices[0][1] = 'Learn about probabilities. ' + choices[0][1]
    choices[1][1] = 'Learn about thresholds. ' + choices[1][1]
    choices[2][1] = 'Learn about options. ' + choices[2][1]
  if player.participant.format_condition == "dfe":
    choices[0][1] = 'Learn about points and probabilities. ' + choices[0][1]
    choices[1][1] = 'Learn about thresholds. ' + choices[1][1]
    choices[2][1] = 'Learn about options. ' + choices[2][1]
  rnd.shuffle(choices)
  return choices

def q_view_choices(player):
  choices = make_q()['q_view'][1:]
  if player.participant.format_condition == "dfe_np":
    choices[0][1] = 'Learn about the probabilities of each option by ' + str(player.participant.num_samples) + " draws from each option."
    choices[1][1] = 'Learn about the probabilities of each option by ' + str(player.participant.num_samples) + " draws from both options."
  if player.participant.format_condition == "dfe":
    choices[0][1] = 'Learn about the points and the probabilities of each option by ' + str(player.participant.num_samples) + " draws from each option."
    choices[1][1] = 'Learn about the points and the probabilities of each option by ' + str(player.participant.num_samples) + " draws from both options."
  rnd.shuffle(choices)
  return choices


##############################################################################
# Variables we want to export in the database
##############################################################################
class Player (BasePlayer):
  # prolificid = models.StringField(doc = "ID of the survey provider")
  condition = models.FloatField(doc = "0 = dfe_np-small, 1 = dfe_np-large, 2 = dfe-small, 3 = dfe-large, 4 = description")
  format_condition = models.FloatField(doc = "dfe_np = dfe with hidden probabilities, 1 = dfe with hidden probabilities and outcomes, 2 = dfd")
  num_samples = models.FloatField(doc = "3 = small sample (3 per option), 10 = large sample (10 per option), 0 = dfd")
  # Comprehension question fields
  # the name of the question field MUST equal the string ("s" in make_q("s"))
  # and the string must be defined in the make_q() function above
  q_task = make_q("q_task")
  q_trials = make_q("q_trials")
  q_options = make_q("q_options")
  q_outcomes = make_q("q_outcomes")
  q_samples = make_q("q_samples")
  q_view = make_q("q_view")
  q_reach = make_q("q_reach")
  # More fields
  browser = models.StringField(doc = "Browser and version", blank = True)
  phase = models.StringField(initial = '')
  block = models.IntegerField(doc = "Current block")
  trial = models.FloatField(doc = "Current choice number")
  stimulus0 = models.StringField(doc = "Risky gamble, format x1_p1_x2")
  stimulus1 = models.StringField(doc = "Risky gamble, format x1_p2_x2")
  state = models.FloatField(doc = "Accumulated points before current decision")
  budget = models.FloatField(doc = "Point goal/threshold in current block")
  sample1 = make_sample_field(1)
  sample2 = make_sample_field(2)
  sample3 = make_sample_field(3)
  sample4 = make_sample_field(4)
  sample5 = make_sample_field(5)
  sample6 = make_sample_field(6)
  sample7 = make_sample_field(7)
  sample8 = make_sample_field(8)
  sample9 = make_sample_field(9)
  sample10 = make_sample_field(10)
  sample11 = make_sample_field(11)
  sample12 = make_sample_field(12)
  sample13 = make_sample_field(13)
  sample14 = make_sample_field(14)
  sample15 = make_sample_field(15)
  sample16 = make_sample_field(16)
  sample17 = make_sample_field(17)
  sample18 = make_sample_field(18)
  sample19 = make_sample_field(19)
  sample20 = make_sample_field(20)
  sample21 = make_sample_field(21)
  sample22 = make_sample_field(22)
  sample23 = make_sample_field(23)
  sample24 = make_sample_field(24)
  sample25 = make_sample_field(25)
  sample26 = make_sample_field(26)
  sample27 = make_sample_field(27)
  sample28 = make_sample_field(28)
  sample29 = make_sample_field(29)
  sample30 = make_sample_field(30)
  sample31 = make_sample_field(31)
  sample32 = make_sample_field(32)
  sample33 = make_sample_field(33)
  sample34 = make_sample_field(34)
  sample35 = make_sample_field(35)
  sample36 = make_sample_field(36)
  sample37 = make_sample_field(37)
  sample38 = make_sample_field(38)
  sample39 = make_sample_field(39)
  sample40 = make_sample_field(40)
  draw1 = make_sample_state_field(1)
  draw2 = make_sample_state_field(2)
  draw3 = make_sample_state_field(3)
  draw4 = make_sample_state_field(4)
  draw5 = make_sample_state_field(5)
  draw6 = make_sample_state_field(6)
  draw7 = make_sample_state_field(7)
  draw8 = make_sample_state_field(8)
  draw9 = make_sample_state_field(9)
  draw10 = make_sample_state_field(10)
  draw11 = make_sample_state_field(11)
  draw12 = make_sample_state_field(12)
  draw13 = make_sample_state_field(13)
  draw14 = make_sample_state_field(14)
  draw15 = make_sample_state_field(15)
  draw16 = make_sample_state_field(16)
  draw17 = make_sample_state_field(17)
  draw18 = make_sample_state_field(18)
  draw19 = make_sample_state_field(19)
  draw20 = make_sample_state_field(20)
  draw21 = make_sample_state_field(21)
  sample_rt_ms1 = make_sample_rt_field(1)
  sample_rt_ms2 = make_sample_rt_field(1)
  sample_rt_ms3 = make_sample_rt_field(1)
  sample_rt_ms4 = make_sample_rt_field(1)
  sample_rt_ms5 = make_sample_rt_field(1)
  sample_rt_ms6 = make_sample_rt_field(1)
  sample_rt_ms7 = make_sample_rt_field(1)
  sample_rt_ms8 = make_sample_rt_field(1)
  sample_rt_ms9 = make_sample_rt_field(1)
  sample_rt_ms10 = make_sample_rt_field(1)
  sample_rt_ms11 = make_sample_rt_field(1)
  sample_rt_ms12 = make_sample_rt_field(1)
  sample_rt_ms13 = make_sample_rt_field(1)
  sample_rt_ms14 = make_sample_rt_field(1)
  sample_rt_ms15 = make_sample_rt_field(1)
  sample_rt_ms16 = make_sample_rt_field(1)
  sample_rt_ms17 = make_sample_rt_field(1)
  sample_rt_ms18 = make_sample_rt_field(1)
  sample_rt_ms19 = make_sample_rt_field(1)
  sample_rt_ms20 = make_sample_rt_field(1)
  sample_rt_ms21 = make_sample_rt_field(1)
  choice_safe1 = make_choice_field(1)
  choice_safe2 = make_choice_field(2)
  choice_safe3 = make_choice_field(3)
  state1  = make_state_field(1)
  state2  = make_state_field(2)
  state3  = make_state_field(3)
  state4  = make_state_field(4)
  rt_ms1 = make_rt_field(1)
  rt_ms2 = make_rt_field(1)
  rt_ms3 = make_rt_field(1)

  success = models.IntegerField(doc = "Indicator if in the current block the earnings requirement (budget) was reached, 1 if yes, 0 otherwise")
  # outcome = models.IntegerField(doc = "Randomly drawn outcome of the chosen option given the choice in this trial")
  successes = models.FloatField(initial = 0, doc = "Count of the total number of blocks where the earnings requirement (budget) was reached")
  rt_ms = models.FloatField(doc = "Reaction time from the end of the page load until the choice or until submit, in case of instruction pages.")
  layout_featurecolor = models.StringField(doc = "Layout: Randomized feature colors per trial (light vs dark grey), 01 means that in this trial feature x1 was light grey and feature x2 dark grey, 10 means that x1 was dark grey and x2 light grey.")
  layout_stimulusposition_01 = models.StringField(doc = "Layout: Randomized stimulus position per trial (left vs right). 01 means that stimulus1 was shown left, 10 means that stimulus1 was shown right.")



def draw_outcomes(action, size):
  probs = action[2: ][1]
  indices = np.random.binomial(n=1, p=probs, size=size)
  #indices = [0, 1, 0, 1, 1, 0, 1, 0, 1, 1]
  x = action[ :2]
  res = [x[i] for i in indices]
  return res

def get_last_state(player):
  if (player.round_number > 1):
    last = player.in_round(player.round_number - 1)
    return last.state + last.outcome
  else:
    return player.state

@staticmethod
def update_successes(player):
  n_ignore = C.NUM_FAMILIARIZATION_ROUNDS
  successes = 0
  if (player.round_number >  n_ignore):
    successes = sum([p.success for p in player.in_rounds(n_ignore + 1, player.round_number - 1)])
    player.successes = successes
  return(successes)
  
def get_last_success(player):
  round_num_ignore = 1 + C.NUM_FAMILIARIZATION_ROUNDS
  if (player.round_number > round_num_ignore):
    return player.in_round(player.round_number - 1).success
  else:
    return 0

# Variables accessable from across all the html pages
def get_vars(player):
  n = player.round_number
  participant = player.participant
  return dict(
    num_blocks = C.NUM_ROUNDS - C.NUM_FAMILIARIZATION_ROUNDS,
    condition = participant.condition,
    condition_num = int(participant.condition),
    format_condition = participant.format_condition,
    num_samples = participant.num_samples,
    #int(C.NUM_SAMPLES_PER_OPTION[participant.sampling_condition]),
    num_trials = C.NUM_TRIALS,
    img1 = participant.img1[n],
    img2 = participant.img2[n],
    stimulus_position = participant.stimulus_position[n],
    state = player.state,
    budget = player.budget,
    trial = player.trial,
    max_earning = participant.max_earnings[n],
    max_less_state = participant.max_earnings[n] - 0,
    decision_number = participant.decision_number[n],
    multitrial = player.phase in ['familiarization', 'training']
    )

# Sets player.payoff to 1, player.payoff is automatically summed up in the end
def draw_bonus(player):
  if player.round_number in player.participant.bonus_rounds:
    player.payoff = player.success











###############################################################################
# PAGES
###############################################################################
# class Prolificid(Page):
#   form_model = 'player'
#   form_fields = ['prolificid', 'browser']
#   def is_displayed(player):
#     return self.round_number == 1

def get_errors(player, values):
  errors = dict()
  quiz = make_q()
  solutions = { k: quiz[k][1][0] for k in values.keys() }
  for s in solutions:
    if values[s] != solutions[s]:
      errors[s] = 'Incorrect. Please re-read the instructions.'
  return(errors)


class Consent_eng(Page):
  def is_displayed(self):
    return self.round_number == 1
  def vars_for_template(self):
    return {
    'participation_fee': self.participant.payoff_plus_participation_fee(),
    'real_world_currency_per_point': c(1).to_real_world_currency(self.session),
    'example_pay': c(12).to_real_world_currency(self.session),
    'num_vouchers': 20,
    "duration": C.DURATION
    }

class instructions1(Page):
  def is_displayed(player):
    return player.phase in ["familiarization"]
  form_model = 'player'
  form_fields = ['q_task', 'q_trials', 'q_options']
  def vars_for_template(player):
    form_fields = ['q_task', 'q_trials', 'q_options']
    rnd.shuffle(form_fields)
    return dict(form_fields = form_fields)
  def error_message(player, values):
    get_errors(player, values)
  def vars_for_template(player):
    return get_vars(player)

class instructions2(Page):
  def is_displayed(player):
    return player.phase in ["familiarization"]
  form_model = 'player'
  def get_form_fields(player):
    return ["q_outcomes", "q_samples", "q_view", "q_reach"]
  def error_message(player, values):
    get_errors(player, values)
  def vars_for_template(player):
    return dict(image_path = "img/" + player.participant.format_condition + "/instr2.png")


# class newblock(Page):
#   def is_displayed(player):
#     return player.round_number > 1
#   form_model = 'player'
#   def vars_for_template(player):
#     context =  player.vars_for_template()
#     return context

class sample(Page):
  @staticmethod
  def is_displayed(player):
    return player.participant.condition < 4
  form_model = 'player'
  def get_form_fields(player):
    n = (player.participant.num_samples + 1) * C.NUM_ACTIONS
    samplefields = ['sample{}'.format(i) for i in range(1, n)]
    drawfields = ['draw{}'.format(i) for i in range(1, n)]
    samplertfields = ['sample_rt_ms{}'.format(i) for i in range(1, n)]
    return samplefields + drawfields + samplertfields
  def vars_for_template(player):
    v =  get_vars(player)
    v.update(
      outcomes = player.participant.sampling_outcomes[player.round_number],
      successes = get_last_success(player),
      num_rounds = C.NUM_ROUNDS,
      round_number = player.round_number - 1)
    return v

class choice(Page):
  form_model = 'player'
  def get_form_fields(self):
    n = C.NUM_TRIALS + 1
    choicefields = ['choice_safe{}'.format(i) for i in range(1, n)]
    statefields = ['state{}'.format(i) for i in range(1, n + 1)]
    rtfields = ['rt_ms{}'.format(i) for i in range(1, n)]
    return choicefields + statefields + rtfields + ['success'] + ['successes']
  def vars_for_template(player):
    v =  get_vars(player)
    v.update(
      outcomes = player.participant.outcomes[player.round_number],
      successes = get_last_success(player),
      num_rounds = C.NUM_ROUNDS,
      round_number = player.round_number - 1)
    return v
  @staticmethod
  def before_next_page(player, timeout_happened):
    draw_bonus(player)


# class Payment(Page):
#   def is_displayed(player):
#     return player.round_number >= C.num_rounds
#   def vars_for_template(player):
#     return {
#       'participation_fee': c(self.session.config['participation_fee']).to_real_world_currency(self.session),
#       # 'bonus': c(self.player.bonus).to_real_world_currency(self.session),
#       'bonus': c(self.player.draw_bonus()).to_real_world_currency(self.session)
#       }

page_sequence = [
  #instructions1,
  #instructions2,
  # newblock,
  sample,
  choice
]