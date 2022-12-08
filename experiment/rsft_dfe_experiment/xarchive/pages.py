from otree.api import Currency as c, currency_range
from ._builtin import Page, WaitPage
from .models import Constants


# class Prolificid(Page):
#   form_model = 'player'
#   form_fields = ['prolificid', 'browser']
#   def is_displayed(self):
#     return self.round_number == 1


class instr_attentioncheck(Page):
  def is_displayed(self):
    return self.player.phase in ["familiarization"]
  form_model = 'player'
  form_fields = ["a1e"]
  def vars_for_template(self):
    return {
    'duration': Constants.duration,
    }

class instr_sample(Page):
  form_model = 'player'
  def get_form_fields(player):
    if 'dfe_condition' == 0:
      return ["c1e"]
    else:
      return ["c1e", "c4e", "c6e"]
  def is_displayed(self):
    return self.player.phase in ["familiarization"]
  def vars_for_template(self):
    return {
      'num_samples': Constants.num_samples_per_option,
      'num_trials': Constants.num_trials,
      'goal_condition': self.participant.vars['goal_condition'],
      'dfe_condition': self.participant.vars['dfe_condition']
    }

class instr_choice(Page):
  form_model = 'player'
  form_fields = ["c2e", "c3e", "c5e"]
  def is_displayed(self):
    return self.player.phase in ["familiarization"]
  def vars_for_template(self):
    return {
      'num_samples': Constants.num_samples_per_option,
      'num_trials': Constants.num_trials,
      'num_blocks': Constants.num_rounds - Constants.num_familiarization_rounds,
      'dfe_condition': self.participant.vars['dfe_condition']
    }

class instr_summary(Page):
  def is_displayed(self):
    return self.round_number == (self.player.phase in ["familiarization"])
  form_model = 'player'
  form_fields = ["c7e"]
  def vars_for_template(self):
    return {
      'num_samples': Constants.num_samples_per_option,
      'num_trials': Constants.num_trials,
      'dfe_condition': self.participant.vars['dfe_condition']
    }

class instr_starttask(Page):
  def is_displayed(self):
    return self.round_number == (self.player.phase in ["familiarization"])
  def vars_for_template(self):
    return {
      'num_trials': Constants.num_trials,
      'dfe_condition': self.participant.vars['dfe_condition'],
      'num_samples': Constants.num_samples_per_option,
      'num_blocks': Constants.num_rounds - Constants.num_familiarization_rounds
    }

class instr_check(Page):
  def is_displayed(self):
    return self.player.phase in ["familiarization"]
  def vars_for_template(self):
    return {
      'dfe_condition': self.participant.vars['dfe_condition'],
    }
  form_model = 'player'
  def get_form_fields(player):
    if 'dfe_condition':
      return ["l1e", "l2e", "l3e", "l4e"]
    else:
        return ["g1e","g2e", "g3e", "g4e", "g5e"]
        
  

class instr_incentives(Page):
  form_model = 'player'
  form_fields = ["i4e"]
  def is_displayed(self):
    return self.player.phase in ["familiarization"]
  def vars_for_template(self):
    return({
    'participation_fee': self.participant.payoff_plus_participation_fee(),
    'bonus_amount': c(1).to_real_world_currency(self.session),
    'num_trials': Constants.num_trials,
    'dfe_condition': self.participant.vars['dfe_condition'],
    #'bonus_amount': Constants.bonus_amount,
    })

class choice_newblock(Page):
  def is_displayed(self):
    return (self.round_number > 1)
  form_model = 'player'
  def vars_for_template(self):
    context =  self.player.vars_for_template()
    p = self.player
    context.update({
      'currentblock': p.block,
      'budget': p.budget,
      'success': p.get_last_success(),
      'successes': p.update_successes(),
      "pstart": p.state,
      'round_number': self.round_number - 1,
      })
    return context

class choice_sample(Page):
  def is_displayed(self):
    return ((self.player.phase in ["familiarization", "stimuli_easy", "attentioncheck_easy", "stimuli_medium", "attentioncheck_medium", "stimuli_hard", "attentioncheck_hard"]) & self.participant.vars['dfe_condition'])
  form_model = 'player'
  def get_form_fields(self):
    n = (Constants.num_samples_per_option + 1) * Constants.num_actions
    samplefields = ['sample{}'.format(i) for i in range(1, n)]
    drawfields = ['draw{}'.format(i) for i in range(1, n)]
    samplertfields = ['sample_rt_ms{}'.format(i) for i in range(1, n)]
    return samplefields + drawfields + samplertfields
  def vars_for_template(self):
    context = self.player.vars_for_template()
    context.update({
      'outcomes': self.participant.vars['sampling_outcomes'][self.round_number],
      'successes': self.player.get_last_success(),
      'dfe_condition': self.participant.vars['dfe_condition'],
      'sample_condition': int(self.participant.vars['dfe_condition']),
      'round_number': self.round_number - 1,
      'samples_per_option': self.participant.vars['num_samples_per_option']
    })
    return context


class choice(Page):
  def is_displayed(self):
    return (self.player.phase in ["familiarization", "stimuli_easy", "attentioncheck_easy", "stimuli_medium", "attentioncheck_medium", "stimuli_hard", "attentioncheck_hard"])
  form_model = 'player'
  def get_form_fields(self):
    n = Constants.num_trials + 1
    choicefields = ['choice{}'.format(i) for i in range(1, n)]
    statefields = ['state{}'.format(i) for i in range(1, n + 1)]
    rtfields = ['rt_ms{}'.format(i) for i in range(1, n)]
    return choicefields + statefields + rtfields + ['success'] + ['successes']
  def vars_for_template(self):
    context =  self.player.vars_for_template()
    context.update({
      'outcomes': self.participant.vars['outcomes'][self.round_number],
      'successes': self.player.get_last_success(),
      'num_rounds': Constants.num_rounds,
      'num_trials': Constants.num_trials,
      'round_number': self.round_number - 1,
      'sample_condition': int(self.participant.vars['dfe_condition'])
    })
    return context
  def before_next_page(self):
    self.player.draw_bonus()


class Payment(Page):
  def is_displayed(self):
    return self.round_number >= Constants.num_rounds
  def vars_for_template(self):
    return {
      'participation_fee': c(self.session.config['participation_fee']).to_real_world_currency(self.session),
      # 'bonus': c(self.player.bonus).to_real_world_currency(self.session),
      'bonus': c(self.player.draw_bonus()).to_real_world_currency(self.session)
      }

page_sequence = [
  # instr_attentioncheck,
  # instr_choice,
  # choice_newblock,
  choice_sample,
  # choice,
  # instr_check,
  # instr_incentives,
  # instr_summary,
  # instr_starttask
  # Instruction_Choices_eng,
]
