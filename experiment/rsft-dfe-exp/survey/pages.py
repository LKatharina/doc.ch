from otree.api import Currency as c, currency_range

from ._builtin import Page, WaitPage
from .models import Constants


class Demographics(Page):
    form_model = 'player'
    form_fields = ['age',
                   'gender',
                   'language_german',
                   "education",
                   "education_major",
                   "income",
                   "risk"]


class Carefulness(Page):
    form_model = 'player'
    form_fields = ["task_clear",
                   'usefulness',
                   'usefulness_text',
                   "open_text"]


class attentioncheck(Page):
    form_model = 'player'
    form_fields = ['strategy',
                   "attention1",
                   "attention2"
                   ]
class End(Page):
  def is_displayed(self):
    return self.round_number == 1
  def vars_for_template(self):
    return {
    "label": self.participant.label,
    'bonus': c(self.player.draw_bonus()).to_real_world_currency(self.session)
    }

class End_Vorstudie(Page):
  def is_displayed(self):
    return self.round_number == 1
  def vars_for_template(self):
    return {
      "label": self.participant.label
    }




# class CognitiveReflectionTest(Page):
#     form_model = 'player'
#     form_fields = ['crt_bat',
#                    'crt_widget',
#                    'crt_lake']

# ENGLISCH ------------------------------------------------
class Demographics_eng(Page):
    form_model = 'player'
    form_fields = ['age_e',
                   'gender_e',
                   'language_eng',
                   "income_e"]


class Carefulness_eng(Page):
    form_model = 'player'
    form_fields = ["task_clear_e",
                   'usefulness_e',
                   'usefulness_text_e',
                   "open_text_e"]


class attentioncheck_eng(Page):
    form_model = 'player'
    form_fields = ['strategy_e',
                   "attention1_e",
                   "attention2_e"
                   ]
class End_eng(Page):
  def is_displayed(self):
    return self.round_number == 1
  def vars_for_template(self):
    return {
    "label": self.participant.label,
    "bonus": self.participant.payoff.to_real_world_currency(self.session),
    "mpl_payoff": self.participant.payoff,
    "total_payoff": self.participant.payoff.to_real_world_currency(self.session) + (self.participant.payoff/2)
    }

class numbersense(Page):
    form_model = 'player'
    form_fields = ["n1e", "n2e", "n3e"]

class Threshold_difficulty(Page):
    form_model = 'player'
    form_fields = ["t1e", "t2e", "t3e"]



#
# page_sequence = [
#     Demographics,
#     attentioncheck,
#     Carefulness,
#     #End,
#     End_Vorstudie,
# ]

page_sequence = [
    Demographics_eng,
    attentioncheck_eng,
    numbersense,
    Threshold_difficulty,
    Carefulness_eng,
    End_eng,
]
