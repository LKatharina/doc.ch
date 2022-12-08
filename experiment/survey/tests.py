from otree.api import Currency as c, currency_range

from . import pages
from ._builtin import Bot
from .models import Constants
import time
from otree.api import Submission

class PlayerBot(Bot):

    def play_round(self):
            yield pages.Demographics_eng, dict(
                age_e = 24,
                gender_e = "Male",
                language_eng = 'Mother tongue',
                income_e = 1,
                risk_e = 8
            )
           # time.sleep(2)


            yield pages.attentioncheck_eng, dict(
                strategy_e = "hallo",
                attention1_e = 1,
                attention2_e = 2,
            )
            #time.sleep(2)

            yield pages.Carefulness_eng, dict(
                eyesight_e=1,
                task_clear_e=1,
                usefulness_e = 1,
                usefulness_text_e="Hallo",
                open_text_e="Hallo",
            )
           # time.sleep(2)
            yield pages.End_eng

