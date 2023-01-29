from otree.api import Currency as c, currency_range
from . import pages
from ._builtin import Bot
import time
from otree.api import Submission
from .models import Constants


class PlayerBot(Bot):

    def play_round(self):

        if self.round_number == 1:
            yield pages.Consent_eng,
            #time.sleep(2)

        if self.round_number == 1:
            yield pages.General_Instruction_eng,
            #time.sleep(2)

        if self.round_number == 1:
            yield pages.Coverstory_eng, dict(
                c1e=1,
                c2e=True,
                c3e=True,
                c4e=2,
                c5e=True,
                c6e=True,
                c7e=True,
            )
           # time.sleep(2)


        if self.round_number == 1:
            yield pages.Coverstory_gain1_eng,
        if self.round_number == 1:
            yield pages.Coverstory_gain2_eng,
            #time.sleep(2)

        if self.round_number == 1:
            yield Submission(pages.Choices, dict(
                choice1=1, choice2=1, choice3=1, choice4=1, choice5=1,
                state1=0, state2=0, state3=0, state4=0, state5=0, state6=0,
                rt_ms1=123, rt_ms2=123, rt_ms3=123, rt_ms4=123, rt_ms5=123,
                success=1, successes=0
                ),check_html=False)
            #time.sleep(2)

        if self.round_number == 1:
            yield pages.Coverstory_check_gain_eng, dict(
                g1e=3,
                g2e=0,
                g3e=-13,
                g4e=9,
            )
           # time.sleep(2)

        if self.round_number == 2:
            yield pages.Coverstory_loss1_eng,
           # time.sleep(2)
        if self.round_number == 2:
            yield pages.Coverstory_loss2_eng,
           # time.sleep(2)
        if self.round_number == 2:
            yield Submission(pages.Choices, dict(
                choice1=1, choice2=1, choice3=1, choice4=1, choice5=1,
                state1=0, state2=0, state3=0, state4=0, state5=0, state6=0,
                rt_ms1=123, rt_ms2=123, rt_ms3=123, rt_ms4=123, rt_ms5=123,
                success=1, successes=0
                ), check_html=False)
            time.sleep(2)

        if self.round_number == 2:
            yield pages.Coverstory_check_loss_eng, dict(
                l1e=0,
                l4e=21,
                l2e=11,
                l3e=60,
            )
          #  time.sleep(2)

        if self.round_number == 2:
            yield pages.Coverstory_sum_eng,
           # time.sleep(2)

        if self.round_number == 3:
            yield pages.Incentives_eng, dict(
                i4e=3,
            )
            #time.sleep(2)

        if self.round_number in [3,24,45,66]:
            yield pages.Block_Instruction1_eng,

        if self.round_number in [3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
                                27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,
                                49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,
                                73,74,75,76,77,78,79,80,81,82,83,84,85,86]:
            yield pages.NewBlock_eng, dict(
            )
            #time.sleep(1)
        if self.round_number in [3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
                                27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,
                                49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,
                                73,74,75,76,77,78,79,80,81,82,83,84,85,86]:
            yield Submission(pages.Choices, dict(
                choice1=1, choice2=1, choice3=1, choice4=1, choice5=1,
                state1=0, state2=0, state3=0, state4=0, state5=0, state6=0,
                rt_ms1=123, rt_ms2=123, rt_ms3=123, rt_ms4=123, rt_ms5=123,
                success=1, successes=0
            ), check_html=False)
            #time.sleep(2)

        #if self.round_number in [3,24,45,66]:
            #yield pages.Block_Instruction1_eng1

        #if self.player_phase in ['positive_gain', "positive_loss", "negative_gain", "negative_loss"]:
            #yield pages.NewBlock_eng

        #if self.player_phase in ["familiarization_gain", "familiarization_loss",'positive_gain', "positive_loss", "negative_gain", "negative_loss"]:
            #yield pages.Choices












