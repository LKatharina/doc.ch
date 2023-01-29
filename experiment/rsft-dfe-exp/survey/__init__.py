from otree.api import *


class C(BaseConstants):
    NAME_IN_URL = 'study1'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1
    PAGE_TEMPLATE = __name__ + "/template.html"


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


# ENGLISH----------------------------------------------------------------
class Player(BasePlayer):

    age_e = models.IntegerField(
        verbose_name='How old are you?',
        min=18, max=100)

    gender_e = models.StringField(
        choices=['Male', 'Female', 'Non-binary / third gender','Prefer not to state'],
        verbose_name='What is your gender?',
        widget=widgets.RadioSelect)

    language_eng = models.StringField(
        choices = ['Mother tongue', 'Fluent', "Good knowledge", "Basic knowledge"],
        verbose_name='How good is your English knowledge?',
        widget=widgets.RadioSelect)

    income_e = models.IntegerField(
    choices = [
    [0, 'up to 1000'],
    [1, '1001 - 2000'],
    [2, '2001 - 3000'],
    [3, '3001 - 4000'],
    [4, '4001 - 5000'],
    [5, 'more'],
    [99, 'Do not want to answer']
    ],
    verbose_name = 'Which category does your monthly income (in £) after tax fall into? (the amount that is available to you either from work or other sources of income)')


    attention1_e = models.IntegerField(
            label="What was your task in the learning phase?",
            choices=[
                [1, "The task was to meet or to exceed a threshold."],
                [2, "The task was to learn about the hidden probabilities."],
                [3, "There was no task."]
            ],
            widget=widgets.RadioSelect
    )

    attention11_e = models.IntegerField(
        label="What was your task in the learning phase?",
        choices=[
            [1, "The task was to meet or to exceed a threshold."],
            [2, "The task was to learn about the hidden outcomes and probabilities."],
            [3, "There was no task."]
        ],
        widget=widgets.RadioSelect
    )

    attention2_e = models.IntegerField(
        label="What was your task in the choice phase?",
        choices=[
            [1, "The task was to meet or to exceed a threshold."],
            [2, "The task was to learn about the hidden probabilities."],
            [3, "There was no task."]
        ],
        widget=widgets.RadioSelect
    )


    usefulness_e = models.IntegerField(
        choices=[
            [1, 'Yes'],
            [2, 'No'],
        ],
        verbose_name="Is the data you just generated of sufficient quality to be useful for scientific research?",
        widget=widgets.RadioSelect
    )

    usefulness_text_e = models.StringField(
        verbose_name="If you clicked «No» to the question above, please describe shortly why we should not use your data in our analyses?",
        blank=True
    )

    strategy_e = models.LongStringField(
         verbose_name='''
            Can you describe the strategy behind your decisions?''',
     )

    task_clear_e = models.IntegerField(
        choices = [
        [0, 'Not clear'],
        [1, 'Mostly not clear'],
        [2, 'Mostly clear'],
        [3, 'Completely clear']
        ],
        verbose_name = 'Was it clear to you what your task was during this study?',
        widget=widgets.RadioSelect
    )

    open_text_e = models.LongStringField(
        verbose_name='''
             Is there anything you would like us to know? (Optional)''',
        blank=True
    )
    n1e = models.IntegerField(
        verbose_name= "Imagine that we rolled a fair, six-sided die 1,000 times. Out of 1,000 rolls, how many times do you think the die would come up even (2, 4, or 6)?",
        min=1, max=1000,

    )

    n2e = models.IntegerField(
        verbose_name= "In the BIG BUCKS LOTTERY, the chances of winning a $10.00 prize is 1%. What is your best guess about how many people would win a $10.00 prize if 1,000 people each buy a single ticket to BIG BUCKS?",
        min=1, max=1000
    )

    n3e = models.FloatField(
        verbose_name= "In the ACME PUBLISHING SWEEPSTAKES, the chance of winning a car is 1 in 1,000. What percent of tickets to ACME PUBLISHING SWEEPSTAKES win a car?",
    )

    t1e = models.IntegerField(
        choices=[
            [0, "very easy"],
            [1, "easy"],
            [2, "medium"],
            [3, "hard"],
            [4, "very hard"]
        ],
        verbose_name="Please indicate how difficult this threshold appears to you given Option A with the threshold = 9",
        widget=widgets.RadioSelect
    )

    t2e = models.IntegerField(
        choices=[
            [0, "very easy"],
            [1, "easy"],
            [2, "medium"],
            [3, "hard"],
            [4, "very hard"]
        ],
        verbose_name="Please indicate how difficult this threshold appears to you given Option B with the threshold = 24",
        widget=widgets.RadioSelect
    )

    t3e = models.IntegerField(
        choices=[
            [0, "very easy"],
            [1, "easy"],
            [2, "medium"],
            [3, "hard"],
            [4, "very hard"]
        ],
        verbose_name="Please indicate how difficult this threshold appears to you given Option C with the threshold = 37",
        widget=widgets.RadioSelect
    )

# PAGES ===================================================
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
                   "attention11_e",
                   "attention2_e"
                   ]
class End_eng(Page):
  def is_displayed(player):
    return player.round_number == 1
  def vars_for_template(player):
    return {
    "label": player.participant.label,
    "bonus": player.participant.payoff.to_real_world_currency(self.session),
    "mpl_payoff": player.participant.payoff,
    "total_payoff": player.participant.payoff.to_real_world_currency(self.session) + (self.participant.payoff/2)
    }

class numbersense(Page):
    form_model = 'player'
    form_fields = ["n1e", "n2e", "n3e"]

class Threshold_difficulty(Page):
    form_model = 'player'
    form_fields = ["t1e", "t2e", "t3e"]


page_sequence = [
    Demographics_eng,
    attentioncheck_eng,
    #numbersense,
    #Threshold_difficulty,
    Carefulness_eng,
    #End_eng,
]
