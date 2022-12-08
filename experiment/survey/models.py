from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer,
    Currency as c, currency_range
)
import random


class Constants(BaseConstants):
    name_in_url = 'survey'
    players_per_group = None
    num_rounds = 1


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass

# German -------------------------------------------------------------------------------------
class Player(BasePlayer):
    age = models.IntegerField(
        verbose_name='Wie alt sind Sie?',
        min=18, max=100)

    gender = models.StringField(
        choices=['Männlich', 'Weiblich', 'Keine Angaben'],
        verbose_name='Was ist Ihr Geschlecht?',
        widget=widgets.RadioSelect)

    language_german = models.StringField(
        choices = ['Muttersprache', 'Verhandlungssicher', "Fliessend", "Grundkenntnisse"],
        verbose_name='Wie gut sind Ihre Deutschkenntnisse?')

    education = models.StringField(
        choices = ['Obligatorische Schule', 'Berufliche Grundausbildung (Lehre)',"Fachmittelschule" ,"Berufsmaturität", "gymnasiale Maturität", "Fachhochschule", "universitäre Hochschule oder höher", "Anderes"],
        verbose_name='Was ist Ihre höchste abgeschlossene Ausbildung?')

    education_major =  models.StringField(
        verbose_name='''
        Falls Sie studieren oder studiert haben, was ist/war Ihr Studienfach?''',
        blank=True
    )

    risk = models.IntegerField(
        choices=[0,1,2,3,4,5,6,7,8,9,10],
        label ='Wie schätzen Sie sich persönlich ein: Sind Sie im Allgemeinen ein risikobereiter Mensch oder versuchen Sie, Risiken zu vermeiden? ( 0 = gar nicht risikobereit, 10 = sehr risikobereit)',
        widget = widgets.RadioSelectHorizontal)

    income = models.IntegerField(
         choices = [
         [0, 'Bis 1000'],
         [1, '1001 - 2000'],
         [2, '2001 - 3000'],
         [3, '3001 - 4000'],
         [4, '4001 oder mehr'],
         [99, 'Keine Angabe']
         ],
         verbose_name = 'In welcher Kategorie befindet sich Ihr monatliches Nettoeinkommen?')

    attention1 = models.IntegerField(
        widget=widgets.RadioSelect,
        label="Was war Ihr Ziel beim Punkte sammeln?",
        choices=[
            [1, "Den Schwellenwert zu erreichen oder zu überschreiten."],
            [2, "Den Schwellenwert nicht zu erreichen und nicht zu überschreiten."],
            [3, "Es gibt keine Zielvorgabe."]
        ])

    attention2 = models.IntegerField(
        widget=widgets.RadioSelect,
        label="Was war Ihr Ziel beim Punkte abgeben?",
        choices=[
            [1, "Den Schwellenwert zu erreichen oder zu unterschreiten."],
            [2, "Den Schwellenwert nicht zu unterschreiten."],
            [3, "Es gibt keine Zielvorgabe."]
        ])

    usefulness = models.IntegerField(
        choices=[
            [1, 'Ja'],
            [2, 'Nein'],
        ],
        verbose_name="Haben Ihre Daten ausreichende Qualität um für wissenschaftliche Zwecke verwendet zu werden?",
        widget=widgets.RadioSelectHorizontal)

    usefulness_text = models.StringField(
        verbose_name='''
        Falls Sie bei der obigen Frage "Nein" angeklickt haben. Bitte beschreiben Sie kurz den Grund dafür, dass Ihre Daten keine ausreichende Qualität haben.''',
        blank=True
    )

    open_text = models.LongStringField(
        verbose_name='''
            Hier haben Sie noch Platz für allfällige Anmerkungen, Kritikpunkte oder sonstiges.''',
        blank=True
    )

    strategy = models.LongStringField(
        verbose_name='''
        Können Sie beschreiben, wie Sie entschieden haben, welche Option Sie wählen?''',
    )

    task_clear = models.IntegerField(
        choices = [
        [0, 'Unklar'],
        [1, 'Teilweise unklar'],
        [2, 'Grösstenteils klar'],
        [3, 'Absolut klar']
        ],
        verbose_name = 'Wie klar war Ihnen die Aufgabe während der Studie?',
        widget=widgets.RadioSelectHorizontal)


# ENGLISH----------------------------------------------------------------
    age_e = models.IntegerField(
        verbose_name='How old are you?',
        min=18, max=100)

    gender_e = models.StringField(
        choices=['Male', 'Female', 'Prefer not to state'],
        verbose_name='What is your gender?',
        widget=widgets.RadioSelect)

    language_eng = models.StringField(
        choices = ['Mother tongue', 'Fluent', "Good knowledge", "Basic knowledge"],
        verbose_name='How good is your English knowledge?',
        widget=widgets.RadioSelect)

    income_e = models.IntegerField(
    choices=[
            [0, 'up to $2,499'],
            [1, '$2,500 to $4,999'],
            [2, '$5,000 to $7,499'],
            [3, '$7,500 to $9,999'],
            [4, '$10,000 to $12,499'],
            [5, '$12,500 to $14,999'],
            [6, '$15,000 to $17,499'],
            [7, '$17,500 to $19,999'],
            [8, '$20,000 to $22,499'],
            [9, '$22,500 to $24,999'],
            [10, '$25,000 to $27,499'],
            [11, '$27,500 to $29,999'],
            [12, '$30,000 to $32,499'],
            [13, '$32,500 to $34,999'],
            [14, '$35,000 to $37,499'],
            [15, '$37,500 to $39,999'],
            [16, '$40,000 to $42,499'],
            [17, '$42,500 to $44,999'],
            [18, '$45,000 to $47,499'],
            [19, '$47,500 to $49,999'],
            [20, '$50,000 to $52,499'],
            [21, '$52,500 to $54,999'],
            [22, '$55,000 to $57,499'],
            [23, '$57,500 to $59,999'],
            [24, '$60,000 to $62,499'],
            [25, '$62,500 to $64,999'],
            [26, '$65,000 to $67,499'],
            [27, '$67,500 to $69,999'],
            [28, '$70,000 to $72,499'],
            [29, '$72,500 to $74,999'],
            [30, '$75,000 to $77,499'],
            [31, '$77,500 to $79,999'],
            [32, '$80,000 to $82,499'],
            [33, '$82,500 to $84,999'],
            [34, '$85,000 to $87,499'],
            [35, '$87,500 to $89,999'],
            [36, '$90,000 to $92,499'],
            [37, '$92,500 to $94,999'],
            [37, '$95,000 to $97,499'],
            [37, '$97,500 to $99,999'],
            [37, '$100,000 and over'],
            [99, 'Do not want to answer'],
        ],
        verbose_name='Which category does your annual income (in $) after tax fall into? (the amount that is available to you either from work or other sources of income)',
        widget=widgets.RadioSelect
    )

    attention1_e = models.IntegerField(
            label="What was your task in the learning phase?",
            choices=[
                [1, "The task was to meet or to exceed a threshold."],
                [2, "The task was to learn about the hidden probabilities."],
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
