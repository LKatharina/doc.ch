from os import environ
# Session configs can be accessed from methods in your apps as self.session.config, e.g. self.session.config['participation_fee']

# Default configs will be inherited by all other configs
SESSION_CONFIG_DEFAULTS = dict(
    display_name="Study",
    num_demo_participants=5,
    real_world_currency_per_point=0.25,
    participation_fee=0.000,
    doc=""
)

SESSION_CONFIGS = [
    dict(name='RSFT',
        participation_fee=6.46,
        app_sequence=[
           # 'consent_page',
            'rsft_dfe_experiment',
            #'survey'
            ],
        use_browser_bots = False,
    )
]

# ISO-639 code for languages, e.g. de, fr, ja, ko, zh-hans
LANGUAGE_CODE = 'en-us'
# e.g. EUR, GBP, CNY, JPY
REAL_WORLD_CURRENCY_CODE = 'USD'
USE_POINTS = True

ROOMS = [
    dict(
        name='test_room',
        display_name='Test class',
        participant_label_file='_rooms/econ101.txt',
    ),
    dict(name='live_demo', display_name='Room for live demo (no participant labels)'),
]

ADMIN_USERNAME = 'admin'
# for security, best to set admin password in an environment variable
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')

DEMO_PAGE_INTRO_HTML = """
Demo page to access Otree experiments.
"""

SECRET_KEY = '{{ secret_key }}'

USE_I18N = False

MIDDLEWARE = [
  'django.middleware.security.SecurityMiddleware',
  'whitenoise.middleware.WhiteNoiseMiddleware',
]

STATICFILES_STORAGE = 'whitenoise.storage.CompressedManifestStaticFilesStorage'

# Participant-specific fields accessible across blocks/trials/apps by e.g. player.participant.field_name
PARTICIPANT_FIELDS = [
    'condition',
    'sampling_condition',
    'num_samples',
    'format_condition',
    'num_blocks',
    'bonus_rounds',
    'stimulus_position',
    'img1',
    'img2',
    'max_earnings',
    'decision_number',
    'outcomes',
    'sampling_outcomes',
    'block_number',
    'PM',
    'AM'
]

SESSION_FIELDS = []


DEBUG = False