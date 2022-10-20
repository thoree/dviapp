import json
import subprocess
from collections import defaultdict

TRANSLATION_JSON_PATH = 'translation.json'
WEBLATE_KEY_JSON_PATH = 'translation/en.json'
LANGUAGES = ['en', 'es']
WEBLATE_TRANSLATION_FILES = ['translation/en.json', 'translation/es.json']

# generate translation keys
subprocess.run(['R', '-e', "library(shiny.i18n); create_translation_file('app.R', output='translation.json')"])

with open(TRANSLATION_JSON_PATH, mode='r') as f:
  t_data = json.load(f)
  
# make sure we get all keys for the monolingual source file
default_keys = {x['key']: x['key'] for x in t_data['translation']}

with open(WEBLATE_KEY_JSON_PATH, mode='r') as f:
  en_data = json.load(f)
  
for key in default_keys:
  if key not in en_data:
    en_data[key] = key

# record new and old keys back to monolingual source file
with open(WEBLATE_KEY_JSON_PATH, mode='w') as f:
  json.dump(en_data, f, indent=2)

# rebuild the ZIP file
subprocess.run(['rm', '-f', 'translation.zip'])
subprocess.run(['zip', '-r', 'translation.zip', 'translation'])


# rebuild the source translation file in the format recognized by shiny.i18n
combined_translations = defaultdict(dict)

# add all keys to combined translations to allow shiny.i18n to perform lookups
for key in default_keys:
  combined_translations[key] = {'key': key}

for lang in LANGUAGES:
  with open(f'translation/{lang}.json', mode='r') as f:
    strings = json.load(f)
    for k, v in strings.items():
      combined_translations[k][lang] = v
      
with open(TRANSLATION_JSON_PATH, mode='w') as f:
  json.dump({
    'languages': ['key'] + LANGUAGES,
    'translation': list(combined_translations.values())
  }, f, indent=2)

    
