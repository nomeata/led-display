#!/usr/bin/python

import requests
import ast

from config import config

response = requests.get(
    config['nagios-url'],
    auth=(config['nagios-username'], config['nagios-password']),
    verify=False) # no SNI :-(

raw_data = ast.literal_eval(str(response.content))

# state = {'OK': 0, 'WARN': 0, 'CRIT': 0}
#state = [0,0,0]

bad_state = set((1,2))
problems = 0

for l in raw_data:
    if l['service_acknowledged']:
	continue
    if l['service_state'] in bad_state:
	problems += 1

if problems == 0:
    #print "<spaceout><icon>smiley</icon><thintext>%d</thintext><icon>smiley</icon></spaceout>" % state['OK']
    #print "<center><icon>smiley</icon></center>"
    pass
else:
    print "<spaceout><icon>skull</icon><thintext>%d</thintext><icon>skull</icon></spaceout>" % problems



