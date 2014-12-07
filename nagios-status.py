#!/usr/bin/python

import requests

from config import config

response = requests.get(
    config['nagios-url'],
    auth=(config['nagios-username'], config['nagios-password']),
    verify=False) # no SNI :-(
brokenjsontxt = str(response.content)

jsontxt = brokenjsontxt.replace(u'''['service_state', 'service_description', 'svc_plugin_output', 'service_icons', 'svc_state_age', 'svc_check_age', 'perfometer'],''', '')

import simplejson
data = simplejson.loads(jsontxt)

state = {'OK': 0, 'WARN': 0, 'CRIT': 0}

for l in data:
    s = l[0]
    if s not in state:
        state[s] = 0
    state[s] += 1

problems = state['CRIT'] + state['WARN']

if problems == 0:
    #print "<spaceout><icon>smiley</icon><thintext>%d</thintext><icon>smiley</icon></spaceout>" % state['OK']
    #print "<center><icon>smiley</icon></center>"
    pass
else:
    print "<spaceout><icon>skull</icon><thintext>%d</thintext><icon>skull</icon></spaceout>" % problems



