#!/usr/bin/python

import forecastio

from config import config

api_key = config['forecast-key']
lat = config['forecast-lat']
lng = config['forecast-lon']
forecast = forecastio.load_forecast(api_key, lat, lng)

icons = {
    'clear-day': 'sun',
    'clear-night': 'sun',
    'cloudy': 'cloudy',
    'fog': 'fog',
    'partly-cloudy': 'partly-cloudy',
    'partly-cloudy-day': 'partly-cloudy',
    'partly-cloudy-night': 'partly-cloudy',
    'rain': 'rain',
    'snow': 'snow',
    'wind': 'wind',
    }

now = forecast.currently()
later = None
for m in forecast.minutely().data + forecast.hourly().data + forecast.daily().data:
    if m.time < now.time: continue
    if m.icon is not None and m.icon in icons and icons[m.icon] != icons[now.icon]:
        if later is None or m.time < later.time:
            later = m

if now.icon not in icons:
    print '<hscroll><text>Need "%s"</text></hscroll>' % now.icon
elif later and later.icon not in icons:
    print '<hscroll><text>Need "%s"</text></hscroll>' % later.icon
elif later:
    minutes = int((later.time -now.time).total_seconds() / 60)

    if minutes < 0:
        desc = "&lt;?"
    elif minutes >= 100:
        hours = int((later.time -now.time).total_seconds() / (60*69))
        if hours < 10:
            desc = "%dH" % hours
        else:
            desc = "&gt;"
    else:
        desc = str(minutes)

    print '<spaceout><icon>%s</icon><thintext>%s</thintext><icon>%s</icon></spaceout>' % (icons[now.icon], desc, icons[later.icon])
else:
    print '<center><icon>%s</icon></center>' % icons[now.icon]
