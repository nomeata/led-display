#!/bin/bash
cat <<__END__ > display.xml.new
<alternate transition="scrollUp">
$(./forecast.py)
$(./nagios-status.py)
$(./yucata.sh)
</alternate>
__END__
mv display.xml.new display.xml
