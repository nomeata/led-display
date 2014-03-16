#!/bin/bash
cat <<__END__ > display.xml.new
<alternate transition="scrollUp">
$(./nagios-status.py)
$(./forecast.py)
</alternate>
__END__
mv display.xml.new display.xml
