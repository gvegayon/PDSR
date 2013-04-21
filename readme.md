### Introduction
PDSR use NASA's FTP service to smoothly access PDS. Through metadata 
file parsing, it helps the data-scientist downloading and describing
data without using the web-browser.

This package was designed during the 2013 version of the NASA's 
International SpaceApps Challenge

Please visit the project home for more information:
https://github.org/gvegayon/PDSR.

or its SpaceApps repo

https://github.org/spaceappcl/team20.

### Details
The package retrieve data sets from 
[NASA's missions](http://pds.jpl.nasa.gov/tools/dsstatus/dsidStatus.jsp?sortOpt1=di.dsid&sortOpt2=&sortOpt3=&sortOpt4=&sortOpt5=&nodename=ALL&col2=dm.msnname&col3=&col4=&col5=&Go=Submit)
into data.frames objects.

Includes special functions for parsing lbl archives into nested lists
and naming and characterizing data (tab files).

Also methods for summarizing and plotting information.

### Motivations
This is an project for the SpaceAppChallenge 2013 (Santiago CL)

### Example
```
# library(PDSR)

# Looking up for Apollo 17
apollo <- dirMissions("apollo 17")

# What data is there
apollo.data <- exploreMission(apollo[1,1])

# Download the data (will download two files called:
# a17_traverse_gravity_data.lbl & a17_traverse_gravity_data.tab)
downLoadMission(apollo.data)

# Read data
apollo.df <- readPDStable("a17_traverse_gravity_data")

summary(apollo.df)

plot(apollo.df)
```
### Authors
George G. Vega (g [dot] vegayon [at] gmail)
Joshua B. Kunst (jbkunst [at] gmail)
