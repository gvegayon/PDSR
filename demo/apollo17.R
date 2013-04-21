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