
# Looking up for mars missions
apollo <- dirMissions("apollo 17")

# What data is there
apollo.data <- exploreMission(apollo[1,1])

# Download the data
downLoadMission(apollo.data)

# Read data
apollo.df <- readPDStable("a17_traverse_gravity_data")