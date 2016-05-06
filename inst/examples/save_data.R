

# Time based dataset
toxicityDataTime = read.csv("inst/examples/Fake_data_time.csv", stringsAsFactors = FALSE)
save(toxicityDataTime, file = "data/toxicityDataTime.Rdata")

# Cycle based dataset
toxicityDataCycle = read.csv("inst/examples/Fake_data_cycle.csv", stringsAsFactors = FALSE)
save(toxicityDataCycle, file = "data/toxicityDataCycle.Rdata")
