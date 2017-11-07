

library(robustToxicities)
library(RUnit)


myTestSuite = defineTestSuite("Robust Toxicities Unit Tests",
                              dirs = paste0(path.package("robustToxicities", quiet = FALSE),"/UnitTests"),
                              testFileRegexp = "^test_+")

testResult <- runTestSuite(myTestSuite)

testResult


testResult = runTestFile("C:/Users/pdutton/Dropbox/R/Robust Toxicities/robustToxicities/inst/UnitTests/test_Data_Setup_Queries.R")

testResult
