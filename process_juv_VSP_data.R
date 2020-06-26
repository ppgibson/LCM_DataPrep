## JUVENILE ABUNDANCE: Process VSP DB data
# Take data export prepared by KB and reformat into standardized format
# for LCM data request.

## ---- SETUP ---- 
# Libraries
  library(lubridate)
  library(dplyr)
  library(reshape2)

# Make table display NAs
  table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)


## ---- PROCESS DATA ---- 
juv.raw <- read.csv("JuvOut_fromKBexportt.csv", stringsAsFactors=FALSE)
  
juv <- juv.raw
colnames(juv) <- tolower(colnames(juv))

# Population
juv <- rename(juv, population=popabrev)
table(juv$population)
  juv$population[juv$population=="Catherine Creek"] <- "Catherine"
  juv$population[juv$population=="Grande Ronde Upper Mainstem"] <- "UGR"
  juv$population[juv$population=="Lostine River"] <- "Lostine"
  juv$population[juv$population=="Minam River"] <- "Minam"
  
# Years
juv <- rename(juv,
              brood.year = broodyear,
              mig.year = outmigrationyear)

# Comments
juv <- rename(juv, comments=comments_migrants)

# Melt to full long form
juv.long <- melt(data = select(juv, population, brood.year, mig.year, starts_with("migrants"), comments), 
                 id.vars = c("population", "brood.year", "mig.year", "comments"))

  # Remove the total CI value
  table(juv.long$variable)
  juv.long <- filter(juv.long, variable!="migrantstotal_95ci")
  juv.long$variable <- droplevels(juv.long$variable)
  
  # Add season variable
  juv.long$season <- NA
    juv.long$season[grep(pattern="late", x=juv.long$variable)] <- "spring"
    juv.long$season[grep(pattern="early", x=juv.long$variable)] <- "fall"
    
  # Clarify type of value
  juv.long$dat.type <- "abund.est"
    juv.long$dat.type[grep(pattern="_95ci", x=juv.long$variable)] <- "abund.ci"
    
  # Cast back out, partially
  juv.data <- dcast(juv.long, 
                    population + brood.year + mig.year + season + comments ~ dat.type, 
                    value.var="value")
  
# Recalculate SE from reported +/- CI value
  juv.data <- mutate(juv.data,
                     abund.se = abund.ci/1.96)
  
## ---- OUTPUT ----
# Select and reorder fields
  juv.abund.data <- select(juv.data, 
                           population, brood.year, mig.year, season,
                           abund.est, abund.se, comments)
  juv.abund.data <- mutate(juv.abund.data, 
                           abund.se = round(abund.se, 1))
  
  juv.abund.data <- arrange(juv.abund.data, population, brood.year, season)
  
# Write output
  write.csv(juv.abund.data, "data_JuvAbund_cleaned.csv", row.names=FALSE)
