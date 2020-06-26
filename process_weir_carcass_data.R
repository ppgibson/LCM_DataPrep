## INDIVIDUAL ADULT DATA: CARCASS AND WEIR
# Take exported data sets from the weir and carcass databases; process them to standardize field 
# names and codes; combine data from the two weir databases; and export clean tables for sharing.

## ---- SETUP ---- 
# Libraries
  library(lubridate)
  library(dplyr)

# Make table display NAs
  table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)


## ---- WEIR DATA ---- 
ugr.raw <- read.csv("data_weir_CCUGR_20200518.csv", stringsAsFactors=FALSE)  #includes UGR and CC weirs
los.raw <- read.csv("data_weir_LOS_20200518.csv", stringsAsFactors=FALSE)


## Basic data cleaning
ugr <- ugr.raw
los <- los.raw

colnames(ugr) <- tolower(colnames(ugr))
colnames(los) <- tolower(colnames(los))

# Population
table(ugr$weirlocation)
ugr$population <- NA
  ugr$population[ugr$weirlocation=="Catherine Creek"] <- "Catherine"
  ugr$population[ugr$weirlocation=="Upper Grande Ronde River"] <- "UGR"
  table(ugr$population)
table(los$siteid)
los$population <- "Lostine"

# Date format
ugr$trapdate <- ymd(ymd_hms(ugr$trapdate))
los$trapdate <- ymd(ymd_hms(los$trapdate))

# Sex and age designation (adult vs jack)
table(ugr$sex)
ugr$agedesignation <- NA
    ugr$agedesignation[ugr$sex=="J"] <- "Jack"
    ugr$agedesignation[ugr$sex=="M" | ugr$sex=="F"] <- "Adult"
    ugr$agedesignation[ugr$sex=="Unk" | ugr$sex==""] <- "Unk"
ugr$sex[ugr$sex=="J"] <- "M"
ugr$sex[ugr$sex==""] <- "Unk"
table(ugr$sex, ugr$agedesignation)

table(los$sex)
los$sex <- trimws(los$sex)
los$sex[los$sex=="ND"] <- "Unk"   #for consistency
table(los$agedesignation)
los$agedesignation[los$agedesignation=="Jack/Jill"] <- "Jack"
los$agedesignation[los$agedesignation==""] <- "Unk"
table(los$sex, los$agedesignation)

# Origin
table(ugr$origin)
ugr$origin[ugr$origin=="HAT"] <- "Hat"
ugr$origin[ugr$origin=="NAT"] <- "Nat"
ugr$origin[ugr$origin==""] <- "Unk"
table(ugr$origin)

table(los$origin)

# Length
los <- rename(los, forklength=fl)

# Age data...
ugr <- rename(ugr, 
              age.best = bestage,
              age.cwt = cwtage,
              age.pit = pitage,
              age.scale = scaleage_best,
              age.key = age_key,
              age.length = lengthage)
table(ugr$age.pit)
ugr$age.cwt[ugr$age.cwt<0] <- NA
ugr$age.pit[ugr$age.pit<0] <- NA  #replace missing data numeric codes with NA
ugr$age.scale[ugr$age.scale<0] <- NA

los <- rename(los, 
              age.best = bestage,
              age.cwt = cwtage,
              age.pit = pitage,
              age.scale = scaleage_lgd,
              age.key = agekey,
              age.length = lengthage)
table(los$age.pit)
los$age.cwt[los$age.cwt<0] <- NA
los$age.pit[los$age.pit<0] <- NA  #replace missing data numeric codes with NA
los$age.scale[los$age.scale<0] <- NA

# Recapture
table(ugr$recapture)
ugr$recapture[ugr$recapture==0] <- "FALSE"
ugr$recapture[ugr$recapture==1] <- "TRUE"
table(los$recapture)
los$recapture <- toupper(los$recapture)

# Opercle markings
table(ugr$oppunch)
table(los$oppunch)
  # Not going to attempt to clean these at this point.

## Pull out relevant fields and combine to one table
ugr.data <- select(ugr, 
                   population, trapyear, trapdate, count, 
                   sex, origin, forklength, agedesignation,  
                   age.best, age.cwt, age.pit, age.scale, age.length, 
                   recapture, oppunch)
ugr.data$notes <- ""

los.data <- select(los, 
                   population, trapyear, trapdate, count, 
                   sex, origin, forklength, agedesignation, 
                   age.best, age.cwt, age.pit, age.scale, age.length, 
                   recapture, oppunch, notes)

# Combine to one weir data set
weir.data <- rbind(ugr.data, los.data)

# Output file
write.csv(weir.data, "data_weir_cleaned.csv", row.names=FALSE)


## ---- CARCASS DATA ----
carc.raw <- read.csv("data_carcass_export_20200519.csv", stringsAsFactors=FALSE)

carc <- carc.raw

colnames(carc) <- tolower(colnames(carc))

# Population
table(carc$lcm_pop)
carc <- rename(carc, population=lcm_pop)

table(carc$river)

# Date format
carc$surveydate <- ymd(ymd_hms(carc$surveydate))

# Above/below data  
carc <- rename(carc, 
               pos.weir = aboveorbelowweir,
               pos.trap = aboveorbelowrst)

  # Weir first
  table(carc$pos.weir)
  carc$pos.weir[carc$pos.weir=="Above Weir"] <- "above"
  carc$pos.weir[carc$pos.weir=="Below Weir"] <- "below"
  carc$pos.weir[carc$pos.weir=="Diversion"] <- "above"   #Assume for now that all those LOS diversions are above weir/trap. Alternatively, diversion carcasses could be filtered out.
  carc$pos.weir[carc$pos.weir=="Lostine Weir"] <- "on.weir"
  carc$pos.weir[carc$pos.weir=="No Weir"] <- NA
  carc$pos.weir[carc$pos.weir=="Prior to weir construction"] <- "before.weir"
  
  # RST 
  table(carc$pos.trap)
  table(carc$population, carc$pos.trap)
    
    # UGR
  carc$pos.trap[carc$pos.trap=="AboveTrap"] <- "above"
  carc$pos.trap[carc$pos.trap=="BelowTrap"] <- "below"
  
    # Minam
  carc$pos.trap[carc$population=="Minam"] <- "above"  #Essentially, all of Minam is above trap
  
    # Los 
  carc$pos.trap[carc$population %in% c("Lostine", "Catherine")] <- 
    carc$pos.weir[carc$population %in% c("Lostine", "Catherine")]  # To start, assume that CC and LOS sites match weir
  carc$pos.trap[carc$pos.trap=="on.weir"] <- "below"  #Lostine weir is below trap, so all the on.weir fish are "below".
  
  # Pre-weir CC
  carc$pos.trap[carc$pos.trap=="before.weir"] <- "above"  #Looking at avg carcass numbers for survey sections, it looks like usually substantially more carcasses are found above the trap than below the trap in this joint section.
  
  # Missing data -> should be fixed with new export
  filter(carc, pos.trap=="")
  carc$pos.trap[carc$pos.trap==""] <- "above"
  
# Survey type...probably not necessary, but I'll keep it anyway 
  table(carc$survey.type)
  carc$survey.type <- tolower(carc$survey.type)
  carc$survey.type <- gsub(pattern="-", replacement="", x=carc$survey.type)  #Remove dashes for consistency
  carc$survey.type[carc$survey.type=="elh crew"] <- "elh.crew"
  
  table(carc$objective)  #Nice and clean
  
# Sex
  table(carc$sex)
  carc$sex[carc$sex=="J"] <- "M"
  carc$sex[carc$sex=="UNK"] <- "Unk"
  table(carc$sex)
  
# Origin  
  table(carc$origin)
  carc$origin[carc$origin %in% c("", "None", "HON")] <- "Unk"
  carc$origin[carc$origin=="SNP"] <- "Hat"  # Safety net pgm seems to be a particular type of hatchery operation
  
# Age
  carc <- rename(carc, 
              age.best = bestage,
              age.cwt = cwtage,
              #age.pit = pitage,  #no pit age here it seems
              age.scale = bestscaleage,
              age.key = agekey,
              age.length = lengthage)
  carc$age.scale[carc$age.scale==-99] <- NA
  
# Prespawn mort
  table(carc$sex, carc$prespawn)  
  carc$prespawn[carc$sex!="F"] <- NA  #Data not useful or usable for any fish not known to be female.
  carc$prespawn[carc$prespawn=="Unknown"] <- "Unk"

# Punch records?
  table(carc$oppunch)
  carc <- rename(carc, oppunch.present = oppunch)
  carc$oppunch.present[carc$oppunch.present=="yes"] <- "Yes"
  carc$oppunch.present[carc$oppunch.present==""] <- "Unk"
  
  table(carc$operclepunchtype)
  # Not going to attempt to clean this one
  carc <- rename(carc, oppunch.type = operclepunchtype)
  
# Size class and eligibility for mark-recap  #NAs indicate a carcass deemed ineligible for mark-recap population estimation.
  table(carc$markrecapsizecategory)
  # Size class field first
  carc <- mutate(carc, agedesignation = NA)
    adult.recs <- grep(pattern="Adult", x=carc$markrecapsizecategory)
    jack.recs <- grep(pattern="Jack", x=carc$markrecapsizecategory)
    mini.recs <- grep(pattern="MiniJ", x=carc$markrecapsizecategory)
    na.recs <- grep(pattern="NA", x=carc$markrecapsizecategory)  #This only gets the Adult/Jack NAs, not the pure NAs
  carc$agedesignation[adult.recs] <- "Adult"
  carc$agedesignation[jack.recs] <- "Jack"
  carc$agedesignation[mini.recs] <- "MiniJ"
  table(carc$markrecapsizecategory, carc$agedesignation)
  
  # Mark recap field
  carc <- mutate(carc, mark.recap.eligible=TRUE)
    carc$mark.recap.eligible[na.recs] <- FALSE
    carc$mark.recap.eligible[is.na(carc$markrecapsizecategory)] <- FALSE
    # Going to leave blanks alone for now.

# Select and order rows for export
  carcass.data <- select(carc,
                         population, river, year, surveydate,
                         pos.weir, pos.trap, siteid, sex, origin, forklength,
                         agedesignation, age.best, age.cwt, age.scale, age.key, age.length,
                         prespawn, oppunch.present, oppunch.type, objective, mark.recap.eligible)
# Output file
write.csv(carcass.data, "data_carcass_cleaned.csv", row.names=FALSE)

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

# Melt to full long form
juv.long <- melt(data = select(juv, population, brood.year, mig.year, starts_with("migrants")), 
                 id.vars = c("population", "brood.year", "mig.year", "comments_migrants"))

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
                    population + brood.year + mig.year + season ~ dat.type, 
                    value.var="value")
  
# Recalculate SE from reported +/- CI value
  juv.data <- mutate(juv.data,
                     abund.se = abund.ci/1.96)
  
## ---- OUTPUT ----
# Select and reorder fields
  juv.abund.data <- select(juv.data, 
                           popualation, brood.year, mig.year, season,
                           abund.est, abund.se)
  
# Write output
  write.csv(juv.abund.data, "data_JuvAbund_cleaned.csv", row.names=FALSE)
