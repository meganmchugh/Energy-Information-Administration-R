MCHUGH_MEGAN_HWK2_EIA <- function(EIA_year){
  #files that have matching formats
    # 2007, 2008
    # 2009, 2010, 2011
    # 2012
    # 2013
    # 2014, 2015, 2016
  
  #load libraries
  library('data.table')

  #read csv files for year
  f860 <- read.csv(paste0("./ME397_A2_data_csv/F860_", EIA_year, ".csv"),header = TRUE,stringsAsFactors = FALSE)
  f923 <- read.csv(paste0("./ME397_A2_data_csv/F923_", EIA_year, ".csv"),header = TRUE,stringsAsFactors = FALSE)
  
  #read 2016 csv
  f860_2016 <- read.csv(paste0("./ME397_A2_data_csv/F860_2016.csv"))
  
  #determine column names
  f860_2016_names <- names(f860_2016)
  
  #get unique combinations of prime mover, energy source, and Technology
  f860_2016_unique <- unique(f860_2016[,c('Prime.Mover','Energy.Source.1','Technology')])
  
  #conditionally remove and resassign column names
  if(EIA_year == "2007" | EIA_year == "2008"){
    f860 <- f860[c(1,4,5,2,23,7,12,18)]
    names(f860) <- c("Utility.Name","Utility.ID","Plant.Code","Plant.Name","Energy.Source.1","Prime.Mover","Nameplate.Capacity","Operating.Year")
    f923 <- f923[c(5,6,96,95,97,1,4)]
    names(f923) <- c("Utility.Name","Utility.ID","Net.Generation.MWh","Elec.Fuel.Consumption.MMBtu","Year","Plant.Code","Plant.Name")
    f860 <- merge(f860,f860_2016_unique,by=c("Prime.Mover","Energy.Source.1"))
  } else if(EIA_year == "2009" | EIA_year == "2010" |EIA_year == "2011" | EIA_year == "2012"){
    f860 <- f860[c(2,1,3,4,16,8,10,15)]
    names(f860) <- c("Utility.Name","Utility.ID","Plant.Code","Plant.Name","Energy.Source.1","Prime.Mover","Nameplate.Capacity","Operating.Year")
    f923 <- f923[c(5,6,96,95,97,1,4)]
    names(f923) <- c("Utility.Name","Utility.ID","Net.Generation.MWh","Elec.Fuel.Consumption.MMBtu","Year","Plant.Code","Plant.Name")
    f860 <- merge(f860,f860_2016_unique,by=c("Prime.Mover","Energy.Source.1"))
  } else if(EIA_year == "2013"){
    f860 <- f860[c(2,1,3,4,33,8,15,26)]
    names(f860) <- c("Utility.Name","Utility.ID","Plant.Code","Plant.Name","Energy.Source.1","Prime.Mover","Nameplate.Capacity","Operating.Year")
    f923 <- f923[c(5,6,96,95,97,1,4)]
    names(f923) <- c("Utility.Name","Utility.ID","Net.Generation.MWh","Elec.Fuel.Consumption.MMBtu","Year","Plant.Code","Plant.Name")
    f860 <- merge(f860,f860_2016_unique,by=c("Prime.Mover","Energy.Source.1"))
  } else if(EIA_year == "2014" | EIA_year == "2015" | EIA_year == "2016"){
    f860 <- f860[c(2,1,3,4,34,9,16,27,8)]
    names(f860) <- c("Utility.Name","Utility.ID","Plant.Code","Plant.Name","Energy.Source.1","Prime.Mover","Nameplate.Capacity","Operating.Year","Technology")
    f923 <- f923[c(5,6,96,95,97,1,4)]
    names(f923) <- c("Utility.Name","Utility.ID","Net.Generation.MWh","Elec.Fuel.Consumption.MMBtu","Year","Plant.Code","Plant.Name")
  }
  
  #format out commas and set NAs to zero for numeric columns
  f860[,'Nameplate.Capacity'] <- (gsub(",","",f860[,'Nameplate.Capacity']))
  f860$Nameplate.Capacity[is.na(f860$Nameplate.Capacity)] <- 0
  f923[,'Net.Generation.MWh'] <- (gsub(",","",f923[,'Net.Generation.MWh']))
  f923$Net.Generation.MWh[is.na(f923$Net.Generation.MWh)] <- 0
  f923[,'Elec.Fuel.Consumption.MMBtu'] <- (gsub(",","",f923[,'Elec.Fuel.Consumption.MMBtu']))
  f923$Elec.Fuel.Consumption.MMBtu[is.na(f923$Elec.Fuel.Consumption.MMBtu)] <- 0
  
  #pre-process data
    #transform appropriate columns to numeric
  f860 <- transform(f860,Nameplate.Capacity=as.numeric(Nameplate.Capacity))
  f923 <- transform(f923,Net.Generation.MWh=as.numeric(Net.Generation.MWh), Elec.Fuel.Consumption.MMBtu=as.numeric(Elec.Fuel.Consumption.MMBtu))
    #aggregate (does not effect sums)
  f860 <- aggregate(.~Utility.ID+Utility.Name+Plant.Code+Technology+Plant.Name+Energy.Source.1+Prime.Mover+Operating.Year, f860, sum, na.rm=TRUE, na.action = na.pass)
  f923 <- aggregate(.~Utility.Name+Utility.ID+Year+Plant.Code+Plant.Name, f923, sum, na.rm=TRUE, na.action = na.pass)
    #merge f860 and f923
  EIA_merged <- merge(f860,f923,by=c("Utility.Name","Utility.ID","Plant.Code","Plant.Name"),all = TRUE)

  #set NAs to zero for numbers and fill in missing years
  EIA_merged$Nameplate.Capacity[is.na(EIA_merged$Nameplate.Capacity)] <- 0
  EIA_merged$Net.Generation.MWh[is.na(EIA_merged$Net.Generation.MWh)] <- 0
  EIA_merged$Elec.Fuel.Consumption.MMBtu[is.na(EIA_merged$Elec.Fuel.Consumption.MMBtu)] <- 0
  EIA_merged$Year[is.na(EIA_merged$Year)] <- EIA_year

  #operating year columns
  f860_OY <- EIA_merged[c("Utility.ID","Plant.Code","Prime.Mover","Energy.Source.1","Operating.Year","Technology")]
  EIA_merged_2 <- as.data.table(f860_OY, keep.rownames = TRUE)[,.(Average.Operating.Year=mean(Operating.Year), Min.Operating.Year=min(Operating.Year), Max.Operating.Year=max(Operating.Year)),by = .(Utility.ID,Plant.Code,Prime.Mover,Energy.Source.1,Technology)]
  EIA_merged_3 <- merge(EIA_merged,EIA_merged_2,by=c("Utility.ID","Plant.Code","Prime.Mover","Energy.Source.1","Technology"), na.action=na.pass)
  EIA_merged_3 <- EIA_merged_3[,c(1,2,3,4,5,6,7,9,10,11,12,13,14,15)]
  EIA_merged_4 <- as.data.table(EIA_merged_3)[,.(Nameplate.Capacity=sum(Nameplate.Capacity)),by=.(Utility.ID,Utility.Name,Plant.Code,Technology,Plant.Name,Energy.Source.1,Prime.Mover)]
  EIA_merged_5 <- merge(EIA_merged_4,EIA_merged_3,all.x = TRUE)

  #set NAs to zero for numbers and fill in missing years
  EIA_merged_5$Nameplate.Capacity[is.na(EIA_merged_5$Nameplate.Capacity)] <- 0
  EIA_merged_5$Net.Generation.MWh[is.na(EIA_merged_5$Net.Generation.MWh)] <- 0
  EIA_merged_5$Elec.Fuel.Consumption.MMBtu[is.na(EIA_merged_5$Elec.Fuel.Consumption.MMBtu)] <- 0
  EIA_merged_5$Year[is.na(EIA_merged_5$Year)] <- EIA_year
  
  #capacity factor calculations
  EIA_merged_5$Capacity.Factor <- as.numeric(EIA_merged_5$Net.Generation.MWh)/(as.numeric(EIA_merged_5$Nameplate.Capacity)*8760)
  EIA_merged_5$Capacity.Factor[which(!is.finite(EIA_merged_5$Capacity.Factor))] <- 0
  EIA_merged$Capacity.Factor <- as.numeric(EIA_merged$Net.Generation.MWh)/(as.numeric(EIA_merged$Nameplate.Capacity)*8760)
  EIA_merged$Capacity.Factor[which(!is.finite(EIA_merged$Capacity.Factor))] <- 0
  
  #heat rate calculation
  EIA_merged_5$Heat.Rate <- as.numeric(EIA_merged_5$Elec.Fuel.Consumption.MMBtu)*1000/as.numeric(EIA_merged_5$Net.Generation.MWh)
  EIA_merged_5$Heat.Rate[which(!is.finite(EIA_merged_5$Heat.Rate))] <- 0

  #conditionally subset capacity factors for values less than 1 (except nuclear) and greater than 0 (except hydro and batteries)
  EIA_unmerged_technology <- EIA_merged_5[!(EIA_merged_5$Technology == "Batteries" | EIA_merged_5$Technology == "Hydroelectric Pumped Storage" | EIA_merged_5$Technology == "Nuclear"),]
  EIA_unmerged_technology <- EIA_unmerged_technology[EIA_unmerged_technology$Capacity.Factor <= 1 & EIA_unmerged_technology$Capacity.Factor >= 0,]
  EIA_unmerged_hydro_batteries <- EIA_merged_5[(EIA_merged_5$Technology == "Batteries" | EIA_merged_5$Technology == "Hydroelectric Pumped Storage"),]
  EIA_unmerged_hydro_batteries <- EIA_unmerged_hydro_batteries[EIA_unmerged_hydro_batteries$Capacity.Factor <= 1,]
  EIA_unmerged_nuclear <- EIA_merged_5[EIA_merged_5$Technology == "Nuclear",]
  EIA_unmerged_nuclear <- EIA_unmerged_nuclear[EIA_unmerged_nuclear$Capacity.Factor >= 0,]
  EIA_remerge <- rbind(EIA_unmerged_technology,EIA_unmerged_nuclear,EIA_unmerged_hydro_batteries)
  EIA_final <- EIA_remerge[,c(1,2,3,4,5,6,7,8,12,13,14,10,11,15,16,9)]

  #calculate percent namplate capacity
  percent_nameplate_capacity <- sum(as.numeric(EIA_final$Nameplate.Capacity), na.rm = T)/sum(as.numeric(f860$Nameplate.Capacity), na.rm = T)
  percent_nameplate_capacity <- signif(percent_nameplate_capacity*100, digits = 4)
  print(paste0("Nameplate.Capacity in final dataset: ",percent_nameplate_capacity,"%"))
  
  #calculate percent net generation
  EIA_f923 <- EIA_merged[!duplicated(EIA_merged[c("Utility.Name","Utility.ID","Plant.Code","Plant.Name","Energy.Source.1","Prime.Mover","Technology")]),]
  percent_net_generation_MWh <- sum(as.numeric(EIA_final$Net.Generation.MWh), na.rm = T)/sum(as.numeric(EIA_f923$Net.Generation.MWh[EIA_f923$Capacity.Factor <= 1 & EIA_f923$Capacity.Factor >= 0]), na.rm = T)
  percent_net_generation_MWh <- signif(percent_net_generation_MWh*100, digits = 4)
  print(paste0("Net.Generation.MWh in final dataset: ",percent_net_generation_MWh,"%"))
  
  #write csv
  write.csv(EIA_final, file = paste0("MCHUGH_MEGAN_EIA_",EIA_year,"_DATA.csv"), row.names = FALSE)
}
