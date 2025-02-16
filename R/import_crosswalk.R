require(dplyr)
require(stringr)
require(baseballr)

## Import ID-matching info:
Crosswalk <- chadwick_player_lu()

Cross_use <- Crosswalk %>%
  mutate(full=str_squish(paste(name_first,name_last,name_suffix,sep=" ")))

## Some manual fixes:
Cross_use[Cross_use$key_bbref=="skenepa01","key_fangraphs"] <- 33677
Cross_use[Cross_use$key_bbref=="schwesp01","key_fangraphs"] <- 31846
Cross_use[Cross_use$key_bbref=="myersto01","key_fangraphs"] <- 22191

save(Cross_use,
     file="int/Crosswalk.Rda")
