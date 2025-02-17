require(dplyr)

## Load service time categories:
load(file="int/FDB.Rda")
## Load Stats
load(file="int/FullStats.Rda")

yrs <- 22:24

## Load Bonuses:
for (yr in yrs) {
  load(file=paste0("int/Bonus_",yr,".Rda"))
  assign(x=paste0("Check_",yr),
         value=get(paste0("Bonus_",yr,"_Full")) %>% 
           rename(year=Season) %>%
           left_join(FDB %>% 
                       dplyr::select(key_bbref,year,status,yos,value,Svc_Cat),
                     by=join_by(key_bbref, year)))
}

## Status Check for Those with Bonuses:
for (yr in yrs) {
  print(paste0("Season: ",yr))
  print(paste0("Number of NAs: ",dim(get(paste0("Check_",yr)) %>% filter(is.na(status)))[1]))
  print(paste0("All in PreArb Svc_Cat? ",all(unique(get(paste0("Check_",yr)) %>% pull(Svc_Cat))=="PreArb")))
}

## Add Stats to FDB data for non-Arb players:
Comps <- FDB %>% dplyr::filter(!(Svc_Cat %in% c("Arb+","SuperTwo"))) %>%
  left_join(FullStats)

PAComp <- Comps %>% dplyr::filter(Svc_Cat == "PreArb")

for (yr in yrs) {
  fullyr <- as.numeric(paste0("20",yr))
  PAC_yr <- PAComp %>% dplyr::filter(year==fullyr)
  PA100_any <- PAC_yr %>% slice_max(bWAR.t, n=100, with_ties=TRUE) %>%
    full_join(PAC_yr %>% slice_max(fWAR.t, n=100, with_ties=TRUE)) %>%
    full_join(PAC_yr %>% slice_max(WARP.t, n=100, with_ties=TRUE))
  UnkC_yr <- Comps %>% dplyr::filter(year==fullyr)
  Unk100_any <- UnkC_yr %>% slice_max(bWAR.t, n=100, with_ties=TRUE) %>%
    full_join(UnkC_yr %>% slice_max(fWAR.t, n=100, with_ties=TRUE)) %>%
    full_join(UnkC_yr %>% slice_max(WARP.t, n=100, with_ties=TRUE))
  
  assign(x=paste0("PA100_any_",yr),
         value=PA100_any)
  assign(x=paste0("Unk100_any_",yr),
         value=Unk100_any)
  save(list=paste0(c("PA100_any_","Unk100_any_"),yr),
       file=paste0("int/Comp100s_",yr,".Rda"))
}
