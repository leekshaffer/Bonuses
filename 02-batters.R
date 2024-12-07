require(tidyverse)
load(file="int/Data_23.Rda")
load(file="int/Data_22.Rda")

## Batter Calcs:
Batters <- Data_23_Pos %>% dplyr::filter(B.b==1) %>%
  dplyr::select(Name,name_first,name_last,MLBAMID,BREFID,FGID,Season,B.b,
                BonusW,PA,OBP,RAA.b,WAA.b,RAR.b,bWAR.b,
                Off,Def,fWAR.b,`DRC+`,WARP.b) %>%
  mutate(BShare=BonusW/sum(BonusW),
         bWAR.bshare=bWAR.b/sum(bWAR.b),
         WARP.bshare=WARP.b/sum(WARP.b),
         fWAR.bshare=fWAR.b/sum(fWAR.b))

reg.p1 <- lm(BShare~0+bWAR.bshare+fWAR.bshare, data=Batters)
reg.p2 <- lm(BShare~0+WARP.bshare+fWAR.bshare, data=Batters)
reg.p3 <- lm(BShare~0+bWAR.bshare+WARP.bshare, data=Batters)
reg.p4 <- lm(BShare~0+bWAR.bshare+WARP.bshare+fWAR.bshare, data=Batters)

BStats <- Batters %>% mutate(
  C.WARs.br.fg=(bWAR.bshare+fWAR.bshare)/2,
  C.WARs.br.bp=(bWAR.bshare+WARP.bshare)/2,
  C.WARs.fg.bp=(fWAR.bshare+WARP.bshare)/2,
  C.WARs.br.bp.fg=(bWAR.bshare+WARP.bshare+fWAR.bshare)/3
)

reg.p.c1 <- lm(BShare~0+C.WARs.br.fg, data=BStats)
reg.p.c2 <- lm(BShare~0+C.WARs.br.bp, data=BStats)
reg.p.c3 <- lm(BShare~0+C.WARs.fg.bp, data=BStats)
reg.p.c4 <- lm(BShare~0+C.WARs.br.bp.fg, data=BStats)

## Combo of BRef and FG WAR or of all 3 wins:
sd(BStats$BShare-BStats$C.WARs.br.fg)
mean(abs(BStats$BShare-BStats$C.WARs.br.fg))

sd(BStats$BShare-BStats$C.WARs.br.bp.fg)
mean(abs(BStats$BShare-BStats$C.WARs.br.bp.fg))


## Apply BREF & FG WAR shares metric to 2022 and 2023 data:
BStats_23 <- BStats %>% dplyr::select(Name,MLBAMID,BREFID,FGID,Season,
                                      BonusW,BShare,
                                      C.WARs.br.fg,C.WARs.br.bp.fg) %>%
  mutate(XBonus.all=sum(BonusW)*C.WARs.br.bp.fg,
         XBonus=sum(BonusW)*C.WARs.br.fg,
         Bonus_Diff=BonusW-XBonus)
ggplot(data=BStats_23, mapping=aes(x=XBonus, y=BonusW)) + geom_point() + 
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")
ggplot(data=BStats_23, mapping=aes(x=XBonus.all, y=BonusW)) + geom_point() + 
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")

BStats_22 <- Data_22_Full %>% 
  dplyr::filter(B.b==1) %>%
  mutate(BShare=Share/sum(Share),
         fWAR.bshare=fWAR.b/sum(fWAR.b),
         bWAR.bshare=bWAR.b/sum(bWAR.b),
         WARP.bshare=WARP.b/sum(WARP.b),
         C.WARs.br.fg=(fWAR.bshare+bWAR.bshare)/2,
         C.WARs.br.bp.fg=(fWAR.bshare+bWAR.bshare+WARP.bshare)/3) %>% 
  dplyr::select(Name,Exact,MLBAMID,BREFID,FGID,Season,
                BonusW,BShare,C.WARs.br.fg,C.WARs.br.bp.fg)
Comp <- BStats_22 %>% dplyr::filter(Exact) %>% 
  dplyr::summarize(BonusW=sum(BonusW),
                   C.WARs.br.fg=sum(C.WARs.br.fg),
                   C.WARs.br.bp.fg=sum(C.WARs.br.bp.fg))
BStats_22 <- BStats_22 %>%
  mutate(BonusW.rel=BonusW/Comp$BonusW,
         C.WARs.br.fg.rel=C.WARs.br.fg/Comp$C.WARs.br.fg,
         C.WARs.br.bp.fg.rel=C.WARs.br.bp.fg/Comp$C.WARs.br.bp.fg,
         XBonus=C.WARs.br.fg.rel*Comp$BonusW,
         XBonus.all=C.WARs.br.bp.fg.rel*Comp$BonusW,
         Bonus_Diff=BonusW-XBonus,
         Bonus_Diff.all=BonusW-XBonus.all)
ggplot(data=BStats_22, 
       mapping=aes(x=XBonus, y=BonusW, shape=Exact, color=Exact)) + 
  geom_point() + 
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")
ggplot(data=BStats_22, 
       mapping=aes(x=XBonus.all, y=BonusW, shape=Exact, color=Exact)) + 
  geom_point() + 
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")
  
## RMSE for BR+FG only beats all 3:
sqrt(mean((BStats_22 %>% filter(Exact) %>% pull(Bonus_Diff))^2))
sqrt(mean((BStats_22 %>% filter(Exact) %>% pull(Bonus_Diff.all))^2))
         