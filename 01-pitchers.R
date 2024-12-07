require(tidyverse)
load(file="int/Data_23.Rda")
load(file="int/Data_22.Rda")

## Pitching Calcs:
Pitchers <- Data_23_Pos %>% dplyr::filter(B.p==1) %>%
  dplyr::select(Name,name_first,name_last,MLBAMID,BREFID,FGID,Season,B.p,
                BonusW,IP,RA9,RAA.p,WAA.p,WAAadj.p,RAR.p,bWAR.p,
                ERA,xERA,FIP,xFIP,fWAR.p,`DRA-`,DRA,cFIP,WHIP,RA9.bp,WARP.p) %>%
  mutate(PShare=BonusW/sum(BonusW),
         FIPshare=FIP/sum(FIP),
         RA9share=RA9/sum(RA9),
         RARpshare=RAR.p/sum(RAR.p),
         RAAshare=RAA.p/sum(RAA.p),
         WAAshare=WAA.p/sum(WAA.p),
         WAAfull=WAA.p+WAAadj.p,
         WAAfshare=WAAfull/sum(WAAfull),
         bWAR.pshare=bWAR.p/sum(bWAR.p),
         WARP.pshare=WARP.p/sum(WARP.p),
         fWAR.pshare=fWAR.p/sum(fWAR.p))

reg.p1 <- lm(PShare~0+FIPshare+RARpshare, data=Pitchers)
reg.p2 <- lm(PShare~0+FIPshare+RAAshare, data=Pitchers)
reg.p3 <- lm(PShare~0+FIPshare+WAAshare, data=Pitchers)
reg.p4 <- lm(PShare~0+FIPshare+WAAfshare, data=Pitchers)
reg.p5 <- lm(PShare~0+FIPshare+bWAR.pshare, data=Pitchers)

PStats <- Pitchers %>% mutate(
  C.WAA=(FIPshare+WAAshare)/2,
  C.WAAf=(FIPshare+WAAfshare)/2,
  C.bWAR=(FIPshare+bWAR.pshare)/2,
  C.WARP=(FIPshare+WARP.pshare)/2,
  C.WARsbr=(fWAR.pshare+bWAR.pshare)/2,
  C.WARsbp=(fWAR.pshare+WARP.pshare)/2
)

reg.p.c1 <- lm(PShare~0+C.WAA, data=PStats)
reg.p.c2 <- lm(PShare~0+C.WAAf, data=PStats)
reg.p.c3 <- lm(PShare~0+C.bWAR, data=PStats)
reg.p.c4 <- lm(PShare~0+C.WARP, data=PStats)
reg.p.c5 <- lm(PShare~0+C.WARsbr, data=PStats)
reg.p.c6 <- lm(PShare~0+C.WARsbp, data=PStats)

## Combo of BRef and FG WAR wins:
sd(PStats$PShare-PStats$C.WARsbr)
mean(abs(PStats$PShare-PStats$C.WARsbr))


## Apply BREF & FG WAR shares metric to 2022 and 2023 data:
PStats_23 <- PStats %>% dplyr::select(Name,MLBAMID,BREFID,FGID,Season,
                                      BonusW,PShare,C.WARsbr) %>%
  mutate(XBonus=sum(BonusW)*C.WARsbr,
         Bonus_Diff=BonusW-XBonus)
ggplot(data=PStats_23, mapping=aes(x=XBonus, y=BonusW)) + geom_point() + 
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")

PStats_22 <- Data_22_Full %>% 
  dplyr::filter(B.p==1) %>%
  mutate(PShare=Share/sum(Share),
         fWAR.pshare=fWAR.p/sum(fWAR.p),
         bWAR.pshare=bWAR.p/sum(bWAR.p),
         C.WARsbr=(fWAR.pshare+bWAR.pshare)/2) %>% 
  dplyr::select(Name,Exact,MLBAMID,BREFID,FGID,Season,
                BonusW,PShare,C.WARsbr)
Comp <- PStats_22 %>% dplyr::filter(Exact) %>% 
  dplyr::summarize(BonusW=sum(BonusW),
                   C.WARsbr=sum(C.WARsbr))
PStats_22 <- PStats_22 %>%
  mutate(BonusW.rel=BonusW/Comp$BonusW,
         C.WARsbr.rel=C.WARsbr/Comp$C.WARsbr,
         XBonus=C.WARsbr.rel*Comp$BonusW,
         Bonus_Diff=BonusW-XBonus)
ggplot(data=PStats_22, 
       mapping=aes(x=XBonus, y=BonusW, shape=Exact, color=Exact)) + 
  geom_point() + 
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")
         
         