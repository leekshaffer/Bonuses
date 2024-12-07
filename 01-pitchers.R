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
  C.WARs.br.fg=(fWAR.pshare+bWAR.pshare)/2,
  C.WARs.fg.bp=(fWAR.pshare+WARP.pshare)/2,
  C.WARs.br.bp.fg=(bWAR.pshare+WARP.pshare+fWAR.pshare)/3
)

reg.p.c1 <- lm(PShare~0+C.WAA, data=PStats)
reg.p.c2 <- lm(PShare~0+C.WAAf, data=PStats)
reg.p.c3 <- lm(PShare~0+C.bWAR, data=PStats)
reg.p.c4 <- lm(PShare~0+C.WARP, data=PStats)
reg.p.c5 <- lm(PShare~0+C.WARs.br.fg, data=PStats)
reg.p.c6 <- lm(PShare~0+C.WARs.fg.bp, data=PStats)
reg.p.c7 <- lm(PShare~0+C.WARs.br.bp.fg, data=PStats)

## Combo of BRef and FG WAR wins:
sd(PStats$PShare-PStats$C.WARs.br.fg)
mean(abs(PStats$PShare-PStats$C.WARs.br.fg))

sd(PStats$PShare-PStats$C.WARs.br.bp.fg)
mean(abs(PStats$PShare-PStats$C.WARs.br.bp.fg))


## Apply BREF & FG WAR shares metric to 2022 and 2023 data:
PStats_23 <- PStats %>% dplyr::select(Name,MLBAMID,BREFID,FGID,Season,
                                      BonusW,PShare,C.WARs.br.fg,C.WARs.br.bp.fg) %>%
  mutate(XBonus=sum(BonusW)*C.WARs.br.fg,
         Bonus_Diff=BonusW-XBonus)
ggplot(data=PStats_23, mapping=aes(x=XBonus, y=BonusW)) + geom_point() + 
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")

PStats_22 <- Data_22_Full %>% 
  dplyr::filter(B.p==1) %>%
  mutate(PShare=Share/sum(Share),
         fWAR.pshare=fWAR.p/sum(fWAR.p),
         bWAR.pshare=bWAR.p/sum(bWAR.p),
         C.WARs.br.fg=(fWAR.pshare+bWAR.pshare)/2) %>% 
  dplyr::select(Name,Exact,MLBAMID,BREFID,FGID,Season,
                BonusW,PShare,C.WARs.br.fg)
Comp <- PStats_22 %>% dplyr::filter(Exact) %>% 
  dplyr::summarize(BonusW=sum(BonusW),
                   C.WARs.br.fg=sum(C.WARs.br.fg))
PStats_22 <- PStats_22 %>%
  mutate(BonusW.rel=BonusW/Comp$BonusW,
         C.WARs.br.fg.rel=C.WARs.br.fg/Comp$C.WARs.br.fg,
         XBonus=C.WARs.br.fg.rel*Comp$BonusW,
         Bonus_Diff=BonusW-XBonus)
ggplot(data=PStats_22, 
       mapping=aes(x=XBonus, y=BonusW, shape=Exact, color=Exact)) + 
  geom_point() + 
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")
         
         