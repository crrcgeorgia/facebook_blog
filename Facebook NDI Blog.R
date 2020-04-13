#Facebook blog replication code
library(foreign)
library(haven)
library(Matching)
library(rgenoud)
library(survey)
library(psych)
library(haven)
library(descr)
library(ggeffects)
library(nnet)
getwd()
setwd("")

NDI19<-read_dta("NDI_2019_Nov_16.12.19_Public.dta")

names(NDI19)
table(as_factor(NDI19$FRQINPN))

NDI19$FRQINPN_r<-NDI19$FRQINPN
NDI19$FRQINPN_r[NDI19$FRQINPN_r==-9]<-NA
NDI19$FRQINPN_r[NDI19$FRQINPN_r==-7]<-0
NDI19$FRQINPN_r[NDI19$FRQINPN_r==-1]<-NA
NDI19$FRQINPN_r[NDI19$FRQINPN_r==-5]<--1
NDI19$FRQINPN_r[NDI19$FRQINPN_r>=1]<-1
NDI19$FRQINPN_r[NDI19$FRQINPN_r==-2]<-NA
table(NDI19$FRQINPN_r)
freq(NDI19$FRQINPN_r, w = NDI19$WTIND)
table(as_factor(NDI19$SETTYPE))

table(as_factor(NDI19$SUBSTRATUM))
table(NDI19$SUBSTRATUM)

NDI19$SUBSTRATUM_r<-NDI19$SUBSTRATUM
NDI19$SUBSTRATUM_r[NDI19$SUBSTRATUM_r==10]<-1
NDI19$SUBSTRATUM_r[NDI19$SUBSTRATUM_r==51]<-2
NDI19$SUBSTRATUM_r[NDI19$SUBSTRATUM_r==52]<-3
NDI19$SUBSTRATUM_r[NDI19$SUBSTRATUM_r>=41]<-3
NDI19$SUBSTRATUM_r[NDI19$SUBSTRATUM_r>=11]<-2
table(NDI19$SUBSTRATUM_r)

NDI19$SUBSTRATUM_r<-as_factor(NDI19$SUBSTRATUM_r)

table(NDI19$RESPSEX)
NDI19$RESPSEX<-as_factor(NDI19$RESPSEX)

table(NDI19$RESPAGE)

table(as_factor(NDI19$RESPEDU))
NDI19$RESPEDU_r<-NDI19$RESPEDU
NDI19$RESPEDU_r[NDI19$RESPEDU_r<=-1]<-NA
NDI19$RESPEDU_r[NDI19$RESPEDU_r<=3]<-1
NDI19$RESPEDU_r[NDI19$RESPEDU_r==4]<-2
NDI19$RESPEDU_r[NDI19$RESPEDU_r>=5]<-3
NDI19$RESPEDU_r<-as_factor(NDI19$RESPEDU_r)

NDI19$OWNAIRC_r<-NDI19$OWNAIRC
NDI19$OWNAIRC_r[NDI19$OWNAIRC_r<=-1]<-NA
NDI19$OWNFRDG_r<-NDI19$OWNFRDG
NDI19$OWNFRDG_r[NDI19$OWNFRDG_r<=-1]<-NA
NDI19$OWNCOTV_r<-NDI19$OWNCOTV
NDI19$OWNCOTV_r[NDI19$OWNCOTV_r<=-1]<-NA
NDI19$OWNSPHN_r<-NDI19$OWNSPHN
NDI19$OWNSPHN_r[NDI19$OWNSPHN_r<=-1]<-NA
NDI19$OWNTBLT_r<-NDI19$OWNTBLT
NDI19$OWNTBLT_r[NDI19$OWNTBLT_r<=-1]<-NA
NDI19$OWNCARS_r<-NDI19$OWNCARS
NDI19$OWNCARS_r[NDI19$OWNCARS_r<=-1]<-NA
NDI19$OWNWASH_r<-NDI19$OWNWASH
NDI19$OWNWASH_r[NDI19$OWNWASH_r<=-1]<-NA
NDI19$OWNCOMP_r<-NDI19$OWNCOMP
NDI19$OWNCOMP_r[NDI19$OWNCOMP_r<=-1]<-NA
NDI19$OWNHWT_r<-NDI19$OWNHWT
NDI19$OWNHWT_r[NDI19$OWNHWT_r<=-1]<-NA
NDI19$OWNCHTG_r<-NDI19$OWNCHTG
NDI19$OWNCHTG_r[NDI19$OWNCHTG_r<=-1]<-NA

NDI19$wealth<-(NDI19$OWNAIRC_r+
                 NDI19$OWNFRDG_r+
                 NDI19$OWNCOTV_r+
                 NDI19$OWNSPHN_r+
                 NDI19$OWNTBLT_r+
                 NDI19$OWNCARS_r+
                 NDI19$OWNWASH_r+
                 NDI19$OWNCOMP_r+
                 NDI19$OWNHWT_r+
                 NDI19$OWNCHTG_r)

table(as_factor(NDI19$POLDIRN))

NDI19$POLDIRN_r<-NDI19$POLDIRN
NDI19$POLDIRN_r[NDI19$POLDIRN_r<=-1]<-NA
NDI19$POLDIRN_r[NDI19$POLDIRN_r<=3]<-0
NDI19$POLDIRN_r[NDI19$POLDIRN_r>=4]<-1
table(as_factor(NDI19$DEMNOW))
NDI19$DEMNOW_r<-NDI19$DEMNOW
NDI19$DEMNOW_r[NDI19$DEMNOW_r<=-1]<-NA

freq(as_factor(NDI19$PARTYLIKE))
freq(as_factor(NDI19$PARTYSUPP1))
freq(as_factor(NDI19$PARTYSUPS))
freq(NDI19$PARTYSUPS)
table(NDI19$AB_GROUP, as_factor(NDI19$PARTYSUPP1))

NDI19$PARTYSUPP1_r<-(NDI19$PARTYSUPP1*10)
table(as_factor(NDI19$PARTYSUPP1))
table(as_factor(NDI19$PARTYSUPS))

NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==-40]<--4
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==-30]<-NA
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==-20]<--2
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==-10]<--1
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==10]<-4
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==20]<-9
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==30]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==40]<-6
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==50]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==60]<-2
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==70]<-3
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==80]<-1
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==90]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==100]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==110]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==120]<-5
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==130]<-8
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==140]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==150]<-7
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==160]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==170]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==180]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==190]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==200]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==210]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==220]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==230]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==240]<-10
NDI19$PARTYSUPP1_r[NDI19$PARTYSUPP1_r==250]<--5


NDI19$party_supp_combined<-ifelse(NDI19$PARTYSUPP1==-4, NDI19$PARTYSUPS, NDI19$PARTYSUPP1_r)
table(as_factor(NDI19$party_supp_combined))
freq(as_factor(NDI19$party_supp_combined_r), w = NDI19$WTIND)

NDI19$party_supp_combined_r<-NDI19$party_supp_combined
NDI19$party_supp_combined_r[NDI19$party_supp_combined_r>=5]<-5
NDI19$party_supp_combined_r<-as_factor(NDI19$party_supp_combined_r)
designNDI19 <- svydesign(id=~PSU,weights=~WTIND, strat=~SUBSTRATUM, data=NDI19)
table(as_factor(NDI19$wealth))
NDI19$wealth<-as.numeric(NDI19$wealth)
NDI19$wealth<-droplevels(NDI19$wealth)
NDI19$SUBSTRATUM_r<-droplevels(NDI19$SUBSTRATUM_r)
NDI19$FRQINPN_r<-as.factor(NDI19$FRQINPN_r)
NDI19$FRQINPN_r<-droplevels(NDI19$FRQINPN_r)
table(as_factor(NDI19$SUBSTRATUM_r))
table(as_factor(NDI19$FRQINPN_r))
NDI19$RESPEDU_r<-as.factor(NDI19$RESPEDU_r)
NDI19$RESPEDU_r<-droplevels(NDI19$RESPEDU_r)
NDI19$RESPSEX<-as.factor(NDI19$RESPSEX)
NDI19$RESPSEX<-droplevels(NDI19$RESPSEX)

NDI19$FRQINPN_r_internet<-NDI19$FRQINPN_r
NDI19$FRQINPN_r_internet[NDI19$FRQINPN_r_internet==0]<-NA

NDI19$FRQINPN_r_no_internet<-NDI19$FRQINPN_r
NDI19$FRQINPN_r_no_internet[NDI19$FRQINPN_r_no_internet==1]<-NA

NDI19$FRQINPN_r_no_internet2<-NDI19$FRQINPN_r
NDI19$FRQINPN_r_no_internet2[NDI19$FRQINPN_r_no_internet2==-1]<-NA
dim(NDI19)
NDI19$weights_small<-(2180*(NDI19$WTIND/sum(NDI19$WTIND)))

model1<-multinom(FRQINPN_r~wealth+ 
                   RESPEDU_r + 
                   RESPSEX + 
                   RESPAGE + 
                   SUBSTRATUM_r, weights = weights_small, data = NDI19)
summary(model1)
z <- summary(model1)$coefficients/summary(model1)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


plot(ggemmeans(model1, "wealth"))
plot(ggemmeans(model1, "RESPEDU_r"))
plot(ggemmeans(model1, "RESPSEX"))
plot(ggemmeans(model1, "RESPAGE[20:80]"))
plot(ggemmeans(model1, "SUBSTRATUM_r"))

designNDI19 <- svydesign(id=~PSU,weights=~WTIND, strat=~SUBSTRATUM, data=NDI19)
table(NDI19$FRQINPN_r_internet)


svyby(~RESPAGE, ~FRQINPN_r_internet, svymean, design = designNDI19, na.rm=TRUE)


svyby(~RESPAGE,~FRQINPN_r_no_internet,   designNDI19,svymean, na.rm = TRUE)

svyby(~RESPAGE,~FRQINPN_r_no_internet2,   designNDI19,svymean, na.rm = TRUE)


#Matched analysis

designNDI19_r<-designNDI19[complete.cases(designNDI19$variables$FRQINPN_r_internet),]
designNDI19_r<-designNDI19_r[complete.cases(designNDI19_r$variables$RESPEDU_r),]
designNDI19_r<-designNDI19_r[complete.cases(designNDI19_r$variables$RESPSEX),]
designNDI19_r<-designNDI19_r[complete.cases(designNDI19_r$variables$RESPAGE),]
designNDI19_r<-designNDI19_r[complete.cases(designNDI19_r$variables$SUBSTRATUM_r),]
designNDI19_r<-designNDI19_r[complete.cases(designNDI19_r$variables$wealth),]
dim(designNDI19_r)
table(designNDI19_r$variables$FRQINPN_r_internet)
designNDI19_r$variables$FRQINPN_r_internet[designNDI19_r$variables$FRQINPN_r_internet==1]<-0
designNDI19_r$variables$FRQINPN_r_internet[designNDI19_r$variables$FRQINPN_r_internet==-1]<-1
designNDI19_r$variables$FRQINPN_r_internet<-droplevels(designNDI19_r$variables$FRQINPN_r_internet)
table(as_factor(designNDI19_r$variables$FRQINPN_r_internet))
designNDI19_r$variables$FRQINPN_r_internet<-as.numeric(designNDI19_r$variables$FRQINPN_r_internet)
table(designNDI19_r$variables$FRQINPN_r_internet)
designNDI19_r$variables$FRQINPN_r_internet<-(designNDI19_r$variables$FRQINPN_r_internet-1)
modelpsm<-svyglm(FRQINPN_r_internet~wealth + 
                 RESPEDU_r + 
                 RESPSEX + 
                 RESPAGE + 
                 SUBSTRATUM_r, design = designNDI19, family = "binomial")
summary(modelpsm)

designNDI19_r$variables$prop.scores<-modelpsm$fitted.values


X=cbind(designNDI19_r$variables$prop.scores,
      designNDI19_r$variables$wealth, 
        designNDI19_r$variables$RESPEDU_r, 
        designNDI19_r$variables$RESPSEX,
        designNDI19_r$variables$RESPAGE, 
        designNDI19_r$variables$SUBSTRATUM_r)

BalanceMat=cbind(designNDI19_r$variables$prop.scores,
                 designNDI19_r$variables$wealth, 
                 designNDI19_r$variables$RESPEDU_r, 
                 designNDI19_r$variables$RESPSEX,
                 designNDI19_r$variables$RESPAGE, 
                 designNDI19_r$variables$SUBSTRATUM_r)

genout<-GenMatch(Tr=designNDI19_r$variables$FRQINPN_r_internet,X=X,
                 BalanceMatrix=BalanceMat,estimand="ATT",
                 pop.size=1000,replace=FALSE,ties=TRUE,unif.seed=round(runif(1,1,2147483647L)),
                 int.seed=round(runif(1,1,2147483647L)))

mout<-Match(Tr=designNDI19_r$variables$FRQINPN_r_internet,X=X,
            Weight.matrix=genout)

summary(mout)

checkbalance<-MatchBalance(designNDI19_r$variables$FRQINPN_r_internet~
                             designNDI19_r$variables$prop.scores+
                           designNDI19_r$variables$wealth+
                           designNDI19_r$variables$RESPEDU_r+
                           designNDI19_r$variables$RESPSEX+
                           designNDI19_r$variables$RESPAGE+ 
                           designNDI19_r$variables$SUBSTRATUM_r
                           ,match.out=mout,nboots=5000)

table(mout$weights)
length(mout$index.control)


#GETTING DK AND RA
designNDI19_r_DK<-designNDI19_r
names(designNDI19_r_DK$variables)
designNDI19_r_DK$variables<-designNDI19_r_DK$variables[,9:200]
designNDI19_r_DK$variables<-designNDI19_r_DK$variables[,1:186]

designNDI19_r_DK$variables[designNDI19_r_DK$variables!=-1]<-0
designNDI19_r_DK$variables[designNDI19_r_DK$variables==-1]<-1

designNDI19_r_RA<-designNDI19_r
names(designNDI19_r_RA$variables)
designNDI19_r_RA$variables<-designNDI19_r_RA$variables[,9:200]
designNDI19_r_RA$variables<-designNDI19_r_RA$variables[,1:186]

designNDI19_r_RA$variables[designNDI19_r_RA$variables!=-2]<-0
designNDI19_r_RA$variables[designNDI19_r_RA$variables==-2]<-1

y<-rowSums(designNDI19_r_RA$variables)
x<-rowSums(designNDI19_r_DK$variables)

table(y)

designNDI19_r$variables$dkcount<-x
designNDI19_r$variables$racount<-y

treatedfacebook<-designNDI19_r$variables[mout$index.treated,]
treatedfacebook$mweights<-mout$weights
controlfacebook<-designNDI19_r$variables[mout$index.control,]
controlfacebook$mweights<-mout$weights
facebook_matches<-merge(treatedfacebook,controlfacebook,all=TRUE)
table(facebook_matches$DEMNOW)
facebook_matches$DEMNOW_r<-facebook_matches$DEMNOW
facebook_matches$DEMNOW_r[facebook_matches$DEMNOW_r<=-1]<-NA

dkmodel<-glm(dkcount~FRQINPN_r_internet, w= mweights,data = facebook_matches, family = "poisson" )
summary(dkmodel)

ggemmeans(dkmodel,"FRQINPN_r_internet")

ramodel<-glm(dkcount~FRQINPN_r_internet, w= mweights,data = facebook_matches, family = "poisson" )
summary(ramodel)

ggemmeans(ramodel,"FRQINPN_r_internet")

demmodel<-glm(DEMNOW_r~FRQINPN_r_internet, w= mweights,data = facebook_matches, family = "binomial" )
summary(demmodel)

ggemmeans(demmodel, terms = "FRQINPN_r_internet", data = facebook_matches)

facebook_matches$POLDIRN_r<-facebook_matches$POLDIRN
facebook_matches$POLDIRN_r[facebook_matches$POLDIRN_r<=-1]<-NA

dirmodel<-glm(POLDIRN_r~FRQINPN_r_internet, w= mweights,data = facebook_matches)
summary(dirmodel)

crosstab( facebook_matches$party_supp_combined_r,facebook_matches$FRQINPN_r_internet, w = facebook_matches$mweights, chisq = TRUE)


table(as_factor(facebook_matches$RATEGOV4))
facebook_matches$RATEGOV4_r<-facebook_matches$RATEGOV4
facebook_matches$RATEGOV4_r[facebook_matches$RATEGOV4_r<=-1]<-NA
govmodel<-glm(RATEGOV4_r~FRQINPN_r_internet, w= mweights,data = facebook_matches)
summary(govmodel)

table(facebook_matches$VOTPARL)
facebook_matches$VOTPARL_r<-facebook_matches$VOTPARL
facebook_matches$VOTPARL_r[facebook_matches$VOTPARL_r<=-1]<-NA
votemodel<-glm(VOTPARL_r~FRQINPN_r_internet, w= mweights,data = facebook_matches)
summary(votemodel)

table(facebook_matches$ELECTDIC)
facebook_matches$ELECTDIC_r<-facebook_matches$ELECTDIC
facebook_matches$ELECTDIC_r[facebook_matches$ELECTDIC_r<=-1]<-NA
decidemodel<-glm(ELECTDIC_r~FRQINPN_r_internet, w= mweights,data = facebook_matches, family = "binomial")
summary(decidemodel)



table(NDI19$SHOPGVST)
designNDI19$variables$SHOPGVST_r<-designNDI19$variables$SHOPGVST
designNDI19$variables$SHOPGVST_r[designNDI19$variables$SHOPGVST_r<=-1]<-NA
model5<-svyglm(SHOPGVST_r~FRQINPN_r+wealth + 
                 RESPEDU_r + 
                 RESPSEX + 
                 RESPAGE + 
                 SUBSTRATUM_r, design = designNDI19)
summary(model5)

