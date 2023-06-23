!ARGS 15 nVit_20 !RENAME 3 !WORKSPACE 2048 !NOGRAPHS #!RENAME !ARGS // !DOPART $1  356 357 422 423 429
Title: group1.
#Site,Stem_id,ruta,rad,planta,Prow,Ppos,Pstem,Ploc,Genotype_id,Family_id,Mum_id,Dad_id,Block,Mum_lat,dia_20,hjd_10,hjd_20,vit_10,vit_20,nVit_10,nVit_20
#356,7181117,1,5,9,5,9,0,1,6413423,74576,1920561,4178,1,67.38,.,.,.,0,0,4.1663002806,4.6084120511
#356,7181533,6,7,5,47,5,0,1,6413839,74576,1920561,4178,2,67.38,.,.,.,0,0,4.126601855,4.8605427284
#356,7181574,7,3,6,51,6,0,1,6413880,74576,1920561,4178,2,67.38,.,.,.,1,0,5.160135265,4.8605427284
#356,7181966,12,2,8,90,8,0,1,6414272,74576,1920561,4178,3,67.38,.,.,.,0,0,3.9356924523,4.7450877517
 Site  !A 4   # 356
 Stem_id  !I 41326      # 7181966
 ruta  *      # 12
 rad  *       # 2 
 planta  *       # 8 
 Prow  *      # 90
 Ppos  *       # 8
 Pstem  *       # 0 
 Ploc  *       # 1 
 Genotype_id  !I 41326     # 6414272
 Family_id  !I      # 74576 
 Mum_id  !I      # 1920561
 Dad_id  !I      # 4178 
 Block  *       # 3
 Mum_lat        # 67.38
 dia_20        # . 
 hjd_10        # . 
 hjd_20        # . 
 vit_10         # 0
 vit_20         # 0
 nVit_10        # 3.9356924523 
 nVit_20        # 4.7450877517
# Check/Correct these field definitions.
#group1P.csv !SKIP 1 !CSV !MAKE !ALPHA
group1.txt  !SKIP 1 !CONTINUE 3 !MAXIT 100 !DOPART $A !MVINCLUDE  #!CONTINUE 3

!PART 1
#Obtain data summary for sites
TABULATE  nVit_20 ~ Site !count
TABULATE nVit_20 ~ Family_id !count
$B  ~ mu Site Mum_lat,         # Specify fixed model
      !r Family_id          # Specify random model

!PART 2
!DDF
#Individual site analysis. Result. block is not significant at any site.
!FILTER Site !SELECT $C
$B ~ mu Mum_lat !r Block ruta.planta ruta.rad ruta Family_id

VPREDICT !DEFINE
F GenVar Family_id * 4
F PhenVar GenVar + Residual
H Herit GenVar PhenVar


!PART 3
!DDF !FCON
#Traditional cross-classsified model.
$B ~ mu Site Mum_lat  ,
!r Family_id ,
Site.Family_id ,
Site.ruta.planta ,
Site.ruta.rad ,
Site.ruta

VPREDICT !DEFINE
F GenVar Family_id * 4
F PhenVar Family_id + Site.Family_id + Residual
H Herit GenVar PhenVar

!PART 4
!CONTINUE !TSV !DDF !FCON
#Compound symmetry model (homogeneous R and G structures)
$B ~ mu Site Mum_lat ,
!r coruv(Site).id(Family_id) ,
Site.ruta.planta ,
Site.ruta.rad ,
Site.ruta 

VPREDICT !DEFINE
F GenVar 7*4
F PhenVar 7+5
H Herit GenVar PhenVar

!PART 5
!CONTINUE !TSV !DDF
#Coruv G structur + heterogenous error. Without block because it was not significant at any site
$B ~ mu Site Mum_lat,
!r coruv(Site).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 6 #CORUH model: heterogeneus variances, constrain correlation to 1
!DDF 1
!ASSIGN SiteCRH !< !INIT
1
0.01 0.02 0.01 0.02 !GFU !>
$B ~ mu Site Mum_lat,
!r coruh(Site $SiteCRH).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 7 #CORUH model: heterogeneus variances, homogeneus correlations
!CONTINUE !TSV !DDF 1
$B ~ mu Site Mum_lat,
!r coruh(Site).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 8 #FA1 model
!CONTINUE !TSV !SLOW !DDF 1
$B ~ mu Site Mum_lat  ,
!r fa1(Site).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 9
!CONTINUE !TSV !SLOW !DDF
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv,
!r fa1(Site).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).idv(Ppos).id(Prow)

VPREDICT !DEFINE

!PART 10 #Without nugget effect = no units term
!CONTINUE !TSV !SLOW  !DDF
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r fa1(Site).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).ar1v(Ppos).ar1(Prow)

VPREDICT !DEFINE

!PART 11
!CONTINUE !TSV !SLOW !DDF#With nugget effect
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r fa1(Site).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow)  

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY fa1(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE


#!PART 12 #Singularities
#!CONTINUE !TSV !SLOW !STEP 0.01 #With nugget effect
#!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
#$B ~ mu Site Mum_lat !f mv ,
#!r fa2(Site).id(Family_id) ,
#at(Site,2,4).ruta.planta ,
#at(Site).ruta.rad ,
#at(Site).ruta ,
#at(Site).idv(units)
#residual sat(Site).ar1v(Ppos).ar1(Prow)

#predict Site Mum_lat #marginal prediction
#predict Family_id
#predict Site.Family_id !present Site Family_id
#predict Family_id !AVE Site !ONLY fa2(Site).id(Family_id) #average of BLUP + SE predictions

#VPREDICT !DEFINE

!PART 12
!CONTINUE !TSV !SLOW !DDF #!STEP 0.01 #With nugget effect
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r corgh(Site !GU).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY corgh(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE
F err 12+15+18+21+33+34+35+36
F merr err*0.25
F famV 29+30+31+32
F mfamV famV*0.25
S sqrt 29:32
X v1v2 41*42
X v1v3 41*43
X v1v4 41*44
X v2v3 42*43
X v2v4 42*44
X v3v4 43*44
X cov12 23*45
X cov13 24*46
X cov14 26*47
X cov23 25*48
X cov24 27*49
X cov34 28*50
F fcov 51+52+53+54+55+56
F mfcov fcov*0.1666
F AdV mfcov*4   #additive variance
F PhenV mfamV+merr  #phenotypic variance
F PhenVf mfamV*0.25+mfcov*0.75+merr*0.003 #phenotypic variance in mean family scale
H h2i AdV PhenV
H h2f mfcov PhenVf
#single site heritabilities
F ad1 29*4
F ad2 30*4
F ad3 31*4
F ad4 32*4
F ph1 12+29+33
F ph2 15+30+43
F ph3 18+31+35
F ph4 21+32+36
H h1 ad1 ph1
H h2 ad2 ph2
H h3 ad3 ph3
H h4 ad4 ph4

!PART 13
!CONTINUE !TSV !SLOW !STEP 0.01 #With nugget effect
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r us(Site !GU).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY us(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE
F err 12+15+18+21+33+34+35+36
F merr err*0.25
F famV 23+25+28+32
F mfamV famV*0.25
F fcov 24+26+27+29+30+31
F mfcov fcov*0.1667
F AdV mfcov*4   #additive variance
F PhenV mfamV+merr  #phenotypic variance
F PhenVf mfamV*0.25+mfcov*0.75+merr*0.003 #phenotypic variance in mean family scale
H h2i AdV PhenV
H h2f mfcov PhenVf
#single site heritabilities
F ad1 23*4
F ad2 25*4
F ad3 28*4
F ad4 32*4
F ph1 12+23+33
F ph2 15+25+34
F ph3 18+28+35
F ph4 21+32+36
H h1 ad1 ph1
H h2 ad2 ph3
H h3 ad3 ph3
H h4 ad4 ph4
R Gencor us(Site).id(Family_id)


!PART 14
!CONTINUE !TSV !SLOW !STEP 0.01 #With nugget effect
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r xfa1(Site).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY xfa1(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE
V Sitevar  xfa1(Site)
F err 12+15+18+21+31+32+33+34
F merr err*0.25
F famV 35+37+40+44
F mfamV famV*0.25
F fcov 36+38+39+41+42+43
F mfcov fcov*0.1667
F AdV mfcov*4   #additive variance
F PhenV mfamV+merr  #phenotypic variance
F PhenVf mfamV*0.25+mfcov*0.75+merr*0.003 #phenotypic variance in mean family scale
H h2i AdV PhenV
H h2f mfcov PhenVf
#single site heritabilities
F ad1 35*4
F ad2 37*4
F ad3 40*4
F ad4 44*4
F ph1 12+31+35
F ph2 15+32+37
F ph3 18+33+40
F ph4 21+34+44
H h1 ad1 ph1
H h2 ad2 ph2
H h3 ad3 ph3 
H h4 ad4 ph4
R GenCorr  Sitevar

!PART 15
!CONTINUE !TSV !SLOW !STEP 0.01 !DDF #With nugget effect
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r xfa2(Site).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY xfa2(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE
V Sitevar  xfa2(Site)
F err 12+15+18+21+35+36+37+38
F merr err*0.25
F famV 39+41+44+48
F mfamV famV*0.25
F fcov 40+42+43+45+46+47
F mfcov fcov*0.1667
F AdV mfcov*4   #additive variance
F PhenV mfamV+merr  #phenotypic variance
F PhenVf mfamV*0.25+mfcov*0.75+merr*0.003 #phenotypic variance in mean family scale
H h2i AdV PhenV
H h2f mfcov PhenVf
#single site heritabilities
F ad1 39*4
F ad2 41*4
F ad3 44*4
F ad4 48*4
F ph1 12+35+39
F ph2 15+36+41
F ph3 18+37+44
F ph4 21+38+48
H h1 ad1 ph1
H h2 ad2 ph2 
H h3 ad3 ph3
H h4 ad4 ph4
R GenCorr  Sitevar

!PART 16
!CONTINUE !TSV !SLOW !STEP 0.01 #With nugget effect
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r xfa3(Site).id(Family_id) ,
at(Site,2,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY xfa3(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE
 V Sitevar  xfa3(Site)
 R GenCorr  Sitevar

















