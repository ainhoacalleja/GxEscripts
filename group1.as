!ARGS 12 vit_10 !RENAME 3 !WORKSPACE 2048 !NOGRAPHS #!RENAME !ARGS // !DOPART $1  356 357 422 423 429
Title: group1.
#Site,Stem_id,ruta,rad,planta,Prow,Ppos,Pstem,Ploc,Genotype_id,Family_id,Mum_id,Dad_id,Block,Mum_lat,dia_20,hjd_10,hjd_20,vit_10,vit_20,nVit_10,nVit_20
#356,7181117,1,5,9,5,9,0,1,6413423,74576,1920561,4178,1,67.38,.,.,.,0,0,4.1663002806,4.6084120511
#356,7181533,6,7,5,47,5,0,1,6413839,74576,1920561,4178,2,67.38,.,.,.,0,0,4.126601855,4.8605427284
#356,7181574,7,3,6,51,6,0,1,6413880,74576,1920561,4178,2,67.38,.,.,.,1,0,5.160135265,4.8605427284
#356,7181966,12,2,8,90,8,0,1,6414272,74576,1920561,4178,3,67.38,.,.,.,0,0,3.9356924523,4.7450877517
 Site  !A 5   # 356
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
TABULATE  vit_10 ~ Site !count
TABULATE vit_10 ~ Family_id !count
$B  ~ mu Site Mum_lat,         # Specify fixed model
      !r Family_id          # Specify random model

!PART 2
!DDF
#Individual site analysis
!FILTER Site !SELECT $C
$B ~ mu Mum_lat !r Block ruta.planta ruta.rad ruta Family_id

VPREDICT !DEFINE
F GenVar Family_id * 4
F PhenVar GenVar + Residual
H Herit GenVar PhenVar

!PART 3
!DDF !FCON
#Traditional cross-classsified model
$B ~ mu Site Mum_lat  ,
!r Family_id ,
Site.Block ,
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
Site.Block ,
Site.ruta.planta ,
Site.ruta.rad ,
Site.ruta

VPREDICT !DEFINE
F GenVar 7*4
F PhenVar 7+5
H Herit GenVar PhenVar

!PART 5
!CONTINUE !TSV !DDF !FCON
#Coruv G structur + heterogenous error
$B ~ mu Site Mum_lat,
!r coruv(Site).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,4,5).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 6 #CORUH model: heterogeneus variances, constrain correlation to 1
!DDF 1 !FCON
!ASSIGN SiteCRH !< !INIT
1
0.09 0.09 0.02 0.04 0.04 !GFU !>
$B ~ mu Site Mum_lat,
!r coruh(Site $SiteCRH).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,4,5).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 7
!CONTINUE !TSV !DDF 1
#Heretogeneous G and R structure, constant genetic correlation between sites
$B ~ mu Site Mum_lat ,
!r coruh(Site).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,4,5).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 8
!CONTINUE !TSV !SLOW !DDF 1
$B ~ mu Site Mum_lat ,
!r fa1(Site).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,4,5).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id
predict Family_id !AVE Site !ONLY fa1(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE

!PART 9 #Heterogeneous G and R, heterogeneous genetic correlation + design factors model
!CONTINUE !TSV !SLOW !DDF 1
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r fa1(Site).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,4,5).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).idv(Ppos).id(Prow)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id
predict Family_id !AVE Site !ONLY fa1(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE

!PART 10  #Without nugget effect
!CONTINUE !TSV !SLOW !DDF 1
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r fa1(Site).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,4,5).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta
residual sat(Site).ar1v(Ppos).ar1(Prow)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id
predict Family_id !AVE Site !ONLY fa1(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE

!PART 11  #FA1 model + AR1 spatial structure + With nugget effect
!CONTINUE !TSV !SLOW !DDF 1
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r fa1(Site).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,4,5).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow) 

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id
predict Family_id !AVE Site !ONLY fa1(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE


!PART 12  #FA2 model + AR1 spatial structure + With nugget effect  = singularities
!CONTINUE !TSV !STEPSIZE 0.1 !AISINGULARITIES !DDF
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r fa2(Site).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,4,5).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow) 

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id
predict Family_id !AVE Site !ONLY fa2(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE

!PART 13    #CORGH  model +AR1 spatial + nugget
!CONTINUE !TSV !SLOW !DDF 1
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r corgh(Site).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,3,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow)

predict Site #marginal prediction
predict Family_id !AVE Site !ONLY corgh(Site).id(Family_id) #average of BLUP + SE predictions
predict Family_id !PLOT Family_id !AVE Block ruta.planta ruta.rad ruta Site
predict Site.Family_id !present Site Family_id !AVE Block ruta.planta ruta.rad ruta

VPREDICT !DEFINE


!PART 14   #US model +AR1 spatial + nugget
!CONTINUE !TSV !SLOW !STEPSIZE 0.1 !DDF 1
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r us(Site).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,3,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow)

predict Site #marginal prediction
predict Family_id !AVE Site !ONLY us(Site).id(Family_id) #average of BLUP + SE predictions
predict Family_id !PLOT Family_id !AVE Block ruta.planta ruta.rad ruta Site
predict Site.Family_id !present Site Family_id !AVE Block ruta.planta ruta.rad ruta

VPREDICT !DEFINE


!PART 15   #xfa1 model +AR1 spatial + nugget
!CONTINUE !TSV !SLOW !STEPSIZE 0.1 !DDF 1
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r xfa1(Site).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,3,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow)

predict Site #marginal prediction
predict Family_id !AVE Site !ONLY xfa1(Site).id(Family_id) #average of BLUP + SE predictions
predict Family_id !PLOT Family_id !AVE Block ruta.planta ruta.rad ruta Site
predict Site.Family_id !present Site Family_id !AVE Block ruta.planta ruta.rad ruta

VPREDICT !DEFINE
 V Sitevar  xfa1(Site)
 R GenCorr  Sitevar

!PART 16  #US model +AR1 spatial + nugget
!CONTINUE !TSV !SLOW !STEPSIZE 0.1 !DDF 1
!SECTION Site !ROWFACTOR Ppos !COLFACTOR Prow !BRIEF -1
$B ~ mu Site Mum_lat !f mv ,
!r xfa2(Site).id(Family_id) ,
at(Site).Block ,
at(Site,1,2,3,4).ruta.planta ,
at(Site).ruta.rad ,
at(Site).ruta ,
at(Site).idv(units)
residual sat(Site).ar1v(Ppos).ar1(Prow)

predict Site #marginal prediction
predict Family_id !AVE Site !ONLY xfa2(Site).id(Family_id) #average of BLUP + SE predictions
predict Family_id !PLOT Family_id !AVE Block ruta.planta ruta.rad ruta Site
predict Site.Family_id !present Site Family_id !AVE Block ruta.planta ruta.rad ruta

VPREDICT !DEFINE
 V Sitevar  xfa2(Site)
 R GenCorr  Sitevar






















