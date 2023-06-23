!ARGS 15 ht10 !RENAME 3 !WORKSPACE 2048 !NOGRAPHS # !WORKSPACE 100 !RENAME !ARGS // !DOPART $1
Title: allsites.
#Site,Mum_lat,Block,Mum_id,Family_id,ht10,ht20,vt10,vt20,nvt10,nvt20
#356,67.87,2,1920563,74559,100.645599,0,2.746339,0.225397,6.204678,4.921253
#356,67.87,2,1920563,74559,105.436302,0,2.301379,0.11796,5.896811,4.841968
#356,67.87,3,1920563,74559,82.058594,0,1.836243,-0.100994,5.556522,4.728149
#356,67.87,5,1920563,74559,32.242699,0,1.942454,-0.070655,5.586533,4.721939
 Site  !A 18     # 356
 Mum_lat !A     # 67.87
 Block  *       # 5 
 Mum_id  !I      # 1920563 
 Family_id  !I 2000     # 74559
 ht10        # 32.242699
 ht20        # 0
 vt10        # 1.942454
 vt20        # -0.070655
 nvt10        # 5.586533
 nvt20        # 4.721939
# Check/Correct these field definitions.
allsites.csv !CSV  !SKIP 1 !SUM  !MAXIT 100 !DOPART $A !MVEXCLUDE

!PART 1
#Obtain data summary for sites
TABULATE  ht10 ~ Site !count
TABULATE ht10 ~ Family_id !count
$B  ~ mu Site Mum_lat,         # Specify fixed model
      !r Family_id          # Specify random model
      

!PART 2
!DDF
!FILTER Site !SELECT $C
$B ~ mu Mum_lat !r Block Family_id

VPREDICT !DEFINE
F GenVar Family_id * 4
F PhenVar GenVar + Residual
H Herit GenVar PhenVar

!PART 3  #Traditional cross-classsified model.
!DDF !FCON
$B ~ mu Site Mum_lat  ,
!r Family_id ,
Site.Family_id

VPREDICT !DEFINE


!PART 4
!DDF !FCON
#Compound symmetry model (homogeneous R and G structures)
$B ~ mu Site Mum_lat ,
!r coruv(Site).id(Family_id)


VPREDICT !DEFINE
F FamV 2*0.693729   #0.693729  is the correlation obtained in the model
F GenVar 4*4
F PhenVar 5+1
H Herit GenVar PhenVar


!PART 5
!CONTINUE !TSV !DDF !FCON
#Coruv G structur + heterogenous error. Without block because it was not significant at any site
$B ~ mu Site Mum_lat,
!r coruv(Site).id(Family_id)
residual sat(Site).id(units)

VPREDICT !DEFINE


!PART 6 #CORUH model: heterogeneus variances, homogeneus correlations
!DDF 1 !FCON
$B ~ mu Site Mum_lat ,
!r coruh(Site).id(Family_id)
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 7 #FA1 model
!CONTINUE !TSV !SLOW !DDF 1
$B ~ mu Site Mum_lat  ,
!r fa1(Site).id(Family_id)
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 8 #FA2 model
!CONTINUE !TSV !STEPSIZE 0.01 !SLOW !AISINGULARITIES
$B ~ mu Site Mum_lat  ,
!r fa2(Site).id(Family_id)
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 9
!ASSIGN SiteCGH !< !INIT
0.1
0.1 0.1
0.1 0.1 0.1
0.1 0.1 0.1 0.1
0 0 0 0 0 
0 0 0 0 0 0.1
0 0 0 0 0 0.1 0.1
0 0 0 0 0 0.1 0.1 0.1
0 0 0 0 0 0.1 0.1 0.1 0.1
0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0.1
0 0 0 0 0 0 0 0 0 0 0.1 0.1
0 0 0 0 0 0 0 0 0 0 0.1 0.1 0.1
0 0 0 0 0 0 0 0 0 0 0 0 0 0 
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.1
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.1 0.1
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.1 0.1 0.1
17 35 67 54 33 42 60 167 96 178 211 156 229 27 177 111 98 48 !G10U10FU5F2U5F3U5F4U20FU10F2U10F3U28FU14F2U14F21U !>
!STEPSIZE 0.01 !SLOW !AISINGULARITIES
$B ~ mu Site Mum_lat  ,
!r corgh(Site $SiteCGH).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY corgh(Site).id(Family_id) #average of BL

VPREDICT !DEFINE

!PART 10
!ASSIGN RUS !< !INIT
17
20 35
27 38 67
27 37 42 54
22 30 35 34 33
0 0 0 0 0 42
0 0 0 0 0 20 60
0 0 0 0 0 54 48 167
0 0 0 0 0 37 33 101 96
0 0 0 0 0 55 49 153 104 178
0 0 0 0 0 0 0 0 0 0 211 
0 0 0 0 0 0 0 0 0 0 152 156
0 0 0 0 0 0 0 0 0 0 195 190 229
0 0 0 0 0 0 0 0 0 0 62 60 77 27
0 0 0 0 0 0 0 0 0 0 0 0 0 0 177
0 0 0 0 0 0 0 0 0 0 0 0 0 0 102 111
0 0 0 0 0 0 0 0 0 0 0 0 0 0 94 99 98
0 0 0 0 0 0 0 0 0 0 0 0 0 0 59 62 57 48 !G15U5FU5F2U5F3U5F4U5F5U10FU10F2U10F3U10F4U14FU14F2U14F3U14F4U !>
!STEPSIZE 0.01 !SLOW !AISINGULARITIES
$B ~ mu Site Mum_lat  ,
!r us(Site $RUS).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY us(Site).id(Family_id) #average of BL

VPREDICT !DEFINE
R Gencor us(Site).id(Family_id)

!PART 11
!CONTINUE !TSV !STEP 0.01 !SLOW !DDF 1
$B ~ mu Site Mum_lat  ,
!r xfa1(Site !GU).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY xfa1(Site).id(Family_id) #average of BL

VPREDICT !DEFINE
V Sitevar  xfa1(Site)
R GenCorr  Sitevar

!PART 12
!CONTINUE !TSV !STEP 0.01 !SLOW !DDF 1
$B ~ mu Site Mum_lat  ,
!r xfa2(Site !GU).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id
predict Family_id !AVE Site !ONLY xfa2(Site).id(Family_id) #average of BL

VPREDICT !DEFINE
V Sitevar  xfa2(Site)
R GenCorr  Sitevar

!PART 13
!CONTINUE !TSV !STEP 0.01 !SLOW !DDF 1
$B ~ mu Site Mum_lat  ,
!r xfa3(Site !GU).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY xfa3(Site).id(Family_id) #average of BL

VPREDICT !DEFINE
V Sitevar  xfa3(Site)
R GenCorr  Sitevar

!PART 14 #CORUH model: heterogeneus variances, homogeneus correlations
!ASSIGN SiteCoruh !< !INIT
1
17 35 67 54 33 42 60 167 96 178 211 156 229 27 177 111 98 48 !GFU !>
!DDF 1 !FCON
$B ~ mu Site Mum_lat ,
!r coruh(Site $SiteCoruh).id(Family_id)
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 15  #Without fixed initial values
!ASSIGN RUS !< !INIT
17
20 35
27 38 67
27 37 42 54
22 30 35 34 33
0 0 0 0 0 42
0 0 0 0 0 20 60
0 0 0 0 0 54 48 167
0 0 0 0 0 37 33 101 96
0 0 0 0 0 55 49 153 104 178
0 0 0 0 0 0 0 0 0 0 211 
0 0 0 0 0 0 0 0 0 0 152 156
0 0 0 0 0 0 0 0 0 0 195 190 229
0 0 0 0 0 0 0 0 0 0 62 60 77 27
0 0 0 0 0 0 0 0 0 0 0 0 0 0 177
0 0 0 0 0 0 0 0 0 0 0 0 0 0 102 111
0 0 0 0 0 0 0 0 0 0 0 0 0 0 94 99 98
0 0 0 0 0 0 0 0 0 0 0 0 0 0 59 62 57 48 !GU !>
!STEPSIZE 0.01 !SLOW !AISINGULARITIES
$B ~ mu Site Mum_lat  ,
!r us(Site $RUS).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY us(Site).id(Family_id) #average of BL

VPREDICT !DEFINE
R Gencor us(Site).id(Family_id)




