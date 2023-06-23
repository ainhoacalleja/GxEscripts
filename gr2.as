!ARGS 12 nvt20 !RENAME 3 !WORKSPACE 2048 !NOGRAPHS # !WORKSPACE 100 !RENAME !ARGS // !DOPART $1
Title: gr2.
#Site,Mum_lat,Block,Mum_id,Family_id,ht10,ht20,nvt10,nvt20
#1,66.5,8,4037536,70632,176.567612,510.752991,6.110792,6.19713
#1,66.5,14,4037536,70632,166.168915,389.424011,6.195799,6.328921
#1,66.5,16,4037536,70632,127.232368,355.469391,6.079647,6.023726
#1,66.5,5,4037536,70632,.,.,4.636908,4.674195
 Site  !A 3       # 1
 Mum_lat  !A      # 66.5
 Block  *       # 5
 Mum_id  !I  2000     # 4037536
 Family_id  !I 2000     # 70632
 ht10 !M0       # .
 ht20 !M0       # .
 nvt10 !M0       # 4.636908
 nvt20 !M0      # 4.674195
# Check/Correct these field definitions.
gr2.csv  !SKIP 1 !CSV !SUM  !MAXIT 100 !DOPART $A !MVINCLUDE

!PART 1
TABULATE nvt20 ~ Site !COUNT
TABULATE nvt20 ~ Mum_lat !COUNT
TABULATE nvt20 ~ Family_id !COUNT
$B ~ mu Site Mum_lat !r Family_id

!PART 2
!DDF
!FILTER Site !SELECT $C
$B ~ mu Mum_lat !r Family_id

VPREDICT !DEFINE

!PART 3
#!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r coruv(Site).id(Family_id)
residual sat(Site).id(units)


!PART 4
#!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r coruh(Site).id(Family_id)
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 5
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r fa1(Site).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id
predict Family_id !AVE Site !only fa1(Site).id(Family_id)

VPREDICT !DEFINE

!PART 6
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r fa2(Site).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id
predict Family_id !AVE Site !only fa2(Site).id(Family_id)

VPREDICT !DEFINE

!PART 7
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r corgh(Site !GU).id(Family_id)
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 8
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r us(Site !GU).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY us(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE
R GenCor us(Site).id(Family_id)

!PART 9
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r xfa1(Site).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY xfa1(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE
V GenVar xfa1(Site)
R GenCor GenVar

!PART 10
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r xfa2(Site).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY xfa2(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE
V GenVar xfa2(Site)
R GenCor GenVar

!PART 11
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r xfa3(Site).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY xfa3(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE
V GenVar xfa3(Site)
R GenCor GenVar

!PART 12
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r fa2(Site).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id
predict Family_id !AVE Site !only fa2(Site).id(Family_id)

VPREDICT !DEFINE



