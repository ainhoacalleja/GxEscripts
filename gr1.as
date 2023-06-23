!ARGS 10 ht10 !RENAME 3 !WORKSPACE 2048 !NOGRAPHS # !WORKSPACE 100 !RENAME !ARGS // !DOPART $1
Title: gr1.
#Site,Mum_lat,Block,Mum_id,Family_id,ht10,ht20,nvt10,nvt20
#356,67.87,2,1920563,74559,100.645599,.,6.204678,4.921253
#356,67.87,2,1920563,74559,105.436302,.,5.896811,4.841968
#356,67.87,3,1920563,74559,82.058594,.,5.556522,4.728149
#356,67.87,5,1920563,74559,32.242699,.,5.586533,4.721939
 Site  !A 5      # 356
 Mum_lat !A       # 67.87
 Block  *       # 5 
 Mum_id  !I 2000     # 1920563
 Family_id  !I  2000    # 74559
 ht10  !M0      # 32.242699
 ht20  !M0      # .
 nvt10 !M0       # 5.586533
 nvt20 !M0      # 4.721939

# Check/Correct these field definitions.

#gr1P.csv !SKIP 1 !CSV !ALPHA
gr1.csv  !SKIP 1 !CSV !SUM  !MAXIT 100 !DOPART $A !MVINCLUDE

!PART 1
$B ~ mu Site Mum_lat ,
!r coruv(Site).nrm(Mum_id)
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 2
#!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r coruh(Site).id(Family_id)
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 3
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r fa1(Site).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id
predict Family_id !AVE Site !only fa1(Site).id(Family_id)

VPREDICT !DEFINE

!PART 4
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r corgh(Site).id(Family_id)
residual sat(Site).id(units)

VPREDICT !DEFINE

!PART 5
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r us(Site).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id 
predict Family_id !AVE Site !ONLY us(Site).id(Family_id) #average of BLUP + SE predictions

VPREDICT !DEFINE
R GenCor us(Site).id(Family_id)

!PART 6
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

!PART 7
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

!PART 8
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

!PART 9
!CONTINUE !TSV !STEP 0.1 !SLOW !DDF
$B ~ mu Site Mum_lat ,
!r fa2(Site).id(Family_id)
residual sat(Site).id(units)

predict Site Mum_lat #marginal prediction
predict Family_id
predict Site.Family_id !present Site Family_id
predict Family_id !AVE Site !only fa2(Site).id(Family_id)

VPREDICT !DEFINE

!PART 10
TABULATE ht10 ~ Site !COUNT
TABULATE ht10 ~ Mum_lat !COUNT
TABULATE ht10 ~ Family_id !COUNT
$B ~ mu Site Mum_lat !r Family_id



