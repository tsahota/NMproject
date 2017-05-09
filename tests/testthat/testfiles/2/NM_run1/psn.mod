;; 1. Based on: 1
;; 2. Description: Test model used in test.R
;; x1. Author: klgk669
$SIZES      NO=1213 LIM1=34004 LIM6=1214
$PROBLEM    6094 PK
;; 4. Date: 01.01.2011

;; 5. Version: 1

;; 6. Label:

;; Basic model

;; 7. Structural model:

;; One compartment model

;; 8. Covariate model:

;; No covariates

;; 9. Inter-individual variability:

;; CL and V1.

;; 10. Inter-occasion variability:

;; No IOV

;; 11. Residual variability:

;; Additive + Proportional

;; 12. Estimation:

;; IMP
$INPUT      ROW=DROP STUDY SUBJID=DROP USUBJID=DROP SITE VISIT=DROP
            CYCLE=DROP DAY PERIOD=DROP FED DAT0=DROP CTIM=DROP ICTIM
            UDTIM=DROP PTAD=DROP TAD=DROP TAFD=DROP AMT IAMT DOSE
            RAT0=DROP REG ROUTE=DROP TYPE DV LNDV=DROP EVID MDV LOQ
            BLOQ=DROP AGE RACE SEX HT=DROP WT IWT=DROP BMI=DROP
            BSA=DROP BWT=DROP BBMI=DROP BBSA=DROP ALB=DROP ALT AST
            BILI CRCL HEPD BALB=DROP BAST=DROP BALT=DROP BBILI=DROP
            BCRCL=DROP BHEPD=DROP ASTLL=DROP ASTUL=DROP ALTLL=DROP
            ALTUL=DROP BILLL=DROP BILUL=DROP TUMOR METS PAT COMB
            COMMENT=DROP ID FREQTXT=DROP DOSETXT=DROP REGTXT=DROP
            TYPETXT=DROP LOWER=DROP UPPER=DROP WDOSE=DROP ORD TAFD2
            AMTLOCF DTAFD=DROP DNUM DOCC1 DOCC2 TIME EXPPRED=DROP
$DATA      data2.csv IGNORE=@ IGNORE=(TYPE.GT.1)
$SUBROUTINE ADVAN2
$PK    

TVKA=EXP(THETA(1))
MU_1=LOG(TVKA)
KA = EXP(MU_1+ETA(1))

TVK=EXP(THETA(2))
MU_2=LOG(TVK)
K = EXP(MU_2+ETA(2))

TVV=EXP(THETA(3))
MU_3=LOG(TVV)
V = EXP(MU_3+ETA(3))

TVALAG1 = EXP(THETA(4))
MU_4=LOG(TVALAG1)
ALAG1 = EXP(MU_4 + ETA(4))

CL = K*V

S2 = V/1000

;; DOSE = mg.
;; Vol = L
;; untransformed CONC unit = mg/L
;; want Ng/mL = ug/L
;; so S2 = V/1000

$ERROR     

IPRED=F
W=SQRT(THETA(5)**2+THETA(6)**2*IPRED*IPRED)  ; proportional + additive error
IRES=DV-IPRED
IWRES=IRES/W
Y=IPRED+W*EPS(1)

;; TMAX around 2.5 hrs
;; t1/2 = ln(2)/Ka
;; 2.5/4 = ln(2)/Ka
;; ka = 1.1
;; logka = 0.1

;; t1/2 (elim) ~ 4 hrs
;; Ke = 0.17 h-1
;; log(Ke) = -1.75

;; V guess 50L
;; log(V) = 4

;-1.28E-01 -1.53E+00  5.15E+00  1.26E+01  4.77E-01

$THETA  -0.1 ; KA; h-1 ;LOG
 -1.53 ; K; h-1 ;LOG
 5.15 ; V; L ;LOG
 -1.52078 ; ALAG1; h ;LOG
 12.6 ; add error
 0.477 ; prop error
$OMEGA  0.7  ;     IIV_KA  ;        LOG
 0.1  ;      IIV_K  ;        LOG
 0.1  ;      IIV_V  ;        LOG
 0.318196  ;  IIV_ALAG1  ;        LOG
$SIGMA  1  FIX
; Parameter estimation - FOCE
;$ESTIMATION METHOD=1 INTER NOABORT MAXEVAL=9999 PRINT=1 NSIG=3 SIGL=9
; Parameter estimation - IMP
$ESTIMATION METHOD=IMP ISAMPLE=300 NITER=100 RANMETHOD=3S CTYPE=3
            CITER=10 CALPHA=0.05 CINTERVAL=3 PRINT=1 NOABORT
            INTERACTION GRD=DDDDSS
; Parameer estimation - SAEM

;$EST METHOD=SAEM ISAMPLE=2 NBURN=1000 NITER=500

;CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=10

;PRINT=1 NOABORT INTERACTION GRD=DDDDSS

; Objective function and covariance evaluation

;$EST METHOD=IMP INTER EONLY= 1 MAPITER=0 ISAMPLE = 2000 NITER = 10 RANMETHOD=3S NOABORT PRINT=1 NSIG=3 SIGL=9 GRD=DDDDSS
$COVARIANCE PRINT=E UNCONDITIONAL SIGL=10
;$SIM (324234) ONLYSIM SUBPR=1
$TABLE      ID TIME IPRED IWRES IRES CWRES NPDE ORD FILE=sdtab2
            NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE      ID CL KA V ; individual parameters
            FILE=patab2 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE      ID AGE WT CRCL DOCC2 ; continuous covariates
            FILE=cotab2 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE      ID STUDY SITE DAY DOSE AMTLOCF REG RACE SEX METS ; categorical covariates
            FILE=catab2 NOPRINT ONEHEADER FORMAT=tF13.4

