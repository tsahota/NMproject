;; 1. Based on: 7
;; 2. Description: 1CMT+oral
;; x1. Author: klgk669

$PROBLEM THEOPP example

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

$INPUT ID AMT TIME DV WT EVID TYPE
$DATA ../DerivedData/THEOPPPKPD.csv IGNORE=@ IGNORE=(TYPE.EQ.2)
$SUB ADVAN2

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

CL = K*V

S2 = V

TVINT=EXP(THETA(4))
MU_4=LOG(TVINT)
INT = EXP(MU_4+ETA(4))

TVSLOP=EXP(THETA(5))
MU_5=LOG(TVSLOP)
SLOP = EXP(MU_5+ETA(5))


$ERROR 

IPRED=F
W=SQRT(THETA(6)**2+THETA(7)**2*IPRED*IPRED)  ; proportional + additive error
IRES=DV-IPRED
IWRES=IRES/W

IF(TYPE.EQ.1) THEN
Y=IPRED+W*EPS(1)
ENDIF

PROB = 1/(1+EXP(INT+SLOP*F))

IF(TYPE.EQ.2) THEN
F_FLAG=1
Y = PROB*DV + (1-DV)*(1-PROB)
ENDIF

$THETA
1             	; KA ; h-1 ; LOG
-2.5            ; K  ; h-1 ; LOG
-0.5          	; V  ; L ; LOG
-4            ; INT  ;  ; LOG
0 FIX          	; SLOP  ;  ; LOG
0.1           	; add error
0.1           	; prop error

$OMEGA
0.1			; IIV_KA ; LOG
0.1			; IIV_K ; LOG
0.1			; IIV_V ; LOG
0.1 FIX			; IIV_INT ; LOG
0 FIX			; IIV_SLOP ; LOG

$SIGMA
1 FIX

; Parameter estimation - FOCE
;$EST METHOD=1 INTER NOABORT MAXEVAL=9999 PRINT=1 NSIG=3 SIGL=9 LAPLACIAN

; Parameter estimation - IMP
$EST METHOD=IMP ISAMPLE=300 NITER=300 RANMETHOD=3S2P
CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=3
PRINT=1 NOABORT INTERACTION GRD=DDDDDSS LAPLACIAN

; Parameer estimation - SAEM
;$EST METHOD=SAEM ISAMPLE=2 NBURN=1000 NITER=500 RANMETHOD=3S2P
;CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=10
;PRINT=1 NOABORT INTERACTION GRD=DDDDDSS LAPLACIAN

; Objective function and covariance evaluation
$EST METHOD=IMP INTER EONLY= 1 MAPITER=0 ISAMPLE = 2000 NITER = 10 RANMETHOD=3S2P NOABORT PRINT=1 NSIG=3 SIGL=9 GRD=DDDDDSS LAPLACIAN

$COV PRINT=E UNCONDITIONAL SIGL=10

$TABLE ID TIME IPRED IWRES IRES CWRES NPDE
FILE=sdtab8 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID ETAS(1:LAST); individual parameters
FILE=patab8 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID ; continuous covariates
FILE=cotab8 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID ; categorical covariates
FILE=catab8 NOPRINT ONEHEADER FORMAT=tF13.4
