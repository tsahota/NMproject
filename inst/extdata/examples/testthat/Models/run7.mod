;; 1. Based on: 5
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
$DATA ../DerivedData/THEOPPPKPD.csv IGNORE=@ IGNORE=(TYPE.GT.2)
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

TVEMAX=EXP(THETA(4))
MU_4=LOG(TVEMAX)
EMAX = EXP(MU_4+ETA(4))

TVEC50=EXP(THETA(5))
MU_5=LOG(TVEC50)
EC50 = EXP(MU_5+ETA(5))


$ERROR 

IF(TYPE.EQ.1) THEN
IPRED=F
ENDIF

IF(TYPE.EQ.2) THEN
IPRED=F*EMAX/(F+EC50)
ENDIF

W=SQRT(THETA(6)**2+THETA(7)**2*IPRED*IPRED)  ; proportional + additive error
IRES=DV-IPRED
IWRES=IRES/W
Y=IPRED+W*EPS(1)


$THETA
1             	; KA ; h-1 ; LOG
-2.5            ; K  ; h-1 ; LOG
-0.5          	; V  ; L ; LOG
0.1            ; EMAX  ;  ; LOG
0.1          	; EC50  ; ng/ml ; LOG
0.1           	; add error
0.1           	; prop error

$OMEGA
0.1			; IIV_KA ; LOG
0.1			; IIV_K ; LOG
0.1			; IIV_V ; LOG
0.1			; IIV_EMAX ; LOG
0.1			; IIV_EC50 ; LOG

$SIGMA
1 FIX

; Parameter estimation - FOCE
;$EST METHOD=1 INTER NOABORT MAXEVAL=9999 PRINT=1 NSIG=3 SIGL=9

; Parameter estimation - IMP
$EST METHOD=IMP ISAMPLE=300 NITER=300 RANMETHOD=3S2P
CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=3
PRINT=1 NOABORT INTERACTION GRD=DDDDDSS

; Parameer estimation - SAEM
;$EST METHOD=SAEM ISAMPLE=2 NBURN=1000 NITER=500 RANMETHOD=3S2P
;CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=10
;PRINT=1 NOABORT INTERACTION GRD=DDDDDSS

; Objective function and covariance evaluation
$EST METHOD=IMP INTER EONLY= 1 MAPITER=0 ISAMPLE = 2000 NITER = 10 RANMETHOD=3S2P NOABORT PRINT=1 NSIG=3 SIGL=9 GRD=DDDDDSS

$COV PRINT=E UNCONDITIONAL SIGL=10

$TABLE ID TIME IPRED IWRES IRES CWRES NPDE
FILE=sdtab7 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID ETAS(1:LAST); individual parameters
FILE=patab7 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID ; continuous covariates
FILE=cotab7 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID ; categorical covariates
FILE=catab7 NOPRINT ONEHEADER FORMAT=tF13.4
