;; 1. Based on: run1
;; 2. Description: Emax model
;; x1. Author: John Smith

$PROBLEM ...

;; 4. Date: 01.01.2011
;; 5. Version: 1
;; 6. Label:
;; Basic model
;; 7. Structural model:
;; Emax
;; 8. Covariate model:
;; No covariates
;; 9. Inter-individual variability:
;; 
;; 10. Inter-occasion variability:
;; No IOV
;; 11. Residual variability:
;; Additive + Proportional
;; 12. Estimation:
;; IMP

$INPUT ... CP
$DATA ... IGNORE=@ 

$PRED

TVEMAX=EXP(THETA(1))
MU_1=LOG(TVEMAX)
EMAX = EXP(MU_1+ETA(1))

TVE50=EXP(THETA(2))
MU_2=LOG(TVE50)
E50 = EXP(MU_2+ETA(2))

IPRED = EMAX*CP/(E50+CP)

W=SQRT(THETA(3)**2+THETA(4)**2*IPRED*IPRED)  ; proportional + additive error
IRES=DV-IPRED
IWRES=IRES/W
Y=IPRED+W*EPS(1)


$THETA
.....          	; EMAX ; ; LOG
.....          	; E50 ; ; LOG
.....          	; add error
.....          	; prop error

$OMEGA
0 FIX  ; IIV_EMAX ; LOG
0 FIX  ; IIV_E50 ; LOG

$SIGMA
1 FIX

; Parameter estimation - FOCE
;$EST METHOD=1 INTER NOABORT MAXEVAL=9999 PRINT=1 NSIG=3 SIGL=9

; Parameter estimation - IMP
$EST METHOD=IMP ISAMPLE=300 NITER=300 RANMETHOD=3S2 
CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=3
PRINT=1 NOABORT INTERACTION GRD=DDDSS

; Parameer estimation - SAEM
;$EST METHOD=SAEM ISAMPLE=2 NBURN=1000 NITER=500 RANMETHOD=3S2
;CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=10
;PRINT=1 NOABORT INTERACTION GRD=DDDSS

; Objective function and covariance evaluation
$EST METHOD=IMP INTER EONLY= 1 MAPITER=0 ISAMPLE = 2000 NITER = 10 RANMETHOD=3S2 NOABORT PRINT=1 NSIG=3 SIGL=9 GRD=DDDSS

$COV PRINT=E UNCONDITIONAL SIGL=10

$TABLE ID TIME IPRED IWRES IRES CWRES NPDE
FILE=sdtab1 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID ; individual parameters
FILE=patab1 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID ; continuous covariates
FILE=cotab1 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID ; categorical covariates
FILE=catab1 NOPRINT ONEHEADER FORMAT=tF13.4
