;; 1. Based on: ...
;; 2. Description: Logistic regression model
;; x1. Author: ...

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

$INPUT ... DOSE
$DATA ... IGNORE=@ 

$PRED

TVINT=THETA(1)
MU_1=TVINT
INT = MU_1+ETA(1)

TVSLOP=THETA(2)
MU_2 = TVSLOP
SLOP = MU_2+ETA(2)

PROB0 = 1/(1+EXP(INT+SLOP*DOSE))

IF(DV.LT.3) THEN 
  PROB = PROB0
ELSE 
  PROB = 1-PROB0
ENDIF 

F_FLAG=1
Y = PROB

DUM = EPS(1) ;; dummy variable

$THETA
.....          	; INT ; ; 
.....          	; SLOP ; ; 

$OMEGA
0 FIX       ; IIV_EMAX ; LOG
0 FIX       ; IIV_E50 ; LOG

$SIGMA
1 FIX

; Parameter estimation - FOCE
;$EST METHOD=1 INTER NOABORT MAXEVAL=9999 PRINT=1 NSIG=3 SIGL=9 LAPLACIAN

; Parameter estimation - IMP
$EST METHOD=IMP ISAMPLE=300 NITER=300 RANMETHOD=3S2 
CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=3
PRINT=1 NOABORT INTERACTION LAPLACIAN

; Parameer estimation - SAEM
;$EST METHOD=SAEM ISAMPLE=2 NBURN=1000 NITER=500 RANMETHOD=3S2
;CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=10
;PRINT=1 NOABORT INTERACTION LAPLACIAN

; Objective function and covariance evaluation
$EST METHOD=IMP INTER EONLY= 1 MAPITER=0 ISAMPLE = 2000 NITER = 10 RANMETHOD=3S2 NOABORT PRINT=1 NSIG=3 SIGL=9 LAPLACIAN

$COV MATRIX=R PRINT=E UNCONDITIONAL SIGL=10

;$SIM (1234) ONLYSIM SUBPR=1

$TABLE ID CWRES NPDE
FILE=sdtab1 NOPRINT ONEHEADER
$TABLE ID ETAS(1:LAST); individual parameters
FILE=patab1 NOPRINT ONEHEADER
$TABLE ID ; continuous covariates
FILE=cotab1 NOPRINT ONEHEADER
$TABLE ID ; categorical covariates
FILE=catab1 NOPRINT ONEHEADER
