;; 1. Based on: run1
;; 2. Description: Count data (Neg-binomial) model
;; x1. Author: John Smith

$PROBLEM ...

;; 4. Date: 01.01.2011
;; 5. Version: 1
;; 6. Label:
;; Basic model
;; 7. Structural model:
;; poisson
;; 8. Covariate model:
;; No covariates
;; 9. Inter-individual variability:
;; Lambda
;; 10. Inter-occasion variability:
;; No IOV
;; 11. Residual variability:
;; poisson distribution
;; 12. Estimation:
;; IMP

$INPUT ...
$DATA ... IGNORE=@ 

$PRED

TVLAMB=EXP(THETA(1))
MU_1=LOG(TVLAMB)
LAMB = EXP(MU_1+ETA(1))

TVOVDP=EXP(THETA(2))
MU_2=LOG(TVOVDP)
OVDP = EXP(MU_2+ETA(2))

; ----- Factorial approximation for Factorial function (Srinivasa Ramanujan) -----

IF(DV.LE.1) THEN 
  LFAC=0 
ELSE 
  LFAC = DV*LOG(DV)-DV +LOG(DV*(1+4*DV*(1+2*DV)))/6 +LOG(3.1415)/2 
ENDIF 

; ----- Arguments for Gamma function -----
AGM1 = DV + (1/OVDP)
AGM2 = 1/OVDP

; ----- Factorial approximation for Gamma function (Srinivasa Ramanujan) -----

LAGM1 = (AGM1-1)*LOG(AGM1) - AGM1 + LOG(AGM1*(1+4*AGM1*(1+2*AGM1)))/6 + LOG(3.1415)/2
LAGM2 = (AGM2-1)*LOG(AGM2) - AGM2 + LOG(AGM2*(1+4*AGM2*(1+2*AGM2)))/6 + LOG(3.1415)/2

LPART1 = LOG(1/(1+OVDP*LAM))*(1/OVDP)
LPART2 = LOG(LAM / ((1/OVDP)+LAM))*DV

LBM = LAGM1 - LAGM2 - LFAC + LPART1 + LPART2

F_FLAG=1
Y = LBM

DUM = EPS(1)  ;; not used

$THETA
.....          	; LAMB ; ; LOG
.....          	; OVDP ; ; LOG
$OMEGA
0.0225 FIX ; IIV_LAMB ; LOG
0.0225 FIX ; IIV_OVDP ; LOG

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

$COV PRINT=E UNCONDITIONAL SIGL=10

$TABLE ID TIME IPRED IWRES IRES CWRES NPDE
FILE=sdtab1 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID LAMB; individual parameters
FILE=patab1 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID ; continuous covariates
FILE=cotab1 NOPRINT ONEHEADER FORMAT=tF13.4
$TABLE ID ; categorical covariates
FILE=catab1 NOPRINT ONEHEADER FORMAT=tF13.4
