;; 1. Based on: ...
;; 2. Description: Differential equation use
;; x1. Author: ...

$PROBLEM Time to event model

;; 4. Date: 01.01.2011
;; 5. Version: 1
;; 6. Label:
;; Basic model
;; 7. Structural model:
;; TTE model
;; 8. Covariate model:
;; No covariates
;; 9. Inter-individual variability:
;; LAMB
;; 10. Inter-occasion variability:
;; No IOV
;; 11. Residual variability:
;; None
;; 12. Estimation:
;; IMP

$INPUT ...

$DATA ... IGNORE=@
  
$SUB ADVAN13 TOL=12
  
$MODEL 
COMP = (CENTRAL, DEFOBS)

$PK

TVLAMB=EXP(THETA(1))
MU_1=LOG(TVLAMB)
LAMB = EXP(MU_1+ETA(1))
  
A_0(1) = LAMB

$DES

DADT(1) = LAMB

$ERROR 

CUMHAZ = A(1)
SURV = EXP(-CUMHAZ)

PROB_0 = SURV
PROB_1 = 1-SURV

F_FLAG = 1

IF(DV.EQ.0) PROB = PROB_0
IF(DV.EQ.1) PROB = PROB_1

Y = PROB

DUM = EPS(1)


$THETA      
-8           ; LAMB ; 1/h ; LOG

$OMEGA    
0.0225 FIX   ; IIV_LAMB

$SIGMA    
0 FIX          	  ; DUM

; Parameter estimation - FOCE
;$EST METHOD=1 INTER NOABORT MAXEVAL=9999 PRINT=1 NSIG=3 SIGL=9

; Parameter estimation - IMP
$EST METHOD=IMP ISAMPLE=500 NITER=600 RANMETHOD=3S2 
CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=3
PRINT=1 NOABORT INTERACTION LAPLACIAN

; Parameer estimation - SAEM
;$EST METHOD=SAEM ISAMPLE=2 NBURN=1000 NITER=500 RANMETHOD=3S2
;CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=10
;PRINT=1 NOABORT INTERACTION LAPLACIAN

; Objective function and covariance evaluation
$EST METHOD=IMP INTER EONLY= 1 MAPITER=0 ISAMPLE = 2000 NITER = 10 RANMETHOD=3S2
NOABORT PRINT=1 NSIG=3 SIGL=9 LAPLACIAN

;$SIM (2342) ONLYSIM SUBPR=1

$COV MATRIX=R PRINT=E UNCONDITIONAL SIGL=10

$TABLE ID TIME IPRED IWRES IRES CWRES NPDE
FILE=sdtabtt1 NOPRINT ONEHEADER
$TABLE ID ETAS(1:LAST) ; individual parameters
FILE=patabtt1 NOPRINT ONEHEADER
$TABLE ID ; continuous covariates
FILE=cotabtt1 NOPRINT ONEHEADER
$TABLE ID ; categorical covariates
FILE=catabtt1 NOPRINT ONEHEADER
