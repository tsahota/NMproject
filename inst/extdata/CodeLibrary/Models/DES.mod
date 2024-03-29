;; 1. Based on: ...
;; 2. Description: Differential equation use
;; x1. Author: ...

$PROBLEM ...

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

$INPUT ...
$DATA ... IGNORE=@ 
$SUB ADVAN13 TOL=12
$MODEL 
       COMP = (DEPOT)
       COMP = (CENTRAL)

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

$DES

DADT(1) = -KA*A(1)
DADT(2) =  KA*A(1) - K*A(2)

$ERROR 

IPRED=F
W=SQRT(SIGMA(1,1)*IPRED**2 + SIGMA(2,2))  ; proportional + additive error
IRES=DV-IPRED
IWRES=IRES/W
Y=IPRED+W*EPS(1)


$THETA
.....          	; KA ; h-1 ; LOG
.....          	; K  ; h-1 ; LOG
.....          	; V  ; L ; LOG

$OMEGA 
0.1	         		; IIV_KA ; LOG
0.1		        	; IIV_K ; LOG
0.1		        	; IIV_V ; LOG

$SIGMA
0.1           	; prop error
.....          	; add error

;; Parameter estimation - FOCE
;$EST METHOD=1 INTER NOABORT MAXEVAL=9999 PRINT=1 NSIG=3 SIGL=9

;; Parameter estimation - IMP
$EST METHOD=IMP ISAMPLE=300 NITER=300 RANMETHOD=3S2P 
CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=3
PRINT=1 NOABORT INTERACTION

;; Parameter estimation - SAEM
;$EST METHOD=SAEM ISAMPLE=2 NBURN=1000 NITER=500 RANMETHOD=3S2P
;CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=10
;PRINT=1 NOABORT INTERACTION

;; Objective function and covariance evaluation
$EST METHOD=IMP INTER EONLY= 1 MAPITER=0 ISAMPLE = 2000 NITER = 10 RANMETHOD=3S2P NOABORT PRINT=1 NSIG=3 SIGL=9

$COV MATRIX=R PRINT=E UNCONDITIONAL SIGL=10

;$SIM (1234) ONLYSIM SUBPR=1

$TABLE ID TIME IPRED IWRES IRES CWRES NPDE
FILE=sdtab1 NOPRINT ONEHEADER
$TABLE ID ETAS(1:LAST); individual parameters
FILE=patab1 NOPRINT ONEHEADER
$TABLE ID ; continuous covariates
FILE=cotab1 NOPRINT ONEHEADER
$TABLE ID ; categorical covariates
FILE=catab1 NOPRINT ONEHEADER
