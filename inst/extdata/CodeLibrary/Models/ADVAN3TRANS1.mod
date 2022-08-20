;; 1. Based on: ...
;; 2. Description: 2CMT (TRANS1)
;; 3. Author: ...

$PROBLEM ...
$INPUT ...
$DATA ... IGNORE=@ 
$SUB ADVAN3 TRANS1

$PK

TVK = EXP(THETA(1))
MU_1 = LOG(TVK)
K = EXP(MU_1+ETA(1))

TVV = EXP(THETA(2))
MU_2 = LOG(TVV)
V = EXP(MU_2+ETA(2))

TVK12 = EXP(THETA(3))
MU_3 = LOG(TVK12)
K12 = EXP(MU_3+ETA(3))

TVK21 = EXP(THETA(4))
MU_4 = LOG(TVK21)
K21 = EXP(MU_4+ETA(4))

V2 = V*K12/K21

S1 = V
S2 = V2

$ERROR 

;; DV untransformed (proportional and additive error)
IPRED = F
W = SQRT(SIGMA(1,1)*IPRED**2 + SIGMA(2,2))

;; DV logscale (exponential error)
; IF (F.GE.0.0001) IPRED = LOG(F)
; IF (F.LT.0.0001) IPRED = LOG(0.0001)
; W = SQRT(SIGMA(1,1))

IRES = DV-IPRED
IWRES = IRES/W

;; Uncomment below for M3 method (requires LLOQ column in dataset)
;; IF (DV.GE.LLOQ) THEN
;   F_FLAG = 0
    Y = IPRED + IPRED*EPS(1) + EPS(2)   ; DV untransformed (proportional and additive error)
;    Y = IPRED + EPS(1)                 ; DV logscale (exponential error)
; ELSE
;   F_FLAG = 1
;   Y = PHI((LLOQ-IPRED)/W)
; ENDIF

$THETA
.....          	; K  ; h-1 ; LOG
.....          	; V ; L ; LOG
.....          	; K12 ; h-1 ; LOG
.....          	; K21 ; h-1 ; LOG

$OMEGA 
0.1		                	; IIV_K ; LOG
0.1			                ; IIV_V ; LOG
0.1		                	; IIV_K12 ; LOG
0.1		                	; IIV_K21 ; LOG

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

;; Parameter estimation - NUTS
;$EST METHOD=NUTS INTERACTION AUTO=1 PRINT=10 FILE=stan1.ext

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
