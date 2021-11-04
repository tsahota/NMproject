;; 1. Based on: ...
;; 2. Description: 1CMT (TRANS1)
;; 3. Author: ...

$PROBLEM ...
$INPUT ...
$DATA ... IGNORE=@ 
$SUB ADVAN1 TRANS1

$PK

TVK = EXP(THETA(1))
MU_1 = LOG(TVK)
K = EXP(MU_1+ETA(1))

TVV = EXP(THETA(2))
MU_2 = LOG(TVV)
V = EXP(MU_2+ETA(2))

S1 = V

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

$OMEGA 
0.1		                	; IIV_K ; LOG
0.1			                ; IIV_V ; LOG

$SIGMA
0.1           	; prop error
.....          	; add error

;; Parameter estimation - FOCE
;$EST METHOD=1 INTER NOABORT MAXEVAL=9999 PRINT=1 NSIG=3 SIGL=9

;; Parameter estimation - LAP
;$EST METHOD=1 LAP INTER NOABORT MAXEVAL=9999 PRINT=1 NSIG=3 SIGL=9

;; Parameter estimation - IMP
$EST METHOD=IMP ISAMPLE=300 NITER=300 RANMETHOD=3S2 
CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=3
PRINT=1 NOABORT INTERACTION

;; Parameer estimation - SAEM
;$EST METHOD=SAEM ISAMPLE=2 NBURN=1000 NITER=500 RANMETHOD=3S2
;CTYPE=3 CITER=10 CALPHA=0.05 CINTERVAL=10
;PRINT=1 NOABORT INTERACTION

;; Objective function and covariance evaluation
$EST METHOD=IMP INTER EONLY= 1 MAPITER=0 ISAMPLE = 2000 NITER = 10 RANMETHOD=3S2 NOABORT PRINT=1 NSIG=3 SIGL=9

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
