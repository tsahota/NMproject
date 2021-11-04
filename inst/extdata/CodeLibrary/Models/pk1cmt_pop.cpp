$PARAM
TVCL = 1, TVVC = 10, TVKA = 1, TVEC50 = 5

$CMT EV1 CENT

$OMEGA 0.1 0.2 0.3 0.5

$SIGMA 0 0

$MAIN
double CL = TVCL*exp(ETA(1));
double VC = TVVC*exp(ETA(2));
double KA = TVKA*exp(ETA(3));
double EC50 = TVEC50*exp(ETA(4));

$ODE
double CP = CENT/VC;

dxdt_EV1 = -KA*EV1;
dxdt_CENT = KA*EV1 - CL*CP;

$TABLE
double E = CP/(CP + EC50);
CP = CP*(1+EPS(1));
E = E*(1+EPS(2));

$CAPTURE CP E



