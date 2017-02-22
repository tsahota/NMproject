logfile=run1.log
search_direction = both
p_forward=0.05
p_backward=0.01
continuous_covariates=AGE
categorical_covariates=ACE,DIG,DIU,SEX,NYHA2

[test_relations]
CL=AGE,ACE,DIG,DIU,SEX,NYHA2
V= AGE,ACE,DIG,DIU,SEX,NYHA2

[valid_states]
continuous = 1,2
categorical= 1,2
