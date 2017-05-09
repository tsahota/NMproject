mpirun -binding cpu -rmk slurm -topolib hwloc -np 1 ./nonmem $* : \
-np 9 ./nonmem -wf : \
-np 10 ./nonmem -wf : \
-np 10 ./nonmem -wf
