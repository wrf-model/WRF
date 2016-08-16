FILE=$(date +"%Y-%m-%d_%H-%M-%S_rapid_stdout.txt")
/usr/bin/time mpiexec                  \
              -n 1                     \
              ./rapid                  \
              -ksp_type richardson     \
              1>$FILE 2>>$FILE

#FILE is a name created based on the time when the model started running
#FILE contains stdout from running the model (through 1), but also stderr 
#(through 2).  The output of the time function is also included because 
#it is located in located in 2.
