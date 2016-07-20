Test and file names:

   Implementing new MPI handles and status with ... 
    A = Fortran derived type 
    B = Fortran sequence derived type 
    C = Fortran BIND(C) derived type 
  
   Usage of new MPI handles and status in the application within ...
    u1 = a user defined common block
    u2 = a user defined derived type
    u3 = a user defined sequence derived type
    u4 = a user defined BIND(C) derived type
  
   Call an MPI routine that is defined
    c1 = without BIND(C) 
    c2 = with BIND(C) 

Compiler: 
 
   Result are:
    R - rejected by the compiler
    i - compiles but issues an information or warning
    y - compiles 
 

Test -->          | A A A A  A A | B B B B  B B | C C C C  C C |
                  | u u u u  c c | u u u u  c c | u u u u  c c |
                  | 1 2 3 4  1 2 | 1 2 3 4  1 2 | 1 2 3 4  1 2 |
------------------+--------------+--------------+--------------+
ifort  6.1        | R y R y  y y | y y y y  y y | R y R y  y y |
ifort 11.1        | y y R y  y y | y y y y  y y | y y R y  y y |
ifort 12.0        | y y R y  y y | y y y y  y y | y y R y  y y |
------------------+--------------+--------------+--------------+
gfortran 4.6      | R y R R  y R | y y y R  y R | i i R i  i i |
nagfor 5.2        | R i R R  i R | y i i R  i R | y i R i  i i |
------------------+--------------+--------------+--------------+
xlf 13.1          | R y R R  y R | y y y R  y R | y y R y  y y |
crayftn 7.3       | R y R R  y y | y y y R  y y | y y R y  y y |
------------------+--------------+--------------+--------------+
pgi 11.3          | y y R y  y y | y y y y  y y | y y R y  y y |
pathscale 3.3b    | R i R R  i R | i i i R  i R | i i R i  i i |
------------------+--------------+--------------+--------------+

Notes: 
      "i" entries from the NAG compiler are due to defined but unused entities.
      "i" entries from the gfortran compiler are wrt INTEGER, ... potentially 
          being non-interoperable.
      "i" entries from the Pathscale compiler are (irrelevant) link-time messages.

Acknowledgments: 
 I want to thank Reinhold Bader (LRZ, Munich, Germany) who made all the compiler tests.

Author: Rolf Rabenseifner (HLRS, University of Stuttgart, Germany) 
