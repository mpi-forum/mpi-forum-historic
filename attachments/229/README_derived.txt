Test and file names:

  derived_types_test_1_impl_integer.f90
  derived_types_test_2_impl_sequ_bc.f90
  derived_types_test_3_impl_derived.f90
  derived_types_test_4_expl_integer.f90
  derived_types_test_5_expl_sequ_bc.f90
  derived_types_test_6_expl_derived.f90
  derived_types_test_7_all.f90
  derived_types_test_1+2_impl_int+seq+bc.f90

  with
    impl = Implicitly defined MPI routine is called.
    expl = Two explicitly defined MPI routines are called
            - without BIND(C),
            - with BIND(C) in the subroutine declaration.
           The buf dummy argument is defined with a TKR ignore directive.

    integer = Actual argument is a derived type and
              it is passed through first (integer) element.
              Three different derived types are used:
               - normal,
               - with SEQUENCE attribute,
               - with BIND(C) attribute.
    sequ_bc = Actual argument is a derived type with
               - with SEQUENCE attribute,
               - with BIND(C) attribute.
    derived = Actual argument is a normal derived type.

  The last test "1+2" is a special test to show a compile-time
  error only detected by nagfor. This test compines the calls in
  ..._1_impl_integer.f90 and ..._2_impl_sequ_bc.f90.


Compiler results may be:

    R  - rejected by the compiler
    Rs - rejected by the compiler (routines and main separately compiled)
    i  - compiles but issues an information or warning and runs correctly
    y  - compiles and runs correctly
    ys - compiles and runs correctly (routines and main separately compiled)
    N  - compiles, then runs with run time failure


Test -->          |  ---impl---   ---expl---   all   impl
                  |  int seq drv  int seq drv      int+seq
                  |   1   2   3    4   5   6    7    1+2
------------------+---------------------------------------
ifort 11.1        |   y   y   y    y   y   y    y     y
ifort 12.0        |   y   y   y    y   y   y    y     y
------------------+---------------------------------------
gfortran 4.6      |   y   y   y    R   R   R    R     y
nagfor 5.2        |   ys  ys  ys   Rs  Rs  Rs   Rs    Rs
------------------+---------------------------------------
xlf 13.1          |   y   y   R    y   y   R    R     y
crayftn 7.3       |   i   i   i    y   y   y    i     i
------------------+---------------------------------------
pgi 11.3          |   y   y   y    y   y   y    y     y
pathscale 3.3b    |   i   i   i    y   y   y    i     i
------------------+---------------------------------------

Notes:
 - Compile-time information, warning and errors with the CALL statements

      gfortran + 1,all      : Rank mismatch warning (rank-1 vs scalar lines for all invocations)
      gfortran + 2,3,all    : Type mismatch warning
      gfortran + 4,all      : Rank mismatch error (rank-1 vs scalar lines for all invocations),
                                i.e., none of the TKR_IGNORE directives work
      gfortran + 5,6,all    : Type mismatch error,
                                i.e., none of the TKR_IGNORE directives work
      nagfor  + 4,all       : Rank mismatch error (rank-1 vs scalar lines for all invocations),
                                i.e., none of the TKR_IGNORE directives work
      nagfor  + 5,6,all     : Type mismatch error,
                                i.e., none of the TKR_IGNORE directives work
      nagfor  + 1+2, all    : Error: Inconsistency between several calls of implicitly defined routine:
                                "Inconsistent data type INTEGER (previously USER_BUFFER_NORMAL_TYPE)
                                for argument 3 in reference to FOO_IMPL",
                                i.e., a combination of the tests in 1 and 2 or 3 may fail.
      crayftn+1,2,3,all,1+2 : Rank mismatch diagnosed, but runs correctly
      xlf + 3,6, all        : Severe error:
                                "A derived-type object that appears as an actual argument in
                                an external procedure reference must be of a sequence type or
                                a type with the BIND(C) attribute, unless it is use-associated
                                or of a type that is use-associated or defined in the
                                specification part of a module.
      pathscale+1-3,all,1+2 : Warns about rank mismatch, but runs correctly.

 - Compile-time information, warning and errors with the declaration statement of MPI_foo...,
   i.e., when compiling the MPI library:

      gfortran + 4,5,6,all  : Warning with BIND(C) declarations:
                                INTEGER may not be interoperable with C, related
                                to line "SUBROUTINE MPI_foo_ex_c(c1,c2,buf,test1,test2) BIND(C)"
      nagfor + "routines"   : Additional information that BIND(C) is an "extension"
                                in line "SUBROUTINE MPI_foo_ex_c(c1,c2,buf,test1,test2) BIND(C)"

 - Other compile-time information, warning and errors:

      gfortran+1,2,4,5,all,1+2:Warning with BIND(C) declarations:
                                INTEGER may not be interoperable with C, related
                                to line "TYPE, BIND(C) :: user_buffer_bind_c_type"
      nagfor+1,2,4,5,all,1+2: Additional information that BIND(C) is an "extension"
                                in line "TYPE, BIND(C) :: user_buffer_bind_c_type"

 - Runtime errors were never detected, i.e.,
   in the cases with actual argument = buf%VAL1 (i.e., the first element of the derived type),
   all compilers used call by reference, i.e., the called implicit or explicit routines
   could correctly access also the second element of the derived type.

Acknowledgments: 
 I want to thank Reinhold Bader (LRZ, Munich, Germany) who made all the compiler protocols.

Author: Rolf Rabenseifner (HLRS, University of Stuttgart, Germany) 
