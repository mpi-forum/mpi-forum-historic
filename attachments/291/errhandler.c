/*
 * Josh Hursey
 * Feb. 3, 2012
 *
 * Example program to test Error Handler Comparisons
 *
 * Expected output:
 * shell$ mpirun -np 1 errhandler
 *  0 of   1)
 *  0 of   1) MPI_ERRORS_ARE_FATAL Case 1
 *  0 of   1) MPI_ERRORS_ARE_FATAL Case 2
 *  0 of   1) MPI_ERRORS_RETURN Case 3
 *  0 of   1) MPI_ERRORS_RETURN Case 4
 *  0 of   1) MPI_ERRORS_RETURN Case 5
 *  0 of   1) MPI_ERRORS_RETURN Case 6
 *  0 of   1) User Defined Case 9
 *  0 of   1) User Defined equal Case 10
 *  0 of   1) User Defined unequal Case 13
 */

#include <stdio.h>
#include "mpi.h"

/*
 * User defined error handler operations
 */
void errhandler_function_A(MPI_Comm *comm, int *code, ...);
void errhandler_function_B(MPI_Comm *comm, int *code, ...);

int main(int argc, char* argv[])
{
    MPI_Errhandler errhdl, errhdl2;
    MPI_Errhandler my_errhdl_A;
    MPI_Errhandler my_errhdl_B;
    MPI_Comm loc_comm;
    int mcw_rank;
    int mcw_size;

    MPI_Init(&argc, &argv);

    MPI_Comm_rank(MPI_COMM_WORLD, &mcw_rank);
    MPI_Comm_size(MPI_COMM_WORLD, &mcw_size);

    printf("%3d of %3d)\n", mcw_rank, mcw_size);

    MPI_Comm_dup(MPI_COMM_WORLD, &loc_comm);
    MPI_Barrier(MPI_COMM_WORLD);


    /*
     * Test the MPI_ERRORS_ARE_FATAL predefine
     * Both tests below are correct.
     */
    MPI_Comm_get_errhandler(MPI_COMM_WORLD, &errhdl);
    errhdl2 = MPI_ERRORS_ARE_FATAL;
    if( errhdl == errhdl2 ) {
        printf("%3d of %3d) MPI_ERRORS_ARE_FATAL Case 1\n", mcw_rank, mcw_size);
    }
    if( errhdl == MPI_ERRORS_ARE_FATAL ) {
        printf("%3d of %3d) MPI_ERRORS_ARE_FATAL Case 2\n", mcw_rank, mcw_size);
    }
    MPI_Errhandler_free(&errhdl);

    /*
     * Tests the MPI_ERRORS_RETURN predefine - direct set
     */
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    MPI_Comm_get_errhandler(MPI_COMM_WORLD, &errhdl);
    errhdl2 = MPI_ERRORS_RETURN;

    if( errhdl == errhdl2 ) {
        printf("%3d of %3d) MPI_ERRORS_RETURN Case 3\n", mcw_rank, mcw_size);
    }
    if( errhdl == MPI_ERRORS_RETURN ) {
        printf("%3d of %3d) MPI_ERRORS_RETURN Case 4\n", mcw_rank, mcw_size);
    }
    MPI_Errhandler_free(&errhdl);

    /*
     * Tests the MPI_ERRORS_RETURN predefine - indirect set
     */
    errhdl2 = MPI_ERRORS_RETURN;
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, errhdl2);
    MPI_Comm_get_errhandler(MPI_COMM_WORLD, &errhdl);

    if( errhdl == errhdl2 ) {
        printf("%3d of %3d) MPI_ERRORS_RETURN Case 5\n", mcw_rank, mcw_size);
    }
    if( errhdl == MPI_ERRORS_RETURN ) {
        printf("%3d of %3d) MPI_ERRORS_RETURN Case 6\n", mcw_rank, mcw_size);
    }
    MPI_Errhandler_free(&errhdl);

    /*
     * Test user defined
     */
    MPI_Comm_create_errhandler(errhandler_function_A, &my_errhdl_A);
    MPI_Comm_set_errhandler(MPI_COMM_WORLD, my_errhdl_A);
    MPI_Comm_get_errhandler(MPI_COMM_WORLD, &errhdl);

    if( errhdl == MPI_ERRORS_ARE_FATAL ) {
        printf("%3d of %3d) MPI_ERRORS_ARE_FATAL Case 7\n", mcw_rank, mcw_size);
    }
    else if( errhdl == MPI_ERRORS_RETURN ) {
        printf("%3d of %3d) MPI_ERRORS_RETURN Case 8\n", mcw_rank, mcw_size);
    }
    else {
        printf("%3d of %3d) User Defined Case 9\n", mcw_rank, mcw_size);
    }
    MPI_Errhandler_free(&errhdl);

    /*
     * Test user defined - equality same user defined function
     * (Unclear to me if the below is 'correct' MPI usage...)
     */
    MPI_Comm_create_errhandler(errhandler_function_A, &my_errhdl_A);

    MPI_Comm_set_errhandler(MPI_COMM_WORLD, my_errhdl_A);
    MPI_Comm_set_errhandler(loc_comm, my_errhdl_A);

    MPI_Comm_get_errhandler(MPI_COMM_WORLD, &errhdl);
    MPI_Comm_get_errhandler(loc_comm, &errhdl2);

    if( errhdl == errhdl2 ) {
        printf("%3d of %3d) User Defined equal Case 10\n", mcw_rank, mcw_size);
    }
    else {
        printf("%3d of %3d) User Defined unequal Case 11\n", mcw_rank, mcw_size);
    }
    MPI_Errhandler_free(&errhdl);
    MPI_Errhandler_free(&my_errhdl_A);

    /*
     * Test user defined - equality different user defined function
     * (Unclear to me if the below is 'correct' MPI usage...)
     */
    MPI_Comm_create_errhandler(errhandler_function_A, &my_errhdl_A);
    MPI_Comm_create_errhandler(errhandler_function_B, &my_errhdl_B);

    MPI_Comm_set_errhandler(MPI_COMM_WORLD, my_errhdl_A);
    MPI_Comm_set_errhandler(loc_comm, my_errhdl_B);

    MPI_Comm_get_errhandler(MPI_COMM_WORLD, &errhdl);
    MPI_Comm_get_errhandler(loc_comm, &errhdl2);

    if( errhdl == errhdl2 ) {
        printf("%3d of %3d) User Defined equal Case 12\n", mcw_rank, mcw_size);
    }
    else {
        printf("%3d of %3d) User Defined unequal Case 13\n", mcw_rank, mcw_size);
    }
    MPI_Errhandler_free(&errhdl);
    MPI_Errhandler_free(&my_errhdl_A);
    MPI_Errhandler_free(&my_errhdl_B);


    MPI_Comm_free(&loc_comm);

    MPI_Finalize();

    return 0;
}

void errhandler_function_A(MPI_Comm *comm, int *code, ...)
{
    return;
}

void errhandler_function_B(MPI_Comm *comm, int *code, ...)
{
    return;
}
