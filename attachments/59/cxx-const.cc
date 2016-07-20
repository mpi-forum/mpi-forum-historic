#include <mpi.h>

void ferr_fn(MPI::File& file, int *err, ...) {}
void cerr_fn(MPI::Comm& comm, int *err, ...) {}
void werr_fn(MPI::Win& win, int *err, ...) {}

int main(int argc, char* argv[])
{
    MPI::Init();

    //////////////////////////////////////////////////////////////////

    MPI::Errhandler ferrh = MPI::File::Create_errhandler(ferr_fn);
    MPI::FILE_NULL.Set_errhandler(ferrh);

    MPI::Errhandler cerrh = MPI::Comm::Create_errhandler(cerr_fn);
    MPI::Intracomm comm = MPI::COMM_WORLD.Dup();
    MPI::COMM_WORLD.Set_errhandler(cerrh);
    // Fails to compile (which is good)
    //MPI::COMM_NULL.Set_errhandler(cerrh);
    comm.Set_errhandler(cerrh);

    int buf;
    MPI::Errhandler werrh = MPI::Win::Create_errhandler(werr_fn);
    MPI::Win win = MPI::Win::Create(&buf, sizeof(int), sizeof(int), 
                                    MPI::INFO_NULL, MPI::COMM_WORLD);
    win.Set_errhandler(werrh);

    //////////////////////////////////////////////////////////////////

    MPI::COMM_WORLD.Set_attr(0, 0);
    // Fails to compile (which is good)
    //MPI::COMM_NULL.Set_attr(0, 0);
    comm.Set_attr(0, 0);

    MPI::INT.Set_attr(0, 0);
    MPI::Datatype type = MPI::CHAR.Dup();
    type.Set_attr(0, 0);

    // Do we want this to compile?
    MPI::WIN_NULL.Set_attr(0, 0);
    win.Set_attr(0, 0);

    //////////////////////////////////////////////////////////////////

    // Need tests for: MPI::REQUEST_NULL, MPI::COMM_NULL

    comm.Free();
    win.Free();
    MPI::Finalize();
    return 0;
}
