! The total test file consists of three parts:
! 
!  - MODULE test_callback_routines 
!      with all needed callback routines 
!  - MODULE test_routines
!      with one individual test routine for each MPI routine 
!  - PROGRAM test_mpif08 
!      which calls all the individual test routines 
!
! ------------------------------------------------------------------------------------ 

MODULE test_callback_routines
 
! This part is (from) mpif08_test_callbacks_header.f90
! It is based on mpif08_header.f90 and the function names used 
! in the PROCEDURE statements in mpi-3.0-all-interfaces-06-f2008.f90

CONTAINS 
 
! ABSTRACT INTERFACE
! SUBROUTINE MPI_User_function(invec, inoutvec, len, datatype) BIND(C)
  SUBROUTINE user_fn          (invec, inoutvec, len, datatype) BIND(C)
    USE mpi_f08 
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR 
    IMPLICIT NONE 
!   TYPE(*), DIMENSION(*) :: invec(len), inoutvec(len)
    TYPE(C_PTR), VALUE :: invec, inoutvec
    INTEGER, INTENT(IN) :: len
    TYPE(MPI_Datatype) :: datatype
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Comm_copy_attr_function(oldcomm, comm_keyval, extra_state, attribute_val_in, attribute_val_out,flag,ierror) BIND(C)
  SUBROUTINE comm_copy_attr_fn          (oldcomm, comm_keyval, extra_state, attribute_val_in, attribute_val_out,flag,ierror) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: oldcomm
    INTEGER :: comm_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    LOGICAL :: flag
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Comm_delete_attr_function(comm, comm_keyval, attribute_val, extra_state, ierror) BIND(C)
  SUBROUTINE comm_delete_attr_fn          (comm, comm_keyval, attribute_val, extra_state, ierror) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: comm
    INTEGER :: comm_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Win_copy_attr_function(oldwin, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror) BIND(C)
  SUBROUTINE win_copy_attr_fn          (oldwin, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    TYPE(MPI_Win) :: oldwin
    INTEGER :: win_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    LOGICAL :: flag
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Win_delete_attr_function(win, win_keyval, attribute_val, extra_state, ierror) BIND(C)
  SUBROUTINE win_delete_attr_fn          (win, win_keyval, attribute_val, extra_state, ierror) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    TYPE(MPI_Win) :: win
    INTEGER :: win_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Type_copy_attr_function(oldtype, type_keyval, extra_state, attribute_val_in,attribute_val_out, flag,ierror) BIND(C)
  SUBROUTINE type_copy_attr_fn          (oldtype, type_keyval, extra_state, attribute_val_in,attribute_val_out, flag,ierror) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    TYPE(MPI_Datatype) :: oldtype
    INTEGER :: type_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    LOGICAL :: flag
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Type_delete_attr_function(datatype, type_keyval, attribute_val, extra_state, ierror) BIND(C)
  SUBROUTINE type_delete_attr_fn          (datatype, type_keyval, attribute_val, extra_state, ierror) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    TYPE(MPI_Datatype) :: datatype
    INTEGER :: type_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Comm_errhandler_function(comm, error_code) BIND(C)
  SUBROUTINE comm_errhandler_fn          (comm, error_code) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: comm
    INTEGER :: error_code
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Win_errhandler_function(win, error_code) BIND(C)
  SUBROUTINE win_errhandler_fn          (win, error_code) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    TYPE(MPI_Win) :: win
    INTEGER :: error_code
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_File_errhandler_function(file, error_code) BIND(C)
  SUBROUTINE file_errhandler_fn          (file, error_code) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    TYPE(MPI_File) :: file
    INTEGER :: error_code
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Grequest_query_function(extra_state, status, ierror) BIND(C)
  SUBROUTINE query_fn                   (extra_state, status, ierror) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    TYPE(MPI_Status) :: status
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Grequest_free_function(extra_state, ierror) BIND(C)
  SUBROUTINE free_fn                   (extra_state, ierror) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Grequest_cancel_function(extra_state, complete, ierror) BIND(C)
  SUBROUTINE cancel_fn                   (extra_state, complete, ierror) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
    LOGICAL :: complete
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Datarep_extent_function(datatype, extent, extra_state, ierror) BIND(C)
  SUBROUTINE dtype_file_extent_fn       (datatype, extent, extra_state, ierror) BIND(C)
    USE mpi_f08 
    IMPLICIT NONE 
    TYPE(MPI_Datatype) :: datatype
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extent, extra_state
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Datarep_conversion_function(userbuf, datatype, count, filebuf, position, extra_state, ierror) BIND(C)
  SUBROUTINE read_conversion_fn             (userbuf, datatype, count, filebuf, position, extra_state, ierror) BIND(C)
    USE mpi_f08 
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR 
    IMPLICIT NONE 
!   TYPE(*), DIMENSION(*) :: userbuf(*), filebuf(*)
    TYPE(C_PTR), VALUE :: userbuf, filebuf
    TYPE(MPI_Datatype) :: datatype
    INTEGER :: count, ierror
    INTEGER(KIND=MPI_OFFSET_KIND) :: position
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  END SUBROUTINE
! END INTERFACE

! ABSTRACT INTERFACE
! SUBROUTINE MPI_Datarep_conversion_function(userbuf, datatype, count, filebuf, position, extra_state, ierror) BIND(C)
  SUBROUTINE write_conversion_fn            (userbuf, datatype, count, filebuf, position, extra_state, ierror) BIND(C)
    USE mpi_f08 
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR 
    IMPLICIT NONE 
!   TYPE(*), DIMENSION(*) :: userbuf(*), filebuf(*)
    TYPE(C_PTR), VALUE :: userbuf, filebuf
    TYPE(MPI_Datatype) :: datatype
    INTEGER :: count, ierror
    INTEGER(KIND=MPI_OFFSET_KIND) :: position
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  END SUBROUTINE
! END INTERFACE

END MODULE test_callback_routines

! ------------------------------------------------------------------------------------ 
 
!-MODULE test_routines
!-CONTAINS


SUBROUTINE TST_Bsend
  USE mpi_f08
  REAL, DIMENSION(5) :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Bsend(buf, count, datatype, dest, tag, comm, ierror)
  CALL MPI_Bsend(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Bsend_init
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Bsend_init(buf, count, datatype, dest, tag, comm, request, ierror)
  CALL MPI_Bsend_init(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Buffer_attach
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buffer
  INTEGER :: size
  INTEGER :: ierror
  size = 5
  CALL MPI_Buffer_attach(buffer, size, ierror)
  CALL MPI_Buffer_attach(buffer=buffer, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Buffer_detach
  USE mpi_f08
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
  TYPE(C_PTR) :: buffer_addr
  INTEGER :: size
  INTEGER :: ierror
  CALL MPI_Buffer_detach(buffer_addr, size, ierror)
  CALL MPI_Buffer_detach(buffer_addr=buffer_addr, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Cancel
  USE mpi_f08
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  request = MPI_REQUEST_NULL
  CALL MPI_Cancel(request, ierror)
  CALL MPI_Cancel(request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Get_count
  USE mpi_f08
  TYPE(MPI_Status) :: status
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: count
  INTEGER :: ierror
  status%MPI_SOURCE = 0
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Get_count(status, datatype, count, ierror)
  CALL MPI_Get_count(status=status, datatype=datatype, count=count, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ibsend
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ibsend(buf, count, datatype, dest, tag, comm, request, ierror)
  CALL MPI_Ibsend(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Iprobe
  USE mpi_f08
  INTEGER :: source, tag
  TYPE(MPI_Comm) :: comm
  LOGICAL :: flag
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  source = 5
  tag = 5
  comm = MPI_COMM_NULL
  CALL MPI_Iprobe(source, tag, comm, flag, status, ierror)
  CALL MPI_Iprobe(source=source, tag=tag, comm=comm, flag=flag, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Irecv
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count, source, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  count = 5
  source = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierror)
  CALL MPI_Irecv(buf=buf, count=count, datatype=datatype, source=source, tag=tag, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Irsend
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Irsend(buf, count, datatype, dest, tag, comm, request, ierror)
  CALL MPI_Irsend(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Isend
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierror)
  CALL MPI_Isend(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Issend
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Issend(buf, count, datatype, dest, tag, comm, request, ierror)
  CALL MPI_Issend(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Probe
  USE mpi_f08
  INTEGER :: source, tag
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  source = 5
  tag = 5
  comm = MPI_COMM_NULL
  CALL MPI_Probe(source, tag, comm, status, ierror)
  CALL MPI_Probe(source=source, tag=tag, comm=comm, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Recv
  USE mpi_f08
  REAL, DIMENSION(5) :: buf
  INTEGER :: count, source, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  count = 5
  source = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Recv(buf, count, datatype, source, tag, comm, status, ierror)
  CALL MPI_Recv(buf=buf, count=count, datatype=datatype, source=source, tag=tag, comm=comm, &
                             status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Recv_init
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count, source, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  count = 5
  source = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Recv_init(buf, count, datatype, source, tag, comm, request, ierror)
  CALL MPI_Recv_init(buf=buf, count=count, datatype=datatype, source=source, tag=tag, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Request_free
  USE mpi_f08
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  request = MPI_REQUEST_NULL
  CALL MPI_Request_free(request, ierror)
  CALL MPI_Request_free(request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Request_get_status
  USE mpi_f08
  TYPE(MPI_Request) :: request
  LOGICAL :: flag
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  request = MPI_REQUEST_NULL
  CALL MPI_Request_get_status(request, flag, status, ierror)
  CALL MPI_Request_get_status(request=request, flag=flag, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Rsend
  USE mpi_f08
  REAL, DIMENSION(5) :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Rsend(buf, count, datatype, dest, tag, comm, ierror)
  CALL MPI_Rsend(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Rsend_init
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Rsend_init(buf, count, datatype, dest, tag, comm, request, ierror)
  CALL MPI_Rsend_init(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Send
  USE mpi_f08
  REAL, DIMENSION(5) :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Send(buf, count, datatype, dest, tag, comm, ierror)
  CALL MPI_Send(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Sendrecv
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcount, dest, sendtag, recvcount, source, recvtag
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  dest = 5
  sendtag = 5
  recvcount = 5
  source = 5
  recvtag = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Sendrecv(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, &
                             source, recvtag, comm, status, ierror)
  CALL MPI_Sendrecv(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, dest=dest, &
                             sendtag=sendtag, recvbuf=recvbuf, recvcount=recvcount, recvtype=recvtype, &
                             source=source, recvtag=recvtag, comm=comm, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Sendrecv_replace
  USE mpi_f08
  REAL, DIMENSION(5) :: buf
  INTEGER :: count, dest, sendtag, source, recvtag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  count = 5
  dest = 5
  sendtag = 5
  source = 5
  recvtag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Sendrecv_replace(buf, count, datatype, dest, sendtag, source, recvtag, comm, &
                             status, ierror)
  CALL MPI_Sendrecv_replace(buf=buf, count=count, datatype=datatype, dest=dest, sendtag=sendtag, &
                             source=source, recvtag=recvtag, comm=comm, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Send_init
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Send_init(buf, count, datatype, dest, tag, comm, request, ierror)
  CALL MPI_Send_init(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ssend
  USE mpi_f08
  REAL, DIMENSION(5) :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ssend(buf, count, datatype, dest, tag, comm, ierror)
  CALL MPI_Ssend(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ssend_init
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count, dest, tag
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  buf = 5.0
  count = 5
  dest = 5
  tag = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ssend_init(buf, count, datatype, dest, tag, comm, request, ierror)
  CALL MPI_Ssend_init(buf=buf, count=count, datatype=datatype, dest=dest, tag=tag, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Start
  USE mpi_f08
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  request = MPI_REQUEST_NULL
  CALL MPI_Start(request, ierror)
  CALL MPI_Start(request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Startall
  USE mpi_f08
  INTEGER :: count
  TYPE(MPI_Request) :: array_of_requests(5)
  INTEGER :: ierror
  count = 5
  array_of_requests = MPI_REQUEST_NULL
  CALL MPI_Startall(count, array_of_requests, ierror)
  CALL MPI_Startall(count=count, array_of_requests=array_of_requests, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Test
  USE mpi_f08
  TYPE(MPI_Request) :: request
  LOGICAL :: flag
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  request = MPI_REQUEST_NULL
  CALL MPI_Test(request, flag, status, ierror)
  CALL MPI_Test(request=request, flag=flag, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Testall
  USE mpi_f08
  INTEGER :: count
  TYPE(MPI_Request) :: array_of_requests(5)
  LOGICAL :: flag
  TYPE(MPI_Status) :: array_of_statuses(5)
  INTEGER :: ierror
  count = 5
  array_of_requests = MPI_REQUEST_NULL
  CALL MPI_Testall(count, array_of_requests, flag, array_of_statuses, ierror)
  CALL MPI_Testall(count=count, array_of_requests=array_of_requests, flag=flag, array_of_statuses=array_of_statuses, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Testany
  USE mpi_f08
  INTEGER :: count
  TYPE(MPI_Request) :: array_of_requests(5)
  INTEGER :: index
  LOGICAL :: flag
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  count = 5
  array_of_requests = MPI_REQUEST_NULL
  CALL MPI_Testany(count, array_of_requests, index, flag, status, ierror)
  CALL MPI_Testany(count=count, array_of_requests=array_of_requests, index=index, flag=flag, &
                             status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Testsome
  USE mpi_f08
  INTEGER :: incount
  TYPE(MPI_Request) :: array_of_requests(5)
  INTEGER :: outcount, array_of_indices(5)
  TYPE(MPI_Status) :: array_of_statuses(5)
  INTEGER :: ierror
  incount = 5
  array_of_requests = MPI_REQUEST_NULL
  CALL MPI_Testsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, &
                             ierror)
  CALL MPI_Testsome(incount=incount, array_of_requests=array_of_requests, outcount=outcount, &
                             array_of_indices=array_of_indices, array_of_statuses=array_of_statuses, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Test_cancelled
  USE mpi_f08
  TYPE(MPI_Status) :: status
  LOGICAL :: flag
  INTEGER :: ierror
  status%MPI_SOURCE = 0
  CALL MPI_Test_cancelled(status, flag, ierror)
  CALL MPI_Test_cancelled(status=status, flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Wait
  USE mpi_f08
  TYPE(MPI_Request) :: request
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  request = MPI_REQUEST_NULL
  CALL MPI_Wait(request, status, ierror)
  CALL MPI_Wait(request=request, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Waitall
  USE mpi_f08
  INTEGER :: count
  TYPE(MPI_Request) :: array_of_requests(5)
  TYPE(MPI_Status) :: array_of_statuses(5)
  INTEGER :: ierror
  count = 5
  array_of_requests = MPI_REQUEST_NULL
  CALL MPI_Waitall(count, array_of_requests, array_of_statuses, ierror)
  CALL MPI_Waitall(count=count, array_of_requests=array_of_requests, array_of_statuses=array_of_statuses, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Waitany
  USE mpi_f08
  INTEGER :: count
  TYPE(MPI_Request) :: array_of_requests(5)
  INTEGER :: index
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  count = 5
  array_of_requests = MPI_REQUEST_NULL
  CALL MPI_Waitany(count, array_of_requests, index, status, ierror)
  CALL MPI_Waitany(count=count, array_of_requests=array_of_requests, index=index, status=status, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Waitsome
  USE mpi_f08
  INTEGER :: incount
  TYPE(MPI_Request) :: array_of_requests(5)
  INTEGER :: outcount, array_of_indices(5)
  TYPE(MPI_Status) :: array_of_statuses(5)
  INTEGER :: ierror
  incount = 5
  array_of_requests = MPI_REQUEST_NULL
  CALL MPI_Waitsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, &
                             ierror)
  CALL MPI_Waitsome(incount=incount, array_of_requests=array_of_requests, outcount=outcount, &
                             array_of_indices=array_of_indices, array_of_statuses=array_of_statuses, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Get_address
  USE mpi_f08
  REAL, DIMENSION(5) :: location
  INTEGER(KIND=MPI_ADDRESS_KIND) :: address
  INTEGER :: ierror
  CALL MPI_Get_address(location, address, ierror)
  CALL MPI_Get_address(location=location, address=address, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Get_elements
  USE mpi_f08
  TYPE(MPI_Status) :: status
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: count
  INTEGER :: ierror
  status%MPI_SOURCE = 0
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Get_elements(status, datatype, count, ierror)
  CALL MPI_Get_elements(status=status, datatype=datatype, count=count, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Pack
  USE mpi_f08
  REAL, DIMENSION(5) :: inbuf
  REAL, DIMENSION(5) :: outbuf
  INTEGER :: incount, outsize
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: position
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  inbuf = 5.0
  incount = 5
  outsize = 5
  datatype = MPI_DATATYPE_NULL
  position = 5
  comm = MPI_COMM_NULL
  CALL MPI_Pack(inbuf, incount, datatype, outbuf, outsize, position, comm, ierror)
  CALL MPI_Pack(inbuf=inbuf, incount=incount, datatype=datatype, outbuf=outbuf, outsize=outsize, &
                             position=position, comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Pack_external
  USE mpi_f08
  CHARACTER(LEN=5) :: datarep
  REAL, DIMENSION(5) :: inbuf
  REAL, DIMENSION(5) :: outbuf
  INTEGER :: incount
  TYPE(MPI_Datatype) :: datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: outsize
  INTEGER(KIND=MPI_ADDRESS_KIND) :: position
  INTEGER :: ierror
  datarep = 'abcde'
  inbuf = 5.0
  incount = 5
  datatype = MPI_DATATYPE_NULL
  outsize = 5
  position = 5
  CALL MPI_Pack_external(datarep, inbuf, incount, datatype, outbuf, outsize, position, ierror)
  CALL MPI_Pack_external(datarep=datarep, inbuf=inbuf, incount=incount, datatype=datatype, &
                             outbuf=outbuf, outsize=outsize, position=position, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Pack_external_size
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: incount
  CHARACTER(LEN=5) :: datarep
  INTEGER(KIND=MPI_ADDRESS_KIND) :: size
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  incount = 5
  datarep = 'abcde'
  CALL MPI_Pack_external_size(datarep, incount, datatype, size, ierror)
  CALL MPI_Pack_external_size(datarep=datarep, incount=incount, datatype=datatype, size=size, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Pack_size
  USE mpi_f08
  INTEGER :: incount
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  INTEGER :: size
  INTEGER :: ierror
  incount = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Pack_size(incount, datatype, comm, size, ierror)
  CALL MPI_Pack_size(incount=incount, datatype=datatype, comm=comm, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_commit
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Type_commit(datatype, ierror)
  CALL MPI_Type_commit(datatype=datatype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_contiguous
  USE mpi_f08
  INTEGER :: count
  TYPE(MPI_Datatype) :: oldtype
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  count = 5
  oldtype = MPI_DATATYPE_NULL
  CALL MPI_Type_contiguous(count, oldtype, newtype, ierror)
  CALL MPI_Type_contiguous(count=count, oldtype=oldtype, newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_create_darray
  USE mpi_f08
  INTEGER :: size, rank, ndims, array_of_gsizes(5), array_of_distribs(5), array_of_dargs(5), &
                             array_of_psizes(5), order
  TYPE(MPI_Datatype) :: oldtype
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  size = 5
  rank = 5
  ndims = 5
  array_of_gsizes = 5
  array_of_distribs = 5
  array_of_dargs = 5
  array_of_psizes = 5
  order = 5
  oldtype = MPI_DATATYPE_NULL
  CALL MPI_Type_create_darray(size, rank, ndims, array_of_gsizes, array_of_distribs, array_of_dargs, &
                             array_of_psizes, order, oldtype, newtype, ierror)
  CALL MPI_Type_create_darray(size=size, rank=rank, ndims=ndims, array_of_gsizes=array_of_gsizes, &
                             array_of_distribs=array_of_distribs, array_of_dargs=array_of_dargs, &
                             array_of_psizes=array_of_psizes, order=order, oldtype=oldtype, &
                             newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_create_hindexed
  USE mpi_f08
  INTEGER :: count, array_of_blocklengths(5)
  INTEGER(KIND=MPI_ADDRESS_KIND) :: array_of_displacements(5)
  TYPE(MPI_Datatype) :: oldtype
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  count = 5
  array_of_blocklengths = 5
  array_of_displacements = 5
  oldtype = MPI_DATATYPE_NULL
  CALL MPI_Type_create_hindexed(count, array_of_blocklengths, array_of_displacements, oldtype, &
                             newtype, ierror)
  CALL MPI_Type_create_hindexed(count=count, array_of_blocklengths=array_of_blocklengths, &
                             array_of_displacements=array_of_displacements, oldtype=oldtype, &
                             newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_create_hvector
  USE mpi_f08
  INTEGER :: count, blocklength
  INTEGER(KIND=MPI_ADDRESS_KIND) :: stride
  TYPE(MPI_Datatype) :: oldtype
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  count = 5
  blocklength = 5
  stride = 5
  oldtype = MPI_DATATYPE_NULL
  CALL MPI_Type_create_hvector(count, blocklength, stride, oldtype, newtype, ierror)
  CALL MPI_Type_create_hvector(count=count, blocklength=blocklength, stride=stride, oldtype=oldtype, &
                             newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_create_indexed_block
  USE mpi_f08
  INTEGER :: count, blocklength, array_of_displacements(5)
  TYPE(MPI_Datatype) :: oldtype
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  count = 5
  blocklength = 5
  array_of_displacements = 5
  oldtype = MPI_DATATYPE_NULL
  CALL MPI_Type_create_indexed_block(count, blocklength, array_of_displacements, oldtype, &
                             newtype, ierror)
  CALL MPI_Type_create_indexed_block(count=count, blocklength=blocklength, array_of_displacements=array_of_displacements, &
                             oldtype=oldtype, newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_create_resized
  USE mpi_f08
  INTEGER(KIND=MPI_ADDRESS_KIND) :: lb, extent
  TYPE(MPI_Datatype) :: oldtype
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  lb = 5
  extent = 5
  oldtype = MPI_DATATYPE_NULL
  CALL MPI_Type_create_resized(oldtype, lb, extent, newtype, ierror)
  CALL MPI_Type_create_resized(oldtype=oldtype, lb=lb, extent=extent, newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_create_struct
  USE mpi_f08
  INTEGER :: count, array_of_blocklengths(5)
  INTEGER(KIND=MPI_ADDRESS_KIND) :: array_of_displacements(5)
  TYPE(MPI_Datatype) :: array_of_types(5)
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  count = 5
  array_of_blocklengths = 5
  array_of_displacements = 5
  array_of_types = MPI_DATATYPE_NULL
  CALL MPI_Type_create_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, &
                             newtype, ierror)
  CALL MPI_Type_create_struct(count=count, array_of_blocklengths=array_of_blocklengths, &
                             array_of_displacements=array_of_displacements, array_of_types=array_of_types, &
                             newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_create_subarray
  USE mpi_f08
  INTEGER :: ndims, array_of_sizes(5), array_of_subsizes(5), array_of_starts(5), order
  TYPE(MPI_Datatype) :: oldtype
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  ndims = 5
  array_of_sizes = 5
  array_of_subsizes = 5
  array_of_starts = 5
  order = 5
  oldtype = MPI_DATATYPE_NULL
  CALL MPI_Type_create_subarray(ndims, array_of_sizes, array_of_subsizes, array_of_starts, &
                             order, oldtype, newtype, ierror)
  CALL MPI_Type_create_subarray(ndims=ndims, array_of_sizes=array_of_sizes, array_of_subsizes=array_of_subsizes, &
                             array_of_starts=array_of_starts, order=order, oldtype=oldtype, &
                             newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_dup
  USE mpi_f08
  TYPE(MPI_Datatype) :: oldtype
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  oldtype = MPI_DATATYPE_NULL
  CALL MPI_Type_dup(oldtype, newtype, ierror)
  CALL MPI_Type_dup(oldtype=oldtype, newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_free
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Type_free(datatype, ierror)
  CALL MPI_Type_free(datatype=datatype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_get_contents
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: max_integers, max_addresses, max_datatypes
  INTEGER :: array_of_integers(5)
  INTEGER(KIND=MPI_ADDRESS_KIND) :: array_of_addresses(5)
  TYPE(MPI_Datatype) :: array_of_datatypes(5)
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  max_integers = 5
  max_addresses = 5
  max_datatypes = 5
  CALL MPI_Type_get_contents(datatype, max_integers, max_addresses, max_datatypes, array_of_integers, &
                             array_of_addresses, array_of_datatypes, ierror)
  CALL MPI_Type_get_contents(datatype=datatype, max_integers=max_integers, max_addresses=max_addresses, &
                             max_datatypes=max_datatypes, array_of_integers=array_of_integers, &
                             array_of_addresses=array_of_addresses, array_of_datatypes=array_of_datatypes, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_get_envelope
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: num_integers, num_addresses, num_datatypes, combiner
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Type_get_envelope(datatype, num_integers, num_addresses, num_datatypes, combiner, &
                             ierror)
  CALL MPI_Type_get_envelope(datatype=datatype, num_integers=num_integers, num_addresses=num_addresses, &
                             num_datatypes=num_datatypes, combiner=combiner, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_get_extent
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: lb, extent
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Type_get_extent(datatype, lb, extent, ierror)
  CALL MPI_Type_get_extent(datatype=datatype, lb=lb, extent=extent, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_get_true_extent
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: true_lb, true_extent
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Type_get_true_extent(datatype, true_lb, true_extent, ierror)
  CALL MPI_Type_get_true_extent(datatype=datatype, true_lb=true_lb, true_extent=true_extent, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_indexed
  USE mpi_f08
  INTEGER :: count, array_of_blocklengths(5), array_of_displacements(5)
  TYPE(MPI_Datatype) :: oldtype
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  count = 5
  array_of_blocklengths = 5
  array_of_displacements = 5
  oldtype = MPI_DATATYPE_NULL
  CALL MPI_Type_indexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, &
                             ierror)
  CALL MPI_Type_indexed(count=count, array_of_blocklengths=array_of_blocklengths, array_of_displacements=array_of_displacements, &
                             oldtype=oldtype, newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_size
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: size
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Type_size(datatype, size, ierror)
  CALL MPI_Type_size(datatype=datatype, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_vector
  USE mpi_f08
  INTEGER :: count, blocklength, stride
  TYPE(MPI_Datatype) :: oldtype
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  count = 5
  blocklength = 5
  stride = 5
  oldtype = MPI_DATATYPE_NULL
  CALL MPI_Type_vector(count, blocklength, stride, oldtype, newtype, ierror)
  CALL MPI_Type_vector(count=count, blocklength=blocklength, stride=stride, oldtype=oldtype, &
                             newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Unpack
  USE mpi_f08
  REAL, DIMENSION(5) :: inbuf
  REAL, DIMENSION(5) :: outbuf
  INTEGER :: insize, outcount
  INTEGER :: position
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  inbuf = 5.0
  insize = 5
  outcount = 5
  position = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Unpack(inbuf, insize, position, outbuf, outcount, datatype, comm, ierror)
  CALL MPI_Unpack(inbuf=inbuf, insize=insize, position=position, outbuf=outbuf, outcount=outcount, &
                             datatype=datatype, comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Unpack_external
  USE mpi_f08
  CHARACTER(LEN=5) :: datarep
  REAL, DIMENSION(5) :: inbuf
  REAL, DIMENSION(5) :: outbuf
  INTEGER(KIND=MPI_ADDRESS_KIND) :: insize
  INTEGER(KIND=MPI_ADDRESS_KIND) :: position
  INTEGER :: outcount
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: ierror
  datarep = 'abcde'
  inbuf = 5.0
  insize = 5
  position = 5
  outcount = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Unpack_external(datarep, inbuf, insize, position, outbuf, outcount, datatype, &
                             ierror)
  CALL MPI_Unpack_external(datarep=datarep, inbuf=inbuf, insize=insize, position=position, &
                             outbuf=outbuf, outcount=outcount, datatype=datatype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Allgather
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcount, recvcount
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror)
  CALL MPI_Allgather(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcount=recvcount, recvtype=recvtype, comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Allgatherv
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcount, recvcounts(5), displs(5)
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcounts = 5
  displs = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Allgatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, &
                             comm, ierror)
  CALL MPI_Allgatherv(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcounts=recvcounts, displs=displs, recvtype=recvtype, comm=comm, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Allreduce
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierror)
  CALL MPI_Allreduce(sendbuf=sendbuf, recvbuf=recvbuf, count=count, datatype=datatype, op=op, &
                             comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Alltoall
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcount, recvcount
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror)
  CALL MPI_Alltoall(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcount=recvcount, recvtype=recvtype, comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Alltoallv
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcounts(5), sdispls(5), recvcounts(5), rdispls(5)
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcounts = 5
  sdispls = 5
  recvcounts = 5
  rdispls = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, &
                             recvtype, comm, ierror)
  CALL MPI_Alltoallv(sendbuf=sendbuf, sendcounts=sendcounts, sdispls=sdispls, sendtype=sendtype, &
                             recvbuf=recvbuf, recvcounts=recvcounts, rdispls=rdispls, recvtype=recvtype, &
                             comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Alltoallw
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcounts(5), sdispls(5), recvcounts(5), rdispls(5)
  TYPE(MPI_Datatype) :: sendtypes(5)
  TYPE(MPI_Datatype) :: recvtypes(5)
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcounts = 5
  sdispls = 5
  recvcounts = 5
  rdispls = 5
  sendtypes = MPI_DATATYPE_NULL
  recvtypes = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, &
                             recvtypes, comm, ierror)
  CALL MPI_Alltoallw(sendbuf=sendbuf, sendcounts=sendcounts, sdispls=sdispls, sendtypes=sendtypes, &
                             recvbuf=recvbuf, recvcounts=recvcounts, rdispls=rdispls, recvtypes=recvtypes, &
                             comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Barrier
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Barrier(comm, ierror)
  CALL MPI_Barrier(comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Bcast
  USE mpi_f08
  REAL, DIMENSION(5) :: buffer
  INTEGER :: count, root
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  count = 5
  root = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Bcast(buffer, count, datatype, root, comm, ierror)
  CALL MPI_Bcast(buffer=buffer, count=count, datatype=datatype, root=root, comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Exscan
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Exscan(sendbuf, recvbuf, count, datatype, op, comm, ierror)
  CALL MPI_Exscan(sendbuf=sendbuf, recvbuf=recvbuf, count=count, datatype=datatype, op=op, &
                             comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Gather
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcount, recvcount, root
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  root = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Gather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, &
                             ierror)
  CALL MPI_Gather(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcount=recvcount, recvtype=recvtype, root=root, comm=comm, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Gatherv
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcount, recvcounts(5), displs(5), root
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcounts = 5
  displs = 5
  root = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Gatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, &
                             root, comm, ierror)
  CALL MPI_Gatherv(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcounts=recvcounts, displs=displs, recvtype=recvtype, root=root, &
                             comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Op_commutative
  USE mpi_f08
  TYPE(MPI_Op) :: op
  LOGICAL :: commute
  INTEGER :: ierror
  op = MPI_OP_NULL
  CALL MPI_Op_commutative(op, commute, ierror)
  CALL MPI_Op_commutative(op=op, commute=commute, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Op_create
  USE mpi_f08
  USE test_callback_routines
  ! PROCEDURE(MPI_User_function) :: user_fn
  LOGICAL :: commute
  TYPE(MPI_Op) :: op
  INTEGER :: ierror
  commute = .TRUE.
  CALL MPI_Op_create(user_fn, commute, op, ierror)
  CALL MPI_Op_create(user_fn=user_fn, commute=commute, op=op, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Op_free
  USE mpi_f08
  TYPE(MPI_Op) :: op
  INTEGER :: ierror
  op = MPI_OP_NULL
  CALL MPI_Op_free(op, ierror)
  CALL MPI_Op_free(op=op, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Reduce
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: count, root
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  count = 5
  root = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierror)
  CALL MPI_Reduce(sendbuf=sendbuf, recvbuf=recvbuf, count=count, datatype=datatype, op=op, &
                             root=root, comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Reduce_local
  USE mpi_f08
  REAL, DIMENSION(5) :: inbuf
  REAL, DIMENSION(5) :: inoutbuf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  INTEGER :: ierror
  inbuf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  CALL MPI_Reduce_local(inbuf, inoutbuf, count, datatype, op, ierror)
  CALL MPI_Reduce_local(inbuf=inbuf, inoutbuf=inoutbuf, count=count, datatype=datatype, &
                             op=op, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Reduce_scatter
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: recvcounts(5)
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  recvcounts = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Reduce_scatter(sendbuf, recvbuf, recvcounts, datatype, op, comm, ierror)
  CALL MPI_Reduce_scatter(sendbuf=sendbuf, recvbuf=recvbuf, recvcounts=recvcounts, datatype=datatype, &
                             op=op, comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Reduce_scatter_block
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: recvcount
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  recvcount = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Reduce_scatter_block(sendbuf, recvbuf, recvcount, datatype, op, comm, ierror)
  CALL MPI_Reduce_scatter_block(sendbuf=sendbuf, recvbuf=recvbuf, recvcount=recvcount, datatype=datatype, &
                             op=op, comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Scan
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Scan(sendbuf, recvbuf, count, datatype, op, comm, ierror)
  CALL MPI_Scan(sendbuf=sendbuf, recvbuf=recvbuf, count=count, datatype=datatype, op=op, &
                             comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Scatter
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcount, recvcount, root
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  root = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Scatter(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, &
                             ierror)
  CALL MPI_Scatter(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcount=recvcount, recvtype=recvtype, root=root, comm=comm, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Scatterv
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcounts(5), displs(5), recvcount, root
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcounts = 5
  displs = 5
  recvcount = 5
  root = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Scatterv(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, &
                             root, comm, ierror)
  CALL MPI_Scatterv(sendbuf=sendbuf, sendcounts=sendcounts, displs=displs, sendtype=sendtype, &
                             recvbuf=recvbuf, recvcount=recvcount, recvtype=recvtype, root=root, &
                             comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_compare
  USE mpi_f08
  TYPE(MPI_Comm) :: comm1, comm2
  INTEGER :: result
  INTEGER :: ierror
  comm1 = MPI_COMM_NULL
  comm2 = MPI_COMM_NULL
  CALL MPI_Comm_compare(comm1, comm2, result, ierror)
  CALL MPI_Comm_compare(comm1=comm1, comm2=comm2, result=result, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_create
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Group) :: group
  TYPE(MPI_Comm) :: newcomm
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  group = MPI_GROUP_NULL
  CALL MPI_Comm_create(comm, group, newcomm, ierror)
  CALL MPI_Comm_create(comm=comm, group=group, newcomm=newcomm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_create_keyval
  USE mpi_f08
  USE test_callback_routines
  ! PROCEDURE(MPI_Comm_copy_attr_function) :: comm_copy_attr_fn
  ! PROCEDURE(MPI_Comm_delete_attr_function) :: comm_delete_attr_fn
  INTEGER :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  INTEGER :: ierror
  extra_state = 5
  CALL MPI_Comm_create_keyval(comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, &
                             ierror)
  CALL MPI_Comm_create_keyval(comm_copy_attr_fn=comm_copy_attr_fn, comm_delete_attr_fn=comm_delete_attr_fn, &
                             comm_keyval=comm_keyval, extra_state=extra_state, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_delete_attr
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: comm_keyval
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  comm_keyval = 5
  CALL MPI_Comm_delete_attr(comm, comm_keyval, ierror)
  CALL MPI_Comm_delete_attr(comm=comm, comm_keyval=comm_keyval, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_dup
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Comm) :: newcomm
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Comm_dup(comm, newcomm, ierror)
  CALL MPI_Comm_dup(comm=comm, newcomm=newcomm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_COMM_DUP_FN
  USE mpi_f08
  TYPE(MPI_Comm) :: oldcomm
  INTEGER :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val_out
  LOGICAL :: flag
  INTEGER :: ierror
  oldcomm = MPI_COMM_NULL
  comm_keyval = 5
  extra_state = 5
  attribute_val_in = 5
  CALL MPI_COMM_DUP_FN(oldcomm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, &
                             flag, ierror)
  CALL MPI_COMM_DUP_FN(oldcomm=oldcomm, comm_keyval=comm_keyval, extra_state=extra_state, &
                             attribute_val_in=attribute_val_in, attribute_val_out=attribute_val_out, &
                             flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_free
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Comm_free(comm, ierror)
  CALL MPI_Comm_free(comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_free_keyval
  USE mpi_f08
  INTEGER :: comm_keyval
  INTEGER :: ierror
  comm_keyval = 5
  CALL MPI_Comm_free_keyval(comm_keyval, ierror)
  CALL MPI_Comm_free_keyval(comm_keyval=comm_keyval, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_get_attr
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val
  LOGICAL :: flag
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  comm_keyval = 5
  CALL MPI_Comm_get_attr(comm, comm_keyval, attribute_val, flag, ierror)
  CALL MPI_Comm_get_attr(comm=comm, comm_keyval=comm_keyval, attribute_val=attribute_val, &
                             flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_get_name
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  CHARACTER(LEN=MPI_MAX_OBJECT_NAME) :: comm_name
  INTEGER :: resultlen
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Comm_get_name(comm, comm_name, resultlen, ierror)
  CALL MPI_Comm_get_name(comm=comm, comm_name=comm_name, resultlen=resultlen, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_group
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Group) :: group
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Comm_group(comm, group, ierror)
  CALL MPI_Comm_group(comm=comm, group=group, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_COMM_NULL_COPY_FN
  USE mpi_f08
  TYPE(MPI_Comm) :: oldcomm
  INTEGER :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val_out
  LOGICAL :: flag
  INTEGER :: ierror
  oldcomm = MPI_COMM_NULL
  comm_keyval = 5
  extra_state = 5
  attribute_val_in = 5
  CALL MPI_COMM_NULL_COPY_FN(oldcomm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, &
                             flag, ierror)
  CALL MPI_COMM_NULL_COPY_FN(oldcomm=oldcomm, comm_keyval=comm_keyval, extra_state=extra_state, &
                             attribute_val_in=attribute_val_in, attribute_val_out=attribute_val_out, &
                             flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_COMM_NULL_DELETE_FN
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  comm_keyval = 5
  attribute_val = 5
  extra_state = 5
  CALL MPI_COMM_NULL_DELETE_FN(comm, comm_keyval, attribute_val, extra_state, ierror)
  CALL MPI_COMM_NULL_DELETE_FN(comm=comm, comm_keyval=comm_keyval, attribute_val=attribute_val, &
                             extra_state=extra_state, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_rank
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: rank
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Comm_rank(comm, rank, ierror)
  CALL MPI_Comm_rank(comm=comm, rank=rank, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_remote_group
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Group) :: group
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Comm_remote_group(comm, group, ierror)
  CALL MPI_Comm_remote_group(comm=comm, group=group, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_remote_size
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: size
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Comm_remote_size(comm, size, ierror)
  CALL MPI_Comm_remote_size(comm=comm, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_set_attr
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  comm_keyval = 5
  attribute_val = 5
  CALL MPI_Comm_set_attr(comm, comm_keyval, attribute_val, ierror)
  CALL MPI_Comm_set_attr(comm=comm, comm_keyval=comm_keyval, attribute_val=attribute_val, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_set_name
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  CHARACTER(LEN=5) :: comm_name
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  comm_name = 'abcde'
  CALL MPI_Comm_set_name(comm, comm_name, ierror)
  CALL MPI_Comm_set_name(comm=comm, comm_name=comm_name, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_size
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: size
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Comm_size(comm, size, ierror)
  CALL MPI_Comm_size(comm=comm, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_split
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: color, key
  TYPE(MPI_Comm) :: newcomm
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  color = 5
  key = 5
  CALL MPI_Comm_split(comm, color, key, newcomm, ierror)
  CALL MPI_Comm_split(comm=comm, color=color, key=key, newcomm=newcomm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_test_inter
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  LOGICAL :: flag
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Comm_test_inter(comm, flag, ierror)
  CALL MPI_Comm_test_inter(comm=comm, flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_compare
  USE mpi_f08
  TYPE(MPI_Group) :: group1, group2
  INTEGER :: result
  INTEGER :: ierror
  group1 = MPI_GROUP_NULL
  group2 = MPI_GROUP_NULL
  CALL MPI_Group_compare(group1, group2, result, ierror)
  CALL MPI_Group_compare(group1=group1, group2=group2, result=result, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_difference
  USE mpi_f08
  TYPE(MPI_Group) :: group1, group2
  TYPE(MPI_Group) :: newgroup
  INTEGER :: ierror
  group1 = MPI_GROUP_NULL
  group2 = MPI_GROUP_NULL
  CALL MPI_Group_difference(group1, group2, newgroup, ierror)
  CALL MPI_Group_difference(group1=group1, group2=group2, newgroup=newgroup, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_excl
  USE mpi_f08
  TYPE(MPI_Group) :: group
  INTEGER :: n, ranks(5)
  TYPE(MPI_Group) :: newgroup
  INTEGER :: ierror
  group = MPI_GROUP_NULL
  n = 5
  ranks = 5
  CALL MPI_Group_excl(group, n, ranks, newgroup, ierror)
  CALL MPI_Group_excl(group=group, n=n, ranks=ranks, newgroup=newgroup, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_free
  USE mpi_f08
  TYPE(MPI_Group) :: group
  INTEGER :: ierror
  group = MPI_GROUP_NULL
  CALL MPI_Group_free(group, ierror)
  CALL MPI_Group_free(group=group, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_incl
  USE mpi_f08
  TYPE(MPI_Group) :: group
  INTEGER :: n, ranks(5)
  TYPE(MPI_Group) :: newgroup
  INTEGER :: ierror
  group = MPI_GROUP_NULL
  n = 5
  ranks = 5
  CALL MPI_Group_incl(group, n, ranks, newgroup, ierror)
  CALL MPI_Group_incl(group=group, n=n, ranks=ranks, newgroup=newgroup, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_intersection
  USE mpi_f08
  TYPE(MPI_Group) :: group1, group2
  TYPE(MPI_Group) :: newgroup
  INTEGER :: ierror
  group1 = MPI_GROUP_NULL
  group2 = MPI_GROUP_NULL
  CALL MPI_Group_intersection(group1, group2, newgroup, ierror)
  CALL MPI_Group_intersection(group1=group1, group2=group2, newgroup=newgroup, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_range_excl
  USE mpi_f08
  TYPE(MPI_Group) :: group
  INTEGER :: n, ranges(3,5)
  TYPE(MPI_Group) :: newgroup
  INTEGER :: ierror
  group = MPI_GROUP_NULL
  n = 5
  ranges = 5
  CALL MPI_Group_range_excl(group, n, ranges, newgroup, ierror)
  CALL MPI_Group_range_excl(group=group, n=n, ranges=ranges, newgroup=newgroup, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_range_incl
  USE mpi_f08
  TYPE(MPI_Group) :: group
  INTEGER :: n, ranges(3,5)
  TYPE(MPI_Group) :: newgroup
  INTEGER :: ierror
  group = MPI_GROUP_NULL
  n = 5
  ranges = 5
  CALL MPI_Group_range_incl(group, n, ranges, newgroup, ierror)
  CALL MPI_Group_range_incl(group=group, n=n, ranges=ranges, newgroup=newgroup, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_rank
  USE mpi_f08
  TYPE(MPI_Group) :: group
  INTEGER :: rank
  INTEGER :: ierror
  group = MPI_GROUP_NULL
  CALL MPI_Group_rank(group, rank, ierror)
  CALL MPI_Group_rank(group=group, rank=rank, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_size
  USE mpi_f08
  TYPE(MPI_Group) :: group
  INTEGER :: size
  INTEGER :: ierror
  group = MPI_GROUP_NULL
  CALL MPI_Group_size(group, size, ierror)
  CALL MPI_Group_size(group=group, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_translate_ranks
  USE mpi_f08
  TYPE(MPI_Group) :: group1, group2
  INTEGER :: n, ranks1(5)
  INTEGER :: ranks2(5)
  INTEGER :: ierror
  group1 = MPI_GROUP_NULL
  group2 = MPI_GROUP_NULL
  n = 5
  ranks1 = 5
  CALL MPI_Group_translate_ranks(group1, n, ranks1, group2, ranks2, ierror)
  CALL MPI_Group_translate_ranks(group1=group1, n=n, ranks1=ranks1, group2=group2, ranks2=ranks2, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Group_union
  USE mpi_f08
  TYPE(MPI_Group) :: group1, group2
  TYPE(MPI_Group) :: newgroup
  INTEGER :: ierror
  group1 = MPI_GROUP_NULL
  group2 = MPI_GROUP_NULL
  CALL MPI_Group_union(group1, group2, newgroup, ierror)
  CALL MPI_Group_union(group1=group1, group2=group2, newgroup=newgroup, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Intercomm_create
  USE mpi_f08
  TYPE(MPI_Comm) :: local_comm, peer_comm
  INTEGER :: local_leader, remote_leader, tag
  TYPE(MPI_Comm) :: newintercomm
  INTEGER :: ierror
  local_comm = MPI_COMM_NULL
  peer_comm = MPI_COMM_NULL
  local_leader = 5
  remote_leader = 5
  tag = 5
  CALL MPI_Intercomm_create(local_comm, local_leader, peer_comm, remote_leader, tag, newintercomm, &
                             ierror)
  CALL MPI_Intercomm_create(local_comm=local_comm, local_leader=local_leader, peer_comm=peer_comm, &
                             remote_leader=remote_leader, tag=tag, newintercomm=newintercomm, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Intercomm_merge
  USE mpi_f08
  TYPE(MPI_Comm) :: intercomm
  LOGICAL :: high
  TYPE(MPI_Comm) :: newintracomm
  INTEGER :: ierror
  intercomm = MPI_COMM_NULL
  high = .TRUE.
  CALL MPI_Intercomm_merge(intercomm, high, newintracomm, ierror)
  CALL MPI_Intercomm_merge(intercomm=intercomm, high=high, newintracomm=newintracomm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_create_keyval
  USE mpi_f08
  USE test_callback_routines
  ! PROCEDURE(MPI_Type_copy_attr_function) :: type_copy_attr_fn
  ! PROCEDURE(MPI_Type_delete_attr_function) :: type_delete_attr_fn
  INTEGER :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  INTEGER :: ierror
  extra_state = 5
  CALL MPI_Type_create_keyval(type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state, &
                             ierror)
  CALL MPI_Type_create_keyval(type_copy_attr_fn=type_copy_attr_fn, type_delete_attr_fn=type_delete_attr_fn, &
                             type_keyval=type_keyval, extra_state=extra_state, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_delete_attr
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: type_keyval
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  type_keyval = 5
  CALL MPI_Type_delete_attr(datatype, type_keyval, ierror)
  CALL MPI_Type_delete_attr(datatype=datatype, type_keyval=type_keyval, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_TYPE_DUP_FN
  USE mpi_f08
  TYPE(MPI_Datatype) :: oldtype
  INTEGER :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val_out
  LOGICAL :: flag
  INTEGER :: ierror
  oldtype = MPI_DATATYPE_NULL
  type_keyval = 5
  extra_state = 5
  attribute_val_in = 5
  CALL MPI_TYPE_DUP_FN(oldtype, type_keyval, extra_state, attribute_val_in, attribute_val_out, &
                             flag, ierror)
  CALL MPI_TYPE_DUP_FN(oldtype=oldtype, type_keyval=type_keyval, extra_state=extra_state, &
                             attribute_val_in=attribute_val_in, attribute_val_out=attribute_val_out, &
                             flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_free_keyval
  USE mpi_f08
  INTEGER :: type_keyval
  INTEGER :: ierror
  type_keyval = 5
  CALL MPI_Type_free_keyval(type_keyval, ierror)
  CALL MPI_Type_free_keyval(type_keyval=type_keyval, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_get_attr
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val
  LOGICAL :: flag
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  type_keyval = 5
  CALL MPI_Type_get_attr(datatype, type_keyval, attribute_val, flag, ierror)
  CALL MPI_Type_get_attr(datatype=datatype, type_keyval=type_keyval, attribute_val=attribute_val, &
                             flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_get_name
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  CHARACTER(LEN=MPI_MAX_OBJECT_NAME) :: type_name
  INTEGER :: resultlen
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Type_get_name(datatype, type_name, resultlen, ierror)
  CALL MPI_Type_get_name(datatype=datatype, type_name=type_name, resultlen=resultlen, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_TYPE_NULL_COPY_FN
  USE mpi_f08
  TYPE(MPI_Datatype) :: oldtype
  INTEGER :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val_out
  LOGICAL :: flag
  INTEGER :: ierror
  oldtype = MPI_DATATYPE_NULL
  type_keyval = 5
  extra_state = 5
  attribute_val_in = 5
  CALL MPI_TYPE_NULL_COPY_FN(oldtype, type_keyval, extra_state, attribute_val_in, attribute_val_out, &
                             flag, ierror)
  CALL MPI_TYPE_NULL_COPY_FN(oldtype=oldtype, type_keyval=type_keyval, extra_state=extra_state, &
                             attribute_val_in=attribute_val_in, attribute_val_out=attribute_val_out, &
                             flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_TYPE_NULL_DELETE_FN
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  type_keyval = 5
  attribute_val = 5
  extra_state = 5
  CALL MPI_TYPE_NULL_DELETE_FN(datatype, type_keyval, attribute_val, extra_state, ierror)
  CALL MPI_TYPE_NULL_DELETE_FN(datatype=datatype, type_keyval=type_keyval, attribute_val=attribute_val, &
                             extra_state=extra_state, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_set_attr
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  type_keyval = 5
  attribute_val = 5
  CALL MPI_Type_set_attr(datatype, type_keyval, attribute_val, ierror)
  CALL MPI_Type_set_attr(datatype=datatype, type_keyval=type_keyval, attribute_val=attribute_val, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_set_name
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  CHARACTER(LEN=5) :: type_name
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  type_name = 'abcde'
  CALL MPI_Type_set_name(datatype, type_name, ierror)
  CALL MPI_Type_set_name(datatype=datatype, type_name=type_name, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_create_keyval
  USE mpi_f08
  USE test_callback_routines
  ! PROCEDURE(MPI_Win_copy_attr_function) :: win_copy_attr_fn
  ! PROCEDURE(MPI_Win_delete_attr_function) :: win_delete_attr_fn
  INTEGER :: win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  INTEGER :: ierror
  extra_state = 5
  CALL MPI_Win_create_keyval(win_copy_attr_fn, win_delete_attr_fn, win_keyval, extra_state, &
                             ierror)
  CALL MPI_Win_create_keyval(win_copy_attr_fn=win_copy_attr_fn, win_delete_attr_fn=win_delete_attr_fn, &
                             win_keyval=win_keyval, extra_state=extra_state, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_delete_attr
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: win_keyval
  INTEGER :: ierror
  win = MPI_WIN_NULL
  win_keyval = 5
  CALL MPI_Win_delete_attr(win, win_keyval, ierror)
  CALL MPI_Win_delete_attr(win=win, win_keyval=win_keyval, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_WIN_DUP_FN
  USE mpi_f08
  INTEGER :: oldwin, win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val_out
  LOGICAL :: flag
  INTEGER :: ierror
  oldwin = 5
  win_keyval = 5
  extra_state = 5
  attribute_val_in = 5
  CALL MPI_WIN_DUP_FN(oldwin, win_keyval, extra_state, attribute_val_in, attribute_val_out, &
                             flag, ierror)
  CALL MPI_WIN_DUP_FN(oldwin=oldwin, win_keyval=win_keyval, extra_state=extra_state, attribute_val_in=attribute_val_in, &
                             attribute_val_out=attribute_val_out, flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_free_keyval
  USE mpi_f08
  INTEGER :: win_keyval
  INTEGER :: ierror
  win_keyval = 5
  CALL MPI_Win_free_keyval(win_keyval, ierror)
  CALL MPI_Win_free_keyval(win_keyval=win_keyval, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_get_attr
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val
  LOGICAL :: flag
  INTEGER :: ierror
  win = MPI_WIN_NULL
  win_keyval = 5
  CALL MPI_Win_get_attr(win, win_keyval, attribute_val, flag, ierror)
  CALL MPI_Win_get_attr(win=win, win_keyval=win_keyval, attribute_val=attribute_val, flag=flag, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_get_name
  USE mpi_f08
  TYPE(MPI_Win) :: win
  CHARACTER(LEN=MPI_MAX_OBJECT_NAME) :: win_name
  INTEGER :: resultlen
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_get_name(win, win_name, resultlen, ierror)
  CALL MPI_Win_get_name(win=win, win_name=win_name, resultlen=resultlen, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_WIN_NULL_COPY_FN
  USE mpi_f08
  INTEGER :: oldwin, win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val_out
  LOGICAL :: flag
  INTEGER :: ierror
  oldwin = 5
  win_keyval = 5
  extra_state = 5
  attribute_val_in = 5
  CALL MPI_WIN_NULL_COPY_FN(oldwin, win_keyval, extra_state, attribute_val_in, attribute_val_out, &
                             flag, ierror)
  CALL MPI_WIN_NULL_COPY_FN(oldwin=oldwin, win_keyval=win_keyval, extra_state=extra_state, &
                             attribute_val_in=attribute_val_in, attribute_val_out=attribute_val_out, &
                             flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_WIN_NULL_DELETE_FN
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  INTEGER :: ierror
  win = MPI_WIN_NULL
  win_keyval = 5
  attribute_val = 5
  extra_state = 5
  CALL MPI_WIN_NULL_DELETE_FN(win, win_keyval, attribute_val, extra_state, ierror)
  CALL MPI_WIN_NULL_DELETE_FN(win=win, win_keyval=win_keyval, attribute_val=attribute_val, &
                             extra_state=extra_state, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_set_attr
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val
  INTEGER :: ierror
  win = MPI_WIN_NULL
  win_keyval = 5
  attribute_val = 5
  CALL MPI_Win_set_attr(win, win_keyval, attribute_val, ierror)
  CALL MPI_Win_set_attr(win=win, win_keyval=win_keyval, attribute_val=attribute_val, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_set_name
  USE mpi_f08
  TYPE(MPI_Win) :: win
  CHARACTER(LEN=5) :: win_name
  INTEGER :: ierror
  win = MPI_WIN_NULL
  win_name = 'abcde'
  CALL MPI_Win_set_name(win, win_name, ierror)
  CALL MPI_Win_set_name(win=win, win_name=win_name, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Cartdim_get
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: ndims
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Cartdim_get(comm, ndims, ierror)
  CALL MPI_Cartdim_get(comm=comm, ndims=ndims, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Cart_coords
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: rank, maxdims
  INTEGER :: coords(5)
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  rank = 5
  maxdims = 5
  CALL MPI_Cart_coords(comm, rank, maxdims, coords, ierror)
  CALL MPI_Cart_coords(comm=comm, rank=rank, maxdims=maxdims, coords=coords, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Cart_create
  USE mpi_f08
  TYPE(MPI_Comm) :: comm_old
  INTEGER :: ndims, dims(5)
  LOGICAL :: periods(5), reorder
  TYPE(MPI_Comm) :: comm_cart
  INTEGER :: ierror
  comm_old = MPI_COMM_NULL
  ndims = 5
  dims = 5
  periods = .TRUE.
  reorder = .TRUE.
  CALL MPI_Cart_create(comm_old, ndims, dims, periods, reorder, comm_cart, ierror)
  CALL MPI_Cart_create(comm_old=comm_old, ndims=ndims, dims=dims, periods=periods, reorder=reorder, &
                             comm_cart=comm_cart, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Cart_get
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: maxdims
  INTEGER :: dims(5), coords(5)
  LOGICAL :: periods(5)
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  maxdims = 5
  CALL MPI_Cart_get(comm, maxdims, dims, periods, coords, ierror)
  CALL MPI_Cart_get(comm=comm, maxdims=maxdims, dims=dims, periods=periods, coords=coords, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Cart_map
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: ndims, dims(5)
  LOGICAL :: periods(5)
  INTEGER :: newrank
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  ndims = 5
  dims = 5
  periods = .TRUE.
  CALL MPI_Cart_map(comm, ndims, dims, periods, newrank, ierror)
  CALL MPI_Cart_map(comm=comm, ndims=ndims, dims=dims, periods=periods, newrank=newrank, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Cart_rank
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: coords(5)
  INTEGER :: rank
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  coords = 5
  CALL MPI_Cart_rank(comm, coords, rank, ierror)
  CALL MPI_Cart_rank(comm=comm, coords=coords, rank=rank, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Cart_shift
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: direction, disp
  INTEGER :: rank_source, rank_dest
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  direction = 5
  disp = 5
  CALL MPI_Cart_shift(comm, direction, disp, rank_source, rank_dest, ierror)
  CALL MPI_Cart_shift(comm=comm, direction=direction, disp=disp, rank_source=rank_source, &
                             rank_dest=rank_dest, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Cart_sub
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  LOGICAL :: remain_dims(5)
  TYPE(MPI_Comm) :: newcomm
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  remain_dims = .TRUE.
  CALL MPI_Cart_sub(comm, remain_dims, newcomm, ierror)
  CALL MPI_Cart_sub(comm=comm, remain_dims=remain_dims, newcomm=newcomm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Dims_create
  USE mpi_f08
  INTEGER :: nnodes, ndims
  INTEGER :: dims(5)
  INTEGER :: ierror
  nnodes = 5
  ndims = 5
  dims = 5
  CALL MPI_Dims_create(nnodes, ndims, dims, ierror)
  CALL MPI_Dims_create(nnodes=nnodes, ndims=ndims, dims=dims, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Dist_graph_create
  USE mpi_f08
  TYPE(MPI_Comm) :: comm_old
  INTEGER :: n, sources(5), degrees(5), destinations(5)
  INTEGER :: weights(5)
  TYPE(MPI_Info) :: info
  LOGICAL :: reorder
  TYPE(MPI_Comm) :: comm_dist_graph
  INTEGER :: ierror
  comm_old = MPI_COMM_NULL
  n = 5
  sources = 5
  degrees = 5
  destinations = 5
  weights = 5
  info = MPI_INFO_NULL
  reorder = .TRUE.
  CALL MPI_Dist_graph_create(comm_old, n, sources, degrees, destinations, weights, info, &
                             reorder, comm_dist_graph, ierror)
  CALL MPI_Dist_graph_create(comm_old=comm_old, n=n, sources=sources, degrees=degrees, destinations=destinations, &
                             weights=weights, info=info, reorder=reorder, comm_dist_graph=comm_dist_graph, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Dist_graph_create_adjacent
  USE mpi_f08
  TYPE(MPI_Comm) :: comm_old
  INTEGER :: indegree, sources(5), outdegree, destinations(5)
  INTEGER :: sourceweights(5), destweights(5)
  TYPE(MPI_Info) :: info
  LOGICAL :: reorder
  TYPE(MPI_Comm) :: comm_dist_graph
  INTEGER :: ierror
  comm_old = MPI_COMM_NULL
  indegree = 5
  sources = 5
  outdegree = 5
  destinations = 5
  sourceweights = 5
  destweights = 5
  info = MPI_INFO_NULL
  reorder = .TRUE.
  CALL MPI_Dist_graph_create_adjacent(comm_old, indegree, sources, sourceweights, outdegree, &
                             destinations, destweights, info, reorder, comm_dist_graph, &
                             ierror)
  CALL MPI_Dist_graph_create_adjacent(comm_old=comm_old, indegree=indegree, sources=sources, &
                             sourceweights=sourceweights, outdegree=outdegree, destinations=destinations, &
                             destweights=destweights, info=info, reorder=reorder, comm_dist_graph=comm_dist_graph, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Dist_graph_neighbors
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: maxindegree, maxoutdegree
  INTEGER :: sources(5), destinations(5)
  INTEGER :: sourceweights(5), destweights(5)
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  maxindegree = 5
  maxoutdegree = 5
  CALL MPI_Dist_graph_neighbors(comm, maxindegree, sources, sourceweights, maxoutdegree, &
                             destinations, destweights, ierror)
  CALL MPI_Dist_graph_neighbors(comm=comm, maxindegree=maxindegree, sources=sources, sourceweights=sourceweights, &
                             maxoutdegree=maxoutdegree, destinations=destinations, destweights=destweights, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Dist_graph_neighbors_count
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: indegree, outdegree
  LOGICAL :: weighted
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Dist_graph_neighbors_count(comm, indegree, outdegree, weighted, ierror)
  CALL MPI_Dist_graph_neighbors_count(comm=comm, indegree=indegree, outdegree=outdegree, &
                             weighted=weighted, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Graphdims_get
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: nnodes, nedges
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Graphdims_get(comm, nnodes, nedges, ierror)
  CALL MPI_Graphdims_get(comm=comm, nnodes=nnodes, nedges=nedges, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Graph_create
  USE mpi_f08
  TYPE(MPI_Comm) :: comm_old
  INTEGER :: nnodes, index(5), edges(5)
  LOGICAL :: reorder
  TYPE(MPI_Comm) :: comm_graph
  INTEGER :: ierror
  comm_old = MPI_COMM_NULL
  nnodes = 5
  index = 5
  edges = 5
  reorder = .TRUE.
  CALL MPI_Graph_create(comm_old, nnodes, index, edges, reorder, comm_graph, ierror)
  CALL MPI_Graph_create(comm_old=comm_old, nnodes=nnodes, index=index, edges=edges, reorder=reorder, &
                             comm_graph=comm_graph, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Graph_get
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: maxindex, maxedges
  INTEGER :: index(5), edges(5)
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  maxindex = 5
  maxedges = 5
  CALL MPI_Graph_get(comm, maxindex, maxedges, index, edges, ierror)
  CALL MPI_Graph_get(comm=comm, maxindex=maxindex, maxedges=maxedges, index=index, edges=edges, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Graph_map
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: nnodes, index(5), edges(5)
  INTEGER :: newrank
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  nnodes = 5
  index = 5
  edges = 5
  CALL MPI_Graph_map(comm, nnodes, index, edges, newrank, ierror)
  CALL MPI_Graph_map(comm=comm, nnodes=nnodes, index=index, edges=edges, newrank=newrank, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Graph_neighbors
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: rank, maxneighbors
  INTEGER :: neighbors(5)
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  rank = 5
  maxneighbors = 5
  CALL MPI_Graph_neighbors(comm, rank, maxneighbors, neighbors, ierror)
  CALL MPI_Graph_neighbors(comm=comm, rank=rank, maxneighbors=maxneighbors, neighbors=neighbors, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Graph_neighbors_count
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: rank
  INTEGER :: nneighbors
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  rank = 5
  CALL MPI_Graph_neighbors_count(comm, rank, nneighbors, ierror)
  CALL MPI_Graph_neighbors_count(comm=comm, rank=rank, nneighbors=nneighbors, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Topo_test
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: status
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Topo_test(comm, status, ierror)
  CALL MPI_Topo_test(comm=comm, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Wtick
  USE mpi_f08
  DOUBLE PRECISION :: t
  t = MPI_Wtick()
END SUBROUTINE


SUBROUTINE TST_Wtime
  USE mpi_f08
  DOUBLE PRECISION :: t
  t = MPI_Wtime()
END SUBROUTINE


SUBROUTINE TST_Abort
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: errorcode
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  errorcode = 5
  CALL MPI_Abort(comm, errorcode, ierror)
  CALL MPI_Abort(comm=comm, errorcode=errorcode, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Add_error_class
  USE mpi_f08
  INTEGER :: errorclass
  INTEGER :: ierror
  CALL MPI_Add_error_class(errorclass, ierror)
  CALL MPI_Add_error_class(errorclass=errorclass, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Add_error_code
  USE mpi_f08
  INTEGER :: errorclass
  INTEGER :: errorcode
  INTEGER :: ierror
  errorclass = 5
  CALL MPI_Add_error_code(errorclass, errorcode, ierror)
  CALL MPI_Add_error_code(errorclass=errorclass, errorcode=errorcode, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Add_error_string
  USE mpi_f08
  INTEGER :: errorcode
  CHARACTER(LEN=5) :: string
  INTEGER :: ierror
  errorcode = 5
  string = 'abcde'
  CALL MPI_Add_error_string(errorcode, string, ierror)
  CALL MPI_Add_error_string(errorcode=errorcode, string=string, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Alloc_mem
  USE mpi_f08
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
  INTEGER(KIND=MPI_ADDRESS_KIND) :: size
  TYPE(MPI_Info) :: info
  TYPE(C_PTR) :: baseptr
  INTEGER :: ierror
  size = 5
  info = MPI_INFO_NULL
  CALL MPI_Alloc_mem(size, info, baseptr, ierror)
  CALL MPI_Alloc_mem(size=size, info=info, baseptr=baseptr, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_call_errhandler
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: errorcode
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  errorcode = 5
  CALL MPI_Comm_call_errhandler(comm, errorcode, ierror)
  CALL MPI_Comm_call_errhandler(comm=comm, errorcode=errorcode, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_create_errhandler
  USE mpi_f08
  USE test_callback_routines
  ! PROCEDURE(MPI_Comm_errhandler_function) :: comm_errhandler_fn
  TYPE(MPI_Errhandler) :: errhandler
  INTEGER :: ierror
  CALL MPI_Comm_create_errhandler(comm_errhandler_fn, errhandler, ierror)
  CALL MPI_Comm_create_errhandler(comm_errhandler_fn=comm_errhandler_fn, errhandler=errhandler, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_get_errhandler
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Errhandler) :: errhandler
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Comm_get_errhandler(comm, errhandler, ierror)
  CALL MPI_Comm_get_errhandler(comm=comm, errhandler=errhandler, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_set_errhandler
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Errhandler) :: errhandler
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  errhandler = MPI_ERRHANDLER_NULL
  CALL MPI_Comm_set_errhandler(comm, errhandler, ierror)
  CALL MPI_Comm_set_errhandler(comm=comm, errhandler=errhandler, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Errhandler_free
  USE mpi_f08
  TYPE(MPI_Errhandler) :: errhandler
  INTEGER :: ierror
  errhandler = MPI_ERRHANDLER_NULL
  CALL MPI_Errhandler_free(errhandler, ierror)
  CALL MPI_Errhandler_free(errhandler=errhandler, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Error_class
  USE mpi_f08
  INTEGER :: errorcode
  INTEGER :: errorclass
  INTEGER :: ierror
  errorcode = 5
  CALL MPI_Error_class(errorcode, errorclass, ierror)
  CALL MPI_Error_class(errorcode=errorcode, errorclass=errorclass, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Error_string
  USE mpi_f08
  INTEGER :: errorcode
  CHARACTER(LEN=MPI_MAX_ERROR_STRING) :: string
  INTEGER :: resultlen
  INTEGER :: ierror
  errorcode = 5
  CALL MPI_Error_string(errorcode, string, resultlen, ierror)
  CALL MPI_Error_string(errorcode=errorcode, string=string, resultlen=resultlen, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_call_errhandler
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER :: errorcode
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  errorcode = 5
  CALL MPI_File_call_errhandler(fh, errorcode, ierror)
  CALL MPI_File_call_errhandler(fh=fh, errorcode=errorcode, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_create_errhandler
  USE mpi_f08
  USE test_callback_routines
  ! PROCEDURE(MPI_File_errhandler_function) :: file_errhandler_fn
  TYPE(MPI_Errhandler) :: errhandler
  INTEGER :: ierror
  CALL MPI_File_create_errhandler(file_errhandler_fn, errhandler, ierror)
  CALL MPI_File_create_errhandler(file_errhandler_fn=file_errhandler_fn, errhandler=errhandler, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_get_errhandler
  USE mpi_f08
  TYPE(MPI_File) :: file
  TYPE(MPI_Errhandler) :: errhandler
  INTEGER :: ierror
  file = MPI_FILE_NULL
  CALL MPI_File_get_errhandler(file, errhandler, ierror)
  CALL MPI_File_get_errhandler(file=file, errhandler=errhandler, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_set_errhandler
  USE mpi_f08
  TYPE(MPI_File) :: file
  TYPE(MPI_Errhandler) :: errhandler
  INTEGER :: ierror
  file = MPI_FILE_NULL
  errhandler = MPI_ERRHANDLER_NULL
  CALL MPI_File_set_errhandler(file, errhandler, ierror)
  CALL MPI_File_set_errhandler(file=file, errhandler=errhandler, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Finalize
  USE mpi_f08
  INTEGER :: ierror
  CALL MPI_Finalize(ierror)
  CALL MPI_Finalize(ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Finalized
  USE mpi_f08
  LOGICAL :: flag
  INTEGER :: ierror
  CALL MPI_Finalized(flag, ierror)
  CALL MPI_Finalized(flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Free_mem
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: base
  INTEGER :: ierror
  base = 5.0
  CALL MPI_Free_mem(base, ierror)
  CALL MPI_Free_mem(base=base, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Get_processor_name
  USE mpi_f08
  CHARACTER(LEN=MPI_MAX_PROCESSOR_NAME) :: name
  INTEGER :: resultlen
  INTEGER :: ierror
  CALL MPI_Get_processor_name(name, resultlen, ierror)
  CALL MPI_Get_processor_name(name=name, resultlen=resultlen, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Get_version
  USE mpi_f08
  INTEGER :: version, subversion
  INTEGER :: ierror
  CALL MPI_Get_version(version, subversion, ierror)
  CALL MPI_Get_version(version=version, subversion=subversion, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Init
  USE mpi_f08
  INTEGER :: ierror
  CALL MPI_Init(ierror)
  CALL MPI_Init(ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Initialized
  USE mpi_f08
  LOGICAL :: flag
  INTEGER :: ierror
  CALL MPI_Initialized(flag, ierror)
  CALL MPI_Initialized(flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_call_errhandler
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: errorcode
  INTEGER :: ierror
  win = MPI_WIN_NULL
  errorcode = 5
  CALL MPI_Win_call_errhandler(win, errorcode, ierror)
  CALL MPI_Win_call_errhandler(win=win, errorcode=errorcode, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_create_errhandler
  USE mpi_f08
  USE test_callback_routines
  ! PROCEDURE(MPI_Win_errhandler_function) :: win_errhandler_fn
  TYPE(MPI_Errhandler) :: errhandler
  INTEGER :: ierror
  CALL MPI_Win_create_errhandler(win_errhandler_fn, errhandler, ierror)
  CALL MPI_Win_create_errhandler(win_errhandler_fn=win_errhandler_fn, errhandler=errhandler, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_get_errhandler
  USE mpi_f08
  TYPE(MPI_Win) :: win
  TYPE(MPI_Errhandler) :: errhandler
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_get_errhandler(win, errhandler, ierror)
  CALL MPI_Win_get_errhandler(win=win, errhandler=errhandler, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_set_errhandler
  USE mpi_f08
  TYPE(MPI_Win) :: win
  TYPE(MPI_Errhandler) :: errhandler
  INTEGER :: ierror
  win = MPI_WIN_NULL
  errhandler = MPI_ERRHANDLER_NULL
  CALL MPI_Win_set_errhandler(win, errhandler, ierror)
  CALL MPI_Win_set_errhandler(win=win, errhandler=errhandler, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Info_create
  USE mpi_f08
  TYPE(MPI_Info) :: info
  INTEGER :: ierror
  CALL MPI_Info_create(info, ierror)
  CALL MPI_Info_create(info=info, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Info_delete
  USE mpi_f08
  TYPE(MPI_Info) :: info
  CHARACTER(LEN=5) :: key
  INTEGER :: ierror
  info = MPI_INFO_NULL
  key = 'abcde'
  CALL MPI_Info_delete(info, key, ierror)
  CALL MPI_Info_delete(info=info, key=key, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Info_dup
  USE mpi_f08
  TYPE(MPI_Info) :: info
  TYPE(MPI_Info) :: newinfo
  INTEGER :: ierror
  info = MPI_INFO_NULL
  CALL MPI_Info_dup(info, newinfo, ierror)
  CALL MPI_Info_dup(info=info, newinfo=newinfo, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Info_free
  USE mpi_f08
  TYPE(MPI_Info) :: info
  INTEGER :: ierror
  info = MPI_INFO_NULL
  CALL MPI_Info_free(info, ierror)
  CALL MPI_Info_free(info=info, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Info_get
  USE mpi_f08
  TYPE(MPI_Info) :: info
  CHARACTER(LEN=5) :: key
  INTEGER :: valuelen
  CHARACTER(LEN=5) :: value
  LOGICAL :: flag
  INTEGER :: ierror
  info = MPI_INFO_NULL
  key = 'abcde'
  valuelen = 5
  CALL MPI_Info_get(info, key, valuelen, value, flag, ierror)
  CALL MPI_Info_get(info=info, key=key, valuelen=valuelen, value=value, flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Info_get_nkeys
  USE mpi_f08
  TYPE(MPI_Info) :: info
  INTEGER :: nkeys
  INTEGER :: ierror
  info = MPI_INFO_NULL
  CALL MPI_Info_get_nkeys(info, nkeys, ierror)
  CALL MPI_Info_get_nkeys(info=info, nkeys=nkeys, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Info_get_nthkey
  USE mpi_f08
  TYPE(MPI_Info) :: info
  INTEGER :: n
  CHARACTER(LEN=5) :: key
  INTEGER :: ierror
  info = MPI_INFO_NULL
  n = 5
  CALL MPI_Info_get_nthkey(info, n, key, ierror)
  CALL MPI_Info_get_nthkey(info=info, n=n, key=key, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Info_get_valuelen
  USE mpi_f08
  TYPE(MPI_Info) :: info
  CHARACTER(LEN=5) :: key
  INTEGER :: valuelen
  LOGICAL :: flag
  INTEGER :: ierror
  info = MPI_INFO_NULL
  key = 'abcde'
  CALL MPI_Info_get_valuelen(info, key, valuelen, flag, ierror)
  CALL MPI_Info_get_valuelen(info=info, key=key, valuelen=valuelen, flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Info_set
  USE mpi_f08
  TYPE(MPI_Info) :: info
  CHARACTER(LEN=5) :: key, value
  INTEGER :: ierror
  info = MPI_INFO_NULL
  key = 'abcde'
  value = 'abcde'
  CALL MPI_Info_set(info, key, value, ierror)
  CALL MPI_Info_set(info=info, key=key, value=value, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Close_port
  USE mpi_f08
  CHARACTER(LEN=5) :: port_name
  INTEGER :: ierror
  port_name = 'abcde'
  CALL MPI_Close_port(port_name, ierror)
  CALL MPI_Close_port(port_name=port_name, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_accept
  USE mpi_f08
  CHARACTER(LEN=5) :: port_name
  TYPE(MPI_Info) :: info
  INTEGER :: root
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Comm) :: newcomm
  INTEGER :: ierror
  port_name = 'abcde'
  info = MPI_INFO_NULL
  root = 5
  comm = MPI_COMM_NULL
  CALL MPI_Comm_accept(port_name, info, root, comm, newcomm, ierror)
  CALL MPI_Comm_accept(port_name=port_name, info=info, root=root, comm=comm, newcomm=newcomm, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_connect
  USE mpi_f08
  CHARACTER(LEN=5) :: port_name
  TYPE(MPI_Info) :: info
  INTEGER :: root
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Comm) :: newcomm
  INTEGER :: ierror
  port_name = 'abcde'
  info = MPI_INFO_NULL
  root = 5
  comm = MPI_COMM_NULL
  CALL MPI_Comm_connect(port_name, info, root, comm, newcomm, ierror)
  CALL MPI_Comm_connect(port_name=port_name, info=info, root=root, comm=comm, newcomm=newcomm, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_disconnect
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Comm_disconnect(comm, ierror)
  CALL MPI_Comm_disconnect(comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_get_parent
  USE mpi_f08
  TYPE(MPI_Comm) :: parent
  INTEGER :: ierror
  CALL MPI_Comm_get_parent(parent, ierror)
  CALL MPI_Comm_get_parent(parent=parent, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_join
  USE mpi_f08
  INTEGER :: fd
  TYPE(MPI_Comm) :: intercomm
  INTEGER :: ierror
  fd = 5
  CALL MPI_Comm_join(fd, intercomm, ierror)
  CALL MPI_Comm_join(fd=fd, intercomm=intercomm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_spawn
  USE mpi_f08
  CHARACTER(LEN=5) :: command, argv(5)
  INTEGER :: maxprocs, root
  TYPE(MPI_Info) :: info
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Comm) :: intercomm
  INTEGER :: array_of_errcodes(5)
  INTEGER :: ierror
  command = 'abcde'
  argv = 'abcde'
  maxprocs = 5
  root = 5
  info = MPI_INFO_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Comm_spawn(command, argv, maxprocs, info, root, comm, intercomm, array_of_errcodes, &
                             ierror)
  CALL MPI_Comm_spawn(command=command, argv=argv, maxprocs=maxprocs, info=info, root=root, &
                             comm=comm, intercomm=intercomm, array_of_errcodes=array_of_errcodes, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_spawn_multiple
  USE mpi_f08
  INTEGER :: count, array_of_maxprocs(5), root
  CHARACTER(LEN=5) :: array_of_commands(5), array_of_argv(5,5)
  TYPE(MPI_Info) :: array_of_info(5)
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Comm) :: intercomm
  INTEGER :: array_of_errcodes(5)
  INTEGER :: ierror
  count = 5
  array_of_maxprocs = 5
  root = 5
  array_of_commands = 'abcde'
  array_of_argv = 'abcde'
  array_of_info = MPI_INFO_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Comm_spawn_multiple(count, array_of_commands, array_of_argv, array_of_maxprocs, &
                             array_of_info, root, comm, intercomm, array_of_errcodes, ierror)
  CALL MPI_Comm_spawn_multiple(count=count, array_of_commands=array_of_commands, array_of_argv=array_of_argv, &
                             array_of_maxprocs=array_of_maxprocs, array_of_info=array_of_info, &
                             root=root, comm=comm, intercomm=intercomm, array_of_errcodes=array_of_errcodes, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Lookup_name
  USE mpi_f08
  CHARACTER(LEN=5) :: service_name
  TYPE(MPI_Info) :: info
  CHARACTER(LEN=MPI_MAX_PORT_NAME) :: port_name
  INTEGER :: ierror
  service_name = 'abcde'
  info = MPI_INFO_NULL
  CALL MPI_Lookup_name(service_name, info, port_name, ierror)
  CALL MPI_Lookup_name(service_name=service_name, info=info, port_name=port_name, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Open_port
  USE mpi_f08
  TYPE(MPI_Info) :: info
  CHARACTER(LEN=MPI_MAX_PORT_NAME) :: port_name
  INTEGER :: ierror
  info = MPI_INFO_NULL
  CALL MPI_Open_port(info, port_name, ierror)
  CALL MPI_Open_port(info=info, port_name=port_name, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Publish_name
  USE mpi_f08
  TYPE(MPI_Info) :: info
  CHARACTER(LEN=5) :: service_name, port_name
  INTEGER :: ierror
  info = MPI_INFO_NULL
  service_name = 'abcde'
  port_name = 'abcde'
  CALL MPI_Publish_name(service_name, info, port_name, ierror)
  CALL MPI_Publish_name(service_name=service_name, info=info, port_name=port_name, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Unpublish_name
  USE mpi_f08
  CHARACTER(LEN=5) :: service_name, port_name
  TYPE(MPI_Info) :: info
  INTEGER :: ierror
  service_name = 'abcde'
  port_name = 'abcde'
  info = MPI_INFO_NULL
  CALL MPI_Unpublish_name(service_name, info, port_name, ierror)
  CALL MPI_Unpublish_name(service_name=service_name, info=info, port_name=port_name, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Accumulate
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: origin_addr
  INTEGER :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: target_disp
  TYPE(MPI_Op) :: op
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  origin_addr = 5.0
  origin_count = 5
  target_rank = 5
  target_count = 5
  origin_datatype = MPI_DATATYPE_NULL
  target_datatype = MPI_DATATYPE_NULL
  target_disp = 5
  op = MPI_OP_NULL
  win = MPI_WIN_NULL
  CALL MPI_Accumulate(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
                             target_count, target_datatype, op, win, ierror)
  CALL MPI_Accumulate(origin_addr=origin_addr, origin_count=origin_count, origin_datatype=origin_datatype, &
                             target_rank=target_rank, target_disp=target_disp, target_count=target_count, &
                             target_datatype=target_datatype, op=op, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Get
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: origin_addr
  INTEGER :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: target_disp
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  origin_count = 5
  target_rank = 5
  target_count = 5
  origin_datatype = MPI_DATATYPE_NULL
  target_datatype = MPI_DATATYPE_NULL
  target_disp = 5
  win = MPI_WIN_NULL
  CALL MPI_Get(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, &
                             target_datatype, win, ierror)
  CALL MPI_Get(origin_addr=origin_addr, origin_count=origin_count, origin_datatype=origin_datatype, &
                             target_rank=target_rank, target_disp=target_disp, target_count=target_count, &
                             target_datatype=target_datatype, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Put
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: origin_addr
  INTEGER :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: target_disp
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  origin_addr = 5.0
  origin_count = 5
  target_rank = 5
  target_count = 5
  origin_datatype = MPI_DATATYPE_NULL
  target_datatype = MPI_DATATYPE_NULL
  target_disp = 5
  win = MPI_WIN_NULL
  CALL MPI_Put(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, &
                             target_datatype, win, ierror)
  CALL MPI_Put(origin_addr=origin_addr, origin_count=origin_count, origin_datatype=origin_datatype, &
                             target_rank=target_rank, target_disp=target_disp, target_count=target_count, &
                             target_datatype=target_datatype, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_complete
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_complete(win, ierror)
  CALL MPI_Win_complete(win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_create
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: base
  INTEGER(KIND=MPI_ADDRESS_KIND) :: size
  INTEGER :: disp_unit
  TYPE(MPI_Info) :: info
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  size = 5
  disp_unit = 5
  info = MPI_INFO_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Win_create(base, size, disp_unit, info, comm, win, ierror)
  CALL MPI_Win_create(base=base, size=size, disp_unit=disp_unit, info=info, comm=comm, win=win, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_fence
  USE mpi_f08
  INTEGER :: assert
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  assert = 5
  win = MPI_WIN_NULL
  CALL MPI_Win_fence(assert, win, ierror)
  CALL MPI_Win_fence(assert=assert, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_free
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_free(win, ierror)
  CALL MPI_Win_free(win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_get_group
  USE mpi_f08
  TYPE(MPI_Win) :: win
  TYPE(MPI_Group) :: group
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_get_group(win, group, ierror)
  CALL MPI_Win_get_group(win=win, group=group, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_lock
  USE mpi_f08
  INTEGER :: lock_type, rank, assert
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  lock_type = 5
  rank = 5
  assert = 5
  win = MPI_WIN_NULL
  CALL MPI_Win_lock(lock_type, rank, assert, win, ierror)
  CALL MPI_Win_lock(lock_type=lock_type, rank=rank, assert=assert, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_post
  USE mpi_f08
  TYPE(MPI_Group) :: group
  INTEGER :: assert
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  group = MPI_GROUP_NULL
  assert = 5
  win = MPI_WIN_NULL
  CALL MPI_Win_post(group, assert, win, ierror)
  CALL MPI_Win_post(group=group, assert=assert, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_start
  USE mpi_f08
  TYPE(MPI_Group) :: group
  INTEGER :: assert
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  group = MPI_GROUP_NULL
  assert = 5
  win = MPI_WIN_NULL
  CALL MPI_Win_start(group, assert, win, ierror)
  CALL MPI_Win_start(group=group, assert=assert, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_test
  USE mpi_f08
  TYPE(MPI_Win) :: win
  LOGICAL :: flag
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_test(win, flag, ierror)
  CALL MPI_Win_test(win=win, flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_unlock
  USE mpi_f08
  INTEGER :: rank
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  rank = 5
  win = MPI_WIN_NULL
  CALL MPI_Win_unlock(rank, win, ierror)
  CALL MPI_Win_unlock(rank=rank, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_wait
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_wait(win, ierror)
  CALL MPI_Win_wait(win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Grequest_complete
  USE mpi_f08
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  request = MPI_REQUEST_NULL
  CALL MPI_Grequest_complete(request, ierror)
  CALL MPI_Grequest_complete(request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Grequest_start
  USE mpi_f08
  USE test_callback_routines
  ! PROCEDURE(MPI_Grequest_query_function) :: query_fn
  ! PROCEDURE(MPI_Grequest_free_function) :: free_fn
  ! PROCEDURE(MPI_Grequest_cancel_function) :: cancel_fn
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  extra_state = 5
  CALL MPI_Grequest_start(query_fn, free_fn, cancel_fn, extra_state, request, ierror)
  CALL MPI_Grequest_start(query_fn=query_fn, free_fn=free_fn, cancel_fn=cancel_fn, extra_state=extra_state, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Init_thread
  USE mpi_f08
  INTEGER :: required
  INTEGER :: provided
  INTEGER :: ierror
  required = 5
  CALL MPI_Init_thread(required, provided, ierror)
  CALL MPI_Init_thread(required=required, provided=provided, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Is_thread_main
  USE mpi_f08
  LOGICAL :: flag
  INTEGER :: ierror
  CALL MPI_Is_thread_main(flag, ierror)
  CALL MPI_Is_thread_main(flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Query_thread
  USE mpi_f08
  INTEGER :: provided
  INTEGER :: ierror
  CALL MPI_Query_thread(provided, ierror)
  CALL MPI_Query_thread(provided=provided, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Status_set_cancelled
  USE mpi_f08
  TYPE(MPI_Status) :: status
  LOGICAL :: flag
  INTEGER :: ierror
  status%MPI_SOURCE = 0
  CALL MPI_Status_set_cancelled(status, flag, ierror)
  CALL MPI_Status_set_cancelled(status=status, flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Status_set_elements
  USE mpi_f08
  TYPE(MPI_Status) :: status
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: count
  INTEGER :: ierror
  status%MPI_SOURCE = 0
  datatype = MPI_DATATYPE_NULL
  count = 5
  CALL MPI_Status_set_elements(status, datatype, count, ierror)
  CALL MPI_Status_set_elements(status=status, datatype=datatype, count=count, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_close
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_close(fh, ierror)
  CALL MPI_File_close(fh=fh, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_delete
  USE mpi_f08
  CHARACTER(LEN=5) :: filename
  TYPE(MPI_Info) :: info
  INTEGER :: ierror
  filename = 'abcde'
  info = MPI_INFO_NULL
  CALL MPI_File_delete(filename, info, ierror)
  CALL MPI_File_delete(filename=filename, info=info, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_get_amode
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER :: amode
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_get_amode(fh, amode, ierror)
  CALL MPI_File_get_amode(fh=fh, amode=amode, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_get_atomicity
  USE mpi_f08
  TYPE(MPI_File) :: fh
  LOGICAL :: flag
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_get_atomicity(fh, flag, ierror)
  CALL MPI_File_get_atomicity(fh=fh, flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_get_byte_offset
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  INTEGER(KIND=MPI_OFFSET_KIND) :: disp
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  offset = 5
  CALL MPI_File_get_byte_offset(fh, offset, disp, ierror)
  CALL MPI_File_get_byte_offset(fh=fh, offset=offset, disp=disp, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_get_group
  USE mpi_f08
  TYPE(MPI_File) :: fh
  TYPE(MPI_Group) :: group
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_get_group(fh, group, ierror)
  CALL MPI_File_get_group(fh=fh, group=group, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_get_info
  USE mpi_f08
  TYPE(MPI_File) :: fh
  TYPE(MPI_Info) :: info_used
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_get_info(fh, info_used, ierror)
  CALL MPI_File_get_info(fh=fh, info_used=info_used, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_get_position
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_get_position(fh, offset, ierror)
  CALL MPI_File_get_position(fh=fh, offset=offset, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_get_position_shared
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_get_position_shared(fh, offset, ierror)
  CALL MPI_File_get_position_shared(fh=fh, offset=offset, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_get_size
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: size
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_get_size(fh, size, ierror)
  CALL MPI_File_get_size(fh=fh, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_get_type_extent
  USE mpi_f08
  TYPE(MPI_File) :: fh
  TYPE(MPI_Datatype) :: datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extent
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_get_type_extent(fh, datatype, extent, ierror)
  CALL MPI_File_get_type_extent(fh=fh, datatype=datatype, extent=extent, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_get_view
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: disp
  TYPE(MPI_Datatype) :: etype, filetype
  CHARACTER(LEN=5) :: datarep
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_get_view(fh, disp, etype, filetype, datarep, ierror)
  CALL MPI_File_get_view(fh=fh, disp=disp, etype=etype, filetype=filetype, datarep=datarep, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_iread
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_iread(fh, buf, count, datatype, request, ierror)
  CALL MPI_File_iread(fh=fh, buf=buf, count=count, datatype=datatype, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_iread_at
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  offset = 5
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_iread_at(fh, offset, buf, count, datatype, request, ierror)
  CALL MPI_File_iread_at(fh=fh, offset=offset, buf=buf, count=count, datatype=datatype, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_iread_shared
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_iread_shared(fh, buf, count, datatype, request, ierror)
  CALL MPI_File_iread_shared(fh=fh, buf=buf, count=count, datatype=datatype, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_iwrite
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_iwrite(fh, buf, count, datatype, request, ierror)
  CALL MPI_File_iwrite(fh=fh, buf=buf, count=count, datatype=datatype, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_iwrite_at
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  offset = 5
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_iwrite_at(fh, offset, buf, count, datatype, request, ierror)
  CALL MPI_File_iwrite_at(fh=fh, offset=offset, buf=buf, count=count, datatype=datatype, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_iwrite_shared
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_iwrite_shared(fh, buf, count, datatype, request, ierror)
  CALL MPI_File_iwrite_shared(fh=fh, buf=buf, count=count, datatype=datatype, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_open
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  CHARACTER(LEN=5) :: filename
  INTEGER :: amode
  TYPE(MPI_Info) :: info
  TYPE(MPI_File) :: fh
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  filename = 'abcde'
  amode = 5
  info = MPI_INFO_NULL
  CALL MPI_File_open(comm, filename, amode, info, fh, ierror)
  CALL MPI_File_open(comm=comm, filename=filename, amode=amode, info=info, fh=fh, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_preallocate
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: size
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  size = 5
  CALL MPI_File_preallocate(fh, size, ierror)
  CALL MPI_File_preallocate(fh=fh, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_read(fh, buf, count, datatype, status, ierror)
  CALL MPI_File_read(fh=fh, buf=buf, count=count, datatype=datatype, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read_all
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_read_all(fh, buf, count, datatype, status, ierror)
  CALL MPI_File_read_all(fh=fh, buf=buf, count=count, datatype=datatype, status=status, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read_all_begin
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_read_all_begin(fh, buf, count, datatype, ierror)
  CALL MPI_File_read_all_begin(fh=fh, buf=buf, count=count, datatype=datatype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read_all_end
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_read_all_end(fh, buf, status, ierror)
  CALL MPI_File_read_all_end(fh=fh, buf=buf, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read_at
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  offset = 5
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_read_at(fh, offset, buf, count, datatype, status, ierror)
  CALL MPI_File_read_at(fh=fh, offset=offset, buf=buf, count=count, datatype=datatype, status=status, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read_at_all
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  offset = 5
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_read_at_all(fh, offset, buf, count, datatype, status, ierror)
  CALL MPI_File_read_at_all(fh=fh, offset=offset, buf=buf, count=count, datatype=datatype, &
                             status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read_at_all_begin
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  offset = 5
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_read_at_all_begin(fh, offset, buf, count, datatype, ierror)
  CALL MPI_File_read_at_all_begin(fh=fh, offset=offset, buf=buf, count=count, datatype=datatype, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read_at_all_end
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_read_at_all_end(fh, buf, status, ierror)
  CALL MPI_File_read_at_all_end(fh=fh, buf=buf, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read_ordered
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_read_ordered(fh, buf, count, datatype, status, ierror)
  CALL MPI_File_read_ordered(fh=fh, buf=buf, count=count, datatype=datatype, status=status, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read_ordered_begin
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_read_ordered_begin(fh, buf, count, datatype, ierror)
  CALL MPI_File_read_ordered_begin(fh=fh, buf=buf, count=count, datatype=datatype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read_ordered_end
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_read_ordered_end(fh, buf, status, ierror)
  CALL MPI_File_read_ordered_end(fh=fh, buf=buf, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_read_shared
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_read_shared(fh, buf, count, datatype, status, ierror)
  CALL MPI_File_read_shared(fh=fh, buf=buf, count=count, datatype=datatype, status=status, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_seek
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  INTEGER :: whence
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  offset = 5
  whence = 5
  CALL MPI_File_seek(fh, offset, whence, ierror)
  CALL MPI_File_seek(fh=fh, offset=offset, whence=whence, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_seek_shared
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  INTEGER :: whence
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  offset = 5
  whence = 5
  CALL MPI_File_seek_shared(fh, offset, whence, ierror)
  CALL MPI_File_seek_shared(fh=fh, offset=offset, whence=whence, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_set_atomicity
  USE mpi_f08
  TYPE(MPI_File) :: fh
  LOGICAL :: flag
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  flag = .TRUE.
  CALL MPI_File_set_atomicity(fh, flag, ierror)
  CALL MPI_File_set_atomicity(fh=fh, flag=flag, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_set_info
  USE mpi_f08
  TYPE(MPI_File) :: fh
  TYPE(MPI_Info) :: info
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  info = MPI_INFO_NULL
  CALL MPI_File_set_info(fh, info, ierror)
  CALL MPI_File_set_info(fh=fh, info=info, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_set_size
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: size
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  size = 5
  CALL MPI_File_set_size(fh, size, ierror)
  CALL MPI_File_set_size(fh=fh, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_set_view
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: disp
  TYPE(MPI_Datatype) :: etype, filetype
  CHARACTER(LEN=5) :: datarep
  TYPE(MPI_Info) :: info
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  disp = 5
  etype = MPI_DATATYPE_NULL
  filetype = MPI_DATATYPE_NULL
  datarep = 'abcde'
  info = MPI_INFO_NULL
  CALL MPI_File_set_view(fh, disp, etype, filetype, datarep, info, ierror)
  CALL MPI_File_set_view(fh=fh, disp=disp, etype=etype, filetype=filetype, datarep=datarep, &
                             info=info, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_sync
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  CALL MPI_File_sync(fh, ierror)
  CALL MPI_File_sync(fh=fh, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_write(fh, buf, count, datatype, status, ierror)
  CALL MPI_File_write(fh=fh, buf=buf, count=count, datatype=datatype, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write_all
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_write_all(fh, buf, count, datatype, status, ierror)
  CALL MPI_File_write_all(fh=fh, buf=buf, count=count, datatype=datatype, status=status, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write_all_begin
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_write_all_begin(fh, buf, count, datatype, ierror)
  CALL MPI_File_write_all_begin(fh=fh, buf=buf, count=count, datatype=datatype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write_all_end
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  buf = 5.0
  CALL MPI_File_write_all_end(fh, buf, status, ierror)
  CALL MPI_File_write_all_end(fh=fh, buf=buf, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write_at
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  offset = 5
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_write_at(fh, offset, buf, count, datatype, status, ierror)
  CALL MPI_File_write_at(fh=fh, offset=offset, buf=buf, count=count, datatype=datatype, &
                             status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write_at_all
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  offset = 5
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_write_at_all(fh, offset, buf, count, datatype, status, ierror)
  CALL MPI_File_write_at_all(fh=fh, offset=offset, buf=buf, count=count, datatype=datatype, &
                             status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write_at_all_begin
  USE mpi_f08
  TYPE(MPI_File) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND) :: offset
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  offset = 5
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_write_at_all_begin(fh, offset, buf, count, datatype, ierror)
  CALL MPI_File_write_at_all_begin(fh=fh, offset=offset, buf=buf, count=count, datatype=datatype, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write_at_all_end
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  buf = 5.0
  CALL MPI_File_write_at_all_end(fh, buf, status, ierror)
  CALL MPI_File_write_at_all_end(fh=fh, buf=buf, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write_ordered
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_write_ordered(fh, buf, count, datatype, status, ierror)
  CALL MPI_File_write_ordered(fh=fh, buf=buf, count=count, datatype=datatype, status=status, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write_ordered_begin
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_write_ordered_begin(fh, buf, count, datatype, ierror)
  CALL MPI_File_write_ordered_begin(fh=fh, buf=buf, count=count, datatype=datatype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write_ordered_end
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  buf = 5.0
  CALL MPI_File_write_ordered_end(fh, buf, status, ierror)
  CALL MPI_File_write_ordered_end(fh=fh, buf=buf, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_File_write_shared
  USE mpi_f08
  TYPE(MPI_File) :: fh
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  fh = MPI_FILE_NULL
  buf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  CALL MPI_File_write_shared(fh, buf, count, datatype, status, ierror)
  CALL MPI_File_write_shared(fh=fh, buf=buf, count=count, datatype=datatype, status=status, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Register_datarep
  USE mpi_f08
  USE test_callback_routines
  CHARACTER(LEN=5) :: datarep
  ! PROCEDURE(MPI_Datarep_conversion_function) :: read_conversion_fn
  ! PROCEDURE(MPI_Datarep_conversion_function) :: write_conversion_fn
  ! PROCEDURE(MPI_Datarep_extent_function) :: dtype_file_extent_fn
  INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  INTEGER :: ierror
  datarep = 'abcde'
  extra_state = 5
  CALL MPI_Register_datarep(datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, &
                             extra_state, ierror)
  CALL MPI_Register_datarep(datarep=datarep, read_conversion_fn=read_conversion_fn, write_conversion_fn=write_conversion_fn, &
                             dtype_file_extent_fn=dtype_file_extent_fn, extra_state=extra_state, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_F_sync_reg
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  CALL MPI_F_sync_reg(buf)
  CALL MPI_F_sync_reg(buf=buf)
END SUBROUTINE


SUBROUTINE TST_Sizeof
  USE mpi_f08
  REAL, DIMENSION(5) :: x
  INTEGER :: size
  INTEGER :: ierror
  CALL MPI_Sizeof(x, size, ierror)
  CALL MPI_Sizeof(x=x, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Status_f2f08
  USE mpi_f08
  INTEGER :: f_status(MPI_STATUS_SIZE)
  TYPE(MPI_Status) :: f08_status
  INTEGER :: ierror 
  f_status = 5
  CALL MPI_Status_f2f08(f_status, f08_status, ierror)
  CALL MPI_Status_f2f08(f_status=f_status, f08_status=f08_status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Status_f082f
  USE mpi_f08
  TYPE(MPI_Status) :: f08_status
  INTEGER :: f_status(MPI_STATUS_SIZE)
  INTEGER :: ierror
  f08_status%MPI_SOURCE = 0
  CALL MPI_Status_f082f(f08_status, f_status, ierror)
  CALL MPI_Status_f082f(f08_status=f08_status, f_status=f_status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_create_f90_complex
  USE mpi_f08
  INTEGER :: p, r
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  p = 5
  r = 5
  CALL MPI_Type_create_f90_complex(p, r, newtype, ierror)
  CALL MPI_Type_create_f90_complex(p=p, r=r, newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_create_f90_integer
  USE mpi_f08
  INTEGER :: r
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  r = 5
  CALL MPI_Type_create_f90_integer(r, newtype, ierror)
  CALL MPI_Type_create_f90_integer(r=r, newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_create_f90_real
  USE mpi_f08
  INTEGER :: p, r
  TYPE(MPI_Datatype) :: newtype
  INTEGER :: ierror
  p = 5
  r = 5
  CALL MPI_Type_create_f90_real(p, r, newtype, ierror)
  CALL MPI_Type_create_f90_real(p=p, r=r, newtype=newtype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_match_size
  USE mpi_f08
  INTEGER :: typeclass, size
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: ierror
  typeclass = 5
  size = 5
  CALL MPI_Type_match_size(typeclass, size, datatype, ierror)
  CALL MPI_Type_match_size(typeclass=typeclass, size=size, datatype=datatype, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Pcontrol
  USE mpi_f08
  INTEGER :: level
  level = 5
  CALL MPI_Pcontrol(level)
  CALL MPI_Pcontrol(level=level)
END SUBROUTINE

! New MPI-3 routines
! 

SUBROUTINE TST_Iallgather
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: sendcount, recvcount
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Iallgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, &
                             request, ierror)
  CALL MPI_Iallgather(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcount=recvcount, recvtype=recvtype, comm=comm, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Iallgatherv
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: sendcount
  INTEGER, ASYNCHRONOUS :: recvcounts(5), displs(5)
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcounts = 5
  displs = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Iallgatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, &
                             comm, request, ierror)
  CALL MPI_Iallgatherv(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcounts=recvcounts, displs=displs, recvtype=recvtype, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Iallreduce
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Iallreduce(sendbuf, recvbuf, count, datatype, op, comm, request, ierror)
  CALL MPI_Iallreduce(sendbuf=sendbuf, recvbuf=recvbuf, count=count, datatype=datatype, &
                             op=op, comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ialltoall
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: sendcount, recvcount
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ialltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, &
                             ierror)
  CALL MPI_Ialltoall(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcount=recvcount, recvtype=recvtype, comm=comm, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ialltoallv
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER, ASYNCHRONOUS :: sendcounts(5), sdispls(5), recvcounts(5), rdispls(5)
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcounts = 5
  sdispls = 5
  recvcounts = 5
  rdispls = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ialltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, &
                             recvtype, comm, request, ierror)
  CALL MPI_Ialltoallv(sendbuf=sendbuf, sendcounts=sendcounts, sdispls=sdispls, sendtype=sendtype, &
                             recvbuf=recvbuf, recvcounts=recvcounts, rdispls=rdispls, recvtype=recvtype, &
                             comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ialltoallw
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER, ASYNCHRONOUS :: sendcounts(5), sdispls(5), recvcounts(5), rdispls(5)
  TYPE(MPI_Datatype), ASYNCHRONOUS :: sendtypes(5), recvtypes(5)
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcounts = 5
  sdispls = 5
  recvcounts = 5
  rdispls = 5
  sendtypes = MPI_DATATYPE_NULL
  recvtypes = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ialltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, &
                             recvtypes, comm, request, ierror)
  CALL MPI_Ialltoallw(sendbuf=sendbuf, sendcounts=sendcounts, sdispls=sdispls, sendtypes=sendtypes, &
                             recvbuf=recvbuf, recvcounts=recvcounts, rdispls=rdispls, recvtypes=recvtypes, &
                             comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ibarrier
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  comm = MPI_COMM_NULL
  CALL MPI_Ibarrier(comm, request, ierror)
  CALL MPI_Ibarrier(comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ibcast
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buffer
  INTEGER :: count, root
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  count = 5
  root = 5
  datatype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ibcast(buffer, count, datatype, root, comm, request, ierror)
  CALL MPI_Ibcast(buffer=buffer, count=count, datatype=datatype, root=root, comm=comm, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Iexscan
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Iexscan(sendbuf, recvbuf, count, datatype, op, comm, request, ierror)
  CALL MPI_Iexscan(sendbuf=sendbuf, recvbuf=recvbuf, count=count, datatype=datatype, op=op, &
                             comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Igather
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: sendcount, recvcount, root
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  root = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Igather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, &
                             request, ierror)
  CALL MPI_Igather(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcount=recvcount, recvtype=recvtype, root=root, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Igatherv
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: sendcount, root
  INTEGER, ASYNCHRONOUS :: recvcounts(5), displs(5)
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  root = 5
  recvcounts = 5
  displs = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Igatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, &
                             root, comm, request, ierror)
  CALL MPI_Igatherv(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcounts=recvcounts, displs=displs, recvtype=recvtype, root=root, &
                             comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ireduce_scatter_block
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: recvcount
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  recvcount = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ireduce_scatter_block(sendbuf, recvbuf, recvcount, datatype, op, comm, request, &
                             ierror)
  CALL MPI_Ireduce_scatter_block(sendbuf=sendbuf, recvbuf=recvbuf, recvcount=recvcount, &
                             datatype=datatype, op=op, comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ireduce_scatter
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER, ASYNCHRONOUS :: recvcounts(5)
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  recvcounts = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ireduce_scatter(sendbuf, recvbuf, recvcounts, datatype, op, comm, request, ierror)
  CALL MPI_Ireduce_scatter(sendbuf=sendbuf, recvbuf=recvbuf, recvcounts=recvcounts, datatype=datatype, &
                             op=op, comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ireduce
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: count, root
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  count = 5
  root = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ireduce(sendbuf, recvbuf, count, datatype, op, root, comm, request, ierror)
  CALL MPI_Ireduce(sendbuf=sendbuf, recvbuf=recvbuf, count=count, datatype=datatype, op=op, &
                             root=root, comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Iscan
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Op) :: op
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  count = 5
  datatype = MPI_DATATYPE_NULL
  op = MPI_OP_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Iscan(sendbuf, recvbuf, count, datatype, op, comm, request, ierror)
  CALL MPI_Iscan(sendbuf=sendbuf, recvbuf=recvbuf, count=count, datatype=datatype, op=op, &
                             comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Iscatter
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: sendcount, recvcount, root
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  root = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Iscatter(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, &
                             request, ierror)
  CALL MPI_Iscatter(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcount=recvcount, recvtype=recvtype, root=root, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Iscatterv
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER, ASYNCHRONOUS :: sendcounts(5), displs(5)
  INTEGER :: recvcount, root
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcounts = 5
  displs = 5
  recvcount = 5
  root = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Iscatterv(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, &
                             root, comm, request, ierror)
  CALL MPI_Iscatterv(sendbuf=sendbuf, sendcounts=sendcounts, displs=displs, sendtype=sendtype, &
                             recvbuf=recvbuf, recvcount=recvcount, recvtype=recvtype, root=root, &
                             comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Improbe
  USE mpi_f08
  INTEGER :: source, tag
  TYPE(MPI_Comm) :: comm
  INTEGER :: flag
  TYPE(MPI_Message) :: message
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  source = 5
  tag = 5
  comm = MPI_COMM_NULL
  CALL MPI_Improbe(source, tag, comm, flag, message, status, ierror)
  CALL MPI_Improbe(source=source, tag=tag, comm=comm, flag=flag, message=message, status=status, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Mprobe
  USE mpi_f08
  INTEGER :: source, tag
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Message) :: message
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  source = 5
  tag = 5
  comm = MPI_COMM_NULL
  CALL MPI_Mprobe(source, tag, comm, message, status, ierror)
  CALL MPI_Mprobe(source=source, tag=tag, comm=comm, message=message, status=status, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Mrecv
  USE mpi_f08
  REAL, DIMENSION(5) :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Message) :: message
  TYPE(MPI_Status) :: status
  INTEGER :: ierror
  count = 5
  datatype = MPI_DATATYPE_NULL
  message = MPI_MESSAGE_NULL
  CALL MPI_Mrecv(buf, count, datatype, message, status, ierror)
  CALL MPI_Mrecv(buf=buf, count=count, datatype=datatype, message=message, status=status, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Imrecv
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: buf
  INTEGER :: count
  TYPE(MPI_Datatype) :: datatype
  TYPE(MPI_Message) :: message
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  count = 5
  datatype = MPI_DATATYPE_NULL
  message = MPI_MESSAGE_NULL
  CALL MPI_Imrecv(buf, count, datatype, message, request, ierror)
  CALL MPI_Imrecv(buf=buf, count=count, datatype=datatype, message=message, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Neighbor_allgather
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcount, recvcount
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Neighbor_allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, &
                             comm, ierror)
  CALL MPI_Neighbor_allgather(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcount=recvcount, recvtype=recvtype, comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Neighbor_allgatherv
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcount, recvcounts(5), displs(5)
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcounts = 5
  displs = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Neighbor_allgatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, &
                             recvtype, comm, ierror)
  CALL MPI_Neighbor_allgatherv(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, &
                             recvbuf=recvbuf, recvcounts=recvcounts, displs=displs, recvtype=recvtype, &
                             comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Neighbor_alltoall
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcount, recvcount
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Neighbor_alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, &
                             comm, ierror)
  CALL MPI_Neighbor_alltoall(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcount=recvcount, recvtype=recvtype, comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Neighbor_alltoallv
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcounts(5), sdispls(5), recvcounts(5), rdispls(5)
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcounts = 5
  sdispls = 5
  recvcounts = 5
  rdispls = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Neighbor_alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, &
                             rdispls, recvtype, comm, ierror)
  CALL MPI_Neighbor_alltoallv(sendbuf=sendbuf, sendcounts=sendcounts, sdispls=sdispls, sendtype=sendtype, &
                             recvbuf=recvbuf, recvcounts=recvcounts, rdispls=rdispls, recvtype=recvtype, &
                             comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Neighbor_alltoallw
  USE mpi_f08
  REAL, DIMENSION(5) :: sendbuf
  REAL, DIMENSION(5) :: recvbuf
  INTEGER :: sendcounts(5), recvcounts(5)
  INTEGER(KIND=MPI_ADDRESS_KIND) :: sdispls(5), rdispls(5)
  ! CAUTION: Please check that all other interfaces also use address-size-integer !!!!
  TYPE(MPI_Datatype) :: sendtypes(5), recvtypes(5)
  TYPE(MPI_Comm) :: comm
  INTEGER :: ierror
  sendbuf = 5.0
  sendcounts = 5
  recvcounts = 5
  sdispls = 5
  rdispls = 5
  sendtypes = MPI_DATATYPE_NULL
  recvtypes = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Neighbor_alltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, &
                             rdispls, recvtypes, comm, ierror)
  CALL MPI_Neighbor_alltoallw(sendbuf=sendbuf, sendcounts=sendcounts, sdispls=sdispls, sendtypes=sendtypes, &
                             recvbuf=recvbuf, recvcounts=recvcounts, rdispls=rdispls, recvtypes=recvtypes, &
                             comm=comm, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ineighbor_allgather
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: sendcount, recvcount
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ineighbor_allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, &
                             comm, request, ierror)
  CALL MPI_Ineighbor_allgather(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, &
                             recvbuf=recvbuf, recvcount=recvcount, recvtype=recvtype, comm=comm, &
                             request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ineighbor_allgatherv
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: sendcount
  INTEGER, ASYNCHRONOUS :: recvcounts(5), displs(5)
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcounts = 5
  displs = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ineighbor_allgatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, &
                             recvtype, comm, request, ierror)
  CALL MPI_Ineighbor_allgatherv(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, &
                             recvbuf=recvbuf, recvcounts=recvcounts, displs=displs, recvtype=recvtype, &
                             comm=comm, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ineighbor_alltoall
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER :: sendcount, recvcount
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcount = 5
  recvcount = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ineighbor_alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, &
                             comm, request, ierror)
  CALL MPI_Ineighbor_alltoall(sendbuf=sendbuf, sendcount=sendcount, sendtype=sendtype, recvbuf=recvbuf, &
                             recvcount=recvcount, recvtype=recvtype, comm=comm, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ineighbor_alltoallv
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER, ASYNCHRONOUS :: sendcounts(5), sdispls(5), recvcounts(5), rdispls(5)
  TYPE(MPI_Datatype) :: sendtype, recvtype
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcounts = 5
  sdispls = 5
  recvcounts = 5
  rdispls = 5
  sendtype = MPI_DATATYPE_NULL
  recvtype = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ineighbor_alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, &
                             rdispls, recvtype, comm, request, ierror)
  CALL MPI_Ineighbor_alltoallv(sendbuf=sendbuf, sendcounts=sendcounts, sdispls=sdispls, &
                             sendtype=sendtype, recvbuf=recvbuf, recvcounts=recvcounts, &
                             rdispls=rdispls, recvtype=recvtype, comm=comm, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Ineighbor_alltoallw
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: sendbuf
  REAL, DIMENSION(5), ASYNCHRONOUS :: recvbuf
  INTEGER, ASYNCHRONOUS :: sendcounts(5), recvcounts(5)
  INTEGER(KIND=MPI_ADDRESS_KIND), ASYNCHRONOUS :: sdispls(5), rdispls(5)
  ! CAUTION: Please check that all other interfaces also use address-size-integer !!!!
  TYPE(MPI_Datatype), ASYNCHRONOUS :: sendtypes(5), recvtypes(5)
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  sendbuf = 5.0
  sendcounts = 5
  recvcounts = 5
  sdispls = 5
  rdispls = 5
  sendtypes = MPI_DATATYPE_NULL
  recvtypes = MPI_DATATYPE_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Ineighbor_alltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, &
                             rdispls, recvtypes, comm, request, ierror)
  CALL MPI_Ineighbor_alltoallw(sendbuf=sendbuf, sendcounts=sendcounts, sdispls=sdispls, &
                             sendtypes=sendtypes, recvbuf=recvbuf, recvcounts=recvcounts, &
                             rdispls=rdispls, recvtypes=recvtypes, comm=comm, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_size_x
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER(KIND=MPI_COUNT_KIND) :: size
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Type_size_x(datatype, size, ierror)
  CALL MPI_Type_size_x(datatype=datatype, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_get_extent_x
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER(KIND = MPI_COUNT_KIND) :: lb, extent
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Type_get_extent_x(datatype, lb, extent, ierror)
  CALL MPI_Type_get_extent_x(datatype=datatype, lb=lb, extent=extent, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Type_get_true_extent_x
  USE mpi_f08
  TYPE(MPI_Datatype) :: datatype
  INTEGER(KIND = MPI_COUNT_KIND) :: true_lb, true_extent
  INTEGER :: ierror
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Type_get_true_extent_x(datatype, true_lb, true_extent, ierror)
  CALL MPI_Type_get_true_extent_x(datatype=datatype, true_lb=true_lb, true_extent=true_extent, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Get_elements_x
  USE mpi_f08
  TYPE(MPI_Status) :: status
  TYPE(MPI_Datatype) :: datatype
  INTEGER(KIND = MPI_COUNT_KIND) :: count
  INTEGER :: ierror
  status%MPI_SOURCE = 0
  datatype = MPI_DATATYPE_NULL
  CALL MPI_Get_elements_x(status, datatype, count, ierror)
  CALL MPI_Get_elements_x(status=status, datatype=datatype, count=count, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Status_set_elements_x
  USE mpi_f08
  TYPE(MPI_Status) :: status
  TYPE(MPI_Datatype) :: datatype
  INTEGER(KIND = MPI_COUNT_KIND) :: count
  INTEGER :: ierror
  status%MPI_SOURCE = 0
  datatype = MPI_DATATYPE_NULL
  count = 5
  CALL MPI_Status_set_elements_x(status, datatype, count, ierror)
  CALL MPI_Status_set_elements_x(status=status, datatype=datatype, count=count, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_allocate
  USE mpi_f08
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
  INTEGER(KIND=MPI_ADDRESS_KIND) :: size
  INTEGER :: disp_unit
  TYPE(MPI_Info) :: info
  TYPE(MPI_Comm) :: comm
  TYPE(C_PTR) :: baseptr
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  size = 5
  disp_unit = 5
  info = MPI_INFO_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Win_allocate(size, disp_unit, info, comm, baseptr, win, ierror)
  CALL MPI_Win_allocate(size=size, disp_unit=disp_unit, info=info, comm=comm, baseptr=baseptr, &
                             win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_create_dynamic
  USE mpi_f08
  TYPE(MPI_Info) :: info
  TYPE(MPI_Comm) :: comm
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  info = MPI_INFO_NULL
  comm = MPI_COMM_NULL
  CALL MPI_Win_create_dynamic(info, comm, win, ierror)
  CALL MPI_Win_create_dynamic(info=info, comm=comm, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_attach
  USE mpi_f08
  TYPE(MPI_Win) :: win
  REAL, ASYNCHRONOUS :: base
  INTEGER(KIND=MPI_ADDRESS_KIND) :: size
  INTEGER :: ierror
  win = MPI_WIN_NULL
  size = 5
  CALL MPI_Win_attach(win, base, size, ierror)
  CALL MPI_Win_attach(win=win, base=base, size=size, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_detach
  USE mpi_f08
  TYPE(MPI_Win) :: win
  REAL, ASYNCHRONOUS :: base
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_detach(win, base, ierror)
  CALL MPI_Win_detach(win=win, base=base, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Get_accumulate
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: origin_addr
  REAL, DIMENSION(5), ASYNCHRONOUS :: result_addr
  INTEGER :: origin_count, result_count, target_rank, target_count
  TYPE(MPI_Datatype) :: origin_datatype, target_datatype, result_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: target_disp
  TYPE(MPI_Op) :: op
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  origin_addr = 5.0
  origin_count = 5
  result_count = 5
  target_rank = 5
  target_count = 5
  origin_datatype = MPI_DATATYPE_NULL
  target_datatype = MPI_DATATYPE_NULL
  result_datatype = MPI_DATATYPE_NULL
  target_disp = 5
  op = MPI_OP_NULL
  win = MPI_WIN_NULL
  CALL MPI_Get_accumulate(origin_addr, origin_count, origin_datatype, result_addr, result_count, &
                             result_datatype, target_rank, target_disp, target_count, target_datatype, &
                             op, win, ierror)
  CALL MPI_Get_accumulate(origin_addr=origin_addr, origin_count=origin_count, origin_datatype=origin_datatype, &
                             result_addr=result_addr, result_count=result_count, result_datatype=result_datatype, &
                             target_rank=target_rank, target_disp=target_disp, target_count=target_count, &
                             target_datatype=target_datatype, op=op, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Fetch_and_op
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: origin_addr
  REAL, DIMENSION(5), ASYNCHRONOUS :: result_addr
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: target_rank
  INTEGER(KIND=MPI_ADDRESS_KIND) :: target_disp
  TYPE(MPI_Op) :: op
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  origin_addr = 5.0
  datatype = MPI_DATATYPE_NULL
  target_rank = 5
  target_disp = 5
  op = MPI_OP_NULL
  win = MPI_WIN_NULL
  CALL MPI_Fetch_and_op(origin_addr, result_addr, datatype, target_rank, target_disp, op, &
                             win, ierror)
  CALL MPI_Fetch_and_op(origin_addr=origin_addr, result_addr=result_addr, datatype=datatype, &
                             target_rank=target_rank, target_disp=target_disp, op=op, win=win, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Compare_and_swap
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: origin_addr, compare_addr
  REAL, DIMENSION(5), ASYNCHRONOUS :: result_addr
  TYPE(MPI_Datatype) :: datatype
  INTEGER :: target_rank
  INTEGER(KIND=MPI_ADDRESS_KIND) :: target_disp
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  origin_addr = 5.0
  compare_addr = 5.0
  datatype = MPI_DATATYPE_NULL
  target_rank = 5
  target_disp = 5
  win = MPI_WIN_NULL
  CALL MPI_Compare_and_swap(origin_addr, compare_addr, result_addr, datatype, target_rank, &
                             target_disp, win, ierror)
  CALL MPI_Compare_and_swap(origin_addr=origin_addr, compare_addr=compare_addr, result_addr=result_addr, &
                             datatype=datatype, target_rank=target_rank, target_disp=target_disp, &
                             win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Rput
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: origin_addr
  INTEGER :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: target_disp
  TYPE(MPI_Win) :: win
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  origin_addr = 5.0
  origin_count = 5
  target_rank = 5
  target_count = 5
  origin_datatype = MPI_DATATYPE_NULL
  target_datatype = MPI_DATATYPE_NULL
  target_disp = 5
  win = MPI_WIN_NULL
  CALL MPI_Rput(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, &
                             target_datatype, win, request, ierror)
  CALL MPI_Rput(origin_addr=origin_addr, origin_count=origin_count, origin_datatype=origin_datatype, &
                             target_rank=target_rank, target_disp=target_disp, target_count=target_count, &
                             target_datatype=target_datatype, win=win, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Rget
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: origin_addr
  INTEGER :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: target_disp
  TYPE(MPI_Win) :: win
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  origin_count = 5
  target_rank = 5
  target_count = 5
  origin_datatype = MPI_DATATYPE_NULL
  target_datatype = MPI_DATATYPE_NULL
  target_disp = 5
  win = MPI_WIN_NULL
  CALL MPI_Rget(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, &
                             target_datatype, win, request, ierror)
  CALL MPI_Rget(origin_addr=origin_addr, origin_count=origin_count, origin_datatype=origin_datatype, &
                             target_rank=target_rank, target_disp=target_disp, target_count=target_count, &
                             target_datatype=target_datatype, win=win, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Raccumulate
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: origin_addr
  INTEGER :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: target_disp
  TYPE(MPI_Op) :: op
  TYPE(MPI_Win) :: win
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  origin_addr = 5.0
  origin_count = 5
  target_rank = 5
  target_count = 5
  origin_datatype = MPI_DATATYPE_NULL
  target_datatype = MPI_DATATYPE_NULL
  target_disp = 5
  op = MPI_OP_NULL
  win = MPI_WIN_NULL
  CALL MPI_Raccumulate(origin_addr, origin_count, origin_datatype, target_rank, target_disp, &
                             target_count, target_datatype, op, win, request, ierror)
  CALL MPI_Raccumulate(origin_addr=origin_addr, origin_count=origin_count, origin_datatype=origin_datatype, &
                             target_rank=target_rank, target_disp=target_disp, target_count=target_count, &
                             target_datatype=target_datatype, op=op, win=win, request=request, &
                             ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Rget_accumulate
  USE mpi_f08
  REAL, DIMENSION(5), ASYNCHRONOUS :: origin_addr
  REAL, DIMENSION(5), ASYNCHRONOUS :: result_addr
  INTEGER :: origin_count, result_count, target_rank, target_count
  TYPE(MPI_Datatype) :: origin_datatype, target_datatype, result_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND) :: target_disp
  TYPE(MPI_Op) :: op
  TYPE(MPI_Win) :: win
  TYPE(MPI_Request) :: request
  INTEGER :: ierror
  origin_addr = 5.0
  origin_count = 5
  result_count = 5
  target_rank = 5
  target_count = 5
  origin_datatype = MPI_DATATYPE_NULL
  target_datatype = MPI_DATATYPE_NULL
  result_datatype = MPI_DATATYPE_NULL
  target_disp = 5
  op = MPI_OP_NULL
  win = MPI_WIN_NULL
  CALL MPI_Rget_accumulate(origin_addr, origin_count, origin_datatype, result_addr, result_count, &
                             result_datatype, target_rank, target_disp, target_count, target_datatype, &
                             op, win, request, ierror)
  CALL MPI_Rget_accumulate(origin_addr=origin_addr, origin_count=origin_count, origin_datatype=origin_datatype, &
                             result_addr=result_addr, result_count=result_count, result_datatype=result_datatype, &
                             target_rank=target_rank, target_disp=target_disp, target_count=target_count, &
                             target_datatype=target_datatype, op=op, win=win, request=request, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_lock_all
  USE mpi_f08
  INTEGER :: assert
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  assert = 5
  win = MPI_WIN_NULL
  CALL MPI_Win_lock_all(assert, win, ierror)
  CALL MPI_Win_lock_all(assert=assert, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_unlock_all
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_unlock_all(win, ierror)
  CALL MPI_Win_unlock_all(win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_flush
  USE mpi_f08
  INTEGER :: rank
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  rank = 5
  win = MPI_WIN_NULL
  CALL MPI_Win_flush(rank, win, ierror)
  CALL MPI_Win_flush(rank=rank, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_flush_all
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_flush_all(win, ierror)
  CALL MPI_Win_flush_all(win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_flush_local
  USE mpi_f08
  INTEGER :: rank
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  rank = 5
  win = MPI_WIN_NULL
  CALL MPI_Win_flush_local(rank, win, ierror)
  CALL MPI_Win_flush_local(rank=rank, win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_flush_local_all
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_flush_local_all(win, ierror)
  CALL MPI_Win_flush_local_all(win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Win_sync
  USE mpi_f08
  TYPE(MPI_Win) :: win
  INTEGER :: ierror
  win = MPI_WIN_NULL
  CALL MPI_Win_sync(win, ierror)
  CALL MPI_Win_sync(win=win, ierror=ierror)
END SUBROUTINE


SUBROUTINE TST_Comm_split_type
  USE mpi_f08
  TYPE(MPI_Comm) :: comm
  INTEGER :: comm_type, key
  TYPE(MPI_Info) :: info
  TYPE(MPI_Comm) :: newcomm
  INTEGER :: ierror 
  comm = MPI_COMM_NULL
  comm_type = 5
  key = 5
  info = MPI_INFO_NULL
  CALL MPI_Comm_split_type(comm, comm_type, key, info, newcomm, ierror)
  CALL MPI_Comm_split_type(comm=comm, comm_type=comm_type, key=key, info=info, newcomm=newcomm, &
                             ierror=ierror)
END SUBROUTINE

!-END MODULE test_routines

! ------------------------------------------------------------------------------------

PROGRAM TEST_MPI_IIIDOTO
!-USE test_routines

  CALL TST_Bsend
  CALL TST_Bsend_init
  CALL TST_Buffer_attach
  CALL TST_Buffer_detach
  CALL TST_Cancel
  CALL TST_Get_count
  CALL TST_Ibsend
  CALL TST_Iprobe
  CALL TST_Irecv
  CALL TST_Irsend
  CALL TST_Isend
  CALL TST_Issend
  CALL TST_Probe
  CALL TST_Recv
  CALL TST_Recv_init
  CALL TST_Request_free
  CALL TST_Request_get_status
  CALL TST_Rsend
  CALL TST_Rsend_init
  CALL TST_Send
  CALL TST_Sendrecv
  CALL TST_Sendrecv_replace
  CALL TST_Send_init
  CALL TST_Ssend
  CALL TST_Ssend_init
  CALL TST_Start
  CALL TST_Startall
  CALL TST_Test
  CALL TST_Testall
  CALL TST_Testany
  CALL TST_Testsome
  CALL TST_Test_cancelled
  CALL TST_Wait
  CALL TST_Waitall
  CALL TST_Waitany
  CALL TST_Waitsome
  CALL TST_Get_address
  CALL TST_Get_elements
  CALL TST_Pack
  CALL TST_Pack_external
  CALL TST_Pack_external_size
  CALL TST_Pack_size
  CALL TST_Type_commit
  CALL TST_Type_contiguous
  CALL TST_Type_create_darray
  CALL TST_Type_create_hindexed
  CALL TST_Type_create_hvector
  CALL TST_Type_create_indexed_block
  CALL TST_Type_create_resized
  CALL TST_Type_create_struct
  CALL TST_Type_create_subarray
  CALL TST_Type_dup
  CALL TST_Type_free
  CALL TST_Type_get_contents
  CALL TST_Type_get_envelope
  CALL TST_Type_get_extent
  CALL TST_Type_get_true_extent
  CALL TST_Type_indexed
  CALL TST_Type_size
  CALL TST_Type_vector
  CALL TST_Unpack
  CALL TST_Unpack_external
  CALL TST_Allgather
  CALL TST_Allgatherv
  CALL TST_Allreduce
  CALL TST_Alltoall
  CALL TST_Alltoallv
  CALL TST_Alltoallw
  CALL TST_Barrier
  CALL TST_Bcast
  CALL TST_Exscan
  CALL TST_Gather
  CALL TST_Gatherv
  CALL TST_Op_commutative
  CALL TST_Op_create
  CALL TST_Op_free
  CALL TST_Reduce
  CALL TST_Reduce_local
  CALL TST_Reduce_scatter
  CALL TST_Reduce_scatter_block
  CALL TST_Scan
  CALL TST_Scatter
  CALL TST_Scatterv
  CALL TST_Comm_compare
  CALL TST_Comm_create
  CALL TST_Comm_create_keyval
  CALL TST_Comm_delete_attr
  CALL TST_Comm_dup
  CALL TST_COMM_DUP_FN
  CALL TST_Comm_free
  CALL TST_Comm_free_keyval
  CALL TST_Comm_get_attr
  CALL TST_Comm_get_name
  CALL TST_Comm_group
  CALL TST_COMM_NULL_COPY_FN
  CALL TST_COMM_NULL_DELETE_FN
  CALL TST_Comm_rank
  CALL TST_Comm_remote_group
  CALL TST_Comm_remote_size
  CALL TST_Comm_set_attr
  CALL TST_Comm_set_name
  CALL TST_Comm_size
  CALL TST_Comm_split
  CALL TST_Comm_test_inter
  CALL TST_Group_compare
  CALL TST_Group_difference
  CALL TST_Group_excl
  CALL TST_Group_free
  CALL TST_Group_incl
  CALL TST_Group_intersection
  CALL TST_Group_range_excl
  CALL TST_Group_range_incl
  CALL TST_Group_rank
  CALL TST_Group_size
  CALL TST_Group_translate_ranks
  CALL TST_Group_union
  CALL TST_Intercomm_create
  CALL TST_Intercomm_merge
  CALL TST_Type_create_keyval
  CALL TST_Type_delete_attr
  CALL TST_TYPE_DUP_FN
  CALL TST_Type_free_keyval
  CALL TST_Type_get_attr
  CALL TST_Type_get_name
  CALL TST_TYPE_NULL_COPY_FN
  CALL TST_TYPE_NULL_DELETE_FN
  CALL TST_Type_set_attr
  CALL TST_Type_set_name
  CALL TST_Win_create_keyval
  CALL TST_Win_delete_attr
  CALL TST_WIN_DUP_FN
  CALL TST_Win_free_keyval
  CALL TST_Win_get_attr
  CALL TST_Win_get_name
  CALL TST_WIN_NULL_COPY_FN
  CALL TST_WIN_NULL_DELETE_FN
  CALL TST_Win_set_attr
  CALL TST_Win_set_name
  CALL TST_Cartdim_get
  CALL TST_Cart_coords
  CALL TST_Cart_create
  CALL TST_Cart_get
  CALL TST_Cart_map
  CALL TST_Cart_rank
  CALL TST_Cart_shift
  CALL TST_Cart_sub
  CALL TST_Dims_create
  CALL TST_Dist_graph_create
  CALL TST_Dist_graph_create_adjacent
  CALL TST_Dist_graph_neighbors
  CALL TST_Dist_graph_neighbors_count
  CALL TST_Graphdims_get
  CALL TST_Graph_create
  CALL TST_Graph_get
  CALL TST_Graph_map
  CALL TST_Graph_neighbors
  CALL TST_Graph_neighbors_count
  CALL TST_Topo_test
  CALL TST_Wtick
  CALL TST_Wtime
  CALL TST_Abort
  CALL TST_Add_error_class
  CALL TST_Add_error_code
  CALL TST_Add_error_string
  CALL TST_Alloc_mem
  CALL TST_Comm_call_errhandler
  CALL TST_Comm_create_errhandler
  CALL TST_Comm_get_errhandler
  CALL TST_Comm_set_errhandler
  CALL TST_Errhandler_free
  CALL TST_Error_class
  CALL TST_Error_string
  CALL TST_File_call_errhandler
  CALL TST_File_create_errhandler
  CALL TST_File_get_errhandler
  CALL TST_File_set_errhandler
  CALL TST_Finalize
  CALL TST_Finalized
  CALL TST_Free_mem
  CALL TST_Get_processor_name
  CALL TST_Get_version
  CALL TST_Init
  CALL TST_Initialized
  CALL TST_Win_call_errhandler
  CALL TST_Win_create_errhandler
  CALL TST_Win_get_errhandler
  CALL TST_Win_set_errhandler
  CALL TST_Info_create
  CALL TST_Info_delete
  CALL TST_Info_dup
  CALL TST_Info_free
  CALL TST_Info_get
  CALL TST_Info_get_nkeys
  CALL TST_Info_get_nthkey
  CALL TST_Info_get_valuelen
  CALL TST_Info_set
  CALL TST_Close_port
  CALL TST_Comm_accept
  CALL TST_Comm_connect
  CALL TST_Comm_disconnect
  CALL TST_Comm_get_parent
  CALL TST_Comm_join
  CALL TST_Comm_spawn
  CALL TST_Comm_spawn_multiple
  CALL TST_Lookup_name
  CALL TST_Open_port
  CALL TST_Publish_name
  CALL TST_Unpublish_name
  CALL TST_Accumulate
  CALL TST_Get
  CALL TST_Put
  CALL TST_Win_complete
  CALL TST_Win_create
  CALL TST_Win_fence
  CALL TST_Win_free
  CALL TST_Win_get_group
  CALL TST_Win_lock
  CALL TST_Win_post
  CALL TST_Win_start
  CALL TST_Win_test
  CALL TST_Win_unlock
  CALL TST_Win_wait
  CALL TST_Grequest_complete
  CALL TST_Grequest_start
  CALL TST_Init_thread
  CALL TST_Is_thread_main
  CALL TST_Query_thread
  CALL TST_Status_set_cancelled
  CALL TST_Status_set_elements
  CALL TST_File_close
  CALL TST_File_delete
  CALL TST_File_get_amode
  CALL TST_File_get_atomicity
  CALL TST_File_get_byte_offset
  CALL TST_File_get_group
  CALL TST_File_get_info
  CALL TST_File_get_position
  CALL TST_File_get_position_shared
  CALL TST_File_get_size
  CALL TST_File_get_type_extent
  CALL TST_File_get_view
  CALL TST_File_iread
  CALL TST_File_iread_at
  CALL TST_File_iread_shared
  CALL TST_File_iwrite
  CALL TST_File_iwrite_at
  CALL TST_File_iwrite_shared
  CALL TST_File_open
  CALL TST_File_preallocate
  CALL TST_File_read
  CALL TST_File_read_all
  CALL TST_File_read_all_begin
  CALL TST_File_read_all_end
  CALL TST_File_read_at
  CALL TST_File_read_at_all
  CALL TST_File_read_at_all_begin
  CALL TST_File_read_at_all_end
  CALL TST_File_read_ordered
  CALL TST_File_read_ordered_begin
  CALL TST_File_read_ordered_end
  CALL TST_File_read_shared
  CALL TST_File_seek
  CALL TST_File_seek_shared
  CALL TST_File_set_atomicity
  CALL TST_File_set_info
  CALL TST_File_set_size
  CALL TST_File_set_view
  CALL TST_File_sync
  CALL TST_File_write
  CALL TST_File_write_all
  CALL TST_File_write_all_begin
  CALL TST_File_write_all_end
  CALL TST_File_write_at
  CALL TST_File_write_at_all
  CALL TST_File_write_at_all_begin
  CALL TST_File_write_at_all_end
  CALL TST_File_write_ordered
  CALL TST_File_write_ordered_begin
  CALL TST_File_write_ordered_end
  CALL TST_File_write_shared
  CALL TST_Register_datarep
  CALL TST_F_sync_reg
  CALL TST_Sizeof
  CALL TST_Status_f2f08
  CALL TST_Status_f082f
  CALL TST_Type_create_f90_complex
  CALL TST_Type_create_f90_integer
  CALL TST_Type_create_f90_real
  CALL TST_Type_match_size
  CALL TST_Pcontrol
  CALL TST_Iallgather
  CALL TST_Iallgatherv
  CALL TST_Iallreduce
  CALL TST_Ialltoall
  CALL TST_Ialltoallv
  CALL TST_Ialltoallw
  CALL TST_Ibarrier
  CALL TST_Ibcast
  CALL TST_Iexscan
  CALL TST_Igather
  CALL TST_Igatherv
  CALL TST_Ireduce_scatter_block
  CALL TST_Ireduce_scatter
  CALL TST_Ireduce
  CALL TST_Iscan
  CALL TST_Iscatter
  CALL TST_Iscatterv
  CALL TST_Improbe
  CALL TST_Mprobe
  CALL TST_Mrecv
  CALL TST_Imrecv
  CALL TST_Neighbor_allgather
  CALL TST_Neighbor_allgatherv
  CALL TST_Neighbor_alltoall
  CALL TST_Neighbor_alltoallv
  CALL TST_Neighbor_alltoallw
  CALL TST_Ineighbor_allgather
  CALL TST_Ineighbor_allgatherv
  CALL TST_Ineighbor_alltoall
  CALL TST_Ineighbor_alltoallv
  CALL TST_Ineighbor_alltoallw
  CALL TST_Type_size_x
  CALL TST_Type_get_extent_x
  CALL TST_Type_get_true_extent_x
  CALL TST_Get_elements_x
  CALL TST_Status_set_elements_x
  CALL TST_Win_allocate
  CALL TST_Win_create_dynamic
  CALL TST_Win_attach
  CALL TST_Win_detach
  CALL TST_Get_accumulate
  CALL TST_Fetch_and_op
  CALL TST_Compare_and_swap
  CALL TST_Rput
  CALL TST_Rget
  CALL TST_Raccumulate
  CALL TST_Rget_accumulate
  CALL TST_Win_lock_all
  CALL TST_Win_unlock_all
  CALL TST_Win_flush
  CALL TST_Win_flush_all
  CALL TST_Win_flush_local
  CALL TST_Win_flush_local_all
  CALL TST_Win_sync
  CALL TST_Comm_split_type

END PROGRAM
