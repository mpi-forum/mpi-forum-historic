MODULE mpi_f08_types 
 IMPLICIT NONE 

 ! constants needed for type definitions, values are implementation dependant
 INTEGER MPI_ADDRESS_KIND
 PARAMETER (MPI_ADDRESS_KIND=8) 

 INTEGER MPI_OFFSET_KIND
 PARAMETER (MPI_OFFSET_KIND=8) 

 INTEGER MPI_STATUS_SIZE
 PARAMETER (MPI_STATUS_SIZE=5) 

 TYPE :: MPI_Comm
  SEQUENCE
  INTEGER :: MPI_VAL
 END TYPE MPI_Comm

 TYPE :: MPI_Datatype
  SEQUENCE
  INTEGER :: MPI_VAL
 END TYPE MPI_Datatype

 TYPE :: MPI_Errhandler
  SEQUENCE
  INTEGER :: MPI_VAL
 END TYPE MPI_Errhandler

 TYPE :: MPI_File
  SEQUENCE
  INTEGER :: MPI_VAL
 END TYPE MPI_File

 TYPE :: MPI_Group
  SEQUENCE
  INTEGER :: MPI_VAL
 END TYPE MPI_Group

 TYPE :: MPI_Info
  SEQUENCE
  INTEGER :: MPI_VAL
 END TYPE MPI_Info

 TYPE :: MPI_Op
  SEQUENCE
  INTEGER :: MPI_VAL
 END TYPE MPI_Op

 TYPE :: MPI_Request
  SEQUENCE
  INTEGER :: MPI_VAL
 END TYPE MPI_Request

 TYPE :: MPI_Status
  SEQUENCE
  INTEGER :: MPI_SOURCE
  INTEGER :: MPI_TAG
  INTEGER :: MPI_ERROR
  INTEGER :: MPI_internal_bytecnt_high ! type, name, and semantics is implement implementation dependent
  INTEGER :: MPI_internal_bytecnt_low  ! type, name, and semantics is implement implementation dependent
  INTEGER :: MPI_internal_cancal_flag  ! type, name, and semantics is implement implementation dependent
 END TYPE MPI_Status

 TYPE :: MPI_Win
  SEQUENCE
  INTEGER :: MPI_VAL
 END TYPE MPI_Win
 
END MODULE mpi_f08_types 
 
!---------------------------------------------------------------------

MODULE mpi_f08_callback_prototypes

!ABSTRACT INTERFACE
! SUBROUTINE MPI_User_function(invec, inoutvec, len, datatype)
!   USE mpi_f08_types 
!   IMPLICIT NONE 
!   <type> invec(len), inoutvec(len)
!   TYPE(MPI_Datatype) :: datatype
!   INTEGER :: len
! END SUBROUTINE
!END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Comm_copy_attr_function(oldcomm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: oldcomm
    INTEGER :: comm_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    LOGICAL flag
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Comm_delete_attr_function(comm, comm_keyval, attribute_val, extra_state, ierror)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: comm
    INTEGER :: comm_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Win_copy_attr_function(oldwin, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Win) :: oldwin
    INTEGER :: win_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    LOGICAL flag
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Win_delete_attr_function(win, win_keyval, attribute_val, extra_state, ierror)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Win) :: win
    INTEGER :: win_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Type_copy_attr_function(oldtype, type_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Datatype) :: oldtype
    INTEGER :: type_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    LOGICAL flag
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Type_delete_attr_function(datatype, type_keyval, attribute_val, extra_state, ierror)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Datatype) :: datatype
    INTEGER :: type_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Comm_errhandler_function(comm, error_code)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: comm
    INTEGER :: error_code
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Win_errhandler_function(win, error_code)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Win) :: win
    INTEGER :: error_code
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_File_errhandler_function(file, error_code)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_File) :: file
    INTEGER :: error_code
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Grequest_query_function(extra_state, status, ierror)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Status) :: status
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Grequest_free_function(extra_state, ierror)
    USE mpi_f08_types 
    IMPLICIT NONE 
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Grequest_cancel_function(extra_state, complete, ierror)
    USE mpi_f08_types 
    IMPLICIT NONE 
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
    LOGICAL complete
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Datarep_extent_function(datatype, extent, extra_state, ierror)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Datatype) :: datatype
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extent, extra_state
  END SUBROUTINE
 END INTERFACE

!ABSTRACT INTERFACE
! SUBROUTINE MPI_Datarep_conversion_function(userbuf, datatype, count, filebuf, position, extra_state, ierror)
!   USE mpi_f08_types 
!   IMPLICIT NONE 
!   <type> userbuf(*), filebuf(*)
!   TYPE(MPI_Datatype) :: datatype
!   INTEGER :: count, ierror
!   INTEGER(KIND=MPI_OFFSET_KIND) :: position
!   INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
! END SUBROUTINE
!END INTERFACE

! For deprecated routines - currently not planned for MPI-3.0

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Copy_function(oldcomm, keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: oldcomm
    INTEGER :: keyval, extra_state, attribute_val_in, attribute_val_out, ierr
    LOGICAL flag
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Delete_function(comm, keyval, attribute_val, extra_state, ierr)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: comm
    INTEGER :: keyval, attribute_val, extra_state, ierr
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Handler_function(comm, error_code)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: comm
    INTEGER :: error_code
  END SUBROUTINE
 END INTERFACE
 
END MODULE mpi_f08_callback_prototypes
 
!---------------------------------------------------------------------

MODULE mpi_f08_constants
 USE mpi_f08_types 
 IMPLICIT NONE 

! some examples, values are implementation dependant

 INTEGER, DIMENSION(1) :: MPI_BOTTOM ! special constant defined as a mudule variable

 TYPE(MPI_Comm) MPI_COMM_WORLD
 PARAMETER (MPI_COMM_WORLD = MPI_Comm(1))
 
END MODULE mpi_f08_constants
 
!---------------------------------------------------------------------
 
MODULE mpi_f08 
 USE mpi_f08_types 
 USE mpi_f08_constants
 USE mpi_f08_callback_prototypes
 IMPLICIT NONE 
 
INTERFACE
 SUBROUTINE MPI_Bsend(buf, count, datatype, dest, tag, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Bsend')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Bsend_init(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Bsend_init')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Buffer_attach(buffer, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Buffer_attach')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buffer
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
    !$PRAGMA IGNORE_TKR buffer
    !DIR$ IGNORE_TKR buffer
    !IBM* IGNORE_TKR buffer
    INTEGER, DIMENSION(*) :: buffer ! choice-dummy-argument
  INTEGER, INTENT(IN) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Buffer_detach(buffer_addr, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Buffer_detach')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buffer_addr
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer_addr
    !$PRAGMA IGNORE_TKR buffer_addr
    !DIR$ IGNORE_TKR buffer_addr
    !IBM* IGNORE_TKR buffer_addr
    INTEGER, DIMENSION(*) :: buffer_addr ! choice-dummy-argument
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cancel(request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Cancel')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(IN) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_count(status, datatype, count, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Get_count')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Status), INTENT(IN) :: status
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(OUT) :: count
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ibsend(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Ibsend')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Iprobe
 SUBROUTINE MPI_Iprobe_with_status(source, tag, comm, flag, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Iprobe_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: source, tag
  TYPE(MPI_Comm), INTENT(IN) :: comm
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Iprobe_without_status(source, tag, comm, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Iprobe_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: source, tag
  TYPE(MPI_Comm), INTENT(IN) :: comm
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Irecv')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, source, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Irsend(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Irsend')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Isend')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Issend(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Issend')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Probe
 SUBROUTINE MPI_Probe_with_status(source, tag, comm, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Probe_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: source, tag
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Probe_without_status(source, tag, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Probe_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: source, tag
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Recv
 SUBROUTINE MPI_Recv_with_status(buf, count, datatype, source, tag, comm, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Recv_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, source, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Recv_without_status(buf, count, datatype, source, tag, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Recv_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, source, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Recv_init(buf, count, datatype, source, tag, comm, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Recv_init')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, source, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Request_free(request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Request_free')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(INOUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Request_get_status
 SUBROUTINE MPI_Request_get_status_with_status( request, flag, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Request_get_status_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(IN) :: request
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Request_get_status_without_status( request, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Request_get_status_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(IN) :: request
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Rsend(buf, count, datatype, dest, tag, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Rsend')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Rsend_init(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Rsend_init')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Send(buf, count, datatype, dest, tag, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Send')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Sendrecv
 SUBROUTINE MPI_Sendrecv_with_status(sendbuf, sendcount, sendtype, dest, sendtag, &
                         recvbuf, recvcount, recvtype, source, recvtag, comm, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Sendrecv_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, dest, sendtag, recvcount, source, recvtag
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Sendrecv_without_status(sendbuf, sendcount, sendtype, dest, sendtag, &
                         recvbuf, recvcount, recvtype, source, recvtag, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Sendrecv_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, dest, sendtag, recvcount, source, recvtag
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Sendrecv_replace
 SUBROUTINE MPI_Sendrecv_replace_with_status(buf, count, datatype, dest, sendtag, source, recvtag, comm, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Sendrecv_replace_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, sendtag, source, recvtag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Sendrecv_replace_without_status(buf, count, datatype, dest, sendtag, source, recvtag, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Sendrecv_replace_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, sendtag, source, recvtag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Send_init(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Send_init')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ssend(buf, count, datatype, dest, tag, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Ssend')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ssend_init(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Ssend_init')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Start(request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Start')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(INOUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Startall(count, array_of_requests, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Startall')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Test
 SUBROUTINE MPI_Test_with_status(request, flag, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Test_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Request), INTENT(INOUT) :: request
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Test_without_status(request, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Test_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Request), INTENT(INOUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Testall
 SUBROUTINE MPI_Testall_with_status(count, array_of_requests, flag, array_of_statuses, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Testall_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(*)
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Status), INTENT(OUT) :: array_of_statuses(*) ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Testall_without_status(count, array_of_requests, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Testall_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(*)
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Testany
 SUBROUTINE MPI_Testany_with_status(count, array_of_requests, index, flag, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Testany_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(*)
  INTEGER, INTENT(OUT) :: index
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Testany_without_status(count, array_of_requests, index, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Testany_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(*)
  INTEGER, INTENT(OUT) :: index
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Testsome
 SUBROUTINE MPI_Testsome_with_status(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Testsome_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: incount
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(*)
  INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
  TYPE(MPI_Status), INTENT(OUT) :: array_of_statuses(*) ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Testsome_without_status(incount, array_of_requests, outcount, array_of_indices, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Testsome_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: incount
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(*)
  INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Test_cancelled(status, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Test_cancelled')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Status), INTENT(IN) :: status
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Wait
 SUBROUTINE MPI_Wait_with_status(request, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Wait_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(INOUT) :: request
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Wait_without_status(request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Wait_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(INOUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Waitall
 SUBROUTINE MPI_Waitall_with_status(count, array_of_requests, array_of_statuses, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Waitall_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  INTEGER, INTENT(INOUT) :: array_of_requests(*)
  TYPE(MPI_Status), INTENT(OUT) :: array_of_statuses(*) ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Waitall_without_status(count, array_of_requests, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Waitall_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  INTEGER, INTENT(INOUT) :: array_of_requests(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Waitany
 SUBROUTINE MPI_Waitany_with_status(count, array_of_requests, index, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Waitany_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(*)
  INTEGER, INTENT(OUT) :: index
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Waitany_without_status(count, array_of_requests, index, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Waitany_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(*)
  INTEGER, INTENT(OUT) :: index
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Waitsome
 SUBROUTINE MPI_Waitsome_with_status(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Waitsome_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: incount
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(*)
  INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
  TYPE(MPI_Status), INTENT(OUT) :: array_of_statuses(*) ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Waitsome_without_status(incount, array_of_requests, outcount, array_of_indices, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Waitsome_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: incount
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(*)
  INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_address(location, address, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Get_address')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: location
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: location
    !$PRAGMA IGNORE_TKR location
    !DIR$ IGNORE_TKR location
    !IBM* IGNORE_TKR location
    INTEGER, DIMENSION(*) :: location ! choice-dummy-argument
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: address
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_elements(status, datatype, count, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Get_elements')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Status), INTENT(IN) :: status
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(OUT) :: count
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Pack(inbuf, incount, datatype, outbuf, outsize, position, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Pack')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: inbuf, outbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
    !$PRAGMA IGNORE_TKR inbuf, outbuf
    !DIR$ IGNORE_TKR inbuf, outbuf
    !IBM* IGNORE_TKR inbuf, outbuf
    INTEGER, DIMENSION(*) :: inbuf, outbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: incount, outsize
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(INOUT) :: position
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Pack_external(datarep, inbuf, incount, datatype, outbuf, outsize, position, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Pack_external')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: datarep
  ! TYPE(*), DIMENSION(..) :: inbuf, outbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
    !$PRAGMA IGNORE_TKR inbuf, outbuf
    !DIR$ IGNORE_TKR inbuf, outbuf
    !IBM* IGNORE_TKR inbuf, outbuf
    INTEGER, DIMENSION(*) :: inbuf, outbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: incount
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: outsize
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(INOUT) :: position
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Pack_external_size(datarep, incount, datatype, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Pack_external_size')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: incount
  CHARACTER(LEN=*), INTENT(IN) :: datarep
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Pack_size(incount, datatype, comm, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Pack_size')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: incount
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_commit(datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_commit')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_contiguous(count, oldtype, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_contiguous')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_darray(size, rank, ndims, array_of_gsizes, array_of_distribs, &
                                   array_of_dargs, array_of_psizes, order, oldtype, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_create_darray')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: size, rank, ndims, array_of_gsizes(*), array_of_distribs(*), array_of_dargs(*), array_of_psizes(*), order
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_hindexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_create_hindexed')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, array_of_blocklengths(*)
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(*)
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_hvector(count, blocklength, stride, oldtype, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_create_hvector')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, blocklength
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: stride
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_indexed_block(count, blocklength, array_of_displacements, oldtype, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_create_indexed_block')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, blocklength, array_of_displacements(*)
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_resized(oldtype, lb, extent, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_create_resized')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: lb, extent
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_create_struct')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, array_of_blocklengths(*)
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(*)
  TYPE(MPI_Datatype), INTENT(IN) :: array_of_types(*)
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_subarray(ndims, array_of_sizes, array_of_subsizes, array_of_starts, order, oldtype, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_create_subarray')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ndims, array_of_sizes(*), array_of_subsizes(*), array_of_starts(*), order
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_dup(oldtype, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_dup')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_free(datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_free')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_contents(datatype, max_integers, max_addresses, max_datatypes, &
                                  array_of_integers, array_of_addresses, array_of_datatypes, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_get_contents')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: max_integers, max_addresses, max_datatypes
  INTEGER, INTENT(OUT) :: array_of_integers(*)
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: array_of_addresses(*)
  TYPE(MPI_Datatype), INTENT(OUT) :: array_of_datatypes(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_envelope(datatype, num_integers, num_addresses, num_datatypes, combiner, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_get_envelope')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(OUT) :: num_integers, num_addresses, num_datatypes, combiner
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_extent(datatype, lb, extent, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_get_extent')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: lb, extent
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_true_extent(datatype, true_lb, true_extent, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_get_true_extent')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: true_lb, true_extent
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_indexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_indexed')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, array_of_blocklengths(*), array_of_displacements(*)
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_size(datatype, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_size')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_vector(count, blocklength, stride, oldtype, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_vector')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, blocklength, stride
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Unpack(inbuf, insize, position, outbuf, outcount, datatype, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Unpack')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: inbuf, outbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
    !$PRAGMA IGNORE_TKR inbuf, outbuf
    !DIR$ IGNORE_TKR inbuf, outbuf
    !IBM* IGNORE_TKR inbuf, outbuf
    INTEGER, DIMENSION(*) :: inbuf, outbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: insize, outcount
  INTEGER, INTENT(INOUT) :: position
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Unpack_external(datarep, inbuf, insize, position, outbuf, outcount, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Unpack_external')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: datarep
  ! TYPE(*), DIMENSION(..) :: inbuf, outbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, outbuf
    !$PRAGMA IGNORE_TKR inbuf, outbuf
    !DIR$ IGNORE_TKR inbuf, outbuf
    !IBM* IGNORE_TKR inbuf, outbuf
    INTEGER, DIMENSION(*) :: inbuf, outbuf ! choice-dummy-argument
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: insize
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(INOUT) :: position
  INTEGER, INTENT(IN) :: outcount
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Allgather')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype
  TYPE(MPI_Datatype), INTENT(IN) :: recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Allgatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Allgatherv')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcounts(*), displs(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype
  TYPE(MPI_Datatype), INTENT(IN) :: recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Allreduce')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Alltoall')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype
  TYPE(MPI_Datatype), INTENT(IN) :: recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Alltoallv')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype
  TYPE(MPI_Datatype), INTENT(IN) :: recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Alltoallw')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtypes(*)
  TYPE(MPI_Datatype), INTENT(IN) :: recvtypes(*)
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Barrier(comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Barrier')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Bcast(buffer, count, datatype, root, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Bcast')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buffer
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
    !$PRAGMA IGNORE_TKR buffer
    !DIR$ IGNORE_TKR buffer
    !IBM* IGNORE_TKR buffer
    INTEGER, DIMENSION(*) :: buffer ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, root
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Exscan(sendbuf, recvbuf, count, datatype, op, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Exscan')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Gather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Gather')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount, root
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype
  TYPE(MPI_Datatype), INTENT(IN) :: recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Gatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Gatherv')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcounts(*), displs(*), root
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype
  TYPE(MPI_Datatype), INTENT(IN) :: recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Op_commutative(op, commute, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Op_commutative')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Op), INTENT(IN) :: op
  LOGICAL, INTENT(OUT) :: commute
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Op_create(user_fn, commute, op, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Op_create')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  EXTERNAL :: user_fn ! used because of choice buffer arguments
  ! PROCEDURE(MPI_User_function) :: user_fn
  LOGICAL, INTENT(IN) :: commute
  TYPE(MPI_Op), INTENT(OUT) :: op
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Op_free(op, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Op_free')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Op), INTENT(INOUT) :: op
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Reduce')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, root
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Reduce_local(inbuf, inoutbuf, count, datatype, op, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Reduce_local')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: inbuf, inoutbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf, inoutbuf
    !$PRAGMA IGNORE_TKR inbuf, inoutbuf
    !DIR$ IGNORE_TKR inbuf, inoutbuf
    !IBM* IGNORE_TKR inbuf, inoutbuf
    INTEGER, DIMENSION(*) :: inbuf, inoutbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Reduce_scatter(sendbuf, recvbuf, recvcounts, datatype, op, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Reduce_scatter')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: recvcounts(*)
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Reduce_scatter_block(sendbuf, recvbuf, recvcount, datatype, op, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Reduce_scatter_block')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Scan(sendbuf, recvbuf, count, datatype, op, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Scan')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Scatter(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Scatter')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount, root
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype
  TYPE(MPI_Datatype), INTENT(IN) :: recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Scatterv(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Scatterv')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: sendbuf, recvbuf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf, recvbuf
    !$PRAGMA IGNORE_TKR sendbuf, recvbuf
    !DIR$ IGNORE_TKR sendbuf, recvbuf
    !IBM* IGNORE_TKR sendbuf, recvbuf
    INTEGER, DIMENSION(*) :: sendbuf, recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcounts(*), displs(*), recvcount, root
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype
  TYPE(MPI_Datatype), INTENT(IN) :: recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_compare(comm1, comm2, result, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_compare')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm1
  TYPE(MPI_Comm), INTENT(IN) :: comm2
  INTEGER, INTENT(OUT) :: result
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_create(comm, group, newcomm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_create')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Group), INTENT(IN) :: group
  TYPE(MPI_Comm), INTENT(OUT) :: newcomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_create_keyval(comm_copy_attr_fn, comm_delete_attr_fn, comm_keyval, extra_state, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_create_keyval')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  ! EXTERNAL :: comm_copy_attr_fn
  PROCEDURE(MPI_Comm_copy_attr_function) :: comm_copy_attr_fn
  ! EXTERNAL :: comm_delete_attr_fn
  PROCEDURE(MPI_Comm_delete_attr_function) :: comm_delete_attr_fn
  INTEGER, INTENT(OUT) :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_delete_attr(comm, comm_keyval, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_delete_attr')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: comm_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_dup(comm, newcomm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_dup')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Comm), INTENT(OUT) :: newcomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_COMM_DUP_FN(oldcomm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_COMM_DUP_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: oldcomm
  INTEGER, INTENT(IN) :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val_out
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_free(comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_free')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(INOUT) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_free_keyval(comm_keyval, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_free_keyval')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: comm_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_get_attr(comm, comm_keyval, attribute_val, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_get_attr')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_get_name(comm, comm_name, resultlen, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_get_name')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  CHARACTER(LEN=*), INTENT(OUT) :: comm_name
  INTEGER, INTENT(OUT) :: resultlen
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_group(comm, group, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_group')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Group), INTENT(OUT) :: group
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_COMM_NULL_COPY_FN(oldcomm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_COMM_NULL_COPY_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: oldcomm
  INTEGER, INTENT(IN) :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val_out
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_COMM_NULL_DELETE_FN(comm, comm_keyval, attribute_val, extra_state, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_COMM_NULL_DELETE_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val, extra_state
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_rank(comm, rank, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_rank')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: rank
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_remote_group(comm, group, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_remote_group')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Group), INTENT(OUT) :: group
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_remote_size(comm, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_remote_size')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_set_attr(comm, comm_keyval, attribute_val, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_set_attr')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: comm_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_set_name(comm, comm_name, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_set_name')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  CHARACTER(LEN=*), INTENT(IN) :: comm_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_size(comm, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_size')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_split(comm, color, key, newcomm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_split')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: color, key
  TYPE(MPI_Comm), INTENT(OUT) :: newcomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_test_inter(comm, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_test_inter')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_compare(group1, group2, result, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_compare')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group1, group2
  INTEGER, INTENT(OUT) :: result
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_difference(group1, group2, newgroup, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_difference')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group1, group2
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_excl(group, n, ranks, newgroup, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_excl')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(IN) :: n, ranks(*)
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_free(group, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_free')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(INOUT) :: group
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_incl(group, n, ranks, newgroup, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_incl')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n, ranks(*)
  TYPE(MPI_Group), INTENT(IN) :: group
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_intersection(group1, group2, newgroup, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_intersection')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group1, group2
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_range_excl(group, n, ranges, newgroup, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_range_excl')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(IN) :: n, ranges(3,*)
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_range_incl(group, n, ranges, newgroup, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_range_incl')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(IN) :: n, ranges(3,*)
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_rank(group, rank, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_rank')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(OUT) :: rank
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_size(group, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_size')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_translate_ranks(group1, n, ranks1, group2, ranks2, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_translate_ranks')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group1, group2
  INTEGER, INTENT(IN) :: n, ranks1(*)
  INTEGER, INTENT(OUT) :: ranks2(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_union(group1, group2, newgroup, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Group_union')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group1, group2
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Intercomm_create(local_comm, local_leader, peer_comm, remote_leader, tag, newintercomm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Intercomm_create')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: local_comm, peer_comm
  INTEGER, INTENT(IN) :: local_leader, remote_leader, tag
  TYPE(MPI_Comm), INTENT(OUT) :: newintercomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Intercomm_merge(intercomm, high, newintracomm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Intercomm_merge')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: intercomm
  LOGICAL, INTENT(IN) :: high
  TYPE(MPI_Comm), INTENT(OUT) :: newintracomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_keyval(type_copy_attr_fn, type_delete_attr_fn, type_keyval, extra_state, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_create_keyval')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  ! EXTERNAL :: type_copy_attr_fn
  PROCEDURE(MPI_Type_copy_attr_function) :: type_copy_attr_fn
  ! EXTERNAL :: type_delete_attr_fn
  PROCEDURE(MPI_Type_delete_attr_function) :: type_delete_attr_fn
  INTEGER, INTENT(OUT) :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_delete_attr(datatype, type_keyval, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_delete_attr')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: type_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_TYPE_DUP_FN(oldtype, type_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_TYPE_DUP_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  INTEGER, INTENT(IN) :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val_out
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_free_keyval(type_keyval, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_free_keyval')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: type_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_attr(datatype, type_keyval, attribute_val, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_get_attr')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_name(datatype, type_name, resultlen, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_get_name')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  CHARACTER(LEN=*), INTENT(OUT) :: type_name
  INTEGER, INTENT(OUT) :: resultlen
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_TYPE_NULL_COPY_FN(oldtype, type_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_TYPE_NULL_COPY_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  INTEGER, INTENT(IN) :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val_out
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_TYPE_NULL_DELETE_FN(datatype, type_keyval, attribute_val, extra_state, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_TYPE_NULL_DELETE_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val, extra_state
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_set_attr(datatype, type_keyval, attribute_val, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_set_attr')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: type_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_set_name(datatype, type_name, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_set_name')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  CHARACTER(LEN=*), INTENT(IN) :: type_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_create_keyval(win_copy_attr_fn, win_delete_attr_fn, win_keyval, extra_state, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_create_keyval')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  ! EXTERNAL :: win_copy_attr_fn
  PROCEDURE(MPI_Win_copy_attr_function) :: win_copy_attr_fn
  ! EXTERNAL :: win_delete_attr_fn
  PROCEDURE(MPI_Win_delete_attr_function) :: win_delete_attr_fn
  INTEGER, INTENT(OUT) :: win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_delete_attr(win, win_keyval, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_delete_attr')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, INTENT(IN) :: win_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_WIN_DUP_FN(oldwin, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_WIN_DUP_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: oldwin, win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val_out
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_free_keyval(win_keyval, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_free_keyval')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: win_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_get_attr(win, win_keyval, attribute_val, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_get_attr')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, INTENT(IN) :: win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_get_name(win, win_name, resultlen, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_get_name')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  CHARACTER(LEN=*), INTENT(OUT) :: win_name
  INTEGER, INTENT(OUT) :: resultlen
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_WIN_NULL_COPY_FN(oldwin, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_WIN_NULL_COPY_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: oldwin, win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state, attribute_val_in
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: attribute_val_out
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_WIN_NULL_DELETE_FN(win, win_keyval, attribute_val, extra_state, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_WIN_NULL_DELETE_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, INTENT(IN) :: win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val, extra_state
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_set_attr(win, win_keyval, attribute_val, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_set_attr')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, INTENT(IN) :: win_keyval
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: attribute_val
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_set_name(win, win_name, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_set_name')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  CHARACTER(LEN=*), INTENT(IN) :: win_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cartdim_get(comm, ndims, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Cartdim_get')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: ndims
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_coords(comm, rank, maxdims, coords, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Cart_coords')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: rank, maxdims
  INTEGER, INTENT(OUT) :: coords(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_create(comm_old, ndims, dims, periods, reorder, comm_cart, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Cart_create')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: ndims, dims(*)
  LOGICAL, INTENT(IN) :: periods(*), reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_cart
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_get(comm, maxdims, dims, periods, coords, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Cart_get')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: maxdims
  INTEGER, INTENT(OUT) :: dims(*), coords(*)
  LOGICAL, INTENT(OUT) :: periods(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_map(comm, ndims, dims, periods, newrank, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Cart_map')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: ndims, dims(*)
  LOGICAL, INTENT(IN) :: periods(*)
  INTEGER, INTENT(OUT) :: newrank
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_rank(comm, coords, rank, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Cart_rank')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: coords(*)
  INTEGER, INTENT(OUT) :: rank
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_shift(comm, direction, disp, rank_source, rank_dest, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Cart_shift')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: direction, disp
  INTEGER, INTENT(OUT) :: rank_source, rank_dest
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_sub(comm, remain_dims, newcomm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Cart_sub')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  LOGICAL, INTENT(IN) :: remain_dims(*)
  TYPE(MPI_Comm), INTENT(OUT) :: newcomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Dims_create(nnodes, ndims, dims, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dims_create')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: nnodes, ndims
  INTEGER, INTENT(INOUT) :: dims(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Dist_graph_create
 SUBROUTINE MPI_Dist_graph_create_with_weights(comm_old, n, sources, degrees, destinations, weights, &
                                               info, reorder, comm_dist_graph, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dist_graph_create_with_weights')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: n, sources(*), degrees(*), destinations(*)
  INTEGER, INTENT(IN) :: weights(*) ! optional by overloading
  TYPE(MPI_Info), INTENT(IN) :: info
  LOGICAL, INTENT(IN) :: reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Dist_graph_create_without_weights(comm_old, n, sources, degrees, destinations, &
                                                  info, reorder, comm_dist_graph, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dist_graph_create_without_weights')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: n, sources(*), degrees(*), destinations(*)
  TYPE(MPI_Info), INTENT(IN) :: info
  LOGICAL, INTENT(IN) :: reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Dist_graph_create_adjacent
 SUBROUTINE MPI_Dist_graph_create_adjacent_with_weights(comm_old, indegree, sources, sourceweights, &
                                           outdegree, destinations, destweights, info, reorder, comm_dist_graph, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dist_graph_create_adjacent_with_weights')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: indegree, sources(*), outdegree, destinations(*)
  INTEGER, INTENT(IN) :: sourceweights(*) ! optional by overloading
  INTEGER, INTENT(IN) :: destweights(*) ! optional by overloading
  TYPE(MPI_Info), INTENT(IN) :: info
  LOGICAL, INTENT(IN) :: reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Dist_graph_create_adjacent_without_sourceweights(comm_old, indegree, sources, &
                                           outdegree, destinations, destweights, info, reorder, comm_dist_graph, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dist_graph_create_adjacent_without_sourceweights')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: indegree, sources(*), outdegree, destinations(*)
  INTEGER, INTENT(IN) :: destweights(*) ! optional by overloading
  TYPE(MPI_Info), INTENT(IN) :: info
  LOGICAL, INTENT(IN) :: reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Dist_graph_create_adjacent_without_destweights(comm_old, indegree, sources, sourceweights, &
                                           outdegree, destinations, info, reorder, comm_dist_graph, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dist_graph_create_adjacent_without_destweights')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: indegree, sources(*), outdegree, destinations(*)
  INTEGER, INTENT(IN) :: sourceweights(*) ! optional by overloading
  TYPE(MPI_Info), INTENT(IN) :: info
  LOGICAL, INTENT(IN) :: reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Dist_graph_create_adjacent_without_weights(comm_old, indegree, sources, &
                                           outdegree, destinations, info, reorder, comm_dist_graph, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dist_graph_create_adjacent_without_weights')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: indegree, sources(*), outdegree, destinations(*)
  TYPE(MPI_Info), INTENT(IN) :: info
  LOGICAL, INTENT(IN) :: reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Dist_graph_neighbors
 SUBROUTINE MPI_Dist_graph_neighbors_with_weights(comm, maxindegree, sources, sourceweights, &
                                                  maxoutdegree, destinations, destweights, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dist_graph_neighbors_with_weights')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: maxindegree, maxoutdegree
  INTEGER, INTENT(OUT) :: sources(*), destinations(*)
  INTEGER, INTENT(OUT) :: sourceweights(*) ! optional by overloading
  INTEGER, INTENT(OUT) :: destweights(*) ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Dist_graph_neighbors_without_sourceweights(comm, maxindegree, sources, &
                                                           maxoutdegree, destinations, destweights, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dist_graph_neighbors_without_sourceweights')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: maxindegree, maxoutdegree
  INTEGER, INTENT(OUT) :: sources(*), destinations(*)
  INTEGER, INTENT(OUT) :: destweights(*) ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Dist_graph_neighbors_without_destweights(comm, maxindegree, sources, sourceweights, &
                                                         maxoutdegree, destinations, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dist_graph_neighbors_without_destweights')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: maxindegree, maxoutdegree
  INTEGER, INTENT(OUT) :: sources(*), destinations(*)
  INTEGER, INTENT(OUT) :: sourceweights(*) ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Dist_graph_neighbors_without_weights(comm, maxindegree, sources, &
                                                     maxoutdegree, destinations, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dist_graph_neighbors_without_weights')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: maxindegree, maxoutdegree
  INTEGER, INTENT(OUT) :: sources(*), destinations(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Dist_graph_neighbors_count(comm, indegree, outdegree, weighted, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Dist_graph_neighbors_count')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: indegree, outdegree
  LOGICAL, INTENT(OUT) :: weighted
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Graphdims_get(comm, nnodes, nedges, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Graphdims_get')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: nnodes, nedges
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Graph_create(comm_old, nnodes, index, edges, reorder, comm_graph, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Graph_create')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: nnodes, index(*), edges(*)
  LOGICAL, INTENT(IN) :: reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_graph
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Graph_get(comm, maxindex, maxedges, index, edges, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Graph_get')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: maxindex, maxedges
  INTEGER, INTENT(OUT) :: index(*), edges(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Graph_map(comm, nnodes, index, edges, newrank, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Graph_map')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: nnodes, index(*), edges(*)
  INTEGER, INTENT(OUT) :: newrank
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Graph_neighbors(comm, rank, maxneighbors, neighbors, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Graph_neighbors')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: rank, maxneighbors
  INTEGER, INTENT(OUT) :: neighbors(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Graph_neighbors_count(comm, rank, nneighbors, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Graph_neighbors_count')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: rank
  INTEGER, INTENT(OUT) :: nneighbors
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Topo_test(comm, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Topo_test')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 DOUBLE PRECISION FUNCTION MPI_Wtick(  &
 &)  ! BIND(C,NAME='MPI_F08_Wtick')
  USE mpi_f08_types
  IMPLICIT NONE
 END FUNCTION 
END INTERFACE

INTERFACE
 DOUBLE PRECISION FUNCTION MPI_Wtime(  &
 &)  ! BIND(C,NAME='MPI_F08_Wtime')
  USE mpi_f08_types
  IMPLICIT NONE
 END FUNCTION
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Abort(comm, errorcode, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Abort')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: errorcode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Add_error_class(errorclass, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Add_error_class')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: errorclass
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Add_error_code(errorclass, errorcode, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Add_error_code')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: errorclass
  INTEGER, INTENT(OUT) :: errorcode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Add_error_string(errorcode, string, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Add_error_string')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: errorcode
  CHARACTER(LEN=*), INTENT(IN) :: string
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Alloc_mem
 SUBROUTINE MPI_Alloc_mem_with_cptr(size, info, baseptr, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Alloc_mem_with_cptr')
  USE mpi_f08_types
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(C_PTR), INTENT(OUT) :: baseptr
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Alloc_mem_with_aint(size, info, baseptr, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Alloc_mem_with_aint')
  USE mpi_f08_types
  USE, INTRINSIC :: ISO_C_BINDING
  IMPLICIT NONE
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: baseptr
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_call_errhandler(comm, errorcode, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_call_errhandler')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: errorcode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_create_errhandler(comm_errhandler_fn, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_create_errhandler')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  ! EXTERNAL :: comm_errhandler_fn
  PROCEDURE(MPI_Comm_errhandler_function) :: comm_errhandler_fn
  TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_get_errhandler(comm, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_get_errhandler')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_set_errhandler(comm, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_set_errhandler')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Errhandler_free(errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Errhandler_free')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Errhandler), INTENT(INOUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Error_class(errorcode, errorclass, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Error_class')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: errorcode
  INTEGER, INTENT(OUT) :: errorclass
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Error_string(errorcode, string, resultlen, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Error_string')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: errorcode
  CHARACTER(LEN=*), INTENT(OUT) :: string
  INTEGER, INTENT(OUT) :: resultlen
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_call_errhandler(fh, errorcode, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_call_errhandler')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER, INTENT(IN) :: errorcode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_create_errhandler(file_errhandler_fn, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_create_errhandler')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  ! EXTERNAL :: file_errhandler_fn
  PROCEDURE(MPI_File_errhandler_function) :: file_errhandler_fn
  TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_errhandler(file, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_get_errhandler')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: file
  TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_set_errhandler(file, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_set_errhandler')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: file
  TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Finalize(ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Finalize')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Finalized(flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Finalized')
  USE mpi_f08_types
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Free_mem(base, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Free_mem')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: base
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: base
    !$PRAGMA IGNORE_TKR base
    !DIR$ IGNORE_TKR base
    !IBM* IGNORE_TKR base
    INTEGER, DIMENSION(*) :: base ! choice-dummy-argument
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_processor_name( name, resultlen, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Get_processor_name')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(OUT) :: name
  INTEGER, INTENT(OUT) :: resultlen
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_version(version, subversion, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Get_version')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: version, subversion
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Init(ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Init')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Initialized(flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Initialized')
  USE mpi_f08_types
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_call_errhandler(win, errorcode, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_call_errhandler')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, INTENT(IN) :: errorcode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_create_errhandler(win_errhandler_fn, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_create_errhandler')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  ! EXTERNAL :: win_errhandler_fn
  PROCEDURE(MPI_Win_errhandler_function) :: win_errhandler_fn
  TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_get_errhandler(win, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_get_errhandler')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_set_errhandler(win, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_set_errhandler')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_create(info, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Info_create')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(OUT) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_delete(info, key, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Info_delete')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=*), INTENT(IN) :: key
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_dup(info, newinfo, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Info_dup')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(MPI_Info), INTENT(OUT) :: newinfo
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_free(info, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Info_free')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(INOUT) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_get(info, key, valuelen, value, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Info_get')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=*), INTENT(IN) :: key
  INTEGER, INTENT(IN) :: valuelen
  CHARACTER(LEN=*), INTENT(OUT) :: value
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_get_nkeys(info, nkeys, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Info_get_nkeys')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, INTENT(OUT) :: nkeys
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_get_nthkey(info, n, key, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Info_get_nthkey')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, INTENT(IN) :: n
  CHARACTER(LEN=*), INTENT(OUT) :: key
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_get_valuelen(info, key, valuelen, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Info_get_valuelen')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=*), INTENT(IN) :: key
  INTEGER, INTENT(OUT) :: valuelen
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_set(info, key, value, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Info_set')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=*), INTENT(IN) :: key, value
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Close_port(port_name, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Close_port')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: port_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_accept(port_name, info, root, comm, newcomm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_accept')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: port_name
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, INTENT(IN) :: root
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Comm), INTENT(OUT) :: newcomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_connect(port_name, info, root, comm, newcomm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_connect')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: port_name
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, INTENT(IN) :: root
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Comm), INTENT(OUT) :: newcomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_disconnect(comm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_disconnect')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(INOUT) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_get_parent(parent, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_get_parent')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(OUT) :: parent
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_join(fd, intercomm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_join')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: fd
  TYPE(MPI_Comm), INTENT(OUT) :: intercomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Comm_spawn
 SUBROUTINE MPI_Comm_spawn_with_errcodes(command, argv, maxprocs, info, root, comm, intercomm, array_of_errcodes, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_spawn_with_errcodes')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: command, argv(*)
  INTEGER, INTENT(IN) :: maxprocs, root
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Comm), INTENT(OUT) :: intercomm
  INTEGER, INTENT(OUT) :: array_of_errcodes(*) ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Comm_spawn_without_errcodes(command, argv, maxprocs, info, root, comm, intercomm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_spawn_without_errcodes')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: command, argv(*)
  INTEGER, INTENT(IN) :: maxprocs, root
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Comm), INTENT(OUT) :: intercomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_Comm_spawn_multiple
 SUBROUTINE MPI_Comm_spawn_multiple_with_errcodes(count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, &
                                    root, comm, intercomm, array_of_errcodes, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_spawn_multiple_with_errcodes')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, array_of_maxprocs(*), root
  CHARACTER(LEN=*), INTENT(IN) :: array_of_commands(*), array_of_argv(count, *)
  TYPE(MPI_Info), INTENT(IN) :: array_of_info(*)
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Comm), INTENT(OUT) :: intercomm
  INTEGER, INTENT(OUT) :: array_of_errcodes(*) ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_Comm_spawn_multiple_without_errcodes(count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, &
                                    root, comm, intercomm, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Comm_spawn_multiple_without_errcodes')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, array_of_maxprocs(*), root
  CHARACTER(LEN=*), INTENT(IN) :: array_of_commands(*), array_of_argv(count, *)
  TYPE(MPI_Info), INTENT(IN) :: array_of_info(*)
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Comm), INTENT(OUT) :: intercomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Lookup_name(service_name, info, port_name, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Lookup_name')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: service_name
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=*), INTENT(OUT) :: port_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Open_port(info, port_name, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Open_port')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=*), INTENT(OUT) :: port_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Publish_name(service_name, info, port_name, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Publish_name')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=*), INTENT(IN) :: service_name, port_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Unpublish_name(service_name, info, port_name, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Unpublish_name')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: service_name, port_name
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Accumulate(origin_addr, origin_count, origin_datatype, &
                           target_rank, target_disp, target_count, target_datatype, op, win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Accumulate')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: origin_addr
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
    !$PRAGMA IGNORE_TKR origin_addr
    !DIR$ IGNORE_TKR origin_addr
    !IBM* IGNORE_TKR origin_addr
    INTEGER, DIMENSION(*) :: origin_addr ! choice-dummy-argument
  INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get(origin_addr, origin_count, origin_datatype, &
                    target_rank, target_disp, target_count, target_datatype, win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Get')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: origin_addr
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
    !$PRAGMA IGNORE_TKR origin_addr
    !DIR$ IGNORE_TKR origin_addr
    !IBM* IGNORE_TKR origin_addr
    INTEGER, DIMENSION(*) :: origin_addr ! choice-dummy-argument
  INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Put(origin_addr, origin_count, origin_datatype, &
                    target_rank, target_disp, target_count, target_datatype, win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Put')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: origin_addr
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
    !$PRAGMA IGNORE_TKR origin_addr
    !DIR$ IGNORE_TKR origin_addr
    !IBM* IGNORE_TKR origin_addr
    INTEGER, DIMENSION(*) :: origin_addr ! choice-dummy-argument
  INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Datatype), INTENT(IN) :: target_datatype
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_complete(win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_complete')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_create(base, size, disp_unit, info, comm, win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_create')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: base
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: base
    !$PRAGMA IGNORE_TKR base
    !DIR$ IGNORE_TKR base
    !IBM* IGNORE_TKR base
    INTEGER, DIMENSION(*) :: base ! choice-dummy-argument
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
  INTEGER, INTENT(IN) :: disp_unit
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Win), INTENT(OUT) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_fence(assert, win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_fence')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: assert
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_free(win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_free')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(INOUT) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_get_group(win, group, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_get_group')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(MPI_Group), INTENT(OUT) :: group
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_lock(lock_type, rank, assert, win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_lock')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: lock_type, rank, assert
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_post(group, assert, win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_post')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(IN) :: assert
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_start(group, assert, win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_start')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(IN) :: assert
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_test(win, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_test')
  USE mpi_f08_types
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_unlock(rank, win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_unlock')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: rank
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_wait(win, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Win_wait')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Grequest_complete(request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Grequest_complete')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(IN) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Grequest_start(query_fn, free_fn, cancel_fn, extra_state, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Grequest_start')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  ! EXTERNAL :: query_fn
  PROCEDURE(MPI_Grequest_query_function) :: query_fn
  ! EXTERNAL :: free_fn
  PROCEDURE(MPI_Grequest_free_function) :: free_fn
  ! EXTERNAL :: cancel_fn
  PROCEDURE(MPI_Grequest_cancel_function) :: cancel_fn
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Init_thread(required, provided, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Init_thread')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: required
  INTEGER, INTENT(OUT) :: provided
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Is_thread_main(flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Is_thread_main')
  USE mpi_f08_types
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Query_thread(provided, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Query_thread')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: provided
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Status_set_cancelled(status, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Status_set_cancelled')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Status), INTENT(INOUT) :: status
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Status_set_elements(status, datatype, count, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Status_set_elements')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Status), INTENT(INOUT) :: status
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: count
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_close(fh, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_close')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(INOUT) :: fh
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_delete(filename, info, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_delete')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: filename
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_amode(fh, amode, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_get_amode')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER, INTENT(OUT) :: amode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_atomicity(fh, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_get_atomicity')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_byte_offset(fh, offset, disp, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_get_byte_offset')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: disp
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_group(fh, group, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_get_group')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(MPI_Group), INTENT(OUT) :: group
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_info(fh, info_used, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_get_info')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(MPI_Info), INTENT(OUT) :: info_used
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_position(fh, offset, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_get_position')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: offset
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_position_shared(fh, offset, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_get_position_shared')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: offset
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_size(fh, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_get_size')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_type_extent(fh, datatype, extent, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_get_type_extent')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: extent
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_view(fh, disp, etype, filetype, datarep, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_get_view')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: disp
  TYPE(MPI_Datatype), INTENT(OUT) :: etype
  TYPE(MPI_Datatype), INTENT(OUT) :: filetype
  CHARACTER(LEN=*), INTENT(OUT) :: datarep
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iread(fh, buf, count, datatype, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_iread')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iread_at(fh, offset, buf, count, datatype, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_iread_at')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iread_shared(fh, buf, count, datatype, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_iread_shared')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iwrite(fh, buf, count, datatype, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_iwrite')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iwrite_at(fh, offset, buf, count, datatype, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_iwrite_at')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iwrite_shared(fh, buf, count, datatype, request, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_iwrite_shared')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_open(comm, filename, amode, info, fh, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_open')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  CHARACTER(LEN=*), INTENT(IN) :: filename
  INTEGER, INTENT(IN) :: amode
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(MPI_File), INTENT(OUT) :: fh
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_preallocate(fh, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_preallocate')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_read
 SUBROUTINE MPI_File_read_with_status(fh, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_read_without_status(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_read_all
 SUBROUTINE MPI_File_read_all_with_status(fh, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_all_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_read_all_without_status(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_all_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_all_begin(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_all_begin')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_read_all_end
 SUBROUTINE MPI_File_read_all_end_with_status(fh, buf, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_all_end_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_read_all_end_without_status(fh, buf, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_all_end_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_read_at
 SUBROUTINE MPI_File_read_at_with_status(fh, offset, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_at_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_read_at_without_status(fh, offset, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_at_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_read_at_all
 SUBROUTINE MPI_File_read_at_all_with_status(fh, offset, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_at_all_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_read_at_all_without_status(fh, offset, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_at_all_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_at_all_begin(fh, offset, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_at_all_begin')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_read_at_all_end
 SUBROUTINE MPI_File_read_at_all_end_with_status(fh, buf, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_at_all_end_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_read_at_all_end_without_status(fh, buf, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_at_all_end_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_read_ordered
 SUBROUTINE MPI_File_read_ordered_with_status_with_status(fh, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_ordered_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_read_ordered_without_status(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_ordered_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_ordered_begin(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_ordered_begin')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_read_ordered_end
 SUBROUTINE MPI_File_read_ordered_end_with_status(fh, buf, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_ordered_end_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_read_ordered_end_without_status(fh, buf, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_ordered_end_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_read_shared
 SUBROUTINE MPI_File_read_shared_with_status(fh, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_shared_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_read_shared_without_status(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_read_shared_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_seek(fh, offset, whence, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_seek')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  INTEGER, INTENT(IN) :: whence
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_seek_shared(fh, offset, whence, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_seek_shared')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  INTEGER, INTENT(IN) :: whence
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_set_atomicity(fh, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_set_atomicity')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  LOGICAL, INTENT(IN) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_set_info(fh, info, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_set_info')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_set_size(fh, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_set_size')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_set_view(fh, disp, etype, filetype, datarep, info, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_set_view')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: disp
  TYPE(MPI_Datatype), INTENT(IN) :: etype
  TYPE(MPI_Datatype), INTENT(IN) :: filetype
  CHARACTER(LEN=*), INTENT(IN) :: datarep
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_sync(fh, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_sync')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_write
 SUBROUTINE MPI_File_write_with_status(fh, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_write_without_status(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_write_all
 SUBROUTINE MPI_File_write_all_with_status(fh, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_all_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_write_all_without_status(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_all_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_all_begin(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_all_begin')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_write_all_end
 SUBROUTINE MPI_File_write_all_end_with_status(fh, buf, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_all_end_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_write_all_end_without_status(fh, buf, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_all_end_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_write_at
 SUBROUTINE MPI_File_write_at_with_status(fh, offset, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_at_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_write_at_without_status(fh, offset, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_at_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_write_at_all
 SUBROUTINE MPI_File_write_at_all_with_status(fh, offset, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_at_all_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_write_at_all_without_status(fh, offset, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_at_all_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_at_all_begin(fh, offset, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_at_all_begin')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_write_at_all_end
 SUBROUTINE MPI_File_write_at_all_end_with_status(fh, buf, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_at_all_end_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_write_at_all_end_without_status(fh, buf, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_at_all_end_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_write_ordered
 SUBROUTINE MPI_File_write_ordered_with_status(fh, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_ordered_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_write_ordered_without_status(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_ordered_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_ordered_begin(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_ordered_begin')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_write_ordered_end
 SUBROUTINE MPI_File_write_ordered_end_with_status(fh, buf, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_ordered_end_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_write_ordered_end_without_status(fh, buf, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_ordered_end_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE MPI_File_write_shared
 SUBROUTINE MPI_File_write_shared_with_status(fh, buf, count, datatype, status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_shared_with_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status), INTENT(OUT) :: status  ! optional by overloading
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
 SUBROUTINE MPI_File_write_shared_without_status(fh, buf, count, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_File_write_shared_without_status')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Register_datarep(datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Register_datarep')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: datarep
  EXTERNAL :: read_conversion_fn ! used because of choice buffer arguments
  ! PROCEDURE(MPI_Datarep_conversion_function) :: read_conversion_fn
  EXTERNAL :: write_conversion_fn ! used because of choice buffer arguments
  ! PROCEDURE(MPI_Datarep_conversion_function) :: write_conversion_fn
  ! EXTERNAL :: dtype_file_extent_fn
  PROCEDURE(MPI_Datarep_extent_function) :: dtype_file_extent_fn
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_F_sync_reg(buf  &
 &)  ! BIND(C,NAME='MPI_F08_F_sync_reg')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: buf
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
    !$PRAGMA IGNORE_TKR buf
    !DIR$ IGNORE_TKR buf
    !IBM* IGNORE_TKR buf
    INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Sizeof(x, size, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Sizeof')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*) :: x
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: x
    !$PRAGMA IGNORE_TKR x
    !DIR$ IGNORE_TKR x
    !IBM* IGNORE_TKR x
    INTEGER :: x ! choice-dummy-argument
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Status_f2f08(f_status, f08_status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Status_f2f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: f_status(MPI_STATUS_SIZE)
  TYPE(MPI_Status), INTENT(OUT) :: f08_status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror 
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Status_f082f(f08_status, f_status, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Status_f082f')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Status), INTENT(IN) :: f08_status
  INTEGER, INTENT(OUT) :: f_status(MPI_STATUS_SIZE)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_f90_complex(p, r, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_create_f90_complex')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: p, r
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_f90_integer(r, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_create_f90_integer')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: r
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_f90_real(p, r, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_create_f90_real')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: p, r
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_match_size(typeclass, size, datatype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_match_size')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: typeclass, size
  TYPE(MPI_Datatype), INTENT(OUT) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Pcontrol(level  &
 &)  ! BIND(C,NAME='MPI_F08_Pcontrol')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: level
 END SUBROUTINE
END INTERFACE

! Deprecated routines - currently not planned for MPI-3.0
 
INTERFACE
 SUBROUTINE MPI_Address(location, address, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Address')
  USE mpi_f08_types
  IMPLICIT NONE
  ! TYPE(*), DIMENSION(..) :: location
    !DEC$ ATTRIBUTES NO_ARG_CHECK :: location
    !$PRAGMA IGNORE_TKR location
    !DIR$ IGNORE_TKR location
    !IBM* IGNORE_TKR location
    INTEGER, DIMENSION(*) :: location ! choice-dummy-argument
  INTEGER, INTENT(OUT) :: address
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Attr_delete(comm, keyval, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Attr_delete')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Attr_get(comm, keyval, attribute_val, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Attr_get')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: keyval
  INTEGER, INTENT(OUT) :: attribute_val
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Attr_put(comm, keyval, attribute_val, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Attr_put')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: keyval, attribute_val
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_DUP_FN(oldcomm, keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_DUP_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: oldcomm
  INTEGER, INTENT(IN) :: keyval, extra_state, attribute_val_in
  INTEGER, INTENT(OUT) :: attribute_val_out
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Errhandler_create(handler_fn, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Errhandler_create')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  ! EXTERNAL :: handler_fn
  PROCEDURE(MPI_Handler_function) :: handler_fn
  TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Errhandler_get(comm, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Errhandler_get')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Errhandler_set(comm, errhandler, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Errhandler_set')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Keyval_create(copy_fn, delete_fn, keyval, extra_state, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Keyval_create')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  ! EXTERNAL :: copy_fn
  PROCEDURE(MPI_Copy_function) :: copy_fn
  ! EXTERNAL :: delete_fn
  PROCEDURE(MPI_Delete_function) :: delete_fn
  INTEGER, INTENT(OUT) :: keyval
  INTEGER, INTENT(IN) :: extra_state
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Keyval_free(keyval, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Keyval_free')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_NULL_COPY_FN(oldcomm, keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_NULL_COPY_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: oldcomm
  INTEGER, INTENT(IN) :: keyval, extra_state, attribute_val_in
  INTEGER, INTENT(OUT) :: attribute_val_out
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_NULL_DELETE_FN(comm, keyval, attribute_val, extra_state, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_NULL_DELETE_FN')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: keyval, attribute_val, extra_state
  INTEGER, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_extent(datatype, extent, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_extent')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(OUT) :: extent
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_hindexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_hindexed')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, array_of_blocklengths(*), array_of_displacements(*)
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_hvector(count, blocklength, stride, oldtype, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_hvector')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, blocklength, stride
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_lb( datatype, displacement, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_lb')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(OUT) :: displacement
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_struct')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, array_of_blocklengths(*), array_of_displacements(*)
  TYPE(MPI_Datatype), INTENT(IN) :: array_of_types(*)
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_ub( datatype, displacement, ierror  &
 &)  ! BIND(C,NAME='MPI_F08_Type_ub')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(OUT) :: displacement
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

END MODULE mpi_f08 
