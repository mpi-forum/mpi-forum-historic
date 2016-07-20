MODULE mpi_f08_types 
 IMPLICIT NONE 

 ! constants needed for type definitions, values are implementation dependant
 INTEGER MPI_ADDRESS_KIND
 PARAMETER (MPI_ADDRESS_KIND=8) 

 INTEGER MPI_OFFSET_KIND
 PARAMETER (MPI_OFFSET_KIND=8) 

 INTEGER MPI_COUNT_KIND
 PARAMETER (MPI_COUNT_KIND=8) 

 INTEGER MPI_STATUS_SIZE
 PARAMETER (MPI_STATUS_SIZE=6) 

 INTEGER MPI_MAX_ERROR_STRING
 PARAMETER (MPI_MAX_ERROR_STRING=72) 

 INTEGER MPI_MAX_PROCESSOR_NAME
 PARAMETER (MPI_MAX_PROCESSOR_NAME=72) 

 INTEGER MPI_MAX_OBJECT_NAME
 PARAMETER (MPI_MAX_OBJECT_NAME=72) 

 INTEGER MPI_MAX_PORT_NAME
 PARAMETER (MPI_MAX_PORT_NAME=72) 

 TYPE, BIND(C) :: MPI_Comm
  INTEGER :: MPI_VAL
 END TYPE MPI_Comm

 TYPE, BIND(C) :: MPI_Datatype
  INTEGER :: MPI_VAL
 END TYPE MPI_Datatype

 TYPE, BIND(C) :: MPI_Errhandler
  INTEGER :: MPI_VAL
 END TYPE MPI_Errhandler

 TYPE, BIND(C) :: MPI_File
  INTEGER :: MPI_VAL
 END TYPE MPI_File

 TYPE, BIND(C) :: MPI_Group
  INTEGER :: MPI_VAL
 END TYPE MPI_Group

 TYPE, BIND(C) :: MPI_Info
  INTEGER :: MPI_VAL
 END TYPE MPI_Info

 TYPE, BIND(C) :: MPI_Op
  INTEGER :: MPI_VAL
 END TYPE MPI_Op

 TYPE, BIND(C) :: MPI_Request
  INTEGER :: MPI_VAL
 END TYPE MPI_Request

 TYPE, BIND(C) :: MPI_Win
  INTEGER :: MPI_VAL
 END TYPE MPI_Win

 TYPE, BIND(C) :: MPI_Message
  INTEGER :: MPI_VAL
 END TYPE MPI_Message

 TYPE, BIND(C) :: MPI_Status
  INTEGER :: MPI_SOURCE
  INTEGER :: MPI_TAG
  INTEGER :: MPI_ERROR
  INTEGER :: MPI_internal_bytecnt_high ! type, name, and semantics is implement implementation dependent
  INTEGER :: MPI_internal_bytecnt_low  ! type, name, and semantics is implement implementation dependent
  INTEGER :: MPI_internal_cancal_flag  ! type, name, and semantics is implement implementation dependent
 END TYPE MPI_Status
 
END MODULE mpi_f08_types 
 
!---------------------------------------------------------------------

MODULE mpi_f08_callback_prototypes

 ABSTRACT INTERFACE
  SUBROUTINE MPI_User_function(invec, inoutvec, len, datatype) BIND(C)
    USE mpi_f08_types 
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR 
    IMPLICIT NONE 
!   TYPE(*), DIMENSION(*) :: invec(len), inoutvec(len)
    TYPE(C_PTR), VALUE :: invec, inoutvec
    INTEGER, INTENT(IN) :: len
    TYPE(MPI_Datatype) :: datatype
  END SUBROUTINE
 END INTERFACE

!Example of a user defined callback function
!  
!  subroutine my_user_function( invec, inoutvec, len, type )   bind(c)
!    use, intrinsic :: iso_c_binding, only : c_ptr, c_f_pointer
!    type(c_ptr), value :: invec, inoutvec
!    integer, intent(in) :: len
!    type(MPI_Datatype) :: type
!    real, pointer :: invec_r(:), inoutvec_r(:)
!    if (type%MPI_VAL == MPI_REAL%MPI_VAL) then
!       call c_f_pointer(invec, invec_r, (/ len /) )
!       call c_f_pointer(inoutvec, inoutvec_r, (/ len /) )
!       inoutvec_r = invec_r + inoutvec_r
!    end if
!  end subroutine my_function
!
!The MPI library may internally store such callbacks in a global array All_MPI_Ops:
!  
!  type, private :: Internal_MPI_op
!    procedure(user_function), nopass, pointer :: user_fn
!  end type
!  type(Internal_MPI_op), private :: All_MPI_Ops(Max_Operations)
!  
!Within MPI_Op_create, the user_fn is stored in All_MPI_Ops:
!  
!  subroutine MPI_Op_create( user_fn, commute, op )   bind(C)
!    procedure(user_function) :: user_fn
!    type(MPI_Op), intent(out) :: op 
!    ... 
!    Registered_Operations = Registered_Operations + 1
!    op%MPI_VAL = Registered_Operations
!    All_MPI_Ops(Registered_Operations)%user_fn => user_fn
!
!Within MPI_Reduce, the stored user_fn is used to, e.g., to combine
!recvbuf = sendbuf+recvbuf 
!  
!  subroutine MPI_Reduce( sendbuf, recvbuf, count, datatype, op )   bind(C)
!    use, intrinsic :: iso_c_binding, only : c_loc
!    ... 
!    call All_MPI_Ops(op%MPI_VAL)%user_fn(c_loc(sendbuf), c_loc(recvbuf), count, datatype)
! 
 

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Comm_copy_attr_function(oldcomm, comm_keyval, extra_state, attribute_val_in, attribute_val_out,flag,ierror) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: oldcomm
    INTEGER :: comm_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    LOGICAL :: flag
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Comm_delete_attr_function(comm, comm_keyval, attribute_val, extra_state, ierror) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: comm
    INTEGER :: comm_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Win_copy_attr_function(oldwin, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Win) :: oldwin
    INTEGER :: win_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    LOGICAL :: flag
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Win_delete_attr_function(win, win_keyval, attribute_val, extra_state, ierror) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Win) :: win
    INTEGER :: win_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Type_copy_attr_function(oldtype, type_keyval, extra_state, attribute_val_in,attribute_val_out, flag,ierror) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Datatype) :: oldtype
    INTEGER :: type_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state, attribute_val_in, attribute_val_out
    LOGICAL :: flag
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Type_delete_attr_function(datatype, type_keyval, attribute_val, extra_state, ierror) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Datatype) :: datatype
    INTEGER :: type_keyval, ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: attribute_val, extra_state
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Comm_errhandler_function(comm, error_code) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Comm) :: comm
    INTEGER :: error_code
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Win_errhandler_function(win, error_code) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Win) :: win
    INTEGER :: error_code
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_File_errhandler_function(file, error_code) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_File) :: file
    INTEGER :: error_code
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Grequest_query_function(extra_state, status, ierror) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Status) :: status
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Grequest_free_function(extra_state, ierror) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Grequest_cancel_function(extra_state, complete, ierror) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
    LOGICAL :: complete
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Datarep_extent_function(datatype, extent, extra_state, ierror) BIND(C)
    USE mpi_f08_types 
    IMPLICIT NONE 
    TYPE(MPI_Datatype) :: datatype
    INTEGER :: ierror
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extent, extra_state
  END SUBROUTINE
 END INTERFACE

 ABSTRACT INTERFACE
  SUBROUTINE MPI_Datarep_conversion_function(userbuf, datatype, count, filebuf, position, extra_state, ierror) BIND(C)
    USE mpi_f08_types 
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR 
    IMPLICIT NONE 
!   TYPE(*), DIMENSION(*) :: userbuf(*), filebuf(*)
    TYPE(C_PTR), VALUE :: userbuf, filebuf
    TYPE(MPI_Datatype) :: datatype
    INTEGER :: count, ierror
    INTEGER(KIND=MPI_OFFSET_KIND) :: position
    INTEGER(KIND=MPI_ADDRESS_KIND) :: extra_state
  END SUBROUTINE
 END INTERFACE

! For deprecated routines - currently not planned for MPI-3.0

!  ABSTRACT INTERFACE
!   SUBROUTINE MPI_Copy_function(oldcomm, keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierr) BIND(C)
!     USE mpi_f08_types 
!     IMPLICIT NONE 
!     TYPE(MPI_Comm) :: oldcomm
!     INTEGER :: keyval, extra_state, attribute_val_in, attribute_val_out, ierr
!     LOGICAL :: flag
!   END SUBROUTINE
!  END INTERFACE
! 
!  ABSTRACT INTERFACE
!   SUBROUTINE MPI_Delete_function(comm, keyval, attribute_val, extra_state, ierr) BIND(C)
!     USE mpi_f08_types 
!     IMPLICIT NONE 
!     TYPE(MPI_Comm) :: comm
!     INTEGER :: keyval, attribute_val, extra_state, ierr
!   END SUBROUTINE
!  END INTERFACE
! 
!  ABSTRACT INTERFACE
!   SUBROUTINE MPI_Handler_function(comm, error_code) BIND(C)
!     USE mpi_f08_types 
!     IMPLICIT NONE 
!     TYPE(MPI_Comm) :: comm
!     INTEGER :: error_code
!   END SUBROUTINE
!  END INTERFACE
 
END MODULE mpi_f08_callback_prototypes
 
!---------------------------------------------------------------------

MODULE mpi_f08_constants
 USE mpi_f08_types 
 IMPLICIT NONE 

! some examples, values are implementation dependant

 INTEGER, DIMENSION(1) :: MPI_BOTTOM ! special constant defined as a module variable

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
 &)  BIND(C,NAME='MPI_Bsend_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Bsend_init(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Bsend_init_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Buffer_attach(buffer, size, ierror  &
 &)  BIND(C,NAME='MPI_Buffer_attach_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buffer
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
  ! !$PRAGMA IGNORE_TKR buffer
  ! !DIR$ IGNORE_TKR buffer
  ! !IBM* IGNORE_TKR buffer
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buffer ! choice-dummy-argument
  INTEGER, INTENT(IN) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Buffer_detach(buffer_addr, size, ierror  &
 &)  BIND(C,NAME='MPI_Buffer_detach_f08')
  USE mpi_f08_types
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
  IMPLICIT NONE
  TYPE(C_PTR), INTENT(OUT) :: buffer_addr
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cancel(request, ierror  &
 &)  BIND(C,NAME='MPI_Cancel_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(IN) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_count(status, datatype, count, ierror  &
 &)  BIND(C,NAME='MPI_Get_count_f08')
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
 &)  BIND(C,NAME='MPI_Ibsend_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Iprobe(source, tag, comm, flag, status, ierror  &
 &)  BIND(C,NAME='MPI_Iprobe_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: source, tag
  TYPE(MPI_Comm), INTENT(IN) :: comm
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Irecv(buf, count, datatype, source, tag, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Irecv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, source, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Irsend(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Irsend_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Isend(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Isend_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Issend(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Issend_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Probe(source, tag, comm, status, ierror  &
 &)  BIND(C,NAME='MPI_Probe_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: source, tag
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Recv(buf, count, datatype, source, tag, comm, status, ierror  &
 &)  BIND(C,NAME='MPI_Recv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, source, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Recv_init(buf, count, datatype, source, tag, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Recv_init_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, source, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Request_free(request, ierror  &
 &)  BIND(C,NAME='MPI_Request_free_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(INOUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Request_get_status(request, flag, status, ierror  &
 &)  BIND(C,NAME='MPI_Request_get_status_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(IN) :: request
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Rsend(buf, count, datatype, dest, tag, comm, ierror  &
 &)  BIND(C,NAME='MPI_Rsend_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Rsend_init(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Rsend_init_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Send(buf, count, datatype, dest, tag, comm, ierror  &
 &)  BIND(C,NAME='MPI_Send_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Sendrecv(sendbuf, sendcount, sendtype, dest, sendtag, recvbuf, recvcount, recvtype, source, recvtag, &
                             comm, status, ierror  &
 &)  BIND(C,NAME='MPI_Sendrecv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, dest, sendtag, recvcount, source, recvtag
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Sendrecv_replace(buf, count, datatype, dest, sendtag, source, recvtag, comm, status, ierror  &
 &)  BIND(C,NAME='MPI_Sendrecv_replace_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, sendtag, source, recvtag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Send_init(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Send_init_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ssend(buf, count, datatype, dest, tag, comm, ierror  &
 &)  BIND(C,NAME='MPI_Ssend_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ssend_init(buf, count, datatype, dest, tag, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Ssend_init_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, dest, tag
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Start(request, ierror  &
 &)  BIND(C,NAME='MPI_Start_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(INOUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Startall(count, array_of_requests, ierror  &
 &)  BIND(C,NAME='MPI_Startall_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Test(request, flag, status, ierror  &
 &)  BIND(C,NAME='MPI_Test_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(INOUT) :: request
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Testall(count, array_of_requests, flag, array_of_statuses, ierror  &
 &)  BIND(C,NAME='MPI_Testall_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Status) :: array_of_statuses(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Testany(count, array_of_requests, index, flag, status, ierror  &
 &)  BIND(C,NAME='MPI_Testany_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
  INTEGER, INTENT(OUT) :: index
  LOGICAL, INTENT(OUT) :: flag
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Testsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierror  &
 &)  BIND(C,NAME='MPI_Testsome_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: incount
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(incount)
  INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
  TYPE(MPI_Status) :: array_of_statuses(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Test_cancelled(status, flag, ierror  &
 &)  BIND(C,NAME='MPI_Test_cancelled_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Status), INTENT(IN) :: status
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Wait(request, status, ierror  &
 &)  BIND(C,NAME='MPI_Wait_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(INOUT) :: request
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Waitall(count, array_of_requests, array_of_statuses, ierror  &
 &)  BIND(C,NAME='MPI_Waitall_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
  TYPE(MPI_Status) :: array_of_statuses(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Waitany(count, array_of_requests, index, status, ierror  &
 &)  BIND(C,NAME='MPI_Waitany_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(count)
  INTEGER, INTENT(OUT) :: index
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Waitsome(incount, array_of_requests, outcount, array_of_indices, array_of_statuses, ierror  &
 &)  BIND(C,NAME='MPI_Waitsome_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: incount
  TYPE(MPI_Request), INTENT(INOUT) :: array_of_requests(incount)
  INTEGER, INTENT(OUT) :: outcount, array_of_indices(*)
  TYPE(MPI_Status) :: array_of_statuses(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_address(location, address, ierror  &
 &)  BIND(C,NAME='MPI_Get_address_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..) :: location
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: location
  ! !$PRAGMA IGNORE_TKR location
  ! !DIR$ IGNORE_TKR location
  ! !IBM* IGNORE_TKR location
  ! INTEGER, DIMENSION(*) :: location ! choice-dummy-argument
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: address
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_elements(status, datatype, count, ierror  &
 &)  BIND(C,NAME='MPI_Get_elements_f08')
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
 &)  BIND(C,NAME='MPI_Pack_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf
  ! !$PRAGMA IGNORE_TKR inbuf
  ! !DIR$ IGNORE_TKR inbuf
  ! !IBM* IGNORE_TKR inbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: inbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: outbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: outbuf
  ! !$PRAGMA IGNORE_TKR outbuf
  ! !DIR$ IGNORE_TKR outbuf
  ! !IBM* IGNORE_TKR outbuf
  ! INTEGER, DIMENSION(*) :: outbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: incount, outsize
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(INOUT) :: position
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Pack_external(datarep, inbuf, incount, datatype, outbuf, outsize, position, ierror  &
 &)  BIND(C,NAME='MPI_Pack_external_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: datarep
  TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf
  ! !$PRAGMA IGNORE_TKR inbuf
  ! !DIR$ IGNORE_TKR inbuf
  ! !IBM* IGNORE_TKR inbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: inbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: outbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: outbuf
  ! !$PRAGMA IGNORE_TKR outbuf
  ! !DIR$ IGNORE_TKR outbuf
  ! !IBM* IGNORE_TKR outbuf
  ! INTEGER, DIMENSION(*) :: outbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: incount
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: outsize
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(INOUT) :: position
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Pack_external_size(datarep, incount, datatype, size, ierror  &
 &)  BIND(C,NAME='MPI_Pack_external_size_f08')
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
 &)  BIND(C,NAME='MPI_Pack_size_f08')
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
 &)  BIND(C,NAME='MPI_Type_commit_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_contiguous(count, oldtype, newtype, ierror  &
 &)  BIND(C,NAME='MPI_Type_contiguous_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_darray(size, rank, ndims, array_of_gsizes, array_of_distribs, array_of_dargs, array_of_psizes, &
                             order, oldtype, newtype, ierror  &
 &)  BIND(C,NAME='MPI_Type_create_darray_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: size, rank, ndims, array_of_gsizes(ndims), array_of_distribs(ndims), array_of_dargs(ndims), &
                             array_of_psizes(ndims), order
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_hindexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Type_create_hindexed_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, array_of_blocklengths(count)
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_hvector(count, blocklength, stride, oldtype, newtype, ierror  &
 &)  BIND(C,NAME='MPI_Type_create_hvector_f08')
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
 &)  BIND(C,NAME='MPI_Type_create_indexed_block_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, blocklength, array_of_displacements(count)
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_resized(oldtype, lb, extent, newtype, ierror  &
 &)  BIND(C,NAME='MPI_Type_create_resized_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: lb, extent
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Type_create_struct_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, array_of_blocklengths(count)
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: array_of_displacements(count)
  TYPE(MPI_Datatype), INTENT(IN) :: array_of_types(count)
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_subarray(ndims, array_of_sizes, array_of_subsizes, array_of_starts, order, oldtype, &
                             newtype, ierror  &
 &)  BIND(C,NAME='MPI_Type_create_subarray_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: ndims, array_of_sizes(ndims), array_of_subsizes(ndims), array_of_starts(ndims), order
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_dup(oldtype, newtype, ierror  &
 &)  BIND(C,NAME='MPI_Type_dup_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_free(datatype, ierror  &
 &)  BIND(C,NAME='MPI_Type_free_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(INOUT) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_contents(datatype, max_integers, max_addresses, max_datatypes, array_of_integers, array_of_addresses, &
                             array_of_datatypes, ierror  &
 &)  BIND(C,NAME='MPI_Type_get_contents_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: max_integers, max_addresses, max_datatypes
  INTEGER, INTENT(OUT) :: array_of_integers(max_integers)
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: array_of_addresses(max_addresses)
  TYPE(MPI_Datatype), INTENT(OUT) :: array_of_datatypes(max_datatypes)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_envelope(datatype, num_integers, num_addresses, num_datatypes, combiner, ierror  &
 &)  BIND(C,NAME='MPI_Type_get_envelope_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(OUT) :: num_integers, num_addresses, num_datatypes, combiner
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_extent(datatype, lb, extent, ierror  &
 &)  BIND(C,NAME='MPI_Type_get_extent_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: lb, extent
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_true_extent(datatype, true_lb, true_extent, ierror  &
 &)  BIND(C,NAME='MPI_Type_get_true_extent_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(OUT) :: true_lb, true_extent
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_indexed(count, array_of_blocklengths, array_of_displacements, oldtype, newtype, ierror  &
 &)  BIND(C,NAME='MPI_Type_indexed_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, array_of_blocklengths(count), array_of_displacements(count)
  TYPE(MPI_Datatype), INTENT(IN) :: oldtype
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_size(datatype, size, ierror  &
 &)  BIND(C,NAME='MPI_Type_size_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_vector(count, blocklength, stride, oldtype, newtype, ierror  &
 &)  BIND(C,NAME='MPI_Type_vector_f08')
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
 &)  BIND(C,NAME='MPI_Unpack_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf
  ! !$PRAGMA IGNORE_TKR inbuf
  ! !DIR$ IGNORE_TKR inbuf
  ! !IBM* IGNORE_TKR inbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: inbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: outbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: outbuf
  ! !$PRAGMA IGNORE_TKR outbuf
  ! !DIR$ IGNORE_TKR outbuf
  ! !IBM* IGNORE_TKR outbuf
  ! INTEGER, DIMENSION(*) :: outbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: insize, outcount
  INTEGER, INTENT(INOUT) :: position
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Unpack_external(datarep, inbuf, insize, position, outbuf, outcount, datatype, ierror  &
 &)  BIND(C,NAME='MPI_Unpack_external_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: datarep
  TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf
  ! !$PRAGMA IGNORE_TKR inbuf
  ! !DIR$ IGNORE_TKR inbuf
  ! !IBM* IGNORE_TKR inbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: inbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: outbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: outbuf
  ! !$PRAGMA IGNORE_TKR outbuf
  ! !DIR$ IGNORE_TKR outbuf
  ! !IBM* IGNORE_TKR outbuf
  ! INTEGER, DIMENSION(*) :: outbuf ! choice-dummy-argument
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: insize
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(INOUT) :: position
  INTEGER, INTENT(IN) :: outcount
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror  &
 &)  BIND(C,NAME='MPI_Allgather_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Allgatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, ierror  &
 &)  BIND(C,NAME='MPI_Allgatherv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcounts(*), displs(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Allreduce(sendbuf, recvbuf, count, datatype, op, comm, ierror  &
 &)  BIND(C,NAME='MPI_Allreduce_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror  &
 &)  BIND(C,NAME='MPI_Alltoall_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Alltoallv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, comm, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Alltoallw_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtypes(*)
  TYPE(MPI_Datatype), INTENT(IN) :: recvtypes(*)
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Barrier(comm, ierror  &
 &)  BIND(C,NAME='MPI_Barrier_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Bcast(buffer, count, datatype, root, comm, ierror  &
 &)  BIND(C,NAME='MPI_Bcast_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..) :: buffer
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
  ! !$PRAGMA IGNORE_TKR buffer
  ! !DIR$ IGNORE_TKR buffer
  ! !IBM* IGNORE_TKR buffer
  ! INTEGER, DIMENSION(*) :: buffer ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, root
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Exscan(sendbuf, recvbuf, count, datatype, op, comm, ierror  &
 &)  BIND(C,NAME='MPI_Exscan_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Gather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierror  &
 &)  BIND(C,NAME='MPI_Gather_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount, root
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Gatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, ierror  &
 &)  BIND(C,NAME='MPI_Gatherv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcounts(*), displs(*), root
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Op_commutative(op, commute, ierror  &
 &)  BIND(C,NAME='MPI_Op_commutative_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Op), INTENT(IN) :: op
  LOGICAL, INTENT(OUT) :: commute
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Op_create(user_fn, commute, op, ierror  &
 &)  BIND(C,NAME='MPI_Op_create_f08')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  ! EXTERNAL :: user_fn 
  PROCEDURE(MPI_User_function) :: user_fn
  LOGICAL, INTENT(IN) :: commute
  TYPE(MPI_Op), INTENT(OUT) :: op
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Op_free(op, ierror  &
 &)  BIND(C,NAME='MPI_Op_free_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Op), INTENT(INOUT) :: op
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Reduce(sendbuf, recvbuf, count, datatype, op, root, comm, ierror  &
 &)  BIND(C,NAME='MPI_Reduce_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, root
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Reduce_local(inbuf, inoutbuf, count, datatype, op, ierror  &
 &)  BIND(C,NAME='MPI_Reduce_local_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: inbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: inbuf
  ! !$PRAGMA IGNORE_TKR inbuf
  ! !DIR$ IGNORE_TKR inbuf
  ! !IBM* IGNORE_TKR inbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: inbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: inoutbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: inoutbuf
  ! !$PRAGMA IGNORE_TKR inoutbuf
  ! !DIR$ IGNORE_TKR inoutbuf
  ! !IBM* IGNORE_TKR inoutbuf
  ! INTEGER, DIMENSION(*) :: inoutbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Reduce_scatter(sendbuf, recvbuf, recvcounts, datatype, op, comm, ierror  &
 &)  BIND(C,NAME='MPI_Reduce_scatter_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: recvcounts(*)
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Reduce_scatter_block(sendbuf, recvbuf, recvcount, datatype, op, comm, ierror  &
 &)  BIND(C,NAME='MPI_Reduce_scatter_block_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Scan(sendbuf, recvbuf, count, datatype, op, comm, ierror  &
 &)  BIND(C,NAME='MPI_Scan_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Scatter(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, ierror  &
 &)  BIND(C,NAME='MPI_Scatter_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount, root
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Scatterv(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, ierror  &
 &)  BIND(C,NAME='MPI_Scatterv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcounts(*), displs(*), recvcount, root
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_compare(comm1, comm2, result, ierror  &
 &)  BIND(C,NAME='MPI_Comm_compare_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm1, comm2
  INTEGER, INTENT(OUT) :: result
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_create(comm, group, newcomm, ierror  &
 &)  BIND(C,NAME='MPI_Comm_create_f08')
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
 &)  BIND(C,NAME='MPI_Comm_create_keyval_f08')
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
 &)  BIND(C,NAME='MPI_Comm_delete_attr_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: comm_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_dup(comm, newcomm, ierror  &
 &)  BIND(C,NAME='MPI_Comm_dup_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Comm), INTENT(OUT) :: newcomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_COMM_DUP_FN(oldcomm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror  &
 &)  BIND(C,NAME='MPI_COMM_DUP_FN_f08')
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
 &)  BIND(C,NAME='MPI_Comm_free_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(INOUT) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_free_keyval(comm_keyval, ierror  &
 &)  BIND(C,NAME='MPI_Comm_free_keyval_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: comm_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_get_attr(comm, comm_keyval, attribute_val, flag, ierror  &
 &)  BIND(C,NAME='MPI_Comm_get_attr_f08')
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
 &)  BIND(C,NAME='MPI_Comm_get_name_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: comm_name
  INTEGER, INTENT(OUT) :: resultlen
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_group(comm, group, ierror  &
 &)  BIND(C,NAME='MPI_Comm_group_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Group), INTENT(OUT) :: group
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_COMM_NULL_COPY_FN(oldcomm, comm_keyval, extra_state, attribute_val_in, attribute_val_out, flag, &
                             ierror  &
 &)  BIND(C,NAME='MPI_COMM_NULL_COPY_FN_f08')
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
 &)  BIND(C,NAME='MPI_COMM_NULL_DELETE_FN_f08')
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
 &)  BIND(C,NAME='MPI_Comm_rank_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: rank
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_remote_group(comm, group, ierror  &
 &)  BIND(C,NAME='MPI_Comm_remote_group_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Group), INTENT(OUT) :: group
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_remote_size(comm, size, ierror  &
 &)  BIND(C,NAME='MPI_Comm_remote_size_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_set_attr(comm, comm_keyval, attribute_val, ierror  &
 &)  BIND(C,NAME='MPI_Comm_set_attr_f08')
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
 &)  BIND(C,NAME='MPI_Comm_set_name_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  CHARACTER(LEN=*), INTENT(IN) :: comm_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_size(comm, size, ierror  &
 &)  BIND(C,NAME='MPI_Comm_size_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_split(comm, color, key, newcomm, ierror  &
 &)  BIND(C,NAME='MPI_Comm_split_f08')
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
 &)  BIND(C,NAME='MPI_Comm_test_inter_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_compare(group1, group2, result, ierror  &
 &)  BIND(C,NAME='MPI_Group_compare_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group1, group2
  INTEGER, INTENT(OUT) :: result
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_difference(group1, group2, newgroup, ierror  &
 &)  BIND(C,NAME='MPI_Group_difference_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group1, group2
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_excl(group, n, ranks, newgroup, ierror  &
 &)  BIND(C,NAME='MPI_Group_excl_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(IN) :: n, ranks(n)
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_free(group, ierror  &
 &)  BIND(C,NAME='MPI_Group_free_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(INOUT) :: group
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_incl(group, n, ranks, newgroup, ierror  &
 &)  BIND(C,NAME='MPI_Group_incl_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(IN) :: n, ranks(n)
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_intersection(group1, group2, newgroup, ierror  &
 &)  BIND(C,NAME='MPI_Group_intersection_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group1, group2
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_range_excl(group, n, ranges, newgroup, ierror  &
 &)  BIND(C,NAME='MPI_Group_range_excl_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(IN) :: n, ranges(3,n)
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_range_incl(group, n, ranges, newgroup, ierror  &
 &)  BIND(C,NAME='MPI_Group_range_incl_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(IN) :: n, ranges(3,n)
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_rank(group, rank, ierror  &
 &)  BIND(C,NAME='MPI_Group_rank_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(OUT) :: rank
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_size(group, size, ierror  &
 &)  BIND(C,NAME='MPI_Group_size_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_translate_ranks(group1, n, ranks1, group2, ranks2, ierror  &
 &)  BIND(C,NAME='MPI_Group_translate_ranks_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group1, group2
  INTEGER, INTENT(IN) :: n, ranks1(n)
  INTEGER, INTENT(OUT) :: ranks2(n)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Group_union(group1, group2, newgroup, ierror  &
 &)  BIND(C,NAME='MPI_Group_union_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Group), INTENT(IN) :: group1, group2
  TYPE(MPI_Group), INTENT(OUT) :: newgroup
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Intercomm_create(local_comm, local_leader, peer_comm, remote_leader, tag, newintercomm, ierror  &
 &)  BIND(C,NAME='MPI_Intercomm_create_f08')
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
 &)  BIND(C,NAME='MPI_Intercomm_merge_f08')
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
 &)  BIND(C,NAME='MPI_Type_create_keyval_f08')
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
 &)  BIND(C,NAME='MPI_Type_delete_attr_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: type_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_TYPE_DUP_FN(oldtype, type_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror  &
 &)  BIND(C,NAME='MPI_TYPE_DUP_FN_f08')
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
 &)  BIND(C,NAME='MPI_Type_free_keyval_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: type_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_attr(datatype, type_keyval, attribute_val, flag, ierror  &
 &)  BIND(C,NAME='MPI_Type_get_attr_f08')
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
 &)  BIND(C,NAME='MPI_Type_get_name_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: type_name
  INTEGER, INTENT(OUT) :: resultlen
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_TYPE_NULL_COPY_FN(oldtype, type_keyval, extra_state, attribute_val_in, attribute_val_out, flag, &
                             ierror  &
 &)  BIND(C,NAME='MPI_TYPE_NULL_COPY_FN_f08')
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
 &)  BIND(C,NAME='MPI_TYPE_NULL_DELETE_FN_f08')
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
 &)  BIND(C,NAME='MPI_Type_set_attr_f08')
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
 &)  BIND(C,NAME='MPI_Type_set_name_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  CHARACTER(LEN=*), INTENT(IN) :: type_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_create_keyval(win_copy_attr_fn, win_delete_attr_fn, win_keyval, extra_state, ierror  &
 &)  BIND(C,NAME='MPI_Win_create_keyval_f08')
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
 &)  BIND(C,NAME='MPI_Win_delete_attr_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, INTENT(IN) :: win_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_WIN_DUP_FN(oldwin, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, ierror  &
 &)  BIND(C,NAME='MPI_WIN_DUP_FN_f08')
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
 &)  BIND(C,NAME='MPI_Win_free_keyval_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(INOUT) :: win_keyval
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_get_attr(win, win_keyval, attribute_val, flag, ierror  &
 &)  BIND(C,NAME='MPI_Win_get_attr_f08')
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
 &)  BIND(C,NAME='MPI_Win_get_name_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  CHARACTER(LEN=MPI_MAX_OBJECT_NAME), INTENT(OUT) :: win_name
  INTEGER, INTENT(OUT) :: resultlen
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_WIN_NULL_COPY_FN(oldwin, win_keyval, extra_state, attribute_val_in, attribute_val_out, flag, &
                             ierror  &
 &)  BIND(C,NAME='MPI_WIN_NULL_COPY_FN_f08')
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
 &)  BIND(C,NAME='MPI_WIN_NULL_DELETE_FN_f08')
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
 &)  BIND(C,NAME='MPI_Win_set_attr_f08')
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
 &)  BIND(C,NAME='MPI_Win_set_name_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  CHARACTER(LEN=*), INTENT(IN) :: win_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cartdim_get(comm, ndims, ierror  &
 &)  BIND(C,NAME='MPI_Cartdim_get_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: ndims
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_coords(comm, rank, maxdims, coords, ierror  &
 &)  BIND(C,NAME='MPI_Cart_coords_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: rank, maxdims
  INTEGER, INTENT(OUT) :: coords(maxdims)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_create(comm_old, ndims, dims, periods, reorder, comm_cart, ierror  &
 &)  BIND(C,NAME='MPI_Cart_create_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: ndims, dims(ndims)
  LOGICAL, INTENT(IN) :: periods(ndims), reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_cart
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_get(comm, maxdims, dims, periods, coords, ierror  &
 &)  BIND(C,NAME='MPI_Cart_get_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: maxdims
  INTEGER, INTENT(OUT) :: dims(maxdims), coords(maxdims)
  LOGICAL, INTENT(OUT) :: periods(maxdims)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_map(comm, ndims, dims, periods, newrank, ierror  &
 &)  BIND(C,NAME='MPI_Cart_map_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: ndims, dims(ndims)
  LOGICAL, INTENT(IN) :: periods(ndims)
  INTEGER, INTENT(OUT) :: newrank
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Cart_rank(comm, coords, rank, ierror  &
 &)  BIND(C,NAME='MPI_Cart_rank_f08')
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
 &)  BIND(C,NAME='MPI_Cart_shift_f08')
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
 &)  BIND(C,NAME='MPI_Cart_sub_f08')
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
 &)  BIND(C,NAME='MPI_Dims_create_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: nnodes, ndims
  INTEGER, INTENT(INOUT) :: dims(ndims)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Dist_graph_create(comm_old, n, sources, degrees, destinations, weights, info, reorder, comm_dist_graph, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Dist_graph_create_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: n, sources(n), degrees(n), destinations(*)
  INTEGER, INTENT(IN) :: weights(*)
  TYPE(MPI_Info), INTENT(IN) :: info
  LOGICAL, INTENT(IN) :: reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Dist_graph_create_adjacent(comm_old, indegree, sources, sourceweights, outdegree, destinations, &
                             destweights, info, reorder, comm_dist_graph, ierror  &
 &)  BIND(C,NAME='MPI_Dist_graph_create_adjacent_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: indegree, sources(indegree), outdegree, destinations(outdegree)
  INTEGER, INTENT(IN) :: sourceweights(*), destweights(*)
  TYPE(MPI_Info), INTENT(IN) :: info
  LOGICAL, INTENT(IN) :: reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_dist_graph
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Dist_graph_neighbors(comm, maxindegree, sources, sourceweights, maxoutdegree, destinations, destweights, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Dist_graph_neighbors_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: maxindegree, maxoutdegree
  INTEGER, INTENT(OUT) :: sources(maxindegree), destinations(maxoutdegree)
  INTEGER :: sourceweights(*), destweights(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Dist_graph_neighbors_count(comm, indegree, outdegree, weighted, ierror  &
 &)  BIND(C,NAME='MPI_Dist_graph_neighbors_count_f08')
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
 &)  BIND(C,NAME='MPI_Graphdims_get_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: nnodes, nedges
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Graph_create(comm_old, nnodes, index, edges, reorder, comm_graph, ierror  &
 &)  BIND(C,NAME='MPI_Graph_create_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm_old
  INTEGER, INTENT(IN) :: nnodes, index(nnodes), edges(*)
  LOGICAL, INTENT(IN) :: reorder
  TYPE(MPI_Comm), INTENT(OUT) :: comm_graph
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Graph_get(comm, maxindex, maxedges, index, edges, ierror  &
 &)  BIND(C,NAME='MPI_Graph_get_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: maxindex, maxedges
  INTEGER, INTENT(OUT) :: index(maxindex), edges(maxedges)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Graph_map(comm, nnodes, index, edges, newrank, ierror  &
 &)  BIND(C,NAME='MPI_Graph_map_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: nnodes, index(nnodes), edges(*)
  INTEGER, INTENT(OUT) :: newrank
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Graph_neighbors(comm, rank, maxneighbors, neighbors, ierror  &
 &)  BIND(C,NAME='MPI_Graph_neighbors_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: rank, maxneighbors
  INTEGER, INTENT(OUT) :: neighbors(maxneighbors)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Graph_neighbors_count(comm, rank, nneighbors, ierror  &
 &)  BIND(C,NAME='MPI_Graph_neighbors_count_f08')
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
 &)  BIND(C,NAME='MPI_Topo_test_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 DOUBLE PRECISION FUNCTION MPI_Wtick(  &
 &)  BIND(C,NAME='MPI_Wtick_f08')
  USE mpi_f08_types
  IMPLICIT NONE
 END FUNCTION
END INTERFACE

INTERFACE
 DOUBLE PRECISION FUNCTION MPI_Wtime(  &
 &)  BIND(C,NAME='MPI_Wtime_f08')
  USE mpi_f08_types
  IMPLICIT NONE
 END FUNCTION
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Abort(comm, errorcode, ierror  &
 &)  BIND(C,NAME='MPI_Abort_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: errorcode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Add_error_class(errorclass, ierror  &
 &)  BIND(C,NAME='MPI_Add_error_class_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: errorclass
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Add_error_code(errorclass, errorcode, ierror  &
 &)  BIND(C,NAME='MPI_Add_error_code_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: errorclass
  INTEGER, INTENT(OUT) :: errorcode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Add_error_string(errorcode, string, ierror  &
 &)  BIND(C,NAME='MPI_Add_error_string_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: errorcode
  CHARACTER(LEN=*), INTENT(IN) :: string
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Alloc_mem(size, info, baseptr, ierror  &
 &)  BIND(C,NAME='MPI_Alloc_mem_f08')
  USE mpi_f08_types
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
  IMPLICIT NONE
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(C_PTR), INTENT(OUT) :: baseptr
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_call_errhandler(comm, errorcode, ierror  &
 &)  BIND(C,NAME='MPI_Comm_call_errhandler_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: errorcode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_create_errhandler(comm_errhandler_fn, errhandler, ierror  &
 &)  BIND(C,NAME='MPI_Comm_create_errhandler_f08')
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
 &)  BIND(C,NAME='MPI_Comm_get_errhandler_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_set_errhandler(comm, errhandler, ierror  &
 &)  BIND(C,NAME='MPI_Comm_set_errhandler_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Errhandler_free(errhandler, ierror  &
 &)  BIND(C,NAME='MPI_Errhandler_free_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Errhandler), INTENT(INOUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Error_class(errorcode, errorclass, ierror  &
 &)  BIND(C,NAME='MPI_Error_class_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: errorcode
  INTEGER, INTENT(OUT) :: errorclass
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Error_string(errorcode, string, resultlen, ierror  &
 &)  BIND(C,NAME='MPI_Error_string_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: errorcode
  CHARACTER(LEN=MPI_MAX_ERROR_STRING), INTENT(OUT) :: string
  INTEGER, INTENT(OUT) :: resultlen
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_call_errhandler(fh, errorcode, ierror  &
 &)  BIND(C,NAME='MPI_File_call_errhandler_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER, INTENT(IN) :: errorcode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_create_errhandler(file_errhandler_fn, errhandler, ierror  &
 &)  BIND(C,NAME='MPI_File_create_errhandler_f08')
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
 &)  BIND(C,NAME='MPI_File_get_errhandler_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: file
  TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_set_errhandler(file, errhandler, ierror  &
 &)  BIND(C,NAME='MPI_File_set_errhandler_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: file
  TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Finalize(ierror  &
 &)  BIND(C,NAME='MPI_Finalize_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Finalized(flag, ierror  &
 &)  BIND(C,NAME='MPI_Finalized_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Free_mem(base, ierror  &
 &)  BIND(C,NAME='MPI_Free_mem_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: base
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: base
  ! !$PRAGMA IGNORE_TKR base
  ! !DIR$ IGNORE_TKR base
  ! !IBM* IGNORE_TKR base
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: base ! choice-dummy-argument
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_processor_name(name, resultlen, ierror  &
 &)  BIND(C,NAME='MPI_Get_processor_name_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=MPI_MAX_PROCESSOR_NAME), INTENT(OUT) :: name
  INTEGER, INTENT(OUT) :: resultlen
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_version(version, subversion, ierror  &
 &)  BIND(C,NAME='MPI_Get_version_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: version, subversion
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Init(ierror  &
 &)  BIND(C,NAME='MPI_Init_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Initialized(flag, ierror  &
 &)  BIND(C,NAME='MPI_Initialized_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_call_errhandler(win, errorcode, ierror  &
 &)  BIND(C,NAME='MPI_Win_call_errhandler_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, INTENT(IN) :: errorcode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_create_errhandler(win_errhandler_fn, errhandler, ierror  &
 &)  BIND(C,NAME='MPI_Win_create_errhandler_f08')
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
 &)  BIND(C,NAME='MPI_Win_get_errhandler_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(MPI_Errhandler), INTENT(OUT) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_set_errhandler(win, errhandler, ierror  &
 &)  BIND(C,NAME='MPI_Win_set_errhandler_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(MPI_Errhandler), INTENT(IN) :: errhandler
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_create(info, ierror  &
 &)  BIND(C,NAME='MPI_Info_create_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(OUT) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_delete(info, key, ierror  &
 &)  BIND(C,NAME='MPI_Info_delete_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=*), INTENT(IN) :: key
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_dup(info, newinfo, ierror  &
 &)  BIND(C,NAME='MPI_Info_dup_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(MPI_Info), INTENT(OUT) :: newinfo
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_free(info, ierror  &
 &)  BIND(C,NAME='MPI_Info_free_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(INOUT) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_get(info, key, valuelen, value, flag, ierror  &
 &)  BIND(C,NAME='MPI_Info_get_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=*), INTENT(IN) :: key
  INTEGER, INTENT(IN) :: valuelen
  CHARACTER(LEN=valuelen), INTENT(OUT) :: value
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_get_nkeys(info, nkeys, ierror  &
 &)  BIND(C,NAME='MPI_Info_get_nkeys_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, INTENT(OUT) :: nkeys
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Info_get_nthkey(info, n, key, ierror  &
 &)  BIND(C,NAME='MPI_Info_get_nthkey_f08')
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
 &)  BIND(C,NAME='MPI_Info_get_valuelen_f08')
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
 &)  BIND(C,NAME='MPI_Info_set_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=*), INTENT(IN) :: key, value
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Close_port(port_name, ierror  &
 &)  BIND(C,NAME='MPI_Close_port_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: port_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_accept(port_name, info, root, comm, newcomm, ierror  &
 &)  BIND(C,NAME='MPI_Comm_accept_f08')
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
 &)  BIND(C,NAME='MPI_Comm_connect_f08')
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
 &)  BIND(C,NAME='MPI_Comm_disconnect_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(INOUT) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_get_parent(parent, ierror  &
 &)  BIND(C,NAME='MPI_Comm_get_parent_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(OUT) :: parent
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_join(fd, intercomm, ierror  &
 &)  BIND(C,NAME='MPI_Comm_join_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: fd
  TYPE(MPI_Comm), INTENT(OUT) :: intercomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_spawn(command, argv, maxprocs, info, root, comm, intercomm, array_of_errcodes, ierror  &
 &)  BIND(C,NAME='MPI_Comm_spawn_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: command, argv(*)
  INTEGER, INTENT(IN) :: maxprocs, root
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Comm), INTENT(OUT) :: intercomm
  INTEGER :: array_of_errcodes(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_spawn_multiple(count, array_of_commands, array_of_argv, array_of_maxprocs, array_of_info, &
                             root, comm, intercomm, array_of_errcodes, ierror  &
 &)  BIND(C,NAME='MPI_Comm_spawn_multiple_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: count, array_of_maxprocs(*), root
  CHARACTER(LEN=*), INTENT(IN) :: array_of_commands(*), array_of_argv(count, *)
  TYPE(MPI_Info), INTENT(IN) :: array_of_info(*)
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Comm), INTENT(OUT) :: intercomm
  INTEGER :: array_of_errcodes(*)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Lookup_name(service_name, info, port_name, ierror  &
 &)  BIND(C,NAME='MPI_Lookup_name_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: service_name
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=MPI_MAX_PORT_NAME), INTENT(OUT) :: port_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Open_port(info, port_name, ierror  &
 &)  BIND(C,NAME='MPI_Open_port_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=MPI_MAX_PORT_NAME), INTENT(OUT) :: port_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Publish_name(service_name, info, port_name, ierror  &
 &)  BIND(C,NAME='MPI_Publish_name_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  CHARACTER(LEN=*), INTENT(IN) :: service_name, port_name
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Unpublish_name(service_name, info, port_name, ierror  &
 &)  BIND(C,NAME='MPI_Unpublish_name_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: service_name, port_name
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Accumulate(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, &
                             target_datatype, op, win, ierror  &
 &)  BIND(C,NAME='MPI_Accumulate_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
  ! !$PRAGMA IGNORE_TKR origin_addr
  ! !DIR$ IGNORE_TKR origin_addr
  ! !IBM* IGNORE_TKR origin_addr
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: origin_addr ! choice-dummy-argument
  INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, &
                             win, ierror  &
 &)  BIND(C,NAME='MPI_Get_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: origin_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
  ! !$PRAGMA IGNORE_TKR origin_addr
  ! !DIR$ IGNORE_TKR origin_addr
  ! !IBM* IGNORE_TKR origin_addr
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: origin_addr ! choice-dummy-argument
  INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Put(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, &
                             win, ierror  &
 &)  BIND(C,NAME='MPI_Put_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
  ! !$PRAGMA IGNORE_TKR origin_addr
  ! !DIR$ IGNORE_TKR origin_addr
  ! !IBM* IGNORE_TKR origin_addr
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: origin_addr ! choice-dummy-argument
  INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_complete(win, ierror  &
 &)  BIND(C,NAME='MPI_Win_complete_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_create(base, size, disp_unit, info, comm, win, ierror  &
 &)  BIND(C,NAME='MPI_Win_create_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: base
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: base
  ! !$PRAGMA IGNORE_TKR base
  ! !DIR$ IGNORE_TKR base
  ! !IBM* IGNORE_TKR base
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: base ! choice-dummy-argument
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
 &)  BIND(C,NAME='MPI_Win_fence_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: assert
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_free(win, ierror  &
 &)  BIND(C,NAME='MPI_Win_free_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(INOUT) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_get_group(win, group, ierror  &
 &)  BIND(C,NAME='MPI_Win_get_group_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(MPI_Group), INTENT(OUT) :: group
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_lock(lock_type, rank, assert, win, ierror  &
 &)  BIND(C,NAME='MPI_Win_lock_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: lock_type, rank, assert
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_post(group, assert, win, ierror  &
 &)  BIND(C,NAME='MPI_Win_post_f08')
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
 &)  BIND(C,NAME='MPI_Win_start_f08')
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
 &)  BIND(C,NAME='MPI_Win_test_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_unlock(rank, win, ierror  &
 &)  BIND(C,NAME='MPI_Win_unlock_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: rank
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_wait(win, ierror  &
 &)  BIND(C,NAME='MPI_Win_wait_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Grequest_complete(request, ierror  &
 &)  BIND(C,NAME='MPI_Grequest_complete_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Request), INTENT(IN) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Grequest_start(query_fn, free_fn, cancel_fn, extra_state, request, ierror  &
 &)  BIND(C,NAME='MPI_Grequest_start_f08')
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
 &)  BIND(C,NAME='MPI_Init_thread_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: required
  INTEGER, INTENT(OUT) :: provided
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Is_thread_main(flag, ierror  &
 &)  BIND(C,NAME='MPI_Is_thread_main_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Query_thread(provided, ierror  &
 &)  BIND(C,NAME='MPI_Query_thread_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(OUT) :: provided
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Status_set_cancelled(status, flag, ierror  &
 &)  BIND(C,NAME='MPI_Status_set_cancelled_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Status), INTENT(INOUT) :: status
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Status_set_elements(status, datatype, count, ierror  &
 &)  BIND(C,NAME='MPI_Status_set_elements_f08')
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
 &)  BIND(C,NAME='MPI_File_close_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(INOUT) :: fh
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_delete(filename, info, ierror  &
 &)  BIND(C,NAME='MPI_File_delete_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: filename
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_amode(fh, amode, ierror  &
 &)  BIND(C,NAME='MPI_File_get_amode_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER, INTENT(OUT) :: amode
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_atomicity(fh, flag, ierror  &
 &)  BIND(C,NAME='MPI_File_get_atomicity_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  LOGICAL, INTENT(OUT) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_byte_offset(fh, offset, disp, ierror  &
 &)  BIND(C,NAME='MPI_File_get_byte_offset_f08')
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
 &)  BIND(C,NAME='MPI_File_get_group_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(MPI_Group), INTENT(OUT) :: group
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_info(fh, info_used, ierror  &
 &)  BIND(C,NAME='MPI_File_get_info_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(MPI_Info), INTENT(OUT) :: info_used
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_position(fh, offset, ierror  &
 &)  BIND(C,NAME='MPI_File_get_position_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: offset
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_position_shared(fh, offset, ierror  &
 &)  BIND(C,NAME='MPI_File_get_position_shared_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: offset
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_size(fh, size, ierror  &
 &)  BIND(C,NAME='MPI_File_get_size_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_get_type_extent(fh, datatype, extent, ierror  &
 &)  BIND(C,NAME='MPI_File_get_type_extent_f08')
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
 &)  BIND(C,NAME='MPI_File_get_view_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(OUT) :: disp
  TYPE(MPI_Datatype), INTENT(OUT) :: etype, filetype
  CHARACTER(LEN=*), INTENT(OUT) :: datarep
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iread(fh, buf, count, datatype, request, ierror  &
 &)  BIND(C,NAME='MPI_File_iread_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iread_at(fh, offset, buf, count, datatype, request, ierror  &
 &)  BIND(C,NAME='MPI_File_iread_at_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iread_shared(fh, buf, count, datatype, request, ierror  &
 &)  BIND(C,NAME='MPI_File_iread_shared_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iwrite(fh, buf, count, datatype, request, ierror  &
 &)  BIND(C,NAME='MPI_File_iwrite_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iwrite_at(fh, offset, buf, count, datatype, request, ierror  &
 &)  BIND(C,NAME='MPI_File_iwrite_at_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_iwrite_shared(fh, buf, count, datatype, request, ierror  &
 &)  BIND(C,NAME='MPI_File_iwrite_shared_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_open(comm, filename, amode, info, fh, ierror  &
 &)  BIND(C,NAME='MPI_File_open_f08')
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
 &)  BIND(C,NAME='MPI_File_preallocate_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read(fh, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_read_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_all(fh, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_read_all_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_all_begin(fh, buf, count, datatype, ierror  &
 &)  BIND(C,NAME='MPI_File_read_all_begin_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_all_end(fh, buf, status, ierror  &
 &)  BIND(C,NAME='MPI_File_read_all_end_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_at(fh, offset, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_read_at_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  TYPE(*), DIMENSION(..) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_at_all(fh, offset, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_read_at_all_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  TYPE(*), DIMENSION(..) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_at_all_begin(fh, offset, buf, count, datatype, ierror  &
 &)  BIND(C,NAME='MPI_File_read_at_all_begin_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_at_all_end(fh, buf, status, ierror  &
 &)  BIND(C,NAME='MPI_File_read_at_all_end_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_ordered(fh, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_read_ordered_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_ordered_begin(fh, buf, count, datatype, ierror  &
 &)  BIND(C,NAME='MPI_File_read_ordered_begin_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_ordered_end(fh, buf, status, ierror  &
 &)  BIND(C,NAME='MPI_File_read_ordered_end_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_read_shared(fh, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_read_shared_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_seek(fh, offset, whence, ierror  &
 &)  BIND(C,NAME='MPI_File_seek_f08')
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
 &)  BIND(C,NAME='MPI_File_seek_shared_f08')
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
 &)  BIND(C,NAME='MPI_File_set_atomicity_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  LOGICAL, INTENT(IN) :: flag
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_set_info(fh, info, ierror  &
 &)  BIND(C,NAME='MPI_File_set_info_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_set_size(fh, size, ierror  &
 &)  BIND(C,NAME='MPI_File_set_size_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_set_view(fh, disp, etype, filetype, datarep, info, ierror  &
 &)  BIND(C,NAME='MPI_File_set_view_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: disp
  TYPE(MPI_Datatype), INTENT(IN) :: etype, filetype
  CHARACTER(LEN=*), INTENT(IN) :: datarep
  TYPE(MPI_Info), INTENT(IN) :: info
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_sync(fh, ierror  &
 &)  BIND(C,NAME='MPI_File_sync_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write(fh, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_write_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), INTENT(IN) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_all(fh, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_write_all_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), INTENT(IN) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_all_begin(fh, buf, count, datatype, ierror  &
 &)  BIND(C,NAME='MPI_File_write_all_begin_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_all_end(fh, buf, status, ierror  &
 &)  BIND(C,NAME='MPI_File_write_all_end_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_at(fh, offset, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_write_at_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  TYPE(*), DIMENSION(..), INTENT(IN) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_at_all(fh, offset, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_write_at_all_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  TYPE(*), DIMENSION(..), INTENT(IN) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_at_all_begin(fh, offset, buf, count, datatype, ierror  &
 &)  BIND(C,NAME='MPI_File_write_at_all_begin_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  INTEGER(KIND=MPI_OFFSET_KIND), INTENT(IN) :: offset
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_at_all_end(fh, buf, status, ierror  &
 &)  BIND(C,NAME='MPI_File_write_at_all_end_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_ordered(fh, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_write_ordered_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), INTENT(IN) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_ordered_begin(fh, buf, count, datatype, ierror  &
 &)  BIND(C,NAME='MPI_File_write_ordered_begin_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_ordered_end(fh, buf, status, ierror  &
 &)  BIND(C,NAME='MPI_File_write_ordered_end_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: buf ! choice-dummy-argument
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_File_write_shared(fh, buf, count, datatype, status, ierror  &
 &)  BIND(C,NAME='MPI_File_write_shared_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_File), INTENT(IN) :: fh
  TYPE(*), DIMENSION(..), INTENT(IN) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Register_datarep(datarep, read_conversion_fn, write_conversion_fn, dtype_file_extent_fn, extra_state, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Register_datarep_f08')
  USE mpi_f08_types
  USE mpi_f08_callback_prototypes
  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN) :: datarep
  ! EXTERNAL :: read_conversion_fn
  PROCEDURE(MPI_Datarep_conversion_function) :: read_conversion_fn
  ! EXTERNAL :: write_conversion_fn
  PROCEDURE(MPI_Datarep_conversion_function) :: write_conversion_fn
  ! EXTERNAL :: dtype_file_extent_fn
  PROCEDURE(MPI_Datarep_extent_function) :: dtype_file_extent_fn
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: extra_state
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_F_sync_reg(buf  &
 &)  BIND(C,NAME='MPI_F_sync_reg_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Sizeof(x, size, ierror  &
 &)  BIND(C,NAME='MPI_Sizeof_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..) :: x
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: x
  ! !$PRAGMA IGNORE_TKR x
  ! !DIR$ IGNORE_TKR x
  ! !IBM* IGNORE_TKR x
  ! INTEGER, DIMENSION(*) :: x ! choice-dummy-argument
  INTEGER, INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Status_f2f08(f_status, f08_status, ierror  &
 &)  BIND(C,NAME='MPI_Status_f2f08_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: f_status(MPI_STATUS_SIZE)
  TYPE(MPI_Status), INTENT(OUT) :: f08_status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror 
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Status_f082f(f08_status, f_status, ierror  &
 &)  BIND(C,NAME='MPI_Status_f082f_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Status), INTENT(IN) :: f08_status
  INTEGER, INTENT(OUT) :: f_status(MPI_STATUS_SIZE)
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_f90_complex(p, r, newtype, ierror  &
 &)  BIND(C,NAME='MPI_Type_create_f90_complex_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: p, r
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_f90_integer(r, newtype, ierror  &
 &)  BIND(C,NAME='MPI_Type_create_f90_integer_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: r
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_create_f90_real(p, r, newtype, ierror  &
 &)  BIND(C,NAME='MPI_Type_create_f90_real_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: p, r
  TYPE(MPI_Datatype), INTENT(OUT) :: newtype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_match_size(typeclass, size, datatype, ierror  &
 &)  BIND(C,NAME='MPI_Type_match_size_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: typeclass, size
  TYPE(MPI_Datatype), INTENT(OUT) :: datatype
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Pcontrol(level  &
 &)  BIND(C,NAME='MPI_Pcontrol_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: level
 END SUBROUTINE
END INTERFACE

! New MPI-3 routines
! 
INTERFACE
 SUBROUTINE MPI_Iallgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Iallgather_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Iallgatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, request, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Iallgatherv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount
  INTEGER, INTENT(IN), ASYNCHRONOUS :: recvcounts(*), displs(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Iallreduce(sendbuf, recvbuf, count, datatype, op, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Iallreduce_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ialltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Ialltoall_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ialltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, comm, &
                             request, ierror  &
 &)  BIND(C,NAME='MPI_Ialltoallv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ialltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, &
                             comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Ialltoallw_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
  TYPE(MPI_Datatype), INTENT(IN), ASYNCHRONOUS :: sendtypes(*), recvtypes(*)
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ibarrier(comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Ibarrier_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ibcast(buffer, count, datatype, root, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Ibcast_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buffer
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buffer
  ! !$PRAGMA IGNORE_TKR buffer
  ! !DIR$ IGNORE_TKR buffer
  ! !IBM* IGNORE_TKR buffer
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buffer ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, root
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Iexscan(sendbuf, recvbuf, count, datatype, op, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Iexscan_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Igather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Igather_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount, root
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Igatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, root, comm, request, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Igatherv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, root
  INTEGER, INTENT(IN), ASYNCHRONOUS :: recvcounts(*), displs(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ireduce_scatter_block(sendbuf, recvbuf, recvcount, datatype, op, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Ireduce_scatter_block_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ireduce_scatter(sendbuf, recvbuf, recvcounts, datatype, op, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Ireduce_scatter_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN), ASYNCHRONOUS :: recvcounts(*)
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ireduce(sendbuf, recvbuf, count, datatype, op, root, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Ireduce_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count, root
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Iscan(sendbuf, recvbuf, count, datatype, op, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Iscan_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Iscatter(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, root, comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Iscatter_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount, root
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Iscatterv(sendbuf, sendcounts, displs, sendtype, recvbuf, recvcount, recvtype, root, comm, request, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Iscatterv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), displs(*)
  INTEGER, INTENT(IN) :: recvcount, root
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Improbe(source, tag, comm, flag, message, status, ierror  &
 &)  BIND(C,NAME='MPI_Improbe_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: source, tag
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(OUT) :: flag
  TYPE(MPI_Message), INTENT(OUT) :: message
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Mprobe(source, tag, comm, message, status, ierror  &
 &)  BIND(C,NAME='MPI_Mprobe_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: source, tag
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Message), INTENT(OUT) :: message
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Mrecv(buf, count, datatype, message, status, ierror  &
 &)  BIND(C,NAME='MPI_Mrecv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..) :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*) :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Message), INTENT(INOUT) :: message
  TYPE(MPI_Status) :: status
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Imrecv(buf, count, datatype, message, request, ierror  &
 &)  BIND(C,NAME='MPI_Imrecv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: buf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: buf
  ! !$PRAGMA IGNORE_TKR buf
  ! !DIR$ IGNORE_TKR buf
  ! !IBM* IGNORE_TKR buf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: buf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: count
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  TYPE(MPI_Message), INTENT(INOUT) :: message
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Neighbor_allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror  &
 &)  BIND(C,NAME='MPI_Neighbor_allgather_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Neighbor_allgatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Neighbor_allgatherv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcounts(*), displs(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Neighbor_alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, ierror  &
 &)  BIND(C,NAME='MPI_Neighbor_alltoall_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Neighbor_alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, &
                             comm, ierror  &
 &)  BIND(C,NAME='MPI_Neighbor_alltoallv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Neighbor_alltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, &
                             comm, ierror  &
 &)  BIND(C,NAME='MPI_Neighbor_alltoallw_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN) :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN) :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..) :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*) :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcounts(*), recvcounts(*)
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: sdispls(*), rdispls(*)
  ! CAUTION: Please check that all other interfaces also use address-size-integer !!!!
  TYPE(MPI_Datatype), INTENT(IN) :: sendtypes(*), recvtypes(*)
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ineighbor_allgather(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Ineighbor_allgather_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ineighbor_allgatherv(sendbuf, sendcount, sendtype, recvbuf, recvcounts, displs, recvtype, comm, &
                             request, ierror  &
 &)  BIND(C,NAME='MPI_Ineighbor_allgatherv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount
  INTEGER, INTENT(IN), ASYNCHRONOUS :: recvcounts(*), displs(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ineighbor_alltoall(sendbuf, sendcount, sendtype, recvbuf, recvcount, recvtype, comm, request, &
                             ierror  &
 &)  BIND(C,NAME='MPI_Ineighbor_alltoall_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN) :: sendcount, recvcount
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ineighbor_alltoallv(sendbuf, sendcounts, sdispls, sendtype, recvbuf, recvcounts, rdispls, recvtype, &
                             comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Ineighbor_alltoallv_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
  TYPE(MPI_Datatype), INTENT(IN) :: sendtype, recvtype
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Ineighbor_alltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf, recvcounts, rdispls, recvtypes, &
                             comm, request, ierror  &
 &)  BIND(C,NAME='MPI_Ineighbor_alltoallw_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: sendbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: sendbuf
  ! !$PRAGMA IGNORE_TKR sendbuf
  ! !DIR$ IGNORE_TKR sendbuf
  ! !IBM* IGNORE_TKR sendbuf
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: sendbuf ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: recvbuf
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: recvbuf
  ! !$PRAGMA IGNORE_TKR recvbuf
  ! !DIR$ IGNORE_TKR recvbuf
  ! !IBM* IGNORE_TKR recvbuf
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: recvbuf ! choice-dummy-argument
  INTEGER, INTENT(IN), ASYNCHRONOUS :: sendcounts(*), recvcounts(*)
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN), ASYNCHRONOUS :: sdispls(*), rdispls(*)
  ! CAUTION: Please check that all other interfaces also use address-size-integer !!!!
  TYPE(MPI_Datatype), INTENT(IN), ASYNCHRONOUS :: sendtypes(*), recvtypes(*)
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_size_x(datatype, size, ierror  &
 &)  BIND(C,NAME='MPI_Type_size_x_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND=MPI_COUNT_KIND), INTENT(OUT) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_extent_x(datatype, lb, extent, ierror  &
 &)  BIND(C,NAME='MPI_Type_get_extent_x_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND = MPI_COUNT_KIND), INTENT(OUT) :: lb, extent
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Type_get_true_extent_x(datatype, true_lb, true_extent, ierror  &
 &)  BIND(C,NAME='MPI_Type_get_true_extent_x_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND = MPI_COUNT_KIND), INTENT(OUT) :: true_lb, true_extent
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_elements_x(status, datatype, count, ierror  &
 &)  BIND(C,NAME='MPI_Get_elements_x_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Status), INTENT(IN) :: status
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND = MPI_COUNT_KIND), INTENT(OUT) :: count
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Status_set_elements_x(status, datatype, count, ierror  &
 &)  BIND(C,NAME='MPI_Status_set_elements_x_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Status), INTENT(INOUT) :: status
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER(KIND = MPI_COUNT_KIND), INTENT(IN) :: count
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_allocate(size, disp_unit, info, comm, baseptr, win, ierror  &
 &)  BIND(C,NAME='MPI_Win_allocate_f08')
  USE mpi_f08_types
  USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_PTR
  IMPLICIT NONE
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
  INTEGER, INTENT(IN) :: disp_unit
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(C_PTR), INTENT(OUT) :: baseptr
  TYPE(MPI_Win), INTENT(OUT) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_create_dynamic(info, comm, win, ierror  &
 &)  BIND(C,NAME='MPI_Win_create_dynamic_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(MPI_Comm), INTENT(IN) :: comm
  TYPE(MPI_Win), INTENT(OUT) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_attach(win, base, size, ierror  &
 &)  BIND(C,NAME='MPI_Win_attach_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(*), ASYNCHRONOUS :: base
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: size
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_detach(win, base, ierror  &
 &)  BIND(C,NAME='MPI_Win_detach_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(*), ASYNCHRONOUS :: base
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Get_accumulate(origin_addr, origin_count, origin_datatype, result_addr, result_count, result_datatype, &
                             target_rank, target_disp, target_count, target_datatype, op, win, ierror  &
 &)  BIND(C,NAME='MPI_Get_accumulate_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
  ! !$PRAGMA IGNORE_TKR origin_addr
  ! !DIR$ IGNORE_TKR origin_addr
  ! !IBM* IGNORE_TKR origin_addr
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: origin_addr ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: result_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: result_addr
  ! !$PRAGMA IGNORE_TKR result_addr
  ! !DIR$ IGNORE_TKR result_addr
  ! !IBM* IGNORE_TKR result_addr
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: result_addr ! choice-dummy-argument
  INTEGER, INTENT(IN) :: origin_count, result_count, target_rank, target_count
  TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype, result_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Fetch_and_op(origin_addr, result_addr, datatype, target_rank, target_disp, op, win, ierror  &
 &)  BIND(C,NAME='MPI_Fetch_and_op_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
  ! !$PRAGMA IGNORE_TKR origin_addr
  ! !DIR$ IGNORE_TKR origin_addr
  ! !IBM* IGNORE_TKR origin_addr
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: origin_addr ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: result_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: result_addr
  ! !$PRAGMA IGNORE_TKR result_addr
  ! !DIR$ IGNORE_TKR result_addr
  ! !IBM* IGNORE_TKR result_addr
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: result_addr ! choice-dummy-argument
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: target_rank
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Compare_and_swap(origin_addr, compare_addr, result_addr, datatype, target_rank, target_disp, &
                             win, ierror  &
 &)  BIND(C,NAME='MPI_Compare_and_swap_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr, compare_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr, compare_addr
  ! !$PRAGMA IGNORE_TKR origin_addr, compare_addr
  ! !DIR$ IGNORE_TKR origin_addr, compare_addr
  ! !IBM* IGNORE_TKR origin_addr, compare_addr
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: origin_addr, compare_addr ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: result_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: result_addr
  ! !$PRAGMA IGNORE_TKR result_addr
  ! !DIR$ IGNORE_TKR result_addr
  ! !IBM* IGNORE_TKR result_addr
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: result_addr ! choice-dummy-argument
  TYPE(MPI_Datatype), INTENT(IN) :: datatype
  INTEGER, INTENT(IN) :: target_rank
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Rput(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, &
                             win, request, ierror  &
 &)  BIND(C,NAME='MPI_Rput_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
  ! !$PRAGMA IGNORE_TKR origin_addr
  ! !DIR$ IGNORE_TKR origin_addr
  ! !IBM* IGNORE_TKR origin_addr
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: origin_addr ! choice-dummy-argument
  INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Rget(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, target_datatype, &
                             win, request, ierror  &
 &)  BIND(C,NAME='MPI_Rget_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: origin_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
  ! !$PRAGMA IGNORE_TKR origin_addr
  ! !DIR$ IGNORE_TKR origin_addr
  ! !IBM* IGNORE_TKR origin_addr
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: origin_addr ! choice-dummy-argument
  INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Raccumulate(origin_addr, origin_count, origin_datatype, target_rank, target_disp, target_count, &
                             target_datatype, op, win, request, ierror  &
 &)  BIND(C,NAME='MPI_Raccumulate_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
  ! !$PRAGMA IGNORE_TKR origin_addr
  ! !DIR$ IGNORE_TKR origin_addr
  ! !IBM* IGNORE_TKR origin_addr
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: origin_addr ! choice-dummy-argument
  INTEGER, INTENT(IN) :: origin_count, target_rank, target_count
  TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Rget_accumulate(origin_addr, origin_count, origin_datatype, result_addr, result_count, result_datatype, &
                             target_rank, target_disp, target_count, target_datatype, op, win, request, ierror  &
 &)  BIND(C,NAME='MPI_Rget_accumulate_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(*), DIMENSION(..), INTENT(IN), ASYNCHRONOUS :: origin_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: origin_addr
  ! !$PRAGMA IGNORE_TKR origin_addr
  ! !DIR$ IGNORE_TKR origin_addr
  ! !IBM* IGNORE_TKR origin_addr
  ! INTEGER, DIMENSION(*), INTENT(IN), ASYNCHRONOUS :: origin_addr ! choice-dummy-argument
  TYPE(*), DIMENSION(..), ASYNCHRONOUS :: result_addr
  ! !DEC$ ATTRIBUTES NO_ARG_CHECK :: result_addr
  ! !$PRAGMA IGNORE_TKR result_addr
  ! !DIR$ IGNORE_TKR result_addr
  ! !IBM* IGNORE_TKR result_addr
  ! INTEGER, DIMENSION(*), ASYNCHRONOUS :: result_addr ! choice-dummy-argument
  INTEGER, INTENT(IN) :: origin_count, result_count, target_rank, target_count
  TYPE(MPI_Datatype), INTENT(IN) :: origin_datatype, target_datatype, result_datatype
  INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: target_disp
  TYPE(MPI_Op), INTENT(IN) :: op
  TYPE(MPI_Win), INTENT(IN) :: win
  TYPE(MPI_Request), INTENT(OUT) :: request
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_lock_all(assert, win, ierror  &
 &)  BIND(C,NAME='MPI_Win_lock_all_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: assert
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_unlock_all(win, ierror  &
 &)  BIND(C,NAME='MPI_Win_unlock_all_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_flush(rank, win, ierror  &
 &)  BIND(C,NAME='MPI_Win_flush_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: rank
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_flush_all(win, ierror  &
 &)  BIND(C,NAME='MPI_Win_flush_all_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_flush_local(rank, win, ierror  &
 &)  BIND(C,NAME='MPI_Win_flush_local_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: rank
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_flush_local_all(win, ierror  &
 &)  BIND(C,NAME='MPI_Win_flush_local_all_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Win_sync(win, ierror  &
 &)  BIND(C,NAME='MPI_Win_sync_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Win), INTENT(IN) :: win
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror
 END SUBROUTINE
END INTERFACE

INTERFACE
 SUBROUTINE MPI_Comm_split_type(comm, comm_type, key, info, newcomm, ierror  &
 &)  BIND(C,NAME='MPI_Comm_split_type_f08')
  USE mpi_f08_types
  IMPLICIT NONE
  TYPE(MPI_Comm), INTENT(IN) :: comm
  INTEGER, INTENT(IN) :: comm_type, key
  TYPE(MPI_Info), INTENT(IN) :: info
  TYPE(MPI_Comm), INTENT(OUT) :: newcomm
  INTEGER, OPTIONAL, INTENT(OUT) :: ierror 
 END SUBROUTINE
END INTERFACE


END MODULE mpi_f08 
