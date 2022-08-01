MODULE mo_mpi
  use mpi
#ifdef __coupling
  use mo_coupling, ONLY : coupler_init, coupler_finalize
#endif
  implicit none
  private

  integer :: ierr
  integer, parameter :: p_mpi_offset = MPI_OFFSET_KIND
  integer, parameter :: p_mpi_info_null = MPI_INFO_NULL

  public :: start_mpi, stop_mpi, abort_mpi, p_mpi_offset, p_mpi_info_null
  public :: p_barrier

  contains

  subroutine start_mpi(global_name, g_comm, rank, nprocs)

    CHARACTER(LEN=*), INTENT(IN) :: global_name
    integer, intent(out)         :: g_comm
    integer, intent(out)         :: rank
    integer, intent(out)         :: nprocs

    call mpi_init(ierr)
    g_comm = MPI_COMM_WORLD

#ifdef __coupling
    call coupler_init(g_comm, global_name)
#endif

    call mpi_comm_rank(g_comm, rank, ierr)
    call mpi_comm_size(g_comm, nprocs, ierr)

  end subroutine start_mpi



  subroutine stop_mpi

    integer :: iexit = 0

    call p_barrier()

#ifdef __coupling
    call coupler_finalize()
#endif

    call mpi_finalize(ierr)

    IF (ierr /= MPI_SUCCESS) THEN
      iexit = 1
      WRITE (*,'(a)') ' MPI_FINALIZE failed.'
      WRITE (*,'(a,i4)') ' Error = ', ierr
      CALL abort_mpi
    END IF

    CALL exit(iexit)

  end subroutine stop_mpi



  subroutine abort_mpi

    call mpi_abort(MPI_COMM_WORLD, 0, ierr)
    IF (ierr /= MPI_SUCCESS) THEN
       WRITE (*,'(a)') ' MPI_ABORT failed.'
       WRITE (*,'(a,i4)') ' Error =  ', ierr
    END IF
    call exit(1)

  end subroutine abort_mpi



  subroutine p_barrier(ocom)
    integer, optional :: ocom
    integer :: com

    if (present(ocom)) then
      com = ocom
    else
      com = MPI_COMM_WORLD
    end if

    call mpi_barrier(com, ierr)

    IF (ierr /= MPI_SUCCESS) THEN
      WRITE (*,'(a,i4,a)') ' MPI_BARRIER failed.'
      WRITE (*,'(a,i4)') ' Error = ', ierr
      CALL abort_mpi
    END IF

  end subroutine p_barrier

END MODULE mo_mpi
