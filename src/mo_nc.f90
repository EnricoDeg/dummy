MODULE mo_nc
#ifdef __pnetcdf
  use pnetcdf
  use mo_mpi, ONLY : abort_mpi, p_mpi_offset, p_mpi_info_null
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: read_field, get_timelen, write_field
  CONTAINS

  subroutine check(err, message)
    integer          :: err
    character(len=*) :: message

    ! check returned value for possible error
    if (err .NE. NF90_NOERR) then
      write(6,*) trim(message), trim(nf90mpi_strerror(err))
      call abort_mpi
    end if
  end subroutine check

  subroutine get_timelen(filename, comm, timelen)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    integer, intent(in) :: comm
    integer(kind=p_mpi_offset), INTENT(OUT) :: timelen

    integer :: fid, timeid
    
    call check( nf90mpi_open(comm, trim(filename), NF90_NOWRITE, p_mpi_info_null, fid), &
                'In nf90mpi_open : ' )

    call check( nf90mpi_inq_dimid(fid, 'time', timeid), &
                'In nf90mpi_inq_dimid (time) :' )

    call check( nf90mpi_inquire_dimension(fid, timeid, len=timelen), &
                'In nf90mpi_inquire_dimension (time) :' )

    call check( nf90mpi_close(fid), &
                'In nf90mpi_close : ' )
    
  end subroutine get_timelen

  subroutine read_field(filename, fieldname, field, comm, &
                        istart, jstart, count_x, count_y)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    CHARACTER(LEN=*), INTENT(IN) :: fieldname
    double precision, intent(inout), dimension(:,:,:) :: field
    integer, intent(in) :: comm
    integer, intent(in) :: istart, jstart
    integer, intent(in) :: count_x, count_y

    integer :: fid, timeid, varid
    integer(kind=p_mpi_offset), dimension(3) :: start, count
    integer(kind=p_mpi_offset) :: timelen

    call check( nf90mpi_open(comm, trim(filename), NF90_NOWRITE, p_mpi_info_null, fid), &
                'In nf90mpi_open : ' )

    call check( nf90mpi_inq_dimid(fid, 'time', timeid), &
                'In nf90mpi_inq_dimid (time) :' )

    call check( nf90mpi_inquire_dimension(fid, timeid, len=timelen), &
                'In nf90mpi_inquire_dimension (time) :' )

    start(1) = istart;  start(2) = jstart;  start(3) = 1;
    count(1) = count_x; count(2) = count_y; count(3) = timelen;

    call check( nf90mpi_inq_varid(fid, fieldname, varid), &
                'In nf90mpi_inq_varid : ' )

    call check( nf90mpi_begin_indep_data(fid), &
                'In nf90mpi_begin_indep_data : ' )

    call check( nf90mpi_get_var(fid, varid, field(1:count_x,1:count_y,1:timelen), &
                                start=start, count=count), &
                'In nf90mpi_get_var : ' )

    call check( nf90mpi_end_indep_data(fid), &
                'In nf90mpi_end_indep_data : ' )

    call check( nf90mpi_close(fid), &
                'In nf90mpi_close : ' )

  end subroutine read_field

  subroutine write_field(filename, fieldname, field, comm, g_nx, g_ny, &
                         istart, jstart, count_x, count_y, nsteps)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    CHARACTER(LEN=*), INTENT(IN) :: fieldname
    double precision, intent(inout), dimension(:,:,:) :: field
    integer, intent(in) :: comm
    integer, intent(in) :: g_nx
    integer, intent(in) :: g_ny
    integer, intent(in) :: istart
    integer, intent(in) :: jstart
    integer, intent(in) :: count_x
    integer, intent(in) :: count_y
    integer, intent(in) :: nsteps

    integer :: cmode, fid, varid
    integer, dimension(3) :: dimid
    integer(kind=p_mpi_offset), dimension(3) :: start, count

    cmode = IOR(NF90_CLOBBER, NF90_64BIT_DATA)
    call check( nf90mpi_create(comm, filename, cmode, p_mpi_info_null, fid), &
                'In nf90mpi_create : ' )

    call check( nf90mpi_def_dim(fid, 'x', int(g_nx,kind=p_mpi_offset), dimid(1)) , &
                'In nf90mpi_def_dim : ' )

    call check( nf90mpi_def_dim(fid, 'y', int(g_ny,kind=p_mpi_offset), dimid(2)) , &
                'In nf90mpi_def_dim : ' )

    call check( nf90mpi_def_dim(fid, 'time', NF_UNLIMITED, dimid(3)), &
                'In nf90mpi_def_dim : ' )

    call check( nf90mpi_def_var(fid, trim(fieldname), NF90_DOUBLE, dimid, varid), &
                'In nf90mpi_def_var : ' )

    call check( nf90mpi_enddef(fid), &
                'In nf90mpi_enddef : ' )

    start(1) = istart;  start(2) = jstart; start(3) = 1;
    count(1) = count_x; count(2) = count_y; count(3) = nsteps;

    call check( nf90mpi_begin_indep_data(fid), &
                'In nf90mpi_begin_indep_data : ' )

    call check( nf90mpi_put_var(fid, varid, field(1:count_x,1:count_y,1:nsteps), &
                                start=start, count=count), &
                'nf90mpi_put_var : ')

    call check( nf90mpi_end_indep_data(fid), &
                'In nf90mpi_end_indep_data : ' )

    call check( nf90mpi_close(fid), &
                'In nf90mpi_close : ' )

  end subroutine write_field
#endif
END MODULE mo_nc
