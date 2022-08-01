MODULE mo_nc4
#ifdef __netcdf
  use netcdf

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
      write(6,*) trim(message), trim(nf90_strerror(err))
      call abort_mpi
    end if
  end subroutine check

  subroutine get_timelen(filename, comm, timelen)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    integer, intent(in) :: comm
    integer, INTENT(OUT) :: timelen

    integer :: fid, timeid
    
    call check( nf90_open_par(trim(filename), NF90_NOWRITE, comm, p_mpi_info_null, fid), &
                'In nf90_open_par : ' )

    call check( nf90_inq_dimid(fid, 'time', timeid), &
                'In nf90_inq_dimid (time) :' )

    call check( nf90_inquire_dimension(fid, timeid, len=timelen), &
                'In nf90_inquire_dimension (time) :' )

    call check( nf90_close(fid), &
                'In nf90_close : ' )

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
    integer, dimension(3) :: start, count
    integer :: timelen

    call check( nf90_open_par(trim(filename), NF90_NOWRITE, comm, p_mpi_info_null, fid), &
                'In nf90_open_par : ' )

    call check( nf90_inq_dimid(fid, 'time', timeid), &
                'In nf90_inq_dimid (time) :' )

    call check( nf90_inquire_dimension(fid, timeid, len=timelen), &
                'In nf90_inquire_dimension (time) :' )

    start(1) = istart;  start(2) = jstart;  start(3) = 1;
    count(1) = count_x; count(2) = count_y; count(3) = timelen;

    call check( nf90_inq_varid(fid, fieldname, varid), &
                'In nf90_inq_varid : ' )

    call check( nf90_get_var(fid, varid, field(1:count_x,1:count_y,1:timelen), &
                                start=start, count=count), &
                'In nf90_get_var : ' )

    call check( nf90_close(fid), &
                'In nf90_close : ' )

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
    integer, dimension(3) :: start, count

    cmode = IOR(NF90_CLOBBER, NF90_NETCDF4)
    call check( nf90_create_par(filename, cmode, comm, p_mpi_info_null, fid), &
                'In nf90_create_par : ' )

    call check( nf90_def_dim(fid, 'x', g_nx, dimid(1)) , &
                'In nf90_def_dim : ' )

    call check( nf90_def_dim(fid, 'y', g_ny, dimid(2)) , &
                'In nf90_def_dim : ' )

    call check( nf90_def_dim(fid, 'time', NF90_UNLIMITED, dimid(3)), &
                'In nf90_def_dim : ' )

    call check( nf90_def_var(fid, trim(fieldname), NF90_DOUBLE, dimid, varid), &
                'In nf90_def_var : ' )

    call check( nf90_enddef(fid), &
                'In nf90_enddef : ' )

    start(1) = istart;  start(2) = jstart; start(3) = 1;
    count(1) = count_x; count(2) = count_y; count(3) = nsteps;

    call check( nf90_var_par_access(fid, varid, NF90_COLLECTIVE), &
                'In nf90_var_par_access : ' )

    call check( nf90_put_var(fid, varid, field(1:count_x,1:count_y,1:nsteps), &
                                start=start, count=count), &
                'nf90_put_var : ')

    call check( nf90_close(fid), &
                'In nf90_close : ' )

  end subroutine write_field
#endif
END MODULE mo_nc4
