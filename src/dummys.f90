PROGRAM dummys
  use mo_mpi,      ONLY : start_mpi, stop_mpi, p_mpi_offset,     &
                          p_barrier, abort_mpi
  use mo_domdcomp, ONLY : domdcomp_create
#ifdef __pnetcdf
  use mo_nc,       ONLY : read_field, get_timelen, write_field
#endif
#ifdef __netcdf
  use mo_nc4,      ONLY : read_field, get_timelen, write_field
#endif
  use mo_coupling, ONLY : coupler_def_grid_reg, coupler_end_def, &
                          coupler_def_field, coupler_put_fields

  implicit none

  integer :: g_nx, g_ny, nprocx, nprocy, nsteps

  integer :: work_com, rank, nprocs, n
#ifdef __pnetcdf
  integer(kind=p_mpi_offset) :: tlen
#endif
#ifdef __netcdf
  integer :: tlen
#endif
  integer :: count_x, count_y, start_x, start_y
  integer :: field_id

  double precision, allocatable, dimension(:,:,:) :: strd

  ! initialize MPI and YAC (when used)
  call start_mpi("DUMMY_SENDER", work_com, rank, nprocs)

  ! Read setup
  call read_setup(g_nx, g_ny, nprocx, nprocy, nsteps)

  if ( nprocx*nprocy /= nprocs ) then
    write(*,*) 'nprocx*nprocy /= nprocs'
    call abort_mpi
  end if

  ! basic domain decomposition
  call domdcomp_create(g_nx, g_ny, nprocx, nprocy, rank, &
                       count_x, count_y, start_x, start_y)

  ! read grid from file


  ! define grid for YAC
  call coupler_def_grid_reg('grid_sender', count_x, count_y, start_x, start_y, &
                            g_nx, g_ny)

  ! define fields for YAC
  call coupler_def_field('strd', field_id)


  ! end of definition phase in YAC
  call coupler_end_def()

  ! Get time dimension length from NetCDF file
  call get_timelen("strd.nc", work_com, tlen)
  allocate(strd(1:count_x,1:count_y,1:tlen))

  ! read fields from files
  call read_field("strd.nc", "strd", strd, work_com, &
                  start_x, start_y, count_x, count_y)

  ! time loop
  do n = 1,nsteps
    ! put fields via YAC
    call coupler_put_fields(field_id, strd(:,:,n), count_x, count_y)
  end do

  call write_field("strd_s.nc", "strd", strd, work_com, g_nx, g_ny, &
                   start_x, start_y, count_x, count_y, nsteps)

  ! finalize MPI and YAC (when used)
  call stop_mpi

  contains

  subroutine read_setup(g_nx, g_ny, nprocx, nprocy, nsteps)
    integer, intent(out) :: g_nx, g_ny, nprocx, nprocy, nsteps
    character(7) :: cinput

    open(9,file='inputs.setup',status='old')
    read(9,*) cinput,g_nx
    read(9,*) cinput,g_ny
    read(9,*) cinput,nprocx
    read(9,*) cinput,nprocy
    read(9,*) cinput,nsteps
    close(9)

  end subroutine read_setup

END PROGRAM dummys
