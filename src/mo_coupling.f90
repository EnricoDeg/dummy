MODULE mo_coupling
#ifdef __coupling
  use mpi
  use mo_yac_finterface, ONLY: yac_finit_comm_group, yac_fget_groupcomm,      &
                               yac_ffinalize, yac_finit, yac_fget_localcomm,  &
                               yac_fdef_comp, yac_fput, yac_fget,             &
                               yac_fdef_grid_reg2d_r8, yac_fset_global_index, &
                               YAC_LOCATION_CELL, yac_fdef_field,             &
                               yac_fset_core_mask, yac_fdef_points_reg2d_r8
#endif
  implicit none

  private

  CHARACTER(LEN=*), PARAMETER :: xml_filename = "coupling.xml"
  CHARACTER(LEN=*), PARAMETER :: xsd_filename = "coupling.xsd"
  integer :: comp_id, grid_id, point_id(1), mask_id

  public :: coupler_init, coupler_finalize
  public :: coupler_def_grid_reg, coupler_end_def
  public :: coupler_def_field, coupler_def_field_masked
  public :: coupler_put_fields, coupler_get_fields

  contains

  subroutine coupler_init(g_comm, global_name)
    integer, intent(inout)       :: g_comm
    CHARACTER(LEN=*), INTENT(IN) :: global_name
#ifdef __coupling
!    integer :: group_comm
!    integer :: ierr, result
    integer :: local_comm

    call yac_finit( TRIM(xml_filename), TRIM(xsd_filename) )
    call yac_fdef_comp ( global_name, comp_id )
    call yac_fget_localcomm ( local_comm, comp_id )
    g_comm = local_comm

!   Might be used later on
!    call yac_finit_comm_group( TRIM(xml_filename), TRIM(xsd_filename), &
!                               g_comm, global_name)
!    call yac_fdef_comp( global_name, comp_id )
!    write(*,*) "Component ", global_name, " has ID ", comp_id
!    call yac_fget_groupcomm(group_comm)
!    call mpi_comm_compare(g_comm, group_comm, result, ierr)
!    IF ((result /= MPI_IDENT) .AND. (result /= MPI_CONGRUENT)) THEN
!      g_comm = group_comm
!    END IF
#endif
  end subroutine coupler_init



  subroutine coupler_finalize
#ifdef __coupling
    CALL yac_ffinalize
#endif
  end subroutine coupler_finalize



  subroutine coupler_end_def
    integer :: error_status
#ifdef __coupling
    call yac_fsearch ( error_status )
#endif
  end subroutine coupler_end_def



  subroutine coupler_def_grid_reg(grid_name, count_x, count_y, start_x, start_y, &
                                  g_nx, g_ny)
    CHARACTER(LEN=*), INTENT(IN) :: grid_name
    integer, intent(in) :: count_x
    integer, intent(in) :: count_y
    integer, intent(in) :: start_x
    integer, intent(in) :: start_y
    integer, intent(in) :: g_nx
    integer, intent(in) :: g_ny

    integer, dimension(2) :: nbr_points, cyclic, nbr_cells
    integer :: i,j
    double precision :: cpi = 4.D0*DATAN(1.D0)
    double precision, allocatable, dimension(:) :: x_vertices, y_vertices
    double precision, allocatable, dimension(:) :: x_points, y_points
    double precision, allocatable, dimension(:) :: g_x_points, g_y_points
    integer, allocatable, dimension(:)   :: local_global_id_rank
    integer, allocatable, dimension(:,:) :: global_global_id
    logical, allocatable, dimension(:) :: cell_core_mask

#ifdef __coupling
    nbr_points = (/ count_x+1, count_y+1 /)
    nbr_cells  = (/ count_x, count_y /)
    cyclic = (/ 0, 0 /)

    allocate(g_x_points(g_nx), g_y_points(g_ny))
    allocate(x_points(count_x+1), y_points(count_y+1))
    allocate(x_vertices(count_x), y_vertices(count_y))
    allocate(local_global_id_rank(count_x*count_y))
    allocate(global_global_id(g_nx,g_ny))
    allocate(cell_core_mask(count_x*count_y))

    do i = 1,g_nx
      g_x_points(i) = - CPI / 2.0D0 + (CPI / g_nx) * (i-1)
    end do
    g_x_points(g_nx+1) = CPI / 2.0D0

    do i = 1,g_ny
      g_y_points(i) = - CPI / 2.0D0 + (CPI / g_ny) * (i-1)
    end do
    g_y_points(g_ny+1) = CPI / 2.0D0

    x_points = g_x_points(start_x:start_x+count_x)
    y_points = g_y_points(start_y:start_y+count_y)

    x_vertices = ( g_x_points(start_x:start_x+count_x-1) + &
                   g_x_points(start_x+1:start_x+count_x) ) / 2
    y_vertices = ( g_y_points(start_y:start_y+count_y-1) + &
                   g_y_points(start_y+1:start_y+count_y) ) / 2

    call yac_fdef_grid_reg2d_r8(grid_name, nbr_points, cyclic, x_points, y_points, grid_id)

    do j = 1,g_ny
      do i = 1,g_nx
        global_global_id(i,j) = i + (j-1) * g_nx
      end do
    end do

    do j = 1,count_y
      do i = 1,count_x
        local_global_id_rank(i+(j-1)*count_x) = global_global_id(start_x+i-1,start_y+j-1)
      end do
    end do

    call yac_fset_global_index(local_global_id_rank, YAC_LOCATION_CELL, grid_id)

    cell_core_mask(:) = .true.
    call yac_fset_core_mask(cell_core_mask, YAC_LOCATION_CELL, grid_id)

    call yac_fdef_points_reg2d_r8(grid_id, nbr_cells, YAC_LOCATION_CELL, x_vertices, y_vertices, point_id(1))

    deallocate(cell_core_mask)
    deallocate(global_global_id)
    deallocate(local_global_id_rank)
    deallocate(x_vertices, y_vertices)
    deallocate(x_points, y_points)
    deallocate(g_x_points, g_y_points)
#endif
  end subroutine coupler_def_grid_reg



  subroutine coupler_def_field_masked(field_name, field_id)
    CHARACTER(LEN=*), INTENT(IN) :: field_name
    integer, intent(out) :: field_id
#ifdef __coupling    
    call yac_fdef_field_mask( TRIM(field_name), & 
                              comp_id,          & ! from yac_fdef_comp()
                              point_id(1),      & ! from yac_fdef_points()
                              mask_id,          & ! yac_fdef_mask()
                              1,                & ! number of pointsets per grid
                              field_id          & ! [out] - to yac_fput() / yac_fget()
                              )
#else
    field_id = -1
#endif
  end subroutine coupler_def_field_masked



  subroutine coupler_def_field(field_name, field_id)
    CHARACTER(LEN=*), INTENT(IN) :: field_name
    integer, intent(out) :: field_id
#ifdef __coupling
    call yac_fdef_field( TRIM(field_name), &
                         comp_id,          &
                         point_id(1),      &
                         1,                &
                         field_id          &
                       )
#else
    field_id = -1
#endif
  end subroutine coupler_def_field



  subroutine coupler_put_fields(field_id, field, count_x, count_y)
    integer, intent(in)                           :: field_id
    double precision, intent(in), dimension(:,:)  :: field
    integer, intent(in)                           :: count_x
    integer, intent(in)                           :: count_y
    double precision, allocatable, dimension(:,:) :: buffer
    integer :: i, j, n_hor_cells, p, info, ierr
#ifdef __coupling
    n_hor_cells = count_x * count_y
    allocate(buffer(1:n_hor_cells,1:1))
    buffer(:,:) = 0.0D0

    p = 1
    do j = 1,count_y
      do i = 1,count_x
        buffer(p,1) = field(i,j)
        p = p + 1
      end do
    end do

    call yac_fput(field_id, n_hor_cells, 1, buffer(1:n_hor_cells,1:1), info, ierr)

    deallocate(buffer)
#endif
  end subroutine coupler_put_fields

  subroutine coupler_get_fields(field_id, field, count_x, count_y)
    integer, intent(in)                              :: field_id
    double precision, intent(inout), dimension(:,:)  :: field
    integer, intent(in)                              :: count_x
    integer, intent(in)                              :: count_y
    double precision, allocatable, dimension(:,:)    :: buffer
    integer :: i, j, n_hor_cells, p, info, ierr
#ifdef __coupling
    n_hor_cells = count_x * count_y
    allocate(buffer(1:n_hor_cells,1:1))
    buffer(:,:) = 0.0D0

    call yac_fget(field_id, n_hor_cells, 1, buffer(1:n_hor_cells,1:1), info, ierr)

    p = 1
    do j = 1,count_y
      do i = 1,count_x
        field(i,j) = buffer(p,1)
        p = p + 1
      end do
    end do

    deallocate(buffer)
#endif
  end subroutine coupler_get_fields

END MODULE mo_coupling
