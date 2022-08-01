MODULE mo_domdcomp
  implicit none
  private

  public :: domdcomp_create

  contains

  subroutine domdcomp_create(g_nx, g_ny, nprocx, nprocy, rank, &
                             count_x, count_y, start_x, start_y)
    integer, intent(in)  :: g_nx, g_ny, nprocx, nprocy, rank
    integer, intent(out) :: count_x, count_y, start_x, start_y

    integer :: coord_x, coord_y

    coord_x = MOD(rank, nprocx)
    coord_y = rank / nprocx

    count_x = g_nx / nprocx
    if ( coord_x == (nprocx-1) ) count_x = g_nx / nprocx + MOD(g_nx, nprocx)

    count_y = g_ny / nprocy
    if ( coord_y == (nprocy-1) ) count_y = g_ny / nprocy + MOD(g_ny, nprocy)

    start_x = coord_x * g_nx / nprocx + 1
    start_y = coord_y * g_ny / nprocy + 1

    write(*,*) '---------- Domain Decomposition ----------'
    write(*,*) 'istart = ', start_x, ' --- jstart = ', start_y, ' --- from rank ', rank
    write(*,*) 'count_x = ', count_x, ' --- count_y = ', count_y, ' --- from rank ', rank

  end subroutine domdcomp_create

END MODULE mo_domdcomp
