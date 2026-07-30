program main
  use lib_const
  use lib_base
  use lib_log
  use lib_io
  use c1_grid, only: &
        get_cellsize_ratio
  use c2_jflw_const, only: &
        DGT_GXY
  use c2_jflw_const, only: &
        set_resolution
  use c2_jflw_grid, only: &
        west_of_gx, &
        east_of_gx, &
        south_of_gy, &
        north_of_gy
  use c3_joint_io, only: &
        get_f_nwk_info, &
        get_f_nwk_map, &
        get_f_nwk_map_rri
! scaleUp.f90
!
! coded by T.Sayama on July 2, 2013
! update on Jan. 16, 2014
! update on May. 21, 2015
!
implicit none

!character(256) infile
!parameter( infile = "scaleUp.txt" )
character(16) :: gid
character(8) :: resl_in, resl_out

! variable definitions

!character(256) infile_dem, infile_dir, infile_acc
!character(256) infile_dem2, infile_dir2, infile_acc2

!character(256) outfile_dem, outfile_dir, outfile_acc
character(256) outfile_dem2, outfile_dir2, outfile_acc2

integer ups, corner

integer nx, ny, nx2, ny2
integer :: xs, xe, ys, ye
integer :: nx_org, ny_org
integer :: gxs_org, gxe_org, gys_org, gye_org
integer :: gxs, gxe, gys, gye
integer :: gxs2, gxe2, gys2, gye2
!real*8 xllcorner, yllcorner, cellsize
real(8) west, east, south, north, cellsize
!real*8 xllcorner2, yllcorner2, cellsize2
real(8) west2, east2, south2, north2, cellsize2
!real*8 nodata_dem
real(4) nodata_dem
integer nodata_dir, nodata_acc

!real*8, dimension(:,:), allocatable :: dem, dem2
real(4), dimension(:,:), allocatable :: dem, dem2
integer, dimension(:,:), allocatable :: dir, acc, dir2, acc2, upstream2, acc_dif

integer, dimension(:,:), allocatable :: left, right, top, bottom
integer, dimension(:,:), allocatable :: tile, accmax, am2_i, am2_j, pass2

integer i_next, j_next, ii_next, jj_next

integer i, j, ii, jj, iii, jjj, k, count
character(256) ctemp
character(CLEN_PATH) :: f
integer :: un
integer :: dgt_xy, dgt_xy2, dgt_gxy2

! STEP 0 : Open File

!open(1, file = infile, status = "old")
!read(1, '(a)') infile_dem
!read(1, '(a)') infile_dir
!read(1, '(a)') infile_acc
!read(1, *) ups
!read(1, '(a)') outfile_dem2
!read(1, '(a)') outfile_dir2
!read(1, '(a)') outfile_acc2
!close(1)

  resl_in = argument(1)
  resl_out = argument(2)
  gid = argument(3)

  ups = get_cellsize_ratio(resl_in, resl_out)

  if( ups == 1 )then
    call logmsg('Same resolution.')
    stop
  endif

  call set_resolution(resl_in)

  !infile_dem = get_f_nwk_map_rri(resl_in, gid, 'elv')
  !infile_dir = get_f_nwk_map_rri(resl_in, gid, 'dir')
  !infile_acc = get_f_nwk_map_rri(resl_in, gid, 'upg')

  outfile_dem2 = get_f_nwk_map_rri(resl_out, gid, 'elv')
  outfile_dir2 = get_f_nwk_map_rri(resl_out, gid, 'dir')
  outfile_acc2 = get_f_nwk_map_rri(resl_out, gid, 'upg')

! STEP 1 : Read Files
  f = get_f_nwk_info(resl_in, gid)
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, status='old')
  read(un,*) ctemp, nx_org, ctemp, gxs_org, gxe_org
  read(un,*) ctemp, ny_org, ctemp, gys_org, gye_org
  read(un,*) ctemp, west, east, south, north
  read(un,*) ctemp, nodata_dem
  read(un,*) ctemp, nodata_dir
  read(un,*) ctemp, nodata_acc
  close(un)

  cellsize = (east - west) / nx_org

  call get_mesh_range(gxs_org, gxe_org, ups, gxs, gxe)
  call get_mesh_range(gys_org, gye_org, ups, gys, gye)
  nx = gxe - gxs + 1
  ny = gye - gys + 1
  dgt_xy = dgt(max(nx,ny))
  call logmsg(str(resl_in)//':')
  call logmsg('  (org) ('//str((/nx_org,ny_org/),dgt_xy,',')//') ['//&
              str((/gxs_org,gxe_org/),DGT_GXY,':')//','//&
              str((/gys_org,gye_org/),DGT_GXY,':')//']'//&
            '\n  (new) ('//str((/nx,ny/),dgt_xy,',')//') ['//&
              str((/gxs,gxe/),DGT_GXY,':')//','//&
              str((/gys,gye/),DGT_GXY,':')//']')
  xs = gxs_org - gxs + 1
  xe = xs + nx_org - 1
  ys = gys_org - gys + 1
  ye = ys + ny_org - 1

  nx2 = nx / ups
  ny2 = ny / ups
  dgt_xy2 = dgt(max(nx2,ny2))

  gxs2 = gxs / ups + 1
  gxe2 = gxe / ups
  gys2 = gys / ups + 1
  gye2 = gye / ups
  dgt_gxy2 = dgt(max(gxe2,gye2))

  cellsize2 = cellsize * ups

  call logmsg(str(resl_out)//':')
  call logmsg('  ('//str((/nx2,ny2/),dgt_xy2,',')//') ['//&
              str((/gxs2,gxe2/),dgt_gxy2,':')//','//&
              str((/gys2,gye2/),dgt_gxy2,':')//']')

  west2 = west_of_gx(gxs)
  east2 = east_of_gx(gxe)
  south2 = south_of_gy(gye)
  north2 = north_of_gy(gys)
  call logmsg('BBox: '//str(sBBox(west,east,south,north))//&
            '\n   -> '//str(sBBox(west2,east2,south2,north2)))

  allocate( dem(ny, nx), dir(ny, nx), acc(ny, nx) )

  f = get_f_nwk_map(resl_in, gid, 'elv')
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, form='unformatted', access='direct', &
       recl=4*nx_org, status='old')
  dem(:,:) = nodata_dem
  do i = 1, ny_org
    read(un, rec=i) dem(i+ys-1,xs:xe)
  enddo
  close(un)

  f = get_f_nwk_map(resl_in, gid, 'dir_rri')
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, form='unformatted', access='direct', &
       recl=4*nx_org, status='old')
  dir(:,:) = nodata_dir
  do i = 1, ny_org
    read(un, rec=i) dir(i+ys-1,xs:xe)
  enddo
  close(un)

  f = get_f_nwk_map(resl_in, gid, 'upg')
  call logmsg('Reading '//str(f))
  open(newunit=un, file=f, form='unformatted', access='direct', &
       recl=4*nx_org, status='old')
  acc(:,:) = nodata_acc
  do i = 1, ny_org
    read(un, rec=i) acc(i+ys-1,xs:xe)
  enddo
  close(un)



!open(10, file = infile_dem, status = "old")
!read(10,*) !ctemp, nx
!read(10,*) !ctemp, ny
!read(10,*) !ctemp, xllcorner
!read(10,*) !ctemp, yllcorner
!read(10,*) !ctemp, cellsize
!read(10,*) ctemp, nodata_dem
!allocate( dem(ny, nx), dir(ny, nx), acc(ny, nx) )
!do i = 1, ny
! read(10, *) (dem(i, j), j = 1, nx)
!enddo
!dem(:,:) = nodata_dem
!do i = ys, ye
!  read(10, *) (dem(i, j), j = xs, xe)
!enddo
!close(10)

!open(10, file = infile_dir, status = "old")
!read(10,*) !ctemp, nx
!read(10,*) !ctemp, ny
!read(10,*) !ctemp, xllcorner
!read(10,*) !ctemp, yllcorner
!read(10,*) !ctemp, cellsize
!read(10,*) ctemp, nodata_dir
!do i = 1, ny
! read(10, *) (dir(i, j), j = 1, nx)
!enddo
!dir(:,:) = nodata_dir
!do i = ys, ye
!  read(10, *) (dir(i, j), j = xs, xe)
!enddo
!close(10)

!open(10, file = infile_acc, status = "old")
!read(10,*) !ctemp, nx
!read(10,*) !ctemp, ny
!read(10,*) !ctemp, xllcorner
!read(10,*) !ctemp, yllcorner
!read(10,*) !ctemp, cellsize
!read(10,*) ctemp, nodata_acc
!do i = 1, ny
! read(10, *) (acc(i, j), j = 1, nx)
!enddo
!acc(:,:) = nodata_acc
!do i = ys, ye
!  read(10, *) (acc(i, j), j = xs, xe)
!enddo
!close(10)
write(*,*) "Done STEP 1"

! STEP 2 : Set

if( ups .ge. 9 ) then
 corner = 3
elseif( ups .ge. 6 ) then
 corner = 2
elseif( ups .ge. 3 ) then
 corner = 1
else
 corner = 0
endif


allocate( top(ny2, nx2), bottom(ny2, nx2), left(ny2, nx2), right(ny2, nx2) )
allocate( tile(ny, nx), accmax(ny2, nx2), upstream2(ny2, nx2), acc_dif(ny, nx) )
allocate( am2_i(ny2, nx2), am2_j(ny2, nx2), pass2(ny2, nx2) )
allocate( dem2(ny2, nx2), acc2(ny2, nx2), dir2(ny2, nx2) )

do ii = 1, ny2
 do jj = 1, nx2
  top(ii, jj) = (ii - 1) * ups + 1
  bottom(ii, jj) = ii * ups
  left(ii, jj) = (jj - 1) * ups + 1
  right(ii, jj) = jj * ups
 enddo
enddo

tile(:,:) = 1
if( corner .ge. 1 ) then
 do ii = 1, ny2
  do jj = 1, nx2

   ! top left
   do i = top(ii, jj), top(ii, jj) + (corner - 1)
    do j = left(ii, jj), left(ii, jj) + (corner - 1)
     tile(i, j) = 0
    enddo
   enddo

   ! top right
   do i = top(ii, jj), top(ii, jj) + (corner - 1)
    do j = right(ii, jj), right(ii, jj) - (corner - 1), -1
     tile(i, j) = 0
    enddo
   enddo

   ! bottom left
   do i = bottom(ii, jj), bottom(ii, jj) - (corner - 1), -1
    do j = left(ii, jj), left(ii, jj) + (corner - 1)
     tile(i, j) = 0
    enddo
   enddo

   ! bottom right
   do i = bottom(ii, jj), bottom(ii, jj) - (corner - 1), -1
    do j = right(ii, jj), right(ii, jj) - (corner - 1), -1
     tile(i, j) = 0
    enddo
   enddo

  enddo
 enddo
endif
write(*,*) "Done STEP 2"


! STEP 3 : Find maximum acc

accmax(:, :) = 0
dem2(:,:) = 0.
do ii = 1, ny2
 do jj = 1, nx2

  k = 0
  do i = top(ii, jj), bottom(ii, jj)
   do j = left(ii, jj), right(ii, jj)
    if( dem(i, j) .lt. -100.d0 .or. tile(i, j) .eq. 0 ) cycle
    if( acc(i, j) .ge. accmax(ii, jj) ) then
     accmax(ii, jj) = acc(i, j)
     am2_i(ii, jj) = i
     am2_j(ii, jj) = j
    endif
    dem2(ii, jj) = dem2(ii, jj) + dem(i, j)
    k = k + 1
   enddo
  enddo

  if( k .ge. ups * ups / 2 ) then
   dem2(ii, jj) = dem2(ii, jj) / real(k)
  else
   !dem2(ii, jj) = -999.9d0
   dem2(ii, jj) = nodata_dem
  endif

 enddo
enddo
write(*,*) "Done STEP 3"


! STEP 4 : Find neighbor

!dir2(:,:) = -999
dir2(:,:) = nodata_dir
do ii = 1, ny2
 do jj = 1, nx2

  if( dem2(ii, jj) .lt. -100.d0 ) cycle 

  i = am2_i(ii, jj)
  j = am2_j(ii, jj)
  iii = i
  jjj = j

  do
   if(dir(iii, jjj) .le. 0) then
    i_next = iii
    j_next = jjj
    exit
   endif
   call down(dir, ny, nx, iii, jjj, i_next, j_next)
   if(tile(i_next, j_next) .eq. 0) then
    iii = i_next
    jjj = j_next
   else
    exit
   endif
  enddo

  ii_next = i_next / ups
  if( mod(i_next, ups) .ne. 0 ) ii_next = ii_next + 1
  jj_next = j_next / ups
  if( mod(j_next, ups) .ne. 0 ) jj_next = jj_next + 1

  if( ii_next .eq. ii .and. jj_next .gt. jj ) dir2(ii, jj) = 1
  if( ii_next .gt. ii .and. jj_next .gt. jj ) dir2(ii, jj) = 2
  if( ii_next .gt. ii .and. jj_next .eq. jj ) dir2(ii, jj) = 4
  if( ii_next .gt. ii .and. jj_next .lt. jj ) dir2(ii, jj) = 8
  if( ii_next .eq. ii .and. jj_next .lt. jj ) dir2(ii, jj) = 16
  if( ii_next .lt. ii .and. jj_next .lt. jj ) dir2(ii, jj) = 32
  if( ii_next .lt. ii .and. jj_next .eq. jj ) dir2(ii, jj) = 64
  if( ii_next .lt. ii .and. jj_next .gt. jj ) dir2(ii, jj) = 128
  if( ii_next .eq. ii .and. jj_next .eq. jj ) dir2(ii, jj) = 0
  if( dem2(ii_next, jj_next) .lt. -100.d0 ) dir2(ii, jj) = 0
 enddo
enddo
write(*,*) "Done STEP 4"


! STEP 5 : Find The Most Upstream Cell

! acc_dif
acc_dif(:,:) = 1
do i = 1, ny
 do j = 1, nx
  if( dem(i, j) .lt. -100.d0 ) cycle
  call down(dir, ny, nx, i, j, i_next, j_next)
  acc_dif(i_next, j_next) = acc_dif(i_next, j_next) + acc(i, j)
 enddo
enddo
acc_dif(:,:) = acc(:,:) - acc_dif(:,:)
where( acc_dif(:,:) .le. 100 ) acc_dif(:,:) = 0 ! note if acc_dif < 100 -> acc_dif = 0
!where( dem(:,:) .lt. -100. ) acc_dif(:,:) = nodata
where( dem(:,:) .lt. -100. ) acc_dif(:,:) = nodata_acc
where( acc_dif(:,:) .le. 0 ) acc_dif(:,:) = 0

do ii = 1, ny2
 do jj = 1, nx2
  do i = top(ii, jj), bottom(ii, jj)
   do j = left(ii, jj), right(ii, jj)
    if( acc_dif(i,j) .gt. 0 .and. dem2(ii, jj) .lt. -100.d0 ) then !acc_dif(i, j) is outside after scale up
     iii = i
     jjj = j
     do
      call down( dir, ny, nx, iii, jjj, i_next, j_next)
      acc_dif(i_next, j_next) = acc_dif(iii, jjj)
      acc_dif(iii, jjj) = 0
      iii = i_next
      jjj = j_next
      if( dem(iii, jjj) .ge. -100 ) exit
     enddo
    endif
   enddo
  enddo
 enddo
enddo


upstream2(:,:) = 1
do ii = 1, ny2
 do jj = 1, nx2
  if( dem2(ii, jj) .lt. -100.d0 ) then
   upstream2(ii, jj) = 0
   cycle
  endif
  call down(dir2, ny2, nx2, ii, jj, i_next, j_next)
  upstream2(i_next, j_next) = 0
 enddo
enddo
write(*,*) "Done STEP 5"


! STEP 6 : Calc Flow Accumulation

1111 continue
acc2(:, :) = 0
pass2(:, :) = 0
do ii = 1, ny2
 do jj = 1, nx2
  if( upstream2(ii, jj) .eq. 1 ) then
   iii = ii
   jjj = jj
   k = 1
   count = 0
   do
    count = count + 1
    pass2(iii, jjj) = 1
    call down( dir2, ny2, nx2, iii, jjj, i_next, j_next)
    if( dir2(i_next, j_next) .lt. 0 ) exit
    acc2(i_next, j_next) = acc2(i_next, j_next) + k
    if( pass2(i_next, j_next) .eq. 0 ) k = k + 1
    iii = i_next
    jjj = j_next
    if( dir2(i_next, j_next) .eq. 0 ) exit
    if( count .ge. nx2 * ny2 ) then
     write(*,*) "warning : dir2 at ", iii, jjj, "is set to be zero."
     dir2(iii, jjj) = 0
     go to 1111
    endif
   enddo
  endif
 enddo
enddo
!where( dem2(:, :) .lt. -100.d0 ) acc2(:, :) = -999
where( dem2(:, :) .lt. -100.d0 ) acc2(:, :) = nodata_acc
write(*,*) "Done STEP 6"


! STEP 6.1 : Add Flow Accumulation
! Extra "acc" is added for river basin with upstream catchments

do ii = 1, ny2
 do jj = 1, nx2

  do i = top(ii, jj), bottom(ii, jj)
   do j = left(ii, jj), right(ii, jj)
    if( acc_dif(i, j) .ge. 100 ) then

     k = acc_dif(i, j) / ups / ups
     write(*,'("Note: acc is added by", i6, " at ", i6, ",", i6)') k, ii, jj
     iii = ii
     jjj = jj
     do
      acc2(iii, jjj) = acc2(iii, jjj) + k
      call down( dir2, ny2, nx2, iii, jjj, i_next, j_next)
      iii = i_next
      jjj = j_next
      if( dir2(i_next, j_next) .eq. 0 ) then
       acc2(iii, jjj) = acc2(iii, jjj) + k
       exit
      endif
     enddo

    endif
   enddo
  enddo

 enddo
enddo


! STEP 7 : Output files

open(20, file = outfile_dem2)
write(20,'("ncols ", i10)') nx2
write(20,'("nrows ", i10)') ny2
write(20,'("xllcorner ", f25.12)') west2
write(20,'("yllcorner ", f25.12)') south2
write(20,'("cellsize  ", f25.12)') cellsize2
write(20,'("NODATA_value ", f25.12)') nodata_dem
do ii = 1, ny2
 write(20, '((100000f11.3))') (dem2(ii, jj), jj = 1, nx2)
enddo
close(20)

open(20, file = outfile_dir2)
write(20,'("ncols ", i10)') nx2
write(20,'("nrows ", i10)') ny2
write(20,'("xllcorner ", f25.12)') west2
write(20,'("yllcorner ", f25.12)') south2
write(20,'("cellsize  ", f25.12)') cellsize2
write(20,'("NODATA_value ", i10)') nodata_dir
do ii = 1, ny2
 write(20, '((100000i5))') (dir2(ii, jj), jj = 1, nx2)
enddo
close(20)

open(20, file = outfile_acc2)
write(20,'("ncols ", i10)') nx2
write(20,'("nrows ", i10)') ny2
write(20,'("xllcorner ", f25.12)') west2
write(20,'("yllcorner ", f25.12)') south2
write(20,'("cellsize  ", f25.12)') cellsize2
write(20,'("NODATA_value ", i10)') nodata_acc
do ii = 1, ny2
 write(20, '((100000i10))') (acc2(ii, jj), jj = 1, nx2)
 !write(20, '((100000i10))') (upstream2(ii, jj), jj = 1, nx2)
 !write(20, '((100000i10))') (accmax(ii, jj), jj = 1, nx2)
enddo
close(20)
write(*,*) "Done STEP 7"

! for checking
!open(23, file = "acc_dif.txt")
!write(23,'("ncols ", i10)') nx
!write(23,'("nrows ", i10)') ny
!write(23,'("xllcorner ", f25.12)') xllcorner
!write(23,'("yllcorner ", f25.12)') yllcorner
!write(23,'("cellsize  ", f25.12)') cellsize
!write(23,'("NODATA_value ", i10)') -999
!do ii = 1, ny
! write(23, '((100000i10))') (acc_dif(ii, jj), jj = 1, nx)
!enddo
!close(23)

  f = get_f_nwk_info(resl_out, gid)
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, status='replace')
  write(un,"(a)") 'nx '//str(nx2,dgt_xy2)//' gx '//str((/gxs2,gxe2/),dgt_gxy2)
  write(un,"(a)") 'ny '//str(ny2,dgt_xy2)//' gy '//str((/gys2,gye2/),dgt_gxy2)
  write(un,"(a)") 'BBox '//str((/west2,east2/),'f12.7')//' '//str((/south2,north2/),'f11.7')
  write(un,"(a)") 'elv_miss '//str(nodata_dem)
  write(un,"(a)") 'fdr_miss '//str(nodata_dir)
  write(un,"(a)") 'upg_miss '//str(nodata_acc)
  close(un)

  f = get_f_nwk_map(resl_out, gid, 'elv')
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, form='unformatted', access='direct', &
       recl=4*nx2, status='replace')
  do ii = 1, ny2
    write(un, rec=ii) dem2(ii,:)
  enddo
  close(un)

  f = get_f_nwk_map(resl_out, gid, 'dir')
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, form='unformatted', access='direct', &
       recl=4*nx2, status='replace')
  do ii = 1, ny2
    write(un, rec=ii) dir2(ii,:)
  enddo
  close(un)

  f = get_f_nwk_map(resl_out, gid, 'upg')
  call logmsg('Writing '//str(f))
  open(newunit=un, file=f, form='unformatted', access='direct', &
       recl=4*nx2, status='replace')
  do ii = 1, ny2
    write(un, rec=ii) acc2(ii,:)
  enddo
  close(un)

contains
!===============================================================
! Search Downstream
!===============================================================
subroutine down(dir, ny, nx, i, j, ii, jj)
  implicit none
  integer, intent(in) :: ny, nx
  integer, intent(in) :: dir(ny, nx)
  integer, intent(in) :: i, j
  integer, intent(out) :: ii, jj

! right
if( dir(i,j).eq.1 ) then
 ii = i
 jj = j + 1
! right down
elseif( dir(i,j).eq.2 ) then
 ii = i + 1
 jj = j + 1
! down
elseif( dir(i,j).eq.4 ) then
 ii = i + 1
 jj = j
! left down
elseif( dir(i,j).eq.8 ) then
 ii = i + 1
 jj = j - 1
! left
elseif( dir(i,j).eq.16 ) then
 ii = i
 jj = j - 1
! left up
elseif( dir(i,j).eq.32 ) then
 ii = i - 1
 jj = j - 1
! up
elseif( dir(i,j).eq.64 ) then
 ii = i - 1
 jj = j
! right up
elseif( dir(i,j).eq.128 ) then
 ii = i - 1
 jj = j + 1
! zero
else
 ii = i
 jj = j
endif

end subroutine
!===============================================================
!
!===============================================================
subroutine get_mesh_range(xs_org, xe_org, ups, xs, xe)
  implicit none
  integer, intent(in) :: xs_org, xe_org
  integer, intent(in) :: ups
  integer, intent(out) :: xs, xe

  xs = ((xs_org - 1) / ups) * ups + 1
  xe = ((xe_org - 1) / ups + 1) * ups
end subroutine get_mesh_range
!===============================================================
!
!===============================================================
function sBBox(west, east, south, north) result(res)
  implicit none
  real(8), intent(in) :: west, east, south, north
  character(:), allocatable ::res

  allocate(character(1) :: res)
  res = '['//str((/west,east/),'f12.7',', ')//&
        ', '//str((/south,north/),'f11.7',', ')//']'
end function sBBox
!===============================================================
!
!===============================================================
function sMeshRange(gxs, gxe, gys, gye) result(res)
  use c2_jflw_const
  implicit none
  integer, intent(in) :: gxs, gxe, gys, gye
  character(:), allocatable :: res

  res = '['//str((/gxs,gxe/),DGT_GXY,':')//&
        ','//str((/gys,gye/),DGT_GXY,':')//']'
end function sMeshRange
!===============================================================
!
!===============================================================
end program main
