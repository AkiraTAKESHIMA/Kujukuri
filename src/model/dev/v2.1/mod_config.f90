! RRI_Read.f90

module mod_config
  use mod_globals
  implicit none
  private
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: read_config
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
subroutine prepare_static_data()
  implicit none

  call read_config()

  call setup_time()

  call read_domain_info()

  call build_topography()

  call setup_index()

  call load_dam_data()

  call load_initial_conditions()

  call load_boundary_conditions()
end subroutine prepare_static_data
!===============================================================
!
!===============================================================
subroutine read_config()
  use mod_globals
  implicit none

  integer :: i
  character(256) :: format_version

open(1, file = "RRI_Input.txt", status = 'old')

read(1,'(a)') format_version
write(*,'("format_version : ", a)') trim(adjustl(format_version))

if( format_version .ne. "RRI_Input_Format_Ver1_4_2" ) stop "This RRI model requires RRI_Input_Format_Ver1_4_2"

read(1,*)
write(*,*)

read(1,'(a)') rainfile
read(1,'(a)') demfile
read(1,'(a)') accfile
read(1,'(a)') dirfile

write(*,'("rainfile : ", a)') trim(adjustl(rainfile))
write(*,'("demfile : ", a)') trim(adjustl(demfile))
write(*,'("accfile : ", a)') trim(adjustl(accfile))
write(*,'("dirfile : ", a)') trim(adjustl(dirfile))

read(1,*)
write(*,*)

read(1,*) utm
read(1,*) eight_dir
read(1,*) lasth
read(1,*) dt
read(1,*) dt_riv
read(1,*) outnum
read(1,*) xllcorner_rain
read(1,*) yllcorner_rain
read(1,*) cellsize_rain_x, cellsize_rain_y

write(*,'("utm : ", i5)') utm
write(*,'("eight_dir : ", i5)') eight_dir
write(*,'("lasth : ", i8)') lasth
write(*,'("dt : ", i12)') dt
write(*,'("dt_riv : ", i8)') dt_riv
write(*,'("outnum : ", i8)') outnum
write(*,'("xllcorner_rain : ", f15.5)') xllcorner_rain
write(*,'("yllcorner_rain : ", f15.5)') yllcorner_rain
write(*,'("cellsize_rain_x : ", f15.5, "  cellsize_rain_y : ", f15.5)') cellsize_rain_x, cellsize_rain_y

read(1,*)
write(*,*)

read(1,*) ns_river
read(1,*) num_of_landuse

allocate( dif(num_of_landuse) )
allocate( ns_slope(num_of_landuse), soildepth(num_of_landuse) )
allocate( gammaa(num_of_landuse) )

read(1,*) (dif(i), i = 1, num_of_landuse)
read(1,*) (ns_slope(i), i = 1, num_of_landuse)
read(1,*) (soildepth(i), i = 1, num_of_landuse)
read(1,*) (gammaa(i), i = 1, num_of_landuse)

write(*,'("ns_river : ", f12.3)') ns_river
write(*,'("num_of_landuse : ", i5)') num_of_landuse
write(*,'("dif : ", 100i5)') (dif(i), i = 1, num_of_landuse)
write(*,'("ns_slope : ", 100f12.3)') (ns_slope(i), i = 1, num_of_landuse)
write(*,'("soildepth : ", 100f12.3)') (soildepth(i), i = 1, num_of_landuse)
write(*,'("gammaa : ", 100f12.3)') (gammaa(i), i = 1, num_of_landuse)

read(1,*)
write(*,*)

allocate( ksv(num_of_landuse), faif(num_of_landuse) )

read(1,*) (ksv(i), i = 1, num_of_landuse)
read(1,*) (faif(i), i = 1, num_of_landuse)

write(*,'("ksv : ", 100e12.3)') (ksv(i), i = 1, num_of_landuse)
write(*,'("faif : ", 100f12.3)') (faif(i), i = 1, num_of_landuse)

read(1,*)
write(*,*)

allocate( ka(num_of_landuse), gammam(num_of_landuse), beta(num_of_landuse) )

read(1,*) (ka(i), i = 1, num_of_landuse)
read(1,*) (gammam(i), i = 1, num_of_landuse)
read(1,*) (beta(i), i = 1, num_of_landuse)

write(*,'("ka : ", 100e12.3)') (ka(i), i = 1, num_of_landuse)
write(*,'("gammam : ", 100f12.3)') (gammam(i), i = 1, num_of_landuse)
write(*,'("beta : ", 100f12.3)') (beta(i), i = 1, num_of_landuse)

do i = 1, num_of_landuse
 if( gammam(i) .gt. gammaa(i) ) stop "gammam must be smaller than gammaa"
enddo

read(1,*)
write(*,*)

allocate( ksg(num_of_landuse), gammag(num_of_landuse), kg0(num_of_landuse), fpg(num_of_landuse), rgl(num_of_landuse) )

read(1,*) (ksg(i), i = 1, num_of_landuse)
read(1,*) (gammag(i), i = 1, num_of_landuse)
read(1,*) (kg0(i), i = 1, num_of_landuse)
read(1,*) (fpg(i), i = 1, num_of_landuse)
read(1,*) (rgl(i), i = 1, num_of_landuse)

write(*,'("ksg : ", 100e12.3)') (ksg(i), i = 1, num_of_landuse)
write(*,'("gammag : ", 100f12.3)') (gammag(i), i = 1, num_of_landuse)
write(*,'("kg0 : ", 100e12.3)') (kg0(i), i = 1, num_of_landuse)
write(*,'("fpg : ", 100f12.3)') (fpg(i), i = 1, num_of_landuse)
write(*,'("rgl : ", 100e12.3)') (rgl(i), i = 1, num_of_landuse)

read(1,*)
write(*,*)

read(1,*) riv_thresh
read(1,*) width_param_c
read(1,*) width_param_s
read(1,*) depth_param_c
read(1,*) depth_param_s
read(1,*) height_param
read(1,*) height_limit_param

read(1,*)
write(*,*)

read(1,*) rivfile_switch
read(1,'(a)') widthfile
read(1,'(a)') depthfile
read(1,'(a)') heightfile

if(rivfile_switch.eq.0) then
 write(*,'("riv_thresh : ", i7)') riv_thresh
 write(*,'("width_param_c : ", f12.2)') width_param_c
 write(*,'("width_param_s : ", f12.2)') width_param_s
 write(*,'("depth_param_c : ", f12.2)') depth_param_c
 write(*,'("depth_param_s : ", f12.2)') depth_param_s
 write(*,'("height_param : ", f12.2)') height_param
 write(*,'("height_limit_param : ", i10)') height_limit_param
else
 write(*,'("widthfile : ", a)') trim(adjustl(widthfile))
 write(*,'("depthfile : ", a)') trim(adjustl(depthfile))
 write(*,'("heightfile : ", a)') trim(adjustl(heightfile))
endif

read(1,*)
write(*,*)

read(1,*) init_slo_switch, init_riv_switch, init_gw_switch, init_gampt_ff_switch
read(1,"(a)") initfile_slo
read(1,'(a)') initfile_riv
read(1,'(a)') initfile_gw
read(1,'(a)') initfile_gampt_ff

if(init_slo_switch.ne.0) write(*,'("initfile_slo : ", a)') trim(adjustl(initfile_slo))
if(init_riv_switch.ne.0) write(*,'("initfile_riv : ", a)') trim(adjustl(initfile_riv))
if(init_gw_switch.ne.0) write(*,'("initfile_gw : ", a)') trim(adjustl(initfile_gw))
if(init_gampt_ff_switch.ne.0) write(*,'("initfile_gampt_ff : ", a)') trim(adjustl(initfile_gampt_ff))

read(1,*)
write(*,*)

read(1,*) bound_slo_wlev_switch, bound_riv_wlev_switch
read(1,'(a)') boundfile_slo_wlev
read(1,'(a)') boundfile_riv_wlev

if(bound_slo_wlev_switch.ne.0) write(*,'("boundfile_slo_wlev : ", a)') trim(adjustl(boundfile_slo_wlev))
if(bound_riv_wlev_switch.ne.0) write(*,'("boundfile_riv_wlev : ", a)') trim(adjustl(boundfile_riv_wlev))

read(1,*)
write(*,*)

read(1,*) bound_slo_disc_switch, bound_riv_disc_switch
read(1,'(a)') boundfile_slo_disc
read(1,'(a)') boundfile_riv_disc

if(bound_slo_disc_switch.ne.0) write(*,'("boundfile_slo_disc : ", a)') trim(adjustl(boundfile_slo_disc))
if(bound_riv_disc_switch.ne.0) write(*,'("boundfile_riv_disc : ", a)') trim(adjustl(boundfile_riv_disc))

read(1,*)
write(*,*)

read(1,*) land_switch
read(1,'(a)') landfile
if(land_switch.eq.1) write(*,'("landfile : ", a)') trim(adjustl(landfile))

read(1,*)
write(*,*)

read(1,*) dam_switch
read(1,'(a)') damfile
if(dam_switch.eq.1) write(*,'("damfile : ", a)') trim(adjustl(damfile))

read(1,*)
write(*,*)

read(1,*) div_switch
read(1,'(a)') divfile
if(div_switch.eq.1) write(*,'("divfile : ", a)') trim(adjustl(divfile))

read(1,*)
write(*,*)

read(1,*) evp_switch
read(1,'(a)') evpfile
read(1,*) xllcorner_evp
read(1,*) yllcorner_evp
read(1,*) cellsize_evp_x, cellsize_evp_y

if( evp_switch .ne. 0 ) then
 write(*,'("evpfile : ", a)') trim(adjustl(evpfile))
 write(*,'("xllcorner_evp : ", f15.5)') xllcorner_evp
 write(*,'("yllcorner_evp : ", f15.5)') yllcorner_evp
 write(*,'("cellsize_evp_x : ", f15.5, " cellsize_evp_y : ", f15.5)') cellsize_evp_x, cellsize_evp_y
endif

read(1,*)
write(*,*)

read(1,*) sec_length_switch
read(1,'(a)') sec_length_file
if(sec_length_switch.eq.1) write(*,'("sec_length : ", a)') trim(adjustl(sec_length_file))

read(1,*)
write(*,*)

read(1,*) sec_switch
read(1,'(a)') sec_map_file
read(1,'(a)') sec_file
if(sec_switch.eq.1) write(*,'("sec_map_file : ", a)') trim(adjustl(sec_map_file))
if(sec_switch.eq.1) write(*,'("sec_file : ", a)') trim(adjustl(sec_file))

read(1,*)
write(*,*)

!read(1,*) emb_switch
!read(1,'(a)') embrfile
!read(1,'(a)') embbfile
!if(emb_switch.eq.1) write(*,'("embrfile : ", a)') trim(adjustl(embrfile))
!if(emb_switch.eq.1) write(*,'("embbfile : ", a)') trim(adjustl(embbfile))

read(1,*) outswitch_hs, outswitch_hr, outswitch_hg, outswitch_qr, outswitch_qu, outswitch_qv, &
          outswitch_gu, outswitch_gv, outswitch_gampt_ff, outswitch_storage
read(1,'(a)') outfile_hs
read(1,'(a)') outfile_hr
read(1,'(a)') outfile_hg
read(1,'(a)') outfile_qr
read(1,'(a)') outfile_qu
read(1,'(a)') outfile_qv
read(1,'(a)') outfile_gu
read(1,'(a)') outfile_gv
read(1,'(a)') outfile_gampt_ff
read(1,'(a)') outfile_storage

if(outswitch_hs .ne. 0) write(*,'("outfile_hs : ", a)') trim(adjustl(outfile_hs))
if(outswitch_hr .ne. 0) write(*,'("outfile_hr : ", a)') trim(adjustl(outfile_hr))
if(outswitch_hg .ne. 0) write(*,'("outfile_hg : ", a)') trim(adjustl(outfile_hg))
if(outswitch_qr .ne. 0) write(*,'("outfile_qr : ", a)') trim(adjustl(outfile_qr))
if(outswitch_qu .ne. 0) write(*,'("outfile_qu : ", a)') trim(adjustl(outfile_qu))
if(outswitch_qv .ne. 0) write(*,'("outfile_qv : ", a)') trim(adjustl(outfile_qv))
if(outswitch_gu .ne. 0) write(*,'("outfile_gu : ", a)') trim(adjustl(outfile_gu))
if(outswitch_gv .ne. 0) write(*,'("outfile_gv : ", a)') trim(adjustl(outfile_gv))
if(outswitch_gampt_ff .ne. 0) write(*,'("outfile_gampt_ff : ", a)') trim(adjustl(outfile_gampt_ff))
if(outswitch_storage .ne. 0) write(*,'("outfile_storage : ", a)') trim(adjustl(outfile_storage))

read(1,*)
write(*,*)

read(1,*) hydro_switch
read(1,'(a)') location_file
if(hydro_switch .eq. 1) write(*,'("location_file : ", a)') trim(adjustl(location_file))

write(*,*)

close(1)

! Parameter Check
do i = 1, num_of_landuse
 if( ksv(i) .gt. 0.d0 .and. ka(i) .gt. 0.d0 ) &
  stop "Error: both ksv and ka are non-zero."
 if( gammam(i) .gt. gammaa(i) ) &
  stop "Error: gammam must be smaller than gammaa."
enddo

! Set da, dm and infilt_limit
allocate( da(num_of_landuse), dm(num_of_landuse), infilt_limit(num_of_landuse))
da(:) = 0.d0
dm(:) = 0.d0
infilt_limit(:) = 0.d0
do i = 1, num_of_landuse
 if( soildepth(i) .gt. 0.d0 .and. ksv(i) .gt. 0.d0 ) infilt_limit(i) = soildepth(i) * gammaa(i)
 if( soildepth(i) .gt. 0.d0 .and. ka(i) .gt. 0.d0 ) da(i)= soildepth(i) * gammaa(i)
 if( soildepth(i) .gt. 0.d0 .and. ka(i) .gt. 0.d0 .and. gammam(i) .gt. 0.d0 ) &
  dm(i) = soildepth(i) * gammam(i)
enddo

! if ksg(i) = 0.d0 -> no gw calculation
gw_switch = 0
do i = 1, num_of_landuse
 if( ksg(i) .gt. 0.d0 ) then
  gw_switch = 1
 else
  gammag(i) = 0.d0
  kg0(i) = 0.d0
  fpg(i) = 0.d0
  rgl(i) = 0.d0
 endif
enddo

end subroutine read_config
!===============================================================
!
!===============================================================
subroutine setup_time()
  use mod_globals
  implicit none

  ! max timestep
  maxt = lasth * 3600 / dt
end subroutine setup_time
!===============================================================
!
!===============================================================
subroutine read_domain_info()
  use mod_globals
  implicit none
  character(*), intent(in) :: demfile
  integer, intent(out) :: nx, ny
  real(8), intent(out) :: xllcorner, yllcorner
  real(8), intent(out) :: cellsize

  open( 10, file = demfile, status = "old" )
  read(10,*) ctemp, nx
  read(10,*) ctemp, ny
  read(10,*) ctemp, xllcorner
  read(10,*) ctemp, yllcorner
  read(10,*) ctemp, cellsize
  read(10,*) ! ctemp, nodata
  close(10)
end subroutine read_domain_info
!===============================================================
!
!===============================================================
subroutine load_land_cover()
  use mod_globals
  implicit none
  character(*), intent(in) :: landfile
  integer, intent(in) :: land_switch
  integer, intent(in) :: num_of_landuse
  integer, pointer :: land(:,:)  ! out

  ! landuse file
  allocate( land(ny, nx) )
  land(:,:) = 1
  if( land_switch.eq.1 ) then
    call read_gis_int(landfile, land)
  endif

  ! land : 1 ... num_of_landuse
  write(*,*) "num_of_landuse : ", num_of_landuse
  where( land .le. 0 .or. land .gt. num_of_landuse ) land = num_of_landuse
end subroutine load_land_cover
!===============================================================
!
!===============================================================
subroutine build_topography()
  use mod_globals
  implicit none
!  real(8) :: x1, x2, y1, y2
  integer :: i, j
  integer :: div_org_i, div_org_j, div_dest_i, div_dest_j

! dx, dy calc
! d1: south side length
x1 = xllcorner
y1 = yllcorner
x2 = xllcorner + nx * cellsize
y2 = yllcorner
if( utm.eq.0 ) call hubeny_sub( x1, y1, x2, y2, d1 )

! d2: north side length
x1 = xllcorner
y1 = yllcorner + ny * cellsize
x2 = xllcorner + nx * cellsize
y2 = yllcorner + ny * cellsize
if( utm.eq.0 ) call hubeny_sub( x1, y1, x2, y2, d2 )

! d3: west side length
x1 = xllcorner
y1 = yllcorner
x2 = xllcorner
y2 = yllcorner + ny * cellsize
if( utm.eq.0 ) call hubeny_sub( x1, y1, x2, y2, d3 )

! d4: east side length
x1 = xllcorner + nx * cellsize
y1 = yllcorner
x2 = xllcorner + nx * cellsize
y2 = yllcorner + ny * cellsize
if( utm.eq.0 ) call hubeny_sub( x1, y1, x2, y2, d4 )

if( utm.eq.1 ) then
 dx = cellsize
 dy = cellsize
else
 dx = (d1 + d2) / 2.d0 / real(nx)
 dy = (d3 + d4) / 2.d0 / real(ny)
endif
write(*,*) "dx [m] : ", dx, "dy [m] : ", dy

! length and area of each cell
length = sqrt(dx * dy)
area = dx * dy

! river widhth, depth, leavy height, river length, river area ratio
allocate ( width(ny, nx), depth(ny, nx), height(ny, nx), len_riv(ny, nx), area_ratio(ny, nx) )

width = 0.d0
depth = 0.d0
height = 0.d0
len_riv = 0.d0

area_ratio = 0.d0

riv = 0 ! slope cell
if( riv_thresh .gt. 0 ) then
 where(acc .gt. riv_thresh) riv = 1 ! river cell
endif

where(riv.eq.1) width = width_param_c * ( acc * dx * dy * 1.d-6 ) ** width_param_s
where(riv.eq.1) depth = depth_param_c * ( acc * dx * dy * 1.d-6 ) ** depth_param_s
where(riv.eq.1 .and. acc.gt.height_limit_param) height = height_param

! river data is replaced by the information in files
if( rivfile_switch .ge. 1 ) then
 riv = 0
 riv_thresh = 1
 call read_gis_real(widthfile, width)
 where(width .gt. 0) riv = 1 ! river cells (if width >= 0.)
 call read_gis_real(depthfile, depth)
 call read_gis_real(heightfile, height)
 where( height(:,:) .lt. 0.d0 ) height(:,:) = 0.d0
endif 
where(riv.eq.1) len_riv = length

! river cross section is set by section files
allocate( sec_map(ny, nx) )
sec_map = 0
if( sec_switch.eq.1 ) then
 call read_gis_int(sec_map_file, sec_map)
 sec_id_max = maxval( sec_map(:,:) )
 call set_section
endif
where(riv.eq.1) len_riv = length ! added on Dec. 27, 2021

! river length is set by input file
allocate( sec_length(ny, nx) )
sec_length = 0
if( sec_length_switch.eq.1 ) then
 call read_gis_real(sec_length_file, sec_length)
 where(sec_length .gt. 0.d0) len_riv = sec_length
endif

if( rivfile_switch .eq. 2 ) then
 ! levee on both river and slope grid cells : zs is increased with height
 where( height .gt. 0.d0 ) zs = zs + height
else
 ! levee on only slope grid cells : zs is increased with height
 where( height .gt. 0.d0 .and. riv .eq. 0 ) zs = zs + height
endif

!where(riv.eq.1) area_ratio = width / length
!where(riv.eq.1) area_ratio = width / len_riv
where(riv.eq.1) area_ratio = width * len_riv / area ! modified by T.Sayama on Nov. 27, 2021

zb_riv = zs
do i = 1, ny
 do j = 1, nx
  zb(i, j) = zs(i, j) - soildepth(land(i,j))
  if(riv(i, j) .eq. 1) zb_riv(i, j) = zs(i, j) - depth(i, j)
 enddo
enddo

! domain setting

! domain = 0 : outside the domain
! domain = 1 : inside the domain
! domain = 2 : outlet point (where dir(i,j) = 0 or dir(i,j) = -1),
!              and cells located at edges
domain = 0
num_of_cell = 0
do i = 1, ny
 do j = 1, nx
  if( zs(i, j) .gt. -100.d0 ) then
   domain(i, j) = 1
   if( dir(i, j).eq.0 ) domain(i, j) = 2
   if( dir(i, j).eq.-1 ) domain(i, j) = 2
   num_of_cell = num_of_cell + 1
  endif
 enddo
enddo
write(*,*) "num_of_cell : ", num_of_cell
write(*,*) "total area [km2] : ", num_of_cell * area / (10.d0 ** 6.0d0)


! div file
div_id_max = 0
if( div_switch.eq.1 ) then
 open( 20, file = divfile, status = "old" )
 do
  read(20, *, iostat = ios) div_org_i, div_org_j, div_dest_i, div_dest_j
  if(ios .ne. 0) exit
  div_id_max = div_id_max + 1
 enddo
 write(*,*) "div_id_max : ", div_id_max
 allocate( div_org_idx(div_id_max), div_dest_idx(div_id_max), div_rate(div_id_max) )
 rewind(20)

 do k = 1, div_id_max
  read(20, *) div_org_i, div_org_j, div_dest_i, div_dest_j, div_rate(k)
  div_org_idx(k) = riv_ij2idx( div_org_i, div_org_j )
  div_dest_idx(k) = riv_ij2idx( div_dest_i, div_dest_j )
 enddo
 write(*,*) "done: reading div file"
 close(20)
endif

end subroutine build_topography
!===============================================================
!
!===============================================================
subroutine setup_index()
  use mod_sub, only: &
    riv_idx_setting, &
    slo_idx_setting

  ! river index setting
  call riv_idx_setting

  ! slope index setting
  call slo_idx_setting
end subroutine setup_index
!===============================================================
!
!===============================================================
subroutine load_dam_data()
  use mod_dam, only: &
    dam_read
  implicit none

  ! reading dam file
  call dam_read
end subroutine load_dam_data
!===============================================================
!
!===============================================================
subroutine load_initial_conditions()
  use mod_globals
  implicit none

! initial condition
allocate(hs(ny, nx), hr(ny, nx), hg(ny, nx), gampt_ff(ny, nx))
allocate(gampt_f(ny, nx), qrs(ny, nx))

hr = -0.1d0
hs = -0.1d0
hg = -0.1d0
gampt_ff = 0.d0
gampt_f = 0.d0
qrs = 0.d0

!where(riv.eq.1) hr = init_cond_riv
!where(domain.eq.1) hs = init_cond_slo
where(riv.eq.1) hr = 0.d0
where(domain.eq.1) hs = 0.d0
where(domain.eq.2) hs = 0.d0

! for slope cells
! if init_slo_switch = 1 => read from file

if(init_slo_switch .eq. 1) then

 allocate( inith(ny, nx) )
 inith = 0.d0
 open(13, file = initfile_slo, status = "old")
 do i = 1, ny
  read(13,*) (inith(i,j), j = 1, nx)
 enddo
 where(inith .le. 0.d0) inith = 0.d0
 where(domain.eq.1 .and. inith .ge. 0.d0) hs = inith
 deallocate( inith )
 close(13)

endif

! for river cells
! if init_riv_switch = 1 => read from file

if(init_riv_switch .eq. 1) then

 allocate( inith(ny, nx) )
 inith = 0.d0
 open(13, file = initfile_riv, status = "old")
 do i = 1, ny
  read(13,*) (inith(i,j), j = 1, nx)
 enddo
 where(inith .le. 0.d0) inith = 0.d0
 where(riv.eq.1 .and. inith .ge. 0.d0) hr = inith
 deallocate( inith )
 close(13)

endif

! for slope cells
! if init_gw_switch = 1 => read from file

if(init_gw_switch .eq. 1) then

 allocate( inith(ny, nx) )
 inith = 0.d0
 open(13, file = initfile_gw, status = "old")
 do i = 1, ny
  read(13,*) (inith(i,j), j = 1, nx)
 enddo
 where(inith .le. 0.d0) inith = 0.d0
 where(domain.eq.1 .and. inith .ge. 0.d0) hg = inith
 deallocate( inith )
 close(13)

endif

! if init_gampt_ff_switch = 1 => read from file

if(init_gampt_ff_switch .eq. 1) then

 allocate( inith(ny, nx) )
 inith = 0.d0
 open(13, file = initfile_gampt_ff, status = "old")
 do i = 1, ny
  read(13,*) (inith(i,j), j = 1, nx)
 enddo
 where(inith .le. 0.d0) inith = 0.d0
 where(domain.eq.1) gampt_ff = inith
 deallocate( inith )
 close(13)

endif
end subroutine load_initial_conditions
!===============================================================
!
!===============================================================
subroutine load_boundary_conditions()
  use mod_globals
  use mod_sub, only: &
    sub_slo_ij2idx, &
    sub_riv_ij2idx
  implicit none

  integer num_of_bound_point
  real(8), allocatable :: bound_opt2(:)
  integer, allocatable :: bound_opt2_loci(:), bound_opt2_locj(:)

  integer i, j, k, t, tt
  integer itemp, jtemp, ios
  real(8) rdummy
  character(256) ctemp
  real(8), allocatable :: rdummy_dim(:)

! boundary condition (wlev)

! for slope cells (wlev)

if( bound_slo_wlev_switch .eq. 1 .or. bound_slo_wlev_switch .eq. 2 ) then
 open( 17, file = boundfile_slo_wlev, status = 'old' )

 tt = 0
 !if( bound_slo_wlev_switch .eq. 1 ) then
 if( bound_slo_wlev_switch .eq. 2 ) then

  do
   read(17, *, iostat = ios) t, itemp, jtemp
   if(nx.ne.itemp .or. ny.ne.jtemp) stop "error in boundary file (slo, wlev)"
   do i = 1, ny
    read(17, *, iostat = ios) (rdummy, j = 1, nx)
   enddo
   if( ios.lt.0 ) exit
   tt = tt + 1
  enddo
  tt_max_bound_slo_wlev = tt - 1

 else ! option 1

  read(17, *) num_of_bound_point
  read(17, '(a)') ctemp
  read(17, '(a)') ctemp
  do
   read(17, *, iostat = ios) t
   if( ios.lt.0 ) exit
   tt = tt + 1
  enddo
  tt_max_bound_slo_wlev = tt - 1

  allocate( bound_opt2(num_of_bound_point) )
  allocate( bound_opt2_loci(num_of_bound_point), bound_opt2_locj(num_of_bound_point) )

 endif

 allocate( t_bound_slo_wlev(0:tt_max_bound_slo_wlev), bound_slo_wlev(ny, nx), &
  bound_slo_wlev_idx(0:tt_max_bound_slo_wlev, slo_count), rdummy_dim(slo_count) )
 rewind(17)

 bound_slo_wlev = -999.9d0
 bound_slo_wlev_idx = -999.9d0
 rdummy_dim = -999.9d0

 !if( bound_slo_wlev_switch .eq. 1 ) then
 if( bound_slo_wlev_switch .eq. 2 ) then

  do tt = 0, tt_max_bound_slo_wlev
   read(17, *) t_bound_slo_wlev(tt), itemp, jtemp
   do i = 1, ny
    read(17,*) (bound_slo_wlev(i, j), j = 1, nx)
   enddo
   call sub_slo_ij2idx( bound_slo_wlev, rdummy_dim )
   do k = 1, slo_count
    bound_slo_wlev_idx(tt, k) = rdummy_dim(k)
   enddo
  enddo
  deallocate( bound_slo_wlev, rdummy_dim )
  write(*,*) "done: reading boundary file for slope cells (wlev)"
  close(17)

 else ! option 1

  read(17, *) itemp
  read(17, *) ctemp, (bound_opt2_loci(k), k = 1, num_of_bound_point)
  read(17, *) ctemp, (bound_opt2_locj(k), k = 1, num_of_bound_point)

  do tt = 0, tt_max_bound_slo_wlev
   read(17, *) t_bound_slo_wlev(tt), (bound_opt2(k), k = 1, num_of_bound_point)
   do k = 1, num_of_bound_point
    bound_slo_wlev( bound_opt2_loci(k), bound_opt2_locj(k) ) = bound_opt2(k)
   enddo
   call sub_slo_ij2idx( bound_slo_wlev, rdummy_dim )
   do k = 1, slo_count
    bound_slo_wlev_idx(tt, k) = rdummy_dim(k)
   enddo
  enddo
  deallocate( bound_slo_wlev, rdummy_dim )
  write(*,*) "done: reading boundary file for slope cells (wlev)"
  close(17)
  deallocate( bound_opt2 )
  deallocate( bound_opt2_loci, bound_opt2_locj )

 endif
endif

! for river cells (wlev)

if( bound_riv_wlev_switch .eq. 1 .or. bound_riv_wlev_switch .eq. 2 ) then
 open( 18, file = boundfile_riv_wlev, status = 'old' )

 tt = 0
 !if( bound_riv_wlev_switch .eq. 1 ) then
 if( bound_riv_wlev_switch .eq. 2 ) then

  do
   read(18, *, iostat = ios) t, itemp, jtemp
   if(nx.ne.itemp .or. ny.ne.jtemp) stop "error in boundary file (riv, wlev)"
   do i = 1, ny
    read(18, *, iostat = ios) (rdummy, j = 1, nx)
   enddo
   if( ios.lt.0 ) exit
   tt = tt + 1
  enddo
  tt_max_bound_riv_wlev = tt - 1

 else ! option 1

  read(18, *) num_of_bound_point
  read(18, '(a)') ctemp
  read(18, '(a)') ctemp
  do
   read(18, *, iostat = ios) t
   if( ios.lt.0 ) exit
   tt = tt + 1
  enddo
  tt_max_bound_riv_wlev = tt - 1

  allocate( bound_opt2(num_of_bound_point) )
  allocate( bound_opt2_loci(num_of_bound_point), bound_opt2_locj(num_of_bound_point) )

 endif

 allocate( t_bound_riv_wlev(0:tt_max_bound_riv_wlev), bound_riv_wlev(ny, nx), &
  bound_riv_wlev_idx(0:tt_max_bound_riv_wlev, riv_count), rdummy_dim(riv_count) )
 rewind(18)

 bound_riv_wlev = -999.9d0
 bound_riv_wlev_idx = -999.9d0
 rdummy_dim = -999.9d0

 !if( bound_riv_wlev_switch .eq. 1 ) then
 if( bound_riv_wlev_switch .eq. 2 ) then

  do tt = 0, tt_max_bound_riv_wlev
   read(18, *) t_bound_riv_wlev(tt), itemp, jtemp
   do i = 1, ny
    read(18,*) (bound_riv_wlev(i, j), j = 1, nx)
   enddo
   call sub_riv_ij2idx( bound_riv_wlev, rdummy_dim )
   do k = 1, riv_count
    bound_riv_wlev_idx(tt, k) = rdummy_dim(k)
   enddo
  enddo
  deallocate( bound_riv_wlev, rdummy_dim )
  write(*,*) "done: reading boundary file for river cells (wlev)"
  close(18)

 else ! option 1

  read(18, *) itemp
  read(18, *) ctemp, (bound_opt2_loci(k), k = 1, num_of_bound_point)
  read(18, *) ctemp, (bound_opt2_locj(k), k = 1, num_of_bound_point)

  do tt = 0, tt_max_bound_riv_wlev

   read(18, *) t_bound_riv_wlev(tt), (bound_opt2(k), k = 1, num_of_bound_point)
   do k = 1, num_of_bound_point
    bound_riv_wlev( bound_opt2_loci(k), bound_opt2_locj(k) ) = bound_opt2(k)
   enddo
   call sub_riv_ij2idx( bound_riv_wlev, rdummy_dim )
   do k = 1, riv_count
    bound_riv_wlev_idx(tt, k) = rdummy_dim(k)
   enddo
  enddo
  deallocate( bound_riv_wlev, rdummy_dim )
  write(*,*) "done: reading boundary file for river cells (wlev)"
  close(18)
  deallocate( bound_opt2 )
  deallocate( bound_opt2_loci, bound_opt2_locj )

 endif
endif

! boundary condition (disc)

! for slope cells (disc)

if( bound_slo_disc_switch .eq. 1 .or. bound_slo_disc_switch .eq. 2 ) then
 open( 17, file = boundfile_slo_disc, status = 'old' )

 tt = 0
 !if( bound_slo_disc_switch .eq. 1 ) then
 if( bound_slo_disc_switch .eq. 2 ) then

  do
   read(17, *, iostat = ios) t, itemp, jtemp
   if(nx.ne.itemp .or. ny.ne.jtemp) stop "error in boundary file (slo, disc)"
   do i = 1, ny
    read(17, *, iostat = ios) (rdummy, j = 1, nx)
   enddo
   if( ios.lt.0 ) exit
   tt = tt + 1
  enddo
  tt_max_bound_slo_disc = tt - 1

 else ! option 1

  read(17, *) num_of_bound_point
  read(17, '(a)') ctemp
  read(17, '(a)') ctemp
  do
   read(17, *, iostat = ios) t
   if( ios.lt.0 ) exit
   tt = tt + 1
  enddo
  tt_max_bound_slo_disc = tt - 1

  allocate( bound_opt2(num_of_bound_point) )
  allocate( bound_opt2_loci(num_of_bound_point), bound_opt2_locj(num_of_bound_point) )

 endif

 allocate( t_bound_slo_disc(0:tt_max_bound_slo_disc), bound_slo_disc(ny, nx), &
  bound_slo_disc_idx(0:tt_max_bound_slo_disc, slo_count), rdummy_dim(slo_count) )
 rewind(17)

 bound_slo_disc = -999.9d0
 bound_slo_disc_idx = -999.9d0
 rdummy_dim = -999.9d0

 !if( bound_slo_disc_switch .eq. 1 ) then
 if( bound_slo_disc_switch .eq. 2 ) then

  do tt = 0, tt_max_bound_slo_disc
   read(17, *) t_bound_slo_disc(tt), itemp, jtemp
   do i = 1, ny
    read(17,*) (bound_slo_disc(i, j), j = 1, nx)
   enddo
   call sub_slo_ij2idx( bound_slo_disc, rdummy_dim )
   do k = 1, slo_count
    bound_slo_disc_idx(tt, k) = rdummy_dim(k)
   enddo
  enddo
  deallocate( bound_slo_disc, rdummy_dim )
  write(*,*) "done: reading boundary file for slope cells (disc)"
  close(17)

 else ! option 1

  read(17, *) itemp
  read(17, *) ctemp, (bound_opt2_loci(k), k = 1, num_of_bound_point)
  read(17, *) ctemp, (bound_opt2_locj(k), k = 1, num_of_bound_point)

  do tt = 0, tt_max_bound_slo_disc
   read(17, *) t_bound_slo_disc(tt), (bound_opt2(k), k = 1, num_of_bound_point)
   do k = 1, num_of_bound_point
    bound_slo_disc( bound_opt2_loci(k), bound_opt2_locj(k) ) = bound_opt2(k)
   enddo
   call sub_slo_ij2idx( bound_slo_disc, rdummy_dim )
   do k = 1, slo_count
    bound_slo_disc_idx(tt, k) = rdummy_dim(k)
   enddo
  enddo
  deallocate( bound_slo_disc, rdummy_dim )
  write(*,*) "done: reading boundary file for slope cells (wlev)"
  close(17)
  deallocate( bound_opt2 )
  deallocate( bound_opt2_loci, bound_opt2_locj )

 endif
endif

! for river cells (disc)

if( bound_riv_disc_switch .eq. 1 .or. bound_riv_disc_switch .eq. 2 ) then
 open( 18, file = boundfile_riv_disc, status = 'old' )

 tt = 0
 !if( bound_riv_disc_switch .eq. 1 ) then
 if( bound_riv_disc_switch .eq. 2 ) then

  do
   read(18, *, iostat = ios) t, itemp, jtemp
   if(nx.ne.itemp .or. ny.ne.jtemp) stop "error in boundary file (riv, disc)"
   do i = 1, ny
    read(18, *, iostat = ios) (rdummy, j = 1, nx)
   enddo
   if( ios.lt.0 ) exit
   tt = tt + 1
  enddo
  tt_max_bound_riv_disc = tt - 1

 else ! option 1

  read(18, *) num_of_bound_point
  read(18, '(a)') ctemp
  read(18, '(a)') ctemp
  do
   read(18, *, iostat = ios) t
   if( ios.lt.0 ) exit
   tt = tt + 1
  enddo
  tt_max_bound_riv_disc = tt - 1

  allocate( bound_opt2(num_of_bound_point) )
  allocate( bound_opt2_loci(num_of_bound_point), bound_opt2_locj(num_of_bound_point) )

 endif

 allocate( t_bound_riv_disc(0:tt_max_bound_riv_disc), bound_riv_disc(ny, nx), &
  bound_riv_disc_idx(0:tt_max_bound_riv_disc, riv_count), rdummy_dim(riv_count) )
 rewind(18)

 bound_riv_disc = -999.9d0
 bound_riv_disc_idx = -999.9d0
 rdummy_dim = -999.9d0

 !if( bound_riv_disc_switch .eq. 1 ) then
 if( bound_riv_disc_switch .eq. 2 ) then

  do tt = 0, tt_max_bound_riv_disc
   read(18, *) t_bound_riv_disc(tt), itemp, jtemp
   do i = 1, ny
    read(18,*) (bound_riv_disc(i, j), j = 1, nx)
   enddo
   call sub_riv_ij2idx( bound_riv_disc, rdummy_dim )
   do k = 1, riv_count
    bound_riv_disc_idx(tt, k) = rdummy_dim(k)
   enddo
  enddo
  deallocate( bound_riv_disc, rdummy_dim )
  write(*,*) "done: reading boundary file for river cells (disc)"
  close(18)

 else ! option 1

  read(18, *) itemp
  read(18, *) ctemp, (bound_opt2_loci(k), k = 1, num_of_bound_point)
  read(18, *) ctemp, (bound_opt2_locj(k), k = 1, num_of_bound_point)

  do tt = 0, tt_max_bound_riv_disc
   read(18, *) t_bound_riv_disc(tt), (bound_opt2(k), k = 1, num_of_bound_point)
   do k = 1, num_of_bound_point
    bound_riv_disc( bound_opt2_loci(k), bound_opt2_locj(k) ) = bound_opt2(k)
   enddo
   call sub_riv_ij2idx( bound_riv_disc, rdummy_dim )
   do k = 1, riv_count
    bound_riv_disc_idx(tt, k) = rdummy_dim(k)
   enddo
  enddo
  deallocate( bound_riv_disc, rdummy_dim )
  write(*,*) "done: reading boundary file for river cells (disc)"
  close(18)
  deallocate( bound_opt2 )
  deallocate( bound_opt2_loci, bound_opt2_locj )

 endif
endif
end subroutine load_boundary_conditions
!===============================================================
!
!===============================================================
subroutine load_hydro_output()
  use mod_globals
  implicit none
  character(256), parameter :: hydro_file = 'hydro.txt'
  character(256), parameter :: hydro_hr_file = 'hydro_hr.txt'

! emb file
!if( emb_switch.eq.1 ) then
! allocate (emb_r(ny, nx), emb_b(ny, nx))
! call read_gis_real(embrfile, emb_r)
! call read_gis_real(embbfile, emb_b)

! call sub_slo_ij2idx(emb_r, emb_r_idx)
! call sub_slo_ij2idx(emb_b, emb_b_idx)
!endif

! hydro file
if( hydro_switch .eq. 1 ) then
 open( 5, file = location_file, status = "old")
 open(1012, file = hydro_file )
 open(1013, file = hydro_hr_file )
 i = 1
 do
  read(5,*,iostat=ios) ctemp, itemp, itemp
  if(ios.ne.0) exit
  i = i + 1
 enddo
 maxhydro = i-1
 rewind(5)
 allocate( hydro_i(maxhydro), hydro_j(maxhydro) )
 do i = 1, maxhydro
  read(5,*) ctemp, hydro_i(i), hydro_j(i)
 enddo
 close(5)
endif
end subroutine load_hydro_output
!===============================================================
!
!===============================================================
end module mod_config
