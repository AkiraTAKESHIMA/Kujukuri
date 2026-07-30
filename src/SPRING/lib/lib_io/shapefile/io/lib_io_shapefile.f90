module lib_io_shapefile
  use iso_c_binding
  implicit none
  private
  !-------------------------------------------------------------
  ! Public types
  !-------------------------------------------------------------
  public :: shp_
  public :: shp_entity_
  public :: shp_part_

  public :: dbf_
  public :: dbf_field_
  public :: dbf_record_
  public :: dbf_value_
  !-------------------------------------------------------------
  ! Public procedures
  !-------------------------------------------------------------
  public :: shp_open
  public :: shp_close
  public :: shp_get_info
  public :: shp_get_entity_info
  public :: shp_get_entity
  public :: shp_get_all
  public :: shp_clear_all
  public :: shp_clear_entity

  public :: dbf_open
  public :: dbf_close
  public :: dbf_get_info
  public :: dbf_get_record
  public :: dbf_get_all
  public :: dbf_clear_all
  public :: dbf_clear_record
  !-------------------------------------------------------------
  ! Public types
  !-------------------------------------------------------------
  type shp_part_
    integer(4) :: id = 0
    integer(4) :: nPoint = 0
    real(8), pointer :: x(:), & !(nPoint)
                        y(:), &
                        z(:), &
                        m(:)
  end type

  type shp_entity_
    integer(4) :: id = 0
    integer(4) :: nPoint = 0
    integer(4) :: nPart = 0
    logical(4) :: part_is_allocated = .false.
    type(shp_part_), pointer :: part(:)  !(nPart)
    real(8) :: xmin = 0.d0, &
               xmax = 0.d0, &
               ymin = 0.d0, &
               ymax = 0.d0, &
               zmin = 0.d0, &
               zmax = 0.d0, &
               mmin = 0.d0, &
               mmax = 0.d0
    logical(4) :: measure_is_used = .false.
  end type

  type shp_
    integer(4) :: nEntity = 0
    logical(4) :: entity_is_allocated = .false.
    type(shp_entity_), pointer :: entity(:)  !(nEntity)
    integer(4) :: SHPType = 0
    character(16) :: SHPTypeName = ''
    real(8) :: minBound(4) = (/0.d0, 0.d0, 0.d0, 0.d0/), &
               maxBound(4) = (/0.d0, 0.d0, 0.d0, 0.d0/)
  end type

  integer, parameter :: CLEN_TITLE = 64

  type dbf_field_
    character(1)          :: typeChar = ''
    character(7)          :: typeName = ''
    character(clen_title) :: title    = ''
    integer(4)            :: width    = 0
    integer(4)            :: decimals = 0
  end type

  type dbf_value_
    character(:), pointer :: s
    integer(4)            :: i = 0
    real(8)               :: d = 0.d0
  end type

  type dbf_record_
    logical :: value_is_allocated = .false.
    type(dbf_value_), pointer :: value(:)  !(nField)
  end type

  type dbf_
    integer(4) :: nField = 0
    integer(4) :: nRecord = 0
    logical :: field_is_allocated = .false.
    logical :: record_is_allocated = .false.
    type(dbf_field_) , pointer :: field(:)
    type(dbf_record_), pointer :: record(:)
  end type
  !-------------------------------------------------------------
  ! Interfaces of public procedures
  !-------------------------------------------------------------
  interface shp_get_info
    module procedure shp_get_info__structure
    module procedure shp_get_info__components
  end interface

  interface shp_get_entity_info
    module procedure shp_get_entity_info__structure
    module procedure shp_get_entity_info__components
  end interface

  interface dbf_get_info
    module procedure dbf_get_info__structure
    module procedure dbf_get_info__components
  end interface
  !-------------------------------------------------------------
  ! Intefaces for external procedures
  !-------------------------------------------------------------
  interface
    function c_shpopen(f) bind(c,name='shpopen')
      import
      integer(c_int) :: c_shpopen
      character(c_char), intent(in) :: f
    end function

    function c_shpclose() bind(c,name='shpclose')
      import
      integer(c_int) :: c_shpclose
    end function

    function c_shpgetinfo(nEntity, SHPType, SHPTypeName, minBound, maxBound) &
    bind(c,name='shpgetinfo')
      import
      integer(c_int) :: c_shpgetinfo
      integer(c_int), intent(out) :: nEntity
      integer(c_int), intent(out) :: SHPType
      character(c_char), intent(out) :: SHPTypeName(*)
      real(c_double), intent(out) :: minBound(*)
      real(c_double), intent(out) :: maxBound(*)
    end function

    function c_shpgetentityinfo(&
        iEntity, nPoint, nPart, measure_is_used, &
        xmin, xmax, ymin, ymax, zmin, zmax, mmin, mmax) &
    bind(c,name='shpgetentityinfo')
      import
      integer(c_int) :: c_shpgetentityinfo
      integer(c_int), value, intent(in) :: iEntity
      integer(c_int), intent(out) :: nPoint
      integer(c_int), intent(out) :: nPart
      integer(c_int), intent(out) :: measure_is_used
      real(c_double), intent(out) :: xmin, xmax, ymin, ymax, &
                                     zmin, zmax, mmin, mmax
    end function

    function c_shpgetpanpartstart(iEntity, panPartStart) &
    bind(c,name='shpgetpanpartstart')
      import
      integer(c_int) :: c_shpgetpanpartstart
      integer(c_int), value, intent(in) :: iEntity
      integer(c_int), intent(out) :: panPartStart(*)
    end function

    function c_shpgetdata(iEntity, x, y, z, m) &
    bind(c,name='shpgetdata')
      import
      integer(c_int) :: c_shpgetdata
      integer(c_int), value, intent(in)  :: iEntity
      real(c_double), intent(out) :: x(*), y(*), z(*), m(*)
    end function

    function c_dbfopen(f) bind(c,name='dbfopen')
      import
      integer(c_int) :: c_dbfopen
      character(c_char), intent(in) :: f
    end function

    function c_dbfclose() bind(c,name='dbfclose')
      import
      integer(c_int) :: c_dbfclose
    end function

    subroutine c_dbfgetfieldcount(nField) bind(c,name='dbfgetfieldcount')
      import
      integer(c_int), intent(out) :: nField
    end subroutine

    subroutine c_dbfgetrecordcount(nRecord) bind(c,name='dbfgetrecordcount')
      import
      integer(c_int), intent(out) :: nRecord
    end subroutine

    function c_dbfgetfieldinfo(&
        iField, &
        typeChar, typeName, title, width, decimals)&
      bind(c,name='dbfgetfieldinfo')
      import
      integer(c_int) :: c_dbfgetfieldinfo
      integer(c_int), value, intent(in) :: iField
      character(c_char), intent(out) :: typeChar(*)
      character(c_char), intent(out) :: typeName(*)
      character(c_char), intent(out) :: title(*)
      integer(c_int)   , intent(out) :: width
      integer(c_int)   , intent(out) :: decimals
    end function

    function c_dbfgetrecord(&
        iRecord, iField, typeChar, &
        recslen, recs, reci, recd)&
    bind(c,name='dbfgetrecord')
      import
      integer(c_int) :: c_dbfgetrecord
      integer(c_int), value, intent(in)  :: iRecord
      integer(c_int), value, intent(in)  :: iField
      character(c_char), intent(in)  :: typeChar(*)
      integer(c_int)   , intent(out) :: recslen
      character(c_char), intent(out) :: recs(*)
      integer(c_int)   , intent(out) :: reci
      real(c_double)   , intent(out) :: recd
    end function
  end interface
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  character(32), parameter :: PROCMOD = 'MODULE lib_io_shapefile'
  !-------------------------------------------------------------
contains
!===============================================================
!
!===============================================================
function fchar(s)
  implicit none
  character(*), intent(in) :: s
  character(len(s)) :: fchar

  fchar = s(:index(s,c_null_char)-1)
end function fchar
!===============================================================
!
!===============================================================
function cchar(s)
  implicit none
  character(*), intent(in) :: s
  character(len_trim(s)+1) :: cchar

  cchar = trim(s)//c_null_char
end function cchar
!===============================================================
!
!===============================================================
subroutine cchar_to_fchar(s)
  implicit none
  character(*), intent(inout) :: s

  s = s(:index(s,c_null_char)-1)
end subroutine cchar_to_fchar
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
integer function shp_open(f) result(info)
  implicit none
  character(*), intent(in) :: f

  info = c_shpopen(cchar(f))
end function shp_open
!===============================================================
!
!===============================================================
integer function shp_close() result(info)
  implicit none

  info = c_shpclose()
end function shp_close
!===============================================================
!
!===============================================================
integer function shp_get_info__structure(shp) result(info)
  implicit none
  type(shp_), intent(out) :: shp

  info = c_shpgetinfo(&
           shp%nEntity, shp%SHPType, shp%SHPTypeName, &
           shp%minBound, shp%maxBound)

  if( info /= 0 )then
    call raise_error_proc('FUNCTION shp_get_info__structure')
    write(0,"(a)") 'Reading error.'
    return
  endif

  call cchar_to_fchar(shp%SHPTypeName)
end function shp_get_info__structure
!===============================================================
!
!===============================================================
integer function shp_get_info__components(&
    nEntity, SHPType, SHPTypeName, minBound, maxBound) result(info)
  implicit none
  integer(4)  , intent(out), optional :: nEntity
  integer(4)  , intent(out), optional :: SHPType
  character(*), intent(out), optional :: SHPTypeName
  real(8)     , intent(out), optional :: minBound(:)  !(4)
  real(8)     , intent(out), optional :: maxBound(:)  !(4)

  integer :: nEntity_
  integer :: SHPType_
  character(16) :: SHPTypeName_
  real(8) :: minBound_(4), maxBound_(4)

  info = c_shpgetinfo(&
           nEntity_, SHPType_, SHPTypeName_, minBound_, maxBound_)

  if( info /= 0 )then
    call raise_error_proc('FUNCTION shp_get_info__components')
    write(0,"(a)") 'Reading error'
    return
  endif

  if( present(nEntity) ) nEntity = nEntity_
  if( present(SHPType) ) SHPType = SHPType_
  if( present(SHPTypeName) ) SHPTypeName = fchar(SHPTypeName_)
  if( present(minBound) ) minBound = minBound_
  if( present(maxBound) ) maxBound = maxBound_
end function shp_get_info__components
!===============================================================
!
!===============================================================
integer function shp_get_entity_info__structure(iEntity, entity) result(info)
  implicit none
  integer(4)       , intent(in)  :: iEntity
  type(shp_entity_), intent(out) :: entity

  info = 0

  call shp_clear_entity(entity)
  entity%id = iEntity

  info = shp_get_entity_info__components(&
           iEntity, &
           entity%nPoint, entity%nPart, entity%measure_is_used, &
           entity%xmin, entity%xmax, &
           entity%ymin, entity%ymax, &
           entity%zmin, entity%zmax, &
           entity%mmin, entity%mmax)
  if( info /= 0 )then
    call raise_error_proc('FUNCTION shp_get_entity_info__structure')
    write(0,"(a)") 'Reading error'
    return
  endif
end function shp_get_entity_info__structure
!===============================================================
!
!===============================================================
integer function shp_get_entity_info__components(&
    iEntity, &
    nPoint, nPart, measure_is_used, &
    xmin, xmax, ymin, ymax, zmin, zmax, mmin, mmax) result(info)
  implicit none
  integer(4), intent(in) :: iEntity
  integer(4), intent(out), optional :: nPoint
  integer(4), intent(out), optional :: nPart
  logical(4), intent(out), optional :: measure_is_used
  real(8)   , intent(out), optional :: xmin, xmax, &
                                       ymin, ymax, &
                                       zmin, zmax, &
                                       mmin, mmax
  integer :: nPoint_, nPart_
  integer :: measure_is_used_
  real(8) :: xmin_, xmax_, ymin_, ymax_, &
             zmin_, zmax_, mmin_, mmax_

  info = c_shpgetentityinfo(&
           iEntity-1, &
           nPoint_, nPart_, measure_is_used_, &
           xmin_, xmax_, ymin_, ymax_, zmin_, zmax_, mmin_, mmax_)

  if( info /= 0 )then
    call raise_error_proc('FUNCTION shp_get_entity_info__components')
    write(0,"(a)") 'Reading error.'
    return
  endif

  if( present(nPoint) ) nPoint = nPoint_
  if( present(nPart) ) nPart = nPart_
  if( present(measure_is_used) ) measure_is_used = measure_is_used_ == 0
  if( present(xmin) ) xmin = xmin_
  if( present(xmax) ) xmax = xmax_
  if( present(ymin) ) ymin = ymin_
  if( present(ymax) ) ymax = ymax_
  if( present(zmin) ) zmin = zmin_
  if( present(zmax) ) zmax = zmax_
  if( present(mmin) ) mmin = mmin_
  if( present(mmax) ) mmax = mmax_
end function shp_get_entity_info__components
!===============================================================
!
!===============================================================
integer function shp_get_entity(iEntity, entity) result(info)
  implicit none
  integer(4)       , intent(in)    :: iEntity
  type(shp_entity_), intent(inout) :: entity

  type(shp_part_)  , pointer :: part
  integer(4) :: iPart
  integer(4), allocatable :: panPartStart(:)
  real(8)   , allocatable :: x(:), y(:), z(:), m(:)
  integer(4) :: ip0
  !-------------------------------------------------------------
  !
  !-------------------------------------------------------------
  info = 0

  call shp_clear_entity(entity)
  entity%id = iEntity

  info = shp_get_entity_info__components(&
           iEntity, &
           entity%nPoint, entity%nPart, entity%measure_is_used, &
           entity%xmin, entity%xmax, &
           entity%ymin, entity%ymax, &
           entity%zmin, entity%zmax, &
           entity%mmin, entity%mmax)
  if( read_error() ) return

  allocate(panPartStart(entity%nPart))

  info = c_shpgetpanpartstart(iEntity-1, panPartStart)
  if( read_error() ) return

  entity%part_is_allocated = .true.
  allocate(entity%part(entity%nPart))
  do iPart = 1, entity%nPart-1
    entity%part(iPart)%nPoint = panPartStart(iPart+1) - panPartStart(iPart)
  enddo
  entity%part(entity%nPart)%nPoint = entity%nPoint - panPartStart(entity%nPart)

  deallocate(panPartStart)
  !-------------------------------------------------------------
  ! Read coordinate data
  !-------------------------------------------------------------
  allocate(x(entity%nPoint), &
           y(entity%nPoint), &
           z(entity%nPoint))
  if( entity%measure_is_used )then
    allocate(m(entity%nPoint))
  else
    allocate(m(1))
  endif

  info = c_shpgetdata(iEntity-1, x, y, z, m)
  if( read_error() ) return

  ip0 = 0
  do iPart = 1, entity%nPart
    part => entity%part(iPart)
    allocate(part%x(part%nPoint), &
             part%y(part%nPoint), &
             part%z(part%nPoint))
    part%x(:) = x(ip0+1:ip0+part%nPoint)
    part%y(:) = y(ip0+1:ip0+part%nPoint)
    part%z(:) = z(ip0+1:ip0+part%nPoint)

    if( entity%measure_is_used )then
      allocate(part%m(part%nPoint))
      part%m(:) = m(ip0+1:ip0+part%nPoint)
    else
      allocate(part%m(1))
      part%m(:) = 0.d0
    endif

    ip0 = ip0 + part%nPoint
  enddo
  !-------------------------------------------------------------
  ! Deallocate
  !-------------------------------------------------------------
  deallocate(x, y, z, m)
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
logical function read_error()
  read_error = .false.
  if( info /= 0 )then    
    call raise_error_proc('FUNCTION shp_get_entity')
    write(0,"(a)") 'Reading error.'
    read_error = .true.
  endif
end function read_error
!---------------------------------------------------------------
end function shp_get_entity
!===============================================================
!
!===============================================================
integer function shp_get_all(shp) result(info)
  implicit none
  type(shp_), intent(out) :: shp

  integer(4) :: iEntity

  call shp_clear_all(shp)
  info = shp_get_info__structure(shp)
  if( read_error() ) return

  shp%entity_is_allocated = .true.
  allocate(shp%entity(shp%nEntity))
  do iEntity = 1, shp%nEntity
    info = shp_get_entity(iEntity, shp%entity(iEntity))
    if( read_error() ) return
  enddo
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
logical function read_error()
  read_error = .false.
  if( info /= 0 )then
    call raise_error_proc('FUNCTION shp_get_all')
    write(0,"(a)") 'Reading error.'
    read_error = .true.
  endif
end function read_error
!---------------------------------------------------------------
end function shp_get_all
!===============================================================
!
!===============================================================
subroutine shp_clear_all(shp)
  implicit none
  type(shp_), intent(out) :: shp

  shp%nEntity = 0
  if( shp%entity_is_allocated )then
    shp%entity_is_allocated = .false.
    deallocate(shp%entity)
  endif
  shp%SHPType = 0
  shp%SHPTypeName = ''
  shp%minBound(:) = 0.d0
  shp%maxBound(:) = 0.d0
end subroutine shp_clear_all
!===============================================================
!
!===============================================================
subroutine shp_clear_entity(ent)
  implicit none
  type(shp_entity_), intent(inout) :: ent

  ent%id = 0
  ent%nPart = 0
  if( ent%part_is_allocated )then
    ent%part_is_allocated = .false.
    deallocate(ent%part)
  endif
  ent%nPoint = 0
  ent%xmin = 0.d0
  ent%xmax = 0.d0
  ent%ymin = 0.d0
  ent%ymax = 0.d0
  ent%zmin = 0.d0
  ent%zmax = 0.d0
  ent%mmin = 0.d0
  ent%mmax = 0.d0
  ent%measure_is_used = .false.
end subroutine shp_clear_entity
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
integer function dbf_open(f) result(info)
  implicit none
  character(*), intent(in) :: f

  info = c_dbfopen(cchar(f))

  if( info /= 0 )then
    call raise_error_proc('FUNCTION dbf_open')
    write(0,"(a)") 'Failed to open: '//trim(f)
    return
  endif
end function dbf_open
!===============================================================
!
!===============================================================
integer function dbf_close() result(info)
  implicit none

  info = c_dbfclose()

  if( info /= 0 )then
    call raise_error_proc('FUNCTION dbf_close')
    write(0,"(a)") 'File is not opened.'
    return
  endif
end function dbf_close
!===============================================================
!
!===============================================================
integer function dbf_get_info__structure(dbf) result(info)
  implicit none
  type(dbf_), intent(out) :: dbf

  type(dbf_field_), pointer :: field
  integer :: iField

  info = 0

  call dbf_clear_all(dbf)

  call c_dbfgetfieldcount(dbf%nField)
  call c_dbfgetrecordcount(dbf%nRecord)

  dbf%field_is_allocated = .true.
  dbf%record_is_allocated = .true.
  allocate(dbf%field(dbf%nField))
  allocate(dbf%record(dbf%nRecord))

  do iField = 1, dbf%nField
    field => dbf%field(iField)
    info = c_dbfgetfieldinfo(&
             iField-1, &
             field%typeChar, field%typeName, field%title, &
             field%width, field%decimals)
    if( info /= 0 )then
      call raise_error_proc('FUNCTION dbf_get_info__structure')
      write(0,"(a)") 'Reading error.'
      return
    endif
  enddo

  nullify(field)
end function dbf_get_info__structure
!===============================================================
!
!===============================================================
integer function dbf_get_info__components(&
    nField, nRecord, &
    typeChar, typeName, title, width, decimals) result(info)
  implicit none
  integer(4)  , intent(out), optional :: nField
  integer(4)  , intent(out), optional :: nRecord
  character(*), pointer    , optional :: typeChar(:)
  character(*), pointer    , optional :: typeName(:)
  character(*), pointer    , optional :: title(:)
  integer(4)  , pointer    , optional :: width(:)
  integer(4)  , pointer    , optional :: decimals(:)

  integer(4) :: nField_
  integer(4) :: nRecord_
  character(1) :: typeChar_
  character(7) :: typeName_
  character(CLEN_TITLE) :: title_
  integer(4)            :: width_
  integer(4)            :: decimals_
  integer(4) :: iField

  info = 0

  call c_dbfgetfieldcount(nField_)
  call c_dbfgetrecordcount(nRecord_)

  if( present(nField)  ) nField  = nField_
  if( present(nRecord) ) nRecord = nRecord_

  if( nField_ == 0 ) return

  if( .not. ( present(typeChar) .or. &
              present(typeName) .or. &
              present(title) .or. &
              present(width) .or. &
              present(decimals) ) ) return

  if( present(typeChar) )then
    if( associated(typeChar) ) deallocate(typeChar)
    allocate(typeChar(nField_))
  endif

  if( present(typeName) )then
    if( associated(typeName) ) deallocate(typeName)
    allocate(typeName(nField_))
  endif

  if( present(title) )then
    if( associated(title) ) deallocate(title)
    allocate(title(nField_))
  endif

  if( present(width) )then
    if( associated(width) ) deallocate(width)
    allocate(width(nField_))
  endif

  if( present(decimals) )then
    if( associated(decimals) ) deallocate(decimals)
    allocate(decimals(nField_))
  endif

  do iField = 1, nField_
    info = c_dbfgetfieldinfo(&
             iField-1, &
             typeChar_, typeName_, &
             title_, width_, decimals_)
    if( info /= 0 )then
      call raise_error_proc('FUNCTION dbf_get_info__components')
      write(0,"(a)") 'Reading error.'
      return
    endif

    if( present(typeChar) ) typeChar(iField) = typeChar_
    if( present(typeName) ) typeName(iField) = fchar(typeName_)
    if( present(title)    ) title(iField)    = fchar(title_)
    if( present(width)    ) width(iField)    = width_
    if( present(decimals) ) decimals(iField) = decimals_
  enddo
end function dbf_get_info__components
!===============================================================
!
!===============================================================
integer function dbf_get_record(iRecord, field, record) result(info)
  implicit none
  integer(4)       , intent(in)  :: iRecord
  type(dbf_field_) , intent(in)  :: field(:)
  type(dbf_record_), intent(out) :: record
  integer(4)            :: nField
  integer(4)   :: iField
  character(1) :: typeChar
  integer(4)   :: recslen
  character(:), allocatable :: recs
  integer(4)   :: reci
  real(8)      :: recd

  call dbf_clear_record(record)

  nField = size(field)

  record%value_is_allocated = .true.
  allocate(record%value(nField))

  do iField = 1, nField
    if( field(iField)%typeChar == 'C' )then
      recslen = field(iField)%width
      allocate(character(recslen) :: record%value(iField)%s)
      record%value(iField)%s = ''
    endif
  enddo

  allocate(character(maxval(field(:)%width)*2) :: recs)

  do iField = 1, nField
    typeChar = field(iField)%typeChar
    info = c_dbfgetrecord(iRecord-1, iField-1, typeChar, &
                          recslen, recs, reci, recd)
    if( read_error() ) return
    if( recslen > 0 )then
      allocate(character(recslen) :: record%value(iField)%s)
      record%value(iField)%s = recs(:recslen)
    endif
    record%value(iField)%i = reci
    record%value(iField)%d = recd
  enddo
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
logical function read_error()
  read_error = .false.
  if( info /= 0 )then
    call raise_error_proc('FUNCTION dbf_get_record')
    write(0,"(a)") 'Reading error.'
    read_error = .true.
  endif
end function read_error
!---------------------------------------------------------------
end function dbf_get_record
!===============================================================
!
!===============================================================
integer function dbf_get_all(dbf) result(info)
  implicit none
  type(dbf_), intent(out) :: dbf

  integer(4) :: iRecord

  info = dbf_get_info__structure(dbf)
  if( read_error() ) return

  do iRecord = 1, dbf%nRecord
    info = dbf_get_record(iRecord, dbf%field, dbf%record(iRecord))
    if( read_error() ) return
  enddo
!---------------------------------------------------------------
contains
!---------------------------------------------------------------
logical function read_error()
  read_error = .false.
  if( info /= 0 )then
    call raise_error_proc('FUNCTION dbf_get_all')
    write(0,"(a)") 'Reading error.'
    read_error = .true.
  endif
end function read_error
!---------------------------------------------------------------
end function dbf_get_all
!===============================================================
!
!===============================================================
subroutine dbf_clear_all(dbf)
  implicit none
  type(dbf_), intent(out) :: dbf

  dbf%nField  = 0
  dbf%nRecord = 0
  if( dbf%field_is_allocated )then
    dbf%field_is_allocated = .false.
    deallocate(dbf%field)
  endif
  if( dbf%record_is_allocated )then
    dbf%record_is_allocated = .false.
    deallocate(dbf%record)
  endif
end subroutine dbf_clear_all
!===============================================================
!
!===============================================================
subroutine dbf_clear_record(record)
  implicit none
  type(dbf_record_), intent(inout) :: record

  if( record%value_is_allocated )then
    record%value_is_allocated = .false.
    deallocate(record%value)
  endif
end subroutine dbf_clear_record
!===============================================================
!
!===============================================================
!
!
!
!
!
!===============================================================
!
!===============================================================
subroutine raise_error_proc(proc)
  implicit none
  character(*), intent(in) :: proc

  write(0,"(a)") '*** @ '//trim(PROCMOD)//' '//trim(proc)//' ***'
end subroutine raise_error_proc
!===============================================================
!
!===============================================================
end module lib_io_shapefile
