module xml_data_input
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_

type equations_t
   character(len=17)                                :: Formulation
   integer                                         :: iVisc
end type equations_t

type multidomain_t
   integer                                         :: ndomain
   integer                                         :: ngc
   integer                                         :: ngls
end type multidomain_t

type geometry_t
   real(kind=kind(1.0d0))                          :: xlen
   real(kind=kind(1.0d0))                          :: ylen
   real(kind=kind(1.0d0))                          :: zlen
   real(kind=kind(1.0d0))                          :: xstart
   real(kind=kind(1.0d0))                          :: ystart
   real(kind=kind(1.0d0))                          :: zstart
   integer                                         :: imax
   integer                                         :: jmax
   integer                                         :: kmax
end type geometry_t

type runtime_t
   integer                                         :: restart
end type runtime_t

type input_type
   type(equations_t)                               :: Equations
   type(runtime_t)                                 :: RunTimeParameters
   type(multidomain_t)                             :: MultiDomain
   type(geometry_t)                                :: Geometry
end type input_type
   type(input_type)                                :: input_data
contains
subroutine read_xml_type_equations_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(equations_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(equations_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_equations_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_equations_t_array

subroutine read_xml_type_equations_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(equations_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_Formulation
   logical                                         :: has_iVisc
   has_Formulation                      = .false.
   has_iVisc                            = .false.
   call init_xml_type_equations_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('Formulation')
         call read_xml_word( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Formulation, has_Formulation )
      case('iVisc')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%iVisc, has_iVisc )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_Formulation ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on Formulation')
   endif
   if ( .not. has_iVisc ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on iVisc')
   endif
end subroutine read_xml_type_equations_t
subroutine init_xml_type_equations_t_array( dvar )
   type(equations_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_equations_t_array
subroutine init_xml_type_equations_t(dvar)
   type(equations_t) :: dvar
end subroutine init_xml_type_equations_t
subroutine write_xml_type_equations_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(equations_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_equations_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_equations_t_array

subroutine write_xml_type_equations_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(equations_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_word( info, 'Formulation', indent+3, dvar%Formulation)
   call write_to_xml_integer( info, 'iVisc', indent+3, dvar%iVisc)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_equations_t

subroutine read_xml_type_multidomain_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(multidomain_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(multidomain_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_multidomain_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_multidomain_t_array

subroutine read_xml_type_multidomain_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(multidomain_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_ndomain
   logical                                         :: has_ngc
   logical                                         :: has_ngls
   has_ndomain                          = .false.
   has_ngc                              = .false.
   has_ngls                             = .false.
   call init_xml_type_multidomain_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('ndomain')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%ndomain, has_ndomain )
      case('ngc')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%ngc, has_ngc )
      case('ngls')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%ngls, has_ngls )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
end subroutine read_xml_type_multidomain_t
subroutine init_xml_type_multidomain_t_array( dvar )
   type(multidomain_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_multidomain_t_array
subroutine init_xml_type_multidomain_t(dvar)
   type(multidomain_t) :: dvar
   dvar%ndomain = 1
   dvar%ngc = 1
   dvar%ngls = 1
end subroutine init_xml_type_multidomain_t
subroutine write_xml_type_multidomain_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(multidomain_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_multidomain_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_multidomain_t_array

subroutine write_xml_type_multidomain_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(multidomain_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_integer( info, 'ndomain', indent+3, dvar%ndomain)
   call write_to_xml_integer( info, 'ngc', indent+3, dvar%ngc)
   call write_to_xml_integer( info, 'ngls', indent+3, dvar%ngls)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_multidomain_t

subroutine read_xml_type_geometry_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(geometry_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(geometry_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_geometry_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_geometry_t_array

subroutine read_xml_type_geometry_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(geometry_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_xlen
   logical                                         :: has_ylen
   logical                                         :: has_zlen
   logical                                         :: has_xstart
   logical                                         :: has_ystart
   logical                                         :: has_zstart
   logical                                         :: has_imax
   logical                                         :: has_jmax
   logical                                         :: has_kmax
   has_xlen                             = .false.
   has_ylen                             = .false.
   has_zlen                             = .false.
   has_xstart                           = .false.
   has_ystart                           = .false.
   has_zstart                           = .false.
   has_imax                             = .false.
   has_jmax                             = .false.
   has_kmax                             = .false.
   call init_xml_type_geometry_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('xlen')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%xlen, has_xlen )
      case('ylen')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%ylen, has_ylen )
      case('zlen')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%zlen, has_zlen )
      case('xstart')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%xstart, has_xstart )
      case('ystart')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%ystart, has_ystart )
      case('zstart')
         call read_xml_double( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%zstart, has_zstart )
      case('imax')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%imax, has_imax )
      case('jmax')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%jmax, has_jmax )
      case('kmax')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%kmax, has_kmax )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
end subroutine read_xml_type_geometry_t
subroutine init_xml_type_geometry_t_array( dvar )
   type(geometry_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_geometry_t_array
subroutine init_xml_type_geometry_t(dvar)
   type(geometry_t) :: dvar
   dvar%xlen = 1
   dvar%ylen = 1
   dvar%zlen = 1
   dvar%xstart = 0
   dvar%ystart = 0
   dvar%zstart = 0
   dvar%imax = 33
   dvar%jmax = 33
   dvar%kmax = 2
end subroutine init_xml_type_geometry_t
subroutine write_xml_type_geometry_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(geometry_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_geometry_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_geometry_t_array

subroutine write_xml_type_geometry_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(geometry_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_double( info, 'xlen', indent+3, dvar%xlen)
   call write_to_xml_double( info, 'ylen', indent+3, dvar%ylen)
   call write_to_xml_double( info, 'zlen', indent+3, dvar%zlen)
   call write_to_xml_double( info, 'xstart', indent+3, dvar%xstart)
   call write_to_xml_double( info, 'ystart', indent+3, dvar%ystart)
   call write_to_xml_double( info, 'zstart', indent+3, dvar%zstart)
   call write_to_xml_integer( info, 'imax', indent+3, dvar%imax)
   call write_to_xml_integer( info, 'jmax', indent+3, dvar%jmax)
   call write_to_xml_integer( info, 'kmax', indent+3, dvar%kmax)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_geometry_t

subroutine read_xml_type_runtime_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(runtime_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(runtime_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_runtime_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_runtime_t_array

subroutine read_xml_type_runtime_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(runtime_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_restart
   has_restart                          = .false.
   call init_xml_type_runtime_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('restart')
         call read_xml_integer( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%restart, has_restart )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
end subroutine read_xml_type_runtime_t
subroutine init_xml_type_runtime_t_array( dvar )
   type(runtime_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_runtime_t_array
subroutine init_xml_type_runtime_t(dvar)
   type(runtime_t) :: dvar
   dvar%restart = 0
end subroutine init_xml_type_runtime_t
subroutine write_xml_type_runtime_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(runtime_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_runtime_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_runtime_t_array

subroutine write_xml_type_runtime_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(runtime_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_integer( info, 'restart', indent+3, dvar%restart)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_runtime_t

subroutine read_xml_type_input_type_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(input_type), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(input_type), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   newvar(1:newsize-1) = dvar
   deallocate( dvar )
   dvar => newvar

   call read_xml_type_input_type( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_input_type_array

subroutine read_xml_type_input_type( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(input_type), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=80)                            :: tag
   logical                                         :: has_Equations
   logical                                         :: has_RunTimeParameters
   logical                                         :: has_MultiDomain
   logical                                         :: has_Geometry
   has_Equations                        = .false.
   has_RunTimeParameters                = .false.
   has_MultiDomain                      = .false.
   has_Geometry                         = .false.
   call init_xml_type_input_type(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('Equations')
         call read_xml_type_equations_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Equations, has_Equations )
      case('RunTimeParameters')
         call read_xml_type_runtime_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%RunTimeParameters, has_RunTimeParameters )
      case('MultiDomain')
         call read_xml_type_multidomain_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%MultiDomain, has_MultiDomain )
      case('Geometry')
         call read_xml_type_geometry_t( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%Geometry, has_Geometry )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_Equations ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on Equations')
   endif
   if ( .not. has_RunTimeParameters ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on RunTimeParameters')
   endif
   if ( .not. has_MultiDomain ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on MultiDomain')
   endif
   if ( .not. has_Geometry ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on Geometry')
   endif
end subroutine read_xml_type_input_type
subroutine init_xml_type_input_type_array( dvar )
   type(input_type), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_input_type_array
subroutine init_xml_type_input_type(dvar)
   type(input_type) :: dvar
end subroutine init_xml_type_input_type
subroutine write_xml_type_input_type_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(input_type), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_input_type( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_input_type_array

subroutine write_xml_type_input_type( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(input_type)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_xml_type_equations_t( info, 'Equations', indent+3, dvar%Equations)
   call write_xml_type_runtime_t( info, 'RunTimeParameters', indent+3, dvar%RunTimeParameters)
   call write_xml_type_multidomain_t( info, 'MultiDomain', indent+3, dvar%MultiDomain)
   call write_xml_type_geometry_t( info, 'Geometry', indent+3, dvar%Geometry)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_input_type

subroutine read_xml_file_input(fname, lurep, errout)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep
   logical, intent(out), optional         :: errout

   type(XML_PARSE)                        :: info
   logical                                :: error
   character(len=80)                      :: tag
   character(len=80)                      :: starttag
   logical                                :: endtag
   character(len=80), dimension(1:2,1:20) :: attribs
   integer                                :: noattribs
   character(len=200), dimension(1:100)   :: data
   integer                                :: nodata
   logical                                         :: has_input_data
   has_input_data                       = .false.

   call init_xml_file_input
   call xml_open( info, fname, .true. )
   call xml_options( info, report_errors=.true., ignore_whitespace=.true.)
   lurep_ = 0
   if ( present(lurep) ) then
      lurep_ = lurep
      call xml_options( info, report_lun=lurep )
   endif
   do
      call xml_get( info, starttag, endtag, attribs, noattribs, &
         data, nodata)
      if ( starttag .ne. '!--' ) exit
   enddo
   if ( starttag .ne. "cfd_input" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "cfd_input"')
      error = .true.
      call xml_close(info)
      return
   endif
   strict_ = .true.
   error = .false.
   do
      call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
      if ( xml_error(info) ) then
         write(lurep_,*) 'Error reading input file!'
         error = .true.
         return
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('input_data')
         call read_xml_type_input_type( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            input_data, has_input_data )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_input_data ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on input_data')
   endif
   if ( present(errout) ) errout = error
end subroutine

subroutine write_xml_file_input(fname, lurep)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep

   type(XML_PARSE)                        :: info
   integer                                :: indent = 0

   call xml_open( info, fname, .false. )
   call xml_options( info, report_errors=.true.)
   if ( present(lurep) ) then
       call xml_options( info, report_errors=.true.)
   endif
   write(info%lun,'(a)') &
      '<cfd_input>'
   call write_xml_type_input_type( info, 'input_data', indent+3, input_data)
   write(info%lun,'(a)') '</cfd_input>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_input

end subroutine

end module
