!
! This work (Common Community Physics Package), identified by NOAA, NCAR,
! CU/CIRES, is free of known copyright restrictions and is placed in the
! public domain.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
! THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
! IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
! CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!

!>
!! @brief Auto-generated cap module for the chemistry scheme
!!
!
module chemistry_cap

    use, intrinsic :: iso_c_binding,                                   &
                      only: c_f_pointer, c_ptr, c_int32_t
    use            :: ccpp_types,                                      &
                      only: ccpp_t
    use            :: ccpp_fields,                                     &
                      only: ccpp_field_get
    use            :: ccpp_errors,                                     &
                      only: ccpp_error, ccpp_debug
    use            :: chemistry, &
                      only: chemistry_run,chemistry_finalize,chemistry_init
    ! Other modules required, e.g. type definitions
    
       use machine, only: kind_phys
       use GFS_typedefs, only: GFS_statein_type,  GFS_stateout_type,    &
                               GFS_sfcprop_type,                        &
                               GFS_coupling_type, GFS_control_type,     &
                               GFS_grid_type,     GFS_tbd_type,         &
                               GFS_cldprop_type,  GFS_radtend_type,     &
                               GFS_diag_type,     GFS_interstitial_type,&
                               GFS_init_type


    implicit none

    private
    public :: chemistry_run_cap,chemistry_finalize_cap,chemistry_init_cap

    contains


    function chemistry_run_cap(ptr) bind(c) result(ierr)

        integer(c_int32_t)         :: ierr
        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer           :: cdata
        type(c_ptr)                     :: cptr
        integer, allocatable            :: cdims(:)
        real(kind_phys), pointer :: co(:)
        character(len=512), pointer :: errmsg
        integer, pointer :: errflg

        ierr = 0

        call c_f_pointer(ptr, cdata)


        call ccpp_field_get(cdata, 'my_volume_mixing_ratio_co', co, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve my_volume_mixing_ratio_co from CCPP data structure')
            return
        end if

        call ccpp_field_get(cdata, 'error_message', errmsg, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve error_message from CCPP data structure')
            return
        end if

        call ccpp_field_get(cdata, 'error_flag', errflg, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve error_flag from CCPP data structure')
            return
        end if

        call chemistry_run(co=co,errmsg=errmsg,errflg=errflg)
        ierr=errflg

    end function chemistry_run_cap

    function chemistry_finalize_cap(ptr) bind(c) result(ierr)

        integer(c_int32_t)         :: ierr
        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer           :: cdata
        type(c_ptr)                     :: cptr
        integer, allocatable            :: cdims(:)


        ierr = 0

        call c_f_pointer(ptr, cdata)



        call chemistry_finalize()
        

    end function chemistry_finalize_cap

    function chemistry_init_cap(ptr) bind(c) result(ierr)

        integer(c_int32_t)         :: ierr
        type(c_ptr), intent(inout) :: ptr

        type(ccpp_t), pointer           :: cdata
        type(c_ptr)                     :: cptr
        integer, allocatable            :: cdims(:)
        real(kind_phys), pointer :: co(:)
        character(len=512), pointer :: errmsg
        integer, pointer :: errflg

        ierr = 0

        call c_f_pointer(ptr, cdata)


        call ccpp_field_get(cdata, 'my_volume_mixing_ratio_co', co, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve my_volume_mixing_ratio_co from CCPP data structure')
            return
        end if

        call ccpp_field_get(cdata, 'error_message', errmsg, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve error_message from CCPP data structure')
            return
        end if

        call ccpp_field_get(cdata, 'error_flag', errflg, ierr)
        if (ierr /= 0) then
            call ccpp_error('Unable to retrieve error_flag from CCPP data structure')
            return
        end if

        call chemistry_init(co=co,errmsg=errmsg,errflg=errflg)
        ierr=errflg

    end function chemistry_init_cap
end module chemistry_cap
