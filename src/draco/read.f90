module draco_read
    use mctc_io, only: toNumber => to_number
    use mctc_env, only: wp, error_type, fatal_error
    implicit none
    private

    interface rdparam_solvscale
       module procedure rdparam_solvscale_file
       module procedure rdparam_solvscale_id
    end interface rdparam_solvscale

    public :: rdparam_solvscale

contains

   subroutine rdparam_solvscale_file(file, prefac, expo, o_shift, error)

      character(len=*), intent(in) :: file
      real(wp), intent(inout) :: prefac(:), expo(:), o_shift(:)

      type(error_type), allocatable, intent(out) :: error

      integer :: id, err

      open(newunit=id,file=file,iostat=err)
      if (err.ne.0) then
         call fatal_error(error, "Error while opening file " // trim(file) // " for parameter reading")
         return
      end if

      call rdparam_solvscale_id(id, prefac, expo, o_shift,error)

      close(id)

   end subroutine rdparam_solvscale_file

   subroutine rdparam_solvscale_id(id, prefac, expo, o_shift,error)

      integer, intent(in) :: id
      real(wp), intent(inout) :: prefac(:), expo(:), o_shift(:)

      !> Error handling
        type(error_type), allocatable, intent(out) :: error

      character(len=10) :: line
      integer :: ie, at, err
      real(wp) :: ddum

      do
         read(id,*,iostat=err) line
         if (err<0) exit
         if (err>0) then
            call fatal_error(error, "Error while reading parameter from file")
            return
         end if
         if (line == '') exit

         at = toNumber(trim(line))
         !if(.not. any(fitted_atoms == at)) write(*,*) 'atomic number', at, &
         !        & 'not supported as parameter input'
         read(id,*,iostat=err) o_shift(at)
         read(id,*,iostat=err) prefac(at)
         read(id,*,iostat=err) expo(at)
         read(id,*,iostat=err)
         if (err.ne.0) then
            call fatal_error(error, "Error while reading parameter for atom " // trim(line) // " from file")
            return
         end if
!         if(getValue(env,line,ddum)) y_shift(at) = ddum

         !if(set%verbose) then
         !   write(env%unit,'(''   '',i0,f7.4,f7.4,f7.4)') at, prefac(at), expo(at), o_shift(at)
         !end if

      enddo

   end subroutine rdparam_solvscale_id

end module draco_read