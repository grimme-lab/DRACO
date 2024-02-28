module draco_read
   use mctc_io, only: toNumber => to_number
   use mctc_env, only: wp, error_type, fatal_error
   implicit none
   private

   interface rdparam_solvscale
      module procedure rdparam_solvscale_file
      module procedure rdparam_solvscale_id
   end interface rdparam_solvscale

   interface read_charges
      module procedure readcharges_file
      module procedure readcharges_id
   end interface read_charges

   interface read_cn
      module procedure readcn_file
      module procedure readcn_id
   end interface read_cn

   public :: rdparam_solvscale, read_charges, read_cn

contains

   subroutine rdparam_solvscale_file(file, prefac, expo, o_shift, k1, error)

      character(len=*), intent(in) :: file
      real(wp), intent(inout) :: prefac(:), expo(:), o_shift(:), k1(:)

      type(error_type), allocatable, intent(out) :: error

      integer :: id, err

      open (newunit=id, file=file, iostat=err)
      if (err .ne. 0) then
         call fatal_error(error, "Error while opening file "//trim(file)//" for parameter reading")
         return
      end if

      call rdparam_solvscale_id(id, prefac, expo, o_shift, k1, error)

      close (id)

   end subroutine rdparam_solvscale_file

   subroutine rdparam_solvscale_id(id, prefac, expo, o_shift, k1, error)

      integer, intent(in) :: id
      real(wp), intent(inout) :: prefac(:), expo(:), o_shift(:), k1(:)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      character(len=10) :: line
      integer :: ie, at, err
      real(wp) :: ddum

      do
         read (id, *, iostat=err) line
         if (err < 0) exit
         if (err > 0) then
            call fatal_error(error, "Error while reading parameter from file")
            return
         end if
         if (line == '') exit

         at = toNumber(trim(line))
         !if(.not. any(fitted_atoms == at)) write(*,*) 'atomic number', at, &
         !        & 'not supported as parameter input'
         read (id, *, iostat=err) o_shift(at)
         read (id, *, iostat=err) prefac(at)
         read (id, *, iostat=err) expo(at)
         read (id, *, iostat=err) k1(at)
         if (err .ne. 0) then
            call fatal_error(error, "Error while reading parameter for atom "//trim(line)//" from file")
            return
         end if
      end do

   end subroutine rdparam_solvscale_id

   subroutine readcharges_file(file, charges, error)

      character(len=*), intent(in) :: file
      real(wp), intent(inout) :: charges(:)

      type(error_type), allocatable, intent(out) :: error

      integer :: id, err

      logical :: ex

      inquire (file=file, exist=ex)
      if (.not. ex) then
         call fatal_error(error, "File "//trim(file)//" for custom charges reading does not exist")
         return
      end if
      open (newunit=id, file=file, iostat=err)
      if (err .ne. 0) then
         call fatal_error(error, "Error while opening file "//trim(file)//" for custom charges reading")
         return
      end if

      call readcharges_id(id, charges, error)

      close (id)

   end subroutine readcharges_file

   subroutine readcharges_id(id, charges, error)

      integer, intent(in) :: id
      real(wp), intent(inout) :: charges(:)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      character(len=10) :: line
      integer :: ie, at, err
      real(wp) :: ddum

      at = 0
      do
         read (id, *, iostat=err) line
         if (err < 0) exit
         if (err > 0) then
            call fatal_error(error, "Error while reading custom charges from file")
            return
         end if
         if (line == '') exit
         read (line, *) ddum

         if (at > size(charges)) then
            call fatal_error(error, "Too many custom charges in file")
            return
         end if
         at = at + 1
         charges(at) = ddum
         if (err .ne. 0) then
            write (at, *) line
            call fatal_error(error, "Error while reading custom charge for atom "//line//" from file")
            return
         end if
      end do

      if (at < size(charges)) then
         call fatal_error(error, "Too few custom charges in file")
         return
      end if

   end subroutine readcharges_id

   subroutine readcn_file(file, cn, error)
      character(len=*), intent(in) :: file
      real(wp), intent(inout), allocatable :: cn(:)

      type(error_type), allocatable, intent(out) :: error

      integer :: id, err
      logical :: ex

      inquire (file=file, exist=ex)
      if (.not. ex) then
         deallocate (cn)
         return
      end if
      open (newunit=id, file=file, iostat=err)

      if (err .ne. 0) then
         call fatal_error(error, "Error while opening file "//trim(file)//" for custom CN reading")
         return
      end if

      call readcn_id(id, cn, error)

      close (id)
   end subroutine readcn_file

   subroutine readcn_id(id, cn, error)
      integer, intent(in) :: id
      real(wp), intent(inout) :: cn(:)

      !> Error handling
      type(error_type), allocatable, intent(out) :: error

      character(len=10) :: line
      integer :: ie, at, err
      real(wp) :: ddum

      at = 0
      do
         read (id, *, iostat=err) line
         if (err < 0) exit
         if (err > 0) then
            call fatal_error(error, "Error while reading custom CN from file")
            return
         end if
         if (line == '') exit
         read (line, *) ddum

         if (at > size(cn)) then
            call fatal_error(error, "Too many custom CN in file")
            return
         end if
         at = at + 1
         cn(at) = ddum
         if (err .ne. 0) then
            write (at, *) line
            call fatal_error(error, "Error while reading custom CN for atom "//line//" from file")
            return
         end if
      end do

      if (at < size(cn)) then
         call fatal_error(error, "Too few custom CN in file")
         return
      end if

   end subroutine readcn_id

end module draco_read
