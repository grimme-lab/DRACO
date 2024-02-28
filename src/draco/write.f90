module draco_write
   use mctc_env, only: wp
   use mctc_io, only: structure_type, to_symbol
   implicit none

   private

   interface write_charges
      module procedure :: write_charges_to_default
      module procedure :: write_charges_to_file
      module procedure :: write_charges_to_unit
   end interface write_charges

   interface write_cn
      module procedure :: write_cn_to_default
      module procedure :: write_cn_to_file
      module procedure :: write_cn_to_unit
   end interface write_cn

   public :: write_charges, write_cn

contains

   subroutine write_charges_to_default(charges)
      real(wp), dimension(:), intent(in) :: charges

      integer :: iunit

      open (newunit=iunit, file='draco_charges', status='replace')
      call write_charges_to_unit(iunit, charges)
      close (iunit)
   end subroutine write_charges_to_default

   subroutine write_charges_to_file(file, charges)
      real(wp), dimension(:), intent(in) :: charges
      character(len=*), intent(in) :: file

      integer :: iunit

      open (newunit=iunit, file=file, status='replace')
      call write_charges_to_unit(iunit, charges)
      close (iunit)
   end subroutine write_charges_to_file

   subroutine write_charges_to_unit(iunit, charges)
      real(wp), dimension(:), intent(in) :: charges
      integer, intent(in) :: iunit

      integer :: i

      do i = 1, size(charges)
         write (iunit, '(f8.5)') charges(i)
      end do
   end subroutine write_charges_to_unit

   subroutine write_cn_to_default(cn)
      real(wp), dimension(:), intent(in) :: cn

      integer :: iunit

      open (newunit=iunit, file='draco_cn', status='replace')
      call write_cn_to_unit(iunit, cn)
      close (iunit)
   end subroutine write_cn_to_default

   subroutine write_cn_to_file(file, cn)
      real(wp), dimension(:), intent(in) :: cn
      character(len=*), intent(in) :: file

      integer :: iunit

      open (newunit=iunit, file=file, status='replace')
      call write_cn_to_unit(iunit, cn)
      close (iunit)
   end subroutine write_cn_to_file

   subroutine write_cn_to_unit(iunit, cn)
      real(wp), dimension(:), intent(in) :: cn
      integer, intent(in) :: iunit

      integer :: i

      do i = 1, size(cn)
         write (iunit, '(f8.5)') cn(i)
      end do
   end subroutine write_cn_to_unit

end module draco_write
