module draco_write
    use mctc_env, only: wp
    implicit none

    private

    interface write_charges
        module procedure :: write_charges_to_default
        module procedure :: write_charges_to_file
        module procedure :: write_charges_to_unit
    end interface write_charges

    public :: write_charges
    
contains

    subroutine write_charges_to_default(charges)
        real(wp), dimension(:), intent(in) :: charges

        integer :: iunit

        open(newunit=iunit, file='draco_charges', status='replace')
        call write_charges_to_unit(iunit,charges)
        close(iunit)
    end subroutine write_charges_to_default

    subroutine write_charges_to_file(file,charges)
        real(wp), dimension(:), intent(in) :: charges
        character(len=*), intent(in) :: file

        integer :: iunit

        open(newunit=iunit, file=file, status='replace')
        call write_charges_to_unit(iunit,charges)
        close(iunit)
    end subroutine write_charges_to_file

    subroutine write_charges_to_unit(iunit,charges)
        real(wp), dimension(:), intent(in) :: charges
        integer, intent(in) :: iunit

        integer :: i

        do i=1, size(charges)
            write(iunit,'(g0)') charges(i)
        end do
    end subroutine write_charges_to_unit

end module draco_write