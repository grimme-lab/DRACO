module draco_charges
    use mctc_env, only: wp, error_type
    use mctc_io, only: structure_type
    use tblite_wavefunction, only: wavefunction_type, new_wavefunction, eeq_guess, shell_partition
    use tblite_xtb_calculator, only: xtb_calculator, new_xtb_calculator
    use tblite_xtb_gfn2, only: new_gfn2_calculator
    use tblite_ceh_ceh, only: ceh_guess, new_ceh_calculator
    use tblite_ceh_calculator, only : ceh_calculator
    use tblite_context, only: context_type
    implicit none
    real(wp), parameter :: kt= 3.166808578e-6_wp
    private

    public :: ceh, eeq, get_cn


contains


    subroutine ceh(mol,charges,error)
        type(structure_type), intent(in) :: mol
        real(wp), dimension(:), intent(out) :: charges
        type(error_type), intent(out), allocatable, optional :: error

        type(wavefunction_type) :: wfn
        type(ceh_calculator) :: calc
        type(context_type) :: ctx


        call new_ceh_calculator(calc, mol)
        call new_wavefunction(wfn,mol%nat,calc%bas%nsh,calc%bas%nao,1,298.15_wp*kt)
        call ceh_guess(ctx,calc,mol,error,wfn,0)
        charges(:)=wfn%qat(:,1)

    end subroutine ceh

    subroutine eeq(mol,charges)
        type(structure_type), intent(in) :: mol
        real(wp), dimension(:), intent(out) :: charges

        type(xtb_calculator) :: calc
        type(wavefunction_type) :: wfn

        call new_gfn2_calculator(calc,mol)
        call new_wavefunction(wfn,mol%nat,calc%bas%nsh,calc%bas%nao,1,298.15_wp*kt)
        call eeq_guess(mol,calc,wfn)
        charges(:)=wfn%qat(:,1)

    end subroutine eeq

    subroutine get_cn(mol,cn, error)
        use tblite_ncoord, only: new_ncoord
        type(structure_type), intent(in) :: mol
        real(wp), dimension(:), intent(inout) :: cn
        type(error_type), intent(out), allocatable, optional :: error

        type(xtb_calculator) :: calc
        type(wavefunction_type) :: wfn

        call new_gfn2_calculator(calc, mol)
        call new_wavefunction(wfn,mol%nat,calc%bas%nsh,calc%bas%nao,1,298.15_wp*kt)
        call new_ncoord(calc%ncoord,mol,"gfn")
        call calc%ncoord%get_cn(mol, cn)

    end subroutine get_cn

end module draco_charges
