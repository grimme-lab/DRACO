module draco_charges
    use mctc_env, only: wp, error_type
    use mctc_io, only: structure_type
    use tblite_wavefunction, only: wavefunction_type, new_wavefunction, shell_partition
    use tblite_xtb_calculator, only: xtb_calculator, new_xtb_calculator
    use tblite_xtb_gfn2, only: new_gfn2_calculator
    use tblite_ceh_ceh, only: ceh_guess, new_ceh_calculator
    use tblite_ceh_calculator, only : ceh_calculator
    use tblite_context, only: context_type
    use tblite_disp_d4, only: get_eeq_charges
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

    subroutine eeq(mol,charges,dqdr,dqdL)
        type(structure_type), intent(in) :: mol
        real(wp), dimension(:), intent(out) :: charges

        type(xtb_calculator) :: calc
        type(wavefunction_type) :: wfn

        real(wp), intent(out), allocatable, optional :: dqdr(:,:,:)
        real(wp), intent(out), allocatable, optional :: dqdL(:,:,:)

        !real(wp), allocatable :: d1t(:,:,:)
        !real(wp), allocatable :: d2t(:,:,:)

        logical :: grad

        grad = present(dqdr) .and. present(dqdL)

        call new_gfn2_calculator(calc,mol)
        call new_wavefunction(wfn,mol%nat,calc%bas%nsh,calc%bas%nao,1,298.15_wp*kt)
        wfn%qat(:, :) = 0.0_wp
        if (grad) then
            allocate(dqdr(3, mol%nat, mol%nat), dqdL(3, 3, mol%nat))
            call get_eeq_charges(mol, wfn%qat(:, 1),dqdr=dqdr,dqdL=dqdL)
            call shell_partition(mol, calc, wfn)
        else
            call get_eeq_charges(mol, wfn%qat(:, 1))
        end if

        charges(:)=wfn%qat(:,1)
        

    end subroutine eeq

    subroutine get_cn(mol,cn, error, dcndr, dcndL)
        use tblite_ncoord, only: new_ncoord
        type(structure_type), intent(in) :: mol
        real(wp), dimension(:), intent(inout) :: cn
        type(error_type), intent(out), allocatable, optional :: error

        real(wp), intent(out), allocatable, optional :: dcndr(:,:,:)
        real(wp), intent(out), allocatable, optional :: dcndL(:,:,:)

        type(xtb_calculator) :: calc
        type(wavefunction_type) :: wfn

        logical :: grad

        grad = present(dcndr) .and. present(dcndL)

        call new_gfn2_calculator(calc, mol)
        call new_wavefunction(wfn,mol%nat,calc%bas%nsh,calc%bas%nao,1,298.15_wp*kt)
        call new_ncoord(calc%ncoord,mol,"gfn")
        if (grad) then
            allocate(dcndr(3, mol%nat, mol%nat), dcndL(3, 3, mol%nat))
            call calc%ncoord%get_cn(mol, cn, dcndr, dcndL)
        else
            call calc%ncoord%get_cn(mol, cn)
        end if

    end subroutine get_cn

end module draco_charges
