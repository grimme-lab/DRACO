module draco_type
    use mctc_io, only: structure_type, get_filetype, read_structure
    use mctc_env, only: wp, error_type, fatal_error
    use draco_data, only: getVanDerWaalsRadCosmo, getVanDerWaalsRadCPCM
    implicit none
    private

    public :: TDraco

    type TDraco
        type(structure_type) :: mol
        real(wp), dimension(:), allocatable :: defaultradii
        real(wp), dimension(:), allocatable :: charges
        real(wp), dimension(:), allocatable :: scaledradii

        !> Parametrisation
        real(wp), dimension(94) :: prefac, expo, o_shift

        !> Radtype
        character(len=:), allocatable :: radtype

    contains
        procedure :: init => draco_init
        procedure :: loadParam => draco_load_parameter
        procedure :: readParam => draco_read_param
        procedure :: charge => draco_charge
        procedure :: calc => draco_scale
    end type TDraco

contains

    subroutine draco_init(self, file, charge, qmodel, radtype, error)
        use iso_fortran_env, only: output_unit
        class(TDraco), intent(inout) :: self
        character(len=*), intent(in) :: file
        !> Charge of the molecule
        integer, intent(in) :: charge
        !> Charge model
        character(len=*), intent(in) :: qmodel
        !> Radii type
        character(len=*), intent(in) :: radtype
        !> Error handling
        type(error_type), allocatable, intent(inout), optional :: error
        type(error_type), allocatable :: local_error

        integer :: i

        call read_structure(self%mol, file, local_error, get_filetype(file))
        if (allocated(local_error)) then
            if (present(error)) then
                error = local_error
                return
            else
                write(output_unit,'(a)') local_error%message
                return
            end if
        end if

        self%mol%charge = charge

        allocate(self%defaultradii(self%mol%nat))
        allocate(self%charges(self%mol%nat))
        allocate(self%scaledradii(self%mol%nat))
        select case(radtype)
        case('cpcm')
            do i=1,self%mol%nat
                self%defaultradii(i)=getVanDerWaalsRadCPCM(self%mol%num(self%mol%id(i)))
            end do
        case('cosmo')
            do i=1,self%mol%nat
                self%defaultradii(i)=getVanDerWaalsRadCosmo(self%mol%num(self%mol%id(i)))
            end do
        case default
            call fatal_error(error,'Unknown radii type: '//trim(radtype))
            return
        end select
        self%radtype = radtype
        self%scaledradii = 0.0_wp
        self%charges = 0.0_wp
        call self%charge(qmodel, error)
    end subroutine draco_init

    subroutine draco_charge(self, model, error)
        use draco_charges, only: ceh, eeq
        class(TDraco), intent(inout) :: self
        character(len=*), intent(in) :: model
        type(error_type), allocatable, intent(inout), optional :: error

        select case(model)
        case('ceh')
            call ceh(self%mol,self%charges, error)
        case ('eeq')
            call eeq(self%mol,self%charges)
        case default
            call fatal_error(error,'Unknown charge model: '//trim(model))
        end select

    end subroutine draco_charge
   
    subroutine draco_load_parameter(self,solvent)
      use draco_data
      !> Calculation environment
      class(TDraco), intent(inout) :: self
      !> Solvent
      character(len=*), intent(in) :: solvent

      select case (self%radtype)
         case default
            !call env%error('This is a Bug, please report with number #1222')

         case('cosmo')
            !call env%error('COSMO is not implemeneted yet')

         case('cpcm')
            self%o_shift = eeq_to_radii_scaling_alpha_cpcm
            select case (solvent)
               case default
                  self%prefac = eeq_to_radii_prefac_other_cpcm
                  self%expo = eeq_to_radii_expo_other_cpcm
                  !if (get_eps(trim(solvent)) < 5.0_wp) then
                  !   call env%warning &
                  !   & ("The current parameterization is only tested for polar solvents.")
                  !end if
               case('water')
                  self%prefac = eeq_to_radii_prefac_water_cpcm
                  self%expo = eeq_to_radii_expo_water_cpcm
            end select

         case('smd')
            !Use the scaling on SMD radii
            self%o_shift = eeq_to_radii_scaling_alpha_smd
            select case (solvent)
               case default
                  self%prefac = eeq_to_radii_prefac_other_smd
                  self%expo = eeq_to_radii_expo_other_smd
                  !if (get_eps(trim(solvent)) < 5.0_wp) then
                  !   call env%warning &
                  !   & ("The current parameterization is only tested for polar solvents.")
                  !end if
               case('water')
                  self%prefac = eeq_to_radii_prefac_water_smd
                  self%expo = eeq_to_radii_expo_water_smd
            end select
        end select

   end subroutine draco_load_parameter

   subroutine draco_read_param(self,file, error)
        use mctc_env, only: error_type, fatal_error
        use draco_read, only: rdparam_solvscale
        class(TDraco), intent(inout) :: self
        character(len=*), intent(in) :: file
        type(error_type), allocatable :: error

        call rdparam_solvscale(file, self%prefac, self%expo, self%o_shift, error)
    
    end subroutine draco_read_param

    subroutine draco_scale(self,solvent,atoms_to_change_radii)
        use draco_calc, only: calc_radii
        class(TDraco), intent(inout) :: self
        character(len=*), intent(in) :: solvent
        integer, dimension(:), intent(in) :: atoms_to_change_radii

        call calc_radii(self%mol, self%charges, self%radtype, solvent, self%prefac, self%expo, self%o_shift,&
        & self%defaultradii, self%scaledradii, atoms_to_change_radii)

    end subroutine draco_scale



end module draco_type
    