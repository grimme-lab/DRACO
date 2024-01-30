module draco_type
    use mctc_io, only: structure_type, get_filetype, read_structure, to_symbol
    use mctc_env, only: wp, error_type, fatal_error
    use draco_data, only: getVanDerWaalsRadCosmo, getVanDerWaalsRadCPCM, getVanDerWaalsRadSMD
    implicit none
    private

    public :: TDraco

    type TDraco
        type(structure_type) :: mol
        real(wp), dimension(:), allocatable :: defaultradii
        real(wp), dimension(:), allocatable :: charges
        real(wp), dimension(:), allocatable :: scaledradii

        !> CEH coordination number
        real(wp), dimension(:), allocatable :: cn

        !> Parametrisation
        real(wp), dimension(94) :: prefac, expo, o_shift, k1

        !> Radtype
        character(len=:), allocatable :: radtype

        !> QC Input
        character(len=:), allocatable :: qc_input

        !> Which charge model?
        character(len=:), allocatable :: qmodel

        !> Write all radii?
        logical :: write_all

        !> Gradients of the charges w.r.t. cartesian coordinates
        real(wp), allocatable :: dqdr(:,:,:)
        !> Gradients of the charges w.r.t. strain deformations
        real(wp), allocatable :: dqdL(:,:,:)
        !> Gradients of the coordination numbers w.r.t. cartesian coordinates
        real(wp), allocatable :: dcndr(:,:,:)
        !> Gradients of the coordination numbers w.r.t. strain deformations
        real(wp), allocatable :: dcndL(:,:,:)
        !> Gradients of the radii w.r.t. coordination numbers
        real(wp), allocatable :: drdr(:,:,:)

    contains
        procedure :: init => draco_init
        procedure :: loadParam => draco_load_parameter
        procedure :: readParam => draco_read_param
        procedure :: charge => draco_charge
        procedure :: calc => draco_scale
        procedure :: write => draco_write_qc_input
        procedure :: element
    end type TDraco

contains

    subroutine draco_init(self, file, charge, qmodel, radtype, qc_input, write_all, error)
        use iso_fortran_env, only: output_unit
        use draco_read, only: read_charges, read_cn
        use draco_charges, only: get_cn
        class(TDraco), intent(inout) :: self
        character(len=*), intent(in) :: file
        !> Charge of the molecule
        integer, intent(in) :: charge
        !> Charge model
        character(len=*), intent(in) :: qmodel
        !> Radii type
        character(len=*), intent(in) :: radtype
        !> QC Input (only necessary for some programs)
        character(len=*), intent(in), optional :: qc_input
        !> Write all radii?
        logical, intent(in) :: write_all
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
        allocate(self%cn(self%mol%nat))
        select case(radtype)
        case('cpcm')
            do i=1,self%mol%nat
                self%defaultradii(i)=getVanDerWaalsRadCPCM(self%mol%num(self%mol%id(i)))
            end do
        case('cosmo')
            do i=1,self%mol%nat
                self%defaultradii(i)=getVanDerWaalsRadCosmo(self%mol%num(self%mol%id(i)))
            end do
        case('smd')
            do i=1,self%mol%nat
                self%defaultradii(i)=getVanDerWaalsRadSMD(self%mol%num(self%mol%id(i)))
            end do
        case default
            call fatal_error(error,'Unknown radii type: '//trim(radtype))
            return
        end select
        if (present(qc_input)) then
            self%qc_input = qc_input
        end if
        self%radtype = radtype
        self%scaledradii = 0.0_wp
        self%charges = 0.0_wp
        self%qmodel = qmodel
        if (qmodel == "custom") then
            call read_charges('draco_charges',self%charges, local_error)

            if (allocated(local_error)) then
                if (present(error)) then
                    error = local_error
                    return
                else
                    write(output_unit,'(a)') local_error%message
                    return
                end if
            end if

            if (abs(sum(self%charges)-self%mol%charge) > 0.1_wp) then ! High tolerance to allow deviation for printouts
                call fatal_error(local_error,'The sum of the custom charges is not equal to the total charge of the molecule')
                if (present(error)) then
                    error = local_error
                else
                    write(output_unit,'(a)') local_error%message
                end if
                return
            end if

            call read_cn('draco_cn',self%cn, local_error)
            
            if (allocated(local_error)) then
                if (present(error)) then
                    error = local_error
                    return
                else
                    write(output_unit,'(a)') local_error%message
                    return
                end if
            end if

            if (.not. allocated(self%cn)) then
                allocate(self%cn(self%mol%nat))
                call get_cn(self%mol, self%cn, dcndr=self%dcndr,dcndL=self%dcndL)
                call fatal_error(local_error,'No coordination number file found, using default',0)
            end if

        else
            call get_cn(self%mol, self%cn,dcndr=self%dcndr,dcndL=self%dcndL)
            call self%charge(qmodel, local_error)
        end if
        if(write_all) self%write_all = .true.
        if (allocated(local_error)) then
           if (present(error)) then
              error = local_error
              return
           else
              write(output_unit,'(a)') local_error%message
              return
           end if
        end if
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
            call eeq(self%mol,self%charges,self%dqdr,self%dqdL)
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
            select case (self%qmodel)
               case default
                   error stop 'Something went wrong'
               case ('eeq')
                   select case (solvent)
                      case default
                         self%prefac = eeq_prefac_other_solvents_cosmo
                         self%expo = eeq_expo_other_solvents_cosmo
                         self%k1 = eeq_k_other_solvents_cosmo
                         self%o_shift = eeq_o_shift_other_solvents_cosmo
                      case('water')
                         self%prefac = eeq_prefac_water_cosmo
                         self%expo = eeq_expo_water_cosmo
                         self%k1 = eeq_k_water_cosmo
                         self%o_shift = 0.0_wp
                   end select
               case ('ceh')
                   select case (solvent)
                      case default
                         self%prefac = ceh_prefac_other_solvents_cosmo
                         self%expo = ceh_expo_other_solvents_cosmo
                         self%k1 = ceh_k_other_solvents_cosmo
                         self%o_shift = ceh_o_shift_other_solvents_cosmo
                      case('water')
                         self%prefac = ceh_prefac_water_cosmo
                         self%expo = ceh_expo_water_cosmo
                         self%k1 = ceh_k_water_cosmo
                         self%o_shift = 0.0_wp
                   end select
               case ('custom')
                   select case (solvent)
                      case default
                         self%prefac = custom_prefac_other_solvents_cosmo
                         self%expo = custom_expo_other_solvents_cosmo
                         self%k1 = custom_k_other_solvents_cosmo
                         self%o_shift = custom_o_shift_other_solvents_cosmo
                      case('water')
                         self%prefac = custom_prefac_water_cosmo
                         self%expo = custom_expo_water_cosmo
                         self%k1 = custom_k_water_cosmo
                         self%o_shift = 0.0_wp
                   end select
                end select

         case('cpcm')
            select case (self%qmodel)
               case default
                   error stop 'Something went wrong'
               case ('eeq')
                   select case (solvent)
                      case default
                         self%prefac = eeq_prefac_other_solvents_cpcm
                         self%expo = eeq_expo_other_solvents_cpcm
                         self%k1 = eeq_k_other_solvents_cpcm
                         self%o_shift = eeq_o_shift_other_solvents_cpcm
                      case('water')
                         self%prefac = eeq_prefac_water_cpcm
                         self%expo = eeq_expo_water_cpcm
                         self%k1 = eeq_k_water_cpcm
                         self%o_shift = 0.0_wp
                   end select
               case ('ceh')
                   select case (solvent)
                      case default
                         self%prefac = ceh_prefac_other_solvents_cpcm
                         self%expo = ceh_expo_other_solvents_cpcm
                         self%k1 = ceh_k_other_solvents_cpcm
                         self%o_shift = ceh_o_shift_other_solvents_cpcm
                      case('water')
                         self%prefac = ceh_prefac_water_cpcm
                         self%expo = ceh_expo_water_cpcm
                         self%k1 = ceh_k_water_cpcm
                         self%o_shift = 0.0_wp
                   end select
               case ('custom')
                   select case (solvent)
                      case default
                         self%prefac = custom_prefac_other_solvents_cpcm
                         self%expo = custom_expo_other_solvents_cpcm
                         self%k1 = custom_k_other_solvents_cpcm
                         self%o_shift = custom_o_shift_other_solvents_cpcm
                      case('water')
                         self%prefac = custom_prefac_water_cpcm
                         self%expo = custom_expo_water_cpcm
                         self%k1 = custom_k_water_cpcm
                         self%o_shift = 0.0_wp
                   end select
                end select


         case('smd')
            select case (self%qmodel)
               case default
                   error stop 'Something went wrong'
               case ('eeq')
                   select case (solvent)
                      case default
                         self%prefac = eeq_prefac_other_solvents_smd
                         self%expo = eeq_expo_other_solvents_smd
                         self%k1 = eeq_k_other_solvents_smd
                         self%o_shift = eeq_o_shift_other_solvents_smd
                      case('water')
                         self%prefac = eeq_prefac_water_smd
                         self%expo = eeq_expo_water_smd
                         self%k1 = eeq_k_water_smd
                         self%o_shift = 0.0_wp
                   end select
               case ('ceh')
                   select case (solvent)
                      case default
                         self%prefac = ceh_prefac_other_solvents_smd
                         self%expo = ceh_expo_other_solvents_smd
                         self%k1 = ceh_k_other_solvents_smd
                         self%o_shift = ceh_o_shift_other_solvents_smd
                      case('water')
                         self%prefac = ceh_prefac_water_smd
                         self%expo = ceh_expo_water_smd
                         self%k1 = ceh_k_water_smd
                         self%o_shift = 0.0_wp
                   end select
               case ('custom')
                   select case (solvent)
                      case default
                         self%prefac = custom_prefac_other_solvents_smd
                         self%expo = custom_expo_other_solvents_smd
                         self%k1 = custom_k_other_solvents_smd
                         self%o_shift = custom_o_shift_other_solvents_smd
                      case('water')
                         self%prefac = custom_prefac_water_smd
                         self%expo = custom_expo_water_smd
                         self%k1 = custom_k_water_smd
                         self%o_shift = 0.0_wp
                   end select
                end select
        end select

   end subroutine draco_load_parameter

   subroutine draco_read_param(self,file, error)
        use mctc_env, only: error_type, fatal_error
        use draco_read, only: rdparam_solvscale
        class(TDraco), intent(inout) :: self
        character(len=*), intent(in) :: file
        type(error_type), allocatable :: error

        call rdparam_solvscale(file, self%prefac, self%expo, self%o_shift, self%k1, error)
    
    end subroutine draco_read_param

    subroutine draco_scale(self,solvent,atoms_to_change_radii)
        use draco_calc, only: calc_radii
        class(TDraco), intent(inout) :: self
        character(len=*), intent(in) :: solvent
        integer, dimension(:), intent(in) :: atoms_to_change_radii

        if (allocated(self%dqdr)) then
            call calc_radii(self%mol, self%charges, self%radtype, solvent, self%prefac, self%expo, self%o_shift,&
            & self%defaultradii, self%cn, self%k1, self%scaledradii, atoms_to_change_radii, dqdr=self%dqdr, dcndr=self%dcndr,&
            & drdr=self%drdr)
        else
            call calc_radii(self%mol, self%charges, self%radtype, solvent, self%prefac, self%expo, self%o_shift,&
            & self%defaultradii, self%cn, self%k1, self%scaledradii, atoms_to_change_radii)
        end if

    end subroutine draco_scale

    subroutine draco_write_qc_input(self, program, atoms_to_change_radii, error)
        use draco_interface, only: write_radii
        
        class(TDraco), intent(inout) :: self
        character(len=*), intent(in) :: program
        integer, dimension(:), intent(in) :: atoms_to_change_radii

        type(error_type), allocatable, intent(inout), optional :: error

        select case(program)
        case('orca')
            if (allocated(self%qc_input)) then
                call write_radii(self%mol, self%scaledradii, self%qc_input, atoms_to_change_radii, self%write_all)
            else
                call fatal_error(error,'No QC input file specified for ORCA')
                return
            end if
        case('turbomole','tm')
            call write_radii(self%mol,self%scaledradii, atoms_to_change_radii, self%write_all)
        case default
            if (present(error)) then
                call fatal_error(error,'Unknown program: '//trim(program))
                return
            end if
        end select
    end subroutine draco_write_qc_input

    pure function element(self,id) result(res)
        class(TDraco), intent(in) :: self
        integer, intent(in) :: id
        character(len=2) :: res

        write(res,'(a)') to_symbol(self%mol%num(self%mol%id(id)))
        

    end function element


end module draco_type
    
