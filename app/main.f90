program dragons_den
    use draco, only: TDraco
    use iso_fortran_env, only: output_unit, input_unit
    use mctc_env, only: wp, error_type
    implicit none
    type TConf
        character(len=:), allocatable :: input
        character(len=:), allocatable :: qc_interface
        character(len=:), allocatable :: radtype
        character(len=:), allocatable :: qmodel
        character(len=:), allocatable :: solvent
        character(len=:), allocatable :: qc_input
        integer :: charge = 0
        integer :: verbose = 0
    end type TConf

    type(TDraco) :: dragon
    type(TConf) :: config
    type(error_type), allocatable :: error
    integer, parameter :: scalable_atoms(8)=(/1,6,7,8,9,16,17,35/)

    integer :: i

    if (file_exists('.CHRG')) then
        open(unit=input_unit, file='.CHRG', status='old', action='read')
        read(input_unit,*) config%charge
        close(input_unit)
    end if

    call get_arguments(config, error)
    Call check_terminate(error)

    call dragon%init(config%input, config%charge, config%qmodel, config%radtype, config%qc_input, error)
    call check_terminate(error)

    if (file_exists('.solvscale.param')) then
        write(*,*) 'Loading solvent scaling parameters from .solvscale.param'
        call dragon%readParam('.solvscale.param',error)
    else
        call dragon%loadParam(config%solvent)
    end if
    call check_terminate(error)
    call dragon%calc(config%solvent,scalable_atoms)
      ! Print radii
      write(output_unit,*) '  Number  Partial Charge  Radii'
      do i = 1, dragon%mol%nat
         write(*,'(5x,i0,9x,f5.2, 7x, f5.2 )') i, dragon%charges(i), dragon%scaledradii(i)
      enddo

    if (allocated(config%qc_interface)) then
        call dragon%write(config%qc_interface, error)
        call check_terminate(error)
    end if
contains

    subroutine get_arguments(config, error)
        use mctc_env, only: get_argument, fatal_error
        use iso_fortran_env, only: output_unit
        implicit none
        type(TConf), intent(out) :: config
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: arg
        integer :: iarg, narg

        iarg = 0
        narg = command_argument_count()
        do while (iarg < narg)
            iarg=iarg+1
            call get_argument(iarg,arg)
            select case(arg)
            case default
                if (index(arg,'-')==1) then
                    call fatal_error(error, "Unknown option: "//trim(arg))
                    return
                else if (.not.allocated(config%input)) then
                    call move_alloc(arg,config%input)
                    cycle
                end if
                call fatal_error(error, 'Only one input file can be specified (got "'//trim(arg)//&
                &'" and "'//trim(config%input)//'")')
            case ('--prog','--interface')
                iarg=iarg+1
                call get_argument(iarg,arg)
                if (.not.allocated(config%qc_interface)) then
                    call move_alloc(arg,config%qc_interface)
                    if (config%qc_interface == 'orca') then
                        iarg=iarg+1
                        call get_argument(iarg,arg)
                        call move_alloc(arg,config%qc_input)
                    end if
                    cycle
                end if
                call fatal_error(error, "Only one program can be specified")
            case ('--model','--chargemodel','--qmodel')
                iarg=iarg+1
                call get_argument(iarg,arg)
                if (.not.allocated(config%qmodel)) then
                    call move_alloc(arg,config%qmodel)
                    cycle
                end if
                call fatal_error(error, "Only one program can be specified")
            case ('--rad','--radii')
                iarg=iarg+1
                call get_argument(iarg,arg)
                if (.not.allocated(config%radtype)) then
                    call move_alloc(arg,config%radtype)
                    cycle
                end if
                call fatal_error(error, "Only one default radii set can be specified")
            case ('--verbose','-v')
                config%verbose = 1
            case ('--charge','-c')
                iarg=iarg+1
                call get_argument(iarg,arg)
                read(arg,*) config%charge
            case ('--solvent','-s')
                iarg=iarg+1
                call get_argument(iarg,arg)
                if (.not.allocated(config%solvent)) then
                    call move_alloc(arg,config%solvent)
                    cycle
                end if
                call fatal_error(error, "Only one solvent can be specified")
            case ('--help','-h')
                call help(output_unit)
                call exit(0)
            end select
        end do

        if (.not.allocated(config%input)) then
            call fatal_error(error, "No input file specified")
            return
        end if

        if (.not.allocated(config%solvent)) then
            call fatal_error(error, "No solvent specified")
            return
        end if

        call set_defaults(config)

    end subroutine get_arguments
    
    subroutine set_defaults(config)
        implicit none
        type(TConf), intent(inout) :: config

        if (.not.allocated(config%qmodel)) then
            config%qmodel="ceh"
        end if
        if (.not.allocated(config%radtype)) then
            config%radtype="cpcm"
        end if

    end subroutine set_defaults

    subroutine check_terminate(err)
        implicit none
        type(error_type), intent(in), allocatable :: err
        if (allocated(err)) then
            write(output_unit,'(a)') error%message
            call exit(1)
        end if

    end subroutine check_terminate

subroutine help(unit)
   integer, intent(in) :: unit

   write(unit,'(a)') ""
   write(unit, '(2x,a)') &
      "Usage: draco [options] <inputfile> [options]", &
      "Calculates dynamically scaled radii based on partial charges of a compound.", &
      ""

   write(unit, '(2x,a)') &
      "Supported geometry input formats are:",&
      "",&
      "- Xmol/xyz files (xyz, log)",&
      "- Turbomole's coord, riper's periodic coord (tmol, coord)",&
      "- DFTB+ genFormat geometry inputs as cluster, supercell or fractional (gen)",&
      "- VASP's POSCAR/CONTCAR input files (vasp, poscar, contcar)",&
      "- Protein Database files, only single files (pdb)",&
      "- Connection table files, molfile (mol) and structure data format (sdf)",&
      "- Gaussian's external program input (ein)", &
      ""

   write(unit, '(2x, a, t25, a)') &
      "Possible options are:", &
      "", &
      "--solvent", "Specify the solvent used for solvent properties and parametrization.", &
      "--prog, --interface", "Specify the QC program for which the input files should be written. (ORCA, TURBOMOLE)", &
      "", "[HINT] For ORCA, the .inp file needs to be given (e.q. --prog ORCA orca.inp)", &
      "--charge", "Manually set the charge of the compound.", &
      "--rad", "Sets the default radii to be scaled. (cpcm, cosmo, smd)", &
      "--help", "Show this help message."
   write(unit, '(a)')
   
end subroutine help

    function file_exists(filename)
        logical :: file_exists
        character(len=*), intent(in) :: filename

        inquire(file=filename, exist=file_exists)

    end function file_exists
    

end program dragons_den
