program dragons_den
   use draco, only: TDraco, write_charges, write_cn
   use fancy, only: header, generic_header
   use iso_fortran_env, only: output_unit, input_unit
   use mctc_env, only: wp, error_type
   use mctc_io, only: to_symbol
   use draco_data, only: aatoau
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
      logical :: write_all = .false.
      logical :: wrcharges = .false.
      logical :: damp_small_rad = .true.
   end type TConf

   type(TDraco) :: dragon
   type(TConf) :: config
   type(error_type), allocatable :: error
   integer, parameter :: scalable_atoms(8) = (/1, 6, 7, 8, 9, 16, 17, 35/)

   character(len=:), allocatable :: warnings

   integer :: i

   if (file_exists('.CHRG')) then
      open (unit=input_unit, file='.CHRG', status='old', action='read')
      read (input_unit, *) config%charge
      close (input_unit)
   end if

   call get_arguments(config, error)
   call header(output_unit, config%verbose)
   Call check_terminate(error)
   if (config%verbose .ge. 0) then
      call print_config(config)
   end if
   call dragon%init(config%input, config%charge, config%qmodel,&
          & config%radtype, config%qc_input, config%write_all, config%damp_small_rad, error)
   call check_terminate(error)

   if (file_exists('.solvscale.param')) then
      if (config%verbose .ge. 0) write (output_unit, '(3x,a)') 'Loading solvent scaling parameters from .solvscale.param'
      call dragon%readParam('.solvscale.param', error)
   else
      call dragon%loadParam(config%solvent)
   end if
   call check_terminate(error)
   call dragon%calc(config%solvent, scalable_atoms)
   ! Print radii
   if (config%verbose .ge. 0) then
      call generic_header(output_unit, "Structure Information", 49, 10)
      write (output_unit, '(a)')
      write (output_unit, '(13x,a,t26,a,t43,a)') 'Identifier', 'Partial Charge', 'Radii (unscaled)'
      write (output_unit, '(a)')
      do i = 1, dragon%mol%nat
         write (output_unit, '(17x,a,i0, t30, f5.2,t43,f5.2,3x,a,g0.3,a )') &
         & trim(dragon%element(i)), i, dragon%charges(i), dragon%scaledradii(i), '('&
         & , dragon%defaultradii(i)/aatoau, ')'
      end do
      write (output_unit, '(a)')
   end if

   if (allocated(config%qc_interface)) then
      call dragon%write(config%qc_interface, scalable_atoms, error)
      call check_terminate(error)
   end if

   if (config%wrcharges) then
      if (config%verbose .ge. 0) then
         write (output_unit, '(3x,a)') '[INFO] Writing charges to "draco_charges" &
         & and coordination numbers to "draco_cn"', &
         ''
      end if
      call write_charges(dragon%charges)
      call write_cn(dragon%cn)
   end if

   call terminate(0)
contains

   subroutine get_arguments(config, error)
      use mctc_env, only: get_argument, fatal_error
      use iso_fortran_env, only: output_unit
      implicit none
      type(TConf), intent(inout) :: config
      type(error_type), allocatable, intent(out) :: error

      character(len=:), allocatable :: arg
      integer :: iarg, narg

      iarg = 0
      narg = command_argument_count()
      do while (iarg < narg)
         iarg = iarg + 1
         call get_argument(iarg, arg)
         select case (arg)
         case default
            if (index(arg, '-') == 1) then
               call fatal_error(error, "Unknown option: "//trim(arg))
               return
            else if (.not. allocated(config%input)) then
               call move_alloc(arg, config%input)
               cycle
            end if
            call fatal_error(error, 'Only one input file can be specified (got "'//trim(arg)//&
            &'" and "'//trim(config%input)//'")')
         case ('--prog', '--interface')
            iarg = iarg + 1
            call get_argument(iarg, arg)
            if (.not. allocated(config%qc_interface)) then
               call move_alloc(arg, config%qc_interface)
               if (config%qc_interface == 'orca') then
                  iarg = iarg + 1
                  call get_argument(iarg, arg)
                  call move_alloc(arg, config%qc_input)
               end if
               cycle
            end if
            call fatal_error(error, "Only one program can be specified")
         case ('--chargemodel', '--qmodel', '--chrgmodel')
            iarg = iarg + 1
            call get_argument(iarg, arg)
            if (.not. allocated(config%qmodel)) then
               call move_alloc(arg, config%qmodel)
               cycle
            end if
            call fatal_error(error, "Only one program can be specified")
         case ('--rad', '--radii')
            iarg = iarg + 1
            call get_argument(iarg, arg)
            if (.not. allocated(config%radtype)) then
               call move_alloc(arg, config%radtype)
               cycle
            end if
            call fatal_error(error, "Only one default radii set can be specified")
         case ('--verbose', '-v')
            config%verbose = 1
         case ('--veryverbose', '-vv')
            config%verbose = 2
         case ('--charge', '-c', '--chrg')
            iarg = iarg + 1
            call get_argument(iarg, arg)
            read (arg, *) config%charge
         case ('--solvent')
            iarg = iarg + 1
            call get_argument(iarg, arg)
            if (.not. allocated(config%solvent)) then
               call move_alloc(arg, config%solvent)
               cycle
            end if
            call fatal_error(error, "Only one solvent can be specified")
         case ('--writeall')
            config%write_all = .true.
         case ('--writecharges', '--wrcharges')
            config%wrcharges = .true.
         case ('--nodamp')
            config%damp_small_rad = .false.
         case ('--version', '-V')
            call header(output_unit, 1)
            call exit(0)
         case ('--help', '-h')
            call help(output_unit)
            call exit(0)
         case ('--silent', '--quiet')
            config%verbose = -1
         end select
      end do

      if (.not. allocated(config%input)) then
         call fatal_error(error, "No input file specified")
         return
      end if

      if (.not. allocated(config%solvent)) then
         call fatal_error(error, "No solvent specified, use --solvent <solvent name>.")
         return
      end if
      
      if (.not. allocated(config%radtype)) then
         call fatal_error(error, "No radii type specified, use --radii [cpcm, smd, cosmo].")
         return
      end if

      call set_defaults(config)

   end subroutine get_arguments

   subroutine set_defaults(config)
      implicit none
      type(TConf), intent(inout) :: config

      if (.not. allocated(config%qmodel)) then
         config%qmodel = "ceh"
      end if

   end subroutine set_defaults

   subroutine check_terminate(err)
      implicit none
      type(error_type), intent(inout), allocatable :: err
      if (allocated(err)) then
         select case (err%stat)
         case (0)
            write (output_unit, '(a)') "[WARNING] "//trim(err%message)
            if (.not. allocated(warnings)) then
               warnings = "[WARNING] "//trim(err%message)
            else
               warnings = trim(warnings)//new_line(trim(err%message))
            end if
            deallocate (err)
         case (1)
            write (output_unit, '(3x, a)') "[ERROR] "//error%message
            write (output_unit, '(a)')
            call terminate(err%stat)
         end select
      end if
   end subroutine check_terminate

   subroutine help(unit)
      integer, intent(in) :: unit

      call header(output_unit, 1)
      write (unit, '(a)') ""
      write (unit, '(2x,a)') &
         "Usage: draco [options] <inputfile> --solvent <solvent name> --rad <solvation model> [options]", &
         "Calculates dynamically scaled radii based on partial charges of a compound.", &
         ""

      write (unit, '(2x,a)') &
         "Supported geometry input formats are:", &
         "", &
         "- Xmol/xyz files (xyz, log)", &
         "- Turbomole's coord, riper's periodic coord (tmol, coord)", &
         "- DFTB+ genFormat geometry inputs as cluster, supercell or fractional (gen)", &
         "- VASP's POSCAR/CONTCAR input files (vasp, poscar, contcar)", &
         "- Protein Database files, only single files (pdb)", &
         "- Connection table files, molfile (mol) and structure data format (sdf)", &
         "- Gaussian's external program input (ein)", &
         ""

      write (unit, '(2x, a, t25, a)') &
         "Possible options are:", &
         "", &
         "--solvent", "Specify the solvent used for solvent properties and parametrization.", &
         "--prog, --interface", "Specify a QC program for which the input files should be modified. (orca, turbomole)", &
         "", "[HINT] For ORCA, the input file needs to be specified (e.q. --prog orca orca.inp)", &
         "--charge, --chrg", "Manually set the charge of the compound.", &
         "--chrgmodel, --qmodel", "Define charge model used (eeq, ceh, custom).", &
         "", "For custom, a file with partial charges must be given (see --wrcharges).", &
         "", "Parameters for custom are optimized for Hirshfeld charges at B97M-V/def2-TZVPPD.", &
         "--rad", "Sets the default radii to be scaled. (cpcm, cosmo, smd)", &
         "--writeall", "Writes all atomic radii including also not scaled ones.", &
         "--writecharges", "Writes the partial charges to a file called 'draco_charges'.", &
         "", "Also writes the coordination numbers to a file called 'draco_cn'.", &
         "--nodamp", "Turn off damping for small radii that are not tested.", &
         "", "Should only be set be used if gradient is required e.g. geometry optimizations.", &
         "--version", "Show version information.", &
         "--verbose", "Show more information.", &
         "--silent", "Supress the printout.", &
         "--help", "Show this help message."
      write (unit, '(a)')

   end subroutine help

   function file_exists(filename)
      logical :: file_exists
      character(len=*), intent(in) :: filename

      inquire (file=filename, exist=file_exists)

   end function file_exists

   subroutine terminate(stat)
      integer, intent(in) :: stat
      if (allocated(warnings)) then
         write (output_unit, '(3x,a)') warnings
         write (output_unit, '(a)')
      end if
      call exit(stat)
   end subroutine terminate

   subroutine print_config(conf)
      type(TConf), intent(in) :: conf

      integer :: l
      character(len=:), allocatable :: cmd

      call generic_header(output_unit, "Configuration", 49, 10)
      call get_command(length=l)
      allocate (character(l) :: cmd)
      call get_command(cmd)

      write (output_unit, '(23x,a,t43,a)') "", "", &
         "program call: ", trim(cmd), &
         "Input file: ", trim(conf%input), &
         "Solvent: ", trim(conf%solvent), &
         "Charge model: ", trim(conf%qmodel), &
         "Radii: ", trim(conf%radtype)
      if (allocated(conf%qc_interface)) then
         write (output_unit, '(23x,a,t43,a)') "QC interface: ", trim(conf%qc_interface)
         if (allocated(conf%qc_input)) then
            write (output_unit, '(23x,a,t43,a)') "QC input file: ", trim(conf%qc_input)
         end if
      end if
      write (output_unit, '(23x,a,t43,i0)') "Charge: ", conf%charge
      write (output_unit, '(a)')

   end subroutine print_config

end program dragons_den
