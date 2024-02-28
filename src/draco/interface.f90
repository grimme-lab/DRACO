module draco_interface
   use mctc_io, only: structure_type, toSymbol => to_symbol
   use mctc_env, only: wp
   implicit none
   private

   interface write_radii
      module procedure write_radii_to_control
      module procedure write_radii_to_orca_input
   end interface write_radii

   public :: write_radii

contains

   subroutine write_radii_to_control(mol, radii, atoms_to_change_radii, write_all)

      !> Molecular data
      type(structure_type), intent(in) :: mol
      !> Radii to write
      real(wp), intent(in) :: radii(mol%nat)
      integer, dimension(:), intent(in) :: atoms_to_change_radii
      logical, intent(in) :: write_all

      character(len=*), parameter :: source = "write_control"
      character(len=128) :: line
      logical :: cosmo, ex

      integer :: i, id, ich, ios

      inquire (file='control', exist=ex)
      !if(.not. ex) then
      !    call fatal_error(error,'There is no control file, nothing will be written')
      !    return
      ! endif
      open (newunit=id, file='control', status='old', action='read')

      !Scan for $cosmo and copy control
      cosmo = .false.
      open (newunit=ich, file='tmp_control', status='unknown', action='write')
      do
         read (id, '(a)', iostat=ios) line
         if (ios /= 0) exit
         if (trim(adjustl(line)) == '$end') exit
         !if(trim(adjustl(line)) == '$cosmo_atoms') &
         !& call env%error('$cosmo_atoms block already exists in control file')
         if (trim(adjustl(line)) == '$cosmo') cosmo = .true.
         write (ich, '(a)') trim(line)
      end do
      close (id)
      !if(.not.cosmo) call env%warning('No $cosmo block found in control')
      write (ich, *) ' $cosmo_atoms'
      write (ich, *) ' #radii in Angstrom units'
      do i = 1, mol%nat
         if (write_all) then !Write all radii
            write (ich, '(2x,a,1x,i0,5x,a)') toSymbol(mol%num(mol%id(i))), i, '\'
            write (ich, '(3x,a,F16.12)') 'radius=', radii(i)
         else if (any(atoms_to_change_radii == mol%num(mol%id(i)))) then !Write only adjusted radii
            write (ich, '(2x,a,1x,i0,5x,a)') toSymbol(mol%num(mol%id(i))), i, '\'
            write (ich, '(3x,a,F16.12)') 'radius=', radii(i)
         end if
      end do
      write (ich, *) '$end'
      close (ich)
      call rename('control', 'control.original', stat=ios)
      call rename('tmp_control', 'control', stat=ios)

   end subroutine write_radii_to_control

   subroutine write_radii_to_orca_input(mol, radii, orca_input, atoms_to_change_radii, write_all)

      !> Molecular data
      type(structure_type), intent(in) :: mol
      !> Radii to write
      real(wp), intent(in) :: radii(mol%nat)
      !> Name of the orca input file
      character(len=*), intent(in) :: orca_input
      integer, dimension(:), intent(in) :: atoms_to_change_radii
      logical, intent(in) :: write_all

      character(len=*), parameter :: source = "write_input"
      character(len=128) :: line
      logical :: cpcm, ex
      logical :: cpcm_block

      integer :: i, id, ich, ios

      inquire (file=orca_input, exist=ex)
      !if(.not. ex) then
      !   call env%warning('There is no input file, nothing will be written')
      !   return
      !endif
      open (newunit=id, file=orca_input, status='old', action='read')

      !Scan for $cosmo and copy control
      cpcm = .false.
      cpcm_block = .false.
      open (newunit=ich, file='tmp_input', status='unknown', action='write')
      do
         read (id, '(a)', iostat=ios) line
         if (ios /= 0) exit
         write (ich, '(a)') trim(line)
         if (trim(adjustl(line)) == '%cpcm' .OR. trim(adjustl(line)) == '%CPCM') then
            cpcm_block = .true.
            do i = 1, mol%nat
               if (write_all) then
                  write (ich, '(2x,a,i0,a,F16.12,a)') 'AtomRadii(', i - 1, ',', radii(i), ')'
               else if (any(atoms_to_change_radii == mol%num(mol%id(i)))) then !Write only adjusted radii
                  write (ich, '(2x,a,i0,a,F16.12,a)') 'AtomRadii(', i - 1, ',', radii(i), ')'
               end if
               !i-1 because Orca starts to count at 0
            end do
         end if
!         if(index(line,'cpcm') /= 0) cpcm=.true.
      end do
      close (id)
!      if(.not.cpcm .or. .not. cpcm_block) call env%warning&
!              &('No cpcm settings found in input')
      if (.not. cpcm_block) then
         write (ich, *)
         write (ich, *) '%cpcm'
         do i = 1, mol%nat
            if (write_all) then !Write all radii
               write (ich, '(2x,a,i0,a,F16.12,a)') 'AtomRadii(', i - 1, ',', radii(i), ')'
            else if (any(atoms_to_change_radii == mol%num(mol%id(i)))) then !Write only adjusted radii
               write (ich, '(2x,a,i0,a,F16.12,a)') 'AtomRadii(', i - 1, ',', radii(i), ')'
            end if
            !i-1 because Orca starts to count at 0
         end do
         write (ich, *) 'end'
      end if
      close (ich)
      call rename(orca_input, trim(orca_input)//'.original', stat=ios)
      call rename('tmp_input', orca_input, stat=ios)

   end subroutine write_radii_to_orca_input

   ! subroutine add_to_control(env,mol)
   !    !> Calculation environment
   !    type(TEnvironment), intent(inout) :: env
   !    !> Molecular structure data
   !    type(TMolecule), intent(in) :: mol
   !    character(len=*), parameter :: source = "rdparam_solvscale"
   !    character(len=:), allocatable :: line
   !    integer :: ie, at, err, id, i
   !    real(wp) :: radii(mol%n), ddum
   !    integer :: write_all_atoms(mol%n)

   !    call open_file(id,'.cosmo.radii','r')
   !    if(ie==-1) call env%error('No .cosmo.radii file found')
   !    write(env%unit,*) '  Radii read form file'

   !    do i=1, mol%n
   !       call getline(id,line,iostat=err)
   !       if (line == '') call env%error('Too less radii in .cosmo.radii')
   !       if(getValue(env,line,ddum)) radii(i) = ddum
   !       write(env%unit,'(a,1x,i0,1x,(a),F12.8)') &
   !               & 'Radius for atom', i, ':', radii(i)
   !       write_all_atoms(i) = i
   !    end do
   !    call close_file(id)

   !    select case (file_format)
   !       case default
   !          call env%error('This is a bug with the fileformat. Please report')
   !          return
   !       case (file_tm)
   !          call write_radii_to_control(env, mol, radii, write_all_atoms)
   !       case (file_orca)
   !          write(*,*) 'To be implemented'
   !    end select

   !    call env%show("Runtime exception occurred")
   !    call raise('F', 'Some non-fatal runtime exceptions were caught,'// &
   !       &           ' please check:')

   ! end subroutine add_to_control

   subroutine rename(src, tgt, stat)
      use, intrinsic :: iso_c_binding, only: c_null_char
      character(len=*), intent(in) :: src
      character(len=*), intent(in) :: tgt
      integer, intent(out) :: stat
      interface
         function sys_rename(src, tgt, lena, lenb) bind(c, name="rename") result(stat)
            use, intrinsic :: iso_c_binding, only: c_char, c_int
            integer(c_int), intent(in) :: lena, lenb
            character(kind=c_char), intent(in) :: src(lena)
            character(kind=c_char), intent(in) :: tgt(lenb)
            integer(c_int) :: stat
         end function sys_rename
      end interface
      stat = sys_rename(src//c_null_char, tgt//c_null_char, len(src), len(tgt))
   end subroutine rename

end module draco_interface
