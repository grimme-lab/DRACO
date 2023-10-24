module draco_calc
    use mctc_io, only: structure_type
    use mctc_env, only: wp
    use draco_data, only: get_alpha, get_beta, get_eps, aatoau
    implicit none

contains

   subroutine calc_radii(mol, q, radtype, solvent, prefac, expo, o_shift, &
                  & radii_in,cn,k1,radii_out,atoms_to_change_radii)
    use iso_fortran_env, only: output_unit
      !> Molecular structure data
      type(structure_type), intent(inout) :: mol
      !> Charge model charges
      real(wp),intent(in) :: q(mol%nat)

      !> Parameter
      real(wp), intent(in) :: prefac(:), expo(:), o_shift(:), k1(:)

      !> Coordination number
      real(wp), intent(in) :: cn(:)

      !> Solvent
      character(len=*), intent(in) :: solvent
      !> Radii type
      character(len=*), intent(in) :: radtype
      !> Radii to be scaled
      integer, intent(in) :: atoms_to_change_radii(:)
      real(wp), intent(in) :: radii_in(mol%nat)
      real(wp), intent(out) :: radii_out(mol%nat)
      real(wp) :: asym, damping, x3, x2, x1, a, b, c, d, k
      real(wp) :: eps, alpha_beta_scaling_h, alpha_beta_scaling_o
      real(wp) :: alpha, beta

      integer :: i

      character(len=*), parameter :: source = "calc_radii"

      alpha = get_alpha(solvent)
      beta = get_beta(solvent)
      eps = get_eps(trim(solvent))

      write(output_unit,'(2x,"Dielectric constant used:", 2x, F5.2)') eps
      write(output_unit,'(2x,"Alpha used:", 2x, F5.2)') alpha
      write(output_unit,'(2x,"Beta used:", 2x, F5.2)') beta
      write(output_unit,*) 
      !if (eps == 0.0_wp) call env%error('Solvent '//trim(solvent)//' not recognized')

      do i=1, mol%nat
         if(any(atoms_to_change_radii == mol%num(mol%id(i)))) then
            a = prefac(mol%num(mol%id(i)))
            b = expo(mol%num(mol%id(i)))
            k = k1(mol%num(mol%id(i)))
            radii_out(i) = erf(a*((q(i)+k*q(i)*cn(i))-b)) + 1
         else
            radii_out(i) = radii_in(i)
         end if
      enddo

      select case (radtype)
         case default
            !call env%error('This is a Bug, please report with number #1221')

         case('cosmo')
            !Use the scaling on COSMO-RS radii
            !write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            !write(*,*) 'Radius = f_sclae * vdwradcpcm'
            !write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
            do i=1, mol%nat
            if(any(atoms_to_change_radii == mol%num(mol%id(i)))) then
                  radii_out(i) = radii_out(i) * (radii_in(i)/aatoau) 
            end if
            end do

         case('bondi')
            !Use the scaling on bondi radii
            do i=1, mol%nat
               if(any(atoms_to_change_radii == mol%num(mol%id(i)))) then
                  radii_out(i) = radii_out(i) * (radii_in(i)/aatoau)
               end if
            end do

         case('cpcm')
            !Use the scaling on cpcm radii
            do i=1, mol%nat
               if(any(atoms_to_change_radii == mol%num(mol%id(i)))) then
                  !> Change only radii of Oxygene (H-bond acceptor)
                  if (mol%num(mol%id(i)) == 8 .AND. get_alpha(solvent) < 0.43) then
                    radii_out(i) = radii_out(i) + (o_shift(mol%num(mol%id(i)))*(0.43-get_alpha(solvent)))
                  end if  
!                  radii(i) = radii(i) + (vanDerWaalsRadCPCM(mol%num(mol%id(i))/aatoau)
                  radii_out(i) = radii_out(i) * (radii_in(i)/aatoau)
                  !write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                  !write(*,*) 'Radius = f_sclae * vdwradcpcm'
                  !write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
               end if
            end do

         case('smd')
            !Use the scaling on SMD radii
            do i=1, mol%nat
               if(any(atoms_to_change_radii == mol%num(mol%id(i)))) then
                  !> Change only radii of Oxygene (H-bond acceptor)
                  if (mol%num(mol%id(i)) == 8 .AND. get_alpha(solvent) < 0.43) then
                    radii_out(i) = radii_out(i) + (o_shift(mol%num(mol%id(i)))*(0.43-get_alpha(solvent)))
                  end if
                  !> Change only for H (H-bond donor), except water
!                  if (mol%num(mol%id(i) == 1 .AND. (solvent /= 'water')) then
!                     radii(i) = (get_beta(solvent)/0.88) * radii(i)
!                  end if
                  !write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                  !write(*,*) 'Radius = f_sclae * vdw_smd'
                  !write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
                  !write(*,*) 'O SHIFT'
                  radii_out(i) = radii_out(i) * (radii_in(i)/aatoau)
               end if
            end do
        end select

   end subroutine calc_radii

end module draco_calc