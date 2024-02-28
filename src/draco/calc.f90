module draco_calc
   use mctc_io, only: structure_type
   use mctc_env, only: wp
   use draco_data, only: get_alpha, get_beta, get_eps, aatoau, min_rad
   implicit none

contains

   subroutine calc_radii(mol, q, radtype, solvent, prefac, expo, o_shift, &
                  & radii_in, cn, k1, radii_out, atoms_to_change_radii, damp_small_rad,&
                  & dqdr, dcndr, drdr)
      use iso_fortran_env, only: output_unit
      !> Molecular structure data
      type(structure_type), intent(inout) :: mol
      !> Charge model charges
      real(wp), intent(in) :: q(mol%nat)

      !> Parameter
      real(wp), intent(in) :: prefac(:), expo(:), o_shift(:), k1(:)

      !> Coordination number
      real(wp), intent(in) :: cn(:)

      !> Derivatives
      real(wp), intent(in), optional, contiguous :: dqdr(:, :, :), dcndr(:, :, :)
      real(wp), intent(out), allocatable, optional :: drdr(:, :, :)

      !> Solvent
      character(len=*), intent(in) :: solvent
      !> Radii type
      character(len=*), intent(in) :: radtype
      !> Damping if radii too small?
      logical, intent(in) :: damp_small_rad
      !> Radii to be scaled
      integer, intent(in) :: atoms_to_change_radii(:)
      real(wp), intent(in) :: radii_in(mol%nat)
      real(wp), intent(out) :: radii_out(mol%nat)
      real(wp) :: asym, x3, x2, x1, a, b, c, d, k
      real(wp) :: eps, alpha_beta_scaling_h, alpha_beta_scaling_o
      real(wp) :: alpha, beta
      real(wp) :: outer

      real(wp), parameter :: pi = 4.0_wp*atan(1.0_wp)

      integer :: i

      logical :: grad

      character(len=*), parameter :: source = "calc_radii"

      alpha = get_alpha(solvent)
      beta = get_beta(solvent)
      eps = get_eps(trim(solvent))

      grad = present(dqdr) .and. present(dcndr) .and. present(drdr)
      if (grad) allocate (drdr(3, mol%nat, mol%nat))

      do i = 1, mol%nat
         if (any(atoms_to_change_radii == mol%num(mol%id(i)))) then
            a = prefac(mol%num(mol%id(i)))
            b = expo(mol%num(mol%id(i)))
            k = k1(mol%num(mol%id(i)))
            radii_out(i) = erf(a*(q(i) + k*q(i)*cn(i) - b)) + 1
            if (damp_small_rad .and. (radii_out(i) < min_rad(radtype))) &
                    & radii_out(i) = min_rad(radtype) !Set value to minimal radii tested
            if (grad) then
               outer = 2*exp(-a**2*(q(i) + k*q(i)*cn(i) - b)**2)/sqrt(pi)
               drdr(:, :, i) = a*(dqdr(:, :, i) + k*dqdr(:, :, i)*cn(i) + q(i)*k*dcndr(:, :, i))*outer
            end if
         else
            radii_out(i) = radii_in(i)/aatoau
         end if
      end do

      select case (radtype)
      case ('bondi')
         !Use the scaling on bondi radii
         do i = 1, mol%nat
            if (any(atoms_to_change_radii == mol%num(mol%id(i)))) then
               radii_out(i) = radii_out(i)*(radii_in(i)/aatoau)
               if (grad) then
                  drdr(:, :, i) = drdr(:, :, i)*(radii_in(i)/aatoau)
               end if
            end if
         end do

      case ('cpcm', 'cosmo')
         !Use the scaling on cpcm radii
         do i = 1, mol%nat
            if (any(atoms_to_change_radii == mol%num(mol%id(i)))) then
               !> Change only radii of Oxygene (H-bond acceptor)
               if (mol%num(mol%id(i)) == 8 .AND. get_alpha(solvent) < 0.43) then
                  radii_out(i) = radii_out(i) + (o_shift(mol%num(mol%id(i)))*(0.43 - get_alpha(solvent)))
               end if
               radii_out(i) = radii_out(i)*(radii_in(i)/aatoau)
               if (grad) then
                  drdr(:, :, i) = drdr(:, :, i)*(radii_in(i)/aatoau)
               end if
            end if
         end do

      case ('smd')
         !Use the scaling on SMD radii
         do i = 1, mol%nat
            if (any(atoms_to_change_radii == mol%num(mol%id(i)))) then
               !> Change only radii of Oxygene (H-bond acceptor)
               if (mol%num(mol%id(i)) == 8 .AND. get_alpha(solvent) < 0.43) then
                  radii_out(i) = radii_out(i) + (o_shift(mol%num(mol%id(i)))*(0.43 - get_alpha(solvent)))
               end if
               radii_out(i) = radii_out(i)*(radii_in(i)/aatoau)
               if (grad) then
                  drdr(:, :, i) = drdr(:, :, i)*(radii_in(i)/aatoau)
               end if
            end if
         end do
      end select

   end subroutine calc_radii

end module draco_calc
