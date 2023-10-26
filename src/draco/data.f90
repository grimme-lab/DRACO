module draco_data
    use mctc_env, only: wp
    use mctc_io, only: toNumber => to_number
    implicit none
    !private

    public
    !>  convert bohr (a.u.) to Ångström and back
    real(wp),parameter :: autoaa = 0.52917726_wp
    real(wp),parameter :: aatoau = 1.0_wp/autoaa

       !> Get van-der-Waals Rad for a species
    interface getVanDerWaalsRadCPCM
        module procedure :: getVanDerWaalsRadCPCMSymbol
        module procedure :: getVanDerWaalsRadCPCMNumber
    end interface getVanDerWaalsRadCPCM

   !> Get van-der-Waals Rad for a species
    interface getVanDerWaalsRadCosmo
        module procedure :: getVanDerWaalsRadCosmoSymbol
        module procedure :: getVanDerWaalsRadCosmoNumber
    end interface getVanDerWaalsRadCosmo

    interface getVanDerWaalsRadSMD
         module procedure :: getVanDerWaalsRadSMDSymbol
         module procedure :: getVanDerWaalsRadSMDNumber
    end interface getVanDerWaalsRadSMD

    !public :: getVanDerWaalsRadCosmo, getVanDerWaalsRadCPCM, aatoau, autoaa

!> Default value for unoptimized van-der-Waals radii
real(wp), parameter :: cpcmstub = 2.223_wp, cosmostub = 2.223_wp

!> CPCM optimized van-der-Waals radii
real(wp), parameter :: vanDerWaalsRadCPCM(94) = aatoau * [ &
    & 1.3200_wp, 1.6800_wp, 2.1840_wp, 1.8360_wp, &   ! h-be
    & 2.3040_wp, 2.0400_wp, 1.8600_wp, 1.8240_wp, &   ! B-O
    & 1.7640_wp, cpcmstub, cpcmstub, cpcmstub, &   ! F-Mg
    & 2.3040_wp, 2.5200_wp, 2.1600_wp, 2.1600_wp, &   ! Al-S
    & 2.1000_wp, cpcmstub, cpcmstub, cpcmstub, &   ! Cl-Ca
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Sc-Cr
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Mn-Ni
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Cu-Ge
    & cpcmstub, cpcmstub, 2.2200_wp, cpcmstub, &   ! As-Kr
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Rb-Zr
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Nb-Ru
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Rh-Cd
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! In-Te
    & 2.3760_wp,cpcmstub, cpcmstub, cpcmstub, &   ! I-Ba
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! La-Nd
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Pm-Gd
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Tb-Er
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Tm-Hf
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Ta-Os
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Ir-Hg
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Tl-Po
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! At-Ra
    & cpcmstub, cpcmstub, cpcmstub, cpcmstub, &   ! Ac-U
    & cpcmstub, cpcmstub]                           ! Np-Pu

    !> COSMO optimized van-der-Waals radii
   real(wp), parameter :: vanDerWaalsRadCosmo(94) = aatoau * [ &
   & 1.3000_wp, 1.6380_wp, 1.5700_wp, 1.0530_wp, &   ! h-be
   & 2.0480_wp, 2.0000_wp, 1.8300_wp, 1.7200_wp, &   ! B-O
   & 1.7200_wp, 1.8018_wp, 1.8000_wp, 1.6380_wp, &   ! F-Mg
   & 2.1530_wp, 2.2000_wp, 2.1060_wp, 2.1600_wp, &   ! Al-S
   & 2.0500_wp, 2.2000_wp, 2.2230_wp, cosmostub, &   ! Cl-Ca
   & cosmostub, 2.2930_wp, cosmostub, cosmostub, &   ! Sc-Cr
   & cosmostub, cosmostub, cosmostub, cosmostub, &   ! Mn-Ni
   & cosmostub, 1.6260_wp, cosmostub, 2.7000_wp, &   ! Cu-Ge
   & 2.3500_wp, 2.2000_wp, 2.1600_wp, 2.3630_wp, &   ! As-Kr
   & cosmostub, cosmostub, cosmostub, cosmostub, &   ! Rb-Zr
   & cosmostub, cosmostub, cosmostub, cosmostub, &   ! Nb-Ru
   & cosmostub, cosmostub, cosmostub, cosmostub, &   ! Rh-Cd
   & 2.2580_wp, 2.5500_wp, 2.4100_wp, 2.4100_wp, &   ! In-Te
   & 2.3200_wp, 2.5270_wp, cosmostub, cosmostub, &   ! I-Ba
   & cosmostub, cosmostub, cosmostub, cosmostub, &   ! La-Nd
   & cosmostub, cosmostub, cosmostub, cosmostub, &   ! Pm-Gd
   & cosmostub, cosmostub, cosmostub, cosmostub, &   ! Tb-Er
   & cosmostub, cosmostub, cosmostub, cosmostub, &   ! Tm-Hf
   & cosmostub, cosmostub, cosmostub, cosmostub, &   ! Ta-Os
   & cosmostub, cosmostub, cosmostub, cosmostub, &   ! Ir-Hg
   & cosmostub, 2.3600_wp, 2.4220_wp, 2.3050_wp, &   ! Tl-Po
   & 2.3630_wp, 2.5740_wp, cosmostub, cosmostub, &   ! At-Ra
   & cosmostub, cosmostub, cosmostub, cosmostub, &   ! Ac-U
   & cosmostub, cosmostub]                           ! Np-Pu

!> Default value for missing bondi radii 
   real(wp), parameter :: smdstub = 2.000_wp

   real(wp), parameter :: vanDerWaalsRadSMD(88) = aatoau * [ &
       & 1.20_wp, 1.40_wp, 1.81_wp, 1.53_wp, 1.92_wp, 1.85_wp, 1.89_wp, 1.52_wp, &  ! H-O
       & 1.73_wp, 1.54_wp, 2.27_wp, 1.73_wp, 1.84_wp, 2.47_wp, 2.12_wp, 2.49_wp, &  ! F-S
       & 2.38_wp, 1.88_wp, 2.75_wp, 2.31_wp, smdstub, smdstub, smdstub, smdstub, &  ! Cl-Cr
       & smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, 1.87_wp, 2.11_wp, &  ! Mn-Ge
       & 1.85_wp, 1.90_wp, 3.06_wp, 2.02_wp, 3.03_wp, 2.49_wp, smdstub, smdstub, &  ! As-Zr
       & smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, &  ! Nb-Cd
       & 1.93_wp, 2.17_wp, 2.06_wp, 2.06_wp, 1.98_wp, 2.16_wp, 3.43_wp, 2.68_wp, &  ! I-Ba
       & smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, &  ! La-Gd
       & smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, &  ! Tb-Hf
       & smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, smdstub, &  ! Ta-Hg
       & 1.96_wp, 2.02_wp, 2.07_wp, 1.97_wp, 2.02_wp, 2.20_wp, 3.48_wp, 2.83_wp]    ! Tl-Ra

       !> In case no van-der-Waals value is provided
       real(wp), parameter :: missing = -1.0_wp
       real(wp), parameter :: vanDerWaalsRadBondi(88) = aatoau * [ &
       & 1.10_wp, 1.40_wp, 1.81_wp, 1.53_wp, 1.92_wp, 1.70_wp, 1.55_wp, 1.52_wp, &  ! H-O
       & 1.47_wp, 1.54_wp, 2.27_wp, 1.73_wp, 1.84_wp, 2.10_wp, 1.80_wp, 1.80_wp, &  ! F-S
       & 1.75_wp, 1.88_wp, 2.75_wp, 2.31_wp, missing, missing, missing, missing, &  ! Cl-Cr
       & missing, missing, missing, missing, missing, missing, 1.87_wp, 2.11_wp, &  ! Mn-Ge
       & 1.85_wp, 1.90_wp, 1.83_wp, 2.02_wp, 3.03_wp, 2.49_wp, missing, missing, &  ! As-Zr
       & missing, missing, missing, missing, missing, missing, missing, missing, &  ! Nb-Cd
       & 1.93_wp, 2.17_wp, 2.06_wp, 2.06_wp, 1.98_wp, 2.16_wp, 3.43_wp, 2.68_wp, &  ! I-Ba
       & missing, missing, missing, missing, missing, missing, missing, missing, &  ! La-Gd
       & missing, missing, missing, missing, missing, missing, missing, missing, &  ! Tb-Hf
       & missing, missing, missing, missing, missing, missing, missing, missing, &  ! Ta-Hg
       & 1.96_wp, 2.02_wp, 2.07_wp, 1.97_wp, 2.02_wp, 2.20_wp, 3.48_wp, 2.83_wp]    ! Tl-Ra

   !> Following, the parameter for scaling radii for implicit solvation according
   !  to partial charges are written
   !  Every charge model (ceh, eeq, custom) has its own parameters as well as
   !  every solvent model (CPCM, COSMO, SMD, COSMO-RS). Also, a parameter set
   !  for water and all other solvents (currently acetonitrile, DMSO, and methanol
   !  exist.

   !> CPCM ####################################################################
   !> EEQ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents

   !> CEH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents
    real(wp), parameter :: ceh_to_radii_prefac_other_solvents_cpcm(94) = [ &
    &0.01549708_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, -1.01920028_wp, -0.01135841_wp, 0.0000_wp, &
    &0.02817292_wp, 8.89210118_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.13758058_wp, &
    &0.59197914_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.11699924_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp]
    real(wp), parameter :: ceh_to_radii_expo_other_solvents_cpcm(94) = [ &
    &2.64324258_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, -0.12086629_wp, 10.05473497_wp, 0.0000_wp, &
    &17.29022309_wp, 0.07154371_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 2.47482899_wp, &
    &-0.46517989_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, -2.31866330_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp]
    real(wp), parameter :: ceh_k_other_solvents_cpcm(94) = [ &
    &-57.99877895_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, -0.16917152_wp, 10.70959466_wp, 0.0000_wp, &
    &-9.02579520_wp, -0.96913362_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.09588330_wp, &
    &-1.50301780_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 21.07530789_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
    &0.0000_wp, 0.0000_wp]

   !> Custom (fitted on hirshfeld DFT charges) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents
   real(wp), parameter :: custom_prefac_other_solvents_cpcm(94) = [ &
   &0.01403900_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, -1.01163306_wp, -0.00911015_wp, 0.0000_wp, &
   &0.01090730_wp, 12.80787048_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, -0.57760924_wp, &
   &0.47465035_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.10170882_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp]
   real(wp), parameter :: custom_expo_other_solvents_cpcm(94) = [ &
   &-4.35392828_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, -0.02991820_wp, 7.37478174_wp, 0.0000_wp, &
   &37.05504711_wp, 0.03673753_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, -0.39667546_wp, &
   &-1.04761400_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.29374586_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp]
   real(wp), parameter :: custom_k_other_solvents_cpcm(94) = [ &
   &-76.62390068_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, -0.07164484_wp, 19.21171200_wp, 0.0000_wp, &
   &-34.38948575_wp, -1.19000518_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, -0.22252643_wp, &
   &1.95672136_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, -3.20296748_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp, 0.0000_wp, 0.0000_wp, &
   &0.0000_wp, 0.0000_wp]


   !> SMD #####################################################################
   !> EEQ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents

   !> CEH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents

   !> Custom (fitted on hirshfeld DFT charges) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents

   !> COSMO ###################################################################
   !> EEQ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents

   !> CEH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents

   !> Custom (fitted on hirshfeld DFT charges) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents

   !> COSMO-RS ################################################################
   !> EEQ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents

   !> CEH !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents

   !> Custom (fitted on hirshfeld DFT charges) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !> Water

   !> Other solvents



contains


!> Get van-der-Waals radius for species with a given symbol
elemental function getVanDerWaalsRadCPCMSymbol(symbol) result(rad)

!> Element symbol
character(len=*), intent(in) :: symbol

!> van-der-Waals radius
real(wp) :: rad

rad = getVanDerWaalsRadCPCM(toNumber(symbol))

end function getVanDerWaalsRadCPCMSymbol


!> Get van-der-Waals radius for species with a given atomic number
elemental function getVanDerWaalsRadCPCMNumber(number) result(rad)

!> Atomic number
integer, intent(in) :: number

!> van-der-Waals radius
real(wp) :: rad

if (number > 0 .and. number <= size(vanDerWaalsRadCPCM, dim=1)) then
   rad = vanDerWaalsRadCPCM(number)
else
   rad = -1.0_wp
end if

end function getVanDerWaalsRadCPCMNumber

!> Get van-der-Waals radius for species with a given symbol
elemental function getVanDerWaalsRadCosmoSymbol(symbol) result(rad)

   !> Element symbol
   character(len=*), intent(in) :: symbol

   !> van-der-Waals radius
   real(wp) :: rad

   rad = getVanDerWaalsRadCosmo(toNumber(symbol))

end function getVanDerWaalsRadCosmoSymbol


!> Get van-der-Waals radius for species with a given atomic number
elemental function getVanDerWaalsRadCosmoNumber(number) result(rad)

   !> Atomic number
   integer, intent(in) :: number

   !> van-der-Waals radius
   real(wp) :: rad

   if (number > 0 .and. number <= size(vanDerWaalsRadCosmo, dim=1)) then
      rad = vanDerWaalsRadCosmo(number)
   else
      rad = -1.0_wp
   end if

end function getVanDerWaalsRadCosmoNumber

elemental function getVanDerWaalsRadSMDSymbol(symbol) result(rad)

   !> Element symbol
   character(len=*), intent(in) :: symbol

   !> van-der-Waals radius
   real(wp) :: rad

   rad = getVanDerWaalsRadSMD(toNumber(symbol))

end function getVanDerWaalsRadSMDSymbol

elemental function getVanDerWaalsRadSMDNumber(number) result(rad)

   !> Atomic number
   integer, intent(in) :: number

   !> van-der-Waals radius
   real(wp) :: rad

   if (number > 0 .and. number <= size(vanDerWaalsRadSMD, dim=1)) then
      rad = vanDerWaalsRadSMD(number)
   else
      rad = -1.0_wp
   end if

end function getVanDerWaalsRadSMDNumber


!> Get default dielectric constant from Minnesota Solvation Database
function get_eps(solvent) result(epsilon)
 character(len=*), intent(in) :: solvent
 real(wp):: epsilon

 select case(solvent)
 case default
     epsilon=0.0_wp
 case('2methylpyridine')
     epsilon=9.9533_wp
 case('4methyl2pentanone')
     epsilon=12.8871
 case('aceticacid')
     epsilon=6.2528
 case('acetonitrile')
     epsilon=35.6881
 case('acetophenone')
     epsilon=17.44
 case('aniline')
     epsilon=6.8882
 case('anisole')
     epsilon=4.2247
 case('benzene')
     epsilon=2.2706
 case('benzonitrile')
     epsilon=25.592
 case('benzylalcohol')
     epsilon=12.4569
 case('bromobenzene')
     epsilon=5.3954
 case('bromoethane')
     epsilon=9.01
 case('bromoform')
     epsilon=4.2488
 case('bromooctane')
     epsilon=5.0244
 case('butanol')
     epsilon=17.3323
 case('butanone')
     epsilon=18.2457
 case('butylacetate')
     epsilon=4.9941
 case('butylbenzene')
     epsilon=2.36
 case('carbondisulfide')
     epsilon=2.6105
 case('carbontet')
     epsilon=2.228
 case('chlorobenzene')
     epsilon=5.6968
 case('chloroform')
     epsilon=4.7113
 case('chlorohexane')
     epsilon=5.9491
 case('cyclohexane')
     epsilon=2.0165
 case('cyclohexanone')
     epsilon=15.6186
 case('decalin')
     epsilon=2.196
 case('decane')
     epsilon=1.9846
 case('decanol')
     epsilon=7.5305
 case('dibromoethane')
     epsilon=4.9313
 case('dibutylether')
     epsilon=3.0473
 case('dichloroethane')
     epsilon=10.125
 case('diethylether')
     epsilon=4.24
 case('diisopropylether')
     epsilon=3.38
 case('dimethylacetamide')
     epsilon=37.7807
 case('dimethylformamide')
     epsilon=37.219
 case('dimethylpyridine')
     epsilon=7.1735
 case('dimethylsulfoxide')
     epsilon=46.826
 case('dodecane')
     epsilon=2.006
 case('ethanol')
     epsilon=24.852
 case('ethoxybenzene')
     epsilon=4.1797
 case('ethylacetate')
     epsilon=5.9867
 case('ethylbenzene')
     epsilon=2.4339
 case('fluorobenzene')
     epsilon=5.42
 case('fluoroctane')
     epsilon=3.89
 case('heptane')
     epsilon=1.9113
 case('heptanol')
     epsilon=11.321
 case('hexadecane')
     epsilon=2.0402
 case('hexadecyliodide')
     epsilon=3.5338
 case('hexane')
     epsilon=1.8819
 case('hexanol')
     epsilon=12.5102
 case('iodobenzene')
     epsilon=4.547
 case('isobutanol')
     epsilon=16.7766
 case('isooctane')
     epsilon=1.9358
 case('isopropanol')
     epsilon=19.2645
 case('isopropylbenzene')
     epsilon=2.3712
 case('isopropyltoluene')
     epsilon=2.2322
 case('mcresol')
     epsilon=12.44
 case('mesitylene')
     epsilon=2.265
 case('methoxyethanol')
     epsilon=17.2
 case('methylenechloride')
     epsilon=8.93
 case('methylformamide')
     epsilon=181.5619
 case('nitrobenzene')
     epsilon=34.8091
 case('nitroethane')
     epsilon=28.2896
 case('nitromethane')
     epsilon=36.5623
 case('nonane')
     epsilon=1.9605
 case('nonanol')
     epsilon=8.5991
 case('octane')
     epsilon=1.9406
 case('octanol')
     epsilon=9.8629
 case('odichlorobenzene')
     epsilon=9.9949
 case('onitrotoluene')
     epsilon=25.6692
 case('pentadecane')
     epsilon=2.0333
 case('pentane')
     epsilon=1.8371
 case('pentanol')
     epsilon=15.13
 case('perfluorobenzene')
     epsilon=2.029
 case('phenylether')
     epsilon=3.73
 case('propanol')
     epsilon=20.5237
 case('pyridine')
     epsilon=12.9776
 case('secbutanol')
     epsilon=15.9436
 case('secbutylbenzene')
     epsilon=2.3446
 case('tbutylbenzene')
     epsilon=2.3447
 case('tetrachloroethene')
     epsilon=2.268
 case('tetrahydrofuran')
     epsilon=7.4257
 case('tetrahydrothiophenedioxide')
     epsilon=43.9622
 case('tetralin')
     epsilon=2.771
 case('toluene')
     epsilon=2.3741
 case('tributylphosphate')
     epsilon=8.1781
 case('triethylamine')
     epsilon=2.3832
 case('trimethylbenzene')
     epsilon=2.3653
 case('undecane')
     epsilon=1.991
 case('water','h2o')
     epsilon=78.36_wp
 case('xylene')
     epsilon=2.3879
 case('benzene-water')
     epsilon=2.2706
 case('carbontet-water')
     epsilon=2.228
 case('chlorobenzene-water')
     epsilon=5.6968
 case('chloroform-water')
     epsilon=4.7113
 case('cyclohexane-water')
     epsilon=2.0165
 case('dibromoethane-water')
     epsilon=4.9313
 case('dibutylether-water')
     epsilon=3.0473
 case('dichloroethane-water')
     epsilon=10.125
 case('diethylether-water')
     epsilon=4.24
 case('ethylacetate-water')
     epsilon=5.9867
 case('heptane-water')
     epsilon=1.9113
 case('hexane-water')
     epsilon=1.8819
 case('nitrobenzene-water')
     epsilon=34.8091
 case('octanol-water')
     epsilon=9.8629
 case('methanol')
     epsilon=32.613
 end select
end function get_eps

function get_alpha(solvent) result(alpha)
 character(len=*), intent(in) :: solvent
 real(wp):: alpha

 select case(solvent)
 case default
     alpha = 0.0_wp
 case("aceticacid")
    alpha=0.61
 case("acetonitrile")
    alpha=0.07
 case("aniline")
    alpha=0.26
 case("benzylalcohol")
    alpha=0.33
 case("bromoform")
    alpha=0.15
 case("butanol")
    alpha=0.37
 case("chloroform")
    alpha=0.15
 case("decanol")
    alpha=0.37
 case("dibromoethane")
    alpha=0.1
 case("dichloroethane")
    alpha=0.1
 case("ethanol")
    alpha=0.37
 case("heptanol")
    alpha=0.37
 case("hexanol")
    alpha=0.37
 case("isobutanol")
    alpha=0.37
 case("isopropanol")
    alpha=0.33
 case("mcresol")
    alpha=0.57
 case("methoxyethanol")
    alpha=0.3
 case("methylenechloride")
    alpha=0.1
 case("methylformamide")
    alpha=0.4
 case("nitroethane")
    alpha=0.02
 case("nitromethane")
    alpha=0.06
 case("nonanol")
    alpha=0.37
 case("octanol")
    alpha=0.37
 case("pentanol")
    alpha=0.37
 case("propanol")
    alpha=0.37
 case("secbutanol")
    alpha=0.33
 case("water")
    alpha=0.82
 case("chloroform-water")
    alpha=0.15
 case("dibromoethane-water")
    alpha=0.1
 case("dichloroethane-water")
    alpha=0.1
 case("octanol-water")
    alpha=0.37
 case("methanol")
    alpha=0.43
 end select
end function get_alpha

function get_beta(solvent) result(beta)
character(len=*), intent(in) :: solvent
real(wp):: beta

select case(solvent)
  case default
     beta = 0.0_wp
  case ('2methylpyridine')
     beta=0.58
  case ('4methyl2pentanone')
     beta=0.51
  case ('aceticacid')
     beta=0.44
  case ('acetonitrile')
     beta=0.32
  case ('acetophenone')
     beta=0.48
  case ('aniline')
     beta=0.41
  case ('anisole')
     beta=0.29
  case ('benzene')
     beta=0.14
  case ('benzonitrile')
     beta=0.33
  case ('benzylalcohol')
     beta=0.56
  case ('bromobenzene')
     beta=0.09
  case ('bromoethane')
     beta=0.12
  case ('bromoform')
     beta=0.06
  case ('bromooctane')
     beta=0.12
  case ('butanol')
     beta=0.48
  case ('butanone')
     beta=0.51
  case ('butylacetate')
     beta=0.45
  case ('butylbenzene')
     beta=0.15
  case ('carbondisulfide')
     beta=0.07
  case ('carbontet')
     beta=0
  case ('chlorobenzene')
     beta=0.07
  case ('chloroform')
     beta=0.02
  case ('chlorohexane')
     beta=0.1
  case ('cyclohexane')
     beta=0
  case ('cyclohexanone')
     beta=0.56
  case ('decalin')
     beta=0
  case ('decane')
     beta=0
  case ('decanol')
     beta=0.48
  case ('dibromoethane')
     beta=0.17
  case ('dibutylether')
     beta=0.45
  case ('dichloroethane')
     beta=0.11
  case ('diethylether')
     beta=0.41
  case ('diisopropylether')
     beta=0.41
  case ('dimethylacetamide')
     beta=0.78
  case ('dimethylformamide')
     beta=0.74
  case ('dimethylpyridine')
     beta=0.63
  case ('dimethylsulfoxide')
     beta=0.88
  case ('dodecane')
     beta=0
  case ('ethanol')
     beta=0.48
  case ('ethoxybenzene')
     beta=0.32
  case ('ethylacetate')
     beta=0.45
  case ('ethylbenzene')
     beta=0.15
  case ('fluorobenzene')
     beta=0.1
  case ('fluoroctane')
     beta=0.1
  case ('heptane')
     beta=0
  case ('heptanol')
     beta=0.48
  case ('hexadecane')
     beta=0
  case ('hexadecyliodide')
     beta=0.15
  case ('hexanol')
     beta=0.48
  case ('iodobenzene')
     beta=0.12
  case ('isobutanol')
     beta=0.48
  case ('isooctane')
     beta=0
  case ('isopropanol')
     beta=0.56
  case ('isopropylbenzene')
     beta=0.16
  case ('isopropyltoluene')
     beta=0.19
  case ('mcresol')
     beta=0.34
  case ('mesitylene')
     beta=0.19
  case ('methoxyethanol')
     beta=0.84
  case ('methylenechloride')
     beta=0.05
  case ('nitrobenzene')
     beta=0.28
  case ('nitroethane')
     beta=0.33
  case ('nitromethane')
     beta=0.31
  case ('nonane')
     beta=0
  case ('nonanol')
     beta=0.48
  case ('octanol')
     beta=0.48
  case ('odichlorobenzene')
     beta=0.04
  case ('onitrotoluene')
     beta=0.27
  case ('pentadecane')
     beta=0
  case ('pentane')
     beta=0
  case ('pentanol')
     beta=0.48
  case ('perfluorobenzene')
     beta=0
  case ('phenylether')
     beta=0.2
  case ('secbutanol')
     beta=0.56
  case ('secbutylbenzene')
     beta=0.16
  case ('tbutylbenzene')
     beta=0.16
  case ('tetrachloroethene')
     beta=0
  case ('tetrahydrofuran')
     beta=0.48
  case ('tetrahydrothiophenedioxide')
     beta=0.88
  case ('tetralin')
     beta=0.19
  case ('tributylphosphate')
     beta=1.21
  case ('triethylamine')
     beta=0.79
  case ('trimethylbenzene')
     beta=0.19
  case ('undecane')
     beta=0
  case ('water')
     beta=0.35
  case ('xylene')
     beta=0.16
  case ('benzene-water')
     beta=0.14
  case ('carbontet-water')
     beta=0
  case ('chlorobenzene-water')
     beta=0.07
  case ('chloroform-water')
     beta=0.02
  case ('cyclohexane-water')
     beta=0
  case ('dibromoethane-water')
     beta=0.17
  case ('dibutylether-water')
     beta=0.45
  case ('dichloroethane-water')
     beta=0.11
  case ('diethylether-water')
     beta=0.41
  case ('ethylacetate-water')
     beta=0.45
  case ('heptane-water')
     beta=0
  case ('nitrobenzene-water')
     beta=0.28
  case ('octanol-water')
     beta=0.48
  case ('methanol')
     beta=0.47
end select

end function get_beta

end module draco_data
