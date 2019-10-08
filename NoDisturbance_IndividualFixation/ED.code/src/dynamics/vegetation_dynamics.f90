!==========================================================================================!
!==========================================================================================!
!     This subroutine is the main driver for the longer-term vegetation dynamics.  This    !
! has become a file by itself to reduce the number of sub-routines that are doubled        !
! between ED-2.1 stand alone and the coupled model.                                        !
!------------------------------------------------------------------------------------------!
subroutine vegetation_dynamics(new_month,new_year)
   use grid_coms        , only : ngrids
   use ed_misc_coms     , only : current_time, iyeara           & ! intent(in)
                               , dtlsm                  & ! intent(in)
                               , frqsum                 & ! intent(in)
                               , ied_init_mode          ! ! intent(in)
   use disturb_coms     , only : include_fire           ! ! intent(in)
   use disturbance_utils, only : apply_disturbances     & ! subroutine
                               , site_disturbance_rates ! ! subroutine
   use fuse_fiss_utils  , only : fuse_patches           ! ! subroutine
   use ed_state_vars    , only : edgrid_g               & ! intent(inout)
                               , edtype,polygontype,sitetype,patchtype                 ! ! variable type
   use growth_balive    , only : dbalive_dt             & ! subroutine
                               , dbalive_dt_eq_0        ! ! subroutine
   use consts_coms      , only : day_sec                & ! intent(in)
                               , yr_day                 ! ! intent(in)
   use mem_polygons     , only : maxpatch               ! ! intent(in)
   use pft_coms         ,only  : c2n_leaf               &
                               , c2n_recruit            &
                               , c2n_stem               &
                               , c2n_slow               &
                               , c2n_structural         &
                               , c2n_storage            &
                               , n_pft                  &
                               , include_pft               
   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   logical     , intent(in)   :: new_month
   logical     , intent(in)   :: new_year
   !----- Local variables. ----------------------------------------------------------------!
   type(edtype), pointer      :: cgrid
   real                       :: tfact1
   real                       :: tfact2
   integer                    :: doy
   integer                    :: ip
   integer                    :: isite
   integer                    :: ifm
   real                       :: oldmsn  !JL!
   real                       :: newmsn  !JL!
   integer                    :: i       !JL
   !----- External functions. -------------------------------------------------------------!
   integer     , external     :: julday
   !---------------------------------------------------------------------------------------!

   real :: oldpn, oldsn, newpn, newsn, oldtn, newtn,new_balive,new_bdead,new_bstorage,old_balive,old_bdead,old_bstorage
   real :: DailyForestFixation, DailyFixationDemand !JL
 

   type(polygontype), pointer :: cpoly
   type(sitetype), pointer :: csite
   type(patchtype),pointer :: cpatch
   integer :: ipy, isi, ipa, ico

   oldpn=0.;oldtn=0.;oldsn=0.;newpn=0.;newsn=0.;newtn=0.;newmsn=0.;oldmsn=0. !old and newmsn were added JL
   old_balive = .0;old_bdead = .0; old_bstorage = .0;  new_balive = .0; new_bdead = .0; new_bstorage = 0. 



   !----- Find the day of year. -----------------------------------------------------------!
   doy = julday(current_time%month, current_time%date, current_time%year)
   !----- Time factor for normalizing daily variables updated on the DTLSM step. ----------!
   tfact1 = dtlsm / day_sec
   !----- Time factor for averaging dailies. ----------------------------------------------!
   tfact2 = 1.0 / yr_day

   !----- Apply events. -------------------------------------------------------------------!

   call prescribed_event(current_time%year,doy)
 
   !---------------------------------------------------------------------------------------!
   !   Loop over all domains.                                                              !
   !---------------------------------------------------------------------------------------!
   do ifm=1,ngrids

      cgrid => edgrid_g(ifm) 

      do ipy=1,cgrid%npolygons
         cpoly => cgrid%polygon(ipy)
         do isi=1,cpoly%nsites
            csite => cpoly%site(isi)
            do ipa=1,csite%npatches
               oldsn = oldsn + (csite%slow_soil_C(ipa)/c2n_slow+csite%fast_soil_N(ipa) + csite%structural_soil_C(ipa)/c2n_structural + csite%mineralized_soil_N(ipa)) * csite%area(ipa)
               cpatch => csite%patch(ipa)
               do i=1,n_pft
                  if(include_pft(i)) then
                     oldpn = oldpn + csite%area(ipa) * (csite%repro(i,ipa)/c2n_recruit(i))
                  end if
               end do 
               oldmsn = oldmsn + csite%mineralized_soil_N(ipa) * csite%area(ipa)!JL!
               do ico=1,cpatch%ncohorts
                    oldpn = oldpn + csite%area(ipa) * cpatch%nplant(ico) * (cpatch%balive(ico)/c2n_leaf(cpatch%pft(ico))   &
                             + cpatch%bdead(ico)/c2n_stem(cpatch%pft(ico))+cpatch%nstorage(ico)) !JL
                    old_balive = old_balive + csite%area(ipa) * cpatch%nplant(ico) * cpatch%balive(ico)/c2n_leaf(cpatch%pft(ico))
                    old_bdead = old_bdead + csite%area(ipa) * cpatch%nplant(ico) * cpatch%bdead(ico)/c2n_stem(cpatch%pft(ico))
                    old_bstorage = old_bstorage + csite%area(ipa) * cpatch%nplant(ico) * cpatch%nstorage(ico)
               enddo
            enddo
         enddo
      enddo

print*,'DN, INITIAL', 'SoilN',oldsn,'PlantN',oldpn, 'ForestN',oldsn+oldpn,'MSN',oldmsn,'BaliveN',old_balive,'BdeadN',old_bdead,'NstorageN',old_bstorage


      !     The following block corresponds to the daily time-step.                        !
      !------------------------------------------------------------------------------------!
      !----- Standardise the fast-scale uptake and respiration, for growth rates. ---------!
 
print*, 'ONE'

     call normalize_ed_daily_vars(cgrid, tfact1)

      !----- Update phenology and growth of live tissues. ---------------------------------!
      select case (ied_init_mode)
      case (-8)
         !----- Special case, in which we don't solve the actual vegetation dynamics. -----!
         call phenology_driver_eq_0(cgrid,doy,current_time%month, tfact1)
         call dbalive_dt_eq_0(cgrid,tfact2)
      case default
print*, 'TWO'
         call phenology_driver(cgrid,doy,current_time%month, tfact1)
print*, 'THREE' 
         call dbalive_dt(cgrid,tfact2)
print*, 'FOUR' 
      end select
      !------------------------------------------------------------------------------------!

      !------------------------------------------------------------------------------------!
      !     The following block corresponds to the monthly time-step:                      !
      !------------------------------------------------------------------------------------!
      if (new_month) then

         !----- Update the mean workload counter. -----------------------------------------!
print*, 'FIVE' 
         call update_workload(cgrid)
 
         !----- Update the growth of the structural biomass. ------------------------------!
print*, 'SIX'
         call structural_growth(cgrid, current_time%month)

         !----- Solve the reproduction rates. ---------------------------------------------!
print*, 'SEVEN'
         call reproduction(cgrid,current_time%month)

         !----- Update the fire disturbance rates. ----------------------------------------!
         if (include_fire /= 0) then
print*, 'EIGHT'
            call fire_frequency(current_time%month,cgrid) 
        end if

         !----- Update the disturbance rates. ---------------------------------------------!
print*, 'NINE'
         call site_disturbance_rates(current_time%month, current_time%year, cgrid)
      endif
      !------  update dmean and mmean values for NPP allocation terms ---------------------!
print*, 'TEN'
      call normalize_ed_dailyNPP_vars(cgrid) 
      !------------------------------------------------------------------------------------!
      !     This should be done every day, but after the longer-scale steps.  We update    !
      ! the carbon and nitrogen pools, and re-set the daily variables.                     !
      !------------------------------------------------------------------------------------!
print*, 'ELEVEN' 
     call update_C_and_N_pools(cgrid)
print*, 'TWELVE'
      call zero_ed_daily_vars(cgrid)
print*, 'THIRTEEN'
      !------------------------------------------------------------------------------------!
 

      !-----Zero Yearly Nitrogen Variables. --------------------------------------------------!
      ! Yearly variables are reset every June. Reset the Annual Nitrogen variables to 0 on 
      ! the first day of June! JL
      if (current_time%month == 6 .and.current_time%date == 2) then 
        cgrid%total_N_Fixation(1) = 0.0
        cgrid%total_DON_loss(1)   = 0.0
        cgrid%total_DIN_loss(1)   = 0.0
        cgrid%total_Ngas_loss(1)  = 0.0
        cgrid%total_N_demand(1)   = 0.0
        cgrid%total_N_supply(1)  = 0.0
        print*, 'YEARLY ZERO'
      endif



     do ipy=1,cgrid%npolygons
         cpoly => cgrid%polygon(ipy)
         do isi=1,cpoly%nsites
            csite => cpoly%site(isi)
            do ipa=1,csite%npatches
               newsn = newsn + (csite%slow_soil_C(ipa)/c2n_slow +csite%fast_soil_N(ipa) + csite%structural_soil_C(ipa)/c2n_structural    &
                      + csite%mineralized_soil_N(ipa)) * csite%area(ipa)
               cpatch => csite%patch(ipa)
               do i=1,n_pft
                  if(include_pft(i)) then
                     newpn = newpn + csite%area(ipa) * (csite%repro(i,ipa)/c2n_recruit(i))
                  end if
               end do
                  newmsn = newmsn + csite%mineralized_soil_N(ipa) * csite%area(ipa)!JL! 
             do ico=1,cpatch%ncohorts
                  newpn = newpn + csite%area(ipa) * cpatch%nplant(ico) * (cpatch%balive(ico)/c2n_leaf(cpatch%pft(ico)) &
                                + cpatch%bdead(ico)/c2n_stem(cpatch%pft(ico))+cpatch%nstorage(ico))
                  new_balive = new_balive + csite%area(ipa) * cpatch%nplant(ico) * cpatch%balive(ico)/c2n_leaf(cpatch%pft(ico))
                  new_bdead = new_bdead + csite%area(ipa) * cpatch%nplant(ico) * cpatch%bdead(ico)/c2n_stem(cpatch%pft(ico))
                  new_bstorage = new_bstorage + csite%area(ipa) * cpatch%nplant(ico) * cpatch%nstorage(ico)
               enddo
            enddo
         enddo
      enddo
      
      print*,'DN, FINAL','SoilN',newsn,'PlantN',newpn,'ForestN',newsn+newpn,'MSN', newmsn,'Balive',new_balive,'Bdead',new_bdead,'Nstorage',new_bstorage ! 


         cgrid%ForestN(1)           = newsn+newpn !JL!
         cgrid%PlantN(1)            = newpn !JL!
         cgrid%SoilN(1)             = newsn !JL!

         !----- This is actually the yearly time-step, apply the disturbances. ------------!
         if (new_month .and. new_year) then

          ! Comment out update_treefall_rate subroutine if you choose regular ED2IN treefall disturbance
          ! This subroutine linearly increases disturbance rate over a specified interval of time 
print*, 'FOURTEEN'  
         call update_treefall_rate(iyeara, current_time%year)
 print*, 'FIFTEEN' 
           call apply_disturbances(cgrid)
print*, 'SIXTEEN' 
         end if


      !------------------------------------------------------------------------------------!
      !      Fuse patches last, after all updates have been applied.  This reduces the     !
      ! number of patch variables that actually need to be fused.                          !

      !------------------------------------------------------------------------------------!

      if(new_year) then
print*, 'SEVENTEEN'   
       if (maxpatch >= 0) call fuse_patches(cgrid,ifm)
print*, 'EIGHTEEN' 
      end if

      !------------------------------------------------------------------------------------!

      !----- Recalculate the AGB and basal area at the polygon level. ---------------------!
print*, 'NINETEEN' 
      call update_polygon_derived_props(cgrid)

print*, 'TWENTY' 
      call print_C_and_N_budgets(cgrid)
print*, 'TWENTY-ONE' 
      !------------------------------------------------------------------------------------!

   end do

   return
end subroutine vegetation_dynamics
!==========================================================================================!
!==========================================================================================!


!==========================================================================================!
!==========================================================================================!

 subroutine update_treefall_rate(first_year, current_year)
  use disturb_coms       , only: treefall_disturbance_rate
  use nutrient_constants , only: dist_start                    & 
                               , dist_end                      & 
                               , max_treefall_disturbance_rate  
                             
  implicit none

  integer, intent(in) :: first_year, current_year

 ! treefall_disturbance_rate = (min(dist_end,max(dist_start,current_year-first_year)) - dist_start) &
 !                              * (max_treefall_disturbance_rate/(dist_end-dist_start))
  treefall_disturbance_rate = 0
  return
 end subroutine update_treefall_rate
!==========================================================================================!
!==========================================================================================!
