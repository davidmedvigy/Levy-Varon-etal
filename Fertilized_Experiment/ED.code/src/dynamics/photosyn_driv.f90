!==========================================================================================!
!==========================================================================================!
!     This subroutine will control the photosynthesis scheme (Farquar and Leuning).  This  !
! is called every step, but not every sub-step.                                            !
!------------------------------------------------------------------------------------------!
subroutine canopy_photosynthesis(csite,cmet,mzg,ipa,lsl,ntext_soil                         &
                                ,leaf_aging_factor,green_leaf_factor)
   use ed_state_vars  , only : sitetype          & ! structure
                             , patchtype         ! ! structure
   use ed_max_dims    , only : n_pft             ! ! intent(in)
   use pft_coms       , only : leaf_width        & ! intent(in)
                             , water_conductance & ! intent(in)
                             , include_pft       ! ! intent(in)
   use soil_coms      , only : soil              & ! intent(in)
                             , slz               & ! intent(in)
                             , dslz              & ! intent(in)
                             , freezecoef        ! ! intent(in)
   use consts_coms    , only : t00               & ! intent(in)
                             , epi               & ! intent(in)
                             , wdnsi             & ! intent(in)
                             , wdns              & ! intent(in)
                             , kgCday_2_umols    & ! intent(in)
                             , lnexp_min         ! ! intent(in)
   use ed_misc_coms   , only : current_time      ! ! intent(in)
   use met_driver_coms, only : met_driv_state    ! ! structure
   use physiology_coms, only : print_photo_debug & ! intent(in)
                             , h2o_plant_lim     ! ! intent(in)
   use farq_leuning   , only : lphysiol_full     ! ! sub-routine
   implicit none
   !----- Arguments -----------------------------------------------------------------------!
   type(sitetype)            , target      :: csite             ! Current site
   type(met_driv_state)      , target      :: cmet              ! Current met. conditions.
   integer                   , intent(in)  :: ipa               ! Current patch #
   integer                   , intent(in)  :: lsl               ! Lowest soil level
   integer                   , intent(in)  :: mzg               ! Number of soil layers
   integer, dimension(mzg)   , intent(in)  :: ntext_soil        ! Soil class
   real   , dimension(n_pft) , intent(in)  :: leaf_aging_factor ! 
   real   , dimension(n_pft) , intent(in)  :: green_leaf_factor ! 
   !----- Local variables -----------------------------------------------------------------!
   type(patchtype)           , pointer     :: cpatch             ! Current site
   integer                                 :: ico                ! Current cohort #
   integer                                 :: tuco               ! Tallest used cohort
   integer                                 :: ipft
   integer                                 :: k1
   integer                                 :: k2
   integer                                 :: kroot
   integer                                 :: nsoil
   integer                                 :: limit_flag
   logical, dimension(mzg+1)               :: root_depth_indices ! 
   logical                                 :: las
   real   , dimension(mzg+1)               :: avg_frozen_water
   real   , dimension(mzg+1)               :: avg_liquid_water
   real   , dimension(mzg+1)               :: available_liquid_water
   real   , dimension(mzg+1)               :: wilting_factor
   real                                    :: leaf_par
   real                                    :: leaf_resp
   real                                    :: d_gsw_open
   real                                    :: d_gsw_closed
   real                                    :: d_lsfc_shv_open
   real                                    :: d_lsfc_shv_closed
   real                                    :: d_lsfc_co2_open
   real                                    :: d_lsfc_co2_closed
   real                                    :: d_lint_co2_open
   real                                    :: d_lint_co2_closed
   real                                    :: slpotv
   real                                    :: swp
   real                                    :: vm
   real                                    :: compp
   real                                    :: broot_tot
   real                                    :: broot_loc
   real                                    :: wgpfrac
   real                                    :: avg_fracliq
   real                                    :: psi_wilting
   real                                    :: psi_layer
   real                                    :: freezecor
   real                                    :: pss_available_water
   !---------------------------------------------------------------------------------------!


   !----- Point to the cohort structures --------------------------------------------------!
   cpatch => csite%patch(ipa)

   !----- Find the patch-level Total Leaf and Wood Area Index. ----------------------------!
   csite%lai(ipa) = 0.0
   csite%wpa(ipa) = 0.0
   csite%wai(ipa) = 0.0
   do ico=1,cpatch%ncohorts
      csite%lai(ipa)  = csite%lai(ipa)  + cpatch%lai(ico)
      csite%wpa(ipa)  = csite%wpa(ipa)  + cpatch%wpa(ico)
      csite%wai(ipa)  = csite%wai(ipa)  + cpatch%wai(ico)
   end do


   !----- Calculate liquid water available for transpiration. -----------------------------!
   available_liquid_water(mzg+1) = 0.
   do k1 = mzg, lsl, -1
      nsoil = ntext_soil(k1)
      available_liquid_water(k1) = available_liquid_water(k1+1)                            &
                                 + wdns * dslz(k1) * csite%soil_fracliq(k1,ipa)            &
                                 * max(0.0, csite%soil_water(k1,ipa) - soil(nsoil)%soilwp )
   end do
   !---------------------------------------------------------------------------------------!


   !---------------------------------------------------------------------------------------!
   !     If we are solving H2O_PLANT_LIM = 2, then we must account for the water potential !
   ! as in CLM.                                                                            !
   !---------------------------------------------------------------------------------------!
   select case (h2o_plant_lim)
   case (2)

      !----- Find the average liquid and frozen water. ------------------------------------!
      avg_liquid_water(:) = 0.0
      avg_frozen_water(:) = 0.0
      do k1=mzg,lsl,-1
         avg_frozen_water(k1) = avg_frozen_water(k1+1)                                     &
                              + dslz(k1) * (1. - csite%soil_fracliq(k1,ipa))               &
                              * csite%soil_water(k1,ipa)
         avg_liquid_water(k1) = avg_liquid_water(k1+1)                                     &
                              + dslz(k1) * csite%soil_fracliq(k1,ipa)                      &
                              * csite%soil_water(k1,ipa)
      end do
      avg_frozen_water(lsl:mzg) = avg_frozen_water(lsl:mzg) / (- slz(lsl:mzg))
      avg_liquid_water(lsl:mzg) = avg_liquid_water(lsl:mzg) / (- slz(lsl:mzg))
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !    Compute the soil potential for transpiration for each layer as in CLM.          !
      !------------------------------------------------------------------------------------!
      wilting_factor(:) = 0.0
      do k1=lsl,mzg
         nsoil = ntext_soil(k1)
         if (avg_liquid_water(k1) > soil(nsoil)%soilwp) then
            !----- Find the average soil wetness. -----------------------------------------!
            wgpfrac = avg_liquid_water(k1) / soil(nsoil)%slmsts
            avg_fracliq  = avg_liquid_water(k1)                                            &
                         / (avg_liquid_water(k1) + avg_frozen_water(k1))

            !----- Apply correction in case the soil is partially frozen. -----------------!
            freezecor    = 10. ** max(lnexp_min,- freezecoef * (1.0 - avg_fracliq))
            psi_wilting  = soil(nsoil)%slpots                                              &
                         / (soil(nsoil)%soilwp / soil(nsoil)%slmsts) ** soil(nsoil)%slbs
            psi_layer    = soil(nsoil)%slpots / wgpfrac ** soil(nsoil)%slbs

            !----- Find the wilting factor which will control the dry soil correction. ----!
            wilting_factor(k1) = max( 0., min(1., (psi_wilting - psi_layer         )       &
                                                / (psi_wilting - soil(nsoil)%slpots) ) )
         end if
      end do
      !------------------------------------------------------------------------------------!
   end select
   !---------------------------------------------------------------------------------------!




   !---------------------------------------------------------------------------------------!
   !     Initialize the array of maximum photosynthesis rates used in the mortality        !
   ! function.                                                                             !
   !---------------------------------------------------------------------------------------!
   csite%A_o_max(1:n_pft,ipa) = 0.0
   csite%A_c_max(1:n_pft,ipa) = 0.0

   !---------------------------------------------------------------------------------------!
   !     Find the tallest cohort with TAI above minimum, sufficient heat capacity, and not !
   ! buried in snow.  The first two conditions are redundant, but we will keep them for    !
   ! the time being, so it is going to be safer.                                           !
   !---------------------------------------------------------------------------------------!
   las = .false.
   do ico = 1,cpatch%ncohorts
      !----- If this is the tallest cohort to be used, we save its index. -----------------!
      if (.not. las .and. cpatch%leaf_resolvable(ico)) then
         las  = .true.
         tuco = ico
      end if
   end do

   !---------------------------------------------------------------------------------------!
   !---------------------------------------------------------------------------------------!
   !    There is at least one cohort that meet requirements.  And this is tallest one, so  !
   ! we can use it to compute the maximum photosynthetic rates, i.e., the rate the cohort  !
   ! would have if it were at the top of the canopy.  This is used for the mortality       !
   ! function.                                                                             !
   !---------------------------------------------------------------------------------------!
   if (las) then
      !----- We now loop over PFTs, not cohorts, skipping those we are not using. ---------!
      do ipft = 1, n_pft
         if (include_pft(ipft)) then

            !------------------------------------------------------------------------------!
            !    Scale photosynthetically active radiation per unit of leaf.               !
            !------------------------------------------------------------------------------!
            leaf_par = csite%par_l_max(ipa) / cpatch%lai(tuco)

            !------------------------------------------------------------------------------!
            !    Call the photosynthesis for maximum photosynthetic rates.  The units      !
            ! of the input and output are the standard in most of ED modules, but many of  !
            ! them are converted inside the photosynthesis model.                          !
            !    Notice that the units that are per unit area are per m� of leaf, not the  !
            ! patch area.                                                                  !
            !------------------------------------------------------------------------------!
            call lphysiol_full(            & !
               csite%can_prss(ipa)         & ! Canopy air pressure              [       Pa]
             , csite%can_rhos(ipa)         & ! Canopy air density               [    kg/m�]
             , csite%can_shv(ipa)          & ! Canopy air sp. humidity          [    kg/kg]
             , csite%can_co2(ipa)          & ! Canopy air CO2 mixing ratio      [ �mol/mol]
             , ipft                        & ! Plant functional type            [      ---]
             , leaf_par                    & ! Absorbed photos. active rad.     [     W/m�]
             , cpatch%leaf_temp(tuco)      & ! Leaf temperature                 [        K]
             , cpatch%lint_shv(tuco)       & ! Leaf intercellular spec. hum.    [    kg/kg]
             , green_leaf_factor(ipft)     & ! Greenness rel. to on-allometry   [      ---]
             , leaf_aging_factor(ipft)     & ! Ageing parameter to scale VM     [      ---]
             , cpatch%llspan(tuco)         & ! Leaf life span                   [       yr]
             , cpatch%vm_bar(tuco)         & ! Average Vm function              [�mol/m�/s]
             , cpatch%leaf_gbw(tuco)       & ! Aerodyn. condct. of water vapour [  kg/m�/s]
             , csite%A_o_max(ipft,ipa)     & ! Photosynthesis rate     (open)   [�mol/m�/s]
             , csite%A_c_max(ipft,ipa)     & ! Photosynthesis rate     (closed) [�mol/m�/s]
             , d_gsw_open                  & ! Stom. condct. of water  (open)   [  kg/m�/s]
             , d_gsw_closed                & ! Stom. condct. of water  (closed) [  kg/m�/s]
             , d_lsfc_shv_open             & ! Leaf sfc. sp. humidity  (open)   [    kg/kg]
             , d_lsfc_shv_closed           & ! Leaf sfc. sp. humidity  (closed) [    kg/kg]
             , d_lsfc_co2_open             & ! Leaf sfc. CO2 mix. rat. (open)   [ �mol/mol]
             , d_lsfc_co2_closed           & ! Leaf sfc. CO2 mix. rat. (closed) [ �mol/mol]
             , d_lint_co2_open             & ! Intercellular CO2       (open)   [ �mol/mol]
             , d_lint_co2_closed           & ! Intercellular CO2       (closed) [ �mol/mol]
             , leaf_resp                   & ! Leaf respiration rate            [�mol/m�/s]
             , vm                          & ! Max. capacity of Rubisco         [�mol/m�/s]
             , compp                       & ! Gross photo. compensation point  [ �mol/mol]
             , limit_flag                  & ! Photosynthesis limitation flag   [      ---]
             , csite%old_stoma_data_max(ipft,ipa) & ! Previous state            [      ---]
             )
         end if
      end do
         
   else
      !---- There is no "active" cohort. --------------------------------------------------!
      csite%A_o_max(1:n_pft,ipa) = 0.0
      csite%A_c_max(1:n_pft,ipa) = 0.0
   end if
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !    Initialize some variables.                                                         !
   !---------------------------------------------------------------------------------------!
   !----- Total root biomass (in kgC/m2) and patch sum available water. -------------------!
   pss_available_water = 0.0
   broot_tot           = 0.0
   !----- Initialize variables for transpiration calculation. -----------------------------!
   root_depth_indices(:) = .false.
   !---------------------------------------------------------------------------------------!

   !---------------------------------------------------------------------------------------!
   !    Loop over all cohorts, from tallest to shortest.                                   !
   !---------------------------------------------------------------------------------------!
   cohortloop: do ico = 1,cpatch%ncohorts
         
      !------------------------------------------------------------------------------------!
      !     Only need to worry about photosyn if radiative transfer has been  done for     !
      ! this cohort.                                                                       !
      !------------------------------------------------------------------------------------!
      if (cpatch%leaf_resolvable(ico)) then

            !----- Alias for PFT ----------------------------------------------------------!
            ipft = cpatch%pft(ico)

            !------------------------------------------------------------------------------!
            !    Scale photosynthetically active radiation per unit of leaf.               !
            !------------------------------------------------------------------------------!
            leaf_par = cpatch%par_l(ico) / cpatch%lai(ico) 


            !------------------------------------------------------------------------------!
            !    Call the photosynthesis for actual photosynthetic rates.  The units       !
            ! of the input and output are the standard in most of ED modules, but many of  !
            ! them are converted inside the photosynthesis model.                          !
            !    Notice that the units that are per unit area are per m� of leaf, not the  !
            ! patch area.                                                                  !
            !------------------------------------------------------------------------------!
            call lphysiol_full(            & !
               csite%can_prss(ipa)         & ! Canopy air pressure              [       Pa]
             , csite%can_rhos(ipa)         & ! Canopy air density               [    kg/m�]
             , csite%can_shv(ipa)          & ! Canopy air sp. humidity          [    kg/kg]
             , csite%can_co2(ipa)          & ! Canopy air CO2 mixing ratio      [ �mol/mol]
             , ipft                        & ! Plant functional type            [      ---]
             , leaf_par                    & ! Absorbed photos. active rad.     [     W/m�]
             , cpatch%leaf_temp(ico)       & ! Leaf temperature                 [        K]
             , cpatch%lint_shv(ico)        & ! Leaf intercellular spec. hum.    [    kg/kg]
             , green_leaf_factor(ipft)     & ! Greenness rel. to on-allometry   [      ---]
             , leaf_aging_factor(ipft)     & ! Ageing parameter to scale VM     [      ---]
             , cpatch%llspan(ico)          & ! Leaf life span                   [       yr]
             , cpatch%vm_bar(ico)          & ! Average Vm function              [�mol/m�/s]
             , cpatch%leaf_gbw(ico)        & ! Aerodyn. condct. of water vapour [  kg/m�/s]
             , cpatch%A_open(ico)          & ! Photosynthesis rate     (open)   [�mol/m�/s]
             , cpatch%A_closed(ico)        & ! Photosynthesis rate     (closed) [�mol/m�/s]
             , cpatch%gsw_open(ico)        & ! Stom. condct. of water  (open)   [  kg/m�/s]
             , cpatch%gsw_closed(ico)      & ! Stom. condct. of water  (closed) [  kg/m�/s]
             , cpatch%lsfc_shv_open(ico)   & ! Leaf sfc. sp. humidity  (open)   [    kg/kg] 
             , cpatch%lsfc_shv_closed(ico) & ! Leaf sfc. sp. humidity  (closed) [    kg/kg]
             , cpatch%lsfc_co2_open(ico)   & ! Leaf sfc. CO2 mix. rat. (open)   [ �mol/mol]
             , cpatch%lsfc_co2_closed(ico) & ! Leaf sfc. CO2 mix. rat. (closed) [ �mol/mol]
             , cpatch%lint_co2_open(ico)   & ! Intercellular CO2       (open)   [ �mol/mol]
             , cpatch%lint_co2_closed(ico) & ! Intercellular CO2       (closed) [ �mol/mol]
             , leaf_resp                   & ! Leaf respiration rate            [�mol/m�/s]
             , vm                          & ! Max. capacity of Rubisco         [�mol/m�/s]
             , compp                       & ! Gross photo. compensation point  [ �mol/mol]
             , limit_flag                  & ! Photosynthesis limitation flag   [      ---]
             , csite%old_stoma_data_max(ipft,ipa) & ! Previous state            [      ---]
             )
!if (ipa == 1) then
!print*,'photosynthesis',cpatch%A_open(ico),'ico',ico,'hite',cpatch%hite(ico),&
!'par',leaf_par,'leaf_par',leaf_par,'lai',cpatch%lai(ico),'pft',cpatch%pft(ico) !leaf level A in umol/co2/m2leaf/sec 
!endif
            !----- Convert leaf respiration to [�mol/m�ground/s] --------------------------!
            cpatch%leaf_respiration(ico) = leaf_resp * cpatch%lai(ico)
            cpatch%mean_leaf_resp(ico)   = cpatch%mean_leaf_resp(ico)                      &
                                         + cpatch%leaf_respiration(ico)
            cpatch%today_leaf_resp(ico)  = cpatch%today_leaf_resp(ico)                     &
                                         + cpatch%leaf_respiration(ico)

            !----- Root biomass [kg/m2]. --------------------------------------------------!
            broot_loc = cpatch%broot(ico)  * cpatch%nplant(ico)

            !----- Supply of water. -------------------------------------------------------!
            cpatch%water_supply(ico) = water_conductance(ipft)                             &
                                     * available_liquid_water(cpatch%krdepth(ico))         &
                                     * broot_loc

            root_depth_indices(cpatch%krdepth(ico)) = .true.
            broot_tot = broot_tot + broot_loc
            pss_available_water = pss_available_water                                      &
                                + available_liquid_water(cpatch%krdepth(ico)) * broot_loc

            !------------------------------------------------------------------------------!
            !     Determine the fraction of open stomata due to water limitation.          !
            ! This is a function of the ratio between the potential water demand           !
            ! (cpatch%psi_open, which is the average over the last time step), and the     !
            ! supply (cpatch%water_supply).                                                !
            !------------------------------------------------------------------------------!
            select case (h2o_plant_lim)
            case (0)
               !---- No water limitation, fsw is always 1.0. ------------------------------!
               cpatch%fsw(ico) = 1.0

            case (1)
               !---- Original ED-1.0 scheme. ----------------------------------------------!
               cpatch%fsw(ico) = cpatch%water_supply(ico)                                  &
                               / max( 1.0e-20                                              &
                                    , cpatch%water_supply(ico) + cpatch%psi_open(ico))
            case (2)
               !---------------------------------------------------------------------------!
               !     Somewhat based on CLM, but we reduce the total amount of available    !
               ! water by the fraction of root biomass belonging to this cohort.  We don't !
               ! have the root profile up to now, assume they are evenly distributed       !
               ! through all layers that have roots.                                       !
               !---------------------------------------------------------------------------!
               cpatch%fsw(ico) = wilting_factor(cpatch%krdepth(ico))

            end select
            !------------------------------------------------------------------------------!



            !------------------------------------------------------------------------------!
            !      Photorespiration can become important at high temperatures.  If so,     !
            ! close down the stomata.                                                      !
            !------------------------------------------------------------------------------!
            if (cpatch%A_open(ico) < cpatch%A_closed(ico)) then
               cpatch%fs_open(ico) = 0.0
            else
               cpatch%fs_open(ico) = min(cpatch%fsw(ico),cpatch%fsn(ico)) 
             !   cpatch%fs_open(ico) = cpatch%fsw(ico) * cpatch%N_limitation_factor_bar(ico) !JL!
            end if
           !----- Net stomatal conductance. ----------------------------------------------!

            cpatch%stomatal_conductance(ico) =  cpatch%fs_open(ico) *cpatch%gsw_open(ico)  &
                                             + (1.0 - cpatch%fs_open(ico))                 &
                                             * cpatch%gsw_closed(ico)

            !----- GPP, averaged over frqstate. -------------------------------------------!
            cpatch%gpp(ico)       = cpatch%lai(ico)                                        &
                                  * ( cpatch%fs_open(ico) * cpatch%A_open(ico)             &
                                    + (1.0 - cpatch%fs_open(ico)) * cpatch%A_closed(ico) ) &
                                  + cpatch%leaf_respiration(ico)
            cpatch%mean_gpp(ico)  = cpatch%mean_gpp(ico) + cpatch%gpp(ico)

            !----- GPP, summed over 1 day. [�mol/m�ground] --------------------------------!
            cpatch%today_gpp(ico) = cpatch%today_gpp(ico) + cpatch%gpp(ico)

            !----- Potential GPP if no N limitation. [�mol/m�ground] ----------------------!
            cpatch%today_gpp_pot(ico) = cpatch%today_gpp_pot(ico)                          &
                                      + cpatch%lai(ico)                                    &
                                      * ( cpatch%fsw(ico) * cpatch%A_open(ico)             &
                                       + (1.0 - cpatch%fsw(ico)) * cpatch%A_closed(ico))  &
                                      + cpatch%leaf_respiration(ico)

            !----- Maximum GPP if at the top of the canopy [�mol/m�ground] ----------------!
            cpatch%today_gpp_max(ico) = cpatch%today_gpp_max(ico)                          &
                                      + cpatch%lai(ico)                                    &
                                      * ( cpatch%fs_open(ico) * csite%A_o_max(ipft,ipa)    &
                                        + (1.0 - cpatch%fs_open(ico))                      &
                                          * csite%A_c_max(ipft,ipa))                       &
                                      + cpatch%leaf_respiration(ico)

      else
         !----- If the cohort wasn't solved, we must assign some zeroes. ------------------!
         cpatch%A_open(ico)               = 0.0
         cpatch%A_closed(ico)             = 0.0
         cpatch%psi_open(ico)             = 0.0
         cpatch%psi_closed(ico)           = 0.0
         cpatch%water_supply(ico)         = 0.0
         cpatch%gsw_open(ico)             = 0.0
         cpatch%gsw_closed(ico)           = 0.0
         cpatch%leaf_gbh(ico)             = 0.0
         cpatch%leaf_gbw(ico)             = 0.0
         cpatch%stomatal_conductance(ico) = 0.0
         cpatch%gpp(ico)                  = 0.0
         cpatch%leaf_respiration(ico)     = 0.0
         vm                               = 0.0
         limit_flag                       = 0
      end if
      
      !------------------------------------------------------------------------------------!
      !    Not really a part of the photosynthesis scheme, but this will do it.  We must   !
      ! integrate the "mean" of the remaining respiration terms, except for the root one.  !
      ! This is done regardless on whether the cohort is doing photosynthesis.  Also, we   !
      ! convert units so all fast respiration terms are in [�mol/m�ground/s].              !
      !------------------------------------------------------------------------------------!
      cpatch%mean_growth_resp (ico) = cpatch%mean_growth_resp (ico)                        &
                                    + cpatch%growth_respiration (ico) * kgCday_2_umols     &
                                    * cpatch%nplant(ico)
      cpatch%mean_storage_resp(ico) = cpatch%mean_storage_resp(ico)                        &
                                    + cpatch%storage_respiration(ico) * kgCday_2_umols     &
                                    * cpatch%nplant(ico)
      cpatch%mean_vleaf_resp  (ico) = cpatch%mean_vleaf_resp  (ico)                        &
                                    + cpatch%vleaf_respiration  (ico) * kgCday_2_umols     &
                                    * cpatch%nplant(ico)                                    
      !------------------------------------------------------------------------------------!

      if (print_photo_debug) then
         call print_photo_details(cmet,csite,ipa,ico,limit_flag,vm,compp)
      end if
   end do cohortloop

   !---------------------------------------------------------------------------------------!
   !     Add the contribution of this time step to the average available water.            !
   !---------------------------------------------------------------------------------------!
   if (broot_tot > 1.e-20) then
      csite%avg_available_water(ipa) = csite%avg_available_water(ipa)                      &
                                     + pss_available_water / broot_tot
   !else
   !  Add nothing, the contribution of this time is zero since no cohort can transpire... 
   end if

   return
end subroutine canopy_photosynthesis
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
!     This sub-routine prints some extra information on the photosynthesis driver in a     !
! convenient ascii file for debugging purposes.                                            !
!------------------------------------------------------------------------------------------!
subroutine print_photo_details(cmet,csite,ipa,ico,limit_flag,vm,compp)
   use ed_max_dims    , only : str_len            ! ! intent(in)
   use ed_state_vars  , only : sitetype           & ! structure
                             , patchtype          ! ! structure
   use met_driver_coms, only : met_driv_state     ! ! structure
   use physiology_coms, only : photo_prefix       ! ! intent(in)
   use ed_misc_coms   , only : current_time       ! ! intent(in)
   use consts_coms    , only : Watts_2_Ein        & ! intent(in)
                             , mol_2_umol         & ! intent(in)
                             , t008               ! ! intent(in)
   use pft_coms       , only : quantum_efficiency & ! intent(in)
                             , photosyn_pathway   ! ! intent(in)
   use physiology_coms, only : quantum_efficiency_T ! ! intent(in)
   
   implicit none
   !----- Arguments. ----------------------------------------------------------------------!
   type(sitetype)            , target      :: csite           ! Current site
   type(met_driv_state)      , target      :: cmet            ! Current met. conditions.
   integer                   , intent(in)  :: ipa             ! Current patch number
   integer                   , intent(in)  :: ico             ! Current cohort number
   integer                   , intent(in)  :: limit_flag      ! Limitation flag
   real                      , intent(in)  :: vm              ! Maximum Rubisco capacity
   real                      , intent(in)  :: compp           ! GPP compensation point
   !----- Local variables. ----------------------------------------------------------------!
   type(patchtype)           , pointer     :: jpatch          ! Current site
   type(patchtype)           , pointer     :: cpatch          ! Current site
   character(len=str_len)                  :: photo_fout      ! File with the cohort info
   integer                                 :: ipft
   integer                                 :: jpa
   integer                                 :: jco
   logical                                 :: isthere
   real                                    :: leaf_resp
   real                                    :: stom_condct
   real                                    :: par_area
   real                                    :: parv
   real                                    :: util_parv
   real                                    :: alpha
   !----- Local constants. ----------------------------------------------------------------!
   character(len=10), parameter :: hfmt='(58(a,1x))'
   character(len=48), parameter :: bfmt='(3(i13,1x),1(es13.6,1x),2(i13,1x),52(es13.6,1x))'
   !----- Locally saved variables. --------------------------------------------------------!
   logical                   , save        :: first_time=.true.
   !---------------------------------------------------------------------------------------!


   !----- Make some aliases. --------------------------------------------------------------!
   cpatch      => csite%patch(ipa)
   ipft        =  cpatch%pft(ico)
   leaf_resp   =  cpatch%leaf_respiration(ico)
   stom_condct =  cpatch%stomatal_conductance(ico)
   !---------------------------------------------------------------------------------------!

   if (cpatch%leaf_resolvable(ico)) then
      par_area   = cpatch%par_l(ico) * Watts_2_Ein * mol_2_umol
      parv       = par_area / cpatch%lai(ico)
      
      
      !------------------------------------------------------------------------------------!
      !    Is alpha (quantum efficiency) temperature dependent?  If so, calculate after    !
      !    Ehlringer and Ollebjorkman 1977, if not use default value from ed_params                                                   !
      !------------------------------------------------------------------------------------!
      select case(quantum_efficiency_T)
      case(1)
           select case (photosyn_pathway(ipft))
           case (4)
               alpha         = dble(quantum_efficiency(ipft))       
           case (3)       
               alpha         = dble(-0.0016*(dble(cpatch%leaf_temp(ico))-t008)+0.1040)
           end select
      case default
            alpha         = dble(quantum_efficiency(ipft))      
      end select
      
      util_parv  = alpha * parv
   else
      par_area  = 0.0
      parv      = 0.0
      util_parv = 0.0
   end if

   !---------------------------------------------------------------------------------------!
   !     First time here.  Delete all files.                                               !
   !---------------------------------------------------------------------------------------!
   if (first_time) then
      do jpa = 1, csite%npatches
         jpatch => csite%patch(jpa)
         do jco = 1, jpatch%ncohorts
            write (photo_fout,fmt='(a,2(a,i4.4),a)')                                       &
                  trim(photo_prefix),'patch_',jpa,'_cohort_',jco,'.txt'
            inquire(file=trim(photo_fout),exist=isthere)
            if (isthere) then
               !---- Open the file to delete when closing. --------------------------------!
               open (unit=57,file=trim(photo_fout),status='old',action='write')
               close(unit=57,status='delete')
            end if
         end do
      end do
      first_time = .false.
   end if
   !---------------------------------------------------------------------------------------!




   !----- Create the file name. -----------------------------------------------------------!
   write (photo_fout,fmt='(a,2(a,i4.4),a)') trim(photo_prefix),'patch_',ipa                &
                                                              ,'_cohort_',ico,'.txt'
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !    Check whether the file exists or not.  In case it doesn't, create it and add the   !
   ! header.                                                                               !
   !---------------------------------------------------------------------------------------!
   inquire(file=trim(photo_fout),exist=isthere)
   if (.not. isthere) then
      open  (unit=57,file=trim(photo_fout),status='replace',action='write')
      write (unit=57,fmt=hfmt)   '         YEAR', '        MONTH', '          DAY'         &
                               , '         TIME', '          PFT', '   LIMIT_FLAG'         &
                               , '       HEIGHT', '       NPLANT', '        BLEAF'         &
                               , '          LAI', '    LEAF_HCAP', '   LEAF_WATER'         &
                               , '    LEAF_TEMP', '    WOOD_TEMP', '     CAN_TEMP'         &
                               , '     ATM_TEMP', '  GROUND_TEMP', '      CAN_SHV'         &
                               , '      ATM_SHV', '   GROUND_SHV', 'LSFC_SHV_OPEN'         &
                               , 'LSFC_SHV_CLOS', '     LINT_SHV', '     ATM_PRSS'         &
                               , '     CAN_PRSS', '         PCPG', '     CAN_RHOS'         &
                               , '      ATM_CO2', '      CAN_CO2', 'LSFC_CO2_OPEN'         &
                               , 'LSFC_CO2_CLOS', 'LINT_CO2_OPEN', 'LINT_CO2_CLOS'         &
                               , '        COMPP', '     PAR_AREA', '         PARV'         &
                               , '    UTIL_PARV', '          GPP', '    LEAF_RESP'         &
                               , '     LEAF_GBH', '     LEAF_GBW', '     WOOD_GBH'         &
                               , '     WOOD_GBW', '  STOM_CONDCT', '       A_OPEN'         &
                               , '       A_CLOS', '     GSW_OPEN', '     GSW_CLOS'         &
                               , '     PSI_OPEN', '     PSI_CLOS', '   H2O_SUPPLY'         &
                               , '          FSW', '          FSN', '      FS_OPEN'         &
                               , '     ATM_WIND', '     VEG_WIND', '        USTAR'         &
                               , '           VM'
                              
      close (unit=57,status='keep')
   end if
   !---------------------------------------------------------------------------------------!



   !---------------------------------------------------------------------------------------!
   !     Re-open the file at the last line, and include the current status.                !
   !---------------------------------------------------------------------------------------!
   open (unit=57,file=trim(photo_fout),status='old',action='write',position='append')
   write(unit=57,fmt=bfmt)                                                                 &
     current_time%year          , current_time%month         , current_time%date           &
   , current_time%time          , cpatch%pft(ico)            , limit_flag                  &
   , cpatch%hite(ico)           , cpatch%nplant(ico)         , cpatch%bleaf(ico)           &
   , cpatch%lai(ico)            , cpatch%leaf_hcap(ico)      , cpatch%leaf_water(ico)      &
   , cpatch%leaf_temp(ico)      , cpatch%wood_temp(ico)      , csite%can_temp(ipa)         &
   , cmet%atm_tmp               , csite%ground_temp(ipa)     , csite%can_shv(ipa)          &
   , cmet%atm_shv               , csite%ground_shv(ipa)      , cpatch%lsfc_shv_open(ico)   &
   , cpatch%lsfc_shv_closed(ico), cpatch%lint_shv(ico)       , cmet%prss                   &
   , csite%can_prss(ipa)        , cmet%pcpg                  , csite%can_rhos(ipa)         &
   , cmet%atm_co2               , csite%can_co2(ipa)         , cpatch%lsfc_co2_open(ico)   &
   , cpatch%lsfc_co2_closed(ico), cpatch%lint_co2_open(ico)  , cpatch%lint_co2_closed(ico) &
   , compp                      , par_area                   , parv                        &
   , util_parv                  , cpatch%gpp(ico)            , leaf_resp                   &
   , cpatch%leaf_gbh(ico)       , cpatch%leaf_gbw(ico)       , cpatch%wood_gbh(ico)        &
   , cpatch%wood_gbw(ico)       , stom_condct                , cpatch%A_open(ico)          &
   , cpatch%A_closed(ico)       , cpatch%gsw_open(ico)       , cpatch%gsw_closed(ico)      &
   , cpatch%psi_open(ico)       , cpatch%psi_closed(ico)     , cpatch%water_supply(ico)    &
   , cpatch%fsw(ico)            , cpatch%fsn(ico)            , cpatch%fs_open(ico)         &
   , cmet%vels                  , cpatch%veg_wind(ico)       , csite%ustar(ipa)            &
   , vm
   

   close(unit=57,status='keep')
   !---------------------------------------------------------------------------------------!

   return
end subroutine print_photo_details
!==========================================================================================!
!==========================================================================================!
