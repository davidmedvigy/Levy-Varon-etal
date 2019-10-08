!==========================================================================================!
!==========================================================================================!
module growth_balive
   !=======================================================================================!
   !=======================================================================================!


   contains



   !=======================================================================================!
   !=======================================================================================!
   !     This subroutine will update the alive biomass, and compute the respiration terms  !
   ! other than leaf respiration.                                                          !
   ! IMPORTANT: The order of the operations here affect the C/N budgets, so don't change   !
   !            the order of the operations unless you really know what you are doing.     !
   !---------------------------------------------------------------------------------------!
   subroutine dbalive_dt(cgrid, tfact)
      use ed_state_vars    , only : edtype                 & ! structure
                                  , polygontype            & ! structure
                                  , sitetype               & ! structure
                                  , patchtype              ! ! structure
      use pft_coms         , only : q                      & ! intent(in)
                                  , qsw                    & ! intent(in)
                                  , plant_N_supply_scale   & ! intent(in)
                                  , c2n_storage            & ! intent(in)
                                  , c2n_leaf               &  ! intent(in)             !JL!   
                                  , c2n_stem               & ! intent(in)              !JL!                
                                  , growth_resp_factor     & ! intent(in)
                                  , storage_turnover_rate  & ! intent(in)
                                  , phenology              ! ! intent(in)
      use physiology_coms  , only : N_plant_lim            ! ! intent(in)
      use grid_coms        , only : nzg                    ! ! intent(in)
      use ed_therm_lib     , only : calc_veg_hcap          & ! function
                                  , update_veg_energy_cweh ! ! function
      use allometry        , only : area_indices           & ! subroutine
                                  , ed_biomass             &! ! function
                                  , dbh2bl                 ! !JL
      use mortality        , only : mortality_rates        ! ! subroutine
      use phenology_coms   , only : theta_crit             ! ! intent(in)
      use decomp_coms      , only : FSN_ndays_to_avg                                   !JL!
      use nutrient_constants, only: BNF_rate               & 
                                  , Cost_BNF               & 
                                  , Cost_fixer             & 
                                  , resorption_factor      &
                                  , nstorage_max_factor
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype)     , target     :: cgrid
      real             , intent(in) :: tfact
      !----- Local variables. -------------------------------------------------------------!
      type(polygontype), pointer    :: cpoly
      type(sitetype)   , pointer    :: csite
      type(patchtype)  , pointer    :: cpatch
      integer                       :: ipy
      integer                       :: isi
      integer                       :: ipa
      integer                       :: ico
      integer                       :: ipft
      real                          :: salloc
      real                          :: salloci
      real                          :: bl
      real                          :: br
      real                          :: daily_C_gain
      real                          :: carbon_balance
      real                          :: carbon_balance_pot
      real                          :: carbon_balance_max
      real                          :: balive_in
      real                          :: dndt
      real                          :: old_leaf_hcap
      real                          :: old_wood_hcap
      real                          :: nitrogen_uptake
      real                          :: N_uptake_pot
      real                          :: temp_dep
      real                          :: shadow_N_uptake2 !JL!
      real                          :: ObligateCost      !JL!
      real                          :: C_cost    !JL!
      real                          :: Nstorage_after_fixation !JL!
      real                          :: N_to_storage !JL!
      real                          :: N_fixed_Extra !JL!
      real                          :: Nstorage_before_fixation !JL!     
      real                          :: N  
      real                          :: i            !JL! 
      real                          :: nstorage_max !JL!
      real                          :: Nstorage_before_resorption !JL!
      real                          :: Nstorage_after_resorption !JL
      real                          :: N_not_resorbed
      real                          :: N_resorbed
      real                          :: total_N_supply !JL!
      real                          ::  DailyForestFixation !JL!
      real                          :: DailyForestNDemand !JL!
      real                          :: DailyForestNSupply !JL!

      !------------------------------------------------------------------------------------!

      do ipy = 1,cgrid%npolygons
         cpoly => cgrid%polygon(ipy)

         do isi = 1,cpoly%nsites
            csite => cpoly%site(isi)

            do ipa = 1,csite%npatches
               cpatch => csite%patch(ipa)

               !----- Reset averaged variables. -------------------------------------------!
               csite%total_plant_nitrogen_uptake(ipa) = 0.0
               !----- Loop over cohorts. --------------------------------------------------!
               do ico = 1,cpatch%ncohorts

                !----- Alias for current PFT. -------------------------------------------!
                  ipft = cpatch%pft(ico)

                  !----- Update the elongation factor. ------------------------------------!
                  select case (phenology(ipft))
                  case (4)
                     cpatch%elongf(ico) = max(0.0, min(1.0, cpatch%paw_avg(ico)            &
                                                          / theta_crit))
                  case default
                     cpatch%elongf(ico) = 1.0

                  end select


                  !----- Initialize cohort nitrogen uptake. -------------------------------!
                  nitrogen_uptake = 0.0
                  N_uptake_pot    = 0.0
                  shadow_N_uptake2= 0.0 !JL!
                  cpatch%shadow_N_uptake(ico) = 0.0 !JL!

                  !----- Set allocation factors. ------------------------------------------!
                  salloc  = 1.0 + qsw(ipft) * cpatch%hite(ico) + q(ipft)
                  salloci = 1.0 / salloc

                  !------------------------------------------------------------------------!
                  !     Compute maintenance costs using actual pools.                      !
                  !------------------------------------------------------------------------!
                  call plant_maintenance(cpatch,ico,cpatch%broot(ico),cpatch%bleaf(ico)    &
                                        ,tfact,daily_C_gain,csite%avg_daily_temp(ipa))




                 !----- Subtract maintenance costs from pools. ---------------------------!
                  cpatch%balive(ico)    = cpatch%balive(ico)                               &
                                        - cpatch%leaf_maintenance(ico)                     &
                                        - cpatch%root_maintenance(ico)
                  cpatch%bleaf(ico)     = cpatch%bleaf(ico)                                &
                                        - cpatch%leaf_maintenance(ico)       
                  cpatch%broot(ico)     = cpatch%broot(ico)                                &
                                        - cpatch%root_maintenance(ico)
                  cpatch%cb(13,ico)     = cpatch%cb(13,ico)                                &
                                        - cpatch%leaf_maintenance(ico)                     &
                                        - cpatch%root_maintenance(ico)
                  cpatch%cb_max(13,ico) = cpatch%cb_max(13,ico)                            &
                                        - cpatch%leaf_maintenance(ico)                     &
                                        - cpatch%root_maintenance(ico)

                  !------------------------------------------------------------------------!
                  !     The commented line is an experimental and arbitrary test, borrowed !
                  ! from maintainence temperature dependency. [[MCD]]                      !
                  !------------------------------------------------------------------------!
                  ! temp_dep = 1.0                                                         &
                  !          / ( 1.0  + exp( 0.4 * (278.15 - csite%avg_daily_temp(ipa))))
                  temp_dep = 1.0
                  !------------------------------------------------------------------------!

                  cpatch%storage_respiration(ico) = cpatch%bstorage(ico)                   &
                                                  * storage_turnover_rate(ipft)            &
                                                  * tfact * temp_dep

                  cpatch%bstorage(ico) = cpatch%bstorage(ico)                              &
                                         - cpatch%storage_respiration(ico)

                  !------------------------------------------------------------------------!
                  !      Calculate actual, potential and maximum carbon balances.          !
                  !      Keep track of carbon availabe for growth in order to use for N    !
                  !      calclations                                                       !
                  !------------------------------------------------------------------------!
                  call plant_carbon_balances(cpatch,ipa,ico,daily_C_gain,carbon_balance    &
                                            ,carbon_balance_pot,carbon_balance_max)
                     cpatch%carbon_balance(ico) = carbon_balance !JL!

                  !------------------------------------------------------------------------!

                  !------------------------------------------------------------------------! 
                  !      Compute respiration rates for coming day [kgC/plant/day].         !
                  !------------------------------------------------------------------------!
                  cpatch%growth_respiration(ico) = max(0.0, daily_C_gain                   &
                                                          * growth_resp_factor(ipft))
                  !------------------------------------------------------------------------!



                  !------------------------------------------------------------------------!
                  !     Find the "virtual" leaf respiration.                               !
                  !------------------------------------------------------------------------!
                  cpatch%vleaf_respiration(ico) = (1.0-cpoly%green_leaf_factor(ipft,isi))  &
                                                * salloci * cpatch%balive(ico)             &
                                                * storage_turnover_rate(ipft)              &
                                                * tfact * temp_dep
                  !------------------------------------------------------------------------!


                  !------------------------------------------------------------------------!
                  !Define maximum Nstorage and update N storage pool now that maintence    !
                  !costs are known. Resorb nitrogen from leaf turnover. Fast N input       !
                  !through mortality or disturbance does not get resorbed.                 !
                  !Once the Nstorage max is reached put the rest of the N into FSN_in      !         
                  !------------------------------------------------------------------------! 

                  !Define maximum Nstorage 
                   nstorage_max = dbh2bl(cpatch%dbh(ico),ipft)/c2n_leaf(ipft) *nstorage_max_factor
                   Nstorage_before_resorption = cpatch%nstorage(ico)

                  ! Resorbing leaf N 
                  if (cpatch%nstorage(ico) <  nstorage_max) then                                
                     cpatch%nstorage(ico) = min((cpatch%nstorage(ico) +                     &
                                            ((cpatch%leaf_maintenance(ico)                  &
                                          / c2n_leaf(ipft))*resorption_factor)),nstorage_max)
                  endif
                  !------------------------------------------------------------------------!

                  !------------------------------------------------------------------------! 
                  ! Add unresorbed N that would have exceeded nstorage_max to the fast     !  
                  ! liter pool.The fraction of N that is not resorbed will be added to     !
                  ! fsn_in in the litter subroutine.                                        !     
                  !------------------------------------------------------------------------!
                  !Reset for each cohort
                  N_resorbed = 0
                  N_not_resorbed = 0

                  Nstorage_after_resorption = cpatch%nstorage(ico) 
                  N_resorbed = Nstorage_after_resorption - Nstorage_before_resorption

                  ! Adding unresorbed N to the litter pool
                  N_not_resorbed = (((cpatch%leaf_maintenance(ico)                         &
                                 + cpatch%root_maintenance(ico))/ c2n_leaf(ipft))*resorption_factor)    &
                                 -  N_resorbed  

                  csite%fsn_in(ipa) = csite%fsn_in(ipa)                                    &
                                    + N_not_resorbed * cpatch%nplant(ico)
                  !------------------------------------------------------------------------!


                  !------------------------------------------------------------------------!
                  !    Shadow calculation for Nitrogen Uptake. Calculate the amount of N   ! 
                  !    the plant would like to use given the amount of carbon it has       !
                  !    based on the carbon_balance term. This is a copy of the             !
                  !    alloc_plant_c_balance subroutine without updating any carbon or     !
                  !   nitrogen pools                                                       ! 
                  !------------------------------------------------------------------------!
                  balive_in = cpatch%balive(ico)                                   
                  call shadow_n(csite,ipa,ico,salloc,salloci,carbon_balance   &
                                            ,shadow_N_uptake2                               &
                                            ,cpoly%green_leaf_factor(ipft,isi))
                   cpatch%shadow_N_uptake(ico) = shadow_N_uptake2

                  !------------------------------------------------------------------------!
                 
                  !------------------------------------------------------------------------!
                  !     Do a shadow calculation to see what would have happened if stomata !
                  ! were open.  This is used to calculate potential nitrogen uptake,       !
                  ! N_uptake_pot.                                                          !
                  !------------------------------------------------------------------------!
                  if (N_plant_lim == 1) then
                     call potential_N_uptake(cpatch,ico,salloc,salloci,balive_in           &
                                            ,carbon_balance_pot,N_uptake_pot               &
                                            ,cpoly%green_leaf_factor(ipft,isi))
                 
                  cpatch%carbon_balance_pot(ico) = carbon_balance_pot !JL!
                  cpatch%N_uptake_pot(ico) = N_uptake_pot !JL!

                  end if

                  !------------------------------------------------------------------------!

                  !------------------------------------------------------------------------!  
                  !      Calculate plant N limitation factor, N fixation,                  !
                  !      actual nitrogen uptake constrained by soil supply                 !
                  !      and excess carbon assimilated. Excess carbon is carbon that was   !
                  !      taken up during photosynthesis but cannot be used due low N supply!
                  !------------------------------------------------------------------------! 
                
                  !------------------------------------------------------------------------!  
                  !Initialize Nitrogen parameters to 0                                     ! 
                  !------------------------------------------------------------------------!  
                  cpatch%excess_carbon(ico) = 0.0 
                  cpatch%excess_carbon_fixer(ico) = 0.0                   
                  cpatch%actual_nitrogen_uptake(ico) = 0.0
                  cpatch%actual_nitrogen_uptake_fixer(ico) = 0.0
                  cpatch%fixation_demand(ico) = 0.0
                  cpatch%N_fixation(ico) = 0.0 
                  cpatch%nitrogen_supply(ico) = 0.0 
                  cpatch%nitrogen_supply_fixer(ico) = 0.0 
                  cpatch%N_fixation(ico) = 0.0 
                  !------------------------------------------------------------------------!      

                  !------------------------------------------------------------------------!  
                  !If plants are not Nitrogen limited set fsn = 1. fsn = nitrogen limitation!
                  !and affects stomatal apeture the next day.                          ! 
                  !If there's no limitation then excess_carbon = 0                         !
                  !------------------------------------------------------------------------!               
                  if (n_plant_lim == 0 .or.  N_uptake_pot <= 0.0) then 
                    ! PFT 31 is the obligate fixer
                     if(ipft .ne. 31) then
                     cpatch%fsn(ico) = 1.0

                     !calculate how much soil N the plant has access to 
		     cpatch%nitrogen_supply(ico) = (cpatch%broot(ico)                      &
                                                 / sum(cpatch%broot * cpatch%nplant))      &
                                                 * csite%mineralized_soil_N(ipa)

                     ! Calculate how much soil N the plant takes up from the soil for growth
		     cpatch%actual_nitrogen_uptake(ico) = min(cpatch%nitrogen_supply(ico)  &
                                                        ,cpatch%shadow_N_uptake(ico))  
                     endif

                  !------------------------------------------------------------------------! 

                  !------------------------------------------------------------------------! 
                  ! If limitation is turned on and plants are non-fixers calculate nitrogen!
                  !supply, actual nitrogen uptake, excess carbon, and nitrogen limitation  !
                  ! factor. Excess carbon is added to fsn_in in the litter subroutine.      !
                  !------------------------------------------------------------------------! 
                  ! PFT 30 and 31 are fixers 
                  else if (ipft .ne. 30 .and. ipft .ne. 31)then 
   
                    !Calculate how much soil N the plant has access to 
		    cpatch%nitrogen_supply(ico) = (cpatch%broot(ico)                        &
                                                / sum(cpatch%broot * cpatch%nplant))        &
				         	* csite%mineralized_soil_N(ipa)	
		
                   ! Calculate how much soil N the plant takes up from the soil for growth
		    cpatch%actual_nitrogen_uptake(ico) = min( cpatch%nitrogen_supply(ico)   &
                                                       , cpatch%shadow_N_uptake(ico))           		  
 
                   ! Calculate how much carbon cannot be used due to N limitation
                    if ( cpatch%shadow_N_uptake(ico) >  cpatch%actual_nitrogen_uptake(ico)  &
                    .and. cpatch%phenology_status(ico) == 1 ) then
 
                           cpatch%excess_carbon(ico) = ( cpatch%shadow_N_uptake(ico)        &
                                                     - cpatch%actual_nitrogen_uptake(ico))  &
                                                     * c2n_leaf(ipft) 
                    endif

                   cpatch%fsn(ico) =  cpatch%nitrogen_supply(ico)                           &
                                   / (cpatch%nitrogen_supply(ico)                           &
                                   +  cpatch%N_uptake_pot(ico))  
                                  
                  !------------------------------------------------------------------------!                  

                  !------------------------------------------------------------------------! 
                  ! If plants are facultative-fixers (PFT 30) calculate nitrogen supply,    !
                  ! actual nitrogen uptake, and excess carbon                              !
                  !------------------------------------------------------------------------! 
                   else if (ipft == 30) then  
 
                      !Calculate how much soil N the plant has access to 
                      cpatch%nitrogen_supply(ico) =  (cpatch%broot(ico)                    &
                                              / sum(cpatch%broot * cpatch%nplant))         &
                                              * csite%mineralized_soil_N(ipa)           
 

                      !Calculate how much soil N the plant takes up from the soil for growth. 
                      !Soil N is used first and then N from fixation (caluculated below)

                      cpatch%actual_nitrogen_uptake(ico) = min( cpatch%nitrogen_supply(ico) &
                                                     , cpatch%shadow_N_uptake(ico))    		

                      ! Calculate how much carbon cannot be used due to N limitation if only
                      !considering soil N available
                      if ( cpatch%shadow_N_uptake(ico) > cpatch%actual_nitrogen_uptake(ico) &
                           .and. cpatch%phenology_status(ico) == 1 ) then
                               
                             cpatch%excess_carbon(ico) = (cpatch%shadow_N_uptake(ico)       &
                                  -  cpatch%actual_nitrogen_uptake(ico))                    &
                                  * c2n_leaf(ipft)  

                      endif
       
                  !------------------------------------------------------------------------! 
                  ! Calculate Biological Nitrogen Fixation                                 !
                  !------------------------------------------------------------------------!        

                      ! Define carbon costs for BNF. Cost_fixer = penalty for being a fixer. 
                      ! Cost_BNF is metabolic cost of carrying out fixation and maintaining 
                      ! root nodules  
                  
                      C_cost = Cost_fixer + Cost_BNF ! #kg C kg N-1


                     ! Calculate fixation demand and amount of fixation
                     !------------------------------------------------------------------------!   
                     ! If the plant is in postive carbon balance and if actual_nitrogen_uptake 
                     ! (from soil) does not satisfy N demand then Fixation demand is the amount
                     ! of nitrogen the plant would fix (kg N) based on the daily carbon_balance
                     ! taking into account the carbon cost of fixation.
                     ! This uses the potential gain in C from doing 
                     ! additonal fixation (numerator) divided by the cost and c2n raio of the 
                     !plant structure that the nitrogen would be going to (depends on phenology)
                     !------------------------------------------------------------------------!   
                     if (cpatch%carbon_balance(ico) > 0                                     &
                     .and. cpatch%shadow_N_uptake(ico) > cpatch%actual_nitrogen_uptake(ico) &
                     .and. cpatch%phenology_status(ico) == 1) then                                                                                                      
                        cpatch%fixation_demand(ico) = cpatch%excess_carbon(ico)             &
                                                 /(C_cost + c2n_leaf(ipft))                                                            


                        ! Calculate Biological Nitrogen Fixation. This is limited by the rate of
                        ! fixation min (i.e. max_fixation_per_day,demand)

                       cpatch%N_fixation(ico) =  min(((cpatch%BSTORAGE(ico) +cpatch%BDEAD(ico) &
                                            + cpatch%BALIVE(ico))*2*BNF_rate)                  &
                                            , cpatch%fixation_demand(ico))
                     ! if not enough carbon or no demand, no fixation
                     else                               
                           cpatch%N_fixation(ico) = 0
                     endif

  
                   ! Plant N uptake equals nitrogen from soil + nitrogen from fixation
                     cpatch%actual_nitrogen_uptake(ico) = cpatch%actual_nitrogen_uptake(ico)   &
                                                          +  cpatch%N_fixation(ico) 
                                                                           
                  ! Recalculate excess C based on additional N acquired by fixation
                     if (cpatch%shadow_N_uptake(ico) >  cpatch%actual_nitrogen_uptake(ico)  &
                     .and. cpatch%phenology_status(ico) == 1 ) then
                         
                      cpatch%excess_carbon(ico) = (cpatch%shadow_N_uptake(ico)              & 
                                                - cpatch%actual_nitrogen_uptake(ico)) 	    &
				            	* c2n_leaf(ipft)  
                     endif


                  ! Calculate N limitation factor: fsn based on N supply from soil +
                  ! N available from fixation
              
                  total_N_supply =  (cpatch%broot(ico)                                     &
                                              / sum(cpatch%broot * cpatch%nplant))         &
                                              * csite%mineralized_soil_N(ipa)              &
                                              + ((cpatch%BSTORAGE(ico)+cpatch%BDEAD(ico)   &
                                              + cpatch%BALIVE(ico))*2*BNF_rate)
                  

                  cpatch%fsn(ico) =  total_N_supply                                        &
                                  / (total_N_supply                                        &
                                  + cpatch% N_uptake_pot(ico))

                  ! Reset cpatch%nitrogen_supply(ico) to total N supply because this value
                  ! is used below to calculate how much N the plant can put into nstorage
                  ! and it is used in the structural growth file to calculate how much 
                  ! structural growth and reproduction can occur.               

                  cpatch%nitrogen_supply(ico) =   total_N_supply
                 
                  !------------------------------------------------------------------------! 
                  ! If plants are obligate-fixers (PFT 31) calculate nitrogen supply, actual!
                  ! nitrogen uptake, biological nitrogen fixation, excess carbon, and      !
                  ! nitrogen limitation factor                                             !
                  !------------------------------------------------------------------------! 

                  else if (ipft == 31)then 

                  !Calculate N fixed if plant fixed all day. Dependent on tree biomass * 
                  ! fixation rate
                  cpatch%N_fixation(ico) =  ((cpatch%BSTORAGE(ico) +cpatch%BDEAD(ico)       &
                                            + cpatch%BALIVE(ico))*2*BNF_rate) 
          

                 ! The fixation comes at a carbon cost. Calculate the carbon cost associated 
                 ! with the daily fixation. First set the cost to 0 for the day and then
                 ! calculate the Carbon used for fixation
  
                  ObligateCost = 0
                  ObligateCost =  cpatch%N_fixation(ico)* C_cost  ! [ kg C/plant]


                 ! Remove the Carbon used for fixation from the plant's carbon balance by
                 ! putting it into the excess C term. This term will be removed from the
                 ! carbon balance before the plant can use the carbon for growth 
                   
                  cpatch%excess_carbon(ico)  = ObligateCost


                  !Determine Nitrogen supply for plant. This will be used for the fsn 
                  !calculation. Nitrogen supply will be the plants access to N from soil
                  !and fixation.

                 
                  if(cpatch%broot(ico)<0) then
                   cpatch%broot(ico) = 0
                  endif              


                  !To calculate N limitation factor you need N supply from soil +N from fixation

                  total_N_supply              =  (cpatch%broot(ico)                        &
                                              / sum(cpatch%broot * cpatch%nplant))         &
                                              * csite%mineralized_soil_N(ipa)              &
                                              + ((cpatch%BSTORAGE(ico)+cpatch%BDEAD(ico)   &
                                              + cpatch%BALIVE(ico))*2*BNF_rate)


                  !Determine How much N the plant takes in (soil + fixation) and update
                  ! actual_Nitrogen uptake                 
                  

                    cpatch%actual_nitrogen_uptake(ico) = min(total_N_supply,               &
                                                         cpatch%shadow_N_uptake(ico)) 

                     if(cpatch%shadow_N_uptake(ico) == 0)then
                      cpatch%actual_nitrogen_uptake(ico) = cpatch%N_fixation(ico)
                     endif

                  
                  !If the soil and fixation do not meet the shadow N uptake add extra C
                  !to excess C pool. 
                 
                  if (cpatch%shadow_N_uptake(ico) >  cpatch%actual_nitrogen_uptake(ico)     &
                  .and. cpatch%phenology_status(ico) == 1 ) then

 
                    cpatch%excess_carbon(ico) = cpatch%excess_carbon(ico) +                  &
                           (cpatch%shadow_N_uptake(ico)- cpatch%actual_nitrogen_uptake(ico)) &
                              *c2n_leaf(ipft)  
    
                  endif

                  ! If fixation is greater then growth demand and nstorage is full, dump 
                  ! extra fixed N into the fast decomposing litter pool

                  if (cpatch%shadow_N_uptake(ico) < cpatch%N_fixation(ico)                &
                  .and. cpatch%nstorage(ico) ==  nstorage_max) then      

                  csite%fsn_in(ipa) = csite%fsn_in(ipa)                                    &
                                   + (cpatch%N_fixation(ico)- cpatch%shadow_N_uptake(ico)) &
                                     * cpatch%nplant(ico)

                  endif


                  ! If the plant fixs more N then its demand for growth add the excess N to
                  ! N storage until it reaches nstorage_max. Add any remaining N to fsn_in.
                  
                   Nstorage_before_fixation = 0
                   Nstorage_before_fixation =  cpatch%nstorage(ico)

                   if (cpatch%shadow_N_uptake(ico) < cpatch%N_fixation(ico)                &
                   .and. cpatch%nstorage(ico) <  nstorage_max)then
 

                       cpatch%nstorage(ico) = min(nstorage_max,cpatch%nstorage(ico)        &
                                   + (cpatch%N_fixation(ico) - cpatch%shadow_N_uptake(ico)))
 
                   !Determine how much N goes to fsn_in and add it to the fast pool
                   Nstorage_after_fixation = 0
                   N_to_storage = 0
                   N_fixed_Extra = 0 

                   Nstorage_after_fixation =  cpatch%nstorage(ico)
                   N_to_storage = Nstorage_after_fixation -  Nstorage_before_fixation
                   N_fixed_Extra = cpatch%N_fixation(ico)- cpatch%shadow_N_uptake(ico)   &
                                  - N_to_storage

                  csite%fsn_in(ipa) = csite%fsn_in(ipa) +  N_fixed_Extra* cpatch%nplant(ico)         
 
                   endif
                                
                  ! Calculate N limitation factor 
                  
                  cpatch%fsn(ico) =  total_N_supply                                       &
                                  / ( total_N_supply                                      &
                                  + cpatch%N_uptake_pot(ico))

                  ! Reset cpatch%nitrogen_supply(ico) to total N supply because this value
                  ! is used below to calculate how much N the plant can put into nstorage
                  ! and it is used in the structural growth file to calculate how much 
                  ! structural growth and reproduction can occur.    
 
                   cpatch%nitrogen_supply(ico) =   total_N_supply

                  end if 
                  !------------------------------------------------------------------------! 
 
                  !------------------------------------------------------------------------!
                  !      Allocate plant carbon balance to balive and bstorage.             !
                  !------------------------------------------------------------------------!
                  balive_in = cpatch%balive(ico)
                
                  !remove excess C from carbon balance before plant growth occurs
                  cpatch%carbon_balance(ico) =  cpatch%carbon_balance(ico)                 &
                                             - cpatch%excess_carbon(ico)!JL!
                  carbon_balance = carbon_balance - cpatch%excess_carbon(ico)!JL!

                  call alloc_plant_c_balance(csite,ipa,ico,salloc,salloci,carbon_balance   &
                                            ,nitrogen_uptake                               &
                                            ,cpoly%green_leaf_factor(ipft,isi))

                  !------------------------------------------------------------------------! 

                  !------------------------------------------------------------------------! 
                  ! Fill Nstorage to max_Nstorage if plant has access to more N then it    !
                  !needs for growth                                                        !
                  !------------------------------------------------------------------------!                       

                  ! Storage input will be the min N the plant needs to reach the storage max 
                  !or the amount of N the plant has access to after N uptake for growth.
  
                   if (cpatch%nitrogen_supply(ico) > cpatch%actual_nitrogen_uptake(ico)    &
                   .and. cpatch%nstorage(ico)< nstorage_max .and. carbon_balance>0) then

                        nitrogen_uptake =  nitrogen_uptake                                 &
                                        + min((nstorage_max-cpatch%nstorage(ico)),         &
                                        (cpatch%nitrogen_supply(ico)                       &
                                        - cpatch%actual_nitrogen_uptake(ico)))
                        cpatch%nstorage(ico) = cpatch%nstorage(ico)                        &
                                             +  min((nstorage_max-cpatch%nstorage(ico))    &
                                             ,(cpatch%nitrogen_supply(ico)                 &
                                             -cpatch%actual_nitrogen_uptake(ico)))
                   endif

                  !------------------------------------------------------------------------! 
                  !------------------------------------------------------------------------! 
                  ! Calculate the total N taken up by all plant in each patch [kgN/m2].    !
                  ! This will be subtracted from the minearlized soil N pool in the        !
                  ! soil_respiration.f90 file                                              !
                  !------------------------------------------------------------------------!
                  csite%total_plant_nitrogen_uptake(ipa) =                                 &
                                       csite%total_plant_nitrogen_uptake(ipa)              &
                                       + nitrogen_uptake * cpatch%nplant(ico)  
                  !------------------------------------------------------------------------! 
                  ! Calculate Ecosystem N fixation [kgN/m2]                                !
                  !------------------------------------------------------------------------!

                  DailyForestFixation = DailyForestFixation + cpatch%N_fixation(ico) *     &
                                        csite%area(ipa) * cpatch%nplant(ico)
                  !------------------------------------------------------------------------!

                  !------------------------------------------------------------------------!
                  !      Do mortality --- note that only frost mortality changes daily.    !
                  !------------------------------------------------------------------------!
                  call mortality_rates(cpatch,ipa,ico,csite%avg_daily_temp(ipa)            &
                                      ,csite%age(ipa))
                  dndt = - sum(cpatch%mort_rate(:,ico)) * cpatch%nplant(ico) * tfact

                  !------- Update monthly mortality rate [plants/m2/month]. ---------------!
                  cpatch%monthly_dndt(ico) = cpatch%monthly_dndt(ico) + dndt

              
                   !----- Updating LAI, WPA, and WAI. --------------------------------------!
                  call area_indices(cpatch%nplant(ico),cpatch%bleaf(ico)                   &
                                   ,cpatch%bdead(ico),cpatch%balive(ico),cpatch%dbh(ico)   &
                                   ,cpatch%hite(ico) ,cpatch%pft(ico),cpatch%sla(ico)      &
                                   ,cpatch%lai(ico),cpatch%wpa(ico),cpatch%wai(ico)        &
                                   ,cpatch%crown_area(ico),cpatch%bsapwood(ico))

                  !----- Update above-ground biomass. -------------------------------------!
                  cpatch%agb(ico) = ed_biomass(cpatch%bdead(ico),cpatch%balive(ico)        &
                                              ,cpatch%bleaf(ico),cpatch%pft(ico)           &
                                              ,cpatch%hite(ico),cpatch%bstorage(ico)       &
                                              ,cpatch%bsapwood(ico))

                  !------------------------------------------------------------------------!
                  !     It is likely that biomass has changed, therefore, update           !
                  ! vegetation energy and heat capacity.                                   !
                  !------------------------------------------------------------------------!
                  old_leaf_hcap         = cpatch%leaf_hcap(ico)
                  old_wood_hcap         = cpatch%wood_hcap(ico)

                  call calc_veg_hcap(cpatch%bleaf(ico) ,cpatch%bdead(ico)                  &
                                    ,cpatch%bsapwood(ico),cpatch%nplant(ico)               &
                                    ,cpatch%pft(ico)                                       &
                                    ,cpatch%leaf_hcap(ico),cpatch%wood_hcap(ico))

 
                  call update_veg_energy_cweh(csite,ipa,ico,old_leaf_hcap,old_wood_hcap)

                 !----- Update the stability status. -------------------------------------!
                  call is_resolvable(csite,ipa,ico,cpoly%green_leaf_factor(:,isi))

               end do
                   !----- Update litter. ----------------------------------------------------!
                    call litter(csite,ipa)
                   ! This is where the N that was not resorbed from leaves and fine
                   ! roots (52%) goes into the fsn_in pool JL
                   !------------------------------------------------------------------------!
                     
               !----- Update patch LAI, WAI, height, roughness... -------------------------!
               call update_patch_derived_props(csite,cpoly%lsl(isi),cpoly%met(isi)%prss,ipa)

               !----- Recalculate storage terms (for budget assessment). ------------------!
               call update_budget(csite,cpoly%lsl(isi),ipa,ipa)

               !----- It's a new day, reset average daily temperature. --------------------!
               csite%avg_daily_temp(ipa) = 0.0 
            end do
         end do
      end do
                  
      cgrid%total_N_Fixation(1)  = cgrid%total_N_Fixation(1) + DailyForestFixation !JL!
      DailyForestFixation= 0.
      return
   end subroutine dbalive_dt
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This subroutine will compute the respiration terms other than leaf                !
   ! respiration, plus the carbon balance and maintenance costs but without                !
   ! updating the pools.                                                                   !
   !---------------------------------------------------------------------------------------!
   subroutine dbalive_dt_eq_0(cgrid, tfact)
      use ed_state_vars   , only : edtype                 & ! structure
                                 , polygontype            & ! structure
                                 , sitetype               & ! structure
                                 , patchtype              ! ! structure
      use pft_coms        , only : q                      & ! intent(in)
                                 , qsw                    & ! intent(in)
                                 , plant_N_supply_scale   & ! intent(in)
                                 , c2n_storage            & ! intent(in)
                                 , growth_resp_factor     & ! intent(in)
                                 , storage_turnover_rate  & ! intent(in)
                                 , phenology              ! ! intent(in)
      use physiology_coms , only : N_plant_lim            ! ! intent(in)
      use grid_coms       , only : nzg                    ! ! intent(in)
      use ed_therm_lib    , only : calc_veg_hcap          & ! function
                                 , update_veg_energy_cweh ! ! function
      use allometry       , only : area_indices           & ! subroutine
                                 , ed_biomass             ! ! function
      use mortality       , only : mortality_rates        ! ! subroutine
      use phenology_coms  , only : theta_crit             ! ! intent(in)
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype)     , target     :: cgrid
      real             , intent(in) :: tfact
      !----- Local variables. -------------------------------------------------------------!
      type(polygontype), pointer    :: cpoly
      type(sitetype)   , pointer    :: csite
      type(patchtype)  , pointer    :: cpatch
      integer                       :: ipy
      integer                       :: isi
      integer                       :: ipa
      integer                       :: ico
      integer                       :: ipft
      real                          :: salloc
      real                          :: salloci
      real                          :: bl
      real                          :: br
      real                          :: daily_C_gain
      real                          :: carbon_balance
      real                          :: carbon_balance_pot
      real                          :: carbon_balance_max
      real                          :: balive_in
      real                          :: nitrogen_supply
      real                          :: dndt
      real                          :: old_leaf_hcap
      real                          :: old_wood_hcap
      real                          :: nitrogen_uptake
      real                          :: N_uptake_pot
      !------------------------------------------------------------------------------------!


      do ipy = 1,cgrid%npolygons
         cpoly => cgrid%polygon(ipy)

         do isi = 1,cpoly%nsites
            csite => cpoly%site(isi)

            do ipa = 1,csite%npatches
               cpatch => csite%patch(ipa)

               !----- Reset averaged variables. -------------------------------------------!
               csite%total_plant_nitrogen_uptake(ipa) = 0.0

               !----- Loop over cohorts. --------------------------------------------------!
               do ico = 1,cpatch%ncohorts

                  !----- Alias for current PFT. -------------------------------------------!
                  ipft = cpatch%pft(ico)

                  !----- Update the elongation factor. ------------------------------------!
                  select case (phenology(ipft))
                  case (4)
                     cpatch%elongf(ico) = max(0.0, min(1.0, cpatch%paw_avg(ico)/theta_crit))
                  case default
                     cpatch%elongf(ico) = 1.0
                  end select

                  !----- Initialize cohort nitrogen uptake. -------------------------------!
                  nitrogen_uptake = 0.0
                  N_uptake_pot    = 0.0

                  !----- Set allocation factors. ------------------------------------------!
                  salloc  = 1.0 + qsw(ipft) * cpatch%hite(ico) + q(ipft)
                  salloci = 1.0 / salloc
                  
                  !----- Leaf and root biomass. -------------------------------------------!
                  bl = cpatch%bleaf(ico)
                  br = cpatch%broot(ico)

                  !------------------------------------------------------------------------!
                  !     Compute maintenance costs.                                         !
                  !------------------------------------------------------------------------!
                  call plant_maintenance(cpatch,ico,br,bl,tfact,daily_C_gain               &
                                        ,csite%avg_daily_temp(ipa))

                  !----- Subtract maintenance costs from balive. --------------------------!
                  cpatch%cb(13,ico)     = cpatch%cb(13,ico)                                &
                                        - cpatch%leaf_maintenance(ico)                     &
                                        - cpatch%root_maintenance(ico)
                  cpatch%cb_max(13,ico) = cpatch%cb_max(13,ico)                            &
                                        - cpatch%leaf_maintenance(ico)                     &
                                        - cpatch%root_maintenance(ico)

                  !------------------------------------------------------------------------!
                  !      Calculate actual, potential and maximum carbon balances.          !
                  !------------------------------------------------------------------------!
                  call plant_carbon_balances(cpatch,ipa,ico,daily_C_gain,carbon_balance    &
                                            ,carbon_balance_pot,carbon_balance_max)

                  !------------------------------------------------------------------------!
                  !      Compute respiration rates for coming day [kgC/plant/day].         !
                  !------------------------------------------------------------------------!
                  cpatch%growth_respiration(ico)  = max(0.0, daily_C_gain                  &
                                                           * growth_resp_factor(ipft))
                  cpatch%storage_respiration(ico) = cpatch%bstorage(ico)                   &
                                                  * storage_turnover_rate(ipft) * tfact
                  cpatch%vleaf_respiration(ico) =                                          &
                                        (1.0 - cpoly%green_leaf_factor(ipft,isi))          &
                                      / (1.0 + q(ipft) + qsw(ipft) * cpatch%hite(ico))     &
                                      * cpatch%balive(ico) * storage_turnover_rate(ipft)   &
                                      * tfact

                  !------------------------------------------------------------------------!
                  !     Do a shadow calculation to see what would have happened if stomata !
                  ! were open.  This is used to calculate potential nitrogen uptake,       !
                  ! N_uptake_pot.                                                          !
                  !------------------------------------------------------------------------!
                  if (N_plant_lim == 1) then
                     call potential_N_uptake(cpatch,ico,salloc,salloci,balive_in           &
                                            ,carbon_balance_pot,N_uptake_pot               &
                                            ,cpoly%green_leaf_factor(ipft,isi))
                  end if

                  !------------------------------------------------------------------------!
                  !  Increment the [kgN/m2] taken up during previous day.                  !
                  !------------------------------------------------------------------------!
                 ! csite%total_plant_nitrogen_uptake(ipa) =                                 &
                 !                                 csite%total_plant_nitrogen_uptake(ipa)  &
                 !                              + nitrogen_uptake * cpatch%nplant(ico)

                  !----- Calculate plant N limitation factor. -----------------------------!
                  if (n_plant_lim == 0 .or. N_uptake_pot <= 0.0) then
                     cpatch%fsn(ico) = 1.0
                  else
                     nitrogen_supply = plant_N_supply_scale * br                           &
                                     * csite%mineralized_soil_N(ipa)
                     cpatch%fsn(ico) = nitrogen_supply / (nitrogen_supply + N_uptake_pot)
                  end if
                  
                  !------------------------------------------------------------------------!
                  !      Do mortality --- note that only frost mortality changes daily.    !
                  !------------------------------------------------------------------------!
                  call mortality_rates(cpatch,ipa,ico,csite%avg_daily_temp(ipa)            &
                                      ,csite%age(ipa))
               end do

               !----- It's a new day, reset average daily temperature. --------------------!
               csite%avg_daily_temp(ipa) = 0.0 
            end do
         end do
      end do

      return
   end subroutine dbalive_dt_eq_0
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !    This subroutine will transfer some of the stored carbon to balive in order to put  !
   ! the plant back on allometry.                                                          !
   !---------------------------------------------------------------------------------------!
   subroutine transfer_C_from_storage(cpatch,ico,salloc,salloci,nitrogen_uptake            &
                                     ,N_uptake_pot)
      use ed_state_vars , only : patchtype
      use pft_coms      , only : c2n_leaf    & ! intent(in)
                               , c2n_storage & ! intent(in)
                               , c2n_stem    & ! intent(in)
                               , q           & ! intent(in)
                               , qsw         ! ! intent(in)
      use decomp_coms   , only : f_labile    ! ! intent(in)
      use allometry     , only : dbh2bl      ! ! function
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(patchtype), target        :: cpatch
      integer        , intent(in)    :: ico
      real           , intent(in)    :: salloc
      real           , intent(in)    :: salloci
      real           , intent(inout) :: nitrogen_uptake
      real           , intent(inout) :: N_uptake_pot
      !----- Local variables. -------------------------------------------------------------!
      integer                        :: ipft
      real                           :: off_allometry_cb
      real                           :: increment
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !     Only do the transfer if leaves exist.                                          !
      !------------------------------------------------------------------------------------!
      if (cpatch%phenology_status(ico) == 2) return
     
      !----- Alias for pft type. ----------------------------------------------------------!
      ipft = cpatch%pft(ico)
     
      !----- Determine how much biomass we need to go back to allometry. ------------------!
      off_allometry_cb = dbh2bl(cpatch%dbh(ico),ipft) * salloc - cpatch%balive(ico)

      !----- If plants have storage, transfer it to balive. -------------------------------!
      increment            = max(0.0,min(max(0.0, off_allometry_cb),cpatch%bstorage(ico)))
      cpatch%balive(ico)   = cpatch%balive(ico)   + increment
      cpatch%bstorage(ico) = cpatch%bstorage(ico) - increment

      !----- Compute sapwood and fine root biomass. ---------------------------------------!
      cpatch%broot(ico)    = q(ipft) * cpatch%balive(ico) * salloci
      cpatch%bsapwood(ico) = qsw(ipft) * cpatch%hite(ico) * cpatch%balive(ico) * salloci

      !------------------------------------------------------------------------------------!
      !      N uptake is required since c2n_leaf < c2n_storage.  Units are kgN/plant/day.  !
      !------------------------------------------------------------------------------------!
      nitrogen_uptake = increment * (        f_labile(ipft)  / c2n_leaf(ipft)              &
                                    + (1.0 - f_labile(ipft)) / c2n_stem(ipft)              &
                                    -  1.0 / c2n_storage)
      N_uptake_pot    = nitrogen_uptake

      return
   end subroutine transfer_C_from_storage
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine plant_maintenance(cpatch,ico,br,bl,tfact,daily_C_gain,tempk)
      use ed_state_vars, only : patchtype          ! ! structure
      use pft_coms     , only : phenology          & ! intent(in)
                              , root_turnover_rate & ! intent(in)
                              , leaf_turnover_rate ! ! intent(in)
      use consts_coms  , only : umol_2_kgC         & ! intent(in)
                              , day_sec            ! ! intent(in)
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(patchtype), target        :: cpatch
      integer        , intent(in)    :: ico
      real           , intent(in)    :: br
      real           , intent(in)    :: bl
      real           , intent(in)    :: tfact
      real           , intent(in)    :: tempk
      real           , intent(out)   :: daily_C_gain
      !----- Local variables. -------------------------------------------------------------!
      integer                        :: ipft
      real                           :: maintenance_temp_dep
      !------------------------------------------------------------------------------------!

      !------ Alias for plant functional type. --------------------------------------------!
      ipft = cpatch%pft(ico)

      !------ Get the temperature dependence. ---------------------------------------------!
      if (phenology(ipft) == 0) then
         maintenance_temp_dep = 1.0 / (1.0 + exp(0.4 * (278.15 - tempk)))
      else
         maintenance_temp_dep = 1.0
      end if

      !----- Calculate maintenance demand (kgC/plant/year). -------------------------------!
      cpatch%root_maintenance(ico) = root_turnover_rate(ipft) * br * maintenance_temp_dep
      if (phenology(ipft) /= 3)then
         cpatch%leaf_maintenance(ico) = leaf_turnover_rate(ipft) * bl * maintenance_temp_dep
      else
         cpatch%leaf_maintenance(ico) = leaf_turnover_rate(ipft) * bl                      &
                                      * cpatch%turnover_amp(ico) * maintenance_temp_dep
      end if


      !----- Convert units of maintenance to [kgC/plant/day]. -----------------------------!
      cpatch%leaf_maintenance(ico) = cpatch%leaf_maintenance(ico) * tfact
      cpatch%root_maintenance(ico) = cpatch%root_maintenance(ico) * tfact


      !----- Compute daily C uptake [kgC/plant/day]. --------------------------------------!
      if(cpatch%nplant(ico) > tiny(1.0)) then
         daily_C_gain = umol_2_kgC * day_sec * ( cpatch%today_gpp(ico)                     &
                                               - cpatch%today_leaf_resp(ico)               &
                                               - cpatch%today_root_resp(ico))              &
                                             / cpatch%nplant(ico)
      else
         daily_C_gain = 0.0
      end if

      return


   end subroutine plant_maintenance
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine plant_carbon_balances(cpatch,ipa,ico,daily_C_gain,carbon_balance             &
                                   ,carbon_balance_pot,carbon_balance_max)
      use ed_state_vars, only : patchtype          ! ! structure
      use pft_coms     , only : growth_resp_factor ! ! intent(in)
      use consts_coms  , only : umol_2_kgC         & ! intent(in)
                              , day_sec            ! ! intent(in)
      use ed_misc_coms , only : current_time       ! ! intent(in)
      use ed_max_dims  , only : n_pft              ! ! intent(in)
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(patchtype)          , target      :: cpatch
      integer                  , intent(in)  :: ipa
      integer                  , intent(in)  :: ico
      real                     , intent(in)  :: daily_C_gain
      real                     , intent(out) :: carbon_balance
      real                     , intent(out) :: carbon_balance_pot
      real                     , intent(out) :: carbon_balance_max
      !----- Local variables. -------------------------------------------------------------!
      real                                   :: daily_C_gain_pot
      real                                   :: daily_C_gain_max
      real                                   :: growth_respiration_pot
      real                                   :: growth_respiration_max
      integer                                :: ipft
      !----- Local constants. -------------------------------------------------------------!
      logical                  , parameter   :: print_debug = .false.
      !----- Locally saved variables. -----------------------------------------------------!
      logical, dimension(n_pft), save        :: first_time  = .true.
      !------------------------------------------------------------------------------------!

      !----- Alias for PFT type. ----------------------------------------------------------!
      ipft = cpatch%pft(ico)

      !------ Calculate actual daily carbon balance: kgC/plant/day. -----------------------!
      carbon_balance = daily_C_gain - cpatch%growth_respiration(ico)                       &
                                    - cpatch%vleaf_respiration(ico)

      if (cpatch%nplant(ico) > tiny(1.0)) then

         !---------------------------------------------------------------------------------!
         !      Calculate potential carbon balance (used for nitrogen demand function).    !
         ! [kgC/plant/day].                                                                !
         !---------------------------------------------------------------------------------!
         daily_C_gain_pot       = umol_2_kgC * day_sec * ( cpatch%today_gpp_pot(ico)       &
                                                         - cpatch%today_leaf_resp(ico)     &
                                                         - cpatch%today_root_resp(ico))    &
                                                       / cpatch%nplant(ico)
         growth_respiration_pot = max(0.0, daily_C_gain_pot * growth_resp_factor(ipft))

         carbon_balance_pot = daily_C_gain_pot - cpatch%growth_respiration(ico)            & !JL!
                                               - cpatch%vleaf_respiration(ico)
         
       !----- Calculate maximum carbon balance (used for mortality). --------------------!
         daily_C_gain_max       = umol_2_kgC * day_sec * ( cpatch%today_gpp_max(ico)       &
                                                         - cpatch%today_leaf_resp(ico)     &
                                                         - cpatch%today_root_resp(ico) )   &
                                                       / cpatch%nplant(ico)
         growth_respiration_max = max(0.0, daily_C_gain_max * growth_resp_factor(ipft))
         carbon_balance_max     = daily_C_gain_max - growth_respiration_max                &
                                                   - cpatch%vleaf_respiration(ico)
      else
         carbon_balance_max = 0.0
         carbon_balance_pot = 0.0
      end if

      !----- Carbon balances for mortality. -----------------------------------------------!
      cpatch%cb(13,ico)     = cpatch%cb(13,ico)     + carbon_balance
      cpatch%cb_max(13,ico) = cpatch%cb_max(13,ico) + carbon_balance_max

      if (print_debug) then

         if (first_time(ipft)) then
            first_time(ipft) = .false.
            write (unit=30+ipft,fmt='(a10,15(1x,a12))')                                    &
                '      TIME','       PATCH','      COHORT','      NPLANT','    CB_TODAY'   &
                            ,' GROWTH_RESP','  VLEAF_RESP','   TODAY_GPP','TODAY_GPPMAX'   &
                            ,'  TODAY_LEAF','  TODAY_ROOT',' CBMAX_TODAY','          CB'   &
                            ,'       CBMAX','  LEAF_MAINT','  ROOT_MAINT'
         end if

         write(unit=30+ipft,fmt='(2(i2.2,a1),i4.4,2(1x,i12),13(1x,es12.5))')               &
              current_time%month,'/',current_time%date,'/',current_time%year               &
             ,ipa,ico,cpatch%nplant(ico),carbon_balance,cpatch%growth_respiration(ico)     &
             ,cpatch%vleaf_respiration(ico),cpatch%today_gpp(ico)                          &
             ,cpatch%today_gpp_max(ico),cpatch%today_leaf_resp(ico)                        &
             ,cpatch%today_root_resp(ico),carbon_balance_max,cpatch%cb(13,ico)             &
             ,cpatch%cb_max(13,ico),cpatch%leaf_maintenance(ico)                           &
             ,cpatch%root_maintenance(ico)
      end if

      return

   end subroutine plant_carbon_balances
   !=======================================================================================!
   !=======================================================================================!







   !=======================================================================================!
   !=======================================================================================!
   subroutine alloc_plant_c_balance(csite,ipa,ico,salloc,salloci,carbon_balance            &
                                   ,nitrogen_uptake,green_leaf_factor)
      use ed_state_vars , only : sitetype     & ! structure
                               , patchtype    ! ! structure
      use pft_coms      , only : c2n_storage  & ! intent(in)
                               , c2n_leaf     & ! intent(in)
                               , sla          & ! intent(in)
                               , q            & ! intent(in)
                               , qsw          & ! intent(in)
                               , c2n_stem     ! ! intent(in)
      use decomp_coms   , only : f_labile     ! ! intent(in)
      use allometry     , only : dbh2bl       ! ! function
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(sitetype) , target        :: csite
      integer        , intent(in)    :: ipa
      integer        , intent(in)    :: ico
      real           , intent(in)    :: salloc
      real           , intent(in)    :: salloci
      real           , intent(in)    :: carbon_balance
      real           , intent(inout) :: nitrogen_uptake
      real           , intent(in)    :: green_leaf_factor
      !----- Local variables. -------------------------------------------------------------!
      type(patchtype), pointer       :: cpatch
      integer                        :: ipft
      real                           :: bl_max
      real                           :: balive_max
      real                           :: bl_pot
      real                           :: increment
      real                           :: old_status
      real                           :: delta_bleaf
      real                           :: delta_broot
      real                           :: delta_bsapwood
      real                           :: available_carbon
      real                           :: f_total
      real                           :: f_bleaf
      real                           :: f_broot
      real                           :: f_bsapwood
      real                           :: f_resp
      real                           :: tr_bleaf
      real                           :: tr_broot
      real                           :: tr_bsapwood
      real                           :: bl
      logical                        :: on_allometry
      logical                        :: time_to_flush

      !------------------------------------------------------------------------------------!

      cpatch => csite%patch(ipa)
      
      ipft = cpatch%pft(ico) 

      !------------------------------------------------------------------------------------!
      !      When plants transit from dormancy to leaf flushing, it is possible that       !
      ! carbon_balance is negative, but the sum of carbon_balance and bstorage is          !
      ! positive. Under this circumstance, we have to allow plants to grow leaves.         !
      !------------------------------------------------------------------------------------!

      increment     = cpatch%bstorage(ico) + carbon_balance
      time_to_flush = (carbon_balance <= 0.0) .and. (increment > 0.0) .and.                &
                      (cpatch%phenology_status(ico) == 1) 

      if (carbon_balance > 0.0 .or. time_to_flush) then  !#1
         if (cpatch%phenology_status(ico) == 1) then   !#2
            !------------------------------------------------------------------------------!
            ! There are leaves, we are not actively dropping leaves and we're off          !
            ! allometry.  Here we will compute the maximum amount that can go to balive    !
            ! pools, and put any excess in storage.                                        !
            !------------------------------------------------------------------------------!
            !  available_carbon = cpatch%bstorage(ico) + carbon_balance
            ! for this section, excess carbon is included and will be subtracted from the
            ! balive portion  when the tr_bleaf ect are calculated
           available_carbon = cpatch%bstorage(ico) + carbon_balance + cpatch%excess_carbon(ico)

            !------------------------------------------------------------------------------!
            !     Maximum bleaf that the allometric relationship would allow.  If the      !
            ! plant is drought stress (elongf < 1), we do not allow the plant to get back  !
            ! to full allometry.                                                           !
            !------------------------------------------------------------------------------!
            bl_max     = dbh2bl(cpatch%dbh(ico),ipft) * green_leaf_factor                  &
                       * cpatch%elongf(ico)
            balive_max = dbh2bl(cpatch%dbh(ico),ipft) * salloc * cpatch%elongf(ico)

            !--- Amount that bleaf, broot, and bsapwood are off allometry -----------------!
            delta_bleaf = max (0.0, bl_max- cpatch%bleaf(ico))
            delta_broot = max (0.0, balive_max * q(ipft) * salloci - cpatch%broot(ico))
            delta_bsapwood = max (0.0, balive_max * qsw(ipft) * cpatch%hite(ico) * salloci &
                                     - cpatch%bsapwood(ico))

            !------------------------------------------------------------------------------!
            ! If the available carbon is less than what we need to get back to allometry.  !
            ! Grow pools in proportion to demand.  If we have enough carbon, we'll put the !
            ! extra into bstorage.                                                         !
            !------------------------------------------------------------------------------!
            
            f_bleaf    = delta_bleaf / bl_max
            f_broot    = delta_broot / (balive_max * q(ipft) * salloci )
            f_bsapwood = delta_bsapwood / (balive_max * qsw(ipft) * cpatch%hite(ico)       &
                       * salloci)
            f_total    = f_bleaf + f_broot + f_bsapwood

            !------------------------------------------------------------------------------!
            !     We only allow transfer from storage to living tissues if there is need   !
            ! to transfer.                                                                 !
            !------------------------------------------------------------------------------!
            if (f_total > 0.0) then
               tr_bleaf    = min( delta_bleaf   , (f_bleaf/f_total)    * available_carbon)
               tr_broot    = min( delta_broot   , (f_broot/f_total)    * available_carbon)
               tr_bsapwood = min( delta_bsapwood, (f_bsapwood/f_total) * available_carbon)
            else
               tr_bleaf    = 0.
               tr_broot    = 0.
               tr_bsapwood = 0.
            end if
            !------------------------------------------------------------------------------!
            !------------------------------------------------------------------------------!
            !XXT
            tr_bleaf = tr_bleaf - cpatch%excess_carbon(ico) * (f_bleaf/f_total)
            tr_broot = tr_broot - cpatch%excess_carbon(ico) * (f_broot/f_total)
            tr_bsapwood = tr_bsapwood - cpatch%excess_carbon(ico) * (f_bsapwood/f_total)

            cpatch%bleaf(ico)    = cpatch%bleaf(ico)    + tr_bleaf
            cpatch%broot(ico)    = cpatch%broot(ico)    + tr_broot
            cpatch%bsapwood(ico) = cpatch%bsapwood(ico) + tr_bsapwood
            cpatch%balive(ico)   = cpatch%bleaf(ico) + cpatch%broot(ico)                   &
                                 + cpatch%bsapwood(ico)
    
            !----- NPP allocation in diff pools in KgC/m2/day. ----------------------------!
            cpatch%today_nppleaf(ico)   = tr_bleaf       * cpatch%nplant(ico)
            cpatch%today_nppfroot(ico)  = tr_broot       * cpatch%nplant(ico)
            cpatch%today_nppsapwood(ico)= tr_bsapwood    * cpatch%nplant(ico)
            cpatch%today_nppdaily(ico)  = carbon_balance * cpatch%nplant(ico)
            
            !------------------------------------------------------------------------------!
            !    Find the amount of carbon used to recover the tissues that were off-      !
            ! -allometry, take that from the carbon balance first, then use some of the    !
            ! storage if needed be.                                                        !
            !------------------------------------------------------------------------------!
            increment = carbon_balance -  tr_bleaf - tr_broot - tr_bsapwood
            cpatch%bstorage(ico) = max(0.0, cpatch%bstorage(ico) + increment) 
            !------------------------------------------------------------------------------!

                nitrogen_uptake = nitrogen_uptake + (carbon_balance -increment)/ c2n_leaf(ipft)

            on_allometry = 2.0 * abs(balive_max - cpatch%balive(ico))                      &
                         / (balive_max + cpatch%balive(ico))          < 1.e-6
            if (cpatch%elongf(ico) == 1.0 .and. on_allometry) then 
               !---------------------------------------------------------------------------!
               !     We're back to allometry, change phenology_status.                     !
               !---------------------------------------------------------------------------!
               cpatch%phenology_status(ico) = 0
            end if
         else !phenology is not 1 #2
            !------------------------------------------------------------------------------!
            !     Put carbon gain into storage.  If we're not actively dropping leaves or  !
            ! off-allometry, this will be used for structural growth at the end of the     !
            ! month.                                                                       !
            !------------------------------------------------------------------------------!
            cpatch%bstorage(ico) = cpatch%bstorage(ico) + carbon_balance                         
            !----- NPP allocation in diff pools in Kg C/m2/day. ---------------------------!
            cpatch%today_nppleaf(ico)    = 0.0
            cpatch%today_nppfroot(ico)   = 0.0
            cpatch%today_nppsapwood(ico) = 0.0
            cpatch%today_nppdaily(ico)   = carbon_balance * cpatch%nplant(ico)
         end if !phenology branch end #2
 

      else  !#1
         !---------------------------------------------------------------------------------!
         !   Carbon balance is negative, take it out of storage.                           !
         !---------------------------------------------------------------------------------!
         increment =  cpatch%bstorage(ico) + carbon_balance

         if (increment <= 0.0)  then !CB will consume all bstorage #3
 
           !----- Use Storage pool first then take out of balive. ------------------------!
            increment            =  - increment
            cpatch%bstorage(ico) = 0.0
           if (cpatch%phenology_status(ico) == 0)  then !#4
               !---------------------------------------------------------------------------!
               !     We were on allometry, but now we need to burn carbon and go off-      !
               ! -allometry.                                                               !
               !---------------------------------------------------------------------------!
               cpatch%balive(ico)   = cpatch%balive(ico) - increment
               cpatch%bleaf(ico)    = cpatch%balive(ico) * salloci * green_leaf_factor
               cpatch%broot(ico)    = cpatch%balive(ico) * q(ipft) * salloci
               cpatch%bsapwood(ico) = cpatch%balive(ico) * cpatch%hite(ico) * qsw(ipft)    &
                                    * salloci
               cpatch%phenology_status(ico) = 1
            else
               f_resp = cpatch%today_leaf_resp(ico)                                        &
                      / ( cpatch%today_leaf_resp(ico) + cpatch%today_root_resp(ico) )
               bl     = cpatch%bleaf(ico) - f_resp * (increment)

               if (bl > 0.0) then
                  cpatch%bleaf(ico) = bl
                  cpatch%broot(ico) = cpatch%broot(ico) - (1.0 - f_resp) * increment 
               else
                  cpatch%broot(ico) = cpatch%broot(ico) - (increment - cpatch%bleaf(ico))
                  cpatch%bleaf(ico) = 0.0
                  cpatch%elongf(ico) = 0.0
                  cpatch%phenology_status(ico) = 2
              end if 

               cpatch%balive(ico) = cpatch%bleaf(ico) + cpatch%broot(ico)                  &
                                  + cpatch%bsapwood(ico)    
          end if!#4

            csite%fsn_in(ipa) = csite%fsn_in(ipa) + increment                              &
                              * ( f_labile(ipft) / c2n_leaf(ipft)                          &
                                + (1.0 - f_labile(ipft)) / c2n_stem(ipft) )                &
                              * cpatch%nplant(ico)

         else !#3 !increment bigger then 0, bstorage large enough to satisfy neg CB
            !------ Burn the storage pool.  Dont' forget the nitrogen. --------------------!
            cpatch%bstorage(ico) =  cpatch%bstorage(ico) + carbon_balance
         end if !#3 

         !---- NPP allocation in diff pools in KgC/m2/day. --------------------------------!
         cpatch%today_nppleaf(ico)    = 0.0
         cpatch%today_nppfroot(ico)   = 0.0
         cpatch%today_nppsapwood(ico) = 0.0
         cpatch%today_nppdaily(ico)   = carbon_balance * cpatch%nplant(ico)
      end if !#1

      return
   end subroutine alloc_plant_c_balance
   !=======================================================================================!
   !=======================================================================================!





   !=======================================================================================!
   !=======================================================================================!
   subroutine potential_N_uptake(cpatch,ico,salloc,salloci,balive_in,carbon_balance_pot    &
                                ,N_uptake_pot,green_leaf_factor)
      use ed_state_vars , only : patchtype    ! ! structure
      use pft_coms      , only : c2n_storage  & ! intent(in)
                               , c2n_leaf     & ! intent(in)
                               , c2n_stem     ! ! intent(in)
      use decomp_coms   , only : f_labile     ! ! intent(in)
      use allometry     , only : dbh2bl       ! ! intent(in)
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(patchtype), target        :: cpatch
      integer        , intent(in)    :: ico
      real           , intent(in)    :: salloc
      real           , intent(in)    :: salloci
      real           , intent(in)    :: balive_in
      real           , intent(in)    :: carbon_balance_pot
      real           , intent(inout) :: N_uptake_pot
      real           , intent(in)    :: green_leaf_factor
      !----- Local variables. -------------------------------------------------------------!
      integer                        :: ipft
      real                           :: bl_max
      real                           :: bl_pot
      real                           :: increment
      real                           :: nstorage_max !JL!
      real                           :: balive_max !JL!
      !------------------------------------------------------------------------------------!

      ipft = cpatch%pft(ico) 

      ! N_uptake_pot is 0 when CB is 0!JL
       if (carbon_balance_pot < 0.0 )then 
           N_uptake_pot = 0.0        
       end if                            

      if ( cpatch%phenology_status(ico) == 0 .and. carbon_balance_pot > 0.0 ) then

         !----- Positive carbon balance with plants fully flushed. ------------------------!
        
      elseif (cpatch%phenology_status(ico) == 1) then

          !JL Calculate how much C will go into balive for full flush, not just bleaf
          balive_max = dbh2bl(cpatch%dbh(ico),ipft) * salloc * cpatch%elongf(ico)
          bl_pot = cpatch%balive(ico) + carbon_balance_pot

         if (bl_pot > balive_max) then
            !------------------------------------------------------------------------------!
            !     This increment would take us over the limit, so we assign all that can   !
            ! go for leaves to them, and put the remainder in storage.                     !
            !------------------------------------------------------------------------------!
            
            ! amount that would go into storage
            increment    = carbon_balance_pot - (balive_max-cpatch%balive(ico)) 
 
           !JL the amount of growth if you have no limitations
            increment    = balive_max-cpatch%balive(ico)  
           !JL the amount o f N you use for the extra growth  
            N_uptake_pot = N_uptake_pot + increment                                        &
                         * ( f_labile(ipft) / c2n_leaf(ipft)                               &
                           + (1.0 - f_labile(ipft)) / c2n_stem(ipft)) 

         elseif (carbon_balance_pot > 0.0) then

            !------------------------------------------------------------------------------!
            !      This increment did not exceed the limit, put everything in leaves.  We  !
            ! don't compute the uptake if carbon balance is negative, just because there   !
            ! will be no uptake...                                                         !
            !------------------------------------------------------------------------------!
            N_uptake_pot = N_uptake_pot + carbon_balance_pot                               &
                         * ( f_labile(ipft) / c2n_leaf(ipft)                               &
                           + (1.0 - f_labile(ipft)) / c2n_stem(ipft))                   

         end if
      end if

     
    return
   end subroutine potential_N_uptake
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine litter(csite,ipa)

      use ed_state_vars      , only : patchtype & ! structure
                             , sitetype  ! ! structure
      use pft_coms           , only : c2n_leaf  & ! intent(in)
                             , c2n_stem  & ! intent(in)
                             , l2n_stem  ! ! intent(in)
      use decomp_coms        , only : f_labile  ! ! intent(in)
      use nutrient_constants , only: resorption_factor
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(sitetype)  , target     :: csite
      integer         , intent(in) :: ipa
      !----- Local variables. -------------------------------------------------------------!
      type(patchtype) , pointer    :: cpatch
      integer                      :: ico
      integer                      :: ipft
      real                         :: plant_litter
      real                         :: plant_litter_f
      real                         :: plant_litter_s
      real                         :: plant_litter_f_no_excess_c!JL
       !------------------------------------------------------------------------------------!

      cpatch => csite%patch(ipa)
   
      !------------------------------------------------------------------------------------!
      !      Add fine root and leaf turnover to the litter.                                !
      !------------------------------------------------------------------------------------!
      do ico=1,cpatch%ncohorts
         ipft = cpatch%pft(ico)

         plant_litter   = ( cpatch%leaf_maintenance(ico) + cpatch%root_maintenance(ico) )  &
                        * cpatch%nplant(ico)
         plant_litter_f = plant_litter * f_labile(ipft)                                    & 
                        + (cpatch%excess_carbon(ico)* cpatch%nplant(ico))                 !JL
         plant_litter_f_no_excess_c = plant_litter * f_labile(ipft)                       !JL
         plant_litter_s = plant_litter - plant_litter_f_no_excess_c                       !JL!

         csite%fsc_in(ipa) = csite%fsc_in(ipa) + plant_litter_f    
         csite%fsc_in_no_excess(ipa) =  csite%fsc_in_no_excess(ipa)                        &
                                     + plant_litter_f_no_excess_c                         !JL!
     
         csite%fsn_in(ipa) = csite%fsn_in(ipa)                                             &
                           + ((plant_litter_f_no_excess_c / c2n_leaf(ipft))                &
                           *(1-resorption_factor))                                        !JL


         csite%ssc_in(ipa) = csite%ssc_in(ipa) + plant_litter_s
         csite%ssl_in(ipa) = csite%ssl_in(ipa) + plant_litter_s * l2n_stem / c2n_stem(ipft)

     end do
      return
   end subroutine litter
   !=======================================================================================!
   !=======================================================================================!
   ! NEW SUBROUTINE., JL,XXT
   !=======================================================================================!
   !=======================================================================================!

  subroutine shadow_n(csite,ipa,ico,salloc,salloci,carbon_balance            &
                                   ,nitrogen_uptake,green_leaf_factor)
      use ed_state_vars , only : sitetype     & ! structure
                               , patchtype    ! ! structure
      use pft_coms      , only : c2n_storage  & ! intent(in)
                               , c2n_leaf     & ! intent(in)
                               , sla          & ! intent(in)
                               , q            & ! intent(in)
                               , qsw          & ! intent(in)
                               , c2n_stem     ! ! intent(in)
      use decomp_coms   , only : f_labile     ! ! intent(in)
      use allometry     , only : dbh2bl       ! ! function
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(sitetype) , target        :: csite
      integer        , intent(in)    :: ipa
      integer        , intent(in)    :: ico
      real           , intent(in)    :: salloc
      real           , intent(in)    :: salloci
      real           , intent(in)    :: carbon_balance
      real           , intent(inout) :: nitrogen_uptake
      real           , intent(in)    :: green_leaf_factor

!----- Local variables. -------------------------------------------------------------!
      type(patchtype), pointer       :: cpatch
      integer                        :: ipft
      real                           :: bl_max
      real                           :: balive_max
      real                           :: bl_pot
      real                           :: increment
      real                           :: old_status
      real                           :: delta_bleaf
      real                           :: delta_broot
      real                           :: delta_bsapwood
      real                           :: available_carbon
      real                           :: f_total
      real                           :: f_bleaf
      real                           :: f_broot
      real                           :: f_bsapwood
      real                           :: f_resp
      real                           :: tr_bleaf
      real                           :: tr_broot
      real                           :: tr_bsapwood
      real                           :: bl
      logical                        :: on_allometry
      logical                        :: time_to_flush
      !JL additions for shadow calculation!      
      real                           :: temp_bleaf
      real                           :: temp_bstorage
      real                           :: temp_broot
      real                           :: temp_bsapwood
      real                           :: temp_balive
      integer                        :: temp_phenology_status
      real                           :: temp_elongf
      real                           :: temp_hite
      real                           :: temp_dbh
      !------------------------------------------------------------------------------------!

      cpatch => csite%patch(ipa)
      
      ipft = cpatch%pft(ico) 

      !Initialize new vairables !
      temp_bleaf = cpatch%bleaf(ico)
      temp_bstorage = cpatch%bstorage(ico)
      temp_broot = cpatch%broot(ico)
      temp_bsapwood= cpatch%bsapwood(ico)
      temp_balive = cpatch%balive(ico)  
      temp_phenology_status = cpatch%phenology_status(ico) 
      temp_elongf=cpatch%elongf(ico)
      temp_dbh=cpatch%dbh(ico)
      temp_hite = cpatch%hite(ico)


      !------------------------------------------------------------------------------------!
      !      When plants transit from dormancy to leaf flushing, it is possible that       !
      ! carbon_balance is negative, but the sum of carbon_balance and bstorage is          !
      ! positive. Under this circumstance, we have to allow plants to grow leaves.         !
      !------------------------------------------------------------------------------------!
      increment     = temp_bstorage + carbon_balance
      time_to_flush = (carbon_balance <= 0.0) .and. (increment > 0.0) .and.                &
                      (temp_phenology_status == 1) 

      if (carbon_balance > 0.0 .or. time_to_flush) then 
         if (temp_phenology_status == 1) then
            !------------------------------------------------------------------------------!
            ! There are leaves, we are not actively dropping leaves and we're off          !
            ! allometry.  Here we will compute the maximum amount that can go to balive    !
            ! pools, and put any excess in storage.                                        !
            !------------------------------------------------------------------------------!
            available_carbon = temp_bstorage + carbon_balance

            !------------------------------------------------------------------------------!
            !     Maximum bleaf that the allometric relationship would allow.  If the      !
            ! plant is drought stress (elongf < 1), we do not allow the plant to get back  !
            ! to full allometry.                                                           !
            !------------------------------------------------------------------------------!
           
           
            bl_max     = dbh2bl(cpatch%dbh(ico),ipft) * green_leaf_factor                  &
                       * temp_elongf
            balive_max = dbh2bl(cpatch%dbh(ico),ipft) * salloc *  temp_elongf

            !--- Amount that bleaf, broot, and bsapwood are off allometry -----------------!
            delta_bleaf = max (0.0, bl_max- temp_bleaf)
            delta_broot = max (0.0, balive_max * q(ipft) * salloci - temp_broot)
            delta_bsapwood = max (0.0, balive_max * qsw(ipft) * temp_hite * salloci &
                                     - temp_bsapwood)

            !------------------------------------------------------------------------------!
            ! If the available carbon is less than what we need to get back to allometry.  !
            ! Grow pools in proportion to demand.  If we have enough carbon, we'll put the !
            ! extra into bstorage.                                                         !
            !------------------------------------------------------------------------------!
            
            f_bleaf    = delta_bleaf / bl_max
            f_broot    = delta_broot / (balive_max * q(ipft) * salloci )
            f_bsapwood = delta_bsapwood / (balive_max * qsw(ipft) * temp_hite      &
                       * salloci)
            f_total    = f_bleaf + f_broot + f_bsapwood

            !------------------------------------------------------------------------------!
            !     We only allow transfer from storage to living tissues if there is need   !
            ! to transfer.                                                                 !
            !------------------------------------------------------------------------------!
            if (f_total > 0.0) then
               tr_bleaf    = min( delta_bleaf   , (f_bleaf/f_total)    * available_carbon)
               tr_broot    = min( delta_broot   , (f_broot/f_total)    * available_carbon)
               tr_bsapwood = min( delta_bsapwood, (f_bsapwood/f_total) * available_carbon)
            else
               tr_bleaf    = 0.
               tr_broot    = 0.
               tr_bsapwood = 0.
            end if
            !------------------------------------------------------------------------------!
            temp_bleaf = temp_bleaf
            temp_bleaf    = temp_bleaf    + tr_bleaf
            temp_broot    = temp_broot    + tr_broot
            temp_bsapwood = temp_bsapwood + tr_bsapwood

            temp_balive   =temp_bleaf + temp_broot                   &
                                 + temp_bsapwood

            !------------------------------------------------------------------------------!
            !    Find the amount of carbon used to recover the tissues that were off-      !
            ! -allometry, take that from the carbon balance first, then use some of the    !
            ! storage if needed be.                                                        !
            !------------------------------------------------------------------------------!
            increment = carbon_balance -  tr_bleaf - tr_broot - tr_bsapwood
            temp_bstorage = max(0.0, temp_bstorage + increment) 
            !------------------------------------------------------------------------------!
            nitrogen_uptake = nitrogen_uptake + (carbon_balance -increment)/ c2n_leaf(ipft)

            on_allometry = 2.0 * abs(balive_max - temp_balive)                      &
                         / (balive_max  + temp_balive)          < 1.e-6
            if (temp_elongf == 1.0 .and. on_allometry) then
               !---------------------------------------------------------------------------!
               !     We're back to allometry, change phenology_status.                     !
               !---------------------------------------------------------------------------!
             temp_phenology_status = 0
            end if
         else
            !------------------------------------------------------------------------------!
            !     Put carbon gain into storage.  If we're not actively dropping leaves or  !
            ! off-allometry, this will be used for structural growth at the end of the     !
            ! month.                                                                       !
            !------------------------------------------------------------------------------!
           temp_bstorage =temp_bstorage + carbon_balance
         end if
 

      else
         !---------------------------------------------------------------------------------!
         !   Carbon balance is negative, take it out of storage.                           !
         !---------------------------------------------------------------------------------!
         increment =  temp_bstorage + carbon_balance

         if (increment <= 0.0)  then
            !----- Use Storage pool first then take out of balive. ------------------------!
            increment            =  - increment
            temp_bstorage = 0.0

            if (temp_phenology_status == 0)  then
               !---------------------------------------------------------------------------!
               !     We were on allometry, but now we need to burn carbon and go off-      !
               ! -allometry.                                                               !
               !---------------------------------------------------------------------------!
              temp_balive   = temp_balive - increment
              temp_bleaf    = temp_balive * salloci * green_leaf_factor
              temp_broot   =temp_balive * q(ipft) * salloci
              temp_bsapwood = temp_balive * temp_hite * qsw(ipft)    &
                                    * salloci
               temp_phenology_status = 1
            else
               f_resp = cpatch%today_leaf_resp(ico)                                        &
                      / ( cpatch%today_leaf_resp(ico) + cpatch%today_root_resp(ico) )
               bl     = temp_bleaf - f_resp * (increment)

               if (bl > 0.0) then
                 temp_bleaf = bl
                  temp_broot = temp_broot - (1.0 - f_resp) * increment 
               else
                 temp_broot =temp_broot - (increment - temp_bleaf)
                  temp_bleaf = 0.0
                 temp_elongf = 0.0
                  temp_phenology_status = 2
               end if

               temp_balive= temp_bleaf + temp_broot                &
                                  + temp_bsapwood   
            end if

         else
            !------ Burn the storage pool.  Dont' forget the nitrogen. --------------------!
           temp_bstorage =  temp_bstorage + carbon_balance
         end if
      end if

      return
   end subroutine shadow_n
!==========================================================================================!
!==========================================================================================!

end module growth_balive
!==========================================================================================!
!==========================================================================================!
