!==========================================================================================!
!==========================================================================================!
!     This module contains the main variable structures in ED, and several important sub-  !
! routines for allocation, de-allocation.                                                  !
!------------------------------------------------------------------------------------------!
module ed_state_vars

!  use grid_coms, only: nzg,nzs,ngrids
!  use c34constants, only : stoma_data,n_stoma_atts
!  use ed_max_dims, only: max_site,n_pft,n_dbh,n_age,n_mort,n_dist_types      &
!                       ,maxmach,maxgrds, str_len
!  use disturb_coms, only : lutime,num_lu_trans,max_lu_years
!  use met_driver_coms, only: met_driv_data,met_driv_state
!  use fusion_fission_coms, only: ff_nhgt,hgt_class
!  use phenology_coms, only: prescribed_phen
!  use ed_misc_coms, only: idoutput, imoutput, iqoutput, ndcycle

  implicit none
!============================================================================!
!============================================================================!



!============================================================================!
!============================================================================!
  !----------------------------------------------!  
  !  GLOBAL VARIABLES



  !------------------------------------------------------------------------------------!
  ! These variables are allocated and assigned before the parallel distribution, so we !
  ! don't want to keep it inside the structure. In case of serial runs, we should have !
  ! offset full of zeroes and the polygons numbered from 1 to the number of polygons.  !
  ! In a non-POI parallel run, we want to allocate "mpolygons" in each node, but then  !
  ! we need to keep track of the actual polygon "ID" to write the output correctly.    !
  !------------------------------------------------------------------------------------!
  
  ! Number of polygons in each grid, for each machine. so this has a ngrids size. 
  integer, dimension(maxmach,maxgrds) :: gdpy

  ! Number of sites in each grid, for each machine.
  integer, dimension(maxmach,maxgrds) :: gdsi

  ! Number of patches in each grid, for each machine.
  integer, dimension(maxmach,maxgrds) :: gdpa

  ! Number of cohorts in each grid, for each machine.
  integer, dimension(maxmach,maxgrds) :: gdco
  

  ! Offset for each machine, so this has a nmachs size.
  integer, dimension(maxmach,maxgrds) :: py_off 
  
  ! Offset for each machine, so this has a nmachs size.
  integer, dimension(maxmach,maxgrds) :: si_off 

  ! Offset for each machine, so this has a nmachs size.
  integer, dimension(maxmach,maxgrds) :: pa_off 

  ! Offset for each machine, so this has a nmachs size.
  integer, dimension(maxmach,maxgrds) :: co_off 


  type(edtype),pointer,dimension(:) :: edgrid_g

  ! The following are swap variables used during
  ! deallocation-reallocation

  type(edtype)      :: edswap_g

  type(polygontype) :: polyswap_g

  type(sitetype)    :: siteswap_g

  type(patchtype)   :: patchswap_g
  
!------------------------------------------------------------------------------------------!
!   The following variables are for tracking the number of variables written in the output,!
! this way we avoid having the same ID used twice.                                         !
!------------------------------------------------------------------------------------------!
  integer :: nioglobal, niogrid, niopoly, niosite

!------------------------------------------------------------------------------------------!
! Logical switch that decides if the pointer tables for IO need to be updated
! The number and allocation of cohorts and patches dictates this, and they 
! change at a monthly frequency typically.
!------------------------------------------------------------------------------------------!
  logical :: filltables

  
  
contains
!============================================================================!
!============================================================================!





!============================================================================!
!============================================================================!
  ! ===================================================
  !  Allocation subroutines of ed state variables
  ! ===================================================

  subroutine allocate_edglobals(ngrids)

    use ed_var_tables,only:num_var,vt_info,maxvars

    implicit none
    integer :: ngrids
    
    if (associated(edgrid_g))  then
       print*,"SHOULD NOT HAVE ASSOCIATED GLOBALS"
    else
       nullify(edgrid_g)
       allocate(edgrid_g(ngrids))
    end if

    !  Allocate the basic var-table structures

    allocate(num_var(ngrids))
    num_var = 0
    
    allocate(vt_info(maxvars,ngrids))
    vt_info(:,:)%first=.true.


    !  Initialize the global offsets

    edgrid_g(:)%mach_cohort_offset_index = 0
    edgrid_g(:)%mach_patch_offset_index = 0
    edgrid_g(:)%mach_site_offset_index = 0
    edgrid_g(:)%mach_polygon_offset_index = 0


    return
  end subroutine allocate_edglobals
!============================================================================!
!============================================================================!




!============================================================================!
!============================================================================!
  subroutine deallocate_polygontype(cpoly)

    implicit none

    type(polygontype),target :: cpoly

    if(associated(cpoly%sipa_id                     )) deallocate(cpoly%sipa_id                     )
    if(associated(cpoly%sipa_n                      )) deallocate(cpoly%sipa_n                      )
    if(associated(cpoly%patch_count                 )) deallocate(cpoly%patch_count                 )
    if(associated(cpoly%site                        )) deallocate(cpoly%site                        )
    if(associated(cpoly%sitenum                     )) deallocate(cpoly%sitenum                     )

    if(associated(cpoly%lsl                         )) deallocate(cpoly%lsl                         )

    if(associated(cpoly%area                        )) deallocate(cpoly%area                        )
    if(associated(cpoly%patch_area                  )) deallocate(cpoly%patch_area                  )
    if(associated(cpoly%elevation                   )) deallocate(cpoly%elevation                   )
    if(associated(cpoly%slope                       )) deallocate(cpoly%slope                       )
    if(associated(cpoly%aspect                      )) deallocate(cpoly%aspect                      )

    if(associated(cpoly%num_landuse_years           )) deallocate(cpoly%num_landuse_years           )
    if(associated(cpoly%mindbh_primary              )) deallocate(cpoly%mindbh_primary              )
    if(associated(cpoly%probharv_primary            )) deallocate(cpoly%probharv_primary            )
    if(associated(cpoly%mindbh_secondary            )) deallocate(cpoly%mindbh_secondary            )
    if(associated(cpoly%probharv_secondary          )) deallocate(cpoly%probharv_secondary          )


    if(associated(cpoly%lai_pft                     )) deallocate(cpoly%lai_pft                     )
    if(associated(cpoly%wpa_pft                     )) deallocate(cpoly%wpa_pft                     )
    if(associated(cpoly%wai_pft                     )) deallocate(cpoly%wai_pft                     )

    if(associated(cpoly%TCI                         )) deallocate(cpoly%TCI                         )
    if(associated(cpoly%pptweight                   )) deallocate(cpoly%pptweight                         )
    if(associated(cpoly%lsl                         )) deallocate(cpoly%lsl                         )
    if(associated(cpoly%hydro_next                  )) deallocate(cpoly%hydro_next                  )
    if(associated(cpoly%hydro_prev                  )) deallocate(cpoly%hydro_prev                  )
    if(associated(cpoly%moist_W                     )) deallocate(cpoly%moist_W                     )
    if(associated(cpoly%moist_f                     )) deallocate(cpoly%moist_f                     )
    if(associated(cpoly%moist_tau                   )) deallocate(cpoly%moist_tau                   )
    if(associated(cpoly%moist_zi                    )) deallocate(cpoly%moist_zi                    )
    if(associated(cpoly%baseflow                    )) deallocate(cpoly%baseflow                    )
    if(associated(cpoly%ntext_soil                  )) deallocate(cpoly%ntext_soil                  )
    if(associated(cpoly%min_monthly_temp            )) deallocate(cpoly%min_monthly_temp            )
    if(associated(cpoly%plantation                  )) deallocate(cpoly%plantation                  )
    if(associated(cpoly%agri_stocking_pft           )) deallocate(cpoly%agri_stocking_pft           )
    if(associated(cpoly%agri_stocking_density       )) deallocate(cpoly%agri_stocking_density       )
    if(associated(cpoly%plantation_stocking_pft     )) deallocate(cpoly%plantation_stocking_pft     )
    if(associated(cpoly%plantation_stocking_density )) deallocate(cpoly%plantation_stocking_density )
    if(associated(cpoly%primary_harvest_memory      )) deallocate(cpoly%primary_harvest_memory      )
    if(associated(cpoly%secondary_harvest_memory    )) deallocate(cpoly%secondary_harvest_memory    )
    if(associated(cpoly%fire_disturbance_rate       )) deallocate(cpoly%fire_disturbance_rate       )
    if(associated(cpoly%ignition_rate               )) deallocate(cpoly%ignition_rate               )
    if(associated(cpoly%lambda_fire                 )) deallocate(cpoly%lambda_fire                 )
    if(associated(cpoly%phen_pars                   )) deallocate(cpoly%phen_pars                   )
    if(associated(cpoly%nat_disturbance_rate        )) deallocate(cpoly%nat_disturbance_rate        )
    if(associated(cpoly%nat_dist_type               )) deallocate(cpoly%nat_dist_type               )
    if(associated(cpoly%disturbance_memory          )) deallocate(cpoly%disturbance_memory          )
    if(associated(cpoly%disturbance_rates           )) deallocate(cpoly%disturbance_rates           )
    if(associated(cpoly%loss_fraction               )) deallocate(cpoly%loss_fraction               )
    if(associated(cpoly%green_leaf_factor           )) deallocate(cpoly%green_leaf_factor           )
    if(associated(cpoly%leaf_aging_factor           )) deallocate(cpoly%leaf_aging_factor           )
    if(associated(cpoly%met                         )) deallocate(cpoly%met                         )
    if(associated(cpoly%basal_area                  )) deallocate(cpoly%basal_area                  )
    if(associated(cpoly%agb                         )) deallocate(cpoly%agb                         )
    if(associated(cpoly%pldens                      )) deallocate(cpoly%pldens                      )
    if(associated(cpoly%bseeds                      )) deallocate(cpoly%bseeds                      )
    
    if(associated(cpoly%basal_area_growth           )) deallocate(cpoly%basal_area_growth           )
    if(associated(cpoly%agb_growth                  )) deallocate(cpoly%agb_growth                  )
    if(associated(cpoly%basal_area_mort             )) deallocate(cpoly%basal_area_mort             )
    if(associated(cpoly%agb_mort                    )) deallocate(cpoly%agb_mort                    )
    if(associated(cpoly%basal_area_cut              )) deallocate(cpoly%basal_area_cut              )!NOT IN REGISTRY
    if(associated(cpoly%agb_cut                     )) deallocate(cpoly%agb_cut                     )

    if(associated(cpoly%cosaoi                      )) deallocate(cpoly%cosaoi                      )
    if(associated(cpoly%avg_albedo_beam             )) deallocate(cpoly%avg_albedo_beam             )
    if(associated(cpoly%avg_albedo_diffuse          )) deallocate(cpoly%avg_albedo_diffuse          )
    if(associated(cpoly%avg_rlong_albedo            )) deallocate(cpoly%avg_rlong_albedo            )
    if(associated(cpoly%avg_albedo                  )) deallocate(cpoly%avg_albedo                  )
    if(associated(cpoly%avg_rlongup                 )) deallocate(cpoly%avg_rlongup                 )

    if(associated(cpoly%daylight                    )) deallocate(cpoly%daylight                    )
    
    if(associated(cpoly%lai                         )) deallocate(cpoly%lai                         )
    if(associated(cpoly%avg_lma                     )) deallocate(cpoly%avg_lma                     )
    if(associated(cpoly%wpa                         )) deallocate(cpoly%wpa                         )
    if(associated(cpoly%wai                         )) deallocate(cpoly%wai                         )

    ! Fast time flux diagnostics
    ! ---------------------------------------------
    if(associated(cpoly%avg_vapor_lc                )) deallocate(cpoly%avg_vapor_lc                )
    if(associated(cpoly%avg_vapor_wc                )) deallocate(cpoly%avg_vapor_wc                )
    if(associated(cpoly%avg_dew_cg                  )) deallocate(cpoly%avg_dew_cg                  )
    if(associated(cpoly%avg_vapor_gc                )) deallocate(cpoly%avg_vapor_gc                )
    if(associated(cpoly%avg_wshed_vg                )) deallocate(cpoly%avg_wshed_vg                )
    if(associated(cpoly%avg_intercepted             )) deallocate(cpoly%avg_intercepted             )
    if(associated(cpoly%avg_throughfall             )) deallocate(cpoly%avg_throughfall             )
    if(associated(cpoly%avg_vapor_ac                )) deallocate(cpoly%avg_vapor_ac                )
    if(associated(cpoly%avg_transp                  )) deallocate(cpoly%avg_transp                  )
    if(associated(cpoly%avg_evap                    )) deallocate(cpoly%avg_evap                    )
    if(associated(cpoly%avg_smoist_gg               )) deallocate(cpoly%avg_smoist_gg               )
    if(associated(cpoly%avg_transloss               )) deallocate(cpoly%avg_transloss               )
    if(associated(cpoly%avg_runoff                  )) deallocate(cpoly%avg_runoff                  )
    if(associated(cpoly%avg_drainage                )) deallocate(cpoly%avg_drainage                )
    if(associated(cpoly%avg_drainage_heat           )) deallocate(cpoly%avg_drainage_heat           )
    if(associated(cpoly%aux                         )) deallocate(cpoly%aux                         )
    if(associated(cpoly%aux_s                       )) deallocate(cpoly%aux_s                       )
    if(associated(cpoly%avg_rshort_gnd              )) deallocate(cpoly%avg_rshort_gnd              )
    if(associated(cpoly%avg_rlong_gnd               )) deallocate(cpoly%avg_rlong_gnd               )
    if(associated(cpoly%avg_carbon_ac               )) deallocate(cpoly%avg_carbon_ac               )
    if(associated(cpoly%avg_sensible_lc             )) deallocate(cpoly%avg_sensible_lc             )
    if(associated(cpoly%avg_sensible_wc             )) deallocate(cpoly%avg_sensible_wc             )
    if(associated(cpoly%avg_qwshed_vg               )) deallocate(cpoly%avg_qwshed_vg               )
    if(associated(cpoly%avg_qintercepted            )) deallocate(cpoly%avg_qintercepted            )
    if(associated(cpoly%avg_qthroughfall            )) deallocate(cpoly%avg_qthroughfall            )
    if(associated(cpoly%avg_sensible_gc             )) deallocate(cpoly%avg_sensible_gc             )
    if(associated(cpoly%avg_sensible_ac             )) deallocate(cpoly%avg_sensible_ac             )
    if(associated(cpoly%avg_sensible_gg             )) deallocate(cpoly%avg_sensible_gg             )
    if(associated(cpoly%avg_runoff_heat             )) deallocate(cpoly%avg_runoff_heat             )
    if(associated(cpoly%avg_leaf_energy             )) deallocate(cpoly%avg_leaf_energy             )
    if(associated(cpoly%avg_leaf_hcap               )) deallocate(cpoly%avg_leaf_hcap               )
    if(associated(cpoly%avg_leaf_temp               )) deallocate(cpoly%avg_leaf_temp               )
    if(associated(cpoly%avg_leaf_fliq               )) deallocate(cpoly%avg_leaf_fliq               )
    if(associated(cpoly%avg_leaf_water              )) deallocate(cpoly%avg_leaf_water              )
    if(associated(cpoly%avg_wood_energy             )) deallocate(cpoly%avg_wood_energy             )
    if(associated(cpoly%avg_wood_hcap               )) deallocate(cpoly%avg_wood_hcap               )
    if(associated(cpoly%avg_wood_temp               )) deallocate(cpoly%avg_wood_temp               )
    if(associated(cpoly%avg_wood_fliq               )) deallocate(cpoly%avg_wood_fliq               )
    if(associated(cpoly%avg_wood_water              )) deallocate(cpoly%avg_wood_water              )
    if(associated(cpoly%avg_can_temp                )) deallocate(cpoly%avg_can_temp                )
    if(associated(cpoly%avg_can_shv                 )) deallocate(cpoly%avg_can_shv                 )
    if(associated(cpoly%avg_can_co2                 )) deallocate(cpoly%avg_can_co2                 )
    if(associated(cpoly%avg_can_rhos                )) deallocate(cpoly%avg_can_rhos                )
    if(associated(cpoly%avg_can_prss                )) deallocate(cpoly%avg_can_prss                )
    if(associated(cpoly%avg_can_theta               )) deallocate(cpoly%avg_can_theta               )
    if(associated(cpoly%avg_can_theiv               )) deallocate(cpoly%avg_can_theiv               )
    if(associated(cpoly%avg_can_depth               )) deallocate(cpoly%avg_can_depth               )
    if(associated(cpoly%avg_soil_energy             )) deallocate(cpoly%avg_soil_energy             )
    if(associated(cpoly%avg_soil_mstpot             )) deallocate(cpoly%avg_soil_mstpot             )
    if(associated(cpoly%avg_soil_water              )) deallocate(cpoly%avg_soil_water              )
    if(associated(cpoly%avg_soil_temp               )) deallocate(cpoly%avg_soil_temp               )
    if(associated(cpoly%avg_soil_fracliq            )) deallocate(cpoly%avg_soil_fracliq            )
    if(associated(cpoly%avg_soil_rootfrac           )) deallocate(cpoly%avg_soil_rootfrac           )
    if(associated(cpoly%avg_soil_wetness            )) deallocate(cpoly%avg_soil_wetness            )
    if(associated(cpoly%avg_skin_temp               )) deallocate(cpoly%avg_skin_temp               )
    if(associated(cpoly%avg_available_water         )) deallocate(cpoly%avg_available_water         )
    if(associated(cpoly%runoff                      )) deallocate(cpoly%runoff                      )
    if(associated(cpoly%rad_avg                     )) deallocate(cpoly%rad_avg                     )
    ! Meteorological information
    if(associated(cpoly%avg_atm_tmp                 )) deallocate(cpoly%avg_atm_tmp                 )
    if(associated(cpoly%avg_atm_shv                 )) deallocate(cpoly%avg_atm_shv                 )
    if(associated(cpoly%avg_atm_prss                )) deallocate(cpoly%avg_atm_prss                )
    ! NACP
    if(associated(cpoly%avg_sfcw_depth            )) deallocate(cpoly%avg_sfcw_depth            )
    if(associated(cpoly%avg_sfcw_energy           )) deallocate(cpoly%avg_sfcw_energy           )
    if(associated(cpoly%avg_sfcw_mass             )) deallocate(cpoly%avg_sfcw_mass             )
    if(associated(cpoly%avg_sfcw_fracliq          )) deallocate(cpoly%avg_sfcw_fracliq          )
    if(associated(cpoly%avg_sfcw_tempk            )) deallocate(cpoly%avg_sfcw_tempk            )
    if(associated(cpoly%avg_fsc                   )) deallocate(cpoly%avg_fsc                   )
    if(associated(cpoly%avg_stsc                  )) deallocate(cpoly%avg_stsc                  )
    if(associated(cpoly%avg_ssc                   )) deallocate(cpoly%avg_ssc                   )
    if(associated(cpoly%avg_bdead                 )) deallocate(cpoly%avg_bdead                 )
    if(associated(cpoly%avg_balive                )) deallocate(cpoly%avg_balive                )
    if(associated(cpoly%avg_fsn                   )) deallocate(cpoly%avg_fsn                   )
    if(associated(cpoly%avg_msn                   )) deallocate(cpoly%avg_msn                   )

    if(associated(cpoly%daily_pcpg                )) deallocate(cpoly%daily_pcpg                )!JL
    if(associated(cpoly%daily_evap                )) deallocate(cpoly%daily_evap                )!JL
    if(associated(cpoly%daily_transp              )) deallocate(cpoly%daily_transp              )!JL
    if(associated(cpoly%PlantN                    )) deallocate(cpolyPlantN                     )!JL
    if(associated(cpoly%ForestN                   )) deallocate(cpoly%ForestN                   )!JL
    if(associated(cpoly%SoilN                     )) deallocate(cpoly%SoilN                     )!JL
    if(associated(cpoly%total_N_Fixation          )) deallocate(cpoly&total_N_Fixation          )!JL
    if(associated(cpoly%total_DON_loss            )) deallocate(cpoly%total_DON_loss            )!JL
    if(associated(cpoly%total_DIN_loss            )) deallocate(cpoly%total_DIN_loss            )!JL
    if(associated(cpoly%total_Ngas_loss           )) deallocate(cpoly%total_Ngas_loss           )!JL
    if(associated(cpoly%total_N_demand            )) deallocate(cpoly%total_N_demand            )!JL
    if(associated(cpoly%total_N_supply            )) deallocate(cpoly%total_N_supply            )!JL

    if(associated(cpoly%avg_bleaf                 )) deallocate(cpoly%avg_bleaf                 )
    if(associated(cpoly%avg_broot                 )) deallocate(cpoly%avg_broot                 )
    if(associated(cpoly%avg_bsapwood              )) deallocate(cpoly%avg_bsapwood              )
    if(associated(cpoly%avg_bstorage              )) deallocate(cpoly%avg_bstorage              )
    if(associated(cpoly%avg_bseeds                )) deallocate(cpoly%avg_bseeds                )

    if(associated(cpoly%dmean_co2_residual        )) deallocate(cpoly%dmean_co2_residual        )
    if(associated(cpoly%dmean_energy_residual     )) deallocate(cpoly%dmean_energy_residual     )
    if(associated(cpoly%dmean_water_residual      )) deallocate(cpoly%dmean_water_residual      )
    if(associated(cpoly%mmean_co2_residual        )) deallocate(cpoly%mmean_co2_residual        )
    if(associated(cpoly%mmean_energy_residual     )) deallocate(cpoly%mmean_energy_residual     )
    if(associated(cpoly%mmean_water_residual      )) deallocate(cpoly%mmean_water_residual      )

    return
  end subroutine deallocate_polygontype
!============================================================================!
!============================================================================!





!============================================================================!
!============================================================================!
  subroutine deallocate_sitetype(csite)

    implicit none

    type(sitetype),target :: csite
    integer :: ipa
    
    if(associated(csite%paco_id                      )) deallocate(csite%paco_id                      )
    if(associated(csite%paco_n                       )) deallocate(csite%paco_n                       )

    if(associated(csite%dist_type                    )) deallocate(csite%dist_type                    )
    if(associated(csite%age                          )) deallocate(csite%age                          )
    if(associated(csite%area                         )) deallocate(csite%area                         )
    if(associated(csite%laiarea                      )) deallocate(csite%laiarea                      )
    if(associated(csite%fast_soil_C                  )) deallocate(csite%fast_soil_C                  )
    if(associated(csite%slow_soil_C                  )) deallocate(csite%slow_soil_C                  )
    if(associated(csite%structural_soil_C            )) deallocate(csite%structural_soil_C            )
    if(associated(csite%structural_soil_L            )) deallocate(csite%structural_soil_L            )
    if(associated(csite%mineralized_soil_N           )) deallocate(csite%mineralized_soil_N           )
    if(associated(csite%fast_soil_N                  )) deallocate(csite%fast_soil_N                  )
    if(associated(csite%pname                        )) deallocate(csite%pname                        )
    if(associated(csite%sum_dgd                      )) deallocate(csite%sum_dgd                      )
    if(associated(csite%sum_chd                      )) deallocate(csite%sum_chd                      )
    if(associated(csite%plantation                   )) deallocate(csite%plantation                   )
    if(associated(csite%cohort_count                 )) deallocate(csite%cohort_count                 )
    if(associated(csite%can_theiv                    )) deallocate(csite%can_theiv                    )
    if(associated(csite%can_temp                     )) deallocate(csite%can_temp                     )
    if(associated(csite%can_shv                      )) deallocate(csite%can_shv                      )
    if(associated(csite%can_co2                      )) deallocate(csite%can_co2                      )
    if(associated(csite%can_rhos                     )) deallocate(csite%can_rhos                     )
    if(associated(csite%can_prss                     )) deallocate(csite%can_prss                     )
    if(associated(csite%can_theta                    )) deallocate(csite%can_theta                    )
    if(associated(csite%can_depth                    )) deallocate(csite%can_depth                    )
    if(associated(csite%opencan_frac                 )) deallocate(csite%opencan_frac                 )
    if(associated(csite%ggbare                       )) deallocate(csite%ggbare                       )
    if(associated(csite%ggveg                        )) deallocate(csite%ggveg                        )
    if(associated(csite%ggnet                        )) deallocate(csite%ggnet                        )
    if(associated(csite%ggsoil                       )) deallocate(csite%ggsoil                       )
    if(associated(csite%lambda_light                 )) deallocate(csite%lambda_light                 )
    if(associated(csite%dmean_lambda_light           )) deallocate(csite%dmean_lambda_light           )
    if(associated(csite%mmean_lambda_light           )) deallocate(csite%mmean_lambda_light           )
    if(associated(csite%lai                          )) deallocate(csite%lai                          )
    if(associated(csite%wpa                          )) deallocate(csite%wpa                          )
    if(associated(csite%wai                          )) deallocate(csite%wai                          )

    if(associated(csite%sfcwater_mass                )) deallocate(csite%sfcwater_mass                )
    if(associated(csite%sfcwater_energy              )) deallocate(csite%sfcwater_energy              )
    if(associated(csite%sfcwater_depth               )) deallocate(csite%sfcwater_depth               )
    if(associated(csite%rshort_s                     )) deallocate(csite%rshort_s                     )
    if(associated(csite%rshort_s_beam                )) deallocate(csite%rshort_s_beam                )
    if(associated(csite%rshort_s_diffuse             )) deallocate(csite%rshort_s_diffuse             )
    if(associated(csite%sfcwater_tempk               )) deallocate(csite%sfcwater_tempk               )
    if(associated(csite%sfcwater_fracliq             )) deallocate(csite%sfcwater_fracliq             )
    if(associated(csite%nlev_sfcwater                )) deallocate(csite%nlev_sfcwater                )
    if(associated(csite%soil_energy                  )) deallocate(csite%soil_energy                  )
    if(associated(csite%soil_water                   )) deallocate(csite%soil_water                   )
    if(associated(csite%soil_tempk                   )) deallocate(csite%soil_tempk                   )
    if(associated(csite%soil_fracliq                 )) deallocate(csite%soil_fracliq                 )
    if(associated(csite%rootdense                    )) deallocate(csite%rootdense                    )
    if(associated(csite%ground_shv                   )) deallocate(csite%ground_shv                   )
    if(associated(csite%ground_ssh                   )) deallocate(csite%ground_ssh                   )
    if(associated(csite%ground_temp                  )) deallocate(csite%ground_temp                  )
    if(associated(csite%ground_fliq                  )) deallocate(csite%ground_fliq                  )
    if(associated(csite%rough                        )) deallocate(csite%rough                        )
    if(associated(csite%par_l_max                    )) deallocate(csite%par_l_max                    )
    if(associated(csite%par_l_beam_max               )) deallocate(csite%par_l_beam_max               )
    if(associated(csite%par_l_diffuse_max            )) deallocate(csite%par_l_diffuse_max            )
    if(associated(csite%A_o_max                      )) deallocate(csite%A_o_max                      )
    if(associated(csite%A_c_max                      )) deallocate(csite%A_c_max                      )

    if(associated(csite%old_stoma_data_max           )) deallocate(csite%old_stoma_data_max           )
    if(associated(csite%old_stoma_vector_max         )) deallocate(csite%old_stoma_vector_max         )

    if(associated(csite%avg_daily_temp               )) deallocate(csite%avg_daily_temp               )
    if(associated(csite%mean_rh                      )) deallocate(csite%mean_rh                      )
    if(associated(csite%dmean_rh                     )) deallocate(csite%dmean_rh                     )
    if(associated(csite%qmean_rh                     )) deallocate(csite%qmean_rh                     )
    if(associated(csite%mmean_rh                     )) deallocate(csite%mmean_rh                     )
    if(associated(csite%dmean_albedo                 )) deallocate(csite%dmean_albedo                 )
    if(associated(csite%dmean_albedo_beam            )) deallocate(csite%dmean_albedo_beam            )
    if(associated(csite%dmean_albedo_diffuse         )) deallocate(csite%dmean_albedo_diffuse         )
    if(associated(csite%mmean_albedo                 )) deallocate(csite%mmean_albedo                 )
    if(associated(csite%mmean_albedo_beam            )) deallocate(csite%mmean_albedo_beam            )
    if(associated(csite%mmean_albedo_diffuse         )) deallocate(csite%mmean_albedo_diffuse         )
    if(associated(csite%qmean_albedo                 )) deallocate(csite%qmean_albedo                 )
    if(associated(csite%qmean_albedo_beam            )) deallocate(csite%qmean_albedo_beam            )
    if(associated(csite%qmean_albedo_diffuse         )) deallocate(csite%qmean_albedo_diffuse         )
    if(associated(csite%mean_nep                     )) deallocate(csite%mean_nep                     )
    if(associated(csite%wbudget_loss2atm             )) deallocate(csite%wbudget_loss2atm             )
    if(associated(csite%wbudget_denseffect           )) deallocate(csite%wbudget_denseffect           )
    if(associated(csite%wbudget_precipgain           )) deallocate(csite%wbudget_precipgain           )
    if(associated(csite%wbudget_loss2runoff          )) deallocate(csite%wbudget_loss2runoff          )
    if(associated(csite%wbudget_loss2drainage        )) deallocate(csite%wbudget_loss2drainage        )
    if(associated(csite%wbudget_initialstorage       )) deallocate(csite%wbudget_initialstorage       )
    if(associated(csite%wbudget_residual             )) deallocate(csite%wbudget_residual             )
    if(associated(csite%ebudget_loss2atm             )) deallocate(csite%ebudget_loss2atm             )
    if(associated(csite%ebudget_denseffect           )) deallocate(csite%ebudget_denseffect           )
    if(associated(csite%ebudget_loss2runoff          )) deallocate(csite%ebudget_loss2runoff          )
    if(associated(csite%ebudget_loss2drainage        )) deallocate(csite%ebudget_loss2drainage        )
    if(associated(csite%ebudget_netrad               )) deallocate(csite%ebudget_netrad               )
    if(associated(csite%ebudget_precipgain           )) deallocate(csite%ebudget_precipgain           )
    if(associated(csite%ebudget_initialstorage       )) deallocate(csite%ebudget_initialstorage       )
    if(associated(csite%ebudget_residual             )) deallocate(csite%ebudget_residual             )
    if(associated(csite%co2budget_initialstorage     )) deallocate(csite%co2budget_initialstorage     )
    if(associated(csite%co2budget_residual           )) deallocate(csite%co2budget_residual           )
    if(associated(csite%co2budget_loss2atm           )) deallocate(csite%co2budget_loss2atm           )
    if(associated(csite%co2budget_denseffect         )) deallocate(csite%co2budget_denseffect         )
    if(associated(csite%co2budget_gpp                )) deallocate(csite%co2budget_gpp                )
    if(associated(csite%co2budget_gpp_dbh            )) deallocate(csite%co2budget_gpp_dbh            )
    if(associated(csite%co2budget_plresp             )) deallocate(csite%co2budget_plresp             )
    if(associated(csite%co2budget_rh                 )) deallocate(csite%co2budget_rh                 )
    if(associated(csite%today_A_decomp               )) deallocate(csite%today_A_decomp               )
    if(associated(csite%today_Af_decomp              )) deallocate(csite%today_Af_decomp              )
    if(associated(csite%dmean_A_decomp               )) deallocate(csite%dmean_A_decomp               )
    if(associated(csite%dmean_Af_decomp              )) deallocate(csite%dmean_Af_decomp              )
    if(associated(csite%mmean_A_decomp               )) deallocate(csite%mmean_A_decomp               )
    if(associated(csite%mmean_Af_decomp              )) deallocate(csite%mmean_Af_decomp              )
    if(associated(csite%fsc_in_no_excess             )) deallocate(csite%fsc_in_no_excess             )           !JL
    if(associated(csite%interval_DON_loss            )) deallocate(csite%interval_DON_loss            )           !JL
    if(associated(csite%mmean_fsn_in                 )) deallocate(csite%mmean_fsn_in                 )           !JL
    if(associated(csite%mmean_ssc_in                 )) deallocate(csite%mmean_ssc_in                 )           !JL
    if(associated(csite%mmean_total_plant_nitrogen_uptake )) deallocate(csite%mmean_total_plant_nitrogen_uptake ) !JL
    if(associated(csite%N_leached                    )) deallocate(csite%N_leached                    )           !JL
    if(associated(csite%N_gas_loss                   )) deallocate(csite%N_gas_loss                   )           !JL
    if(associated(csite%repro                        )) deallocate(csite%repro                        )
    if(associated(csite%veg_rough                    )) deallocate(csite%veg_rough                    )
    if(associated(csite%veg_height                   )) deallocate(csite%veg_height                   )
    if(associated(csite%veg_displace                 )) deallocate(csite%veg_displace                 )
    if(associated(csite%fsc_in                       )) deallocate(csite%fsc_in                       )
    if(associated(csite%ssc_in                       )) deallocate(csite%ssc_in                       )
    if(associated(csite%ssl_in                       )) deallocate(csite%ssl_in                       )
    if(associated(csite%fsn_in                       )) deallocate(csite%fsn_in                       )
    if(associated(csite%total_plant_nitrogen_uptake  )) deallocate(csite%total_plant_nitrogen_uptake  )
    if(associated(csite%mineralized_N_input          )) deallocate(csite%mineralized_N_input          )
    if(associated(csite%mineralized_N_loss           )) deallocate(csite%mineralized_N_loss           )
    if(associated(csite%rshort_g                     )) deallocate(csite%rshort_g                     )
    if(associated(csite%rshort_g_beam                )) deallocate(csite%rshort_g_beam                )
    if(associated(csite%rshort_g_diffuse             )) deallocate(csite%rshort_g_diffuse             )
    if(associated(csite%rlong_g                      )) deallocate(csite%rlong_g                      )
    if(associated(csite%rlong_g_surf                 )) deallocate(csite%rlong_g_surf                 )
    if(associated(csite%rlong_g_incid                )) deallocate(csite%rlong_g_incid                )
    if(associated(csite%rlong_s                      )) deallocate(csite%rlong_s                      )
    if(associated(csite%rlong_s_surf                 )) deallocate(csite%rlong_s_surf                 )
    if(associated(csite%rlong_s_incid                )) deallocate(csite%rlong_s_incid                )
    if(associated(csite%albedo                       )) deallocate(csite%albedo                       )
    if(associated(csite%albedo_beam                  )) deallocate(csite%albedo_beam                  )
    if(associated(csite%albedo_diffuse               )) deallocate(csite%albedo_diffuse               )
    if(associated(csite%rlongup                      )) deallocate(csite%rlongup                      )
    if(associated(csite%rlong_albedo                 )) deallocate(csite%rlong_albedo                 )
    if(associated(csite%total_sfcw_depth             )) deallocate(csite%total_sfcw_depth             )
    if(associated(csite%snowfac                      )) deallocate(csite%snowfac                      )
    if(associated(csite%A_decomp                     )) deallocate(csite%A_decomp                     )
    if(associated(csite%f_decomp                     )) deallocate(csite%f_decomp                     )
    if(associated(csite%rh                           )) deallocate(csite%rh                           )
    if(associated(csite%cwd_rh                       )) deallocate(csite%cwd_rh                       )
    if(associated(csite%fuse_flag                    )) deallocate(csite%fuse_flag                    )
    if(associated(csite%cumlai_profile               )) deallocate(csite%cumlai_profile               )
    if(associated(csite%plant_ag_biomass             )) deallocate(csite%plant_ag_biomass             )

    if(associated(csite%mean_wflux                   )) deallocate(csite%mean_wflux                   )
    if(associated(csite%mean_latflux                 )) deallocate(csite%mean_latflux                 )
    if(associated(csite%mean_hflux                   )) deallocate(csite%mean_hflux                   )
    if(associated(csite%mean_runoff                  )) deallocate(csite%mean_runoff                  )
    if(associated(csite%mean_qrunoff                 )) deallocate(csite%mean_qrunoff                 )

    if(associated(csite%htry                         )) deallocate(csite%htry                         )
    if(associated(csite%avg_rk4step                  )) deallocate(csite%avg_rk4step                  )
    if(associated(csite%dmean_rk4step                )) deallocate(csite%dmean_rk4step                )
    if(associated(csite%mmean_rk4step                )) deallocate(csite%mmean_rk4step                )

    if(associated(csite%avg_available_water          )) deallocate(csite%avg_available_water          )

    if(associated(csite%ustar                        )) deallocate(csite%ustar                        )
    if(associated(csite%tstar                        )) deallocate(csite%tstar                        )
    if(associated(csite%qstar                        )) deallocate(csite%qstar                        )
    if(associated(csite%cstar                        )) deallocate(csite%cstar                        )

    if(associated(csite%zeta                         )) deallocate(csite%zeta                         )
    if(associated(csite%ribulk                       )) deallocate(csite%ribulk                       )

    if(associated(csite%upwp                         )) deallocate(csite%upwp                         )
    if(associated(csite%qpwp                         )) deallocate(csite%qpwp                         )
    if(associated(csite%cpwp                         )) deallocate(csite%cpwp                         )
    if(associated(csite%tpwp                         )) deallocate(csite%tpwp                         )
    if(associated(csite%wpwp                         )) deallocate(csite%wpwp                         )

    if(associated(csite%avg_rshort_gnd               )) deallocate(csite%avg_rshort_gnd               )
    if(associated(csite%avg_rlong_gnd                )) deallocate(csite%avg_rlong_gnd                )
    if(associated(csite%avg_carbon_ac                )) deallocate(csite%avg_carbon_ac                )
    if(associated(csite%avg_rlongup                  )) deallocate(csite%avg_rlongup                  )
    if(associated(csite%avg_albedo                   )) deallocate(csite%avg_albedo                   )
    if(associated(csite%avg_albedo_beam              )) deallocate(csite%avg_albedo_beam              )
    if(associated(csite%avg_albedo_diffuse           )) deallocate(csite%avg_albedo_diffuse           )
    if(associated(csite%avg_rlong_albedo             )) deallocate(csite%avg_rlong_albedo             )

    if(associated(csite%avg_vapor_lc                 )) deallocate(csite%avg_vapor_lc                 )
    if(associated(csite%avg_vapor_wc                 )) deallocate(csite%avg_vapor_wc                 )
    if(associated(csite%avg_dew_cg                   )) deallocate(csite%avg_dew_cg                   )
    if(associated(csite%avg_vapor_gc                 )) deallocate(csite%avg_vapor_gc                 )
    if(associated(csite%avg_wshed_vg                 )) deallocate(csite%avg_wshed_vg                 )
    if(associated(csite%avg_intercepted              )) deallocate(csite%avg_intercepted              )
    if(associated(csite%avg_throughfall              )) deallocate(csite%avg_throughfall              )
    if(associated(csite%avg_vapor_ac                 )) deallocate(csite%avg_vapor_ac                 )
    if(associated(csite%avg_transp                   )) deallocate(csite%avg_transp                   )
    if(associated(csite%avg_evap                     )) deallocate(csite%avg_evap                     )
    if(associated(csite%avg_smoist_gg                )) deallocate(csite%avg_smoist_gg                )
    if(associated(csite%avg_transloss                )) deallocate(csite%avg_transloss                )
    if(associated(csite%avg_runoff                   )) deallocate(csite%avg_runoff                   )
    if(associated(csite%avg_drainage                 )) deallocate(csite%avg_drainage                 )
    if(associated(csite%avg_drainage_heat            )) deallocate(csite%avg_drainage_heat            )
    if(associated(csite%aux                          )) deallocate(csite%aux                          )
    if(associated(csite%aux_s                        )) deallocate(csite%aux_s                        )
    if(associated(csite%avg_sensible_lc              )) deallocate(csite%avg_sensible_lc              )
    if(associated(csite%avg_sensible_wc              )) deallocate(csite%avg_sensible_wc              )
    if(associated(csite%avg_qwshed_vg                )) deallocate(csite%avg_qwshed_vg                )
    if(associated(csite%avg_qintercepted             )) deallocate(csite%avg_qintercepted             )
    if(associated(csite%avg_qthroughfall             )) deallocate(csite%avg_qthroughfall             )
    if(associated(csite%avg_sensible_gc              )) deallocate(csite%avg_sensible_gc              )
    if(associated(csite%avg_sensible_ac              )) deallocate(csite%avg_sensible_ac              )
    if(associated(csite%avg_sensible_gg              )) deallocate(csite%avg_sensible_gg              )
    if(associated(csite%avg_runoff_heat              )) deallocate(csite%avg_runoff_heat              )
    if(associated(csite%avg_leaf_energy              )) deallocate(csite%avg_leaf_energy              )
    if(associated(csite%avg_leaf_temp                )) deallocate(csite%avg_leaf_temp                )
    if(associated(csite%avg_leaf_hcap                )) deallocate(csite%avg_leaf_hcap                )
    if(associated(csite%avg_leaf_fliq                )) deallocate(csite%avg_leaf_fliq                )
    if(associated(csite%avg_leaf_water               )) deallocate(csite%avg_leaf_water               )
    if(associated(csite%avg_wood_energy              )) deallocate(csite%avg_wood_energy              )
    if(associated(csite%avg_wood_temp                )) deallocate(csite%avg_wood_temp                )
    if(associated(csite%avg_wood_hcap                )) deallocate(csite%avg_wood_hcap                )
    if(associated(csite%avg_wood_fliq                )) deallocate(csite%avg_wood_fliq                )
    if(associated(csite%avg_wood_water               )) deallocate(csite%avg_wood_water               )

    if(associated(csite%watertable                   )) deallocate(csite%watertable                   )
    if(associated(csite%moist_dz                     )) deallocate(csite%moist_dz                     )
    if(associated(csite%ksat                         )) deallocate(csite%ksat                         )
    if(associated(csite%soil_sat_energy              )) deallocate(csite%soil_sat_energy              )
    if(associated(csite%soil_sat_water               )) deallocate(csite%soil_sat_water               )
    if(associated(csite%soil_sat_heat                )) deallocate(csite%soil_sat_heat                )
    if(associated(csite%runoff_A                     )) deallocate(csite%runoff_A                     )
    if(associated(csite%runoff_rate                  )) deallocate(csite%runoff_rate                  )
    if(associated(csite%runoff                       )) deallocate(csite%runoff                       )

    if(associated(csite%current_paw))deallocate(csite%current_paw)
    if(associated(csite%past_paw))deallocate(csite%past_paw)

    if(associated(csite%dmean_co2_residual        )) deallocate(csite%dmean_co2_residual        )
    if(associated(csite%dmean_energy_residual     )) deallocate(csite%dmean_energy_residual     )
    if(associated(csite%dmean_water_residual      )) deallocate(csite%dmean_water_residual      )
    if(associated(csite%mmean_co2_residual        )) deallocate(csite%mmean_co2_residual        )
    if(associated(csite%mmean_energy_residual     )) deallocate(csite%mmean_energy_residual     )
    if(associated(csite%mmean_water_residual      )) deallocate(csite%mmean_water_residual      )

    do ipa=1,csite%npatches
       if (csite%patch(ipa)%ncohorts > 0) call deallocate_patchtype(csite%patch(ipa))
    end do

    if(associated(csite%patch                        )) deallocate(csite%patch                        )

    return
  end subroutine deallocate_sitetype
!============================================================================!
!============================================================================!





!============================================================================!
!============================================================================!
  subroutine deallocate_patchtype(cpatch)

    implicit none

    type(patchtype),target :: cpatch
    
    if (cpatch%ncohorts == 0) return
    
    if(associated(cpatch%pft))                 deallocate(cpatch%pft)
    if(associated(cpatch%nplant))              deallocate(cpatch%nplant)
    if(associated(cpatch%hite))                deallocate(cpatch%hite)
    if(associated(cpatch%agb))                 deallocate(cpatch%agb)
    if(associated(cpatch%basarea))             deallocate(cpatch%basarea)
    if(associated(cpatch%dagb_dt))             deallocate(cpatch%dagb_dt)
    if(associated(cpatch%dba_dt))              deallocate(cpatch%dba_dt)
    if(associated(cpatch%ddbh_dt))             deallocate(cpatch%ddbh_dt)
    if(associated(cpatch%dbh))                 deallocate(cpatch%dbh)
    if(associated(cpatch%bdead))               deallocate(cpatch%bdead)
    if(associated(cpatch%bleaf))               deallocate(cpatch%bleaf)
    if(associated(cpatch%phenology_status))    deallocate(cpatch%phenology_status)
    if(associated(cpatch%balive))              deallocate(cpatch%balive)
    if(associated(cpatch%broot))               deallocate(cpatch%broot)
    if(associated(cpatch%bsapwood))            deallocate(cpatch%bsapwood)
!********************************************************XXT,JL
   if(associated(cpatch%excess_carbon))                deallocate(cpatch%excess_carbon)
   if(associated(cpatch%N_fixation))                   deallocate(cpatch%N_fixation)
   if(associated(cpatch%excess_carbon_fixer))          deallocate(cpatch%excess_carbon_fixer)
   if(associated(cpatch%nitrogen_supply_fixer))        deallocate(cpatch%nitrogen_supply_fixer)
   if(associated(cpatch%nitrogen_supply))              deallocate(cpatch%nitrogen_supply)
   if(associated(cpatch%shadow_N_uptake))              deallocate(cpatch%shadow_N_uptake)
   if(associated(cpatch%actual_nitrogen_uptake))       deallocate(cpatch%actual_nitrogen_uptake)
   if(associated(cpatch%actual_nitrogen_uptake_fixer)) deallocate(cpatch%actual_nitrogen_uptake_fixer)
   if(associated(cpatch%fixation_demand))              deallocate(cpatch%fixation_demand)
   if(associated(cpatch%fsn_fixer ))                   deallocate(cpatch%fsn_fixer )    
   if(associated(cpatch%nstorage))                     deallocate(cpatch%nstorage)
  !if(associated(cpatch%mmean_nitrogen_supply ))      deallocate(cpatch%mmean_nitrogen_supply  )
  !if(associated(cpatch%mmean_N_uptake_pot ))         deallocate(cpatch%mmean_N_uptake_pot  )
  !if(associated(cpatch%excess_carbon))               deallocate(cpatch%excess_carbon)
   if(associated(cpatch%carbon_balance))              deallocate(cpatch%carbon_balance) 
   if(associated(cpatch%N_uptake_pot))                deallocate(cpatch%N_uptake_pot) 
   if(associated(cpatch%nitrogen_uptake))             deallocate(cpatch%nitrogen_uptake) 
   if(associated(cpatch%carbon_balance_pot))          deallocate(cpatch%carbon_balance_pot) 
   if(associated(cpatch%nitrogen_supply_test))        deallocate(cpatch%nitrogen_supply_test) 
   if(associated(cpatch%nitrogen_supply_test2))       deallocate(cpatch%nitrogen_supply_test2) 


!********************************************************
    if(associated(cpatch%lai))                 deallocate(cpatch%lai)
    if(associated(cpatch%wpa))                 deallocate(cpatch%wpa)
    if(associated(cpatch%wai))                 deallocate(cpatch%wai)
    if(associated(cpatch%crown_area))          deallocate(cpatch%crown_area)
    if(associated(cpatch%leaf_resolvable))     deallocate(cpatch%leaf_resolvable)
    if(associated(cpatch%wood_resolvable))     deallocate(cpatch%wood_resolvable)
    if(associated(cpatch%bstorage))            deallocate(cpatch%bstorage)
    if(associated(cpatch%cb))                  deallocate(cpatch%cb)
   !if(associated(cpatch%N_limitation_factor)) deallocate(cpatch%N_limitation_factor)!JL!
   !if(associated(cpatch%N_limitation_factor_bar)) deallocate(cpatch%N_limitation_factor_bar)!JL!
    if(associated(cpatch%cb_max))              deallocate(cpatch%cb_max)
    if(associated(cpatch%cbr_bar))             deallocate(cpatch%cbr_bar)
    if(associated(cpatch%mmean_cb))            deallocate(cpatch%mmean_cb)
    if(associated(cpatch%leaf_energy))         deallocate(cpatch%leaf_energy)
    if(associated(cpatch%leaf_temp  ))         deallocate(cpatch%leaf_temp  )
    if(associated(cpatch%leaf_hcap  ))         deallocate(cpatch%leaf_hcap  )
    if(associated(cpatch%leaf_fliq  ))         deallocate(cpatch%leaf_fliq  )
    if(associated(cpatch%leaf_water ))         deallocate(cpatch%leaf_water )
    if(associated(cpatch%wood_energy))         deallocate(cpatch%wood_energy)
    if(associated(cpatch%wood_temp  ))         deallocate(cpatch%wood_temp  )
    if(associated(cpatch%wood_hcap  ))         deallocate(cpatch%wood_hcap  )
    if(associated(cpatch%wood_fliq  ))         deallocate(cpatch%wood_fliq  )
    if(associated(cpatch%wood_water ))         deallocate(cpatch%wood_water )
    if(associated(cpatch%veg_wind   ))         deallocate(cpatch%veg_wind   )
    if(associated(cpatch%lsfc_shv_open))       deallocate(cpatch%lsfc_shv_open)
    if(associated(cpatch%lsfc_shv_closed))     deallocate(cpatch%lsfc_shv_closed)
    if(associated(cpatch%lsfc_co2_open))       deallocate(cpatch%lsfc_co2_open)
    if(associated(cpatch%lsfc_co2_closed))     deallocate(cpatch%lsfc_co2_closed)
    if(associated(cpatch%lint_shv))            deallocate(cpatch%lint_shv)
    if(associated(cpatch%lint_co2_open))       deallocate(cpatch%lint_co2_open)
    if(associated(cpatch%lint_co2_closed))     deallocate(cpatch%lint_co2_closed)
    if(associated(cpatch%mean_gpp))            deallocate(cpatch%mean_gpp)
    if(associated(cpatch%mean_leaf_resp))      deallocate(cpatch%mean_leaf_resp)
    if(associated(cpatch%mean_root_resp))      deallocate(cpatch%mean_root_resp)
    if(associated(cpatch%mean_growth_resp))    deallocate(cpatch%mean_growth_resp)
    if(associated(cpatch%mean_storage_resp))   deallocate(cpatch%mean_storage_resp)
    if(associated(cpatch%mean_vleaf_resp))     deallocate(cpatch%mean_vleaf_resp)
    if(associated(cpatch%today_leaf_resp))     deallocate(cpatch%today_leaf_resp)
    if(associated(cpatch%today_root_resp))     deallocate(cpatch%today_root_resp)
    if(associated(cpatch%today_gpp))           deallocate(cpatch%today_gpp)
    if(associated(cpatch%today_nppleaf))       deallocate(cpatch%today_nppleaf)
    if(associated(cpatch%today_nppfroot))      deallocate(cpatch%today_nppfroot)
    if(associated(cpatch%today_nppsapwood))    deallocate(cpatch%today_nppsapwood)
    if(associated(cpatch%today_nppcroot))      deallocate(cpatch%today_nppcroot)
    if(associated(cpatch%today_nppseeds))      deallocate(cpatch%today_nppseeds)
    if(associated(cpatch%today_nppwood))       deallocate(cpatch%today_nppwood)
    if(associated(cpatch%today_nppdaily))      deallocate(cpatch%today_nppdaily)
    if(associated(cpatch%today_gpp_pot))       deallocate(cpatch%today_gpp_pot)
    if(associated(cpatch%today_gpp_max))       deallocate(cpatch%today_gpp_max)
    if(associated(cpatch%growth_respiration))  deallocate(cpatch%growth_respiration)
    if(associated(cpatch%storage_respiration)) deallocate(cpatch%storage_respiration)
    if(associated(cpatch%vleaf_respiration))   deallocate(cpatch%vleaf_respiration)
    if(associated(cpatch%dmean_gpp         ))  deallocate(cpatch%dmean_gpp         )
    if(associated(cpatch%dmean_nppleaf    ))   deallocate(cpatch%dmean_nppleaf     )
    if(associated(cpatch%dmean_nppfroot   ))   deallocate(cpatch%dmean_nppfroot    )
    if(associated(cpatch%dmean_nppsapwood ))   deallocate(cpatch%dmean_nppsapwood  )
    if(associated(cpatch%dmean_nppcroot   ))   deallocate(cpatch%dmean_nppcroot    )
    if(associated(cpatch%dmean_nppseeds   ))   deallocate(cpatch%dmean_nppseeds    )
    if(associated(cpatch%dmean_nppwood    ))   deallocate(cpatch%dmean_nppwood     )
    if(associated(cpatch%dmean_nppdaily   ))   deallocate(cpatch%dmean_nppdaily    )
    if(associated(cpatch%dmean_leaf_resp   ))  deallocate(cpatch%dmean_leaf_resp   )
    if(associated(cpatch%dmean_root_resp   ))  deallocate(cpatch%dmean_root_resp   )
    if(associated(cpatch%mmean_gpp         ))  deallocate(cpatch%mmean_gpp         )
    if(associated(cpatch%mmean_nppleaf    ))   deallocate(cpatch%mmean_nppleaf     )
    if(associated(cpatch%mmean_nppfroot   ))   deallocate(cpatch%mmean_nppfroot    )
    if(associated(cpatch%mmean_nppsapwood ))   deallocate(cpatch%mmean_nppsapwood  )
    if(associated(cpatch%mmean_nppcroot   ))   deallocate(cpatch%mmean_nppcroot    )
    if(associated(cpatch%mmean_nppseeds   ))   deallocate(cpatch%mmean_nppseeds    )
    if(associated(cpatch%mmean_nppwood    ))   deallocate(cpatch%mmean_nppwood     )
    if(associated(cpatch%mmean_nppdaily   ))   deallocate(cpatch%mmean_nppdaily    )
    if(associated(cpatch%mmean_leaf_resp   ))  deallocate(cpatch%mmean_leaf_resp   )
    if(associated(cpatch%mmean_root_resp   ))  deallocate(cpatch%mmean_root_resp   )
    if(associated(cpatch%mmean_growth_resp ))  deallocate(cpatch%mmean_growth_resp )
    if(associated(cpatch%mmean_storage_resp))  deallocate(cpatch%mmean_storage_resp)
    if(associated(cpatch%mmean_vleaf_resp  ))  deallocate(cpatch%mmean_vleaf_resp  )
    if(associated(cpatch%fsn))                 deallocate(cpatch%fsn)

    if(associated(cpatch%monthly_dndt))        deallocate(cpatch%monthly_dndt)
    if(associated(cpatch%mort_rate))           deallocate(cpatch%mort_rate)
    if(associated(cpatch%mmean_mort_rate))     deallocate(cpatch%mmean_mort_rate)

    if(associated(cpatch%old_stoma_data))         deallocate(cpatch%old_stoma_data)
    if(associated(cpatch%old_stoma_vector))       deallocate(cpatch%old_stoma_vector)
    if(associated(cpatch%Psi_open))               deallocate(cpatch%Psi_open)
    if(associated(cpatch%krdepth))                deallocate(cpatch%krdepth)
    if(associated(cpatch%first_census))           deallocate(cpatch%first_census)
    if(associated(cpatch%new_recruit_flag))       deallocate(cpatch%new_recruit_flag)
    if(associated(cpatch%light_level))            deallocate(cpatch%light_level)
    if(associated(cpatch%dmean_light_level))      deallocate(cpatch%dmean_light_level)
    if(associated(cpatch%mmean_light_level))      deallocate(cpatch%mmean_light_level)
    if(associated(cpatch%light_level_beam))       deallocate(cpatch%light_level_beam)
    if(associated(cpatch%dmean_light_level_beam)) deallocate(cpatch%dmean_light_level_beam)
    if(associated(cpatch%mmean_light_level_beam)) deallocate(cpatch%mmean_light_level_beam)
    if(associated(cpatch%light_level_diff))       deallocate(cpatch%light_level_diff)
    if(associated(cpatch%dmean_light_level_diff)) deallocate(cpatch%dmean_light_level_diff)
    if(associated(cpatch%mmean_light_level_diff)) deallocate(cpatch%mmean_light_level_diff)
    if(associated(cpatch%beamext_level))          deallocate(cpatch%beamext_level)
    if(associated(cpatch%dmean_beamext_level))    deallocate(cpatch%dmean_beamext_level)
    if(associated(cpatch%mmean_beamext_level))    deallocate(cpatch%mmean_beamext_level)
    if(associated(cpatch%diffext_level))          deallocate(cpatch%diffext_level)
    if(associated(cpatch%dmean_diffext_level))    deallocate(cpatch%dmean_diffext_level)
    if(associated(cpatch%mmean_diffext_level))    deallocate(cpatch%mmean_diffext_level)
    if(associated(cpatch%lambda_light))           deallocate(cpatch%lambda_light)
    if(associated(cpatch%dmean_lambda_light))     deallocate(cpatch%dmean_lambda_light)
    if(associated(cpatch%mmean_lambda_light))     deallocate(cpatch%mmean_lambda_light)
    if(associated(cpatch%par_l))                  deallocate(cpatch%par_l)
    if(associated(cpatch%par_l_beam))             deallocate(cpatch%par_l_beam)
    if(associated(cpatch%par_l_diffuse))          deallocate(cpatch%par_l_diffuse)
    if(associated(cpatch%dmean_par_l))            deallocate(cpatch%dmean_par_l)
    if(associated(cpatch%dmean_par_l_beam))       deallocate(cpatch%dmean_par_l_beam)
    if(associated(cpatch%dmean_par_l_diff))       deallocate(cpatch%dmean_par_l_diff)
    if(associated(cpatch%mmean_par_l))            deallocate(cpatch%mmean_par_l)
    if(associated(cpatch%mmean_par_l_beam))       deallocate(cpatch%mmean_par_l_beam)
    if(associated(cpatch%mmean_par_l_diff))       deallocate(cpatch%mmean_par_l_diff)
    if(associated(cpatch%rshort_l))               deallocate(cpatch%rshort_l)
    if(associated(cpatch%rshort_l_beam))          deallocate(cpatch%rshort_l_beam)
    if(associated(cpatch%rshort_l_diffuse))       deallocate(cpatch%rshort_l_diffuse)
    if(associated(cpatch%rlong_l))                deallocate(cpatch%rlong_l)
    if(associated(cpatch%rlong_l_surf))           deallocate(cpatch%rlong_l_surf)
    if(associated(cpatch%rlong_l_incid))          deallocate(cpatch%rlong_l_incid)
    if(associated(cpatch%rshort_w))               deallocate(cpatch%rshort_w)
    if(associated(cpatch%rshort_w_beam))          deallocate(cpatch%rshort_w_beam)
    if(associated(cpatch%rshort_w_diffuse))       deallocate(cpatch%rshort_w_diffuse)
    if(associated(cpatch%rlong_w))                deallocate(cpatch%rlong_w)
    if(associated(cpatch%rlong_w_surf))           deallocate(cpatch%rlong_w_surf)
    if(associated(cpatch%rlong_w_incid))          deallocate(cpatch%rlong_w_incid)
    if(associated(cpatch%leaf_gbh))               deallocate(cpatch%leaf_gbh)
    if(associated(cpatch%leaf_gbw))               deallocate(cpatch%leaf_gbw)
    if(associated(cpatch%wood_gbh))               deallocate(cpatch%wood_gbh)
    if(associated(cpatch%wood_gbw))               deallocate(cpatch%wood_gbw)
    if(associated(cpatch%A_open))                 deallocate(cpatch%A_open)    
    if(associated(cpatch%A_closed))               deallocate(cpatch%A_closed)
    if(associated(cpatch%Psi_closed))             deallocate(cpatch%Psi_closed)
    if(associated(cpatch%gsw_open))               deallocate(cpatch%gsw_open)
    if(associated(cpatch%gsw_closed))             deallocate(cpatch%gsw_closed)
    if(associated(cpatch%fsw))                    deallocate(cpatch%fsw)
    if(associated(cpatch%fs_open))                deallocate(cpatch%fs_open)
    if(associated(cpatch%water_supply))           deallocate(cpatch%water_supply)
    if(associated(cpatch%dmean_fs_open))          deallocate(cpatch%dmean_fs_open)
    if(associated(cpatch%dmean_fsw))              deallocate(cpatch%dmean_fsw)
    if(associated(cpatch%dmean_fsn))              deallocate(cpatch%dmean_fsn)
    if(associated(cpatch%dmean_psi_open))         deallocate(cpatch%dmean_psi_open)
    if(associated(cpatch%dmean_psi_closed))       deallocate(cpatch%dmean_psi_closed)
    if(associated(cpatch%dmean_water_supply))     deallocate(cpatch%dmean_water_supply)
    if(associated(cpatch%mmean_fs_open))          deallocate(cpatch%mmean_fs_open)
    if(associated(cpatch%mmean_fsw))              deallocate(cpatch%mmean_fsw)
    if(associated(cpatch%mmean_fsn))              deallocate(cpatch%mmean_fsn)
    if(associated(cpatch%mmean_psi_open))         deallocate(cpatch%mmean_psi_open)
    if(associated(cpatch%mmean_psi_closed))       deallocate(cpatch%mmean_psi_closed)
    if(associated(cpatch%mmean_water_supply))     deallocate(cpatch%mmean_water_supply)
    if(associated(cpatch%stomatal_conductance))   deallocate(cpatch%stomatal_conductance)
    if(associated(cpatch%leaf_maintenance))       deallocate(cpatch%leaf_maintenance)
    if(associated(cpatch%root_maintenance))       deallocate(cpatch%root_maintenance)
    if(associated(cpatch%mmean_leaf_maintenance)) deallocate(cpatch%mmean_leaf_maintenance)
    if(associated(cpatch%mmean_root_maintenance)) deallocate(cpatch%mmean_root_maintenance)
    if(associated(cpatch%leaf_drop))              deallocate(cpatch%leaf_drop)
    if(associated(cpatch%mmean_leaf_drop))        deallocate(cpatch%mmean_leaf_drop)
    if(associated(cpatch%bseeds))                 deallocate(cpatch%bseeds)
    if(associated(cpatch%leaf_respiration))       deallocate(cpatch%leaf_respiration)
    if(associated(cpatch%root_respiration))       deallocate(cpatch%root_respiration)
    if(associated(cpatch%gpp))                    deallocate(cpatch%gpp)
    if(associated(cpatch%paw_avg))                deallocate(cpatch%paw_avg)
    if(associated(cpatch%elongf))                 deallocate(cpatch%elongf)
    if(associated(cpatch%turnover_amp))           deallocate(cpatch%turnover_amp)
    if(associated(cpatch%llspan))                 deallocate(cpatch%llspan)
    if(associated(cpatch%vm_bar))                 deallocate(cpatch%vm_bar)
    if(associated(cpatch%sla))                    deallocate(cpatch%sla)

    if(associated(cpatch%qmean_par_l       )) deallocate(cpatch%qmean_par_l       )
    if(associated(cpatch%qmean_par_l_beam  )) deallocate(cpatch%qmean_par_l_beam  )
    if(associated(cpatch%qmean_par_l_diff  )) deallocate(cpatch%qmean_par_l_diff  )
    if(associated(cpatch%qmean_fs_open     )) deallocate(cpatch%qmean_fs_open     )
    if(associated(cpatch%qmean_fsw         )) deallocate(cpatch%qmean_fsw         )
    if(associated(cpatch%qmean_fsn         )) deallocate(cpatch%qmean_fsn         )
    if(associated(cpatch%qmean_psi_open    )) deallocate(cpatch%qmean_psi_open    )
    if(associated(cpatch%qmean_psi_closed  )) deallocate(cpatch%qmean_psi_closed  )
    if(associated(cpatch%qmean_water_supply)) deallocate(cpatch%qmean_water_supply)
    if(associated(cpatch%qmean_gpp         )) deallocate(cpatch%qmean_gpp         )
    if(associated(cpatch%qmean_leaf_resp   )) deallocate(cpatch%qmean_leaf_resp   )
    if(associated(cpatch%qmean_root_resp   )) deallocate(cpatch%qmean_root_resp   )

    return
  end subroutine deallocate_patchtype
!============================================================================!
!============================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This subroutine copies the patches from the input site to the output.   The       !
   ! number of patches to be copied should be always match between the input and output,   !
   ! and every single variable, including the cohorts, will be copied.                     !
   ! IMPORTANT.  This subroutine assumes that the output patches still don't have cohorts  !
   !             allocated, so this should be never used in a previously allocated patch.  !
   !---------------------------------------------------------------------------------------!
   subroutine copy_sitetype(isite,osite,ipaa,ipaz,opaa,opaz)
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(sitetype)  , target     :: isite ! Input  (donor) site
      type(sitetype)  , target     :: osite ! Output (receptor) site
      integer         , intent(in) :: ipaa  ! First input  patch index
      integer         , intent(in) :: ipaz  ! Last  input  patch index
      integer         , intent(in) :: opaa  ! First output patch index
      integer         , intent(in) :: opaz  ! Last  output patch index
      !----- Local variables. -------------------------------------------------------------!
      type(stoma_data), pointer    :: osdi  ! Old stomate data - input patch
      type(stoma_data), pointer    :: osdo  ! Old stomate data - output patch
      integer                      :: ipa   ! Counter for the input  site patches
      integer                      :: opa   ! Counter for the output site patches
      integer                      :: k     ! Vertical layer counter
      integer                      :: ipft  ! PFT counter
      integer                      :: isto  ! Stomate attribute counter
      integer                      :: idbh  ! DBH counter
      integer                      :: ihgt  ! Height counter
      integer                      :: icyc  ! Time of day counter
      !------------------------------------------------------------------------------------!


      !------ Check whether this patch copying makes sense. -------------------------------!
      if (ipaz - ipaa /= opaz - opaa) then
         write (unit=*,fmt='(a)')       '------------------------------------------------'
         write (unit=*,fmt='(a)')       ' - Input site:'
         write (unit=*,fmt='(a,1x,i6)') '   * First patch: ',ipaa
         write (unit=*,fmt='(a,1x,i6)') '   * Last  patch: ',ipaz
         write (unit=*,fmt='(a,1x,i6)') '   * Patch count: ',ipaz-ipaa+1
         write (unit=*,fmt='(a)')       ' - Output site:'
         write (unit=*,fmt='(a,1x,i6)') '   * First patch: ',opaa
         write (unit=*,fmt='(a,1x,i6)') '   * Last  patch: ',opaz
         write (unit=*,fmt='(a,1x,i6)') '   * Patch count: ',opaz-opaa+1
         write (unit=*,fmt='(a)'      ) '------------------------------------------------'
         call fatal_error ('Patch count of input and output paths don''t match'            &
                          ,'copy_sitetype','ed_state_vars.f90')
      end if
      !------------------------------------------------------------------------------------!

      ipa = ipaa - 1
      opaloop: do opa = opaa, opaz
         ipa = ipa + 1

         osite%paco_id(opa)                     = isite%paco_id(ipa)
         osite%paco_n(opa)                      = isite%paco_n(ipa)
         osite%dist_type(opa)                   = isite%dist_type(ipa)
         osite%age(opa)                         = isite%age(ipa)
         osite%area(opa)                        = isite%area(ipa)
         osite%fast_soil_C(opa)                 = isite%fast_soil_C(ipa)
         osite%slow_soil_C(opa)                 = isite%slow_soil_C(ipa)
         osite%structural_soil_C(opa)           = isite%structural_soil_C(ipa)
         osite%structural_soil_L(opa)           = isite%structural_soil_L(ipa)
         osite%mineralized_soil_N(opa)          = isite%mineralized_soil_N(ipa)
         osite%N_leached(opa)                   = isite%N_leached(ipa)                      !JL
         osite%N_gas_loss(opa)                  = isite%N_gas_loss(ipa)                     !JL
         osite%fsc_in_no_excess (opa)           = isite%fsc_in_no_excess (ipa)              !JL
         osite%interval_DON_loss(opa)           = isite%interval_DON_loss(ipa)              !JL
         osite%fast_soil_N(opa)                 = isite%fast_soil_N(ipa)
         osite%sum_dgd(opa)                     = isite%sum_dgd(ipa)
         osite%sum_chd(opa)                     = isite%sum_chd(ipa)
         osite%plantation(opa)                  = isite%plantation(ipa)
         osite%cohort_count(opa)                = isite%cohort_count(ipa)
         osite%can_theiv(opa)                   = isite%can_theiv(ipa)
         osite%can_temp(opa)                    = isite%can_temp(ipa)
         osite%can_shv(opa)                     = isite%can_shv(ipa)
         osite%can_co2(opa)                     = isite%can_co2(ipa)
         osite%can_rhos(opa)                    = isite%can_rhos(ipa)
         osite%can_prss(opa)                    = isite%can_prss(ipa)
         osite%can_theta(opa)                   = isite%can_theta(ipa)
         osite%can_depth(opa)                   = isite%can_depth(ipa)
         osite%opencan_frac(opa)                = isite%opencan_frac(ipa)
         osite%ggbare(opa)                      = isite%ggbare(ipa)
         osite%ggveg(opa)                       = isite%ggveg(ipa)
         osite%ggnet(opa)                       = isite%ggnet(ipa)
         osite%ggsoil(opa)                      = isite%ggsoil(ipa)
         osite%lambda_light(opa)                = isite%lambda_light(ipa)
         osite%lai(opa)                         = isite%lai(ipa)
         osite%wpa(opa)                         = isite%wpa(ipa)
         osite%wai(opa)                         = isite%wai(ipa)
         osite%avg_daily_temp(opa)              = isite%avg_daily_temp(ipa)
         osite%mean_rh(opa)                     = isite%mean_rh(ipa)
         osite%mean_nep(opa)                    = isite%mean_nep(ipa)
         osite%wbudget_loss2atm(opa)            = isite%wbudget_loss2atm(ipa)
         osite%wbudget_denseffect(opa)          = isite%wbudget_denseffect(ipa)
         osite%wbudget_precipgain(opa)          = isite%wbudget_precipgain(ipa)
         osite%wbudget_loss2runoff(opa)         = isite%wbudget_loss2runoff(ipa)
         osite%wbudget_loss2drainage(opa)       = isite%wbudget_loss2drainage(ipa)
         osite%wbudget_initialstorage(opa)      = isite%wbudget_initialstorage(ipa)
         osite%wbudget_residual(opa)            = isite%wbudget_residual(ipa)
         osite%ebudget_loss2atm(opa)            = isite%ebudget_loss2atm(ipa)
         osite%ebudget_denseffect(opa)          = isite%ebudget_denseffect(ipa)
         osite%ebudget_loss2runoff(opa)         = isite%ebudget_loss2runoff(ipa)
         osite%ebudget_loss2drainage(opa)       = isite%ebudget_loss2drainage(ipa)
         osite%ebudget_netrad(opa)              = isite%ebudget_netrad(ipa)
         osite%ebudget_precipgain(opa)          = isite%ebudget_precipgain(ipa)
         osite%ebudget_initialstorage(opa)      = isite%ebudget_initialstorage(ipa)
         osite%ebudget_residual(opa)            = isite%ebudget_residual(ipa)
         osite%co2budget_initialstorage(opa)    = isite%co2budget_initialstorage(ipa)
         osite%co2budget_residual(opa)          = isite%co2budget_residual(ipa)
         osite%co2budget_loss2atm(opa)          = isite%co2budget_loss2atm(ipa)
         osite%co2budget_denseffect(opa)        = isite%co2budget_denseffect(ipa)
         osite%co2budget_gpp(opa)               = isite%co2budget_gpp(ipa)
         osite%co2budget_plresp(opa)            = isite%co2budget_plresp(ipa)
         osite%co2budget_rh(opa)                = isite%co2budget_rh(ipa)
         osite%today_A_decomp(opa)              = isite%today_A_decomp(ipa)
         osite%today_Af_decomp(opa)             = isite%today_Af_decomp(ipa)
         osite%veg_rough(opa)                   = isite%veg_rough(ipa)
         osite%veg_height(opa)                  = isite%veg_height(ipa)
         osite%veg_displace(opa)                = isite%veg_displace(ipa)
         osite%fsc_in(opa)                      = isite%fsc_in(ipa)
         osite%ssc_in(opa)                      = isite%ssc_in(ipa)
         osite%ssl_in(opa)                      = isite%ssl_in(ipa)
         osite%fsn_in(opa)                      = isite%fsn_in(ipa)
         osite%total_plant_nitrogen_uptake(opa) = isite%total_plant_nitrogen_uptake(ipa)
         osite%mineralized_N_loss(opa)          = isite%mineralized_N_loss(ipa)
         osite%mineralized_N_input(opa)         = isite%mineralized_N_input(ipa)
         osite%rshort_g(opa)                    = isite%rshort_g(ipa)
         osite%rshort_g_beam(opa)               = isite%rshort_g_beam(ipa)
         osite%rshort_g_diffuse(opa)            = isite%rshort_g_diffuse(ipa)
         osite%rlong_g(opa)                     = isite%rlong_g(ipa)
         osite%rlong_g_surf(opa)                = isite%rlong_g_surf(ipa)
         osite%rlong_g_incid(opa)               = isite%rlong_g_incid(ipa)
         osite%rlong_s(opa)                     = isite%rlong_s(ipa)
         osite%rlong_s_surf(opa)                = isite%rlong_s_surf(ipa)
         osite%rlong_s_incid(opa)               = isite%rlong_s_incid(ipa)
         osite%albedo(opa)                      = isite%albedo(ipa)
         osite%albedo_beam(opa)                 = isite%albedo_beam(ipa)
         osite%albedo_diffuse(opa)              = isite%albedo_diffuse(ipa)
         osite%rlongup(opa)                     = isite%rlongup(ipa)
         osite%rlong_albedo(opa)                = isite%rlong_albedo(ipa)
         osite%total_sfcw_depth(opa)            = isite%total_sfcw_depth(ipa)
         osite%snowfac(opa)                     = isite%snowfac(ipa)
         osite%A_decomp(opa)                    = isite%A_decomp(ipa)
         osite%f_decomp(opa)                    = isite%f_decomp(ipa)
         osite%rh(opa)                          = isite%rh(ipa)
         osite%cwd_rh(opa)                      = isite%cwd_rh(ipa)
         osite%fuse_flag(opa)                   = isite%fuse_flag(ipa)
         osite%plant_ag_biomass(opa)            = isite%plant_ag_biomass(ipa)
         osite%mean_wflux(opa)                  = isite%mean_wflux(ipa)
         osite%mean_latflux(opa)                = isite%mean_latflux(ipa)
         osite%mean_hflux(opa)                  = isite%mean_hflux(ipa)
         osite%runoff(opa)                      = isite%runoff(ipa)
         osite%mean_runoff(opa)                 = isite%mean_runoff(ipa)
         osite%mean_qrunoff(opa)                = isite%mean_qrunoff(ipa)
         osite%htry(opa)                        = isite%htry(ipa)
         osite%avg_rk4step(opa)                 = isite%avg_rk4step(ipa)
         osite%avg_available_water(opa)         = isite%avg_available_water(ipa)
         osite%ustar(opa)                       = isite%ustar(ipa)
         osite%tstar(opa)                       = isite%tstar(ipa)
         osite%qstar(opa)                       = isite%qstar(ipa)
         osite%cstar(opa)                       = isite%cstar(ipa)

         osite%zeta(opa)                        = isite%zeta(ipa)
         osite%ribulk(opa)                      = isite%ribulk(ipa)

         osite%upwp(opa)                        = isite%upwp(ipa)
         osite%tpwp(opa)                        = isite%tpwp(ipa)
         osite%qpwp(opa)                        = isite%qpwp(ipa)
         osite%cpwp(opa)                        = isite%cpwp(ipa)
         osite%wpwp(opa)                        = isite%wpwp(ipa)

         osite%nlev_sfcwater(opa)               = isite%nlev_sfcwater(ipa)
         osite%ground_shv(opa)                  = isite%ground_shv(ipa)
         osite%ground_ssh(opa)                  = isite%ground_ssh(ipa)
         osite%ground_temp(opa)                 = isite%ground_temp(ipa)
         osite%ground_fliq(opa)                 = isite%ground_fliq(ipa)
         osite%rough(opa)                       = isite%rough(ipa)

         osite%avg_rshort_gnd     (opa)         = isite%avg_rshort_gnd     (ipa)
         osite%avg_rlong_gnd      (opa)         = isite%avg_rlong_gnd      (ipa)
         osite%avg_carbon_ac      (opa)         = isite%avg_carbon_ac      (ipa)
         osite%avg_rlongup        (opa)         = isite%avg_rlongup        (ipa)
         osite%avg_albedo         (opa)         = isite%avg_albedo         (ipa)
         osite%avg_albedo_beam    (opa)         = isite%avg_albedo_beam    (ipa)
         osite%avg_albedo_diffuse (opa)         = isite%avg_albedo_diffuse (ipa)
         osite%avg_rlong_albedo   (opa)         = isite%avg_rlong_albedo   (ipa)

         osite%avg_vapor_lc(opa)                = isite%avg_vapor_lc(ipa)
         osite%avg_vapor_wc(opa)                = isite%avg_vapor_wc(ipa)
         osite%avg_dew_cg(opa)                  = isite%avg_dew_cg(ipa)
         osite%avg_vapor_gc(opa)                = isite%avg_vapor_gc(ipa)
         osite%avg_wshed_vg(opa)                = isite%avg_wshed_vg(ipa)
         osite%avg_intercepted(opa)             = isite%avg_intercepted(ipa)
         osite%avg_throughfall(opa)             = isite%avg_throughfall(ipa)
         osite%avg_vapor_ac(opa)                = isite%avg_vapor_ac(ipa)
         osite%avg_transp(opa)                  = isite%avg_transp(ipa)
         osite%avg_evap(opa)                    = isite%avg_evap(ipa)
         osite%avg_runoff(opa)                  = isite%avg_runoff(ipa)
         osite%avg_drainage(opa)                = isite%avg_drainage(ipa)
         osite%avg_drainage_heat(opa)           = isite%avg_drainage_heat(ipa)
         osite%aux(opa)                         = isite%aux(ipa)
         osite%avg_sensible_lc(opa)             = isite%avg_sensible_lc(ipa)
         osite%avg_sensible_wc(opa)             = isite%avg_sensible_wc(ipa)
         osite%avg_qwshed_vg(opa)               = isite%avg_qwshed_vg(ipa)
         osite%avg_qintercepted(opa)            = isite%avg_qintercepted(ipa)
         osite%avg_qthroughfall(opa)            = isite%avg_qthroughfall(ipa)
         osite%avg_sensible_gc(opa)             = isite%avg_sensible_gc(ipa)
         osite%avg_sensible_ac(opa)             = isite%avg_sensible_ac(ipa)
         osite%avg_runoff_heat(opa)             = isite%avg_runoff_heat(ipa)
         osite%avg_leaf_energy(opa)             = isite%avg_leaf_energy(ipa)
         osite%avg_leaf_temp(opa)               = isite%avg_leaf_temp(ipa)
         osite%avg_leaf_hcap(opa)               = isite%avg_leaf_hcap(ipa)
         osite%avg_leaf_fliq(opa)               = isite%avg_leaf_fliq(ipa)
         osite%avg_leaf_water(opa)              = isite%avg_leaf_water(ipa)
         osite%avg_wood_energy(opa)             = isite%avg_wood_energy(ipa)
         osite%avg_wood_temp(opa)               = isite%avg_wood_temp(ipa)
         osite%avg_wood_hcap(opa)               = isite%avg_wood_hcap(ipa)
         osite%avg_wood_fliq(opa)               = isite%avg_wood_fliq(ipa)
         osite%avg_wood_water(opa)              = isite%avg_wood_water(ipa)
         osite%par_l_max(opa)                   = isite%par_l_max(ipa)
         osite%par_l_beam_max(opa)              = isite%par_l_beam_max(ipa)
         osite%par_l_diffuse_max(opa)           = isite%par_l_diffuse_max(ipa)

         !----- Copy the temporary surface water variables. -------------------------------!
         do k=1,nzs
            osite%sfcwater_mass(k,opa)          = isite%sfcwater_mass(k,ipa)
            osite%sfcwater_energy(k,opa)        = isite%sfcwater_energy(k,ipa)
            osite%sfcwater_depth(k,opa)         = isite%sfcwater_depth(k,ipa)
            osite%rshort_s(k,opa)               = isite%rshort_s(k,ipa)
            osite%rshort_s_beam(k,opa)          = isite%rshort_s_beam(k,ipa)
            osite%rshort_s_diffuse(k,opa)       = isite%rshort_s_diffuse(k,ipa)
            osite%sfcwater_tempk(k,opa)         = isite%sfcwater_tempk(k,ipa)
            osite%sfcwater_fracliq(k,opa)       = isite%sfcwater_fracliq(k,ipa)
         end do

         !----- Copy the soil variables. --------------------------------------------------!
         do k=1,nzg
            osite%soil_energy(k,opa)            =  isite%soil_energy(k,ipa)
            osite%soil_water(k,opa)             =  isite%soil_water(k,ipa)
            osite%soil_tempk(k,opa)             =  isite%soil_tempk(k,ipa)
            osite%soil_fracliq(k,opa)           =  isite%soil_fracliq(k,ipa)
            osite%rootdense(k,opa)              =  isite%rootdense(k,ipa)
            osite%avg_smoist_gg(k,opa)          =  isite%avg_smoist_gg(k,ipa)
            osite%avg_transloss(k,opa)          =  isite%avg_transloss(k,ipa)
            osite%avg_sensible_gg(k,opa)        =  isite%avg_sensible_gg(k,ipa)
            osite%aux_s(k,opa)                  =  isite%aux_s(k,ipa)

            osite%current_paw(k,opa) = isite%current_paw(k,ipa)
            do ihgt = 1, 10
               osite%past_paw(k,ihgt,opa) = isite%past_paw(k,ihgt,ipa)
            enddo
         end do

         !----- PFT types. ----------------------------------------------------------------!
         do ipft=1,n_pft
            osite%repro(ipft,opa)                =  isite%repro(ipft,ipa)
            osite%A_o_max(ipft,opa)              =  isite%A_o_max(ipft,ipa)
            osite%A_c_max(ipft,opa)              =  isite%A_c_max(ipft,ipa)
            
            do isto=1,n_stoma_atts
               osite%old_stoma_vector_max(isto,ipft,opa) =                                 &
                                                  isite%old_stoma_vector_max(isto,ipft,ipa)
            end do

            do ihgt=1,ff_nhgt
               osite%cumlai_profile(ipft,ihgt,opa) = isite%cumlai_profile(ipft,ihgt,ipa)
            end do
            
            !----- This is to copy the old_stoma_data_max structure. ----------------------!
            osdo => osite%old_stoma_data_max(ipft,opa)
            osdi => isite%old_stoma_data_max(ipft,ipa)
           
            osdo%recalc           = osdi%recalc
            osdo%T_L              = osdi%T_L
            osdo%e_A              = osdi%e_A
            osdo%PAR              = osdi%PAR
            osdo%rb_factor        = osdi%rb_factor
            osdo%prss             = osdi%prss
            osdo%phenology_factor = osdi%phenology_factor
            osdo%gsw_open         = osdi%gsw_open
            osdo%ilimit           = osdi%ilimit
            osdo%T_L_residual     = osdi%T_L_residual
            osdo%e_a_residual     = osdi%e_a_residual
            osdo%par_residual     = osdi%par_residual
            osdo%rb_residual      = osdi%rb_residual
            osdo%leaf_residual    = osdi%leaf_residual
            osdo%gsw_residual     = osdi%gsw_residual
         end do

         !----- DBH types. ----------------------------------------------------------------!
         do idbh=1,n_dbh
            osite%co2budget_gpp_dbh(idbh,opa) = isite%co2budget_gpp_dbh(idbh,ipa)
         end do

         !----- Daily averages. -----------------------------------------------------------!
         if (idoutput > 0 .or. imoutput > 0 .or. iqoutput > 0) then
            osite%dmean_rh             (opa) = isite%dmean_rh             (ipa)
            osite%dmean_co2_residual   (opa) = isite%dmean_co2_residual   (ipa)
            osite%dmean_energy_residual(opa) = isite%dmean_energy_residual(ipa)
            osite%dmean_water_residual (opa) = isite%dmean_water_residual (ipa)
            osite%dmean_lambda_light   (opa) = isite%dmean_lambda_light   (ipa)
            osite%dmean_A_decomp       (opa) = isite%dmean_A_decomp       (ipa)
            osite%dmean_Af_decomp      (opa) = isite%dmean_Af_decomp      (ipa)
            osite%dmean_rk4step        (opa) = isite%dmean_rk4step        (ipa)
            osite%dmean_albedo         (opa) = isite%dmean_albedo         (ipa)
            osite%dmean_albedo_beam    (opa) = isite%dmean_albedo_beam    (ipa)
            osite%dmean_albedo_diffuse (opa) = isite%dmean_albedo_diffuse (ipa)
         end if

         if (imoutput > 0 .or. iqoutput > 0) then
            osite%mmean_rh             (opa) = isite%mmean_rh             (ipa)
            osite%mmean_co2_residual   (opa) = isite%mmean_co2_residual   (ipa)
            osite%mmean_energy_residual(opa) = isite%mmean_energy_residual(ipa)
            osite%mmean_water_residual (opa) = isite%mmean_water_residual (ipa)
            osite%mmean_lambda_light   (opa) = isite%mmean_lambda_light   (ipa)
            osite%mmean_A_decomp       (opa) = isite%mmean_A_decomp       (ipa)
            osite%mmean_Af_decomp      (opa) = isite%mmean_Af_decomp      (ipa)
            osite%mmean_fsn_in         (opa) = isite%mmean_fsn_in         (ipa)                         !JL
            osite%mmean_ssc_in         (opa) = isite%mmean_ssc_in         (ipa)                         !JL
            osite%mmean_total_plant_nitrogen_uptake(opa) = isite%mmean_total_plant_nitrogen_uptake(ipa) !JL
            osite%mmean_rk4step        (opa) = isite%mmean_rk4step        (ipa)
            osite%mmean_albedo         (opa) = isite%mmean_albedo         (ipa)
            osite%mmean_albedo_beam    (opa) = isite%mmean_albedo_beam    (ipa)
            osite%mmean_albedo_diffuse (opa) = isite%mmean_albedo_diffuse (ipa)
         end if

         if (iqoutput > 0) then
            do icyc=1,ndcycle
               osite%qmean_rh             (icyc,opa) = isite%qmean_rh             (icyc,ipa)
               osite%qmean_albedo         (icyc,opa) = isite%qmean_albedo         (icyc,ipa)
               osite%qmean_albedo_beam    (icyc,opa) = isite%qmean_albedo_beam    (icyc,ipa)
               osite%qmean_albedo_diffuse (icyc,opa) = isite%qmean_albedo_diffuse (icyc,ipa)
            end do
         end if

         !----- Copy all cohorts. ---------------------------------------------------------!
         call allocate_patchtype(osite%patch(opa),isite%patch(ipa)%ncohorts)
         call copy_patchtype(isite%patch(ipa),osite%patch(opa),1,isite%patch(ipa)%ncohorts &
                                                              ,1,isite%patch(ipa)%ncohorts)
      end do opaloop

      return
   end subroutine copy_sitetype
   !=======================================================================================!
   !=======================================================================================!





!============================================================================!
!============================================================================!
  subroutine copy_sitetype_mask(sitein,siteout,logmask,masksz,newsz)

    ! This subroutine assumes that the size of vectors in siteout
    ! are the number of true elements in mask, while the size of the
    ! vectors in sitein are of the size of the mask itself. If this
    ! is not true, you will get a segmentation violation and the
    ! code will crash.args 1 and 3 must be dimension of arg 4
    ! argument 2 must be the dimension of the sum of the 3rd argument
    ! 
    ! THIS ROUTINE CURRENTLY ASSUMES THAT THE OUTPUT SITE
    ! HAS NOT ALLOCATED IT'S PATCH'S COHORT VECTORS YET, THIS
    ! IS BECAUSE THE LENGTHS OF THESE VECTORS ARE BASED ON THE
    ! DONOR PATH'S VECTOR SIZES.  DO NOT USE PRE-ALLOCATED
    ! RECIPIENTS

    implicit none

    type(sitetype),target :: sitein,siteout
    integer :: masksz,newsz
    integer,dimension(newsz) :: incmask
    logical,dimension(masksz)           :: logmask
    integer :: i,k,m,inc,ipft,icyc
    type(stoma_data),pointer :: osdi,osdo

    inc = 0
    do i=1,masksz
       if (logmask(i)) then
          inc = inc + 1
          incmask(inc) = i
       end if
    end do

    ! First do all of the true vectors
    siteout%paco_id(1:inc)              = pack(sitein%paco_id,logmask)
    siteout%paco_n(1:inc)               = pack(sitein%paco_n,logmask)
    siteout%dist_type(1:inc)            = pack(sitein%dist_type,logmask)
    siteout%age(1:inc)                  = pack(sitein%age,logmask)
    siteout%area(1:inc)                 = pack(sitein%area,logmask)
    siteout%fast_soil_C(1:inc)          = pack(sitein%fast_soil_C,logmask)
    siteout%slow_soil_C(1:inc)          = pack(sitein%slow_soil_C,logmask)
    siteout%structural_soil_C(1:inc)    = pack(sitein%structural_soil_C,logmask)
    siteout%structural_soil_L(1:inc)    = pack(sitein%structural_soil_L,logmask)
    siteout%mineralized_soil_N(1:inc)   = pack(sitein%mineralized_soil_N,logmask)
    siteout%N_leached(1:inc)            = pack(sitein%N_leached,logmask)         !JL
    siteout%N_gas_loss(1:inc)           = pack(sitein%N_gas_loss,logmask)        !JL
    siteout%fsc_in_no_excess (1:inc)    = pack(sitein%fsc_in_no_excess ,logmask) !JL
    siteout%interval_DON_loss(1:inc)    = pack(sitein%interval_DON_loss,logmask) !JL
    siteout%fast_soil_N(1:inc)          = pack(sitein%fast_soil_N,logmask)
    siteout%sum_dgd(1:inc)              = pack(sitein%sum_dgd,logmask)
    siteout%sum_chd(1:inc)              = pack(sitein%sum_chd,logmask)
    siteout%plantation(1:inc)           = pack(sitein%plantation,logmask)
    siteout%cohort_count(1:inc)         = pack(sitein%cohort_count,logmask)
    siteout%can_theiv(1:inc)            = pack(sitein%can_theiv,logmask)
    siteout%can_temp(1:inc)             = pack(sitein%can_temp,logmask)
    siteout%can_shv(1:inc)              = pack(sitein%can_shv,logmask)
    siteout%can_co2(1:inc)              = pack(sitein%can_co2,logmask)
    siteout%can_rhos(1:inc)             = pack(sitein%can_rhos,logmask)
    siteout%can_prss(1:inc)             = pack(sitein%can_prss,logmask)
    siteout%can_theta(1:inc)            = pack(sitein%can_theta,logmask)
    siteout%can_depth(1:inc)            = pack(sitein%can_depth,logmask)
    siteout%opencan_frac(1:inc)         = pack(sitein%opencan_frac,logmask)
    siteout%ggbare(1:inc)               = pack(sitein%ggbare,logmask)
    siteout%ggveg(1:inc)                = pack(sitein%ggveg,logmask)
    siteout%ggnet(1:inc)                = pack(sitein%ggnet,logmask)
    siteout%ggsoil(1:inc)               = pack(sitein%ggsoil,logmask)
    siteout%lambda_light(1:inc)         = pack(sitein%lambda_light,logmask)
    siteout%lai(1:inc)                  = pack(sitein%lai,logmask)
    siteout%wpa(1:inc)                  = pack(sitein%wpa,logmask)
    siteout%wai(1:inc)                  = pack(sitein%wai,logmask)
    siteout%avg_daily_temp(1:inc)       = pack(sitein%avg_daily_temp,logmask)
    siteout%mean_rh(1:inc)              = pack(sitein%mean_rh,logmask)
    siteout%mean_nep(1:inc)             = pack(sitein%mean_nep,logmask)
    siteout%wbudget_loss2atm(1:inc)     = pack(sitein%wbudget_loss2atm,logmask)
    siteout%wbudget_denseffect(1:inc)        = pack(sitein%wbudget_denseffect,logmask)
    siteout%wbudget_precipgain(1:inc)        = pack(sitein%wbudget_precipgain,logmask)
    siteout%wbudget_loss2runoff(1:inc)       = pack(sitein%wbudget_loss2runoff,logmask)
    siteout%wbudget_loss2drainage(1:inc)     = pack(sitein%wbudget_loss2drainage,logmask)
    siteout%wbudget_initialstorage(1:inc)    = pack(sitein%wbudget_initialstorage,logmask)
    siteout%wbudget_residual(1:inc)          = pack(sitein%wbudget_residual,logmask)
    siteout%ebudget_loss2atm(1:inc)          = pack(sitein%ebudget_loss2atm,logmask)
    siteout%ebudget_denseffect(1:inc)        = pack(sitein%ebudget_denseffect,logmask)
    siteout%ebudget_loss2runoff(1:inc)       = pack(sitein%ebudget_loss2runoff,logmask)
    siteout%ebudget_loss2drainage(1:inc)     = pack(sitein%ebudget_loss2drainage,logmask)
    siteout%ebudget_netrad(1:inc)            = pack(sitein%ebudget_netrad,logmask)
    siteout%ebudget_precipgain(1:inc)        = pack(sitein%ebudget_precipgain,logmask)
    siteout%ebudget_initialstorage(1:inc)    = pack(sitein%ebudget_initialstorage,logmask)
    siteout%ebudget_residual(1:inc)          = pack(sitein%ebudget_residual,logmask)
    siteout%co2budget_initialstorage(1:inc)  = pack(sitein%co2budget_initialstorage,logmask)
    siteout%co2budget_residual(1:inc)   = pack(sitein%co2budget_residual,logmask)
    siteout%co2budget_loss2atm(1:inc)   = pack(sitein%co2budget_loss2atm,logmask)
    siteout%co2budget_denseffect(1:inc) = pack(sitein%co2budget_denseffect,logmask)
    siteout%co2budget_gpp(1:inc)        = pack(sitein%co2budget_gpp,logmask)
    siteout%co2budget_plresp(1:inc)     = pack(sitein%co2budget_plresp,logmask)
    siteout%co2budget_rh(1:inc)         = pack(sitein%co2budget_rh,logmask)
    siteout%today_A_decomp(1:inc)       = pack(sitein%today_A_decomp,logmask)
    siteout%today_Af_decomp(1:inc)      = pack(sitein%today_Af_decomp,logmask)
    siteout%veg_rough(1:inc)            = pack(sitein%veg_rough,logmask)
    siteout%veg_height(1:inc)           = pack(sitein%veg_height,logmask)
    siteout%veg_displace(1:inc)         = pack(sitein%veg_displace,logmask)
    siteout%fsc_in(1:inc)               = pack(sitein%fsc_in,logmask)
    siteout%ssc_in(1:inc)               = pack(sitein%ssc_in,logmask)
    siteout%ssl_in(1:inc)               = pack(sitein%ssl_in,logmask)
    siteout%fsn_in(1:inc)               = pack(sitein%fsn_in,logmask)
    siteout%total_plant_nitrogen_uptake(1:inc)    = pack(sitein%total_plant_nitrogen_uptake,logmask)
    siteout%mineralized_N_loss(1:inc)   = pack(sitein%mineralized_N_loss,logmask)
    siteout%mineralized_N_input(1:inc)  = pack(sitein%mineralized_N_input,logmask)
    siteout%rshort_g(1:inc)             = pack(sitein%rshort_g,logmask)
    siteout%rshort_g_beam(1:inc)        = pack(sitein%rshort_g_beam,logmask)
    siteout%rshort_g_diffuse(1:inc)     = pack(sitein%rshort_g_diffuse,logmask)
    siteout%rlong_g(1:inc)              = pack(sitein%rlong_g,logmask)
    siteout%rlong_g_surf(1:inc)         = pack(sitein%rlong_g_surf,logmask)
    siteout%rlong_g_incid(1:inc)        = pack(sitein%rlong_g_incid,logmask)
    siteout%rlong_s(1:inc)              = pack(sitein%rlong_s,logmask)
    siteout%rlong_s_surf(1:inc)         = pack(sitein%rlong_s_surf,logmask)
    siteout%rlong_s_incid(1:inc)        = pack(sitein%rlong_s_incid,logmask)
    siteout%albedo(1:inc)               = pack(sitein%albedo,logmask)
    siteout%albedo_beam(1:inc)          = pack(sitein%albedo_beam,logmask)
    siteout%albedo_diffuse(1:inc)       = pack(sitein%albedo_diffuse,logmask)
    siteout%rlongup(1:inc)              = pack(sitein%rlongup,logmask)
    siteout%rlong_albedo(1:inc)         = pack(sitein%rlong_albedo,logmask)
    siteout%total_sfcw_depth(1:inc)     = pack(sitein%total_sfcw_depth,logmask)
    siteout%snowfac(1:inc)              = pack(sitein%snowfac,logmask)
    siteout%A_decomp(1:inc)             = pack(sitein%A_decomp,logmask)
    siteout%f_decomp(1:inc)             = pack(sitein%f_decomp,logmask)
    siteout%rh(1:inc)                   = pack(sitein%rh,logmask)
    siteout%cwd_rh(1:inc)               = pack(sitein%cwd_rh,logmask)
    siteout%fuse_flag(1:inc)            = pack(sitein%fuse_flag,logmask)
    siteout%plant_ag_biomass(1:inc)     = pack(sitein%plant_ag_biomass,logmask)
    siteout%mean_wflux(1:inc)           = pack(sitein%mean_wflux,logmask)
    siteout%mean_latflux(1:inc)         = pack(sitein%mean_latflux,logmask)
    siteout%mean_hflux(1:inc)           = pack(sitein%mean_hflux,logmask)
    siteout%runoff(1:inc)               = pack(sitein%runoff,logmask)
    siteout%mean_runoff(1:inc)          = pack(sitein%mean_runoff,logmask)
    siteout%mean_qrunoff(1:inc)         = pack(sitein%mean_qrunoff,logmask)
    siteout%htry(1:inc)                 = pack(sitein%htry,logmask)
    siteout%avg_rk4step(1:inc)          = pack(sitein%avg_rk4step,logmask)
    siteout%avg_available_water(1:inc)  = pack(sitein%avg_available_water,logmask)
    siteout%ustar(1:inc)                = pack(sitein%ustar,logmask)
    siteout%tstar(1:inc)                = pack(sitein%tstar,logmask)
    siteout%qstar(1:inc)                = pack(sitein%qstar,logmask)
    siteout%cstar(1:inc)                = pack(sitein%cstar,logmask)
    
    siteout%zeta(1:inc)                 = pack(sitein%zeta,logmask)
    siteout%ribulk(1:inc)               = pack(sitein%ribulk,logmask)

    siteout%upwp(1:inc)                 = pack(sitein%upwp,logmask)
    siteout%tpwp(1:inc)                 = pack(sitein%tpwp,logmask)
    siteout%qpwp(1:inc)                 = pack(sitein%qpwp,logmask)
    siteout%cpwp(1:inc)                 = pack(sitein%cpwp,logmask)
    siteout%wpwp(1:inc)                 = pack(sitein%wpwp,logmask)

    siteout%nlev_sfcwater(1:inc)        = pack(sitein%nlev_sfcwater,logmask)
    siteout%ground_shv(1:inc)           = pack(sitein%ground_shv,logmask)
    siteout%ground_ssh(1:inc)           = pack(sitein%ground_ssh,logmask)
    siteout%ground_temp(1:inc)          = pack(sitein%ground_temp,logmask)
    siteout%ground_fliq(1:inc)          = pack(sitein%ground_fliq,logmask)
    siteout%rough(1:inc)                = pack(sitein%rough,logmask)

    siteout%avg_rshort_gnd     (1:inc)  = pack(sitein%avg_rshort_gnd    ,logmask)
    siteout%avg_rlong_gnd      (1:inc)  = pack(sitein%avg_rlong_gnd     ,logmask)
    siteout%avg_carbon_ac      (1:inc)  = pack(sitein%avg_carbon_ac     ,logmask)
    siteout%avg_rlongup        (1:inc)  = pack(sitein%avg_rlongup       ,logmask)
    siteout%avg_albedo         (1:inc)  = pack(sitein%avg_albedo        ,logmask)
    siteout%avg_albedo_beam    (1:inc)  = pack(sitein%avg_albedo_beam   ,logmask)
    siteout%avg_albedo_diffuse (1:inc)  = pack(sitein%avg_albedo_diffuse,logmask)
    siteout%avg_rlong_albedo   (1:inc)  = pack(sitein%avg_rlong_albedo  ,logmask)

    siteout%avg_vapor_lc(1:inc)         = pack(sitein%avg_vapor_lc,logmask)
    siteout%avg_vapor_wc(1:inc)         = pack(sitein%avg_vapor_wc,logmask)
    siteout%avg_dew_cg(1:inc)           = pack(sitein%avg_dew_cg,logmask)
    siteout%avg_vapor_gc(1:inc)         = pack(sitein%avg_vapor_gc,logmask)
    siteout%avg_wshed_vg(1:inc)         = pack(sitein%avg_wshed_vg,logmask)
    siteout%avg_intercepted(1:inc)      = pack(sitein%avg_intercepted,logmask)
    siteout%avg_throughfall(1:inc)      = pack(sitein%avg_throughfall,logmask)
    siteout%avg_vapor_ac(1:inc)         = pack(sitein%avg_vapor_ac,logmask)
    siteout%avg_transp(1:inc)           = pack(sitein%avg_transp,logmask)
    siteout%avg_evap(1:inc)             = pack(sitein%avg_evap,logmask)
    siteout%avg_runoff(1:inc)           = pack(sitein%avg_runoff,logmask)
    siteout%avg_drainage(1:inc)         = pack(sitein%avg_drainage,logmask)
    siteout%avg_drainage_heat(1:inc)    = pack(sitein%avg_drainage_heat,logmask)
    siteout%aux(1:inc)                  = pack(sitein%aux,logmask)
    siteout%avg_sensible_lc(1:inc)      = pack(sitein%avg_sensible_lc,logmask)
    siteout%avg_sensible_wc(1:inc)      = pack(sitein%avg_sensible_wc,logmask)
    siteout%avg_qwshed_vg(1:inc)        = pack(sitein%avg_qwshed_vg,logmask)
    siteout%avg_qintercepted(1:inc)     = pack(sitein%avg_qintercepted,logmask)
    siteout%avg_qthroughfall(1:inc)     = pack(sitein%avg_qthroughfall,logmask)
    siteout%avg_sensible_gc(1:inc)      = pack(sitein%avg_sensible_gc,logmask)
    siteout%avg_sensible_ac(1:inc)      = pack(sitein%avg_sensible_ac,logmask)
    siteout%avg_runoff_heat(1:inc)      = pack(sitein%avg_runoff_heat,logmask)
    siteout%avg_leaf_energy(1:inc)      = pack(sitein%avg_leaf_energy,logmask)
    siteout%avg_leaf_temp(1:inc)        = pack(sitein%avg_leaf_temp,logmask)
    siteout%avg_leaf_hcap(1:inc)        = pack(sitein%avg_leaf_hcap,logmask)
    siteout%avg_leaf_fliq(1:inc)        = pack(sitein%avg_leaf_fliq,logmask)
    siteout%avg_leaf_water(1:inc)       = pack(sitein%avg_leaf_water,logmask)
    siteout%avg_wood_energy(1:inc)      = pack(sitein%avg_wood_energy,logmask)
    siteout%avg_wood_temp(1:inc)        = pack(sitein%avg_wood_temp,logmask)
    siteout%avg_wood_hcap(1:inc)        = pack(sitein%avg_wood_hcap,logmask)
    siteout%avg_wood_fliq(1:inc)        = pack(sitein%avg_wood_fliq,logmask)
    siteout%avg_wood_water(1:inc)       = pack(sitein%avg_wood_water,logmask)
    siteout%par_l_max(1:inc)            = pack(sitein%par_l_max,logmask)
    siteout%par_l_beam_max(1:inc)       = pack(sitein%par_l_beam_max,logmask)
    siteout%par_l_diffuse_max(1:inc)    = pack(sitein%par_l_diffuse_max,logmask)

    ! Water layers 1:nzs
    
    do k=1,nzs
       siteout%sfcwater_mass(k,1:inc)    = pack(sitein%sfcwater_mass(k,:),logmask)
       siteout%sfcwater_energy(k,1:inc)  = pack(sitein%sfcwater_energy(k,:),logmask)
       siteout%sfcwater_depth(k,1:inc)   = pack(sitein%sfcwater_depth(k,:),logmask)
       siteout%rshort_s(k,1:inc)         = pack(sitein%rshort_s(k,:),logmask)
       siteout%rshort_s_beam(k,1:inc)    = pack(sitein%rshort_s_beam(k,:),logmask)
       siteout%rshort_s_diffuse(k,1:inc) = pack(sitein%rshort_s_diffuse(k,:),logmask)
       siteout%sfcwater_tempk(k,1:inc)   = pack(sitein%sfcwater_tempk(k,:),logmask)
       siteout%sfcwater_fracliq(k,1:inc)  = pack(sitein%sfcwater_fracliq(k,:),logmask)
    end do

    ! Soil layers 1:nzg

    do k=1,nzg
       siteout%soil_energy(k,1:inc)        = pack(sitein%soil_energy(k,:),logmask)
       siteout%soil_water(k,1:inc)         = pack(sitein%soil_water(k,:),logmask)
       siteout%soil_tempk(k,1:inc)         = pack(sitein%soil_tempk(k,:),logmask)
       siteout%soil_fracliq(k,1:inc)       = pack(sitein%soil_fracliq(k,:),logmask)
       siteout%rootdense(k,1:inc)          = pack(sitein%rootdense(k,:),logmask)
       siteout%avg_smoist_gg(k,1:inc)      = pack(sitein%avg_smoist_gg(k,:),logmask)
       siteout%avg_transloss(k,1:inc)      = pack(sitein%avg_transloss(k,:),logmask)
       siteout%avg_sensible_gg(k,1:inc)    = pack(sitein%avg_sensible_gg(k,:),logmask)
       siteout%aux_s(k,1:inc)              = pack(sitein%aux_s(k,:),logmask)

       siteout%current_paw(k,1:inc) = pack(sitein%current_paw(k,:),logmask)
       do m = 1, 10
          siteout%past_paw(k,m,1:inc) = pack(sitein%past_paw(k,m,:),logmask)
       enddo
    end do

    ! pft types

    do k=1,n_pft
       siteout%repro(k,1:inc)            = pack(sitein%repro(k,:),logmask)
       siteout%A_o_max(k,1:inc)          = pack(sitein%A_o_max(k,:),logmask)
       siteout%A_c_max(k,1:inc)          = pack(sitein%A_c_max(k,:),logmask)
       
       do m=1,n_stoma_atts
          siteout%old_stoma_vector_max(m,k,1:inc) = pack(sitein%old_stoma_vector_max(m,k,:),logmask)
       end do

       do m=1,ff_nhgt
          siteout%cumlai_profile(k,m,1:inc)       = pack(sitein%cumlai_profile(k,m,:),logmask)
       end do
    end do
    
    !dbh types
    do k=1,n_dbh
        siteout%co2budget_gpp_dbh(k,1:inc)        = pack(sitein%co2budget_gpp_dbh(k,:),logmask)
    end do

    
    ! Old_stoma_data_max type
    ! Derived type with n_pft x n_patch, so.... the intrinsic "pack" wont work

    do m=1,newsz
       k=incmask(m)
       call allocate_patchtype(siteout%patch(m),sitein%patch(k)%ncohorts)
       
       call copy_patchtype(sitein%patch(k),siteout%patch(m),1,sitein%patch(k)%ncohorts,1,sitein%patch(k)%ncohorts)

       do ipft=1,n_pft

          osdo => siteout%old_stoma_data_max(ipft,m)
          osdi => sitein%old_stoma_data_max(ipft,k)
          
          osdo%recalc           = osdi%recalc
          osdo%T_L              = osdi%T_L
          osdo%e_A              = osdi%e_A
          osdo%PAR              = osdi%PAR
          osdo%rb_factor        = osdi%rb_factor
          osdo%prss             = osdi%prss
          osdo%phenology_factor = osdi%phenology_factor
          osdo%gsw_open         = osdi%gsw_open
          osdo%ilimit           = osdi%ilimit
          osdo%T_L_residual     = osdi%T_L_residual
          osdo%e_a_residual     = osdi%e_a_residual
          osdo%par_residual     = osdi%par_residual
          osdo%rb_residual      = osdi%rb_residual
          osdo%leaf_residual    = osdi%leaf_residual
          osdo%gsw_residual     = osdi%gsw_residual

       end do
       
    end do
       
    if (idoutput > 0 .or. imoutput > 0 .or. iqoutput > 0) then
       siteout%dmean_rh             (1:inc) = pack(sitein%dmean_rh             ,logmask)
       siteout%dmean_co2_residual   (1:inc) = pack(sitein%dmean_co2_residual   ,logmask)
       siteout%dmean_energy_residual(1:inc) = pack(sitein%dmean_energy_residual,logmask)
       siteout%dmean_water_residual (1:inc) = pack(sitein%dmean_water_residual ,logmask)
       siteout%dmean_lambda_light   (1:inc) = pack(sitein%dmean_lambda_light   ,logmask)
       siteout%dmean_A_decomp       (1:inc) = pack(sitein%dmean_A_decomp       ,logmask)
       siteout%dmean_Af_decomp      (1:inc) = pack(sitein%dmean_Af_decomp      ,logmask)
       siteout%dmean_rk4step        (1:inc) = pack(sitein%dmean_rk4step        ,logmask)
       siteout%dmean_albedo         (1:inc) = pack(sitein%dmean_albedo         ,logmask)
       siteout%dmean_albedo_beam    (1:inc) = pack(sitein%dmean_albedo_beam    ,logmask)
       siteout%dmean_albedo_diffuse (1:inc) = pack(sitein%dmean_albedo_diffuse ,logmask)
    end if
    
    if (imoutput > 0 .or. iqoutput > 0) then
       siteout%mmean_rh             (1:inc) = pack(sitein%mmean_rh             ,logmask)
       siteout%mmean_co2_residual   (1:inc) = pack(sitein%mmean_co2_residual   ,logmask)
       siteout%mmean_energy_residual(1:inc) = pack(sitein%mmean_energy_residual,logmask)
       siteout%mmean_water_residual (1:inc) = pack(sitein%mmean_water_residual ,logmask)
       siteout%mmean_lambda_light   (1:inc) = pack(sitein%mmean_lambda_light   ,logmask)
       siteout%mmean_A_decomp       (1:inc) = pack(sitein%mmean_A_decomp       ,logmask)
       siteout%mmean_Af_decomp      (1:inc) = pack(sitein%mmean_Af_decomp      ,logmask)
       siteout%mmean_fsn_in         (1:inc) = pack(sitein%mmean_fsn_in         ,logmask)                        !JL
       siteout%mmean_ssc_in         (1:inc) = pack(sitein%mmean_ssc_in         ,logmask)                        !JL
       siteout%mmean_total_plant_nitrogen_uptake(1:inc) = pack(sitein%mmean_total_plant_nitrogen_uptake,logmask)!JL
       siteout%mmean_rk4step        (1:inc) = pack(sitein%mmean_rk4step        ,logmask)
       siteout%mmean_albedo         (1:inc) = pack(sitein%mmean_albedo         ,logmask)
       siteout%mmean_albedo_beam    (1:inc) = pack(sitein%mmean_albedo_beam    ,logmask)
       siteout%mmean_albedo_diffuse (1:inc) = pack(sitein%mmean_albedo_diffuse ,logmask)
    end if
    
    if (iqoutput > 0) then
       do icyc=1,ndcycle
          siteout%qmean_rh             (icyc,1:inc) = pack(sitein%qmean_rh            (icyc,:) ,logmask)
          siteout%qmean_albedo         (icyc,1:inc) = pack(sitein%qmean_albedo        (icyc,:) ,logmask)
          siteout%qmean_albedo_beam    (icyc,1:inc) = pack(sitein%qmean_albedo_beam   (icyc,:) ,logmask)
          siteout%qmean_albedo_diffuse (icyc,1:inc) = pack(sitein%qmean_albedo_diffuse(icyc,:) ,logmask)
       end do
    end if

    return
  end subroutine copy_sitetype_mask
!============================================================================!
!============================================================================!





!============================================================================!
!============================================================================!
  subroutine copy_patchtype_mask(patchin,patchout,mask,masksz,newsz)

    ! This subroutine assumes that the size of vectors in siteout
    ! are the number of true elements in mask, while the size of the
    ! vectors in sitein are of the size of the mask itself. If this
    ! is not true, you will get a segmentation violation and the
    ! code will crash.args 1 and 3 must be dimension of arg 4
    ! argument 2 must be the dimension of the sum of the 3rd argument
    ! 
    ! THIS ROUTINE CURRENTLY ASSUMES THAT THE OUTPUT SITE
    ! HAS NOT ALLOCATED IT'S PATCH'S COHORT VECTORS YET, THIS
    ! IS BECAUSE THE LENGTHS OF THESE VECTORS ARE BASED ON THE
    ! DONOR PATH'S VECTOR SIZES.  DO NOT USE PRE-ALLOCATED
    ! RECIPIENTS

   !  use decomp_coms     , only : FSN_ndays_to_avg !JL!
    
implicit none

    type(patchtype),target :: patchin,patchout
    integer :: masksz,newsz
    integer,dimension(newsz) :: incmask
    integer,dimension(masksz):: imask
    logical,dimension(masksz)  :: mask
    integer :: i,k,m,inc
    type(stoma_data),pointer :: osdi,osdo

    do i=1,masksz
       imask(i) = i
    end do
    inc=count(mask)                   ! Number of true elements

    if (inc == 0) return

    incmask=pack(imask,mask)   ! List of true elements
    
    patchout%pft(1:inc)              = pack(patchin%pft,mask)
    patchout%nplant(1:inc)           = pack(patchin%nplant,mask)
    patchout%hite(1:inc)             = pack(patchin%hite,mask)
    patchout%agb(1:inc)              = pack(patchin%agb,mask)
    patchout%basarea(1:inc)          = pack(patchin%basarea,mask)
    patchout%dagb_dt(1:inc)          = pack(patchin%dagb_dt,mask)
    patchout%dba_dt(1:inc)           = pack(patchin%dba_dt,mask)
    patchout%ddbh_dt(1:inc)          = pack(patchin%ddbh_dt,mask)
    patchout%dbh(1:inc)              = pack(patchin%dbh,mask)
    patchout%bdead(1:inc)            = pack(patchin%bdead,mask)
    patchout%bleaf(1:inc)            = pack(patchin%bleaf,mask)
    patchout%phenology_status(1:inc) = pack(patchin%phenology_status,mask)
    patchout%balive(1:inc)           = pack(patchin%balive,mask)
    patchout%broot(1:inc)            = pack(patchin%broot,mask)
    patchout%bsapwood(1:inc)         = pack(patchin%bsapwood,mask)
!******************************************************************** JL,XXT
    patchout%excess_carbon(1:inc)                = pack(patchin%excess_carbon,mask)
    patchout%excess_carbon_fixer(1:inc)          = pack(patchin%excess_carbon_fixer,mask)
    patchout%nitrogen_supply_fixer(1:inc)        = pack(patchin%nitrogen_supply_fixer,mask)
    patchout%nitrogen_supply(1:inc)              = pack(patchin%nitrogen_supply,mask)
    patchout%shadow_N_uptake(1:inc)              = pack(patchin%shadow_N_uptake,mask)
    patchout%actual_nitrogen_uptake(1:inc)       = pack(patchin%actual_nitrogen_uptake,mask)
    patchout%actual_nitrogen_uptake_fixer(1:inc) = pack(patchin%actual_nitrogen_uptake_fixer,mask)
    patchout%fixation_demand(1:inc)              = pack(patchin%fixation_demand,mask)
    patchout%N_fixation(1:inc)                   = pack(patchin%N_fixation,mask)
    patchout%fsn_fixer(1:inc)                    = pack(patchin%fsn_fixer,mask)
    patchout%nstorage(1:inc)                     = pack(patchin%nstorage,mask)
    patchout%carbon_balance(1:inc)               = pack(patchin%carbon_balance,mask) 
    patchout%N_uptake_pot(1:inc)                 = pack(patchin%N_uptake_pot,mask)
    patchout%nitrogen_uptake(1:inc)              = pack(patchin%nitrogen_uptake,mask)
    patchout%carbon_balance_pot(1:inc)           = pack(patchin%carbon_balance_pot,mask)
    patchout%nitrogen_supply_test(1:inc)         = pack(patchin%nitrogen_supply_test,mask)
    patchout%nitrogen_supply_test2(1:inc)        = pack(patchin%nitrogen_supply_test2,mask)
 ! patchout%N_limitation_factor_bar(1:inc)       = pack(patchin%N_limitation_factor_bar,mask)
!********************************************************************
 patchout%lai(1:inc)              = pack(patchin%lai,mask)
    patchout%wpa(1:inc)              = pack(patchin%wpa,mask)
    patchout%wai(1:inc)              = pack(patchin%wai,mask)
    patchout%crown_area(1:inc)       = pack(patchin%crown_area,mask)
    patchout%leaf_resolvable(1:inc)  = pack(patchin%leaf_resolvable,mask)
    patchout%wood_resolvable(1:inc)  = pack(patchin%wood_resolvable,mask)
    patchout%bstorage(1:inc)         = pack(patchin%bstorage,mask)

    patchout%cbr_bar(1:inc)          = pack(patchin%cbr_bar,mask)
    patchout%leaf_energy(1:inc)      = pack(patchin%leaf_energy,mask)
    patchout%leaf_hcap(1:inc)        = pack(patchin%leaf_hcap,mask)
    patchout%leaf_temp(1:inc)        = pack(patchin%leaf_temp,mask)
    patchout%leaf_fliq(1:inc)        = pack(patchin%leaf_fliq,mask)
    patchout%leaf_water(1:inc)       = pack(patchin%leaf_water,mask)
    patchout%wood_energy(1:inc)      = pack(patchin%wood_energy,mask)
    patchout%wood_hcap(1:inc)        = pack(patchin%wood_hcap,mask)
    patchout%wood_temp(1:inc)        = pack(patchin%wood_temp,mask)
    patchout%wood_fliq(1:inc)        = pack(patchin%wood_fliq,mask)
    patchout%wood_water(1:inc)       = pack(patchin%wood_water,mask)
    patchout%veg_wind(1:inc)         = pack(patchin%veg_wind,mask)
    patchout%lsfc_shv_open(1:inc)    = pack(patchin%lsfc_shv_open,mask)
    patchout%lsfc_shv_closed(1:inc)  = pack(patchin%lsfc_shv_closed,mask)
    patchout%lsfc_co2_open(1:inc)    = pack(patchin%lsfc_co2_open,mask)
    patchout%lsfc_co2_closed(1:inc)  = pack(patchin%lsfc_co2_closed,mask)
    patchout%lint_shv(1:inc)         = pack(patchin%lint_shv,mask)
    patchout%lint_co2_open(1:inc)    = pack(patchin%lint_co2_open,mask)
    patchout%lint_co2_closed(1:inc)  = pack(patchin%lint_co2_closed,mask)
    patchout%mean_gpp(1:inc)         = pack(patchin%mean_gpp,mask)
    patchout%mean_leaf_resp(1:inc)   = pack(patchin%mean_leaf_resp,mask)
    patchout%mean_root_resp(1:inc)   = pack(patchin%mean_root_resp,mask)
    patchout%mean_growth_resp(1:inc) = pack(patchin%mean_growth_resp,mask)
    patchout%mean_storage_resp(1:inc)= pack(patchin%mean_storage_resp,mask)
    patchout%mean_vleaf_resp(1:inc)  = pack(patchin%mean_vleaf_resp,mask)
    patchout%today_leaf_resp(1:inc)  = pack(patchin%today_leaf_resp,mask)
    patchout%today_root_resp(1:inc)  = pack(patchin%today_root_resp,mask)
    patchout%today_gpp(1:inc)        = pack(patchin%today_gpp,mask)
    patchout%today_nppleaf(1:inc)    = pack(patchin%today_nppleaf,mask)
    patchout%today_nppfroot(1:inc)   = pack(patchin%today_nppfroot,mask)
    patchout%today_nppsapwood(1:inc) = pack(patchin%today_nppsapwood,mask)
    patchout%today_nppcroot(1:inc)   = pack(patchin%today_nppcroot,mask)
    patchout%today_nppseeds(1:inc)   = pack(patchin%today_nppseeds,mask)
    patchout%today_nppwood(1:inc)    = pack(patchin%today_nppwood,mask)
    patchout%today_nppdaily(1:inc)   = pack(patchin%today_nppdaily,mask)
    patchout%today_gpp_pot(1:inc)    = pack(patchin%today_gpp_pot,mask)
    patchout%today_gpp_max(1:inc)    = pack(patchin%today_gpp_max,mask)
    patchout%growth_respiration(1:inc) = pack(patchin%growth_respiration,mask)
    patchout%storage_respiration(1:inc) = pack(patchin%storage_respiration,mask)
    patchout%vleaf_respiration(1:inc) = pack(patchin%vleaf_respiration,mask)
    patchout%fsn(1:inc)              = pack(patchin%fsn,mask)

    patchout%monthly_dndt(1:inc)     = pack(patchin%monthly_dndt,mask)
    
    patchout%Psi_open(1:inc)         = pack(patchin%Psi_open,mask)
    patchout%krdepth(1:inc)          = pack(patchin%krdepth,mask)
    patchout%first_census(1:inc)     = pack(patchin%first_census,mask)
    patchout%new_recruit_flag(1:inc) = pack(patchin%new_recruit_flag,mask)
    patchout%light_level(1:inc)      = pack(patchin%light_level,mask)
    patchout%light_level_beam(1:inc) = pack(patchin%light_level_beam,mask)
    patchout%light_level_diff(1:inc) = pack(patchin%light_level_diff,mask)
    patchout%beamext_level(1:inc)      = pack(patchin%beamext_level,mask)
    patchout%diffext_level(1:inc)      = pack(patchin%diffext_level,mask)
    patchout%lambda_light(1:inc)     = pack(patchin%lambda_light,mask)
    patchout%par_l(1:inc)            = pack(patchin%par_l,mask)
    patchout%par_l_beam(1:inc)       = pack(patchin%par_l_beam,mask)
    patchout%par_l_diffuse(1:inc)    = pack(patchin%par_l_diffuse,mask)
    patchout%rshort_l(1:inc)         = pack(patchin%rshort_l,mask)
    patchout%rshort_l_beam(1:inc)    = pack(patchin%rshort_l_beam,mask)
    patchout%rshort_l_diffuse(1:inc) = pack(patchin%rshort_l_diffuse,mask)
    patchout%rlong_l(1:inc)          = pack(patchin%rlong_l,mask)
    patchout%rlong_l_surf(1:inc)     = pack(patchin%rlong_l_surf,mask)
    patchout%rlong_l_incid(1:inc)    = pack(patchin%rlong_l_incid,mask)
    patchout%rshort_w(1:inc)         = pack(patchin%rshort_w,mask)
    patchout%rshort_w_beam(1:inc)    = pack(patchin%rshort_w_beam,mask)
    patchout%rshort_w_diffuse(1:inc) = pack(patchin%rshort_w_diffuse,mask)
    patchout%rlong_w(1:inc)          = pack(patchin%rlong_w,mask)
    patchout%rlong_w_surf(1:inc)     = pack(patchin%rlong_w_surf,mask)
    patchout%rlong_w_incid(1:inc)    = pack(patchin%rlong_w_incid,mask)
    patchout%leaf_gbh(1:inc)         = pack(patchin%leaf_gbh,mask)
    patchout%leaf_gbw(1:inc)         = pack(patchin%leaf_gbw,mask)
    patchout%wood_gbh(1:inc)         = pack(patchin%wood_gbh,mask)
    patchout%wood_gbw(1:inc)         = pack(patchin%wood_gbw,mask)
    patchout%A_open(1:inc)           = pack(patchin%A_open,mask)
    patchout%A_closed(1:inc)         = pack(patchin%A_closed,mask)
    patchout%Psi_closed(1:inc)       = pack(patchin%Psi_closed,mask)
    patchout%gsw_open(1:inc)         = pack(patchin%gsw_open,mask)
    patchout%gsw_closed(1:inc)       = pack(patchin%gsw_closed,mask)
    patchout%fsw(1:inc)              = pack(patchin%fsw,mask)
    patchout%fs_open(1:inc)          = pack(patchin%fs_open,mask)
    patchout%water_supply(1:inc)     = pack(patchin%water_supply,mask)
    patchout%stomatal_conductance(1:inc) = pack(patchin%stomatal_conductance,mask)
    patchout%leaf_maintenance(1:inc) = pack(patchin%leaf_maintenance,mask)
    patchout%root_maintenance(1:inc) = pack(patchin%root_maintenance,mask)
    patchout%leaf_drop(1:inc)        = pack(patchin%leaf_drop,mask)
    patchout%bseeds(1:inc)           = pack(patchin%bseeds,mask)
    patchout%leaf_respiration(1:inc) = pack(patchin%leaf_respiration,mask)
    patchout%root_respiration(1:inc) = pack(patchin%root_respiration,mask)
    patchout%gpp(1:inc)              = pack(patchin%gpp,mask)
    patchout%paw_avg(1:inc)          = pack(patchin%paw_avg,mask)
    patchout%elongf(1:inc)           = pack(patchin%elongf,mask)
    patchout%turnover_amp(1:inc)     = pack(patchin%turnover_amp,mask)
    patchout%llspan(1:inc)           = pack(patchin%llspan,mask)
    patchout%vm_bar(1:inc)           = pack(patchin%vm_bar,mask)
    patchout%sla(1:inc)              = pack(patchin%sla,mask)    

    do m=1,inc
       k=incmask(m)
       do i = 1,13
          patchout%cb(i,m)               = patchin%cb(i,k)
          patchout%cb_max(i,m)           = patchin%cb_max(i,k)
       end do
!JL!
     !do i = 1, FSN_ndays_to_avg                                
!         patchout%N_limitation_factor(i,m)  = patchin%N_limitation_factor(i,k)
!     end do
!JL!


       do i = 1,n_stoma_atts
          patchout%old_stoma_vector(i,m) = patchin%old_stoma_vector(i,k)
       end do
       do i = 1,n_mort
          patchout%mort_rate(i,m)       = patchin%mort_rate(i,k)
       end do
    end do

    
    
    ! Copy the stoma data
    do m=1,inc
       k=incmask(m)
       
       osdo => patchout%old_stoma_data(m)
       osdi => patchin%old_stoma_data(k)

       osdo%recalc           = osdi%recalc
       osdo%T_L              = osdi%T_L
       osdo%e_A              = osdi%e_A
       osdo%PAR              = osdi%PAR
       osdo%rb_factor        = osdi%rb_factor
       osdo%prss             = osdi%prss
       osdo%phenology_factor = osdi%phenology_factor
       osdo%gsw_open         = osdi%gsw_open
       osdo%ilimit           = osdi%ilimit
       osdo%T_L_residual     = osdi%T_L_residual
       osdo%e_a_residual     = osdi%e_a_residual
       osdo%par_residual     = osdi%par_residual
       osdo%rb_residual      = osdi%rb_residual
       osdo%leaf_residual    = osdi%leaf_residual
       osdo%gsw_residual     = osdi%gsw_residual
       
    end do
    
    if (idoutput > 0 .or. imoutput > 0 .or. iqoutput > 0) then
       patchout%dmean_fs_open         (1:inc) = pack(patchin%dmean_fs_open         ,mask)
       patchout%dmean_fsw             (1:inc) = pack(patchin%dmean_fsw             ,mask)
       patchout%dmean_fsn             (1:inc) = pack(patchin%dmean_fsn             ,mask)
       patchout%dmean_psi_open        (1:inc) = pack(patchin%dmean_psi_open        ,mask)
       patchout%dmean_psi_closed      (1:inc) = pack(patchin%dmean_psi_closed      ,mask)
       patchout%dmean_water_supply    (1:inc) = pack(patchin%dmean_water_supply    ,mask)
       patchout%dmean_lambda_light    (1:inc) = pack(patchin%dmean_lambda_light    ,mask)
       patchout%dmean_light_level     (1:inc) = pack(patchin%dmean_light_level     ,mask)
       patchout%dmean_light_level_beam(1:inc) = pack(patchin%dmean_light_level_beam,mask)
       patchout%dmean_light_level_diff(1:inc) = pack(patchin%dmean_light_level_diff,mask)
       patchout%dmean_beamext_level   (1:inc) = pack(patchin%dmean_beamext_level   ,mask)
       patchout%dmean_diffext_level   (1:inc) = pack(patchin%dmean_diffext_level   ,mask)
       patchout%dmean_gpp             (1:inc) = pack(patchin%dmean_gpp             ,mask)
       patchout%dmean_nppleaf         (1:inc) = pack(patchin%dmean_nppleaf         ,mask)
       patchout%dmean_nppfroot        (1:inc) = pack(patchin%dmean_nppfroot        ,mask)
       patchout%dmean_nppsapwood      (1:inc) = pack(patchin%dmean_nppsapwood      ,mask)
       patchout%dmean_nppcroot        (1:inc) = pack(patchin%dmean_nppcroot        ,mask)
       patchout%dmean_nppseeds        (1:inc) = pack(patchin%dmean_nppseeds        ,mask)
       patchout%dmean_nppwood         (1:inc) = pack(patchin%dmean_nppwood         ,mask)
       patchout%dmean_nppdaily        (1:inc) = pack(patchin%dmean_nppdaily        ,mask)       
       patchout%dmean_leaf_resp       (1:inc) = pack(patchin%dmean_leaf_resp       ,mask)
       patchout%dmean_root_resp       (1:inc) = pack(patchin%dmean_root_resp       ,mask)
       patchout%dmean_par_l           (1:inc) = pack(patchin%dmean_par_l           ,mask)
       patchout%dmean_par_l_beam      (1:inc) = pack(patchin%dmean_par_l_beam      ,mask)
       patchout%dmean_par_l_diff      (1:inc) = pack(patchin%dmean_par_l_diff      ,mask)
    end if
    if (imoutput > 0 .or. iqoutput > 0) then
       patchout%mmean_fs_open         (1:inc) = pack(patchin%mmean_fs_open         ,mask)
       patchout%mmean_fsw             (1:inc) = pack(patchin%mmean_fsw             ,mask)
       patchout%mmean_fsn             (1:inc) = pack(patchin%mmean_fsn             ,mask)
       patchout%mmean_psi_open        (1:inc) = pack(patchin%mmean_psi_open        ,mask)
       patchout%mmean_psi_closed      (1:inc) = pack(patchin%mmean_psi_closed      ,mask)
       patchout%mmean_water_supply    (1:inc) = pack(patchin%mmean_water_supply    ,mask)
       patchout%mmean_leaf_maintenance(1:inc) = pack(patchin%mmean_leaf_maintenance,mask)
       patchout%mmean_root_maintenance(1:inc) = pack(patchin%mmean_root_maintenance,mask)
       patchout%mmean_leaf_drop       (1:inc) = pack(patchin%mmean_leaf_drop       ,mask)
       patchout%mmean_cb              (1:inc) = pack(patchin%mmean_cb              ,mask)
       patchout%mmean_lambda_light    (1:inc) = pack(patchin%mmean_lambda_light    ,mask)
       patchout%mmean_light_level     (1:inc) = pack(patchin%mmean_light_level     ,mask)
       patchout%mmean_light_level_beam(1:inc) = pack(patchin%mmean_light_level_beam,mask)
       patchout%mmean_light_level_diff(1:inc) = pack(patchin%mmean_light_level_diff,mask)
       patchout%mmean_beamext_level   (1:inc) = pack(patchin%mmean_beamext_level   ,mask)
       patchout%mmean_diffext_level   (1:inc) = pack(patchin%mmean_diffext_level   ,mask)
       patchout%mmean_gpp             (1:inc) = pack(patchin%mmean_gpp             ,mask)
       patchout%mmean_nppleaf         (1:inc) = pack(patchin%mmean_nppleaf         ,mask)
       patchout%mmean_nppfroot        (1:inc) = pack(patchin%mmean_nppfroot        ,mask)
       patchout%mmean_nppsapwood      (1:inc) = pack(patchin%mmean_nppsapwood      ,mask)
    !  patchout%mmean_nitrogen_supply (1:inc) = pack(patchin%mmean_nitrogen_supply ,mask)!JL!
     ! patchout%mmean_N_uptake_pot    (1:inc) = pack(patchin%mmean_N_uptake_pot    ,mask)!JL!
       patchout%mmean_nppcroot        (1:inc) = pack(patchin%mmean_nppcroot        ,mask)
       patchout%mmean_nppseeds        (1:inc) = pack(patchin%mmean_nppseeds        ,mask)
       patchout%mmean_nppwood         (1:inc) = pack(patchin%mmean_nppwood         ,mask)
       patchout%mmean_nppdaily        (1:inc) = pack(patchin%mmean_nppdaily        ,mask)     
       patchout%mmean_leaf_resp       (1:inc) = pack(patchin%mmean_leaf_resp       ,mask)
       patchout%mmean_root_resp       (1:inc) = pack(patchin%mmean_root_resp       ,mask)
       patchout%mmean_growth_resp     (1:inc) = pack(patchin%mmean_growth_resp     ,mask)
       patchout%mmean_storage_resp    (1:inc) = pack(patchin%mmean_storage_resp    ,mask)
       patchout%mmean_vleaf_resp      (1:inc) = pack(patchin%mmean_vleaf_resp      ,mask)
       patchout%mmean_par_l           (1:inc) = pack(patchin%mmean_par_l           ,mask)
       patchout%mmean_par_l_beam      (1:inc) = pack(patchin%mmean_par_l_beam      ,mask)
       patchout%mmean_par_l_diff      (1:inc) = pack(patchin%mmean_par_l_diff      ,mask)

       do m=1,inc
         k=incmask(m)
         do i = 1,n_mort
             patchout%mmean_mort_rate(i,m) = patchin%mmean_mort_rate(i,k)
          end do
       end do
    end if

    if (iqoutput > 0) then
       do m=1,ndcycle
          patchout%qmean_par_l       (m,1:inc) = pack(patchin%qmean_par_l       (m,:),mask)
          patchout%qmean_par_l_beam  (m,1:inc) = pack(patchin%qmean_par_l_beam  (m,:),mask)
          patchout%qmean_par_l_diff  (m,1:inc) = pack(patchin%qmean_par_l_diff  (m,:),mask)
          patchout%qmean_fs_open     (m,1:inc) = pack(patchin%qmean_fs_open     (m,:),mask)
          patchout%qmean_fsw         (m,1:inc) = pack(patchin%qmean_fsw         (m,:),mask)
          patchout%qmean_fsn         (m,1:inc) = pack(patchin%qmean_fsn         (m,:),mask)
          patchout%qmean_psi_open    (m,1:inc) = pack(patchin%qmean_psi_open    (m,:),mask)
          patchout%qmean_psi_closed  (m,1:inc) = pack(patchin%qmean_psi_closed  (m,:),mask)
          patchout%qmean_water_supply(m,1:inc) = pack(patchin%qmean_water_supply(m,:),mask)
          patchout%qmean_gpp         (m,1:inc) = pack(patchin%qmean_gpp         (m,:),mask)
          patchout%qmean_leaf_resp   (m,1:inc) = pack(patchin%qmean_leaf_resp   (m,:),mask)
          patchout%qmean_root_resp   (m,1:inc) = pack(patchin%qmean_root_resp   (m,:),mask)
       end do
    end if

    return
  end subroutine copy_patchtype_mask
!============================================================================!
!============================================================================!





!============================================================================!
!============================================================================!
  subroutine copy_patchtype(patchin,patchout,ipin1,ipin2,ipout1,ipout2)

    implicit none
    integer :: ipin1,ipin2,ipout1,ipout2
    type(patchtype),target :: patchin,patchout
    type(stoma_data),pointer :: osdo,osdi
    integer :: iout,iin

    if (ipout2-ipout1.ne.ipin2-ipin1) then
       print*,"In copy_patchtype:"
       print*,"You specified unequal vector lengths"
       print*,"in the input and output targets"
       print*,"This cannot be..stopping"
       call fatal_error('unequal vector lengths','copy_patchtype','ed_state_vars.f90')
    end if

    ! Copy the stoma data. Added the loop back here because sometimes ipin1 < ipin2
    ! for example, when ncohorts=0

    iin = ipin1
    do iout=ipout1,ipout2

       patchout%pft(iout)              = patchin%pft(iin)
       patchout%nplant(iout)           = patchin%nplant(iin)
       patchout%hite(iout)             = patchin%hite(iin)
       patchout%agb(iout)              = patchin%agb(iin)
       patchout%basarea(iout)          = patchin%basarea(iin)
       patchout%dagb_dt(iout)          = patchin%dagb_dt(iin)
       patchout%dba_dt(iout)           = patchin%dba_dt(iin)
       patchout%ddbh_dt(iout)          = patchin%ddbh_dt(iin)
       patchout%dbh(iout)              = patchin%dbh(iin)
       patchout%bdead(iout)            = patchin%bdead(iin)
       patchout%bleaf(iout)            = patchin%bleaf(iin)
       patchout%phenology_status(iout) = patchin%phenology_status(iin)
       patchout%balive(iout)           = patchin%balive(iin)
       patchout%broot(iout)            = patchin%broot(iin)
       patchout%bsapwood(iout)         = patchin%bsapwood(iin)
       patchout%lai(iout)              = patchin%lai(iin)
       patchout%wpa(iout)              = patchin%wpa(iin)
       patchout%wai(iout)              = patchin%wai(iin)
       patchout%crown_area(iout)       = patchin%crown_area(iin)
       patchout%leaf_resolvable(iout)  = patchin%leaf_resolvable(iin)
       patchout%wood_resolvable(iout)  = patchin%wood_resolvable(iin)
       patchout%bstorage(iout)         = patchin%bstorage(iin)
       patchout%cb(:,iout)             = patchin%cb(:,iin)
       patchout%cb_max(:,iout)         = patchin%cb_max(:,iin)
       patchout%cbr_bar(iout)          = patchin%cbr_bar(iin)
       patchout%leaf_energy(iout)      = patchin%leaf_energy(iin)
       patchout%leaf_hcap(iout)        = patchin%leaf_hcap(iin)
       patchout%leaf_temp(iout)        = patchin%leaf_temp(iin)
       patchout%leaf_fliq(iout)        = patchin%leaf_fliq(iin)
       patchout%leaf_water(iout)       = patchin%leaf_water(iin)
       patchout%wood_energy(iout)      = patchin%wood_energy(iin)
       patchout%wood_hcap(iout)        = patchin%wood_hcap(iin)
       patchout%wood_temp(iout)        = patchin%wood_temp(iin)
       patchout%wood_fliq(iout)        = patchin%wood_fliq(iin)
       patchout%wood_water(iout)       = patchin%wood_water(iin)
       patchout%veg_wind(iout)         = patchin%veg_wind(iin)
       patchout%lsfc_shv_open(iout)    = patchin%lsfc_shv_open(iin)
       patchout%lsfc_shv_closed(iout)  = patchin%lsfc_shv_closed(iin)
       patchout%lsfc_co2_open(iout)    = patchin%lsfc_co2_open(iin)
       patchout%lsfc_co2_closed(iout)  = patchin%lsfc_co2_closed(iin)
       patchout%lint_shv(iout)         = patchin%lint_shv(iin)
       patchout%lint_co2_open(iout)    = patchin%lint_co2_open(iin)
       patchout%lint_co2_closed(iout)  = patchin%lint_co2_closed(iin)
       patchout%mean_gpp(iout)         = patchin%mean_gpp(iin)
       patchout%mean_leaf_resp(iout)   = patchin%mean_leaf_resp(iin)
       patchout%mean_root_resp(iout)   = patchin%mean_root_resp(iin)
       patchout%mean_growth_resp(iout) = patchin%mean_growth_resp(iin)
       patchout%mean_storage_resp(iout)= patchin%mean_storage_resp(iin)
       patchout%mean_vleaf_resp(iout)  = patchin%mean_vleaf_resp(iin)
       patchout%today_leaf_resp(iout)  = patchin%today_leaf_resp(iin)
       patchout%today_root_resp(iout)  = patchin%today_root_resp(iin)
       patchout%today_gpp(iout)        = patchin%today_gpp(iin)
       patchout%today_nppleaf(iout)    = patchin%today_nppleaf(iin)
       patchout%today_nppfroot(iout)   = patchin%today_nppfroot(iin)
       patchout%today_nppsapwood(iout) = patchin%today_nppsapwood(iin)
       patchout%today_nppcroot(iout)   = patchin%today_nppcroot(iin)
       patchout%today_nppseeds(iout)   = patchin%today_nppseeds(iin)
       patchout%today_nppwood(iout)    = patchin%today_nppwood(iin)
       patchout%today_nppdaily(iout)   = patchin%today_nppdaily(iin)
       patchout%today_gpp_pot(iout)    = patchin%today_gpp_pot(iin)
       patchout%today_gpp_max(iout)    = patchin%today_gpp_max(iin)
       patchout%growth_respiration(iout) = patchin%growth_respiration(iin)
       patchout%storage_respiration(iout) = patchin%storage_respiration(iin)
       patchout%vleaf_respiration(iout) = patchin%vleaf_respiration(iin)
       patchout%fsn(iout)               = patchin%fsn(iin)

       patchout%monthly_dndt(iout)      = patchin%monthly_dndt(iin)
       patchout%mort_rate(:,iout)       = patchin%mort_rate(:,iin)
    
       patchout%Psi_open(iout)         = patchin%Psi_open(iin)
       patchout%krdepth(iout)          = patchin%krdepth(iin)
       patchout%first_census(iout)     = patchin%first_census(iin)
       patchout%new_recruit_flag(iout) = patchin%new_recruit_flag(iin)
       patchout%light_level(iout)      = patchin%light_level(iin)
       patchout%light_level_beam(iout) = patchin%light_level_beam(iin)
       patchout%light_level_diff(iout) = patchin%light_level_diff(iin)
       patchout%beamext_level(iout)    = patchin%beamext_level(iin)
       patchout%diffext_level(iout)    = patchin%diffext_level(iin)
       patchout%lambda_light(iout)     = patchin%lambda_light(iin)
       patchout%par_l(iout)            = patchin%par_l(iin)
       patchout%par_l_beam(iout)       = patchin%par_l_beam(iin)
       patchout%par_l_diffuse(iout)    = patchin%par_l_diffuse(iin)
       patchout%rshort_l(iout)         = patchin%rshort_l(iin)
       patchout%rshort_l_beam(iout)    = patchin%rshort_l_beam(iin)
       patchout%rshort_l_diffuse(iout) = patchin%rshort_l_diffuse(iin)
       patchout%rlong_l(iout)          = patchin%rlong_l(iin)
       patchout%rlong_l_surf(iout)     = patchin%rlong_l_surf(iin)
       patchout%rlong_l_incid(iout)    = patchin%rlong_l_incid(iin)
       patchout%rshort_w(iout)         = patchin%rshort_w(iin)
       patchout%rshort_w_beam(iout)    = patchin%rshort_w_beam(iin)
       patchout%rshort_w_diffuse(iout) = patchin%rshort_w_diffuse(iin)
       patchout%rlong_w(iout)          = patchin%rlong_w(iin)
       patchout%rlong_w_surf(iout)     = patchin%rlong_w_surf(iin)
       patchout%rlong_w_incid(iout)    = patchin%rlong_w_incid(iin)
       patchout%leaf_gbh(iout)         = patchin%leaf_gbh(iin)
       patchout%leaf_gbw(iout)         = patchin%leaf_gbw(iin)
       patchout%wood_gbh(iout)         = patchin%wood_gbh(iin)
       patchout%wood_gbw(iout)         = patchin%wood_gbw(iin)
       patchout%A_open(iout)           = patchin%A_open(iin)
       patchout%A_closed(iout)         = patchin%A_closed(iin)
       patchout%Psi_closed(iout)       = patchin%Psi_closed(iin)
       patchout%gsw_open(iout)         = patchin%gsw_open(iin)
       patchout%gsw_closed(iout)       = patchin%gsw_closed(iin)
       patchout%fsw(iout)              = patchin%fsw(iin)
       patchout%fs_open(iout)          = patchin%fs_open(iin)
       patchout%water_supply(iout)     = patchin%water_supply(iin)
       patchout%stomatal_conductance(iout) = patchin%stomatal_conductance(iin)
       patchout%leaf_maintenance(iout) = patchin%leaf_maintenance(iin)
       patchout%root_maintenance(iout) = patchin%root_maintenance(iin)
       patchout%leaf_drop(iout)        = patchin%leaf_drop(iin)
       patchout%bseeds(iout)           = patchin%bseeds(iin)
       patchout%leaf_respiration(iout) = patchin%leaf_respiration(iin)
       patchout%root_respiration(iout) = patchin%root_respiration(iin)
       patchout%gpp(iout)              = patchin%gpp(iin)
       patchout%paw_avg(iout)          = patchin%paw_avg(iin)
       patchout%elongf(iout)           = patchin%elongf(iin)
       patchout%turnover_amp(iout)     = patchin%turnover_amp(iin)
       patchout%llspan(iout)           = patchin%llspan(iin)
       patchout%vm_bar(iout)           = patchin%vm_bar(iin)
       patchout%sla(iout)              = patchin%sla(iin)

!**********************************************************************JL,XXT
       patchout%excess_carbon(iout)                = patchin%excess_carbond(iin)
       patchout%excess_carbon_fixer(iout)          = patchin%excess_carbon_fixer(iin)
       patchout% nitrogen_supply_fixer(iout)       = patchin% nitrogen_supply_fixer(iin)
       patchout%nitrogen_supply(iout)              = patchin%nitrogen_supply(iin)
       patchout%shadow_N_uptake(iout)              = patchin%shadow_N_uptake(iin)
       patchout%actual_nitrogen_uptake(iout)       = patchin%actual_nitrogen_uptake(iin)
       patchout%actual_nitrogen_uptake_fixer(iout) = patchin%actual_nitrogen_uptake_fixer(iin)
       patchout%fixation_demand(iout)              = patchin%fixation_demand(iin)
       patchout%N_fixation(iout)                   = patchin%N_fixation(iin)
       patchout%fsn_fixer(iout)                    = patchin%fsn_fixer(iin)
       patchout%nstorage(iout)                     = patchin%nstorage(iin)
       patchout%carbon_balance(iout)               = patchin%carbon_balance(iin)
       patchout%N_uptake_pot(iout)                 = patchin%N_uptake_pot(iin)
       patchout%nitrogen_uptake(iout)              = patchin%nitrogen_uptake(iin)
       patchout%carbon_balance_pot(iout)           = patchin%carbon_balance_pot(iin)
       patchout%nitrogen_supply_test(iout)         = patchin%nitrogen_supply_test(iin)
       patchout%nitrogen_supply_test2(iout)        = patchin%nitrogen_supply_test2(iin)
      !patchout%N_limitation_factor(:,iout)        = patchin%N_limitation_factor(:,iin)
      !patchout%N_limitation_factor_bar(:,iout)    = patchin%N_limitation_factor_bar(:,iin)
      !patchout%excess_carbon(iout)                = patchin%excess_carbon(iin) 
!**********************************************************************         
       patchout%old_stoma_vector(:,iout) = patchin%old_stoma_vector(:,iin)
       osdo => patchout%old_stoma_data(iout)
       osdi => patchin%old_stoma_data(iin)

       osdo%recalc           = osdi%recalc
       osdo%T_L              = osdi%T_L
       osdo%e_A              = osdi%e_A
       osdo%PAR              = osdi%PAR
       osdo%rb_factor        = osdi%rb_factor
       osdo%prss             = osdi%prss
       osdo%phenology_factor = osdi%phenology_factor
       osdo%gsw_open         = osdi%gsw_open
       osdo%ilimit           = osdi%ilimit
       osdo%T_L_residual     = osdi%T_L_residual
       osdo%e_a_residual     = osdi%e_a_residual
       osdo%par_residual     = osdi%par_residual
       osdo%rb_residual      = osdi%rb_residual
       osdo%leaf_residual    = osdi%leaf_residual
       osdo%gsw_residual     = osdi%gsw_residual

       if (imoutput > 0 .or. idoutput > 0 .or. iqoutput > 0) then
          patchout%dmean_fs_open           (iout) = patchin%dmean_fs_open           (iin)
          patchout%dmean_fsw               (iout) = patchin%dmean_fsw               (iin)
          patchout%dmean_fsn               (iout) = patchin%dmean_fsn               (iin)
          patchout%dmean_psi_open          (iout) = patchin%dmean_psi_open          (iin)
          patchout%dmean_psi_closed        (iout) = patchin%dmean_psi_closed        (iin)
          patchout%dmean_water_supply      (iout) = patchin%dmean_water_supply      (iin)
          patchout%dmean_light_level       (iout) = patchin%dmean_light_level       (iin)
          patchout%dmean_light_level_beam  (iout) = patchin%dmean_light_level_beam  (iin)
          patchout%dmean_light_level_diff  (iout) = patchin%dmean_light_level_diff  (iin)
          patchout%dmean_beamext_level     (iout) = patchin%dmean_beamext_level     (iin)
          patchout%dmean_diffext_level     (iout) = patchin%dmean_diffext_level     (iin)
          patchout%dmean_lambda_light      (iout) = patchin%dmean_lambda_light      (iin)
          patchout%dmean_gpp               (iout) = patchin%dmean_gpp               (iin)
          patchout%dmean_nppleaf           (iout) = patchin%dmean_nppleaf           (iin)
          patchout%dmean_nppfroot          (iout) = patchin%dmean_nppfroot          (iin)
          patchout%dmean_nppsapwood        (iout) = patchin%dmean_nppsapwood        (iin)
          patchout%dmean_nppcroot          (iout) = patchin%dmean_nppcroot          (iin)
          patchout%dmean_nppseeds          (iout) = patchin%dmean_nppseeds          (iin)
          patchout%dmean_nppwood           (iout) = patchin%dmean_nppwood           (iin)
          patchout%dmean_nppdaily          (iout) = patchin%dmean_nppdaily          (iin)  
          patchout%dmean_leaf_resp         (iout) = patchin%dmean_leaf_resp         (iin)
          patchout%dmean_root_resp         (iout) = patchin%dmean_root_resp         (iin)
          patchout%dmean_par_l             (iout) = patchin%dmean_par_l             (iin)
          patchout%dmean_par_l_beam        (iout) = patchin%dmean_par_l_beam        (iin)
          patchout%dmean_par_l_diff        (iout) = patchin%dmean_par_l_diff        (iin)
       end if
       if (imoutput > 0 .or. iqoutput > 0) then
          patchout%mmean_fs_open           (iout) = patchin%mmean_fs_open           (iin)
          patchout%mmean_fsw               (iout) = patchin%mmean_fsw               (iin)
          patchout%mmean_fsn               (iout) = patchin%mmean_fsn               (iin)
          patchout%mmean_psi_open          (iout) = patchin%mmean_psi_open          (iin)
          patchout%mmean_psi_closed        (iout) = patchin%mmean_psi_closed        (iin)
          patchout%mmean_water_supply      (iout) = patchin%mmean_water_supply      (iin)
          patchout%mmean_leaf_maintenance  (iout) = patchin%mmean_leaf_maintenance  (iin)
          patchout%mmean_root_maintenance  (iout) = patchin%mmean_root_maintenance  (iin)
          patchout%mmean_leaf_drop         (iout) = patchin%mmean_leaf_drop         (iin)
          patchout%mmean_cb                (iout) = patchin%mmean_cb                (iin)
          patchout%mmean_light_level       (iout) = patchin%mmean_light_level       (iin)
          patchout%mmean_light_level_beam  (iout) = patchin%mmean_light_level_beam  (iin)
          patchout%mmean_light_level_diff  (iout) = patchin%mmean_light_level_diff  (iin)
          patchout%mmean_beamext_level     (iout) = patchin%mmean_beamext_level     (iin)
          patchout%mmean_diffext_level     (iout) = patchin%mmean_diffext_level     (iin)
          patchout%mmean_gpp               (iout) = patchin%mmean_gpp               (iin)
          patchout%mmean_nppleaf           (iout) = patchin%mmean_nppleaf           (iin)
          patchout%mmean_nppfroot          (iout) = patchin%mmean_nppfroot          (iin)
          patchout%mmean_nppsapwood        (iout) = patchin%mmean_nppsapwood        (iin)
       !  patchout%mmean_nitrogen_supply   (iout) = patchin%mmean_nitrogen_supply   (iin)!JL!
       !  patchout%mmean_N_uptake_pot      (iout) = patchin%mmean_N_uptake_pot      (iin)!JL!
          patchout%mmean_nppcroot          (iout) = patchin%mmean_nppcroot          (iin)
          patchout%mmean_nppseeds          (iout) = patchin%mmean_nppseeds          (iin)
          patchout%mmean_nppwood           (iout) = patchin%mmean_nppwood           (iin)
          patchout%mmean_nppdaily          (iout) = patchin%mmean_nppdaily          (iin)  
          patchout%mmean_leaf_resp         (iout) = patchin%mmean_leaf_resp         (iin)
          patchout%mmean_root_resp         (iout) = patchin%mmean_root_resp         (iin)
          patchout%mmean_growth_resp       (iout) = patchin%mmean_growth_resp       (iin)
          patchout%mmean_storage_resp      (iout) = patchin%mmean_storage_resp      (iin)
          patchout%mmean_vleaf_resp        (iout) = patchin%mmean_vleaf_resp        (iin)
          patchout%mmean_mort_rate       (:,iout) = patchin%mmean_mort_rate       (:,iin)
          patchout%mmean_lambda_light      (iout) = patchin%mmean_lambda_light      (iin)
          patchout%mmean_par_l             (iout) = patchin%mmean_par_l             (iin)
          patchout%mmean_par_l_beam        (iout) = patchin%mmean_par_l_beam        (iin)
          patchout%mmean_par_l_diff        (iout) = patchin%mmean_par_l_diff        (iin)
       end if

       if (iqoutput > 0) then
          patchout%qmean_par_l        (:,iout) = patchin%qmean_par_l        (:,iin)
          patchout%qmean_par_l_beam   (:,iout) = patchin%qmean_par_l_beam   (:,iin)
          patchout%qmean_par_l_diff   (:,iout) = patchin%qmean_par_l_diff   (:,iin)
          patchout%qmean_fs_open      (:,iout) = patchin%qmean_fs_open      (:,iin)
          patchout%qmean_fsw          (:,iout) = patchin%qmean_fsw          (:,iin)
          patchout%qmean_fsn          (:,iout) = patchin%qmean_fsn          (:,iin)
          patchout%qmean_psi_open     (:,iout) = patchin%qmean_psi_open     (:,iin)
          patchout%qmean_psi_closed   (:,iout) = patchin%qmean_psi_closed   (:,iin)
          patchout%qmean_water_supply (:,iout) = patchin%qmean_water_supply (:,iin)
          patchout%qmean_gpp          (:,iout) = patchin%qmean_gpp          (:,iin)
          patchout%qmean_leaf_resp    (:,iout) = patchin%qmean_leaf_resp    (:,iin)
          patchout%qmean_root_resp    (:,iout) = patchin%qmean_root_resp    (:,iin)
       end if

       iin = iin + 1

    end do

    return
  end subroutine copy_patchtype
!============================================================================!
!============================================================================!





!============================================================================!
!============================================================================!
  

  ! ===============================================================
  ! Define the vtables of the state/output variables
  !
  ! The various state scalars, vectors and arrays are
  ! now populate the vtable.  The vtable indexes the array
  ! gives it a name, records its dimensions, when it is to be
  ! used as output and how (averaging and such) and most importantly
  ! saves a pointer to its starting position.  If this routine is
  ! being called as a compute node in parallel, the first position
  ! is not necessarily the first position of the whole datavector,
  ! but will only be the first position of that nodes hyperslab chunk
  ! within the given continuous dataset.
  !
  ! The first number correspond to the data level:
  ! 1. Gridtype     (polygon level)
  ! 2. Polygontype  (site level)
  ! 3. Sitetype     (patch level)
  ! 4. Patchtype    (cohort level)

  ! 9. Scalar
  ! The other numbers correspond to the kind of dimension and variable.
  ! 0. Main vector ordinate only, integer.
  ! 1. Main vector ordinate only, real.
  ! 2. Soil layer
  ! 3. Surface water layer
  ! 4. PFT
  ! 5. Disturbance Type
  ! 6. DBH class
  ! 7. FF_DBH class
  ! 8. Mortality
  ! 9. Month/13 months
  !
  ! An extra dimension for diurnal cycle can be denoted by the negative sign
  !
  ! Of these possible dimensions (2-9), they may be used concurrently
  ! to partition the data into multi-dimensional spaces, but all seven
  ! will not be used simultaneously.  The highest ranks in use are 3.
  ! Each unique combination will have a call number associated with it.
  !
  !  10    : rank 1 : polygon, integer
  !  11    : rank 1 : polygon
  ! -11    : rank 2 : polygon, diurnal cycle
  !  12    : rank 2 : polygon, s-layer
  !  120   : rank 2 : polygon, s-layer, integer
  ! -12    : rank 3 : polygon, s-layer, diurnal cycle
  !  13    : rank 2 : polygon, w-layer
  !  14    : rank 2 : polygon, pft
  !  14567 : rank 5 : polygon, pft, disturbance, dbh, age
  !  146   : rank 3 : polygon, pft, dbh
  !  15    : rank 2 : polygon, disturbance
  !  155   : rank 3 : polygon, disturbance, disturbance
  !  157   : rank 3 : polygon, disturbance, age
  !  16    : rank 2 : polygon, dbh
  !  17    : rank 2 : polygon, age
  !  18    : rank 2 : polygon, mort
  !  19    : rank 2 : polygon, month+1
  !
  !  20    : rank 1 : site, integer
  !  21    : rank 1 : site
  !  22    : rank 2 : site, s-layer
  !  23    : rank 2 : site, w-layer
  !  24    : rank 2 : site, pft
  !  246   : rank 3 : site, pft, dbh
  !  25    : rank 2 : site, disturbance
  !  255   : rank 3 : site, disturbance, disturbance
  !  26    : rank 2 : site, dbh
  !  27    : rank 2 : site, age
  !  28    : rank 2 : site, mort
  !  29    : rank 2 : site, month
  !
  !  30    : rank 1 : patch, integer
  !  31    : rank 1 : patch
  ! -31    : rank 2 : patch, diurnal cycle
  !  32    : rank 2 : patch, s-layer
  !  33    : rank 2 : patch, w-layer
  !  34    : rank 2 : patch, pft
  !  346   : rank 3 : patch, pft, ff_dbh
  !  35    : rank 2 : patch, disturbance
  !  36    : rank 2 : patch, dbh
  !  37    : rank 2 : patch, age
  !  38    : rank 2 : patch, mort
  !
  !  40    : rank 1 : cohort, integer
  !  41    : rank 1 : cohort
  ! -41    : rank 2 : cohort, diurnal cycle
  !  44    : rank 2 : cohort, pft
  !  46    : rank 2 : cohort, dbh
  !  47    : rank 2 : cohort, age
  !  48    : rank 2 : cohort, mort
  !  49    : rank 2 : cohort, month+1
  !
  !  90    : rank 0 : integer scalar 
  !  92    : rank 1 : s-layer
  !===================================================================
  
  subroutine filltab_alltypes

    ! =================================================
    !
    ! This subroutine is the main driver for filling
    ! filling the var_table of ED variables.  On a
    ! serial computing environment, this routine should be
    ! called near the end of the initialization process
    ! after the hierarchical tree structure has been
    ! trimmed via fusion/fission.  Similiarly, this
    ! routine should be called after any fusion/fission
    ! process, assuming that the major vtable structures
    ! have been deallocated prior to reallocation.
    !
    ! In a paralell environment, this routine should
    ! operate in a similiar fashion on each of the compute
    ! nodes.  It is designed such that the compute nodes
    ! will write hyperslabs of data in parallel to a
    ! joing HDF5 dataset as "collective-chunked" data
    ! The modifications that must be made after running this
    ! subroutine, are that the indexes should account
    ! for the offset of the current compute node.
    !
    ! =================================================
    
    
    use ed_var_tables,only:num_var,vt_info,var_table,nullify_vt_vector_pointers
    use ed_node_coms,only:mynum,mchnum,machs,nmachs,nnodetot,sendnum,recvnum,master_num
    use ed_max_dims, only: maxgrds, maxmach
    implicit none
    
    include 'mpif.h'

    integer :: ncohorts_g,npatches_g,nsites_g
    integer :: igr,ipy,isi,ipa,nv,ierr,nm,iptr
    integer,       dimension(MPI_STATUS_SIZE) :: status
    integer :: ping,uniqueid
    logical,save :: model_start = .true.
   
    type(edtype),pointer      :: cgrid
    type(polygontype),pointer :: cpoly
    type(sitetype),pointer    :: csite
    type(patchtype),pointer   :: cpatch
    logical :: verbose = .false.    

!    if (mynum == 1) then
!       write(*,"(a)")'--- Re-hashing the IO pointer tables and mapping arrays'
!    end if


    ! The first loop through populates the info tables

    do igr = 1,ngrids
       cgrid => edgrid_g(igr)
       
       if (num_var(igr)>0) then
          do nv=1,num_var(igr)
             if (associated(vt_info(nv,igr)%vt_vector)) then
                do iptr=1,vt_info(nv,igr)%nptrs
                   call nullify_vt_vector_pointers(vt_info(nv,igr)%vt_vector(iptr))
                end do
                deallocate(vt_info(nv,igr)%vt_vector)
             end if
          end do
       end if

       num_var(igr) = 0

       cgrid%npolygons_global = cgrid%npolygons
       cgrid%nsites_global    = get_nsites(cgrid)
       cgrid%npatches_global  = get_npatches(cgrid)
       cgrid%ncohorts_global  = get_ncohorts(cgrid)
       
       cgrid%mach_cohort_offset_index = 0
       cgrid%mach_patch_offset_index  = 0
       cgrid%mach_site_offset_index   = 0
       cgrid%mach_polygon_offset_index= 0

       if (nnodetot /= 1) then

          ! Send all them sizes to root (CHANGED, NODE 1)
          

          if (mynum == 1) then
          
             gdpy(1,igr) = cgrid%npolygons_global
             gdsi(1,igr) = cgrid%nsites_global
             gdpa(1,igr) = cgrid%npatches_global
             gdco(1,igr) = cgrid%ncohorts_global

             call MPI_Send(ping,1,MPI_INTEGER,sendnum,94,MPI_COMM_WORLD,ierr)
             
             ! Have node 1 recieve the info
             do nm=2,nnodetot
                uniqueid=((igr-1)*maxmach)+nm
                call MPI_Recv(gdpy(nm,igr),1,MPI_INTEGER,machs(nm),500000+uniqueid,MPI_COMM_WORLD,status,ierr)
                call MPI_Recv(gdsi(nm,igr),1,MPI_INTEGER,machs(nm),600000+uniqueid,MPI_COMM_WORLD,status,ierr)
                call MPI_Recv(gdpa(nm,igr),1,MPI_INTEGER,machs(nm),700000+uniqueid,MPI_COMM_WORLD,status,ierr)
                call MPI_Recv(gdco(nm,igr),1,MPI_INTEGER,machs(nm),800000+uniqueid,MPI_COMM_WORLD,status,ierr)
             end do

             ! Broadcast all this info to the nodes
             do nm=2,nnodetot
                uniqueid=((igr-1)*maxmach)+nm
                call MPI_Send(gdpy,maxmach*maxgrds,MPI_INTEGER,machs(nm), 900000+uniqueid,MPI_COMM_WORLD,ierr)
                call MPI_Send(gdsi,maxmach*maxgrds,MPI_INTEGER,machs(nm),1000000+uniqueid,MPI_COMM_WORLD,ierr)
                call MPI_Send(gdpa,maxmach*maxgrds,MPI_INTEGER,machs(nm),1100000+uniqueid,MPI_COMM_WORLD,ierr)
                call MPI_Send(gdco,maxmach*maxgrds,MPI_INTEGER,machs(nm),1200000+uniqueid,MPI_COMM_WORLD,ierr)
             end do

         else

            ! Set the blocking recieve to allow ordering, start with machine 1
            call MPI_Recv(ping,1,MPI_INTEGER,recvnum,94,MPI_COMM_WORLD,status,ierr)

            uniqueid=((igr-1)*maxmach)+mynum
            ! Send the information to node (1)
            call MPI_Send(cgrid%npolygons_global, 1,MPI_INTEGER,machs(1),500000+uniqueid,MPI_COMM_WORLD,ierr)
            call MPI_Send(cgrid%nsites_global   , 1,MPI_INTEGER,machs(1),600000+uniqueid,MPI_COMM_WORLD,ierr)
            call MPI_Send(cgrid%npatches_global , 1,MPI_INTEGER,machs(1),700000+uniqueid,MPI_COMM_WORLD,ierr)
            call MPI_Send(cgrid%ncohorts_global , 1,MPI_INTEGER,machs(1),800000+uniqueid,MPI_COMM_WORLD,ierr)
          
            ! When this node is finished, send the blocking MPI_Send to the next machine
            if (mynum /= nnodetot) call MPI_Send(ping,1,MPI_INTEGER,sendnum,94,MPI_COMM_WORLD,ierr)

            uniqueid=((igr-1)*maxmach)+mynum
            call MPI_Recv(gdpy,maxmach*maxgrds,MPI_INTEGER,machs(1), 900000+uniqueid,MPI_COMM_WORLD,status,ierr)
            call MPI_Recv(gdsi,maxmach*maxgrds,MPI_INTEGER,machs(1),1000000+uniqueid,MPI_COMM_WORLD,status,ierr)
            call MPI_Recv(gdpa,maxmach*maxgrds,MPI_INTEGER,machs(1),1100000+uniqueid,MPI_COMM_WORLD,status,ierr)
            call MPI_Recv(gdco,maxmach*maxgrds,MPI_INTEGER,machs(1),1200000+uniqueid,MPI_COMM_WORLD,status,ierr)
 
         end if


         if(mynum == 1 .and. model_start .and. verbose) then
            
            print*,"Global Polygons: ",gdpy(1:nnodetot,igr)
            print*,"Global Site: "    ,gdsi(1:nnodetot,igr)
            print*,"Global Patches: " ,gdpa(1:nnodetot,igr)
            print*,"Global Cohorts: " ,gdco(1:nnodetot,igr)

         end if

         ! Calculate the offsets that each machine has
         
         py_off(1,igr) = 0
         si_off(1,igr) = 0
         pa_off(1,igr) = 0
         co_off(1,igr) = 0
         do nm=2,nnodetot
            py_off(nm,igr) = py_off(nm-1,igr) + gdpy(nm-1,igr)
            si_off(nm,igr) = si_off(nm-1,igr) + gdsi(nm-1,igr)
            pa_off(nm,igr) = pa_off(nm-1,igr) + gdpa(nm-1,igr)
            co_off(nm,igr) = co_off(nm-1,igr) + gdco(nm-1,igr)
         end do
         
         ! Calculate the total sizes of the arrays
         
         cgrid%npolygons_global = sum(gdpy(1:nnodetot,igr))
         cgrid%nsites_global    = sum(gdsi(1:nnodetot,igr))
         cgrid%npatches_global  = sum(gdpa(1:nnodetot,igr))
         cgrid%ncohorts_global  = sum(gdco(1:nnodetot,igr))

         ! Calculate the local offsets
         
         cgrid%mach_polygon_offset_index = py_off(mynum,igr)
         cgrid%mach_site_offset_index    = si_off(mynum,igr)
         cgrid%mach_patch_offset_index   = pa_off(mynum,igr)
         cgrid%mach_cohort_offset_index  = co_off(mynum,igr)

          
       end if

       call filltab_globtype(igr)

       call filltab_edtype(igr,0)
       
       if (gdpy(mynum,igr)>0) then
          call filltab_polygontype(igr,1,0)
          call filltab_sitetype(igr,1,1,0)
          call filltab_patchtype(igr,1,1,1,0)
       end if
       
       
    end do


    do igr = 1,ngrids

       ! Test to see if the var_table has been initialized. If it has
       ! then deallocate its pointers and reset its first flag. These
       ! will be reallocated on the first pass of the filltab_
       ! subroutines.

       cgrid => edgrid_g(igr)

       cgrid%pyglob_id = 0 + cgrid%mach_polygon_offset_index
       
       ! Determine the total number of variables for each grid
       ! These will determine the length of the vt_vector

       call filltab_edtype(igr,1)
       
       ncohorts_g = 0 + cgrid%mach_cohort_offset_index
       npatches_g = 0 + cgrid%mach_patch_offset_index
       nsites_g   = 0 + cgrid%mach_site_offset_index
       
       do ipy = 1,cgrid%npolygons
          
          cpoly => cgrid%polygon(ipy)
          
          cpoly%siglob_id = nsites_g + 0 ! This is the offset for the vtable write
          
          cgrid%pysi_id(ipy) = nsites_g + 1 ! This is the index written in the file
                                            ! for the user to reference

          cgrid%pysi_n(ipy) = cpoly%nsites

          nsites_g = nsites_g + cpoly%nsites
          
          call filltab_polygontype(igr,ipy,1)
          
          do isi = 1,cpoly%nsites
             
             csite => cpoly%site(isi)
             
             csite%paglob_id = npatches_g + 0

             cpoly%sipa_id(isi) = npatches_g + 1

             cpoly%sipa_n(isi) = csite%npatches

             npatches_g = npatches_g + csite%npatches
             
             call filltab_sitetype(igr,ipy,isi,1)
             
             do ipa = 1,csite%npatches

                cpatch => csite%patch(ipa)
                
                cpatch%coglob_id = ncohorts_g + 0

                csite%paco_id(ipa) = ncohorts_g + 1

                csite%paco_n(ipa) = cpatch%ncohorts

                ncohorts_g = ncohorts_g + cpatch%ncohorts

                if (cpatch%ncohorts > 0 ) then
                   
                   call filltab_patchtype(igr,ipy,isi,ipa,1)
                   
                end if

             end do
             
          end do
          
       end do
!       if (mynum.eq.1) then
!          write(*,"(a)")'--- Mapping Completed'
!       end if

       if (mynum.eq.1 .and. model_start .and. verbose) then
          model_start = .false.
          do nv=1,num_var(igr)
!             write(*,"(a,i4,a,i4,a,a)")'Registering: ',nv,' of',num_var(igr),'  ',vt_info(nv,igr)%name
          end do
       end if 

    end do


    return
  end subroutine filltab_alltypes
!==========================================================================================!
!==========================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine filltab_globtype(igr)

      use ed_var_tables, only : vtable_edio_r       & ! sub-routine
                              , vtable_edio_r_sca   & ! sub-rouitne
                              , vtable_edio_i_sca   ! ! sub-rouitne
      use soil_coms    , only : slz                 & ! intent(in)
                              , slxclay             & ! intent(in)
                              , slxsand             & ! intent(in)
                              , isoilflg            ! ! intent(in)


      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      integer     , intent(in) :: igr
      !----- Local variables. -------------------------------------------------------------!
      integer                  :: var_len
      integer                  :: max_ptrs
      integer                  :: var_len_global
      integer                  :: nvar
      type(edtype), pointer    :: cgrid
      !------------------------------------------------------------------------------------!


      cgrid => edgrid_g(igr)


      !------------------------------------------------------------------------------------!
      !     Single values (scalars).                                                       !
      !------------------------------------------------------------------------------------!
      var_len        = 1
      var_len_global = 1
      max_ptrs       = 1

      nvar=1
      call vtable_edio_i_sca(cgrid%npolygons_global,nvar,igr,0,0                           &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NPOLYGONS_GLOBAL :90:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_i_sca(cgrid%npolygons_global,nvar,igr,1,0                           &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NPOLYGONS_GLOBAL :90:hist:anal:dail:mont:dcyc:year')
      
      nvar=nvar+1
      call vtable_edio_i_sca(cgrid%nsites_global,nvar,igr,0,0                              &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NSITES_GLOBAL :90:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_i_sca(cgrid%nsites_global,nvar,igr,1,0                              &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NSITES_GLOBAL :90:hist:anal:dail:mont:dcyc:year')
      
      nvar=nvar+1
      call vtable_edio_i_sca(cgrid%npatches_global,nvar,igr,0,0                            &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NPATCHES_GLOBAL :90:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_i_sca(cgrid%npatches_global,nvar,igr,1,0                            &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NPATCHES_GLOBAL :90:hist:anal:dail:mont:dcyc:year')

      nvar=nvar+1
      call vtable_edio_i_sca(cgrid%ncohorts_global,nvar,igr,0,0                            &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NCOHORTS_GLOBAL :90:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_i_sca(cgrid%ncohorts_global,nvar,igr,1,0                            &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NCOHORTS_GLOBAL :90:hist:anal:dail:mont:dcyc:year')

      nvar=nvar+1
      call vtable_edio_i_sca(nzg,nvar,igr,0,0                                              &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NZG :90:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_i_sca(nzg,nvar,igr,1,0                                              &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NZG :90:hist:anal:dail:mont:dcyc:year')

      nvar=nvar+1
      call vtable_edio_i_sca(nzs,nvar,igr,0,0                                              &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NZS :90:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_i_sca(nzs,nvar,igr,1,0                                              &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NZS :90:hist:anal:dail:mont:dcyc:year')

      nvar=nvar+1
      call vtable_edio_i_sca(ff_nhgt,nvar,igr,0,0                                              &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'FF_NHGT :90:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_i_sca(ff_nhgt,nvar,igr,1,0                                          &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'FF_NHGT :90:hist:anal:dail:mont:dcyc:year')

      nvar=nvar+1
      call vtable_edio_i_sca(ndcycle,nvar,igr,0,0                                              &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NDCYCLE :90:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_i_sca(ndcycle,nvar,igr,1,0                                              &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'NDCYCLE :90:hist:anal:dail:mont:dcyc:year')

      nvar=nvar+1
      call vtable_edio_i_sca(isoilflg(igr),nvar,igr,0,0                                    &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'ISOILFLG :90:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_i_sca(isoilflg(igr),nvar,igr,1,0                                    &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'ISOILFLG :90:hist:anal:dail:mont:dcyc:year')

      nvar=nvar+1
      call vtable_edio_r_sca(slxsand,nvar,igr,0,0                                          &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'SLXSAND :90:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_r_sca(slxsand,nvar,igr,1,0                                          &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'SLXSAND :90:hist:anal:dail:mont:dcyc:year')

      nvar=nvar+1
      call vtable_edio_r_sca(slxclay,nvar,igr,0,0                                          &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'SLXCLAY :90:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_r_sca(slxclay,nvar,igr,1,0                                          &
                            ,var_len,var_len_global,max_ptrs                               &
                            ,'SLXCLAY :90:hist:anal:dail:mont:dcyc:year')


      !------------------------------------------------------------------------------------!
      !    1-D variables, soil layers.                                                     !
      !------------------------------------------------------------------------------------!
      var_len        = nzg
      var_len_global = nzg



      nvar=nvar+1
      call vtable_edio_r(nzg,slz,nvar,igr,0,0                                              &
                        ,var_len,var_len_global,max_ptrs                                   &
                        ,'SLZ :92:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_r(nzg,slz,nvar,igr,1,0                                              &
                        ,var_len,var_len_global,max_ptrs                                   &
                        ,'SLZ :92:hist:anal:dail:mont:dcyc:year')
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !    1-D variables, height classes.                                                  !
      !------------------------------------------------------------------------------------!
      var_len        = ff_nhgt
      var_len_global = ff_nhgt



      nvar=nvar+1
      call vtable_edio_r(ff_nhgt,hgt_class,nvar,igr,0,0                                    &
                        ,var_len,var_len_global,max_ptrs                                   &
                        ,'HGT_CLASS :96:hist:anal:dail:mont:dcyc:year')
      call vtable_edio_r(ff_nhgt,hgt_class,nvar,igr,1,0                                    &
                        ,var_len,var_len_global,max_ptrs                                   &
                        ,'HGT_CLASS :96:hist:anal:dail:mont:dcyc:year')
      !------------------------------------------------------------------------------------!


   


      !----- Save the number of global-level variables that go to the output. -------------!
      nioglobal=nvar
      !------------------------------------------------------------------------------------!

      return
   end subroutine filltab_globtype
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype).                                                                             !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype(igr,init)

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      integer     , intent(in) :: init
      integer     , intent(in) :: igr
      !----- Local variables. -------------------------------------------------------------!
      type(edtype), pointer    :: cgrid
      integer                  :: var_len
      integer                  :: max_ptrs
      integer                  :: var_len_global
      integer                  :: nvar
      integer                  :: npts
      !------------------------------------------------------------------------------------!

      cgrid => edgrid_g(igr)

      !------ Define the global dimensions. -----------------------------------------------!
      var_len        = cgrid%npolygons
      var_len_global = cgrid%npolygons_global
      max_ptrs       = 1

      !------ Continue the counting. ------------------------------------------------------!
      nvar = nioglobal

      call filltab_edtype_p10 (cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      call filltab_edtype_p11 (cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      call filltab_edtype_m11 (cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      call filltab_edtype_p120(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      call filltab_edtype_p12 (cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      call filltab_edtype_m12 (cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      call filltab_edtype_p14 (cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      call filltab_edtype_p16 (cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      call filltab_edtype_p19 (cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      call filltab_edtype_p146(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      call filltab_edtype_p199(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      call filltab_edtype_p155(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      !----- Save the number of polygon-level (edtype) variables that go to the output. ---!
      if (init == 0) niogrid=nvar-nioglobal
      !------------------------------------------------------------------------------------!

      return
     
   end subroutine filltab_edtype
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have one dimension and are integer (type 10).                           !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_p10(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_i  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!
      
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 1-D block.  All variables must have the number of points defined   !
      ! by npts.                                                                           !
      !------------------------------------------------------------------------------------!

      npts = cgrid%npolygons

      if (associated(cgrid%pysi_id)) then
         nvar = nvar + 1
         call vtable_edio_i(npts,cgrid%pysi_id,nvar,igr,init,cgrid%pyglob_id               &
                           ,var_len,var_len_global,max_ptrs                                &
                           ,'PYSI_ID :10:hist:anal:dail:mont:dcyc:year')
         call metadata_edio(nvar,igr,'Polygons first site indices','NA','ipoly')
      end if

      
      if (associated(cgrid%pysi_n)) then
         nvar=nvar+1
         call vtable_edio_i(npts,cgrid%pysi_n,nvar,igr,init,cgrid%pyglob_id                &
                           ,var_len,var_len_global,max_ptrs                                &
                           ,'PYSI_N :10:hist:anal:dail:mont:dcyc:year')
         call metadata_edio(nvar,igr,'Number of sites per polygon','NA','ipoly')
      end if

      if (associated(cgrid%xatm)) then
         nvar=nvar+1
         call vtable_edio_i(npts,cgrid%xatm,nvar,igr,init,cgrid%pyglob_id                  &
                           ,var_len,var_len_global,max_ptrs                                &
                           ,'XATM :10:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'Atm. cell x-indices of polygon','NA','ipoly')
      end if
      
      if (associated(cgrid%yatm)) then
         nvar=nvar+1
         call vtable_edio_i(npts,cgrid%yatm,nvar,igr,init,cgrid%pyglob_id                  &
                           ,var_len,var_len_global,max_ptrs                                &
                           ,'YATM :10:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'Atm cell y-indices of polygon','NA','ipoly')
      end if
      
      if (associated(cgrid%lsl)) then
         nvar=nvar+1
         call vtable_edio_i(npts,cgrid%lsl,nvar,igr,init,cgrid%pyglob_id                   &
                           ,var_len,var_len_global,max_ptrs                                &
                           ,'LSL :10:hist:anal:dail:mont:dcyc:year')
         call metadata_edio(nvar,igr,'Index of lowest soil layer','NA','ipoly')
         
      end if
      
      if (associated(cgrid%load_adjacency)) then
         nvar=nvar+1
         call vtable_edio_i(npts,cgrid%load_adjacency,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'LOAD_ADJACENCY :10:hist') 
         call metadata_edio(nvar,igr,'Load Adjacency','[NA]','ipoly')
      end if

      return     
   end subroutine filltab_edtype_p10
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have one dimension and are real (type 11).                              !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_p11(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_r  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!


      
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 1-D block.  All variables must have the number of points defined   !
      ! by npts.                                                                           !
      !------------------------------------------------------------------------------------!
      npts = cgrid%npolygons




      if (associated(cgrid%lat)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%lat,nvar,igr,init,cgrid%pyglob_id                   &
                           ,var_len,var_len_global,max_ptrs                                &
                           ,'LATITUDE :11:hist:anal:dail:mont:dcyc:year')
         call metadata_edio(nvar,igr,'Latitude of Polygon','decimal degrees','ipoly')
      end if


      if (associated(cgrid%lon)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%lon,nvar,igr,init,cgrid%pyglob_id                   &
                           ,var_len,var_len_global,max_ptrs                                &
                           ,'LONGITUDE :11:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'Longitude of Polygon','decimal degrees','ipoly')
      end if
      
      if (associated(cgrid%wbar)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%wbar,nvar,igr,init,cgrid%pyglob_id                  &
                           ,var_len,var_len_global,max_ptrs                                &
                           ,'WBAR :11:hist') 
         call metadata_edio(nvar,igr,'Polygon average topographic moisture index'          &
                           ,'NA','ipoly')
      end if
      
      if (associated(cgrid%Te)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Te,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'TE :11:hist') 
         call metadata_edio(nvar,igr,'NA','NA','ipoly')
      end if
      
      if (associated(cgrid%zbar)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%zbar,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'ZBAR :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon average water table depth','[m]','ipoly')
      end if
      
      if (associated(cgrid%sheat)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%sheat,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'SHEAT :11:hist') 
         call metadata_edio(nvar,igr,'soil heat pool for lateral hydrology','NA','ipoly')
      end if
      
      if (associated(cgrid%baseflow)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%baseflow,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'BASEFLOW :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'loss of water from site to watershed discharge','kg/m2/s','ipoly')
      end if
      
      if (associated(cgrid%runoff)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%runoff,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'RUNOFF :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'NA','NA','ipoly')
      end if
      
      if (associated(cgrid%swliq)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%swliq,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'SWLIQ :11:hist:anal') 
         call metadata_edio(nvar,igr,'NA','NA','ipoly')
      end if

      if (associated(cgrid%total_agb)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_agb,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'TOTAL_AGB :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Polygon Total Above Ground Biomass','[kgC/m2]','ipoly')
      end if
      
      if (associated(cgrid%total_basal_area)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_basal_area,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'TOTAL_BASAL_AREA :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Polygon Total Basal Area','[cm2/m2]','ipoly')
         
      end if

      if (associated(cgrid%total_agb_growth)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_agb_growth,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'TOTAL_AGB_GROWTH:11:hist:anal:year') 
          call metadata_edio(nvar,igr,'Polygon AGB gain through growth','[kgC/m2/yr]','ipoly')
      end if
      
      if (associated(cgrid%total_agb_mort)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_agb_mort,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'TOTAL_AGB_MORT :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Polygon AGB lost due to mortality','[kgC/m2/yr]','ipoly')
      end if
      
      if (associated(cgrid%total_agb_recruit)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_agb_recruit,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'TOTAL_AGB_RECRUIT :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Polygon AGB used to generate recruits','[kgC/m2/yr]','ipoly')
      end if
      
      if (associated(cgrid%total_basal_area_growth)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_basal_area_growth,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'TOTAL_BASAL_AREA_GROWTH :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Polygon basal area gained through growth ','[kgC/m2/yr]','ipoly')
      end if
      
      if (associated(cgrid%total_basal_area_mort)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_basal_area_mort,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'TOTAL_BASAL_AREA_MORT :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Polygon basal area lost through growth ','[cm2/m2/yr]','ipoly')
      end if
      
      if (associated(cgrid%total_basal_area_recruit)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_basal_area_recruit,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'TOTAL_BASAL_AREA_RECRUIT :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Polygon basal area gained by recruits','[cm2/m2/yr]','ipoly')
      end if
      
      if (associated(cgrid%cosz)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%cosz,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'COSZ :11:hist') 
         call metadata_edio(nvar,igr,'Cosine of the zenith angle','[a/h]','ipoly')
      end if
      
      if (associated(cgrid%cbudget_initialstorage)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%cbudget_initialstorage,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CBUDGET_INITIALSTORAGE :11:hist') 
         call metadata_edio(nvar,igr,'Vegetation and soil carbon,at start of budget-averaging','[kgC/m2]','ipoly')
      end if
          
      if (associated(cgrid%cbudget_nep)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%cbudget_nep,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CBUDGET_NEP :11:hist') 
         call metadata_edio(nvar,igr,'Polygon average net ecosystem production','[kgC/m2/day]','ipoly')
      end if
      
      if (associated(cgrid%nbudget_initialstorage)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%nbudget_initialstorage,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NBUDGET_INITIALSTORAGE :11:hist') 
         call metadata_edio(nvar,igr,'Veg and soil nitrogen, at start of budget-averaging','[kgN/m2]','ipoly')
      end if

      if (associated(cgrid%avg_vapor_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_vapor_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_VAPOR_LC :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'polygon leaf to canopy air vapor flux','[kg/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_vapor_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_vapor_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_VAPOR_WC :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'polygon wood to canopy air vapor flux','[kg/m2/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_dew_cg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_dew_cg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_DEW_CG :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon averaged dew to ground flux','[kg/m2/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_vapor_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_vapor_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_VAPOR_GC :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'polygon moisture flux ground to canopy air','[kg/m2/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_wshed_vg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_wshed_vg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_WSHED_VG :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged water shed from vegetation to ground','[kg/m2/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_intercepted)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_intercepted,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_INTERCEPTED :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged intercepted precipitation by vegetation','[kg/m2/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_throughfall)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_throughfall,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_THROUGHFALL :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged throughfall precipitation','[kg/m2/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_vapor_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_vapor_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_VAPOR_AC :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'polygon vapor flux atmosphere to canopy air','[kg/m2/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_transp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_transp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_TRANSP :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'polygon transpiration from stomata to canopy air space','[kg/m2/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_evap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_evap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_EVAP :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon averaged evap/dew from ground and leaves to CAS','[kg/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_runoff)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_runoff,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_RUNOFF :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon average surface runoff','[kg/m2/s]','NA') 
      end if
      
      if (associated(cgrid%avg_drainage)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_drainage,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_DRAINAGE :11:hist:anal') 
         call metadata_edio(nvar,igr,'polygon average water flux through lower soil layer','[kg/m2/s]','ipoly') 
      end if
       
      if (associated(cgrid%avg_drainage_heat)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_drainage_heat,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_DRAINAGE_HEAT :11:hist:anal') 
         call metadata_edio(nvar,igr,'polygon average internal energy loss through lower soil layer','[W/m2]','ipoly') 
      end if
     
      if (associated(cgrid%aux)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%aux,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AUX :11:hist:anal') 
         call metadata_edio(nvar,igr,'Auxillary variable - user discretion,see rk4_derivs.f90','[user-defined]','ipoly') 
      end if
      

      if (associated(cgrid%avg_rshort_gnd)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_rshort_gnd,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_RSHORT_GND :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged ground absorbed SW radiation','[W/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_rlong_gnd)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_rlong_gnd,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_RLONG_GND :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged ground absorbed LW radiation','[W/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_carbon_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_carbon_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_CARBON_AC :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged vegetation to canopy air  CO2 flux','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_sensible_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_sensible_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SENSIBLE_LC :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged leaf to canopy air sensible heat flux','[W/m2]','ipoly') 
      end if
 
      if (associated(cgrid%avg_sensible_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_sensible_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SENSIBLE_WC :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged wood to canopy air sensible heat flux','[W/m2]','ipoly') 
      end if
     
      if (associated(cgrid%avg_qwshed_vg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_qwshed_vg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_QWSHED_VG :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged internal energy flux of water shed from vegetation to ground','[W/m2]','ipoly') 
      end if
       
      if (associated(cgrid%avg_qintercepted)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_qintercepted,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_QINTERCEPTED :11:hist:anal') 
         call metadata_edio(nvar,igr,&
         'Polygon averaged internal energy flux of intercepted precipitation by vegetation','[W/m2]','ipoly') 
      end if
       
      if (associated(cgrid%avg_qthroughfall)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_qthroughfall,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_QTHROUGHFALL :11:hist:anal') 
         call metadata_edio(nvar,igr,&
             'Polygon averaged internal energy flux of throughfall precipitation','[W/m2]','ipoly') 
      end if
     
      if (associated(cgrid%avg_sensible_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_sensible_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SENSIBLE_GC :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged sensible heat flux ground to canopy air','[W/m2]','ipoly') 
      end if
      
      if (associated(cgrid%avg_sensible_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_sensible_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SENSIBLE_AC :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon averaged sensible heat flux atmosphere  to canopy','[W/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_runoff_heat)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_runoff_heat,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_RUNOFF_HEAT :11:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      ! ---------------------------------------------
      
      if (associated(cgrid%avg_gpp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_gpp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_GPP :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average GPP','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_nppleaf)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_nppleaf,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_NPPLEAF :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average NPP leaf','[kgC/m2/day]','ipoly') 
      end if

      if (associated(cgrid%avg_nppfroot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_nppfroot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_NPPFROOT :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average NPP froot','[kgC/m2/day]','ipoly') 
      end if
            
      if (associated(cgrid%avg_nppsapwood)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_nppsapwood,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_NPPSAPWOOD :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average NPP sapwood','[kgC/m2/day]','ipoly') 
      end if
      
      if (associated(cgrid%avg_nppcroot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_nppcroot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_NPPCROOT :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average NPP croot','[kgC/m2/day]','ipoly') 
      end if

      if (associated(cgrid%avg_nppseeds)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_nppseeds,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_NPPSEEDS :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average NPP seeds','[kgC/m2/day]','ipoly') 
      end if


      if (associated(cgrid%avg_nppwood)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_nppwood,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_NPPWOOD :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average NPP wood','[kgC/m2/day]','ipoly') 
      end if
      
      if (associated(cgrid%avg_nppdaily)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_nppdaily,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_NPPDAILY :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average NPP daily','[kgC/m2/day]','ipoly') 
      end if

      if (associated(cgrid%lai)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%lai,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'LAI :11:hist:anal:dail') 
         call metadata_edio(nvar,igr,'Polygon  LAI','[m2/m2]','ipoly') 
      end if

      if (associated(cgrid%wpa)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%wpa,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'WPA :11:hist:anal:dail') 
         call metadata_edio(nvar,igr,'Polygon wood projected area','[m2/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_lma)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_lma,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_LMA :11:hist:dail') 
         call metadata_edio(nvar,igr,'Polygon LMA','[NA]','ipoly') 
      end if

      if (associated(cgrid%wai)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%wai,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'WAI :11:hist:anal:dail') 
         call metadata_edio(nvar,igr,'Polygon  wood area index','[m2/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_leaf_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_leaf_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_LEAF_RESP :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Leaf Respiration','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_root_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_root_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_ROOT_RESP :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Root Respiration','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_growth_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_growth_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_GROWTH_RESP :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Growth Respiration','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_storage_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_storage_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_STORAGE_RESP :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Storage Respiration','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_vleaf_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_vleaf_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_VLEAF_RESP :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Virtual Leaf Respiration','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_plant_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_plant_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_PLANT_RESP :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Plant Respiration','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_growth_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_growth_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_GROWTH_RESP :11:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Growth Respiration','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_storage_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_storage_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_STORAGE_RESP :11:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Storage Respiration','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_vleaf_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_vleaf_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_VLEAF_RESP :11:opti') 
         call metadata_edio(nvar,igr,'Polygon Average VLeaf Respiration','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_htroph_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_htroph_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_HTROPH_RESP :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Heterotrohic Respiration','[umol/m2/s]','ipoly') 
      end if

      if (associated(cgrid%avg_leaf_drop)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_leaf_drop,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_LEAF_DROP :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Leaf loss','[kgC/m2/yr]','ipoly') 
      end if

      if (associated(cgrid%avg_leaf_maintenance)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_leaf_maintenance,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_LEAF_MAINTENANCE :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Leaf maintenance cost','[kgC/m2/day]','ipoly') 
      end if

      if (associated(cgrid%avg_root_maintenance)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_root_maintenance,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_ROOT_MAINTENANCE :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average fine root maintenance cost','[kgC/m2/yr]','ipoly') 
      end if


         !!! added for NACP intercomparison (MCD)
      if (associated(cgrid%avg_sfcw_depth)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_sfcw_depth,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SNOWDEPTH :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Poly Avg. Snow Depth ','[m]','ipoly') 
      end if
      if (associated(cgrid%avg_sfcw_energy)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_sfcw_energy,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SNOWENERGY :11:hist:anal') 
         call metadata_edio(nvar,igr,'Poly Avg. Snow Energy ','[J/kg]','ipoly') 
      end if
      if (associated(cgrid%avg_sfcw_mass)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_sfcw_mass,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SNOWMASS :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Poly Avg. Snow Mass (SWE) ','[kg/m2]','ipoly') 
      end if
      if (associated(cgrid%avg_sfcw_tempk)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_sfcw_tempk,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SNOWTEMP :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Poly Avg. Snow Temperature','[K]','ipoly') 
      end if
      if (associated(cgrid%avg_sfcw_fracliq)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_sfcw_fracliq,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SNOWFRACLIQ :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Poly Avg. Snow liquid fraction','[proportion]','ipoly') 
      end if
      if (associated(cgrid%avg_bdead)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_bdead,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_BDEAD :11:hist:opti:dail:anal:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Biomass - structural','[kgC/m2]','ipoly') 
      end if
      if (associated(cgrid%avg_bstorage)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_bstorage,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_BSTORAGE :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Biomass - storage','[kgC/m2]','ipoly') 
      end if
      if (associated(cgrid%avg_bseeds)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_bseeds,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_BSEEDS :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Biomass - seeds','[kgC/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_balive)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_balive,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_BALIVE :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Biomass -- living','[kgC/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_bleaf)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_bleaf,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_BLEAF :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Biomass -- leaf','[kgC/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_broot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_broot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_BROOT :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Biomass -- fine roots','[kgC/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_bsapwood)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_bsapwood,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_BSAPWOOD :11:hist:anal:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Biomass -- sapwood','[kgC/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_fsc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_fsc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_FSC :11:hist:anal:opti:dail:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Fast Soil Carbon','[kg/m2]','ipoly') 
      end if
      if (associated(cgrid%avg_ssc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_ssc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SSC :11:hist:anal:opti:dail:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Slow Soil Carbon','[kg/m2]','ipoly') 
      end if
      if (associated(cgrid%avg_stsc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_stsc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_STSC :11:hist:anal:opti:year:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Structural Soil Carbon','[kg/m2]','ipoly') 
      end if


      if (associated(cgrid%avg_fsn)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_fsn,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_FSN :11:hist:anal:opti:dail:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Fast Soil Nitrogen','[kg/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_msn)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_msn,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_MSN :11:hist:anal:opti:dail:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Mineralized Soil Carbon','[kg/m2]','ipoly') 
      end if

         !_____JL ADDITIONS___________________________________________!

      if (associated(cgrid%daily_pcpg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%daily_pcpg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DAILY_PCPG :11:hist:anal:opti:dail:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Daily precipitaion','[kg h20/m2/sec]','ipoly') 
      end if

      if (associated(cgrid%daily_evap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%daily_evap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DAILY_EVAP :11:hist:anal:opti:dail:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Daily evaporation','[kg h20/m2/sec]','ipoly') 
      end if

      if (associated(cgrid%daily_transp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%daily_transp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DAILY_transp :11:hist:anal:opti:dail:year') 
         call metadata_edio(nvar,igr,'Poly Avg. Daily precipitaion','[kg h20/m2/sec]','ipoly') 
      end if

      if (associated(cgrid%SoilN)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%SoilN,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'SoilN :11:hist:anal:opti:dail:year') 
         call metadata_edio(nvar,igr,'SoilN','[kg N/m2 forest]','ipoly') 
      end if

      if (associated(cgrid%PlantN)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%PlantN,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'PlantN :11:hist:anal:opti:dail:year') 
         call metadata_edio(nvar,igr,'PlantN','[kg N/m2 forest]','ipoly') 
      end if

      if (associated(cgrid%ForestN)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%ForestN,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'ForestN :11:hist:anal:opti:dail:year') 
         call metadata_edio(nvar,igr,'ForestN','[kg N/m2 forest]','ipoly') 
      end if

      if (associated(cgrid%total_N_Fixation)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_N_Fixation,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'total_N_Fixation :11:hist:anal:opti:dail:year:mont') 
         call metadata_edio(nvar,igr,'total_N_Fixation','[kg N/m2 forest]','ipoly') 
      end if

      if (associated(cgrid%total_DON_loss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_DON_loss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'total_DON_loss :11:hist:anal:opti:dail:year:mont') 
         call metadata_edio(nvar,igr,'total_DON_loss','[kg N/m2 forest]','ipoly') 
      end if

      if (associated(cgrid%total_DIN_loss) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_DIN_loss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'total_DIN_loss :11:hist:anal:opti:dail:year:mont') 
         call metadata_edio(nvar,igr,'total_DIN_loss','[kg N/m2 forest]','ipoly') 
      end if

      if (associated(cgrid%total_Ngas_loss) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_Ngas_loss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'total_Ngas_loss :11:hist:anal:opti:dail:year:mont') 
         call metadata_edio(nvar,igr,'total_Ngas_loss','[kg N/m2 forest]','ipoly') 
      end if
      if (associated(cgrid%total_N_supply)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_N_supply,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'total_N_supply :11:hist:anal:opti:dail:year:mont') 
         call metadata_edio(nvar,igr,'total_N_supply','[kg N/m2 forest]','ipoly') 
      end if

      if (associated(cgrid%total_N_demand)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%total_N_demand,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'total_N_demand :11:hist:anal:opti:dail:year:mont') 
         call metadata_edio(nvar,igr,'total_N_demand','[kg N/m2 forest]','ipoly') 
      end if


      !if (associated(cgrid%mmean_nitrogen_supply)) then
      !   nvar=nvar+1
      !   call vtable_edio_r(npts,cgrid%mmean_nitrogen_supply,nvar,igr,init,cgrid%pyglob_id, &
      !       var_len,var_len_global,max_ptrs,'MMEAN_NITROGEN_SUPPLY :11:hist:mont:dcyc') 
      !   call metadata_edio(nvar,igr,'Polygon Average Monthly NPP sapwood','[kgC/m2/yr]','ipoly') 
      !end if

      !if (associated(cgrid%mmean_N_uptake_pot)) then
      !    nvar=nvar+1
      !   call vtable_edio_r(npts,cgrid%mmean_N_uptake_pot,nvar,igr,init,cgrid%pyglob_id, &
      !        var_len,var_len_global,max_ptrs,'MMEAN_N_UPTAKE_POT :11:hist:mont:dcyc') 
      !   call metadata_edio(nvar,igr,'Polygon Average Monthly NPP sapwood','[kgC/m2/yr]','ipoly') 
      !end if

         !_____JL ADDITIONS___________________________________________!




       !-------- TOTAL CARBON AND NITROGEN POOLS  ---------------
       ! Added by MCD for NCEAS/FACE intercomparison (Apr 7 2009)
      if (associated(cgrid%Cleaf)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Cleaf,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CLEAF :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Leaf Carbon','?','ipoly') 
      end if
      if (associated(cgrid%Croot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Croot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CROOT :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Fine Root Carbon','?','ipoly') 
      end if
      if (associated(cgrid%Cstore)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%cstore,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CSTORE :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Storage/TNC Carbon','?','ipoly') 
      end if
      if (associated(cgrid%Ccwd)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Ccwd,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CCWD :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Coarse Woody Debris Carbon','?','ipoly') 
      end if
      if (associated(cgrid%Nleaf)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Nleaf,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NLEAF :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Leaf Nitrogen','?','ipoly') 
      end if
      if (associated(cgrid%Ndead)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Ndead,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NDEAD :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Wood Nitrogen','?','ipoly') 
      end if
      if (associated(cgrid%Nroot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Nroot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NROOT :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Fine Root Nitrogen','?','ipoly') 
      end if
      if (associated(cgrid%Nstore)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Nstore,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NSTORE :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Storage Nitrogen','?','ipoly') 
      end if
      if (associated(cgrid%Ncwd)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Ncwd,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NCWD :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Leaf Carbon','?','ipoly') 
      end if

       !-------- TOTAL CARBON AND NITROGEN FLUX  ---------------
       ! Added by MCD for NCEAS/FACE intercomparison (Apr 7 2009)
      if (associated(cgrid%Cleaf_grow)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Cleaf_grow,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CLEAF_GROW :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Leaf Carbon Growth','?','ipoly') 
      end if
      if (associated(cgrid%Croot_grow)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Croot_grow,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CROOT_GROW :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Fine Root Carbon Growth','?','ipoly') 
      end if
      if (associated(cgrid%Cdead_grow)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Cdead_grow,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CDEAD_GROW :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Wood Carbon Growth','?','ipoly') 
      end if
      if (associated(cgrid%Cstore_grow)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Cstore_Grow,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CSTORE_GROW :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Storage Carbon growth','?','ipoly') 
      end if
      if (associated(cgrid%Cleaf_litter_flux)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Cleaf_litter_flux,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CLEAF_LITTER_FLUX :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Leaf Litter Carbon Flux','?','ipoly') 
      end if
      if (associated(cgrid%Croot_litter_flux)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Croot_litter_flux,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CROOT_LITTER_FLUX :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Fine Root Litter Carbon Flux','?','ipoly') 
      end if
      if (associated(cgrid%Ccwd_flux)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Ccwd_flux,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'CCWD_FLUX :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Coarse Woody Debris Carbon FLUX','?','ipoly') 
      end if
      if (associated(cgrid%Nleaf_grow)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Nleaf_grow,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NLEAF_GROW :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Leaf Nitrogen growth','?','ipoly') 
      end if
      if (associated(cgrid%Nroot_grow)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Nroot_grow,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NROOT_GROW :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Fine Root Nitrogen Growth','?','ipoly') 
      end if
      if (associated(cgrid%Ndead_grow)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Ndead_grow,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NDEAD_GROW :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Wood Nitrogen growth','?','ipoly') 
      end if
      if (associated(cgrid%Nstore_grow)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Nstore_grow,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NSTORE_GROW :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Storage Nitrogen Growth','?','ipoly') 
      end if
      if (associated(cgrid%Nleaf_litter_flux)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Nleaf_litter_flux,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NLEAF_LITTER_FLUX :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Leaf litter Nitrogen flux','?','ipoly') 
      end if
      if (associated(cgrid%Nroot_litter_flux)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Nroot_litter_flux,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NROOT_LITTER_FLUX :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. fine root litter Nitrogen flux','?','ipoly') 
      end if
      if (associated(cgrid%Ncwd_flux)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Ncwd_flux,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NCWD_FLUX :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. CWD Nitrogen flux','?','ipoly') 
      end if
      if (associated(cgrid%Nbiomass_uptake)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Nbiomass_uptake,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NBIOMASS_UPTAKE :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Nitrogen Uptake','?','ipoly') 
      end if
      if (associated(cgrid%Ngross_min)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Ngross_min,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NGROSS_MIN :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Gross Nitrogen mineralization','?','ipoly') 
      end if
      if (associated(cgrid%Nnet_min)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%Nnet_min,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'NNET_MIN :11:dail') 
         call metadata_edio(nvar,igr,'Poly Avg. Net Nitrogen mineralization','?','ipoly') 
      end if

      ! ----------------------------------------------
      
      if (associated(cgrid%avg_nir_beam)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_nir_beam,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_NIR_BEAM:11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Averaged Incident Near Infrared Beam Radiation','[W/m2]','ipoly') 
      end if
      
      if (associated(cgrid%avg_nir_diffuse)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_nir_diffuse,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_NIR_DIFFUSE :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Averaged Incident Near Infrared Diffuse Radiation','[W/m2]','ipoly') 
      end if
      
      if (associated(cgrid%avg_par_beam)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_par_beam,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_PAR_BEAM :11:hist:opti:anal') 
         call metadata_edio(nvar,igr,'Polygon Averaged Incident Beam Photosynthetically Active Radiation','[W/m2]','ipoly') 
      end if
      
      if (associated(cgrid%avg_par_diffuse)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_par_diffuse,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_PAR_DIFFUSE :11:hist:opti:anal') 
         call metadata_edio(nvar,igr,'Polygon Averaged Incident Diffuse Photosynthetically Active Radiation','[W/m2]','ipoly') 
      end if
      
      if (associated(cgrid%avg_atm_tmp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_atm_tmp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_ATM_TMP :11:hist:opti:anal') 
         call metadata_edio(nvar,igr,'Polygon Averaged Atmospheric Temperature at Reference Height','[K]','ipoly') 
      end if
      
      if (associated(cgrid%avg_atm_shv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_atm_shv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_ATM_SHV :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Averaged Atmospheric Specific Humidity at Reference Height','[kg/kg]','ipoly') 
      end if
      
      if (associated(cgrid%avg_rshort)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_rshort,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_RSHORT :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Total Incident Shortwave Radiation','[W/m2]','ipoly') 
      end if
      
      if (associated(cgrid%avg_rshort_diffuse)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_rshort_diffuse,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_RSHORT_DIFFUSE :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Diffuse Incident Shortwave Radiation','[W/m2]','ipoly') 
      end if
      
      if (associated(cgrid%avg_rlong)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_rlong,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_RLONG :11:hist:opti:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Incident Longwave Radiation','[W/m2]','ipoly') 
      end if
      
      if (associated(cgrid%avg_pcpg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_pcpg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_PCPG :11:hist:opti:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Total Precipitation Rate','[kg/m2/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_qpcpg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_qpcpg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_QPCPG:11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Precipitation Internal Energy Deposition Rate','[W/m2/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_dpcpg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_dpcpg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_DPCPG :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Total Precipitation Depth Rate ','[mm/m2/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_vels)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_vels,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_VELS :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Wind Magnitude (with instability correction)','[m/s]','ipoly') 
      end if
      
      if (associated(cgrid%avg_atm_prss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_atm_prss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_ATM_PRSS :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Atmospheric Pressure at Ref. Height','[Pa]','ipoly') 
      end if
      
      if (associated(cgrid%avg_exner)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_exner,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_EXNER :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Exner Correction','[????]','ipoly') 
      end if
      
      if (associated(cgrid%avg_geoht)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_geoht,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_GEOHT :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Geopotential of Met. Forcing Refernce Height','[m]','ipoly') 
      end if
      
      if (associated(cgrid%avg_atm_co2)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_atm_co2,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_ATM_CO2 :11:hist:opti:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Atmospheric CO2 Concentration at Ref. Height','[ppm]','ipoly') 
      end if
  
      if (associated(cgrid%avg_albedo)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_albedo,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_ALBEDO :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Surface Albedo','[W/W]','ipoly') 
      end if
      
      if (associated(cgrid%avg_albedo_beam)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_albedo_beam,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_ALBEDO_BEAM :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Surface direct Albedo','[W/W]','ipoly') 
      end if
      
      if (associated(cgrid%avg_albedo_diffuse)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_albedo_diffuse,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_ALBEDO_DIFFUSE :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Surface diffuse Albedo','[W/W]','ipoly') 
      end if
      
      if (associated(cgrid%avg_rlong_albedo)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_rlong_albedo,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_RLONG_ALBEDO :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Albedo for Longwave Radiation','[W/m2]','ipoly') 
      end if
          
      if (associated(cgrid%avg_rlongup)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_rlongup,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_RLONGUP :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Upwelling Longwave Radiation','[W/m2]','ipoly') 
      end if

      if (associated(cgrid%max_leaf_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%max_leaf_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MAX_LEAF_TEMP :11:anal') 
         call metadata_edio(nvar,igr,'Temp of the hottest cohort in the polygon','[K]','ipoly') 
      end if     

      if (associated(cgrid%min_leaf_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%min_leaf_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MIN_LEAF_TEMP :11:anal') 
         call metadata_edio(nvar,igr,'Temp of the coldest cohort in the polygon','[K]','ipoly') 
      end if

      if (associated(cgrid%max_wood_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%max_wood_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MAX_WOOD_TEMP :11:anal') 
         call metadata_edio(nvar,igr,'Temp of the hottest cohort in the polygon','[K]','ipoly') 
      end if     

      if (associated(cgrid%min_wood_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%min_wood_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MIN_WOOD_TEMP :11:anal') 
         call metadata_edio(nvar,igr,'Temp of the coldest cohort in the polygon','[K]','ipoly') 
      end if

      if (associated(cgrid%max_soil_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%max_soil_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MAX_SOIL_TEMP :11:anal') 
         call metadata_edio(nvar,igr,'Temp of the hottest soil layer in the polygon','[K]','ipoly') 
      end if    

      if (associated(cgrid%min_soil_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%min_soil_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MIN_SOIL_TEMP :11:anal') 
         call metadata_edio(nvar,igr,'Temp of the coldest soil layer in the polygon','[K]','ipoly') 
      end if    
      
      if (associated(cgrid%avg_leaf_energy)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_leaf_energy,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_LEAF_ENERGY :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Internal Energy of Vegetation','[J/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_leaf_hcap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_leaf_hcap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_LEAF_HCAP :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average of Vegetation heat capacity','[J/m2/K]','ipoly') 
      end if

      if (associated(cgrid%avg_leaf_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_leaf_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_LEAF_TEMP :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Temperature of Vegetation','[K]','ipoly') 
      end if

      if (associated(cgrid%avg_leaf_fliq)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_leaf_fliq,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_LEAF_FLIQ :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Liquid fraction of Vegetation Sfc Water','[--]','ipoly') 
      end if
      
      if (associated(cgrid%avg_leaf_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_leaf_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_LEAF_WATER :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Resident Leaf Surface Water','[kg/m2]','ipoly') 
      end if
      
      if (associated(cgrid%avg_wood_energy)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_wood_energy,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_WOOD_ENERGY :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Internal Energy of wood','[J/m2]','ipoly') 
      end if

      if (associated(cgrid%avg_wood_hcap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_wood_hcap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_WOOD_HCAP :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average of wood heat capacity','[J/m2/K]','ipoly') 
      end if

      if (associated(cgrid%avg_wood_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_wood_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_WOOD_TEMP :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Wood of Vegetation','[K]','ipoly') 
      end if

      if (associated(cgrid%avg_wood_fliq)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_wood_fliq,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_WOOD_FLIQ :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Liquid fraction of Vegetation Sfc Water','[--]','ipoly') 
      end if
      
      if (associated(cgrid%avg_wood_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_wood_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_WOOD_WATER :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Resident Wood Surface Water','[kg/m2]','ipoly') 
      end if
      
      if (associated(cgrid%avg_can_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_can_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_CAN_TEMP :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Temperature of Canopy Air Space','[K]','ipoly') 
      end if
      
      if (associated(cgrid%avg_can_shv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_can_shv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_CAN_SHV :11:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Specific Humidity of Canopy Air','[kg/kg]','NA') 
      end if
      
      if (associated(cgrid%avg_can_co2)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_can_co2,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_CAN_CO2 :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average CO2 mixing ratio of Canopy Air','[umol/mol]','NA') 
      end if
      
      if (associated(cgrid%avg_can_rhos)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_can_rhos,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_CAN_RHOS :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Density of Canopy Air','[kg/m3]','NA') 
      end if
      
      if (associated(cgrid%avg_can_prss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_can_prss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_CAN_PRSS :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Canopy Air Pressure','[Pa]','NA') 
      end if
      
      if (associated(cgrid%avg_can_theta)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_can_theta,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_CAN_THETA :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Canopy Air Potential temperature','[K]','NA') 
      end if
      
      if (associated(cgrid%avg_can_theiv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_can_theiv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_CAN_THEIV :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Canopy Air ice-vapour equiv. pot. temp.','[K]','NA') 
      end if
      
      if (associated(cgrid%avg_can_depth)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_can_depth,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_CAN_DEPTH :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Canopy height','[m]','NA') 
      end if
      
      if (associated(cgrid%avg_soil_wetness)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_soil_wetness,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SOIL_WETNESS :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Soil Wetness RELATIVE TO WILTING POINT','[m3/m3]','ipoly') !relative to wilting point
      end if
      
      if (associated(cgrid%avg_skin_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_skin_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SKIN_TEMP :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Temperature of all surfaces','[K]','ipoly') 
      end if
      
      if (associated(cgrid%avg_available_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_available_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_AVAILABLE_WATER :11:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Available water','[K]','ipoly') 
      end if
      
      ! Daily and monthly variables. Note that all these variables need to be stored at the
      ! history file, because the averaging can be resumed...
      
      if(associated(cgrid%dmean_gpp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_gpp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_GPP :11:hist:dail') 
         call metadata_edio(nvar,igr,'Polygon Average Daily Integrated Gross Primary Productivity','[kgC/m2/yr]','ipoly') 
      end if

      if (associated(cgrid%dmean_nppleaf)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_nppleaf,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPLEAF :11:hist:dail') 
         call metadata_edio(nvar,igr,'Polygon Average Daily NPP leaf','[kgC/m2/yr]','ipoly') 
      end if

      if (associated(cgrid%dmean_nppfroot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_nppfroot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPFROOT :11:hist:dail') 
         call metadata_edio(nvar,igr,'Polygon Average Daily NPP froot','[kgC/m2/yr]','ipoly') 
      end if

            
      if (associated(cgrid%dmean_nppsapwood)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_nppsapwood,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPSAPWOOD :11:hist:dail') 
         call metadata_edio(nvar,igr,'Polygon Average Daily NPP sapwood','[kgC/m2/yr]','ipoly') 
      end if

      if (associated(cgrid%dmean_nppcroot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_nppcroot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPCROOT :11:hist:dail') 
         call metadata_edio(nvar,igr,'Polygon Average Daily NPP croot','[kgC/m2/yr]','ipoly') 
      end if

      if (associated(cgrid%dmean_nppseeds)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_nppseeds,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPSEEDS :11:hist:dail') 
         call metadata_edio(nvar,igr,'Polygon Average Daily NPP seeds','[kgC/m2/yr]','ipoly') 
      end if

      if (associated(cgrid%dmean_nppwood)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_nppwood,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPWOOD :11:hist:dail') 
         call metadata_edio(nvar,igr,'Polygon Average Daily NPP wood','[kgC/m2/yr]','ipoly') 
      end if
      
      if (associated(cgrid%dmean_nppdaily)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_nppdaily,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPDAILY :11:hist:dail') 
         call metadata_edio(nvar,igr,'Polygon Average Daily NPP daily','[kgC/m2/yr]','ipoly') 
      end if

      
      if(associated(cgrid%dmean_nee)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_nee,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NEE :11:hist:dail') 
         call metadata_edio(nvar,igr,'Polygon Average Daily Integrated Net Ecosystem Exchange','[kgC/m2/yr]','ipoly') 
      end if
      
      
      if(associated(cgrid%dmean_pcpg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_pcpg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_PCPG :11:hist:dail') 
         call metadata_edio(nvar,igr,'total daily precipitation','[kg/m2/day]','ipoly') 
      end if

      if(associated(cgrid%dmean_runoff)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_runoff,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_RUNOFF :11:hist:dail') 
         call metadata_edio(nvar,igr,'total daily surface runoff','[kg/m2/day]','ipoly') 
      end if

      if(associated(cgrid%dmean_drainage)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_drainage,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_DRAINAGE :11:hist:dail') 
         call metadata_edio(nvar,igr,'total daily water flux through lower soil layer','[kg/m2/day]','ipoly') 
      end if

      if(associated(cgrid%dmean_vapor_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_vapor_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_VAPOR_AC :11:hist:dail') 
         call metadata_edio(nvar,igr,'total daily vapor flux atm->canopy','[kg/m2/day]','ipoly') 
      end if


      if(associated(cgrid%dmean_vapor_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_vapor_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_VAPOR_GC :11:hist:dail') 
         call metadata_edio(nvar,igr,'total daily vapor flux ground->canopy','[kg/m2/day]','ipoly') 
      end if


      if(associated(cgrid%dmean_vapor_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_vapor_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_VAPOR_LC :11:hist:dail') 
         call metadata_edio(nvar,igr,'total daily vapor flux leaf->canopy','[kg/m2/day]','ipoly') 
      end if

      if(associated(cgrid%dmean_vapor_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_vapor_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_VAPOR_WC :11:hist:dail') 
         call metadata_edio(nvar,igr,'total daily vapor flux wood->canopy','[kg/m2/day]','ipoly') 
      end if

      if(associated(cgrid%dmean_evap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_evap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_EVAP :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean (leaf+soil) evaporation','[kg/m2/s]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_transp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_transp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_TRANSP :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_sensible_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_sensible_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_SENSIBLE_LC :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_sensible_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_sensible_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_SENSIBLE_WC :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_sensible_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_sensible_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_SENSIBLE_GC :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_sensible_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_sensible_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_SENSIBLE_AC :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_plresp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_plresp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_PLRESP :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_rh)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_rh,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_RH :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_leaf_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_leaf_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_LEAF_RESP :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_root_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_root_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ROOT_RESP :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_growth_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_growth_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_GROWTH_RESP :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_storage_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_storage_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_STORAGE_RESP :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_vleaf_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_vleaf_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_VLEAF_RESP :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_nep)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_nep,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NEP :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%dmean_fs_open)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_fs_open,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_FS_OPEN :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_fsw)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_fsw,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_FSW :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_fsn)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_fsn,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_FSN :11:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_can_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_can_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_CAN_TEMP :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean canopy temperature','[K]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_can_shv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_can_shv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_CAN_SHV :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean canopy specific humidity','[kg/kg]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_can_co2)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_can_co2,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_CAN_CO2 :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean canopy CO2 mixing ratio','[umol/mol]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_can_rhos)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_can_rhos,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_CAN_RHOS :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean canopy air density','[kg/m3]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_can_prss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_can_prss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_CAN_PRSS :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean canopy air pressure','[Pa]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_can_theta)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_can_theta,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_CAN_THETA :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean canopy air potential temperature','[K]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_can_theiv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_can_theiv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_CAN_THEIV :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean canopy air theta_Eiv','[K]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_gnd_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_gnd_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_GND_TEMP :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of ground temperature','[K]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_gnd_shv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_gnd_shv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_GND_SHV :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of ground specific humidity','[kg/kg]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_leaf_energy)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_leaf_energy,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_LEAF_ENERGY :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean vegetation internal energy','[J/m2]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_leaf_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_leaf_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_LEAF_WATER :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean vegetation surface water','[kg/m2]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_leaf_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_leaf_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_LEAF_TEMP :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean vegetation temperature','[K]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_leaf_hcap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_leaf_hcap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_LEAF_HCAP :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean vegetation heat capacity','[J/m2/K]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_wood_energy)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_wood_energy,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_WOOD_ENERGY :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean wood internal energy','[J/m2]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_wood_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_wood_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_WOOD_WATER :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean wood surface water','[kg/m2]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_wood_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_wood_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_WOOD_TEMP :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean wood temperature','[K]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_wood_hcap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_wood_hcap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_WOOD_HCAP :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean vegetation heat capacity','[J/m2/K]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_atm_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_atm_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ATM_TEMP :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean air temperature','[K]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_atm_shv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_atm_shv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ATM_SHV :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean air specific humidity','[kg/kg]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_atm_co2)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_atm_co2,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ATM_CO2 :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean air specific humidity','[kg/kg]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_atm_prss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_atm_prss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ATM_PRSS :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean air pressure','[ Pa]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_atm_vels)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_atm_vels,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ATM_VELS :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean wind speed','[m/s]','ipoly') 
      end if

      if(associated(cgrid%dmean_rshort)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_rshort,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_RSHORT :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean shortwave radiation','[w/m2]','ipoly') 
      end if

      if(associated(cgrid%dmean_rshort_diff)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_rshort_diff,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_RSHORT_DIFF :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean diffuse shortwave radiation','[w/m2]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_rlong)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_rlong,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_RLONG :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean longwave radiation','[w/m2]','ipoly') 
      end if

      if(associated(cgrid%dmean_rshort_gnd)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_rshort_gnd,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_RSHORT_GND :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean ground abs. shortwave radiation','[w/m2]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_rlong_gnd)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_rlong_gnd,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_RLONG_GND :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean ground abs. longwave radiation','[w/m2]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_albedo)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_albedo,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ALBEDO :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean albedo','[---]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_albedo_beam)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_albedo_beam,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ALBEDO_BEAM :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean direct albedo','[---]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_albedo_diffuse)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_albedo_diffuse,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ALBEDO_DIFFUSE :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean diffuse albedo','[---]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_rlong_albedo)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_rlong_albedo,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_RLONG_ALBEDO :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean longwave albedo','[---]','ipoly') 
      end if
      
      if(associated(cgrid%dmean_rlongup)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_rlongup,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_RLONGUP :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean longwave emission from ground','[---]','ipoly') 
      end if

      if(associated(cgrid%dmean_co2_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_co2_residual,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_CO2_RESIDUAL :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of residual canopy CO2 ','[umol/m2/s]','ipoly') 
      end if

      if(associated(cgrid%dmean_water_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_water_residual,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_WATER_RESIDUAL :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of residual water ','[kg/m2/s]','ipoly') 
      end if

      if(associated(cgrid%dmean_energy_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_energy_residual,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ENERGY_RESIDUAL :11:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of residual water ','[J/m2/s]','ipoly') 
      end if

      if(associated(cgrid%mmean_gpp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_gpp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_GPP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cgrid%mmean_nppleaf)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_nppleaf,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPLEAF :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Polygon Average Monthly NPP leaf','[kgC/m2/yr]','ipoly') 
      end if

      if (associated(cgrid%mmean_nppfroot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_nppfroot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPFROOT :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Polygon Average Monthly NPP froot','[kgC/m2/yr]','ipoly') 
      end if
            
      if (associated(cgrid%mmean_nppsapwood)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_nppsapwood,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPSAPWOOD :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Polygon Average Monthly NPP sapwood','[kgC/m2/yr]','ipoly') 
      end if

      if (associated(cgrid%mmean_nppcroot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_nppcroot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPCROOT :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Polygon Average Monthly NPP croot','[kgC/m2/yr]','ipoly') 
      end if

      if (associated(cgrid%mmean_nppseeds)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_nppseeds,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPSEEDS :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Polygon Average Monthly NPP seeds','[kgC/m2/yr]','ipoly') 
      end if

      if (associated(cgrid%mmean_nppwood)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_nppwood,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPWOOD :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Polygon Average Monthly NPP wood','[kgC/m2/yr]','ipoly') 
      end if
      
      if (associated(cgrid%mmean_nppdaily)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_nppdaily,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPDAILY :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Polygon Average Monthly NPP daily','[kgC/m2/yr]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_nee)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_nee,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NEE :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%mmean_evap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_evap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_EVAP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly Mean (leaf+soil) Evaporation Rate','[kg/m2/s]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_transp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_transp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_TRANSP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmean_sensible_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_sensible_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_SENSIBLE_AC :11:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if

      if(associated(cgrid%mmean_sensible_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_sensible_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_SENSIBLE_GC :11:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if

      if(associated(cgrid%mmean_sensible_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_sensible_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_SENSIBLE_LC :11:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if

      if(associated(cgrid%mmean_sensible_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_sensible_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_SENSIBLE_WC :11:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if

      if(associated(cgrid%mmean_vapor_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_vapor_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_VAPOR_AC :11:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if

      if(associated(cgrid%mmean_vapor_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_vapor_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_VAPOR_GC :11:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if

      if(associated(cgrid%mmean_vapor_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_vapor_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_VAPOR_LC :11:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if

      if(associated(cgrid%mmean_vapor_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_vapor_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_VAPOR_WC :11:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if
      
      if(associated(cgrid%mmean_nep)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_nep,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NEP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmean_fs_open)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_fs_open,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_FS_OPEN :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%mmean_fsw)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_fsw,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_FSW :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%mmean_fsn)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_fsn,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_FSN :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmean_plresp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_plresp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_PLRESP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%mmean_rh)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_rh,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_RH :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%mmean_leaf_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_leaf_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_LEAF_RESP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%mmean_root_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_root_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ROOT_RESP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%mmean_growth_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_growth_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_GROWTH_RESP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%mmean_storage_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_storage_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_STORAGE_RESP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%mmean_vleaf_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_vleaf_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_VLEAF_RESP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmean_can_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_can_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_CAN_TEMP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean canopy temperature','[K]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_can_shv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_can_shv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_CAN_SHV :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean canopy specific humidity','[kg/kg]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_can_co2)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_can_co2,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_CAN_CO2 :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean canopy CO2 mixing ratio','[umol/mol]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_can_rhos)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_can_rhos,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_CAN_RHOS :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean canopy air density','[kg/m3]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_can_prss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_can_prss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_CAN_PRSS :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean canopy air pressure','[Pa]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_can_theta)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_can_theta,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_CAN_THETA :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean canopy air potential temperature','[K]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_can_theiv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_can_theiv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_CAN_THEIV :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean canopy air theta_Eiv','[K]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_gnd_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_gnd_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_GND_TEMP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean ground temperature','[K]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_gnd_shv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_gnd_shv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_GND_SHV :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean ground specific humidity','[kg/kg]','ipoly') 
      end if

      if(associated(cgrid%mmean_leaf_energy)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_leaf_energy,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_LEAF_ENERGY :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean leaf internal energy','[J/m2]','ipoly') 
      end if

      if(associated(cgrid%mmean_leaf_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_leaf_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_LEAF_TEMP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean leaf temperature','[K]','ipoly') 
      end if

      if(associated(cgrid%mmean_leaf_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_leaf_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_LEAF_WATER :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean leaf water ','[kg/m2]','ipoly') 
      end if

      if(associated(cgrid%mmean_leaf_hcap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_leaf_hcap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_LEAF_HCAP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean leaf heat capacity','[J/m2/K]','ipoly') 
      end if

      if(associated(cgrid%mmean_wood_energy)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_wood_energy,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_WOOD_ENERGY :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean wood internal energy','[J/m2]','ipoly') 
      end if

      if(associated(cgrid%mmean_wood_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_wood_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_WOOD_TEMP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean wood temperature','[K]','ipoly') 
      end if

      if(associated(cgrid%mmean_wood_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_wood_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_WOOD_WATER :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean wood water ','[kg/m2]','ipoly') 
      end if

      if(associated(cgrid%mmean_wood_hcap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_wood_hcap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_WOOD_HCAP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean wood heat capacity','[J/m2/K]','ipoly') 
      end if

      if(associated(cgrid%mmean_rshort)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_rshort,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_RSHORT :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean downwelling solar radiation','[w/m2]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_rshort_diff)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_rshort_diff,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_RSHORT_DIFF :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean incoming diffuse solar radiation','[w/m2]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_rlong)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_rlong,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_RLONG :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean downwelling longwave radiation','[w/m2]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_rshort_gnd)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_rshort_gnd,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_RSHORT_GND :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean ground abs. solar radiation','[w/m2]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_rlong_gnd)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_rlong_gnd,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_RLONG_GND :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean ground abs. longwave radiation','[w/m2]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_albedo)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_albedo,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ALBEDO :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean albedo','[---]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_albedo_beam)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_albedo_beam,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ALBEDO_BEAM :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean direct albedo','[---]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_albedo_diffuse)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_albedo_diffuse,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ALBEDO_DIFFUSE :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean diffuse albedo','[---]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_rlong_albedo)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_rlong_albedo,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_RLONG_ALBEDO :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean longwave albedo','[---]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_rlongup)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_rlongup,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_RLONGUP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean longwave emission from ground','[---]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_atm_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_atm_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ATM_TEMP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean air temperature','[K]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_atm_shv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_atm_shv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ATM_SHV :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean air specific humidity','[kg/kg]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_atm_co2)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_atm_co2,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ATM_CO2 :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean air specific humidity','[kg/kg]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_atm_prss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_atm_prss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ATM_PRSS :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean air pressure','[ Pa]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_atm_vels)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_atm_vels,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ATM_VELS :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean wind speed','[m/s]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_pcpg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_pcpg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_PCPG :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean precipitation rate','[kg/m2/s]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_runoff)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_runoff,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_RUNOFF :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean runoff','[kg/m2/s]','ipoly') 
      end if
      
      if(associated(cgrid%mmean_drainage)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_drainage,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_DRAINAGE :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean drainage','[kg/m2/day]','ipoly') 
      end if

      if(associated(cgrid%mmean_co2_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_co2_residual,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_CO2_RESIDUAL :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of residual canopy CO2 ','[umol/m2/s]','ipoly') 
      end if

      if(associated(cgrid%mmean_water_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_water_residual,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_WATER_RESIDUAL :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of residual water ','[kg/m2/s]','ipoly') 
      end if

      if(associated(cgrid%mmean_energy_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_energy_residual,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ENERGY_RESIDUAL :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of residual energy ','[J/m2/s]','ipoly') 
      end if



      if(associated(cgrid%mmsqu_gpp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_gpp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_GPP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_leaf_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_leaf_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_LEAF_RESP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_root_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_root_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_ROOT_RESP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_plresp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_plresp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_PLRESP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_nee)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_nee,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_NEE :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_rh)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_rh,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_RH :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_sensible_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_sensible_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_SENSIBLE_AC :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_sensible_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_sensible_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_SENSIBLE_LC :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_sensible_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_sensible_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_SENSIBLE_WC :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_sensible_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_sensible_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_SENSIBLE_GC :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_evap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_evap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_EVAP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_transp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_transp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_TRANSP :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_vapor_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_vapor_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_VAPOR_AC :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_vapor_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_vapor_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_VAPOR_LC :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_vapor_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_vapor_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_VAPOR_WC :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmsqu_vapor_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmsqu_vapor_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMSQU_VAPOR_GC :11:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!

      return     
   end subroutine filltab_edtype_p11
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have two dimensions (ndcycle,npolygons) and are real (type -11).        !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_m11(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_r  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!




      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 2-D block, with diurnal cycle.  All variables must have the number !
      ! of points defined by npts.                                                         !
      !------------------------------------------------------------------------------------!
      npts = cgrid%npolygons * ndcycle
      
      if(associated(cgrid%qmean_pcpg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_pcpg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_PCPG :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%qmean_runoff)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_runoff,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_RUNOFF :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%qmean_drainage)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_drainage,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_DRAINAGE :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%qmean_evap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_evap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_EVAP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%qmean_transp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_transp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_TRANSP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%qmean_vapor_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_vapor_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_VAPOR_AC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%qmean_vapor_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_vapor_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_VAPOR_GC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_vapor_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_vapor_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_VAPOR_LC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_vapor_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_vapor_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_VAPOR_WC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_sensible_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_sensible_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_SENSIBLE_AC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_sensible_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_sensible_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_SENSIBLE_GC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_sensible_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_sensible_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_SENSIBLE_LC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_sensible_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_sensible_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_SENSIBLE_WC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_nee)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_nee,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_NEE :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_gpp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_gpp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_GPP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_nep)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_nep,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_NEP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_plresp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_plresp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_PLRESP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_rh)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_rh,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_RH :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_leaf_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_leaf_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_LEAF_RESP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_root_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_root_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_ROOT_RESP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_fs_open)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_fs_open,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_FS_OPEN :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_fsw)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_fsw,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_FSW :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_fsn)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_fsn,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_FSN :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_can_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_can_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_CAN_TEMP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_can_shv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_can_shv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_CAN_SHV :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_can_co2)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_can_co2,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_CAN_CO2 :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_can_rhos)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_can_rhos,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_CAN_RHOS :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_can_prss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_can_prss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_CAN_PRSS :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_can_theta)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_can_theta,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_CAN_THETA :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_can_theiv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_can_theiv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_CAN_THEIV :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_gnd_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_gnd_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_GND_TEMP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_gnd_shv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_gnd_shv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_GND_SHV :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_leaf_energy)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_leaf_energy,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_LEAF_ENERGY :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_leaf_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_leaf_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_LEAF_WATER :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_leaf_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_leaf_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_LEAF_TEMP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_leaf_hcap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_leaf_hcap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_LEAF_HCAP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_wood_energy)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_wood_energy,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_WOOD_ENERGY :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_wood_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_wood_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_WOOD_WATER :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_wood_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_wood_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_WOOD_TEMP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_wood_hcap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_wood_hcap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_WOOD_HCAP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_atm_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_atm_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_ATM_TEMP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_rshort)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_rshort,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_RSHORT :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_rshort_diff)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_rshort_diff,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_RSHORT_DIFF :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_rlong)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_rlong,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_RLONG :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_rshort_gnd)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_rshort_gnd,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_RSHORT_GND :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_rlong_gnd)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_rlong_gnd,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_RLONG_GND :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%qmean_albedo)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_albedo,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_ALBEDO :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%qmean_albedo_beam)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_albedo_beam,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_ALBEDO_BEAM :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%qmean_albedo_diffuse)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_albedo_diffuse,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_ALBEDO_DIFFUSE :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%qmean_rlong_albedo)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_rlong_albedo,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_RLONG_ALBEDO :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%qmean_rlongup)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_rlongup,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_RLONGUP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_atm_shv)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_atm_shv,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_ATM_SHV :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_atm_co2)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_atm_co2,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_ATM_CO2 :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_atm_prss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_atm_prss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_ATM_PRSS :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_atm_vels)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_atm_vels,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_ATM_VELS :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_gpp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_gpp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_GPP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_leaf_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_leaf_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_LEAF_RESP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_root_resp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_root_resp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_ROOT_RESP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_plresp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_plresp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_PLRESP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_nee)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_nee,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_NEE :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_nep)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_nep,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_NEP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_rh)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_rh,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_RH :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_sensible_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_sensible_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_SENSIBLE_AC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_sensible_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_sensible_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_SENSIBLE_LC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_sensible_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_sensible_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_SENSIBLE_WC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_sensible_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_sensible_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_SENSIBLE_GC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_evap)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_evap,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_EVAP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_transp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_transp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_TRANSP :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_vapor_ac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_vapor_ac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_VAPOR_AC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_vapor_lc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_vapor_lc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_VAPOR_LC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_vapor_wc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_vapor_wc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_VAPOR_WC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmsqu_vapor_gc)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmsqu_vapor_gc,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMSQU_VAPOR_GC :-11:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!


      return     
   end subroutine filltab_edtype_m11
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have two dimensions (nzg,npolygons) and are integer (type 120).         !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_p120(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_i  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 2-D block, with soil layers.  All variables must have the number   !
      ! of points defined by npts.                                                         !
      !------------------------------------------------------------------------------------!
      npts = cgrid%npolygons * nzg

      if (associated(cgrid%ntext_soil)) then
         nvar=nvar+1
         call vtable_edio_i(npts,cgrid%ntext_soil,nvar,igr,init,cgrid%pyglob_id            &
                           ,var_len,var_len_global,max_ptrs                                &
                           ,'NTEXT_SOIL :120:hist:anal:dail:mont:dcyc:year')  
         call metadata_edio(nvar,igr,'Polygon mode soil class','OGE2 Class','ipoly-ngz')

      end if


      return     
   end subroutine filltab_edtype_p120
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have two dimensions (ndcycle,npolygons) and are real (type 12).         !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_p12(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_r  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 2-D block, with soil layers.  All variables must have the number   !
      ! of points defined by npts.                                                         !
      !------------------------------------------------------------------------------------!
      npts = cgrid%npolygons * nzg

      
      if (associated(cgrid%avg_smoist_gg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_smoist_gg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SMOIST_GG :12:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged soil moisture flux,layer nzg is flux with CAS','[kg/m2/s]','ipoly-nzg') 
      end if
      
      if (associated(cgrid%avg_transloss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_transloss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_TRANSLOSS :12:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged soil moisture sink to transpiration','[kg/m2/s]','ipoly-nzg') 
      end if

      if (associated(cgrid%aux_s)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%aux_s,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AUX_S :12:hist:anal') 
         call metadata_edio(nvar,igr,'Soil layer discretized, auxillary variable, see rk4_derivs.f90','[user-defined]','ipoly-nzg') 
      end if

      
      if (associated(cgrid%avg_sensible_gg)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_sensible_gg,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SENSIBLE_GG :12:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon averaged soil sensible heat flux,layer nzg is flux with CAS ','[W/m2]','ipoly-nzg') 
      end if

      if (associated(cgrid%avg_soil_energy)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_soil_energy,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SOIL_ENERGY :12:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Volumetric Soil Water','[m/m]','ipoly - nzg') 
      end if
      
      if (associated(cgrid%avg_soil_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_soil_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SOIL_WATER :12:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Volumetric Soil Water','[m/m]','ipoly - nzg') 
      end if
      
      if (associated(cgrid%avg_soil_mstpot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_soil_mstpot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SOIL_MSTPOT :12:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Soil Moisture Potential','[m]','ipoly - nzg') 
      end if
      
      if (associated(cgrid%avg_soil_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_soil_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SOIL_TEMP :12:hist:anal:opti') 
         call metadata_edio(nvar,igr,'Polygon Average Soil Temperature','[K]','ipoly - nzg') 
      end if

      if (associated(cgrid%avg_soil_fracliq)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_soil_fracliq,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_SOIL_FRACLIQ :12:hist:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Soil Fraction Liquid','[proportion]','ipoly - nzg') 
      end if
      

      if (associated(cgrid%avg_soil_rootfrac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_soil_rootfrac,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'POLY_SOIL_ROOTFRAC :12:hist:mont:year') 
         call metadata_edio(nvar,igr,'Polygon Average Root Fraction','[pdf]','ipoly - nzg') 
      end if


      if(associated(cgrid%dmean_soil_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_soil_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_SOIL_TEMP :12:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_soil_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_soil_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_SOIL_WATER :12:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_soil_mstpot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_soil_mstpot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_SOIL_MSTPOT :12:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%dmean_transloss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_transloss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_TRANSLOSS :12:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%mmean_soil_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_soil_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_SOIL_TEMP :12:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if

      if(associated(cgrid%mmean_soil_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_soil_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_SOIL_WATER :12:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if

      if(associated(cgrid%mmean_soil_mstpot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_soil_mstpot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_SOIL_MSTPOT :12:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if
      
      if(associated(cgrid%mmean_transloss)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_transloss,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_TRANSLOSS :12:hist:mont:dcyc')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if
      
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!



      return     
   end subroutine filltab_edtype_p12
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have three dimensions (nzg,ndcycle,npolygons) and are real (type -12).  !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_m12(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_r  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 3-D block, with soil depth class and diurnal cycle.  All vari-     !
      ! ables must have the number of points defined by npts.                              !
      !------------------------------------------------------------------------------------!
      npts = cgrid%npolygons *  nzg * ndcycle

      if(associated(cgrid%qmean_soil_temp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_soil_temp,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_SOIL_TEMP :-12:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_soil_water)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_soil_water,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_SOIL_WATER :-12:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(cgrid%qmean_soil_mstpot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%qmean_soil_mstpot,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'QMEAN_SOIL_MSTPOT :-12:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!


      return     
   end subroutine filltab_edtype_m12
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have two dimensions (n_pft,npolygons) and are real (type 14).           !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_p14(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_r  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 2-D block, with PFT class.  All variables must have the number of  !
      ! points defined by npts.                                                            !
      !------------------------------------------------------------------------------------!
      npts = cgrid%npolygons *  n_pft


      if(associated(cgrid%lai_pft)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%lai_pft,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'LAI_PFT :14:hist:anal:dail') 
         call metadata_edio(nvar,igr,'Leaf Area Index','[m2/m2]','NA') 
      end if
      
      if(associated(cgrid%wpa_pft)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%wpa_pft,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'WPA_PFT :14:hist:anal:dail') 
         call metadata_edio(nvar,igr,'Wood Projected Area','[m2/m2]','NA') 
      end if
      
      if(associated(cgrid%wai_pft)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%wai_pft,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'WAI_PFT :14:hist:anal:dail') 
         call metadata_edio(nvar,igr,'Wood Area Index','[m2/m2]','NA') 
      end if
      
      if(associated(cgrid%mmean_lai_pft)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_lai_pft,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_LAI_PFT :14:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%mmean_wpa_pft)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_wpa_pft,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_WPA_PFT :14:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%mmean_wai_pft)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_wai_pft,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_WAI_PFT :14:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%bseeds_pft)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%bseeds_pft,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'BSEEDS_PFT :14:hist:dail:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if(associated(cgrid%agb_pft)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%agb_pft,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AGB_PFT :14:hist:dail:mont:dcyc') 
         call metadata_edio(nvar,igr,'Above-ground biomass by PFT','[kgC/m2]','NA') 
      end if
      
      if(associated(cgrid%ba_pft)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%ba_pft,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'BA_PFT :14:hist:dail:mont:dcyc') 
         call metadata_edio(nvar,igr,'Basal area by PFT','[cm2/m2]','NA') 
      end if

      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!


      return     
   end subroutine filltab_edtype_p14
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have two dimensions (n_dbh,npolygons) and are real (type 16).           !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_p16(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_r  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 2-D block, with size (DBH) class.  All variables must have the     !
      ! number of points defined by npts.                                                  !
      !------------------------------------------------------------------------------------!
      npts = cgrid%npolygons *  n_dbh

      if(associated(cgrid%dmean_gpp_dbh)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%dmean_gpp_dbh,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_GPP_DBH :16:hist:dail') 
         call metadata_edio(nvar,igr,'Polygon Averaged by DBH, Daily Integrated Gross Primary Production' &
              ,'[kgC/m2/yr]','ipoly - ndbh') 
      end if

      if(associated(cgrid%mmean_gpp_dbh)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%mmean_gpp_dbh,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_GPP_DBH :16:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!

      return     
   end subroutine filltab_edtype_p16
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have two dimensions (13 months,npolygons) and are real (type 19).       !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_p19(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_r  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!

      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 2-D block, with 13 months.  All variables must have the            !
      ! number of points defined by npts.                                                  !
      !------------------------------------------------------------------------------------!
      npts = cgrid%npolygons * 13

      if(associated(cgrid%workload)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%workload,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'WORKLOAD :19:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Disturbance Rates','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!


      return     
   end subroutine filltab_edtype_p19
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have three dimensions (n_dbh,n_pft,npolygons) and are real (type 146).  !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_p146(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_r  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 3-D block, with PFT and size class.  All variables must have the   !
      ! number of points defined by npts.                                                  !
      !------------------------------------------------------------------------------------!
      npts = cgrid%npolygons * n_pft * n_dbh

      if (associated(cgrid%basal_area)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%basal_area,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'BASAL_AREA :146:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'Polygon basal area profile','[cm2/m2]','ipoly - n_dbh - n_pft')
      end if
      
      if (associated(cgrid%agb)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%agb,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AGB :146:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'Polygon above ground biomass profile','[kgC/m2]','ipoly - n_dbh - n_pft')
      end if
      
      if (associated(cgrid%pldens)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%pldens,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'PLDENS :146:hist:anal:dail:dcyc:mont:year') 
         call metadata_edio(nvar,igr,'Polygon plant density profile','[#/m2]','ipoly - n_dbh - n_pft')
      end if
      
      if (associated(cgrid%bseeds)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%bseeds,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'BSEEDS :146:hist:anal:dail:dcyc:mont:year') 
         call metadata_edio(nvar,igr,'Polygon seed biomass','[kgC/m2]','ipoly - n_dbh - n_pft')
      end if
      
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!


      return     
   end subroutine filltab_edtype_p146
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have three dimensions (3,4,npolygons) and are real (type 199).          !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_p199(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_r  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 3-D block, with LAI and 4 vars.  All variables must have the       !
      ! number of points defined by npts.                                                  !
      !------------------------------------------------------------------------------------!
      npts = cgrid%npolygons * 3 * 4

      if (associated(cgrid%avg_lai_ebalvars)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%avg_lai_ebalvars,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'AVG_LAI_EBALVARS :199:anal') 
         call metadata_edio(nvar,igr,'Polygon Average Energy Balance Variables','[variable]','ipoly - 4 - 3') 
      end if
      
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!



      return     
   end subroutine filltab_edtype_p199
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This routine will fill the pointer table with the polygon-level variables         !
   ! (edtype) that have three dimensions (n_dist_types,n_dist_types,npolygons) and are     !
   ! real (type 155).                                                                      !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_edtype_p155(cgrid,igr,init,var_len,var_len_global,max_ptrs,nvar)
      use ed_var_tables, only : vtable_edio_r  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(edtype), target        :: cgrid
      integer     , intent(in)    :: init
      integer     , intent(in)    :: igr
      integer     , intent(in)    :: var_len
      integer     , intent(in)    :: max_ptrs
      integer     , intent(in)    :: var_len_global
      integer     , intent(inout) :: nvar
      !----- Local variables. -------------------------------------------------------------!
      integer                     :: npts
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 3-D block, with disturbance transitions.  All variables must have  !
      ! the number of points defined by npts.                                              !
      !------------------------------------------------------------------------------------!
      npts = cgrid%npolygons * n_dist_types * n_dist_types

      if(associated(cgrid%disturbance_rates)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cgrid%disturbance_rates,nvar,igr,init,cgrid%pyglob_id, &
              var_len,var_len_global,max_ptrs,'DISTURBANCE_RATES :155:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Disturbance Rates','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!


      return     
   end subroutine filltab_edtype_p155
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine filltab_polygontype(igr,ipy,init)

      use ed_var_tables, only : vtable_edio_r & ! sub-routine
                              , vtable_edio_i & ! sub-routine
                              , metadata_edio ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      integer, intent(in) :: init
      integer, intent(in) :: igr
      integer, intent(in) :: ipy
      !----- Local variables. -------------------------------------------------------------!
      type(polygontype), pointer :: cpoly
      integer                    :: var_len
      integer                    :: max_ptrs
      integer                    :: var_len_global
      integer                    :: nvar
      integer                    :: npts
      !------------------------------------------------------------------------------------!



      !----- Check whether this polygon is allocated. -------------------------------------!
      if (.not.associated(edgrid_g(igr)%polygon(ipy)%sipa_id)) then
         write (unit=*,fmt='(a)')       '------------------------------------------------'
         write (unit=*,fmt='(a)')       ' Returning from filltab_polygontype'
         write (unit=*,fmt='(a,1x,i5)') ' IGR  = ',igr
         write (unit=*,fmt='(a,1x,i5)') ' IPY  = ',ipy
         write (unit=*,fmt='(a,1x,i5)') ' INIT = ',init
         write (unit=*,fmt='(a)')       '------------------------------------------------'
         return
      end if
      !------------------------------------------------------------------------------------!



      !----- Assign a pointer to the current polygon. -------------------------------------!
      cpoly => edgrid_g(igr)%polygon(ipy)
      !------------------------------------------------------------------------------------!



      !----- Define some variables to be used by all variables. ---------------------------!
      var_len        = cpoly%nsites
      var_len_global = edgrid_g(igr)%nsites_global
      max_ptrs       = edgrid_g(igr)%npolygons
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !     Initialise the variable number, which should be unique for each variable.      !
      ! nioglobal has the current position after all the parent structure variables have   !
      ! been positioned in the variable table.                                             !
      !------------------------------------------------------------------------------------!
      nvar=nioglobal+niogrid
      !------------------------------------------------------------------------------------!




      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 1-D vectors (nsites).  Notice that they all use   !
      ! npts = cpoly%nsites.  Add only variables of types 20 and 21 here.                  !
      !------------------------------------------------------------------------------------!
      npts = cpoly%nsites


      if (associated(cpoly%sipa_id)) then
         nvar=nvar+1
         call vtable_edio_i(npts,cpoly%sipa_id,nvar,igr,init,cpoly%siglob_id, &
              var_len,var_len_global,max_ptrs,'SIPA_ID :20:hist:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(cpoly%sipa_n)) then
         nvar=nvar+1
         call vtable_edio_i(npts,cpoly%sipa_n,nvar,igr,init,cpoly%siglob_id, &
              var_len,var_len_global,max_ptrs,'SIPA_N :20:hist:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(cpoly%patch_count)) then
         nvar=nvar+1
         call vtable_edio_i(npts,cpoly%patch_count,nvar,igr,init,cpoly%siglob_id, &
              var_len,var_len_global,max_ptrs,'PATCH_COUNT :20:hist:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(cpoly%sitenum)) then
         nvar=nvar+1
         call vtable_edio_i(npts,cpoly%sitenum,nvar,igr,init,cpoly%siglob_id, &
              var_len,var_len_global,max_ptrs,'SITENUM :20:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%lsl)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpoly%lsl,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'LSL_SI :20:hist:dail:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if   
      
      if (associated(cpoly%num_landuse_years)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpoly%num_landuse_years,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'NUM_LANDUSE_YEARS :20:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%hydro_next)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpoly%hydro_next,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'HYDRO_NEXT :20:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%hydro_prev)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpoly%hydro_prev,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'HYDRO_PREV :20:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%plantation)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpoly%plantation,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'PLANTATION_SI :20:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if 

      if (associated(cpoly%agri_stocking_pft)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpoly%agri_stocking_pft,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'AGRI_STOCKING_PFT :20:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%plantation_stocking_pft)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpoly%plantation_stocking_pft,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'PLANTATION_STOCKING_PFT :20:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%nat_dist_type)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpoly%nat_dist_type,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'NAT_DIST_TYPE :20:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%area)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%area,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'AREA_SI:21:hist:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%patch_area)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%patch_area,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'PATCH_AREA:21:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%elevation)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%elevation,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'ELEVATION :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%slope)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%slope,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'SLOPE :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%aspect)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%aspect,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'ASPECT :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%TCI)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%TCI,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'TCI :21:hist') 
         call metadata_edio(nvar,igr,'Topographic convergence index','[NA]','NA') 
      end if      

      if (associated(cpoly%pptweight)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%TCI,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'pptweight :21:hist') 
         call metadata_edio(nvar,igr,'precip lapse weighting','[NA]','NA') 
      end if      

      if (associated(cpoly%moist_W)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%moist_W,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'MOIST_W :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%moist_f)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%moist_f,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'MOIST_F :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if  

      if (associated(cpoly%moist_tau)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%moist_tau,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'MOIST_TAU :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%moist_zi)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%moist_zi,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'MOIST_ZI :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if 

      if (associated(cpoly%baseflow)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%baseflow  ,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'BASEFLOW_SI :21:hist') 
         call metadata_edio(nvar,igr,'loss of water from site to watershed discharge','[kg/m2/s]','NA') 
      end if 

      if (associated(cpoly%min_monthly_temp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%min_monthly_temp,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'MIN_MONTHLY_TEMP :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%agri_stocking_density)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%agri_stocking_density,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'AGRI_STOCKING_DENSITY :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%plantation_stocking_density)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%plantation_stocking_density,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'PLANTATION_STOCKING_DENSITY :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%primary_harvest_memory)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%primary_harvest_memory,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'PRIMARY_HARVEST_MEMORY :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%secondary_harvest_memory)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%secondary_harvest_memory,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'SECONDARY_HARVEST_MEMORY:21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%fire_disturbance_rate)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%fire_disturbance_rate,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'FIRE_DISTURBANCE_RATE :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%ignition_rate)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%ignition_rate,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'IGNITION_RATE :21:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%nat_disturbance_rate)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%nat_disturbance_rate,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'NAT_DISTURBANCE_RATE :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(cpoly%rad_avg)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%rad_avg,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'RAD_AVG :21:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if


      if(associated(cpoly%dmean_co2_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpoly%dmean_co2_residual,nvar,igr,init,cpoly%siglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_CO2_RESIDUAL_SI :21:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of residual canopy CO2 ','[umol/m2/day]','ipoly') 
      end if

      if(associated(cpoly%dmean_water_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpoly%dmean_water_residual,nvar,igr,init,cpoly%siglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_WATER_RESIDUAL_SI :21:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of residual water ','[kg/m2/day]','ipoly') 
      end if

      if(associated(cpoly%dmean_energy_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpoly%dmean_energy_residual,nvar,igr,init,cpoly%siglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ENERGY_RESIDUAL_SI :21:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of residual water ','[J/m2/day]','ipoly') 
      end if

      if(associated(cpoly%mmean_co2_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpoly%mmean_co2_residual,nvar,igr,init,cpoly%siglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_CO2_RESIDUAL_SI :21:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of residual canopy CO2 ','[umol/m2/day]','ipoly') 
      end if

      if(associated(cpoly%mmean_water_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpoly%mmean_water_residual,nvar,igr,init,cpoly%siglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_WATER_RESIDUAL_SI :21:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of residual water ','[kg/m2/day]','ipoly') 
      end if

      if(associated(cpoly%mmean_energy_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpoly%mmean_energy_residual,nvar,igr,init,cpoly%siglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ENERGY_RESIDUAL_SI :21:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of residual energy ','[J/m2/day]','ipoly') 
      end if

      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 2-D block, with dimensions being nsites and nzg.  Make sure to     !
      ! include only variables of types 22 and 220 here, as they will all use the same     !
      ! npts.                                                                              !
      !------------------------------------------------------------------------------------!
      npts = cpoly%nsites * nzg

  
      if (associated(cpoly%ntext_soil)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpoly%ntext_soil,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'NTEXT_SOIL_SI :220:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 2-D block, with dimensions being nsites and n_pft.  Make sure to   !
      ! include only variables of type 24 here, as they will all use the same npts.        !
      !------------------------------------------------------------------------------------!
      npts = cpoly%nsites * n_pft

      if (associated(cpoly%lai_pft)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%lai_pft,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'LAI_PFT_SI :24:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(cpoly%wpa_pft)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%wpa_pft,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'WPA_PFT_SI :24:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(cpoly%wai_pft)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%wai_pft,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'WAI_PFT_SI :24:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%green_leaf_factor)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%green_leaf_factor,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'GREEN_LEAF_FACTOR :24:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%leaf_aging_factor)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%leaf_aging_factor,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'LEAF_AGING_FACTOR :24:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 2-D block, with dimensions being nsites and n_dist_types.  Make    !
      ! sure to include only variables of type 25 here, as they will all use the same      !
      ! npts.                                                                              !
      !------------------------------------------------------------------------------------!
      npts = cpoly%nsites * n_dist_types

      if (associated(cpoly%loss_fraction)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%loss_fraction,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'LOSS_FRACTION :25:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 2-D block, with dimensions being nsites and 12 months.  Make sure  !
      ! to include only variables of type 29 here, as they will all use the same npts.     !
      !------------------------------------------------------------------------------------!
      npts = cpoly%nsites * 12

      if (associated(cpoly%lambda_fire)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%lambda_fire,nvar,igr,init,cpoly%siglob_id         &
                             ,var_len,var_len_global,max_ptrs,'LAMBDA_FIRE :29:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 3-D block, with dimensions being n_pft and n_dbh.  Make sure to    !
      ! include only variables of type 246 here, as they will all use the same npts.       !
      !------------------------------------------------------------------------------------!
      npts = cpoly%nsites * n_pft * n_dbh

      if (associated(cpoly%basal_area)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%basal_area,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'BASAL_AREA_SI :246:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%agb)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%agb,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'AGB_SI :246:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%pldens)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%pldens,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'PLDENS_SI :246:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%bseeds)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%bseeds,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'BSEEDS_SI :246:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%basal_area_growth)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%basal_area_growth,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'BASAL_AREA_GROWTH :246:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%agb_growth)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%agb_growth,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'AGB_GROWTH :246:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%basal_area_mort)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%basal_area_mort,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'BASAL_AREA_MORT :246:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%basal_area_cut)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%basal_area_cut,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'BASAL_AREA_CUT :246:year:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%agb_mort)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%agb_mort,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'AGB_MORT :246:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%agb_cut)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%agb_cut,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'AGB_CUT :246:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%avg_soil_rootfrac)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpoly%avg_soil_rootfrac,nvar,igr,init,cpoly%siglob_id, &
              var_len,var_len_global,max_ptrs,'SITE_SOIL_ROOTFRAC :12:hist:mont:year') 
         call metadata_edio(nvar,igr,'Site Average Root Fraction','[kg m-3]','ipoly - nzg') 
      end if


      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !     This is the 3-D block, with dimensions being nsites and n_dist_types,          !
      ! n_dist_types (from and to).  Make sure to include only variables of type 255 here, !
      ! as they will all use the same npts.                                                !
      !------------------------------------------------------------------------------------!
      npts = cpoly%nsites * n_dist_types * n_dist_types

      if (associated(cpoly%disturbance_memory)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%disturbance_memory,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'DISTURBANCE_MEMORY :255:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpoly%disturbance_rates)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpoly%disturbance_rates,nvar,igr,init,cpoly%siglob_id, &
           var_len,var_len_global,max_ptrs,'DISTURBANCE_RATES_SI :255:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!


      !----- Save the number of site-level (polygontype) variables that go to the output. -!
      if (init == 0) niopoly=nvar-niogrid-nioglobal
      !------------------------------------------------------------------------------------!

      return
   end subroutine filltab_polygontype
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   !     This sub-routine will fill the variable table with the sitetype variables (patch- !
   ! -level).                                                                              !
   !---------------------------------------------------------------------------------------!
   subroutine filltab_sitetype(igr,ipy,isi,init)

      use ed_var_tables, only : vtable_edio_r  & ! sub-routine
                              , vtable_edio_i  & ! sub-routine
                              , metadata_edio  ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      integer       , intent(in) :: init
      integer       , intent(in) :: igr
      integer       , intent(in) :: ipy
      integer       , intent(in) :: isi
      !----- Local variables. -------------------------------------------------------------!
      type(sitetype), pointer    :: csite
      integer                    :: var_len
      integer                    :: max_ptrs
      integer                    :: var_len_global
      integer                    :: nvar
      integer                    :: npts
      !------------------------------------------------------------------------------------!



      !----- Assign a pointer to the current polygon. -------------------------------------!
      csite => edgrid_g(igr)%polygon(ipy)%site(isi)
      !------------------------------------------------------------------------------------!



      !----- Define some variables to be used by all variables. ---------------------------!
      var_len        = csite%npatches
      var_len_global = edgrid_g(igr)%npatches_global
      max_ptrs       = get_nsites(edgrid_g(igr))
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !     Initialise the variable number, which should be unique for each variable.      !
      ! nioglobal has the current position after all the parent structure variables have   !
      ! been positioned in the variable table.                                             !
      !------------------------------------------------------------------------------------!
      nvar=nioglobal+niogrid+niopoly
      !------------------------------------------------------------------------------------!




      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 1-D vectors (npatches).  Notice that they all use !
      ! npts = csite%npatches.  Add only variables of types 30 and 31 here.                !
      !------------------------------------------------------------------------------------!
      npts = csite%npatches


      if (associated(csite%paco_id)) then
         nvar=nvar+1
         call vtable_edio_i(npts,csite%paco_id,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'PACO_ID :30:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%paco_n)) then
         nvar=nvar+1
           call vtable_edio_i(npts,csite%paco_n,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'PACO_N :30:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(csite%dist_type)) then
         nvar=nvar+1
           call vtable_edio_i(npts,csite%dist_type,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'DIST_TYPE :30:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%plantation)) then
         nvar=nvar+1
           call vtable_edio_i(npts,csite%plantation,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'PLANTATION :30:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%nlev_sfcwater)) then
         nvar=nvar+1
           call vtable_edio_i(npts,csite%nlev_sfcwater,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'NLEV_SFCWATER :30:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%fuse_flag)) then
         nvar=nvar+1
           call vtable_edio_i(npts,csite%fuse_flag,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'FUSE_FLAG :30:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%age)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%age,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AGE :31:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%area)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%area,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AREA :31:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%fast_soil_C)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%fast_soil_C,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'FAST_SOIL_C :31:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%slow_soil_C)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%slow_soil_C,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SLOW_SOIL_C :31:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%structural_soil_C)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%structural_soil_C,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'STRUCTURAL_SOIL_C :31:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%structural_soil_L)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%structural_soil_L,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'STRUCTURAL_SOIL_L :31:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%mineralized_soil_N)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mineralized_soil_N,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MINERALIZED_SOIL_N :31:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%fast_soil_N)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%fast_soil_N,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'FAST_SOIL_N :31:hist:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(csite%sum_dgd)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%sum_dgd,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SUM_DGD :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%sum_chd)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%sum_chd,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SUM_CHD :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if



      if (associated(csite%can_theiv)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%can_theiv,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CAN_THEIV :31:hist') 
         call metadata_edio(nvar,igr,'Canopy air ice-vapour equivalent potential temperature','[K]','NA') 
      end if

      if (associated(csite%can_temp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%can_temp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CAN_TEMP :31:hist') 
         call metadata_edio(nvar,igr,'Canopy air temperature','[K]','NA') 
      end if

      if (associated(csite%can_shv)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%can_shv,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CAN_SHV :31:hist') 
         call metadata_edio(nvar,igr,'Canopy air specific humidity','[kg_H2O/kg_Air]','NA') 
      end if

      if (associated(csite%can_co2)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%can_co2,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CAN_CO2 :31:hist') 
         call metadata_edio(nvar,igr,'Canopy air CO2 mixing ratio','[umol/mol]','NA') 
      end if
   
      if (associated(csite%can_rhos)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%can_rhos,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CAN_RHOS :31:hist') 
         call metadata_edio(nvar,igr,'Canopy air Density','[kg/m3]','NA') 
      end if
   
      if (associated(csite%can_prss)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%can_prss,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CAN_PRSS :31:hist') 
         call metadata_edio(nvar,igr,'Canopy air Pressure','[Pa]','NA') 
      end if
   
      if (associated(csite%can_theta)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%can_theta,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CAN_THETA :31:hist') 
         call metadata_edio(nvar,igr,'Canopy air Potential temperature','[K]','NA') 
      end if
     
      if (associated(csite%can_depth)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%can_depth,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CAN_DEPTH :31:hist') 
         call metadata_edio(nvar,igr,'Canopy depth','[m]','NA') 
      end if
     
      if (associated(csite%opencan_frac)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%opencan_frac,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'OPENCAN_FRAC :31:hist')
           call metadata_edio(nvar,igr,'Fraction of open canopy','[---]','NA') 
      end if
     
      if (associated(csite%ggbare)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ggbare,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'GGBARE :31:hist')
           call metadata_edio(nvar,igr,'Bare ground conductance','[m/s]','NA') 
      end if

      if (associated(csite%ggveg)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ggveg,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'GGVEG :31:hist')
           call metadata_edio(nvar,igr,'Vegetated ground conductance','[m/s]','NA') 
      end if

      if (associated(csite%ggnet)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ggnet,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'GGNET :31:hist')
           call metadata_edio(nvar,igr,'Net ground conductance','[m/s]','NA') 
      end if

      if (associated(csite%ggsoil)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ggsoil,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'GGSOIL :31:hist')
           call metadata_edio(nvar,igr,'Soil conductance for evaporation','[m/s]','NA') 
      end if

      if (associated(csite%lambda_light)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%lambda_light,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'LAMBDA_LIGHT :31:hist') 
         call metadata_edio(nvar,igr,'Light extinction','[m2/,2]','NA') 
      end if
     
      if (associated(csite%dmean_lambda_light)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%dmean_lambda_light,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_LAMBDA_LIGHT :31:hist:dail') 
         call metadata_edio(nvar,igr,'Light extinction','[m2/,2]','NA') 
      end if
     
      if (associated(csite%mmean_lambda_light)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mmean_lambda_light,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_LAMBDA_LIGHT :31:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Light extinction','[m2/,2]','NA') 
      end if

      if (associated(csite%lai)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%lai,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'LAI_PA :31:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%wpa)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%wpa,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'WPA_PA :31:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%wai)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%wai,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'WAI_PA :31:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ground_shv)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ground_shv,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'GROUND_SHV :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ground_ssh)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ground_ssh,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'GROUND_SSH :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ground_temp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ground_temp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'GROUND_TEMP :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ground_fliq)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ground_fliq,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'GROUND_FLIQ :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rough)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rough,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'ROUGH :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if


      if (associated(csite%mean_rh)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mean_rh,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MEAN_RH :31:hist') 
         call metadata_edio(nvar,igr,'Heterotrophic respiration','[umol/m2/s]','ipatch') 
      end if

      if (associated(csite%dmean_rh)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%dmean_rh,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_RH_PA :31:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of heterotrophic respiration','[umol/m2/s]','ipatch') 
      end if

      if (associated(csite%mmean_rh)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mmean_rh,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_RH_PA :31:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of heterotrophic respiration','[umol/m2/s]','ipatch') 
      end if

      if (associated(csite%dmean_albedo)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%dmean_albedo,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_ALBEDO_PA :31:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of albedo','[---]','ipatch') 
      end if

      if (associated(csite%dmean_albedo_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%dmean_albedo_beam,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_ALBEDO_BEAM_PA :31:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of beam albedo','[---]','ipatch') 
      end if

      if (associated(csite%dmean_albedo_diffuse)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%dmean_albedo_diffuse,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_ALBEDO_DIFFUSE_PA :31:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of diffuse albedo','[---]','ipatch') 
      end if

      if (associated(csite%mmean_albedo)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mmean_albedo,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_ALBEDO_PA :31:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of albedo','[---]','ipatch') 
      end if

      if (associated(csite%mmean_albedo_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mmean_albedo_beam,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_ALBEDO_BEAM_PA :31:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of beam albedo','[---]','ipatch') 
      end if

      if (associated(csite%mmean_albedo_diffuse)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mmean_albedo_diffuse,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_ALBEDO_DIFFUSE_PA :31:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of diffuse albedo','[---]','ipatch') 
      end if

      if (associated(csite%mean_nep)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mean_nep,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MEAN_NEP :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%wbudget_loss2atm)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%wbudget_loss2atm,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'WBUDGET_LOSS2ATM :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%wbudget_denseffect)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%wbudget_denseffect,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'WBUDGET_DENSEFFECT :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%wbudget_precipgain)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%wbudget_precipgain,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'WBUDGET_PRECIPGAIN :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%wbudget_loss2runoff)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%wbudget_loss2runoff,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'WBUDGET_LOSS2RUNOFF :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%wbudget_loss2drainage)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%wbudget_loss2drainage,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'WBUDGET_LOSS2DRAINAGE :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%wbudget_initialstorage)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%wbudget_initialstorage,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'WBUDGET_INITIALSTORAGE :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%wbudget_residual)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%wbudget_residual,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'WBUDGET_RESIDUAL :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ebudget_loss2atm)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ebudget_loss2atm,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'EBUDGET_LOSS2ATM :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ebudget_denseffect)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ebudget_denseffect,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'EBUDGET_DENSEFFECT :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ebudget_loss2runoff)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ebudget_loss2runoff,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'EBUDGET_LOSS2RUNOFF :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ebudget_loss2drainage)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ebudget_loss2drainage,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'EBUDGET_LOSS2DRAINAGE :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ebudget_netrad)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ebudget_netrad,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'EBUDGET_NETRAD :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ebudget_precipgain)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ebudget_precipgain,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'EBUDGET_PRECIPGAIN :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ebudget_initialstorage)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ebudget_initialstorage,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'EBUDGET_INITIALSTORAGE :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ebudget_residual)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ebudget_residual,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'EBUDGET_RESIDUAL :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%co2budget_initialstorage)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%co2budget_initialstorage,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CO2BUDGET_INITIALSTORAGE :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%co2budget_residual)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%co2budget_residual,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CO2BUDGET_RESIDUAL :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%co2budget_loss2atm)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%co2budget_loss2atm,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CO2BUDGET_LOSS2ATM :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%co2budget_denseffect)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%co2budget_denseffect,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CO2BUDGET_DENSEFFECT :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%co2budget_gpp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%co2budget_gpp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CO2BUDGET_GPP :31:hist') 
         call metadata_edio(nvar,igr,'Patch total gross primary productivity per timestep','[umol/m2/dtlsm]','NA') 
      end if


      if (associated(csite%avg_daily_temp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%avg_daily_temp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AVG_DAILY_TEMP :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if  

      if (associated(csite%co2budget_plresp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%co2budget_plresp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CO2BUDGET_PLRESP :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%co2budget_rh)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%co2budget_rh,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CO2BUDGET_RH :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%today_A_decomp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%today_A_decomp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_A_DECOMP :31:hist') 
         call metadata_edio(nvar,igr,'NOT A DIAGNOSTIC-WILL ZERO AT END OF DAY','[NA]','NA') 
      end if

      if (associated(csite%today_Af_decomp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%today_Af_decomp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_AF_DECOMP :31:hist') 
         call metadata_edio(nvar,igr,'NOT A DIAGNOSTIC-WILL ZERO AT END OF DAY','[NA]','NA') 
      end if


      if (associated(csite%dmean_A_decomp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%dmean_A_decomp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_A_DECOMP :31:hist:dail') 
         call metadata_edio(nvar,igr,'A factor for decomposition','[--]','ipatch') 
      end if

      if (associated(csite%dmean_Af_decomp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%dmean_Af_decomp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_AF_DECOMP :31:hist:dail') 
         call metadata_edio(nvar,igr,'A factor for decomposition, including N','[--]','ipatch') 
      end if


      if (associated(csite%mmean_A_decomp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mmean_A_decomp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_A_DECOMP :31:hist:dcyc:mont') 
         call metadata_edio(nvar,igr,'Monthly mean of A factor for decomposition','[--]','ipatch') 
      end if

      if (associated(csite%mmean_Af_decomp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mmean_Af_decomp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_AF_DECOMP :31:hist:dcyc:mont') 
         call metadata_edio(nvar,igr,'Monthly mean A factor for decomposition, including N','[--]','ipatch') 
      end if

      if (associated(csite%veg_rough)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%veg_rough,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'VEG_ROUGH :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%veg_height)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%veg_height ,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'VEG_HEIGHT :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%veg_displace)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%veg_displace ,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'VEG_DISPLACE :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%fsc_in)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%fsc_in,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'FSC_IN :31:dail:hist:mont:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ssc_in)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ssc_in,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SSC_IN :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%ssl_in)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ssl_in,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SSL_IN :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%fsn_in)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%fsn_in,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'FSN_IN :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%total_plant_nitrogen_uptake)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%total_plant_nitrogen_uptake,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'TOTAL_PLANT_NITROGEN_UPTAKE :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%mineralized_N_loss)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mineralized_N_loss,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'NMIN_LOSS :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if


      if (associated(csite%mineralized_N_input)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mineralized_N_input,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'NMIN_INPUT :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rshort_g)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rshort_g,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_G :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rshort_g_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rshort_g_beam,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_G_BEAM :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rshort_g_diffuse)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rshort_g_diffuse,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_G_DIFFUSE :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rlong_g)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rlong_g,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_G :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rlong_g_surf)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rlong_g_surf,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_G_SURF :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rlong_g_incid)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rlong_g_incid,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_G_INCID :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rlong_s)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rlong_s,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_S :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rlong_s_surf)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rlong_s_surf,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_S_SURF :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rlong_s_incid)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rlong_s_incid,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_S_INCID :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%albedo)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%albedo,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'ALBEDO :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%albedo_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%albedo_beam,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'ALBEDO_BEAM :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%albedo_diffuse)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%albedo_diffuse,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'ALBEDO_DIFFUSE :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rlongup)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rlongup,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RLONGUP :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rlong_albedo)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rlong_albedo,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_ALBEDO :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%total_sfcw_depth)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%total_sfcw_depth,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'TOTAL_SFCW_DEPTH :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%snowfac)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%snowfac,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SNOWFAC :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%A_decomp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%A_decomp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'A_DECOMP :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%f_decomp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%f_decomp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'F_DECOMP :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rh)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rh,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RH :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%cwd_rh)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%cwd_rh,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CWD_RH :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%plant_ag_biomass)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%plant_ag_biomass,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'PLANT_AG_BIOMASS :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(csite%mean_wflux)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mean_wflux,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MEAN_WFLUX :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%mean_latflux)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mean_latflux,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MEAN_LATFLUX :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%mean_hflux)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mean_hflux,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MEAN_HFLUX :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%mean_runoff)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mean_runoff,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MEAN_RUNOFF :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%mean_qrunoff)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mean_qrunoff,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MEAN_QRUNOFF :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(csite%htry)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%htry,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'HTRY :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(csite%avg_rk4step)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%avg_rk4step,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AVG_RK4STEP :31:hist:anal') 
         call metadata_edio(nvar,igr,'Average time step used by Runge-Kutta','[s]','ipatch') 
      end if
      
      if (associated(csite%avg_available_water)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%avg_available_water,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AVG_AVAILABLE_WATER_PA :31:hist:anal') 
         call metadata_edio(nvar,igr,'Average available water for transpiration','[kg/m2]','ipatch') 
      end if
      
      if (associated(csite%dmean_rk4step)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%dmean_rk4step,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_RK4STEP :31:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean time step used by Runge-Kutta','[s]','ipatch') 
      end if
      
      if (associated(csite%mmean_rk4step)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mmean_rk4step,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_RK4STEP :31:hist:dcyc:mont') 
         call metadata_edio(nvar,igr,'Monthly mean time step used by Runge-Kutta','[s]','ipatch') 
      end if
      
      if (associated(csite%ustar)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ustar,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'USTAR :31:hist') 
           call metadata_edio(nvar,igr,'Patch level friction velocity','[m/s]','ipatch') 
      end if

      if (associated(csite%tstar)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%tstar,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'TSTAR :31:hist') 
         call metadata_edio(nvar,igr,'patch level heat transfer atm->canopy','[K]','ipatch') 
      end if

      if (associated(csite%qstar)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%qstar,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'QSTAR :31:hist') 
         call metadata_edio(nvar,igr,'patch level vapor transfer atm->canopy','[kg/kg]','ipatch') 
      end if

      if (associated(csite%cstar)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%cstar,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CSTAR :31:hist') 
         call metadata_edio(nvar,igr,'patch level co2 transfer atm->canopy','[ppm?]','ipatch') 
      end if

      if (associated(csite%zeta)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%zeta,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'ZETA :31:hist') 
         call metadata_edio(nvar,igr,'patch level height over Obukhov length','[---]','ipatch') 
      end if

      if (associated(csite%ribulk)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%ribulk,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RIBULK :31:hist') 
         call metadata_edio(nvar,igr,'patch level bulk Richardson number','[---]','ipatch') 
      end if
      
      if (associated(csite%upwp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%upwp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'UPWP :31:hist') 
         call metadata_edio(nvar,igr,'Vertical flux of U-direction momentum','[kg m^-1 s^-2]','ipatch') 
      end if

      if (associated(csite%tpwp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%tpwp,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'TPWP :31:hist') 
         call metadata_edio(nvar,igr,'Vertical flux of Heat','[kg K m^-2 s^-1]','ipatch')
      end if

      if (associated(csite%qpwp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%qpwp,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'QPWP :31:hist') 
         call metadata_edio(nvar,igr,'Vertical flux of Moisture','[kg m^-2 s% -1]','ipatch')
      end if

      if (associated(csite%cpwp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%cpwp,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'CPWP :31:hist') 
         call metadata_edio(nvar,igr,'Vertical flux of Carbon','[kg umolC mol^-1 m^-2 s-1]','ipatch')
      end if
      
      if (associated(csite%wpwp)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%wpwp,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'WPWP :31:hist') 
         call metadata_edio(nvar,igr,'Vertical flux of W-direction momentum','[kg m^-1 s^-2]','ipatch')
      end if
      
      if (associated(csite%par_l_max)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%par_l_max,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'PAR_L_MAX :31:hist') 
         call metadata_edio(nvar,igr,'Maximum PAR - not an output variable','[W/m2]','ipatch')
      end if
      
      if (associated(csite%par_l_beam_max)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%par_l_beam_max,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'PAR_L_BEAM_MAX :31:hist') 
         call metadata_edio(nvar,igr,'Maximum direct PAR - not an output variable' &
                           ,'[W/m2]','ipatch')
      end if
      
      if (associated(csite%par_l_diffuse_max)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%par_l_diffuse_max,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'PAR_L_DIFFUSE_MAX :31:hist') 
         call metadata_edio(nvar,igr,'Maximum diffuse PAR - not an output variable' &
                           ,'[W/m2]','ipatch')
      end if

      if (associated(csite%avg_rshort_gnd)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%avg_rshort_gnd,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AVG_RSHORT_GND_PA :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%avg_rlong_gnd)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%avg_rlong_gnd,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AVG_RLONG_GND_PA :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%avg_rlongup)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%avg_rlongup,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AVG_RLONGUP_PA :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%avg_albedo)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%avg_albedo,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AVG_ALBEDO_PA :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%avg_albedo_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%avg_albedo_beam,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AVG_ALBEDO_BEAM_PA :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%avg_albedo_diffuse)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%avg_albedo_diffuse,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AVG_ALBEDO_DIFFUSE_PA :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%avg_rlong_albedo)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%avg_rlong_albedo,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AVG_RLONG_ALBEDO_PA :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%avg_carbon_ac)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%avg_carbon_ac,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'AVG_CARBON_AC_PA :31:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if(associated(csite%dmean_co2_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%dmean_co2_residual,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_CO2_RESIDUAL_PA :31:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of residual canopy CO2 ','[umol/m2/day]','ipoly') 
      end if

      if(associated(csite%dmean_water_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%dmean_water_residual,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_WATER_RESIDUAL_PA :31:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of residual water ','[kg/m2/day]','ipoly') 
      end if

      if(associated(csite%dmean_energy_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%dmean_energy_residual,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_ENERGY_RESIDUAL_PA :31:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean of residual water ','[J/m2/day]','ipoly') 
      end if

      if(associated(csite%mmean_co2_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%mmean_co2_residual,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_CO2_RESIDUAL_PA :31:hist:dcyc:mont') 
         call metadata_edio(nvar,igr,'Monthly mean of residual canopy CO2 ','[umol/m2/day]','ipoly') 
      end if

      if(associated(csite%mmean_water_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%mmean_water_residual,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_WATER_RESIDUAL_PA :31:hist:dcyc:mont') 
         call metadata_edio(nvar,igr,'Monthly mean of residual water ','[kg/m2/day]','ipoly') 
      end if

      if(associated(csite%mmean_energy_residual)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%mmean_energy_residual,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_ENERGY_RESIDUAL_PA :31:hist:dcyc:mont') 
         call metadata_edio(nvar,igr,'Monthly mean of residual energy ','[J/m2/day]','ipoly') 
      end if

      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!






      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 2-D vectors with dimensions npatches and ndcycle. !
      !  Notice that they all use the same npts.  Here you should only add variables of    !
      ! types -31.                                                                         !
      !------------------------------------------------------------------------------------!
      npts = csite%npatches * ndcycle

      if (associated(csite%qmean_rh)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%qmean_rh,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_RH_PA :-31:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%qmean_albedo)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%qmean_albedo,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_ALBEDO_PA :-31:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%qmean_albedo_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%qmean_albedo_beam,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_ALBEDO_BEAM_PA :-31:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%qmean_albedo_diffuse)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%qmean_albedo_diffuse,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_ALBEDO_DIFFUSE_PA :-31:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !--------JL----------------------------------------------!

      if (associated(csite%interval_DON_loss)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%interval_DON_loss,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'interval_DON_loss :31:dail:hist:mont:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%N_leached)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%N_leached,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'N_LEACHED :31:hist:dail:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%N_gas_loss)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%N_gas_loss,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'N_GAS_LOSS :31:hist:dail:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 

      if (associated(csite%mmean_fsn_in)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mmean_fsn_in,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_FSN_IN :31:hist:dcyc:mont') 
         call metadata_edio(nvar,igr,'Monthly mean A factor for decomposition, including N','[--]','ipatch') 
      end if

     if (associated(csite%mmean_ssc_in)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mmean_ssc_in,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_SSC_IN :31:hist:dcyc:mont') 
         call metadata_edio(nvar,igr,'Monthly mean A factor for decomposition, including N','[--]','ipatch') 
      end if

     if (associated(csite%mmean_total_plant_nitrogen_uptake)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%mmean_total_plant_nitrogen_uptake,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_TOTAL_PLANT_NITROGEN_UPTAKE :31:hist:dcyc:mont') 
         call metadata_edio(nvar,igr,'Monthly mean A factor for decomposition, including N','[--]','ipatch') 
      end if

      if (associated(csite%fsc_in_no_excess )) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%fsc_in_no_excess ,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'FSC_IN_NO_EXCESS :31:dail:hist:mont:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !--------JL----------------------------------------------!
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!






      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 2-D vectors with dimensions npatches and nzg.     !
      !  Notice that they all use the same npts.  Here you should only add variables of    !
      ! types 32 and 320.                                                                  !
      !------------------------------------------------------------------------------------!
      npts = csite%npatches * nzg

      if (associated(csite%soil_energy)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%soil_energy,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SOIL_ENERGY_PA :32:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%soil_water)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%soil_water,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SOIL_WATER_PA :32:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%current_paw)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%current_paw,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CURRENT_PAW_PA :32:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%soil_tempk)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%soil_tempk,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SOIL_TEMPK_PA :32:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[K]','ipatch : nzg') 
      end if

      if (associated(csite%soil_fracliq)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%soil_fracliq,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SOIL_FRACLIQ_PA :32:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[normalized]','ipatch : nzg') 
      end if

      if (associated(csite%rootdense)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rootdense,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'PATCH_ROOT_DENSITY :32:hist') 
         call metadata_edio(nvar,igr,'Patch level root density with depth','[kg/m3]',&
              'ipatch : nzg') 
      end if

      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 2-D vectors with dimensions npatches and nzs.     !
      !  Notice that they all use the same npts.  Here you should only add variables of    !
      ! type 33.                                                                           !
      !------------------------------------------------------------------------------------!
      npts = csite%npatches * nzs


      if (associated(csite%sfcwater_mass)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%sfcwater_mass,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SFCWATER_MASS :33:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%sfcwater_energy)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%sfcwater_energy,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SFCWATER_ENERGY :33:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%sfcwater_depth)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%sfcwater_depth,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SFCWATER_DEPTH :33:hist:opti') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','m') 
      end if

      if (associated(csite%rshort_s)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rshort_s,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_S :33:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rshort_s_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rshort_s_beam,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_S_BEAM :33:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%rshort_s_diffuse)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%rshort_s_diffuse,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_S_DIFFUSE :33:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%sfcwater_tempk)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%sfcwater_tempk,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SFCWATER_TEMPK :33:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%sfcwater_fracliq)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%sfcwater_fracliq,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'SFCWATER_FRACLIQ :33:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!






      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 2-D vectors with dimensions npatches and n_pft.   !
      !  Notice that they all use the same npts.  Here you should only add variables of    !
      ! type 34.                                                                           !
      !------------------------------------------------------------------------------------!
      npts = csite%npatches * n_pft

      if (associated(csite%A_o_max)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%A_o_max,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'A_O_MAX :34:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%A_c_max)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%A_c_max,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'A_C_MAX :34:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(csite%repro)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%repro,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'REPRO_PA :34:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!






      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 2-D vectors with dimensions npatches and n_dbh.   !
      !  Notice that they all use the same npts.  Here you should only add variables of    !
      ! type 36.                                                                           !
      !------------------------------------------------------------------------------------!
      npts = csite%npatches * n_dbh

      if (associated(csite%co2budget_gpp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%co2budget_gpp,nvar,igr,init,csite%paglob_id, &
           var_len,var_len_global,max_ptrs,'CO2BUDGET_GPP_DBH :36:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!






      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 3-D vectors with dimensions npatches and n_dbh.   !
      !  Notice that they all use the same npts.  Here you should only add variables of    !
      ! type 346.                                                                          !
      !------------------------------------------------------------------------------------!
      npts = csite%npatches * n_pft * n_dbh

      if (associated(csite%cumlai_profile)) then
         nvar=nvar+1
           call vtable_edio_r(npts,csite%cumlai_profile,nvar,igr,init,csite%paglob_id,     &
           var_len,var_len_global,max_ptrs,'CUMLAI_PROFILE :346:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!






      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 3-D vectors with dimensions n_stoma_atts and      !
      ! n_dbh.  Notice that they all use the same npts.  Here you should only add vari-    !
      ! ables of type 316.                                                                 !
      !------------------------------------------------------------------------------------!
      npts = csite%npatches * n_stoma_atts * n_pft

      if (associated(csite%old_stoma_vector_max)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%old_stoma_vector_max,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'OLD_STOMA_VECTOR_MAX :316:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!
      npts = csite%npatches * nzg * 10

      if (associated(csite%past_paw)) then
         nvar=nvar+1
         call vtable_edio_r(npts,csite%past_paw,nvar,igr,init,csite%paglob_id, &
              var_len,var_len_global,max_ptrs,'PAST_PAW_PA :317:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      !------------------------------------------------------------------------------------!



      !----- Save the number of patch-level (sitetype) variables that go to the output. ---!
      if (init == 0) niosite=nvar-niopoly-niogrid-nioglobal
      !------------------------------------------------------------------------------------!

      return
      
   end subroutine filltab_sitetype
   !=======================================================================================!
   !=======================================================================================!






   !=======================================================================================!
   !=======================================================================================!
   subroutine filltab_patchtype(igr,ipy,isi,ipa,init)

      use ed_var_tables, only : vtable_edio_r & ! sub-routine
                              , vtable_edio_i & ! sub-routine
                              , metadata_edio ! ! sub-routine

      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      integer          , intent(in) :: init
      integer          , intent(in) :: igr
      integer          , intent(in) :: ipy
      integer          , intent(in) :: isi
      integer          , intent(in) :: ipa
      !----- Local variables. -------------------------------------------------------------!
      type(polygontype), pointer    :: cpoly
      type(sitetype)   , pointer    :: csite
      type(patchtype)  , pointer    :: cpatch
      integer                       :: ip
      integer                       :: np
      integer                       :: is
      integer                       :: ns
      integer                       :: iy
      integer                       :: ny
      integer                       :: var_len
      integer                       :: max_ptrs
      integer                       :: var_len_global
      integer                       :: nvar
      integer                       :: npts
      !------------------------------------------------------------------------------------!



      !------------------------------------------------------------------------------------!
      !      The condition exists where some patches may have no cohorts, particularly     !
      ! over deserts.  Therefore it is possible that the first patch in the site has no    !
      ! cohorts, but the others might have some cohorts.  Therefore, we want to make sure  !
      ! we create mapping tables if "any" of the patches have cohorts, so we will provide  !
      ! for this conditions when we make the mapping tables.  The condition has to arise   !
      ! where there are no cohorts at all in any of the patches in any of the polygons on  !
      ! the current machine to not initialize these vectors.                               !
      !------------------------------------------------------------------------------------!
      if (init==0) then
         ny = edgrid_g(igr)%npolygons

         polyloop: do iy=1,ny
            cpoly => edgrid_g(igr)%polygon(iy)
            ns = cpoly%nsites
            siteloop: do is=1,ns
               csite => cpoly%site(is)
               np = csite%npatches
               patchloop: do ip=1,np
                  cpatch => edgrid_g(igr)%polygon(iy)%site(is)%patch(ip)
                  if (cpatch%ncohorts > 0) exit polyloop
               end do patchloop
            end do siteloop
         end do polyloop

      else
         cpatch => edgrid_g(igr)%polygon(ipy)%site(isi)%patch(ipa)
      end if
      !------------------------------------------------------------------------------------!


      !----- Nothing else to do here in case this is an empty patch. ----------------------!
      if (cpatch%ncohorts == 0) return
      !------------------------------------------------------------------------------------!


      !----- Define some variables to be used by all variables. ---------------------------!
      var_len        = cpatch%ncohorts
      var_len_global = edgrid_g(igr)%ncohorts_global
      max_ptrs       = edgrid_g(igr)%npatches_global
      !------------------------------------------------------------------------------------!


      !------------------------------------------------------------------------------------!
      !     Initialise the variable number, which should be unique for each variable.      !
      ! nioglobal has the current position after all the parent structure variables have   !
      ! been positioned in the variable table.                                             !
      !------------------------------------------------------------------------------------!
      nvar=nioglobal+niogrid+niopoly+niosite
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 1-D vectors, with dimension ncohorts.  Notice     !
      ! that they all use the same npts.  Here you should only add variables of type 40    !
      ! and 41.                                                                            !
      !------------------------------------------------------------------------------------!
      npts = cpatch%ncohorts

      if (associated(cpatch%pft)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpatch%pft,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'PFT :40:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'Plant Functional Type','[-]','NA') 
      end if


      if (associated(cpatch%phenology_status)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpatch%phenology_status,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'PHENOLOGY_STATUS :40:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%krdepth)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpatch%krdepth,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'KRDEPTH :40:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%first_census)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpatch%first_census,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'FIRST_CENSUS :40:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%new_recruit_flag)) then
         nvar=nvar+1
           call vtable_edio_i(npts,cpatch%new_recruit_flag,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'NEW_RECRUIT_FLAG :40:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if


      if (associated(cpatch%nplant)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%nplant,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'NPLANT :41:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'Plant density','[plant/m2]','NA') 
      end if

      if (associated(cpatch%hite)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%hite,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'HITE :41:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%agb)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%agb,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'AGB_CO :41:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'Above-ground biomass','[kgC/plant]','icohort') 
      end if

      if (associated(cpatch%basarea)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%basarea,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'BA_CO :41:hist:anal:dail:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'Basal-area','[cm2]','icohort') 
      end if

      if (associated(cpatch%dagb_dt)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dagb_dt,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DAGB_DT :41:hist:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'Above-ground biomass growth','[kgC/plant/yr]','icohort') 
      end if

      if (associated(cpatch%dba_dt)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dba_dt,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DBA_DT :41:hist:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'Basal-area growth','[cm2/plant/yr]','icohort') 
      end if

      if (associated(cpatch%ddbh_dt)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%ddbh_dt,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DDBH_DT :41:hist:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'DBH growth','[cm/plant/yr]','icohort') 
      end if

      if (associated(cpatch%dbh)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dbh,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DBH :41:hist:anal:year:dail:mont:dcyc') 
         call metadata_edio(nvar,igr,'Diameter at breast height','[cm]','icohort') 
      end if

      if (associated(cpatch%bdead)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%bdead,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'BDEAD :41:hist:mont:anal:dail:year:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%bleaf)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%bleaf,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'BLEAF :41:hist:year:anal:dail:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%balive)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%balive,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'BALIVE :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%broot)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%broot,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'BROOT :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%bsapwood)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%bsapwood,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'BSAPWOOD :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%lai)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%lai,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LAI_CO :41:hist:dail:anal:mont:year:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%wpa)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%wpa,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'WPA_CO :41:hist:dail:anal:mont:year:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%wai)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%wai,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'WAI_CO :41:hist:dail:anal:mont:year:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%crown_area)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%crown_area,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'CROWN_AREA_CO :41:hist:dail:anal:mont:year:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%bstorage)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%bstorage,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'BSTORAGE :41:hist:year:anal:dail:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%cbr_bar)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%cbr_bar,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'CBR_BAR :41:hist:mont:year:dcyc') 
         call metadata_edio(nvar,igr,'Annual average ratio of cb/cb_max','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_cb)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_cb,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_CB :41:hist:mont:year:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of carbon balance','[kgC/plant/yr]','NA') 
      end if

      if (associated(cpatch%leaf_energy)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%leaf_energy,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LEAF_ENERGY :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%leaf_hcap)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%leaf_hcap,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LEAF_HCAP :41:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%leaf_temp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%leaf_temp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LEAF_TEMP :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%leaf_fliq)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%leaf_fliq,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LEAF_FLIQ :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%leaf_water)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%leaf_water,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LEAF_WATER :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%wood_energy)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%wood_energy,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'WOOD_ENERGY :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%wood_hcap)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%wood_hcap,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'WOOD_HCAP :41:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%wood_temp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%wood_temp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'WOOD_TEMP :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%wood_fliq)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%wood_fliq,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'WOOD_FLIQ :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%wood_water)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%wood_water,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'WOOD_WATER :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%veg_wind)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%veg_wind,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'VEG_WIND :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%lsfc_shv_closed)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%lsfc_shv_closed,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LSFC_SHV_CLOSED :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%lsfc_shv_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%lsfc_shv_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LSFC_SHV_OPEN :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%lsfc_co2_closed)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%lsfc_co2_closed,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LSFC_CO2_CLOSED :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%lsfc_co2_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%lsfc_co2_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LSFC_CO2_OPEN :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%lint_shv)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%lint_shv,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LINT_SHV :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%lint_co2_closed)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%lint_co2_closed,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LINT_CO2_CLOSED :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%lint_co2_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%lint_co2_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LINT_CO2_OPEN :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mean_gpp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mean_gpp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MEAN_GPP :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mean_leaf_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mean_leaf_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MEAN_LEAF_RESP :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mean_root_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mean_root_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MEAN_ROOT_RESP :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%today_leaf_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_leaf_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_LEAF_RESP :41:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%today_root_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_root_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_ROOT_RESP :41:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%today_gpp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_gpp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_GPP :41:hist') 
         call metadata_edio(nvar,igr,'NOT A DIAGNOSTIC-WILL ZERO PRIOR TO DAILY WRITE OUT','[umol/m2/s]','icohort') 
      end if

      
      if (associated(cpatch%today_nppleaf)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_nppleaf,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_NPPLEAF :41:hist') 
         call metadata_edio(nvar,igr,'NOT A DIAGNOSTIC-WILL ZERO PRIOR TO DAILY WRITE OUT','[kgc/m2/day]','icohort') 
      end if
      
      if (associated(cpatch%today_nppfroot)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_nppfroot,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_NPPFROOT :41:hist') 
         call metadata_edio(nvar,igr,'NOT A DIAGNOSTIC-WILL ZERO PRIOR TO DAILY WRITE OUT','[kgc/m2/day]','icohort') 
      end if
      
      if (associated(cpatch%today_nppsapwood)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_nppsapwood,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_NPPSAPWOOD :41:hist') 
         call metadata_edio(nvar,igr,'NOT A DIAGNOSTIC-WILL ZERO PRIOR TO DAILY WRITE OUT','[kgc/m2/day]','icohort') 
      end if

      if (associated(cpatch%today_nppcroot)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_nppcroot,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_NPPCROOT :41:hist') 
         call metadata_edio(nvar,igr,'NOT A DIAGNOSTIC-WILL ZERO PRIOR TO DAILY WRITE OUT','[kgc/m2/day]','icohort') 
      end if
      
      if (associated(cpatch%today_nppseeds)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_nppseeds,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_NPPSEEDS :41:hist') 
         call metadata_edio(nvar,igr,'NOT A DIAGNOSTIC-WILL ZERO PRIOR TO DAILY WRITE OUT','[kgc/m2/day]','icohort') 
      end if
      
      if (associated(cpatch%today_nppwood)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_nppwood,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_NPPWOOD :41:hist') 
         call metadata_edio(nvar,igr,'NOT A DIAGNOSTIC-WILL ZERO PRIOR TO DAILY WRITE OUT','[kgc/m2/day]','icohort') 
      end if
      
      if (associated(cpatch%today_nppdaily)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_nppdaily,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_NPPDAILY :41:hist') 
         call metadata_edio(nvar,igr,'NOT A DIAGNOSTIC-WILL ZERO PRIOR TO DAILY WRITE OUT','[kgc/m2/day]','icohort') 
      end if
      
      if (associated(cpatch%today_gpp_pot)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_gpp_pot,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_GPP_POT :41:hist') 
         call metadata_edio(nvar,igr,'NOT A DIAGNOSTIC-WILL ZERO PRIOR TO DAILY WRITE OUT','[NA]','NA') 
      end if

      if (associated(cpatch%today_gpp_max)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%today_gpp_max,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TODAY_GPP_MAX :41:hist') 
         call metadata_edio(nvar,igr,'NOT A DIANOSTIC-WILL ZERO PRIOR TO DAILY WRITE OUT','[NA]','NA') 
      end if

      if (associated(cpatch%growth_respiration)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%growth_respiration,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'GROWTH_RESPIRATION :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%storage_respiration)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%storage_respiration,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'STORAGE_RESPIRATION :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%vleaf_respiration)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%vleaf_respiration,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'VLEAF_RESPIRATION :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if  

      if (associated(cpatch%dmean_gpp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_gpp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_GPP_CO :41:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean Gross Primary Productivity','[kgC/plant/yr]','icohort') 
      end if


      if (associated(cpatch%dmean_nppleaf)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%dmean_nppleaf,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPLEAF_CO :41:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean NPP leaf','[kgC/plant/yr]','ipoly') 
      end if

      if (associated(cpatch%dmean_nppfroot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%dmean_nppfroot,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPFROOT_CO :41:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean NPP froot','[kgC/plant/yr]','ipoly') 
      end if

            
      if (associated(cpatch%dmean_nppsapwood)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%dmean_nppsapwood,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPSAPWOOD_CO :41:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean NPP sapwood','[kgC/plant/yr]','ipoly') 
      end if

      if (associated(cpatch%dmean_nppcroot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%dmean_nppcroot,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPCROOT_CO :41:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean NPP croot','[kgC/plant/yr]','ipoly') 
      end if
      
      if (associated(cpatch%dmean_nppseeds)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%dmean_nppseeds,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPSEEDS_CO :41:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean NPP seeds','[kgC/plant/yr]','ipoly') 
      end if

      if (associated(cpatch%dmean_nppwood)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%dmean_nppwood,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPWOOD_CO :41:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean NPP wood','[kgC/plant/yr]','ipoly') 
      end if
      
      if (associated(cpatch%dmean_nppdaily)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%dmean_nppdaily,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'DMEAN_NPPDAILY_CO :41:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean NPP daily','[kgC/plant/yr]','ipoly') 
      end if


      if (associated(cpatch%dmean_leaf_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_leaf_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_LEAF_RESP_CO :41:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean leaf respiration','[kgC/plant/yr]','icohort') 
      end if

      if (associated(cpatch%dmean_root_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_root_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_ROOT_RESP_CO :41:hist:dail') 
         call metadata_edio(nvar,igr,'Daily mean root respiration','[kgC/plant/yr]','icohort') 
      end if

      if (associated(cpatch%mmean_gpp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_gpp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_GPP_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean Gross Primary Productivity','[kgC/plant/yr]','icohort') 
      end if

      if (associated(cpatch%mmean_nppleaf)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%mmean_nppleaf,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPLEAF_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean NPP leaf','[kgC/plant/yr]','ipoly') 
      end if

      if (associated(cpatch%mmean_nppfroot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%mmean_nppfroot,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPFROOT_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean NPP froot','[kgC/plant/yr]','ipoly') 
      end if

            
      if (associated(cpatch%mmean_nppsapwood)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%mmean_nppsapwood,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPSAPWOOD_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean NPP sapwood','[kgC/plant/yr]','ipoly') 
      end if

      if (associated(cpatch%mmean_nppcroot)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%mmean_nppcroot,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPCROOT_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean NPP croot','[kgC/plant/yr]','ipoly') 
      end if

      if (associated(cpatch%mmean_nppseeds)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%mmean_nppseeds,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPSEEDS_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean NPP seeds','[kgC/plant/yr]','ipoly') 
      end if

      if (associated(cpatch%mmean_nppwood)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%mmean_nppwood,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPWOOD_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean NPP wood','[kgC/plant/yr]','ipoly') 
      end if
      
      if (associated(cpatch%mmean_nppdaily)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%mmean_nppdaily,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'MMEAN_NPPDAILY_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean NPP daily','[kgC/plant/yr]','ipoly') 
      end if

      if (associated(cpatch%mmean_leaf_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_leaf_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_LEAF_RESP_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mstructural_growthean leaf respiration','[kgC/plant/yr]','icohort') 
      end if

      if (associated(cpatch%mmean_root_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_root_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_ROOT_RESP_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean root respiration','[kgC/plant/yr]','icohort') 
      end if

      if (associated(cpatch%mmean_growth_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_growth_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_GROWTH_RESP_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean growth respiration','[kgC/plant/yr]','icohort') 
      end if

      if (associated(cpatch%mmean_storage_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_storage_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_STORAGE_RESP_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean storage respiration','[kgC/plant/yr]','icohort') 
      end if

      if (associated(cpatch%mmean_vleaf_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_vleaf_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_VLEAF_RESP_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean virtual leaf respiration','[kgC/plant/yr]','icohort') 
      end if

      if (associated(cpatch%fsn)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%fsn,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'FSN :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%monthly_dndt)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%monthly_dndt,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MONTHLY_DNDT :41:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%Psi_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%Psi_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'PSI_OPEN :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%light_level)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%light_level,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LIGHT_LEVEL :41:hist:anal') 
         call metadata_edio(nvar,igr,'Relative light level','[NA]','icohort') 
      end if

      if (associated(cpatch%light_level_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%light_level_beam,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LIGHT_LEVEL_BEAM :41:hist:anal') 
         call metadata_edio(nvar,igr,'Relative light level, beam fraction','[NA]','icohort') 
      end if

      if (associated(cpatch%light_level_diff)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%light_level_diff,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LIGHT_LEVEL_DIFF :41:hist:anal') 
         call metadata_edio(nvar,igr,'Relative light level, diffuse fraction','[NA]','icohort') 
      end if

      if (associated(cpatch%beamext_level)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%beamext_level,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'BEAMEXT_LEVEL :41:hist:anal') 
         call metadata_edio(nvar,igr,'Beam extinction level','[NA]','icohort') 
      end if

      if (associated(cpatch%diffext_level)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%diffext_level,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DIFFEXT_LEVEL :41:hist:anal') 
         call metadata_edio(nvar,igr,'diff extinction level','[NA]','icohort') 
      end if

      if (associated(cpatch%dmean_light_level)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_light_level,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_LIGHT_LEVEL :41:hist:dail') 
         call metadata_edio(nvar,igr,'Diurnal mean of Relative light level ','[NA]','icohort') 
      end if

      if (associated(cpatch%dmean_light_level_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_light_level_beam,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_LIGHT_LEVEL_BEAM :41:hist:dail') 
         call metadata_edio(nvar,igr,'Diurnal mean of Relative light level (beam)','[NA]','icohort') 
      end if

      if (associated(cpatch%dmean_light_level_diff)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_light_level_diff,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_LIGHT_LEVEL_DIFF :41:hist:dail') 
         call metadata_edio(nvar,igr,'Diurnal mean of Relative light level (diffuse)','[NA]','icohort') 
      end if

      if (associated(cpatch%dmean_beamext_level)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_beamext_level,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_BEAMEXT_LEVEL :41:hist:dail') 
         call metadata_edio(nvar,igr,'Diurnal mean of beam extinction level ','[NA]','icohort') 
      end if

      if (associated(cpatch%dmean_diffext_level)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_diffext_level,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_DIFFEXT_LEVEL :41:hist:dail') 
         call metadata_edio(nvar,igr,'Diurnal mean of diff extinction level ','[NA]','icohort') 
      end if

      if (associated(cpatch%mmean_light_level)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_light_level,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_LIGHT_LEVEL :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of Relative light level ','[NA]','icohort') 
      end if

      if (associated(cpatch%mmean_light_level_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_light_level_beam,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_LIGHT_LEVEL_BEAM :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of Relative light level (beam)','[NA]','icohort') 
      end if

      if (associated(cpatch%mmean_light_level_diff)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_light_level_diff,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_LIGHT_LEVEL_DIFF :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of Relative light level (diff)','[NA]','icohort') 
      end if

      if (associated(cpatch%mmean_beamext_level)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_beamext_level,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_BEAMEXT_LEVEL :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Diurnal mean of beam extinction level ','[NA]','icohort') 
      end if

      if (associated(cpatch%mmean_diffext_level)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_diffext_level,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_DIFFEXT_LEVEL :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Diurnal mean of diff extinction level ','[NA]','icohort') 
      end if

      if (associated(cpatch%lambda_light)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%lambda_light,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LAMBDA_LIGHT_CO :41:hist:anal') 
         call metadata_edio(nvar,igr,'Light extinction','[m2/m2]','icohort') 
      end if

      if (associated(cpatch%dmean_lambda_light)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_lambda_light,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_LAMBDA_LIGHT_CO :41:hist:dail') 
         call metadata_edio(nvar,igr,'Diurnal mean of light extinction ','[m2/m2]','icohort') 
      end if

      if (associated(cpatch%mmean_lambda_light)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_lambda_light,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_LAMBDA_LIGHT_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean of light extinction ','[m2/m2]','icohort') 
      end if

      if (associated(cpatch%par_l)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%par_l,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'PAR_L :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%par_l_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%par_l_beam,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'PAR_L_BEAM :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%par_l_diffuse)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%par_l_diffuse,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'PAR_L_DIFFUSE :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%dmean_par_l)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_par_l,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_PAR_L :41:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%dmean_par_l_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_par_l_beam,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_PAR_L_BEAM :41:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%dmean_par_l_diff)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_par_l_diff,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_PAR_L_DIFF :41:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_par_l)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_par_l,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_PAR_L :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_par_l_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_par_l_beam,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_PAR_L_BEAM :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_par_l_diff)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_par_l_diff,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_PAR_L_DIFF :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rshort_l)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rshort_l,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_L :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rshort_l_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rshort_l_beam,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_L_BEAM :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rshort_l_diffuse)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rshort_l_diffuse,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_L_DIFFUSE :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rlong_l)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rlong_l,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_L :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rlong_l_surf)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rlong_l_surf,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_L_SURF :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rlong_l_incid)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rlong_l_incid,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_L_INCID :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rshort_w)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rshort_w,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_W :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rshort_w_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rshort_w_beam,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_W_BEAM :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rshort_w_diffuse)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rshort_w_diffuse,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RSHORT_W_DIFFUSE :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rlong_w)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rlong_w,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_W :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rlong_w_surf)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rlong_w_surf,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_W_SURF :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%rlong_w_incid)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%rlong_w_incid,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'RLONG_W_INCID :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%leaf_gbh)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%leaf_gbh,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LEAF_GBH :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%leaf_gbw)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%leaf_gbw,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LEAF_GBW :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%wood_gbh)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%wood_gbh,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'WOOD_GBH :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%wood_gbw)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%wood_gbw,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'WOOD_GBW :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(cpatch%llspan)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%llspan,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LLSPAN :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if
      
      if (associated(cpatch%A_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%A_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'A_OPEN :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%A_closed)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%A_closed,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'A_CLOSED :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%Psi_closed)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%Psi_closed,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'PSI_CLOSED :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%gsw_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%gsw_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'GSW_OPEN :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%gsw_closed)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%gsw_closed,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'GSW_CLOSED :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%fsw)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%fsw,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'FSW :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%fs_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%fs_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'FS_OPEN :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%water_supply)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%water_supply,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'WATER_SUPPLY :41:hist:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%dmean_fs_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_fs_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_FS_OPEN_CO :41:dail:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%dmean_fsw)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_fsw,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_FSW_CO :41:dail:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%dmean_fsn)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_fsn,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_FSN_CO :41:dail:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%dmean_psi_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_psi_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_PSI_OPEN_CO :41:dail:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%dmean_psi_closed)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_psi_closed,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_PSI_CLOSED_CO :41:dail:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%dmean_water_supply)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%dmean_water_supply,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'DMEAN_WATER_SUPPLY_CO :41:dail:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_fs_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_fs_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_FS_OPEN_CO :41:mont:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_fsw)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_fsw,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_FSW_CO :41:mont:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_fsn)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_fsn,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_FSN_CO :41:mont:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_psi_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_psi_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_PSI_OPEN_CO :41:mont:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_psi_closed)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_psi_closed,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_PSI_CLOSED_CO :41:mont:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_water_supply)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_water_supply,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_WATER_SUPPLY_CO :41:mont:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%stomatal_conductance)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%stomatal_conductance,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'STOMATAL_CONDUCTANCE :41:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%leaf_maintenance)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%leaf_maintenance,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LEAF_MAINTENANCE :41:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%root_maintenance)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%root_maintenance,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'ROOT_MAINTENANCE :41:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_leaf_maintenance)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_leaf_maintenance,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_LEAF_MAINTENANCE :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_root_maintenance)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_root_maintenance,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_ROOT_MAINTENANCE :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%leaf_drop)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%leaf_drop,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LEAF_DROP :41:hist:dail') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%mmean_leaf_drop)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_leaf_drop,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_LEAF_DROP_CO :41:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%bseeds)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%bseeds,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'BSEEDS_CO :41:hist:dail:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%leaf_respiration)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%leaf_respiration,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'LEAF_RESPIRATION :41:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%root_respiration)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%root_respiration,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'ROOT_RESPIRATION :41:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%gpp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%gpp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'GPP :41:hist') 
         call metadata_edio(nvar,igr,'Gross Primary Production','[umol/m2/s]','icohort') 
      end if

      if (associated(cpatch%paw_avg)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%paw_avg,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'PAW_AVG :41:hist:dail:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%elongf)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%elongf,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'ELONGF :41:hist:dail:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%turnover_amp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%turnover_amp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'TURNOVER_AMP :41:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%vm_bar)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%vm_bar,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'VM_BAR :41:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%sla)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%sla,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'SLA :41:hist') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 2-D vectors, with dimension ncohorts and ndcycle. !
      ! Notice that they all use the same npts.  Here you should only add variables of     !
      ! type -41.                                                                          !
      !------------------------------------------------------------------------------------!
      npts = cpatch%ncohorts * ndcycle

      if (associated(cpatch%qmean_par_l)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_par_l,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_PAR_L :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if

      if (associated(cpatch%qmean_par_l_beam)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_par_l_beam,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_PAR_L_BEAM :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if

      if (associated(cpatch%qmean_par_l_diff)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_par_l_diff,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_PAR_L_DIFF :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if

      if (associated(cpatch%qmean_fs_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_fs_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_FS_OPEN_CO :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if

      if (associated(cpatch%qmean_fsw)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_fsw,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_FSW_CO :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if

      if (associated(cpatch%qmean_fsn)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_fsn,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_FSN_CO :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if

      if (associated(cpatch%qmean_psi_open)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_psi_open,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_PSI_OPEN_CO :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if

      if (associated(cpatch%qmean_psi_closed)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_psi_closed,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_PSI_CLOSED_CO :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if

      if (associated(cpatch%qmean_water_supply)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_water_supply,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_WATER_SUPPLY_CO :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if

      if (associated(cpatch%qmean_gpp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_gpp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_GPP_CO :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if

      if (associated(cpatch%qmean_leaf_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_leaf_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_LEAF_RESP_CO :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if

      if (associated(cpatch%qmean_root_resp)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%qmean_root_resp,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'QMEAN_ROOT_RESP_CO :-41:hist:dcyc') 
         call metadata_edio(nvar,igr,'No metadata avaialable','[NA]','NA') 
      end if
!************************************************************************************************** JL,XXT
      if (associated(cpatch%excess_carbon)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%excess_carbon,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'EXCESS_CARBON :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%excess_carbon_fixer)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%excess_carbon_fixer,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'EXCESS_CARBON_FIXER :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%nitrogen_supply_fixer)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%nitrogen_supply_fixer,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'NITROGEN_SUPPLY_FIXER :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%nitrogen_supply)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%nitrogen_supply,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'NITROGEN_SUPPLY :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%shadow_N_uptake)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%shadow_N_uptake,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'SHADOW_N_UPTAKE :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%actual_nitrogen_uptake)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%actual_nitrogen_uptake,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'ACTUAL_NITROGEN_UPTAKE :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%actual_nitrogen_uptake_fixer)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%actual_nitrogen_uptake_fixer,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'ACTUAL_NITROGEN_UPTAKE_FIXER :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%fixation_demand)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%fixation_demand,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'FIXATION DEMAND :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%N_fixation )) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%N_fixation ,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'N_FIXATION :41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%fsn_fixer)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%fsn_fixer,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'FSN_FIXER:41:hist:year:dail:anal:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%nstorage)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%nstorage,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'NSTORAGE :41:hist:year:anal:dail:mont:dcyc') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%carbon_balance)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%carbon_balance,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'CARBON_BALANCE :41:hist:dail:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%N_uptake_pot)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%N_uptake_pot,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'N_UPTAKE_POT :41:hist:dail:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%nitrogen_uptake)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%nitrogen_uptake,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'NITROGEN_UPTAKE :41:hist:dail:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%carbon_balance_pot)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%carbon_balance_pot,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'CARBON_BALANCE_POT :41:hist:dail:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%nitrogen_supply_test)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%nitrogen_supply_test,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'NITROGEN_SUPPLY_TEST :41:hist:dail:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

      if (associated(cpatch%nitrogen_supply_test2)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%nitrogen_supply_test2,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'NITROGEN_SUPPLY_TEST2 :41:hist:dail:anal') 
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
      end if

    ! if (associated(cpatch%N_limitation_factor_bar)) then
    !      nvar=nvar+1
    !        call vtable_edio_r(npts,cpatch%N_limitation_factor_bar,nvar,igr,init,cpatch%coglob_id, &
    !             var_len,var_len_global,max_ptrs,'N_LIMITATION_FACTOR_BAR :41:hist:mont:year:dcyc') 
    !     call metadata_edio(nvar,igr,'Annual average ratio of cb/cb_max','[NA]','NA') 
    ! end if

    !if (associated(cpatch%mmean_nitrogen_supply)) then
    !     nvar=nvar+1
    !     call vtable_edio_r(npts,cpatch%mmean__nitrogen_supply,nvar,igr,init,cpatch%coglob_id, &
    !          var_len,var_len_global,max_ptrs,'MMEAN_NITROGEN_SUPPLY_CO :41:hist:mont:dcyc') 
    !     call metadata_edio(nvar,igr,'Monthly mean NPP sapwood','[kgC/plant/yr]','ipoly') 
    !end if

    !if (associated(cpatch%mmean_N_uptake_pot)) then
    !    nvar=nvar+1
    !    call vtable_edio_r(npts,cpatch%mmean_N_uptake_pot,nvar,igr,init,cpatch%coglob_id, &
    !         var_len,var_len_global,max_ptrs,'MMEAN_N_UPTAKE_POT_CO :41:hist:mont:dcyc') 
    !    call metadata_edio(nvar,igr,'Monthly mean NPP sapwood','[kgC/plant/yr]','ipoly') 
    !end if

    !if (associated(cpatch%excess_carbon)) then
    !     nvar=nvar+1
    !       call vtable_edio_r(npts,cpatch%excess_carbon,nvar,igr,init,cpatch%coglob_id, &
    !       var_len,var_len_global,max_ptrs,'EXCESS_CARBON :41:hist:dail') 
    !     call metadata_edio(nvar,igr,'No metadata available','[NA]','NA') 
    ! end if
!JL,XXT
!************************************************************************************************** 
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 2-D vectors, with dimension ncohorts and mortal-  !
      ! ity types months.  Notice that they all use the same npts.  Here you should only add vari-   !
      ! ables of type 48.                                                                  !
      !------------------------------------------------------------------------------------!
      npts = cpatch%ncohorts * n_mort

      if (associated(cpatch%mort_rate)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mort_rate,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MORT_RATE_CO :48:hist:dail') 
         call metadata_edio(nvar,igr,'Mortality rates','[1/yr]','icohort') 
      end if

      if (associated(cpatch%mmean_mort_rate)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%mmean_mort_rate,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'MMEAN_MORT_RATE_CO :48:hist:mont:dcyc') 
         call metadata_edio(nvar,igr,'Monthly mean mortality rate','[1/yr]','icohort') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 2-D vectors, with dimension ncohorts and 13       !
      ! months.  Notice that they all use the same npts.  Here you should only add vari-   !
      ! ables of type 49.                                                                  !
      !------------------------------------------------------------------------------------!
      npts = cpatch%ncohorts * 13


      if (associated(cpatch%cb)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%cb,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'CB :49:hist:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'carbon balance previous 12 months+current','[kgC/plant]','13 - icohort') 
      end if
!JL!
     !if (associated(cpatch%N_limitation_factor)) then
     !     nvar=nvar+1
     !       call vtable_edio_r(npts,cpatch%N_limitation_factor,nvar,igr,init,cpatch%coglob_id, &
     !       var_len,var_len_global,max_ptrs,'N_LIMITATION_FACTOR :49:hist:mont:dcyc:year') 
     !     call metadata_edio(nvar,igr,'carbon balance previous 12 months+current','[kgC/plant]','13 - icohort') 
     !end if
!JL!
      if (associated(cpatch%cb_max)) then
         nvar=nvar+1
           call vtable_edio_r(npts,cpatch%cb_max,nvar,igr,init,cpatch%coglob_id, &
           var_len,var_len_global,max_ptrs,'CB_MAX :49:hist:mont:dcyc:year') 
         call metadata_edio(nvar,igr,'TOC carbon balance previous 12 months+current','[kgC/plant]','13 - icohort') 
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!





      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!
      !       This part should have only 2-D vectors, with dimension n_stoma_atts and      !
      ! ncohorts.  Notice that they all use the same npts.  Here you should only add vari- !
      ! ables of type 416.                                                                 !
      !------------------------------------------------------------------------------------!
      npts = cpatch%ncohorts * 16

      if (associated(cpatch%old_stoma_vector)) then
         nvar=nvar+1
         call vtable_edio_r(npts,cpatch%old_stoma_vector,nvar,igr,init,cpatch%coglob_id, &
              var_len,var_len_global,max_ptrs,'OLD_STOMA_VECTOR :416:hist')
         call metadata_edio(nvar,igr,'No metadata available','[NA]','NA')
      end if
      !------------------------------------------------------------------------------------!
      !------------------------------------------------------------------------------------!

      return
   end subroutine filltab_patchtype
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
                   ! ========= UTILITITY FUNCTIONS =========== !
!==========================================================================================!
!==========================================================================================!
  function get_nsites(cgrid)

    implicit none
    integer :: get_nsites
    integer :: ipy,isi
    type(edtype),target           :: cgrid
    type(polygontype),pointer     :: cpoly

    get_nsites = 0

    do ipy=1,cgrid%npolygons
       cpoly=>cgrid%polygon(ipy)
       do isi=1,cpoly%nsites
          get_nsites = get_nsites + 1
       end do
    end do

    return
  end function get_nsites
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
  function get_npatches(cgrid)

    implicit none
    integer :: get_npatches
    integer :: ipy,isi,ipa
    type(edtype),target           :: cgrid
    type(polygontype),pointer     :: cpoly
    type(sitetype),pointer        :: csite

    get_npatches = 0

    do ipy=1,cgrid%npolygons
       cpoly=>cgrid%polygon(ipy)
       do isi=1,cpoly%nsites
          csite=>cpoly%site(isi)
          do ipa=1,csite%npatches
             get_npatches = get_npatches + 1
          end do
       end do
    end do
    return
  end function get_npatches
!==========================================================================================!
!==========================================================================================!






!==========================================================================================!
!==========================================================================================!
  function get_ncohorts(cgrid)

    implicit none
    integer :: get_ncohorts
    integer :: ipy,isi,ipa,ico
    type(edtype),target           :: cgrid
    type(polygontype),pointer     :: cpoly
    type(sitetype),pointer        :: csite
    type(patchtype),pointer       :: cpatch

    get_ncohorts = 0

    do ipy=1,cgrid%npolygons
       cpoly=>cgrid%polygon(ipy)
       do isi=1,cpoly%nsites
          csite=>cpoly%site(isi)
          do ipa=1,csite%npatches
             cpatch=>csite%patch(ipa)
             get_ncohorts = get_ncohorts + cpatch%ncohorts
          end do
       end do
    end do
    return
end function get_ncohorts
!==========================================================================================!
!==========================================================================================!
end module ed_state_vars
