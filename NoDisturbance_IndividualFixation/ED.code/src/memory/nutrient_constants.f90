! Defines constants used in nitrogen cycle 

module nutrient_constants

  !! Constants for Disturbance settings
  integer, parameter :: dist_start = 0 !50 ! year disturbance starts
  integer, parameter :: dist_end = 0 !100  ! year disturbance reaches max_treefall_distrubance_rate
  real, parameter :: max_treefall_disturbance_rate = 0 !0.008 !  1/years

  !!Constants for N inputs and losses
  real, parameter :: BNF_rate= 0.0003941912             !kg N fixed/total tree biomass(above and belowground)/ day
                                                        !NOTE: 1.784159e-05 is the mean observed rate at Sarah Batterman's field site (Agua Salud)
  real, parameter :: Cost_BNF = 9.12                    !( 9.12 g C g N -1), (gutschick, 1981) metabolic and nodule maintence cost
  real, parameter :: Cost_fixer = 0                     !( g C gN -1), additional cost for being a fixer
  real, parameter :: resorption_factor = 0.48           !0.48% (McGroddy et al. 2004). 
  real, parameter :: N_deposition = 0.0009/365          !(kg N/m2/day) (Hedin et al.2009)
  real, parameter :: Asymbiotic_N_Fixation = 0          !0.0004/365  !(kg N/m2/day )  
  real, parameter :: leaching_efficiency_factor = 0.05  !fraction of N that can be lost by leaching from mineralized_soil_N 
  real, parameter :: DON_loss    = 0.0001/365           !Dissolved Organic N loss, about 1/5 of the total N deposition
  real, parameter :: soil_depth = 1.5                   !used to calcualte leaching (meters) 1.5 from (Jackson 1996)
  real, parameter :: nstorage_max_factor = 1.33         ! Maximum storage capacity of Nitrogen = N in leaves*nstorage_max_factor
  real, parameter :: soil_layers = 9                    ! number of soil layers to use in leaching calculations. Should be determined by rooting depth.
  real, parameter :: max_rooting_layer = 6              ! lowest layer in soil column used to compute mineralized N leaching 
  real, parameter :: gas_loss_rate = 0.2                ! Gas loss is approximately 20% of total N loss if the site has
                                                        ! 2700 mm rain/yr or less but is about 50% gas loss if >2700 mm rain/yr (Houlton et al 2006 PNAS) 


end module nutrient_constants
