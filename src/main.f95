program opacity
use regime
use utils
use opacity_mod
implicit none


integer 		:: na, nw, i, input_file, nT
real(knd)		:: a_min, a_max, p, rho_si, rho_c, weight_si
character(3)	:: sca_flag
character(128)	:: file_path

real(knd), allocatable:: a_arr(:), w_arr(:), Q_abs_asil(:,:), Q_sca_asil(:,:), Q_abs_gra(:,:), Q_sca_gra(:,:)
real(knd), allocatable:: kappa_mix(:), sigma_mix(:)
real(knd), allocatable:: T_arr(:), kappaP_mix(:), kappaR_mix(:)

write(*,*) ""


section_info_str = "Reading input files"
call print_section_info(section_info_str, time_start, time_stop, 0)

file_path = "config.txt"
call read_input_namelist(file_path, a_min, a_max, p, rho_si, rho_c, weight_si, sca_flag)
call command_arg_parser(a_min, a_max, p, rho_si, rho_c, weight_si, sca_flag)

file_path = "data/aSil.txt"
call read_input_table(file_path, na, nw)
allocate(a_arr(na), w_arr(nw), Q_abs_asil(na, nw), Q_sca_asil(na, nw))
call read_table_Q(file_path, a_arr, w_arr, Q_abs_asil, Q_sca_asil)

file_path = "data/Gra.txt"
call read_input_table(file_path, na, nw)
allocate(Q_abs_gra(na, nw), Q_sca_gra(na, nw))
call read_table_Q(file_path, a_arr, w_arr, Q_abs_gra, Q_sca_gra)

call print_section_info(section_info_str, time_start, time_stop, 1)
call print_input_pars(a_min, a_max, p, rho_si, rho_c, weight_si, sca_flag)


section_info_str = "Calculation of opacities"
call print_section_info(section_info_str, time_start, time_stop, 0)

allocate(kappa_mix(nw), sigma_mix(nw))
! weight_si = 0.8

kappa_mix = coef_arr_mix_auto(p, a_min, a_max, rho_si, rho_c, a_arr, Q_abs_asil, Q_abs_gra, weight_si)
if (trim(sca_flag) == "yes") then
	sigma_mix = coef_arr_mix_auto(p, a_min, a_max, rho_si, rho_c, a_arr, Q_sca_asil, Q_sca_gra, weight_si)
else if (trim(sca_flag) == "no") then
	sigma_mix = 0
end if 

file_path = "res/mix.txt"
! call save_coefs_nu(file_path, -1._knd, w_arr, kappa_mix, sigma_mix)
! call save_coefs_nu_likeHURAKAN(file_path, -1._knd, w_arr(216::2), kappa_mix(216::2), sigma_mix(216::2))
call save_coefs_nu_likeHURAKAN(file_path, -1._knd, w_arr(::2), kappa_mix(::2), sigma_mix(::2))
deallocate(a_arr, Q_abs_asil, Q_sca_asil, Q_abs_gra, Q_sca_gra)

call print_section_info(section_info_str, time_start, time_stop, 1)




section_info_str = "Calculation of Planck and Rosseland opacities"
call print_section_info(section_info_str, time_start, time_stop, 0)

nT = 10000
allocate(T_arr(nT), kappaP_mix(nT), kappaR_mix(nT))
T_arr = [real(knd) :: (i, i=1, nT)]

call coef_arr_P_R(kappa_mix, sigma_mix, lambda2nu(w_arr), T_arr, kappaP_mix, kappaR_mix)
file_path = "res/PRmix.txt"
! call save_coefs_T(file_path, -1._knd, T_arr, kappaP_mix, kappaR_mix)
call save_coefs_T_likeHURAKAN(file_path, -1._knd, T_arr, kappaP_mix, kappaR_mix)

call print_section_info(section_info_str, time_start, time_stop, 1)



deallocate(w_arr, kappa_mix, sigma_mix, T_arr, kappaP_mix, kappaR_mix)





end program opacity
