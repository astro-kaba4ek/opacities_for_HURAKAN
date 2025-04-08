module opacity_mod
use regime
use contains
use utils
implicit none
	
contains






function const_from_PSDfunc_norm(a_max, a_min, p, rho_s) result(const)
	real(knd), intent(in)	:: a_max, a_min, p, rho_s

	real(knd)				:: a1, a0

	real(knd)				:: const

	! a1 = a_max**(4-p) / (4 - p)
	! a0 = a_min**(4-p) / (4 - p)

	! ! const = (3._knd / 4._knd / PI) * (rho_d / rho_s) / (a1 - a0)
	! const = 3._knd / 4._knd / rho_s / (a1 - a0)

	const = 3._knd / 4._knd / rho_s / (a_max**(4-p) - a_min**(4-p)) * (4 - p)
	
end function const_from_PSDfunc_norm



function mass_coef_arr(p, a_min, a_max, rho_s, a_arr, Q) result(coef)

	real(knd), intent(in)	:: p, a_min, a_max, rho_s, a_arr(:), Q(:,:)

	integer					:: na, nw, iw, ia, ia_start, ia_stop, aver_start_flag, aver_stop_flag, new_len
	real(knd), allocatable	:: aa(:), QQ(:,:)
	logical					:: ia_start_flag, ia_stop_flag
	real(knd)				:: summ

	real(knd), allocatable	:: coef(:)


	na = size(a_arr)
	nw = size(Q(1,:))

	ia_start_flag = .False.
	ia_stop_flag = .False.
	ia_start = 1
	ia_stop = na
	aver_start_flag = 0
	aver_stop_flag = 0
	

	do ia = 1, na

		if (.not. ia_start_flag) then
			if (a_min == a_arr(ia)) then
				ia_start = ia 
				ia_start_flag = .True.
				aver_start_flag = 0
			else if (a_min > a_arr(ia) .and. a_min < a_arr(ia+1)) then
				ia_start = ia
				ia_start_flag = .True.
				aver_start_flag = 1
			end if
		else
			if (a_max == a_arr(ia)) then
				ia_stop = ia 
				ia_stop_flag = .True.
				aver_stop_flag = 0
			else if (a_max > a_arr(ia) .and. a_max < a_arr(ia+1)) then
				ia_stop = ia
				ia_stop_flag = .True.
				aver_stop_flag = 1
			end if
		end if 

		if (ia_stop_flag) exit
	end do 

	
	new_len = (ia_stop - ia_start + 1)

	allocate(aa(1-aver_start_flag : new_len+aver_stop_flag), QQ(1-aver_start_flag : new_len+aver_stop_flag, nw))

	aa(1-aver_start_flag) = a_min
	aa(new_len+aver_stop_flag) = a_max
	aa(1:new_len) = a_arr(ia_start:ia_stop)


	QQ(1-aver_start_flag, :) = (Q(ia_start, :)+Q(ia_start+1, :)) / 2
	QQ(new_len+aver_stop_flag, :) = (Q(ia_stop, :)+Q(ia_stop+1, :)) / 2
	QQ(1:new_len, :) = Q(ia_start:ia_stop, :)


	allocate(coef(nw))

	do iw = 1, nw
		coef(iw) = integr_trap(aa**(-p+2)*QQ(:,iw), aa) 
	end do

	coef = const_from_PSDfunc_norm(a_max, a_min, p, rho_s) * coef

	deallocate(aa, QQ)

end function mass_coef_arr


function coef_arr_mix(coef_1, coef_2, frac_1) result(coef_mix)

	real(knd), intent(in)	:: coef_1(:), coef_2(:), frac_1

	real(knd), allocatable	:: coef_mix(:)


	allocate(coef_mix(size(coef_1)))

	coef_mix = frac_1 * coef_1 + (1-frac_1) * coef_2

end function coef_arr_mix


function coef_arr_mix_auto(p, a_min, a_max, rho_s_1, rho_s_2, a_arr, Q_1, Q_2, frac_1) result(coef_mix)

	real(knd), intent(in)	:: p, a_min, a_max, rho_s_1, rho_s_2, a_arr(:), Q_1(:,:), Q_2(:,:), frac_1

	real(knd), allocatable	:: coef_1(:), coef_2(:)

	real(knd), allocatable	:: coef_mix(:)


	allocate(coef_mix(size(Q_1(1,:))), coef_1(size(Q_1(1,:))), coef_2(size(Q_1(1,:))))

	coef_1 = mass_coef_arr(p, a_min, a_max, rho_s_1, a_arr, Q_1) 
	coef_2 = mass_coef_arr(p, a_min, a_max, rho_s_2, a_arr, Q_2) 

	coef_mix = frac_1 * coef_1 + (1-frac_1) * coef_2

	deallocate(coef_1, coef_2)

end function coef_arr_mix_auto


subroutine coef_arr_P_R(kappa, sigma, freq, T_arr, kappa_P, kappa_R)

	real(knd), intent(in)	:: kappa(:), sigma(:), freq(:), T_arr(:)

	integer					:: iT
	real(knd), allocatable	:: x(:), und_int(:) 
	real(knd)				:: const
	
	real(knd), intent(out)	:: kappa_P(:), kappa_R(:)


	allocate(x(size(freq)), und_int(size(freq)))

	do iT = 1, size(T_arr)

		x = h_Pl * freq / k_B / T_arr(iT)

		! --------------------------------

		const = 15 * (h_Pl / T_arr(iT) / PI / k_B)**4
		kappa_P(iT) = const * integr_trap(kappa * freq**3 / (exp(x)-1), freq) 

		! --------------------------------

		const = 2 * h_Pl**2 / c_light**2 / k_B / T_arr(iT)**2
		und_int = const * freq**4 * (1/(exp(x)-1) + 1/(exp(x)-1)**2)
		kappa_R(iT) = integr_trap(und_int, freq) / integr_trap(und_int / (kappa + sigma), freq) 

	end do

	deallocate(x, und_int)

end subroutine coef_arr_P_R

subroutine coef_arr_P_R_auto(p, a_min, a_max, rho_s_1, rho_s_2, a_arr, &
								Q_abs_1, Q_abs_2, Q_sca_1, Q_sca_2, frac_1, freq, T_arr, kappa_P, kappa_R) 
	
	real(knd), intent(in)	:: p, a_min, a_max, rho_s_1, rho_s_2, a_arr(:), &
								Q_abs_1(:,:), Q_abs_2(:,:), Q_sca_1(:,:), Q_sca_2(:,:), frac_1, freq(:), T_arr(:)

	real(knd), allocatable	:: kappa(:), sigma(:) 
	
	real(knd), intent(out)	:: kappa_P(:), kappa_R(:)

	allocate(kappa(size(freq)), sigma(size(freq)))

	kappa = coef_arr_mix_auto(p, a_min, a_max, rho_s_1, rho_s_2, a_arr, Q_abs_1, Q_abs_2, frac_1)
	sigma = coef_arr_mix_auto(p, a_min, a_max, rho_s_1, rho_s_2, a_arr, Q_sca_1, Q_sca_2, frac_1)

	call coef_arr_P_R(kappa, sigma, freq, T_arr, kappa_P, kappa_R)

	deallocate(kappa, sigma)

end subroutine coef_arr_P_R_auto


	

end module opacity_mod