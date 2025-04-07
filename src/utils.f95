module utils
use regime	
use contains
implicit none


integer, parameter 				:: section_info_str_len = 50
character(section_info_str_len)	:: section_info_str 
real							:: time_start, time_stop 
	
contains


subroutine print_section_info(str, start, stop, flag)
	character(section_info_str_len), intent(in)	:: str
	integer, intent(in)							:: flag

	character(section_info_str_len)				:: done
	integer										:: n, i

	real, intent(inout)							:: start, stop

	if (flag == 0) then
		write(*,"('#', 1x, A, 1x)", advance="no") trim(str)
		call cpu_time(start)
	else if (flag == 1) then
		call cpu_time(stop)
		n = section_info_str_len - len(" ") - len(trim(str)) !- len(" [Done:")

		done = ""
		do i = 1, n+1
			done = trim(done) // "."
		end do 
		! done = trim(done) // " [Done:"
		write(*,"(A, A, 1x, f7.3, 1x, A)") trim(done), "... [Done:", stop-start, "s]"
	end if
	 
end subroutine print_section_info

subroutine command_arg_parser(a_min, a_max, p, rho_1, rho_2)
	real(knd), intent(inout)	:: a_min, a_max, p, rho_1, rho_2

	integer						:: i 
	character(128)				:: arg, a_min_str, a_max_str, p_str, rho_1_str, rho_2_str

	do i = 1, command_argument_count(), 2
		call get_command_argument(i, arg)
		if (trim(arg) == "--amin") then
			call get_command_argument(i+1, a_min_str) 
			read(a_min_str, *) a_min
		else if (trim(arg) == '--amax') then
			call get_command_argument(i+1, a_max_str) 
			read(a_max_str, *) a_max
		else if (trim(arg) == '--p') then
			call get_command_argument(i+1, p_str) 
			read(p_str, *) p
		else if (trim(arg) == '--rho1') then
			call get_command_argument(i+1, rho_1_str) 
			read(rho_1_str, *) rho_1
		else if (trim(arg) == '--rho2') then
			call get_command_argument(i+1, rho_2_str) 
			read(rho_2_str, *) rho_2
			! else
		!     call assert(.false., 'unknown parameter '//trim(arg))
		end if
	enddo

end subroutine command_arg_parser


subroutine read_input_namelist(file_path, a_min, a_max, p, rho_si, rho_c) 

	character(128), intent(in)	:: file_path

	integer						:: input_file

	real(knd), intent(out)		:: a_min, a_max, p, rho_si, rho_c


	namelist /config/ a_min, a_max, p, rho_si, rho_c

	input_file = 25

	open(input_file, file="../"//trim(file_path))
	read(input_file, config)
	close(input_file)

end subroutine read_input_namelist

subroutine read_input_table(file_path, na, nw)

	character(128), intent(in)	:: file_path

	integer						:: input_file

	integer, intent(out)		:: na, nw


	input_file = 35
	open(input_file, file="../"//trim(file_path))

	read(input_file, *)
	read(input_file, *)
	read(input_file, *)

	read(input_file, *) na
	read(input_file, *) nw

	close(input_file)

end subroutine read_input_table


subroutine read_table_Q(file_path, a_arr, w_arr, Q_abs, Q_sca)

	character(128), intent(in)	:: file_path

	integer						:: input_file, ia, iw, na, nw

	real(knd), intent(out)		:: a_arr(:), w_arr(:), Q_abs(:,:), Q_sca(:,:)

	
	input_file = 45
	open(input_file, file="../"//trim(file_path))

	read(input_file, *)
	read(input_file, *)
	read(input_file, *)

	read(input_file, *) na
	read(input_file, *) nw

	! allocate(a_arr(na), w_arr(nw), Q_abs(na, nw), Q_sca(na, nw))

	do ia = 1, na
	! print*, "aaaaaaaa", ia
		read(input_file, *)
		read(input_file, *) a_arr(ia)
		read(input_file, *)

		do iw = 1, nw
			read(input_file, *) w_arr(iw), Q_abs(ia, iw), Q_sca(ia, iw)
		enddo
	enddo

	close(input_file)

	! to cgs
	a_arr = a_arr * 1q-4
	w_arr = w_arr * 1q-4

end subroutine read_table_Q


function lambda2nu(lambda) result(nu)
	real(knd), intent(in) 	:: lambda(:)

	real(knd), allocatable	:: nu(:)

	allocate(nu(size(lambda)))

	nu = c_light / lambda
	
end function lambda2nu


subroutine save_coefs_nu(file_path, rho_s, w_arr, kappa, sigma)

	character(128), intent(in)	:: file_path
	real(knd), intent(in)		:: w_arr(:), kappa(:), sigma(:), rho_s

	character(25)				:: body_format, header_format
	integer						:: output_file, i
	real(knd), allocatable		:: freq_arr(:)


	output_file = 55

	open(output_file, file="../"//trim(file_path))

	if (knd == 8) then
		body_format = "(1x, i4, 3(1x, e23.14))"
		header_format = "(a5, 3(a24))"
	else if (knd == 16) then
		body_format = "(1x, i4, 3(1x, e39.30))"
		header_format = "(a5, 3(a40))"
	end if

	if (rho_s < 0) then
		write(output_file, "('# substance density: MIX [g/cm3]')")
	else
		write(output_file, "('# substance density: ', f4.2, ' [g/cm3]')") rho_s
	end if
	write(output_file, header_format) "n", "freq[1/s]", "kappa(abs)[cm2/g]", "sigma(sca)[cm2/g]"

	allocate(freq_arr(size(w_arr)))
	freq_arr = lambda2nu(w_arr)

	do i = 1, size(w_arr)
		write(output_file, body_format) i, freq_arr(i), kappa(i), sigma(i)
	end do

	close(output_file)
	deallocate(freq_arr)

end subroutine save_coefs_nu

subroutine save_coefs_T(file_path, rho_s, T_arr, kappa_P, kappa_R)

	character(128), intent(in)	:: file_path
	real(knd), intent(in)		:: T_arr(:), kappa_P(:), kappa_R(:), rho_s

	character(25)				:: body_format, header_format
	character(128)				:: g2d_info
	integer						:: output_file, i, g2d
	real(knd), allocatable		:: freq_arr(:)

	g2d = 100
	g2d_info = " !opacity coefficients are in cm^2 per gram of GAS assuming dust-to-gas mass ratio 0.01"

	output_file = 65

	open(output_file, file="../"//trim(file_path))

	if (knd == 8) then
		body_format = "(1x, i5, 3(1x, e23.14))"
		header_format = "(a6, 3(a24))"
	else if (knd == 16) then
		body_format = "(1x, i5, 3(1x, e39.30))"
		header_format = "(a6, 3(a40))"
	end if

	if (rho_s < 0) then
		write(output_file, "('# substance density: MIX [g/cm3]', A)") g2d_info
	else
		write(output_file, "('# substance density: ', f4.2, ' [g/cm3]', A)") rho_s, g2d_info
	end if
	write(output_file, header_format) "n", "temperature[K]", "kappa_P[cm2/g]", "kappa_R[cm2/g]"


	do i = 1, size(T_arr)
		write(output_file, body_format) i, T_arr(i), kappa_P(i)/g2d, kappa_R(i)/g2d
	end do

	close(output_file)

end subroutine save_coefs_T


real(knd) function integr_trap(f0, x0) 
	real(knd), intent(in)	:: f0(:), x0(:)

	real(knd), allocatable	:: f(:), x(:)

	allocate(f(size(f0)), x(size(f0)))

	f = f0 
	x = x0

	integr_trap = sum((f(2:)+f(:size(f)-1)) / 2 * (x(2:)-x(:size(f)-1)))

	deallocate(f, x)

end function integr_trap
	
end module utils


