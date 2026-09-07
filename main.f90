
program a3q4
    implicit none
    integer :: n, i, j
    real :: x_target, exact_val, approx_val, abs_error
    real, allocatable :: x(:), y(:), L(:)

    ! Open input file
    open(unit=10, file="in3q4.txt", status="old", action="read")

    ! Read target evaluation point (1.25)
    read(10, *) x_target

    ! Read number of points (5)
    read(10, *) n

    allocate(x(n))
    allocate(y(n))
    allocate(L(n))

    ! Read x values
    read(10, *) (x(i), i = 1, n)
    close(10)

    ! (a) Calculate function values y = f(x) = 0.3 * x * exp(0.15 * x**2)
    do i = 1, n
        y(i) = 0.3 * x(i) * exp(0.15 * (x(i)**2))
    end do

    ! (b) Apply Lagrange's Interpolation Formula
    approx_val = 0.0
    do i = 1, n
        L(i) = 1.0
        do j = 1, n
            if (i /= j) then
                L(i) = L(i) * (x_target - x(j)) / (x(i) - x(j))
            end if
        end do
        approx_val = approx_val + L(i) * y(i)
    end do

    ! (c) Calculate exact value of f(1.25)
    exact_val = 0.3 * x_target * exp(0.15 * (x_target**2))

    ! (d) Calculate absolute error
    abs_error = abs(approx_val - exact_val)

    ! Open output file
    open(unit=20, file="out3q4.txt", status="replace", action="write")

    write(20, '(A)') "=========================================================="
    write(20, '(A)') "             LAGRANGE INTERPOLATION METHOD                "
    write(20, '(A)') "=========================================================="
    write(20, '(A)') "(a) Function values at given interpolation points:"
    write(20, '(A10, A16)') "x_i", "f(x_i)"
    write(20, '(A26)') "--------------------------"
    do i = 1, n
        write(20, '(F10.2, F16.6)') x(i), y(i)
    end do

    write(20, '(A)') "----------------------------------------------------------"
    write(20, '(A, F6.2, A, F10.6)') "(b) Interpolated Value  f(", x_target, ") = ", approx_val
    write(20, '(A, F6.2, A, F10.6)') "(c) Exact Value         f(", x_target, ") = ", exact_val
    write(20, '(A, F10.6)')          "(d) Absolute Error                = ", abs_error
    write(20, '(A)') "=========================================================="

    ! Close output file to write data to disk
    close(20)

    print *, "Finished successfully. Check out3q4.txt!"

    deallocate(x, y, L)
end program a3q4
