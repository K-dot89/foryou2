program a3q1
    implicit none
    integer,parameter::n=6
    real::x(n),y(n,n)
    real::x_target,h,p,term,y_est
    integer::i,j

    open(unit=10,file="in3q1.txt")
    open(unit=20,file="out3q1.txt")

    read(10,*)x_target

    do i=1,n
        read(10,*)x(i),y(i,1)
    end do

    !Construct forward difference table
    do j=2,n
        do i=1,n-j+1
            y(i,j)=y(i+1,j-1)-y(i,j-1)
        end do
    end do

    write(20,'(A6,6A10)')"x","y","Dy","D2y","D3y","D4y","D5y"

    do i=1,n
        write(20,'(F6.0)',advance='no')x(i)
        do j=1,n-i+1
            write(20,'(F10.4)',advance='no')y(i,j)
        end do
        write(20,*)
    end do

    h=x(2)-x(1)
    p=(x_target-x(1))/h
    y_est=y(1,1)
    term=1.0

    do j=1,n-1
        term=term*(p-real(j-1))/real(j)
        y_est=y_est+term*y(1,j+1)
    end do
    write(20,*)
    write(20,'(A,F6.1,A,F10.4,A)')"Estimated GDP in the year ",x_target," = ",y_est," billion USD"

end program a3q1

program a3q2
    implicit none
    integer, parameter :: n = 5
    real :: x(n), y(n, n)
    real :: x_target, h, p, term, y_est
    integer :: i, j

    open(unit=10, file="in3q2.txt", status="old", action="read")
    open(unit=20, file="out3q2.txt", status="replace", action="write")

    read(10, *) x_target

    do i = 1, n
        read(10, *) x(i), y(i, 1)
    end do
    close(10)

    ! Construct backward difference table
    ! Initialize matrix to zero first
    y = 0.0
    do j = 2, n
        do i = n, j, -1
            y(i, j) = y(i, j - 1) - y(i - 1, j - 1)
        end do
    end do

    ! Table Header
    write(20, '(A8, 5A12)') "x", "y", "Nab_y", "Nab2_y", "Nab3_y", "Nab4_y"

    ! Write table rows (safely looping only up to available entries per row)
    do i = 1, n
        write(20, '(F8.1)', advance='no') x(i)
        do j = 1, i
            write(20, '(F12.2)', advance='no') y(i, j)
        end do
        write(20, *)
    end do

    h = x(2) - x(1)
    p = (x_target - x(n)) / h
    y_est = y(n, 1)
    term = 1.0

    ! Newton's Backward Difference Formula
    do j = 1, n - 1
        term = term * (p + real(j - 1)) / real(j)
        y_est = y_est + term * y(n, j + 1)
    end do

    write(20, *)
    write(20, '(A, F5.1, A, F10.2, A)') "Estimated maturity value for ", x_target, " years = Tk. ", y_est

    ! Close file to force output buffer flush
    close(20)

    print *, "Program executed successfully. Check out3q2.txt."
end program a3q2


program a3q3
    implicit none
    integer, parameter :: n = 5
    real :: x(n), y(n, n)
    real :: x_target, product_term, y_est
    integer :: i, j

    ! Open input file
    open(unit=10, file="in3q3.txt", status="old", action="read")

    ! Read target time (3.0 hours)
    read(10, *) x_target

    ! Read x and y values
    do i = 1, n
        read(10, *) x(i), y(i, 1)
    end do
    close(10)

    ! Construct Divided Difference Table
    ! Formula: f[x_i, ..., x_{i+j-1}] = (f[x_{i+1}, ..., x_{i+j-1}] - f[x_i, ..., x_{i+j-2}]) / (x_{i+j-1} - x_i)
    y = 0.0
    do j = 2, n
        do i = 1, n - j + 1
            y(i, j) = (y(i + 1, j - 1) - y(i, j - 1)) / (x(i + j - 1) - x(i))
        end do
    end do

    ! Open output file
    open(unit=20, file="out3q3.txt", status="replace", action="write")

    ! Write table header
    write(20, '(A8, 5A12)') "x", "f(x)", "DD1", "DD2", "DD3", "DD4"

    ! Write Divided Difference Table
    do i = 1, n
        write(20, '(F8.1)', advance='no') x(i)
        do j = 1, n - i + 1
            write(20, '(F12.4)', advance='no') y(i, j)
        end do
        write(20, *)
    end do

    ! Apply Newton's Divided Difference Interpolation Formula
    y_est = y(1, 1)
    product_term = 1.0

    do j = 1, n - 1
        product_term = product_term * (x_target - x(j))
        y_est = y_est + product_term * y(1, j + 1)
    end do

    write(20, *)
    write(20, '(A, F4.1, A, F10.4, A)') "Estimated exchange rate at x = ", x_target, " hours = ", y_est, " Tk./USD"

    ! Close output file to write data to disk
    close(20)

    print *, "Finished successfully. Check out3q3.txt!"
end program a3q3

