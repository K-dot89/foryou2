program as4q1
    implicit none
    logical:: F, B, C
    logical:: R1, R2
    logical:: is_equivalent
    integer:: i, j, k
    logical, dimension(2):: tf_values=(/.true., .false./)

    open(unit=10, file='out4q1.txt', status='replace')

    write(10, '(A)')'======================================================='
    write(10, '(A)')'TRUTH TABLE FOR HOSPITAL RULE (R1) AND DOCTOR RULE (R2)'
    write(10, '(A)')'======================================================='
    write(10, '(A)')'     Fever(F)| Breathing(B)| Chest Pain(C) |   R1    |     R2     '
    write(10, '(A)')'------------------------------------------------------------------'

    is_equivalent = .true.

        do i=1, 2
            F = tf_values(i)
            do j = 1, 2
                B = tf_values(j)
                do k = 1, 2
                    C = tf_values(k)

                    R1 = (F .and. B) .or. (B .and. C) .or. (F .and. C)
                    R2 = (F .and. B) .or. (C .and.(F .or. B))

                    write(10, '(5(L8, 6X))') F, B, C, R1, R2

                    if (R1 .neqv. R2) then
                        is_equivalent = .false.
                    end if
                end do
            end do
        end do

        write(10, '(A)')'--------------------------------------------------------------'

        if(is_equivalent)then
            write(10,'(A)')'Conclusion: Both rules (R1 and R2) are logically equivalent.'
        else
            write(10,'(A)')'Conclusion: The two rules are not logically equivalent.'
        end if
        write(10,'(A)')'==========================================================='

        close(10)
        print*, 'Program successful.'
        end program as4q1

program as4q2
    implicit none
    logical:: W, H, A, B, IFF, is_tautology = .true.
    integer:: i, j
    logical, dimension(2):: tf=(/.true., .false./)

    open(unit=10, file='out4q2.txt', status='replace')
    write(10,'(A)')'===================================================='
    write(10,'(A)')' Truth table for thermostat rules (A <-> B) '
    write(10,'(A)')'===================================================='
    write(10,'(A)')' Warm(W) | Home(A) | Version A | Version B | A <-> B'
    write(10,'(A)')'----------------------------------------------------'

    do i=1, 2
        do j= 1, 2
            W = tf(i)
            H = tf(j)

            A= W .and. H
            B= .not.(.not. W .or. .not. H)

            iff= (A .eqv. B)

            if(.not. iff) is_tautology = .false.

            write(10, '(2(L8, 4X),3(L9, 3X))') W, H, A, B, iff


            end do
            end do

            Write(10, '(A)')'------------------------------------------------'
                if(is_tautology) then
                    write(10, '(A)')'Conclusion: (A) <-> (B) is Tautology'
                    else
                    write(10, '(A)')'Conclusion: (A) <-> (B) is not Tautology'
             end if
             write(10, '(A)') '-----------------------------------------------'

             close(10)

             print*, 'program successful.'
             end program as4q2

program as4q3
    implicit none

    logical:: p, q, imp, bicond, rhs, is_equivalent = .true.
    integer:: i , j
    logical, dimension(2):: tf = (/.true., .false./)

    open(unit=10, file='out4q3.txt', status='replace')

    write(10, '(A)')'============================================================'
    write(10, '(A)')'Truth table for implication, biconditional, and equivalence'
    write(10, '(A)')'============================================================'
    write(10, '(A)')' p | q | p -> q | p <-> q | (p AND q) OR (NOT p AND NOT q) '
    write(10, '(A)')'------------------------------------------------------------'

    do  i = 1, 2
        do j = 1, 2
            p = tf(i)
            q = tf(j)

            imp = (.not. p) .or. q

            bicond = (p .eqv. q)

            rhs = (p .and. q) .or. ((.not. p) .and. (.not. q))

            if (bicond .neqv. rhs) is_equivalent = .false.

            write (10, '(5(L8, 3X))') p, q, imp, bicond, rhs

        end do
    end do
     Write(10, '(A)')'------------------------------------------------'
                if(is_equivalent) then
                    write(10, '(A)')'Conclusion:  p <-> q is logically equivalent to (p And q ) OR ( NOT p AND NOT q). '
                    else
                    write(10, '(A)')'Conclusion: The two expressions are not equivalent.'
             end if
             write(10, '(A)') '-----------------------------------------------'

             close(10)

             print*, 'program successful.'
             end program as4q3

program as4q4
    implicit none
    logical:: p, q, not_q, lhs_bicond, rhs_bicond, lhs, rhs, is_equivalent= .true.
    integer:: i, j
    logical, dimension(2):: tf=(/.true., .false./)

    open(unit=10, file='out4q4.txt', status='replace')

    write(10,'(A)')'======================================================='
    write(10,'(A)')' Truth table to prove: NOT(p <-> q) <==> p <-> Not(q)  '
    write(10,'(A)')'======================================================='
    write(10,'(A)')' p | q | p <-> q | NOT(p <-> q) | p <-> NOT q '
    write(10,'(A)')'-------------------------------------------------------'
    do i= 1, 2
        do j= 1, 2
            p = tf(i)
            q =  tf(j)
             not_q = .not. q
          call get_biconditional(p, q, lhs_bicond)
          lhs= .not. lhs_bicond

          call get_biconditional(p, not_q, rhs_bicond)
          rhs= rhs_bicond

          if(lhs .neqv. rhs) is_equivalent = .false.

           write(10,'(5(L8, 4X))') p, q, lhs_bicond, lhs, rhs

           end do
           end do

           write(10,'(A)')'------------------------------------------------'
           if(is_equivalent)then
            write(10,'(A)')'Conclusion: NOT(p <-> q)is logically equivalent to p <-> NOT q. '
            else
                write(10,'(A)')'Conclusion: The two expressions are not logically equivalent'
             end if
             write(10,'(A)')'--------------------------------------------------------------------'
             close(10)
             print*,'Program successful.'

contains
             subroutine get_biconditional(x, y, result)
                implicit none
                logical, intent(in):: x, y
                logical, intent(out):: result

                result =  (x .eqv. y)
                end subroutine get_biconditional

             end program as4q4
program as4q5
    implicit none

    integer:: n
    integer:: H_n
    real:: time_minutes, total_cost



    open(unit=10, file='out4q5.txt',status='replace')


     write(10, '(A)')'====================================================='
     write(10, '(A)')'             Tower of hanoi computation           '
     write(10, '(A)')'====================================================='
     write(10, '(A)')' Disk(n) | Moves (Hn) | Time (Minutes) | Cost ($)    '
     Write(10, '(A)')'-----------------------------------------------------'

  do n= 1, 10
        H_n= hanoi(n)
        time_minutes = (real(H_n) * 3.0)/60.0
        total_cost =  real(H_n) * 0.02

    write(10, '(I8, 6X, I10, F12.2, 5X, F10.2)') n, H_n, time_minutes, total_cost
end do

write(10,'(A)')'----------------------------------------------------------------'

close(10)
print*, 'program successful.'

contains

 recursive function hanoi(n) result(h)
  implicit none
  integer, intent(in):: n
  integer :: h

  if( n == 1 )then
     h = 1
    else
        h=2 * hanoi(n-1) + 1
    end if

end function hanoi

end program as4q5

program as4q6
    implicit none

    integer :: n, M_n
    logical :: is_p

    open(unit=10, file='out4q6.txt', status='replace')

    write(10, '(A)') '==========================================================='
    write(10, '(A)') '        MERSENNE NUMBERS GENERATION AND PRIME CHECK        '
    write(10, '(A)') '==========================================================='
    write(10, '(A)') '   n   |   Mersenne Number (Mn = 2^n - 1)   |   Is Prime?  '
    write(10, '(A)') '-----------------------------------------------------------'


    do n = 0, 14
        M_n = (2**n) - 1
        is_p = is_prime(M_n)

        if (is_p) then
            write(10, '(I5, 10X, I15, 15X, A)') n, M_n, 'Yes'
        else
            write(10, '(I5, 10X, I15, 15X, A)') n, M_n, 'No'
        end if
    end do

    write(10, '(A)') '==========================================================='

    close(10)
    print *, 'Program executed successfully. Output saved in out4q6.txt'

contains


    function is_prime(num) result(p_flag)
        implicit none
        integer, intent(in) :: num
        logical :: p_flag
        integer :: i, limit

        if (num <= 1) then
            p_flag = .false.
            return
        end if

        p_flag = .true.
        limit = int(sqrt(real(num)))

        do i = 2, limit
            if (mod(num, i) == 0) then
                p_flag = .false.
                return
            end if
        end do
    end function is_prime

end program as4q6
program tower_vs_mersenne
    implicit none

    integer :: n, H_n, M_n
    logical :: are_equal = .true.

    open(unit=10, file='out4q7.txt', status='replace')

    write(10, '(A)') '==========================================================='
    write(10, '(A)') '     COMPARISON OF TOWER OF HANOI (Hn) AND MERSENNE (Mn)   '
    write(10, '(A)') '==========================================================='
    write(10, '(A)') '   n   |   Hn (Tower of Hanoi)   |   Mn (Mersenne Number)  '
    write(10, '(A)') '-----------------------------------------------------------'

    do n = 1, 10

        H_n = hanoi(n)


        M_n = (2**n) - 1


        if (H_n /= M_n) are_equal = .false.

        write(10, '(I5, 12X, I10, 15X, I10)') n, H_n, M_n
    end do

    write(10, '(A)') '-----------------------------------------------------------'

    if (are_equal) then
        write(10, '(A)') 'CONCLUSION: Hn = Mn for all n from 1 to 10.'
        write(10, '(A)') 'The number of moves to solve Tower of Hanoi with n disks'
        write(10, '(A)') 'is exactly equal to the n-th Mersenne number (2^n - 1).'
    else
        write(10, '(A)') 'CONCLUSION: Hn and Mn are NOT equal for all n.'
    end if

    write(10, '(A)') '==========================================================='

    close(10)
    print *, 'Program executed successfully. Output saved in out4q7.txt'

contains


    recursive function hanoi(n) result(h)
        implicit none
        integer, intent(in) :: n
        integer :: h

        if (n == 1) then
            h = 1
        else
            h = 2 * hanoi(n - 1) + 1
        end if
    end function hanoi

end program tower_vs_mersenne

program rumor_spread
    implicit none

    integer :: i
    real :: I_n, percentage
    real, parameter :: N_pop = 1000.0
    real, parameter :: k_rate = 0.3
    real, parameter :: I_zero = 5.0

    open(unit=10, file='out4q8.txt', status='replace')

    write(10, '(A)') '================================================================='
    write(10, '(A)') '                 RUMOR SPREAD SIMULATION (20 DAYS)              '
    write(10, '(A)') '================================================================='
    write(10, '(A)') '   Day (n)   |   In (People Heard)   |   Percentage Heard (%)    '
    write(10, '(A)') '-----------------------------------------------------------------'


    do i = 1, 20
        I_n = get_rumor(i)
        percentage = (I_n / N_pop) * 100.0

        write(10, '(I8, 12X, F10.2, 17X, F8.2)') i, I_n, percentage
    end do

    write(10, '(A)') '================================================================='

    close(10)
    print *, 'Program executed successfully. Output saved in out4q8.txt'

contains

    recursive function get_rumor(d) result(res)
        implicit none
        integer, intent(in) :: d
        real :: res, prev

        if (d == 0) then
            res = I_zero
        else
            prev = get_rumor(d - 1)
            res = prev + (k_rate * prev * (N_pop - prev)) / N_pop
        end if
    end function get_rumor

end program rumor_spread
program as4q9
    implicit none

    integer:: n
    real:: layer_crates, total_crates, total_cost
    real, parameter:: p_cost=15.0
    open(unit=10, file='out4q9.txt', status='replace')

write(10, '(A)') '========================================================================'
    write(10, '(A)') '                     WAREHOUSE ROBOT CRATE PYRAMID                      '
    write(10, '(A)') '========================================================================'
    write(10, '(A)') '  Layer (n)  |  Crates in Layer (n^2)  |  Total Crates (Tn)  |  Total Cost ($) '
    write(10, '(A)') '------------------------------------------------------------------------'

    do n= 1, 20
        layer_crates = real(n**2)
        total_crates = get_total_crates(n)
        total_cost=get_total_cost(n)

        write(10,'(I8, 11X, F12.2, 10X, F12.2, 7x, F12.2)') n, layer_crates, total_crates, total_cost
        end do
      write(10,'(A)')'----------------------------------------------------------------------------'

      close(10)
      print*, 'program successful.'
      contains
      recursive function get_total_crates(k) result(res)
      implicit none
      integer, intent(in)::k
      real::res
      if(k==1)then
        res=1.0
        else
            res=get_total_crates(k-1)+real(k**2)
            end if
            end function get_total_crates


        recursive function get_total_cost(k) result(res)
      implicit none
      integer, intent(in)::k
      real::res
      if(k==1)then
        res=p_cost
        else
            res=get_total_cost(k-1)+ (p_cost*real(k**2))
            end if
            end function get_total_cost
            end program as4q9
program as4q10
    implicit none

    integer:: n
    real:: a_rec, a_exact, diff
    open(unit=10, file='out4q10.txt',status='replace')

    write(10, '(A)') '======================================================================='
    write(10, '(A)') '                 RECURRENCE VS EXACT SOLUTION TABLE                   '
    write(10, '(A)') '======================================================================='
    write(10, '(A)') '   n   |   Recurrence (a_n)   |     Exact (a_n)     |    Difference    '
    write(10, '(A)') '-----------------------------------------------------------------------'

do n= 1, 50
    a_rec =get_a_n(n)
    a_exact =1.5d0*dble(n)**2 + 2.5d0*dble(n)+1.0d0
    diff=abs(a_rec - a_exact)
    write(10,'(I5, 7X, F14.2, 8X, F14.2, 8X, F10.4)')n, a_rec, a_exact, diff
    end do

    write(10,'(A)')'--------------------------------------------------------------------------'
    close(10)
    print*,'Program successful.'

    contains

    function get_a_n(n) result(res)
        implicit none
        integer, intent(in)::n
        double precision:: res
        res= compute_rec(n, 0,1.0d0,5.0d0)
        end function get_a_n


    recursive function compute_rec(target, current, a_curr, a_next) result(res)
    implicit none
    integer,intent(in):: target, current
    double precision, intent(in)::a_curr, a_next

    double precision:: res
    if(target==0)then
        res=1.0d0
        else if (target==1)then
            res=5.0d0
            else if (current == target)then
                res = a_curr

            else
                res = compute_rec(target, current + 1, a_next, 2.0d0*a_next-a_curr+3.0d0)
            end if
            end function compute_rec
            end program as4q10
program as4q11
    implicit none

    integer:: r, n_val, term_num
    real:: a_val, b_val, coeff, term_val
    parameter (a_val=3.0,b_val=2.0,n_val=5)
    open(unit=10,file='out4q11.txt', status='replace')


    write(10, '(A)') '========================================================================'
    write(10, '(A)') '                   BINOMIAL EXPANSION TABLE (a+b)^n                     '
    write(10, '(A)') '========================================================================'
    write(10, '(A)') '   r   |   Term (T_r+1)   |   Coefficient C(n,r)   |   Value of Term    '
    write(10, '(A)') '------------------------------------------------------------------------'

    do r = 0, n_val
        term_num = r+1
        coeff =nCr(n_val, r)
        term_val =coeff * (a_val**(n_val-r))*(b_val**r)

         write(10,'(I5, 9X, I8, 12X, F10.0, 10X, F14.2)') r, term_num, coeff, term_val

         end do
         write(10,'(A)')'---------------------------------------------------------------------'
         close(10)
         print*,'Progeam successful.'

         contains
         function nCr(n, r) result(res)
            implicit none
            integer, intent(in):: n, r
            real:: res

            res=fact(n)/(fact(r)*fact(n-r))
            end function nCr
            recursive function fact(k) result(res)

            implicit none
            integer, intent(in)::k
            real:: res
            if(k<=1)then
                res=1.0
                else
                    res=real(k)*fact(k-1)
                    end if
                    end function fact
                    end program as4q11






