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







