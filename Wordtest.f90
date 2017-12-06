Program WordTest

    implicit none

    integer :: wordnum,testnum,i,j,wordindex,correct,correctnum
    character*200 :: input,input2
    character*200,allocatable :: words(:)
    character*200,allocatable :: meaning(:)
    character*200 :: answer(4)
    real*8 :: randx,input3

    !===========================================
    ! Program starts

    !internal control
    wordnum=2996
    allocate(words(wordnum))
    allocate(meaning(wordnum))

    !Read-in Wordlist
    open(20,file="Words.txt",status='unknown')
    do i=1,wordnum
        read(20,*) words(i),meaning(i)
        words(i)=trim(adjustl(words(i)))
        meaning(i)=trim(adjustl(meaning(i)))
        !write(*,*) meaning(i)
        !write(*,*) words(i)
    end do

10  continue

    testnum=0
    wordindex=0
    call system("cls")
    write(*,*) "============================================"
    write(*,*) "GRE WORD TEST VERSION 1.0"
    write(*,*) "============================================"
    write(*,*) "Written by Diandong Tang 12/6/17"
    write(*,*)

    !Menu
    write(*,*) "============================================"
    write(*,*) "Please choose a mode:"
    write(*,*) ""
    write(*,*) "1.Single Practice (1 word)"
    write(*,*) "2.Regular Practice (100 words)"
    write(*,*) "3.Quit"
    write(*,*) ""
    write(*,*) "============================================"
    read(*,*) input
    input=trim(adjustl(input))

    if (input=="1") then
        goto 30
        elseif (input=="2") then
            goto 40
        elseif (input=="3") then
            stop
        else
            goto 10
    end if

! Single Test
30  continue
    call system("cls")
    ! get the word
    call init_random_seed(0)
    call random_number(randx)
    wordindex=int(randx*wordnum)+1
    ! prepare the answers
    do i=1,4
        call init_random_seed(i)
        call random_number(randx)
        answer(i)=meaning(int(randx*wordnum)+1)
    end do
    correct=int(randx*4)+1
    !write(*,*) correct
    answer(correct)=meaning(wordindex)
    ! show question
    write(*,*) "============================================"
    write(*,*) "Single Test                  Press 0 to Exit"
    write(*,*) "============================================"
    write(*,"(' >',a,':                  (Enter 1, 2, 3 or 4)')") trim(adjustl(words(wordindex)))
    write(*,*)
    write(*,"(' 1.',a,';')") trim(adjustl(answer(1)))
    write(*,"(' 2.',a,';')") trim(adjustl(answer(2)))
    write(*,"(' 3.',a,';')") trim(adjustl(answer(3)))
    write(*,"(' 4.',a,';')") trim(adjustl(answer(4)))
    write(*,*)
31  continue
    read(*,*) input2
    input2=trim(adjustl(input2))

    if (input2 == "0") then
        call system("cls")
        goto 10
    elseif (input2 == "1") then
        input3=1
    elseif (input2 == "2") then
        input3=2
    elseif (input2 == "3") then
        input3=3
    elseif (input2 == "4") then
        input3=4
    else
        goto 31
    end if

    if (answer(input3)==trim(adjustl(meaning(wordindex)))) then
        write(*,*) "Correct. Next Question."
    else
        write(*,"('Wrong answer or invalid input! Correct answer is ',a)") trim(adjustl(meaning(wordindex)))
    end if
    read(*,*)
    goto 30

! 100 Test
40  continue
call system("cls")
write(*,*) "============================================"
write(*,*) "Welcome to 100 word test."
write(*,*) ""
write(*,*) "Instructions:"
write(*,*) "In this test, you will do 100 practice continuously,"
write(*,*) "The words you fail to recognize will be recorded into 'Note.txt',"
write(*,*) "It's a file which you can find next to this Test Program."
write(*,*) ""
write(*,*) "A score will be shown on the screen."
write(*,*) "============================================"
write(*,*) "Press any key to continue."
read(*,*)
call system("cls")
correctnum=0
open(50,file="Notes.txt",status='unknown')
do j=1,100
    call system("cls")
    ! get the word
    call init_random_seed(0)
    call random_number(randx)
    wordindex=int(randx*wordnum)+1
    ! prepare the answers
    do i=1,4
        call init_random_seed(i)
        call random_number(randx)
        answer(i)=meaning(int(randx*wordnum)+1)
    end do
    correct=int(randx*4)+1
    !write(*,*) correct
    answer(correct)=meaning(wordindex)
    ! show question
    write(*,*) "============================================"
    write(*,*) "100 Words Test               Press 0 to Exit"
    write(*,*) "============================================"
    write(*,"(' Question: ',i3,'/100')") j
    write(*,"(' Score: ',i3,'/100')") correctnum
    write(*,*) "============================================"
    write(*,"(' >',a,':                  (Enter 1, 2, 3 or 4)')") trim(adjustl(words(wordindex)))
    write(*,*)
    write(*,"(' 1.',a,';')") trim(adjustl(answer(1)))
    write(*,"(' 2.',a,';')") trim(adjustl(answer(2)))
    write(*,"(' 3.',a,';')") trim(adjustl(answer(3)))
    write(*,"(' 4.',a,';')") trim(adjustl(answer(4)))
    write(*,*)

    read(*,*) input2
    input2=trim(adjustl(input2))

    if (input2 == "0") then
        call system("cls")
        goto 10
    elseif (input2 == "1") then
        input3=1
    elseif (input2 == "2") then
        input3=2
    elseif (input2 == "3") then
        input3=3
    elseif (input2 == "4") then
        input3=4
    else
        goto 31
    end if

    if (answer(input3)==trim(adjustl(meaning(wordindex)))) then
        write(*,*) "Correct. Next Question."
        correctnum=correctnum+1
    else
        write(*,"('Wrong answer or invalid input! Correct answer is ',a)") trim(adjustl(meaning(wordindex)))
        write(50,"(a,'     ',a)") trim(adjustl(words(wordindex))), trim(adjustl(meaning(wordindex)))
    end if
    read(*,*)
end do

call system("cls")
write(*,*) "============================================"
write(*,*) "100 word test finish"
write(*,"(' Score: ',i3,'/100')") correctnum
write(*,*) "============================================"
write(*,*) "Press any key to continue."
read(*,*)
goto 10

contains

subroutine init_random_seed(k)

      INTEGER :: i, n, clock, k
      INTEGER, DIMENSION(:), ALLOCATABLE :: seed

      CALL RANDOM_SEED(size = n)
      ALLOCATE(seed(n))
      CALL SYSTEM_CLOCK(COUNT=clock)
      seed = clock + 37 * (/ (i - 1, i = 1, n) /)
      CALL RANDOM_SEED(PUT = seed - k)
      DEALLOCATE(seed)

end subroutine

End Program WordTest
