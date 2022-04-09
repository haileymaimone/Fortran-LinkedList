! CSC-306 - Program 7 - Hailey Maimone
      PROGRAM LINKEDLIST
        TYPE :: NODE
        ! Nodes for names and their counts declared
            CHARACTER(LEN=20) :: NAME
            INTEGER :: COUNT     ! integer to hold count
            TYPE (NODE), POINTER :: NEXT, PREV   ! pointers of type node to point to the previous or next data
        END TYPE

! Declarations
        TYPE (NODE), POINTER :: HEAD, TAIL, CURRENT, DELETE  ! Pointers of type node, for the beginning, end and current of the list, and the removed
        CHARACTER(LEN=20) :: FILE_NAME, OFILE_NAME, OUTPUT   ! variables to hold file names
        INTEGER :: STATUS
        INTEGER :: ISTAT
        LOGICAL :: EXISTS               ! logical variable to check if files exist
        LOGICAL :: ARE_OPEN = .FALSE.   ! logical variable to check if files are open
        INTEGER :: FCOUNT = 0
        INTEGER :: FINT
        INTEGER :: I
        INTEGER :: CURCOUNT             ! integer variable to hold current count
        LOGICAL :: FIRST_ENTRY = .TRUE.
        CHARACTER(LEN=20) :: FNAME
        CHARACTER(LEN=20) :: FINTA
        INTEGER :: POSCOUNT
        INTEGER :: NEGCOUNT
		LOGICAL :: on = .true.          ! logical variable to loop until quit is entered
		CHARACTER*20 :: answer          ! variable to hold users answer
        
		DO WHILE(on .eqv. .true.) ! Loop to continue until users enters quit
        WRITE(*,*) '-----------------------------------'
        WRITE(*,*) 'Welcome to the linked list program!'
		WRITE(*,*) '-----------------------------------'
		WRITE(*,*) 'This program will demonstrate going through a linked list of names'
		WRITE(*,*) 'and removing the links one by one until only one name remains.'
        WRITE(*,*) 'Lets us begin...'
		WRITE(*,*) ' '
		
        NULLIFY(HEAD, TAIL)

        ! File routine    
        WRITE(*, *) 'Enter the name of the input file:'
        READ(*, '(A)') FILE_NAME                 ! Reads in file name
        INQUIRE(FILE=FILE_NAME, EXIST=EXISTS)    ! Checks if file exists
! If file doesn't exist and the user doesn't type QUIT, open the file
        DO WHILE (.NOT. EXISTS .AND. FILE_NAME .NE. 'QUIT') ! Loop to continue if file does not exist
			WRITE(*,*) ' '
			WRITE(*,*) '--------'
			WRITE(*,*) 'WARNING:'
			WRITE(*,*) '--------'
			WRITE(*,*) 'This file does not exist.  Please try again.'
            WRITE(*, *) 'Enter name of input file or "QUIT" to quit:'
            READ(*, '(A)') FILE_NAME              ! Reads in new file name if file doesn't exist
            INQUIRE(FILE=FILE_NAME, EXIST=EXISTS) ! Checks if file exists
        END DO
		
! If the user types QUIT, exit the program
        IF (FILE_NAME .EQ. 'QUIT') THEN
            WRITE(*,*) 'Exiting program...'
			on = .false. 
        ELSE
            OPEN(UNIT=1, FILE=FILE_NAME, STATUS='OLD', IOSTAT=STATUS)   ! Opens file
			WRITE(*,*) ' '
			WRITE(*,*) '----------------------------'
			WRITE(*,*) '      CONGRATULATIONS!      '
            WRITE(*,*) 'FILE WAS SUCCESSFULLY OPENED'
			WRITE(*,*) '----------------------------'
            IF (STATUS .EQ. 0) THEN
				WRITE(*,*) ' '
                WRITE(*,*) 'Enter the name of an output file:'
                READ(*, '(A)') OFILE_NAME     ! Reads in output file
                INQUIRE(FILE=OFILE_NAME, EXIST=EXISTS)  ! Checks if output file entered exists
! If file name already exists, user will be asked to overwrite, enter new name, or quit
				IF(EXISTS)THEN
					WRITE(*,*) ' '
					WRITE(*,*) 'WARNING:  A file already exists with that name.'
					WRITE(*,*) 'You may overwrite the file, create a new file with a new name, or quit.'
					WRITE(*,*) 'Please enter OVERWRITE, NEW, or QUIT'
                    READ(*, '(A)') answer  ! Reads in what user wants to do with existing file
! Loop to continue if user enters invalid answer
					DO WHILE(answer .NE. 'OVERWRITE' .AND. answer .NE. 'overwrite' .AND. &
						answer .NE. 'Overwrite' .AND. answer .NE. 'NEW' .AND. answer .NE. 'New' .AND. &
						answer .NE. 'new' .AND. answer .NE. 'QUIT' .AND. answer .NE. 'quit' &
						.AND. answer .NE. 'Quit')
						WRITE(*,*) ' '
						WRITE(*,*) ' '
						WRITE(*,*) 'WARNING:  INVALID ANSWER'
						WRITE(*,*) 'Please enter one of the following choices:'
						WRITE(*,*) 'OVERWRITE, NEW, or QUIT'
						READ(*, '(A)') answer        ! asks user to enter answer again
					END DO

! If user chooses to overwrite, file is replaced and opened
					IF(answer .eq. 'OVERWRITE' .OR. answer .eq. 'Overwrite' .OR. answer .eq. 'overwrite')THEN
						OPEN(UNIT=2, FILE=OFILE_NAME, STATUS='REPLACE', IOSTAT=STATUS, ACTION='WRITE')
						IF (STATUS .EQ. 0) THEN
							ARE_OPEN = .TRUE.    ! If file is successfully opened
							WRITE(*,*) ' '
							WRITE(*,*) '-------------------------------'
							WRITE(*,*) '        CONGRATULATIONS        '
							WRITE(*,*) 'Both files successfully opened!'
							WRITE(*,*) '-------------------------------'
						ELSE          ! If file is not successfully opened
							PRINT *, 'Error opening output file'
						END IF
! If user chooses to quit, program is exited
					ELSE IF(answer .eq. 'QUIT' .OR. answer .EQ. 'quit' .OR. answer .EQ. 'Quit')THEN
						on = .false.
! If user chooses to enter a new name, the following if statement is acted out
					ELSE IF(answer .EQ. 'new' .OR. answer .eq. 'NEW' .OR. answer .eq. 'New')THEN
					WRITE(*,*) ' '
						WRITE(*,*) 'Enter the new name for the output file: '
						READ(*, '(A)') OUTPUT   ! Reads in new output file name
						INQUIRE(FILE=OUTPUT, EXIST=EXISTS)   ! Checks if new output name exists already
						DO WHILE(EXISTS)    ! Loop if the new name also exists
							WRITE(*,*) ' '
							WRITE(*,*) 'WARNING:'
							WRITE(*,*) 'Sorry, that file name already exists.  Please enter a new name.'
							READ(*,'(A)') OUTPUT    ! Reads in new name
							INQUIRE(FILE=OUTPUT, EXIST=EXISTS)  ! Checks if this file exists
						END DO
						OPEN(UNIT=2, FILE=OUTPUT, STATUS='NEW', IOSTAT=status)  ! OUTPUT file is opened
						IF (STATUS .EQ. 0) THEN   ! ensures file is opened
							ARE_OPEN = .TRUE.  ! file is opened successfully
							WRITE(*,*) ' '
							WRITE(*,*) '-------------------------------'
							WRITE(*,*) '        CONGRATULATIONS        '
							WRITE(*,*) 'Both files successfully opened!'
							WRITE(*,*) '-------------------------------'
						ELSE
							PRINT *, 'Error opening output file'
						END IF
						WRITE(*,*) 'The new file name is: ', OUTPUT   ! Prints new file name to screen 
						WRITE(*,*) ' '
					ENDIF
				ELSE IF(.NOT. exists)THEN     ! If file name does not exist, if statement acted out
					OPEN(UNIT=2, FILE=OFILE_NAME, STATUS='NEW', IOSTAT=status)   ! OFILE_NAME file is opened
					IF(status .eq. 0) THEN  ! ensures file is opened
						ARE_OPEN = .TRUE.   ! file is opened
					ELSE  ! If output file is not successfully opened
						WRITE(*,*) 'Error opening output file '
					ENDIF
				END IF
            ELSE   ! If input file is not successfully opened
                PRINT *, 'Error opening input file'
            END IF
            IF (.NOT. ARE_OPEN) THEN  !If they are not opened, file is closed
                CLOSE(1)
            END IF
        END IF
! If both files are open then start to process data
        IF (ARE_OPEN) THEN
            DO WHILE (STATUS .EQ. 0 .AND. FCOUNT .NE. 25) ! while not end-of-file and count is less than 25
                READ(1, '(A)', IOSTAT=STATUS) FNAME ! read string for name
                IF (FNAME .NE. '') THEN ! if name isn't empty, proceed
                    READ(1, '(A)', IOSTAT=STATUS) FINTA ! read integer as a string to be able to detect if the line is empty
                    IF (FINTA .NE. '') THEN
                        READ(FINTA, '(I5)') FINT ! if line is not empty, convert string to integer
                        IF (FINT .NE. 0) THEN ! if integer isn't 0, proceed
                            IF (STATUS .EQ. 0) THEN ! proceed only if status is 0, to prevent final record from being duplicated
                                FCOUNT = FCOUNT + 1 ! complete record, so proceed
                                ! allocate node and record to the node
                                ALLOCATE(CURRENT, STAT=ISTAT)
                                IF (ISTAT .EQ. 0) THEN
                                    NULLIFY(CURRENT%NEXT)
                                    NULLIFY(CURRENT%PREV)
                                    CURRENT%NAME = FNAME
                                    CURRENT%COUNT = FINT
                                    IF (ASSOCIATED(HEAD)) THEN
                                        ! if link list exists, add node to the end
                                        TAIL%NEXT => CURRENT
                                        CURRENT%PREV => TAIL
                                        TAIL => CURRENT
                                    ELSE
                                        ! otherwise make it the first node
                                        HEAD => CURRENT
                                        TAIL => CURRENT
                                    END IF
                                END IF
                            END IF
                        ELSE 
                            PRINT*, FNAME, 'has a count of 0.'
                        END IF
                    ELSE
                        PRINT*, FNAME, 'No Count entered for this record'
                    END IF
                END IF
            END DO
            IF (FCOUNT .EQ. 25 .AND. STATUS .EQ. 0) THEN  ! Ensures records less than 25
                PRINT *, 'More than 25 records entered'
            END IF

            NULLIFY(HEAD%PREV, TAIL%NEXT)


            CURRENT => HEAD
            CURCOUNT = CURRENT%COUNT
            PRINT*, ''
            WRITE(2,*) ' '
            WRITE(*, '(A, A, I5)') CURRENT%NAME, 'with a count of ', CURRENT%COUNT     ! Prints out name and their count to screen
			WRITE(2,*) CURRENT%NAME, 'with a count of ', CURRENT%COUNT  ! Prints out name and their count to output file
            PRINT*, ' '
			WRITE(2,*) ' '
            DO WHILE(ASSOCIATED(CURRENT%NEXT) .OR. ASSOCIATED(CURRENT%PREV))
                IF (FIRST_ENTRY .EQV. .TRUE.) THEN ! If first time entering
                    POSCOUNT = CURCOUNT ! set poscount equal to curcount
                    NEGCOUNT = ABS(CURCOUNT) ! set negcount equal to absolute value of curcount
                ELSE
                    POSCOUNT = CURCOUNT-1   ! Decrements curcount by one and saves as poscount
                    NEGCOUNT = ABS(CURCOUNT)-1 ! takes absolute value of curcount decremented by one and saves as negcount
                END IF
                IF (CURCOUNT .LT. 0) THEN  ! If current count is less than 0 then if statement is acted out
                    DO I = 1, NEGCOUNT   ! loop starts at 1 and continues until it reaches negcount, increasing by one each loop
                        ! traverse backwards to the node designated by count
						IF (ASSOCIATED(CURRENT%PREV)) THEN  ! checks if associated
                            CURRENT => CURRENT%PREV ! if associated, current points to current's previous
                        ELSE
                            CURRENT => TAIL   ! CURRENT points to TAIL if not associated
                        END IF
                    END DO
                ELSE IF (CURCOUNT .GT. 0) THEN   ! if current count is greater than zero, if statement is acted out
                    DO I = 1, POSCOUNT   ! loop to continue from 1 until it reaches POSCOUNT, increasing by 1 each loop
                        ! traverse forwards to the node designated by count
						IF(ASSOCIATED(CURRENT%NEXT)) THEN ! checks if associated
                            CURRENT => CURRENT%NEXT ! if associated, current points to current's next
                        ELSE
                            CURRENT => HEAD   ! if not associated, CURRENT points to HEAD
                        END IF
                    END DO
                END IF
                ! if current is associated
                IF (ASSOCIATED(CURRENT)) THEN
                    ! is current's previous is associated
                    IF (ASSOCIATED(CURRENT%PREV)) THEN
                        ! point current's previous next to current's next
                        CURRENT%PREV%NEXT => CURRENT%NEXT
                    ELSE
                        ! if not associated point head to current's next
                        HEAD => CURRENT%NEXT
                    END IF
                    ! if current's next is associated
                    IF (ASSOCIATED(CURRENT%NEXT)) THEN
                        ! point current's next previous to current's previous
                        CURRENT%NEXT%PREV => CURRENT%PREV
                    ELSE
                        ! if not associated point tail to current's previous
                        TAIL => CURRENT%PREV
                    END IF
                    ! set delete pointer to be equal to current
                    DELETE => CURRENT
                    ! write node deletion output to screen and file
                    PRINT*, 'Removing Node: ', DELETE%NAME, 'with a count of ', DELETE%COUNT
					WRITE(2,*) 'Removing Node: ', DELETE%NAME, 'with a count of ', DELETE%COUNT
                    PRINT*, ''
                    WRITE(2,*) ' '
                    ! update the current count to be that of the delete node
                    CURCOUNT = DELETE%COUNT
                    ! delete node
                    DEALLOCATE(DELETE)

                    ! process negative count
                    IF (CURCOUNT .LT. 0) THEN
                        ! if current count is less than 0 and current's previous is associated
                        IF (ASSOCIATED(CURRENT%PREV)) THEN
                            ! set current to current's previous
                            CURRENT => CURRENT%PREV
                        ELSE
                            ! if not associated, set current to tail
                            CURRENT => TAIL
                        END IF
                    END IF

                    ! process positive count
                    IF (CURCOUNT .GT. 0) THEN
                        ! if current count is greater than 0 and current' next is associated
                        IF (ASSOCIATED(CURRENT%NEXT)) THEN
                            ! set current to current's next
                            CURRENT => CURRENT%NEXT
                        ELSE
                            ! if not associated, set current to head
                            CURRENT => HEAD
                        END IF
                    END IF
                    ! end of entry, set first_entry to always be false since we are no longer on our first entry
                    FIRST_ENTRY = .FALSE.
                END IF
            END DO

                IF(.NOT. ASSOCIATED(CURRENT%NEXT) .AND. .NOT. ASSOCIATED(CURRENT%PREV)) THEN
                    ! if current's next and previous are null, we found the survivor
                    PRINT*, 'The survivor is ', CURRENT%NAME
					WRITE(2, *) 'The survivor is ', CURRENT%NAME
                ELSE
                    ! something went horribly wrong
                    PRINT*, 'Error'
                END IF
        END IF
        CLOSE(1)
        CLOSE(2)
        STOP
		ENDDO
      END PROGRAM LINKEDLIST
