
      MODULE json_string_utility
        IMPLICIT NONE
      CONTAINS
        FUNCTION is_whitespace(code_point) RESULT (boolean)
          CHARACTER(4) :: code_point
          LOGICAL :: boolean

C       In the case of a horizontal tab (9 in decimal = 0x09 = '\t')
          IF(code_point .EQ. CHAR(9)) THEN
            boolean = .TRUE.
            RETURN
          END IF

C       In the case of a linefeed (10 in decimal = 0x0a = '\n')
          IF(code_point .EQ. CHAR(10)) THEN
            boolean =.TRUE.
            RETURN
          END IF

C       In the case of a carriage return (13 in decimal = 0x0d = '\r')
          IF(code_point .EQ. CHAR(13)) THEN
            boolean =.TRUE.
            RETURN
          END IF

C       In the case of a space (32 in decimal = 0x20 = ' ')
          IF(code_point .EQ. CHAR(32)) THEN
            boolean =.TRUE.
            RETURN
          END IF

          boolean = .FALSE.
          RETURN
        END FUNCTION



        FUNCTION is_digit(code_point) RESULT (boolean)
          CHARACTER(4) :: code_point
          LOGICAL :: boolean

          IF(code_point .LT. '0') THEN
            boolean = .FALSE.
            RETURN
          END IF

          IF(code_point .GT. '9') THEN
            boolean = .FALSE.
            RETURN
          END IF

          boolean = .TRUE.
          RETURN
        END FUNCTION is_digit



        FUNCTION is_lowercase(code_point) RESULT (boolean)
          CHARACTER(4) :: code_point
          LOGICAL :: boolean

          IF(code_point .LT. 'a') THEN
            boolean = .FALSE.
            RETURN
          END IF

          IF(code_point .GT. 'z') THEN
            boolean = .FALSE.
            RETURN
          END IF

          boolean = .TRUE.
          RETURN
        END FUNCTION is_lowercase



        FUNCTION is_uppercase(code_point) RESULT (boolean)
          CHARACTER(4) :: code_point
          LOGICAL :: boolean

          IF(code_point .LT. 'A') THEN
            boolean = .FALSE.
            RETURN
          END IF

          IF(code_point .GT. 'Z') THEN
            boolean = .FALSE.
            RETURN
          END IF

          boolean = .TRUE.
          RETURN
        END FUNCTION is_uppercase



        FUNCTION is_letter(code_point) RESULT (boolean)
          CHARACTER(4) :: code_point
          LOGICAL :: boolean

          IF(is_lowercase(code_point)) THEN
            boolean = .TRUE.
            RETURN
          END IF

          IF(is_uppercase(code_point)) THEN
            boolean = .TRUE.
            RETURN
          END IF

          boolean = .FALSE.
          RETURN
        END FUNCTION is_letter



        FUNCTION is_sign_of_block_1(code_point) RESULT (boolean)
          CHARACTER(4) :: code_point
          LOGICAL :: boolean

          IF(code_point .LT. '!') THEN
            boolean = .FALSE.
            RETURN
          END IF

          IF(code_point .GT. '/') THEN
            boolean = .FALSE.
            RETURN
          END IF

          boolean = .TRUE.
          RETURN
        END FUNCTION is_sign_of_block_1



        FUNCTION is_sign_of_block_2(code_point) RESULT (boolean)
          CHARACTER(4) :: code_point
          LOGICAL :: boolean

          IF(code_point .LT. ':') THEN
            boolean = .FALSE.
            RETURN
          END IF

          IF(code_point .GT. '@') THEN
            boolean = .FALSE.
            RETURN
          END IF

          boolean = .TRUE.
          RETURN
        END FUNCTION is_sign_of_block_2



        FUNCTION is_sign_of_block_3(code_point) RESULT (boolean)
          CHARACTER(4) :: code_point
          LOGICAL :: boolean

          IF(code_point .LT. '[') THEN
            boolean = .FALSE.
            RETURN
          END IF

          IF(code_point .GT. '`') THEN
            boolean = .FALSE.
            RETURN
          END IF

          boolean = .TRUE.
          RETURN
        END FUNCTION is_sign_of_block_3



        FUNCTION is_sign_of_block_4(code_point) RESULT (boolean)
          CHARACTER(4) :: code_point
          LOGICAL :: boolean

          IF(code_point .LT. '{') THEN
            boolean = .FALSE.
            RETURN
          END IF

          IF(code_point .GT. '~') THEN
            boolean = .FALSE.
            RETURN
          END IF

          boolean = .TRUE.
          RETURN
        END FUNCTION is_sign_of_block_4



        FUNCTION is_sign(code_point) RESULT (boolean)
          CHARACTER(4) :: code_point
          LOGICAL :: boolean

          IF(is_sign_of_block_1(code_point)) THEN
            boolean = .TRUE.
            RETURN
          END IF

          IF(is_sign_of_block_2(code_point)) THEN
            boolean = .TRUE.
            RETURN
          END IF

          IF(is_sign_of_block_3(code_point)) THEN
            boolean = .TRUE.
            RETURN
          END IF

          IF(is_sign_of_block_4(code_point)) THEN
            boolean = .TRUE.
            RETURN
          END IF

          boolean = .FALSE.
          RETURN
        END FUNCTION is_sign
      END MODULE json_string_utility

