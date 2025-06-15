
      MODULE json_tokenizer
        USE json_string_utility
        IMPLICIT NONE

      CONTAINS

C       Return Value:
C         The return value (tokens) is built up as an array of integers
C         of four bytes each.  Of those integers, there are always five
C         that are  grouped as  one token.  The meaning is  as follows:
C
C           0. token_type
C           1. start_offset
C           2. len_string
C           3. line_index
C           4. column_index
C
C       Token Types:
C         The 'token_type'-field contains the type of data found at the
C         offset. It can be any one of the following:
C
C           0.   string
C           1.   integer
C           2.   float
C           3.   word
C           4.   opening_curly_brace
C           5.   closing_curly_brace
C           6.   opening_square_bracket
C           7.   closing_square_bracket
C           8.   colon
C           9.   comma
C           255. stream_end
C
C       List of tokenization FSM (Finite State Manchine) states:
C
C           0. accept_all
C           1. within_string
C           2. within_number
C           3. within_decimal
C           4. within_word
C           5. sign_found
C
C     @todo: Split this function into multiple parts for each FSM state
        SUBROUTINE tokenize_json(source, len_source, tokens, num_tokens)
          IMPLICIT NONE

C       Declarations

          INTEGER(4) :: num_tokens
          INTEGER(4),POINTER :: tokens(:)
          CHARACTER,POINTER :: source(:)
          INTEGER(4) :: source_offset
          INTEGER(4) :: token_start

C       Number of the next token; index of the token one after the last
C       currently used one. This is  the index of the token, not of the
C       integer within the 'tokens'-array!
          INTEGER(4) :: token_index

C       When the start of a string was found, the delimiter will be set
C       to a single quotation mark or a double quotation mark, the same
C       as the start character. This is used to  determine the end char
C       of a string.
C
C       When a sign is found, it is first stored into the 'delimiter'
C       variable so it can be used again in the next iteration of the
C       loop, when the branch for the sign is taken.
          CHARACTER(4) :: delimiter
          CHARACTER(4) :: current_character
          INTEGER(4) :: len_source
          INTEGER(1) :: fsm_state

C       Initialization

          token_index = 0
          source_offset = 1
          fsm_state = 0

C       @todo: Make one pass for counting the number of tokens and then
C              allocate the tokens for creation in the next pass.

          ALLOCATE(tokens(len_source * 5))

C       Tokenization Loop

          DO 390 source_offset = 1, len_source
            current_character = source(source_offset)
            SELECT CASE(fsm_state)

              CASE(0)
                IF(is_whitespace(current_character)) THEN
                  GOTO 390
                END IF

                IF(current_character .EQ. '"') THEN
                  token_start = source_offset
                  delimiter = '"'
                  fsm_state = 1
                  GOTO 390
                END IF

                IF(current_character .EQ. "'") THEN
                  token_start = source_offset
                  delimiter = "'"
                  fsm_state = 1
                  GOTO 390
                END IF
                
                IF(is_sign(current_character)) THEN
                  token_start = source_offset
                  delimiter = current_character
                  fsm_state = 5
                  GOTO 390
                END IF

              CASE(1)
                
                

              CASE(2)
                
                

              CASE(3)
                
                

              CASE(4)
                
                

              CASE(5)
                IF(delimiter .EQ. '{') THEN
                  tokens(token_index*5+1) = 4
                  tokens(token_index*5+2) = token_start
                  tokens(token_index*5+3) = source_offset - token_start
C               @todo: Fill the fields 'line_index' and 'column_index'
                  tokens(token_index*5+4) = 1
                  tokens(token_index*5+5) = 1

                  token_index = token_index + 1

                  fsm_state = 0
                  GOTO 390
                END IF

                IF(delimiter .EQ. '}') THEN
                  tokens(token_index*5+1) = 5
                  tokens(token_index*5+2) = token_start
                  tokens(token_index*5+3) = source_offset - token_start
C               @todo: Fill the fields 'line_index' and 'column_index'
                  tokens(token_index*5+4) = 1
                  tokens(token_index*5+5) = 1

                  token_index = token_index + 1

                  fsm_state = 0
                  GOTO 390
                END IF

                IF(delimiter .EQ. '[') THEN
                  tokens(token_index*5+1) = 6
                  tokens(token_index*5+2) = token_start
                  tokens(token_index*5+3) = source_offset - token_start
C               @todo: Fill the fields 'line_index' and 'column_index'
                  tokens(token_index*5+4) = 1
                  tokens(token_index*5+5) = 1

                  token_index = token_index + 1

                  fsm_state = 0
                  GOTO 390
                END IF

                IF(delimiter .EQ. ']') THEN
                  tokens(token_index*5+1) = 7
                  tokens(token_index*5+2) = token_start
                  tokens(token_index*5+3) = source_offset - token_start
C               @todo: Fill the fields 'line_index' and 'column_index'
                  tokens(token_index*5+4) = 1
                  tokens(token_index*5+5) = 1

                  token_index = token_index + 1

                  fsm_state = 0
                  GOTO 390
                END IF

                IF(delimiter .EQ. ':') THEN
                  tokens(token_index*5+1) = 8
                  tokens(token_index*5+2) = token_start
                  tokens(token_index*5+3) = source_offset - token_start
C               @todo: Fill the fields 'line_index' and 'column_index'
                  tokens(token_index*5+4) = 1
                  tokens(token_index*5+5) = 1

                  token_index = token_index + 1

                  fsm_state = 0
                  GOTO 390
                END IF

                IF(delimiter .EQ. ',') THEN
                  tokens(token_index*5+1) = 9
                  tokens(token_index*5+2) = token_start
                  tokens(token_index*5+3) = source_offset - token_start
C               @todo: Fill the fields 'line_index' and 'column_index'
                  tokens(token_index*5+4) = 1
                  tokens(token_index*5+5) = 1

                  token_index = token_index + 1

                  fsm_state = 0
                  GOTO 390
                END IF


            END SELECT
390       END DO

C       Write the 'stream_end'-token
          tokens(token_index*5+1) = 255
          tokens(token_index*5+2) = 0
          tokens(token_index*5+3) = len_source
C       @todo: Fill the fields 'line_index' and 'column_index'
          tokens(token_index*5+4) = 1
          tokens(token_index*5+5) = 1

          token_index = token_index + 1

          tokens = tokens(1:token_index*5)
          num_tokens = token_index
          RETURN
        END SUBROUTINE tokenize_json



        SUBROUTINE write_tokens(tokens, num_tokens, source)

          INTEGER(4),POINTER    :: tokens(:)
          INTEGER(4)            :: num_tokens
          CHARACTER(1),POINTER  :: source(:)
          INTEGER               :: unit_number

          CHARACTER,POINTER     :: type_string(:)
          INTEGER(4)            :: token_type, offset, len_string
          INTEGER(4)            :: integer_index

          DO 2700 integer_index = 0, num_tokens
            token_type = tokens(integer_index * 5 + 1)
            offset     = tokens(integer_index * 5 + 2)
            len_string = tokens(integer_index * 5 + 3)
            
            IF(token_type .EQ. 255) THEN
              EXIT
            END IF

            SELECT CASE(token_type)
              CASE(0)
                WRITE (*, "(A)", ADVANCE="no") "STRING         "

              CASE(1)
                WRITE (*, "(A)", ADVANCE="no") "INTEGER        "

              CASE(2)
                WRITE (*, "(A)", ADVANCE="no") "FLOAT          "

              CASE(3)
                WRITE (*, "(A)", ADVANCE="no") "WORD           "

              CASE(4)
                WRITE (*, "(A)", ADVANCE="no") "OPENING_BRACE  "

              CASE(5)
                WRITE (*, "(A)", ADVANCE="no") "CLOSING_BRACE  "

              CASE(6)
                WRITE (*, "(A)", ADVANCE="no") "OPENING_BRACKET"

              CASE(7)
                WRITE (*, "(A)", ADVANCE="no") "CLOSING_BRACKET"

              CASE(8)
                WRITE (*, "(A)", ADVANCE="no") "COLON          "

              CASE(9)
                WRITE (*, "(A)", ADVANCE="no") "COMMA          "

              CASE(255)
                WRITE (*, "(A)", ADVANCE="no") "STREAM_END     "
            END SELECT

            WRITE (*, *) "'", source(offset:offset+len_string - 1), "'"
2700      END DO

        END SUBROUTINE write_tokens

      END MODULE json_tokenizer

