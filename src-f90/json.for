
      MODULE json
        USE json_string_utility
        USE json_path_utility
        USE json_tokenizer
        IMPLICIT NONE
      CONTAINS

C     
        FUNCTION tokenize_json_string(source, len_source) RESULT(tokens)

C       Explicit argument-type definitions
          CHARACTER,POINTER       :: source(:)
          INTEGER(4)              :: len_source
          
C       Explicit return-type definition
          INTEGER(4),POINTER      :: tokens(:)

        END FUNCTION tokenize_json_string

C     
        SUBROUTINE tokenize_json_file(path, source, tokens)

C       Explicit argument-type definitions
          CHARACTER(1024),POINTER :: path

C       Explicit return-type definition
          INTEGER(4),POINTER      :: tokens(:)

          INTEGER(4)              :: len_source
          INTEGER                 :: file_id
          INTEGER                 :: file_status
          
          CHARACTER,POINTER       :: source(:)

          INQUIRE(FILE=path, SIZE=len_source)

          OPEN(NEWUNIT=file_id,                                         &
     &      FILE=path,                                                  &
     &      ACTION="read",                                              &
     &      IOSTAT=file_status,                                         &
     &      FORM="UNFORMATTED",                                         &
     &      ACCESS="DIRECT",                                            &
     &      RECL=len_source)

          IF(file_status .NE. 0) THEN
            WRITE (*, *) "error: failed opening the json-file:",        &
     &        path
            STOP
          ENDIF

          ALLOCATE(source(len_source))
          READ(UNIT=file_id, IOSTAT=file_status, REC=1) source
          CLOSE(UNIT=file_id)

          tokens => tokenize_json_string(source, len_source)
          RETURN
        END SUBROUTINE tokenize_json_file



        FUNCTION search_json_location(tokens, source, json_path)        &
     &      RESULT(token_index)

C       Explicit argument-type definitions
          INTEGER(4),POINTER      :: tokens(:)
          CHARACTER,POINTER       :: source(:)
          CHARACTER,TARGET        :: json_path(1024)

C       Explicit return-type definition
          INTEGER(4)              :: token_index

C       Explicit internal variable definitions
          CHARACTER,POINTER       :: path(:)
          INTEGER                 :: len_path
          INTEGER                 :: path_offset
          INTEGER                 :: backwards_offset

          INTEGER                 :: element_start
          INTEGER                 :: len_element
          INTEGER                 :: element_end

C       Initialization
          path => json_path

C       Find the path's actual usable length
          DO 2360 path_offset = 1, 1023
            backwards_offset = 1024 - path_offset
            IF(is_printable(path(backwards_offset))) THEN
              EXIT
            END IF
2360      END DO
          len_path = 1024 - path_offset

          IF(len_path .EQ. 0) THEN
            WRITE(*, *) "error: empty json-path given."
            STOP
          END IF
          
          CALL json_search_path_element(path, len_path, 1,              &
     &      element_start, len_element)

          element_end = element_start + len_element - 1

          WRITE(*, *) "'", path(element_start:element_end), "'"

          RETURN
        END FUNCTION search_json_location



        FUNCTION json_exists(tokens, source, path)                      &
     &      RESULT(path_exists)

C       Explicit argument-type definitions
          INTEGER(4),POINTER      :: tokens(:)
          CHARACTER,POINTER       :: source(:)
          CHARACTER,POINTER       :: path(:)

C       Explicit return-type definition
          LOGICAL                 :: path_exists

          RETURN
        END FUNCTION json_exists



        FUNCTION json_is_object(tokens, source, path)                   &
     &      RESULT(is_object)

C       Explicit argument-type definitions
          INTEGER(4),POINTER      :: tokens(:)
          CHARACTER,POINTER       :: source(:)
          CHARACTER,POINTER       :: path(:)

C       Explicit return-type definition
          LOGICAL                 :: is_object

          RETURN
        END FUNCTION json_is_object



        FUNCTION json_is_array(tokens, source, path)                    &
     &      RESULT(is_array)

C       Explicit argument-type definitions
          INTEGER(4),POINTER      :: tokens(:)
          CHARACTER,POINTER       :: source(:)
          CHARACTER,POINTER       :: path(:)

C       Explicit return-type definition
          LOGICAL                 :: is_array

          RETURN
        END FUNCTION json_is_array



        FUNCTION json_is_string(tokens, source, path)                   &
     &      RESULT(is_string)

C       Explicit argument-type definitions
          INTEGER(4),POINTER      :: tokens(:)
          CHARACTER,POINTER       :: source(:)
          CHARACTER,POINTER       :: path(:)

C       Explicit return-type definition
          LOGICAL                 :: is_string

          RETURN
        END FUNCTION json_is_string



        FUNCTION json_is_integer(tokens, source, path)                  &
     &      RESULT(is_integer)

C       Explicit argument-type definitions
          INTEGER(4),POINTER      :: tokens(:)
          CHARACTER,POINTER       :: source(:)
          CHARACTER,POINTER       :: path(:)

C       Explicit return-type definition
          LOGICAL                 :: is_integer

          RETURN
        END FUNCTION json_is_integer



        FUNCTION json_is_boolean(tokens, source, path)                  &
     &      RESULT(is_boolean)

C       Explicit argument-type definitions
          INTEGER(4),POINTER      :: tokens(:)
          CHARACTER,POINTER       :: source(:)
          CHARACTER,POINTER       :: path(:)

C       Explicit return-type definition
          LOGICAL                 :: is_boolean

          RETURN
        END FUNCTION json_is_boolean



        FUNCTION json_is_null(tokens, source, path)                     &
     &      RESULT(is_null)

C       Explicit argument-type definitions
          INTEGER(4),POINTER      :: tokens(:)
          CHARACTER,POINTER       :: source(:)
          CHARACTER,POINTER       :: path(:)

C       Explicit return-type definition
          LOGICAL                 :: is_null

          RETURN
        END FUNCTION json_is_null

      END MODULE json

