
      MODULE json_path_utility
        IMPLICIT NONE
      CONTAINS

C     @todo: This function could be useful.
C        FUNCTION json_count_path_elements(path, len_path) RESULT(count)
C          CHARACTER,POINTER       :: path(:)
C          INTEGER                 :: len_path
C          INTEGER                 :: count
C
C
C        END FUNCTION



C     json_search_path_element:
C       Finds out the start and length of a path element from a string
C       that is formatted like a unixoid path, with slashes as element
C       splitters.
C
C       Parameters(5):
C         0. path
C            String with the length given as 'len_path'. This should be
C            ASCII-formatted and have no spaces in the front or back.
C
C         1. len_path
C            Length of the path given in element 0, given in bytes.
C
C         2. element_index
C            Index of the path  element that  should be searched for by
C            this  function,  starting from one  for the  element right
C            after the initial slash.
C
C         3. start
C            Output  argument that tells  at which one-based  index the
C            path element's first character (after the slash) was found
C
C         4. length
C            Number of bytes found to be belonging to the path element.
C
        SUBROUTINE json_search_path_element(path, len_path,             &
     &      element_index, start, length)

C       Explicit argument-type definitions
          CHARACTER,POINTER       :: path(:)
          INTEGER                 :: len_path
          INTEGER                 :: element_index

C       Explicit return-type definition
          INTEGER                 :: start
          INTEGER                 :: length

C       Explicit internal variable definitions
          INTEGER                 :: separator_count
          INTEGER                 :: offset
          CHARACTER               :: current_character

C       Initialization
          separator_count         = 0
          start                   = 1

C       Find the start of the searched element

          DO 140 offset = 1, len_path
C         @todo: Accept UTF-8
            current_character = path(offset)
            IF(current_character .EQ. '/') THEN
              separator_count = separator_count + 1
            END IF

            IF(separator_count .GT. element_index) THEN
C           Jump over the current character (the slash); it isn't part
C           of the actual path element that is being searched.
              start  = offset + 1
              EXIT
            END IF
140       END DO

C       If the 'start'-variable wasn't set yet, the element couldn't
C       be found; there are too few elements.
          IF(start .EQ. 0) THEN
            RETURN
          END IF

C       Jump over the slash
          offset = offset + 1

          DO 440 offset = offset, len_path
C         @todo: Accept UTF-8
            current_character = path(offset)

            IF(current_character .EQ. '/') THEN
              EXIT
            END IF
440       END DO

          length = offset - start
          RETURN
        END SUBROUTINE json_search_path_element
      END MODULE json_path_utility

