
      MODULE json_tokenizer
        USE json_utility
        IMPLICIT NONE
      CONTAINS
        FUNCTION tokenize_json(source) RESULT (tokens)
          IMPLICIT NONE

          CHARACTER,POINTER :: source(:)
          BYTE,POINTER :: tokens(:)
          BYTE,POINTER :: arena(:)
          arena => new_arena(16384)

          tokens => arena
          RETURN
        END FUNCTION tokenize_json

      END MODULE json_tokenizer

