
      MODULE json_tokenizer
        USE json_utility
      CONTAINS
        FUNCTION tokenize_json() RESULT(tokens)
          BYTE,ALLOCATABLE :: tokens(:)
          BYTE,ALLOCATABLE :: arena(:)
          arena = new_arena(16384)

          tokens = arena
        END FUNCTION tokenize_json

      END MODULE json_tokenizer

