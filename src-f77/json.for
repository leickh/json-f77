
      MODULE json_parser
        USE json_tokenizer
      CONTAINS
        FUNCTION parse_json() RESULT(tokens)
          BYTE,ALLOCATABLE :: tokens(:)
          tokens = tokenize_json()
          
        END FUNCTION parse_json
      END MODULE json_parser

