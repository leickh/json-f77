
      MODULE json_parser
        USE json_tokenizer
      CONTAINS
        FUNCTION parse_json(source) RESULT(tokens)
          CHARACTER,POINTER :: source(:)
          BYTE,POINTER :: tokens(:)
          tokens => tokenize_json(source)

          RETURN
        END FUNCTION parse_json
      END MODULE json_parser

