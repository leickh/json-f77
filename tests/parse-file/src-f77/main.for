
      PROGRAM json_parse_file
        USE json
        IMPLICIT NONE

        INTEGER(4),POINTER        :: tokens(:)
        CHARACTER,POINTER         :: json_source(:)
        INTEGER                   :: location

        CHARACTER(1024),TARGET    :: input_path
        CHARACTER(1024),POINTER   :: input_path_pointer

        CHARACTER(1024),TARGET    :: directive_path
        CHARACTER(1024),POINTER   :: directive_path_pointer

        input_path = "input.json"
        input_path_pointer => input_path

        directive_path = "/programming_languages[1]/name"
        directive_path_pointer => directive_path

        CALL tokenize_json_file(input_path_pointer, json_source, tokens)
        location = search_json_location(tokens, json_source,            &
     &    directive_path_pointer)

        DEALLOCATE(json_source)
      END

