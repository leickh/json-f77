
      PROGRAM json_write_tokens
        USE json_tokenizer
        IMPLICIT NONE

        INTEGER(4),POINTER :: tokens(:)
        CHARACTER,POINTER :: source(:)

        INTEGER(4) :: num_tokens
        INTEGER :: file_status
        INTEGER :: len_file
        INTEGER :: file_id
        INQUIRE(FILE="input.json", SIZE=len_file)
        
        OPEN(NEWUNIT=file_id, FILE="input.json", ACTION="read", IOSTAT= &
     &file_status, FORM="UNFORMATTED", ACCESS="DIRECT", RECL=len_file)

        IF(file_status .NE. 0) THEN
          WRITE (*, *) "error: failed opening the json-file."
          STOP
        ENDIF

        ALLOCATE(source(len_file))
        READ(UNIT=file_id, IOSTAT=file_status, REC=1) source
        CLOSE(UNIT=file_id)

        CALL tokenize_json(source, len_file, tokens, num_tokens)
        CALL write_tokens(tokens, num_tokens, source)

        DEALLOCATE(tokens)
        DEALLOCATE(source)
      END

