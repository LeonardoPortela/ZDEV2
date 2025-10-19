FUNCTION z_pfe_arquivo.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(ARQUIVO) TYPE  CLIKE
*"     REFERENCE(UNIX) TYPE  CHAR01 DEFAULT 'X'
*"     REFERENCE(LOCAL) TYPE  CHAR01 OPTIONAL
*"  TABLES
*"      T_ARQUIVO STRUCTURE  ZPFE_ARQUIVO
*"  EXCEPTIONS
*"      FILE_OPEN_ERROR
*"      FILE_READ_ERROR
*"      NO_BATCH
*"      GUI_REFUSE_FILETRANSFER
*"      INVALID_TYPE
*"      NO_AUTHORITY
*"      UNKNOWN_ERROR
*"      BAD_DATA_FORMAT
*"      HEADER_NOT_ALLOWED
*"      SEPARATOR_NOT_ALLOWED
*"      HEADER_TOO_LONG
*"      UNKNOWN_DP_ERROR
*"      ACCESS_DENIED
*"      DP_OUT_OF_MEMORY
*"      DISK_FULL
*"      DP_TIMEOUT
*"      OUTROS
*"----------------------------------------------------------------------

  IF unix EQ 'X'.
*   Lê arquivo UNIX
    OPEN DATASET arquivo FOR INPUT IN BINARY MODE.
    DO.
      READ DATASET arquivo INTO t_arquivo.
      IF sy-subrc IS INITIAL.
        APPEND t_arquivo.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ELSEIF local EQ 'X'.
*   Lê arquivo WINDOWS
    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = arquivo
        filetype                = 'ASC'
      TABLES
        data_tab                = t_arquivo
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.

    CASE sy-subrc.
      WHEN 01.
        RAISE file_open_error.
      WHEN 02.
        RAISE file_read_error.
      WHEN 03.
        RAISE no_batch.
      WHEN 04.
        RAISE gui_refuse_filetransfer.
      WHEN 05.
        RAISE invalid_type.
      WHEN 06.
        RAISE no_authority.
      WHEN 07.
        RAISE unknown_error.
      WHEN 08.
        RAISE bad_data_format.
      WHEN 09.
        RAISE header_not_allowed.
      WHEN 10.
        RAISE separator_not_allowed.
      WHEN 11.
        RAISE header_too_long.
      WHEN 12.
        RAISE unknown_dp_error.
      WHEN 13.
        RAISE access_denied.
      WHEN 14.
        RAISE dp_out_of_memory.
      WHEN 15.
        RAISE disk_full.
      WHEN 16.
        RAISE dp_timeout.
      WHEN 17.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING outros.

    ENDCASE.

  ENDIF.



ENDFUNCTION.
