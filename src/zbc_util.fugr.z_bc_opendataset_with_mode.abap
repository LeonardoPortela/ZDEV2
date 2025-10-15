function z_bc_opendataset_with_mode.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(FILENAME) LIKE  RLGRAP-FILENAME
*"     REFERENCE(MODE) TYPE  XFLAG DEFAULT 'T'
*"     REFERENCE(ACESS) TYPE  XFLAG DEFAULT 'I'
*"  TABLES
*"      TABLE
*"  EXCEPTIONS
*"      OPEN_ERROR
*"      READ_ERROR
*"      WRITE_ERROR
*"      CLOSE_ERROR
*"----------------------------------------------------------------------


  data: wa_table           type string,
        file               type c length 250.
* Abrir o arquivo informado

  file = filename.
  if ( mode eq 'T' ).
    case acess.
      when 'I'.
        open dataset filename in text mode encoding default for input
                                      ignoring conversion errors
                                      with SMART LINEFEED.
      when 'O'.
        open dataset filename in text mode encoding default for output
                                      ignoring conversion errors
                                      with SMART LINEFEED.
      when 'A'.
        open dataset filename in text mode encoding default
                        for appending ignoring conversion errors
                                      with SMART LINEFEED.
      when 'U'.
        open dataset filename in text mode encoding default for update
                                      ignoring conversion errors
                                      with SMART LINEFEED.
    endcase.
  else.
    case acess.
      when 'I'.
        open dataset filename in BINARY MODE for input.
      when 'O'.
        open dataset filename in BINARY MODE for output.
      when 'A'.
        open dataset filename in BINARY MODE for appending.
      when 'U'.
        open dataset filename in BINARY MODE for update.
    endcase.
  endif.

  if ( sy-subrc ne 0 ).
    message e004(z01) with file acess mode raising open_error.
  endif.


  if ( acess eq 'I' ).
    do.
      read dataset file into wa_table.
      if ( sy-subrc ne 0 ).
        exit.
      endif.
      append wa_table to table.
    enddo.
  else.
    loop at table into wa_table.
      transfer wa_table to file.
      if ( sy-subrc ne 0 ).
        message e006(z01) with file sy-tabix raising write_error.
      endif.
    endloop.
  endif.

  close dataset file.

  if ( sy-subrc ne 0 ).
    message e007(z01) with file acess mode raising close_error.
  endif.



endfunction.
