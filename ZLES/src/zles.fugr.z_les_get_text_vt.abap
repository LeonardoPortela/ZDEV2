FUNCTION Z_LES_GET_TEXT_VT.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_TKNUM) TYPE  TKNUM
*"     VALUE(I_ID_TEXT) TYPE  THEAD-TDID DEFAULT 'CM18'
*"  TABLES
*"      TL_TLINE STRUCTURE  TLINE
*"----------------------------------------------------------------------

  DATA: vl_name  TYPE thead-tdname,
        tl_lines TYPE STANDARD TABLE OF tline.

*--------------------------------------------------------------------------------------------------------*
*   "Seleção das informações txt vinculada ao documento transporte
*--------------------------------------------------------------------------------------------------------*

  vl_name = i_tknum.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = i_id_text
      language                = sy-langu
      name                    = vl_name
      object                  = 'VTTK'
    TABLES
      lines                   = tl_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc EQ 0.

  ENDIF.
ENDFUNCTION.
