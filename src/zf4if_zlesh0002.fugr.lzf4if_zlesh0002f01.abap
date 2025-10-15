*----------------------------------------------------------------------*
***INCLUDE LZF4IF_ZLESH0002F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_COUNTRY                                          *
*&---------------------------------------------------------------------*
*                               Busca País                             *
*----------------------------------------------------------------------*
FORM z_busca_country USING p_campo   TYPE c
                  CHANGING p_country TYPE j_1btreg_city-country.

  DATA: tl_dynpread TYPE TABLE OF dynpread,
        sl_dynpread TYPE dynpread         .

  CLEAR p_country.
  sl_dynpread-fieldname = p_campo.
  APPEND sl_dynpread TO tl_dynpread.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = 'SAPLZLES'
      dynumb               = '0200'
    TABLES
      dynpfields           = tl_dynpread
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  CHECK sy-subrc IS INITIAL.

  READ TABLE tl_dynpread INTO sl_dynpread INDEX 1.
  p_country = sl_dynpread-fieldvalue.

ENDFORM.                    " Z_BUSCA_COUNTRY

*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_REGION                                           *
*&---------------------------------------------------------------------*
*                              Busca Região                            *
*----------------------------------------------------------------------*
FORM z_busca_region USING p_campo  TYPE c
                 CHANGING p_region TYPE j_1btreg_city-region.

  DATA: tl_dynpread TYPE TABLE OF dynpread,
        sl_dynpread TYPE dynpread         .

  CLEAR p_region.
  sl_dynpread-fieldname = p_campo.
  APPEND sl_dynpread TO tl_dynpread.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = 'SAPLZLES'
      dynumb               = '0200'
    TABLES
      dynpfields           = tl_dynpread
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.

  CHECK sy-subrc IS INITIAL.

  READ TABLE tl_dynpread INTO sl_dynpread INDEX 1.
  p_region = sl_dynpread-fieldvalue.

ENDFORM.                    " Z_BUSCA_REGION
