*----------------------------------------------------------------------*
***INCLUDE LZGLT039F03.
*----------------------------------------------------------------------*
FORM VALIDAR_AUTORIZACAO.

  DATA: PARAMETER TYPE CHAR1.
  GET PARAMETER ID 'Z_VIS_ZGL021' FIELD PARAMETER.

  IF PARAMETER EQ ABAP_TRUE.
    VIM_AUTH_RC = 0.
  ELSE.
    VIM_AUTH_RC = 8.
    VIM_AUTH_MSGID = 'FI'.
    VIM_AUTH_MSGNO = 899.
    VIM_AUTH_MSGV1 = TEXT-001.
*   VIM_AUTH_MSGV2 = .
*   VIM_AUTH_MSGV3 = .
*   VIM_AUTH_MSGV4 = .
  ENDIF.

ENDFORM.
