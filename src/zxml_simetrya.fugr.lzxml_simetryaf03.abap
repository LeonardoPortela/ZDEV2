*----------------------------------------------------------------------*
***INCLUDE LZXML_SIMETRYAF03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  LIMPA_NUMERO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM limpa_numero  USING texto.

  IF texto IS NOT INITIAL.
    vg_limpo = texto.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN vg_limpo WITH ''.
  ELSE.
    CLEAR: vg_limpo.
  ENDIF.

ENDFORM.                    " LIMPA_NUMERO

*&---------------------------------------------------------------------*
*&      Form  LCT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM lct  USING texto.

  IF texto IS NOT INITIAL.
    vg_limpo = texto.
    REPLACE ALL OCCURRENCES OF 'º' IN vg_limpo WITH 'o'.
    REPLACE ALL OCCURRENCES OF 'ª' IN vg_limpo WITH 'a'.
  ELSE.
    CLEAR: vg_limpo.
  ENDIF.

ENDFORM.                    " LCT
