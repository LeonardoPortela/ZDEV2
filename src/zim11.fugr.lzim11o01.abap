*----------------------------------------------------------------------*
***INCLUDE LZIM11O01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_DESCRICAO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atualiza_descricao OUTPUT.

  IF NOT zim011-cod_gpo IS INITIAL.

    SELECT SINGLE descr_grupo FROM zim010
      INTO zim011-descr_grupo
      WHERE cod_gpo = zim011-cod_gpo.

  ENDIF.


  IF NOT zim011-knttp IS INITIAL.
    SELECT SINGLE knttx FROM t163i INTO zim011-knttx
      WHERE spras = sy-langu AND
            knttp = zim011-knttp.
  ELSE.
    CLEAR zim011-knttx.
  ENDIF.

  IF NOT zim011-saknr IS INITIAL.
    SELECT SINGLE txt20 FROM skat INTO zim011-txt20
      WHERE spras = sy-langu AND
            saknr = zim011-saknr.
  ELSE.
    CLEAR zim011-txt20.
  ENDIF.

ENDMODULE.                 " ATUALIZA_DESCRICAO  OUTPUT
