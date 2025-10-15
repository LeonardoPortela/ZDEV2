*&---------------------------------------------------------------------*
*& Report  ZLESR0110_AVERBACAO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0110_averbacao MESSAGE-ID zavseguro.

TABLES: zlest0143, zlest0145.

DATA: i_filtro  TYPE zde_zlest0143_filtro,
      vg_tp_rel TYPE char01.

*=============================================================================*
*Tela_Seleção                                                                 *
*=============================================================================*
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETER ck_anil RADIOBUTTON GROUP tpp DEFAULT 'X' USER-COMMAND muda_tela.
PARAMETER ck_sint RADIOBUTTON GROUP tpp.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK ba WITH FRAME TITLE text-001.
SELECT-OPTIONS: p0001 FOR zlest0145-bukrs,
                p0002 FOR zlest0145-branch,
                p0003 FOR zlest0143-cd_seguradora,
                p0004 FOR zlest0143-docnum,
                p0005 FOR zlest0143-dt_cadastro DEFAULT sy-datum,
                p0006 FOR zlest0143-us_cadastro. "DEFAULT SY-UNAME.
SELECTION-SCREEN: END OF BLOCK ba.


START-OF-SELECTION.

  IF ck_anil IS NOT INITIAL.

    APPEND VALUE #(
    sign   =  'I'
    option =  'EQ'
    low    =  'A' "Analitico
    high = '' ) TO i_filtro-tp_relatorio.
  ELSE.

    APPEND VALUE #(
  sign   =  'I'
  option =  'EQ'
  low    =  'S' "Sintetico
  high = '' ) TO i_filtro-tp_relatorio.
  ENDIF.

  MOVE: p0001[] TO i_filtro-bukrs,
        p0002[] TO i_filtro-branch,
        p0003[] TO i_filtro-cd_seguradora,
        p0004[] TO i_filtro-docnum,
        p0005[] TO i_filtro-dt_cadastro,
        p0006[] TO i_filtro-us_cadastro,
        p0006[] TO i_filtro-us_cadastro.

  CALL FUNCTION 'ZLES_CADASTRO_AVSEG'
    EXPORTING
      i_filtro = i_filtro.
