************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...:                                                     *
* Data desenv ...: 23.01.2013                                          *
* Objetivo    ...: Atualiza Tables                                *
* Transação   ...: ZMMY0002                                            *
************************************************************************
* Data Modif    Autor         Descriçao      Hora           Request    *
************************************************************************
* 23.01,2013   Antonio Luiz   Criação       07:37                      *
************************************************************************

REPORT  zgl_dre_dados.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zgl022_dre_dados, zgl023_dre_dados, zgl024_dre_dados.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
      BEGIN OF ty_table,
        bukrs TYPE zgl022_dre_dados-bukrs,
        monat TYPE zgl022_dre_dados-monat,
        gjahr TYPE zgl022_dre_dados-gjahr,
        saknr TYPE zgl022_dre_dados-saknr,
        kostl TYPE zgl022_dre_dados-kostl,
        prctr TYPE zgl023_dre_dados-prctr,
        matkl TYPE zgl024_dre_dados-matkl,
        vlr_rea TYPE zgl022_dre_dados-vlr_rea,
        vlr_dolar TYPE zgl022_dre_dados-vlr_dolar,
        vlr_grupo TYPE zgl022_dre_dados-vlr_grupo,

      END OF ty_table.


*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA:  it_table       TYPE TABLE OF ty_table.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*

DATA:
      wa_table       TYPE ty_table.



*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: rb1 RADIOBUTTON GROUP rad1 ,
            rb2 RADIOBUTTON GROUP rad1 ,
            rb3 RADIOBUTTON GROUP rad1 ,
            rb4 RADIOBUTTON GROUP rad1 ,
            rb5 RADIOBUTTON GROUP rad1 ,
            rb6 RADIOBUTTON GROUP rad1 .
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
        p_bukrs  FOR zgl022_dre_dados-bukrs ,
        p_monat  FOR zgl022_dre_dados-monat ,
        p_gjahr  FOR zgl022_dre_dados-gjahr ,
        p_saknr  FOR zgl022_dre_dados-saknr ,
        p_kostl  FOR zgl022_dre_dados-kostl	,
        p_prctr  FOR zgl023_dre_dados-prctr	,
        p_matkl  FOR zgl024_dre_dados-matkl .
SELECTION-SCREEN: END OF BLOCK b1.



*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM:
            f_seleciona_dados     .


END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .
  DATA: wconts TYPE i VALUE 0,
        wcontg TYPE i VALUE 0,
        w1(6)  type c,
        w2(6)  type c,
        Mensa(50) type c.


  IF rb1 = 'X'.
    SELECT bukrs  monat  gjahr  saknr   kostl kostl kostl vlr_rea   vlr_dolar  vlr_grupo
      FROM zgl022_dre_dados
      INTO TABLE it_table
      WHERE bukrs  in p_bukrs
      AND monat    in p_monat
      AND gjahr    in p_gjahr
      AND saknr    in p_saknr
      AND kostl    in p_kostl.
  ENDIF.

  IF rb2 = 'X'.
    SELECT bukrs  monat     gjahr  saknr   kostl kostl kostl vlr_rea   vlr_dolar  vlr_grupo
      FROM zgl026_dre_acm
      INTO TABLE it_table
      WHERE bukrs  in p_bukrs
      AND monat    in p_monat
      AND gjahr    in p_gjahr
      AND saknr    in p_saknr
      AND kostl    in p_kostl.
  ENDIF.

  IF rb3 = 'X'.
    SELECT bukrs  monat     gjahr  saknr   prctr prctr prctr vlr_rea   vlr_dolar  vlr_grupo
      FROM zgl023_dre_dados
      INTO TABLE it_table
      WHERE bukrs  in p_bukrs
      AND monat    in p_monat
      AND gjahr    in p_gjahr
      AND saknr    in p_saknr
      AND prctr    in p_prctr.
  ENDIF.

  IF rb4 = 'X'.
    SELECT bukrs  monat     gjahr  saknr   prctr prctr prctr vlr_rea   vlr_dolar  vlr_grupo
      FROM zgl027_dre_acm
      INTO TABLE it_table
      WHERE bukrs  in p_bukrs
      AND monat    in p_monat
      AND gjahr    in p_gjahr
      AND saknr    in p_saknr
      AND prctr    in p_prctr.
  ENDIF.
  IF rb5 = 'X'.
    SELECT bukrs  monat     gjahr  saknr   matkl matkl matkl vlr_rea   vlr_dolar  vlr_grupo
      FROM zgl024_dre_dados
      INTO TABLE it_table
      WHERE bukrs  in p_bukrs
      AND monat    in p_monat
      AND gjahr    in p_gjahr
      AND saknr    in p_saknr
      AND matkl    in p_matkl.
  ENDIF.

  IF rb6 = 'X'.
    SELECT bukrs  monat     gjahr  saknr   matkl matkl matkl vlr_rea   vlr_dolar  vlr_grupo
      FROM zgl028_dre_acm
      INTO TABLE it_table
      WHERE bukrs  in p_bukrs
      AND monat    in p_monat
      AND gjahr    in p_gjahr
      AND saknr    in p_saknr
      AND matkl    in p_matkl.
  ENDIF.

  LOOP AT it_table INTO wa_table.
    ADD 1 TO wconts.
    IF rb1 = 'X'.
      UPDATE zgl029_dre_dados SET vlr_rea     = wa_table-vlr_rea
                                  vlr_dolar   = wa_table-vlr_dolar
                                  vlr_grupo   = wa_table-vlr_grupo
      WHERE   bukrs  = wa_table-bukrs
      AND     monat  = wa_table-monat
      AND    gjahr    = wa_table-gjahr
      AND    saknr    = wa_table-saknr
      AND    kostl   = wa_table-kostl.
      IF sy-subrc = 0.
        ADD 1 TO wcontg.
      ENDIF.
    ENDIF.

    IF rb2 = 'X'.
      UPDATE zgl030_dre_acm  SET vlr_rea     = wa_table-vlr_rea
                                  vlr_dolar   = wa_table-vlr_dolar
                                  vlr_grupo   = wa_table-vlr_grupo
      WHERE   bukrs  = wa_table-bukrs
      AND     monat  = wa_table-monat
      AND    gjahr    = wa_table-gjahr
      AND    saknr    = wa_table-saknr
      AND    kostl   = wa_table-kostl.
      IF sy-subrc = 0.
        ADD 1 TO wcontg.
      ENDIF.
    ENDIF.

    IF rb3 = 'X'.
      UPDATE zgl029_dre_dados  SET vlr_rea     = wa_table-vlr_rea
                                  vlr_dolar   = wa_table-vlr_dolar
                                  vlr_grupo   = wa_table-vlr_grupo
      WHERE   bukrs  = wa_table-bukrs
      AND     monat  = wa_table-monat
      AND    gjahr    = wa_table-gjahr
      AND    saknr    = wa_table-saknr
      AND    prctr   = wa_table-prctr.
      IF sy-subrc = 0.
        ADD 1 TO wcontg.
      ENDIF.
    ENDIF.

    IF rb4 = 'X'.
      UPDATE ZGL030_DRE_ACM   SET vlr_rea     = wa_table-vlr_rea
                                  vlr_dolar   = wa_table-vlr_dolar
                                  vlr_grupo   = wa_table-vlr_grupo
      WHERE   bukrs  = wa_table-bukrs
      AND     monat  = wa_table-monat
      AND    gjahr    = wa_table-gjahr
      AND    saknr    = wa_table-saknr
      AND    prctr   = wa_table-prctr.
      IF sy-subrc = 0.
        ADD 1 TO wcontg.
      ENDIF.
    ENDIF.

    IF rb5 = 'X'.
      UPDATE ZGL029_DRE_DADOS   SET vlr_rea     = wa_table-vlr_rea
                                  vlr_dolar   = wa_table-vlr_dolar
                                  vlr_grupo   = wa_table-vlr_grupo
      WHERE   bukrs  = wa_table-bukrs
      AND     monat  = wa_table-monat
      AND    gjahr    = wa_table-gjahr
      AND    saknr    = wa_table-saknr
      AND    matkl   = wa_table-matkl.
      IF sy-subrc = 0.
        ADD 1 TO wcontg.
      ENDIF.
    ENDIF.

    IF rb6 = 'X'.
      UPDATE ZGL030_DRE_ACM    SET vlr_rea     = wa_table-vlr_rea
                                  vlr_dolar   = wa_table-vlr_dolar
                                  vlr_grupo   = wa_table-vlr_grupo
      WHERE   bukrs  = wa_table-bukrs
      AND     monat  = wa_table-monat
      AND    gjahr    = wa_table-gjahr
      AND    saknr    = wa_table-saknr
      AND    matkl   = wa_table-matkl.
      IF sy-subrc = 0.
        ADD 1 TO wcontg.
      ENDIF.
    ENDIF.

  ENDLOOP.
  move: wconts to w1,
        wcontg to w2.

  CONCATENATE 'Lidos' w1 'Gravados' w2 into mensa SEPARATED BY space.
  message Mensa type 'I'.

  refresh it_table.

ENDFORM.                    " F_SELECIONA_DADOS
