*&---------------------------------------------------------------------*
*& Report  ZIM10
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZIM10.
TYPE-POOLS: SLIS.
TABLES: ZIM08_REL_INV_US.

TYPES: BEGIN OF TY_SAIDA,
        VISAO TYPE ZIM08_REL_INV_US-VISAO,
        ABUKRS TYPE ZIM08_REL_INV_US-ABUKRS,
        POSNR TYPE ZIM08_REL_INV_US-POSNR,
        GJAHR TYPE ZIM08_REL_INV_US-GJAHR,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

DATA: TG_SAIDA TYPE TABLE OF TY_SAIDA WITH HEADER LINE.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER,
      LT_SORT     TYPE SLIS_T_SORTINFO_ALV,
      LS_SORT     TYPE SLIS_SORTINFO_ALV.

SELECT-OPTIONS: S_VIS   FOR ZIM08_REL_INV_US-VISAO,
                S_BUKRS FOR ZIM08_REL_INV_US-ABUKRS,
                S_POSNR FOR ZIM08_REL_INV_US-POSNR.

PARAMETERS: P_DELE AS CHECKBOX.



START-OF-SELECTION.
*delete FROM ZIM08_REL_INV_US WHERE visao  eq p_vis
*                               and abukrs eq p_bukrs
*                               and posnr  eq p_posnr.

  DATA: TL_USD TYPE TABLE OF ZIM08_REL_INV_US WITH HEADER LINE,
        WL_CONT TYPE SY-TABIX.

  SELECT *
    FROM ZIM08_REL_INV_US
    INTO TABLE TL_USD
     WHERE VISAO IN S_VIS
       AND ABUKRS IN S_BUKRS
       AND POSNR  IN S_POSNR.


  IF SY-SUBRC IS INITIAL.
    LOOP AT TL_USD.
      IF TL_USD-GJAHR EQ '    '.
        ADD 1 TO WL_CONT.
        MOVE-CORRESPONDING: TL_USD TO TG_SAIDA.
        APPEND TG_SAIDA.
        IF P_DELE IS NOT INITIAL.
          DELETE ZIM08_REL_INV_US FROM TL_USD.
        ENDIF.
      ENDIF.
    ENDLOOP.
*    WRITE WL_CONT.
    IF WL_CONT IS NOT INITIAL.
*---> 04/07/2023 - Migração S4 - WS
  SORT TG_SAIDA.
*<--- 04/07/2023 - Migração S4 - WS
      DELETE ADJACENT DUPLICATES FROM TG_SAIDA COMPARING ALL FIELDS.
      MESSAGE S836(SD) WITH WL_CONT
                            ' Linhas foram eliminadas!'.
      PERFORM IMPRIMIR_DADOS.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
  PERFORM MONTAR_LAYOUT.

  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
   EXPORTING
     I_CALLBACK_PROGRAM                = SY-REPID
*     IS_VARIANT                        = GS_VARIANT_C
*     I_CALLBACK_HTML_TOP_OF_PAGE       = 'HTML_TOP_OF_PAGE'
*     I_HTML_HEIGHT_TOP                 = '40'
*    I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
     IT_FIELDCAT                       = ESTRUTURA[]
     IS_LAYOUT                         = WL_LAYOUT
     I_SAVE                            = 'X'
     IT_EVENTS                         = EVENTS
     IS_PRINT                          = T_PRINT
     IT_SORT                           = LT_SORT

    TABLES
      T_OUTTAB                          = TG_SAIDA.
ENDFORM.                    " IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT.

  PERFORM MONTAR_ESTRUTURA USING:
        1  'ZIM08_REL_INV_US'             'VISAO'          'TG_SAIDA' 'VISAO'         ' '                   ' ' ,
        2  'ZIM08_REL_INV_US'             'ABUKRS'         'TG_SAIDA' 'ABUKRS'        ' '                   ' ' ,
        3  'ZIM08_REL_INV_US'             'POSNR'          'TG_SAIDA' 'POSNR'         ' '                   ' ' ,
        4  'ZIM08_REL_INV_US'             'GJAHR'          'TG_SAIDA' 'GJAHR'         ' '                   ' ' .


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  CLEAR WA_ESTRUTURA.

  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.


  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA
