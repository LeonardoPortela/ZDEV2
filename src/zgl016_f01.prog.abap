*&---------------------------------------------------------------------*
*&  Include           ZGL016_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_SORT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_MONTAR_SORT.

  WG_SORT-SPOS       = 1.
  WG_SORT-FIELDNAME  = 'DOC_LCTO'.
*  wg_sort-tabname    = .
*  wg_sort-group      = .
  WG_SORT-SUBTOT     = C_X.
  APPEND WG_SORT TO TG_SORT.

ENDFORM.                    " F_MONTAR_SORT

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_MONTAR_LAYOUT  USING P_EDIT.
  REFRESH TG_FIELDCATALOG.
  PERFORM F_MONTAR_ESTRUTURA USING:
        0   ' '   ' '   'TG_ITEM'     'STATUS'            TEXT-A01              '07'   ' ' ' ' ' ' ' ' 'X',
        1   ' '   ' '   'TG_ITEM'     'DOC_LCTO'          TEXT-A02              '12'  ' ' ' ' ' ' ' ' ' ',
        2   ' '   ' '   'TG_ITEM'     'TP_LCTO'           TEXT-A03              '08'  ' ' ' ' ' ' ' ' ' ',
        3   ' '   ' '   'TG_ITEM'     'MOEDA_DOC'         TEXT-A04              '12'  ' ' ' ' ' ' ' ' ' ',
        6   ' '   ' '   'TG_ITEM'     'BSCHL'             TEXT-A05              '15'  ' ' ' ' ' ' ' ' ' ',
        7   ' '   ' '   'TG_ITEM'     'HKONT'             TEXT-A06              '15'  ' ' ' ' ' ' ' ' ' ',
        8   ' '   ' '   'TG_ITEM'     'DESCR'             TEXT-A07              '40'  ' ' ' ' ' ' ' ' ' ',
        9   ' '   ' '   'TG_ITEM'     'VLR_MOEDA_DOC'     TEXT-A25              '20'  ' ' 'X' ' ' ' ' ' ',
        10  ' '   ' '   'TG_ITEM'     'VLR_MOEDA_FORTE'   TEXT-A22              '20'  ' ' 'X' ' ' ' ' ' ',
        11  ' '   ' '   'TG_ITEM'     'VLR_MOEDA_INT'     TEXT-A21              '20'  ' ' 'X' ' ' ' ' ' ',
        12  ' '   ' '   'TG_ITEM'     'VLR_MOEDA_GRUPO'   TEXT-A23              '20'  ' ' 'x' ' ' ' ' ' ',
        13  ' '   ' '   'TG_ITEM'     'UMSKZ'             TEXT-A08              '12'  ' ' ' ' ' ' ' ' ' ',
        14  ' '   ' '   'TG_ITEM'     'ANBWA'             TEXT-A09              '12'  ' ' ' ' ' ' ' ' ' ',
        15  ' '   ' '   'TG_ITEM'     'BEWAR'             TEXT-A10              '12'  ' ' ' ' ' ' ' ' ' ',
        16  ' '   ' '   'TG_ITEM'     'VBUND'             TEXT-A12              '12'  ' ' ' ' ' ' ' ' ' ',
        17  ' '   ' '   'TG_ITEM'     'KOSTL'             TEXT-A13              '12'  ' ' ' ' ' ' ' ' ' ',
        18  ' '   ' '   'TG_ITEM'     'PRCTR'             TEXT-A14              '12'  ' ' ' ' ' ' ' ' ' ',
        19  ' '   ' '   'TG_ITEM'     'AUFNR'             TEXT-A15              '12'  ' ' ' ' ' ' ' ' ' ',
        20  ' '   ' '   'TG_ITEM'     'MATNR'             TEXT-A24              '12'  ' ' ' ' ' ' ' ' ' ',
        21  ' '   ' '   'TG_ITEM'     'MATNR_FI'          TEXT-A16              '12'  ' ' ' ' ' ' ' ' ' ',
        22  ' '   ' '   'TG_ITEM'     'D_C'               TEXT-A17              '5'   ' ' ' ' ' ' ' ' ' ',
        23  ' '   ' '   'TG_ITEM'     'GSBER'             TEXT-A18              '12'  ' ' ' ' ' ' ' ' ' ',
        24  ' '   ' '   'TG_ITEM'     'ZUONR'             TEXT-A19              '12'  ' ' ' ' ' ' ' ' ' ',
        25  ' '   ' '   'TG_ITEM'     'SGTXT'             TEXT-A20              '25'  ' ' ' ' ' ' ' ' ' ',
* Início - CS2019001942 - Sara Oikawa - Jun/2020
        26  ' '   ' '   'TG_ITEM'     'IVA'               TEXT-A26              '02'  ' ' ' ' ' ' ' ' ' ',
        27  ' '   ' '   'TG_ITEM'     'IVA_TXT'           TEXT-A27              '30'  ' ' ' ' ' ' ' ' ' '.
* Fim - CS2019001942 - Sara Oikawa - Jun/2020


ENDFORM.                    " F_MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_MONTAR_ESTRUTURA  USING  P_COL_POS     P_REF_TABNAME   P_REF_FIELDNAME
                                P_TABNAME     P_FIELD         P_SCRTEXT_L
                                P_OUTPUTLEN   P_EDIT          P_SUM
                                P_EMPHASIZE   P_F4            P_ICO.


  CLEAR WG_FIELDCATALOG.
  WG_FIELDCATALOG-FIELDNAME    = P_FIELD.
  WG_FIELDCATALOG-TABNAME      = P_TABNAME.
  WG_FIELDCATALOG-REF_TABLE    = P_REF_TABNAME.
  WG_FIELDCATALOG-REF_FIELD    = P_REF_FIELDNAME.
  WG_FIELDCATALOG-KEY          = ' '.
  WG_FIELDCATALOG-EDIT         = P_EDIT.
  WG_FIELDCATALOG-DO_SUM       = P_SUM.

  WG_FIELDCATALOG-COL_POS      = P_COL_POS.

  IF P_OUTPUTLEN IS NOT INITIAL.
    WG_FIELDCATALOG-OUTPUTLEN  = P_OUTPUTLEN.
  ENDIF.

  WG_FIELDCATALOG-NO_OUT       = ' '.
  WG_FIELDCATALOG-REPTEXT      = P_SCRTEXT_L.
  WG_FIELDCATALOG-SCRTEXT_S    = P_SCRTEXT_L.
  WG_FIELDCATALOG-SCRTEXT_M    = P_SCRTEXT_L.
  WG_FIELDCATALOG-SCRTEXT_L    = P_SCRTEXT_L.
  WG_FIELDCATALOG-EMPHASIZE    = P_EMPHASIZE.

  IF P_F4 IS NOT INITIAL.
    WG_FIELDCATALOG-F4AVAILABL = C_X.
  ENDIF.

  IF P_ICO IS NOT INITIAL.
*    wg_fieldcatalog-hotspot    = c_x.
    WG_FIELDCATALOG-ICON       = C_X.
  ENDIF.

  APPEND WG_FIELDCATALOG TO TG_FIELDCATALOG.
ENDFORM.                    " F_MONTAR_ESTRUTURA

*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_BUSCA_DADOS.
  DATA: WL_ZGLT035        TYPE ZGLT035,
        TL_ZGLT035        TYPE TABLE OF ZGLT035,
        WL_ZGLT034        TYPE ZGLT034,
        WL_T001           TYPE T001,
        WL_ZIMP_CAD_DEPTO TYPE ZIMP_CAD_DEPTO,
        TL_ZGLT036        TYPE TABLE OF ZGLT036 WITH HEADER LINE,
        WL_TBSL           TYPE TBSL.

* Início - CS2019001942 - Sara Oikawa - Jun/2020
  DATA: WL_T005  TYPE TY_T005,
        TL_T007S TYPE TABLE OF T007S,
        WL_T007S TYPE T007S.
* Fim - CS2019001942 - Sara Oikawa - Jun/2020

  REFRESH: TG_ITEM.
  CHECK WG_CABECALHO-LOTE IS NOT INITIAL.

  SELECT SINGLE * FROM ZGLT034
      INTO WL_ZGLT034
    WHERE LOTE EQ WG_CABECALHO-LOTE.

  IF  WL_ZGLT034-STATUS_LOTE EQ 'L'.
    MESSAGE TEXT-I01 TYPE 'I'.
    EXIT.
  ENDIF.
  IF WL_ZGLT034-STATUS_LOTE NE ' '.
    MESSAGE TEXT-I02  TYPE 'I'.
    EXIT.
  ENDIF.
  WG_CABECALHO-BUKRS = WL_ZGLT034-BUKRS.
*  SELECT SINGLE * FROM ZGLT035
*    INTO WL_ZGLT035
*  WHERE BUKRS EQ WG_CABECALHO-BUKRS
*    AND LOTE  EQ WG_CABECALHO-LOTE.

  SELECT  * FROM ZGLT035
    INTO TABLE TL_ZGLT035
  WHERE BUKRS EQ WG_CABECALHO-BUKRS
    AND LOTE  EQ WG_CABECALHO-LOTE
    AND LOEKZ EQ ''.

  IF SY-SUBRC = 0.
    SELECT SINGLE * FROM ZIMP_CAD_DEPTO
      INTO WL_ZIMP_CAD_DEPTO
    WHERE DEP_RESP EQ WL_ZGLT034-DEP_RESP.

    MOVE: WL_ZGLT034-DESCR_LOTE     TO WG_CABECALHO-DESCR_LOTE,
          WL_ZGLT034-DEP_RESP      TO WG_CABECALHO-DPTO_RESP,
          WL_ZIMP_CAD_DEPTO-DEP_RESP_DESC   TO WG_CABECALHO-DEPARTAMENTO.

    SELECT SINGLE *
      FROM T001
      INTO WL_T001
      WHERE BUKRS EQ WG_CABECALHO-BUKRS.

    WG_CABECALHO-BUTXT  = WL_T001-BUTXT.

* Início - CS2019001942 - Sara Oikawa - Jun/2020
* Buscar Descrições IVA para a empresa
    REFRESH TL_T007S.
    CLEAR   WL_T005.

    SELECT SINGLE LAND1 KALSM
      FROM T005
      INTO WL_T005
     WHERE LAND1 EQ WL_T001-LAND1.

    SELECT *
      FROM T007S
      INTO TABLE TL_T007S
    WHERE SPRAS EQ SY-LANGU
      AND KALSM EQ WL_T005-KALSM.
* Fim  - CS2019001942 - Sara Oikawa - Jun/2020

    "Selecionar Itens
    SELECT * FROM ZGLT036
      INTO TABLE TL_ZGLT036
      FOR ALL ENTRIES IN TL_ZGLT035
    WHERE DOC_LCTO EQ TL_ZGLT035-DOC_LCTO.

    CLEAR: WG_ITEM, TL_ZGLT036.
    LOOP AT TL_ZGLT036.
      MOVE-CORRESPONDING TL_ZGLT036 TO WG_ITEM.

      CASE WL_ZGLT034-STATUS_LOTE.
        WHEN 'L'.
          WG_ITEM-STATUS = '@01@'.
        WHEN ''.
          WG_ITEM-STATUS = '@1A@'.
        WHEN OTHERS.
      ENDCASE.

      SELECT SINGLE * FROM TBSL INTO WL_TBSL WHERE BSCHL EQ WG_ITEM-BSCHL.
      IF SY-SUBRC EQ 0.
        CASE WL_TBSL-KOART.
          WHEN 'K'.
            SELECT SINGLE NAME1 FROM LFA1 INTO WG_ITEM-DESCR WHERE LIFNR EQ WG_ITEM-HKONT.
          WHEN 'D'.
            SELECT SINGLE NAME1 FROM KNA1 INTO WG_ITEM-DESCR WHERE KUNNR EQ WG_ITEM-HKONT.
          WHEN 'S'.
            SELECT SINGLE TXT50 FROM SKAT INTO WG_ITEM-DESCR WHERE SAKNR EQ WG_ITEM-HKONT
                                                                 AND SPRAS EQ SY-LANGU
                                                                 AND KTOPL EQ '0050'.
          WHEN 'I'.
            SELECT SINGLE MCOA1 FROM ANLA INTO WG_ITEM-DESCR WHERE ANLN1 EQ WG_ITEM-HKONT.
        ENDCASE.

        IF WL_TBSL-SHKZG EQ 'S'.
          WG_ITEM-D_C = 'D'.
        ELSE.
          WG_ITEM-D_C = 'C'.
          MULTIPLY WG_ITEM-VLR_MOEDA_DOC BY -1.
          MULTIPLY WG_ITEM-VLR_MOEDA_INT BY -1.
*          MULTIPLY WG_ITEM-VLR_MOEDA_INT BY -1.
          MULTIPLY WG_ITEM-VLR_MOEDA_FORTE BY -1.
          MULTIPLY WG_ITEM-VLR_MOEDA_GRUPO BY -1.
        ENDIF.
      ENDIF.

* Início - CS2019001942 - Sara Oikawa - Jun/2020
* Preencher Informações IVA
      WG_ITEM-IVA = TL_ZGLT036-TAX_CODE.
      IF NOT TL_ZGLT036-TAX_CODE IS INITIAL.
        READ TABLE TL_T007S INTO WL_T007S WITH KEY MWSKZ = WG_ITEM-IVA.
        IF SY-SUBRC IS INITIAL.
          WG_ITEM-IVA_TXT = WL_T007S-TEXT1.
        ENDIF.
      ENDIF.
* Fim - CS2019001942 - Sara Oikawa - Jun/2020

      APPEND WG_ITEM TO TG_ITEM.
      CLEAR: WG_ITEM, WL_TBSL, TL_ZGLT036.
    ENDLOOP.
  ELSE.
    MESSAGE TEXT-I03  TYPE 'I'.
  ENDIF.

ENDFORM.                    " F_BUSCA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_LIBERAR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_LIBERAR.
  DATA:  V_LIBERADO    TYPE CHAR01.

  CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
    EXPORTING
      P_NUM_LOTE = WG_CABECALHO-LOTE
    IMPORTING
      P_LIBERADO = V_LIBERADO.

  IF V_LIBERADO = 'X'.
    PERFORM F_BUSCA_DADOS.
  ELSE.
    MESSAGE TEXT-I05 TYPE 'I'.
  ENDIF.
ENDFORM.                    " F_LIBERAR

*&---------------------------------------------------------------------*
*&      Form  F_REINICIAR
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_REINICIAR.
  DATA: WL_LOTE   TYPE ZGLT034,
        W_ERRO(1).

  CLEAR: WL_LOTE, W_ERRO.

  SELECT SINGLE * FROM ZGLT034 INTO WL_LOTE WHERE LOTE = WG_CABECALHO-LOTE.
  IF SY-SUBRC = 0.
    SELECT *
      FROM ZGLT035
      INTO TABLE IT_ZGLT035
      WHERE LOTE = WG_CABECALHO-LOTE.
    LOOP AT IT_ZGLT035 INTO WA_ZGLT035.
      CONCATENATE 'ZGL17' WA_ZGLT035-DOC_LCTO WA_ZGLT035-BUDAT+0(4) INTO WA_ZIB_CONTABIL_CHV-OBJ_KEY.
      SELECT SINGLE OBJ_KEY BELNR BUKRS GJAHR
      FROM ZIB_CONTABIL_CHV
      INTO WA_ZIB_CONTABIL_CHV
      WHERE OBJ_KEY EQ WA_ZIB_CONTABIL_CHV-OBJ_KEY.

      IF SY-SUBRC = 0.
        MESSAGE TEXT-I06  TYPE 'I'.
        W_ERRO = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF W_ERRO = 'X'.
      EXIT.
    ENDIF.

    IF WL_LOTE-STATUS_LOTE = 'A'.
      MESSAGE TEXT-I04  TYPE 'I'.
    ELSE.
      UPDATE ZGLT034
        SET STATUS_LOTE = ''
      WHERE LOTE EQ WG_CABECALHO-LOTE.

      COMMIT WORK.

      PERFORM F_BUSCA_DADOS.
    ENDIF.
  ELSE.
    MESSAGE TEXT-I05  TYPE 'I'.
  ENDIF.

ENDFORM.                    " F_REINICIAR
*&---------------------------------------------------------------------*
*&      Module  SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SEARCH_LOTE INPUT.
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
        TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_LOTE OCCURS 0,
          LOTE       TYPE ZGLT034-LOTE,
          DESCR_LOTE TYPE ZGLT034-DESCR_LOTE,
          BUKRS      TYPE ZGLT034-BUKRS,
          USNAM      TYPE ZGLT034-USNAM,
        END OF TL_LOTE.

  SELECT LOTE DESCR_LOTE BUKRS USNAM
    FROM ZGLT034
    INTO TABLE TL_LOTE
    WHERE STATUS_LOTE = ''.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'LOTE'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZGLT034-LOTE'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_LOTE
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
ENDMODULE.                 " SEARCH_LOTE  INPUT
