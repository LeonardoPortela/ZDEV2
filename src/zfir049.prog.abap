*&---------------------------------------------------------------------*
*& Report  ZFIR049
*&
*& Relatório para listar as partidas em aberto para acompanhamento do
*& departamento de risco
*&---------------------------------------------------------------------*
*& Autor            Request       Data
*& Marcos Faneli    DEVK937219    02.05.2014
*& Marcos Faneli    DEVK937377    07.05.2014
*&---------------------------------------------------------------------*

REPORT  ZFIR049.

* Tabelas
*----------------------------------------------------------------------*
TABLES: BSID,
        KNA1,
        T001,
        VBAK,
        VBAP,
        TVAKT,
        LFA1,
        BSIK,
        EKKO,
        BKPF,
        T161T.

* Estruturas
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF TY_BSID, "Dados de Clientes
    KUNNR TYPE BSID-KUNNR,
    BUDAT TYPE BSID-BUDAT,
    BELNR TYPE BSID-BELNR,
    BLART TYPE BSID-BLART,
    WAERS TYPE BSID-WAERS,
    XBLNR TYPE BSID-XBLNR,
    SHKZG TYPE BSID-SHKZG,
    GSBER TYPE BSID-GSBER,
    SGTXT TYPE BSID-SGTXT,
    ZFBDT TYPE BSID-ZFBDT,
    ZBD1T TYPE BSID-ZBD1T,
    VBELN TYPE BSID-VBELN, "Fatura
    VBEL2 TYPE BSID-VBEL2, "ov
    ZLSCH TYPE BSID-ZLSCH,
    DMBTR TYPE BSID-DMBTR,
    DMBE2 TYPE BSID-DMBE2,
    UMSKS TYPE BSID-UMSKS,
    BUKRS TYPE BSID-BUKRS,
    HKONT TYPE BSID-HKONT,
  END OF TY_BSID,

  BEGIN OF TY_T001,
    BUKRS TYPE T001-BUKRS,
    BUTXT TYPE T001-BUTXT,
  END OF TY_T001,

  BEGIN OF TY_KNA1,
    KUNNR TYPE KNA1-KUNNR,
    NAME1 TYPE KNA1-NAME1,
    KTOKD TYPE KNA1-KTOKD,
  END OF TY_KNA1,

  BEGIN OF TY_VBAK,
    VBELN TYPE VBAK-VBELN,
    AUART TYPE VBAK-AUART,
  END OF TY_VBAK,

  BEGIN OF TY_VBAP,
    VBELN TYPE VBAP-VBELN,
    MATNR TYPE VBAP-MATNR,
    ARKTX TYPE VBAP-ARKTX,
  END OF TY_VBAP,

  BEGIN OF TY_TVAKT,
    AUART TYPE TVAKT-AUART,
    SPRAS TYPE TVAKT-SPRAS,
    BEZEI TYPE TVAKT-BEZEI,
    BSART TYPE T161T-BSART,
    BATXT TYPE T161T-BATXT,
  END OF TY_TVAKT,

  BEGIN OF TY_BSIK, "Dados do fornecedor
    LIFNR  TYPE BSIK-LIFNR,
    BUDAT  TYPE BSIK-BUDAT,
    BELNR  TYPE BSIK-BELNR,
    BLART  TYPE BSIK-BLART,
    BUKRS  TYPE BSIK-BUKRS,
    GJAHR  TYPE BSIK-GJAHR,
    WAERS  TYPE BSIK-WAERS,
    XBLNR  TYPE BSIK-XBLNR,
    SHKZG  TYPE BSIK-SHKZG,
    GSBER  TYPE BSIK-GSBER,
    SGTXT  TYPE BSIK-SGTXT,
    ZFBDT  TYPE BSIK-ZFBDT,
    ZBD1T  TYPE BSIK-ZBD1T,
    ZLSCH  TYPE BSIK-ZLSCH,
    DMBTR  TYPE BSIK-DMBTR,
    DMBE2  TYPE BSIK-DMBE2,
    UMSKS  TYPE BSIK-UMSKS,
    ZUONR  TYPE BSIK-ZUONR,
    ZUONR2 TYPE BSIK-EBELN,
    EBELN  TYPE BSIK-EBELN,
    HKONT  TYPE BSIK-HKONT,
  END OF TY_BSIK,

  BEGIN OF TY_LFA1,
    LIFNR	TYPE LFA1-LIFNR,
    NAME1 TYPE LFA1-NAME1,
    KTOKK TYPE LFA1-KTOKK,
  END OF TY_LFA1,

  BEGIN OF TY_EKKO,
    EBELN	TYPE EKKO-EBELN,
    BSART TYPE EKKO-BSART,
  END OF TY_EKKO,

  BEGIN OF TY_BKPF,
    BUKRS TYPE BKPF-BUKRS,
    GJAHR TYPE BKPF-GJAHR,
    TCODE TYPE BKPF-TCODE,
    AWKEY TYPE BKPF-AWKEY,
  END OF TY_BKPF,

  BEGIN OF TY_T161T,
    BSART TYPE T161T-BSART,
    BATXT TYPE T161T-BATXT,
  END OF TY_T161T,

  BEGIN OF TY_SAIDA,
    KUNNR       TYPE BSID-KUNNR,
    BUKRS       TYPE BSID-BUKRS,
    BUTXT       TYPE T001-BUTXT,
    BUDAT       TYPE BSID-BUDAT,
    BELNR       TYPE BSID-BELNR,
    BLART       TYPE BSID-BLART,
    WAERS       TYPE BSID-WAERS,
    XBLNR       TYPE BSID-XBLNR,
    SHKZG       TYPE BSID-SHKZG,
    GSBER       TYPE BSID-GSBER,
    SGTXT       TYPE BSID-SGTXT,
    ZFBDT       TYPE BSID-ZFBDT,
    ZBD1T       TYPE BSID-ZBD1T,
    VBELN       TYPE BSID-VBELN, "Fatura
    VBEL2       TYPE BSID-VBEL2, "ov
    ZLSCH       TYPE BSID-ZLSCH,
    DMBTR       TYPE BSID-DMBTR,
    DMBE2       TYPE BSID-DMBE2,
    UMSKS(20), "TYPE BSID-UMSKS,
    NAME1       TYPE KNA1-NAME1,
    HKONT       TYPE BSID-HKONT,
    AUART(40),
    MATNR       TYPE VBAP-MATNR,
    ARKTX       TYPE VBAP-ARKTX,
    KLIMG       TYPE KNKA-KLIMG, "#EC CI_USAGE_OK[2227014]
    GRP_CTA(35),
  END OF TY_SAIDA,

  BEGIN OF TY_ERRO,
    FIELD(30),
    MSG(30),
  END OF TY_ERRO.

* Tabelas internas
*----------------------------------------------------------------------*
DATA: IT_BSID  TYPE TABLE OF TY_BSID,
      IT_T001  TYPE TABLE OF TY_T001,
      IT_KNA1  TYPE TABLE OF TY_KNA1,
      IT_KNKA  TYPE TABLE OF KNKA,     "#EC CI_USAGE_OK[2227014]
      IT_VBAK  TYPE TABLE OF TY_VBAK,
      IT_VBAP  TYPE TABLE OF TY_VBAP,
      IT_TVAKT TYPE TABLE OF TY_TVAKT,
      IT_BSIK  TYPE TABLE OF TY_BSIK,
      IT_LFA1  TYPE TABLE OF TY_LFA1,
      IT_EKKO  TYPE TABLE OF TY_EKKO,
      IT_BKPF  TYPE TABLE OF TY_BKPF,
      IT_T161T TYPE TABLE OF TY_T161T,
      IT_ERRO  TYPE TABLE OF TY_ERRO,
      IT_T077Y TYPE TABLE OF T077Y,
      IT_T077X TYPE TABLE OF T077X,
      IT_SAIDA TYPE TABLE OF TY_SAIDA.

* Work Áreas
*----------------------------------------------------------------------*
DATA: WA_BSID  TYPE TY_BSID,
      WA_T001  TYPE TY_T001,
      WA_KNA1  TYPE TY_KNA1,
      WA_KNKA  TYPE KNKA,    "#EC CI_USAGE_OK[2227014]
      WA_VBAK  TYPE TY_VBAK,
      WA_VBAP  TYPE TY_VBAP,
      WA_TVAKT TYPE TY_TVAKT,
      WA_BSIK  TYPE TY_BSIK,
      WA_LFA1  TYPE TY_LFA1,
      WA_BKPF  TYPE TY_BKPF,
      WA_EKKO  TYPE TY_EKKO,
      WA_T161T TYPE TY_T161T,
      WA_ERRO  TYPE TY_ERRO,
      WA_T077Y TYPE T077Y,
      WA_T077X TYPE T077X,
      WA_SAIDA TYPE TY_SAIDA.

* ALV
*----------------------------------------------------------------------*
TYPE-POOLS: KKBLO,
            VRM.

DATA: ST_HEADER         TYPE KKBLO_LISTHEADER,
      IT_HEADER         TYPE KKBLO_T_LISTHEADER,
      IT_FIELDCAT       TYPE KKBLO_T_FIELDCAT,
      ST_FIELDCAT       TYPE KKBLO_FIELDCAT,
      IT_FIELDCAT_ALV   TYPE SLIS_T_FIELDCAT_ALV,
      IT_SPECIAL_GROUPS TYPE SLIS_T_SP_GROUP_ALV,
      IT_LAYOUT_ALV     TYPE SLIS_LAYOUT_ALV,
      ST_LAYOUT         TYPE KKBLO_LAYOUT,
      VC_REPID          LIKE SY-REPID,
      ST_VARIANT        TYPE DISVARIANT,
      ST_PRINT          TYPE SLIS_PRINT_ALV,
      IT_SORT           TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
      IT_EVENTOS        TYPE SLIS_T_EVENT.

* Função apresentar erros
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: P_EMP   FOR BSID-BUKRS," OBLIGATORY,
                P_DATA  FOR BSID-BUDAT NO-EXTENSION." OBLIGATORY.
PARAMETERS:     P_TIPO  AS LISTBOX VISIBLE LENGTH 15 USER-COMMAND ACT.
SELECT-OPTIONS: P_CLI   FOR BSID-KUNNR MODIF ID A,
                P_AUART FOR VBAK-AUART MODIF ID A,
                P_FOR   FOR BSIK-LIFNR MODIF ID B.

SELECTION-SCREEN: END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM LOAD_DROP_LIST USING 'P_TIPO'.

  LOOP AT SCREEN.
    IF P_TIPO = 'C'.
      IF SCREEN-GROUP1 = 'A'.
        SCREEN-ACTIVE = 1.
      ENDIF.
      IF SCREEN-GROUP1 = 'B'.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ELSEIF P_TIPO = 'F'.
      IF SCREEN-GROUP1 = 'A'.
        SCREEN-ACTIVE = 0.
      ENDIF.
      IF SCREEN-GROUP1 = 'B'.
        SCREEN-ACTIVE = 1.
      ENDIF.
    ELSE.
      IF SCREEN-GROUP1 = 'A'.
        SCREEN-ACTIVE = 0.
      ENDIF.
      IF SCREEN-GROUP1 = 'B'.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


START-OF-SELECTION.
  PERFORM: VERIFICA_ERROS.

  IF IT_ERRO[] IS INITIAL.
    PERFORM: F_SELECIONA_DADOS,
             F_SAIDA,
             F_IMPRIME_DADOS.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Verifica erros do Formulário
*&---------------------------------------------------------------------*
FORM VERIFICA_ERROS.
  REFRESH: IT_ERRO.
  IF P_EMP IS INITIAL.
    MESSAGE 'Selecione no minínmo uma empresa. ' TYPE 'I'.
    WA_ERRO-FIELD = 'P_EMP'.
    WA_ERRO-MSG = 'Selecione no minínmo uma empresa.'.
    APPEND WA_ERRO TO IT_ERRO.
    EXIT.
*    CLEAR: IT_ERRO.
  ENDIF.

  IF P_DATA IS INITIAL.
    MESSAGE 'Selecione o período.' TYPE 'I'.
    WA_ERRO-FIELD = 'P_DATA'.
    WA_ERRO-MSG = 'Selecione o período.'.
    APPEND WA_ERRO TO IT_ERRO.
    EXIT.
*    CLEAR: IT_ERRO.
  ENDIF.

  IF P_TIPO = ''.
    MESSAGE 'Selecione um tipo válido.' TYPE 'I'.
    WA_ERRO-FIELD = 'P_TIPO'.
    WA_ERRO-MSG = 'Selecione um tipo válido.'.
    APPEND WA_ERRO TO IT_ERRO.
    EXIT.
*    CLEAR: IT_ERRO.
  ENDIF.
ENDFORM.                    "VERIFICA_ERROS

*&---------------------------------------------------------------------*
*&      Form  Load_DROP_LIST
*&---------------------------------------------------------------------*
*&      Carrega as informações para o Droplist
*&---------------------------------------------------------------------*
FORM LOAD_DROP_LIST USING VALUE(F_PARAM).
  DATA:NAME  TYPE VRM_ID,
       LIST  TYPE VRM_VALUES,
       VALUE LIKE LINE OF LIST.

  VALUE-KEY = 'F'.
  VALUE-TEXT = 'Fornecedor'.
  APPEND VALUE TO LIST.

  VALUE-KEY = 'C'.
  VALUE-TEXT = 'Cliente'.
  APPEND VALUE TO LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID              = F_PARAM
      VALUES          = LIST
    EXCEPTIONS
      ID_ILLEGAL_NAME = 1
      OTHERS          = 2.
ENDFORM.                    "LOAD_DROP_LIST

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona os dados a serem usados no Report
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS.
  TRY .
      IF P_TIPO = 'C'.
        SELECT KUNNR BUDAT BELNR BLART WAERS XBLNR SHKZG GSBER SGTXT ZFBDT ZBD1T
               VBELN VBEL2 ZLSCH DMBTR DMBE2 UMSKS BUKRS HKONT
          FROM BSID
          INTO TABLE IT_BSID
          WHERE BUKRS IN P_EMP
          AND   KUNNR IN P_CLI
          AND   BLART <> 'VC'
          AND   UMSKZ <> 'F'
          AND   BUDAT IN P_DATA.

        IF IT_BSID[] IS NOT INITIAL.
          SELECT KNA1~KUNNR NAME1 KTOKD
            FROM KNA1
            INTO TABLE IT_KNA1
            FOR ALL ENTRIES IN IT_BSID
            WHERE KNA1~KUNNR = IT_BSID-KUNNR.

          IF IT_KNA1[] IS NOT INITIAL.
            SELECT *
              FROM KNKA "#EC CI_USAGE_OK[2227014]
              INTO TABLE IT_KNKA
              FOR ALL ENTRIES IN IT_KNA1
              WHERE KUNNR = IT_KNA1-KUNNR.
          ENDIF.

          SELECT *
            FROM T077X
            INTO TABLE IT_T077X
            FOR ALL ENTRIES IN  IT_KNA1
            WHERE KTOKD =  IT_KNA1-KTOKD
            AND SPRAS = SY-LANGU.

          SELECT BUKRS BUTXT
            FROM T001
            INTO TABLE IT_T001
            FOR ALL ENTRIES IN IT_BSID
            WHERE BUKRS = IT_BSID-BUKRS.

          SELECT VBELN AUART
            FROM VBAK
            INTO TABLE IT_VBAK
            FOR ALL ENTRIES IN IT_BSID
            WHERE VBELN = IT_BSID-VBEL2
            AND   AUART IN P_AUART.

          SELECT VBELN MATNR ARKTX
            FROM VBAP
            INTO TABLE IT_VBAP
            FOR ALL ENTRIES IN IT_BSID
            WHERE VBELN = IT_BSID-VBEL2.

          IF IT_VBAK IS NOT INITIAL.
            SELECT AUART SPRAS BEZEI
              FROM TVAKT
              INTO TABLE IT_TVAKT
              FOR ALL ENTRIES IN IT_VBAK
              WHERE AUART = IT_VBAK-AUART
              AND SPRAS = SY-LANGU.
          ENDIF.
        ENDIF.
      ELSEIF P_TIPO = 'F'.
        SELECT LIFNR BUDAT BELNR BLART BUKRS GJAHR WAERS XBLNR SHKZG GSBER
               SGTXT ZFBDT ZBD1T ZLSCH DMBTR DMBE2 UMSKS ZUONR ZUONR EBELN HKONT
          FROM BSIK
          INTO TABLE IT_BSIK
          WHERE BUKRS IN P_EMP
          AND   LIFNR IN P_FOR
          AND   BLART <> 'VC'
          AND   UMSKZ <> 'F'
          AND   BUDAT IN P_DATA.

        IF IT_BSIK[] IS NOT INITIAL.
          SELECT LIFNR NAME1 KTOKK
            FROM LFA1
            INTO TABLE IT_LFA1
            FOR ALL ENTRIES IN IT_BSIK
            WHERE LIFNR = IT_BSIK-LIFNR.

          SELECT *
          FROM T077Y
          INTO TABLE IT_T077Y
          FOR ALL ENTRIES IN  IT_LFA1
          WHERE KTOKK =  IT_LFA1-KTOKK
          AND SPRAS = SY-LANGU.

          SELECT BUKRS BUTXT
            FROM T001
            INTO TABLE IT_T001
            FOR ALL ENTRIES IN IT_BSIK
            WHERE BUKRS = IT_BSIK-BUKRS.

          SELECT BUKRS GJAHR TCODE AWKEY
            FROM BKPF
            INTO TABLE IT_BKPF
            FOR ALL ENTRIES IN IT_BSIK
            WHERE BUKRS = IT_BSIK-BUKRS
            AND  GJAHR = IT_BSIK-GJAHR
            AND  BELNR = IT_BSIK-BELNR.

          SORT IT_BKPF BY BUKRS GJAHR.
SELECT EBELN BSART
            FROM EKKO
            INTO TABLE IT_EKKO
            FOR ALL ENTRIES IN IT_BSIK
            WHERE EBELN = IT_BSIK-ZUONR2.

          IF IT_EKKO IS NOT INITIAL.
            SELECT BSART BATXT
              FROM T161T
              INTO TABLE IT_T161T
              FOR ALL ENTRIES IN IT_EKKO
              WHERE BSART = IT_EKKO-BSART
              AND   SPRAS = SY-LANGU.
          SORT IT_T161T BY BSART.
ENDIF.
        ENDIF.
      ENDIF.
    CATCH CX_ROOT.
      MESSAGE E000(O0) WITH 'Erro ao selecionar dados!'.
  ENDTRY.
ENDFORM.                    "SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       Gera saída para o usuário
*----------------------------------------------------------------------*
FORM F_SAIDA.
  IF P_TIPO = 'C'.
    SORT: IT_KNA1  BY KUNNR,
          IT_KNKA  BY KUNNR,
          IT_VBAK  BY VBELN,
          IT_VBAP  BY VBELN,
          IT_T001  BY BUKRS,
          IT_TVAKT BY AUART,
          IT_T077X BY KTOKD.

    LOOP AT IT_BSID INTO WA_BSID.
      WA_SAIDA-KUNNR = WA_BSID-KUNNR.
      WA_SAIDA-BUKRS = WA_BSID-BUKRS.

*     Nome da empresa
      READ TABLE IT_T001 INTO WA_T001 WITH KEY BUKRS = WA_BSID-BUKRS BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SAIDA-BUTXT = WA_T001-BUTXT.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_SAIDA-KUNNR
        IMPORTING
          OUTPUT = WA_SAIDA-KUNNR.
      WA_SAIDA-BUDAT = WA_BSID-BUDAT.
      WA_SAIDA-BELNR = WA_BSID-BELNR.
      WA_SAIDA-BLART = WA_BSID-BLART.
      WA_SAIDA-WAERS = WA_BSID-WAERS.
      WA_SAIDA-XBLNR = WA_BSID-XBLNR.
      WA_SAIDA-SHKZG = WA_BSID-SHKZG.
      WA_SAIDA-GSBER = WA_BSID-GSBER.
      WA_SAIDA-SGTXT = WA_BSID-SGTXT.
      WA_SAIDA-ZFBDT = WA_BSID-ZFBDT + WA_BSID-ZBD1T.
      WA_SAIDA-VBELN = WA_BSID-VBELN.
      WA_SAIDA-VBEL2 = WA_BSID-VBEL2.
      WA_SAIDA-ZLSCH = WA_BSID-ZLSCH.
      SHIFT WA_BSID-HKONT LEFT DELETING LEADING '0'.
      WA_SAIDA-HKONT = WA_BSID-HKONT.

      IF WA_BSID-SHKZG = 'H'.
        WA_SAIDA-DMBTR = WA_BSID-DMBTR * -1.
        WA_SAIDA-DMBE2 = WA_BSID-DMBE2 * -1.
      ELSE.
        WA_SAIDA-DMBTR = WA_BSID-DMBTR.
        WA_SAIDA-DMBE2 = WA_BSID-DMBE2.
      ENDIF.

      IF WA_BSID-UMSKS <> ''.
        WA_SAIDA-UMSKS = 'Adiantamento'.
      ELSE.
        WA_SAIDA-UMSKS = 'Fatura'.
      ENDIF.

      READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_BSID-KUNNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SAIDA-NAME1 = WA_KNA1-NAME1.
        READ TABLE IT_KNKA INTO WA_KNKA WITH KEY KUNNR = WA_KNA1-KUNNR BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-KLIMG = WA_KNKA-KLIMG.
        ENDIF.

        READ TABLE IT_T077X INTO WA_T077X WITH KEY KTOKD =  WA_KNA1-KTOKD BINARY SEARCH.
        CONCATENATE WA_KNA1-KTOKD '-' WA_T077X-TXT30 INTO WA_SAIDA-GRP_CTA.
      ENDIF.

      READ TABLE IT_VBAP INTO WA_VBAP WITH KEY VBELN = WA_BSID-VBEL2 BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SAIDA-MATNR = WA_VBAP-MATNR.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            INPUT  = WA_SAIDA-MATNR
          IMPORTING
            OUTPUT = WA_SAIDA-MATNR.
        WA_SAIDA-ARKTX = WA_VBAP-ARKTX.
      ENDIF.

      READ TABLE IT_VBAK INTO WA_VBAK WITH KEY VBELN = WA_BSID-VBEL2 BINARY SEARCH.
      IF SY-SUBRC = 0.
        IF WA_VBAK-AUART NOT IN P_AUART.
          CONTINUE.
        ENDIF.
        READ TABLE IT_TVAKT INTO WA_TVAKT WITH KEY AUART = WA_VBAK-AUART BINARY SEARCH.
        IF SY-SUBRC = 0.
          CONCATENATE WA_VBAK-AUART '-' WA_TVAKT-BEZEI INTO WA_SAIDA-AUART SEPARATED BY SPACE.
        ENDIF.
      ELSEIF P_AUART IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      APPEND WA_SAIDA TO IT_SAIDA.
      CLEAR WA_SAIDA.
    ENDLOOP.
  ELSEIF P_TIPO = 'F'.
    SORT: IT_LFA1  BY LIFNR,
          IT_T077Y BY KTOKK,
          IT_EKKO  BY EBELN,
          IT_T001  BY BUKRS,
          IT_TVAKT BY BSART.

    LOOP AT IT_BSIK INTO WA_BSIK.
      WA_SAIDA-KUNNR = WA_BSIK-LIFNR.

      WA_SAIDA-BUKRS = WA_BSIK-BUKRS.

*     Nome da empresa
      READ TABLE IT_T001 INTO WA_T001 WITH KEY BUKRS = WA_BSIK-BUKRS BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SAIDA-BUTXT = WA_T001-BUTXT.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_SAIDA-KUNNR
        IMPORTING
          OUTPUT = WA_SAIDA-KUNNR.
      WA_SAIDA-BUDAT = WA_BSIK-BUDAT.
      WA_SAIDA-BELNR = WA_BSIK-BELNR.
      WA_SAIDA-WAERS = WA_BSIK-WAERS.
      WA_SAIDA-XBLNR = WA_BSIK-XBLNR.
      WA_SAIDA-SHKZG = WA_BSIK-SHKZG.
      WA_SAIDA-GSBER = WA_BSIK-GSBER.
      WA_SAIDA-SGTXT = WA_BSIK-SGTXT.
      WA_SAIDA-ZFBDT = WA_BSIK-ZFBDT + WA_BSIK-ZBD1T.
      WA_SAIDA-BLART = WA_BSIK-BLART.
      SHIFT WA_BSIK-HKONT LEFT DELETING LEADING '0'.
      WA_SAIDA-HKONT = WA_BSIK-HKONT.
      READ TABLE IT_BKPF INTO WA_BKPF WITH KEY BUKRS = WA_BSIK-BUKRS
                                               GJAHR = WA_BSIK-GJAHR BINARY SEARCH.
      IF SY-SUBRC = 0.
        IF WA_BKPF-TCODE = 'MIRO'.
          WA_SAIDA-VBELN = WA_BKPF-AWKEY+0(11).
        ENDIF.
      ENDIF.

      WA_SAIDA-VBEL2 = WA_BSIK-ZUONR.
      WA_SAIDA-ZLSCH = WA_BSIK-ZLSCH.

      IF WA_BSIK-SHKZG = 'H'.
        WA_SAIDA-DMBTR = WA_BSIK-DMBTR * -1.
        WA_SAIDA-DMBE2 = WA_BSIK-DMBE2 * -1.
      ELSE.
        WA_SAIDA-DMBTR = WA_BSIK-DMBTR.
        WA_SAIDA-DMBE2 = WA_BSIK-DMBE2.
      ENDIF.

      IF WA_BSIK-UMSKS <> ''.
        WA_SAIDA-UMSKS = 'Adiantamento'.
      ELSE.
        WA_SAIDA-UMSKS = 'Fatura'.
      ENDIF.

      READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = WA_BSIK-LIFNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SAIDA-NAME1 = WA_LFA1-NAME1.
        READ TABLE IT_T077Y INTO WA_T077Y WITH KEY KTOKK =  WA_LFA1-KTOKK BINARY SEARCH.
        CONCATENATE WA_LFA1-KTOKK '-' WA_T077Y-TXT30 INTO WA_SAIDA-GRP_CTA.
      ENDIF.

      READ TABLE IT_EKKO INTO WA_EKKO WITH KEY EBELN = WA_BSIK-ZUONR2 BINARY SEARCH.
      IF SY-SUBRC = 0.
        READ TABLE IT_T161T INTO WA_T161T WITH KEY BSART = WA_EKKO-BSART BINARY SEARCH.
        IF SY-SUBRC = 0.
          WA_SAIDA-AUART = WA_T161T-BATXT.
        ENDIF.
      ENDIF.

      APPEND WA_SAIDA TO IT_SAIDA.
      CLEAR WA_SAIDA.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       Exibir dados no video
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS.
  PERFORM: F_CABECALHO,
           F_CATALOGO,
           F_LAYOUT,
           F_RELATORIO.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_CABECALHO
*&---------------------------------------------------------------------*
*       Cabeçalho do relatório
*----------------------------------------------------------------------*
FORM F_CABECALHO .
  DATA V_STR_1(20).
  DATA V_STR_2(20).

*  EXIBE FILTRO USADO NO CAMPO EMPRESA
*-----------------------------------------------------------------
  CLEAR ST_HEADER.
  ST_HEADER-TYP  = 'S'.
  ST_HEADER-KEY  = 'Empresa:'.

  IF P_EMP-HIGH IS NOT INITIAL.
    CONCATENATE P_EMP-LOW 'a' P_EMP-HIGH INTO ST_HEADER-INFO SEPARATED BY SPACE.
  ELSE.
    ST_HEADER-INFO = P_EMP-LOW.
  ENDIF.
  APPEND ST_HEADER TO IT_HEADER.

*  EXIBE FILTRO USADO NO CAMPO TIPO
*-----------------------------------------------------------------
  CLEAR ST_HEADER.
  ST_HEADER-TYP  = 'S'.
  ST_HEADER-KEY  = 'Tipo:'.

  IF P_TIPO  = 'F'.
    ST_HEADER-INFO = 'Fornecedor'.
  ELSE.
    ST_HEADER-INFO = 'Cliente'.
  ENDIF.
  APPEND ST_HEADER TO IT_HEADER.

*  EXIBE FILTRO USADO NO CAMPO DATA DE LANÇAMENTO
*----------------------------------------------------------------------*
  IF P_CLI-LOW IS NOT INITIAL.
    CLEAR ST_HEADER.
    ST_HEADER-TYP  = 'S'.
    ST_HEADER-KEY  = 'Cliente:'.

    IF P_CLI-HIGH IS NOT INITIAL.
      CONCATENATE P_CLI-LOW 'a' P_CLI-HIGH INTO ST_HEADER-INFO SEPARATED BY SPACE.
    ELSE.
      CONCATENATE 'a partir' P_CLI-LOW INTO ST_HEADER-INFO SEPARATED BY SPACE.
    ENDIF.

    APPEND ST_HEADER TO IT_HEADER.
  ENDIF.

*  EXIBE FILTRO USADO NO CAMPO DATA DE LANÇAMENTO
*----------------------------------------------------------------------*
  CLEAR ST_HEADER.
  ST_HEADER-TYP  = 'S'.
  ST_HEADER-KEY  = 'Data de Lançamento:'.

  CONCATENATE P_DATA-LOW+6(2) '/' P_DATA-LOW+4(2) '/' P_DATA-LOW+0(4) INTO V_STR_1.

  IF P_DATA-HIGH IS NOT INITIAL.
    CONCATENATE P_DATA-HIGH+6(2) '/' P_DATA-HIGH+4(2) '/' P_DATA-HIGH+0(4) INTO V_STR_2.
    CONCATENATE V_STR_1 'a' V_STR_2 INTO ST_HEADER-INFO SEPARATED BY SPACE.
  ELSE.
    CONCATENATE 'a partir' V_STR_1 INTO ST_HEADER-INFO SEPARATED BY SPACE.
  ENDIF.

  APPEND ST_HEADER TO IT_HEADER.
ENDFORM.                    " F_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_CATALOGO
*&---------------------------------------------------------------------*
*       Define as informaçãoes a serem exibidas na grid
*----------------------------------------------------------------------*
FORM F_CATALOGO .
  DATA I TYPE I.

  REFRESH IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'BUKRS'.
  ST_FIELDCAT-OUTPUTLEN     = '7'.
  ST_FIELDCAT-SELTEXT_L     = 'Empresa'.
  ST_FIELDCAT-SELTEXT_M     = 'Empresa'.
  ST_FIELDCAT-SELTEXT_S     = 'Empresa'.
  ST_FIELDCAT-KEY           = 'X'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'BUTXT'.
  ST_FIELDCAT-OUTPUTLEN     = '20'.
  ST_FIELDCAT-SELTEXT_L     = 'Empresa'.
  ST_FIELDCAT-SELTEXT_M     = 'Empresa'.
  ST_FIELDCAT-SELTEXT_S     = 'Empresa'.
  ST_FIELDCAT-KEY           = 'X'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'KUNNR'.
  ST_FIELDCAT-OUTPUTLEN     = '7'.
  ST_FIELDCAT-SELTEXT_L     = 'Código'.
  ST_FIELDCAT-SELTEXT_M     = 'Código'.
  ST_FIELDCAT-SELTEXT_S     = 'Código'.
  ST_FIELDCAT-KEY           = 'X'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'NAME1'.
  ST_FIELDCAT-OUTPUTLEN     = '30'.
  ST_FIELDCAT-SELTEXT_L     = 'Nome do Cliente'.
  ST_FIELDCAT-SELTEXT_M     = 'Nome do Cliente'.
  ST_FIELDCAT-SELTEXT_S     = 'Nome do Cliente'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'HKONT'.
  ST_FIELDCAT-OUTPUTLEN     = '10'.
  ST_FIELDCAT-SELTEXT_L     = 'Razão'.
  ST_FIELDCAT-SELTEXT_M     = 'Razão'.
  ST_FIELDCAT-SELTEXT_S     = 'Razão'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  IF P_TIPO = 'C'.

    I = I + 1.
    CLEAR ST_FIELDCAT.
    ST_FIELDCAT-COL_POS       = I.
    ST_FIELDCAT-FIELDNAME     = 'KLIMG'.
    ST_FIELDCAT-OUTPUTLEN     = '10'.
    ST_FIELDCAT-SELTEXT_L     = 'Limite total'.
    ST_FIELDCAT-SELTEXT_M     = 'Limite total'.
    ST_FIELDCAT-SELTEXT_S     = 'Limite total'.
    APPEND ST_FIELDCAT TO IT_FIELDCAT.
  ENDIF.


  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'BUDAT'.
  ST_FIELDCAT-OUTPUTLEN     = '10'.
  ST_FIELDCAT-SELTEXT_L     = 'Dt. Lcto'.
  ST_FIELDCAT-SELTEXT_M     = 'Dt. Lcto'.
  ST_FIELDCAT-SELTEXT_S     = 'Dt. Lcto'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'BELNR'.
  ST_FIELDCAT-OUTPUTLEN     = '12'.
  ST_FIELDCAT-SELTEXT_L     = 'Doc. Contábil'.
  ST_FIELDCAT-SELTEXT_M     = 'Doc. Contábil'.
  ST_FIELDCAT-SELTEXT_S     = 'Doc. Contábil'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'UMSKS'.
  ST_FIELDCAT-OUTPUTLEN     = '12'.
  ST_FIELDCAT-SELTEXT_L     = 'Class. Partida'.
  ST_FIELDCAT-SELTEXT_M     = 'Class. Partida'.
  ST_FIELDCAT-SELTEXT_S     = 'Class. Partida'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'BLART'.
  ST_FIELDCAT-OUTPUTLEN     = '6'.
  ST_FIELDCAT-SELTEXT_L     = 'Tp. Doc.'.
  ST_FIELDCAT-SELTEXT_M     = 'Tp. Doc.'.
  ST_FIELDCAT-SELTEXT_S     = 'Tp. Doc.'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  IF P_TIPO = 'C'.
    I = I + 1.
    CLEAR ST_FIELDCAT.
    ST_FIELDCAT-COL_POS       = I.
    ST_FIELDCAT-FIELDNAME     = 'VBEL2'.
    ST_FIELDCAT-OUTPUTLEN     = '10'.
    ST_FIELDCAT-SELTEXT_L     = 'Nro. OV'.
    ST_FIELDCAT-SELTEXT_M     = 'Nro. OV'.
    ST_FIELDCAT-SELTEXT_S     = 'Nro. OV'.
    APPEND ST_FIELDCAT TO IT_FIELDCAT.

    I = I + 1.
    CLEAR ST_FIELDCAT.
    ST_FIELDCAT-COL_POS       = I.
    ST_FIELDCAT-FIELDNAME     = 'AUART'.
    ST_FIELDCAT-OUTPUTLEN     = '20'.
    ST_FIELDCAT-SELTEXT_L     = 'Tp. OV'.
    ST_FIELDCAT-SELTEXT_M     = 'Tp. OV'.
    ST_FIELDCAT-SELTEXT_S     = 'Tp. OV'.
    APPEND ST_FIELDCAT TO IT_FIELDCAT.
  ELSEIF P_TIPO = 'F'.
    I = I + 1.
    CLEAR ST_FIELDCAT.
    ST_FIELDCAT-COL_POS       = I.
    ST_FIELDCAT-FIELDNAME     = 'VBEL2'.
    ST_FIELDCAT-OUTPUTLEN     = '10'.
    ST_FIELDCAT-SELTEXT_L     = 'Nro.Pedido'.
    ST_FIELDCAT-SELTEXT_M     = 'Nro.Pedido'.
    ST_FIELDCAT-SELTEXT_S     = 'Nro.Pedido'.
    APPEND ST_FIELDCAT TO IT_FIELDCAT.

    I = I + 1.
    CLEAR ST_FIELDCAT.
    ST_FIELDCAT-COL_POS       = I.
    ST_FIELDCAT-FIELDNAME     = 'AUART'.
    ST_FIELDCAT-OUTPUTLEN     = '20'.
    ST_FIELDCAT-SELTEXT_L     = 'Tp.Pedido'.
    ST_FIELDCAT-SELTEXT_M     = 'Tp.Pedido'.
    ST_FIELDCAT-SELTEXT_S     = 'Tp.Pedido'.
    APPEND ST_FIELDCAT TO IT_FIELDCAT.
  ENDIF.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'VBELN'.
  ST_FIELDCAT-OUTPUTLEN     = '10'.
  ST_FIELDCAT-SELTEXT_L     = 'Doc. Fatura'.
  ST_FIELDCAT-SELTEXT_M     = 'Doc. Fatura'.
  ST_FIELDCAT-SELTEXT_S     = 'Doc. Fatura'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  IF P_TIPO = 'C'.
    I = I + 1.
    CLEAR ST_FIELDCAT.
    ST_FIELDCAT-COL_POS       = I.
    ST_FIELDCAT-FIELDNAME     = 'MATNR'.
    ST_FIELDCAT-OUTPUTLEN     = '8'.
    ST_FIELDCAT-SELTEXT_L     = 'Cod. Mat.'.
    ST_FIELDCAT-SELTEXT_M     = 'Cod. Mat.'.
    ST_FIELDCAT-SELTEXT_S     = 'Cod. Mat.'.
    APPEND ST_FIELDCAT TO IT_FIELDCAT.

    I = I + 1.
    CLEAR ST_FIELDCAT.
    ST_FIELDCAT-COL_POS       = I.
    ST_FIELDCAT-FIELDNAME     = 'ARKTX'.
    ST_FIELDCAT-OUTPUTLEN     = '40'.
    ST_FIELDCAT-SELTEXT_L     = 'Descr. Material'.
    ST_FIELDCAT-SELTEXT_M     = 'Descr. Material'.
    ST_FIELDCAT-SELTEXT_S     = 'Descr. Material'.
    APPEND ST_FIELDCAT TO IT_FIELDCAT.
  ENDIF.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'ZFBDT'.
  ST_FIELDCAT-OUTPUTLEN     = '10'.
  ST_FIELDCAT-SELTEXT_L     = 'Dt. Vencto.'.
  ST_FIELDCAT-SELTEXT_M     = 'Dt. Vencto.'.
  ST_FIELDCAT-SELTEXT_S     = 'Dt. Vencto.'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'WAERS'.
  ST_FIELDCAT-OUTPUTLEN     = '6'.
  ST_FIELDCAT-SELTEXT_L     = 'Moeda'.
  ST_FIELDCAT-SELTEXT_M     = 'Moeda'.
  ST_FIELDCAT-SELTEXT_S     = 'Moeda'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'DMBTR'.
  ST_FIELDCAT-OUTPUTLEN     = '12'.
  ST_FIELDCAT-SELTEXT_L     = 'Valor R$'.
  ST_FIELDCAT-SELTEXT_M     = 'Valor R$'.
  ST_FIELDCAT-SELTEXT_S     = 'Valor R$'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'DMBE2'.
  ST_FIELDCAT-OUTPUTLEN     = '12'.
  ST_FIELDCAT-SELTEXT_L     = 'Valor US$'.
  ST_FIELDCAT-SELTEXT_M     = 'Valor US$'.
  ST_FIELDCAT-SELTEXT_S     = 'Valor US$'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'GSBER'.
  ST_FIELDCAT-OUTPUTLEN     = '6'.
  ST_FIELDCAT-SELTEXT_L     = 'Filial'.
  ST_FIELDCAT-SELTEXT_M     = 'Filial'.
  ST_FIELDCAT-SELTEXT_S     = 'Filial'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'SGTXT'.
  ST_FIELDCAT-OUTPUTLEN     = '22'.
  ST_FIELDCAT-SELTEXT_L     = 'Texto Contábil'.
  ST_FIELDCAT-SELTEXT_M     = 'Texto Contábil'.
  ST_FIELDCAT-SELTEXT_S     = 'Texto Contábil'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'ZLSCH'.
  ST_FIELDCAT-OUTPUTLEN     = '10'.
  ST_FIELDCAT-SELTEXT_L     = 'Forma de Pgto.'.
  ST_FIELDCAT-SELTEXT_M     = 'Forma de Pgto.'.
  ST_FIELDCAT-SELTEXT_S     = 'Forma de Pgto.'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.

  I = I + 1.
  CLEAR ST_FIELDCAT.
  ST_FIELDCAT-COL_POS       = I.
  ST_FIELDCAT-FIELDNAME     = 'GRP_CTA'.
  ST_FIELDCAT-OUTPUTLEN     = '30'.
  ST_FIELDCAT-SELTEXT_L     = 'Gpo.Contas'.
  ST_FIELDCAT-SELTEXT_M     = 'Gpo.Contas'.
  ST_FIELDCAT-SELTEXT_S     = 'Gpo.Contas'.
  APPEND ST_FIELDCAT TO IT_FIELDCAT.
ENDFORM.                    " F_CATALOGO
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*       Define Layout da grid que exibe os dados
*----------------------------------------------------------------------*
FORM F_LAYOUT .
  ST_LAYOUT-NO_ZEBRA            = ' '.
  ST_LAYOUT-COLWIDTH_OPTIMIZE   = ' '.

  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA_BACK'
    EXPORTING
      IT_FIELDCAT       = IT_FIELDCAT
      IS_LAYOUT         = ST_LAYOUT
    IMPORTING
      ET_FIELDCAT       = IT_FIELDCAT_ALV
      ES_LAYOUT         = IT_LAYOUT_ALV
      ET_SPECIAL_GROUPS = IT_SPECIAL_GROUPS.
ENDFORM.                    " F_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  F_RELATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_RELATORIO.
  VC_REPID = SY-REPID.
  MOVE VC_REPID TO ST_VARIANT-REPORT.

  ST_PRINT-NO_PRINT_SELINFOS  = 'X'.
  ST_PRINT-NO_PRINT_LISTINFOS = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM     = VC_REPID
      IS_LAYOUT              = IT_LAYOUT_ALV
      IT_FIELDCAT            = IT_FIELDCAT_ALV
      IT_SORT                = IT_SORT[]
      I_SAVE                 = 'A'
      I_DEFAULT              = 'X'
      IT_EVENTS              = IT_EVENTOS[]
      I_CALLBACK_TOP_OF_PAGE = 'TOP_OF_PAGE'
    TABLES
      T_OUTTAB               = IT_SAIDA[]
    EXCEPTIONS
      PROGRAM_ERROR          = 1
      OTHERS                 = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    STOP.
  ENDIF.
ENDFORM.                    " F_RELATORIO

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       Exibe o cabeçalho
*----------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = IT_HEADER.
ENDFORM.                    "TOP_OF_PAGE
