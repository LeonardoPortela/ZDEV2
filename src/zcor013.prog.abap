*&---------------------------------------------------------------------*
*& Report  ZCOR013
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZCOR013.
TABLES: ZCOT0007.

DATA: CL_ALV TYPE REF TO CL_GUI_ALV_GRID.
TYPE-POOLS: SLIS.

TYPES: BEGIN OF TY_SAIDA.
        INCLUDE STRUCTURE ZCOT0007.
TYPES:  BELNR_FB03  TYPE ZIB_CONTABIL_CHV-BELNR,
        ERDAT       TYPE LIPS-ERDAT,
        DS_OPER(25) TYPE C,
        SHTYPT(30)  TYPE C,
        MAKTX       TYPE MAKT-MAKTX,
        LFIMG       TYPE LIPS-LFIMG,
        WERKSO      TYPE LIPS-WERKS,
        KWERT       TYPE KONV-KWERT,
        END OF TY_SAIDA.

TYPES: BEGIN OF TY_ZCOT0007.
        INCLUDE STRUCTURE ZCOT0007.
TYPES:  BELNR_FB03  TYPE ZIB_CONTABIL_CHV-BELNR,
        ERDAT       TYPE LIPS-ERDAT,
        DS_OPER(25) TYPE C,
        SHTYPT(30)  TYPE C,
        MAKTX       TYPE MAKT-MAKTX,
        LFIMG       TYPE LIPS-LFIMG,
        WERKSO      TYPE LIPS-WERKS,
        KWERT       TYPE KONV-KWERT,
        END OF TY_ZCOT0007.

TYPES: BEGIN OF TY_BKPF,
         BELNR TYPE BKPF-BELNR,
         GJAHR TYPE BKPF-GJAHR,
       END OF TY_BKPF,

       BEGIN OF TY_VTTP,
         VBELN TYPE VTTP-VBELN,
         TKNUM TYPE VTTP-TKNUM,
       END OF TY_VTTP,

       BEGIN OF TY_VTFA,
         VBELV TYPE VTFA-VBELV,
         VBELN TYPE VTFA-VBELN,
       END OF TY_VTFA,

       BEGIN OF TY_VFKP,
         FKNUM TYPE VFKP-FKNUM,
         KNUMV TYPE VFKP-KNUMV,
       END OF TY_VFKP,

       BEGIN OF TY_KONV,
         KNUMV TYPE KONV-KNUMV,
         KWERT TYPE KONV-KWERT,
       END OF TY_KONV.

TYPES: BEGIN OF TY_LIPS,
         VBELN TYPE LIPS-VBELN,
         ERDAT TYPE LIPS-ERDAT,
         MATNR TYPE LIPS-MATNR,
         LFIMG TYPE LIPS-LFIMG,
         WERKS TYPE LIPS-WERKS,
       END OF TY_LIPS.

TYPES:    BEGIN OF TY_TVTKT,
            SHTYP TYPE TVTKT-SHTYP,
            BEZEI TYPE TVTKT-BEZEI,
          END OF TY_TVTKT,

          BEGIN OF TY_MAKT,
            MATNR TYPE MAKT-MATNR,
            MAKTX TYPE MAKT-MAKTX,
          END OF TY_MAKT.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

TYPES :BEGIN OF TY_ZIB_CONTABIL_CHV,
         OBJ_KEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
         BELNR   TYPE ZIB_CONTABIL_CHV-BELNR,
         BUKRS   TYPE ZIB_CONTABIL_CHV-BUKRS,
         GJAHR   TYPE ZIB_CONTABIL_CHV-GJAHR,
       END OF TY_ZIB_CONTABIL_CHV.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA:
  IT_FCAT             TYPE TABLE OF TY_ESTRUTURA,
  S_VARIANT           TYPE DISVARIANT           , " Tabela Estrutura co
  T_TOP               TYPE SLIS_T_LISTHEADER,
  XS_EVENTS           TYPE SLIS_ALV_EVENT,
  EVENTS              TYPE SLIS_T_EVENT,
  GD_LAYOUT           TYPE SLIS_LAYOUT_ALV,
  T_PRINT             TYPE SLIS_PRINT_ALV,
  V_REPORT            LIKE SY-REPID,
  T_SORT              TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
  IT_SETLEAF          LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
  ESTRUTURA           TYPE TABLE OF TY_ESTRUTURA,

  IT_ZIB_CONTABIL_CHV TYPE TABLE OF TY_ZIB_CONTABIL_CHV,
  IT_ZCOT0007         TYPE TABLE OF TY_ZCOT0007,
  IT_BKPF             TYPE TABLE OF TY_BKPF,
  IT_LIPS             TYPE TABLE OF TY_LIPS,
  IT_LIPS2            TYPE TABLE OF TY_LIPS,
  IT_TVTKT            TYPE TABLE OF TY_TVTKT,
  IT_MAKT             TYPE TABLE OF TY_MAKT,
  IT_VTTP             TYPE TABLE OF TY_VTTP,
  IT_VTFA             TYPE TABLE OF TY_VTFA,
  IT_VFKP             TYPE TABLE OF TY_VFKP,
  IT_KONV             TYPE TABLE OF TY_KONV,
  IT_SAIDA            TYPE TABLE OF TY_SAIDA.


DATA:
  WA_CONT             TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  WA_ALV              TYPE REF TO CL_GUI_ALV_GRID,
  WA_LAYOUT           TYPE LVC_S_LAYO,

  WA_ZIB_CONTABIL_CHV TYPE TY_ZIB_CONTABIL_CHV,
  WA_ZCOT0007         TYPE TY_ZCOT0007,
  WA_BKPF             TYPE TY_BKPF,
  WA_LIPS             TYPE TY_LIPS,
  WA_LIPS2            TYPE TY_LIPS,
  WA_TVTKT            TYPE TY_TVTKT,
  WA_MAKT             TYPE TY_MAKT,
  WA_VTTP             TYPE TY_VTTP,
  WA_VTFA             TYPE TY_VTFA,
  WA_VFKP             TYPE TY_VFKP,
  WA_KONV             TYPE TY_KONV,
  WA_SAIDA            TYPE TY_SAIDA.

*&---------------------------------------------------------------------*
*& Tela de Seleção                                                     *
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
                 P_BUKRS FOR ZCOT0007-BUKRS OBLIGATORY,
                 P_BUDAT FOR ZCOT0007-BUDAT OBLIGATORY,
                 P_MATNR FOR ZCOT0007-MATNR.
SELECTION-SCREEN: END OF BLOCK B1.

*&---------------------------------------------------------------------*
*& Início do Programa                                                  *
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM: SELECIONA_DADOS,
           SAIDA_DADOS,
           CHAMA_ALV.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS .
  DATA: VTABIX   TYPE SY-TABIX,
        VKWERT   TYPE KONV-KWERT,
        TL_MATNR TYPE RANGE OF ZCOT0007-MATNR WITH HEADER LINE,
        LV_MATNR TYPE ZCOT0007-MATNR.

  FIELD-SYMBOLS: <FS_007> TYPE TY_ZCOT0007.


  LOOP AT P_MATNR.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = P_MATNR-LOW
      IMPORTING
        OUTPUT = P_MATNR-LOW.

    IF P_MATNR-HIGH IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = P_MATNR-HIGH
        IMPORTING
          OUTPUT = P_MATNR-HIGH.

    ENDIF.

    APPEND P_MATNR TO TL_MATNR.

    SHIFT P_MATNR-LOW LEFT DELETING LEADING '0'.

    IF P_MATNR-HIGH IS NOT INITIAL.
      SHIFT P_MATNR-HIGH LEFT DELETING LEADING '0'.

    ENDIF.

    APPEND P_MATNR TO TL_MATNR.

  ENDLOOP.

  SELECT *
    FROM ZCOT0007
    INTO TABLE IT_ZCOT0007
    WHERE BUKRS	IN  P_BUKRS
     AND  BUDAT IN  P_BUDAT
     AND  MATNR IN  TL_MATNR.

  CHECK IT_ZCOT0007 IS NOT INITIAL.

  LOOP AT IT_ZCOT0007 ASSIGNING <FS_007>.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = <FS_007>-MATNR
      IMPORTING
        OUTPUT = LV_MATNR.

    <FS_007>-MATNR = LV_MATNR.

  ENDLOOP.

  SELECT  OBJ_KEY BELNR BUKRS GJAHR
    FROM ZIB_CONTABIL_CHV
    INTO TABLE IT_ZIB_CONTABIL_CHV
    FOR ALL ENTRIES IN IT_ZCOT0007
    WHERE OBJ_KEY = IT_ZCOT0007-OBJ_KEY.

  SELECT  VBELN ERDAT MATNR LFIMG WERKS
    FROM LIPS
    INTO TABLE IT_LIPS
    FOR ALL ENTRIES IN IT_ZCOT0007
    WHERE VBELN = IT_ZCOT0007-VBELN.

  IT_LIPS2[] = IT_LIPS[].

  SELECT  VBELN TKNUM
    FROM VTTP
    INTO TABLE IT_VTTP
    FOR ALL ENTRIES IN IT_ZCOT0007
    WHERE VBELN = IT_ZCOT0007-VBELN.

  IF IT_VTTP[] IS NOT INITIAL.
    SELECT  VBELV  VBELN
     FROM VTFA
     INTO TABLE IT_VTFA
     FOR ALL ENTRIES IN IT_VTTP
     WHERE VBELV = IT_VTTP-TKNUM.

    IF IT_VTFA[] IS NOT INITIAL.
      SELECT  FKNUM KNUMV
        FROM VFKP
        INTO TABLE IT_VFKP
        FOR ALL ENTRIES IN IT_VTFA
        WHERE FKNUM = IT_VTFA-VBELN.

      IF IT_VFKP[] IS NOT INITIAL.
        SELECT FROM V_KONV FIELDS KNUMV , KWERT FOR ALL ENTRIES IN @IT_VFKP WHERE KNUMV = @IT_VFKP-KNUMV AND KSCHL IN ( 'ZFRE' , 'ZLOT' ) INTO TABLE @IT_KONV .

      ENDIF.
    ENDIF.
  ENDIF.

  SELECT SHTYP BEZEI
    FROM TVTKT
    INTO TABLE IT_TVTKT
    FOR ALL ENTRIES IN IT_ZCOT0007
    WHERE SHTYP = IT_ZCOT0007-SHTYP
    AND   SPRAS	=	'PT'.

  SELECT MATNR MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_ZCOT0007
    WHERE MATNR = IT_ZCOT0007-MATNR
    AND SPRAS = SY-LANGU.

  SORT IT_ZIB_CONTABIL_CHV BY OBJ_KEY.
  SORT: IT_LIPS  BY VBELN,
        IT_LIPS2 BY VBELN MATNR,
        IT_TVTKT BY SHTYP,
        IT_MAKT  BY MATNR,
        IT_VTTP  BY VBELN,
        IT_VTFA  BY VBELV,
        IT_VFKP  BY FKNUM,
        IT_KONV  BY KNUMV.

  LOOP AT IT_ZCOT0007 INTO WA_ZCOT0007.
    VTABIX = SY-TABIX.
    READ TABLE IT_ZIB_CONTABIL_CHV INTO WA_ZIB_CONTABIL_CHV WITH KEY OBJ_KEY = WA_ZCOT0007-OBJ_KEY BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_ZCOT0007-BELNR_FB03 = WA_ZIB_CONTABIL_CHV-BELNR.
      MODIFY IT_ZCOT0007 INDEX VTABIX FROM WA_ZCOT0007 TRANSPORTING BELNR_FB03.
    ENDIF.

    READ TABLE IT_LIPS INTO WA_LIPS WITH KEY VBELN = WA_ZCOT0007-VBELN BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_ZCOT0007-ERDAT = WA_LIPS-ERDAT.
      MODIFY IT_ZCOT0007 INDEX VTABIX FROM WA_ZCOT0007 TRANSPORTING ERDAT.
    ENDIF.

    IF WA_ZCOT0007-TP_OPER = '01'.
      WA_ZCOT0007-DS_OPER = '01-Despesas de Vendas' .
    ELSEIF WA_ZCOT0007-TP_OPER = '03'.
      WA_ZCOT0007-DS_OPER = '03-Estoque' .
    ELSEIF WA_ZCOT0007-TP_OPER = '04'.
      WA_ZCOT0007-DS_OPER = '04-Estoque Porto' .
    ENDIF.

    READ TABLE IT_TVTKT INTO WA_TVTKT WITH KEY SHTYP = WA_ZCOT0007-SHTYP BINARY SEARCH.
    CONCATENATE WA_ZCOT0007-SHTYP '-' WA_TVTKT-BEZEI INTO  WA_ZCOT0007-SHTYPT.

    READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR = WA_ZCOT0007-MATNR BINARY SEARCH.
    WA_ZCOT0007-MAKTX = WA_MAKT-MAKTX.

    SHIFT WA_ZCOT0007-MATNR LEFT DELETING LEADING '0'.

    MODIFY IT_ZCOT0007 INDEX VTABIX FROM WA_ZCOT0007 TRANSPORTING DS_OPER SHTYPT MAKTX.

    READ TABLE IT_LIPS2 INTO WA_LIPS2 WITH KEY VBELN = WA_ZCOT0007-VBELN MATNR = WA_ZCOT0007-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      IF 'FR_FT' CS WA_ZCOT0007-BLART .
        WA_ZCOT0007-LFIMG  = WA_LIPS2-LFIMG.
        READ TABLE IT_VTTP INTO WA_VTTP WITH KEY VBELN = WA_ZCOT0007-VBELN BINARY SEARCH.
        READ TABLE IT_VTFA INTO WA_VTFA WITH KEY VBELV = WA_VTTP-TKNUM     BINARY SEARCH.
        READ TABLE IT_VFKP INTO WA_VFKP WITH KEY FKNUM = WA_VTFA-VBELN     BINARY SEARCH.
        VKWERT = 0.
        LOOP AT IT_VFKP INTO WA_VFKP WHERE FKNUM = WA_VTFA-VBELN .
          READ TABLE IT_KONV INTO WA_KONV WITH KEY KNUMV = WA_VFKP-KNUMV     BINARY SEARCH.
          LOOP AT IT_KONV INTO WA_KONV WHERE KNUMV = WA_VFKP-KNUMV .
            ADD WA_KONV-KWERT TO VKWERT.
          ENDLOOP.
        ENDLOOP.
        WA_ZCOT0007-KWERT = VKWERT.
      ENDIF.
      WA_ZCOT0007-WERKSO = WA_LIPS2-WERKS.
      MODIFY IT_ZCOT0007 INDEX VTABIX FROM WA_ZCOT0007 TRANSPORTING LFIMG WERKSO KWERT.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_ZCOT0007-MATNR
      IMPORTING
        OUTPUT = WA_ZCOT0007-MATNR.

    MODIFY IT_ZCOT0007 INDEX VTABIX FROM WA_ZCOT0007 TRANSPORTING MATNR.
    CLEAR WA_ZCOT0007.

  ENDLOOP.

  IT_SAIDA[] = IT_ZCOT0007.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CHAMA_ALV
*&---------------------------------------------------------------------*
FORM CHAMA_ALV .


  GD_LAYOUT-NO_INPUT          = 'X'.
  GD_LAYOUT-ZEBRA             = 'X'.
  GD_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  GD_LAYOUT-BOX_TABNAME       = 'IT_SAIDA'.
  GD_LAYOUT-WINDOW_TITLEBAR   = 'Apropriação de Custo de Frete'.
  GD_LAYOUT-DETAIL_TITLEBAR   = 'Apropriação de Custo de Frete'.

  PERFORM F_INICIAR_VARIAVES.
  PERFORM F_DEFINIR_EVENTOS.
  PERFORM ALV_PREENCHE_CAT USING:
              'BUKRS'       TEXT-002        '05'      ' '   ' '     ' ', "Empresa
              'BELNR'       TEXT-003        '15'      ' '   ' '     ' ', "Nº documento de um documento contábil
              'GJAHR'       TEXT-004        '05'      ' '   ' '     ' ', "Exercício
              'DS_OPER'     TEXT-028        '25'      ' '   ' '     ' ', "Descr.Operação
              'SHTYPT'      TEXT-006        '25'      ' '   ' '     ' ', "Tipo de transporte
              'BUDAT'       TEXT-007        '20'      ' '   ' '     ' ', "Data de lançamento no documento
              'BLART'       TEXT-008        '15'      ' '   ' '     ' ', "Tipo de documento
              'XBLNR'       TEXT-009        '15'      ' '   ' '     ' ', "Nº documento de referência
              'ZUONR'       TEXT-010        '15'      ' '   ' '     ' ', "Nº atribuição
              'BELNR_F'     TEXT-011        '15'      ' '   ' '     ' ', "Nº de um documento de faturamento
              'DMBTR'       TEXT-012        '15'      ' '   ' '     ' ', "Montante em moeda interna
              'DMBE2'       TEXT-013        '15'      ' '   ' '     ' ', "Montante na 2ª moeda interna
              'SGTXT'       TEXT-014        '30'      ' '   ' '     ' ', "Texto do item
              'MATNR'       TEXT-015        '18'      ' '   ' '     ' ', "Nº do material
              'MAKTX'       TEXT-029        '30'      ' '   ' '     ' ' ,"Descrição Material
              'WERKS'       TEXT-016        '10'      ' '   ' '     ' ', "Centro
              'DOC_MR22'    TEXT-017        '20'      'X'   ' '     ' ', "Doc gerado BDC MR22
              'OBJ_KEY'     TEXT-018        '20'      ' '   ' '     ' ', "Chave referência
              'DT_APROPR'   TEXT-019        '10'      ' '   ' '     ' ', "Data apropriação
              'VBELN'       TEXT-020        '15'      ' '   ' '     ' ', "Fornecimento
              'ERDAT'       TEXT-027        '15'      ' '   ' '     ' ', "Data Remessa
              'VBELN_V'     TEXT-021        '15'      ' '   ' '     ' ', "Documento de vendas
              'EBELN'       TEXT-022        '15'      ' '   ' '     ' ', "Nº do documento de compras
              'KOSTL'       TEXT-023        '10'      ' '   ' '     ' ', "Centro de custo
              'CPUDT'       TEXT-024        '10'      ' '   ' '     ' ', "Data da entrada do documento contábil
              'CPUTM'       TEXT-025        '10'      ' '   ' '     ' ', "Hora da entrada
              'BELNR_FB03'  TEXT-026        '20'      'X'   ' '     ' ', "Documento Gerado
              'LFIMG'       TEXT-030        '15'      ' '   ' '     ' ', "Qtd.Remessa
              'WERKSO'      TEXT-031        '15'      ' '   ' '     ' ', "Centro Origem
              'KWERT'       TEXT-032        '15'      ' '   ' '     ' '. "Custo Frete Bruto


  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      IS_LAYOUT               = GD_LAYOUT
      "I_CALLBACK_PF_STATUS_SET  = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND = 'USER_COMMAND'
      IT_FIELDCAT             = IT_FCAT[]
      IT_SORT                 = T_SORT[]
      I_SAVE                  = 'X'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
*     IS_VARIANT              = VG_VARIANT
    TABLES
      T_OUTTAB                = IT_SAIDA.

ENDFORM.                    " CHAMA_ALV
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING:
                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->NAME       text
*      -->FORM       text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                      " F_CARREGAR_EVENTOS
*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CAMPO    text
*      -->P_DESC     text
*      -->P_TAM      text
*      -->P_HOT      text
*      -->P_ZERO     text
*      -->P_SOMA     text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING   P_CAMPO  TYPE C
                               P_DESC   TYPE C
                               P_TAM    TYPE C
                               P_HOT    TYPE C
                               P_ZERO   TYPE C
                               P_SOMA   TYPE C.


  DATA: WL_FCAT TYPE TY_ESTRUTURA.

  WL_FCAT-TABNAME   = 'IT_SAIDA'.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SELTEXT_S = P_DESC.
  WL_FCAT-SELTEXT_M = P_DESC.
  WL_FCAT-SELTEXT_L = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-DO_SUM    = P_SOMA.
  IF P_CAMPO = 'ICON'.
    WL_FCAT-ICON      = 'X'.
  ENDIF.

  APPEND WL_FCAT TO IT_FCAT.
ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_INICIAR_VARIAVES .
  DATA:
    W_TEXTO1(10),
    W_TEXTO2(10),
    W_TEXTO3(40),

    W_EMPRESA_TEXTO(40),
    W_CENTRO_TEXTO(40),
    W_PER_TEXTO(40),

    EMPRESA             TYPE C LENGTH 50,
    CENTRO              TYPE C LENGTH 50,
    PERIODO             TYPE C LENGTH 50.


  V_REPORT = SY-REPID.

  W_TEXTO3 = 'Relatório Apropriação de Fretes'.
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO3.


  IF P_BUKRS IS NOT INITIAL.
    W_EMPRESA_TEXTO = 'Empresa:'.
    IF ( P_BUKRS-LOW IS NOT INITIAL ) AND ( P_BUKRS-HIGH IS NOT INITIAL ).
      CONCATENATE W_EMPRESA_TEXTO  P_BUKRS-LOW 'á' P_BUKRS-HIGH INTO EMPRESA SEPARATED BY SPACE.
    ELSEIF ( P_BUKRS-LOW IS NOT INITIAL ).
      CONCATENATE W_EMPRESA_TEXTO P_BUKRS-LOW  INTO EMPRESA SEPARATED BY SPACE.
    ELSE.
      CONCATENATE W_EMPRESA_TEXTO 'Todas'  INTO EMPRESA SEPARATED BY SPACE.
    ENDIF.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' EMPRESA.
  ENDIF.

  IF ( NOT  P_BUDAT IS INITIAL ).
    W_PER_TEXTO = 'Período :'.
    CONCATENATE P_BUDAT-LOW+6(2)   '.' P_BUDAT-LOW+4(2)  '.' P_BUDAT-LOW(4)  INTO W_TEXTO1.
    CONCATENATE P_BUDAT-HIGH+6(2)  '.' P_BUDAT-HIGH+4(2) '.' P_BUDAT-HIGH(4) INTO W_TEXTO2.
    CONCATENATE W_PER_TEXTO W_TEXTO1 ' - ' W_TEXTO2 INTO PERIODO   SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S' PERIODO .
  ENDIF.

ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0466   text
*      -->P_PERIODO  text
*----------------------------------------------------------------------*


FORM F_CONSTRUIR_CABECALHO    USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  SAIDA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAIDA_DADOS .
  IT_SAIDA[] = IT_ZCOT0007.
  SORT IT_SAIDA BY BELNR.

ENDFORM.                    " SAIDA_DADOS

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->R_UCOMM      text
*      -->RS_SELFIELD  text
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM     LIKE SY-UCOMM           "#EC CALLED
                        RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA VAWKEY TYPE BKPF-AWKEY.

  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX RS_SELFIELD-TABINDEX.
  CASE RS_SELFIELD-FIELDNAME.

    WHEN 'BELNR_FB03'.
      IF WA_SAIDA-BELNR_FB03 IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD WA_SAIDA-BELNR_FB03.
        SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD WA_SAIDA-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ENDIF.

    WHEN 'DOC_MR22'.
      IF WA_SAIDA-DOC_MR22 IS NOT INITIAL.
        CONCATENATE WA_SAIDA-DOC_MR22 WA_SAIDA-GJAHR INTO VAWKEY.
        SELECT SINGLE BELNR
          FROM BKPF
          INTO WA_BKPF
          WHERE BUKRS      = WA_SAIDA-BUKRS
          AND   GJAHR      = WA_SAIDA-GJAHR
          AND   AWKEY      = VAWKEY
          AND   BLART      = 'PR'.

        SET PARAMETER ID 'BLN' FIELD WA_BKPF-BELNR.
        SET PARAMETER ID 'BUK' FIELD WA_SAIDA-BUKRS.
        SET PARAMETER ID 'GJR' FIELD WA_SAIDA-GJAHR.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ENDIF.

  ENDCASE.

ENDFORM.                    "USER_COMMAND
