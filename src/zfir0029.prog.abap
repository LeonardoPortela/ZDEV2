*&---------------------------------------------------------------------*
*& Report  ZFIR0029
*&
*&---------------------------------------------------------------------*
*&TITULO  : Relatório Controle Recebimentos  - INVOICES
*&AUTOR   : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA.   :  10.07.2013
*TRANSACAO: ZFI0023
*&---------------------------------------------------------------------*
REPORT  ZFIR0029.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: ICON,
            SLIS.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: ZFIT0036.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
     BEGIN OF TY_ZFIT0036,
      OBJ_KEY       TYPE ZFIT0036-OBJ_KEY,
      BUKRS         TYPE ZFIT0036-BUKRS,
      INVOICE       TYPE ZFIT0036-INVOICE,
      NAVIO         TYPE ZFIT0036-NAVIO,
      MATNR         TYPE ZFIT0036-MATNR,
     END OF TY_ZFIT0036,

     BEGIN OF TY_ZIB_CONTABIL_CHV,
      OBJ_KEY       TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
      BELNR         TYPE ZIB_CONTABIL_CHV-BELNR,
      BUKRS         TYPE ZIB_CONTABIL_CHV-BUKRS,
      GJAHR         TYPE ZIB_CONTABIL_CHV-GJAHR,
     END OF TY_ZIB_CONTABIL_CHV,

     BEGIN OF TY_BSAD,
       BELNR        TYPE BSAD-BELNR,
       GJAHR        TYPE BSAD-GJAHR,
       BUKRS        TYPE BSAD-BUKRS,
       AUGDT        TYPE BSAD-AUGDT,
       DMBE2        TYPE BSAD-DMBE2,
       KUNNR        TYPE BSAD-KUNNR,
     END OF TY_BSAD,

     BEGIN OF TY_ZFIT0041,
       NR_INVOICE   TYPE ZFIT0041-NR_INVOICE,
       NR_RE        TYPE ZFIT0041-NR_RE,
     END OF TY_ZFIT0041,

     BEGIN OF TY_ZDOC_EXP,
       NR_REGISTRO_EXPO TYPE ZDOC_EXP-NR_REGISTRO_EXPO,
       NUMERO_DUE       TYPE ZDOC_EXP-NUMERO_DUE,
       ID_NOMEACAO_TRAN TYPE ZDOC_EXP-ID_NOMEACAO_TRAN,
       VBELN            TYPE ZDOC_EXP-VBELN,
     END OF TY_ZDOC_EXP,

     BEGIN OF TY_ZNOM_CONHEC,
       ID_NOMEACAO_TRAN TYPE ZNOM_CONHEC-ID_NOMEACAO_TRAN,
       DT_DATA          TYPE ZNOM_CONHEC-DT_DATA,
       ID_CONHEC        TYPE ZNOM_CONHEC-ID_CONHEC,
     END OF TY_ZNOM_CONHEC,

     BEGIN OF TY_ZNOM_TRANSPORTE,
        ID_NOMEACAO_TRAN  TYPE ZNOM_TRANSPORTE-ID_NOMEACAO_TRAN,
        DS_PORTO          TYPE ZNOM_TRANSPORTE-DS_PORTO,
        DS_NOME_TRANSPOR  TYPE ZNOM_TRANSPORTE-DS_NOME_TRANSPOR,
     END OF TY_ZNOM_TRANSPORTE,

     BEGIN OF TY_LIPS,
       VBELN              TYPE LIPS-VBELN,
       MATNR              TYPE LIPS-MATNR,
     END OF TY_LIPS,

     BEGIN OF TY_MAKT,
       MATNR              TYPE MAKT-MATNR,
       SPRAS              TYPE MAKT-SPRAS,
       MAKTX              TYPE MAKT-MAKTX,
     END OF TY_MAKT,

     BEGIN OF TY_KNA1,
       KUNNR              TYPE KNA1-KUNNR,
       NAME1              TYPE KNA1-NAME1,
     END OF TY_KNA1,

     BEGIN OF TY_SAIDA,
       DS_NOME_TRANSPOR   TYPE ZNOM_TRANSPORTE-DS_NOME_TRANSPOR, "Nome do navio
       NAME1              TYPE KNA1-NAME1                      , "Comprador
       DS_PORTO           TYPE ZNOM_TRANSPORTE-DS_PORTO        , "Porto
       MAKTX              TYPE MAKT-MAKTX                      , "Produto
       DT_DATA            TYPE ZNOM_CONHEC-DT_DATA             , "Data do BL
       AUGDT              TYPE BSAD-AUGDT                      , "Data do recebimento
       INVOICE            TYPE ZFIT0036-INVOICE                , "nº da invoice
       DMBE2              TYPE BSAD-DMBE2                      , "Valor recebido (U$)
       TEMPO              TYPE I                               , "Tempo gasto
     END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: T_BDC     TYPE TABLE OF BDCDATA WITH HEADER LINE INITIAL SIZE 0,
      T_MESSTAB TYPE TABLE OF BDCMSGCOLL,

      IT_ZFIT0036         TYPE TABLE OF TY_ZFIT0036,
      IT_ZIB_CONTABIL_CHV TYPE TABLE OF TY_ZIB_CONTABIL_CHV,
      IT_BSAD             TYPE TABLE OF TY_BSAD,
      IT_ZFIT0041         TYPE TABLE OF TY_ZFIT0041,
      IT_ZDOC_EXP         TYPE TABLE OF TY_ZDOC_EXP,
      IT_ZNOM_CONHEC      TYPE TABLE OF TY_ZNOM_CONHEC,
      IT_ZNOM_TRANSPORTE  TYPE TABLE OF TY_ZNOM_TRANSPORTE,
      IT_LIPS             TYPE TABLE OF TY_LIPS,
      IT_MAKT             TYPE TABLE OF TY_MAKT,
      IT_KNA1             TYPE TABLE OF TY_KNA1,
      IT_SAIDA            TYPE TABLE OF TY_SAIDA,

      IT_COLOR            TYPE TABLE OF LVC_S_SCOL.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
      WA_CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV    TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT TYPE LVC_S_LAYO,

      WA_ZFIT0036         TYPE TY_ZFIT0036,
      WA_ZIB_CONTABIL_CHV TYPE TY_ZIB_CONTABIL_CHV,
      WA_BSAD             TYPE TY_BSAD,
      WA_ZFIT0041         TYPE TY_ZFIT0041,
      WA_ZDOC_EXP         TYPE TY_ZDOC_EXP,
      WA_ZNOM_CONHEC      TYPE TY_ZNOM_CONHEC,
      WA_ZNOM_TRANSPORTE  TYPE TY_ZNOM_TRANSPORTE,
      WA_LIPS             TYPE TY_LIPS,
      WA_MAKT             TYPE TY_MAKT,
      WA_KNA1             TYPE TY_KNA1,
      WA_SAIDA            TYPE TY_SAIDA,
      WA_COLOR            TYPE LVC_S_SCOL.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
     IT_FCAT    TYPE TABLE OF TY_ESTRUTURA,
     S_VARIANT  TYPE DISVARIANT           , " Tabela Estrutura co
     T_TOP      TYPE SLIS_T_LISTHEADER    ,
     XS_EVENTS  TYPE SLIS_ALV_EVENT       ,
     EVENTS     TYPE SLIS_T_EVENT         ,
     GD_LAYOUT  TYPE SLIS_LAYOUT_ALV      ,
     T_PRINT    TYPE SLIS_PRINT_ALV       ,
     V_REPORT   LIKE SY-REPID             ,
     T_SORT     TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE,
     IT_SETLEAF LIKE TABLE OF SETLEAF INITIAL SIZE 0 WITH HEADER LINE,
     ESTRUTURA  TYPE TABLE OF TY_ESTRUTURA,
     WA_ESTRUTURA TYPE TY_ESTRUTURA,
     VG_I       TYPE I.

DEFINE MC_PREENCHE_CLASS.
  VG_I = VG_I + 1.
  CLEAR T_SORT.
  T_SORT-SPOS      = VG_I.
  T_SORT-FIELDNAME = &1.
  T_SORT-GROUP     = &2.
  T_SORT-UP        = &3.
  T_SORT-SUBTOT    = &4.
  APPEND T_SORT.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

PARAMETERS:     P_BUKRS    TYPE ZFIT0036-BUKRS  OBLIGATORY.

SELECT-OPTIONS: P_DATA    FOR  ZFIT0036-DT_PGTO  OBLIGATORY,
                P_TIPO    FOR  ZFIT0036-ID_TIPO_INVOICE NO INTERVALS NO-EXTENSION..

SELECTION-SCREEN: END OF BLOCK B1.

INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_TIPO-LOW .
  DATA: TL_RETURN_TAB TYPE TABLE OF DDSHRETVAL WITH HEADER LINE,
           TL_DSELC      TYPE TABLE OF DSELC      WITH HEADER LINE.

  DATA: BEGIN OF TL_TIPO OCCURS 0,
            ID_TIPO_INVOICE   TYPE ZFIT0047-ID_TIPO_INVOICE,
            DESCRICAO         TYPE ZFIT0047-DESCRICAO,
         END OF TL_TIPO.

  SELECT ID_TIPO_INVOICE  DESCRICAO
    FROM ZFIT0047
    INTO TABLE TL_TIPO.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'ID_TIPO_INVOICE'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'ZFIT0047-ID_TIPO_INVOICE'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = TL_TIPO
      RETURN_TAB      = TL_RETURN_TAB
      DYNPFLD_MAPPING = TL_DSELC.
*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
            F_INICIAR_VARIAVES, " Cabeçalho
            F_SELECIONA_DADOS, " Form seleciona dados
            F_SAIDA, " Form de saida
            F_IMPRIME_DADOS.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM  F_INICIAR_VARIAVES.

  DATA:        W_DATA(10),
               W_HORA(10),
               W_TEXTO(40),
               CABEC        TYPE C LENGTH 200.


  DATA WA_T001     TYPE T001.


  V_REPORT = SY-REPID.

  W_TEXTO = 'Invoices'.
  PERFORM F_CONSTRUIR_CABECALHO USING 'H' W_TEXTO.

  SELECT SINGLE *
    FROM T001
    INTO WA_T001
    WHERE BUKRS = P_BUKRS.
  IF SY-SUBRC  = 0.
    W_TEXTO = 'Empresa    :'.
    CONCATENATE W_TEXTO WA_T001-BUKRS '-' WA_T001-BUTXT    INTO CABEC  SEPARATED BY SPACE.
    PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC.
  ENDIF.

  CONCATENATE P_DATA-LOW+6(2) '.' P_DATA-LOW+4(2) '.' P_DATA-LOW+0(4) INTO W_DATA.
  W_TEXTO = 'Data de Pagamento:'.
  CONCATENATE W_TEXTO W_DATA  INTO CABEC SEPARATED BY SPACE.
  IF P_DATA-HIGH IS NOT INITIAL.
    CONCATENATE P_DATA-HIGH+6(2) '.' P_DATA-HIGH+4(2) '.' P_DATA-HIGH+0(4) INTO W_DATA.
    CONCATENATE CABEC 'à'  W_DATA INTO CABEC SEPARATED BY SPACE.
  ENDIF.
  PERFORM F_CONSTRUIR_CABECALHO USING 'S'  CABEC.

ENDFORM.                    " INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.


  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.


ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .
  SELECT OBJ_KEY BUKRS INVOICE NAVIO MATNR
    FROM ZFIT0036
    INTO TABLE IT_ZFIT0036
    WHERE BUKRS           = P_BUKRS
    AND   ID_TIPO_INVOICE IN P_TIPO
    AND   STATUS_CTAS_REC	=	'P'.


  CHECK IT_ZFIT0036[] IS NOT INITIAL.

  SELECT OBJ_KEY BELNR BUKRS GJAHR
    FROM ZIB_CONTABIL_CHV
    INTO TABLE IT_ZIB_CONTABIL_CHV
    FOR ALL ENTRIES IN IT_ZFIT0036
    WHERE OBJ_KEY = IT_ZFIT0036-OBJ_KEY.

  CHECK IT_ZIB_CONTABIL_CHV[] IS NOT INITIAL.

  SELECT  BELNR GJAHR BUKRS AUGDT DMBE2 KUNNR
    FROM BSAD
    INTO TABLE IT_BSAD
    FOR ALL ENTRIES IN IT_ZIB_CONTABIL_CHV
    WHERE BELNR	=	IT_ZIB_CONTABIL_CHV-BELNR
    AND GJAHR	  =	IT_ZIB_CONTABIL_CHV-GJAHR
    AND BUKRS	  =	IT_ZIB_CONTABIL_CHV-BUKRS
    AND AUGDT   IN P_DATA.

  IF IT_BSAD[] IS NOT INITIAL.
    SELECT KUNNR NAME1
      FROM KNA1
      INTO TABLE IT_KNA1
      FOR ALL ENTRIES IN IT_BSAD
      WHERE KUNNR	=	IT_BSAD-KUNNR.
  ENDIF.

  SELECT NR_INVOICE NR_RE
    FROM ZFIT0041
    INTO TABLE IT_ZFIT0041
    FOR ALL ENTRIES IN IT_ZFIT0036
    WHERE NR_INVOICE  = IT_ZFIT0036-INVOICE.

  CHECK IT_ZFIT0041[] IS NOT INITIAL.

  SELECT NR_REGISTRO_EXPO NUMERO_DUE ID_NOMEACAO_TRAN VBELN
    FROM ZDOC_EXP
    INTO TABLE IT_ZDOC_EXP
    FOR ALL ENTRIES IN IT_ZFIT0041
    WHERE NR_REGISTRO_EXPO  = IT_ZFIT0041-NR_RE.

  SELECT NR_REGISTRO_EXPO NUMERO_DUE ID_NOMEACAO_TRAN VBELN
    FROM ZDOC_EXP
    APPENDING TABLE IT_ZDOC_EXP
    FOR ALL ENTRIES IN IT_ZFIT0041
    WHERE NUMERO_DUE  = IT_ZFIT0041-NR_RE.

  CHECK IT_ZDOC_EXP[] IS NOT INITIAL.

  SELECT ID_NOMEACAO_TRAN DT_DATA ID_CONHEC
    FROM ZNOM_CONHEC
    INTO TABLE IT_ZNOM_CONHEC
    FOR ALL ENTRIES IN IT_ZDOC_EXP
    WHERE ID_NOMEACAO_TRAN =  IT_ZDOC_EXP-ID_NOMEACAO_TRAN.


  SELECT  ID_NOMEACAO_TRAN DS_PORTO DS_NOME_TRANSPOR
    FROM ZNOM_TRANSPORTE
    INTO TABLE IT_ZNOM_TRANSPORTE
    FOR ALL ENTRIES IN IT_ZDOC_EXP
    WHERE ID_NOMEACAO_TRAN =  IT_ZDOC_EXP-ID_NOMEACAO_TRAN.

  SELECT VBELN MATNR
    FROM LIPS
    INTO TABLE IT_LIPS
    FOR ALL ENTRIES IN IT_ZDOC_EXP
    WHERE VBELN   = IT_ZDOC_EXP-VBELN.

  CHECK IT_LIPS[] IS NOT  INITIAL.

  SELECT MATNR SPRAS MAKTX
    FROM MAKT
    INTO TABLE IT_MAKT
    FOR ALL ENTRIES IN IT_LIPS
    WHERE MATNR = IT_LIPS-MATNR
    AND   SPRAS = SY-LANGU.

  SELECT MATNR SPRAS MAKTX
  FROM MAKT
  APPENDING TABLE IT_MAKT
  FOR ALL ENTRIES IN IT_ZFIT0036
  WHERE MATNR = IT_ZFIT0036-MATNR
  AND   SPRAS = SY-LANGU.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_SAIDA .
  SORT: IT_ZFIT0041         BY NR_INVOICE,
        IT_ZIB_CONTABIL_CHV BY OBJ_KEY,
        IT_BSAD             BY BELNR GJAHR BUKRS,
        IT_ZDOC_EXP         BY NR_REGISTRO_EXPO,
        IT_ZNOM_CONHEC      BY ID_NOMEACAO_TRAN DT_DATA DESCENDING,
        IT_ZNOM_TRANSPORTE  BY ID_NOMEACAO_TRAN,
        IT_LIPS             BY VBELN,
        IT_MAKT             BY MATNR,
        IT_KNA1             BY KUNNR.

  LOOP AT IT_ZFIT0036 INTO WA_ZFIT0036.
    READ TABLE IT_ZIB_CONTABIL_CHV INTO WA_ZIB_CONTABIL_CHV WITH KEY OBJ_KEY  = WA_ZFIT0036-OBJ_KEY BINARY SEARCH.
    CHECK SY-SUBRC = 0.
    READ TABLE IT_BSAD INTO WA_BSAD WITH KEY  BELNR = WA_ZIB_CONTABIL_CHV-BELNR
                                              GJAHR = WA_ZIB_CONTABIL_CHV-GJAHR
                                              BUKRS = WA_ZIB_CONTABIL_CHV-BUKRS BINARY SEARCH.
    CHECK SY-SUBRC = 0.
    READ TABLE IT_KNA1 INTO WA_KNA1 WITH KEY KUNNR = WA_BSAD-KUNNR BINARY SEARCH.
    CHECK SY-SUBRC = 0.
    WA_SAIDA-NAME1            = WA_KNA1-NAME1.
    READ TABLE IT_ZFIT0041 INTO WA_ZFIT0041 WITH KEY NR_INVOICE	=	WA_ZFIT0036-INVOICE BINARY SEARCH.
    IF SY-SUBRC = 0.
      LOOP AT IT_ZFIT0041 INTO WA_ZFIT0041 WHERE NR_INVOICE	=	WA_ZFIT0036-INVOICE.
        READ TABLE IT_ZDOC_EXP INTO WA_ZDOC_EXP WITH KEY NR_REGISTRO_EXPO  = WA_ZFIT0041-NR_RE.
        IF SY-SUBRC NE 0.
          READ TABLE IT_ZDOC_EXP INTO WA_ZDOC_EXP WITH KEY NUMERO_DUE  = WA_ZFIT0041-NR_RE.
          CHECK SY-SUBRC = 0.
        ENDIF.
        READ TABLE IT_ZNOM_CONHEC	INTO WA_ZNOM_CONHEC WITH KEY ID_NOMEACAO_TRAN =	WA_ZDOC_EXP-ID_NOMEACAO_TRAN BINARY SEARCH.
        CHECK SY-SUBRC = 0.
        READ TABLE IT_ZNOM_TRANSPORTE INTO WA_ZNOM_TRANSPORTE WITH KEY ID_NOMEACAO_TRAN	=	WA_ZDOC_EXP-ID_NOMEACAO_TRAN BINARY SEARCH.
        CHECK SY-SUBRC = 0.
        READ TABLE IT_LIPS INTO WA_LIPS WITH KEY VBELN =  WA_ZDOC_EXP-VBELN BINARY SEARCH.
        CHECK SY-SUBRC = 0.
        READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR  = WA_LIPS-MATNR BINARY SEARCH.
        CHECK SY-SUBRC = 0.
        "
        WA_SAIDA-DS_NOME_TRANSPOR = WA_ZNOM_TRANSPORTE-DS_NOME_TRANSPOR.
        WA_SAIDA-DS_PORTO         = WA_ZNOM_TRANSPORTE-DS_PORTO.
        WA_SAIDA-MAKTX            = WA_MAKT-MAKTX.
        WA_SAIDA-DT_DATA          = WA_ZNOM_CONHEC-DT_DATA.
        WA_SAIDA-TEMPO            = WA_BSAD-AUGDT - WA_ZNOM_CONHEC-DT_DATA.
        EXIT. " Uma RE
      ENDLOOP.
    ELSE.
      WA_SAIDA-DS_NOME_TRANSPOR = WA_ZFIT0036-NAVIO.
      WA_SAIDA-DS_PORTO         = 'Erro integração'.
      READ TABLE IT_MAKT INTO WA_MAKT WITH KEY MATNR  = WA_ZFIT0036-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_SAIDA-MAKTX            = WA_MAKT-MAKTX.
      ENDIF.
    ENDIF.
    WA_SAIDA-AUGDT            = WA_BSAD-AUGDT.
    WA_SAIDA-INVOICE          = WA_ZFIT0036-INVOICE.
    WA_SAIDA-DMBE2            = WA_BSAD-DMBE2.

    "
    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR: WA_SAIDA.

    CLEAR:    WA_BSAD,
              WA_KNA1,
              WA_ZFIT0041 ,
              WA_ZDOC_EXP,
              WA_ZNOM_CONHEC ,
              WA_ZNOM_TRANSPORTE,
              WA_LIPS,
              WA_MAKT.

  ENDLOOP.
ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_IMPRIME_DADOS .
  PERFORM F_DEFINIR_EVENTOS.

  PERFORM F_ALV.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM      = V_REPORT
      IS_LAYOUT               = GD_LAYOUT
      "I_CALLBACK_PF_STATUS_SET  = 'SET_PF_STATUS'
      "I_CALLBACK_USER_COMMAND   = 'USER_COMMAND'
      IT_FIELDCAT             = IT_FCAT[]
      IT_SORT                 = T_SORT[]
      I_SAVE                  = 'X'
      IT_EVENTS               = EVENTS
      IS_PRINT                = T_PRINT
*      IS_VARIANT              = VG_VARIANT
    TABLES
      T_OUTTAB                = IT_SAIDA.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_DEFINIR_EVENTOS .
  PERFORM F_CARREGAR_EVENTOS USING:
                                     SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0621   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.
ENDFORM.                      " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ALV .

  PERFORM ALV_PREENCHE_CAT USING:
             'DS_NOME_TRANSPOR'   TEXT-002 '50'       ' '     ' '    ' ', "Nome do navio
             'NAME1'              TEXT-003 '30'       ' '     ' '    ' ', "Comprador
             'DS_PORTO'           TEXT-004 '30'       ' '     ' '    ' ', "Porto
             'MAKTX'              TEXT-005 '30'       ' '     ' '    ' ', "Produto
             'DT_DATA'            TEXT-006 '15'       ' '     ' '    ' ', "Data do BL
             'AUGDT'              TEXT-007 '18'       ' '     ' '    ' ', "Data do recebimento
             'INVOICE'            TEXT-008 '15'       ' '     ' '    ' ', "nº da invoice
             'DMBE2'              TEXT-009 '20'       ' '     ' '    ' ', "Valor recebido (U$)
             'TEMPO'              TEXT-010 '12'       ' '     ' '    ' '. "Tempo gasto

ENDFORM.                    " F_AL
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0649   text
*      -->P_TEXT_003  text
*      -->P_0651   text
*      -->P_0652   text
*      -->P_0653   text
*      -->P_0654   text
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

  APPEND WL_FCAT TO IT_FCAT.
ENDFORM.                    " ALV_PREENCHE_CAT
