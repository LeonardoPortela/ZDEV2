*&---------------------------------------------------------------------*
*& Report  ZMMR0032
*&--------------------------------------------------------------------&*
*& Projeto..: Amaggi                                                  &*
*& Autor....: Enio Jesus                                              &*
*& Data.....: 28/04/2015                                              &*
*& Descrição: Relatório Mensagem de Erros                             &*
*& Transação: PP                                                      &*
*&--------------------------------------------------------------------&*

REPORT  ZMMR0032.
TYPE-POOLS: VRM.
TABLES: ZPPT0006.
*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF TY_SAIDA,
         BUDAT        TYPE ZPPT0006-BUDAT,       "DATA LANÇ.
         WERKS        TYPE ZPPT0006-WERKS,       "CENTRO
         MATNR        TYPE ZPPT0006-MATNR,       "N. MATERIAL
         IDFARDINHO   TYPE ZPPT0006-ID_FARDINHO, "ID FARDINHO
         ID_COTTON    TYPE ZPPT0006-ID_COTTON,   " RFID
         IDFARDAO     TYPE ZPPT0006-ID_FARDAO,   "ID FARDAO
         ACHARG       TYPE ZPPT0006-ACHARG,      "N. LOTE FARDINHO
         CHARG        TYPE ZPPT0006-CHARG,       "N. LOTE FARDAO
         MSGNR        TYPE ZPPT0006-MSGNR,       "N. MENSAGEM
         STATUS       TYPE C LENGTH 2,           "STATUS MENSAGEM
         DATA         TYPE ZPPT0006-DATA,        "DATA
         HORA         TYPE ZPPT0006-HORA,        "HORA
         LGORT        TYPE ZPPT0006-LGORT,       "BLOCO
         DESC_MSG     TYPE T100-TEXT,            "DESCRIÇÃO DA MENSAGEM
         DESC_MAT     TYPE MAKT-MAKTX,           "DESCRIÇÃO DO MATERIAL
         CD_CLASS     TYPE ZPPT0006-CD_CLASSIFICACAO, "COD CLASSIFICAÇÃO
         CELLCOLORS   TYPE LVC_T_SCOL,                "CELL COLORS
         CLABS        TYPE MCHB-CLABS,                "PESO FARDÃO.
         PESO_LIQUIDO TYPE ZPPT0002-PESO_LIQUIDO.    "PESO FARDINHO.
TYPES: END OF TY_SAIDA.

*&---------------------------------------------------------------------*
*& TABELAS INTERNAS
*&---------------------------------------------------------------------*
DATA: IT_ZPPT0006     TYPE TABLE OF ZPPT0006,
      IT_ZPPT0006_AUX TYPE TABLE OF ZPPT0006,
      IT_ZPPT0002     TYPE TABLE OF ZPPT0002,
      IT_T100         TYPE TABLE OF T100,
      IT_SAIDA        TYPE TABLE OF TY_SAIDA WITH HEADER LINE,
      IT_MAKT         TYPE TABLE OF MAKT,
      IT_MCHB         TYPE TABLE OF MCHB.

DATA: IT_FCAT         TYPE TABLE OF LVC_S_FCAT.

*&---------------------------------------------------------------------*
*& TABELAS WORK-ÁREAS
*&---------------------------------------------------------------------*
DATA: WA_ZPPT0006     TYPE ZPPT0006,
      WA_ZPPT0002     TYPE ZPPT0002,
      WA_T100         TYPE T100,
      WA_SAIDA        TYPE TY_SAIDA,
      WA_MAKT         TYPE MAKT,
      WA_MCHB         TYPE MCHB,
      WA_MARA         TYPE MARA,
      WA_ZPPT0006_AUX TYPE ZPPT0006.

DATA: WA_LAYOUT TYPE LVC_S_LAYO,
      WA_CONT   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV    TYPE REF TO CL_GUI_ALV_GRID.

DATA: LS_CELLCOLOR TYPE LVC_S_SCOL,
      V_INDEX      TYPE SY-TABIX.

*&---------------------------------------------------------------------*
*& ESTRUTURA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_BUDAT   FOR ZPPT0006-BUDAT OBLIGATORY,
                P_WERKS   FOR ZPPT0006-WERKS NO INTERVALS NO-EXTENSION OBLIGATORY,
                P_FARD    FOR ZPPT0006-ID_FARDINHO,
                P_ACHARG  FOR ZPPT0006-ACHARG,
                P_CHARG   FOR ZPPT0006-CHARG,
                P_MATNR   FOR ZPPT0006-MATNR.


SELECTION-SCREEN: END OF BLOCK B1.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETER: P_PEND  RADIOBUTTON GROUP GR1,
           P_SUCE  RADIOBUTTON GROUP GR1,
           P_ERRO  RADIOBUTTON GROUP GR1,
           P_TODAS RADIOBUTTON GROUP GR1.
SELECTION-SCREEN: END OF BLOCK B2.

START-OF-SELECTION.
  PERFORM: SELECIONA_DADOS,
           F_ALV.

END-OF-SELECTION.

  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*& FORM SELECIONA DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS.

  "All error
  IF P_ERRO = 'X'.
    SELECT * FROM ZPPT0006 INTO TABLE IT_ZPPT0006
      WHERE BUDAT     IN P_BUDAT
      AND WERKS       IN P_WERKS
      AND MATNR       IN P_MATNR
      AND ID_FARDINHO IN P_FARD
      AND ACHARG      IN P_ACHARG
      AND CHARG       IN P_CHARG
      AND STATUS_MSG  EQ 'E'
      ORDER BY BUDAT CHARG ACHARG ID.

    "All pending
  ELSEIF P_PEND = 'X'.
    SELECT * FROM ZPPT0006 INTO TABLE IT_ZPPT0006
      WHERE STATUS_MSG EQ 'E'.

    SELECT * FROM ZPPT0006 INTO TABLE IT_ZPPT0006_AUX
      FOR ALL ENTRIES IN IT_ZPPT0006
        WHERE ID_FARDINHO EQ IT_ZPPT0006-ID_FARDINHO
          AND DATA >= IT_ZPPT0006-DATA
          AND HORA > IT_ZPPT0006-HORA
          AND STATUS_MSG EQ 'S'.

    LOOP AT IT_ZPPT0006_AUX INTO WA_ZPPT0006_AUX.
      READ TABLE IT_ZPPT0006 INTO WA_ZPPT0006
            WITH KEY ID_FARDINHO = WA_ZPPT0006_AUX-ID_FARDINHO.
      DELETE IT_ZPPT0006 WHERE ID_FARDINHO = WA_ZPPT0006_AUX-ID_FARDINHO.
    ENDLOOP.

    "All success
  ELSEIF P_SUCE = 'X'.
    SELECT * FROM ZPPT0006 INTO TABLE IT_ZPPT0006
      WHERE BUDAT     IN P_BUDAT
      AND WERKS       IN P_WERKS
      AND MATNR       IN P_MATNR
      AND ID_FARDINHO IN P_FARD
      AND ACHARG      IN P_ACHARG
      AND CHARG       IN P_CHARG
      AND STATUS_MSG  EQ 'S'
      ORDER BY BUDAT CHARG ACHARG ID.

    "All
  ELSEIF P_TODAS = 'X'.
    SELECT * FROM ZPPT0006 INTO TABLE IT_ZPPT0006
      WHERE BUDAT     IN P_BUDAT
      AND WERKS       IN P_WERKS
      AND MATNR       IN P_MATNR
      AND ID_FARDINHO IN P_FARD
      AND ACHARG      IN P_ACHARG
      AND CHARG       IN P_CHARG
      ORDER BY BUDAT CHARG ACHARG ID.
  ENDIF.

  "Tabela mensagens
  IF IT_ZPPT0006[] IS NOT INITIAL.
    SELECT * FROM T100 INTO TABLE IT_T100
      FOR ALL ENTRIES IN IT_ZPPT0006
        WHERE  MSGNR = IT_ZPPT0006-MSGNR
        AND  ARBGB = 'ZTRACECOTTON'.

    "Tabela materiais
    SELECT * FROM MAKT INTO TABLE IT_MAKT
      FOR ALL ENTRIES IN IT_ZPPT0006
        WHERE MATNR = IT_ZPPT0006-MATNR.

    "Peso fardão
    SELECT * FROM MCHB INTO TABLE IT_MCHB
      FOR ALL ENTRIES IN IT_ZPPT0006
        WHERE CHARG = IT_ZPPT0006-CHARG.

    "Peso fardinho
    SELECT * FROM ZPPT0002 INTO TABLE IT_ZPPT0002
       FOR ALL ENTRIES IN IT_ZPPT0006
         WHERE ACHARG = IT_ZPPT0006-ACHARG.
  ENDIF.

  SORT: IT_MAKT     BY MATNR,
        IT_T100     BY MSGNR,
        IT_MCHB     BY CHARG,
        IT_ZPPT0002 BY ACHARG,
        IT_ZPPT0006_AUX BY ID_FARDINHO.

  LOOP AT IT_ZPPT0006 INTO WA_ZPPT0006.

    READ TABLE IT_T100 INTO WA_T100
      WITH KEY MSGNR = WA_ZPPT0006-MSGNR BINARY SEARCH.

    READ TABLE IT_MAKT INTO WA_MAKT
      WITH KEY MATNR = WA_ZPPT0006-MATNR BINARY SEARCH.

    READ TABLE IT_MCHB INTO WA_MCHB
      WITH KEY CHARG = WA_ZPPT0006-CHARG BINARY SEARCH.

    READ TABLE IT_ZPPT0002 INTO WA_ZPPT0002
      WITH KEY ACHARG = WA_ZPPT0006-ACHARG BINARY SEARCH.

    READ TABLE IT_ZPPT0006_AUX INTO WA_ZPPT0006_AUX
     WITH KEY ID_FARDINHO = WA_ZPPT0006-ID_FARDINHO BINARY SEARCH.

    WA_SAIDA-WERKS      = WA_ZPPT0006-WERKS.
    WA_SAIDA-IDFARDINHO = WA_ZPPT0006-ID_FARDINHO.
    WA_SAIDA-ID_COTTON  = WA_ZPPT0006-ID_COTTON.
    WA_SAIDA-IDFARDAO   = WA_ZPPT0006-ID_FARDAO.
    WA_SAIDA-CHARG      = WA_ZPPT0006-CHARG.
    WA_SAIDA-MSGNR      = WA_ZPPT0006-MSGNR.
    WA_SAIDA-BUDAT      = WA_ZPPT0006-BUDAT.
    WA_SAIDA-DATA       = WA_ZPPT0006-DATA.
    WA_SAIDA-HORA       = WA_ZPPT0006-HORA.
    WA_SAIDA-LGORT      = WA_ZPPT0006-LGORT.
    WA_SAIDA-CD_CLASS   = WA_ZPPT0006-CD_CLASSIFICACAO.

    IF ( WA_ZPPT0006-CD_MENSAGEM IS NOT INITIAL ).
      WA_SAIDA-DESC_MSG   = WA_ZPPT0006-CD_MENSAGEM.
    ELSE.
      WA_SAIDA-DESC_MSG   = WA_T100-TEXT.
    ENDIF.

    IF P_PEND = 'X'.
      WA_SAIDA-STATUS     = 'EP'.
    ELSE.
      WA_SAIDA-STATUS     = WA_ZPPT0006-STATUS_MSG.
    ENDIF.

    IF WA_ZPPT0006-ACHARG NE WA_ZPPT0006-CHARG.
      WA_SAIDA-ACHARG     = WA_ZPPT0006-ACHARG.
    ENDIF.

    "Pega o novo nº material e descrição do doc gerado
    IF WA_SAIDA-DESC_MSG(5) EQ 'Doc.:'.
      CLEAR: WA_MAKT, WA_MARA.

      SELECT SINGLE * FROM MARA INTO WA_MARA
       WHERE NORMT = WA_ZPPT0006-CD_CLASSIFICACAO.

      SELECT SINGLE * FROM MAKT INTO WA_MAKT
        WHERE MATNR = WA_MARA-MATNR.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_MARA-MATNR
        IMPORTING
          OUTPUT = WA_SAIDA-MATNR.

      WA_SAIDA-DESC_MAT = WA_MAKT-MAKTX.

    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WA_ZPPT0006-MATNR
        IMPORTING
          OUTPUT = WA_SAIDA-MATNR.

      WA_SAIDA-DESC_MAT = WA_MAKT-MAKTX.
    ENDIF.

    "Caso for estorno, transforma peso fardão em negativo.
    IF WA_MAKT-MAKTX(23) EQ 'Estorno material fardão'.
      WA_SAIDA-CLABS = WA_MCHB-CLABS * -1.
    ELSE.
      WA_SAIDA-CLABS = WA_MCHB-CLABS.
    ENDIF.

    "Caso for estorno, transforma peso fardinho em negativo.
    IF WA_MAKT-MAKTX(25) EQ 'Estorno material fardinho'.
      WA_SAIDA-PESO_LIQUIDO = WA_ZPPT0002-PESO_LIQUIDO * -1.
    ELSE.
      WA_SAIDA-PESO_LIQUIDO = WA_ZPPT0002-PESO_LIQUIDO.
    ENDIF.

    APPEND WA_SAIDA TO IT_SAIDA.
  ENDLOOP.

  "Tratamento de cores no Alv
  LOOP AT IT_SAIDA.
    V_INDEX = SY-TABIX.

    CASE IT_SAIDA-STATUS.
      WHEN 'E'.
        LS_CELLCOLOR-FNAME = 'STATUS'.
        LS_CELLCOLOR-COLOR-COL = '6'.
        LS_CELLCOLOR-COLOR-INT = '0'.
        LS_CELLCOLOR-COLOR-INV = '0'.
      WHEN 'S'.
        LS_CELLCOLOR-FNAME = 'STATUS'.
        LS_CELLCOLOR-COLOR-COL = '5'.
        LS_CELLCOLOR-COLOR-INT = '0'.
        LS_CELLCOLOR-COLOR-INV = '0'.
      WHEN 'EP'.
        LS_CELLCOLOR-FNAME = 'STATUS'.
        LS_CELLCOLOR-COLOR-COL = '6'.
        LS_CELLCOLOR-COLOR-INT = '0'.
        LS_CELLCOLOR-COLOR-INV = '0'.
      WHEN OTHERS.
    ENDCASE.

    APPEND LS_CELLCOLOR TO IT_SAIDA-CELLCOLORS.
    MODIFY IT_SAIDA INDEX V_INDEX TRANSPORTING CELLCOLORS.
  ENDLOOP.

ENDFORM.                    "SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM F_ALV.

  PERFORM ALV_PREENCHE_CAT USING:
        'BUDAT'        'Data Lcto'             '10'    ''    ''    '',
        'WERKS'        'Centro'                '05'    ''    ''    '',
        'DESC_MSG'     'Desc. Mensagem'        '35'    ''    ''    '',
        'MATNR'        'Material'              '08'    ''    ''    '',
        'DESC_MAT'     'Desc. Material'        '32'    ''    ''    '',
        'IDFARDAO'     'ID Fardão'              '8'    ''    ''    '',
        'IDFARDINHO'   'ID Fardinho'            '8'    ''    ''    '',
        'ID_COTTON'    'ID Cotton'             '15'    ''    ''    '',
        'CHARG'        'Lote Fardao'           '11'    ''    ''    '',
        'ACHARG'       'Lote Fardinho'          '9'    ''    ''    '',
        'LGORT'        'Bloco'                 '05'    ''    ''    '',
        'CLABS'        'Peso Fardão'            '9'    ''    ''    '',
        'PESO_LIQUIDO' 'Peso Fardinho'         '10'    ''    ''    '',
        'CD_CLASS'     'Cód. Classificação'     '4'    ''    ''    '',
        'DATA'         'Data'                  '10'    ''    ''    '',
        'HORA'         'Hora'                  '08'    ''    ''    '',
        'STATUS'       'Status'                '05'    ''    ''    ''.

ENDFORM.                    "F_ALV

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING:     P_CAMPO TYPE C
                                  P_DESC  TYPE C
                                  P_TAM   TYPE C
                                  P_HOT   TYPE C
                                  P_ZERO  TYPE C
                                  P_SUM   TYPE C.

  DATA: WL_FCAT TYPE LVC_S_FCAT.

  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SCRTEXT_L = P_DESC.
  WL_FCAT-SCRTEXT_M = P_DESC.
  WL_FCAT-SCRTEXT_S = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-OUTPUTLEN = P_TAM.

  APPEND WL_FCAT TO IT_FCAT.
ENDFORM.                    "ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR 'TITLE_001'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.
  CREATE OBJECT WA_CONT
    EXPORTING
      CONTAINER_NAME              = 'TELA0100'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  CREATE OBJECT WA_ALV
    EXPORTING
      I_PARENT          = WA_CONT
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.

  WA_LAYOUT-CTAB_FNAME = 'CELLCOLORS'.
  CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
    CHANGING
      IT_OUTTAB                     = IT_SAIDA[]
      IT_FIELDCATALOG               = IT_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

ENDMODULE.                    "PBO_0100 OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
MODULE PAI_0100 INPUT.
  IF SY-DYNNR EQ '0100'.
    CASE SY-UCOMM.
      WHEN 'BACK' OR
           'CANC'.
        LEAVE TO SCREEN 0.
      WHEN 'EXIT'.
        LEAVE PROGRAM.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " PAI_0100  INPUT
