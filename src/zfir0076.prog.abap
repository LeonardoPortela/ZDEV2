*&---------------------------------------------------------------------*
*& Report  ZFIR0076
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZFIR0076.
TABLES: ZFISOLCTR.
*======================================================================*
* TYPES                                                                *
*======================================================================*
TYPES: BEGIN OF TY_SAIDA,
         ZID_EMPRESA   TYPE ZFICTRMU-ZID_EMPRESA,
         DT_INI        TYPE ZFITAXCTR-DT_INI,
         DT_FIM        TYPE ZFITAXCTR-DT_FIM,
         TAXA          TYPE ZFITAXCTR-TAXA,
         VLR_CTR       TYPE ZFITAXCTR-VLR_CTR,
         WAERS         TYPE ZFITAXCTR-WAERS,
         DES_EMPRESA   TYPE T001-BUTXT,
         ZID_CONTR     TYPE ZFIRESCMU-ZID_CONTR,
         NOM_CONTR     TYPE ZFITAXCTR-NOM_CONTR,
         ZRESDT_CALC   TYPE ZFIRESCMU-ZRESDT_CALC,
         ZRESVL_PRNC   TYPE BSEG-WRBTR, "ZFIRESCMU-ZRESVL_PRINC,
         ZRESID_JUROS  TYPE BSEG-WRBTR, "ZFIRESCMU-ZRESID_JUROS,
         ZRESID_IOF    TYPE BSEG-WRBTR, "ZFIRESCMU-ZRESID_IOF,
         ZRESID_IR     TYPE BSEG-WRBTR, "ZFIRESCMU-ZRESID_IR,
         ZRESID_RESULT TYPE BSEG-WRBTR, "ZFIRESCMU-ZRESID_RESULT,
         BELNR_RES     TYPE ZFIRESCMU-BELNR_RES.
TYPES: END OF TY_SAIDA.


TYPES: BEGIN OF TY_BKPF,
         BUKRS TYPE BKPF-BUKRS,
         BELNR TYPE BKPF-BELNR,
         GJAHR TYPE BKPF-GJAHR,
         BUDAT TYPE BKPF-BUDAT,
         BLDAT TYPE BKPF-BLDAT.
TYPES:END OF TY_BKPF.


TYPES: BEGIN OF TY_BSEG,
         BUKRS TYPE BSEG-BUKRS,
         BELNR TYPE BSEG-BELNR,
         GJAHR TYPE BSEG-GJAHR,
         DMBTR TYPE BSEG-DMBTR,
         DMBE2 TYPE BSEG-DMBE2,
         GSBER TYPE BSEG-GSBER,
         ZUONR TYPE BSEG-ZUONR.
TYPES:END OF TY_BSEG.


TYPES:BEGIN OF TY_ZFIRESCMU.
        INCLUDE STRUCTURE ZFIRESCMU.
TYPES:  BELNR TYPE ZIB_CONTABIL_CHV-BELNR,
        GJAHR TYPE ZIB_CONTABIL_CHV-GJAHR.
TYPES: END OF TY_ZFIRESCMU.

TYPES:BEGIN OF TY_ZFISOLCTR.
        INCLUDE STRUCTURE ZFISOLCTR.
TYPES:  OBJ_KEYF TYPE AWKEY,
        OBJ_KEYC TYPE AWKEY.
TYPES: END OF TY_ZFISOLCTR.

*======================================================================*
* VARIÁVEIS                                                            *
*======================================================================*
DATA: IT_ZFIRESCMU        TYPE TABLE OF TY_ZFIRESCMU,
      IT_ZFICTRMU         TYPE TABLE OF ZFICTRMU,
      IT_ZFISOLCTR        TYPE TABLE OF TY_ZFISOLCTR,
      IT_ZFISOLCTR_D      TYPE TABLE OF TY_ZFISOLCTR,
      IT_BSEG             TYPE TABLE OF TY_BSEG,
      IT_BKPF             TYPE TABLE OF TY_BKPF,
      IT_ZIB_CONTABIL_CHV TYPE TABLE OF ZIB_CONTABIL_CHV,
      WA_ZIB_CONTABIL_CHV TYPE ZIB_CONTABIL_CHV,
      WA_ZFIRESCMU        TYPE TY_ZFIRESCMU,
      WA_ZFICTRMU         TYPE ZFICTRMU,
      WA_ZFISOLCTR        TYPE TY_ZFISOLCTR,
      WA_BSEG             TYPE TY_BSEG,
      WA_BKPF             TYPE TY_BKPF,
      WA_ZFITAXCTR        TYPE ZFITAXCTR,
      IT_SAIDA            TYPE STANDARD TABLE OF TY_SAIDA,
      WA_SAIDA            TYPE TY_SAIDA.

DATA: GR_ALVGRID             TYPE REF TO CL_GUI_ALV_GRID,
      GC_CUSTOM_CONTROL_NAME TYPE SCRFNAME VALUE 'CC_ALV',
      GR_CCONTAINER          TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GT_FIELDCAT            TYPE LVC_T_FCAT,
      GS_LAYOUT              TYPE LVC_S_LAYO,
      IT_SORT                TYPE LVC_T_SORT,
      WA_SORT                TYPE LVC_S_SORT.

DATA: OK_CODE TYPE SY-UCOMM.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS: ZM_HANDLE_HOTSPOT FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID
                  E_COLUMN_ID
                  ES_ROW_NO.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*======================================================================*
* SELECTION-SCREEN                                                     *
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*
*SELECT-OPTIONS: S_BUKRS FOR ZFISOLCTR-BUKRS_F OBLIGATORY,
*                S_DATA  FOR ZFISOLCTR-DT_LCTO OBLIGATORY.
PARAMETERS: P_DATA TYPE ZFISOLCTR-DT_LCTO OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.
*======================================================================*
* START-OF-SELECTION                                                   *
*======================================================================*
PERFORM SELECIONA_DADOS.
PERFORM ORGANIZA_DADOS.

IF IT_SAIDA IS INITIAL.
  MESSAGE TEXT-002 TYPE 'S' DISPLAY LIKE 'E'.
  STOP.
ELSE.
  CALL SCREEN 9000.
ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SELECIONA_DADOS .
  DATA:  VDATAI   TYPE ZFISOLCTR-DT_LCTO,
         VOBJ_KEY TYPE ZIB_CONTABIL-OBJ_KEY,
         TABIX    TYPE SY-TABIX.

  REFRESH IT_BSEG.

  SELECT *
   FROM ZFIRESCMU
   INTO TABLE IT_ZFIRESCMU
   WHERE ZRESDT_CALC EQ P_DATA
   AND   BELNR_RES   NE ''.

*  " Pega os lançamentos de IR+IOF+JUR ( DOC_LCTO )
*  SELECT *
*    FROM ZFIRESCMU
*    INTO TABLE IT_ZFIRESCMU
*    WHERE ZID_EMPRESA IN S_BUKRS
*    AND   ZRESDT_CALC IN S_DATA
*    AND   BELNR_RES   NE ''.

*  LOOP AT IT_ZFIRESCMU INTO WA_ZFIRESCMU.
*    IF WA_ZFIRESCMU-DOC_LCTO IS NOT INITIAL.
*      TABIX = SY-TABIX.
*      CONCATENATE 'ZGL17' WA_ZFIRESCMU-DOC_LCTO WA_ZFIRESCMU-ZRESDT_CALC+0(4) INTO VOBJ_KEY.
*      SELECT SINGLE *
*       FROM ZIB_CONTABIL_CHV
*       INTO  WA_ZIB_CONTABIL_CHV
*       WHERE OBJ_KEY EQ VOBJ_KEY.
*
*      IF SY-SUBRC = 0.
*        WA_ZFIRESCMU-BELNR = WA_ZIB_CONTABIL_CHV-BELNR.
*        WA_ZFIRESCMU-GJAHR = WA_ZIB_CONTABIL_CHV-GJAHR.
*        MODIFY IT_ZFIRESCMU FROM WA_ZFIRESCMU INDEX TABIX TRANSPORTING BELNR GJAHR.
*      ENDIF.
*    ENDIF.
*
*  ENDLOOP.
*
*  IF IT_ZFIRESCMU[] IS NOT INITIAL.
*    SELECT BUKRS BELNR GJAHR DMBTR DMBE2 GSBER ZUONR
*        FROM BSEG
*        INTO TABLE IT_BSEG
*        FOR ALL ENTRIES IN IT_ZFIRESCMU
*        WHERE BUKRS EQ IT_ZFIRESCMU-ZID_EMPRESA
*        AND   BELNR EQ IT_ZFIRESCMU-BELNR
*        AND   GJAHR EQ IT_ZFIRESCMU-GJAHR
*        AND   UMSKZ EQ ''
*        AND   AUGBL NE ''.
*  ENDIF.
*
*  "Pega Captações/Amortizações (-resisual)
*  SELECT *
*    FROM  ZFISOLCTR
*    INTO TABLE IT_ZFISOLCTR
*     WHERE BUKRS_F     IN S_BUKRS
*     AND   DT_LCTO     IN S_DATA
*     AND   CD_MOD      NE 'R'.
*
*  LOOP AT IT_ZFISOLCTR INTO WA_ZFISOLCTR.
*    IF WA_ZFISOLCTR-BUKRS_F = '9999'.
*      CLEAR WA_ZFISOLCTR-OBJ_KEYF.
*    ELSE.
*      CONCATENATE 'ZGL17' WA_ZFISOLCTR-DOC_LCTOF WA_ZFISOLCTR-DT_LCTO+0(4) INTO WA_ZFISOLCTR-OBJ_KEYF.
*    ENDIF.
*    IF WA_ZFISOLCTR-BUKRS_C = '9999'.
*      CLEAR WA_ZFISOLCTR-OBJ_KEYC.
*    ELSE.
*      CONCATENATE 'ZGL17' WA_ZFISOLCTR-DOC_LCTOC WA_ZFISOLCTR-DT_LCTO+0(4) INTO WA_ZFISOLCTR-OBJ_KEYC.
*    ENDIF.
*    MODIFY IT_ZFISOLCTR FROM WA_ZFISOLCTR INDEX SY-TABIX TRANSPORTING OBJ_KEYF OBJ_KEYC.
*  ENDLOOP.
*
*  IF IT_ZFISOLCTR[] IS NOT INITIAL.
*    SELECT  *
*       FROM ZIB_CONTABIL_CHV
*       INTO TABLE IT_ZIB_CONTABIL_CHV
*       FOR ALL ENTRIES IN IT_ZFISOLCTR
*       WHERE OBJ_KEY EQ IT_ZFISOLCTR-OBJ_KEYF.
*
*    SELECT  *
*     FROM ZIB_CONTABIL_CHV
*     APPENDING TABLE IT_ZIB_CONTABIL_CHV
*     FOR ALL ENTRIES IN IT_ZFISOLCTR
*     WHERE OBJ_KEY EQ IT_ZFISOLCTR-OBJ_KEYC.
*
*    SORT IT_ZIB_CONTABIL_CHV BY OBJ_KEY.
*
*    LOOP AT IT_ZFISOLCTR INTO WA_ZFISOLCTR.
*      TABIX = SY-TABIX.
*      "docs fornecedor
*      READ TABLE IT_ZIB_CONTABIL_CHV INTO WA_ZIB_CONTABIL_CHV WITH KEY OBJ_KEY = WA_ZFISOLCTR-OBJ_KEYF BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        WA_ZFISOLCTR-BELNRF = WA_ZIB_CONTABIL_CHV-BELNR.
*        WA_ZFISOLCTR-GJAHRF = WA_ZIB_CONTABIL_CHV-GJAHR.
*      ENDIF.
*      "docs cliente
*      READ TABLE IT_ZIB_CONTABIL_CHV INTO WA_ZIB_CONTABIL_CHV WITH KEY OBJ_KEY = WA_ZFISOLCTR-OBJ_KEYC BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        WA_ZFISOLCTR-BELNRC = WA_ZIB_CONTABIL_CHV-BELNR.
*        WA_ZFISOLCTR-GJAHRC = WA_ZIB_CONTABIL_CHV-GJAHR.
*      ENDIF.
*      MODIFY IT_ZFISOLCTR FROM WA_ZFISOLCTR INDEX TABIX TRANSPORTING BELNRF GJAHRF BELNRC GJAHRC.
*    ENDLOOP.
*    IF IT_ZFISOLCTR[] IS NOT INITIAL.
*      SELECT BUKRS BELNR GJAHR DMBTR DMBE2 GSBER
*          FROM BSEG
*          APPENDING TABLE IT_BSEG
*          FOR ALL ENTRIES IN IT_ZFISOLCTR
*          WHERE BUKRS EQ IT_ZFISOLCTR-BUKRS_C
*          AND   BELNR EQ IT_ZFISOLCTR-BELNRC
*          AND   GJAHR EQ IT_ZFISOLCTR-GJAHRC
*          AND   UMSKZ EQ ''
*          AND   AUGBL NE ''.
*      "
*      SELECT BUKRS BELNR GJAHR DMBTR DMBE2 GSBER
*         FROM BSEG
*         APPENDING TABLE IT_BSEG
*         FOR ALL ENTRIES IN IT_ZFISOLCTR
*         WHERE BUKRS EQ IT_ZFISOLCTR-BUKRS_F
*         AND   BELNR EQ IT_ZFISOLCTR-BELNRF
*         AND   GJAHR EQ IT_ZFISOLCTR-GJAHRF
*         AND   UMSKZ EQ ''
*         AND   AUGBL NE ''.
*
*    ENDIF.
*  ENDIF.
*
*  IF IT_BSEG[] IS NOT INITIAL.
*    SELECT BUKRS BELNR GJAHR BUDAT BLDAT
*        FROM BKPF
*        INTO TABLE IT_BKPF
*        FOR ALL ENTRIES IN IT_BSEG
*        WHERE BUKRS EQ IT_BSEG-BUKRS
*        AND   BELNR EQ IT_BSEG-BELNR
*        AND   GJAHR EQ IT_BSEG-GJAHR.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ORGANIZA_DADOS.

  DATA: WL_ZFITAXCTR TYPE ZFITAXCTR.
  "
  LOOP AT IT_ZFIRESCMU INTO WA_ZFIRESCMU.
    WA_SAIDA-ZID_EMPRESA = WA_ZFIRESCMU-ZID_EMPRESA.
    SELECT SINGLE BUTXT
      FROM T001
      INTO WA_SAIDA-DES_EMPRESA
      WHERE BUKRS EQ WA_ZFIRESCMU-ZID_EMPRESA
      AND SPRAS EQ SY-LANGU.

    WA_SAIDA-ZID_CONTR = WA_ZFIRESCMU-ZID_CONTR.
    SELECT SINGLE *
      FROM ZFITAXCTR
      INTO  WL_ZFITAXCTR
      WHERE ZID_CONTR EQ WA_ZFIRESCMU-ZID_CONTR.

    WA_SAIDA-DT_INI      = WL_ZFITAXCTR-DT_INI.
    WA_SAIDA-DT_FIM      = WL_ZFITAXCTR-DT_FIM.
    WA_SAIDA-TAXA        = WL_ZFITAXCTR-TAXA.
    WA_SAIDA-VLR_CTR     = WL_ZFITAXCTR-VLR_CTR.
    WA_SAIDA-WAERS       = WL_ZFITAXCTR-WAERS.
    WA_SAIDA-NOM_CONTR   = WL_ZFITAXCTR-NOM_CONTR.
    WA_SAIDA-ZRESDT_CALC = P_DATA.
    WA_SAIDA-ZRESVL_PRNC = WA_ZFIRESCMU-ZRESVL_PRINC.
    WA_SAIDA-ZRESID_JUROS = WA_ZFIRESCMU-ZRESID_JUROS.
    WA_SAIDA-ZRESID_IOF = WA_ZFIRESCMU-ZRESID_IOF.
    WA_SAIDA-ZRESID_IR = WA_ZFIRESCMU-ZRESID_IR.
    IF  WA_SAIDA-ZRESID_RESULT = WA_ZFIRESCMU-ZRESVL_PRINC.
      WA_SAIDA-ZRESID_RESULT = WA_ZFIRESCMU-ZRESID_RESULT + WA_ZFIRESCMU-ZRESID_JUROS + WA_ZFIRESCMU-ZRESID_IOF - WA_ZFIRESCMU-ZRESID_IR.
    ELSE.
      WA_SAIDA-ZRESID_RESULT = WA_ZFIRESCMU-ZRESID_RESULT.
    ENDIF.
    WA_SAIDA-BELNR_RES = WA_ZFIRESCMU-BELNR_RES.

    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR WA_SAIDA.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  MOSTRAR_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOSTRAR_ALV OUTPUT.
  PERFORM MOSTRAR_ALV.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MOSTRAR_ALV .

  DATA: WA_EVENT TYPE REF TO LCL_EVENT_RECEIVER.

  IF GR_ALVGRID IS INITIAL .

    CREATE OBJECT GR_CCONTAINER
      EXPORTING
        CONTAINER_NAME              = GC_CUSTOM_CONTROL_NAME
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.

    IF SY-SUBRC <> 0.
      "DO NOTHING
    ENDIF.

    CREATE OBJECT GR_ALVGRID
      EXPORTING
        I_PARENT          = GR_CCONTAINER
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.

    IF SY-SUBRC <> 0.
      "DO NOTHING
    ENDIF.

    PERFORM PREPARA_FIELD_CATALOG CHANGING GT_FIELDCAT .
    PERFORM PREPARA_LAYOUT CHANGING GS_LAYOUT .

    CREATE OBJECT WA_EVENT.
    SET HANDLER: WA_EVENT->ZM_HANDLE_HOTSPOT FOR GR_ALVGRID.

    CALL METHOD GR_ALVGRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*       I_BUFFER_ACTIVE               =
*       I_CONSISTENCY_CHECK           =
*       I_STRUCTURE_NAME              =
*       IS_VARIANT                    =
*       I_SAVE                        =
*       I_DEFAULT                     = 'X'
        IS_LAYOUT                     = GS_LAYOUT
*       IS_PRINT                      =
*       IT_SPECIAL_GROUPS             =
*       IT_TOOLBAR_EXCLUDING          =
*       IT_HYPERLINK                  =
      CHANGING
        IT_OUTTAB                     = IT_SAIDA
        IT_FIELDCATALOG               = GT_FIELDCAT
        IT_SORT                       = IT_SORT
*       IT_FILTER                     =
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.

    IF SY-SUBRC <> 0.
      "DO NOTHING
    ENDIF.

  ELSE .

    CALL METHOD GR_ALVGRID->REFRESH_TABLE_DISPLAY
*     EXPORTING
*       IS_STABLE =
*       I_SOFT_REFRESH =
      EXCEPTIONS
        FINISHED = 1
        OTHERS   = 2.

    IF SY-SUBRC <> 0.
      "DO NOTHING
    ENDIF.

  ENDIF .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREPARA_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM PREPARA_FIELD_CATALOG  CHANGING P_GT_FIELDCAT.

  PERFORM MONTA_TAB_FIELD_CAT USING:
        'ZID_CONTR'         'Contrato'                ''       ''      '',

        'DT_INI'            'Data Inicio'             ''       ''      '',
        'DT_FIM'            'Data Fim'                ''       ''      '',
        'TAXA'              'Taxa'                    ''       ''      '',
        'VLR_CTR'           'Vlr. Inicial'            ''       ''      '',
        'WAERS'             'Moeda'                   ''       ''      '',
        'NOM_CONTR'         'Nome do Contrato'        ''       ''      '',
        'ZID_EMPRESA'       'Empresa'                 ''       ''      '',
        'DES_EMPRESA'       'Descrição'               ''       ''      '',
        'ZRESVL_PRNC'       'Valor Principal'         'X'      ''      '',
        'ZRESID_JUROS'      'Juros (+)'               'X'      ''      '',
        'ZRESID_IOF'        'IOF (+)'                 'X'      ''      '',
        'ZRESID_IR'         'IR (-)'                  'X'      ''      '',
        'ZRESID_RESULT'     'Valor Total'             'X'      ''      '',
        'BELNR_RES'         'Doc. Cont. Res.'         ''       ''      'X'.

  "Subtotal por Contrato
  WA_SORT-SPOS = 1.
  WA_SORT-FIELDNAME = 'ZID_CONTR'.
  WA_SORT-DOWN = 'X'.
  WA_SORT-GROUP = '*'.
  WA_SORT-SUBTOT = 'X'.
  APPEND WA_SORT TO IT_SORT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTA_TAB_FIELD_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MONTA_TAB_FIELD_CAT USING    VALUE(P_FNAME)
                                  VALUE(P_DESC)
                                  VALUE(P_SUM)
                                  VALUE(P_TAM)
                                  VALUE(P_HOT).

  DATA LS_FCAT TYPE LVC_S_FCAT.

  LS_FCAT-FIELDNAME = P_FNAME.
  LS_FCAT-COLTEXT = P_DESC.
  LS_FCAT-SCRTEXT_L  = P_DESC.
  LS_FCAT-SCRTEXT_M  = P_DESC.
  LS_FCAT-SCRTEXT_S  = P_DESC.
  LS_FCAT-OUTPUTLEN = P_TAM.
  LS_FCAT-DO_SUM = P_SUM.
  LS_FCAT-HOTSPOT = P_HOT.

  APPEND LS_FCAT TO GT_FIELDCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PREPARA_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM PREPARA_LAYOUT  CHANGING P_GS_LAYOUT TYPE LVC_S_LAYO.

  MOVE ABAP_TRUE TO P_GS_LAYOUT-ZEBRA.
  MOVE ABAP_TRUE TO P_GS_LAYOUT-CWIDTH_OPT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.

  SET PF-STATUS 'ZMUTUO'.
  SET TITLEBAR 'T001'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE OK_CODE.
    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      "DO NOTHING
  ENDCASE.
ENDMODULE.
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD ZM_HANDLE_HOTSPOT.

    CHECK E_ROW_ID-ROWTYPE(1) NE 'S'.
    CHECK E_ROW_ID-ROWTYPE(1) NE 'T'.

    PERFORM Z_HANDLE_HOTSPOT USING E_ROW_ID
                                   E_COLUMN_ID
                                   ES_ROW_NO.

  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM Z_HANDLE_HOTSPOT  USING    P_E_ROW_ID
                                P_E_COLUMN_ID
                                P_ES_ROW_NO.

  CLEAR: WA_SAIDA.
  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.

  IF SY-SUBRC EQ 0.
    SET PARAMETER ID 'BLN' FIELD WA_SAIDA-BELNR_RES.
    SET PARAMETER ID 'BUK' FIELD WA_SAIDA-ZID_EMPRESA.
    SET PARAMETER ID 'GJR' FIELD P_DATA(4).
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.
