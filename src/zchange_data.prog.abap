*&---------------------------------------------------------------------*
*& Report  ZCHANGE_DATA
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZCHANGE_DATA.

TABLES: SSCRFIELDS, ZLOG_CHANGE_DATA.

DATA: LT_FIELDS  TYPE TY_SVAL.
DATA: LS_FIELDS  LIKE LINE OF LT_FIELDS.

TYPES: BEGIN OF TY_ESTRUTURA.
         INCLUDE TYPE SLIS_FIELDCAT_MAIN.
         INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
       TYPES: END OF TY_ESTRUTURA.

 DATA: GIT_ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
       GWA_ESTRUTURA    TYPE TY_ESTRUTURA.


DATA: GIT_FIELDS TYPE TABLE OF SE16N_SELTAB,
      GIT_ZLOG_CHANGE_DATA TYPE TABLE OF ZLOG_CHANGE_DATA.

DATA: GWA_SEL_BUTTON         TYPE SMP_DYNTXT,
      GWA_ZLOG_CHANGE_DATA TYPE ZLOG_CHANGE_DATA.

DATA: GIT_TEXTO_DETALHE_ALT TYPE CATSXT_LONGTEXT_ITAB.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001 .

PARAMETERS: P_TABLE  TYPE SE16N_TAB.
PARAMETERS: P_MAXLIN TYPE SYTABIX DEFAULT 200 OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK b1.


SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

PARAMETERS: P_FIELD   TYPE SE16N_SELTAB-FIELD,
            P_OPTION  TYPE SE16N_SELTAB-OPTION,
            P_LOW     TYPE SE16N_SELTAB-LOW,
            P_HIGH    TYPE SE16N_SELTAB-HIGH.

SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

PARAMETERS: P_CASE TYPE ZLOG_CHANGE_DATA-CASE_NUMBER.
PARAMETERS: P_IR   TYPE ZLOG_CHANGE_DATA-IR_NUMBER.

SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.
SELECTION-SCREEN FUNCTION KEY 4.
SELECTION-SCREEN FUNCTION KEY 5.

INITIALIZATION.

  GWA_SEL_BUTTON-ICON_ID   = ICON_INSERT_ROW.
  GWA_SEL_BUTTON-ICON_TEXT = 'Adicionar Filtro'.
  SSCRFIELDS-FUNCTXT_01    = GWA_SEL_BUTTON.

  GWA_SEL_BUTTON-ICON_ID   = ICON_DELETE_ROW.
  GWA_SEL_BUTTON-ICON_TEXT = 'Remover Filtro'.
  SSCRFIELDS-FUNCTXT_02    = GWA_SEL_BUTTON.

  GWA_SEL_BUTTON-ICON_ID   = ICON_DISPLAY.
  GWA_SEL_BUTTON-ICON_TEXT = 'Visualizar Filtros'.
  SSCRFIELDS-FUNCTXT_03    = GWA_SEL_BUTTON.

  GWA_SEL_BUTTON-ICON_ID   = ICON_BEN_CURRENT_BENEFITS.
  GWA_SEL_BUTTON-ICON_TEXT = 'Detalhamento das Alterações'.
  SSCRFIELDS-FUNCTXT_04    = GWA_SEL_BUTTON.

  GWA_SEL_BUTTON-ICON_ID   = ICON_DOC_ITEM_DETAIL.
  GWA_SEL_BUTTON-ICON_TEXT = 'Histórico de Alterações'.
  SSCRFIELDS-FUNCTXT_05    = GWA_SEL_BUTTON.


AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'FC01'.

      IF P_FIELD IS INITIAL.
        MESSAGE 'Informe o Campo!' TYPE 'I'.
        EXIT.
      ENDIF.

      IF P_OPTION IS INITIAL.
        MESSAGE 'Informe o opção!' TYPE 'I'.
        EXIT.
      ENDIF.

      APPEND VALUE #(  FIELD  = P_FIELD
                       SIGN   = 'I'
                       OPTION = P_OPTION
                       LOW    = P_LOW
                       HIGH   = p_HIGH ) TO GIT_FIELDS.

      CLEAR: P_FIELD, P_OPTION, P_LOW, P_HIGH.

      MESSAGE 'Filtro adicionado com sucesso!' TYPE 'S'.

    WHEN 'FC02'.
      DELETE GIT_FIELDS WHERE FIELD EQ P_FIELD.

      MESSAGE 'Filtro removido com sucesso!' TYPE 'S'.
    WHEN 'FC03'.
      PERFORM F_SHOW_FILTERS.
    WHEN 'FC04'.
      PERFORM F_INF_DETALHAMENTO_ALTERACAO.
    WHEN 'FC05'.
      PERFORM F_SHOW_HIST_ALTERACOES.
  ENDCASE.


*--------------------------------------------------------------------------------------------*
*  Validações Screen
*--------------------------------------------------------------------------------------------*
  DATA(LVA_ERRO) = ABAP_FALSE.
  PERFORM F_CHECK_FILL_FIELDS_SCREEN CHANGING LVA_ERRO.

  IF LVA_ERRO EQ ABAP_TRUE.
    LEAVE TO SCREEN 1000.
  ENDIF.

START-OF-SELECTION.

*--------------------------------------------------------------------------------------------*
*  Registrar Log Alteração
*--------------------------------------------------------------------------------------------*

  GWA_ZLOG_CHANGE_DATA-DT_REGISTRO   = SY-DATUM.
  GWA_ZLOG_CHANGE_DATA-HR_REGISTRO   = SY-UZEIT.
  GWA_ZLOG_CHANGE_DATA-US_REGISTRO   = SY-UNAME.
  GWA_ZLOG_CHANGE_DATA-TABELA        = P_TABLE.
  GWA_ZLOG_CHANGE_DATA-CASE_NUMBER   = P_CASE.
  GWA_ZLOG_CHANGE_DATA-IR_NUMBER     = P_IR.

  MODIFY ZLOG_CHANGE_DATA FROM GWA_ZLOG_CHANGE_DATA.
  IF SY-SUBRC NE 0.
    MESSAGE 'Houve um erro ao salvar os logs de alteração! Tente novamente' TYPE 'I'.
    RETURN.
  ENDIF.

*--------------------------------------------------------------------------------------------*
*  Chamada a função para registro das alterações
*--------------------------------------------------------------------------------------------*

  DATA(LVA_CHECK_KEY) = ABAP_TRUE.

  CALL FUNCTION 'SE16N_INTERFACE'
    EXPORTING
      I_TAB                       = P_TABLE
      I_EDIT                      = ABAP_TRUE
      I_SAPEDIT                   = ABAP_TRUE
      I_MAX_LINES                 = P_MAXLIN
      I_CHECKKEY                  = LVA_CHECK_KEY
      I_TECH_NAMES                = ABAP_TRUE
    TABLES
      IT_SELFIELDS                = GIT_FIELDS
    EXCEPTIONS
      NO_VALUES                   = 1
      OTHERS                      = 2.


FORM F_MONTAR_ESTRUTURA USING  VALUE(P_COL_POS)       TYPE I
                               VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                               VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                               VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                               VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                               VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                               VALUE(P_OUTPUTLEN)
                               VALUE(P_HOTSPOT).

  CLEAR GWA_ESTRUTURA.

  GWA_ESTRUTURA-FIELDNAME     = P_FIELD.
  GWA_ESTRUTURA-TABNAME       = P_TABNAME.
  GWA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  GWA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  GWA_ESTRUTURA-KEY           = ' '.
  GWA_ESTRUTURA-KEY_SEL       = 'X'.
  GWA_ESTRUTURA-COL_POS       = P_COL_POS.
  GWA_ESTRUTURA-NO_OUT        = ' '.
  GWA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  GWA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  GWA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.
  GWA_ESTRUTURA-HOTSPOT       = P_HOTSPOT.

  IF P_SCRTEXT_L IS NOT INITIAL.
    GWA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  TRANSLATE  GWA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  GWA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  GWA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  GWA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND GWA_ESTRUTURA TO GIT_ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM F_SHOW_FILTERS .

   DATA: LWA_LAYOUT  TYPE SLIS_LAYOUT_ALV.

   CLEAR: GIT_ESTRUTURA[].

   LWA_LAYOUT-COLWIDTH_OPTIMIZE = ABAP_TRUE.

   PERFORM F_MONTAR_ESTRUTURA USING:

      1   ''  ''              'GIT_FIELDS' 'FIELD'      'Campo'              '20' '',
      2   ''  ''              'GIT_FIELDS' 'OPTION'     'Condição'           '08' '',
      3   ''  ''              'GIT_FIELDS' 'LOW'        'De'                 '02' '',
      4   ''  ''              'GIT_FIELDS' 'HIGH'       'Até'                '03' ''.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_grid_title = 'Campos do Filtro'
        is_layout    = LWA_LAYOUT
        IT_FIELDCAT  = GIT_ESTRUTURA
      TABLES
        t_outtab    = GIT_FIELDS.

ENDFORM.

FORM F_INF_DETALHAMENTO_ALTERACAO.

  CLEAR: GWA_ZLOG_CHANGE_DATA.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      IM_TITLE      = 'Detalhe a alteração que será realizada...'
    CHANGING
      CH_TEXT       = GIT_TEXTO_DETALHE_ALT.

  LOOP AT GIT_TEXTO_DETALHE_ALT INTO DATA(LWA_TEXTO).
    CONCATENATE GWA_ZLOG_CHANGE_DATA-DS_ALTERACAO LWA_TEXTO
           INTO GWA_ZLOG_CHANGE_DATA-DS_ALTERACAO SEPARATED BY SPACE.
  ENDLOOP.


ENDFORM.

FORM F_SHOW_HIST_ALTERACOES.

  DATA: LWA_REPORT  LIKE SY-REPID,
        LV_RETURN   TYPE C,
        LWA_LAYOUT  TYPE SLIS_LAYOUT_ALV.

  RANGES: LRA_DT_REGISTRO FOR ZLOG_CHANGE_DATA-DT_REGISTRO,
          LRA_CASE_NUMBER FOR ZLOG_CHANGE_DATA-CASE_NUMBER,
          LRA_IR_NUMBER   FOR ZLOG_CHANGE_DATA-IR_NUMBER.

  LWA_REPORT = SY-REPID.

  CLEAR: LT_FIELDS[], GIT_ESTRUTURA[], LRA_DT_REGISTRO[], GIT_ZLOG_CHANGE_DATA[],LRA_CASE_NUMBER[], LRA_IR_NUMBER[].

  CLEAR LS_FIELDS.
  LS_FIELDS-TABNAME   = 'ZSDT0001'.
  LS_FIELDS-FIELDTEXT = 'Periodo Inicio'.
  LS_FIELDS-FIELD_OBL = ABAP_FALSE.
  LS_FIELDS-FIELDNAME = 'DT_ABERTURA'.
  APPEND LS_FIELDS TO LT_FIELDS.

  LS_FIELDS-TABNAME   = 'ZSDT0001'.
  LS_FIELDS-FIELDTEXT = 'Periodo Final'.
  LS_FIELDS-FIELD_OBL = ABAP_FALSE.
  LS_FIELDS-FIELDNAME = 'DT_FECHAMENTO'.
  APPEND LS_FIELDS TO LT_FIELDS.

  LS_FIELDS-TABNAME   = 'ZLOG_CHANGE_DATA'.
  LS_FIELDS-FIELDTEXT = 'Numero Case'.
  LS_FIELDS-FIELD_OBL = ABAP_FALSE.
  LS_FIELDS-FIELDNAME = 'CASE_NUMBER'.
  APPEND LS_FIELDS TO LT_FIELDS.

  LS_FIELDS-TABNAME   = 'ZLOG_CHANGE_DATA'.
  LS_FIELDS-FIELDTEXT = 'Numero IR'.
  LS_FIELDS-FIELD_OBL = ABAP_FALSE.
  LS_FIELDS-FIELDNAME = 'IR_NUMBER'.
  APPEND LS_FIELDS TO LT_FIELDS.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      POPUP_TITLE     = 'Histórico de Alterações'
    IMPORTING
      RETURNCODE      = LV_RETURN
    TABLES
      FIELDS          = LT_FIELDS
    EXCEPTIONS
      ERROR_IN_FIELDS = 1
      OTHERS          = 2.

   READ TABLE LT_FIELDS INTO DATA(LWA_FIELD_DT_INI) WITH KEY FIELDNAME = 'DT_ABERTURA'.
   READ TABLE LT_FIELDS INTO DATA(LWA_FIELD_DT_FIM) WITH KEY FIELDNAME = 'DT_FECHAMENTO'.

   DATA(_INF_DATA) = ABAP_FALSE.
   DATA(_INF_CASE) = ABAP_FALSE.
   DATA(_INF_IR)   = ABAP_FALSE.

   IF LWA_FIELD_DT_INI-value IS NOT INITIAL AND
      LWA_FIELD_DT_FIM-value IS NOT INITIAL.


     _INF_DATA = ABAP_TRUE.

     APPEND VALUE #( SIGN   = 'I'
                     OPTION = 'BT'
                     LOW    = LWA_FIELD_DT_INI-value
                     HIGH   = LWA_FIELD_DT_FIM-value ) TO LRA_DT_REGISTRO.

   ELSEIF LWA_FIELD_DT_INI-value IS NOT INITIAL.

     _INF_DATA = ABAP_TRUE.

     APPEND VALUE #( SIGN   = 'I'
                     OPTION = 'EQ'
                     LOW    = LWA_FIELD_DT_INI-value ) TO LRA_DT_REGISTRO.

   ENDIF.



   READ TABLE LT_FIELDS INTO DATA(LWA_FIELD_CASE_NR) WITH KEY FIELDNAME = 'CASE_NUMBER'.
   IF SY-SUBRC EQ 0 AND LWA_FIELD_CASE_NR-VALUE IS NOT INITIAL.

     _INF_CASE = ABAP_TRUE.

     APPEND VALUE #( SIGN   = 'I'
                     OPTION = 'EQ'
                     LOW    = LWA_FIELD_CASE_NR-VALUE ) TO LRA_CASE_NUMBER.
   ENDIF.

   READ TABLE LT_FIELDS INTO DATA(LWA_FIELD_IR_NR) WITH KEY FIELDNAME = 'IR_NUMBER'.
   IF SY-SUBRC EQ 0 AND LWA_FIELD_IR_NR-VALUE IS NOT INITIAL.

     _INF_IR = ABAP_TRUE.

     APPEND VALUE #( SIGN   = 'I'
                     OPTION = 'EQ'
                     LOW    = LWA_FIELD_IR_NR-VALUE ) TO LRA_IR_NUMBER.
   ENDIF.

   IF _INF_DATA EQ ABAP_FALSE AND
      _INF_CASE EQ ABAP_FALSE AND
      _INF_IR   EQ ABAP_FALSE.
     MESSAGE 'Nenhum filtro foi informado!' TYPE 'I'.
     RETURN.
   ENDIF.


   LWA_LAYOUT-COLWIDTH_OPTIMIZE = ABAP_TRUE.

   SELECT *
     FROM ZLOG_CHANGE_DATA INTO TABLE GIT_ZLOG_CHANGE_DATA
    WHERE DT_REGISTRO IN LRA_DT_REGISTRO
      AND CASE_NUMBER IN LRA_CASE_NUMBER
      AND IR_NUMBER   IN LRA_IR_NUMBER.

   PERFORM F_MONTAR_ESTRUTURA USING:

      1   ''  ''              'LIT_ZLOG_CHANGE_DATA' 'DT_REGISTRO'    'Dt.Registro'               '12' '',
      2   ''  ''              'LIT_ZLOG_CHANGE_DATA' 'HR_REGISTRO'    'Hr.Registro'               '12' '',
      3   ''  ''              'LIT_ZLOG_CHANGE_DATA' 'US_REGISTRO'    'Us.Registro'               '12' '',
      4   ''  ''              'LIT_ZLOG_CHANGE_DATA' 'TABELA'         'Tabela'                    '10' '',
      5   ''  ''              'LIT_ZLOG_CHANGE_DATA' 'CASE_NUMBER'    'Numero Case'               '15' '',
      6   ''  ''              'LIT_ZLOG_CHANGE_DATA' 'IR_NUMBER'      'Numero IR'                 '15' '',
      7   ''  ''              'LIT_ZLOG_CHANGE_DATA' 'DS_ALTERACAO'   'Detalhamento Alteração'    '25' 'X'.


    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_grid_title             = 'Histórico de Alterações'
        i_callback_program       = LWA_REPORT
        is_layout                = LWA_LAYOUT
        i_callback_user_command  = 'USER_COMMAND'
        IT_FIELDCAT              = GIT_ESTRUTURA
      TABLES
        T_OUTTAB                 = GIT_ZLOG_CHANGE_DATA.



ENDFORM.



FORM user_command  USING r_ucomm      LIKE sy-ucomm
                         rs_selfield  TYPE slis_selfield.
  CASE r_ucomm.
    WHEN: '&IC1'.

      CASE RS_SELFIELD-FIELDNAME.
        WHEN 'DS_ALTERACAO'.

          DATA: LIT_TABLE      TYPE STANDARD TABLE OF TXLINE,
                LIT_TEXTO_SHOW TYPE CATSXT_LONGTEXT_ITAB,
                LVA_STRING     TYPE STRING.

          READ TABLE GIT_ZLOG_CHANGE_DATA INTO DATA(LWA_LOG_CHANGE_DATA) INDEX RS_SELFIELD-TABINDEX.
          CHECK SY-SUBRC EQ 0.

          LVA_STRING = LWA_LOG_CHANGE_DATA-DS_ALTERACAO.

          CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
            EXPORTING
              I_STRING            = LVA_STRING
              I_TABLINE_LENGTH    = 72
            TABLES
              ET_TABLE            = LIT_TABLE.

           LOOP AT LIT_TABLE INTO DATA(LWA_TABLE).
             APPEND LWA_TABLE TO LIT_TEXTO_SHOW.
           ENDLOOP.

           CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
            EXPORTING
              im_display_mode = 'X'
              IM_TITLE        = 'Detalhamento da alteração realizada'
            CHANGING
              CH_TEXT         = LIT_TEXTO_SHOW.

      ENDCASE.


  ENDCASE.


ENDFORM.

FORM F_CHECK_FILL_FIELDS_SCREEN CHANGING p_erro.

  CLEAR: p_erro.


  IF P_TABLE IS INITIAL.
    p_erro = ABAP_TRUE.
    MESSAGE 'Tabela não informada!' TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM TADIR INTO @DATA(LWA_TADIR)
   WHERE PGMID    EQ 'R3TR'
     AND OBJECT   EQ 'TABL'
     AND OBJ_NAME EQ @P_TABLE.

  IF SY-SUBRC NE 0 .
    p_erro = ABAP_TRUE.
    MESSAGE 'Tabela informada não existe!' TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM TVARVC INTO @DATA(LWA_STVARC)
   WHERE NAME EQ 'ZCHANGE_DATA_NOT_GET_INF_ATEND'
     AND LOW  EQ @ABAP_TRUE.

  IF SY-SUBRC NE 0.
    IF ( P_CASE IS INITIAL ) AND ( P_IR IS INITIAL ).
      p_erro = ABAP_TRUE.
      MESSAGE 'Informe o número do IR ou Case Rimini!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF ( P_CASE IS NOT INITIAL ) AND ( STRLEN( P_CASE ) < 8 ).
      p_erro = ABAP_TRUE.
      MESSAGE 'Numero case inválido!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF ( P_IR IS NOT INITIAL ) AND ( STRLEN( P_IR ) < 7 ).
      p_erro = ABAP_TRUE.
      MESSAGE 'Numero IR inválido!' TYPE 'S'.
      RETURN.
    ENDIF.

    IF GWA_ZLOG_CHANGE_DATA-DS_ALTERACAO IS INITIAL.
      p_erro = ABAP_TRUE.
      MESSAGE 'Detalhamento da alteração não foi informado!' TYPE 'S'.
      RETURN.
    ENDIF.


    IF STRLEN( GWA_ZLOG_CHANGE_DATA-DS_ALTERACAO  ) < 30.
      p_erro = ABAP_TRUE.
      MESSAGE 'Detalhamento da alteração deve ter no minimo 30 caracteres!' TYPE 'S'.
      RETURN.
    ENDIF.
  ENDIF.


ENDFORM.
