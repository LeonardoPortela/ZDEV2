*&---------------------------------------------------------------------*
*& Report  ZSDR0122
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZSDR0122.

TABLES: ZIB_NFE ,ZIB_NFE_FORN.

DATA: LT_URL_DOWN     TYPE TABLE OF ZIB_NFE.

DATA: LC_URI          TYPE STRING.

DATA: E_TEXTO	        TYPE STRING,
      E_TEXTO_X	      TYPE XSTRING,
      LS_STRING       TYPE SOLIX,
      LT_STRING       LIKE STANDARD TABLE OF LS_STRING,
      LV_PATH_SAVE    TYPE STRING,
      LV_NAME_FILE    TYPE STRING,
      LV_FILE_SAVE    TYPE C LENGTH 100,
      LV_SHOW_MSG     TYPE STRING.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_DOCNUM FOR ZIB_NFE-DOCNUM,
                "P_URL    FOR ZIB_NFE-DS_URL_DANFE NO-DISPLAY,
                P_CHAVES FOR ZIB_NFE_FORN-NU_CHAVE.
SELECTION-SCREEN: END OF BLOCK B1.

  CLEAR: LT_URL_DOWN[].

  CL_GUI_FRONTEND_SERVICES=>DIRECTORY_BROWSE(
    EXPORTING
      WINDOW_TITLE         = 'Pasta para salvar arquivos PDF'
    CHANGING
      SELECTED_FOLDER      = LV_PATH_SAVE
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4  ).

  IF P_DOCNUM[] IS NOT INITIAL.

    SELECT *
      FROM ZIB_NFE INTO TABLE LT_URL_DOWN
     WHERE DOCNUM IN P_DOCNUM.

  ENDIF.

  IF P_CHAVES[] IS INITIAL.
    MESSAGE 'Nenhuma chave foi informada!' TYPE 'I'.
    RETURN.
  ENDIF.

  LOOP AT LT_URL_DOWN INTO DATA(WL_URL_DOWN).

    CHECK WL_URL_DOWN-DS_URL_DANFE IS NOT INITIAL.

    SELECT SINGLE *
      FROM J_1BNFE_ACTIVE INTO @DATA(WL_ACTIVE)
     WHERE DOCNUM EQ @WL_URL_DOWN-DOCNUM.

    CHECK SY-SUBRC EQ 0.

    CONCATENATE WL_ACTIVE-REGIO WL_ACTIVE-NFYEAR WL_ACTIVE-NFMONTH WL_ACTIVE-STCD1
                WL_ACTIVE-MODEL WL_ACTIVE-SERIE  WL_ACTIVE-NFNUM9  WL_ACTIVE-DOCNUM9 WL_ACTIVE-CDV
                INTO DATA(V_CHAVE_DOC).

    "Validação Temporaria
    IF V_CHAVE_DOC NOT IN P_CHAVES.
      CONCATENATE 'Chave'  V_CHAVE_DOC 'não validada para download' INTO LV_SHOW_MSG.
      MESSAGE LV_SHOW_MSG TYPE 'I'.
      CONTINUE.
    ENDIF.

    CASE WL_ACTIVE-MODEL.
      WHEN '55'.
        LV_NAME_FILE = 'nfe' && V_CHAVE_DOC && '.pdf'.
      WHEN '57'.
        LV_NAME_FILE = 'cte' && V_CHAVE_DOC && '.pdf'.
      WHEN '58'.
        LV_NAME_FILE = 'mdfe' && V_CHAVE_DOC && '.pdf'.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    LC_URI = WL_URL_DOWN-DS_URL_DANFE.

    CL_HTTP_CLIENT=>CREATE_BY_URL(
      EXPORTING
        URL                = LC_URI
      IMPORTING
        CLIENT             = DATA(I_CLIENTE)
      EXCEPTIONS
        ARGUMENT_NOT_FOUND = 1
        PLUGIN_NOT_ACTIVE  = 2
        INTERNAL_ERROR     = 3
        OTHERS             = 4 ).

    CHECK SY-SUBRC IS INITIAL.

    I_CLIENTE->REQUEST->SET_METHOD( METHOD = IF_HTTP_REQUEST=>CO_REQUEST_METHOD_GET ).

    I_CLIENTE->SEND(
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        HTTP_INVALID_TIMEOUT       = 4
        OTHERS                     = 5
    ).

    CHECK SY-SUBRC IS INITIAL.

    I_CLIENTE->RECEIVE(
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        OTHERS                     = 4
    ).

    CHECK SY-SUBRC IS INITIAL.

    E_TEXTO   = I_CLIENTE->RESPONSE->GET_CDATA( ).
    E_TEXTO_X = I_CLIENTE->RESPONSE->GET_DATA( ).

    I_CLIENTE->CLOSE( ).

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        BUFFER     = E_TEXTO_X
      TABLES
        BINARY_TAB = LT_STRING.

    CONCATENATE LV_PATH_SAVE '\' LV_NAME_FILE INTO LV_FILE_SAVE.

    CALL FUNCTION 'SCMS_DOWNLOAD'
      EXPORTING
        FILENAME = LV_FILE_SAVE
        BINARY   = 'X'
        FRONTEND = 'X'
      TABLES
        DATA     = LT_STRING
      EXCEPTIONS
        ERROR    = 1
        OTHERS   = 2.

  ENDLOOP.
