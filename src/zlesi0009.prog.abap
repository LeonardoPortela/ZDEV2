*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report  ZLESI0009
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZLESI0009 MESSAGE-ID ZLES.

TYPE-POOLS: ICON.
*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_CTE        ,
         CHAVE_CTE TYPE ZLEST0008-FILENAME,
*         CHAVE_CTE TYPE CHAR40,
       END   OF TY_CTE.

*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*
DATA: T_ZLEST0007     TYPE TABLE OF ZLEST0007,
      T_ZLEST0008     TYPE TABLE OF ZLEST0008,
      T_DIR_LOC_F     TYPE TABLE OF SDOKPATH,
      T_DIR_LOCAL     TYPE TABLE OF SDOKPATH,
      T_DIR_UNIX      TYPE TABLE OF EPSFILI,
      T_ZLEST0045     TYPE TABLE OF ZLEST0045,
      T_MENSAGENS     TYPE TABLE OF ZLEST0008,
      T_CTE           TYPE TABLE OF TY_CTE.

*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*
DATA: ST_ZLEST0007    TYPE ZLEST0007,
      ST_ZLEST0008    TYPE ZLEST0008,
      ST_MESS         TYPE ZLEST0008,
      ST_FILES_UNIX   TYPE EPSFILI,
      ST_FILES_DOC    TYPE SDOKPATH,
      ST_ZLEST0044    TYPE ZLEST0044,
      ST_ZLEST0045    TYPE ZLEST0045,
      ST_CTE          TYPE TY_CTE,

      WA_CONT         TYPE REF TO CL_GUI_CUSTOM_CONTAINER , " Objeto Container
      WA_ALV          TYPE REF TO CL_GUI_ALV_GRID, " Objeto ALV
      WA_LAYOUT       TYPE LVC_S_LAYO            . " Layout da Lista / Fim do


*----------------------------------------------------------------------*
* Variaveis                                                            *
*----------------------------------------------------------------------*
DATA: V_ERRO         TYPE C,
      V_MENSAGEM     TYPE BAPI_MSG,
      V_PREFIX_ENT   TYPE ZLEST0007-PREFIX,
      V_PREFIX_LOG   TYPE ZLEST0007-PREFIX,
      V_PREFIX_PROC  TYPE ZLEST0007-PREFIX,
      V_FILE_AUX     TYPE DRAW-FILEP,
      V_FILE_AUX2    TYPE DRAW-FILEP.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
CONSTANTS: C_X            TYPE C VALUE 'X',
           C_LOG(10)      TYPE C VALUE 'LOG',
           C_PROC(10)     TYPE C VALUE 'PROC',
           C_ENT(10)      TYPE C VALUE 'ENT',
           C_ALL(3)       TYPE C VALUE 'ALL',      " Confirmação de saída Ferroviário
           C_ASC(10)      TYPE C VALUE 'ASC',
           C_MASK_LOC(6)  TYPE C VALUE '*.xml',
           C_MASK_UNIX(6) TYPE C VALUE '*.xml',
           C_U            TYPE C VALUE 'U',
           C_W            TYPE C VALUE 'W',
           C_L            TYPE C VALUE 'L',
           C_E            TYPE C VALUE 'E',
           C_S            TYPE C VALUE 'S'.

*&---------------------------------------------------------------------*
*& Estrutura ALV
*&---------------------------------------------------------------------*

DATA: IT_FCAT    TYPE TABLE OF LVC_S_FCAT,
      S_VARIANT  TYPE DISVARIANT.


*----------------------------------------------------------------------*
* Tela de Seleção                                                      *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME TITLE TEXT-002.
PARAMETERS: P_INPUT(60) TYPE C MODIF ID FIL,
            P_PROC(60)  TYPE C MODIF ID FIL,
            P_LOG(60)   TYPE C MODIF ID FIL,
            P_CHKSO(1)  TYPE C NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B.

SELECTION-SCREEN BEGIN OF BLOCK E WITH FRAME TITLE TEXT-011.
PARAMETERS: P_KNOTE TYPE TVKN-KNOTE.
"P_MIRO TYPE C AS CHECKBOX default 'X'.
SELECTION-SCREEN END OF BLOCK E.

SELECTION-SCREEN BEGIN OF BLOCK D WITH FRAME TITLE TEXT-003.
PARAMETERS: P_VT_VI TYPE C AS CHECKBOX DEFAULT 'X'.
"P_MIRO TYPE C AS CHECKBOX default 'X'.
SELECTION-SCREEN END OF BLOCK D.

SELECTION-SCREEN BEGIN OF BLOCK C WITH FRAME TITLE TEXT-003.
PARAMETERS: R_LOCAL RADIOBUTTON GROUP 1
                    DEFAULT 'X'
                    USER-COMMAND SCR,
            R_UNIX  RADIOBUTTON GROUP 1.
SELECTION-SCREEN END OF BLOCK C.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'FIL'.
      SCREEN-INPUT = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF P_INPUT IS INITIAL AND P_PROC IS INITIAL AND P_LOG IS INITIAL
    OR T_ZLEST0007[] IS INITIAL.
    PERFORM VALIDA_TELA_SELECAO.
  ELSE.
    PERFORM BUSCA_FILE.
  ENDIF.

*----------------------------------------------------------------------*
* Start of Selection                                                   *
*----------------------------------------------------------------------*

START-OF-SELECTION.
  PERFORM LE_DIRETORIO.


*&---------------------------------------------------------------------*
*&      Form  VALIDA_TELA_SELECAO
*&---------------------------------------------------------------------*
FORM VALIDA_TELA_SELECAO .

  CLEAR V_ERRO.
  PERFORM SELECIONA_INTERFACE.

  IF V_ERRO IS INITIAL.
    PERFORM BUSCA_FILE.
  ELSE.
    CLEAR: P_INPUT,
           P_PROC,
           P_LOG,
           P_CHKSO.
  ENDIF.

ENDFORM.                    " VALIDA_TELA_SELECAO

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_INTERFACE
*&---------------------------------------------------------------------*
FORM SELECIONA_INTERFACE .

  DATA: V_LIFNR TYPE LFA1-LIFNR.

  CLEAR: P_CHKSO,
         T_ZLEST0007[].

  SELECT SINGLE *
    FROM ZLEST0007
    INTO ST_ZLEST0007
   WHERE ID_INTERFACE = SY-REPID
     AND ID_CTG = C_ENT
     AND PREFIX = C_ALL
     AND VLDE <= SY-DATUM
     AND VLATE >= SY-DATUM.

  IF SY-SUBRC IS INITIAL.
    APPEND ST_ZLEST0007 TO T_ZLEST0007.
  ELSE.
    MESSAGE W026 WITH C_ENT 'ZLES0009' INTO V_MENSAGEM.
    PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                         C_E
                                         '999'
                                         V_MENSAGEM.
    MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.
    V_ERRO = C_X.
    EXIT.
  ENDIF.

  IF V_ERRO IS INITIAL.
    SELECT SINGLE *
     FROM ZLEST0007
     INTO  ST_ZLEST0007
    WHERE ID_INTERFACE = SY-REPID
      AND ID_CTG = C_LOG
      AND VLDE <= SY-DATUM
      AND VLATE >= SY-DATUM.

    IF SY-SUBRC IS INITIAL.
      APPEND ST_ZLEST0007 TO T_ZLEST0007.
    ELSE.
      MESSAGE W026 WITH C_LOG 'ZLES0009' INTO V_MENSAGEM.
      PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                           C_E
                                           '999'
                                           V_MENSAGEM.
      MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.
      V_ERRO = C_X.
      EXIT.
    ENDIF.
  ENDIF.

  IF V_ERRO IS INITIAL.
    SELECT SINGLE *
      FROM ZLEST0007
      INTO  ST_ZLEST0007
     WHERE ID_INTERFACE = SY-REPID
       AND ID_CTG = C_PROC
       AND VLDE <= SY-DATUM
       AND VLATE >= SY-DATUM.

    IF SY-SUBRC IS INITIAL.
      APPEND ST_ZLEST0007 TO T_ZLEST0007.
    ELSE.
      MESSAGE W026 WITH C_PROC 'ZLES0009' INTO V_MENSAGEM.
      PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                           C_E
                                           '999'
                                           V_MENSAGEM.
      MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.
      V_ERRO = C_X.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECIONA_INTERFACE

*&---------------------------------------------------------------------*
*&      Form  BUSCA_FILE_LOCAL
*&---------------------------------------------------------------------*
FORM BUSCA_FILE .

  CHECK: P_CHKSO IS INITIAL
     OR  ( R_LOCAL = C_X  AND P_CHKSO = C_U )
     OR  ( R_UNIX  = C_X  AND P_CHKSO = C_W ).

  CLEAR: P_INPUT,
         P_LOG,
         P_PROC.

  PERFORM PREENCHE_CAMINHO USING: C_ENT  CHANGING P_INPUT,
                                  C_LOG  CHANGING P_LOG,
                                  C_PROC CHANGING P_PROC.

  IF P_INPUT IS INITIAL OR P_LOG  IS INITIAL OR P_PROC IS INITIAL.
    CLEAR: P_CHKSO.
    IF R_UNIX = C_X.
      MESSAGE W003 INTO V_MENSAGEM.
      PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                           C_E
                                           '999'
                                           V_MENSAGEM.
    ELSE.
      MESSAGE W004 INTO V_MENSAGEM.
      PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                           C_E
                                           '999'
                                           V_MENSAGEM.
    ENDIF.
    MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.
  ENDIF.

ENDFORM.                    " BUSCA_FILE_LOCAL



*&---------------------------------------------------------------------*
*&      Form  PREENCHE_CAMINHO
*&---------------------------------------------------------------------*
FORM PREENCHE_CAMINHO  USING    V_CATEG
                       CHANGING V_PATH.

  READ TABLE T_ZLEST0007 INTO ST_ZLEST0007 WITH KEY ID_CTG = V_CATEG.
  IF R_LOCAL = C_X.
    IF NOT ST_ZLEST0007-PATHWIN IS INITIAL.
      V_PATH = ST_ZLEST0007-PATHWIN.
      P_CHKSO = C_W.
    ENDIF.
  ELSE.
    IF NOT ST_ZLEST0007-PATHUNIX IS INITIAL.
      V_PATH = ST_ZLEST0007-PATHUNIX.
      P_CHKSO = C_U.
    ENDIF.
  ENDIF.

  CASE ST_ZLEST0007-ID_CTG.
    WHEN C_LOG.
      V_PREFIX_LOG = ST_ZLEST0007-PREFIX.
    WHEN C_PROC.
      V_PREFIX_PROC = ST_ZLEST0007-PREFIX.
    WHEN C_ENT.
      V_PREFIX_ENT = ST_ZLEST0007-PREFIX.
  ENDCASE.

  CLEAR ST_ZLEST0007.

ENDFORM.                    " PREENCHE_CAMINHO


*&---------------------------------------------------------------------*
*&      Form  ENVIA_MENSAGEM_PROCTO
*&---------------------------------------------------------------------*
FORM ENVIA_MENSAGEM_PROCTO  USING P_FILENAME
                                  P_MSGTYP
                                  P_MSGNR
                                  P_MSGV1.

  STATICS: LV_FILENAME  TYPE EPSFILNAM VALUE %_MAXCHAR,
           LV_VERSION   TYPE ZIDCTRL,
           LV_VCONT     TYPE NUMC10.

  IF LV_FILENAME <> P_FILENAME.

    CLEAR: ST_ZLEST0008,
           LV_VCONT.

    IF P_FILENAME = SY-REPID.

      SELECT MAX( IDCTRL ) MAX( CONT )
        INTO (ST_ZLEST0008-IDCTRL, LV_VCONT)
        FROM ZLEST0008
       WHERE FILENAME = P_FILENAME
        GROUP BY IDCTRL.
      ENDSELECT.

      IF SY-SUBRC IS INITIAL.
        IF LV_VCONT >= '9999999998'.
          LV_VERSION = ST_ZLEST0008-IDCTRL + 1.
          CLEAR LV_VCONT.
        ELSE.
          LV_VERSION = ST_ZLEST0008-IDCTRL.
        ENDIF.
      ELSE.
        LV_VERSION = ST_ZLEST0008-IDCTRL + 1.
      ENDIF.

    ELSE.
      READ TABLE T_ZLEST0008 INTO ST_ZLEST0008 WITH KEY FILENAME = P_FILENAME.
      LV_VERSION = ST_ZLEST0008-IDCTRL + 1.
    ENDIF.

    LV_FILENAME = P_FILENAME.
  ENDIF.

  ADD 1 TO LV_VCONT.
  ST_MESS-FILENAME = P_FILENAME.
  ST_MESS-IDCTRL   = LV_VERSION.
  ST_MESS-TCODE    = SY-TCODE.
  ST_MESS-CONT     = LV_VCONT.
  ST_MESS-DYNAME   = 'LES'.
  ST_MESS-MSGTYP   = P_MSGTYP.
  ST_MESS-MSGSPRA  = SY-LANGU.
  ST_MESS-MSGID    = 'FR'.
  ST_MESS-MSGNR    = P_MSGNR.
  ST_MESS-MSGV1    = P_MSGV1.
  ST_MESS-DATA     = SY-DATUM.
  ST_MESS-HORA     = SY-UZEIT.
  ST_MESS-USUARIO  = SY-UNAME.
  ST_MESS-LOTE     = SPACE.

  INSERT ZLEST0008 FROM ST_MESS.
  COMMIT WORK.
  CLEAR ST_MESS.

ENDFORM.                    " ENVIA_MENSAGEM_PROCTO


*&---------------------------------------------------------------------*
*&      Form  LE_DIRETORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LE_DIRETORIO .

  DATA: V_INDEX         TYPE SY-TABIX,
      V_MASK_UNIX     TYPE EPSFILNAM,
      V_MASK_LOCL(60) TYPE C,
      V_ERRO_LOG.

  CHECK: NOT P_INPUT IS INITIAL
     AND NOT P_PROC  IS INITIAL
     AND NOT P_LOG   IS INITIAL.

  CLEAR: V_ERRO_LOG.

* Processa arquivos de origem UNIX
  IF R_UNIX = C_X.

    CONCATENATE V_PREFIX_ENT C_MASK_UNIX INTO V_MASK_UNIX.

    CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
      EXPORTING
        DIR_NAME               = P_INPUT
        FILE_MASK              = V_MASK_UNIX
      TABLES
        DIR_LIST               = T_DIR_UNIX
      EXCEPTIONS
        INVALID_EPS_SUBDIR     = 1
        SAPGPARAM_FAILED       = 2
        BUILD_DIRECTORY_FAILED = 3
        NO_AUTHORIZATION       = 4
        READ_DIRECTORY_FAILED  = 5
        TOO_MANY_READ_ERRORS   = 6
        EMPTY_DIRECTORY_LIST   = 7
        OTHERS                 = 8.

    IF SY-SUBRC <> 0 OR T_DIR_UNIX[] IS INITIAL.

      MESSAGE W000
      WITH 'Diretório Unix: ' P_INPUT
           ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
           V_MASK_UNIX
      INTO V_MENSAGEM.

      PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                             C_E
                                             '999'
                                             V_MENSAGEM.

      MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.

      LEAVE LIST-PROCESSING.

    ELSE.

*     Bloqueia arquivos para processamento
      LOOP AT T_DIR_UNIX INTO ST_FILES_UNIX.

        V_INDEX = SY-TABIX.

        DELETE T_DIR_UNIX INDEX V_INDEX.

*        perform transfere_file using p_input
*                                     p_log
*                                     p_proc
*                                     c_u
*                                     st_files_unix-name
*                                     c_x.

        V_ERRO_LOG = C_X.

      ENDLOOP.

*     Consiste arquivo apto para processamento
      LOOP AT T_DIR_UNIX INTO ST_FILES_UNIX.
        PERFORM CARREGA_ARQ USING ST_FILES_UNIX-NAME C_U.
        IF NOT V_ERRO IS INITIAL.
          V_ERRO_LOG = C_X.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ELSEIF R_LOCAL = C_X.

    CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
      EXPORTING
        DIRECTORY  = P_INPUT
        FILTER     = C_MASK_LOC
      TABLES
        FILE_TABLE = T_DIR_LOC_F
        DIR_TABLE  = T_DIR_LOCAL
      EXCEPTIONS
        CNTL_ERROR = 1
        OTHERS     = 2.

    IF SY-SUBRC <> 0 OR T_DIR_LOC_F[] IS INITIAL.

      MESSAGE W000
         WITH 'Diretório Local: ' P_INPUT
              ' Inválido ou nenhum arquivo encontrado p/o prefixo: '
              C_MASK_LOC
          INTO V_MENSAGEM.
      PERFORM ENVIA_MENSAGEM_PROCTO  USING SY-REPID
                                           C_E
                                           '999'
                                           V_MENSAGEM.
      MESSAGE V_MENSAGEM TYPE C_S DISPLAY LIKE C_E.

      LEAVE LIST-PROCESSING.

    ELSE.
*      delete from zlest0045.
*      delete from zlest0044.

*     Consiste arquivo apto para processamento
      LOOP AT T_DIR_LOC_F INTO ST_FILES_DOC.
        CLEAR: V_FILE_AUX, V_FILE_AUX2.
        PERFORM CARREGA_ARQ USING ST_FILES_DOC-PATHNAME C_L.

        IF NOT V_ERRO IS INITIAL.

          V_ERRO_LOG = C_X.
        ELSE.
          CONCATENATE P_INPUT ST_FILES_DOC-PATHNAME INTO V_FILE_AUX.
          CONCATENATE P_PROC ST_FILES_DOC-PATHNAME INTO V_FILE_AUX2.
          CALL FUNCTION 'CV120_COPY_FILE'
            EXPORTING
              PF_SOURCE_FILE = V_FILE_AUX
              PF_TARGET_FILE = V_FILE_AUX2
            EXCEPTIONS
              ERROR          = 1
              OTHERS         = 2.
          IF SY-SUBRC EQ 0.
            CALL FUNCTION 'CV120_DELETE_FILE'
              EXPORTING
                PF_FILE = V_FILE_AUX
              EXCEPTIONS
                ERROR   = 1
                OTHERS  = 2.

          ENDIF.
        ENDIF.

      ENDLOOP.

      "Exibe as mesagens do cte
      SELECT *
        FROM ZLEST0008
        INTO TABLE T_MENSAGENS
         FOR ALL ENTRIES IN T_CTE
       WHERE FILENAME EQ T_CTE-CHAVE_CTE
         AND DATA     EQ SY-DATUM.

      SORT T_MENSAGENS BY MSGTYP.

      PERFORM F_ALV.

      CALL SCREEN 0100.


    ENDIF.

  ENDIF.

* Verifica se houve erro em algum processamento...
  IF V_ERRO_LOG IS INITIAL.
    MESSAGE S000(ZLES) WITH 'Arquivos processado!'
                       DISPLAY LIKE C_S.
  ELSE.
    MESSAGE S000(ZLES)
       WITH 'Existem arquivos/registros que não foram'
            'processados,'
            'verificar transação LOG ZLES0010'
       DISPLAY LIKE C_E.
  ENDIF.




ENDFORM.                    " LE_DIRETORIO


CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA : WA_EVENT TYPE REF TO LCL_EVENT_RECEIVER,
       WA_EVENT2 TYPE REF TO LCL_EVENT_RECEIVER.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS:ZM_HANDLE_HOTSPOT FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW_ID
                      E_COLUMN_ID
                      ES_ROW_NO                      ,

            ZM_HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
            IMPORTING
                E_OBJECT E_INTERACTIVE                   ,

            ZM_HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
            IMPORTING
                 E_UCOMM.
ENDCLASS.                    "lcl_event_receiver DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT.
    PERFORM Z_HANDLE_HOTSPOT USING    E_ROW_ID
                                      E_COLUMN_ID
                                      ES_ROW_NO.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD ZM_HANDLE_TOOLBAR.
*   Incluindo Botão ALV
    PERFORM Z_HANDLE_TOOLBAR USING E_OBJECT
                                   E_INTERACTIVE.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD ZM_HANDLE_USER_COMMAND.
*   User Command Botões Incluidos
    PERFORM Z_HANDLE_COMMAND USING E_UCOMM.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  CARREGA_ARQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_FILES_UNIX_NAME  text
*      -->P_C_U  text
*----------------------------------------------------------------------*
FORM CARREGA_ARQ  USING   V_FILE V_TIPO.
  DATA: V_SIZE       TYPE I,
        VL_ARQUIVO   TYPE STRING,
        ST_XML       TYPE ZEXML,
        ST_XML_FERRO TYPE ZEXML_FERRO,
        T_XML        TYPE TABLE OF ZEXML,
        T_XML_FERRO  TYPE TABLE OF ZEXML_FERRO.


  CLEAR : ST_XML_FERRO, ST_XML.
  REFRESH : T_XML_FERRO.

  CONCATENATE P_INPUT V_FILE INTO VL_ARQUIVO.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD
    EXPORTING
      FILENAME                = VL_ARQUIVO
      FILETYPE                = 'BIN'
    IMPORTING
      FILELENGTH              = V_SIZE
    CHANGING
      DATA_TAB                = T_XML
    EXCEPTIONS
      FILE_READ_ERROR         = 3
      INVALID_TYPE            = 4
      NO_BATCH                = 5
      GUI_REFUSE_FILETRANSFER = 7
      OTHERS                  = 99.

  READ TABLE T_XML INTO ST_XML INDEX 1.
  ST_XML_FERRO-XML   = ST_XML-XML.
  ST_XML_FERRO-VT_VI = P_VT_VI.
  "st_xml_ferro-MIRO  = p_miro.
  ST_XML_FERRO-MIRO  = ''.

  APPEND ST_XML_FERRO TO T_XML_FERRO.

  CALL FUNCTION 'Z_LES_INBOUND_PROC_XML_FERRO'
    TABLES
      XML = T_XML_FERRO.

  READ TABLE T_XML_FERRO INTO ST_XML_FERRO INDEX 1.


  ST_CTE-CHAVE_CTE = ST_XML_FERRO-CHAVE_CTE.

  APPEND ST_CTE TO T_CTE.

ENDFORM.                    " CARREGA_ARQ
*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Z_STATUS OUTPUT.
  SET PF-STATUS 'STATUS_ALV'.
ENDMODULE.                 " Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_MENSAGENS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Z_EXIBE_MENSAGENS OUTPUT.

  S_VARIANT-REPORT = SY-REPID.
  "IF wa_cont2 IS INITIAL.

  CREATE OBJECT WA_CONT
    EXPORTING
      CONTAINER_NAME              = 'CC_ALV'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.
  "ENDIF.

  IF WA_ALV IS INITIAL AND NOT  WA_CONT IS INITIAL.

    CREATE OBJECT WA_ALV
      EXPORTING
        I_PARENT          = WA_CONT
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.
  ENDIF.

  IF WA_EVENT IS INITIAL.

    CREATE OBJECT WA_EVENT2.
    SET HANDLER: WA_EVENT2->ZM_HANDLE_HOTSPOT FOR WA_ALV.
    SET HANDLER: WA_EVENT2->ZM_HANDLE_TOOLBAR FOR WA_ALV.
    SET HANDLER: WA_EVENT2->ZM_HANDLE_USER_COMMAND FOR WA_ALV.

  ENDIF.

  CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE                        = 'A'
      I_DEFAULT                     = 'X'
      IS_VARIANT                    = S_VARIANT      "is_layout = s_layout
      IS_LAYOUT                     = WA_LAYOUT
    CHANGING
      IT_OUTTAB                     = T_MENSAGENS
      IT_FIELDCATALOG               = IT_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CHECK NOT WA_ALV IS INITIAL.
ENDMODULE.                 " Z_EXIBE_MENSAGENS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM Z_HANDLE_COMMAND  USING    P_E_UCOMM.

ENDFORM.                    " Z_HANDLE_COMMAND
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

ENDFORM.                    " Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
FORM Z_HANDLE_TOOLBAR  USING   P_OBJECT  TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET
                                P_INTERACTIVE TYPE CHAR1 .

** Constants for button type
  CONSTANTS:
        C_BUTTON_NORMAL           TYPE I VALUE 0        ,
        C_MENU_AND_DEFAULT_BUTTON TYPE I VALUE 1        ,
        C_MENU                    TYPE I VALUE 2        ,
        C_SEPARATOR               TYPE I VALUE 3        ,
        C_RADIO_BUTTON            TYPE I VALUE 4        ,
        C_CHECKBOX                TYPE I VALUE 5        ,
        C_MENU_ENTRY              TYPE I VALUE 6        .

  DATA SL_TOOLBAR TYPE STB_BUTTON.

* Append Seperator
  MOVE C_SEPARATOR  TO SL_TOOLBAR-BUTN_TYPE.
  APPEND SL_TOOLBAR TO P_OBJECT->MT_TOOLBAR.
ENDFORM.                    " Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE Z_USER_COMMAND INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK' OR
         'CANC' OR
         'EXIT'  .
      LEAVE TO SCREEN 0 .
  ENDCASE.
ENDMODULE.                 " Z_USER_COMMAND  INPUT


*&---------------------------------------------------------------------*
*&      Form  f_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALV.
  REFRESH  IT_FCAT.
  PERFORM ALV_PREENCHE_CAT USING:
        'T_MENSAGENS' 'FILENAME' TEXT-009   '44'  ' '  ' ',"Chave Xml
        'T_MENSAGENS' 'CONT'     TEXT-010   '16'  ' '  ' ',"Cod Fornecedor
        'T_MENSAGENS' 'MSGTYP'   TEXT-005   '10'  ' '  ' ',
        'T_MENSAGENS' 'MSGV1'    TEXT-008   '40'  ' '  ' ',
        'T_MENSAGENS' 'LOTE'     TEXT-006   '13'  ' '  ' '."Dacte

ENDFORM  .                    "f_alv

*&---------------------------------------------------------------------*
*&      Form  alv_preenche_cat2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TABNAME  text
*      -->P_CAMPO    text
*      -->P_DESC     text
*      -->P_TAM      text
*      -->P_HOT      text
*      -->P_ZERO     text
*----------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT USING   P_TABNAME TYPE DD02D-TABNAME
                              P_CAMPO TYPE C
                              P_DESC  TYPE C
                              P_TAM   TYPE C
                              P_HOT   TYPE C
                              P_ZERO  TYPE C           .
  DATA: WL_FCAT TYPE LVC_S_FCAT.

  WL_FCAT-TABNAME   = P_TABNAME.
  WL_FCAT-FIELDNAME = P_CAMPO  .
  WL_FCAT-SCRTEXT_L = P_DESC   .
  WL_FCAT-SCRTEXT_M = P_DESC   .
  WL_FCAT-SCRTEXT_S = P_DESC   .
  WL_FCAT-HOTSPOT   = P_HOT    .
  WL_FCAT-NO_ZERO   = P_ZERO   .
  WL_FCAT-OUTPUTLEN = P_TAM    .

  APPEND WL_FCAT TO IT_FCAT.

ENDFORM.                    "alv_preenche_cat2
