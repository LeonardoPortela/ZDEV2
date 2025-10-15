*&---------------------------------------------------------------------*
*& Report  ZMMR144
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZMMR144.
TABLES : ZMMT0069.

INTERFACE: ZIF_DATA_READER.
  TYPES:     BEGIN OF TY_SAIDA,
               NUREQ      TYPE ZMMT0069_LOG-NUREQ,
               DATA       TYPE ZMMT0069_LOG-DATA,
               HORA       TYPE ZMMT0069_LOG-HORA,
               WERKS      TYPE ZMMT0069_LOG-WERKS,
               VKORG      TYPE ZMMT0069_LOG-VKORG,
               VTWEG      TYPE ZMMT0069_LOG-VTWEG,
               HOMOLOG    TYPE ZMMT0069_LOG-HOMOLOG,
               EXPANSAO   TYPE ZMMT0069_LOG-EXPANSAO,
               MATNR      TYPE ZMMT0069_LOG-MATNR,
               MAKTX      TYPE ZMMT0069_LOG-MAKTX,
               MATKL      TYPE ZMMT0069_LOG-MATKL,
               MEINS      TYPE ZMMT0069_LOG-MEINS,
               MATNRG     TYPE ZMMT0069_LOG-MATNRG,
               CRIADO     TYPE ZMMT0069_LOG-CRIADO,
               DT_CRIACAO TYPE ZMMT0069_LOG-DT_CRIACAO,
               HR_CRIACAO TYPE ZMMT0069_LOG-HR_CRIACAO,
               SOLIC      TYPE ZMMT0069_LOG-SOLIC,
               USNAM      TYPE ZMMT0069_LOG-USNAM,
               DT_ENTRADA TYPE ZMMT0069_LOG-DT_ENTRADA,
               HR_ENTRADA TYPE ZMMT0069_LOG-HR_ENTRADA,
               ELIMINADO  TYPE ZMMT0069_LOG-ELIMINADO,
               OBSERV     TYPE ZMMT0069_LOG-OBSERV,
               RECUSA     TYPE ZMMT0069_LOG-RECUSA,
               GER_WF     TYPE ZMMT0069_LOG-GER_WF,
               MTART      TYPE ZMMT0069_LOG-MTART,
               MBRSH      TYPE ZMMT0069_LOG-MBRSH,
               DT_HOMOL   TYPE ZMMT0069_LOG-DT_HOMOL,
               HR_HOMOL   TYPE ZMMT0069_LOG-HR_HOMOL,
               HOMOLOGA   TYPE ZMMT0069_LOG-HOMOLOGA,
               DT_HOMOLF  TYPE ZMMT0069_LOG-DT_HOMOLF,
               HR_HOMOLF  TYPE ZMMT0069_LOG-HR_HOMOLF,
               EPI_CA     TYPE ZMMT0069_LOG-EPI_CA,
               EPI_PERI   TYPE ZMMT0069_LOG-EPI_PERI,
               EPI_VAL    TYPE ZMMT0069_LOG-EPI_VAL,
               ACAO       TYPE ZMMT0069_LOG-ACAO,
             END OF TY_SAIDA.
  METHODS: READ_DATA, GENERATE_GRID.
ENDINTERFACE.                    "ZIF_DATA_READER


DATA: IT_SAIDA TYPE TABLE OF ZIF_DATA_READER=>TY_SAIDA,
      WA_SAIDA TYPE ZIF_DATA_READER=>TY_SAIDA.

DATA: WA_LAYOUT       TYPE LVC_S_LAYO,
      WA_STABLE       TYPE LVC_S_STBL,
      IT_FIELDCATALOG TYPE LVC_T_FCAT,
      WA_FIELDCATALOG TYPE LVC_S_FCAT.

DATA:
  DG_SPLITTER_1    TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
  DG_PARENT_1      TYPE REF TO CL_GUI_CONTAINER,
  DG_SPLITTER_2    TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
  DG_PARENT_2      TYPE REF TO CL_GUI_CONTAINER,
  DG_PARENT_2A     TYPE REF TO CL_GUI_CONTAINER,
  DG_PARENT_ALV    TYPE REF TO CL_GUI_CONTAINER,
  GS_LAYOUT        TYPE LVC_S_LAYO,
  GS_VARIANT       TYPE DISVARIANT,
  IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
  WA_EXCLUDE_FCODE LIKE LINE OF IT_EXCLUDE_FCODE,
  DG_DYNDOC_ID     TYPE REF TO CL_DD_DOCUMENT.

DATA: G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CTL_ALV            TYPE REF TO CL_GUI_ALV_GRID.

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: S_WERKS  FOR ZMMT0069-WERKS NO-EXTENSION NO INTERVALS OBLIGATORY,
                S_NUREQ  FOR ZMMT0069-NUREQ,
                S_SOLIC  FOR ZMMT0069-SOLIC.

SELECTION-SCREEN: END OF BLOCK B1.



*----------------------------------------------------------------------*
*       CLASS LCL_SELECIONA_DADOS DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_SELECIONA_DADOS DEFINITION.

  PUBLIC SECTION.
    INTERFACES ZIF_DATA_READER.

  PRIVATE SECTION.

*VARIÁVEIS
    DATA: N_MONTHS TYPE I,
          V_TEXT   TYPE CHAR45.

*TABELAS INTERNAS*
    DATA: IT_MARD   TYPE TABLE OF MARD,
          IT_FCAT   TYPE TABLE OF SLIS_FIELDCAT_ALV,
          IT_EVENTO TYPE          SLIS_T_EVENT.

*TABELAS WORK-ÁREAS*
    DATA: WA_MARD   TYPE MARD,
          WA_FCAT   TYPE SLIS_FIELDCAT_ALV,
          WA_EVENTO TYPE SLIS_ALV_EVENT,
          WA_LAYOUT TYPE SLIS_LAYOUT_ALV,
          WA_SAIDA  TYPE ZIF_DATA_READER=>TY_SAIDA.
ENDCLASS.                    "LCL_SELECIONA_DADOS DEFINITION

CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      ZM_HANDLE_HOTSPOT_REPORT
                    FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

ENDCLASS.                    "lcl_event_compras DEFINITION

CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD: ZM_HANDLE_HOTSPOT_REPORT.

    PERFORM USER_COMMAND_0100 USING E_ROW_ID E_COLUMN_ID ES_ROW_NO.

  ENDMETHOD.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS LCL_MONTA_FIELDCAT DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_MONTA_FIELDCAT DEFINITION.

  PUBLIC SECTION.
    METHODS: SET_FIELDCAT IMPORTING I_CAMPO  TYPE CHAR30
                                    I_DESC_L TYPE CHAR40
                                    I_TAM    TYPE NUM6.

    METHODS: GET_FIELDCAT EXPORTING E_CAMPO  TYPE CHAR30
                                    E_DESC_L TYPE CHAR40
                                    E_TAM    TYPE NUM6.
    DATA: AT_CAMPO  TYPE CHAR30,
          AT_DESC_L TYPE CHAR40,
          AT_TAM    TYPE NUM6.

ENDCLASS.                    "LCL_MONTA_FIELDCAT DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_SELECIONA_DADOS IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_SELECIONA_DADOS IMPLEMENTATION.
  METHOD ZIF_DATA_READER~READ_DATA.

    SELECT *
      FROM ZMMT0069_LOG
      INTO CORRESPONDING FIELDS OF TABLE IT_SAIDA
      WHERE WERKS IN S_WERKS
      AND   NUREQ IN S_NUREQ
      AND   SOLIC IN S_SOLIC.

  ENDMETHOD.                    "ZIF_DATA_READER~READ_DATA


  METHOD ZIF_DATA_READER~GENERATE_GRID.
    DATA: R_MONTA_FIELDCAT TYPE REF TO LCL_MONTA_FIELDCAT.

    CREATE OBJECT R_MONTA_FIELDCAT.
    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'NUREQ'
                                              I_DESC_L  = 'Solicitação'
                                              I_TAM   = 8 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'DATA'
                                              I_DESC_L  = 'Data'
                                              I_TAM   = 8 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'HORA'
                                              I_DESC_L  = 'Hora'
                                              I_TAM   = 8 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'ACAO'
                                               I_DESC_L  = 'Ação'
                                               I_TAM   = 20 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'VKORG'
                                              I_DESC_L  = 'Org.Vendas'
                                              I_TAM   = 8 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'VTWEG'
                                                 I_DESC_L  = 'Canal'
                                                 I_TAM   = 8 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'HOMOLOG'
                                             I_DESC_L  = 'Status Homol.'
                                             I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'MATNR'
                                             I_DESC_L  = 'Material'
                                             I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'MBRSH'
                         I_DESC_L  = 'Setor Ind.'
                         I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'MTART'
                         I_DESC_L  = 'Tp.Material'
                         I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'MATNRG'
                                             I_DESC_L  = 'Codigo Gerado'
                                             I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'MAKTX'
                                             I_DESC_L  = 'Texto Breve'
                                             I_TAM   = 30 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'MATKL'
                                         I_DESC_L  = 'Grupo'
                                         I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'MEINS'
                                         I_DESC_L  = 'Uni.Med'
                                         I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'CRIADO'
                                     I_DESC_L  = 'Criado'
                                     I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'DT_CRIACAO'
                                     I_DESC_L  = 'Dt.Criado'
                                     I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'HR_CRIACAO'
                                     I_DESC_L  = 'Hr.Criado'
                                     I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'SOLIC'
                                 I_DESC_L  = 'Solicitante'
                                 I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'DT_ENTRADA'
                                 I_DESC_L  = 'Dt.Entrada'
                                 I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'HR_ENTRADA'
                                 I_DESC_L  = 'Hr.Entrada'
                                 I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'ELIMINADO'
                                 I_DESC_L  = 'Eliminado'
                                 I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'OBSERV'
                                 I_DESC_L  = 'Observação'
                                 I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'RECUSA'
                                 I_DESC_L  = 'Recusa'
                                 I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'GER_WF'
                             I_DESC_L  = 'Ger.WF'
                             I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'DT_HOMOL'
                             I_DESC_L  = 'Dt.Homol'
                             I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'HR_HOMOL'
                             I_DESC_L  = 'Hr.Homol'
                             I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'DT_HOMOLF'
                             I_DESC_L  = 'Dt.Homol.EPI'
                             I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    R_MONTA_FIELDCAT->SET_FIELDCAT( EXPORTING I_CAMPO = 'HR_HOMOLF'
                             I_DESC_L  = 'Hr.Homol.EPI'
                             I_TAM   = 10 ).

    R_MONTA_FIELDCAT->GET_FIELDCAT( IMPORTING E_DESC_L = ME->WA_FCAT-SELTEXT_L
                                              E_CAMPO  = ME->WA_FCAT-FIELDNAME
                                              E_TAM    = ME->WA_FCAT-OUTPUTLEN ).
    APPEND WA_FCAT TO IT_FCAT.

    CLEAR WA_EVENTO.
    WA_EVENTO-NAME = SLIS_EV_USER_COMMAND.
    WA_EVENTO-FORM = 'USER_COMMAND'.
    APPEND WA_EVENTO TO IT_EVENTO.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        I_CALLBACK_PROGRAM = SY-REPID
        IS_LAYOUT          = WA_LAYOUT
        IT_FIELDCAT        = IT_FCAT
        IT_EVENTS          = IT_EVENTO
        I_SAVE             = 'X'
      TABLES
        T_OUTTAB           = IT_SAIDA
      EXCEPTIONS
        PROGRAM_ERROR      = 1
        OTHERS             = 2.
  ENDMETHOD.                    "ZIF_DATA_READER~GENERATE_GRID
ENDCLASS.                    "LCL_SELECIONA_DADOS IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_MONTA_FIELDCAT IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_MONTA_FIELDCAT IMPLEMENTATION.
  METHOD SET_FIELDCAT.
    ME->AT_CAMPO  = I_CAMPO.
    ME->AT_DESC_L = I_DESC_L.
    ME->AT_TAM    = I_TAM.
  ENDMETHOD.                    "SET_FIELDCAT

  METHOD GET_FIELDCAT.
    E_CAMPO  = ME->AT_CAMPO.
    E_DESC_L = ME->AT_DESC_L.
    E_TAM    = ME->AT_TAM.
  ENDMETHOD.                    "GET_FIELDCAT
ENDCLASS.                    "LCL_MONTA_FIELDCAT IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM USER_COMMAND USING UCOMM    LIKE SY-UCOMM
                        SELFIELD TYPE KKBLO_SELFIELD.



  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX SELFIELD-TABINDEX.




ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100.
  LEAVE TO SCREEN 0.
ENDMODULE.


FORM IMPRIMI_ALV_RES.
  REFRESH IT_FIELDCATALOG.
  "CLEAR: IT_FIELDCATALOG[].
  PERFORM PREENCHE_CAT USING:
       'RSNUM'           'Reserva'                         '14'     ''      'X' '' '',
       'BDMNG'           'Quantidade'                      '15'     ''       '' '' ''  .
  CALL SCREEN 0100.
ENDFORM.


FORM PREENCHE_CAT USING VALUE(P_CAMPO)
                        VALUE(P_DESC)
                        VALUE(P_TAM)
                        VALUE(P_ZERO)
                        VALUE(P_HOT)
                        VALUE(P_SUM)
                        VALUE(P_JUST).

  WA_FIELDCATALOG-FIELDNAME   = P_CAMPO.
  WA_FIELDCATALOG-COLTEXT     = P_DESC.
  WA_FIELDCATALOG-SCRTEXT_L   = P_DESC.
  WA_FIELDCATALOG-SCRTEXT_M   = P_DESC.
  WA_FIELDCATALOG-SCRTEXT_S   = P_DESC.


  WA_FIELDCATALOG-OUTPUTLEN   = P_TAM.
  WA_FIELDCATALOG-HOTSPOT     = P_HOT.
  WA_FIELDCATALOG-NO_ZERO     = P_ZERO.
  WA_FIELDCATALOG-DO_SUM      = P_SUM.
  WA_FIELDCATALOG-JUST        = P_JUST.


  APPEND WA_FIELDCATALOG TO IT_FIELDCATALOG.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  PERFORM ALV_CADASTRO.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.


FORM USER_COMMAND_0100 USING E_ROW_ID TYPE LVC_S_ROW
                             P_E_COLUMN_ID TYPE  LVC_S_COL
                             P_ES_ROW_NO   TYPE  LVC_S_ROID.




ENDFORM.

FORM ALV_CADASTRO.

  IF G_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5.

    IF SY-SUBRC <> 0.
      MESSAGE A000(TREE_CONTROL_MSG).
    ENDIF.

    CREATE OBJECT DG_SPLITTER_1
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_1.

    CALL METHOD DG_SPLITTER_1->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_ALV.

    CREATE OBJECT DG_SPLITTER_2
      EXPORTING
        PARENT  = DG_PARENT_1
        ROWS    = 1
        COLUMNS = 0.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 0
      RECEIVING
        CONTAINER = DG_PARENT_2.

    CALL METHOD DG_SPLITTER_2->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_2A.

    CALL METHOD DG_SPLITTER_1->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 0.

    CALL METHOD DG_SPLITTER_2->SET_COLUMN_WIDTH
      EXPORTING
        ID    = 1
        WIDTH = 0.

    GS_LAYOUT-SEL_MODE   = 'A'.
    GS_LAYOUT-STYLEFNAME = 'CELLSTYLES'.

    CREATE OBJECT CTL_ALV
      EXPORTING
        I_PARENT = DG_PARENT_ALV.

    SET HANDLER: LCL_EVENT_RECEIVER=>ZM_HANDLE_HOTSPOT_REPORT FOR CTL_ALV.

    CALL METHOD CTL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = GS_LAYOUT
        IS_VARIANT      = GS_VARIANT
        "IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG
        IT_OUTTAB       = IT_SAIDA.

    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.
  ELSE.
    CALL METHOD CTL_ALV->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.
  ENDIF.
ENDFORM.


START-OF-SELECTION.
  DATA: R_READ_DATA     TYPE REF TO LCL_SELECIONA_DADOS,
        R_GENERATE_GRID TYPE REF TO LCL_SELECIONA_DADOS.

  CREATE OBJECT R_READ_DATA.
  R_READ_DATA->ZIF_DATA_READER~READ_DATA( ).

  CREATE OBJECT R_GENERATE_GRID.
  R_GENERATE_GRID->ZIF_DATA_READER~GENERATE_GRID( ).

END-OF-SELECTION.
