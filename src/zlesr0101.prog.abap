*&---------------------------------------------------------------------*
*& Report  ZLESR0101
*&
*&---------------------------------------------------------------------*
*& Consulta Situação do Transportados
*&
*&---------------------------------------------------------------------*
REPORT ZLESR0101.

DATA: CTL_CCCONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GS_LAYOUT        TYPE LVC_S_LAYO,
      GS_VARIANT       TYPE DISVARIANT,
      CTL_ALV_CONSULTA TYPE REF TO CL_GUI_ALV_GRID.

DATA: IT_FIELDCATALOG TYPE LVC_T_FCAT,
      WA_FIELDCATALOG TYPE LVC_S_FCAT.

DATA: IT_EXCLUDE_FCODE TYPE UI_FUNCTIONS,
      WA_EXCLUDE_FCODE LIKE LINE OF IT_EXCLUDE_FCODE.

DATA: IT_CONSULTA  TYPE ZLEST0135_T,
      IT_ZLEST0135 TYPE TABLE OF ZLEST0135 WITH HEADER LINE.

DATA: GS_SCROLL_COL TYPE LVC_S_COL,
      GS_SCROLL_ROW TYPE LVC_S_ROID.

SELECTION-SCREEN: BEGIN OF BLOCK B0 WITH FRAME TITLE TEXT-001.
PARAMETERS:     S_PROPR TYPE ZLEST0135-CD_TRANSPORTADOR,
                S_PLACA TYPE ZLEST0135-DS_PLACA.
SELECTION-SCREEN: END OF BLOCK B0.

SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS:     S_CNPJ  TYPE LFA1-STCD1,
                S_RNTRC TYPE LFA1-STCD3.
SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.

  IF S_CNPJ IS NOT INITIAL.

    CALL METHOD ZCL_WEBSERVICE_TIPCARD=>CONS_SITUACAO_TRANSPORTADOR
      EXPORTING
        I_PLACA          = S_PLACA
        I_PESQUISA_LIVRE = ABAP_TRUE
        I_CNPJ           = S_CNPJ
        I_RNTRC          = S_RNTRC
      RECEIVING
        E_CONSULTAS      = IT_CONSULTA
      EXCEPTIONS
        ERRO             = 1
        WEBSERVICE       = 2
        OTHERS           = 3.
  ELSE.

    CALL METHOD ZCL_WEBSERVICE_TIPCARD=>CONS_SITUACAO_TRANSPORTADOR
      EXPORTING
        I_PARTINER  = S_PROPR
        I_PLACA     = S_PLACA
      RECEIVING
        E_CONSULTAS = IT_CONSULTA
      EXCEPTIONS
        ERRO        = 1
        WEBSERVICE  = 2
        OTHERS      = 3.

  ENDIF.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    STOP.
  ENDIF.

  IF IT_CONSULTA IS NOT INITIAL.
    MOVE IT_CONSULTA[] TO IT_ZLEST0135[].
    CALL SCREEN 0100.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TL0100'.

  IF CTL_CCCONTAINER IS INITIAL.

    CREATE OBJECT CTL_CCCONTAINER
      EXPORTING
        CONTAINER_NAME = 'ALV_CONSULTA'.

    PERFORM FILL_IT_FIELDCATALOG.

*   Fill info for layout variant
    PERFORM FILL_GS_VARIANT.

*   Set layout parameters for ALV grid
    GS_LAYOUT-GRID_TITLE = TEXT-100.

    CREATE OBJECT CTL_ALV_CONSULTA
      EXPORTING
        I_PARENT = CTL_CCCONTAINER.

    CALL METHOD CTL_ALV_CONSULTA->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT            = GS_LAYOUT
        IS_VARIANT           = GS_VARIANT
        IT_TOOLBAR_EXCLUDING = IT_EXCLUDE_FCODE
        I_SAVE               = 'A'
      CHANGING
        IT_FIELDCATALOG      = IT_FIELDCATALOG
        IT_OUTTAB            = IT_ZLEST0135[].

  ENDIF.

  CALL METHOD CTL_ALV_CONSULTA->GET_SCROLL_INFO_VIA_ID
    IMPORTING
      ES_COL_INFO = GS_SCROLL_COL
      ES_ROW_NO   = GS_SCROLL_ROW.


ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FILL_IT_FIELDCATALOG .

  DATA: LC_COL_POS  TYPE LVC_COLPOS.

  FIELD-SYMBOLS: <FS_CAT> TYPE LVC_S_FCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      I_STRUCTURE_NAME = 'ZLEST0135'
    CHANGING
      CT_FIELDCAT      = IT_FIELDCATALOG.

  LOOP AT IT_FIELDCATALOG ASSIGNING <FS_CAT>.
    CASE <FS_CAT>-fieldname.
      WHEN 'CD_TRANSPORTADOR'.
        <FS_CAT>-OUTPUTLEN = 10.
      WHEN 'DS_PLACA'.
        <FS_CAT>-OUTPUTLEN = 10.
      WHEN 'TP_TRANSPORTADOR'.
        <FS_CAT>-OUTPUTLEN = 10.
      WHEN 'DT_VALIDADE_RNTRC'.
        <FS_CAT>-OUTPUTLEN = 10.
      WHEN 'CK_RNTRC_ATIVO'.
        <FS_CAT>-OUTPUTLEN = 10.
      WHEN 'CK_ETC_EQUIPARADO'.
        <FS_CAT>-OUTPUTLEN = 10.
      WHEN 'QT_EIXOS'.
        <FS_CAT>-OUTPUTLEN = 08.
      WHEN 'NR_TAG'.
        <FS_CAT>-OUTPUTLEN = 10.
      WHEN 'DS_RAZAO_SOCIAL'.
        <FS_CAT>-OUTPUTLEN = 20.
      WHEN 'DS_VEICULO'.
        <FS_CAT>-OUTPUTLEN = 20.
      WHEN 'DS_PROPRIETARIO'.
        <FS_CAT>-OUTPUTLEN = 20.
      WHEN 'DS_MSG_TRANSPORTADOR'.
        <FS_CAT>-OUTPUTLEN = 20.
      WHEN 'DS_MSG_VEICULO'.
        <FS_CAT>-OUTPUTLEN = 20.
      WHEN 'DT_ATUALIZACAO'.
        <FS_CAT>-OUTPUTLEN = 10.
      WHEN 'HR_ATUALIZACAO'.
        <FS_CAT>-OUTPUTLEN = 10.
      WHEN 'CK_SEM_PARAR'.
        <FS_CAT>-OUTPUTLEN = 08.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG


*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM FILL_GS_VARIANT .

  GS_VARIANT-REPORT      = SY-REPID.
  GS_VARIANT-HANDLE      = '0100'.
  GS_VARIANT-LOG_GROUP   = ABAP_FALSE.
  GS_VARIANT-USERNAME    = ABAP_FALSE.
  GS_VARIANT-VARIANT     = ABAP_FALSE.
  GS_VARIANT-TEXT        = ABAP_FALSE.
  GS_VARIANT-DEPENDVARS  = ABAP_FALSE.

ENDFORM.                    " FILL_GS_VARIANT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
