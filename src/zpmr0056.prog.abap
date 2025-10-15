*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Autor......: Rogério Filipsick                                       *
* Data.......: 01/10/2019                                              *
* Descrição  : Lançamento de consumo de combustível frota própria.     *
* Transação..: ZPM0070                                                 *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
REPORT ZPMR0056.

*----------------------------------------------------------------------*
* Tabelas -------------------------------------------------------------*
*----------------------------------------------------------------------*
TABLES: ZPMT0024.

*----------------------------------------------------------------------*
* Tipos ---------------------------------------------------------------*
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_JOIN_1,
         FATURA        TYPE ZPMT0026-FATURA,
         COD_MATERIAL  TYPE ZPMT0026-COD_MATERIAL,
         DESC_MATERIAL TYPE ZPMT0026-DESC_MATERIAL,
         QTDE          TYPE ZPMT0026-QTDE,
         STATUS_APONT  TYPE ZPMT0026-STATUS_APONT,
         CENTRO        TYPE ZPMT0024-CENTRO,
         DT_CUPOM_FISC TYPE ZPMT0024-DT_CUPOM_FISC,
         HR_CUPOM_FISC TYPE ZPMT0024-HR_CUPOM_FISC,
         ORDEM         TYPE ZPMT0024-ORDEM,
         PLACA         TYPE ZPMT0024-PLACA,
         ODOMETRO      TYPE ZPMT0024-ODOMETRO,
         PEDIDO        TYPE ZPMT0024-PEDIDO,
       END OF TY_JOIN_1,


       BEGIN OF TY_JOIN_2,
         EQUNR       TYPE EQUZ-EQUNR,
         HEQUI       TYPE EQUZ-HEQUI,
         LICENSE_NUM TYPE FLEET-LICENSE_NUM,
         IWERK       TYPE EQUZ-IWERK,
       END OF TY_JOIN_2.

*----------------------------------------------------------------------*
* Tabelas internas ----------------------------------------------------*
*----------------------------------------------------------------------*
DATA:
  T_JOIN_1   TYPE TABLE OF TY_JOIN_1,
  T_JOIN_2   TYPE TABLE OF TY_JOIN_2,
  T_EQKT     TYPE TABLE OF EQKT,
  T_FIELDCAT TYPE LVC_T_FCAT,
  T_REPORT   TYPE TABLE OF ZPME0048,
  T_ZPMT0026 TYPE TABLE OF ZPMT0026.

*----------------------------------------------------------------------*
* Estruturas internas -------------------------------------------------*
*----------------------------------------------------------------------*
DATA: WA_TOOLBAR TYPE STB_BUTTON,
      WA_LAYOUT  TYPE LVC_S_LAYO.

*----------------------------------------------------------------------*
* Variaveis Globais ---------------------------------------------------*
*----------------------------------------------------------------------*
DATA:
  V_UCOMM      TYPE SY-UCOMM,
  V_CHAR_VALUE TYPE RIHIMRG-PYEAC.

*---------------------------------------------------------------------
*
* Classes locais (Definição)
*
*---------------------------------------------------------------------
DATA: CTL_ALV  TYPE REF TO CL_GUI_ALV_GRID.

CLASS LCL_GRID_EVENT DEFINITION.
* seção publica
  PUBLIC SECTION.
*...Barra de Ferramentas
    METHODS HANDLE_TOOLBAR
                FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
      IMPORTING E_OBJECT.
*...User Command
    METHODS HANDLE_COMMAND_GRID
                FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
      IMPORTING E_UCOMM.
ENDCLASS. "LCL_GRID_EVENT DEFINITION


*---------------------------------------------------------------------
*
* Classes locais (Implementação)
*
*---------------------------------------------------------------------
*
CLASS LCL_GRID_EVENT IMPLEMENTATION.
  METHOD HANDLE_TOOLBAR.
*...Barra de Ferramentas
    PERFORM F_TOOLBAR_GRID CHANGING E_OBJECT.
  ENDMETHOD. "handle_toolba

  METHOD HANDLE_COMMAND_GRID.
*...Rotinas do botão Z da barra de ferramentas do ALV
    V_UCOMM = E_UCOMM.
    PERFORM F_COMMAND USING E_UCOMM.
  ENDMETHOD. "handle_command_grid

ENDCLASS. "LCL_GRID_EVENT IMPLEMENTATION

DATA: LCL_ALV           TYPE REF TO CL_GUI_ALV_GRID,
      LCL_CONTAINER_ALV TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      LCL_EVENT         TYPE REF TO LCL_GRID_EVENT.


*----------------------------------------------------------------------*
* Tela de seleção -----------------------------------------------------*
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_CENTRO FOR ZPMT0024-CENTRO OBLIGATORY,
                S_FAT    FOR ZPMT0024-FATURA,
                S_PLACA  FOR ZPMT0024-PLACA.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* START-OF-SELECTION --------------------------------------------------*
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM F_SELECIONA_DADOS.

  PERFORM F_PROCESSAMENTO.

* Exibir a tela do ALV.
  CALL SCREEN 9000.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM F_SELECIONA_DADOS .

  SELECT A~FATURA A~COD_MATERIAL A~DESC_MATERIAL
         A~QTDE A~STATUS_APONT
         B~CENTRO B~DT_CUPOM_FISC B~HR_CUPOM_FISC
         B~ORDEM B~PLACA B~ODOMETRO B~PEDIDO
    FROM ZPMT0026       AS A
   INNER JOIN ZPMT0024  AS B ON A~FATURA     EQ B~FATURA AND
                                A~CUPOM_FISC EQ B~CUPOM_FISC
   INNER JOIN ZPMT0030  AS C ON C~FATURA EQ B~FATURA
    INTO TABLE T_JOIN_1
   WHERE A~FATURA IN S_FAT
     AND A~STATUS_APONT EQ SPACE
     AND B~PLACA  IN S_PLACA
     AND B~CENTRO IN S_CENTRO
     AND C~BELNR  NE SPACE.

  IF SY-SUBRC IS INITIAL.

    SELECT *
      FROM ZPMT0026
      INTO TABLE T_ZPMT0026
       FOR ALL ENTRIES IN T_JOIN_1
     WHERE FATURA = T_JOIN_1-FATURA.

    SELECT A~EQUNR A~HEQUI C~LICENSE_NUM A~IWERK
      FROM EQUZ AS A
     INNER JOIN EQUI  AS B ON B~EQUNR EQ A~EQUNR
     INNER JOIN FLEET AS C ON C~OBJNR EQ B~OBJNR
      INTO TABLE T_JOIN_2
       FOR ALL ENTRIES IN T_JOIN_1
     WHERE C~LICENSE_NUM EQ T_JOIN_1-PLACA
       AND A~IWERK EQ T_JOIN_1-CENTRO
       AND A~DATBI EQ '99991231'.

    IF SY-SUBRC IS INITIAL.

      SELECT *
        FROM EQKT
        INTO TABLE T_EQKT
         FOR ALL ENTRIES IN T_JOIN_2
       WHERE EQUNR = T_JOIN_2-EQUNR
         AND SPRAS = SY-LANGU.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSAMENTO
*&---------------------------------------------------------------------*
*       Processamento
*----------------------------------------------------------------------*
FORM F_PROCESSAMENTO .


  DATA: T_RETURN   TYPE TABLE OF BAPIRET2,
        T_DIMPT    TYPE TABLE OF DIIMPT,
        T_IMRG     TYPE TABLE OF IMRG,
        WA_DIMPT   TYPE DIIMPT,
        WA_IMRG    TYPE IMRG,
        V_ODOMETRO TYPE ZPMT0024-ODOMETRO,
        V_EQUNR    TYPE EQUZ-EQUNR.

  DELETE T_JOIN_1 WHERE COD_MATERIAL NE '000000000000000007'. "EXCLUIR DEPOIS

  LOOP AT T_JOIN_1 ASSIGNING FIELD-SYMBOL(<FS_JOIN_1>).

    APPEND INITIAL LINE TO T_REPORT ASSIGNING FIELD-SYMBOL(<FS_REPORT>).

    <FS_REPORT>-FATURA        = <FS_JOIN_1>-FATURA.
    <FS_REPORT>-CENTRO        = <FS_JOIN_1>-CENTRO.
    <FS_REPORT>-DT_CUPOM_FISC = <FS_JOIN_1>-DT_CUPOM_FISC.
    <FS_REPORT>-HR_CUPOM_FISC = <FS_JOIN_1>-HR_CUPOM_FISC.
    <FS_REPORT>-ORDEM         = <FS_JOIN_1>-ORDEM.
    <FS_REPORT>-PLACA         = <FS_JOIN_1>-PLACA.
    <FS_REPORT>-ODOMETRO      = <FS_JOIN_1>-ODOMETRO.
    <FS_REPORT>-COD_MATERIAL  = <FS_JOIN_1>-COD_MATERIAL.
    <FS_REPORT>-DESC_MATERIAL = <FS_JOIN_1>-DESC_MATERIAL.
    <FS_REPORT>-QTDE          = <FS_JOIN_1>-QTDE.
    <FS_REPORT>-PEDIDO        = <FS_JOIN_1>-PEDIDO.

    READ TABLE T_JOIN_2 ASSIGNING FIELD-SYMBOL(<FS_JOIN_2>)
                                      WITH KEY LICENSE_NUM = <FS_JOIN_1>-PLACA
                                               IWERK       = <FS_JOIN_1>-CENTRO.
    IF SY-SUBRC IS INITIAL.

      CLEAR: T_RETURN, T_DIMPT, WA_DIMPT , V_CHAR_VALUE.
      CALL FUNCTION 'GET_MEASURING_POINTS_4_EQUIPM'
        EXPORTING
          I_EQUNR    = <FS_JOIN_2>-EQUNR
        TABLES
          ET_RETURN1 = T_RETURN
          ET_DIIMPT  = T_DIMPT.

      IF T_DIMPT[] IS NOT INITIAL.
        DELETE T_DIMPT WHERE INACT NE SPACE.
        SORT T_DIMPT BY ERDAT DESCENDING.

        CLEAR: WA_DIMPT.
        READ TABLE T_DIMPT INTO WA_DIMPT WITH KEY ATNAM = 'ODOMETRO'
                                                  PSORT = 'ODOMETRO'.

        IF SY-SUBRC IS INITIAL.
          <FS_REPORT>-PONTO_ODOMETRO = WA_DIMPT-POINT.
        ELSE.
          CLEAR: WA_DIMPT.
          READ TABLE T_DIMPT INTO WA_DIMPT WITH KEY ATNAM = 'HORIMETRO'
                                                    PSORT = 'HORIMETRO'.
          IF SY-SUBRC IS INITIAL.
            <FS_REPORT>-PONTO_ODOMETRO = WA_DIMPT-POINT.
          ENDIF.
        ENDIF.

        READ TABLE T_DIMPT ASSIGNING FIELD-SYMBOL(<FS_DIMPT_COMBUSTIVEL>)
                                         WITH KEY ATNAM = 'COMBUSTIVEL'.
        IF SY-SUBRC IS INITIAL.
          <FS_REPORT>-PONTO_COMBUSTIVEL = <FS_DIMPT_COMBUSTIVEL>-POINT.
        ENDIF.

        IF <FS_JOIN_2>-EQUNR NE V_EQUNR.

          V_EQUNR    = <FS_JOIN_2>-EQUNR.
          V_ODOMETRO = <FS_REPORT>-ODOMETRO.

          IF WA_DIMPT IS NOT INITIAL.
            CLEAR: T_IMRG.
            SELECT *
              FROM IMRG
              INTO TABLE T_IMRG
              WHERE POINT = WA_DIMPT-POINT.

            IF SY-SUBRC IS INITIAL.
              SORT T_IMRG BY POINT ERDAT ERUHR DESCENDING.
              READ TABLE T_IMRG ASSIGNING FIELD-SYMBOL(<FS_IMRG>) INDEX 1.

              CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
                EXPORTING
                  CHAR_UNIT       = <FS_IMRG>-RECDU
                  DECIMALS        = 0
                  EXPONENT        = 0
                  FLTP_VALUE_SI   = <FS_IMRG>-CNTRR
                  INDICATOR_VALUE = ABAP_TRUE
                  MASC_SYMBOL     = ' '
                IMPORTING
                  CHAR_VALUE      = V_CHAR_VALUE.

              <FS_REPORT>-MEDICAO_ANTERIOR = V_CHAR_VALUE.
              CONDENSE <FS_REPORT>-MEDICAO_ANTERIOR NO-GAPS.

            ELSE.
              <FS_REPORT>-MEDICAO_ANTERIOR = V_ODOMETRO.
              CONDENSE <FS_REPORT>-MEDICAO_ANTERIOR NO-GAPS.
            ENDIF.
          ENDIF.

        ELSE.

          <FS_REPORT>-MEDICAO_ANTERIOR =  V_ODOMETRO.
          CONDENSE <FS_REPORT>-MEDICAO_ANTERIOR NO-GAPS.

        ENDIF.
      ENDIF.

      <FS_REPORT>-DIFERENCA = <FS_REPORT>-ODOMETRO - <FS_REPORT>-MEDICAO_ANTERIOR.
      CONDENSE <FS_REPORT>-DIFERENCA NO-GAPS.

* Odometro anterior
      V_ODOMETRO = <FS_REPORT>-ODOMETRO.

      SEARCH <FS_REPORT>-DIFERENCA FOR '-' AND MARK.
      IF SY-SUBRC IS INITIAL.
        <FS_REPORT>-LOG = 'Odometro negativo'.
        <FS_REPORT>-ICON = ICON_RED_LIGHT.
      ENDIF.

      READ TABLE T_EQKT ASSIGNING FIELD-SYMBOL(<FS_EQKT>)
                                      WITH KEY EQUNR = <FS_JOIN_2>-EQUNR.
      IF SY-SUBRC IS INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            INPUT  = <FS_JOIN_2>-EQUNR
          IMPORTING
            OUTPUT = <FS_REPORT>-EQUNR.

        <FS_REPORT>-EQKTX = <FS_EQKT>-EQKTX.
      ENDIF.

    ENDIF.
  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_COMMAND
*&---------------------------------------------------------------------*
FORM F_COMMAND  USING  E_UCOMM TYPE SY-UCOMM.

  CASE V_UCOMM.
    WHEN 'EXECUTAR'.
      PERFORM F_EXECUTAR.

* Refresh no Relatório
      PERFORM F_ATUALIZA_ALV.

    WHEN 'REFRESH'.
      PERFORM F_ATUALIZAR_VALORES.

* Refresh no Relatório
      PERFORM F_ATUALIZA_ALV.

  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_TOOLBAR_GRID
*&---------------------------------------------------------------------*
FORM F_TOOLBAR_GRID CHANGING P_OBJECT TYPE REF TO CL_ALV_EVENT_TOOLBAR_SET.

  CLEAR WA_TOOLBAR.
  MOVE: 'REFRESH' TO WA_TOOLBAR-FUNCTION ,
  ICON_REFRESH TO WA_TOOLBAR-ICON ,
  TEXT-004 TO WA_TOOLBAR-TEXT ,
  SPACE TO WA_TOOLBAR-DISABLED .
  APPEND WA_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

  CLEAR WA_TOOLBAR.
  MOVE: 'EXECUTAR' TO WA_TOOLBAR-FUNCTION ,
  ICON_SYSTEM_SAVE TO WA_TOOLBAR-ICON ,
  TEXT-002 TO WA_TOOLBAR-TEXT ,
  SPACE TO WA_TOOLBAR-DISABLED .
  APPEND WA_TOOLBAR TO P_OBJECT->MT_TOOLBAR.

  DELETE P_OBJECT->MT_TOOLBAR WHERE FUNCTION <> 'EXECUTAR'
                                AND FUNCTION <> 'REFRESH'
                                AND FUNCTION <> '&MB_EXPORT'
                                AND FUNCTION <> '&SORT_ASC'
                                AND FUNCTION <> '&SORT_DSC'
                                AND FUNCTION <> '&PRINT_BACK'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.


  SET PF-STATUS 'PF_0900'.
  SET TITLEBAR 'TITULO_0900'.

  IF T_REPORT[] IS INITIAL.
    MESSAGE S000(Z_LES) WITH TEXT-003 DISPLAY LIKE 'S'.
    LEAVE TO SCREEN 0.
  ENDIF.

  PERFORM F_ALV.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

  CASE SY-UCOMM.
    WHEN 'BACK'.
      FREE T_REPORT[].
      LEAVE TO SCREEN 0.
    WHEN 'CANC' OR 'EXIT'.
      LEAVE TO TRANSACTION 'ZPM0073'.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
FORM F_ALV .

  IF LCL_ALV IS BOUND.
    CALL METHOD LCL_ALV->FREE.
    CLEAR LCL_ALV.
  ENDIF.

  IF LCL_CONTAINER_ALV IS BOUND.
    CALL METHOD LCL_CONTAINER_ALV->FREE.
    CLEAR LCL_CONTAINER_ALV.
  ENDIF.

  CREATE OBJECT LCL_CONTAINER_ALV
    EXPORTING
      CONTAINER_NAME              = 'ALV'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  IF SY-SUBRC IS INITIAL.

    CREATE OBJECT LCL_ALV
      EXPORTING
        I_PARENT          = LCL_CONTAINER_ALV
      EXCEPTIONS
        ERROR_CNTL_CREATE = 1
        ERROR_CNTL_INIT   = 2
        ERROR_CNTL_LINK   = 3
        ERROR_DP_CREATE   = 4
        OTHERS            = 5.


    CREATE OBJECT LCL_EVENT.

* Incluir a referência a o evento TOOLBAR
    SET HANDLER LCL_EVENT->HANDLE_TOOLBAR FOR LCL_ALV.

* Incluir a referência a o evento USER_COMMAND
    SET HANDLER LCL_EVENT->HANDLE_COMMAND_GRID FOR LCL_ALV.

    REFRESH T_FIELDCAT.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        I_STRUCTURE_NAME       = 'ZPME0048'
      CHANGING
        CT_FIELDCAT            = T_FIELDCAT
      EXCEPTIONS
        INCONSISTENT_INTERFACE = 1
        PROGRAM_ERROR          = 2
        OTHERS                 = 3.

    LOOP AT T_FIELDCAT ASSIGNING FIELD-SYMBOL(<FS_FIELDCAT>).
      IF <FS_FIELDCAT>-FIELDNAME = 'ODOMETRO'.
        <FS_FIELDCAT>-EDIT = ABAP_TRUE.
      ENDIF.

      IF <FS_FIELDCAT>-FIELDNAME = 'FATURA'.
        DELETE T_FIELDCAT INDEX SY-TABIX.
      ENDIF.
    ENDLOOP.

    WA_LAYOUT-ZEBRA = ABAP_TRUE.       "Código Zebrado
    WA_LAYOUT-CWIDTH_OPT = ABAP_TRUE.  "Ajusta tamanho na coluna
    WA_LAYOUT-BOX_FNAME = ABAP_TRUE.   "

    SORT T_REPORT BY EQUNR DT_CUPOM_FISC HR_CUPOM_FISC DESCENDING.
    CALL METHOD LCL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_SAVE          = 'A'
        I_DEFAULT       = 'X'
        IS_LAYOUT       = WA_LAYOUT
      CHANGING
        IT_OUTTAB       = T_REPORT[]
        IT_FIELDCATALOG = T_FIELDCAT[].

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXECUTAR
*&---------------------------------------------------------------------*
FORM F_EXECUTAR .

  DATA: T_RETURN            TYPE TABLE OF BAPIRET2,
        T_DIMPT             TYPE TABLE OF DIIMPT,
        V_RECORDED_VALUE    TYPE RIMR0-RECDC,
        V_MEASUREMENT_POINT TYPE IMRG-POINT,
        V_MEASUREMENT_DOC   TYPE IMRG-MDOCM,
        V_STRING_FORM       TYPE IMRG-MDTXT,
        V_IMRG_USR          TYPE IMRG_USR,
        V_MSG_ODOMETRO      TYPE CHAR100,
        V_MSG_COMBUST       TYPE CHAR100.

  LOOP AT T_REPORT ASSIGNING FIELD-SYMBOL(<FS_REPORT>).

    READ TABLE T_ZPMT0026 ASSIGNING FIELD-SYMBOL(<FS_ZPMT0026>)
                                        WITH KEY FATURA = <FS_REPORT>-FATURA.

    IF <FS_REPORT>-ICON NE ICON_RED_LIGHT.

      CLEAR: V_RECORDED_VALUE, V_MEASUREMENT_POINT, V_MEASUREMENT_DOC,
             V_MSG_ODOMETRO, V_MSG_COMBUST.

      V_RECORDED_VALUE = <FS_REPORT>-ODOMETRO.
      V_MEASUREMENT_POINT = <FS_REPORT>-PONTO_ODOMETRO.
      CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001' "Lançamento da Posição do Odometro
        EXPORTING
          MEASUREMENT_POINT    = V_MEASUREMENT_POINT
          SECONDARY_INDEX      = ' '
          READING_DATE         = <FS_REPORT>-DT_CUPOM_FISC
          READING_TIME         = <FS_REPORT>-HR_CUPOM_FISC
          SHORT_TEXT           = ' '
          READER               = SY-UNAME
          ORIGIN_INDICATOR     = ' ' "'A'
          READING_AFTER_ACTION = ' '
          RECORDED_VALUE       = V_RECORDED_VALUE
          RECORDED_UNIT        = ' '
          DIFFERENCE_READING   = ' '
          CODE_VERSION         = ' '
          CHECK_CUSTOM_DUPREC  = ' '
          WITH_DIALOG_SCREEN   = ' '
          PREPARE_UPDATE       = 'X'
          COMMIT_WORK          = 'X'
          WAIT_AFTER_COMMIT    = 'X'
          CREATE_NOTIFICATION  = ' '
          NOTIFICATION_TYPE    = ' '
          NOTIFICATION_PRIO    = ' '
        IMPORTING
          MEASUREMENT_DOCUMENT = V_MEASUREMENT_DOC
        EXCEPTIONS
          NO_AUTHORITY         = 1
          POINT_NOT_FOUND      = 2
          INDEX_NOT_UNIQUE     = 3
          TYPE_NOT_FOUND       = 4
          POINT_LOCKED         = 5
          POINT_INACTIVE       = 6
          TIMESTAMP_IN_FUTURE  = 7
          TIMESTAMP_DUPREC     = 8
          UNIT_UNFIT           = 9
          VALUE_NOT_FLTP       = 10
          VALUE_OVERFLOW       = 11
          VALUE_UNFIT          = 12
          VALUE_MISSING        = 13
          CODE_NOT_FOUND       = 14
          NOTIF_TYPE_NOT_FOUND = 15
          NOTIF_PRIO_NOT_FOUND = 16
          NOTIF_GENER_PROBLEM  = 17
          UPDATE_FAILED        = 18
          INVALID_TIME         = 19
          INVALID_DATE         = 20
          OTHERS               = 21.

      IF SY-SUBRC NE 0.
        V_MSG_ODOMETRO = TEXT-008.
      ENDIF.

      CLEAR: V_RECORDED_VALUE, V_MEASUREMENT_POINT, V_MEASUREMENT_DOC,
             V_STRING_FORM, V_IMRG_USR.

      V_MEASUREMENT_POINT = <FS_REPORT>-PONTO_COMBUSTIVEL.
      V_RECORDED_VALUE   = <FS_REPORT>-QTDE.
      CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
        EXPORTING
          MEASUREMENT_POINT    = V_MEASUREMENT_POINT
          READING_DATE         = <FS_REPORT>-DT_CUPOM_FISC
          READING_TIME         = <FS_REPORT>-HR_CUPOM_FISC
          READER               = SY-UNAME
          ORIGIN_INDICATOR     = 'A'
          RECORDED_VALUE       = V_RECORDED_VALUE
          DIFFERENCE_READING   = ABAP_TRUE
          PREPARE_UPDATE       = 'X'
          COMMIT_WORK          = 'X'
          WAIT_AFTER_COMMIT    = 'X'
          SHORT_TEXT           = V_STRING_FORM
          USER_DATA            = V_IMRG_USR
        IMPORTING
          MEASUREMENT_DOCUMENT = V_MEASUREMENT_DOC
        EXCEPTIONS
          NO_AUTHORITY         = 1
          POINT_NOT_FOUND      = 2
          INDEX_NOT_UNIQUE     = 3
          TYPE_NOT_FOUND       = 4
          POINT_LOCKED         = 5
          POINT_INACTIVE       = 6
          TIMESTAMP_IN_FUTURE  = 7
          TIMESTAMP_DUPREC     = 8
          UNIT_UNFIT           = 9
          VALUE_NOT_FLTP       = 10
          VALUE_OVERFLOW       = 11
          VALUE_UNFIT          = 12
          VALUE_MISSING        = 13
          CODE_NOT_FOUND       = 14
          NOTIF_TYPE_NOT_FOUND = 15
          NOTIF_PRIO_NOT_FOUND = 16
          NOTIF_GENER_PROBLEM  = 17
          UPDATE_FAILED        = 18
          INVALID_TIME         = 19
          INVALID_DATE         = 20
          OTHERS               = 21.

      IF SY-SUBRC NE 0.
        V_MSG_COMBUST = TEXT-009.
      ENDIF.

      IF V_MSG_ODOMETRO IS NOT INITIAL OR
         V_MSG_COMBUST  IS NOT INITIAL.
        CONCATENATE V_MSG_ODOMETRO '|' V_MSG_ODOMETRO
        INTO <FS_REPORT>-LOG SEPARATED BY SPACE.
        <FS_REPORT>-ICON = ICON_RED_LIGHT.
      ELSE.
        LOOP AT T_ZPMT0026 ASSIGNING FIELD-SYMBOL(<FS_UPDATE_26>) WHERE FATURA = <FS_REPORT>-FATURA.
          <FS_UPDATE_26>-STATUS_APONT = ABAP_TRUE.
          MODIFY ZPMT0026 FROM <FS_UPDATE_26>.
        ENDLOOP.

        <FS_REPORT>-LOG = 'Sucesso'.
        <FS_REPORT>-ICON = ICON_GREEN_LIGHT.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ENDIF.


    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_ALV
*&---------------------------------------------------------------------*
FORM F_ATUALIZA_ALV .

  CALL METHOD LCL_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_SAVE          = 'A'
      I_DEFAULT       = 'X'
      IS_LAYOUT       = WA_LAYOUT
    CHANGING
      IT_OUTTAB       = T_REPORT[]
      IT_FIELDCATALOG = T_FIELDCAT[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZAR_VALORES
*&---------------------------------------------------------------------*
FORM F_ATUALIZAR_VALORES .

  LOOP AT T_REPORT ASSIGNING FIELD-SYMBOL(<FS_REPORT>).
    <FS_REPORT>-DIFERENCA = <FS_REPORT>-ODOMETRO - <FS_REPORT>-MEDICAO_ANTERIOR.
    CONDENSE <FS_REPORT>-DIFERENCA NO-GAPS.
  ENDLOOP.

ENDFORM.
