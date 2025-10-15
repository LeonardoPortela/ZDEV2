*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Ricardo Furst                                           &*
*& Data.....: 26/08/2009                                              &*
*& Descrição: Relatório de Solicitação de Viagem                      &*
*& Transação: ZFITV002                                                   &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*

REPORT  ZFITV002.
INCLUDE <ICONS>.
TABLES: ZFITV_PED_VIAG.
*----------------------------------------------------------------------*
*                  TIPOS
*----------------------------------------------------------------------*
TYPES: BEGIN OF TY_FTPT_REQ_HEAD.
        INCLUDE STRUCTURE FTPT_REQ_HEAD.
TYPES: END OF TY_FTPT_REQ_HEAD.



TYPES: BEGIN OF TY_FTPT_REQ_ADVANCE.
        INCLUDE STRUCTURE FTPT_REQ_ADVANCE.
TYPES: END OF TY_FTPT_REQ_ADVANCE.


TYPES: BEGIN OF TY_ZFITV_PED_VIAG.
        INCLUDE STRUCTURE ZFITV_PED_VIAG.
TYPES: END OF TY_ZFITV_PED_VIAG.


TYPES: BEGIN OF TY_SAIDA,
       ICON_STAT(45)  TYPE C,
       PERNR          TYPE FTPT_REQ_HEAD-PERNR,
       ENAME          TYPE PA0001-ENAME,
       REINR          TYPE FTPT_REQ_HEAD-REINR,
       DATE_BEG       TYPE FTPT_REQ_HEAD-DATE_BEG,
       DATE_END       TYPE FTPT_REQ_HEAD-DATE_END,
       LOCATION_END   TYPE FTPT_REQ_HEAD-LOCATION_END,
       REQUEST_REASON TYPE FTPT_REQ_HEAD-REQUEST_REASON,
       VORSC          TYPE FTPT_REQ_ADVANCE-VORSC,
       DATVS          TYPE FTPT_REQ_ADVANCE-DATVS,
       UNAME          TYPE FTPT_REQ_HEAD-UNAME,
       DATES(10)      TYPE C,
       TIMES(8)       TYPE C,
       UNAME2         TYPE ZFITV_PED_VIAG-UNAME,
       DATES2(10)     TYPE C,
       TIMES2(8)      TYPE C,
       OBS            TYPE ZFITV_PED_VIAG-OBS,
       ICONE(45)      TYPE C,
       STATUS(45)     TYPE C,
       STATUS1        TYPE ZFITV_PED_VIAG-STATUS,
       STATUS2        TYPE ZFITV_PED_VIAG-STATUS_AD,
       EMPRESA(50),
       FILIAL(50),
       CCUSTO(50),
       SEL,
       END OF TY_SAIDA.

DATA: BEGIN OF T_PA1 OCCURS 0,
        PERNR LIKE PA0001-PERNR,
        BUKRS LIKE PA0001-BUKRS,
        GSBER LIKE PA0001-GSBER,
        KOSTL LIKE PA0001-KOSTL,
        BUTXT LIKE T001-BUTXT,
        GTEXT LIKE TGSBT-GTEXT,
        KTEXT LIKE CSKT-KTEXT,
      END OF T_PA1.
* -------------------------------------------------------------------- *
*    TABELAS                                                           *
* -------------------------------------------------------------------- *
DATA: T_FTPT_REQ_HEAD    TYPE STANDARD TABLE OF TY_FTPT_REQ_HEAD,
      T_FTPT_REQ_ADVANCE TYPE STANDARD TABLE OF TY_FTPT_REQ_ADVANCE,
      T_ZFITV_PED_VIAG   TYPE STANDARD TABLE OF TY_ZFITV_PED_VIAG,
      T_SAIDA            TYPE STANDARD TABLE OF TY_SAIDA.

DATA: BEGIN OF T_PERIO OCCURS 100.  "Table of periods
        INCLUDE STRUCTURE PTP42.                            "XOWK000899
DATA: END OF T_PERIO.
* -------------------------------------------------------------------- *
*    WORK AREA                                                         *
* -------------------------------------------------------------------- *
DATA: W_FTPT_REQ_HEAD    LIKE LINE OF T_FTPT_REQ_HEAD,
      W_FTPT_REQ_ADVANCE LIKE LINE OF T_FTPT_REQ_ADVANCE,
      W_ZFITV_PED_VIAG   LIKE LINE OF T_ZFITV_PED_VIAG,
      W_SAIDA            LIKE LINE OF T_SAIDA.

* -------------------------------------------------------------------- *
*    VARIAVEIS                                                         *
* -------------------------------------------------------------------- *
DATA: OKCODE   TYPE SY-UCOMM,
      V_OKCODE TYPE OKCODE,
      W_PERNR_LOW(8),
      W_REINR_LOW(10),
      W_DATES_LOW(10),
      W_DATES_HIGH(10),
      L_SELLINE LIKE SY-STEPL,
      V_VALID TYPE C.
* -------------------------------------------------------------------- *
*    RANGES                                                            *
* -------------------------------------------------------------------- *
RANGES: R_PERNR FOR ZFITV_PED_VIAG-PERNR,
        R_REINR FOR ZFITV_PED_VIAG-REINR.

RANGES: R_VIAGEM FOR ZFITV_PED_VIAG-STATUS,
        R_ADIANTAMENTO FOR ZFITV_PED_VIAG-STATUS_AD.
*&---------------------------------------------------------------------*
*& Definições para ALV
*&---------------------------------------------------------------------*
DATA: IT_FIELDCATALOG     TYPE LVC_T_FCAT,
      FC                  TYPE LINE OF LVC_T_FCAT,
      IT_SORT             TYPE LVC_T_SORT,
      T_SORT              TYPE LINE OF LVC_T_SORT,
      I                   TYPE LVC_S_FCAT-COL_POS,
      O_GRID              TYPE REF TO CL_GUI_ALV_GRID,
      O_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_DOCKING_CONTAINER,
*      O_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      V_REPID             LIKE SY-REPID,
      IT_LAYOUT           TYPE LVC_S_LAYO,
      CL_ALV              TYPE REF TO CL_GUI_ALV_GRID.

TYPES: BEGIN OF TY_ROW_NO.
        INCLUDE STRUCTURE LVC_S_ROID.
TYPES: END OF TY_ROW_NO.

TYPES: BEGIN OF TY_INDEX_ROWS.
        INCLUDE STRUCTURE LVC_S_ROW.
TYPES: END OF TY_INDEX_ROWS.

DATA: INDEX_ROWS TYPE STANDARD TABLE OF TY_INDEX_ROWS,
      ROW_NO TYPE STANDARD TABLE OF TY_ROW_NO.
* -------------------------------------------------------------------- *
*    CLASSES                                                           *
* -------------------------------------------------------------------- *
CLASS LCL_EVENT_HANDLER DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
                      IMPORTING E_ROW E_COLUMN.

    CLASS-METHODS:
      ON_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
                      IMPORTING E_UCOMM.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.

  METHOD ON_USER_COMMAND.

  ENDMETHOD.                    "ON_USER_COMMAND

  METHOD ON_DOUBLE_CLICK.

    CASE E_COLUMN-FIELDNAME.
      WHEN 'PERNR'.
        PERFORM CHAMA_TRIP USING E_ROW-INDEX.
      WHEN 'VORSC'.
        PERFORM CHAMA_PR05 USING E_ROW-INDEX.
    ENDCASE.

  ENDMETHOD.                    "on_double_click

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

* -------------------------------------------------------------------- *
*    Tela de Seleção                                                   *
* -------------------------------------------------------------------- *
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-S01.
PARAMETERS: P_PERNR TYPE ZFITV_PED_VIAG-PERNR MATCHCODE OBJECT PREM,
            P_REINR TYPE ZFITV_PED_VIAG-REINR MATCHCODE OBJECT Z_WITHT_BR.
SELECT-OPTIONS: S_DATES FOR ZFITV_PED_VIAG-DATES OBLIGATORY.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-S02.
PARAMETERS: P_STAT1C AS CHECKBOX,
            P_STAT1P AS CHECKBOX,
            P_STAT1A AS CHECKBOX,
            P_STAT1S AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-S03.
PARAMETERS: P_STAT2C AS CHECKBOX,
            P_STAT2P AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN END OF BLOCK B1.




* -------------------------------------------------------------------- *
*    Processamento                                                     *
* -------------------------------------------------------------------- *
START-OF-SELECTION.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  M_TRATA_DADOS  OUTPUT
*&---------------------------------------------------------------------*
MODULE M_TRATA_DADOS OUTPUT.

  PERFORM LIMPA_CAMPOS.
  PERFORM SELECIONA_DADOS.
  PERFORM MONTA_SAIDA.
  PERFORM MONTA_CATALOGO.

ENDMODULE.                 " M_TRATA_DADOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
FORM LIMPA_CAMPOS .

  REFRESH: T_FTPT_REQ_HEAD, T_FTPT_REQ_ADVANCE, T_ZFITV_PED_VIAG, T_SAIDA, R_PERNR, R_REINR,
           INDEX_ROWS, ROW_NO.

  CLEAR: W_FTPT_REQ_HEAD, W_FTPT_REQ_ADVANCE, W_ZFITV_PED_VIAG, W_SAIDA, OKCODE, V_OKCODE,
         W_PERNR_LOW, W_REINR_LOW, W_DATES_LOW, W_DATES_HIGH, IT_FIELDCATALOG, FC, IT_SORT,
         T_SORT, I, O_GRID, O_CUSTOM_CONTAINER, V_REPID, IT_LAYOUT, CL_ALV.

ENDFORM.                    " LIMPA_CAMPOS

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  IF NOT P_PERNR IS INITIAL.
    R_PERNR-SIGN = 'I'.
    R_PERNR-OPTION = 'EQ'.
    R_PERNR-LOW = P_PERNR.
    APPEND R_PERNR.
    MOVE R_PERNR-LOW TO W_PERNR_LOW.
  ENDIF.

  IF NOT P_REINR IS INITIAL.
    R_REINR-SIGN = 'I'.
    R_REINR-OPTION = 'EQ'.
    R_REINR-LOW = P_REINR.
    APPEND R_REINR.
    MOVE R_REINR-LOW TO W_REINR_LOW.
  ENDIF.

  WRITE S_DATES-LOW TO W_DATES_LOW.
  WRITE S_DATES-HIGH TO W_DATES_HIGH.

  SELECT *
    FROM FTPT_REQ_HEAD
    INTO TABLE T_FTPT_REQ_HEAD
    WHERE PERNR        IN R_PERNR
      AND REINR        IN R_REINR
      AND DATES        IN S_DATES
      AND PLAN_REQUEST EQ 'R'.

  IF SY-SUBRC NE 0.

    MESSAGE I023(ZFI).
    LEAVE TO SCREEN 0.

  ENDIF.

  SELECT *
    FROM FTPT_REQ_ADVANCE
    INTO TABLE T_FTPT_REQ_ADVANCE
    FOR ALL ENTRIES IN T_FTPT_REQ_HEAD
    WHERE PERNR EQ T_FTPT_REQ_HEAD-PERNR
    AND   REINR EQ T_FTPT_REQ_HEAD-REINR.

  IF SY-SUBRC NE 0.

*    MESSAGE i024(zfi).

  ENDIF.

  CLEAR:   R_VIAGEM, R_ADIANTAMENTO.
  REFRESH: R_VIAGEM, R_ADIANTAMENTO.

  IF NOT P_STAT1C  IS INITIAL.
    R_VIAGEM-SIGN = 'I'.
    R_VIAGEM-OPTION = 'EQ'.
    R_VIAGEM-LOW = '01'.
    APPEND R_VIAGEM.
  ENDIF.

  IF NOT P_STAT1P  IS INITIAL.
    R_VIAGEM-SIGN = 'I'.
    R_VIAGEM-OPTION = 'EQ'.
    R_VIAGEM-LOW = '02'.
    APPEND R_VIAGEM.
  ENDIF.

  IF NOT P_STAT1S  IS INITIAL.
    R_VIAGEM-SIGN = 'I'.
    R_VIAGEM-OPTION = 'EQ'.
    R_VIAGEM-LOW = '04'.
    APPEND R_VIAGEM.
  ENDIF.

  IF NOT P_STAT1A  IS INITIAL.
    R_VIAGEM-SIGN = 'I'.
    R_VIAGEM-OPTION = 'EQ'.
    R_VIAGEM-LOW = '03'.
    APPEND R_VIAGEM.
  ENDIF.

  IF NOT P_STAT2C  IS INITIAL.
    R_ADIANTAMENTO-SIGN = 'I'.
    R_ADIANTAMENTO-OPTION = 'EQ'.
    R_ADIANTAMENTO-LOW = '01'.
    APPEND R_ADIANTAMENTO.
  ENDIF.

  IF NOT P_STAT2P  IS INITIAL.
    R_ADIANTAMENTO-SIGN = 'I'.
    R_ADIANTAMENTO-OPTION = 'EQ'.
    R_ADIANTAMENTO-LOW = ''.
    APPEND R_ADIANTAMENTO.
  ENDIF.

  SELECT *
   FROM ZFITV_PED_VIAG
   INTO TABLE T_ZFITV_PED_VIAG
   FOR ALL ENTRIES IN T_FTPT_REQ_HEAD
   WHERE PERNR     EQ T_FTPT_REQ_HEAD-PERNR
   AND   REINR     EQ T_FTPT_REQ_HEAD-REINR.
*   AND   status    IN r_viagem
*   AND   status_ad IN r_adiantamento.

  SORT T_ZFITV_PED_VIAG BY PERNR REINR.

  SELECT PA0001~PERNR PA0001~BUKRS PA0001~GSBER PA0001~KOSTL
         T001~BUTXT TGSBT~GTEXT CSKT~KTEXT
    FROM PA0001 INNER JOIN T001 ON
    PA0001~BUKRS EQ T001~BUKRS
                INNER JOIN TGSBT ON
    PA0001~GSBER EQ TGSBT~GSBER
                INNER JOIN CSKT ON
    PA0001~KOSTL EQ CSKT~KOSTL
    INTO TABLE T_PA1
    FOR ALL ENTRIES IN T_FTPT_REQ_HEAD
    WHERE PERNR     EQ T_FTPT_REQ_HEAD-PERNR AND
        ( ENDDA     GE SY-DATUM AND
          BEGDA     LE SY-DATUM    )         AND
          TGSBT~SPRAS EQ SY-LANGU            AND
          CSKT~SPRAS  EQ SY-LANGU            AND
          CSKT~KOKRS  EQ 'MAGI'.

  SORT T_PA1 BY PERNR.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTA_SAIDA .

  DATA: IM_ANTRG     TYPE ANTRG,
        IM_ABREC     TYPE ABREC,
        IM_AUGDT     TYPE PTRV_AUGDT,
        IM_PARTIAL   TYPE XFELD,
        IM_XFERNOPAY TYPE XFELD,
        EX_ICON      TYPE PTRV_STATUS_ICON.

  DATA:   LF_STATUS_ICON TYPE STRING.

  LOOP AT T_FTPT_REQ_HEAD INTO W_FTPT_REQ_HEAD.

    MOVE W_FTPT_REQ_HEAD-PERNR TO W_SAIDA-PERNR.

*    break abap.
    CALL FUNCTION 'TRIPS_READ_PERIO'
      EXPORTING
        EMPLOYEENUMBER         = W_SAIDA-PERNR
        OLD_VERSIONS           = ''
* IMPORTING
*   NO_TRIPS               =
      TABLES
        PERIO                  = T_PERIO.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    READ TABLE T_PERIO WITH KEY PERNR = W_FTPT_REQ_HEAD-PERNR
                                REINR = W_FTPT_REQ_HEAD-REINR.

    IF SY-SUBRC EQ 0.

      MOVE T_PERIO-ANTRG TO IM_ANTRG.
      MOVE T_PERIO-ABREC TO IM_ABREC.

      CALL METHOD CL_DBSEL_TRIP_APPROVAL=>SET_TRIP_ICON
        EXPORTING
          IM_ANTRG                 = IM_ANTRG
          IM_ABREC                 = IM_ABREC
          IM_PAYMENT_DATE          = IM_AUGDT
          IM_PARTIAL_PAYMENT       = IM_PARTIAL
          IM_TRANSFERED_NO_PAYMENT = IM_XFERNOPAY
        IMPORTING
          EX_ICON                  = LF_STATUS_ICON.

      W_SAIDA-ICON_STAT = LF_STATUS_ICON.

    ENDIF.

    SELECT SINGLE ENAME
      FROM PA0001
      INTO W_SAIDA-ENAME
      WHERE PERNR EQ W_FTPT_REQ_HEAD-PERNR.

    MOVE W_FTPT_REQ_HEAD-REINR TO W_SAIDA-REINR.
    MOVE W_FTPT_REQ_HEAD-DATE_BEG TO W_SAIDA-DATE_BEG.
    MOVE W_FTPT_REQ_HEAD-DATE_END TO W_SAIDA-DATE_END.
    MOVE W_FTPT_REQ_HEAD-LOCATION_END TO W_SAIDA-LOCATION_END.
    MOVE W_FTPT_REQ_HEAD-REQUEST_REASON TO W_SAIDA-REQUEST_REASON.

    SORT T_FTPT_REQ_ADVANCE BY PERNR REINR.
    READ TABLE T_FTPT_REQ_ADVANCE INTO W_FTPT_REQ_ADVANCE WITH KEY PERNR = W_FTPT_REQ_HEAD-PERNR
                                                                   REINR = W_FTPT_REQ_HEAD-REINR
                                                          BINARY SEARCH.

    IF SY-SUBRC EQ 0.

      MOVE W_FTPT_REQ_ADVANCE-VORSC TO W_SAIDA-VORSC.
      MOVE W_FTPT_REQ_ADVANCE-DATVS TO W_SAIDA-DATVS.

    ENDIF.

    MOVE W_FTPT_REQ_HEAD-UNAME  TO W_SAIDA-UNAME.
    WRITE W_FTPT_REQ_HEAD-DATES TO W_SAIDA-DATES.
    WRITE W_FTPT_REQ_HEAD-TIMES TO W_SAIDA-TIMES.

    READ TABLE T_ZFITV_PED_VIAG INTO W_ZFITV_PED_VIAG WITH KEY PERNR = W_FTPT_REQ_HEAD-PERNR
                                                               REINR = W_FTPT_REQ_HEAD-REINR
                                                      BINARY SEARCH.

    IF SY-SUBRC EQ 0.
      W_SAIDA-STATUS1 = W_ZFITV_PED_VIAG-STATUS.
      CASE W_ZFITV_PED_VIAG-STATUS.
        WHEN 01.
          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              NAME                  = ICON_SYSTEM_OKAY
              INFO                  = 'Concluído'
              ADD_STDINF            = 'X'
            IMPORTING
              RESULT                = W_SAIDA-ICONE
            EXCEPTIONS
              ICON_NOT_FOUND        = 1
              OUTPUTFIELD_TOO_SHORT = 2
              OTHERS                = 3.
        WHEN 02.
          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              NAME                  = ICON_MESSAGE_WARNING_SMALL
              INFO                  = 'Pendente'
              ADD_STDINF            = 'X'
            IMPORTING
              RESULT                = W_SAIDA-ICONE
            EXCEPTIONS
              ICON_NOT_FOUND        = 1
              OUTPUTFIELD_TOO_SHORT = 2
              OTHERS                = 3.
        WHEN 3.
          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              NAME                  = ICON_DELETE
              INFO                  = 'Cancelado'
              ADD_STDINF            = 'X'
            IMPORTING
              RESULT                = W_SAIDA-ICONE
            EXCEPTIONS
              ICON_NOT_FOUND        = 1
              OUTPUTFIELD_TOO_SHORT = 2
              OTHERS                = 3.

        WHEN OTHERS.
          CALL FUNCTION 'ICON_CREATE'
            EXPORTING
              NAME                  = ICON_MESSAGE_QUESTION
              INFO                  = 'Falta opção apoio a viagem'
              ADD_STDINF            = 'X'
            IMPORTING
              RESULT                = W_SAIDA-ICONE
            EXCEPTIONS
              ICON_NOT_FOUND        = 1
              OUTPUTFIELD_TOO_SHORT = 2
              OTHERS                = 3.

      ENDCASE.



      MOVE W_ZFITV_PED_VIAG-UNAME  TO W_SAIDA-UNAME2.
      WRITE W_ZFITV_PED_VIAG-DATES TO W_SAIDA-DATES2.
      WRITE W_ZFITV_PED_VIAG-TIMES TO W_SAIDA-TIMES2.
      MOVE W_ZFITV_PED_VIAG-OBS    TO W_SAIDA-OBS.

    ELSE.
      W_SAIDA-STATUS1 = '04'.
      CLEAR W_ZFITV_PED_VIAG.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          NAME                  = ICON_MESSAGE_QUESTION
          INFO                  = 'Falta opção apoio a viagem'
          ADD_STDINF            = 'X'
        IMPORTING
          RESULT                = W_SAIDA-ICONE
        EXCEPTIONS
          ICON_NOT_FOUND        = 1
          OUTPUTFIELD_TOO_SHORT = 2
          OTHERS                = 3.
    ENDIF.

    IF W_ZFITV_PED_VIAG-STATUS_AD = '01'.
      W_SAIDA-STATUS2 = W_ZFITV_PED_VIAG-STATUS_AD.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          NAME                  = ICON_SYSTEM_OKAY
          INFO                  = 'Adiantamento'
          ADD_STDINF            = 'X'
        IMPORTING
          RESULT                = W_SAIDA-STATUS
        EXCEPTIONS
          ICON_NOT_FOUND        = 1
          OUTPUTFIELD_TOO_SHORT = 2
          OTHERS                = 3.
    ELSE.
      W_SAIDA-STATUS2 = ''.
      CALL FUNCTION 'ICON_CREATE'
        EXPORTING
          NAME                  = ICON_MESSAGE_QUESTION
          INFO                  = 'Adiantamento'
          ADD_STDINF            = 'X'
        IMPORTING
          RESULT                = W_SAIDA-STATUS
        EXCEPTIONS
          ICON_NOT_FOUND        = 1
          OUTPUTFIELD_TOO_SHORT = 2
          OTHERS                = 3.
    ENDIF.

    READ TABLE T_PA1 WITH KEY PERNR = W_SAIDA-PERNR
                              BINARY SEARCH.
    IF SY-SUBRC = 0.
      CONCATENATE T_PA1-BUKRS T_PA1-BUTXT
                  INTO W_SAIDA-EMPRESA SEPARATED BY ' - '.

      CONCATENATE T_PA1-GSBER T_PA1-GTEXT
                  INTO W_SAIDA-FILIAL SEPARATED BY ' - '.

      CONCATENATE T_PA1-KOSTL T_PA1-KTEXT
                  INTO W_SAIDA-CCUSTO SEPARATED BY ' - '.
    ENDIF.

    APPEND W_SAIDA TO T_SAIDA.

    CLEAR W_SAIDA.

  ENDLOOP.

  IF NOT R_ADIANTAMENTO[] IS INITIAL AND
     NOT R_VIAGEM[]       IS INITIAL.
    DELETE T_SAIDA WHERE NOT STATUS1 IN R_VIAGEM AND
                         NOT STATUS2 IN R_ADIANTAMENTO.
  ELSEIF R_ADIANTAMENTO[] IS INITIAL AND
     NOT R_VIAGEM[] IS INITIAL.
    DELETE T_SAIDA WHERE NOT STATUS1 IN R_VIAGEM.
  ELSEIF NOT R_ADIANTAMENTO[] IS INITIAL AND
             R_VIAGEM[]       IS INITIAL.
    DELETE T_SAIDA WHERE NOT STATUS2 IN R_ADIANTAMENTO.
  ENDIF.

ENDFORM.                    " MONTA_SAIDA
*&---------------------------------------------------------------------*
*&      Module  M_STATUS  OUTPUT
*&---------------------------------------------------------------------*
MODULE M_STATUS OUTPUT.

  SET TITLEBAR '100'.
  SET PF-STATUS 'ALV'.

ENDMODULE.                 " M_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  M_CONTAINER  OUTPUT
*&---------------------------------------------------------------------*
MODULE M_CONTAINER OUTPUT.

    DATA: X_VARIANT TYPE DISVARIANT.
    X_VARIANT-REPORT = SY-REPID.

  IF O_CUSTOM_CONTAINER IS INITIAL.

    CREATE OBJECT O_CUSTOM_CONTAINER
      EXPORTING
        REPID = V_REPID
        DYNNR = '0100'
        SIDE  = O_CUSTOM_CONTAINER->DOCK_AT_BOTTOM
        RATIO = 83.

    CREATE OBJECT O_GRID
      EXPORTING
        I_PARENT = O_CUSTOM_CONTAINER.

    IT_LAYOUT-ZEBRA      = 'X'.
    IT_LAYOUT-SEL_MODE   = 'X'.
    IT_LAYOUT-BOX_FNAME  = 'SEL'.
    IT_LAYOUT-CWIDTH_OPT = 'X'.

    CALL METHOD O_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = IT_LAYOUT
        I_SAVE          = 'A'
        IS_VARIANT      = X_VARIANT
      CHANGING
        IT_OUTTAB       = T_SAIDA
        IT_SORT         = IT_SORT
        IT_FIELDCATALOG = IT_FIELDCATALOG.

    SET HANDLER:
                  LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK FOR O_GRID.

  ENDIF.

ENDMODULE.                 " M_CONTAINER  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  PERFORM MODIFICA_TABLE.

  MOVE SY-UCOMM TO OKCODE.
  MOVE OKCODE TO V_OKCODE.
  CLEAR OKCODE.

  CASE V_OKCODE.
    WHEN 'CONC'.
      PERFORM F_APROVA.
      CALL METHOD O_CUSTOM_CONTAINER->FREE.
    WHEN 'PEND'.
      PERFORM F_REPROVA.
      CALL METHOD O_CUSTOM_CONTAINER->FREE.
    WHEN 'CANCEL'.
      PERFORM F_BLOQUEIA.
      CALL METHOD O_CUSTOM_CONTAINER->FREE.
    WHEN 'ADIA'.
      PERFORM F_ADIAMENTO.
      CALL METHOD O_CUSTOM_CONTAINER->FREE.
    WHEN 'SAVE'.
      PERFORM F_SALVA.
      CALL METHOD O_CUSTOM_CONTAINER->FREE.
    WHEN 'VOLTA'.
      CALL METHOD O_CUSTOM_CONTAINER->FREE.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      CALL METHOD O_CUSTOM_CONTAINER->FREE.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      CALL METHOD O_CUSTOM_CONTAINER->FREE.
      LEAVE PROGRAM.
    WHEN OTHERS.
      CALL METHOD O_CUSTOM_CONTAINER->FREE.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTA_CATALOGO
*&---------------------------------------------------------------------*
FORM MONTA_CATALOGO .

  REFRESH IT_FIELDCATALOG.

  PERFORM MONTA_FIELDCAT USING:
       'ICON_STAT'      'T_SAIDA' ' '                'Status            ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
       'ICONE'          'T_SAIDA' ' '                'Control           ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
       'PERNR'          'T_SAIDA' 'FTPT_REQ_HEAD'    'Nº Pessoal        ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'ENAME'          'T_SAIDA' 'FTPT_REQ_HEAD'    'Nome              ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'REINR'          'T_SAIDA' 'FTPT_REQ_HEAD'    'Nº Viagem         ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'DATE_BEG'       'T_SAIDA' 'FTPT_REQ_HEAD'    'Data Início       ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'DATE_END'       'T_SAIDA' 'FTPT_REQ_HEAD'    'Data Fim          ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'LOCATION_END'   'T_SAIDA' 'FTPT_REQ_HEAD'    'Local Destino     ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'REQUEST_REASON' 'T_SAIDA' 'FTPT_REQ_HEAD'    'Motivo            ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'STATUS'         'T_SAIDA' 'FTPT_REQ_HEAD'    'Status            ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
       'VORSC'          'T_SAIDA' 'FTPT_REQ_ADVANCE' 'Vlr. Adiantamento ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'DATVS'          'T_SAIDA' 'FTPT_REQ_ADVANCE' 'Data Pgto         ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'UNAME'          'T_SAIDA' 'FTPT_REQ_HEAD'    'Usuário           ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'DATES'          'T_SAIDA' 'FTPT_REQ_HEAD'    'Data              ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'TIMES'          'T_SAIDA' 'FTPT_REQ_HEAD'    'Hora              ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'UNAME2'         'T_SAIDA' 'ZFITV_PED_VIAG'   'Us. Adm. Viagem   ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'DATES2'         'T_SAIDA' ' '   'Data              ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'TIMES2'         'T_SAIDA' ' '   'Hora              ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'EMPRESA'         'T_SAIDA' ' '   'Empresa              ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'FILIAL'         'T_SAIDA' ' '   'Filial              ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'CCUSTO'         'T_SAIDA' ' '   'Centro de Custo              ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
       'OBS'            'T_SAIDA' 'ZFITV_PED_VIAG'   'Obs.              ' ' ' ' ' ' ' ' ' '30' 'X' ' '.

ENDFORM.                    " MONTA_CATALOGO
*&---------------------------------------------------------------------*
*&      Form  monta_fieldcat
*&---------------------------------------------------------------------*
FORM MONTA_FIELDCAT USING
               X_FIELD X_TAB X_REF X_TEXT X_SUM X_JUST X_KEY X_HOTSPOT X_OUTPUTLEN X_EDIT X_ICON.

  I = I + 1.
  CLEAR FC.
  FC-COL_POS       = I.
  FC-FIELDNAME     = X_FIELD.
  FC-TABNAME       = X_TAB.
  FC-REF_TABLE     = X_REF.
  FC-DO_SUM        = X_SUM.
  FC-JUST          = X_JUST.
  FC-KEY           = X_KEY.
  FC-HOTSPOT       = X_HOTSPOT.
  FC-SELTEXT       =
  FC-COLTEXT       = X_TEXT.
  FC-OUTPUTLEN     = X_OUTPUTLEN.
  FC-EDIT          = X_EDIT.
  FC-ICON          = X_ICON.

  IF X_FIELD = 'ICONE'.
    FC-OUTPUTLEN = '4'.
  ENDIF.

  APPEND FC TO IT_FIELDCATALOG.
  CLEAR FC.

ENDFORM.                               " MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_APROVA
*&---------------------------------------------------------------------*
FORM F_APROVA .

  CALL METHOD O_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = INDEX_ROWS
      ET_ROW_NO     = ROW_NO.

  FIELD-SYMBOLS <ROW_NO> TYPE TY_ROW_NO.

  LOOP AT T_SAIDA INTO W_SAIDA.

    READ TABLE ROW_NO ASSIGNING <ROW_NO> WITH KEY ROW_ID = SY-TABIX.

    IF SY-SUBRC EQ 0.

      SELECT SINGLE * FROM ZFITV_PED_VIAG INTO W_ZFITV_PED_VIAG
        WHERE PERNR = W_SAIDA-PERNR AND
              REINR = W_SAIDA-REINR.
      IF SY-SUBRC <> 0.
        CLEAR W_ZFITV_PED_VIAG.
      ENDIF.
      MOVE W_SAIDA-PERNR TO W_ZFITV_PED_VIAG-PERNR.
      MOVE W_SAIDA-REINR TO W_ZFITV_PED_VIAG-REINR.
      MOVE '01'          TO W_ZFITV_PED_VIAG-STATUS.
      MOVE SY-UNAME      TO W_ZFITV_PED_VIAG-UNAME.
      MOVE SY-DATUM      TO W_ZFITV_PED_VIAG-DATES.
      MOVE SY-UZEIT      TO W_ZFITV_PED_VIAG-TIMES.
      MOVE W_SAIDA-OBS   TO W_ZFITV_PED_VIAG-OBS.

      MODIFY ZFITV_PED_VIAG FROM W_ZFITV_PED_VIAG.

    ENDIF.

  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.                    " F_APROVA
*&---------------------------------------------------------------------*
*&      Form  F_REPROVA
*&---------------------------------------------------------------------*
FORM F_REPROVA .

  CALL METHOD O_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = INDEX_ROWS
      ET_ROW_NO     = ROW_NO.

  FIELD-SYMBOLS <ROW_NO> TYPE TY_ROW_NO.

  LOOP AT T_SAIDA INTO W_SAIDA.

    READ TABLE ROW_NO ASSIGNING <ROW_NO> WITH KEY ROW_ID = SY-TABIX.

    IF SY-SUBRC EQ 0.

      SELECT SINGLE * FROM ZFITV_PED_VIAG INTO W_ZFITV_PED_VIAG
        WHERE PERNR = W_SAIDA-PERNR AND
              REINR = W_SAIDA-REINR.
      IF SY-SUBRC <> 0.
        CLEAR W_ZFITV_PED_VIAG.
      ENDIF.
      MOVE W_SAIDA-PERNR TO W_ZFITV_PED_VIAG-PERNR.
      MOVE W_SAIDA-REINR TO W_ZFITV_PED_VIAG-REINR.
      MOVE '02'          TO W_ZFITV_PED_VIAG-STATUS.
      MOVE SY-UNAME      TO W_ZFITV_PED_VIAG-UNAME.
      MOVE SY-DATUM      TO W_ZFITV_PED_VIAG-DATES.
      MOVE SY-UZEIT      TO W_ZFITV_PED_VIAG-TIMES.
      MOVE W_SAIDA-OBS   TO W_ZFITV_PED_VIAG-OBS.

      MODIFY ZFITV_PED_VIAG FROM W_ZFITV_PED_VIAG.

    ENDIF.

  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.                    " F_REPROVA
*&---------------------------------------------------------------------*
*&      Form  F_BLOQUEIA
*&---------------------------------------------------------------------*
FORM F_BLOQUEIA .

  CALL METHOD O_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = INDEX_ROWS
      ET_ROW_NO     = ROW_NO.

  FIELD-SYMBOLS <ROW_NO> TYPE TY_ROW_NO.

  LOOP AT T_SAIDA INTO W_SAIDA.

    READ TABLE ROW_NO ASSIGNING <ROW_NO> WITH KEY ROW_ID = SY-TABIX.

    IF SY-SUBRC EQ 0.

      SELECT SINGLE * FROM ZFITV_PED_VIAG INTO W_ZFITV_PED_VIAG
        WHERE PERNR = W_SAIDA-PERNR AND
              REINR = W_SAIDA-REINR.
      IF SY-SUBRC <> 0.
        CLEAR W_ZFITV_PED_VIAG.
      ENDIF.
      MOVE W_SAIDA-PERNR TO W_ZFITV_PED_VIAG-PERNR.
      MOVE W_SAIDA-REINR TO W_ZFITV_PED_VIAG-REINR.
      MOVE '03'          TO W_ZFITV_PED_VIAG-STATUS.
      MOVE SY-UNAME      TO W_ZFITV_PED_VIAG-UNAME.
      MOVE SY-DATUM      TO W_ZFITV_PED_VIAG-DATES.
      MOVE SY-UZEIT      TO W_ZFITV_PED_VIAG-TIMES.
      MOVE W_SAIDA-OBS   TO W_ZFITV_PED_VIAG-OBS.

      MODIFY ZFITV_PED_VIAG FROM W_ZFITV_PED_VIAG.

    ENDIF.

  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.                    " F_BLOQUEIA
*&---------------------------------------------------------------------*
*&      Form  F_SALVA
*&---------------------------------------------------------------------*
FORM F_SALVA .
  DATA: W_FIELD(20),
        W_VALUE(20).

  LOOP AT T_SAIDA INTO W_SAIDA.

    READ TABLE T_ZFITV_PED_VIAG INTO  W_ZFITV_PED_VIAG WITH KEY PERNR = W_SAIDA-PERNR
                                                                REINR = W_SAIDA-REINR.

    MOVE W_SAIDA-OBS   TO W_ZFITV_PED_VIAG-OBS.

    MODIFY ZFITV_PED_VIAG FROM W_ZFITV_PED_VIAG.

  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.                    " F_SALVA
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_TABLE
*&---------------------------------------------------------------------*
FORM MODIFICA_TABLE .

  CALL METHOD O_GRID->CHECK_CHANGED_DATA
    IMPORTING
      E_VALID = V_VALID.

ENDFORM.                    " MODIFICA_TABLE
*&---------------------------------------------------------------------*
*&      Form  CHAMA_TRIP
*&---------------------------------------------------------------------*
FORM CHAMA_TRIP USING P_INDEX TYPE  LVC_S_ROW-INDEX.

  LOOP AT T_SAIDA INTO W_SAIDA.

    IF SY-TABIX EQ P_INDEX.

      SET PARAMETER ID 'PER' FIELD W_SAIDA-PERNR.
      CALL TRANSACTION 'TRIP' AND SKIP FIRST SCREEN.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHAMA_TRIP
*&---------------------------------------------------------------------*
*&      Form  CHAMA_PR05
*&---------------------------------------------------------------------*
FORM CHAMA_PR05 USING P_INDEX TYPE  LVC_S_ROW-INDEX.

  LOOP AT T_SAIDA INTO W_SAIDA.

    IF SY-TABIX EQ P_INDEX.

      SET PARAMETER ID 'PER' FIELD W_SAIDA-PERNR.
      CALL TRANSACTION 'PR05' AND SKIP FIRST SCREEN.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " CHAMA_PR05
*&---------------------------------------------------------------------*
*&      Form  F_ADIAMENTO
*&---------------------------------------------------------------------*
FORM F_ADIAMENTO .

  CALL METHOD O_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = INDEX_ROWS
      ET_ROW_NO     = ROW_NO.

  FIELD-SYMBOLS <ROW_NO> TYPE TY_ROW_NO.

  LOOP AT T_SAIDA INTO W_SAIDA.

    READ TABLE ROW_NO ASSIGNING <ROW_NO> WITH KEY ROW_ID = SY-TABIX.

    IF SY-SUBRC EQ 0.

      SELECT SINGLE * FROM ZFITV_PED_VIAG INTO W_ZFITV_PED_VIAG
        WHERE PERNR = W_SAIDA-PERNR AND
              REINR = W_SAIDA-REINR.
      IF SY-SUBRC <> 0.
        CLEAR W_ZFITV_PED_VIAG.
      ENDIF.
      MOVE W_SAIDA-PERNR TO W_ZFITV_PED_VIAG-PERNR.
      MOVE W_SAIDA-REINR TO W_ZFITV_PED_VIAG-REINR.
      MOVE '01'          TO W_ZFITV_PED_VIAG-STATUS_AD.
      MOVE SY-UNAME      TO W_ZFITV_PED_VIAG-UNAME.
      MOVE SY-DATUM      TO W_ZFITV_PED_VIAG-DATES.
      MOVE SY-UZEIT      TO W_ZFITV_PED_VIAG-TIMES.
      MOVE W_SAIDA-OBS   TO W_ZFITV_PED_VIAG-OBS.


      MODIFY ZFITV_PED_VIAG FROM W_ZFITV_PED_VIAG.

    ENDIF.

  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.                    " F_ADIAMENTO
