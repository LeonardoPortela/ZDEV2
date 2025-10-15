*&--------------------------------------------------------------------&*
*& Report Name    : Parametrização de Safra para Consulta de Fardos   *&
*& Author         : Victor Hugo                                       *&
*& Date           : 30.05.2012                                        *&
*& Funcional Area : MM                                                *&
*&--------------------------------------------------------------------&*
REPORT  ZMMR0005.

*&--------------------------------------------------------------------&*
*& TABLES
*&--------------------------------------------------------------------&*
TABLES: ZTSAFRAFARDOS.

*&--------------------------------------------------------------------&*
*& TYPES
*&--------------------------------------------------------------------&*
TYPES:
  BEGIN OF TY_SAIDA,
    MARK(1),
    CHARG               TYPE ZTSAFRAFARDOS-CHARG,
    DATA_INICIO         TYPE ZTSAFRAFARDOS-DATA_INICIO,
    DATA_FIM            TYPE ZTSAFRAFARDOS-DATA_FIM,
    WERKS_FROM          TYPE ZTSAFRAFARDOS-WERKS_FROM,
    WERKS_TO            TYPE ZTSAFRAFARDOS-WERKS_TO,
    WERKS_KEY           TYPE ZTSAFRAFARDOS-WERKS_KEY,
    MATNR               TYPE ZTSAFRAFARDOS-MATNR,
    LOGIN               TYPE ZTSAFRAFARDOS-LOGIN,
    SENHA               TYPE ZTSAFRAFARDOS-SENHA,
    USUARIO             TYPE ZTSAFRAFARDOS-USUARIO,
    DATA                TYPE ZTSAFRAFARDOS-DATA,
    HORA                TYPE ZTSAFRAFARDOS-HORA,
    STATUS(4)           TYPE C,
    EDIT(4)             TYPE C,
    EXCLUIR(4)          TYPE C,
    STATUS_T            TYPE ZTSAFRAFARDOS-STATUS,
    IDALGODOEIRA        TYPE ZTSAFRAFARDOS-IDALGODOEIRA,
    ALGODOEIRADESCRICAO TYPE ZTSAFRAFARDOS-ALGODOEIRADESCRICAO,
  END OF TY_SAIDA,

  TY_SAFRAFARDOS TYPE TABLE OF ZTSAFRAFARDOS.

*&--------------------------------------------------------------------&*
*& Internal Table
*&--------------------------------------------------------------------&*
*DATA: IT_ZTSAFRAFARDOS TYPE TABLE OF TY_ZTSAFRAFARDOS,
DATA: IT_SAIDA      TYPE TABLE OF TY_SAIDA,
      TL_INDEX_ROWS TYPE LVC_T_ROW,
      WL_INDEX_ROWS TYPE LVC_S_ROW.

*&--------------------------------------------------------------------&*
*& Work Area
*&--------------------------------------------------------------------&*
*DATA: WA_ZTSAFRAFARDOS TYPE TY_ZTSAFRAFARDOS,
DATA WA_SAIDA         TYPE TY_SAIDA.
DATA WA_NOVO          TYPE ZTSAFRAFARDOS.
*&--------------------------------------------------------------------&*
*& ALV
*&--------------------------------------------------------------------&*
DATA: CL_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_GRID      TYPE REF TO CL_GUI_ALV_GRID,
      IT_FCAT      TYPE LVC_T_FCAT,
      WA_FCAT      TYPE LVC_S_FCAT,
      WA_LAYOUT    TYPE LVC_S_LAYO,
      WA_VARIANT   TYPE DISVARIANT,
      WA_STABLE    TYPE LVC_S_STBL.

*&--------------------------------------------------------------------&*
*& Constants
*&--------------------------------------------------------------------&*
CONSTANTS: TELA_01(4) TYPE C VALUE '0100',
           TELA_02(4) TYPE C VALUE '0200'.

*&--------------------------------------------------------------------&*
*& Variables
*&--------------------------------------------------------------------&*
DATA: EDIT TYPE C.

*&--------------------------------------------------------------------&*
*& Parameters
*&--------------------------------------------------------------------&*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: P_CHARG FOR ZTSAFRAFARDOS-CHARG NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK B1.

START-OF-SELECTION.

  PERFORM: SELECIONA_DADOS.

  CALL SCREEN TELA_01.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  SELECT *
    FROM ZTSAFRAFARDOS
    INTO TABLE @DATA(T_TABLE)
   WHERE CHARG IN @P_CHARG.

  PERFORM: SAIDA TABLES
                 T_TABLE.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  SAIDA
*&---------------------------------------------------------------------*
FORM SAIDA TABLES T_TABLE TYPE TY_SAFRAFARDOS.

  LOOP AT T_TABLE INTO DATA(W_TABLE).

    DATA(WA_SAIDA) = VALUE TY_SAIDA( EXCLUIR              = ICON_DELETE
                                     EDIT                 = ICON_CHANGE
                                     CHARG                = W_TABLE-CHARG
                                     WERKS_FROM           = W_TABLE-WERKS_FROM
                                     WERKS_TO             = W_TABLE-WERKS_TO
                                     MATNR                = W_TABLE-MATNR
                                     WERKS_KEY            = W_TABLE-WERKS_KEY
                                     IDALGODOEIRA         = W_TABLE-IDALGODOEIRA
                                     ALGODOEIRADESCRICAO  = W_TABLE-ALGODOEIRADESCRICAO
                                     LOGIN                = W_TABLE-LOGIN
                                     SENHA                = W_TABLE-SENHA
                                     DATA_INICIO          = W_TABLE-DATA_INICIO
                                     DATA_FIM             = W_TABLE-DATA_FIM
                                     STATUS_T             = W_TABLE-STATUS
                                     USUARIO              = W_TABLE-USUARIO
                                     DATA                 = W_TABLE-DATA
                                     HORA                 = W_TABLE-HORA
                                     STATUS               = SWITCH #( W_TABLE-STATUS
                                                                 WHEN 'L' THEN ICON_LED_GREEN
                                                                   ELSE ICON_LED_RED )
                                   ).

    APPEND WA_SAIDA TO IT_SAIDA.
  ENDLOOP.

ENDFORM.                    " SAIDA

*&---------------------------------------------------------------------*
*&      Module  pbo  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO OUTPUT.

  IF ( CL_CONTAINER IS INITIAL ).
    PERFORM: CREATE_OBJECT.
  ENDIF.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE PAI INPUT.
  CASE SY-UCOMM.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'.
      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
    WHEN: 'EXIT'.
      LEAVE PROGRAM.
    WHEN: 'CREATE'.
      PERFORM: CRIAR_SAFRA.
    WHEN: 'COPY'.
      PERFORM: COPIAR_SAFRA.
  ENDCASE.
ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS: HANDLE_HOTSPOT_CLICK
        FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
ENDCLASS.                    "lcv_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcv_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD HANDLE_HOTSPOT_CLICK.
    PERFORM HANDLE_HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcv_event_handler IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM HANDLE_HOTSPOT_CLICK  USING   I_ROW_ID     TYPE LVC_S_ROW
                                   I_COLUMN_ID  TYPE LVC_S_COL
                                   IS_ROW_NO    TYPE LVC_S_ROID.
  DATA  W_ANSWER(1).

  CASE I_COLUMN_ID.
    WHEN: 'EDIT'.
      EDIT = 'X'.
      PERFORM: EDITAR_SAFRA   USING IS_ROW_NO-ROW_ID.
    WHEN: 'EXCLUIR'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          TEXT_QUESTION         = 'Confirma eliminação?'
          TEXT_BUTTON_1         = 'Sim'(100)
          ICON_BUTTON_1         = 'ICON_OKAY '
          TEXT_BUTTON_2         = 'Não'(101)
          ICON_BUTTON_2         = 'ICON_CANCEL'
          DEFAULT_BUTTON        = '1'
          DISPLAY_CANCEL_BUTTON = ' '
          START_COLUMN          = 25
          START_ROW             = 6
        IMPORTING
          ANSWER                = W_ANSWER
        EXCEPTIONS
          TEXT_NOT_FOUND        = 1
          OTHERS                = 2.

      IF W_ANSWER = '1'. "não
        PERFORM: EXCLUIR_SAFRA  USING IS_ROW_NO-ROW_ID.
      ENDIF.
    WHEN: 'STATUS'.
      PERFORM: BLOQUEAR_SAFRA USING IS_ROW_NO-ROW_ID.

  ENDCASE.
ENDFORM.                    " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  EDITAR_SAFRA
*&---------------------------------------------------------------------*
FORM EDITAR_SAFRA  USING P_ROW_ID.

  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_ROW_ID.

  CLEAR: WA_NOVO.

  WA_NOVO-CHARG                 = WA_SAIDA-CHARG.
  WA_NOVO-DATA_INICIO           = WA_SAIDA-DATA_INICIO.
  WA_NOVO-DATA_FIM              = WA_SAIDA-DATA_FIM.
  WA_NOVO-WERKS_FROM            = WA_SAIDA-WERKS_FROM.
  WA_NOVO-WERKS_TO              = WA_SAIDA-WERKS_TO.
  WA_NOVO-IDALGODOEIRA          = WA_SAIDA-IDALGODOEIRA.
  WA_NOVO-ALGODOEIRADESCRICAO   = WA_SAIDA-ALGODOEIRADESCRICAO.
  WA_NOVO-MATNR                 = WA_SAIDA-MATNR.
  WA_NOVO-WERKS_KEY             = WA_SAIDA-WERKS_KEY.
  WA_NOVO-LOGIN                 = WA_SAIDA-LOGIN.
  WA_NOVO-SENHA                 = WA_SAIDA-SENHA.

  PERFORM: CRIAR_SAFRA.
ENDFORM.                    " EDITAR_SAFRA
*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_SAFRA
*&---------------------------------------------------------------------*
FORM EXCLUIR_SAFRA  USING    P_ROW_ID.

  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_ROW_ID.

  IF ( SY-SUBRC EQ 0 ).
    DELETE FROM ZTSAFRAFARDOS WHERE CHARG      = WA_SAIDA-CHARG
                                AND WERKS_FROM = WA_SAIDA-WERKS_FROM
                                AND WERKS_TO   = WA_SAIDA-WERKS_TO
                                AND LOGIN      = WA_SAIDA-LOGIN.
  ENDIF.

  CLEAR: IT_FCAT[], IT_SAIDA[].
  PERFORM: SELECIONA_DADOS.

  CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = WA_STABLE.

  LEAVE TO SCREEN TELA_01.
  CLEAR: WA_SAIDA.

ENDFORM.                    " EXCLUIR_SAFRA
*&---------------------------------------------------------------------*
*&      Form  BLOQUEAR_SAFRA
*&---------------------------------------------------------------------*
FORM BLOQUEAR_SAFRA  USING   P_ROW_ID.

  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_ROW_ID.

  IF ( SY-SUBRC EQ 0 ).

    IF ( WA_SAIDA-STATUS_T EQ 'L' ).

      UPDATE ZTSAFRAFARDOS SET STATUS  = 'B'
                           WHERE CHARG = WA_SAIDA-CHARG.


      MESSAGE S888(SABAPDOCU) WITH 'Safra bloqueada'.

    ELSEIF ( WA_SAIDA-STATUS_T EQ 'B' ) .

      UPDATE ZTSAFRAFARDOS SET STATUS  = 'L'
                     WHERE CHARG = WA_SAIDA-CHARG.

      MESSAGE S888(SABAPDOCU) WITH 'Safra desbloqueada'.

    ENDIF.

    CLEAR: IT_FCAT[], IT_SAIDA[].
    PERFORM: SELECIONA_DADOS.

    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    LEAVE TO SCREEN TELA_01.
  ENDIF.
ENDFORM.                    " BLOQUEAR_SAFRA

*&---------------------------------------------------------------------*
*&      Form  UPDATE_SAFRA
*&---------------------------------------------------------------------*
FORM UPDATE_SAFRA .

  IF ( EDIT EQ 'X' ).
    UPDATE ZTSAFRAFARDOS SET DATA_INICIO          = WA_NOVO-DATA_INICIO
                             DATA_FIM             = WA_NOVO-DATA_FIM
                             WERKS_FROM           = WA_NOVO-WERKS_FROM
                             WERKS_TO             = WA_NOVO-WERKS_TO
                             IDALGODOEIRA         = WA_NOVO-IDALGODOEIRA
                             ALGODOEIRADESCRICAO  = WA_NOVO-ALGODOEIRADESCRICAO
                             MATNR                = WA_NOVO-MATNR
                             LOGIN                = WA_NOVO-LOGIN
                             SENHA                = WA_NOVO-SENHA
                             STATUS               = WA_SAIDA-STATUS_T
                             USUARIO              = SY-UNAME
                             DATA                 = SY-DATUM
                             HORA                 = SY-UZEIT
    WHERE CHARG    = WA_NOVO-CHARG
    AND WERKS_FROM = WA_NOVO-WERKS_FROM
    AND WERKS_TO   = WA_NOVO-WERKS_TO
    AND LOGIN      = WA_NOVO-LOGIN.

    CLEAR:IT_FCAT[], IT_SAIDA[].
    PERFORM: SELECIONA_DADOS.

    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    CLEAR: EDIT.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.                    " UPDATE_SAFRA

*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
FORM CREATE_OBJECT .
  DATA: GR_EVENT_HANDLER TYPE REF TO LCL_EVENT_HANDLER.

  WA_LAYOUT-BOX_FNAME  = 'MARK'.
  WA_LAYOUT-SEL_MODE   = 'A'.

  IF ( CL_CONTAINER IS INITIAL ).

    CREATE OBJECT CL_CONTAINER
      EXPORTING
        CONTAINER_NAME              = 'CONTAINER'
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.


    CREATE OBJECT CL_GRID
      EXPORTING
        I_PARENT = CL_CONTAINER.

    PERFORM: FCAT.

  ENDIF.


  CREATE OBJECT GR_EVENT_HANDLER.
  SET HANDLER GR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR CL_GRID.

  CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      IS_VARIANT                    = WA_VARIANT
      I_SAVE                        = 'A'
    CHANGING
      IT_OUTTAB                     = IT_SAIDA
      IT_FIELDCATALOG               = IT_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.


ENDFORM.                    " CREATE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  FCAT
*&---------------------------------------------------------------------*
FORM FCAT .

  PERFORM MONTA_CATALOG USING:
    'EXCLUIR'               'Excluir'        '6'  '' 'X' '' 'C' '',
    'EDIT'                  'Editar'         '6'  '' 'X' '' 'C' '',
    'CHARG'                 'Safra'          '6'  '' '' '' '' '',
    'DATA_INICIO'           'Data Inicio'    '11' '' '' '' '' '',
    'DATA_FIM'              'Data Fim'       '11' '' '' '' '' '',
    'WERKS_FROM'            'Centro Origem'  '13' '' '' '' '' '',
    'WERKS_TO'              'Beneficiamento' '14' '' '' '' '' '',
    'IDALGODOEIRA'          'Id Algodoeira'  '10' '' '' '' '' '',
    'ALGODOEIRADESCRICAO'   'Descrição'      '40' '' '' '' '' '',
    'MATNR'                 'Equipamento'    '11' '' '' '' '' '',
    'WERKS_KEY'             'Digito Lote'    '11' '' '' '' '' '',
    'LOGIN'                 'Login'          '11' '' '' '' '' '',
    'SENHA'                 'Senha'          '11' '' '' '' '' '',
    'STATUS'                'Status'         '7'  '' 'X' '' 'C' '',
    'USUARIO'               'Usuario'        '12'  '' '' '' '' '',
    'DATA'                  'Data'           '12' '' '' '' '' '',
    'HORA'                  'Hora'           '12' '' '' '' '' ''.
ENDFORM.                    " FCAT
*&---------------------------------------------------------------------*
*&      Form  MONTA_CATALOG
*&---------------------------------------------------------------------*
FORM MONTA_CATALOG  USING  VALUE(P_FIELDNAME)
                           VALUE(P_DESC)
                           VALUE(P_TAM)
                           VALUE(P_NO_ZERO)
                           VALUE(P_HOTSPOT)
                           VALUE(P_COR)
                           VALUE(P_JUST)
                           VALUE(P_SUM).
  CLEAR: WA_FCAT.

  WA_FCAT-FIELDNAME = P_FIELDNAME.
  WA_FCAT-SCRTEXT_L = P_DESC.
  WA_FCAT-SCRTEXT_M = P_DESC.
  WA_FCAT-SCRTEXT_S = P_DESC.
  WA_FCAT-OUTPUTLEN = P_TAM.
  WA_FCAT-NO_ZERO   = P_NO_ZERO.
  WA_FCAT-HOTSPOT   = P_HOTSPOT.
  WA_FCAT-EMPHASIZE = P_COR.
  WA_FCAT-JUST      = P_JUST.
  WA_FCAT-DO_SUM    = P_SUM.


  APPEND WA_FCAT TO IT_FCAT.
ENDFORM.                    " MONTA_CATALOG
*&---------------------------------------------------------------------*
*&      Form  CRIAR_SAFRA
*&---------------------------------------------------------------------*
FORM CRIAR_SAFRA .

  "CALL SCREEN TELA_02 STARTING AT 15 10 ENDING AT 60 18.
  CALL SCREEN TELA_02 STARTING AT 10 5 ENDING AT 80 18.

ENDFORM.                    " CRIAR_SAFRA
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR  'TB0200'.
ENDMODULE.                 " PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*

MODULE PAI_0200 INPUT.

  CASE SY-UCOMM.
    WHEN: 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
    WHEN: 'OK'.
      IF ( EDIT EQ 'X' ).
        PERFORM: UPDATE_SAFRA.
      ELSE.
        PERFORM: INSERIR_SAFRA.
      ENDIF.
  ENDCASE.

  CLEAR WA_NOVO.

ENDMODULE.                 " PAI_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  INSERIR_SAFRA
*&---------------------------------------------------------------------*
FORM INSERIR_SAFRA .

  IF NOT ( WA_NOVO IS INITIAL ).

    WA_NOVO-STATUS = 'L'.
    WA_NOVO-USUARIO     = SY-UNAME.
    WA_NOVO-DATA        = SY-DATUM.
    WA_NOVO-HORA        = SY-UZEIT.
    INSERT INTO ZTSAFRAFARDOS VALUES WA_NOVO.

    CLEAR: IT_FCAT[], IT_SAIDA[].
    PERFORM: SELECIONA_DADOS.

    CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        IS_STABLE = WA_STABLE.

    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " INSERIR_SAFRA
*&---------------------------------------------------------------------*
*&      Form  COPIAR_SAFRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COPIAR_SAFRA.

  DATA VLINHA TYPE I.

  CALL METHOD CL_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = TL_INDEX_ROWS.

  VLINHA = LINES( TL_INDEX_ROWS ).

  IF VLINHA NE 1.
    MESSAGE 'Selecione 1 linha para copiar' TYPE 'I'.
    EXIT.
  ENDIF.
  LOOP AT TL_INDEX_ROWS INTO WL_INDEX_ROWS.
    IF WL_INDEX_ROWS-ROWTYPE IS NOT INITIAL.
      CONTINUE.
    ENDIF.
    READ TABLE IT_SAIDA INTO WA_SAIDA INDEX WL_INDEX_ROWS-INDEX.
    PERFORM: EDITAR_SAFRA   USING WL_INDEX_ROWS-INDEX.
    EXIT.
  ENDLOOP.
ENDFORM.
