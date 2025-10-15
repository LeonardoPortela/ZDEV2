************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 17.04.2013                                          *
* Objetivo    ...: Seguro de Mercadoria para Transporte Rodoviário     *
* Transação   ...: ZLES0071                                            *
************************************************************************
REPORT  ZLESR0071.

TYPE-POOLS: VRM.

INCLUDE  <ICON>.
*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: ZLEST0052.
*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES:
    BEGIN OF TY_ZLEST0052,
        BUKRS         TYPE ZLEST0052-BUKRS,
        MATKL         TYPE ZLEST0052-MATKL,
        WGBEZ         TYPE ZLEST0052-WGBEZ,
        DATA_INICIO   TYPE ZLEST0052-DATA_INICIO,
        DATA_FIM      TYPE ZLEST0052-DATA_FIM,
        NR_APOLICE    TYPE ZLEST0052-NR_APOLICE,
        LIFNR         TYPE ZLEST0052-LIFNR,
        NOME_SEG      TYPE ZLEST0052-NOME_SEG,
        RESP_SEG      TYPE ZLEST0052-RESP_SEG,
        DATA_CADASTRO TYPE ZLEST0052-DATA_CADASTRO,
        HORA_CADASTRO TYPE ZLEST0052-HORA_CADASTRO,
        USUARIO       TYPE ZLEST0052-USUARIO,
        STATUS        TYPE ZLEST0052-STATUS,
        BLOQUEIO      TYPE ZLEST0052-BLOQUEIO,

    END OF TY_ZLEST0052,


    BEGIN OF TY_SAIDA,
        BUKRS         TYPE ZLEST0052-BUKRS,
        MATKL         TYPE ZLEST0052-MATKL,
        WGBEZ         TYPE ZLEST0052-WGBEZ,
        DATA_INICIO   TYPE ZLEST0052-DATA_INICIO,
        DATA_FIM      TYPE ZLEST0052-DATA_FIM,
        NR_APOLICE    TYPE ZLEST0052-NR_APOLICE,
        LIFNR         TYPE ZLEST0052-LIFNR,
        NOME_SEG      TYPE ZLEST0052-NOME_SEG,
        RESP_SEG      TYPE C LENGTH 30,
        DATA_CADASTRO TYPE ZLEST0052-DATA_CADASTRO,
        HORA_CADASTRO TYPE ZLEST0052-HORA_CADASTRO,
        USUARIO       TYPE ZLEST0052-USUARIO,
        STATUS        TYPE C LENGTH 4,
        "EXCLUIR       TYPE C LENGTH 4,
        BLOQUEIO       TYPE C LENGTH 4,
   END OF TY_SAIDA.

*----------------------------------------------------------------------*
* Internal Table
*----------------------------------------------------------------------*
DATA: IT_ZLEST0052   TYPE TABLE OF TY_ZLEST0052,
      IT_ZLEST0052_V TYPE TABLE OF TY_ZLEST0052,
      IT_SAIDA       TYPE TABLE OF TY_SAIDA.

*----------------------------------------------------------------------*
* Work Area
*----------------------------------------------------------------------*
DATA: WA_ZLEST0052   TYPE TY_ZLEST0052,
      WA_ZLEST0052_V TYPE TY_ZLEST0052,
      WA_SAIDA       TYPE TY_SAIDA,
      WA_INSR        TYPE ZLEST0052.

*----------------------------------------------------------------------*
* Icon
*----------------------------------------------------------------------*
DATA:
      ICON_EMPRESA          TYPE C LENGTH 4,
      ICON_GRUPO_MERCADORIA TYPE C LENGTH 4,
      ICON_DATA_INICIO      TYPE C LENGTH 4,
      ICON_DATA_FIM         TYPE C LENGTH 4,
      ICON_NR_APOLICE       TYPE C LENGTH 4,
      ICON_SEGURADORA       TYPE C LENGTH 4,
      ICON_RESP_SEGURO      TYPE C LENGTH 4.
*----------------------------------------------------------------------*
* Campos
*----------------------------------------------------------------------*
DATA:
      WA_BUKRS       TYPE ZLEST0052-BUKRS,
      WA_MATKL       TYPE ZLEST0052-MATKL,
      WA_WGBEZ       TYPE T023T-WGBEZ,
      WA_DATA_INICIO TYPE ZLEST0052-DATA_INICIO,
      WA_DATA_FIM    TYPE ZLEST0052-DATA_FIM,
      WA_NR_APOLICE  TYPE ZLEST0052-NR_APOLICE,
      WA_LIFNR       TYPE ZLEST0052-LIFNR,
      WA_NAME1       TYPE LFA1-NAME1,
      WA_RESP        TYPE ZLEST0052-RESP_SEG.
*----------------------------------------------------------------------*
* ListBox
*----------------------------------------------------------------------*
DATA:
      NAME  TYPE VRM_ID,
      LIST  TYPE VRM_VALUES,
      VALUE LIKE LINE OF LIST.

*----------------------------------------------------------------------*
* ALV
*----------------------------------------------------------------------*
DATA: CL_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      CL_GRID      TYPE REF TO CL_GUI_ALV_GRID,
      IT_FCAT      TYPE TABLE OF LVC_S_FCAT,
      WA_LAYOUT    TYPE LVC_S_LAYO,
      GS_VARIANT   TYPE DISVARIANT.

*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
DATA: WA_OPCAO TYPE C,
      OK_CODE  LIKE SY-UCOMM.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
            ZM_HANDLE_HOTSPOT FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW_ID
                      E_COLUMN_ID
                      ES_ROW_NO.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT.
    PERFORM Z_HANDLE_HOTSPOT USING    E_ROW_ID
                                      E_COLUMN_ID
                                      ES_ROW_NO.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*

FORM Z_HANDLE_HOTSPOT  USING  P_E_ROW_ID    TYPE  LVC_S_ROW
                              P_E_COLUMN_ID TYPE  LVC_S_COL
                              P_ES_ROW_NO   TYPE  LVC_S_ROID.


  DATA: VER_STATUS TYPE ZLEST0052,
        VAR_STATUS TYPE ZLEST0052-STATUS.

  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.

  CASE P_E_COLUMN_ID.

    WHEN: 'STATUS'.

      IF ( WA_SAIDA-STATUS NE 'X' ).

        WA_SAIDA-STATUS = 'X'.
        MODIFY IT_SAIDA FROM WA_SAIDA INDEX P_E_ROW_ID.

        UPDATE ZLEST0052
          SET STATUS        = WA_SAIDA-STATUS
              DATA_CADASTRO = SY-DATUM
              HORA_CADASTRO = SY-UZEIT
              USUARIO       = SY-UNAME
              BLOQUEIO      = 'A'
          WHERE BUKRS         EQ WA_SAIDA-BUKRS
            AND MATKL         EQ WA_SAIDA-MATKL
            AND DATA_CADASTRO EQ WA_SAIDA-DATA_CADASTRO
            AND HORA_CADASTRO EQ WA_SAIDA-HORA_CADASTRO.

        IF ( SY-SUBRC EQ 0 ).

          PERFORM: SELECIONA_DADOS,
                   ATUALIZA_DADOS.

        ENDIF.

      ELSE.

        CLEAR: WA_SAIDA-STATUS.
        MODIFY IT_SAIDA FROM WA_SAIDA INDEX P_E_ROW_ID.


        UPDATE ZLEST0052
          SET STATUS        = WA_SAIDA-STATUS
              DATA_CADASTRO = SY-DATUM
              HORA_CADASTRO = SY-UZEIT
              USUARIO       = SY-UNAME
              BLOQUEIO      = 'B'
          WHERE BUKRS         EQ WA_SAIDA-BUKRS
            AND MATKL         EQ WA_SAIDA-MATKL
            AND DATA_CADASTRO EQ WA_SAIDA-DATA_CADASTRO
            AND HORA_CADASTRO EQ WA_SAIDA-HORA_CADASTRO.


        IF ( SY-SUBRC EQ 0 ).

          PERFORM: SELECIONA_DADOS,
                   ATUALIZA_DADOS.

        ENDIF.


      ENDIF.

    WHEN: 'EXCLUIR'.

      DELETE FROM ZLEST0052 WHERE BUKRS         EQ WA_SAIDA-BUKRS
                              AND MATKL         EQ WA_SAIDA-MATKL
                              AND DATA_CADASTRO EQ WA_SAIDA-DATA_CADASTRO
                              AND HORA_CADASTRO EQ WA_SAIDA-HORA_CADASTRO.

      IF ( SY-SUBRC EQ 0 ).

        PERFORM: SELECIONA_DADOS,
                 ATUALIZA_DADOS.

      ENDIF.
  ENDCASE.
ENDFORM.                    "Z_HANDLE_HOTSPOT
*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM: PREENCHE_ICON,
           SELECIONA_DADOS,
           CRIAR_ALV.
  CALL SCREEN 0100.
*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO OUTPUT.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.

  "ListBox
*0 – Remetente
*1 – Expedidor
*2 - Recebedor
*3 – Destinatário
*4 – Emitente do CT-e
*5 – Tomador do Serviço

  CASE WA_OPCAO.
      " Destrava os Campos e Botões para novos registros
    WHEN: '1'.

      "      PERFORM: CRIAR_LISTBOX.

      LOOP AT SCREEN.
        IF
          ( SCREEN-NAME EQ 'BTN_CADASTRAR' )  OR
          ( SCREEN-NAME EQ 'BTN_CANCELAR' )   OR
          ( SCREEN-NAME EQ 'BTN_ATUALIZAR' )  OR
          ( SCREEN-NAME EQ 'WA_BUKRS' )       OR
          ( SCREEN-NAME EQ 'WA_MATKL' )       OR
          ( SCREEN-NAME EQ 'WA_DATA_INICIO' ) OR
          ( SCREEN-NAME EQ 'WA_DATA_FIM' )    OR
          ( SCREEN-NAME EQ 'WA_NR_APOLICE' )  OR
          ( SCREEN-NAME EQ 'WA_LIFNR' )       OR
          ( SCREEN-NAME EQ' WA_RESP' ).

          SCREEN-OUTPUT = '1'.
          SCREEN-INPUT  = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN: '2'.

      " Inicia com os Campos e Botões travados
    WHEN OTHERS.

      LOOP AT SCREEN.
        IF
          ( SCREEN-NAME EQ 'BTN_CADASTRAR' )  OR
          ( SCREEN-NAME EQ 'BTN_CANCELAR' )   OR
          ( SCREEN-NAME EQ 'BTN_ATUALIZAR' )  OR
          ( SCREEN-NAME EQ 'WA_BUKRS' )       OR
          ( SCREEN-NAME EQ 'WA_MATKL' )       OR
          ( SCREEN-NAME EQ 'WA_WGBEZ' )       OR
          ( SCREEN-NAME EQ 'WA_DATA_INICIO' ) OR
          ( SCREEN-NAME EQ 'WA_DATA_FIM' )    OR
          ( SCREEN-NAME EQ 'WA_NR_APOLICE' )  OR
          ( SCREEN-NAME EQ 'WA_LIFNR' )       OR
          ( SCREEN-NAME EQ 'WA_NAME1')        OR
          ( SCREEN-NAME EQ' WA_RESP' ).

          SCREEN-OUTPUT = '1'.
          SCREEN-INPUT  = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDMODULE.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE PAI INPUT.
  IF ( SY-DYNNR EQ '0100' ).
    CASE SY-UCOMM.
      WHEN: 'BACK' OR 'CANC'.
        LEAVE TO SCREEN 0.
      WHEN 'EXIT'.
        LEAVE PROGRAM.
        "WHEN: 'BTN_ATUALIZAR'.
      WHEN: 'ENTER'.
        PERFORM: ATUALIZAR_INFO.
      WHEN: 'NOVA'.
        WA_OPCAO = '1'.
        LEAVE TO SCREEN 0100.
      WHEN: 'BTN_CADASTRAR'.
        PERFORM: CADASTRAR_SEGURADORA.

      WHEN: 'BTN_CANCELAR'.
        CLEAR: WA_OPCAO.
        PERFORM: LIMPAR_CAMPOS.
        LEAVE TO SCREEN 0100.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_INCON
*&---------------------------------------------------------------------*
FORM PREENCHE_ICON .
  ICON_EMPRESA           = ICON_PLANT.
  ICON_GRUPO_MERCADORIA  = ICON_MATERIAL_REVISION.
  ICON_DATA_INICIO       = ICON_DATE.
  ICON_DATA_FIM          = ICON_DATE.
  ICON_NR_APOLICE        = ICON_CHANGE_NUMBER.
  ICON_SEGURADORA        = ICON_PARTNER.
  ICON_RESP_SEGURO       = ICON_PERSONAL_SETTINGS.
ENDFORM.                    " PREENCHE_INCON
*&---------------------------------------------------------------------*
*&      Form  CRIAR_LISTBOX
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM CRIAR_LISTBOX .

  CLEAR: VALUE, LIST.

  NAME = 'WA_RESP'.
  VALUE-KEY = '0'.
  VALUE-TEXT = 'Remetente'.
  APPEND VALUE TO LIST.

  NAME = 'WA_RESP'.
  VALUE-KEY = '1'.
  VALUE-TEXT = 'Expedidor'.
  APPEND VALUE TO LIST.

  NAME = 'WA_RESP'.
  VALUE-KEY = '2'.
  VALUE-TEXT = 'Recebedor'.
  APPEND VALUE TO LIST.

  NAME = 'WA_RESP'.
  VALUE-KEY = '3'.
  VALUE-TEXT = 'Destinatário'.
  APPEND VALUE TO LIST.

  NAME = 'WA_RESP'.
  VALUE-KEY = '4'.
  VALUE-TEXT = 'Emitente de Ct-e'.
  APPEND VALUE TO LIST.

  NAME = 'WA_RESP'.
  VALUE-KEY = '5'.
  VALUE-TEXT = 'Tomador do Serviço'.
  APPEND VALUE TO LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = NAME
      VALUES = LIST.
ENDFORM.                    " CRIAR_LISTBOX

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  REFRESH: IT_ZLEST0052[], IT_SAIDA[].

  SELECT BUKRS MATKL WGBEZ DATA_INICIO  DATA_FIM NR_APOLICE
         LIFNR NOME_SEG RESP_SEG DATA_CADASTRO HORA_CADASTRO USUARIO STATUS BLOQUEIO
    FROM ZLEST0052
    INTO TABLE IT_ZLEST0052.

  LOOP AT IT_ZLEST0052 INTO WA_ZLEST0052.

    WA_SAIDA-STATUS       = WA_ZLEST0052-STATUS.

    CASE WA_ZLEST0052-BLOQUEIO.
      WHEN: 'A'.
        WA_SAIDA-BLOQUEIO = ICON_UNLOCKED .
      WHEN: 'B'.
        WA_SAIDA-BLOQUEIO = ICON_LOCKED.
    ENDCASE.

    WA_SAIDA-BUKRS        = WA_ZLEST0052-BUKRS.
    WA_SAIDA-MATKL        = WA_ZLEST0052-MATKL.
    WA_SAIDA-WGBEZ        = WA_ZLEST0052-WGBEZ.
    WA_SAIDA-DATA_INICIO  = WA_ZLEST0052-DATA_INICIO.
    WA_SAIDA-DATA_FIM     = WA_ZLEST0052-DATA_FIM.
    WA_SAIDA-NR_APOLICE   = WA_ZLEST0052-NR_APOLICE.
    WA_SAIDA-LIFNR        = WA_ZLEST0052-LIFNR.
    WA_SAIDA-NOME_SEG     = WA_ZLEST0052-NOME_SEG.

    "0 – Remetente
    "1 – Expedidor
    "2 - Recebedor
    "3 – Destinatário
    "4 – Emitente do CT-e
    "5 – Tomador do Serviço

*    CASE WA_ZLEST0052-RESP_SEG.
*      WHEN: '0'.
*
*        WA_SAIDA-RESP_SEG         = 'Remetente'.
*      WHEN: '1'.
*        WA_SAIDA-RESP_SEG         = 'Expedidor'.
*      WHEN: '2'.
*        WA_SAIDA-RESP_SEG         = 'Recebedor'.
*      WHEN: '3'.
*        WA_SAIDA-RESP_SEG         = 'Destinatário'.
*      WHEN: '4'.
*        WA_SAIDA-RESP_SEG         = 'Emitente do Ct-e'.
*      WHEN: '5'.
*        WA_SAIDA-RESP_SEG         = 'Tomador do Serviço'.
*
*    ENDCASE.

    WA_SAIDA-DATA_CADASTRO    = WA_ZLEST0052-DATA_CADASTRO.
    WA_SAIDA-HORA_CADASTRO    = WA_ZLEST0052-HORA_CADASTRO.
    WA_SAIDA-USUARIO          = WA_ZLEST0052-USUARIO.

    APPEND WA_SAIDA TO IT_SAIDA.
    CLEAR: WA_SAIDA, WA_ZLEST0052.
  ENDLOOP.

ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  atualiza_dados
*&---------------------------------------------------------------------*
FORM ATUALIZA_DADOS.
  CALL METHOD CL_GRID->REFRESH_TABLE_DISPLAY.
ENDFORM.                    "atualiza_dados

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_INFO
*&---------------------------------------------------------------------*
FORM ATUALIZAR_INFO .

  DATA: WA_T023T TYPE T023T,
        WA_LFA1 TYPE LFA1.


  IF NOT ( WA_MATKL IS INITIAL ).
    SELECT SINGLE * FROM T023T INTO WA_T023T WHERE MATKL EQ WA_MATKL.
    WA_WGBEZ = WA_T023T-WGBEZ.
  ENDIF.

  IF NOT ( WA_LIFNR IS INITIAL ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WA_LIFNR
      IMPORTING
        OUTPUT = WA_LIFNR.

    SELECT SINGLE * FROM LFA1 INTO WA_LFA1 WHERE LIFNR EQ WA_LIFNR.
    WA_NAME1 = WA_LFA1-NAME1.
  ENDIF.

ENDFORM.                    " ATUALIZAR_INFO
*&---------------------------------------------------------------------*
*&      Form  CADASTRAR_SEGURADORA
*&---------------------------------------------------------------------*
FORM CADASTRAR_SEGURADORA .
  DATA: VAR_ERR TYPE C.

  SELECT BUKRS MATKL WGBEZ DATA_INICIO  DATA_FIM NR_APOLICE
         LIFNR NOME_SEG RESP_SEG DATA_CADASTRO HORA_CADASTRO USUARIO STATUS BLOQUEIO
    FROM ZLEST0052
    INTO TABLE IT_ZLEST0052_V
  WHERE BUKRS EQ WA_BUKRS
    AND MATKL EQ WA_MATKL.

  LOOP AT IT_ZLEST0052_V INTO WA_ZLEST0052_V.

    IF ( WA_ZLEST0052_V-BLOQUEIO EQ 'A' ).
      VAR_ERR = 'X'.
    ENDIF.

    CLEAR: WA_ZLEST0052_V.
  ENDLOOP.


  IF ( VAR_ERR NE 'X' ).


    IF ( WA_WGBEZ IS INITIAL ) AND ( WA_NAME1 IS INITIAL ).
      PERFORM: ATUALIZAR_INFO.
    ENDIF.

    CLEAR: WA_ZLEST0052.
    WA_INSR-STATUS        = 'X'.
    WA_INSR-BLOQUEIO      = 'A'.
    WA_INSR-BUKRS         = WA_BUKRS.
    WA_INSR-MATKL         = WA_MATKL.
    WA_INSR-WGBEZ         = WA_WGBEZ.
    WA_INSR-DATA_INICIO   = WA_DATA_INICIO.
    WA_INSR-DATA_FIM      = WA_DATA_FIM.
    WA_INSR-NR_APOLICE    = WA_NR_APOLICE.
    WA_INSR-LIFNR         = WA_LIFNR.
    WA_INSR-NOME_SEG      = WA_NAME1.
    "WA_INSR-RESP_SEG      = WA_RESP.
    WA_INSR-DATA_CADASTRO = SY-DATUM.
    WA_INSR-HORA_CADASTRO = SY-UZEIT.
    WA_INSR-USUARIO       = SY-UNAME.

    INSERT INTO ZLEST0052 VALUES WA_INSR.

    PERFORM: LIMPAR_CAMPOS,
             SELECIONA_DADOS,
             ATUALIZA_DADOS.


    CLEAR: WA_OPCAO.
    LEAVE TO SCREEN 0100.

  ELSE.
    MESSAGE E899(SD) WITH 'Apólice já cadastrada para' 'este grupo de Mercadoria'.
  ENDIF.



ENDFORM.                    " CADASTRAR_SEGURADORA


*&---------------------------------------------------------------------*
*&      Form  limpar_campos
*&---------------------------------------------------------------------*
FORM LIMPAR_CAMPOS.
  CLEAR: WA_BUKRS, WA_MATKL, WA_WGBEZ, WA_DATA_INICIO, WA_DATA_FIM, WA_NR_APOLICE, WA_LIFNR, WA_NAME1, WA_RESP, NAME, VALUE.
  REFRESH: LIST.
ENDFORM.                    "limpar_campos

*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV
*&---------------------------------------------------------------------*
FORM CRIAR_ALV .

  DATA: WA_EVENT TYPE REF TO LCL_EVENT_RECEIVER.

  CREATE OBJECT CL_CONTAINER
    EXPORTING
      CONTAINER_NAME              = 'CONTAINER_PRINCIPAL'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  PERFORM: CRIAR_FCAT.

  CREATE OBJECT CL_GRID
    EXPORTING
      I_PARENT          = CL_CONTAINER
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.

  CREATE OBJECT WA_EVENT.
  SET HANDLER:  WA_EVENT->ZM_HANDLE_HOTSPOT FOR CL_GRID.

  CALL METHOD CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
    CHANGING
      IT_OUTTAB                     = IT_SAIDA[]
      IT_FIELDCATALOG               = IT_FCAT
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

ENDFORM.                    " CRIAR_ALV
*&---------------------------------------------------------------------*
*&      Form  CRIAR_FCAT
*&---------------------------------------------------------------------*
FORM CRIAR_FCAT .

  PERFORM ALV_PREENCHE_CAT USING:

        'STATUS'        'Status'                  '6'  'X' '' ''  '' '' 'X',
        "'EXCLUIR'       'Excluir'                 '6'  'X' '' ''  '' 'C' '',
        'BLOQUEIO'      'Bloqueio'                '8'  '' '' ''  '' 'C' '',
        'BUKRS'         'Empresa'                 '8'  ''  '' ''  '' ''  '',
        'MATKL'         'Grupo de Mercadoria'     '15' ''  '' ''  '' ''  '',
        'WGBEZ'         'Descrição'               '25' ''  '' ''  '' ''  '',
        'DATA_INICIO'   'Data Inicio(vigência)'   '15' ''  '' ''  '' ''  '',
        'DATA_FIM'      'Data Fim(vigência)'      '15' ''  '' ''  '' ''  '',
        'NR_APOLICE'    'Nr. Apólice'             '20' ''  '' ''  '' ''  '',
        'LIFNR'         'Nr. Seguradora'          '15' ''  '' ''  '' ''  '',
        'NOME_SEG'      'Nome Seguradora'         '25' ''  '' ''  '' ''  '',
        "'RESP_SEG'      'Responsável pelo Seguro' '15' ''  '' ''  '' ''  '',
        'USUARIO'       'Usuário'                 '12' ''  '' ''  '' ''  '',
        'DATA_CADASTRO' 'Data Cadastro'           '15' ''  '' ''  '' ''  '',
        'HORA_CADASTRO' 'Hora Cadastro'           '15' ''  '' ''  '' ''  ''.



ENDFORM.                    " CRIAR_FCAT
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING  P_CAMPO TYPE C
                              P_DESC  TYPE C
                              P_TAM   TYPE C
                              P_HOT   TYPE C
                              P_ZERO  TYPE C
                              P_SUM   TYPE C
                              P_COR   TYPE C
                              P_JUST  TYPE C
                              P_CHECK TYPE C.

  DATA: WL_FCAT TYPE LVC_S_FCAT.
  WL_FCAT-FIELDNAME = P_CAMPO.
  WL_FCAT-SCRTEXT_L = P_DESC.
  WL_FCAT-SCRTEXT_M = P_DESC.
  WL_FCAT-SCRTEXT_S = P_DESC.
  WL_FCAT-HOTSPOT   = P_HOT.
  WL_FCAT-DO_SUM    = P_SUM.
  WL_FCAT-OUTPUTLEN = P_TAM.
  WL_FCAT-NO_ZERO   = P_ZERO.
  WL_FCAT-EMPHASIZE = P_COR.
  WL_FCAT-JUST      = P_JUST.
  WL_FCAT-CHECKBOX  = P_CHECK.
  APPEND WL_FCAT TO IT_FCAT.

ENDFORM.                    " ALV_PREENCHE_CAT
