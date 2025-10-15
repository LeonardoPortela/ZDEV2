************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 02.04.2013                                          *
* Objetivo    ...: Controle de Chave de Banco                          *
* Transação   ...: ZTRR0001                                            *
************************************************************************
REPORT  ZTRR0001.

INCLUDE <ICON>.
*------------------------------------------------------------------------
* TABLES
*------------------------------------------------------------------------
TABLES: ZTRT0001, ZTRT0002.
*------------------------------------------------------------------------
* TYPES
*------------------------------------------------------------------------
TYPES:

      BEGIN OF TY_ZTRT0001,
          BUKRS       TYPE ZTRT0001-BUKRS,
          HBKID       TYPE ZTRT0001-HBKID,
          BANKK       TYPE ZTRT0001-BANKK,
          BANKA       TYPE ZTRT0001-BANKA,
          CH_ACESSO   TYPE ZTRT0001-CH_ACESSO,
          PW_AC       TYPE ZTRT0001-PW_AC,
          PW_CC       TYPE ZTRT0001-PW_CC,
          USNAM       TYPE ZTRT0001-USNAM,
          DT_ATUAL    TYPE ZTRT0001-DT_ATUAL,
          HORA_ATUAL  TYPE ZTRT0001-HORA_ATUAL,
          OBSERVACAO  TYPE ZTRT0001-OBSERVACAO,
          IDENT       TYPE ZTRT0001-IDENT,
      END OF TY_ZTRT0001,




      BEGIN OF TY_ZTRT0002,
        DT_ATUAL     TYPE ZTRT0002-DT_ATUAL,
        HORA_ATUAL   TYPE ZTRT0002-HORA_ATUAL,
        BUKRS        TYPE ZTRT0002-BUKRS,
        HBKID        TYPE ZTRT0002-HBKID,
        CH_ACESSO    TYPE ZTRT0002-CH_ACESSO,
        PW_AC        TYPE ZTRT0002-PW_AC,
        PW_CC        TYPE ZTRT0002-PW_CC,
        USNAM        TYPE ZTRT0002-USNAM,
        OBSERVACAO   TYPE ZTRT0002-OBSERVACAO,
        ACAO         TYPE ZTRT0002-ACAO,
        ID_LOG       TYPE ZTRT0002-ID_LOG,
      END OF TY_ZTRT0002,

      BEGIN OF TY_SAIDA,
          BUKRS      TYPE ZTRT0001-BUKRS,
          HBKID      TYPE ZTRT0001-HBKID,
          BANKK      TYPE ZTRT0001-BANKK,
          BANKA      TYPE ZTRT0001-BANKA,
          CH_ACESSO  TYPE ZTRT0001-CH_ACESSO,
          PW_AC      TYPE ZTRT0001-PW_AC,
          PW_CC      TYPE ZTRT0001-PW_CC,
          USNAM      TYPE ZTRT0001-USNAM,
          DT_ATUAL   TYPE ZTRT0001-DT_ATUAL,
          HORA_ATUAL TYPE ZTRT0001-HORA_ATUAL,
          OBSERVACAO TYPE ZTRT0001-OBSERVACAO,
          IDENT      TYPE ZTRT0001-IDENT,
          EDITAR     TYPE C LENGTH 4,
          EXCLUIR    TYPE C LENGTH 4,
       END OF TY_SAIDA.

*------------------------------------------------------------------------
* INTERNAL TABLE
*------------------------------------------------------------------------

DATA: IT_SAIDA     TYPE TABLE OF TY_SAIDA,
      IT_FCAT      TYPE TABLE OF LVC_S_FCAT,
      IT_FCAT_LOG  TYPE TABLE OF LVC_S_FCAT,
      IT_ZTRT0001  TYPE TABLE OF TY_ZTRT0001,
      IT_ZTRT0002  TYPE TABLE OF TY_ZTRT0002.
*------------------------------------------------------------------------
* WORK AREA
*------------------------------------------------------------------------
DATA: WA_SAIDA    TYPE TY_SAIDA,
      WA_ZTRT0001 TYPE TY_ZTRT0001,
      WA_ZTRT0002 TYPE TY_ZTRT0002.

DATA: WA_ZTRT0001_AUX TYPE ZTRT0001.

DATA: WA_CONT           TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV            TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT         TYPE LVC_S_LAYO.


DATA: WA_CONT_LOG    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WA_ALV_LOG     TYPE REF TO CL_GUI_ALV_GRID,
      WA_LAYOUT_LOG  TYPE LVC_S_LAYO.

DATA: VAR_FILTRO TYPE C.

*------------------------------------------------------------------------
* VARIAVEL
*------------------------------------------------------------------------
DATA: WA_EDIT TYPE C.

*------------------------------------------------------------------------
* CAMPOS TELA
*------------------------------------------------------------------------
DATA: P_BUKRS        TYPE T012-BUKRS,
      P_HBKID        TYPE T012-HBKID,
      P_BANKL        TYPE T012-BANKL,
      P_BANKA        TYPE BNKA-BANKA,
      P_BANKN        TYPE T012K-BANKN,
      P_CHAVE        TYPE C LENGTH 15,
      P_SENHA_AC     TYPE C LENGTH 15,
      P_SENHA_CC     TYPE C LENGTH 15,
      P_OBS          TYPE C LENGTH 50,
      P_USUARIO      TYPE SY-UNAME,
      P_DATA         TYPE C LENGTH 10,
      P_HORA         TYPE C LENGTH 8,
      P_EMPRESA      TYPE T012-BUKRS,
      P_BCO_EMPRESA  TYPE T012-HBKID.

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.
DATA: WA_EVENT           TYPE REF TO LCL_EVENT_RECEIVER.

*------------------------------------------------------------------------
* CONSTANTS
*------------------------------------------------------------------------
CONSTANTS: C_TELA_01 TYPE SY-DYNNR VALUE '0100'.

START-OF-SELECTION.

  PERFORM:  SELECIONA_DADOS,
            CRIAR_ALV,
            CRIAR_ALV_LOG.

  CALL SCREEN C_TELA_01.

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
MODULE PBO OUTPUT.

  SET PF-STATUS: 'PF0100'.
  SET TITLEBAR:  'TB0100'.

  LOOP AT SCREEN.

    IF ( SCREEN-NAME  = 'BTN_ALTERAR' ) OR ( SCREEN-NAME = 'BTN_CANCELAR' ) AND ( WA_EDIT IS INITIAL ).
      SCREEN-OUTPUT = '1'.
      SCREEN-INPUT  = '0'.
      MODIFY SCREEN.
    ENDIF.

    IF ( WA_EDIT EQ 'X' ).

      IF ( SCREEN-NAME  = 'BTN_ALTERAR' ).
        SCREEN-OUTPUT = '1'.
        SCREEN-INPUT  = '1'.
        MODIFY SCREEN.
      ENDIF.

      IF ( SCREEN-NAME  = 'BTN_ADICIONAR' ) OR ( SCREEN-NAME = 'BTN_EXCLUIR'  ) OR ( SCREEN-NAME = 'P_BUKRS' ) OR ( SCREEN-NAME = 'P_HBKID' ).
        SCREEN-OUTPUT = '1'.
        SCREEN-INPUT  = '0'.
        MODIFY SCREEN.
      ENDIF.

    ENDIF.

  ENDLOOP.

  PERFORM: PREENCHE_DADOS_USUARIO.

ENDMODULE.                 " PBO  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE PAI INPUT.

  CASE SY-DYNNR.
    WHEN: '0100'.
      CASE SY-UCOMM.
        WHEN: 'BACK' OR 'CANC'.
          LEAVE TO SCREEN 0.
        WHEN: 'EXIT'.
          LEAVE PROGRAM.

        WHEN: 'BTN_ADICIONAR'.
          PERFORM: ADICIONAR_CONTA.

        WHEN: 'BTN_REFRESH'.
          PERFORM: ATUALIZAR_INFORMACOES.

        WHEN: 'BTN_ALTERAR'.
          PERFORM: ALTERAR_CONTA.

        WHEN: 'BTN_CANCELAR'.
          PERFORM: LIMPAR_CAMPOS.
          REFRESH IT_ZTRT0002[].
          CALL METHOD WA_ALV_LOG->REFRESH_TABLE_DISPLAY.
          CLEAR: WA_EDIT.
          LEAVE TO SCREEN 0100.

          "WHEN: 'BTN_FILTRAR'.
        WHEN: 'ENTER'.
          VAR_FILTRO = 'X'.

          IF ( P_BCO_EMPRESA IS INITIAL ) AND ( P_EMPRESA IS INITIAL ).
            PERFORM: SELECIONA_DADOS.
          ELSE.
            PERFORM: SELECIONAR_FILTRO.
          ENDIF.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " PAI  INPUT
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
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
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD: ZM_HANDLE_HOTSPOT.
    PERFORM Z_HANDLE_HOTSPOT USING    E_ROW_ID
                                      E_COLUMN_ID
                                      ES_ROW_NO.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
FORM Z_HANDLE_HOTSPOT  USING    P_E_ROW_ID    TYPE  LVC_S_ROW
                                P_E_COLUMN_ID TYPE  LVC_S_COL
                                P_ES_ROW_NO   TYPE  LVC_S_ROID.
  DATA: WA_STRING TYPE STRING.

  DATA: WA_T012  TYPE T012,
        WA_BNKA  TYPE BNKA,
        WA_T012K TYPE T012K.

  CLEAR: WA_T012 ,
         WA_BNKA ,
         WA_T012K.

  CLEAR: WA_SAIDA.

  CASE P_E_COLUMN_ID.

    WHEN: 'EDITAR'.

      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.

      WA_EDIT = 'X'.

      P_BUKRS    = WA_SAIDA-BUKRS.
      P_HBKID    = WA_SAIDA-HBKID.
      P_BANKL    = WA_SAIDA-BANKK.
      P_BANKN    = WA_SAIDA-BANKA.

      CLEAR: WA_STRING.
      WA_STRING = WA_SAIDA-CH_ACESSO.
      CALL METHOD CL_HTTP_UTILITY=>IF_HTTP_UTILITY~DECODE_BASE64
        EXPORTING
          ENCODED = WA_STRING
        RECEIVING
          DECODED = P_CHAVE.


      CLEAR: WA_STRING.
      WA_STRING = WA_SAIDA-PW_AC.
      CALL METHOD CL_HTTP_UTILITY=>IF_HTTP_UTILITY~DECODE_BASE64
        EXPORTING
          ENCODED = WA_STRING
        RECEIVING
          DECODED = P_SENHA_AC.

      CLEAR: WA_STRING.
      WA_STRING = WA_SAIDA-PW_CC.
      CALL METHOD CL_HTTP_UTILITY=>IF_HTTP_UTILITY~DECODE_BASE64
        EXPORTING
          ENCODED = WA_STRING
        RECEIVING
          DECODED = P_SENHA_CC.

      P_OBS      = WA_SAIDA-OBSERVACAO.

*      SELECT SINGLE * FROM T012 INTO WA_T012 WHERE BUKRS EQ WA_SAIDA-BUKRS
*                                               AND HBKID EQ WA_SAIDA-HBKID.
*      P_BANKL = WA_T012-BANKL.
*
      SELECT SINGLE * FROM BNKA INTO WA_BNKA WHERE BANKL EQ WA_SAIDA-BANKK.
      P_BANKA = WA_BNKA-BANKA.
*
*      SELECT SINGLE * FROM T012K INTO WA_T012K WHERE BUKRS EQ WA_SAIDA-BUKRS
*                                                 AND HBKID EQ WA_SAIDA-HBKID.
*      P_BANKN = WA_T012K-BANKN.

      CLEAR: WA_ZTRT0001_AUX.
      WA_ZTRT0001_AUX-BUKRS      = WA_SAIDA-BUKRS.
      WA_ZTRT0001_AUX-HBKID      = WA_SAIDA-HBKID.
      WA_ZTRT0001_AUX-CH_ACESSO  = WA_SAIDA-CH_ACESSO.
      WA_ZTRT0001_AUX-PW_AC      = WA_SAIDA-PW_AC.
      WA_ZTRT0001_AUX-PW_CC      = WA_SAIDA-PW_CC.
      WA_ZTRT0001_AUX-OBSERVACAO = WA_SAIDA-OBSERVACAO.
      WA_ZTRT0001_AUX-IDENT      = WA_SAIDA-IDENT.

      PERFORM: GRAVAR_LOG USING WA_ZTRT0001_AUX
                                'C' .
      PERFORM: VISUALIZAR_LOG USING WA_SAIDA-BUKRS
                                    WA_SAIDA-HBKID
                                    WA_SAIDA-IDENT.

      LEAVE TO SCREEN 0100.

    WHEN: 'EXCLUIR'.

      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_E_ROW_ID.

      IF ( SY-SUBRC EQ 0 ).
        DELETE IT_SAIDA INDEX P_E_ROW_ID.
        DELETE FROM ZTRT0001 WHERE BUKRS EQ WA_SAIDA-BUKRS
                               AND HBKID EQ WA_SAIDA-HBKID
                               AND IDENT EQ WA_SAIDA-IDENT.

        CLEAR: WA_ZTRT0001_AUX.

        WA_ZTRT0001_AUX-BUKRS      = WA_SAIDA-BUKRS.
        WA_ZTRT0001_AUX-HBKID      = WA_SAIDA-HBKID.
        WA_ZTRT0001_AUX-CH_ACESSO  = WA_SAIDA-CH_ACESSO.
        WA_ZTRT0001_AUX-PW_AC      = WA_SAIDA-PW_AC.
        WA_ZTRT0001_AUX-PW_CC      = WA_SAIDA-PW_CC.
        WA_ZTRT0001_AUX-OBSERVACAO = WA_SAIDA-OBSERVACAO.
        WA_ZTRT0001_AUX-IDENT      = WA_SAIDA-IDENT.


        PERFORM: GRAVAR_LOG USING WA_ZTRT0001_AUX
                                  'E'.
        IF ( SY-SUBRC EQ 0 ).
          CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY .
        ENDIF.

        PERFORM: ENVIAR_EMAIL USING WA_ZTRT0001_AUX
                       'Conta Excluída - Contr. de Chave de Bancária'
                       'E'.
      ENDIF.

  ENDCASE.
ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  PREENCHE_MACTHCODE
*&---------------------------------------------------------------------*
FORM PREENCHE_DADOS_USUARIO .

  DATA: DIA  TYPE C LENGTH 2,
        MES  TYPE C LENGTH 2,
        ANO  TYPE C LENGTH 4,
        HORA TYPE C LENGTH 2,
        MIN  TYPE C LENGTH 2,
        SEG  TYPE C LENGTH 2.


  CLEAR: P_USUARIO, P_DATA, P_HORA,
         DIA, MES, ANO.

  P_USUARIO = SY-UNAME.

  DIA = SY-DATUM+6(2).
  MES = SY-DATUM+4(2).
  ANO = SY-DATUM(4).

  CONCATENATE DIA '/' MES '/' ANO INTO P_DATA.

  HORA = SY-UZEIT(2).
  MIN  = SY-UZEIT+2(2).
  SEG  = SY-UZEIT+4(2).

  CONCATENATE HORA ':' MIN ':' SEG INTO P_HORA.


ENDFORM.                    " PREENCHE_MACTHCODE

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM SELECIONA_DADOS .

  REFRESH: IT_ZTRT0001.
  CLEAR: IT_SAIDA[].

  SELECT BUKRS HBKID BANKK BANKA CH_ACESSO PW_AC PW_CC USNAM DT_ATUAL HORA_ATUAL OBSERVACAO IDENT
    FROM ZTRT0001
    INTO TABLE IT_ZTRT0001.

  LOOP AT IT_ZTRT0001 INTO WA_ZTRT0001.

    WA_SAIDA-IDENT      = WA_ZTRT0001-IDENT.

    WA_SAIDA-BUKRS      = WA_ZTRT0001-BUKRS.
    WA_SAIDA-HBKID      = WA_ZTRT0001-HBKID.
    WA_SAIDA-BANKK      = WA_ZTRT0001-BANKK.
    WA_SAIDA-BANKA      = WA_ZTRT0001-BANKA.
    WA_SAIDA-CH_ACESSO  = WA_ZTRT0001-CH_ACESSO.

    WA_SAIDA-PW_AC      = WA_ZTRT0001-PW_AC.
    WA_SAIDA-PW_CC      = WA_ZTRT0001-PW_CC.
    WA_SAIDA-OBSERVACAO = WA_ZTRT0001-OBSERVACAO.
    WA_SAIDA-USNAM      = WA_ZTRT0001-USNAM.
    WA_SAIDA-DT_ATUAL   = WA_ZTRT0001-DT_ATUAL.
    WA_SAIDA-HORA_ATUAL = WA_ZTRT0001-HORA_ATUAL.


    WA_SAIDA-EDITAR  = ICON_DISPLAY.
    WA_SAIDA-EXCLUIR = ICON_INCOMPLETE.

    APPEND WA_SAIDA TO IT_SAIDA.

  ENDLOOP.

  IF NOT ( IT_SAIDA[] IS INITIAL )  AND ( VAR_FILTRO EQ 'X' ).
    CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  ADICIONAR_CONTA
*&---------------------------------------------------------------------*
FORM ADICIONAR_CONTA.

  DATA: WA_INSR     TYPE ZTRT0001.
  DATA: WA_VERIFICA TYPE ZTRT0001.
  DATA: WA_STRING   TYPE STRING.
  DATA: IDENT       TYPE NUMC10.

  IF ( P_BUKRS IS INITIAL ).
    MESSAGE W899(FI) DISPLAY LIKE 'W' WITH 'Informar Empresa.'.
  ELSEIF ( P_HBKID IS INITIAL ).
    MESSAGE W899(FI) DISPLAY LIKE 'W' WITH 'Informar Bco. Empresa'.
  ELSE.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR = '01'
        OBJECT      = 'ZID_BC'
      IMPORTING
        NUMBER      = IDENT.

    IF ( SY-SUBRC EQ 0 ).

      WA_INSR-IDENT           = IDENT.
      WA_INSR-BUKRS           = P_BUKRS.
      WA_INSR-HBKID           = P_HBKID.
      WA_INSR-BANKK           = P_BANKL.
      WA_INSR-BANKA           = P_BANKN.

      CLEAR: WA_STRING.
      WA_STRING = P_CHAVE.
      CALL METHOD CL_HTTP_UTILITY=>IF_HTTP_UTILITY~ENCODE_BASE64
        EXPORTING
          UNENCODED = WA_STRING
        RECEIVING
          ENCODED   = WA_INSR-CH_ACESSO.

      CLEAR: WA_STRING.
      WA_STRING = P_SENHA_AC.

      CALL METHOD CL_HTTP_UTILITY=>IF_HTTP_UTILITY~ENCODE_BASE64
        EXPORTING
          UNENCODED = WA_STRING
        RECEIVING
          ENCODED   = WA_INSR-PW_AC.

      CLEAR: WA_STRING.
      WA_STRING = P_SENHA_CC.

      CALL METHOD CL_HTTP_UTILITY=>IF_HTTP_UTILITY~ENCODE_BASE64
        EXPORTING
          UNENCODED = WA_STRING
        RECEIVING
          ENCODED   = WA_INSR-PW_CC.

      WA_INSR-OBSERVACAO  = P_OBS.
      WA_INSR-USNAM      = SY-UNAME.
      WA_INSR-DT_ATUAL   = SY-DATUM.
      WA_INSR-HORA_ATUAL = SY-UZEIT.

      INSERT INTO ZTRT0001 VALUES WA_INSR.

      IF ( SY-SUBRC EQ 0 ).
        PERFORM: LIMPAR_CAMPOS.
        PERFORM: SELECIONA_DADOS.
        PERFORM: GRAVAR_LOG USING WA_INSR
                                  'G'.
        PERFORM: ENVIAR_EMAIL USING WA_INSR
                               'Conta cadastrada - Contr. de Chave de Bancária'
                               'C'.


        CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY .
        LEAVE TO SCREEN 0100.
        MESSAGE W899(FI) DISPLAY LIKE 'S' WITH 'Registro Gravado com Sucesso'.
      ELSE.

        SELECT SINGLE * FROM ZTRT0001 INTO WA_VERIFICA WHERE BUKRS EQ P_BUKRS
                                                         AND HBKID EQ P_HBKID.

        IF ( SY-SUBRC EQ 0 ).
          MESSAGE W899(FI) DISPLAY LIKE 'W' WITH 'Registro já gravado.'.
        ELSE.
          MESSAGE W899(FI) DISPLAY LIKE 'W' WITH 'Registro não gravado.'.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " ADICIONAR_CONTA
*&---------------------------------------------------------------------*
*&      Form  GRAVAR_LOG
*&---------------------------------------------------------------------*
FORM GRAVAR_LOG  USING    WA_INSR STRUCTURE ZTRT0001
                          P_TIPO  TYPE C.

  DATA: WA_INSR_LOG TYPE ZTRT0002.

  IF NOT ( WA_INSR IS INITIAL ).

    WA_INSR_LOG-DT_ATUAL    = SY-DATUM.
    WA_INSR_LOG-HORA_ATUAL  = SY-UZEIT.
    WA_INSR_LOG-BUKRS       = WA_INSR-BUKRS.
    WA_INSR_LOG-HBKID       = WA_INSR-HBKID.
    WA_INSR_LOG-CH_ACESSO   = WA_INSR-CH_ACESSO.
    WA_INSR_LOG-PW_AC       = WA_INSR-PW_AC.
    WA_INSR_LOG-PW_CC       = WA_INSR-PW_CC.
    WA_INSR_LOG-USNAM       = SY-UNAME.
    WA_INSR_LOG-OBSERVACAO  = WA_INSR-OBSERVACAO.
    WA_INSR_LOG-ACAO        = P_TIPO.
    WA_INSR_LOG-ID_LOG      = WA_INSR-IDENT.

    INSERT INTO ZTRT0002 VALUES WA_INSR_LOG.

  ENDIF.

ENDFORM.                    " GRAVAR_LOG

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_INFORMACOES
*&---------------------------------------------------------------------*
FORM ATUALIZAR_INFORMACOES .

  DATA: WA_T012  TYPE T012,
        WA_BNKA  TYPE BNKA,
        WA_T012K TYPE T012K.

  IF ( P_BUKRS IS INITIAL ).
    MESSAGE W899(FI) DISPLAY LIKE 'W' WITH 'Informar Empresa.'.
  ELSEIF ( P_HBKID IS INITIAL ).
    MESSAGE W899(FI) DISPLAY LIKE 'W' WITH 'Informar Bco. Empresa.'.
  ELSE.

    SELECT SINGLE * FROM T012 INTO WA_T012 WHERE BUKRS EQ P_BUKRS
                                             AND HBKID EQ P_HBKID.
    IF ( SY-SUBRC EQ 0 ).

      P_BANKL = WA_T012-BANKL.

      SELECT SINGLE * FROM BNKA INTO WA_BNKA WHERE BANKL EQ WA_T012-BANKL.
      P_BANKA = WA_BNKA-BANKA.

      SELECT SINGLE * FROM T012K INTO WA_T012K WHERE BUKRS EQ P_BUKRS
                                                 AND HBKID EQ P_HBKID.
      P_BANKN = WA_T012K-BANKN.
    ENDIF.
  ENDIF.

ENDFORM.                    " ATUALIZAR_INFORMACOES
*&---------------------------------------------------------------------*
*&      Form  ALTERAR_CONTA
*&---------------------------------------------------------------------*
FORM ALTERAR_CONTA .

  DATA: WA_STRING TYPE STRING,
        WA_IDENT TYPE ZTRT0001-IDENT.

  IF NOT ( P_BUKRS IS INITIAL ) AND NOT ( P_HBKID IS INITIAL ) AND ( SY-UCOMM EQ 'BTN_ALTERAR' ).

    CLEAR: WA_STRING.
    WA_STRING = P_CHAVE.
    CALL METHOD CL_HTTP_UTILITY=>IF_HTTP_UTILITY~ENCODE_BASE64
      EXPORTING
        UNENCODED = WA_STRING
      RECEIVING
        ENCODED   = P_CHAVE.

    CLEAR: WA_STRING.
    WA_STRING = P_SENHA_AC.

    CALL METHOD CL_HTTP_UTILITY=>IF_HTTP_UTILITY~ENCODE_BASE64
      EXPORTING
        UNENCODED = WA_STRING
      RECEIVING
        ENCODED   = P_SENHA_AC.

    CLEAR: WA_STRING.
    WA_STRING = P_SENHA_CC.

    CALL METHOD CL_HTTP_UTILITY=>IF_HTTP_UTILITY~ENCODE_BASE64
      EXPORTING
        UNENCODED = WA_STRING
      RECEIVING
        ENCODED   = P_SENHA_CC.

    UPDATE ZTRT0001
              SET
                  BANKK      = P_BANKL
                  BANKA      = P_BANKN
                  CH_ACESSO  = P_CHAVE
                  PW_AC      = P_SENHA_AC
                  PW_CC      = P_SENHA_CC
                  USNAM      = SY-UNAME
                  DT_ATUAL   = SY-DATUM
                  HORA_ATUAL = SY-UZEIT
                  OBSERVACAO = P_OBS
    WHERE BUKRS EQ P_BUKRS
      AND HBKID EQ P_HBKID
      AND IDENT EQ WA_ZTRT0001_AUX-IDENT.

    IF ( SY-SUBRC EQ 0 ).
      CLEAR: WA_IDENT.
      WA_IDENT = WA_ZTRT0001_AUX-IDENT.
      CLEAR: WA_ZTRT0001_AUX.

      WA_ZTRT0001_AUX-BUKRS      = P_BUKRS.
      WA_ZTRT0001_AUX-HBKID      = P_HBKID.
      WA_ZTRT0001_AUX-CH_ACESSO  = P_CHAVE.
      WA_ZTRT0001_AUX-PW_AC      = P_SENHA_AC.
      WA_ZTRT0001_AUX-PW_CC      = P_SENHA_CC.
      WA_ZTRT0001_AUX-OBSERVACAO = P_OBS.
      WA_ZTRT0001_AUX-IDENT      = WA_IDENT.

      PERFORM: GRAVAR_LOG USING WA_ZTRT0001_AUX
                                'A'.
      CLEAR: WA_IDENT.
      PERFORM: ENVIAR_EMAIL USING WA_ZTRT0001_AUX
                             'Conta Alterada - Contr. de Chave de Bancária'
                             'A'.


      PERFORM: SELECIONA_DADOS .
      PERFORM: LIMPAR_CAMPOS.
      CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY .
      REFRESH IT_ZTRT0002[].
      CALL METHOD WA_ALV_LOG->REFRESH_TABLE_DISPLAY.
      CLEAR: WA_EDIT.

      LEAVE TO SCREEN 0100.
    ENDIF.
  ENDIF.

ENDFORM.                    " ALTERAR_CONTA
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_FILTRO
*&---------------------------------------------------------------------*
FORM SELECIONAR_FILTRO .

  REFRESH: IT_ZTRT0001.
  CLEAR: IT_SAIDA[].


  IF NOT ( P_EMPRESA IS INITIAL ) AND ( P_BCO_EMPRESA IS INITIAL ).

*---> 01/06/2023 - Migração S4 - JS
*   SELECT BUKRS HBKID CH_ACESSO PW_AC PW_CC USNAM DT_ATUAL HORA_ATUAL OBSERVACAO IDENT
    SELECT BUKRS
           HBKID
           BANKK
           BANKA
           CH_ACESSO
           PW_AC
           PW_CC
           USNAM
           DT_ATUAL
           HORA_ATUAL
           OBSERVACAO
           IDENT
*<--- 01/06/2023 - Migração S4 - JS
      FROM ZTRT0001
      INTO TABLE IT_ZTRT0001
    WHERE BUKRS EQ P_EMPRESA.


  ELSEIF ( P_EMPRESA IS INITIAL ) AND ( NOT P_BCO_EMPRESA IS INITIAL ).
*---> 01/06/2023 - Migração S4 - JS
*    SELECT BUKRS HBKID CH_ACESSO PW_AC PW_CC USNAM DT_ATUAL HORA_ATUAL OBSERVACAO IDENT
     SELECT     BUKRS
                HBKID
                BANKK
                BANKA
                CH_ACESSO
                PW_AC
                PW_CC
                USNAM
                DT_ATUAL
                HORA_ATUAL
                OBSERVACAO
                IDENT
*<--- 01/06/2023 - Migração S4 - JS
      FROM ZTRT0001
      INTO TABLE IT_ZTRT0001
    WHERE HBKID EQ P_BCO_EMPRESA.

  ELSEIF ( NOT P_EMPRESA IS INITIAL ) AND ( NOT P_BCO_EMPRESA IS INITIAL ).

*---> 01/06/2023 - Migração S4 - JS
*    SELECT BUKRS HBKID CH_ACESSO PW_AC PW_CC USNAM DT_ATUAL HORA_ATUAL OBSERVACAO IDENT
     SELECT   BUKRS
              HBKID
              BANKK
              BANKA
              CH_ACESSO
              PW_AC
              PW_CC
              USNAM
              DT_ATUAL
              HORA_ATUAL
              OBSERVACAO
              IDENT
*<--- 01/06/2023 - Migração S4 - JS
      FROM ZTRT0001
      INTO TABLE IT_ZTRT0001
    WHERE BUKRS EQ P_EMPRESA
      AND HBKID EQ P_BCO_EMPRESA.

  ENDIF.

  LOOP AT IT_ZTRT0001 INTO WA_ZTRT0001.

    WA_SAIDA-BUKRS      = WA_ZTRT0001-BUKRS.
    WA_SAIDA-HBKID      = WA_ZTRT0001-HBKID.
    WA_SAIDA-CH_ACESSO  = WA_ZTRT0001-CH_ACESSO.

    WA_SAIDA-PW_AC      = WA_ZTRT0001-PW_AC.
    WA_SAIDA-PW_CC      = WA_ZTRT0001-PW_CC.
    WA_SAIDA-OBSERVACAO = WA_ZTRT0001-OBSERVACAO.
    WA_SAIDA-USNAM      = WA_ZTRT0001-USNAM.
    WA_SAIDA-DT_ATUAL   = WA_ZTRT0001-DT_ATUAL.
    WA_SAIDA-HORA_ATUAL = WA_ZTRT0001-HORA_ATUAL.

    WA_SAIDA-EDITAR  = ICON_DISPLAY.
    WA_SAIDA-EXCLUIR = ICON_INCOMPLETE.


    APPEND WA_SAIDA TO IT_SAIDA.

    CLEAR: WA_SAIDA.
  ENDLOOP.

  CALL METHOD WA_ALV->REFRESH_TABLE_DISPLAY .


ENDFORM.                    " SELECIONAR_FILTRO
*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_LOG
*&---------------------------------------------------------------------*
FORM VISUALIZAR_LOG  USING    P_BUKRS TYPE ZTRT0001-BUKRS
                              P_HBKID TYPE ZTRT0001-HBKID
                              P_IDENT TYPE ZTRT0001-IDENT.

  IF NOT ( P_BUKRS IS INITIAL ) AND NOT ( P_HBKID IS INITIAL ).

    SELECT DT_ATUAL HORA_ATUAL BUKRS HBKID CH_ACESSO PW_AC PW_CC USNAM OBSERVACAO ACAO ID_LOG
      FROM ZTRT0002
      INTO TABLE IT_ZTRT0002
    WHERE BUKRS  EQ P_BUKRS
      AND HBKID  EQ P_HBKID
      AND ID_LOG EQ P_IDENT.

    CALL METHOD WA_ALV_LOG->REFRESH_TABLE_DISPLAY.

  ENDIF.

ENDFORM.                    " VISUALIZAR_LOG
*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL
*&---------------------------------------------------------------------*
FORM ENVIAR_EMAIL  USING P_WA      STRUCTURE ZTRT0001
                         P_ASS     TYPE STRING
                         P_TIPO    TYPE C.

  DATA: IT_DESTINATARIO TYPE STANDARD TABLE OF SOMLREC90 WITH HEADER LINE,
        IT_ASSUNTO      TYPE SODOCCHGI1,
        IT_TEXTO        TYPE STANDARD TABLE OF SOLI WITH HEADER LINE.


  DATA:  DIA  TYPE C LENGTH 2,
         MES  TYPE C LENGTH 2,
         ANO  TYPE C LENGTH 4,
         DATA TYPE C LENGTH 10,
         HR   TYPE C LENGTH 2,
         MIN  TYPE C LENGTH 2,
         SEG  TYPE C LENGTH 2,
         HORA TYPE C LENGTH 8.

  DATA: WA_T012  TYPE T012,
        WA_BNKA  TYPE BNKA,
        WA_T012K TYPE T012K.


  DATA: WA_ZMAIL TYPE ZMAIL.

  IF NOT ( P_WA IS INITIAL ).


    REFRESH: IT_DESTINATARIO[], IT_TEXTO[].
    CLEAR: IT_ASSUNTO.


    SELECT SINGLE * FROM ZMAIL INTO WA_ZMAIL WHERE TCODE EQ SY-TCODE.

    IT_DESTINATARIO-REC_TYPE = 'U'.
    IT_DESTINATARIO-RECEIVER = WA_ZMAIL-EMAIL.
    APPEND IT_DESTINATARIO.


    IT_ASSUNTO-OBJ_NAME  = 'Controle de Chave de Banco'.
    IT_ASSUNTO-OBJ_LANGU = SY-LANGU.
    IT_ASSUNTO-OBJ_DESCR = P_ASS.

    IT_TEXTO = '<!DOCTYPE html>'.
    APPEND IT_TEXTO.
    IT_TEXTO = '<html>'.
    APPEND IT_TEXTO.
    IT_TEXTO = '<head>'.
    APPEND IT_TEXTO.
    IT_TEXTO = '<style type="text/css">'.
    APPEND IT_TEXTO.

    "Informações de  Novo
    IT_TEXTO = '#infoNovo table { font-size: 12px; padding: 0px; border-collapse: collapse; border: 1px solid #BEBEBE; }'.
    APPEND IT_TEXTO.
    IT_TEXTO = '#infoNovo th { width: 90px; font-size: 12px; background-color: #66CDAA; color: #191970; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; }'.
    APPEND IT_TEXTO.
    IT_TEXTO = '#infoNovo td { font-size: 12px; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; text-align: center; }'.
    APPEND IT_TEXTO.

    "Informações de Excluir
    IT_TEXTO = '#infoExcluir table { font-size: 12px; padding: 0px; border-collapse: collapse; border: 1px solid #BEBEBE; }'.
    APPEND IT_TEXTO.
    IT_TEXTO = '#infoExcluir th { width: 90px; font-size: 12px; background-color: #EE3B3B; color: #FFFFFF; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; }'.
    APPEND IT_TEXTO.
    IT_TEXTO = '#infoExcluir td { font-size: 12px; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; text-align: center; }'.
    APPEND IT_TEXTO.

    "Informações Alterar
    IT_TEXTO = '#infoAlterar table { font-size: 12px; padding: 0px; border-collapse: collapse; border: 1px solid #BEBEBE; }'.
    APPEND IT_TEXTO.
    IT_TEXTO = '#infoAlterar th { width: 90px; font-size: 12px; background-color: #EEE9E9; color: #191970; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; }'.
    APPEND IT_TEXTO.
    IT_TEXTO = '#infoAlterar td { font-size: 12px; font-style: "Tahoma, Geneva, sans-serif"; border: 1px solid #BEBEBE; text-align: center; }'.
    APPEND IT_TEXTO.

    IT_TEXTO = '</style>'.
    APPEND IT_TEXTO.
    IT_TEXTO = '</head>'.

    APPEND IT_TEXTO.
    IT_TEXTO = '<body lang="pt-br">'.
    APPEND IT_TEXTO.
    CLEAR: DATA, DIA, MES, ANO, HR, MIN, SEG, HORA.

    DIA = SY-DATUM+6(2).
    MES = SY-DATUM+4(2).
    ANO = SY-DATUM(4).
    CONCATENATE DIA '/' MES '/' ANO INTO DATA.

    HR  = SY-UZEIT(2).
    MIN = SY-UZEIT+2(2).
    SEG = SY-UZEIT+4(2).

    CONCATENATE HR ':' MIN ':' SEG INTO HORA.
    CONCATENATE '<div id="info"> Informações do Controle de Acesso Bancário' DATA '-' HORA '</div>' INTO IT_TEXTO SEPARATED BY SPACE.

    APPEND IT_TEXTO.

    CASE P_TIPO.

      WHEN: 'C'.
        IT_TEXTO = '<div id="infoNovo">'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<table>'.
        APPEND IT_TEXTO.

        " Cabeçalho
        IT_TEXTO = '<tr>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Empresa</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Bco. Empresa</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Bco. Agência</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Banco</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Cont. Corrente</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Usuário</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Data</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Hora</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '</tr>'.
        APPEND IT_TEXTO.

        " Informações.

        IT_TEXTO = '<tr>'.
        APPEND IT_TEXTO.


        CONCATENATE '<td>' P_WA-BUKRS '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        CONCATENATE '<td>' P_WA-HBKID '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        SELECT SINGLE * FROM T012 INTO WA_T012 WHERE BUKRS EQ P_WA-BUKRS
                                                 AND HBKID EQ P_WA-HBKID.


        CONCATENATE '<td>' WA_T012-BANKL  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        SELECT SINGLE * FROM BNKA INTO WA_BNKA WHERE BANKL EQ WA_T012-BANKL.

        CONCATENATE '<td>' WA_BNKA-BANKA  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        SELECT SINGLE * FROM T012K INTO WA_T012K WHERE BUKRS EQ P_WA-BUKRS
                                                   AND HBKID EQ P_WA-HBKID.

        CONCATENATE '<td>' WA_T012K-BANKN  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        CONCATENATE '<td>' SY-UNAME  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        CLEAR: DATA, DIA, MES, ANO, HR, MIN, SEG, HORA.

        DIA = SY-DATUM+6(2).
        MES = SY-DATUM+4(2).
        ANO = SY-DATUM(4).
        CONCATENATE DIA '/' MES '/' ANO INTO DATA.

        CONCATENATE '<td>' DATA  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        HR  = SY-UZEIT(2).
        MIN = SY-UZEIT+2(2).
        SEG = SY-UZEIT+4(2).

        CONCATENATE HR ':' MIN ':' SEG INTO HORA.

        CONCATENATE '<td>' HORA  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        IT_TEXTO = '</tr>'.
        APPEND IT_TEXTO.



      WHEN: 'E'.

        IT_TEXTO = '<div id="infoExcluir">'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<table>'.
        APPEND IT_TEXTO.

        " Cabeçalho
        IT_TEXTO = '<tr>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Empresa</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Bco. Empresa</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Bco. Agência</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Banco</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Cont. Corrente</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Usuário</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Data</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Hora</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '</tr>'.
        APPEND IT_TEXTO.

        " Informações.

        IT_TEXTO = '<tr>'.
        APPEND IT_TEXTO.


        CONCATENATE '<td>' P_WA-BUKRS '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        CONCATENATE '<td>' P_WA-HBKID '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        SELECT SINGLE * FROM T012 INTO WA_T012 WHERE BUKRS EQ P_WA-BUKRS
                                                 AND HBKID EQ P_WA-HBKID.


        CONCATENATE '<td>' WA_T012-BANKL  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        SELECT SINGLE * FROM BNKA INTO WA_BNKA WHERE BANKL EQ WA_T012-BANKL.

        CONCATENATE '<td>' WA_BNKA-BANKA  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        SELECT SINGLE * FROM T012K INTO WA_T012K WHERE BUKRS EQ P_WA-BUKRS
                                                   AND HBKID EQ P_WA-HBKID.

        CONCATENATE '<td>' WA_T012K-BANKN  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        CONCATENATE '<td>' SY-UNAME  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        CLEAR: DATA, DIA, MES, ANO, HR, MIN, SEG, HORA.

        DIA = SY-DATUM+6(2).
        MES = SY-DATUM+4(2).
        ANO = SY-DATUM(4).
        CONCATENATE DIA '/' MES '/' ANO INTO DATA.

        CONCATENATE '<td>' DATA  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        HR  = SY-UZEIT(2).
        MIN = SY-UZEIT+2(2).
        SEG = SY-UZEIT+4(2).

        CONCATENATE HR ':' MIN ':' SEG INTO HORA.

        CONCATENATE '<td>' HORA  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        IT_TEXTO = '</tr>'.
        APPEND IT_TEXTO.

      WHEN: 'A'.

        IT_TEXTO = '<div id="infoAlterar">'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<table>'.
        APPEND IT_TEXTO.

        " Cabeçalho
        IT_TEXTO = '<tr>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Empresa</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Bco. Empresa</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Bco. Agência</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Banco</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Cont. Corrente</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Usuário</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Data</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '<th>Hora</th>'.
        APPEND IT_TEXTO.

        IT_TEXTO = '</tr>'.
        APPEND IT_TEXTO.

        " Informações.

        IT_TEXTO = '<tr>'.
        APPEND IT_TEXTO.


        CONCATENATE '<td>' P_WA-BUKRS '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        CONCATENATE '<td>' P_WA-HBKID '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        SELECT SINGLE * FROM T012 INTO WA_T012 WHERE BUKRS EQ P_WA-BUKRS
                                                 AND HBKID EQ P_WA-HBKID.


        CONCATENATE '<td>' WA_T012-BANKL  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        SELECT SINGLE * FROM BNKA INTO WA_BNKA WHERE BANKL EQ WA_T012-BANKL.

        CONCATENATE '<td>' WA_BNKA-BANKA  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        SELECT SINGLE * FROM T012K INTO WA_T012K WHERE BUKRS EQ P_WA-BUKRS
                                                   AND HBKID EQ P_WA-HBKID.

        CONCATENATE '<td>' WA_T012K-BANKN  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        CONCATENATE '<td>' SY-UNAME  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        CLEAR: DATA, DIA, MES, ANO, HR, MIN, SEG, HORA.

        DIA = SY-DATUM+6(2).
        MES = SY-DATUM+4(2).
        ANO = SY-DATUM(4).
        CONCATENATE DIA '/' MES '/' ANO INTO DATA.

        CONCATENATE '<td>' DATA  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        HR  = SY-UZEIT(2).
        MIN = SY-UZEIT+2(2).
        SEG = SY-UZEIT+4(2).

        CONCATENATE HR ':' MIN ':' SEG INTO HORA.

        CONCATENATE '<td>' HORA  '</td>' INTO IT_TEXTO.
        APPEND IT_TEXTO.

        IT_TEXTO = '</tr>'.
        APPEND IT_TEXTO.

    ENDCASE.

    IT_TEXTO = '</body>'.
    APPEND IT_TEXTO.
    IT_TEXTO = '</html>'.
    APPEND IT_TEXTO.

    "Enviar E-mail
    CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
      EXPORTING
        DOCUMENT_DATA              = IT_ASSUNTO
        DOCUMENT_TYPE              = 'HTM'
      TABLES
        OBJECT_CONTENT             = IT_TEXTO
        RECEIVERS                  = IT_DESTINATARIO
      EXCEPTIONS
        TOO_MANY_RECEIVERS         = 1
        DOCUMENT_NOT_SENT          = 2
        DOCUMENT_TYPE_NOT_EXIST    = 3
        OPERATION_NO_AUTHORIZATION = 4
        PARAMETER_ERROR            = 5
        X_ERROR                    = 6
        ENQUEUE_ERROR              = 7
        OTHERS                     = 8.

    IF ( SY-SUBRC EQ 0 ).
      COMMIT WORK.
      SUBMIT RSCONN01 WITH MODE = 'INT' AND RETURN.
    ENDIF.
  ENDIF.

ENDFORM.                    " ENVIAR_EMAIL



*&---------------------------------------------------------------------*
*&      Form  LIMPAR_CAMPOS
*&---------------------------------------------------------------------*
FORM LIMPAR_CAMPOS .
  CLEAR: P_BUKRS,
         P_HBKID,
         P_BANKL,
         P_BANKA,
         P_BANKN,
         P_CHAVE,
         P_SENHA_AC,
         P_SENHA_CC,
         P_OBS.

ENDFORM.                    " LIMPAR_CAMPOS



*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV
*&---------------------------------------------------------------------*
FORM CRIAR_ALV .

  DATA: GS_VARIANT_C      TYPE DISVARIANT.


  CREATE OBJECT WA_CONT
    EXPORTING
      CONTAINER_NAME              = 'CONTAINER_P'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  PERFORM: CATALOG_FCAT.

  CREATE OBJECT WA_ALV
    EXPORTING
      I_PARENT          = WA_CONT
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.

  CREATE OBJECT WA_EVENT.
  SET HANDLER:  WA_EVENT->ZM_HANDLE_HOTSPOT FOR WA_ALV.

  CALL METHOD WA_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT
      IS_VARIANT                    = GS_VARIANT_C
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
*&      Form  CATALOG_FCAT
*&---------------------------------------------------------------------*
FORM CATALOG_FCAT .
  PERFORM ALV_PREENCHE_CAT USING:
        'EDITAR'     'Editar'           '8'  'X' ''  ''  '' 'C',
        'EXCLUIR'    'Excluir'          '8'  'X' ''  ''  '' 'C',
        'BUKRS'      'Empresa'          '8'  ''  ''  ''  '' '',
        'HBKID'      'Bco. Empresa'     '12' ''  ''  ''  '' '',
        'CH_ACESSO'  'Chave Acesso'     '20' ''  ''  ''  '' '',
        'PW_AC'      'Senha Acesso'     '20' ''  ''  ''  '' '',
        'PW_CC'      'Senha Conta'      '20' ''  ''  ''  '' '',
        'OBSERVACAO' 'Observação'       '50' ''  ''  ''  '' '',
        'USNAM'      'Usuário'          '10' ''  ''  ''  '' '',
        'DT_ATUAL'   'Data do Registro' '15' ''  ''  ''  '' '',
        'HORA_ATUAL' 'Hora do Registro' '15' ''  ''  ''  '' ''.

ENDFORM.                    " CATALOG_FCAT
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
                              P_JUST  TYPE C.

  DATA: WL_FCAT TYPE LVC_S_FCAT.

  WL_FCAT-TABNAME   = 'IT_SAIDA'.
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

  APPEND WL_FCAT TO IT_FCAT.
ENDFORM.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_LOG
*&---------------------------------------------------------------------*
FORM CRIAR_ALV_LOG .


  DATA:


        GS_VARIANT_C   TYPE DISVARIANT.


  CREATE OBJECT WA_CONT_LOG
    EXPORTING
      CONTAINER_NAME              = 'CONTAINER_LOG'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  PERFORM: CATALOG_FCAT_LOG.

  CREATE OBJECT WA_ALV_LOG
    EXPORTING
      I_PARENT          = WA_CONT_LOG
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.

  CALL METHOD WA_ALV_LOG->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT                     = WA_LAYOUT_LOG
      IS_VARIANT                    = GS_VARIANT_C
      I_SAVE                        = 'A'
    CHANGING
      IT_OUTTAB                     = IT_ZTRT0002[]
      IT_FIELDCATALOG               = IT_FCAT_LOG
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.

ENDFORM.                    " CRIAR_ALV_LOG
*&---------------------------------------------------------------------*
*&      Form  CATALOG_FCAT_LOG
*&---------------------------------------------------------------------*
FORM CATALOG_FCAT_LOG .
  PERFORM ALV_PREENCHE_CAT_LOG USING:

      'BUKRS'       'Empresa'          '13'   ''  ''  ''  '' '',
      'HBKID'       'Bco. Empresa'     '20'   ''  ''  ''  '' '',
      'CH_ACESSO'   'Chave de Acesso'  '20'   ''  ''  ''  '' '',
      'PW_AC'       'Senha Acesso'     '20'   ''  ''  ''  '' '',
      'PW_CC'       'Senha Conta'      '20'   ''  ''  ''  '' '',
      'USNAM'       'Usuário'          '10'   '' ''   ''  '' '',
      'ACAO'        'Ação'             '2'    '' ''   ''  '' '',
      'DT_ATUAL'    'Data'             '10'   '' ''   ''  '' '',
      'HORA_ATUAL'  'Hora'             '10'   '' ''   ''  '' '',
      'OBSERVACAO'  'Observação'       '30'   '' ''   ''  '' ''.



ENDFORM.                    " CATALOG_FCAT_LOG
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT_LOG
*&---------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT_LOG  USING   P_CAMPO TYPE C
                                   P_DESC  TYPE C
                                   P_TAM   TYPE C
                                   P_HOT   TYPE C
                                   P_ZERO  TYPE C
                                   P_SUM   TYPE C
                                   P_COR   TYPE C
                                   P_JUST  TYPE C.


  DATA: WL_FCAT TYPE LVC_S_FCAT.

  CLEAR: WL_FCAT.

  WL_FCAT-TABNAME   = 'IT_SAIDA'.
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

  APPEND WL_FCAT TO IT_FCAT_LOG.

ENDFORM.                    " ALV_PREENCHE_CAT_LOG
