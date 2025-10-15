*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 20/02/2013                                              &*
*& Descrição: Atualização de Saldo Moeda Estrangeira                  &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         07.07.2014                            &*
*&--------------------------------------------------------------------&*

REPORT  ZFIR050.

*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.

INCLUDE <ICON>.
TYPES: BEGIN OF TY_SETLEAF.
        INCLUDE TYPE SETLEAF.
TYPES:  KTOKS TYPE SKA1-KTOKS,
        END OF TY_SETLEAF,


        BEGIN OF TY_BKPF,
          BUKRS TYPE BKPF-BUKRS,
          BELNR TYPE BKPF-BELNR,
          GJAHR TYPE BKPF-GJAHR,
          BUDAT TYPE BKPF-BUDAT,
          STBLG TYPE BKPF-STBLG,
          STJAH TYPE BKPF-STJAH,
        END OF TY_BKPF,

        BEGIN OF TY_SAIDA,
          MARK,
          RACCT       TYPE FAGLFLEXT-RACCT,
          RASSC       TYPE FAGLFLEXT-RASSC,
          TXT50       TYPE SKAT-TXT50,
          KTOKS       TYPE SKA1-KTOKS,
          CURR1       TYPE FAGLFLEXT-HSLVT,
          CURR2       TYPE FAGLFLEXT-KSLVT,
          CURR3       TYPE FAGLFLEXT-OSLVT,
          TX_USD      TYPE ZFIT0082-TX_USD,
          TX_BRL      TYPE ZFIT0082-TX_BRL,
          SALDO_CORR  TYPE FAGLFLEXT-HSLVT,
          SALDO_CORR2 TYPE FAGLFLEXT-HSLVT,
          VLR_AJUST   TYPE FAGLFLEXT-KSLVT,
          VLR_AJUST2  TYPE FAGLFLEXT-KSLVT,
          BELNR       TYPE ZIB_CONTABIL_CHV-BELNR,
          BELNR_EST   TYPE ZIB_CONTABIL_CHV-BELNR,
          OBJ_KEY     TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
          OBJ_KEY_EST TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
          LOG(4),
        END OF TY_SAIDA,

        BEGIN OF TY_SAIDA_EXEC,
          ICON(4),
          RACCT   TYPE FAGLFLEXT-RACCT,
          TXT50   TYPE SKAT-TXT50,
          MSG(80),
        END OF TY_SAIDA_EXEC.

TYPES: BEGIN OF TY_ESTRUTURA.
        INCLUDE TYPE SLIS_FIELDCAT_MAIN.
        INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: TG_SETLEAF    TYPE TABLE OF TY_SETLEAF,
      TG_T882G      TYPE TABLE OF T882G,
      TG_T001       TYPE TABLE OF T001,
      TG_SKA1       TYPE TABLE OF SKA1,
      TG_SKAT       TYPE TABLE OF SKAT,
      TG_SKB1       TYPE TABLE OF SKB1,
      TG_0082       TYPE TABLE OF ZFIT0082,
      TG_0082_AUX   TYPE TABLE OF ZFIT0082,
      TG_FAGLFLEXT  TYPE TABLE OF FAGLFLEXT,
      TG_TCURR      TYPE TABLE OF TCURR,
      TG_TCURR_LDAY TYPE TABLE OF TCURR,
      TG_0081       TYPE TABLE OF ZFIT0081,
      TG_ZIB        TYPE TABLE OF ZIB_CONTABIL,
      TG_ZIB_CHV    TYPE TABLE OF ZIB_CONTABIL_CHV,
      TG_ZIB_ERR    TYPE TABLE OF ZIB_CONTABIL_ERR,
      TG_SAIDA      TYPE TABLE OF TY_SAIDA,
      TG_SAIDA_EXEC TYPE TABLE OF TY_SAIDA_EXEC.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WG_SETLEAF     TYPE TY_SETLEAF,
      WG_T882G       TYPE T882G,
      WG_T001        TYPE T001,
      WG_SKA1        TYPE SKA1,
      WG_SKAT        TYPE SKAT,
      WG_SKB1        TYPE SKB1,
      WG_0082        TYPE ZFIT0082,
      WG_FAGLFLEXT   TYPE FAGLFLEXT,
      WG_TCURR       TYPE TCURR,
      WG_TCURR_LDAY  TYPE TCURR,
      WG_0081        TYPE ZFIT0081,
      WL_INPUT_0082  TYPE ZFIT0082,
      WG_ZIB_CHV     TYPE ZIB_CONTABIL_CHV,
      WG_ZIB_ERR     TYPE ZIB_CONTABIL_ERR,
      WG_BKPF_FB08   TYPE TY_BKPF,
      WG_BKPF_FB08_E TYPE TY_BKPF,
      WG_SAIDA       TYPE TY_SAIDA,
      WG_SAIDA_EXEC  TYPE TY_SAIDA_EXEC.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      TABIX        TYPE SY-TABIX,
      T_TOP        TYPE SLIS_T_LISTHEADER,
      LT_SORT      TYPE SLIS_T_SORTINFO_ALV,
      LS_SORT      TYPE SLIS_SORTINFO_ALV.
*----------------------------------------------------------------------*
* Variaveis
*----------------------------------------------------------------------*
DATA: VG_RYEAR            TYPE FAGLFLEXT-RYEAR,
      VG_LAST_DAY         TYPE SY-DATUM,
      VG_FIRST_DAY        TYPE SY-DATUM,
      VG_LAST_DAY_AUX(8),
      VG_LAST_DAY_AUX2(8),
      VG_INIT,
      W_ANSWER(1),
      E_STATUS(1),
      E_MESSA(64).

DATA: BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MSG.

DATA: TI_BDCDATA       TYPE STANDARD TABLE OF BDCDATA ,   "Guarda o mapeamento
      WA_BDCDATA       LIKE LINE OF TI_BDCDATA,
      T_MESSTAB        TYPE TABLE OF BDCMSGCOLL,
      VOBJ_KEY         TYPE ZIB_CONTABIL_ERR-OBJ_KEY,
      WL_MESSAGE       TYPE PMST_RAW_MESSAGE,
      WG_DOCUMENTO(10),
      WL_MODE(1).

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: C_X(1) TYPE C VALUE 'X'.
*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE ACAO.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      FUNCTIONCODE           = &1
    EXCEPTIONS
      FUNCTION_NOT_SUPPORTED = 1.

END-OF-DEFINITION.
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS ON_BUTTON_CLICK FOR EVENT BUTTON_CLICK OF CL_GUI_ALV_GRID
      IMPORTING ES_COL_ID ES_ROW_NO.
  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
* TELA DE SELECAO.
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS FOR WG_0082-BUKRS OBLIGATORY NO INTERVALS NO-EXTENSION,
                S_MES   FOR WG_0082-MES_ANO NO INTERVALS NO-EXTENSION OBLIGATORY,
                S_SAKNR FOR WG_0082-SAKNR.

SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_PROC RADIOBUTTON GROUP G1,
            P_REVE RADIOBUTTON GROUP G1,
            P_VISU RADIOBUTTON GROUP G1.

SELECTION-SCREEN: END OF BLOCK B2.
SELECTION-SCREEN: END OF BLOCK B1.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD ON_BUTTON_CLICK.
    DATA: TL_TEXTO   TYPE CATSXT_LONGTEXT_ITAB,
          WL_TEXTO   TYPE LINE OF CATSXT_LONGTEXT_ITAB,
          WL_REPROC,
          IT_ZIB_ERR TYPE TABLE OF ZIB_CONTABIL_ERR,
          WL_OBJ_KEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY,
          WL_TEXT    TYPE SYTITLE.

    REFRESH: TL_TEXTO.
    CLEAR: WL_TEXTO.
    IF ES_COL_ID EQ 'LOG'.
      READ TABLE TG_SAIDA INTO WG_SAIDA INDEX ES_ROW_NO-ROW_ID.
**       Contabilização
      IF WG_SAIDA-OBJ_KEY IS NOT INITIAL.
        SELECT *
          FROM ZIB_CONTABIL_ERR
          INTO TABLE IT_ZIB_ERR
          WHERE OBJ_KEY EQ WG_SAIDA-OBJ_KEY.
        LOOP AT IT_ZIB_ERR INTO WG_ZIB_ERR.

          WL_TEXTO = WG_ZIB_ERR-MESSAGE.

          APPEND WL_TEXTO TO TL_TEXTO.
          CLEAR: WL_TEXTO.
        ENDLOOP.
        IF TL_TEXTO[] IS NOT INITIAL.
          WL_TEXT = TEXT-022.
          CALL FUNCTION 'ZCATSXT_SIMPLE_TEXT_EDITOR2'
            EXPORTING
*             IM_TITLE        = 'Contabilização'           "" Título
              IM_TITLE        = WL_TEXT           "" Título
              IM_DISPLAY_MODE = C_X
            IMPORTING
              E_REPROC        = WL_REPROC
            CHANGING
              CH_TEXT         = TL_TEXTO.
          IF WL_REPROC IS NOT INITIAL.
            DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY EQ WG_SAIDA-OBJ_KEY.
            DELETE FROM ZIB_CONTABIL_CHV WHERE OBJ_KEY EQ WG_SAIDA-OBJ_KEY.
            DELETE FROM ZIB_CONTABIL     WHERE OBJ_KEY EQ WG_SAIDA-OBJ_KEY.

            WL_OBJ_KEY = WG_SAIDA-OBJ_KEY.
            PERFORM GERA_CONTABIL USING WG_SAIDA
                                        SPACE       " estorna
                                  CHANGING WL_OBJ_KEY.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "ON_BUTTON_CLICK
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

START-OF-SELECTION.
  PERFORM SELECIONAR_DADOS.

  IF P_VISU IS INITIAL.


    IF P_REVE IS NOT INITIAL.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          I_BUKRS  = S_BUKRS-LOW
          I_DATA   = VG_FIRST_DAY
*         I_DEP_RESP = VG_DEPTO
        IMPORTING
          E_STATUS = E_STATUS
          E_MESSA  = E_MESSA
        EXCEPTIONS
          ERROR    = 1
          OTHERS   = 2.
    ELSE.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          I_BUKRS  = S_BUKRS-LOW
          I_DATA   = VG_LAST_DAY
*         I_DEP_RESP = VG_DEPTO
        IMPORTING
          E_STATUS = E_STATUS
          E_MESSA  = E_MESSA
        EXCEPTIONS
          ERROR    = 1
          OTHERS   = 2.
    ENDIF.
    IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    IF  E_STATUS = 'E'.
      MESSAGE E398(00) WITH E_MESSA.
      RETURN.
    ENDIF.
  ENDIF.

  PERFORM INICIAR_VARIAVEIS.
  PERFORM ORGANIZACAO_DADOS.
  PERFORM IMPRIMIR_DADOS.


*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS.
  CLEAR: WG_T001.

  READ TABLE TG_T001 INTO WG_T001 INDEX 1.
  IF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR WG_T001-LAND1 EQ 'CH'.
    PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-003.
  ELSEIF WG_T001-LAND1 EQ 'AR'
      OR WG_T001-LAND1 EQ 'PY'.
    PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-004.
  ENDIF.
ENDFORM.                    " INICIAR_VARIAVES
*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XUSER_COMMAND USING UCOMM LIKE SY-UCOMM
                         SELFIELD TYPE KKBLO_SELFIELD.
  DATA: TL_ZIB     TYPE TABLE OF ZIB_CONTABIL WITH HEADER LINE,
        TL_ZIB_CHV TYPE TABLE OF ZIB_CONTABIL_CHV WITH HEADER LINE,
        TL_ZIB_ERR TYPE TABLE OF ZIB_CONTABIL_ERR WITH HEADER LINE,
        TL_SAIDA   TYPE TABLE OF TY_SAIDA,
        WL_OBJ_KEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY.

  REFRESH: TG_SAIDA_EXEC, TL_ZIB, TL_ZIB_CHV, TL_ZIB_ERR, TL_SAIDA.
  CLEAR:WL_OBJ_KEY.

  TL_SAIDA[]  = TG_SAIDA[].
  DELETE TL_SAIDA WHERE OBJ_KEY IS INITIAL.
  IF TL_SAIDA[] IS NOT INITIAL.
    SELECT *
      FROM ZIB_CONTABIL
      INTO TABLE TL_ZIB
       FOR ALL ENTRIES IN TL_SAIDA
       WHERE OBJ_KEY EQ TL_SAIDA-OBJ_KEY.
  ENDIF.


  IF TL_ZIB[] IS NOT INITIAL.
    SELECT *
     FROM ZIB_CONTABIL_CHV
     INTO TABLE TL_ZIB_CHV
      FOR ALL ENTRIES IN TL_ZIB
      WHERE OBJ_KEY EQ TL_ZIB-OBJ_KEY.

    SELECT *
     FROM ZIB_CONTABIL_ERR
     INTO TABLE TL_ZIB_ERR
      FOR ALL ENTRIES IN TL_ZIB
      WHERE OBJ_KEY EQ TL_ZIB-OBJ_KEY.

  ENDIF.

  SORT: TL_ZIB     BY OBJ_KEY,
        TL_ZIB_CHV BY OBJ_KEY,
        TL_ZIB_ERR BY OBJ_KEY.

  CASE UCOMM.
    WHEN 'GERA' OR 'GERA2'.
      LOOP AT TG_SAIDA INTO WG_SAIDA
        WHERE MARK IS NOT INITIAL.
        IF WG_SAIDA-OBJ_KEY IS NOT INITIAL AND WG_SAIDA-BELNR IS INITIAL.
          READ TABLE TL_ZIB WITH KEY OBJ_KEY = WG_SAIDA-OBJ_KEY BINARY SEARCH.
          IF TL_ZIB-RG_ATUALIZADO EQ 'N'.
            CLEAR: WG_SAIDA_EXEC.
            WG_SAIDA_EXEC-ICON   = ICON_YELLOW_LIGHT.
            WG_SAIDA_EXEC-RACCT  = WG_SAIDA-RACCT.
            WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
            WG_SAIDA_EXEC-MSG    = TEXT-M01 .
            APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
            CONTINUE.
          ELSEIF TL_ZIB-RG_ATUALIZADO EQ 'S'.
            READ TABLE TL_ZIB_CHV WITH KEY OBJ_KEY = WG_SAIDA-OBJ_KEY BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              CLEAR: WG_SAIDA_EXEC.
              WG_SAIDA_EXEC-ICON   = ICON_GREEN_LIGHT.
              WG_SAIDA_EXEC-RACCT  = WG_SAIDA-RACCT.
              WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
              WG_SAIDA_EXEC-MSG    = TEXT-M02 .
              APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
              ACAO '&ATUAL'.
              CONTINUE.
            ELSE.
              READ TABLE TL_ZIB_ERR  WITH KEY OBJ_KEY = WG_SAIDA-OBJ_KEY BINARY SEARCH.
              IF SY-SUBRC IS INITIAL.
                CLEAR: WG_SAIDA_EXEC.
                WG_SAIDA_EXEC-ICON   = ICON_RED_LIGHT.
                WG_SAIDA_EXEC-RACCT  = WG_SAIDA-RACCT.
                WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
                WG_SAIDA_EXEC-MSG    = TEXT-M03 .
                APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.

                CALL FUNCTION 'POPUP_TO_CONFIRM'
                  EXPORTING
                    TEXT_QUESTION         = TEXT-M04
*                   TEXT_BUTTON_1         = 'Sim'(100)
                    TEXT_BUTTON_1         = TEXT-B01
                    ICON_BUTTON_1         = 'ICON_OKAY'
*                   TEXT_BUTTON_2         = 'Não'(101)
                    TEXT_BUTTON_2         = TEXT-B02
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

                IF SY-SUBRC <> 0.
                  MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
                ENDIF.
                IF W_ANSWER = '1'.
                  DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY = WG_SAIDA-OBJ_KEY.
                  DELETE FROM ZIB_CONTABIL     WHERE OBJ_KEY = WG_SAIDA-OBJ_KEY.
                ELSE.
                  ACAO '&ATUAL'.
                  CONTINUE.
                ENDIF.
              ELSE.
                CLEAR: WG_SAIDA_EXEC.
                WG_SAIDA_EXEC-ICON   = ICON_YELLOW_LIGHT.
                WG_SAIDA_EXEC-RACCT  = WG_SAIDA-RACCT.
                WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
                WG_SAIDA_EXEC-MSG    = TEXT-M01 .
                APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
                CONTINUE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        IF WG_SAIDA-BELNR IS NOT INITIAL.
          CLEAR: WG_SAIDA_EXEC.
          WG_SAIDA_EXEC-ICON   = ICON_RED_LIGHT.
          WG_SAIDA_EXEC-RACCT  = WG_SAIDA-RACCT.
          WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
          WG_SAIDA_EXEC-MSG    = TEXT-M05 .
          APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
          CONTINUE.
        ENDIF.
        CLEAR: WL_OBJ_KEY.
        PERFORM GERA_CONTABIL USING WG_SAIDA SPACE CHANGING WL_OBJ_KEY.

        WG_SAIDA-OBJ_KEY = WL_OBJ_KEY.
        CLEAR: WG_SAIDA-OBJ_KEY_EST, WG_SAIDA-BELNR_EST.
        MODIFY TG_SAIDA FROM WG_SAIDA TRANSPORTING OBJ_KEY OBJ_KEY_EST BELNR_EST.
      ENDLOOP.

      SELFIELD-COL_STABLE = C_X.
      SELFIELD-ROW_STABLE = C_X.
      SELFIELD-REFRESH = C_X.

      PERFORM IMPRIMIR_EXEC.
      ACAO '&ATUAL'.
    WHEN 'ESTORNO'
      OR 'ESTORNO2'.

      IF P_PROC IS NOT INITIAL.
        PERFORM ESTORNA_DOCUMENTOS USING 'EST'.
      ELSE.
        PERFORM ESTORNA_DOCUMENTOS USING 'REV'.
      ENDIF.
      SELFIELD-COL_STABLE = C_X.
      SELFIELD-ROW_STABLE = C_X.
      SELFIELD-REFRESH = C_X.
      PERFORM IMPRIMIR_EXEC.
      ACAO '&ATUAL'.

    WHEN '&ALL2'.
      UCOMM = '&ALL'.

    WHEN '&SAL2'.
      UCOMM = '&SAL'.

    WHEN '&IC1'.
* Lê na tabela de saída
      READ TABLE TG_SAIDA INTO WG_SAIDA INDEX SELFIELD-TABINDEX.

      IF SY-SUBRC EQ 0.

        IF SELFIELD-FIELDNAME = 'BELNR'.
          IF WG_SAIDA-BELNR IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD WG_SAIDA-BELNR.
            SET PARAMETER ID 'GJR' FIELD WG_SAIDA-OBJ_KEY+16(4).
            SET PARAMETER ID 'BUK' FIELD S_BUKRS-LOW.

            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ELSEIF SELFIELD-FIELDNAME = 'BELNR_EST'.
          IF WG_SAIDA-BELNR_EST IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD WG_SAIDA-BELNR_EST.
            SET PARAMETER ID 'GJR' FIELD WG_SAIDA-OBJ_KEY_EST+16(4).
            SET PARAMETER ID 'BUK' FIELD S_BUKRS-LOW.

            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN '&ATUAL'.
      PERFORM ATUALIZA_SAIDA TABLES TG_SAIDA
                                    TL_ZIB
                                    TL_ZIB_CHV
                                    TL_ZIB_ERR.
  ENDCASE.

  SELFIELD-COL_STABLE = C_X.
  SELFIELD-ROW_STABLE = C_X.
  SELFIELD-REFRESH = C_X.
ENDFORM. "XUSER_COMMAND
*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XPF_STATUS_SET USING E_COMM.                           "#EC CALLED
  DATA: GR_EVENTS TYPE REF TO LCL_EVENT_RECEIVER.
  DATA : LS_SEL_HIDE            TYPE SLIS_SEL_HIDE_ALV.

  DATA: FCODE TYPE TABLE OF SY-UCOMM.
  REFRESH: FCODE.

  DATA: REF1            TYPE REF TO CL_GUI_ALV_GRID,
        TL_FIELDCATALOG TYPE LVC_T_FCAT,
        WL_FIELDCATALOG TYPE LVC_S_FCAT,
        IS_TABLE        TYPE LVC_S_STBL..

  IF VG_INIT IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        ES_SEL_HIDE = LS_SEL_HIDE
        E_GRID      = REF1.

    CALL METHOD REF1->GET_FRONTEND_FIELDCATALOG
      IMPORTING
        ET_FIELDCATALOG = TL_FIELDCATALOG.

    LOOP AT TL_FIELDCATALOG INTO WL_FIELDCATALOG.
      TABIX = SY-TABIX.
      READ TABLE ESTRUTURA INTO WA_ESTRUTURA WITH KEY FIELDNAME = WL_FIELDCATALOG-FIELDNAME.
      IF SY-SUBRC = 0.
        WL_FIELDCATALOG-COL_POS = WA_ESTRUTURA-COL_POS.
        MODIFY TL_FIELDCATALOG FROM WL_FIELDCATALOG INDEX TABIX.
      ENDIF.
    ENDLOOP.
    SORT TL_FIELDCATALOG BY COL_POS.

    READ TABLE TL_FIELDCATALOG INTO WL_FIELDCATALOG
      WITH KEY FIELDNAME = 'LOG'.
    IF SY-SUBRC IS INITIAL.
      WL_FIELDCATALOG-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
*    WL_FIELDCATALOG-EDIT = C_X.

      MODIFY TL_FIELDCATALOG FROM WL_FIELDCATALOG INDEX SY-TABIX TRANSPORTING STYLE EDIT.

      CALL METHOD REF1->SET_FRONTEND_FIELDCATALOG
        EXPORTING
          IT_FIELDCATALOG = TL_FIELDCATALOG.

      IS_TABLE-ROW = 'X'.
      IS_TABLE-COL = 'X'.
      CALL METHOD REF1->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE      = IS_TABLE
          I_SOFT_REFRESH = 'X'.

    ENDIF.

    CREATE OBJECT GR_EVENTS.
    "SET HANDLER GR_EVENTS->ON_BUTTON_CLICK FOR REF1.
    SET HANDLER LCL_EVENT_RECEIVER=>ON_BUTTON_CLICK FOR REF1.
    VG_INIT = C_X.
  ENDIF.

  IF P_PROC IS INITIAL.
    APPEND 'GERA' TO FCODE.
    APPEND 'GERA2' TO FCODE.
  ENDIF.

  IF P_VISU IS NOT INITIAL.
    APPEND 'ESTORNO' TO FCODE.
    APPEND 'ESTORNO2' TO FCODE.
  ENDIF.

  IF P_PROC IS NOT INITIAL.
    READ TABLE TG_T001 INTO WG_T001
      WITH KEY BUKRS = S_BUKRS-LOW.
    IF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR WG_T001-LAND1 EQ 'CH'.
      SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING FCODE.
    ELSEIF WG_T001-LAND1 EQ 'AR'
        OR WG_T001-LAND1 EQ 'PY'.
      SET PF-STATUS 'STANDARD_FULLSCREEN2' EXCLUDING FCODE.
    ENDIF.
  ELSE.
    SET PF-STATUS 'STANDARD_FULLSCREEN3' EXCLUDING FCODE.
  ENDIF.

ENDFORM. "XPF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XTOP_OF_PAGE.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_TOP
      I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM F_CONSTRUIR_CABECALHO USING TYP TEXT.

  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  LS_LINE-TYP = TYP.
  LS_LINE-INFO = TEXT.
  APPEND LS_LINE TO T_TOP.


  LS_LINE-TYP = 'S'.
  IF WG_T001-LAND1 EQ 'BR'.
*    LS_LINE-KEY = 'Empresa:'.
    LS_LINE-KEY = TEXT-001.
    CONCATENATE  WG_T001-BUKRS '-' WG_T001-BUTXT INTO LS_LINE-INFO SEPARATED BY SPACE.
  ELSEIF WG_T001-LAND1 EQ 'AR'
      OR WG_T001-LAND1 EQ 'PY'.
*    LS_LINE-KEY = 'Sociedad:'.
    LS_LINE-KEY = TEXT-002.
    CONCATENATE  WG_T001-BUKRS '-' WG_T001-BUTXT INTO LS_LINE-INFO SEPARATED BY SPACE.
  ENDIF.
  APPEND LS_LINE TO T_TOP.

  IF WG_T001-LAND1 EQ 'BR'.
*    LS_LINE-KEY = 'Mês/Ano:'.
    LS_LINE-KEY = TEXT-005.
    CONCATENATE  S_MES-LOW(2) '/' S_MES-LOW+2(4)  INTO LS_LINE-INFO SEPARATED BY SPACE.
  ELSEIF WG_T001-LAND1 EQ 'AR'
      OR WG_T001-LAND1 EQ 'PY'.
*    LS_LINE-KEY = 'Ejercicio:'.
    LS_LINE-KEY = TEXT-006.
    CONCATENATE  S_MES-LOW(2) '/' S_MES-LOW+2(4)  INTO LS_LINE-INFO SEPARATED BY SPACE.
  ENDIF.
  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DADOS .
  DATA: WL_LAYOUT TYPE SLIS_LAYOUT_ALV.
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT USING 'TG_SAIDA'.

* Ordenar a saida do alv
*  ADD 1 TO LS_SORT-SPOS.
*  LS_SORT-FIELDNAME = 'EBELN'.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-SUBTOT    = 'X'.
*  APPEND LS_SORT TO LT_SORT.
*
*  CLEAR: LS_SORT-SPOS.
*  ADD 1 TO LS_SORT-SPOS.
*  LS_SORT-FIELDNAME = 'EBELP'.
*  LS_SORT-UP        = 'X'.
*  LS_SORT-SUBTOT    = 'X'.
*  APPEND LS_SORT TO LT_SORT.
  IF P_PROC IS NOT INITIAL.
    WL_LAYOUT-BOX_FIELDNAME = 'MARK'.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = SY-REPID
*     is_variant         = gs_variant_c
*     I_CALLBACK_USER_COMMAND           = 'XUSER_COMMAND' "sem 2º click
      IT_FIELDCAT        = ESTRUTURA[]
      IS_LAYOUT          = WL_LAYOUT
      I_SAVE             = 'X'
      IT_EVENTS          = EVENTS
      IS_PRINT           = T_PRINT
      IT_SORT            = LT_SORT
    TABLES
      T_OUTTAB           = TG_SAIDA.



ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEFINIR_EVENTOS.
  IF P_PROC IS NOT INITIAL.
    PERFORM F_CARREGAR_EVENTOS USING:
                                     SLIS_EV_USER_COMMAND 'XUSER_COMMAND', "para tira duplo click
                                     SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                     SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
  ELSE.
    PERFORM F_CARREGAR_EVENTOS USING:
*                                     SLIS_EV_USER_COMMAND 'XUSER_COMMAND', "para tira duplo click
                                     SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                     SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
  ENDIF.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM F_CARREGAR_EVENTOS USING    NAME FORM.
  CLEAR XS_EVENTS.
  XS_EVENTS-NAME = NAME.
  XS_EVENTS-FORM = FORM.
  APPEND XS_EVENTS TO EVENTS.

ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT USING P_TABNAME.
  DATA: WL_CURR1_AUX  TYPE DD03P-SCRTEXT_L,
        WL_CURR2_AUX  TYPE DD03P-SCRTEXT_L,
        WL_CURR3_AUX  TYPE DD03P-SCRTEXT_L,
        WL_SALDO_COR  TYPE DD03P-SCRTEXT_L,
        WL_SALDO_COR2 TYPE DD03P-SCRTEXT_L,
        WL_VLR_AJST   TYPE DD03P-SCRTEXT_L,
        WL_VLR_AJST2  TYPE DD03P-SCRTEXT_L.

  REFRESH ESTRUTURA.
  CLEAR: WG_T001.
  READ TABLE TG_T001 INTO WG_T001 INDEX 1.
  IF P_TABNAME EQ 'TG_SAIDA'.
    READ TABLE TG_T882G INTO WG_T882G
      WITH KEY RBUKRS =  S_BUKRS-LOW
               BINARY SEARCH.

    IF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR WG_T001-LAND1 EQ 'CH'.
*      CONCATENATE 'Saldo' WG_T882G-CURR1 INTO WL_CURR1_AUX SEPARATED BY SPACE.
      CONCATENATE TEXT-007 WG_T882G-CURR1 INTO WL_CURR1_AUX SEPARATED BY SPACE.
*      CONCATENATE 'Saldo' WG_T882G-CURR2 INTO WL_CURR2_AUX SEPARATED BY SPACE.
      CONCATENATE TEXT-007 WG_T882G-CURR2 INTO WL_CURR2_AUX SEPARATED BY SPACE.
      IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
*        CONCATENATE 'Sdo.Corrigido' WG_T882G-CURR1 INTO WL_SALDO_COR SEPARATED BY SPACE.
        CONCATENATE TEXT-008 WG_T882G-CURR1 INTO WL_SALDO_COR SEPARATED BY SPACE.
*        CONCATENATE 'Vlr.Ajuste' WG_T882G-CURR1 INTO WL_VLR_AJST SEPARATED BY SPACE.
        CONCATENATE TEXT-009 WG_T882G-CURR1 INTO WL_VLR_AJST SEPARATED BY SPACE.
      ELSE.
*        CONCATENATE 'Sdo.Corrigido' WG_T882G-CURR2 INTO WL_SALDO_COR SEPARATED BY SPACE.
*        CONCATENATE 'Vlr.Ajuste' WG_T882G-CURR2 INTO WL_VLR_AJST SEPARATED BY SPACE.
        CONCATENATE TEXT-008 WG_T882G-CURR2 INTO WL_SALDO_COR SEPARATED BY SPACE.
        CONCATENATE TEXT-009 WG_T882G-CURR2 INTO WL_VLR_AJST SEPARATED BY SPACE.
      ENDIF.

      PERFORM MONTAR_ESTRUTURA USING:
                1  'FAGLFLEXT'         'RACCT'          'TG_SAIDA' 'RACCT'                   TEXT-A01                   ' ' ,
                1  'FAGLFLEXT'         'RASSC'          'TG_SAIDA' 'RASSC'                   TEXT-A02                   ' ' ,
                2  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   TEXT-A04                   ' ' ,
                3  'SKA1'              'KTOKS'          'TG_SAIDA' 'KTOKS'                   TEXT-A05                   ' ' ,
                4  'FAGLFLEXT'         'HSLVT'          'TG_SAIDA' 'CURR1'                   WL_CURR1_AUX              '15' ,
                5  'FAGLFLEXT'         'KSLVT'          'TG_SAIDA' 'CURR2'                   WL_CURR2_AUX              '15' ,
                6  'ZFIT0082'          'TX_USD'         'TG_SAIDA' 'TX_USD'                  TEXT-A06                  '10' ,
                7  'FAGLFLEXT'         'HSLVT'          'TG_SAIDA' 'SALDO_CORR'              WL_SALDO_COR              '15' ,
                8  'FAGLFLEXT'         'KSLVT'          'TG_SAIDA' 'VLR_AJUST'               WL_VLR_AJST               '15' ,
                9  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDA' 'BELNR'                   TEXT-A07                  ' ' ,
               11  ' '                 ' '              'TG_SAIDA' 'LOG'                     TEXT-A10                  ' ' .
    ELSEIF WG_T001-LAND1 EQ 'AR' OR WG_T001-LAND1 EQ 'PY'.
      CONCATENATE TEXT-007 WG_T882G-CURR1 INTO WL_CURR1_AUX  SEPARATED BY SPACE.
      CONCATENATE TEXT-007 WG_T882G-CURR2 INTO WL_CURR2_AUX  SEPARATED BY SPACE.
      CONCATENATE TEXT-007 WG_T882G-CURR3 INTO WL_CURR3_AUX  SEPARATED BY SPACE.
      CONCATENATE TEXT-010 WG_T882G-CURR2 INTO WL_SALDO_COR  SEPARATED BY SPACE.
      CONCATENATE TEXT-010 WG_T882G-CURR3 INTO WL_SALDO_COR2 SEPARATED BY SPACE.
      CONCATENATE TEXT-009 WG_T882G-CURR2 INTO WL_VLR_AJST   SEPARATED BY SPACE.
      CONCATENATE TEXT-009 WG_T882G-CURR3 INTO WL_VLR_AJST2  SEPARATED BY SPACE.

      PERFORM MONTAR_ESTRUTURA USING:
            1  'FAGLFLEXT'         'RACCT'          'TG_SAIDA' 'RACCT'                   TEXT-A11             ' ' ,
            2  'SKAT'              'TXT50'          'TG_SAIDA' 'TXT50'                   TEXT-A12             ' ' ,
            3  'SKA1'              'KTOKS'          'TG_SAIDA' 'KTOKS'                   TEXT-A13             ' ' ,
            4  'FAGLFLEXT'         'HSLVT'          'TG_SAIDA' 'CURR1'                   WL_CURR1_AUX         '15' ,
            5  'FAGLFLEXT'         'KSLVT'          'TG_SAIDA' 'CURR2'                   WL_CURR2_AUX         '15' ,
            6  'FAGLFLEXT'         'OSLVT'          'TG_SAIDA' 'CURR3'                   WL_CURR3_AUX         '15' ,
            7  'ZFIT0082'          'TX_USD'         'TG_SAIDA' 'TX_USD'                  TEXT-A06             '10' ,
            8  'FAGLFLEXT'         'KSLVT'          'TG_SAIDA' 'VLR_AJUST'               WL_VLR_AJST          '15' ,
            9  'FAGLFLEXT'         'HSLVT'          'TG_SAIDA' 'SALDO_CORR'              WL_SALDO_COR         '15' ,

           10  'ZFIT0082'          'TX_BRL'         'TG_SAIDA' 'TX_BRL'                  TEXT-A09             '10' ,
           11  'FAGLFLEXT'         'KSLVT'          'TG_SAIDA' 'VLR_AJUST2'              WL_VLR_AJST2         '15' ,
           12  'FAGLFLEXT'         'HSLVT'          'TG_SAIDA' 'SALDO_CORR2'             WL_SALDO_COR2        '15' ,

           13  'ZIB_CONTABIL_CHV'  'BELNR'          'TG_SAIDA' 'BELNR'                   TEXT-A07             ' ' ,
           15  ' '                 ' '              'TG_SAIDA' 'LOG'                     TEXT-A10             ' ' .
      "
    ENDIF.
  ELSEIF P_TABNAME EQ 'TG_SAIDA_EXEC'.
    IF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR WG_T001-LAND1 EQ 'CH'.
      PERFORM MONTAR_ESTRUTURA USING:
          1  ' '          ' '                'TG_SAIDA_EXEC' 'ICON'      TEXT-A14           ' ',
          2  'FAGLFLEXT'  'RACCT'            'TG_SAIDA_EXEC' 'RACCT'     TEXT-A01           ' ',
          3  'SKAT'       'TXT50'            'TG_SAIDA_EXEC' 'TXT50'     TEXT-A04           ' ',
          4  ' '          ' '                'TG_SAIDA_EXEC' 'MSG'       TEXT-A15           '80'.
    ELSEIF WG_T001-LAND1 EQ 'AR'
        OR WG_T001-LAND1 EQ 'PY'.
      PERFORM MONTAR_ESTRUTURA USING:
        1  ' '          ' '                'TG_SAIDA_EXEC' 'ICON'        TEXT-A14           ' ',
        2  'FAGLFLEXT'  'RACCT'            'TG_SAIDA_EXEC' 'RACCT'       TEXT-A11           ' ',
        3  'SKAT'       'TXT50'            'TG_SAIDA_EXEC' 'TXT50'       TEXT-A12           ' ',
        4  ' '          ' '                'TG_SAIDA_EXEC' 'MSG'         TEXT-A15           '80'.
    ENDIF.
  ENDIF.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0321   text
*      -->P_0322   text
*      -->P_0323   text
*      -->P_0324   text
*      -->P_0325   text
*      -->P_0326   text
*----------------------------------------------------------------------*
FORM MONTAR_ESTRUTURA USING VALUE(P_COL_POS)       TYPE I
                            VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                            VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                            VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                            VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                            P_SCRTEXT_L
*                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  CLEAR WA_ESTRUTURA.

* Utiliza para retirar os ZEROS.
  IF P_FIELD EQ 'BELNR'
  OR P_FIELD EQ 'BELNR_EST'.
    WA_ESTRUTURA-JUST = 'C'.
    WA_ESTRUTURA-HOTSPOT = C_X.
  ENDIF.

  WA_ESTRUTURA-OUTPUTLEN     = P_OUTPUTLEN.
  WA_ESTRUTURA-FIELDNAME     = P_FIELD.
  WA_ESTRUTURA-TABNAME       = P_TABNAME.
  WA_ESTRUTURA-REF_TABNAME   = P_REF_TABNAME.
  WA_ESTRUTURA-REF_FIELDNAME = P_REF_FIELDNAME.
  WA_ESTRUTURA-KEY           = ' '.
  WA_ESTRUTURA-KEY_SEL       = 'X'.
  WA_ESTRUTURA-COL_POS       = P_COL_POS.
  WA_ESTRUTURA-NO_OUT        = ' '.
  WA_ESTRUTURA-SELTEXT_S     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_M     = P_SCRTEXT_L.
  WA_ESTRUTURA-SELTEXT_L     = P_SCRTEXT_L.

  IF P_SCRTEXT_L IS NOT INITIAL.
    WA_ESTRUTURA-REPTEXT_DDIC  = P_SCRTEXT_L.
  ENDIF.

  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .
  DATA: WL_TABIX TYPE SY-TABIX.

** Processamento
  IF P_PROC IS NOT INITIAL OR P_REVE IS NOT INITIAL.
    VG_RYEAR  = S_MES-LOW+2(4).
    CONCATENATE VG_RYEAR S_MES-LOW(2) '01' INTO VG_LAST_DAY_AUX.
    VG_LAST_DAY = VG_LAST_DAY_AUX.
    CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
      EXPORTING
        I_DATE = VG_LAST_DAY
      IMPORTING
        E_DATE = VG_LAST_DAY.

    CONVERT INVERTED-DATE VG_LAST_DAY INTO DATE VG_LAST_DAY_AUX2.

*    IF  NOT '0101_0200' CS S_BUKRS-LOW.
    ADD 1 TO VG_LAST_DAY.
*    ENDIF.


    CONVERT INVERTED-DATE VG_LAST_DAY INTO DATE VG_LAST_DAY_AUX.

*    IF  NOT '0101_0200' CS S_BUKRS-LOW.
    SUBTRACT 1 FROM VG_LAST_DAY.
*    ENDIF.

    VG_FIRST_DAY = VG_LAST_DAY.

    ADD 1 TO VG_FIRST_DAY.

    SELECT *
      FROM SETLEAF
      INTO TABLE TG_SETLEAF
       WHERE SETNAME EQ 'MAGGI_ZFI061'.

    IF SY-SUBRC IS INITIAL.
      LOOP AT TG_SETLEAF INTO WG_SETLEAF.
        MOVE WG_SETLEAF-VALFROM TO WG_SETLEAF-KTOKS.
        MODIFY TG_SETLEAF FROM WG_SETLEAF.
      ENDLOOP.

      SELECT * "#EC CI_DB_OPERATION_OK[2431747]
        FROM T882G
        INTO TABLE TG_T882G
         WHERE RBUKRS IN S_BUKRS.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM T001
          INTO TABLE TG_T001
           FOR ALL ENTRIES IN TG_T882G
           WHERE BUKRS EQ TG_T882G-RBUKRS.

        IF SY-SUBRC IS INITIAL.
          SELECT * "#EC CI_DB_OPERATION_OK[2389136]
            FROM SKA1 "#EC CI_DB_OPERATION_OK[2431747]
            INTO TABLE TG_SKA1
             FOR ALL ENTRIES IN TG_SETLEAF
             WHERE KTOPL EQ '0050'
               AND KTOKS EQ TG_SETLEAF-KTOKS
               AND SAKNR IN S_SAKNR.

          IF SY-SUBRC IS INITIAL.
            SELECT *
              FROM SKAT
              INTO TABLE TG_SKAT
               FOR ALL ENTRIES IN TG_SKA1
               WHERE SAKNR EQ TG_SKA1-SAKNR
               AND KTOPL EQ '0050'
               AND SPRAS EQ SY-LANGU.

            SELECT * "#EC CI_DB_OPERATION_OK[2431747]
              FROM SKB1
              INTO TABLE TG_SKB1
               FOR ALL ENTRIES IN TG_SKA1
               WHERE SAKNR EQ TG_SKA1-SAKNR
                 AND BUKRS IN S_BUKRS.

            IF SY-SUBRC IS INITIAL.
              SELECT *
                FROM ZFIT0082
                INTO TABLE TG_0082
                 FOR ALL ENTRIES IN TG_SKB1
                 WHERE BUKRS   IN S_BUKRS
                   AND MES_ANO IN S_MES
                   AND SAKNR   EQ TG_SKB1-SAKNR.


              IF SY-SUBRC IS INITIAL.
                REFRESH: TG_0082_AUX.
                TG_0082_AUX[] = TG_0082[].
                DELETE TG_0082_AUX WHERE OBJ_KEY IS INITIAL.
                IF TG_0082_AUX[] IS NOT INITIAL.
                  SELECT *
                    FROM ZIB_CONTABIL
                    INTO TABLE TG_ZIB
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.

                  SELECT *
                    FROM ZIB_CONTABIL_CHV
                    INTO TABLE TG_ZIB_CHV
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.

                  SELECT *
                    FROM ZIB_CONTABIL_ERR
                    INTO TABLE TG_ZIB_ERR
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.
                ENDIF.
**             Estorno
                REFRESH: TG_0082_AUX.
                TG_0082_AUX[] = TG_0082[].
                DELETE TG_0082_AUX WHERE OBJ_KEY_EST IS INITIAL.
                IF TG_0082_AUX[] IS NOT INITIAL.
                  SELECT *
                    FROM ZIB_CONTABIL
                    APPENDING TABLE TG_ZIB
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.

                  SELECT *
                    FROM ZIB_CONTABIL_CHV
                    APPENDING TABLE TG_ZIB_CHV
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.

                  SELECT *
                    FROM ZIB_CONTABIL_ERR
                    APPENDING TABLE TG_ZIB_ERR
                     FOR ALL ENTRIES IN TG_0082_AUX
                     WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.
                ENDIF.
              ENDIF.

              IF TG_SKB1[] IS NOT INITIAL.
                SELECT *
                  FROM FAGLFLEXT
                  INTO TABLE TG_FAGLFLEXT
                   FOR ALL ENTRIES IN TG_SKB1
                   WHERE RYEAR  EQ VG_RYEAR
                     AND RBUKRS IN S_BUKRS
                     AND RACCT  EQ TG_SKB1-SAKNR
                     AND RLDNR  EQ '0L'.

                SELECT *
                  FROM TCURR
                  INTO TABLE TG_TCURR
                   WHERE GDATU EQ VG_LAST_DAY_AUX
                     AND KURST EQ 'B'
                     AND FCURR IN ('USD', 'BRL')
                     AND TCURR IN ('BRL', 'ARS', 'PYG','EUR','CHF').

                SELECT *
                  FROM TCURR
                  INTO TABLE TG_TCURR_LDAY
                   WHERE GDATU EQ VG_LAST_DAY_AUX2
                     AND KURST EQ 'B'
                     AND FCURR IN ('USD', 'BRL')
                     AND TCURR IN ('BRL', 'ARS', 'PYG','EUR','CHF').

                SELECT *
                  FROM ZFIT0081
                  INTO TABLE TG_0081
                   WHERE CTA_MONET EQ 'S'
                     AND BUKRS IN S_BUKRS.

              ENDIF.

            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

** Visualização
  ELSE.
    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE TG_0082
      FROM ZFIT0082
      INNER JOIN SKB1 ON SKB1~BUKRS = ZFIT0082~BUKRS "#EC CI_DB_OPERATION_OK[2431747]
                     AND SKB1~SAKNR = ZFIT0082~SAKNR
                     AND SKB1~MITKZ = ''

       WHERE ZFIT0082~BUKRS   IN S_BUKRS
         AND ZFIT0082~MES_ANO IN S_MES
         AND ZFIT0082~SAKNR   IN S_SAKNR.

    IF SY-SUBRC IS INITIAL.
      SELECT * "#EC CI_DB_OPERATION_OK[2431747]
        FROM T882G
        INTO TABLE TG_T882G
         WHERE RBUKRS IN S_BUKRS.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM T001
          INTO TABLE TG_T001
           FOR ALL ENTRIES IN TG_T882G
           WHERE BUKRS EQ TG_T882G-RBUKRS.
      ENDIF.
      SELECT * "#EC CI_DB_OPERATION_OK[2389136]
        FROM SKA1 "#EC CI_DB_OPERATION_OK[2431747]
        INTO TABLE TG_SKA1
         FOR ALL ENTRIES IN TG_0082
         WHERE KTOPL EQ '0050'
           AND SAKNR EQ TG_0082-SAKNR.

      IF SY-SUBRC IS INITIAL.
        SELECT *
          FROM SKAT
          INTO TABLE TG_SKAT
           FOR ALL ENTRIES IN TG_SKA1
           WHERE SAKNR EQ TG_SKA1-SAKNR
           AND KTOPL EQ '0050'
           AND SPRAS EQ SY-LANGU.
      ENDIF.

      SELECT *
          FROM T001
          INTO TABLE TG_T001
           FOR ALL ENTRIES IN TG_0082
           WHERE BUKRS EQ TG_0082-BUKRS.

      REFRESH: TG_0082_AUX.
      TG_0082_AUX[] = TG_0082[].
      DELETE TG_0082_AUX WHERE OBJ_KEY IS INITIAL.
      IF TG_0082_AUX[] IS NOT INITIAL.
        SELECT *
          FROM ZIB_CONTABIL
          INTO TABLE TG_ZIB
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.

        SELECT *
          FROM ZIB_CONTABIL_CHV
          INTO TABLE TG_ZIB_CHV
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.

        SELECT *
          FROM ZIB_CONTABIL_ERR
          INTO TABLE TG_ZIB_ERR
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY.
      ENDIF.
**             Estorno
      REFRESH: TG_0082_AUX.
      TG_0082_AUX[] = TG_0082[].
      DELETE TG_0082_AUX WHERE OBJ_KEY_EST IS INITIAL.
      IF TG_0082_AUX[] IS NOT INITIAL.
        SELECT *
          FROM ZIB_CONTABIL
          APPENDING TABLE TG_ZIB
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.

        SELECT *
          FROM ZIB_CONTABIL_CHV
          APPENDING TABLE TG_ZIB_CHV
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.

        SELECT *
          FROM ZIB_CONTABIL_ERR
          APPENDING TABLE TG_ZIB_ERR
           FOR ALL ENTRIES IN TG_0082_AUX
           WHERE OBJ_KEY EQ TG_0082_AUX-OBJ_KEY_EST.
      ENDIF.

      SELECT *
        FROM ZFIT0081
        INTO TABLE TG_0081
         WHERE CTA_MONET EQ 'S'
         AND   BUKRS IN S_BUKRS.
    ELSE.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-011
                                             TEXT-012.
      STOP.
    ENDIF.
  ENDIF.


ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZACAO_DADOS .
  DATA: WL_SALDO_MI  TYPE FAGLFLEXT-HSLVT,
        WL_SALDO_MI2 TYPE FAGLFLEXT-KSLVT,
        WL_SALDO_MI3 TYPE FAGLFLEXT-OSLVT,
        WL_SAIDA     LIKE WG_SAIDA,
        WL_UPDATE    TYPE SY-TABIX,
        VG_RACCT     TYPE FAGLFLEXT-RACCT,
        WL_FLAG.

** Processamento
  IF P_PROC IS NOT INITIAL.
    SORT: TG_SKAT       BY SAKNR,
          TG_SKA1       BY SAKNR,
          TG_TCURR      BY FCURR TCURR,
          TG_TCURR_LDAY BY FCURR TCURR,
          TG_T001       BY BUKRS,
          TG_T882G      BY RBUKRS,
          TG_0082       BY SAKNR VBUND,
          TG_ZIB_CHV    BY OBJ_KEY,
          TG_ZIB_ERR    BY OBJ_KEY.

    LOOP AT TG_FAGLFLEXT INTO WG_FAGLFLEXT.
      CLEAR: WL_SALDO_MI, WL_SALDO_MI2, WL_SALDO_MI3,
      WL_UPDATE, WL_SAIDA, WL_FLAG.

      "Documento Processado não deve mais consultar pois vai alterar novamente
      "com base diferente, a variação ou valor em moeda forte irá mudar
      "IR56206 ALRS 13.03.2015
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WG_FAGLFLEXT-RACCT
        IMPORTING
          OUTPUT = VG_RACCT.

      IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
        READ TABLE TG_0082 INTO WG_0082 WITH KEY SAKNR = WG_FAGLFLEXT-RACCT
                                                 VBUND = WG_FAGLFLEXT-RASSC.
      ELSE.
        READ TABLE TG_0082 INTO WG_0082 WITH KEY SAKNR = WG_FAGLFLEXT-RACCT.
      ENDIF.
      IF ( SY-SUBRC IS INITIAL ) AND ( WG_0082-OBJ_KEY IS NOT INITIAL ) AND ( WG_0082-OBJ_KEY_EST IS INITIAL ).
        CONTINUE.
      ENDIF.
      CLEAR: WG_0082.

      IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
        READ TABLE TG_SAIDA INTO WL_SAIDA WITH KEY RACCT = WG_FAGLFLEXT-RACCT
                                                   RASSC = WG_FAGLFLEXT-RASSC.
      ELSE.
        READ TABLE TG_SAIDA INTO WL_SAIDA WITH KEY RACCT = WG_FAGLFLEXT-RACCT.
      ENDIF.
      IF SY-SUBRC IS INITIAL.
        WL_UPDATE = SY-TABIX.
        ADD WL_SAIDA-CURR1 TO WL_SALDO_MI.
        ADD WL_SAIDA-CURR2 TO WL_SALDO_MI2.
        ADD WL_SAIDA-CURR3 TO WL_SALDO_MI3.
      ENDIF.

**  Saldo Mi1 ***************************************************************
      ADD WG_FAGLFLEXT-HSLVT TO WL_SALDO_MI.
      IF S_MES-LOW(2) GE '01'.
        ADD WG_FAGLFLEXT-HSL01 TO WL_SALDO_MI.
      ENDIF.
      IF S_MES-LOW(2) GE '02'.
        ADD WG_FAGLFLEXT-HSL02 TO WL_SALDO_MI.
      ENDIF.
      IF S_MES-LOW(2) GE '03'.
        ADD WG_FAGLFLEXT-HSL03 TO WL_SALDO_MI.
      ENDIF.
      IF S_MES-LOW(2) GE '04'.
        ADD WG_FAGLFLEXT-HSL04 TO WL_SALDO_MI.
      ENDIF.
      IF S_MES-LOW(2) GE '05'.
        ADD WG_FAGLFLEXT-HSL05 TO WL_SALDO_MI.
      ENDIF.
      IF S_MES-LOW(2) GE '06'.
        ADD WG_FAGLFLEXT-HSL06 TO WL_SALDO_MI.
      ENDIF.
      IF S_MES-LOW(2) GE '07'.
        ADD WG_FAGLFLEXT-HSL07 TO WL_SALDO_MI.
      ENDIF.
      IF S_MES-LOW(2) GE '08'.
        ADD WG_FAGLFLEXT-HSL08 TO WL_SALDO_MI.
      ENDIF.
      IF S_MES-LOW(2) GE '09'.
        ADD WG_FAGLFLEXT-HSL09 TO WL_SALDO_MI.
      ENDIF.
      IF S_MES-LOW(2) GE '10'.
        ADD WG_FAGLFLEXT-HSL10 TO WL_SALDO_MI.
      ENDIF.
      IF S_MES-LOW(2) GE '11'.
        ADD WG_FAGLFLEXT-HSL11 TO WL_SALDO_MI.
      ENDIF.
      IF S_MES-LOW(2) GE '12'.
        ADD WG_FAGLFLEXT-HSL12 TO WL_SALDO_MI.
        ADD WG_FAGLFLEXT-HSL13 TO WL_SALDO_MI.
        ADD WG_FAGLFLEXT-HSL14 TO WL_SALDO_MI.
        ADD WG_FAGLFLEXT-HSL15 TO WL_SALDO_MI.
        ADD WG_FAGLFLEXT-HSL16 TO WL_SALDO_MI.
      ENDIF.

**  Saldo Mi2 *****************************************************************
      ADD WG_FAGLFLEXT-KSLVT TO WL_SALDO_MI2.
      IF S_MES-LOW(2) GE '01'.
        ADD WG_FAGLFLEXT-KSL01 TO WL_SALDO_MI2.
      ENDIF.
      IF S_MES-LOW(2) GE '02'.
        ADD WG_FAGLFLEXT-KSL02 TO WL_SALDO_MI2.
      ENDIF.
      IF S_MES-LOW(2) GE '03'.
        ADD WG_FAGLFLEXT-KSL03 TO WL_SALDO_MI2.
      ENDIF.
      IF S_MES-LOW(2) GE '04'.
        ADD WG_FAGLFLEXT-KSL04 TO WL_SALDO_MI2.
      ENDIF.
      IF S_MES-LOW(2) GE '05'.
        ADD WG_FAGLFLEXT-KSL05 TO WL_SALDO_MI2.
      ENDIF.
      IF S_MES-LOW(2) GE '06'.
        ADD WG_FAGLFLEXT-KSL06 TO WL_SALDO_MI2.
      ENDIF.
      IF S_MES-LOW(2) GE '07'.
        ADD WG_FAGLFLEXT-KSL07 TO WL_SALDO_MI2.
      ENDIF.
      IF S_MES-LOW(2) GE '08'.
        ADD WG_FAGLFLEXT-KSL08 TO WL_SALDO_MI2.
      ENDIF.
      IF S_MES-LOW(2) GE '09'.
        ADD WG_FAGLFLEXT-KSL09 TO WL_SALDO_MI2.
      ENDIF.
      IF S_MES-LOW(2) GE '10'.
        ADD WG_FAGLFLEXT-KSL10 TO WL_SALDO_MI2.
      ENDIF.
      IF S_MES-LOW(2) GE '11'.
        ADD WG_FAGLFLEXT-KSL11 TO WL_SALDO_MI2.
      ENDIF.
      IF S_MES-LOW(2) GE '12'.
        ADD WG_FAGLFLEXT-KSL12 TO WL_SALDO_MI2.
        ADD WG_FAGLFLEXT-KSL13 TO WL_SALDO_MI2.
        ADD WG_FAGLFLEXT-KSL14 TO WL_SALDO_MI2.
        ADD WG_FAGLFLEXT-KSL15 TO WL_SALDO_MI2.
        ADD WG_FAGLFLEXT-KSL16 TO WL_SALDO_MI2.
      ENDIF.

      READ TABLE TG_T001 INTO WG_T001 WITH KEY BUKRS = WG_FAGLFLEXT-RBUKRS BINARY SEARCH.

      IF WL_UPDATE IS INITIAL.
        READ TABLE TG_SKAT INTO WG_SKAT WITH KEY SAKNR = WG_FAGLFLEXT-RACCT BINARY SEARCH.
        READ TABLE TG_SKA1 INTO WG_SKA1 WITH KEY SAKNR = WG_FAGLFLEXT-RACCT BINARY SEARCH.
        "IR56206 ALRS 13.03.2015
        IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
          READ TABLE TG_0082 INTO WG_0082 WITH KEY SAKNR = WG_FAGLFLEXT-RACCT
                                                   VBUND = WG_FAGLFLEXT-RASSC BINARY SEARCH.
        ELSE.
          READ TABLE TG_0082 INTO WG_0082 WITH KEY SAKNR = WG_FAGLFLEXT-RACCT BINARY SEARCH.
        ENDIF.
        IF SY-SUBRC IS INITIAL.
          CLEAR: WG_ZIB_CHV.
          READ TABLE TG_ZIB_CHV INTO WG_ZIB_CHV WITH KEY OBJ_KEY = WG_0082-OBJ_KEY BINARY SEARCH.
          IF SY-SUBRC IS NOT INITIAL.
            READ TABLE TG_ZIB_ERR INTO WG_ZIB_ERR WITH KEY OBJ_KEY = WG_0082-OBJ_KEY BINARY SEARCH.
          ELSEIF WG_0082-OBJ_KEY_EST IS NOT INITIAL.   " estorno
            CLEAR: WG_ZIB_CHV.
            READ TABLE TG_ZIB_CHV  INTO WG_ZIB_CHV WITH KEY OBJ_KEY = WG_0082-OBJ_KEY_EST BINARY SEARCH.
            IF SY-SUBRC IS NOT INITIAL.
              READ TABLE TG_ZIB_ERR  INTO WG_ZIB_ERR WITH KEY OBJ_KEY = WG_0082-OBJ_KEY_EST BINARY SEARCH.
            ELSE.
              WG_SAIDA-BELNR_EST = WG_ZIB_CHV-BELNR.
            ENDIF.
          ELSEIF WG_0082-OBJ_KEY_EST IS INITIAL.
            WG_SAIDA-BELNR = WG_ZIB_CHV-BELNR.
          ENDIF.
        ENDIF.
        WG_SAIDA-OBJ_KEY     = WG_0082-OBJ_KEY.
        WG_SAIDA-OBJ_KEY_EST = WG_0082-OBJ_KEY_EST.
        WG_SAIDA-RACCT       = WG_FAGLFLEXT-RACCT.
        WG_SAIDA-TXT50       = WG_SKAT-TXT50.
        WG_SAIDA-KTOKS       = WG_SKA1-KTOKS.
      ENDIF.
      WG_SAIDA-CURR1       = WL_SALDO_MI.
      WG_SAIDA-CURR2       = WL_SALDO_MI2.

      CASE WG_T001-LAND1.
        WHEN 'BR'.
          "Recupera
          CLEAR: WG_TCURR.
          READ TABLE TG_TCURR INTO WG_TCURR
            WITH KEY FCURR = 'USD'
                     TCURR = 'BRL'
                     BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WG_SAIDA-TX_USD = WG_TCURR-UKURS.
            TRY.
                IF WG_T001-BUKRS EQ '0101'.
                  WG_SAIDA-SALDO_CORR  = ( ( WG_SAIDA-CURR1 * 100 ) / WG_SAIDA-TX_USD ).
                ELSE.
                  WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-CURR1 / WG_SAIDA-TX_USD ).
                ENDIF.
              CATCH CX_SY_ZERODIVIDE .
            ENDTRY.
            WG_SAIDA-VLR_AJUST   = ( WG_SAIDA-SALDO_CORR  - WG_SAIDA-CURR2 ).
          ENDIF.
        WHEN 'NL'.
          "Recupera
          CLEAR: WG_TCURR_LDAY.
          READ TABLE TG_TCURR_LDAY INTO WG_TCURR_LDAY
            WITH KEY FCURR = 'USD'
                     TCURR = 'EUR'
                     BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WG_SAIDA-TX_USD = WG_TCURR_LDAY-UKURS.
            IF WG_SAIDA-TX_USD LT 0.
              MULTIPLY WG_SAIDA-TX_USD BY -1.
              TRY.
                  WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-CURR1 * WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE .
              ENDTRY.
            ELSE.
              TRY.
                  WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-CURR1 / WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE .
              ENDTRY.
            ENDIF.
            WG_SAIDA-VLR_AJUST   = ( WG_SAIDA-SALDO_CORR  - WG_SAIDA-CURR2 ).
          ENDIF.
        WHEN 'CH'.
          "Recupera
          CLEAR: WG_TCURR_LDAY.
          READ TABLE TG_TCURR_LDAY INTO WG_TCURR_LDAY
            WITH KEY FCURR = 'USD'
                     TCURR = 'CHF'
                     BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            WG_SAIDA-TX_USD = WG_TCURR_LDAY-UKURS.
            IF WG_SAIDA-TX_USD LT 0.
              MULTIPLY WG_SAIDA-TX_USD BY -1.
              TRY.
                  WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-CURR1 * WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE .
              ENDTRY.
            ELSE.
              TRY.
                  WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-CURR1 / WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE .
              ENDTRY.
            ENDIF.
          ENDIF.
          WG_SAIDA-VLR_AJUST   = ( WG_SAIDA-SALDO_CORR  - WG_SAIDA-CURR2 ).
        WHEN 'AR' OR 'PY'.

**  Saldo Mi3 ****************************************************************************
          ADD WG_FAGLFLEXT-OSLVT TO WL_SALDO_MI3.
          IF S_MES-LOW(2) GE '01'.
            ADD WG_FAGLFLEXT-OSL01 TO WL_SALDO_MI3.
          ENDIF.
          IF S_MES-LOW(2) GE '02'.
            ADD WG_FAGLFLEXT-OSL02 TO WL_SALDO_MI3.
          ENDIF.
          IF S_MES-LOW(2) GE '03'.
            ADD WG_FAGLFLEXT-OSL03 TO WL_SALDO_MI3.
          ENDIF.
          IF S_MES-LOW(2) GE '04'.
            ADD WG_FAGLFLEXT-OSL04 TO WL_SALDO_MI3.
          ENDIF.
          IF S_MES-LOW(2) GE '05'.
            ADD WG_FAGLFLEXT-OSL05 TO WL_SALDO_MI3.
          ENDIF.
          IF S_MES-LOW(2) GE '06'.
            ADD WG_FAGLFLEXT-OSL06 TO WL_SALDO_MI3.
          ENDIF.
          IF S_MES-LOW(2) GE '07'.
            ADD WG_FAGLFLEXT-OSL07 TO WL_SALDO_MI3.
          ENDIF.
          IF S_MES-LOW(2) GE '08'.
            ADD WG_FAGLFLEXT-OSL08 TO WL_SALDO_MI3.
          ENDIF.
          IF S_MES-LOW(2) GE '09'.
            ADD WG_FAGLFLEXT-OSL09 TO WL_SALDO_MI3.
          ENDIF.
          IF S_MES-LOW(2) GE '10'.
            ADD WG_FAGLFLEXT-OSL10 TO WL_SALDO_MI3.
          ENDIF.
          IF S_MES-LOW(2) GE '11'.
            ADD WG_FAGLFLEXT-OSL11 TO WL_SALDO_MI3.
          ENDIF.
          IF S_MES-LOW(2) GE '12'.
            ADD WG_FAGLFLEXT-OSL12 TO WL_SALDO_MI3.
            ADD WG_FAGLFLEXT-OSL13 TO WL_SALDO_MI3.
            ADD WG_FAGLFLEXT-OSL14 TO WL_SALDO_MI3.
            ADD WG_FAGLFLEXT-OSL15 TO WL_SALDO_MI3.
            ADD WG_FAGLFLEXT-OSL16 TO WL_SALDO_MI3.
          ENDIF.

          WG_SAIDA-CURR3 = WL_SALDO_MI3.
          IF WG_FAGLFLEXT-RBUKRS EQ '0100'.
            CLEAR: WG_TCURR.
            READ TABLE TG_TCURR INTO WG_TCURR
              WITH KEY FCURR = 'USD'
                       TCURR = 'ARS'
                       BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              WG_SAIDA-TX_USD = WG_TCURR-UKURS.
              TRY.
                  WG_SAIDA-SALDO_CORR = ( WG_SAIDA-CURR1  / WG_SAIDA-TX_USD ).
                CATCH CX_SY_ZERODIVIDE.
              ENDTRY.
              WG_SAIDA-VLR_AJUST = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-CURR2 ).
            ENDIF.

            CLEAR: WG_TCURR.
            READ TABLE TG_TCURR INTO WG_TCURR
              WITH KEY FCURR = 'BRL'
                       TCURR = 'ARS'
                       BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              WG_SAIDA-TX_BRL      = WG_TCURR-UKURS.
              TRY.
                  WG_SAIDA-SALDO_CORR2 = ( WG_SAIDA-CURR1  / WG_SAIDA-TX_BRL ).
                CATCH CX_SY_ZERODIVIDE.
              ENDTRY.
              WG_SAIDA-VLR_AJUST2 = ( WG_SAIDA-SALDO_CORR2 - WG_SAIDA-CURR3 ).
            ENDIF.

          ELSEIF WG_FAGLFLEXT-RBUKRS EQ '0101'.
            CLEAR: WG_TCURR.
            READ TABLE TG_TCURR INTO WG_TCURR
              WITH KEY FCURR = 'USD'
                       TCURR = 'PYG'
                       BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              WG_SAIDA-TX_USD = WG_TCURR-UKURS.
              TRY.
                  IF WG_T001-BUKRS EQ '0101'.
                    WG_SAIDA-SALDO_CORR  = ( ( WG_SAIDA-CURR1 * 100 ) / WG_SAIDA-TX_USD ).
                  ELSE.
                    WG_SAIDA-SALDO_CORR  = ( WG_SAIDA-CURR1 / WG_SAIDA-TX_USD ).
                  ENDIF.
                CATCH CX_SY_ZERODIVIDE.
              ENDTRY.
              WG_SAIDA-VLR_AJUST = ( WG_SAIDA-SALDO_CORR - WG_SAIDA-CURR2 ).
            ENDIF.

            CLEAR: WG_TCURR.
            READ TABLE TG_TCURR INTO WG_TCURR
              WITH KEY FCURR = 'BRL'
                       TCURR = 'PYG'
                       BINARY SEARCH.
            IF SY-SUBRC IS INITIAL.
              WG_SAIDA-TX_BRL = WG_TCURR-UKURS.
              TRY.
                  WG_SAIDA-SALDO_CORR2 = ( ( WG_SAIDA-CURR1 * 100 )  / WG_SAIDA-TX_BRL ).
                CATCH CX_SY_ZERODIVIDE.
              ENDTRY.
              WG_SAIDA-VLR_AJUST2 = ( WG_SAIDA-SALDO_CORR2 - WG_SAIDA-CURR3 ).
            ENDIF.
          ENDIF.
      ENDCASE.

      LOOP AT TG_0081 INTO WG_0081.
        IF WG_0081-CONTA_DE IS NOT INITIAL.
          IF WG_FAGLFLEXT-RACCT EQ WG_0081-CONTA_DE.
            WL_FLAG = C_X.
            EXIT.
          ENDIF.
        ENDIF.
        CLEAR:WG_0081.
      ENDLOOP.

      "IR56206 ALRS 13.03.2015
      IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
        READ TABLE TG_0082 INTO WG_0082
         WITH KEY SAKNR = WG_FAGLFLEXT-RACCT
                  VBUND = WG_FAGLFLEXT-RASSC BINARY SEARCH.
      ELSE.
        READ TABLE TG_0082 INTO WG_0082
          WITH KEY SAKNR = WG_FAGLFLEXT-RACCT BINARY SEARCH.
      ENDIF.
      " Se for estorno recupera valor de ajuste
      IF SY-SUBRC = 0.
        IF WG_0082-OBJ_KEY_EST IS INITIAL AND WG_0082-OBJ_KEY IS NOT INITIAL.
          WG_SAIDA-VLR_AJUST  = WG_0082-VLR_CORR_MI2.
          WG_SAIDA-VLR_AJUST2 = WG_0082-VLR_CORR_MI3.
        ENDIF.
      ENDIF.

      "IR56206 ALRS 13.03.2015
      IF  '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
        WG_SAIDA-RASSC = WG_FAGLFLEXT-RASSC.
      ENDIF.


      IF WL_FLAG IS NOT INITIAL.
        IF WL_UPDATE IS INITIAL.
          APPEND WG_SAIDA TO TG_SAIDA.
        ELSE.
          MODIFY TG_SAIDA FROM WG_SAIDA INDEX WL_UPDATE TRANSPORTING CURR1 CURR2 CURR3 TX_USD TX_BRL SALDO_CORR SALDO_CORR2 VLR_AJUST VLR_AJUST2.
        ENDIF.
      ENDIF.
      CLEAR: WG_SAIDA, WG_T001, WG_SKA1, WG_SKAT, WG_0082, WL_FLAG.
    ENDLOOP.

    "DELETE TG_SAIDA WHERE VLR_AJUST = 0 AND  VLR_AJUST2 = 0.

    "**********************************************************************************************
    " Ajusta Saida de Documento Processado """"""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT TG_0082 INTO WG_0082.
      "IR56206 ALRS 13.03.2015
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          INPUT  = WG_0082-SAKNR
        IMPORTING
          OUTPUT = VG_RACCT.
      IF '214101_224100_121130_113130_113131_214121' CS VG_RACCT.
        READ TABLE TG_SAIDA INTO WL_SAIDA WITH KEY RACCT = WG_0082-SAKNR
                                                   RASSC = WG_0082-VBUND.
      ELSE.
        READ TABLE TG_SAIDA INTO WL_SAIDA WITH KEY RACCT = WG_0082-SAKNR.
      ENDIF.
      IF SY-SUBRC IS NOT INITIAL.
        READ TABLE TG_SKAT INTO WG_SKAT WITH KEY SAKNR = WG_0082-SAKNR BINARY SEARCH.
        READ TABLE TG_SKA1 INTO WG_SKA1 WITH KEY SAKNR = WG_0082-SAKNR BINARY SEARCH.
        WL_SAIDA-RACCT        = WG_0082-SAKNR.
        "IR56206 ALRS 13.03.2015
        WL_SAIDA-RASSC        = WG_0082-VBUND.
        WL_SAIDA-TXT50        = WG_SKAT-TXT50.
        WL_SAIDA-KTOKS        = WG_SKA1-KTOKS.
        WL_SAIDA-CURR1        = WG_0082-DMBTR.
        WL_SAIDA-CURR2        = WG_0082-DMBE2.
        WL_SAIDA-CURR3        = WG_0082-DMBE3.
        WL_SAIDA-TX_USD       = WG_0082-TX_USD.
        WL_SAIDA-TX_BRL       = WG_0082-TX_BRL.
        WL_SAIDA-SALDO_CORR   = WG_0082-SDO_CORR_MI2.
        WL_SAIDA-SALDO_CORR2  = WG_0082-SDO_CORR_MI3.
        WL_SAIDA-VLR_AJUST    = WG_0082-VLR_CORR_MI2.
        WL_SAIDA-VLR_AJUST2   = WG_0082-VLR_CORR_MI3.
        WL_SAIDA-OBJ_KEY      = WG_0082-OBJ_KEY.
        WL_SAIDA-OBJ_KEY_EST  = WG_0082-OBJ_KEY_EST.
        READ TABLE TG_ZIB_CHV INTO WG_ZIB_CHV WITH KEY OBJ_KEY = WG_0082-OBJ_KEY BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          WL_SAIDA-BELNR = WG_ZIB_CHV-BELNR.
        ENDIF.
        APPEND WL_SAIDA TO TG_SAIDA.
      ENDIF.
    ENDLOOP.
    "**********************************************************************************************

    SORT TG_SAIDA BY RACCT.


    PERFORM ATUALIZA_SAIDA TABLES TG_SAIDA
                                  TG_ZIB
                                  TG_ZIB_CHV
                                  TG_ZIB_ERR.

    IF TG_SAIDA[] IS INITIAL.
*      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Não foram encontrados dados para processamento'.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-013.
      STOP.
    ENDIF.

** Visualização
  ELSE.
    SORT: TG_SKAT    BY SAKNR,
          TG_SKA1    BY SAKNR,
          TG_T001    BY BUKRS,
          TG_ZIB_CHV BY OBJ_KEY,
          TG_ZIB_ERR BY OBJ_KEY.

    LOOP AT TG_0082 INTO WG_0082.
      CLEAR: WL_FLAG.

      READ TABLE TG_SKAT INTO WG_SKAT WITH KEY SAKNR = WG_0082-SAKNR BINARY SEARCH.
      READ TABLE TG_SKA1 INTO WG_SKA1 WITH KEY SAKNR = WG_0082-SAKNR BINARY SEARCH.

      WG_SAIDA-RACCT        = WG_0082-SAKNR.
      WG_SAIDA-TXT50        = WG_SKAT-TXT50.
      WG_SAIDA-KTOKS        = WG_SKA1-KTOKS.
      WG_SAIDA-CURR1        = WG_0082-DMBTR.
      WG_SAIDA-CURR2        = WG_0082-DMBE2.
      WG_SAIDA-CURR3        = WG_0082-DMBE3.
      WG_SAIDA-TX_USD       = WG_0082-TX_USD.
      WG_SAIDA-SALDO_CORR   = WG_0082-SDO_CORR_MI2.
      WG_SAIDA-VLR_AJUST    = WG_0082-VLR_CORR_MI2.
      WG_SAIDA-TX_BRL       = WG_0082-TX_BRL.
      WG_SAIDA-SALDO_CORR2  = WG_0082-SDO_CORR_MI3.
      WG_SAIDA-VLR_AJUST2   = WG_0082-VLR_CORR_MI3.
      WG_SAIDA-OBJ_KEY      = WG_0082-OBJ_KEY.
      WG_SAIDA-OBJ_KEY_EST  = WG_0082-OBJ_KEY_EST.

      LOOP AT TG_0081 INTO WG_0081.

        IF WG_0081-CONTA_DE IS NOT INITIAL.
          IF WG_SAIDA-RACCT EQ WG_0081-CONTA_DE.
            WL_FLAG = C_X.
            EXIT.
          ENDIF.
        ENDIF.
        CLEAR:WG_0081.
      ENDLOOP.
      IF WL_FLAG IS NOT INITIAL.
        APPEND WG_SAIDA TO TG_SAIDA.
      ENDIF.
      CLEAR: WG_SAIDA, WG_SKA1, WG_SKAT, WL_FLAG.
    ENDLOOP.

    PERFORM ATUALIZA_SAIDA TABLES TG_SAIDA
                                  TG_ZIB
                                  TG_ZIB_CHV
                                  TG_ZIB_ERR.
  ENDIF.

  "10.09.2015 ALRS
  IF S_BUKRS-LOW EQ '0101'.
    LOOP AT  TG_SAIDA INTO WG_SAIDA.
      MULTIPLY WG_SAIDA-CURR1 BY 100.
      MODIFY TG_SAIDA FROM WG_SAIDA INDEX SY-TABIX TRANSPORTING CURR1.
    ENDLOOP.
  ENDIF.
** Processamento
  IF P_PROC IS NOT INITIAL.
    IF S_BUKRS-LOW EQ '0101'.
*      LOOP AT  TG_SAIDA INTO WG_SAIDA.
*        MULTIPLY WG_SAIDA-CURR1 BY 100.
*        MODIFY TG_SAIDA FROM WG_SAIDA INDEX SY-TABIX TRANSPORTING CURR1.
*      ENDLOOP.
    ELSEIF S_BUKRS-LOW EQ '0004' OR S_BUKRS-LOW EQ '0037'. " recalcula ajuste
      LOOP AT  TG_SAIDA INTO WG_SAIDA.
        TABIX = SY-TABIX.
        CLEAR WG_0082.
        READ TABLE TG_0082 INTO WG_0082
              WITH KEY SAKNR = WG_SAIDA-RACCT  BINARY SEARCH.
        " Se for estorno recupera valor de ajuste
        IF SY-SUBRC NE 0
          OR  ( WG_0082-OBJ_KEY_EST IS NOT INITIAL OR  WG_0082-OBJ_KEY IS NOT INITIAL ).
          WG_SAIDA-SALDO_CORR = WG_SAIDA-CURR2 * WG_SAIDA-TX_USD.
          WG_SAIDA-VLR_AJUST  = WG_SAIDA-SALDO_CORR - WG_SAIDA-CURR1.
          MODIFY TG_SAIDA FROM WG_SAIDA INDEX TABIX TRANSPORTING VLR_AJUST SALDO_CORR.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*&      Form  GERA_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_SAIDA  text
*----------------------------------------------------------------------*
FORM GERA_CONTABIL  USING    WL_SAIDA TYPE TY_SAIDA
                             WL_ESTORNO
                   CHANGING  P_OBJ_KEY TYPE ZIB_CONTABIL_CHV-OBJ_KEY.

  DATA: WL_INPUT_ZIB  TYPE ZIB_CONTABIL,
        TL_INPUT_ZIB  TYPE TABLE OF ZIB_CONTABIL,
        WL_OBJ_KEY(9),
        V_SAKNR       TYPE ZFIT0081-SAKNR,
        V_BEWAR       TYPE BSEG-BEWAR,
        VANOMES(7).

  CONCATENATE S_MES-LOW(2) '/' S_MES-LOW+2(4) INTO VANOMES.


  REFRESH: TL_INPUT_ZIB.
  CLEAR: WL_INPUT_ZIB, WL_INPUT_0082.
  READ TABLE TG_T001 INTO WG_T001
      WITH KEY BUKRS =  S_BUKRS-LOW
               BINARY SEARCH.

  READ TABLE TG_T882G INTO WG_T882G
    WITH KEY RBUKRS =  S_BUKRS-LOW
             BINARY SEARCH.

  LOOP AT TG_0081 INTO WG_0081.
    IF WG_0081-CONTA_DE IS NOT INITIAL.
      IF WL_SAIDA-RACCT EQ WG_0081-CONTA_DE.
        EXIT.
      ENDIF.
    ENDIF.
    CLEAR:WG_0081.
  ENDLOOP.
  IF WG_0081 IS NOT INITIAL .
    IF WG_T001-LAND1 EQ 'AR' OR WG_T001-LAND1 EQ 'PY'.
      V_SAKNR      =  WG_0081-SAKNR.
    ELSEIF WG_0081-SAKNR IS NOT INITIAL.
      V_SAKNR      =  WG_0081-SAKNR.
    ELSEIF WG_0081-HKONT IS NOT INITIAL.
      V_SAKNR      =  WG_0081-HKONT.
    ENDIF.

    IF P_OBJ_KEY IS INITIAL.
      PERFORM GET_NEXT_NUMBER  USING  'Z_SALDOMO2'
                                      '01'
                             CHANGING WL_OBJ_KEY.
    ELSE.
      WL_OBJ_KEY = P_OBJ_KEY+7(9).
    ENDIF.
** 1 Partida
    WL_INPUT_ZIB-MANDT    = SY-MANDT.
    CONCATENATE 'ZFI0061' WL_OBJ_KEY VG_RYEAR INTO  WL_INPUT_ZIB-OBJ_KEY.
    WL_INPUT_ZIB-SEQITEM    = '0001'.
    CONCATENATE S_MES-LOW(2) '/' S_MES-LOW+2(4) INTO WL_INPUT_ZIB-XBLNR.
    IF WL_ESTORNO IS INITIAL.
      IF WL_SAIDA-VLR_AJUST LT 0.
        WL_INPUT_ZIB-BSCHL      = '50'.
      ELSE.
        WL_INPUT_ZIB-BSCHL      = '40'.
      ENDIF.
    ELSE.
      IF WL_SAIDA-VLR_AJUST LT 0.
        WL_INPUT_ZIB-BSCHL      = '40'.
      ELSE.
        WL_INPUT_ZIB-BSCHL      = '50'.
      ENDIF.
    ENDIF.
    IF WG_T001-LAND1 EQ 'BR'.
      CONCATENATE S_BUKRS-LOW+2(2) '01' INTO WL_INPUT_ZIB-GSBER.
    ELSEIF WG_T001-LAND1 EQ 'NL'.
      WL_INPUT_ZIB-GSBER = 'H201'.
    ELSEIF WG_T001-LAND1 EQ 'CH'.
      WL_INPUT_ZIB-GSBER = 'S201'.
    ELSEIF WG_T001-LAND1 EQ 'AR'
        OR WG_T001-LAND1 EQ 'PY'.
      IF S_BUKRS-LOW EQ '0100'.
        WL_INPUT_ZIB-GSBER = 'T001'.
      ELSE.
        WL_INPUT_ZIB-GSBER = 'P001'.
      ENDIF.
    ENDIF.
    WL_INPUT_ZIB-BUKRS      = S_BUKRS-LOW.
    WL_INPUT_ZIB-INTERFACE    = '35'.
    WL_INPUT_ZIB-BKTXT      = TEXT-014.
    WRITE VG_LAST_DAY TO  WL_INPUT_ZIB-BLDAT.
    WRITE VG_LAST_DAY TO  WL_INPUT_ZIB-BUDAT.
*    WL_INPUT_ZIB-BUDAT      = VG_LAST_DAY.
    WL_INPUT_ZIB-GJAHR      = VG_RYEAR.
    WL_INPUT_ZIB-MONAT      = S_MES-LOW(2).
    WL_INPUT_ZIB-BLART      = 'VC'.
    WL_INPUT_ZIB-HKONT      = WL_SAIDA-RACCT.
    WL_INPUT_ZIB-WRBTR      = 0.
    WL_INPUT_ZIB-VBUND      = WL_SAIDA-RASSC.

    IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-WAERS      = 'USD'.
    ELSE.
      WL_INPUT_ZIB-WAERS      = WG_T882G-CURR1.
    ENDIF.

    WL_INPUT_ZIB-BUPLA      = SPACE.

    IF WG_T001-LAND1 EQ 'AR'
      OR WG_T001-LAND1 EQ 'PY'.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      CONCATENATE TEXT-015 VANOMES
            INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ELSEIF WG_0081-SAKNR IS NOT INITIAL.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      CONCATENATE TEXT-015 VANOMES
          INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ELSEIF WG_0081-HKONT IS NOT INITIAL.
*      CONCATENATE 'Ajuste moeda apresentação ' VANOMES
      CONCATENATE TEXT-016 VANOMES
          INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ENDIF.

    CLEAR V_BEWAR.
    SELECT SINGLE BEWAR
      INTO V_BEWAR
      FROM ZFIT0030
      WHERE HKONT EQ WL_INPUT_ZIB-HKONT
      AND   COND  EQ ''.

    WL_INPUT_ZIB-BEWAR      = V_BEWAR.

    WL_INPUT_ZIB-WAERS_I    = 'X'.

    IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-DMBTR      = WL_SAIDA-VLR_AJUST.
      IF WL_INPUT_ZIB-DMBTR LT 0.
        MULTIPLY WL_INPUT_ZIB-DMBTR BY -1.
      ENDIF.
    ELSE.
      WL_INPUT_ZIB-DMBTR      = 0.
    ENDIF.

    WL_INPUT_ZIB-WAERS_F    = WG_T882G-CURR2.

    IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-DMBE2     = 0.
    ELSE.
      WL_INPUT_ZIB-DMBE2      = WL_SAIDA-VLR_AJUST.
      IF WL_INPUT_ZIB-DMBE2 LT 0.
        MULTIPLY WL_INPUT_ZIB-DMBE2 BY -1.
      ENDIF.
    ENDIF.

*    IF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR WG_T001-LAND1 = 'CH'.
*      WL_INPUT_ZIB-WAERS_G    = SPACE.
*      WL_INPUT_ZIB-DMBE3      = SPACE.
*    ELSEIF WG_T001-LAND1 EQ 'AR'
*        OR WG_T001-LAND1 EQ 'PY'.
*      WL_INPUT_ZIB-WAERS_G    = 'BRL'.
*      WL_INPUT_ZIB-DMBE3      = '0.01'.
*    ENDIF.
    WL_INPUT_ZIB-RG_ATUALIZADO  = 'N'.

    APPEND WL_INPUT_ZIB TO TL_INPUT_ZIB.
    CLEAR: WL_INPUT_ZIB.

** 2 Partida
    WL_INPUT_ZIB-MANDT    = SY-MANDT.
    CONCATENATE 'ZFI0061' WL_OBJ_KEY VG_RYEAR INTO  WL_INPUT_ZIB-OBJ_KEY.
    WL_INPUT_ZIB-SEQITEM    = '0002'.
    CONCATENATE S_MES-LOW(2) '/' S_MES-LOW+2(4) INTO WL_INPUT_ZIB-XBLNR.
    IF WL_ESTORNO IS INITIAL.
      IF WL_SAIDA-VLR_AJUST LT 0.
        WL_INPUT_ZIB-BSCHL      = '40'.
      ELSE.
        WL_INPUT_ZIB-BSCHL      = '50'.
      ENDIF.
    ELSE.
      IF WL_SAIDA-VLR_AJUST LT 0.
        WL_INPUT_ZIB-BSCHL      = '50'.
      ELSE.
        WL_INPUT_ZIB-BSCHL      = '40'.
      ENDIF.
    ENDIF.
    IF WG_T001-LAND1 EQ 'BR'.
      CONCATENATE S_BUKRS-LOW+2(2) '01' INTO WL_INPUT_ZIB-GSBER.
    ELSEIF WG_T001-LAND1 EQ 'NL'.
      WL_INPUT_ZIB-GSBER = 'H201'.
    ELSEIF WG_T001-LAND1 EQ 'CH'.
      WL_INPUT_ZIB-GSBER = 'S201'.
    ELSEIF WG_T001-LAND1 EQ 'AR'
        OR WG_T001-LAND1 EQ 'PY'.
      IF S_BUKRS-LOW EQ '0100'.
        WL_INPUT_ZIB-GSBER = 'T001'.
      ELSE.
        WL_INPUT_ZIB-GSBER = 'P001'.
      ENDIF.
    ENDIF.
    WL_INPUT_ZIB-BUKRS      = S_BUKRS-LOW.
    WL_INPUT_ZIB-INTERFACE    = '35'.
*    WL_INPUT_ZIB-BKTXT      = 'VAR.CTAS MONETÁRIAS'.
    WL_INPUT_ZIB-BKTXT      = TEXT-014.
    WRITE VG_LAST_DAY TO  WL_INPUT_ZIB-BLDAT.
*    WL_INPUT_ZIB-BLDAT      = VG_LAST_DAY.
    WRITE VG_LAST_DAY TO  WL_INPUT_ZIB-BUDAT.
*    WL_INPUT_ZIB-BUDAT      = VG_LAST_DAY.
    WL_INPUT_ZIB-GJAHR      = VG_RYEAR.
    WL_INPUT_ZIB-MONAT      = S_MES-LOW(2).
    WL_INPUT_ZIB-BLART      = 'VC'.

    IF WG_T001-LAND1 EQ 'AR'
        OR WG_T001-LAND1 EQ 'PY'.
      WL_INPUT_ZIB-HKONT      =  WG_0081-SAKNR.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      CONCATENATE TEXT-015 VANOMES
         INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ELSEIF WG_0081-SAKNR IS NOT INITIAL.
      WL_INPUT_ZIB-HKONT      =  WG_0081-SAKNR.
*      CONCATENATE 'Ajuste moeda funcional ' VANOMES
      CONCATENATE TEXT-015 VANOMES
      INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ELSEIF WG_0081-HKONT IS NOT INITIAL.
      WL_INPUT_ZIB-HKONT      =  WG_0081-HKONT.
*      CONCATENATE 'Ajuste moeda apresentação ' VANOMES
      CONCATENATE TEXT-016 VANOMES
      INTO WL_INPUT_ZIB-SGTXT SEPARATED BY SPACE.
    ENDIF.

    CLEAR V_BEWAR.
    SELECT SINGLE BEWAR
      INTO V_BEWAR
      FROM ZFIT0030
      WHERE HKONT EQ WL_INPUT_ZIB-HKONT
      AND   COND  EQ ''.

    WL_INPUT_ZIB-BEWAR      = V_BEWAR.
    WL_INPUT_ZIB-VBUND      = WL_SAIDA-RASSC.

    WL_INPUT_ZIB-WRBTR      = 0.

    IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-WAERS      = 'USD'.
    ELSE.
      WL_INPUT_ZIB-WAERS      = WG_T882G-CURR1.
    ENDIF.


    WL_INPUT_ZIB-BUPLA      = SPACE.

    WL_INPUT_ZIB-WAERS_I    = 'X'.

    IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-DMBTR      = WL_SAIDA-VLR_AJUST.
      IF WL_INPUT_ZIB-DMBTR LT 0.
        MULTIPLY WL_INPUT_ZIB-DMBTR BY -1.
      ENDIF.
    ELSE.
      WL_INPUT_ZIB-DMBTR      = 0.
    ENDIF.

    WL_INPUT_ZIB-WAERS_F    = WG_T882G-CURR2.
    IF S_BUKRS-LOW = '0004' OR S_BUKRS-LOW EQ '0037'.
      WL_INPUT_ZIB-DMBE2     = 0.
    ELSE.
      WL_INPUT_ZIB-DMBE2      = WL_SAIDA-VLR_AJUST.
      IF WL_INPUT_ZIB-DMBE2 LT 0.
        MULTIPLY WL_INPUT_ZIB-DMBE2 BY -1.
      ENDIF.
    ENDIF.

*    IF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR WG_T001-LAND1 EQ 'CH'.
*      WL_INPUT_ZIB-WAERS_G    = SPACE.
*      WL_INPUT_ZIB-DMBE3      = SPACE.
*    ELSEIF WG_T001-LAND1 EQ 'AR'
*        OR WG_T001-LAND1 EQ 'PY'.
*      WL_INPUT_ZIB-WAERS_G    = 'BRL'.
*      WL_INPUT_ZIB-DMBE3      = '0.01'.
*    ENDIF.
    WL_INPUT_ZIB-RG_ATUALIZADO  = 'N'.

    APPEND WL_INPUT_ZIB TO TL_INPUT_ZIB.
    CLEAR: WL_INPUT_ZIB.


    MODIFY ZIB_CONTABIL FROM TABLE TL_INPUT_ZIB.
    COMMIT WORK.
    IF WL_ESTORNO IS INITIAL.
      WL_INPUT_0082-MANDT = SY-MANDT.
      WL_INPUT_0082-BUKRS = S_BUKRS-LOW.
      WL_INPUT_0082-MES_ANO = S_MES-LOW.
      WL_INPUT_0082-SAKNR = WL_SAIDA-RACCT.
      WL_INPUT_0082-VBUND = WL_SAIDA-RASSC.
      WL_INPUT_0082-DMBTR = WL_SAIDA-CURR1.
      WL_INPUT_0082-DMBE2 = WL_SAIDA-CURR2.
      WL_INPUT_0082-DMBE3 = WL_SAIDA-CURR3.
      WL_INPUT_0082-TX_USD = WL_SAIDA-TX_USD.
      WL_INPUT_0082-TX_BRL = WL_SAIDA-TX_BRL.
      WL_INPUT_0082-SDO_CORR_MI2 = WL_SAIDA-SALDO_CORR.
      WL_INPUT_0082-VLR_CORR_MI2 = WL_SAIDA-VLR_AJUST.
      IF WG_T001-LAND1 EQ 'AR'
      OR WG_T001-LAND1 EQ 'PY'.
        WL_INPUT_0082-SDO_CORR_MI3 = WL_SAIDA-SALDO_CORR2.
        WL_INPUT_0082-VLR_CORR_MI3 = WL_SAIDA-VLR_AJUST2.
      ELSEIF WG_T001-LAND1 EQ 'BR' OR WG_T001-LAND1 EQ 'NL' OR WG_T001-LAND1 EQ 'CH'.
        CLEAR: WL_INPUT_0082-SDO_CORR_MI3, WL_INPUT_0082-VLR_CORR_MI3.
      ENDIF.
      CONCATENATE 'ZFI0061' WL_OBJ_KEY VG_RYEAR INTO WL_INPUT_0082-OBJ_KEY.
      P_OBJ_KEY = WL_INPUT_0082-OBJ_KEY.

      WL_INPUT_0082-OBJ_KEY_EST  = SPACE.
      WL_INPUT_0082-USNAM        = SY-UNAME.
      WL_INPUT_0082-AEDAT        = SY-DATUM.
      WL_INPUT_0082-CPUTM        = SY-UZEIT.

      MODIFY  ZFIT0082 FROM WL_INPUT_0082.
      COMMIT WORK.
      WG_SAIDA_EXEC-ICON   = ICON_GREEN_LIGHT.
      WG_SAIDA_EXEC-RACCT = WL_SAIDA-RACCT.
      WG_SAIDA_EXEC-TXT50 = WL_SAIDA-TXT50.
*      WG_SAIDA_EXEC-MSG   = 'Documento foi importado na ZIB_CONTABIL com sucesso.' .
      WG_SAIDA_EXEC-MSG   = TEXT-017 .
      APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
      CLEAR: WL_INPUT_0082, WG_SAIDA_EXEC.
    ELSE.
      CONCATENATE 'ZFI0061' WL_OBJ_KEY VG_RYEAR INTO WL_INPUT_0082-OBJ_KEY_EST.
      P_OBJ_KEY = WL_INPUT_0082-OBJ_KEY_EST.

      UPDATE ZFIT0082 SET OBJ_KEY_EST = WL_INPUT_0082-OBJ_KEY_EST
                      WHERE BUKRS   EQ S_BUKRS-LOW
                        AND MES_ANO EQ S_MES-LOW
                        AND SAKNR   EQ WL_SAIDA-RACCT
                        AND VBUND   EQ WL_SAIDA-RASSC.
      COMMIT WORK.

      WG_SAIDA_EXEC-ICON  = ICON_GREEN_LIGHT.
      WG_SAIDA_EXEC-RACCT = WL_SAIDA-RACCT.
      WG_SAIDA_EXEC-TXT50 = WL_SAIDA-TXT50.
*      WG_SAIDA_EXEC-MSG   = 'Documento foi marcado para estorno na ZIB_CONTABIL com sucesso.' .
      WG_SAIDA_EXEC-MSG   = TEXT-018 .
      APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.

    ENDIF.
  ELSE.
    CLEAR: WG_SAIDA_EXEC.
    WG_SAIDA_EXEC-ICON   = ICON_RED_LIGHT.
    WG_SAIDA_EXEC-RACCT  = WL_SAIDA-RACCT.
    WG_SAIDA_EXEC-TXT50  = WL_SAIDA-TXT50.
*    WG_SAIDA_EXEC-MSG    = 'Não foi possivel encontra a conta DE - PARA , na tabela ZFIT0081, para o lançamento'.
    WG_SAIDA_EXEC-MSG    = TEXT-019.
    APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
  ENDIF.
ENDFORM.                    " GERA_CONTABIL
*&---------------------------------------------------------------------*
*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
FORM GET_NEXT_NUMBER  USING    P_OBJECT   "TYPE nrobj
                               P_NR_RANGE "TYPE nrnr
*                               P_COMMIT
                      CHANGING P_NUMBER.

*  IF P_COMMIT IS NOT INITIAL.
  CLEAR P_NUMBER.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = P_NR_RANGE
      OBJECT                  = P_OBJECT
    IMPORTING
      NUMBER                  = P_NUMBER
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.
  IF SY-SUBRC NE 0.
    CLEAR: P_NUMBER.
*    MESSAGE E836(SD) WITH 'O intervalo de numeração,'
*                      'não foi encontrado!'.

    MESSAGE E836(SD) WITH TEXT-020
                          TEXT-021.

  ENDIF.
*  ELSE.
*    P_NUMBER = '$00000001'.
*    WG_FLAG = C_X.
*  ENDIF.

ENDFORM.                    " get_next_number
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IMPRIMIR_EXEC .
  PERFORM MONTAR_LAYOUT USING 'TG_SAIDA_EXEC'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     i_callback_program    = v_report
*     I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
      IT_FIELDCAT           = ESTRUTURA[]
*     IT_SORT               = T_SORT[]
      I_SAVE                = 'A'
      I_SCREEN_START_COLUMN = 3
      I_SCREEN_START_LINE   = 3
      I_SCREEN_END_COLUMN   = 100
      I_SCREEN_END_LINE     = 13
    TABLES
      T_OUTTAB              = TG_SAIDA_EXEC.
ENDFORM.                    " IMPRIMIR_EXEC
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TG_SAIDA  text
*      -->P_TL_ZIB  text
*      -->P_TL_ZIB_CHV  text
*      -->P_TL_ZIB_ERR  text
*----------------------------------------------------------------------*
FORM ATUALIZA_SAIDA  TABLES   TL_SAIDA   LIKE TG_SAIDA
                              TL_ZIB     STRUCTURE ZIB_CONTABIL
                              TL_ZIB_CHV STRUCTURE ZIB_CONTABIL_CHV
                              TL_ZIB_ERR STRUCTURE ZIB_CONTABIL_ERR.

  SORT: TL_ZIB     BY OBJ_KEY RG_ATUALIZADO,
        TL_ZIB_CHV BY OBJ_KEY,
        TL_ZIB_ERR BY OBJ_KEY.

  LOOP AT TL_SAIDA.
    IF TL_SAIDA-OBJ_KEY IS NOT INITIAL.
      CLEAR: TL_ZIB.
      READ TABLE TL_ZIB
        WITH KEY OBJ_KEY       = TL_SAIDA-OBJ_KEY
                 RG_ATUALIZADO = 'S'
                         BINARY SEARCH.

      IF SY-SUBRC IS INITIAL.
        CLEAR: TL_ZIB_CHV.
        READ TABLE TL_ZIB_CHV
        WITH KEY OBJ_KEY       = TL_SAIDA-OBJ_KEY
                         BINARY SEARCH.
        IF SY-SUBRC IS INITIAL.
          TL_SAIDA-BELNR = TL_ZIB_CHV-BELNR.
          "FB08
          SELECT SINGLE   BUKRS BELNR  GJAHR  BUDAT STBLG STJAH
            FROM BKPF
            INTO WG_BKPF_FB08
            WHERE BUKRS EQ TL_ZIB_CHV-BUKRS
            AND   BELNR EQ TL_ZIB_CHV-BELNR
            AND   GJAHR EQ TL_ZIB_CHV-GJAHR
            AND   STBLG NE ''.
          IF SY-SUBRC = 0.
            SELECT SINGLE   BUKRS BELNR GJAHR BUDAT
               FROM BKPF
               INTO WG_BKPF_FB08_E
               WHERE BUKRS EQ WG_BKPF_FB08-BUKRS
               AND   BELNR EQ WG_BKPF_FB08-STBLG
               AND   GJAHR EQ WG_BKPF_FB08-STJAH.
            IF WG_BKPF_FB08_E-BUDAT+0(6) = WG_BKPF_FB08-BUDAT+0(6). "estorno e lançamento mesmo mês, limpa pra gerar outro documento
              DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY  = TL_SAIDA-OBJ_KEY.
              CLEAR: TL_SAIDA-BELNR, TL_SAIDA-OBJ_KEY.
            ENDIF.
          ENDIF.
          TL_SAIDA-LOG   = ICON_ENTER_MORE.
          MODIFY TL_SAIDA TRANSPORTING BELNR  OBJ_KEY  LOG.
        ELSE.
          CLEAR: TL_ZIB_ERR.
          READ TABLE TL_ZIB_ERR
            WITH KEY OBJ_KEY       = TL_SAIDA-OBJ_KEY
                             BINARY SEARCH.
          IF SY-SUBRC IS INITIAL.
            TL_SAIDA-BELNR = SPACE.
            TL_SAIDA-LOG   = ICON_DISPLAY_MORE.
            MODIFY TL_SAIDA TRANSPORTING BELNR LOG.
          ENDIF.
        ENDIF.
      ELSE.
        TL_SAIDA-BELNR = ICON_OPERATION.
        MODIFY TL_SAIDA TRANSPORTING BELNR.
      ENDIF.
    ELSE.
      TL_SAIDA-BELNR = SPACE.
      TL_SAIDA-LOG   = ICON_ENTER_MORE.
      MODIFY TL_SAIDA TRANSPORTING LOG BELNR.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " ATUALIZA_SAIDA

*&---------------------------------------------------------------------*
*&      Form  REVERTE_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ESTORNA_DOCUMENTOS USING P_TIPO.

  DATA: W_CONT     TYPE I,
        VDATA(10),
        P_ERRO(1),
        V_ESTORNO  TYPE UF05A-STGRD,
        WA_ZIB_CHV TYPE ZIB_CONTABIL_CHV.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TEXT_QUESTION         = TEXT-M10
      TEXT_BUTTON_1         = TEXT-B01
      ICON_BUTTON_1         = 'ICON_OKAY'
      TEXT_BUTTON_2         = TEXT-B02
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

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  IF W_ANSWER NE '1'.
    EXIT.
  ENDIF.

  VG_RYEAR  = S_MES-LOW+2(4).
  CONCATENATE VG_RYEAR S_MES-LOW(2) '01' INTO VG_LAST_DAY_AUX.
  VG_LAST_DAY = VG_LAST_DAY_AUX.
  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      I_DATE = VG_LAST_DAY
    IMPORTING
      E_DATE = VG_LAST_DAY.

  IF P_TIPO = 'REV'.
    IF VG_LAST_DAY_AUX+0(6) = SY-DATUM+0(6).
      MESSAGE TEXT-041 TYPE 'I'.
      EXIT.
    ENDIF.
    ADD 1 TO VG_LAST_DAY.
    V_ESTORNO = '01'.
  ELSE.
    V_ESTORNO = '01'.
  ENDIF.

  LOOP AT TG_SAIDA INTO WG_SAIDA WHERE MARK IS NOT INITIAL.
    IF WG_SAIDA-BELNR IS INITIAL.
      CLEAR: WG_SAIDA_EXEC.
      WG_SAIDA_EXEC-ICON   = ICON_YELLOW_LIGHT.
      WG_SAIDA_EXEC-RACCT  = WG_SAIDA-RACCT.
      WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
      WG_SAIDA_EXEC-MSG    = TEXT-M01 .
      APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.
    ELSE.
      SELECT SINGLE *
        FROM ZIB_CONTABIL_CHV
        INTO  WA_ZIB_CHV
         WHERE OBJ_KEY EQ WG_SAIDA-OBJ_KEY.
      "FB08
      SELECT SINGLE   BUKRS BELNR  GJAHR  BUDAT STBLG STJAH
        FROM BKPF
        INTO WG_BKPF_FB08
        WHERE BUKRS EQ WA_ZIB_CHV-BUKRS
        AND   BELNR EQ WA_ZIB_CHV-BELNR
        AND   GJAHR EQ WA_ZIB_CHV-GJAHR
        AND   STBLG NE ''.
      IF SY-SUBRC = 0.
        CLEAR: WG_SAIDA_EXEC.
        WG_SAIDA_EXEC-ICON   = ICON_YELLOW_LIGHT.
        WG_SAIDA_EXEC-RACCT  = WG_SAIDA-RACCT.
        WG_SAIDA_EXEC-TXT50  = WG_SAIDA-TXT50.
        WG_SAIDA_EXEC-MSG    = TEXT-M07 .
        APPEND WG_SAIDA_EXEC TO TG_SAIDA_EXEC.

      ENDIF.
    ENDIF.

  ENDLOOP.
  "
  IF TG_SAIDA_EXEC[] IS NOT INITIAL.
    EXIT.
  ENDIF.

  LOOP AT TG_SAIDA INTO WG_SAIDA WHERE MARK IS NOT INITIAL.
    SELECT SINGLE *
        FROM ZIB_CONTABIL_CHV
        INTO  WA_ZIB_CHV
         WHERE OBJ_KEY EQ WG_SAIDA-OBJ_KEY.
    CONCATENATE  VG_LAST_DAY+6(2) '.' VG_LAST_DAY+4(2) '.' VG_LAST_DAY+0(4) INTO VDATA.

    REFRESH TI_BDCDATA.
    PERFORM F_BDC_DATA USING:
          'SAPMF05A'  '0105'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'	      '/00',
          ''          ''      ''   'RF05A-BELNS'      WA_ZIB_CHV-BELNR,
          ''          ''      ''   'BKPF-BUKRS'       WA_ZIB_CHV-BUKRS,
          ''          ''      ''   'RF05A-GJAHS'      WA_ZIB_CHV-GJAHR,
          ''          ''      ''   'UF05A-STGRD'      V_ESTORNO,
          ''          ''      ''   'BSIS-BUDAT'       VDATA,
          'SAPMF05A'  '0105'  'X'  ''                 ' ',
          ''          ''      ''   'BDC_OKCODE'	      '=BU'.

    CLEAR P_ERRO.
    VOBJ_KEY = WG_SAIDA-OBJ_KEY.
    PERFORM ZF_CALL_TRANSACTION USING 'FB08' CHANGING P_ERRO.

    IF P_ERRO IS NOT INITIAL.
      MESSAGE TEXT-M08 TYPE 'I'.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM F_BDC_DATA  USING P_PROGRAM P_DYNPRO P_START P_FNAM P_FVAL.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR WA_BDCDATA.
  WA_BDCDATA-PROGRAM   = P_PROGRAM.
  WA_BDCDATA-DYNPRO    = P_DYNPRO.
  WA_BDCDATA-DYNBEGIN  = P_START.
  WA_BDCDATA-FNAM      = P_FNAM.
  WA_BDCDATA-FVAL      = P_FVAL.
  APPEND WA_BDCDATA TO TI_BDCDATA.

ENDFORM.                    " F_BDC_DATA

FORM ZF_CALL_TRANSACTION USING P_TRANS CHANGING P_ERRO.
  CONSTANTS: C_MSGID LIKE IT_MSG-MSGID VALUE 'F5',
             C_MSGNR LIKE IT_MSG-MSGNR VALUE '312',
             C_MSGNE LIKE IT_MSG-MSGNR VALUE '539'.

  DATA: WL_CONT     TYPE SY-TABIX.

  REFRESH: IT_MSG, TG_ZIB_ERR.
  CLEAR TG_ZIB_ERR.

  WL_MODE = 'E'.

  CALL TRANSACTION P_TRANS USING TI_BDCDATA
        MODE WL_MODE
        MESSAGES INTO IT_MSG.
  CLEAR: WL_CONT.

  LOOP AT IT_MSG WHERE MSGTYP EQ 'E'.
    ADD 1 TO WL_CONT.
  ENDLOOP.
  IF WL_CONT  GT 0.
    CLEAR WL_CONT.
    DELETE FROM ZIB_CONTABIL_ERR WHERE OBJ_KEY  = VOBJ_KEY.
    LOOP AT IT_MSG WHERE MSGTYP EQ 'E'.
      ADD 1 TO WL_CONT.
      CLEAR: WL_MESSAGE.
      CALL FUNCTION 'CUTC_GET_MESSAGE'
        EXPORTING
          MSG_TYPE       = IT_MSG-MSGTYP
          MSG_ID         = IT_MSG-MSGID
          MSG_NO         = SY-MSGNO
          MSG_ARG1       = SY-MSGV1
          MSG_ARG2       = SY-MSGV2
          MSG_ARG3       = SY-MSGV3
          MSG_ARG4       = SY-MSGV4
        IMPORTING
          RAW_MESSAGE    = WL_MESSAGE
        EXCEPTIONS
          MSG_NOT_FOUND  = 1
          INTERNAL_ERROR = 2
          OTHERS         = 3.

      IF ( SY-SUBRC NE 0 ).
        WL_MESSAGE = 'Erro na mensagem do BATCH-INPUT'.
      ENDIF.

      WG_ZIB_ERR-OBJ_KEY            = VOBJ_KEY.
      WG_ZIB_ERR-NR_ITEM            = WL_CONT.
      WG_ZIB_ERR-INTERFACE          = ''.
      WG_ZIB_ERR-DT_ATUALIZACAO     = SY-DATUM.
      WG_ZIB_ERR-HR_ATUALIZACAO     = SY-UZEIT.
      WG_ZIB_ERR-TYPE               = IT_MSG-MSGTYP.
      WG_ZIB_ERR-ID                 = IT_MSG-MSGID.
      WG_ZIB_ERR-NUM                = SY-MSGNO.
      WG_ZIB_ERR-MESSAGE            = WL_MESSAGE.
      WG_ZIB_ERR-MESSAGE_V1         = IT_MSG-MSGV1.
      WG_ZIB_ERR-MESSAGE_V2         = IT_MSG-MSGV2.
      WG_ZIB_ERR-MESSAGE_V3         = IT_MSG-MSGV3.
      WG_ZIB_ERR-MESSAGE_V4         = IT_MSG-MSGV4.

      APPEND WG_ZIB_ERR TO TG_ZIB_ERR.
      CLEAR WG_ZIB_ERR.

    ENDLOOP.

    MODIFY ZIB_CONTABIL_ERR FROM TABLE TG_ZIB_ERR.
  ENDIF.

  READ TABLE IT_MSG WITH KEY MSGTYP = 'A'.
  IF SY-SUBRC = 0.
    P_ERRO = 'X'.
  ELSE.
    READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.
    IF SY-SUBRC = 0.
      P_ERRO = 'X'.
    ENDIF.
  ENDIF.

  CLEAR WG_DOCUMENTO.

  READ TABLE IT_MSG WITH KEY MSGID = C_MSGID
                             MSGNR = C_MSGNR
                             MSGTYP = 'S'.

  IF SY-SUBRC = 0.
    MOVE IT_MSG-MSGV1 TO WG_DOCUMENTO.
  ENDIF.

  IF  WG_DOCUMENTO IS INITIAL.
    P_ERRO = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_DOCUMENTO
      IMPORTING
        OUTPUT = WG_DOCUMENTO.
  ENDIF.


ENDFORM.                    "ZF_CALL_TRANSACTION
