*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski                                      &*
*& Data.....: 25/10/2013                                              &*
*& Descrição: Indicadores de Cadastro de Materiais                    &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  ZMMR039.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS, KKBLO.

TYPES: BEGIN OF TY_ZTWF_MAT_LOG,
         MATNR TYPE ZTWF_MAT_LOG-MATNR,
         WERKS TYPE ZTWF_MAT_LOG-WERKS,
         DATA  TYPE ZTWF_MAT_LOG-DATA,
         VKORG TYPE ZTWF_MAT_LOG-VKORG,
         VTWEG TYPE ZTWF_MAT_LOG-VTWEG,
         NSEQU TYPE ZTWF_MAT_LOG-NSEQU,
         HORA  TYPE ZTWF_MAT_LOG-HORA,
         USNAM TYPE ZTWF_MAT_LOG-USNAM,
         CONT  TYPE I,
         SEQ   TYPE I,
       END OF TY_ZTWF_MAT_LOG,

       BEGIN OF TY_NUREQ,
         NUREQ(12),
         QTDE      TYPE I,
       END OF TY_NUREQ,

       BEGIN OF TY_MARA,
         MATNR TYPE MARA-MATNR,
         MTART TYPE MARA-MTART,
       END OF TY_MARA,

       BEGIN OF TY_MAKT,
         MATNR TYPE MAKT-MATNR,
         MAKTX TYPE MAKT-MAKTX,
       END OF TY_MAKT,

       BEGIN OF TY_USER,
         BNAME     TYPE USR21-BNAME,
         NAME_TEXT TYPE ADRP-NAME_TEXT,
       END OF TY_USER,

       BEGIN OF TY_ZTWF_MAT_SEQ,
         MTART TYPE ZTWF_MAT_SEQ-MTART,
         WERKS TYPE ZTWF_MAT_SEQ-WERKS,
         NSEQU TYPE ZTWF_MAT_SEQ-NSEQU,
         VKORG TYPE ZTWF_MAT_SEQ-VKORG,
         VTWEG TYPE ZTWF_MAT_SEQ-VTWEG,
         AREA  TYPE ZTWF_MAT_SEQ-AREA,
       END OF TY_ZTWF_MAT_SEQ,

       BEGIN OF TY_HEADER,
         AREA   TYPE ZTWF_MAT_SEQ-AREA,
         QTD(4) TYPE C,
         TEMPO  TYPE T,
       END OF TY_HEADER,


       BEGIN OF TY_SAIDA,
         ICON(4)    TYPE C,
         WERKS      TYPE ZTWF_MAT_LOG-WERKS,
         DATA       TYPE ZTWF_MAT_LOG-DATA,
         MATNR      TYPE ZTWF_MAT_LOG-MATNR,
         MAKTX      TYPE MAKT-MAKTX,
         MTART      TYPE ZTWF_MAT_SEQ-MTART,
         AREA       TYPE ZTWF_MAT_SEQ-AREA,
         INICIO     TYPE ZTWF_MAT_LOG-HORA,
         FIM        TYPE ZTWF_MAT_LOG-HORA,
         TEMPO(10)  TYPE C,
* ---> S4 Migration - 17/07/2023 - LA
*        TEMPO_AUX  TYPE WSTN_AL_ALLOC-QTY_AL_SITE,
         TEMPO_AUX  TYPE p LENGTH 10 DECIMALS 0,
*        TEMPO_TOT  TYPE WSTN_AL_ALLOC-QTY_AL_SITE,
         TEMPO_TOT  TYPE p LENGTH 10 DECIMALS 0,
* <--- S4 Migration - 17/07/2023 - LA
         DATA_INI   TYPE SY-DATUM,
         DATA_FIM   TYPE SY-DATUM,
         STATUS(10),
         NAME_TEXT  TYPE ADRP-NAME_TEXT,
       END OF TY_SAIDA.

TYPES: BEGIN OF TY_ESTRUTURA.
         INCLUDE TYPE SLIS_FIELDCAT_MAIN.
         INCLUDE TYPE SLIS_FIELDCAT_ALV_SPEC.
       TYPES: END OF TY_ESTRUTURA.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: T_LOG            TYPE TABLE OF TY_ZTWF_MAT_LOG,
      T_LOG_ANT        TYPE TABLE OF TY_ZTWF_MAT_LOG,
      T_LOG_AUX        TYPE TABLE OF TY_ZTWF_MAT_LOG,
      T_LOG_AUX2       TYPE TABLE OF TY_ZTWF_MAT_LOG,
      T_LOG_WERKS      TYPE TABLE OF TY_ZTWF_MAT_LOG,
      T_ZMMT0069_LOG   TYPE TABLE OF ZMMT0069_LOG,
      T_ZMMT0069_AUX   TYPE TABLE OF ZMMT0069_LOG,
      T_ZMMT0069_WERKS TYPE TABLE OF ZMMT0069_LOG,
      T_MARA           TYPE TABLE OF TY_MARA,
      T_MAKT           TYPE TABLE OF TY_MAKT,
      T_USER           TYPE TABLE OF TY_USER,
      T_SEQ            TYPE TABLE OF TY_ZTWF_MAT_SEQ,
      T_HEADER         TYPE TABLE OF TY_HEADER,
      T_SAIDA          TYPE TABLE OF TY_SAIDA,
      T_SAIDA_AUX      TYPE TABLE OF TY_SAIDA,
      T_CONTADOR       TYPE TABLE OF TY_NUREQ WITH HEADER LINE.
*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: WA_LOG           TYPE TY_ZTWF_MAT_LOG,
      WA_LOG_AUX       TYPE TY_ZTWF_MAT_LOG,
      WA_LOG_AUX2      TYPE TY_ZTWF_MAT_LOG,
      W_ZMMT0069_LOG   TYPE ZMMT0069_LOG,
      W_ZMMT0069_AUX   TYPE ZMMT0069_LOG,
      W_ZMMT0069_WERKS TYPE ZMMT0069_LOG,
      WA_MARA          TYPE TY_MARA,
      WA_MAKT          TYPE TY_MAKT,
      WA_USER          TYPE TY_USER,
      WA_SEQ           TYPE TY_ZTWF_MAT_SEQ,
      WA_HEADER        TYPE TY_HEADER,
      WA_SAIDA         TYPE TY_SAIDA,
      WA_SAIDA_AUX     TYPE TY_SAIDA.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: XS_EVENTS    TYPE SLIS_ALV_EVENT,
      EVENTS       TYPE SLIS_T_EVENT,
      T_PRINT      TYPE SLIS_PRINT_ALV,
      ESTRUTURA    TYPE TABLE OF TY_ESTRUTURA,
      WA_ESTRUTURA TYPE TY_ESTRUTURA,
      V_REPORT     LIKE SY-REPID,
      T_TOP        TYPE SLIS_T_LISTHEADER,
      LT_SORT      TYPE SLIS_T_SORTINFO_ALV,
      LS_SORT      TYPE SLIS_SORTINFO_ALV,
      INIT,


      IT_HEADER    TYPE KKBLO_T_LISTHEADER WITH HEADER LINE.   "Cabeçalho
DATA : IS_LAYOUT TYPE LVC_S_LAYO.
*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: WG_HORA_TOTAL TYPE T,
      WG_HORA_UTEIS TYPE T,
      W_CONT        TYPE I,
      TABIX         TYPE SY-TABIX,
      W_DATA        TYPE SY-DATUM,
      VAR_LAYOUT(1).
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.

    METHODS HANDLE_AFTER_USER_COMMAND
                FOR EVENT AFTER_USER_COMMAND OF CL_GUI_ALV_GRID
      IMPORTING E_UCOMM.



  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_AFTER_USER_COMMAND.
    IF E_UCOMM EQ '&FILTER'.
      CLEAR: INIT.
    ENDIF.

  ENDMETHOD.                    "handle_after_user_command
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULÁRIO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_MATNR FOR WA_LOG-MATNR,
                S_MTART FOR WA_MARA-MTART,
                S_WERKS FOR WA_LOG-WERKS,
                S_DATA  FOR WA_LOG-DATA OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM SELECIONAR_DADOS.
  PERFORM SELECIONAR_DADOS_PRE.
  PERFORM ORGANIZAR_DADOS.
  PERFORM INICIAR_VARIAVEIS.
  PERFORM IMPRIMIR_DADOS.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECIONAR_DADOS .
  DATA:  WA_SELECTION TYPE RSPARAMS.
  CLEAR VAR_LAYOUT.
  SELECT SINGLE USR21~BNAME USR21~BNAME
          INTO WA_USER
           FROM USR21 JOIN ADCP ON USR21~PERSNUMBER = ADCP~PERSNUMBER AND
                                   USR21~ADDRNUMBER = ADCP~ADDRNUMBER

           WHERE USR21~BNAME = SY-UNAME
           AND   ADCP~ROOMNUMBER IN ('010010027','10010027').

  IF SY-SUBRC = 0 . "Suprimentos layout total
    VAR_LAYOUT = 'T'.
  ENDIF.

  SELECT MATNR WERKS DATA VKORG VTWEG NSEQU HORA USNAM
    FROM ZTWF_MAT_LOG
      INTO TABLE T_LOG
        WHERE MATNR IN S_MATNR
          AND WERKS IN S_WERKS
          AND DATA  IN S_DATA
          AND LOEKZ NE 'X'.

  T_LOG_WERKS[] = T_LOG[].
  SORT: T_LOG_WERKS BY WERKS .
  DELETE ADJACENT DUPLICATES FROM  T_LOG_WERKS  COMPARING WERKS.
  LOOP AT T_LOG_WERKS INTO WA_LOG_AUX.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
         ID 'WERKS' FIELD  WA_LOG_AUX-WERKS
         ID 'ACTVT' FIELD '03'.

    CASE SY-SUBRC.
      WHEN 0.

      WHEN 4.
        DELETE T_LOG WHERE WERKS = WA_LOG_AUX-WERKS.
        MESSAGE 'Sem autorização para esta filial' TYPE 'I'.
      WHEN 12.
        DELETE T_LOG WHERE WERKS = WA_LOG_AUX-WERKS.
        MESSAGE 'Sem autorização neste objeto ' TYPE 'I'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  IF T_LOG[] IS NOT INITIAL.
    SELECT MATNR MTART
      FROM MARA
        INTO TABLE T_MARA
        FOR ALL ENTRIES IN T_LOG
          WHERE MATNR EQ T_LOG-MATNR
            AND MTART IN S_MTART.

    IF SY-SUBRC IS INITIAL.
      SELECT MATNR MAKTX
        FROM MAKT
          INTO TABLE T_MAKT
          FOR ALL ENTRIES IN T_MARA
            WHERE MATNR EQ T_MARA-MATNR
              AND SPRAS EQ SY-LANGU.
    ENDIF.


    " Faz busca nos LOG unicos para identificar se é inicio ou fim de atendimento da sequencia
    T_LOG_AUX[]  = T_LOG[].
    SORT: T_LOG_AUX  BY WERKS MATNR VKORG VTWEG NSEQU DATA  HORA.
    DELETE ADJACENT DUPLICATES FROM  T_LOG_AUX  COMPARING WERKS MATNR VKORG VTWEG NSEQU.
    LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
      W_CONT = 0.
      TABIX = SY-TABIX.
      LOOP AT T_LOG INTO WA_LOG WHERE WERKS = WA_LOG_AUX-WERKS
                                       AND   MATNR = WA_LOG_AUX-MATNR
                                        AND   VKORG = WA_LOG_AUX-VKORG
                                        AND   VTWEG = WA_LOG_AUX-VTWEG
                                        AND   NSEQU = WA_LOG_AUX-NSEQU.
        ADD 1 TO W_CONT.
      ENDLOOP.
      WA_LOG_AUX-CONT = W_CONT.
      MODIFY T_LOG_AUX FROM WA_LOG_AUX INDEX TABIX TRANSPORTING CONT.
    ENDLOOP.
    DELETE T_LOG_AUX WHERE CONT NE 1.

    IF T_LOG_AUX[] IS NOT INITIAL.
      W_DATA = S_DATA-LOW - 10. "até 10 dias considera mesmo atendimento
      SELECT MATNR WERKS DATA VKORG VTWEG NSEQU HORA
      FROM ZTWF_MAT_LOG
        INTO TABLE T_LOG_AUX2
        FOR ALL ENTRIES IN T_LOG_AUX
          WHERE MATNR EQ T_LOG_AUX-MATNR
            AND WERKS EQ T_LOG_AUX-WERKS
            AND VKORG EQ T_LOG_AUX-VKORG
            AND VTWEG EQ T_LOG_AUX-VTWEG
            AND DATA  GT W_DATA
            AND DATA  LT S_DATA-LOW.

      SORT: T_LOG_AUX2  BY WERKS MATNR VKORG VTWEG ASCENDING DATA DESCENDING  NSEQU DESCENDING.

      " Se ultimo LOG é da mesma sequencia, significa que é o ultimo atendimento o primeiro select
      LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
        READ TABLE T_LOG_AUX2 INTO WA_LOG_AUX2 WITH KEY WERKS = WA_LOG_AUX-WERKS
                                                        MATNR = WA_LOG_AUX-MATNR
                                                        VKORG = WA_LOG_AUX-VKORG
                                                        VTWEG = WA_LOG_AUX-VTWEG.
        IF SY-SUBRC = 0.
          IF WA_LOG_AUX2-NSEQU = WA_LOG_AUX-NSEQU.
            APPEND WA_LOG_AUX2 TO T_LOG.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    SELECT MTART WERKS NSEQU VKORG VTWEG AREA
      FROM ZTWF_MAT_SEQ
        INTO TABLE T_SEQ
        FOR ALL ENTRIES IN T_LOG
          WHERE WERKS EQ T_LOG-WERKS
            AND NSEQU EQ T_LOG-NSEQU
            AND VKORG EQ T_LOG-VKORG
            AND VTWEG EQ T_LOG-VTWEG.

    T_LOG_AUX[]  = T_LOG[].
    T_LOG_AUX2[] = T_LOG[].

    IF T_LOG[] IS NOT INITIAL.
      SELECT USR21~BNAME ADRP~NAME_TEXT
          INTO TABLE T_USER
           FROM USR21 JOIN ADRP ON USR21~PERSNUMBER = ADRP~PERSNUMBER AND
                                   ADRP~NATION      = ''
           FOR ALL ENTRIES IN T_LOG
           WHERE USR21~BNAME = T_LOG-USNAM.
    ENDIF.
  ENDIF.



ENDFORM.                    " SELECIONAR_DADOS

FORM SELECIONAR_DADOS_PRE .

  DATA TABIX   TYPE SY-TABIX.
  DATA TABIXF  TYPE SY-TABIX.
  DATA VCONTA  TYPE I.
  DATA VNUREQ  TYPE ZMMT0069_LOG-NUREQ.


  SELECT *
    FROM ZMMT0069_LOG
      INTO TABLE T_ZMMT0069_LOG
        WHERE MATNR IN S_MATNR
          AND WERKS IN S_WERKS
          AND DATA  IN S_DATA
          AND ELIMINADO NE 'X'.

  CHECK T_ZMMT0069_LOG[] IS NOT INITIAL.
  SELECT *
   FROM ZMMT0069_LOG
     INTO TABLE T_ZMMT0069_AUX
    FOR ALL ENTRIES IN T_ZMMT0069_LOG
    WHERE NUREQ = T_ZMMT0069_LOG-NUREQ
    AND   ( MATNR = ' ' OR MATNR = '000000000000000000' ).


  SORT: T_ZMMT0069_LOG BY NUREQ ASCENDING MATNR DESCENDING.

  LOOP AT T_ZMMT0069_AUX INTO W_ZMMT0069_AUX.
    TABIX = SY-TABIX.
    READ TABLE T_ZMMT0069_LOG INTO W_ZMMT0069_LOG WITH KEY NUREQ = W_ZMMT0069_AUX-NUREQ BINARY SEARCH.
    IF W_ZMMT0069_LOG-MATNR IS NOT INITIAL.
      W_ZMMT0069_AUX-MATNR = W_ZMMT0069_LOG-MATNR.
      MODIFY T_ZMMT0069_AUX FROM W_ZMMT0069_AUX INDEX TABIX TRANSPORTING MATNR.
    ENDIF.
  ENDLOOP.

  DELETE T_ZMMT0069_AUX WHERE MATNR IS INITIAL.
  LOOP AT T_ZMMT0069_AUX INTO W_ZMMT0069_AUX.
    APPEND W_ZMMT0069_AUX TO T_ZMMT0069_LOG.
  ENDLOOP.

  T_ZMMT0069_WERKS[] = T_ZMMT0069_LOG[].
  SORT: T_ZMMT0069_WERKS BY WERKS .
  DELETE ADJACENT DUPLICATES FROM  T_ZMMT0069_WERKS  COMPARING WERKS.
  LOOP AT T_ZMMT0069_WERKS INTO W_ZMMT0069_WERKS.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
         ID 'WERKS' FIELD  W_ZMMT0069_WERKS-WERKS
         ID 'ACTVT' FIELD '03'.

    CASE SY-SUBRC.
      WHEN 0.

      WHEN 4.
        DELETE T_ZMMT0069_LOG WHERE WERKS = WA_LOG_AUX-WERKS.
        MESSAGE 'Sem autorização para esta filial' TYPE 'I'.
      WHEN 12.
        DELETE T_ZMMT0069_LOG WHERE WERKS = WA_LOG_AUX-WERKS.
        MESSAGE 'Sem autorização neste objeto ' TYPE 'I'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  IF T_ZMMT0069_LOG[] IS NOT INITIAL.
    SELECT MATNR MTART
      FROM MARA
        APPENDING TABLE T_MARA
        FOR ALL ENTRIES IN T_ZMMT0069_LOG
          WHERE MATNR EQ T_ZMMT0069_LOG-MATNR
            AND MTART IN S_MTART.

    IF SY-SUBRC IS INITIAL.
      SELECT MATNR MAKTX
        FROM MAKT
          APPENDING TABLE T_MAKT
          FOR ALL ENTRIES IN T_MARA
            WHERE MATNR EQ T_MARA-MATNR
              AND SPRAS EQ SY-LANGU.
    ENDIF.


    IF  T_ZMMT0069_LOG[] IS NOT INITIAL.
      SELECT USR21~BNAME ADRP~NAME_TEXT
          APPENDING TABLE T_USER
           FROM USR21 JOIN ADRP ON USR21~PERSNUMBER = ADRP~PERSNUMBER AND
                                   ADRP~NATION      = ''
           FOR ALL ENTRIES IN T_ZMMT0069_LOG
           WHERE USR21~BNAME =  T_ZMMT0069_LOG-USNAM.
    ENDIF.
  ENDIF.

  "Contagem do tempo após a ultima recusa
  LOOP AT T_ZMMT0069_LOG INTO W_ZMMT0069_LOG.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = W_ZMMT0069_LOG-NUREQ
      IMPORTING
        OUTPUT = T_CONTADOR-NUREQ.
    IF W_ZMMT0069_LOG-RECUSA_FLAG = 'X'.
      T_CONTADOR-QTDE = 1.
      COLLECT T_CONTADOR.
    ENDIF.
  ENDLOOP.

  SORT: T_ZMMT0069_LOG BY NUREQ DATA HORA,
        T_CONTADOR     BY NUREQ.

  CLEAR: VCONTA, VNUREQ.
  LOOP AT T_ZMMT0069_LOG INTO W_ZMMT0069_LOG.
    TABIX = SY-TABIX.
    READ TABLE T_CONTADOR  WITH KEY NUREQ = W_ZMMT0069_LOG-NUREQ BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    IF VNUREQ NE W_ZMMT0069_LOG-NUREQ.
      VCONTA = 0.
      READ TABLE T_CONTADOR  WITH KEY NUREQ = W_ZMMT0069_LOG-NUREQ BINARY SEARCH.
      TABIXF  = T_CONTADOR-QTDE.
      VNUREQ  = W_ZMMT0069_LOG-NUREQ.
    ENDIF.
    IF W_ZMMT0069_LOG-RECUSA_FLAG = 'X'.
      ADD 1 TO VCONTA.
    ENDIF.
    IF VCONTA LT TABIXF.
      W_ZMMT0069_LOG-RECUSA_FLAG = 'X'.
      MODIFY T_ZMMT0069_LOG FROM W_ZMMT0069_LOG INDEX TABIX TRANSPORTING RECUSA_FLAG.
    ENDIF.
  ENDLOOP.

  DELETE T_ZMMT0069_LOG WHERE RECUSA_FLAG = 'X'.

  REFRESH T_CONTADOR.
  LOOP AT T_ZMMT0069_LOG INTO W_ZMMT0069_LOG.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = W_ZMMT0069_LOG-NUREQ
      IMPORTING
        OUTPUT = T_CONTADOR-NUREQ.

    T_CONTADOR-QTDE = 1.
    COLLECT T_CONTADOR.
  ENDLOOP.

  SORT: T_ZMMT0069_LOG BY NUREQ DATA HORA,
        T_CONTADOR     BY NUREQ.


  CLEAR: VCONTA, VNUREQ.
  LOOP AT T_ZMMT0069_LOG INTO W_ZMMT0069_LOG.
    ADD 1 TO VCONTA.
    IF VNUREQ NE W_ZMMT0069_LOG-NUREQ.
      VCONTA = 1.
      READ TABLE T_CONTADOR  WITH KEY NUREQ = W_ZMMT0069_LOG-NUREQ BINARY SEARCH.
      TABIXF  = T_CONTADOR-QTDE.
      VNUREQ  = W_ZMMT0069_LOG-NUREQ.
    ENDIF.
    IF VCONTA = 1 OR TABIXF = VCONTA.
      WA_LOG-MATNR = W_ZMMT0069_LOG-MATNR.
      WA_LOG-WERKS = W_ZMMT0069_LOG-WERKS.
      WA_LOG-DATA  = W_ZMMT0069_LOG-DATA.
      WA_LOG-VKORG = W_ZMMT0069_LOG-VKORG.
      WA_LOG-VTWEG = W_ZMMT0069_LOG-VTWEG.
      WA_LOG-NSEQU = 0. "PRE CADASTRO
      WA_LOG-HORA  = W_ZMMT0069_LOG-HORA.
      WA_LOG-USNAM = W_ZMMT0069_LOG-USNAM.
      WA_LOG-CONT  = 0.
      WA_LOG-SEQ   = 0.
      APPEND WA_LOG TO T_LOG.
      "
      WA_SEQ-MTART =  W_ZMMT0069_LOG-MTART.
      WA_SEQ-WERKS =  W_ZMMT0069_LOG-WERKS.
      WA_SEQ-NSEQU = 0.
      WA_SEQ-VKORG =  W_ZMMT0069_LOG-VKORG.
      WA_SEQ-VTWEG =  W_ZMMT0069_LOG-VTWEG.
      WA_SEQ-AREA  = 'PRE_CADASTRO'.
      APPEND  WA_SEQ TO T_SEQ.
    ENDIF.
  ENDLOOP.

  T_LOG_AUX[]  = T_LOG[].
  T_LOG_AUX2[] = T_LOG[].


ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORGANIZAR_DADOS .

  DATA: VSEQ  TYPE I,
        VCONT TYPE I.

  SORT: T_LOG_AUX  BY WERKS MATNR VKORG VTWEG NSEQU DATA  HORA,
        T_LOG_AUX2 BY WERKS MATNR VKORG VTWEG NSEQU DATA  HORA,
        T_USER     BY BNAME,
        T_SEQ      BY MTART WERKS NSEQU VKORG VTWEG.
  DELETE ADJACENT DUPLICATES FROM  T_SEQ  COMPARING MTART WERKS NSEQU VKORG VTWEG.

  DELETE ADJACENT DUPLICATES FROM  T_LOG_AUX  COMPARING WERKS MATNR VKORG VTWEG NSEQU.

  LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
    VSEQ  = 1.
    VCONT = 0.
    LOOP AT T_LOG_AUX2 INTO WA_LOG_AUX2 WHERE WERKS = WA_LOG_AUX-WERKS
                                         AND  MATNR = WA_LOG_AUX-MATNR
                                         AND  VKORG = WA_LOG_AUX-VKORG
                                         AND  VTWEG = WA_LOG_AUX-VTWEG
                                         AND  NSEQU = WA_LOG_AUX-NSEQU.
      ADD 1 TO  VCONT.
      IF VCONT = 3.
        VCONT = 1.
        ADD 1 TO VSEQ.
      ENDIF.
      WA_LOG_AUX2-SEQ = VSEQ.
      MODIFY T_LOG_AUX2 FROM WA_LOG_AUX2 INDEX SY-TABIX TRANSPORTING SEQ.
    ENDLOOP.
  ENDLOOP.

  T_LOG_AUX[] = T_LOG_AUX2[].
  SORT: T_LOG_AUX  BY WERKS MATNR VKORG VTWEG  NSEQU SEQ DATA  HORA,
        T_LOG_AUX2 BY WERKS MATNR VKORG VTWEG  NSEQU SEQ DATA  HORA.

  DELETE ADJACENT DUPLICATES FROM  T_LOG_AUX  COMPARING WERKS MATNR VKORG VTWEG NSEQU SEQ.

  LOOP AT T_LOG_AUX INTO WA_LOG_AUX.

    READ TABLE T_MARA INTO WA_MARA
      WITH KEY MATNR = WA_LOG_AUX-MATNR.

    IF SY-SUBRC IS INITIAL.
      READ TABLE T_MAKT INTO WA_MAKT
        WITH KEY MATNR = WA_MARA-MATNR.

      WA_SAIDA-MAKTX = WA_MAKT-MAKTX.

      LOOP AT  T_SEQ INTO WA_SEQ WHERE MTART = WA_MARA-MTART
                                 AND   WERKS = WA_LOG_AUX-WERKS
                                 AND   NSEQU = WA_LOG_AUX-NSEQU
                                 AND   VKORG = WA_LOG_AUX-VKORG
                                 AND   VTWEG = WA_LOG_AUX-VTWEG.
        " obtem Data inicio e Data Fim
        CLEAR: WA_SAIDA-DATA_FIM, WA_SAIDA-FIM.
        CLEAR: WA_SAIDA-DATA_INI, WA_SAIDA-INICIO.
        LOOP AT T_LOG_AUX2 INTO WA_LOG_AUX2 WHERE WERKS = WA_LOG_AUX-WERKS
                                            AND   MATNR = WA_LOG_AUX-MATNR
                                            AND   VKORG = WA_LOG_AUX-VKORG
                                            AND   VTWEG = WA_LOG_AUX-VTWEG
                                            AND   NSEQU = WA_LOG_AUX-NSEQU
                                            AND   SEQ   = WA_LOG_AUX-SEQ.
          IF WA_SAIDA-DATA_INI IS INITIAL. "1
            WA_SAIDA-DATA_INI    = WA_LOG_AUX2-DATA.
            WA_SAIDA-INICIO      = WA_LOG_AUX2-HORA.
          ENDIF.
        ENDLOOP.
        IF WA_LOG_AUX2-DATA = WA_SAIDA-DATA_INI AND
           WA_LOG_AUX2-HORA = WA_SAIDA-INICIO.
          WA_SAIDA-DATA_FIM = SY-DATUM.
          WA_SAIDA-FIM      = SY-UZEIT.
          WA_SAIDA-ICON     = ICON_INCOMPLETE.
          WA_SAIDA-STATUS   = 'Pendente'.
          READ TABLE T_USER INTO WA_USER WITH KEY BNAME = WA_LOG_AUX2-USNAM BINARY SEARCH.
          IF SY-SUBRC = 0.
            WA_SAIDA-NAME_TEXT = WA_USER-NAME_TEXT.
          ENDIF.
        ELSE.
          WA_SAIDA-STATUS      = 'Finalizado'.
          WA_SAIDA-DATA_FIM    = WA_LOG_AUX2-DATA.
          WA_SAIDA-FIM         = WA_LOG_AUX2-HORA.
          READ TABLE T_USER INTO WA_USER WITH KEY BNAME = WA_LOG_AUX2-USNAM BINARY SEARCH.
          IF SY-SUBRC = 0.
            WA_SAIDA-NAME_TEXT = WA_USER-NAME_TEXT.
          ENDIF.
        ENDIF.

        PERFORM CALCULO_DE_TEMPO.

        WA_SAIDA-MTART  = WA_SEQ-MTART.
        WA_SAIDA-AREA   = WA_SEQ-AREA.
        WA_SAIDA-WERKS  = WA_LOG_AUX-WERKS.
        WA_SAIDA-DATA   = WA_LOG_AUX-DATA.
        WA_SAIDA-MATNR  = WA_LOG_AUX-MATNR.
        WA_SAIDA-TEMPO  = WG_HORA_TOTAL.
        WA_SAIDA-TEMPO_AUX = WA_SAIDA-TEMPO.
        APPEND WA_SAIDA TO T_SAIDA.
      ENDLOOP.
    ENDIF.
    CLEAR: WA_SAIDA, WA_SEQ, WA_MAKT, WA_MARA, WA_LOG_AUX2.
  ENDLOOP.

  T_SAIDA_AUX[] = T_SAIDA[].
  SORT T_SAIDA_AUX BY WERKS MATNR.
  DELETE ADJACENT DUPLICATES FROM T_SAIDA_AUX COMPARING WERKS MATNR.
  LOOP AT T_SAIDA_AUX INTO WA_SAIDA_AUX.
    CLEAR WA_SAIDA_AUX-TEMPO_TOT.
    LOOP AT T_SAIDA INTO WA_SAIDA WHERE WERKS = WA_SAIDA_AUX-WERKS
                                  AND   MATNR = WA_SAIDA_AUX-MATNR.
      ADD WA_SAIDA-TEMPO_AUX TO WA_SAIDA_AUX-TEMPO_TOT.
    ENDLOOP.

    LOOP AT T_SAIDA INTO WA_SAIDA WHERE WERKS = WA_SAIDA_AUX-WERKS
                                  AND   MATNR = WA_SAIDA_AUX-MATNR.
      WA_SAIDA-TEMPO_TOT = WA_SAIDA_AUX-TEMPO_TOT.
      MODIFY  T_SAIDA FROM  WA_SAIDA INDEX SY-TABIX TRANSPORTING TEMPO_TOT.
    ENDLOOP.

  ENDLOOP.
ENDFORM.                    " ORGANIZAR_DADOS

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
*  PERFORM DEFINIR_EVENTOS.
  PERFORM DEFINIR_EVENTOS.
  PERFORM MONTAR_LAYOUT.

  ADD 1 TO LS_SORT-SPOS.
  LS_SORT-FIELDNAME = 'WERKS'.
  LS_SORT-UP        = 'X'.
  LS_SORT-SUBTOT    = 'X'.
  APPEND LS_SORT TO LT_SORT.

  ADD 1 TO LS_SORT-SPOS.
  LS_SORT-FIELDNAME = 'MATNR'.
  LS_SORT-UP        = 'X'.
  LS_SORT-SUBTOT    = 'X'.
  APPEND LS_SORT TO LT_SORT.

*  WL_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  CLEAR: INIT.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM          = SY-REPID
*     IS_VARIANT                  = GS_VARIANT_C
      I_CALLBACK_HTML_TOP_OF_PAGE = 'HTML_TOP_OF_PAGE'
      I_CALLBACK_TOP_OF_PAGE      = 'XCALLBACK_TOP_OF_PAGE'
      I_HTML_HEIGHT_TOP           = '40'
*     I_CALLBACK_USER_COMMAND     = 'XUSER_COMMAND' "sem 2º click
      IT_FIELDCAT                 = ESTRUTURA[]
      IS_LAYOUT                   = WL_LAYOUT
      I_SAVE                      = 'X'
      IT_EVENTS                   = EVENTS
      IS_PRINT                    = T_PRINT
      IT_SORT                     = LT_SORT
    TABLES
      T_OUTTAB                    = T_SAIDA.



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

  PERFORM F_CARREGAR_EVENTOS USING:
*                                   SLIS_EV_USER_COMMAND 'XUSER_COMMAND', "para tira duplo click
                                   SLIS_EV_PF_STATUS_SET 'XPF_STATUS_SET',
                                   SLIS_EV_TOP_OF_LIST 'TOP_OF_LIST'.
*                                   SLIS_EV_TOP_OF_PAGE  'XTOP_OF_PAGE'.
*                                   SLIS_EV_SUBTOTAL_TEXT  'SUBTOTAL_TEXT'.


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
FORM MONTAR_LAYOUT.
  IF VAR_LAYOUT = 'T'.
    PERFORM MONTAR_ESTRUTURA USING:
          "1  ' '                        ' '              'T_SAIDA' 'ICON'          ' '                     '12' ,
          1  'ZTWF_MAT_LOG'             'WERKS'          'T_SAIDA' 'WERKS'         ' '                     '12' ,
          2  'ZTWF_MAT_LOG'             'DATA'           'T_SAIDA' 'DATA'          'Data início '          ' ' ,
          3  'ZTWF_MAT_LOG'             'DATA'           'T_SAIDA' 'DATA_FIM'      'Data término'          ' ' ,
          4  'ZTWF_MAT_LOG'             'MATNR'          'T_SAIDA' 'MATNR'         ' '                     ' ' ,
          5  'MAKT'                     'MAKTX'          'T_SAIDA' 'MAKTX'         ' '                     ' ' ,
          6  'ZTWF_MAT_SEQ'             'MTART'          'T_SAIDA' 'MTART'         'Tipo. Mat'             '10' ,
          6  'ZTWF_MAT_SEQ'             'AREA'           'T_SAIDA' 'AREA'          'Área'                  ' ' ,
          7  'ZTWF_MAT_LOG'             'HORA'           'T_SAIDA' 'INICIO'        'Início atendimento'    '15' ,
          8  'ZTWF_MAT_LOG'             'HORA'           'T_SAIDA' 'FIM'           'Fim atendimento'       '15' ,
          9  ' '                        ' '              'T_SAIDA' 'TEMPO'         'Tempo atendimento'     '18' ,
* ---> S4 Migration - 17/07/2023 - LA
*          9  'WSTN_AL_ALLOC'            'QTY_AL_SITE'    'T_SAIDA' 'TEMPO_AUX'     'D-Tempo atendimento'   '20' ,
          9  ' '                        ' '              'T_SAIDA' 'TEMPO_AUX'     'D-Tempo atendimento'   '20' ,
* <--- S4 Migration - 17/07/2023 - LA
          9  ' '                        ' '              'T_SAIDA' 'TEMPO_TOT'     'Tempo Total'           '20' ,
          9  ' '                        ' '              'T_SAIDA' 'STATUS'        'Status Atendimento'    '20' ,
          9  ' '                        ' '              'T_SAIDA' 'NAME_TEXT'     'Nome Usuário'          '20' .
  ELSE.
    PERFORM MONTAR_ESTRUTURA USING:
              "1  ' '                        ' '              'T_SAIDA' 'ICON'          ' '                     '12' ,
              1  'ZTWF_MAT_LOG'             'WERKS'          'T_SAIDA' 'WERKS'         ' '                     '12' ,
              2  'ZTWF_MAT_LOG'             'DATA'           'T_SAIDA' 'DATA'          'Data início '          ' ' ,
              3  'ZTWF_MAT_LOG'             'DATA'           'T_SAIDA' 'DATA_FIM'      'Data término'          ' ' ,
              4  'ZTWF_MAT_LOG'             'MATNR'          'T_SAIDA' 'MATNR'         ' '                     ' ' ,
              5  'MAKT'                     'MAKTX'          'T_SAIDA' 'MAKTX'         ' '                     ' ' ,
*              6  'ZTWF_MAT_SEQ'             'MTART'          'T_SAIDA' 'MTART'         'Tipo. Mat'             '10' ,
              6  'ZTWF_MAT_SEQ'             'AREA'           'T_SAIDA' 'AREA'          'Área'                  ' ' ,
*              7  'ZTWF_MAT_LOG'             'HORA'           'T_SAIDA' 'INICIO'        'Início atendimento'    '15' ,
*              8  'ZTWF_MAT_LOG'             'HORA'           'T_SAIDA' 'FIM'           'Fim atendimento'       '15' ,
*              9  ' '                        ' '              'T_SAIDA' 'TEMPO'         'Tempo atendimento'     '18' ,
*              9  'WSTN_AL_ALLOC'            'QTY_AL_SITE'    'T_SAIDA' 'TEMPO_AUX'     'D-Tempo atendimento'   '20' ,
*              9  ' '                        ' '              'T_SAIDA' 'TEMPO_TOT'     'Tempo Total'           '20' ,
              9  ' '                        ' '              'T_SAIDA' 'STATUS'        'Status Atendimento'    '20' .
*              9  ' '                        ' '              'T_SAIDA' 'NAME_TEXT'     'Nome Usuário'          '20' .
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
                            VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                            VALUE(P_OUTPUTLEN).

  CLEAR WA_ESTRUTURA.
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

  IF P_FIELD EQ 'TEMPO'.
    WA_ESTRUTURA-INTTYPE = 'I'.
    "WA_ESTRUTURA-DO_SUM = 'X'.
    WA_ESTRUTURA-EDIT_MASK = 'RR____:__:__'.
  ENDIF.

  IF P_FIELD EQ 'TEMPO_AUX' OR P_FIELD EQ 'TEMPO_TOT'.
    WA_ESTRUTURA-EDIT_MASK = 'RR____:__:__'.
  ENDIF.

  IF P_FIELD EQ 'ICON'.
    WA_ESTRUTURA-ICON = 'X'.
  ENDIF.
  TRANSLATE  WA_ESTRUTURA-FIELDNAME     TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-TABNAME       TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_TABNAME   TO UPPER CASE.
  TRANSLATE  WA_ESTRUTURA-REF_FIELDNAME TO UPPER CASE.

  APPEND WA_ESTRUTURA TO ESTRUTURA.

ENDFORM.                    " MONTAR_ESTRUTURA
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

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INICIAR_VARIAVEIS.
  DATA: WL_RESUMO(200),
        WL_CUSTEIO(200),
        WL_FISCAL(200),
        WL_HELP_DESK(200).

  V_REPORT = SY-REPID.
*  WRITE '000000' USING EDIT MASK
*  CONCATENATE 'Resumo:' 'Total de atendimentos:' 'Média de tempo:' INTO WL_RESUMO SEPARATED BY SPACE.
*  CONCATENATE 'CUSTEIO' 'teste ' INTO WL_CUSTEIO SEPARATED BY SPACE.
*  PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-002.
*  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_RESUMO.
*  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_CUSTEIO.
*  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_FISCAL.
*  PERFORM F_CONSTRUIR_CABECALHO USING 'S' WL_HELP_DESK.



ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  CALCULO_DE_TEMPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALCULO_DE_TEMPO .
  DATA: WL_SAB_DOM_FER   TYPE TABLE OF ISCAL_DAY WITH HEADER LINE,
        WL_N_UTEIS       TYPE SY-INDEX,
        WL_DATA_FIM      TYPE SY-DATUM,
        WL_DIAS(4)       TYPE C,
        WL_A             TYPE T,
        WL_B             TYPE T,
        WL_HORA_TRABALHO TYPE T,
        WL_HORA_AUX(10)  TYPE C,
        WL_TESTE         TYPE T VALUE '113000',
        WL_TESTE2        TYPE T,
        L                TYPE I,
        J(20)            TYPE C,
        K(20)            TYPE C,
        V_HORA           TYPE I,
        V_RESTO          TYPE I,
        V_MINUTO         TYPE I,
        V_SEGUNDO        TYPE I,
        C_HORA(2),
        C_MINUTO(2),
        C_SEGUNDO(2),
        WL_INICIO_EXP    TYPE T,
        WL_FIM_EXP       TYPE T.
*           WL_HORA_TRABALHO TYPE I.


  CLEAR: WL_SAB_DOM_FER, WL_N_UTEIS, WL_DATA_FIM,
         WL_DIAS, WL_A, WL_B, WG_HORA_TOTAL.

  WL_HORA_AUX = '31680'. "8h e 48 min
* Cálculo de finais de semana e feriados
  CALL FUNCTION 'HOLIDAY_GET'
    EXPORTING
*     HOLIDAY_CALENDAR           = ' '
      FACTORY_CALENDAR           = 'ZF'
      DATE_FROM                  = WA_SAIDA-DATA_INI
      DATE_TO                    = WA_SAIDA-DATA_FIM
    TABLES
      HOLIDAYS                   = WL_SAB_DOM_FER
    EXCEPTIONS
      FACTORY_CALENDAR_NOT_FOUND = 1
      HOLIDAY_CALENDAR_NOT_FOUND = 2
      DATE_HAS_INVALID_FORMAT    = 3
      DATE_INCONSISTENCY         = 4
      OTHERS                     = 5.

  DESCRIBE TABLE WL_SAB_DOM_FER LINES WL_N_UTEIS.

  WL_DATA_FIM = WA_SAIDA-DATA_FIM.

  SUBTRACT WL_N_UTEIS FROM WL_DATA_FIM.

** Cálculo dos dias
*  CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
*    EXPORTING
*      I_DATUM_BIS             = WA_SAIDA-DATA_INI
*      I_DATUM_VON             = WL_DATA_FIM
*    IMPORTING
*      E_TAGE                  = WL_DIAS
*    EXCEPTIONS
*      DAYS_METHOD_NOT_DEFINED = 1
*      OTHERS                  = 2.

  WL_DIAS = WL_DATA_FIM - WA_SAIDA-DATA_INI.

  IF WL_DIAS LT 0.
    MULTIPLY WL_DIAS BY -1.
  ENDIF.

  IF WL_DIAS GE 1.
    SUBTRACT 1 FROM WL_DIAS.
  ENDIF.

* dias cheios
  MULTIPLY WL_HORA_AUX BY WL_DIAS.

  IF WL_HORA_AUX GT 356400. " 99 Horas
    WL_HORA_AUX = '356400'.
  ENDIF.

  L = WL_HORA_AUX.

  V_HORA = L DIV 3600.
  V_RESTO = L MOD 3600.
  V_MINUTO = V_RESTO DIV 60.
  V_SEGUNDO = V_RESTO MOD 60.

  C_HORA = V_HORA.
  C_MINUTO = V_MINUTO.
  C_SEGUNDO = V_SEGUNDO.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = C_HORA
    IMPORTING
      OUTPUT = C_HORA.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = C_MINUTO
    IMPORTING
      OUTPUT = C_MINUTO.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = C_SEGUNDO
    IMPORTING
      OUTPUT = C_SEGUNDO.

  CONCATENATE C_HORA C_MINUTO C_SEGUNDO INTO WG_HORA_UTEIS.

  WL_INICIO_EXP = '071500'.
  WL_FIM_EXP    = '173300'.

  IF WA_SAIDA-DATA_INI EQ WA_SAIDA-DATA_FIM.
    "
    IF WA_SAIDA-INICIO LT '071500'.
      WL_A =  WA_SAIDA-FIM - WL_INICIO_EXP.
    ELSE.
      WL_A = WA_SAIDA-FIM - WA_SAIDA-INICIO.
    ENDIF.

    WG_HORA_TOTAL = WL_A.

    IF WA_SAIDA-INICIO BETWEEN '113000' AND '130000'.
      WL_TESTE2 = '013000'.
      SUBTRACT WL_TESTE2 FROM WG_HORA_TOTAL.
    ENDIF.

    IF WA_SAIDA-INICIO LT '113000' AND WA_SAIDA-FIM GT '130000'.
      WL_TESTE2 = '013000'.
      SUBTRACT WL_TESTE2 FROM WG_HORA_TOTAL.
    ENDIF.

  ELSE.
    "horas do dia inicio
    CLEAR: WL_A , WG_HORA_TOTAL.
    IF WA_SAIDA-INICIO LT WL_FIM_EXP.
      WL_A = WL_FIM_EXP - WA_SAIDA-INICIO.
      WG_HORA_TOTAL = WL_A.
      IF WA_SAIDA-INICIO BETWEEN '113000' AND '130000'.
        WL_TESTE2 = '013000'.
        SUBTRACT WL_TESTE2 FROM WG_HORA_TOTAL.
      ENDIF.

      IF WA_SAIDA-INICIO LT '113000'.
        WL_TESTE2 = '013000'.
        SUBTRACT WL_TESTE2 FROM WG_HORA_TOTAL.
      ENDIF.

    ENDIF.

    "horas do dia fim
    WL_B =  WA_SAIDA-FIM - WL_INICIO_EXP.
    WG_HORA_TOTAL = WG_HORA_TOTAL +  WL_B.
    IF WA_SAIDA-FIM GT '130000'.
      WL_TESTE2 = '013000'.
      SUBTRACT WL_TESTE2 FROM WG_HORA_TOTAL.
    ENDIF.
  ENDIF.

  IF WG_HORA_UTEIS = '990000'.
    WG_HORA_TOTAL = WG_HORA_UTEIS.
  ELSE.
    C_HORA    = WG_HORA_TOTAL+0(2).
    C_MINUTO  = WG_HORA_TOTAL+2(2).
    C_SEGUNDO = WG_HORA_TOTAL+4(2).
    "
    V_HORA = C_HORA.
    V_MINUTO = C_MINUTO.
    V_SEGUNDO = C_SEGUNDO.
    "
    V_HORA = V_HORA * 3600.
    V_MINUTO = V_MINUTO * 60.

    WL_HORA_AUX = WL_HORA_AUX  + V_HORA + V_MINUTO + V_SEGUNDO.

    IF WL_HORA_AUX GT 356400. " 99 Horas
      WL_HORA_AUX = '356400'.
    ENDIF.


    L = WL_HORA_AUX.

    V_HORA = L DIV 3600.
    V_RESTO = L MOD 3600.
    V_MINUTO = V_RESTO DIV 60.
    V_SEGUNDO = V_RESTO MOD 60.

    C_HORA = V_HORA.
    C_MINUTO = V_MINUTO.
    C_SEGUNDO = V_SEGUNDO.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = C_HORA
      IMPORTING
        OUTPUT = C_HORA.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = C_MINUTO
      IMPORTING
        OUTPUT = C_MINUTO.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = C_SEGUNDO
      IMPORTING
        OUTPUT = C_SEGUNDO.

    CONCATENATE C_HORA C_MINUTO C_SEGUNDO INTO  WG_HORA_TOTAL.
  ENDIF.

ENDFORM.                    " CALCULO_DE_TEMPO
*&---------------------------------------------------------------------*
*&      Form  html_top_of_page
*&---------------------------------------------------------------------*
*       TO WRITE THE HEADER AS HTML
*----------------------------------------------------------------------*
FORM HTML_TOP_OF_PAGE USING R_TOP TYPE REF TO CL_DD_DOCUMENT.
  DATA: TEXT TYPE SDYDO_TEXT_ELEMENT.
  DATA: S_TABLE TYPE REF TO CL_DD_TABLE_ELEMENT.
  DATA: COL_RESUMO TYPE REF TO CL_DD_AREA.
  DATA: COL_TOTAL_ATEND TYPE REF TO CL_DD_AREA.
  DATA: COL_MEDIA_TEMPO TYPE REF TO CL_DD_AREA.
  DATA: COL_TEMPO_TOTAL TYPE REF TO CL_DD_AREA.
  DATA: COL_TOTAIS TYPE REF TO CL_DD_AREA.
  DATA: A_LOGO TYPE REF TO CL_DD_AREA.
  DATA: LOGO TYPE SDYDO_VALUE.
  DATA: LINK_AREA TYPE REF TO CL_DD_AREA.

  DATA: WL_TEMPO_SEGUNDOS(10)    TYPE C,
        WL_TEMPO_TOTAL(10)       TYPE C,
        WL_HORAS(4)              TYPE C,
        WL_MINUTOS(2)            TYPE C,
        WL_SEGUNDOS(2)           TYPE C,
        L(10)                    TYPE P DECIMALS 0, "TYPE I ,
        J(20)                    TYPE C,
        K(20)                    TYPE C,
        HORA(10)                 TYPE C,
        RESTO_HORA               TYPE I,
        MINUTO(2)                TYPE N,
        RESTO_MINUTO             TYPE I,
        SEGUNDO(2)               TYPE N,
        WL_HORA_AUX(4)           TYPE C,
        WL_HORA_TOTAL            TYPE T,
        WL_CONT_AUX              TYPE SY-INDEX,
        WL_TEMPO_MASK(10),
        WL_TOTAL_ATENDIMENTOS(8),
        WL_TOTAL_MEDIA(15),
        WL_MEDIA(8)  ,
        WL_MEDIA2(8),
        WL_TOTAL_TEMPO(10)       TYPE C,
        WL_HORA_AUX2(10),
        WL_HORA_AUX3(10),
        WL_AUX(10)               TYPE C,
        WL_AUX2(10)              TYPE C,
        HORAS_ADD(10),
        HORAS_ADD2(10).

  DATA: BEGIN OF TL_AREAS OCCURS 10,
          AREA(40),
          TOT_ATEND       TYPE I,
          MEDIDA_TEMP(10) TYPE C,
          TEMPO_TOTAL(10) TYPE C,
        END OF TL_AREAS.

  CLEAR: WL_TOTAL_ATENDIMENTOS, WL_TOTAL_MEDIA, WL_TOTAL_TEMPO.


  LOOP AT T_SAIDA INTO WA_SAIDA.
    IF WA_SAIDA-TEMPO NE '000000'.
      TL_AREAS-AREA = WA_SAIDA-AREA.
      TL_AREAS-TOT_ATEND = 1.

      COLLECT TL_AREAS.
    ENDIF.

  ENDLOOP.

  LOOP AT TL_AREAS.

    LOOP AT T_SAIDA INTO WA_SAIDA
       WHERE AREA EQ TL_AREAS-AREA.

      WL_CONT_AUX = STRLEN( WA_SAIDA-TEMPO ).
      SUBTRACT 2 FROM WL_CONT_AUX.
      WL_SEGUNDOS = WA_SAIDA-TEMPO+WL_CONT_AUX(2).
      SUBTRACT 2 FROM WL_CONT_AUX.
      WL_MINUTOS = WA_SAIDA-TEMPO+WL_CONT_AUX(2).

      WL_HORAS = WA_SAIDA-TEMPO(WL_CONT_AUX).


      WL_TEMPO_SEGUNDOS = ( WL_TEMPO_SEGUNDOS + ( ( WL_HORAS * 3600 ) + ( WL_MINUTOS * 60 )  + WL_SEGUNDOS ) ).

    ENDLOOP.
    WL_TEMPO_TOTAL    =  WL_TEMPO_SEGUNDOS.
    WL_TEMPO_SEGUNDOS = ( WL_TEMPO_SEGUNDOS / TL_AREAS-TOT_ATEND ).

    L = WL_TEMPO_SEGUNDOS.
    J = L / 60 .
    K = L / 3600 .

    HORA = L DIV 3600.
    RESTO_HORA = L MOD 3600.
    MINUTO = RESTO_HORA DIV 60.
    SEGUNDO = RESTO_HORA MOD 60.

    CONDENSE HORA    NO-GAPS.
    CONDENSE MINUTO  NO-GAPS.
    CONDENSE SEGUNDO NO-GAPS.

    CONCATENATE HORA ':' MINUTO ':' SEGUNDO  INTO TL_AREAS-MEDIDA_TEMP.

    MODIFY TL_AREAS TRANSPORTING MEDIDA_TEMP.
*****************************************************************
    L = WL_TEMPO_TOTAL.
    J = L / 60 .
    K = L / 3600 .

    HORA = L DIV 3600.
    RESTO_HORA = L MOD 3600.
    MINUTO = RESTO_HORA DIV 60.
    SEGUNDO = RESTO_HORA MOD 60.

    CONDENSE HORA    NO-GAPS.
    CONDENSE MINUTO  NO-GAPS.
    CONDENSE SEGUNDO NO-GAPS.

    CONCATENATE HORA ':' MINUTO ':' SEGUNDO  INTO TL_AREAS-TEMPO_TOTAL.

    MODIFY TL_AREAS TRANSPORTING TEMPO_TOTAL.
*****************************************************************
    CLEAR: WL_TEMPO_SEGUNDOS,WL_TEMPO_SEGUNDOS, WL_HORA_AUX, HORA, MINUTO, SEGUNDO, TL_AREAS, WL_TEMPO_TOTAL.
  ENDLOOP.

** split TOP-Document
  CALL METHOD R_TOP->VERTICAL_SPLIT
    EXPORTING
      SPLIT_AREA  = R_TOP
      SPLIT_WIDTH = '50%'
    IMPORTING
      RIGHT_AREA  = A_LOGO.

  TEXT = 'Indicadores de Cadastro de Materiais'(201).
  CALL METHOD R_TOP->ADD_TEXT
    EXPORTING
      TEXT      = TEXT
      SAP_STYLE = 'HEADING'.
  CALL METHOD R_TOP->NEW_LINE.
  CALL METHOD R_TOP->NEW_LINE.

  CALL METHOD R_TOP->ADD_TABLE
    EXPORTING
      NO_OF_COLUMNS = 3
      WITH_HEADING  = ' '
      WIDTH         = '50'
      BORDER        = '0'
    IMPORTING
      TABLE         = S_TABLE.
  CALL METHOD S_TABLE->ADD_COLUMN
    IMPORTING
      COLUMN = COL_RESUMO.
  CALL METHOD S_TABLE->ADD_COLUMN
    IMPORTING
      COLUMN = COL_TOTAL_ATEND.
  CALL METHOD S_TABLE->ADD_COLUMN
    IMPORTING
      COLUMN = COL_MEDIA_TEMPO.
  CALL METHOD S_TABLE->ADD_COLUMN
    IMPORTING
      COLUMN = COL_TEMPO_TOTAL.

** Coluna do Resumo
  CALL METHOD COL_RESUMO->ADD_GAP
    EXPORTING
      WIDTH = 50.
  TEXT = 'Resumo'.
  CALL METHOD COL_RESUMO->ADD_TEXT
    EXPORTING
      TEXT         = TEXT
      SAP_EMPHASIS = 'Strong'.
** Coluna do Total Atendimento
  CALL METHOD COL_TOTAL_ATEND->ADD_GAP
    EXPORTING
      WIDTH = 50.
  TEXT = 'Total Atendimento' .
  CALL METHOD COL_TOTAL_ATEND->ADD_TEXT
    EXPORTING
      TEXT         = TEXT
      SAP_EMPHASIS = 'Strong'.

** Coluna do Média Tempo
  CALL METHOD COL_MEDIA_TEMPO->ADD_GAP
    EXPORTING
      WIDTH = 50.
  TEXT = 'Média Tempo(Horas)' .
  CALL METHOD COL_MEDIA_TEMPO->ADD_TEXT
    EXPORTING
      TEXT         = TEXT
      SAP_EMPHASIS = 'Strong'.

** Coluna do Tempo Total
  CALL METHOD COL_TEMPO_TOTAL->ADD_GAP
    EXPORTING
      WIDTH = 50.
  TEXT = 'Tempo Total(Horas)' .
  CALL METHOD COL_TEMPO_TOTAL->ADD_TEXT
    EXPORTING
      TEXT         = TEXT
      SAP_EMPHASIS = 'Strong'.

  CLEAR: HORAS_ADD, WL_HORA_AUX2, WL_HORA_AUX3.

  HORAS_ADD  = '000000'.
  HORAS_ADD2 = '000000'.


  LOOP AT TL_AREAS.
** Coluna do Resumo
    CALL METHOD COL_RESUMO->ADD_GAP
      EXPORTING
        WIDTH = 50.
    TEXT = TL_AREAS-AREA.
    CALL METHOD COL_RESUMO->ADD_TEXT
      EXPORTING
        TEXT      = TEXT
        SAP_STYLE = ' '.
** Coluna do Total Atendimento
    CALL METHOD COL_TOTAL_ATEND->ADD_GAP
      EXPORTING
        WIDTH = 50.
    TEXT = TL_AREAS-TOT_ATEND . "total de atendimentos da area
    CALL METHOD COL_TOTAL_ATEND->ADD_TEXT
      EXPORTING
        TEXT      = TEXT
        SAP_STYLE = ' '.

** Coluna do Média Tempo
    CALL METHOD COL_MEDIA_TEMPO->ADD_GAP
      EXPORTING
        WIDTH = 50.
    TEXT = TL_AREAS-MEDIDA_TEMP  . "media de tempo da area
    CALL METHOD COL_MEDIA_TEMPO->ADD_TEXT
      EXPORTING
        TEXT      = TEXT
        SAP_STYLE = ' '.

** Coluna do Tempo Total
    CALL METHOD COL_TEMPO_TOTAL->ADD_GAP
      EXPORTING
        WIDTH = 50.
    TEXT = TL_AREAS-TEMPO_TOTAL  . "media de tempo total
    CALL METHOD COL_TEMPO_TOTAL->ADD_TEXT
      EXPORTING
        TEXT      = TEXT
        SAP_STYLE = ' '.

    WL_HORA_AUX2 = TL_AREAS-MEDIDA_TEMP  .
    WL_HORA_AUX3 = TL_AREAS-TEMPO_TOTAL.

    TRANSLATE WL_HORA_AUX2  USING ': ' .
    CONDENSE WL_HORA_AUX2 NO-GAPS.

    PERFORM ADD_HORA USING WL_HORA_AUX2
                  CHANGING HORAS_ADD.
    WL_TOTAL_ATENDIMENTOS = WL_TOTAL_ATENDIMENTOS + TL_AREAS-TOT_ATEND .

    TRANSLATE WL_HORA_AUX3  USING ': ' .
    CONDENSE WL_HORA_AUX3 NO-GAPS.

    PERFORM ADD_HORA USING WL_HORA_AUX3
                  CHANGING HORAS_ADD2.

  ENDLOOP.

  CONCATENATE HORAS_ADD+7(1) HORAS_ADD+6(1) HORAS_ADD+5(1) HORAS_ADD+4(1)
              HORAS_ADD+3(1) HORAS_ADD+2(1) HORAS_ADD+1(1) HORAS_ADD(1)
              INTO WL_AUX.

  CONCATENATE WL_AUX+7(1) WL_AUX+6(1) WL_AUX+5(1) WL_AUX+4(1) ':'
              WL_AUX+3(1) WL_AUX+2(1) ':' WL_AUX+1(1) WL_AUX(1)
              INTO WL_TOTAL_MEDIA.

* Tempo Total
  CONCATENATE HORAS_ADD2+7(1) HORAS_ADD2+6(1) HORAS_ADD2+5(1) HORAS_ADD2+4(1)
              HORAS_ADD2+3(1) HORAS_ADD2+2(1) HORAS_ADD2+1(1) HORAS_ADD2(1)
              INTO WL_AUX2.

  CONCATENATE WL_AUX2+7(1) WL_AUX2+6(1) WL_AUX2+5(1) WL_AUX2+4(1) ':'
              WL_AUX2+3(1) WL_AUX2+2(1) ':' WL_AUX2+1(1) WL_AUX2(1)
              INTO WL_TOTAL_TEMPO.

* Média de Atendimento
  WL_MEDIA2 = HORAS_ADD2.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = WL_MEDIA2
    IMPORTING
      OUTPUT = WL_MEDIA2.

  WL_MEDIA = ( ( WL_MEDIA2(4) * 3600 ) + ( WL_MEDIA2+4(2) * 60 ) + ( WL_MEDIA2+6(2) ) ) / WL_TOTAL_ATENDIMENTOS.

  L = WL_MEDIA.
  J = L / 60 .
  K = L / 3600 .

  HORA = L DIV 3600.
  RESTO_HORA = L MOD 3600.
  MINUTO = RESTO_HORA DIV 60.
  SEGUNDO = RESTO_HORA MOD 60.

  CONDENSE HORA    NO-GAPS.
  CONDENSE MINUTO  NO-GAPS.
  CONDENSE SEGUNDO NO-GAPS.

  CONCATENATE HORA ':' MINUTO ':' SEGUNDO  INTO WL_TOTAL_MEDIA.

** Totais
  CALL METHOD COL_RESUMO->ADD_GAP
    EXPORTING
      WIDTH = 50.
  TEXT = 'Totais' .
  CALL METHOD COL_RESUMO->ADD_TEXT
    EXPORTING
      TEXT         = TEXT
      SAP_EMPHASIS = 'Strong'.

** Coluna do Atendimentos totais
  CALL METHOD COL_TOTAL_ATEND->ADD_GAP
    EXPORTING
      WIDTH = 50.
  TEXT = WL_TOTAL_ATENDIMENTOS .
  CALL METHOD COL_TOTAL_ATEND->ADD_TEXT
    EXPORTING
      TEXT         = TEXT
      SAP_EMPHASIS = 'Strong'.

** Coluna do Total Média Tempo
  CALL METHOD COL_MEDIA_TEMPO->ADD_GAP
    EXPORTING
      WIDTH = 50.
  TEXT = WL_TOTAL_MEDIA .
  CALL METHOD COL_MEDIA_TEMPO->ADD_TEXT
    EXPORTING
      TEXT         = TEXT
      SAP_EMPHASIS = 'Strong'.

** Coluna do total do tempo
  CALL METHOD COL_TEMPO_TOTAL->ADD_GAP
    EXPORTING
      WIDTH = 50.
  TEXT = WL_TOTAL_TEMPO .
  CALL METHOD COL_TEMPO_TOTAL->ADD_TEXT
    EXPORTING
      TEXT         = TEXT
      SAP_EMPHASIS = 'Strong'.



ENDFORM.                    "html_top_of_page
*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM XPF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.

  DATA: GR_EVENTS TYPE REF TO LCL_EVENT_RECEIVER.
  DATA : LS_SEL_HIDE            TYPE SLIS_SEL_HIDE_ALV.
  DATA REF1 TYPE REF TO CL_GUI_ALV_GRID.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      ES_SEL_HIDE = LS_SEL_HIDE
      E_GRID      = REF1.


  CREATE OBJECT GR_EVENTS.
  SET HANDLER GR_EVENTS->HANDLE_AFTER_USER_COMMAND FOR REF1.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.

  IF SY-UCOMM IS NOT INITIAL
  OR INIT IS INITIAL.
    PERFORM SUBTOTAL.
    INIT = 'X'.
  ENDIF.
ENDFORM. "XUSER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  XCALLBACK_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM XCALLBACK_TOP_OF_PAGE.
  DATA: TEXT TYPE SDYDO_TEXT_ELEMENT.
  DATA: S_TABLE TYPE REF TO CL_DD_TABLE_ELEMENT.
  DATA: COL_RESUMO TYPE REF TO CL_DD_AREA.
  DATA: COL_TOTAL_ATEND TYPE REF TO CL_DD_AREA.
  DATA: COL_MEDIA_TEMPO TYPE REF TO CL_DD_AREA.
  DATA: COL_TEMPO_TOTAL TYPE REF TO CL_DD_AREA.
  DATA: COL_TOTAIS TYPE REF TO CL_DD_AREA.
  DATA: A_LOGO TYPE REF TO CL_DD_AREA.
  DATA: LOGO TYPE SDYDO_VALUE.
  DATA: LINK_AREA TYPE REF TO CL_DD_AREA.

  DATA: WL_TEMPO_SEGUNDOS(10) TYPE C,
        WL_TEMPO_TOTAL(10)    TYPE C,
        WL_HORAS(4)           TYPE C,
        WL_MINUTOS(2)         TYPE C,
        WL_SEGUNDOS(2)        TYPE C,
        L(10)                 TYPE P DECIMALS 0, "TYPE I ,
        J(20)                 TYPE C,
        K(20)                 TYPE C,
        HORA(10)              TYPE C,
        RESTO_HORA            TYPE I,
        MINUTO(2)             TYPE N,
        RESTO_MINUTO          TYPE I,
        SEGUNDO(2)            TYPE N,
        WL_HORA_AUX(4)        TYPE C,
        WL_HORA_TOTAL         TYPE T,
        WL_CONT_AUX           TYPE SY-INDEX,
        WL_TEMPO_MASK(10),
        WL_TOTAL_ATENDIMENTOS TYPE I,
        WL_TOTAL_MEDIA(15),
        WL_MEDIA(15),
        WL_TOTAL_TEMPO(10)    TYPE C,
        WL_HORA_AUX2(10),
        WL_HORA_AUX3(10),
        WL_AUX(10)            TYPE C,
        WL_AUX2(10)           TYPE C,
        HORAS_ADD(10),
        HORAS_ADD2(10),
        WL_MEDIA3(8),
        WL_MEDIA2(8).

  DATA: BEGIN OF TL_AREAS OCCURS 10,
          AREA(40),
          TOT_ATEND       TYPE I,
          MEDIDA_TEMP(10) TYPE C,
          TEMPO_TOTAL(10) TYPE C,
        END OF TL_AREAS.

  CLEAR: WL_TOTAL_ATENDIMENTOS, WL_TOTAL_MEDIA, WL_TOTAL_TEMPO.


  LOOP AT T_SAIDA INTO WA_SAIDA.
    TL_AREAS-AREA = WA_SAIDA-AREA.
    TL_AREAS-TOT_ATEND = 1.

    COLLECT TL_AREAS.
  ENDLOOP.

  LOOP AT TL_AREAS.

    LOOP AT T_SAIDA INTO WA_SAIDA
       WHERE AREA EQ TL_AREAS-AREA.

      WL_CONT_AUX = STRLEN( WA_SAIDA-TEMPO ).
      SUBTRACT 2 FROM WL_CONT_AUX.
      WL_SEGUNDOS = WA_SAIDA-TEMPO+WL_CONT_AUX(2).
      SUBTRACT 2 FROM WL_CONT_AUX.
      WL_MINUTOS = WA_SAIDA-TEMPO+WL_CONT_AUX(2).

      WL_HORAS = WA_SAIDA-TEMPO(WL_CONT_AUX).


      WL_TEMPO_SEGUNDOS = ( WL_TEMPO_SEGUNDOS + ( ( WL_HORAS * 3600 ) + ( WL_MINUTOS * 60 )  + WL_SEGUNDOS ) ).

    ENDLOOP.
    WL_TEMPO_TOTAL    =  WL_TEMPO_SEGUNDOS.
    WL_TEMPO_SEGUNDOS = ( WL_TEMPO_SEGUNDOS / TL_AREAS-TOT_ATEND ).

    L = WL_TEMPO_SEGUNDOS.
    J = L / 60 .
    K = L / 3600 .

    HORA = L DIV 3600.
    RESTO_HORA = L MOD 3600.
    MINUTO = RESTO_HORA DIV 60.
    SEGUNDO = RESTO_HORA MOD 60.

    CONDENSE HORA    NO-GAPS.
    CONDENSE MINUTO  NO-GAPS.
    CONDENSE SEGUNDO NO-GAPS.

    CONCATENATE HORA ':' MINUTO ':' SEGUNDO  INTO TL_AREAS-MEDIDA_TEMP.

    MODIFY TL_AREAS TRANSPORTING MEDIDA_TEMP.
*****************************************************************
    L = WL_TEMPO_TOTAL.
    J = L / 60 .
    K = L / 3600 .

    HORA = L DIV 3600.
    RESTO_HORA = L MOD 3600.
    MINUTO = RESTO_HORA DIV 60.
    SEGUNDO = RESTO_HORA MOD 60.

    CONDENSE HORA    NO-GAPS.
    CONDENSE MINUTO  NO-GAPS.
    CONDENSE SEGUNDO NO-GAPS.

    CONCATENATE HORA ':' MINUTO ':' SEGUNDO  INTO TL_AREAS-TEMPO_TOTAL.

    MODIFY TL_AREAS TRANSPORTING TEMPO_TOTAL.
*****************************************************************
    CLEAR: WL_TEMPO_SEGUNDOS,WL_TEMPO_SEGUNDOS, WL_HORA_AUX, HORA, MINUTO, SEGUNDO, TL_AREAS, WL_TEMPO_TOTAL.
  ENDLOOP.


  CLEAR: HORAS_ADD, WL_HORA_AUX2, WL_HORA_AUX3.

  SKIP.
  WRITE:  18 'Indicadores de Cadastro de Materiais'.
  SKIP.
  ULINE (76).
  WRITE:/ '|', 2 'Resumo',  15 '|', 17 'Total Atendimento', 35 '|', 37 'Média Tempo(Horas)', 55 '|', 57 'Tempo Total(Horas)', 76 '|'.
  HORAS_ADD  = '000000'.
  HORAS_ADD2 = '000000'.

  LOOP AT TL_AREAS.
    ULINE (76).
    WRITE:/ '|', 2  TL_AREAS-AREA,  15 '|', 17 TL_AREAS-TOT_ATEND, 35 '|', 37 TL_AREAS-MEDIDA_TEMP, 55 '|', 57 TL_AREAS-TEMPO_TOTAL , 76 '|'.
    WL_HORA_AUX2 = TL_AREAS-TEMPO_TOTAL.
    WL_HORA_AUX3 = TL_AREAS-TEMPO_TOTAL.

    TRANSLATE WL_HORA_AUX2  USING ': ' .
    CONDENSE WL_HORA_AUX2 NO-GAPS.

    PERFORM ADD_HORA USING WL_HORA_AUX2
                  CHANGING HORAS_ADD.
    WL_TOTAL_ATENDIMENTOS = WL_TOTAL_ATENDIMENTOS + TL_AREAS-TOT_ATEND .

    TRANSLATE WL_HORA_AUX3  USING ': ' .
    CONDENSE WL_HORA_AUX3 NO-GAPS.


    PERFORM ADD_HORA USING WL_HORA_AUX3
                  CHANGING HORAS_ADD2.

  ENDLOOP.
  WL_MEDIA = WL_HORA_AUX2 / WL_TOTAL_ATENDIMENTOS.

*  CONCATENATE HORAS_ADD+7(1) HORAS_ADD+6(1) HORAS_ADD+5(1) HORAS_ADD+4(1)
*              HORAS_ADD+3(1) HORAS_ADD+2(1) HORAS_ADD+1(1) HORAS_ADD(1)
*              INTO WL_AUX.

*
*  CONCATENATE WL_AUX+7(1) WL_AUX+6(1) WL_AUX+5(1) WL_AUX+4(1) ':'
*              WL_AUX+3(1) WL_AUX+2(1) ':' WL_AUX+1(1) WL_AUX(1)
*              INTO WL_TOTAL_MEDIA.


  CONCATENATE HORAS_ADD2+7(1) HORAS_ADD2+6(1) HORAS_ADD2+5(1) HORAS_ADD2+4(1)
              HORAS_ADD2+3(1) HORAS_ADD2+2(1) HORAS_ADD2+1(1) HORAS_ADD2(1)
              INTO WL_AUX2.

  CONCATENATE WL_AUX2+7(1) WL_AUX2+6(1) WL_AUX2+5(1) WL_AUX2+4(1) ':'
              WL_AUX2+3(1) WL_AUX2+2(1) ':' WL_AUX2+1(1) WL_AUX2(1)
              INTO WL_TOTAL_TEMPO.

* Média de Atendimento
  WL_MEDIA2 = HORAS_ADD2.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = WL_MEDIA2
    IMPORTING
      OUTPUT = WL_MEDIA2.

  WL_MEDIA3 = ( ( WL_MEDIA2(4) * 3600 ) + ( WL_MEDIA2+4(2) * 60 ) + ( WL_MEDIA2+6(2) ) ) / WL_TOTAL_ATENDIMENTOS.

  L = WL_MEDIA3.
  J = L / 60 .
  K = L / 3600 .

  HORA = L DIV 3600.
  RESTO_HORA = L MOD 3600.
  MINUTO = RESTO_HORA DIV 60.
  SEGUNDO = RESTO_HORA MOD 60.

  CONDENSE HORA    NO-GAPS.
  CONDENSE MINUTO  NO-GAPS.
  CONDENSE SEGUNDO NO-GAPS.

  CONCATENATE HORA ':' MINUTO ':' SEGUNDO  INTO WL_TOTAL_MEDIA.



  ULINE (76).
  WRITE:/ '|', 2  'Totais',  15 '|', 17 WL_TOTAL_ATENDIMENTOS, 35 '|', 37 WL_TOTAL_MEDIA, 55 '|', 57 WL_TOTAL_TEMPO , 76 '|'.
  ULINE (76).
  SKIP.

ENDFORM. " XCALLBACK_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TOP_OF_LIST.
*
  DATA: LO_GRID TYPE REF TO CL_GUI_ALV_GRID.
* FOR Print out:
  CHECK SY-UCOMM = 'PRIN'            " Print
  OR    SY-UCOMM = '&RNT_PREV'.      " Print Preview
*
* get the global reference
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      E_GRID = LO_GRID.
*
* get the subtotal
  DATA: LREF_DATA0  TYPE REF TO DATA.
  DATA: LREF_DATA1  TYPE REF TO DATA.
  DATA: LREF_DATA2  TYPE REF TO DATA.
  DATA: REF1          TYPE REF TO CL_GUI_ALV_GRID,
        HORAS_ADD(10),
        IS_TABLE      TYPE LVC_S_STBL.

  FIELD-SYMBOLS: <L_SUM_TAB> TYPE TABLE,
                 <L_SUM>     TYPE TY_SAIDA.

* Set the subtotal for the Print / Print preview
  FIELD-SYMBOLS: <FS_TAB1> TYPE ANY TABLE.
  DATA: L_TAB_NAME TYPE STRING.
*
  CALL METHOD LO_GRID->GET_SUBTOTALS
    IMPORTING
      EP_COLLECT00 = LREF_DATA0
      EP_COLLECT01 = LREF_DATA1
      EP_COLLECT02 = LREF_DATA2.

  ASSIGN LREF_DATA2->* TO <L_SUM_TAB>.
  LOOP AT  <L_SUM_TAB> ASSIGNING <L_SUM>.
    HORAS_ADD = '000000'.
    LOOP AT T_SAIDA INTO WA_SAIDA
       WHERE WERKS EQ <L_SUM>-WERKS
         AND MATNR EQ <L_SUM>-MATNR.
      PERFORM ADD_HORA USING WA_SAIDA-TEMPO
                    CHANGING HORAS_ADD.
    ENDLOOP.
    <L_SUM>-TEMPO = HORAS_ADD.
  ENDLOOP.

  UNASSIGN <FS_TAB1>.
  L_TAB_NAME = '(SAPLKKBL)IT_COLLECT02[]'.

  ASSIGN (L_TAB_NAME) TO <FS_TAB1>.
  IF <FS_TAB1> IS ASSIGNED.
    <FS_TAB1> = <L_SUM_TAB>.

  ENDIF.

  ASSIGN LREF_DATA1->* TO <L_SUM_TAB>.
  LOOP AT  <L_SUM_TAB> ASSIGNING <L_SUM>.
    HORAS_ADD = '000000'.
    LOOP AT T_SAIDA INTO WA_SAIDA
       WHERE WERKS EQ <L_SUM>-WERKS.
      PERFORM ADD_HORA USING WA_SAIDA-TEMPO
                    CHANGING HORAS_ADD.
    ENDLOOP.
    <L_SUM>-TEMPO = HORAS_ADD.
  ENDLOOP.

  UNASSIGN <FS_TAB1>.
  L_TAB_NAME = '(SAPLKKBL)IT_COLLECT01[]'.

  ASSIGN (L_TAB_NAME) TO <FS_TAB1>.
  IF <FS_TAB1> IS ASSIGNED.
    <FS_TAB1> = <L_SUM_TAB>.

  ENDIF.

  ASSIGN LREF_DATA0->* TO <L_SUM_TAB>.
  LOOP AT  <L_SUM_TAB> ASSIGNING <L_SUM>.
    HORAS_ADD = '000000'.
*    LOOP AT T_SAIDA INTO WA_SAIDA.
*      PERFORM ADD_HORA USING WA_SAIDA-TEMPO
*                    CHANGING HORAS_ADD.
*    ENDLOOP.
    <L_SUM>-TEMPO = HORAS_ADD.
  ENDLOOP.

  UNASSIGN <FS_TAB1>.
  L_TAB_NAME = '(SAPLKKBL)IT_COLLECT00[]'.

  ASSIGN (L_TAB_NAME) TO <FS_TAB1>.
  IF <FS_TAB1> IS ASSIGNED.
    <FS_TAB1> = <L_SUM_TAB>.

  ENDIF.
ENDFORM. " TOP_OF_LIST
*&---------------------------------------------------------------------*
*&      Form  SUBTOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUBTOTAL .
  DATA: LREF_DATA0  TYPE REF TO DATA.
  DATA: LREF_DATA1  TYPE REF TO DATA.
  DATA: LREF_DATA2  TYPE REF TO DATA.
  DATA: REF1          TYPE REF TO CL_GUI_ALV_GRID,
        HORAS_ADD(10),
        IS_TABLE      TYPE LVC_S_STBL.

  FIELD-SYMBOLS: <L_SUM_TAB> TYPE TABLE,
                 <L_SUM>     TYPE TY_SAIDA.

  DATA : LS_SEL_HIDE  TYPE SLIS_SEL_HIDE_ALV.

  DATA : IT_FIELDCATALOG TYPE LVC_T_FCAT,
         WA_FIELDCATALOG TYPE LVC_S_FCAT.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      ES_SEL_HIDE = LS_SEL_HIDE
      E_GRID      = REF1.

  CALL METHOD REF1->GET_FRONTEND_FIELDCATALOG
    IMPORTING
      ET_FIELDCATALOG = IT_FIELDCATALOG.
******************************************************
  CALL METHOD REF1->GET_FRONTEND_LAYOUT
    IMPORTING
      ES_LAYOUT = IS_LAYOUT.

  CALL METHOD REF1->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = IT_FIELDCATALOG.

  CALL METHOD REF1->REFRESH_TABLE_DISPLAY
    EXPORTING
      I_SOFT_REFRESH = ''.

  CALL METHOD REF1->GET_SUBTOTALS
    IMPORTING
      EP_COLLECT00 = LREF_DATA0
      EP_COLLECT01 = LREF_DATA1
      EP_COLLECT02 = LREF_DATA2.
*******************************************
  ASSIGN LREF_DATA2->* TO <L_SUM_TAB>.
  LOOP AT  <L_SUM_TAB> ASSIGNING <L_SUM>.
    HORAS_ADD = '000000'.
    LOOP AT T_SAIDA INTO WA_SAIDA
       WHERE WERKS EQ <L_SUM>-WERKS
         AND MATNR EQ <L_SUM>-MATNR.
      PERFORM ADD_HORA USING WA_SAIDA-TEMPO
                    CHANGING HORAS_ADD.
    ENDLOOP.
    <L_SUM>-TEMPO = HORAS_ADD.
  ENDLOOP.

  ASSIGN LREF_DATA1->* TO <L_SUM_TAB>.
  LOOP AT  <L_SUM_TAB> ASSIGNING <L_SUM>.
    HORAS_ADD = '000000'.
    LOOP AT T_SAIDA INTO WA_SAIDA
       WHERE WERKS EQ <L_SUM>-WERKS.
      PERFORM ADD_HORA USING WA_SAIDA-TEMPO
                    CHANGING HORAS_ADD.
    ENDLOOP.
    <L_SUM>-TEMPO = HORAS_ADD.
  ENDLOOP.

  ASSIGN LREF_DATA0->* TO <L_SUM_TAB>.
  LOOP AT  <L_SUM_TAB> ASSIGNING <L_SUM>.
    HORAS_ADD = '000000'.
*    LOOP AT T_SAIDA INTO WA_SAIDA.
*      PERFORM ADD_HORA USING WA_SAIDA-TEMPO
*                    CHANGING HORAS_ADD.
*    ENDLOOP.
    <L_SUM>-TEMPO = HORAS_ADD.
  ENDLOOP.
*******************************************
  CALL METHOD REF1->GET_FRONTEND_LAYOUT
    IMPORTING
      ES_LAYOUT = IS_LAYOUT.
  CALL METHOD REF1->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = IT_FIELDCATALOG.

  IS_TABLE-ROW = 'X'.
  IS_TABLE-COL = 'X'.
  CALL METHOD REF1->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE      = IS_TABLE
      I_SOFT_REFRESH = 'X'.
ENDFORM.                    " SUBTOTAL
*&---------------------------------------------------------------------*
*&      Form  ADD_HORA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<L_SUM>_TEMPO  text
*      <--P_HORAS_ADD  text
*----------------------------------------------------------------------*
FORM ADD_HORA  USING    P_TEMPO
               CHANGING P_HORAS_ADD.

  DATA: WL_TEMPO_SEGUNDOS1(10) TYPE C,
        WL_TEMPO_SEGUNDOS2(10) TYPE C,
        WL_TEMPO_SEGUNDOST(10) TYPE C,
        WL_HORAS(4)            TYPE C,
        WL_MINUTOS(2)          TYPE C,
        WL_SEGUNDOS(2)         TYPE C,
        WL_CONT_AUX            TYPE SY-INDEX,
        L                      TYPE I,
        J(20)                  TYPE C,
        K(20)                  TYPE C,
        HORA(10)               TYPE C,
        RESTO_HORA             TYPE I,
        MINUTO(2)              TYPE N,
        RESTO_MINUTO           TYPE I,
        SEGUNDO(2)             TYPE N.

  WL_CONT_AUX = STRLEN( P_TEMPO ).
  SUBTRACT 2 FROM WL_CONT_AUX.
  WL_SEGUNDOS = P_TEMPO+WL_CONT_AUX(2).
  SUBTRACT 2 FROM WL_CONT_AUX.
  WL_MINUTOS = P_TEMPO+WL_CONT_AUX(2).

  WL_HORAS = P_TEMPO(WL_CONT_AUX).


  WL_TEMPO_SEGUNDOS1 = ( WL_TEMPO_SEGUNDOS1 + ( ( WL_HORAS * 3600 ) + ( WL_MINUTOS * 60 )  + WL_SEGUNDOS ) ).


  WL_CONT_AUX = STRLEN( P_HORAS_ADD ).
  SUBTRACT 2 FROM WL_CONT_AUX.
  WL_SEGUNDOS = P_HORAS_ADD+WL_CONT_AUX(2).
  SUBTRACT 2 FROM WL_CONT_AUX.
  WL_MINUTOS = P_HORAS_ADD+WL_CONT_AUX(2).

  WL_HORAS = P_HORAS_ADD(WL_CONT_AUX).


  WL_TEMPO_SEGUNDOS2 = ( WL_TEMPO_SEGUNDOS2 + ( ( WL_HORAS * 3600 ) + ( WL_MINUTOS * 60 )  + WL_SEGUNDOS ) ).

  WL_TEMPO_SEGUNDOST = WL_TEMPO_SEGUNDOS2 + WL_TEMPO_SEGUNDOS1.

  L = WL_TEMPO_SEGUNDOST.
  J = L / 60 .
  K = L / 3600 .

  HORA = L DIV 3600.
  RESTO_HORA = L MOD 3600.
  MINUTO = RESTO_HORA DIV 60.
  SEGUNDO = RESTO_HORA MOD 60.

  CONDENSE HORA    NO-GAPS.
  CONDENSE MINUTO  NO-GAPS.
  CONDENSE SEGUNDO NO-GAPS.

  CONCATENATE HORA  MINUTO  SEGUNDO  INTO P_HORAS_ADD.

ENDFORM.                    " ADD_HORA
