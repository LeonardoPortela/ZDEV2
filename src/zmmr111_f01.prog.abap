*&---------------------------------------------------------------------*
*&  Include           ZMMR111_F01
*&---------------------------------------------------------------------*

FORM ENVIA_EMAIL USING P_TIPO.

  DATA: WL_ZMMT0069 TYPE ZMMT0069,
        IT_ZMMT0078 TYPE TABLE OF ZMMT0078,
        WA_ZMMT0078 TYPE ZMMT0078.

  SELECT SINGLE * FROM ZMMT0069 INTO WL_ZMMT0069 WHERE NUREQ = WG_CADMAT-NUREQ.

  DATA: GT_DESTINATARIO TYPE STANDARD TABLE OF SOMLREC90 WITH HEADER LINE,
        GT_ASSUNTO      TYPE SODOCCHGI1,
        GT_TEXTO        TYPE STANDARD TABLE OF SOLI WITH HEADER LINE.

  REFRESH: GT_DESTINATARIO,
           GT_TEXTO.
  CLEAR    GT_ASSUNTO.
***********************
* Objetos
***********************
  DATA: OBJ_ZCL_UTIL TYPE REF TO ZCL_UTIL.

***********************
* Variavel
***********************
  DATA: VAR_DATA    TYPE SY-DATUM,
        VAR_DATA_I  TYPE SY-DATUM,
        VLR_STRING  TYPE STRING,
        VAR_DATA_BR TYPE C LENGTH 10.


  DATA: BSMTP_ADDR TYPE ADR6-SMTP_ADDR.

  REFRESH IT_ZMMT0078.
  IF P_TIPO = 'H'.
    IF WL_ZMMT0069-MTART NE 'ZEPI'.
      SELECT *
      FROM ZMMT0078
      INTO TABLE IT_ZMMT0078
      WHERE MTART = WL_ZMMT0069-MTART.
    ELSE.
      SELECT *
        FROM ZMMT0078
        INTO TABLE IT_ZMMT0078
        WHERE MTART = WL_ZMMT0069-MTART
        AND  ( WERKS = WL_ZMMT0069-WERKS OR WERKS = '9999' ).
    ENDIF.
  ENDIF.

  IF IT_ZMMT0078[] IS INITIAL.
    BSMTP_ADDR = 'adm.materiais@amaggi.com.br'.
    GT_DESTINATARIO-REC_TYPE = 'U'.
    GT_DESTINATARIO-RECEIVER = BSMTP_ADDR.
    APPEND GT_DESTINATARIO.
  ELSE.
    LOOP AT IT_ZMMT0078 INTO WA_ZMMT0078.
      SELECT SINGLE ADR6~SMTP_ADDR INTO BSMTP_ADDR
        FROM USR21
          INNER JOIN ADR6
             ON  USR21~ADDRNUMBER = ADR6~ADDRNUMBER
            AND USR21~PERSNUMBER = ADR6~PERSNUMBER
                WHERE USR21~BNAME =  WA_ZMMT0078-USNAM.
      IF SY-SUBRC = 0.
        GT_DESTINATARIO-REC_TYPE = 'U'.
        GT_DESTINATARIO-RECEIVER = BSMTP_ADDR.
        APPEND GT_DESTINATARIO.
      ENDIF.
    ENDLOOP.
  ENDIF.

  GT_ASSUNTO-OBJ_NAME = 'MATERIAL'.
  CONCATENATE 'Solicitação' WL_ZMMT0069-NUREQ INTO GT_ASSUNTO-OBJ_DESCR.
  GT_ASSUNTO-OBJ_LANGU = SY-LANGU.

  IF P_TIPO = 'H'.
    GT_TEXTO = 'Solicitação de Material para homologação'.
  ELSEIF P_TIPO = 'P'.
    GT_TEXTO = 'Solicitação aguardando análise'.
  ENDIF.
  APPEND GT_TEXTO.


  "Enviar E-mail
  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = GT_ASSUNTO
      DOCUMENT_TYPE              = 'HTM'
    TABLES
      OBJECT_CONTENT             = GT_TEXTO
      RECEIVERS                  = GT_DESTINATARIO
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      DOCUMENT_TYPE_NOT_EXIST    = 3
      OPERATION_NO_AUTHORIZATION = 4
      PARAMETER_ERROR            = 5
      X_ERROR                    = 6
      ENQUEUE_ERROR              = 7
      OTHERS                     = 8.

ENDFORM.                    " ENVIA_EMAIL
*&---------------------------------------------------------------------*
*&      Form  F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*      -->P_0023   text
*      -->P_C_0  text
*      -->P_C_0  text
*----------------------------------------------------------------------*
FORM F_TRATA_CAMPOS  USING P_FIELD P_GROUP1 P_VALUE P_INVISIBLE.
  TG_FIELDS-CAMPO     = P_FIELD.
  TG_FIELDS-GROUP1    = P_GROUP1.
  TG_FIELDS-VALUE     = P_VALUE.
  TG_FIELDS-INVISIBLE = P_INVISIBLE.
  APPEND TG_FIELDS.
ENDFORM.                    " F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_LIMPA_CAMPOS .
  CLEAR: WG_CADMAT,
         TG_EDITOR.
  WG_CADMAT-MBRSH = 'B'.
  CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
    EXPORTING
      TABLE = TG_EDITOR.
  CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
    EXPORTING
      READONLY_MODE = 1.

  CALL METHOD OBG_DESCBOXR->SET_TEXT_AS_R3TABLE
    EXPORTING
      TABLE = TG_EDITOR.
  CALL METHOD OBG_DESCBOXR->SET_READONLY_MODE
    EXPORTING
      READONLY_MODE = 1.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VERIFICA_ERROS .
  DATA: WA_MARA     TYPE MARA,
        WA_MARC     TYPE MARC,
        WA_T023     TYPE T023,
        WA_T137     TYPE T137,
        WA_T134     TYPE T134,
        WA_T006     TYPE T006,
        VG_MSEHI    TYPE T006-MSEHI,
        WA_T001W    TYPE T001W,
        WA_ZMMT0069 TYPE ZMMT0069,
        V_MAKTX     TYPE MAKT-MAKTX,
        V_MATNR     TYPE MAKT-MATNR,
        V_NUREQ(12).

  DATA: T_MTART TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'MAGGI_MM01_MTART'
    TABLES
      SET_VALUES    = T_MTART
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  SORT T_MTART BY FROM.

  CLEAR:    TG_MSG_RET.
  REFRESH:  TG_MSG_RET.

  REFRESH: TG_EDITOR.
  IF OBG_DESCBOX IS NOT INITIAL.
    CALL METHOD OBG_DESCBOX->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE = TG_EDITOR.
  ENDIF.

  IF LINES( TG_EDITOR ) GT 13.
    MOVE: 'WG_CADMAT-WERKS'           TO TG_MSG_RET-FIELD,
   'descrição com mais de 13 linhas'  TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ENDIF.

  IF WG_CADMAT-WERKS IS INITIAL.
    MOVE: 'WG_CADMAT-WERKS'      TO TG_MSG_RET-FIELD,
    'Informe o Centro'           TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
      FROM T001W
      INTO WA_T001W
      WHERE WERKS = WG_CADMAT-WERKS.
    IF SY-SUBRC NE 0.
      MOVE: 'WG_CADMAT-WERKS'      TO TG_MSG_RET-FIELD,
            'Centro inválido'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
        ID 'WERKS' FIELD  WG_CADMAT-WERKS
        ID 'ACTVT' FIELD '03'.    "Alteração

    CASE SY-SUBRC.
      WHEN 0.

      WHEN 4.
        MOVE: 'WG_CADMAT-WERKS'      TO TG_MSG_RET-FIELD,
        'Sem autorização para este centro'           TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      WHEN 12.
        MOVE: 'WG_CADMAT-WERKS'      TO TG_MSG_RET-FIELD,
       'Sem autorização neste objeto'           TO TG_MSG_RET-MSG.
      WHEN OTHERS.
    ENDCASE.


    IF WG_CADMAT-EXPANSAO = 'X'.
      IF WG_ACAO = C_DISPLA AND WG_CADMAT-MATNRG IS NOT INITIAL.
        MOVE: 'WG_CADMAT-MATNR'      TO TG_MSG_RET-FIELD,
        'Workflow já disparado!'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ENDIF.
      IF WG_CADMAT-MATNR IS INITIAL.
        MOVE: 'WG_CADMAT-MATNR'      TO TG_MSG_RET-FIELD,
        'Informe material para expansão'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ELSE.
        SELECT SINGLE *
          FROM MARA
          INTO WA_MARA
          WHERE MATNR EQ WG_CADMAT-MATNR.
        IF SY-SUBRC NE 0.
          MOVE: 'WG_CADMAT-MATNR'      TO TG_MSG_RET-FIELD,
         'Material para expansão, não cadastrado'      TO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ENDIF.
        IF WG_ACAO NE C_DISPLA.
          SELECT SINGLE *
            FROM ZMMT0069
            INTO WA_ZMMT0069
            WHERE WERKS  = WG_CADMAT-WERKS
            AND   MATNR  = WG_CADMAT-MATNR
             AND  MATNRG = ''
            AND   ELIMINADO = ''
            AND   RECUSA_FLAG = ''.

          IF SY-SUBRC = 0.
            V_NUREQ = WA_ZMMT0069-NUREQ.
            MOVE: 'WG_CADMAT-MATNR'      TO TG_MSG_RET-FIELD,
            'Solicitação já cadastrada, em aberto'      TO TG_MSG_RET-MSG.
            CONCATENATE  TG_MSG_RET-MSG'->' V_NUREQ  INTO TG_MSG_RET-MSG.
            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
          ENDIF.
        ENDIF.
      ENDIF.

      IF NOT WG_CADMAT-VKORG IS INITIAL AND NOT WG_CADMAT-VTWEG IS INITIAL.
        SELECT SINGLE *
            FROM MARC
            INTO WA_MARC
            WHERE WERKS EQ  WG_CADMAT-WERKS
            AND   MATNR EQ WG_CADMAT-MATNR.

      ENDIF.
    ELSE.
      IF WG_CADMAT-MAKTX IS INITIAL.
        MOVE: 'WG_CADMAT-MAKTX'      TO TG_MSG_RET-FIELD,
        'Informe o texto breve'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ELSE.
        SELECT SINGLE MATNR MAKTX
            FROM MAKT
            INTO ( V_MATNR, V_MAKTX )
            WHERE SPRAS = SY-LANGU
            AND   MAKTX LIKE WG_CADMAT-MAKTX.
        IF SY-SUBRC = 0.
          MOVE: 'WG_CADMAT-MAKTX'      TO TG_MSG_RET-FIELD,
          'Descrição já existe no cadastro'      TO TG_MSG_RET-MSG.
          CONCATENATE TG_MSG_RET-MSG '-' V_MATNR  INTO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.
      IF WG_CADMAT-MEINS IS INITIAL.
        MOVE: 'WG_CADMAT-MEINS'      TO TG_MSG_RET-FIELD,
        'Informe a unidade de medida'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            INPUT          = WG_CADMAT-MEINS
            LANGUAGE       = SY-LANGU
          IMPORTING
            OUTPUT         = VG_MSEHI
          EXCEPTIONS
            UNIT_NOT_FOUND = 1
            OTHERS         = 2.
        IF SY-SUBRC <> 0.
* Implement suitable error handling here
        ENDIF.

        SELECT SINGLE *
         FROM T006
         INTO WA_T006
         WHERE MSEHI EQ VG_MSEHI.
        IF SY-SUBRC NE 0.
          MOVE: 'WG_CADMAT-MEINS'      TO TG_MSG_RET-FIELD,
         'Unidade de medida não cadastrada'      TO TG_MSG_RET-MSG.
          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
        ENDIF.
      ENDIF.
    ENDIF.


    IF WG_CADMAT-VKORG IS INITIAL AND NOT WG_CADMAT-VTWEG IS INITIAL.
      MOVE: 'WG_CADMAT-VKORG'      TO TG_MSG_RET-FIELD,
       'Informar Organização de vendas.'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

    IF WG_CADMAT-VTWEG IS INITIAL AND NOT WG_CADMAT-VKORG IS INITIAL.
      MOVE: 'WG_CADMAT-VKORG'      TO TG_MSG_RET-FIELD,
      'Informar canal de distribuição.'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ENDIF.

    IF WG_CADMAT-MBRSH IS INITIAL.
      MOVE: 'WG_CADMAT-MBRSH'      TO TG_MSG_RET-FIELD,
      'Informe Setor Industrial'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ELSE.
      SELECT SINGLE *
        FROM T137
        INTO WA_T137
        WHERE MBRSH = WG_CADMAT-MBRSH.
      IF SY-SUBRC NE 0.
        MOVE: 'WG_CADMAT-MBRSH'      TO TG_MSG_RET-FIELD,
        'Setor Industrial não existe!'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.

    IF WG_CADMAT-MTART IS INITIAL.
      MOVE: 'WG_CADMAT-MTART'      TO TG_MSG_RET-FIELD,
      'Informe ipo de Material'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ELSE.
      READ TABLE T_MTART WITH KEY FROM = WG_CADMAT-MTART.
      IF SY-SUBRC NE 0.
        MOVE: 'WG_CADMAT-MTART'      TO TG_MSG_RET-FIELD,
       'Tipo de Material não permitido, para solicitação de cadastro!'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ENDIF.
      SELECT SINGLE *
        FROM T134
        INTO WA_T134
        WHERE MTART = WG_CADMAT-MTART.
      IF SY-SUBRC NE 0.
        MOVE: 'WG_CADMAT-MTART'      TO TG_MSG_RET-FIELD,
        'Tipo de Material não existe!'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
*      ELSEIF WG_CADMAT-MTART = 'ZEPI'.
*        IF WG_CADMAT-CODAGREPI IS INITIAL.
*          MOVE: 'WG_CADMAT-CODAGREPI'      TO TG_MSG_RET-FIELD,
*          'Informe o agrupador de EPI!'      TO TG_MSG_RET-MSG.
*          APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
*        ELSE.
*          SELECT SINGLE DENAGREPI
*             FROM ZHRST_MED_AG_EPI
*             INTO WG_CADMAT-DENAGREPI
*            WHERE CODAGREPI = WG_CADMAT-CODAGREPI.
*          IF SY-SUBRC NE 0.
*            MOVE: 'WG_CADMAT-CODAGREPI'      TO TG_MSG_RET-FIELD,
*            'Agrupador de EPI não existe!'   TO TG_MSG_RET-MSG.
*            APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
*          ENDIF.
*        ENDIF.

      ENDIF.
    ENDIF.


    IF WG_CADMAT-MATKL IS INITIAL.
      MOVE: 'WG_CADMAT-MATKL'      TO TG_MSG_RET-FIELD,
      'Informe Grupo de Material'      TO TG_MSG_RET-MSG.
      APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
    ELSE.
      SELECT SINGLE *
      FROM T023
      INTO WA_T023
      WHERE MATKL = WG_CADMAT-MATKL.
      IF SY-SUBRC NE 0.
        MOVE: 'WG_CADMAT-MATKL'      TO TG_MSG_RET-FIELD,
        'Grupo de Material não existe!'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ELSEIF WA_T023-BEGRU = '9999'.
        MOVE: 'WG_CADMAT-MATKL'      TO TG_MSG_RET-FIELD,
       'Grupo de Material bloqueado!'      TO TG_MSG_RET-MSG.
        APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
      ENDIF.
    ENDIF.


    IF TG_MSG_RET[] IS INITIAL AND WG_CADMAT-EXPANSAO = 'X'.
      PERFORM F_VALIDA_ENTRADA.
    ENDIF.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_OBTEM_PROXIMO .
  DATA: VL_NUMBER TYPE I.
  IF WG_CADMAT-NUREQ IS INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        NR_RANGE_NR             = '01'
        OBJECT                  = 'ZMM_SMAT'
      IMPORTING
        NUMBER                  = VL_NUMBER
      EXCEPTIONS
        INTERVAL_NOT_FOUND      = 1
        NUMBER_RANGE_NOT_INTERN = 2
        OBJECT_NOT_FOUND        = 3
        QUANTITY_IS_0           = 4
        QUANTITY_IS_NOT_1       = 5
        INTERVAL_OVERFLOW       = 6
        BUFFER_OVERFLOW         = 7
        OTHERS                  = 8.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      WG_CADMAT-NUREQ = VL_NUMBER.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GRAVA_DADOS .
  CHECK WG_CADMAT-NUREQ IS NOT INITIAL.

  DATA: WL_INPUT TYPE ZMMT0069,
        WL_CONT  TYPE SY-TABIX.

  MOVE-CORRESPONDING WG_CADMAT TO WL_INPUT.

  IF WG_CADMAT-MTART = 'ZINF'.
    WL_INPUT-HOMOLOG = 'T'.
  ELSEIF WG_CADMAT-MTART = 'ZSSO'.
    WL_INPUT-HOMOLOG = 'O'.
  ELSEIF WG_CADMAT-MTART = 'ZEPI'.
    WL_INPUT-HOMOLOG = 'E'.
  ELSE.
    WL_INPUT-HOMOLOG = 'S'.
  ENDIF.
  MOVE: SY-MANDT                    TO WL_INPUT-MANDT,
        SY-UNAME                    TO WL_INPUT-SOLIC,
        SY-UNAME                    TO WL_INPUT-USNAM,
        SY-DATUM                    TO WL_INPUT-DT_ENTRADA,
        SY-UZEIT                    TO WL_INPUT-HR_ENTRADA.

  REFRESH: TG_EDITOR.
  IF OBG_DESCBOX IS NOT INITIAL.
    CALL METHOD OBG_DESCBOX->GET_TEXT_AS_R3TABLE
      IMPORTING
        TABLE = TG_EDITOR.

    WL_CONT = 0.
    LOOP AT TG_EDITOR INTO WG_EDITOR.
      IF WG_EDITOR-LINE IS INITIAL.
        CONTINUE.
      ENDIF.
      IF SY-TABIX EQ 1.
        WL_INPUT-OBSERV+0(72) = WG_EDITOR-LINE.
        WL_CONT = 72.
      ELSEIF SY-TABIX GE 2.
        ADD 72 TO WL_CONT.
*        CONCATENATE WL_INPUT-OBSERV+WL_CONT  WG_EDITOR-LINE INTO WL_INPUT-OBSERV. " SEPARATED  BY CL_ABAP_CHAR_UTILITIES=>CR_LF.
        WL_INPUT-OBSERV+WL_CONT = WG_EDITOR-LINE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  TRANSLATE  WL_INPUT-MAKTX TO UPPER CASE.
  TRANSLATE  WL_INPUT-OBSERV TO UPPER CASE.
  MODIFY ZMMT0069 FROM       WL_INPUT.
  "
  MOVE-CORRESPONDING WL_INPUT TO WL_LOG.
  WL_LOG-DATA = SY-DATUM.
  WL_LOG-HORA = SY-UZEIT.
  WL_LOG-ACAO = 'Alterado/incluido'.
  MODIFY ZMMT0069_LOG FROM       WL_LOG.
  COMMIT WORK.

  MESSAGE S836(SD) WITH TEXT-M01 WG_CADMAT-NUREQ TEXT-M02.

  IF WG_CADMAT-MTART = 'ZINF' OR WG_CADMAT-MTART = 'ZSSO' OR WG_CADMAT-MTART = 'ZEPI'.
    PERFORM ENVIA_EMAIL USING 'H'.
  ELSE.
    PERFORM ENVIA_EMAIL USING 'P'.
  ENDIF.

ENDFORM.

FORM F_VALIDA_ENTRADA .
  "verificar se iniciado
  TYPES: BEGIN OF TY_ZTWF_MAT_LOG,
           MATNR      TYPE ZTWF_MAT_LOG-MATNR,
           WERKS      TYPE ZTWF_MAT_LOG-WERKS,
           DATA       TYPE ZTWF_MAT_LOG-DATA,
           VKORG      TYPE ZTWF_MAT_LOG-VKORG,
           VTWEG      TYPE ZTWF_MAT_LOG-VTWEG,
           NSEQU      TYPE ZTWF_MAT_LOG-NSEQU,
           HORA       TYPE ZTWF_MAT_LOG-HORA,
           USNAM      TYPE ZTWF_MAT_LOG-USNAM,
           MTART      TYPE MARA-MTART,
           CONT       TYPE I,
           SEQ        TYPE I,
           STATUS(10),
         END OF TY_ZTWF_MAT_LOG,

         BEGIN OF TY_ZTWF_MAT_SEQ,
           MTART TYPE ZTWF_MAT_SEQ-MTART,
           WERKS TYPE ZTWF_MAT_SEQ-WERKS,
           NSEQU TYPE ZTWF_MAT_SEQ-NSEQU,
           VKORG TYPE ZTWF_MAT_SEQ-VKORG,
           VTWEG TYPE ZTWF_MAT_SEQ-VTWEG,
           AREA  TYPE ZTWF_MAT_SEQ-AREA,
         END OF TY_ZTWF_MAT_SEQ.

  DATA: T_LOG              TYPE TABLE OF TY_ZTWF_MAT_LOG,
        T_LOG_ANT          TYPE TABLE OF TY_ZTWF_MAT_LOG,
        T_LOG_AUX          TYPE TABLE OF TY_ZTWF_MAT_LOG,
        T_LOG_AUX2         TYPE TABLE OF TY_ZTWF_MAT_LOG,
        T_SEQ              TYPE TABLE OF TY_ZTWF_MAT_SEQ,
        T_ZTWF_MAT_AREA_US TYPE ZTWF_MAT_AREA_US   OCCURS 0 WITH HEADER LINE.

  DATA: WA_LOG      TYPE TY_ZTWF_MAT_LOG,
        WA_LOG_AUX  TYPE TY_ZTWF_MAT_LOG,
        WA_LOG_AUX2 TYPE TY_ZTWF_MAT_LOG,
        WA_SEQ      TYPE TY_ZTWF_MAT_SEQ,
        TABIX       TYPE SY-TABIX,
        VMATNR      TYPE MARA-MATNR,
        W_DATA      TYPE SY-DATUM,
        W_CONT      TYPE I.


  REFRESH  T_LOG.
  W_DATA = SY-DATUM - 30.

  SELECT ZTWF_MAT_LOG~MATNR ZTWF_MAT_LOG~WERKS
         ZTWF_MAT_LOG~DATA ZTWF_MAT_LOG~VKORG
         ZTWF_MAT_LOG~VTWEG ZTWF_MAT_LOG~NSEQU ZTWF_MAT_LOG~HORA
         ZTWF_MAT_LOG~USNAM MARA~MTART
   FROM ZTWF_MAT_LOG
    INNER JOIN MARA ON MARA~MATNR = ZTWF_MAT_LOG~MATNR
     INTO TABLE T_LOG
   WHERE  ZTWF_MAT_LOG~MATNR EQ WG_CADMAT-MATNR
     AND  ZTWF_MAT_LOG~WERKS EQ WG_CADMAT-WERKS
*     AND  ZTWF_MAT_LOG~VKORG EQ WG_CADMAT-VKORG
*     AND  ZTWF_MAT_LOG~VTWEG EQ WG_CADMAT-VTWEG
     AND  ZTWF_MAT_LOG~LOEKZ NE 'X'
     AND  ZTWF_MAT_LOG~DATA  GE W_DATA. "Apaga Workflow menos de 30 dias


  CHECK T_LOG[] IS NOT INITIAL.

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
    W_DATA = SY-DATUM - 10. "até 10 dias considera mesmo atendimento
    SELECT MATNR WERKS DATA VKORG VTWEG NSEQU HORA
    FROM ZTWF_MAT_LOG
      INTO TABLE T_LOG_AUX2
      FOR ALL ENTRIES IN T_LOG_AUX
        WHERE MATNR EQ T_LOG_AUX-MATNR
          AND WERKS EQ T_LOG_AUX-WERKS
          AND VKORG EQ T_LOG_AUX-VKORG
          AND VTWEG EQ T_LOG_AUX-VTWEG
          AND DATA  GT W_DATA
          AND DATA  LT SY-DATUM.

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

    SELECT MTART WERKS NSEQU VKORG VTWEG AREA
      FROM ZTWF_MAT_SEQ
        INTO TABLE T_SEQ
        FOR ALL ENTRIES IN T_LOG
          WHERE "MTART EQ WMARA-MTART
                WERKS EQ T_LOG-WERKS
            AND NSEQU EQ T_LOG-NSEQU
            AND VKORG EQ T_LOG-VKORG
            AND VTWEG EQ T_LOG-VTWEG.
  ENDIF.

  T_LOG_AUX[]  = T_LOG[].
  T_LOG_AUX2[] = T_LOG[].

  "Organiza dados
  DATA: VSEQ     TYPE I,
        VCONT    TYPE I,
        DATA_INI TYPE SY-DATUM,
        DATA_FIM TYPE SY-DATUM,
        INICIO   TYPE ZTWF_MAT_LOG-HORA,
        FIM      TYPE ZTWF_MAT_LOG-HORA.

  SORT: T_LOG_AUX  BY WERKS MATNR VKORG VTWEG NSEQU DATA  HORA,
        T_LOG_AUX2 BY WERKS MATNR VKORG VTWEG NSEQU DATA  HORA,
        T_SEQ      BY MTART WERKS NSEQU VKORG VTWEG.

  DELETE ADJACENT DUPLICATES FROM  T_LOG_AUX  COMPARING WERKS MATNR VKORG VTWEG NSEQU.

  LOOP AT T_LOG_AUX INTO WA_LOG_AUX.
    VSEQ  = 1.
    VCONT = 0.
    LOOP AT T_LOG_AUX2 INTO WA_LOG_AUX2 WHERE WERKS = WA_LOG_AUX-WERKS
                                         AND   MATNR = WA_LOG_AUX-MATNR
                                         AND   VKORG = WA_LOG_AUX-VKORG
                                         AND   VTWEG = WA_LOG_AUX-VTWEG
                                         AND   NSEQU = WA_LOG_AUX-NSEQU.
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
    TABIX = SY-TABIX.
    LOOP AT  T_SEQ INTO WA_SEQ WHERE  MTART = WA_LOG_AUX-MTART
                                AND   WERKS = WA_LOG_AUX-WERKS
                                AND   NSEQU = WA_LOG_AUX-NSEQU
                                AND   VKORG = WA_LOG_AUX-VKORG
                                AND   VTWEG = WA_LOG_AUX-VTWEG.
      " obtem Data inicio e Data Fim
      CLEAR: DATA_FIM, FIM.
      CLEAR: DATA_INI, INICIO.

      LOOP AT T_LOG_AUX2 INTO WA_LOG_AUX2 WHERE WERKS = WA_LOG_AUX-WERKS
                                          AND   MATNR = WA_LOG_AUX-MATNR
                                          AND   VKORG = WA_LOG_AUX-VKORG
                                          AND   VTWEG = WA_LOG_AUX-VTWEG
                                          AND   NSEQU = WA_LOG_AUX-NSEQU
                                          AND   SEQ   = WA_LOG_AUX-SEQ.
        IF DATA_INI IS INITIAL. "1
          DATA_INI    = WA_LOG_AUX2-DATA.
          INICIO      = WA_LOG_AUX2-HORA.
        ENDIF.
      ENDLOOP.
      IF WA_LOG_AUX2-DATA = DATA_INI AND
         WA_LOG_AUX2-HORA = INICIO.
        WA_LOG_AUX-STATUS   = 'Pendente'.
      ELSE.
        WA_LOG_AUX-STATUS   = 'Finalizado'.
      ENDIF.
      MODIFY T_LOG_AUX FROM WA_LOG_AUX INDEX TABIX TRANSPORTING STATUS.
    ENDLOOP.

  ENDLOOP.

  DELETE T_LOG_AUX WHERE STATUS   NE 'Finalizado'.

  IF T_LOG_AUX[] IS  INITIAL.
    MOVE: 'WG_CADMAT-MATNR'      TO TG_MSG_RET-FIELD,
    'Workflow já iniciado para este material e centro. Aguarde finalização do cadastro'      TO TG_MSG_RET-MSG.
    APPEND TG_MSG_RET.  CLEAR: TG_MSG_RET.
  ENDIF.
ENDFORM.                " VALIDA_ENTRADA
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUSCA_DADOS .
  DATA: WL_ZMMT0069  TYPE ZMMT0069,
        WL_MARA      TYPE MARA,
        WL_MAKT      TYPE MAKT,
        WL_CONT      TYPE SY-TABIX,
        WL_CONT_AUX  TYPE SY-TABIX,
        WL_CONT_AUX2 TYPE SY-TABIX,
        V_MATNR      TYPE MARA-MATNR,
        WA_T134T     TYPE T134T,
        WA_T137T     TYPE T137T,
        WA_T023T     TYPE T023T.

  IF WG_CADMAT-MATNR IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT  = WG_CADMAT-MATNR
      IMPORTING
        OUTPUT = WG_CADMAT-MATNR.
    SELECT SINGLE *
      FROM MARA
      INTO WL_MARA
      WHERE MATNR = WG_CADMAT-MATNR.
    IF SY-SUBRC NE 0.
      MESSAGE 'Material não existe!' TYPE 'I'.
      CLEAR: WG_CADMAT-MATNR,
             WG_CADMAT-MAKTX,
             WG_CADMAT-MATKL,
             WG_CADMAT-MEINS.
      EXIT.
    ENDIF.
    SELECT SINGLE *
      FROM MAKT
      INTO WL_MAKT
      WHERE MATNR = WG_CADMAT-MATNR
      AND   SPRAS = SY-LANGU.

    WG_CADMAT-MTART = WL_MARA-MTART.
    WG_CADMAT-MAKTX = WL_MAKT-MAKTX.
    WG_CADMAT-MATKL = WL_MARA-MATKL.
    WG_CADMAT-MEINS = WL_MARA-MEINS.

  ENDIF.
  IF WG_CADMAT-MATKL IS NOT INITIAL.
    SELECT SINGLE *
      FROM T023T
      INTO WA_T023T
      WHERE SPRAS EQ SY-LANGU
      AND   MATKL EQ WG_CADMAT-MATKL.

    WG_CADMAT-WGBEZ = WA_T023T-WGBEZ.

  ENDIF.
  IF WG_CADMAT-MTART IS NOT INITIAL.
    SELECT SINGLE *
      FROM T134T
      INTO WA_T134T
      WHERE SPRAS EQ SY-LANGU
      AND   MTART EQ WG_CADMAT-MTART.

    WG_CADMAT-MTBEZ = WA_T134T-MTBEZ.

  ENDIF.
  IF WG_CADMAT-MBRSH IS NOT INITIAL.
    SELECT SINGLE *
      FROM T137T
      INTO WA_T137T
      WHERE SPRAS EQ SY-LANGU
      AND   MBRSH EQ WG_CADMAT-MBRSH.

    WG_CADMAT-MBBEZ = WA_T137T-MBBEZ.

  ENDIF.

  IF WG_CADMAT-CODAGREPI IS NOT INITIAL.
    SELECT SINGLE DENAGREPI
         FROM ZHRST_MED_AG_EPI
         INTO WG_CADMAT-DENAGREPI
        WHERE CODAGREPI = WG_CADMAT-CODAGREPI.
  ENDIF.

  IF WG_ACAO = C_ADD. "Novo Lançamento
    WG_ACAO = C_MODIF.
  ELSEIF WG_ACAO = C_DISPLA.
    WG_ACAO = C_MODIF.
    CHECK WG_CADMAT-NUREQ IS NOT INITIAL.
    SELECT SINGLE * FROM ZMMT0069
      INTO WL_ZMMT0069
    WHERE NUREQ EQ WG_CADMAT-NUREQ.

    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-I04.
      LEAVE TO SCREEN 100.
    ELSE.
      AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
       ID 'WERKS' FIELD  WL_ZMMT0069-WERKS
       ID 'ACTVT' FIELD '03'.    "Alteração

      CASE SY-SUBRC.
        WHEN 0.

        WHEN 4.
          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Sem autorização para este centro'.
          EXIT.
        WHEN 12.
          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH 'Sem autorização neste objeto'.
          EXIT.
        WHEN OTHERS.
      ENDCASE.

      MOVE-CORRESPONDING WL_ZMMT0069 TO WG_CADMAT.

      IF WL_ZMMT0069-HOMOLOG = 'T'.
        WG_CADMAT-HOM_TI = 'X'.
      ELSEIF  WL_ZMMT0069-HOMOLOG = 'O'.
        WG_CADMAT-HOM_SSO = 'X'.
      ELSEIF  WL_ZMMT0069-HOMOLOG = 'E'.
        WG_CADMAT-HOM_EPI = 'X'.
      ELSE.
        WG_CADMAT-HOM_SUP = 'X'.
      ENDIF.

      "Observação
      REFRESH: TG_EDITOR.
      CLEAR: WL_CONT_AUX2, WL_CONT_AUX, WL_CONT.
      WL_CONT = STRLEN( WL_ZMMT0069-OBSERV ).
      WL_CONT_AUX = WL_CONT / 72.

      DO.
        MOVE: WL_ZMMT0069-OBSERV+WL_CONT_AUX2 TO WG_EDITOR-LINE.
        ADD 72 TO WL_CONT_AUX2.
        APPEND WG_EDITOR TO TG_EDITOR.

        IF WL_CONT_AUX2 GT WL_CONT.
          EXIT.
        ENDIF.
      ENDDO.

      CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TG_EDITOR.
      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 1.
      "Recusa
      REFRESH: TG_EDITOR.
      CLEAR: WL_CONT_AUX2, WL_CONT_AUX, WL_CONT.
      WL_CONT = STRLEN( WL_ZMMT0069-RECUSA ).
      WL_CONT_AUX = WL_CONT / 72.

      DO.
        MOVE: WL_ZMMT0069-RECUSA+WL_CONT_AUX2 TO WG_EDITOR-LINE.
        ADD 72 TO WL_CONT_AUX2.
        APPEND WG_EDITOR TO TG_EDITOR.

        IF WL_CONT_AUX2 GT WL_CONT.
          EXIT.
        ENDIF.
      ENDDO.

      CALL METHOD OBG_DESCBOXR->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TG_EDITOR.
      CALL METHOD OBG_DESCBOXR->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 1.

    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ELIMINAR_LANCAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_ELIMINAR_LANCAMENTO .
  DATA: WL_ZMMT0069 TYPE ZMMT0069.

  SELECT SINGLE * FROM ZMMT0069
    INTO WL_ZMMT0069
  WHERE NUREQ EQ WG_CADMAT-NUREQ.

  IF SY-SUBRC IS INITIAL.
    IF WL_ZMMT0069-ELIMINADO IS INITIAL.
      MOVE: C_X TO WL_ZMMT0069-ELIMINADO.
      MODIFY ZMMT0069 FROM WL_ZMMT0069.

      MESSAGE S836(SD) WITH TEXT-M05.

      CLEAR:    WG_CADMAT, TG_EDITOR.
      REFRESH:  TG_EDITOR, TG_FIELDS.

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR2'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR3'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR4'
                                    C_0       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM F_TRATA_CAMPOS USING  SPACE
                                    'GR1'
                                    C_1       "INPUT 1     NO INPUT 0
                                    C_0.      "INVISIBLE 1 VISIBLE 0

      CALL METHOD OBG_DESCBOX->SET_TEXT_AS_R3TABLE
        EXPORTING
          TABLE = TG_EDITOR.
      CALL METHOD OBG_DESCBOX->SET_READONLY_MODE
        EXPORTING
          READONLY_MODE = 1.
    ELSE.
      MESSAGE S836(SD) DISPLAY LIKE 'E'
         WITH TEXT-M06.
    ENDIF.
  ENDIF.
ENDFORM.
