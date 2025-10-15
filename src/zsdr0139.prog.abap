*&---------------------------------------------------------------------*
*& Report  ZSDR0139
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZSDR0139.

TYPES: BEGIN OF TY_DOCS,
         DOCNUM   TYPE J_1BNFDOC-DOCNUM,
         CANDAT   TYPE J_1BNFDOC-CANDAT,
         MSG_PROC TYPE C LENGTH 250,
       END OF TY_DOCS.


DATA: LIT_EVENT             TYPE TABLE OF J_1BNFE_EVENT,
      LIT_DOC_ESTORNO       TYPE TABLE OF TY_DOCS,
      LIT_DOCS_CANC_AVERB   TYPE TABLE OF TY_DOCS,
      LIT_DOCS_EMIT_CONT    TYPE TABLE OF TY_DOCS,
      LIT_MSG_PROC          TYPE TABLE OF TY_DOCS,
      LIT_ZLEST0224         TYPE TABLE OF ZLEST0224.

DATA: LVA_DT_CRIACAO       TYPE J_1BNFDOC-CREDAT,
      LVA_DT_CRIACAO_EVENT TYPE TZNTSTMPS,
      LVA_ERRO_PROC        TYPE C.

DATA: it_packing_list TYPE TABLE OF sopcklsti1,
      it_header       TYPE TABLE OF solisti1,
      it_contents_txt TYPE TABLE OF solisti1,
      it_contents_bin TYPE TABLE OF solisti1,
      it_receivers    TYPE TABLE OF somlreci1.

DATA: wa_packing_list TYPE sopcklsti1,
      wa_header       TYPE solisti1,
      wa_contents_txt TYPE solisti1,
      wa_contents_bin TYPE solisti1,
      wa_receivers    TYPE somlreci1.

DATA: qt_dias  TYPE i,
      qt_meses TYPE i,
      date(15) TYPE c,
      lv_lines        LIKE sy-tabix,
      wa_doc_data     LIKE sodocchgi1.

DEFINE new_line.
  CLEAR  WA_CONTENTS_TXT.
         WA_CONTENTS_TXT = &1.
  APPEND WA_CONTENTS_TXT TO IT_CONTENTS_TXT.
END-OF-DEFINITION.

DEFINE add_receiver.
  CLEAR  WA_RECEIVERS.
         WA_RECEIVERS-RECEIVER   = &1.
         WA_RECEIVERS-REC_TYPE   = 'U'.
         WA_RECEIVERS-BLIND_COPY = &2.
  APPEND WA_RECEIVERS TO IT_RECEIVERS.
END-OF-DEFINITION.


CONSTANTS: C_AVERB_CONTIGENCIA TYPE C LENGTH 10 VALUE '99999'.


SELECTION-SCREEN: BEGIN OF BLOCK b1.
PARAMETERS: P_DIAS TYPE I.
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.

  IF SY-BATCH EQ ABAP_TRUE.
    TRY.
      ZCL_JOB=>GET_CK_PROGRAM_EXECUCAO( EXPORTING I_NOME_PROGRAM = SY-CPROG IMPORTING E_QTD = DATA(E_QTD) ).
    CATCH ZCX_JOB.
    ENDTRY.

    IF E_QTD GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  SELECT SINGLE * FROM ZLEST0162 INTO @DATA(LWA_ZLEST0162).
  CHECK LWA_ZLEST0162-CK_CONTINGENCIA IS INITIAL. "Só processar se a contingencia estiver desativada

  PERFORM F_ENV_DOCS_CONTINGENCIA. "Averbar documentos que foram emitidos em contigencia.
  PERFORM F_ENV_CANC_ABERBACAO.    "Enviar Cancelamento Averbação
  PERFORM F_ENVIA_EMAIL.


FORM F_ENV_CANC_ABERBACAO .

  CLEAR: LIT_DOCS_CANC_AVERB[].

  IF P_DIAS IS INITIAL.
    P_DIAS = 2.
  ENDIF.

  LVA_DT_CRIACAO = SY-DATUM - P_DIAS.
  GET TIME STAMP FIELD LVA_DT_CRIACAO_EVENT.
  PERFORM F_CALC_TIMESTAMP USING LVA_DT_CRIACAO_EVENT
                                 ABAP_TRUE
                                 P_DIAS "Dias
                                 0 "Horas
                                 0 "Minutos
                                 0 "Segundos
                     CHANGING LVA_DT_CRIACAO_EVENT.

  "Ler Eventos de Cancelamento Autorizado de Documentos que tiveram averbação de seguro
  SELECT *
    FROM J_1BNFE_EVENT AS EV INTO TABLE LIT_EVENT
   WHERE ISSUE_TMPL GE LVA_DT_CRIACAO_EVENT
     AND EXISTS ( SELECT DOCNUM
                    FROM ZLEST0143 AS AV
                   WHERE AV~DOCNUM           EQ EV~DOCNUM
                     AND AV~CANCEL_AVERBACAO EQ SPACE
                     AND AV~NR_AVERBACAO     NE SPACE
                     AND AV~NR_AVERBACAO     NE C_AVERB_CONTIGENCIA ).

  DELETE LIT_EVENT WHERE NOT ( AUTHCOD IS NOT INITIAL  AND DOCSTA EQ '1'  AND EXT_EVENT = '110111' ).

  "Ler Registros de estorno referenciando Documentos que tiveram averbação de seguro
  SELECT DOCREF
    FROM J_1BNFDOC AS A INTO TABLE LIT_DOC_ESTORNO
   WHERE CREDAT GE LVA_DT_CRIACAO
     AND DOCTYP EQ '5'
     AND EXISTS ( SELECT DOCNUM
                    FROM ZLEST0143 AS AV
                   WHERE AV~DOCNUM           EQ A~DOCREF
                     AND AV~CANCEL_AVERBACAO EQ SPACE
                     AND AV~NR_AVERBACAO     NE SPACE
                     AND AV~NR_AVERBACAO     NE C_AVERB_CONTIGENCIA ).

   LOOP AT LIT_EVENT INTO DATA(LWA_EVENT).
     APPEND VALUE #( DOCNUM = LWA_EVENT-DOCNUM ) TO LIT_DOCS_CANC_AVERB.
   ENDLOOP.

   LOOP AT LIT_DOC_ESTORNO INTO DATA(LWA_DOC_ESTORNO).
     APPEND VALUE #( DOCNUM = LWA_DOC_ESTORNO-DOCNUM ) TO LIT_DOCS_CANC_AVERB.
   ENDLOOP.

   SORT LIT_DOCS_CANC_AVERB BY DOCNUM.
   DELETE ADJACENT DUPLICATES FROM LIT_DOCS_CANC_AVERB COMPARING DOCNUM.

   LOOP AT LIT_DOCS_CANC_AVERB INTO DATA(LWA_DOC_CANC_AVERB).

     CLEAR: LVA_ERRO_PROC.

     TRY.
        ZCL_AVERBACAO_SEGURO=>CANCELAR_AVERBACAO_CTE( I_DOCNUM = LWA_DOC_CANC_AVERB-DOCNUM ).
     CATCH ZCX_AVERBACAO_SEGURO.
        LVA_ERRO_PROC = ABAP_TRUE.
     CATCH ZCX_ARQUIVO.
       LVA_ERRO_PROC = ABAP_TRUE.
     CATCH ZCX_CADASTRO.
       LVA_ERRO_PROC = ABAP_TRUE.
     ENDTRY.

     IF LVA_ERRO_PROC EQ ABAP_TRUE.
       APPEND VALUE #( DOCNUM   = LWA_DOC_CANC_AVERB-DOCNUM
                       MSG_PROC = |Não foi possível cancelar automaticamente a averbação do documento: { LWA_DOC_CANC_AVERB-DOCNUM } ! Realizar processo pela Transação ZLES0148!| ) TO LIT_MSG_PROC.
     ENDIF.

   ENDLOOP.

ENDFORM.


FORM F_ENV_DOCS_CONTINGENCIA.

  DATA: LIT_ZLEST0143  TYPE TABLE OF ZLEST0143,
        LIT_DOC        TYPE TABLE OF TY_DOCS.

  DATA: LVA_DT_LIMITE TYPE ZLEST0143-DT_CADASTRO.

  CLEAR: LIT_DOCS_EMIT_CONT[], LIT_ZLEST0143[].

  LVA_DT_LIMITE = '20220529'.

  "Ler Registros de Documentos que foram emitidos durante o periodo de Contingencia(Autorizado e não cancelado)
  SELECT *
    FROM ZLEST0143 AS A INTO TABLE LIT_ZLEST0143
   WHERE NR_AVERBACAO IN ( C_AVERB_CONTIGENCIA , SPACE )
     AND EXISTS ( SELECT DOCNUM
                    FROM J_1BNFE_ACTIVE AS D
                   WHERE D~DOCNUM EQ A~DOCNUM
                     AND D~DOCSTA EQ '1'
                     AND D~SCSSTA NE '2'
                     AND D~CANCEL EQ ABAP_FALSE ).

  DELETE LIT_ZLEST0143 WHERE CANCEL_AVERBACAO EQ ABAP_TRUE.
  DELETE LIT_ZLEST0143 WHERE DT_CADASTRO < LVA_DT_LIMITE.

  LOOP AT LIT_ZLEST0143 INTO DATA(LWA_ZLEST0143).
    APPEND VALUE #( DOCNUM = LWA_ZLEST0143-DOCNUM ) TO LIT_DOCS_EMIT_CONT.
  ENDLOOP.

  LOOP AT LIT_DOCS_EMIT_CONT INTO DATA(LWA_DOC_EMIT_CONT).

    CLEAR: LVA_ERRO_PROC.

    TRY.
       ZCL_AVERBACAO_SEGURO=>EMITIR_AVERBACAO_CTE( I_DOCNUM = LWA_DOC_EMIT_CONT-DOCNUM ).
     CATCH ZCX_DOC_ELETRONICO INTO DATA(EX_DOC_MDFE).
        LVA_ERRO_PROC = ABAP_TRUE.
     CATCH ZCX_AVERBACAO_SEGURO INTO DATA(CX_AVERBACAO_SEGURO).
        LVA_ERRO_PROC = ABAP_TRUE.
     CATCH ZCX_ARQUIVO INTO DATA(CX_ARQUIVO).
        LVA_ERRO_PROC = ABAP_TRUE.
     CATCH ZCX_CADASTRO INTO DATA(CX_CADASTRO).
        LVA_ERRO_PROC = ABAP_TRUE.
    ENDTRY.

    IF LVA_ERRO_PROC EQ ABAP_TRUE.
      APPEND VALUE #( DOCNUM = LWA_DOC_EMIT_CONT-DOCNUM
                      MSG_PROC = |Não foi possível averbar automaticamente o documento: { LWA_DOC_EMIT_CONT-DOCNUM } ! Realizar processo pela Transação ZLES0148!| ) TO LIT_MSG_PROC .
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM F_ENVIA_EMAIL.

   RANGES: R_HR_EMAIL FOR ZLEST0143-HR_CADASTRO.

   APPEND VALUE #( SIGN = 'I' OPTION = 'BT' LOW = '230000' HIGH = '231000' ) TO R_HR_EMAIL. "Job Roda de 5 em 5 Minutos

   CHECK SY-UZEIT IN R_HR_EMAIL.

   CHECK LIT_MSG_PROC[] IS NOT INITIAL.

   SELECT * from ZLEST0224 INTO TABLE LIT_ZLEST0224.
   CHECK LIT_ZLEST0224[] IS NOT INITIAL.

   PERFORM: f_corpo_email,
            f_send_email USING 'ALERTA - PROCESSAMENTO AVERBAÇÃO DE SEGURO'.

ENDFORM.

FORM f_corpo_email.

  new_line'<!DOCTYPE HTML>'.
  new_line'<HTML>'.
  new_line'<BODY>'.
  new_line'<STYLE>'.

  new_line' TABLE, TH, TD { BORDER: 1PX SOLID BLACK; BORDER-COLLAPSE: COLLAPSE; }'.
  new_line' TH, TD { PADDING: 5PX; }'.
  new_line' TH { TEXT-ALIGN: LEFT; }'.
  new_line' TABLE#T01 TH { BACKGROUND-COLOR:#1c75b9; COLOR: WHITE; }'.
  new_line'</STYLE>'.

  new_line' <H3 STYLE="COLOR:#191970" ALIGN=LEFT><b>Documentos com Pendência de Averbação de Seguro<b></H3>'.
  new_line''.
  new_line' <TABLE BORDER="1" STYLE="WIDTH:50%" ID="T01">'.
  new_line'  <TR>'.
  new_line'   <TH><FONT SIZE="2">Documento</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2">Mensagem Processamento</FONT></TH>'.
  new_line' </TR>'.

  LOOP AT LIT_MSG_PROC INTO DATA(LWS_MSG_PROC).

    new_line' <TR>'.
    new_line'  <TD><FONT SIZE="2">'.
    new_line      LWS_MSG_PROC-DOCNUM.
    new_line'  </FONT></TD>'.

    new_line'  <TD><FONT SIZE="2">'.
    new_line      LWS_MSG_PROC-MSG_PROC.
    new_line'  </FONT></TD>'.

    new_line '</TR>'.

  ENDLOOP.

  new_line ' </TABLE>'.
  new_line'  <h6>Este e-mail é automatizado</h6> '.
  new_line '</BODY>'.
  new_line '</HTML>'.

ENDFORM.

FORM f_send_email USING p_subject.

  LOOP AT LIT_ZLEST0224 INTO DATA(wa_email).
    add_receiver wa_email-email 'X'.
  ENDLOOP.

  lv_lines = lines( it_contents_txt ).

  CLEAR wa_packing_list.
  wa_packing_list-transf_bin = space.
  wa_packing_list-head_start = 1.
  wa_packing_list-head_num   = 0.
  wa_packing_list-body_start = 1.
  wa_packing_list-body_num   = lv_lines.
  wa_packing_list-doc_type   = 'HTM'.
  APPEND wa_packing_list TO it_packing_list.

  READ TABLE it_contents_txt INTO wa_contents_txt INDEX lv_lines.

  wa_doc_data-obj_descr = p_subject.
  wa_doc_data-doc_size  = ( lv_lines - 1 ) * 255 + strlen( wa_contents_txt ).

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = wa_doc_data
      put_in_outbox              = 'X'
      sender_address             = ''
      sender_address_type        = ''
      commit_work                = 'X'
    TABLES
      packing_list               = it_packing_list
      object_header              = it_header
      contents_bin               = it_contents_bin
      contents_txt               = it_contents_txt
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

ENDFORM.


FORM F_CALC_TIMESTAMP USING IV_REFDATE   TYPE  TZNTSTMPS
                            IV_XBACKWARD TYPE  CHAR01
                            IV_DAYS      TYPE  INT4
                            IV_HOURS     TYPE  INT4
                            IV_MINUTES   TYPE  INT4
                            IV_SECONDS   TYPE  INT4
                   CHANGING EV_DATE      TYPE  TZNTSTMPS.

  CONSTANTS:
    min_per_day TYPE i VALUE 1440,
    sec_per_day TYPE i VALUE 86400.

  DATA:
    sec_left        TYPE i,
    sec_total       TYPE i,
    additional_days TYPE i,
    lv_start_date   TYPE dats,
    lv_start_time   TYPE tims,
    lv_result_date  TYPE dats,
    lv_result_time  TYPE tims.

*------ timestamp to date/time ----------------------------------------
  CONVERT TIME STAMP iv_refdate TIME ZONE sy-zonlo INTO DATE lv_start_date TIME lv_start_time.

  sec_total = ( iv_hours * 60 + iv_minutes ) * 60 + iv_seconds.
  additional_days = sec_total DIV sec_per_day.
  sec_left = sec_total MOD sec_per_day.

  IF iv_xbackward IS INITIAL.
*------ start date plus delta -----------------------------------------
    lv_result_time = lv_start_time + sec_left.
    IF lv_result_time <= lv_start_time AND sec_left > 0. "really <= !
      ADD 1 TO additional_days.
    ENDIF.

    lv_result_date = lv_start_date + iv_days + additional_days.

  ELSE.
*------ start date minus delta ----------------------------------------
    lv_result_time = lv_start_time - sec_left.
    IF lv_result_time > lv_start_time AND sec_left > 0. "really greater than!
      ADD 1 TO additional_days.
    ENDIF.

    lv_result_date = lv_start_date - iv_days - additional_days.

  ENDIF.

*------ date/time to timestamp ----------------------------------------
  CONVERT DATE lv_result_date TIME lv_result_time INTO TIME STAMP ev_date TIME ZONE sy-zonlo.

ENDFORM.
