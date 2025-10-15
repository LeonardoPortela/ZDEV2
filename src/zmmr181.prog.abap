* ==================================================================== *
* Program.....: ZMMR181                                                *
* Title.......: JOB dos centros cadastrados na plataforma WebFormat    *
* Author......: Fabrício Fonseca                                       *
* Date........: 01/02/2023                                             *
* Ticket......: CS2022000203                                           *
* -------------------------------------------------------------------- *
REPORT zmmr181.

* Data for sending e-mail:
DATA: it_packing_list TYPE TABLE OF sopcklsti1,
      it_header       TYPE TABLE OF solisti1,
      it_contents_txt TYPE TABLE OF solisti1,
      it_contents_txb TYPE TABLE OF solisti1,
      it_contents_bin TYPE TABLE OF solisti1,
      it_receivers    TYPE TABLE OF somlreci1.

DATA: wa_packing_list TYPE sopcklsti1,
      wa_contents_txt TYPE solisti1,
      wa_receivers    TYPE somlreci1.

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
  APPEND WA_RECEIVERS TO IT_RECEIVERS[].
END-OF-DEFINITION.


SELECT *
  FROM t001l
  INTO TABLE @DATA(lt_centros_nao_cad)
  WHERE NOT EXISTS ( SELECT * FROM zmmt0167 WHERE branch = t001l~werks
                                              AND lgort = t001l~lgort ).

IF sy-subrc = 0 AND lt_centros_nao_cad[] IS NOT INITIAL.

  PERFORM f_email TABLES lt_centros_nao_cad.

ENDIF.
*&---------------------------------------------------------------------*
*&      Form  F_EMAIL
*&---------------------------------------------------------------------*

FORM f_email TABLES p_lt_centros_nao_cad STRUCTURE t001l.

  DATA(vl_assunto) = | Depositos não cadastrados. |.
  PERFORM: corpo_email_e TABLES p_lt_centros_nao_cad,
           send_email_e  USING vl_assunto.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CORPO_EMAIL_E
*&---------------------------------------------------------------------*

FORM corpo_email_e  TABLES p_centros STRUCTURE t001l.

* CRIA TABELA EM HTML EM FORMATO DE E-MAIL.
  new_line'<!DOCTYPE HTML>'.
  new_line'<HTML>'.
  new_line'<BODY>'.
  new_line'<STYLE>'.
  new_line' TABLE, TH, TD { BORDER: 1PX SOLID BLACK; BORDER-COLLAPSE: COLLAPSE; }'.
  new_line' TH, TD { PADDING: 5PX; }'.
  new_line' TH { TEXT-ALIGN: LEFT; }'.
  new_line' TABLE#T01 TH { BACKGROUND-COLOR:#1c75b9; COLOR: WHITE; }'.
  new_line' TABLE#T02 TH { BACKGROUND-COLOR:#218bdb; COLOR: WHITE; }'.
  new_line'</STYLE>'.

  new_line' <H4 ALIGN=LEFT>Depositos não cadastrados na transação ZMM0209, favor verificar.:</H4>'.
  new_line''.
  new_line' <TABLE BORDER="1" STYLE="WIDTH:20%" ID="T02">'.
  new_line'  <TR>'.
  new_line'   <TH><FONT SIZE="2" STYLE="WIDTH:5%">Centro</FONT></TH>'.
  new_line'   <TH><FONT SIZE="2" STYLE="WIDTH:5%">Depósito</FONT></TH>'.
  new_line' </TR>'.

  LOOP AT p_centros[] INTO DATA(wl_centros).
    new_line' <TR>'.

    new_line'  <TD style="width:5%"><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_centros-werks.
    new_line'  </FONT></TD>'.

    new_line'  <TD style="width:5%"><FONT SIZE="2" WHITE-SPACE=NOWRAP>'.
    new_line      wl_centros-lgort.
    new_line'  </FONT></TD>'.

    new_line ' </TR>'.
  ENDLOOP.
  new_line ' </TABLE>'.
  new_line'  <H6>Atenciosamente,</H6><H6><b>ADM-AMAGGI</b></H6><H6>https://www.amaggi.com.br/</H6>'.
  new_line '</BODY>'.
  new_line '</HTML>'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL_E
*&---------------------------------------------------------------------*

FORM send_email_e  USING    p_vl_assunto.

 DATA: lv_lines    LIKE sy-tabix,
        wa_doc_data LIKE sodocchgi1,
        wa_zmail    TYPE zmail,
        it_zmail    TYPE TABLE OF zmail.

  FREE: wa_zmail, it_zmail.
  SELECT *
    FROM zmail
    INTO TABLE it_zmail
    WHERE tcode EQ sy-cprog.

  IF it_zmail[] IS NOT INITIAL.
    LOOP AT it_zmail INTO wa_zmail.
      IF wa_zmail-email IS INITIAL.
        wa_zmail-email = text-001.
      ELSE.
        add_receiver wa_zmail-email 'X'.
      ENDIF.
    ENDLOOP.
  ELSE.
    wa_zmail-email = text-001.
    add_receiver wa_zmail-email 'X'.
  ENDIF.

  lv_lines = lines( it_contents_txt ).

  wa_packing_list-transf_bin = space.
  wa_packing_list-head_start = 1.
  wa_packing_list-head_num   = 0.
  wa_packing_list-body_start = 1.
  wa_packing_list-body_num   = lv_lines.
  wa_packing_list-doc_type   = 'HTM'.
  APPEND wa_packing_list TO it_packing_list.

  READ TABLE it_contents_txt INTO wa_contents_txt INDEX lv_lines.

  wa_doc_data-obj_descr = p_vl_assunto.
  wa_doc_data-doc_size  = ( lv_lines - 1 ) * 255 + strlen( wa_contents_txt ).

  CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
    TABLES
      text_tab   = it_contents_txb
      binary_tab = it_contents_bin
    EXCEPTIONS
      failed     = 1
      OTHERS     = 2.

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
  IF sy-subrc IS INITIAL.
    CLEAR: wa_packing_list, it_contents_bin[], it_packing_list[], it_header[], it_contents_txt[], it_receivers[].
    IF sy-batch IS INITIAL.
      MESSAGE 'Mensagem enviada com sucesso!'(005) TYPE 'S'.
    ENDIF.
  ELSE.
    IF sy-batch IS INITIAL.
      MESSAGE 'Erro no envio!'(e01) TYPE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.
