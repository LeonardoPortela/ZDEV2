*&---------------------------------------------------------------------*
*& Report  ZFIR0082
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zfir088.

TABLES: tcurr.

PARAMETERS: p_true    TYPE c  NO-DISPLAY,
            p_false   TYPE c  NO-DISPLAY,
            p_hora(8) TYPE c NO-DISPLAY.

DATA: v_true    TYPE c,
      v_false   TYPE c,
      v_hora(8) TYPE c.


DATA: lo_create_mail TYPE REF TO cl_crm_email_data,
      lt_to          TYPE crmt_email_recipients,
      lt_copy        TYPE crmt_email_recipients,
      ls_recep       TYPE crms_email_recipient,
      lt_mail_body   TYPE crmt_email_mime_struc,
      ls_mail_body   TYPE crms_email_mime_struc,
      lv_activity    TYPE sysuuid_x.

DATA: it_tcurr   TYPE TABLE OF tcurr,
      lc_datat   TYPE char08,
      lc_gdatu   TYPE gdatu_inv,
      p_data_aux TYPE char10,
      t_html     TYPE string,
      tx_cambio  TYPE char12,
      t_host     TYPE sy-host.

DATA: lc_data          TYPE sy-datum,
      vg_ambiente      TYPE char03,
      v_dt_cotacao(10) TYPE c.

RANGES: r_data_up FOR sy-datum.


START-OF-SELECTION.

  vg_ambiente = 'PRD'.
  CHECK sy-sysid EQ  vg_ambiente.


  REFRESH r_data_up.
  IMPORT r_data_up FROM MEMORY ID 'DATA_UP'.
  DELETE FROM MEMORY ID 'DATA_UP'.

  IMPORT v_true    FROM MEMORY ID 'P_TRUE'.
  IMPORT v_false   FROM MEMORY ID 'P_FALSE'.
  IMPORT v_hora    FROM MEMORY ID 'P_HORA'.

  IF v_hora IS NOT INITIAL.
    p_hora = v_hora.
    DELETE FROM MEMORY ID 'P_HORA'.
  ENDIF.

  READ TABLE r_data_up INDEX 1.
  CONCATENATE r_data_up-low+6(2) '/' r_data_up-low+4(2) '/' r_data_up-low(4) INTO v_dt_cotacao.

  IF v_true IS NOT INITIAL.

    p_true =  v_true.
    DELETE FROM MEMORY ID 'P_TRUE'.

    PERFORM z_envia_email_t.

  ELSEIF v_false IS NOT INITIAL.

    p_false =  v_false.
    DELETE FROM MEMORY ID 'P_FALSE'.

    PERFORM z_envia_email_f.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_ENVIA_EMAIL_T
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_envia_email_t .

  CREATE OBJECT lo_create_mail.



  RANGES: r_gdatu   FOR scurr-gdatu,
          r_kurst   FOR tcurr-kurst,
          r_fcurr   FOR tcurr-fcurr,
          r_tcurr   FOR tcurr-tcurr.

  t_host = sy-host.

  LOOP AT r_data_up.
    r_gdatu-sign   = 'I'.
    r_gdatu-option = 'EQ'.

    lc_data = r_data_up-low.
    CONCATENATE lc_data+6(2) lc_data+4(2) lc_data(4) INTO lc_datat.

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lc_datat
      IMPORTING
        output = r_gdatu-low.

    APPEND r_gdatu.

    CLEAR: lc_data, lc_datat.
  ENDLOOP.


  r_kurst-sign   = 'I'.
  r_kurst-option = 'EQ'.
  r_kurst-low    = 'BCRA'.
  APPEND r_kurst.

  r_fcurr-sign    = 'I'.
  r_fcurr-option  = 'EQ'.
  r_fcurr-low     = 'ARS'.
  APPEND  r_fcurr.

  r_tcurr-sign    = 'I'.
  r_tcurr-option  = 'EQ'.
  r_tcurr-low     = 'USD'.
  APPEND r_tcurr.

  r_fcurr-sign    = 'I'.
  r_fcurr-option  = 'EQ'.
  r_fcurr-low     = 'USD'.
  APPEND  r_fcurr.

  r_tcurr-sign    = 'I'.
  r_tcurr-option  = 'EQ'.
  r_tcurr-low     = 'ARS'.
  APPEND r_tcurr.


  SELECT * FROM tcurr INTO TABLE it_tcurr
    WHERE gdatu IN r_gdatu
     AND  kurst IN r_kurst
     AND  fcurr IN r_fcurr
     AND  tcurr IN r_tcurr.

  SORT it_tcurr BY gdatu DESCENDING.

  IF it_tcurr IS NOT INITIAL.

    CLEAR t_html.
    CONCATENATE t_html '<html>'  INTO t_html.
    CONCATENATE t_html '<head>'  INTO t_html.
    CONCATENATE t_html '</head>' INTO t_html.
    CONCATENATE t_html '<body>'  INTO t_html.

    TRANSLATE t_host TO UPPER CASE.
    CONCATENATE t_html '<div align=center><font face=Verdana size=4>' t_host '</font></div>'            INTO t_html.
    CONCATENATE t_html '<div align=center><font face=Verdana size=3>Taxa de Câmbio - PTAX</font></div>' INTO t_html.
    CONCATENATE t_html '<div align=center><font face=Verdana size=3>Fonte: Banco Central da Argentina</font></div>' INTO t_html.
    CONCATENATE t_html '<div align=center><font face=Verdana size=3>Data Cotação: ' v_dt_cotacao ' - Horário Liberação:  ' p_hora INTO t_html SEPARATED BY space.
    CONCATENATE t_html '<br><br>'                               INTO t_html.
    CONCATENATE t_html '<align=left>&nbsp;</div>'               INTO t_html.
    CONCATENATE t_html '<table border=1 align=center>'          INTO t_html.
    CONCATENATE t_html '<tr><font face=Verdana size=1><strong>' INTO t_html.
    CONCATENATE t_html '<td align=center>Data</td>'             INTO t_html.
    CONCATENATE t_html '<td align=center>Categoria</td>'        INTO t_html.
    CONCATENATE t_html '<td align=center>De Moeda</td>'         INTO t_html.
    CONCATENATE t_html '<td align=center>Para Moeda</td>'       INTO t_html.
    CONCATENATE t_html '<td align=center>Taxa Câmbio</td>'      INTO t_html.
    CONCATENATE t_html '</td>' INTO t_html.

    LOOP AT it_tcurr INTO DATA(wa_tcurr).

      CONCATENATE t_html '<tr><font face=Verdana size=1>' INTO t_html.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = wa_tcurr-gdatu
        IMPORTING
          output = p_data_aux.

      CONCATENATE t_html '<td align=center>' p_data_aux     '</td>' INTO t_html.
      CONCATENATE t_html '<td align=center>' wa_tcurr-kurst '</td>' INTO t_html.
      CONCATENATE t_html '<td align=center>' wa_tcurr-fcurr '</td>' INTO t_html.
      CONCATENATE t_html '<td align=center>' wa_tcurr-tcurr '</td>' INTO t_html.

      CALL FUNCTION 'CONVERSION_EXIT_EXCRT_OUTPUT'
        EXPORTING
          input  = wa_tcurr-ukurs
        IMPORTING
          output = tx_cambio.

      REPLACE ALL OCCURRENCES OF  '/'  IN  tx_cambio WITH ' ' IGNORING CASE.

      CONCATENATE t_html '<td align=right>' tx_cambio '</td>' INTO t_html.
      CONCATENATE t_html '</font></tr>' INTO t_html.

      CLEAR wa_tcurr.
    ENDLOOP.

    CONCATENATE t_html '</table>' INTO t_html.
    CONCATENATE t_html '</body>'  INTO t_html.
    CONCATENATE t_html '</html>'  INTO t_html.

    lo_create_mail->subject =  'Atualização Taxa Câmbio – PTAX (SAP)'.

    CLEAR ls_mail_body.
    ls_mail_body-content_ascii = t_html.
    ls_mail_body-mime_type     = 'text/html'.
    APPEND ls_mail_body TO lt_mail_body.

    MOVE lt_mail_body TO lo_create_mail->body.

    CLEAR ls_recep.
    ls_recep-address = 'csc.financeiro@amaggi.com.br'.
    APPEND ls_recep TO lt_to.
    MOVE lt_to TO lo_create_mail->to.

    ls_recep-address = 'suporte.sap@amaggi.com.br'.
    APPEND ls_recep TO lt_copy.
    MOVE lt_copy TO lo_create_mail->copy.

    CLEAR ls_recep.
    ls_recep-address = ''.
    MOVE ls_recep TO lo_create_mail->from.

    CALL METHOD cl_crm_email_utility_base=>send_email
      EXPORTING
        iv_mail_data       = lo_create_mail
      RECEIVING
        ev_send_request_id = lv_activity.

    COMMIT WORK.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ENVIA_EMAIL_F
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_envia_email_f .

  CREATE OBJECT lo_create_mail.


  CLEAR: t_html.
  CONCATENATE t_html '<html>'   INTO t_html.
  CONCATENATE t_html '<head>'  INTO t_html.
  CONCATENATE t_html '</head>' INTO t_html.
  CONCATENATE t_html '<body>'  INTO t_html.

  CONCATENATE t_html '<div align=center><font face=Verdana size=5> TAXA DE CÂMBIO - PTAX(Banco Central da Argentina) - NÃO ATUALIZADO NO SAP</font></div>' INTO t_html.
  CONCATENATE t_html '<br><br><br>' INTO t_html.
  CONCATENATE t_html '<div align=left><font face=Verdana size=2> Comunicamos que até o momento a Taxa de Câmbio - PTAX do Banco central do Brasil não foi atualizado'
                     'no SAP na transação OB08 para a data de ' v_dt_cotacao '.</font></div>' INTO t_html SEPARATED BY space.
  CONCATENATE t_html '<br><br>' INTO t_html.
  CONCATENATE t_html '<div align=left><font face=Verdana size=2> A orientação é entrar manualmente na transação OB08 e atualizar as cotações para esta data. </font></div>' INTO t_html.

  CONCATENATE t_html '</body>'  INTO t_html.
  CONCATENATE t_html '</html>'   INTO t_html.

  lo_create_mail->subject =  'NÃO ATUALIZADO - Taxa Câmbio – PTAX (SAP - OB08)'.

  CLEAR ls_mail_body.
  ls_mail_body-content_ascii = t_html.
  ls_mail_body-mime_type     = 'text/html'.
  APPEND ls_mail_body TO lt_mail_body.

  MOVE lt_mail_body TO lo_create_mail->body.

  CLEAR ls_recep.

  ls_recep-address = 'csc.financeiro@amaggi.com.br'.
  APPEND ls_recep TO lt_to.
  MOVE lt_to TO lo_create_mail->to.

  ls_recep-address = 'suporte.sap@amaggi.com.br'.
  APPEND ls_recep TO lt_copy.
  MOVE lt_copy TO lo_create_mail->copy.

  CLEAR ls_recep.
  ls_recep-address = ''.
  MOVE ls_recep TO lo_create_mail->from.

  CALL METHOD cl_crm_email_utility_base=>send_email
    EXPORTING
      iv_mail_data       = lo_create_mail
    RECEIVING
      ev_send_request_id = lv_activity.

  COMMIT WORK.

ENDFORM.
