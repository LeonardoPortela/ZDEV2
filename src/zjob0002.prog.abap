*&---------------------------------------------------------------------*
*& Report  ZJOB0002
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zjob0002.

DATA: vg_job            TYPE i,
      lc_inicio         TYPE datum,
      it_zjob0002       TYPE TABLE OF zjob0002 WITH HEADER LINE,
      it_zjob0001       TYPE TABLE OF zjob0001 WITH HEADER LINE,
      i_html_entra      TYPE string,
      mail_destinatario TYPE string,
      v_anal_resp       TYPE zde_usnam_basis.

DATA: it_bapiret2    TYPE TABLE OF bapiret2 WITH HEADER LINE,
      it_bapiadsmtp  TYPE TABLE OF bapiadsmtp WITH HEADER LINE,
      lo_create_mail TYPE REF TO cl_crm_email_data,
      ls_mail_body   TYPE crms_email_mime_struc,
      ls_recep       TYPE crms_email_recipient,
      lv_activity    TYPE sysuuid_x.


SELECT SINGLE COUNT( * ) INTO vg_job
  FROM tbtco
 WHERE jobname EQ 'GESTAO_JOB_CANCELADO'
   AND status  EQ 'R'.

IF ( vg_job EQ 1 ).

*  CALL FUNCTION 'OIUREP_MONTH_FIRST_LAST'
*    EXPORTING
*      I_DATE      = SY-DATUM
*    IMPORTING
*      E_FIRST_DAY = LC_INICIO
*    EXCEPTIONS
*      WRONG_DATE  = 1
*      OTHERS      = 2.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = sy-datum
      days      = 1
      months    = 0
      signum    = '-'
      years     = 0
    IMPORTING
      calc_date = lc_inicio.

  SELECT * INTO TABLE @DATA(it_tbtco)
    FROM tbtco AS j
   WHERE j~sdlstrtdt GE @lc_inicio
     AND j~status    EQ 'A'
     AND EXISTS ( SELECT * FROM zjob0001 AS t
                   WHERE t~jobname  = j~jobname )
     AND NOT EXISTS ( SELECT * FROM zjob0002 AS r
                       WHERE r~jobname  = j~jobname
                         AND r~jobcount = j~jobcount ).

  LOOP AT it_tbtco INTO DATA(wa_tbtco).
    it_zjob0002-jobname   = wa_tbtco-jobname.
    it_zjob0002-jobcount  = wa_tbtco-jobcount.
    it_zjob0002-dt_cancel = wa_tbtco-enddate.
    it_zjob0002-hr_cancel = wa_tbtco-endtime.
    APPEND it_zjob0002.
  ENDLOOP.

  IF it_zjob0002[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_job)
      FROM zjob0001
      FOR ALL ENTRIES IN @it_zjob0002
     WHERE jobname EQ @it_zjob0002-jobname.
  ENDIF.

  "Envio Usuário Processador
***  MOVE IT_JOB[] TO IT_ZJOB0001[].
***  SORT IT_ZJOB0001 BY USUARIO_PROC.
***  DELETE ADJACENT DUPLICATES FROM IT_ZJOB0001 COMPARING USUARIO_PROC.

***  LOOP AT IT_ZJOB0001.
***    PERFORM CABECALHO_E_MAIL.

***    "Comentado para não enviar mais email para usuário de sistema ( JOBADM )
***    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
***      EXPORTING
***        USERNAME = IT_ZJOB0001-USUARIO_PROC
***      TABLES
***        RETURN   = IT_BAPIRET2
***        ADDSMTP  = IT_BAPIADSMTP.
***
***    READ TABLE IT_BAPIADSMTP INDEX 1.
***    IF SY-SUBRC IS INITIAL AND IT_BAPIADSMTP-E_MAIL IS NOT INITIAL.
***      MAIL_DESTINATARIO = IT_BAPIADSMTP-E_MAIL.
***    ELSE.
***      CONTINUE.
***    ENDIF.
***
***    "jobs do usuário
***    LOOP AT IT_JOB INTO DATA(WA_JOB) WHERE USUARIO_PROC EQ IT_ZJOB0001-USUARIO_PROC.
***      "logs do job cancelado
***      LOOP AT IT_TBTCO INTO WA_TBTCO WHERE JOBNAME EQ WA_JOB-JOBNAME.
***        CONCATENATE I_HTML_ENTRA '<TR><FONT face=Verdana size=1>' INTO I_HTML_ENTRA.
***        CONCATENATE I_HTML_ENTRA '<TD align=center>' WA_TBTCO-JOBNAME  '</TD>' INTO I_HTML_ENTRA.
***        CONCATENATE I_HTML_ENTRA '<TD align=center>' WA_TBTCO-RELUNAME '</TD>' INTO I_HTML_ENTRA.
***        CONCATENATE I_HTML_ENTRA '<TD align=center>' WA_TBTCO-STRTDATE '</TD>' INTO I_HTML_ENTRA.
***        CONCATENATE I_HTML_ENTRA '<TD align=center>' WA_TBTCO-STRTTIME '</TD>' INTO I_HTML_ENTRA.
***        CONCATENATE I_HTML_ENTRA '<TD align=center>' WA_TBTCO-ENDDATE  '</TD>' INTO I_HTML_ENTRA.
***        CONCATENATE I_HTML_ENTRA '<TD align=center>' WA_TBTCO-ENDTIME  '</TD>' INTO I_HTML_ENTRA.
***        CONCATENATE I_HTML_ENTRA '</FONT></TR>' INTO I_HTML_ENTRA.
***        READ TABLE IT_ZJOB0002 WITH KEY JOBNAME = WA_TBTCO-JOBNAME JOBCOUNT = WA_TBTCO-JOBCOUNT ASSIGNING FIELD-SYMBOL(<FS_PROCESSADOR>).
***        <FS_PROCESSADOR>-USUARIO_PROC  = IT_ZJOB0001-USUARIO_PROC.
***        <FS_PROCESSADOR>-CK_EMAIL_PROC = ABAP_TRUE.
***        <FS_PROCESSADOR>-EMAIL_PROC    = MAIL_DESTINATARIO.
***      ENDLOOP.
***    ENDLOOP.
***    PERFORM RODAPE_E_MAIL.
***    PERFORM ENVIAR_E_MAIL.
***  ENDLOOP.

  "Envio Analista Responsável
  MOVE it_job[] TO it_zjob0001[].
  SORT it_zjob0001 BY anal_resp.
  DELETE ADJACENT DUPLICATES FROM it_zjob0001 COMPARING anal_resp.

  SELECT SINGLE low
  FROM tvarvc
    INTO @DATA(email_infra)
  WHERE name EQ 'ENVIAR_EMAIL_INFRA_JOB'.

  SELECT SINGLE low
  FROM tvarvc
    INTO @DATA(user_infra)
  WHERE name EQ 'USER_SAP_INFRA_RESP'.

  LOOP AT it_zjob0001.
    PERFORM cabecalho_e_mail.
    v_anal_resp = it_zjob0001-anal_resp(12).
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = v_anal_resp
      TABLES
        return   = it_bapiret2
        addsmtp  = it_bapiadsmtp.

    READ TABLE it_bapiadsmtp INDEX 1.
    IF sy-subrc IS INITIAL AND it_bapiadsmtp-e_mail IS NOT INITIAL.
      IF user_infra EQ v_anal_resp.
        mail_destinatario = email_infra.
      ELSE.
        mail_destinatario = it_bapiadsmtp-e_mail.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

    "jobs do usuário
    LOOP AT it_job INTO DATA(wa_job) WHERE anal_resp EQ it_zjob0001-anal_resp.
      "logs do job cancelado
      LOOP AT it_tbtco INTO wa_tbtco WHERE jobname EQ wa_job-jobname.
        CONCATENATE i_html_entra '<TR><FONT face=Verdana size=1>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-jobname  '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-reluname '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-strtdate '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-strttime '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-enddate  '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-endtime  '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '</FONT></TR>' INTO i_html_entra.
        READ TABLE it_zjob0002 WITH KEY jobname = wa_tbtco-jobname jobcount = wa_tbtco-jobcount ASSIGNING FIELD-SYMBOL(<fs_processador>).
        <fs_processador>-usuario_resp  = v_anal_resp.
        <fs_processador>-ck_email_resp = abap_true.
        <fs_processador>-email_resp    = mail_destinatario.
      ENDLOOP.
    ENDLOOP.
    PERFORM rodape_e_mail.
    PERFORM enviar_e_mail.
  ENDLOOP.

  "Envio Analista BASIS
  MOVE it_job[] TO it_zjob0001[].
  SORT it_zjob0001 BY usnam_basis.
  DELETE ADJACENT DUPLICATES FROM it_zjob0001 COMPARING usnam_basis.

  LOOP AT it_zjob0001.
    PERFORM cabecalho_e_mail.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = it_zjob0001-usnam_basis
      TABLES
        return   = it_bapiret2
        addsmtp  = it_bapiadsmtp.

    READ TABLE it_bapiadsmtp INDEX 1.
    IF sy-subrc IS INITIAL AND it_bapiadsmtp-e_mail IS NOT INITIAL.
      IF user_infra EQ v_anal_resp.
        mail_destinatario = email_infra.
      ELSE.
        mail_destinatario = it_bapiadsmtp-e_mail.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

    "jobs do usuário
    LOOP AT it_job INTO wa_job WHERE usnam_basis EQ it_zjob0001-usnam_basis.
      "logs do job cancelado
      LOOP AT it_tbtco INTO wa_tbtco WHERE jobname EQ wa_job-jobname.
        CONCATENATE i_html_entra '<TR><FONT face=Verdana size=1>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-jobname  '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-reluname '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-strtdate '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-strttime '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-enddate  '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '<TD align=center>' wa_tbtco-endtime  '</TD>' INTO i_html_entra.
        CONCATENATE i_html_entra '</FONT></TR>' INTO i_html_entra.
        READ TABLE it_zjob0002 WITH KEY jobname = wa_tbtco-jobname jobcount = wa_tbtco-jobcount ASSIGNING <fs_processador>.
        <fs_processador>-usuario_basis  = it_zjob0001-usnam_basis.
        <fs_processador>-ck_email_basis = abap_true.
        <fs_processador>-email_basis    = mail_destinatario.
      ENDLOOP.
    ENDLOOP.
    PERFORM rodape_e_mail.
    PERFORM enviar_e_mail.
  ENDLOOP.

  IF it_zjob0002[] IS NOT INITIAL.
    MODIFY zjob0002 FROM TABLE it_zjob0002.
    COMMIT WORK.
  ENDIF.

ENDIF.

*&---------------------------------------------------------------------*
*&      Form  CABECALHO_E_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cabecalho_e_mail .

  CLEAR: i_html_entra.
  CONCATENATE i_html_entra '<html>'  INTO i_html_entra.
  CONCATENATE i_html_entra '<head>'  INTO i_html_entra.
  CONCATENATE i_html_entra '</head>' INTO i_html_entra.
  CONCATENATE i_html_entra '<body>'  INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma>Job(s) Cancelado(s)</FONT></DIV>' INTO i_html_entra SEPARATED BY space.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.

  CONCATENATE i_html_entra '<TABLE border=1 align=center>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TR><FONT face=Verdana size=1><STRONG>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TD align=center>Nome Job</TD>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TD align=center>Usuário Cancelou</TD>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TD align=center>Data Início</TD>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TD align=center>hora Início</TD>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TD align=center>Data Fim</TD>' INTO i_html_entra.
  CONCATENATE i_html_entra '<TD align=center>hora Fim</TD>' INTO i_html_entra.
  CONCATENATE i_html_entra '</TR>' INTO i_html_entra.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  RODAPE_E_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rodape_e_mail .

  CONCATENATE i_html_entra '</TABLE>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma>Este é um e-mail automático' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV align=left>&nbsp;</DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma color:#1F497D>Antes de imprimir, pense em sua responsabilidade e compromisso com o MEIO AMBIENTE! </FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma color:#1F497D>Antes de imprimir, piense en su responsabilidad con el MEDIO AMBIENTE!</FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '<DIV><FONT face=Tahoma color:#1F497D>Before printing, think about your responsibility for the ENVIRONMENT!!!</FONT></DIV>' INTO i_html_entra.
  CONCATENATE i_html_entra '</body>'     INTO i_html_entra.
  CONCATENATE i_html_entra '</html>'     INTO i_html_entra.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_E_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enviar_e_mail .

  CLEAR: lo_create_mail.

  CREATE OBJECT lo_create_mail.

  CLEAR: lo_create_mail->subject.
  lo_create_mail->subject = 'Job(s) Cancelado(s)'.

  CLEAR ls_mail_body.
  ls_mail_body-content_ascii = i_html_entra.
  ls_mail_body-mime_type     = 'text/html'.
  APPEND  ls_mail_body TO lo_create_mail->body.

  CLEAR ls_recep.
  ls_recep-address = mail_destinatario.
  APPEND ls_recep TO lo_create_mail->to.

  CLEAR ls_recep.
*  LS_RECEP-NAME    = MAIL_DESTINATARIO.
*  LS_RECEP-ADDRESS = MAIL_DESTINATARIO.
*  MOVE LS_RECEP TO LO_CREATE_MAIL->FROM.

  CALL METHOD cl_crm_email_utility_base=>send_email
    EXPORTING
      iv_mail_data       = lo_create_mail
    RECEIVING
      ev_send_request_id = lv_activity.

  CLEAR: lo_create_mail.

ENDFORM.
