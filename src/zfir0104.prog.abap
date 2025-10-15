*&---------------------------------------------------------------------*
*& Report ZFIR0104
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0104.

TABLES: tcurr.

DATA: lr_kurst TYPE RANGE OF tcurr-kurst,
      lr_email TYPE RANGE OF char255,
      lr_fcurr TYPE RANGE OF tcurr-fcurr.

DATA gv_dt_cotacao TYPE datum.

DATA: gt_tcurr TYPE TABLE OF tcurr.
" Ajustes - ZFIR0104 - Email dolar argentina #146697  PANF - Inicio
DATA: gt_tcurr_dados TYPE TABLE OF tcurr.
" Ajustes - ZFIR0104 - Email dolar argentina #146697  PANF - Fim

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-h01.
  PARAMETERS: p_gdatu TYPE datum.
SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.

  PERFORM p_get_stvarv.
  PERFORM p_get_dados.
  PERFORM p_envia_email.




FORM p_get_stvarv.

  SELECT *
  FROM tvarvc
  INTO TABLE @DATA(lt_tvarvc)
     WHERE name = 'Z_TAXA_AR_KURST'.

  lr_kurst = VALUE #( FOR ls_tvarvc IN lt_tvarvc
                       ( sign = 'I' option = 'EQ' low = ls_tvarvc-low ) ).

  SELECT *
  FROM tvarvc
  INTO TABLE lt_tvarvc
     WHERE name = 'Z_TAXA_AR_EMAIL'.

  lr_email = VALUE #( FOR ls_tvarvc IN lt_tvarvc
                     ( sign = 'I' option = 'EQ' low = ls_tvarvc-low ) ).


  lr_fcurr = VALUE #( ( sign = 'I' option = 'EQ' low = 'ARS' )
                      ( sign = 'I' option = 'EQ' low = 'USD' ) ).

  IF lr_email IS INITIAL.
    MESSAGE e000(z_fi) WITH 'Email não cadastrado: Z_TAXA_AR_EMAIL'.
  ENDIF.

ENDFORM.

FORM p_get_dados.

  DATA: lv_gdatu TYPE scurr-gdatu,
        lv_data  TYPE scurr-gdatu.

  IF p_gdatu IS NOT INITIAL.

    gv_dt_cotacao = p_gdatu.

  ELSE.

    gv_dt_cotacao = sy-datum + 1.

  ENDIF.

  CONCATENATE gv_dt_cotacao+6(2) gv_dt_cotacao+4(2) gv_dt_cotacao(4) INTO lv_data.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = lv_data
    IMPORTING
      output = lv_gdatu.


  DO  5 TIMES.

    SELECT *
      FROM tcurr
      INTO TABLE @gt_tcurr
      WHERE kurst IN @lr_kurst
      AND fcurr IN @lr_fcurr
      AND tcurr IN @lr_fcurr
      AND gdatu = @lv_gdatu.
    IF sy-subrc = 0.

      " Ajustes - ZFIR0104 - Email dolar argentina #146697  PANF - Inicio
      SORT: gt_tcurr BY fcurr tcurr kurst.

      LOOP AT gt_tcurr ASSIGNING FIELD-SYMBOL(<fs_b>) WHERE kurst = 'B'.

        READ TABLE gt_tcurr ASSIGNING FIELD-SYMBOL(<fs_m>) WITH KEY fcurr = <fs_b>-fcurr
                                                                    tcurr = <fs_b>-tcurr
                                                                    kurst = 'M'
                                                                    BINARY SEARCH.
        IF sy-subrc = 0.

          READ TABLE gt_tcurr ASSIGNING FIELD-SYMBOL(<fs_g>) WITH KEY fcurr = <fs_b>-fcurr
                                                                      tcurr = <fs_b>-tcurr
                                                                      kurst = 'G'
                                                                      BINARY SEARCH.

          IF sy-subrc = 0.
            APPEND CORRESPONDING #( <fs_b> ) TO gt_tcurr_dados.
            APPEND CORRESPONDING #( <fs_m> ) TO gt_tcurr_dados.
            APPEND CORRESPONDING #( <fs_g> ) TO gt_tcurr_dados.
          ENDIF.

        ENDIF.

      ENDLOOP.

      " Ajustes - ZFIR0104 - Email dolar argentina #146697  PANF - Fim
    ENDIF.


    IF gt_tcurr_dados IS INITIAL.
      WAIT UP TO 1800 SECONDS. "Esperar 30 minutos
    ELSE.
      EXIT.
    ENDIF.

  ENDDO.

  IF gt_tcurr_dados IS INITIAL.
    MESSAGE e000(z_fi) WITH 'Dados não encontrados'.
  ENDIF.

ENDFORM.

FORM p_envia_email.

  DATA: t_html TYPE string,
        t_host TYPE sy-host.
  DATA: lt_mail_body TYPE crmt_email_mime_struc,
        ls_mail_body TYPE crms_email_mime_struc,
        lt_to        TYPE crmt_email_recipients,
        lt_copy      TYPE crmt_email_recipients.
  DATA: lv_dt_cotacao(10) TYPE c,
        lv_data_aux       TYPE c LENGTH 10,
        lv_tx_cambio      TYPE char12,
        lv_activity       TYPE sysuuid_x.
  DATA: lo_create_mail TYPE REF TO cl_crm_email_data.


  WRITE gv_dt_cotacao TO lv_data_aux USING EDIT MASK '__/__/____'.

  SORT gt_tcurr_dados BY gdatu DESCENDING.

  IF gt_tcurr_dados IS NOT INITIAL.

    CREATE OBJECT lo_create_mail.


    CLEAR t_html.
    CONCATENATE t_html '<html>'  INTO t_html.
    CONCATENATE t_html '<head>'  INTO t_html.
    CONCATENATE t_html '</head>' INTO t_html.
    CONCATENATE t_html '<body>'  INTO t_html.

    TRANSLATE t_host TO UPPER CASE.
    CONCATENATE t_html '<div align=center><font face=Verdana size=4>' t_host '</font></div>'            INTO t_html.
    CONCATENATE t_html '<div align=center><font face=Verdana size=3>Taxa de Câmbio - PTAX</font></div>' INTO t_html.
    CONCATENATE t_html '<div align=center><font face=Verdana size=3></font></div>' INTO t_html.
    CONCATENATE t_html '<div align=center><font face=Verdana size=3>Data Cotação: ' lv_data_aux  INTO t_html SEPARATED BY space.
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

    LOOP AT gt_tcurr_dados INTO DATA(wa_tcurr).

      CONCATENATE t_html '<tr><font face=Verdana size=1>' INTO t_html.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
        EXPORTING
          input  = wa_tcurr-gdatu
        IMPORTING
          output = lv_data_aux.

      CONCATENATE t_html '<td align=center>' lv_data_aux     '</td>' INTO t_html.
      CONCATENATE t_html '<td align=center>' wa_tcurr-kurst '</td>' INTO t_html.
      CONCATENATE t_html '<td align=center>' wa_tcurr-fcurr '</td>' INTO t_html.
      CONCATENATE t_html '<td align=center>' wa_tcurr-tcurr '</td>' INTO t_html.

      CALL FUNCTION 'CONVERSION_EXIT_EXCRT_OUTPUT'
        EXPORTING
          input  = wa_tcurr-ukurs
        IMPORTING
          output = lv_tx_cambio.

      REPLACE ALL OCCURRENCES OF  '/'  IN  lv_tx_cambio WITH ' ' IGNORING CASE.

      CONCATENATE t_html '<td align=right>' lv_tx_cambio '</td>' INTO t_html.
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

    LOOP AT lr_email ASSIGNING FIELD-SYMBOL(<fs_email>).
      APPEND INITIAL LINE TO lt_to ASSIGNING FIELD-SYMBOL(<fs_recep>).
      <fs_recep>-address = <fs_email>-low.
      MOVE lt_to TO lo_create_mail->to.
    ENDLOOP.


    CALL METHOD cl_crm_email_utility_base=>send_email
      EXPORTING
        iv_mail_data       = lo_create_mail
      RECEIVING
        ev_send_request_id = lv_activity.

    COMMIT WORK.

  ENDIF.

ENDFORM.
