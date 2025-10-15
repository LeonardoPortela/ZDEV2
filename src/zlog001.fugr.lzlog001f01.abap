
FORM f_prepare_run_time_info .

  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>[].
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>[].
  ENDIF.

  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>.
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>.
  ENDIF.

  FREE: l_data,  l_data_line,  l_data_descr,  l_data_line_descr.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                          metadata = abap_false
                                          data     = abap_true ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_runtime_info
*&---------------------------------------------------------------------*
FORM f_get_runtime_info .

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data_descr      = l_data_descr
          r_data_line_descr = l_data_line_descr ).

      CHECK ( l_data_descr IS NOT INITIAL ) OR ( l_data_line_descr IS  NOT INITIAL ).

      CREATE DATA l_data      TYPE HANDLE  l_data_descr.
      CREATE DATA l_data_line TYPE HANDLE  l_data_line_descr.

      ASSIGN l_data->* TO <t_data>.
      ASSIGN l_data_line->* TO <t_data_line>.

      cl_salv_bs_runtime_info=>get_data( IMPORTING t_data      = <t_data>
                                                   t_data_line = <t_data_line> ).
    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN l_data->*        TO <w_data>.
  ASSIGN l_data_line->*   TO <w_data_line>.


ENDFORM.



FORM f_email_aviso_inatividade  USING p_data.

  DATA: objpack     LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE.
  DATA: objhead     LIKE solisti1   OCCURS  1 WITH HEADER LINE.
  DATA: objbin_ord  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_log  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_ann  TYPE solisti1.
  DATA: objbin    LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        wa_objbin LIKE LINE OF objbin.
  DATA: content_hex TYPE STANDARD TABLE OF solix WITH HEADER LINE.
  DATA: objtxt      LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: reclist     LIKE somlreci1  OCCURS  5 WITH HEADER LINE.
  DATA: doc_chng    LIKE sodocchgi1.
  DATA: tab_lines   LIKE sy-tabix.
  DATA: it_html     TYPE TABLE OF w3html INITIAL SIZE 0 WITH HEADER LINE.

  DATA: vl_titulo   TYPE string.
  DATA: vl_tipo     TYPE string.
  DATA: lva_value_html TYPE string.

  DEFINE conc_html.

    lva_value_html = &1.

    CALL FUNCTION 'ZHTML_ADD'
      EXPORTING
        i_texto = lva_value_html
      TABLES
        it_html = it_html.

  END-OF-DEFINITION.

  "Emails Notificação
  SELECT *
    FROM tvarvc INTO TABLE @DATA(lit_email_alert)
   WHERE name = 'ZLOG_SM20N_PAR_005'.

  CHECK lit_email_alert[] IS NOT INITIAL.

  LOOP AT lit_email_alert INTO DATA(lwa_email).
    TRANSLATE lwa_email-low TO LOWER CASE.
    reclist-receiver = lwa_email-low.
    reclist-rec_type = 'U'.
    APPEND reclist.
  ENDLOOP.

  vl_titulo = |Há logs de inatividade no dia { p_data } |.

  "Monta Corpo Email
  conc_html '<html>'.
  conc_html '<head><title>'.
  conc_html vl_titulo.
  conc_html '  </title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
  conc_html '<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.

  conc_html '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>'.
  conc_html vl_titulo.
  conc_html '</STRONG></FONT></DIV><BR>'.
  conc_html '<FONT face=Verdana color=#0000ff size=2>'.
  conc_html '<BR>'.

  conc_html '<table cellspacing="0" border="1" bordercolor="FFFFFF" width="100%">'.

  conc_html '<BR>'.
  conc_html '<BR>'.
  conc_html '<DIV align=left>'.

  conc_html '<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>E-mail gerado automáticamente pelo sistema</STRONG></FONT></DIV>'.

  conc_html '</DIV>'.
  conc_html '<BR>'.
  conc_html '</body>'.
  conc_html '</html>'.

  "Corpo
  doc_chng-obj_name = vl_titulo..
  doc_chng-obj_descr = vl_titulo.
  doc_chng-no_change = 'X'.

  CLEAR objpack-transf_bin.
  objpack-head_start = 1.
  objpack-head_num = 0.
  objpack-body_start = 1.
  objpack-body_num = 99999.
  objpack-doc_type = 'HTM'.
  APPEND objpack.

  "Enviar
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = objpack
      contents_txt               = it_html
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      OTHERS                     = 99.


ENDFORM.                    " EMAIL_AVISO_ENC

FORM f_registrar_tempo_atividade  TABLES t_dados_sm20n_dev_qas TYPE rsau_t_result
                                   USING p_tvarvc_par_002 TYPE tvarvc
                                         p_eclipse        TYPE char01.

  DATA: lv_tempo_atividade TYPE int4,
        lv_total_atividade TYPE int4.

  DATA: lva_dt_inicio_atividade TYPE erdat,
        lva_hr_inicio_atividade TYPE erzet,
        lva_dt_fim_atividade    TYPE erdat,
        lva_hr_fim_atividade    TYPE erzet.

  SORT t_dados_sm20n_dev_qas BY sal_date sal_time.

  LOOP AT t_dados_sm20n_dev_qas INTO DATA(lwa_dados_sm20n).

    DATA(_tabix) = sy-tabix.

    IF _tabix EQ 1.
      "Inicia Marcação Horario Atividade
      lva_dt_inicio_atividade = lwa_dados_sm20n-sal_date.
      lva_hr_inicio_atividade = lwa_dados_sm20n-sal_time.

      APPEND INITIAL LINE TO git_periodo_atividade ASSIGNING FIELD-SYMBOL(<fs_periodo_atividade>).
      <fs_periodo_atividade>-usnam                = lwa_dados_sm20n-slguser.
      <fs_periodo_atividade>-erdat                = lva_dt_inicio_atividade.
      <fs_periodo_atividade>-erzet_ini            = lva_hr_inicio_atividade.

      CONTINUE.
    ENDIF.

    lva_dt_fim_atividade = lwa_dados_sm20n-sal_date.
    lva_hr_fim_atividade = lwa_dados_sm20n-sal_time.

    lv_tempo_atividade = lva_hr_fim_atividade - lva_hr_inicio_atividade.

    IF ( lv_tempo_atividade < p_tvarvc_par_002-low ).

      "Atualiza tempo de atividade do registro
      lva_dt_inicio_atividade = lwa_dados_sm20n-sal_date.
      lva_hr_inicio_atividade = lwa_dados_sm20n-sal_time.

      <fs_periodo_atividade>-erzet_fim            = lva_hr_fim_atividade.
      <fs_periodo_atividade>-atividade_segundos   = <fs_periodo_atividade>-erzet_fim - <fs_periodo_atividade>-erzet_ini.
      <fs_periodo_atividade>-atividade_minutos    = <fs_periodo_atividade>-atividade_segundos / 60.
      <fs_periodo_atividade>-atividade_horas      = <fs_periodo_atividade>-atividade_minutos  / 60.


      CONTINUE.
    ELSE.
      "Inicia nova Marcação Horario Atividade
      lva_dt_inicio_atividade = lwa_dados_sm20n-sal_date.
      lva_hr_inicio_atividade = lwa_dados_sm20n-sal_time.

      APPEND INITIAL LINE TO git_periodo_atividade ASSIGNING <fs_periodo_atividade>.
      <fs_periodo_atividade>-usnam                = lwa_dados_sm20n-slguser.
      <fs_periodo_atividade>-erdat                = lva_dt_inicio_atividade.
      <fs_periodo_atividade>-erzet_ini            = lva_hr_inicio_atividade.

      CONTINUE.

    ENDIF.

  ENDLOOP.

  DELETE git_periodo_atividade WHERE erzet_fim IS INITIAL OR atividade_segundos IS INITIAL.

  LOOP AT git_periodo_atividade ASSIGNING <fs_periodo_atividade>.
    <fs_periodo_atividade>-eclipse = p_eclipse.
  ENDLOOP.

  IF git_periodo_atividade[] IS NOT INITIAL.
    MODIFY zlog1002 FROM TABLE git_periodo_atividade.
  ENDIF.



ENDFORM.
