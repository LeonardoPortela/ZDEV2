*&---------------------------------------------------------------------*
*& Report  ZHCMR_PA0107
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zhcmr_pa0107.

DATA: lv_lines          TYPE sy-tabix,
      lv_qtd            TYPE sy-tabix,
      lit_zhcmt0007_aux TYPE TABLE OF zhcmt0007.

START-OF-SELECTION.

  TRY.
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd  = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.

  SELECT *
    FROM zhcmt0007 INTO TABLE @DATA(lit_zhcmt0007)
   WHERE int_sistemas_legado EQ @abap_false.

  CHECK lit_zhcmt0007[] IS NOT INITIAL.

  DATA(_error) = abap_false.

  DESCRIBE TABLE lit_zhcmt0007[] LINES lv_lines.

  LOOP AT lit_zhcmt0007 ASSIGNING FIELD-SYMBOL(<fs_zhcmt0007>).

    ADD 1 TO lv_qtd.

   APPEND <fs_zhcmt0007> TO lit_zhcmt0007_aux.

    IF lv_qtd = 500 OR lv_qtd = lv_lines.

      TRY .
          zcl_int_ob_funcionarios_legado=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = lit_zhcmt0007_aux ).
        CATCH zcx_integracao INTO DATA(zcx_integracao).
          _error = abap_true.
          MESSAGE ID zcx_integracao->zif_error~msgid TYPE 'I'
           NUMBER zcx_integracao->zif_error~msgno
             WITH zcx_integracao->zif_error~msgv1
                  zcx_integracao->zif_error~msgv2
                  zcx_integracao->zif_error~msgv3
                  zcx_integracao->zif_error~msgv4.
        CATCH zcx_error INTO DATA(zcx_error).
          _error = abap_true.
          MESSAGE ID zcx_error->zif_error~msgid TYPE 'I'
           NUMBER zcx_error->zif_error~msgno
             WITH zcx_error->zif_error~msgv1
                  zcx_error->zif_error~msgv2
                  zcx_error->zif_error~msgv3
                  zcx_error->zif_error~msgv4.

      ENDTRY.

      IF _error EQ abap_true.
        PERFORM f_notifica_suporte_sap.
      ENDIF.

      IF 1 = 2.
        PERFORM f_carga_zhcmt0007.
      ENDIF.

      lv_lines = lv_lines - lv_qtd.

      CLEAR lv_qtd.
      REFRESH: lit_zhcmt0007_aux.

    ENDIF.

  ENDLOOP.


FORM f_notifica_suporte_sap .

  DATA: vl_titulo    TYPE string.
  DATA: lit_zsdt0105 TYPE TABLE OF zsdt0105.
  DATA: lit_zsdt0296 TYPE TABLE OF zsdt0296.
  DATA: lit_zmail    TYPE TABLE OF zmail.
  DATA: it_html      TYPE TABLE OF w3html INITIAL SIZE 0.


  DATA: objpack     TYPE TABLE OF sopcklsti1,
        lwa_objpack TYPE sopcklsti1.

  DATA: objhead     TYPE TABLE OF solisti1.
  DATA: objbin_ord  TYPE TABLE OF solisti1.
  DATA: objbin_log  TYPE TABLE OF solisti1.
  DATA: objbin_ann  TYPE solisti1.
  DATA: objbin      TYPE TABLE OF solisti1.
  DATA: wa_objbin   TYPE solisti1.
  DATA: content_hex TYPE STANDARD TABLE OF solix.
  DATA: objtxt      TYPE TABLE OF solisti1.
  DATA: reclist     TYPE TABLE OF somlreci1.
  DATA: lwa_reclist TYPE somlreci1.
  DATA: doc_chng    TYPE sodocchgi1.
  DATA: tab_lines   TYPE sy-tabix.

  DATA: lv_valor TYPE string.

  DATA: lv_title_email TYPE string.

  DATA: vl_nmdfe    TYPE string,
        vl_docnum   TYPE string,
        vl_filial   TYPE string,
        vl_data_aut TYPE string,
        vl_msg_ret  TYPE string.

  DEFINE conc_html.
    LV_VALOR = &1.

    CALL FUNCTION 'ZHTML_ADD'
      EXPORTING
        I_TEXTO = LV_VALOR
      TABLES
        IT_HTML = IT_HTML.
  END-OF-DEFINITION.


  CLEAR: reclist[].

  "Determinação Destinatarios
  CASE sy-sysid.
    WHEN 'PRD'.
      lwa_reclist-receiver = 'suporte.sap@amaggi.com.br'.
      lwa_reclist-rec_type = 'U'.
      APPEND lwa_reclist TO reclist.
    WHEN OTHERS.
      lwa_reclist-receiver = 'wellington.pereira@amaggi.com.br'.
      lwa_reclist-rec_type = 'U'.
      APPEND lwa_reclist TO reclist.
  ENDCASE.

  lv_title_email = 'ENVIO_DADOS_FUNCIONARIOS_LEGADOS'.


  vl_titulo = 'Houve falha de comunicação no Job de Envio Dados Funcionarios para Sistemas Legados! Verificar Log SM37 Programa: ZHCMR_PA0107!'.

  CONCATENATE vl_titulo ' Data:' sy-datum '- Hora: ' sy-uzeit INTO vl_titulo SEPARATED BY space.

  "Monta Corpo Email
  conc_html '<html>'.
  conc_html '<head><title>'.
  conc_html    lv_title_email.
  conc_html '</title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
  conc_html '<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.
  conc_html '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>'.
  conc_html    vl_titulo.
  conc_html '</STRONG></FONT></DIV><BR>'.
  conc_html '<FONT face=Verdana color=#0000ff size=2>'.
  conc_html '<BR>'.
  conc_html '<BR>'.
  conc_html '<BR>'.
  conc_html '<DIV align=left>'.
  conc_html '<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>E-mail gerado automáticamente pelo sistema</STRONG></FONT></DIV>'.

  conc_html '</DIV>'.
  conc_html '<BR>'.
  conc_html '</body>'.
  conc_html '</html>'.

  "Corpo
  doc_chng-obj_name  = lv_title_email.
  doc_chng-obj_descr = lv_title_email.
  doc_chng-no_change = 'X'.

  CLEAR lwa_objpack-transf_bin.
  lwa_objpack-head_start = 1.
  lwa_objpack-head_num = 0.
  lwa_objpack-body_start = 1.
  lwa_objpack-body_num = 99999.
  lwa_objpack-doc_type = 'HTM'.
  APPEND lwa_objpack TO objpack.

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

ENDFORM.

FORM f_carga_zhcmt0007 .

  "RANGES:
  "SELECT * FROM ZHCMT0007 INTO TABLE @DATA(LIT_ZHCMT0007).

  "CHECK LIT_ZHCMT0007[] IS NOT INITIAL.

  "SUBMIT ZHCMR_PA0012 WITH

ENDFORM.
