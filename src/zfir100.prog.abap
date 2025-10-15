*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: RODRIGO C.                                              &*
*& Data.....: 07/12/2023                                              &*
*& Descrição: Job Cobrança Pendencia Fornecedor.                      &*
*& Transação:                                                         &*
*&                                                                    &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zfir100.


*----------------------------------------------------------------------*
* Tabelas                                                          *
*----------------------------------------------------------------------*
TABLES: lfb1, bseg.


DATA: lr_list_fbl1n TYPE REF TO data.

FIELD-SYMBOLS: <lt_list_fbl1n> TYPE STANDARD TABLE,
               <ls_list_fbl1n> TYPE any,
               <lv_field>      TYPE any.

*----------------------------------------------------------------------*
* Types                                                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_line,
         line(3000),
       END OF ty_line,

       BEGIN OF ty_dados_fbl1n,
         gsber     TYPE gsber,
         shkzg     TYPE shkzg,
         bukrs     TYPE bukrs,
         lifnr     TYPE lifnr,
         name1     TYPE name1_gp,
         ebeln     TYPE ebeln,
         xblnr     TYPE xblnr1,
         "zuonr TYPE dzuonr,
         belnr     TYPE belnr_d,
         budat     TYPE char10,
         faedt     TYPE char10,
         waers     TYPE waers,
         bwwrt(20),
         bwwr2(20),
         blart     TYPE blart,
         bschl     TYPE bschl,
         zlspr     TYPE dzlspr,
         sgtxt     TYPE sgtxt,
         hkont     TYPE hkont,
         augbl     TYPE augbl,
         bsart     TYPE bsart,
       END OF ty_dados_fbl1n.


DATA: aux_bwwrt(20) TYPE c,
      aux_bwwr2(20) TYPE c.

*----------------------------------------------------------------------*
* Tabela interna                                                       *
*----------------------------------------------------------------------*
TYPE-POOLS: slis.

DATA: BEGIN OF ti_fieldcat OCCURS 1.
        INCLUDE TYPE slis_fieldcat_main.
        INCLUDE TYPE slis_fieldcat_alv_spec.
DATA: END OF ti_fieldcat.

DATA: gt_list_tab    TYPE TABLE OF abaplist,
      gt_asci        TYPE TABLE OF ty_line,
      gt_dados_fbl1n TYPE TABLE OF ty_dados_fbl1n,
      wa_dados_fbl1n TYPE ty_dados_fbl1n.

DATA: keyinfo        TYPE slis_keyinfo_alv.
DATA: ti_layout      TYPE slis_layout_alv.
DATA: color          TYPE slis_t_specialcol_alv WITH HEADER LINE.
DATA: gruppen        TYPE slis_t_sp_group_alv   WITH HEADER LINE.
DATA: variante       LIKE disvariant.
DATA: sort           TYPE STANDARD TABLE OF slis_sortinfo_alv WITH HEADER LINE.
DATA: gt_zmail TYPE STANDARD TABLE OF zmail INITIAL SIZE 0.
DATA: wa_zmail TYPE zmail.


DATA: gv_bukrs  TYPE rvari_val_255,
      gv_blart  TYPE rvari_val_255,
      gv_hkont  TYPE rvari_val_255,
      vl_format TYPE vtcur12.

DATA : it_contents TYPE STANDARD TABLE OF solisti1.
DATA : html        TYPE STANDARD TABLE OF solisti1 WITH HEADER LINE.
DATA nm_sendprocess TYPE string.
"DATA lt_mailrecipientes TYPE STANDARD TABLE OF somlrec90 WITH HEADER LINE.
DATA: get_bwwrt TYPE decfloat34.
DATA: get_bwwr2 TYPE decfloat34.
DATA: soma_bwwrt TYPE decfloat34.
DATA: soma_bwwr2 TYPE decfloat34.
DATA tot_bwwrt TYPE tex50.
DATA tot_bwwr2 TYPE tex50.
DATA bwwrt_amount TYPE zbapicurr_d2.
DATA bwwr2_amount TYPE zbapicurr_d2.

*----------------------------------------------------------------------*
* Ranges                                                       *
*----------------------------------------------------------------------*
DATA: rg_gsber TYPE RANGE OF bseg-gsber,
      wa_gsber LIKE LINE  OF rg_gsber,
      rg_bukrs TYPE RANGE OF lfb1-bukrs,
      wa_bukrs LIKE LINE  OF rg_bukrs,
      rg_blart TYPE RANGE OF blart,
      wa_blart LIKE LINE  OF rg_blart,
      rg_hkont TYPE RANGE OF hkont,
      wa_hkont LIKE LINE  OF rg_hkont.


DATA: lo_create_mail TYPE REF TO cl_crm_email_data,
      lt_mail_body   TYPE crmt_email_mime_struc,
      ls_mail_body   TYPE crms_email_mime_struc,
      lt_to          TYPE crmt_email_recipients,
      lt_copy        TYPE crmt_email_recipients,
      ls_recep       TYPE crms_email_recipient,
      t_html         TYPE string,
      lv_activity    TYPE sysuuid_x,
      vl_check_h     TYPE char01,
      vl_check_s     TYPE char01,
      vl_gsber       TYPE bseg-gsber,
      vl_chec_qb     TYPE char01,
      texto_corpo_s  TYPE string,
      texto_corpo_h  TYPE string,
      texto_corpo  TYPE string,
      text_s(1)      TYPE c,
      text_h(1)      TYPE c.

*----------------------------------------------------------------------*
* Tela de seleção                                                             *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_lifnr FOR lfb1-lifnr,
  s_bukrs FOR lfb1-bukrs.

SELECTION-SCREEN END OF BLOCK b1.



*----------------------------------------------------------------------*
* START-OF-SELECTION                                                             *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  SELECT
  'I' AS sign,
  'EQ' AS option,
  bukrs AS low,
  bukrs AS high
  FROM t001
  WHERE bukrs IN @s_bukrs
  AND bukrs NOT IN ( SELECT low
  FROM tvarvc
  WHERE name EQ 'ZFIR0100_EMP_FORNECEDORES')
  INTO TABLE @rg_bukrs.

  IF rg_bukrs[] IS INITIAL.

    CALL SELECTION-SCREEN 1000.
    MESSAGE 'Existem empresas parametrizadas para não enviar e-mail!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.

  ELSE.
    SORT rg_bukrs BY low.

    cl_salv_bs_runtime_info=>set(
    EXPORTING
      display  = abap_true
      metadata = abap_true
    data     = abap_true ).

    SUBMIT rfitemap
    WITH kd_lifnr-low IN s_lifnr   "Conta do fornecedor
    WITH kd_bukrs-low IN rg_bukrs[] "Empresa
    WITH x_opsel      EQ 'X'      "Partidas em aberto
    WITH pa_stida     EQ sy-datum "Aberto a data fixada
    WITH x_norm       EQ 'X'      "Partidas normais
    EXPORTING LIST TO MEMORY AND RETURN.

    TRY.
        cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_list_fbl1n ).
        ASSIGN lr_list_fbl1n->* TO <lt_list_fbl1n>.

      CATCH cx_root.

    ENDTRY.
  ENDIF.


  IF sy-subrc EQ 0.

    "Monta dados fornecedores
    PERFORM monta_dados_fornecedores.

    "Envia email fornecedores diversos por filial
    PERFORM envia_email_fornecedores.

  ENDIF.

  IF gt_dados_fbl1n[] IS INITIAL.
    MESSAGE i000(z_fi) WITH 'Registros não encontrados'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  PERFORM campos_display_alv.

END-OF-SELECTION.

  PERFORM display_alv.
*&---------------------------------------------------------------------*
*& Form envia_email_fornecedores
*&---------------------------------------------------------------------*
FORM envia_email_fornecedores .

  IF NOT gt_dados_fbl1n[] IS INITIAL.

    SELECT werks, email
    FROM zmail
    INTO TABLE @DATA(gt_zmail)
          WHERE werks IN @rg_gsber
          AND   tcode EQ 'ZFI0151'.

    SORT gt_zmail BY werks.

    SORT gt_dados_fbl1n BY gsber shkzg.

***********************************************************************
**Get unidade
***********************************************************************
    LOOP AT gt_zmail ASSIGNING FIELD-SYMBOL(<get_werks>) GROUP BY <get_werks>-werks.

***********************************************************************
**Get dados unidade
***********************************************************************
      LOOP AT gt_dados_fbl1n ASSIGNING FIELD-SYMBOL(<get_dados>) WHERE gsber = <get_werks>-werks GROUP BY <get_dados>-gsber.
**********************************************************************
*Monta HTML Cabeçalho
**********************************************************************
        CLEAR: nm_sendprocess,html.
        FREE:html[].
        nm_sendprocess = |PENDENCIA VENCIDA - FORNECEDORES DIVERSOS - { <get_dados>-gsber }|.
        PERFORM inicia_html.

**********************************************************************
*Monta HTML Corpo H
**********************************************************************
        LOOP AT gt_dados_fbl1n ASSIGNING FIELD-SYMBOL(<get_filter_H>) WHERE gsber = <get_dados>-gsber AND shkzg = 'H'.
          "GROUP BY ( gsber = <get_filter_h>-gsber shkzg = <get_filter_h>-shkzg ).
          IF text_h = ''.
            CLEAR: texto_corpo_h,texto_corpo.
*            texto_corpo_h = |Favor retornar o e-mail ao contas.pagar@amaggi.com.br sobre a previsão de pagamentos das faturas vencidas listadas abaixo:|. "SMC 07-02-2025 CS2025000158
            texto_corpo_h = |Favor retornar o e-mail ao cscfinanceiro.fornecedor@amaggi.com.br sobre a previsão de pagamentos das faturas vencidas listadas abaixo:|. "SMC 07-02-2025 CS2025000158
            move texto_corpo_h to texto_corpo.
            text_h = 'X'.
            PERFORM monta_reader_tabela.
          ENDIF.

          APPEND '<tr>' TO html.
          CONCATENATE '<td>'<get_filter_H>-bukrs '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-lifnr '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-name1 '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-ebeln '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-gsber '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-xblnr '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-belnr '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-budat '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-faedt '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-waers '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-bwwrt '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-bwwr2 '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-blart '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-bschl '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-zlspr '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-shkzg '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-sgtxt '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_H>-hkont '</td>' INTO html.    APPEND html.
          "CONCATENATE '<td>'<get_filter_H>-bsart '</td>' INTO html.    APPEND html.
          APPEND '</tr>' TO html.

          aux_bwwrt = <get_filter_H>-bwwrt.
          aux_bwwr2 = <get_filter_H>-bwwr2.

          PERFORM prepra_soma.

        ENDLOOP.

        IF text_h = 'X'.
          PERFORM totalizadores_tabela.
          CLEAR: texto_corpo_h.
        ENDIF.

**********************************************************************
*Monta HTML Corpo S
**********************************************************************
        LOOP AT gt_dados_fbl1n ASSIGNING FIELD-SYMBOL(<get_filter_s>) WHERE gsber = <get_dados>-gsber AND shkzg = 'S'.
          IF text_s = ''.
            CLEAR: texto_corpo_s,texto_corpo.
            texto_corpo_s = |Favor retornar O e-mail ao contas.pagar@amaggi.com.br sobre a previsão de utilização dos créditos vencidos listados abaixo:|.
            move texto_corpo_s to texto_corpo.
            text_s = 'X'.
            PERFORM monta_reader_tabela.
          ENDIF.
          APPEND '<tr>' TO html.
          CONCATENATE '<td>'<get_filter_s>-bukrs '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-lifnr '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-name1 '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-ebeln '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-gsber '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-xblnr '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-belnr '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-budat '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-faedt '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-waers '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-bwwrt '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-bwwr2 '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-blart '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-bschl '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-zlspr '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-shkzg '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-sgtxt '</td>' INTO html.    APPEND html.
          CONCATENATE '<td>'<get_filter_s>-hkont '</td>' INTO html.    APPEND html.
          "CONCATENATE '<td>'<get_filter_s>-bsart '</td>' INTO html.    APPEND html.
          APPEND '</tr>' TO html.

          aux_bwwrt = <get_filter_s>-bwwrt.
          aux_bwwr2 = <get_filter_s>-bwwr2.

          PERFORM prepra_soma.

        ENDLOOP.

        IF text_s = 'X'.
          PERFORM totalizadores_tabela.
          CLEAR: texto_corpo_s.
        ENDIF.


**********************************************************************
*Monta HTML finaliza
**********************************************************************

        APPEND '</table>' TO html.
        APPEND '</body>' TO html.
        APPEND '</html>' TO html.

        APPEND LINES OF html TO it_contents.

        CLEAR:t_html,text_h,text_s.

        "Junta as linhas HTML para uma unica fonte
        LOOP AT it_contents ASSIGNING FIELD-SYMBOL(<create_htm>).
          CONCATENATE t_html <create_htm>  INTO t_html SEPARATED BY space.
        ENDLOOP.


***********************************************************************
**Get Receivers Mail
***********************************************************************
        CLEAR:lt_to,ls_recep.
        LOOP AT gt_zmail ASSIGNING FIELD-SYMBOL(<get_zmail>) WHERE werks = <get_dados>-gsber.
          IF sy-sysid = 'QAS'.
            ls_recep-address = 'samuel.cabana@amaggi.com.br'.
            APPEND ls_recep TO lt_to.
            ls_recep-address = 'larissa.pereira@amaggi.com.br'.
            APPEND ls_recep TO lt_to.
            CLEAR ls_recep.
          ELSE.
            ls_recep-address = <get_zmail>-email.
            APPEND ls_recep TO lt_to.
            CLEAR ls_recep.
          ENDIF.
        ENDLOOP.

        SORT lt_to.
        DELETE ADJACENT DUPLICATES FROM lt_to.

        CREATE OBJECT lo_create_mail.

        CONCATENATE 'PENDENCIA VENCIDA - FORNECEDORES DIVERSOS -' <get_dados>-gsber
        INTO lo_create_mail->subject SEPARATED BY space.

        CLEAR: ls_mail_body,lt_mail_body.
        ls_mail_body-content_ascii = t_html.
        ls_mail_body-mime_type     = 'text/html'.
        APPEND ls_mail_body TO lt_mail_body.

        MOVE lt_mail_body TO lo_create_mail->body.
        MOVE lt_to TO lo_create_mail->to.

        CLEAR ls_recep.
        ls_recep-address = ''.
        MOVE ls_recep TO lo_create_mail->from.

        CALL METHOD cl_crm_email_utility_base=>send_email
          EXPORTING
            iv_mail_data       = lo_create_mail
          RECEIVING
            ev_send_request_id = lv_activity.

        COMMIT WORK.

        CLEAR: t_html,lt_to,lo_create_mail,lt_mail_body,lv_activity,html,it_contents,it_contents[].
        FREE: t_html,lt_to,lo_create_mail,lt_mail_body,lv_activity,html[],it_contents,it_contents[].

      ENDLOOP.

    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form monta_dados_fornecedores
*&---------------------------------------------------------------------*
FORM monta_dados_fornecedores.


  "Seleciona nome de fornecedor
  SELECT lifnr, name1
  FROM lfa1
  INTO TABLE @DATA(gt_lfa1)
        WHERE lifnr IN @s_lifnr.

  SORT gt_lfa1 BY lifnr.


  LOOP AT <lt_list_fbl1n> ASSIGNING <ls_list_fbl1n>.

    ASSIGN COMPONENT 'BUKRS' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-bukrs = <lv_field>.
    ENDIF.

    ASSIGN COMPONENT 'BLART' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-blart = <lv_field>.
    ENDIF.

    ASSIGN COMPONENT 'HKONT' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-hkont = <lv_field>.
    ENDIF.

    ASSIGN COMPONENT 'AUGBL' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-augbl = <lv_field>.
    ENDIF.

    IF NOT wa_dados_fbl1n-augbl IS INITIAL.
      CONTINUE.
    ENDIF.

    ASSIGN COMPONENT 'GSBER' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-gsber = <lv_field>.
    ENDIF.

    wa_gsber-sign   = 'I'.
    wa_gsber-option = 'EQ'.
    wa_gsber-low    = wa_dados_fbl1n-gsber.
    COLLECT wa_gsber INTO rg_gsber.


    ASSIGN COMPONENT 'KONTO' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-lifnr = <lv_field>.
    ENDIF.

    READ TABLE gt_lfa1 INTO DATA(wa_lfa1) WITH KEY lifnr = wa_dados_fbl1n-lifnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_dados_fbl1n-name1 = wa_lfa1-name1.
    ENDIF.

    ASSIGN COMPONENT 'EBELN' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-ebeln = <lv_field>.
    ENDIF.

    ASSIGN COMPONENT 'XBLNR' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-xblnr = <lv_field>.
    ENDIF.

*    ASSIGN COMPONENT 'ZUONR' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
*    IF sy-subrc = 0.
*      wa_dados_fbl1n-zuonr = <lv_field>.
*    ENDIF.

    ASSIGN COMPONENT 'BELNR' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-belnr = <lv_field>.
    ENDIF.

    ASSIGN COMPONENT 'BUDAT' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-budat = <lv_field>.
      CONCATENATE
      wa_dados_fbl1n-budat+06(02) '.' wa_dados_fbl1n-budat+04(02) '.' wa_dados_fbl1n-budat(04) INTO wa_dados_fbl1n-budat.
    ENDIF.

    ASSIGN COMPONENT 'FAEDT' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      IF <lv_field> IS NOT INITIAL.
        DATA: dttemp TYPE dats.
        dttemp = <lv_field>.
        IF dttemp+0(6) < sy-datum+0(6).
          wa_dados_fbl1n-faedt = <lv_field>.
          CONCATENATE
          wa_dados_fbl1n-faedt+06(02) '.' wa_dados_fbl1n-faedt+04(02) '.' wa_dados_fbl1n-faedt(04) INTO wa_dados_fbl1n-faedt.
        ENDIF.
      ENDIF.

    ENDIF.

    ASSIGN COMPONENT 'WAERS' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-waers = <lv_field>.
    ENDIF.

    ASSIGN COMPONENT 'BWWRT' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-bwwrt = <lv_field>.
      vl_format = wa_dados_fbl1n-bwwrt.
      WRITE vl_format TO wa_dados_fbl1n-bwwrt.
      CONDENSE wa_dados_fbl1n-bwwrt.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = wa_dados_fbl1n-bwwrt.
    ENDIF.

    ASSIGN COMPONENT 'BWWR2' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-bwwr2 = <lv_field>.
      vl_format = wa_dados_fbl1n-bwwr2.
      WRITE vl_format TO wa_dados_fbl1n-bwwr2.
      CONDENSE wa_dados_fbl1n-bwwr2.
      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
        CHANGING
          value = wa_dados_fbl1n-bwwr2.
    ENDIF.

    ASSIGN COMPONENT 'BSCHL' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-bschl = <lv_field>.
    ENDIF.

    ASSIGN COMPONENT 'SHKZG' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-shkzg = <lv_field>.
    ENDIF.

    ASSIGN COMPONENT 'ZLSPR' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-zlspr = <lv_field>.
    ENDIF.

    ASSIGN COMPONENT 'SGTXT' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-sgtxt = <lv_field>.
    ENDIF.

    ASSIGN COMPONENT 'ZZ_BSART' OF STRUCTURE <ls_list_fbl1n> TO <lv_field>.
    IF sy-subrc = 0.
      wa_dados_fbl1n-bsart = <lv_field>.
    ENDIF.

    APPEND wa_dados_fbl1n TO gt_dados_fbl1n.
    CLEAR wa_dados_fbl1n.

  ENDLOOP.

  "Tipos de documento que serão descartados do envio do e-mail.
  SELECT low
  FROM tvarvc
  INTO TABLE @DATA(gt_tp_doc_fornecedores)
        WHERE name EQ 'ZFIR0100_TP_DOC_FORNECEDORES'.

  "conta razão que deverão ser enviados por e-mail.
  SELECT low
  FROM tvarvc
  INTO TABLE @DATA(gt_razao_fornecedores)
        WHERE name EQ 'ZFIR0100_RAZAO_FORNECEDORES'.

  SORT gt_tp_doc_fornecedores BY low.
  SORT gt_razao_fornecedores  BY low.

  wa_blart-sign   = 'I'.
  wa_blart-option = 'EQ'.
  LOOP AT gt_tp_doc_fornecedores INTO DATA(wa_tp_doc_fornecedores).
    wa_blart-low    = wa_tp_doc_fornecedores-low.
    APPEND wa_blart TO rg_blart.
  ENDLOOP.

  wa_hkont-sign   = 'I'.
  wa_hkont-option = 'EQ'.
  LOOP AT gt_razao_fornecedores INTO DATA(wa_razao_fornecedores).
    wa_hkont-low    = wa_razao_fornecedores-low.
    APPEND wa_hkont TO rg_hkont.
  ENDLOOP.

  DELETE gt_dados_fbl1n WHERE blart IN rg_blart.

  DELETE gt_dados_fbl1n WHERE hkont NOT IN rg_hkont.

  DELETE gt_dados_fbl1n WHERE faedt = ''.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_alv
*&---------------------------------------------------------------------*
FORM display_alv .

  ti_layout-zebra  = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      is_layout              = ti_layout
      it_fieldcat            = ti_fieldcat[]
      i_save                 = 'X'
    TABLES
      t_outtab               = gt_dados_fbl1n
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.


  DATA it_header TYPE  slis_t_listheader.
  DATA st_header TYPE  slis_listheader.

  DATA: lv_name1 TYPE name1,
        lv_name2 TYPE name1.

  st_header-typ  = 'H'.
  st_header-info = 'Cobrança Pendencia de Fornecedor'.
  APPEND st_header TO it_header.


  st_header-typ = 'S'.
  CONCATENATE 'Data de Geração: ' '  ' sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4)
  INTO st_header-info SEPARATED BY space.
  APPEND st_header TO it_header.


  st_header-typ = 'S'.
  CONCATENATE 'Hora de Geração: ' '  ' sy-uzeit(2) ':' sy-uzeit+02(2) ':' sy-uzeit+04(2)
  INTO st_header-info SEPARATED BY space.
  APPEND st_header TO it_header.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_header.



ENDFORM.                    "TOP_OF_PAGE
*&---------------------------------------------------------------------*
*& Form campos_display_alv
*&---------------------------------------------------------------------*
FORM campos_display_alv .

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'BUKRS'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Empresa'.
  ti_fieldcat-key          = 'X'.
  ti_fieldcat-col_pos      = '1'.
  ti_fieldcat-outputlen    = '10'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'LIFNR'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Fornecedor'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '2'.
  ti_fieldcat-outputlen    = '10'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'NAME1'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Nome'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '3'.
  ti_fieldcat-outputlen    = '30'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'EBELN'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Pedido'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '4'.
  ti_fieldcat-outputlen    = '10'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'XBLNR'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Referência'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '5'.
  ti_fieldcat-outputlen    = '15'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'BELNR'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'N. documento'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '6'.
  ti_fieldcat-outputlen    = '15'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'BUDAT'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Data de lançamento'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '7'.
  ti_fieldcat-outputlen    = '18'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'FAEDT'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Data vencimento'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '8'.
  ti_fieldcat-outputlen    = '18'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'GSBER'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Divisão'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '9'.
  ti_fieldcat-outputlen    = '8'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'SHKZG'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'débito/crédito'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '10'.
  ti_fieldcat-outputlen    = '15'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'WAERS'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Moeda'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '11'.
  ti_fieldcat-outputlen    = '10'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'BWWRT'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Montante MI'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '12'.
  ti_fieldcat-outputlen    = '15'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'BWWR2'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Montante MI2'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '13'.
  ti_fieldcat-outputlen    = '15'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'BLART'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Tp. documento'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '14'.
  ti_fieldcat-outputlen    = '15'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'BSCHL'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'CL'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '15'.
  ti_fieldcat-outputlen    = '06'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'ZLSPR'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Chave BP'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '16'.
  ti_fieldcat-outputlen    = '08'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'SGTXT'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Texto'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '17'.
  ti_fieldcat-outputlen    = '30'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'HKONT'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Conta razão'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '18'.
  ti_fieldcat-outputlen    = '10'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'AUGBL'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Doc. compensação'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '19'.
  ti_fieldcat-outputlen    = '15'.
  APPEND ti_fieldcat.

  CLEAR ti_fieldcat.
  ti_fieldcat-fieldname    = 'BSART'.
  ti_fieldcat-tabname      = 'gt_dados_fbl1n'.
  ti_fieldcat-reptext_ddic = 'Tipo de pedido'.
  ti_fieldcat-key          = ''.
  ti_fieldcat-col_pos      = '20'.
  ti_fieldcat-outputlen    = '15'.
  APPEND ti_fieldcat.

ENDFORM.

*FORM send_email.
*
*  CREATE OBJECT lo_create_mail.
*
*  LOOP AT gt_zmail INTO DATA(wa_zmail) WHERE werks = vl_gsber.
*
*    vl_check_h = abap_false.
*    vl_check_s = abap_false.
*
*    CONCATENATE 'PENDENCIA VENCIDA - FORNECEDORES DIVERSOS -' vl_gsber
*    INTO lo_create_mail->subject SEPARATED BY space.
*
*    CLEAR ls_mail_body.
*    ls_mail_body-content_ascii = t_html.
*    ls_mail_body-mime_type     = 'text/html'.
*    APPEND ls_mail_body TO lt_mail_body.
*
*    MOVE lt_mail_body TO lo_create_mail->body.
*
*    CLEAR ls_recep.
*    IF sy-sysid EQ 'QAS'.
*      ls_recep-address = 'samuel.cabana@amaggi.com.br'.
*      APPEND ls_recep TO lt_to.
*      ls_recep-address = 'amaury.silva@amaggi.com.br'.
*      APPEND ls_recep TO lt_to.
*    ELSE.
*      ls_recep-address = wa_zmail-email.
*
*      APPEND ls_recep TO lt_to.
*    ENDIF.
*
*    CLEAR ls_recep.
*  ENDLOOP.
*
*  MOVE lt_to TO lo_create_mail->to.
*
**        IF sy-sysid EQ 'QAS'.
**          ls_recep-address = 'amaury.silva@amaggi.com.br'.
**        ELSE.
**          ls_recep-address = wa_zmail-email.
**        ENDIF.
**        APPEND ls_recep TO lt_copy.
**        MOVE lt_copy TO lo_create_mail->copy.
*
*  CLEAR ls_recep.
*  ls_recep-address = ''.
*  MOVE ls_recep TO lo_create_mail->from.
*
*  CALL METHOD cl_crm_email_utility_base=>send_email
*    EXPORTING
*      iv_mail_data       = lo_create_mail
*    RECEIVING
*      ev_send_request_id = lv_activity.
*
*  COMMIT WORK.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form html_h
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM html_h .
*  vl_check_h = abap_true.
*  CONCATENATE t_html '<html>'  INTO t_html.
*  CONCATENATE t_html '<head>'  INTO t_html.
*  CONCATENATE t_html '</head>' INTO t_html.
*  CONCATENATE t_html '<body>'  INTO t_html.
*  CONCATENATE t_html '<div align=left><font face=Verdana size=3>Favor retornar o e-mail ao contas.pagar@amaggi.com.br sobre a previsão de pagamentos das faturas vencidas listadas abaixo:</font></div>' INTO t_html.
*  CONCATENATE t_html '<br><br>'                                 INTO t_html.
*  CONCATENATE t_html '<align=left>&nbsp;</div>'                 INTO t_html.
*  CONCATENATE t_html '<table border=1 align=left width=100% size=2>'              INTO t_html.
*  "CONCATENATE t_html '<tr><font face=Verdana size=1><strong>'   INTO t_html.
*
*  CONCATENATE t_html '<td align=center width=5%>Empresa</td>'              INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Conta</td>'                INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Fornecedor</td>'           INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Doc.compra</td>'           INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Divisão</td>'              INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Referência</td>'           INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Nº documento</td>'         INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Data Lançamento</td>'      INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Vencimento Liquido</td>'   INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Moeda</td>'                INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Montante em MI (R$)</td>'  INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Montante em MI (USD)</td>' INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Tipo Doc</td>'             INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>CL</td>'                   INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Bloq Pgto</td>'            INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Cod D/C</td>'              INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Texto</td>'                INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Razão</td>'                INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Tipo de Pedido</td>'       INTO t_html.
*  CONCATENATE t_html '</td>' INTO t_html.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form html_s
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM html_s .
*  vl_check_s = abap_true.
*  CONCATENATE t_html '<html>'  INTO t_html.
*  CONCATENATE t_html '<head>'  INTO t_html.
*  CONCATENATE t_html '</head>' INTO t_html.
*  CONCATENATE t_html '<body>'  INTO t_html.
*  CONCATENATE t_html '<div align=left><font face=Verdana size=3>Favor retornar o e-mail ao contas.pagar@amaggi.com.br sobre a previsão de utilização dos créditos vencidos listados abaixo:</font></div>' INTO t_html.
*  CONCATENATE t_html '<br><br>'                                 INTO t_html.
*  CONCATENATE t_html '<align=left>&nbsp;</div>'                 INTO t_html.
*  CONCATENATE t_html '<table border=1 align=left width=100%>'              INTO t_html.
*  "CONCATENATE t_html '<tr><font face=Verdana size=1><strong>'   INTO t_html.
*
*  CONCATENATE t_html '<td align=center width=5%>Empresa</td>'              INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Conta</td>'                INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Fornecedor</td>'           INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Doc.compra</td>'           INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Divisão</td>'              INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Referência</td>'           INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Nº documento</td>'         INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Data Lançamento</td>'      INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Vencimento Liquido</td>'   INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Moeda</td>'                INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Montante em MI (R$)</td>'  INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Montante em MI (USD)</td>' INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Tipo Doc</td>'             INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>CL</td>'                   INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Bloq Pgto</td>'            INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Cod D/C</td>'              INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Texto</td>'                INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Razão</td>'                INTO t_html.
*  CONCATENATE t_html '<td align=center width=5%>Tipo de Pedido</td>'       INTO t_html.
*  CONCATENATE t_html '</td>' INTO t_html.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form html_continue
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM html_continue .
*  CONCATENATE t_html '<tr><font face=Verdana>' INTO t_html.
*
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-bukrs '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-lifnr '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-name1 '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-ebeln '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-gsber '</td>' INTO t_html.
**      CONCATENATE t_html '<td align=left>'  wa_dados_fbl1n-zuonr '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>'  wa_dados_fbl1n-xblnr '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-belnr '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-budat '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-faedt '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-waers '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-bwwrt '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-bwwr2 '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-blart '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-bschl '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-zlspr '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-shkzg '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-sgtxt '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-hkont '</td>' INTO t_html.
*  CONCATENATE t_html '<td align=left>' wa_dados_fbl1n-bsart '</td>' INTO t_html.
*
*  REPLACE ALL OCCURRENCES OF '.' IN wa_dados_fbl1n-bwwrT WITH ''.
*  REPLACE ALL OCCURRENCES OF '.' IN wa_dados_fbl1n-bwwr2 WITH ''.
*
*  REPLACE ALL OCCURRENCES OF ',' IN wa_dados_fbl1n-bwwrT WITH '.'.
*  REPLACE ALL OCCURRENCES OF ',' IN wa_dados_fbl1n-bwwr2 WITH '.'.
*
*  get_vl_bwwrt = wa_dados_fbl1n-bwwrT.
*  aux_vl_bwwr2 = wa_dados_fbl1n-bwwr2.
*  aux_vl_bwwrt = get_vl_bwwrt + aux_vl_bwwrt.
*  aux_vl_bwwr2 = get_vl_bwwr2 + aux_vl_bwwr2.
*
*  DATA bwwrt_amount TYPE bapicurr_d.
*  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
*    EXPORTING
*      currency        = 'BRL'
*      amount_internal = aux_vl_bwwrT
*    IMPORTING
*      amount_external = bwwrT_amount.
*
*  DATA bwwr2_amount TYPE bapicurr_d.
*  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
*    EXPORTING
*      currency        = 'BRL'
*      amount_internal = aux_vl_bwwr2
*    IMPORTING
*      amount_external = bwwr2_amount.
*
*  vl_bwwrt = bwwrT_amount."SMC COMENTADO EM 06-03-24 (DAVA DUMP)
*  vl_bwwr2 = bwwr2_amount. "SMC COMENTADO EM 06-03-24  (DAVA DUMP)
*
*  IF bwwrT_amount < 0.
*    REPLACE ALL OCCURRENCES OF '-' IN vl_bwwrt WITH ''.
*    CONDENSE vl_bwwrT NO-GAPS.
*    vl_bwwrt = |-{ vl_bwwrt }|.
*  ELSE.
*
*  ENDIF.
*
*  IF bwwr2_amount < 0.
*    REPLACE ALL OCCURRENCES OF '-' IN vl_bwwr2 WITH ''.
*    CONDENSE vl_bwwr2 NO-GAPS.
*    vl_bwwr2 = |-{ vl_bwwr2 }|.
*  ELSE.
*
*  ENDIF.
*
*  vl_gsber = wa_dados_fbl1n-gsber.
*
*  AT END OF shkzg.
*    vl_chec_qb = abap_true.
*    CONCATENATE t_html '<tr><font face=Verdana size=1>' INTO t_html.
*
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>'  '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*      CHANGING
*        value = vl_bwwrt.
*    CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*      CHANGING
*        value = vl_bwwr2.
*    CONCATENATE t_html '<td align=left>' vl_bwwrt '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' vl_bwwr2 '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*
*    CONCATENATE t_html '</table>' INTO t_html.
*    CONCATENATE t_html '</body>'  INTO t_html.
*    CONCATENATE t_html '</html>'  INTO t_html.
*    CLEAR: vl_bwwrt, vl_bwwr2.
*  ENDAT.
*
*  AT END OF gsber.
*
*    IF vl_chec_qb EQ abap_false.
*
*      CONCATENATE t_html '<tr><font face=Verdana size=1>' INTO t_html.
*
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>'  '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*        CHANGING
*          value = vl_bwwrt.
*      CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
*        CHANGING
*          value = vl_bwwr2.
*      CONCATENATE t_html '<td align=left>' vl_bwwrt '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' vl_bwwr2 '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*      CONCATENATE t_html '<td align=left>' '' '</td>' INTO t_html.
*    ENDIF.
*    CONCATENATE t_html '</table>' INTO t_html.
*    CONCATENATE t_html '</body>'  INTO t_html.
*    CONCATENATE t_html '</html>'  INTO t_html.
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form monta_reader_tabela
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM monta_reader_tabela .
  APPEND '<strong  style="color: #000; font-size: 14px, font-weight: bold, margin-buttom: 11px,' TO html.
  APPEND ' font-family: Calibri" > ' TO html.
  APPEND '<table style="width:0%">' TO html.
  APPEND '<tr bgcolor="#E0ECF8" style="color: #000; font-Size: 14px" align="center">'TO html.
  APPEND '<h3><strong>'&& texto_corpo &&'</h3>' TO html.
  APPEND '<tr>'TO html.
  APPEND '<th><strong>Empresa               </strong></th>' TO html.
  APPEND '<th><strong>Conta                 </strong></th>' TO html.
  APPEND '<th><strong>Fornecedor            </strong></th>' TO html.
  APPEND '<th><strong>Doc.compra            </strong></th>' TO html.
  APPEND '<th><strong>Divisão               </strong></th>' TO html.
  APPEND '<th><strong>Referência            </strong></th>' TO html.
  APPEND '<th><strong>Nº documento          </strong></th>' TO html.
  APPEND '<th><strong>Data Lançamento       </strong></th>' TO html.
  APPEND '<th><strong>Vencimento Liquido    </strong></th>' TO html.
  APPEND '<th><strong>Moeda                 </strong></th>' TO html.
  APPEND '<th><strong>Montante em MI (R$)   </strong></th>' TO html.
  APPEND '<th><strong>Montante em MI (USD)  </strong></th>' TO html.
  APPEND '<th><strong>Tipo Doc              </strong></th>' TO html.
  APPEND '<th><strong>CL                    </strong></th>' TO html.
  APPEND '<th><strong>Bloq Pgto             </strong></th>' TO html.
  APPEND '<th><strong>Cod D/C               </strong></th>' TO html.
  APPEND '<th><strong>Texto                 </strong></th>' TO html.
  APPEND '<th><strong>Razão                 </strong></th>' TO html.
  "APPEND '<th><strong>Tipo de Pedido        </strong></th>' TO html.
  APPEND '</tr>' TO html.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form totalizadores_tabela
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM totalizadores_tabela .
  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = 'BRL'
      amount_internal = soma_bwwrt
    IMPORTING
      amount_external = bwwrt_amount.

  WRITE bwwrt_amount TO tot_bwwrt.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = 'BRL'
      amount_internal = soma_bwwr2
    IMPORTING
      amount_external = bwwr2_amount.

  WRITE bwwr2_amount TO tot_bwwr2.

  IF bwwrT_amount < 0.
    REPLACE ALL OCCURRENCES OF '-' IN tot_bwwrt WITH ''.
    CONDENSE tot_bwwrt NO-GAPS.
    tot_bwwrt = |-{ tot_bwwrt }|.
  ELSE.

  ENDIF.

  IF bwwr2_amount < 0.
    REPLACE ALL OCCURRENCES OF '-' IN tot_bwwr2 WITH ''.
    CONDENSE tot_bwwr2 NO-GAPS.
    tot_bwwr2 = |-{ tot_bwwr2 }|.
  ELSE.

  ENDIF.


  APPEND '<tr>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  CONCATENATE '<td>'tot_bwwrt'</td>' INTO html.    APPEND html.
  CONCATENATE '<td>'tot_bwwr2'</td>' INTO html.    APPEND html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  APPEND '<td></td>' TO html.
  "APPEND '<td></td>' TO html.
  APPEND '</tr>' TO html.
  APPEND '<br>' TO html.

  CLEAR: get_bwwrt, get_bwwr2, soma_bwwrt,soma_bwwr2,tot_bwwrt,tot_bwwr2,bwwrt_amount,bwwr2_amount,aux_bwwr2,aux_bwwrt.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form inicia_html
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM inicia_html .
  CLEAR: html.
  FREE: html[].
  APPEND '<!doctype html>' TO html.
  APPEND '<html>' TO html.
  APPEND '<head><meta charset="UTF-8" /><title>Modelo HTML E-mail</title></head>' TO html.
  APPEND '<style>' TO html.
  APPEND 'table { border-collapse: collapse;border: 2px solid rgb(140 140 140);' TO html.
  APPEND 'font-family: sans-serif;font-size: 0.8rem;letter-spacing: 1px; }' TO html.
  APPEND 'caption { caption-side: bottom;padding: 10px;font-weight: bold; } ' TO html.
  APPEND 'thead,tfoot { background-color: rgb(228 240 245); } ' TO html.
  APPEND 'th,td { border: 1px solid rgb(160 160 160);padding: 8px 10px; } ' TO html.
  APPEND 'td:last-of-type { text-align: center; }' TO html.
  APPEND 'tbody > tr:nth-of-type(even) { background-color: rgb(237 238 242); } ' TO html.
  APPEND 'tfoot th { text-align: right; } ' TO html.
  APPEND 'tfoot td { font-weight: bold; } ' TO html.
  APPEND '</style>' TO html.
  APPEND '<body>' TO html.
  APPEND '<h1 class="title">'&& nm_sendprocess &&'</h1>' TO html.
  APPEND '<h4>Senhores(as),</h4>' TO html.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form prepra_soma
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM prepra_soma .

  REPLACE ALL OCCURRENCES OF '.' IN aux_bwwrt WITH ''.
  REPLACE ALL OCCURRENCES OF '.' IN aux_bwwr2 WITH ''.

  REPLACE ALL OCCURRENCES OF ',' IN aux_bwwrt WITH '.'.
  REPLACE ALL OCCURRENCES OF ',' IN aux_bwwr2 WITH '.'.

  get_bwwrt = aux_bwwrt.
  get_bwwr2 = aux_bwwr2.

  soma_bwwrt =  get_bwwrt + soma_bwwrt.
  soma_bwwr2 =  get_bwwr2 + soma_bwwr2.
ENDFORM.
