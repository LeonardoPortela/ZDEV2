*&---------------------------------------------------------------------*
*& Report ZALV_TEST_RGA
*&---------------------------------------------------------------------*
*&ABAP: Renato Garcia (Wayon Consultoria)
*Data: 02.06.2025
**146630- CS2024000604 Isenção de Juros Insumos - Parte 1
*&---------------------------------------------------------------------*
REPORT zfir0125.

INCLUDE zfir0125_top.

INCLUDE zfir0125_pbo_0100.
INCLUDE zfir0125_pai_0100.
INCLUDE zfir0125_pbo_0200.
INCLUDE zfir0125_pai_0200.
INCLUDE zfir0125_pbo_0300.
INCLUDE zfir0125_pai_0300.
INCLUDE zfir0125_pbo_0400.
INCLUDE zfir0125_pai_0400.

*----------------------------------------------------------------------*
* Class Definition
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    METHODS: handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

    CLASS-METHODS:
      on_click FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.


    CLASS-METHODS:
      on_click2 FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.

DATA: go_event_handler TYPE REF TO lcl_event_handler.
*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  CALL SCREEN 100.

*----------------------------------------------------------------------*
* Subroutines
*----------------------------------------------------------------------*
FORM create_alv.
  " Create custom container
  CREATE OBJECT go_container
    EXPORTING
      container_name = 'ALV_CONTAINER'.

  " Create ALV Grid
  CREATE OBJECT go_alv
    EXPORTING
      i_parent = go_container.

  " Create event handler
  CREATE OBJECT go_event_handler.

  " Register events
  SET HANDLER go_event_handler->handle_toolbar FOR go_alv.
  SET HANDLER go_event_handler->handle_user_command FOR go_alv.
  SET HANDLER go_event_handler->handle_double_click FOR go_alv.
  SET HANDLER go_event_handler->on_click FOR go_alv.

ENDFORM.


FORM get_data.

  FREE: gt_output, gs_output, gt_ordens_aux.

  DATA: v_msg     TYPE char50,
        lt_ordens TYPE TABLE OF zsd_ord_vendas_est_isen_juros,
        lt_estra  TYPE TABLE OF zsd_estrategia_ov,
        lt_itens  TYPE TABLE OF zsd_itens_ov_est_isen_jur.

  DATA lr_solic_isen TYPE RANGE OF zfit186-cd_sol_isen.
  DATA lr_ov_princ   TYPE RANGE OF zfit186-ov_principal.


  DATA: lt_idd07v TYPE TABLE OF  dd07v.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZDOFI_TP_NEG'   "<-- Your Domain Here
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = lt_idd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  SELECT *
    FROM zsdt0060
    INTO TABLE @DATA(lt_sdt0060)
    WHERE usnam  EQ @sy-uname
    AND   programa EQ @sy-cprog.
  IF sy-subrc EQ 0.
    DATA(lr_user_param) = VALUE rsdsselopt_t(
        FOR ls_sdt0060 IN lt_sdt0060
      ( sign = 'I' option = 'EQ' low = ls_sdt0060-vkbur ) ).
  ELSE.
    RETURN.
  ENDIF.


  IF p_sol_isen IS NOT INITIAL.
    lr_solic_isen = VALUE #( ( sign = 'I' option = 'EQ' low = p_sol_isen ) ).
  ENDIF.

  IF p_ov_princ IS NOT INITIAL.
    lr_ov_princ = VALUE #( ( sign = 'I' option = 'EQ' low = p_ov_princ ) ).
  ENDIF.

  SELECT z~cd_sol_isen,
         z~ov_principal AS ov_principal,
         z~data_venc,
         z~moeda,
         z~vl_moeda_doc,
         z~tipo_negocio,
         z~usuario_solicit,
         z~data,
         z~hora,
         z~justificativa,
         z~justificativa_ger,
         z~status_solicit,
         v~vkorg,
         v~vkbur
    FROM zfit186 AS z
    LEFT OUTER JOIN vbak AS v ON z~ov_principal = v~vbeln
    INTO TABLE @DATA(lt_fi186)
    WHERE z~status_solicit = @c_aguard_lib
      AND z~cd_sol_isen IN @lr_solic_isen
      AND z~ov_principal IN @lr_ov_princ.

  IF sy-subrc EQ 0.


    gt_output = VALUE #( FOR ls_fi186 IN lt_fi186
                       ( CORRESPONDING #( ls_fi186 )

                          ) ).

    gt_zfit186 = VALUE #( FOR ls_fi186 IN lt_fi186
                       ( CORRESPONDING #( ls_fi186 )

                          ) ).
    "busca ordens relacionadas

    FREE: tg_ordens, it_estra, it_itens, gt_ordens_aux, gt_estra_aux, gt_itens_aux.

    CALL FUNCTION 'Z_OV_ESTRATEGIA_LISTA_ISEN_JUR'
      EXPORTING
        i_usuario = sy-uname
      IMPORTING
        e_msg     = v_msg
      TABLES
        t_ordens  = lt_ordens
        t_estra   = lt_estra
        t_itens   = lt_itens
        t_026     = tg_026.

    APPEND LINES OF lt_ordens TO gt_ordens_aux.
    APPEND LINES OF lt_estra  TO gt_estra_aux.
    APPEND LINES OF lt_itens  TO gt_itens_aux.

    LOOP AT gt_output ASSIGNING FIELD-SYMBOL(<ls_output>).

      SELECT SINGLE ukurs
       FROM zi_est_tcurr
       INTO @DATA(lv_ptax)
       WHERE kurst EQ 'G'
       AND fcurr   EQ @<ls_output>-moeda
       AND tcurr   EQ 'BRL'
       AND gdatu   EQ @sy-datum.
      IF sy-subrc EQ 0.
        <ls_output>-ptax = lv_ptax.
      ENDIF.

      IF <ls_output>-moeda NE 'BRL'.
        <ls_output>-vl_isenc_brl = <ls_output>-vl_moeda_doc * <ls_output>-ptax.
      ELSE.
        <ls_output>-vl_isenc_brl = <ls_output>-vl_moeda_doc.
      ENDIF.


      READ TABLE lt_idd07v INTO DATA(ls_idd07v) WITH KEY domvalue_l = <ls_output>-tipo_negocio.
      IF sy-subrc EQ 0.

        <ls_output>-tipo_negocio2 = ls_idd07v-ddtext.

      ENDIF.


      READ TABLE lt_itens WITH KEY ov_principal = <ls_output>-ov_principal INTO DATA(ls_itens).
      IF sy-subrc EQ  0.
        <ls_output>-simulador = ls_itens-simul_venda.
      ENDIF.

      <ls_output>-justificativa2 = icon_display_note.
      <ls_output>-justificativa3 = icon_display_note.

    ENDLOOP.

    IF lr_user_param IS NOT INITIAL.
      DELETE gt_output WHERE vkbur NOT IN lr_user_param.
    ENDIF.

    IF p_simula IS NOT INITIAL.
      DELETE gt_output WHERE vkbur NE p_simula.
    ENDIF.


  ENDIF.

ENDFORM.

FORM check_filters USING ps_data TYPE ty_output.

  DATA: lv_include TYPE abap_bool VALUE abap_true.

*  " Check Material filter
*  IF lr_matnr[] IS NOT INITIAL.
*    IF ps_data-material NOT IN lr_matnr.
*      lv_include = abap_false.
*    ENDIF.
*  ENDIF.

ENDFORM.

FORM display_alv.

  DATA: lt_fieldcat TYPE lvc_t_fcat,
        ls_fieldcat TYPE lvc_s_fcat,
        ls_layout   TYPE lvc_s_layo.

  " Build field catalog
  CLEAR: lt_fieldcat[], ls_fieldcat.

  " Nº solicitação isenção
  ls_fieldcat-fieldname = 'CD_SOL_ISEN'.
  ls_fieldcat-scrtext_s = 'Nº Sol.Isen.'.
  ls_fieldcat-scrtext_m = 'Nº Solicit. Isenção'.
  ls_fieldcat-scrtext_l = 'Nº Solicit. Isenção'.
  ls_fieldcat-outputlen = 20.
  APPEND ls_fieldcat TO lt_fieldcat.

  "Tipo Negócio
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TIPO_NEGOCIO2'.
  ls_fieldcat-scrtext_s = 'Tp.Negócio'.
  ls_fieldcat-scrtext_m = 'Tipo Negócio'.
  ls_fieldcat-scrtext_l = 'Tipo Negócio'.
  ls_fieldcat-outputlen = 20.
  APPEND ls_fieldcat TO lt_fieldcat.

  "simulador solic. OV
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SIMULADOR'.
  ls_fieldcat-scrtext_s = 'Sim.Solic.OV'.
  ls_fieldcat-scrtext_m = 'Simulador/Solic.OV'.
  ls_fieldcat-scrtext_l = 'Simulador/Solic.OV'.
  ls_fieldcat-outputlen = 20.
  APPEND ls_fieldcat TO lt_fieldcat.

  "ov. principal
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'OV_PRINCIPAL'.
  ls_fieldcat-scrtext_s = 'Ov.Principal'.
  ls_fieldcat-scrtext_m = 'Ov.Principal'.
  ls_fieldcat-scrtext_l = 'Ov.Principal'.
  ls_fieldcat-outputlen = 20.
  APPEND ls_fieldcat TO lt_fieldcat.

  "organização de vendas
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VKORG'.
  ls_fieldcat-scrtext_s = 'Org.Vendas'.
  ls_fieldcat-scrtext_m =  'Org.Vendas'.
  ls_fieldcat-scrtext_l =  'Org.Vendas'.
  ls_fieldcat-outputlen = 20.
  APPEND ls_fieldcat TO lt_fieldcat.

  "escritório de vendas
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'VKBUR'.
  ls_fieldcat-scrtext_s = 'Esc.Vendas'.
  ls_fieldcat-scrtext_m = 'Esc.Vendas'.
  ls_fieldcat-scrtext_l = 'Esc.Vendas'.
  ls_fieldcat-outputlen = 20.
  APPEND ls_fieldcat TO lt_fieldcat.

  "Moeda Doc.
  ls_fieldcat-fieldname = 'MOEDA'.
  ls_fieldcat-scrtext_s = 'Moeda Doc.'.
  ls_fieldcat-scrtext_m = 'Moeda Doc.'.
  ls_fieldcat-scrtext_l = 'Moeda Doc'.
  ls_fieldcat-outputlen = 10.
  APPEND ls_fieldcat TO lt_fieldcat.


  CLEAR ls_fieldcat.
  " Valor Moeda Documento
  ls_fieldcat-fieldname = 'VL_MOEDA_DOC'.
  ls_fieldcat-scrtext_m = 'Vlr.Is.Jur. Moeda doc.'.
  ls_fieldcat-scrtext_l = 'Vlr.Isenç.Juros Moeda Doc'.
  ls_fieldcat-outputlen = 15.
  ls_fieldcat-datatype = 'CURR'.
  ls_fieldcat-do_sum = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.


  "Ptax
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PTAX'.
  ls_fieldcat-scrtext_s = 'Ptax'.
  ls_fieldcat-scrtext_m = 'Ptax'.
  ls_fieldcat-scrtext_l = 'Ptax'.
  ls_fieldcat-outputlen = 15.
  ls_fieldcat-datatype = 'CURR'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.


  "Valor isenção juros brl
  ls_fieldcat-fieldname = 'VL_ISENC_BRL'.
  ls_fieldcat-scrtext_m = 'Vlr.Is.Jur. BRL.'.
  ls_fieldcat-scrtext_l = 'Vlr.Isenç.Juros BRL'.
  ls_fieldcat-outputlen = 15.
  ls_fieldcat-datatype = 'CURR'.
  ls_fieldcat-do_sum = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.


  " Justificativa
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'JUSTIFICATIVA2'.
  ls_fieldcat-scrtext_s = 'Justific.'.
  ls_fieldcat-scrtext_m = 'Justificativa'.
  ls_fieldcat-scrtext_l = 'Justificativa'.
  ls_fieldcat-icon = 'X'.
  ls_fieldcat-outputlen = 50.
  ls_fieldcat-hotspot = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

*  " Justificativa gerente
*  CLEAR ls_fieldcat.
*  ls_fieldcat-fieldname = 'JUSTIFICATIVA3'.
*  ls_fieldcat-scrtext_s = 'Justif Ger.'.
*  ls_fieldcat-scrtext_m = 'Justif.Gerente'.
*  ls_fieldcat-scrtext_l = 'Justif.Gerente'.
*  ls_fieldcat-icon = 'X'.
*  ls_fieldcat-outputlen = 50.
*  ls_fieldcat-hotspot = 'X'.
*  APPEND ls_fieldcat TO lt_fieldcat.


  " Usuário Solicitante
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'USUARIO_SOLICIT'.
  ls_fieldcat-scrtext_s = 'Usuário Sol.'.
  ls_fieldcat-scrtext_m = 'Usuário Solic.'.
  ls_fieldcat-scrtext_l = 'Usuário Solicitante'.
  ls_fieldcat-outputlen = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

  " Data
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'DATA'.
  ls_fieldcat-scrtext_s = 'Data'.
  ls_fieldcat-scrtext_m = 'Data'.
  ls_fieldcat-scrtext_l = 'Data'.
  ls_fieldcat-outputlen = 10.
  ls_fieldcat-datatype = 'DATS'.
  APPEND ls_fieldcat TO lt_fieldcat.

  " Hora
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'HORA'.
  ls_fieldcat-scrtext_s = 'Hora'.
  ls_fieldcat-scrtext_m = 'Hora'.
  ls_fieldcat-scrtext_l = 'Hora'.
  ls_fieldcat-outputlen = 8.
  ls_fieldcat-datatype = 'TIMS'.
  APPEND ls_fieldcat TO lt_fieldcat.


  " Set layout
  CLEAR ls_layout.
  ls_layout-zebra = 'X'.
  ls_layout-sel_mode = 'A'.
  ls_layout-cwidth_opt = 'X'.
  ls_layout-grid_title = 'Solicitação Isenção de Juros'.

  " Display ALV
  CALL METHOD go_alv->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_outtab       = gt_output
      it_fieldcatalog = lt_fieldcat.

ENDFORM.

FORM refresh_alv.
  go_alv->refresh_table_display( ).
ENDFORM.

FORM free_objects.
  IF go_alv IS NOT INITIAL.
    CALL METHOD go_alv->free.
    CLEAR go_alv.
  ENDIF.

  IF go_container IS NOT INITIAL.
    CALL METHOD go_container->free.
    CLEAR go_container.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* Class Implementation
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_toolbar.
    " Add custom buttons to ALV toolbar
    DATA: ls_toolbar TYPE stb_button.

    " Separator
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    "Liberar solicitação
    CLEAR ls_toolbar.
    ls_toolbar-function = 'LIBERAR'.
    ls_toolbar-icon = icon_release.
    ls_toolbar-quickinfo = 'Liberar Solicitação.'.
    ls_toolbar-text = 'Liberar Solicitação.'.
    ls_toolbar-butn_type = 0.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    "Cancelar Solicitação
    CLEAR ls_toolbar.
    ls_toolbar-function = 'CANCSOL'.
    ls_toolbar-icon = icon_system_undo.
    ls_toolbar-quickinfo = 'Cancelar Solicitação'.
    ls_toolbar-text = 'Cancelar Solicitação'.
    ls_toolbar-butn_type = 0.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    " Itens da solicitação
    CLEAR ls_toolbar.
    ls_toolbar-function = 'ITENS'.
    ls_toolbar-icon = icon_list.
    ls_toolbar-quickinfo = 'Itens da Solicitação'.
    ls_toolbar-text = 'Itens da Solicitação'.
    ls_toolbar-butn_type = 0.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    " Estratégia liberação
    CLEAR ls_toolbar.
    ls_toolbar-function = 'ESTRAT'.
    ls_toolbar-icon = icon_report.
    ls_toolbar-quickinfo = 'Estratégia Liberação'.
    ls_toolbar-text = 'Estratégia Liberação'.
    ls_toolbar-butn_type = 0.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    DATA: lv_message TYPE string.

    CASE e_ucomm.
      WHEN 'LIBERAR'.

        PERFORM liberar_solic.

      WHEN 'CANCSOL'.

        PERFORM cancela_solic.
*
      WHEN 'ITENS'.

        PERFORM exibe_itens.

      WHEN 'ESTRAT'.

        PERFORM exibe_estrategia.

    ENDCASE.
  ENDMETHOD.

  METHOD handle_double_click.



  ENDMETHOD.

  METHOD on_click2.

    DATA: lt_msg TYPE TABLE OF bdcmsgcoll.

    CHECK e_row_id GT 0.

    IF e_column_id = 'VBELN'.
      READ TABLE tg_itens INTO DATA(wl_itens) INDEX e_row_id.
      CHECK sy-subrc = 0.
      SET PARAMETER ID 'AUN' FIELD wl_itens-vbeln.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    ELSEIF e_column_id = 'SIMUL_VENDA'.
      DATA(lt_ordens) = tg_ordens[].
      SORT lt_ordens BY ov_principal.

      READ TABLE tg_itens INTO wl_itens INDEX e_row_id.
      CHECK sy-subrc = 0.
      DATA: lv_juros_calc TYPE vbap-netwr,
            lv_juros_rbdo TYPE vbap-netwr,
            lv_desc_jros  TYPE vbap-netwr,
*            lt_log        TYPE TABLE OF ty_log,
            lt_fieldcat   TYPE slis_t_fieldcat_alv.

      READ TABLE lt_ordens ASSIGNING FIELD-SYMBOL(<fs_ordens>)
      WITH KEY ov_principal = wl_itens-ov_principal
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        REFRESH it_bdcdata.

        DATA(lv_mode) = 'E'.

        IF <fs_ordens>-tipo_negocio(1) EQ '2'.

          PERFORM f_bdc_data USING:
                  'ZSDR016'   '0100'  'X'  ''                           ' ',
                  ''          ''      ''   'BDC_CURSOR'	                'WG_HEADER-DOC_SIMULACAO',
                  ''          ''      ''   'BDC_OKCODE'	                'ATUAL',
                  ''          ''      ''   'WG_HEADER-DOC_SIMULACAO'     wl_itens-simul_venda.

          CALL TRANSACTION 'ZSDT0044' USING it_bdcdata
                                        MODE lv_mode
                               MESSAGES INTO lt_msg.
        ELSEIF <fs_ordens>-tipo_negocio(1) EQ '1'.

          PERFORM f_bdc_data USING:
                  'ZSDR0022'  '0050'  'X'  ''                           ' ',
                  ''          ''      ''   'BDC_CURSOR'	                'WG_HEADER-NRO_SOL_OV',
                  ''          ''      ''   'BDC_OKCODE'	                'ATUAL',
                  ''          ''      ''   'WG_HEADER-NRO_SOL_OV'       wl_itens-simul_venda.

          CALL TRANSACTION 'ZSDT0062' USING it_bdcdata
                                       MODE lv_mode
                              MESSAGES INTO lt_msg.
        ENDIF.

      ENDIF.

    ELSEIF e_column_id = 'VLR_JUROS_CALC'.

      READ TABLE tg_itens INTO wl_itens INDEX e_row_id.
      CHECK sy-subrc = 0.
      PERFORM f_saida_memoria_calculo USING wl_itens.
      CALL SCREEN 0400 STARTING AT 1 1 ENDING AT 95 25.


    ENDIF.

  ENDMETHOD.

  METHOD on_click.

    DATA: lv_texto_alt TYPE string,
          lv_pos       TYPE i.

    DATA lv_data     TYPE c LENGTH 10.
    DATA lv_data_lib TYPE c LENGTH 10.
    DATA lv_hora     TYPE c LENGTH 10.
    DATA lv_hora_lib TYPE c LENGTH 10.

    IF e_column_id = 'JUSTIFICATIVA2'.

      FREE tg_texto.

      CLEAR: lv_texto_alt, lv_pos.

      READ TABLE gt_output INTO gs_output INDEX e_row_id.

      WRITE gs_output-data TO lv_data DD/MM/YY.

      lv_hora = gs_output-hora(2) && ':' && gs_output-hora+2(2) && ':' && gs_output-hora+4(2).

      APPEND INITIAL LINE TO tg_texto ASSIGNING FIELD-SYMBOL(<fs_texto>).
      <fs_texto> = |Usuário solicitante: | && gs_output-usuario_solicit && ' Data: ' && lv_data && ' Hora: ' && lv_hora.

      lv_texto_alt = gs_output-justificativa.

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_texto_alt WITH ''.  "

      " Quebra automaticamente em partes de 72
      DO.
        IF strlen( lv_texto_alt ) <= 72.
          " Última linha
          IF strlen( lv_texto_alt ) > 0.
            APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
            <fs_texto> = lv_texto_alt.
          ENDIF.
          EXIT.
        ELSE.
          " Adiciona parte de 72 caracteres
          APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
          <fs_texto> = lv_texto_alt(72).
          " Remove os 72 primeiros caracteres
          lv_texto_alt = lv_texto_alt+72.
        ENDIF.
      ENDDO.


*      APPEND INITIAL LINE TO tg_texto ASSIGNING FIELD-SYMBOL(<fs_texto>).
*      <fs_texto> = gs_output-justificativa.
      gv_display = 'X'.


      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title        = 'Texto para Motivo'
          im_display_mode = gv_display " Somente vizualizar ou inserir
        CHANGING
          ch_text         = tg_texto.


    ENDIF.

  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*& Form liberar_solic
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM liberar_solic .

  DATA: lt_text     TYPE TABLE OF as4text,
        ls_cadastro TYPE ty_cad_ordem.

  DATA: lt_selected_rows TYPE lvc_t_row,
        ls_selected_row  TYPE lvc_s_row,
        lv_text_name     TYPE tdobname,
        lv_text_id       TYPE tdobject VALUE 'TEXT',
        lv_text_language TYPE spras VALUE 'P',
        lv_client        TYPE mandt.

  DATA lt_estra    TYPE TABLE OF zsds019.


  DATA: lv_answer        TYPE c,
        lv_message       TYPE string,
        lv_total_count   TYPE i,
        lv_success_count TYPE i,
        lv_error_count   TYPE i,
        lv_cancel_count  TYPE i.


  DATA lv_total_chars TYPE i.
  DATA lv_line_chars  TYPE i.

  CALL METHOD go_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_selected_rows.

  IF lines( lt_selected_rows ) = 0.
    MESSAGE 'Selecione uma solicitação para liberar' TYPE 'S' DISPLAY LIKE 'E'.

  ELSE.

    lv_total_count = lines( lt_selected_rows ).


    IF lv_total_count > 1.

      lv_message = |Deseja liberar { lv_total_count } solicitações selecionadas? |.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação de Liberação Múltipla'
          text_question         = lv_message
          text_button_1         = 'Sim'(001)
          text_button_2         = 'Não'(002)
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF lv_answer NE '1'.
        MESSAGE 'Operação cancelada pelo usuário' TYPE 'S'.
        RETURN.
      ENDIF.

    ENDIF.

    gv_ucomm = sy-ucomm.

    CALL SCREEN '0200' STARTING AT 1 1 ENDING AT 80 20.

    IF gt_text IS NOT INITIAL.

      CLEAR: lv_total_chars.

      " Loop na tabela para contar caracteres
      LOOP AT gt_text INTO DATA(wa_text).
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN wa_text WITH ''.  "
        lv_line_chars = strlen( wa_text ).
        lv_total_chars = lv_total_chars + lv_line_chars.
      ENDLOOP.

      " Verificar se total é maior que 255
      IF lv_total_chars > 255.
        MESSAGE 'Texto excede 255 caracteres' TYPE 'S' DISPLAY LIKE 'E'.
        DATA(v_erro) = 'X'.

      ELSE.
        CLEAR v_erro.
      ENDIF.

    ENDIF.

    CLEAR gv_ucomm.

    IF gt_text IS NOT INITIAL AND v_erro IS INITIAL.

      " Process each selected row
      LOOP AT lt_selected_rows INTO ls_selected_row.

        READ TABLE gt_output INTO gs_output INDEX ls_selected_row-index.

        MOVE-CORRESPONDING gs_output TO gs_zfit186.

        gs_zfit186-status_solicit = '3'. " Aguardando Aprovação Workflow

*        CLEAR gs_zfit186-justificativa."BUG IMPEDITIVO 186091 SMC*


        LOOP AT gt_text ASSIGNING FIELD-SYMBOL(<fs_text>).

          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN <fs_text> WITH ''.
*          gs_zfit186-JUSTIFICATIVA_GER = gs_zfit186-justificativa && space && <fs_text>.
          gs_zfit186-justificativa_ger = gs_zfit186-justificativa_ger && space && <fs_text>. "BUG IMPEDITIVO 186091 SMC*
          gs_zfit186-usuar_lib = sy-uname."BUG IMPEDITIVO 186091 SMC*
          gs_zfit186-data_lib = sy-datum."BUG IMPEDITIVO 186091 SMC*
          gs_zfit186-hora_lib = sy-uzeit."BUG IMPEDITIVO 186091 SMC*

        ENDLOOP.

        MODIFY zfit186 FROM gs_zfit186.

        IF sy-subrc EQ 0.
*          COMMIT WORK.
        ENDIF.

*    SELECT SINGLE *
*    FROM vbak
*    INTO @DATA(ls_vbak)
*    WHERE vbeln = @ls_zfit186-ov_principal.

        SELECT *
           FROM zsdt0336
           INTO TABLE @DATA(lt_0336)
          WHERE  bukrs      <= @gs_output-vkorg
             AND bukrs_ate  >= @gs_output-vkorg
             AND vkbur      <= @gs_output-vkbur
             AND vkbur_ate  >= @gs_output-vkbur
             AND tp_negocio_de <= @gs_output-tipo_negocio
             AND tp_negocio_ate >= @gs_output-tipo_negocio
*         AND waers      EQ @gs_output-waerk

             AND valor_de   <= @gs_output-vl_isenc_brl
             AND valor_ate  >= @gs_output-vl_isenc_brl
             AND dt_val_de  <= @sy-datum
             AND dt_val_ate >= @sy-datum.
        IF sy-subrc IS INITIAL.

          DELETE lt_0336 WHERE dt_val_de  = sy-datum AND hr_val_de > sy-uzeit.
          DELETE lt_0336 WHERE dt_val_ate = sy-datum AND hr_val_ate < sy-uzeit.

          SORT lt_0336 BY nivel.
          READ TABLE lt_0336 TRANSPORTING NO FIELDS
          WITH KEY nivel = '1'
          BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.

            SORT lt_0336 BY nivel DESCENDING.

            READ TABLE lt_0336 ASSIGNING FIELD-SYMBOL(<fs_0336>) INDEX 1.

            SELECT *
               FROM zsdt0336
               APPENDING TABLE lt_0336
              WHERE  bukrs     <= gs_output-vkorg
                 AND bukrs_ate >= gs_output-vkorg
                 AND vkbur     <= gs_output-vkbur
                 AND vkbur_ate >= gs_output-vkbur
                 AND tp_negocio_de <= gs_output-tipo_negocio
                 AND tp_negocio_ate >= gs_output-tipo_negocio
*             AND waers     = gs_output
                 AND nivel < <fs_0336>-nivel
                 AND dt_val_de  <= sy-datum
                 AND dt_val_ate >= sy-datum.
            IF sy-subrc IS INITIAL.
              DELETE lt_0336 WHERE dt_val_de  = sy-datum AND hr_val_de > sy-uzeit.
              DELETE lt_0336 WHERE dt_val_ate = sy-datum AND hr_val_ate < sy-uzeit.
            ENDIF.

          ENDIF.

          SORT lt_0336 BY nivel.

          DATA lt_zsdt0337 TYPE TABLE OF zsdt0337.

          LOOP AT lt_0336 ASSIGNING <fs_0336>.

            APPEND INITIAL LINE TO lt_zsdt0337 ASSIGNING FIELD-SYMBOL(<fs_zsdt0337>).
            <fs_zsdt0337>-bukrs      = gs_output-vkorg.
            <fs_zsdt0337>-vbeln      = gs_output-ov_principal.
            <fs_zsdt0337>-status_apr = '1'.
            <fs_zsdt0337>-valor_de   = <fs_0336>-valor_de.
            <fs_zsdt0337>-valor_ate  = <fs_0336>-valor_ate.
            <fs_zsdt0337>-nivel      = <fs_0336>-nivel.
            <fs_zsdt0337>-data_atual = sy-datum.
            <fs_zsdt0337>-hora_atual = sy-uzeit.
            <fs_zsdt0337>-usuario    = sy-uname.
            <fs_zsdt0337>-aprovador  = <fs_0336>-aprovador.
            <fs_zsdt0337>-valor_moeda_doc = gs_output-vl_moeda_doc.
            <fs_zsdt0337>-moeda_doc = gs_output-moeda."SMC 18-06-2025
            <fs_zsdt0337>-cod_solict_isencao = gs_output-cd_sol_isen."SMC 18-06-2025
            <fs_zsdt0337>-valor_brl  = gs_output-vl_isenc_brl."SMC 18-06-2025

*
            APPEND INITIAL LINE TO lt_estra ASSIGNING FIELD-SYMBOL(<fs_estra>).
            <fs_estra>-nivel     = <fs_0336>-nivel.
            <fs_estra>-aprovador = <fs_0336>-aprovador.

          ENDLOOP.

*      CLEAR lv_id.

        ENDIF.

      ENDLOOP.

      IF lt_zsdt0337 IS NOT INITIAL.

        DELETE ADJACENT DUPLICATES FROM lt_zsdt0337 COMPARING ALL FIELDS.

        MODIFY zsdt0337 FROM TABLE lt_zsdt0337.

        IF sy-subrc IS INITIAL.

*          COMMIT WORK.

          CALL METHOD go_textedit->delete_text.

          MESSAGE 'Isenção liberada com sucesso' TYPE 'S'.

        ENDIF.

      ENDIF.

      DELETE ADJACENT DUPLICATES FROM lt_estra COMPARING ALL FIELDS.

      SORT lt_estra BY nivel.

      READ TABLE lt_zsdt0337 INTO DATA(ls_0337) INDEX 1.

      IF ls_0337 IS NOT INITIAL.

        DATA(ls_vbak) = gt_output[ ov_principal = ls_0337-vbeln ].

        ls_cadastro-vbeln      = ls_0337-vbeln.
        ls_cadastro-valor      = ls_0337-valor_brl.
        ls_cadastro-org_vendas = ls_vbak-vkorg.
        ls_cadastro-solicitante = sy-uname.
        ls_cadastro-filial      = ls_vbak-vkbur.

        PERFORM envia_email TABLES lt_estra
                             USING ls_cadastro
                                   '1'.

        COMMIT WORK.

        PERFORM get_data.

        PERFORM refresh_alv.

      ELSE.

        ROLLBACK WORK.

        MESSAGE 'Não foi encontrado estratégia para solicitação' TYPE 'I'.

      ENDIF.

    ENDIF.

  ENDIF.

  CLEAR gt_text.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form cancela_solic
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM cancela_solic .

  DATA: lt_selected_rows TYPE lvc_t_row,
        ls_selected_row  TYPE lvc_s_row.

  DATA lv_canc TYPE boolean VALUE 'X'.

  CALL METHOD go_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_selected_rows.

  IF lines( lt_selected_rows ) = 0.
    MESSAGE 'Selecione ao menos uma solicitação' TYPE 'S' DISPLAY LIKE 'E'.
  ELSEIF lines( lt_selected_rows ) > 1.
    MESSAGE 'Selecione apenas uma linha para cancelar a solicitação' TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.

    PERFORM verifica_estrategia . "--------------#146630-CS2024000604 Isenção de Juros Insumos---SMC

    gv_ucomm = sy-ucomm.

    FREE gt_text.

    CALL SCREEN '0200' STARTING AT 1 1 ENDING AT 80 20.

    CLEAR gv_ucomm.

    IF gt_text IS NOT INITIAL.
      " Process each selected row
      LOOP AT lt_selected_rows INTO ls_selected_row.

        READ TABLE gt_output INTO gs_output INDEX ls_selected_row-index.

*        CASE gs_output-status_solicit.

*          WHEN '1'.

        MOVE-CORRESPONDING gs_output TO gs_zfit186.
        gs_zfit186-status_solicit = '2'.

        gs_zfit186-dtcan = sy-datum.
        gs_zfit186-hrcan = sy-uzeit.
        gs_zfit186-usrcanc = sy-uname.

        LOOP AT gt_text ASSIGNING FIELD-SYMBOL(<fs_text>).
          REPLACE ALL OCCURRENCES OF '#' IN <fs_text> WITH space.
          gs_zfit186-juscanc = gs_zfit186-juscanc && space && <fs_text>.
        ENDLOOP.

        MODIFY zfit186 FROM gs_zfit186.

        IF sy-subrc EQ 0.

          COMMIT WORK.

          MESSAGE 'Solicitação cancelada com sucesso' TYPE 'S'.

        ENDIF.

        CLEAR gs_zfit186.


*          WHEN '2'.

*            MESSAGE 'Item já possui status cancelado' TYPE 'S' DISPLAY LIKE 'E'.

*          WHEN '3' OR '4'.

*        SELECT *
*         FROM zsdt0337
*         INTO TABLE @DATA(lt_zsdt0337)
*         WHERE vbeln EQ @gs_output-ov_principal
*         AND   bukrs EQ @gs_output-vkorg
*         AND   cod_solict_isencao EQ @gs_output-cd_sol_isen.
*
*        IF sy-subrc EQ 0.
*
*          SORT lt_zsdt0337 BY nivel DESCENDING.
*
*          DATA(ls_ultimo_nivel) = lt_zsdt0337[ 1 ].
*
*          IF ls_ultimo_nivel-status_apr EQ '4'.
*
*            LOOP AT lt_zsdt0337 ASSIGNING FIELD-SYMBOL(<fs_0337>).
*
*              <fs_0337>-status_apr = '2'. "cancelado
*
*            ENDLOOP.
*
*            MODIFY zsdt0337 FROM TABLE lt_zsdt0337.
*
*          ELSE.
*
*            MESSAGE |Item { ls_selected_row-index } não pode ser cancelado| TYPE 'S' DISPLAY LIKE 'E'.
*
*          ENDIF.
*
*        ENDIF.

*          WHEN OTHERS.

*        ENDCASE.

      ENDLOOP.

      PERFORM get_data.

      PERFORM refresh_alv.


      CLEAR gt_text.

    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form verifica_estrategia
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM verifica_estrategia .




ENDFORM.


*&---------------------------------------------------------------------*
*& Form envia_email
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ESTRA
*&      --> LS_CADASTRO
*&      --> P_
*&---------------------------------------------------------------------*
FORM envia_email TABLES tg_estra
                  USING VALUE(wg_cad_ordem) TYPE ty_cad_ordem
                              plinha  .

  FIELD-SYMBOLS: <fs_solix> TYPE solix.

* Objetos para enviar email
  DATA: objpack     LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE.
  DATA: objhead     LIKE solisti1   OCCURS  1 WITH HEADER LINE.
  DATA: objbin_ord  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_log  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_ann  TYPE solisti1.
  DATA: objbin    LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objbin1   TYPE soli_tab, "   OCCURS 10 WITH HEADER LINE.
        wa_objbin LIKE LINE OF objbin.
  DATA: content_hex TYPE STANDARD TABLE OF solix WITH HEADER LINE.
  DATA: objtxt      LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: reclist     LIKE somlreci1  OCCURS  5 WITH HEADER LINE.
  DATA: doc_chng    LIKE sodocchgi1.
  DATA: tab_lines   LIKE sy-tabix.
  DATA: l_anex      TYPE string.
  DATA: l_leng      TYPE i.
  DATA: l_arq       TYPE string.
  DATA: l_tam       TYPE i.
  DATA: l_tam_ord   TYPE i.
  DATA: l_tam_log   TYPE i.
  DATA: l_email(300) TYPE c.
  DATA: vlinha      TYPE i.
  DATA: vuser       TYPE sy-uname.
  DATA: it_shortcut_param LIKE zst_shortcut_par OCCURS 0 WITH HEADER LINE.
  DATA: content  TYPE string,
        wa_estra TYPE zsds019.
  DATA: bsmtp_addr TYPE adr6-smtp_addr,
        lv_valor   TYPE string.

*  ** Pass the required parameters and create the shortcut
  CLEAR it_shortcut_param.
  REFRESH it_shortcut_param.

  DELETE ADJACENT DUPLICATES FROM tg_estra COMPARING ALL FIELDS.

  IF plinha > 0.

    vlinha = plinha.

    READ TABLE tg_estra INTO wa_estra INDEX vlinha .

    SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
      FROM usr21
        INNER JOIN adr6
           ON  usr21~addrnumber = adr6~addrnumber
          AND usr21~persnumber = adr6~persnumber
              WHERE usr21~bname = wa_estra-aprovador.

  ELSE.

    LOOP AT tg_estra INTO wa_estra.
      SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
        FROM usr21
       INNER JOIN adr6
             ON  usr21~addrnumber = adr6~addrnumber
            AND usr21~persnumber = adr6~persnumber
          WHERE usr21~bname = wa_estra-aprovador.
      IF sy-subrc IS INITIAL.
        reclist-receiver = bsmtp_addr.
        reclist-rec_type = 'U'.                    "Define email externo
        APPEND reclist.
      ENDIF.
    ENDLOOP.

  ENDIF.
* Criação do documento de Email
  doc_chng-obj_name = 'LOG_ESTRA'.

* Assunto do Email
  doc_chng-obj_descr = 'Solicitação de Isenção de Juros OV: ' && wg_cad_ordem-vbeln.

* Texto
  objtxt-line = 'Está disponível para aprovação no sistema SAP:'.
  APPEND objtxt.
  CLEAR objtxt.
  APPEND objtxt.

  objtxt-line = 'Aprovação de Isenção de Juros OV: ' &&  wg_cad_ordem-vbeln.
  APPEND objtxt.
  CLEAR objtxt.

  SELECT SINGLE butxt
    FROM t001
    INTO @DATA(lv_butxt)
    WHERE bukrs = @wg_cad_ordem-org_vendas.

  CONCATENATE 'Organização de Venda: ' wg_cad_ordem-org_vendas '-' lv_butxt INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

  SELECT SINGLE name1
    FROM t001w
    INTO @DATA(lv_name1)
    WHERE werks = @wg_cad_ordem-filial.

  CONCATENATE 'Escr. Vendas: ' wg_cad_ordem-filial '-' lv_name1 INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

  lv_valor = wg_cad_ordem-valor.
  CONCATENATE  'Valor BRL: ' lv_valor INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

  SELECT name_first, name_last
    FROM user_addr
    INTO @DATA(ls_nome)
    UP TO 1 ROWS
    WHERE bname = @wg_cad_ordem-solicitante.
  ENDSELECT.
  IF sy-subrc IS INITIAL.

    IF ls_nome-name_first IS INITIAL AND ls_nome-name_last IS INITIAL.
      ls_nome-name_first = wg_cad_ordem-solicitante.
    ENDIF.

  ENDIF.
  CONCATENATE 'Solicitante: ' ls_nome-name_first ls_nome-name_last INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = '-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = 'Para aprovar clique no link "Estratégia" em anexo' .
  APPEND objtxt.
  CLEAR objtxt.

  DATA: ctotal(20),
        vdata(10).

  WRITE wg_cad_ordem-valor TO ctotal CURRENCY 'USD'.

  CONDENSE ctotal NO-GAPS.

  SELECT SINGLE waerk
     FROM vbak INTO @DATA(_waerk)
    WHERE vbeln = @wg_cad_ordem-vbeln.

* Setar tamanho da mensagem
  DESCRIBE TABLE objtxt LINES tab_lines.
  READ TABLE objtxt INDEX tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

* Criar entrada de documento comprimido
  CLEAR objpack-transf_bin.
  "OBJPACK-TRANSF_BIN = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 0.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'RAW'.
  APPEND objpack.

  CALL FUNCTION 'ZFM_CREATE_SHORTCUT'
    EXPORTING
      recipient_user_id = wa_estra-aprovador
      transaction       = 'ZFIS66'
    IMPORTING
      content           = content
    TABLES
      shortcut_param    = it_shortcut_param.

  CLEAR : tab_lines, objbin.
  CONCATENATE content wa_objbin-line INTO wa_objbin-line.
  APPEND  wa_objbin TO objbin.

  DESCRIBE TABLE objbin LINES tab_lines.
  objhead = 'ESTRATEGIA.SAP'.
  APPEND objhead.

** Creation of the entry for the compressed attachment
  objpack-transf_bin = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 1.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'EXT'." SAP
  objpack-obj_name   = 'SAPSHORTCUTMAIL'.
  objpack-obj_descr  = 'ESTRATEGIA.SAP'.
  objpack-doc_size   = tab_lines * 255.
  APPEND objpack.

* Alimentar destinatários do email
  IF bsmtp_addr IS INITIAL.
    MESSAGE 'O aprovador seguinte não tem e-mail cadastrado, por favor contacte a T.I.' TYPE 'I'.
    EXIT.
  ENDIF.

  reclist-receiver = bsmtp_addr.
  reclist-rec_type = 'U'.                    "Define email externo
  APPEND reclist.

* Enviar email
  vuser = sy-uname.
  sy-uname = 'R3JOB'.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = objpack
      object_header              = objhead
      contents_bin               = objbin
      contents_txt               = objtxt      "CONTENTS_HEX = CONTENT_HEX
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      OTHERS                     = 99.

  sy-uname = vuser.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form exibe_estrategia
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM exibe_estrategia .

  DATA v_report        LIKE sy-repid.

  DATA: lt_selected_rows TYPE lvc_t_row,
        ls_selected_row  TYPE lvc_s_row.

  DATA lv_canc TYPE boolean VALUE 'X'.

  CALL METHOD go_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_selected_rows.

  IF lines( lt_selected_rows ) = 0.
    MESSAGE 'Selecione ao menos uma solicitação' TYPE 'S' DISPLAY LIKE 'E'.

  ELSEIF lines( lt_selected_rows ) > 1.
    MESSAGE 'Selecione apenas uma linha para cancelar a solicitação' TYPE 'S' DISPLAY LIKE 'E'.

  ELSEIF lines( lt_selected_rows ) = 1.

    DATA(lv_line) = lt_selected_rows[ 1 ].

  ENDIF.


  READ TABLE gt_output INTO gs_output INDEX lv_line-index.

  REFRESH: estrutura.

  FREE: gt_fieldcat.

  PERFORM montar_estrutura_alv USING:
     1  'ZSDT0161'    'NIVEL'       'TG_ESTRAT' 'NIVEL'      ''           ' ' ' '  ' ' ' ',
     2  'ZSDT0161'    'APROVADOR'   'TG_ESTRAT' 'APROVADOR'  ''           ' ' ' '  ' ' ' ',
     3  'ZSDT0161'    'VALOR_DE'    'TG_ESTRAT' 'VALOR_DE'   'Valor De'   ' ' ' '  ' ' ' ',
     4  'ZSDT0161'    'VALOR_ATE'   'TG_ESTRAT' 'VALOR_ATE'  'Valor Até'  ' ' ' '  ' ' ' '.

  FREE tg_estrat.

  SELECT *
    FROM zsdt0336
    INTO TABLE @DATA(lt_0336)
   WHERE  bukrs     <= @gs_output-vkorg
      AND bukrs_ate >= @gs_output-vkorg
      AND vkbur     <= @gs_output-vkbur
      AND vkbur_ate >= @gs_output-vkbur
"INICIO BUG SOLTO 145061 - RU
      AND tp_negocio_de <= @gs_output-tipo_negocio
      AND tp_negocio_ate >= @gs_output-tipo_negocio
*      AND waers     = @ls_vbak-waerk       "146630 - RGA
"FIM BUG SOLTO 145061 - RU
      AND valor_de <= @gs_output-vl_isenc_brl   "146630 - RGA
      AND valor_ate >= @gs_output-vl_isenc_brl  "146630 - RGA
      AND dt_val_de  <= @sy-datum
      AND dt_val_ate >= @sy-datum.
*      AND hr_val_de  <= @sy-uzeit
*      AND hr_val_ate >= @sy-uzeit.
  IF sy-subrc IS INITIAL.

    DELETE lt_0336 WHERE dt_val_de = sy-datum AND hr_val_de > sy-uzeit.
    DELETE lt_0336 WHERE dt_val_ate = sy-datum AND hr_val_ate < sy-uzeit.

    SORT lt_0336 BY nivel.
    READ TABLE lt_0336 TRANSPORTING NO FIELDS
    WITH KEY nivel = '1'
    BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      SORT lt_0336 BY nivel DESCENDING.

      READ TABLE lt_0336 ASSIGNING FIELD-SYMBOL(<fs_0336>) INDEX 1.

      SELECT *
         FROM zsdt0336
         APPENDING TABLE lt_0336
        WHERE  bukrs     <= gs_output-vkorg
           AND bukrs_ate >= gs_output-vkorg
           AND vkbur     <= gs_output-vkbur
           AND vkbur_ate >= gs_output-vkbur
"INICIO BUG SOLTO 145061 - RU
           AND tp_negocio_de <= gs_output-tipo_negocio
           AND tp_negocio_ate >= gs_output-tipo_negocio
*           AND waers     EQ ls_vbak-waerk
"FIM BUG SOLTO 145061 - RU
           AND nivel     < <fs_0336>-nivel
           AND dt_val_de  <= sy-datum
           AND dt_val_ate >= sy-datum.
      IF sy-subrc IS INITIAL.
        DELETE lt_0336 WHERE dt_val_de = sy-datum AND hr_val_de > sy-uzeit.
        DELETE lt_0336 WHERE dt_val_ate = sy-datum AND hr_val_ate < sy-uzeit.
      ENDIF.

    ENDIF.


    SORT lt_0336 BY nivel.
    LOOP AT lt_0336 ASSIGNING <fs_0336>.
      APPEND INITIAL LINE TO tg_estrat ASSIGNING FIELD-SYMBOL(<fs_estrat>).
      <fs_estrat>-aprovador = <fs_0336>-aprovador.
      <fs_estrat>-nivel     = <fs_0336>-nivel.
      <fs_estrat>-valor_de  = <fs_0336>-valor_de.
      <fs_estrat>-valor_ate = <fs_0336>-valor_ate.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM tg_estrat COMPARING ALL FIELDS.
  ENDIF.

  SORT tg_estrat BY nivel.

  IF ( tg_estrat[] IS INITIAL ).
    MESSAGE 'Não foi encontrado estratégia para solicitação!' TYPE 'I'.
    EXIT.
  ENDIF.

  DATA: wl_layout TYPE  slis_layout_alv.

  wl_layout-zebra = abap_true.
  wl_layout-colwidth_optimize = abap_true.
  wl_layout-window_titlebar = 'Estratégia de liberação'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = v_report
      is_layout             = wl_layout
      it_fieldcat           = gt_fieldcat[]
      i_default             = ' '
      i_save                = ' '
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 100
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tg_estrat.

ENDFORM.

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  DATA: x_contador   TYPE string,
        wa_estrutura TYPE slis_fieldcat_alv.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-edit          = p_edit.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-reptext_ddic  = p_scrtext_l.
  wa_estrutura-outputlen     = x_contador.

  IF p_field EQ 'SALDO'.
    wa_estrutura-do_sum = abap_true.
  ENDIF.

  IF ( p_field EQ 'NIVEL' ) OR ( p_field EQ 'APROVADOR' ).
    wa_estrutura-no_zero = abap_true.
    wa_estrutura-just    = 'C'.
  ENDIF.


  APPEND wa_estrutura TO gt_fieldcat.

ENDFORM.                    " montar_estrutura


*&---------------------------------------------------------------------*
*& Form exibe_itens
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM exibe_itens .

  DATA : tl_filter TYPE lvc_t_filt,
         wl_filter TYPE lvc_s_filt.

  PERFORM atualiza_ordens.


  PERFORM carrega_itens.


  IF go_container3 IS INITIAL.


    CREATE OBJECT go_container3
      EXPORTING
        container_name = g_cc_itens.

    CREATE OBJECT grid3
      EXPORTING
        i_parent = go_container3.

    PERFORM montar_layout_itens.

    PERFORM f_config_function_alv USING 'GRID3'.

    wa_layout-no_toolbar = space.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = 'Itens da Ordem' .
    wa_layout-no_toolbar = 'X'.

*    PERFORM montar_layout_itens.

    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_itens[].

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
                  lcl_event_handler=>on_click2 FOR grid3.

  ELSE.

    PERFORM montar_layout_itens.

    CALL METHOD grid3->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CALL SCREEN 300.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form montar_layout_itens
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM montar_layout_itens .

  REFRESH t_fieldcatalog.

  IF tg_itens[] IS NOT INITIAL.
    READ TABLE tg_itens INTO DATA(w_itens) INDEX 1.
  ENDIF.

  PERFORM montar_estrutura USING:
        01 ''           ''    'TG_ITENS'  'ORG_VENDAS'      TEXT-a01         '10' ' ' ' ' ' ',
        02 ''           ''    'TG_ITENS'  'ESCR_VENDAS'     TEXT-a22         '20' ' ' ' ' ' ',
        03 ''           ''    'TG_ITENS'  'SIMUL_VENDA'     TEXT-a23         '13' ' ' ' ' ' ',
        04 ''           ''    'TG_ITENS'  'VBELN'           TEXT-a02         '13' ' ' ' ' ' ',
        05 ''           ''    'TG_ITENS'  'DATA_VENC'       TEXT-a36         '13' ' ' ' ' ' ',
        06 ''           ''    'TG_ITENS'  'WAERK'           TEXT-a16         '06' ' ' ' ' ' ',
        07 ''           ''    'TG_ITENS'  'VALOR_OV'        TEXT-a29         '13' ' ' ' ' ' ',
        08 ''           ''    'TG_ITENS'  'VLR_JUROS_CALC'  TEXT-a30         '13' ' ' ' ' ' ',
        09 ''           ''    'TG_ITENS'  'VLR_JUROS_RBDO'  TEXT-a31         '13' ' ' ' ' ' ',
        10 ''           ''    'TG_ITENS'  'VLR_DESC_JROS'   TEXT-a32         '13' ' ' ' ' ' ',
        11 ''           ''    'TG_ITENS'  'SALDO_JUROS'     TEXT-a33         '13' ' ' ' ' ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_config_function_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM f_config_function_alv  USING  p_grid.

  CASE p_grid.

    WHEN 'GRID1' OR 'GRID2' OR 'GRID3'.
      REFRESH: tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
      APPEND wl_function TO tl_function.

      IF p_grid = 'GRID1'.
        wl_function = cl_gui_alv_grid=>mc_fc_check.
        APPEND wl_function TO tl_function.
        wl_function = cl_gui_alv_grid=>mc_fc_refresh.
        APPEND wl_function TO tl_function.
      ENDIF.

  ENDCASE.
ENDFORM.

FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            p_scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  IF w_fieldcatalog-fieldname EQ 'EMPRESA' OR
     w_fieldcatalog-fieldname EQ 'ESCVENDA'.
    w_fieldcatalog-no_out        = abap_true.
  ELSE.
    w_fieldcatalog-no_out        = ' '.
  ENDIF.

  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  IF p_field EQ 'OPCOES' OR
     p_field EQ 'DOC_LCTO' OR
     p_field EQ 'BELNR' OR
     p_field EQ 'SIMUL_VENDA' OR
     p_field EQ 'VLR_JUROS_CALC'.
    w_fieldcatalog-hotspot = 'X'.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*& Form atualiza_ordens
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM atualiza_ordens .


*  DATA: v_msg    TYPE char50,
*        t_ordens TYPE TABLE OF zsd_ord_vendas_est_isen_juros,
*        t_estra  TYPE TABLE OF zsd_estrategia_ov,
*        t_itens  TYPE TABLE OF zsd_itens_ov_est_isen_jur.

  CLEAR: wg_cad_ordem, it_itens.

*  REFRESH: tg_ordens, it_estra, it_itens.

*  CALL FUNCTION 'Z_OV_ESTRATEGIA_LISTA_ISEN_JUR'
*    EXPORTING
*      i_usuario = sy-uname
*    IMPORTING
*      e_msg     = v_msg
*    TABLES
*      t_ordens  = t_ordens
*      t_estra   = t_estra
*      t_itens   = t_itens
*      t_026     = tg_026.

  LOOP AT gt_ordens_aux INTO DATA(w_ordens).

    CLEAR: tg_ordens.
    MOVE-CORRESPONDING w_ordens TO tg_ordens.
**********************************************************************
*120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
    FREE: aux_t001w.
    CLEAR: aux_t001w.

    SELECT SINGLE vkbur FROM vbak
      INTO @aux_empresa
      WHERE vbeln = @w_ordens-ov_principal.
    SELECT SINGLE werks,name1 FROM t001w
      INTO CORRESPONDING FIELDS OF @aux_t001w
      WHERE werks = @aux_empresa.

    CONCATENATE  aux_t001w-werks '-' aux_t001w-name1 INTO tg_ordens-escvenda SEPARATED BY space.
    tg_ordens-vkbur = aux_t001w-werks. "120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
**********************************************************************
    APPEND tg_ordens.
  ENDLOOP.

  IF tg_ordens[] IS NOT INITIAL.

    LOOP AT gt_estra_aux INTO DATA(w_estra).
      CLEAR: wa_estra.
      MOVE-CORRESPONDING w_estra TO wa_estra.
      APPEND wa_estra TO it_estra.
    ENDLOOP.

    SORT it_estra BY vbeln nivel.

    DELETE gt_itens_aux WHERE saldo_juros = 0.

    LOOP AT gt_itens_aux INTO DATA(w_itens).
      CLEAR: wa_itens.
      MOVE-CORRESPONDING w_itens TO wa_itens.
      APPEND wa_itens TO it_itens.
    ENDLOOP.

    SORT it_itens BY vbeln.

  ENDIF.


  IF go_container3 IS NOT INITIAL.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form carrega_itens
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM carrega_itens .


  DATA: lt_selected_rows TYPE lvc_t_row,
        ls_selected_row  TYPE lvc_s_row.

  DATA lv_canc TYPE boolean VALUE 'X'.

  CALL METHOD go_alv->get_selected_rows
    IMPORTING
      et_index_rows = lt_selected_rows.

  IF lines( lt_selected_rows ) = 0.
    MESSAGE 'Selecione ao menos uma solicitação' TYPE 'S' DISPLAY LIKE 'E'.

  ELSEIF lines( lt_selected_rows ) > 1.
    MESSAGE 'Selecione apenas uma linha para cancelar a solicitação' TYPE 'S' DISPLAY LIKE 'E'.

  ELSEIF lines( lt_selected_rows ) = 1.

    DATA(lv_line) = lt_selected_rows[ 1 ].

  ENDIF.

  CLEAR: wg_cad_ordem, tg_estra[], tg_itens[].
*
  READ TABLE gt_output INTO DATA(wl_ordens) INDEX lv_line-index.
*
  CHECK sy-subrc = 0.
  wg_cad_ordem-empresa = wl_ordens-vkbur(4).
  wg_cad_ordem-vbeln   = wl_ordens-ov_principal.
  wg_cad_ordem-usuario = wl_ordens-usuario_solicit.
  wg_cad_ordem-netwr   = wl_ordens-vl_moeda_doc.
  wg_cad_ordem-waerk   = wl_ordens-moeda.
*
*
  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '=ENT'
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.

*
*    LOOP AT it_estra INTO DATA(wa_estra) WHERE vbeln = wl_ordens-ov_principal
*                                           AND waerk = wl_ordens-moeda. "BUG SOLTO 145061 - SMC
*      APPEND wa_estra TO tg_estra.
*    ENDLOOP.
*
  LOOP AT it_itens ASSIGNING FIELD-SYMBOL(<fs_itens>) WHERE ov_principal = wl_ordens-ov_principal.

**********************************************************************
*120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
*      FREE: aux_t001w.
*      CLEAR: aux_t001w.

    SELECT SINGLE werks,name1 FROM t001w
      INTO @DATA(aux_t001w)
      WHERE werks = @<fs_itens>-escr_vendas.
    <fs_itens>-escr_vendas = |{ aux_t001w-werks } - { aux_t001w-name1 }|. "120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
**********************************************************************

    APPEND <fs_itens> TO tg_itens.

  ENDLOOP.
*
*  ENDIF.
*
*  SORT tg_estra BY nivel.

*  CALL METHOD grid2->refresh_table_display
*    EXPORTING
*      is_stable = wa_stable.

  IF grid3 IS NOT INITIAL.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
*    IF sy-subrc = 0.
*      MESSAGE |Material: { gs_output-material } - { gs_output-description }| TYPE 'I'.
*    ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      -->P_DYNPRO   text
*      -->P_START    text
*      -->P_FNAM     text
*      -->P_FVAL     text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.

  DATA: wa_bdcdata TYPE bdcdata.

  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                    " F_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_MEMORIA_CALCULO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida_memoria_calculo USING p_itens TYPE ty_itens.

  DATA qtde_dias_atraso TYPE int4.
  DATA data_venc_format TYPE char20.
  DATA data_pgto_format TYPE char20.
  DATA: total_ov TYPE vbap-netwr.
  DATA: vlr_total TYPE vbap-netwr.
  DATA: total_ov_parc TYPE vbap-netwr,
        lv_tp_neg     TYPE ty_ordens-tipo_negocio.
  CLEAR: wa_edit, w_edit, total_ov, total_ov_parc, ind_rec_total, ind_doc_fatura, ind_rec_parc.

*  DATA_VENC_FORMAT = | { WA_SAIDA_P-DATA_VENC+6(2) }.{ WA_SAIDA_P-DATA_VENC+4(2) }.{ WA_SAIDA_P-DATA_VENC(4) }|.
*  DATA_PGTO_FORMAT = | { WA_SAIDA_P-DATA_PGTO+6(2) }.{ WA_SAIDA_P-DATA_PGTO+4(2) }.{ WA_SAIDA_P-DATA_PGTO(4) }|.
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_MULTA =  | Valor Multa = { WA_SAIDA_P-MONT_RBDO }(Mont.RBDO) * { WA_SAIDA_P-TX_MULTA }(Tx.Multa)%  = { WA_SAIDA_P-VLR_MULTA_CALC }|.
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_JUROS_DIA_ULTIL = |Se a data de vencimento { DATA_VENC_FORMAT } for dia não útil e a data de pagamento { DATA_PGTO_FORMAT } for primeiro dia útil subsequente a data de vencimento { DATA_VENC_FORMAT }  |.



*  CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
*    EXPORTING
*      I_DATUM_BIS = WA_SAIDA_P-DATA_VENC  " Data Maior
*      I_DATUM_VON = WA_SAIDA_P-DATA_PGTO  " Data Menor
*    IMPORTING
*      E_TAGE      = QTDE_DIAS_ATRASO. " Dieferença em dias
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_DIAS_ATRASO = |Quantidade de dias em atraso: {  COND #( WHEN  QTDE_DIAS_ATRASO = 0 OR QTDE_DIAS_ATRASO IS INITIAL THEN 0 ELSE QTDE_DIAS_ATRASO   ) } dias. | .
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_JUROS_DIA = 0.
*
*  IF  WA_SAIDA-TX_JUROS IS NOT INITIAL AND WA_SAIDA-TX_JUROS <> 0 .
*
*    WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_JUROS_DIA  =  | Valor de Juros ao dia: { WA_SAIDA-MONT_RBDO *  QTDE_DIAS_ATRASO *  ( WA_SAIDA-TX_JUROS / 360 ) }%|.

*  ENDIF.

*===============================================================================

*  SELECT *  FROM VBAP INTO TABLE @DATA(T_VBAP)
*          WHERE VBELN EQ @WA_SAIDA-VBELN.
*
*  CLEAR TOTAL_OV.
*  LOOP AT IT_VBAP INTO WA_VBAP WHERE VBELN = WA_SAIDA-VBELN.
*    ADD WA_VBAP-NETWR TO TOTAL_OV.
*    ADD WA_VBAP-MWSBP TO TOTAL_OV.
*  ENDLOOP.

****  Selecionar informações zfit0026.
  SELECT SINGLE *
  FROM zfit0026
    INTO @DATA(w_zfit0026)
     WHERE zid_lanc EQ @p_itens-zid_lanc.

  CHECK w_zfit0026 IS NOT INITIAL.
*  W_EDIT-DOC_FATURA = W_ZFIT0026-DOC_FATURA.
*  w_edit-doc_fatura = p_itens-vbeln.
  w_edit-vbeln = w_zfit0026-vbeln.



  IF w_zfit0026-doc_fatura IS INITIAL.
****   Buscar o saldo total e parcial da OV.
    zcl_dados_ov=>i_vlr_ov(
      EXPORTING
        i_vbeln       = w_zfit0026-vbeln
      IMPORTING
        e_vlr_total   = vlr_total
        e_vlr_parcial = total_ov_parc ).


    total_ov = ( total_ov_parc + w_zfit0026-mont_moeda ).

  ELSE.
****     Buscar o saldo total e parcial da referencia.
    zcl_dados_ov=>i_vlr_referencia_ov(
      EXPORTING
        i_vbeln       = w_zfit0026-vbeln
        i_vbelnn      = w_zfit0026-doc_fatura
      IMPORTING
        e_vlr_total   = vlr_total
        e_vlr_parcial = total_ov_parc ).
*    W_EDIT-TXT_DOC = 'Calculo com base na referencia'.

    CLEAR: total_ov.
    total_ov = ( total_ov_parc + w_zfit0026-mont_moeda ).
    ind_doc_fatura = abap_true.
  ENDIF.

***   Achar o % fator

  READ TABLE tg_ordens ASSIGNING FIELD-SYMBOL(<fs_ordens>)
  WITH KEY ov_principal = p_itens-ov_principal.
  IF sy-subrc IS INITIAL.
    lv_tp_neg = <fs_ordens>-tipo_negocio.
  ENDIF.

  IF lv_tp_neg(1) EQ '1'. "Se mercado interno for selecionado.

    IF p_itens-vlr_juros_calc > 0.
      IF w_zfit0026-data_pgto > w_zfit0026-data_venc.
        wa_edit-dias_atraso = ( w_zfit0026-data_pgto - w_zfit0026-data_venc ).
        w_edit-dias_atraso = wa_edit-dias_atraso.

        SELECT SINGLE *  FROM zsdt0051 INTO @DATA(wa_zsdt0051)
        WHERE nro_sol_ov EQ @p_itens-simul_venda.

        wa_edit-data_venc    =    w_zfit0026-data_venc.
        wa_edit-data_pgto    =    w_zfit0026-data_pgto.
        wa_edit-fator      = ( wa_zsdt0051-tx_juros / 360 ) * wa_edit-dias_atraso.

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          wa_edit-juros      = ( total_ov * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( total_ov * wa_zsdt0051-tx_multa ) / 100.
          DATA(t_ov)              = ( wa_edit-juros + total_ov +  wa_edit-multa ). "Total da OV + juros.

        ELSE.

          wa_edit-juros      = ( vlr_total * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( vlr_total * wa_zsdt0051-tx_multa ) / 100.
          t_ov               = ( wa_edit-juros + vlr_total +  wa_edit-multa ). "Total da OV + juros.

          wa_edit-porc_juros = ( wa_edit-juros / t_ov )  * 100. " Porcentagem proporcional ao valor do juros
          DATA(prop_multa)   = ( wa_edit-multa / t_ov )  * 100. " Porcentagem proporcional da multa.

          wa_edit-juros_parc = ( wa_edit-porc_juros * w_zfit0026-mont_rbdo ) / 100.
          DATA(vlr_multa)    = ( prop_multa * w_zfit0026-mont_rbdo ) / 100.
          total_ov = vlr_total.
        ENDIF.

        wa_edit-tx_jros      = wa_zsdt0051-tx_juros.
        wa_edit-vlr_total_ov  = total_ov.
        wa_edit-vlr_rbdo      = w_zfit0026-mont_rbdo.

        wa_edit-dias_ano      = '360'.
        w_edit-fator          = wa_edit-fator.
        w_edit-juros          = wa_edit-juros.
        wa_edit-jros          = wa_edit-juros.
        w_edit-vlr_total_ov   = wa_edit-vlr_total_ov.
        w_edit-porc_juros     = wa_edit-porc_juros.
        wa_edit-porc          = '100'.
        w_edit-porc           = '100'.
        w_edit-por            = '100'.


        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          w_edit-porc_juros = ' '.
          wa_edit-vlr_rbdo = ' '.
          w_edit-por = ' '.
          wa_edit-juros_parc = ' '.
          ind_rec_total = abap_true.
        ELSE.
          ind_rec_parc = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSEIF lv_tp_neg(1) EQ '2'. "Se insumo interno for selecionado.
*    CLEAR: D_ATRASO.
    IF p_itens-vlr_juros_calc > 0.
      IF w_zfit0026-data_pgto > w_zfit0026-data_venc.
        wa_edit-dias_atraso = ( w_zfit0026-data_pgto - w_zfit0026-data_venc ).
        w_edit-dias_atraso = wa_edit-dias_atraso.

        SELECT SINGLE *  FROM zsdt0040 INTO @DATA(wa_zsdt0040)
        WHERE doc_simulacao EQ @p_itens-simul_venda.

        wa_edit-data_venc    =    w_zfit0026-data_venc.
        wa_edit-data_pgto    =    w_zfit0026-data_pgto.
        wa_edit-fator      = ( wa_zsdt0040-juros_ano / 360 ) * wa_edit-dias_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.

          wa_edit-juros      = ( total_ov * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( total_ov * wa_zsdt0051-tx_multa ) / 100.
          DATA(to_ov)        = ( wa_edit-juros + total_ov +  wa_edit-multa ). "Total da OV + juros.

        ELSE.
          wa_edit-juros      = ( vlr_total * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( vlr_total * wa_zsdt0051-tx_multa ) / 100.
          to_ov        = ( wa_edit-juros + vlr_total +  wa_edit-multa ). "Total da OV + juros.

          wa_edit-porc_juros = ( wa_edit-juros / to_ov )  * 100. " Porcentagem proporcional ao valor do juros
          prop_multa         = ( wa_edit-multa / to_ov )  * 100. " Porcentagem proporcional da multa.

          wa_edit-juros_parc = ( wa_edit-porc_juros * w_zfit0026-mont_rbdo ) / 100.
          vlr_multa          = ( prop_multa * w_zfit0026-mont_rbdo ) / 100.
          total_ov = vlr_total.
        ENDIF.

        wa_edit-tx_jros       = wa_zsdt0040-juros_ano.
        wa_edit-vlr_total_ov  = total_ov.
        wa_edit-vlr_rbdo      = w_zfit0026-mont_rbdo.

        wa_edit-dias_ano      = '360'.
        w_edit-fator          = wa_edit-fator.
        w_edit-juros          = wa_edit-juros.
        wa_edit-jros          = wa_edit-juros.
*        WA_EDIT-MULTA         = VLR_MULT.
        w_edit-vlr_total_ov   = wa_edit-vlr_total_ov.
        w_edit-porc_juros     = wa_edit-porc_juros.
        wa_edit-porc          = '100'.
        w_edit-porc           = '100'.
        w_edit-por            = '100'.

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          w_edit-porc_juros = ' '.
          wa_edit-vlr_rbdo = ' '.
          w_edit-por = ' '.
          wa_edit-juros_parc = ' '.
          ind_rec_total = abap_true.
        ELSE.
          ind_rec_parc = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: w_zfit0026, total_ov, to_ov, vlr_total.
  ENDIF.

ENDFORM.
