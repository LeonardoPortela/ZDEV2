*----------------------------------------------------------------------*
***INCLUDE LZGFSD0004F01.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    CLASS-METHODS:

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,

      "handle_top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid.

      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,

      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no,

      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .


ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD  handle_double_click.
    PERFORM f_double_click USING e_row e_column es_row_no.
  ENDMETHOD.                    "on_user_command

*  METHOD handle_top_of_page.
*    PERFORM f_top_of_page.
*  ENDMETHOD.

  METHOD handle_data_changed.
    PERFORM f_on_data_changed USING er_data_changed.
  ENDMETHOD.

  METHOD catch_hotspot.
*    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
*    IF sy-subrc = 0.
*      IF e_column_id = 'VBELN2'.
*        SET PARAMETER ID 'AUN' FIELD wa_saida-vbeln2.
*        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
*      ENDIF.
*
*      IF e_column_id = 'DOC_SIMULACAO2'.
*        PERFORM f_call_zsdt0044.
*      ENDIF.
*
*      PERFORM refresh.
*    ENDIF.
  ENDMETHOD.
  METHOD on_data_changed_finished.

    DATA wa_stable TYPE lvc_s_stbl.

    wa_stable = VALUE #( row = abap_true col = abap_true ).

    LOOP AT gt_9000g_alv ASSIGNING FIELD-SYMBOL(<fs_saida>).

      IF NOT <fs_saida>-selec IS INITIAL.
        <fs_saida>-color = 'C510'.
      ELSE.
        <fs_saida>-color = ''.
      ENDIF.

    ENDLOOP.

    CALL METHOD go_alv_9000->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "on_data_changed_finished

  METHOD on_data_changed.

  ENDMETHOD.                    "on_data_changed

ENDCLASS.                    "lcl_'handle_events DEFINITION
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_9000
*&---------------------------------------------------------------------*
FORM f_refresh_9000 .

  CLEAR zsds084.
  CLEAR gv_ucomm_9000.
  "CLEAR go_alv_9000.
  "CLEAR go_cc_9000.
  CLEAR gt_9000g_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ALV_9000_INIT
*&---------------------------------------------------------------------*
FORM f_alv_9000_init .

  DATA lt_sort TYPE lvc_t_sort.
  DATA lt_fieldcat TYPE lvc_t_fcat.

  DATA ls_stable TYPE lvc_s_stbl.
  DATA lw_settings TYPE lvc_s_glay.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.

  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_event_handler.

  IF go_cc_9000 IS INITIAL.

    CREATE OBJECT go_cc_9000
      EXPORTING
        container_name              = 'CC_ALV_01'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc NE 0.
      MESSAGE i000(d2) WITH 'The custom control could not be created'.
      RETURN.
    ENDIF.

  ENDIF.

  lw_layout-sel_mode = 'D'.
  lw_layout-no_rowmark = abap_true.
  lw_layout-col_opt = abap_true.
  lw_layout-cwidth_opt = abap_true.
  lw_layout-no_toolbar = abap_true.
  lw_layout-zebra = 'X'.

  IF go_alv_9000 IS INITIAL.

    PERFORM f_filtra_alv_01.

    CREATE OBJECT go_alv_9000
      EXPORTING
        i_parent = go_cc_9000.

    PERFORM f_monta_fieldcat USING 'ZSDS084' CHANGING lt_fieldcat.

    PERFORM f_fieldcat_modi USING 'SELEC' 'EDIT' 'X' CHANGING lt_fieldcat.
    PERFORM f_fieldcat_modi USING 'SELEC' 'FIX_COLUMN' 'X' CHANGING lt_fieldcat.
    PERFORM f_fieldcat_modi USING 'SELEC' 'CHECKBOX' 'X' CHANGING lt_fieldcat.
    PERFORM f_fieldcat_modi USING 'SELEC' 'OUTPUTLEN' '0000003' CHANGING lt_fieldcat.
    PERFORM f_fieldcat_modi USING 'SELEC' 'DD_OUTLEN' '0000003' CHANGING lt_fieldcat.

    PERFORM f_fieldcat_modi USING 'C_TRAVA' 'CHECKBOX' 'X' CHANGING lt_fieldcat.
    PERFORM f_fieldcat_modi USING 'KWMENG' 'DO_SUM' 'X' CHANGING lt_fieldcat.

    DELETE lt_fieldcat WHERE fieldname = 'FLAG_FERT'.
    DELETE lt_fieldcat WHERE fieldname = 'FLAG_SEME'.
    DELETE lt_fieldcat WHERE fieldname = 'FLAG_DEFE'.

    DELETE lt_fieldcat WHERE fieldname = 'BSTNK'.
    DELETE lt_fieldcat WHERE fieldname = 'CHARG'.
    DELETE lt_fieldcat WHERE fieldname = 'COLOR'.
    DELETE lt_fieldcat WHERE fieldname = 'KURRF'.
    DELETE lt_fieldcat WHERE fieldname = 'LGORT'.
    DELETE lt_fieldcat WHERE fieldname = 'NETPR'.
    DELETE lt_fieldcat WHERE fieldname = 'POSNR'.
    DELETE lt_fieldcat WHERE fieldname = 'SPART'.
    DELETE lt_fieldcat WHERE fieldname = 'VALDT'.
    DELETE lt_fieldcat WHERE fieldname = 'VKGRP'.
    DELETE lt_fieldcat WHERE fieldname = 'VKORG'.
    DELETE lt_fieldcat WHERE fieldname = 'VL_ITEM_BRL'.
    DELETE lt_fieldcat WHERE fieldname = 'VL_ITEM_USD'.
    DELETE lt_fieldcat WHERE fieldname = 'VL_UNIT'.
    DELETE lt_fieldcat WHERE fieldname = 'VTWEG'.
    DELETE lt_fieldcat WHERE fieldname = 'WAERK'.
    DELETE lt_fieldcat WHERE fieldname = 'ZLSCH'.
    DELETE lt_fieldcat WHERE fieldname = 'ZTERM'.

    PERFORM f_coluna_edita2 USING 'SELEC' 'Chk' 'Chk' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'DOC_SIMULACAO' 'N°Simu' 'N°Simulação' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'VBELN' 'N° OV' 'N° OV' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'VKBUR' 'Esc.Ven' 'Esc de Ve' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'WERKS' 'Centro' 'Centro' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'AUART' 'TpOV' ' Tipo OV' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'KWMENG' 'Qtde' 'Quantidade' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'C_TRAVA' 'C/Trava' 'C/Trava' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'INCO1' 'Inco' 'Incoterms' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'EMISSOR' 'Emis.Or' 'Emissor Or' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'NAME1' 'Descr.' 'Descrição' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'MATNR' 'Material' 'Material' CHANGING lt_fieldcat.
    PERFORM f_coluna_edita2 USING 'MAKTX' 'Descr.' 'Descrição'  CHANGING lt_fieldcat.

    CALL METHOD go_alv_9000->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD go_alv_9000->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD go_alv_9000->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT lo_handle.

    SET HANDLER lcl_event_handler=>on_data_changed_finished FOR go_alv_9000.
    SET HANDLER lcl_event_handler=>on_data_changed FOR go_alv_9000.

    lw_variant-report       = sy-repid.
    lw_variant-username     = sy-uname.

    IF sy-sysid = 'DEV'.

      DO 10 TIMES.

        APPEND INITIAL LINE TO gt_9000_alv ASSIGNING FIELD-SYMBOL(<fs_teste>).
        <fs_teste>-doc_simulacao = sy-index.

      ENDDO.

    ENDIF.

    "PERFORM f_sort_table CHANGING lt_sort.

    lw_layout-info_fname = 'COLOR'.

    CALL METHOD go_alv_9000->set_table_for_first_display
      EXPORTING
        is_layout       = lw_layout
        "is_variant      = lw_variant
        i_save          = 'A'
      CHANGING
        it_outtab       = gt_9000_alv
        it_fieldcatalog = lt_fieldcat
        it_sort         = lt_sort.

  ELSE.

    PERFORM f_filtra_alv_01.
    PERFORM f_refresh_grid.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_MODI
*&---------------------------------------------------------------------*
FORM f_fieldcat_modi USING p_fieldname TYPE slis_fieldname
                           p_column TYPE c
                           p_value TYPE any
                  CHANGING p_field_cat TYPE lvc_t_fcat.

  READ TABLE p_field_cat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  DATA(lv_name) = '<FS_FCAT>-' && p_column.

  ASSIGN (lv_name) TO FIELD-SYMBOL(<fs_colum>).

  CHECK sy-subrc EQ 0.

  <fs_colum> = p_value.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat USING p_struct TYPE tabname
                   CHANGING p_field_cat TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = p_struct
    CHANGING
      ct_fieldcat            = p_field_cat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM f_double_click USING e_row TYPE lvc_s_row
                          e_column TYPE lvc_s_col
                          es_row_no TYPE lvc_s_roid.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ON_DATA_CHANGED
*&---------------------------------------------------------------------*
FORM f_on_data_changed USING io_data TYPE REF TO cl_alv_changed_data_protocol.

*  DATA lo_type TYPE REF TO cl_abap_datadescr.
*
*  DATA lv_field TYPE c LENGTH 40.
*
*  LOOP AT io_data->mt_mod_cells ASSIGNING FIELD-SYMBOL(<fs_mod_cells>).
*
*    READ TABLE gt_4000_alv_02 ASSIGNING FIELD-SYMBOL(<fs_alv>)
*     INDEX <fs_mod_cells>-row_id.
*
*    CHECK sy-subrc EQ 0.
*
*    lv_field = '<FS_ALV>-' && <fs_mod_cells>-fieldname.
*
*    ASSIGN (lv_field) TO FIELD-SYMBOL(<fs_field>).
*
*    CHECK <fs_field> IS ASSIGNED.
*
*    lo_type ?= cl_abap_typedescr=>describe_by_data( <fs_field> ).
*
*    DATA(lv_value) = <fs_mod_cells>-value.
*
*    CASE lo_type->type_kind.
*
*      WHEN 'P'.
*
*        REPLACE ',' IN lv_value WITH ''.
*        REPLACE '.' IN lv_value WITH ''.
*
*        lv_value = lv_value && '.000'.
*
*      WHEN 'D'.
*
*        CALL FUNCTION 'CONVERSION_EXIT_IDATE_INPUT'
*          EXPORTING
*            input  = <fs_mod_cells>-value
*          IMPORTING
*            output = lv_value.
*
*      WHEN OTHERS.
*    ENDCASE.
*
*    <fs_field> = lv_value.
*
*    READ TABLE gt_xnota ASSIGNING FIELD-SYMBOL(<fs_xnota>)
*      WITH KEY numero = <fs_alv>-nfenum.
*
*    CHECK sy-subrc EQ 0.
*
*    <fs_xnota>-peso_descarga = <fs_alv>-zpeso_destino.
*    <fs_xnota>-data_descarga = <fs_alv>-zdt_chegada.
*
*    PERFORM f_calculo_linha CHANGING <fs_xnota>.
*
*    PERFORM f_xnota_to_alv
*      USING <fs_xnota>
*   CHANGING <fs_alv>.
*
*  ENDLOOP.
*
*  PERFORM f_alv_refresh_grid_4000 USING space 'X'.
*  PERFORM f_atualiza_totais_4000.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
FORM f_coluna_edita2  USING p_fieldname TYPE slis_fieldname
                            p_text_s TYPE scrtext_s
                            p_text_l TYPE scrtext_l
                   CHANGING p_field_cat TYPE lvc_t_fcat.

  READ TABLE p_field_cat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-scrtext_s  = p_text_s.
  <fs_cat>-scrtext_m = p_text_s.
  <fs_cat>-scrtext_l = p_text_l.
  <fs_cat>-reptext = p_text_s.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_OV_FAT
*&---------------------------------------------------------------------*
FORM f_get_ov_fat CHANGING ct_vbeln TYPE shp_vbeln_range_t.

  SELECT 'I' AS sign, 'EQ' AS option, vbelv AS low, ' ' AS high
    FROM vbfa
      INTO TABLE @ct_vbeln
         WHERE vbtyp_v = 'C'
           AND vbtyp_n <> 'J'
           AND vbeln IN @ct_vbeln.

  SORT ct_vbeln BY low ASCENDING.

  DELETE ADJACENT DUPLICATES FROM ct_vbeln COMPARING low.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELEC_0041
*&---------------------------------------------------------------------*
FORM f_selec_0041 USING ut_vbeln TYPE shp_vbeln_range_t
               CHANGING ct_agrp TYPE zsdc084.

  SELECT zsdt0041~doc_simulacao, zsdt0041~vbeln,posnr,vkbur, vkorg, zsdt0041~werks,
         zsdt0041~auart,zsdt0041~spart,inco1,
         zsdt0041~matnr,maktx, zsdt0041~zmeng AS kwmeng,
         zsdt0041~zieme, zsdt0040~kunnr, kna1~name1 FROM zsdt0041
    INNER JOIN zsdt0040 ON zsdt0040~doc_simulacao = zsdt0041~doc_simulacao
    INNER JOIN makt ON makt~matnr = zsdt0041~matnr
    INNER JOIN kna1 ON kna1~kunnr = zsdt0040~kunnr
     INTO TABLE @DATA(lt_0041)
        WHERE zsdt0041~vbeln IN @ut_vbeln
          AND zsdt0041~vbelv_agp = @space.

  LOOP AT lt_0041 ASSIGNING FIELD-SYMBOL(<fs_0041>).

    APPEND INITIAL LINE TO ct_agrp ASSIGNING FIELD-SYMBOL(<fs_ov>).

    CASE <fs_0041>-auart.
      WHEN 'ZFTE' OR 'ZOFE'.
        <fs_ov>-flag_fert = abap_true.
      WHEN  'ZSEM' OR 'ZOSM'.
        <fs_ov>-flag_seme = abap_true.
      WHEN 'ZDEF' OR 'ZODF'.
        <fs_ov>-flag_defe = abap_true.
    ENDCASE.

    <fs_ov>-doc_simulacao = <fs_0041>-doc_simulacao.
    <fs_ov>-vbeln = <fs_0041>-vbeln.
    <fs_ov>-posnr = '000010'.
    <fs_ov>-vkbur = <fs_0041>-vkbur.
    <fs_ov>-werks = <fs_0041>-werks.
    <fs_ov>-auart = <fs_0041>-auart.
    <fs_ov>-kwmeng = <fs_0041>-kwmeng.
    <fs_ov>-vrkme = <fs_0041>-zieme.

    <fs_ov>-inco1 = <fs_0041>-inco1.
    <fs_ov>-emissor = <fs_0041>-kunnr.
    <fs_ov>-name1 = <fs_0041>-name1.
    <fs_ov>-matnr = <fs_0041>-matnr.
    <fs_ov>-maktx = <fs_0041>-maktx.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELEC_0041
*&---------------------------------------------------------------------*
FORM f_selec_0090 USING ut_vbeln TYPE shp_vbeln_range_t
               CHANGING ct_agrp TYPE zsdc084.

  SELECT zsdt0090~doc_simulacao, zsdt0090~vbeln,posnn,vkbur, vkorg, zsdt0090~werks,
         zsdt0090~auart,zsdt0090~spart,inco1,
         zsdt0090~matnr,maktx, zsdt0090~zmeng,
         zsdt0090~zieme, zsdt0090~kunnr, kna1~name1  FROM zsdt0090
    INNER JOIN makt ON makt~matnr = zsdt0090~matnr
    INNER JOIN kna1 ON kna1~kunnr = zsdt0090~kunnr
    INNER JOIN zsdt0040 ON zsdt0040~doc_simulacao = zsdt0090~doc_simulacao
    INTO TABLE @DATA(lt_desme)
      WHERE vbeln IN @ut_vbeln
        AND estorno = @space
        AND categoria <> 'H'
        AND trav_camb_utilizada = @space
        AND vbelv_agp = @space.

  SORT lt_desme BY vbeln posnn ASCENDING.

  DELETE ADJACENT DUPLICATES FROM lt_desme COMPARING vbeln posnn.

  LOOP AT lt_desme ASSIGNING FIELD-SYMBOL(<fs_0041>).

    APPEND INITIAL LINE TO ct_agrp ASSIGNING FIELD-SYMBOL(<fs_ov>).

    <fs_ov>-doc_simulacao = <fs_0041>-doc_simulacao.
    <fs_ov>-vbeln = <fs_0041>-vbeln.
    <fs_ov>-posnr = <fs_0041>-posnn.
    <fs_ov>-vkbur = <fs_0041>-vkbur.
    <fs_ov>-werks = <fs_0041>-werks.
    <fs_ov>-auart = <fs_0041>-auart.
    <fs_ov>-kwmeng = <fs_0041>-zmeng.
    <fs_ov>-vrkme = <fs_0041>-zieme.

    <fs_ov>-inco1 = <fs_0041>-inco1.
    <fs_ov>-emissor = <fs_0041>-kunnr.
    <fs_ov>-name1 = <fs_0041>-name1.
    <fs_ov>-matnr = <fs_0041>-matnr.
    <fs_ov>-maktx = <fs_0041>-maktx.
*    <fs_ov>-color

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FILTRA_ALV_01
*&---------------------------------------------------------------------*
FORM f_filtra_alv_01 .

  CLEAR gt_9000_alv[].

  gt_9000_alv[] = gt_9000g_alv[].

  CASE 'X'.
    WHEN zsds084-flag_defe.
      DELETE gt_9000_alv WHERE flag_seme IS NOT INITIAL.
      DELETE gt_9000_alv WHERE flag_fert IS NOT INITIAL.
    WHEN zsds084-flag_fert.
      DELETE gt_9000_alv WHERE flag_seme IS NOT INITIAL.
      DELETE gt_9000_alv WHERE flag_defe IS NOT INITIAL.
    WHEN zsds084-flag_seme.
      DELETE gt_9000_alv WHERE flag_defe IS NOT INITIAL.
      DELETE gt_9000_alv WHERE flag_fert IS NOT INITIAL.
  ENDCASE.

  PERFORM f_refresh_grid.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SORT_TABLE
*&---------------------------------------------------------------------*
FORM f_sort_table CHANGING ct_sort TYPE lvc_t_sort .

  DATA ls_sort TYPE lvc_s_sort.

  ls_sort-spos = '1' .
  ls_sort-fieldname = 'WERKS'.
  ls_sort-up = 'X' . "A to Z
  ls_sort-down = space .

  APPEND ls_sort TO ct_sort .

  ls_sort-spos = '2' .
  ls_sort-fieldname = 'AUART'.
  APPEND ls_sort TO ct_sort .

  ls_sort-spos = '3' .
  ls_sort-fieldname = 'INCO1'.
  APPEND ls_sort TO ct_sort .

  ls_sort-spos = '4' .
  ls_sort-fieldname = 'EMISSOR'.
  APPEND ls_sort TO ct_sort .

  ls_sort-spos = '5' .
  ls_sort-fieldname = 'MATNR'.
  "ls_sort-subtot = 'X'.
  APPEND ls_sort TO ct_sort.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICAR_CAMPOS
*&---------------------------------------------------------------------*
FORM f_verificar_campos USING uv_erro TYPE c.

  DATA ls_agrp_erro TYPE c LENGTH 6.

  DATA(lt_alv) = gt_9000_alv.

  CLEAR uv_erro.

  DELETE lt_alv WHERE selec IS INITIAL.

  IF lines( lt_alv ) = 1.

    MESSAGE 'Selecionar mais de uma linha para agrupar' TYPE 'S' DISPLAY LIKE 'E'.

    uv_erro = abap_true.

  ENDIF.

***Permitir agrupar OV do mesmo Incoterms - VBKD-INCO1
***Permitir agrupar OV do mesmo Simulador de Vendas
***Permitir agrupar somente OV que possuem trava de cambio ou somente OV sem trava de cambio

  LOOP AT lt_alv ASSIGNING FIELD-SYMBOL(<fs_agrp>).

    DATA(lv_index) = sy-tabix + 1.

    READ TABLE lt_alv ASSIGNING FIELD-SYMBOL(<fs_agrp2>) INDEX lv_index.

    CHECK sy-subrc EQ 0.

    IF <fs_agrp>-matnr <> <fs_agrp2>-matnr.

      MESSAGE 'Os materiais precisam ser iguais' TYPE 'S' DISPLAY LIKE 'E'.

      uv_erro = abap_true.

      EXIT.

    ENDIF.

    IF <fs_agrp>-werks <> <fs_agrp2>-werks.

      MESSAGE 'Os centros precisam ser iguais' TYPE 'S' DISPLAY LIKE 'E'.

      uv_erro = abap_true.

      EXIT.

    ENDIF.

    IF <fs_agrp>-emissor <> <fs_agrp2>-emissor.

      MESSAGE 'Os centros precisam ser iguais' TYPE 'S' DISPLAY LIKE 'E'.

      uv_erro = abap_true.

      EXIT.

    ENDIF.

    IF <fs_agrp>-inco1 <> <fs_agrp2>-inco1.

      MESSAGE 'Os incoterms precisam ser iguais' TYPE 'S' DISPLAY LIKE 'E'.

      uv_erro = abap_true.

      EXIT.

    ENDIF.

    IF <fs_agrp>-doc_simulacao <> <fs_agrp2>-doc_simulacao.

      MESSAGE 'Os doc. simulação precisam ser iguais' TYPE 'S' DISPLAY LIKE 'E'.

      uv_erro = abap_true.

      EXIT.

    ENDIF.

    IF <fs_agrp>-c_trava <> <fs_agrp2>-c_trava.

      MESSAGE 'Não é possível agrupar ovs com e sem travas de câmbio ao mesmo tempo' TYPE 'S' DISPLAY LIKE 'E'.

      uv_erro = abap_true.

      EXIT.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_AGRUPAR_OVS
*&---------------------------------------------------------------------*
FORM f_agrupar_ovs CHANGING uv_erro TYPE c
                            ct_ovs  TYPE zsdc084
                            ct_agr_ov TYPE zsdc084.

  DATA lv_posnr TYPE posnr.
  DATA ls_alv TYPE zsds084.

  CLEAR ct_agr_ov.

  CHECK ct_ovs[] IS NOT INITIAL.

  LOOP AT ct_ovs ASSIGNING FIELD-SYMBOL(<fs_alv>).

    " só continua se tiver escolhido no popup, ou
    " se for uma execução sem popup
    CHECK <fs_alv>-selec = abap_true OR gv_sem_popup = abap_true.

    ls_alv = <fs_alv>.

    "APPEND ls_alv TO ct_ovs.

    CLEAR: ls_alv-vbeln, ls_alv-posnr.

    IF ls_alv-vrkme = 'TO'.

      PERFORM f_converte_matnr
        USING ls_alv-matnr
              ls_alv-vrkme
              'KG'
              ls_alv-kwmeng
     CHANGING ls_alv-kwmeng.

      ls_alv-vrkme = 'KG'.

    ENDIF.

    COLLECT ls_alv INTO ct_agr_ov.

  ENDLOOP.

  " dados para criação da OV
  LOOP AT ct_agr_ov ASSIGNING FIELD-SYMBOL(<fs_agrp_ov>).

    ADD 10 TO lv_posnr.

    READ TABLE ct_ovs ASSIGNING <fs_alv> INDEX 1.

    CHECK sy-subrc EQ 0.

    <fs_agrp_ov>-posnr = lv_posnr.
    <fs_agrp_ov>-netpr = <fs_alv>-netpr.
    <fs_agrp_ov>-waerk = <fs_alv>-waerk.

    <fs_agrp_ov>-charg = <fs_alv>-charg.
    <fs_agrp_ov>-lgort = <fs_alv>-lgort.

    <fs_agrp_ov>-kurrf = <fs_agrp_ov>-vl_item_brl / <fs_agrp_ov>-vl_item_usd.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONVERTE_MATNR
*&---------------------------------------------------------------------*
FORM f_converte_matnr USING uv_matnr TYPE matnr
                            uv_meins_in TYPE meins
                            uv_meins_out TYPE meins
                            uv_menge_in TYPE kwmeng
                   CHANGING cv_menge_out TYPE kwmeng.

  DATA lv_menge_in TYPE menge_d.
  DATA lv_menge_out TYPE menge_d.

  lv_menge_in = uv_menge_in.


  CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
    EXPORTING
      i_matnr              = uv_matnr
      i_in_me              = uv_meins_in
      i_out_me             = uv_meins_out
      i_menge              = lv_menge_in
    IMPORTING
      e_menge              = lv_menge_out
    EXCEPTIONS
      error_in_application = 1
      error                = 2
      OTHERS               = 3.

  IF sy-subrc <> 0.
    lv_menge_out = uv_menge_in.
  ENDIF.

  cv_menge_out = lv_menge_out.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GERAR_OV
*&---------------------------------------------------------------------*
FORM f_gerar_ov USING uv_commit TYPE flag
                      ut_agr_ov TYPE zsdc084
             CHANGING cv_vbeln TYPE vbeln
                      cv_erro TYPE flag.

  CALL FUNCTION 'ZSDMF_INSUMOS_CRIAR_OV'
    EXPORTING
      it_itens_ov = ut_agr_ov
      iv_commit   = uv_commit
    IMPORTING
      ev_vbeln    = cv_vbeln
      ev_erro     = cv_erro.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_ATUALIZA_ZTABS
*&---------------------------------------------------------------------*
FORM f_atualiza_ztabs USING uv_vbeln TYPE vbeln
                            uv_commit TYPE flag
                            ut_agr_ov TYPE zsdc084
                            ut_ovs TYPE zsdc084
                   CHANGING cv_erro TYPE flag
                            ct_0090 TYPE zsdt0090_t
                            ct_0041 TYPE zsdt0041_t.

  DATA lr_doc_simulacao TYPE zsd_range_docsi_t.
  DATA lr_vbeln_de TYPE shp_vbeln_range_t.
  DATA lt_0090_travas TYPE zsdt0090_t.

  DATA ls_0090 TYPE zsdt0090.

  CHECK ut_ovs IS NOT INITIAL.

  LOOP AT ut_agr_ov ASSIGNING FIELD-SYMBOL(<fs_agrp>).


    ls_0090-doc_simulacao = <fs_agrp>-doc_simulacao.

    PERFORM f_get_next_seq
      USING ls_0090-doc_simulacao
           CHANGING ls_0090-sequencia.

    ls_0090-auart = <fs_agrp>-auart.
    ls_0090-vbeln = uv_vbeln.
    "ls_0090-vbelv = uv_vbeln.
    ls_0090-posnn = <fs_agrp>-posnr.
    ls_0090-spart = <fs_agrp>-spart.

    ls_0090-zmeng = <fs_agrp>-kwmeng.
    ls_0090-zieme = <fs_agrp>-vrkme.
    ls_0090-netpr  = <fs_agrp>-netpr.
    ls_0090-kmein  = <fs_agrp>-vrkme.

    ls_0090-charg  = <fs_agrp>-charg.
    ls_0090-matnr = <fs_agrp>-matnr.
    ls_0090-matkl = <fs_agrp>-maktx.

    ls_0090-inco1 = <fs_agrp>-inco1.
    ls_0090-kurrf = <fs_agrp>-kurrf.

    ls_0090-werks = <fs_agrp>-werks.
    ls_0090-kunnr = <fs_agrp>-emissor.
    ls_0090-lgort = <fs_agrp>-lgort.
    ls_0090-netwr = <fs_agrp>-netpr * ls_0090-zmeng.
    ls_0090-categoria = 'H'.

    ls_0090-cod_parc = <fs_agrp>-emissor.
    ls_0090-usnam = sy-uname.
    ls_0090-data_atual = sy-datum.
    ls_0090-hora_atual = sy-uzeit.

    APPEND ls_0090 TO ct_0090.

  ENDLOOP.

  LOOP AT ut_ovs ASSIGNING FIELD-SYMBOL(<fs_ovs>).
    APPEND 'IEQ' && <fs_ovs>-doc_simulacao TO lr_doc_simulacao.
    APPEND 'IEQ' && <fs_ovs>-vbeln TO lr_vbeln_de.
  ENDLOOP.

  CALL FUNCTION 'ZSDMF_CRIAR_TRV_CMB_COM_REF'
    EXPORTING
      ir_doc_simulacao = lr_doc_simulacao
      ir_vbeln_de      = lr_vbeln_de
      iv_vbeln_para    = uv_vbeln
      iv_com_agrp      = 'X'
    IMPORTING
      ev_erro          = cv_erro
    TABLES
      et_travas        = lt_0090_travas.

  IF lt_0090_travas IS NOT INITIAL.
    APPEND LINES OF lt_0090_travas TO ct_0090.
  ENDIF.

  CHECK cv_erro IS INITIAL.

  SELECT * FROM zsdt0041
    INTO TABLE ct_0041
      FOR ALL ENTRIES IN ut_ovs
        WHERE doc_simulacao = ut_ovs-doc_simulacao
          AND vbeln = ut_ovs-vbeln.

  LOOP AT ct_0041 ASSIGNING FIELD-SYMBOL(<fs_0041>).

    <fs_0041>-vbelv_agp = uv_vbeln.

  ENDLOOP.

  IF uv_commit = abap_true AND ct_0041 IS NOT INITIAL.
    MODIFY zsdt0041 FROM TABLE ct_0041.
  ENDIF.

  PERFORM f_upd_seq_0090 CHANGING ct_0090.

  IF uv_commit = abap_true.
    MODIFY zsdt0090 FROM TABLE ct_0090.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SEQ_TRAVA
*&---------------------------------------------------------------------*
FORM f_get_next_seq USING uv_doc_simu TYPE zsded003
                CHANGING cv_seq TYPE numc4.

  SELECT MAX( sequencia ) AS max FROM zsdt0090
    INTO @DATA(lv_max)
    WHERE doc_simulacao = @uv_doc_simu.

  cv_seq = lv_max + 1.

  UNPACK cv_seq TO cv_seq.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_bapi_confirm_process
*&---------------------------------------------------------------------*
FORM f_bapi_confirm_process USING uv_commit TYPE flag
                         CHANGING ct_return TYPE bapiret2_tt
                                  cv_erro TYPE c.

  SORT ct_return.

  READ TABLE ct_return TRANSPORTING NO FIELDS
    WITH KEY type = 'E'.

  IF sy-subrc EQ 0.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    cv_erro = abap_true.

  ELSE.

    IF uv_commit = abap_true.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

    ENDIF.

    cv_erro = abap_false.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DADOS_STD_OV
*&---------------------------------------------------------------------*
FORM f_dados_std_ov CHANGING ct_agrp TYPE zsdc084.

  CHECK ct_agrp IS NOT INITIAL.

  SELECT vbak~vbeln,vbap~posnr,vkorg,vtweg,vkbur,vkgrp,bstnk,
         vbap~waerk,zlsch,valdt,kursk,zterm, inco1,inco2,vbap~spart,
         kwmeng, vbap~netpr, vbap~netwr
     FROM vbak
    INNER JOIN vbap ON vbak~vbeln = vbap~vbeln
    INNER JOIN vbkd ON vbak~vbeln = vbkd~vbeln
                   AND vbkd~posnr = '000000'
    INTO TABLE @DATA(lt_vbak)
      FOR ALL ENTRIES IN @ct_agrp
        WHERE vbak~vbeln = @ct_agrp-vbeln
          AND vbap~posnr = @ct_agrp-posnr.

  LOOP AT lt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>).

    READ TABLE ct_agrp ASSIGNING FIELD-SYMBOL(<fs_agrp>)
      WITH KEY vbeln = <fs_vbak>-vbeln
               posnr = <fs_vbak>-posnr.

    CHECK sy-subrc EQ 0.

    <fs_agrp>-vkorg = <fs_vbak>-vkorg.
    <fs_agrp>-vtweg = <fs_vbak>-vtweg.
    <fs_agrp>-vkbur = <fs_vbak>-vkbur.
    <fs_agrp>-vkgrp = <fs_vbak>-vkgrp.
    <fs_agrp>-bstnk = <fs_vbak>-bstnk.
    <fs_agrp>-waerk = <fs_vbak>-waerk.
    <fs_agrp>-zlsch = <fs_vbak>-zlsch.
    <fs_agrp>-valdt = <fs_vbak>-valdt.
    <fs_agrp>-kurrf = <fs_vbak>-kursk.
    <fs_agrp>-zterm = <fs_vbak>-zterm.
    <fs_agrp>-inco1 = <fs_vbak>-inco1.
    <fs_agrp>-inco2 = <fs_vbak>-inco2.
    <fs_agrp>-spart = <fs_vbak>-spart.
    <fs_agrp>-netpr = <fs_vbak>-netpr.

    <fs_agrp>-vl_unit = <fs_vbak>-netwr / <fs_vbak>-kwmeng.
    <fs_agrp>-vl_item_usd = <fs_agrp>-kwmeng * <fs_agrp>-vl_unit.
    <fs_agrp>-vl_item_brl = <fs_agrp>-vl_item_usd * <fs_vbak>-kursk.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DELETE_OV
*&---------------------------------------------------------------------*
FORM f_delete_ov USING iv_vbeln TYPE vbeln
                       iv_commit TYPE flag
              CHANGING cv_erro TYPE flag
                       ct_ret TYPE bapiret2_t.

  DATA lv_text TYPE sy-ucomm.
  DATA(header_inx) = VALUE bapisdh1x( updateflag = 'D' ).

  CHECK iv_vbeln IS NOT INITIAL.

  lv_text = |Deletando O.V { iv_vbeln }|.

  PERFORM f_sap_indicator USING lv_text.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
    EXPORTING
      salesdocument    = iv_vbeln
      order_header_inx = header_inx
    TABLES
      return           = ct_ret.

  IF NOT line_exists( ct_ret[ type = 'E' ] ).

    IF iv_commit = abap_true.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

    ENDIF.

  ELSE.
    cv_erro = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
FORM f_sap_indicator USING p_text TYPE sy-ucomm.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_UPD_SEQ
*&---------------------------------------------------------------------*
FORM f_upd_seq_0090  CHANGING ct_0090 TYPE zsdt0090_t.

  DATA lv_seq TYPE numc4.

  SORT ct_0090 BY doc_simulacao sequencia ASCENDING.

  LOOP AT ct_0090 ASSIGNING FIELD-SYMBOL(<fs_0090>) WHERE estorno = abap_false.

    IF lv_seq IS INITIAL.

      lv_seq = <fs_0090>-sequencia.

    ELSE.

      ADD 1 TO lv_seq.

      <fs_0090>-sequencia = lv_seq.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_INSERE_TXT
*&---------------------------------------------------------------------*
FORM f_mensagem_insere_txt USING i_type TYPE bapi_mtype
                                 p_string TYPE string
                        CHANGING ct_ret TYPE bapiret2_t.

  DATA: lt_trtexts     TYPE trtexts,
        lw_trtexts     TYPE trtext,
        lv_texto(4000).

  DATA lv_msg1 TYPE sy-msgv1.
  DATA lv_msg2 TYPE sy-msgv1.
  DATA lv_msg3 TYPE sy-msgv1.
  DATA lv_msg4 TYPE sy-msgv1.

  lv_texto = p_string.

  CALL FUNCTION 'TR_SPLIT_TEXT'
    EXPORTING
      iv_text  = lv_texto
      iv_len   = 30
    IMPORTING
      et_lines = lt_trtexts.

  LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE sy-tabix.
      WHEN 1.
        lv_msg1 = <fs_line>.
      WHEN 2.
        lv_msg2 = <fs_line>.
      WHEN 3.
        lv_msg3 = <fs_line>.
      WHEN 4.
        lv_msg4 = <fs_line>.
    ENDCASE.

  ENDLOOP.

  PERFORM f_mensagem_insere
    TABLES ct_ret
     USING i_type
           'DS'
           '016'
           lv_msg1
           lv_msg2
           lv_msg3
           lv_msg4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_insere TABLES p_ret_tab STRUCTURE bapiret2
                        USING i_type TYPE bapi_mtype
                              i_id  TYPE  symsgid
                              i_number  TYPE  symsgno
                              i_mess_v1 TYPE any
                              i_mess_v2 TYPE any
                              i_mess_v3 TYPE any
                              i_mess_v4 TYPE any.

  APPEND INITIAL LINE TO p_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

  <fs_ret>-type = i_type.
  <fs_ret>-id = i_id.
  <fs_ret>-number = i_number.
  <fs_ret>-message_v1 = i_mess_v1.
  <fs_ret>-message_v2 = i_mess_v2.
  <fs_ret>-message_v3 = i_mess_v3.
  <fs_ret>-message_v4 = i_mess_v4.
  <fs_ret>-system = sy-sysid.

  MESSAGE ID <fs_ret>-id TYPE <fs_ret>-type NUMBER <fs_ret>-number
    WITH <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
      <fs_ret>-message_v4 INTO <fs_ret>-message.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_GRID
*&---------------------------------------------------------------------*
FORM f_refresh_grid .

  DATA lw_stable TYPE lvc_s_stbl.

  "lw_stable-row = 'X'.
  "lw_stable-col = 'X'.

  IF go_alv_9000 IS NOT INITIAL.

    CALL METHOD go_alv_9000->refresh_table_display
      EXPORTING
        is_stable = lw_stable
        "i_soft_refresh = 'X'
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ENDIF.

ENDFORM.
