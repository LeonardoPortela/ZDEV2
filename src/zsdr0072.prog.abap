**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*


**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Paulo Quevedo ( paulo.quevedo@amaggi.com.br )                        |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Cadastro de Instrução                                                     |*
**/===========================================================================\*

REPORT zsdr0072.

***********************************************************************************************
* TABELAS
***********************************************************************************************
TABLES: zsdt0051.

TYPE-POOLS: ole2.

***********************************************************************************************
* PARAMETROS DE SELEÇÃO
***********************************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_solov FOR zsdt0051-nro_sol_ov NO-EXTENSION NO INTERVALS OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

" Parâmetro 'P_INST' utilizado pelo report ZSDR0093 para add a tabela na lista.
" Não alterar
PARAMETER: p_inst TYPE char1 NO-DISPLAY.

***********************************************************************************************
* ESTRUTURAS
***********************************************************************************************
TYPES: BEGIN OF ty_0045.
TYPES: icon(4)             TYPE c.
       INCLUDE STRUCTURE zsdt0045.
TYPES: color(4)            TYPE c,
       safra_sel           TYPE lgort_d,    "*-CS2023000189-06.04.2023-#108697-JT
       status_trace        TYPE char4,      "*-CS2023000189-06.04.2023-#108697-JT
       bukrs_desc          TYPE c LENGTH 100,
       matnr_desc          TYPE maktx,
       terminal_desc       TYPE name1,
       ponto_c_desc        TYPE name1,
       cod_despac_desc     TYPE name1,
       cod_transp_desc     TYPE name1,
       terminal_estuf_desc TYPE name1,
       controladora_desc   TYPE name1,
       landx               TYPE landx,
       id_trace            TYPE numc15,
       armz_desc           TYPE name1,
       linha_edit          TYPE i,
*** Inicio - Rubenilson Pereira - 12.02.25 - US164130
       desc_local_entrega  TYPE name1,
       desc_oper_log       TYPE name1.
*** Fim - Rubenilson Pereira - 12.02.25 - US164130
TYPES END OF ty_0045.

TYPES: BEGIN OF ty_45.
         INCLUDE STRUCTURE zsdt0045.
TYPES:
         id_trace            TYPE numc15,
         qtd_fardos_vinc     TYPE int4,
         saldo_fardos        TYPE int4,
         qtd_peso_vinc       TYPE dzmeng,
         saldo_peso          TYPE dzmeng,
         terminal_desc       TYPE name1,
         ponto_c_desc        TYPE name1,
         cod_despac_desc     TYPE name1,
         cod_transp_desc     TYPE name1,
         terminal_estuf_desc TYPE name1,
         controladora_desc   TYPE name1,
         armz_desc           TYPE name1,
*** Inicio - Rubenilson Pereira - 12.02.25 - US164130
         desc_local_entrega  TYPE name1,
         desc_oper_log       TYPE name1.
*** Fim - Rubenilson Pereira - 12.02.25 - US164130
TYPES END OF ty_45.

TYPES: BEGIN OF ty_0051.
         INCLUDE TYPE zsdt0051.
TYPES:   %nro_sol_ov TYPE zsdt0045-objek,
         %bstkd      TYPE zsdt0045-contrato.
TYPES END OF  ty_0051.

TYPES: BEGIN OF ty_deposito,
         matnr TYPE mchb-matnr,
         werks TYPE mchb-werks,
         lgort TYPE mchb-lgort,
         clabs TYPE mchb-clabs,
         cumlm TYPE mchb-cumlm,
         cinsm TYPE mchb-cinsm,
         ceinm TYPE mchb-ceinm,
         cspem TYPE mchb-cspem,
         cretm TYPE mchb-cretm,
         cvmla TYPE mchb-cvmla,
         cvmum TYPE mchb-cvmum,
         cvmin TYPE mchb-cvmin,
         cvmei TYPE mchb-cvmei,
         cvmsp TYPE mchb-cvmsp,
         cvmre TYPE mchb-cvmre,
         saldo TYPE mchb-clabs,
       END OF ty_deposito.

TYPES: BEGIN OF ty_deposito_2,
         matnr TYPE mslb-matnr,
         werks TYPE mslb-werks,
         charg TYPE mslb-charg,
         sobkz TYPE mslb-sobkz,
         lifnr TYPE mslb-lifnr,
         lblab TYPE mslb-lblab,
         lbins TYPE mslb-lbins,
         lbvla TYPE mslb-lbvla,
         lbvin TYPE mslb-lbvin,
         saldo TYPE mslb-lblab,
       END OF ty_deposito_2.

TYPES:
  BEGIN OF ty_editor,
    line(255),
  END OF ty_editor.

TYPES: BEGIN OF ty_colum_excel,
         name(100) TYPE c,
       END OF ty_colum_excel.

TYPES: BEGIN OF ty_campos,
         bukrs TYPE bukrs,
         field TYPE bstnk,
         nome  TYPE char80.
TYPES: END   OF ty_campos.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

***********************************************************************************************
* DECLARAÇÕES GLOBAIS
***********************************************************************************************
DATA: it_0045         TYPE TABLE OF ty_0045 WITH HEADER LINE,
      wa_0045         TYPE ty_0045,
      tg_0045         TYPE TABLE OF zsdt0045,
      wg_0045         TYPE zsdt0045,
      it_0045_temp    TYPE TABLE OF ty_0045 WITH HEADER LINE,
      wa_0045_temp    TYPE ty_0045,
      it_0045_sel     TYPE TABLE OF ty_0045 WITH HEADER LINE,
      wa_0045_sel     TYPE ty_0045,
      it_import       TYPE TABLE OF ty_0045 WITH HEADER LINE,
      wa_import       TYPE ty_0045,
      it_0051         TYPE TABLE OF ty_0051 WITH HEADER LINE,
      it_0327         TYPE TABLE OF zsdt0327,
      wa_0327         TYPE zsdt0327,
*
      it_new          TYPE TABLE OF ty_0045 WITH HEADER LINE,
      it_new_aux      TYPE zsdt0045,
      wa              TYPE ty_0045,
      wa_save         TYPE zsdt0045,
      it_save         TYPE STANDARD TABLE OF zsdt0045 WITH DEFAULT KEY,
      it_edit         TYPE STANDARD TABLE OF zsdt0045 WITH DEFAULT KEY,
      it_dele         TYPE STANDARD TABLE OF zsdt0045 WITH DEFAULT KEY,
      it_0045_old     TYPE STANDARD TABLE OF zsdt0045 WITH DEFAULT KEY,
      it_0182         TYPE TABLE OF zsdt0182,
      it_0182_d       TYPE TABLE OF zsdt0182,
      t_0045_old      TYPE TABLE OF zsdt0045,
      it_instrucao    TYPE TABLE OF ty_45,
      wa_instrucao    TYPE ty_0045,
      wa_0045_old     TYPE zsdt0045,
      tg_save_log     TYPE TABLE OF zsdt0083,
      it_log_old      TYPE STANDARD TABLE OF zsdt0045 WITH DEFAULT KEY,
      obj_alv         TYPE REF TO cl_gui_alv_grid,
      obj_cont        TYPE REF TO cl_gui_custom_container,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
      wa_variant      TYPE disvariant,
      wa_layout       TYPE lvc_s_layo,
      wa_stable       TYPE lvc_s_stbl,
      str             TYPE REF TO data,
      t_campos        TYPE TABLE OF ty_campos,
      w_campos        TYPE ty_campos,
      it_fcat         TYPE lvc_t_fcat,
      wa_fcat         TYPE lvc_s_fcat,
      tg_msg_ret      TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      wg_mensagem(30),
      saldo           TYPE TABLE OF ty_deposito,
      tmchb           TYPE TABLE OF ty_deposito,
      tmslb           TYPE TABLE OF ty_deposito_2,
      saldo2          TYPE TABLE OF ty_deposito_2,
      l_tabix         TYPE sy-tabix,
      lv_acao         TYPE sy-ucomm,
      lv_acao2        TYPE sy-ucomm,
      lv_importa      TYPE char1,
      git_fieldnames  TYPE TABLE OF zst_zsdr0072_excel,
      git_colum_excel TYPE TABLE OF ty_colum_excel.

DATA: cont       TYPE n LENGTH 3,
      str_l(255) TYPE c,
      campo      TYPE char80,
      tam        TYPE i,
      gvr_check.

DATA: r_seq TYPE RANGE OF sy-ucomm.

DATA: obj_custom_txt    TYPE REF TO cl_gui_custom_container,
      obj_custom_editor TYPE REF TO cl_gui_textedit,
      gt_editor         TYPE TABLE OF ty_editor,
      gt_email          TYPE TABLE OF ty_editor,
      gs_editor         TYPE ty_editor.

FIELD-SYMBOLS: <fs_campo>  TYPE any,
               <fs_campo1> TYPE any.

DATA: tg_instrucao TYPE TABLE OF zsdt0045,
      w_instrucao  TYPE zsdt0045,
      l_set_upd    TYPE string.

DATA : lv_filename TYPE string,
       lv_path     TYPE string,
       lv_fullpath TYPE string,
       lv_file     TYPE string.

DATA : lr_excel_structure      TYPE REF TO data,
       lv_content              TYPE xstring,
       ls_stream               TYPE /iwbep/if_mgw_core_srv_runtime=>ty_s_media_resource,
       lo_table_row_descriptor TYPE REF TO cl_abap_structdescr,
       lo_source_table_descr   TYPE REF TO cl_abap_tabledescr,
       ls_header               TYPE ihttpnvp.

"file download
DATA : lt_binary_tab TYPE TABLE OF sdokcntasc,
       lv_length     TYPE i.

DATA: it_fieldcat  TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      wa_fieldcat  TYPE ty_estrutura.

DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.

DEFINE m_message.
  CASE sy-subrc.
    WHEN 0.
    WHEN 1.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    WHEN OTHERS.
  ENDCASE.
END-OF-DEFINITION.

CONSTANTS:  gc_esc              VALUE '"'.

*CLASS lcl_alv_toolbar DEFINITION.
*  PUBLIC SECTION.
*
*    METHODS:
**      constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
*      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
*      on_data_ch FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,
*      on_data_ch_f FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.
*
*ENDCLASS.

*DATA: obg_toolbar TYPE REF TO lcl_alv_toolbar.

CLASS zcl_instrucao DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_alsmex_tabline,
             row   TYPE kcd_ex_row_n,
             col   TYPE kcd_ex_col_n,
             value TYPE char8000,
           END OF ty_alsmex_tabline.

    TYPES: BEGIN OF ty_s_senderline,
             line(5120) TYPE c,
           END OF ty_s_senderline,
           ty_t_sender TYPE ty_s_senderline.

    DATA: it_dados TYPE TABLE OF char8000.

    DATA: excel_tab    TYPE TABLE OF ty_t_sender.

    DATA: at_hist      TYPE zsded032,
          at_limit_qtd TYPE dzmeng,
          at_inst_qtd  TYPE gsgew,
          it_index     TYPE lvc_t_row,
          it_frete     TYPE TABLE OF zsdt0045,
          it_excel     TYPE TABLE OF ty_alsmex_tabline,
          it_aux       TYPE TABLE OF ty_alsmex_tabline WITH DEFAULT KEY,
          cont_col     TYPE kcd_ex_col_n,
          t_excel      TYPE hrcnex_tab.

    METHODS:

*-CS2023000189-06.04.2023-#108697-JT-inicio
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,
*-CS2023000189-06.04.2023-#108697-JT-fim

    on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
    handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
    on_data_ch FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,
    on_data_ch_f FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells,

    get_dados RETURNING VALUE(return) TYPE char1,
    get_header IMPORTING nro_sol_ov   TYPE zsded013 OPTIONAL
    bstkd        TYPE bstkd OPTIONAL RETURNING VALUE(value) TYPE zsdt0051,
    get_zseq_inst IMPORTING i_value        TYPE nrobj OPTIONAL
    l_0045         TYPE ty_0045 OPTIONAL RETURNING VALUE(r_value) TYPE zseq_inst,
    monta_log,
    set_log,
    input_log
    IMPORTING value1 TYPE lvc_fname OPTIONAL
    value2 TYPE char20    OPTIONAL
    value3 TYPE sy-tabix  OPTIONAL,
    f4_bstkd
    RETURNING VALUE(r_value) TYPE bstkd,
    f4_solov
    RETURNING VALUE(r_value) TYPE zsded013,
    set_limit_qtd,
    get_limit_qtd
    RETURNING VALUE(r_value) TYPE dzmeng,
    set_inst_qtd,
    get_inst_qtd
    RETURNING VALUE(r_value) TYPE gsgew,
    set_desc,
    get_desc_emp
    IMPORTING input TYPE any OPTIONAL
    RETURNING VALUE(value) TYPE string,
    get_desc_cen
    IMPORTING input TYPE any OPTIONAL
    RETURNING VALUE(value) TYPE string,
    get_desc_mat
    IMPORTING input TYPE any OPTIONAL
    RETURNING VALUE(value) TYPE string,
    get_desc_ter
    IMPORTING input TYPE any OPTIONAL
    RETURNING VALUE(value) TYPE string,
    get_desc_armz
    IMPORTING input TYPE any OPTIONAL
    RETURNING VALUE(value) TYPE string,
    get_desc_pon
    IMPORTING input TYPE any OPTIONAL
    RETURNING VALUE(value) TYPE string,
    get_desc_pais
    IMPORTING input TYPE any OPTIONAL
    RETURNING VALUE(value) TYPE string,
*** Inicio - Rubenilson Pereira - 12.02.25 - US164130
    get_desc_local_entrega
    IMPORTING input TYPE any OPTIONAL
    RETURNING VALUE(value) TYPE string,
    get_desc_oper_log
    IMPORTING input TYPE any OPTIONAL
    RETURNING VALUE(value) TYPE string,
*** Fim - Rubenilson Pereira - 12.02.25 - US164130
    get_limite_peso
    IMPORTING input TYPE any OPTIONAL
    RETURNING VALUE(value) TYPE string,
    get_peso_max
    IMPORTING input TYPE any OPTIONAL
    RETURNING VALUE(value) TYPE string,
    get_instrucao
    IMPORTING input TYPE any OPTIONAL
              safra TYPE any OPTIONAL  "*-CS2023000189-06.04.2023-#108697-JT
    RETURNING VALUE(value) TYPE zsdt0045,
    set_layout,
    show_msgre,
    set_erros,
    save,
*-CS2021000532-#59109-08.08.2022-JT-inicio
    set_valida_item
    IMPORTING i_valida TYPE char1 OPTIONAL
    CHANGING wa TYPE  ty_0045,
    set_update_table
    IMPORTING field TYPE char100
    RETURNING VALUE(value) TYPE string,
    set_campos_obrigatorios
    IMPORTING i_tipo TYPE char1 OPTIONAL
              i_linha TYPE i OPTIONAL,
*-CS2021000532-#59109-08.08.2022-JT-fim
    act_new_inst,
    act_edit_inst,
    act_dele_inst,
    act_reenvia_trace,  "*-CS2023000189-06.04.2023-#108697-JT
    cotacao_frete,
    get_index
    IMPORTING input TYPE sy-ucomm OPTIONAL RETURNING VALUE(r_value) TYPE sy-tabix,
    confirm
    IMPORTING input TYPE any      OPTIONAL RETURNING VALUE(r_value) TYPE char1,
    remove_zero
    IMPORTING input TYPE any      OPTIONAL RETURNING VALUE(r_value) TYPE numc10,
    modify_alv,
    get_excel
    RETURNING VALUE(r_value) TYPE rlgrap-filename,
    set_excel
    IMPORTING input TYPE char128,
    exc_excel,
    exc_layout_excel,
    get_mard
    IMPORTING input TYPE ty_0045
    RETURNING VALUE(return) TYPE sy-subrc,
    col
    RETURNING VALUE(r_value) TYPE kcd_ex_col_n,
    chartonum
    IMPORTING input TYPE char8000 RETURNING VALUE(r_value) TYPE zfi_dec15, "/bev3/chdec17_5,
    chartodate
    IMPORTING input TYPE char8000 RETURNING VALUE(r_value) TYPE sy-datum,
    en_de_queue
    IMPORTING input        TYPE any
    dir          TYPE any
    RETURNING VALUE(subrc) TYPE sy-subrc,
    check_vbeln
    IMPORTING input        TYPE ty_0045
    RETURNING VALUE(subrc) TYPE sy-subrc,
    check_botao
    IMPORTING input TYPE sy-ucomm OPTIONAL,
    check_0182
    IMPORTING input TYPE zseq_inst
    RETURNING VALUE(return) TYPE sy-subrc,
    get_saldo_fardos
    IMPORTING input TYPE zseq_inst
    RETURNING VALUE(return) TYPE int4,
    get_saldo_peso
    IMPORTING input TYPE zseq_inst
    RETURNING VALUE(return) TYPE dmbtr,
    valida_existe_lfa1
    IMPORTING cod_lifnr TYPE lifnr
    RETURNING VALUE(lifnr_existente) TYPE abap_bool,
    clear.

  PRIVATE SECTION.

    DATA: ld_separator TYPE c,
          application  TYPE ole2_object,
          workbook     TYPE ole2_object,
          range        TYPE ole2_object,
          worksheet    TYPE ole2_object,
          h_cell       TYPE ole2_object,
          h_cell1      TYPE ole2_object,
          ld_rc        TYPE i.

ENDCLASS.

DATA(obj_inst) = NEW zcl_instrucao( ).

***********************************************************************************************
* SELECTION-SCREEN VALUE-REQUEST
***********************************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_solov-low.
  s_solov-low = NEW zcl_instrucao( )->f4_solov( ).
***********************************************************************************************


*CLASS lcl_alv_toolbar IMPLEMENTATION.
*
*  METHOD on_toolbar.
*
*    CHECK it_0051-vkorg EQ '0001'.
*
*    APPEND VALUE #(                   function = '&&SEP04' butn_type = 3 ) TO e_object->mt_toolbar.
*    APPEND VALUE #( icon = '@B_COPY@' function = 'COPY'    butn_type = 0 quickinfo = 'Copiar' text = 'Copiar' ) TO e_object->mt_toolbar.
*    APPEND VALUE #(                   function = '&&SEP04' butn_type = 3 ) TO e_object->mt_toolbar.
*
*  ENDMETHOD.                    "ON_TOOLBAR
*
*  METHOD handle_user_command.
*
*    CASE e_ucomm.
*      WHEN 'COPY'.
*
*        CALL METHOD obj_alv->get_selected_cells
*          IMPORTING
*            et_cell = tg_selectedcell.
*
*        LOOP AT tg_selectedcell INTO wg_selectedcell.
*          READ TABLE it_0045 INTO DATA(wa_0045) INDEX wg_selectedcell-row_id-index.
*          IF sy-subrc IS INITIAL.
*            CLEAR: wa_0045-zseq_inst, wa_0045-charg, wa_0045-matnr, wa_0045-ponto_c, wa_0045-quantidade, wa_0045-btgew.
*            APPEND wa_0045 TO it_0045.
*          ENDIF.
*        ENDLOOP.
*
*    ENDCASE.
*
*  ENDMETHOD.
*
*  METHOD on_data_ch.
*    BREAK-POINT.
**    data_changed
**    er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm
*  ENDMETHOD.
*
*  METHOD on_data_ch_f.
*    BREAK-POINT.
**    e_modified et_good_cells
*  ENDMETHOD.
*
*ENDCLASS.


CLASS zcl_instrucao IMPLEMENTATION.

  METHOD catch_hotspot.

    READ TABLE it_0045 INTO wa_0045 INDEX  e_row_id-index.

*-CS2023000189-06.04.2023-#108697-JT-inicio
    CASE e_column_id.
      WHEN 'STATUS_TRACE'.
        PERFORM f_exibe_log_trace USING wa_0045-zseq_inst
                                        wa_0045-objek
                                        wa_0045-objecttable.
*-CS2023000189-06.04.2023-#108697-JT-fim
    ENDCASE.

  ENDMETHOD.

  METHOD on_toolbar.

*-CS2023000189-06.04.2023-#108697-JT-inicio
    APPEND VALUE #(                   function = '&&SEP04' butn_type = 3 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = '@42@'     function = 'REFRESH' butn_type = 0 quickinfo = 'Atualizar' text = 'Atualizar' ) TO e_object->mt_toolbar.
    APPEND VALUE #(                   function = '&&SEP04' butn_type = 3 ) TO e_object->mt_toolbar.
*-CS2023000189-06.04.2023-#108697-JT-fim

    CHECK it_0051-vkorg EQ '0001'.

    APPEND VALUE #(                   function = '&&SEP04' butn_type = 3 ) TO e_object->mt_toolbar.
    APPEND VALUE #( icon = '@B_COPY@' function = 'COPY'    butn_type = 0 quickinfo = 'Copiar' text = 'Copiar' ) TO e_object->mt_toolbar.
    APPEND VALUE #(                   function = '&&SEP04' butn_type = 3 ) TO e_object->mt_toolbar.

  ENDMETHOD.                    "ON_TOOLBAR

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'COPY'.

        DATA(index) = obj_inst->get_index( ).

        READ TABLE it_0045 INTO DATA(wa_0045) INDEX index.
        IF sy-subrc IS INITIAL.
          CLEAR: wa_0045-zseq_inst, wa_0045-charg, wa_0045-matnr, wa_0045-ponto_c, wa_0045-quantidade, wa_0045-btgew.
          APPEND wa_0045 TO it_0045.

          CALL METHOD obj_alv->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ENDIF.

*-CS2023000189-06.04.2023-#108697-JT-inicio
      WHEN 'REFRESH'.
        obj_inst->get_dados( ).

        CALL METHOD obj_alv->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
*-CS2023000189-06.04.2023-#108697-JT-fim

    ENDCASE.

  ENDMETHOD.

  METHOD on_data_ch.
    BREAK-POINT.
*    data_changed
*    er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm
  ENDMETHOD.

  METHOD on_data_ch_f.
    BREAK-POINT.
*    e_modified et_good_cells
  ENDMETHOD.

  METHOD get_dados.
    me->clear( ).

    FREE lv_importa.

    MOVE-CORRESPONDING obj_inst->get_header( nro_sol_ov = s_solov-low ) TO it_0051.

    IF it_0051-param_espec NE 'A' AND
       it_0051-param_espec NE 'X' AND
       it_0051-param_espec NE 'Z'  .
      MESSAGE 'Solic. de Venda informada não é de Algodão!' TYPE 'S' DISPLAY LIKE 'E'.
      return = abap_true.
      EXIT.
    ENDIF.

    it_0051-%nro_sol_ov = it_0051-nro_sol_ov.
    it_0051-%bstkd      = it_0051-bstkd.

    SELECT *
      FROM zsdt0045
      INTO CORRESPONDING FIELDS OF TABLE it_0045
      WHERE objek EQ it_0051-%nro_sol_ov
     AND contrato EQ it_0051-%bstkd.

*-CS2023000189-06.04.2023-#108697-JT-inicio
    IF it_0045[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0327
        INTO TABLE it_0327
         FOR ALL ENTRIES IN it_0045
       WHERE zseq_inst   = it_0045-zseq_inst
         AND objek       = it_0045-objek
         AND objecttable = it_0045-objecttable.
    ENDIF.

    SORT it_0327 BY zseq_inst objek objecttable seq DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_0327
                     COMPARING zseq_inst objek objecttable.


    LOOP AT it_0045 INTO wa_0045.
      l_tabix = sy-tabix.

      READ TABLE it_0327 INTO wa_0327 WITH KEY zseq_inst   = wa_0045-zseq_inst
                                               objek       = wa_0045-objek
                                               objecttable = wa_0045-objecttable.
      IF sy-subrc <> 0.
        wa_0045-status_trace = icon_dummy.
      ELSEIF wa_0327-tipo_msg = 'E'.
        wa_0045-status_trace = icon_alert. "ICON_FAILURE
      ELSEIF wa_0327-tipo_msg = 'S'.
        wa_0045-status_trace = icon_checked.
      ELSE.
        wa_0045-status_trace = icon_dummy.
      ENDIF.

      MODIFY it_0045 FROM wa_0045 INDEX l_tabix TRANSPORTING status_trace.
    ENDLOOP.
*-CS2023000189-06.04.2023-#108697-JT-fim

*-CS2021000532-#59109-08.08.2022-JT-inicio
    it_0045_sel[] = it_0045[].
*-CS2021000532-#59109-08.08.2022-JT-fim

    MOVE: it_0051-nro_sol_ov TO it_new-objek,
          'ZSDT0051'         TO it_new-objecttable,
          it_0051-bstkd      TO it_new-contrato,
          'N'                TO it_new-limite_peso,
          sy-datum           TO it_new-data_criacao,
          sy-uname           TO it_new-usuario.

    IF p_inst IS NOT INITIAL.
      IMPORT _instrucao TO it_instrucao FROM MEMORY ID 'ZSENDINS'.
      IF sy-subrc = 0.
        lv_importa = abap_true.
      ENDIF.

      FREE MEMORY ID 'ZSENDINS'.
      LOOP AT it_instrucao INTO DATA(wa_ins).
        MOVE-CORRESPONDING wa_ins TO wa_instrucao.

        wa_instrucao-objek = it_0051-nro_sol_ov.
        wa_instrucao-status = abap_false.
        wa_instrucao-quantidade = wa_ins-saldo_fardos.
        wa_instrucao-btgew = wa_ins-saldo_peso.

        APPEND wa_instrucao TO it_0045.
      ENDLOOP.
      lv_acao = 'INSTRUCAO'.
    ENDIF.

    obj_inst->set_desc( ).
    obj_inst->monta_log( ).
    obj_inst->set_limit_qtd( ).

  ENDMETHOD.

  METHOD get_header.
    SELECT SINGLE * FROM zsdt0051 INTO value WHERE nro_sol_ov EQ nro_sol_ov AND param_espec IN ('A','X','Z') .
  ENDMETHOD.

  METHOD get_zseq_inst.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = i_value
      IMPORTING
        number      = r_value.

    at_hist = r_value.

    CHECK l_0045-id_trace IS NOT INITIAL.

    IF p_inst IS NOT INITIAL.
      APPEND VALUE #(
                        id_trace = l_0045-id_trace
                        id_inst = r_value
                        qtd_fardos = l_0045-quantidade
                        peso = l_0045-btgew
                    ) TO it_0182.
    ENDIF.

  ENDMETHOD.

  METHOD monta_log.
    it_0045_old[] = CORRESPONDING #( it_0045[] ).
  ENDMETHOD.

  METHOD set_log.

    DATA: tabix TYPE sy-tabix.

    LOOP AT it_0045 INTO wa_0045.

      tabix = sy-tabix.
      CLEAR wa_0045_old.

      TRY .
          wa_0045_old = it_0045_old[ tabix ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      LOOP AT it_fcat INTO DATA(wa_fcat).
        obj_inst->input_log( EXPORTING value1 = |WA_0045-{ wa_fcat-fieldname }|
                                       value2 = 'Instrução'
                                       value3 = tabix
                                      ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD input_log.

    DATA: wl_field(30),
          wl_field_old(40),
          wl_field_aux(40),
          wl_field_aux2(40).

    FIELD-SYMBOLS: <fs_field>     TYPE any,
                   <fs_field_old> TYPE any.

    UNASSIGN <fs_field>.
    UNASSIGN <fs_field_old>.

    wl_field = value1.
    SPLIT wl_field AT '-' INTO wl_field_aux
                               wl_field_aux2.

    wl_field_old = |{ wl_field_aux }_OLD-{ wl_field_aux2 }| .

    ASSIGN (wl_field) TO <fs_field>.
    ASSIGN (wl_field_old) TO <fs_field_old>.
    IF <fs_field> IS ASSIGNED AND <fs_field_old> IS ASSIGNED.
      IF <fs_field> NE <fs_field_old>.
        IF  obj_inst->at_hist IS INITIAL.
          obj_inst->get_zseq_inst( EXPORTING i_value = 'ZHISTORIC').
        ENDIF.
        SPLIT value1 AT '-' INTO wl_field
                                 wl_field_aux.

        APPEND VALUE #( nro_sol_ov   = s_solov-low
                        linha        = value3
                        id_historico = obj_inst->at_hist
                        area         = value2
                        campo        = wl_field_aux
                        new_value    = <fs_field>
                        old_value    = <fs_field_old>
                        usnam        = sy-uname
                        data_atual   = sy-datum
                        hora_atual   = sy-uzeit
                     ) TO tg_save_log.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD f4_bstkd.

    DATA: it_return TYPE TABLE OF ddshretval,
          tl_dselc  TYPE TABLE OF dselc.

    TYPES: BEGIN OF ty_bstkd,
             contrato TYPE zsdt0143-contrato,
             safra    TYPE zsdt0143-safra,
             empresa  TYPE zsdt0143-empresa,
           END OF ty_bstkd.

    DATA: it_bstkd TYPE TABLE OF ty_bstkd.

    SELECT contrato safra empresa
      FROM zsdt0143
      INTO TABLE it_bstkd
    WHERE cancelado EQ abap_false.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'CONTRATO'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        value_org       = 'S'
      TABLES
        value_tab       = it_bstkd
        return_tab      = it_return
        dynpfld_mapping = tl_dselc.

    TRY .
        r_value = it_return[ 1 ]-fieldval.
      CATCH cx_sy_itab_line_not_found.
        CLEAR r_value.
    ENDTRY.

  ENDMETHOD.

  METHOD f4_solov.

    DATA: it_return TYPE TABLE OF ddshretval,
          tl_dselc  TYPE TABLE OF dselc.

    TYPES: BEGIN OF ty_solov,
             nro_sol_ov TYPE zsdt0051-nro_sol_ov,
             vkbur      TYPE zsdt0051-vkbur,
             auart      TYPE zsdt0051-auart,
             inco1      TYPE zsdt0051-inco1,
             matnr      TYPE zsdt0051-matnr,
           END OF ty_solov.

    DATA: it_solov TYPE  TABLE OF ty_solov.

    SELECT nro_sol_ov vkbur auart inco1 matnr
      FROM zsdt0051
      INTO TABLE it_solov
      WHERE param_espec IN ('A','X')
*      AND tp_venda EQ '14'
      ORDER BY nro_sol_ov DESCENDING.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'NRO_SOL_OV'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        value_org       = 'S'
      TABLES
        value_tab       = it_solov
        return_tab      = it_return
        dynpfld_mapping = tl_dselc.

    TRY .
        r_value = it_return[ 1 ]-fieldval.
      CATCH cx_sy_itab_line_not_found.
        CLEAR r_value.
    ENDTRY.

  ENDMETHOD.

  METHOD set_limit_qtd.

    CLEAR at_limit_qtd.

    SELECT *
        FROM zsdt0143
        INTO TABLE @DATA(it_0143)
        WHERE contrato EQ @it_0051-bstkd
          AND cancelado EQ @abap_false.

    IF NOT sy-subrc IS INITIAL.
      FREE it_0143.
    ELSE.
      LOOP AT it_0143 INTO DATA(wa_0143).
        wa_0143-quatidade = ( wa_0143-quatidade * ( ( wa_0143-tolerancia / 100 ) + 1 ) ).
        ADD wa_0143-quatidade TO obj_inst->at_limit_qtd.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD get_limit_qtd.
    r_value = obj_inst->at_limit_qtd.
  ENDMETHOD.

  METHOD set_inst_qtd.

    DATA l_error  TYPE c.

    SELECT *
    FROM zsdt0045
    INTO TABLE @DATA(it_soma)
    WHERE contrato EQ @it_0051-bstkd
    AND objek NE @s_solov-low.

    it_soma[] = CORRESPONDING #( it_0045[] ).

*    APPEND LINES OF IT_0045[] TO IT_SOMA[].

    IF NOT it_new[] IS INITIAL.
      it_soma[] = CORRESPONDING #( it_new[] ).
*      APPEND CORRESPONDING ZSDT0045( IT_NEW[] ) TO IT_SOMA.
    ENDIF.

*-CS2023000189-06.04.2023-#108697-JT-inicio
    TRY.
        obj_inst->at_inst_qtd = REDUCE #( INIT x = 0 FOR wa IN it_soma NEXT x = x + wa-btgew ).
      CATCH cx_sy_arithmetic_overflow INTO DATA(ex_error).
        l_error = abap_true.
    ENDTRY.

    IF l_error = abap_true.
      APPEND VALUE #(
              field = 'IT_NEW-BTGEW'
              msg = |Total Peso muito alto! Revise!|
             ) TO tg_msg_ret.
    ENDIF.
*-CS2023000189-06.04.2023-#108697-JT-fim

  ENDMETHOD.

  METHOD get_inst_qtd.
    r_value = obj_inst->at_inst_qtd.
  ENDMETHOD.

  METHOD get_desc_emp.
    SELECT SINGLE butxt FROM t001 INTO value WHERE bukrs EQ input.
  ENDMETHOD.

  METHOD get_desc_cen.
    SELECT SINGLE name1 FROM t001w INTO value WHERE werks EQ input.
  ENDMETHOD.

  METHOD get_desc_mat.
    SELECT SINGLE maktx FROM makt INTO value WHERE matnr EQ input AND spras EQ sy-langu.
  ENDMETHOD.

  METHOD get_desc_ter.
    SELECT SINGLE name1 FROM lfa1 INTO value WHERE lifnr EQ input.
  ENDMETHOD.

  METHOD get_desc_armz.
    SELECT SINGLE name1 FROM lfa1 INTO value WHERE lifnr EQ input.
  ENDMETHOD.

  METHOD get_desc_pon.
    SELECT SINGLE name1 FROM lfa1 INTO value WHERE lifnr EQ input.
  ENDMETHOD.

  METHOD get_desc_pais.
    SELECT SINGLE landx FROM t005t INTO value WHERE land1 EQ input.
  ENDMETHOD.

*** Inicio - Rubenilson Pereira - 12.02.25 - US164130
  METHOD get_desc_local_entrega.
    DATA: lv_kunnr TYPE kna1-kunnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = input
      IMPORTING
        output = lv_kunnr.

    SELECT SINGLE name1 FROM kna1 INTO value WHERE kunnr EQ lv_kunnr.
  ENDMETHOD.

  METHOD get_desc_oper_log.
    DATA: lv_lifnr TYPE lfa1-lifnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = input
      IMPORTING
        output = lv_lifnr.

    SELECT SINGLE name1 FROM lfa1 INTO value WHERE lifnr EQ lv_lifnr.
  ENDMETHOD.
*** Fim - Rubenilson Pereira - 12.02.25 - US164130

  METHOD get_limite_peso.
    CHECK input IS NOT INITIAL.
    SELECT SINGLE limite_peso
      FROM zsdt0045
      INTO value
      WHERE instrucao EQ input.
  ENDMETHOD.

  METHOD get_peso_max.
    CHECK input IS NOT INITIAL.
    SELECT SINGLE peso_max
      FROM zsdt0045
      INTO @DATA(vl_value)
      WHERE instrucao EQ @input.
    value = vl_value.
  ENDMETHOD.

  METHOD get_instrucao.

    DATA: r_instr  TYPE RANGE OF zsdt0045-instrucao,
          rr_instr LIKE LINE OF r_instr.

    CHECK input IS NOT INITIAL.

    rr_instr-sign   = 'I'.
    rr_instr-option = 'CP'.
    rr_instr-low    = input.
    APPEND rr_instr TO r_instr.
    CLEAR rr_instr.

    SELECT SINGLE * FROM zsdt0045 INTO  @DATA(vl_value)
      WHERE instrucao IN @r_instr
        AND safra      = @safra.  "*-CS2023000189-06.04.2023-#108697-JT

*   IF sy-subrc <> 0.  "*-CS2023000189-06.04.2023-#108697-JT
*     CLEAR vl_value.
*   ENDIF.

    value = vl_value.
  ENDMETHOD.


  METHOD set_desc.

    it_new-bukrs_desc    = |{ obj_inst->get_desc_emp( it_new-bukrs ) }/{ obj_inst->get_desc_cen( it_new-werks ) }|.
    it_new-matnr_desc    = obj_inst->get_desc_mat( it_new-matnr ).
    it_new-terminal_desc = obj_inst->get_desc_ter( it_new-terminal ).
    it_new-armz_desc     = obj_inst->get_desc_armz( it_new-cod_armz ). "bug 50095
    it_new-ponto_c_desc  = obj_inst->get_desc_pon( it_new-ponto_c ).
    it_new-terminal_estuf_desc = obj_inst->get_desc_pon( it_new-terminal_estuf ).
    it_new-controladora_desc = obj_inst->get_desc_pon( it_new-controladora ).
    it_new-landx = obj_inst->get_desc_pais( it_new-pais_des ).
*** Inicio - Rubenilson Pereira - 12.02.25 - US164130
    it_new-desc_local_entrega = obj_inst->get_desc_local_entrega( it_new-local_entrega ).
    it_new-desc_oper_log = obj_inst->get_desc_oper_log( it_new-oper_log ).
*** Fim - Rubenilson Pereira - 12.02.25 - US164130
    it_new-cod_despac_desc = obj_inst->get_desc_pon( it_new-cod_despach ).
    it_new-cod_transp_desc = obj_inst->get_desc_pon( it_new-cod_transp ).

    obj_inst->set_inst_qtd( ).

  ENDMETHOD.

  METHOD set_layout.

    CLEAR: wa_layout, wa_variant, wa_stable.

    wa_layout = VALUE #(
                        zebra      = abap_true
                        no_rowins  = abap_true
                        stylefname = 'ESTILO'
                        info_fname = 'COLOR'
                        sel_mode   = 'A'
                        ).

    wa_variant = VALUE #(
                         report = sy-repid
                         ).

    wa_stable = VALUE #(
                        row = abap_true
                        col = abap_true
                        ).

  ENDMETHOD.

  METHOD show_msgre.

*-CS2021000532-#59109-08.08.2022-JT-inicio
    IF it_0045_temp[] IS INITIAL AND ( it_new-bukrs IS NOT INITIAL
                                 AND   it_new-werks IS NOT INITIAL
                                 AND   it_new-matnr IS NOT INITIAL )
                                 AND   lv_acao2 <> 'BNT_EDIT'.
      APPEND VALUE #(
                      mandt            = sy-mandt
                      zseq_inst        = abap_false
                      objek            = it_new-objek
                      objecttable      = it_new-objecttable
                      bukrs            = it_new-bukrs"
                      werks            = it_new-werks"
                      instrucao        = it_new-instrucao"
                      data_instr       = it_new-data_instr"
                      contrato         = it_new-contrato"
                      data_retirada    = it_new-data_retirada"
                      deadline_draft   = it_new-deadline_draft"
                      deadline_documen = it_new-deadline_documen"
                      porto_embarque   = it_new-porto_embarque"
                      data_porto       = it_new-data_porto"
                      safra            = it_new-safra"
                      status           = it_new-status
                      data_criacao     = sy-datum
                      usuario          = sy-uname
                      quantidade       = it_new-quantidade"
                      voleh            = it_new-voleh"
                      matnr            = it_new-matnr
                      charg            = it_new-charg"
                      dmbtr            = it_new-dmbtr"
                      pmein            = it_new-pmein"
                      terminal         = it_new-terminal"
                      ponto_c          = it_new-ponto_c"
                      btgew            = it_new-btgew"
                      gewei            = it_new-gewei"
                      observacao       = it_new-observacao"
                      booking          = it_new-booking"
                      armador          = it_new-armador"
                      qtd_ctners       = it_new-qtd_ctners"
                      mapa             = it_new-mapa"
                      fumigacao        = it_new-fumigacao"
                      hrs_fgacao       = it_new-hrs_fgacao"
                      vlr_frete        = it_new-vlr_frete"
                      free_time        = it_new-free_time"
                      cod_despach      = it_new-cod_despach"
                      cod_transp       = it_new-cod_transp"
                      pais_des         = it_new-pais_des
                      terminal_estuf   = it_new-terminal_estuf
                      controladora     = it_new-controladora
                      navio            = it_new-navio
                      tamanho_fardo    = it_new-tamanho_fardo
                      data_container   = it_new-data_container
                      e_mail           = abap_false
                      data_in_porto    = it_new-data_in_porto"
                      data_eta         = it_new-data_eta"
                      color            = 'C500'
                      limite_peso      = COND #( WHEN it_new-limite_peso EQ 'S' THEN 'S' ELSE 'N')
                      peso_max         = COND #( WHEN it_new-limite_peso EQ 'S' THEN it_new-peso_max ELSE 0 )
                      armazenagem      = it_new-armazenagem
                      lote_armz        = it_new-lote_armz
                      cod_armz         = it_new-cod_armz
                    ) TO it_0045_temp.

      obj_inst->set_inst_qtd( ).
    ELSE.
      READ TABLE it_0045_temp     INTO wa_0045_temp INDEX 1.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING it_new   TO wa_0045_temp.
        MODIFY it_0045_temp       FROM wa_0045_temp INDEX sy-tabix.
      ENDIF.

      obj_inst->set_inst_qtd( ).
    ENDIF.
*-CS2021000532-#59109-08.08.2022-JT-fim

    obj_inst->set_erros( ).

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen    = '100'
        i_show      = abap_true
        i_repid     = sy-repid
*       I_POPUP     = 1
        i_set_field = 'X_FIELD'
      IMPORTING
        e_messagem  = wg_mensagem
      TABLES
        it_msgs     = tg_msg_ret.

  ENDMETHOD.

*-CS2021000532-#59109-08.08.2022-JT-inicio
  METHOD set_valida_item.

    DATA: wl_linha(6).
    DATA: vmatnr18 TYPE matnr18.

    wl_linha = sy-tabix.

    IF wa-bukrs NE '0001'.  " Não faz a duas validações para a empresa 0001

      FREE tmchb.
      vmatnr18 = |{ wa-matnr ALPHA = IN }|.
      wa-matnr = vmatnr18.
      IF wa-zseq_inst IS INITIAL OR i_valida = abap_true. "*-CS2021000532-#59109-08.08.2022-JT-inicio
**** CS2020001361  - Inicio - Camila Brand
        IF wa-armazenagem IS INITIAL.
**** CS2020001361  - Fim - Camila Brand
          SELECT * FROM mchb
            INTO CORRESPONDING FIELDS OF TABLE tmchb
            WHERE matnr EQ wa-matnr
              AND werks EQ wa-werks
              AND lgort EQ wa-charg(4).

          IF sy-subrc IS INITIAL.
            FREE saldo.

            LOOP AT tmchb INTO DATA(wmchb).
              COLLECT wmchb INTO saldo.
              CLEAR wmchb.
            ENDLOOP.

            DATA(wsaldo) = saldo[ 1 ].

            ADD wsaldo-clabs TO wsaldo-saldo.
            ADD wsaldo-cumlm TO wsaldo-saldo.
            ADD wsaldo-cinsm TO wsaldo-saldo.
            ADD wsaldo-ceinm TO wsaldo-saldo.
            ADD wsaldo-cspem TO wsaldo-saldo.
            ADD wsaldo-cretm TO wsaldo-saldo.
            ADD wsaldo-cvmla TO wsaldo-saldo.
            ADD wsaldo-cvmum TO wsaldo-saldo.
            ADD wsaldo-cvmin TO wsaldo-saldo.
            ADD wsaldo-cvmei TO wsaldo-saldo.
            ADD wsaldo-cvmsp TO wsaldo-saldo.
            ADD wsaldo-cvmre TO wsaldo-saldo.

          ELSE.
            wsaldo-saldo = 0.
          ENDIF.

          IF wa-btgew > wsaldo-saldo.
            IF lv_acao NE 'BNT_EDIT'.     "<<-RIM-SKM-IR127685-14.03.23
              APPEND VALUE #(
                              msg = |Total do Lote { wa-charg(4) }/{ wa-matnr } maior que o disponível { wsaldo-saldo }. Linha { wl_linha }!|
                            ) TO tg_msg_ret.
            ENDIF.                       "<<-RIM-SKM-IR127685-14.03.23
          ENDIF.
        ELSE.
**** CS2020001361  - Inicio - Camila Brand
          SELECT * FROM mslb
                     INTO CORRESPONDING FIELDS OF TABLE tmslb
                     WHERE matnr EQ wa-matnr
                       AND werks EQ wa-werks
                       AND charg EQ wa-lote_armz
                       AND lifnr EQ wa-cod_armz.

          IF sy-subrc IS INITIAL.
            FREE saldo2.

            LOOP AT tmslb INTO DATA(wmslb).
              COLLECT wmslb INTO saldo2.
              CLEAR wmslb.
            ENDLOOP.

            DATA(wsaldo2) = saldo2[ 1 ].

            ADD wsaldo2-lblab TO wsaldo2-saldo.
            ADD wsaldo2-lbins TO wsaldo2-saldo.
            ADD wsaldo2-lbvla TO wsaldo2-saldo.
            ADD wsaldo2-lbvin TO wsaldo2-saldo.


          ELSE.
            wsaldo2-saldo = 0.
          ENDIF.

          IF wa-btgew > wsaldo2-saldo.
            APPEND VALUE #(
                            msg = |Total da Instrução da linha { wl_linha } maior que o disponível { wsaldo2-saldo }!|
                          ) TO tg_msg_ret.
          ENDIF.
        ENDIF.
**** CS2020001361  - Fim - Camila Brand
      ENDIF.

* Verifica se o Conjunto de Lote+Material+centro existe na Tabela MARD
      IF NOT me->get_mard( wa ) IS INITIAL.
        APPEND VALUE #(
           field = 'CHARG'
           tabix = sy-tabix
           msg = |O lote { wa-charg } não foi encontrado para o material { wa-matnr }! Linha: { wl_linha }|
          ) TO tg_msg_ret.
      ENDIF.
    ENDIF.

  ENDMETHOD.
*-CS2021000532-#59109-08.08.2022-JT-fim

*-CS2021000532-#59109-08.08.2022-JT-inicio
  METHOD set_campos_obrigatorios.

    DATA: t_tab     TYPE TABLE OF rgsb4,
          w_tab     TYPE rgsb4,
          t_tab_txt TYPE TABLE OF rgsb4,
          w_tab_txt TYPE rgsb4.

    FREE: t_tab, t_tab_txt, t_campos.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class           = '0000'
        setnr           = 'ZSDT0121_CAMPOS'
        no_descriptions = abap_off
      TABLES
        set_values      = t_tab
      EXCEPTIONS
        set_not_found   = 1
        OTHERS          = 2.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class           = '0000'
        setnr           = 'ZSDT0121_CAMPOS_TEXTO'
        no_descriptions = abap_off
      TABLES
        set_values      = t_tab_txt
      EXCEPTIONS
        set_not_found   = 1
        OTHERS          = 2.

    SELECT * FROM t001  INTO TABLE @DATA(t_t001).

    LOOP AT t_tab    INTO w_tab.
      CLEAR: w_campos, w_tab_txt.

      READ TABLE t_tab_txt INTO w_tab_txt WITH KEY from = w_tab-from.
      w_campos-nome         = w_tab_txt-title.

      IF w_tab-title = '*'.
        LOOP AT t_t001   INTO DATA(w_t001).
          w_campos-bukrs    = w_t001-bukrs.
          w_campos-field    = w_tab-from.
          APPEND w_campos  TO t_campos.
        ENDLOOP.
      ELSE.
        w_campos-bukrs      = w_tab-title.
        w_campos-field      = w_tab-from.
        APPEND w_campos    TO t_campos.
      ENDIF.
    ENDLOOP.

    LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat2>).
      CASE i_tipo.
        WHEN '1'.
          campo = |IT_NEW-{ <fcat2>-fieldname }|.
        WHEN '2'.
          campo = |WA-{ <fcat2>-fieldname }|.
      ENDCASE.

      ASSIGN (campo) TO <fs_campo>.

      IF <fs_campo> IS INITIAL.
        READ TABLE t_campos INTO w_campos WITH KEY bukrs = it_0051-vkorg
                                                   field = <fcat2>-fieldname(20).
        IF sy-subrc = 0.
          IF i_tipo = '1'.
            APPEND VALUE #( field = <fcat2>-fieldname
                            msg   = |Campo { w_campos-nome } obrigatório!|
                           )
                      TO tg_msg_ret.
          ELSE.
            APPEND VALUE #( field = <fcat2>-fieldname
                            msg   = |Campo { w_campos-nome } obrigatório! Linha: { i_linha }|
                           )
                      TO tg_msg_ret.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

*-CS2021000532-#59109-08.08.2022-JT-inicio
    CASE i_tipo.
      WHEN '1'.
        IF it_new-fumigacao EQ 'S'.
          IF it_new-hrs_fgacao IS INITIAL.
            APPEND VALUE #(
                            field = 'HRS_FGACAO'
                            msg = |Campo Horas Fumigação obrigatório!|
                           ) TO tg_msg_ret.
          ENDIF.
        ENDIF.

        IF it_new-limite_peso EQ 'S'.
          IF it_new-peso_max IS INITIAL.
            APPEND VALUE #(
                            field = 'HRS_FGACAO'
                            msg = |Campo Peso Máximo obrigatório!|
                           ) TO tg_msg_ret.
          ENDIF.
        ENDIF.

        IF it_new-bukrs <> it_0051-vkorg.
          APPEND VALUE #(
                          field = 'BUKRS'
                          msg = |Empresa da Solicitação divergente à informada!|
                         ) TO tg_msg_ret.
        ENDIF.

      WHEN '2'.
        IF wa-fumigacao EQ 'S'.
          IF wa-hrs_fgacao IS INITIAL.
            APPEND VALUE #(
                            field = 'HRS_FGACAO'
                            msg = |Campo Horas Fumigação obrigatório! Linha: { i_linha }|
                           ) TO tg_msg_ret.
          ENDIF.
        ENDIF.

        IF wa-limite_peso EQ 'S'.
          IF wa-peso_max IS INITIAL.
            APPEND VALUE #(
                            field = 'HRS_FGACAO'
                            msg = |Campo Peso Máximo obrigatório! Linha: { i_linha }|
                           ) TO tg_msg_ret.
          ENDIF.
        ENDIF.

        IF wa-bukrs <> it_0051-vkorg.
          APPEND VALUE #(
                          field = 'BUKRS'
                          msg = |Empresa da Solicitação divergente à informada! Linha: { i_linha }|
                         ) TO tg_msg_ret.
        ENDIF.

    ENDCASE.
*-CS2021000532-#59109-08.08.2022-JT-fim

  ENDMETHOD.
*-CS2021000532-#59109-08.08.2022-JT-fim

  METHOD set_erros.

    DATA: wl_linha(6).

    cont = 1.
    FREE: tg_msg_ret.

    CHECK lv_acao NE 'BNT_DEL'.

    obj_inst->set_inst_qtd( ).  "*-CS2023000189-06.04.2023-#108697-JT

* Checa se Todos os Campos Obrigatórios foram preenchidos
    IF it_0045_temp[] IS NOT INITIAL OR lv_acao2 = 'BNT_EDIT'.

      obj_inst->set_campos_obrigatorios( i_tipo = '1' ).

*      LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat2>).
*
*        ADD 1 TO cont.
*
**          IF CONT BETWEEN 46 AND 51 AND IT_0051-VKORG EQ '0015'.
*        IF ( it_0051-vkorg EQ '0015' ) OR ( it_0051-vkorg EQ '0050' ).
*          IF cont >= 46.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*
*        campo = |IT_NEW-{ <fcat2>-fieldname }|.
*        ASSIGN (campo) TO <fs_campo>.
*
*        IF <fs_campo> IS INITIAL.
*          CASE cont.
*            WHEN '2' OR '3' OR '17' OR '30' OR '31' OR '32' OR '33' OR '34' OR '43' OR  '44' OR  '45' OR '42'.
*            WHEN OTHERS.
*              IF ( cont EQ '53' OR cont EQ '54' ) AND it_0051-vkorg EQ '0001'.
*                CONTINUE.
*              ENDIF.
*              campo = |TEXT-{ cont }|.
*              ASSIGN (campo) TO <fs_campo1>.
*              APPEND VALUE #(
*                              field = <fcat2>-fieldname
*                              msg = |Campo { <fs_campo1> } obrigatório!|
*                             ) TO tg_msg_ret.
*          ENDCASE.
*        ENDIF.
*      ENDLOOP.

    ENDIF.

    CASE lv_acao.
      WHEN 'BNT_ADD' OR 'SHOW_MSGRE'.

        IF lv_importa = abap_off.
          obj_inst->set_campos_obrigatorios( i_tipo = '1' ).
        ENDIF.

*        LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
*
*          ADD 1 TO cont.
*
**          IF CONT BETWEEN 46 AND 51 AND IT_0051-VKORG EQ '0015'.
*          IF ( it_0051-vkorg EQ '0015' ) OR ( it_0051-vkorg EQ '0050' ).
*            IF cont >= 46.
*              CONTINUE.
*            ENDIF.
*          ENDIF.
*
*          campo = |IT_NEW-{ <fcat>-fieldname }|.
*          ASSIGN (campo) TO <fs_campo>.
*
*          IF <fs_campo> IS INITIAL.
*            CASE cont.
*              WHEN '2' OR '3' OR '17' OR '30' OR '31' OR '32' OR '33' OR '34' OR '43' OR  '44' OR  '45' OR '42'.
*              WHEN OTHERS.
*                IF ( cont EQ '53' OR cont EQ '54' ) AND it_0051-vkorg EQ '0001'.
*                  CONTINUE.
*                ENDIF.
*                campo = |TEXT-{ cont }|.
*                ASSIGN (campo) TO <fs_campo1>.
*                APPEND VALUE #(
*                                field = <fcat>-fieldname
*                                msg = |Campo { <fs_campo1> } obrigatório!|
*                               ) TO tg_msg_ret.
*            ENDCASE.
*          ENDIF.
*        ENDLOOP.

        CLEAR lv_acao.

      WHEN 'INSTRUCAO' OR 'SHOW_MSGRE'.
*        LOOP AT it_0045 INTO wa WHERE zseq_inst IS INITIAL.
*
*          obj_inst->set_campos_obrigatorios( i_tipo = '2' ).

*          cont = 1.
*          LOOP AT it_fcat ASSIGNING <fcat>.
*
*            ADD 1 TO cont.
*
**            IF CONT BETWEEN 46 AND 51 AND IT_0051-VKORG EQ '0015'.
**            IF cont >= 46 AND it_0051-vkorg EQ '0015'.
**              CONTINUE.
**            ENDIF.
*            IF ( it_0051-vkorg EQ '0015' ) OR ( it_0051-vkorg EQ '0050' ).
*              IF cont >= 46.
*                CONTINUE.
*              ENDIF.
*            ENDIF.
*
*            campo = |WA-{ <fcat>-fieldname }|.
*            ASSIGN (campo) TO <fs_campo>.
*
*            IF <fs_campo> IS INITIAL.
*              CASE cont.
*                WHEN '2' OR '3' OR '17' OR '30' OR '35' OR '39' OR '40' OR '41' OR '43' OR '44' OR '45'.
*                WHEN OTHERS.
*                  IF ( cont EQ '53' OR cont EQ '54' ) AND it_0051-vkorg EQ '0001'.
*                    CONTINUE.
*                  ENDIF.
*                  campo = |TEXT-{ cont }|.
*                  ASSIGN (campo) TO <fs_campo1>.
*                  APPEND VALUE #(
*                                  field = <fcat>-fieldname
*                                  msg = |Campo { <fs_campo1> } obrigatório!|
*                                 ) TO tg_msg_ret.
*              ENDCASE.
*            ENDIF.
*          ENDLOOP.

*          IF wa-fumigacao EQ 'S'.
*            IF wa-hrs_fgacao IS INITIAL.
*              APPEND VALUE #(
*                              field = 'HRS_FGACAO'
*                              msg = |Campo Horas Fumigação obrigatório!|
*                             ) TO tg_msg_ret.
*            ENDIF.
*          ENDIF.
*
**-CS2021000532-#59109-08.08.2022-JT-inicio
*          IF wa-limite_peso EQ 'S'.
*            IF wa-peso_max IS INITIAL.
*              APPEND VALUE #(
*                              field = 'HRS_FGACAO'
*                              msg = |Campo Peso Máximo obrigatório!|
*                             ) TO tg_msg_ret.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*-CS2021000532-#59109-08.08.2022-JT-fim
    ENDCASE.

    IF lv_importa = abap_true.
      DATA(l_lin) = 0.
      LOOP AT it_0045 INTO wa WHERE zseq_inst IS INITIAL.
        l_lin = sy-tabix.
        obj_inst->set_campos_obrigatorios( i_tipo = '2' i_linha = l_lin ).
      ENDLOOP.
    ENDIF.

    IF  obj_inst->check_0182( it_new-zseq_inst ) IS INITIAL.

      IF obj_inst->get_saldo_fardos( it_new-zseq_inst ) < it_new-quantidade.
        APPEND VALUE #(
                        field = 'IT_NEW-QUANTIDADE'
                        msg = |Quantidade instruída esta superior a quantidade do TAKE-UP para o lote!|
                       ) TO tg_msg_ret.
      ENDIF.

      IF obj_inst->get_saldo_peso( it_new-zseq_inst ) < it_new-btgew.
        APPEND VALUE #(
                field = 'IT_NEW-BTGEW'
                msg = |Peso instruído esta superior ao peso do TAKE-UP para o lote!|
               ) TO tg_msg_ret.
      ENDIF.

    ENDIF.

**** CS2020001361  - Inicio - Camila Brand
    IF it_new-armazenagem EQ 'X' AND  ( it_new-lote_armz IS INITIAL AND it_new-cod_armz IS INITIAL ).

      APPEND VALUE #(
                      field = 'LOTE_ARMZ'
                      msg = |Campo Lote Armazenagem obrigatório!|
                     ) TO tg_msg_ret.

      APPEND VALUE #(
        field = 'COD_ARMZ'
        msg = |Campo Armazém obrigatório!|
       ) TO tg_msg_ret.
    ENDIF.
**** CS2020001361  - Fim - Camila Brand


    IF ( it_new-matnr IS NOT INITIAL ).
      SELECT SINGLE matnr FROM mara INTO @DATA(lva_check_matnr) WHERE matnr = @it_new-matnr.
      IF ( sy-subrc <> 0 ).
        APPEND VALUE #(
          field = 'it_new-matnr'
          msg = |Código de Material não encontrado!|
        ) TO tg_msg_ret.
      ENDIF.
    ENDIF.

    IF ( it_new-terminal IS NOT INITIAL ) AND ( obj_inst->valida_existe_lfa1( it_new-terminal ) = abap_false ).
      APPEND VALUE #(
        field = 'it_new-terminal'
        msg = |Terminal de Embarque não encontrado!|
      ) TO tg_msg_ret.
    ENDIF.

    IF ( it_new-terminal_estuf  IS NOT INITIAL ) AND ( obj_inst->valida_existe_lfa1( it_new-terminal_estuf  ) = abap_false ).
      APPEND VALUE #(
        field = 'it_new-terminal_estuf'
        msg = |Terminal Estufagem não encontrado!|
      ) TO tg_msg_ret.
    ENDIF.

    IF ( it_new-ponto_c  IS NOT INITIAL ) AND ( obj_inst->valida_existe_lfa1( it_new-ponto_c  ) = abap_false ).
      APPEND VALUE #(
        field = 'it_new-ponto_c'
        msg = |Ponto de Coleta não encontrado!|
      ) TO tg_msg_ret.
    ENDIF.

    IF ( it_new-cod_despach IS NOT INITIAL ) AND ( obj_inst->valida_existe_lfa1( it_new-cod_despach  ) = abap_false ).
      APPEND VALUE #(
        field = 'it_new-cod_despach'
        msg = |Despachante não encontrado!|
      ) TO tg_msg_ret.
    ENDIF.

    IF ( it_new-controladora IS NOT INITIAL ) AND ( obj_inst->valida_existe_lfa1( it_new-controladora  ) = abap_false ).
      APPEND VALUE #(
        field = 'it_new-controladora'
        msg = |Controladora não encontrada!|
      ) TO tg_msg_ret.
    ENDIF.

    IF ( it_new-cod_armz IS NOT INITIAL ) AND ( obj_inst->valida_existe_lfa1( it_new-cod_armz  ) = abap_false ).
      APPEND VALUE #(
        field = 'it_new-cod_armz'
        msg = |Armazém não encontrado!|
      ) TO tg_msg_ret.
    ENDIF.

    IF ( it_new-cod_transp IS NOT INITIAL ) AND ( obj_inst->valida_existe_lfa1( it_new-cod_transp  ) = abap_false ).
      APPEND VALUE #(
        field = 'it_new-cod_transp'
        msg = |Transportadora não encontrada!|
      ) TO tg_msg_ret.
    ENDIF.

* verifica se Exixte estoque para a edição ou nova instrução
*    LOOP AT IT_NEW INTO IT_NEW.
*-CS2021000532-#59109-08.08.2022-JT-inicio
    IF it_new-bukrs    IS NOT INITIAL AND it_new-werks IS NOT INITIAL AND it_new-matnr IS NOT INITIAL AND
       it_new-contrato IS NOT INITIAL AND it_new-charg IS NOT INITIAL AND
       it_0045_temp[]  IS     INITIAL.
      DATA(lv_valida) = COND #( WHEN lv_acao = 'BNT_EDIT' THEN abap_false
                                                          ELSE abap_true ).
      obj_inst->set_valida_item( EXPORTING i_valida = lv_valida CHANGING wa = it_new ).
    ENDIF.

    IF it_0045_temp[] IS NOT INITIAL.
      LOOP AT it_0045_temp INTO wa_0045_temp.
        obj_inst->set_valida_item( CHANGING wa = wa_0045_temp ).
      ENDLOOP.
    ELSE.
      LOOP AT it_0045 INTO wa.
        obj_inst->set_valida_item( CHANGING wa = wa ).
      ENDLOOP.
    ENDIF.
*-CS2021000532-#59109-08.08.2022-JT-fim

*  Verifica se a Quantidade do Contrato é inferior a quantidade das instruções
    IF obj_inst->get_inst_qtd( ) GE obj_inst->get_limit_qtd( ).
      APPEND VALUE #(
                     msg = |Limite da Quantidade do Contrato Excedida! { obj_inst->get_limit_qtd( ) }|
                    ) TO tg_msg_ret.
    ENDIF.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen    = '100'
        i_repid     = sy-repid
        i_set_field = 'X_FIELD'
      IMPORTING
        e_messagem  = wg_mensagem
      TABLES
        it_msgs     = tg_msg_ret.

  ENDMETHOD.

  METHOD act_new_inst.

    obj_inst->set_erros( ).

    IF tg_msg_ret[] IS INITIAL AND it_new IS NOT INITIAL.

      APPEND VALUE #(
                      mandt            = sy-mandt
                      zseq_inst        = abap_false
                      objek            = it_new-objek
                      objecttable      = it_new-objecttable
                      bukrs            = it_new-bukrs"
                      werks            = it_new-werks"
                      instrucao        = it_new-instrucao"
                      data_instr       = it_new-data_instr"
                      contrato         = it_new-contrato"
                      data_retirada    = it_new-data_retirada"
                      deadline_draft   = it_new-deadline_draft"
                      deadline_documen = it_new-deadline_documen"
                      porto_embarque   = it_new-porto_embarque"
                      data_porto       = it_new-data_porto"
                      safra            = it_new-safra"
                      status           = it_new-status
*                    ICON             = SWITCH #( IT_NEW-STATUS
*                                                  WHEN ''  THEN '@Q3@'
*                                                  WHEN 'L' THEN '@5Y@'
*                                                  WHEN 'A' THEN '@K4@'
*                                                  WHEN 'F' THEN '@DF@' )
                      data_criacao     = sy-datum
                      usuario          = sy-uname
                      quantidade       = it_new-quantidade"
                      voleh            = it_new-voleh"
                      matnr            = it_new-matnr
                      charg            = it_new-charg"
                      dmbtr            = it_new-dmbtr"
                      pmein            = it_new-pmein"
                      terminal         = it_new-terminal"
                      ponto_c          = it_new-ponto_c"
                      btgew            = it_new-btgew"
                      gewei            = it_new-gewei"
                      observacao       = it_new-observacao"
                      booking          = it_new-booking"
                      armador          = it_new-armador"
                      qtd_ctners       = it_new-qtd_ctners"
                      mapa             = it_new-mapa"
                      fumigacao        = it_new-fumigacao"
                      hrs_fgacao       = it_new-hrs_fgacao"
                      vlr_frete        = it_new-vlr_frete"
                      free_time        = it_new-free_time"
                      cod_despach      = it_new-cod_despach"
                      cod_transp       = it_new-cod_transp"
                      pais_des         = it_new-pais_des
                      terminal_estuf   = it_new-terminal_estuf
                      controladora     = it_new-controladora
                      navio            = it_new-navio
                      tamanho_fardo    = it_new-tamanho_fardo
                      data_container   = it_new-data_container
*                    usnam_lt         =
*                    data_lt          =
*                    hora_lt          =
                      e_mail           = abap_false
                      data_in_porto    = it_new-data_in_porto"
                      data_eta         = it_new-data_eta"
                      color            = 'C500'
                      limite_peso      = COND #( WHEN it_new-limite_peso EQ 'S' THEN 'S' ELSE 'N')
                      peso_max         = COND #( WHEN it_new-limite_peso EQ 'S' THEN it_new-peso_max ELSE 0 )
*** CS2020001361 - Inicio - Camila Brand
                     armazenagem      = it_new-armazenagem
                     lote_armz        = it_new-lote_armz
                     cod_armz         = it_new-cod_armz
*** CS2020001361 - Fim - Camila Brand

                    ) TO it_0045.

      obj_inst->set_inst_qtd( ).

      FREE: it_0045_temp,
            it_new.
      CLEAR: it_new.
    ENDIF.

    MOVE-CORRESPONDING obj_inst->get_header( nro_sol_ov = s_solov-low
                                           ) TO it_0051.

*    CLEAR IT_NEW.

    MOVE: it_0051-nro_sol_ov TO it_new-objek,
          'ZSDT0051'         TO it_new-objecttable,
          it_0051-bstkd      TO it_new-contrato,
          sy-datum           TO it_new-data_criacao,
          sy-uname           TO it_new-usuario.

*-CS2021000532-#59109-08.08.2022-JT-inicio
*   CLEAR it_new.
*-CS2021000532-#59109-08.08.2022-JT-fim

  ENDMETHOD.

  METHOD save.

    DATA vmatnr18 TYPE matnr18.

    IF p_inst IS NOT INITIAL.
      FREE it_0182.
    ENDIF.


*   DATA: tg_0045 TYPE TABLE OF zsdt0045.
*   DATA: tg_instrucao TYPE TABLE OF zsdt0045.
    DATA: em_0045 TYPE TABLE OF zsdt0045.

    FREE: tg_0045.

    obj_inst->show_msgre( ).

    CHECK tg_msg_ret[] IS INITIAL.

    CLEAR lv_acao2.

*---> 04/07/2023 - Migração S4 - WS
    SORT it_0045.
*<--- 04/07/2023 - Migração S4 - WS
    DELETE ADJACENT DUPLICATES FROM it_0045 COMPARING ALL FIELDS.

*    it_save =
*      VALUE #( FOR ls_0045 IN it_0045 WHERE ( zseq_inst IS INITIAL )
*                   ( CORRESPONDING #( ls_0045 ) )
*                   ( zseq_inst = obj_inst->get_zseq_inst( i_value = 'ZSEQ_INST' l_0045 = ls_0045 )
*                     matnr = |{ ls_0045-matnr ALPHA = IN }|
*                   )
*             ).

    LOOP AT it_0045 INTO  wa_0045 WHERE zseq_inst IS INITIAL.
      wa_0045-zseq_inst = obj_inst->get_zseq_inst( i_value = 'ZSEQ_INST' l_0045 = wa_0045 ).
      MOVE-CORRESPONDING wa_0045 TO wa_save.
      APPEND wa_save TO it_save.
    ENDLOOP.


    DELETE it_save WHERE zseq_inst IS INITIAL OR zseq_inst = 0.
    DELETE it_0045 WHERE zseq_inst IS INITIAL OR zseq_inst = 0.
    DELETE FROM zsdt0045 WHERE zseq_inst IS NULL OR zseq_inst = 0.

    it_edit =
      VALUE #( FOR ls_0045 IN it_0045 WHERE ( color EQ 'C700' )
                   ( CORRESPONDING #( ls_0045 ) )
             ).

    DELETE it_save WHERE zseq_inst IS INITIAL.
    DELETE it_dele WHERE zseq_inst IS INITIAL.

    APPEND LINES OF it_save[] TO tg_0045[].
    APPEND LINES OF it_edit[] TO tg_0045[].

    IF NOT it_dele IS INITIAL.

*-CS2023000189-06.04.2023-#108697-JT-inicio
*-------------------
*-- exclui no trace
*-------------------
      DATA(_erro_exclusao_trace) = abap_false.
      PERFORM f_exclui_trace CHANGING _erro_exclusao_trace.
      CHECK _erro_exclusao_trace EQ abap_false.
*-CS2023000189-06.04.2023-#108697-JT-fim

      DELETE zsdt0045 FROM TABLE it_dele.
      DELETE zsdt0182 FROM TABLE it_0182_d.

      IF sy-subrc IS INITIAL.
        IF lines( it_dele[] ) > 1.
          MESSAGE |Documentos Deletados com Sucesso!| TYPE 'S'.
        ELSE.
          MESSAGE |Documento Deletado com Sucesso!| TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE |Documentos não foram Deletados!| TYPE 'S' DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.
      FREE: it_dele[], it_0182_d, it_0182.
      me->clear( ).
      WAIT UP TO 2 SECONDS.
    ENDIF.

    CHECK tg_0045[] IS NOT INITIAL.

*-CS2023000189-06.04.2023-#108697-JT-inicio
*-------------------
*-- recupera dados antes de salvar
*-------------------
    PERFORM f_dados_antigos_trace.
*-CS2023000189-06.04.2023-#108697-JT-fim


    DATA: l_acts TYPE zsdt0166-acts, l_id TYPE zsdt0166-id.   "<<-RIM-SKM-IR147418-30.08.23
    LOOP AT tg_0045 ASSIGNING FIELD-SYMBOL(<f45>).

*-CS2023000189-06.04.2023-#108697-JT-inicio
*---- recupera flag ACTS contrato
      " Alteração - RIM-SKM-IR147418-30.08.23 - Inicio
*      SELECT single acts
*        INTO @DATA(l_acts)
      SELECT id acts
        INTO (l_id, l_acts)
        FROM zsdt0166
*       WHERE contrato EQ @<f45>-contrato
*         AND lote     EQ @<f45>-charg
*         AND safra    EQ @<f45>-safra
*         AND werks    EQ @<f45>-werkS
       WHERE contrato EQ <f45>-contrato
         AND lote     EQ <f45>-charg
         AND safra    EQ <f45>-safra
         AND werks    EQ <f45>-werks
         AND status   EQ 'A' ORDER BY id.
      ENDSELECT.
      " Alteração - RIM-SKM-IR147418-30.08.23 - Fim
      IF sy-subrc <> 0.
        CLEAR l_acts.
      ENDIF.

      <f45>-acts     = l_acts.
*-CS2023000189-06.04.2023-#108697-JT-fim

      vmatnr18       = |{ <f45>-matnr    ALPHA = IN }|.
      <f45>-terminal = |{ <f45>-terminal ALPHA = IN }|.
      <f45>-ponto_c  = |{ <f45>-ponto_c  ALPHA = IN }|.
*     <f45>-matnr    = |{ <f45>-matnr    ALPHA = IN }|.
      <f45>-matnr    = vmatnr18.
      <f45>-bukrs    = |{ <f45>-bukrs    ALPHA = IN }|.
      <f45>-werks    = |{ <f45>-werks    ALPHA = IN }|.
      <f45>-terminal_estuf = |{ <f45>-terminal_estuf    ALPHA = IN }|.
      <f45>-controladora = |{ <f45>-controladora    ALPHA = IN }|.
**** CS2020001361 - Inicio - Camila Brand
      IF <f45>-armazenagem IS INITIAL.
        <f45>-lote_armz	= ''.
        <f45>-cod_armz = ''.
      ENDIF.
**** CS2020001361 - Fim - Camila Brand
    ENDLOOP.

    MODIFY zsdt0045 FROM TABLE tg_0045.
    tg_instrucao = tg_0045.
    SORT tg_instrucao BY instrucao.
    DELETE ADJACENT DUPLICATES FROM tg_instrucao COMPARING instrucao.

    SORT it_0045_sel BY zseq_inst objek objecttable.

    LOOP AT tg_instrucao INTO w_instrucao.

      FREE: l_set_upd.

*-CS2021000532-#59109-08.08.2022-JT-inicio
      READ TABLE it_0045_sel INTO wa_0045_sel WITH KEY zseq_inst   = w_instrucao-zseq_inst
                                                       objek       = w_instrucao-objek
                                                       objecttable = w_instrucao-objecttable
                                              BINARY SEARCH.

      CHECK sy-subrc = 0.

      l_set_upd = |{ l_set_upd } { set_update_table( field = 'INSTRUCAO' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'PORTO_EMBARQUE' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'NAVIO' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'BOOKING' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'MAPA' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'FUMIGACAO' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'ARMADOR' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'HRS_FGACAO' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'QTD_CTNERS' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'FREE_TIME' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'OBSERVACAO' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'DATA_INSTR' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'DATA_IN_PORTO' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'DEADLINE_DRAFT' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'DATA_RETIRADA' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'DATA_PORTO' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'DEADLINE_DOCUMEN' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'DATA_ETA' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'DATA_CONTAINER' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'PAIS_DES' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'TERMINAL' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'TERMINAL_ESTUF' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'COD_DESPACH' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'CONTROLADORA' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'COD_TRANSP' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'LIMITE_PESO' ) }|.
      l_set_upd = |{ l_set_upd } { set_update_table( field = 'PESO_MAX' ) }|.

      CONDENSE l_set_upd.

      IF l_set_upd IS NOT INITIAL.
        TRY.
            UPDATE zsdt0045 SET (l_set_upd)
                          WHERE  instrucao  = wa_0045_sel-instrucao
                            AND  safra      = wa_0045_sel-safra.  "*-CS2023000189-06.04.2023-#108697-JT
          CATCH cx_sy_dynamic_osql_error.
        ENDTRY.
      ENDIF.
*-CS2021000532-#59109-08.08.2022-JT-fim

*      IF w_instrucao-limite_peso = 'S'.
*        UPDATE zsdt0045
*        SET limite_peso =  w_instrucao-limite_peso
*            peso_max    =  w_instrucao-peso_max
*        WHERE instrucao EQ w_instrucao-instrucao.
*      ELSE.
*        UPDATE zsdt0045
*                SET limite_peso =  w_instrucao-limite_peso
*                    peso_max    =  0
*                WHERE instrucao EQ w_instrucao-instrucao.
*      ENDIF.

    ENDLOOP.

*    CHECK sy-subrc IS INITIAL.

    MODIFY zsdt0182 FROM TABLE it_0182.

    LOOP AT it_0045 ASSIGNING FIELD-SYMBOL(<s45>).
      <s45>-terminal = |{ <s45>-terminal ALPHA = IN }|.
      <s45>-ponto_c  = |{ <s45>-ponto_c  ALPHA = IN }|.
      vmatnr18       = |{ <s45>-matnr    ALPHA = IN }|.
      <s45>-matnr    = vmatnr18.
      <s45>-bukrs    = |{ <s45>-bukrs    ALPHA = IN }|.
      <s45>-werks    = |{ <s45>-werks    ALPHA = IN }|.
      <s45>-terminal_estuf = |{ <s45>-terminal_estuf    ALPHA = IN }|.
      <s45>-controladora = |{ <s45>-controladora    ALPHA = IN }|.
    ENDLOOP.

    em_0045 = VALUE #( FOR ls_0045 IN it_0045 WHERE ( e_mail EQ abap_false )
                        ( CORRESPONDING #( ls_0045 ) )
                     ).
*    IF em_0045 IS NOT INITIAL.
*      NEW zcl_solicitacao_ov( )->envio_automatico( p_nro_sol_ov = s_solov-low
*                                                     i_zsdt0045 = em_0045[]
*                                                     i_tcode    = sy-tcode
*                                                  ).
*      FREE em_0045.
*    ENDIF.

    obj_inst->set_log( ).
    MODIFY zsdt0083 FROM TABLE tg_save_log[].

    COMMIT WORK AND WAIT.

*    "Forçar o reenvio dos emails
*    SELECT * FROM zsdt0045 INTO TABLE @DATA(it_0045_mail)
*            WHERE objek = @s_solov-low
*              AND objecttable = 'ZSDT0051'.
*    DELETE it_0045_mail[] WHERE e_mail = abap_true.
*    IF it_0045_mail[] IS NOT INITIAL.
*
*      it_0045[] = CORRESPONDING #( it_0045_mail[] ).
*
*      em_0045 = VALUE #( FOR ls_0045 IN it_0045 WHERE ( e_mail EQ abap_false )
*                       ( CORRESPONDING #( ls_0045 ) )
*                    ).
*      IF em_0045 IS NOT INITIAL.
*        NEW zcl_solicitacao_ov( )->envio_automatico( p_nro_sol_ov = s_solov-low
*                                                       i_zsdt0045 = em_0045[]
*                                                       i_tcode    = sy-tcode
*                                                    ).
*        FREE em_0045.
*      ENDIF.
*    ENDIF.
*
*-CS2023000189-06.04.2023-#108697-JT-inicio
*-------------------
*-- envia trace
*-------------------
    PERFORM f_envia_trace.
*-CS2023000189-06.04.2023-#108697-JT-fim

    CLEAR: tg_0045[], it_save[], it_edit[], it_new, p_inst.

    MESSAGE |Documentos Salvos com Sucesso!| TYPE 'S'.
*    obj_inst->get_dados( ).

  ENDMETHOD.

  METHOD set_update_table.

    FIELD-SYMBOLS: <value1> TYPE any,
                   <value2> TYPE any.

    DATA: l_field1 TYPE char100,
          l_field2 TYPE char255.

    FREE value.

    l_field1 = 'WA_0045_SEL' && '-' && field.
    l_field2 = 'W_INSTRUCAO' && '-' && field.

    ASSIGN (l_field1) TO <value1>.
    CHECK sy-subrc = 0.

    ASSIGN (l_field2) TO <value2>.
    CHECK sy-subrc = 0.

    CHECK <value1> <> <value2>.

    value = |{ field } = W_INSTRUCAO-{ field }|.

  ENDMETHOD.

  METHOD act_edit_inst.

    TRY .
        ASSIGN it_0045[ obj_inst->get_index( ) ] TO FIELD-SYMBOL(<f0045>).
        IF <f0045> IS ASSIGNED.

          <f0045>-color      = 'C700'.
          <f0045>-matnr      = |{ <f0045>-matnr ALPHA = IN }|.
          <f0045>-linha_edit = obj_inst->get_index( ).
          it_new = <f0045>.
          it_new-limite_peso = COND #( WHEN <f0045>-limite_peso IS INITIAL THEN 'N' ELSE <f0045>-limite_peso ).
          it_new-peso_max = SWITCH #( <f0045>-limite_peso WHEN 'S' THEN <f0045>-peso_max
                                                          ELSE '0'
                                     ).
          obj_inst->set_desc( ).
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        CLEAR it_new.
    ENDTRY.
    IF it_new-zseq_inst IS INITIAL.
      FIND icon_led_red IN wg_mensagem.
      IF sy-subrc NE 0.
        MESSAGE |Nr. do Documento não pode ser Editado!| TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD cotacao_frete.

    obj_inst->get_index( ).

    it_frete = VALUE #( FOR ls_index IN it_index
                        FOR ls_0045 IN it_0045 FROM ls_index-index TO ls_index-index
                        ( CORRESPONDING #( ls_0045 ) )
                      ).

    IF REDUCE i( INIT x = 0
                 FOR ls_index IN it_index
                 FOR ls_0045 IN it_0045 FROM ls_index-index TO ls_index-index
                 FOR ls_aux IN it_frete
                 WHERE ( instrucao <> ls_0045-instrucao  ) NEXT x = x + 1 ) IS INITIAL.
      CALL SCREEN 300 ENDING AT 60 7 STARTING AT 3 3.
    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH |Selecione apenas as Linhas da mesma Instrução!|.
    ENDIF.


  ENDMETHOD.

  METHOD get_index.

    CALL METHOD obj_alv->get_selected_rows
      IMPORTING
        et_index_rows = it_index.

    CASE input.
      WHEN 'BNT_FRETE' OR 'BNT_AUT' OR 'BNT_TRACE'. "*-CS2023000189-06.04.2023-#108697-JT

        IF it_index[] IS INITIAL.
          MESSAGE |Selecione ao menos uma Linha!| TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.

      WHEN OTHERS.

        IF lines( it_index[] ) EQ 1.
          r_value = it_index[ 1 ].
        ELSE.
          MESSAGE |Selecione Somente uma Linha para executar essa Ação!| TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.

  METHOD act_reenvia_trace.

    DATA: t_index TYPE lvc_t_row,
          w_index TYPE lvc_s_row.

    FREE: t_index.

    CALL METHOD obj_alv->get_selected_rows
      IMPORTING
        et_index_rows = t_index.

    IF t_index[] IS INITIAL.
      MESSAGE s024(sd) WITH 'Selecione ao menos uma Linha!' DISPLAY LIKE 'W'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = 'Aguarde...Integrando...'.

*-----------------------------
*-- reenvia instrucoes ao trace
*-----------------------------
    LOOP AT t_index INTO w_index.

      READ TABLE it_0045  INTO wa_0045  INDEX w_index-index.

*     SELECT SINGLE *
*       FROM zsdt0045
*       INTO @DATA(w_0045_old)
*      WHERE zseq_inst   = @wa_0045-zseq_inst
*        AND objek       = @wa_0045-objek
*        AND objecttable = @wa_0045-objecttable.
*
*     IF sy-subrc = 0.
*       IF wa_0045-safra          = w_0045_old-safra          AND
*          wa_0045-peso_max       = w_0045_old-peso_max       AND
*          wa_0045-data_in_porto  = w_0045_old-data_in_porto  AND
*          wa_0045-data_porto     = w_0045_old-data_porto     AND
*          wa_0045-instrucao      = w_0045_old-instrucao      AND
*          wa_0045-terminal_estuf = w_0045_old-terminal_estuf AND
*          wa_0045-observacao     = w_0045_old-observacao     AND
*          wa_0045-werks          = w_0045_old-werks          AND
*          wa_0045-charg          = w_0045_old-charg          AND
*          wa_0045-quantidade     = w_0045_old-quantidade.
*         CONTINUE.
*       ENDIF.
*     ENDIF.

      DATA(l_task) = 'TRACE_INSTRUCAO' && wa_0045-zseq_inst && wa_0045-objek && wa_0045-objecttable.

      CALL FUNCTION 'ZSD_ENVIO_INSTRUCAO_TRACE' STARTING NEW TASK l_task
        EXPORTING
          i_zseq_inst   = wa_0045-zseq_inst
          i_objek       = wa_0045-objek
          i_objecttable = wa_0045-objecttable
          i_acao        = 'C'
        EXCEPTIONS
          OTHERS        = 1.

      wa_0045-status_trace = icon_activity.
      MODIFY it_0045 FROM wa_0045 INDEX w_index-index TRANSPORTING status_trace.

    ENDLOOP.

    CALL METHOD obj_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

  METHOD act_dele_inst.

    DATA(index) = obj_inst->get_index( ).

    TRY .
        DATA(wa) = it_0045[ index ].
      CATCH cx_sy_itab_line_not_found.
        EXIT.
    ENDTRY.

    IF obj_inst->check_vbeln( wa ) IS INITIAL.
      MESSAGE 'Instrução que possui Formação de Lote não podem ser Deletadas!' TYPE 'S' DISPLAY LIKE 'E'.
      me->clear( ).
      EXIT.
    ENDIF.

    TRY .
        CHECK obj_inst->confirm(
                                obj_inst->remove_zero(
                                                      it_0045[ obj_inst->get_index( ) ]-zseq_inst
                                                      )
                               ) EQ 1.
      CATCH cx_sy_itab_line_not_found.
        EXIT.
    ENDTRY.

    APPEND CORRESPONDING #( it_0045[ obj_inst->get_index( ) ] ) TO it_dele.

    SELECT *
      FROM zsdt0182
      APPENDING TABLE it_0182_d
      WHERE id_inst EQ wa-zseq_inst.

    DELETE it_0045 INDEX obj_inst->get_index( ).

  ENDMETHOD.

  METHOD confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = |Deseja Deletar o Nr. Documento { input }?|
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        display_cancel_button = ' '
      IMPORTING
        answer                = r_value.

  ENDMETHOD.

  METHOD remove_zero.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = input
      IMPORTING
        output = r_value.

  ENDMETHOD.

  METHOD modify_alv.

    CHECK lv_acao CS 'EDIT'.

    LOOP AT it_0045 ASSIGNING FIELD-SYMBOL(<f0045>) WHERE zseq_inst EQ it_new-zseq_inst.
      CHECK NOT <f0045>-zseq_inst IS INITIAL.
      MOVE-CORRESPONDING it_new TO <f0045>.
    ENDLOOP.

    "====================================Comentado / Ajuste BUG SOLTO 121844*/ AOENNING
*    CHECK it_0051-vkorg EQ '0001'.
    "====================================Comentado / Ajuste BUG SOLTO 121844*/ AOENNING

*-CS2022000332-#78810-30.04.2022-JT-inicio
*    LOOP AT it_0045 ASSIGNING <f0045> WHERE instrucao EQ it_new-instrucao.
*     <f0045>-charg      = it_new-charg.

    "====================================Comentado / Ajuste BUG SOLTO 121844*/ AOENNING
*      <f0045>-matnr      = it_new-matnr.
*      <f0045>-ponto_c    = it_new-ponto_c.
    "====================================Comentado / Ajuste BUG SOLTO 121844*/ AOENNING

*     <f0045>-quantidade = it_new-quantidade.
*     <f0045>-btgew      = it_new-btgew.
*    ENDLOOP.
*-CS2022000332-#78810-30.04.2022-JT-fim

  ENDMETHOD.

  METHOD  en_de_queue.

    MOVE-CORRESPONDING obj_inst->get_header( nro_sol_ov = s_solov-low
                                            ) TO it_0051.

    it_0051-%nro_sol_ov = it_0051-nro_sol_ov.

    DATA function TYPE string.
    function = |{ dir }QUEUE_EZSDT0045|.

    CALL FUNCTION function
      EXPORTING
        objek          = it_0051-%nro_sol_ov
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    CHECK sy-subrc IS NOT INITIAL.
    MESSAGE |Nº de Solicitação Bloqueado pelo Usuário { sy-msgv1 }| TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE TO CURRENT TRANSACTION.

  ENDMETHOD.

  METHOD get_excel.

    DATA: it_file TYPE filetable,
          l_subrc TYPE i.

    cl_gui_frontend_services=>file_open_dialog( EXPORTING default_filename = ' '
                                                          file_filter      = '*.xls'
                                                CHANGING  file_table       = it_file
                                                          rc               = l_subrc
                                                        ).
    TRY .
        r_value = it_file[ 1 ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR r_value.
    ENDTRY.

  ENDMETHOD.

  METHOD set_excel.

    CHECK input IS NOT INITIAL.

    FREE: it_excel[], excel_tab[].

    CLASS cl_abap_char_utilities DEFINITION LOAD.

    ld_separator = cl_abap_char_utilities=>horizontal_tab.

    IF application-header = space OR application-handle = -1.
      CREATE OBJECT application 'Excel.Application'.
      m_message.
    ENDIF.

    CALL METHOD OF application 'Workbooks' = workbook.
    m_message.
    CALL METHOD OF workbook 'Open' EXPORTING #1 = input.
    m_message.
    GET PROPERTY OF  application 'ACTIVESHEET' = worksheet.
    m_message.
    CALL METHOD OF worksheet 'Cells' = h_cell
        EXPORTING #1 = 1 #2 = 1.
    m_message.
    CALL METHOD OF worksheet 'Cells' = h_cell1
        EXPORTING #1 = 10000 #2 = 46.
    m_message.

    CALL METHOD  OF worksheet 'RANGE' = range
                   EXPORTING #1 = h_cell #2 = h_cell1.
    m_message.
    CALL METHOD OF range 'SELECT'.
    m_message.
    CALL METHOD OF range 'COPY'.
    m_message.

    cl_gui_frontend_services=>clipboard_import( IMPORTING data = excel_tab ).
    IF sy-subrc <> 0.
      MESSAGE a037(alsmex).
    ENDIF.

    LOOP AT excel_tab INTO DATA(wa).

      DATA(cont) = sy-tabix.
      SPLIT wa AT ld_separator INTO TABLE it_dados.
      it_aux = VALUE #( FOR ls IN it_dados (
                         row   = cont
                         col   = col( )
                         value = ls
                        ) ).
      CLEAR cont_col.
      APPEND LINES OF it_aux TO it_excel.
    ENDLOOP.

    FREE excel_tab.
    cl_gui_frontend_services=>clipboard_export( IMPORTING data = excel_tab
                                                CHANGING  rc   = ld_rc
                                                        ).

    CALL METHOD OF application 'QUIT'.
    m_message.

    FREE OBJECT h_cell.       m_message.
    FREE OBJECT h_cell1.      m_message.
    FREE OBJECT range.        m_message.
    FREE OBJECT worksheet.    m_message.
    FREE OBJECT workbook.     m_message.
    FREE OBJECT application.  m_message.

  ENDMETHOD.

  METHOD exc_excel.

    FREE: it_import[].

    CLEAR gvr_check.

    obj_inst->set_excel( obj_inst->get_excel( ) ).

    LOOP AT it_excel INTO DATA(lwa_excel)
      WHERE row EQ 1.

      READ TABLE git_colum_excel INTO DATA(lwa_colum_excel) WITH KEY name = lwa_excel-value.
      IF sy-subrc NE 0.
        MESSAGE 'Planilha importada não esta no formato padrão!' TYPE 'E'.
        EXIT.
      ENDIF.

    ENDLOOP.


    LOOP AT it_excel INTO DATA(wa_excel).
      CHECK wa_excel-row NE 1.

      CASE wa_excel-col.
        WHEN 2.   wa_import-charg            = wa_excel-value.                         "LOTE
        WHEN 3.   wa_import-matnr            = wa_excel-value.                         "MATERIAL
        WHEN 4.   wa_import-quantidade       = wa_excel-value.                         "FARDOS
        WHEN 5.   wa_import-btgew            = obj_inst->chartonum( wa_excel-value ).  "PESO
        WHEN 6.   wa_import-contrato         = wa_excel-value.                         "CONTRATO
        WHEN 7.   wa_import-dmbtr            = obj_inst->chartonum( wa_excel-value ).  "PREÇO
        WHEN 8.   wa_import-instrucao        = wa_excel-value.                         "INSTRUCAO
        WHEN 9.   wa_import-bukrs            = wa_excel-value.                         "EMPRESA
        WHEN 10.  wa_import-werks            = wa_excel-value.                         "CENTRO
        WHEN 11.  wa_import-voleh            = wa_excel-value.                         "UN FARDOS
        WHEN 12.  wa_import-terminal         = wa_excel-value.                         "TERMINAL DE EMBARQUE
        WHEN 13.  wa_import-ponto_c          = wa_excel-value.                         "PONTO DE COLETA
        WHEN 14.  wa_import-pmein            = wa_excel-value.                         "UN MOEDA
        WHEN 15.  wa_import-gewei            = wa_excel-value.                         "UN PESO
        WHEN 16.  wa_import-data_instr       = obj_inst->chartodate( wa_excel-value ). "DATA INSTRUCAO
        WHEN 17.  wa_import-data_retirada    = obj_inst->chartodate( wa_excel-value ). "DATA RETIRADA
        WHEN 18.  wa_import-data_in_porto    = obj_inst->chartodate( wa_excel-value ). "DATA INICIAL
        WHEN 19.  wa_import-data_porto       = obj_inst->chartodate( wa_excel-value ). "DATA FINAL
        WHEN 20.  wa_import-deadline_draft   = obj_inst->chartodate( wa_excel-value ). "DDLINE DRAFT
        WHEN 21.  wa_import-deadline_documen = obj_inst->chartodate( wa_excel-value ). "DDLINE CARGA
        WHEN 22.  wa_import-porto_embarque   = wa_excel-value.                         "PORTO EMBARQUE
        WHEN 23.  wa_import-safra            = wa_excel-value.                         "SAFRA
        WHEN 24.  wa_import-observacao       = wa_excel-value.                         "OBSERVAÇÃO
        WHEN 25.  wa_import-booking          = wa_excel-value.                         "BOOKING
        WHEN 26.  wa_import-mapa             = wa_excel-value.                         "MAPA
        WHEN 27.  wa_import-fumigacao        = wa_excel-value.                         "FUMIGACAO
        WHEN 28.  wa_import-hrs_fgacao       = wa_excel-value.                         "HORAS FUMIGAÇÃO
        WHEN 29.  wa_import-armador          = wa_excel-value.                         "ARMADOR
        WHEN 30.  wa_import-free_time        = wa_excel-value.                         "FREE TIME
        WHEN 31.  wa_import-qtd_ctners       = wa_excel-value.                         "QTD CONTAINERS
        WHEN 32.  wa_import-cod_despach      = wa_excel-value.                         "DESPACHANTE
        WHEN 33.  wa_import-vlr_frete        = obj_inst->chartonum( wa_excel-value ).  "VALOR FRETE
        WHEN 34.  wa_import-cod_transp       = wa_excel-value.                         "TRANSPORTADORA
        WHEN 35.  wa_import-data_eta         = obj_inst->chartodate( wa_excel-value ). "DATA DO ETA
        WHEN 36.  wa_import-pais_des         = wa_excel-value.                         "PAÍS DESTINO
        WHEN 37.  wa_import-terminal_estuf   = wa_excel-value.                         "TERMINAL ESTUFAGEM
        WHEN 38.  wa_import-controladora     = wa_excel-value.                         "CONTROLADORA
        WHEN 39.  wa_import-navio            = wa_excel-value.                         "NAVIO
        WHEN 40.  wa_import-tamanho_fardo    = wa_excel-value.                         "TIPO FARDO
        WHEN 41.  wa_import-data_container   = obj_inst->chartodate( wa_excel-value ). "DATA CONTAINER
        WHEN 42.  wa_import-limite_peso      = wa_excel-value.                         "LIMITE DE PESO
        WHEN 43.
          wa_import-limite_peso = COND #( WHEN wa_import-limite_peso IS INITIAL THEN 'N' ELSE wa_import-limite_peso ).
          wa_import-peso_max         = obj_inst->chartonum( wa_excel-value ).  "PESO MAXIMO
          wa_import-peso_max = SWITCH #( wa_import-limite_peso WHEN 'S' THEN wa_import-peso_max
                                                               ELSE 0
                                        ).

          MOVE-CORRESPONDING obj_inst->get_header(
                                                    nro_sol_ov = s_solov-low
                                                  ) TO it_0051.

          wa_import-objek           = it_0051-nro_sol_ov.
          wa_import-objecttable     = 'ZSDT0051'.
          wa_import-data_criacao    = sy-datum.
          wa_import-usuario         = sy-uname.

          wa_import-color           = 'C510'.

          IF wa_import-bukrs NE '0001'.
            CLEAR: wa_import-limite_peso, wa_import-peso_max.
          ENDIF.

          obj_inst->set_desc( ).

          APPEND wa_import TO it_import.

          gvr_check = abap_true.
      ENDCASE.

    ENDLOOP.

    IF gvr_check EQ abap_true.
      APPEND LINES OF it_import[] TO it_0045[].
    ELSE.
      MESSAGE 'Planilha importada com campos incompletos. Verifique os dados informado!' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    IF REDUCE i( INIT x = 0 FOR ls IN it_import WHERE ( contrato = it_0051-bstkd ) NEXT x = x + 1 )
    NE REDUCE i( INIT x = 0 FOR ls IN it_import                                    NEXT x = x + 1 ) .
      FREE it_import.
      MESSAGE 'Existe Contrato Diferente do Selecionado!' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.

  METHOD exc_layout_excel.

    CLEAR: lv_filename, lv_path, lv_fullpath, lv_file, lv_content, ls_stream, ls_header.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title      = 'Enter File Name'
        default_extension = 'xlsx'
        default_file_name = 'Instrucao'
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_fullpath.

    lv_file = lv_fullpath.


    GET REFERENCE OF git_fieldnames INTO lr_excel_structure.
    DATA(lo_itab_services) = cl_salv_itab_services=>create_for_table_ref( lr_excel_structure ).
    lo_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( lr_excel_structure  ).
    lo_table_row_descriptor ?= lo_source_table_descr->get_table_line_type( ).
    DATA(lt_fields) = lo_table_row_descriptor->get_ddic_field_list( p_langu = sy-langu  ) .

    "excel instantiate
    DATA(lo_tool_xls) = cl_salv_export_tool_ats_xls=>create_for_excel(
      EXPORTING
        r_data = lr_excel_structure ).

    DATA(lo_config) = lo_tool_xls->configuration( ).
    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<lfs_field>) .
      lo_config->add_column(
        EXPORTING
          header_text  = CONV string( <lfs_field>-fieldtext )
          field_name   = CONV string( <lfs_field>-fieldname )
          display_type = if_salv_bs_model_column=>uie_text_view ).
    ENDLOOP .

    "get excel in xstring
    lo_tool_xls->read_result(  IMPORTING content  = lv_content  ).

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_content
      IMPORTING
        output_length = lv_length
      TABLES
        binary_tab    = lt_binary_tab.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = lv_length
        filename                = lv_file
        filetype                = 'BIN'
      TABLES
        data_tab                = lt_binary_tab
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc EQ 0.
      MESSAGE 'Planilha exportada com sucesso!' TYPE 'S'.
    ENDIF.

  ENDMETHOD.


  METHOD chartonum.

    CALL FUNCTION 'MOVE_CHAR_TO_NUM'
      EXPORTING
        chr             = input
      IMPORTING
        num             = r_value
      EXCEPTIONS
        convt_no_number = 1
        convt_overflow  = 2
        OTHERS          = 3.

  ENDMETHOD.

  METHOD chartodate.
    r_value = |{ input+4(4) }{ input+2(2) }{ input(2) }|.
  ENDMETHOD.

  METHOD col.
    ADD 1 TO cont_col.
    r_value = cont_col.
  ENDMETHOD.

  METHOD check_vbeln.

    DATA: vmatnr18         TYPE matnr18.
    DATA: nro_sol_ov       TYPE zsded013,
          lit_0045         TYPE TABLE OF zsdt0045,
          lva_qtd_0045     TYPE zsdt0045-quantidade,
          lva_qtd_0066     TYPE zsdt0066-volum,
          lva_qtd_restante TYPE zsdt0066-volum.

    nro_sol_ov = input-objek.
    vmatnr18   = |{ input-matnr ALPHA = IN }|.

    SELECT SUM( volum )
      FROM zsdt0066
      INTO lva_qtd_0066
          WHERE nro_sol_ov EQ nro_sol_ov
            AND instrucao  EQ input-instrucao
            AND matnr      EQ vmatnr18 "input-matnr
            AND werks      EQ input-werks
            AND status     NE 'D'.

    SELECT *
      FROM zsdt0045
      INTO TABLE @lit_0045
      WHERE objek    EQ @nro_sol_ov
      AND instrucao  EQ @input-instrucao
      AND matnr      EQ @vmatnr18 "input-matnr
      AND werks      EQ @input-werks.

    LOOP AT  lit_0045 INTO DATA(lwa_0045).
      lva_qtd_0045 = lva_qtd_0045 + lwa_0045-quantidade.
    ENDLOOP.

    lva_qtd_restante = lva_qtd_0045 - input-quantidade.

    IF lva_qtd_restante GE lva_qtd_0066.
      subrc = 4.
    ELSE.
      subrc = 0.
    ENDIF.

*    SELECT COUNT(*) FROM ZSDT0066
*      WHERE NRO_SOL_OV EQ NRO_SOL_OV
*        AND INSTRUCAO  EQ INPUT-INSTRUCAO
*        AND MATNR      EQ INPUT-MATNR
*        AND WERKS      EQ INPUT-WERKS
*        AND STATUS     NE 'D'.

    "SUBRC = SY-SUBRC.

  ENDMETHOD.

  METHOD get_mard.

    SELECT COUNT(*)
      FROM mard
        WHERE matnr EQ input-matnr
          AND werks EQ input-werks
          AND lgort EQ input-charg(4).

    return = sy-subrc.

  ENDMETHOD.

  METHOD check_botao.

    IF lv_acao CS 'BNT_'.

      LOOP AT SCREEN.
        IF screen-name CS 'BNT_' AND screen-name NE input.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    ELSEIF lv_acao EQ 'INSTRUCAO'.

      LOOP AT SCREEN.
        IF screen-name CS 'BNT_'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    ENDIF.

    IF it_new-limite_peso NE 'S'.
      LOOP AT SCREEN.
        IF screen-name EQ 'IT_NEW-PESO_MAX'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

*-CS2021000532-#59109-08.08.2022-JT-inicio
    LOOP AT SCREEN.
      IF screen-name = 'BNT_DEL' OR
         screen-name = 'BNT_EDIT'.
        IF it_0045_temp[] IS NOT INITIAL.
          screen-input = 0.
          MODIFY SCREEN.
        ELSE.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
      IF lv_importa = abap_true.
        IF screen-name = 'BNT_DEL' OR
           screen-name = 'BNT_ADD'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
*-CS2021000532-#59109-08.08.2022-JT-fim

*    LOOP AT SCREEN.
*      IF SCREEN-GROUP1 EQ 'G1' AND IT_NEW-BUKRS NE '0001'.
*        SCREEN-INPUT = 0.
*        SCREEN-INVISIBLE = 1.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.

  METHOD check_0182.

    return = 4.

    CHECK input IS NOT INITIAL.

    SELECT COUNT(*)
       FROM zsdt0182
       WHERE id_inst EQ input.

    return = sy-subrc.

  ENDMETHOD.

  METHOD get_saldo_fardos.

    DATA qtd_vinculado TYPE int4.

    SELECT SINGLE id_trace
       FROM zsdt0182
       INTO @DATA(_id_trace)
       WHERE id_inst EQ @input.

    IF sy-subrc IS INITIAL.
      SELECT *
         FROM zsdt0182
         INTO TABLE @DATA(it_0182)
         WHERE id_trace EQ @_id_trace.
    ENDIF.

    qtd_vinculado = REDUCE #( INIT x = 0 FOR ls IN it_0182 WHERE ( id_inst NE input ) NEXT x = x + ls-qtd_fardos ).

    SELECT SINGLE qtd_fardos
      FROM zsdt0166
      INTO @DATA(qtd_fardos)
      WHERE id EQ @_id_trace.

    return = qtd_fardos - qtd_vinculado.

  ENDMETHOD.

  METHOD get_saldo_peso.

    DATA peso_vinculado TYPE dmbtr.

    SELECT SINGLE id_trace
       FROM zsdt0182
       INTO @DATA(_id_trace)
       WHERE id_inst EQ @input.

    IF sy-subrc IS INITIAL.
      SELECT *
         FROM zsdt0182
         INTO TABLE @DATA(it_0182)
         WHERE id_trace EQ @_id_trace.
    ENDIF.

    peso_vinculado = REDUCE #( INIT x = peso_vinculado FOR ls IN it_0182 WHERE ( id_inst NE input ) NEXT x = x + ls-peso ).

    SELECT SINGLE peso_lote
      FROM zsdt0166
      INTO @DATA(qtd_peso)
      WHERE id EQ @_id_trace.

    return = qtd_peso - peso_vinculado.

  ENDMETHOD.

  METHOD valida_existe_lfa1.

    SELECT SINGLE * FROM lfa1 INTO @DATA(ls_lfa1)
        WHERE lifnr = @cod_lifnr.

    lifnr_existente = COND #( WHEN ls_lfa1 IS INITIAL OR cod_lifnr IS INITIAL THEN abap_false
                              ELSE abap_true ).

  ENDMETHOD.

  METHOD clear.
    CLEAR lv_acao.
    FREE it_instrucao.
  ENDMETHOD.

ENDCLASS.

***********************************************************************************************
* SELEÇÃO DE DADOS
***********************************************************************************************
START-OF-SELECTION.

  PERFORM preenche_field.

  obj_inst->en_de_queue( input = s_solov-low
                         dir   = 'EN' ).
  CHECK obj_inst->get_dados( ) IS INITIAL.
  CALL SCREEN 0100.
*  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TI0100'.

  obj_inst->check_botao( lv_acao ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      obj_inst->en_de_queue( input = s_solov-low
                             dir   = 'DE' ).
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      obj_inst->en_de_queue( input = s_solov-low
                             dir   = 'DE' ).
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'CANCEL'.
      obj_inst->get_dados( ).
      CLEAR it_new.
      obj_inst->clear( ).
    WHEN 'SAVE'.
      obj_inst->save( ).

      "Forçar o reenvio dos emails
      SELECT * FROM zsdt0045
        INTO TABLE @DATA(it_0045_mail)
      WHERE objek = @s_solov-low
        AND objecttable = 'ZSDT0051'
        AND e_mail EQ @abap_false.

      IF it_0045_mail[] IS NOT INITIAL.
        NEW zcl_solicitacao_ov( )->envio_automatico( p_nro_sol_ov = s_solov-low
                                                     i_zsdt0045   = it_0045_mail[]
                                                     i_tcode      = sy-tcode
                                                     ).
      ENDIF.

      IF tg_msg_ret[] IS INITIAL.
        obj_inst->get_dados( ).
      ENDIF.

    WHEN 'BNT_AUT'.

      DATA: t_index     TYPE lvc_t_row.

      FREE: t_index, it_0045_mail.

      CALL METHOD obj_alv->get_selected_rows
        IMPORTING
          et_index_rows = t_index.

      it_0045_mail = VALUE #( FOR ls_index IN t_index
                              FOR ls_0045 IN it_0045 FROM ls_index-index TO ls_index-index
                              ( CORRESPONDING #( ls_0045 ) )
                            ).

      LOOP AT it_0045_mail ASSIGNING FIELD-SYMBOL(<f_0045_mail>).
        <f_0045_mail>-e_mail = abap_false.
      ENDLOOP.

      IF it_0045_mail[] IS NOT INITIAL.
        NEW zcl_solicitacao_ov( )->envio_automatico( p_nro_sol_ov = s_solov-low
                                                     i_zsdt0045   = it_0045_mail[]
                                                     i_tcode      = sy-tcode
                                                     ).
      ENDIF.

      obj_inst->get_dados( ).

    WHEN 'BNT_ADD'.
      lv_acao = sy-ucomm.
      obj_inst->show_msgre( ).   "*-CS2021000532-#59109-08.08.2022-JT
      lv_acao = sy-ucomm.
      obj_inst->act_new_inst( ).
    WHEN 'BNT_FRETE'.
      obj_inst->cotacao_frete( ).
    WHEN 'BNT_EDIT'.
      lv_acao2 = sy-ucomm.
      obj_inst->act_edit_inst( ).
      lv_acao = sy-ucomm.
    WHEN 'BNT_DEL'.
      lv_acao = sy-ucomm.
      obj_inst->act_dele_inst( ).
    WHEN 'SHOW_MSGRE'.
      lv_acao = sy-ucomm.
      obj_inst->show_msgre( ).
    WHEN 'BNT_EXCEL'.
      lv_acao = sy-ucomm.
      obj_inst->exc_excel( ).
    WHEN 'BNT_LAYOUT'.
      obj_inst->exc_layout_excel( ).
    WHEN 'BTNP'.
      IF it_new-limite_peso NE 'S'.
        it_new-peso_max = 0.
      ENDIF.
*-CS2023000189-06.04.2023-#108697-JT-inicio
    WHEN 'BNT_TRACE'.
      obj_inst->act_reenvia_trace( ).
*-CS2023000189-06.04.2023-#108697-JT-fim
  ENDCASE.

  CALL METHOD obj_alv->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJ  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_fcat OUTPUT.

  FREE it_fcat.

  ASSIGN 'ZSDT0045' TO FIELD-SYMBOL(<fs_str>).
  CREATE DATA str TYPE (<fs_str>).

  it_fcat = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

  ADD 1 TO cont.

  LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>).
*    <FCAT>-NO_OUT = ABAP_TRUE.

    LOOP AT it_0045 ASSIGNING FIELD-SYMBOL(<f45>).

      campo = <fcat>-fieldname.
      ASSIGN COMPONENT campo OF STRUCTURE <f45> TO <fs_campo1>.

      WRITE <fs_campo1> TO str_l.
      IF strlen( str_l ) > tam.
        tam = strlen( str_l ).
      ENDIF.

    ENDLOOP.

    CLEAR campo.
    ADD 1 TO cont.
    campo = |TEXT-{ cont }|.
    ASSIGN (campo) TO <fs_campo>.

*    IF CONT BETWEEN 47 AND 52 AND IT_0051-VKORG EQ '0015'.
*      <FCAT>-NO_OUT = ABAP_TRUE.
*    ENDIF.
*    IF ( CONT EQ '53' OR CONT EQ '54' ) AND IT_0051-VKORG EQ '0001'.
*      <FCAT>-NO_OUT = ABAP_TRUE.
*    ENDIF.

    IF <fs_campo> IS ASSIGNED.
      <fcat>-reptext = <fcat>-scrtext_s = <fcat>-scrtext_m = <fcat>-scrtext_l = <fs_campo>.
    ENDIF.

*** Inicio - Rubenilson Pereira - 14.02.25 - US164130
    CASE <fcat>-fieldname.
      WHEN 'LOCAL_ENTREGA'.
        <fcat>-reptext   = 'LocalEntre'.
        <fcat>-scrtext_s = 'Local Entrega'.
        <fcat>-scrtext_m = 'Local Entrega'.
        <fcat>-scrtext_l = 'Local Entrega'.

      WHEN 'OPER_LOG'.

        <fcat>-reptext   = 'OperLog'.
        <fcat>-scrtext_s = 'Operador Log.'.
        <fcat>-scrtext_m = 'Operador Logístico'.
        <fcat>-scrtext_l = 'Operador Logístico'.

      WHEN 'INCOTERM'.

        <fcat>-reptext   = 'Incoterm'.
        <fcat>-scrtext_s = 'Incoterm'.
        <fcat>-scrtext_m = 'Incoterm'.
        <fcat>-scrtext_l = 'Incoterm'.

      WHEN OTHERS.
    ENDCASE.
*** Fim - Rubenilson Pereira - 14.02.25 - US164130

    IF <fcat>-inttype EQ 'P'.
      <fcat>-outputlen = <fcat>-intlen + 1 .
    ELSE.
      IF tam IS INITIAL.
        <fcat>-outputlen = <fcat>-intlen  + 1.
      ELSE.
        <fcat>-outputlen = tam  + 1.
      ENDIF.
    ENDIF.

*-CS2023000189-06.04.2023-#108697-JT-inicio
    IF <fcat>-fieldname = 'ACTS'.
      <fcat>-checkbox  = abap_true.
      <fcat>-reptext   = 'ACTS'.
      <fcat>-scrtext_s = 'ACTS'.
      <fcat>-scrtext_m = 'ACTS'.
      <fcat>-scrtext_l = 'ACTS'.
      <fcat>-col_opt   = abap_off.
      <fcat>-outputlen = 7.
    ENDIF.
*-CS2023000189-06.04.2023-#108697-JT-fim

    CLEAR tam.
  ENDLOOP.

*-CS2023000189-06.04.2023-#108697-JT-inicio
  LOOP AT it_fcat  INTO wa_fcat.
    wa_fcat-col_pos   = 10.
    MODIFY it_fcat FROM wa_fcat.
  ENDLOOP.

  wa_fcat-col_pos   = 1.
  wa_fcat-fieldname = 'STATUS_TRACE'.
  wa_fcat-reptext   = 'Status Trace'.
  wa_fcat-scrtext_s = 'Status Trace'.
  wa_fcat-scrtext_m = 'Status Trace'.
  wa_fcat-scrtext_l = 'Status Trace'.
  wa_fcat-outputlen = 4.
  wa_fcat-checkbox  = abap_false.
  wa_fcat-hotspot   = abap_true.
  wa_fcat-icon      = abap_true.
* wa_fcat-just      = abap_true.
  APPEND wa_fcat   TO it_fcat.
*-CS2023000189-06.04.2023-#108697-JT-fim

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  POPULA_OBJ  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE popula_obj OUTPUT.

  IF obj_cont IS INITIAL.

    CREATE OBJECT obj_cont
      EXPORTING
        container_name = 'CC_INTRUCAO'.

    CREATE OBJECT obj_alv
      EXPORTING
        i_shellstyle    = 0
        i_parent        = obj_cont
        i_appl_events   = abap_false
        i_fcat_complete = abap_false.

    obj_inst->set_layout( ).

    SET HANDLER: obj_inst->on_toolbar          FOR obj_alv,
                 obj_inst->handle_user_command FOR obj_alv,
                 obj_inst->on_data_ch          FOR obj_alv,
                 obj_inst->on_data_ch_f        FOR obj_alv,
                 obj_inst->catch_hotspot       FOR obj_alv. "*-CS2023000189-06.04.2023-#108697-JT

    CALL METHOD obj_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        is_variant                    = wa_variant
        i_save                        = abap_true
      CHANGING
        it_outtab                     = it_0045[]
        it_fieldcatalog               = it_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.

    IF lv_acao = 'BNT_EDIT' OR sy-ucomm = 'SAVE' OR lv_acao2 = 'BNT_EDIT'. " OR sy-ucomm = 'SAVE'.
      IF lv_importa = abap_true.
        READ TABLE it_0045 TRANSPORTING NO FIELDS WITH KEY zseq_inst   = it_new-zseq_inst
                                                           objek       = it_new-objek
                                                           objecttable = it_new-objecttable
                                                           linha_edit  = it_new-linha_edit.
      ELSE.
        READ TABLE it_0045 TRANSPORTING NO FIELDS WITH KEY zseq_inst   = it_new-zseq_inst
                                                           objek       = it_new-objek
                                                           objecttable = it_new-objecttable.
      ENDIF.
      IF sy-subrc = 0 AND ( it_new-bukrs IS NOT INITIAL AND
                            it_new-werks IS NOT INITIAL AND
                            it_new-matnr IS NOT INITIAL ).
        MODIFY it_0045 FROM it_new INDEX sy-tabix.
      ENDIF.
    ENDIF.

    CALL METHOD obj_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRL_REG_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE crl_reg_modify INPUT.

  obj_inst->modify_alv( ).
  obj_inst->set_desc( ).
* obj_inst->set_erros( ).

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen    = '100'
      i_repid     = sy-repid
      i_set_field = 'X_FIELD'
    IMPORTING
      e_messagem  = wg_mensagem
    TABLES
      it_msgs     = tg_msg_ret.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_OBRIGATORIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_obrigatorio INPUT.

  obj_inst->modify_alv( ).
  obj_inst->set_desc( ).
* obj_inst->set_erros( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.

  SET PF-STATUS 'T_0300'.

  FREE: gt_editor.

  IF NOT obj_custom_txt IS INITIAL AND NOT obj_custom_editor IS INITIAL.
    CALL METHOD obj_custom_txt->free( ).
    CALL METHOD obj_custom_editor->free( ).
  ENDIF.

  CREATE OBJECT: obj_custom_txt    EXPORTING container_name = 'C_EMAIL',
                 obj_custom_editor EXPORTING
                                              wordwrap_mode     = 1
                                              wordwrap_position = 76
                                              max_number_chars  = 200
                                              parent         = obj_custom_txt.

  CALL METHOD obj_custom_editor->set_toolbar_mode( toolbar_mode = obj_custom_editor->false ).
  CALL METHOD obj_custom_editor->set_statusbar_mode( statusbar_mode = obj_custom_editor->false ).
  CALL METHOD obj_custom_editor->set_readonly_mode( readonly_mode = obj_custom_editor->false ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0300 INPUT.

  CASE sy-ucomm.
    WHEN 'OK'.

      CALL METHOD obj_custom_editor->get_text_as_stream
        IMPORTING
          text = gt_editor.

      NEW zcl_solicitacao_ov( )->check_email( EXPORTING i_table = gt_editor
                                              IMPORTING e_table = gt_email
                                                       ).

      NEW zcl_solicitacao_ov( )->envio_frete( EXPORTING t_0045 = obj_inst->it_frete
                                                        email  = gt_email
                                                       ).
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      MESSAGE |Operação Cancelada pelo Usuário!| TYPE 'S' DISPLAY LIKE 'W'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_QTD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_qtd INPUT.
* obj_inst->set_erros( ).

  IF tg_msg_ret[] IS INITIAL.
    obj_inst->modify_alv( ).
  ENDIF.

  SELECT SINGLE *
     FROM zsdt0182
     INTO @DATA(wa_0182)
     WHERE id_inst EQ @it_new-zseq_inst.

  CHECK sy-subrc IS INITIAL.

  wa_0182-qtd_fardos = it_new-quantidade.

  IF line_exists( it_0182[ id_inst = it_new-zseq_inst ] ).
    MODIFY it_0182 FROM wa_0182 TRANSPORTING qtd_fardos WHERE id_inst EQ it_new-zseq_inst.
  ELSE.
    APPEND wa_0182 TO it_0182.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_PESO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_peso INPUT.
* obj_inst->set_erros( ).

* IF it_new-btgew > 999999998.
*   MESSAGE e024(sd) WITH 'Revisar o peso Informado!'.
*   EXIT.
* ENDIF.

  IF tg_msg_ret[] IS INITIAL.
    obj_inst->modify_alv( ).
  ENDIF.

  SELECT SINGLE *
     FROM zsdt0182
     INTO wa_0182
     WHERE id_inst EQ it_new-zseq_inst.

  CHECK sy-subrc IS INITIAL.

  wa_0182-peso = it_new-btgew.

  IF line_exists( it_0182[ id_inst = it_new-zseq_inst ] ).
    MODIFY it_0182 FROM wa_0182 TRANSPORTING peso WHERE id_inst EQ it_new-zseq_inst.
  ELSE.
    APPEND wa_0182 TO it_0182.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PESO_MAX  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE peso_max INPUT.
  IF it_new-limite_peso NE 'S'.
    it_new-peso_max = 0.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  INSTRUCAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE instrucao INPUT.

  it_new_aux = obj_inst->get_instrucao( input = it_new-instrucao safra = it_new-safra_sel ).
  IF  it_new_aux IS NOT INITIAL.
    it_new_aux-zseq_inst   = it_new-zseq_inst. "ok
    it_new_aux-objek       = it_new-objek. "ok
    it_new_aux-objecttable = it_new-objecttable. "ok
    it_new_aux-bukrs       = it_new-bukrs. "ok
    it_new_aux-werks       = it_new-werks. "ok
    it_new_aux-contrato    = it_new-contrato. "ok
    it_new_aux-quantidade  = it_new-quantidade. "ok
    it_new_aux-voleh       = it_new-voleh. "ok
    it_new_aux-matnr       = it_new-matnr. "ok
    it_new_aux-charg       = it_new-charg. "ok
    it_new_aux-tamanho_fardo = it_new-tamanho_fardo. "ok
    it_new_aux-dmbtr       = it_new-dmbtr. "ok
    it_new_aux-pmein       = it_new-pmein. "ok
    it_new_aux-btgew       = it_new-btgew. "ok
    it_new_aux-gewei       = it_new-gewei. "ok
    it_new_aux-ponto_c     = it_new-ponto_c. "ok
    MOVE-CORRESPONDING it_new_aux TO it_new.
  ELSE.
*   it_new_aux-bukrs       = it_new-bukrs. "ok
*   it_new_aux-werks       = it_new-werks. "ok
*   it_new_aux-contrato    = it_new-contrato. "ok
*   it_new_aux-instrucao   = it_new-instrucao. "ok
*   MOVE-CORRESPONDING it_new_aux TO it_new.    "*-CS2023000189-06.04.2023-#108697-JT
  ENDIF.

  it_new-limite_peso = COND #( WHEN obj_inst->get_limite_peso( it_new-instrucao ) EQ 'S' THEN obj_inst->get_limite_peso( it_new-instrucao ) ELSE 'N' ).
  it_new-peso_max = COND #( WHEN it_new-limite_peso EQ 'S' THEN CONV #( obj_inst->get_peso_max( it_new-instrucao ) ) ELSE '0' ).
ENDMODULE.


FORM preenche_field.

  REFRESH git_colum_excel.

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen    = '100'
      i_repid     = sy-repid
      i_set_field = 'X_FIELD'
    IMPORTING
      e_messagem  = wg_mensagem
    TABLES
      it_msgs     = tg_msg_ret.

  GET REFERENCE OF git_fieldnames INTO lr_excel_structure.
  DATA(lo_itab_services) = cl_salv_itab_services=>create_for_table_ref( lr_excel_structure ).
  lo_source_table_descr ?= cl_abap_tabledescr=>describe_by_data_ref( lr_excel_structure  ).
  lo_table_row_descriptor ?= lo_source_table_descr->get_table_line_type( ).
  DATA(lt_fields) = lo_table_row_descriptor->get_ddic_field_list( p_langu = sy-langu  ) .


  DATA(lo_tool_xls) = cl_salv_export_tool_ats_xls=>create_for_excel(
    EXPORTING
      r_data = lr_excel_structure ).

  DATA(lo_config) = lo_tool_xls->configuration( ).
  LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<lfs_field>) .
    lo_config->add_column(
      EXPORTING
        header_text  = CONV string( <lfs_field>-fieldtext )
        field_name   = CONV string( <lfs_field>-fieldname )
        display_type = if_salv_bs_model_column=>uie_text_view ).

    APPEND VALUE #( name = CONV string( <lfs_field>-fieldtext ) ) TO  git_colum_excel.
  ENDLOOP .

ENDFORM.

***************************************************
* recupera dados anteriores ajustes
***************************************************
FORM f_dados_antigos_trace.

  SELECT *
    FROM zsdt0045
    INTO TABLE t_0045_old
     FOR ALL ENTRIES IN tg_0045
   WHERE zseq_inst   = tg_0045-zseq_inst
     AND objek       = tg_0045-objek
     AND objecttable = tg_0045-objecttable.

ENDFORM.

***************************************************
* envia instrucao ao trace
***************************************************
FORM f_envia_trace.

  LOOP AT tg_0045 INTO wg_0045.

    READ TABLE t_0045_old INTO DATA(w_0045_old) WITH KEY zseq_inst    = wg_0045-zseq_inst
                                                         objek        = wg_0045-objek
                                                         objecttable  = wg_0045-objecttable.

    IF sy-subrc = 0.
      IF wg_0045-safra          = w_0045_old-safra          AND
         wg_0045-peso_max       = w_0045_old-peso_max       AND
         wg_0045-data_in_porto  = w_0045_old-data_in_porto  AND
         wg_0045-data_porto     = w_0045_old-data_porto     AND
         wg_0045-instrucao      = w_0045_old-instrucao      AND
         wg_0045-terminal_estuf = w_0045_old-terminal_estuf AND
         wg_0045-observacao     = w_0045_old-observacao     AND
         wg_0045-werks          = w_0045_old-werks          AND
         wg_0045-charg          = w_0045_old-charg          AND
         wg_0045-quantidade     = w_0045_old-quantidade.
        CONTINUE.
      ENDIF.
    ENDIF.

    DATA(l_task) = 'TRACE_INSTRUCAO' && wg_0045-zseq_inst && wg_0045-objek && wg_0045-objecttable.

    CALL FUNCTION 'ZSD_ENVIO_INSTRUCAO_TRACE' STARTING NEW TASK l_task
      EXPORTING
        i_zseq_inst   = wg_0045-zseq_inst
        i_objek       = wg_0045-objek
        i_objecttable = wg_0045-objecttable
        i_acao        = 'C'
      EXCEPTIONS
        OTHERS        = 1.
  ENDLOOP.

ENDFORM.

***************************************************
* EXCLUI instrucao ao trace
***************************************************
FORM f_exclui_trace CHANGING p_error_exclusao.



  CLEAR: p_error_exclusao.

  LOOP AT it_dele INTO DATA(w_dele).

    "Projeto Reestruturação Algodao 2024
*    DATA(l_task) = 'TRACE_INSTRUCAO' && w_dele-zseq_inst && w_dele-objek && w_dele-objecttable.
*
*    CALL FUNCTION 'ZSD_ENVIO_INSTRUCAO_TRACE' STARTING NEW TASK l_task
*      EXPORTING
*        i_zseq_inst   = w_dele-zseq_inst
*        i_objek       = w_dele-objek
*        i_objecttable = w_dele-objecttable
*        i_acao        = 'E'
*      EXCEPTIONS
*        OTHERS        = 1.

    SELECT SINGLE *
      FROM zsdt0327 INTO @DATA(w_0327_aux)
     WHERE zseq_inst   EQ @w_dele-zseq_inst
       AND objek       EQ @w_dele-objek
       AND objecttable EQ @w_dele-objecttable
       AND metodo_http EQ 'POST'
       AND tipo_msg    EQ 'S'.

    IF sy-subrc EQ 0. "Já foi enviada a instrução para o Trace? Se sim, deve excluir antes...

      CALL FUNCTION 'ZSD_ENVIO_INSTRUCAO_TRACE'
        EXPORTING
          i_zseq_inst   = w_dele-zseq_inst
          i_objek       = w_dele-objek
          i_objecttable = w_dele-objecttable
          i_acao        = 'E'
        EXCEPTIONS
          OTHERS        = 1.

      "Verificar se foi realizado a exclusão com sucesso no trace...
      SELECT SINGLE *
        FROM zsdt0327 INTO w_0327_aux
       WHERE zseq_inst   EQ w_dele-zseq_inst
         AND objek       EQ w_dele-objek
         AND objecttable EQ w_dele-objecttable
         AND metodo_http EQ 'DELETE'
         AND tipo_msg    EQ 'S'.

      IF sy-subrc NE 0.
        p_error_exclusao = abap_true.
        MESSAGE |Não foi possível excluir a Instrução Seq: { w_dele-zseq_inst } do Trace Cotton! Verificar logs na coluna "Status Trace"! | TYPE 'I'.
        RETURN.
      ENDIF.
    ENDIF.
    "Projeto Reestruturação Algodao 2024 - Fim

  ENDLOOP.


ENDFORM.

***************************************************
* exibe log trace cotton
***************************************************
FORM f_exibe_log_trace USING p_zseq_inst
                             p_objek
                             p_objecttable.

  CALL FUNCTION 'ZSD_LOG_ENVIO_TRACE_COTTON'
    EXPORTING
      i_zseq_inst    = p_zseq_inst
      i_objek        = p_objek
      i_objecttable  = p_objecttable
      i_tipo_integra = 'IN'.

*  TYPES: BEGIN OF ty_det.
*           INCLUDE TYPE zsdt0327.
*           TYPES: status TYPE char4.
*  TYPES: END   OF ty_det.
*
*  DATA : ls_variant   TYPE disvariant,
*         t_0327       TYPE TABLE OF zsdt0327,
*         w_0327       TYPE zsdt0327,
*         t_det        TYPE TABLE OF ty_det,
*         t_text       TYPE TABLE OF string,
*         w_text       TYPE string,
*         w_det        TYPE ty_det,
*         l_grid_title TYPE lvc_title.
*
*  FREE: it_fieldcat, t_det.
*
*  SELECT *
*    FROM zsdt0327
*    INTO TABLE t_0327
*   WHERE zseq_inst   = p_zseq_inst
*     AND objek       = p_objek
*     AND objecttable = p_objecttable.
*
*  LOOP AT t_0327 INTO w_0327.
*    MOVE-CORRESPONDING w_0327  TO w_det.
*    w_det-status    = COND #( WHEN w_0327-tipo_msg = 'E' THEN icon_led_red
*                                                         ELSE icon_led_green ).
*    SPLIT w_det-mensagem AT '|' INTO TABLE t_text.
*
*    LOOP AT t_text INTO w_text.
*      CHECK w_text IS NOT INITIAL.
*      w_det-mensagem  = w_text.
*      APPEND w_det   TO t_det.
*    ENDLOOP.
*  ENDLOOP.
*
*  CHECK t_det[] IS NOT INITIAL.
*
*  PERFORM f_preenche_fcat USING :
*   '01' ''          ''            'T_DET'  'STATUS'                 'Status'                 '05'     ''    ''     ''    '' '' 'X',
*   '02' ''          ''            'T_DET'  'ZSEQ_INST'              'Sequencia'              '10'     ''    ''     ''    '' '' '',
*   '03' ''          ''            'T_DET'  'MENSAGEM'               'Mensagem'               '60'     ''    ''     ''    '' '' '',
*   '04' ''          ''            'T_DET'  'USNAME'                 'Usuário'                '15'     ''    ''     ''    '' '' '',
*   '05' ''          ''            'T_DET'  'DATA'                   'Data'                   '12'     ''    ''     ''    '' '' '',
*   '06' ''          ''            'T_DET'  'HORA'                   'Hora'                   '12'     ''    ''     ''    '' '' ''.
*
*  ls_variant-report = sy-repid && 'XXX'.
*  l_grid_title = 'Log de Integração Trace Cotton'.
*
*  SORT t_det  BY seq DESCENDING.
*
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_callback_program    = sy-repid
*      it_fieldcat           = it_fieldcat[]
**     it_sort               = t_sort[]
**     i_callback_user_command = 'USER_COMMAND_COMPRO'
*      i_grid_title          = l_grid_title
*      i_save                = 'X'
*      is_variant            = ls_variant
*      i_screen_start_column = 35
*      i_screen_start_line   = 08
*      i_screen_end_column   = 140
*      i_screen_end_line     = 18
*    TABLES
*      t_outtab              = t_det.

ENDFORM.

FORM f_preenche_fcat   USING  VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit)
                              VALUE(p_do_sum)
                              VALUE(p_just)
                              VALUE(p_hotspot)
                              VALUE(p_checkbox)
                              VALUE(p_icon).

  CLEAR: wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-do_sum        = p_do_sum.
  wa_estrutura-just          = p_just.
  wa_estrutura-icon          = p_icon.
  wa_estrutura-hotspot       = p_hotspot.
  wa_estrutura-checkbox      = p_checkbox.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO it_fieldcat.

ENDFORM.
