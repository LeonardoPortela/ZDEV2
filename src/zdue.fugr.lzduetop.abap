FUNCTION-POOL zdue MESSAGE-ID zdue.

TABLES: zsdt0170, zsdt0172, zsdt0173, zsdt0174, zsdt0305.

*&--------------------------------------------------------------------&*
*& Tipos                                                              &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_cab_due,
         ds_empresa        TYPE t001-butxt,
         ds_cliente        TYPE kna1-name1,
         ds_urf_despacho   TYPE zsdt0167-ds_urf,
         ds_ra_despacho    TYPE zsdt0168-ds_ra,
         ds_urf_embarque   TYPE zsdt0167-ds_urf,
         ds_ra_embarque    TYPE zsdt0168-ds_ra,
         ds_nome_transpor  TYPE znom_transporte-ds_nome_transpor,
         retificar         TYPE c,
         ds_status         TYPE dd07t-ddtext,
         ds_situacao_due   TYPE dd07t-ddtext,
         ds_ind_bloqueio   TYPE dd07t-ddtext,
         ds_situacao_carga TYPE dd07t-ddtext,
         ds_controle_adm   TYPE dd07t-ddtext,
         xmls_exportacao   TYPE znfe_xml_sefaz_auth_t.
         INCLUDE STRUCTURE zsdt0170.
       TYPES  END OF ty_cab_due.

TYPES: BEGIN OF ty_item_due,
         meins_ue TYPE mara-meins.
TYPES  END OF ty_item_due.

TYPES: BEGIN OF ty_branch,
         cnpj    TYPE bapibranch-cgc_number,
         name    TYPE bapibranch-name,
         country TYPE adrc-country,
         region  TYPE adrc-region,
         street  TYPE adrc-street,
         city2   TYPE adrc-city2,
         city1   TYPE adrc-city1.
TYPES  END OF ty_branch.

TYPES: BEGIN OF ty_due_default,
         fatura_tp_codigo          TYPE zsdt0172-fatura_tp_codigo,
         fatura_motivo_dispensa_nf TYPE zsdt0172-fatura_motivo_dispensa_nf,
         codigo_cond_venda         TYPE zsdt0172-codigo_cond_venda,
         codigo_enquadramento      TYPE zsdt0172-codigo_enquadramento.
TYPES  END OF ty_due_default.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida_0120.
         INCLUDE STRUCTURE zsdt0172.
       TYPES END OF ty_saida_0120.

TYPES: BEGIN OF ty_saida_0122,
         ck_modify TYPE c,
         estilo    TYPE lvc_t_styl.
         INCLUDE STRUCTURE zsdt0174.
       TYPES END OF ty_saida_0122.

TYPES: BEGIN OF ty_saida_0123,
         ck_modify        TYPE c,
         estilo           TYPE lvc_t_styl,
         emissor_cnpj_cpf TYPE c LENGTH 14,
         ncm_xml          TYPE zib_nfe_dist_itm-prod_ncm,
         utrib_xml        TYPE zib_nfe_dist_itm-prod_und_trib.
         INCLUDE STRUCTURE zsdt0173.
       TYPES END OF ty_saida_0123.

TYPES: BEGIN OF ty_saida_0125,
         ck_modify TYPE c,
         estilo    TYPE lvc_t_styl.
         INCLUDE STRUCTURE zsdt0190.
       TYPES END OF ty_saida_0125.

TYPES: BEGIN OF ty_saida_0201,
         id_due_ret TYPE zsdt0170-id_due.
         INCLUDE STRUCTURE zsdt0289.
       TYPES END OF ty_saida_0201.

TYPES: BEGIN OF ty_due_control,
         modo      TYPE c,
         retificar TYPE c.
TYPES: END OF ty_due_control.

TYPES: BEGIN OF ty_doc_exp,
         waerk_ft  TYPE vbrk-waerk,
         netwr     TYPE vbrk-netwr,
         vgbel     TYPE lips-vgbel,
         auart     TYPE vbak-auart,
         vbeln     TYPE likp-vbeln,
         menge     TYPE j_1bnflin-menge,
         meins     TYPE j_1bnflin-meins,
         nbm       TYPE j_1bnflin-nbm,
         matnr     TYPE mara-matnr,
         preco_ton TYPE konv-kbetr.
         INCLUDE STRUCTURE j_1bnfdoc.
       TYPES: END OF ty_doc_exp.

TYPES: BEGIN OF ty_znom_prog_reme,
         vgbel TYPE lips-vgbel.
         INCLUDE STRUCTURE znom_prog_reme.
       TYPES: END OF ty_znom_prog_reme.

TYPES: BEGIN OF ty_saida_1000,
         code       TYPE zsdt0170-msg_erro,
         tag        TYPE zsdt0170-msg_erro,
         message    TYPE zsdt0170-msg_erro,
         message_v1 TYPE zsdt0170-msg_erro,
         message_v2 TYPE zsdt0170-msg_erro,
       END OF ty_saida_1000.


TYPES: BEGIN OF ty_doc_fat_ref,
         docnum_exp        TYPE j_1bnfdoc-docnum,
         docnum            TYPE j_1bnfdoc-docnum,
         chave_nfe         TYPE zde_chave_doc_e,
         matnr             TYPE mara-matnr,
         menge             TYPE j_1bnflin-menge,
         meins             TYPE j_1bnflin-meins,
         ue_exportada	     TYPE zsdt0173-ue_exportada,
         qtde_ue_exportada TYPE zsdt0173-qtde_ue_exportada,
         peso_liq_total    TYPE zsdt0173-peso_liq_total,
         registro_cct      TYPE zsdt0173-registro_cct,
         nf_produtor       TYPE zsdt0173-nf_produtor,
         entrada_propria   TYPE zsdt0173-entrada_propria,
         complemento       TYPE zsdt0173-complemento,
         rfl_terceiro      TYPE zsdt0173-rfl_terceiro,
         emissor_cnpj      TYPE zsdt0173-emissor_cnpj,
         emissor_cpf       TYPE zsdt0173-emissor_cpf,
         emissor_ie        TYPE zsdt0173-emissor_ie.
TYPES: END OF ty_doc_fat_ref.

TYPES: BEGIN OF ty_nomeacao_sol_ov,
         id_transporte    TYPE znom_transporte-id_transporte,
         id_nomeacao_tran TYPE znom_transporte-id_nomeacao_tran,
         ds_nome_transpor TYPE znom_transporte-ds_nome_transpor,
         booking          TYPE znom_transporte-booking,
         id_conhec        TYPE znom_conhec-id_conhec,
         dt_data          TYPE znom_conhec-dt_data,
         nr_conhec        TYPE znom_conhec-nr_conhec,
         nro_sol_ov       TYPE zsdt0053-nro_sol_ov,
         numero_ruc       TYPE zsdt0053-numero_ruc,
         nr_qtde_nomeada  TYPE zsdt0053-zmeng,
         vkorg            TYPE zsdt0051-vkorg,
         werks            TYPE zsdt0053-werks,
         instrucao        TYPE zsdt0053-instrucao.
TYPES  END OF ty_nomeacao_sol_ov.

TYPES: BEGIN OF ty_saida_pre_acd_nf,
         qtde_ue_due   TYPE zsdt0173-qtde_ue_exportada,
         saldo_pos_ret TYPE zsdt0173-qtde_ue_exportada,
         cct_due       TYPE zsdt0173-registro_cct.
         INCLUDE STRUCTURE zlest0186.
       TYPES END OF ty_saida_pre_acd_nf.


TYPES: BEGIN OF ty_dados_geracao_ruc,
         maktx            TYPE makt-maktx,
         name1            TYPE kna1-name1,
         ds_ra            TYPE zsdt0168-ds_ra,
         ds_nome_transpor TYPE znom_transporte-ds_nome_transpor.
         INCLUDE TYPE zde_dados_geracao_ruc.
       TYPES: END OF ty_dados_geracao_ruc.


*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0120 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS lcl_alv_toolbar_0122 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

CLASS lcl_alv_toolbar_0123 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.


CLASS lcl_alv_toolbar_0125 DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event_handler_0120 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.


CLASS lcl_event_handler_0122 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm .

ENDCLASS.

CLASS lcl_event_handler_0123 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm .

ENDCLASS.

CLASS lcl_event_handler_0125 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm .

ENDCLASS.

DATA: obj_alv_0120       TYPE REF TO cl_gui_alv_grid,
      obj_container_0120 TYPE REF TO cl_gui_custom_container,
      obj_alv_0201       TYPE REF TO cl_gui_alv_grid,
      obj_container_0201 TYPE REF TO cl_gui_custom_container,
      obj_alv_0122       TYPE REF TO cl_gui_alv_grid,
      obj_container_0122 TYPE REF TO cl_gui_custom_container,
      obj_alv_0123       TYPE REF TO cl_gui_alv_grid,
      obj_container_0123 TYPE REF TO cl_gui_custom_container,
      obj_alv_0125       TYPE REF TO cl_gui_alv_grid,
      obj_container_0125 TYPE REF TO cl_gui_custom_container,
      obj_alv_1000       TYPE REF TO cl_gui_alv_grid,
      obj_container_1000 TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: obj_toolbar_0120 TYPE REF TO lcl_alv_toolbar_0120,
      obj_toolbar_0122 TYPE REF TO lcl_alv_toolbar_0122,
      obj_toolbar_0123 TYPE REF TO lcl_alv_toolbar_0123,
      obj_toolbar_0125 TYPE REF TO lcl_alv_toolbar_0125.
"OBJ_TOOLBAR_0201 TYPE REF TO LCL_ALV_TOOLBAR_0201.

* ALV field catalogs
DATA: it_fcat TYPE lvc_t_fcat,
      wa_fcat TYPE lvc_s_fcat.

* ALV excluded functions
DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* Alv Styles
DATA: ls_edit TYPE lvc_s_styl,
      lt_edit TYPE lvc_t_styl.

* ALV layout variant
DATA: gs_variant       TYPE disvariant.

* ALV layout
DATA: gs_layout        TYPE lvc_s_layo.

* ALV Stable
DATA: wa_stable        TYPE lvc_s_stbl.

DATA: it_selectedcell TYPE lvc_t_cell,
      wa_selectedcell TYPE lvc_s_cell.

DATA: it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_estilo TYPE lvc_t_styl WITH HEADER LINE,
      wl_estilo TYPE lvc_s_styl.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: wa_estrutura TYPE ty_estrutura,
      estrutura    TYPE TABLE OF ty_estrutura.

DATA: vg_field_sel_0110 TYPE c LENGTH 100,
      vg_field_pos_0110 TYPE i,
      vg_field_sel_0121 TYPE c LENGTH 100,
      vg_field_pos_0121 TYPE i.

DATA vg_ucomm TYPE sy-ucomm.


*&--------------------------------------------------------------------&*
*& Variaveis de Comando                                               &*
*&--------------------------------------------------------------------&*

DATA: ok_code_0100 TYPE syucomm.

*&--------------------------------------------------------------------&*
*& Variaveis Dynpro                                                   &*
*&--------------------------------------------------------------------&*

DATA: due_dynnr_000 LIKE sy-dynnr.

*&--------------------------------------------------------------------&*
*& Controles                                                          &*
*&--------------------------------------------------------------------&*

CONTROLS: info_due_tab  TYPE TABSTRIP.

*&--------------------------------------------------------------------&*
*& Constantes
*&--------------------------------------------------------------------&*

CONSTANTS: c_due_novo          TYPE c VALUE '1'                 LENGTH 1,
           c_due_change        TYPE c VALUE '2'                 LENGTH 1,
           c_due_view          TYPE c VALUE '3'                 LENGTH 1,
           c_novo              TYPE c VALUE 'NOVO'              LENGTH 4,
           c_del               TYPE c VALUE 'DEL'               LENGTH 4,
           c_save              TYPE c VALUE 'SAVE'              LENGTH 4,
           c_cancel            TYPE c VALUE 'CANCEL'            LENGTH 6,
           c_ciencia_alerta    TYPE c VALUE 'CIENCIA_ALERTA'    LENGTH 50,
           c_change            TYPE c VALUE 'CHANGE'            LENGTH 6,
           c_view              TYPE c VALUE 'VIEW'              LENGTH 6,
           c_pais_dst          TYPE c VALUE 'PAIS_DST'          LENGTH 8,
           c_lib_leitura_opus  TYPE c VALUE 'LIB_LEITURA_OPUS'  LENGTH 50,
           c_sol_modific_opus  TYPE c VALUE 'SOL_MODIFIC_OPUS'  LENGTH 50,
           c_event_interface   TYPE c VALUE 'EVENT_INTERFACE'   LENGTH 50,
           c_inf_dt_reg_portal TYPE c VALUE 'INF_DT_REG_PORTAL' LENGTH 50,
           c_retransmitir      TYPE c VALUE 'RETRANSMITIR'      LENGTH 50,
           c_fat_ref_due       TYPE c VALUE 'FAT_REF_DUE'       LENGTH 50,
           c_fat_cons_pre_acd  TYPE c VALUE 'FAT_CONS_PRE_ACD'  LENGTH 50,
           c_lpco_item         TYPE c VALUE 'LPCO_ITEM'         LENGTH 50,
           c_drawnback         TYPE c VALUE 'DRAWNBACK'         LENGTH 50,
           c_fill_fat_ref      TYPE c VALUE 'FILL_FAT_REF'      LENGTH 50,
           c_fill_due          TYPE c VALUE 'FILL_DUE'          LENGTH 50,
           c_down_xml          TYPE c VALUE 'DOWN_XML'          LENGTH 50,
           c_fat_ref           TYPE c VALUE 'FAT_REF'           LENGTH 7.

"Tabs
CONSTANTS: due_tb01 TYPE c LENGTH 8 VALUE 'DUE_TB01',
           due_tb02 TYPE c LENGTH 8 VALUE 'DUE_TB02'.

"Screens
CONSTANTS: due_0110 LIKE sy-dynnr VALUE '0110',
           due_0120 LIKE sy-dynnr VALUE '0120'.


"Variaveis para Radio Button
DATA: rb_consultar_0201 TYPE c,
      rb_gerar_ruc_0201 TYPE c.

"Variaveis para CheckBox
DATA: ck_list_all_0201  TYPE c.



*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: vg_operacao_0120   TYPE c LENGTH 20,
      vg_docum_exp_venda TYPE j_1bnfdoc-docnum,
      vg_dt_reg_portal   TYPE zsdt0170-dt_registro_portal,
      var_answer         TYPE c.

*&--------------------------------------------------------------------&*
*& Internal Table e Work Area
*&--------------------------------------------------------------------&*

DATA: BEGIN OF tg_0172 OCCURS 0.
        INCLUDE TYPE zsdt0172.
      DATA: END OF tg_0172.

DATA: BEGIN OF tg_0173 OCCURS 0.
        INCLUDE TYPE zsdt0173.
      DATA: END OF tg_0173.

DATA: BEGIN OF tg_zib_nfe_dist_itm OCCURS 0.
        INCLUDE TYPE zib_nfe_dist_itm.
      DATA: END OF tg_zib_nfe_dist_itm.

DATA: BEGIN OF tg_0174 OCCURS 0.
        INCLUDE TYPE zsdt0174.
      DATA: END OF tg_0174.

DATA: BEGIN OF tg_0190 OCCURS 0.
        INCLUDE TYPE zsdt0190.
      DATA: END OF tg_0190.

DATA: BEGIN OF tg_arquivos_xml OCCURS 0,
        name_file TYPE string,
        xml       TYPE string,
      END OF tg_arquivos_xml.


DATA: it_saida_0120       TYPE TABLE OF ty_saida_0120,
      wa_saida_0120       TYPE ty_saida_0120,

      it_saida_0122       TYPE TABLE OF ty_saida_0122,
      wa_saida_0122       TYPE ty_saida_0122,
      it_saida_0122_itm   TYPE TABLE OF ty_saida_0122,
      wa_saida_0122_itm   TYPE ty_saida_0122,

      it_saida_0123       TYPE TABLE OF ty_saida_0123,
      wa_saida_0123       TYPE ty_saida_0123,
      it_saida_0123_itm   TYPE TABLE OF ty_saida_0123,
      wa_saida_0123_itm   TYPE ty_saida_0123,


      it_saida_0125       TYPE TABLE OF ty_saida_0125,
      wa_saida_0125       TYPE ty_saida_0125,
      it_saida_0125_itm   TYPE TABLE OF ty_saida_0125,
      wa_saida_0125_itm   TYPE ty_saida_0125,

      it_saida_0201       TYPE TABLE OF ty_saida_0201,
      wa_saida_0201       TYPE ty_saida_0201,

      it_saida_pre_acd_nf TYPE TABLE OF ty_saida_pre_acd_nf,
      wa_saida_pre_acd_nf TYPE ty_saida_pre_acd_nf.

DATA: it_saida_1000 TYPE TABLE OF ty_saida_1000,
      wa_saida_1000 TYPE ty_saida_1000.

DATA: cab_due     TYPE ty_cab_due,
      item_due    TYPE ty_item_due,
      due_control TYPE ty_due_control,
      due_default TYPE ty_due_default.

DATA:  ls_zsdt0305 TYPE zsdt0305,
  t_zsdt0305  TYPE TABLE OF zsdt0305.


DATA: gwa_dados_geracao_ruc TYPE ty_dados_geracao_ruc.

DATA: gva_numero_ruc_gerado_sel TYPE zde_numero_ruc.

DATA: tg_znom_reme_notas  TYPE TABLE OF znom_reme_notas    WITH HEADER LINE,
      tg_zdoc_nf_produtor TYPE TABLE OF zdoc_nf_produtor   WITH HEADER LINE,
      tg_znom_prog_reme   TYPE TABLE OF ty_znom_prog_reme  WITH HEADER LINE,
      tg_zsdt_export      TYPE TABLE OF zsdt_export        WITH HEADER LINE,
      tg_zsdt_retlote     TYPE TABLE OF zsdt_retlote       WITH HEADER LINE,
      tg_zsdt_retlote_ter TYPE TABLE OF zsdt_retlote_ter   WITH HEADER LINE,
      tg_zdoc_exp         TYPE TABLE OF zdoc_exp           WITH HEADER LINE,
      tg_vbfa             TYPE TABLE OF vbfa               WITH HEADER LINE,
      tg_j_1bnfdoc_exp    TYPE TABLE OF ty_doc_exp         WITH HEADER LINE,
      tg_doc_fat_ref      TYPE TABLE OF ty_doc_fat_ref     WITH HEADER LINE,
      tg_parametros       TYPE ustyp_t_parameters          WITH HEADER LINE.


*-----------------------------------------------------------------------------------------------*
* Cadastro de Nomeação por Solicitação de O.V
*-----------------------------------------------------------------------------------------------*
DATA: gwa_nomeacao_sol_ov TYPE ty_nomeacao_sol_ov,
      git_zsdt0053        TYPE TABLE OF zsdt0053.

"INCLUDE LZDUED...                          " Local class definition
