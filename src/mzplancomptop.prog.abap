*&---------------------------------------------------------------------*
*& Include MZPLANCOMPTOP                                     PoolMóds.        SAPMZPLANCOMP
*&
*&---------------------------------------------------------------------*

PROGRAM  sapmzplancomp MESSAGE-ID zplan_nomeacao.

*----------------------------------------------------------------------*
* Tipe-Pools
*----------------------------------------------------------------------*
TYPE-POOLS: zplac, icon, zmemo.

***********************************************************************
* LOCAL CLASSES
***********************************************************************

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id es_row_no,

      on_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      on_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  e_fieldvalue
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.


ENDCLASS.                    "lcl_event_handler DEFINITION

CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      handle_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_ucomm.

ENDCLASS.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_due_retificacao DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_hotspot
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id
                es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Definition -----------------------------------------------*
CLASS lcl_event_programa DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_hotspot_programa
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id
                es_row_no,

      on_double_programa
        FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row
                  e_column
                  es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_notas DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click_notas
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id
                es_row_no.

ENDCLASS.                    "lcl_event_handler DEFINITION


*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_notas2 DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_hotspot_click_notas2
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id
                  es_row_no,

      on_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row
                  e_column
                  es_row_no.

ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_prog_reme DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click_prog_reme FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS on_double_prog_reme FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler_produtor DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click_produtor FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_disp DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING  e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_vinc DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING  e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_prog_reme DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING  e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar_conhec_vinc DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_conhec_vinc DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      free,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING  e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar_conhec_vinc DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar_conhec_vinc DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_conhec_a_vinc DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      free,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING  e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar_conhec_vinc DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_remessa_re DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING  e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar_conf_exp DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING  e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
TABLES: znom_transporte,
        znom_programacao,
        znom_remetente.

TYPES: BEGIN OF ty_znom_remetente.
         INCLUDE STRUCTURE znom_remetente.
TYPES:   lifnr   TYPE lfa1-lifnr,
         novo(1),
       END OF ty_znom_remetente.

TYPES: BEGIN OF ty_zplac_nom_programacao.
         INCLUDE TYPE zplac_nom_programacao.
TYPES: chk(1),
       END OF ty_zplac_nom_programacao.

TYPES: BEGIN OF ty_zplac_due_antecipada,
         chk(1),
         ue_exportada         TYPE zsdt0172-ue_exportada,
         qtde_ue_exportada    TYPE zsdt0172-qtde_ue_exportada,
         peso_liq_total       TYPE zsdt0172-peso_liq_total,
         codigo_cond_venda    TYPE zsdt0172-codigo_cond_venda, "CS2019001113
         matnr                TYPE zsdt0172-matnr,
         maktx                TYPE makt-maktx,
         codigo_ncm           TYPE zsdt0172-codigo_ncm,
         preco_ton            TYPE zsdt0172-preco_ton,
         codigo_enquadramento TYPE zsdt0172-codigo_enquadramento,
         pais_dest            TYPE c LENGTH 20,

         bloq_desbloq         TYPE c LENGTH 4.
         INCLUDE TYPE zplac_due_antecipada.
TYPES: END OF ty_zplac_due_antecipada.

TYPES: BEGIN OF ty_zplac_due_retificacao,
         chk(1),
         ue_exportada      TYPE zsdt0172-ue_exportada,
         qtde_ue_exportada TYPE zsdt0172-qtde_ue_exportada,
         id_due_ret        TYPE zsdt0170-id_due,
         peso_liq_tot_ret  TYPE zsdt0172-peso_liq_total,
         peso_liq_total    TYPE zsdt0172-peso_liq_total,
         matnr             TYPE zsdt0172-matnr,
         nf_exp            TYPE zde_nf_exp_due-menge.
         INCLUDE TYPE zplac_due_antecipada.
TYPES: END OF ty_zplac_due_retificacao.

TYPES: BEGIN OF ty_zplac_due_retificacao_conf.
         INCLUDE TYPE zplac_due_retificacao_conf.
TYPES: END OF ty_zplac_due_retificacao_conf.

TYPES: BEGIN OF ty_zplac_nom_prog_reme.
         INCLUDE TYPE zplac_nom_prog_reme.
TYPES:   vbelv TYPE vbfa-vbelv,
       END OF ty_zplac_nom_prog_reme.

* Type para dados da aba de planjamento, contagem de caracteres.
TYPES: BEGIN OF ty_qtd_carac,
         remet TYPE i,
         ret   TYPE i,
         nr_nf TYPE i,
       END OF ty_qtd_carac.

TYPES: BEGIN OF ty_znom_reme_notas.
         INCLUDE STRUCTURE znom_reme_notas.
TYPES:   docnum_rt TYPE znom_remetente-docnum_rt,
         nr_ordem  TYPE znom_remetente-nr_ordem,
         tipov     TYPE auart,          "113637 - CS2023000378 - PSA
         preco     TYPE dmbtr,          "113637 - CS2023000378 - PSA
         depst     TYPE lgort_d,        "113637 - CS2023000378 - PSA
         safra     TYPE charg_d,        "113637 - CS2023000378 - PSA
         cvirt     TYPE werks_ext,      "113637 - CS2023000378 - PSA
       END OF ty_znom_reme_notas.

TYPES: BEGIN OF ty_grp_retorno,
         grp_retorno LIKE znom_reme_notas-grp_retorno,
         total       LIKE znom_transporte-nr_qtde_nomeada,
         novos       LIKE znom_transporte-nr_qtde_nomeada,
         saldo       LIKE znom_transporte-nr_qtde_nomeada,
         quantidade  LIKE znom_transporte-nr_qtde_nomeada,
         docnum_rt   TYPE znom_remetente-docnum_rt,
         nr_ordem    TYPE znom_remetente-nr_ordem,
       END OF ty_grp_retorno.

TYPES: BEGIN OF ty_regio_due.
         INCLUDE STRUCTURE t005s.
TYPES: END OF ty_regio_due.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.


TYPES: BEGIN OF ty_due_nf_exp,
         branch TYPE j_1bnfdoc-branch,
         docnum TYPE j_1bnfdoc-docnum,
         nfenum TYPE j_1bnfdoc-nfenum,
         menge  TYPE j_1bnflin-menge,
         meins  TYPE j_1bnflin-meins,
         netwr  TYPE j_1bnflin-netwr.
TYPES: END OF ty_due_nf_exp.

TYPES: BEGIN OF ty_zsdt0001,
         branch        TYPE zsdt0001-branch,
         nr_safra      TYPE zsdt0001-nr_safra,
         id_referencia TYPE zsdt0001-id_referencia.
TYPES: END OF ty_zsdt0001.

TYPES: BEGIN OF ty_baixas_nf,
         docnum TYPE j_1bnfdoc-docnum,
         itmnum TYPE j_1bnflin-docnum,
         menge  TYPE zsdt0276-menge.
TYPES: END   OF ty_baixas_nf.

TYPES: BEGIN OF ty_msg,
         msg TYPE string,
       END OF ty_msg.

TYPES:
  BEGIN OF ty_alv_vinc_lotes,
    docnum      TYPE j_1bdocnum,
    itmnum      TYPE j_1bitmnum,
    docnum_rfl  TYPE j_1bdocnum,
    item_rfl    TYPE j_1bitmnum,
    filial_rfl  TYPE werks_d,
    qtd_rfl     TYPE zqtd_vinc,
    saldo_rfl   TYPE zqtd_vinc,
    data_rfl    TYPE datum,
    cfop_rfl    TYPE j_1bcfop,
    eudr        TYPE zeudr, "// WBARBOSA 28102024 US-153330
    dt_recepcao TYPE erdat,  "<<<------"165835 - NMS------>>>
  END OF ty_alv_vinc_lotes.

*----------------------------------------------------------------------*
* Objetos
*----------------------------------------------------------------------*
DATA : obj_zcl_util_sd TYPE REF TO zcl_util_sd,
       zcl_due         TYPE REF TO zcl_due.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
DATA: c_x               TYPE c LENGTH 01 VALUE 'X',
      c_a               TYPE c LENGTH 01 VALUE 'A',
      c_j               TYPE c LENGTH 01 VALUE 'J',
      c_due_novo        TYPE c LENGTH 1  VALUE '1',
      c_due_change      TYPE c LENGTH 1  VALUE '2',
      c_due_view        TYPE c LENGTH 1  VALUE '3',
      c_rfl             TYPE c LENGTH 06 VALUE 'RFL',
      c_pro             TYPE c LENGTH 06 VALUE 'PRO',
      c_status          TYPE c LENGTH 06 VALUE 'STATUS',
      c_planeja         TYPE c LENGTH 07 VALUE 'PLANEJA',
      c_remessa         TYPE c LENGTH 10 VALUE 'ID_REMESSA',
      c_fatura          TYPE c LENGTH 05 VALUE 'VBELN',
      c_docnum          TYPE c LENGTH 06 VALUE 'DOCNUM',
      c_docnum_rfl      TYPE c LENGTH 10 VALUE 'DOCNUM_RFL',
      c_docnump         TYPE c LENGTH 11 VALUE 'DOCNUM_PROD',
      c_grid_color_c200 TYPE c LENGTH 04 VALUE 'C200',
      c_grid_color_c300 TYPE c LENGTH 04 VALUE 'C300',
      c_grid_color_c400 TYPE c LENGTH 04 VALUE 'C400',
      c_grid_color_c500 TYPE c LENGTH 04 VALUE 'C500',
      c_grid_color_c600 TYPE c LENGTH 04 VALUE 'C600',
      c_grid_color_c700 TYPE c LENGTH 04 VALUE 'C700',
      c_grid_color_recu TYPE c LENGTH 04 VALUE 'C100'.

*----------------------------------------------------------------------*
* Constantes Telas
*----------------------------------------------------------------------*
DATA: vg_dynnr_xxxx TYPE sydynnr,
      vg_dynnr_30xx TYPE sydynnr,
      vg_dynnr_36xx TYPE sydynnr,
      vg_dynnr_0002 TYPE sydynnr VALUE '0002',
      vg_dynnr_0003 TYPE sydynnr VALUE '0003',
      vg_dynnr_0010 TYPE sydynnr VALUE '0010',
      vg_dynnr_0011 TYPE sydynnr VALUE '0011',
      vg_dynnr_0012 TYPE sydynnr VALUE '0012',
      vg_dynnr_0020 TYPE sydynnr VALUE '0020',
      vg_dynnr_0030 TYPE sydynnr VALUE '0030',
      vg_dynnr_0031 TYPE sydynnr VALUE '0031',
      vg_dynnr_0032 TYPE sydynnr VALUE '0032',
      vg_dynnr_0033 TYPE sydynnr VALUE '0033',
      vg_dynnr_0034 TYPE sydynnr VALUE '0034',
      vg_dynnr_0035 TYPE sydynnr VALUE '0035',
      vg_dynnr_0036 TYPE sydynnr VALUE '0036',
      vg_dynnr_0037 TYPE sydynnr VALUE '0037',
      vg_dynnr_0038 TYPE sydynnr VALUE '0038',
      vg_dynnr_0040 TYPE sydynnr VALUE '0040',
      vg_dynnr_0041 TYPE sydynnr VALUE '0041',
      vg_dynnr_0043 TYPE sydynnr VALUE '0043',
      vg_dynnr_0044 TYPE sydynnr VALUE '0044',
      vg_dynnr_0045 TYPE sydynnr VALUE '0045',
      vg_dynnr_0060 TYPE sydynnr VALUE '0060',
      vg_dynnr_0070 TYPE sydynnr VALUE '0070',
      vg_dynnr_0073 TYPE sydynnr VALUE '0073'. "// wbarbosa 28102024 US-153330

*----------------------------------------------------------------------*
* Constantes UCOMN
*----------------------------------------------------------------------*

DATA: ok_pesq           TYPE c LENGTH 04 VALUE 'PESQ',
      ok_back           TYPE c LENGTH 04 VALUE 'BACK',
      ok_exit           TYPE c LENGTH 04 VALUE 'EXIT',
      ok_cancel         TYPE c LENGTH 06 VALUE 'CANCEL',
      ok_tab01          TYPE c LENGTH 05 VALUE 'TAB01',
      ok_tab02          TYPE c LENGTH 05 VALUE 'TAB02',
      ok_tab03          TYPE c LENGTH 05 VALUE 'TAB03',
      ok_tab04          TYPE c LENGTH 05 VALUE 'TAB04',
      ok_tab05          TYPE c LENGTH 05 VALUE 'TAB05',
      ok_tab06          TYPE c LENGTH 05 VALUE 'TAB06',
      ok_tab07          TYPE c LENGTH 05 VALUE 'TAB07', "// wbarbosa 28102024 US-153330
      ok_tab31          TYPE c LENGTH 05 VALUE 'TAB31',
      ok_tab32          TYPE c LENGTH 05 VALUE 'TAB32',
      ok_tab33          TYPE c LENGTH 05 VALUE 'TAB33',
      ok_add_prog       TYPE c LENGTH 08 VALUE 'ADD_PROG',
      ok_edi_prog       TYPE c LENGTH 08 VALUE 'EDI_PROG',
      ok_exc_prog       TYPE c LENGTH 08 VALUE 'EXC_PROG',
      ok_ger_mail       TYPE c LENGTH 08 VALUE 'GER_MAIL',
      ok_ger_cont       TYPE c LENGTH 08 VALUE 'GER_CONT',
      ok_ger_mail_e     TYPE c LENGTH 10 VALUE 'GER_MAIL_E',
      ok_salvar         TYPE c LENGTH 06 VALUE 'SALVAR',
      ok_enter          TYPE c LENGTH 05 VALUE 'ENTER',
      ok_criar_ret      TYPE c LENGTH 10 VALUE 'CRIAR_RET',
      ok_lib_ov_rec     TYPE c LENGTH 10 VALUE 'LIB_OV_REC',
      ok_nota_fis       TYPE c LENGTH 09 VALUE 'NOTA_FIS',
      ok_criar_ord      TYPE c LENGTH 10 VALUE 'CRIAR_ORD',
      ok_cancelar       TYPE c LENGTH 08 VALUE 'CANCELAR',
      ok_btpsqn         TYPE c LENGTH 06 VALUE 'BTPSQN',
      ok_vinc           TYPE c LENGTH 04 VALUE 'VINC',
      ok_excl           TYPE c LENGTH 04 VALUE 'EXCL',
      ok_desv           TYPE c LENGTH 04 VALUE 'DESV',
      ok_comp           TYPE c LENGTH 04 VALUE 'COMP',
      ok_domp           TYPE c LENGTH 04 VALUE 'DOMP',
      ok_dlog           TYPE c LENGTH 04 VALUE 'DLOG',
      ok_dcon           TYPE c LENGTH 04 VALUE 'DCON',
      ok_dund           TYPE c LENGTH 04 VALUE 'DUND',
      ok_vlog           TYPE c LENGTH 04 VALUE 'VLOG',
      ok_bt_reme        TYPE c LENGTH 07 VALUE 'BT_REME',
      ok_bt_blre        TYPE c LENGTH 07 VALUE 'BT_BLRE',
      ok_bt_blfe        TYPE c LENGTH 07 VALUE 'BT_BLFE',
      ok_bt_refr        TYPE c LENGTH 07 VALUE 'FT_REFR',
      ok_ft_nota        TYPE c LENGTH 07 VALUE 'FT_NOTA',
      ok_ft_recusa      TYPE c LENGTH 09 VALUE 'FT_RECUSA',
      ok_ft_memora      TYPE c LENGTH 09 VALUE 'FT_MEMORA',
      ok_ft_fatura      TYPE c LENGTH 09 VALUE 'FT_FATURA',
      ok_btn_expa       TYPE c LENGTH 08 VALUE 'BTN_EXPA',
      ok_btn_mini       TYPE c LENGTH 08 VALUE 'BTN_MINI',
      ok_bt_rem2        TYPE c LENGTH 08 VALUE 'BT_REM2',
      ok_add_due        TYPE c LENGTH 50 VALUE 'ADD_DUE',
      ok_edit_mat       TYPE c LENGTH 50 VALUE 'EDIT_MAT',
      ok_moni_due       TYPE c LENGTH 50 VALUE 'MONI_DUE',
      ok_del_due        TYPE c LENGTH 50 VALUE 'DEL_DUE',
      ok_lanc_ret_due   TYPE c LENGTH 50 VALUE 'LC_RET_DUE',
      ok_edit_due       TYPE c LENGTH 50 VALUE 'EDIT_DUE',
      ok_view_due       TYPE c LENGTH 50 VALUE 'VIEW_DUE',
      ok_bloq_desbl_due TYPE c LENGTH 50 VALUE 'BLOQ_DESBL',
      ok_troca_due      TYPE c LENGTH 50 VALUE 'TROCA_DUE',
      ok_edit_regio     TYPE c LENGTH 50 VALUE 'EDIT_REGIO',
      ok_edit_tp_ex     TYPE c LENGTH 50 VALUE 'EDIT_TP_EX',
      ok_edit_eudr      TYPE c LENGTH 09 VALUE 'EDIT_EUDR',
      ok_inf_qtd_exp    TYPE c LENGTH 04 VALUE 'INF_QTD_EXP',
      ok_val_qtd_exp    TYPE c LENGTH 04 VALUE 'VAL_QTD_EXP',
      ok_ret_due        TYPE c LENGTH 50 VALUE 'RET_DUE'.


*----------------------------------------------------------------------*
* Controles de Tela
*----------------------------------------------------------------------*
CONTROLS: tabpag  TYPE TABSTRIP,
          tab01   TYPE TABLEVIEW USING SCREEN 0002,
          tab02   TYPE TABLEVIEW USING SCREEN 0002,
          tab03   TYPE TABLEVIEW USING SCREEN 0002,
          tab04   TYPE TABLEVIEW USING SCREEN 0002,
          tab05   TYPE TABLEVIEW USING SCREEN 0002,
          tabreme TYPE TABSTRIP,
          tab31   TYPE TABLEVIEW USING SCREEN 0031,
          tab32   TYPE TABLEVIEW USING SCREEN 0031.

*----------------------------------------------------------------------*
* Variáveis de Comando
*----------------------------------------------------------------------*
DATA: ok_code_0001 TYPE syucomm,
      ok_code_0021 TYPE syucomm,
      ok_code_0042 TYPE syucomm,
      ok_code_0050 TYPE syucomm.

CONSTANTS : sy_tab03 TYPE syucomm VALUE 'TAB03'.


CONSTANTS: c_f TYPE c VALUE 'F',
           c_c TYPE c VALUE 'C',
           c_s TYPE c VALUE 'S'.

*----------------------------------------------------------------------*
* Variáveis de Seleção de Consultas
*----------------------------------------------------------------------*
DATA: it_znom_transporte       TYPE TABLE OF znom_transporte WITH HEADER LINE,
      wa_znom_transporte       TYPE znom_transporte,
      it_znom_transporte_alv   TYPE TABLE OF zplac_nom_transporte WITH HEADER LINE,
      wa_znom_transporte_alv   TYPE zplac_nom_transporte,
      it_due_antecipada        TYPE TABLE OF zsdt0170 WITH HEADER LINE,
      wa_due_antecipada        TYPE zsdt0170,
      it_znom_programacao      TYPE TABLE OF znom_programacao WITH HEADER LINE,
      wa_znom_programacao      TYPE znom_programacao,
      wa_znom_programacao_f    TYPE znom_programacao,
      it_znom_programacao_alv  TYPE TABLE OF ty_zplac_nom_programacao WITH HEADER LINE,
      wa_znom_programacao_alv  TYPE ty_zplac_nom_programacao,
      it_due_antecipada_alv    TYPE TABLE OF ty_zplac_due_antecipada WITH HEADER LINE,
      wa_due_antecipada_alv    TYPE ty_zplac_due_antecipada,
      it_due_retificacao       TYPE TABLE OF zsdt0170 WITH HEADER LINE,
      wa_due_retificacao       TYPE zsdt0170,
      it_due_retificacao_alv   TYPE TABLE OF ty_zplac_due_retificacao WITH HEADER LINE,
      wa_due_retificacao_alv   TYPE ty_zplac_due_retificacao,
      it_due_ret_conf_alv      TYPE TABLE OF ty_zplac_due_retificacao_conf WITH HEADER LINE,
      wa_due_ret_conf_alv      TYPE ty_zplac_due_retificacao_conf,
      it_due_nf_exp_alv        TYPE TABLE OF ty_due_nf_exp WITH HEADER LINE,
      wa_due_nf_exp_alv        TYPE ty_due_nf_exp,
      it_znom_remetente        TYPE TABLE OF ty_znom_remetente WITH HEADER LINE,
      it_znom_remetente_log    TYPE TABLE OF ty_znom_remetente WITH HEADER LINE,
      wa_znom_remetente        TYPE ty_znom_remetente,
      wa_znom_remetente_log    TYPE ty_znom_remetente,

      it_znom_remetente_alv    TYPE TABLE OF zplac_nom_remetente WITH HEADER LINE,
      wa_znom_remetente_alv    LIKE LINE OF it_znom_remetente_alv,
      wa_filtro_remetente      TYPE zplac_filtro_reme,
      wa_filtro_reme_qtde      TYPE i,
      wa_filtro_reme_qtde_sel  TYPE i,

      it_znom_reme_notas       TYPE TABLE OF ty_znom_reme_notas WITH HEADER LINE,
      it_znom_reme_notase      TYPE TABLE OF znom_reme_notase WITH HEADER LINE,
      wa_znom_reme_notas       TYPE ty_znom_reme_notas, "ZNOM_REME_NOTAS,
      it_znom_reme_notas_alv   TYPE TABLE OF zplac_nom_reme_notas WITH HEADER LINE,
      it_znom_reme_dnotas_alv  TYPE TABLE OF zplac_nom_reme_notlog WITH HEADER LINE,
      it_znom_reme_notas_sel   TYPE TABLE OF zplac_nom_reme_notas WITH HEADER LINE,
      wa_znom_reme_notas_alv   TYPE zplac_nom_reme_notas,
      wa_znom_reme_dnotas_alv  TYPE zplac_nom_reme_notlog,

      it_msg_val               TYPE STANDARD TABLE OF ty_msg INITIAL SIZE 0,
      ls_msg_val               TYPE ty_msg,

      it_znom_notasfiscais_alv TYPE TABLE OF zplac_notasfiscais WITH HEADER LINE,
      it_znom_notasfiscais_sel TYPE TABLE OF zplac_notasfiscais WITH HEADER LINE,
      wa_znom_notasfiscais_alv TYPE zplac_notasfiscais,
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
      it_nf_doc                TYPE TABLE OF zplac_notasfiscais,
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração


      it_alv_vinc_lotes        TYPE TABLE OF ty_alv_vinc_lotes,
*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Início de Alteração
      it_vinc_lotes_ord        TYPE TABLE OF ty_alv_vinc_lotes,
*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Fim de Alteração

      it_znom_prog_reme        TYPE TABLE OF znom_prog_reme WITH HEADER LINE,
      wa_znom_prog_reme        TYPE znom_prog_reme,
      it_znom_prog_reme_alv    TYPE TABLE OF ty_zplac_nom_prog_reme WITH HEADER LINE,
      wa_znom_prog_reme_alv    TYPE ty_zplac_nom_prog_reme,

      it_zdoc_exp              TYPE TABLE OF zdoc_exp WITH HEADER LINE,
      wa_zdoc_exp              TYPE zdoc_exp,

      it_zdoc_nf_produtor      TYPE TABLE OF zdoc_nf_produtor WITH HEADER LINE,
      wa_zdoc_nf_produtor      TYPE zdoc_nf_produtor,
      it_zdoc_nf_produtor_alv  TYPE TABLE OF zplac_zdoc_nf_produtor WITH HEADER LINE,
      wa_zdoc_nf_produtor_alv  TYPE zplac_zdoc_nf_produtor,

      it_zdoc_rem_bl           TYPE TABLE OF zdoc_rem_bl WITH HEADER LINE,
      wa_zdoc_rem_bl           TYPE zdoc_rem_bl,
      it_zdoc_rem_bl_alv       TYPE TABLE OF zplac_zdoc_rem_bl WITH HEADER LINE,
      wa_zdoc_rem_bl_alv       TYPE zplac_zdoc_rem_bl,

      it_znom_conhec           TYPE TABLE OF znom_conhec WITH HEADER LINE,
      wa_znom_conhec           TYPE znom_conhec,
      it_znom_conhec_alv       TYPE TABLE OF zplac_znom_conhec WITH HEADER LINE,
      wa_znom_conhec_alv       TYPE zplac_znom_conhec,

      wa_vinc_remessa          TYPE zplac_vinc_remessa,
      it_prod_remessa          TYPE TABLE OF zplac_vinc_produtor WITH HEADER LINE,
      tx_ds_estado             TYPE bezei20,

      it_dta                   TYPE STANDARD TABLE OF bdcdata,
      wa_dta                   LIKE LINE OF           it_dta,

      it_grp_retorno           TYPE TABLE OF ty_grp_retorno WITH HEADER LINE,
      wa_grp_retorno           TYPE ty_grp_retorno,

      wg_regio_due             TYPE ty_regio_due,
      wg_tp_exp_due            TYPE zsdt0170-tp_exportacao,
*"// wbarbosa 25102024 - US-153330
      wg_reclassificacao_eudr  TYPE zsdt0170-eudr,
      wg_email                 TYPE zemail,
*"// wbarbosa 25102024 - US-153330
      wg_peso_liq_exp          TYPE zsdt0172-peso_liq_total,
      wg_id_due_troca          TYPE zsdt0170-id_due,

      it_qtd_carac             TYPE TABLE OF ty_qtd_carac WITH HEADER LINE,
      wa_qtd_carac             TYPE ty_qtd_carac,

      it_zsdt0001              TYPE TABLE OF ty_zsdt0001 WITH HEADER LINE,
      wa_zsdt0001              TYPE ty_zsdt0001,

*-CS2021000343 - 28.04.2021 - JT - inicio
      it_zsdt0276              TYPE TABLE OF zsdt0276,
      wa_zsdt0276              TYPE zsdt0276,
      it_baixas_nf             TYPE TABLE OF ty_baixas_nf,
      ws_baixas_nf             TYPE ty_baixas_nf,
*-CS2021000343 - 28.04.2021 - JT - fim

      wa_znom_remetente_dados  TYPE znom_remetente.

" Visualização LOG notas Desvinculadas
DATA: tg_notas          TYPE TABLE OF znom_reme_notlog,
      tg_j_1bnfe_active TYPE TABLE OF j_1bnfe_active,
      tg_reme           TYPE TABLE OF znom_reme_log,
      tg_prod           TYPE TABLE OF zdoc_nf_prod_log.

DATA: vg_txt_empresa    LIKE t001-butxt,
      vg_txt_filial     LIKE j_1bbranch-name,
      vg_txt_material   LIKE makt-maktx,
      vg_txt_cliente    LIKE kna1-name1,
      vg_total_alt      LIKE znom_programacao-nr_programada,
      vg_total_nomeado  LIKE znom_transporte-nr_qtde_nomeada,
      vg_saldo_nomeado  LIKE znom_transporte-nr_qtde_nomeada,
      vg_qtd_vinc_conh  LIKE znom_transporte-nr_qtde_nomeada,
      vg_qtde_saldo_rem TYPE znom_programacao-nr_programada,
      vg_tabix_sel      TYPE sy-tabix,
      vg_troca_notas(1),
      fg_propria(1).

DATA: r_docnum TYPE RANGE OF docnum.

*----------------------------------------------------------------------*
* Variáveis Telas
*----------------------------------------------------------------------*

DATA: plan_prim_0010 TYPE c LENGTH 1,
      vg_desbloq_nom TYPE c LENGTH 1,
      t_usermd       TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
      container_0010 TYPE REF TO cl_gui_custom_container,
      picture_0010   TYPE REF TO cl_gui_picture.

DATA: plan_scroll_col         TYPE lvc_s_col,
      plan_scroll_row         TYPE lvc_s_roid,
      plan_prim_nomeacao      TYPE c LENGTH 1,
      plan_container_nomeacao TYPE REF TO cl_gui_custom_container,
      plan_alv_nomeacao       TYPE REF TO cl_gui_alv_grid,
      plan_catalog            TYPE lvc_s_fcat,
      plan_catalogo_nomeacao  TYPE lvc_t_fcat,
      plan_gs_layout          TYPE lvc_s_layo,
      plan_event_handler      TYPE REF TO lcl_event_handler.

DATA: plan_catalogo_programa      TYPE lvc_t_fcat,
      plan_catalogo_due_antecip   TYPE lvc_t_fcat,
      plan_catalogo_due_retific   TYPE lvc_t_fcat,
      plan_catalogo_due_ret_conf  TYPE lvc_t_fcat,
      plan_prim_programa          TYPE c LENGTH 1,
      plan_container_programa     TYPE REF TO cl_gui_custom_container,
      plan_container_due_antecip  TYPE REF TO cl_gui_custom_container,
      plan_container_due_retific  TYPE REF TO cl_gui_custom_container,
      plan_container_due_ret_conf TYPE REF TO cl_gui_custom_container,
      plan_alv_programa           TYPE REF TO cl_gui_alv_grid,
      plan_alv_due_antecipada     TYPE REF TO cl_gui_alv_grid,
      plan_alv_due_retificacao    TYPE REF TO cl_gui_alv_grid,
      plan_alv_due_ret_conf       TYPE REF TO cl_gui_alv_grid,
      toolbar_due_conf_exp        TYPE REF TO lcl_alv_toolbar_conf_exp,
      plan_event_due_ret          TYPE REF TO lcl_event_due_retificacao,
      plan_event_programa         TYPE REF TO lcl_event_programa,
      gr_event_handler            TYPE REF TO lcl_event_handler.

DATA: plan_catalogo_remetente  TYPE lvc_t_fcat,
      plan_prim_remetente      TYPE c LENGTH 1,
      plan_container_remetente TYPE REF TO cl_gui_custom_container,
      plan_alv_remetente       TYPE REF TO cl_gui_alv_grid.

DATA: plan_catalogo_reme_notas   TYPE lvc_t_fcat,
      plan_catalogo_reme_notlog  TYPE lvc_t_fcat,
      plan_prim_reme_notas       TYPE c LENGTH 1,
      plan_container_reme_notas  TYPE REF TO cl_gui_custom_container,
      plan_container_reme_notlog TYPE REF TO cl_gui_custom_container,
      plan_alv_reme_notas        TYPE REF TO cl_gui_alv_grid,
      plan_alv_reme_notlog       TYPE REF TO cl_gui_alv_grid,
      plan_event_handler_notas2  TYPE REF TO lcl_event_handler_notas2,
      plan_alv_remet             TYPE REF TO lcl_event_handler_notas2,
      toolbar_reme_notas_alv     TYPE REF TO lcl_alv_toolbar_vinc,
      c_alv_toolbarmanager_vinc  TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: plan_catalogo_notasfiscais  TYPE lvc_t_fcat,
      plan_prim_notasfiscais      TYPE c LENGTH 1,
      plan_alv_vinc_lotes         TYPE c LENGTH 1,
      plan_container_notasfiscais TYPE REF TO cl_gui_custom_container,
      go_cc_vinc_lotes            TYPE REF TO cl_gui_custom_container,
      plan_alv_notasfiscais       TYPE REF TO cl_gui_alv_grid,
      go_alv_vinc_lotes           TYPE REF TO cl_gui_alv_grid,
      plan_event_handler_notas    TYPE REF TO lcl_event_handler_notas,
      toolbar_notasfiscais_alv    TYPE REF TO lcl_alv_toolbar_disp,
      c_alv_toolbarmanager        TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: plan_catalogo_prog_reme      TYPE lvc_t_fcat,
      plan_prim_prog_reme          TYPE c LENGTH 1,
      plan_container_prog_reme     TYPE REF TO cl_gui_custom_container,
      plan_alv_prog_reme           TYPE REF TO cl_gui_alv_grid,
      plan_event_handler_prog_reme TYPE REF TO lcl_event_handler_prog_reme,
      toolbar_prog_reme_alv        TYPE REF TO lcl_alv_toolbar_prog_reme,
      c_alv_toolbar_prog_reme      TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: plan_catalogo_nf_produtor   TYPE lvc_t_fcat,
      plan_prim_nf_produtor       TYPE c LENGTH 1,
      plan_container_nf_produtor  TYPE REF TO cl_gui_custom_container,
      plan_alv_nf_produtor        TYPE REF TO cl_gui_alv_grid,
      plan_event_handler_produtor TYPE REF TO lcl_event_handler_produtor.

DATA: plan_catalogo_conhec_vinc  TYPE lvc_t_fcat,
      plan_prim_conhec_vinc      TYPE c LENGTH 1,
      plan_container_conhec_vinc TYPE REF TO cl_gui_custom_container,
      plan_alv_conhec_vinc       TYPE REF TO cl_gui_alv_grid,
      toolbar_conhec_vinc_alv    TYPE REF TO lcl_alv_toolbar_conhec_vinc,
      c_alv_toolbar_conhec_vinc  TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: plan_catalogo_conhec_a_vinc  TYPE lvc_t_fcat,
      plan_prim_conhec_a_vinc      TYPE c LENGTH 1,
      plan_container_conhec_a_vinc TYPE REF TO cl_gui_custom_container,
      plan_alv_conhec_a_vinc       TYPE REF TO cl_gui_alv_grid,
      toolbar_conhec_a_vinc_alv    TYPE REF TO lcl_alv_toolbar_conhec_a_vinc,
      c_alv_toolbar_conhec_a_vinc  TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: plan_catalogo_remetente_re  TYPE lvc_t_fcat,
      plan_prim_remetente_re      TYPE c LENGTH 1,
      plan_container_remetente_re TYPE REF TO cl_gui_custom_container,
      plan_alv_remetente_re       TYPE REF TO cl_gui_alv_grid,
      toolbar_prog_remessa_re     TYPE REF TO lcl_alv_toolbar_remessa_re,
      c_alv_toolbar_remessa_re    TYPE REF TO cl_alv_grid_toolbar_manager.
"plan_event_remessa_re        type ref to lcl_event_handler_remessa_re.

DATA: it_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wa_msg TYPE bdcmsgcoll,

      BEGIN OF it_msgtext OCCURS 0,
        texto TYPE t100-text,
      END OF it_msgtext,

      BEGIN OF tl_remet OCCURS 0,
        nr_ordem TYPE znom_remetente-nr_ordem,
      END OF tl_remet.

DATA: it_sort TYPE lvc_t_sort,
      wa_sort LIKE LINE OF it_sort.



DATA: wa_estrutura TYPE ty_estrutura,
      estrutura    TYPE TABLE OF ty_estrutura.

DATA: vl_vbeln               TYPE bapivbeln-vbeln,
      vg_verifica_selecao_pr TYPE sy-subrc,
      tl_programacao         TYPE TABLE OF ty_zplac_nom_programacao WITH HEADER LINE,
      wl_programacao         TYPE znom_programacao,
      wa_header              TYPE bapisdhd1,
      wa_item                TYPE bapisditm,
      wa_schedules           TYPE bapischdl,
      wa_partner             TYPE bapiparnr,
      wa_conditions          TYPE bapicond,
      t_return               TYPE TABLE OF bapiret2,
      t_item                 TYPE TABLE OF bapisditm,
      t_partner              TYPE TABLE OF bapiparnr,
      t_schedules            TYPE TABLE OF bapischdl,
      t_conditions           TYPE TABLE OF bapicond,
      t_bapiparex            TYPE TABLE OF bapiparex.

DATA: vl_erro           TYPE i,
      vl_sum_programada TYPE j_1bnetqty.

DATA: wl_header_in      TYPE bapisdhd1,
      wl_header_inx     TYPE bapisdhd1x,
      doc_venda         LIKE bapivbeln-vbeln,
      tl_items_in       TYPE TABLE OF bapisditm  WITH HEADER LINE,
      tl_items_inx      TYPE TABLE OF bapisditmx WITH HEADER LINE,
      tl_return         TYPE TABLE OF bapiret2   WITH HEADER LINE,
      tl_partners       TYPE TABLE OF bapiparnr  WITH HEADER LINE,
      tl_conditions_in  TYPE TABLE OF bapicond   WITH HEADER LINE,
      tl_conditions_inx TYPE TABLE OF bapicondx  WITH HEADER LINE,
      f_headinx         LIKE bapisdh1x,
      tl_return2        TYPE TABLE OF bapiret2   WITH HEADER LINE,
      tl_return3        TYPE TABLE OF bapiret2   WITH HEADER LINE,
      tl_text_in        TYPE TABLE OF bapisdtext WITH HEADER LINE,
      tl_bapiparex      TYPE TABLE OF bapiparex  WITH HEADER LINE,
      wl_bape_vbak      TYPE bape_vbak,
      wl_bape_vbakx     TYPE bape_vbakx,
      tl_contrato       TYPE TABLE OF bapictr    WITH HEADER LINE,
      lv_msg            TYPE c LENGTH 255,
      lv_ukurs          TYPE ukurs_curr,
      lv_ukurs_         TYPE char9,
      lv_data_atual     TYPE gdatu_inv,
      wl_param          TYPE znom_param_cont.

*----------------------------------------------------------------------*
* Selections Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 0011 AS SUBSCREEN NESTING LEVEL 4.
  SELECT-OPTIONS: "T_IDNOME  FOR  ZNOM_TRANSPORTE-ID_NOMEACAO_TRAN NO-EXTENSION NO INTERVALS,
                  t_emp     FOR  znom_transporte-bukrs            NO-EXTENSION NO INTERVALS,
                  t_nr_ano  FOR  znom_transporte-nr_ano           NO-EXTENSION NO INTERVALS,
                  t_nr_mes  FOR  znom_transporte-nr_mes,
                  t_trans   FOR  znom_transporte-ds_nome_transpor NO-EXTENSION NO INTERVALS MATCHCODE OBJECT zmemoaj001, " Navio
                  t_porto   FOR  znom_transporte-ds_porto         NO-EXTENSION NO INTERVALS MATCHCODE OBJECT zmemoaj002. " Porto
  PARAMETERS:     t_cancs AS CHECKBOX.
SELECTION-SCREEN END OF SCREEN 0011.

*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
FORM z_estrutura_fieldcat TABLES it_catalogo TYPE lvc_t_fcat
                           USING p_tab_name
                                 p_fieldname
                                 p_texto_grande
                                 p_hot
                                 p_posicao
                                 p_outputlen
                                 p_fix_column
                                 p_convexit
                                 p_do_sum
                                 p_icon
                                 p_just
                                 p_emphasize
                                 p_edit.

  CLEAR plan_catalog.
  plan_catalog-tabname     = p_tab_name.
  plan_catalog-fieldname   = p_fieldname.
  plan_catalog-scrtext_l   = p_texto_grande.
  plan_catalog-scrtext_m   = p_texto_grande.
  plan_catalog-scrtext_s   = p_texto_grande.
  plan_catalog-hotspot     = p_hot.
  plan_catalog-col_pos     = p_posicao.
  plan_catalog-outputlen   = p_outputlen.
  plan_catalog-fix_column  = p_fix_column.
  plan_catalog-convexit    = p_convexit.
  plan_catalog-do_sum      = p_do_sum.
  plan_catalog-icon        = p_icon.
  plan_catalog-just        = p_just.
  plan_catalog-emphasize   = p_emphasize.
  plan_catalog-edit        = p_edit.

  IF p_tab_name EQ 'IT_ZNOM_NOTASFISCAIS_ALV'.

    IF p_fieldname EQ 'ICONE' OR
*       p_fieldname EQ 'QTDE_NF' OR <<<------"145379 - NMS------>>>
       p_fieldname EQ 'PESO_CCT' OR
       p_fieldname EQ 'NR_QUANTIDADE2' OR
       p_fieldname EQ 'NR_UTILIZADA' OR
*       p_fieldname EQ 'NR_SALDO' OR
       p_fieldname EQ 'NR_UTILIZADA_CCT' OR
       p_fieldname EQ 'DIF_PESO_CCT_NF' OR
       p_fieldname EQ 'CHARG' OR
       p_fieldname EQ 'ROM_COMPLETO' OR
       p_fieldname EQ 'DOCNUM_RFL' OR
       p_fieldname EQ 'CCT_RFL' OR
       p_fieldname EQ 'IND_RFL' OR
       p_fieldname EQ 'PESO_AFERIDO_CCT'.
      plan_catalog-no_out = abap_true.
    ENDIF.

  ENDIF.

  CASE p_tab_name.
    WHEN 'IT_ZNOM_PROGRAMACAO_ALV'.
      IF p_fieldname = 'CHK'.
        plan_catalog-checkbox = c_x.
      ENDIF.
    WHEN 'IT_ZNOM_REME_NOTAS_ALV'.
      IF p_fieldname = 'TP_NF_REM'.
        plan_catalog-ref_table  = 'ZNOM_REME_NOTAS'.
        plan_catalog-ref_field  = 'TP_NF_REM'.
      ENDIF.
    WHEN 'IT_DUE_RET_CONF_ALV'.
      IF p_fieldname = 'MATNR'.
        plan_catalog-ref_table  = 'MARA'.
        plan_catalog-ref_field  = 'MATNR'.
      ENDIF.
    WHEN 'IT_DUE_ANTECIPADA_ALV'.

      IF p_fieldname = 'SOLIC_MODIFICACAO_OPUS' OR
         p_fieldname = 'LIB_LEITURA_OPUS'       OR
         p_fieldname = 'LEITURA_OPUS'.
        plan_catalog-checkbox = c_x.
      ENDIF.

      IF p_fieldname = 'MATNR'.
        plan_catalog-ref_table  = 'MARA'.
        plan_catalog-ref_field  = 'MATNR'.
        plan_catalog-f4availabl = 'X'.
      ENDIF.

  ENDCASE.

  APPEND plan_catalog TO it_catalogo.
ENDFORM.                    " Z_ESTRUTURA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  handle_hotspot_click
*&---------------------------------------------------------------------*
FORM handle_hotspot_click
       USING VALUE(row_id)    LIKE lvc_s_roid-row_id
             VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_znom_transporte  INDEX row_id INTO wa_znom_transporte.

  CASE fieldname.
    WHEN c_status.
      PERFORM troca_aba_05 USING c_x.
      LEAVE TO SCREEN 0001.
  ENDCASE.

ENDFORM.                    " handle_hotspot_click

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_NOTAS
*&---------------------------------------------------------------------*
FORM handle_hotspot_click_notas
       USING VALUE(row_id)    LIKE lvc_s_roid-row_id
             VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_znom_notasfiscais_alv INDEX row_id.

  CASE fieldname.
    WHEN c_docnum.
      PERFORM nf_writer USING it_znom_notasfiscais_alv-docnum.
    WHEN c_docnum_rfl.
      PERFORM nf_writer USING it_znom_notasfiscais_alv-docnum_rfl.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK_NOTAS

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK_NOTAS
*&---------------------------------------------------------------------*
FORM handle_hotspot_click_notas2
       USING VALUE(row_id)    LIKE lvc_s_roid-row_id
             VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_znom_reme_notas_alv INDEX row_id.

  CASE fieldname.
    WHEN c_docnum.
      PERFORM nf_writer USING it_znom_reme_notas_alv-docnum.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK_NOTAS

*&---------------------------------------------------------------------*
*&      Form  handle_hotspot_click
*&---------------------------------------------------------------------*
FORM handle_hotspot_click_prog_reme
       USING VALUE(row_id)    LIKE lvc_s_roid-row_id
             VALUE(fieldname) LIKE lvc_s_col-fieldname.

  DATA: vg_valid TYPE char01.

  READ TABLE it_znom_prog_reme_alv INDEX row_id INTO wa_znom_prog_reme_alv.

  CASE fieldname.
    WHEN c_remessa.
      PERFORM authority_check USING 'VL03N' vg_valid.
      IF vg_valid IS INITIAL.
        SET PARAMETER ID 'VL' FIELD wa_znom_prog_reme_alv-id_remessa.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN c_fatura.
      CHECK NOT wa_znom_prog_reme_alv-vbeln IS INITIAL.
      PERFORM authority_check USING 'VF03' vg_valid.
      IF vg_valid IS INITIAL.
        SET PARAMETER ID 'VF' FIELD wa_znom_prog_reme_alv-vbeln.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN c_docnum.
      CHECK NOT wa_znom_prog_reme_alv-docnum IS INITIAL.
      PERFORM nf_writer USING wa_znom_prog_reme_alv-docnum.
  ENDCASE.

ENDFORM.                    " handle_hotspot_click


*&---------------------------------------------------------------------*
*&      Form  nf_writer
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_NFE_ALV_DOCNUM  text
*----------------------------------------------------------------------*
FORM nf_writer  USING p_docnum TYPE j_1bdocnum.

  CHECK p_docnum NE '9999999999'.
  CHECK NOT p_docnum IS INITIAL.

  DATA: gf_nfobjn LIKE j_1binterf-nfobjn.

  CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
    EXPORTING
      doc_number         = p_docnum
    IMPORTING
      obj_number         = gf_nfobjn
    EXCEPTIONS
      document_not_found = 1
      docum_lock         = 2
      OTHERS             = 3.

  CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
    EXPORTING
      obj_number         = gf_nfobjn
    EXCEPTIONS
      object_not_found   = 1
      scr_ctrl_not_found = 2
      OTHERS             = 3.

ENDFORM.                    " nf_writer

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_notas IMPLEMENTATION.
  METHOD handle_hotspot_click_notas.
    PERFORM handle_hotspot_click_notas
       USING es_row_no-row_id
             e_column_id-fieldname.


  ENDMETHOD.                    "handle_hotspot_click

*  METHOD ON_CLICK.
*
*    DATA: LT_ROWS TYPE LVC_T_ROW.
*    DATA: L_ROW   TYPE LVC_S_ROW.
*    DATA: LINES   TYPE I.
*
*    CALL METHOD PLAN_ALV_NOTASFISCAIS->GET_SELECTED_ROWS
*      IMPORTING
*        ET_INDEX_ROWS = LT_ROWS.
*
*    CALL METHOD CL_GUI_CFW=>FLUSH.
*    READ TABLE LT_ROWS INDEX 1 TRANSPORTING NO FIELDS.
*    IF SY-SUBRC NE 0.
*      EXIT.
*    ENDIF.
*    DESCRIBE TABLE LT_ROWS LINES LINES.
*    MOVE LINES TO WA_FILTRO_REME_QTDE.
*
*  ENDMETHOD.                    "handle_delayed_changed_sel_cb
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_notas2 IMPLEMENTATION.
  METHOD handle_hotspot_click_notas2.
    PERFORM handle_hotspot_click_notas2
       USING es_row_no-row_id
             e_column_id-fieldname.

  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK_NOTAS2

  METHOD on_double_click.
    TYPES: BEGIN OF ty_cont_ret,
             nfenum TYPE zsdt_retlote-nfenum,
           END OF ty_cont_ret,

           BEGIN OF ty_cont_vin,
             name1 TYPE lfa1-name1,
             stcd1 TYPE lfa1-stcd1,
             lifnr TYPE lfa1-lifnr,
           END OF ty_cont_vin,

           BEGIN OF ty_cont_nf_vin,
             nfenum TYPE j_1bnfdoc-nfenum,
             nfnum  TYPE j_1bnfdoc-nfnum,
           END OF ty_cont_nf_vin.

    DATA: tl_cont_ret    TYPE TABLE OF ty_cont_ret,
          wl_cont_ret    TYPE ty_cont_ret,

          tl_cont_vin    TYPE TABLE OF ty_cont_vin,
          wl_cont_vin    TYPE ty_cont_vin,

          tl_cont_nf_vin TYPE TABLE OF ty_cont_nf_vin,
          wl_cont_nf_vin TYPE ty_cont_nf_vin.

    DATA: tl_remet     TYPE TABLE OF zplac_nom_remetente,
          wl_remet     LIKE LINE OF tl_remet,
          lv_cont      TYPE i,
          lv_docnum_rt TYPE znom_remetente-docnum_rt,
          lv_valor     TYPE p DECIMALS 2,
          lv_contt     TYPE i.

**  CONTADORES
    IF it_znom_remetente_alv[] IS NOT INITIAL.

***  Contador Retorno
      CLEAR: wa_qtd_carac-ret, lv_cont, lv_contt, wl_remet, tl_remet[].
      REFRESH: tl_cont_ret.

      READ TABLE it_znom_remetente_alv INTO wl_remet INDEX es_row_no-row_id.
      IF wl_remet-docnum_rt IS NOT INITIAL.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_remet-docnum_rt
          IMPORTING
            output = lv_docnum_rt.

        SELECT nfenum
          FROM zsdt_retlote
          APPENDING TABLE tl_cont_ret
          WHERE docnum_ret = lv_docnum_rt.

        IF tl_cont_ret[] IS NOT INITIAL.
          LOOP AT tl_cont_ret INTO wl_cont_ret.
            SHIFT wl_cont_ret-nfenum LEFT DELETING LEADING '0'.
            lv_contt = strlen( wl_cont_ret-nfenum ).
            ADD lv_contt TO lv_cont.
            ADD 1 TO lv_cont.
          ENDLOOP.

          MOVE lv_cont TO wa_qtd_carac-ret.
        ENDIF.

*** Contador NF vinculadas
        CLEAR: lv_cont, lv_contt, lv_valor, lv_valor,
               wa_qtd_carac-remet, wa_qtd_carac-nr_nf,
               tl_cont_vin[].

        IF wl_remet-id_remetente IS NOT INITIAL.
          lv_docnum_rt = wl_remet-docnum_rt.
          tl_remet[] = it_znom_remetente_alv[].

          LOOP AT tl_remet INTO wl_remet WHERE docnum_rt = lv_docnum_rt.
            SELECT *
              FROM lfa1
              APPENDING CORRESPONDING FIELDS OF TABLE tl_cont_vin
              WHERE lifnr = wl_remet-id_remetente.
          ENDLOOP.

          LOOP AT tl_cont_vin INTO wl_cont_vin.
            lv_contt = strlen( wl_cont_vin-stcd1 ).
            ADD lv_contt TO lv_cont.
            lv_contt = strlen( wl_cont_vin-name1 ).
            ADD lv_contt TO lv_cont.
            ADD 54 TO lv_cont.
          ENDLOOP.

          SELECT nf~nfenum nf~nfnum
            INTO CORRESPONDING FIELDS OF TABLE tl_cont_nf_vin
            FROM znom_reme_notas AS rn
            INNER JOIN j_1bnfdoc AS nf ON nf~docnum = rn~docnum
            FOR ALL ENTRIES IN tl_cont_vin
            WHERE rn~id_nomeacao_tran = wl_remet-id_nomeacao_tran
             AND  rn~id_empresa       = wl_remet-id_empresa
             AND  rn~id_filial        = wl_remet-id_filial
             AND  rn~id_remetente     = tl_cont_vin-lifnr
             AND  rn~grp_retorno      = wl_remet-grp_retorno.

          LOOP AT tl_cont_nf_vin INTO wl_cont_nf_vin.
            SHIFT wl_cont_nf_vin-nfenum LEFT DELETING LEADING '0'.
            lv_contt = strlen( wl_cont_nf_vin-nfenum ).
            ADD lv_contt TO lv_cont.
            SHIFT wl_cont_nf_vin-nfnum LEFT DELETING LEADING '0'.
            lv_contt = strlen( wl_cont_nf_vin-nfnum ).
            ADD lv_contt TO lv_cont.
            ADD 1 TO lv_cont.
          ENDLOOP.
        ENDIF.

        MOVE lv_cont TO wa_qtd_carac-remet.

*** Totalizador de NF
        lv_valor = ( wa_qtd_carac-ret + wa_qtd_carac-remet ) / 4200.
        lv_cont = ceil( lv_valor ).
        MOVE lv_cont TO wa_qtd_carac-nr_nf.

        LEAVE TO SCREEN 0001.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "on_double_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_prog_reme IMPLEMENTATION.
  METHOD handle_hotspot_click_prog_reme.
    PERFORM handle_hotspot_click_prog_reme
       USING es_row_no-row_id
             e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD on_double_prog_reme.
    PERFORM troca_aba_04a.
    CALL METHOD plan_alv_nf_produtor->refresh_table_display.
    LEAVE TO SCREEN 0001.
  ENDMETHOD.                    "on_double_prog_reme
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler_produtor IMPLEMENTATION.
  METHOD handle_hotspot_click_produtor.

    DATA: vg_valid TYPE char01.

    READ TABLE it_zdoc_nf_produtor_alv INDEX es_row_no-row_id INTO wa_zdoc_nf_produtor_alv.

    CASE e_column_id-fieldname.
      WHEN c_docnump.
        CHECK NOT wa_zdoc_nf_produtor_alv-docnum_prod IS INITIAL.
        PERFORM nf_writer USING wa_zdoc_nf_produtor_alv-docnum_prod.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click
       USING es_row_no-row_id
             e_column_id-fieldname.

  ENDMETHOD.                    "handle_hotspot_click
  METHOD on_double_click.
    IF e_row-index IS NOT INITIAL.
      READ TABLE it_znom_transporte INDEX e_row-index INTO wa_znom_transporte.
      PERFORM troca_aba_05 USING c_x.
      LEAVE TO SCREEN 0001.
    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK
  METHOD on_f4.
    PERFORM on_f4 USING e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display.
  ENDMETHOD.
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed.
    DATA : modi  TYPE TABLE OF lvc_s_modi,
           modis TYPE lvc_s_modi.
    DATA:  zcl_due         TYPE REF TO zcl_due.
    DATA   vmatnr18        TYPE matnr18.

    DATA: lv_verifica_selecao_da TYPE sy-subrc,
          lv_answer              TYPE c.

    DATA: lv_matnr TYPE matnr.

    modi = er_data_changed->mt_mod_cells .
    IF modi[] IS NOT INITIAL.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Deseja realmente alterar o material ?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF it_due_antecipada[] IS NOT INITIAL AND lv_answer EQ '1'.
        LOOP AT modi INTO modis.
          IF modis-fieldname = 'MATNR'.
            READ TABLE  it_due_antecipada_alv INTO wa_due_antecipada_alv INDEX modis-row_id.
            IF sy-subrc = 0 AND modis-value IS NOT INITIAL.

              MOVE modis-value TO lv_matnr.

              CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                EXPORTING
                  input        = lv_matnr
                IMPORTING
                  output       = vmatnr18
                EXCEPTIONS
                  length_error = 1
                  OTHERS       = 2.

              lv_matnr = vmatnr18.

              SELECT SINGLE maktx FROM makt
              INTO wa_due_antecipada_alv-maktx
              WHERE matnr = lv_matnr
              AND spras = sy-langu.

              SELECT SINGLE * INTO @DATA(ls_marc)
                FROM marc
                WHERE steuc = @wa_due_antecipada_alv-codigo_ncm
                  AND matnr = @lv_matnr.
              IF sy-subrc NE 0.
                MESSAGE 'Material não pertece ao NCM informado.' TYPE 'S' DISPLAY LIKE 'E'.
                CONTINUE.
              ENDIF.
              wa_due_antecipada_alv-matnr = modis-value.
              MODIFY it_due_antecipada_alv FROM wa_due_antecipada_alv INDEX modis-tabix.

              UPDATE zsdt0172 SET matnr = lv_matnr
                    WHERE id_due = wa_due_antecipada_alv-id_due.
              COMMIT WORK AND WAIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
        DATA : t_filedcat  TYPE lvc_t_fcat,
               wa_filedcat TYPE lvc_s_fcat.

        CALL METHOD plan_alv_due_antecipada->get_frontend_fieldcatalog
          IMPORTING
            et_fieldcatalog = t_filedcat[].

        LOOP AT t_filedcat INTO wa_filedcat .
          IF wa_filedcat-fieldname = 'MATNR' .
            wa_filedcat-edit = '' .
            MODIFY t_filedcat FROM wa_filedcat .
          ENDIF .
        ENDLOOP .

        CALL METHOD plan_alv_due_antecipada->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_filedcat[].

        PERFORM consulta_due_antecipada.
        CALL METHOD plan_alv_due_antecipada->refresh_table_display.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.


*---------- Implementation -------------------------------------------*
CLASS lcl_event_programa IMPLEMENTATION.
  METHOD handle_hotspot_programa.
    CASE e_column_id.
      WHEN 'CONTRATO'.
        READ TABLE it_znom_programacao INTO wa_znom_programacao INDEX es_row_no-row_id.
        IF sy-subrc = 0.
          IF wa_znom_programacao-contrato IS NOT INITIAL.
            CLEAR: it_dta[].
            PERFORM f_bdc_data USING:
                  ''          ''      'T' 'VA43'          '',
                  'SAPMV45A'  '0102'  'X' ''              '',
                  ''          ''      ''  'BDC_CURSOR'    'VBAK-VBELN',
                  ''          ''      ''  'VBAK-VBELN'    wa_znom_programacao-contrato,
                  ''          ''      ''  'BDC_OKCODE'    '=SUCH'.
            CALL TRANSACTION 'VA43' USING it_dta
              MODE 'E'
              UPDATE 'S'.
          ENDIF.
        ENDIF.
      WHEN 'CHK'.
        READ TABLE it_znom_programacao_alv INTO wa_znom_programacao_alv INDEX es_row_no-row_id.
        IF wa_znom_programacao_alv-chk IS INITIAL.
          MOVE c_x TO wa_znom_programacao_alv-chk.
        ELSE.
          CLEAR wa_znom_programacao_alv-chk.
        ENDIF.

        MODIFY it_znom_programacao_alv INDEX es_row_no-row_id FROM wa_znom_programacao_alv TRANSPORTING chk.

        CALL METHOD plan_alv_programa->refresh_table_display.
      WHEN OTHERS.
        PERFORM troca_aba_03 USING c_x.
        LEAVE TO SCREEN 0001.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click
  METHOD on_double_programa.
    CLEAR vg_troca_notas.
    IF it_znom_reme_dnotas_alv[] IS INITIAL.
      PERFORM troca_aba_03 USING c_x.
    ELSE.
      "manter a mesma programação
      MOVE-CORRESPONDING wa_znom_programacao TO wa_znom_programacao_f.
      PERFORM troca_aba_03 USING 'S'.
    ENDIF.
    LEAVE TO SCREEN 0001.
  ENDMETHOD.                    "ON_DOUBLE_CLICK
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*---------- Implementation -------------------------------------------*
CLASS lcl_event_due_retificacao IMPLEMENTATION.

  METHOD handle_hotspot.
    CASE e_column_id.
      WHEN 'NF_EXP'.
        READ TABLE it_due_retificacao_alv INTO wa_due_retificacao_alv INDEX es_row_no-row_id.
        IF sy-subrc = 0.

          DATA: t_nf_exp_due TYPE zde_nf_exp_due_t.

          CLEAR: it_due_nf_exp_alv[], t_nf_exp_due[].

          CALL FUNCTION 'ZDUE_DADOS_RETIFICAR'
            EXPORTING
              i_id_due     = wa_due_retificacao_alv-id_due
            TABLES
              t_nf_exp_due = t_nf_exp_due.

          LOOP AT t_nf_exp_due INTO DATA(_wl_nf_due_exp).
            CLEAR: wa_due_nf_exp_alv.
            MOVE-CORRESPONDING _wl_nf_due_exp TO wa_due_nf_exp_alv.
            APPEND wa_due_nf_exp_alv TO it_due_nf_exp_alv.
          ENDLOOP.

          IF it_due_nf_exp_alv[] IS INITIAL.
            MESSAGE 'Nenhuma Nota Exportação encontrada!' TYPE 'I'.
            EXIT.
          ENDIF.

          PERFORM f_montar_layout_nf_exp_due.

          CHECK estrutura[] IS NOT INITIAL.

          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              it_fieldcat           = estrutura[]
              i_save                = 'A'
              i_screen_start_column = 3
              i_screen_start_line   = 3
              i_screen_end_column   = 100
              i_screen_end_line     = 13
            TABLES
              t_outtab              = it_due_nf_exp_alv.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_disp IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA : vg_toolbar TYPE stb_button.

    vg_toolbar-icon      =  icon_assign.
    vg_toolbar-function  =  ok_vinc.
    vg_toolbar-text      = 'Vinc.'.
    APPEND vg_toolbar TO e_object->mt_toolbar.

*    VG_TOOLBAR-ICON      =  ''.
*    VG_TOOLBAR-FUNCTION  =  ''.
*    VG_TOOLBAR-DISABLED  = 1.
*    VG_TOOLBAR-TEXT      = 'Selecionadas:'.
*    APPEND VG_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
**********************************************************************VALIDACAMPOS
    CASE e_ucomm.
      WHEN ok_vinc.

        PERFORM valida_campos.

**********************************************************************

        IF it_msg_val[] IS INITIAL.

          PERFORM vincular_notas.

        ENDIF.

*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Início de Alteração
        PERFORM consultar_alv_notas.
*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Fim de Alteração

        CALL METHOD plan_alv_notasfiscais->refresh_table_display.
        CALL METHOD plan_alv_reme_notas->refresh_table_display.
        IF NOT plan_alv_remetente IS INITIAL.
          CALL METHOD plan_alv_remetente->refresh_table_display.
        ENDIF.
        LEAVE TO SCREEN 0001.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_vinc IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager_vinc
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA : vg_toolbar TYPE stb_button.

    vg_toolbar-icon      =  icon_unassign.
    vg_toolbar-function  =  ok_desv.
    vg_toolbar-text      = 'Desv.'.
    APPEND vg_toolbar TO e_object->mt_toolbar.

    CLEAR: vg_toolbar.
    vg_toolbar-butn_type = '3'.
    APPEND vg_toolbar TO e_object->mt_toolbar.

    IF wa_filtro_remetente-tp_vinc1 IS INITIAL.

      CLEAR: vg_toolbar.
      vg_toolbar-icon      =  icon_import_all_requests.
      vg_toolbar-function  =  ok_comp.
      APPEND vg_toolbar TO e_object->mt_toolbar.

      CLEAR: vg_toolbar.
      vg_toolbar-butn_type = '3'.
      APPEND vg_toolbar TO e_object->mt_toolbar.

      CLEAR: vg_toolbar.
      vg_toolbar-icon      =  icon_import_transport_request.
      vg_toolbar-function  =  ok_domp.
      APPEND vg_toolbar TO e_object->mt_toolbar.

      CLEAR: vg_toolbar.
      vg_toolbar-butn_type = '3'.
      APPEND vg_toolbar TO e_object->mt_toolbar.

    ENDIF.

    IF vg_troca_notas = 'X'.
      CLEAR: vg_toolbar.
      vg_toolbar-icon      =  icon_enter_more.
      vg_toolbar-function  =  ok_dlog.
      vg_toolbar-text      = 'Trocar'.
      APPEND vg_toolbar TO e_object->mt_toolbar.

      CLEAR: vg_toolbar.
      vg_toolbar-icon      =  icon_change.
      vg_toolbar-function  =  ok_dcon.
      vg_toolbar-text      = 'Gravar Troca'.
      APPEND vg_toolbar TO e_object->mt_toolbar.

      CLEAR: vg_toolbar.
      vg_toolbar-icon      =  icon_system_undo.
      vg_toolbar-function  =  ok_dund.
      vg_toolbar-text      = 'Reinicializar Troca'.
      APPEND vg_toolbar TO e_object->mt_toolbar.

      CLEAR: vg_toolbar.
      vg_toolbar-icon      =  icon_delete.
      vg_toolbar-function  =  ok_vlog.
      vg_toolbar-text      = 'LOG Troca'.
      APPEND vg_toolbar TO e_object->mt_toolbar.
    ENDIF.

    CALL METHOD c_alv_toolbarmanager_vinc->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN ok_desv.
        PERFORM desvincular_notas.
*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Início de Alteração
        PERFORM consultar_alv_notas.
*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Fim de Alteração
        CALL METHOD plan_alv_notasfiscais->refresh_table_display.
        CALL METHOD plan_alv_reme_notas->refresh_table_display.
        IF NOT plan_alv_remetente IS INITIAL.
          CALL METHOD plan_alv_remetente->refresh_table_display.
        ENDIF.
        LEAVE TO SCREEN 0001.
      WHEN ok_vlog.
        SELECT *
          FROM znom_reme_notlog
          INTO TABLE tg_notas
          WHERE id_nomeacao_tran  = wa_znom_programacao-id_nomeacao_tran
          AND   id_empresa        = wa_znom_programacao-id_empresa
          AND   id_filial         = wa_znom_programacao-id_filial
          ORDER BY grp_retorno seq_log.

        IF tg_notas[] IS NOT INITIAL.
          SELECT *
           FROM zdoc_nf_prod_log
           INTO TABLE tg_prod
           FOR ALL ENTRIES IN tg_notas
           WHERE docnum_prod = tg_notas-docnum
           AND   itmnum_prod = tg_notas-itmnum
           AND   grp_retorno = tg_notas-grp_retorno
           AND   seq_log     = tg_notas-seq_log.
          SORT tg_prod BY grp_retorno seq_log.

          SELECT *
          FROM znom_reme_log
          INTO TABLE tg_reme
          WHERE id_nomeacao_tran  = wa_znom_programacao-id_nomeacao_tran
          AND   id_empresa        = wa_znom_programacao-id_empresa
          AND   id_filial         = wa_znom_programacao-id_filial
          ORDER BY grp_retorno seq_log.

          CALL SCREEN 0050 STARTING AT 20  1
                ENDING   AT 130 27.
        ELSE.
          MESSAGE 'Não existe LOG para esta Nomeação/Filial' TYPE 'I'.
        ENDIF.
      WHEN ok_dund.
        "manter a mesma programação
        MOVE-CORRESPONDING wa_znom_programacao TO wa_znom_programacao_f.
        PERFORM consulta_programacoes.
        PERFORM troca_aba_03 USING 'S'.
        CALL METHOD plan_alv_notasfiscais->refresh_table_display.
        CALL METHOD plan_alv_reme_notas->refresh_table_display.
        IF NOT plan_alv_remetente IS INITIAL.
          CALL METHOD plan_alv_remetente->refresh_table_display.
        ENDIF.
        LEAVE TO SCREEN 0001.
      WHEN ok_dlog.
        PERFORM desvincular_notlog.
        CALL METHOD plan_alv_notasfiscais->refresh_table_display.
        CALL METHOD plan_alv_reme_notas->refresh_table_display.
        IF NOT plan_alv_remetente IS INITIAL.
          CALL METHOD plan_alv_remetente->refresh_table_display.
        ENDIF.
        LEAVE TO SCREEN 0001.
      WHEN ok_dcon.
        IF it_znom_reme_dnotas_alv[] IS INITIAL.
          MESSAGE 'Não há notas para serem trocadas!' TYPE 'I'.
          EXIT.
        ENDIF.
        IF wa_znom_programacao_alv-nr_qtde_saldo_rem NE 0.
          MESSAGE 'Saldo deve ser Zero!' TYPE 'I'.
          EXIT.
        ENDIF.
        PERFORM gravar_notlog.
        "
        "manter a mesma programação
        MOVE-CORRESPONDING wa_znom_programacao TO wa_znom_programacao_f.
        PERFORM consulta_programacoes.
        PERFORM troca_aba_03 USING 'S'.
        "
        CALL METHOD plan_alv_notasfiscais->refresh_table_display.
        CALL METHOD plan_alv_reme_notas->refresh_table_display.
        IF NOT plan_alv_remetente IS INITIAL.
          CALL METHOD plan_alv_remetente->refresh_table_display.
        ENDIF.
        LEAVE TO SCREEN 0001.

      WHEN ok_comp.

        PERFORM valida_campos.


        READ TABLE it_msg_val INTO ls_msg_val INDEX 1.
        IF sy-subrc NE 0.

          IF wa_filtro_remetente-grp_retorno IS NOT INITIAL OR
              it_znom_reme_dnotas_alv[] IS NOT INITIAL. "ALRS.

            "distribui a quantidade pelos grupos
            REFRESH it_grp_retorno.
            IF it_znom_reme_dnotas_alv[] IS NOT INITIAL. "ALRS
              LOOP AT it_znom_reme_dnotas_alv INTO wa_znom_reme_dnotas_alv.
                wa_grp_retorno-grp_retorno  = wa_znom_reme_dnotas_alv-grp_retorno.
                wa_grp_retorno-docnum_rt    = wa_znom_reme_dnotas_alv-docnum_rt.
                wa_grp_retorno-nr_ordem     = wa_znom_reme_dnotas_alv-nr_ordem.
                wa_grp_retorno-total        = wa_znom_reme_dnotas_alv-nr_quantidade.
                wa_grp_retorno-novos        = 0.
                wa_grp_retorno-saldo        = 0.
                wa_grp_retorno-quantidade   = 0.
                COLLECT wa_grp_retorno INTO it_grp_retorno.
              ENDLOOP.

              LOOP AT it_znom_reme_notas INTO wa_znom_reme_notas.  "Novos - Notas.
                IF wa_znom_reme_notas-mandt = 999.
                  wa_grp_retorno-grp_retorno  = wa_znom_reme_notas-grp_retorno.
                  wa_grp_retorno-docnum_rt    = wa_znom_reme_notas-docnum_rt.
                  wa_grp_retorno-nr_ordem     = wa_znom_reme_notas-nr_ordem.
                  wa_grp_retorno-total        = 0.
                  wa_grp_retorno-novos        = wa_znom_reme_notas-nr_quantidade.
                  wa_grp_retorno-saldo        = 0.
                  wa_grp_retorno-quantidade   = 0.
                  COLLECT wa_grp_retorno INTO it_grp_retorno.
                ENDIF.
              ENDLOOP.

              LOOP AT it_grp_retorno INTO wa_grp_retorno.
                wa_grp_retorno-saldo = wa_grp_retorno-total - wa_grp_retorno-novos.
                MODIFY it_grp_retorno FROM wa_grp_retorno INDEX sy-tabix TRANSPORTING saldo.
              ENDLOOP.
              DELETE it_grp_retorno WHERE saldo = 0.
            ENDIF.

            vg_qtde_saldo_rem = wa_znom_programacao_alv-nr_qtde_saldo_rem.

            LOOP AT it_grp_retorno INTO wa_grp_retorno.

              IF wa_grp_retorno-saldo GE vg_qtde_saldo_rem.
                wa_grp_retorno-quantidade = vg_qtde_saldo_rem.
                MODIFY it_grp_retorno FROM wa_grp_retorno INDEX sy-tabix TRANSPORTING quantidade.
                EXIT.
              ELSE.
                vg_qtde_saldo_rem = vg_qtde_saldo_rem - wa_grp_retorno-saldo.
                wa_grp_retorno-quantidade = wa_grp_retorno-saldo.
                MODIFY it_grp_retorno FROM wa_grp_retorno INDEX sy-tabix TRANSPORTING quantidade.
              ENDIF.
            ENDLOOP.

            DELETE it_grp_retorno WHERE quantidade = 0.
            IF it_grp_retorno[] IS INITIAL.
              PERFORM vincular_saldo_filial.
            ELSE.
              LOOP AT it_grp_retorno INTO wa_grp_retorno.
                wa_znom_programacao_alv-nr_qtde_saldo_rem = wa_grp_retorno-quantidade.
                wa_filtro_remetente-grp_retorno = wa_grp_retorno-grp_retorno.
                PERFORM vincular_saldo_filial.
              ENDLOOP.
            ENDIF.

            CALL METHOD plan_alv_notasfiscais->refresh_table_display.
            CALL METHOD plan_alv_reme_notas->refresh_table_display.
            IF NOT plan_alv_remetente IS INITIAL.
              CALL METHOD plan_alv_remetente->refresh_table_display.
            ENDIF.
          ELSE.
            MESSAGE TEXT-054 TYPE 'S'.
          ENDIF.
        ELSE.
          MESSAGE ls_msg_val-msg TYPE 'S'.
        ENDIF.

        LEAVE TO SCREEN 0001.
      WHEN ok_domp.
        PERFORM des_vincular_saldo_filial.
        CALL METHOD plan_alv_notasfiscais->refresh_table_display.
        CALL METHOD plan_alv_reme_notas->refresh_table_display.
        IF NOT plan_alv_remetente IS INITIAL.
          CALL METHOD plan_alv_remetente->refresh_table_display.
        ENDIF.
        LEAVE TO SCREEN 0001.
    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_prog_reme IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbar_prog_reme
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA : vg_toolbar TYPE stb_button.

    vg_toolbar-icon      =  icon_insert_row.
    vg_toolbar-function  =  ok_bt_reme.
    APPEND vg_toolbar TO e_object->mt_toolbar.

    vg_toolbar-icon      =  icon_delete_row.
    vg_toolbar-function  =  ok_excl.
    APPEND vg_toolbar TO e_object->mt_toolbar.

    vg_toolbar-icon      =  icon_open.
    vg_toolbar-function  =  ok_bt_blre.
    APPEND vg_toolbar TO e_object->mt_toolbar.

    vg_toolbar-icon      =  icon_close_object.
    vg_toolbar-function  =  ok_bt_blfe.
    APPEND vg_toolbar TO e_object->mt_toolbar.

    CALL METHOD c_alv_toolbar_prog_reme->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN ok_excl.
        PERFORM excluir_remessa.
        LEAVE TO SCREEN 0001.
      WHEN ok_bt_reme.
        PERFORM incluir_remessa.
        LEAVE TO SCREEN 0001.
      WHEN ok_bt_blre.
        PERFORM troca_aba_04a.
        LEAVE TO SCREEN 0001.
      WHEN ok_bt_blfe.
        PERFORM limpa_aba_04a.
        LEAVE TO SCREEN 0001.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_conhec_vinc IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbar_conhec_vinc
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD free.
    CALL METHOD me->free.
  ENDMETHOD.                    "free

  METHOD on_toolbar.
    DATA : vg_toolbar TYPE stb_button.

    vg_toolbar-icon      =  icon_unassign.
    vg_toolbar-function  =  ok_desv.
    vg_toolbar-text      = 'Desv.'.
    APPEND vg_toolbar TO e_object->mt_toolbar.

    CALL METHOD c_alv_toolbar_conhec_vinc->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN ok_desv.
        PERFORM desvincular_conhecimento.
        CALL METHOD plan_alv_conhec_vinc->refresh_table_display.
        LEAVE TO SCREEN 0001.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_conhec_a_vinc IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbar_conhec_a_vinc
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD free.
    CALL METHOD me->free.
  ENDMETHOD.                    "free

  METHOD on_toolbar.
    DATA : vg_toolbar TYPE stb_button.
    vg_toolbar-icon      =  icon_assign.
    vg_toolbar-function  =  ok_vinc.
    vg_toolbar-text      = 'Vinc.'.
    APPEND vg_toolbar TO e_object->mt_toolbar.

    CALL METHOD c_alv_toolbar_conhec_a_vinc->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN ok_vinc.
        PERFORM vincular_conhecimento.
        CALL METHOD plan_alv_conhec_a_vinc->refresh_table_display.
        LEAVE TO SCREEN 0001.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM authority_check  USING u_tcd u_valid.

  CLEAR: u_valid.
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD u_tcd.
  IF sy-subrc NE 0.
    MESSAGE s172(00) WITH u_tcd.
    u_valid = 'X'.
  ENDIF.

ENDFORM.                    " AUTHORITY_CHECK

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_remessa_re IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT c_alv_toolbar_remessa_re
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA : vg_toolbar TYPE stb_button.
    vg_toolbar-icon      =  icon_delete_row.
    vg_toolbar-function  =  ok_excl.
    APPEND vg_toolbar TO e_object->mt_toolbar.

    CALL METHOD c_alv_toolbar_remessa_re->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN ok_excl.
        PERFORM excluir_produtores.
    ENDCASE.
  ENDMETHOD.                    "handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION



CLASS lcl_alv_toolbar_conf_exp IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA : vg_toolbar TYPE stb_button.

    vg_toolbar-icon      =  icon_intensify.
    vg_toolbar-function  =  ok_inf_qtd_exp.
    vg_toolbar-text      = 'Inf. Quantidade Exportada'.
    APPEND vg_toolbar TO e_object->mt_toolbar.

    vg_toolbar-icon      =  icon_okay.
    vg_toolbar-function  =  ok_val_qtd_exp.
    vg_toolbar-text      = 'Validar Quantidades'.
    APPEND vg_toolbar TO e_object->mt_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: it_selected_rows TYPE lvc_t_row,
          wa_selected_rows TYPE lvc_s_row.

    CASE e_ucomm.
      WHEN ok_inf_qtd_exp.

        CLEAR: wa_due_ret_conf_alv.

        CALL METHOD plan_alv_due_ret_conf->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        IF lines( it_selected_rows[] ) NE 1.
          MESSAGE 'Selecione um registro!' TYPE 'I'.
          EXIT.
        ENDIF.

        READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.

        CHECK sy-subrc EQ 0.

        READ TABLE it_due_ret_conf_alv INTO wa_due_ret_conf_alv INDEX wa_selected_rows-index.

        CHECK sy-subrc = 0.

        CLEAR: wg_peso_liq_exp.

        CALL SCREEN 0071 STARTING AT 07 05 ENDING AT 45 05.

        PERFORM consulta_due_retificacao.

        LEAVE TO SCREEN 0001.
      WHEN ok_val_qtd_exp.

        DATA(_validou) = abap_false.
        PERFORM f_valida_qtde_exp CHANGING _validou.


    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


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
  CLEAR wa_dta.
  wa_dta-program   = p_program.
  wa_dta-dynpro    = p_dynpro.
  wa_dta-dynbegin  = p_start.
  wa_dta-fnam      = p_fnam.
  wa_dta-fval      = p_fval.
  APPEND wa_dta TO it_dta.
ENDFORM.                    "F_BDC_DATA


*&---------------------------------------------------------------------*
*&      Form  F_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*      -->P_MODE     text
*      -->P_UPD      text
*----------------------------------------------------------------------*
FORM f_call_transaction USING p_trans
                              p_mode
                              p_upd.

*  DATA: BEGIN OF tl_msg OCCURS 0,
*         msg TYPE t100-text,
*         fld TYPE bdcmsgcoll-fldname,
*        END OF tl_msg.

  REFRESH: it_msg, it_msgtext.

  CALL TRANSACTION p_trans USING it_dta
    MODE p_mode
    MESSAGES INTO it_msg
    UPDATE p_upd.

  IF it_msg[] IS NOT INITIAL.
    SELECT text
      FROM t100
      INTO TABLE it_msgtext
      FOR ALL ENTRIES IN it_msg
      WHERE arbgb = it_msg-msgid AND
            msgnr = it_msg-msgnr AND
            sprsl = sy-langu.

    LOOP AT it_msgtext.
      TRANSLATE it_msgtext-texto USING '& '.
      CONDENSE it_msgtext-texto.
      MODIFY it_msgtext.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "F_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*& Form VALIDA_CAMPOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM valida_campos .

  SELECT SINGLE * FROM znom_remetente WHERE numero_due = @wa_filtro_remetente-numero_due INTO @DATA(get_znom_remetente) .

*        IF wa_filtro_remetente-grp_retorno IS INITIAL AND
* it_znom_reme_dnotas_alv[] IS INITIAL. "ALRS.
*          MESSAGE TEXT-054 TYPE 'I'.
*          EXIT.

  CLEAR: it_msg_val[].

  IF it_znom_reme_dnotas_alv[] IS INITIAL OR wa_filtro_remetente-grp_retorno IS INITIAL.

    TYPES: BEGIN OF ty_field,
             field TYPE string,
             value TYPE string,
           END OF ty_field.

    DATA it_filed_validation TYPE STANDARD TABLE OF ty_field INITIAL SIZE 0.
    DATA ls_filed_validation TYPE ty_field.
    CLEAR: it_filed_validation[].

    ls_filed_validation-field = 'grp_retorno'.
    ls_filed_validation-value = wa_filtro_remetente-grp_retorno.
    APPEND ls_filed_validation TO it_filed_validation.

    ls_filed_validation-field = 'tipov'.
    ls_filed_validation-value = wa_filtro_remetente-tipov.
    APPEND ls_filed_validation TO it_filed_validation.

    ls_filed_validation-field = 'preco'.
    ls_filed_validation-value = wa_filtro_remetente-preco.
    APPEND ls_filed_validation TO it_filed_validation.

    ls_filed_validation-field = 'safra'.
    ls_filed_validation-value = wa_filtro_remetente-safra.
    APPEND ls_filed_validation TO it_filed_validation.

    ls_filed_validation-field = 'cvirt'.
    ls_filed_validation-value = wa_filtro_remetente-cvirt.
    APPEND ls_filed_validation TO it_filed_validation.

    ls_filed_validation-field = 'depst'.
    ls_filed_validation-value = wa_filtro_remetente-depst.
    APPEND ls_filed_validation TO it_filed_validation.



*    TYPES: BEGIN OF ty_msg,
*             msg TYPE string,
*           END OF ty_msg.

*    DATA it_msg TYPE STANDARD TABLE OF ty_msg INITIAL SIZE 0.
*    DATA ls_msg TYPE ty_msg.

    DATA: vl_preco TYPE dmbtr.
    DATA: wa_msg TYPE ty_msg.
    DATA lv_msg TYPE string.
    DATA lv_msg_tit TYPE string.

*    CLEAR: it_msg_val[].

    LOOP AT it_filed_validation ASSIGNING FIELD-SYMBOL(<field_value>).

      IF <field_value>-field = 'preco'.
        vl_preco = <field_value>-value.
      ENDIF.
      IF <field_value>-field = 'grp_retorno' AND <field_value>-value IS INITIAL.
        CLEAR: ls_msg_val.
        ls_msg_val-msg = 'O campo Grp. Retorno é obrigatório!'.
        MESSAGE ls_msg_val-msg TYPE 'S'.
        SET SCREEN 0.
        APPEND ls_msg_val TO it_msg_val.
        EXIT.
      ELSEIF <field_value>-field = 'tipov' AND <field_value>-value IS INITIAL.
        CLEAR: ls_msg_val.
        ls_msg_val-msg =  'O campo Tipo OV é obrigatório!'.
        MESSAGE ls_msg_val-msg TYPE 'S'.
        SET SCREEN 0.
        APPEND ls_msg_val TO it_msg_val.
        EXIT.
      ELSEIF <field_value>-field = 'preco' AND vl_preco EQ '0'.
        CLEAR: ls_msg_val.
        ls_msg_val-msg =  'O campo Preço é obrigatório!'.
        MESSAGE ls_msg_val-msg TYPE 'S'.
        SET SCREEN 0.
        APPEND ls_msg_val TO it_msg_val.
        EXIT.
      ELSEIF <field_value>-field = 'depst' AND <field_value>-value IS INITIAL.
        CLEAR: ls_msg_val.
        ls_msg_val-msg =  'O campo Deposito é obrigatório!'.
        MESSAGE ls_msg_val-msg TYPE 'S'.
        SET SCREEN 0.
        APPEND ls_msg_val TO it_msg_val.
        EXIT.
      ELSEIF <field_value>-field = 'safra' AND <field_value>-value IS INITIAL.
        CLEAR: ls_msg_val.
        ls_msg_val-msg =  'O campo Safra é obrigatório!'.
        MESSAGE ls_msg_val-msg TYPE 'S'.
        SET SCREEN 0.
        APPEND ls_msg_val TO it_msg_val.
        EXIT.
      ELSEIF <field_value>-field = 'cvirt' AND <field_value>-value IS INITIAL.
        CLEAR: ls_msg_val.
        ls_msg_val-msg =  'O campo Centro Virtual é obrigatório!'.
        MESSAGE ls_msg_val-msg TYPE 'S'.
        SET SCREEN 0.
        APPEND ls_msg_val TO it_msg_val.
        EXIT.
      ENDIF.

    ENDLOOP.

*    lv_msg_TIT = |Existem campos de preenchimento obrigatorio! |.
*    CONSTANTS: c_delimiter(1) TYPE c VALUE cl_abap_char_utilities=>newline.
*
*    lv_msg = lv_msg_TIT.
*    LOOP AT it_msg ASSIGNING FIELD-SYMBOL(<msg>).
*      CONCATENATE lv_msg  <msg>-msg  INTO lv_msg SEPARATED BY space.
*    ENDLOOP.

    "MESSAGE TEXT-054 TYPE 'S'.

*    MESSAGE lv_msg_TIT TYPE 'S'.
*    SET SCREEN 0.
  ENDIF.
ENDFORM.
