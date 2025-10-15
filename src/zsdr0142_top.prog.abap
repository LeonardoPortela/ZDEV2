*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_TOP
*&---------------------------------------------------------------------*

**********************************************************************
* TABELAS
**********************************************************************
TABLES: t001, t001w, zsdt0001, sscrfields.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_zsdt0302,
         nro_cgd             TYPE zsdt0302-nro_cgd,
         ch_referencia       TYPE zsdt0302-ch_referencia,
         gera_solicitacao_ra TYPE zsdt0302-gera_solicitacao_ra,
         qtd_solicitacao_ra  TYPE zsdt0302-qtd_solicitacao_ra,
         nr_romaneio         TYPE zsdt0001-nr_romaneio,
         branch              TYPE zsdt0001-branch.
TYPES: END   OF ty_zsdt0302.

TYPES: BEGIN OF ty_zsdt0298,
         nro_cgd         TYPE zsdt0298-nro_cgd,
         ch_referencia   TYPE zsdt0298-ch_referencia,
         id              TYPE zsdt0298-id,
         receitakey      TYPE zsdt0298-receitakey,
         data_atual      TYPE zsdt0298-data_atual,
         tipo_assinatura TYPE zsdt0298-tipo_assinatura.
TYPES: END OF ty_zsdt0298.

TYPES: BEGIN OF ty_zsdt0218,
         numeroreceita      TYPE zsdt0218-numeroreceita,
         numeropedido       TYPE zsdt0218-numeropedido,
         cpfrt              TYPE zsdt0218-cpfrt,
         receitakey         TYPE zsdt0218-receitakey,
         dataemissao        TYPE zsdt0218-dataemissao,
         tipo_assinatura    TYPE zsdt0218-tipo_assinatura,
         chave_workflow     TYPE zsdt0218-chave_workflow,
         chave_pdf_assinado TYPE zsdt0218-chave_pdf_assinado.
TYPES: END OF ty_zsdt0218.

TYPES: BEGIN OF ty_zsdt0139,
         nro_cgd        TYPE zsdt0139-nro_cgd,
         cpf_rtc        TYPE zsdt0139-cpf_rtc,
         nome           TYPE zsdt0259-nome,
         ass_eletronica TYPE zsdt0259-ass_eletronica.
TYPES: END   OF ty_zsdt0139.

TYPES: BEGIN OF ty_assina01,
         id_referencia TYPE zint_assina01-id_referencia,
         id_processo   TYPE zint_assina01-id_processo,
         etapa         TYPE zint_assina01-etapa,
         chave_coleta  TYPE zint_assina01-chave_coleta,
         log_date      TYPE zint_assina01-log_date.
TYPES: END OF ty_assina01.

TYPES: BEGIN OF ty_tab_err,
         etapa    TYPE char2,
         mensagem TYPE char50.
TYPES: END   OF ty_tab_err.

TYPES: BEGIN OF ty_alv,
         status             TYPE icon_d,
         nro_cgd            TYPE zsdt0302-nro_cgd,
         ch_referencia      TYPE zsdt0302-ch_referencia,
         nr_romaneio        TYPE zsdt0001-nr_romaneio,
         qtd_solicitacao_ra TYPE zsdt0302-qtd_solicitacao_ra,
         id                 TYPE zsdt0298-id,
         data_atual         TYPE zsdt0298-data_atual,
         nome               TYPE zsdt0259-nome,
         receitakey         TYPE zsdt0218-receitakey,
         numeroreceita      TYPE zsdt0218-numeroreceita,
         branch             TYPE zsdt0001-branch,
         dataemissao        TYPE zsdt0218-dataemissao,
         tipo_assinatura    TYPE zsdt0298-tipo_assinatura,
         desc_assinatura    TYPE char30,
         chave_coleta       TYPE zint_assina01-chave_coleta,
         log_date           TYPE zint_assina01-log_date,
         etapa              TYPE char2,
         etapa_head         TYPE char2,
         mensagem           TYPE char50,
         cellcolor          TYPE lvc_t_scol.
TYPES: END   OF ty_alv.

**********************************************************************
* VARIAVEIS
**********************************************************************
DATA: g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      picture              TYPE REF TO cl_gui_picture,
      l_graphic_conv       TYPE i,
      l_graphic_offs       TYPE i,
      graphic_size         TYPE i,
      l_graphic_xstr       TYPE xstring,
      url(255)             TYPE c,
      graphic_url(255),
      t_function           TYPE ui_functions,
      w_function           TYPE ui_func,
*
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_sort               TYPE lvc_t_sort,
      w_sort               TYPE lvc_s_sort,
      t_color              TYPE lvc_t_scol,
      w_color              TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_alv                TYPE TABLE OF ty_alv,
      w_alv                TYPE ty_alv,
      t_tab_err            TYPE TABLE OF ty_tab_err,
      w_tab_err            TYPE ty_tab_err,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
*
      l_qtd                TYPE i,
      l_tabix              TYPE sy-tabix,
      l_cockpit            TYPE char02,
      l_erro               TYPE c,
      l_resp               TYPE c,
      ok_code              TYPE sy-ucomm,
*
      zcl_util             TYPE REF TO zcl_util,
*
      l_sel_button         TYPE smp_dyntxt,

      BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table,
*
      t_zsdt0302 TYPE TABLE OF ty_zsdt0302,
      w_zsdt0302 TYPE ty_zsdt0302,
      t_zsdt0298 TYPE TABLE OF ty_zsdt0298,
      w_zsdt0298 TYPE ty_zsdt0298,
      t_0298     TYPE TABLE OF ty_zsdt0298,
      w_0298     TYPE ty_zsdt0298,
      t_zsdt0218 TYPE TABLE OF ty_zsdt0218,
      w_zsdt0218 TYPE ty_zsdt0218,
      t_zsdt0139 TYPE TABLE OF ty_zsdt0139,
      w_zsdt0139 TYPE ty_zsdt0139,
      t_assina01 TYPE TABLE OF ty_assina01,
      w_assina01 TYPE ty_assina01.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
