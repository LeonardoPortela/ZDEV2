FUNCTION-POOL zgfs_insumos.                 "MESSAGE-ID ..

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_log,
         status   TYPE icon_d,
         id_seq   TYPE zsdt0313-id_seq,
         id_log   TYPE zsdt0313-id_log,
         nr_venda TYPE zsdt0310-nr_venda,
         tpdoc    TYPE zsdt0310-tipo_doc,
         tipo_doc TYPE char30,
         mensagem TYPE zsdt0313-mensagem,
         usname   TYPE zsdt0313-usname,
         data     TYPE zsdt0313-data,
         hora     TYPE zsdt0313-hora.
TYPES: END OF ty_log.

TYPES: BEGIN OF ty_parti,
         status    TYPE icon_d,
         nome      TYPE zsde0075-nome,
         codigo    TYPE zsde0075-codigo,
         email     TYPE zsde0075-email,
         descricao TYPE zsde0075-situacaoassinatura-descricao.
TYPES: END   OF ty_parti.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END   OF ty_estrutura.

**********************************************************************
* VARIAVEIS
**********************************************************************
DATA: t_log     TYPE TABLE OF ty_log,
      w_log     TYPE ty_log,
*
      p_insumo  TYPE char1,
      p_merint  TYPE char1,
      p_sintet  TYPE char1,
      p_analit  TYPE char1,
      p_contra  TYPE char1,
      p_venda   TYPE char1,
      p_distra  TYPE char1,
      p_aditiv  TYPE char1,
      p_decrec  TYPE char1,
      p_doctod  TYPE char1,
      p_pend    TYPE char1,
      p_conclu  TYPE char1,
      p_todos   TYPE char1,
      l_imprime TYPE c,
      gv_id     TYPE zid_documento, " US #158242 - RAMON
      gv_wf_id  TYPE string. " US #158242 - RAMON
*     p_vlr        TYPE c,
*     p_manual     TYPE char1,
*     p_eletronica TYPE char1.
*
**********************************************************************
* VARIAVEIS
**********************************************************************
DATA: g_grid                 TYPE REF TO cl_gui_alv_grid,
      g_custom_container     TYPE REF TO cl_gui_custom_container,
      g_grid_pop             TYPE REF TO cl_gui_alv_grid,
      g_custom_container_pop TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager   TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1            TYPE REF TO cl_gui_container,
      cl_container_95        TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id          TYPE REF TO cl_dd_document,
*
      cl_editor              TYPE REF TO cl_gui_textedit,
      container_editor       TYPE REF TO cl_gui_custom_container,
*
      picture                TYPE REF TO cl_gui_picture,
      l_graphic_conv         TYPE i,
      l_graphic_offs         TYPE i,
      graphic_size           TYPE i,
      l_graphic_xstr         TYPE xstring,
      url(255)               TYPE c,
      graphic_url(255),
      t_function             TYPE ui_functions,
      w_function             TYPE ui_func,
*
      t_zsdt0090             TYPE TABLE OF zsdt0090,
      w_zsdt0090             TYPE zsdt0090,
      t_zsdt0040             TYPE TABLE OF zsdt0040,
      w_zsdt0040             TYPE zsdt0040,
      t_zsdt0041             TYPE TABLE OF zsdt0041,
      w_zsdt0041             TYPE zsdt0041,
      t_zsdt0310             TYPE TABLE OF zsdt0310,
      w_zsdt0310             TYPE zsdt0310,
      t_zsdt0310_aux         TYPE TABLE OF zsdt0310,
      w_zsdt0310_aux         TYPE zsdt0310,
      t_zsdt0312             TYPE TABLE OF zsdt0312,
      w_zsdt0312             TYPE zsdt0312,
      t_zsdt0313             TYPE TABLE OF zsdt0313,
      w_zsdt0313             TYPE zsdt0313,
      t_zsdt0314             TYPE TABLE OF zsdt0314,
      w_zsdt0314             TYPE zsdt0314,
      t_tvkbt                TYPE TABLE OF tvkbt,
      t_kna1                 TYPE TABLE OF kna1,
      w_kna1                 TYPE kna1,
      w_tvkbt                TYPE tvkbt,
      t_tpvenda              TYPE TABLE OF rgsb4,
      w_tpvenda              TYPE rgsb4,
      t_tipodoc              TYPE TABLE OF dd07v,
      w_tipodoc              TYPE dd07v,
      t_status               TYPE TABLE OF dd07v,
      w_status               TYPE dd07v,
      t_assinantes           TYPE zsde0075_t,
      w_assinantes           TYPE zsde0075,
      t_parti                TYPE TABLE OF ty_parti,
      w_parti                TYPE ty_parti,
*
      it_fieldcat            TYPE TABLE OF ty_estrutura,
      wa_estrutura           TYPE ty_estrutura,
      wa_fieldcat            TYPE ty_estrutura,
      ls_variant             TYPE disvariant,
      l_grid_title           TYPE lvc_title,
*
      t_fieldcat             TYPE lvc_t_fcat,
      w_fieldcat             TYPE lvc_s_fcat,
      t_sort                 TYPE lvc_t_sort,
      w_sort                 TYPE lvc_s_sort,
      t_color                TYPE lvc_t_scol,
      w_color                TYPE lvc_s_scol,
      t_exctab               TYPE slis_t_extab,
      w_exctab               TYPE slis_extab,
      w_layout               TYPE lvc_s_layo,
      w_stable               TYPE lvc_s_stbl    VALUE 'XX',
      t_style                TYPE lvc_t_styl,
      w_style                TYPE lvc_s_styl,
      t_rows                 TYPE lvc_t_row,
      w_rows                 TYPE lvc_s_row,
      t_bdc                  TYPE TABLE OF bdcdata,
      w_bdc                  TYPE bdcdata,
*
*     p_insumo             TYPE char1,
*     p_merint             TYPE char1,
*     p_sintet             TYPE char1,
*     p_analit             TYPE char1,
*
      p_vlr                  TYPE char1,
      p_manual               TYPE char1,
      p_eletronica           TYPE char1,
*
      l_qtd                  TYPE i,
      l_tabix                TYPE sy-tabix,
      l_cockpit              TYPE char02,
      l_erro                 TYPE c,
      l_pend                 TYPE c,
      l_tem_0041             TYPE c,
      l_tem_0310             TYPE c,
      l_status               TYPE icon_d,
      l_editar               TYPE c,
      l_werks                TYPE char10,
      l_lote_editado         TYPE numc10,
      l_id_seq               TYPE zsdt0225-id_seq,
      l_resp                 TYPE c,
      ok_code                TYPE sy-ucomm,
      ok_code2               TYPE sy-ucomm,
      ok_code3               TYPE sy-ucomm,
      r_matkl_sel            TYPE RANGE OF matkl WITH HEADER LINE,
*
      zcl_util               TYPE REF TO zcl_util,
      zcl_insumos            TYPE REF TO zcl_integracao_insumos,
*
      t_tabedit              TYPE TABLE OF char4000,        "char0241,
      w_tabedit              LIKE LINE OF t_tabedit,
*
      l_sel_button           TYPE smp_dyntxt,

      BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

RANGES: s_vkorg    FOR zsdt0040-vkorg,
        s_vkbur    FOR zsdt0040-vkbur,
        s_docsi    FOR zsdt0040-doc_simulacao,
        s_kunnr    FOR zsdt0040-kunnr,
        s_erdat    FOR zsdt0040-erdat,
        s_cultu    FOR zsdt0040-cultura,
        s_safra    FOR zsdt0040-safra,
        s_spart    FOR zsdt0040-spart,
        s_moeda    FOR zsdt0040-waerk.

*INCLUDE zsdr0150_top_comum.

***********************************************************************************
***********************************************************************************
