FUNCTION-POOL zgfs_trace_cotton.            "MESSAGE-ID ..

TABLES: j_1bbranch,  j_1bnflin, zsdt0001, j_1bnfe_active, zmmt0126.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_lotes,
         mark                 TYPE char1,
         zseq_inst            TYPE zseq_inst,
         objek                TYPE objnum,
         objecttable          TYPE tabelle,
         instrucao            TYPE zsded030,
         referencia           TYPE numc10,
         contrato             TYPE text50,
         matnr                TYPE matnr,
         werks                TYPE werks_ext,
         ponto_c              TYPE lifnr,
         terminal             TYPE lifnr,
         charg                TYPE charg_d,
         quantidade           TYPE numc10,
         quantidade_disp      TYPE int4,  "*-CS2023000189-04.09.2023-#122555-JT
         quantidade_util      TYPE int4,  "*-CS2023000189-04.09.2023-#122555-JT
         quantidade_disp_orig TYPE int4,  "*-CS2023000189-04.09.2023-#122555-JT
         quantidade_util_orig TYPE int4,  "*-CS2023000189-04.09.2023-#122555-JT
         dmbtr                TYPE mcl_bzeitd, "/bev3/chdec17_5,
         btgew                TYPE gsgew,
         gewei                TYPE gewei,
       END OF ty_lotes,

*** Inicio - Rubenilson Pereira - 03.03.2025 - US164130
       BEGIN OF ty_lotes2,
         seq                  TYPE zsdt0045-zseq_inst,
         instrucao            TYPE zsded030,
         matnr                TYPE matnr,
         werks                TYPE werks_ext,
         quantidade           TYPE numc10,
         quantidade_disp      TYPE int4,
         quantidade_util      TYPE int4,
         quantidade_disp_orig TYPE int4,
         quantidade_util_orig TYPE int4,
         dmbtr                TYPE char20,
         btgew                TYPE gsgew,
         gewei                TYPE gewei,
         inco1                TYPE zsdt0045-incoterm,
         region               TYPE t001w-regio,
       END OF ty_lotes2,

       BEGIN OF ty_lotes3,
         seq                  TYPE zsdt0045-zseq_inst,
         objek                TYPE objnum,
         objecttable          TYPE tabelle,
         referencia           TYPE numc10,
         instrucao            TYPE zsded030,
         matnr                TYPE matnr,
         werks                TYPE werks_ext,
         quantidade           TYPE numc10,
         quantidade_disp      TYPE int4,
         quantidade_util      TYPE int4,
         quantidade_disp_orig TYPE int4,
         quantidade_util_orig TYPE int4,
         dmbtr                TYPE char20,
         btgew                TYPE gsgew,
         gewei                TYPE gewei,
         inco1                TYPE zsdt0045-incoterm,
         region               TYPE t001w-regio,
         charg                TYPE charg_d,
         contrato             TYPE text50,
       END OF ty_lotes3,
*** Fim - Rubenilson Pereira - 03.03.2025 - US164130

       BEGIN OF ty_vbrp,
         vbeln  TYPE vbrp-vbeln,
         vgbel  TYPE vbrp-vgbel,
         matnr  TYPE vbrp-matnr,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_vbrp,

       BEGIN OF ty_lin,
         docnum TYPE j_1bnflin-docnum,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_lin,

       BEGIN OF ty_bnfe,
         docnum TYPE j_1bnfe_active-docnum,
         nfnum9 TYPE j_1bnfe_active-nfnum9,
         bukrs  TYPE j_1bnfe_active-bukrs,
         branch TYPE j_1bnfe_active-branch,
       END OF ty_bnfe,

       BEGIN OF ty_zmmt0008,
         werks       TYPE zmmt0008-werks,
         lgort       TYPE zmmt0008-lgort,
         nr_romaneio TYPE zmmt0008-nr_romaneio,
         charg       TYPE zmmt0008-charg,
         menge       TYPE zmmt0008-menge,
         werks_orig  TYPE zmmt0008-werks_orig,
       END OF ty_zmmt0008,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         normt TYPE mara-normt,
       END OF ty_mara,

       BEGIN OF ty_saida,
         mark        TYPE c,
         werks       TYPE zmmt0008-werks,
         bloco       TYPE zmmt0008-lgort,
         fardo       TYPE zmmt0008-charg,
         nr_romaneio TYPE zmmt0008-nr_romaneio,
         nfnum       TYPE zmmt0008-nfnum,
         vbeln       TYPE zmmt0008-vbeln,
         vbeln_vf    TYPE zmmt0008-vbeln_vf,
         placa_cav   TYPE zmmt0008-placa_cav,
         matnr       TYPE vbrp-matnr,
         menge       TYPE zmmt0008-menge,
         werks_orig  TYPE zmmt0008-werks_orig,
       END OF ty_saida.

TYPES: BEGIN OF ty_det.
         INCLUDE TYPE zsdt0327.
TYPES:   cd_sai TYPE zsdt0331-cd_sai.
TYPES: status TYPE char4.
TYPES: END   OF ty_det.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_zsdt0330.
         INCLUDE TYPE zsdt0330.
TYPES:   charg TYPE vbap-charg.
TYPES: END   OF ty_zsdt0330.

*** Inicio - Rubenilson Pereira - 03.03.2025 - US164130
TYPES: tb_retorno TYPE TABLE OF zsdt0328,
       tb_lotes2  TYPE TABLE OF ty_lotes2,
       tb_lotes3  TYPE TABLE OF ty_lotes3.
*** Fim - Rubenilson Pereira - 03.03.2025 - US164130

**********************************************************************
* VARIAVEIS
**********************************************************************
DATA: l_error TYPE c,
      l_mesg  TYPE string.

DATA: t_lotes                TYPE TABLE OF ty_lotes,
      w_lotes                TYPE ty_lotes,
      w_lotes_sel            TYPE ty_lotes,
      t_saida                TYPE TABLE OF zsdt0328,
      w_saida                TYPE zsdt0328,
      t_entrada              TYPE TABLE OF zsdt0328,
      w_entrada              TYPE zsdt0328,
      t_zsdt0045             TYPE TABLE OF zsdt0045,
      w_zsdt0045             TYPE zsdt0045,
      t_zsdt0066             TYPE TABLE OF zsdt0066, "*-CS2023000189-04.09.2023-#122555-JT
      w_zsdt0066             TYPE zsdt0066,          "*-CS2023000189-04.09.2023-#122555-JT
      t_zsdt0328             TYPE TABLE OF zsdt0328,
      w_zsdt0328             TYPE zsdt0328,
      w_retorno              TYPE zsdt0328,
      t_zsdt0327             TYPE TABLE OF zsdt0327,
      t_zsdt0330             TYPE TABLE OF ty_zsdt0330,
      t_zsdt0331             TYPE TABLE OF zsdt0331,
      w_zsdt0327             TYPE zsdt0327,
      w_zsdt0330             TYPE ty_zsdt0330,
      w_zsdt0331             TYPE zsdt0331,
*
      w_zsdt0001             TYPE zsdt0001,
      wg_header              TYPE zmms005,
      tg_fardos              TYPE TABLE OF zmms004 WITH HEADER LINE,
      tg_mara                TYPE TABLE OF ty_mara,
      wg_mara                TYPE ty_mara,
      tg_vbrp                TYPE TABLE OF ty_vbrp,
      wg_vbrp                TYPE ty_vbrp,
      tg_lin                 TYPE TABLE OF ty_lin,
      wg_lin                 TYPE ty_lin,
      tg_bnfe                TYPE TABLE OF ty_bnfe,
      wg_bnfe                TYPE ty_bnfe,
      tl_0008                TYPE TABLE OF zmmt0008 WITH HEADER LINE,
      t_zmmt0008             TYPE TABLE OF zmmt0008,
      w_zmmt0008             TYPE zmmt0008,
      t_zmmt0008_grp         TYPE TABLE OF zmmt0008,
      w_zmmt0008_grp         TYPE zmmt0008,
      tg_zmmt0008            TYPE TABLE OF ty_zmmt0008,
      tg_zmmt0008_aux        TYPE TABLE OF ty_zmmt0008,
      wg_zmmt0008            TYPE ty_zmmt0008,
      tg_kna1                TYPE TABLE OF ty_kna1,
      wg_kna1                TYPE ty_kna1,
      tg_saida               TYPE TABLE OF ty_saida,
      wg_saida               TYPE ty_saida,
      t_pdf_files            TYPE zsdt_pdf_files,
      w_pdf_files            TYPE zsde_pdf_files,
      l_pdf_xtring           TYPE xstring,
*
      ls_job_output_info     TYPE ssfcrescl,
      ls_control             TYPE ssfctrlop,
      ls_otfdata             TYPE tsfotf,
      ls_bin_fsize           TYPE i,
      ls_xstring_document    TYPE xstring,
      t_lines                TYPE STANDARD TABLE OF tline,
*
      ls_variant             TYPE disvariant,
      t_det                  TYPE TABLE OF ty_det,
      t_text                 TYPE TABLE OF string,
      w_text                 TYPE string,
      w_det                  TYPE ty_det,
      l_grid_title           TYPE lvc_title,
      it_fieldcat            TYPE TABLE OF ty_estrutura,
      wa_estrutura           TYPE ty_estrutura,
      wa_fieldcat            TYPE ty_estrutura,
*
      g_grid                 TYPE REF TO cl_gui_alv_grid,
      g_custom_container     TYPE REF TO cl_gui_custom_container,
      g_grid_pop             TYPE REF TO cl_gui_alv_grid,
      g_custom_container_pop TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager   TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1            TYPE REF TO cl_gui_container,
      cl_container_95        TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id          TYPE REF TO cl_dd_document,
      picture                TYPE REF TO cl_gui_picture,
      l_graphic_conv         TYPE i,
      l_graphic_offs         TYPE i,
      graphic_size           TYPE i,
      l_graphic_xstr         TYPE xstring,
      url(255)               TYPE c,
      graphic_url(255),
*
      pt_exclude             TYPE ui_functions,
      t_function             TYPE ui_functions,
      w_function             TYPE ui_func,
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
      l_tabix                TYPE sy-tabix,
      g_somente_exibe        TYPE char1,
      g_editar               TYPE char1,
      g_referencia           TYPE numc10,
      g_back                 TYPE char1,
      l_erro                 TYPE c,
      l_quantidade           TYPE i,                 "*-CS2023000189-04.09.2023-#122555-JT
      l_restante             TYPE i,                 "*-CS2023000189-04.09.2023-#122555-JT
      f_code                 TYPE TABLE OF sy-ucomm,
      ok_code                TYPE sy-ucomm,
      zcl_util               TYPE REF TO zcl_util,
*
      BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

**********************************************************************
* METODOS
**********************************************************************
INCLUDE lzgfs_trace_cottono02.

**********************************************************************
**********************************************************************
