*&---------------------------------------------------------------------*
*&  Include           ZSDR0112_TOP
*&---------------------------------------------------------------------*

REPORT zsdr0112.


TABLES: lfa1, kna1, t001l, mch1,  zsdt_export, zsdt0053, t001, t001w , zsdt0002, mara, zdco_produtor, zlest0146,
        j_1bnfdoc.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida_0100,
         docnum              TYPE zde_nota_retorno_rfl-docnum,
         reftyp              TYPE zde_nota_retorno_rfl-reftyp,
         vbeln_vl            TYPE zde_nota_retorno_rfl-vbeln_vl,
         mblnr_s             TYPE mkpf-mblnr,
         mjahr_s             TYPE mkpf-mjahr,
         werks_d             TYPE zsdt0023-werks_v,
         lgort_d             TYPE zsdt0023-lgort_v,
         credat              TYPE zde_nota_retorno_rfl-credat,
         pstdat              TYPE zde_nota_retorno_rfl-pstdat,
         docdat              TYPE zde_nota_retorno_rfl-docdat,
         nfenum              TYPE zde_nota_retorno_rfl-nfenum,
         bukrs               TYPE zde_nota_retorno_rfl-bukrs,
         branch              TYPE zde_nota_retorno_rfl-branch,
         werks               TYPE zde_nota_retorno_rfl-werks,
         matnr               TYPE zde_nota_retorno_rfl-matnr,
         menge               TYPE zde_nota_retorno_rfl-menge,
         meins               TYPE zde_nota_retorno_rfl-meins,
         netpr               TYPE zde_nota_retorno_rfl-netpr,
         netwrt              TYPE zde_nota_retorno_rfl-netwrt,
         charg               TYPE zde_nota_retorno_rfl-charg,
         lgort               TYPE zde_nota_retorno_rfl-lgort,
         lifnr_z1            TYPE zde_nota_retorno_rfl-lifnr_z1,
         lifnr_z1_cct        TYPE zde_nota_retorno_rfl-lifnr_z1,
         chave_nfe           TYPE zde_nota_retorno_rfl-chave_nfe,
         parid               TYPE zde_nota_retorno_rfl-parid,
         partyp              TYPE zde_nota_retorno_rfl-partyp,
         qtde_vinc           TYPE zde_nota_retorno_rfl-qtde_vinc,
         qtde_cct            TYPE zde_nota_retorno_rfl-qtde_cct,
         dt_recepcao_cct     TYPE zde_nota_retorno_rfl-dt_recepcao_cct,
         qtde_quebra         TYPE zde_nota_retorno_rfl-qtde_quebra,
         qtde_sobra          TYPE zde_nota_retorno_rfl-qtde_sobra,
         saldo_nf            TYPE zde_nota_retorno_rfl-saldo_nf,
         saldo_cct           TYPE zde_nota_retorno_rfl-saldo_cct,

         docnum_retorno      TYPE zfiwrt0008-docnum_retorno,
         docnum_ret_flag(20),
         nferet_quebra       TYPE j_1bnfdoc-nfenum, "nfenum_retorno
         nferet_flag(20),
         docdat_quebra       TYPE j_1bnfdoc-docdat, "docdat_retorno

         mblnr               TYPE mkpf-mblnr,
         mjahr               TYPE mkpf-mjahr,
         budat               TYPE mkpf-budat,
         mblnr_flag(20),

         seq_lcto_znfw       TYPE zfiwrt0008-seq_lcto,
         seq_lcto_flag(20),
         docnum_znfw         TYPE j_1bnfdoc-docnum,
         docnum_flag(20),
         nfenum_znfw         TYPE j_1bnfdoc-nfenum,
         nfenum_flag(20),
         docdat_znfw         TYPE j_1bnfdoc-docdat,
         mblnr_znfw          TYPE zfiwrt0008-mblnr,
         mblnr_znfw_flag(20),

         lcto_conc           TYPE char30,
         nferet_quebra_in    TYPE j_1bnfdoc-nfenum,
         nfenum_znfw_in      TYPE j_1bnfdoc-nfenum,
         del_registro        TYPE c,
         qtde_atribuida      TYPE zde_nota_retorno_rfl-saldo_nf,
         finalidade(30)      TYPE c,

         mblnr_cce           TYPE zde_nota_retorno_rfl-mblnr_cce,
         mjahr_cce           TYPE zde_nota_retorno_rfl-mjahr_cce,
         authcode            TYPE zcarta_correcao-authcode,
*         marc             TYPE char1,
         gerar_lcto          TYPE char1,
         dias                TYPE i,
         matkl               TYPE matkl,
         datatransb          TYPE zlest0039-datatransb,
         pesotransb          TYPE zlest0039-pesotransb.
TYPES:   color             TYPE   kkblo_specialcol OCCURS 0.
TYPES END OF ty_saida_0100.

DATA: BEGIN OF tg_mkpf OCCURS 0,
        mblnr TYPE mkpf-mblnr,
        mjahr TYPE mkpf-mjahr,
        bktxt TYPE mkpf-bktxt,
      END OF tg_mkpf,

      BEGIN OF tg_mkpf_2 OCCURS 0,
        mblnr TYPE mkpf-mblnr,
        mjahr TYPE mkpf-mjahr,
        xblnr TYPE mkpf-xblnr,
        budat TYPE mkpf-budat,
      END OF tg_mkpf_2,

      BEGIN OF tg_mseg OCCURS 0,
        mblnr TYPE mseg-mblnr,
        mjahr TYPE mseg-mjahr,
        werks TYPE mseg-werks,
        lgort TYPE mseg-lgort,
      END OF tg_mseg,

      BEGIN OF tg_mseg_2 OCCURS 0,
        mblnr TYPE mseg-mblnr,
        mjahr TYPE mseg-mjahr,
        smbln TYPE mseg-smbln,
      END OF tg_mseg_2.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
       END OF ty_mara.

TYPES: BEGIN OF ty_notas.
         INCLUDE STRUCTURE zde_nota_retorno_rfl.
       TYPES: END OF ty_notas.




*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_finished FOR EVENT finished OF cl_gui_timer.

  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_alv_toolbar_0100 DEFINITION.
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

CLASS lcl_event_handler_0100 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.


DATA: obj_alv_0100       TYPE REF TO cl_gui_alv_grid,
      obj_container_0100 TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: obj_toolbar_0100 TYPE REF TO lcl_alv_toolbar_0100.

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
DATA: wa_stable        TYPE lvc_s_stbl. " VALUE 'XX'.

DATA: it_selectedcell TYPE lvc_t_cell,
      wa_selectedcell TYPE lvc_s_cell.

DATA: it_sel_rows TYPE  lvc_t_row,
      wa_sel_rows TYPE lvc_s_row.

DATA: gt_estilo TYPE lvc_t_styl WITH HEADER LINE,
      wl_estilo TYPE lvc_s_styl.

DATA: gt_f4  TYPE lvc_t_f4 WITH HEADER LINE.

* Objetos
DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar           TYPE stb_button.

DATA: wa_estrutura TYPE ty_estrutura,
      estrutura    TYPE TABLE OF ty_estrutura.

DATA vg_atrib_qtde TYPE zde_nota_retorno_rfl-saldo_nf.

DATA: t_dd07v TYPE TABLE OF dd07v,
      s_dd07v TYPE dd07v.


*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100   TYPE TABLE OF ty_saida_0100,
      it_saida_copy   TYPE TABLE OF ty_saida_0100,
      wa_saida_0100   TYPE ty_saida_0100,
      wa_saida_lc     TYPE ty_saida_0100,
      wa_saida_copy   TYPE ty_saida_0100,
      tg_zsdt0023     TYPE TABLE OF zsdt0023 WITH HEADER LINE,
      tg_zsdt0023_aux TYPE TABLE OF zsdt0023 WITH HEADER LINE,
      tg_notas_rfl    TYPE zde_nota_retorno_rfl_t,
      tg_mara         TYPE TABLE OF ty_mara,
      tg_notas        TYPE TABLE OF ty_notas WITH HEADER LINE,
      tg_zlest0039    TYPE TABLE OF zlest0039,
      tg_notas_aux    TYPE TABLE OF ty_notas WITH HEADER LINE,
      tg_doc_ret      TYPE TABLE OF j_1bnfdoc       WITH HEADER LINE,
      tg_zsdt_retlote TYPE TABLE OF zsdt_retlote    WITH HEADER LINE,
      tg_zsdt0168     TYPE TABLE OF zsdt0168        WITH HEADER LINE,
      tg_zsdt_export  TYPE TABLE OF zsdt_export     WITH HEADER LINE,
      tg_active       TYPE TABLE OF j_1bnfe_active  WITH HEADER LINE,
      tg_zfiwrt0008   TYPE TABLE OF zfiwrt0008      WITH HEADER LINE,
      t_zfiwrt0008    TYPE TABLE OF zfiwrt0008      WITH HEADER LINE,
      t_zfiwrt0009    TYPE TABLE OF zfiwrt0009      WITH HEADER LINE,
      tg_zsdt0283     TYPE TABLE OF zsdt0283,
      wg_zsdt0283     TYPE zsdt0283.


*&--------------------------------------------------------------------&*
*& SHDB                                                               &*
*&--------------------------------------------------------------------&*
DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata LIKE LINE OF ti_bdcdata,
      tl_bdc     TYPE TABLE OF bdcdata,
      wl_bdc     TYPE bdcdata,
      opt        TYPE ctu_params.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: vg_operacao TYPE c LENGTH 20,
      var_answer  TYPE c.

DATA: vl_status_line TYPE char1 VALUE 0.
DATA: vl_status_esto TYPE char1 VALUE 0.
DATA: it_row TYPE lvc_t_row.
*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_gerar_estorno TYPE c VALUE 'GERAR_ESTORNO'  LENGTH 50,
           c_gerar_sefaz   TYPE c VALUE 'GERAR_SEFAZ'    LENGTH 50,
           c_gerar_lcto    TYPE c VALUE 'GERAR_LCTO'     LENGTH 50,
           c_aut_docret    TYPE c VALUE 'AUT_DOCRET'     LENGTH 50,
           c_atualizar     TYPE c VALUE 'ATUALIZAR'      LENGTH 50,
           c_nfs_sel       TYPE c VALUE 'NFS_SEL'        LENGTH 50,
           c_atrib_qtd     TYPE c VALUE 'ATRIB_QTDE'     LENGTH 50,
           c_refresh_alv   TYPE c VALUE 'X'.


*----------------------------------------------------------------------*
* Tela de Seleção
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_bukrs TYPE t001-bukrs     OBLIGATORY,
            p_fkart TYPE zsdt0002-fkart DEFAULT 'ZRFL' MATCHCODE OBJECT zsdt0002,
            p_werks TYPE t001w-werks    OBLIGATORY,
            p_lgort TYPE t001l-lgort    DEFAULT 'ARMZ' OBLIGATORY,
            p_final TYPE zsdt_export-finalidade,
            p_safra TYPE mch1-charg     OBLIGATORY,
            p_kunnr TYPE kna1-kunnr     OBLIGATORY,
            p_termi TYPE lfa1-lifnr,
            p_matnr TYPE mara-matnr     OBLIGATORY.

SELECT-OPTIONS:
    p_dtrec FOR zlest0146-dt_recepcao NO-EXTENSION,
    p_dtem  FOR zlest0146-dt_recepcao NO-EXTENSION,
    p_docnum FOR j_1bnfdoc-docnum.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_ger RADIOBUTTON GROUP rb1 DEFAULT 'X',
            p_pen RADIOBUTTON GROUP rb1,
            p_amb RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b2.


AT SELECTION-SCREEN.


  IF p_ger = 'X'.
    IF p_final IS INITIAL.
      MESSAGE |Campo Finalidade é obrigatório!| TYPE 'E' DISPLAY LIKE 'S'.
      EXIT.
    ENDIF.
  ENDIF.

  IF p_fkart IS INITIAL.
    MESSAGE |Campo Tipo de Documento é obrigatório!| TYPE 'E' DISPLAY LIKE 'S'.
    EXIT.
  ENDIF.

  IF p_dtem IS INITIAL AND p_docnum IS INITIAL.
    MESSAGE |Campo Dt Emissão é obrigatório!| TYPE 'E' DISPLAY LIKE 'S'.
    EXIT.
  ENDIF.


START-OF-SELECTION.

  IF p_dtem IS INITIAL AND p_docnum IS INITIAL.
    MESSAGE |Campo Dt Emissão é obrigatório!| TYPE 'E' DISPLAY LIKE 'S'.

  ELSE.
    PERFORM: f_selecionar_dados USING '',
             f_processa_dados,
             f_call_alv.
  ENDIF.
