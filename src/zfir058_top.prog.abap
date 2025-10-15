*&---------------------------------------------------------------------*
*&  Include           ZFIR058_TOP
*&---------------------------------------------------------------------*


TABLES: bkpf, bsad, kna1, zsdt0041, bsik, t001, tgsb, vbak, bseg.

TYPE-POOLS: icon.

TYPES: BEGIN OF ty_cabecalho_0110,
         adt_dmbtr    TYPE bsid-dmbtr,
         sel_dmbtr    TYPE bsid-dmbtr,
         sld_dmbtr    TYPE bsid-dmbtr,
         adt_dmbtr2   TYPE bsid-dmbtr,
         sel_dmbtr2   TYPE bsid-dmbtr,
         sld_dmbtr2   TYPE bsid-dmbtr,

         adt_dmbe2    TYPE bsid-dmbe2,
         sel_dmbe2    TYPE bsid-dmbe2,
         sld_dmbe2    TYPE bsid-dmbe2,
         ds_cliente   TYPE char70,
         waers        TYPE bsid-waers,
         rsd_adt      TYPE bsid-dmbtr,
         sgtxt_rsd    TYPE bsid-sgtxt,
         manter_tp_dc TYPE c,
       END OF ty_cabecalho_0110.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES : BEGIN OF ty_display,
          bukrs TYPE bsik-bukrs,
          lifnr TYPE bsik-lifnr,
          bschl TYPE bsid-bschl,
          belnr TYPE bsik-belnr,
          buzei TYPE bsik-buzei,
          gjahr TYPE bsik-gjahr,
          bldat TYPE bsik-bldat,
          budat TYPE bsik-budat,
          zuonr TYPE bsik-zuonr,
          kidno TYPE bsid-kidno,
          xblnr TYPE bkpf-xblnr,
          ebeln TYPE bsik-ebeln,
          waers TYPE bsik-waers,
          wrbtr TYPE bsid-wrbtr,
          dmbtr TYPE bsik-dmbtr,
          dmbe2 TYPE bsik-dmbe2,
          zfbdt TYPE bsik-zfbdt,
          zbd1t TYPE bsik-zbd1t,
          sgtxt TYPE bsid-sgtxt,
        END OF ty_display.

TYPES: BEGIN OF ty_saida_0100,
         bukrs     TYPE bsid-bukrs,
         parid     TYPE kna1-kunnr,
         name1     TYPE kna1-name1,
         belnr     TYPE bsid-belnr,
         buzei     TYPE bsid-buzei,
         gjahr     TYPE bsid-gjahr,
         bldat     TYPE bsid-bldat,
         budat     TYPE bsid-budat,
         waers     TYPE bsid-waers,
         wrbtr     TYPE bsid-wrbtr,
         dmbtr     TYPE bsid-dmbtr,
         dmbe2     TYPE bsid-dmbe2,
         hkont     TYPE bsid-hkont,
         bschl     TYPE bsid-bschl,
         umsks     TYPE bsid-umsks,
         umskz     TYPE bsid-umskz,
         shkzg     TYPE bsid-shkzg,
         gsber     TYPE bsid-gsber,
         sgtxt     TYPE bsid-sgtxt,
         zfbdt     TYPE bsid-zfbdt,
         zbd1t     TYPE bsid-zbd1t,
         kidno     TYPE bsid-kidno,
         zuonr     TYPE bsid-zuonr,
         xblnr     TYPE bkpf-xblnr,
         blart     TYPE bsid-blart,
         ovped     TYPE bsid-vbel2,
         itmop     TYPE bsid-posn2,
         dcsim     TYPE zsdt0041-doc_simulacao,
         ebelp     TYPE ekpo-ebelp,
         posn2     TYPE bsid-posn2,
         spart     TYPE vbak-spart,
         zterm     TYPE bsid-zterm,
         anln1     TYPE bsid-anln1,
         anln2     TYPE bsid-anln2,
         dmbtr_au2 TYPE bsid-dmbtr,
         dmbtr_aux TYPE bsid-dmbtr,
         dmbe2_aux TYPE bsid-dmbe2,
         vtext     TYPE tspat-vtext,
         ft_dmbtr  TYPE bsid-dmbtr,
         ft_dmbe2  TYPE bsid-dmbe2,
         df_dmbe2  TYPE bsid-dmbe2,
         kursf     TYPE bkpf-kursf,
         qtde_ad   TYPE i,
         qtde_ft   TYPE i,
         st_comp   TYPE c,
         comp      TYPE char04,
         count     TYPE i,
         koart     TYPE bseg-koart,
         view_cp   TYPE char04,
         view_ad   TYPE char04,
         vlr_rsd   TYPE tslxx12, "BSID-DMBTR,
         sgtxt_rsd TYPE bsid-sgtxt,
         xref1     TYPE bsid-xref1,
         xref3     TYPE bsid-xref3,

       END OF ty_saida_0100,

       BEGIN OF ty_vbel,
         vbeln TYPE vbeln_nach,
         vbelv TYPE vbelv,
       END OF ty_vbel,

       BEGIN OF ty_saida_0110,
         check      TYPE c,
         bukrs      TYPE bsid-bukrs,
         parid      TYPE kna1-kunnr,
         name1      TYPE kna1-name1,
         belnr      TYPE bsid-belnr,
         buzei      TYPE bsid-buzei,
         gjahr      TYPE bsid-gjahr,
         bldat      TYPE bsid-bldat,
         budat      TYPE bsid-budat,
         waers      TYPE bsid-waers,
         wrbtr      TYPE bsid-wrbtr,
         dmbtr      TYPE bsid-dmbtr,
         dmbe2      TYPE bsid-dmbe2,
         hkont      TYPE bsid-hkont,
         bschl      TYPE bsid-bschl,
         umsks      TYPE bsid-umsks,
         umskz      TYPE bsid-umskz,
         shkzg      TYPE bsid-shkzg,
         gsber      TYPE bsid-gsber,
         sgtxt      TYPE bsid-sgtxt,
         zfbdt      TYPE bsid-zfbdt,
         zbd1t      TYPE bsid-zbd1t,
         kidno      TYPE bsid-kidno,
         zuonr      TYPE bsid-zuonr,
         xblnr      TYPE bkpf-xblnr,
         blart      TYPE bsid-blart,
         ovped      TYPE bsid-vbel2,
         itmop      TYPE bsid-posn2,
         dcsim      TYPE zsdt0041-doc_simulacao,
         ebelp      TYPE ekpo-ebelp,
         posn2      TYPE bsid-posn2,
         zterm      TYPE bsid-zterm,
         anln1      TYPE bsid-anln1,
         anln2      TYPE bsid-anln2,
         xref1      TYPE bsid-xref1,
         xref3      TYPE bsid-xref3,
         kursf      TYPE bkpf-kursf,
         dmbtr_au2  TYPE bsid-dmbtr,
         dmbtr_aux  TYPE bsid-dmbtr,
         dmbe2_aux  TYPE bsid-dmbe2,
         bl_desmemb TYPE bsid-belnr,
         bz_desmemb TYPE bsid-buzei,
         vlr_rsd    TYPE bsid-dmbtr,
         st_comp    TYPE c,
         vlr_rsdp   TYPE tslxx12,
         manual     TYPE c,
         ic_manual  TYPE char04,
         estilo     TYPE lvc_t_styl,
         koart      TYPE tbsl-koart,
         part_princ TYPE c,
         sgtxt_rsd  TYPE bsid-sgtxt,
       END OF ty_saida_0110.

TYPES: BEGIN OF ty_saida_0120,
         st_ctb TYPE c LENGTH 4. "St Contábil
         INCLUDE STRUCTURE zfit0139.
TYPES: END OF ty_saida_0120.

DATA: BEGIN OF tg_bsid_adt OCCURS 0,
        bukrs TYPE bsid-bukrs,
        kunnr TYPE bsid-kunnr,
        belnr TYPE bsid-belnr,
        buzei TYPE bsid-buzei,
        gjahr TYPE bsid-gjahr,
        bldat TYPE bsid-bldat,
        budat TYPE bsid-budat,
        waers TYPE bsid-waers,
        wrbtr TYPE bsid-wrbtr,
        dmbtr TYPE bsid-dmbtr,
        dmbe2 TYPE bsid-dmbe2,
        hkont TYPE bsid-hkont,
        bschl TYPE bsid-bschl,
        umsks TYPE bsid-umsks,
        umskz TYPE bsid-umskz,
        shkzg TYPE bsid-shkzg,
        gsber TYPE bsid-gsber,
        sgtxt TYPE bsid-sgtxt,
        zfbdt TYPE bsid-zfbdt,
        zbd1t TYPE bsid-zbd1t,
        kidno TYPE bsid-kidno,
        xref1 TYPE bsid-xref1,
        xref3 TYPE bsid-xref3,
        zuonr TYPE bsid-zuonr,
        xblnr TYPE bkpf-xblnr,
        blart TYPE bsid-blart,
        vbel2 TYPE bsid-vbel2,
        posn2 TYPE bsid-posn2,
        zterm TYPE bsid-zterm,
        anln1 TYPE bsid-anln1,
        anln2 TYPE bsid-anln2,
        dcsim TYPE zsdt0041-doc_simulacao,
      END OF tg_bsid_adt,

      BEGIN OF tg_bsik_adt OCCURS 0,
        bukrs   TYPE bsik-bukrs,
        lifnr   TYPE bsik-lifnr,
        belnr   TYPE bsik-belnr,
        buzei   TYPE bsik-buzei,
        gjahr   TYPE bsik-gjahr,
        bldat   TYPE bsik-bldat,
        budat   TYPE bsik-budat,
        waers   TYPE bsik-waers,
        wrbtr   TYPE bsik-wrbtr,
        dmbtr   TYPE bsik-dmbtr,
        dmbe2   TYPE bsik-dmbe2,
        hkont   TYPE bsik-hkont,
        bschl   TYPE bsik-bschl,
        umsks   TYPE bsik-umsks,
        umskz   TYPE bsik-umskz,
        shkzg   TYPE bsik-shkzg,
        gsber   TYPE bsik-gsber,
        sgtxt   TYPE bsik-sgtxt,
        zfbdt   TYPE bsik-zfbdt,
        zbd1t   TYPE bsik-zbd1t,
        kidno   TYPE bsik-kidno,
        xref1   TYPE bsid-xref1,
        xref3   TYPE bsid-xref3,
        zuonr   TYPE bsik-zuonr,
        xblnr   TYPE bkpf-xblnr,
        blart   TYPE bsik-blart,
        ebeln   TYPE bsik-ebeln,
        ebelp   TYPE bsik-ebelp,
        zterm   TYPE bsik-zterm,
        anln1   TYPE bsik-anln1,
        anln2   TYPE bsik-anln2,
        qtde_ad TYPE i,
      END OF tg_bsik_adt,

      BEGIN OF tg_bsik_copy OCCURS 0,
        bukrs   TYPE bsik-bukrs,
        lifnr   TYPE bsik-lifnr,
        belnr   TYPE bsik-belnr,
        buzei   TYPE bsik-buzei,
        gjahr   TYPE bsik-gjahr,
        bldat   TYPE bsik-bldat,
        budat   TYPE bsik-budat,
        waers   TYPE bsik-waers,
        wrbtr   TYPE bsik-wrbtr,
        dmbtr   TYPE bsik-dmbtr,
        dmbe2   TYPE bsik-dmbe2,
        hkont   TYPE bsik-hkont,
        bschl   TYPE bsik-bschl,
        umsks   TYPE bsik-umsks,
        umskz   TYPE bsik-umskz,
        shkzg   TYPE bsik-shkzg,
        gsber   TYPE bsik-gsber,
        sgtxt   TYPE bsik-sgtxt,
        zfbdt   TYPE bsik-zfbdt,
        zbd1t   TYPE bsik-zbd1t,
        kidno   TYPE bsik-kidno,
        xref1   TYPE bsid-xref1,
        xref3   TYPE bsid-xref3,
        zuonr   TYPE bsik-zuonr,
        xblnr   TYPE bkpf-xblnr,
        blart   TYPE bsik-blart,
        ebeln   TYPE bsik-ebeln,
        ebelp   TYPE bsik-ebelp,
        zterm   TYPE bsik-zterm,
        anln1   TYPE bsik-anln1,
        anln2   TYPE bsik-anln2,
        qtde_ad TYPE i,
      END OF tg_bsik_copy,

      BEGIN OF tg_bsik_prod OCCURS 0,
        bukrs   TYPE bsik-bukrs,
        lifnr   TYPE bsik-lifnr,
        bschl   TYPE bsid-bschl,
        blart   TYPE bsid-blart,
        shkzg   TYPE bsid-shkzg,
        gsber   TYPE bsid-gsber,
        zuonr   TYPE bsik-zuonr,
        waers   TYPE bsik-waers,
        xref3   TYPE bsid-xref3,
        xref1   TYPE bsid-xref1,
        hkont   TYPE bsid-hkont,
        sgtxt   TYPE bsid-sgtxt,
        wrbtr   TYPE bsik-wrbtr,
        dmbtr   TYPE bsik-dmbtr,
        dmbe2   TYPE bsik-dmbe2,
        qtde_ad TYPE i,
      END OF tg_bsik_prod,

      BEGIN OF tg_bsid_comp OCCURS 0,
        bukrs TYPE bsid-bukrs,
        kunnr TYPE bsid-kunnr,
        belnr TYPE bsid-belnr,
        buzei TYPE bsid-buzei,
        gjahr TYPE bsid-gjahr,
        bldat TYPE bsid-bldat,
        budat TYPE bsid-budat,
        waers TYPE bsid-waers,
        wrbtr TYPE bsid-wrbtr,
        dmbtr TYPE bsid-dmbtr,
        dmbe2 TYPE bsid-dmbe2,
        hkont TYPE bsid-hkont,
        bschl TYPE bsid-bschl,
        umsks TYPE bsid-umsks,
        umskz TYPE bsid-umskz,
        shkzg TYPE bsid-shkzg,
        gsber TYPE bsid-gsber,
        sgtxt TYPE bsid-sgtxt,
        zfbdt TYPE bsid-zfbdt,
        zbd1t TYPE bsid-zbd1t,
        kidno TYPE bsid-kidno,
        xref1 TYPE bsid-xref1,
        xref3 TYPE bsid-xref3,
        zuonr TYPE bsid-zuonr,
        xblnr TYPE bkpf-xblnr,
        blart TYPE bsid-blart,
        vbel2 TYPE bsid-vbel2,
        posn2 TYPE bsid-posn2,
        zterm TYPE bsid-zterm,
        anln1 TYPE bsid-anln1,
        anln2 TYPE bsid-anln2,
        dcsim TYPE zsdt0041-doc_simulacao,
      END OF tg_bsid_comp,

      BEGIN OF tg_bsik_comp OCCURS 0,
        bukrs TYPE bsik-bukrs,
        lifnr TYPE bsik-lifnr,
        belnr TYPE bsik-belnr,
        buzei TYPE bsik-buzei,
        gjahr TYPE bsik-gjahr,
        bldat TYPE bsik-bldat,
        budat TYPE bsik-budat,
        waers TYPE bsik-waers,
        wrbtr TYPE bsik-wrbtr,
        dmbtr TYPE bsik-dmbtr,
        dmbe2 TYPE bsik-dmbe2,
        hkont TYPE bsik-hkont,
        bschl TYPE bsik-bschl,
        umsks TYPE bsik-umsks,
        umskz TYPE bsik-umskz,
        shkzg TYPE bsik-shkzg,
        gsber TYPE bsik-gsber,
        sgtxt TYPE bsik-sgtxt,
        zfbdt TYPE bsik-zfbdt,
        zbd1t TYPE bsik-zbd1t,
        kidno TYPE bsik-kidno,
        xref1 TYPE bsid-xref1,
        xref3 TYPE bsid-xref3,
        zuonr TYPE bsik-zuonr,
        xblnr TYPE bkpf-xblnr,
        blart TYPE bsik-blart,
        ebeln TYPE bsik-ebeln,
        ebelp TYPE bsik-ebelp,
        zterm TYPE bsik-zterm,
        anln1 TYPE bsik-anln1,
        anln2 TYPE bsik-anln2,
      END OF tg_bsik_comp,

      BEGIN OF tg_kna1 OCCURS 0,
        kunnr TYPE kna1-kunnr,
        name1 TYPE kna1-name1,
      END OF tg_kna1,

      BEGIN OF tg_lfa1 OCCURS 0,
        lifnr TYPE lfa1-lifnr,
        name1 TYPE lfa1-name1,
      END OF tg_lfa1,

      BEGIN OF tg_t001 OCCURS 0,
        bukrs  TYPE t001-bukrs,
        waers  TYPE t001-waers,
        waers2 TYPE x001-hwae2,
      END OF tg_t001,

      BEGIN OF tg_bkpf OCCURS 0,
        bukrs TYPE bkpf-bukrs,
        belnr TYPE bkpf-belnr,
        gjahr TYPE bkpf-gjahr,
        kursf TYPE bkpf-kursf,
        kurs2 TYPE bkpf-kurs2,
        waers TYPE bkpf-waers,
        xblnr TYPE bkpf-xblnr,
      END OF tg_bkpf,

      BEGIN OF tg_vbak OCCURS 0,
        vbeln TYPE vbak-vbeln,
        spart TYPE vbak-spart,
      END OF tg_vbak,

      BEGIN OF tg_ekko OCCURS 0,
        ebeln TYPE ekko-ebeln,
        vsart TYPE ekko-vsart,
      END OF tg_ekko,

      BEGIN OF tg_bsis_cbanco OCCURS 0,
        bukrs LIKE bsis-bukrs,
        belnr LIKE bsis-belnr,
        gjahr LIKE bsis-gjahr,
        hkont LIKE bsis-hkont,
        dmbtr TYPE bsis-dmbtr,
        dmbe2 TYPE bsis-dmbe2,
        waers TYPE bsis-waers,
        wrbtr TYPE bsis-wrbtr,
        fdlev TYPE skb1-fdlev,
      END OF tg_bsis_cbanco,

      BEGIN OF tg_bseg OCCURS 0,
        bukrs TYPE bseg-bukrs,
        belnr TYPE bseg-belnr,
        anln1 TYPE bseg-anln1,
        anln2 TYPE bseg-anln2,
        ebeln TYPE bseg-ebeln,
        ebelp TYPE bseg-ebelp,
        vbel2 TYPE bseg-vbel2,
        posn2 TYPE bseg-posn2,
        zbd1t TYPE bseg-zbd1t, " PBI - 70679
      END OF tg_bseg,

      BEGIN OF tg_ekkn OCCURS 0,
        ebeln TYPE ekkn-ebeln,
        ebelp TYPE ekkn-ebelp,
        sakto TYPE ekkn-sakto,
        anln1 TYPE ekkn-anln1,
        anln2 TYPE ekkn-anln2,
      END OF tg_ekkn,

      BEGIN OF tg_ekpo_aux OCCURS 0,
        ebeln TYPE ekpo-ebeln,
        ebelp TYPE ekpo-ebelp,
      END OF tg_ekpo_aux,

      BEGIN OF tg_zsdt0041 OCCURS 0,
        vbeln         TYPE zsdt0041-vbeln,
        doc_simulacao TYPE zsdt0041-doc_simulacao,
      END OF tg_zsdt0041,

      BEGIN OF tg_zsdt0090 OCCURS 0,
        vbeln         TYPE zsdt0090-vbeln,
        doc_simulacao TYPE zsdt0090-doc_simulacao,
        vbelv         TYPE zsdt0090-vbelv,
      END OF tg_zsdt0090,

      BEGIN OF tg_zsdt0090_aux OCCURS 0,
        vbeln         TYPE zsdt0090-vbeln,
        doc_simulacao TYPE zsdt0090-doc_simulacao,
        vbelv         TYPE zsdt0090-vbelv,
      END OF tg_zsdt0090_aux,

      BEGIN OF tg_tspat OCCURS 0.
        INCLUDE STRUCTURE tspat.
DATA: END OF tg_tspat.

DATA: BEGIN OF tg_vbfa_rd OCCURS 0,
        vbeln TYPE  vbfa-vbeln,
        vbelv TYPE  vbfa-vbelv.
DATA: END OF tg_vbfa_rd.

*CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
*RANGES: r_xref3    FOR bsik-xref3.
*CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

DATA : gr_table TYPE REF TO cl_salv_table.
DATA : it_display TYPE TABLE OF ty_display,
       wa_display TYPE ty_display.

DATA: vl_error    TYPE c, "PSA
      vl_comp     TYPE c,
      vl_doc_comp TYPE bsad-belnr.
*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

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

CLASS lcl_alv_toolbar_0110 DEFINITION.
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


CLASS lcl_event_handler_0110 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

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


CLASS lcl_event_handler_0100 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION


CLASS lcl_event_handler_0120 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

DATA: obj_alv_0100       TYPE REF TO cl_gui_alv_grid,
      obj_container_0100 TYPE REF TO cl_gui_custom_container,
      obj_alv_0110       TYPE REF TO cl_gui_alv_grid,
      obj_container_0110 TYPE REF TO cl_gui_custom_container,
      obj_alv_0120       TYPE REF TO cl_gui_alv_grid,
      obj_container_0120 TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: obj_toolbar_0100 TYPE REF TO lcl_alv_toolbar_0100,
      obj_toolbar_0110 TYPE REF TO lcl_alv_toolbar_0110,
      obj_toolbar_0120 TYPE REF TO lcl_alv_toolbar_0120.

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

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100     TYPE TABLE OF ty_saida_0100,
      wa_saida_0100     TYPE ty_saida_0100,
      it_saida_0110     TYPE TABLE OF ty_saida_0110,
      wa_saida_0110     TYPE ty_saida_0110,
      it_saida_0120     TYPE TABLE OF ty_saida_0120,
      wa_saida_0120     TYPE ty_saida_0120,
      wa_cabecalho_0110 TYPE ty_cabecalho_0110,
      tg_zib_err        TYPE TABLE OF zib_contabil_err,
      wa_bseg           TYPE bseg,
      it_vbel           TYPE TABLE OF ty_vbel,
      it_vbelx          TYPE TABLE OF ty_vbel,
      it_vbel2          TYPE TABLE OF ty_vbel,


      tg_bsid_adt_aux   LIKE TABLE OF tg_bsid_adt  WITH HEADER LINE,
      tg_bsik_adt_aux   LIKE TABLE OF tg_bsik_adt  WITH HEADER LINE,
      tg_bsik_comp_aux  LIKE TABLE OF tg_bsik_comp WITH HEADER LINE,
      tg_bsik_comp_aux2 LIKE TABLE OF tg_bsik_comp WITH HEADER LINE,
      tg_bsid_comp_aux  LIKE TABLE OF tg_bsid_comp WITH HEADER LINE,
      tg_bsid_comp_aux2 LIKE TABLE OF tg_bsid_comp WITH HEADER LINE.

*-------------------------------------------------------------------*
* Parâmetros
*-------------------------------------------------------------------*
RANGES:  r_bukrs     FOR bsid-bukrs,  "Empresa
         r_parid     FOR bsid-kunnr,  "Cliente/Fornecedor
         r_ovped     FOR bsid-vbel2,  "Ordem Venda/Pedido
         r_augdt     FOR bsad-augdt,  "Data Compensação
         r_sgtxt     FOR bsad-sgtxt,  "Texto Contábil
         r_zuonr     FOR bsad-zuonr,  "Atribuição
         r_dcsim     FOR zsdt0041-doc_simulacao, "Simulador
         r_vbeln     FOR zsdt0090-vbeln,

         r_umsks_p   FOR bsad-umsks,
         r_umskz_p   FOR bsad-umskz,
         r_shkzg_p   FOR bsad-shkzg,
         r_umsks_c   FOR bsad-umsks,
         r_umskz_c   FOR bsad-umskz.

*-------------------------------------------------------------------*
* Ranges
*-------------------------------------------------------------------*

RANGES: r_fdlev_banco FOR skb1-fdlev.

*-------------------------------------------------------------------*
* Variaveis
*-------------------------------------------------------------------*

DATA: vg_not_found  TYPE c,
      vg_opr_lcto   TYPE c LENGTH 10,
      vg_cancel     TYPE c,
      var_answer    TYPE c,
      p_file        TYPE rlgrap-filename,
      vg_id_log     TYPE i,
      vg_message    TYPE zglt087-message,
      vg_parausd(1).

DATA: lva_bseg-zlspr TYPE   bseg-zlspr, "bloqueio pagto
      lva_bseg-zlsch TYPE   bseg-zlsch, "forma pagto
      lva_bseg-hbkid TYPE   bseg-hbkid, "banco empresa
      lva_bseg-bvtyp TYPE   bseg-bvtyp, "tp bco parceiro
      lva_bseg-zfbdt TYPE   bseg-zfbdt. "Data Base

DATA: tl_parametros TYPE ustyp_t_parameters,
      wl_parametros TYPE ustyp_parameters.

DATA: _automation TYPE bool,
      _step       TYPE string.

*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_display TYPE c VALUE 'DISPLAY' LENGTH 7,
           c_edit    TYPE c VALUE 'EDIT'    LENGTH 7,
           c_new     TYPE c VALUE 'NEW'     LENGTH 7.


SELECTION-SCREEN: BEGIN OF SCREEN 0001 AS SUBSCREEN.
  SELECTION-SCREEN: BEGIN OF BLOCK b1 ."WITH FRAME  TITLE TEXT-001.

    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: rb_cli   RADIOBUTTON GROUP rb1 USER-COMMAND act.
      SELECTION-SCREEN COMMENT 3(08)  TEXT-i16 FOR FIELD rb_cli.
      PARAMETERS: rb_forn  RADIOBUTTON GROUP rb1.
      SELECTION-SCREEN COMMENT 14(10) TEXT-i17 FOR FIELD rb_forn.
*PARAMETERS: rb_prod  RADIOBUTTON GROUP rb1.
*SELECTION-SCREEN COMMENT 26(10) text-i29 FOR FIELD rb_prod. * PBI - 70679 - Inicio - CBRAND
      PARAMETERS: ck_massa   LIKE bsid-umskz AS CHECKBOX  DEFAULT ' ' MODIF ID b.
      SELECTION-SCREEN COMMENT 70(30) TEXT-i30 FOR FIELD ck_massa.
    SELECTION-SCREEN END OF LINE.

    SELECT-OPTIONS: p_bukrs FOR t001-bukrs NO INTERVALS OBLIGATORY,
*               p_gsber FOR tgsb-gsber,
                    p_parid FOR bsad-kunnr,
                    p_ovped FOR bsad-vbel2,
                    p_sgtxt FOR bsad-sgtxt NO INTERVALS NO-EXTENSION NO-DISPLAY,
                    p_zuonr FOR bsad-zuonr NO INTERVALS,
*               p_belnr FOR bsad-belnr NO INTERVALS,
                    "p_dcsim FOR zsdt0041-doc_simulacao NO INTERVALS, "NO INTERVALS NO-EXTENSION, "PBI - 79679
                    p_dcsim FOR vbak-vbeln NO INTERVALS, "PBI - 79679
                    p_spart FOR vbak-spart NO INTERVALS.
*               p_safra FOR bsik-xref1 NO INTERVALS NO-EXTENSION,
*               p_produ FOR bsik-xref3 NO INTERVALS NO-EXTENSION.

    SELECTION-SCREEN BEGIN OF LINE.

      SELECTION-SCREEN COMMENT 1(31) TEXT-i18. "Unificação Partidas

      PARAMETERS: p_ovpedj AS CHECKBOX DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 35(10)  TEXT-i20 FOR FIELD p_ovpedj.
      PARAMETERS: p_sgtxtj AS CHECKBOX ." DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 48(11) TEXT-i11 FOR FIELD p_sgtxtj.
      PARAMETERS: p_zuonrj AS CHECKBOX MODIF ID a.
      SELECTION-SCREEN COMMENT 62(10) TEXT-i12 FOR FIELD p_zuonrj .
      PARAMETERS: p_dcsimj AS CHECKBOX ." DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 74(12) TEXT-i28 FOR FIELD p_dcsimj.

    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF LINE.

      SELECTION-SCREEN COMMENT 1(31) TEXT-i19.   "Tipo Partida

      PARAMETERS: p_cta_rz AS CHECKBOX ." DDEFAULT 'X'.
      SELECTION-SCREEN COMMENT 35(10)  TEXT-i22 FOR FIELD p_cta_rz.
      PARAMETERS: p_cta_nr AS CHECKBOX ." DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 48(13) TEXT-i23 FOR FIELD p_cta_nr.

    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME  TITLE TEXT-i24. "Compensação
      SELECTION-SCREEN BEGIN OF LINE.
        SELECTION-SCREEN COMMENT 1(4) TEXT-i04 FOR FIELD p_augdt.
        SELECT-OPTIONS: p_augdt FOR bsad-augdt NO INTERVALS NO-EXTENSION DEFAULT sy-datum OBLIGATORY.

        SELECTION-SCREEN COMMENT 23(15) TEXT-i13 FOR FIELD p_blt_cp.
        PARAMETERS: p_blt_cp TYPE c LENGTH 2 DEFAULT 'AB' OBLIGATORY.

        SELECTION-SCREEN COMMENT 42(02) TEXT-i21. "Espaço

*PARAMETERS: P_CPLIB  AS CHECKBOX.
*SELECTION-SCREEN COMMENT 46(09) TEXT-I25 FOR FIELD P_CPLIB.
*
*SELECTION-SCREEN COMMENT 56(05) TEXT-I27 FOR FIELD P_KSFCP.
*PARAMETERS: P_KSFCP TYPE BKPF-KURSF.

        SELECTION-SCREEN COMMENT 46(05) TEXT-i27 FOR FIELD p_ksfcp.
        PARAMETERS: p_ksfcp TYPE bkpf-kursf.

        PARAMETERS: p_cplib  AS CHECKBOX.
        SELECTION-SCREEN COMMENT 77(09) TEXT-i25 FOR FIELD p_cplib.

      SELECTION-SCREEN END OF LINE.
    SELECTION-SCREEN: END OF BLOCK b4.

  SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: END OF SCREEN 0001.

INITIALIZATION.
  gs_variant-report      = sy-repid.

AT SELECTION-SCREEN OUTPUT.

  IF ck_massa = 'X'. "AND rb_prod = 'X'. PBI - 70679
    REFRESH  p_zuonr.
    SELECT *
      FROM zfit0155
      INTO TABLE @DATA(it_zfit0155)
      WHERE bukrs IN @p_bukrs
      AND   EXISTS ( SELECT * FROM bsik WHERE bsik~bukrs = zfit0155~bukrs AND bsik~belnr = zfit0155~belnr AND bsik~gjahr = zfit0155~gjahr AND bsik~zuonr = zfit0155~lote AND bsik~blart IN ('MA','MB')  ).

*    IF p_bukrs-low IS NOT INITIAL.
*      DELETE it_zfit0155 WHERE bukrs <> p_bukrs-low.
*    ENDIF.

    SORT it_zfit0155 BY lote.
    DELETE ADJACENT DUPLICATES FROM it_zfit0155 COMPARING lote.
    LOOP AT  it_zfit0155 INTO DATA(wa_zfit0155).
      p_zuonr-sign   = 'I'.
      p_zuonr-option = 'EQ'.
      p_zuonr-low = wa_zfit0155-lote.
      APPEND  p_zuonr.
    ENDLOOP.

*    IF p_bukrs-low IS INITIAL.
*      REFRESH p_bukrs.
*      p_bukrs-sign    = 'I'.
*      p_bukrs-option  = 'EQ'.
*      p_bukrs-low     = '0001'.
*      APPEND p_bukrs.
*    ENDIF.
  ELSEIF ck_massa = 'X'. "AND rb_prod = 'X'. PBI - 70679
*    REFRESH  p_zuonr.
  ENDIF.
  LOOP AT SCREEN.
* PBI - 70679 - Inicio -  CBRAND
*    IF rb_prod = 'X'.
*      IF screen-group1 = 'A'.
*        p_zuonrj = 'X'.
*        CLEAR: p_ovpedj, p_sgtxtj,p_dcsimj.
*      ENDIF.
*      IF screen-group1 = 'B'.
*        screen-active = 1.
*      ENDIF .
*    ELSE.
* PBI - 70679 - Fim - CBRAND
    IF screen-group1 = 'A'.
*        CLEAR p_zuonrj.
    ENDIF.
    IF screen-group1 = 'B'.
      screen-active = 0.
    ENDIF .
*    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

IF sy-tcode = 'ZFI00109'. "Automação

    PERFORM f_selecionar_dados.
    PERFORM: f_processa_dados.

    SORT it_saida_0100 BY comp ASCENDING.

    DATA vtotal_fatura TYPE bsid-dmbtr.

    "Adto X Fatura iguais
    DELETE it_saida_0100 WHERE waers NE 'BRL'.
    DELETE it_saida_0100 WHERE ft_dmbtr = 0.
    LOOP AT it_saida_0100 INTO wa_saida_0100.
      CASE wa_saida_0100-st_comp.
        WHEN '1' OR '3' . "Liberado Compensação ( Total Adiamento = Total Partidas Compensar )
          PERFORM f_get_part_comp USING wa_saida_0100.
          PERFORM f_bapi_f51 USING abap_false
                          CHANGING wa_saida_0100
                                   vl_error
                                   vl_doc_comp.
          "perform f_renovar_cons.
        WHEN '2'. "Selecionar Partidas para compensar
          IF wa_saida_0100-df_dmbe2 NE 0 .
            PERFORM f_get_part_comp USING wa_saida_0100.
            IF it_saida_0110[] IS NOT INITIAL.
              CLEAR vtotal_fatura.
              SORT it_saida_0110 BY wrbtr.
              LOOP AT it_saida_0110 ASSIGNING FIELD-SYMBOL(<_saida_0110>).
                IF ( <_saida_0110>-shkzg NE wa_saida_0100-shkzg ).
                  ADD <_saida_0110>-wrbtr TO vtotal_fatura.
                ELSE.
                  SUBTRACT <_saida_0110>-wrbtr FROM vtotal_fatura.
                ENDIF.
                IF wa_saida_0100-df_dmbe2 LT 0 .
                  <_saida_0110>-check = 'X'.
                ELSEIF vtotal_fatura LT wa_saida_0100-wrbtr.
                  <_saida_0110>-check = 'X'.
                ELSE.
                  <_saida_0110>-vlr_rsd = vtotal_fatura - wa_saida_0100-wrbtr.
                  IF <_saida_0110>-vlr_rsd LT 0.
                    CLEAR <_saida_0110>-vlr_rsd.
                    <_saida_0110>-check = 'X'.
                  ELSE.
                    <_saida_0110>-check = 'X'.
                    EXIT.
                  ENDIF.
                ENDIF.
              ENDLOOP.
              IF wa_saida_0100-df_dmbe2 LT 0 .
                wa_cabecalho_0110-rsd_adt = wa_saida_0100-wrbtr - vtotal_fatura.
                IF wa_cabecalho_0110-rsd_adt LT 0.
                  CONTINUE.
                ENDIF.
                _step = 'Menor'.
              ELSE.
                CLEAR wa_cabecalho_0110-rsd_adt .
                _step = 'Maior'.
              ENDIF.

              PERFORM f_compensar_adt USING wa_saida_0100.
              CLEAR _step.
            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ELSE."Processo Normal
    CALL SCREEN 0100.
  ENDIF.
