*&---------------------------------------------------------------------*
*&  Include           ZFIR058_TOP
*&---------------------------------------------------------------------*


TABLES: bkpf, bsad, kna1, zsdt0041, bsik, t001, tgsb.

TYPE-POOLS: icon.

TYPES: BEGIN OF ty_cabecalho_0110,
         adt_dmbtr    TYPE bsid_view-dmbtr,
         sel_dmbtr    TYPE bsid_view-dmbtr,
         sld_dmbtr    TYPE bsid_view-dmbtr,
         adt_dmbe2    TYPE bsid_view-dmbe2,
         sel_dmbe2    TYPE bsid_view-dmbe2,
         sld_dmbe2    TYPE bsid_view-dmbe2,
         ds_cliente   TYPE char70,
         waers        TYPE bsid_view-waers,
         rsd_adt      TYPE bsid_view-dmbtr,
         sgtxt_rsd    TYPE bsid_view-sgtxt,
         manter_tp_dc TYPE c,
       END OF ty_cabecalho_0110.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES : BEGIN OF ty_display,
          bukrs TYPE bsik_view-bukrs,
          lifnr TYPE bsik_view-lifnr,
          bschl TYPE bsid_view-bschl,
          belnr TYPE bsik_view-belnr,
          buzei TYPE bsik_view-buzei,
          gjahr TYPE bsik_view-gjahr,
          bldat TYPE bsik_view-bldat,
          budat TYPE bsik_view-budat,
          zuonr TYPE bsik_view-zuonr,
          kidno TYPE bsid_view-kidno,
          xblnr TYPE bkpf-xblnr,
          ebeln TYPE bsik-ebeln,
          waers TYPE bsik-waers,
          wrbtr TYPE bsid_view-wrbtr,
          dmbtr TYPE bsik_view-dmbtr,
          dmbe2 TYPE bsik_view-dmbe2,
          zfbdt TYPE bsik_view-zfbdt,
          zbd1t TYPE bsik_view-zbd1t,
          xref1     TYPE bsid_view-xref1,
          xref3     TYPE bsid_view-xref3,
          sgtxt TYPE bsid_view-sgtxt,
        END OF ty_display.

TYPES: BEGIN OF ty_saida_0100,
         bukrs     TYPE bsid_view-bukrs,
         parid     TYPE kna1-kunnr,
         name1     TYPE kna1-name1,
         belnr     TYPE bsid_view-belnr,
         buzei     TYPE bsid_view-buzei,
         gjahr     TYPE bsid_view-gjahr,
         bldat     TYPE bsid_view-bldat,
         budat     TYPE bsid_view-budat,
         waers     TYPE bsid_view-waers,
         wrbtr     TYPE bsid_view-wrbtr,
         dmbtr     TYPE bsid_view-dmbtr,
         dmbe2     TYPE bsid_view-dmbe2,
         hkont     TYPE bsid_view-hkont,
         bschl     TYPE bsid_view-bschl,
         umsks     TYPE bsid_view-umsks,
         umskz     TYPE bsid_view-umskz,
         shkzg     TYPE bsid_view-shkzg,
         gsber     TYPE bsid_view-gsber,
         sgtxt     TYPE bsid_view-sgtxt,
         zfbdt     TYPE bsid_view-zfbdt,
         zbd1t     TYPE bsid_view-zbd1t,
         kidno     TYPE bsid_view-kidno,
         zuonr     TYPE bsid_view-zuonr,
         xblnr     TYPE bkpf-xblnr,
         blart     TYPE bsid_view-blart,
         ovped     TYPE bsid_view-vbel2,
         itmop     TYPE bsid_view-posn2,
         dcsim     TYPE zsdt0041-doc_simulacao,
         ebelp     TYPE ekpo-ebelp,
         posn2     TYPE bsid_view-posn2,
         spart     TYPE vbak-spart,
         zterm     TYPE bsid_view-zterm,
         anln1     TYPE bsid_view-anln1,
         anln2     TYPE bsid_view-anln2,
         dmbtr_aux TYPE bsid_view-dmbtr,
         dmbe2_aux TYPE bsid_view-dmbe2,
         vtext     TYPE tspat-vtext,
         ft_dmbtr  TYPE bsid_view-dmbtr,
         ft_dmbe2  TYPE bsid_view-dmbe2,
         df_dmbe2  TYPE bsid_view-dmbe2,
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
         sgtxt_rsd TYPE bsid_view-sgtxt,
         xref1     TYPE bsid_view-xref1,
         xref3     TYPE bsid_view-xref3,
       END OF ty_saida_0100,

       BEGIN OF ty_saida_0110,
         check      TYPE c,
         bukrs      TYPE bsid_view-bukrs,
         parid      TYPE kna1-kunnr,
         name1      TYPE kna1-name1,
         belnr      TYPE bsid_view-belnr,
         buzei      TYPE bsid_view-buzei,
         gjahr      TYPE bsid_view-gjahr,
         bldat      TYPE bsid_view-bldat,
         budat      TYPE bsid_view-budat,
         waers      TYPE bsid_view-waers,
         wrbtr      TYPE bsid_view-wrbtr,
         dmbtr      TYPE bsid_view-dmbtr,
         dmbe2      TYPE bsid_view-dmbe2,
         hkont      TYPE bsid_view-hkont,
         bschl      TYPE bsid_view-bschl,
         umsks      TYPE bsid_view-umsks,
         umskz      TYPE bsid_view-umskz,
         shkzg      TYPE bsid_view-shkzg,
         gsber      TYPE bsid_view-gsber,
         sgtxt      TYPE bsid_view-sgtxt,
         zfbdt      TYPE bsid_view-zfbdt,
         zbd1t      TYPE bsid_view-zbd1t,
         kidno      TYPE bsid_view-kidno,
         zuonr      TYPE bsid_view-zuonr,
         xblnr      TYPE bkpf-xblnr,
         blart      TYPE bsid_view-blart,
         ovped      TYPE bsid_view-vbel2,
         itmop      TYPE bsid_view-posn2,
         dcsim      TYPE zsdt0041-doc_simulacao,
         ebelp      TYPE ekpo-ebelp,
         posn2      TYPE bsid_view-posn2,
         zterm      TYPE bsid_view-zterm,
         anln1      TYPE bsid_view-anln1,
         anln2      TYPE bsid_view-anln2,
         xref1      TYPE bsid_view-xref1,
         xref3      TYPE bsid_view-xref3,
         kursf      TYPE bkpf-kursf,
         dmbtr_aux  TYPE bsid_view-dmbtr,
         dmbe2_aux  TYPE bsid_view-dmbe2,
         bl_desmemb TYPE bsid_view-belnr,
         bz_desmemb TYPE bsid_view-buzei,
         vlr_rsd    TYPE bsid_view-dmbtr,
         st_comp    TYPE c,
         vlr_rsdp   TYPE tslxx12,
         manual     TYPE c,
         ic_manual  TYPE char04,
         estilo     TYPE lvc_t_styl,
         koart      TYPE tbsl-koart,
         part_princ TYPE c,
         sgtxt_rsd  TYPE bsid_view-sgtxt,
       END OF ty_saida_0110.

TYPES: BEGIN OF ty_saida_0120,
         st_ctb TYPE c LENGTH 4. "St Contábil
         INCLUDE STRUCTURE zfit0139.
       TYPES: END OF ty_saida_0120.

DATA: BEGIN OF tg_bsid_adt OCCURS 0,
        bukrs TYPE bsid_view-bukrs,
        kunnr TYPE bsid_view-kunnr,
        belnr TYPE bsid_view-belnr,
        buzei TYPE bsid_view-buzei,
        gjahr TYPE bsid_view-gjahr,
        bldat TYPE bsid_view-bldat,
        budat TYPE bsid_view-budat,
        waers TYPE bsid_view-waers,
        wrbtr TYPE bsid_view-wrbtr,
        dmbtr TYPE bsid_view-dmbtr,
        dmbe2 TYPE bsid_view-dmbe2,
        hkont TYPE bsid_view-hkont,
        bschl TYPE bsid_view-bschl,
        umsks TYPE bsid_view-umsks,
        umskz TYPE bsid_view-umskz,
        shkzg TYPE bsid_view-shkzg,
        gsber TYPE bsid_view-gsber,
        sgtxt TYPE bsid_view-sgtxt,
        zfbdt TYPE bsid_view-zfbdt,
        zbd1t TYPE bsid_view-zbd1t,
        kidno TYPE bsid_view-kidno,
        xref1 TYPE bsid_view-xref1,
        xref3 TYPE bsid_view-xref3,
        zuonr TYPE bsid_view-zuonr,
        xblnr TYPE bkpf-xblnr,
        blart TYPE bsid_view-blart,
        vbel2 TYPE bsid_view-vbel2,
        posn2 TYPE bsid_view-posn2,
        zterm TYPE bsid_view-zterm,
        anln1 TYPE bsid_view-anln1,
        anln2 TYPE bsid_view-anln2,
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
        xref1   TYPE bsid_view-xref1,
        xref3   TYPE bsid_view-xref3,
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
        xref1   TYPE bsid_view-xref1,
        xref3   TYPE bsid_view-xref3,
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
        bschl   TYPE bsid_view-bschl,
        blart   TYPE bsid_view-blart,
        shkzg   TYPE bsid_view-shkzg,
        gsber   TYPE bsid_view-gsber,
        zuonr   TYPE bsik-zuonr,
        waers   TYPE bsik-waers,
        xref3   TYPE bsid_view-xref3,
        xref1   TYPE bsid_view-xref1,
        hkont   TYPE bsid_view-hkont,
        sgtxt   TYPE bsid_view-sgtxt,
        wrbtr   TYPE bsik-wrbtr,
        dmbtr   TYPE bsik-dmbtr,
        dmbe2   TYPE bsik-dmbe2,
        qtde_ad TYPE i,
      END OF tg_bsik_prod,

      BEGIN OF tg_bsid_comp OCCURS 0,
        bukrs TYPE bsid_view-bukrs,
        kunnr TYPE bsid_view-kunnr,
        belnr TYPE bsid_view-belnr,
        buzei TYPE bsid_view-buzei,
        gjahr TYPE bsid_view-gjahr,
        bldat TYPE bsid_view-bldat,
        budat TYPE bsid_view-budat,
        waers TYPE bsid_view-waers,
        wrbtr TYPE bsid_view-wrbtr,
        dmbtr TYPE bsid_view-dmbtr,
        dmbe2 TYPE bsid_view-dmbe2,
        hkont TYPE bsid_view-hkont,
        bschl TYPE bsid_view-bschl,
        umsks TYPE bsid_view-umsks,
        umskz TYPE bsid_view-umskz,
        shkzg TYPE bsid_view-shkzg,
        gsber TYPE bsid_view-gsber,
        sgtxt TYPE bsid_view-sgtxt,
        zfbdt TYPE bsid_view-zfbdt,
        zbd1t TYPE bsid_view-zbd1t,
        kidno TYPE bsid_view-kidno,
        xref1 TYPE bsid_view-xref1,
        xref3 TYPE bsid_view-xref3,
        zuonr TYPE bsid_view-zuonr,
        xblnr TYPE bkpf-xblnr,
        blart TYPE bsid_view-blart,
        vbel2 TYPE bsid_view-vbel2,
        posn2 TYPE bsid_view-posn2,
        zterm TYPE bsid_view-zterm,
        anln1 TYPE bsid_view-anln1,
        anln2 TYPE bsid_view-anln2,
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
        xref1 TYPE bsid_view-xref1,
        xref3 TYPE bsid_view-xref3,
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
        bukrs type bsis_view-bukrs,
        belnr type bsis_view-belnr,
        gjahr type bsis_view-gjahr,
        hkont type bsis_view-hkont,
        dmbtr TYPE bsis_view-dmbtr,
        dmbe2 TYPE bsis_view-dmbe2,
        waers TYPE bsis_view-waers,
        wrbtr TYPE bsis_view-wrbtr,
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
      END OF tg_zsdt0090,

      BEGIN OF tg_tspat OCCURS 0.
        INCLUDE STRUCTURE tspat.
      DATA: END OF tg_tspat.

DATA: BEGIN OF tg_vbfa_rd OCCURS 0,
        vbeln TYPE  vbfa-vbeln,
        vbelv TYPE  vbfa-vbelv.
DATA: END OF tg_vbfa_rd.

*CS2020001116 - Jaime Tassoni - 09.11.2020 - inicio
RANGES: r_xref3    FOR bsik-xref3.
*CS2020001116 - Jaime Tassoni - 09.11.2020 - fim

DATA : gr_table TYPE REF TO cl_salv_table.
DATA : it_display TYPE TABLE OF ty_display,
       wa_display TYPE ty_display.
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

DATA: vg_not_found TYPE c,
      vg_opr_lcto  TYPE c LENGTH 10,
      vg_cancel    TYPE c,
      var_answer   TYPE c,
      p_file       TYPE rlgrap-filename,
      vg_id_log    TYPE i,
      vg_message   TYPE zglt087-message.

*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_display TYPE c VALUE 'DISPLAY' LENGTH 7,
           c_edit    TYPE c VALUE 'EDIT'    LENGTH 7,
           c_new     TYPE c VALUE 'NEW'     LENGTH 7.


SELECTION-SCREEN: BEGIN OF SCREEN 0001 AS SUBSCREEN.
SELECTION-SCREEN: BEGIN OF BLOCK b1 ."WITH FRAME  TITLE TEXT-001.



SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: rb_prod  RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND act MODIF ID a.
SELECTION-SCREEN COMMENT 3(10) text-i29 FOR FIELD rb_prod.
PARAMETERS: ck_massa   LIKE bsid-umskz AS CHECKBOX  DEFAULT ' '. "
SELECTION-SCREEN COMMENT 70(30) text-i30 FOR FIELD ck_massa.
PARAMETERS: rb_cli   RADIOBUTTON GROUP rb1   MODIF ID c.
*SELECTION-SCREEN COMMENT 3(08)  text-i16 FOR FIELD rb_cli.
PARAMETERS: rb_forn  RADIOBUTTON GROUP rb1 MODIF ID c.
*SELECTION-SCREEN COMMENT 14(10) text-i17 FOR FIELD rb_forn.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*PARAMETERS: rb_cli   RADIOBUTTON GROUP rb1 USER-COMMAND act.
*SELECTION-SCREEN COMMENT 3(08)  text-i16 FOR FIELD rb_cli.
*PARAMETERS: rb_forn  RADIOBUTTON GROUP rb1.
*SELECTION-SCREEN COMMENT 14(10) text-i17 FOR FIELD rb_forn.
*PARAMETERS: rb_prod  RADIOBUTTON GROUP rb1.
*SELECTION-SCREEN COMMENT 26(10) text-i29 FOR FIELD rb_prod.
*PARAMETERS: ck_massa   LIKE bsid-umskz AS CHECKBOX  DEFAULT ' ' MODIF ID b.
*SELECTION-SCREEN COMMENT 70(30) text-i30 FOR FIELD ck_massa.
*SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS: p_bukrs FOR t001-bukrs NO INTERVALS OBLIGATORY,
                p_gsber FOR tgsb-gsber,
                p_parid FOR bsad-kunnr,
                p_ovped FOR bsad-vbel2 MODIF ID c,
                p_sgtxt FOR bsad-sgtxt NO INTERVALS NO-EXTENSION,
                p_zuonr FOR bsad-zuonr NO INTERVALS,
                p_belnr FOR bsad-belnr NO INTERVALS,
                p_dcsim FOR zsdt0041-doc_simulacao NO INTERVALS MODIF ID c,
                p_safra FOR bsik-xref1 NO INTERVALS NO-EXTENSION,
                p_produ FOR bsik-xref3 NO INTERVALS NO-EXTENSION.

SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(31) text-i18. "Unificação Partidas


PARAMETERS: p_zuonrj AS CHECKBOX DEFAULT 'X' MODIF ID a.
SELECTION-SCREEN COMMENT 35(10) text-i12 FOR FIELD p_zuonrj .
PARAMETERS: p_sgtxtj AS CHECKBOX ." DEFAULT 'X'.
SELECTION-SCREEN COMMENT 48(11) text-i11 FOR FIELD p_sgtxtj.

*-------
PARAMETERS: p_dcsimj AS CHECKBOX MODIF ID c.
*SELECTION-SCREEN COMMENT 74(10) text-i28 FOR FIELD p_dcsimj.

PARAMETERS: p_ovpedj  AS CHECKBOX  MODIF ID c.
*SELECTION-SCREEN COMMENT 35(10)  text-i20 FOR FIELD p_ovpedj.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.

SELECTION-SCREEN COMMENT 1(31) text-i19.   "Tipo Partida

PARAMETERS: p_cta_rz AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 35(10)  text-i22 FOR FIELD p_cta_rz.
PARAMETERS: p_cta_nr AS CHECKBOX ." DEFAULT 'X'.
SELECTION-SCREEN COMMENT 48(13) text-i23 FOR FIELD p_cta_nr.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME  TITLE text-i24. "Compensação
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(4) text-i04 FOR FIELD p_augdt.
SELECT-OPTIONS: p_augdt FOR bsad-augdt NO INTERVALS NO-EXTENSION DEFAULT sy-datum OBLIGATORY.

SELECTION-SCREEN COMMENT 23(15) text-i13 FOR FIELD p_blt_cp.
PARAMETERS: p_blt_cp TYPE c LENGTH 2 DEFAULT 'AB' OBLIGATORY.

SELECTION-SCREEN COMMENT 42(02) text-i21. "Espaço


SELECTION-SCREEN COMMENT 46(05) text-i27 FOR FIELD p_ksfcp.
PARAMETERS: p_ksfcp TYPE bkpf-kursf.

PARAMETERS: p_cplib  AS CHECKBOX.
SELECTION-SCREEN COMMENT 77(21) text-i25 FOR FIELD p_cplib.

SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b4.

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: END OF SCREEN 0001.

INITIALIZATION.
  gs_variant-report      = sy-repid.

AT SELECTION-SCREEN OUTPUT.

  IF ck_massa = 'X' AND rb_prod = 'X'.
    REFRESH  p_zuonr.
    SELECT *
      FROM zfit0155
      WHERE bukrs IN @p_bukrs
      AND   EXISTS ( SELECT * FROM bsik_view as b WHERE b~bukrs = zfit0155~bukrs AND b~belnr = zfit0155~belnr AND b~gjahr = zfit0155~gjahr AND b~zuonr = zfit0155~lote AND b~blart IN ('MA','MB')  )
      INTO TABLE @DATA(it_zfit0155).


    SORT it_zfit0155 BY lote.
    DELETE ADJACENT DUPLICATES FROM it_zfit0155 COMPARING lote.
    LOOP AT  it_zfit0155 INTO DATA(wa_zfit0155).
      p_zuonr-sign   = 'I'.
      p_zuonr-option = 'EQ'.
      p_zuonr-low = wa_zfit0155-lote.
      APPEND  p_zuonr.
    ENDLOOP.

  ELSEIF ck_massa = 'X' AND rb_prod = 'X'.
*    REFRESH  p_zuonr.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-group1 = 'C'.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

  CALL SCREEN 0100.
