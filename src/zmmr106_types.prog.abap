*&---------------------------------------------------------------------*
*&  Include           ZMMR106_TYPES
*&---------------------------------------------------------------------*

TABLES: ekko, sflight.

TYPES:

* Cabeçario
  BEGIN OF ty_ekko,
    bukrs TYPE ekko-bukrs,
    bedat TYPE ekko-bedat,
    ebeln TYPE ekko-ebeln,
  END OF ty_ekko,

* Folha de Aceite
  BEGIN OF ty_aceite,
    ebeln TYPE ekbe-ebeln,
    bewtp TYPE ekbe-bewtp,
    belnr TYPE ekbe-belnr,
    gjahr TYPE ekbe-gjahr,
    bukrs TYPE ekko-bukrs,
  END OF ty_aceite,

* Migo e Miro
  BEGIN OF ty_mm,
    ebeln TYPE ekbe-ebeln,
    lfbnr TYPE ekbe-lfbnr,
    lfgja TYPE ekbe-lfgja,
    bewtp TYPE ekbe-bewtp,
    belnr TYPE ekbe-belnr,
    gjahr TYPE ekbe-gjahr,
    shkzg TYPE ekbe-shkzg,
    xblnr TYPE ekbe-xblnr,
    budat TYPE ekbe-budat,
    bukrs TYPE ekko-bukrs,
    awkey TYPE bkpf-awkey,
    gsber TYPE rbkp-gsber,
    bsart TYPE ekko-bsart,
  END OF ty_mm,

* Solicitações SoftExpert
  BEGIN OF ty_softexpert,
    belnr      TYPE bkpf-belnr,
    id_process TYPE z_id_processo_se_sm,
  END OF ty_softexpert,

* Solicitações SoftExpert
  BEGIN OF ty_softexpert_ped,
    ebeln      TYPE ekko-ebeln,
    id_process TYPE z_id_processo_se_sm,
  END OF ty_softexpert_ped,


  BEGIN OF ty_bk_migo,
    awkey     TYPE bkpf-awkey,
    bukrs     TYPE bkpf-bukrs,
    belnr     TYPE bkpf-belnr,
    gjahr     TYPE bkpf-gjahr,
    belnr_key TYPE ekbe-belnr,
    gjahr_key TYPE ekbe-gjahr,
  END OF ty_bk_migo,

  BEGIN OF ty_bs_migo,
    bukrs TYPE bsis-bukrs,
    belnr TYPE bsis-belnr,
    gjahr TYPE bsis-gjahr,
    hkont TYPE bsis-hkont,
    wrbtr TYPE bsis-wrbtr,
    shkzg TYPE bsis-shkzg,
    buzei TYPE bsis-buzei,
    zuonr TYPE bsis-zuonr,
    xblnr TYPE bsis-xblnr,
  END OF ty_bs_migo,

  BEGIN OF ty_bk_miro,
    awkey     TYPE bkpf-awkey,
    bukrs     TYPE bkpf-bukrs,
    belnr     TYPE bkpf-belnr,
    gjahr     TYPE bkpf-gjahr,
    belnr_key TYPE ekbe-belnr,
    gjahr_key TYPE ekbe-gjahr,
  END OF ty_bk_miro,

  BEGIN OF ty_bs_miro,
    bukrs TYPE bsis-bukrs,
    belnr TYPE bsis-belnr,
    gjahr TYPE bsis-gjahr,
    hkont TYPE bsis-hkont,
    wrbtr TYPE bsis-wrbtr,
    shkzg TYPE bsis-shkzg,
    buzei TYPE bsis-buzei,
    zuonr TYPE bsis-zuonr,
    xblnr TYPE bsis-xblnr,
  END OF ty_bs_miro,

  BEGIN OF ty_saida,
    empresa    TYPE ekko-bukrs,
    pedido     TYPE ekko-ebeln,
    bsart      TYPE ekko-bsart,
    nr_folha   TYPE ekbe-belnr,
    nr_migo    TYPE ekbe-belnr,
    dt_migo    TYPE mkpf-budat,
    nr_miro	   TYPE ekbe-belnr,
    dt_miro    TYPE rbkp-budat,
    gsber      TYPE rbkp-gsber,
    to_miro    TYPE bsis-wrbtr,
    to_migo    TYPE bsis-wrbtr,
    diferenca	 TYPE bsis-wrbtr,
    gjahr_mr   TYPE gjahr,
    gjahr_mg   TYPE gjahr,
    id_process TYPE string,
  END OF ty_saida.

DATA: tw_ekko           TYPE TABLE OF ty_ekko,
      tw_ekko_aux       TYPE TABLE OF ty_ekko,
      tw_aceite         TYPE TABLE OF ty_aceite,
      wa_aceite         TYPE ty_aceite,
      wa_softexpert_ped TYPE ty_softexpert_ped,
      wa_softexpert     TYPE ty_softexpert,
      tw_aceite_aux     TYPE TABLE OF ty_aceite,
      tw_migo           TYPE TABLE OF ty_mm,
      tw_migo_m         TYPE TABLE OF ty_mm,
      wa_migo_m         TYPE ty_mm,
      tw_migo_s_miro    TYPE TABLE OF ty_mm,
      tw_miro           TYPE TABLE OF ty_mm,
      tw_softexpert_ped TYPE TABLE OF ty_softexpert_ped,
      tw_softexpert     TYPE TABLE OF ty_softexpert,
      tw_miro_m         TYPE TABLE OF ty_mm,
      wa_miro_m         TYPE ty_mm,
      tw_temp           TYPE TABLE OF ty_mm,
      tw_bk_migo        TYPE TABLE OF ty_bk_migo,
      tw_bs_migo        TYPE TABLE OF ty_bs_migo,
      tw_bk_miro        TYPE TABLE OF ty_bk_miro,
      tw_bs_miro        TYPE TABLE OF ty_bs_miro,
      tw_bs_migo_c      TYPE TABLE OF ty_bs_miro,
      tw_bs_vbeln       TYPE TABLE OF ty_bs_migo,
      w_bs_vbeln        TYPE ty_bs_migo,
      w_bs_migo         TYPE  ty_bs_miro,
      w_bs_migo_c       TYPE  ty_bs_miro,
      tw_bs_miro_c      TYPE TABLE OF ty_bs_miro,
      w_bs_miro         TYPE  ty_bs_miro,
      w_bs_miro_c       TYPE  ty_bs_miro,
      it_s_migo         TYPE TABLE OF ty_saida,
      it_s_miro         TYPE TABLE OF ty_saida,
      it_saida          TYPE TABLE OF ty_saida,
      tw_saida          TYPE ty_saida,
      it_saida_aux      TYPE TABLE OF ty_saida,
      tw_saida_aux      TYPE ty_saida,
      it_rbkp           TYPE TABLE OF rbkp,
      it_mkpf           TYPE TABLE OF mkpf,
      wa_mkpf           TYPE mkpf,
      wa_rbkp           TYPE rbkp,
*---> 05/07/2023 - Migração S4 - Inicio - AF
*     BUDAT             TYPE BAPI_JBD_DTE_DZTERM,
*     DATA              TYPE BAPI_JBD_DTE_DZTERM.
      budat             TYPE TB_DZTERM,
      data              TYPE TB_DZTERM.
*---> 05/07/2023 - Migração S4 - Fim - AF

"DATA: IT_SS TYPE TABLE OF SFLIGHT.

FIELD-SYMBOLS: <fs_bkpf> TYPE ty_mm,
               <ekko>    TYPE ty_ekko,
               <migo>    TYPE ty_mm,
               <aux>     TYPE ty_mm,
               <saida>   TYPE ty_saida,
               <aceite>  TYPE ty_aceite,
               <miro>    TYPE ty_mm,
               <bk_migo> TYPE ty_bk_migo,
               <bs_migo> TYPE ty_bs_migo,
               <bk_miro> TYPE ty_bk_miro,
               <bs_miro> TYPE ty_bs_miro.

DATA: it_fcat     TYPE lvc_t_fcat,
      wa_fcat     TYPE lvc_s_fcat,
      wa_layout   TYPE lvc_s_layo,
      wa_variante TYPE disvariant,
      wa_alv      TYPE REF TO cl_gui_alv_grid,
      wa_cont     TYPE REF TO cl_gui_custom_container,
      wa_stable   TYPE lvc_s_stbl,
      ty_toolbar  TYPE stb_button,
      c_alv_tm    TYPE REF TO cl_alv_grid_toolbar_manager,
      it_sel_rows TYPE lvc_t_row,
      wa_sel_rows TYPE lvc_s_row,
      ok_code     LIKE sy-ucomm,
      sc_tela     TYPE sy-dynnr VALUE '0103', " Tela Inicial Em Branco
      sc_alv      TYPE sy-dynnr VALUE '0101', " Tela da ALV
      conta       TYPE bsis-hkont VALUE '0000212100'.

DATA: it_bdc     TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      messtab    TYPE bdcmsgcoll OCCURS 0,
      wmesstab   TYPE bdcmsgcoll,
      wl_mode(1),
      p_texto    TYPE string.

DATA: tl_bdc TYPE TABLE OF bdcdata,
      wl_bdc TYPE bdcdata.

SELECTION-SCREEN: BEGIN OF SCREEN 0102 AS SUBSCREEN.
  SELECT-OPTIONS: s_emp FOR ekko-bukrs NO INTERVALS NO-EXTENSION OBLIGATORY,
                  s_dtp FOR ekko-bedat OBLIGATORY,
                  s_ped FOR ekko-ebeln.
SELECTION-SCREEN: END OF SCREEN 0102.
