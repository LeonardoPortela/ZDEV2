*&---------------------------------------------------------------------*
*& Include          ZMMR196_TOP
*&---------------------------------------------------------------------*

TABLES: eban, marc.

TYPES:
  BEGIN OF ty_agrup,
    matnr       TYPE mara-matnr,
    werks       TYPE marc-werks,
    total_neces TYPE menge_d,
    saldo_estoq TYPE menge_d,
    saldo_requi TYPE menge_d,
    saldo_pedid TYPE menge_d,
    total_reser TYPE menge_d,
    total_consu TYPE menge_d,
  END OF ty_agrup,

  BEGIN OF ty_mdrs,
    matnr TYPE mdrs-matnr,
    werks TYPE mdrs-werks,
    bdmng TYPE mdrs-bdmng,
  END OF ty_mdrs,

  BEGIN OF ty_mseg,
    matnr      TYPE mseg-matnr,
    werks      TYPE mseg-werks,
    menge      TYPE mseg-menge,
    budat_mkpf TYPE mseg-budat_mkpf,
    anomes(6),
  END OF ty_mseg,

  BEGIN OF ty_contratos2,
    contrato   TYPE string,
    fornecedor TYPE lifnr,
    nome       TYPE name1,
    cidade     TYPE ort01,
    matnr      TYPE matnr,
  END OF ty_contratos2.

TYPES:  BEGIN OF ty_saida.
          INCLUDE STRUCTURE zmm_alv_mrp.
TYPES:    price TYPE   eban-preis.
TYPES:   color             TYPE   kkblo_specialcol OCCURS 0.
TYPES:  END OF ty_saida.

DATA: t_bdcdata    TYPE TABLE OF bdcdata,
      t_plaf       TYPE TABLE OF plaf,
      t_mara1      TYPE TABLE OF mara,
      t_mara       TYPE TABLE OF mara,
      t_marc1      TYPE TABLE OF marc,
      t_marc       TYPE TABLE OF marc,
      t_mard       TYPE TABLE OF mard,
      t_mard_aux   TYPE TABLE OF mard,
      t_eban       TYPE TABLE OF eban,
      t_eban_konnr TYPE TABLE OF eban,
      t_ekpo       TYPE TABLE OF ekpo,
* INICIO - RRIBEIRO - IR243024 - 09.07.2025 - STEFANINI
      t_ekbe       TYPE TABLE OF ekbe,
* FIM - RRIBEIRO - IR243024 - 09.07.2025 - STEFANINI
      t_resb       TYPE TABLE OF resb,
      t_mdrs       TYPE TABLE OF ty_mdrs,
      t_mseg       TYPE TABLE OF ty_mseg,
      t_mseg_aux   TYPE TABLE OF ty_mseg,
      t_agrup      TYPE TABLE OF ty_agrup,
      t_saida      TYPE TABLE OF ty_saida,
      w_saida      TYPE ty_saida,
      t_makt       TYPE TABLE OF makt,
      t_contratos  TYPE TABLE OF ty_contratos2.

DATA: o_alv      TYPE REF TO cl_gui_alv_grid,
      wa_stable  TYPE lvc_s_stbl VALUE 'XX', " RJF
      it_dd07t   type table of dd07t,
      t_fieldcat TYPE lvc_t_fcat.

*DATA: o_container    TYPE REF TO cl_gui_custom_container,
*      o_alv          TYPE REF TO cl_gui_alv_grid,
**      t_fieldcat     TYPE lvc_t_fcat,
*      g_onf4         TYPE REF TO lcl_application_f4,
*      event_receiver TYPE REF TO lcl_event_receiver.

DATA: gv_comprador     TYPE t024-ekgrp,
      gv_descompra     TYPE t024-eknam,
      gv_requisitante  TYPE afnam,
      gv_fornecedor    TYPE lifnr,
      gv_descfornec    TYPE lfa1-name1,
      gv_dt_remessa    TYPE sy-datum,
      gv_deposito      TYPE mard-lgort,
      gv_un            TYPE zmm_alv_mrp-urgencia_neces,
      gv_planejador    TYPE dispo,
      gv_acompanha     TYPE bednr,
      gv_justificativa TYPE char200.
