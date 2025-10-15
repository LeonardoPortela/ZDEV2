*&---------------------------------------------------------------------*
*& Include          ZSDR0197_TOP
*&---------------------------------------------------------------------*

  TYPES:
    BEGIN OF ty_lotes,
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
    END OF ty_lotes.

  TYPES: BEGIN OF ty_0066.
  TYPES: icon(4)       TYPE c.
  TYPES: status_trace(4) TYPE c.
         INCLUDE STRUCTURE zsdt0066.
  TYPES: color(4)        TYPE c,
         werks_desc      TYPE name1,
         matnr_desc      TYPE maktx,
         terminal_desc   TYPE name1,
         ponto_c_desc    TYPE name1,
         lentrega_desc   TYPE name1,
         kunnr_desc      TYPE name1,
         btgew           TYPE gsgew.

  TYPES END OF ty_0066.

  DATA: t_instrucao TYPE TABLE OF ty_lotes.

  DATA(o_form_lote) = NEW zcl_cadastro_formacao_lote( ).
