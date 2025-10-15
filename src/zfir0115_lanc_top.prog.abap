TABLES: zfit0216,t001w.

TYPES:
  BEGIN OF ty_lanc,
    bukrs         TYPE zfit0217-bukrs,
    butxt         TYPE t001-butxt,
    werks         TYPE zfit0217-werks,
    name1         TYPE t001w-name1,
    periodo       TYPE char10,
    status        TYPE zfit0217-status,
    status_LANC   TYPE icon_int,
    doc_cont      TYPE zlote_num,
    seqitem       TYPE zfit0217-seqitem,
    dt_doc        TYPE zfit0217-dt_doc,
    dt_lanc       TYPE zfit0217-dt_lanc,
    saknr         TYPE zfit0217-saknr,
    txt50         TYPE zfit0217-txt50,
    d_c           TYPE zfit0217-d_c,
    kostl         TYPE zfit0217-kostl,
    aufnr         TYPE zfit0217-aufnr,
    desc_fornec   TYPE zfit0217-desc_fornec,
    desc_desp_rec TYPE zfit0217-desc_desp_rec,
    nr_doc        TYPE zfit0217-nr_doc,
    dmbtr         TYPE zfit0217-dmbtr,
    estorno       TYPE zfit0217-estorno,
    lote          TYPE zfit0217-lote,
    obj_key       TYPE zfit0217-obj_key,
    monat	        TYPE zfit0217-monat,
    gjahr	        TYPE zfit0217-gjahr,
  END OF ty_lanc.

    TYPES: BEGIN OF ty_memory,
           lr_werks TYPE RANGE OF werks,
           LR_gjahr TYPE RANGE OF gjahr,
           lr_monat TYPE RANGE OF monat,
         END OF ty_memory.

  DATA: LR_memory TYPE ty_memory.

DATA: o_alv    TYPE REF TO cl_salv_table,
      it_saida TYPE STANDARD TABLE OF ty_lanc INITIAL SIZE 0,
      wa_saida TYPE ty_lanc.


SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001 .
  SELECT-OPTIONS:
*                 p_bukrs   FOR t001w-vkorg,
                  p_werks   FOR t001w-werks,"OBLIGATORY,
                  p_gjahr FOR zfit0216-gjahr NO-EXTENSION NO INTERVALS MATCHCODE OBJECT zyear,
                  p_monat FOR zfit0216-monat NO-EXTENSION NO INTERVALS MATCHCODE OBJECT z_help_meses.
SELECTION-SCREEN END OF BLOCK part1.

*START-OF-SELECTION.
*
*
*  CALL SCREEN '0100'.
