*&---------------------------------------------------------------------*
*&  Include           MZENTREMESSATOP
*&---------------------------------------------------------------------*

PROGRAM  sapmzentremessa .

TABLES zsdt_entrada_rem.

DATA: vg_dynnr_0001 TYPE sy-dynnr VALUE '0001',
      vg_dynnr_0002 TYPE sy-dynnr VALUE '0002',
      vg_dynnr_1001 TYPE sy-dynnr VALUE '1001',
      vg_pesquisou  TYPE c LENGTH 1,
      vg_alterou    TYPE c LENGTH 1.

DATA: ok_code   TYPE sy-ucomm,
      wa_fcode  TYPE sy-ucomm,
      it_fcode  LIKE TABLE OF wa_fcode.

DATA: BEGIN OF wa_notas.
        INCLUDE STRUCTURE j_1bnfdoc.
DATA: marc        TYPE c LENGTH 1,
      vbeln       TYPE vbeln_vf,
      lifnr       TYPE lifnr,
      terminal    TYPE name1_gp,
      status      TYPE char1,
      icone       TYPE char4,
      werks_v     TYPE werks_d,
      lgort       TYPE lgort_d,
      vbeln_s     TYPE vbeln_vl,
      dt_chegada  TYPE j_1bpstdat,
      qt_chegada  TYPE ntgew,
      vbeln_ord_e TYPE vbeln_va,
      vbeln_e     TYPE vbeln_vl,
      tp_status   TYPE zstatus,
      vbkd_inco1  TYPE inco1,
      vkorg       TYPE vkorg,
      vtweg       TYPE vtweg,
      spart       TYPE spart.
DATA: END OF wa_notas.

DATA: it_itens TYPE TABLE OF j_1bnflin INITIAL SIZE 0 WITH HEADER LINE.

DATA: it_disp   LIKE STANDARD TABLE OF wa_notas INITIAL SIZE 0 WITH HEADER LINE,
      it_notas  LIKE STANDARD TABLE OF wa_notas INITIAL SIZE 0 WITH HEADER LINE,
      it_desvin LIKE STANDARD TABLE OF wa_notas INITIAL SIZE 0 WITH HEADER LINE,
      it_vinc   LIKE STANDARD TABLE OF wa_notas INITIAL SIZE 0 WITH HEADER LINE,
      it_saidas TYPE TABLE OF zsdt_entrada_rem INITIAL SIZE 0 WITH HEADER LINE.

DATA: wa_itens   TYPE j_1bnflin,
      wa_entrada TYPE zsdt_entrada_rem,
      wa_depara  TYPE zsdt_depara_depo,
      wa_inforv  TYPE zsdt_depara_cenv.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TAB_NOTAS_LIVRE' ITSELF
CONTROLS: tab_notas_livre TYPE TABLEVIEW USING SCREEN 0002.

*&SPWIZARD: DECLARATION OF TABLECONTROL 'TAB_NOTAS_VINC' ITSELF
CONTROLS: tab_notas_vinc TYPE TABLEVIEW USING SCREEN 0002.
