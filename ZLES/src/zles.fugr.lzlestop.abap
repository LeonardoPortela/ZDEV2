*---------------------------------------------------------------------*
*    generated viewmaintenance function pool top
*---------------------------------------------------------------------*
FUNCTION-POOL zles                       MESSAGE-ID sv.

TABLES: zlest0101.

INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzlest00                                . "view rel. data dcl.


* Controle de PopUp
TYPES:
  BEGIN OF vt04_shipment,
    tvtk_wa  LIKE tvtk,
    ttds_wa  LIKE ttds,
    xvttk_wa LIKE vttkvb,
    yvttk_wa LIKE vttkvb,
    xvttp_wa LIKE vttpvb,
    yvttp_wa LIKE vttpvb,
    xvtts_wa LIKE vttsvb,
    yvtts_wa LIKE vttsvb,
    xvtsp_wa LIKE vtspvb,
    yvtsp_wa LIKE vtspvb,
    xvbpa_wa LIKE vbpavb,
    yvbpa_wa LIKE vbpavb,
    xvtfa_wa LIKE vtfavb,
    yvtfa_wa LIKE vtfavb,
    xsadr_wa LIKE sadrvb,
    ysadr_wa LIKE sadrvb,
    xtrlk_wa LIKE vtrlk,
    xtrlp_wa LIKE vtrlp,
    xvttk    LIKE vttkvb OCCURS 0,
    yvttk    LIKE vttkvb OCCURS 0,
    xvttp    LIKE vttpvb OCCURS 0,
    yvttp    LIKE vttpvb OCCURS 0,
    xvtts    LIKE vttsvb OCCURS 0,
    yvtts    LIKE vttsvb OCCURS 0,
    xvtsp    LIKE vtspvb OCCURS 0,
    yvtsp    LIKE vtspvb OCCURS 0,
    xvbpa    LIKE vbpavb OCCURS 0,
    yvbpa    LIKE vbpavb OCCURS 0,
    xvtfa    LIKE vtfavb OCCURS 0,
    yvtfa    LIKE vtfavb OCCURS 0,
    xsadr    LIKE sadrvb OCCURS 0,
    ysadr    LIKE sadrvb OCCURS 0,
    xtrlk    LIKE vtrlk  OCCURS 0,
    xtrlp    LIKE vtrlp  OCCURS 0,
    xvekp    LIKE vekpvb OCCURS 0,
    yvekp    LIKE vekpvb OCCURS 0,
    xvepo    LIKE vepovb OCCURS 0,
    yvepo    LIKE vepovb OCCURS 0,
  END OF vt04_shipment.

DATA : l_shipment_data      TYPE  vt04_shipment,
       i_veiculo_eixos      TYPE zqt_eixo,
       vl_tknum             TYPE vttk-tknum,
       wk_netwr_all         TYPE  netwr_all,
       wk_waers_all         TYPE  waers_all,
       wk_errors            TYPE  c,
       vl_route             TYPE vttk-route,
       "VL_MARGEM            TYPE ZLEST0103-MARGADTO,
       vl_tplst             TYPE vttk-tplst,
       vl_tdlnr	            TYPE vttk-tdlnr,
       vl_bukrs             TYPE ttds-bukrs,
       vl_adto              TYPE zlest0026-adto,
       wk_0026              TYPE zlest0026,
       vl_fknum             TYPE vfkp-fknum,
       vl_chvadto(10)       TYPE c,
       ti_xvttk             TYPE TABLE OF  vttkvb WITH HEADER LINE,
       ti_xvtts             TYPE TABLE OF  vttsvb WITH HEADER LINE,
       ti_xvttp             TYPE TABLE OF  vttpvb WITH HEADER LINE,
       ti_placa             TYPE TABLE OF zlese0032,
       v_region             TYPE j_1btreg_city-region,
       v_country            TYPE j_1btreg_city-country,
       v_placa1             TYPE zpc_veiculo,
       v_placa2             TYPE zpc_veiculo,
       v_placa3             TYPE zpc_veiculo,
       v_placa4             TYPE zpc_veiculo,
       v_cidade1            TYPE text60,
       v_cidade2            TYPE text60,
       v_cidade3            TYPE text60,
       v_cidade4            TYPE text60,
       nm_fornecedor        TYPE lfa1-name1,
       t_dynpfields         TYPE STANDARD TABLE OF dynpread   INITIAL SIZE 1 WITH HEADER LINE,
       vg_cartao_pedagio    TYPE char01,
       vg_solicita_pedagio  TYPE char01,
       vg_pedi_pedagio      TYPE char01,
       vg_pedi_informado    TYPE char01,
       vg_ck_ped_param      TYPE char01,
       vg_ck_alterou_cidade TYPE char01,
       vg_ck_admim_ped      TYPE char01,
       vg_ck_credito_ped    TYPE char01,
       vg_ck_admim_frete    TYPE char01,
       vg_ck_credito_pedt   TYPE char01,
       vg_pedi_mesmo_veicul TYPE char01,
       vg_pedi_mesma_carga  TYPE char01,
       vg_ck_adiantamento   TYPE char01,
       parceiro_pv          TYPE lifnr,
       lc_tx_cid_origem     TYPE text60,
       lc_tx_cid_destino    TYPE text60,
       obj_pedagio          TYPE REF TO zcl_repom_viagem_vpr,
       wa_zlest0123         TYPE zlest0123.

DATA: ctl_alv_0100       TYPE REF TO cl_gui_alv_grid,
      ctl_con_0100       TYPE REF TO cl_gui_custom_container,
      gs_lay_0100        TYPE lvc_s_layo,
      gs_var_0100        TYPE disvariant,
      gs_scroll_col_0100 TYPE lvc_s_col,
      gs_scroll_row_0100 TYPE lvc_s_roid,
      it_catalog_0100    TYPE lvc_t_fcat.

DATA: it_pracas          TYPE TABLE OF zlest0102 WITH HEADER LINE.

DATA: it_exclude_0100 TYPE ui_functions,
      wa_exclude_0100 LIKE LINE OF it_exclude_0100.


DATA: var_vlr_pedagio TYPE zlest0084-vlr_pedagio,
      var_tp_pedagio  TYPE c.
*TABLES zlest0026.

DATA comboio          TYPE zlest0056.
DATA comboios         TYPE TABLE OF zlest0063.
DATA notas_vinculadas TYPE TABLE OF zlest0060.

TYPES: BEGIN OF ty_codigo,
         rm_codigo TYPE kunnr,
         dt_codigo TYPE kunnr,
       END OF ty_codigo.

DATA: it_rmcodigo TYPE TABLE OF ty_codigo.
DATA: it_dtcodigo TYPE TABLE OF ty_codigo.
DATA: _protocolo TYPE TABLE OF zlest0166.


CONSTANTS: BEGIN OF tipo_embarcacao,
             barcaca    VALUE 'B',
             empurrador VALUE 'E',
             rebocador  VALUE 'R',
           END OF tipo_embarcacao.

*-#133089-12.02.2024-JT-inicio
DATA: vg_faturauto_in           TYPE zlese0220,
      vg_faturauto_out          TYPE zlese0221,
      vg_faturamento_autom      TYPE char01,
      vg_zlest0240_in           TYPE zlest0240,
      w_zlest0241               TYPE zlest0241,
      lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico.
*-#133089-12.02.2024-JT-fim

*CLASS ZCL_AQUAVIARIO DEFINITION.
*
*  PUBLIC SECTION.
*
*    CLASS-METHODS:
*      GETSEQ RETURNING VALUE(RETURN) TYPE NUMC10.
*
*ENDCLASS.
*
*CLASS ZCL_AQUAVIARIO IMPLEMENTATION.
*
*  METHOD GETSEQ.
*    SELECT COUNT(*) INTO RETURN FROM ZLEST0167. ADD 1 TO RETURN.
*  ENDMETHOD.
*
*ENDCLASS.
