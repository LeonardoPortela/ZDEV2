*&---------------------------------------------------------------------*
*&  Include           ZSDR0084TOP
*&---------------------------------------------------------------------*

REPORT zsdr0084.

*----------------------------------------------------------------------*
*TABLES                                                                 *
*----------------------------------------------------------------------*
TABLES: tvv3t, tinct.                                       "ZSDT0158.
** Campo STRING Mensagem não deixa declarar a tabela ZSDT0158

*----------------------------------------------------------------------*
*TYPES                                                                 *
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_zsdt0158,
    tipo           TYPE c,
    status_icon(4) TYPE c,
    include        TYPE zsdt0158,
    mensagem       TYPE string,
  END OF ty_zsdt0158,

  BEGIN OF ty_t001w,
    werks      TYPE t001w-werks,
    name1      TYPE t001w-name1,
    j_1bbranch TYPE t001w-j_1bbranch,
    vkorg      TYPE t001w-vkorg,
    regio      TYPE t001w-regio,
    cnpj       TYPE bapibranch-cgc_number,
  END OF ty_t001w,

  BEGIN OF ty_j_1bbranch,
    bukrs      TYPE j_1bbranch-bukrs,
    bukrs1     TYPE j_1bbranch-bukrs,
    branch     TYPE j_1bbranch-branch,
    state_insc TYPE j_1bbranch-state_insc,
  END OF ty_j_1bbranch,

  BEGIN OF ty_lfa1,
    lifnr     TYPE lfa1-lifnr,
    name1     TYPE lfa1-name1,
    land1     TYPE lfa1-land1,
    stras     TYPE lfa1-stras,
    ort01     TYPE lfa1-ort01,
    regio     TYPE lfa1-regio,
    stcd1     TYPE lfa1-stcd1,
    stcd3     TYPE lfa1-stcd3,
    lzone     TYPE lfa1-lzone,
    zone_desc TYPE tzont-vtext,
  END OF ty_lfa1,

  BEGIN OF ty_makt,
    matnr     TYPE makt-matnr,
    maktx     TYPE makt-maktx,
    spras     TYPE makt-spras,
    tipo_prod TYPE tvv3-kvgr3,
  END OF ty_makt,

  BEGIN OF ty_mara,
    matnr TYPE mara-matnr,
    meins TYPE mara-meins,
  END OF ty_mara,

  BEGIN OF ty_kna1,
    kunnr     TYPE kna1-kunnr,
    name1     TYPE kna1-name1,
    land1     TYPE kna1-land1,
    stras     TYPE kna1-stras,
    ort01     TYPE kna1-ort01,
    regio     TYPE kna1-regio,
    stcd1     TYPE kna1-stcd1,
    stcd3     TYPE kna1-stcd3,
    lzone     TYPE kna1-lzone,
    zone_desc TYPE tzont-vtext,
  END OF ty_kna1,

  BEGIN OF ty_trolz,
    aland TYPE trolz-aland,
    azone TYPE trolz-azone,
    lland TYPE trolz-lland,
    lzone TYPE trolz-lzone,
    route TYPE trolz-route,
  END OF ty_trolz,

  BEGIN OF ty_konh,
    knumh   TYPE konh-knumh,
    kotabnr TYPE konh-kotabnr,
    kschl   TYPE konh-kschl,
*---> 10.07.2023 15:51:23 - Migração S4 - DL
*   vakey   TYPE konh-vakey,
    vakey   TYPE konh_kks-vakey,
*<--- 10.07.2023 15:51:23 - Migração S4 - DL
    datab   TYPE konh-datab,
    datbi   TYPE konh-datbi,
  END OF ty_konh,

  BEGIN OF ty_konp,
    knumh TYPE konp-knumh,
    kbetr TYPE konp-kbetr,
    kmein TYPE konp-kmein,
    konwa TYPE konp-konwa,
  END OF ty_konp,

  BEGIN OF ty_frete,
    tp_frete          TYPE tinc-inco1,
    exige_ag_frete(3) TYPE c,
    ag_frete          TYPE lfa1-lifnr,
    ag_frete_desc     TYPE lfa1-name1,
    preco_pauta       TYPE konp-kbetr,
  END OF ty_frete,

  BEGIN OF ty_id_compra,
    id_compra       TYPE zsdt0187-nu_compra, "zsdt0187-id_compra, " 03.05.2022 - RAMON LIMA - RECLIKE
    quantidade      TYPE zsdt0187-quantidade,
    unidade         TYPE zsdt0187-unidade,
    tp_frete_compra TYPE zsdt0187-tp_frete_compra,
  END OF ty_id_compra,

  BEGIN OF ty_saida,
    sequencial(10)        TYPE c,
    nro_sol_ov            TYPE zsdt0158-nro_sol_ov,
    " 22.09.2022 - RAMON - 19450 -->
    ebeln                 TYPE ebeln,
    " 22.09.2022 - RAMON - 19450 --<
    " 05.07.2022 - RAMON - 76636 ->
    indefinido            TYPE c,
    form_lote             TYPE c,
    indust                TYPE c,
    " 05.07.2022 - RAMON - 76636 -<
    " 22.09.2022 - RAMON - 19450 -->
    ped_trans             TYPE c,
    " 22.09.2022 - RAMON - 19450 --<
    filial_desc           TYPE t001w-name1,
    pc_desc               TYPE lfa1-name1,
    propriedade_desc      TYPE lfa1-name1,
    produto_desc          TYPE makt-maktx,
    destino_desc          TYPE kna1-name1,
    ag_frete_desc         TYPE kna1-name1,
    status_itinerario(4)  TYPE c,
    status_solicitacao(4) TYPE c,
    itinerario_desc(40)   TYPE c,
    eudr                  TYPE zsdt0158-eudr, "// wbarbosa US 152850 10/10/2024
  END OF ty_saida.


TYPES: BEGIN OF ty_saida_exec,
         vbeln    TYPE vbak-vbeln,
         msg(255),
       END OF ty_saida_exec.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_zlest0153,
         land1       TYPE zlest0153-land1,
         vtext       TYPE tzont-vtext,
         lzone       TYPE zlest0153-lzone,
         lifnr       TYPE zlest0153-lifnr,
         kunnr       TYPE zlest0153-kunnr,
         us_registro TYPE zlest0153-us_registro,
         dt_registro TYPE zlest0153-dt_registro,
         hr_registro TYPE zlest0153-hr_registro,
       END OF ty_zlest0153.



*----------------------------------------------------------------------*
*CONSTANTES
*----------------------------------------------------------------------*
CONSTANTS:
  BEGIN OF c_main_tab,
    tab1 LIKE sy-ucomm VALUE 'MAIN_TAB_TAB1',
    tab2 LIKE sy-ucomm VALUE 'MAIN_TAB_TAB2',
  END OF c_main_tab.


"Dados para seleção de tela
DATA:
  BEGIN OF i_main_tab,                                 "Tabela para controle das tabs selecionadas na screen 9000.
    subscreen   LIKE sy-dynnr,                         "Subscreen
    prog        LIKE sy-repid VALUE 'ZSDR0084',        "Program
    pressed_tab LIKE sy-ucomm VALUE c_main_tab-tab1,   "Tab
  END OF i_main_tab.

DATA: check_z1     TYPE c,
      check_dco    TYPE c,
      pr_produto_a TYPE c,
      pr_produto_f TYPE c,
      valida       TYPE c.

DATA: vtp_produto TYPE zmmt0017-tp_produto.

*----------------------------------------------------------------------*
*TABLES/WA
*----------------------------------------------------------------------*
DATA: it_t001w          TYPE TABLE OF ty_t001w,
      it_filial_dest    TYPE TABLE OF ty_t001w,
      it_filial_dest2   TYPE TABLE OF ty_t001w,
      it_j_1bbranch     TYPE TABLE OF ty_j_1bbranch,
      it_lfa1_pc        TYPE TABLE OF ty_lfa1,
      it_lfa1_z1        TYPE TABLE OF ty_lfa1,
      it_makt           TYPE TABLE OF ty_makt,
      it_mara           TYPE TABLE OF ty_mara,
      it_kna1           TYPE TABLE OF ty_kna1,
      it_trolz          TYPE TABLE OF ty_trolz,
      it_saida          TYPE TABLE OF ty_saida,
      it_konh           TYPE TABLE OF ty_konh,
      it_konp           TYPE TABLE OF ty_konp,
      it_frete          TYPE TABLE OF ty_frete,
      it_zlest0153      TYPE TABLE OF zlest0153,
      it_z0153          TYPE TABLE OF zlese0153,
      it_zdco_prod      TYPE TABLE OF zdco_produtor,
      it_zsdt0158       TYPE TABLE OF ty_zsdt0158,
      it_resumo         TYPE TABLE OF ty_zsdt0158,
      it_id_compra      TYPE TABLE OF zsdt0187,
      it_zsdt0158_aux   TYPE TABLE OF ty_zsdt0158,
      it_zsdt0158_saida TYPE TABLE OF zsdt0158,
      it_saida_exec     TYPE TABLE OF ty_saida_exec,
      it_zmmt0017       TYPE TABLE OF zmmt0017.


DATA: wa_t001w          TYPE ty_t001w,
      wa_filial_dest    TYPE ty_t001w,
      wa_filial_dest2   TYPE ty_t001w,
      wa_t001w_aux      TYPE ty_t001w,
      wa_j_1bbranch     TYPE ty_j_1bbranch,
      wa_j_1bbranch_aux TYPE ty_j_1bbranch,
      wa_lfa1_pc        TYPE ty_lfa1,
      wa_lfa1_pc_aux    TYPE ty_lfa1,
      wa_lfa1_z1        TYPE ty_lfa1,
      wa_lfa1_z1_aux    TYPE ty_lfa1,
      wa_fornecedor     TYPE ty_lfa1,
      wa_makt           TYPE ty_makt,
      wa_makt_aux       TYPE ty_makt,
      wa_mara           TYPE ty_mara,
      wa_kna1           TYPE ty_kna1,
      wa_kna1_aux       TYPE ty_kna1,
      wa_trolz          TYPE ty_trolz,
      wa_konh           TYPE ty_konh,
      wa_konp           TYPE ty_konp,
      wa_frete          TYPE ty_frete,
      wa_zlest0153      TYPE zlest0153,
      wa_z0153          TYPE zlese0153,
      wa_zdco_prod      TYPE zdco_produtor,
      wa_tvv4t          TYPE tvv4t,
      wa_tvv5t          TYPE tvv5t,
      wa_zsdt0158       TYPE ty_zsdt0158,
      wa_resumo         TYPE ty_zsdt0158,
      wa_zsdt0158_aux   TYPE zsdt0158,
      wa_zsdt0158_saida TYPE zsdt0158,
      wa_saida          TYPE ty_saida,
      wa_id_compra      TYPE ty_id_compra,
      wa_zsdt0158_qt    TYPE zsdt0158_qt,
      wa_saida_exec     TYPE ty_saida_exec,
      wa_zmmt0017       TYPE zmmt0017,
      "wa_t0017          TYPE zmmt0017,
      " 26.07.2022 - RAMON LIMA - RECLIKE -->
      wa_lote           TYPE zsde0005.
" 26.07.2022 - RAMON LIMA - RECLIKE --<

*   "// wbarbosa US 152850 10/10/2024
DATA: is_material_eudr TYPE c.
DATA: is_filial_participante_eudr TYPE c.
DATA: is_filial_classificacao_eudr TYPE c.
DATA: is_processo_eudr TYPE c.
*   "// wbarbosa US 152850 10/10/2024


DATA: estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura.


*----------------------------------------------------------------------*
*VARIÁVEIS
*----------------------------------------------------------------------*
DATA: screen_cadastro TYPE sy-dynnr VALUE 0102,
      screen_a        TYPE sy-repid,
      rd_form         TYPE c,
      rd_ped          TYPE c,
      acao            TYPE sy-ucomm,
      habilita        TYPE c LENGTH 8,
      produto         TYPE makt-matnr,
      ok_code         TYPE sy-ucomm,
      erro_validacao  TYPE c,
      vg_focus(60)    TYPE c,
      vl_block        TYPE t001k-bukrs,
      vl_subrc        TYPE sy-subrc.

" 03.05.2022 - RAMON LIMA - RECLIKE -->
DATA gv_renew TYPE flag.
DATA gv_bukrs TYPE bukrs.
DATA gv_matnr TYPE matnr.

DATA gt_lotes TYPE TABLE OF zsde0005.
DATA gt_ordens TYPE TABLE OF zsde0006.

DATA gv_msg TYPE c LENGTH 200.

DATA gw_zsdt0187_edit TYPE zsdt0187.

DATA gv_text TYPE c LENGTH 200.

" 03.05.2022 - RAMON LIMA - RECLIKE <--
DATA e_lgort          TYPE lgort_d. "// wbarbosa US 152850 10/10/2024
*----------------------------------------------------------------------*
*ALV
*----------------------------------------------------------------------*
DATA: obj_container TYPE REF TO cl_gui_custom_container,
      obj_alv       TYPE REF TO cl_gui_alv_grid.

PARAMETERS p_tab TYPE zsdc0005 NO-DISPLAY.

*----------------------------------------------------------------------*
*START OF SELECTION
*----------------------------------------------------------------------*

START-OF-SELECTION.

  " 03.05.2022 - RAMON LIMA - RECLIKE -->
  DATA lv_erro TYPE c.

  " TRAVA PARA O NOVO DEV, DEPOIS DESCOMENTAR
  "IF sy-uname = 'RBLIMA'.

  PERFORM f_dados_sigam CHANGING lv_erro.

  "ELSE.

  "PERFORM f_tela_go_flux USING space CHANGING lv_erro.

  "ENDIF.

  CHECK lv_erro IS INITIAL.

  " 03.05.2022 - RAMON LIMA - RECLIKE <--

  CALL SCREEN 0100.
