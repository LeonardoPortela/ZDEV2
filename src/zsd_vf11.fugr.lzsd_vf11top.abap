FUNCTION-POOL zsd_vf11.                     "MESSAGE-ID ..

TYPES: BEGIN OF ty_insert,
         doc   TYPE zsdt0041-doc_simulacao,
         vbeln TYPE vbeln,
         vbelv TYPE vbelv,
         matnr TYPE matnr,
         posnr TYPE posnr,
         charg TYPE charg_d,
       END OF ty_insert,

       BEGIN OF ty_0090.
         INCLUDE STRUCTURE zsdt0090.
TYPES posnr TYPE vbap-posnr.
TYPES END OF ty_0090.

DATA: it_insert TYPE TABLE OF ty_insert,
      wa_insert TYPE ty_insert.
DATA: im_0090 TYPE zsdt0090,
      p_0090  TYPE ty_0090.

DATA: wa_setleaf TYPE setleaf.

DATA: lt_vbfa TYPE TABLE OF vbfa,
      lw_vbfa TYPE vbfa.

DATA: lt_vbfa_equ TYPE TABLE OF vbfa,
      lw_vbfa_equ TYPE vbfa.

DATA: lt_zsdt0053 TYPE TABLE OF zsdt0053,
      lw_zsdt0053 TYPE zsdt0053.

DATA: lt_zsdt0055 TYPE TABLE OF zsdt0055,
      lw_zsdt0055 TYPE zsdt0055.

DATA: lt_zsdt0073 TYPE TABLE OF zsdt0073,
      lw_zsdt0073 TYPE zsdt0073.

DATA: wa_ins_0053      TYPE zsdt0053.
DATA: lt_zsdt0053_item TYPE TABLE OF zsdt0053,
      lw_zsdt0053_item TYPE zsdt0053,

      lt_zsdt0053_aux  TYPE TABLE OF zsdt0053,
      lw_zsdt0053_aux  TYPE zsdt0053,
      lw_zsdt0053_POS  TYPE zsdt0053,    " <<RIM-SKM-IR130621/IR134233 - 16.03.23

      lt_zsdt0041      TYPE TABLE OF zsdt0041,
      lw_zsdt0041      TYPE zsdt0041,

      lt_zsdt0090      TYPE TABLE OF zsdt0090,
      lw_zsdt0090      TYPE zsdt0090,

      lt_zsdt0053_equ  TYPE TABLE OF zsdt0053,
      lw_zsdt0053_equ  TYPE zsdt0053,

      lt_vbap          TYPE TABLE OF vbap,
      lw_vbap          TYPE vbap,

      it_vbap          TYPE TABLE OF vbap,
      wa_vbap          TYPE vbap.

DATA: var_next_posnr TYPE zsdt0053-posnr,
      var_tabix      TYPE sy-tabix.

DATA: lt_zsdt0059 TYPE TABLE OF zsdt0059,
      lw_zsdt0059 TYPE zsdt0059.

DATA: lt_zsdt0100 TYPE TABLE OF zsdt0100,
      lw_zsdt0100 TYPE zsdt0100.

DATA: lt_zsdt0056 TYPE TABLE OF zsdt0056,
      lw_zsdt0056 TYPE zsdt0056.

DATA: r_bezei      TYPE RANGE OF zsdt0056-bezei,
      r_bezei_line LIKE LINE OF r_bezei.

DATA: c_bezei      TYPE RANGE OF zsdt0056-bezei,
      c_bezei_line LIKE LINE OF c_bezei.

DATA: p_bezei      TYPE RANGE OF zsdt0056-bezei,
      p_bezei_line LIKE LINE OF p_bezei.

" 29.10.2024 - 147331 - RAMON -->
DATA: b_bezei      TYPE RANGE OF zsdt0056-bezei,
      b_bezei_line LIKE LINE OF p_bezei.
" 29.10.2024 - 147331 - RAMON --<

DATA: var_len   TYPE i.
DATA: var_dir   TYPE bezei30.
DATA: dir_mi_in   TYPE c LENGTH 3.
DATA: vl_auart TYPE vbak-auart.

DATA: var_bezei TYPE zsdt0056-bezei.

DATA: var_formula  TYPE zsdt0059-formula.

DATA: var_msg                       TYPE string.
DATA: cx_exception                  TYPE REF TO zcx_webservice.
DATA: obj_zcl_webservice_taxa_curva TYPE REF TO zcl_webservice_tx_curva.
DATA: obj_auart                     TYPE REF TO zcl_taxa_curva.

DATA: r_auart TYPE RANGE OF auart.
DATA: r_comp TYPE RANGE OF auart.
DATA: r_devo_recu TYPE RANGE OF auart.

DATA: r_auart_ztrg TYPE RANGE OF auart.

DATA: var_transf(20) TYPE c,
      msg            TYPE string.

DATA: sl_header          TYPE bapi2017_gm_head_01,
      vl_code            TYPE bapi2017_gm_code,
      vl_doc             TYPE bapi2017_gm_head_ret-mat_doc,
      vl_year            TYPE bapi2017_gm_head_ret-doc_year,
      tl_item            TYPE TABLE OF bapi2017_gm_item_create,
      tl_return          TYPE TABLE OF bapiret2,
      sl_return          TYPE bapiret2,
      sl_item            TYPE bapi2017_gm_item_create,
      it_return          TYPE TABLE OF bapiret2,
      wa_return          TYPE bapiret2,
      es_bflushflags     LIKE bapi_rm_flg,
      es_bflushdatagen   LIKE bapi_rm_datgen,
      es_confirmation    LIKE bapi_rm_datkey-confirmation,
      sperr_user         TYPE sy-msgv1,
      fg_bloqueio(1),
      vlines             TYPE sy-tabix,
      vg_interface(2),
      lva_msg_erro_vf    TYPE string,
      lva_valor_s_aux_01 TYPE c LENGTH 50,
      lva_valor_s_aux_02 TYPE c LENGTH 50,
      vg_obj_key         TYPE zmmt_ee_zgr-obj_key,
      it_outreturn       TYPE TABLE OF zfie_ret_document,
      wa_outreturn       TYPE zfie_ret_document,
      it_zppt0006        TYPE TABLE OF zppt0006,
      wa_zppt0006        TYPE zppt0006,
      wa_head_ret        TYPE bapi2017_gm_head_ret.

DATA: obj TYPE REF TO zcl_solicitacao_ov.
DATA vcategoria TYPE c.
DATA: sair TYPE i.
