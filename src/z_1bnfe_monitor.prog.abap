*&---------------------------------------------------------------------*
*& Report  J_1BNFE_MONITOR                                             *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  Electronic Nota Fiscal/Conhecimento (NF-e/CT-e) - Brazil           *
*&  Monitoring of document status and SCS (system-communication status)*
*&  Component:   XX-CSC-BR                                             *
*&  Development: Q4 2006                                               *
*&---------------------------------------------------------------------*
REPORT  z1bnfe_monitor.

************************************************************************
* SELECTION CRITERIA**                                                 *
************************************************************************

* Screen data transfer - tables declaration                 "1144149
                                                            "1144149
TABLES j_1bnfe_active.                                      "1144149
TABLES j_1bnfe_cancelrt.                                    "1144149
TABLES zib_nfe_forn.
TABLES zsdt0118.
TABLES j_1bnfdoc.

* Internal table with current NF-e status

DATA: it_nfe_active TYPE TABLE OF j_1bnfe_active
         WITH KEY docnum,
      wa_nfe_active TYPE j_1bnfe_active.

* tabela interna Carta de Correção
DATA: it_zcarta_correcao TYPE TABLE OF zcarta_correcao.

* PARAMETERS: NF-e Data
SELECTION-SCREEN BEGIN OF BLOCK nfedata WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: docnum FOR wa_nfe_active-docnum,
                  date0  FOR wa_nfe_active-credat DEFAULT sy-datum,
                  date   FOR wa_nfe_active-action_date,
                  docsta FOR wa_nfe_active-docsta,
                  scssta FOR wa_nfe_active-scssta,
                  code   FOR wa_nfe_active-code,
                  st_nota FOR zib_nfe_forn-st_nota  ,
                  direct FOR wa_nfe_active-direct,
                  form   FOR wa_nfe_active-form.
  PARAMETERS: contin TYPE j_1bnfe_active-conting AS CHECKBOX,
              contis TYPE j_1bnfe_active-conting_s AS CHECKBOX,
              cancel TYPE j_1bnfe_active-cancel    AS CHECKBOX,
              printd TYPE j_1bnfe_active-printd    AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK nfedata.

* PARAMETERS: Documents requiring manual step
SELECTION-SCREEN BEGIN OF BLOCK useact WITH FRAME TITLE TEXT-005.
  SELECT-OPTIONS: useact   FOR wa_nfe_active-action_requ.
SELECTION-SCREEN END OF BLOCK useact.

* PARAMETERS: Access Key Data
SELECTION-SCREEN BEGIN OF BLOCK acckey WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: stcd1    FOR wa_nfe_active-stcd1,
                  model    FOR wa_nfe_active-model NO-DISPLAY,
                  nfnum9   FOR wa_nfe_active-nfnum9,
                  serie    FOR wa_nfe_active-serie,
                  nfyear   FOR wa_nfe_active-nfyear,
                  nfmont   FOR wa_nfe_active-nfmonth,
                  chave    FOR j_1bnfdoc-street NO INTERVALS."MODIF ID nfe

  SELECT-OPTIONS: "chavecte FOR j_1bnfdoc-street MODIF ID cte NO INTERVALS,
                  chavecnf FOR j_1bnfdoc-street MODIF ID cte NO INTERVALS.
SELECTION-SCREEN END OF BLOCK acckey.

* PARAMETERS: Company Data
SELECTION-SCREEN BEGIN OF BLOCK compdata WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: bukrs  FOR wa_nfe_active-bukrs
                         NO INTERVALS NO-EXTENSION          "1375894
                         OBLIGATORY MEMORY ID buk,          "1375894
                  "bupla  FOR wa_nfe_active-branch NO INTERVALS NO-EXTENSION OBLIGATORY ,
                  bupla  FOR wa_nfe_active-branch  ,
                  shipt  FOR wa_nfe_active-vstel.
SELECTION-SCREEN END OF BLOCK compdata.

* PARMETERS: Additional Data
SELECTION-SCREEN BEGIN OF BLOCK addata WITH FRAME TITLE TEXT-004.
  SELECT-OPTIONS: user     FOR wa_nfe_active-action_user,
                  usercre  FOR wa_nfe_active-crenam DEFAULT sy-uname, "1165360
                  partner  FOR wa_nfe_active-parid NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK addata.

SELECTION-SCREEN BEGIN OF BLOCK bmdfe WITH FRAME TITLE TEXT-017.
  SELECT-OPTIONS: pdocref  FOR  j_1bnfdoc-docref MODIF ID mfe,
                  pplaca   FOR  zsdt0118-placa_cav MODIF ID mfe.
SELECTION-SCREEN END OF BLOCK bmdfe.

************************************************************************
* TYPE DEFINITION
************************************************************************

TYPE-POOLS: icon, vrm.

************************************************************************
* DATA DEFINITION
************************************************************************

*---------- Constants -------------------------------------------------*

DATA: c_x                  TYPE c         VALUE 'X',
      c_100(3)             TYPE c         VALUE '100',      "1144149
      c_102(3)             TYPE c         VALUE '102',      "1144149
      c_18                 TYPE i         VALUE '18',
      c_15                 TYPE i         VALUE '15',       "1144149
      c_1                  TYPE i         VALUE '1',
      c_2                  TYPE i         VALUE '2',        "1265172


      c_fcode_req_cancel   TYPE sy-ucomm  VALUE 'REQ_CANCEL',
      c_fcode_req_again    TYPE sy-ucomm  VALUE 'REQ_AGAIN',
      c_fcode_conting      TYPE sy-ucomm  VALUE 'CONTING',
      c_fcode_conting_rs   TYPE sy-ucomm  VALUE 'CONTING_RS',
      c_fcode_send_nfe     TYPE sy-ucomm  VALUE 'SEND_NFE',
      c_fcode_delete_log   TYPE sy-ucomm  VALUE 'DELETE_LOG',
      c_fcode_cont_region  TYPE sy-ucomm  VALUE 'C_REGION',
      c_fcode_cont_branch  TYPE sy-ucomm  VALUE 'C_BUPLA',
      c_fcode_ccorrec      TYPE sy-ucomm  VALUE 'CCORREC',
      c_fcode_canc_extem   TYPE sy-ucomm  VALUE 'CANC_EXTEM',
      c_fcode_reenviar_grc TYPE sy-ucomm  VALUE 'REENV_GRC',
      c_fcode_reenv_contin TYPE sy-ucomm  VALUE 'RE_CONTING', "*-CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
      c_fcode_sincdocecc   TYPE sy-ucomm  VALUE 'SINCDOCECC',
      c_fcode_guia_agro    TYPE sy-ucomm  VALUE 'GUIA_AGRO',  "<<<------"188425 - NMS ------->>>
      c_fcode_cng_doc      TYPE sy-ucomm  VALUE 'CNG_DOC',
      c_fcode_exporta      TYPE sy-ucomm  VALUE 'EXPORTA',

      c_fcode_req_cancel2  TYPE sy-ucomm  VALUE 'REQ_CANCE2',
      c_fcode_req_again2   TYPE sy-ucomm  VALUE 'REQ_AGAIN2',
      c_fcode_conting2     TYPE sy-ucomm  VALUE 'CONTING2',
      c_fcode_conting_rs2  TYPE sy-ucomm  VALUE 'CONTING_R2',
      c_fcode_send_nfe2    TYPE sy-ucomm  VALUE 'SEND_CTE',
      c_fcode_delete_log2  TYPE sy-ucomm  VALUE 'DELETE_LO2',
      c_fcode_cont_region2 TYPE sy-ucomm  VALUE 'C_REGION2',
      c_fcode_cont_branch2 TYPE sy-ucomm  VALUE 'C_BUPLA2',
      c_fcode_cte_tela     TYPE sy-ucomm  VALUE 'CTE_TELA',
      c_fcode_cte_ciot     TYPE sy-ucomm  VALUE 'CTE_CIOT',

      c_fcode_send_mdfe    TYPE sy-ucomm  VALUE 'SEND_MDFE',
      c_fcode_close_mdfe   TYPE sy-ucomm  VALUE 'ENC_MDFE',
      c_fcode_emitir_mdfe  TYPE sy-ucomm  VALUE 'NEW_MDFE',
      c_fcode_frete_segur  TYPE sy-ucomm  VALUE 'FRETE_SEG'," Rubenilson Pereira - 09.10.25 #192341

      c_object             TYPE balobj    VALUE 'NFE',
      c_subobject          TYPE balsubobj VALUE 'MONITOR',
      c_grid_color_c300(4) TYPE c         VALUE 'C300'.       "Yellow
DATA: c_scs_0 TYPE c         VALUE '0',
      c_scs_1 TYPE c         VALUE '1',
      c_scs_3 TYPE c         VALUE '3',
      c_mss_a TYPE c         VALUE 'A',
      c_mss_b TYPE c         VALUE 'B',
      c_mss_c TYPE c         VALUE 'C'.



*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_wa_corr_parc,
         parvw        TYPE c LENGTH 50,
         partxt       TYPE c LENGTH 20,
         old_parid    TYPE j_1bnfnad-parid,
         old_cnpj_cpf TYPE stcd1,
         old_tipo     TYPE c,
         old_ie       TYPE stcd3,
         old_rz       TYPE name1_gp,
         new_parid    TYPE j_1bnfnad-parid,
         new_parid_c  TYPE j_1bnfnad-parid,
         new_parid_v  TYPE j_1bnfnad-parid,
         new_cnpj_cpf TYPE stcd1,
         new_tipo     TYPE c,
         new_ie       TYPE stcd3,
         new_rz       TYPE name1_gp,
         txt_corr     TYPE c LENGTH 200,
       END OF ty_wa_corr_parc.

TYPES: BEGIN OF ty_active_chg,
         change_st_doc  TYPE char01,
         change_dt_auth TYPE char01,
         manual         TYPE j_1bnfdoc-manual,
         encerrado      TYPE char01.
         INCLUDE STRUCTURE j_1bnfe_active.
TYPES: END OF ty_active_chg.


TYPES: BEGIN OF ty_editor,
         line(100),
       END   OF ty_editor.

DATA: g_descbox   TYPE scrfname VALUE 'CC_DESC',
      obg_descbox TYPE REF TO cl_gui_textedit,
      obg_desccte TYPE REF TO cl_gui_textedit,
      tg_editor   TYPE TABLE OF ty_editor,
      wg_editor   TYPE ty_editor.

DATA: obg_descbox_mdfe   TYPE REF TO cl_gui_textedit,
      obg_desc_mdfe_canc TYPE REF TO cl_gui_textedit,
      tg_editor_mdfe     TYPE TABLE OF ty_editor,
      wg_editor_mdfe     TYPE ty_editor.

*---------- Fields ----------------------------------------------------*

DATA: index  TYPE sy-tabix.
DATA: indic  TYPE c.
DATA: subrc  TYPE c.

DATA gf_log_entries_exist    TYPE c.
DATA gf_nfobjn               LIKE j_1binterf-nfobjn.
DATA gf_index(4)             TYPE c.
DATA gf_col_num_total        TYPE i.
DATA gf_authorization_nfe_03 TYPE c.
DATA gf_authorization_nfe_35 TYPE c.
DATA gf_authorization_nfe_85 TYPE c.
DATA gf_authorization_mdf_01 TYPE c.
DATA gf_authorization_mdf_02 TYPE c.
DATA gf_docnum               TYPE j_1bnfe_active-docnum.
DATA gf_werks                TYPE j_1bnflin-werks.
DATA gf_werks_d              TYPE j_1bnflin-werks.
DATA gf_lgort_origem         TYPE t001l-lgort.
DATA gf_lgort_destino        TYPE t001l-lgort.
DATA gf_opt_dep(1).
DATA gf_opt_cen(1).
DATA gf_inf_dados_transf_cce(1).



DATA: wg_active_change TYPE ty_active_chg.

DATA check_negative TYPE c.                                 "1144149

* Fields for screen 0100, 0101

DATA: ok_code            LIKE sy-ucomm,
      gf_cancel          TYPE c,
      gf_first_display   TYPE c         VALUE 'X',
      gf_scroll          TYPE c,
      txt_correc         TYPE c LENGTH 1000,
      g_custom_cont_desc TYPE REF TO cl_gui_custom_container,
      g_custom_cont_cte  TYPE REF TO cl_gui_custom_container,
      g_custom_mdfe_canc TYPE REF TO cl_gui_custom_container.


DATA: BEGIN OF gs_101,
        textline1(50) TYPE c,
        textline2(50) TYPE c,
      END OF gs_101.
**<<<------"188425 - NMS - INI------>>>
DATA: BEGIN OF eg_guia_agro,
        docnum    TYPE j_1bdocnum,
        tpguia    TYPE j_1btpguia,
        ufguia    TYPE j_1bufguia,
        serieguia TYPE j_1bserieguia,
        nguia     TYPE j_1bnguia,
      END   OF eg_guia_agro,
      eg_guia_agro_ori LIKE eg_guia_agro,
      tg_guia_agro     LIKE TABLE OF eg_guia_agro.
**<<<------"188425 - NMS - FIM------>>>
*---------- Select options --------------------------------------------*

DATA: BEGIN OF gs_contin,
        sign      TYPE c,
        option(2) TYPE c,
        low       TYPE c,
        high      TYPE c,
      END OF gs_contin.
DATA: gt_contin LIKE TABLE OF gs_contin.

DATA: BEGIN OF gs_contis,
        sign      TYPE c,
        option(2) TYPE c,
        low       TYPE c,
        high      TYPE c,
      END OF gs_contis.
DATA: gt_contis LIKE TABLE OF gs_contis.

DATA: BEGIN OF gs_cancel,
        sign      TYPE c,
        option(2) TYPE c,
        low       TYPE c,
        high      TYPE c,
      END OF gs_cancel.
DATA: gt_cancel LIKE TABLE OF gs_cancel.

DATA: BEGIN OF gs_printed,
        sign      TYPE c,
        option(2) TYPE c,
        low       TYPE c,
        high      TYPE c,
      END OF gs_printed.
DATA: gt_printed LIKE TABLE OF gs_printed.

*---------- ALV grid -------------------------------------------------*

* Reference for ALV-grid control and container control

DATA: ctl_alv_nfe     TYPE REF TO cl_gui_alv_grid,
      ctl_cccontainer TYPE REF TO cl_gui_container,
      splitter        TYPE REF TO cl_gui_splitter_container,
      splitter_button TYPE REF TO cl_gui_splitter_container.

DATA: ctl_alv_nfe_hist     TYPE REF TO cl_gui_alv_grid,
      ctl_cccontainer2     TYPE REF TO cl_gui_container,
      ctl_cccontainer_hist TYPE REF TO cl_gui_container.

DATA: ctl_alv_nfe_ref     TYPE REF TO cl_gui_alv_grid,
      ctl_cccontainer_ref TYPE REF TO cl_gui_container.

DATA: wg_zsdt0102 TYPE zsdt0102.

***************************************************************
* Alteracao 26.11.2013 -ALRS
***************************************************************
*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.

DATA: ctl_alv_cte_corr     TYPE REF TO cl_gui_alv_grid,
      ctl_alv_cte_resu     TYPE REF TO cl_gui_alv_grid,
      wa_stable            TYPE lvc_s_stbl,
      ctl_cccontainer3     TYPE REF TO cl_gui_custom_container,
      ctl_cccontainer4     TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar.

DATA:
  tg_selectedcell TYPE lvc_t_cell,
  wg_selectedcell TYPE lvc_s_cell,
  x_field(30),
  x_linha         TYPE sy-tabix,
  wl_desactive.

*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.

***************************************************************
* Alteracao 26.11.2013 -ALRS
***************************************************************

DATA: ctl_cancel_alv    TYPE REF TO cl_gui_alv_grid,        "1144149
      ctl_cccontainer_c TYPE REF TO cl_gui_custom_container. "1144149

* ALV field catalogs

DATA: it_fieldcatalog TYPE lvc_t_fcat,
      wa_fieldcatalog TYPE lvc_s_fcat.
DATA: it_fieldcatalog2 TYPE lvc_t_fcat,
      wa_fieldcatalog2 TYPE lvc_s_fcat.
DATA: it_fieldcatalog_c TYPE lvc_t_fcat,                    "1144149
      wa_fieldcatalog_c TYPE lvc_s_fcat.                    "1144149

***************************************************************
* Alteracao 26.11.2013 -ALRS
***************************************************************
DATA: it_fieldcatalog3 TYPE lvc_t_fcat,
      wa_fieldcatalog3 TYPE lvc_s_fcat.

DATA: it_fieldcatalog4 TYPE lvc_t_fcat,
      wa_fieldcatalog4 TYPE lvc_s_fcat.


* ALV layout

DATA: gs_layout   TYPE lvc_s_layo,
      gs_layout2  TYPE lvc_s_layo,
      gs_layout3  TYPE lvc_s_layo,
      gs_layout4  TYPE lvc_s_layo,
      gs_layout_c TYPE lvc_s_layo.                          "1144149

* ALV scroll info - actual row and column in the ALV display

DATA: gs_scroll_col TYPE lvc_s_col,
      gs_scroll_row TYPE lvc_s_roid.
DATA: gs_scroll_col_102 TYPE lvc_s_col,                     "1144149
      gs_scroll_row_102 TYPE lvc_s_roid.                    "1144149

* ALV display options

DATA gs_alv_refres_cond TYPE lvc_s_stbl.

* ALV excluded functions

DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

* ALV layout variant

DATA: gs_variant     TYPE disvariant,
      gs_variant_c   TYPE disvariant,                       "1144149
      gs_variant_cte TYPE disvariant.                       "1144149

* Alv Styles
DATA : ls_edit TYPE lvc_s_styl,
       lt_edit TYPE lvc_t_styl.

*---------- Application Log  ------------------------------------------*

DATA: gs_log_filter  TYPE bal_s_lfil,
      gs_object      TYPE bal_s_obj,
      gs_subobject   TYPE bal_s_sub,
      gs_extnum      TYPE bal_s_extn,
      gs_date_time   TYPE bal_s_dttm,
      gs_log         TYPE bal_s_log,
*      it_log_header TYPE balhdr_t,                         "1150172
      it_log_header2 TYPE balhdr_t,                         "1150172
      it_log_handle  TYPE bal_t_logh,
      it_msg_handle  TYPE bal_t_msgh,
      wa_log_header  LIKE LINE OF it_log_header2,
      wa_log_handle  LIKE LINE OF it_log_handle,
      wa_msg_handle  LIKE LINE OF it_msg_handle.
DATA: it_log_header  TYPE TABLE OF balhdr                   "1150172
         WITH KEY extnumber.                                "1150172

*---------- Counter ---------------------------------------------------*


*---------- Internal tables (and related work areas)-------------------*

DATA: wa_fcode TYPE sy-ucomm,
      it_fcode LIKE TABLE OF wa_fcode.


* NF-e History

DATA: it_nfe_history TYPE TABLE OF j_1bnfe_history
         WITH KEY docnum docsta,
      wa_nfe_history TYPE j_1bnfe_history.

* Official-Status-Codes Texts

DATA: it_code_text TYPE TABLE OF j_1bstscodet
         WITH KEY code,
      wa_code_text TYPE j_1bstscodet.

* Process Status Texts

DATA: it_scs_text TYPE TABLE OF dd07v
         WITH KEY domvalue_l,
      wa_scs_text TYPE dd07v.

* Process Status Texts

DATA: it_doc_text TYPE TABLE OF dd07v
         WITH KEY domvalue_l,
      wa_doc_text TYPE dd07v.

* Cancellation Reason Texts                                 "1144194
                                                            "1144194
DATA: BEGIN OF wa_cancel_reason,                            "1144194
        reason  TYPE j_1bnfe_cancelrt-reason,               "1144194
        reason1 TYPE j_1bnfe_cancelrt-reason1,              "1144194
        reason2 TYPE j_1bnfe_cancelrt-reason2,              "1144194
        reason3 TYPE j_1bnfe_cancelrt-reason3,              "1144194
        reason4 TYPE j_1bnfe_cancelrt-reason4,              "1144194
      END OF wa_cancel_reason.                              "1144194
DATA: gt_cancel_reason LIKE TABLE OF wa_cancel_reason       "1144194
         WITH KEY reason.                                   "1144194
DATA: wa_cancelrt      TYPE j_1bnfe_cancelrt.               "1144194

* ALV display of current NF-e status
TYPES BEGIN OF ty_nfe_alv.
TYPES: status      TYPE icon-id.
TYPES: status_ciot TYPE icon-id.
TYPES: errlog      TYPE icon-id.
TYPES: encerrado   TYPE icon-id.
TYPES: refkey      TYPE j_1bnflin-refkey.
TYPES: usnam       TYPE rbkp-usnam.
TYPES: belnr       TYPE bkpf-belnr.
TYPES: zfbdt       TYPE rbkp-zfbdt.
TYPES: augdt    TYPE bsak-augdt.
       INCLUDE STRUCTURE j_1bnfe_active.
TYPES: dt_envio TYPE zde_dt_envio_email.
TYPES: hr_envio TYPE zde_hr_envio_email.
TYPES: ds_email TYPE char255.
TYPES: chave    TYPE zde_chave_doc_e.
TYPES: chavecte TYPE zde_chave_doc_e.
TYPES: chavecnf TYPE string.
TYPES END OF ty_nfe_alv.

DATA: wa_nfe_alv TYPE ty_nfe_alv.

*DATA: BEGIN OF WA_NFE_ALV,
*        STATUS      TYPE ICON-ID,
*        STATUS_CIOT TYPE ICON-ID,
*        ERRLOG      TYPE ICON-ID,
*        ENCERRADO   TYPE ICON-ID,
*        REFKEY      TYPE J_1BNFLIN-REFKEY,
*        USNAM       TYPE RBKP-USNAM,
*        BELNR       TYPE BKPF-BELNR,
*        ZFBDT       TYPE RBKP-ZFBDT,
*        AUGDT       TYPE BSAK-AUGDT.
*        INCLUDE STRUCTURE J_1BNFE_ACTIVE.
*      DATA: END OF WA_NFE_ALV.

DATA: it_nfe_alv LIKE TABLE OF wa_nfe_alv
         WITH KEY docnum.

* ALV display of NF-e history

DATA: BEGIN OF wa_nfe_alv2,
        docstat TYPE j_1bstscodet-text,
        scsstat TYPE j_1bstscodet-text,
        codet   TYPE j_1bstscodet-text.
        INCLUDE STRUCTURE j_1bnfe_history.
DATA: END OF wa_nfe_alv2.

DATA: it_nfe_alv2 LIKE TABLE OF wa_nfe_alv2
         WITH KEY docnum docsta.

***************************************************************
* Alteracao 26.11.2013 -ALRS
***************************************************************
DATA: BEGIN OF wa_cte_corr,
        grupo       TYPE zsdt0081-grupo,
        campo       TYPE zsdt0081-campo,
        valor       TYPE zsdt0081-valor,
        valor1      TYPE zsdt0081-valor1,
        valor2      TYPE zsdt0081-valor2,
        valor3      TYPE zsdt0081-valor3,
        pc_veiculo  TYPE c LENGTH 14,
        chave       TYPE c LENGTH 50,
        field_style TYPE lvc_t_styl,
      END OF wa_cte_corr.

DATA: it_cte_corr    LIKE TABLE OF wa_cte_corr,
      st_it_cte_corr LIKE LINE  OF it_cte_corr,
      gt_f4          TYPE lvc_t_f4 WITH HEADER LINE.

DATA: BEGIN OF wa_cte_resu,
        grupo     TYPE zsdt0080-grupo,
        campo     TYPE zsdt0080-campo,
        valor(30),
      END OF wa_cte_resu.

DATA: it_cte_resu LIKE TABLE OF wa_cte_resu.

* ALV selection

DATA: it_selected_rows    TYPE lvc_t_row,
      wa_selected_rows    TYPE lvc_s_row,
      it_selected_rows_cc TYPE lvc_t_row, "Carta de correção
      wa_selected_rows_cc TYPE lvc_s_row.

DATA: wa_alv_selection TYPE j_1bnfe_active,
      it_alv_selection TYPE TABLE OF j_1bnfe_active
         WITH KEY docnum,
      it_alv_error     TYPE TABLE OF j_1bnfe_active
         WITH KEY docnum   .

* Modified entries of ALV (current NF-e statuses)            "1090279
DATA wa_active_mod TYPE          j_1bnfe_active.            "1090279
DATA it_active_mod LIKE TABLE OF wa_active_mod              "1090279
                   WITH KEY      docnum.                    "1090279
* Modified entries for ALV2 (single NF-e history)            "1090279
DATA wa_nfe_alv2_new LIKE        wa_nfe_alv2.               "1090279


DATA: it_corr_parc TYPE TABLE OF ty_wa_corr_parc WITH HEADER LINE,
      wg_corr_parc TYPE ty_wa_corr_parc.

*---------- Screen 0102 - Cancellation Reason --------START--1144194--*

* Fields for screen 0102

DATA dynp_0102_no_nfe TYPE n LENGTH 10.

* ALV selection

DATA: it_selected_rows_0102 TYPE lvc_t_row,
      wa_selected_rows_0102 TYPE lvc_s_row.

DATA: gf_first_display_0102  TYPE c VALUE 'X'.
DATA: gf_first_display_0106  TYPE c VALUE 'X'.

DATA: it_alv_selection_mod TYPE TABLE OF j_1bnfe_active     "1151112
         WITH KEY docnum.                                   "1151112

* ALV display of NF-e cancellation reason

DATA: BEGIN OF wa_cancel_alv,
        status  TYPE icon-id,
        docnum  TYPE j_1bnfe_active-docnum,
        regio   TYPE j_1bnfe_active-regio,
        nfyear  TYPE j_1bnfe_active-nfyear,
        nfmonth TYPE j_1bnfe_active-nfmonth,
        stcd1   TYPE j_1bnfe_active-stcd1,
        model   TYPE j_1bnfe_active-model,
        serie   TYPE j_1bnfe_active-serie,
        nfnum9  TYPE j_1bnfe_active-nfnum9,
        docnum9 TYPE j_1bnfe_active-docnum9,
        cdv     TYPE j_1bnfe_active-cdv,
        reason  TYPE j_1bnfe_active-reason,
        reason1 TYPE j_1bnfe_active-reason1,
        reason2 TYPE j_1bnfe_active-reason2,
        reason3 TYPE j_1bnfe_active-reason3,
        reason4 TYPE j_1bnfe_active-reason4.
DATA: END OF wa_cancel_alv.

TYPES: BEGIN OF ty_documentos,
         docnum TYPE j_1bnfe_active-docnum,
       END   OF ty_documentos.

DATA: it_cancel_alv LIKE TABLE OF wa_cancel_alv
         WITH KEY docnum.


DATA: zcl_util   TYPE REF TO zcl_util.


DATA: it_nfe_active_pesq TYPE TABLE OF j_1bnfe_active,
      wa_nfe_active_pesq TYPE TABLE OF j_1bnfe_active,
      t_campos_nfe       TYPE TABLE OF zde_campos_nfe,
      w_campos_nfe       TYPE zde_campos_nfe,
      t_zcte_info_nota   TYPE TABLE OF zcte_info_nota,
      wa_zcte_info_nota  TYPE zcte_info_nota,
      t_zlest0060        TYPE TABLE OF zlest0060,
      wa_zlest0060       TYPE zlest0060,
      t_documentos       TYPE TABLE OF ty_documentos,
      wa_documentos      TYPE ty_documentos.

DATA: dt_posicao    TYPE sy-datum.

DATA: rgdoref TYPE RANGE OF j_1bdocnum.

*-----------------------------------------------------END----1144194--*
DATA: p_valor_objeto TYPE z_canc_nfe.

***********************************************************************
* Manifesto do Documento Eletrônico
***********************************************************************
DATA: gc_alv_mdfe            TYPE REF TO cl_gui_alv_grid,
      gc_container_mdfe      TYPE REF TO cl_gui_custom_container,
      gc_alv_uf_perc         TYPE REF TO cl_gui_alv_grid,
      gc_container_uf_perc   TYPE REF TO cl_gui_custom_container,
      gc_alv_hist_mdfe       TYPE REF TO cl_gui_alv_grid,
      gc_container_hist_mdfe TYPE REF TO cl_gui_custom_container,
      gc_alv_mdfe_enc        TYPE REF TO cl_gui_alv_grid,
      gc_container_mdfe_enc  TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog_mdfe      TYPE lvc_t_fcat,
      gw_catalog_mdfe      TYPE lvc_s_fcat,
      gt_catalog_uf_perc   TYPE lvc_t_fcat,
      gw_catalog_uf_perc   TYPE lvc_s_fcat,
      gt_catalog_hist_mdfe TYPE lvc_t_fcat,
      gw_catalog_hist_mdfe TYPE lvc_s_fcat,
      gt_catalog_mdfe_enc  TYPE lvc_t_fcat,
      gw_catalog_mdfe_enc  TYPE lvc_s_fcat.

DATA: it_selected_rows_uf TYPE lvc_t_row,
      wa_selected_rows_uf TYPE lvc_s_row.

CLASS: lcl_alv_toolbar_up DEFINITION DEFERRED.

DATA: c_alv_toolbarmanager_up TYPE REF TO cl_alv_grid_toolbar_manager.
DATA:       obj_toolbar_up          TYPE REF TO lcl_alv_toolbar_up.
DATA:       ty_toolbar_up           TYPE stb_button.
DATA:       wl_desactive_up.

DATA: gl_layout_mdfe      TYPE lvc_s_layo.

TYPES: BEGIN OF ty_mdfe,
         status TYPE c LENGTH 4,
         nmdfe  TYPE j_1bnfe_active-nfnum9,
         docnum TYPE j_1bnfe_active-docnum,
         ncte   TYPE j_1bnfe_active-nfnum9,
         bukrs  TYPE j_1bnfe_active-bukrs,
         branch TYPE j_1bnfe_active-branch,
       END OF ty_mdfe.

TYPES: BEGIN OF ty_uf_perc,
         uf(2) TYPE c,
       END OF ty_uf_perc,

       BEGIN OF ty_transp_mdfe,
         placa_cav(7)     TYPE c,
         placa_car1(7)    TYPE c,
         placa_car2(7)    TYPE c,
         placa_car3(7)    TYPE c,
         cd_cidade        TYPE zlest0002-cd_cidade,
         cd_uf            TYPE zlest0002-cd_uf,
         cd_renavam       TYPE zlest0002-cd_renavam,
         proprietario     TYPE zlest0002-proprietario,
         tp_veiculo       TYPE zlest0002-tp_veiculo,
         des_proprietario TYPE lfa1-name1,
         cnpj_cpf_prop    TYPE lfa1-stcd1,
         motorista        TYPE lfa1-lifnr,
         ds_motorista     TYPE lfa1-name1,
         cpf_motorista    TYPE lfa1-stcd2,
         cunid            TYPE c LENGTH 2,
         qcarga           TYPE brgew,
         vcarga           TYPE zxnfe_vcarga,
         lib_tot_carga    TYPE c,
       END OF ty_transp_mdfe.

TYPES: ty_zsdt0107 TYPE zsdt0107.

TYPES: BEGIN OF ty_hist_mdfe,
         tp_authcod	TYPE zsdt0107-tp_authcod,
         authcode	  TYPE zsdt0107-authcode,
         dt_authcod	TYPE zsdt0107-dt_authcod,
         hr_authcod	TYPE zsdt0107-hr_authcod,
         code	      TYPE zsdt0107-code,
         msg        TYPE zsdt0107-msg,
         ds_evento  TYPE string,
       END OF ty_hist_mdfe.

DATA: gt_mdfe TYPE TABLE OF ty_mdfe,
      gw_mdfe TYPE ty_mdfe.

DATA: gt_uf_perc TYPE TABLE OF ty_uf_perc,
      gw_uf_perc TYPE ty_uf_perc.

DATA: gt_transp_mdfe TYPE TABLE OF ty_transp_mdfe,
      gw_transp_mdfe TYPE ty_transp_mdfe.

DATA: gt_hist_mdfe TYPE TABLE OF ty_hist_mdfe,
      gw_hist_mdfe TYPE ty_hist_mdfe.

DATA: gt_mdfe_enc     TYPE TABLE OF zsdt0102,
      gw_mdfe_enc     TYPE zsdt0102,
      gt_mdfe_enc_aux TYPE TABLE OF zsdt0102,
      gw_mdfe_enc_aux TYPE zsdt0102.

DATA: gw_mdfetextfield TYPE j_1bnfe_active-nfnum9,
      gw_mdfe_serie    TYPE j_1bnfe_active-serie.

"MDF-e
DATA: gw_mdfe_docnum_list TYPE zsdt0102-docnum.

"Status Log
DATA: gw_mdfe_status TYPE zsdt0102.


DATA: vg_init_mdfe TYPE c,
      vg_new_mdfe  TYPE c.

***********************************************************************
* Manifesto do Documento Eletrônico - FIM
***********************************************************************

***********************************************************************
* LOCAL CLASSES
***********************************************************************
*ALRS
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  'ADD'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  'DEL'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  'CLOS_MSG'.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  "
  METHOD handle_user_command.
    DATA: tl_cte_corr_aux LIKE TABLE OF wa_cte_corr,
          wl_cte_corr     LIKE LINE  OF tl_cte_corr_aux,
          wl_lines        TYPE sy-tabix.

    REFRESH: tl_cte_corr_aux.

    CASE e_ucomm.
      WHEN 'ADD'.
        tl_cte_corr_aux[] = it_cte_corr[].
        REFRESH: it_cte_corr.
        LOOP AT tl_cte_corr_aux INTO wl_cte_corr.
          APPEND wl_cte_corr TO it_cte_corr.
        ENDLOOP.
        CLEAR: wl_cte_corr.
        APPEND wl_cte_corr TO it_cte_corr.

        CALL METHOD ctl_alv_cte_corr->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      WHEN 'DEL'.
        CALL METHOD ctl_alv_cte_corr->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          DELETE it_cte_corr INDEX wg_selectedcell-row_id-index.
        ENDLOOP.

        CALL METHOD ctl_alv_cte_corr->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
    ENDCASE.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
"ALRS


*Class Definition for ALV toolbar
*------------------------------------------------------------

CLASS lcl_alv_toolbar_up DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_data_changer FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION
*
*
CLASS lcl_alv_toolbar_up IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager_up
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: tl_parametros TYPE ustyp_t_parameters,
          l_botao_ok    TYPE char01.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    CLEAR: tl_parametros[].

    CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
      EXPORTING
        user_name           = sy-uname
*       WITH_TEXT           =
      TABLES
        user_parameters     = tl_parametros
      EXCEPTIONS
        user_name_not_exist = 1
        OTHERS              = 2.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

    " TYPE-POOLS ICON.
*    TY_TOOLBAR_UP-ICON      = ICON_CHANGE.
*    TY_TOOLBAR_UP-FUNCTION  = 'EDIT'.
*    TY_TOOLBAR_UP-DISABLED  = WL_DESACTIVE_UP.
*    TY_TOOLBAR_UP-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR_UP TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR_UP.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
    IF gw_mdfe_docnum_list IS INITIAL.
      ty_toolbar_up-icon      = icon_delete_row.
      ty_toolbar_up-function  = 'DEL'.
      ty_toolbar_up-disabled  = wl_desactive_up.
      ty_toolbar_up-butn_type = 0.
      APPEND ty_toolbar_up TO e_object->mt_toolbar.
      CLEAR ty_toolbar_up.
      l_botao_ok = abap_true.
    ELSE.
      READ TABLE tl_parametros INTO DATA(wl_parametros) WITH KEY parid = 'ZREENVIAR_DOC_GRC'.
      IF sy-subrc = 0.
        IF gw_mdfe_docnum_list IS NOT INITIAL AND  gw_mdfe_status-estornado = abap_false.
          ty_toolbar_up-icon      = icon_delete_row.
          ty_toolbar_up-function  = 'DEL'.
          ty_toolbar_up-disabled  = wl_desactive_up.
          ty_toolbar_up-butn_type = 0.
          APPEND ty_toolbar_up TO e_object->mt_toolbar.
          CLEAR ty_toolbar_up.

          ty_toolbar_up-butn_type = 3.
          APPEND ty_toolbar_up TO e_object->mt_toolbar.
          CLEAR ty_toolbar_up.

          ty_toolbar_up-icon      =  icon_system_save.
          ty_toolbar_up-function  = 'SAVE'.
          ty_toolbar_up-disabled  = wl_desactive_up.
          ty_toolbar_up-butn_type = 0.
          APPEND ty_toolbar_up TO e_object->mt_toolbar.
          CLEAR ty_toolbar_up.

          ty_toolbar_up-butn_type = 3.
          APPEND ty_toolbar_up TO e_object->mt_toolbar.
          CLEAR ty_toolbar_up.
          l_botao_ok = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

*    TY_TOOLBAR_UP-ICON      = ICON_REFRESH.
*    TY_TOOLBAR_UP-FUNCTION  = 'REN'.
*    TY_TOOLBAR_UP-DISABLED  = WL_DESACTIVE_UP.
*    TY_TOOLBAR_UP-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR_UP TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR_UP.

*    TY_TOOLBAR-ICON      = ICON_SYSTEM_SAVE.
*    TY_TOOLBAR-FUNCTION  =  'GRAVAR'.
*    TY_TOOLBAR-DISABLED  = WL_DESACTIVE_UP.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

*    TY_TOOLBAR_UP-BUTN_TYPE = 3.
*    APPEND TY_TOOLBAR_UP TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR_UP.

    IF l_botao_ok = abap_false.
      ty_toolbar_up-icon      =  icon_refresh.
      ty_toolbar_up-function  = 'REFRESH'.
      ty_toolbar_up-disabled  = wl_desactive_up.
      ty_toolbar_up-butn_type = 0.
      APPEND ty_toolbar_up TO e_object->mt_toolbar.
      CLEAR ty_toolbar_up.
    ENDIF.

    CALL METHOD c_alv_toolbarmanager_up->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: zcl_mdfe TYPE REF TO zcl_mdfe.

    CASE e_ucomm.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
      WHEN 'SAVE'.

        FREE: zcl_mdfe.
        CREATE OBJECT zcl_mdfe.

        IF gw_mdfe_docnum_list IS NOT INITIAL.
*---------Validar UFs de Percurso.
          LOOP AT gt_uf_perc INTO gw_uf_perc WHERE NOT ( uf IS INITIAL ).
            DATA(l_erro) = zcl_mdfe->set_validar_uf( gw_uf_perc-uf ).
            IF l_erro = abap_true.
              MESSAGE i024(sd) WITH 'UF informada incorreta!'.
              RETURN.
            ENDIF.
          ENDLOOP.

*---------Adiciona UFs de Percurso.
          LOOP AT gt_uf_perc INTO gw_uf_perc WHERE NOT ( uf IS INITIAL ).
            zcl_mdfe->add_uf_perc( gw_uf_perc-uf ).
          ENDLOOP.

*---------GRava UFs de Percurso.
          zcl_mdfe->set_gravar_uf_perc( gw_mdfe_docnum_list ).
        ENDIF.

        MESSAGE i024(sd) WITH 'UF Salvas!'.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

      WHEN 'EDIT'.

*        CLEAR IT_SELECTED_ROWS_UF.
*
*        CALL METHOD OBJ_GRID->GET_SELECTED_ROWS
*          IMPORTING
*            ET_INDEX_ROWS = IT_SELECTED_ROWS_UF.
*
*        IF IT_SELECTED_ROWS IS NOT INITIAL.
*
*          PERFORM CHANGE_ROWS USING 'ACTIVE'.
*          PERFORM CHANGE_CATALOGO USING 'X'.
*          WL_DESACTIVE = 'X'.
*
*        ELSE.
*          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH TEXT-M01.
*        ENDIF.
*
*        CALL METHOD OBJ_GRID->REFRESH_TABLE_DISPLAY
*          EXPORTING
*            IS_STABLE = GS_STABLE.

      WHEN 'DEL'.

*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================
*       IF NOT ( gw_mdfe_docnum_list IS INITIAL ).
*         MESSAGE 'Documento já gravado! Operação não permitida!' TYPE 'S'.
*         RETURN.
*       ENDIF.
*CONTINGENCIA MDF-E - JT - 06.05.2024 =================================

        CALL METHOD gc_alv_uf_perc->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows_uf.

        IF it_selected_rows_uf IS NOT INITIAL.

          LOOP AT it_selected_rows_uf INTO wa_selected_rows_uf.

            "READ TABLE GT_UF_PERC INTO GW_UF_PERC INDEX WA_SELECTED_ROWS_UF-INDEX.
            DELETE gt_uf_perc INDEX wa_selected_rows_uf-index.

          ENDLOOP.

        ENDIF.

        CALL METHOD gc_alv_uf_perc->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN 'REN'.

*        PERFORM SELECIONAR_DADOS.
*
*        CALL METHOD OBJ_GRID->REFRESH_TABLE_DISPLAY
*          EXPORTING
*            IS_STABLE = GS_STABLE.

    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD on_data_changer.
    DATA: ls_good       TYPE lvc_s_modi.

    " LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
    "                          INTO LS_GOOD
    "                          WHERE FIELDNAME = 'CADENCIA_QTE'.
    " ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGER

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


*---------- Definition -----------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    METHODS handle_hotspot_click
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id
                es_row_no.

ENDCLASS.                    "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0102 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler_0102 DEFINITION.                    "1144194
  PUBLIC SECTION.                                           "1144194
                                                            "1144194
    METHODS handle_hotspot_click                            "1144194
      FOR EVENT hotspot_click OF cl_gui_alv_grid            "1144194
      IMPORTING e_column_id                                 "1144194
                es_row_no.                                  "1144194
                                                            "1144194
ENDCLASS.               "lcl_event_handler_0102 DEFINITION  "1144194

DATA: event_handler      TYPE REF TO lcl_event_handler.
DATA: event_handler_0102 TYPE REF TO lcl_event_handler_0102. "1144194

*---------- Implementation -------------------------------------------*

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click
       USING es_row_no-row_id
             e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0102 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler_0102 IMPLEMENTATION.                "1144194
                                                            "1144194
  METHOD handle_hotspot_click.                              "1144194
    PERFORM handle_hotspot_click_0102                       "1144194
       USING es_row_no-row_id                               "1144194
             e_column_id-fieldname.                         "1144194
  ENDMETHOD.                    "handle_hotspot_click       "1144194
                                                            "1144194
ENDCLASS.           "lcl_event_handler_0102 IMPLEMENTATION  "1144194


*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0106 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler_0106 DEFINITION.

  PUBLIC SECTION.                                           "
    CLASS-METHODS:
      on_f4                      FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    "
ENDCLASS.               "lcl_event_handler_0102 DEFINITION  "

"DATA: EVENT_HANDLER_0106 TYPE REF TO LCL_EVENT_HANDLER_0106. "

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler_0103 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler_0106 IMPLEMENTATION.                "
  METHOD on_f4.
    TYPES: BEGIN OF t_f4_structure,
             fieldtext TYPE dfies-fieldtext,
             fieldname TYPE dfies-fieldname,
           END OF t_f4_structure.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA: ls_modi TYPE lvc_s_modi.

    CASE e_fieldname.
      WHEN 'GRUPO'.
        TYPES : BEGIN OF ty_zsdt0079,
                  grupo     TYPE zsdt0079-grupo,
                  descricao TYPE zsdt0079-descricao,
                END OF ty_zsdt0079.


        DATA: wl_return_79 TYPE  ddshretval,
              wl_dselc79   TYPE  dselc,
              tl_zsdt0079  TYPE TABLE OF ty_zsdt0079,
              wl_zsdt0079  TYPE ty_zsdt0079,
              tl_return_79 TYPE TABLE OF ddshretval,
              tl_dselc79   TYPE TABLE OF dselc,
              tabix_079    TYPE sy-tabix.

        SELECT grupo descricao
          FROM zsdt0079 AS a
          INTO TABLE tl_zsdt0079
          WHERE EXISTS (  SELECT * FROM j_1bnfe_c_ccecfg AS b WHERE b~xml_group = a~grupo ).

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'GRUPO'
            value_org       = 'S'
          " dynpprog        = sy-repid
          " dynpnr          = sy-dynnr
          " dynprofield     =
          TABLES
            value_tab       = tl_zsdt0079
            return_tab      = tl_return_79
            dynpfld_mapping = tl_dselc79.

        READ TABLE tl_return_79 INTO wl_return_79 INDEX 1.
        IF sy-subrc = 0 AND wl_return_79-fieldval <> ''.
          ASSIGN er_event_data->m_data->* TO <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'GRUPO'.
          ls_modi-value     = wl_return_79-fieldval.
          APPEND ls_modi TO <itab>.

          er_event_data->m_event_handled = 'X'.
        ENDIF.

      WHEN 'CAMPO'.


        TYPES : BEGIN OF ty_zsdt0080,
                  campo     TYPE zsdt0080-campo,
                  descricao TYPE zsdt0080-descricao,
                END OF ty_zsdt0080.


        DATA: wl_return_80 TYPE  ddshretval,
              wl_dselc80   TYPE  dselc,
              tl_zsdt0080  TYPE TABLE OF ty_zsdt0080,
              wl_zsdt0080  TYPE ty_zsdt0080,
              tl_return_80 TYPE TABLE OF ddshretval,
              tl_dselc80   TYPE TABLE OF dselc,
              tabix_080    TYPE sy-tabix.

        READ TABLE it_cte_corr INTO wa_cte_corr INDEX es_row_no-row_id.
        SELECT campo descricao
          FROM zsdt0080 AS a
          INTO TABLE tl_zsdt0080
          WHERE grupo = wa_cte_corr-grupo
           AND EXISTS (  SELECT * FROM j_1bnfe_c_ccecfg AS b WHERE b~xml_group = a~grupo
                                                               AND b~xml_field = a~campo  ).

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'CAMPO'
            value_org       = 'S'
          " dynpprog        = sy-repid
          " dynpnr          = sy-dynnr
          " dynprofield     =
          TABLES
            value_tab       = tl_zsdt0080
            return_tab      = tl_return_80
            dynpfld_mapping = tl_dselc80.

        READ TABLE tl_return_80 INTO wl_return_80 INDEX 1.
        IF sy-subrc = 0 AND wl_return_80-fieldval <> ''.
          ASSIGN er_event_data->m_data->* TO <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'CAMPO'.
          ls_modi-value     = wl_return_80-fieldval.
          APPEND ls_modi TO <itab>.

          er_event_data->m_event_handled = 'X'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "ON_F4
  "

  METHOD on_data_changed.

    DATA: t_fieldcatalog TYPE lvc_t_fcat,
          t_fieldcat_edt TYPE lvc_t_fcat,
          w_fieldcatalog TYPE lvc_s_fcat,
          ls_good        TYPE lvc_s_modi,
          ls_good_aux    TYPE lvc_s_modi,
          lv_value       TYPE lvc_value,
          lv_value_aux   TYPE lvc_value,
          v_exist_grupo  TYPE c,
          v_tabix        TYPE sy-tabix,
          v_row          TYPE lvc_s_row,
          v_col          TYPE lvc_s_col.

    CLEAR v_tabix.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      READ TABLE it_cte_corr INTO wa_cte_corr INDEX ls_good-row_id.

      v_tabix = ls_good-row_id.

      CALL METHOD ctl_alv_cte_corr->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog INTO w_fieldcatalog.

        CASE w_fieldcatalog-fieldname.
          WHEN 'GRUPO'.
            w_fieldcatalog-col_pos = 1.
          WHEN 'CAMPO'.
            w_fieldcatalog-col_pos = 2.
          WHEN 'PC_VEICULO'.
            w_fieldcatalog-col_pos = 3.
            IF ( ( wa_cte_corr-grupo EQ 'veic' ) OR
                 ( wa_cte_corr-grupo EQ 'moto' ) OR
                 ( wa_cte_corr-grupo EQ 'infQ' ) ).
              w_fieldcatalog-no_out = space.
            ELSE.
              "Verifica se tem algum item/linha da carta de correção
              "com os grupos acima, caso não tenha, retira coluna do alv.
              CLEAR v_exist_grupo.
              LOOP AT it_cte_corr INTO st_it_cte_corr WHERE ( ( grupo EQ 'veic' ) OR
                                                              ( grupo EQ 'moto' ) OR
                                                              ( grupo EQ 'infQ' ) ).

                v_exist_grupo = 'X'.
                EXIT.
              ENDLOOP.
              IF v_exist_grupo IS INITIAL.
                w_fieldcatalog-no_out = 'X'.
              ENDIF.
            ENDIF.

          WHEN 'CHAVE' .
            w_fieldcatalog-col_pos = 4.
            IF ( wa_cte_corr-grupo EQ 'infNFe' ) OR
               ( wa_cte_corr-grupo EQ 'infUnidTransp' ).
              w_fieldcatalog-no_out = space.
            ELSE.
              "Verifica se tem algum item/linha da carta de correção
              "com os grupos acima, caso não tenha, retira coluna do alv.
              CLEAR v_exist_grupo.
              LOOP AT it_cte_corr INTO st_it_cte_corr WHERE ( grupo EQ 'infNFe' ) OR
                                                            ( grupo EQ 'infUnidTransp' ).
                v_exist_grupo = 'X'.
                EXIT.
              ENDLOOP.
              IF v_exist_grupo IS INITIAL.
                w_fieldcatalog-no_out = 'X'.
              ENDIF.
            ENDIF.

          WHEN 'VALOR'.
            w_fieldcatalog-col_pos = 5.
          WHEN 'VALOR1'.
            w_fieldcatalog-col_pos = 6.
          WHEN 'VALOR2'.
            w_fieldcatalog-col_pos = 7.
          WHEN 'VALOR3'.
            w_fieldcatalog-col_pos = 8.

        ENDCASE.

        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING no_out col_pos.

      ENDLOOP.

      CALL METHOD ctl_alv_cte_corr->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.

    ENDLOOP.

    IF NOT ( v_tabix IS INITIAL ).
      CALL METHOD ctl_alv_cte_corr->get_current_cell
        IMPORTING
          es_row_id = v_row
          es_col_id = v_col.

      PERFORM change_rows.

      CALL METHOD ctl_alv_cte_corr->set_current_cell_via_id
        EXPORTING
          is_row_id    = v_row
          is_column_id = v_col.
    ENDIF.



  ENDMETHOD.                    "ON_DATA_CHANGED


ENDCLASS.           "lcl_event_handler_0103 IMPLEMENTATION



AT SELECTION-SCREEN.                                        "1375894
  DATA vtexto(15) TYPE c.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'                       "1375894
    ID 'ACTVT' FIELD '03'       "display                    "1375894
    ID 'BUKRS' FIELD bukrs-low.                             "1375894
  IF sy-subrc <> 0.                                         "1375894
    SET CURSOR FIELD 'BUKRS-LOW'.                           "1375894
    MESSAGE e091(8b) WITH bukrs-low.                        "1375894
  ENDIF.                                                    "1375894
** ALRS 17.12.12
*  AUTHORITY-CHECK OBJECT 'A_S_WERK'
*     ID 'BUKRS' FIELD bukrs-low
*     ID 'WERKS' FIELD bupla-low.
*  IF sy-subrc <> 0.
*    SET CURSOR FIELD 'BUPLA-LOW'.
*    CONCATENATE  bukrs-low '-' bupla-low INTO vtexto.
*    MESSAGE e091(8b) WITH vtexto.
*  ENDIF.



*&---------------------------------------------------------------------*
*&      TOP-OF-PAGE
*&---------------------------------------------------------------------*
INITIALIZATION.

  DATA: it_docnum_filter TYPE TABLE OF j_1bnfdoc WITH HEADER LINE.

  IF sy-calld = 'X' AND ( sy-tcode = 'ZNFE' OR sy-tcode = 'ZCTE' OR sy-tcode = 'ZMDFE' OR sy-tcode = 'ZNFE_TERC' OR sy-tcode = 'ZCTE_TERC' ).
    CLEAR:   bukrs, docnum, date0,user.
    REFRESH: bukrs, docnum, date0,user.
    docnum-sign   = 'I'.
    docnum-option = 'EQ'.

    CLEAR: it_docnum_filter[].
    IMPORT it_docnum_filter FROM MEMORY ID 'IT_DOCNUM_FILTER'.

    LOOP AT it_docnum_filter.
      docnum-low = it_docnum_filter-docnum.
      APPEND docnum.
    ENDLOOP.
    DELETE FROM MEMORY ID 'IT_DOCNUM_FILTER'.

    IF it_docnum_filter[] IS INITIAL.
      GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD docnum-low.
      APPEND docnum.
    ENDIF.

    bukrs-sign   = 'I'.
    bukrs-option = 'EQ'.
    GET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD bukrs-low.
    APPEND bukrs.




    CLEAR: usercre[].
  ENDIF.

  CASE sy-tcode.
    WHEN 'ZNFE'.
      SET TITLEBAR 'TITLE_100'.
    WHEN 'ZNFE_TERC'.
      SET TITLEBAR 'TITLE_101'.
    WHEN 'ZCTE'.
      SET TITLEBAR 'TITLE_100C'.
    WHEN 'ZCTE_TERC'.
      SET TITLEBAR 'TITLE_101C'.
    WHEN 'ZMDFE'.
      SET TITLEBAR 'TITLE_107'.
  ENDCASE.


***********************************************************************
* DATA SELECTION:
***********************************************************************

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN INTO DATA(screen_wa).

    IF sy-tcode NE 'ZMDFE' AND screen_wa-group1 = 'MFE'.
      screen_wa-active = '0'.
    ENDIF.
    MODIFY SCREEN FROM screen_wa.

    IF sy-tcode NE 'ZNFE' AND screen_wa-group1 = 'NFE'.
      screen_wa-active = '0'.
    ENDIF.
    MODIFY SCREEN FROM screen_wa.

    IF sy-tcode NE 'ZCTE' AND screen_wa-group1 = 'CTE'.
      screen_wa-active = '0'.
    ENDIF.
    MODIFY SCREEN FROM screen_wa.

  ENDLOOP.


  IF sy-tcode NE 'ZNFE' AND sy-tcode NE 'ZCTE' AND sy-tcode NE 'ZMDFE'.

    LOOP AT SCREEN.
      IF screen-name = 'FORM-LOW' OR screen-name = 'FORM-HIGH'.
        screen-input  = '0'.
        screen-output = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

START-OF-SELECTION.

* Get records from data base --> J_1BNFE_ACTIVE
  PERFORM nfe_active_read.

* Check authorization profile of user
  PERFORM check_authorization_profile.

* Get Status-Code Texts
  SELECT * FROM j_1bstscodet INTO wa_code_text
     WHERE spras = sy-langu.
    APPEND wa_code_text TO it_code_text.
  ENDSELECT.

* Read texts of NF-e document status from domain in logon language
  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'J_1BNFEDOCSTATUS'
      state         = 'A'
      langu         = sy-langu
    TABLES
      dd07v_tab     = it_doc_text
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID 'J1B_NFE' TYPE 'W' NUMBER '025'.
  ENDIF.

* Read texts of NF-e process status from domain in logon language
  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'J_1BNFESCSSTATUS'
      state         = 'A'
      langu         = sy-langu
    TABLES
      dd07v_tab     = it_scs_text
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID 'J1B_NFE' TYPE 'W' NUMBER '025'.
  ENDIF.

* Read texts of cancellation reasons                        "1144194
  SELECT * FROM j_1bnfe_cancelrt INTO wa_cancelrt           "1144194
     WHERE spras = sy-langu.                                "1144194
    MOVE-CORRESPONDING wa_cancelrt TO wa_cancel_reason.     "1144194
    APPEND wa_cancel_reason TO gt_cancel_reason.            "1144194
  ENDSELECT.                                                "1144194
  CLEAR wa_cancel_reason.                                   "1162512


END-OF-SELECTION.


* ALV function not to be displayed
  wa_exclude_fcode = '&PRINT'.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = '&MB_SUM'.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = '&AVERAGE'.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = '&MB_VIEW'.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = '&MB_EXPORT'.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = '&MB_FILTER'.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = '&GRAPH'.
  APPEND wa_exclude_fcode TO it_exclude_fcode.
  wa_exclude_fcode = '&INFO'.
  APPEND wa_exclude_fcode TO it_exclude_fcode.

* ALV display options
  gs_alv_refres_cond-row = c_x.
  gs_alv_refres_cond-col = c_x.


***********************************************************************
* OUTPUT: ALV grid
***********************************************************************

* Call Screen for ALV grid

  CALL SCREEN '0100'.






***********************************************************************
* PBO Modules
***********************************************************************

******************** S C R E E N  0100 ********************************

  INCLUDE z_1bnfe_monitor_o01.

  INCLUDE z_1bnfe_monitor_o02.

  INCLUDE z_1bnfe_monitor_o03.

  INCLUDE z_1bnfe_monitor_o04.

******************** S C R E E N  0101 ********************************

  INCLUDE z_1bnfe_monitor_o05.

******************** S C R E E N  0102 ********************************

  INCLUDE z_1bnfe_monitor_o06.

  INCLUDE z_1bnfe_monitor_o07.

  INCLUDE z_1bnfe_monitor_o08.

  INCLUDE z_1bnfe_monitor_o09.


***********************************************************************
* PAI Modules
***********************************************************************

******************** S C R E E N  0100 ********************************

  INCLUDE z_1bnfe_monitor_i01.

  INCLUDE z_1bnfe_monitor_i02.

  INCLUDE z_1bnfe_monitor_i03.

  INCLUDE z_1bnfe_monitor_i04.

******************** S C R E E N  0101 ********************************

  INCLUDE z_1bnfe_monitor_i05.

******************** S C R E E N  0102 ********************************

  INCLUDE z_1bnfe_monitor_i06.

  INCLUDE z_1bnfe_monitor_i07.

  INCLUDE z_1bnfe_monitor_i08.

  INCLUDE z_1bnfe_monitor_i09.

  INCLUDE z_1bnfe_monitor_i10.


***********************************************************************
* INCLUDES for FORM ROUTINES
***********************************************************************

  INCLUDE z_1bnfe_monitor_f01.

  INCLUDE z_1bnfe_monitor_f02.

  INCLUDE z_1bnfe_monitor_f03.

  INCLUDE z_1bnfe_monitor_f04.

  INCLUDE z_1bnfe_monitor_f05.

  INCLUDE z_1bnfe_monitor_f06.

  INCLUDE z_1bnfe_monitor_f07.

  INCLUDE z_1bnfe_monitor_f08.

  INCLUDE z_1bnfe_monitor_f09.

  INCLUDE z_1bnfe_monitor_f10.

  INCLUDE z_1bnfe_monitor_f11.

  INCLUDE z_1bnfe_monitor_f12.

  INCLUDE z_1bnfe_monitor_f13.

  INCLUDE z_1bnfe_monitor_f14.

  INCLUDE z_1bnfe_monitor_f15.

  INCLUDE z_1bnfe_monitor_f16.

  INCLUDE z_1bnfe_monitor_f17.

  INCLUDE z_1bnfe_monitor_f18.

  INCLUDE z_1bnfe_monitor_f19.

  INCLUDE z_1bnfe_monitor_f20.

  INCLUDE z_1bnfe_monitor_f21.

  INCLUDE z_1bnfe_monitor_f22.

  INCLUDE z_1bnfe_monitor_f23.

  INCLUDE z_1bnfe_monitor_f24.

  INCLUDE z_1bnfe_monitor_f25.

  INCLUDE z_1bnfe_monitor_f26.

  INCLUDE z_1bnfe_monitor_f27.

  INCLUDE z_1bnfe_monitor_f28.     "Screen 0102

  INCLUDE z_1bnfe_monitor_f29.

  INCLUDE z_1bnfe_monitor_f30.

  INCLUDE z_1bnfe_monitor_f31.

  INCLUDE z_1bnfe_monitor_z01.

  INCLUDE z_1bnfe_monitor_f32.

  INCLUDE z_1bnfe_monitor_status_0103o01.

  INCLUDE z_1bnfe_monitor_f33.

  INCLUDE z_1bnfe_monitor_f34.
