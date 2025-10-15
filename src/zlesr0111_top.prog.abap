*&---------------------------------------------------------------------*
*&  Include           ZLESR0111_TOP
*&---------------------------------------------------------------------*

REPORT zlesr0111 MESSAGE-ID zcct.

TABLES: zlest0142, zlest0146, zsdt0001, zsdt0170, zlest0149, zsdt0179, zsdt0180, zsdt0181.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida_0100_01,
         butxt              TYPE t001-butxt,
         dif_peso           TYPE brgew,
         name1              TYPE lfa1-name1,
         maktx              TYPE makt-maktx,
         ic_lib_envio_parc  TYPE c LENGTH 4,
         nf_complemento     TYPE c LENGTH 4,
         nfs_complementadas TYPE c LENGTH 200,
         cnpj_dest          TYPE zlest0142-cnpj_emissor,
         cpf_dest           TYPE zlest0142-cpf_emissor,
         nr_tk_guardian     type zsdt0001-nr_tk_guardian.
         INCLUDE STRUCTURE zlest0142.
       TYPES END OF ty_saida_0100_01.

TYPES: BEGIN OF ty_zlest0146_cancel,
         chave_nfe    TYPE zlest0142-chave_nfe,
         chave_nff    TYPE zlest0142-chave_nff,
         cnpj_emissor TYPE zlest0142-cnpj_emissor,
         numero       TYPE zlest0142-numero,
         dt_emissao   TYPE zlest0142-dt_emissao.
         INCLUDE STRUCTURE zlest0146.
       TYPES END OF ty_zlest0146_cancel.

TYPES: BEGIN OF ty_saida_0100_02,
         enviada            TYPE c LENGTH 4,
         docnum             TYPE zlest0147-docnum,
         chave_nfe          TYPE zlest0142-chave_nfe,
         chave_nff          TYPE zlest0142-chave_nff,
         cnpj_emissor       TYPE zlest0142-cnpj_emissor,
         cpf_emissor        TYPE zlest0142-cpf_emissor,
         numero             TYPE zlest0142-numero,
         cfop               TYPE zlest0142-cfop,
         dt_chegada         TYPE zlest0142-dt_chegada,
         dt_emissao         TYPE zlest0142-dt_emissao,
         peso_chegada       TYPE zlest0142-peso_chegada,
         peso_fiscal        TYPE zlest0142-peso_fiscal,
         peso_transbordo    TYPE zlest0142-peso_transbordo,
         complemento        TYPE zlest0142-complemento,
         matnr              TYPE makt-matnr,
         maktx              TYPE makt-maktx,
         lib_envio_parc     TYPE zlest0142-lib_envio_parc,
         dt_lib_envio_parc  TYPE zlest0142-dt_lib_envio_parc,
         hr_lib_envio_parc  TYPE zlest0142-hr_lib_envio_parc,
         us_lib_envio_parc  TYPE zlest0142-us_lib_envio_parc,
         ic_lib_envio_parc  TYPE c LENGTH 4,
         nf_complemento     TYPE c LENGTH 4,
         nfs_complementadas TYPE c LENGTH 200,
         cnpj_dest          TYPE zlest0142-cnpj_emissor,
         cpf_dest           TYPE zlest0142-cpf_emissor,
         rowcolor           TYPE c LENGTH 4.
         INCLUDE STRUCTURE zlest0146.
       TYPES END OF ty_saida_0100_02.

TYPES: BEGIN OF ty_saida_0100_03,
         ds_nome_transpor    TYPE znom_transporte-ds_nome_transpor,
         peso_liq_total      TYPE zsdt0172-peso_liq_total,
         peso_bruto_entregue TYPE zsdt0181-peso_bruto_entregue,
         saldo_disponivel    TYPE zsdt0181-peso_bruto_entregue.
         INCLUDE STRUCTURE zsdt0170.
       TYPES END OF ty_saida_0100_03.

TYPES: BEGIN OF ty_saida_0100_04,
         id_nomeacao_tran TYPE znom_transporte-id_nomeacao_tran,
         ds_nome_transpor TYPE znom_transporte-ds_nome_transpor,
         bukrs_due        TYPE zsdt0170-bukrs,
         numero_due       TYPE zsdt0170-numero_due,
         numero_ruc       TYPE zsdt0170-numero_ruc,
         cnpj_declarante  TYPE zsdt0170-cnpj_declarante.
         INCLUDE STRUCTURE zsdt0179.
       TYPES END OF ty_saida_0100_04.

TYPES: BEGIN OF ty_saida_0100_05.
         INCLUDE STRUCTURE zsdt0170.
       TYPES END OF ty_saida_0100_05.

TYPES: BEGIN OF ty_zlest0142.
         INCLUDE STRUCTURE zlest0142.
       TYPES  END OF ty_zlest0142.

TYPES: BEGIN OF ty_msg,
         filename   TYPE sdok_filnm,
         repid      TYPE sy-repid,
         type(1)    TYPE c,
         msgno(3)   TYPE c,
         texto(255) TYPE c,
         tabix      TYPE sy-tabix,
       END OF ty_msg.

TYPES: BEGIN OF ty_inf_parid,
         cnpj_cpf TYPE j_1bstcd1,
         region   TYPE adrc-region,
       END OF ty_inf_parid.

TYPES: BEGIN OF ty_retorno,
         type(1)    TYPE c,
         msgno(3)   TYPE c,
         texto(255) TYPE c,
       END OF ty_retorno.

TYPES: BEGIN OF ty_file,
         linha(400),
       END OF ty_file.

TYPES: BEGIN OF ty_j_1bnflin,
         docnum TYPE j_1bnflin-docnum,
         matnr  TYPE j_1bnflin-matnr.
TYPES  END OF ty_j_1bnflin.

TYPES: BEGIN OF ty_path,
         p_input(100) TYPE c,
       END OF ty_path.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_finished FOR EVENT finished OF cl_gui_timer.

  PRIVATE SECTION.
ENDCLASS.

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


CLASS lcl_event_handler_0100 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.


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
      catch_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

DATA: obj_alv_0100       TYPE REF TO cl_gui_alv_grid,
      obj_container_0100 TYPE REF TO cl_gui_custom_container.

DATA: gt_catalog TYPE lvc_t_fcat,
      gw_catalog TYPE lvc_s_fcat.

DATA: obj_toolbar_0100 TYPE REF TO lcl_alv_toolbar_0100.

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

DATA: go_alarm TYPE REF TO lcl_event_receiver,
      go_clock TYPE REF TO cl_gui_timer.

DATA: vg_data_lim      TYPE erdat,
      vg_time_lim      TYPE t,
      vg_time_restante TYPE t,
      vg_tst_dif       TYPE timestamp,
      vg_tst_lim       TYPE timestamp,
      vg_tst_atual     TYPE timestamp,
      vg_time_interval TYPE i VALUE 60.

*-------------------------------------------------------------------
* Classes
*-------------------------------------------------------------------
DATA: zcl_token_siscomex    TYPE REF TO zcl_token_siscomex,
      zcl_cct_entrega_carga TYPE REF TO zcl_cct_entrega_carga.

*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida_0100_01    TYPE TABLE OF ty_saida_0100_01,
      wa_saida_0100_01    TYPE ty_saida_0100_01,
      it_saida_0100_02    TYPE TABLE OF ty_saida_0100_02,
      wa_saida_0100_02    TYPE ty_saida_0100_02,
      it_saida_0100_03    TYPE TABLE OF ty_saida_0100_03,
      wa_saida_0100_03    TYPE ty_saida_0100_03,
      it_saida_0100_04    TYPE TABLE OF ty_saida_0100_04,
      wa_saida_0100_04    TYPE ty_saida_0100_04,
      it_saida_0100_05    TYPE TABLE OF ty_saida_0100_05,
      wa_saida_0100_05    TYPE ty_saida_0100_05,
      wa_registro_entrega TYPE zde_registro_entrega,
      wa_path             TYPE ty_path,
      tg_msg              TYPE TABLE OF ty_msg WITH HEADER LINE,
      tg_file             TYPE STANDARD TABLE OF ty_file WITH HEADER LINE INITIAL SIZE 0,
      tg_0142             TYPE TABLE OF zlest0142 WITH HEADER LINE,
      tg_0142_aux         TYPE TABLE OF zlest0142 WITH HEADER LINE,
      tg_0192             TYPE TABLE OF zlest0192 WITH HEADER LINE,
      tg_zib_nfe_dist_ter TYPE TABLE OF zib_nfe_dist_ter WITH HEADER LINE,
      tg_zsdt0001         TYPE TABLE OF zsdt0001  WITH HEADER LINE,
      tg_0146             TYPE TABLE OF zlest0146 WITH HEADER LINE,
      tg_0146_cancel      TYPE TABLE OF ty_zlest0146_cancel WITH HEADER LINE,
      tg_0147             TYPE TABLE OF zlest0147 WITH HEADER LINE,
      tg_lin              TYPE TABLE OF ty_j_1bnflin WITH HEADER LINE,
      tg_0172             TYPE TABLE OF zsdt0172  WITH HEADER LINE,
      tg_0170             TYPE TABLE OF zsdt0170  WITH HEADER LINE,
      tg_znom_transporte  TYPE TABLE OF znom_transporte  WITH HEADER LINE,
      tg_0179             TYPE TABLE OF zsdt0179  WITH HEADER LINE,
      tg_0180             TYPE TABLE OF zsdt0180  WITH HEADER LINE,
      tg_0181             TYPE TABLE OF zsdt0181  WITH HEADER LINE,
      tg_lfa1             TYPE TABLE OF lfa1      WITH HEADER LINE,
      tg_makt             TYPE TABLE OF makt      WITH HEADER LINE,
      tg_j_1bnfdoc        TYPE TABLE OF j_1bnfdoc  WITH HEADER LINE,
      wg_par_cct          TYPE zlest0149.

*-------------------------------------------------------------------
* Váriaveis
*-------------------------------------------------------------------
DATA: vg_operacao        TYPE c LENGTH 20,
      var_answer         TYPE c,
      vg_not_found       TYPE c,
      vg_vis_atual       TYPE c VALUE '01',
      vg_cnpj_transp     TYPE zlest0142-cnpj_transp,
      vg_cpf_transp      TYPE zlest0142-cpf_transp,
      vg_dtini_proc_rom  TYPE sy-datum,
      vg_ucomm           TYPE sy-ucomm,
      vg_st_logon        TYPE string VALUE 'DISCONNECTED',
      vg_time_connection TYPE erzet.

DATA: v_prefix_ent   TYPE zprefix,
      v_mensagem     TYPE bapi_msg,
      t_dir_loc_f    TYPE TABLE OF sdokpath,
      t_dir_local    TYPE TABLE OF sdokpath,
      t_dir_unix     TYPE TABLE OF epsfili,
      v_file_aux     TYPE draw-filep,
      v_file_aux2    TYPE draw-filep,
      it_xml_forn    TYPE TABLE OF zxml,
      wa_xml_forn    TYPE zxml,
      vg_bloq_filial TYPE c,
      variante       LIKE disvariant,
      vg_repid       LIKE sy-repid.

*-------------------------------------------------------------------
* Constantes
*-------------------------------------------------------------------
CONSTANTS: c_novo                 TYPE c VALUE  'NOVO'                  LENGTH 4,
           c_change               TYPE c VALUE  'CHANGE'                LENGTH 6,
           c_connected            TYPE c VALUE  'CONNECTED'             LENGTH 9,
           c_disconnected         TYPE c VALUE  'DISCONNECTED'          LENGTH 12,
           c_entrega_novo         TYPE c VALUE  '1'                     LENGTH 1,
           c_entrega_change       TYPE c VALUE  '2'                     LENGTH 1,
           c_entrega_view         TYPE c VALUE  '3'                     LENGTH 1,
           c_lib_envio_carga_parc TYPE c VALUE  'LIB_ENVIO_CARGA_PARC'  LENGTH 50,
           c_log_cancel           TYPE c VALUE  'LOG_CANCEL'            LENGTH 50,
           c_doc_rat_recepcao     TYPE c VALUE  'DOC_RAT_RECEPCAO'      LENGTH 50,
           c_down_xml_rc_nfe      TYPE c VALUE  'DOWN_XML_RC_NFE'       LENGTH 50,
           c_def_cnpj_cpf_transp  TYPE c VALUE  'DEF_CNPJ_CPF_TRANSP'   LENGTH 50,
           c_reproc_rom           TYPE c VALUE  'REPROC_ROM'            LENGTH 50,
           c_call_web             TYPE c VALUE  'CALL_WEB'              LENGTH 50,
           c_login                TYPE c VALUE  'LOGIN'                 LENGTH 50,
           c_logout               TYPE c VALUE  'LOGOUT'                LENGTH 50,
           c_recepcionar_carga    TYPE c VALUE  'RECEPCIONAR_CARGA'     LENGTH 50,
           c_cancel_recep_carga   TYPE c VALUE  'CANCEL_RECEP_CARGA'    LENGTH 50,
           c_cancel_entrega_carga TYPE c VALUE  'CANCEL_ENTREGA_CARGA'  LENGTH 50,
           c_envio_carga          TYPE c VALUE  'ENVIO_CARGA'           LENGTH 50,
           c_disp_nfe_ajuste      TYPE c VALUE  'DISP_NFE_AJUSTE'       LENGTH 50,
           c_nova_entrega         TYPE c VALUE  'NOVA_ENTREGA'          LENGTH 50,
           c_change_entrega       TYPE c VALUE  'CHANGE_ENTREGA'        LENGTH 50,
           c_view_entrega         TYPE c VALUE  'VIEW_ENTREGA'          LENGTH 50,
           c_entregar_carga       TYPE c VALUE  'ENTREGAR_CARGA'        LENGTH 50,
           c_proc_rcc             TYPE c VALUE  'PROC_RCC'              LENGTH 50.

CONSTANTS: c_x            TYPE c VALUE 'X',
           c_log(10)      TYPE c VALUE 'LOG',
           c_proc(10)     TYPE c VALUE 'PROC',
           c_ent(10)      TYPE c VALUE 'ENT',
           c_all(3)       TYPE c VALUE 'ALL',
           c_asc(10)      TYPE c VALUE 'ASC',
           c_mask_loc(6)  TYPE c VALUE '*.txt',
           c_mask_unix(6) TYPE c VALUE '*.txt',
           c_u            TYPE c VALUE 'U',
           c_w            TYPE c VALUE 'W',
           c_l            TYPE c VALUE 'L',
           c_e            TYPE c VALUE 'E',
           c_s            TYPE c VALUE 'S'.

*-----------------------------------------------------------------------------------------------------*
*  Modo
*-----------------------------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-005.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_tp_rcc RADIOBUTTON GROUP rb1 DEFAULT 'X'  USER-COMMAND mod. "Recepcionar
SELECTION-SCREEN COMMENT 3(25) text-006 FOR FIELD  p_tp_rcc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_tp_ecg RADIOBUTTON GROUP rb1.                               "Entregar
SELECTION-SCREEN COMMENT 03(35) text-007 FOR FIELD p_tp_ecg.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.

*-----------------------------------------------------------------------------------------------------*
*  Recinto Alfandegado
*-----------------------------------------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE text-014.

SELECT-OPTIONS: p_bukrs  FOR zlest0142-bukrs_rom  NO-EXTENSION NO INTERVALS MODIF ID rec,
                p_branch FOR zlest0142-branch_rom NO-EXTENSION NO INTERVALS MODIF ID rec.

SELECTION-SCREEN END OF BLOCK b6.

*-----------------------------------------------------------------------------------------------------*
*  Visualizar
*-----------------------------------------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-010.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_vw_nf RADIOBUTTON GROUP rb2 DEFAULT 'X'      MODIF ID v1 USER-COMMAND vis.     "Notas Fiscais
SELECTION-SCREEN COMMENT 3(25) text-002 FOR FIELD  p_vw_nf MODIF ID v1.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
* PARAMETERS: P_VW_NFF RADIOBUTTON GROUP RB2                  MODIF ID V6.                      "Notas Fiscais Formulário
* SELECTION-SCREEN COMMENT 3(25) TEXT-021 FOR FIELD  P_VW_NFF MODIF ID V6.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_vw_due RADIOBUTTON GROUP rb2                  MODIF ID v2.                      "DU-e's
SELECTION-SCREEN COMMENT 3(25) text-008 FOR FIELD  p_vw_due MODIF ID v2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rc_nf RADIOBUTTON GROUP rb2                   MODIF ID v3.                     "Recepções de Carga por NF
SELECTION-SCREEN COMMENT 03(35) text-003 FOR FIELD p_rc_nf  MODIF ID v3.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
* PARAMETERS: P_RC_NFF RADIOBUTTON GROUP RB2                  MODIF ID V3.                     "Recepções de Carga por NF-f
* SELECTION-SCREEN COMMENT 03(35) TEXT-003 FOR FIELD P_RC_NFF MODIF ID V3.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_rc_due RADIOBUTTON GROUP rb2                  MODIF ID v4.                     "Recepções de Carga por DU-e
SELECTION-SCREEN COMMENT 3(35) text-009 FOR FIELD p_rc_due  MODIF ID v4.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_ec_due RADIOBUTTON GROUP rb2                  MODIF ID v5.                     "Entregas de Carga por DU-e
SELECTION-SCREEN COMMENT 3(35) text-011 FOR FIELD p_rc_due  MODIF ID v5.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b2.

*-----------------------------------------------------------------------------------------------------*
*  Carga
*-----------------------------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-001.

SELECT-OPTIONS: p_dtmov  FOR zlest0142-dt_chegada OBLIGATORY DEFAULT sy-datum      MODIF ID cg,
                p_stcd1  FOR zlest0142-cnpj_emissor                                MODIF ID cg,
                p_dtemi  FOR zlest0142-dt_emissao                                  MODIF ID cg,
                p_numnf  FOR zsdt0001-nfnum                                        MODIF ID cg,
                p_model  FOR zlest0142-model                                       MODIF ID cg,
                p_modal  FOR zlest0142-modal                                       MODIF ID cg.

SELECTION-SCREEN END OF BLOCK b3.

*-----------------------------------------------------------------------------------------------------*
* DU-e
*-----------------------------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-013.

SELECTION-SCREEN BEGIN OF BLOCK b9 WITH FRAME TITLE text-016.

SELECT-OPTIONS: p_iddue  FOR zsdt0170-id_due                            MODIF ID due,
                p_bkdue  FOR zsdt0170-bukrs  NO-EXTENSION NO INTERVALS  MODIF ID due,
                p_nrdue  FOR zsdt0170-numero_due                        MODIF ID due,
                p_nrruc  FOR zsdt0170-numero_ruc                        MODIF ID due.

SELECTION-SCREEN END OF BLOCK b9.

SELECTION-SCREEN BEGIN OF BLOCK b8 WITH FRAME TITLE text-017.

SELECT-OPTIONS: p_dcdue   FOR zsdt0170-dt_registro                      MODIF ID due,
                p_hcdue   FOR zsdt0170-hr_registro                      MODIF ID due,
                p_ucdue   FOR zsdt0170-us_registro                      MODIF ID due.

SELECTION-SCREEN END OF BLOCK b8.

SELECTION-SCREEN END OF BLOCK b5.

*-----------------------------------------------------------------------------------------------------*
*  Recepção Carga
*-----------------------------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-012.

SELECT-OPTIONS: p_dtrcc  FOR zlest0146-dt_recepcao OBLIGATORY DEFAULT sy-datum    MODIF ID rcc,
                p_usrcc  FOR zlest0146-us_recepcao NO-EXTENSION NO INTERVALS      MODIF ID rcc.

SELECTION-SCREEN END OF BLOCK b4.

*-----------------------------------------------------------------------------------------------------*
*  Entrega Carga
*-----------------------------------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b7 WITH FRAME TITLE text-015.

SELECT-OPTIONS: p_dtent  FOR zsdt0179-dt_entrega OBLIGATORY DEFAULT sy-datum    MODIF ID ent,
                p_usent  FOR zsdt0179-us_entrega NO-EXTENSION NO INTERVALS      MODIF ID ent.

SELECTION-SCREEN END OF BLOCK b7.

*-----------------------------------------------------------------------------------------------------*
*  Layout
*-----------------------------------------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b10 WITH FRAME TITLE text-020.
PARAMETER: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b10.



*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid          = sy-repid.
  variante-report   = vg_repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
    MOVE variante-variant TO gs_variant-variant.
  ENDIF.


AT SELECTION-SCREEN OUTPUT.

  "SET PF-STATUS 'PF1000'.

  IF vg_ucomm EQ 'MOD'.
    CASE abap_true.
      WHEN p_tp_rcc. "Recepcionar Carga
        p_vw_nf  = abap_true.
        "P_VW_NFF = ABAP_FALSE.
        p_vw_due = abap_false.
        p_rc_nf  = abap_false.
        p_rc_due = abap_false.
        p_ec_due = abap_false.
      WHEN p_tp_ecg. "Entregar Carga
        p_vw_nf  = abap_false.
        "P_VW_NFF = ABAP_FALSE.
        p_vw_due = abap_true.
        p_rc_nf  = abap_false.
        p_rc_due = abap_false.
        p_ec_due = abap_false.
    ENDCASE.
  ENDIF.

  CLEAR: vg_ucomm.

  LOOP AT SCREEN.
    CHECK screen-group1 IS NOT INITIAL.

    CASE abap_true.
      WHEN p_tp_rcc. "Recepcionar Carga
        IF screen-group1 = 'V1' OR
           screen-group1 = 'V2' OR
           screen-group1 = 'V3' OR
           screen-group1 = 'V4' OR
           screen-group1 = 'V6'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
      WHEN p_tp_ecg. "Entregar Carga
        IF screen-group1 = 'V2' OR
           screen-group1 = 'V5'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
    ENDCASE.

    CASE screen-group1.
      WHEN 'DUE'.
        IF ( p_vw_due EQ abap_true ) OR
           ( p_ec_due EQ abap_true ) OR
           ( p_rc_due EQ abap_true ).
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
      WHEN 'CG'.
        IF ( p_vw_nf  EQ abap_true ) OR
           "( P_VW_NFF EQ ABAP_TRUE ) OR
           ( p_rc_nf  EQ abap_true ).
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
      WHEN 'RCC'.
        IF ( p_rc_nf    EQ abap_true ) OR
           ( p_rc_due   EQ abap_true ).
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
      WHEN 'ENT'.
        IF ( p_ec_due EQ abap_true ).
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
      WHEN 'REC'.
        IF ( p_tp_rcc EQ abap_true ) OR  "Recepcionar Carga
           ( p_tp_ecg EQ abap_true ).    "Entregar Carga
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.

    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

AT SELECTION-SCREEN.

  vg_ucomm = sy-ucomm. "CAPTURE USER COMMAND

  IF p_varia IS INITIAL.
    CLEAR: gs_variant.
  ENDIF.

  CASE vg_ucomm.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'ONLI'.
      PERFORM f_executar.
  ENDCASE.
