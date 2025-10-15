*&---------------------------------------------------------------------*
*& Report  ZMMR158
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr158  MESSAGE-ID zcarga.

TABLES: zmmt0122, zsdt0001, zfiwrt0008, mard.

TYPES: BEGIN OF ty_saida,
         status          TYPE v_icon-name,
         doc_znfw        TYPE zfiwrt0008-seq_lcto,
         nr_romaneio     TYPE zfiwrt0008-nr_romaneio,
         dt_saida        TYPE zfiwrt0008-budat,
         ped_compra      TYPE zfiwrt0008-ebeln,
         doc_mt_mv       TYPE zfiwrt0008-mblnr,
         doc_num         TYPE zfiwrt0008-docnum,
         nro_nfe         TYPE j_1bnfdoc-nfenum,
         dep_origem      TYPE zfiwrt0009-lgort,
         dep_destino     TYPE zfiwrt0008-move_stloc,
         mat_rev         TYPE zfiwrt0009-matnr,
         descricao       TYPE makt-maktx,
         peso_fiscal     TYPE zfiwrt0009-menge,
         unidade         TYPE zfiwrt0009-meins,
         vlr_nfe         TYPE zfiwrt0009-netwr,
         cfop            TYPE zfiwrt0009-cfop,
         placa_cav       TYPE zsdt0001-placa_cav,
         doc_trans       TYPE zsdt0001-doc_transp,
         doc_custo       TYPE zsdt0001-fknum,
         ov_frete        TYPE zsdt0001-ov_frete,
         fat_frete       TYPE zsdt0001-fatura_frete,
         doc_num_cte     TYPE zsdt0001-nro_nf_frete,
         nro_cte         TYPE j_1bnfdoc-nfenum, "VARIÁVEL              XNROCTE,
         motorista       TYPE zsdt0001-motorista,
         desc_motorista  TYPE lfa1-name1,
         doc_mat_mv315   TYPE v_icon-name, "ZFIWRT0008-MBLNR,
         doc_mat_mv309   TYPE v_icon-name, "ZFIWRT0008-MBLNR,
*---> 14/06/2023 - Migração S4 - JS
*         mat_prov        TYPE mara-normt,
*<--- 14/06/2023 - Migração S4 - JS
         mat_prov        TYPE mara-matnr,
         desc_prov       TYPE makt-maktx,
         erro_log(250)   TYPE c,

         tp_mov_et       TYPE zmmt0122-tp_mov_et,
         tp_mov_mat      TYPE zmmt0122-tp_mov_mat,
         charg           TYPE zfiwrt0009-charg,
         doc_material_e  TYPE zsdt0001-doc_material_e,
         doc_material    TYPE zsdt0001-doc_material,
         ch_referencia   TYPE zsdt0001-ch_referencia,
         nr_romaneio_ent TYPE zsdt0001-nr_romaneio,
       END OF ty_saida.

TYPES: BEGIN OF ty_saida_opus,
         status            TYPE v_icon-name,
         dt_entrada        TYPE zsdt0001-dt_movimento,
         nr_romaneio       TYPE zsdt0001-nr_romaneio,
         doc_num           TYPE j_1bnfe_active-docnum,
         nro_nfe           TYPE j_1bnfe_active-nfnum9,
         valor_nfe         TYPE zfiwrt0009-netwr,
         peso_fiscal       TYPE zfiwrt0009-menge,
         peso_balanca      TYPE zsdt0001-peso_liq,
         peso_quebra       TYPE zsdt0001-peso_liq,
         unidade           TYPE zfiwrt0009-meins,
*---> 14/06/2023 - Migração S4 - JS
         "mat_prod          TYPE mara-normt,
         mat_prod          TYPE mara-matnr,
*<--- 14/06/2023 - Migração S4 - JS
         desc_mat_pro      TYPE makt-maktx,
         doc_mat_mv315     TYPE v_icon-name,
* Início - Sara - CS2020000884  - Agosto/2020
         doc_mat_mv315_q   TYPE v_icon-name,
* Fim - Sara - CS2020000884  - Agosto/2020
         mat_rev           TYPE zfiwrt0009-matnr,
         desc_mat_rev      TYPE makt-maktx,
         lote              TYPE zfiwrt0009-charg,
         doc_mat_mv309     TYPE v_icon-name,
         doc_znfw          TYPE j_1bnflin-refkey,
         nr_romaneio_saida TYPE zfiwrt0008-nr_romaneio,
         dt_saida          TYPE zfiwrt0008-budat,
         ped_compra        TYPE zfiwrt0008-ebeln,
         doc_mat_mv313     TYPE zfiwrt0008-mblnr,
         dep_origem        TYPE zfiwrt0009-lgort,
         dep_destino       TYPE zfiwrt0008-move_stloc,
         placa_cav         TYPE zsdt0001-placa_cav,
         log_erro(250)     TYPE c,
         doc_material_e    TYPE zsdt0001-doc_material_e,
         doc_material      TYPE zsdt0001-doc_material,
         ch_referencia     TYPE zsdt0001-ch_referencia,
         seq_lcto          TYPE zfiwrt0008-seq_lcto,
       END OF ty_saida_opus.


DATA: it_saida        TYPE TABLE OF ty_saida,
      it_saida_aux    TYPE TABLE OF ty_saida,
      wa_saida        TYPE ty_saida,
      it_saida_opus   TYPE TABLE OF ty_saida_opus,
      it_saida_op_aux TYPE TABLE OF ty_saida_opus,
      wa_saida_opus   TYPE ty_saida_opus,
      it_0008         TYPE TABLE OF zfiwrt0008,
      wa_0008         TYPE  zfiwrt0008,
      it_0009         TYPE TABLE OF zfiwrt0009,
      wa_0009         TYPE zfiwrt0009,
      it_j_1bnfdoc    TYPE TABLE OF j_1bnfdoc,
      wa_j_1bnfdoc    TYPE j_1bnfdoc,
      it_zsdt0001     TYPE TABLE OF zsdt0001,
      wa_zsdt0001     TYPE zsdt0001,
      it_lfa1         TYPE TABLE OF lfa1,
      wa_lfa1         TYPE lfa1,
      it_zmmt0122     TYPE TABLE OF zmmt0122,
      wa_zmmt0122     TYPE zmmt0122,
      it_mara         TYPE TABLE OF mara,
      wa_mara         TYPE mara,
      it_makt         TYPE TABLE OF makt,
      wa_makt         TYPE makt,
      it_t001k        TYPE TABLE OF t001k,
      wa_t001k        TYPE t001k,
      wa_zmmt0123     TYPE zmmt0123,
      wa_zfiwrt0008   TYPE zfiwrt0008.


DATA: g_custom_container      TYPE REF TO cl_gui_custom_container,
      g_grid                  TYPE REF TO cl_gui_alv_grid,
      dg_splitter_1           TYPE REF TO cl_gui_splitter_container,
      dg_parent_1             TYPE REF TO cl_gui_container,
      dg_splitter_2           TYPE REF TO cl_gui_splitter_container,
      dg_parent_2             TYPE REF TO cl_gui_container,
      dg_parent_2a            TYPE REF TO cl_gui_container,
      dg_parent_alv           TYPE REF TO cl_gui_container,
      picture                 TYPE REF TO cl_gui_picture,
      dg_dyndoc_id            TYPE REF TO cl_dd_document,
      table_element           TYPE REF TO cl_dd_table_element,
      column                  TYPE REF TO cl_dd_area,
      table_element2          TYPE REF TO cl_dd_table_element,
      column_1                TYPE REF TO cl_dd_area,
      column_2                TYPE REF TO cl_dd_area,
      dg_html_cntrl           TYPE REF TO cl_gui_html_viewer,
      sdydo_text_element(255),
      p_text_table            TYPE sdydo_text_table,
      it_fieldcat             TYPE lvc_t_fcat,
      it_fieldcat02           TYPE lvc_t_fcat,
      tl_function             TYPE ui_functions,
      wl_function             LIKE tl_function  WITH HEADER LINE,
      tg_selectedrow          TYPE lvc_t_row,
      wg_selectedrow          TYPE lvc_s_row,
      wa_layout               TYPE lvc_s_layo,
      wa_variant              TYPE disvariant,
      wa_estilo               TYPE lvc_t_styl,
      wa_stable               TYPE lvc_s_stbl VALUE 'XX',
      c_alv_toolbarmanager    TYPE REF TO cl_alv_grid_toolbar_manager,
      ty_toolbar              TYPE stb_button.

DATA: _manual,
      _opus,
      xregio       TYPE j_1bnfe_active-regio,
      xnfyear      TYPE j_1bnfe_active-nfyear,
      xnfmonth     TYPE j_1bnfe_active-nfmonth,
      xstcd1       TYPE j_1bnfe_active-stcd1,
      xmodel       TYPE j_1bnfe_active-model,
      xserie       TYPE j_1bnfe_active-serie,
      xnfnum9      TYPE j_1bnfe_active-nfnum9,
      xdocnum9     TYPE j_1bnfe_active-docnum9,
      xcdv         TYPE j_1bnfe_active-cdv,
      xdocmat315   TYPE mblnr,
* Início - Sara - CS2020000884  - Agosto/2020
      xdocmat315_q TYPE mblnr,
* Fim - Sara - CS2020000884  - Agosto/2020
      xdocmat309   TYPE mblnr,
      _seq_lcto    TYPE zfiwrt0008-seq_lcto,
      _proc_job    TYPE zmmt0122-proc_job,
      _status      TYPE  v_icon-name,
      _log         TYPE string,
      msg(100)     TYPE c,
      xerro(20)    TYPE c.

DATA: wl_header              TYPE bapi2017_gm_head_01,
      wl_headret             TYPE bapi2017_gm_head_01,
      wl_code                TYPE bapi2017_gm_code,
      wl_mblnr               TYPE zfiwrt0008-mblnr,
      wl_mjahr               TYPE mjahr,
      tl_item                TYPE TABLE OF bapi2017_gm_item_create,
      wl_item                TYPE bapi2017_gm_item_create,
      tl_return              TYPE TABLE OF bapiret2,
      tl_return2             TYPE TABLE OF bapiret2,
      wa_return              TYPE bapiret2,
      document_storno        TYPE bapi2017_gm_head_ret,
      return                 TYPE bapiret2_t,
      _materialdocument315   TYPE  bapi2017_gm_head_02-mat_doc,
* Início - Sara - CS2020000884  - Agosto/2020
      _materialdocument315_q TYPE  bapi2017_gm_head_02-mat_doc,
* Fim - Sara - CS2020000884  - Agosto/2020
      _materialdocument309   TYPE  bapi2017_gm_head_02-mat_doc,
      _matdocumentyear       TYPE  bapi2017_gm_head_02-doc_year.



SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_mov   RADIOBUTTON GROUP b1 USER-COMMAND modify_screen DEFAULT 'X',
              p_trans RADIOBUTTON GROUP b1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002. "Mov.Entrada
  SELECT-OPTIONS: p_werks   FOR zmmt0122-werks         MODIF ID t1,
                  p_lgort   FOR mard-lgort             MODIF ID t1,
                  p_dt_mov  FOR zsdt0001-dt_movimento  MODIF ID t1,
                  p_rom_et  FOR zsdt0001-nr_romaneio   MODIF ID t1,
                  p_nro_nf  FOR zsdt0001-nfnum         MODIF ID t1,
                  p_mat_re  FOR zsdt0001-matnr         MODIF ID t1,
                  p_mat_pm  FOR zsdt0001-matnr         MODIF ID t1.

  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN COMMENT /1(50) descr MODIF ID t1.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_manual RADIOBUTTON GROUP g2  MODIF ID t1. "(Sem pesagem OPUS)
    SELECTION-SCREEN COMMENT 3(20)  p_campo4   MODIF ID t1.
    PARAMETERS: p_peso   RADIOBUTTON GROUP g2  MODIF ID t1." (Com Pesagem OPUS)
    SELECTION-SCREEN COMMENT 27(20)  p_campo1  MODIF ID t1.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: r_todos RADIOBUTTON GROUP g3   MODIF ID t1. "(NFE - TODAS)
    SELECTION-SCREEN COMMENT 3(20)  p_campo2   MODIF ID t1.
    PARAMETERS: r_nfe   RADIOBUTTON GROUP g3   MODIF ID t1.
    SELECTION-SCREEN COMMENT 27(24)  p_campo3  MODIF ID t1.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK  b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002. " Em transito
  SELECT-OPTIONS: p_filial  FOR zmmt0122-werks         MODIF ID t2,
                  p_lgorts  FOR mard-lgort             MODIF ID t2, "
                  p_lgortd  FOR mard-lgort             MODIF ID t2, "
                  p_dt_m    FOR zsdt0001-dt_movimento  MODIF ID t2,
                  p_ped_c   FOR zfiwrt0008-ebeln       MODIF ID t2,
                  p_doc_nf  FOR zsdt0001-nfnum         MODIF ID t2,
                  p_doc_zw  FOR zfiwrt0008-seq_lcto    MODIF ID t2,
                  p_mt_re   FOR zsdt0001-matnr         MODIF ID t2.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-002. " Em transito
  PARAMETERS: p_campo6 DEFAULT 'E'.
SELECTION-SCREEN END OF BLOCK b4.

AT SELECTION-SCREEN OUTPUT.


  descr     = 'Entrada Estoque'.
  p_campo1  = 'Com Pesagem OPUS'.
  p_campo2  = 'NFE - Todos'.
  p_campo3  = 'NFE - Faltam Processar'.
  p_campo4  = 'Sem Pesagem OPUS'.


  PERFORM modify_screen.

INITIALIZATION.

START-OF-SELECTION.

  IF p_werks IS INITIAL .
    MESSAGE 'Favor informar Filial !' TYPE 'S'.
    EXIT.
  ENDIF.

  IF p_mov IS NOT INITIAL.

    _manual = abap_true.

    IF p_werks IS INITIAL .
      MESSAGE 'Favor informar Filial !' TYPE 'S'.
      EXIT.
    ELSEIF p_lgort IS INITIAL.
      MESSAGE 'Favor informar Deposito Destino !' TYPE 'S'.
      EXIT.
    ELSEIF p_dt_mov IS INITIAL.
      MESSAGE 'Favor informar Data Movimento !' TYPE 'S'.
      EXIT.
    ELSEIF p_dt_mov-low < '20200615'.
      MESSAGE 'Este processo somente é valido para pesquisa a partir de 15/06/2020 ' TYPE 'S'.
      EXIT.
    ELSE.

      IF p_lgort IS NOT INITIAL.
        SELECT SINGLE * FROM t001l INTO @DATA(wa_t001l)
         WHERE lgort IN @p_lgort
          AND  werks IN @p_werks.

        IF sy-subrc NE 0.
          MESSAGE 'Deposito Destino informado não existe !' TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.

      IF p_manual IS NOT INITIAL.

        PERFORM z_mov_entrada_manual.

      ELSEIF p_peso IS NOT INITIAL.

        PERFORM z_mov_entrada_opus.

      ENDIF.

      CALL SCREEN 0100.

    ENDIF.

  ELSEIF p_trans IS NOT INITIAL.
    IF p_filial IS INITIAL.
      MESSAGE 'Favor informar Filial !' TYPE 'S'.
      EXIT.
    ELSEIF p_dt_m-low  < '20200615'.
      MESSAGE 'Este processo somente é valido para pesquisa a partir de 15/06/2020 ' TYPE 'S'.
      EXIT.
    ELSE.

      IF p_lgorts IS NOT INITIAL OR p_lgortd IS NOT INITIAL.
        CLEAR wa_t001l.

        SELECT SINGLE * FROM t001l INTO wa_t001l
          WHERE lgort  IN p_lgorts
           AND  werks  IN p_filial.

        IF sy-subrc NE 0.
          MESSAGE 'Deposito Saída informado não existe !' TYPE 'S'.
          EXIT.
        ENDIF.

        CLEAR wa_t001l.
        SELECT SINGLE * FROM t001l INTO wa_t001l
          WHERE lgort IN p_lgortd
           AND  werks  IN p_filial.

        IF sy-subrc NE 0.
          MESSAGE 'Deposito Destino informado não existe !' TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.

      PERFORM z_ent_manual.

      CALL SCREEN 0100.

    ENDIF.

  ENDIF.


CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no sender.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD on_hotspot_click.

    IF p_peso IS NOT INITIAL.
      READ TABLE it_saida_opus INTO wa_saida_opus INDEX e_row_id-index.

      CASE e_column_id.
        WHEN 'STATUS'.
          IF wa_saida_opus-status NE icon_complete.

            AUTHORITY-CHECK OBJECT 'Z_PROC_EST'
            ID 'ACTVT' FIELD '01'. "Código para criar/processar AUTHORITY-CHECK OBJECT 'Z_NOME_OBJETO'

            IF sy-subrc NE 0.
              CONCATENATE  'Falta permissão para o parâmetro "Z_PROC_EST" para usuário ' sy-uname INTO msg SEPARATED BY space.
              MESSAGE msg TYPE 'I'.

            ELSE.

              wa_saida_opus-status = icon_workflow_process.
              MODIFY it_saida_opus FROM wa_saida_opus INDEX  e_row_id-index.
              CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).


              PERFORM z_proc_troca_mat_opus USING ' ' wa_saida_opus-dt_entrada
                                                      p_werks-low  CHANGING  _status _log.

              IF _status IS NOT INITIAL AND _log IS NOT INITIAL.
                wa_saida_opus-status   = _status.
                wa_saida_opus-log_erro = _log.
                MODIFY it_saida_opus FROM wa_saida_opus INDEX  e_row_id-index.
              ENDIF.
*--> CS1002304 --->
              CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).
*<-- CS1002304 ---<
            ENDIF.


          ENDIF.

        WHEN 'DOC_NUM'.
          SET PARAMETER ID 'JEF' FIELD wa_saida_opus-doc_num.
          CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

        WHEN 'MAT_PROD'.
          SET PARAMETER ID 'MAT' FIELD wa_saida_opus-mat_prod.
          SET PARAMETER ID 'MXX'  FIELD 'K'.
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

        WHEN 'MAT_REV'.
          SET PARAMETER ID 'MAT' FIELD wa_saida_opus-mat_rev.
          SET PARAMETER ID 'MXX'  FIELD 'K'.
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

        WHEN 'DOC_ZNFW'.
          SET PARAMETER ID 'SEQ' FIELD  wa_saida_opus-doc_znfw.
          CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

        WHEN 'PED_COMPRA'.
          SET PARAMETER ID 'BES' FIELD wa_saida_opus-ped_compra.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

        WHEN 'DOC_MAT_MV313'.
          IF  wa_saida_opus-doc_mat_mv313 NE icon_checked AND wa_saida_opus-doc_mat_mv313 NE icon_incomplete.
*            SET PARAMETER ID 'MBN' FIELD wa_saida_opus-doc_mat_mv313.
*            SET PARAMETER ID 'MJA' FIELD wa_saida_opus-dt_saida+0(4).
*            CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[1804812]

            CALL FUNCTION 'MIGO_DIALOG'
              EXPORTING
                i_action            = 'A04'
                i_refdoc            = 'R02'
                i_notree            = 'X'
                i_no_auth_check     = ' '
                i_deadend           = 'X'
                i_skip_first_screen = 'X'
                i_okcode            = 'OK_GO'
                i_mblnr             = CONV mblnr( wa_saida_opus-doc_mat_mv313 )
                i_mjahr             = wa_saida_opus-dt_saida+0(4).

          ENDIF.

        WHEN 'DOC_MAT_MV315'.
          IF  wa_saida_opus-doc_mat_mv315 NE icon_checked AND wa_saida_opus-doc_mat_mv315 NE icon_incomplete.
*            SET PARAMETER ID 'MBN' FIELD wa_saida_opus-doc_mat_mv315.
*            SET PARAMETER ID 'MJA' FIELD wa_saida_opus-dt_saida+0(4).
*            CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[1804812]

            CALL FUNCTION 'MIGO_DIALOG'
              EXPORTING
                i_action            = 'A04'
                i_refdoc            = 'R02'
                i_notree            = 'X'
                i_no_auth_check     = ' '
                i_deadend           = 'X'
                i_skip_first_screen = 'X'
                i_okcode            = 'OK_GO'
                i_mblnr             = CONV mblnr( wa_saida_opus-doc_mat_mv315 )
                i_mjahr             = wa_saida_opus-dt_saida+0(4).
          ENDIF.

* Início - Sara - CS2020000884  - Agosto/2020
        WHEN 'DOC_MAT_MV315_Q'.
          IF  wa_saida_opus-doc_mat_mv315_q NE icon_checked AND wa_saida_opus-doc_mat_mv315_q NE icon_incomplete.
*            SET PARAMETER ID 'MBN' FIELD wa_saida_opus-doc_mat_mv315_q.
*            SET PARAMETER ID 'MJA' FIELD wa_saida_opus-dt_saida+0(4).
*            CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[1804812]

            CALL FUNCTION 'MIGO_DIALOG'
              EXPORTING
                i_action            = 'A04'
                i_refdoc            = 'R02'
                i_notree            = 'X'
                i_no_auth_check     = ' '
                i_deadend           = 'X'
                i_skip_first_screen = 'X'
                i_okcode            = 'OK_GO'
                i_mblnr             = CONV mblnr( wa_saida_opus-doc_mat_mv315_q )
                i_mjahr             = wa_saida_opus-dt_saida+0(4).


          ENDIF.
* Fim - Sara - CS2020000884  - Agosto/2020

        WHEN 'DOC_MAT_MV309'.
          IF  wa_saida_opus-doc_mat_mv309 NE icon_checked AND wa_saida_opus-doc_mat_mv309 NE icon_incomplete.
*            SET PARAMETER ID 'MBN' FIELD wa_saida_opus-doc_mat_mv309.
*            SET PARAMETER ID 'MJA' FIELD wa_saida_opus-dt_saida+0(4).
*            CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[1804812]

            CALL FUNCTION 'MIGO_DIALOG'
              EXPORTING
                i_action            = 'A04'
                i_refdoc            = 'R02'
                i_notree            = 'X'
                i_no_auth_check     = ' '
                i_deadend           = 'X'
                i_skip_first_screen = 'X'
                i_okcode            = 'OK_GO'
                i_mblnr             = CONV mblnr( wa_saida_opus-doc_mat_mv309 )
                i_mjahr             = wa_saida_opus-dt_saida+0(4).

          ENDIF.
      ENDCASE.

    ELSEIF p_trans IS NOT INITIAL OR  p_manual IS NOT INITIAL.

      READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
      CASE e_column_id.
        WHEN 'STATUS'.
          IF p_manual IS NOT INITIAL.

            AUTHORITY-CHECK OBJECT 'Z_PROC_EST'
            ID 'ACTVT' FIELD '01'. "Código para criar/processar AUTHORITY-CHECK OBJECT 'Z_NOME_OBJETO'

            IF sy-subrc NE 0.
              CONCATENATE  'Falta permissão para o parâmetro "Z_PROC_EST" para usuário ' sy-uname INTO msg SEPARATED BY space.
              MESSAGE msg TYPE 'I'.
            ELSE.

              wa_saida-status = icon_workflow_process.
              MODIFY it_saida FROM wa_saida INDEX  e_row_id-index.
              CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).


              PERFORM z_proc_troca_mat_manual USING wa_saida
                                              CHANGING  _status _log.

              IF _status IS NOT INITIAL AND _log IS NOT INITIAL.
                wa_saida-status   = _status.
                wa_saida-erro_log = _log.
                MODIFY it_saida FROM wa_saida INDEX  e_row_id-index.
              ENDIF.

            ENDIF.
          ENDIF.

        WHEN 'MAT_REV'.
          SET PARAMETER ID 'MAT' FIELD wa_saida-mat_rev.
          SET PARAMETER ID 'MXX'  FIELD 'K'.
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

        WHEN 'MAT_PROV'.
          SET PARAMETER ID 'MAT' FIELD wa_saida-mat_prov.
          SET PARAMETER ID 'MXX'  FIELD 'K'.
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

        WHEN 'PED_COMPRA'.
          SET PARAMETER ID 'BES' FIELD wa_saida-ped_compra.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

        WHEN 'DOC_MT_MV'.
*          SET PARAMETER ID 'MBN' FIELD wa_saida-doc_mt_mv.
*          SET PARAMETER ID 'MJA' FIELD wa_saida-dt_saida+0(4).
*          CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[1804812]

          CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
              i_action            = 'A04'
              i_refdoc            = 'R02'
              i_notree            = 'X'
              i_no_auth_check     = ' '
              i_deadend           = 'X'
              i_skip_first_screen = 'X'
              i_okcode            = 'OK_GO'
              i_mblnr             = CONV mblnr( wa_saida-doc_mt_mv )
              i_mjahr             = wa_saida_opus-dt_saida+0(4).

        WHEN 'DOC_NUM'.
          SET PARAMETER ID 'JEF' FIELD wa_saida-doc_num.
          CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

        WHEN 'DOC_TRANS'.
          SET PARAMETER ID 'TNR' FIELD wa_saida-doc_trans.
          CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.

        WHEN 'DOC_CUSTO'.
          SET PARAMETER ID 'FKK' FIELD wa_saida-doc_custo.
          CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.

        WHEN 'OV_FRETE'.
          SET PARAMETER ID 'AUN' FIELD wa_saida-ov_frete.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

        WHEN 'FAT_FRETE' .
          SET PARAMETER ID 'VF' FIELD wa_saida-fat_frete.
          CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.

        WHEN 'DOC_NUM_CTE'.
          SET PARAMETER ID 'JEF' FIELD wa_saida-doc_num_cte.
          CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

        WHEN 'MOTORISTA'.
          SET PARAMETER ID 'LIF' FIELD wa_saida-motorista.
          SET PARAMETER ID 'KDY' FIELD '/110'.
          CALL TRANSACTION 'XK03' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[2265093]

        WHEN 'DOC_MAT_MV315'.
          IF  wa_saida-doc_mat_mv315 NE icon_checked AND wa_saida-doc_mat_mv315 NE icon_incomplete.
*            SET PARAMETER ID 'MBN' FIELD wa_saida-doc_mat_mv315+7(10).
*            SET PARAMETER ID 'MJA' FIELD wa_saida-dt_saida+0(4).
*            CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[1804812]

            CALL FUNCTION 'MIGO_DIALOG'
              EXPORTING
                i_action            = 'A04'
                i_refdoc            = 'R02'
                i_notree            = 'X'
                i_no_auth_check     = ' '
                i_deadend           = 'X'
                i_skip_first_screen = 'X'
                i_okcode            = 'OK_GO'
                i_mblnr             = CONV mblnr( wa_saida-doc_mat_mv315+7(10) )
                i_mjahr             = wa_saida_opus-dt_saida+0(4).

          ENDIF.

        WHEN  'DOC_MAT_MV309'.
          IF  wa_saida-doc_mat_mv309 NE icon_checked AND wa_saida-doc_mat_mv309 NE icon_incomplete.
*            SET PARAMETER ID 'MBN' FIELD wa_saida-doc_mat_mv309+7(10).
*            SET PARAMETER ID 'MJA' FIELD wa_saida-dt_saida+0(4).
*            CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN. "#EC CI_USAGE_OK[1804812]

            CALL FUNCTION 'MIGO_DIALOG'
              EXPORTING
                i_action            = 'A04'
                i_refdoc            = 'R02'
                i_notree            = 'X'
                i_no_auth_check     = ' '
                i_deadend           = 'X'
                i_skip_first_screen = 'X'
                i_okcode            = 'OK_GO'
                i_mblnr             = CONV mblnr( wa_saida-doc_mat_mv309+7(10) )
                i_mjahr             = wa_saida_opus-dt_saida+0(4).


          ENDIF.


        WHEN 'DOC_ZNFW'.
          SET PARAMETER ID 'SEQ' FIELD  wa_saida-doc_znfw.
          CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.
      ENDCASE.
    ENDIF.

    CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).

  ENDMETHOD.
ENDCLASS.

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.

    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.


CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.

  METHOD on_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_generate.
    ty_toolbar-function  = 'GERAR'.
    ty_toolbar-text      = 'Processar Entrada\Troca de Material'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_storno.
    ty_toolbar-function  = 'ESTORNO'.
    ty_toolbar-text      = 'Estorno Doc.Material'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_refresh.
    ty_toolbar-function  = 'ATUALIZAR'.
    ty_toolbar-text      = 'Refresh'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'GERAR'.
        AUTHORITY-CHECK OBJECT 'Z_PROC_EST'
        ID 'ACTVT' FIELD '01'. "Código para criar/processar AUTHORITY-CHECK OBJECT 'Z_NOME_OBJETO'

        IF sy-subrc NE 0.
          CONCATENATE  'Falta permissão para o parâmetro "Z_PROC_EST" para usuário ' sy-uname INTO msg SEPARATED BY space.
          MESSAGE msg TYPE 'I'.
        ELSE.

          IF p_peso IS NOT INITIAL.
            LOOP AT it_saida_opus INTO wa_saida_opus
               WHERE doc_material_e EQ ' '  OR doc_material EQ ' '.

              IF wa_saida_opus-status NE icon_complete.

                wa_saida_opus-status = icon_workflow_process.
                MODIFY it_saida_opus FROM wa_saida_opus.
                CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).

                DELETE FROM zmmt0123
                WHERE werks     =  p_werks-low
                  AND lgort     =  wa_saida_opus-dep_destino
                  AND nfenum    =  wa_saida_opus-nro_nfe.
                COMMIT WORK.

                PERFORM z_proc_troca_mat_opus USING ' ' wa_saida_opus-dt_entrada
                                                       p_werks-low  CHANGING _status _log.

                IF _status IS NOT INITIAL AND _log IS NOT INITIAL.
                  wa_saida_opus-status   = _status.
                  wa_saida_opus-log_erro = _log.
                  MODIFY it_saida_opus FROM wa_saida_opus.
                ENDIF.
              ENDIF.
*--> CS1002304 --->
              CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).
*<-- CS1002304 ---<

              CLEAR: wa_saida_opus,   _status, _log.

            ENDLOOP.

          ELSEIF p_manual IS NOT INITIAL.

            LOOP AT it_saida INTO wa_saida
              WHERE doc_material_e EQ ' ' OR doc_material EQ ' '.

              IF wa_saida-status NE icon_complete.

                wa_saida-status = icon_workflow_process.
                MODIFY it_saida FROM wa_saida.
                CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).

                PERFORM z_proc_troca_mat_manual USING wa_saida
                                                CHANGING  _status _log.

                IF _status IS NOT INITIAL AND _log IS NOT INITIAL.
                  wa_saida-status   = _status.
                  wa_saida-erro_log = _log.
                  MODIFY it_saida FROM wa_saida.

                ENDIF.
              ENDIF.
              CLEAR: wa_saida,   _status, _log.
            ENDLOOP.
          ENDIF.

        ENDIF.

      WHEN 'ESTORNO'.
        AUTHORITY-CHECK OBJECT 'Z_PROC_EST'
        ID 'ACTVT' FIELD '85'. "Código para estorna

        IF sy-subrc NE 0.
          CONCATENATE  'Falta permissão para o parâmetro "Z_PROC_EST" para usuário ' sy-uname INTO msg SEPARATED BY space.
          MESSAGE msg TYPE 'I'.
        ELSE.
          PERFORM z_estorno.
        ENDIF.

      WHEN 'ATUALIZAR'.

        PERFORM z_clear.

        IF p_manual IS NOT INITIAL.
          PERFORM z_mov_entrada_manual.
        ELSEIF p_peso IS NOT INITIAL.
          PERFORM z_mov_entrada_opus.
        ENDIF.
    ENDCASE.

    CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).

  ENDMETHOD.
ENDCLASS.

*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen .

  LOOP AT SCREEN.
    IF p_mov = 'X'.
      IF screen-group1 = 'T2'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
      IF screen-group1 = 'T1'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ELSE.
      IF screen-group1 = 'T1'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
      IF screen-group1 = 'T2'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_MOV_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_mov_entrada_opus.

  SELECT *
    FROM t001k INTO TABLE it_t001k
   WHERE bwkey IN p_werks.

  IF sy-subrc EQ 0.

    IF p_peso IS NOT INITIAL AND r_nfe IS NOT INITIAL.

      SELECT *
        FROM zsdt0001 INTO TABLE it_zsdt0001
        FOR ALL ENTRIES IN it_t001k
      WHERE tp_movimento    EQ 'E'
        AND dt_movimento    IN p_dt_mov
        AND bukrs           EQ it_t001k-bukrs
        AND branch          IN p_werks
        AND ( doc_material_e EQ ' ' OR doc_material EQ ' '  ).

    ELSE.

      SELECT *
        FROM zsdt0001 INTO TABLE it_zsdt0001
        FOR ALL ENTRIES IN it_t001k
      WHERE tp_movimento EQ 'E'
        AND dt_movimento IN p_dt_mov
        AND bukrs        EQ it_t001k-bukrs
        AND branch       IN p_werks.

    ENDIF.
  ENDIF.

  PERFORM z_trata_dados_opus.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_EM_TRANSITO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_ent_manual.

  SELECT *
  FROM zfiwrt0008 INTO TABLE it_0008
WHERE  seq_lcto         IN p_doc_zw
    AND branch          IN p_filial
    AND budat           IN p_dt_m
    AND docnum          IN p_doc_nf
    AND ebeln           IN p_ped_c
    AND move_stloc      IN p_lgortd
    AND doc_material_e  EQ ' '.

  IF  sy-subrc EQ 0.

    SELECT *
      FROM zfiwrt0009 INTO TABLE it_0009
      FOR ALL ENTRIES IN it_0008
     WHERE seq_lcto EQ it_0008-seq_lcto
      AND  matnr    IN p_mt_re
      AND  lgort    IN p_lgorts.

    SELECT *
      FROM j_1bnfdoc INTO TABLE it_j_1bnfdoc
       FOR ALL ENTRIES IN it_0008
     WHERE docnum EQ it_0008-docnum.

    SELECT *
      FROM zsdt0001 INTO TABLE it_zsdt0001
       FOR ALL ENTRIES IN it_0008
     WHERE ch_referencia EQ it_0008-ch_referencia.

    IF  sy-subrc EQ 0 .

      SELECT *
        FROM lfa1 INTO TABLE it_lfa1
        FOR ALL ENTRIES IN it_zsdt0001
      WHERE lifnr EQ it_zsdt0001-motorista.

      SELECT *
        FROM mara INTO TABLE it_mara
        FOR ALL ENTRIES IN it_0009
       WHERE matnr EQ   it_0009-matnr.
    ENDIF.

    SELECT *
      FROM zmmt0122 INTO TABLE it_zmmt0122
     WHERE werks IN p_filial
      AND  lgort IN p_lgortd.
  ENDIF.

  PERFORM z_trata_dados_em_transito.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATAR_DADOS_EM_TRANSITO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_trata_dados_em_transito .

  LOOP AT it_0008 INTO wa_0008.

    READ TABLE it_zmmt0122 INTO wa_zmmt0122 WITH KEY werks = p_werks-low
                                                     lgort = p_lgort-low.

    wa_saida-status       =  icon_transport.

    wa_saida-doc_znfw     =  wa_0008-seq_lcto.
    wa_saida-nr_romaneio  =  wa_0008-nr_romaneio.
    wa_saida-dt_saida     =  wa_0008-budat.
    wa_saida-ped_compra   =  wa_0008-ebeln.
    wa_saida-doc_mt_mv    =  wa_0008-mblnr.
    wa_saida-doc_num      =  wa_0008-docnum.
    wa_saida-dep_destino  =  wa_0008-move_stloc.

    READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_0008-docnum.
    IF sy-subrc EQ 0.
      wa_saida-nro_nfe = wa_j_1bnfdoc-nfenum.
    ENDIF.

    READ TABLE it_0009 INTO wa_0009 WITH KEY seq_lcto = wa_0008-seq_lcto.
    IF sy-subrc EQ 0 .

      wa_saida-peso_fiscal = wa_0009-menge.
      wa_saida-unidade     = wa_0009-meins.
      wa_saida-vlr_nfe     = wa_0009-netwr.
      wa_saida-cfop        = wa_0009-cfop.
      wa_saida-dep_origem  = wa_0009-lgort.
*---> 04/07/2023 - Migração S4 - AF
*     wa_saida-mat_rev     = wa_0009-matnr.
      wa_saida-mat_rev     = CONV #( wa_0009-matnr ).
*---> 04/07/2023 - Migração S4 - AF


      SELECT SINGLE * FROM makt INTO wa_makt  WHERE  matnr = wa_0009-matnr.
      IF sy-subrc = 0.
        wa_saida-descricao = wa_makt-maktx.
      ENDIF.

      READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY  ch_referencia = wa_0008-ch_referencia.
      IF sy-subrc EQ 0.

        wa_saida-placa_cav   = wa_zsdt0001-placa_cav.
        wa_saida-doc_trans   = wa_zsdt0001-doc_transp.
        wa_saida-doc_custo   = wa_zsdt0001-fknum.
        wa_saida-ov_frete    = wa_zsdt0001-ov_frete.
        wa_saida-fat_frete   = wa_zsdt0001-fatura_frete.
        wa_saida-doc_num_cte = wa_zsdt0001-nro_nf_frete.


        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zsdt0001-motorista.
        IF sy-subrc EQ 0.
          wa_saida-motorista      = wa_zsdt0001-motorista.
          wa_saida-desc_motorista = wa_lfa1-name1.
        ENDIF.

        SELECT SINGLE nfenum  FROM j_1bnfdoc INTO  wa_saida-nro_cte WHERE docnum EQ wa_zsdt0001-nro_nf_frete.

      ENDIF.
    ENDIF.

    APPEND wa_saida TO it_saida.
    CLEAR: wa_saida, wa_zsdt0001, wa_0008, wa_0009, wa_lfa1, wa_j_1bnfdoc.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: url(255)    TYPE c,
        p_text      TYPE sdydo_text_element,
        obg_toolbar TYPE REF TO lcl_alv_toolbar.


  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR  'TL_0100'.

  PERFORM z_cria_alv.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container EXPORTING container_name = 'CONTAINER'.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 30.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 50.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM pega_logo USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = dg_parent_alv.


    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = g_grid.

    IF _manual IS NOT INITIAL.

      SET HANDLER: obg_toolbar->on_toolbar FOR g_grid,
                   obg_toolbar->handle_user_command FOR g_grid.

    ENDIF.


    IF p_mov IS NOT INITIAL AND p_peso IS NOT INITIAL.

      SET HANDLER lcl_handler=>on_hotspot_click FOR g_grid.

      CALL METHOD g_grid->set_table_for_first_display
        EXPORTING
          i_save          = 'X'
          is_layout       = wa_layout
        CHANGING
          it_outtab       = it_saida_opus
          it_fieldcatalog = it_fieldcat02.

    ELSE.

      SET HANDLER lcl_handler=>on_hotspot_click FOR g_grid.

      CALL METHOD g_grid->set_table_for_first_display
        EXPORTING
          i_save          = 'X'
          is_layout       = wa_layout
        CHANGING
          it_outtab       = it_saida
          it_fieldcatalog = it_fieldcat.
    ENDIF.


    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'G_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        sap_style = cl_dd_document=>heading.

    IF p_trans IS NOT INITIAL.
      p_text = TEXT-003.
    ELSEIF p_mov IS NOT INITIAL AND p_manual IS NOT INITIAL.
      p_text = TEXT-004.
    ELSE.
      p_text = TEXT-005.
    ENDIF.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    PERFORM  top-of-page.

    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_2
      EXCEPTIONS
        html_display_error = 1.
  ELSE.

    CALL METHOD g_grid->refresh_table_display( is_stable = wa_stable ).

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_cria_alv.

  REFRESH: it_fieldcat, it_fieldcat02.

  IF p_trans IS NOT INITIAL.

    it_fieldcat =  VALUE lvc_t_fcat(
      ( fieldname = 'STATUS'            coltext = 'Status'            outputlen = '07'  icon = 'X'   )
      ( fieldname = 'DOC_ZNFW'          coltext = 'Doc.ZNFW'          outputlen = '10'  no_zero = 'X')
      ( fieldname = 'NR_ROMANEIO'       coltext = 'Rom.Saída'         outputlen = '10'  )
      ( fieldname = 'DT_SAIDA'          coltext = 'Dt.Saída'          outputlen = '10'  )
      ( fieldname = 'PED_COMPRA'        coltext = 'Ped.Compras'       outputlen = '12'  hotspot = 'X' )
      ( fieldname = 'DOC_MT_MV'         coltext = 'Doc.Mat.MV.313'    outputlen = '15'  hotspot = 'X' )
      ( fieldname = 'DOC_NUM'           coltext = 'Doc.Num'           outputlen = '10'  hotspot = 'X' )
      ( fieldname = 'NRO_NFE'           coltext = 'Nr.NFE'            outputlen = '10'  no_zero = 'X' )
      ( fieldname = 'DEP_ORIGEM'        coltext = 'Dep. Origem'       outputlen = '12'  )
      ( fieldname = 'DEP_DESTINO'       coltext = 'Dep. Destino'      outputlen = '12'  )
      ( fieldname = 'MAT_REV'           coltext = 'Material.Rev.'     outputlen = '12'  hotspot = 'X' no_zero = 'X' )
      ( fieldname = 'DESCRICAO'         coltext = 'Descrição'         outputlen = '25'  )
      ( fieldname = 'PESO_FISCAL'       coltext = 'Peso Fiscal'       outputlen = '15'  )
      ( fieldname = 'UNIDADE'           coltext = 'Unidade'           outputlen = '10'  )
      ( fieldname = 'VLR_NFE'           coltext = 'Vlr NFE'           outputlen = '15'  )
      ( fieldname = 'CFOP'              coltext = 'CFOP'              outputlen = '06'  )
      ( fieldname = 'PLACA_CAV'         coltext = 'Plava Cav.'        outputlen = '10'  )
      ( fieldname = 'DOC_TRANS'         coltext = 'Doc.Transp.'       outputlen = '10'  hotspot = 'X' )
      ( fieldname = 'DOC_CUSTO'         coltext = 'Doc.Custo'         outputlen = '10'  hotspot = 'X' )
      ( fieldname = 'OV_FRETE'          coltext = 'Ov.Frete'          outputlen = '10'  hotspot = 'X'  no_zero = 'X' )
      ( fieldname = 'FAT_FRETE'         coltext = 'Fat.Frete'         outputlen = '10'  hotspot = 'X' )
      ( fieldname = 'DOC_NUM_CTE'       coltext = 'Doc.Num Cte'       outputlen = '12'  hotspot = 'X' )
      ( fieldname = 'NRO_CTE'           coltext = 'Nr. CTE'           outputlen = '10'  no_zero = 'X' )
      ( fieldname = 'MOTORISTA'         coltext = 'Motorista'         outputlen = '10'  hotspot = 'X'  no_zero = 'X' )
      ( fieldname = 'DESC_MOTORISTA'    coltext = 'Nome Motorista'    outputlen = '25'  )  ).

  ELSEIF _manual IS NOT INITIAL AND p_manual IS NOT INITIAL.

    it_fieldcat =  VALUE lvc_t_fcat(
      ( fieldname = 'STATUS'            coltext = 'Status'            outputlen = '07'  icon = 'X'  hotspot = 'X'  )
      ( fieldname = 'DOC_ZNFW'          coltext = 'Doc.ZNFW'          outputlen = '10'  no_zero = 'X' hotspot = 'X'  )
      ( fieldname = 'NR_ROMANEIO'       coltext = 'Rom.Saída'         outputlen = '10'  )
      ( fieldname = 'DT_SAIDA'          coltext = 'Dt.Saída'          outputlen = '10'  )
      ( fieldname = 'PED_COMPRA'        coltext = 'Ped.Compras'       outputlen = '12'  hotspot = 'X' )
      ( fieldname = 'DOC_MT_MV'         coltext = 'Doc.Mat.MV.313'    outputlen = '15'  hotspot = 'X' )
      ( fieldname = 'DOC_MAT_MV315'     coltext = 'Doc.Mat.MV.315'    outputlen = '15'  icon = 'X' hotspot = 'X' )
      ( fieldname = 'DOC_MAT_MV309'     coltext = 'Doc.Mat.MV.319'    outputlen = '15'  icon = 'X' hotspot = 'X' )
      ( fieldname = 'DOC_NUM'           coltext = 'Doc.Num'           outputlen = '10'  hotspot = 'X' )
      ( fieldname = 'NRO_NFE'           coltext = 'Nr.NFE'            outputlen = '10'  no_zero = 'X' )
      ( fieldname = 'DEP_ORIGEM'        coltext = 'Dep. Origem'       outputlen = '12'  )
      ( fieldname = 'DEP_DESTINO'       coltext = 'Dep. Destino'      outputlen = '12'  )
      ( fieldname = 'MAT_REV'           coltext = 'Material.Rev.'     outputlen = '12'  no_zero = 'X' hotspot = 'X' )
      ( fieldname = 'DESCRICAO'         coltext = 'Descrição'         outputlen = '40'  )
      ( fieldname = 'MAT_PROV'          coltext = 'Material.Prov.'    outputlen = '12'  no_zero = 'X' hotspot = 'X' )
      ( fieldname = 'DESC_PROV'         coltext = 'Descrição'         outputlen = '40'  )
      ( fieldname = 'PESO_FISCAL'       coltext = 'Peso Fiscal'       outputlen = '15'  )
      ( fieldname = 'UNIDADE'           coltext = 'Unidade'           outputlen = '10'  )
      ( fieldname = 'VLR_NFE'           coltext = 'Vlr NFE'           outputlen = '15'  )
      ( fieldname = 'CFOP'              coltext = 'CFOP'              outputlen = '06'  )
      ( fieldname = 'PLACA_CAV'         coltext = 'Plava Cav.'        outputlen = '10'  )
      ( fieldname = 'DOC_TRANS'         coltext = 'Doc.Transp.'       outputlen = '10'  hotspot = 'X' )
      ( fieldname = 'DOC_CUSTO'         coltext = 'Doc.Custo'         outputlen = '10'  hotspot = 'X' )
      ( fieldname = 'OV_FRETE'          coltext = 'Ov.Frete'          outputlen = '10'  hotspot = 'X'  no_zero = 'X' )
      ( fieldname = 'FAT_FRETE'         coltext = 'Fat.Frete'         outputlen = '10'  hotspot = 'X' )
      ( fieldname = 'DOC_NUM_CTE'       coltext = 'Doc.Num Cte'       outputlen = '12'  hotspot = 'X' )
      ( fieldname = 'NRO_CTE'           coltext = 'Nr.CTE'            outputlen = '10'  no_zero = 'X' )
      ( fieldname = 'MOTORISTA'         coltext = 'Motorista'         outputlen = '10'  hotspot = 'X'  no_zero = 'X' )
      ( fieldname = 'DESC_MOTORISTA'    coltext = 'Nome Motorista'    outputlen = '25'  )
      ( fieldname = 'ERRO_LOG'          coltext = 'Log Erro'          outputlen = '45'  ) ).

  ELSEIF _manual IS NOT INITIAL AND p_peso  IS NOT INITIAL.

    it_fieldcat02 =  VALUE lvc_t_fcat(
      ( fieldname = 'STATUS'               coltext = 'Status'                   outputlen = '07'  icon = 'X'  hotspot = 'X'  )
      ( fieldname = 'DT_ENTRADA'           coltext = 'Data Entrada'             outputlen = '10'  )
      ( fieldname = 'NR_ROMANEIO'          coltext = 'Rom.Entrada'              outputlen = '10'  )
      ( fieldname = 'DOC_NUM'              coltext = 'DocNum'                   outputlen = '10'  hotspot = 'X' )
      ( fieldname = 'NRO_NFE'              coltext = 'Nr.NFE'                   outputlen = '12'  no_zero = 'X' )
      ( fieldname = 'VALOR_NFE'            coltext = 'Valor NFe'                outputlen = '15'  )
      ( fieldname = 'PESO_FISCAL'          coltext = 'Peso Fiscal'              outputlen = '15'  icon = 'X' )
      ( fieldname = 'PESO_BALANCA'         coltext = 'Peso Balança'             outputlen = '15'  icon = 'X' )
      ( fieldname = 'PESO_QUEBRA'          coltext = 'Peso Quebra'              outputlen = '10'  )
      ( fieldname = 'UNIDADE'              coltext = 'Unidade'                  outputlen = '10'  )
      ( fieldname = 'MAT_PROD'             coltext = 'Mat.Prod.'                outputlen = '12'  no_zero = 'X' hotspot = 'X' )
      ( fieldname = 'DESC_MAT_PRO'         coltext = 'Desc.Material Producao'   outputlen = '40'  )
      ( fieldname = 'DOC_MAT_MV315'        coltext = 'Doc.Mat.MV.315'           outputlen = '14'  hotspot = 'X' )
* Início - Sara - CS2020000884  - Agosto/2020
      ( fieldname = 'DOC_MAT_MV315_Q'      coltext = 'Doc.Mat.Quebra'           outputlen = '14'  hotspot = 'X' )
* Fim - Sara - CS2020000884  - Agosto/2020
      ( fieldname = 'MAT_REV'              coltext = 'Mat.Revenda'              outputlen = '12'  no_zero = 'X' hotspot = 'X' )
      ( fieldname = 'DESC_MAT_REV'         coltext = 'Desc.Material Revenda'    outputlen = '40'  )
      ( fieldname = 'LOTE'                 coltext = 'Lote'                     outputlen = '08'  )
      ( fieldname = 'DOC_MAT_MV309'        coltext = 'Doc.Mat.MV 309'           outputlen = '14'  hotspot = 'X' )
      ( fieldname = 'DOC_ZNFW'             coltext = 'Doc.ZNFW'                 outputlen = '10'  no_zero = 'X' hotspot = 'X' )
      ( fieldname = 'NR_ROMANEIO_SAIDA'    coltext = 'Romaneio Saida'           outputlen = '14'  )
      ( fieldname = 'DT_SAIDA'             coltext = 'Data Saida'               outputlen = '10'  )
      ( fieldname = 'PED_COMPRA'           coltext = 'Ped.Compra'               outputlen = '10'  hotspot = 'X' )
      ( fieldname = 'DOC_MAT_MV313'        coltext = 'Doc.Mat.MV. 313'          outputlen = '14'  hotspot = 'X' )
      ( fieldname = 'DEP_ORIGEM'           coltext = 'Deposito Origem'          outputlen = '14'  )
      ( fieldname = 'DEP_DESTINO'          coltext = 'Deposito Destino'         outputlen = '14'  )
      ( fieldname = 'PLACA_CAV'            coltext = 'Plava Cav.'               outputlen = '10'  )
      ( fieldname = 'LOG_ERRO'             coltext = 'Log Erro'                 outputlen = '40'  ) ).

  ENDIF.
ENDFORM.

FORM pega_logo USING nome_logo CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.
  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

  REFRESH graphic_table.

  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.

  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.

  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.

ENDFORM.

FORM top-of-page.
  DATA: _dt_mov_l(10) TYPE c,
        _dt_mov_h(10) TYPE c.

  CLEAR: p_text_table.

  sdydo_text_element =  ' '.
  APPEND sdydo_text_element TO p_text_table.

  IF p_mov  IS NOT INITIAL.

    CONCATENATE 'Filial: ' p_werks-low INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.

    CONCATENATE 'Depósito Destino: ' p_lgort-low INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.

    CONCATENATE p_dt_mov-low+6(2)  '-'  p_dt_mov-low+4(2)  '-' p_dt_mov-low+0(4)  INTO _dt_mov_l.
    CONCATENATE p_dt_mov-high+6(2) '-'  p_dt_mov-high+4(2) '-' p_dt_mov-high+0(4) INTO _dt_mov_h.

    CONCATENATE 'Data Movimento: ' _dt_mov_l '-' _dt_mov_h INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.

    IF p_peso IS NOT INITIAL.
      IF p_rom_et IS NOT INITIAL.
        CONCATENATE 'Romaneio Entrada: ' p_rom_et-low INTO sdydo_text_element SEPARATED BY space.
        APPEND sdydo_text_element TO p_text_table.
      ENDIF.
    ENDIF.

    IF p_nro_nf IS NOT INITIAL.
      CONCATENATE 'Nro.Nota Fiscal: ' p_nro_nf-low INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.

    IF p_peso IS NOT INITIAL.
      IF p_mat_pm IS NOT INITIAL.

        DATA(_mat_prima) = |{ p_mat_pm-low ALPHA = OUT }|.

        CONCATENATE 'Material Mat.Prima: ' _mat_prima INTO sdydo_text_element SEPARATED BY space.
        APPEND sdydo_text_element TO p_text_table.
      ENDIF.
    ENDIF.

    IF p_mat_re IS NOT INITIAL.
      DATA(_mat_revenda) = |{ p_mat_re-low ALPHA = OUT }|.

      CONCATENATE 'Material Mat.Revenda: ' _mat_revenda INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.

  ELSEIF p_trans IS NOT INITIAL.

    CONCATENATE 'Filial: ' p_filial-low INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.

    IF p_lgortd IS NOT INITIAL.
      CONCATENATE 'Depósito Saída: ' p_lgorts-low INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.

    IF p_lgorts IS NOT INITIAL.
      CONCATENATE 'Depósito Destino: ' p_lgortd-low INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.

    IF p_dt_m IS NOT INITIAL.
      CONCATENATE p_dt_m-low+6(2)  '-'  p_dt_m-low+4(2)  '-' p_dt_m-low+0(4)    INTO _dt_mov_l.
      CONCATENATE p_dt_m-high+6(2) '-'  p_dt_m-high+4(2) '-' p_dt_m-high+0(4)   INTO _dt_mov_h.

      CONCATENATE 'Data Movimento: ' _dt_mov_l '-' _dt_mov_h INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.

    IF p_ped_c IS NOT INITIAL.
      CONCATENATE 'Pedido de Compra: ' p_ped_c-low INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.

    IF p_doc_nf IS NOT INITIAL.
      CONCATENATE 'Doc.Num NFE: ' p_doc_nf-low INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.

    IF p_doc_zw IS NOT INITIAL.
      CONCATENATE 'Doc.ZNFW: ' p_doc_zw-low INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.

    IF p_mt_re IS NOT INITIAL.
      DATA(_mat_revenda2) = |{ p_mt_re-low ALPHA = OUT }|.
      CONCATENATE 'Material Revenda: ' _mat_revenda2 INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_PROCESS_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_proc_troca_mat_manual USING _saida TYPE ty_saida
                             CHANGING r_status TYPE v_icon-name
                                      r_log    TYPE string .

  DATA p_erro TYPE char01.
  DATA: v_typ TYPE char01.

  IF p_campo6 NE 'X'.
    v_typ = 'E'.
  ELSE.
    v_typ = 'ABCDEFGHIJKLMNOPQRS' .
  ENDIF.

  CLEAR: wl_header, wl_code, wl_mblnr, wl_mjahr, xdocmat309,  xdocmat315, p_erro.
  FREE: tl_item, tl_return.

  DELETE FROM zmmt0123
    WHERE werks      = p_werks-low
      AND lgort      = _saida-dep_destino
      AND nfenum    = _saida-nro_nfe.
  COMMIT WORK.

  IF _saida-doc_material_e = ' ' AND _saida-tp_mov_et  NE ' ' .

    wl_header-pstng_date  = sy-datum.
    wl_header-doc_date    = sy-datum.
    wl_header-header_txt  = _saida-nro_nfe.

    wl_code-gm_code       = '06'.
*---> 04/07/2023 - Migração S4 - AF
*   wl_item-material      = _saida-mat_rev.
    wl_item-material      = CONV #( _saida-mat_rev ).
*---> 04/07/2023 - Migração S4 - AF

    wl_item-plant	        =	p_werks-low.
    wl_item-stge_loc      = _saida-dep_destino.
    wl_item-batch         = _saida-charg.
    wl_item-move_type     = _saida-tp_mov_et.
    wl_item-entry_qnt     = _saida-peso_fiscal.
    APPEND wl_item TO  tl_item.
    CLEAR: wl_item.

    FREE: tl_return.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = wl_header
        goodsmvt_code    = wl_code
      IMPORTING
        materialdocument = wl_mblnr
        matdocumentyear  = wl_mjahr
      TABLES
        goodsmvt_item    = tl_item
        return           = tl_return.

    IF sy-subrc IS INITIAL.

      IF p_campo6 NE 'X'.
        v_typ = 'E'.
      ELSE.
        v_typ = 'ABCDEFGHIJKLMNOPQRSX' .
      ENDIF.

      LOOP AT tl_return  INTO wa_return  WHERE type CA v_typ.
        wa_zmmt0123-mandt           = sy-mandt.
        wa_zmmt0123-werks           = p_werks-low.
        wa_zmmt0123-lgort           = _saida-dep_destino.
        wa_zmmt0123-nfenum          = _saida-nro_nfe.
        wa_zmmt0123-dt_atualizacao  = sy-datum.
        wa_zmmt0123-type            = wa_return-type.
        wa_zmmt0123-id              = wa_return-id.
        wa_zmmt0123-message         = wa_return-message.

        MODIFY  zmmt0123 FROM wa_zmmt0123.
        CLEAR: wa_zmmt0123, wa_return.

        p_erro = 'X'.
      ENDLOOP.

      IF p_erro IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

*       xdocmat315 =  wl_mblnr.
        xdocmat315 =  CONV #( wl_mblnr ).

      ELSE.
        EXIT.
      ENDIF.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = tl_return2.

      EXIT.
    ENDIF.
  ENDIF.

  CLEAR: wl_header, wl_code, wl_mblnr, wl_mjahr, p_erro.
  FREE: tl_item, tl_return.
  IF _saida-doc_material EQ ' ' AND _saida-tp_mov_mat NE ' '.

    wl_header-pstng_date  =  sy-datum.
    wl_header-doc_date    =  sy-datum.
    wl_header-header_txt  =  _saida-nro_nfe.

    wl_code-gm_code       = '06'.
*---> 04/07/2023 - Migração S4 - AF
*   wl_item-material      =  _saida-mat_rev.
    wl_item-material      =  CONV #( _saida-mat_rev ).
*---> 04/07/2023 - Migração S4 - AF

    wl_item-plant         =  p_werks-low.
    wl_item-stge_loc      =  _saida-dep_destino.
    wl_item-batch         =  _saida-charg.
    wl_item-move_type     =  _saida-tp_mov_mat.
    wl_item-entry_qnt     =  _saida-peso_fiscal.
    wl_item-move_mat      =  _saida-mat_prov.
    wl_item-move_plant    =  p_werks-low.
    wl_item-move_stloc    =  _saida-dep_destino.
    APPEND wl_item TO  tl_item.
    CLEAR: wl_item.

    FREE: tl_return.
    CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = wl_header
        goodsmvt_code    = wl_code
      IMPORTING
        materialdocument = wl_mblnr
        matdocumentyear  = wl_mjahr
      TABLES
        goodsmvt_item    = tl_item
        return           = tl_return.

    IF sy-subrc IS INITIAL.

      IF p_campo6 NE 'X'.
        v_typ = 'E'.
      ELSE.
        v_typ = 'ABCDEFGHIJKLMNOPQRSX' .
      ENDIF.

      LOOP AT tl_return  INTO wa_return  WHERE type CA v_typ.
        wa_zmmt0123-mandt           = sy-mandt.
        wa_zmmt0123-werks           = p_werks-low.
        wa_zmmt0123-lgort           = _saida-dep_destino.
        wa_zmmt0123-nfenum          = _saida-nro_nfe.
        wa_zmmt0123-dt_atualizacao  = sy-datum.
        wa_zmmt0123-type            = wa_return-type.
        wa_zmmt0123-id              = wa_return-id.
        wa_zmmt0123-message         = wa_return-message.

        MODIFY  zmmt0123 FROM wa_zmmt0123.
        CLEAR: wa_zmmt0123, wa_return.

        p_erro = 'X'.
      ENDLOOP.

      IF p_erro IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

*---> 04/07/2023 - Migração S4 - AF
*       xdocmat309 =  wl_mblnr.
        xdocmat309 = CONV #( wl_mblnr ).
*---> 04/07/2023 - Migração S4 - AF
      ELSE.

        IF xdocmat315 IS NOT INITIAL.
          FREE: tl_return[].
          CLEAR: wl_headret.
          CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
            EXPORTING
              materialdocument = xdocmat315
              matdocumentyear  = wl_mjahr
            IMPORTING
              goodsmvt_headret = wl_headret
            TABLES
              return           = tl_return[].
        ENDIF.
        EXIT.

      ENDIF.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = tl_return2.
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE it_0008 INTO DATA(_0008) WITH KEY seq_lcto = _saida-doc_znfw.

*---> 04/07/2023 - Migração S4 - AF
*  _0008-doc_material_trm = xdocmat309.
*  _0008-doc_material_e   = xdocmat315.
  _0008-doc_material_trm = CONV #( xdocmat309 ).
  _0008-doc_material_e   = CONV #( xdocmat315 ).
*---> 04/07/2023 - Migração S4 - AF

  MODIFY zfiwrt0008 FROM _0008.
  CLEAR _0008.


  SELECT SINGLE * FROM zmmt0123 INTO wa_zmmt0123
    WHERE werks           EQ p_werks-low
    AND   lgort           EQ _saida-dep_destino
    AND   nfenum          EQ _saida-nro_nfe
    AND   dt_atualizacao  EQ sy-datum.

  IF sy-subrc EQ 0.
    r_status = icon_incomplete.
    r_log    = wa_zmmt0123-message.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATA_DADOS_OPUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_trata_dados_opus.

  REFRESH it_saida_op_aux.
*  DATA: WA_J_1BNFE_ACTIVE TYPE J_1BNFE_ACTIVE.

  LOOP AT  it_zsdt0001 INTO wa_zsdt0001.

    xregio   = wa_zsdt0001-chave_nfe+0(2).
    xnfyear  = wa_zsdt0001-chave_nfe+2(2).
    xnfmonth = wa_zsdt0001-chave_nfe+4(2).
    xstcd1   = wa_zsdt0001-chave_nfe+6(14).
    xmodel   = wa_zsdt0001-chave_nfe+20(2).
    xserie   = wa_zsdt0001-chave_nfe+22(3).
    xnfnum9  = wa_zsdt0001-chave_nfe+25(9).
    xdocnum9 = wa_zsdt0001-chave_nfe+34(9).
    xcdv     = wa_zsdt0001-chave_nfe+43(1).


    SELECT SINGLE *
      FROM j_1bnfe_active INTO @DATA(wa_j_1bnfe_active)
        WHERE docsta  EQ 1
        AND   regio   EQ @xregio
        AND   nfyear  EQ @xnfyear
        AND   nfmonth EQ @xnfmonth
        AND   stcd1   EQ @xstcd1
        AND   model   EQ @xmodel
        AND   serie   EQ @xserie
        AND   nfnum9  EQ @xnfnum9
        AND   docnum9 EQ @xdocnum9
        AND   cdv     EQ @xcdv
*-CS1105820-#RIMINI-19.06.2023-BEGIN
        AND   cancel  EQ @space.
*-CS1105820-#RIMINI-19.06.2023-END
    IF sy-subrc EQ 0.

      SELECT SINGLE *  FROM j_1bnflin INTO @DATA(wa_j_1bnflin)
       WHERE docnum EQ @wa_j_1bnfe_active-docnum
        AND  reftyp EQ 'ZW'.

      IF sy-subrc EQ 0 .



        _seq_lcto = wa_j_1bnflin-refkey.



        SELECT SINGLE *  FROM  zfiwrt0008 INTO @DATA(wa_t0008)
         WHERE seq_lcto   EQ @_seq_lcto
          AND  move_stloc IN @p_lgort.
        IF sy-subrc <> 0.
*--> CS1002304 --->
          wa_zmmt0123-mandt           = sy-mandt.
          wa_zmmt0123-werks           = wa_zmmt0122-werks.
          wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
          wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
          wa_zmmt0123-dt_atualizacao  = sy-datum.
          wa_zmmt0123-type            = 'E'.
          wa_zmmt0123-id              = 'tabela zfiwrt0008'.
          wa_zmmt0123-message         = 'Verifique Tabela de Lançamento  Fiscais NF.WRITER'.
          MODIFY  zmmt0123 FROM wa_zmmt0123.
          CLEAR: wa_zmmt0123, wa_return.
*--> CS1002304 --->
          CONTINUE.
        ENDIF.


        SELECT SINGLE * FROM zfiwrt0009 INTO @DATA(wa_t0009)
          WHERE seq_lcto EQ @_seq_lcto.

        SELECT SINGLE maktx  FROM makt INTO @DATA(xmatrev)
          WHERE matnr EQ @wa_t0009-matnr
           AND  spras EQ @sy-langu.

        SELECT SINGLE * FROM mara INTO wa_mara
          WHERE matnr EQ wa_t0009-matnr.

        IF wa_mara-normt NE ' '.

          wa_mara-normt = |{ wa_mara-normt ALPHA = IN }|.

          SELECT  SINGLE * FROM mara INTO @DATA(wa_mara2)
            WHERE matnr EQ @wa_mara-normt.   "#EC CI_FLDEXT_OK[2215424]

          SELECT SINGLE maktx FROM makt INTO @DATA(xmatprod)
            WHERE matnr EQ @wa_mara2-matnr
            AND   spras EQ @sy-langu.
        ENDIF.
      ELSE.
*--> CS1002304 --->
        wa_zmmt0123-mandt           = sy-mandt.
        wa_zmmt0123-werks           = wa_zmmt0122-werks.
        wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
        wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
        wa_zmmt0123-dt_atualizacao  = sy-datum.
        wa_zmmt0123-type            = 'E'.
        wa_zmmt0123-id              = ''.
        wa_zmmt0123-message         = 'Verifique o tipo de NFe'.
        MODIFY  zmmt0123 FROM wa_zmmt0123.
        CLEAR: wa_zmmt0123, wa_return.
*--> CS1002304 --->
        CONTINUE.
      ENDIF.

      SELECT  SINGLE * FROM zmmt0122 INTO wa_zmmt0122
       WHERE werks IN p_werks
       AND   lgort IN p_lgort.

      IF wa_zsdt0001-doc_material_e NE ' ' AND wa_zsdt0001-doc_material NE ' '.
        wa_saida_opus-status        = icon_complete.
      ELSEIF  wa_zsdt0001-doc_material_e EQ ' ' AND wa_zsdt0001-doc_material EQ ' '.
        wa_saida_opus-status        = icon_generate.
      ENDIF.

      wa_saida_opus-ch_referencia   = wa_zsdt0001-ch_referencia.
      wa_saida_opus-dt_entrada      = wa_zsdt0001-dt_movimento.
      wa_saida_opus-nr_romaneio     = wa_zsdt0001-nr_romaneio.

*---> 04/07/2023 - Migração S4 - AF
*     wa_saida_opus-doc_material_e  = wa_zsdt0001-doc_material_e.
*     wa_saida_opus-doc_material    = wa_zsdt0001-doc_material.
      wa_saida_opus-doc_material_e  = CONV #( wa_zsdt0001-doc_material_e ).
      wa_saida_opus-doc_material    = CONV #( wa_zsdt0001-doc_material ).
*---> 04/07/2023 - Migração S4 - AF
      wa_saida_opus-doc_num         = wa_j_1bnfe_active-docnum.
      wa_saida_opus-nro_nfe         = wa_j_1bnfe_active-nfnum9.
      wa_saida_opus-valor_nfe       = wa_t0009-netwr.
      wa_saida_opus-peso_fiscal     = wa_t0009-menge.
      wa_saida_opus-peso_balanca    = wa_zsdt0001-peso_liq.
      wa_saida_opus-peso_quebra     = ( wa_zsdt0001-peso_liq - wa_t0009-menge  ).
      wa_saida_opus-unidade         = wa_t0009-meins.
*---> 04/07/2023 - Migração S4 - AF
*     wa_saida_opus-mat_prod        = wa_mara2-matnr.
      wa_saida_opus-mat_prod        = CONV #( wa_mara2-matnr ).

*     wa_saida_opus-desc_mat_pro    = xmatprod.
      wa_saida_opus-desc_mat_pro    = CONV #( xmatprod ).
*---> 04/07/2023 - Migração S4 - AF



      IF wa_zsdt0001-doc_material_e EQ ' '.
        wa_saida_opus-doc_mat_mv315 = icon_incomplete.
      ELSE.
*---> 04/07/2023 - Migração S4 - AF
*       wa_saida_opus-doc_mat_mv315 = wa_zsdt0001-doc_material_e.
        wa_saida_opus-doc_mat_mv315 = CONV #( wa_zsdt0001-doc_material_e ).
*---> 04/07/2023 - Migração S4 - AF

      ENDIF.

* Início - Sara - CS2020000884  - Agosto/2020
      IF wa_t0008-doc_material_q EQ ' '.
        wa_saida_opus-doc_mat_mv315_q = icon_incomplete.
      ELSE.
*---> 04/07/2023 - Migração S4 - AF
*       wa_saida_opus-doc_mat_mv315_q = wa_t0008-doc_material_q.
        wa_saida_opus-doc_mat_mv315_q = CONV #( wa_t0008-doc_material_q ).
*---> 04/07/2023 - Migração S4 - AF

      ENDIF.
* Fim - Sara - CS2020000884  - Agosto/2020
*---> 04/07/2023 - Migração S4 - AF
*     wa_saida_opus-mat_rev      = wa_t0009-matnr.
      wa_saida_opus-mat_rev      = CONV #( wa_t0009-matnr ).
*---> 04/07/2023 - Migração S4 - AF

      wa_saida_opus-desc_mat_rev = xmatrev.
      wa_saida_opus-lote         = wa_t0009-charg.

      IF wa_zsdt0001-doc_material EQ ' ' .
        wa_saida_opus-doc_mat_mv309 = icon_incomplete.
      ELSE.
*---> 04/07/2023 - Migração S4 - AF
*       wa_saida_opus-doc_mat_mv309 = wa_zsdt0001-doc_material.
        wa_saida_opus-doc_mat_mv309 = CONV #( wa_zsdt0001-doc_material ).
*---> 04/07/2023 - Migração S4 - AF

      ENDIF.

      wa_saida_opus-doc_znfw          = wa_j_1bnflin-refkey.
      wa_saida_opus-seq_lcto          = wa_t0008-seq_lcto.
      wa_saida_opus-nr_romaneio_saida = wa_t0008-nr_romaneio.
      wa_saida_opus-dt_saida          = wa_t0008-budat.
      wa_saida_opus-ped_compra        = wa_t0008-ebeln.
*---> 04/07/2023 - Migração S4 - AF
*     wa_saida_opus-doc_mat_mv313     = wa_t0008-mblnr.
      wa_saida_opus-doc_mat_mv313     = CONV #( wa_t0008-mblnr ).
*---> 04/07/2023 - Migração S4 - AF

      wa_saida_opus-dep_destino       = wa_t0008-move_stloc.
      wa_saida_opus-dep_origem        = wa_t0009-lgort.
      wa_saida_opus-placa_cav         = wa_zsdt0001-placa_cav.

      IF wa_mara-normt EQ ' '.

        CONCATENATE 'De-Para de Material Revenda p; Material Matéria Prima, não foi realizado. Atualizar Mestre do Material ' wa_t0009-matnr
        INTO wa_saida_opus-log_erro SEPARATED BY space.

      ELSEIF wa_mara2-mtart NE 'ZROH'.

        CONCATENATE 'O Material ' wa_mara2-normt 'informado como materia prima não é do tipo ZROH. Atualizar Mestre do Material ' wa_t0009-matnr
        INTO wa_saida_opus-log_erro SEPARATED BY space.

* Início - Sara - CS2020000884  - Agosto/2020
*     ELSEIF WA_SAIDA_OPUS-DOC_MAT_MV315 EQ  ICON_INCOMPLETE AND WA_SAIDA_OPUS-DOC_MAT_MV309 EQ ICON_INCOMPLETE.
      ELSEIF wa_saida_opus-doc_mat_mv315 EQ  icon_incomplete AND wa_saida_opus-doc_mat_mv309 EQ icon_incomplete
         AND wa_saida_opus-doc_mat_mv315_q  EQ  icon_incomplete.
* Fim - Sara - CS2020000884  - Agosto/2020

        SELECT SINGLE * FROM zmmt0123 INTO @DATA(wa_zmmt0123)
          WHERE werks           IN @p_werks
          AND   lgort           IN @p_lgort
          AND   nfenum          EQ @wa_saida_opus-nro_nfe
          AND   dt_atualizacao  EQ @sy-datum.

        IF sy-subrc EQ 0.
          wa_saida_opus-status   = icon_incomplete.
          wa_saida_opus-log_erro = wa_zmmt0123-message.
        ENDIF.
      ENDIF.

      APPEND wa_saida_opus TO it_saida_op_aux.
    ENDIF.

    CLEAR: wa_zsdt0001, wa_mara, wa_zmmt0122, wa_saida_opus, wa_j_1bnfe_active, wa_j_1bnflin, wa_t0008, wa_t0009, xmatprod.
    CLEAR: xregio,  xnfyear,  xnfmonth, xstcd1, xmodel, xserie, xnfnum9, xdocnum9, xcdv, _seq_lcto, xmatprod, xmatrev.
  ENDLOOP.

  LOOP AT it_saida_op_aux INTO wa_saida_opus
   WHERE nro_nfe          IN p_nro_nf
   AND   nr_romaneio      IN p_rom_et
   AND   mat_rev          IN p_mat_re
   AND   mat_prod         IN p_mat_pm.       "#EC CI_FLDEXT_OK[2215424]

    APPEND wa_saida_opus  TO it_saida_opus.
    CLEAR wa_saida_opus.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_MOV_ENTRADA_MANUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_mov_entrada_manual .

  IF r_nfe IS NOT INITIAL.

    SELECT *
     FROM zfiwrt0008 INTO TABLE it_0008
      WHERE branch          IN p_filial
       AND  budat           IN p_dt_mov
       AND  move_stloc      IN p_lgort
       AND  doc_material_e  EQ ' '.

  ELSEIF r_todos IS NOT INITIAL.

    SELECT *
   FROM zfiwrt0008 INTO TABLE it_0008
    WHERE branch          IN p_filial
     AND  budat           IN p_dt_mov
     AND  move_stloc      IN p_lgort.

  ENDIF.

  IF  sy-subrc EQ 0.

    SELECT *
      FROM zfiwrt0009 INTO TABLE it_0009
      FOR ALL ENTRIES IN it_0008
     WHERE seq_lcto EQ it_0008-seq_lcto
      AND  matnr    IN p_mat_re.

    SELECT *
      FROM j_1bnfdoc INTO TABLE it_j_1bnfdoc
       FOR ALL ENTRIES IN it_0008
     WHERE docnum EQ it_0008-docnum
      AND  nfenum IN p_nro_nf.

    SELECT *
      FROM zsdt0001 INTO TABLE it_zsdt0001
       FOR ALL ENTRIES IN it_0008
     WHERE ch_referencia EQ it_0008-ch_referencia.

    IF  sy-subrc EQ 0 .

      SELECT *
        FROM lfa1 INTO TABLE it_lfa1
        FOR ALL ENTRIES IN it_zsdt0001
      WHERE lifnr EQ it_zsdt0001-motorista.

      SELECT *
        FROM mara INTO TABLE it_mara
        FOR ALL ENTRIES IN it_0009
       WHERE matnr EQ   it_0009-matnr.

    ENDIF.

    SELECT *
      FROM zmmt0122 INTO TABLE it_zmmt0122
     WHERE werks IN p_werks
      AND  lgort IN p_lgort.

  ENDIF.

  PERFORM z_trata_mov_manual.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_TRATA_MOV_MANUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_trata_mov_manual .

  LOOP AT it_0008 INTO wa_0008.

    READ TABLE it_zmmt0122 INTO wa_zmmt0122 WITH KEY werks = p_werks-low
                                                     lgort = p_lgort-low.
    wa_saida-doc_znfw     =  wa_0008-seq_lcto.
    wa_saida-nr_romaneio  =  wa_0008-nr_romaneio.
    wa_saida-dt_saida     =  wa_0008-budat.
    wa_saida-ped_compra   =  wa_0008-ebeln.
*---> 04/07/2023 - Migração S4 - AF
*   wa_saida-doc_mt_mv    =  wa_0008-mblnr.
    wa_saida-doc_mt_mv    =  CONV #( wa_0008-mblnr ).
*---> 04/07/2023 - Migração S4 - AF

    wa_saida-doc_num      =  wa_0008-docnum.
    wa_saida-dep_destino  =  wa_0008-move_stloc.

    READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_0008-docnum.
    IF sy-subrc EQ 0.
      wa_saida-nro_nfe = wa_j_1bnfdoc-nfenum.
    ENDIF.

    READ TABLE it_0009 INTO wa_0009 WITH KEY seq_lcto = wa_0008-seq_lcto.
    IF sy-subrc EQ 0 .

      wa_saida-peso_fiscal = wa_0009-menge.
      wa_saida-unidade     = wa_0009-meins.
      wa_saida-vlr_nfe     = wa_0009-netwr.
      wa_saida-cfop        = wa_0009-cfop.
      wa_saida-dep_origem  = wa_0009-lgort.
*---> 04/07/2023 - Migração S4 - AF
*     wa_saida-mat_rev     = wa_0009-matnr.
      wa_saida-mat_rev     = CONV #( wa_0009-matnr ).
*---> 04/07/2023 - Migração S4 - AF

      wa_saida-charg       = wa_0009-charg.


      READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY  ch_referencia = wa_0008-ch_referencia.
      IF sy-subrc EQ 0.

        wa_saida-motorista    = wa_zsdt0001-motorista.

        READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zsdt0001-motorista.
        IF sy-subrc EQ 0.
          wa_saida-desc_motorista = wa_lfa1-name1.
        ENDIF.

        SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(wa_doc) WHERE docnum EQ @wa_zsdt0001-nro_nf_frete.
        IF sy-subrc EQ 0.
          wa_saida-nro_cte = wa_doc-nfenum.
        ENDIF.

        wa_saida-placa_cav      = wa_zsdt0001-placa_cav.
        wa_saida-doc_trans      = wa_zsdt0001-doc_transp.
        wa_saida-doc_custo      = wa_zsdt0001-fknum.
        wa_saida-ov_frete       = wa_zsdt0001-ov_frete.
        wa_saida-fat_frete      = wa_zsdt0001-fatura_frete.
        wa_saida-doc_num_cte    = wa_zsdt0001-nro_nf_frete.
        wa_saida-ch_referencia  = wa_zsdt0001-ch_referencia.
        wa_saida-nr_romaneio_ent = wa_zsdt0001-nr_romaneio.
      ENDIF.

      SELECT SINGLE maktx FROM makt INTO @DATA(xmatrev)
         WHERE matnr EQ @wa_0009-matnr
         AND   spras EQ @sy-langu.







      wa_saida-descricao =  xmatrev.





      READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_0009-matnr.
      IF wa_mara-normt NE ' '.

        wa_mara-normt = |{ wa_mara-normt ALPHA = IN }|.


        SELECT SINGLE * FROM mara INTO @DATA(wa_mara2)
          WHERE matnr EQ @wa_mara-normt.     "#EC CI_FLDEXT_OK[2215424]

        SELECT SINGLE  maktx FROM makt INTO @DATA(xmatprod)
          WHERE matnr  EQ @wa_zsdt0001-matnr
           AND   spras EQ @sy-langu.
*---> 04/07/2023 - Migração S4 - AF
*       wa_saida-mat_prov  = wa_mara2-matnr.



        wa_saida-mat_prov  = CONV #( wa_mara2-matnr ).
*---> 04/07/2023 - Migração S4 - AF

        wa_saida-desc_prov = xmatprod.
      ENDIF.
    ENDIF.

    wa_saida-tp_mov_et      = wa_zmmt0122-tp_mov_et.
    wa_saida-tp_mov_mat     = wa_zmmt0122-tp_mov_mat.


    IF wa_zmmt0122-tp_mov_et NE ' ' AND   wa_0008-doc_material_e EQ ' '.
      wa_saida-doc_mat_mv315 = icon_incomplete.
    ELSEIF wa_zmmt0122-tp_mov_et EQ ' '  OR  wa_0008-doc_material_e IS NOT INITIAL.
      CONCATENATE icon_checked '-' wa_0008-doc_material_e INTO wa_saida-doc_mat_mv315 SEPARATED BY space.
    ENDIF.

    IF wa_zmmt0122-tp_mov_mat NE ' '  AND wa_zsdt0001-doc_material EQ ' '.
      wa_saida-doc_mat_mv309 = icon_incomplete.
    ELSEIF wa_zmmt0122-tp_mov_mat EQ ' ' OR wa_zsdt0001-doc_material IS NOT INITIAL.
      CONCATENATE icon_checked '-' wa_zsdt0001-doc_material INTO wa_saida-doc_mat_mv309 SEPARATED BY space.
    ENDIF.

    IF wa_saida-doc_mat_mv315 NE icon_incomplete AND  wa_saida-doc_mat_mv309 NE icon_incomplete.
      wa_saida-status       =  icon_complete.
    ELSE.
      wa_saida-status       =  icon_generate.
    ENDIF.

    IF wa_zmmt0122-tp_mov_mat NE ' ' AND wa_mara-normt EQ ' '.
      CONCATENATE 'De - Para de Material Revenda pra Matéria prima não foi realizado. Atualizar Mestre do Material ' wa_0009-matnr  INTO  wa_saida-erro_log
      SEPARATED BY space.
    ELSEIF wa_zmmt0122-tp_mov_mat NE ' ' AND wa_mara-mtart NE 'ZROH'.
      CONCATENATE 'O Material' wa_mara-normt 'informado como materia prima não é do tipo ZROH. Atualizar Mestre do Material ' wa_0009-matnr INTO wa_saida-erro_log
      SEPARATED BY space.

    ELSEIF wa_saida-doc_mat_mv315 NE icon_incomplete  OR  wa_saida-doc_mat_mv309 NE icon_incomplete.

      SELECT SINGLE * FROM zmmt0123 INTO wa_zmmt0123
        WHERE werks           IN p_werks
        AND   lgort           IN p_lgort
        AND   nfenum          EQ wa_saida-nro_nfe
        AND   dt_atualizacao  EQ sy-datum.

      IF sy-subrc EQ 0.
        wa_saida-status   = icon_incomplete.
        wa_saida-erro_log = wa_zmmt0123-message.
      ENDIF.
    ENDIF.

    APPEND wa_saida TO it_saida_aux.
    CLEAR: wa_saida, wa_zsdt0001, wa_0008, wa_0009, wa_lfa1, wa_j_1bnfdoc, wa_zmmt0123.
  ENDLOOP.

  LOOP AT it_saida_aux INTO wa_saida
    WHERE nro_nfe          IN p_nro_nf
    AND   nr_romaneio_ent  IN p_rom_et
    AND   mat_rev          IN p_mat_re
    AND   mat_prov         IN p_mat_pm.      "#EC CI_FLDEXT_OK[2215424]

    APPEND wa_saida TO it_saida.
    CLEAR wa_saida.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_estorno .

  IF p_peso IS NOT INITIAL .

    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_index_rows = tg_selectedrow.

    IF tg_selectedrow[] IS INITIAL.
      MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
      EXIT.
    ELSE.
      READ TABLE tg_selectedrow INTO wg_selectedrow INDEX 1.

      READ TABLE it_saida_opus INTO wa_saida_opus INDEX wg_selectedrow-index.

      READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY ch_referencia = wa_saida_opus-ch_referencia.

      SELECT SINGLE *  FROM  zfiwrt0008 INTO @DATA(wa_t0008)  WHERE seq_lcto  EQ @wa_saida_opus-seq_lcto.
*---> 04/07/2023 - Migração S4 - AF
*       _materialdocument315 =  wa_saida_opus-doc_mat_mv315.
      _materialdocument315 =  CONV #( wa_saida_opus-doc_mat_mv315 ).

* Início - Sara - CS2020000884  - Agosto/2020
*       _materialdocument315_q =  wa_saida_opus-doc_mat_mv315_q.
      _materialdocument315_q =  CONV #( wa_saida_opus-doc_mat_mv315_q ).

* Fim - Sara - CS2020000884  - Agosto/2020
*       _materialdocument309 =  wa_saida_opus-doc_mat_mv309.
      _materialdocument309 =  CONV #( wa_saida_opus-doc_mat_mv309 ).
*---> 04/07/2023 - Migração S4 - AF

      _matdocumentyear     =  sy-datum+0(4).






















      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument = _materialdocument309
          matdocumentyear  = _matdocumentyear
        IMPORTING
          goodsmvt_headret = document_storno
        TABLES
          return           = return.


      READ TABLE return INTO DATA(_return) WITH KEY type = 'E'.
      IF sy-subrc = 0.
        MESSAGE _return-message TYPE 'S'.
        EXIT.
      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

        REFRESH return.

        CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
          EXPORTING
            materialdocument = _materialdocument315
            matdocumentyear  = _matdocumentyear
          IMPORTING
            goodsmvt_headret = document_storno
          TABLES
            return           = return.


        READ TABLE return INTO _return WITH KEY type = 'E'.
        IF sy-subrc = 0.
          MESSAGE _return-message TYPE 'S'.
          EXIT.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

** Inicio - 23.12.2021 - CBRAND
          wa_zsdt0001-doc_material     = ''.
          wa_zsdt0001-ano_material     = ''.
          wa_zsdt0001-doc_material_e   = ''.
          wa_zsdt0001-ano_material_e   = ''.
          MODIFY zsdt0001 FROM wa_zsdt0001.

          wa_t0008-doc_material_e = ''.
          wa_t0008-doc_material_q   = ''.
          MODIFY zfiwrt0008 FROM wa_t0008.
*** Fim - 23.12.2021 - CBRAND

** Comentado dia 23.12.2021 - CBRAND
** Início - Sara - CS2020000884  - Agosto/2020
*          REFRESH return.
*
*          CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
*            EXPORTING
*              materialdocument = _materialdocument315_q
*              matdocumentyear  = _matdocumentyear
*            IMPORTING
*              goodsmvt_headret = document_storno
*            TABLES
*              return           = return.
*
*          READ TABLE return INTO _return WITH KEY type = 'E'.
*          IF sy-subrc = 0.
*            MESSAGE _return-message TYPE 'S'.
*            EXIT.
*          ELSE.
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                wait = 'X'.
** Fim - Sara - CS2020000884  - Agosto/2020
*
*            wa_zsdt0001-doc_material     = ''.
*            wa_zsdt0001-ano_material     = ''.
*            wa_zsdt0001-doc_material_e   = ''.
*            wa_zsdt0001-ano_material_e   = ''.
*            MODIFY zsdt0001 FROM wa_zsdt0001.
*
*            wa_t0008-doc_material_e = ''.
** Início - Sara - CS2020000884  - Agosto/2020
*            wa_t0008-doc_material_q   = ''.
** Fim - Sara - CS2020000884  - Agosto/2020
*            MODIFY zfiwrt0008 FROM wa_t0008.
*
*          ENDIF.
* Fim comentário 23.12.2021 - CBRAND
        ENDIF.
      ENDIF.

      REFRESH: it_saida_op_aux[], it_saida_opus[].
      PERFORM z_mov_entrada_opus.
    ENDIF.

  ELSE.

    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_index_rows = tg_selectedrow.

    IF tg_selectedrow[] IS INITIAL.
      MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
      EXIT.
    ELSE.

      READ TABLE tg_selectedrow INTO wg_selectedrow INDEX 1.



      READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.




      SELECT SINGLE *  FROM  zfiwrt0008 INTO wa_t0008  WHERE seq_lcto EQ wa_saida-doc_znfw.

*---> 04/07/2023 - Migração S4 - AF
*     _materialdocument315 =  wa_saida-doc_mat_mv315.
*     _materialdocument309 =  wa_saida-doc_mat_mv309.
      _materialdocument315 =  CONV #( wa_saida-doc_mat_mv315 ).
      _materialdocument309 =  CONV #( wa_saida-doc_mat_mv309 ).
*---> 04/07/2023 - Migração S4 - AF
      _matdocumentyear     =  sy-datum+0(4).

      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument = _materialdocument309
          matdocumentyear  = _matdocumentyear
        IMPORTING
          goodsmvt_headret = document_storno
        TABLES
          return           = return.

      READ TABLE return INTO _return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        MESSAGE _return-message TYPE 'S'.
        EXIT.
      ELSE.




        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.



        REFRESH return.



        CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
          EXPORTING
            materialdocument = _materialdocument315
            matdocumentyear  = _matdocumentyear
          IMPORTING
            goodsmvt_headret = document_storno
          TABLES
            return           = return.

        READ TABLE return INTO _return WITH KEY type = 'E'.
        IF sy-subrc = 0.
          MESSAGE _return-message TYPE 'S'.
          EXIT.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          wa_t0008-doc_material_trm = ''.
          wa_t0008-doc_material_e   = ''.
          MODIFY zfiwrt0008 FROM wa_t0008.
          CLEAR wa_t0008.





        ENDIF.
      ENDIF.



      REFRESH: it_saida_aux, it_saida[].
      PERFORM z_mov_entrada_manual.
    ENDIF.
  ENDIF.






ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_PROC_TROCA_MAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_proc_troca_mat_opus USING s_proc_job   TYPE  zmmt0122-proc_job
                                 s_dt_mov     TYPE  zsdt0001-dt_movimento
                                 s_werks      TYPE  zmmt0122-werks        CHANGING r_status   TYPE v_icon-name r_log_erro TYPE string.

  CLEAR: xdocmat309, xdocmat315, xerro.


  CALL FUNCTION 'ZPROC_ENTR_EST_COMODORO'
    EXPORTING
      p_proc_job     = s_proc_job
      p_dt_movimento = s_dt_mov
      p_werks        = s_werks
    IMPORTING
      _xdocmat309    = xdocmat309     " Nº documento de material
* Início - Sara - CS2020000884  - Agosto/2020
      _xdocmat315_q  = xdocmat315_q   " Nº documento de material Quebra
* Fim - Sara - CS2020000884  - Agosto/2020
      _xdocmat315    = xdocmat315.     " Nº documento de material

* Início - Sara - CS2020000884  - Agosto/2020
*  IF XDOCMAT309 IS INITIAL AND XDOCMAT315 IS INITIAL.
* Fim - Sara - CS2020000884  - Agosto/2020
  IF xdocmat309 IS INITIAL AND xdocmat315 IS INITIAL AND xdocmat315_q IS INITIAL.

    SELECT SINGLE * FROM zmmt0123 INTO wa_zmmt0123
      WHERE werks           EQ s_werks
      AND   lgort           IN p_lgort
      AND   dt_atualizacao  EQ sy-datum.

    IF sy-subrc EQ 0.
      r_status    = icon_incomplete.
      r_log_erro  = wa_zmmt0123-message.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_clear .

  REFRESH: it_saida_aux[], it_saida[], it_saida_op_aux[],  it_saida_opus[], it_zsdt0001[], it_0008[], it_0009[],
           it_lfa1[], it_j_1bnfdoc[], it_zmmt0122[].

  CLEAR: wa_saida, wa_zsdt0001, wa_0008, wa_0009, wa_lfa1, wa_j_1bnfdoc, wa_zmmt0123, wa_zsdt0001, wa_mara, wa_zmmt0122, wa_saida_opus.
  CLEAR: xregio,  xnfyear,  xnfmonth, xstcd1, xmodel, xserie, xnfnum9, xdocnum9, xcdv, _seq_lcto.

ENDFORM.
