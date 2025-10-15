*&---------------------------------------------------------------------*
*& Include ZFIR0031_TOP                                      PoolMóds.        ZFIR0031
*&
*&---------------------------------------------------------------------*

PROGRAM  zfir0031.
*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPE-POOLS vrm.

DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.

DATA: name_oper  TYPE vrm_id,
      list_oper  TYPE vrm_values,
      value_oper LIKE LINE OF list.

DATA: p_exibir.

TYPES: BEGIN OF ty_cadlan,
         nro_sol       TYPE zfit0045-nro_sol,
         ebeln         TYPE zfit0045-ebeln,
         bukrs         TYPE zfit0045-bukrs,
         butxt         TYPE t001-butxt,
         lifnr         TYPE zfit0045-lifnr,
         name1         TYPE lfa1-name1,
         lifnr_p       TYPE lfa1-lifnr,
         name1_p       TYPE lfa1-name1,
         dt_pgto       TYPE zfit0045-dt_pgto,
         zlsch         TYPE bseg-zlsch,
         bvtyp         TYPE bseg-bvtyp,
         hbkid         TYPE bseg-hbkid,
         hbkid_e       TYPE bseg-hbkid,
         identificador TYPE zfit0045-identificador,
         dt_prev_liq   TYPE zfit0045-dt_prev_liq,
         moeda_pgto    TYPE zfit0045-moeda_pgto,
         waers         TYPE ekko-waers,
         motivo        TYPE zfit0045-motivo,
         sgtxt         TYPE zfit0045-sgtxt,
         resp_neg      TYPE zfit0045-resp_neg,
         resp_neg_n    TYPE v_usr_name-name_text,
         dep_resp      TYPE zfit0045-dep_resp,
         descricao     TYPE zimp_cad_depto-dep_resp_desc,
         usnam         TYPE zfit0045-usnam,
         usnam_n       TYPE v_usr_name-name_text,
         status        TYPE zfit0045-status,
         belnr         TYPE zfit0045-belnr,
         orig_pgt      TYPE zfit0045-orig_pgt,
         form_pgt      TYPE zfit0045-form_pgt,
         taxa          TYPE zfit0045-taxa,
         saldo(20),
         limite(20),
         adto_ins      TYPE zfit0045-adto_ins,
         icon(4),
         tp_oper       TYPE ztp_oper,
         nro_sm_se     TYPE znro_sm_se,
         nro_sol_cp    TYPE zmmt0035-nro_sol_cp,
       END OF ty_cadlan,

       BEGIN OF ty_itens,
         ebelp            TYPE zfit0046-ebelp,
         knttp            TYPE zfit0046-knttp,
         pstyp            TYPE zfit0046-pstyp,
         matnr            TYPE zfit0046-matnr,
         txz01            TYPE ekpo-txz01,
         aufnr            TYPE zfit0046-aufnr,
         anln1            TYPE zfit0046-anln1,
         anln2            TYPE zfit0046-anln2,
         saldo_item       TYPE zfit0046-saldo_item,
         pgtos_real       TYPE zfit0046-pgtos_real,
         sdo_disponivel   TYPE zfit0046-sdo_disponivel,
         vlr_adiantamento TYPE zfit0046-vlr_adiantamento,
         nro_sol_cp       TYPE zmmt0035-nro_sol_cp,
       END OF ty_itens,

       BEGIN OF ty_ekko,
         ebeln TYPE ekko-ebeln,
         bsart TYPE ekko-bsart,
         lifnr TYPE ekko-lifnr,
         waers TYPE ekko-waers,
         frgke TYPE ekko-frgke,
         bukrs TYPE ekko-bukrs,
       END OF ty_ekko,

       BEGIN OF ty_ekpo,
         ebeln   TYPE ekpo-ebeln,
         loekz   TYPE ekpo-loekz,
         ebelp   TYPE ekpo-ebelp,
         knttp   TYPE ekpo-knttp,
         pstyp   TYPE ekpo-pstyp,
         matnr   TYPE ekpo-matnr,
         txz01   TYPE ekpo-txz01,
         bukrs   TYPE ekpo-bukrs,
         werks   TYPE ekpo-werks,
         menge   TYPE ekpo-menge,
         peinh   TYPE ekpo-peinh,
         j_1bnbm TYPE ekpo-j_1bnbm,
       END OF ty_ekpo,

       BEGIN OF ty_ekkn,
         ebeln TYPE ekkn-ebeln,
         ebelp TYPE ekkn-ebelp,
         anln1 TYPE ekkn-anln1,
         anln2 TYPE ekkn-anln2,
         aufnr TYPE ekkn-aufnr,
         objnr TYPE cobrb-objnr,
       END OF ty_ekkn,

       BEGIN OF ty_cobrb,
         objnr TYPE cobrb-objnr,
         anln1 TYPE cobrb-anln1,
         anln2 TYPE cobrb-anln2,
       END OF ty_cobrb,

       BEGIN OF ty_ekbe,
         ebeln TYPE ekbe-ebeln,
         ebelp TYPE ekbe-ebelp,
         belnr TYPE ekbe-belnr,
         buzei TYPE ekbe-buzei,
         gjahr TYPE ekbe-gjahr,
         wrbtr TYPE ekbe-wrbtr,
         shkzg TYPE ekbe-shkzg,
         reewr TYPE ekbe-reewr,
         bukrs TYPE ekpo-bukrs,
         awkey TYPE bkpf-awkey,
       END OF ty_ekbe,

       BEGIN OF ty_bsak,
         bukrs TYPE bsak-bukrs,
         belnr TYPE bsak-belnr,
         gjahr TYPE bsak-gjahr,
         dmbtr TYPE bsak-dmbtr,
         dmbe2 TYPE bsak-dmbe2,
         waers TYPE bsak-waers,
       END OF ty_bsak,

       BEGIN OF ty_bsik,
         bukrs TYPE bsik-bukrs,
         belnr TYPE bsik-belnr,
         gjahr TYPE bsik-gjahr,
         dmbtr TYPE bsik-dmbtr,
         dmbe2 TYPE bsik-dmbe2,
         zlspr TYPE bsik-zlspr,
       END OF ty_bsik,

       BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         gjahr TYPE bkpf-gjahr,
         awkey TYPE bkpf-awkey,
         belnr TYPE bkpf-belnr,
       END OF ty_bkpf,

       BEGIN OF ty_komp,
         mwsbp TYPE komp-mwsbp,
         netwr TYPE komp-netwr,
       END OF ty_komp,

       BEGIN OF ty_seq_lcto,
         seq_lcto TYPE zfit0045-nro_sol,
       END OF ty_seq_lcto  ,

       BEGIN OF ty_fields,
         campo(30) TYPE c,
         group1(5) TYPE c,
         value     TYPE sy-tabix,
         invisible TYPE sy-tabix,
       END OF ty_fields,

       BEGIN OF ty_editor,
         line(72),
       END OF ty_editor,

       BEGIN OF ty_conta,
         bvtyp TYPE lfbk-bvtyp,
         banks TYPE lfbk-banks,
         bankl TYPE lfbk-bankl,
         bankn TYPE lfbk-bankn,
         bkont TYPE lfbk-bkont,
         iban  TYPE tiban-iban,
         swift TYPE bnka-swift,
         banka TYPE bnka-banka,
         lifnr TYPE lfbk-lifnr,
       END OF ty_conta.



*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: tl_parametros TYPE ustyp_t_parameters,
      wl_parametros TYPE ustyp_parameters.


DATA: v_forn_paga TYPE lifnr.

DATA: tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
      x_field(30).

** Criação de tabela dinamica
DATA: tg_fieldcatalog TYPE lvc_t_fcat,
      wg_fieldcatalog TYPE lvc_s_fcat,
      wa_layout       TYPE lvc_s_layo,
      wa_stable       TYPE lvc_s_stbl,

      tg_fields       TYPE TABLE OF ty_fields   WITH HEADER LINE,
      tg_editor       TYPE TABLE OF ty_editor,
      wg_editor       TYPE ty_editor,
      tg_msg_ret      TYPE TABLE OF zfiwrs0002  WITH HEADER LINE,
      it_seq_lcto     TYPE TABLE OF ty_seq_lcto WITH HEADER LINE,
      tg_conta        TYPE TABLE OF ty_conta.

DATA: manager        TYPE REF TO cl_gos_manager.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas Seleção                                      &*
*&--------------------------------------------------------------------&*
DATA : it_ekko      TYPE TABLE OF ty_ekko,
       it_ekpo      TYPE TABLE OF ty_ekpo,
       it_ekkn      TYPE TABLE OF ty_ekkn,
       it_cobrb     TYPE TABLE OF ty_cobrb,
       it_ekbe      TYPE TABLE OF ty_ekbe,
       it_ekbe_miro TYPE TABLE OF ty_ekbe,
       it_bsak      TYPE TABLE OF ty_bsak,
       it_bsik      TYPE TABLE OF ty_bsik,
       it_bsak_miro TYPE TABLE OF ty_bsak,
       it_bkpf      TYPE TABLE OF ty_bkpf,
       it_zfit0045  TYPE TABLE OF zfit0045,
       it_zfit0046  TYPE TABLE OF zfit0046,
       it_zfit0053  TYPE TABLE OF zfit0053,
       t_libped     TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE.

*&--------------------------------------------------------------------&*
*& Declaração de Work Areas Seleção                                   &*
*&--------------------------------------------------------------------&*
DATA : wa_ekko     TYPE ty_ekko,
       wa_ekpo     TYPE ty_ekpo,
       wa_ekkn     TYPE ty_ekkn,
       wa_cobrb    TYPE ty_cobrb,
       wa_ekbe     TYPE ty_ekbe,
       wa_bsak     TYPE ty_bsak,
       wa_bsik     TYPE ty_bsik,
       wa_bkpf     TYPE ty_bkpf,
       wa_komp     TYPE ty_komp,
       wa_zfit0045 TYPE zfit0045,
       wa_zfit0046 TYPE zfit0046,
       wa_zfit0053 TYPE zfit0053.

DATA: cursorfield(30) TYPE c,
      cursorline(30)  TYPE c,
      cursorvalue(30) TYPE c.

DATA: ok-code                LIKE sy-ucomm,
      wg_cadlan              TYPE ty_cadlan,
      tg_itens               TYPE TABLE OF ty_itens,
      wg_itens               TYPE ty_itens,
      wg_mensagem(30),
      wg_acao(30),
      wl_erro(1),
      vg_habilitar(1),
      vg_habilitar_e(1),
      vg_check_banco(1),
      vg_chamada,
      tabix                  TYPE sy-tabix,
      gf_authorization_ft_09 TYPE c VALUE abap_true. "Workflow de Documentos

DATA: v_orig_pgm          TYPE sy-repid, "PBI - 60949
      v_vlr_adto_residual TYPE zmmt0147-vlr_adto,
      v_vlr_adto          TYPE zmmt0147-vlr_adto, "PBI - 60949
      v_nro_sol_cp        TYPE zmmt0035-nro_sol_cp. "PBI - 60949


DATA: vl_number TYPE i.

*Class definition for ALV toolbar
CLASS:  lcl_alv_toolbar     DEFINITION DEFERRED.

* ---> S4 Migration - 19/06/2023 - JS
*DATA: P_COMP_SERV   TYPE CHAR02,
*      P_COMP_IMPORT TYPE CHAR02.
DATA: p_comp_serv(02)   TYPE c,
      p_comp_import(02) TYPE c.
* <--- S4 Migration - 19/06/2023 - JS

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: g_container          TYPE scrfname VALUE 'CC_ITENS',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      container_1          TYPE REF TO cl_gui_container,
      container_2          TYPE REF TO cl_gui_container,
      splitter             TYPE REF TO cl_gui_splitter_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,

      obg_docking          TYPE REF TO cl_gui_docking_container,

      wa_style             TYPE lvc_s_styl,
      style                TYPE lvc_t_styl   WITH HEADER LINE,
      style2               TYPE lvc_t_styl   WITH HEADER LINE.

DATA: tl_fieldcat TYPE slis_t_fieldcat_alv,
      wl_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      wl_layout   TYPE slis_layout_alv,
      wl_print    TYPE slis_print_alv.

DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_alv_popup TYPE REF TO cl_gui_alv_grid.

* alrs
*Declaration for toolbar buttons
DATA: ty_toolbar TYPE stb_button.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS:
  c_0              TYPE c VALUE '0',
  c_1              TYPE c VALUE '1',
  c_x              TYPE c VALUE 'X',
  c_i              TYPE c VALUE 'I',
  c_n              TYPE c VALUE 'N',
  c_ne(2)          TYPE c VALUE 'NE',
  c_add(3)         TYPE c VALUE 'ADD',
  c_del(3)         TYPE c VALUE 'DEL',
  c_dg1(3)         TYPE c VALUE 'DG1',
  c_dg2(3)         TYPE c VALUE 'DG2',
  c_exit(4)        TYPE c VALUE 'EXIT',
  c_back(4)        TYPE c VALUE 'BACK',
  c_save(4)        TYPE c VALUE 'SAVE',
  c_modif(5)       TYPE c VALUE 'MODIF',
  c_cancel(6)      TYPE c VALUE 'CANCEL',
  c_deldoc(6)      TYPE c VALUE 'DELDOC',
  c_search(6)      TYPE c VALUE 'SEARCH',
  c_displa(6)      TYPE c VALUE 'DISPLA',
  c_atuali(6)      TYPE c VALUE 'ATUALI',
  c_clos_msg(8)    TYPE c VALUE 'CLOS_MSG',
  c_show_msgre(10) TYPE c VALUE 'SHOW_MSGRE'.


*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_cta FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor
        IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command_cta FOR EVENT user_command OF cl_gui_alv_grid
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
    DATA: wl_desactive.

    IF wg_acao NE c_modif AND wg_acao NE c_add.
      wl_desactive = 1.
    ENDIF.

    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = c_add.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = c_del.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*   variable for Toolbar Button
    ty_toolbar-icon      = icon_view_close.
    ty_toolbar-function  = c_clos_msg.
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

  METHOD handle_user_command_cta.
    DATA: tl_conta_aux TYPE TABLE OF ty_conta,
          wl_conta     LIKE LINE OF tg_conta,
          wl_lines     TYPE sy-tabix.

    REFRESH: tl_conta_aux.

    CASE e_ucomm.
      WHEN c_add.
        "TL_CONTA_AUX[] = TG_CONTA[].
        REFRESH: tg_conta.
        LOOP AT tl_conta_aux INTO wl_conta.
          APPEND wl_conta TO tg_conta.
        ENDLOOP.
        CLEAR: wl_conta.
        APPEND wl_conta TO tg_conta.

        CALL METHOD go_alv_popup->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      WHEN c_del.
        CALL METHOD go_alv_popup->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        REFRESH: tg_conta.

        CALL METHOD go_alv_popup->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
    ENDCASE.

  ENDMETHOD.                    "zm_handle_user_command

  METHOD handle_user_command.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*

"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click

  METHOD on_data_changed.

  ENDMETHOD.                    "on_DATA_CHANGED

  METHOD on_data_changed_finished.

*** Método de atualização de dados na Tela
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    "PERFORM F_VERIFICA_ERROS.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '100'
        i_show        = space
        i_repid       = sy-repid
        i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.

  ENDMETHOD.                    "on_data_changed_finished

  METHOD on_data_changed_cta.

    DATA: ls_good     TYPE lvc_s_modi,
          lv_value    TYPE lvc_value,
          vl_value    TYPE lvc_value,
          wl_lfbk     TYPE lfbk,
          tl_tiban    TYPE TABLE OF tiban,
          wl_tiban    TYPE tiban,
          wl_bnka     TYPE bnka,
          vlifnr      TYPE lfbk-lifnr,
          vbvtyp      TYPE lfbk-bvtyp,
          vbvtyp2     TYPE lfbk-bvtyp,
          vtabkey     TYPE tiban-tabkey,
          vbanks      TYPE lfbk-banks,
          vbankl      TYPE lfbk-bankl,
          vbankn      TYPE lfbk-bankn,
          vbkont      TYPE lfbk-bkont,
          viban       TYPE tiban-iban,
          vlines      TYPE sy-tabix,
          tabix       TYPE sy-tabix,
          w_erro(1),
          w_answer(1).

* RJF - Ini - IR182022  - ZFI0025 ( banco intermediario e retorno aprovacao )
    DESCRIBE TABLE tg_conta LINES vlines.
    IF vlines EQ '2'.
      LOOP AT er_data_changed->mt_good_cells INTO ls_good.
        READ TABLE tg_conta ASSIGNING FIELD-SYMBOL(<fs_conta1>) INDEX vlines.
        CHECK sy-subrc EQ 0.
        IF <fs_conta1>+3(1) EQ '2'.
          IF ls_good-row_id EQ 1.
            READ TABLE tg_conta ASSIGNING FIELD-SYMBOL(<fs_conta2>) INDEX ls_good-row_id.
            IF ( ls_good-fieldname EQ 'BANKl' AND ls_good-value NE <fs_conta2>-bankl )
            OR ( ls_good-fieldname EQ 'BANKN' AND ls_good-value NE <fs_conta2>-bankn )
            OR ( ls_good-fieldname EQ 'IBAN'  AND ls_good-value NE <fs_conta2>-iban ).
              MESSAGE 'Este Banco é o intermediário!' TYPE 'I'.
              w_erro = 'X'.

              IF ( ls_good-fieldname EQ 'BANKl' AND ls_good-value NE <fs_conta2>-bankl ).
                lv_value = <fs_conta2>-bankl.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'BANKl'
                    i_value     = lv_value.
              ENDIF.

              IF ( ls_good-fieldname EQ 'BANKN' AND ls_good-value NE <fs_conta2>-bankn ).
                lv_value = <fs_conta2>-bankn.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'BANKN'
                    i_value     = lv_value.
              ENDIF.

              IF ( ls_good-fieldname EQ 'IBAN'  AND ls_good-value NE <fs_conta2>-iban ).
                lv_value = <fs_conta2>-iban.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'IBAN'
                    i_value     = lv_value.
              ENDIF.

              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    CHECK w_erro IS INITIAL.
* RJF - Fim - IR182022  - ZFI0025 ( banco intermediario e retorno aprovacao )

    vlifnr = wg_cadlan-lifnr.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      READ TABLE tg_conta ASSIGNING FIELD-SYMBOL(<fs_conta>) INDEX  ls_good-row_id.
      CHECK sy-subrc EQ 0.
      CASE ls_good-fieldname.
        WHEN 'BANKL'.
          <fs_conta>-bankl = ls_good-value.
        WHEN 'IBAN'.
          <fs_conta>-iban = ls_good-value.
        WHEN 'BANKL'.
          <fs_conta>-bankl = ls_good-value.
        WHEN 'BANKN'.
          <fs_conta>-bankn = ls_good-value.
      ENDCASE.
    ENDLOOP.

    vlifnr = wg_cadlan-lifnr.
    vbankl =  <fs_conta>-bankl.

    SELECT SINGLE *
    FROM lfbk
    INTO wl_lfbk
    WHERE lifnr = vlifnr
      AND bankl = vbankl.
    IF sy-subrc = 0.
      lv_value = wl_lfbk-banks.
      <fs_conta>-banks = wl_lfbk-banks.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'BANKS'
          i_value     = lv_value.
    ENDIF.
    CLEAR w_erro.
    LOOP AT er_data_changed->mt_good_cells
                                 INTO ls_good
                                 WHERE fieldname = 'IBAN'
                                 OR    fieldname = 'BANKS'
                                 OR    fieldname = 'BANKL'
                                 OR    fieldname = 'BANKN' .
      READ TABLE tg_conta INTO DATA(wg_conta) INDEX  ls_good-row_id.

      IF ls_good-row_id = 2.
        lv_value = wg_conta-banks.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'BANKS'
            i_value     = lv_value.

        lv_value = wg_conta-bankl.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'BANKL'
            i_value     = lv_value.

        lv_value = wg_conta-bankn.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'BANKN'
            i_value     = lv_value.

        lv_value = wg_conta-iban.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'IBAN'
            i_value     = lv_value.
        EXIT.
      ENDIF.

      MOVE: wg_conta-banks TO vbanks,
            wg_conta-bankl TO vbankl,
            wg_conta-bankn TO vbankn,
*            WG_CONTA-BKONT TO VBKONT,
            wg_conta-iban  TO viban.

      IF ls_good-fieldname  = 'IBAN'.
        viban =  ls_good-value.
      ELSEIF ls_good-fieldname  = 'BANKS'.
        vbanks =  ls_good-value.
      ELSEIF ls_good-fieldname  = 'BANKL'.
        vbankl =  ls_good-value.
      ELSEIF ls_good-fieldname  = 'BANKN'.
        vbankn =  ls_good-value.
      ENDIF.

      IF vbanks IS  NOT INITIAL AND
         vbankl IS  NOT INITIAL AND
        ( vbankn IS  NOT INITIAL OR viban IS NOT INITIAL ) .

        IF vlifnr IS INITIAL.
          IF sy-langu EQ 'P'.
            MESSAGE  'Fornecedor não encontrados' TYPE 'I'.
            w_erro = 'X'.
            EXIT.
          ELSE.
            MESSAGE  'Supplier not found' TYPE 'I'.
            w_erro = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vlifnr
          IMPORTING
            output = vlifnr.

        lv_value = vlifnr.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'LIFNR'
            i_value     = lv_value.

        REFRESH tl_tiban.
        MOVE vlifnr TO vtabkey.
        IF vbankn IS NOT INITIAL.
          SELECT SINGLE *
            FROM lfbk
            INTO wl_lfbk
            WHERE lifnr = vlifnr
            AND banks  = vbanks
            AND bankl  = vbankl
            AND bankn  = vbankn.
        ELSE.
          " Acesso o TIBAN sem tabkey para pegar a data mais recente
          SELECT  *
           FROM tiban
           INTO TABLE tl_tiban
           WHERE banks   = vbanks
           AND   bankl   = vbankl
           AND   tabname IN ('LFBK', 'BUT0BK')
           AND   iban    = viban
           ORDER BY erdat DESCENDING.
        ENDIF.

        IF sy-subrc EQ 0 OR tl_tiban[] IS NOT INITIAL.
          IF vbankn IS NOT INITIAL.
            "ponto
            lv_value = wl_lfbk-bkont.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'BKONT'
                i_value     = lv_value.

            lv_value = wl_lfbk-bvtyp.
            vbvtyp2 = wl_lfbk-bvtyp.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'BVTYP'
                i_value     = lv_value.

            " acesso sem tabkey
            SELECT SINGLE *
             FROM tiban
             INTO wl_tiban
             WHERE banks   =  vbanks
             AND   bankl   =  vbankl
             AND   bankn   =  vbankn
             "AND   TABKEY  =  VTABKEY
             AND   tabname  IN ('LFBK', 'BUT0BK').
            IF sy-subrc NE 0.
              CLEAR lv_value.
            ELSE.
              lv_value = wl_tiban-iban.
            ENDIF.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'IBAN'
                i_value     = lv_value.

          ELSE.
            READ TABLE tl_tiban INTO wl_tiban INDEX 1.
            lv_value = wl_tiban-bankn.
            vbankn   = wl_tiban-bankn..
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'BANKN'
                i_value     = lv_value.

            SELECT SINGLE *
            FROM lfbk
            INTO wl_lfbk
            WHERE lifnr = vlifnr
            AND banks  = vbanks
            AND bankl  = vbankl
            AND bankn  = vbankn.
            IF sy-subrc = 0.
              "ponto
              lv_value = wl_lfbk-bkont.
              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'BKONT'
                  i_value     = lv_value.

              lv_value = wl_lfbk-bvtyp.
              vbvtyp2 = wl_lfbk-bvtyp.
              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'BVTYP'
                  i_value     = lv_value.
            ELSE.
              IF sy-langu EQ 'P'.
                MESSAGE 'Dados informados não encontrado no dado mestre do fornecedor' TYPE 'I'.
                w_erro = 'X'.
                EXIT.
              ELSE.
                MESSAGE 'Data informed not found in the supplier master data' TYPE 'I'.
                w_erro = 'X'.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.

          SELECT SINGLE *
            FROM bnka
            INTO wl_bnka
            WHERE banks	=	vbanks
            AND   bankl	=	vbankl.

          CLEAR lv_value.
          IF sy-subrc = 0.
            lv_value = wl_bnka-swift.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'SWIFT'
                i_value     = lv_value.
            lv_value = wl_bnka-banka.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'BANKA'
                i_value     = lv_value.
          ELSE.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'SWIFT'
                i_value     = lv_value.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'BANKA'
                i_value     = lv_value.
            EXIT.
          ENDIF.
          DESCRIBE TABLE tg_conta LINES vlines.
          CONCATENATE vbvtyp2+0(3) '2' INTO vbvtyp.
          SELECT SINGLE *
           FROM lfbk
           INTO wl_lfbk
           WHERE lifnr = vlifnr
           AND bvtyp  = vbvtyp.

          IF sy-subrc = 0.
            MOVE vlifnr TO vtabkey.
            SELECT SINGLE *
               FROM tiban
               INTO wl_tiban
               WHERE banks   =  wl_lfbk-banks
               AND   bankl   =  wl_lfbk-bankl
               AND   bankn   =  wl_lfbk-bankn
               AND   tabkey  =  vtabkey
               AND   tabname IN ('LFBK', 'BUT0BK').
            IF sy-subrc NE 0.
              CLEAR wl_tiban.
            ENDIF.
            SELECT SINGLE *
               FROM bnka
               INTO wl_bnka
               WHERE banks  = wl_lfbk-banks
               AND   bankl  = wl_lfbk-bankl.
            IF sy-subrc NE 0.
              CLEAR wl_bnka.
            ENDIF.

            wg_conta-bvtyp    = vbvtyp.
            wg_conta-banks    = wl_lfbk-banks.
            wg_conta-bankl    = wl_lfbk-bankl.
            wg_conta-bankn    = wl_lfbk-bankn.
            wg_conta-bkont    = wl_lfbk-bkont.
            wg_conta-iban     = wl_tiban-iban.
            wg_conta-swift    = wl_bnka-swift.
            wg_conta-banka    = wl_bnka-banka.
            wg_conta-lifnr    = vlifnr.

            IF vlines = 2.
              DELETE tg_conta INDEX 2.
            ENDIF.
            APPEND wg_conta TO tg_conta.

          ELSE.
            IF sy-langu = 'P'.
              MESSAGE  'Dados da SEGUNDA conta não encontrados' TYPE 'I'.
              EXIT.
            ELSE.
              MESSAGE  'SECOND Account data not found' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.
        ELSE.
          IF sy-langu = 'P'.
            MESSAGE 'Dados informados não encontrado no dado mestre do fornecedor' TYPE 'I'.
            w_erro = 'X'.
            EXIT.
          ELSE.
            MESSAGE 'Informed data not found in the supplier master data' TYPE 'I'.
            w_erro = 'X'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
    IF w_erro = 'X'.
      REFRESH tg_conta.
      vg_check_banco = 'E'.
    ENDIF.

*** Método de atualização de dados na Tela
    CALL METHOD go_alv_popup->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "ON_DATA_CHANGED

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION


CLASS zlc_zfir0031 DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: hab_tip_oper.

    CLASS-METHODS: check_pstyp_pedido IMPORTING i_ebeln TYPE ebeln.

    CLASS-METHODS: hab_input IMPORTING i_acao TYPE char30.


ENDCLASS.


CLASS zlc_zfir0031 IMPLEMENTATION.


**============================================================================
  "Method para habilitar INPUT campo.
  METHOD hab_input.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'WG_CADLAN-NRO_SM_SE'.
          screen-input = 1.
          MODIFY SCREEN.
        WHEN 'TXT_NR_SE'.
          screen-input = 1.
          MODIFY SCREEN.

        WHEN 'TXT_TP_OPER'.
          screen-input = 1.
          MODIFY SCREEN.

        WHEN 'WG_CADLAN-TP_OPER'.
          screen-input = 1.
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.


  ENDMETHOD.
**============================================================================
  "Method para habilitar a opção tipo de operação.

  METHOD hab_tip_oper.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'TXT_TP_OPER'.

          IF wg_cadlan-orig_pgt NE 'E'.
            screen-invisible = 1.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = 0.
            MODIFY SCREEN.
          ENDIF.


        WHEN 'WG_CADLAN-TP_OPER'.

          IF wg_cadlan-orig_pgt NE 'E'.
            screen-invisible = 1.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = 0.
            MODIFY SCREEN.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

**============================================================================
  METHOD check_pstyp_pedido.

    SELECT SINGLE *
    FROM ekpo
    INTO @DATA(w_ekpo)
      WHERE ebeln EQ @i_ebeln.

    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'WG_CADLAN-NRO_SM_SE'.
          IF wg_cadlan-nro_sm_se IS INITIAL.
*            SCREEN-INPUT = 0.
            screen-invisible = 1.
            MODIFY SCREEN.
          ELSE.
*            SCREEN-INPUT = 1.
            screen-invisible = 0.
          ENDIF.
        WHEN 'TXT_NR_SE'.
          IF wg_cadlan-nro_sm_se IS INITIAL.
            screen-invisible = 1.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = 0.
            MODIFY SCREEN.
          ENDIF.
      ENDCASE.
    ENDLOOP.
    CLEAR: w_ekpo.
  ENDMETHOD.
ENDCLASS.
