*&---------------------------------------------------------------------*
*& Report  zgl017
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zsdr0162.

*&--------------------------------------------------------------------&*
*& Types                                                              &*
*&--------------------------------------------------------------------&*

TYPES: BEGIN OF ty_cad_ordem,
         empresa     TYPE  char30,
         vbeln       TYPE  vbeln,
         usuario     TYPE  usnam,
         netwr       TYPE  netwr, "valor brl
         data        TYPE  erdat,
         waerk       TYPE  waerk,
         filial      TYPE  werks_d,
         netwr_usd   TYPE  netwr,
         cd_sol_isen TYPE zfit186-cd_sol_isen,
       END OF ty_cad_ordem.

TYPES: BEGIN OF ty_cad_ordem2,
         vbeln       TYPE vbeln,
         org_vendas  TYPE vkorg,
         valor       TYPE netwr,
         filial      TYPE werks_d,
         solicitante TYPE string,
         cd_sol_isen TYPE zfit186-cd_sol_isen,
       END OF ty_cad_ordem2.

*comentado smc - 24-06-2025>>>
TYPES:  BEGIN OF ty_ordens.
          INCLUDE TYPE zsd_ord_vendas_est_isen_juros.
TYPES:    escvenda TYPE char250.
TYPES:    vkbur TYPE vbak-vkbur.
TYPES:    justif TYPE char30. "146630 - RGA - icone
TYPES:  END OF ty_ordens.
*comentado smc - 24-06-2025<<<<

TYPES:  BEGIN OF ty_estra.
          INCLUDE TYPE zsd_estrategia_ov.
TYPES:  END OF ty_estra.

TYPES:  BEGIN OF ty_itens.
          INCLUDE TYPE zsd_itens_ov_est_isen_jur.
TYPES:    escvenda TYPE string.
TYPES   END OF ty_itens.

TYPES: BEGIN OF ty_fields,
         campo(30) TYPE c,
         group1(5) TYPE c,
         value     TYPE sy-tabix,
         invisible TYPE sy-tabix,
       END OF ty_fields,

       BEGIN OF ty_editor,
         line(72),
       END OF ty_editor,

       BEGIN OF ty_edit_juros,
         vbeln        TYPE vbeln,
         doc_fatura   TYPE vbeln,
         data_venc    TYPE zfit0026-data_venc,
         data_pgto    TYPE  zfit0026-data_pgto,
         dias_atraso  TYPE p,
         fator        TYPE p DECIMALS 9,
         juros        TYPE zsdt0053-vlrtot,
         jros         TYPE zsdt0053-vlrtot,
         multa        TYPE zsdt0053-vlrtot,
         mult         TYPE zsdt0053-vlrtot,
         porc_juros   TYPE p DECIMALS 9,
         porc_multa   TYPE p DECIMALS 9,
         juros_parc   TYPE zsdt0053-vlrtot,
         multa_parc   TYPE zsdt0053-vlrtot,
         tx_jros      TYPE zsdt0051-tx_multa,
         tx_multa     TYPE zsdt0051-tx_multa,
         vlr_total_ov TYPE zfit0026-mont_moeda,
         vlr_rbdo     TYPE zfit0026-mont_moeda,
         vlr_t_ov     TYPE zfit0026-mont_moeda,
         dias_ano     TYPE char4,
         txt_doc      TYPE char100,
         porc         TYPE char3,
         por          TYPE char3,
       END OF ty_edit_juros.

TYPES: tb_itens TYPE TABLE OF ty_itens.

*&--------------------------------------------------------------------&*
*& Declaração objetos ALV                                             &*
*&--------------------------------------------------------------------&*

CLASS: lcl_alv_toolbar   DEFINITION DEFERRED.

DATA: t_fieldcatalog       TYPE lvc_t_fcat,
      w_fieldcatalog       TYPE lvc_s_fcat,
      wa_layout            TYPE lvc_s_layo,
      wa_stable            TYPE lvc_s_stbl,
      wg_editor            TYPE ty_editor,
      dyfields             LIKE dynpread OCCURS 1 WITH HEADER LINE,
      ok-code              TYPE sy-ucomm,
      tg_selectedcell      TYPE lvc_t_cell,
      wg_selectedcell      TYPE lvc_s_cell,
      g_container          TYPE scrfname VALUE 'CC_ORDENS',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      container_1          TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2          TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter             TYPE REF TO cl_gui_splitter_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      grid2                TYPE REF TO cl_gui_alv_grid,
      grid3                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      g_custom_cont_desc   TYPE REF TO cl_gui_custom_container,
      obg_descbox          TYPE REF TO cl_gui_textedit,
      obg_docking          TYPE REF TO cl_gui_docking_container,
      obg_conteiner_estra  TYPE REF TO cl_gui_custom_container,
      obg_conteiner_itens  TYPE REF TO cl_gui_custom_container,
      g_cc_estra           TYPE scrfname VALUE 'CC_ESTRA',
      g_cc_itens           TYPE scrfname VALUE 'CC_ITENS',
      wa_style             TYPE lvc_s_styl,
      tl_function          TYPE ui_functions,
      wl_function          LIKE tl_function WITH HEADER LINE,
      style                TYPE lvc_t_styl  WITH HEADER LINE,
      style2               TYPE lvc_t_styl WITH HEADER LINE,
      ty_toolbar           TYPE stb_button,
      node_itab            LIKE node_str OCCURS 0,
      node                 LIKE node_str,
      container            TYPE REF TO cl_gui_custom_container,
      splitter_msg         TYPE REF TO cl_gui_easy_splitter_container,
      right                TYPE REF TO cl_gui_container,
      left                 TYPE REF TO cl_gui_container,
      editor               TYPE REF TO cl_gui_textedit,
      tree                 TYPE REF TO cl_gui_simple_tree,
      behaviour_left       TYPE REF TO cl_dragdrop,
      behaviour_right      TYPE REF TO cl_dragdrop,
      handle_tree          TYPE i,
      num_row              TYPE i VALUE 0,
      btn_rej(30),
      it_bdcdata           TYPE TABLE OF bdcdata,
      wa_edit              TYPE ty_edit_juros,
      w_edit               TYPE ty_edit_juros.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: wg_cad_ordem TYPE ty_cad_ordem,
      wa_estra     TYPE ty_estra,
      wa_itens     TYPE ty_itens,
      w_itens      TYPE ty_itens,
      tg_fields    TYPE TABLE OF ty_fields   WITH HEADER LINE,
      tg_editor    TYPE TABLE OF ty_editor,
      tg_ordens    TYPE TABLE OF  ty_ORDENS WITH HEADER LINE,
      tg_estra     TYPE TABLE OF ty_estra,
      tg_itens     TYPE TABLE OF ty_itens,
      it_itens     TYPE TABLE OF ty_itens,
      it_estra     TYPE TABLE OF ty_estra,
      tg_msg_ret   TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      tg_026       TYPE TABLE OF zfit0026,
      gt_fit186    TYPE TABLE OF zfit186.

DATA: ind_rec_total            TYPE c.
DATA: ind_rec_parc             TYPE c.
DATA: ind_doc_fatura           TYPE c.
DATA: tot_saldo                TYPE zfit0026-mont_moeda.
DATA: vlr_saldo_parcial        TYPE zfit0026-mont_moeda.
DATA: vlr_saldo_finc           TYPE zfit0026-mont_moeda.
DATA: tot_saldo_fin            TYPE zfit0026-mont_moeda.

CONSTANTS: BEGIN OF c_tab_strip_imp,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC3',
           END OF c_tab_strip_imp.

CONTROLS:  tab_strip_imp TYPE TABSTRIP.

DATA: BEGIN OF g_tab_strip_imp,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZSDR0069',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_imp-tab1,
      END OF g_tab_strip_imp.

*&--------------------------------------------------------------------&*
*& Declaração de variaveis                                            &*
*&--------------------------------------------------------------------&*

DATA: ok_code          LIKE sy-ucomm,
      wg_mensagem(30),
      wg_acao(30),
      vdt_apuracao(1),
      vmes_apuracao(1),
      vkokrs           TYPE tka02-kokrs,
      xclasse(1),
      xmodif(1),
      vvalor_ate       TYPE zsdt0142-valor_ate.

DATA: txtemp(10),
      txtlot(15),
      txtusu(15),
      txtmoe(15),
      txtval(15).

DATA: aux_empresa TYPE vbak-vkbur,
      aux_t001w   TYPE t001w,
      p_doc       TYPE bseg-belnr,
      p_emp       TYPE vbak-vkbur.


"146630 - RGA - Tela 0140 - Justificativa rejeição
DATA go_container2  TYPE REF TO cl_gui_custom_container.
DATA go_textedit             TYPE REF TO cl_gui_textedit.
DATA gt_text                 TYPE TABLE OF as4text.

"146630 - RGA - POPUP justificativa
DATA tg_texto     TYPE catsxt_longtext_itab.
DATA gv_display .
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_0               TYPE c VALUE '0',
           c_1               TYPE c VALUE '1',
           c_2               TYPE c VALUE '2',
           c_b               TYPE c VALUE 'B',
           c_s               TYPE c VALUE 'S',
           c_l               TYPE c VALUE 'L',
           c_x               TYPE c VALUE 'X',
           c_d               TYPE c VALUE 'D',
           c_k               TYPE c VALUE 'K',
           c_w               TYPE c VALUE 'W',
           c_f               TYPE c VALUE 'F',
           c_t               TYPE c VALUE 'T',
           c_i               TYPE c VALUE 'I',
           c_n               TYPE c VALUE 'N',
           c_h               TYPE c VALUE 'H',
           c_ag(2)           TYPE c VALUE 'AG',
           c_ne(2)           TYPE c VALUE 'NE',
           c_01(2)           TYPE c VALUE '01',
           c_30(2)           TYPE c VALUE '30',
           c_40(2)           TYPE c VALUE '40',
           c_50(4)           TYPE c VALUE '0050',
           c_76(2)           TYPE c VALUE '76',
           c_71(2)           TYPE c VALUE '71',
           c_72(2)           TYPE c VALUE '72',
           c_br(2)           TYPE c VALUE 'BR',
           c_lf(2)           TYPE c VALUE 'LF',
           c_lr(2)           TYPE c VALUE 'LR',
           c_z1(2)           TYPE c VALUE 'Z1',
           c_add(3)          TYPE c VALUE 'ADD',
           c_del(3)          TYPE c VALUE 'DEL',
           c_dg1(3)          TYPE c VALUE 'DG1',
           c_dg2(3)          TYPE c VALUE 'DG2',
           c_dummy_header(3) TYPE c VALUE '099',
           c_dummy_itens(3)  TYPE c VALUE '098',
           c_exit(4)         TYPE c VALUE 'EXIT',
           c_root(4)         TYPE c VALUE 'ROOT',
           c_minimizar(4)    TYPE c VALUE '@K2@',
           c_maximizar(4)    TYPE c VALUE '@K1@',
           c_back(4)         TYPE c VALUE 'BACK',
           c_save(4)         TYPE c VALUE 'SAVE',
           c_desat(5)        TYPE c VALUE 'DESAT',
           c_dmbtr(5)        TYPE c VALUE 'DMBTR',
           c_refresh(7)      TYPE c VALUE 'REFRESH',
           c_filtro(6)       TYPE c VALUE 'FILTRO',
           c_modif(5)        TYPE c VALUE 'MODIF',
           c_cancel(6)       TYPE c VALUE 'CANCEL',
           c_deldoc(6)       TYPE c VALUE 'DELDOC',
           c_displa(6)       TYPE c VALUE 'DISPLA',
           c_dclick(6)       TYPE c VALUE 'DCLICK',
           c_search(6)       TYPE c VALUE 'SEARCH',
           c_atuali(6)       TYPE c VALUE 'ATUALI',
           c_add_msg(7)      TYPE c VALUE 'ADD_MSG',
           c_del_msg(7)      TYPE c VALUE 'DEL_MSG',
           c_clos_msg(8)     TYPE c VALUE 'CLOS_MSG',
           c_save_msg(8)     TYPE c VALUE 'SAVE_MSG',
           c_show_msgre(10)  TYPE c VALUE 'SHOW_MSGRE'.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_click2 FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_click FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION


CLASS lcl_drag_object DEFINITION.
  PUBLIC SECTION.
    DATA text TYPE mtreesnode-text.
ENDCLASS.                    "lcl_drag_object DEFINITION

CLASS lcl_dragdrop_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      node_double_click FOR EVENT node_double_click OF cl_gui_simple_tree
        IMPORTING node_key.

ENDCLASS.                    "lcl_dragdrop_receiver DEFINITION

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

CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: wl_desactive.

    IF wg_acao NE c_modif.
      wl_desactive = 1.
    ENDIF.

    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.


  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_double_click.

    vvalor_ate = 0.

    IF e_row GT 0.
      CLEAR: wg_cad_ordem, tg_estra[], tg_itens[].

      READ TABLE tg_ordens INTO DATA(wl_ordens) INDEX e_row.

      CHECK sy-subrc = 0.

*      IF WL_ORDENS-STATUS = ICON_ALERT.
*        MESSAGE text-I01 TYPE 'I'.
*        CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*          EXPORTING
*            FUNCTIONCODE           = '=ENT'
*          EXCEPTIONS
*            FUNCTION_NOT_SUPPORTED = 1
*            OTHERS                 = 2.
*        EXIT.
*        CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
*          EXPORTING
*            IS_STABLE = WA_STABLE.
*        CALL METHOD GRID3->REFRESH_TABLE_DISPLAY
*          EXPORTING
*            IS_STABLE = WA_STABLE.
*      ENDIF.

      wg_cad_ordem-empresa     = wl_ordens-empresa(4).
      wg_cad_ordem-vbeln       = wl_ordens-ov_principal.
      wg_cad_ordem-usuario     = wl_ordens-ernam.
      wg_cad_ordem-netwr       = wl_ordens-netwr.
      wg_cad_ordem-cd_sol_isen = wl_ordens-cd_sol_isen. "146630 - RGA
      wg_cad_ordem-netwr_usd   = wl_ordens-netwr_usd.
      wg_cad_ordem-waerk       = wl_ordens-moeda. "SMC - 16-06-2025

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '=ENT'
        EXCEPTIONS
          function_not_supported = 1
          OTHERS                 = 2.

      LOOP AT it_estra INTO DATA(wa_estra) WHERE vbeln = wl_ordens-ov_principal
                                             AND tp_negocio_de <= wl_ordens-tipo_negocio(1)
                                             AND tp_negocio_ate >= wl_ordens-tipo_negocio(1).
*                                             AND waerk = WL_ORDENS-MOEDA. "BUG SOLTO 145061 - SMC - COMENTADO SMC 16-06-2025
        APPEND wa_estra TO tg_estra.
      ENDLOOP.

      LOOP AT it_itens INTO DATA(wa_itens) WHERE ov_principal = wl_ordens-ov_principal.

**********************************************************************
*120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
        FREE: aux_t001w.
        CLEAR: aux_t001w.

        SELECT SINGLE werks,name1 FROM t001w
          INTO CORRESPONDING FIELDS OF @aux_t001w
          WHERE werks = @wa_itens-escr_vendas.
        wa_itens-escvenda = |{ aux_t001w-werks } - { aux_t001w-name1 }|. "120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
**********************************************************************

        APPEND wa_itens TO tg_itens.

      ENDLOOP.

    ENDIF.

    SORT tg_estra BY nivel.

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_click2.

    DATA: lt_msg TYPE TABLE OF bdcmsgcoll.

    CHECK e_row_id GT 0.

    IF e_column_id = 'VBELN'.
      READ TABLE tg_itens INTO DATA(wl_itens) INDEX e_row_id.
      CHECK sy-subrc = 0.
      SET PARAMETER ID 'AUN' FIELD wl_itens-vbeln.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    ELSEIF e_column_id = 'SIMUL_VENDA'.
      DATA(lt_ordens) = tg_ordens[].
      SORT lt_ordens BY ov_principal.

      READ TABLE tg_itens INTO wl_itens INDEX e_row_id.
      CHECK sy-subrc = 0.
      DATA: lv_juros_calc TYPE vbap-netwr,
            lv_juros_rbdo TYPE vbap-netwr,
            lv_desc_jros  TYPE vbap-netwr,
*            lt_log        TYPE TABLE OF ty_log,
            lt_fieldcat   TYPE slis_t_fieldcat_alv.

      READ TABLE lt_ordens ASSIGNING FIELD-SYMBOL(<fs_ordens>)
      WITH KEY ov_principal = wl_itens-ov_principal
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        REFRESH it_bdcdata.

        DATA(lv_mode) = 'E'.

        IF <fs_ordens>-tipo_negocio(1) EQ '2'.

          PERFORM f_bdc_data USING:
                  'ZSDR016'   '0100'  'X'  ''                           ' ',
                  ''          ''      ''   'BDC_CURSOR'	                'WG_HEADER-DOC_SIMULACAO',
                  ''          ''      ''   'BDC_OKCODE'	                'ATUAL',
                  ''          ''      ''   'WG_HEADER-DOC_SIMULACAO'     wl_itens-simul_venda.

          CALL TRANSACTION 'ZSDT0044' USING it_bdcdata
                                        MODE lv_mode
                               MESSAGES INTO lt_msg.
        ELSEIF <fs_ordens>-tipo_negocio(1) EQ '1'.

          PERFORM f_bdc_data USING:
                  'ZSDR0022'  '0050'  'X'  ''                           ' ',
                  ''          ''      ''   'BDC_CURSOR'	                'WG_HEADER-NRO_SOL_OV',
                  ''          ''      ''   'BDC_OKCODE'	                'ATUAL',
                  ''          ''      ''   'WG_HEADER-NRO_SOL_OV'       wl_itens-simul_venda.

          CALL TRANSACTION 'ZSDT0062' USING it_bdcdata
                                       MODE lv_mode
                              MESSAGES INTO lt_msg.
        ENDIF.

      ENDIF.
    ELSEIF e_column_id = 'VLR_JUROS_CALC'.
      READ TABLE tg_itens INTO wl_itens INDEX e_row_id.
      CHECK sy-subrc = 0.
      PERFORM f_saida_memoria_calculo USING wl_itens.
      CALL SCREEN 1001 STARTING AT 1 1 ENDING AT 95 25.

    ENDIF.

  ENDMETHOD.                    "ON_DOUBLE_CLICK2

  METHOD on_click.

    DATA: v_msg    TYPE char50,
          t_ordens TYPE TABLE OF zsd_ordens,
          w_ordens TYPE           zsd_ordens,
          t_estra  TYPE TABLE OF ty_estra,
          w_estra  TYPE          ty_estra,
          lv_lanca TYPE zseq_lac_ins,
          lw_edit  TYPE  zfi_edit.


    DATA lv_data TYPE c LENGTH 10.
    DATA lv_data_lib TYPE c LENGTH 10.
    DATA lv_hora TYPE c LENGTH 10.
    DATA lv_hora_lib TYPE c LENGTH 10.

    DATA: lv_texto_alt TYPE char255,
          lv_pos       TYPE i.

    FREE: tg_texto.

    CHECK e_row_id GT 0.
    "146630 - RGA
    IF e_column_id = 'JUSTIF'.

      READ TABLE tg_ordens INTO DATA(ls_ordens) INDEX e_row_id.


      READ TABLE gt_fit186 INTO DATA(gs_fit186) WITH KEY ov_principal = ls_ordens-ov_principal
                                                         cd_sol_isen  = ls_ordens-cd_sol_isen.




      WRITE gs_fit186-data TO lv_data DD/MM/YY.

      lv_hora = gs_fit186-hora(2) && ':' && gs_fit186-hora+2(2) && ':' && gs_fit186-hora+4(2).

      APPEND INITIAL LINE TO tg_texto ASSIGNING FIELD-SYMBOL(<fs_texto>).
      <fs_texto> = |Usuário solicitante: | && gs_fit186-usuario_solicit && ' Data: ' && lv_data && ' Hora: ' && lv_hora.

      lv_texto_alt = gs_fit186-justificativa.

      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_texto_alt WITH ''.  " Line feed
*      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_texto_alt WITH ''.  " Tab
      " Quebra automaticamente em partes de 72
      DO.
        IF strlen( lv_texto_alt ) <= 72.
          " Última linha
          IF strlen( lv_texto_alt ) > 0.
            APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
            <fs_texto> = lv_texto_alt.

          ENDIF.
          EXIT.
        ELSE.
          " Adiciona parte de 72 caracteres
          APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
          <fs_texto> = lv_texto_alt(72).
          " Remove os 72 primeiros caracteres
          lv_texto_alt = lv_texto_alt+72.
        ENDIF.
      ENDDO.


      APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
      <fs_texto> = '-----------------------------------'.

      "liberação do Gerente
      WRITE gs_fit186-data_lib TO lv_data_lib DD/MM/YY.

      lv_hora_lib = gs_fit186-hora_lib(2) && ':' && gs_fit186-hora_lib+2(2) && ':' && gs_fit186-hora_lib+4(2).

      APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
      <fs_texto> = |Usuário liberação: | && gs_fit186-usuar_lib && ' Data: ' && lv_data_lib && ' Hora: ' && lv_hora_lib.

      lv_texto_alt = gs_fit186-justificativa_ger.
*      TRANSLATE lv_texto_alt USING '# '.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_texto_alt WITH ''.  " Line feed
*      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN lv_texto_alt WITH ''.  " Tab
      " Quebra automaticamente em partes de 72
      DO.
        IF strlen( lv_texto_alt ) <= 72.
          " Última linha
          IF strlen( lv_texto_alt ) > 0.
            APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
            <fs_texto> = lv_texto_alt.

          ENDIF.
          EXIT.
        ELSE.
          " Adiciona parte de 72 caracteres
          APPEND INITIAL LINE TO tg_texto ASSIGNING <fs_texto>.
          <fs_texto> = lv_texto_alt(72).
          " Remove os 72 primeiros caracteres
          lv_texto_alt = lv_texto_alt+72.
        ENDIF.
      ENDDO.

      gv_display = 'X'.

      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title        = 'Texto para Motivo'
          im_display_mode = gv_display " Somente vizualizar ou inserir
        CHANGING
          ch_text         = tg_texto.

      PERFORM f_refresh.
      RETURN.

    ENDIF.

    CLEAR: w_ordens, t_ordens[], t_estra[].

    READ TABLE tg_estra INTO DATA(wl_estra_aux) INDEX e_row_id.

    CHECK sy-subrc = 0 AND wl_estra_aux-opcoes IS NOT INITIAL.



*RMNI - CS1115369 - Removido o binary search pois pode ser que a tabela não venha ordenada de forma correta - INICIO
*    READ TABLE TG_ORDENS INTO DATA(WL_ORDENS) WITH KEY VBELN = WL_ESTRA_AUX-VBELN BINARY SEARCH.
    READ TABLE tg_ordens INTO DATA(wl_ordens) WITH KEY ov_principal = wl_estra_aux-vbeln.
*RMNI - CS1115369 - FIM
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING wl_ordens TO w_ordens.

    APPEND w_ordens  TO t_ordens.

    LOOP AT it_estra INTO DATA(wl_estra) WHERE vbeln EQ wl_ordens-ov_principal.
      CLEAR: w_estra.
      IF wl_estra-aprovador EQ sy-uname.
        MOVE-CORRESPONDING wl_estra_aux TO w_estra.
      ELSE.
        MOVE-CORRESPONDING wl_estra TO w_estra.
      ENDIF.
      APPEND w_estra TO t_estra.
    ENDLOOP.

    CALL FUNCTION 'Z_ESTRATEGIA_EXECUTAR_ISEN_JUR'
      EXPORTING
        i_usuario = sy-uname
      IMPORTING
        e_msg     = v_msg
      TABLES
        t_ordens  = t_ordens
        t_estra   = t_estra.

    LOOP AT t_estra INTO w_estra WHERE aprovador EQ sy-uname.
      MOVE: w_estra-opcoes TO wl_estra-opcoes,
            w_estra-estado TO wl_estra-estado.
      MODIFY tg_estra FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes estado.
    ENDLOOP.


    DATA(lt_estra) = tg_estra.
    DELETE lt_estra WHERE estado = icon_checked.

    IF lt_estra IS INITIAL.

      lw_edit-ajuste         = abap_true.
      lw_edit-moeda          = wl_ordens-moeda.
      lw_edit-bukrs          = wl_ordens-empresa(4).
      lw_edit-vbeln          = wl_ordens-ov_principal.
      lw_edit-data_pgto      = sy-datum.
      lw_edit-mont_rbdo      = wl_ordens-netwr_usd."SMC 25-06-2025
      lw_edit-vlr_juros_rbdo =  wl_ordens-netwr_usd."SMC 25-06-2025
      lw_edit-forma_pag      = 'P'.
      lw_edit-zterm          = 'C002'.
      lw_edit-observacao = 'Isenção de juros aprovado por Workflow'.

      "INICIO BUG SOLTO 145061 - RU
      DATA(lt_itens) = tg_itens.
      SORT lt_itens BY vbeln.
      LOOP AT tg_026 ASSIGNING FIELD-SYMBOL(<fs_026>).
        READ TABLE tg_itens TRANSPORTING NO FIELDS
        WITH KEY vbeln = <fs_026>-vbeln
        BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          DELETE tg_026 WHERE vbeln = <fs_026>-vbeln.
        ENDIF.

      ENDLOOP.
      "FIM BUG SOLTO 145061 - RU
      CALL FUNCTION 'ZFI_CRIAR_LANCAMENTO'
        EXPORTING
          i_edit              = lw_edit
        IMPORTING
          e_numero_lancamento = lv_lanca
        TABLES
          t_026               = tg_026.

    ENDIF.


    IF e_column_id NE 'JUSTIF'.
      PERFORM f_refresh.
    ENDIF.


  ENDMETHOD.                    "ON_CLICK

  METHOD on_data_changed.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.

  ENDMETHOD.                    "on_data_changed_finisheD

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

CLASS lcl_dragdrop_receiver IMPLEMENTATION.
  METHOD node_double_click.

  ENDMETHOD.                    "drop_complete
ENDCLASS.                    "lcl_dragdrop_receiver IMPLEMENTATION


MODULE trata_fields OUTPUT.
  LOOP AT tg_fields.
    LOOP AT SCREEN.
      IF ( screen-name   EQ tg_fields-campo  ) OR
         ( screen-group1 EQ tg_fields-group1 ).
        screen-input     = tg_fields-value.
        screen-invisible = tg_fields-invisible.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT

MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.

  REFRESH: fcode.
  APPEND c_save TO fcode.
  SET PF-STATUS 'Z001' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT

MODULE cria_objetos OUTPUT.

  DATA: event     TYPE cntl_simple_event,
        events    TYPE cntl_simple_events,
        tl_filter TYPE lvc_t_filt,
        wl_filter TYPE lvc_s_filt.

  DATA: waref      TYPE REF TO data.
  IF g_custom_container IS INITIAL.
    txtemp = TEXT-l01.
    txtlot = TEXT-l02.
    txtusu = TEXT-l03.
    txtval = TEXT-l04.
    txtmoe = TEXT-l05.
    btn_rej = TEXT-b01.

    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
    wa_stable-row        = c_x.
    wa_layout-info_fname = 'COLOR'.

    wa_layout-no_toolbar = c_x.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = TEXT-t01 .

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container_1.


    PERFORM montar_layout_ordens.

    PERFORM f_config_function_alv USING 'GRID1'.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_ordens[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
              lcl_event_handler=>on_click FOR grid1,
              lcl_event_handler=>on_double_click FOR grid1,
              lcl_event_handler=>on_data_changed_finished FOR grid1,
              lcl_event_handler=>on_data_changed FOR grid1.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ELSE.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  "GRID2
  IF obg_conteiner_estra IS INITIAL.

    CREATE OBJECT obg_conteiner_estra
      EXPORTING
        container_name = g_cc_estra.

    CREATE OBJECT grid2
      EXPORTING
        i_parent = obg_conteiner_estra.

    PERFORM montar_layout_estra.

    PERFORM f_config_function_alv USING 'GRID2'.

    wa_layout-no_toolbar = space.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = TEXT-t02 .
    wa_layout-no_toolbar = c_x.
    PERFORM montar_layout_estra.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_estra[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
     lcl_event_handler=>on_click FOR grid2.
  ELSE.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  "GRID3
  IF obg_conteiner_itens IS INITIAL.
    CREATE OBJECT obg_conteiner_itens
      EXPORTING
        container_name = g_cc_itens.


    CREATE OBJECT grid3
      EXPORTING
        i_parent = obg_conteiner_itens.

    PERFORM montar_layout_itens.

    PERFORM f_config_function_alv USING 'GRID3'.

    wa_layout-no_toolbar = space.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = TEXT-t03 .
    wa_layout-no_toolbar = c_x.
    PERFORM montar_layout_itens.

    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_itens[].

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
                  lcl_event_handler=>on_click2 FOR grid3.

  ELSE.
    PERFORM montar_layout_itens.

    CALL METHOD grid3->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT

FORM montar_layout_ordens.
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
*        0 ''                ''            'TG_ORDENS' 'STATUS'                     ' '             '03' ' ' ' ' ' ',
        1 ''                ''            'TG_ORDENS' 'CD_SOL_ISEN'                TEXT-a38        '05' ' ' ' ' ' ',
        2 ''                ''            'TG_ORDENS' 'TIPO_NEGOCIO'               TEXT-a27        '10' ' ' ' ' ' ', "ajustado tamanho da coluna - SMC - #120237
        2 ''                ''            'TG_ORDENS' 'EMPRESA'                    TEXT-a01        '10' ' ' ' ' ' ', "ajustado tamanho da coluna - SMC - #120237
        3 ''                ''            'TG_ORDENS' 'ESCVENDA'                   TEXT-a22        '20' ' ' ' ' ' ',
        4 ''                ''            'TG_ORDENS' 'OV_PRINCIPAL'               TEXT-a34        '12' ' ' ' ' ' ',
        5 ''                ''            'TG_ORDENS' 'MOEDA'                      TEXT-a16        '05' ' ' ' ' ' ',
        6 ''                ''            'TG_ORDENS' 'DS_CLIENTE'                 TEXT-a24        '30' ' ' ' ' ' ',
        7 ''                ''            'TG_ORDENS' 'NETWR_USD'                  TEXT-a35        '13' ' ' ' ' ' ',
        8 ''                ''            'TG_ORDENS' 'NETWR'                      TEXT-a37        '13' ' ' ' ' ' ', "SMC 16-06-25
        9 ''                ''            'TG_ORDENS' 'JUSTIF'                     TEXT-a25        '10' ' ' ' ' ' '.



ENDFORM.                    " MONTAR_LAYOUT

FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            p_scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  IF w_fieldcatalog-fieldname EQ 'EMPRESA' OR
     w_fieldcatalog-fieldname EQ 'ESCVENDA'.
    w_fieldcatalog-no_out        = abap_true.
  ELSE.
    w_fieldcatalog-no_out        = ' '.
  ENDIF.


  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  IF p_field EQ 'OPCOES' OR
     p_field EQ 'DOC_LCTO' OR
     p_field EQ 'BELNR' OR
     p_field EQ 'SIMUL_VENDA' OR
     p_field EQ 'VLR_JUROS_CALC'.
    w_fieldcatalog-hotspot = c_x.
  ENDIF.

  "146630 - RGA
  IF  w_fieldcatalog-fieldname =  'JUSTIF'.
    w_fieldcatalog-icon = 'X'.
    w_fieldcatalog-hotspot = 'X'.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura

MODULE user_command_0100 INPUT.
  DATA: ls_zsdt0337 TYPE zsdt0337.

  DATA lv_justif TYPE string.

  CASE ok-code.
    WHEN c_refresh.
      PERFORM f_refresh.
    WHEN c_filtro.
      PERFORM f_filtro. "PSA

    WHEN 'REJ'.
      READ TABLE tg_estra INTO wa_estra WITH KEY  aprovador = sy-uname.
      IF sy-subrc = 0.

* CS2024000604 - Erros diversos Isenção de Juros #189279 - SMC 04-09-2025
        IF wa_estra-estado = icon_checked.
          MESSAGE 'Solicitação já está aprovada para seu nível. Não é possível rejeitar.' TYPE 'E'.
          RETURN.
        ENDIF.
* CS2024000604 - Erros diversos Isenção de Juros #189279 - SMC 04-09-2025

        FREE gt_text."146630 - RGA

        CALL SCREEN '0140' STARTING AT 1 1 ENDING AT 80 20. "146630 - RGA


        IF gt_text IS NOT INITIAL.

          CLEAR lv_justif.

          "146630 - RGA
          LOOP AT gt_text ASSIGNING FIELD-SYMBOL(<fs_text>).
            REPLACE ALL OCCURRENCES OF '#' IN <fs_text> WITH space.
            lv_justif = lv_justif && space && <fs_text>.
          ENDLOOP.


          UPDATE zfit186 SET status_solicit = '5'
                             juscanc = lv_justif
                             dtcan = sy-datum
                             hrcan = sy-uzeit
                             usrcanc = sy-uname
            WHERE ov_principal = wg_cad_ordem-vbeln
            AND   cd_sol_isen  = wg_cad_ordem-cd_sol_isen. "146630 - RGA

          ls_zsdt0337-bukrs = wg_cad_ordem-empresa.
          ls_zsdt0337-vbeln = wg_cad_ordem-vbeln.
          ls_zsdt0337-nivel = wa_estra-nivel.
          ls_zsdt0337-usuario = sy-uname.
          ls_zsdt0337-data_atual = sy-datum.
          ls_zsdt0337-hora_atual = sy-uzeit.
          ls_zsdt0337-valor_de = wa_estra-valor_de.
          ls_zsdt0337-valor_ate = wa_estra-valor_ate.
          ls_zsdt0337-valor_moeda_doc = wg_cad_ordem-netwr_usd. "146630 - RGA
          ls_zsdt0337-valor_brl = wg_cad_ordem-netwr. "146630 - SMC 16-06-2025
          ls_zsdt0337-aprovador = sy-uname.
          ls_zsdt0337-moeda_doc = wg_cad_ordem-waerk. "146630 - RGA
          ls_zsdt0337-cod_solict_isencao = wg_cad_ordem-cd_sol_isen."146630 - RGA
          ls_zsdt0337-status_apr = '4'."146630 - RGA
          MODIFY zsdt0337 FROM ls_zsdt0337.

          COMMIT WORK.

          PERFORM f_refresh.
*        btn_rej = TEXT-b01.
*        IF  wa_estra-opcoes = icon_reject.
*          wa_estra-opcoes = icon_set_state.
*        ELSEIF  wa_estra-opcoes = icon_set_state.
*          wa_estra-opcoes = icon_reject.
*        ENDIF.
*        MODIFY tg_estra FROM wa_estra INDEX sy-tabix TRANSPORTING opcoes.

        ENDIF.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

MODULE user_command_exit INPUT.
  CASE ok-code.
    WHEN c_back.
      SET SCREEN 0.

    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT

FORM montar_layout_estra .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'VBAP'           'NETWR'      'TG_ESTRA' 'VALOR_DE'         TEXT-a06      '15' ' ' ' ' ' ',
        1 'VBAP'           'NETWR'      'TG_ESTRA' 'VALOR_ATE'        TEXT-a07      '15' ' ' ' ' ' ',
*        1 'ZSDT0141'       'WAERS'      'TG_ESTRA' 'WAERK'            TEXT-a16      '05' ' ' ' ' ' ', SMC - 16-06-2025
        1 'ZSDT0141'       'APROVADOR'  'TG_ESTRA' 'APROVADOR'        TEXT-a08      '13' ' ' ' ' ' ',
        1 ' '              ' '          'TG_ESTRA' 'ESTADO'           TEXT-a09      '05' ' ' ' ' ' ',
        1 ' '              ' '          'TG_ESTRA' 'OPCOES'           TEXT-a10      '08' ' ' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_DOCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_itens.

  REFRESH t_fieldcatalog.

  CLEAR w_itens.
  IF tg_itens[] IS NOT INITIAL.
    READ TABLE tg_itens INTO w_itens INDEX 1.
  ENDIF.

  PERFORM montar_estrutura USING:
        01 ''           ''    'TG_ITENS'  'ORG_VENDAS'      TEXT-a01         '10' ' ' ' ' ' ',
        02 ''           ''    'TG_ITENS'  'ESCR_VENDAS'     TEXT-a22         '20' ' ' ' ' ' ',
        03 ''           ''    'TG_ITENS'  'SIMUL_VENDA'     TEXT-a23         '13' ' ' ' ' ' ',
        04 ''           ''    'TG_ITENS'  'VBELN'           TEXT-a02         '13' ' ' ' ' ' ',
        05 ''           ''    'TG_ITENS'  'DATA_VENC'       TEXT-a36         '13' ' ' ' ' ' ',
        06 ''           ''    'TG_ITENS'  'WAERK'           TEXT-a16         '06' ' ' ' ' ' ',
        07 ''           ''    'TG_ITENS'  'VALOR_OV'        TEXT-a29         '13' ' ' ' ' ' ',
        08 ''           ''    'TG_ITENS'  'VLR_JUROS_CALC'  TEXT-a30         '13' ' ' ' ' ' ',
        09 ''           ''    'TG_ITENS'  'VLR_JUROS_RBDO'  TEXT-a31         '13' ' ' ' ' ' ',
        10 ''           ''    'TG_ITENS'  'VLR_DESC_JROS'   TEXT-a32         '13' ' ' ' ' ' ',
        11 ''           ''    'TG_ITENS'  'SALDO_JUROS'     TEXT-a33         '13' ' ' ' ' ' '.


ENDFORM.                    " MONTAR_LAYOUT_DOCS

MODULE carrega_ordens OUTPUT.

  IF g_custom_container IS INITIAL.
    PERFORM atualiza_ordens.
  ENDIF.

ENDMODULE.                 " CARREGA_LOTES  OUTPUT

FORM dynp_values_update USING us_repid
                              us_dynnr
                              us_field
                              us_value
                     CHANGING ch_subrc.

  DATA: da_dynpfield_tab LIKE dynpread OCCURS 0 WITH HEADER LINE,
        da_stepl         LIKE sy-stepl,
        da_repid         LIKE d020s-prog,
        da_dynnr         LIKE d020s-dnum.

  ch_subrc = 4.
  REFRESH da_dynpfield_tab.

  MOVE us_repid TO da_repid.
  MOVE us_dynnr TO da_dynnr.

  GET CURSOR LINE da_stepl.

  MOVE da_stepl TO da_dynpfield_tab-stepl.
  MOVE us_field TO da_dynpfield_tab-fieldname.
  MOVE us_value TO da_dynpfield_tab-fieldvalue.
  APPEND da_dynpfield_tab.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = da_repid
      dynumb               = da_dynnr
    TABLES
      dynpfields           = da_dynpfield_tab
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

  IF sy-subrc EQ 0.
    ch_subrc = 0.
  ENDIF.

ENDFORM.                    " DYNP_VALUES_UPDATE

FORM atualiza_ordens.

  DATA: v_msg    TYPE char50,
        t_ordens TYPE TABLE OF zsd_ord_vendas_est_isen_juros,
        t_estra  TYPE TABLE OF zsd_estrategia_ov,
        t_itens  TYPE TABLE OF zsd_itens_ov_est_isen_jur.

  CLEAR: wg_cad_ordem.

  CALL FUNCTION 'Z_OV_ESTRATEGIA_LISTA_ISEN_JUR'
    EXPORTING
      i_usuario = sy-uname
    IMPORTING
      e_msg     = v_msg
    TABLES
      t_ordens  = t_ordens
      t_estra   = t_estra
      t_itens   = t_itens
      t_026     = tg_026.

  REFRESH: tg_ordens, it_estra, it_itens.

  LOOP AT t_ordens INTO DATA(w_ordens).
    CLEAR: tg_ordens.
    MOVE-CORRESPONDING w_ordens TO tg_ordens.
**********************************************************************
*120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
    FREE: aux_t001w.
    CLEAR: aux_t001w.

    SELECT SINGLE vkbur FROM vbak
      INTO @aux_empresa
      WHERE vbeln = @w_ordens-ov_principal.
    SELECT SINGLE werks,name1 FROM t001w
      INTO CORRESPONDING FIELDS OF @aux_t001w
      WHERE werks = @aux_empresa.

    CONCATENATE  aux_t001w-werks '-' aux_t001w-name1 INTO tg_ordens-escvenda SEPARATED BY space.
    tg_ordens-vkbur = aux_t001w-werks. "120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
**********************************************************************


    "RGA - 04.08.25
    SELECT SINGLE *
      FROM zfit186
      INTO @DATA(gs_fit186)
      WHERE ov_principal EQ @w_ordens-ov_principal
      AND   cd_sol_isen  EQ @w_ordens-cd_sol_isen.
    IF sy-subrc EQ 0.
      APPEND gs_fit186 TO gt_fit186.
    ENDIF.

    tg_ordens-justif = icon_display_text.

    APPEND tg_ordens.

  ENDLOOP.

  IF tg_ordens[] IS NOT INITIAL.

    LOOP AT t_estra INTO DATA(w_estra).
      CLEAR: wa_estra.
      MOVE-CORRESPONDING w_estra TO wa_estra.
      APPEND wa_estra TO it_estra.
    ENDLOOP.

    SORT it_estra BY vbeln nivel.

    DELETE t_itens WHERE saldo_juros = 0.

    LOOP AT t_itens INTO DATA(w_itens).
      CLEAR: wa_itens.
      MOVE-CORRESPONDING w_itens TO wa_itens.
      APPEND wa_itens TO it_itens.
    ENDLOOP.

    SORT it_itens BY vbeln.

  ENDIF.

  IF g_custom_container IS NOT INITIAL.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  IF obg_conteiner_estra IS NOT INITIAL.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  IF obg_conteiner_itens IS NOT INITIAL.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDFORM.                    " Atualiza_lotes

FORM envia_email TABLES tg_estra USING VALUE(wg_cad_ordem) TYPE ty_cad_ordem plinha  .

  FIELD-SYMBOLS: <fs_solix> TYPE solix.

* Objetos para enviar email
  DATA: objpack     LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE.
  DATA: objhead     LIKE solisti1   OCCURS  1 WITH HEADER LINE.
  DATA: objbin_ord  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_log  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_ann  TYPE solisti1.
  DATA: objbin    LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objbin1   TYPE soli_tab, "   OCCURS 10 WITH HEADER LINE.
        wa_objbin LIKE LINE OF objbin.
  DATA: content_hex TYPE STANDARD TABLE OF solix WITH HEADER LINE.
  DATA: objtxt      LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: reclist     LIKE somlreci1  OCCURS  5 WITH HEADER LINE.
  DATA: doc_chng    LIKE sodocchgi1.
  DATA: tab_lines   LIKE sy-tabix.
  DATA: l_anex      TYPE string.
  DATA: l_leng      TYPE i.
  DATA: l_arq       TYPE string.
  DATA: l_tam       TYPE i.
  DATA: l_tam_ord   TYPE i.
  DATA: l_tam_log   TYPE i.
  DATA: l_email(300) TYPE c.
  DATA: vlinha      TYPE i.
  DATA: vuser       TYPE sy-uname.
  DATA: it_shortcut_param LIKE zst_shortcut_par OCCURS 0 WITH HEADER LINE.
  DATA: content  TYPE string,
        lv_valor TYPE string.

*  ** Pass the required parameters and create the shortcut
  CLEAR it_shortcut_param.
  REFRESH it_shortcut_param.

  vlinha = plinha.
  ADD 1 TO vlinha.

  READ TABLE tg_estra INTO wa_estra INDEX vlinha .

  DATA: bsmtp_addr TYPE adr6-smtp_addr.

  SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
    FROM usr21
      INNER JOIN adr6
         ON  usr21~addrnumber = adr6~addrnumber
        AND usr21~persnumber = adr6~persnumber
            WHERE usr21~bname = wa_estra-aprovador.

* Criação do documento de Email
  doc_chng-obj_name = 'LOG_ESTRA'.

* Assunto do Email
  doc_chng-obj_descr = 'Solicitação de Isenção de Juros OV: ' && wg_cad_ordem-vbeln.

* Texto
  objtxt-line = 'Está disponível para aprovação no sistema SAP:'.
  APPEND objtxt.
  CLEAR objtxt.
  APPEND objtxt.

  objtxt-line = 'Aprovação de Isenção de Juros OV: ' &&  wg_cad_ordem-vbeln.
  APPEND objtxt.
  CLEAR objtxt.

*  SELECT SINGLE butxt
*    FROM t001
*    INTO @DATA(lv_butxt)
*    WHERE bukrs = @wg_cad_ordem-empresa(4).
  objtxt-line = wg_cad_ordem-empresa.
  APPEND objtxt.
  CLEAR objtxt.

  SELECT SINGLE name1
    FROM t001w
    INTO @DATA(lv_name1)
    WHERE werks = @wg_cad_ordem-filial.

  CONCATENATE 'Escr. Vendas: ' wg_cad_ordem-filial '-' lv_name1 INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

  lv_valor = wg_cad_ordem-netwr.
  CONCATENATE  'Valor BRL: ' lv_valor INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

  SELECT name_first, name_last
    FROM user_addr
    INTO @DATA(ls_nome)
    UP TO 1 ROWS
    WHERE bname = @wg_cad_ordem-usuario.
  ENDSELECT.
  IF sy-subrc IS INITIAL.

    IF ls_nome-name_first IS INITIAL AND ls_nome-name_last IS INITIAL.
      ls_nome-name_first = wg_cad_ordem-usuario.
    ENDIF.

  ENDIF.
  CONCATENATE 'Solicitante: ' ls_nome-name_first ls_nome-name_last INTO objtxt-line SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = '-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = 'Para aprovar clique no link "Estratégia" em anexo' .
  APPEND objtxt.
  CLEAR objtxt.

  DATA: ctotal(20),
        vdata(10).

  WRITE wg_cad_ordem-netwr TO ctotal CURRENCY 'USD'.

  CONDENSE ctotal NO-GAPS.

  SELECT SINGLE waerk
     FROM vbak INTO @DATA(_waerk)
    WHERE vbeln = @wg_cad_ordem-vbeln.


* Setar tamanho da mensagem
  DESCRIBE TABLE objtxt LINES tab_lines.
  READ TABLE objtxt INDEX tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

* Criar entrada de documento comprimido
  CLEAR objpack-transf_bin.
  "OBJPACK-TRANSF_BIN = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 0.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'RAW'.
  APPEND objpack.

  CALL FUNCTION 'ZFM_CREATE_SHORTCUT'
    EXPORTING
      recipient_user_id = wa_estra-aprovador
      transaction       = 'ZFIS66'
    IMPORTING
      content           = content
    TABLES
      shortcut_param    = it_shortcut_param.

  CLEAR : tab_lines, objbin.
  CONCATENATE content wa_objbin-line INTO wa_objbin-line.
  APPEND  wa_objbin TO objbin.

  DESCRIBE TABLE objbin LINES tab_lines.
  objhead = 'ESTRATEGIA.SAP'.
  APPEND objhead.

** Creation of the entry for the compressed attachment
  objpack-transf_bin = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 1.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'EXT'." SAP
  objpack-obj_name   = 'SAPSHORTCUTMAIL'.
  objpack-obj_descr  = 'ESTRATEGIA.SAP'.
  objpack-doc_size   = tab_lines * 255.
  APPEND objpack.

* Alimentar destinatários do email
  IF bsmtp_addr IS INITIAL.
    MESSAGE 'O aprovador seguinte não tem e-mail cadastrado, por favor contacte a T.I.' TYPE 'I'.
    EXIT.
  ENDIF.

  reclist-receiver = bsmtp_addr.
  reclist-rec_type = 'U'.                    "Define email externo
  APPEND reclist.

* Enviar email
  vuser = sy-uname.
  sy-uname = 'R3JOB'.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = objpack
      object_header              = objhead
      contents_bin               = objbin
      contents_txt               = objtxt      "CONTENTS_HEX = CONTENT_HEX
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      OTHERS                     = 99.

  sy-uname = vuser.

ENDFORM.                    " ENVIA_EMAIL

FORM f_refresh.

  PERFORM atualiza_ordens.

  CLEAR: tg_estra[], tg_itens[].

  LOOP AT it_estra INTO wa_estra WHERE vbeln =  wg_cad_ordem-vbeln.
    APPEND wa_estra TO tg_estra.
  ENDLOOP.

  LOOP AT it_itens INTO wa_itens WHERE vbeln  = wg_cad_ordem-vbeln.

    APPEND wa_itens TO tg_itens.

  ENDLOOP.

  SORT tg_estra BY nivel.

  CALL METHOD grid2->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid3->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDFORM.                    " F_REFRESH

FORM f_config_function_alv  USING p_grid.

  CASE p_grid.
    WHEN 'GRID1' OR 'GRID2' OR 'GRID3'.
      REFRESH: tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
      APPEND wl_function TO tl_function.

      IF p_grid = 'GRID1'.
        wl_function = cl_gui_alv_grid=>mc_fc_check.
        APPEND wl_function TO tl_function.
        wl_function = cl_gui_alv_grid=>mc_fc_refresh.
        APPEND wl_function TO tl_function.
      ENDIF.

  ENDCASE.

ENDFORM.

FORM f_filtro.

  DATA: wa_fields LIKE sval,
        li_fields TYPE STANDARD TABLE OF sval.

  MOVE 'VBAK' TO wa_fields-tabname.
  MOVE 'VKBUR' TO wa_fields-fieldname.
  APPEND wa_fields TO li_fields.

  MOVE 'BSEG' TO wa_fields-tabname.
  MOVE 'BELNR' TO wa_fields-fieldname.
  APPEND wa_fields TO li_fields. " add more fields if required

  PERFORM f_refresh.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
*     NO_VALUE_CHECK = ' '
      popup_title = 'Filtros'
*     START_COLUMN = '5'
*     START_ROW   = '5'
* IMPORTING
*     RETURNCODE  =
    TABLES
      fields      = li_fields
* EXCEPTIONS
*     ERROR_IN_FIELDS = 1
*     OTHERS      = 2
    .
  CLEAR: p_emp,p_doc.

*    p_emp = VALUE #( FOR wa_doc IN li_fields WHERE ( tabname = 'VBAK' AND fieldname = 'VKBUR' ) ( option = 'EQ' sign = 'I' low = wa_doc-value ) ).
*    p_doc = VALUE #( FOR wa_doc IN li_fields WHERE ( tabname = 'BSEG' AND fieldname = 'BELNR' ) ( option = 'EQ' sign = 'I' low = wa_doc-value ) ).


  FIELD-SYMBOLS <li_fields> LIKE sval.

  CLEAR: p_emp,p_doc.

  LOOP AT li_fields ASSIGNING <li_fields> WHERE tabname = 'VBAK' AND fieldname = 'VKBUR'.
    p_emp = <li_fields>-value.
  ENDLOOP.

  LOOP AT li_fields ASSIGNING <li_fields> WHERE tabname = 'BSEG' AND fieldname = 'BELNR'.
    p_doc = <li_fields>-value.
  ENDLOOP.


  IF p_doc IS NOT INITIAL.
    DELETE tg_ordens WHERE ov_principal <> p_doc.
  ENDIF.

  IF p_emp IS NOT INITIAL.
    DELETE tg_ordens WHERE vkbur <> p_emp.
  ENDIF.

  CALL METHOD grid2->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid3->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      -->P_DYNPRO   text
*      -->P_START    text
*      -->P_FNAM     text
*      -->P_FVAL     text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.

  DATA: wa_bdcdata TYPE bdcdata.

  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                    " F_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_MEMORIA_CALCULO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida_memoria_calculo USING p_itens TYPE ty_itens.

  DATA qtde_dias_atraso TYPE int4.
  DATA data_venc_format TYPE char20.
  DATA data_pgto_format TYPE char20.
  DATA: total_ov TYPE vbap-netwr.
  DATA: vlr_total TYPE vbap-netwr.
  DATA: total_ov_parc TYPE vbap-netwr,
        lv_tp_neg     TYPE ty_ordens-tipo_negocio.
  CLEAR: wa_edit, w_edit, total_ov, total_ov_parc, ind_rec_total, ind_doc_fatura, ind_rec_parc.

*  DATA_VENC_FORMAT = | { WA_SAIDA_P-DATA_VENC+6(2) }.{ WA_SAIDA_P-DATA_VENC+4(2) }.{ WA_SAIDA_P-DATA_VENC(4) }|.
*  DATA_PGTO_FORMAT = | { WA_SAIDA_P-DATA_PGTO+6(2) }.{ WA_SAIDA_P-DATA_PGTO+4(2) }.{ WA_SAIDA_P-DATA_PGTO(4) }|.
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_MULTA =  | Valor Multa = { WA_SAIDA_P-MONT_RBDO }(Mont.RBDO) * { WA_SAIDA_P-TX_MULTA }(Tx.Multa)%  = { WA_SAIDA_P-VLR_MULTA_CALC }|.
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_JUROS_DIA_ULTIL = |Se a data de vencimento { DATA_VENC_FORMAT } for dia não útil e a data de pagamento { DATA_PGTO_FORMAT } for primeiro dia útil subsequente a data de vencimento { DATA_VENC_FORMAT }  |.



*  CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
*    EXPORTING
*      I_DATUM_BIS = WA_SAIDA_P-DATA_VENC  " Data Maior
*      I_DATUM_VON = WA_SAIDA_P-DATA_PGTO  " Data Menor
*    IMPORTING
*      E_TAGE      = QTDE_DIAS_ATRASO. " Dieferença em dias
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_DIAS_ATRASO = |Quantidade de dias em atraso: {  COND #( WHEN  QTDE_DIAS_ATRASO = 0 OR QTDE_DIAS_ATRASO IS INITIAL THEN 0 ELSE QTDE_DIAS_ATRASO   ) } dias. | .
*
*  WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_JUROS_DIA = 0.
*
*  IF  WA_SAIDA-TX_JUROS IS NOT INITIAL AND WA_SAIDA-TX_JUROS <> 0 .
*
*    WA_SAIDA_MEMORIA_CALCULO-DESC_VLR_JUROS_DIA  =  | Valor de Juros ao dia: { WA_SAIDA-MONT_RBDO *  QTDE_DIAS_ATRASO *  ( WA_SAIDA-TX_JUROS / 360 ) }%|.

*  ENDIF.

*===============================================================================

*  SELECT *  FROM VBAP INTO TABLE @DATA(T_VBAP)
*          WHERE VBELN EQ @WA_SAIDA-VBELN.
*
*  CLEAR TOTAL_OV.
*  LOOP AT IT_VBAP INTO WA_VBAP WHERE VBELN = WA_SAIDA-VBELN.
*    ADD WA_VBAP-NETWR TO TOTAL_OV.
*    ADD WA_VBAP-MWSBP TO TOTAL_OV.
*  ENDLOOP.

****  Selecionar informações zfit0026.
  SELECT SINGLE *
  FROM zfit0026
    INTO @DATA(w_zfit0026)
     WHERE zid_lanc EQ @p_itens-zid_lanc.

  CHECK w_zfit0026 IS NOT INITIAL.
*  W_EDIT-DOC_FATURA = W_ZFIT0026-DOC_FATURA.
*  w_edit-doc_fatura = p_itens-vbeln.
  w_edit-vbeln = w_zfit0026-vbeln.



  IF w_zfit0026-doc_fatura IS INITIAL.
****   Buscar o saldo total e parcial da OV.
    zcl_dados_ov=>i_vlr_ov(
      EXPORTING
        i_vbeln       = w_zfit0026-vbeln
      IMPORTING
        e_vlr_total   = vlr_total
        e_vlr_parcial = total_ov_parc ).


    total_ov = ( total_ov_parc + w_zfit0026-mont_moeda ).

  ELSE.
****     Buscar o saldo total e parcial da referencia.
    zcl_dados_ov=>i_vlr_referencia_ov(
      EXPORTING
        i_vbeln       = w_zfit0026-vbeln
        i_vbelnn      = w_zfit0026-doc_fatura
      IMPORTING
        e_vlr_total   = vlr_total
        e_vlr_parcial = total_ov_parc ).
*    W_EDIT-TXT_DOC = 'Calculo com base na referencia'.

    CLEAR: total_ov.
    total_ov = ( total_ov_parc + w_zfit0026-mont_moeda ).
    ind_doc_fatura = abap_true.
  ENDIF.

***   Achar o % fator

  READ TABLE tg_ordens ASSIGNING FIELD-SYMBOL(<fs_ordens>)
  WITH KEY ov_principal = p_itens-ov_principal.
  IF sy-subrc IS INITIAL.
    lv_tp_neg = <fs_ordens>-tipo_negocio.
  ENDIF.

  IF lv_tp_neg(1) EQ '1'. "Se mercado interno for selecionado.

    IF p_itens-vlr_juros_calc > 0.
      IF w_zfit0026-data_pgto > w_zfit0026-data_venc.
        wa_edit-dias_atraso = ( w_zfit0026-data_pgto - w_zfit0026-data_venc ).
        w_edit-dias_atraso = wa_edit-dias_atraso.

        SELECT SINGLE *  FROM zsdt0051 INTO @DATA(wa_zsdt0051)
        WHERE nro_sol_ov EQ @p_itens-simul_venda.

        wa_edit-data_venc    =    w_zfit0026-data_venc.
        wa_edit-data_pgto    =    w_zfit0026-data_pgto.
        wa_edit-fator      = ( wa_zsdt0051-tx_juros / 360 ) * wa_edit-dias_atraso.

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          wa_edit-juros      = ( total_ov * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( total_ov * wa_zsdt0051-tx_multa ) / 100.
          DATA(t_ov)              = ( wa_edit-juros + total_ov +  wa_edit-multa ). "Total da OV + juros.

        ELSE.

          wa_edit-juros      = ( vlr_total * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( vlr_total * wa_zsdt0051-tx_multa ) / 100.
          t_ov               = ( wa_edit-juros + vlr_total +  wa_edit-multa ). "Total da OV + juros.

          wa_edit-porc_juros = ( wa_edit-juros / t_ov )  * 100. " Porcentagem proporcional ao valor do juros
          DATA(prop_multa)   = ( wa_edit-multa / t_ov )  * 100. " Porcentagem proporcional da multa.

          wa_edit-juros_parc = ( wa_edit-porc_juros * w_zfit0026-mont_rbdo ) / 100.
          DATA(vlr_multa)    = ( prop_multa * w_zfit0026-mont_rbdo ) / 100.
          total_ov = vlr_total.
        ENDIF.

        wa_edit-tx_jros      = wa_zsdt0051-tx_juros.
        wa_edit-vlr_total_ov  = total_ov.
        wa_edit-vlr_rbdo      = w_zfit0026-mont_rbdo.

        wa_edit-dias_ano      = '360'.
        w_edit-fator          = wa_edit-fator.
        w_edit-juros          = wa_edit-juros.
        wa_edit-jros          = wa_edit-juros.
        w_edit-vlr_total_ov   = wa_edit-vlr_total_ov.
        w_edit-porc_juros     = wa_edit-porc_juros.
        wa_edit-porc          = '100'.
        w_edit-porc           = '100'.
        w_edit-por            = '100'.


        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          w_edit-porc_juros = ' '.
          wa_edit-vlr_rbdo = ' '.
          w_edit-por = ' '.
          wa_edit-juros_parc = ' '.
          ind_rec_total = abap_true.
        ELSE.
          ind_rec_parc = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSEIF lv_tp_neg(1) EQ '2'. "Se insumo interno for selecionado.
*    CLEAR: D_ATRASO.
    IF p_itens-vlr_juros_calc > 0.
      IF w_zfit0026-data_pgto > w_zfit0026-data_venc.
        wa_edit-dias_atraso = ( w_zfit0026-data_pgto - w_zfit0026-data_venc ).
        w_edit-dias_atraso = wa_edit-dias_atraso.

        SELECT SINGLE *  FROM zsdt0040 INTO @DATA(wa_zsdt0040)
        WHERE doc_simulacao EQ @p_itens-simul_venda.

        wa_edit-data_venc    =    w_zfit0026-data_venc.
        wa_edit-data_pgto    =    w_zfit0026-data_pgto.
        wa_edit-fator      = ( wa_zsdt0040-juros_ano / 360 ) * wa_edit-dias_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.

          wa_edit-juros      = ( total_ov * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( total_ov * wa_zsdt0051-tx_multa ) / 100.
          DATA(to_ov)        = ( wa_edit-juros + total_ov +  wa_edit-multa ). "Total da OV + juros.

        ELSE.
          wa_edit-juros      = ( vlr_total * wa_edit-fator ) / 100.  "Valor do juros com base no valor total da OV.
          wa_edit-multa      = ( vlr_total * wa_zsdt0051-tx_multa ) / 100.
          to_ov        = ( wa_edit-juros + vlr_total +  wa_edit-multa ). "Total da OV + juros.

          wa_edit-porc_juros = ( wa_edit-juros / to_ov )  * 100. " Porcentagem proporcional ao valor do juros
          prop_multa         = ( wa_edit-multa / to_ov )  * 100. " Porcentagem proporcional da multa.

          wa_edit-juros_parc = ( wa_edit-porc_juros * w_zfit0026-mont_rbdo ) / 100.
          vlr_multa          = ( prop_multa * w_zfit0026-mont_rbdo ) / 100.
          total_ov = vlr_total.
        ENDIF.

        wa_edit-tx_jros       = wa_zsdt0040-juros_ano.
        wa_edit-vlr_total_ov  = total_ov.
        wa_edit-vlr_rbdo      = w_zfit0026-mont_rbdo.

        wa_edit-dias_ano      = '360'.
        w_edit-fator          = wa_edit-fator.
        w_edit-juros          = wa_edit-juros.
        wa_edit-jros          = wa_edit-juros.
*        WA_EDIT-MULTA         = VLR_MULT.
        w_edit-vlr_total_ov   = wa_edit-vlr_total_ov.
        w_edit-porc_juros     = wa_edit-porc_juros.
        wa_edit-porc          = '100'.
        w_edit-porc           = '100'.
        w_edit-por            = '100'.

        IF w_zfit0026-rec_vlr_total IS NOT INITIAL.
          w_edit-porc_juros = ' '.
          wa_edit-vlr_rbdo = ' '.
          w_edit-por = ' '.
          wa_edit-juros_parc = ' '.
          ind_rec_total = abap_true.
        ELSE.
          ind_rec_parc = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: w_zfit0026, total_ov, to_ov, vlr_total.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_1001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1001 OUTPUT.

  SET PF-STATUS 'STATUS_1001'.
  SET TITLEBAR 'TITLEBAR_1001'.

***      Ocutar campos de recebimento parcial.
  PERFORM ocutar_campos_juros.
  PERFORM ocutar_campos_fatura.

  IF ind_rec_parc IS NOT INITIAL.
    PERFORM ocutar_campo_rec_parc.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPOS_JUROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campos_juros .


  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'TXT_JUROS'                   OR 'TXTTAXA_JUROS'
        OR 'JUROS'                       OR 'TXT_MULT'
        OR 'TXT_TOT_OV'                  OR '100'
        OR 'TXT_PORC_JUROS'              OR 'TXTVLR_JUROS_PARC'
        OR 'TXT_JROS'                    OR 'TXT_SALD_OV'
        OR '%#AUTOTEXT003'               OR '%#AUTOTEXT007'
        OR 'TXTCAL_VALR_TAXA_JUROS_PARC' OR '%#AUTOTEXT006'
        OR 'TXT_SUB'                     OR 'TXT_MAIS'
        OR 'TXT_MAISS'                   OR 'TXT_SUB'
        OR 'TXT_SUBB'                    OR '%#AUTOTEXT001'
        OR '%#AUTOTEXT002'               OR 'TXT_P'
        OR 'TXT_PO'                      OR 'WA_EDIT-JROS'
        OR 'W_EDIT-JUROS'                OR '%#AUTOTEXT008'
        OR 'WA_EDIT-PORC_JUROS'          OR 'W_EDIT-PORC_JUROS'
        OR 'WA_EDIT-MULTA'               OR 'WA_EDIT-VLR_RBDO'
        OR 'W_EDIT-VLR_TOTAL_OV'         OR 'W_EDIT-POR'
        OR 'W_EDIT-PORC'                 OR 'WA_EDIT-JUROS_PARC'.

        IF NOT ind_rec_total IS INITIAL.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPOS_MULTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campos_multa .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'TXTTAXA_JUROS'                 OR '%#AUTOTEXT001'
        OR 'TXT_MULT'                      OR '%#AUTOTEXT006'
        OR 'TXT_MUL'                       OR 'TXT_JUROS'
        OR 'TOTAL_OV'                      OR '%#AUTOTEXT005'
        OR 'TXTCAL_TAXA_JUROS'             OR '%#AUTOTEXT002'
        OR 'TXTVLR_JUROS_PARC'             OR 'TXT_PORC_MULT'
        OR 'TXTCAL_VALR_TAXA_JUROS_PARC'   OR '%#AUTOTEXT008'
        OR '%#AUTOTEXT007'                 OR 'TXT_PORC_MUL'
        OR 'W_EDIT-PORC_MULTA'             OR 'WA_EDIT-VLR_RBDO'
        OR 'W_EDIT-POR'                    OR 'WA_EDIT-MULTA_PARC'
        OR 'TXT_MAIS'                      OR 'W_EDIT-MULT'
        OR 'W_EDIT-MULTA'                  OR'W_EDIT-VLR_TOTAL_OV'
        OR 'WA_EDIT-JUROS'                 OR'WA_EDIT-PORC_MULTA'
        OR 'W_EDIT-PORC'                   OR'TXT_P'
        OR 'TXT_PP'                        OR 'TXT_MAISS'
        OR 'TXT_SUB'                       OR 'TXT_SUBB'.

        IF NOT ind_rec_total IS INITIAL.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPOS_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campos_fatura .
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'W_EDIT-DOC_FATURA' OR 'TXT_FATURA'.
        IF ind_doc_fatura IS NOT INITIAL.
          screen-invisible = 0. "Campo Fechado
          MODIFY SCREEN.
        ELSE.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPO_REC_PARC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campo_rec_parc .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'TXTVLR_JUROS' OR
           'TXT_FAT' OR
           'TXT_T_OV' OR
           'TXT_DV' OR
           '%#AUTOTEXT009' OR
           'TXT_100' OR
           '%#AUTOTEXT005' OR
           'TXTCAL_VALR_' OR
           'W_EDIT-FATOR' OR
           'WA_EDIT-VLR_TOTAL_OV' OR
           'WA_EDIT-PORC' OR
           'WA_EDIT-JUROS' OR
           '%#AUTOTEXT001' OR
           'TXTTAXA_JUROS' OR
           '%#AUTOTEXT002' OR
           'TXTINDICADOR' OR
           'TXT_JUROS' OR
           '%#AUTOTEXT007' OR
           'JUROS' OR
           'TXT_MAIS' OR
           'TXT_MULT' OR
           'TXT_MAISS' OR
           'TXT_TOT_OV' OR
           'TXT_SUB' OR
           'TXT_PO' OR
           '%#AUTOTEXT006' OR
           'TXT_PORC_JUROS' OR
           'WA_EDIT-JROS' OR
           'W_EDIT-JUROS' OR
           'WA_EDIT-MULTA' OR
           'W_EDIT-VLR_TOTAL_OV' OR
           'W_EDIT-PORC' OR
           'WA_EDIT-PORC_JUROS'.

        IF NOT ind_rec_parc IS INITIAL.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1001 INPUT.

  CASE sy-ucomm.
    WHEN 'CANCELAR'.
      LEAVE TO SCREEN  0.

    WHEN 'SAIR'.
      LEAVE TO SCREEN  0.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'STATUS_0140'.
* SET TITLEBAR 'xxx'.

  IF go_container2 IS NOT BOUND.

*   create control container
    CREATE OBJECT go_container2
      EXPORTING
        container_name = 'CC_JUST'
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc = 0.

      CREATE OBJECT go_textedit
        EXPORTING
          max_number_chars           = 255
          parent                     = go_container2
          wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position          = 60
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    ENDIF.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  SET SCREEN 0.
  LEAVE SCREEN.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE sy-ucomm.

    WHEN 'BT_SAIR' OR 'EXIT'.

      CALL METHOD go_textedit->delete_text.

      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'BT_CONF'.

      PERFORM processa_justificativa.

      SET SCREEN 0.
      LEAVE SCREEN.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form processa_justificativa
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM processa_justificativa .
  CALL METHOD go_textedit->get_text_as_stream
    EXPORTING
      only_when_modified     = cl_gui_textedit=>true
    IMPORTING
      text                   = gt_text
    EXCEPTIONS
      error_dp               = 1
      error_cntl_call_method = 2
      OTHERS                 = 3.


  IF gt_text IS INITIAL.
    MESSAGE 'É obrigatório o preenchimento do Motivo da Rejeição' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.
