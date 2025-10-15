*&---------------------------------------------------------------------*
*& Report  ZLESR0148
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0148.
TABLES: zsdt0001, lfa1, j_1bbranch.

INITIALIZATION.

  TYPES:BEGIN OF ty_saida_c_pend,
          bukrs          TYPE zsdt0001-bukrs,
          branch         TYPE zsdt0001-branch,
          peso_fiscal    TYPE zsdt0001-peso_fiscal,
          peso_subtotal  TYPE zsdt0001-peso_subtotal,
          diferenca      TYPE zsdt0001-peso_subtotal,
          peso_compl     TYPE zsdt0001-peso_subtotal,
          nf_compl       TYPE v_icon-name,
          dt_movimento   TYPE zsdt0001-dt_movimento,
          nr_romaneio    TYPE zsdt0001-nr_romaneio,
          nr_safra       TYPE zsdt0001-nr_safra,
          parid          TYPE zsdt0001-parid,
          matnr          TYPE zsdt0001-matnr,
          nfnum          TYPE zsdt0001-nfnum,
          series         TYPE zsdt0001-series,
          docdat         TYPE zsdt0001-docdat,
          netwr          TYPE zsdt0001-netwr,
          placa_cav      TYPE zsdt0001-placa_cav,
          local_descarga TYPE zsdt0001-local_descarga,
          tp_transgenia  TYPE zsdt0001-tp_transgenia,
          desc_trans(30) TYPE c,
          ch_referencia  TYPE zsdt0001-ch_referencia,
          cfop           TYPE zsdt0001-cfop,
          chave_nfe      TYPE zsdt0001-chave_nfe,
          dt_emissao     TYPE zsdt0001-docdat,
        END OF ty_saida_c_pend,

        BEGIN OF ty_saida_s_pend,
          bukrs          TYPE zsdt0001-bukrs,
          branch         TYPE zsdt0001-branch,
          peso_fiscal    TYPE zsdt0001-peso_fiscal,
          peso_subtotal  TYPE zsdt0001-peso_subtotal,
          diferenca      TYPE zsdt0001-peso_subtotal,
          dt_movimento   TYPE zsdt0001-dt_movimento,
          nr_romaneio    TYPE zsdt0001-nr_romaneio,
          nr_safra       TYPE zsdt0001-nr_safra,
          parid          TYPE zsdt0001-parid,
          matnr          TYPE zsdt0001-matnr,
          nfnum          TYPE zsdt0001-nfnum,
          series         TYPE zsdt0001-series,
          dt_emissao     TYPE zsdt0001-docdat,
          vlr_nf         TYPE zsdt0001-netwr,
          placa_cav      TYPE zsdt0001-placa_cav,
          local_descarga TYPE zsdt0001-local_descarga,
          nf_compl       TYPE zlest0205-nfnum,
          serie_nf_compl TYPE zlest0205-series,
          dt_nf_compl    TYPE zlest0205-docdat,
          peso_nf_compl  TYPE zlest0205-peso_fiscal,
          vlr_nf_compl   TYPE zlest0205-netwr,
          ct_aquav       TYPE zlest0205-ct_aquav,
          chave_nfe      TYPE zlest0205-chave_nfe,
          ch_referencia  TYPE zsdt0001-ch_referencia,

        END OF ty_saida_s_pend,


        BEGIN OF ty_cabecalho,
          bukrs         TYPE zsdt0001-bukrs,
          bukrstxt(50)  TYPE c,
          branch        TYPE zsdt0001-branch,
          branchtxt(50) TYPE c,
          nfnum         TYPE zsdt0001-nfnum,
          serie         TYPE zsdt0001-series,
          parid         TYPE zsdt0001-parid,
          paridtxt(50)  TYPE c,
          diferenca     TYPE zsdt0001-peso_subtotal,
          ch_referencia TYPE zsdt0001-ch_referencia,
        END OF ty_cabecalho,

        BEGIN OF ty_nf_complemento,
          nf_elet_n  TYPE c,
          nf_elet_s  TYPE c,
          chave_nfe  TYPE zib_nfe_dist_itm-chave_nfe,
          nr_nfe     TYPE zsdt0001-nfnum,
          serie      TYPE zsdt0001-series,
          fornecedor TYPE lfa1-lifnr,
          cfop       TYPE zsdt0001-cfop,
          dt_emissao TYPE zib_nfe_dist_ter-dt_emissao,
          material   TYPE zsdt0001-matnr,
          quantidade TYPE zib_nfe_dist_itm-prod_qtd_comerci,
          valor      TYPE zib_nfe_dist_itm-prod_vlr_total_b,
        END OF ty_nf_complemento,

        BEGIN OF ty_saida_0101,
          nfnum         TYPE zlest0205-nfnum,
          peso_fiscal   TYPE zlest0205-peso_fiscal,
          chave_nfe     TYPE zlest0205-chave_nfe,
          ch_referencia TYPE zsdt0001-ch_referencia,
        END OF ty_saida_0101,

        BEGIN OF ty_saida_0102,
          chave_nfe     TYPE zlest0205-chave_nfe,
          ch_referencia TYPE zsdt0001-ch_referencia,
          nr_romaneio   TYPE zsdt0001-nr_romaneio,
          dt_emissao    TYPE zsdt0001-docdat,
          nfnum         TYPE zlest0205-nfnum,
          peso_fiscal   TYPE zsdt0001-peso_fiscal,
          peso_subtotal TYPE zsdt0001-peso_subtotal,
          diferenca     TYPE zsdt0001-peso_subtotal,
          nf_compl      TYPE zlest0205-nfnum,
          saldo         TYPE zsdt0001-peso_subtotal,
          peso_compl    TYPE zsdt0001-peso_subtotal,
          parid         TYPE zsdt0001-parid,
          matnr         TYPE zsdt0001-matnr,
          cfop          TYPE zsdt0001-cfop,
        END OF ty_saida_0102,

        BEGIN OF ty_saida_0103,
          chave_nfe  TYPE zlest0205_ref-chave_nfe,
          peso_compl TYPE zlest0205_ref-peso_compl,
          icon_est   TYPE v_icon-name,
        END OF ty_saida_0103,

        BEGIN OF ty_saida,
          bukrs          TYPE zsdt0001-bukrs,
          branch         TYPE zsdt0001-branch,
          peso_fiscal    TYPE zsdt0001-peso_fiscal,
          peso_subtotal  TYPE zsdt0001-peso_subtotal,
          diferenca      TYPE zsdt0001-peso_subtotal,
          peso_compl     TYPE zsdt0001-peso_subtotal,
          saldo_compl    TYPE zsdt0001-peso_subtotal,
          dt_movimento   TYPE zsdt0001-dt_movimento,
          nr_romaneio    TYPE zsdt0001-nr_romaneio,
          nr_safra       TYPE zsdt0001-nr_safra,
          parid          TYPE zsdt0001-parid,
          matnr          TYPE zsdt0001-matnr,
          tp_transgenia  TYPE zsdt0001-tp_transgenia,
          desc_trans(30) TYPE c,
          nfnum          TYPE zsdt0001-nfnum,
          series         TYPE zsdt0001-series,
          docdat         TYPE zsdt0001-docdat,
          netwr          TYPE zsdt0001-netwr,
          placa_cav      TYPE zsdt0001-placa_cav,
          local_descarga TYPE zsdt0001-local_descarga,
          ch_referencia  TYPE zsdt0001-ch_referencia,
          cfop           TYPE zsdt0001-cfop,
          chave_nfe      TYPE zsdt0001-chave_nfe,
          dt_emissao     TYPE zsdt0001-docdat,
        END OF ty_saida.


  TYPES: BEGIN OF ty_transg,
           tp_transg       TYPE zsdt0001-tp_transgenia,
           desc_transg(30) TYPE c,
         END OF ty_transg.


  DATA: git_saida_c_pend     TYPE TABLE OF ty_saida_c_pend,
        git_saida_s_pend     TYPE TABLE OF ty_saida_s_pend,
        git_saida            TYPE TABLE OF ty_saida,
        git_saida_0101       TYPE TABLE OF ty_saida_0101,
        git_saida_0102       TYPE TABLE OF ty_saida_0102,
        git_saida_0103       TYPE TABLE OF ty_saida_0103,
        git_zlest0204        TYPE TABLE OF zlest0204,
        git_zsdt0001         TYPE TABLE OF zsdt0001,
        git_zlest0205        TYPE TABLE OF zlest0205,
        git_zlest0205_ref    TYPE TABLE OF zlest0205_ref,
        git_zlest0205_alv    TYPE TABLE OF zlest0205_ref,
        git_zlest0205_est    TYPE TABLE OF zlest0205_ref,
        git_zlest0205_aux    TYPE TABLE OF zlest0205,
        git_zlest0205_nf     TYPE TABLE OF zlest0205,
        git_t001             TYPE TABLE OF t001,
        git_t001w            TYPE TABLE OF t001w,
        git_lfa1             TYPE TABLE OF lfa1,
        git_zib_nfe_dist_ter TYPE TABLE OF zib_nfe_dist_ter,
        git_cabecalho        TYPE TABLE OF ty_cabecalho,
        git_nf_complemento   TYPE TABLE OF ty_nf_complemento,
        git_transg           TYPE TABLE OF ty_transg.


  DATA: gwa_saida_c_pend     TYPE ty_saida_c_pend,
        gwa_saida_s_pend     TYPE ty_saida_s_pend,
        gwa_saida            TYPE ty_saida,
        gwa_saida_aux        TYPE ty_saida,
        gwa_saida_0101       TYPE ty_saida_0101,
        gwa_saida_0102       TYPE ty_saida_0102,
        gwa_saida_0103       TYPE ty_saida_0103,
        gwa_zlest0204        TYPE zlest0204,
        gwa_zlest0205_save   TYPE zlest0205,
        gwa_zlest0205_ref    TYPE zlest0205_ref,
        gwa_zib_nfe_dist_ter TYPE zib_nfe_dist_ter,
        gwa_zib_nfe_dist_itm TYPE zib_nfe_dist_itm,
        gwa_cabecalho        TYPE ty_cabecalho,
        gwa_nf_complemento   TYPE ty_nf_complemento,
        gwa_zlest0205_aux    TYPE zlest0205,
        gwa_transg           TYPE ty_transg.

  DATA: vg_matnr TYPE char18.


  DATA: gvr_chave_nfe_texto   TYPE c LENGTH 100,
        gvr_total_diferenca   TYPE zsdt0001-peso_subtotal,
        gvr_soma_peso         TYPE zsdt0001-peso_subtotal,
        gvr_peso_fiscal       TYPE zlest0205-peso_fiscal,
        gvr_total_peso_fiscal TYPE zlest0205-peso_fiscal,
        gvr_peso_compl        TYPE zsdt0001-peso_subtotal,
        gvr_dif_total         TYPE c,
        gvr_estorno           TYPE c,
        gvr_aqua              TYPE c.


  "OBJETOS
  DATA: g_custom_container      TYPE REF TO cl_gui_custom_container,
        g_custom_container_0101 TYPE REF TO cl_gui_custom_container,
        g_custom_container_0102 TYPE REF TO cl_gui_custom_container,
        g_custom_container_0103 TYPE REF TO cl_gui_custom_container,
        grid                    TYPE REF TO cl_gui_alv_grid,
        grid_0101               TYPE REF TO cl_gui_alv_grid,
        grid_0102               TYPE REF TO cl_gui_alv_grid,
        grid_0103               TYPE REF TO cl_gui_alv_grid,
        dg_splitter_1           TYPE REF TO cl_gui_splitter_container,
        dg_parent_1             TYPE REF TO cl_gui_container,
        dg_splitter_2           TYPE REF TO cl_gui_splitter_container,
        dg_parent_2             TYPE REF TO cl_gui_container,
        dg_parent_2a            TYPE REF TO cl_gui_container,
        dg_parent_alv           TYPE REF TO cl_gui_container,
        picture                 TYPE REF TO cl_gui_picture,
        it_fieldcat             TYPE lvc_t_fcat,
        wa_fieldcat             TYPE lvc_s_fcat,
        it_fieldcat_0101        TYPE lvc_t_fcat,
        wa_fieldcat_0101        TYPE lvc_s_fcat,
        it_fieldcat_0102        TYPE lvc_t_fcat,
        wa_fieldcat_0102        TYPE lvc_s_fcat,
        it_fieldcat_0103        TYPE lvc_t_fcat,
        wa_fieldcat_0103        TYPE lvc_s_fcat,
        tl_function             TYPE ui_functions,
        wl_function             LIKE tl_function  WITH HEADER LINE,
        tl_function01           TYPE ui_functions,
        wl_function01           LIKE tl_function01  WITH HEADER LINE,
        it_sort                 TYPE lvc_t_sort,
        wa_sort                 LIKE LINE OF it_sort,
        dg_dyndoc_id            TYPE REF TO cl_dd_document,
        table_element           TYPE REF TO cl_dd_table_element,
        column                  TYPE REF TO cl_dd_area,
        table_element2          TYPE REF TO cl_dd_table_element,
        column_1                TYPE REF TO cl_dd_area,
        column_2                TYPE REF TO cl_dd_area,
        wa_layout               TYPE lvc_s_layo,
        wa_variant              TYPE disvariant,
        wa_variant_1            TYPE disvariant,
        wa_estilo               TYPE lvc_t_styl,
        wa_stable               TYPE lvc_s_stbl VALUE 'XX',
        dg_html_cntrl           TYPE REF TO cl_gui_html_viewer,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        ty_toolbar              TYPE stb_button,
        gwa_toolbar_01          TYPE stb_button,
        c_alv_toolbarmanager    TYPE REF TO cl_alv_grid_toolbar_manager.




  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: s_burks  FOR zsdt0001-bukrs         NO INTERVALS NO-EXTENSION  OBLIGATORY,
                    s_branch FOR j_1bbranch-branch      NO INTERVALS NO-EXTENSION  OBLIGATORY,
                    s_dt_mov FOR zsdt0001-dt_movimento  OBLIGATORY,
                    s_lifnr  FOR lfa1-lifnr             NO INTERVALS,
                    s_l_desc FOR zsdt0001-local_descarga NO INTERVALS,
                    s_matnr  FOR zsdt0001-matnr NO INTERVALS. "PBI - 62063 - CSB
    PARAMETERS: s_tprod  TYPE ty_transg-tp_transg." LIKE zsdt0001-tp_transgenia. "PBI - 62063 - CSB
  SELECTION-SCREEN END OF BLOCK b1.



  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: p_c_pend RADIOBUTTON GROUP g1,
                p_s_pend RADIOBUTTON GROUP g1.
  SELECTION-SCREEN END OF BLOCK b2.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_tprod.

*  DATA: t_mapping TYPE STANDARD TABLE OF dselc.
*
*  CLEAR: gwa_transg,git_transg.
*
*  gwa_transg-tp_transg   = 'CO'.
*  gwa_transg-desc_transg = 'CO-Convencional'.
*  APPEND gwa_transg TO git_transg.
*
*  gwa_transg-tp_transg   = 'R1-R2'.
*  gwa_transg-desc_transg = 'R1-R2-Transgênico'.
*  APPEND gwa_transg TO git_transg.
*
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield    = 'TP_TRANSG'
*      dynpprog    = sy-repid
*      dynpnr      = sy-dynnr
*      dynprofield = 'S_TPROD'
*      value_org   = 'S'
*    TABLES
*      value_tab   = git_transg
*      field_tab   = field_tab.


  DATA: it_fmap TYPE STANDARD TABLE OF dselc,
        it_ret  TYPE TABLE OF ddshretval.

  DATA:  tl_transg TYPE TABLE OF zde_psq_class_produto WITH HEADER LINE.

  CLEAR: it_fmap[].
  APPEND VALUE #( fldname = 'TP_CLASS'   dyfldname = 'Tipo' ) TO it_fmap.
  APPEND VALUE #( fldname = 'DESC_CLASS' dyfldname = 'Desrição' ) TO it_fmap.

  tl_transg-tp_class   = 'CO'.
  tl_transg-desc_class = 'CO-Convencional'.
  APPEND tl_transg.

  tl_transg-tp_class   = 'R1-R2'.
  tl_transg-desc_class = 'R1-R2-Transgênico'.
  APPEND tl_transg.


*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      ddic_structure  = 'ZDE_PSQ_CLASS_PRODUTO'
*      retfield        = 'TP_CLASS'
*      window_title    = 'Selecionar tipo:'
*      value_org       = 'S'
*      dynprofield     = 'S_TPROD'
*    TABLES
*      value_tab       = tl_transg[]
*      return_tab      = it_ret
*      dynpfld_mapping = it_fmap.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      ddic_structure  = 'ZDE_PSQ_CLASS_PRODUTO'
      retfield        = 'TP_CLASS'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'S_TPROD'
      value_org       = 'S'
    TABLES
      value_tab       = tl_transg[]
      return_tab      = it_ret
      dynpfld_mapping = it_fmap.

  IF sy-subrc IS INITIAL AND it_ret[] IS NOT INITIAL.
    READ TABLE tl_transg INTO DATA(wa_transg) WITH KEY tp_class = it_ret[ 1 ]-fieldval BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      s_tprod = wa_transg-tp_class.
    ENDIF.
  ENDIF.


START-OF-SELECTION.
  PERFORM  fm_busca_dados.

END-OF-SELECTION.
  CALL SCREEN 0100.


CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no sender.
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.
    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished  OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.


ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD on_hotspot_click.

    IF e_column_id EQ 'PESO_COMPL'.
      READ TABLE git_saida INTO DATA(gwa_saida_aux) INDEX e_row_id-index.
      IF sy-subrc EQ 0.
        " Buscar as notas vinculadas nesse romaneio.
        PERFORM fm_busca_nf_complemento USING gwa_saida_aux-ch_referencia.

        CALL SCREEN 0103 STARTING AT 10 08
                         ENDING AT   100 15.
      ENDIF.
    ELSE.
      IF e_column_id EQ 'ICON_EST'.
        DATA: var_answer  TYPE c.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirmação'
            text_question         = 'Deseja realmente estornar o(s) registro(s)?'
            text_button_1         = 'Sim'
            text_button_2         = 'Não'
            default_button        = '2'
            display_cancel_button = 'X'
          IMPORTING
            answer                = var_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF var_answer EQ '1'.
          READ TABLE git_saida_0103 INTO DATA(gwa_saida_0103) INDEX e_row_id-index.
          IF sy-subrc EQ 0.
            PERFORM fm_estorna_nf_complemento USING gwa_saida_0103-chave_nfe.

            IF gvr_estorno EQ abap_true.
              MESSAGE 'NF estornada com sucesso!' TYPE 'S'.
              PERFORM fm_busca_dados.
              CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).
              LEAVE TO SCREEN 0.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*    IF p_c_pend IS NOT INITIAL.
*
*      CLEAR: gwa_cabecalho, gwa_saida_aux, gwa_nf_complemento, gvr_chave_nfe_texto.
*
*      READ TABLE git_saida_c_pend INTO gwa_saida_c_pend INDEX e_row_id-index.
*      IF sy-subrc EQ 0.
*        MOVE gwa_saida_c_pend TO gwa_saida_aux.
*
*        gwa_saida_aux-matnr = |{ gwa_saida_aux-matnr ALPHA = OUT }|.
*        gwa_saida_aux-parid = |{ gwa_saida_aux-parid ALPHA = OUT }|.
*
*        READ TABLE git_t001 INTO DATA(gwa_t001) WITH KEY bukrs = gwa_saida_c_pend-bukrs.
*        IF sy-subrc EQ 0.
*          gwa_cabecalho-bukrs =  gwa_saida_c_pend-bukrs.
*          CONCATENATE  gwa_saida_c_pend-bukrs '-' gwa_t001-butxt INTO gwa_cabecalho-bukrstxt.
*        ENDIF.
*
*        READ TABLE git_t001w INTO DATA(gwa_t001w) WITH KEY werks = gwa_saida_c_pend-branch.
*        IF sy-subrc EQ 0.
*          gwa_cabecalho-branch =  gwa_saida_c_pend-branch.
*          CONCATENATE  gwa_saida_c_pend-branch '-' gwa_t001w-name1 INTO gwa_cabecalho-branchtxt.
*        ENDIF.
*
*        READ TABLE git_lfa1 INTO DATA(gwa_lfa1) WITH KEY lifnr = gwa_saida_c_pend-parid.
*        IF sy-subrc EQ 0.
*          gwa_cabecalho-parid =  gwa_saida_c_pend-parid.
*          CONCATENATE  gwa_saida_c_pend-parid '-' gwa_lfa1-name1 INTO gwa_cabecalho-paridtxt.
*        ENDIF.
*
*        gwa_cabecalho-nfnum         = gwa_saida_c_pend-nfnum.
*        gwa_cabecalho-serie         = gwa_saida_c_pend-series.
*        gwa_cabecalho-diferenca     = gwa_saida_c_pend-diferenca.
*        gwa_cabecalho-ch_referencia = gwa_saida_c_pend-ch_referencia.
*
*        PERFORM fm_busca_nf_complemento USING gwa_saida_c_pend-ch_referencia.
*
*        CALL SCREEN 0101.
*
*      ENDIF.
*    ENDIF.
  ENDMETHOD.
  METHOD on_data_changed.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
      WHERE fieldname EQ 'PESO_COMPL'.
      LOOP AT git_saida_0102 INTO DATA(gwa_saida_0102).

        CHECK wa_good_cells-row_id EQ sy-tabix.
        CASE wa_good_cells-fieldname.
          WHEN 'PESO_COMPL'.
            CLEAR: gvr_peso_compl.
            MOVE wa_good_cells-value TO gvr_peso_compl.
            IF gvr_peso_compl > gwa_saida_0102-diferenca.
              MESSAGE 'Peso Informado maior que valor da diferença!' TYPE 'I'.
              EXIT.
*            ELSE.
*              IF gvr_peso_compl < gwa_saida_0102-diferenca.
*                MESSAGE 'Peso Informado menor que valor da diferença!' TYPE 'I'.
*                EXIT.
            ELSE.
              gwa_saida_0102-saldo = gwa_saida_0102-diferenca - gvr_peso_compl.
              gwa_saida_0102-peso_compl =  gvr_peso_compl.
              MODIFY git_saida_0102 FROM gwa_saida_0102 INDEX wa_good_cells-row_id.
            ENDIF.
            "ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
    CALL METHOD grid_0102->refresh_table_display( is_stable = wa_stable ).
  ENDMETHOD.
  METHOD on_data_changed_finished.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,

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

    IF  p_s_pend IS NOT INITIAL.

*      CLEAR ty_toolbar.
*      ty_toolbar-butn_type = 3.
*      APPEND ty_toolbar TO e_object->mt_toolbar.
*
*      CLEAR ty_toolbar.
*      ty_toolbar-icon     = icon_storno.
*      ty_toolbar-function = 'ESTORNO'.
*      ty_toolbar-text     = 'Estorno'.
*      ty_toolbar-butn_type = 0.
*      APPEND ty_toolbar TO e_object->mt_toolbar.
    ELSE.

      CLEAR ty_toolbar.
      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.

      CLEAR ty_toolbar.
      ty_toolbar-icon     = icon_insert_relation.
      ty_toolbar-function = 'VINC_COMPL'.
      ty_toolbar-text     = 'Vincular Compl'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.

    ENDIF.

  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'ESTORNO'.
        PERFORM fm_estorno_nf.
      WHEN 'VINC_COMPL'.
        PERFORM fm_vinc_compl.
    ENDCASE.
    CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).
  ENDMETHOD.

ENDCLASS.

FORM fm_busca_dados.

  SELECT * FROM  zlest0204 INTO TABLE git_zlest0204
    WHERE bukrs  IN s_burks
    AND   branch IN s_branch
    AND   parid  IN s_lifnr.

  IF sy-subrc NE 0.
    MESSAGE 'Não existe parâmetro para esta Empresa, Filial e Fornecedor na Transação ZLES0188!' TYPE 'E'.
    EXIT.
  ELSE.

    SELECT * FROM zsdt0001 INTO TABLE git_zsdt0001
      FOR ALL ENTRIES IN git_zlest0204
      WHERE bukrs          EQ git_zlest0204-bukrs
      AND   branch         EQ git_zlest0204-branch
      AND   parid          EQ git_zlest0204-parid
      AND   dt_movimento   IN s_dt_mov
      AND   local_descarga IN s_l_desc
      AND   tp_movimento   EQ 'E'
      "AND   tp_transgenia  IN s_tprod
      AND   matnr          IN s_matnr .

    IF sy-subrc EQ 0.

      IF s_tprod IS NOT INITIAL.
        IF s_tprod = 'CO'.
          DELETE git_zsdt0001 WHERE tp_transgenia <> 'CO'.
        ENDIF.
        IF s_tprod <> 'CO'.
          DELETE git_zsdt0001 WHERE tp_transgenia = 'CO'.
        ENDIF.
      ENDIF.

      SELECT * FROM t001 INTO TABLE git_t001
        FOR ALL ENTRIES IN git_zsdt0001
       WHERE bukrs EQ git_zsdt0001-bukrs.

      SELECT * FROM t001w INTO TABLE git_t001w
       FOR ALL ENTRIES IN git_zsdt0001
      WHERE werks EQ git_zsdt0001-branch.

      SELECT * FROM lfa1 INTO TABLE git_lfa1
       FOR ALL ENTRIES IN git_zsdt0001
      WHERE lifnr EQ git_zsdt0001-parid.


      SELECT * FROM zlest0205_ref INTO TABLE git_zlest0205_ref
         FOR ALL ENTRIES IN git_zsdt0001
        WHERE ch_referencia EQ git_zsdt0001-ch_referencia.

      PERFORM fm_trata_dados.


**      IF p_c_pend IS NOT INITIAL.
      "CS2020001103 - INICIO - AL
*        SELECT * FROM ZLEST0205 INTO TABLE GIT_ZLEST0205
*           FOR ALL ENTRIES IN GIT_ZSDT0001
*          WHERE CH_REFERENCIA EQ GIT_ZSDT0001-CH_REFERENCIA.

*        PERFORM fm_trata_dados_c_pend.

*      ELSE.
*        SELECT * FROM zlest0205_ref INTO TABLE git_zlest0205_ref
*           FOR ALL ENTRIES IN git_zsdt0001
*          WHERE ch_referencia EQ git_zsdt0001-ch_referencia.

*        SELECT * FROM zlest0205 INTO TABLE git_zlest0205
*           FOR ALL ENTRIES IN git_zsdt0001
*          WHERE ch_referencia EQ git_zsdt0001-ch_referencia.

*       PERFORM fm_trata_dados_s_pend.
*      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM fm_trata_dados_c_pend.

  REFRESH git_saida_c_pend.

  LOOP AT git_zsdt0001 INTO DATA(gwa_zsdt0001).
    CLEAR: gwa_saida_c_pend, gvr_total_diferenca, gvr_soma_peso.

*    LOOP AT  GIT_ZLEST0205_ INTO DATA(GWA_ZLEST0205)
    LOOP AT  git_zlest0205_ref INTO DATA(gwa_zlest0205_ref)
      WHERE ch_referencia = gwa_zsdt0001-ch_referencia.
*      GVR_SOMA_PESO = GVR_SOMA_PESO +  GWA_ZLEST0205-PESO_FISCAL.
      gvr_soma_peso = gvr_soma_peso +  gwa_zlest0205_ref-peso_compl.
    ENDLOOP.

    "gwa_saida_c_pend-peso_compl = gvr_soma_peso. ???? CSB

    gvr_total_diferenca = ( gwa_zsdt0001-peso_subtotal - gwa_zsdt0001-peso_fiscal ).
    "GVR_TOTAL_DIFERENCA - INICIO - AL
*    IF ( ( GVR_TOTAL_DIFERENCA - GVR_SOMA_PESO ) <= 0  AND  ( GVR_TOTAL_DIFERENCA <= 0 ) ).
*      CONTINUE.
*    ELSE.
    gwa_saida_c_pend-diferenca  = COND #( WHEN gvr_soma_peso EQ 0 THEN gvr_total_diferenca ELSE ( gvr_total_diferenca - gvr_soma_peso )  ).
*    ENDIF.
    "GVR_TOTAL_DIFERENCA - FIM    - AL
    gwa_saida_c_pend-bukrs           = gwa_zsdt0001-bukrs.
    gwa_saida_c_pend-branch          = gwa_zsdt0001-branch.
    gwa_saida_c_pend-peso_fiscal     = gwa_zsdt0001-peso_fiscal.
    gwa_saida_c_pend-peso_subtotal   = gwa_zsdt0001-peso_subtotal.
    gwa_saida_c_pend-nf_compl        = icon_led_yellow.
    gwa_saida_c_pend-dt_movimento    = gwa_zsdt0001-dt_movimento.
    gwa_saida_c_pend-nr_romaneio     = gwa_zsdt0001-nr_romaneio.
    gwa_saida_c_pend-nr_safra        = gwa_zsdt0001-nr_safra.
    gwa_saida_c_pend-parid           = gwa_zsdt0001-parid.
    gwa_saida_c_pend-matnr           = gwa_zsdt0001-matnr.
    gwa_saida_c_pend-nfnum           = gwa_zsdt0001-nfnum.
    gwa_saida_c_pend-series          = gwa_zsdt0001-series.
    gwa_saida_c_pend-docdat          = gwa_zsdt0001-docdat.
    gwa_saida_c_pend-netwr           = gwa_zsdt0001-netwr.
    gwa_saida_c_pend-placa_cav       = gwa_zsdt0001-placa_cav.
    gwa_saida_c_pend-local_descarga  = gwa_zsdt0001-local_descarga.
    gwa_saida_c_pend-tp_transgenia   = gwa_zsdt0001-tp_transgenia.

    IF gwa_saida_c_pend-tp_transgenia = 'CO'.
      gwa_saida_c_pend-desc_trans   =  'CO-Convencional'.
    ELSE.
      gwa_saida_c_pend-desc_trans   = 'R1-R2-Transgênico'..
    ENDIF.

    gwa_saida_c_pend-ch_referencia   = gwa_zsdt0001-ch_referencia.
    gwa_saida_c_pend-cfop            = gwa_zsdt0001-cfop.
    gwa_saida_c_pend-chave_nfe      = gwa_zsdt0001-chave_nfe.
    gwa_saida_c_pend-dt_emissao     = gwa_zsdt0001-docdat.

    CLEAR gwa_zlest0205_ref.

*    READ TABLE GIT_ZLEST0205 INTO GWA_ZLEST0205 WITH KEY CH_REFERENCIA =  GWA_ZSDT0001-CH_REFERENCIA.
    READ TABLE git_zlest0205_ref INTO gwa_zlest0205_ref WITH KEY ch_referencia =  gwa_zsdt0001-ch_referencia.
    IF sy-subrc EQ 0.
*      IF GWA_ZLEST0205-DIF_TOTAL NE 'X'.
*      IF gwa_zlest0205_ref-dif_total NE 'X'.
*        APPEND gwa_saida_c_pend TO git_saida_c_pend.
*      ENDIF.
*    ELSE.
*      APPEND gwa_saida_c_pend TO git_saida_c_pend.
    ENDIF.

  ENDLOOP.
ENDFORM.

FORM fm_trata_dados_s_pend.

  REFRESH git_saida_s_pend.
  CLEAR gwa_saida_s_pend.


  LOOP AT git_zlest0205 INTO DATA(gwa_zlest0205).

    LOOP AT git_zlest0205_ref INTO DATA(gwa_zlest0205_ref) WHERE chave_nfe = gwa_zlest0205-chave_nfe .

      CLEAR  gwa_saida_s_pend.

      gwa_saida_s_pend-nf_compl          = gwa_zlest0205-nfnum.
      gwa_saida_s_pend-serie_nf_compl    = gwa_zlest0205-series.
      gwa_saida_s_pend-dt_nf_compl       = gwa_zlest0205-docdat.
      gwa_saida_s_pend-peso_nf_compl     = gwa_zlest0205-peso_fiscal.
      gwa_saida_s_pend-vlr_nf_compl      = gwa_zlest0205-netwr.
      gwa_saida_s_pend-ct_aquav          = gwa_zlest0205-ct_aquav.
      gwa_saida_s_pend-chave_nfe         = gwa_zlest0205-chave_nfe.
      gwa_saida_s_pend-ch_referencia     = gwa_zlest0205_ref-ch_referencia.

      READ TABLE git_zsdt0001 INTO DATA(gwa_zsdt0001) WITH KEY ch_referencia = gwa_zlest0205_ref-ch_referencia                                                             .
      IF sy-subrc EQ 0.
        gwa_saida_s_pend-bukrs             = gwa_zsdt0001-bukrs.
        gwa_saida_s_pend-branch            = gwa_zsdt0001-branch.
        gwa_saida_s_pend-peso_fiscal       = gwa_zsdt0001-peso_fiscal.
        gwa_saida_s_pend-peso_subtotal     = gwa_zsdt0001-peso_subtotal.
        gwa_saida_s_pend-diferenca         = ( gwa_zsdt0001-peso_subtotal - gwa_zsdt0001-peso_fiscal ).
        gwa_saida_s_pend-dt_movimento      = gwa_zsdt0001-dt_movimento.
        gwa_saida_s_pend-nr_romaneio       = gwa_zsdt0001-nr_romaneio.
        gwa_saida_s_pend-nr_safra          = gwa_zsdt0001-nr_safra.
        gwa_saida_s_pend-parid             = gwa_zsdt0001-parid.
        gwa_saida_s_pend-matnr             = gwa_zsdt0001-matnr.
        gwa_saida_s_pend-nfnum             = gwa_zsdt0001-nfnum.
        gwa_saida_s_pend-series            = gwa_zsdt0001-series.
        gwa_saida_s_pend-dt_emissao        = gwa_zsdt0001-docdat.
        gwa_saida_s_pend-vlr_nf            = gwa_zsdt0001-netwr.
        gwa_saida_s_pend-placa_cav         = gwa_zsdt0001-placa_cav.
        gwa_saida_s_pend-local_descarga    = gwa_zsdt0001-local_descarga.
      ENDIF.

      APPEND gwa_saida_s_pend TO git_saida_s_pend.
    ENDLOOP.
  ENDLOOP.

  SORT git_saida_s_pend BY nfnum DESCENDING.

* PBI - 55740 - Incio
*  REFRESH git_saida_s_pend.
*  CLEAR gwa_saida_s_pend.
*
*
*  LOOP AT git_zlest0205 INTO DATA(gwa_zlest0205).
*
*    CLEAR  gwa_saida_s_pend.
*
*    gwa_saida_s_pend-nf_compl          = gwa_zlest0205-nfnum.
*    gwa_saida_s_pend-serie_nf_compl    = gwa_zlest0205-series.
*    gwa_saida_s_pend-dt_nf_compl       = gwa_zlest0205-docdat.
*    gwa_saida_s_pend-peso_nf_compl     = gwa_zlest0205-peso_fiscal.
*    gwa_saida_s_pend-vlr_nf_compl      = gwa_zlest0205-netwr.
*    gwa_saida_s_pend-ct_aquav          = gwa_zlest0205-ct_aquav.
*    gwa_saida_s_pend-chave_nfe         = gwa_zlest0205-chave_nfe.
*    gwa_saida_s_pend-ch_referencia     = gwa_zlest0205-ch_referencia.
*
*    READ TABLE git_zsdt0001 INTO DATA(gwa_zsdt0001) WITH KEY ch_referencia = gwa_zlest0205-ch_referencia                                                             .
*    IF sy-subrc EQ 0.
*      gwa_saida_s_pend-bukrs             = gwa_zsdt0001-bukrs.
*      gwa_saida_s_pend-branch            = gwa_zsdt0001-branch.
*      gwa_saida_s_pend-peso_fiscal       = gwa_zsdt0001-peso_fiscal.
*      gwa_saida_s_pend-peso_subtotal     = gwa_zsdt0001-peso_subtotal.
*      gwa_saida_s_pend-diferenca         = ( gwa_zsdt0001-peso_subtotal - gwa_zsdt0001-peso_fiscal ).
*      gwa_saida_s_pend-dt_movimento      = gwa_zsdt0001-dt_movimento.
*      gwa_saida_s_pend-nr_romaneio       = gwa_zsdt0001-nr_romaneio.
*      gwa_saida_s_pend-nr_safra          = gwa_zsdt0001-nr_safra.
*      gwa_saida_s_pend-parid             = gwa_zsdt0001-parid.
*      gwa_saida_s_pend-matnr             = gwa_zsdt0001-matnr.
*      gwa_saida_s_pend-nfnum             = gwa_zsdt0001-nfnum.
*      gwa_saida_s_pend-series            = gwa_zsdt0001-series.
*      gwa_saida_s_pend-dt_emissao        = gwa_zsdt0001-docdat.
*      gwa_saida_s_pend-vlr_nf            = gwa_zsdt0001-netwr.
*      gwa_saida_s_pend-placa_cav         = gwa_zsdt0001-placa_cav.
*      gwa_saida_s_pend-local_descarga    = gwa_zsdt0001-local_descarga.
*    ENDIF.
*
*    APPEND gwa_saida_s_pend TO git_saida_s_pend.
*  ENDLOOP.
*
*  SORT git_saida_s_pend BY nfnum DESCENDING.
* PBI - 55740 - Fim
ENDFORM.

FORM fm_trata_dados.

  REFRESH git_saida.

  LOOP AT git_zsdt0001 INTO DATA(gwa_zsdt0001).

    CLEAR: gwa_saida_c_pend, gvr_total_diferenca, gvr_soma_peso,
           gwa_zlest0204.

*-PBI 66101 - 01.10.2021 - JT - inicio
    READ TABLE git_zlest0204 INTO gwa_zlest0204
                             WITH KEY bukrs        = gwa_zsdt0001-bukrs
                                      branch       = gwa_zsdt0001-branch
                                      parid        = gwa_zsdt0001-parid
                                      tp_movimento = 'E'.
    IF gwa_zlest0204-desc_classif = abap_true.
      gwa_zsdt0001-peso_subtotal =   gwa_zsdt0001-peso_subtotal
                                 - ( gwa_zsdt0001-nr_qtd_umidade + gwa_zsdt0001-nr_qtd_impureza +
                                     gwa_zsdt0001-nr_qtd_avaria  + gwa_zsdt0001-nr_qtd_ardido   +
                                     gwa_zsdt0001-nr_qtd_quebra  + gwa_zsdt0001-nr_qtd_esverd ).
    ENDIF.
*-PBI 66101 - 01.10.2021 - JT - fim

    LOOP AT  git_zlest0205_ref INTO DATA(gwa_zlest0205_ref)
      WHERE ch_referencia = gwa_zsdt0001-ch_referencia.
      gvr_soma_peso = gvr_soma_peso +  gwa_zlest0205_ref-peso_compl.
    ENDLOOP.

    gvr_total_diferenca = ( gwa_zsdt0001-peso_subtotal - gwa_zsdt0001-peso_fiscal ).
    gwa_saida-diferenca       = gvr_total_diferenca.
    gwa_saida-peso_compl      = gvr_soma_peso.
    gwa_saida-saldo_compl     = gwa_saida-diferenca - gvr_soma_peso  .
    gwa_saida-bukrs           = gwa_zsdt0001-bukrs.
    gwa_saida-branch          = gwa_zsdt0001-branch.
    gwa_saida-peso_fiscal     = gwa_zsdt0001-peso_fiscal.
    gwa_saida-peso_subtotal   = gwa_zsdt0001-peso_subtotal.
    gwa_saida-dt_movimento    = gwa_zsdt0001-dt_movimento.
    gwa_saida-nr_romaneio     = gwa_zsdt0001-nr_romaneio.
    gwa_saida-nr_safra        = gwa_zsdt0001-nr_safra.
    gwa_saida-parid           = gwa_zsdt0001-parid.
    gwa_saida-matnr           = gwa_zsdt0001-matnr.
    gwa_saida-nfnum           = gwa_zsdt0001-nfnum.
    gwa_saida-series          = gwa_zsdt0001-series.
    gwa_saida-docdat          = gwa_zsdt0001-docdat.
    gwa_saida-netwr           = gwa_zsdt0001-netwr.
    gwa_saida-placa_cav       = gwa_zsdt0001-placa_cav.
    gwa_saida-local_descarga  = gwa_zsdt0001-local_descarga.
    gwa_saida-tp_transgenia   = gwa_zsdt0001-tp_transgenia.

    IF gwa_saida-tp_transgenia = 'CO'.
      gwa_saida-desc_trans   =  'CO-Convencional'.
    ELSE.
      gwa_saida-desc_trans   = 'R1-R2-Transgênico'..
    ENDIF.


    gwa_saida-ch_referencia   = gwa_zsdt0001-ch_referencia.
    gwa_saida-cfop            = gwa_zsdt0001-cfop.
    gwa_saida-chave_nfe       = gwa_zsdt0001-chave_nfe.
    gwa_saida-dt_emissao      = gwa_zsdt0001-docdat.

    CLEAR gwa_zlest0205_ref.

    APPEND gwa_saida TO git_saida.
  ENDLOOP.

  IF p_c_pend = 'X'.
    DELETE git_saida WHERE saldo_compl = 0.
  ELSE.
    DELETE git_saida WHERE saldo_compl <> 0.
  ENDIF.

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
  SET TITLEBAR 'TL_0100'.

  PERFORM fm_alv.


  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container EXPORTING container_name = 'CONTAINER'.

    "CRIANDO CABEÇALHO
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
        height = 25.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 50.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM fm_pega_logo USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.


    wl_function  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function  = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    CREATE OBJECT grid
      EXPORTING
        i_parent = dg_parent_alv.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid.

    SET HANDLER: obg_toolbar->on_toolbar FOR grid,
                 obg_toolbar->handle_user_command FOR grid.



    wa_variant_1-report = sy-repid.
    wa_variant_1-username = sy-uname.

    CALL METHOD grid->set_table_for_first_display
      EXPORTING
        i_save               = 'X'
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
        is_variant           = wa_variant_1
      CHANGING
        it_outtab            = git_saida
        it_fieldcatalog      = it_fieldcat.

    SET HANDLER: lcl_handler=>on_hotspot_click FOR grid.


*    IF p_c_pend IS NOT INITIAL.
*
*      wa_variant-report = sy-repid.
*      wa_variant-username = sy-uname.
*
*
*      CALL METHOD grid->set_table_for_first_display
*        EXPORTING
*          i_save               = 'X'
*          is_layout            = wa_layout
*          it_toolbar_excluding = tl_function
*          is_variant           = wa_variant
*        CHANGING
*          it_outtab            = git_saida_c_pend
*          it_fieldcatalog      = it_fieldcat.
*
*      SET HANDLER: lcl_handler=>on_hotspot_click FOR grid.
*
*    ELSE.
*
*      wa_variant_1-report = sy-repid.
*      wa_variant_1-username = sy-uname.
*
*      CALL METHOD grid->set_table_for_first_display
*        EXPORTING
*          i_save               = 'X'
*          is_layout            = wa_layout
*          it_toolbar_excluding = tl_function
*          is_variant           = wa_variant_1
*        CHANGING
*          it_outtab            = git_saida_s_pend
*          it_fieldcatalog      = it_fieldcat.
*
*    ENDIF.


    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'GRID'.

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

    IF p_c_pend IS NOT INITIAL.
      p_text = 'Com Pendência Fisico/Fiscal'.
    ELSE.
      p_text = 'Sem Pendência Fisico/Fiscal'.
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

    PERFORM fm_top-of-page.

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

    CALL METHOD grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.
    CALL METHOD grid->set_frontend_fieldcatalog( it_fieldcatalog = it_fieldcat ).

    CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).

  ENDIF.
ENDMODULE.

FORM fm_pega_logo USING nome_logo CHANGING url.

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
*&      Form  FR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_alv.


  REFRESH it_fieldcat.

  PERFORM fm_preenche_cat USING :
          'BUKRS'            'Empresa'              '07'  ''   ''  ''  ''  ''   ''  '' '',
          'BRANCH'           'Filial'               '07'  ''   ''  ''  ''  ''   ''  '' '',
          'PESO_FISCAL'      'Peso Fiscal'          '12'  ''   ''  ''  ''  ''   ''  '' '',
          'PESO_SUBTOTAL'    'Peso Físico'          '12'  ''   ''  ''  ''  ''   ''  '' '',
          'DIFERENCA'        'Diferença'            '12'  ''   ''  ''  ''  ''   ''  '' '',
          'SALDO_COMPL'      'Saldo à Complementar' '20'  ''   ''  ''  ''  ''   ''  '' '',
          'PESO_COMPL'       'Peso Complementado'   '20'  ''   'X'  ''  ''  ''   '' '' '',
          'DT_MOVIMENTO'     'Dt Movimento'         '12'  ''   ''  ''  ''  ''   ''  '' '',
          'NR_ROMANEIO'      'Nr.Romaneio'          '12'  ''   ''  ''  ''  ''   ''  '' '',
          'NR_SAFRA'         'Safra'                '06'  ''   ''  ''  ''  ''   ''  '' '',
          'PARID'            'Fornecedor'           '12'  ''  ''  ''  ''  ''   ''  ''  '',
          'MATNR'            'Material'             '25'  ''  ''  ''  ''  ''   ''  ''  '',
          'DESC_TRANS'       'Classif. Produto'     '25'  ''  ''  ''  ''  ''   ''  ''  '',
          'NFNUM'            'NF'                   '12'  ''   ''  ''  ''  ''   ''  '' '',
          'SERIES'           'Serie'                '06'  ''   ''  ''  ''  ''   ''  '' '',
          'DOCDAT'           'Dt Emissão'           '12'  ''   ''  ''  ''  ''   ''  '' '',
          'NETWR'            'Valor NF'             '12'  ''   ''  ''  ''  ''   ''  '' '',
          'PLACA_CAV'        'Placa Cav'            '10'  ''   ''  ''  ''  ''   ''  '' '',
          'LOCAL_DESCARGA'   'Local Descarga'       '10'  ''   ''  ''  ''  ''   ''  '' ''.

*  IF p_c_pend IS NOT INITIAL.
*    PERFORM fm_preenche_cat USING :
*          'BUKRS'            'Empresa'          '07'  ''   ''  ''  ''  ''   ''  '',
*          'BRANCH'           'Filial'           '07'  ''   ''  ''  ''  ''   ''  '',
*          'PESO_FISCAL'      'Peso Fiscal'      '12'  ''   ''  ''  ''  ''   ''  '',
*          'PESO_SUBTOTAL'    'Peso Físico'      '12'  ''   ''  ''  ''  ''   ''  '',
*          'PESO_COMPL'       'Peso Complemento' '17'  ''   ''  ''  ''  ''   ''  '',
*          'DIFERENCA'        'Diferença'        '12'  ''   ''  ''  ''  ''   ''  '',
*          'DT_MOVIMENTO'     'Dt Movimento'     '12'  ''   ''  ''  ''  ''   ''  '',
*          'NR_ROMANEIO'      'Nr.Romaneio'      '12'  ''   ''  ''  ''  ''   ''  '',
*          'NR_SAFRA'         'Safra'            '06'  ''   ''  ''  ''  ''   ''  '',
*          'PARID'            'Fornecedor'       '12'  'X'  ''  ''  ''  ''   ''  '',
*          'MATNR'            'Material'         '12'  'X'  ''  ''  ''  ''   ''  '',
*          'NFNUM'            'NF'               '12'  ''   ''  ''  ''  ''   ''  '',
*          'SERIES'           'Serie'            '06'  ''   ''  ''  ''  ''   ''  '',
*          'DOCDAT'           'Dt Emissão'       '12'  ''   ''  ''  ''  ''   ''  '',
*          'NETWR'            'Valor NF'         '12'  ''   ''  ''  ''  ''   ''  '',
*          'PLACA_CAV'        'Placa Cav'        '10'  ''   ''  ''  ''  ''   ''  '',
*          'LOCAL_DESCARGA'   'Local Descarga'   '10'  ''   ''  ''  ''  ''   ''  ''.
*
*  ELSE.
*
*    PERFORM fm_preenche_cat USING :
*          'BUKRS'           'Empresa'           '07'  ''   ''  ''  ''  ''   ''  '',
*          'BRANCH'          'Filial'            '07'  ''   ''  ''  ''  ''   ''  '',
*          'PESO_FISCAL'     'Peso Fiscal'       '12'  ''   ''  ''  ''  ''   ''  '',
*          'PESO_SUBTOTAL'   'Peso Físico'       '12'  ''   ''  ''  ''  ''   ''  '',
*          'DIFERENCA'       'Diferença'         '12'  ''   ''  ''  ''  ''   ''  '',
*          'DT_MOVIMENTO'    'Dt Movimento'      '12'  ''   ''  ''  ''  ''   ''  '',
*          'NR_ROMANEIO'     'Nr.Romaneio'       '12'  ''   ''  ''  ''  ''   ''  '',
*          'NR_SAFRA'        'Safra'             '06'  ''   ''  ''  ''  ''   ''  '',
*          'PARID'           'Fornecedor'        '12'  'X'  ''  ''  ''  ''   ''  '',
*          'MATNR'           'Material'          '12'  'X'  ''  ''  ''  ''   ''  '',
*          'NFNUM'           'NF'                '12'  ''   ''  ''  ''  ''   ''  '',
*          'SERIES'          'Serie'             '06'  ''   ''  ''  ''  ''   ''  '',
*          'DT_EMISSAO'      'Dt Emissão'        '12'  ''   ''  ''  ''  ''   ''  '',
*          'VLR_NF'          'Valor NF'          '12'  ''   ''  ''  ''  ''   ''  '',
*          'PLACA_CAV'       'Placa Cav'         '10'  ''   ''  ''  ''  ''   ''  '',
*          'LOCAL_DESCARGA'  'Local Descarga'    '14'  ''   ''  ''  ''  ''   ''  '',
*          'NF_COMPL'        'NF Compl'         '10'  ''   ''  ''  ''  ''   ''  '',
*          'SALDO'           'Saldo'            '10'  ''   ''  ''  ''  ''   ''  ''.
**        'NF_COMPL'        'NF Compl'          '10'  ''   ''  ''  ''  ''   ''  '',
**        'SERIE_NF_COMPL'  'Serie NF Compl.'   '15'  ''   ''  ''  ''  ''   ''  '',
**        'DT_NF_COMPL'     'Data NF Compl.'    '14'  ''   ''  ''  ''  ''   ''  '',
**        'PESO_NF_COMPL'   'Peso NF Compl.'    '14'  ''   ''  ''  ''  ''   ''  '',
**        'VLR_NF_COMPL'    'Valor NF Compl.'   '15'  ''   ''  ''  ''  ''   ''  ''.
*
*  ENDIF.

ENDFORM.

FORM fm_preenche_cat USING VALUE(p_campo)
                           VALUE(p_desc)
                           VALUE(p_tam)
                           VALUE(p_zero)
                           VALUE(p_hot)
                           VALUE(p_icon)
                           VALUE(p_just)
                           VALUE(p_table)
                           VALUE(p_fieldname)
                           VALUE(p_f4)
                           VALUE(p_cfield).

  wa_fieldcat-fieldname   = p_campo.
  wa_fieldcat-coltext     = p_desc.
  wa_fieldcat-scrtext_l   = p_desc.
  wa_fieldcat-scrtext_m   = p_desc.
  wa_fieldcat-scrtext_s   = p_desc.
  wa_fieldcat-outputlen   = p_tam.
  wa_fieldcat-no_zero     = p_zero.
  wa_fieldcat-hotspot     = p_hot.
  wa_fieldcat-icon        = p_icon.
  wa_fieldcat-just        = p_just.
  wa_fieldcat-no_zero     = p_zero.
  wa_fieldcat-ref_table   = p_table.
  wa_fieldcat-ref_field   = p_fieldname.
  wa_fieldcat-f4availabl  = p_f4.
  wa_fieldcat-cfieldname  = p_cfield.

  APPEND wa_fieldcat TO it_fieldcat.

ENDFORM.

FORM fm_alv_0101.

  REFRESH it_fieldcat_0101.

  it_fieldcat_0101 = VALUE lvc_t_fcat(
   ( fieldname = 'NFNUM'        coltext = 'NF Lançadas'     outputlen = '12'     )
   ( fieldname = 'PESO_FISCAL'  coltext = 'Quantidade'      outputlen = '15'  do_sum = 'X' ) ).

ENDFORM.

FORM fm_alv_0102.

  REFRESH it_fieldcat_0102.

  it_fieldcat_0102 = VALUE lvc_t_fcat(
  (  fieldname = 'CHAVE_NFE'      coltext = 'Chave Nfe'       outputlen = '12'     )
  (  fieldname = 'CH_REFERENCIA'  coltext = 'Ch Referencia'   outputlen = '12'     )
  (  fieldname = 'NR_ROMANEIO'    coltext = 'Romaneio'        outputlen = '12'     )
  (  fieldname = 'DT_EMISSAO'     coltext = 'Dt Movimento'    outputlen = '12'     )
  (  fieldname = 'NFNUM'          coltext = 'NF Lançadas'     outputlen = '12'     )
  (  fieldname = 'PESO_FISCAL'    coltext = 'Peso Fiscal'     outputlen = '12'     )
  (  fieldname = 'PESO_SUBTOTAL'  coltext = 'Peso Subt.'      outputlen = '12'     )
  (  fieldname = 'DIFERENCA'      coltext = 'Diferença'       outputlen = '12' do_sum = abap_true datatype = 'QUAN'  )
  (  fieldname = 'NF_COMPL'       coltext = 'Nfs Compl'       outputlen = '12'     )
  (  fieldname = 'SALDO'          coltext = 'Saldo'           outputlen = '12' do_sum = abap_true  datatype = 'QUAN'  )
  (  fieldname = 'PESO_COMPL'     coltext = 'Peso Compl'      outputlen = '12' do_sum = abap_true  edit = abap_true  ref_table   = 'VBAP' ref_field = 'KWMENG' ) ).

ENDFORM.

FORM fm_alv_0103.

  REFRESH it_fieldcat_0103.

  it_fieldcat_0103 = VALUE lvc_t_fcat(
  (  fieldname = 'CHAVE_NFE'      coltext = 'Chave Nfe Complemento'      outputlen = '50'     )
  (  fieldname = 'PESO_COMPL'     coltext = 'Peso Complementado'         outputlen = '12' do_sum = abap_true  datatype = 'QUAN'  )
  (  fieldname = 'ICON_EST'       coltext = 'Estornar'                   outputlen = '12' hotspot = abap_true     ) ).

ENDFORM.


FORM fm_top-of-page.
  DATA: gvr_mov_de(10)  TYPE c,
        gvr_mov_ate(10) TYPE c.

  CLEAR: p_text_table, gvr_mov_de, gvr_mov_ate.

  sdydo_text_element =  ' '.
  APPEND sdydo_text_element TO p_text_table.


  READ TABLE git_t001 INTO DATA(gwa_t001) WITH KEY bukrs = s_burks-low.
  IF sy-subrc EQ 0.
    CONCATENATE 'Empresa: ' s_burks-low '-' gwa_t001-butxt INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.

  READ TABLE git_t001w INTO DATA(gwa_t001w) WITH KEY werks = s_branch-low.
  IF sy-subrc EQ 0.
    CONCATENATE 'Filial: ' s_branch-low '-' gwa_t001w-name1 INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.

  CONCATENATE s_dt_mov-low+6(2)  '.' s_dt_mov-low+4(2)  '.'  s_dt_mov-low(4)  INTO gvr_mov_de.
  CONCATENATE s_dt_mov-high+6(2) '.' s_dt_mov-high+4(2) '.'  s_dt_mov-high(4) INTO gvr_mov_ate.
  CONCATENATE 'Periodo: ' gvr_mov_de 'a'  gvr_mov_ate INTO sdydo_text_element SEPARATED BY space.
  APPEND sdydo_text_element TO p_text_table.

  IF s_lifnr IS NOT INITIAL.
    READ TABLE git_lfa1 INTO DATA(gwa_lfa1) WITH KEY lifnr = s_lifnr-low.
    IF sy-subrc EQ 0.
      CONCATENATE 'Fornecedor: ' s_lifnr-low '-' gwa_lfa1-name1  INTO sdydo_text_element SEPARATED BY space.
      APPEND sdydo_text_element TO p_text_table.
    ENDIF.
  ENDIF.

  IF s_l_desc IS NOT INITIAL.
    CONCATENATE 'Local de Descarga:' s_l_desc-low INTO  sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_SAVE'.
      PERFORM fm_save.
    WHEN 'BTN_CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_ESTORNO'.

      CALL METHOD grid_0101->get_selected_rows
        IMPORTING
          et_row_no = DATA(it_row_0101).

      IF it_row_0101[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
        EXIT.
      ELSE.

        READ TABLE  it_row_0101 INTO DATA(wa_row_0101) INDEX 1.
        IF sy-subrc EQ 0.

          READ TABLE git_saida_0101 INTO gwa_saida_0101 INDEX wa_row_0101-row_id.
          IF sy-subrc EQ 0.
            CLEAR gvr_estorno.

            DELETE FROM zlest0205
              WHERE chave_nfe = gwa_saida_0101-chave_nfe.
* PBI - 55740 - Inicio
*            UPDATE zlest0205 SET dif_total = ''
*               WHERE ch_referencia EQ gwa_saida_0101-ch_referencia.
* PBI - 55740 - Fim

            COMMIT WORK.

            gvr_estorno = abap_true.
          ENDIF.
        ENDIF.

        IF  gvr_estorno EQ abap_true.
          MESSAGE 'Dados estornado com sucesso!' TYPE 'S'.

          PERFORM fm_busca_nf_complemento  USING gwa_saida_0101-ch_referencia.
          PERFORM fm_busca_dados.
* PBI - 55740 - Inicio
          "PERFORM fm_valida_nf_compl  USING gwa_saida_0101-ch_referencia CHANGING gvr_dif_total.
* PBI - 55740 - Fim
          CALL METHOD grid_0101->refresh_table_display( is_stable = wa_stable ).
          CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).
        ENDIF.
      ENDIF.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_SAVE'.
      PERFORM fm_save.
    WHEN 'BTN_CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_ESTORNO'.

      CALL METHOD grid_0102->get_selected_rows
        IMPORTING
          et_row_no = DATA(it_row_0102).

      IF it_row_0102[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
        EXIT.
      ELSE.

        READ TABLE  it_row_0102 INTO DATA(wa_row_0102) INDEX 1.
        IF sy-subrc EQ 0.

          READ TABLE git_saida_0102 INTO gwa_saida_0102 INDEX wa_row_0102-row_id.
          IF sy-subrc EQ 0.
            CLEAR gvr_estorno.

            DELETE FROM zlest0205
              WHERE chave_nfe = gwa_saida_0102-chave_nfe.
* PBI - 55740 - Inicio
*            UPDATE zlest0205 SET dif_total = ''
*               WHERE ch_referencia EQ gwa_saida_0101-ch_referencia.
* PBI - 55740 - Fim
            COMMIT WORK.

            gvr_estorno = abap_true.
          ENDIF.
        ENDIF.

        IF  gvr_estorno EQ abap_true.
          MESSAGE 'Dados estornado com sucesso!' TYPE 'S'.

          PERFORM fm_busca_nf_complemento  USING gwa_saida_0102-ch_referencia.
          PERFORM fm_busca_dados.
* PBI - 55740 - Inicio
*          PERFORM fm_valida_nf_compl  USING gwa_saida_0102-ch_referencia CHANGING gvr_dif_total.
* PBI - 55740 - Inicio
          CALL METHOD grid_0102->refresh_table_display( is_stable = wa_stable ).
          CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).
        ENDIF.
      ENDIF.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'BTN_ESTORNO'.

      CALL METHOD grid_0103->get_selected_rows
        IMPORTING
          et_row_no = DATA(it_row_0103).

      IF it_row_0103[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
        EXIT.
      ELSE.

        READ TABLE  it_row_0103 INTO DATA(wa_row_0103) INDEX 1.
        IF sy-subrc EQ 0.

          READ TABLE git_saida_0103 INTO gwa_saida_0103 INDEX wa_row_0102-row_id.
          IF sy-subrc EQ 0.
            CLEAR gvr_estorno.

            DELETE FROM zlest0205
              WHERE chave_nfe = gwa_saida_0103-chave_nfe.
* PBI - 55740 - Inicio
*            UPDATE zlest0205 SET dif_total = ''
*               WHERE ch_referencia EQ gwa_saida_0101-ch_referencia.
* PBI - 55740 - Fim
            COMMIT WORK.

            gvr_estorno = abap_true.
          ENDIF.
        ENDIF.

        IF  gvr_estorno EQ abap_true.
          MESSAGE 'Dados estornado com sucesso!' TYPE 'S'.

          PERFORM fm_busca_nf_complemento  USING gwa_saida_0102-ch_referencia.
          PERFORM fm_busca_dados.
          PERFORM fm_valida_nf_compl  USING gwa_saida_0102-ch_referencia CHANGING gvr_dif_total.

          CALL METHOD grid_0103->refresh_table_display( is_stable = wa_stable ).
          CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).
        ENDIF.
      ENDIF.

  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  PERFORM fm_alv_0101.

  IF g_custom_container_0101 IS INITIAL.

    CREATE OBJECT g_custom_container_0101
      EXPORTING
        container_name = 'CONTAINER_0101'.

    IF grid_0101 IS INITIAL AND g_custom_container_0101 IS NOT  INITIAL.

      CREATE OBJECT grid_0101
        EXPORTING
          i_parent = g_custom_container_0101.
    ENDIF.


    PERFORM fm_excluding_toolbar.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid_0101.

    CALL METHOD grid_0101->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function01
        is_layout            = VALUE #( col_opt = abap_true )
      CHANGING
        it_outtab            = git_saida_0101
        it_fieldcatalog      = it_fieldcat_0101.

    CALL METHOD grid_0101->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD grid_0101->refresh_table_display( is_stable = wa_stable ).

    CALL METHOD grid_0101->set_frontend_fieldcatalog( it_fieldcatalog = it_fieldcat_0101 ).

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.

  PERFORM fm_alv_0102.

  IF g_custom_container_0102 IS INITIAL.

    CREATE OBJECT g_custom_container_0102
      EXPORTING
        container_name = 'CONTAINER_0102'.

    IF grid_0102 IS INITIAL AND g_custom_container_0102 IS NOT  INITIAL.

      CREATE OBJECT grid_0102
        EXPORTING
          i_parent = g_custom_container_0102.
    ENDIF.


    PERFORM fm_excluding_toolbar.
    PERFORM fm_sort.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid_0102.

    CALL METHOD grid_0102->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function01
        is_layout            = VALUE #( col_opt = abap_true )
      CHANGING
        it_outtab            = git_saida_0102
        it_fieldcatalog      = it_fieldcat_0102
        it_sort              = it_sort.


    SET HANDLER: lcl_handler=>on_data_changed FOR grid_0102,
                 lcl_handler=>on_data_changed_finished FOR grid_0102.

    CALL METHOD grid_0102->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid_0102->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid_0102->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD grid_0102->refresh_table_display( is_stable = wa_stable ).

    CALL METHOD grid_0102->set_frontend_fieldcatalog( it_fieldcatalog = it_fieldcat_0102 ).

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0103 OUTPUT.

  PERFORM fm_alv_0103.

  IF g_custom_container_0103 IS INITIAL.

    CREATE OBJECT g_custom_container_0103
      EXPORTING
        container_name = 'CONTAINER_0103'.

    IF grid_0103 IS INITIAL AND g_custom_container_0103 IS NOT  INITIAL.

      CREATE OBJECT grid_0103
        EXPORTING
          i_parent = g_custom_container_0103.
    ENDIF.

    PERFORM fm_excluding_toolbar.
    PERFORM fm_sort.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid_0103.

    CALL METHOD grid_0103->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function01
        is_layout            = VALUE #( col_opt = abap_true )
      CHANGING
        it_outtab            = git_saida_0103
        it_fieldcatalog      = it_fieldcat_0103
        it_sort              = it_sort.

    SET HANDLER: lcl_handler=>on_hotspot_click FOR grid_0103.

    SET HANDLER: lcl_handler=>on_data_changed FOR grid_0103,
                 lcl_handler=>on_data_changed_finished FOR grid_0103.

    CALL METHOD grid_0103->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid_0103->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid_0103->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD grid_0103->refresh_table_display( is_stable = wa_stable ).

    CALL METHOD grid_0103->set_frontend_fieldcatalog( it_fieldcatalog = it_fieldcat_0103 ).

  ENDIF.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  GET_CHAVE_NFE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_chave_nfe INPUT.

  CHECK gwa_nf_complemento-nf_elet_s IS NOT INITIAL.

  DATA(qt_a) = strlen( gvr_chave_nfe_texto ).
  CONDENSE gvr_chave_nfe_texto NO-GAPS.
  DATA(qt_b) =  strlen( gvr_chave_nfe_texto ).

  CHECK qt_a EQ qt_b .

  gwa_nf_complemento-chave_nfe =  gvr_chave_nfe_texto.

  SELECT SINGLE * FROM zib_nfe_dist_ter INTO gwa_zib_nfe_dist_ter
    WHERE chave_nfe EQ gwa_nf_complemento-chave_nfe.

  IF  sy-subrc EQ 0.
    gwa_nf_complemento-nr_nfe        =  gwa_zib_nfe_dist_ter-numero.
    gwa_nf_complemento-serie         =  gwa_zib_nfe_dist_ter-serie.
    gwa_nf_complemento-dt_emissao    =  gwa_zib_nfe_dist_ter-dt_emissao.

*-IR 197393-01.10.2024-#152481-JT-inicio
    IF gwa_zib_nfe_dist_ter-forne_cnpj IS NOT INITIAL.
      SELECT SINGLE * FROM lfa1 INTO @DATA(gwa_lfa1) WHERE stcd1 EQ @gwa_zib_nfe_dist_ter-forne_cnpj
                                                       AND stcd3 EQ @gwa_zib_nfe_dist_ter-forne_ie
                                                       AND loevm EQ @abap_off
                                                       AND sperr EQ @abap_off
                                                       AND sperm EQ @abap_off.

    ELSE.
      SELECT SINGLE * FROM lfa1 INTO      @gwa_lfa1  WHERE stcd2 EQ @gwa_zib_nfe_dist_ter-forne_cpf
                                                       AND stcd3 EQ @gwa_zib_nfe_dist_ter-forne_ie
                                                       AND loevm EQ @abap_off
                                                       AND sperr EQ @abap_off
                                                       AND sperm EQ @abap_off.
    ENDIF.
*-IR 197393-01.10.2024-#152481-JT-fim

    IF sy-subrc EQ 0.
      gwa_nf_complemento-fornecedor  =  gwa_lfa1-lifnr.
    ENDIF.

    SELECT SINGLE * FROM zib_nfe_dist_itm INTO  gwa_zib_nfe_dist_itm
       WHERE chave_nfe EQ gwa_nf_complemento-chave_nfe.

    IF sy-subrc EQ 0.
*-IR 197393-01.10.2024-#152481-JT-inicio
      TRANSLATE gwa_zib_nfe_dist_itm-prod_und_comerci TO UPPER CASE.  "*-IR 197393-01.10.2024-#153762-JT-inicio

      IF gwa_zib_nfe_dist_itm-prod_und_comerci = 'TO' OR
         gwa_zib_nfe_dist_itm-prod_und_comerci = 'TL' OR
         gwa_zib_nfe_dist_itm-prod_und_comerci = 'T'  OR
         gwa_zib_nfe_dist_itm-prod_und_comerci = 'TON'.
        gwa_zib_nfe_dist_itm-prod_qtd_comerci  = gwa_zib_nfe_dist_itm-prod_qtd_comerci * 1000.
      ENDIF.
*-IR 197393-01.10.2024-#152481-JT-fim

      gwa_nf_complemento-cfop          = gwa_zib_nfe_dist_itm-prod_cfop.
      gwa_nf_complemento-quantidade    = gwa_zib_nfe_dist_itm-prod_qtd_comerci.
      gwa_nf_complemento-valor         = gwa_zib_nfe_dist_itm-prod_vlr_total_b.
    ENDIF.

    READ TABLE git_zsdt0001 INTO DATA(gwa_zsdt0001) WITH KEY ch_referencia = gwa_cabecalho-ch_referencia.
    IF sy-subrc EQ 0.
      gwa_nf_complemento-material =  |{ gwa_zsdt0001-matnr ALPHA = OUT }|.
    ENDIF.

  ELSE.
    MESSAGE 'Chave informada não encontrada!' TYPE 'E'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FR_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_save.

  DATA: lva_erro(1) TYPE c,
        lva_sum     TYPE zib_nfe_dist_itm-prod_qtd_comerci.

  CLEAR: lva_sum.
  "Ajuste realizado IR161054 / 23/11/2023 - AOENNING / Alteração declaração type campo lva_sum.
*  lva_sum = REDUCE i( INIT x = 0 FOR wa IN git_saida_0102 NEXT x = x + wa-peso_compl ).
  LOOP AT git_saida_0102 ASSIGNING FIELD-SYMBOL(<ws_saida_0102>).
    IF <ws_saida_0102>-peso_compl IS NOT INITIAL.
      ADD <ws_saida_0102>-peso_compl TO lva_sum.
    ENDIF.

  ENDLOOP.
  "Ajuste realizado IR161054 / 23/11/2023 - AOENNING / Alteração declaração type campo lva_sum.

  IF  lva_sum > gwa_nf_complemento-quantidade .
    CLEAR: sy-ucomm.
    MESSAGE 'Não permitido salvar registro! Quantidade informada maior que a diferença!.' TYPE 'E'.
    lva_erro = 'X'.
    EXIT.
  ENDIF.

  IF  lva_sum < gwa_nf_complemento-quantidade .
    CLEAR: sy-ucomm.
    MESSAGE 'Não permitido salvar registro! Quantidade informada menor que a diferença!.' TYPE 'E'.
    lva_erro = 'X'.
    EXIT.
  ENDIF.

  LOOP AT git_saida_0102 INTO DATA(gwa_saida_0102_aux).

    IF gwa_nf_complemento-cfop NE gwa_saida_0102_aux-cfop.
      MESSAGE 'Não permitido salvar registro! CFOP informado diferente da Nota Original.' TYPE 'I'.
      lva_erro = 'X'.
      EXIT.
    ENDIF.

    IF gwa_nf_complemento-material NE  gwa_saida_0102_aux-matnr.
      MESSAGE 'Não permitido salvar registro! Material informado diferente da Nota Original.' TYPE 'I'.
      lva_erro = 'X'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_nf_complemento-fornecedor
      IMPORTING
        output = gwa_nf_complemento-fornecedor.


    IF gwa_nf_complemento-fornecedor NE gwa_saida_0102_aux-parid.
      MESSAGE 'Não permitido salvar registro! Fornecedor informado diferente da Nota Original.' TYPE 'I'.
      lva_erro = 'X'.
      EXIT.
    ENDIF.

  ENDLOOP.

  IF lva_erro <> 'X'.
    IF gwa_nf_complemento-nf_elet_s IS NOT INITIAL.
      gwa_zlest0205_save-nfe       = abap_true.
      gwa_zlest0205_save-chave_nfe = gwa_nf_complemento-chave_nfe.
    ELSE.

      SELECT SINGLE * FROM lfa1 INTO @DATA(gwa_lfa1)
        WHERE lifnr EQ @gwa_nf_complemento-fornecedor.

      IF sy-subrc EQ 0.
        gwa_zlest0205_save-nfe   = abap_false.
        gwa_nf_complemento-serie = |{ gwa_nf_complemento-serie ALPHA = IN }|.

        CONCATENATE
           'F'
           gwa_lfa1-regio
           gwa_nf_complemento-dt_emissao+2(2)
           gwa_nf_complemento-dt_emissao+4(2)
           '000'
           gwa_lfa1-stcd2
           '04'
           gwa_nf_complemento-serie
           gwa_nf_complemento-nr_nfe  INTO gwa_zlest0205_save-chave_nfe.
      ENDIF.
    ENDIF.

*** Salva Cabeçalho
    gwa_zlest0205_save-mandat         = sy-mandt.
    gwa_zlest0205_save-bukrs          = gwa_saida_aux-bukrs.
    gwa_zlest0205_save-branch         = gwa_saida_aux-branch.
    gwa_zlest0205_save-parid          = gwa_nf_complemento-fornecedor.

*&------------------------------------------------------------------------------------Convertendo quantidade caractere material para tamanho 18 / IR159023 / AOENNING.
    CLEAR: vg_matnr.
    vg_matnr = |{ gwa_nf_complemento-material ALPHA = OUT }|.
    vg_matnr = |{ vg_matnr ALPHA = IN }|.
    CLEAR: gwa_nf_complemento-material.
    gwa_nf_complemento-material = vg_matnr.
*&------------------------------------------------------------------------------------Convertendo quantidade caractere material para tamanho 18 / IR159023 / AOENNING.

    gwa_zlest0205_save-matnr          = gwa_nf_complemento-material.
    gwa_zlest0205_save-peso_fiscal    = gwa_nf_complemento-quantidade.
    gwa_zlest0205_save-nfnum          = gwa_nf_complemento-nr_nfe.
    gwa_zlest0205_save-series         = gwa_nf_complemento-serie.
    gwa_zlest0205_save-docdat         = gwa_nf_complemento-dt_emissao.
    gwa_zlest0205_save-netwr          = gwa_nf_complemento-valor.
    gwa_zlest0205_save-tp_transgenia  = gwa_saida_aux-tp_transgenia.
    gwa_zlest0205_save-peso_subtotal  = gwa_nf_complemento-quantidade.
    "gwa_zlest0205_save-ch_referencia  = gwa_saida_aux-ch_referencia. * PBI - 55740
    gwa_zlest0205_save-local_descarga = gwa_saida_aux-local_descarga.
    gwa_zlest0205_save-cfop           = gwa_nf_complemento-cfop.
    gwa_zlest0205_save-nr_safra       = gwa_saida_aux-nr_safra.

    MODIFY zlest0205 FROM gwa_zlest0205_save.
    COMMIT WORK.

*** Salva as notas complementadas

    LOOP AT git_saida_0102 INTO DATA(gwa_saida_0102).

      IF gwa_nf_complemento-chave_nfe IS INITIAL.
        gwa_zlest0205_ref-chave_nfe = gwa_zlest0205_save-chave_nfe.
      ELSE.
        gwa_zlest0205_ref-chave_nfe     = gwa_nf_complemento-chave_nfe.
      ENDIF.

      gwa_zlest0205_ref-ch_referencia = gwa_saida_0102-ch_referencia.
      gwa_zlest0205_ref-peso_compl    = gwa_saida_0102-peso_compl.

      MODIFY zlest0205_ref FROM gwa_zlest0205_ref.

    ENDLOOP.

    MESSAGE 'Dados gravado com sucesso!' TYPE 'S'.

    CLEAR: gwa_nf_complemento, gvr_chave_nfe_texto .
    PERFORM fm_busca_dados.
    CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).
    LEAVE TO SCREEN 0.
  ENDIF.
* PBI - 55740 - Inicio
*  CLEAR: gvr_peso_fiscal, gvr_total_peso_fiscal, gwa_zlest0205_save.
*
*  IF gwa_cabecalho-diferenca EQ 0.
*    MESSAGE 'Ação Não permitida! Todas as NF de complementros ja foram lançadas!' TYPE 'E'.
*    EXIT.
*  ENDIF.
*
*  IF gwa_nf_complemento-fornecedor NE gwa_cabecalho-parid.
*    MESSAGE 'Não permitido salvar registro! Fornecedor informado diferente da Nota Original.' TYPE 'S'.
*    EXIT.
*  ENDIF.
*
*  IF gwa_nf_complemento-material NE  gwa_saida_aux-matnr.
*    MESSAGE 'Não permitido salvar registro! Material informado diferente da Nota Original.' TYPE 'S'.
*    EXIT.
*  ENDIF.
*
*  IF gwa_nf_complemento-cfop NE gwa_saida_aux-cfop.
*    MESSAGE 'Não permitido salvar registro! CFOP informado diferente da Nota Original.' TYPE 'S'.
*    EXIT.
*  ENDIF.
*
*  IF gwa_nf_complemento-quantidade > gwa_cabecalho-diferenca.
*    MESSAGE 'Não permitido salvar registro! Quantidade informada maior que a diferença!.' TYPE 'S'.
*    EXIT.
*  ENDIF.
*
*
*  IF gwa_nf_complemento-nf_elet_s IS NOT INITIAL.
*    gwa_zlest0205_save-nfe       = abap_true.
*    gwa_zlest0205_save-chave_nfe = gwa_nf_complemento-chave_nfe.
*  ELSE.
*
*    SELECT SINGLE * FROM lfa1 INTO @DATA(gwa_lfa1)
*      WHERE lifnr EQ @gwa_nf_complemento-fornecedor.
*
*    IF sy-subrc EQ 0.
*      gwa_zlest0205_save-nfe   = abap_false.
*      gwa_nf_complemento-serie = |{ gwa_nf_complemento-serie ALPHA = IN }|.
*
*      CONCATENATE
*         'F'
*         gwa_lfa1-regio
*         sy-datum+2(2)
*         sy-datum+4(2)
*         '000'
*         gwa_lfa1-stcd2
*         '04'
*         gwa_nf_complemento-serie
*         gwa_nf_complemento-nr_nfe  INTO gwa_zlest0205_save-chave_nfe.
*    ENDIF.
*  ENDIF.
*
*  gwa_zlest0205_save-mandat         = sy-mandt.
*  gwa_zlest0205_save-bukrs          = gwa_saida_aux-bukrs.
*  gwa_zlest0205_save-branch         = gwa_saida_aux-branch.
*  gwa_zlest0205_save-parid          = gwa_nf_complemento-fornecedor.
*  gwa_zlest0205_save-matnr          = |{ gwa_nf_complemento-material ALPHA = IN }|.
*  gwa_zlest0205_save-peso_fiscal    = gwa_nf_complemento-quantidade.
*  gwa_zlest0205_save-nfnum          = gwa_nf_complemento-nr_nfe.
*  gwa_zlest0205_save-series         = gwa_nf_complemento-serie.
*  gwa_zlest0205_save-docdat         = gwa_nf_complemento-dt_emissao.
*  gwa_zlest0205_save-netwr          = gwa_nf_complemento-valor.
*  gwa_zlest0205_save-tp_transgenia  = gwa_saida_aux-tp_transgenia.
*  gwa_zlest0205_save-peso_subtotal  = gwa_nf_complemento-quantidade.
*  gwa_zlest0205_save-ch_referencia  = gwa_saida_aux-ch_referencia.
*  gwa_zlest0205_save-local_descarga = gwa_saida_aux-local_descarga.
*  gwa_zlest0205_save-cfop           = gwa_nf_complemento-cfop.
*  gwa_zlest0205_save-nr_safra       = gwa_saida_aux-nr_safra.
*
*
*  "CS2020001103 - INICIO - AL
*  gwa_zlest0205_ref-chave_nfe     = gwa_zlest0205_save-chave_nfe.
*  gwa_zlest0205_ref-ch_referencia = gwa_zlest0205_save-ch_referencia.
*  gwa_zlest0205_ref-peso_compl    = gvr_total_diferenca .
*  gwa_zlest0205_ref-dif_total     = gwa_zlest0205_save-dif_total.
*  MODIFY zlest0205_ref FROM gwa_zlest0205_ref.
*  "CS2020001103 - FIM    - AL
*
*  MODIFY zlest0205 FROM gwa_zlest0205_save.
*  COMMIT WORK.
*
*  MESSAGE 'Dados gravado com sucesso!' TYPE 'S'.
*
*  PERFORM fm_valida_nf_compl USING gwa_saida_aux-ch_referencia CHANGING gvr_dif_total.
*
*
*  CLEAR: gwa_nf_complemento, gvr_chave_nfe_texto .
*
*
*  PERFORM fm_busca_nf_complemento USING gwa_saida_aux-ch_referencia.
*  CALL METHOD grid_0101->refresh_table_display( is_stable = wa_stable ).
*
*  PERFORM fm_busca_dados.
*  CALL METHOD grid->refresh_table_display( is_stable = wa_stable ).
* PBI - 55740 - Fim
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FR_BUSCA_NF_COMPLEMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_busca_nf_complemento USING p_ch_referencia TYPE zsdt0001-ch_referencia.

  REFRESH: git_zlest0205_alv, git_saida_0103.

  SELECT * FROM zlest0205_ref INTO TABLE git_zlest0205_alv
   WHERE ch_referencia EQ p_ch_referencia.

  LOOP AT git_zlest0205_alv INTO DATA(gwa_zlest0205_alv).
    CLEAR gwa_saida_0103.

    gwa_saida_0103-chave_nfe  =  gwa_zlest0205_alv-chave_nfe.
    gwa_saida_0103-peso_compl =  gwa_zlest0205_alv-peso_compl.
    gwa_saida_0103-icon_est   =  icon_storno.

    APPEND gwa_saida_0103 TO git_saida_0103.
  ENDLOOP.


* PBI - 55740 - Inicio
*  REFRESH: git_zlest0205_alv, git_saida_0101.
*
*  SELECT * FROM zlest0205 INTO TABLE git_zlest0205_alv
*   WHERE ch_referencia EQ p_ch_referencia.

*  LOOP AT git_zlest0205_alv INTO DATA(gwa_zlest0205_alv).
*    CLEAR gwa_saida_0101.
*    gwa_saida_0101-nfnum          =  gwa_zlest0205_alv-nfnum.
*    gwa_saida_0101-peso_fiscal    =  gwa_zlest0205_alv-peso_fiscal.
*    gwa_saida_0101-chave_nfe      =  gwa_zlest0205_alv-chave_nfe.
*    gwa_saida_0101-ch_referencia  =  gwa_zlest0205_alv-ch_referencia.
*
*    APPEND gwa_saida_0101 TO git_saida_0101.
*    ENDLOOP.
* PBI - 55740 - Fim

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FR_ESTORNA_NF_COMPLEMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_estorna_nf_complemento USING p_chave_nfe TYPE zsdt0001-chave_nfe.

  REFRESH: git_zlest0205, git_zlest0205_alv, git_saida_0103.
  CLEAR: gvr_aqua, gvr_estorno, git_zlest0205_est.


  SELECT * FROM zlest0205_ref INTO TABLE git_zlest0205_est
   WHERE chave_nfe EQ p_chave_nfe.

*  SELECT * FROM zlest0205_ref INTO TABLE git_zlest0205_est
*    FOR ALL ENTRIES IN git_zlest0205_alv
*    WHERE ch_referencia EQ git_zlest0205_alv-ch_referencia.
*
*  DELETE ADJACENT DUPLICATES FROM git_zlest0205_est COMPARING chave_nfe.

  SELECT * FROM zlest0205 INTO TABLE git_zlest0205
    FOR ALL ENTRIES IN git_zlest0205_est
    WHERE chave_nfe EQ git_zlest0205_est-chave_nfe.

  LOOP AT  git_zlest0205 INTO DATA(gwa_zlest0205) .
    IF gwa_zlest0205-ct_aquav IS NOT INITIAL.
      MESSAGE 'Ação não permitida. Ct-e Aquaviario está gerado!' TYPE 'I'.
      gvr_aqua = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF gvr_aqua IS INITIAL.

    LOOP AT git_zlest0205_est INTO DATA(gwa_zlest0205_est).
      DELETE FROM zlest0205
        WHERE chave_nfe = gwa_zlest0205_est-chave_nfe.

      DELETE FROM zlest0205_ref
         WHERE chave_nfe = gwa_zlest0205_est-chave_nfe.
    ENDLOOP.

    gvr_estorno = abap_true.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_OUTRA_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM FM_OUTRA_NF .
*
*  PERFORM FM_VALIDA_NF_COMPL USING GWA_SAIDA_AUX-CH_REFERENCIA  CHANGING GVR_DIF_TOTAL.
*
*  IF GVR_DIF_TOTAL  EQ ABAP_TRUE.
*    MESSAGE 'Ação Não permitida! NF lançadas já preencheu a diferença!' TYPE 'E'.
*    EXIT.
*  ENDIF.
*ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  SET_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_screen OUTPUT.
  LOOP AT SCREEN.
    IF gwa_nf_complemento-nf_elet_n EQ abap_true.
      IF ( screen-group1 = 'T1' ).
        screen-invisible = 0.
        screen-input     = 0.
        screen-active    = 1.
        MODIFY SCREEN.
      ENDIF.

      IF ( screen-group1 = 'T2' ).
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
      ENDIF.

      CLEAR gvr_chave_nfe_texto.

    ELSE.

      IF ( screen-group1 = 'T1' ).
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
      ENDIF.
      IF ( screen-group1 = 'T2' ).
        screen-invisible = 0.
        screen-input     = 0.
        screen-active    = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_ESTORNO_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_estorno_nf .
* PBI - 55740 - Inicio
*  CALL METHOD grid->get_selected_rows
*    IMPORTING
*      et_row_no = DATA(it_row).
*
*  IF it_row[] IS INITIAL.
*    MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
*    EXIT.
*  ELSE.
*
*    LOOP AT it_row INTO DATA(wa_row).
*      READ TABLE git_saida_s_pend INTO gwa_saida_s_pend INDEX wa_row-row_id.
*      IF sy-subrc EQ 0.
*        IF gwa_saida_s_pend-ct_aquav IS NOT INITIAL.
*          MESSAGE 'Ação não permitida. Ct Aquaviario está gerado!' TYPE 'S'.
*          EXIT.
*        ELSE.
*          CLEAR gvr_estorno.
*
*          DELETE FROM zlest0205
*            WHERE chave_nfe = gwa_saida_s_pend-chave_nfe.
*
*          UPDATE zlest0205 SET dif_total = ''
*             WHERE ch_referencia EQ gwa_saida_s_pend-ch_referencia.
*
*          COMMIT WORK.
*
*          gvr_estorno = abap_true.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    IF gvr_estorno EQ abap_true.
*      MESSAGE 'NF estornada com sucesso!' TYPE 'S'.
*      PERFORM fm_busca_dados.
*    ENDIF.
*  ENDIF.
* PBI - 55740 - Fim
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_ESTORNO_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_vinc_compl .

  CLEAR: git_saida_0102.

  CALL METHOD grid->get_selected_rows
    IMPORTING
      et_row_no = DATA(it_row).

  IF it_row[] IS INITIAL.
    MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
    EXIT.
  ELSE.

    LOOP AT it_row INTO DATA(wa_row).
      READ TABLE git_saida INTO gwa_saida INDEX wa_row-row_id.
      IF sy-subrc EQ 0.

        MOVE gwa_saida TO gwa_saida_aux.

        gwa_saida_aux-matnr = |{ gwa_saida_aux-matnr ALPHA = OUT }|.
        gwa_saida_aux-parid = |{ gwa_saida_aux-parid ALPHA = OUT }|.

        READ TABLE git_t001 INTO DATA(gwa_t001) WITH KEY bukrs = gwa_saida-bukrs.
        IF sy-subrc EQ 0.
          gwa_cabecalho-bukrs =  gwa_saida-bukrs.
          CONCATENATE  gwa_saida-bukrs '-' gwa_t001-butxt INTO gwa_cabecalho-bukrstxt.
        ENDIF.

        READ TABLE git_t001w INTO DATA(gwa_t001w) WITH KEY werks = gwa_saida-branch.
        IF sy-subrc EQ 0.
          gwa_cabecalho-branch =  gwa_saida-branch.
          CONCATENATE  gwa_saida-branch '-' gwa_t001w-name1 INTO gwa_cabecalho-branchtxt.
        ENDIF.

        READ TABLE git_lfa1 INTO DATA(gwa_lfa1) WITH KEY lifnr = gwa_saida-parid.
        IF sy-subrc EQ 0.
          gwa_cabecalho-parid =  gwa_saida-parid.
          CONCATENATE  gwa_saida-parid '-' gwa_lfa1-name1 INTO gwa_cabecalho-paridtxt.
        ENDIF.

        gwa_cabecalho-nfnum         = gwa_saida-nfnum.
        gwa_cabecalho-serie         = gwa_saida-series.
        gwa_cabecalho-diferenca     = gwa_saida-diferenca.
        gwa_cabecalho-ch_referencia = gwa_saida-ch_referencia.


        gwa_saida_0102-chave_nfe       =  gwa_saida-chave_nfe.
        gwa_saida_0102-ch_referencia   =  gwa_saida-ch_referencia.
        gwa_saida_0102-nr_romaneio     =  gwa_saida-nr_romaneio.
        gwa_saida_0102-dt_emissao      =  gwa_saida-dt_emissao.
        gwa_saida_0102-nfnum           =  gwa_saida-nfnum.
        gwa_saida_0102-peso_fiscal     =  gwa_saida-peso_fiscal.
        gwa_saida_0102-peso_subtotal   =  gwa_saida-peso_subtotal.
        gwa_saida_0102-diferenca       =  gwa_saida-diferenca.

        IF gwa_saida_0102-peso_compl IS INITIAL.
          gwa_saida_0102-peso_compl      =  gwa_saida-saldo_compl .
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = gwa_saida-matnr
          IMPORTING
            output = gwa_saida_0102-matnr.

        gwa_saida_0102-parid           =  gwa_saida-parid.
        gwa_saida_0102-cfop           =  gwa_saida-cfop.

        APPEND gwa_saida_0102 TO git_saida_0102.

        CLEAR: gwa_saida_0102.

      ENDIF.
    ENDLOOP.

    CALL SCREEN 0102.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_VALIDA_NF_COMPL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_valida_nf_compl  USING  u_ch_referencia TYPE zlest0205_ref-ch_referencia  CHANGING r_dif_total TYPE c.
* PBI - 55740 - Inicio
*  REFRESH: git_zlest0205_aux,  git_zlest0205_nf.
*  CLEAR: gwa_zlest0205_aux,
*         gvr_peso_fiscal,
*         gwa_saida_c_pend.
*
*  READ TABLE git_saida_c_pend INTO gwa_saida_c_pend  WITH KEY ch_referencia = u_ch_referencia.
*
*  SELECT * FROM zlest0205 INTO TABLE  git_zlest0205_nf
*  WHERE ch_referencia EQ u_ch_referencia.
*
*  IF sy-subrc EQ 0.
*
*    LOOP AT git_zlest0205_nf INTO DATA(gwa_zlest0205_nf).
*
*      gvr_peso_fiscal = ( gvr_peso_fiscal + gwa_zlest0205_nf-peso_fiscal ).
*
*      MOVE gwa_zlest0205_nf TO gwa_zlest0205_aux.
*      gwa_zlest0205_aux-dif_total = 'X'.
*      APPEND gwa_zlest0205_aux TO git_zlest0205_aux.
*
*      CLEAR gwa_zlest0205_aux.
*    ENDLOOP.
*
*    gwa_cabecalho-diferenca =  ( gwa_saida_c_pend-diferenca  - gvr_peso_fiscal  ).
*
*    IF gvr_peso_fiscal EQ gwa_saida_c_pend-diferenca.
*      r_dif_total = abap_true.
*
*      MODIFY zlest0205 FROM TABLE git_zlest0205_aux.
*      COMMIT WORK.
*    ENDIF.
*
*  ELSE.
*    gwa_cabecalho-diferenca  =  gwa_saida_c_pend-diferenca.
*  ENDIF.
* PBI - 55740 - Fim
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_EXCLUDING_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_excluding_toolbar .
  wl_function01  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_check.
  APPEND wl_function01 TO tl_function01.
  wl_function01  = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_call_chain.
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_call_crbatch .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_call_crweb .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_call_lineitems .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_call_master_data .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_call_more .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_call_report .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_call_xint .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_call_xxl .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_check .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_col_invisible .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_col_optimize .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_count .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_current_variant .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_data_save .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_delete_filter .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_deselect_all .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_detail .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_expcrdesig .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_expcrtempl .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_expmdb .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_extend .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_f4 .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_filter .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_find .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_fix_columns .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_graph .
  APPEND wl_function01 TO tl_function01.
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_loc_undo .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_maintain_variant .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_maximum .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_minimum .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_pc_file .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_print .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_print_prev .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_refresh .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_reprep .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_save_variant .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_select_all .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_send .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_sort .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_sort_asc .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_sort_dsc .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_to_office .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_to_rep_tree .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_unfix_columns .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_variant_admin .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_views .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fc_view_crystal .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_mb_paste .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_mb_variant .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_mb_view .
  APPEND wl_function01 TO tl_function01.
  wl_function01 = cl_gui_alv_grid=>mc_fg_sort .
  APPEND wl_function01 TO tl_function01.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_sort .
  CLEAR wa_sort.

  wa_sort-fieldname = 'CHAVE_NFE'.
  wa_sort-spos = 1.
  wa_sort-up = 'X'.

  APPEND wa_sort TO it_sort.
ENDFORM.
