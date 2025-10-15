*&---------------------------------------------------------------------*
*& Report  ZWRR0010
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zwrr0010.

TABLES: zfiwrt0021, zfiwrt0022.

TYPES: BEGIN OF ty_saida.
         INCLUDE STRUCTURE zfiwrt2004.
*         cen_emissor   TYPE zfiwrt2004-cen_emissor,
*         cen_receptor  TYPE zfiwrt2004-cen_receptor,
*         bukrs         TYPE zfiwrt2004-bukrs,
*         mes           TYPE zfiwrt2004-mes,
*         ano           TYPE zfiwrt2004-ano,
*         icms          TYPE zfiwrt2004-icms,
*         deb_cred      TYPE zfiwrt2004-deb_cred,
*         operacao      TYPE zfiwrt2004-operacao,
*         matnr         TYPE zfiwrt2004-matnr,
*         seq_lanc_s    TYPE zfiwrt2004-seq_lanc_s,
*         seq_lanc_e    TYPE zfiwrt2004-seq_lanc_e,
*         docnum_s      TYPE zfiwrt2004-docnum_s,
*         docnum_e      TYPE zfiwrt2004-docnum_e,
*         n_nfe_s       TYPE zfiwrt2004-n_nfe_s,
*         msg_ret_sefaz TYPE zfiwrt2004-msg_ret_sefaz,
TYPES:   deletar(1)  TYPE c,
         field_style TYPE lvc_t_styl,
         colorcell   TYPE lvc_t_scol.
TYPES:   END OF ty_saida.

DATA: BEGIN OF tg_itens OCCURS 0,
        mark(1),
        itmnum  TYPE zfiwrt0009-itmnum,
        matnr   TYPE zfiwrt0009-matnr,
        maktx   TYPE makt-maktx,
        cfop    TYPE zfiwrt0006-cfop,
        charg   TYPE zfiwrt0009-charg,
        werks   TYPE t001w-werks,
        lgort   TYPE zfiwrt0009-lgort,
        menge   TYPE zfiwrt0009-menge,
        meins   TYPE zfiwrt0009-meins,
        netpr   TYPE zfiwrt0009-netpr,
        netwr   TYPE zfiwrt0009-netwr,
        anln1   TYPE zfiwrt0009-anln1,
        anln2   TYPE zfiwrt0009-anln2,
        steuc   TYPE marc-steuc,
      END OF tg_itens.

DATA: BEGIN OF tg_contab OCCURS 0,
        bschl   TYPE zfiwrt0011-bschl,
        hkont   TYPE zfiwrt0011-hkont,
        txt50   TYPE skat-txt50,
        dmbtr   TYPE zfiwrt0011-dmbtr,
        taxtyp  TYPE zfiwrt0011-taxtyp,
        estorno TYPE zfiwrt0011-estorno,
        newbw   TYPE zfiwrt0011-newbw,
        zlsch   TYPE zfiwrt0011-zlsch,
        zfbdt   TYPE zfiwrt0011-zfbdt,
        kostl   TYPE zfiwrt0011-kostl,
        umskz   TYPE zfiwrt0011-umskz,
        vbund   TYPE zfiwrt0011-vbund,
        style2  TYPE lvc_t_styl,
      END OF tg_contab.

DATA: BEGIN OF tg_impo OCCURS 0,
        taxtyp   TYPE zfiwrt0010-taxtyp,
        ttypetxt TYPE j_1bttytxt,
        taxgrp   TYPE j_1btaxgrp,
        base     TYPE zfiwrt0010-base,
        rate     TYPE zfiwrt0010-rate,
        taxval   TYPE zfiwrt0010-taxval,
        excbas   TYPE zfiwrt0010-excbas,
        othbas   TYPE zfiwrt0010-othbas,
      END OF  tg_impo.

DATA:BEGIN OF tg_impo_comp OCCURS 0,
       itmnum TYPE zfiwrt0010-itmnum.
       INCLUDE STRUCTURE tg_impo.
DATA: END OF tg_impo_comp.


DATA: BEGIN OF tg_mensagens OCCURS 0,
        seqnum  TYPE  zfiwrt0005-seqnum,
        linnum  TYPE  zfiwrt0005-linnum,
        message TYPE  zfiwrt0005-message,
      END OF  tg_mensagens.

TYPES: BEGIN OF ty_parc,
         parvw    TYPE zfiwrt0015-parvw,
         parid    TYPE zfiwrt0015-parid,
         nome(80),
         style    TYPE lvc_t_styl,
       END OF ty_parc.

DATA: BEGIN OF tg_movest OCCURS 0,
        bwart   TYPE zfiwrt0004-bwart,
        tcode   TYPE zfiwrt0004-tcode,
        mwskz1  TYPE zfiwrt0004-mwskz1,
        estorno TYPE zfiwrt0004-estorno,
      END OF tg_movest.

DATA: it_saida      TYPE TABLE OF ty_saida,
      wa_saida      TYPE ty_saida,
      it_saida_del  TYPE TABLE OF ty_saida,
      it_saida_aux  TYPE TABLE OF ty_saida,
      wa_saida_aux  TYPE ty_saida,
      wa_saida_del  TYPE ty_saida,
      wa_zfiwrt2004 TYPE zfiwrt2004,
      sl_saida      LIKE LINE OF it_saida.

DATA: it_zfiwrt0001       TYPE TABLE OF zfiwrt0001,
      wa_zfiwrt0001       TYPE zfiwrt0001,
      it_zfiwrt0002       TYPE TABLE OF zfiwrt0002,
      wa_zfiwrt0002       TYPE zfiwrt0002,
      it_zfiwrt0003       TYPE TABLE OF zfiwrt0003,
      wa_zfiwrt0003       TYPE zfiwrt0003,
      it_zfiwrt0004       TYPE TABLE OF zfiwrt0004,
      wa_zfiwrt0004       TYPE zfiwrt0004,
      it_zfiwrt0005       TYPE TABLE OF zfiwrt0005,
      wa_zfiwrt0005       TYPE zfiwrt0005,
      it_zfiwrt0006       TYPE TABLE OF zfiwrt0006,
      wa_zfiwrt0006       TYPE zfiwrt0006,
      it_zfiwrt0007       TYPE TABLE OF zfiwrt0007,
      wa_zfiwrt0007       TYPE zfiwrt0007,
      it_zfiwrt0008       TYPE TABLE OF zfiwrt0008,
      wa_zfiwrt0008       TYPE zfiwrt0008,
      it_zfiwrt8          TYPE TABLE OF zfiwrt0008,
      wa_zfiwrt8          TYPE zfiwrt0008,
      it_zfiwrt0009       TYPE TABLE OF zfiwrt0009,
      wa_zfiwrt0009       TYPE zfiwrt0009,
      it_zfiwrt0010       TYPE TABLE OF zfiwrt0010,
      wa_zfiwrt0010       TYPE zfiwrt0010,
      it_zfiwrt0011       TYPE TABLE OF zfiwrt0011,
      wa_zfiwrt0011       TYPE zfiwrt0011,
      it_zfiwrt0012       TYPE TABLE OF zfiwrt0012,
      wa_zfiwrt0012       TYPE  zfiwrt0012,
      it_zfiwrt0013       TYPE TABLE OF zfiwrt0013,
      wa_zfiwrt0013       TYPE  zfiwrt0013,
      it_zfiwrt0015       TYPE TABLE OF zfiwrt0015,
      wa_zfiwrt0015       TYPE  zfiwrt0015,
      it_zfiwrt0021       TYPE TABLE OF zfiwrt0021,
      wa_zfiwrt0021       TYPE zfiwrt0021,
      it_zfiwrt0022       TYPE TABLE OF zfiwrt0022,
      wa_zfiwrt0022       TYPE zfiwrt0022,
      it_zfiwrt22         TYPE TABLE OF zfiwrt0022,
      wa_zfiwrt22         TYPE zfiwrt0022,
      it_zfiwrt1000       TYPE TABLE OF  zfiwrt1000,
      wa_zfiwrt1000       TYPE zfiwrt1000,
      tl_1000             TYPE TABLE OF zfiwrt1000,
      wl_1000             TYPE zfiwrt1000,

      tl_0008             TYPE TABLE OF zfiwrt0008 WITH HEADER LINE,
      tl_zib_chv          TYPE TABLE OF zib_contabil_chv WITH HEADER LINE,
      tl_zib_err          TYPE TABLE OF zib_contabil_err WITH HEADER LINE,
      tl_zib_cont         TYPE TABLE OF zib_contabil WITH HEADER LINE,
      tl_0011             TYPE TABLE OF zfiwrt0011 WITH HEADER LINE,

      it_j_1baj           TYPE TABLE OF j_1baj,
      wa_j_1baj           TYPE j_1baj,
      it_j_1bajt          TYPE TABLE OF j_1bajt,
      wa_j_1bajt          TYPE j_1bajt,
      it_tbsl             TYPE TABLE OF tbsl,
      wa_tbsl             TYPE tbsl,
      it_skat             TYPE TABLE OF skat,
      wa_skat             TYPE skat,
      wa_cskb             TYPE cskb,
      it_mara             TYPE TABLE OF mara,
      wa_mara             TYPE mara,
      it_marc             TYPE TABLE OF marc,
      wa_marc             TYPE marc,
      it_t001             TYPE TABLE OF t001,
      wa_t001             TYPE t001,
      it_t001w            TYPE TABLE OF t001w,
      wa_t001w            TYPE  t001w,
      it_kna1             TYPE TABLE OF kna1,
      wa_kna1             TYPE kna1,
      wa_lfa1             TYPE lfa1,
      it_j_1baa           TYPE TABLE OF j_1baa,
      wa_j_1baa           TYPE j_1baa,
      wl_texto_fiscal(30),
      wl_indcoper         TYPE zfiwrt0006-indcoper,
      wg_shipfrom         TYPE lfa1-regio,
      wg_shipto           TYPE lfa1-regio,
      tl_impo_aux         LIKE TABLE OF tg_impo WITH HEADER LINE,
      tg_parc             TYPE TABLE OF ty_parc  WITH HEADER LINE.

DATA: tl_docest   TYPE TABLE OF zfiwrs0003 WITH HEADER LINE.

DATA: it_selected_rows     TYPE lvc_t_row,
      t_row_no             TYPE lvc_t_roid,
      wa_selected_rows     TYPE lvc_s_row,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      dg_splitter_1        TYPE REF TO cl_gui_splitter_container,
      dg_splitter_2        TYPE REF TO cl_gui_splitter_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      tl_function          TYPE ui_functions,
      wl_function          LIKE tl_function WITH HEADER LINE,
      container_1          TYPE REF TO cl_gui_container,
      container_2          TYPE REF TO cl_gui_container,
      tg_selectedrow       TYPE lvc_t_row,
      wg_selectedrow       TYPE lvc_s_row,
      pt_exclude           TYPE ui_functions,
      it_fcat              TYPE TABLE OF lvc_s_fcat,
      wl_fcat              TYPE lvc_s_fcat,
      ls_edit              TYPE lvc_s_styl,
      lt_edit              TYPE lvc_t_styl,
      msg_id               LIKE t100-arbgb,
      msg_no               LIKE t100-msgnr,
      msg_var1             LIKE balm-msgv1,
      msg_var2             LIKE balm-msgv2,
      msg_var3             LIKE balm-msgv3,
      msg_var4             LIKE balm-msgv4,
      wmessage             TYPE bapi_msg,
      l_status             TYPE char0001,
      l_messa              TYPE char0064.

DATA: t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      t_colorcell    TYPE TABLE OF lvc_s_scol,
      w_colorcell    TYPE lvc_s_scol,
      t_exctab       TYPE slis_t_extab,
      w_exctab       TYPE slis_extab,
      wa_layout      TYPE lvc_s_layo,
      wa_stable      TYPE lvc_s_stbl,
      "estrutura      TYPE TABLE OF ty_estrutura,
      "wa_estrutura   TYPE ty_estrutura,
      wg_indic_cont  TYPE sy-tabix,
      l_tabix        TYPE sy-tabix,
      l_words        TYPE spell,
      l_lastday      TYPE sy-datum,
      l_datum        TYPE sy-datum,
      l_parid        TYPE numc10,
      l_mes          TYPE numc2,
      l_mensagem     TYPE char255,
      l_data_c       TYPE char20,
      l_char1(72)    TYPE c,
      l_char2(72)    TYPE c,
      l_char3(72)    TYPE c,
      l_t247         TYPE t247,
      l_ano          TYPE numc4,
      l_valor_c      TYPE char50.

*DATA: wg_shipfrom TYPE lfa1-regio,
*      wg_shipto   TYPE lfa1-regio.

DATA: p_seq_lcto TYPE zfiwrt0008-seq_lcto.


DATA: git_filtro TYPE zif_screen_linha_filtro_t.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs FOR zfiwrt0021-bukrs NO INTERVALS OBLIGATORY.
  PARAMETERS: p_mes TYPE zfiwrt0022-mes   OBLIGATORY,
              p_ano TYPE zfiwrt0022-ano   OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_proc_s RADIOBUTTON GROUP g1 DEFAULT 'X',
              p_proc_n RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.

START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotsopt_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

    DATA: ls_good      TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          lv_value1    TYPE lvc_value,
          lv_value2    TYPE lvc_value,
          lv_value3    TYPE lvc_value,
          lv_value4    TYPE lvc_value,
          v_ope_cred_s TYPE zfiwrt2003-ope_saida_credor,
          v_ope_dev_s  TYPE zfiwrt2003-ope_saida_devedor,
          v_mat_cred_s TYPE zfiwrt2003-mat_saida_credor,
          v_mat_dev_s  TYPE zfiwrt2003-mat_saida_devedor,
          v_ope_cred_e TYPE zfiwrt2003-ope_entrada_credor,
          v_ope_dev_e  TYPE zfiwrt2003-ope_entrada_devedor,
          v_mat_cred_e TYPE zfiwrt2003-mat_entrada_credor,
          v_mat_dev_e  TYPE zfiwrt2003-mat_entrada_devedor.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      lv_value = ls_good-value.

      CLEAR: lv_value1, lv_value2.

      READ TABLE it_saida INTO wa_saida INDEX ls_good-row_id.

      CASE ls_good-fieldname.
        WHEN 'CEN_EMISSOR'.
          SELECT SINGLE cen_receptor
                   FROM zfiwrt2003
                   INTO @DATA(v_centro_receptor)
                  WHERE cen_emissor = @lv_value.

          IF sy-subrc = 0.
            lv_value = v_centro_receptor.
            CONDENSE lv_value NO-GAPS.

            wa_saida-cen_receptor = lv_value.
            MODIFY it_saida FROM wa_saida INDEX ls_good-row_id.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'CEN_RECEPTOR'
                i_value     = lv_value.
          ELSE.
            CLEAR: wa_saida-cen_receptor,
                   wa_saida-operacao_s,
                   wa_saida-operacao_e,
                   wa_saida-matnr_s,
                   wa_saida-matnr_e.
            MODIFY it_saida FROM wa_saida INDEX ls_good-row_id.
            MESSAGE s024(sd) WITH 'Parametros não localizados!' DISPLAY LIKE 'E'.
          ENDIF.

          SELECT SINGLE ope_saida_credor   ope_saida_devedor   mat_saida_credor   mat_saida_devedor
                        ope_entrada_credor ope_entrada_devedor mat_entrada_credor mat_entrada_devedor
                   FROM zfiwrt2003
                   INTO (v_ope_cred_s, v_ope_dev_s, v_mat_cred_s, v_mat_dev_s,
                         v_ope_cred_e, v_ope_dev_e, v_mat_cred_e, v_mat_dev_e  )
                  WHERE cen_emissor = wa_saida-cen_emissor.

          IF sy-subrc = 0.
            IF     wa_saida-deb_cred EQ 'D'.
              lv_value1 = v_ope_dev_s.
              lv_value2 = v_mat_dev_s.
              lv_value3 = v_ope_dev_e.
              lv_value4 = v_mat_dev_e.
            ELSEIF wa_saida-deb_cred EQ 'C'.
              lv_value1 = v_ope_cred_s.
              lv_value2 = v_mat_cred_s.
              lv_value3 = v_ope_cred_e.
              lv_value4 = v_mat_cred_e.
            ENDIF.

            wa_saida-operacao_s = lv_value1.
            wa_saida-matnr_s    = lv_value2.
            wa_saida-operacao_e = lv_value3.
            wa_saida-matnr_e    = lv_value4.
            MODIFY it_saida FROM wa_saida INDEX ls_good-row_id.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'OPERACAO_S'
                i_value     = lv_value1.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'MATNR_S'
                i_value     = lv_value2.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'OPERACAO_E'
                i_value     = lv_value3.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'MATNR_E'
                i_value     = lv_value4.

          ELSE.
            CLEAR: wa_saida-operacao_s,
                   wa_saida-matnr_s,
                   wa_saida-operacao_e,
                   wa_saida-matnr_e.
            MODIFY it_saida FROM wa_saida INDEX ls_good-row_id.
            MESSAGE s024(sd) WITH 'Parametros não localizados!' DISPLAY LIKE 'E'.
          ENDIF.

        WHEN 'DEB_CRED'.

          wa_saida-deb_cred = lv_value.
          MODIFY it_saida FROM wa_saida INDEX ls_good-row_id.

          SELECT SINGLE ope_saida_credor   ope_saida_devedor   mat_saida_credor   mat_saida_devedor
                        ope_entrada_credor ope_entrada_devedor mat_entrada_credor mat_entrada_devedor
                   FROM zfiwrt2003
                   INTO (v_ope_cred_s, v_ope_dev_s, v_mat_cred_s, v_mat_dev_s,
                         v_ope_cred_e, v_ope_dev_e, v_mat_cred_e, v_mat_dev_e  )
                  WHERE cen_emissor = wa_saida-cen_emissor.

          IF sy-subrc = 0.
            IF     wa_saida-deb_cred EQ 'D'.
              lv_value1 = v_ope_dev_s.
              lv_value2 = v_mat_dev_s.
              lv_value3 = v_ope_dev_e.
              lv_value4 = v_mat_dev_e.
            ELSEIF wa_saida-deb_cred EQ 'C'.
              lv_value1 = v_ope_cred_s.
              lv_value2 = v_mat_cred_s.
              lv_value3 = v_ope_cred_e.
              lv_value4 = v_mat_cred_e.
            ENDIF.

            wa_saida-operacao_s = lv_value1.
            wa_saida-matnr_s    = lv_value2.
            wa_saida-operacao_e = lv_value3.
            wa_saida-matnr_e    = lv_value4.
            MODIFY it_saida FROM wa_saida INDEX ls_good-row_id.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'OPERACAO_S'
                i_value     = lv_value1.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'MATNR_S'
                i_value     = lv_value2.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'OPERACAO_E'
                i_value     = lv_value3.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'MATNR_E'
                i_value     = lv_value4.

          ELSE.
            CLEAR: wa_saida-operacao_s,
                   wa_saida-matnr_s,
                   wa_saida-operacao_e,
                   wa_saida-matnr_e.
            MODIFY it_saida FROM wa_saida INDEX ls_good-row_id.
            MESSAGE s024(sd) WITH 'Parametros não localizados!' DISPLAY LIKE 'E'.
          ENDIF.

        WHEN 'OPERACAO'.
*          CONDENSE lv_value NO-GAPS.
*          CALL METHOD er_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = ls_good-row_id
*              i_fieldname = 'OPERACAO'
*              i_value     = lv_value.
      ENDCASE.

    ENDLOOP.

    PERFORM refresh_alv.

  ENDMETHOD.

  METHOD on_hotsopt_click.

    DATA: vl_nfobjn TYPE j_1binterf-nfobjn,
          vl_docnum TYPE j_1bnfdoc-docnum,
          opt       TYPE ctu_params.

    CASE e_column_id.

      WHEN 'SEQ_LANC_S'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        SELECT * FROM  zfiwrt1000 INTO TABLE @DATA(tg_1000)
          WHERE field    EQ 'SEQ_LCTO'
          AND   seq_lcto EQ  @wa_saida-seq_lanc_s.

        IF wa_saida-seq_lanc_s IS NOT INITIAL.
          SET PARAMETER ID 'SEQ' FIELD  wa_saida-seq_lanc_s.
          CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.
        ENDIF.

      WHEN 'SEQ_LANC_E'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        SELECT * FROM  zfiwrt1000 INTO TABLE @tg_1000
          WHERE field    EQ 'SEQ_LCTO'
          AND   seq_lcto EQ  @wa_saida-seq_lanc_e.

        IF wa_saida-seq_lanc_e IS NOT INITIAL.
          SET PARAMETER ID 'SEQ' FIELD  wa_saida-seq_lanc_e.
          CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.
        ENDIF.
      WHEN 'DOCNUM_S'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        SET PARAMETER ID 'JEF'  FIELD wa_saida-docnum_s.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_E'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        SET PARAMETER ID 'JEF'  FIELD wa_saida-docnum_e.
        CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

      WHEN 'N_NFE_S'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_saida-docnum_s.
        SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_saida-bukrs.

        CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.

    ENDCASE.

  ENDMETHOD.

  METHOD data_changed_finished.

    DATA: wa_good_cells TYPE lvc_s_modi,
          v_ope_cred    TYPE zfiwrt2003-ope_saida_credor,
          v_ope_dev     TYPE zfiwrt2003-ope_saida_devedor,
          v_mat_cred    TYPE zfiwrt2003-mat_saida_credor,
          v_mat_dev     TYPE zfiwrt2003-mat_saida_devedor,
          v_ope_cred_s  TYPE zfiwrt2003-ope_saida_credor,
          v_ope_dev_s   TYPE zfiwrt2003-ope_saida_devedor,
          v_mat_cred_s  TYPE zfiwrt2003-mat_saida_credor,
          v_mat_dev_s   TYPE zfiwrt2003-mat_saida_devedor,
          v_ope_cred_e  TYPE zfiwrt2003-ope_entrada_credor,
          v_ope_dev_e   TYPE zfiwrt2003-ope_entrada_devedor,
          v_mat_cred_e  TYPE zfiwrt2003-mat_entrada_credor,
          v_mat_dev_e   TYPE zfiwrt2003-mat_entrada_devedor.

    LOOP AT et_good_cells INTO wa_good_cells.
      IF e_modified EQ abap_true.
        IF wa_good_cells-fieldname EQ 'EMISSOR'.
          READ TABLE it_saida INTO wa_saida INDEX wa_good_cells-row_id.

          SELECT SINGLE cen_receptor FROM zfiwrt2003
            INTO @DATA(v_centro_receptor)
            WHERE cen_emissor = @wa_saida-cen_emissor.

          IF sy-subrc = 0.

            wa_saida-cen_receptor = v_centro_receptor.

            MODIFY it_saida FROM wa_saida INDEX wa_good_cells-row_id.

          ENDIF.


        ELSEIF  wa_good_cells-fieldname EQ 'DEB_CRED'.
          READ TABLE it_saida INTO wa_saida INDEX wa_good_cells-row_id.

          SELECT SINGLE ope_saida_credor   ope_saida_devedor   mat_saida_credor   mat_saida_devedor
                        ope_entrada_credor ope_entrada_devedor mat_entrada_credor mat_entrada_devedor
                   FROM zfiwrt2003
                   INTO (v_ope_cred_s, v_ope_dev_s, v_mat_cred_s, v_mat_dev_s,
                         v_ope_cred_e, v_ope_dev_e, v_mat_cred_e, v_mat_dev_e  )
                  WHERE cen_emissor = wa_saida-cen_emissor.

          IF sy-subrc = 0.
            IF wa_saida-deb_cred EQ 'D'.
              wa_saida-operacao_e = v_ope_dev_e.
              wa_saida-matnr_e    = v_mat_dev_e.
              wa_saida-operacao_s = v_ope_dev_s.
              wa_saida-matnr_s    = v_mat_dev_s.
            ELSEIF wa_saida-deb_cred EQ 'C'.
              wa_saida-operacao_e = v_ope_cred_e.
              wa_saida-matnr_e    = v_mat_cred_e.
              wa_saida-operacao_s = v_ope_cred_s.
              wa_saida-matnr_s    = v_mat_cred_s.
            ENDIF.

            MODIFY it_saida FROM wa_saida INDEX wa_good_cells-row_id.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  FREE: t_exctab.

  IF p_proc_s = abap_true.
    MOVE 'SAVE'      TO w_exctab-fcode.
    APPEND w_exctab  TO t_exctab.
    MOVE 'INC'       TO w_exctab-fcode.
    APPEND w_exctab  TO t_exctab.
    MOVE 'DEL'       TO w_exctab-fcode.
    APPEND w_exctab  TO t_exctab.

  ENDIF.
  IF p_proc_s EQ abap_true OR p_proc_n EQ abap_true.
    MOVE 'PRT'       TO w_exctab-fcode.
    APPEND w_exctab  TO t_exctab.
  ENDIF.

  SET PF-STATUS 'PF0100' EXCLUDING t_exctab.
  SET TITLEBAR 'TI0100'.

  PERFORM fm_criar_objetos.
  PERFORM z_alv.
  PERFORM z_cria_alv.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: l_erro TYPE c.

  CASE sy-ucomm.
    WHEN 'BAC'.
      LEAVE TO SCREEN 0.

    WHEN 'SAVE'.
      PERFORM z_salvar      CHANGING l_erro.

    WHEN 'NWS'."        Gerar Nota Writer Saida
      PERFORM z_salvar      CHANGING l_erro.
      IF l_erro = abap_false.
        PERFORM z_gerar_writer USING 'S'.
        PERFORM fm_dados_seleciona.
      ENDIF.

    WHEN 'ESF'."        Envio SEFAZ
      PERFORM z_salvar CHANGING l_erro.
      IF l_erro = abap_false.
        PERFORM z_envia_sefaz.
      ENDIF.

    WHEN 'DNF'."        Danfe
      PERFORM z_imprime_danfe.

    WHEN 'EST'."        Estornar writer saida
      PERFORM z_estorno_saida.
      PERFORM fm_dados_seleciona.

    WHEN 'EST_E'."      Estornar writer entrada
      PERFORM z_estorno_entrada.
      PERFORM fm_dados_seleciona.

    WHEN 'ATZ'."        Atualizar Tela
      PERFORM fm_dados_seleciona.

    WHEN 'NWE'."        Gerar Nota Writer Entrada
      PERFORM z_salvar      CHANGING l_erro.
      IF l_erro = abap_false.
        PERFORM z_gerar_writer USING 'E'.
        PERFORM fm_dados_seleciona.
      ENDIF.

    WHEN 'INC'."        Inclui
      PERFORM z_inserir_linha.

    WHEN 'DEL'."        Deletar
      PERFORM z_excluir_linha.

    WHEN 'PRT'."        Parametrização NFe Transferência
      CALL TRANSACTION 'ZNFW0015'.
  ENDCASE.

  PERFORM refresh_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .

  PERFORM fm_dados_seleciona.
* PERFORM fm_dados_processa.
  CALL SCREEN 0100.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .
*  PERFORM fm_filtros.
*  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_filtros .
  DATA vl_text TYPE TABLE OF textpool.

*  CALL FUNCTION 'RS_TEXTPOOL_READ'
*    EXPORTING
*      objectname = sy-repid
*      action     = 'SHOW'
*      language   = sy-langu
*    TABLES
*      tpool      = vl_text.
*
*  FREE: git_filtro.

*  LOOP AT SCREEN.
*    git_filtro = VALUE #(
*      ( parametro = '' valor = p_bukrs )
*      ( parametro = '' valor = p_werks )
*    ).
*  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona .

  SELECT *
    FROM zfiwrt2004
    INTO CORRESPONDING FIELDS OF TABLE it_saida
   WHERE bukrs IN s_bukrs
     AND mes    = p_mes
     AND ano    = p_ano.

  PERFORM z_busca_dados_atualizado.
  PERFORM fm_dados_processa.

  IF     p_proc_s = abap_true.
    DELETE it_saida WHERE docnum_s IS INITIAL AND
                          docnum_e IS INITIAL.
  ELSEIF p_proc_n = abap_true.
    DELETE it_saida WHERE NOT ( docnum_s IS INITIAL AND
                                docnum_e IS INITIAL ).
  ENDIF.

  PERFORM z_alv.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_processa .

  LOOP AT it_saida INTO wa_saida.
    l_tabix = sy-tabix.

    IF wa_saida-docnum_s IS NOT INITIAL AND
       wa_saida-n_nfe_s  IS INITIAL.
      wa_saida-n_nfe_s = icon_execute_object.
    ENDIF.

    SELECT *
      INTO @DATA(w_2003)
        UP TO 1 ROWS
      FROM zfiwrt2003
     WHERE cen_emissor = @wa_saida-cen_emissor.
    ENDSELECT.

    CASE wa_saida-deb_cred.
      WHEN 'D'.
        IF wa_saida-operacao_s IS INITIAL.
          wa_saida-operacao_s = w_2003-ope_saida_devedor.
        ENDIF.
        IF wa_saida-matnr_s IS INITIAL.
          wa_saida-matnr_s = w_2003-mat_saida_devedor.
        ENDIF.
        IF wa_saida-operacao_e IS INITIAL.
          wa_saida-operacao_e = w_2003-ope_entrada_devedor.
        ENDIF.
        IF wa_saida-matnr_e IS INITIAL.
          wa_saida-matnr_e = w_2003-mat_entrada_devedor.
        ENDIF.

      WHEN 'C'.
        IF wa_saida-operacao_s IS INITIAL.
          wa_saida-operacao_s = w_2003-ope_saida_credor.
        ENDIF.
        IF wa_saida-matnr_s IS INITIAL.
          wa_saida-matnr_s = w_2003-mat_saida_credor.
        ENDIF.
        IF wa_saida-operacao_e IS INITIAL.
          wa_saida-operacao_e = w_2003-ope_entrada_credor.
        ENDIF.
        IF wa_saida-matnr_e IS INITIAL.
          wa_saida-matnr_e = w_2003-mat_entrada_credor.
        ENDIF.
    ENDCASE.

    MODIFY it_saida FROM wa_saida INDEX l_tabix.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos .

ENDFORM.

FORM z_alv .

  "CLEAR wl_fcat.
  REFRESH it_fcat[].

  PERFORM preenche_cat USING:
          'CEN_EMISSOR'      'ZFIWRT2004' 'CEN_EMISSOR' 'Emissor'            '12'       ''     ''    ''    ''    '' 'X' ' ',
          'CEN_RECEPTOR'     ' ' ' '                    'Receptor'           '12'       ''     ''    ''    ''    '' ' ' ' ',
          'ICMS'             'ZFIWRT2004' 'ICMS'        'Valor ICMS'         '15'       ''     ''    ''    ''    '' 'X' ' ',
          'DEB_CRED'         'ZFIWRT2004' 'DEB_CRED'    'Debito/Credito'     '13'       ''     ''    ''    ''    '' 'X' ' ',
          'OPERACAO_E'       ' ' ' '                    'Operação Entrada'   '16'       ''     ''    ''    ''    '' ' ' ' ',
          'MATNR_E'          ' ' ' '                    'Material Entrada'   '20'       ''     ''    ''    ''    '' ' ' ' ',
          'OPERACAO_S'       ' ' ' '                    'Operação Saída'     '16'       ''     ''    ''    ''    '' ' ' ' ',
          'MATNR_S'          ' ' ' '                    'Material Saída'     '20'       ''     ''    ''    ''    '' ' ' ' ',
          'SEQ_LANC_S'       ' ' ' '                    'Seq.Lancto Saída'   '17'       ''     'X'   ''    ''    '' ' ' ' ',
          'DOCNUM_S'         ' ' ' '                    'Docnum Saída'       '13'       ''     'X'   ''    ''    '' ' ' ' ',
          'N_NFE_S'          ' ' ' '                    'Num NFE Saída'      '13'       ''     'X'   ''    ''    '' ' ' 'X',
          'SEQ_LANC_E'       ' ' ' '                    'Seq.Lancto Entrada' '17'       ''     'X'   ''    ''    '' ' ' ' ',
          'DOCNUM_E'         ' ' ' '                    'Docnum Entrada'     '13'       ''     'X'   ''    ''    '' ' ' ' ',
          'MSG_RET_SEFAZ'    ' ' ' '                    'Mens. Ret. Sefaz'   '50'       ''     ''    ''    ''    '' ' ' ' '.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_ref_table)
                        VALUE(p_ref_field)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_cor)
                        VALUE(p_edit)
                        VALUE(p_icon).

  wl_fcat-fieldname = p_campo.
  wl_fcat-ref_table = p_ref_table.
  wl_fcat-ref_field = p_ref_field.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.
  wl_fcat-just      = p_just.
  wl_fcat-emphasize = p_cor.
  wl_fcat-edit      = p_edit.
  wl_fcat-icon      = p_icon.

  IF p_proc_s = abap_true.
    wl_fcat-edit    = abap_false.
  ENDIF.

*  CASE  p_campo.
*    WHEN 'EMISSOR'     OR 'VAL_ICMS'   OR 'DEB_CRED'.
*      wl_fcat-edit = 'X'.
*  ENDCASE.

  APPEND wl_fcat TO  it_fcat.
  CLEAR wl_fcat.
ENDFORM.

**************************************************************************
* criar alv
**************************************************************************
FORM z_cria_alv .
  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid IS INITIAL AND  g_custom_container IS NOT INITIAL.
      CREATE OBJECT g_grid
        EXPORTING
          i_parent          = g_custom_container
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    PERFORM toolbar_alv.

    IF p_proc_s = abap_true.
      wa_layout-edit       = abap_false. " Makes all Grid editable
    ENDIF.

    wa_layout-zebra        = abap_false.
*   wa_layout-edit         = abap_true. " Makes all Grid editable
    wa_layout-no_totarr    = abap_true.
*   wa_layout-cwidth_opt   = 'X'.
    wa_layout-no_totexp    = abap_true.
    wa_layout-no_totline   = abap_true.
    wa_layout-no_toolbar   = abap_false.
    wa_layout-ctab_fname   = 'COLORCELL'.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        i_save                        = 'A'
        it_toolbar_excluding          = pt_exclude
*       it_toolbar_excluding          = tl_function
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = it_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    SET HANDLER lcl_event_handler=>on_hotsopt_click  FOR g_grid.
    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.
    wa_stable-row = 'X'.
    wa_stable-col = 'X'.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
*       i_soft_refresh = 'X'.
  ENDIF.
ENDFORM.

*******************************************************************************************
* Form  funcrtion
*******************************************************************************************
FORM toolbar_alv.

  FREE: pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_mb_export TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_sort TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_separator TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_call_more TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_filter TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_find TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_find_more TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_average TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_auf TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_subtot TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_sum TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_mb_view TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_print TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_graph TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_info TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_detail TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_check TO pt_exclude.
*  APPEND cl_gui_alv_grid=>mc_fc_call_abc TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_mb_paste TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO pt_exclude.
*
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO pt_exclude.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO pt_exclude.

ENDFORM.
**************************************************************************
* insere linha
**************************************************************************
FORM z_inserir_linha.

  CLEAR wa_saida.
  wa_saida-bukrs   = s_bukrs-low.
  wa_saida-mes     = p_mes.
  wa_saida-ano     = p_ano.
  APPEND wa_saida TO it_saida.

ENDFORM.

**************************************************************************
* salvar documnto
**************************************************************************
FORM z_salvar CHANGING p_erro.

  DATA: l_erro TYPE c.

  CLEAR: l_erro,
         p_erro.

  CALL METHOD g_grid->check_changed_data.

  PERFORM z_valida_dados USING l_erro.

  IF l_erro = abap_false.
    PERFORM f_grava.
  ENDIF.

  p_erro = l_erro.

ENDFORM.

**************************************************************************
* gravar docimento
**************************************************************************
FORM f_grava.

  LOOP AT it_saida_del  INTO wa_saida_del.
    DELETE FROM zfiwrt2004 WHERE cen_emissor  = wa_saida_del-cen_emissor
                             AND cen_receptor = wa_saida_del-cen_receptor
                             AND bukrs        = wa_saida_del-bukrs
                             AND mes          = wa_saida_del-mes
                             AND ano          = wa_saida_del-ano.
  ENDLOOP.

  LOOP AT  it_saida INTO wa_saida.

    IF wa_saida-seq_lanc_s IS NOT INITIAL OR
       wa_saida-seq_lanc_e IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    wa_zfiwrt2004-ano           = p_ano.
    wa_zfiwrt2004-bukrs         = s_bukrs-low .
    wa_zfiwrt2004-mes           = p_mes.
    wa_zfiwrt2004-cen_emissor   = wa_saida-cen_emissor.
    wa_zfiwrt2004-cen_receptor  = wa_saida-cen_receptor.
    wa_zfiwrt2004-icms          = wa_saida-icms.
    wa_zfiwrt2004-deb_cred      = wa_saida-deb_cred.
    wa_zfiwrt2004-operacao_e    = wa_saida-operacao_e.
    wa_zfiwrt2004-matnr_e       = wa_saida-matnr_e.
    wa_zfiwrt2004-operacao_s    = wa_saida-operacao_s.
    wa_zfiwrt2004-matnr_s       = wa_saida-matnr_s.
    wa_zfiwrt2004-seq_lanc_s    = wa_saida-seq_lanc_s.
    wa_zfiwrt2004-seq_lanc_e    = wa_saida-seq_lanc_e.
    wa_zfiwrt2004-docnum_s      = wa_saida-docnum_s.
    wa_zfiwrt2004-docnum_s      = wa_saida-docnum_s.
    wa_zfiwrt2004-n_nfe_s       = wa_saida-n_nfe_s.
    wa_zfiwrt2004-msg_ret_sefaz = wa_saida-msg_ret_sefaz.

    MODIFY zfiwrt2004 FROM wa_zfiwrt2004.
  ENDLOOP.

ENDFORM.

**************************************************************************
* validacao
**************************************************************************
FORM z_valida_dados USING p_erro.

  LOOP AT  it_saida INTO wa_saida.
    FREE: t_colorcell.

    l_tabix = sy-tabix.

    IF wa_saida-seq_lanc_s IS NOT INITIAL OR
       wa_saida-seq_lanc_e IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    SELECT *
      INTO @DATA(w_2004)
      FROM zfiwrt2004
        UP TO 1 ROWS
     WHERE cen_emissor  = @wa_saida-cen_emissor
       AND cen_receptor = @wa_saida-cen_receptor
       AND bukrs        = @s_bukrs-low
       AND mes          = @p_mes
       AND ano          = @p_ano
       AND ( seq_lanc_s  <> ''
        OR   seq_lanc_e  <> '' ).
    ENDSELECT.

    IF sy-subrc = 0.
      p_erro = abap_true.
      PERFORM f_color_cell USING 'CEN_EMISSOR'.
      PERFORM f_color_cell USING 'CEN_RECEPTOR'.
      MESSAGE s024(sd) WITH TEXT-110 DISPLAY LIKE 'E'.
    ENDIF.

    SELECT werks
      INTO @DATA(l_werks)
        UP TO 1 ROWS
      FROM t001w
     WHERE werks = @wa_saida-cen_emissor.
    ENDSELECT.

    IF sy-subrc <> 0.
      p_erro = abap_true.
      PERFORM f_color_cell USING 'CEN_EMISSOR'.
      MESSAGE s024(sd) WITH TEXT-100 DISPLAY LIKE 'E'.
    ENDIF.

    IF wa_saida-icms IS INITIAL.
      p_erro = abap_true.
      PERFORM f_color_cell USING 'ICMS'.
      MESSAGE s024(sd) WITH TEXT-101 DISPLAY LIKE 'E'.
    ENDIF.

    IF wa_saida-deb_cred <> 'C' AND
       wa_saida-deb_cred <> 'D'.
      p_erro = abap_true.
      PERFORM f_color_cell USING 'DEB_CRED'.
      MESSAGE s024(sd) WITH TEXT-102 DISPLAY LIKE 'E'.
    ENDIF.


  ENDLOOP.

ENDFORM.

**************************************************************************
* TRATAEMNTO ERROS
**************************************************************************
FORM f_color_cell USING p_campo.

  w_colorcell-fname = p_campo.
  w_colorcell-color-col = '6'.
  w_colorcell-color-int = '1'.
  w_colorcell-color-inv = '1'.
  APPEND w_colorcell  TO t_colorcell.

  wa_saida-colorcell[] = t_colorcell[].

  MODIFY it_saida FROM wa_saida INDEX l_tabix.
ENDFORM.

**************************************************************************
* EXCLUI LINHA
**************************************************************************
FORM z_excluir_linha.

  CLEAR: it_selected_rows, wa_selected_rows.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_saida INTO DATA(wa_saida_del) INDEX wa_selected_rows-index.
    IF sy-subrc IS INITIAL.
      IF wa_saida_del-seq_lanc_e IS NOT INITIAL OR
         wa_saida_del-seq_lanc_s IS NOT INITIAL.
        MESSAGE 'A linha selecionada já possui Documento gerado. Impossivel deletar a linha.' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        wa_saida_del-deletar = 'X'.
        MODIFY it_saida FROM wa_saida_del INDEX wa_selected_rows-index.
      ENDIF.
    ENDIF.
  ENDLOOP.

  it_saida_del[] = it_saida[].

  DELETE it_saida_del WHERE deletar = abap_false.
  DELETE it_saida     WHERE deletar = abap_true.

ENDFORM.

**************************************************************************
* RESFESR SLV
**************************************************************************
FORM refresh_alv.

  DATA: wa_stable TYPE lvc_s_stbl.

  wa_stable-row = 'X'.
  wa_stable-col = 'X'.

*  CALL METHOD g_grid->get_frontend_layout
*    IMPORTING
*      es_layout = wa_layout.

* wa_layout-cwidth_opt = abap_true.

*  CALL METHOD g_grid->set_frontend_layout
*    EXPORTING
*      is_layout = wa_layout.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
*     i_soft_refresh = 'X'.

ENDFORM.

**************************************************************************
* gerar writer
**************************************************************************
FORM z_gerar_writer USING p_tipo .

  DATA: v_zfbdt      TYPE zfiwrt0011-zfbdt,
        v_data_aux   TYPE sy-datum,
        wl_cont      TYPE sy-tabix,
        wl_linha     TYPE sy-tabix,
        wl_cont_aux  TYPE sy-tabix,
        wl_cont_aux2 TYPE sy-tabix,
        v_erro(1).

  DATA: l_data   TYPE  p0001-begda,
        l_days   TYPE  t5a4a-dlydy,
        l_months TYPE  t5a4a-dlymo,
        l_signum TYPE  t5a4a-split,
        l_years  TYPE  t5a4a-dlyyr.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'E'.
    EXIT.
  ELSE.

    IF p_tipo = 'E'.

*** BUG - 72282 - Inicio - CSB
      LOOP AT tg_selectedrow INTO wg_selectedrow.
        READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.
        IF wa_saida-seq_lanc_s IS NOT INITIAL.
          SELECT SINGLE *
            FROM j_1bnfe_active
            INTO @DATA(wa_active)
            WHERE docnum EQ @wa_saida-docnum_s.
          IF sy-subrc = 0.
            IF wa_active-docsta <> '1'.  "1 autorizado
              IF  wa_active-docsta = '2' . "2 recusado
                v_erro = abap_true.
                MESSAGE |Saída Recusado pela SEFAZ, DocNum { wa_saida-docnum_s } |   TYPE 'E'.
                EXIT.
              ELSEIF   wa_active-docsta = '3' . "3 rejeitado
                v_erro = abap_true.
                MESSAGE |SEFAZ rejeitou a saída!, DocNum { wa_saida-docnum_s } |  TYPE 'E'.
                EXIT.
              ELSEIF wa_active-docsta IS INITIAL.
                v_erro = abap_true.
                MESSAGE |Não enviado SEEFAZ a saída!, DocNum { wa_saida-docnum_s } |  TYPE 'E'.
                EXIT.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF v_erro = abap_true.
        EXIT.
      ENDIF.

*** BUG - 72282 - Fim - CSB

      SELECT *  FROM  zfiwrt0001  INTO TABLE it_zfiwrt0001
        FOR ALL ENTRIES IN it_saida
      WHERE operacao EQ it_saida-operacao_e.
    ELSE.
      SELECT *  FROM  zfiwrt0001  INTO TABLE it_zfiwrt0001
        FOR ALL ENTRIES IN it_saida
      WHERE operacao EQ it_saida-operacao_s.
    ENDIF.

    IF sy-subrc <> 0.
      MESSAGE 'Não existe Operação Nota Writer Cadastado' TYPE 'I'.
      EXIT.
    ENDIF.

    SELECT  * FROM t001w INTO TABLE it_t001w
      FOR ALL ENTRIES IN it_saida
      WHERE werks EQ it_saida-cen_emissor.

    SELECT  * FROM j_1baa INTO TABLE  it_j_1baa
     FOR ALL ENTRIES IN it_zfiwrt0001
      WHERE nftype EQ it_zfiwrt0001-nftype.

    IF p_tipo = 'E'.
      SELECT *  FROM zfiwrt0002  INTO TABLE it_zfiwrt0002
        FOR ALL ENTRIES IN it_saida
        WHERE operacao EQ it_saida-operacao_e.
    ELSE.
      SELECT *  FROM zfiwrt0002  INTO TABLE it_zfiwrt0002
        FOR ALL ENTRIES IN it_saida
        WHERE operacao EQ it_saida-operacao_s.
    ENDIF.

    IF sy-subrc IS INITIAL.

      SELECT * FROM j_1baj  INTO TABLE it_j_1baj
        FOR ALL ENTRIES IN it_zfiwrt0002
        WHERE taxtyp EQ it_zfiwrt0002-taxtyp.

      SELECT *  FROM j_1bajt INTO TABLE it_j_1bajt
        FOR ALL ENTRIES IN it_zfiwrt0002
        WHERE spras  EQ sy-langu
        AND   taxtyp EQ it_zfiwrt0002-taxtyp.

    ENDIF.

    SELECT *  FROM zfiwrt0003  INTO TABLE it_zfiwrt0003
      FOR ALL ENTRIES IN it_zfiwrt0021
       WHERE operacao EQ it_zfiwrt0021-operacao.

    IF sy-subrc IS INITIAL.

      SELECT  *  FROM tbsl  INTO TABLE it_tbsl
        FOR ALL ENTRIES IN it_zfiwrt0003
         WHERE bschl EQ it_zfiwrt0003-bschl.

      SELECT  *  FROM skat INTO TABLE it_skat
        FOR ALL ENTRIES IN it_zfiwrt0003
          WHERE spras EQ sy-langu
            AND ktopl EQ '0050'
            AND saknr EQ it_zfiwrt0003-hkont.

    ENDIF.

    IF p_tipo = 'E'.
      SELECT  *  FROM zfiwrt0004 INTO TABLE  it_zfiwrt0004
        FOR ALL ENTRIES IN it_saida
         WHERE operacao EQ it_saida-operacao_e.

      SELECT  * FROM zfiwrt0005  INTO TABLE it_zfiwrt0005
        FOR ALL ENTRIES IN it_saida
       WHERE operacao EQ it_saida-operacao_e.


      SELECT * FROM zfiwrt0006  INTO TABLE it_zfiwrt0006
        FOR ALL ENTRIES IN it_saida
       WHERE operacao EQ it_saida-operacao_e.

      SELECT  * FROM zfiwrt0007   INTO TABLE it_zfiwrt0007
        FOR ALL ENTRIES IN it_saida
          WHERE operacao EQ it_saida-operacao_e
            AND branch   EQ it_saida-cen_emissor
            AND tipo     EQ 'W'.
    ELSE.
      SELECT  *  FROM zfiwrt0004 INTO TABLE  it_zfiwrt0004
        FOR ALL ENTRIES IN it_saida
         WHERE operacao EQ it_saida-operacao_s.

      SELECT  * FROM zfiwrt0005  INTO TABLE it_zfiwrt0005
        FOR ALL ENTRIES IN it_saida
       WHERE operacao EQ it_saida-operacao_s.


      SELECT * FROM zfiwrt0006  INTO TABLE it_zfiwrt0006
        FOR ALL ENTRIES IN it_saida
       WHERE operacao EQ it_saida-operacao_s.

      SELECT  * FROM zfiwrt0007   INTO TABLE it_zfiwrt0007
        FOR ALL ENTRIES IN it_saida
          WHERE operacao EQ it_saida-operacao_s
            AND branch   EQ it_saida-cen_emissor
            AND tipo     EQ 'W'.
    ENDIF.

    SORT: it_zfiwrt0004 BY operacao,
          it_zfiwrt0005 BY operacao seqnum linnum,
          it_zfiwrt0006 BY operacao indcoper,
          it_zfiwrt0007 BY operacao.

    DATA(_lcto_gerados) = abap_false.

    LOOP AT tg_selectedrow INTO wg_selectedrow.
      READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.

      l_tabix = sy-tabix.

      SELECT *
        INTO @DATA(w_2003)
          UP TO 1 ROWS
        FROM zfiwrt2003
       WHERE cen_emissor = @wa_saida-cen_emissor.
      ENDSELECT.

      CHECK  sy-subrc = 0.

      CASE wa_saida-deb_cred.
        WHEN 'D'.
          IF p_tipo = 'E'.
            wa_saida-operacao_e = w_2003-ope_entrada_devedor.
            wa_saida-matnr_e    = w_2003-mat_entrada_devedor.
          ELSE.
            wa_saida-operacao_s = w_2003-ope_saida_devedor.
            wa_saida-matnr_s    = w_2003-mat_saida_devedor.
          ENDIF.

        WHEN 'C'.
          IF p_tipo = 'E'.
            wa_saida-operacao_e = w_2003-ope_entrada_credor.
            wa_saida-matnr_e    = w_2003-mat_entrada_credor.
          ELSE.
            wa_saida-operacao_s = w_2003-ope_saida_credor.
            wa_saida-matnr_s    = w_2003-mat_saida_credor.
          ENDIF.
      ENDCASE.
      MODIFY it_saida FROM wa_saida INDEX l_tabix.
    ENDLOOP.

    LOOP AT tg_selectedrow INTO wg_selectedrow.

      REFRESH: it_zfiwrt0008,
               it_zfiwrt0009,
               it_zfiwrt0010,
               it_zfiwrt0011,
               it_zfiwrt0012,
               it_zfiwrt0013,
               it_zfiwrt0015,
               it_zfiwrt22.

      READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.

      IF p_tipo = 'S' AND wa_saida-seq_lanc_s IS NOT INITIAL.
        MESSAGE s024(sd) WITH 'Documento ZNFW já foi gerado!' DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      IF p_tipo = 'E' AND wa_saida-seq_lanc_e IS NOT INITIAL.
        MESSAGE s024(sd) WITH 'Documento ZNFW já foi gerado!' DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      IF p_tipo = 'E' AND wa_saida-docnum_s IS INITIAL.
        MESSAGE s024(sd) WITH 'Gerar primeiro a NF de saída!' DISPLAY LIKE 'E'.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING wa_saida TO wa_saida_aux.

      CLEAR: wl_cont_aux2, wl_cont_aux, wl_cont.
      REFRESH: tg_mensagens.

      IF p_tipo = 'E'.
        LOOP AT it_zfiwrt0005 INTO wa_zfiwrt0005 WHERE  operacao = wa_saida-operacao_e.
          tg_mensagens-linnum    =  wa_zfiwrt0005-linnum.
          tg_mensagens-seqnum    =  wa_zfiwrt0005-seqnum.
          tg_mensagens-message   =  wa_zfiwrt0005-message.
          APPEND tg_mensagens.
          ADD 1 TO wl_cont.
        ENDLOOP.
      ELSE.
        LOOP AT it_zfiwrt0005 INTO wa_zfiwrt0005 WHERE  operacao = wa_saida-operacao_s.
          tg_mensagens-linnum    =  wa_zfiwrt0005-linnum.
          tg_mensagens-seqnum    =  wa_zfiwrt0005-seqnum.
          tg_mensagens-message   =  wa_zfiwrt0005-message.
          APPEND tg_mensagens.
          ADD 1 TO wl_cont.
        ENDLOOP.
      ENDIF.

      REFRESH: tg_movest.

      IF p_tipo = 'E'.
        LOOP AT it_zfiwrt0004 INTO wa_zfiwrt0004 WHERE  operacao = wa_saida-operacao_e.
          MOVE: wa_zfiwrt0004-bwart   TO tg_movest-bwart,
                wa_zfiwrt0004-tcode   TO tg_movest-tcode,
                wa_zfiwrt0004-mwskz1  TO tg_movest-mwskz1,
                wa_zfiwrt0004-estorno TO tg_movest-estorno.
          APPEND tg_movest.
          CLEAR: tg_movest, wa_zfiwrt0004.
        ENDLOOP.
      ELSE.
        LOOP AT it_zfiwrt0004 INTO wa_zfiwrt0004 WHERE  operacao = wa_saida-operacao_s.
          MOVE: wa_zfiwrt0004-bwart   TO tg_movest-bwart,
                wa_zfiwrt0004-tcode   TO tg_movest-tcode,
                wa_zfiwrt0004-mwskz1  TO tg_movest-mwskz1,
                wa_zfiwrt0004-estorno TO tg_movest-estorno.
          APPEND tg_movest.
          CLEAR: tg_movest, wa_zfiwrt0004.
        ENDLOOP.
      ENDIF.

      IF p_tipo = 'E'.
        READ TABLE it_zfiwrt0001 INTO wa_zfiwrt0001 WITH KEY  operacao = wa_saida-operacao_e.
      ELSE.
        READ TABLE it_zfiwrt0001 INTO wa_zfiwrt0001 WITH KEY  operacao = wa_saida-operacao_s.
      ENDIF.

      READ TABLE it_j_1baa INTO wa_j_1baa WITH KEY  nftype =  wa_zfiwrt0001-nftype.

      READ TABLE it_t001w INTO wa_t001w  WITH KEY werks = wa_saida-cen_emissor.

      wl_indcoper = 'D'.
      PERFORM f_define_origem_destino USING    "wa_kna1
                                               wa_lfa1
                                               wa_t001w
                                               wa_j_1baa
                                               wa_zfiwrt0001
                                      CHANGING wl_indcoper
                                               wl_texto_fiscal.

      IF p_tipo = 'E'.
        SELECT SINGLE * FROM mara
          INTO wa_mara
          WHERE matnr EQ wa_saida-matnr_e.

        READ TABLE it_zfiwrt0006 INTO wa_zfiwrt0006 WITH KEY  operacao = wa_saida-operacao_e
                                                              indcoper = wl_indcoper BINARY SEARCH.
      ELSE.
        SELECT SINGLE * FROM mara
          INTO wa_mara
          WHERE matnr EQ wa_saida-matnr_s.

        READ TABLE it_zfiwrt0006 INTO wa_zfiwrt0006 WITH KEY  operacao = wa_saida-operacao_s
                                                              indcoper = wl_indcoper BINARY SEARCH.
      ENDIF.
      IF sy-subrc NE 0.
        MESSAGE |Erro na operacao { wa_saida-operacao_s } | TYPE 'I'.
        CONTINUE.
      ENDIF.

*-------------------------------------------------------
* cabecalho
*-------------------------------------------------------
      l_mes = sy-datum+4(2) - 1.
      l_ano = sy-datum(4).

      IF l_mes = 0.
        l_mes = 12.
        l_ano = l_ano - 1.
      ENDIF.

      l_datum = l_ano && l_mes && '01'.

      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = l_datum
        IMPORTING
          last_day_of_month = l_lastday
        EXCEPTIONS
          day_in_no_date    = 1
          OTHERS            = 2.

      IF p_tipo = 'E'.
        MOVE wa_saida-operacao_e              TO wa_zfiwrt0008-operacao.

**================Inicio "BUG IMPEDITIVO 73132 /  Anderson Oenning
        "Recuperando o numero da NFe e Série.
        IF wa_saida-docnum_s IS NOT INITIAL.
          SELECT SINGLE series nfenum
          FROM j_1bnfdoc INTO (wa_zfiwrt0008-series, wa_zfiwrt0008-nfenum)
            WHERE docnum EQ wa_saida-docnum_s.
        ENDIF.
**================Fim "BUG IMPEDITIVO 73132 /  Anderson Oenning

      ELSE.
        MOVE wa_saida-operacao_s              TO wa_zfiwrt0008-operacao.
      ENDIF.

      MOVE:  sy-mandt                         TO wa_zfiwrt0008-mandt,
*            wa_saida-operacao                TO wa_zfiwrt0008-operacao,
             wa_saida-bukrs                   TO wa_zfiwrt0008-bukrs,
*            wa_saida-cen_emissor             TO wa_zfiwrt0008-branch,
             wa_zfiwrt0001-parvw              TO wa_zfiwrt0008-parvw,
*            wa_saida-cen_receptor            TO wa_zfiwrt0008-parid,
             wa_zfiwrt0001-nftype             TO wa_zfiwrt0008-nftype,
*            wa_saida-cen_emissor             TO wa_zfiwrt0008-move_plant,
             wa_zfiwrt0001-energia            TO wa_zfiwrt0008-energia,
             wa_zfiwrt0001-servico            TO wa_zfiwrt0008-servico,
             wa_zfiwrt0001-complemento        TO wa_zfiwrt0008-complemento,
             'CIF'                            TO wa_zfiwrt0008-inco1,
             'CIF'                            TO wa_zfiwrt0008-inco2,
             wa_zfiwrt0001-referencia         TO wa_zfiwrt0008-referencia,
             wa_zfiwrt0006-cfop               TO wa_zfiwrt0008-cfop,
             wa_zfiwrt0006-taxlw1             TO wa_zfiwrt0008-taxlw1,
             wa_zfiwrt0006-taxlw2             TO wa_zfiwrt0008-taxlw2,
             wa_zfiwrt0006-taxlw4             TO wa_zfiwrt0008-taxlw4,
             wa_zfiwrt0006-taxlw5             TO wa_zfiwrt0008-taxlw5,
             wa_zfiwrt0006-opertyp            TO wa_zfiwrt0008-opertyp,
             wa_zfiwrt0006-taxcode            TO wa_zfiwrt0008-taxcode,
             'A'                              TO wa_zfiwrt0008-status,
             sy-uname                         TO wa_zfiwrt0008-usuario_ult_mod,
             sy-datum                         TO wa_zfiwrt0008-dt_ult_mod,
             sy-uzeit                         TO wa_zfiwrt0008-hr_ult_mod,
             l_lastday                        TO wa_zfiwrt0008-budat,
             l_lastday                        TO wa_zfiwrt0008-bldat,
             sy-uname                         TO wa_zfiwrt0008-usnam,
             sy-datum                         TO wa_zfiwrt0008-dt_criacao,
             sy-uzeit                         TO wa_zfiwrt0008-hr_criacao.

      IF p_tipo = 'S'.
        l_parid = wa_saida-cen_receptor.
        MOVE: wa_saida-cen_emissor            TO wa_zfiwrt0008-branch,
              l_parid                         TO wa_zfiwrt0008-parid,
              wa_saida-cen_emissor            TO wa_zfiwrt0008-move_plant.
      ELSE.
        l_parid = wa_saida-cen_emissor.
        MOVE: wa_saida-cen_receptor           TO wa_zfiwrt0008-branch,
              l_parid                         TO wa_zfiwrt0008-parid,
              wa_saida-cen_receptor           TO wa_zfiwrt0008-move_plant.
      ENDIF.

      APPEND wa_zfiwrt0008 TO it_zfiwrt0008.

      IF p_tipo = 'E'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_saida-matnr_e
          IMPORTING
            output = wa_saida-matnr_e.

        SELECT SINGLE *  FROM marc  INTO wa_marc
          WHERE matnr EQ  wa_saida-matnr_e.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_saida-matnr_s
          IMPORTING
            output = wa_saida-matnr_s.

        SELECT SINGLE *  FROM marc  INTO wa_marc
          WHERE matnr EQ  wa_saida-matnr_s.
      ENDIF.

      REFRESH  tg_itens.
      tg_itens-itmnum  = 10.
      IF p_tipo = 'E'.
        tg_itens-matnr   = wa_saida-matnr_e.
      ELSE.
        tg_itens-matnr   = wa_saida-matnr_s.
      ENDIF.

      IF p_tipo = 'E'.
        SELECT SINGLE maktx FROM makt INTO tg_itens-maktx WHERE matnr EQ wa_saida-matnr_e.
      ELSE.
        SELECT SINGLE maktx FROM makt INTO tg_itens-maktx WHERE matnr EQ wa_saida-matnr_s.
      ENDIF.

      tg_itens-cfop    = wa_zfiwrt0006-cfop.
      tg_itens-werks   = wa_saida-cen_emissor.
*     tg_itens-menge   = wa_saida-icms.
      tg_itens-meins   = wa_mara-meins.
*     tg_itens-netpr   = wa_saida-icms.
      tg_itens-netwr   = ( tg_itens-netpr  * tg_itens-menge ).
      tg_itens-steuc   = wa_marc-steuc.
      APPEND tg_itens.

*-------------------------------------------------------
* itens
*-------------------------------------------------------
      LOOP AT tg_itens.
        MOVE: sy-mandt          TO wa_zfiwrt0009-mandt,
         wa_zfiwrt0008-seq_lcto TO wa_zfiwrt0009-seq_lcto,
         tg_itens-itmnum        TO wa_zfiwrt0009-itmnum,
         tg_itens-matnr         TO wa_zfiwrt0009-matnr,
         tg_itens-cfop          TO wa_zfiwrt0009-cfop,
         tg_itens-charg         TO wa_zfiwrt0009-charg,
         tg_itens-menge         TO wa_zfiwrt0009-menge,
         tg_itens-meins         TO wa_zfiwrt0009-meins,
         tg_itens-netpr         TO wa_zfiwrt0009-netpr,
         tg_itens-netwr         TO wa_zfiwrt0009-netwr,
         wa_zfiwrt0001-itmtyp   TO wa_zfiwrt0009-itmtyp,
         tg_itens-werks         TO wa_zfiwrt0009-bwkey,
         tg_itens-lgort         TO wa_zfiwrt0009-lgort,
         tg_itens-anln1         TO wa_zfiwrt0009-anln1,
         tg_itens-anln2         TO wa_zfiwrt0009-anln2.
        APPEND wa_zfiwrt0009 TO it_zfiwrt0009.
      ENDLOOP.

*-------------------------------------------------------
* parceiro
*-------------------------------------------------------
      REFRESH tg_parc.
      tg_parc-parvw = wa_zfiwrt0008-parvw.
      tg_parc-parid = wa_zfiwrt0008-parid.
      APPEND tg_parc.

      LOOP AT tg_parc.
        MOVE: sy-mandt                 TO wa_zfiwrt0015-mandt,
              wa_zfiwrt0008-seq_lcto   TO wa_zfiwrt0015-seq_lcto,
              tg_parc-parvw            TO wa_zfiwrt0015-parvw,
              tg_parc-parid            TO wa_zfiwrt0015-parid.
        APPEND wa_zfiwrt0015 TO it_zfiwrt0015.
      ENDLOOP.

*--------------------------------------------------------------------------------------------------------*
*   Imposto
*--------------------------------------------------------------------------------------------------------*
      FREE: it_zfiwrt0010.

      CLEAR: wa_zfiwrt0010.
      wa_zfiwrt0010-mandt  = sy-mandt.
      wa_zfiwrt0010-itmnum = 10.
      wa_zfiwrt0010-taxtyp = 'ICM3'.
      wa_zfiwrt0010-taxval = wa_saida-icms.
      APPEND wa_zfiwrt0010  TO it_zfiwrt0010.

      CLEAR: wa_zfiwrt0010.
      wa_zfiwrt0010-mandt  = sy-mandt.
      wa_zfiwrt0010-itmnum = 10.
      wa_zfiwrt0010-taxtyp = 'ICOF'.
      wa_zfiwrt0010-othbas = wa_saida-icms.
      APPEND wa_zfiwrt0010  TO it_zfiwrt0010.

      CLEAR: wa_zfiwrt0010.
      wa_zfiwrt0010-mandt  = sy-mandt.
      wa_zfiwrt0010-itmnum = 10.
      wa_zfiwrt0010-taxtyp = 'IPIS'.
      wa_zfiwrt0010-othbas = wa_saida-icms.
      APPEND wa_zfiwrt0010  TO it_zfiwrt0010.


*--------------------------------------------------------------------------------------------------------*
* montar tabelas
*--------------------------------------------------------------------------------------------------------*
      TRY.
          zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento(
                                                     )->set_cabecalho(       EXPORTING i_cabecalho   = wa_zfiwrt0008
                                                     )->add_item(            EXPORTING i_item        = wa_zfiwrt0009
                                                     )->validar_registro(
                                                     )->prepara_lancamento(  EXPORTING i_impostos    = it_zfiwrt0010[]
                                                     )->add_parceiro(        EXPORTING i_parceiro    = wa_zfiwrt0015
                                                     ).

          zcl_nf_writer=>zif_nf_writer~get_instance( )->get_monta_contabil(  IMPORTING e_contabil    = it_zfiwrt0011[] ).

*-------------------------------------------------------
* mensagens
*-------------------------------------------------------
          "107014 CS2023000203 Melhoria ZNFW0016/Transf ICMS PSA
          PERFORM f_monta_mensagem USING p_tipo wa_saida-icms.

          "PERFORM f_monta_mensagem.


          zcl_nf_writer=>zif_nf_writer~get_instance( )->set_monta_mensagens( EXPORTING i_mensagens   = it_zfiwrt0005[] ).

*-------------------------------------------------------
* gravar documento
*-------------------------------------------------------
          DATA: _nao_imposto(1) TYPE c."107014 CS2023000203 Melhoria ZNFW0016/Transf ICMS PSA
          IF p_tipo = 'S'. "wa_saida-icms. "107014 CS2023000203 Melhoria ZNFW0016/Transf ICMS PSA
            _nao_imposto = abap_true.
          ELSE.
*            _nao_imposto = abap_false.
            _nao_imposto = abap_true.  "Entrada tambem nao gera imposto
          ENDIF.


          zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento(    EXPORTING i_nao_imposto = _nao_imposto "107014 CS2023000203 Melhoria ZNFW0016/Transf ICMS PSA
                                                                                       i_nao_valida  = abap_true
                                                                                       i_nao_prepara = abap_true
                                                                             IMPORTING e_seq_lcto    = DATA(_seq_lcto_gerado) ).

          IF _seq_lcto_gerado IS NOT INITIAL.
            MESSAGE |Lançamento { _seq_lcto_gerado } gerado com sucesso!| TYPE 'S'.

            IF p_tipo = 'S'.
              wa_saida-seq_lanc_s  = _seq_lcto_gerado.

              UPDATE zfiwrt2004 SET seq_lanc_s   = _seq_lcto_gerado
                              WHERE cen_emissor  = wa_saida-cen_emissor
                                AND cen_receptor = wa_saida-cen_receptor
                                AND bukrs        = wa_saida-bukrs
                                AND mes          = wa_saida-mes
                                AND ano          = wa_saida-ano.
            ELSE.
              wa_saida-seq_lanc_e  = _seq_lcto_gerado.

              UPDATE zfiwrt2004 SET seq_lanc_e   = _seq_lcto_gerado
                              WHERE cen_emissor  = wa_saida-cen_emissor
                                AND cen_receptor = wa_saida-cen_receptor
                                AND bukrs        = wa_saida-bukrs
                                AND mes          = wa_saida-mes
                                AND ano          = wa_saida-ano.
            ENDIF.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = _seq_lcto_gerado.

            _lcto_gerados = abap_true.

          ELSE.
            MESSAGE |Houve um erro ao gravar o lançamento!| TYPE 'S'.
          ENDIF.

        CATCH zcx_nf_writer INTO DATA(zcx_nf_writer).
          zcx_nf_writer->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

    ENDLOOP.

*   PERFORM z_busca_dados_atualizado.
  ENDIF.

ENDFORM.

**************************************************************************
* monta mensagem
**************************************************************************
FORM f_monta_mensagem
            USING value_tipo TYPE Char1
                  value_icms TYPE j_1btaxval. "107014 CS2023000203 Melhoria ZNFW0016/Transf ICMS PSA

  CLEAR wa_zfiwrt0011.
  LOOP AT it_zfiwrt0011 INTO wa_zfiwrt0011.
    IF wa_zfiwrt0011-dmbtr IS NOT INITIAL.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF value_tipo = 'S' AND value_icms IS NOT INITIAL. "107014 CS2023000203 Melhoria ZNFW0016/Transf ICMS PSA
    wa_zfiwrt0011-dmbtr = value_icms.
  ENDIF.

  CHECK wa_zfiwrt0011-dmbtr IS NOT INITIAL.

  WRITE wa_zfiwrt0011-dmbtr TO l_valor_c.
  CONDENSE l_valor_c NO-GAPS.

  CALL FUNCTION 'IDWT_READ_MONTH_TEXT'
    EXPORTING
      langu = sy-langu
      month = l_mes
    IMPORTING
      t247  = l_t247.

  CALL FUNCTION 'SPELL_AMOUNT'
    EXPORTING
      amount    = wa_zfiwrt0011-dmbtr
      currency  = 'BRL'
      language  = sy-langu
    IMPORTING
      in_words  = l_words
    EXCEPTIONS
      not_found = 1
      too_large = 2
      OTHERS    = 3.

  l_data_c = l_t247-ltx && '/' && l_ano &&','.

  TRANSLATE l_data_c TO LOWER CASE.
  TRANSLATE l_words-word TO LOWER CASE.
  TRANSLATE l_words-decword TO LOWER CASE.

  IF l_words-decword = 'zero'.
    CONCATENATE 'Apurado em' l_data_c 'no valor de R$ ' l_valor_c '('
                l_words-word 'reais )'
           INTO l_mensagem SEPARATED BY space.
  ELSE.
    CONCATENATE 'Apurado em' l_data_c 'no valor de R$ ' l_valor_c '('
                l_words-word 'reais e' l_words-decword 'centavos )'
           INTO l_mensagem SEPARATED BY space.
  ENDIF.

  l_char1 = l_mensagem(72).
  l_char2 = l_mensagem+72(72).
  l_char3 = l_mensagem+144(72).

  FREE it_zfiwrt0005.
  wa_zfiwrt0005-mandt = sy-mandt.
  wa_zfiwrt0005-operacao = wa_zfiwrt0008-operacao.
  wa_zfiwrt0005-seqnum = 01.
  wa_zfiwrt0005-linnum = 01.
  wa_zfiwrt0005-message = l_char1.
  APPEND wa_zfiwrt0005            TO it_zfiwrt0005.

  IF l_char2 <> ''.
    wa_zfiwrt0005-mandt = sy-mandt.
    wa_zfiwrt0005-operacao = wa_zfiwrt0008-operacao.
    wa_zfiwrt0005-seqnum = 01.
    wa_zfiwrt0005-linnum = 02.
    wa_zfiwrt0005-message = l_char2.
    APPEND wa_zfiwrt0005            TO it_zfiwrt0005.
  ENDIF.

  IF l_char3 <> ''.
    wa_zfiwrt0005-mandt = sy-mandt.
    wa_zfiwrt0005-operacao = wa_zfiwrt0008-operacao.
    wa_zfiwrt0005-seqnum = 01.
    wa_zfiwrt0005-linnum = 03.
    wa_zfiwrt0005-message = l_char3.
    APPEND wa_zfiwrt0005            TO it_zfiwrt0005.
  ENDIF.

ENDFORM.

**************************************************************************
* estorno writer saida
**************************************************************************
FORM z_estorno_saida.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  DESCRIBE TABLE tg_selectedrow LINES DATA(l_lines).

  IF l_lines = 0.
    MESSAGE s024(sd) WITH 'Selecione pelo menos uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT tg_selectedrow  INTO wg_selectedrow.

    CLEAR: wa_saida,
           l_status,
           l_messa.

    READ TABLE it_saida        INTO wa_saida INDEX wg_selectedrow-index.

    IF wa_saida-seq_lanc_s IS INITIAL.
      CONTINUE.
    ENDIF.

*-----------------------
* valida fecha mes
*-----------------------
    SELECT SINGLE *
      FROM zfiwrt0008
      INTO @DATA(wa_0008)
     WHERE seq_lcto EQ @wa_saida-seq_lanc_s.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        i_bukrs  = wa_0008-bukrs
        i_data   = wa_0008-budat
*       i_dep_resp =
        i_user   = sy-uname
*       i_monat  =
      IMPORTING
        e_status = l_status
        e_messa  = l_messa
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF l_status = 'E'.
      CONCATENATE 'Erro na seq lcto ' wa_0008-seq_lcto l_messa INTO l_mensagem SEPARATED BY space.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH l_mensagem.
      CONTINUE.
    ENDIF.

    TRY.
        DATA(_doc_znfw_estornado) = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wa_saida-seq_lanc_s
                                                                                     i_wait_estorno = abap_true ).
      CATCH zcx_nf_writer INTO DATA(zcx_nf_writer).
        zcx_nf_writer->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.

    IF _doc_znfw_estornado = abap_true.
      wa_saida-seq_lanc_s  = abap_off.
      wa_saida-docnum_s    = abap_off.

      UPDATE zfiwrt2004 SET seq_lanc_s   = ''
                            docnum_s     = ''
                            n_nfe_s      = ''
                      WHERE cen_emissor  = wa_saida-cen_emissor
                        AND cen_receptor = wa_saida-cen_receptor
                        AND bukrs        = wa_saida-bukrs
                        AND mes          = wa_saida-mes
                        AND ano          = wa_saida-ano.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      MESSAGE s836(sd) WITH 'Não foi possível estornar NF saída.' DISPLAY LIKE 'E'.
    ENDIF.

  ENDLOOP.

ENDFORM.

**************************************************************************
* estorno writer entrada
**************************************************************************
FORM z_estorno_entrada.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  DESCRIBE TABLE tg_selectedrow LINES DATA(l_lines).

  IF l_lines = 0.
    MESSAGE s024(sd) WITH 'Selecione pelo menos uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT tg_selectedrow  INTO wg_selectedrow.

    CLEAR wa_saida.
    READ TABLE it_saida        INTO wa_saida INDEX wg_selectedrow-index.

    IF wa_saida-seq_lanc_e IS INITIAL.
      CONTINUE.
    ENDIF.

*-----------------------
* valida fecha mes
*-----------------------
    SELECT SINGLE *
      FROM zfiwrt0008
      INTO @DATA(wa_0008)
     WHERE seq_lcto EQ @wa_saida-seq_lanc_e.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        i_bukrs  = wa_0008-bukrs
        i_data   = wa_0008-budat
*       i_dep_resp =
        i_user   = sy-uname
*       i_monat  =
      IMPORTING
        e_status = l_status
        e_messa  = l_messa
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF l_status = 'E'.
      CONCATENATE 'Erro na seq lcto ' wa_0008-seq_lcto l_messa INTO l_mensagem SEPARATED BY space.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH l_mensagem.
      CONTINUE.
    ENDIF.

    TRY.
        DATA(_doc_znfw_estornado) = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wa_saida-seq_lanc_e
                                                                                     i_wait_estorno = abap_true ).
      CATCH zcx_nf_writer INTO DATA(zcx_nf_writer).
        zcx_nf_writer->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.

    IF _doc_znfw_estornado = abap_true.
      wa_saida-seq_lanc_e  = abap_off.
      wa_saida-docnum_e    = abap_off.

      UPDATE zfiwrt2004 SET seq_lanc_e   = ''
                            docnum_e     = ''
                      WHERE cen_emissor  = wa_saida-cen_emissor
                        AND cen_receptor = wa_saida-cen_receptor
                        AND bukrs        = wa_saida-bukrs
                        AND mes          = wa_saida-mes
                        AND ano          = wa_saida-ano.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ELSE.
      MESSAGE s836(sd) WITH 'Não foi possível estornar NF entrada.' DISPLAY LIKE 'E'.
    ENDIF.

  ENDLOOP.

ENDFORM.

**************************************************************************
* imprime danfe
**************************************************************************
FORM z_imprime_danfe.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  DESCRIBE TABLE tg_selectedrow LINES DATA(l_lines).

  IF l_lines <> 1.
    MESSAGE s024(sd) WITH 'Selecione uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE tg_selectedrow  INTO wg_selectedrow INDEX 1.
  READ TABLE it_saida        INTO wa_saida INDEX wg_selectedrow-index.

  CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
    EXPORTING
      doc_numero     = wa_saida-docnum_s
    EXCEPTIONS
      nao_localizado = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

**************************************************************************
* envia zefaz
**************************************************************************
FORM z_envia_sefaz .
  "
  DATA vnfnum_flag(20).

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'E'.
    EXIT.
  ENDIF.

  LOOP AT tg_selectedrow INTO wg_selectedrow.
    READ TABLE it_saida INTO DATA(wsaida) INDEX wg_selectedrow-index.
    DATA(tabix) = sy-tabix.
    "CLEAR wsaida-message.

    "vnfnum_flag = wsaida-nfenum.
    CONDENSE vnfnum_flag NO-GAPS.
    "
    SHIFT vnfnum_flag LEFT DELETING LEADING '0'.
    CONCATENATE  icon_activity vnfnum_flag INTO vnfnum_flag SEPARATED BY ' - '.

    UPDATE zfiwrt2004 SET msg_ret_sefaz = ''
                    WHERE cen_emissor   = wa_saida-cen_emissor
                      AND cen_receptor  = wa_saida-cen_receptor
                      AND bukrs         = wa_saida-bukrs
                      AND mes           = wa_saida-mes
                      AND ano           = wa_saida-ano.

    "MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING message.
    IF wsaida-seq_lanc_s IS NOT INITIAL
      AND wsaida-docnum_s IS NOT INITIAL
      AND ( wsaida-n_nfe_s IS INITIAL
       OR   wsaida-n_nfe_s = '@15@' ).

      TRY.
          zcl_nfe=>zif_doc_eletronico~get_instance(
          EXPORTING
            i_docnum = wsaida-docnum_s
          )->set_registro(
            EXPORTING
              i_docnum       = wsaida-docnum_s
              i_sem_bloqueio = abap_true
          )->set_autorizar(
          EXPORTING
            i_aguardar = abap_false

          )->get_ck_autorizado_uso(

          )->get_registro(
          IMPORTING
            e_documento = DATA(e_documento)
          )->set_clear( ).

        CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).
*          EX_DOC_ELETRONICO->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'S' ).
          msg_id   = ex_doc_eletronico->msgid.
          msg_no   = ex_doc_eletronico->msgno.
          msg_var1 = ex_doc_eletronico->msgv1.
          msg_var2 = ex_doc_eletronico->msgv2.
          msg_var3 = ex_doc_eletronico->msgv3.
          msg_var4 = ex_doc_eletronico->msgv4.
          CALL FUNCTION 'MESSAGE_PREPARE'
            EXPORTING
              language = 'P'
              msg_id   = msg_id
              msg_no   = msg_no
              msg_var1 = msg_var1
              msg_var2 = msg_var2
              msg_var3 = msg_var3
              msg_var4 = msg_var4
            IMPORTING
              msg_text = wmessage.

          UPDATE zfiwrt2004 SET msg_ret_sefaz = wmessage
                          WHERE cen_emissor   = wa_saida-cen_emissor
                            AND cen_receptor  = wa_saida-cen_receptor
                            AND bukrs         = wa_saida-bukrs
                            AND mes           = wa_saida-mes
                            AND ano           = wa_saida-ano.

          wsaida-msg_ret_sefaz = wmessage.
          MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING msg_ret_sefaz.
      ENDTRY.

      IF  e_documento-nfenum IS NOT INITIAL.
        wsaida-n_nfe_s = e_documento-nfenum.
        MODIFY it_saida FROM wsaida INDEX tabix.
      ELSE.
*        MESSAGE 'Ocorreu um erro ao gerar a NFE!' TYPE 'I'.
*        EXIT.
      ENDIF.
    ELSEIF  wsaida-docnum_s IS  INITIAL.
      wsaida-msg_ret_sefaz = 'Documento fiscal não gerado!'.
      MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING msg_ret_sefaz.
*   ELSEIF wsaida-docs_estornados = 'X'.
*     wsaida-message = 'Documento fiscal estornado!'.
*     MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING message.
*   ELSEIF  wsaida-doc_contab IS  INITIAL.
*     wsaida-message = 'Documento contabil não gerado!'.
*     MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING message.
*   ELSEIF vnfnum_flag =  wsaida-nfnum_flag.
*     wsaida-msg_ret_sefaz = 'Aguarde retorno SEFAZ'.
*     MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING msg_ret_sefaz.
    ELSEIF wsaida-n_nfe_s IS NOT INITIAL.
      wsaida-msg_ret_sefaz = 'Já enviado a SEFAZ'.
      MODIFY it_saida FROM wsaida INDEX tabix TRANSPORTING msg_ret_sefaz.
    ENDIF.

    CLEAR wsaida.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDLOOP.

*  PERFORM z_busca_dados_atualizado.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_DANFE
*&---------------------------------------------------------------------*
FORM z_busca_dados_atualizado.

  DATA: l_seq_lanc_e TYPE zfiwrt2004-seq_lanc_e,
        l_seq_lanc_s TYPE zfiwrt2004-seq_lanc_s,
        l_docnum_e   TYPE zfiwrt2004-docnum_e,
        l_docnum_s   TYPE zfiwrt2004-docnum_s,
        l_n_nfe_s    TYPE zfiwrt2004-n_nfe_s.

  LOOP AT it_saida INTO wa_saida.

    CLEAR: l_seq_lanc_e, l_seq_lanc_s, l_docnum_e,
           l_docnum_s,   l_n_nfe_s.

    l_tabix = sy-tabix.

    IF wa_saida-seq_lanc_s IS NOT INITIAL.
      SELECT SINGLE *
        FROM zfiwrt0008
        INTO @DATA(wa_t0008)
       WHERE seq_lcto EQ @wa_saida-seq_lanc_s.

      IF sy-subrc <> 0 OR wa_t0008-docs_estornados = abap_true.
        CLEAR l_seq_lanc_s.
      ELSE.
        l_seq_lanc_s = wa_t0008-seq_lcto.
      ENDIF.

      IF wa_t0008-docnum IS NOT INITIAL AND
         wa_t0008-docs_estornados = abap_false.
        SELECT SINGLE *
          FROM j_1bnfe_active
          INTO @DATA(wa_active)
         WHERE docnum EQ @wa_t0008-docnum.

        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM j_1bnfdoc
            INTO @DATA(wa_nfdoc)
            WHERE docnum EQ @wa_t0008-docnum.

          l_docnum_s = wa_nfdoc-docnum.
          l_n_nfe_s  = wa_nfdoc-nfenum.
        ENDIF.
      ENDIF.

      IF wa_saida-seq_lanc_s <> l_seq_lanc_s OR
         wa_saida-docnum_s   <> l_docnum_s   OR
         wa_saida-n_nfe_s    <> l_n_nfe_s.

        wa_saida-seq_lanc_s = l_seq_lanc_s.
        wa_saida-docnum_s   = l_docnum_s.
        wa_saida-n_nfe_s    = l_n_nfe_s.

        UPDATE zfiwrt2004 SET seq_lanc_s   = l_seq_lanc_s
                              docnum_s     = l_docnum_s
                              n_nfe_s      = l_n_nfe_s
                        WHERE cen_emissor  = wa_saida-cen_emissor
                          AND cen_receptor = wa_saida-cen_receptor
                          AND bukrs        = wa_saida-bukrs
                          AND mes          = wa_saida-mes
                          AND ano          = wa_saida-ano.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ELSE.
      wa_saida-seq_lanc_s = abap_false.
      wa_saida-docnum_s   = abap_false.
      wa_saida-n_nfe_s    = abap_false.

      UPDATE zfiwrt2004 SET seq_lanc_s   = abap_false
                            docnum_s     = abap_false
                            n_nfe_s      = abap_false
                      WHERE cen_emissor  = wa_saida-cen_emissor
                        AND cen_receptor = wa_saida-cen_receptor
                        AND bukrs        = wa_saida-bukrs
                        AND mes          = wa_saida-mes
                        AND ano          = wa_saida-ano.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

    IF wa_saida-seq_lanc_e IS NOT INITIAL.
      SELECT SINGLE *
        FROM zfiwrt0008
        INTO @wa_t0008
       WHERE seq_lcto EQ @wa_saida-seq_lanc_e.

      IF sy-subrc <> 0 OR wa_t0008-docs_estornados = abap_true.
        CLEAR l_seq_lanc_e.
      ELSE.
        l_seq_lanc_e = wa_t0008-seq_lcto.
      ENDIF.

      IF wa_t0008-docnum IS NOT INITIAL AND
         wa_t0008-docs_estornados = abap_false.
        SELECT SINGLE *
          FROM j_1bnfe_active
          INTO @wa_active
         WHERE docnum EQ @wa_t0008-docnum.

        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM j_1bnfdoc
            INTO @wa_nfdoc
            WHERE docnum EQ @wa_t0008-docnum.

          l_docnum_e = wa_nfdoc-docnum.
        ENDIF.
      ENDIF.

      IF wa_saida-seq_lanc_e <> l_seq_lanc_e OR
         wa_saida-docnum_e   <> l_docnum_e.

        wa_saida-seq_lanc_e = l_seq_lanc_e.
        wa_saida-docnum_e   = l_docnum_e.

        UPDATE zfiwrt2004 SET seq_lanc_e   = l_seq_lanc_e
                              docnum_e     = l_docnum_e
                        WHERE cen_emissor  = wa_saida-cen_emissor
                          AND cen_receptor = wa_saida-cen_receptor
                          AND bukrs        = wa_saida-bukrs
                          AND mes          = wa_saida-mes
                          AND ano          = wa_saida-ano.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDIF.
    ELSE.
      wa_saida-seq_lanc_e = abap_false.
      wa_saida-docnum_e   = abap_false.

      UPDATE zfiwrt2004 SET seq_lanc_e   = abap_false
                            docnum_e     = abap_false
                      WHERE cen_emissor  = wa_saida-cen_emissor
                        AND cen_receptor = wa_saida-cen_receptor
                        AND bukrs        = wa_saida-bukrs
                        AND mes          = wa_saida-mes
                        AND ano          = wa_saida-ano.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

    MODIFY it_saida      FROM wa_saida INDEX l_tabix.

  ENDLOOP.
ENDFORM.

**************************************************************************
* define origem/destino
**************************************************************************
FORM f_define_origem_destino USING "p_kna1  TYPE kna1
                                   p_lfa1  TYPE lfa1
                                   p_t001w TYPE t001w
                                   p_1baa  TYPE j_1baa
                                   wa_zfiwrt0001 TYPE zfiwrt0001
                          CHANGING p_indcoper
                                   p_texto_fiscal.

  IF wa_zfiwrt0001-parvw EQ 'AG'.
    "IF p_kna1-regio EQ p_t001w-regio.
    p_indcoper = 'D'.
    p_texto_fiscal = 'Dentro do Estado'.
    "ELSE.
    " p_indcoper = 'F'.
    " p_texto_fiscal = 'Fora do Estado'.
    "ENDIF.
    "IF p_1baa-direct EQ 1.
    "MOVE: p_kna1-regio TO wg_shipfrom.
    "ELSE.
    "MOVE: p_kna1-regio TO wg_shipto.
    "ENDIF.
  ELSEIF wa_zfiwrt0001-parvw EQ 'BR'
     OR  wa_zfiwrt0001-parvw EQ 'LF'.
    IF p_lfa1-regio EQ p_t001w-regio.
      p_indcoper = 'D'.
      p_texto_fiscal = 'Dentro do Estado'.
    ELSE.
      p_indcoper = 'F'.
      p_texto_fiscal = 'Fora do Estado'.
    ENDIF.

    IF p_1baa-direct EQ 1.
      MOVE: p_lfa1-regio TO wg_shipfrom.
    ELSE.
      MOVE: p_lfa1-regio TO wg_shipto.
    ENDIF.
  ENDIF.

  IF p_1baa-direct EQ 1.
    MOVE: p_t001w-regio TO wg_shipto.
  ELSE.
    MOVE: p_t001w-regio TO wg_shipfrom.
  ENDIF.

ENDFORM.
