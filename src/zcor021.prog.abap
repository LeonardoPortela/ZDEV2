*&------- --------------------------------------------------------------*
*& Report  ZCOR021
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zcor021.

TYPE-POOLS: icon.
TABLES: sscrfields.
TABLES: zsdt0225, zcot0011.


TYPES: BEGIN OF ty_saida,
         status(4)      TYPE c,
         id_seq         TYPE zsdt0225-id_seq,
         bukrs          TYPE zsdt0225-bukrs,
         werks          TYPE zsdt0225-werks,
         cl_codigo      TYPE kna1-kunnr,
         name1          TYPE kna1-name1,
         cod_material   TYPE zsdt0225-cod_material,
         maktx          TYPE makt-maktx,
         dt_fatura      TYPE zsdt0225-dt_fatura,
         peso_vinculado TYPE zsdt0225-peso_vinculado,
         waerk          TYPE zsdt0225-waerk,
         waerk_fatura   TYPE zsdt0225-waerk_fatura,
         nf_saida       TYPE j_1bnfdoc-nfenum,
         series         TYPE j_1bnfdoc-series,
         vlr_usd        TYPE zsdt0225-vlr_usd,
         vlr_brl        TYPE zsdt0225-vlr_brl,
         lote           TYPE zsdt0225-lote,
         doc_lcto       TYPE zsdt0225-doc_lcto,
         belnr          TYPE zsdt0225-belnr,
         doc_mr22       TYPE zsdt0225-doc_mr22,
         matnr_ov       TYPE zsdt0225-matnr_ov,
         budat          TYPE zfiwrt0008-budat,
         cofi_usd       TYPE zfiwrt0011-dmbe2,
         pis_usd        TYPE zfiwrt0011-dmbe2,
         icm_usd        TYPE zfiwrt0011-dmbe2,
         cofi_brl       TYPE zfiwrt0011-dmbtr,
         pis_brl        TYPE zfiwrt0011-dmbtr,
         icm_brl        TYPE zfiwrt0011-dmbtr,
         vlr_usd_liq    TYPE zsdt0225-vlr_usd,
         vlr_brl_liq    TYPE zsdt0225-vlr_brl,
         operacao       TYPE zsdt0225-operacao,
         docnum         TYPE zsdt0225-docnum,
       END OF ty_saida.

TYPES: BEGIN OF ty_saida_01,
         bukrs   TYPE zcot0011-bukrs,
         butxt   TYPE t001-butxt,
         werks   TYPE zcot0011-werks,
         name1   TYPE t001w-name1,
         matkl   TYPE mara-matkl, " Rubenilson Pereira - 31.07.25 #181597
         wgbez   TYPE t023t-wgbez60, " Rubenilson Pereira - 31.07.25 #181597
         kostl   TYPE zcot0011-kostl,
         mctxt   TYPE cskt-mctxt,
         celltab TYPE lvc_t_styl,
       END OF ty_saida_01.


TYPES: BEGIN OF ty_saida_02,
         werks_og TYPE zcot0012-werks_og,
         texto_og TYPE t001w-name1,
         matkl    TYPE zcot0012-matkl,
         wgbez60  TYPE t023t-wgbez60,
         werks_dt TYPE zcot0012-werks_dt,
         texto_dt TYPE t001w-name1,
         operacao TYPE zsdt0225-operacao,
         texto_op TYPE dd07v-ddtext,
         celltab  TYPE lvc_t_styl,
       END OF ty_saida_02.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
TYPES: BEGIN OF ty_saida_03,
         bukrs         TYPE zcot0019-bukrs,
         butxt         TYPE t001-butxt,
         matkl         TYPE zcot0019-matkl,
         wgbez         TYPE t023t-wgbez60,
         tp_lcto       TYPE zcot0019-tp_lcto,
         descricao     TYPE char50,
         usuario       TYPE sy-uname,
         data_registro TYPE zcot0019-data_registro,
         hora_registro TYPE zcot0019-hora_registro,
         celltab       TYPE lvc_t_styl,
       END OF ty_saida_03.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

TYPES: BEGIN OF ty_branch,
         bukrs  TYPE j_1bbranch-bukrs,
         branch TYPE zsdt0225-cl_codigo,
       END OF ty_branch.


DATA: it_saida     TYPE TABLE OF ty_saida,
      wa_saida     TYPE ty_saida,
      it_saida_01  TYPE TABLE OF ty_saida_01,
      wa_saida_01  TYPE ty_saida_01,
      it_saida_02  TYPE TABLE OF ty_saida_02,
      wa_saida_02  TYPE ty_saida_02,
*** Inicio - Rubenilson Pereira - 31.07.25 #181597
      it_saida_03  TYPE TABLE OF ty_saida_03,
      wa_saida_03  TYPE ty_saida_03,
      it_mara      TYPE TABLE OF mara,
      it_zcot0019  TYPE TABLE OF zcot0019,
*** Fim - Rubenilson Pereira - 31.07.25 #181597
      it_zsdt0225  TYPE TABLE OF zsdt0225,
      wa_zsdt0225  TYPE zsdt0225,
      it_zglt031   TYPE TABLE OF zglt031,
      wa_zglt031   TYPE zglt031,
      it_zglt032   TYPE TABLE OF zglt032,
      wa_zglt032   TYPE zglt032,
      wa_zglt035   TYPE zglt035,
      it_zglt036   TYPE TABLE OF zglt036,
      wa_zglt036   TYPE zglt036,
      it_j_1bnfdoc TYPE TABLE OF j_1bnfdoc,
      wa_j_1bnfdoc TYPE j_1bnfdoc,
      it_jbranch   TYPE TABLE OF ty_branch,
      wa_jbranch   TYPE ty_branch,
      it_zglt035   TYPE TABLE OF zglt035,
      wl_zglt035   TYPE zglt035,
      it_kna1      TYPE TABLE OF kna1,
      wa_kna1      TYPE kna1,
      it_makt      TYPE TABLE OF makt,
      wa_makt      TYPE makt.

DATA: t_tp_lcto TYPE RANGE OF zglt031-tp_lcto,

      w_tp_lcto LIKE LINE OF t_tp_lcto,
      lv_matnr  TYPE matnr18,
      lv_matnr2 TYPE matnr18.

DATA: lra_mat       TYPE RANGE OF matnr18.

DATA: g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      tl_function          TYPE ui_functions,
      wl_function          LIKE tl_function WITH HEADER LINE,
      it_fcat              TYPE TABLE OF lvc_s_fcat,
      wl_fcat              TYPE lvc_s_fcat,
      wa_layout            TYPE lvc_s_layo,
      ty_toolbar           TYPE stb_button,
      wa_stable            TYPE lvc_s_stbl VALUE 'X'.

DATA: g_grid_01               TYPE REF TO cl_gui_alv_grid,
      g_custom_container_01   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager_01 TYPE REF TO cl_alv_grid_toolbar_manager,
      tl_function_01          TYPE ui_functions,
      wl_function_01          LIKE tl_function WITH HEADER LINE,
      it_fcat_01              TYPE TABLE OF lvc_s_fcat,
      wl_fcat_01              TYPE lvc_s_fcat,
      wa_layout_01            TYPE lvc_s_layo,
      ty_toolbar_01           TYPE stb_button,
      it_estilo_01            TYPE lvc_t_styl,
      vdisvariant             TYPE disvariant.

DATA: g_grid_02               TYPE REF TO cl_gui_alv_grid,
      g_custom_container_02   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager_02 TYPE REF TO cl_alv_grid_toolbar_manager,
      tl_function_02          TYPE ui_functions,
      wl_function_02          LIKE tl_function WITH HEADER LINE,
      it_fcat_02              TYPE TABLE OF lvc_s_fcat,
      wl_fcat_02              TYPE lvc_s_fcat,
      wa_layout_02            TYPE lvc_s_layo,
      ty_toolbar_02           TYPE stb_button,
      it_estilo_02            TYPE lvc_t_styl,
      vdisvariant_02          TYPE disvariant.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
DATA: g_grid_03               TYPE REF TO cl_gui_alv_grid,
      g_custom_container_03   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager_03 TYPE REF TO cl_alv_grid_toolbar_manager,
      tl_function_03          TYPE ui_functions,
      wl_function_03          LIKE tl_function WITH HEADER LINE,
      it_fcat_03              TYPE TABLE OF lvc_s_fcat,
      wl_fcat_03              TYPE lvc_s_fcat,
      wa_layout_03            TYPE lvc_s_layo,
      ty_toolbar_03           TYPE stb_button,
      it_estilo_03            TYPE lvc_t_styl,
      vdisvariant_03          TYPE disvariant.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

DATA: tg_selectedrow    TYPE lvc_t_row,
      wg_selectedrow    TYPE lvc_s_row,
      tg_selectedrow_01 TYPE lvc_t_row,
      wg_selectedrow_01 TYPE lvc_s_row,
      tg_selectedrow_02 TYPE lvc_t_row,
      wg_selectedrow_02 TYPE lvc_s_row,
*** Inicio - Rubenilson Pereira - 31.07.25 #181597
      tg_selectedrow_03 TYPE lvc_t_row,
      wg_selectedrow_03 TYPE lvc_s_row,
*** Fim - Rubenilson Pereira - 31.07.25 #181597
      it_dta            TYPE STANDARD TABLE OF bdcdata WITH HEADER LINE,
      wa_dta            TYPE bdcdata,
      opt               TYPE ctu_params,
      v_werks           TYPE t001w-werks.


DATA: t_set  TYPE TABLE OF rgsb4,
      wa_set TYPE rgsb4.
DATA: t_bdcdata    TYPE bdcdata    OCCURS 0 WITH HEADER LINE,
      t_bdcmsgcoll TYPE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.

DATA: it_dd07v    TYPE TABLE OF dd07v,
      wa_dd07v    TYPE dd07v,
      vdomvalue_l TYPE  dd07v-domvalue_l.

CONSTANTS: gs_layout     TYPE lvc_s_layo VALUE abap_true.

INITIALIZATION.

  PERFORM criar_botao.

  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: p_bukrs FOR zsdt0225-bukrs,
                    p_dt_mv FOR zsdt0225-dt_fatura.
  SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN FUNCTION KEY 1.
  SELECTION-SCREEN FUNCTION KEY 2.
  SELECTION-SCREEN FUNCTION KEY 3." Rubenilson Pereira - 31.07.25 #181597

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM seleciona_dados_01.
      CALL SCREEN 0101.
    WHEN 'FC02'.
      PERFORM seleciona_dados_02.
      CALL SCREEN 0102.
*** Inicio - Rubenilson Pereira - 31.07.25 #181597
    WHEN 'FC03'.
      PERFORM seleciona_dados_03.
      CALL SCREEN 0103.
*** Fim - Rubenilson Pereira - 31.07.25 #181597
  ENDCASE.


START-OF-SELECTION.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      client        = sy-mandt
      setnr         = 'MAGGI_ZCOR021'
      class         = '0000'
    TABLES
      set_values    = t_set
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  CHECK t_set IS NOT INITIAL.


  CLEAR t_tp_lcto.
  LOOP AT t_set INTO wa_set.
    w_tp_lcto-sign    = 'I'.
    w_tp_lcto-option  = 'EQ'.
    w_tp_lcto-low     = wa_set-from.
    APPEND  w_tp_lcto TO t_tp_lcto.
    CLEAR: wa_set, w_tp_lcto.
  ENDLOOP.

  PERFORM seleciona_dados.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,

      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.
ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_data_changed.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
      WHERE fieldname EQ 'BUKRS' OR fieldname EQ 'WERKS' OR
            fieldname EQ 'KOSTL' OR fieldname EQ 'MATKL'." Rubenilson Pereira - 31.07.25 #181597

      LOOP AT it_saida_01 INTO wa_saida_01.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.
          WHEN 'BUKRS'.

            SELECT SINGLE * FROM t001 INTO @DATA(wa_t001)
              WHERE bukrs EQ @wa_good_cells-value.

            IF sy-subrc = 0.
              wa_saida_01-bukrs  = wa_t001-bukrs.
              wa_saida_01-butxt  = wa_t001-butxt.

              MODIFY it_saida_01 FROM wa_saida_01 INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE ' Empresa informada não existe!' TYPE 'I'.
              EXIT.
            ENDIF.

            " Inicio - Rubenilson Pereira - 31.07.25 #181597
          WHEN 'MATKL'.

            SELECT SINGLE * FROM t023t INTO @DATA(wa_t023t)
              WHERE matkl EQ @wa_good_cells-value
                AND spras EQ @sy-langu.

            IF sy-subrc = 0.
              wa_saida_01-matkl = wa_t023t-matkl.
              wa_saida_01-wgbez = wa_t023t-wgbez60.

              MODIFY it_saida_01 FROM wa_saida_01 INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE 'Grp. Material informado não existe!' TYPE 'I'.
              EXIT.
            ENDIF.
            " Fim - Rubenilson Pereira - 31.07.25 #181597
          WHEN 'WERKS'.

            SELECT SINGLE * FROM t001w INTO @DATA(wa_t001w)
              WHERE werks EQ @wa_good_cells-value.

            IF sy-subrc = 0.
              wa_saida_01-werks = wa_t001w-werks.
              wa_saida_01-name1 = wa_t001w-name1.

              MODIFY it_saida_01 FROM wa_saida_01 INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE 'Filial informada não existe!' TYPE 'I'.
              EXIT.
            ENDIF.

          WHEN 'KOSTL'.

            SELECT SINGLE * FROM cskt INTO @DATA(wa_cskt)
              WHERE spras EQ @sy-langu
              AND   kostl EQ @wa_good_cells-value
              AND   datbi EQ '99991231'.

            IF sy-subrc = 0.
              wa_saida_01-kostl =  wa_cskt-kostl.
              wa_saida_01-mctxt =  wa_cskt-mctxt.

              MODIFY it_saida_01 FROM wa_saida_01 INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE 'Centro de custo informado não existe !' TYPE 'I'.
              EXIT.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    CALL METHOD g_grid_01->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.

  METHOD on_data_changed_finished.
  ENDMETHOD.
ENDCLASS.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
CLASS lcl_event_handler_03 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

ENDCLASS.

CLASS lcl_event_handler_03 IMPLEMENTATION.
  METHOD on_data_changed.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
      WHERE fieldname EQ 'BUKRS' OR fieldname EQ 'MATKL' OR
            fieldname EQ 'TP_LCTO'.

      LOOP AT it_saida_03 INTO wa_saida_03.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.
          WHEN 'BUKRS'.

            SELECT SINGLE * FROM t001 INTO @DATA(wa_t001)
              WHERE bukrs EQ @wa_good_cells-value.

            IF sy-subrc = 0.
              wa_saida_03-bukrs  = wa_t001-bukrs.
              wa_saida_03-butxt  = wa_t001-butxt.

              MODIFY it_saida_03 FROM wa_saida_03 INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE ' Empresa informada não existe!' TYPE 'I'.
              EXIT.
            ENDIF.

          WHEN 'MATKL'.

            SELECT SINGLE * FROM t023t INTO @DATA(wa_t023t)
              WHERE matkl EQ @wa_good_cells-value
                AND spras EQ @sy-langu.

            IF sy-subrc = 0.
              wa_saida_03-matkl = wa_t023t-matkl.
              wa_saida_03-wgbez = wa_t023t-wgbez60.

              MODIFY it_saida_03 FROM wa_saida_03 INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE 'Grp. Material informado não existe!' TYPE 'I'.
              EXIT.
            ENDIF.

          WHEN 'TP_LCTO'.

            SELECT SINGLE * FROM zglt031 INTO @DATA(wa_031)
              WHERE tp_lcto = @wa_good_cells-value.

            IF sy-subrc = 0.
              wa_saida_03-tp_lcto =  wa_031-tp_lcto.
              wa_saida_03-descricao =  wa_031-descricao.

              MODIFY it_saida_03 FROM wa_saida_03 INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE 'Tp. Lançamento informado não existe !' TYPE 'I'.
              EXIT.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    CALL METHOD g_grid_03->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.

ENDCLASS.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

CLASS lcl_hotspot_click DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id e_row_id es_row_no sender.
ENDCLASS.

CLASS lcl_hotspot_click IMPLEMENTATION.
  METHOD on_hotspot_click.
    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
    IF sy-subrc = 0.
      CASE e_column_id.
        WHEN 'DOC_LCTO'.
          CHECK wa_saida-doc_lcto IS NOT INITIAL.

          FREE it_dta.
          DEFINE shdb.
            wa_dta-program     = &1.
            wa_dta-dynpro      = &2.
            wa_dta-dynbegin    = &3.
            wa_dta-fnam        = &4.
            wa_dta-fval        = &5.
            APPEND wa_dta TO it_dta.
          END-OF-DEFINITION.

          shdb:
          'ZGL015'   '0050'   'X'    ' '                   ' ',
          ' '        ' '      ' '    'BDC_CURSOR'          'WG_ZGLT034-LOTE',
          ' '        ' '      ' '    'BDC_OKCODE'          '=DISPLA',
          'ZGL015'   '0050'   'X'    ' '                   ' ',
          ' '        ' '      ' '    'BDC_CURSOR'          'WG_ZGLT035-DOC_LCTO',
          ' '        ' '      ' '    'BDC_OKCODE'          '=SEARCH',
          ' '        ' '      ' '    'WG_ZGLT035-DOC_LCTO' wa_saida-doc_lcto.

          opt-dismode = 'E'.
          CALL TRANSACTION 'ZGL016' USING it_dta OPTIONS FROM opt.
      ENDCASE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM seleciona_dados.

  DATA: vl_total_usd TYPE zsdt0225-vlr_usd,
        vl_total_brl TYPE zsdt0225-vlr_brl.

  REFRESH it_saida.

  IF p_bukrs IS INITIAL.
    MESSAGE 'Favor informar o código da empresa! ' TYPE 'S'.
    EXIT.
  ELSE.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
*    SELECT *
*      FROM zglt031 INTO TABLE it_zglt031
*     WHERE tp_lcto IN t_tp_lcto.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

    SELECT  bukrs
            branch
       FROM j_1bbranch INTO TABLE it_jbranch
      WHERE bukrs IN p_bukrs.

    LOOP AT it_jbranch ASSIGNING FIELD-SYMBOL(<wa_jbranch>).
      <wa_jbranch>-branch =  |{ <wa_jbranch>-branch ALPHA = IN }|.
    ENDLOOP.


    SELECT *
      FROM zsdt0225 INTO TABLE it_zsdt0225
      FOR ALL ENTRIES IN it_jbranch
      WHERE cl_codigo       EQ it_jbranch-branch
      AND   dt_fatura       IN p_dt_mv
      AND   doc_znfw        NE ' '
      AND   docnum_entrada  NE ' '
      AND   ( auart         NE 'ZELI' " Rubenilson Pereira - 31.07.25 #181597
      AND     auart         NE 'ZELE'   ) ." Rubenilson Pereira - 31.07.25 #181597

    CHECK it_zsdt0225 IS NOT INITIAL.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
    SELECT *
      FROM mara
      INTO TABLE it_mara
      FOR ALL ENTRIES IN it_zsdt0225
      WHERE matnr = it_zsdt0225-cod_material.
    IF sy-subrc IS INITIAL.

      SORT it_mara BY matnr.

      SELECT *
        FROM zcot0019
        INTO TABLE it_zcot0019
        FOR ALL ENTRIES IN it_mara
        WHERE matkl = it_mara-matkl.
      IF sy-subrc IS INITIAL.
        SORT it_zcot0019 BY bukrs matkl.
      ENDIF.
    ENDIF.


    SELECT *
      FROM zsdt0306_fat
      INTO TABLE @DATA(lt_fat)
      FOR ALL ENTRIES IN @it_zsdt0225
      WHERE id_seq = @it_zsdt0225-id_seq
        AND dt_recreg = @it_zsdt0225-dt_recreg.
    IF sy-subrc IS INITIAL.
      SORT lt_fat BY id_seq dt_recreg.
    ENDIF.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

    SELECT *
      FROM j_1bnfdoc INTO TABLE it_j_1bnfdoc
        FOR ALL ENTRIES IN it_zsdt0225
      WHERE docnum EQ it_zsdt0225-docnum
        AND cancel EQ ' '.

    SELECT *
      FROM kna1 INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_zsdt0225
     WHERE kunnr EQ it_zsdt0225-cl_codigo.


    SELECT *
      FROM makt INTO TABLE it_makt
     FOR ALL ENTRIES IN it_zsdt0225
    WHERE matnr EQ it_zsdt0225-cod_material
      AND spras EQ sy-langu.

    LOOP AT it_zsdt0225 INTO DATA(wa_mat).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_mat-cod_material
        IMPORTING
          output = lv_matnr.

      lv_matnr2 = lv_matnr.
      lv_matnr2 = |{ lv_matnr2 ALPHA = IN }|.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_matnr2 ) TO lra_mat.
    ENDLOOP.

    SELECT *
   FROM makt INTO TABLE @DATA(it_makt2)
 WHERE matnr IN @lra_mat
   AND spras EQ @sy-langu.

    SELECT *
      FROM zfiwrt0008 INTO TABLE @DATA(it_0008)
      FOR ALL ENTRIES IN @it_zsdt0225
     WHERE seq_lcto EQ @it_zsdt0225-doc_znfw.

    SELECT *
      FROM  zfiwrt0011 INTO TABLE @DATA(it_0011)
      FOR ALL ENTRIES IN @it_zsdt0225
      WHERE seq_lcto EQ @it_zsdt0225-doc_znfw
      AND   estorno  NE 'X'
      AND   taxtyp   IN ('ICOF','IPIS','ICM3')
      AND   bschl    EQ '40'.


    LOOP AT it_zsdt0225 INTO wa_zsdt0225.

      READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_zsdt0225-docnum.
      IF sy-subrc = 0.
        wa_saida-nf_saida = wa_j_1bnfdoc-nfnum.
        wa_saida-series   = wa_j_1bnfdoc-series.
      ENDIF.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_zsdt0225-cl_codigo.
      IF sy-subrc = 0.
        wa_saida-name1    =  wa_kna1-name1.
      ENDIF.


      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_zsdt0225-cod_material.
      IF sy-subrc = 0.
        wa_saida-maktx   =  wa_makt-maktx.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_mat-cod_material
          IMPORTING
            output = lv_matnr.

        lv_matnr2 = lv_matnr.
        lv_matnr2 = |{ lv_matnr2 ALPHA = IN }|.
        READ TABLE it_makt2 INTO wa_makt WITH KEY matnr = lv_matnr2.
        IF sy-subrc = 0.
          wa_saida-maktx   =  wa_makt-maktx.
        ENDIF.

      ENDIF.

      READ TABLE it_0008 INTO DATA(wa_0008) WITH KEY seq_lcto = wa_zsdt0225-doc_znfw.
      IF sy-subrc = 0.
        wa_saida-budat = wa_0008-budat.
      ENDIF.

      READ TABLE it_jbranch  INTO wa_jbranch WITH KEY branch = wa_zsdt0225-cl_codigo.


      wa_saida-id_seq           =  wa_zsdt0225-id_seq.
      wa_saida-bukrs            =  wa_jbranch-bukrs.
      wa_saida-werks            =  wa_zsdt0225-cl_codigo+6(4).
      wa_saida-cl_codigo        =  | { wa_zsdt0225-cl_codigo ALPHA = OUT } |.
      wa_saida-cod_material     =  wa_zsdt0225-cod_material.
      wa_saida-matnr_ov         =  wa_zsdt0225-matnr_ov.
      wa_saida-dt_fatura        =  wa_zsdt0225-dt_fatura.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
      wa_saida-docnum           =  wa_zsdt0225-docnum.

      IF wa_zsdt0225-peso_vinculado <= 0.
        READ TABLE lt_fat ASSIGNING FIELD-SYMBOL(<fs_fat>)
        WITH KEY id_seq = wa_zsdt0225-id_seq
                 dt_recreg = wa_zsdt0225-dt_recreg
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-peso_vinculado   =  <fs_fat>-qtdfatur.
        ENDIF.
      ELSE.
*** Fim - Rubenilson Pereira - 31.07.25 #181597
        wa_saida-peso_vinculado   =  wa_zsdt0225-peso_vinculado.
      ENDIF.

      wa_saida-waerk            =  wa_zsdt0225-waerk.
      wa_saida-waerk_fatura     =  wa_zsdt0225-waerk_fatura.
      wa_saida-vlr_usd          =  wa_zsdt0225-vlr_usd.
      wa_saida-vlr_brl          =  wa_zsdt0225-vlr_brl.
      wa_saida-lote             =  wa_zsdt0225-lote.
      wa_saida-doc_lcto         =  wa_zsdt0225-doc_lcto.
      wa_saida-belnr            =  wa_zsdt0225-belnr.
      wa_saida-doc_mr22         =  wa_zsdt0225-doc_mr22.
      wa_saida-operacao         =  wa_zsdt0225-operacao.

      IF wa_zsdt0225-lote  IS INITIAL AND  wa_zsdt0225-doc_lcto IS INITIAL AND
         wa_zsdt0225-belnr IS INITIAL AND  wa_zsdt0225-doc_mr22 IS INITIAL.

        wa_saida-status = '@0A@'.

      ELSEIF ( ( wa_zsdt0225-lote  IS NOT INITIAL   AND  wa_zsdt0225-doc_lcto IS NOT INITIAL AND
                 wa_zsdt0225-belnr IS INITIAL       AND  wa_zsdt0225-doc_mr22 IS INITIAL ) OR
              (  wa_zsdt0225-lote  IS NOT INITIAL   AND  wa_zsdt0225-doc_lcto IS NOT INITIAL AND
                 wa_zsdt0225-belnr IS NOT INITIAL   AND  wa_zsdt0225-doc_mr22 IS INITIAL ) ).

        wa_saida-status = '@09@'.
      ELSE.
        wa_saida-status = '@08@'.
      ENDIF.


      LOOP AT it_0011 INTO DATA(wa_0011)
        WHERE  seq_lcto = wa_zsdt0225-doc_znfw.

        CASE wa_0011-taxtyp.
          WHEN 'ICOF'.
            wa_saida-cofi_usd  = wa_0011-dmbe2.
            wa_saida-cofi_brl  = wa_0011-dmbtr.
          WHEN 'IPIS'.
            wa_saida-pis_usd  = wa_0011-dmbe2.
            wa_saida-pis_brl  = wa_0011-dmbtr.
          WHEN 'ICM3'.
            wa_saida-icm_usd  =  wa_0011-dmbe2.
            wa_saida-icm_brl  =  wa_0011-dmbtr.
        ENDCASE.

        vl_total_brl = ( wa_zsdt0225-vlr_brl - ( wa_saida-cofi_brl + wa_saida-pis_brl + wa_saida-icm_brl  ) ).
        vl_total_usd = ( wa_zsdt0225-vlr_usd - ( wa_saida-cofi_usd + wa_saida-pis_usd + wa_saida-icm_usd  ) ).

        wa_saida-vlr_brl_liq  = vl_total_brl.
        wa_saida-vlr_usd_liq  = vl_total_usd.

      ENDLOOP.

      APPEND wa_saida TO it_saida.
      CLEAR: wa_saida, wa_zsdt0225, wa_kna1, wa_kna1,    vl_total_brl,  vl_total_usd.

    ENDLOOP.

    CALL SCREEN 0100.

  ENDIF.

ENDFORM.

FORM seleciona_dados_01.

  REFRESH: it_saida_01.

  SELECT *
    FROM zcot0011 INTO TABLE @DATA(it_zcot0011).

  IF it_zcot0011[] IS NOT INITIAL.

    SELECT *
      FROM t001 INTO TABLE @DATA(it_t001)
     FOR ALL ENTRIES IN @it_zcot0011
     WHERE bukrs EQ @it_zcot0011-bukrs.

    SELECT *
      FROM t001w INTO TABLE @DATA(it_t001w)
      FOR ALL ENTRIES IN @it_zcot0011
      WHERE werks EQ @it_zcot0011-werks.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
    SELECT *
      FROM t023t INTO TABLE @DATA(it_t023t)
      FOR ALL ENTRIES IN @it_zcot0011
      WHERE matkl EQ @it_zcot0011-matkl
        AND spras EQ @sy-langu.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

    SELECT *
      FROM cskt INTO TABLE @DATA(it_cskt)
     FOR ALL ENTRIES IN @it_zcot0011
      WHERE spras EQ @sy-langu
      AND   kostl EQ @it_zcot0011-kostl
      AND   datbi EQ '99991231'.

    LOOP AT it_zcot0011 INTO DATA(wa_zcot0011).

      wa_saida_01-bukrs = wa_zcot0011-bukrs.
      wa_saida_01-werks = wa_zcot0011-werks.
      wa_saida_01-kostl = wa_zcot0011-kostl.
      wa_saida_01-matkl = wa_zcot0011-matkl." Rubenilson Pereira - 31.07.25 #181597

      READ TABLE it_t001 INTO DATA(wa_t001) WITH KEY bukrs = wa_zcot0011-bukrs.
      IF sy-subrc = 0.
        wa_saida_01-butxt = wa_t001-butxt.
      ENDIF.

      READ TABLE it_t001w INTO DATA(wa_t001w) WITH KEY werks = wa_zcot0011-werks.
      IF sy-subrc = 0.
        wa_saida_01-name1 = wa_t001w-name1.
      ENDIF.
*** Inicio - Rubenilson Pereira - 31.07.25 #181597
      READ TABLE it_t023t INTO DATA(wa_t023t) WITH KEY matkl = wa_zcot0011-matkl.
      IF sy-subrc = 0.
        wa_saida_01-wgbez = wa_t023t-wgbez60.
      ENDIF.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

      READ TABLE it_cskt INTO DATA(wa_cskt) WITH KEY kostl = wa_zcot0011-kostl.
      IF sy-subrc = 0.
        wa_saida_01-mctxt = wa_cskt-mctxt.
      ENDIF.


      FREE wa_saida_01-celltab.
      it_estilo_01 =  VALUE #( ( fieldname = 'BUKRS'      style = cl_gui_alv_grid=>mc_style_disabled  )
                               ( fieldname = 'WERKS'      style = cl_gui_alv_grid=>mc_style_disabled  )
                               ( fieldname = 'MATKL'      style = cl_gui_alv_grid=>mc_style_disabled  )
                               ( fieldname = 'KOSTL'      style = cl_gui_alv_grid=>mc_style_disabled  ) ).
      INSERT LINES OF it_estilo_01 INTO TABLE wa_saida_01-celltab.

      APPEND wa_saida_01 TO it_saida_01.
      CLEAR: wa_saida_01, wa_zcot0011.
    ENDLOOP.
  ENDIF.


ENDFORM.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
FORM seleciona_dados_03.
  DATA: lt_031 TYPE TABLE OF  zglt031.

  REFRESH: it_saida_03.

  SELECT *
    FROM zcot0019 INTO TABLE @DATA(it_zcot0019).

  IF it_zcot0019[] IS NOT INITIAL.

    SELECT *
      FROM t001 INTO TABLE @DATA(it_t001)
     FOR ALL ENTRIES IN @it_zcot0019
     WHERE bukrs EQ @it_zcot0019-bukrs.

    SELECT *
      FROM t023t INTO TABLE @DATA(it_t023t)
      FOR ALL ENTRIES IN @it_zcot0019
      WHERE matkl EQ @it_zcot0019-matkl
        AND spras EQ @sy-langu.

    SELECT *
      FROM zglt031
      INTO TABLE lt_031
      FOR ALL ENTRIES IN it_zcot0019
      WHERE tp_lcto EQ it_zcot0019-tp_lcto.

    LOOP AT it_zcot0019 INTO DATA(wa_zcot0019).

      wa_saida_03-bukrs = wa_zcot0019-bukrs.
      wa_saida_03-matkl = wa_zcot0019-matkl.
      wa_saida_03-tp_lcto = wa_zcot0019-tp_lcto.

      READ TABLE it_t001 INTO DATA(wa_t001) WITH KEY bukrs = wa_zcot0019-bukrs.
      IF sy-subrc = 0.
        wa_saida_03-butxt = wa_t001-butxt.
      ENDIF.

      READ TABLE it_t023t INTO DATA(wa_t023t) WITH KEY matkl = wa_zcot0019-matkl.
      IF sy-subrc = 0.
        wa_saida_03-wgbez = wa_t023t-wgbez60.
      ENDIF.

      READ TABLE lt_031 INTO DATA(wa_031) WITH KEY tp_lcto = wa_zcot0019-tp_lcto.
      IF sy-subrc = 0.
        wa_saida_03-descricao = wa_031-descricao.
      ENDIF.

      wa_saida_03-usuario = wa_zcot0019-usuario.
      wa_saida_03-data_registro = wa_zcot0019-data_registro.
      wa_saida_03-hora_registro = wa_zcot0019-hora_registro.

      FREE wa_saida_03-celltab.
      it_estilo_03 =  VALUE #( ( fieldname = 'BUKRS'      style = cl_gui_alv_grid=>mc_style_disabled  )
                               ( fieldname = 'MATKL'      style = cl_gui_alv_grid=>mc_style_disabled  )
                               ( fieldname = 'TP_LCTO'    style = cl_gui_alv_grid=>mc_style_disabled  ) ).
      INSERT LINES OF it_estilo_03 INTO TABLE wa_saida_03-celltab.

      APPEND wa_saida_03 TO it_saida_03.
      CLEAR: wa_saida_03, wa_zcot0019.
    ENDLOOP.
  ENDIF.


ENDFORM.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

FORM alv.
  CLEAR wl_fcat.
  REFRESH it_fcat[].

  PERFORM preenche_cat USING :
        'STATUS'              'Status'            '07'    ''    ''     ''     ''    ''    'X',
        'BUKRS'               'Empresa'           '07'    ''    ''     ''     ''    ''    '',
        'WERKS'               'Filial'            '05'    ''    ''     ''     ''    ''    '',
        'CL_CODIGO'           'Cliente'           '08'    ''    ''     ''     ''    ''    '',
        'NAME1'               'Descrição'         '25'    ''    ''     ''     ''    ''    '',
        'COD_MATERIAL'        'Material'          '10'    'X'   ''     ''     ''    ''    '',
        'MAKTX'               'Descrição'         '25'    ''    ''     ''     ''    ''    '',
        'DT_FATURA'           'Dt.Fatura'         '13'    ''    ''     ''     ''    ''    '',
        'OPERACAO'            'OP'                '10'    ''    ''     ''     ''    ''    '',
        'PESO_VINCULADO'      'Peso Vinc.'        '13'    ''    ''     ''     ''    ''    '',
        'VLR_USD'             'Valor USD'         '10'    ''    ''     ''     ''    ''    '',
        'COFI_USD'            'Cofins USD'        '08'    ''    ''     ''     ''    ''    '',
        'PIS_USD'             'PIS USD'           '08'    ''    ''     ''     ''    ''    '',
        'ICM_USD'             'ICMS USD'          '08'    ''    ''     ''     ''    ''    '',
        'VLR_USD_LIQ'         'Valor USD Liq.'    '12'    ''    ''     ''     ''    ''    '',
        'VLR_BRL'             'Valor BRL'         '08'    ''    ''     ''     ''    ''    '',
        'COFI_BRL'            'Cofins BRL'        '08'    ''    ''     ''     ''    ''    '',
        'PIS_BRL'             'PIS BRL'           '08'    ''    ''     ''     ''    ''    '',
        'ICM_BRL'             'ICMS BRL'          '08'    ''    ''     ''     ''    ''    '',
        'VLR_BRL_LIQ'         'Valor BRL Liq.'    '12'    ''    ''     ''     ''    ''    '',
        'NF_SAIDA'            'NFPS'              '10'    ''    ''     ''     ''    ''    '',
        'SERIES'              'Series'            '05'    ''    ''     ''     ''    ''    '',
        'BUDAT'               'Data Lcto Entrada' '16'    ''    ''     ''     ''    ''    '',
        'LOTE'                'Lote'              '07'    ''    ''     ''     ''    ''    '',
        'DOC_LCTO'            'Doc ZGL016'        '12'    ''    'X'    ''     ''    ''    '',
        'BELNR'               'Apropr.Contabil'   '12'    ''    ''     ''     ''    ''    '',
        'DOC_MR22'            'Aprop MR22'        '12'    ''    ''     ''     ''    ''    ''.

ENDFORM.

FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_cor)
                        VALUE(p_icon).

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.
  wl_fcat-just      = p_just.
  wl_fcat-emphasize = p_cor.
  wl_fcat-icon      = p_icon.

  APPEND wl_fcat TO  it_fcat.

ENDFORM.

FORM alv_01.

  CLEAR wl_fcat_01.
  REFRESH it_fcat_01[].

  PERFORM preenche_cat_01 USING :
        'BUKRS'           'Empresa'            '07'    ''    ''     ''     ''    ''    'X'     'T001'     'BUKRS',
        'BUTXT'           'Descrição'          '25'    ''    ''     ''     ''    ''    ''      ''         '',
        'WERKS'           'Centro'             '07'    ''    ''     ''     ''    ''    'X'     'T001W'    'WERKS',
        'NAME1'           'Descrição'          '25'    ''    ''     ''     ''    ''    ''      ''         '',
        'MATKL'           'Grp. Material'      '12'    ''    ''     ''     ''    ''    'X'     'MARA'     'MATKL'," Rubenilson Pereira - 31.07.25 #181597
        'WGBEZ'           'Descrição'          '25'    ''    ''     ''     ''    ''    ''      ''         ''," Rubenilson Pereira - 31.07.25 #181597
        'KOSTL'           'Centro de custo'    '12'    ''    ''     ''     ''    ''    'X'     'CSKS'     'KOSTL',
        'MCTXT'           'Descrição'          '25'    ''    ''     ''     ''    ''    ''      ''         ''.
ENDFORM.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
FORM alv_03.

  CLEAR wl_fcat_03.
  REFRESH it_fcat_03[].

  PERFORM preenche_cat_03 USING :
        'BUKRS'           'Empresa'            '07'    ''    ''     ''     ''    ''    'X'     'T001'     'BUKRS',
        'BUTXT'           'Descrição'          '25'    ''    ''     ''     ''    ''    ''      ''         '',
        'MATKL'           'Grp. Material'      '12'    ''    ''     ''     ''    ''    'X'     'MARA'     'MATKL',
        'WGBEZ'           'Descrição'          '25'    ''    ''     ''     ''    ''    ''      ''         '',
        'TP_LCTO'         'Tp. Lcto'           '07'    ''    ''     ''     ''    ''    'X'     'ZGLT031' 'TP_LCTO',
        'DESCRICAO'       'Descrição'          '60'    ''    ''     ''     ''    ''    ''      ''         '',
        'USUARIO'         'Usuário'            '15'    ''    ''     ''     ''    ''    ''      ''         '',
        'DATA_REGISTRO'   'Data Registro'      '10'    ''    ''     ''     ''    ''    ''      ''         '',
        'HORA_REGISTRO'   'Hora Registro'      '10'    ''    ''     ''     ''    ''    ''      ''         ''.
ENDFORM.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

FORM preenche_cat_01 USING  VALUE(p_campo)
                            VALUE(p_desc)
                            VALUE(p_tam)
                            VALUE(p_zero)
                            VALUE(p_hot)
                            VALUE(p_sum)
                            VALUE(p_just)
                            VALUE(p_cor)
                            VALUE(p_edit)
                            VALUE(p_table)
                            VALUE(p_field).

  wl_fcat_01-fieldname = p_campo.
  wl_fcat_01-scrtext_l = p_desc.
  wl_fcat_01-scrtext_m = p_desc.
  wl_fcat_01-scrtext_s = p_desc.
  wl_fcat_01-outputlen = p_tam.
  wl_fcat_01-hotspot   = p_hot.
  wl_fcat_01-no_zero   = p_zero.
  wl_fcat_01-do_sum    = p_sum.
  wl_fcat_01-just      = p_just.
  wl_fcat_01-emphasize = p_cor.
  wl_fcat_01-edit      = p_edit.
  wl_fcat_01-ref_table = p_table.
  wl_fcat_01-ref_field = p_field.

  APPEND wl_fcat_01 TO  it_fcat_01.

ENDFORM.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
FORM preenche_cat_03 USING  VALUE(p_campo)
                            VALUE(p_desc)
                            VALUE(p_tam)
                            VALUE(p_zero)
                            VALUE(p_hot)
                            VALUE(p_sum)
                            VALUE(p_just)
                            VALUE(p_cor)
                            VALUE(p_edit)
                            VALUE(p_table)
                            VALUE(p_field).

  wl_fcat_03-fieldname = p_campo.
  wl_fcat_03-scrtext_l = p_desc.
  wl_fcat_03-scrtext_m = p_desc.
  wl_fcat_03-scrtext_s = p_desc.
  wl_fcat_03-outputlen = p_tam.
  wl_fcat_03-hotspot   = p_hot.
  wl_fcat_03-no_zero   = p_zero.
  wl_fcat_03-do_sum    = p_sum.
  wl_fcat_03-just      = p_just.
  wl_fcat_03-emphasize = p_cor.
  wl_fcat_03-edit      = p_edit.
  wl_fcat_03-ref_table = p_table.
  wl_fcat_03-ref_field = p_field.

  APPEND wl_fcat_03 TO  it_fcat_03.

ENDFORM.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  PERFORM alv.

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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.


    SET HANDLER : lcl_hotspot_click=>on_hotspot_click FOR g_grid.


    vdisvariant-report   = sy-repid.
    vdisvariant-username = sy-uname.


    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
        it_toolbar_excluding          = tl_function
        is_variant                    = vdisvariant
        i_save                        = 'A'
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = it_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
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
    WHEN 'GERAR'.
      PERFORM gerar_aprop.
    WHEN 'REFRESH'.
      PERFORM atualiza_doc.
      PERFORM seleciona_dados.

      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN 'GERA_MR22'.
      PERFORM gera_mr22.
    WHEN 'EST_MR22'.
      PERFORM estorno_mr22.
    WHEN 'EST_DOCS'.
      PERFORM estorno_docs.
  ENDCASE.
ENDMODULE.

FORM gerar_aprop.

  DATA: dp_resp    TYPE char2,
        descricao  TYPE zdescr_lote,
        lote       TYPE zglt035-lote,
        v_liberado TYPE char01,
        lv_subseq  TYPE zglt036-seqsub.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE  'Favor selecione uma linha! ' TYPE 'S'.
    EXIT.
  ELSE.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = 'Gerando Lançamentos '.

    LOOP AT tg_selectedrow INTO wg_selectedrow.

      READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
*      READ TABLE it_zglt031 INTO wa_zglt031  INDEX 1.

      READ TABLE it_mara ASSIGNING FIELD-SYMBOL(<fs_mara>)
      WITH KEY matnr = wa_saida-cod_material
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE it_zcot0019 INTO DATA(wa_zcot0019)
        WITH KEY bukrs = wa_saida-bukrs
                 matkl = <fs_mara>-matkl
        BINARY SEARCH.

*** Fim - Rubenilson Pereira - 31.07.25 #181597

        SELECT *
          FROM zglt032 INTO TABLE it_zglt032
         WHERE tp_lcto EQ  wa_zcot0019-tp_lcto." Rubenilson Pereira - 31.07.25 #181597

        IF it_zglt032 IS NOT INITIAL.

          MOVE wa_zglt031-descricao TO descricao.
          MOVE wa_zglt031-dpto_resp TO dp_resp.

          CALL METHOD zcl_gerar_lote=>create_lote
            EXPORTING
              i_bukrs      = wa_saida-bukrs
              i_descr_lote = 'Apropriação '
              i_dep_resp   = dp_resp
              i_user_resp  = sy-uname
            IMPORTING
              e_num_lote   = lote.

          MOVE: lote                        TO wa_zglt035-lote,
                lote                        TO wa_saida-lote,
                wa_saida-bukrs              TO wa_zglt035-bukrs,
*              wa_zglt031-tp_lcto          TO wa_zglt035-tp_lcto, " Rubenilson Pereira - 31.07.25 #181597
                wa_zglt031-tp_lcto          TO wa_zcot0019-tp_lcto," Rubenilson Pereira - 31.07.25 #181597
                dp_resp                     TO wa_zglt035-dpto_resp,
                'Apropriação '              TO wa_zglt035-bktxt,
                wa_saida-waerk_fatura       TO wa_zglt035-moeda_doc,
                wa_zglt031-st_lc_moeda      TO wa_zglt035-st_lc_moeda,
                wa_zglt031-moeda_int_hist   TO wa_zglt035-moeda_int_hist,
                wa_zglt031-moeda_ft_hist    TO wa_zglt035-moeda_ft_hist,
                wa_zglt031-moeda_gp_hist    TO wa_zglt035-moeda_gp_hist,
                wa_zglt031-blart            TO wa_zglt035-blart,
                wa_zglt031-descricao        TO wa_zglt035-xblnr,
                wa_zglt031-bktxt            TO wa_zglt035-bktxt,
                wa_saida-budat              TO wa_zglt035-bldat,
                wa_saida-budat              TO wa_zglt035-budat,
                wa_saida-budat              TO wa_zglt035-dt_lcto,
                wa_zglt031-prov_est         TO wa_zglt035-prov_est,
                wa_zglt031-st_ap_fiscal     TO wa_zglt035-st_ap_fiscal,
                wa_saida-budat+4(2)         TO wa_zglt035-monat,
                wa_saida-budat+0(4)         TO wa_zglt035-gjahr,
                sy-uname                    TO wa_zglt035-usnam,
                wa_saida-budat              TO wa_zglt035-dt_entrada,
                sy-uzeit                    TO wa_zglt035-hr_entrada.

          FREE it_zglt036.

          CLEAR v_werks .
          v_werks  = |{ wa_saida-cl_codigo ALPHA = IN }|.

          SELECT SINGLE * FROM tka02 INTO @DATA(wl_tka02)
             WHERE bukrs EQ @wa_saida-bukrs.

          LOOP AT  it_zglt032 INTO wa_zglt032.

            ADD 1 TO lv_subseq.

* ---> S4 Migration - 06/07/2023 - DG
*          SELECT SINGLE * FROM cskb INTO @DATA(wa_cskb)
*            WHERE kokrs  EQ @wl_tka02-kokrs
*            AND   kstar  EQ @wa_zglt032-hkont
*            AND   datab  LE @sy-datum
*            AND   datbi  GE @sy-datum.

            DATA: lt_returns TYPE TABLE OF bapiret2,
                  ls_coeldes TYPE bapi1030_ceoutputlist.

            DATA: wa_cskb            TYPE  cskb,
                  lv_controllingarea TYPE  bapi1030_gen-co_area,
                  lv_costelement     TYPE  bapi1030_gen-cost_elem,
                  lv_keydate         TYPE  bapi1030_gen-some_date.

            lv_controllingarea  = wl_tka02-kokrs.
            lv_costelement      = wa_zglt032-hkont.
            lv_keydate          = sy-datum.

            CLEAR: lt_returns[], ls_coeldes, wa_cskb.

            CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
              EXPORTING
                controllingarea   = lv_controllingarea
                costelement       = lv_costelement
                keydate           = lv_keydate
              IMPORTING
                costelementdetail = ls_coeldes
              TABLES
                return            = lt_returns.

            READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
            IF sy-subrc = 0.
            ELSE.
              wa_cskb-kokrs = lv_controllingarea.
              wa_cskb-kstar = lv_costelement .
              wa_cskb-datbi = ls_coeldes-valid_to.
              wa_cskb-katyp = ls_coeldes-celem_category.
              SELECT SINGLE * FROM zcot0011 INTO @DATA(wa_zcot0011)
                   WHERE bukrs EQ  @wa_saida-bukrs
                    AND  werks EQ  @v_werks.
              wa_zglt036-kostl = wa_zcot0011-kostl.
              wa_zglt036-seqsub = '1'.
            ENDIF.

* <--- S4 Migration - 06/07/2023 - DG

            MOVE: lv_subseq                 TO wa_zglt036-seqitem,
                  wa_zglt032-tp_lcto        TO wa_zglt036-tp_lcto,
                  wa_zglt032-bschl          TO wa_zglt036-bschl,
                  v_werks                   TO wa_zglt036-gsber,
                  wa_zglt032-hkont          TO wa_zglt036-hkont,
                  wa_zglt032-sgtxt          TO wa_zglt036-sgtxt,
                  wa_zglt032-umskz          TO wa_zglt036-umskz,
                  wa_saida-budat            TO wa_zglt036-dt_vct.

            wa_zglt036-seqsub = lv_subseq.

            IF '31_21_01_11' CS wa_zglt032-bschl.
              MOVE:     'S'            TO wa_zglt036-zlsch.
            ENDIF.

            MOVE: abs( wa_saida-vlr_brl_liq ) TO wa_zglt036-vlr_moeda_doc,
                  abs( wa_saida-vlr_brl_liq ) TO wa_zglt036-vlr_moeda_int,
                  abs( wa_saida-vlr_usd_liq ) TO wa_zglt036-vlr_moeda_forte.


            APPEND wa_zglt036 TO it_zglt036.
            CLEAR: wa_zglt036, wa_zglt032.

          ENDLOOP.
          CALL METHOD zcl_gerar_lote=>contabilizar_lote(
            EXPORTING
              i_arredonda = abap_true
            CHANGING
              i_zglt036   = it_zglt036
              i_zglt035   = wa_zglt035 ).

          CLEAR: v_liberado.
          CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
            EXPORTING
              p_num_lote = wa_zglt035-lote
            IMPORTING
              p_liberado = v_liberado.

          CHECK v_liberado EQ abap_true.

          MOVE: wa_zglt035-lote     TO wa_saida-lote,
                wa_zglt035-doc_lcto TO wa_saida-doc_lcto.

          UPDATE zsdt0225 SET lote     = lote
                              doc_lcto = wa_zglt035-doc_lcto
          WHERE id_seq EQ wa_saida-id_seq
            AND docnum EQ wa_saida-docnum.

          SUBMIT z_grava_zib_zgl  WITH p_lote = lote AND RETURN.

        ELSE.
          MESSAGE s836(sd) WITH TEXT-003.
        ENDIF.

      ENDIF.

      CLEAR wg_selectedrow.

    ENDLOOP.
  ENDIF.

  REFRESH it_saida.
  PERFORM seleciona_dados.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.

FORM gera_mr22.

  DATA: vxblnr      TYPE xblnr,
        vdata(10)   TYPE c,
        vvalor1(16) TYPE c,
        vvalor2(16) TYPE c,
        vdmbtr      TYPE dmbtr,
        vdmbe2      TYPE dmbe2,
        vbukrs      TYPE zsdt0225-bukrs,
        vwerks      TYPE zsdt0225-werks,
        vmaterial   TYPE zsdt0225-cod_material.


  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.


  REFRESH:  t_bdcdata, t_bdcmsgcoll.

  LOOP AT tg_selectedrow INTO wg_selectedrow.

    READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.

    IF wa_saida-status = '@0A@'.
      MESSAGE 'Favor gerar o lançamento na ZGL016! '  TYPE 'S'.
      EXIT.
    ELSEIF wa_saida-status = '@09@' AND wa_saida-belnr IS INITIAL.
      MESSAGE 'Favor aguarde o retorno do lançamento Apropr.Contabil ! '  TYPE 'S'.
      EXIT.
    ELSE.

      CLEAR vwerks.
      CONDENSE wa_saida-cl_codigo.
      vwerks  = |{ wa_saida-cl_codigo ALPHA = IN }|.

      IF wa_saida-bukrs = '0015' AND wa_saida-cod_material = '000000000000119892'.
        vmaterial = '000000000000119891'.
      ELSEIF wa_saida-bukrs = '0050' AND wa_saida-cod_material = '000000000000119892'. "USER STORY 102947 / Anderson Oenning
        vmaterial = '000000000000119891'.
      ELSEIF wa_saida-bukrs = '0015' AND wa_saida-cod_material = '000000000000119895'.
        vmaterial = '000000000000119894'.
      ELSEIF wa_saida-bukrs = '0050' AND wa_saida-cod_material = '000000000000119895'. "USER STORY 102947 / Anderson Oenning
        vmaterial = '000000000000119894'.
      ELSE.
        vmaterial = wa_saida-cod_material.
      ENDIF.


      SELECT SINGLE * FROM mara INTO @DATA(wa_mara)
        WHERE matnr EQ @vmaterial.

      SELECT  SINGLE * FROM zcot0012 INTO @DATA(wa_0012)
        WHERE werks_og EQ @vwerks
        AND   matkl    EQ @wa_mara-matkl
        AND operacao   EQ @wa_saida-operacao.


      IF sy-subrc <> 0.
        MESSAGE 'Centro Destino não parametrizado !' TYPE 'S'.
        EXIT.
      ELSE.

        CONCATENATE wa_saida-budat+6(2) '.'  wa_saida-budat+4(2) '.' wa_saida-budat+0(4) INTO vdata.
        WRITE: wa_saida-vlr_brl_liq TO vvalor1.
        CONDENSE vvalor1 NO-GAPS.

        WRITE: wa_saida-vlr_usd_liq TO vvalor2.
        CONDENSE vvalor2 NO-GAPS.

        CONCATENATE 'SERV-' wa_saida-belnr INTO vxblnr.
        vbukrs = wa_saida-bukrs.
        vwerks = wa_0012-werks_dt.

        PERFORM z_mr22 USING  vdata
                              vvalor1
                              vvalor2
                              vxblnr
                              vbukrs
                              vwerks
                              vmaterial.


        IF t_bdcmsgcoll[] IS NOT INITIAL.
          READ TABLE t_bdcmsgcoll WITH KEY msgtyp = 'S'
                                           msgnr = '019'.
          IF sy-subrc = 0.
            wa_saida-doc_mr22 = t_bdcmsgcoll-msgv1.
            wa_saida-status   = '@08@'.
            MODIFY it_saida INDEX sy-tabix FROM wa_saida TRANSPORTING doc_mr22.

            UPDATE zsdt0225 SET doc_mr22 = wa_saida-doc_mr22
              WHERE id_seq EQ wa_saida-id_seq
                AND docnum EQ wa_saida-docnum." Rubenilson Pereira - 31.07.25 #181597
          ELSE.
            READ TABLE t_bdcmsgcoll WITH KEY msgtyp = 'E'.
*                                           msgnr = '019'.

*            wa_saida-status   = '@0A@'.
            MESSAGE i024(sd) WITH TEXT-004 ', ' t_bdcmsgcoll-msgv1.
            EXIT.
          ENDIF.
        ELSE.
          READ TABLE t_bdcmsgcoll WITH KEY msgtyp = 'E'.
          MESSAGE i024(sd) WITH TEXT-004 ', ' t_bdcmsgcoll-msgv1..
          EXIT.
        ENDIF.
      ENDIF.

    ENDIF.

    CLEAR: wa_saida,  vxblnr, vdata, vvalor1,  vvalor2,  vdmbtr,  vdmbe2,  vbukrs, vwerks, vmaterial.

    REFRESH:  t_bdcdata, t_bdcmsgcoll.
  ENDLOOP.

ENDFORM.


FORM criar_botao.
  DATA: wa_button   TYPE smp_dyntxt,
        wa_button_2 TYPE smp_dyntxt,
        wa_button_3 TYPE smp_dyntxt.

  wa_button-text      = 'Parâmetro'.
  wa_button-icon_id   = icon_settings.
  wa_button-icon_text = 'Parâmetro'.
  sscrfields-functxt_01 = wa_button.


  wa_button_2-text      = 'Apropriação MR22'.
  wa_button_2-icon_id   = icon_table_settings.
  wa_button_2-icon_text = 'Apropriação MR22'.
  sscrfields-functxt_02 = wa_button_2.

  " Inicio - Rubenilson Pereira - 31.07.25 #181597
  wa_button_3-text      = 'Parametrização ZGL'.
  wa_button_3-icon_id   = icon_settings.
  wa_button_3-icon_text = 'Parametrização ZGL'.
  sscrfields-functxt_03 = wa_button_3.
  " Fim - Rubenilson Pereira - 31.07.25 #181597
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS 'ST_0101'.
  SET TITLEBAR 'TL_0101'.

  PERFORM alv_01.

  IF g_custom_container_01 IS INITIAL.

    CREATE OBJECT g_custom_container_01
      EXPORTING
        container_name              = 'CONTAINER01'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid_01 IS INITIAL AND  g_custom_container_01 IS NOT INITIAL.

      CREATE OBJECT g_grid_01
        EXPORTING
          i_parent          = g_custom_container_01
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function_01 TO tl_function_01.
    wl_function_01 = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function_01 TO tl_function_01.

    wa_layout_01-stylefname = 'CELLTAB'.

    SET HANDLER: lcl_event_handler=>on_data_changed FOR g_grid_01,
                 lcl_event_handler=>on_data_changed_finished FOR g_grid_01.


    CALL METHOD g_grid_01->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout_01
        it_toolbar_excluding          = tl_function_01
      CHANGING
        it_outtab                     = it_saida_01[]
        it_fieldcatalog               = it_fcat_01
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CALL METHOD g_grid_01->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_01->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid_01->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD g_grid_01->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fcat_01.


    CALL METHOD g_grid_01->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM gravar_dados.
    WHEN 'INS'.
      APPEND INITIAL LINE TO it_saida_01.
    WHEN 'DEL'.

      CALL METHOD g_grid_01->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow_01.

      IF tg_selectedrow_01[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'I'.
        EXIT.
      ELSE.
        LOOP AT tg_selectedrow_01 INTO wg_selectedrow_01.

          READ TABLE it_saida_01 INTO wa_saida_01 INDEX wg_selectedrow_01-index.

          IF wa_saida_01 IS INITIAL.

            DELETE it_saida_01 INDEX wg_selectedrow_01-index.
          ELSE.

            DELETE FROM  zcot0011
              WHERE  bukrs EQ wa_saida_01-bukrs
              AND    werks EQ wa_saida_01-werks
              AND    kostl EQ wa_saida_01-kostl.

            REFRESH it_saida_01.
            PERFORM seleciona_dados_01.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CALL METHOD g_grid_01->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
  ENDCASE.
ENDMODULE.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0103 OUTPUT.
  SET PF-STATUS 'ST_0103'.
  SET TITLEBAR 'TL_0103'.

  PERFORM alv_03.

  IF g_custom_container_03 IS INITIAL.

    CREATE OBJECT g_custom_container_03
      EXPORTING
        container_name              = 'CONTAINER0103'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid_03 IS INITIAL AND  g_custom_container_03 IS NOT INITIAL.

      CREATE OBJECT g_grid_03
        EXPORTING
          i_parent          = g_custom_container_03
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function_03 TO tl_function_03.
    wl_function_03 = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function_03 TO tl_function_03.

    wa_layout_03-stylefname = 'CELLTAB'.

    SET HANDLER: lcl_event_handler_03=>on_data_changed FOR g_grid_03.

    CALL METHOD g_grid_03->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout_03
        it_toolbar_excluding          = tl_function_03
      CHANGING
        it_outtab                     = it_saida_03[]
        it_fieldcatalog               = it_fcat_03
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CALL METHOD g_grid_03->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_03->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid_03->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD g_grid_03->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fcat_03.


    CALL METHOD g_grid_03->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM gravar_dados_03.
    WHEN 'INS'.
      APPEND INITIAL LINE TO it_saida_03.
    WHEN 'DEL'.

      CALL METHOD g_grid_03->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow_03.

      IF tg_selectedrow_03[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'I'.
        EXIT.
      ELSE.
        LOOP AT tg_selectedrow_03 INTO wg_selectedrow_03.

          READ TABLE it_saida_03 INTO wa_saida_03 INDEX wg_selectedrow_03-index.

          IF wa_saida_03 IS INITIAL.

            DELETE it_saida_03 INDEX wg_selectedrow_03-index.
          ELSE.

            DELETE FROM  zcot0019
              WHERE  bukrs   EQ wa_saida_03-bukrs
              AND    matkl   EQ wa_saida_03-matkl
              AND    tp_lcto EQ wa_saida_03-tp_lcto.

            REFRESH it_saida_03.
            PERFORM seleciona_dados_03.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CALL METHOD g_grid_03->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
  ENDCASE.
ENDMODULE.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

FORM gravar_dados.
  DATA:  wa_gravar TYPE zcot0011.

  LOOP AT it_saida_01 INTO wa_saida_01.

    SELECT SINGLE * FROM zcot0011 INTO @DATA(wa_0011)
      WHERE bukrs EQ @wa_saida_01-bukrs
      AND   werks EQ @wa_saida_01-werks
      AND   kostl EQ @wa_saida_01-kostl
      AND   matkl EQ @wa_saida_01-matkl.

    IF sy-subrc <> 0.

      SELECT SINGLE * FROM t001 INTO @DATA(wt001)
       WHERE bukrs EQ @wa_saida_01-bukrs.

      IF sy-subrc EQ  0.
        wa_gravar-bukrs = wa_saida_01-bukrs.
      ELSE.
        MESSAGE 'Empresa informada não existe !' TYPE 'I'.
        EXIT.
      ENDIF.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
      SELECT SINGLE * FROM t023 INTO @DATA(wt023)
        WHERE matkl EQ @wa_saida_01-matkl.

      IF sy-subrc EQ 0.
        wa_gravar-matkl = wa_saida_01-matkl.
      ELSE.
        MESSAGE 'Grp. Material informado não existe!' TYPE 'I'.
        EXIT.
      ENDIF.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

      SELECT SINGLE * FROM t001w INTO @DATA(wt001w)
        WHERE werks EQ @wa_saida_01-werks.

      IF sy-subrc EQ 0.
        wa_gravar-werks = wa_saida_01-werks.
      ELSE.
        MESSAGE 'Filial informada não existe!' TYPE 'I'.
        EXIT.
      ENDIF.

      SELECT SINGLE * FROM csks INTO @DATA(wcsks)
        WHERE kostl EQ @wa_saida_01-kostl
        AND   bukrs EQ @wa_saida_01-bukrs
        AND   gsber EQ @wa_saida_01-werks
        AND   datbi EQ '99991231'.

      IF sy-subrc = 0.

        IF  wcsks-bkzkp EQ ' '.
          wa_gravar-kostl =  wa_saida_01-kostl.
        ELSE.
          MESSAGE 'Centro de custo bloqueado p/ Lançamentos  !' TYPE 'I'.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE 'Centro de custo não pertence a divisão  !' TYPE 'I'.
        EXIT.
      ENDIF.

      MODIFY zcot0011 FROM wa_gravar.
      COMMIT WORK.

    ENDIF.
    CLEAR: wa_saida_01, wa_gravar.
  ENDLOOP.

  CHECK wa_gravar IS INITIAL.
  MESSAGE 'Dados gravado com sucesso!' TYPE 'I'.

  REFRESH it_saida_01.

  PERFORM seleciona_dados_01.
ENDFORM.

*** Inicio - Rubenilson Pereira - 31.07.25 #181597
FORM gravar_dados_03.
  DATA:  wa_gravar TYPE zcot0019.

  LOOP AT it_saida_03 INTO wa_saida_03.

    SELECT SINGLE * FROM zcot0019 INTO @DATA(wa_0019)
      WHERE bukrs EQ @wa_saida_03-bukrs
      AND   matkl EQ @wa_saida_03-matkl
      AND   tp_lcto EQ @wa_saida_03-tp_lcto.

    IF sy-subrc <> 0.

      SELECT SINGLE * FROM t001 INTO @DATA(wt001)
       WHERE bukrs EQ @wa_saida_03-bukrs.

      IF sy-subrc EQ  0.
        wa_gravar-bukrs = wa_saida_03-bukrs.
      ELSE.
        MESSAGE 'Empresa informada não existe !' TYPE 'I'.
        EXIT.
      ENDIF.

      SELECT SINGLE * FROM t023 INTO @DATA(wt023)
        WHERE matkl EQ @wa_saida_03-matkl.

      IF sy-subrc EQ 0.
        wa_gravar-matkl = wa_saida_03-matkl.
      ELSE.
        MESSAGE 'Grp. Material informado não existe!' TYPE 'I'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM zglt031
        INTO @DATA(lw_zglt031)
        WHERE tp_lcto = @wa_saida_03-tp_lcto.
      IF sy-subrc IS INITIAL.
        wa_gravar-tp_lcto = wa_saida_03-tp_lcto.
      ELSE.
        MESSAGE 'Tipo de lançamento informado não existe' TYPE 'I'.
        EXIT.
      ENDIF.

      wa_gravar-usuario = sy-uname.
      wa_gravar-data_registro = sy-datum.
      wa_gravar-hora_registro = sy-uzeit.

      MODIFY zcot0019 FROM wa_gravar.
      COMMIT WORK.

    ENDIF.
    CLEAR: wa_saida_03, wa_gravar.
  ENDLOOP.

  CHECK wa_gravar IS INITIAL.
  MESSAGE 'Dados gravado com sucesso!' TYPE 'I'.

  REFRESH it_saida_03.

  PERFORM seleciona_dados_03.
ENDFORM.
*** Fim - Rubenilson Pereira - 31.07.25 #181597

FORM f_dynpro USING p_dynbegin TYPE any
                    p_fnam     TYPE any
                    p_fval     TYPE any.


  IF p_dynbegin = 'X'.
    t_bdcdata-dynbegin = 'X'.
    t_bdcdata-program  = p_fnam.
    t_bdcdata-dynpro   = p_fval.
  ELSE.
    t_bdcdata-fnam = p_fnam.
    t_bdcdata-fval = p_fval.
  ENDIF.

  APPEND t_bdcdata.
  CLEAR  t_bdcdata.

ENDFORM.

FORM atualiza_doc.

  DATA obj_key TYPE zib_contabil_chv-obj_key.

  SELECT  *
    FROM zsdt0225 INTO TABLE it_zsdt0225
    WHERE doc_lcto NE '0000000000'
     AND  belnr    EQ ' '.

  LOOP AT it_zsdt0225 INTO wa_zsdt0225.

    CONCATENATE 'ZGL17' wa_zsdt0225-doc_lcto wa_zsdt0225-dt_fatura+0(4) INTO obj_key.

    SELECT SINGLE * FROM zib_contabil_chv INTO @DATA(wa_zib_contabil_chv)
        WHERE obj_key EQ @obj_key.

    IF sy-subrc = 0.
      UPDATE zsdt0225 SET belnr = wa_zib_contabil_chv-belnr
      WHERE id_seq EQ wa_zsdt0225-id_seq
        AND docnum EQ wa_zsdt0225-docnum." Rubenilson Pereira - 31.07.25 #181597

      COMMIT WORK.
    ENDIF.

    CLEAR: wa_zsdt0225,  obj_key, wa_zib_contabil_chv.
  ENDLOOP.

  REFRESH it_zsdt0225.
ENDFORM.

FORM estorno_mr22.

  DATA: vxblnr      TYPE xblnr,
        vdata(10)   TYPE c,
        vvalor1(16) TYPE c,
        vvalor2(16) TYPE c,
        vdmbtr      TYPE dmbtr,
        vdmbe2      TYPE dmbe2,
        vbukrs      TYPE zsdt0225-bukrs,
        vwerks      TYPE zsdt0225-werks,
        vmaterial   TYPE zsdt0225-cod_material.


  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.


  REFRESH:  t_bdcdata, t_bdcmsgcoll.

  LOOP AT tg_selectedrow INTO wg_selectedrow.

    CLEAR v_werks.

    READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.

    CONDENSE wa_saida-cl_codigo.
    v_werks  = |{ wa_saida-cl_codigo ALPHA = IN }|.

    CONCATENATE wa_saida-budat+6(2) '.'  wa_saida-budat+4(2) '.' wa_saida-budat+0(4) INTO vdata.

    MULTIPLY wa_saida-vlr_brl_liq BY -1.
    MULTIPLY wa_saida-vlr_usd_liq BY -1.

    WRITE: wa_saida-vlr_brl_liq TO vvalor1.
    WRITE: wa_saida-vlr_usd_liq TO vvalor2.

    CONCATENATE 'SERV-' wa_saida-belnr INTO vxblnr.
    vbukrs = wa_saida-bukrs.
    "VWERKS = V_WERKS.

    IF wa_saida-bukrs = '0015' AND wa_saida-cod_material = '000000000000119892'.
      vmaterial = '000000000000119891'.
    ELSEIF wa_saida-bukrs = '0050' AND wa_saida-cod_material = '000000000000119892'. "USER STORY 102947 / Anderson Oenning
      vmaterial = '000000000000119891'.

    ELSEIF wa_saida-bukrs = '0015' AND wa_saida-cod_material = '000000000000119895'.
      vmaterial = '000000000000119894'.

    ELSEIF wa_saida-bukrs = '0050' AND wa_saida-cod_material = '000000000000119895'. "USER STORY 102947 / Anderson Oenning
      vmaterial = '000000000000119894'.
    ELSE.
      vmaterial = wa_saida-cod_material.
    ENDIF.

    SELECT SINGLE * FROM mara INTO @DATA(wa_mara)
      WHERE matnr EQ @vmaterial.

    SELECT SINGLE werks_dt FROM zcot0012 INTO vwerks
      WHERE werks_og EQ v_werks
      AND   matkl    EQ wa_mara-matkl
      AND   operacao EQ wa_saida-operacao.


    PERFORM z_mr22 USING  vdata
                          vvalor1
                          vvalor2
                          vxblnr
                          vbukrs
                          vwerks
                          vmaterial.

    IF t_bdcmsgcoll[] IS NOT INITIAL.
      READ TABLE t_bdcmsgcoll WITH KEY msgtyp = 'S'
                                       msgnr = '019'.
      IF sy-subrc = 0.
        wa_saida-doc_mr22 = ' '.
        wa_saida-status   = '@09@'.
        MODIFY it_saida INDEX sy-tabix FROM wa_saida TRANSPORTING doc_mr22.

        UPDATE zsdt0225 SET doc_mr22 = wa_saida-doc_mr22
          WHERE id_seq EQ wa_saida-id_seq.
      ELSE.
        MESSAGE s836(sd) WITH TEXT-004.
        EXIT.
      ENDIF.
    ELSE.
      MESSAGE s836(sd) WITH TEXT-004.
      EXIT.
    ENDIF.

    CLEAR: wa_saida,  vxblnr, vdata, vvalor1,  vvalor2,  vdmbtr,  vdmbe2,  vbukrs, vwerks, vmaterial.

    REFRESH:  t_bdcdata, t_bdcmsgcoll.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ESTORNO_DOCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM estorno_docs .

  DATA vdata(10) TYPE c.
  DATA wl_mode(1).


  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Favor selecione uma linha !' TYPE 'S'.
    EXIT.
  ELSE.

    REFRESH it_msg[].

    LOOP AT tg_selectedrow INTO wg_selectedrow.

      READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.

      IF wa_saida-doc_mr22 IS NOT INITIAL.
        MESSAGE 'Realizar antes o estorno da MR22' TYPE 'S'.
        EXIT.
      ELSE.
        IF wa_saida-belnr IS NOT INITIAL.

          CONCATENATE wa_saida-budat+6(2) '.' wa_saida-budat+4(2) '.' wa_saida-budat+0(4) INTO vdata.
          REFRESH t_bdcdata.

          PERFORM grava_est_zgl USING wa_saida-bukrs  wa_saida-belnr wa_saida-budat+0(4) vdata.

*          PERFORM F_BDC_DATA  USING:
*                    'SAPMF05A'  '0105'  'X'  ''                 ' ',
*                    ''          ''      ''   'BDC_OKCODE'        '/00',
*                    ''          ''      ''   'RF05A-BELNS'      WA_SAIDA-BELNR,
*                    ''          ''      ''   'BKPF-BUKRS'       WA_SAIDA-BUKRS,
*                    ''          ''      ''   'RF05A-GJAHS'      WA_SAIDA-BUDAT+0(4),
*                    ''          ''      ''   'UF05A-STGRD'      '02',
*                    ''          ''      ''   'BSIS-BUDAT'       VDATA,
*                    'SAPMF05A'  '0105'  'X'  ''                 ' ',
*                    ''          ''      ''   'BDC_OKCODE'        '=BU'.
*
*          CALL TRANSACTION 'FB08' USING T_BDCDATA MODE 'E' MESSAGES INTO IT_MSG.

        ENDIF.

        READ TABLE it_msg WITH KEY  msgtyp = 'E'.
        IF sy-subrc = 0.
          DATA(vmessage) = it_msg-msgv1.
          MESSAGE  vmessage TYPE 'S'.
          EXIT.
        ELSE.
          UPDATE zsdt0225 SET  lote      = ' '
                               doc_lcto  = ' '
                               belnr     = ' '
            WHERE id_seq EQ wa_saida-id_seq.
          COMMIT WORK.
        ENDIF.
      ENDIF.
    ENDLOOP.

    PERFORM seleciona_dados.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDFORM.


FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.

  t_bdcdata-program   = p_program.
  t_bdcdata-dynpro    = p_dynpro.
  t_bdcdata-dynbegin  = p_start.
  t_bdcdata-fnam      = p_fnam.
  t_bdcdata-fval      = p_fval.
  APPEND t_bdcdata.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_MR22
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_mr22 USING  vdata
                   vvalor1
                   vvalor2
                   vxblnr
                   vbukrs
                   vwerks
                   matnr.


  PERFORM f_dynpro USING:  'X' 'SAPRCKM_MR22'                 '0201',
                             ' ' 'BDC_CURSOR'                 'MR21HEAD-XBLNR',
                             ' ' 'BDC_OKCODE'                 '=ENTR',
                             ' ' 'MR21HEAD-BUDAT'             vdata,
                             ' ' 'MR21HEAD-BUKRS'             vbukrs,
                             ' ' 'MR21HEAD-WERKS'             vwerks,
                             ' ' 'MR21HEAD-XBLNR'             vxblnr,
                             ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                           0250MR22_SUB',
                             ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL_0250'.


  PERFORM f_dynpro USING:  'X' 'SAPRCKM_MR22'               '0201',
                           ' ' 'BDC_OKCODE'                 '=ENTR',
                           ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                            0250MR22_SUB',
                           ' ' 'BDC_CURSOR'                 'CKI_MR22_0250-ZUUMB(01)',
                           ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL - OHNE BWKEY_025',
                           ' ' 'CKI_MR22_0250-MATNR(01)'    matnr,
                           ' ' 'CKI_MR22_0250-ZUUMB(01)'    vvalor1.

  PERFORM f_dynpro USING:  'X' 'SAPRCKM_MR22'               '0201',
                           ' ' 'BDC_OKCODE'                 '=TAB2',
                           ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                            0250MR22_SUB',
                           ' ' 'BDC_CURSOR'                 'CKI_MR22_0250-MATNR(02)',
                           ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL - OHNE BWKEY_025'.

  PERFORM f_dynpro USING:  'X' 'SAPRCKM_MR22'               '0201',
                           ' ' 'BDC_OKCODE'                 '=ENTR',
                           ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                            0250MR22_SUB',
                           ' ' 'BDC_CURSOR'                 'CKI_MR22_0250-ZUUMB(01)',
                           ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL - OHNE BWKEY_025',
                           ' ' 'CKI_MR22_0250-ZUUMB(01)'    vvalor2.

  PERFORM f_dynpro USING:  'X' 'SAPRCKM_MR22'               '0201',
                           ' ' 'BDC_OKCODE'                 '=TAB3',
                           ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                            0250MR22_SUB',
                           ' ' 'BDC_CURSOR'                 'CKI_MR22_0250-MATNR(02)',
                           ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL - OHNE BWKEY_025'.

  PERFORM f_dynpro USING:  'X' 'SAPRCKM_MR22'               '0201',
                           ' ' 'BDC_OKCODE'                 '=CALC',
                           ' ' 'BDC_SUBSCR'                 'SAPRCKM_MR22                            0250MR22_SUB',
                           ' ' 'BDC_CURSOR'                 'CKI_MR22_0250-MATNR(01)',
                           ' ' 'MR21HEAD-SCREEN_VARIANT'    'LAGERMATERIAL - OHNE BWKEY_025',
                           ' ' 'CKI_MR22_0250-SELKZ(01)'    'X'.

  PERFORM f_dynpro USING: 'X' 'SAPRCKM_MR22'                '0400',
                          ' ' 'BDC_CURSOR'                  '%#AUTOTEXT001',
                          ' ' 'BDC_OKCODE'                  '=ENTR',
                          ' ' 'DISPLAY-F_CURR1-SELKZ'       'X'.

  PERFORM f_dynpro USING: 'X' 'SAPRCKM_MR22'                '0201',
                          ' ' 'BDC_OKCODE'                  '=SAVE',
                          ' ' 'BDC_SUBSCR'                  'SAPRCKM_MR22                            0250MR22_SUB',
                          ' ' 'BDC_CURSOR'                  'CKI_MR22_0250-MATNR(02)',
                          ' ' 'MR21HEAD-SCREEN_VARIANT'     'LAGERMATERIAL - OHNE BWKEY_025'.

  DATA(vmode) = 'N'.
  CALL TRANSACTION 'MR22'
         USING t_bdcdata MODE vmode UPDATE 'S'   MESSAGES INTO t_bdcmsgcoll.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados_02 .

  REFRESH: it_saida_02[], it_dd07v[].

  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'ZDM_OPER_AQUAV'
      state         = 'A'
      langu         = sy-langu
    TABLES
      dd07v_tab     = it_dd07v
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.


  SELECT * FROM zcot0012 INTO TABLE @DATA(it_zcot0012).

  IF it_zcot0012[] IS NOT INITIAL.

    LOOP AT it_zcot0012 INTO DATA(wa_zcot0012).

      SELECT SINGLE * FROM t001w INTO @DATA(wa_t001w)
        WHERE werks EQ @wa_zcot0012-werks_og.

      wa_saida_02-werks_og = wa_zcot0012-werks_og.
      wa_saida_02-texto_og = wa_t001w-name1.

      CLEAR wa_t001w.

      SELECT SINGLE * FROM t001w INTO wa_t001w
        WHERE werks EQ wa_zcot0012-werks_dt.

      wa_saida_02-werks_dt = wa_zcot0012-werks_dt.
      wa_saida_02-texto_dt = wa_t001w-name1.


      SELECT SINGLE * FROM t023t INTO @DATA(wa_t023t)
        WHERE spras EQ @sy-langu
        AND   matkl EQ @wa_zcot0012-matkl.

      wa_saida_02-matkl   = wa_zcot0012-matkl.
      wa_saida_02-wgbez60 = wa_t023t-wgbez60.

      vdomvalue_l =  wa_zcot0012-operacao.

      READ TABLE it_dd07v  INTO wa_dd07v  WITH KEY domvalue_l = vdomvalue_l.
      IF sy-subrc EQ 0.
        wa_saida_02-operacao = wa_zcot0012-operacao.
        wa_saida_02-texto_op = wa_dd07v-ddtext.
      ENDIF.

      FREE wa_saida_02-celltab.
      it_estilo_02 =  VALUE #( ( fieldname = 'WERKS_OG'   style = cl_gui_alv_grid=>mc_style_disabled  )
                               ( fieldname = 'MATKL'      style = cl_gui_alv_grid=>mc_style_disabled  )
                               ( fieldname = 'WERKS_DT'   style = cl_gui_alv_grid=>mc_style_disabled  )
                               ( fieldname = 'OPERACAO'   style = cl_gui_alv_grid=>mc_style_disabled  ) ).
      INSERT LINES OF it_estilo_02 INTO TABLE wa_saida_02-celltab.

      APPEND wa_saida_02 TO it_saida_02.
      CLEAR: wa_saida_02, wa_zcot0012, wa_t001w, wa_t023t, wa_dd07v, vdomvalue_l.

    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.
  SET PF-STATUS 'ST_0102'.
  SET TITLEBAR 'TL_0102'.


  IF g_custom_container_02 IS INITIAL.

    CREATE OBJECT g_custom_container_02
      EXPORTING
        container_name              = 'CONTAINER2'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid_02 IS INITIAL AND  g_custom_container_02 IS NOT INITIAL.

      CREATE OBJECT g_grid_02
        EXPORTING
          i_parent          = g_custom_container_02
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    PERFORM alv_02.

    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function_02 TO tl_function_02.
    wl_function_02 = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function_02 TO tl_function_02.

    wa_layout_02-stylefname = 'CELLTAB'.


    CALL METHOD g_grid_02->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout_02
        it_toolbar_excluding          = tl_function_02
      CHANGING
        it_outtab                     = it_saida_02[]
        it_fieldcatalog               = it_fcat_02
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CALL METHOD g_grid_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid_02->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.
*
*    CALL METHOD G_GRID_02->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = IT_FCAT_02.
*
*
    CALL METHOD g_grid_02->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE  TO SCREEN 0.
    WHEN 'SALVAR'.
      PERFORM  gravar_dados_02.

    WHEN  'INS'.
      APPEND INITIAL LINE TO it_saida_02.
    WHEN 'DEL'.

      CALL METHOD g_grid_02->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow_02.

      IF tg_selectedrow_02[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!'  TYPE 'S'.
        EXIT.
      ELSE.

        LOOP AT tg_selectedrow_02 INTO wg_selectedrow_02.

          READ TABLE it_saida_02 INTO wa_saida_02 INDEX wg_selectedrow_02-index.

          IF wa_saida_02 IS INITIAL.
            DELETE it_saida_02 INDEX wg_selectedrow_02-index.
          ELSE.

            DELETE FROM  zcot0012
              WHERE  werks_og EQ wa_saida_02-werks_og
              AND    matkl    EQ wa_saida_02-matkl
              AND    werks_dt EQ wa_saida_02-werks_dt
              AND    operacao EQ wa_saida_02-operacao.

            COMMIT WORK.

            REFRESH it_saida_02.
            PERFORM seleciona_dados_02.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CALL METHOD g_grid_02->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ALV_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_02 .

  CLEAR wl_fcat_02.
  REFRESH it_fcat_02[].

  PERFORM preenche_cat_02 USING :
        'WERKS_OG'           'Centro Origem'      '12'    ''    ''     ''     ''    ''    'X'     'T001W'     'WERKS',
        'TEXTO_OG'           'Descrição'          '25'    ''    ''     ''     ''    ''    ''      ''         '',
        'MATKL   '           'Grupo Mercadoria'   '15'    ''    ''     ''     ''    ''    'X'     'MARA'     'MATKL',
        'WGBEZ60 '           'Descrição'          '25'    ''    ''     ''     ''    ''    ''      ''         '',
        'WERKS_DT'           'Centro Destino'     '12'    ''    ''     ''     ''    ''    'X'     'T001W'    'WERKS',
        'TEXTO_DT'           'Descrição'          '25'    ''    ''     ''     ''    ''    ''      ''         '',
        'OPERACAO'           'Operação'           '10'    ''    ''     ''     ''    ''    'X'     'ZSDT0225' 'OPERACAO',
        'TEXTO_OP'           'Descrição'          '25'    ''    ''     ''     ''    ''    ''      ''         ''.

ENDFORM.

FORM preenche_cat_02 USING  VALUE(p_campo)
                            VALUE(p_desc)
                            VALUE(p_tam)
                            VALUE(p_zero)
                            VALUE(p_hot)
                            VALUE(p_sum)
                            VALUE(p_just)
                            VALUE(p_cor)
                            VALUE(p_edit)
                            VALUE(p_table)
                            VALUE(p_field).

  wl_fcat_02-fieldname = p_campo.
  wl_fcat_02-scrtext_l = p_desc.
  wl_fcat_02-scrtext_m = p_desc.
  wl_fcat_02-scrtext_s = p_desc.
  wl_fcat_02-outputlen = p_tam.
  wl_fcat_02-hotspot   = p_hot.
  wl_fcat_02-no_zero   = p_zero.
  wl_fcat_02-do_sum    = p_sum.
  wl_fcat_02-just      = p_just.
  wl_fcat_02-emphasize = p_cor.
  wl_fcat_02-edit      = p_edit.
  wl_fcat_02-ref_table = p_table.
  wl_fcat_02-ref_field = p_field.

  APPEND wl_fcat_02 TO  it_fcat_02.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GRAVAR_DADOS_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gravar_dados_02 .

  DATA:  wa_gravar TYPE zcot0012.

  LOOP AT it_saida_02 INTO wa_saida_02.

    SELECT SINGLE * FROM zcot0012 INTO @DATA(wa_0012)
      WHERE werks_og EQ @wa_saida_02-werks_og
      AND   matkl    EQ @wa_saida_02-matkl
      AND   werks_dt EQ @wa_saida_02-werks_dt
      AND   operacao EQ @wa_saida_02-operacao.

    IF sy-subrc <> 0.

      SELECT SINGLE * FROM t001w INTO @DATA(wa_t001w)
        WHERE werks EQ @wa_saida_02-werks_og.

      IF sy-subrc = 0.
        wa_gravar-werks_og = wa_saida_02-werks_og.
      ELSE.
        MESSAGE 'Centro Origem informado não existe!' TYPE 'S'.
        EXIT.
      ENDIF.

      CLEAR wa_t001w.

      SELECT SINGLE * FROM t001w INTO wa_t001w
        WHERE werks EQ wa_saida_02-werks_dt.

      IF sy-subrc = 0.
        wa_gravar-werks_dt = wa_saida_02-werks_dt.
      ELSE.
        MESSAGE 'Centro Destino informado não existe!' TYPE 'S'.
        EXIT.
      ENDIF.


      SELECT SINGLE * FROM t023 INTO @DATA(wa_t023)
        WHERE matkl EQ @wa_saida_02-matkl.

      IF sy-subrc = 0.
        wa_gravar-matkl = wa_saida_02-matkl.

      ELSE.
        MESSAGE 'Grupo de Mercadoria informado não existe !' TYPE 'S'.
        EXIT.
      ENDIF.

      CLEAR vdomvalue_l.

      vdomvalue_l =  wa_saida_02-operacao.

      READ TABLE it_dd07v INTO wa_dd07v WITH KEY domvalue_l = vdomvalue_l.
      IF sy-subrc = 0 OR vdomvalue_l EQ ' '.
        wa_gravar-operacao = wa_saida_02-operacao.
      ELSE.
        MESSAGE 'Operação informado não existe !' TYPE 'S'.
        EXIT.
      ENDIF.

      MODIFY zcot0012 FROM wa_gravar.
      COMMIT WORK.
    ENDIF.

    CLEAR: wa_saida_02, wa_gravar, wa_t001w, wa_0012.

  ENDLOOP.

  CHECK wa_gravar IS INITIAL.
  MESSAGE 'Dados gravado com sucesso!' TYPE 'I'.

  REFRESH it_saida_02.

  PERFORM seleciona_dados_02.

ENDFORM.

FORM grava_est_zgl USING p_burks p_belnr p_gjahs p_data.

  DATA: wl_setleaf TYPE setleaf,
        i_head     TYPE tbtcjob.

  DATA:   wl_job_id   LIKE tbtcjob-jobcount.
  DATA:   wl_jobn(32).

  DATA: BEGIN OF i_steplist OCCURS 10.
          INCLUDE STRUCTURE tbtcstep.
  DATA: END OF i_steplist.
  DATA : c_no(1) TYPE c . "value 'N', " Criação do job

  DATA: wl_tbtcjob  TYPE  tbtcjob,
        wl_tbtcstrt TYPE  tbtcstrt.

  DATA: lv_repname LIKE  rsvar-report.           " for variant handling
  DATA: iv_varname LIKE  raldb-variant VALUE 'SAP_UPGRADE'.
  DATA: iv_varianttext  LIKE  varit-vtext VALUE 'Upgrade variant'.
  DATA: wl_subrc TYPE sy-subrc.
  DATA: tt_reportparam TYPE TABLE OF  rsparams WITH HEADER LINE.

  SELECT SINGLE *
   FROM setleaf
   INTO wl_setleaf
    WHERE setname EQ 'MAGGI_JOB_USER'.

  IF sy-subrc NE 0.
    MESSAGE TEXT-e01 TYPE 'E'.
    EXIT.
  ENDIF.
  CONCATENATE 'Z_ESTORA_ZGL' sy-tcode  INTO wl_jobn SEPARATED BY '|'.

  i_head-jobname = wl_jobn. " Nome do JOBi_head-sdlstrtdt = sy-datum. " Dia
  i_head-sdlstrttm = sy-uzeit + 20. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
  i_head-stepcount = 1.

  tt_reportparam-selname = 'P_BURKS'.
  tt_reportparam-kind    = 'P'.
  tt_reportparam-sign    = 'I'.
  tt_reportparam-option  = 'EQ'.
  tt_reportparam-low = p_burks.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.

  tt_reportparam-selname = 'P_BELNR'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low   = p_belnr.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.

  tt_reportparam-selname = 'P_GJAHS'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = p_gjahs.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.

  tt_reportparam-selname = 'P_DATA'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = p_data.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.



  lv_repname = 'Z_ESTORA_ZGL'.
*    Write the variant first (Insert or Update)
  CALL FUNCTION 'SUBST_WRITE_UPGRADE_VARIANT'
    EXPORTING
      iv_reportname         = lv_repname
      iv_variantname        = iv_varname
      iv_varianttext        = iv_varianttext
    IMPORTING
      ev_funcrc             = wl_subrc
    TABLES
      tt_reportparam        = tt_reportparam
    EXCEPTIONS
      exist_check_failed    = 1
      update_failed         = 2
      update_not_authorized = 3
      update_no_report      = 4
      update_no_variant     = 5
      update_variant_locked = 6
      insert_failed         = 7
      insert_not_authorized = 8
      insert_no_report      = 9
      insert_variant_exists = 10
      insert_variant_locked = 11
      OTHERS                = 12.

  i_steplist-parameter = iv_varname. " Nome da variante
  i_steplist-program = 'Z_ESTORA_ZGL'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
  i_steplist-typ = 'A'. " Tipo de Job
  i_steplist-authcknam = wl_setleaf-valfrom.
  i_steplist-language = sy-langu.
  i_steplist-arcuser = wl_setleaf-valfrom.

  APPEND i_steplist.


  c_no = 'N'.
  CALL FUNCTION 'BP_JOB_CREATE'
    EXPORTING
      job_cr_dialog       = c_no " Coloque 'Y' se quiser ver
      job_cr_head_inp     = i_head " os valores atribuidos
    IMPORTING
      job_cr_head_out     = wl_tbtcjob
      job_cr_stdt_out     = wl_tbtcstrt
    TABLES
      job_cr_steplist     = i_steplist
    EXCEPTIONS
      cant_create_job     = 1
      invalid_dialog_type = 2
      invalid_job_data    = 3
      job_create_canceled = 4
      OTHERS              = 5.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobname   = wl_jobn
      jobcount  = wl_tbtcjob-jobcount
      strtimmed = 'X'.
ENDFORM.
