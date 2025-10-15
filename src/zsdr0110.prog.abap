*&---------------------------------------------------------------------*
*& Report  ZSDR0110
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0110.

TABLES: zlest0061.


TYPES: BEGIN OF ty_saida,
         filial         TYPE zlest0061-werks,
         safra          TYPE zlest0061-safra,
         cod_material   TYPE zlest0061-cod_material,
         tp_class       TYPE zlest0061-tp_class,
         nr_dco         TYPE zlest0061-nr_dco,
         peso_vinculado TYPE zlest0061-peso_vinculado,
         vlr_brl        TYPE zlest0061-vlr_brl,
         vlr_usd        TYPE zlest0061-vlr_usd,
         dt_fatura      TYPE zlest0061-dt_fatura,
         docnum         TYPE zlest0061-docnum,
         nf_saida       TYPE j_1bnfdoc-nfenum,
         series         TYPE j_1bnfdoc-series,
         doc_znfw       TYPE zsdt0225-doc_znfw,
         docnum_entrada TYPE zsdt0225-doc_znfw,
         id_seq         TYPE zsdt0225-id_seq,
         operacao       TYPE zsdt0229-operacao,
         operacao_225   TYPE zsdt0225-operacao,
         matnr          TYPE zsdt0225-matnr_ov,
         bukrs          TYPE zsdt0225-bukrs,
         doc_contabil   TYPE bkpf-belnr,
         b_bukrs        TYPE bkpf-bukrs,
         b_gjahr        TYPE bkpf-gjahr,
         werks_serv     TYPE zsdt0225-werks_serv,
         auart          TYPE auart,
       END OF ty_saida.

TYPES: BEGIN OF ty_saida_02,
         bukrs     TYPE zsdt0229-bukrs,
         butxt     TYPE t001-butxt,
         matkl     TYPE mara-matkl,
         auart     TYPE vbak-auart,
         operacao  TYPE zsdt0229-operacao,
         descricao TYPE zfiwrt0001-descricao,
         celltab   TYPE lvc_t_styl,
       END OF ty_saida_02.

TYPES: BEGIN OF ty_parc,
         parvw    TYPE zfiwrt0015-parvw,
         parid    TYPE zfiwrt0015-parid,
         nome(80),
         style    TYPE lvc_t_styl,
       END OF ty_parc.

DATA: BEGIN OF tg_impo OCCURS 0,
        taxtyp   TYPE zfiwrt0010-taxtyp,
        ttypetxt TYPE j_1bttytxt,
        taxgrp   TYPE j_1btaxgrp,
        base     TYPE zfiwrt0010-base,
        rate     TYPE zfiwrt0010-rate,
        taxval   TYPE zfiwrt0010-taxval,
        excbas   TYPE zfiwrt0010-excbas,
        othbas   TYPE zfiwrt0010-othbas,
      END OF tg_impo,

      BEGIN OF tg_mensagems OCCURS 0,
        seqnum  TYPE zfiwrt0005-seqnum,
        linnum  TYPE zfiwrt0005-linnum,
        message TYPE zfiwrt0005-message,
      END OF tg_mensagems,

      BEGIN OF tg_movest OCCURS 0,
        bwart   TYPE zfiwrt0004-bwart,
        tcode   TYPE zfiwrt0004-tcode,
        mwskz1  TYPE zfiwrt0004-mwskz1,
        estorno TYPE zfiwrt0004-estorno,
      END OF tg_movest,

      BEGIN OF tg_itens OCCURS 0,
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
      END OF tg_itens,

      BEGIN OF tg_contab OCCURS 0,
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
        style2  TYPE lvc_t_styl,
      END OF tg_contab,

      BEGIN OF tg_impo_comp OCCURS 0,
        itmnum TYPE zfiwrt0010-itmnum.
        INCLUDE STRUCTURE tg_impo.
DATA: END OF tg_impo_comp.



DATA: it_saida        TYPE TABLE OF ty_saida,
      wa_saida        TYPE ty_saida,
      it_saida_02     TYPE TABLE OF ty_saida_02,
      wa_saida_02     TYPE ty_saida_02,
      it_zsdt0225     TYPE TABLE OF zsdt0225,
      wa_zsdt0225     TYPE  zsdt0225,
      it_zsdt0229     TYPE TABLE OF zsdt0229,
      wa_zsdt0229     TYPE zsdt0229,
      it_zsdt0306_fat TYPE TABLE OF zsdt0306_fat,  "*-IR194132-29.10.2024-#156085-JT
      wa_zsdt0306_fat TYPE zsdt0306_fat,           "*-IR194132-29.10.2024-#156085-JT
      it_229          TYPE TABLE OF zsdt0229,
      wa_229          TYPE zsdt0229,
      tl_0008         TYPE TABLE OF zfiwrt0008,
      wa_0008         TYPE zfiwrt0008,
      it_zfiwrt0001   TYPE TABLE OF zfiwrt0001,
      wa_zfiwrt0001   TYPE zfiwrt0001,
      it_j_1bnfdoc    TYPE TABLE OF j_1bnfdoc,
      wa_j_1bnfdoc    TYPE j_1bnfdoc,
      it_bbranch      TYPE TABLE OF j_1bbranch,
      wa_bbranch      TYPE j_1bbranch,
      it_kna1         TYPE TABLE OF kna1,
      wa_kna1         TYPE kna1,
      it_zsdt0226     TYPE TABLE OF zsdt0226,
      wa_zsdt0226     TYPE zsdt0226,
      it_zfiwrt0008   TYPE TABLE OF zfiwrt0008,
      wa_zfiwrt0008   TYPE zfiwrt0008,
      it_bkpf         TYPE TABLE OF bkpf,
      wa_bkpf         TYPE bkpf,
      it_0002         TYPE TABLE OF zfiwrt0002 WITH HEADER LINE,
      it_0003         TYPE TABLE OF zfiwrt0003 WITH HEADER LINE,
      it_0004         TYPE TABLE OF zfiwrt0004 WITH HEADER LINE,
      it_0005         TYPE TABLE OF zfiwrt0005 WITH HEADER LINE,
      it_0006         TYPE TABLE OF zfiwrt0006 WITH HEADER LINE,
      it_0007         TYPE TABLE OF zfiwrt0007 WITH HEADER LINE,
      it_1baj         TYPE TABLE OF j_1baj     WITH HEADER LINE,
      it_1bajt        TYPE TABLE OF j_1bajt    WITH HEADER LINE,
      it_tbsl         TYPE TABLE OF tbsl       WITH HEADER LINE,
      it_skat         TYPE TABLE OF skat       WITH HEADER LINE,
      it_cskb         TYPE TABLE OF cskb       WITH HEADER LINE,
      it_user         TYPE TABLE OF user_addr  WITH HEADER LINE,
      it_parc         TYPE TABLE OF ty_parc    WITH HEADER LINE,
      it_lines        TYPE STANDARD TABLE OF tline WITH HEADER LINE,
      it_texto        TYPE catsxt_longtext_itab,
      t_mara          TYPE TABLE OF mara.


DATA: wa_mara     TYPE mara,
      wa_marc     TYPE marc,
      wa_cont     TYPE sy-tabix,
      wa_lin      TYPE sy-tabix,
      wl_0001     TYPE zfiwrt0001,
      wa_0019     TYPE zfiwrt0019,
      wa_lfa1     TYPE lfa1,
      wa_t001w    TYPE t001w,
      wa_t001     TYPE t001,
      wa_1bbranch TYPE j_1bbranch,
      wa_1bad     TYPE j_1bad,
      wa_1badt    TYPE j_1badt,
      wa_1baa     TYPE j_1baa,
      wa_indcoper TYPE zfiwrt0006-indcoper,
      wa_shipfrom TYPE lfa1-regio,
      wa_shipto   TYPE lfa1-regio,
      wl_texto    TYPE LINE OF catsxt_longtext_itab.



DATA: g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      tl_function          TYPE ui_functions,
      wl_function          LIKE tl_function WITH HEADER LINE,
      it_fcat              TYPE TABLE OF lvc_s_fcat,
      wl_fcat              TYPE lvc_s_fcat,
      wa_layout            TYPE lvc_s_layo,
      ty_toolbar           TYPE stb_button,
      wa_stable            TYPE lvc_s_stbl VALUE 'XX'.


DATA: g_grid_02               TYPE REF TO cl_gui_alv_grid,
      g_custom_container_02   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager_02 TYPE REF TO cl_alv_grid_toolbar_manager,
      tl_function_02          TYPE ui_functions,
      wl_function_02          LIKE tl_function WITH HEADER LINE,
      it_fcat_02              TYPE TABLE OF lvc_s_fcat,
      wl_fcat_02              TYPE lvc_s_fcat,
      gt_estilo_02            TYPE lvc_t_styl,
      wa_layout_02            TYPE lvc_s_layo.

DATA: tg_selectedrow TYPE lvc_t_row,
      wg_selectedrow TYPE lvc_s_row.


DATA: vl_seq_lcto TYPE zfiwrt0008-seq_lcto,
      wg_flag.
DATA: p_parvw   TYPE zfiwrt0008-parvw,
      p_parid   TYPE zfiwrt0008-parid,
      vl_nfobjn LIKE j_1binterf-nfobjn.



DATA: tela(4)   TYPE c VALUE '0101'.

SELECTION-SCREEN BEGIN OF SCREEN 0101 AS SUBSCREEN.
  SELECTION-SCREEN BEGIN OF BLOCK b1.
    SELECT-OPTIONS: p_werks   FOR zlest0061-werks NO INTERVALS NO-EXTENSION,
                    p_data    FOR zlest0061-dt_fatura.
  SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 0101.


START-OF-SELECTION.

  "PERFORM VALIDA_DOC_ENTRADA.

  CALL SCREEN 0100.

CLASS lcl_hotspot_click DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: on_hotspot_click  FOR EVENT  hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id e_row_id es_row_no sender.
ENDCLASS.

CLASS lcl_hotspot_click IMPLEMENTATION.
  METHOD on_hotspot_click.

    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
    IF sy-subrc = 0.

      CASE e_column_id.
        WHEN 'DOC_ZNFW'.
          PERFORM z_gerar_znfw USING wa_saida.

        WHEN 'DOCNUM_ENTRADA'.

          SELECT SINGLE *
           FROM zfiwrt0008 INTO @DATA(wl_0008)
          WHERE seq_lcto EQ @wa_saida-doc_znfw.

          IF sy-subrc = 0.

            IF wl_0008-docnum IS NOT INITIAL.

              CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
                EXPORTING
                  doc_number         = wl_0008-docnum
                IMPORTING
                  obj_number         = vl_nfobjn
                EXCEPTIONS
                  document_not_found = 1
                  docum_lock         = 2
                  OTHERS             = 3.

              CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
                EXPORTING
                  obj_number         = vl_nfobjn
                EXCEPTIONS
                  object_not_found   = 1
                  scr_ctrl_not_found = 2
                  OTHERS             = 3.


              PERFORM: valida_doc_entrada,
                       busca_dados,
                       tratar_dados.
            ELSE.
              SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wl_0008-seq_lcto.
              SUBMIT zwrr0004  AND RETURN.
            ENDIF.
          ELSE.
            MESSAGE 'Favor gerar o Doc ZNFW!' TYPE 'I'.
            EXIT.
          ENDIF.

        WHEN  'DOC_CONTABIL'.

          IF wa_saida-doc_contabil IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD wa_saida-doc_contabil.
            SET PARAMETER ID 'BUK' FIELD wa_saida-b_bukrs.
            SET PARAMETER ID 'GJR' FIELD wa_saida-b_gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'DOCNUM'.

          SET PARAMETER ID 'JEF' FIELD wa_saida-docnum.
          CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.
      ENDCASE.
    ENDIF.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.


  ENDMETHOD.
ENDCLASS.


CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING
                           io_alv_grid TYPE REF TO cl_gui_alv_grid,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handler_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.


CLASS lcl_alv_toolbar IMPLEMENTATION .

  METHOD constructor.
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.


  METHOD on_toolbar.
    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_storno.
    ty_toolbar-function  = 'ESTORNAR'.
    ty_toolbar-text      = 'Estornar'.
    ty_toolbar-butn_type =  0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD handler_user_command.

    CASE e_ucomm.
      WHEN 'ESTORNAR'.
        PERFORM z_estornar.
    ENDCASE.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_f4_02 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS on_f4_01 FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells sender.
ENDCLASS.

CLASS lcl_event_f4_02 IMPLEMENTATION.
  METHOD  on_f4_01.

    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE  dd03l-fieldname,
             s(1)      TYPE c,
           END OF ty_field.

    TYPES: BEGIN OF ty_value,
             taname      TYPE dd03l-tabname,
             fieldname   TYPE  dd03l-fieldname,
             char79(100) TYPE c,
           END OF ty_value.

    DATA: BEGIN OF wl_operacao,
            field(50),
          END OF wl_operacao.

    DATA: tl_operacao LIKE TABLE OF wl_operacao,
          tl_field    TYPE TABLE OF ty_field,
          wl_field    TYPE ty_field,
          tl_value    TYPE TABLE OF ty_value,
          wl_value    TYPE ty_value,
          wl_char(20),
          wl_index    TYPE sy-tabix.

    IF e_fieldname = 'OPERACAO'.

      SELECT *
        FROM zfiwrt0001  INTO TABLE @DATA(it_operacao).

      LOOP AT it_operacao INTO DATA(wa_operacao).

        MOVE: wa_operacao-operacao TO wl_operacao-field.
        APPEND wl_operacao TO tl_operacao.

        MOVE: wa_operacao-descricao TO wl_operacao-field.
        APPEND wl_operacao TO tl_operacao.
      ENDLOOP.


      wl_field-tabname = 'ZFIWRT0001'.
      wl_field-fieldname = 'OPERACAO'.
      wl_field-s = 'X'.
      APPEND wl_field TO tl_field.

      wl_field-tabname   = 'ZFIWRT0001'.
      wl_field-fieldname = 'DESCRICAO'.
      wl_field-s = ' '.
      APPEND wl_field TO tl_field.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
          fieldname                 = 'OPERACAO'
          tabname                   = 'ZFIWRT0001'
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_operacao
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      IF sy-subrc IS INITIAL.
        READ TABLE it_operacao INTO wa_operacao INDEX wl_index.
        IF es_row_no-row_id GT 0.
          READ TABLE it_saida_02 INTO wa_saida_02 INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL.
            MOVE: wa_operacao-operacao TO wa_saida_02-operacao.
            MODIFY it_saida_02 FROM wa_saida_02 INDEX es_row_no-row_id.
          ENDIF.
        ENDIF.
      ENDIF.

      CALL METHOD g_grid_02->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


FORM busca_dados.

  REFRESH: it_saida, it_zsdt0225, it_j_1bnfdoc.

  PERFORM valida_doc_entrada.


  IF p_werks IS INITIAL.
    MESSAGE 'Favor informe o Centro!' TYPE 'S'.
    EXIT.
  ELSEIF p_data-low IS INITIAL.
    MESSAGE 'Favor informe Período Inicial! ' TYPE 'S'.
    EXIT.
  ELSEIF p_data-high IS INITIAL.
    MESSAGE 'Favor informe Período final! ' TYPE 'S'.
    EXIT.
  ELSE.
    SELECT *
     FROM zsdt0225 INTO TABLE it_zsdt0225
     WHERE werks_serv IN p_werks
     AND   dt_fatura >= p_data-low
     AND   dt_fatura <= p_data-high.

    CHECK it_zsdt0225 IS NOT INITIAL.

    SELECT *
      FROM mara
      INTO TABLE t_mara
      FOR ALL ENTRIES IN it_zsdt0225
      WHERE matnr = it_zsdt0225-cod_material.
    IF sy-subrc IS INITIAL.
      SORT t_mara BY matnr.
    ENDIF.

*-IR194132-29.10.2024-#156085-JT-inicio
    SELECT *
      FROM zsdt0306_fat
      INTO TABLE it_zsdt0306_fat
       FOR ALL ENTRIES IN it_zsdt0225
     WHERE id_seq    = it_zsdt0225-id_seq
       AND dt_recreg = it_zsdt0225-dt_recreg.
*-IR194132-29.10.2024-#156085-JT-fim

    SELECT *
      FROM kna1 INTO TABLE it_kna1
       FOR ALL ENTRIES IN it_zsdt0225
    WHERE kunnr EQ it_zsdt0225-cl_codigo
      AND ktokd EQ 'ZCIC'.

    SELECT *
      FROM j_1bnfdoc INTO TABLE it_j_1bnfdoc
      FOR ALL ENTRIES IN it_zsdt0225
    WHERE docnum EQ it_zsdt0225-docnum
      AND cancel EQ ' '.

    SELECT *
      FROM j_1bbranch INTO TABLE it_bbranch
     FOR ALL ENTRIES IN it_zsdt0225
     WHERE branch EQ it_zsdt0225-cl_codigo+6(4).

    IF sy-subrc = 0.

      SELECT  *
        FROM zsdt0229 INTO TABLE it_229
        FOR ALL ENTRIES IN it_bbranch
      WHERE bukrs  EQ it_bbranch-bukrs.
**** Inicio - Rubenilson Pereira - 29.07.25 #181046
      IF sy-subrc IS INITIAL.
        SORT it_229 BY bukrs auart matkl.
      ENDIF.
**** Fim - Rubenilson Pereira - 29.07.25 #181046

    ENDIF.

  ENDIF.
ENDFORM.


FORM tratar_dados.

  LOOP AT  it_zsdt0225 INTO wa_zsdt0225.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_zsdt0225-cl_codigo.
    IF sy-subrc = 0.

      READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wa_zsdt0225-docnum.
      IF sy-subrc = 0.
        wa_saida-nf_saida = wa_j_1bnfdoc-nfnum.
        wa_saida-series   = wa_j_1bnfdoc-series.
      ENDIF.

*-IR194132-29.10.2024-#156085-JT-inicio
      CLEAR wa_zsdt0306_fat.
      READ TABLE it_zsdt0306_fat INTO wa_zsdt0306_fat WITH KEY id_seq    = wa_zsdt0225-id_seq
                                                               dt_recreg = wa_zsdt0225-dt_recreg.
*-IR194132-29.10.2024-#156085-JT-fim

      wa_saida-id_seq          =  wa_zsdt0225-id_seq.
      wa_saida-filial          =  wa_zsdt0225-cl_codigo+6(4).
      wa_saida-safra           =  wa_zsdt0225-safra.
      wa_saida-cod_material    =  wa_zsdt0225-cod_material.
      wa_saida-tp_class        =  wa_zsdt0225-tp_class.
      wa_saida-nr_dco          =  wa_zsdt0225-nr_dco.
      wa_saida-peso_vinculado  =  COND #( WHEN wa_zsdt0225-peso_vinculado IS INITIAL THEN wa_zsdt0306_fat-qtdfatur    "*-IR194132-29.10.2024-#156085-JT
                                                                                     ELSE wa_zsdt0225-peso_vinculado ).
      wa_saida-vlr_brl         =  wa_zsdt0225-vlr_brl.
      wa_saida-vlr_usd         =  wa_zsdt0225-vlr_usd.
      wa_saida-dt_fatura       =  wa_zsdt0225-dt_fatura.
      wa_saida-docnum          =  wa_zsdt0225-docnum.
      wa_saida-matnr           =  wa_zsdt0225-matnr_ov.
      wa_saida-bukrs           =  wa_zsdt0225-bukrs.
      wa_saida-werks_serv      =  wa_zsdt0225-werks_serv.
      wa_saida-operacao_225    = wa_zsdt0225-operacao. " Rubenilson Pereira - 29.07.25 #181046
      wa_saida-auart           = wa_zsdt0225-auart." Rubenilson Pereira - 29.07.25 #181046

      IF wa_zsdt0225-doc_znfw IS NOT INITIAL.
        wa_saida-doc_znfw        =  wa_zsdt0225-doc_znfw.

        SELECT SINGLE *  FROM zfiwrt0008 INTO wa_zfiwrt0008
            WHERE seq_lcto EQ wa_zsdt0225-doc_znfw
              AND loekz NE 'X'.

        IF wa_zfiwrt0008-obj_key IS NOT INITIAL.

          SELECT SINGLE *  FROM bkpf INTO wa_bkpf
            WHERE awkey EQ wa_zfiwrt0008-obj_key.

          IF sy-subrc = 0.
            wa_saida-doc_contabil = wa_bkpf-belnr.
            wa_saida-b_bukrs      = wa_bkpf-bukrs.
            wa_saida-b_gjahr      = wa_bkpf-gjahr.
          ENDIF.
        ENDIF.

      ELSE.
        wa_saida-doc_znfw        =  '@15@'.
      ENDIF.

      IF wa_zsdt0225-docnum_entrada IS NOT INITIAL.
        wa_saida-docnum_entrada  =  wa_zsdt0225-docnum_entrada.
      ELSE.
        wa_saida-docnum_entrada  =  '@15@'.
      ENDIF.

      READ TABLE it_bbranch INTO wa_bbranch WITH KEY branch = wa_zsdt0225-cl_codigo+6(4).

** Inicio - Rubenilson Pereira - 29.07.25 #181046
      READ TABLE t_mara ASSIGNING FIELD-SYMBOL(<fs_mara>)
      WITH KEY matnr = wa_zsdt0225-cod_material
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE it_229 INTO wa_229
        WITH KEY bukrs = wa_bbranch-bukrs
                 auart = wa_zsdt0225-auart
                 matkl = <fs_mara>-matkl
        BINARY SEARCH.
        IF sy-subrc = 0.
          wa_saida-operacao      =  wa_229-operacao.
        ENDIF.
      ENDIF.
** Fim - Rubenilson Pereira - 29.07.25 #181046

      APPEND wa_saida TO it_saida.
    ENDIF.

    CLEAR: wa_saida, wa_j_1bnfdoc, wa_zsdt0225, wa_229, wa_kna1, wa_zfiwrt0008, wa_bkpf.
  ENDLOOP.
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
    WHEN 'OK'.
      PERFORM busca_dados.
      PERFORM tratar_dados.
    WHEN 'PARAM_ZNFW'.
      PERFORM busca_dados_02.
      PERFORM alv_02.
      CALL SCREEN 0102.
    WHEN 'REFRESH'.
      PERFORM busca_dados.
      PERFORM tratar_dados.
  ENDCASE.
ENDMODULE.

FORM alv.
  CLEAR wl_fcat.
  REFRESH it_fcat[].

  PERFORM preenche_cat USING :
        'ID_SEQ'              'ID Seq'                '10'    ''    ''     ''     ''    ''    '',
        'FILIAL'              'Filial'                '07'    ''    ''     ''     ''    ''    '',
        'SAFRA'               'Safra'                 '05'    ''    ''     ''     ''    ''    '',
        'WERKS_SERV'          'Centro Serviço'        '12'    ''    ''     ''     ''    ''    '',
        'COD_MATERIAL'        'Material'              '10'    'X'   ''     ''     ''    ''    '',
        'TP_CLASS'            'Class.Mat'             '10'    ''    ''     ''     ''    ''    '',
        'NR_DCO'              'Nr.Dco'                '10'    ''    ''     ''     ''    ''    '',
        'PESO_VINCULADO '     'Peso Vinc.'            '13'    ''    ''     ''     ''    ''    '',
        'VLR_BRL'             'Vlr BRL'               '13'    ''    ''     ''     ''    ''    '',
        'VLR_USD'             'Vlr USD'               '13'    ''    ''     ''     ''    ''    '',
        'DT_FATURA'           'Dt Fatura'             '10'    ''    ''     ''     ''    ''    '',
        'AUART'               'Tipo OV'               '12'    ''    ''     ''     ''    ''    ''," Rubenilson Pereira - 29.07.25 #181046
        'OPERACAO_225'        'Operação'              '12'    ''    ''     ''     ''    ''    ''," Rubenilson Pereira - 29.07.25 #181046
        'DOCNUM'              'Docnum Saída'          '12'    ''    'X'     ''     ''    ''    '',
        'NF_SAIDA'            'NFPS'                  '10'    ''    ''     ''     ''    ''    '',
        'SERIES'              'Serie'                 '06'    ''    ''     ''     ''    ''    '',
        'DOC_ZNFW'            'Doc ZNFW'              '10'    ''    'X'    ''     ''    ''    'X',
        'DOCNUM_ENTRADA'      'Docnum Entrada'        '14'    ''    'X'    ''     ''    ''    'X',
        'DOC_CONTABIL'        'Doc.Contabil'          '12'    ''    'X'    ''     ''    ''    ''.

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

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: obg_toolbar TYPE REF TO lcl_alv_toolbar,
        ls_variant  TYPE disvariant.

  SET PF-STATUS 'ST_0100'.
  SET TITLEBAR 'TL_0100'.

  PERFORM alv.

  IF g_custom_container IS INITIAL.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER01'
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

    IF obg_toolbar IS INITIAL.
      CREATE OBJECT obg_toolbar
        EXPORTING
          io_alv_grid = g_grid.
    ENDIF.

    SET HANDLER: obg_toolbar->on_toolbar FOR g_grid,
                 obg_toolbar->handler_user_command FOR g_grid.

    SET HANDLER: lcl_hotspot_click=>on_hotspot_click FOR g_grid.

    ls_variant-report = sy-repid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        i_save                        = 'X'
        is_variant                    = ls_variant
        it_toolbar_excluding          = tl_function
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = it_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
*
*    CALL METHOD g_grid->set_ready_for_input
*      EXPORTING
*        i_ready_for_input = 1.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.
  DATA: lt_f402 TYPE lvc_t_f4,
        wl_f402 TYPE lvc_s_f4.

  SET PF-STATUS 'ST_0102'.
  SET TITLEBAR 'TL_0102'.

  IF g_custom_container_02 IS INITIAL.

    CREATE OBJECT g_custom_container_02
      EXPORTING
        container_name              = 'CONTAINER02'
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

    wl_f402-fieldname = 'OPERACAO'.
    wl_f402-register   = 'X'.
    wl_f402-getbefore  = 'X'.
    APPEND wl_f402 TO  lt_f402.
    CLEAR wl_f402.


    SET HANDLER lcl_event_f4_02=>on_f4_01 FOR g_grid_02.

    CALL METHOD g_grid_02->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout_02
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function_02
      CHANGING
        it_outtab                     = it_saida_02[]
        it_fieldcatalog               = it_fcat_02
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD g_grid_02->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD g_grid_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    CALL METHOD g_grid_02->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f402[].

  ELSE.

    CALL METHOD g_grid_02->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.


FORM alv_02.
  CLEAR wl_fcat_02.
  REFRESH it_fcat_02[].

  PERFORM preenche_cat_02 USING :
        'BUKRS'          'Emp.Tomadora'            '10'    'X'    ''    ''    ''     ''     ''    ''     'T001'    'BUKRS'   '',
        'BUTXT'          'Descrição'               '30'    ''     ''    ''    ''     ''     ''    ''     ''        ''        '',
        'MATKL'          'Grp. Material'           '12'    'X'    ''    ''    ''     ''     ''    ''     'MARA'    'MATKL'   '',
        'AUART'          'Tipo OV'                 '12'    'X'    ''    ''    ''     ''     ''    ''     'VBAK'    'AUART'   '',
        'OPERACAO'       'Cód.Operação'            '12'    'X'    ''    ''    ''     ''     ''    ''     ''        ''        'X',
        'DESCRICAO'      'Descrição'               '35'    ''     ''    ''    ''     ''     ''    ''     ''        ''        ''.

ENDFORM.

FORM preenche_cat_02 USING  VALUE(p_campo)
                            VALUE(p_desc)
                            VALUE(p_tam)
                            VALUE(p_edit)
                            VALUE(p_zero)
                            VALUE(p_hot)
                            VALUE(p_sum)
                            VALUE(p_just)
                            VALUE(p_cor)
                            VALUE(p_icon)
                            VALUE(p_table)
                            VALUE(p_field)
                            VALUE(p_f4).

  wl_fcat_02-fieldname = p_campo.
  wl_fcat_02-scrtext_l = p_desc.
  wl_fcat_02-scrtext_m = p_desc.
  wl_fcat_02-scrtext_s = p_desc.
  wl_fcat_02-outputlen = p_tam.
  wl_fcat_02-edit      = p_edit.
  wl_fcat_02-hotspot   = p_hot.
  wl_fcat_02-no_zero   = p_zero.
  wl_fcat_02-do_sum    = p_sum.
  wl_fcat_02-just      = p_just.
  wl_fcat_02-icon      = p_icon.
  wl_fcat_02-emphasize = p_cor.
  wl_fcat_02-ref_table = p_table.
  wl_fcat_02-ref_field = p_field.
  wl_fcat_02-f4availabl = p_f4.

  APPEND wl_fcat_02 TO  it_fcat_02.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.
  DATA: it_salvar TYPE TABLE OF zsdt0229,
        wa_salvar TYPE zsdt0229.

  CASE sy-ucomm.
    WHEN 'SALVE'.
      LOOP AT it_saida_02 INTO wa_saida_02.

        SELECT SINGLE * FROM zsdt0229 INTO wa_229
          WHERE bukrs     EQ wa_saida_02-bukrs
          AND   operacao  EQ wa_saida_02-operacao
          AND   matkl     EQ wa_saida_02-matkl
          AND   auart     EQ wa_saida_02-auart.

        IF sy-subrc <> 0.

          SELECT SINGLE * FROM zfiwrt0001 INTO @DATA(wa_zfiwrt1)
            WHERE  operacao EQ @wa_saida_02-operacao.

          IF sy-subrc = 0.
            wa_salvar-mandt           = sy-mandt.
            wa_salvar-bukrs           = wa_saida_02-bukrs.
            wa_salvar-operacao        = wa_saida_02-operacao.
            wa_salvar-matkl           = wa_saida_02-matkl. " Rubenilson Pereira - 29.07.25 #181046
            wa_salvar-auart           = wa_saida_02-auart. " Rubenilson Pereira - 29.07.25 #181046
            wa_salvar-dt_ult_mod      = sy-datum.
            wa_salvar-hr_ult_mod      = sy-uzeit.
            wa_salvar-usuario_ult_mod = sy-uname.

            APPEND wa_salvar TO it_salvar.
          ELSE.
            REFRESH it_salvar.
            MESSAGE 'Código Operação não existe!' TYPE 'S'.
            EXIT.
          ENDIF.
        ENDIF.
        CLEAR: wa_salvar, wa_saida_02, wa_229.
      ENDLOOP.

      CHECK it_salvar IS NOT INITIAL.

      INSERT zsdt0229 FROM TABLE it_salvar.
      COMMIT WORK.

      MESSAGE 'Registro gravado com sucesso!' TYPE 'S'.

      REFRESH: it_salvar, it_saida_02.

      PERFORM busca_dados_02.

      CALL METHOD g_grid_02->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN '&INS'.
      APPEND INITIAL LINE TO it_saida_02.
    WHEN '&DEL'.

      CALL METHOD g_grid_02->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF tg_selectedrow[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!'  TYPE 'S'.
        EXIT.
      ELSE.

        LOOP AT tg_selectedrow INTO wg_selectedrow.
          READ TABLE it_saida_02 INTO wa_saida_02 INDEX wg_selectedrow-index.

          DELETE FROM zsdt0229 WHERE bukrs   = wa_saida_02-bukrs
                                AND operacao = wa_saida_02-operacao
                                AND matkl    = wa_saida_02-matkl" Rubenilson Pereira - 29.07.25 #181046
                                AND auart    = wa_saida_02-auart." Rubenilson Pereira - 29.07.25 #181046
          CLEAR: wa_saida_02.
        ENDLOOP.

        REFRESH it_saida_02.
        CLEAR wa_saida_02.

        PERFORM busca_dados_02.

        CALL METHOD g_grid_02->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      ENDIF.

    WHEN 'BACK'.
      REFRESH it_saida_02.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados_02 .

  SELECT * FROM zsdt0229  INTO TABLE it_zsdt0229.

  CHECK it_zsdt0229 IS NOT INITIAL.

  SELECT * FROM t001 INTO TABLE @DATA(it_t001)
    FOR ALL ENTRIES IN @it_zsdt0229
   WHERE bukrs EQ @it_zsdt0229-bukrs.

  SELECT * FROM zfiwrt0001 INTO TABLE it_zfiwrt0001
    FOR ALL ENTRIES IN it_zsdt0229
   WHERE operacao EQ it_zsdt0229-operacao.


  LOOP AT  it_zsdt0229 INTO wa_zsdt0229.

    wa_saida_02-bukrs      = wa_zsdt0229-bukrs.
    wa_saida_02-operacao   = wa_zsdt0229-operacao.
    wa_saida_02-matkl      = wa_zsdt0229-matkl. " Rubenilson Pereira - 29.07.25 #181046
    wa_saida_02-auart      = wa_zsdt0229-auart. " Rubenilson Pereira - 29.07.25 #181046

    READ TABLE it_t001 INTO DATA(wa_t001) WITH KEY bukrs = wa_zsdt0229-bukrs.
    IF sy-subrc = 0.
      wa_saida_02-butxt = wa_t001-butxt.
    ENDIF.

    READ TABLE it_zfiwrt0001 INTO wa_zfiwrt0001 WITH KEY operacao = wa_zsdt0229-operacao.
    IF sy-subrc = 0.
      wa_saida_02-descricao = wa_zfiwrt0001-descricao.
    ENDIF.

    FREE wa_saida_02-celltab.
    gt_estilo_02 =  VALUE #( ( fieldname = 'BUKRS'      style = cl_gui_alv_grid=>mc_style_disabled  )
                             ( fieldname = 'OPERACAO'   style = cl_gui_alv_grid=>mc_style_disabled  )
                             ( fieldname = 'MATKL'   style = cl_gui_alv_grid=>mc_style_disabled  )
                             ( fieldname = 'AUART'   style = cl_gui_alv_grid=>mc_style_disabled  )    ).
    INSERT LINES OF gt_estilo_02 INTO TABLE wa_saida_02-celltab.

    APPEND wa_saida_02 TO it_saida_02.
    CLEAR: wa_saida_02, wa_zsdt0229, wa_zfiwrt0001.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_GERAR_ZNFW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_gerar_znfw USING p_saida TYPE ty_saida.
  DATA: lv_refer TYPE zfiwrt0008-ch_referencia.

  IF p_saida-doc_znfw = '@15@'.
    lv_refer = p_saida-id_seq && p_saida-docnum.

    SELECT SINGLE * FROM  zfiwrt0008 INTO @DATA(w_0008)
      WHERE ch_referencia   EQ @lv_refer
        AND loekz           NE 'X'
        AND docs_estornados NE 'X'.

    IF sy-subrc = 0.
      MESSAGE i000(z01) WITH 'Já existe nota de remessa '  w_0008-ch_referencia.
      EXIT.
    ELSEIF p_saida-nf_saida EQ '000000'.
      MESSAGE i000(z01) WITH 'Favor informar a NFPS p/ o documento  ' p_saida-docnum ' na transação J1B2N!' .
      EXIT.
    ELSE.
      CLEAR vl_seq_lcto.
      PERFORM z_nota_remessa USING p_saida  CHANGING vl_seq_lcto.
    ENDIF.
  ELSE.
    SET PARAMETER ID 'SEQ' FIELD  p_saida-doc_znfw.
    CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.

FORM z_nota_remessa USING t_saida TYPE ty_saida
                    CHANGING t_seq_lcto TYPE zfiwrt0008-seq_lcto.

  DATA: wl_0008 TYPE zfiwrt0008,
        wl_0009 TYPE zfiwrt0009,
        wl_0023 TYPE zfiwrt0023.

* ---> S4 Migration - 18/07/2023 - CA
  DATA: lt_returns         TYPE TABLE OF bapiret2,
        ls_coeldes         TYPE bapi1030_ceoutputlist,
        ls_cskb            LIKE LINE OF it_cskb,
        lv_controllingarea TYPE  bapi1030_gen-co_area,
        lv_costelement     TYPE  bapi1030_gen-cost_elem,
        lv_keydate         TYPE  bapi1030_gen-some_date.
* <--- S4 Migration - 18/07/2023 - CA

  CLEAR:  p_parid, p_parvw.


  SELECT SINGLE * FROM zsdt0229 INTO @DATA(w229)
    WHERE operacao EQ @t_saida-operacao.

  IF sy-subrc <> 0.
    MESSAGE 'Não Existe operação Nota Writer Cadastrada!' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM zfiwrt0001 INTO @DATA(wl_0001)
    WHERE operacao EQ @t_saida-operacao.

  IF sy-subrc <> 0.
    MESSAGE 'Não Existe operação Nota Writer Cadastrada!' TYPE 'I'.
    EXIT.
  ENDIF.

  p_parid = |{ t_saida-werks_serv ALPHA = IN }|.
  p_parvw   = wl_0001-parvw.


  SELECT * FROM zfiwrt0006 INTO TABLE @DATA(tl_0006)
    WHERE operacao EQ @t_saida-operacao.

  SELECT SINGLE *  FROM t001w  INTO wa_t001w WHERE werks EQ t_saida-filial.

  IF wl_0001-parvw EQ 'AG'.
    SELECT SINGLE * FROM kna1 INTO @DATA(wa_kna1)
      WHERE kunnr EQ @p_parid.

  ELSEIF wl_0001-parvw EQ 'BR' OR wl_0001-parvw EQ 'LF'.
    SELECT SINGLE * FROM lfa1 INTO @DATA(wa_lfa1)
      WHERE lifnr EQ @p_parid.
  ENDIF.


  IF wl_0001-parvw EQ 'AG'.
    IF wa_kna1-regio EQ wa_t001w-regio.
      wa_indcoper = 'D'.
    ELSE.
      wa_indcoper = 'F'.
    ENDIF.
  ELSEIF wl_0001-parvw EQ 'BR' OR wl_0001-parvw EQ 'LF'.
    IF wa_lfa1-regio EQ wa_t001w-regio.
      wa_indcoper = 'D'.
    ELSE.
      wa_indcoper = 'F'.
    ENDIF.
  ENDIF.

  CLEAR wl_0008.

  READ TABLE tl_0006 INTO DATA(wl_0006) WITH KEY indcoper = wa_indcoper.

  SELECT SINGLE * FROM j_1bbranch INTO @DATA(w1bbranch)
    WHERE branch EQ @t_saida-filial.

  SELECT  *
   FROM zfiwrt0003 INTO TABLE @DATA(it_zfiwrt0003)
     WHERE operacao EQ @t_saida-operacao.

  SELECT SINGLE * FROM tka02 INTO @DATA(wl_tka02)
    WHERE bukrs EQ @w1bbranch-bukrs.

* ---> S4 Migration - 18/07/2023 - CA
*  SELECT * FROM cskb INTO TABLE @DATA(it_cskb)
*    FOR ALL ENTRIES IN @it_zfiwrt0003
*    WHERE  kokrs  EQ @wl_tka02-kokrs
*      AND  kstar  EQ @it_zfiwrt0003-hkont
*      AND  datab  LE @sy-datum
*      AND  datbi  GE @sy-datum.

  LOOP AT it_zfiwrt0003 INTO DATA(ls_0003).

    lv_controllingarea  = wl_tka02-kokrs.
    lv_costelement      = ls_0003-hkont.
    lv_keydate          = sy-datum.

    CLEAR: lt_returns[], ls_coeldes.

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
    IF sy-subrc <> 0.
      ls_cskb-kokrs = wl_tka02-kokrs.
      ls_cskb-kstar = ls_0003-hkont.
      ls_cskb-katyp = ls_coeldes-celem_category.

      APPEND ls_cskb TO it_cskb.
      CLEAR ls_cskb.
    ENDIF.

    CLEAR: ls_0003.
  ENDLOOP.

  IF it_cskb[] IS NOT INITIAL.
*  IF SY-SUBRC = 0.
* <--- S4 Migration - 18/07/2023 - CA

*** Inicio - Rubenilson Pereira - 29.07.25 #181046
    SELECT SINGLE matkl
      FROM mara
      INTO @DATA(lv_matkl)
      WHERE matnr = @t_saida-cod_material.
    IF sy-subrc IS INITIAL.
*** Fim - Rubenilson Pereira - 29.07.25 #181046
      SELECT SINGLE * FROM zcot0011 INTO  @DATA(wl_zcot0011)
     WHERE  bukrs EQ @w1bbranch-bukrs
     AND    werks EQ @t_saida-filial
     AND    matkl EQ @lv_matkl." Rubenilson Pereira - 29.07.25 #181046
    ENDIF.

    SELECT SINGLE * FROM csks INTO @DATA(wcsks)
      WHERE kostl EQ @wl_zcot0011-kostl
       AND  bukrs EQ @w1bbranch-bukrs
       AND  gsber EQ @t_saida-filial.

    IF  wcsks-bkzkp EQ 'X'.
      MESSAGE 'Centro de custo bloqueado p/ Lançamentos  !' TYPE 'I'.
      EXIT.
    ELSE.
      IF wl_zcot0011 IS NOT INITIAL.
        wl_0023-perc   = '100'.
        wl_0023-kostl  = wl_zcot0011-kostl.
      ELSE.
        MESSAGE 'Falta parâmetro na transação ZCO0029. Solicite Depto. Orçamentos e Custos.' TYPE 'I'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = t_saida-docnum.

  wl_0008-operacao          =  t_saida-operacao.
  wl_0008-bukrs             =  w1bbranch-bukrs.
  wl_0008-branch            =  t_saida-filial.
  wl_0008-nfenum            =  t_saida-nf_saida.
  wl_0008-series            =  t_saida-series.
  wl_0008-ch_referencia     =  t_saida-id_seq.
  wl_0008-parid             =  p_parid.
  wl_0008-parvw             =  p_parvw.
  wl_0008-nftype            =  wl_0001-nftype.
  wl_0008-ctrl_zrfl         =  wl_0001-ctrl_zrfl.
  wl_0008-zpesagem          =  wl_0001-zpesagem.
  wl_0008-dias              =  wl_0001-dias.
  wl_0008-retorno           =  wl_0001-retorno.
  wl_0008-energia           =  wl_0001-energia.
  wl_0008-servico           =  wl_0001-servico.
  wl_0008-complemento       =  wl_0001-complemento.
  wl_0008-referencia        =  wl_0001-referencia.
  wl_0008-budat             =  wa_saida-dt_fatura.
  wl_0008-bldat             =  wa_saida-dt_fatura.
  wl_0008-usuario_ult_mod   =  sy-uname.
  wl_0008-dt_ult_mod        =  sy-datum.
  wl_0008-hr_ult_mod        =  sy-uzeit.
  wl_0008-inco1             =  'SRV'.
  wl_0008-inco2             =  'Serviços'.
  wl_0008-cfop              =  wl_0006-cfop.
  wl_0008-taxlw1            =  wl_0006-taxlw1.
  wl_0008-taxlw2            =  wl_0006-taxlw2.
  wl_0008-taxlw4            =  wl_0006-taxlw4.
  wl_0008-taxlw5            =  wl_0006-taxlw5.
  wl_0008-opertyp           =  wl_0006-opertyp.
  wl_0008-taxcode           =  wl_0006-taxcode.
  wl_0008-zlsch             = 'U'.
  wl_0008-tcode_org         = sy-tcode.
  wl_0008-not_check_xml     = abap_true.

  PERFORM forma_pgto USING wa_j_1bnfdoc-zterm
                           wa_j_1bnfdoc-docdat   CHANGING   wl_0008-zfbdt.

  CLEAR wl_0009.

  SELECT SINGLE *
    FROM mara INTO @DATA(wl_mara)
   WHERE matnr EQ @t_saida-matnr.

  SELECT SINGLE * FROM marc INTO @DATA(wa_marc)
    WHERE matnr EQ @t_saida-matnr.

  wl_0009-itmnum  =  10.
  wl_0009-matnr   =  t_saida-matnr.
  wl_0009-bwkey   =  t_saida-filial.
  wl_0009-cfop    =  wl_0006-cfop.
  wl_0009-menge   =  '1'.

  IF  wl_mara-mtart = 'ZDIE'.
    wl_0009-meins   = wl_mara-meins.
  ELSE.
    wl_0009-meins   =  wa_marc-ausme.
  ENDIF.

  wl_0009-netpr   =  t_saida-vlr_brl.
*---> 15/06/2023 - Migração S4 - JS
*   WL_0009-NETWR   =  T_SAIDA-VLR_BRL.
  wl_0009-netwr = CONV #( t_saida-vlr_brl ).
*<--- 15/06/2023 - Migração S4 - JS
  wl_0009-itmtyp  =  wl_0001-itmtyp.


  TRY.
      zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento( )->set_cabecalho( i_cabecalho =  wl_0008 )->add_item( i_item = wl_0009 )->set_rateio( i_rateio = wl_0023 ).

      zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = t_seq_lcto ).

      IF t_seq_lcto IS NOT INITIAL.

        MESSAGE |Lançamento { t_seq_lcto } gerado com sucesso!| TYPE 'S'.

        UPDATE zsdt0225 SET doc_znfw = t_seq_lcto
        WHERE id_seq EQ t_saida-id_seq
          AND docnum EQ t_saida-docnum.

        REFRESH it_saida.

        PERFORM busca_dados.
        PERFORM tratar_dados.

        CALL METHOD g_grid->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      ELSE.
        MESSAGE |Houve um erro ao gravar o lançamento!| TYPE 'S'.
      ENDIF.

    CATCH zcx_nf_writer INTO DATA(zcx_nf_writer).
      zcx_nf_writer->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

FORM valida_doc_entrada.

  SELECT *
   FROM zsdt0225 INTO TABLE it_zsdt0225
   WHERE doc_znfw       NE ' '
    AND  docnum_entrada EQ ' '.

  CHECK it_zsdt0225 IS NOT INITIAL.

  SELECT  *
   FROM zfiwrt0008 INTO TABLE tl_0008
   FOR ALL ENTRIES IN it_zsdt0225
  WHERE seq_lcto EQ it_zsdt0225-doc_znfw
    AND docnum   NE ' '.

  LOOP AT tl_0008 INTO wa_0008.
    UPDATE zsdt0225 SET docnum_entrada = wa_0008-docnum
    WHERE  doc_znfw EQ wa_0008-seq_lcto.
    COMMIT WORK.

    CLEAR: wa_0008.
  ENDLOOP.

  REFRESH: it_zsdt0225, tl_0008.
ENDFORM.



FORM forma_pgto USING i_zterm
                      i_dt_doc TYPE j_1bnfdoc-docdat CHANGING r_dt_vencimento TYPE zfiwrt0008-zfbdt.


  DATA: tag1          TYPE bseg-zbd1t, " help variables
        tag2          TYPE bseg-zbd2t, " for due date
        tag3          TYPE bseg-zbd3t,
        help_due_date TYPE bseg-zfbdt.

  SELECT SINGLE * INTO @DATA(wa_t052)
    FROM t052
   WHERE zterm EQ @i_zterm.

  IF sy-subrc = 0.
    CASE wa_t052-zdart.
      WHEN 'C'.
        r_dt_vencimento = i_dt_doc.
      WHEN 'D'.
        r_dt_vencimento = i_dt_doc.
      WHEN 'B'.
        r_dt_vencimento = i_dt_doc.
      WHEN OTHERS.
        r_dt_vencimento = i_dt_doc.
    ENDCASE.

    IF NOT wa_t052-zfael IS INITIAL.
      CONCATENATE r_dt_vencimento(6) wa_t052-zfael INTO r_dt_vencimento.
    ENDIF.

    IF NOT wa_t052-zmona IS INITIAL.
      CALL FUNCTION 'MONTH_PLUS_DETERMINE'
        EXPORTING
          months  = wa_t052-zmona
          olddate = r_dt_vencimento
        IMPORTING
          newdate = r_dt_vencimento.
    ENDIF.

    tag1 = wa_t052-ztag1.
    tag2 = wa_t052-ztag2.
    tag3 = wa_t052-ztag3.

    CALL FUNCTION 'J_1B_FI_NETDUE'
      EXPORTING
        zfbdt   = r_dt_vencimento
        zbd1t   = tag1
        zbd2t   = tag2
        zbd3t   = tag3
        zstg1   = wa_t052-zstg1
        zsmn1   = wa_t052-zsmn1
        zstg2   = wa_t052-zstg2
        zsmn2   = wa_t052-zsmn2
        zstg3   = wa_t052-zstg3
        zsmn3   = wa_t052-zsmn3
      IMPORTING
        duedate = help_due_date
      EXCEPTIONS
        OTHERS  = 1.

    IF sy-subrc = 0.
      r_dt_vencimento  = help_due_date.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ESTORNAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_estornar .

  DATA wl_answer.
  DATA wg_ativo.
  DATA tl_docs        TYPE TABLE OF zfiwrs0003.
  DATA wl_cont        TYPE sy-tabix.
  DATA: lv_refer      TYPE zfiwrt0008-ch_referencia.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow[] IS INITIAL.
    MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
    EXIT.
  ELSE.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = 'Tem certeza que deseja processar esse(s) documento(s)?'
      IMPORTING
        answer         = wl_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CASE wl_answer.
      WHEN '2' OR 'A'.
        EXIT.
    ENDCASE.

    LOOP AT tg_selectedrow INTO wg_selectedrow.

      READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.

      lv_refer = wa_saida-id_seq && wa_saida-docnum.

      SELECT COUNT(*)
        FROM zfiwrt0008
        WHERE seq_lcto EQ wa_saida-doc_znfw
          AND ch_referencia = lv_refer " Rubenilson Pereira - 04.08.25
          AND status NOT IN ('E', 'P' ).

      IF sy-subrc IS INITIAL.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH |Documento não pode ser estornado { wa_saida-doc_znfw }!|.
        EXIT.
      ENDIF.

      SELECT SINGLE * FROM zsdt0225 INTO @DATA(wa_225)
        WHERE id_seq EQ @wa_saida-id_seq
          AND docnum EQ @wa_saida-docnum." Rubenilson Pereira - 04.08.25

      IF  wa_225-lote IS NOT INITIAL.

        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Solicitar o estorno da apropriação depto de custos!'.
        EXIT.

      ELSE.
        IF wa_saida-docnum_entrada NE '@15@'.

          CALL FUNCTION 'ZNFW_ESTORNA_SEQ_LCTO'
            EXPORTING
              i_seq_lcto = wa_saida-doc_znfw
              i_estorno  = 'X'
            TABLES
              t_docs     = tl_docs.

        ELSE.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Os documentos que estão aguardando aprovação, não'
                                                 'serão processados!'.
        ENDIF.

        IF wa_saida-doc_znfw NE '@15@'.

          CALL FUNCTION 'ENQUEUE_EZFIWRT0008'
            EXPORTING
              seq_lcto       = wa_saida-doc_znfw
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.

          IF sy-subrc IS INITIAL.

            UPDATE zfiwrt0008  SET ch_referencia   = ' '
                                   docs_estornados = 'X'
              WHERE seq_lcto EQ wa_saida-doc_znfw
                AND ch_referencia = lv_refer." Rubenilson Pereira - 04.08.25
            COMMIT WORK.

            UPDATE zsdt0225 SET doc_znfw       = ' '
                                docnum_entrada = ' '
                   WHERE id_seq EQ wa_saida-id_seq
                    AND  docnum EQ wa_saida-docnum." Rubenilson Pereira - 04.08.25
            COMMIT WORK.

          ELSE.

            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

          ENDIF.

          CALL FUNCTION 'DEQUEUE_EZFIWRT0008'
            EXPORTING
              seq_lcto = wa_saida-doc_znfw.

        ENDIF.

        ADD 1 TO wl_cont.

      ENDIF.

    ENDLOOP.

    IF wl_cont GT 0.
      MESSAGE s836(sd) WITH 'Os documentos foram marcados para estorno!'.
    ENDIF.

    REFRESH it_saida.

    PERFORM busca_dados.
    PERFORM tratar_dados.
  ENDIF.

ENDFORM.
