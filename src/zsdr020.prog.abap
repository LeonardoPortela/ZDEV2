*&---------------------------------------------------------------------*
*& Report  ZSDR020
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr020.


TABLES: zsdt0132, zsdt0207, kna1, zsdt0216, zsdt0217, zsdt0299,
        zsdt0197. " Rubenilson Pereira - 10.04.25 #168932.

**********************************************************************
* field symbols
**********************************************************************
FIELD-SYMBOLS: <fs_fld> TYPE any.


TYPES: BEGIN OF ty_saida_01.
         INCLUDE TYPE zsde0408. "*-US191683-25.09.2025-#191683-JT-inicio
*         nr_rot         TYPE zsdt0132-nr_rot,
*         kunnr          TYPE zsdt0132-kunnr,
*         name1          TYPE kna1-name1,
*         id_propriedade TYPE zsdt0207-id_propriedade,
*         rot_desc       TYPE zsdt0132-rot_desc,
*         tel_number     TYPE zsdt0132-tel_number,
*         city1          TYPE zsdt0132-city1,
*         uf             TYPE zsdt0132-uf,
*         texto01        TYPE char5,
*         style2         TYPE lvc_t_styl,
*         nome           TYPE zsdt0207-nome,
*         municipio      TYPE char50,
*         via_acesso     TYPE zsdt0207-via_acesso,
*         texto02        TYPE char5,
*         celltab        TYPE lvc_t_styl,
*         status         TYPE zsdt0132-status,
*         color          TYPE char4,
TYPES: END OF ty_saida_01.

TYPES: BEGIN OF ty_saida_02,
         matnr              TYPE mara-matnr, "makt-matnr,
         maktx              TYPE makt-maktx,
         matkl              TYPE mara-matkl,
         id_produto         TYPE zsdt0201-id_produto,
         codmapa            TYPE zsdt0201-codmapa,    "*-CS2021000218-06.09.2022-#89493-JT-inicio
         descricao          TYPE zsdt0201-nome,
         fabricante         TYPE zsdt0201-fabricante,
         embalagem          TYPE zsdt0201-embalagem,
         tipoembalagem      TYPE zsdt0201-tipoembalagem,
         volume             TYPE zsdt0201-volume,
         unidade            TYPE zsdt0201-unidade,
         usname             TYPE zsdt0201-usname,
         data_atual         TYPE zsdt0201-data_atual,
         hora_atual         TYPE zsdt0201-hora_atual,
         id_especie         TYPE zsdt0197-id_especie,
         nomecientifico     TYPE zsdt0197-nomecientifico,
         nomecomum          TYPE zsdt0197-nomecomum,
         produtoid          TYPE zsdt0299-produtoid,     "*-CS2021000218-06.09.2022-#89493-JT-inicio
         numeroderisco      TYPE zsdt0299-numeroderisco, "*-CS2021000218-06.09.2022-#89493-JT-inicio
         registroministerio TYPE zsdt0299-registroministerio, "*-CS2021000218-06.09.2022-#89493-JT-inicio
         descricao2         TYPE zsdt0299-descricao,     "*-CS2021000218-06.09.2022-#89493-JT-inicio
         check              TYPE char1,
         editar             TYPE char1,
         celltab            TYPE lvc_t_styl,
         cellcolor          TYPE lvc_t_scol, ""*-CS2021000218-06.09.2022-#89493-JT-inicio
       END OF ty_saida_02.

TYPES: BEGIN OF ty_saida_03,
         bukrs   TYPE t001-bukrs,
         butxt   TYPE t001-butxt,
         lifnr   TYPE lfa1-lifnr,
         name1   TYPE lfa1-name1,
         stcd1   TYPE lfa1-stcd1,
         ort01   TYPE lfa1-ort01,
         stras   TYPE lfa1-stras,
         regio   TYPE lfa1-regio,
         celltab TYPE lvc_t_styl,
       END OF    ty_saida_03.

TYPES: BEGIN OF ty_saida_04,
         tipo            TYPE zsdt0216-tipo,
         text            TYPE char20,
         setor_atividade TYPE zsdt0216-setor_atividade,
         text01          TYPE char20,
         revenda         TYPE zsdt0216-revenda,
         text02          TYPE char20,
         bukrs           TYPE t001-bukrs,
         butxt           TYPE t001-butxt,
         kunnr           TYPE kna1-kunnr,
         name1           TYPE kna1-name1,
         stcd1           TYPE kna1-stcd1,
         ort01           TYPE kna1-ort01,
         stras           TYPE kna1-stras,
         regio           TYPE kna1-regio,
         renasem         TYPE zsdt0216-renasem,
         celltab         TYPE lvc_t_styl,
       END OF   ty_saida_04.

TYPES: BEGIN OF ty_saida_05,
         busca_indea TYPE zsdt0217-busca_indea,
         nr_cad      TYPE zsdt0217-nr_cad,
         nr_reg      TYPE zsdt0217-nr_reg,
         marca       TYPE zsdt0217-marca,
         ing_ativo   TYPE zsdt0217-ing_ativo,
         classe      TYPE zsdt0217-classe,
         registrante TYPE zsdt0217-registrante,
         validade    TYPE zsdt0217-validade,
         celltab     TYPE lvc_t_styl,
         color       TYPE char4,
       END OF ty_saida_05.


TYPES: BEGIN OF ty_arq_mapa,
         nr_cad      TYPE string,
         nr_reg      TYPE string,
         marca       TYPE string,
         ing_ativo   TYPE string,
         classe      TYPE string,
         registrante TYPE string,
         validade    TYPE string,
       END OF ty_arq_mapa.

TYPES: BEGIN OF ty_arq_material,
         matnr      TYPE mara-matnr,
         id_produto TYPE char10, "zsdt0201-id_produto,
         produtoid  TYPE zsdt0299-produtoid,     "*-CS2021000218-06.09.2022-#89493-JT-inicio
       END OF        ty_arq_material.

TYPES: BEGIN OF ty_arq_roteiro,
         kunnr          TYPE zsdt0132-kunnr,
         nr_rot         TYPE zsdt0132-nr_rot,
         id_propriedade TYPE zsdt0132-id_propriedade,
       END OF ty_arq_roteiro.


DATA: it_saida_01     TYPE TABLE OF ty_saida_01,
      wa_saida_01     TYPE ty_saida_01,
      it_saida_02     TYPE TABLE OF ty_saida_02,
      wa_saida_02     TYPE ty_saida_02,
      it_saida_03     TYPE TABLE OF ty_saida_03,
      wa_saida_03     TYPE ty_saida_03,
      it_saida_04     TYPE TABLE OF ty_saida_04,
      wa_saida_04     TYPE ty_saida_04,
      it_saida_05     TYPE TABLE OF ty_saida_05,
      wa_saida_05     TYPE ty_saida_05,
      it_arq_mapa     TYPE TABLE OF  ty_arq_mapa,
      wa_arq_mapa     TYPE ty_arq_mapa,
      it_arq_material TYPE TABLE OF ty_arq_material,
      wa_arq_material TYPE ty_arq_material,
      it_arq_roteiro  TYPE TABLE OF ty_arq_roteiro,
      wa_arq_roteiro  TYPE ty_arq_roteiro,
      it_zsdt0132     TYPE TABLE OF zsdt0132,
      wa_zsdt0132     TYPE zsdt0132,
      it_zsdt0207     TYPE TABLE OF zsdt0207,
      wa_zsdt0207     TYPE zsdt0207,
      it_zsdt0211     TYPE TABLE OF zsdt0211,
      wa_zsdt0211     TYPE zsdt0211,
      it_zsdt0210     TYPE TABLE OF zsdt0210,
      wa_zsdt0210     TYPE zsdt0210,
      it_zsdt0300     TYPE TABLE OF zsdt0300,
      wa_zsdt0300     TYPE zsdt0300,
      it_zsdt0197     TYPE TABLE OF zsdt0197,
      wa_zsdt0197     TYPE zsdt0197,
      it_zsdt0198     TYPE TABLE OF  zsdt0198,
      wa_zsdt0198     TYPE  zsdt0198,
      it_zsdt0201     TYPE TABLE OF zsdt0201,
      wa_zsdt0201     TYPE zsdt0201,
      it_zsdt0299     TYPE TABLE OF zsdt0299,
      wa_zsdt0299     TYPE zsdt0299,
      it_zsdt0216     TYPE TABLE OF zsdt0216,
      wa_zsdt0216     TYPE zsdt0216,
      it_zsdt0217     TYPE TABLE OF zsdt0217,
      wa_zsdt0217     TYPE zsdt0217.


DATA: it_rsparams TYPE TABLE OF rsparams,
      wa_rsparams TYPE rsparams.

DATA: g_custom_container02 TYPE REF TO cl_gui_custom_container,
      g_grid02             TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog02    TYPE lvc_t_fcat,
      wa_fieldcatalog02    TYPE lvc_s_fcat,
      tl_function02        TYPE ui_functions,
      wl_function02        LIKE tl_function02  WITH HEADER LINE,
      gs_layout02          TYPE lvc_s_layo,
      gs_variant02         TYPE disvariant,
      gt_estilo02          TYPE lvc_t_styl,
      gt_color02           TYPE lvc_t_scol,  "*-CS2021000218-08.09.2022-#89493-JT-inicio
      wa_stable02          TYPE lvc_s_stbl VALUE 'XX'.

DATA: g_custom_container03 TYPE REF TO cl_gui_custom_container,
      g_grid03             TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog03    TYPE lvc_t_fcat,
      wa_fieldcatalog03    TYPE lvc_s_fcat,
      tl_function03        TYPE ui_functions,
      wl_function03        LIKE tl_function03 WITH HEADER LINE,
      gs_layout03          TYPE lvc_s_layo,
      gs_variant03         TYPE disvariant,
      gt_estilo03          TYPE lvc_t_styl,
      wa_stable03          TYPE lvc_s_stbl VALUE 'XX'.

DATA: g_custom_container01 TYPE REF TO cl_gui_custom_container,
      g_grid01             TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog01    TYPE lvc_t_fcat,
      wa_fieldcatalog01    TYPE lvc_s_fcat,
      tl_function01        TYPE ui_functions,
      wl_function01        LIKE tl_function01 WITH HEADER LINE,
      gs_layout01          TYPE lvc_s_layo,
      gs_variant01         TYPE disvariant,
      gt_estilo01          TYPE lvc_t_styl,
      wa_stable01          TYPE lvc_s_stbl VALUE 'XX'.

DATA: g_custom_container04 TYPE REF TO cl_gui_custom_container,
      g_grid04             TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog04    TYPE lvc_t_fcat,
      wa_fieldcatalog04    TYPE lvc_s_fcat,
      tl_function04        TYPE ui_functions,
      wl_function04        LIKE tl_function01 WITH HEADER LINE,
      gs_layout04          TYPE lvc_s_layo,
      gs_variant04         TYPE disvariant,
      gt_estilo04          TYPE lvc_t_styl,
      wa_stable04          TYPE lvc_s_stbl VALUE 'XX'.

DATA: g_custom_container05 TYPE REF TO cl_gui_custom_container,
      g_grid05             TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog05    TYPE lvc_t_fcat,
      wa_fieldcatalog05    TYPE lvc_s_fcat,
      tl_function05        TYPE ui_functions,
      wl_function05        LIKE tl_function01 WITH HEADER LINE,
      gs_layout05          TYPE lvc_s_layo,
      gs_variant05         TYPE disvariant,
      gt_estilo05          TYPE lvc_t_styl,
      wa_stable05          TYPE lvc_s_stbl VALUE 'XX'.


DATA: it_texto     TYPE STANDARD TABLE OF tline,
      wa_texto     TYPE tline,
      tl_texto     TYPE catsxt_longtext_itab,
      wl_texto     TYPE LINE OF catsxt_longtext_itab,
      wl_name      TYPE thead-tdname,
      l_erro       TYPE c,
      l_qt_erro    TYPE i,
      l_qt_ok      TYPE i,
      l_tabix      TYPE sy-tabix,
      t_tab        TYPE TABLE OF alsmex_tabline,
      w_tab        TYPE alsmex_tabline,
      l_rows       TYPE i,
      l_cols       TYPE i,
      l_mesg1      TYPE char200,
      l_mesg2      TYPE char200,
      l_mesg3      TYPE char200,
      l_id_produto TYPE zsdt0201-id_produto,
      l_confirma   TYPE c.

DATA: tg_selectedrow TYPE lvc_t_row,
      wg_selectedrow TYPE lvc_s_row.

DATA: wa_style TYPE lvc_s_styl,
      style2   TYPE lvc_t_styl WITH HEADER LINE.

DATA: it_raw TYPE truxs_t_text_data.
CONSTANTS  true TYPE c VALUE 'X'.

DATA: t_codmapa TYPE RANGE OF zsdt0201-codmapa,
      r_codmapa LIKE LINE OF t_codmapa,
      l_row     TYPE lvc_s_row,
      l_col     TYPE lvc_s_col,
      g_control TYPE REF TO cl_gui_control,
      t_sort    TYPE lvc_t_sort,  "*-CS2021000218-27.12.2022-#99520-JT-inicio
      w_sort    TYPE lvc_s_sort.  "*-CS2021000218-27.12.2022-#99520-JT-inicio

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_ur   RADIOBUTTON GROUP g1 USER-COMMAND modifica_tela DEFAULT 'X', "CADASTRO DE URE
              p_cli  RADIOBUTTON GROUP g1, "Cliente Revenda
              p_mapa RADIOBUTTON GROUP g1, "Cadastro Codigo Mapa
              p_prop RADIOBUTTON GROUP g1. "PROPRIEDADE
  PARAMETERS: p_submit      TYPE char01 NO-DISPLAY.  "*-US191683-25.09.2025-#191683-JT
  PARAMETERS: p_nr_rot      TYPE zsdt0132-nr_rot NO-DISPLAY.  "*-US191683-25.09.2025-#191683-JT

*-CS2021000218-08.09.2022-#89825-JT-inicio
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_produt RADIOBUTTON GROUP g1. "PRODUTO
    SELECTION-SCREEN COMMENT 2(50) TEXT-110 FOR FIELD p_produt.
  SELECTION-SCREEN END   OF LINE.
*-CS2021000218-08.09.2022-#89825-JT-fim

*-CS2021000218-03.10.2022-#91289-JT-inicio
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_siagri RADIOBUTTON GROUP g1. "PRODUTO
    SELECTION-SCREEN COMMENT 2(60) TEXT-111 FOR FIELD p_siagri.
  SELECTION-SCREEN END   OF LINE.
*-CS2021000218-03.10.2022-#91289-JT-fim

  PARAMETERS: "p_siagri RADIOBUTTON GROUP g1, "RTC x Siagri
    p_aut   RADIOBUTTON GROUP g1, "ATUALIZAÇÃO TABLES INDEA
*           p_rtc    RADIOBUTTON GROUP g1. "ATUALIZAÇÃO TABLES INDEA  "*-CS2021000218-01.09.2022-#89886-JT-inicio
    p_agriq RADIOBUTTON GROUP g1. "ATUALIZAÇÃO TABLES INDEA  "*-CS2021000218-01.09.2022-#89886-JT-inicio
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: s_esp    RADIOBUTTON GROUP g2 MODIF ID t1 USER-COMMAND modifica_tela DEFAULT 'X', "Especie
              s_cult   RADIOBUTTON GROUP g2 MODIF ID t1, "Cultivar
              s_cat    RADIOBUTTON GROUP g2 MODIF ID t1, "Categoria Sementes
              s_tpem   RADIOBUTTON GROUP g2 MODIF ID t1, "Tipo Embalagam
              s_produt RADIOBUTTON GROUP g2 MODIF ID t1, "Produto Autorizado
              s_tpapli RADIOBUTTON GROUP g2 MODIF ID t1, "Tipo Aplicação
              s_unid   RADIOBUTTON GROUP g2 MODIF ID t1, "Unidade de Medida
              s_praga  RADIOBUTTON GROUP g2 MODIF ID t1, "Praga
              s_p_auto RADIOBUTTON GROUP g2 MODIF ID t1, "Pessoa autorizada
              s_prod   RADIOBUTTON GROUP g2 MODIF ID t1, "Produtor
              s_propri RADIOBUTTON GROUP g2 MODIF ID t1, "Propriedade
              s_ure    RADIOBUTTON GROUP g2 MODIF ID t1, "URE
              s_saldo  RADIOBUTTON GROUP g2 MODIF ID t1, "Saldo Revenda
              s_todos  RADIOBUTTON GROUP g2 MODIF ID t1.
SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS: p_kunnr FOR kna1-kunnr MODIF ID t2.
SELECTION-SCREEN END OF BLOCK b3.

PARAMETER p_file TYPE rlgrap-filename NO-DISPLAY.

*-CS2021000218-06.09.2022-#89493-JT-inicio
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.

  PARAMETERS: p_rtc   RADIOBUTTON GROUP g4 USER-COMMAND gg1 MODIF ID t4.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_prod1 RADIOBUTTON GROUP g4                  MODIF ID t4.
    SELECTION-SCREEN COMMENT 2(10) TEXT-006 FOR FIELD p_prod1 MODIF ID t4.
    SELECTION-SCREEN POSITION 15.
    SELECTION-SCREEN COMMENT (16)  TEXT-005                   MODIF ID t4.
    PARAMETERS: p_data  LIKE sy-datum                         MODIF ID t4.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_prod2 RADIOBUTTON GROUP g4                  MODIF ID t4.
    SELECTION-SCREEN COMMENT (11) TEXT-007 FOR FIELD p_prod1  MODIF ID t4.
    SELECTION-SCREEN POSITION 15.
    SELECTION-SCREEN COMMENT (16)  TEXT-008                   MODIF ID t4.
    PARAMETERS: p_mapa2 LIKE zsdt0299-codigosiga              MODIF ID t4.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END   OF BLOCK b4.
*-CS2021000218-06.09.2022-#89493-JT-fim

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-009.

  SELECT-OPTIONS: s_kunnr FOR kna1-kunnr NO INTERVALS.

SELECTION-SCREEN END OF BLOCK b5.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-010.

  SELECT-OPTIONS: s_espec FOR zsdt0197-id_especie NO INTERVALS MODIF ID t5 MATCHCODE OBJECT zap_especie.

SELECTION-SCREEN END OF BLOCK b6.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

AT SELECTION-SCREEN.
  IF p_agriq = abap_true.
    IF p_prod1 = abap_true.
      IF p_data IS INITIAL.
        MESSAGE s024(sd) WITH TEXT-101 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ENDIF.

    IF p_prod2 = abap_true.
      IF p_mapa2 IS INITIAL.
        MESSAGE s024(sd) WITH TEXT-101 DISPLAY LIKE 'E'.
        STOP.
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modifica_tela.

INITIALIZATION.

START-OF-SELECTION.

  IF p_ur IS NOT INITIAL.

    PERFORM seleciona_dados_ure.
    CALL SCREEN 0103.

  ELSEIF p_mapa IS NOT INITIAL.

    PERFORM seleciona_dados_mapa.
    CALL SCREEN 0105.

  ELSEIF p_prop IS NOT INITIAL.

    PERFORM seleciona_dados_roteiro.
*-US191683-25.09.2025-#191683-JT-inicio
    IF p_submit = abap_true.
      PERFORM f_preenche_dados_roteiro.
    ENDIF.
*-US191683-25.09.2025-#191683-JT-fim
    CALL SCREEN 0100.

  ELSEIF  p_produt  IS NOT INITIAL.

    PERFORM seleciona_dados_material.
    CALL SCREEN 0102.

  ELSEIF p_cli IS NOT INITIAL.

    PERFORM seleciona_dados_cliente.
    CALL SCREEN 0104.

  ELSEIF p_aut IS NOT INITIAL.

    PERFORM z_atualiza_tab_indea.

* ELSEIF p_rtc IS NOT INITIAL.
  ELSEIF p_agriq IS NOT INITIAL. "*-CS2021000218-01.09.2022-#89886-JT-inicio

    PERFORM z_atualiza_rtc_siagri.

*---CS2019001891 - 08.02.2021 - JT - inicio
  ELSEIF p_siagri IS NOT INITIAL.

    SUBMIT zsdr0126  AND RETURN.

  ENDIF.
*---CS2019001891 - 08.02.2021 - JT - fim


CLASS lcl_event_handler01 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_button_click01 FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.

    CLASS-METHODS:
      on_data_changed01 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,

      on_data_changed_finished01 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.

ENDCLASS.

CLASS lcl_event_f401 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_f4_01 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells sender.
ENDCLASS.


CLASS lcl_event_handler01 IMPLEMENTATION.
  METHOD on_button_click01.

    READ TABLE it_saida_01 INTO wa_saida_01 INDEX es_row_no-row_id.
    CASE es_col_id.
      WHEN 'TEXTO01'.
        REFRESH: it_texto, tl_texto.
        CLEAR: wl_texto.

        wl_name =  wa_saida_01-nr_rot.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = 'ZROT'
            language                = sy-langu
            name                    = wl_name
            object                  = 'ZSDROTEIRO'
          TABLES
            lines                   = it_texto
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF sy-subrc IS INITIAL.
          LOOP AT it_texto INTO wa_texto.
            MOVE: wa_texto-tdline TO wl_texto.
            APPEND wl_texto TO tl_texto.
            CLEAR: wl_texto.
          ENDLOOP.
        ENDIF.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Roteiro'
            im_display_mode = 'X'
          CHANGING
            ch_text         = tl_texto.

      WHEN 'TEXTO02'.
        REFRESH:tl_texto.
        CLEAR: wl_texto.

        wl_texto = wa_saida_01-via_acesso.
        APPEND wl_texto TO tl_texto.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Via de Acesso'
            im_display_mode = 'X'
          CHANGING
            ch_text         = tl_texto.
    ENDCASE.

    CALL METHOD g_grid01->refresh_table_display
      EXPORTING
        is_stable = wa_stable01.

  ENDMETHOD.

  METHOD on_data_changed01.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
       WHERE fieldname = 'NR_ROT' OR
             fieldname = 'ID_PROPRIEDADE' OR
             fieldname = 'KUNNR'.


      LOOP AT it_saida_01 INTO wa_saida_01.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.
          WHEN 'KUNNR'.

            SELECT SINGLE * FROM kna1 INTO @DATA(wa_kna1)
              WHERE kunnr EQ @wa_good_cells-value.

            IF sy-subrc = 0.
              MOVE: wa_kna1-kunnr TO wa_saida_01-kunnr.
              MOVE: wa_kna1-name1 TO wa_saida_01-name1.

              MODIFY it_saida_01 FROM wa_saida_01 INDEX wa_good_cells-row_id.

            ELSE.
              MESSAGE 'Cliente informado não existe!' TYPE 'I'.
            ENDIF.

          WHEN 'NR_ROT'.
            CHECK wa_good_cells-value IS NOT INITIAL AND
                  wa_good_cells-value <> '0000000000'.  "*-US191683-25.09.2025-#191683-JT

            READ TABLE it_saida_01 INTO DATA(_saida)  INDEX wa_good_cells-row_id.

            SELECT SINGLE * FROM  zsdt0132 INTO @DATA(wa_0132)
             WHERE nr_rot  EQ @wa_good_cells-value
              AND  kunnr   EQ @_saida-kunnr.  "*-US191683-25.09.2025-#191683-JT
*             AND  id_propriedade = ''.

            IF sy-subrc = 0.

              MOVE wa_good_cells-value TO  wa_saida_01-nr_rot.
              MODIFY it_saida_01 FROM wa_saida_01 INDEX wa_good_cells-row_id.

              READ TABLE it_saida_01 INTO DATA(wsaida) WITH KEY kunnr  = wa_0132-kunnr
                                                                nr_rot = wa_0132-nr_rot .
              IF sy-subrc <> 0 .
*-US191683-25.09.2025-#191683-JT-inicio
                PERFORM f_move_dados_roteiro USING wsaida wa_0132 wa_good_cells-row_id.

*                MOVE: wa_0132-nr_rot     TO wa_saida_01-nr_rot.
*                MOVE: wa_0132-rot_desc   TO wa_saida_01-rot_desc.
*                MOVE: wa_0132-city1      TO wa_saida_01-city1.
*                MOVE: wa_0132-uf         TO wa_saida_01-uf.
*                MOVE: wa_0132-tel_number TO wa_saida_01-tel_number.
*                MOVE: wa_0132-status     TO wa_saida_01-status.
*
*                CLEAR wl_name.
*                wl_name =  wa_saida_01-nr_rot.
*
*                CALL FUNCTION 'READ_TEXT'
*                  EXPORTING
*                    id       = 'ZROT'
*                    language = sy-langu
*                    name     = wl_name
*                    object   = 'ZSDROTEIRO'
*                  TABLES
*                    lines    = it_texto.
*
*                IF it_texto IS INITIAL.
*                  wa_saida_01-texto01 = '@1F@'.
*                ELSE.
*                  wa_saida_01-texto01 = '@1E@'.
*                ENDIF.
*
*                SELECT SINGLE * FROM zsdt0206 INTO @DATA(wa206)
*                  WHERE kunnr EQ @wa_saida_01-kunnr.
*
*                SELECT SINGLE * FROM  zsdt0207 INTO  @DATA(wa207)
*                 WHERE id_produtor EQ @wa206-id_produtor.
*
*                wa_saida_01-id_propriedade = wa207-id_propriedade.
*                wa_saida_01-nome           = wa207-nome.
*
*                IF wa207-via_acesso IS INITIAL.
*                  wa_saida_01-texto02 = '@1F@'.
*                ELSE.
*                  wa_saida_01-texto02 = '@1E@'.
*                  wa_saida_01-via_acesso     = wa207-via_acesso.
*                ENDIF.
*
*                MODIFY it_saida_01 FROM wa_saida_01 INDEX wa_good_cells-row_id.
*-US191683-25.09.2025-#191683-JT-fim
              ELSE.
*               MESSAGE 'Roteiro já informado!' TYPE 'I'.
*               CLEAR wa_saida_01-nr_rot.
              ENDIF.
            ELSE.
              MESSAGE 'Numero de roteiro não existe!' TYPE 'I'.
            ENDIF.

          WHEN 'ID_PROPRIEDADE'.

            SELECT SINGLE * FROM  zsdt0207 INTO  @DATA(wa_0207)
            WHERE id_propriedade EQ  @wa_good_cells-value.

            IF sy-subrc = 0.
              MOVE: wa_0207-id_propriedade   TO wa_saida_01-id_propriedade.
              MOVE: wa_0207-nome             TO wa_saida_01-nome.

              IF wa_0207-via_acesso IS INITIAL.
                wa_saida_01-texto02 = '@1F@'.
              ELSE.
                wa_saida_01-texto02 = '@1E@'.
                MOVE wa_0207-via_acesso TO wa_saida_01-via_acesso.
              ENDIF.
              MODIFY it_saida_01 FROM wa_saida_01 INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE 'Id Propriedade não existe' TYPE 'I'.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_data_changed_finished01.

    CALL METHOD g_grid01->refresh_table_display
      EXPORTING
        is_stable = wa_stable01.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_event_f401 IMPLEMENTATION.

  METHOD on_f4_01.

    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE  dd03l-fieldname,
             s(1)      TYPE c,
           END OF ty_field.

    TYPES: BEGIN OF ty_value,
             tabname     TYPE dd03l-tabname,    "Nome da tabela
             fieldname   TYPE dd03l-fieldname,    "Nome de campo
             char79(100) TYPE c,
           END OF ty_value.

    DATA: BEGIN OF wl_roteiro,
            field(50),
          END OF  wl_roteiro.

    DATA: BEGIN OF wl_cliente,
            field(50),
          END OF  wl_cliente.

    DATA: BEGIN OF wl_propriedade,
            field(50),
          END OF  wl_propriedade.

    DATA: tl_roteiro     LIKE TABLE OF wl_roteiro,
          tl_cliente     LIKE TABLE OF wl_cliente,
          tl_propriedade LIKE TABLE OF wl_propriedade,
          tl_field       TYPE TABLE OF ty_field,
          wl_field       TYPE ty_field,
          tl_value       TYPE TABLE OF ty_value,
          wl_value       TYPE ty_value,
          wl_char(20),
          wl_index       TYPE sy-tabix,
          tl_0132        TYPE TABLE OF zsdt0132,
          wl_0132        TYPE  zsdt0132,
          tg_0132        TYPE TABLE OF zsdt0132,
          wg_0132        TYPE  zsdt0132.

    DATA: vnr_rot TYPE zsdt0132-nr_rot.

    IF e_fieldname = 'KUNNR'.

      SELECT *
         FROM  kna1 INTO TABLE @DATA(tl_kna1).

      LOOP AT tl_kna1 INTO DATA(wa_kna1).

        MOVE: wa_kna1-kunnr TO wl_cliente-field.
        APPEND wl_cliente TO tl_cliente.

        MOVE: wa_kna1-name1 TO  wl_cliente-field.
        APPEND wl_cliente TO tl_cliente.
      ENDLOOP.

      wl_field-tabname = 'KNA1'.
      wl_field-fieldname = 'KUNNR'.
      wl_field-s = 'X'.
      APPEND wl_field TO tl_field.

      wl_field-tabname   = 'KNA1'.
      wl_field-fieldname = 'NAME1'.
      wl_field-s = ' '.
      APPEND wl_field TO tl_field.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
          fieldname                 = 'KUNNR'
          tabname                   = 'KNA1'
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_cliente
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      IF sy-subrc IS INITIAL.
        READ TABLE tl_kna1 INTO wa_kna1 INDEX wl_index.
        IF es_row_no-row_id GT 0.
          READ TABLE it_saida_01 INTO wa_saida_01  INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL.
            MOVE: wa_kna1-kunnr   TO wa_saida_01-kunnr.
            MOVE: wa_kna1-name1   TO wa_saida_01-name1.
            MODIFY it_saida_01 FROM wa_saida_01 INDEX es_row_no-row_id.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSEIF e_fieldname = 'NR_ROT'.

      FREE: tl_0132.
      READ TABLE it_saida_01 INTO wa_saida_01 INDEX es_row_no-row_id.

      SELECT  *  FROM  zsdt0132 INTO TABLE tg_0132
       WHERE  kunnr  EQ wa_saida_01-kunnr
*       AND ( id_propriedade = ''      "*-US191683-25.09.2025-#191683-JT-inicio
*        OR   id_propriedade = 0 )     "*-US191683-25.09.2025-#191683-JT-inicio
        AND   status = 'A'.

      LOOP AT tg_0132 INTO wg_0132.
        READ TABLE it_saida_01 INTO DATA(wsaida) WITH KEY kunnr  = wg_0132-kunnr
                                                          nr_rot = wg_0132-nr_rot .
*        IF sy-subrc <> 0.             "*-US191683-25.09.2025-#191683-JT-inicio
        APPEND wg_0132 TO tl_0132.
*        ENDIF.                        "*-US191683-25.09.2025-#191683-JT-inicio
        CLEAR: wg_0132,  wsaida.
      ENDLOOP.

      IF tl_0132[] IS NOT INITIAL.

        LOOP AT tl_0132 INTO DATA(wa_0132).

          MOVE: wa_0132-nr_rot TO wl_roteiro-field.
          APPEND wl_roteiro TO tl_roteiro.

          MOVE: wa_0132-rot_desc TO wl_roteiro-field.
          APPEND wl_roteiro TO tl_roteiro.
        ENDLOOP.

        wl_field-tabname = 'ZSDT0132'.
        wl_field-fieldname = 'NR_ROT'.
        wl_field-s = 'X'.
        APPEND wl_field TO tl_field.

        wl_field-tabname = 'ZSDT0132'.
        wl_field-fieldname = 'ROT_DESC'.
        wl_field-s = ' '.
        APPEND wl_field TO tl_field.

        CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
          EXPORTING
            fieldname                 = 'NR_ROT'
            tabname                   = 'ZSDT0132'
          IMPORTING
            index                     = wl_index
            select_value              = wl_char
          TABLES
            fields                    = tl_field
            select_values             = tl_value
            valuetab                  = tl_roteiro
          EXCEPTIONS
            field_not_in_ddic         = 001
            more_then_one_selectfield = 002
            no_selectfield            = 003.

        IF sy-subrc IS INITIAL.
          READ TABLE tl_0132 INTO wa_0132 INDEX wl_index.
          IF es_row_no-row_id GT 0.
            READ TABLE it_saida_01 INTO wa_saida_01  INDEX es_row_no-row_id.
            IF sy-subrc IS INITIAL AND wa_saida_01-nr_rot IS INITIAL.
*-US191683-25.09.2025-#191683-JT-inicio
              PERFORM f_move_dados_roteiro USING wa_saida_01 wa_0132 es_row_no-row_id.

*              MOVE: wa_0132-nr_rot   TO wa_saida_01-nr_rot.
*              MOVE: wa_0132-rot_desc TO wa_saida_01-rot_desc.
*              MOVE: wa_0132-city1    TO wa_saida_01-city1.
*              MOVE: wa_0132-uf       TO wa_saida_01-uf.
*
*              CLEAR wl_name.
*              wl_name =  wa_saida_01-nr_rot.
*
*              CALL FUNCTION 'READ_TEXT'
*                EXPORTING
*                  id       = 'ZROT'
*                  language = sy-langu
*                  name     = wl_name
*                  object   = 'ZSDROTEIRO'
*                TABLES
*                  lines    = it_texto
*                EXCEPTIONS    "*-US191683-25.09.2025-#191683-JT
*                  OTHERS   = 1.
*
*              IF it_texto IS INITIAL.
*                wa_saida_01-texto01 = '@1F@'.
*              ELSE.
*                wa_saida_01-texto01 = '@1E@'.
*              ENDIF.
*
*              SELECT SINGLE * FROM zsdt0206 INTO @DATA(wa206)
*                WHERE kunnr EQ @wa_saida_01-kunnr.
*
*              SELECT SINGLE * FROM  zsdt0207 INTO  @DATA(wa207)
*               WHERE id_produtor EQ @wa206-id_produtor.
*
*              wa_saida_01-id_propriedade = wa207-id_propriedade.
*              wa_saida_01-nome           = wa207-nome.
*
*              IF wa207-via_acesso IS INITIAL.
*                wa_saida_01-texto02 = '@1F@'.
*              ELSE.
*                wa_saida_01-texto02 = '@1E@'.
*                wa_saida_01-via_acesso     = wa207-via_acesso.
*              ENDIF.
*              MODIFY it_saida_01 FROM wa_saida_01 INDEX es_row_no-row_id.
*-US191683-25.09.2025-#191683-JT-fim
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        MESSAGE 'Não existe roteiro cadastrado para o cliente informado!' TYPE 'I'.
      ENDIF.

    ELSEIF e_fieldname = 'ID_PROPRIEDADE'.

      READ TABLE it_saida_01 INTO wa_saida_01 INDEX es_row_no-row_id.

      SELECT * FROM zsdt0206 INTO TABLE @DATA(it_0206)
        WHERE kunnr EQ @wa_saida_01-kunnr.

      SELECT * FROM  zsdt0207 INTO TABLE @DATA(it_0207)
        FOR ALL ENTRIES IN @it_0206
       WHERE id_produtor EQ @it_0206-id_produtor
         AND data_atual EQ @it_0206-data_atual
         AND inativo EQ ''.    "<<RIM-SKM-IR134577-27.04.23

      LOOP AT it_0207  INTO DATA(wa_207).
        MOVE: wa_207-id_propriedade TO wl_propriedade-field.
        APPEND wl_propriedade TO tl_propriedade.

        MOVE: wa_207-nome TO wl_propriedade-field.
        APPEND wl_propriedade TO tl_propriedade.


        MOVE: wa_207-municipio TO wl_propriedade-field.
        APPEND wl_propriedade TO tl_propriedade.
      ENDLOOP.

      wl_field-tabname    = 'ZDE_ZSDT0207'.
      wl_field-fieldname  = 'ID_PROPRIEDADE'.
      wl_field-s = 'X'.
      APPEND wl_field TO tl_field.

      wl_field-tabname   = 'ZDE_ZSDT0207'.
      wl_field-fieldname = 'NOME'.
      wl_field-s = ' '.
      APPEND wl_field TO tl_field.

      wl_field-tabname   = 'ZSDT0207'.
      wl_field-fieldname = 'MUNICIPIO'.
      wl_field-s = ' '.
      APPEND wl_field TO tl_field.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
          fieldname                 = 'ID_PROPRIEDADE'
          tabname                   = 'ZSDT0207'
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_propriedade
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.


      IF sy-subrc IS INITIAL.
        READ TABLE it_0207 INTO wa_207 INDEX wl_index.
        IF es_row_no-row_id GT 0.
          READ TABLE it_saida_01 INTO wa_saida_01  INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL AND wa_saida_01-id_propriedade IS INITIAL.
            MOVE: wa_207-id_propriedade   TO wa_saida_01-id_propriedade.
            MOVE: wa_207-nome             TO wa_saida_01-nome.

            IF wa_207-via_acesso IS INITIAL.
              wa_saida_01-texto02 = '@1F@'.
            ELSE.
              wa_saida_01-texto02 = '@1E@'.
              MOVE wa_207-via_acesso TO wa_saida_01-via_acesso.
            ENDIF.
            MODIFY it_saida_01 FROM wa_saida_01 INDEX es_row_no-row_id.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL METHOD g_grid01->refresh_table_display
      EXPORTING
        is_stable = wa_stable01.

  ENDMETHOD.
ENDCLASS.


CLASS lcl_event_handler02 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed02 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,

      on_data_changed_finished02 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.
ENDCLASS.

CLASS lcl_event_f402 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_f4_02 FOR EVENT  onf4 OF cl_gui_alv_grid
        IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells sender.
ENDCLASS.


CLASS lcl_event_handler02 IMPLEMENTATION.
  METHOD on_data_changed02.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
      WHERE fieldname = 'MATNR' OR fieldname = 'ID_PRODUTO' OR
            fieldname = 'PRODUTOID'.    "*-CS2021000218-08.09.2022-#89493-JT-inicio
      LOOP AT it_saida_02 INTO wa_saida_02.

        CONDENSE wa_good_cells-value.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.
          WHEN 'MATNR'.

            SELECT SINGLE *
              FROM mara INTO @DATA(wa_mara)
              WHERE matnr EQ @wa_good_cells-value
               AND  matkl IN ('700130', '700280', '700230', '700240', '700350','658445').

            IF sy-subrc = 0 .

              SELECT SINGLE * FROM makt INTO @DATA(wa_makt)
                WHERE matnr EQ @wa_mara-matnr
                  AND spras EQ @sy-langu.

              wa_saida_02-matnr  =  wa_good_cells-value.
              wa_saida_02-maktx  =  wa_makt-maktx.
              wa_saida_02-matkl  =  wa_mara-matkl.

              MODIFY it_saida_02 FROM wa_saida_02 INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE 'Favor informar material que esteja em um dos grupos ( 700130, 700230, 700240, 700350 ou 658445 )' TYPE 'I'.
            ENDIF.

*-CS2021000218-08.09.2022-#89493-JT-inicio
          WHEN 'PRODUTOID'.

            IF wa_good_cells-value IS NOT INITIAL.
              SELECT SINGLE *
                INTO @DATA(w_0299)
                FROM zsdt0299
               WHERE produtoid EQ @wa_good_cells-value.

              IF sy-subrc = 0.
                wa_saida_02-numeroderisco      = w_0299-numeroderisco.
                wa_saida_02-registroministerio = w_0299-registroministerio.
                wa_saida_02-descricao2         = w_0299-descricao.
              ELSE.
                wa_saida_02-numeroderisco      = abap_off.
                wa_saida_02-registroministerio = abap_off.
                wa_saida_02-descricao2         = abap_off.
                MESSAGE 'Código AgriQ não existe!' TYPE 'I'.
              ENDIF.
              MODIFY it_saida_02 FROM wa_saida_02 INDEX wa_good_cells-row_id.
            ENDIF.
*-CS2021000218-08.09.2022-#89493-JT-fim

          WHEN 'ID_PRODUTO'.

*-CS2021000218-08.09.2022-#89493-JT-inicio
            SELECT SINGLE codmapa
              INTO @DATA(l_cdmapa)
              FROM zsdt0201
             WHERE id_produto EQ @wa_good_cells-value.
            IF sy-subrc = 0.
              wa_saida_02-codmapa  = l_cdmapa.
            ENDIF.
*-CS2021000218-08.09.2022-#89493-JT-fim

            CASE wa_saida_02-matkl.
              WHEN '700130' OR '700280' OR '700230' OR '700240' OR '700350'.

                SELECT SINGLE *
                 FROM zsdt0198 INTO  @DATA(wa_0198)
                  WHERE id_cultivar EQ @wa_good_cells-value.

                IF sy-subrc = 0.

                  wa_saida_02-id_produto  =  wa_0198-id_cultivar.
                  wa_saida_02-descricao   =  wa_0198-nome.


                  SELECT SINGLE *
                    FROM   zsdt0197 INTO @DATA(wa_0197)
                  WHERE id_especie EQ @wa_0198-id_especie.

                  IF sy-subrc = 0.
                    wa_saida_02-nomecientifico  =  wa_0197-nomecientifico.
                    wa_saida_02-nomecomum       =  wa_0197-nomecomum.
                  ENDIF.

                  MODIFY it_saida_02 FROM wa_saida_02 INDEX wa_good_cells-row_id.
                ELSE.
                  MESSAGE 'Código Indea não existe!' TYPE 'I'.
                ENDIF.

              WHEN '658445'.

                SELECT SINGLE *
                   FROM zsdt0201 INTO @DATA(wa_0201)
                 WHERE id_produto EQ @wa_good_cells-value.

                IF sy-subrc = 0.
                  wa_saida_02-id_produto      = wa_0201-id_produto.
                  wa_saida_02-descricao       = wa_0201-nome.
                  wa_saida_02-embalagem       = wa_0201-embalagem.
                  wa_saida_02-fabricante      = wa_0201-fabricante.
                  wa_saida_02-tipoembalagem   = wa_0201-tipoembalagem.
                  wa_saida_02-volume          = wa_0201-volume.
                  wa_saida_02-unidade         = wa_0201-unidade.

                  MODIFY it_saida_02 FROM wa_saida_02 INDEX wa_good_cells-row_id.
                ELSE.
                  MESSAGE 'Código Indea não existe!' TYPE 'I'.
                ENDIF.
              WHEN OTHERS.
                MESSAGE 'Favor informar o código do materiaL!' TYPE 'I'.
            ENDCASE.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD on_data_changed_finished02.
    CALL METHOD g_grid02->refresh_table_display
      EXPORTING
        is_stable = wa_stable02.
  ENDMETHOD.
ENDCLASS.



CLASS lcl_event_f402 IMPLEMENTATION.
  METHOD on_f4_02.

    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE  dd03l-fieldname,
             s(1)      TYPE c,
           END OF ty_field.

    TYPES: BEGIN OF ty_value,
             tabname     TYPE dd03l-tabname,    "Nome da tabela
             fieldname   TYPE dd03l-fieldname,    "Nome de campo
             char79(100) TYPE c,
           END OF ty_value.

    DATA: BEGIN OF wl_matnr,
            field(50),
          END OF  wl_matnr.

    DATA: BEGIN OF wl_produto,
            field(50),
          END OF  wl_produto.


    DATA: tl_matnr    LIKE TABLE OF wl_matnr,
          tl_produto  LIKE TABLE OF wl_produto,
          tl_field    TYPE TABLE OF ty_field,
          wl_field    TYPE ty_field,
          tl_value    TYPE TABLE OF ty_value,
          wl_value    TYPE ty_value,
          wl_char(20),
          wl_index    TYPE sy-tabix.

    IF  e_fieldname = 'MATNR'.

      SELECT * FROM  mara
        INTO TABLE @DATA(it_mara)
      WHERE matkl IN ('700130', '700280', '700230', '700240', '700350','658445').

      CHECK  it_mara IS NOT INITIAL.

      SELECT *
        FROM makt INTO TABLE @DATA(it_makt)
        FOR ALL ENTRIES IN @it_mara
      WHERE spras EQ @sy-langu
        AND matnr EQ @it_mara-matnr.

      LOOP AT it_makt  INTO DATA(wa_makt).

        MOVE: wa_makt-matnr TO wl_matnr-field.
        APPEND wl_matnr TO tl_matnr.

        MOVE: wa_makt-maktx TO wl_matnr-field.
        APPEND wl_matnr TO tl_matnr.
      ENDLOOP.


      wl_field-tabname    = 'MAKT'.
      wl_field-fieldname  = 'MATNR'.
      wl_field-s = 'X'.
      APPEND wl_field TO tl_field.

      wl_field-tabname   = 'MAKT'.
      wl_field-fieldname = 'MAKTX'.
      wl_field-s = ' '.
      APPEND wl_field TO tl_field.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
          fieldname                 = 'MATNR'
          tabname                   = 'MAKT'
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_matnr
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.


      IF sy-subrc IS INITIAL.
        READ TABLE it_makt INTO wa_makt INDEX wl_index.
        IF sy-subrc = 0 AND es_row_no-row_id GT 0.
          READ TABLE it_saida_02 INTO wa_saida_02  INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL.
            MOVE: wa_makt-matnr   TO wa_saida_02-matnr.
            MOVE: wa_makt-maktx   TO wa_saida_02-maktx.

            READ TABLE it_mara INTO DATA(wa_mara)  WITH KEY matnr =  wa_makt-matnr.
            IF sy-subrc = 0.
              MOVE: wa_mara-matkl TO wa_saida_02-matkl.
            ENDIF.

            MODIFY it_saida_02 FROM wa_saida_02 INDEX es_row_no-row_id.
          ENDIF.
        ENDIF.
      ENDIF.


    ELSEIF e_fieldname = 'ID_PRODUTO'.

      READ TABLE it_saida_02 INTO  wa_saida_02  INDEX es_row_no-row_id.

      CASE wa_saida_02-matkl.
        WHEN '700130' OR '700280' OR  '700230' OR '700240' OR '700350'.

          SELECT *
            FROM zsdt0198 INTO TABLE it_zsdt0198.

          SELECT *
            FROM   zsdt0197 INTO TABLE it_zsdt0197
            FOR ALL ENTRIES IN it_zsdt0198
          WHERE id_especie EQ it_zsdt0198-id_especie.


          LOOP AT it_zsdt0198  INTO DATA(wa_0198).
            MOVE: wa_0198-id_cultivar TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0198-nome TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0198-usname TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0198-data_atual TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0198-hora_atual TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            READ TABLE it_zsdt0197 INTO DATA(wa_0197) WITH KEY id_especie = wa_0198-id_especie.
            IF sy-subrc = 0.

              MOVE: wa_0197-nomecientifico TO wl_produto-field.
              APPEND wl_produto TO tl_produto.

              MOVE:  wa_0197-nomecomum TO wl_produto-field.
              APPEND wl_produto TO tl_produto.

            ENDIF.
          ENDLOOP.


          wl_field-tabname    = 'ZDE_ZSDT0198'.
          wl_field-fieldname  = 'ID_CULTIVAR'.
          wl_field-s = 'X'.
          APPEND wl_field TO tl_field.

          wl_field-tabname   = 'ZDE_ZSDT0198'.
          wl_field-fieldname = 'NOME'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname   = 'ZDE_ZSDT0198'.
          wl_field-fieldname = 'USNAME'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname   = 'ZDE_ZSDT0198'.
          wl_field-fieldname = 'DATA_ATUAL'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname   = 'ZDE_ZSDT0198'.
          wl_field-fieldname = 'HORA_ATUAL'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname   = 'ZSDT0197'.
          wl_field-fieldname = 'NOMECIENTIFICO'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname   = 'ZSDT0197'.
          wl_field-fieldname = 'NOMECOMUM'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
            EXPORTING
              fieldname                 = 'ID_CULTIVAR'
              tabname                   = 'ZSDT0198'
            IMPORTING
              index                     = wl_index
              select_value              = wl_char
            TABLES
              fields                    = tl_field
              select_values             = tl_value
              valuetab                  = tl_produto
            EXCEPTIONS
              field_not_in_ddic         = 001
              more_then_one_selectfield = 002
              no_selectfield            = 003.

          IF sy-subrc IS INITIAL.
            READ TABLE it_zsdt0198 INTO wa_0198 INDEX wl_index.
            IF sy-subrc = 0 AND es_row_no-row_id GT 0.
              READ TABLE it_saida_02 INTO wa_saida_02  INDEX es_row_no-row_id.
              IF sy-subrc IS INITIAL.

*-CS2021000218-08.09.2022-#89493-JT-inicio
                SELECT SINGLE codmapa
                  INTO @DATA(l_cdmapa2)
                  FROM zsdt0201
                 WHERE id_produto EQ @wa_0198-id_cultivar.
                IF sy-subrc = 0.
                  wa_saida_02-codmapa  = l_cdmapa2.
                ENDIF.
*-CS2021000218-08.09.2022-#89493-JT-fim

                MOVE: wa_0198-id_cultivar   TO wa_saida_02-id_produto.
                MOVE: wa_0198-nome          TO wa_saida_02-descricao.
                MOVE: wa_0198-usname        TO wa_saida_02-usname.
                MOVE: wa_0198-data_atual    TO wa_saida_02-data_atual.
                MOVE: wa_0198-hora_atual    TO wa_saida_02-hora_atual.

                READ TABLE it_zsdt0197 INTO wa_0197  WITH KEY id_especie = wa_0198-id_especie.
                IF sy-subrc = 0.
                  MOVE: wa_0197-nomecientifico   TO wa_saida_02-nomecientifico.
                  MOVE: wa_0197-nomecomum        TO wa_saida_02-nomecomum.
                ENDIF.

                MODIFY it_saida_02 FROM wa_saida_02 INDEX es_row_no-row_id.
              ENDIF.
            ENDIF.
          ENDIF.

        WHEN '658445'.

          SELECT *
            FROM zsdt0201 INTO TABLE it_zsdt0201.

          LOOP AT it_zsdt0201 INTO DATA(wa_0201).

            WRITE: wa_0201-id_produto TO wl_produto-field.
            CONDENSE wl_produto-field NO-GAPS.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0201-nome TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0201-codmapa TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0201-fabricante TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0201-embalagem TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0201-tipoembalagem TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0201-volume TO wl_produto-field.
            CONDENSE wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0201-unidade TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0201-usname  TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0201-data_atual  TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            MOVE: wa_0201-hora_atual  TO wl_produto-field.
            APPEND wl_produto TO tl_produto.

            CLEAR: wa_0201, wl_produto.
          ENDLOOP.

          wl_field-tabname    = 'ZDE_ZSDT0201'.
          wl_field-fieldname  = 'ID_PRODUTO'.
          wl_field-s = 'X'.
          APPEND wl_field TO tl_field.

          wl_field-tabname    = 'ZDE_ZSDT0201'.
          wl_field-fieldname  = 'NOME'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname    = 'ZDE_ZSDT0201'.
          wl_field-fieldname  = 'CODMAPA'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname    = 'ZDE_ZSDT0201'.
          wl_field-fieldname  = 'FABRICANTE'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname    = 'ZDE_ZSDT0201'.
          wl_field-fieldname  = 'EMBALAGEM'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname    = 'ZDE_ZSDT0201'.
          wl_field-fieldname  = 'TIPOEMBALAGEM'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname    = 'ZDE_ZSDT0201'.
          wl_field-fieldname  = 'VOLUME'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname    = 'ZDE_ZSDT0201'.
          wl_field-fieldname  = 'UNIDADE'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname    = 'ZDE_ZSDT0201'.
          wl_field-fieldname  = 'USNAME'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname    = 'ZDE_ZSDT0201'.
          wl_field-fieldname  = 'DATA_ATUAL'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          wl_field-tabname    = 'ZDE_ZSDT0201'.
          wl_field-fieldname  = 'HORA_ATUAL'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
            EXPORTING
              fieldname                 = 'ID_PRODUTO'
              tabname                   = 'ZSDT0201'
            IMPORTING
              index                     = wl_index
              select_value              = wl_char
            TABLES
              fields                    = tl_field
              select_values             = tl_value
              valuetab                  = tl_produto
            EXCEPTIONS
              field_not_in_ddic         = 001
              more_then_one_selectfield = 002
              no_selectfield            = 003.

          IF sy-subrc IS INITIAL.
            READ TABLE it_zsdt0201 INTO wa_0201 INDEX wl_index.
            IF sy-subrc = 0 AND es_row_no-row_id GT 0.
              READ TABLE it_saida_02 INTO wa_saida_02  INDEX es_row_no-row_id.
              IF sy-subrc IS INITIAL.

*-CS2021000218-08.09.2022-#89493-JT-inicio
                SELECT SINGLE codmapa
                  INTO @DATA(l_cdmapa3)
                  FROM zsdt0201
                 WHERE id_produto EQ @wa_0201-id_produto.
                IF sy-subrc = 0.
                  wa_saida_02-codmapa  = l_cdmapa3.
                ENDIF.
*-CS2021000218-08.09.2022-#89493-JT-fim

                MOVE: wa_0201-id_produto     TO wa_saida_02-id_produto.
                MOVE: wa_0201-nome           TO wa_saida_02-descricao.
                MOVE: wa_0201-fabricante     TO wa_saida_02-fabricante.
                MOVE: wa_0201-embalagem      TO wa_saida_02-embalagem.
                MOVE: wa_0201-tipoembalagem  TO wa_saida_02-tipoembalagem.
                MOVE: wa_0201-volume         TO wa_saida_02-volume.
                MOVE: wa_0201-unidade        TO wa_saida_02-unidade.
                MOVE: wa_0201-usname         TO wa_saida_02-usname.
                MOVE: wa_0201-data_atual     TO wa_saida_02-data_atual.
                MOVE: wa_0201-hora_atual     TO wa_saida_02-hora_atual.

                MODIFY it_saida_02 FROM wa_saida_02 INDEX es_row_no-row_id.
              ENDIF.
            ENDIF.
          ENDIF.
      ENDCASE.

*-CS2021000218-08.09.2022-#89493-JT-inicio
    ELSEIF e_fieldname = 'PRODUTOID'.

      FREE: tl_produto, tl_field.

      READ TABLE it_saida_02 INTO  wa_saida_02  INDEX es_row_no-row_id.

      SELECT SINGLE codmapa
        INTO @DATA(l_codmapa)
        FROM zsdt0201
       WHERE id_produto = @wa_saida_02-id_produto.

      IF sy-subrc = 0.
* Inclusão - RIM-SKM - IR147650 - 10.08.23 - Inicio
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = l_codmapa
          IMPORTING
            output = l_codmapa.
* Inclusão - RIM-SKM - IR147650 - 10.08.23 - Fim

        FREE: t_codmapa.
        r_codmapa-sign    = 'I'.
        r_codmapa-option  = 'CP'.
        r_codmapa-low     = '*' && l_codmapa && '*'.
        APPEND r_codmapa TO t_codmapa.

        SELECT *
          FROM zsdt0299
          INTO TABLE @DATA(t_0299)
         WHERE registroministerio IN @t_codmapa.
      ELSE.
        SELECT *
          FROM zsdt0299
          INTO TABLE t_0299.
      ENDIF.

      FREE: l_codmapa.

      LOOP AT t_0299 INTO DATA(wa_0299).
        MOVE: wa_0299-produtoid           TO wl_produto-field.
        APPEND wl_produto TO tl_produto.
        MOVE: wa_0299-descricao           TO wl_produto-field.
        APPEND wl_produto TO tl_produto.
        MOVE: wa_0299-registroministerio  TO wl_produto-field.
        APPEND wl_produto TO tl_produto.
      ENDLOOP.

      wl_field-tabname    = 'ZDE_ZSDT0299'.
      wl_field-fieldname  = 'PRODUTOID'.
      wl_field-s = 'X'.
      APPEND wl_field TO tl_field.

      wl_field-tabname    = 'ZDE_ZSDT0299'.
      wl_field-fieldname  = 'DESCRICAO'.
      wl_field-s = 'X'.
      APPEND wl_field TO tl_field.

      wl_field-tabname    = 'ZDE_ZSDT0299'.
      wl_field-fieldname  = 'REGISTROMINISTERIO'.
      wl_field-s = 'X'.
      APPEND wl_field TO tl_field.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
          fieldname                 = 'PRODUTIOD'
          tabname                   = 'ZSDT0299'
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_produto
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      IF sy-subrc IS INITIAL.
        READ TABLE t_0299 INTO wa_0299 INDEX wl_index.
        IF sy-subrc = 0 AND es_row_no-row_id GT 0.
          READ TABLE it_saida_02 INTO wa_saida_02  INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL.
            CLEAR wa_style.
            READ TABLE wa_saida_02-celltab INTO wa_style WITH KEY fieldname =  'PRODUTOID'.
            IF wa_style-style = cl_gui_alv_grid=>mc_style_enabled OR
               wa_style-style = '00000000'.
              MOVE: wa_0299-produtoid                   TO wa_saida_02-produtoid.
              MOVE: wa_0299-descricao                   TO wa_saida_02-descricao2.
              MOVE: wa_0299-numeroderisco               TO wa_saida_02-numeroderisco.
              MOVE: wa_0299-registroministerio          TO wa_saida_02-registroministerio.
              MODIFY it_saida_02 FROM wa_saida_02    INDEX es_row_no-row_id.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*-CS2021000218-08.09.2022-#89493-JT-fim

    ENDIF.

    CALL METHOD g_grid02->refresh_table_display
      EXPORTING
        is_stable = wa_stable02.


  ENDMETHOD.
ENDCLASS.


CLASS lcl_event_handler03 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed03 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,

      on_data_changed_finished03 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.
ENDCLASS.


CLASS lcl_event_f403 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_f4_03 FOR EVENT  onf4 OF cl_gui_alv_grid
        IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells sender.
ENDCLASS.


CLASS lcl_event_handler03 IMPLEMENTATION.
  METHOD on_data_changed03.

    LOOP AT  er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
      WHERE fieldname EQ 'BUKRS' OR fieldname EQ 'LIFNR'.

      LOOP AT it_saida_03 INTO wa_saida_03.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.
          WHEN 'BUKRS'.

            SELECT SINGLE butxt FROM t001 INTO wa_saida_03-butxt
              WHERE bukrs EQ wa_good_cells-value.

            IF sy-subrc <> 0.
              MESSAGE 'Empresa não existe!' TYPE  'I'.
            ELSE.
              MODIFY it_saida_03 FROM wa_saida_03 INDEX wa_good_cells-row_id.
            ENDIF.

          WHEN 'LIFNR'.
            SELECT SINGLE * FROM lfa1  INTO @DATA(wa_lfa1)
             WHERE lifnr EQ @wa_good_cells-value.

            IF sy-subrc = 0.
              wa_saida_03-name1  = wa_lfa1-name1.
              wa_saida_03-ort01  = wa_lfa1-ort01.
              wa_saida_03-regio  = wa_lfa1-regio.
              wa_saida_03-stcd1  = wa_lfa1-stcd1.
              wa_saida_03-stras  = wa_lfa1-stras.

              MODIFY it_saida_03 FROM wa_saida_03 INDEX wa_good_cells-row_id.
            ELSE.
              MESSAGE 'Fonecedor não existe!' TYPE  'I'.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_data_changed_finished03.

    CALL METHOD g_grid03->refresh_table_display
      EXPORTING
        is_stable = wa_stable03.

  ENDMETHOD.
ENDCLASS.


CLASS lcl_event_f403 IMPLEMENTATION.
  METHOD on_f4_03.

    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE  dd03l-fieldname,
             s(1)      TYPE c,
           END OF ty_field.

    TYPES: BEGIN OF ty_value,
             tabname    TYPE dd03l-tabname,    "Nome da tabela
             fieldname  TYPE dd03l-fieldname,    "Nome de campo
             char79(79) TYPE c,
           END OF ty_value.

    DATA: BEGIN OF wl_empresa,
            field(50),
          END OF  wl_empresa.

    DATA: tl_empresa  LIKE TABLE OF wl_empresa,
          tl_field    TYPE TABLE OF ty_field,
          wl_field    TYPE ty_field,
          tl_value    TYPE TABLE OF ty_value,
          wl_value    TYPE ty_value,
          wl_char(20),
          wl_index    TYPE sy-tabix.

    IF e_fieldname = 'BUKRS'.

      SELECT *
        FROM t001 INTO TABLE @DATA(tl_t001).

      LOOP AT tl_t001 INTO DATA(wa_t001).

        MOVE: wa_t001-bukrs TO wl_empresa-field.
        APPEND wl_empresa TO tl_empresa.

        MOVE: wa_t001-butxt TO wl_empresa-field.
        APPEND wl_empresa TO tl_empresa.
      ENDLOOP.

      wl_field-tabname = 'T001'.
      wl_field-fieldname = 'BUKRS'.
      wl_field-s = 'X'.
      APPEND wl_field TO tl_field.

      wl_field-tabname = 'T001'.
      wl_field-fieldname = 'BUTXT'.
      wl_field-s = ' '.
      APPEND wl_field TO tl_field.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
          fieldname                 = 'BUKRS'
          tabname                   = 'T001'
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_empresa
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      IF sy-subrc IS INITIAL.
        READ TABLE tl_t001 INTO wa_t001 INDEX wl_index.
        IF es_row_no-row_id GT 0.
          READ TABLE it_saida_03 INTO wa_saida_03  INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL.
            MOVE: wa_t001-bukrs TO wa_saida_03-bukrs.
            MOVE: wa_t001-butxt TO wa_saida_03-butxt.
            MODIFY it_saida_03 FROM wa_saida_03 INDEX es_row_no-row_id.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CALL METHOD g_grid03->refresh_table_display
      EXPORTING
        is_stable = wa_stable03.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_handler04 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed04 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,

      on_data_changed_finished04 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.
ENDCLASS.


CLASS lcl_event_f404 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_f4_04 FOR EVENT  onf4 OF cl_gui_alv_grid
        IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells sender.
ENDCLASS.


CLASS lcl_event_handler04 IMPLEMENTATION.
  METHOD on_data_changed04.


    LOOP AT  er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
      WHERE fieldname EQ 'BUKRS' OR fieldname EQ 'KUNNR' OR fieldname EQ 'TIPO' OR
            fieldname EQ 'SETOR_ATIVIDADE' OR fieldname EQ 'REVENDA'.

      LOOP AT it_saida_04 INTO wa_saida_04.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.
          WHEN 'BUKRS'.

            SELECT SINGLE butxt FROM t001 INTO wa_saida_04-butxt
              WHERE bukrs EQ wa_good_cells-value.

            IF sy-subrc <> 0.
              MESSAGE 'Empresa não existe!' TYPE  'I'.
            ELSE.
              MODIFY it_saida_04 FROM wa_saida_04 INDEX wa_good_cells-row_id.
            ENDIF.

          WHEN 'TIPO'.

            IF wa_good_cells-value EQ 'C'.
              wa_saida_04-text            = 'Cliente'.
            ELSE.
              wa_saida_04-text = 'Fornecedor'.

            ENDIF.

            MODIFY it_saida_04 FROM wa_saida_04 INDEX wa_good_cells-row_id.
          WHEN 'KUNNR'.

            CASE wa_saida_04-tipo.
              WHEN 'C'.
                SELECT SINGLE * FROM kna1  INTO @DATA(wa_kna1)
                 WHERE kunnr EQ @wa_good_cells-value.

                IF sy-subrc = 0.
                  wa_saida_04-name1  = wa_kna1-name1.
                  wa_saida_04-ort01  = wa_kna1-ort01.
                  wa_saida_04-regio  = wa_kna1-regio.

                  IF wa_kna1-stcd1 IS NOT INITIAL.
                    wa_saida_04-stcd1 = wa_kna1-stcd1.
                  ELSE.
                    wa_saida_04-stcd1 = wa_kna1-stcd2.
                  ENDIF.
                  wa_saida_04-stras  = wa_kna1-stras.

                  MODIFY it_saida_04 FROM wa_saida_04 INDEX wa_good_cells-row_id.
                ELSE.
                  MESSAGE 'Cliente não existe!' TYPE  'I'.
                ENDIF.

              WHEN 'F'.

                SELECT SINGLE * FROM lfa1  INTO @DATA(wa_lfa1)
                 WHERE lifnr EQ @wa_good_cells-value.

                IF sy-subrc = 0.
                  wa_saida_04-name1  = wa_lfa1-name1.
                  wa_saida_04-ort01  = wa_lfa1-ort01.
                  wa_saida_04-regio  = wa_lfa1-regio.
                  IF wa_lfa1-stcd1 IS NOT INITIAL.
                    wa_saida_04-stcd1 = wa_lfa1-stcd1.
                  ELSE.
                    wa_saida_04-stcd1 = wa_lfa1-stcd2.
                  ENDIF.
                  wa_saida_04-stras  = wa_lfa1-stras.

                  MODIFY it_saida_04 FROM wa_saida_04 INDEX wa_good_cells-row_id.
                ELSE.
                  MESSAGE 'Fornecedor não existe!' TYPE  'I'.
                ENDIF.
            ENDCASE.

          WHEN 'SETOR_ATIVIDADE'.
            CASE wa_good_cells-value.
              WHEN 'A'.
                wa_saida_04-text01 = 'Agrotóxico'.

              WHEN 'S'.
                wa_saida_04-text01 = 'Sementes'.
            ENDCASE.
            MODIFY it_saida_04 FROM wa_saida_04 INDEX wa_good_cells-row_id.

          WHEN 'REVENDA'.
            CASE wa_good_cells-value.
              WHEN 'S'.
                wa_saida_04-text02 = 'Sim'.
              WHEN 'N'.
                wa_saida_04-text02 = 'Não'.
            ENDCASE.
            MODIFY it_saida_04 FROM wa_saida_04 INDEX wa_good_cells-row_id.

        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_data_changed_finished04.

    CALL METHOD g_grid04->refresh_table_display
      EXPORTING
        is_stable = wa_stable04.

  ENDMETHOD.
ENDCLASS.

CLASS lcl_event_f404 IMPLEMENTATION.
  METHOD on_f4_04.

    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,
             fieldname TYPE  dd03l-fieldname,
             s(1)      TYPE c,
           END OF ty_field.

    TYPES: BEGIN OF ty_value,
             tabname    TYPE dd03l-tabname,    "Nome da tabela
             fieldname  TYPE dd03l-fieldname,    "Nome de campo
             char79(79) TYPE c,
           END OF ty_value.

    DATA: BEGIN OF wl_codigo,
            field(50),
          END OF  wl_codigo.

    DATA: tl_codigo   LIKE TABLE OF wl_codigo,
          tl_field    TYPE TABLE OF ty_field,
          wl_field    TYPE ty_field,
          tl_value    TYPE TABLE OF ty_value,
          wl_value    TYPE ty_value,
          wl_char(20),
          wl_index    TYPE sy-tabix.

    IF e_fieldname = 'KUNNR'.

      READ TABLE it_saida_04 INTO wa_saida_04  INDEX es_row_no-row_id.
      CASE wa_saida_04-tipo.
        WHEN 'C'.

          SELECT *
            FROM kna1 INTO TABLE @DATA(tl_kna1).

          LOOP AT tl_kna1 INTO DATA(wa_kna1).

            MOVE: wa_kna1-kunnr TO wl_codigo-field.
            APPEND wl_codigo TO tl_codigo.

            MOVE: wa_kna1-name1 TO wl_codigo-field.
            APPEND wl_codigo TO tl_codigo.
          ENDLOOP.

          wl_field-tabname = 'KNA1'.
          wl_field-fieldname = 'KUNNR'.
          wl_field-s = 'X'.
          APPEND wl_field TO tl_field.

          wl_field-tabname = 'KNA1'.
          wl_field-fieldname = 'NAME1'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
            EXPORTING
              fieldname                 = 'KUNNR'
              tabname                   = 'KNA1'
            IMPORTING
              index                     = wl_index
              select_value              = wl_char
            TABLES
              fields                    = tl_field
              select_values             = tl_value
              valuetab                  = tl_codigo
            EXCEPTIONS
              field_not_in_ddic         = 001
              more_then_one_selectfield = 002
              no_selectfield            = 003.

          IF sy-subrc IS INITIAL.
            READ TABLE tl_kna1 INTO wa_kna1 INDEX wl_index.
            IF es_row_no-row_id GT 0.
              READ TABLE it_saida_04 INTO wa_saida_04  INDEX es_row_no-row_id.
              IF sy-subrc IS INITIAL.
                MOVE: wa_kna1-kunnr TO wa_saida_04-kunnr,
                      wa_kna1-name1 TO wa_saida_04-name1,
                      wa_kna1-ort01 TO wa_saida_04-ort01,
                      wa_kna1-regio TO wa_saida_04-regio,
                      wa_kna1-stcd1 TO wa_saida_04-stcd1,
                      wa_kna1-stras TO wa_saida_04-stras.

                MODIFY it_saida_04 FROM wa_saida_04 INDEX es_row_no-row_id.
              ENDIF.
            ENDIF.
          ENDIF.


        WHEN 'F'.

          SELECT *
            FROM lfa1 INTO TABLE @DATA(tl_lfa1).

          LOOP AT tl_lfa1 INTO DATA(wa_lfa1).

            MOVE: wa_lfa1-lifnr TO wl_codigo-field.
            APPEND wl_codigo TO tl_codigo.

            MOVE: wa_lfa1-name1 TO wl_codigo-field.
            APPEND wl_codigo TO tl_codigo.
          ENDLOOP.

          wl_field-tabname = 'LFA1'.
          wl_field-fieldname = 'LIFNR'.
          wl_field-s = 'X'.
          APPEND wl_field TO tl_field.

          wl_field-tabname = 'LFA1'.
          wl_field-fieldname = 'NAME1'.
          wl_field-s = ' '.
          APPEND wl_field TO tl_field.

          CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
            EXPORTING
              fieldname                 = 'LIFNR'
              tabname                   = 'LFA1'
            IMPORTING
              index                     = wl_index
              select_value              = wl_char
            TABLES
              fields                    = tl_field
              select_values             = tl_value
              valuetab                  = tl_codigo
            EXCEPTIONS
              field_not_in_ddic         = 001
              more_then_one_selectfield = 002
              no_selectfield            = 003.

          IF sy-subrc IS INITIAL.
            READ TABLE tl_lfa1 INTO wa_lfa1 INDEX wl_index.
            IF es_row_no-row_id GT 0.
              READ TABLE it_saida_04 INTO wa_saida_04  INDEX es_row_no-row_id.
              IF sy-subrc IS INITIAL.
                MOVE: wa_lfa1-lifnr TO wa_saida_04-kunnr,
                      wa_lfa1-name1 TO wa_saida_04-name1,
                      wa_lfa1-ort01 TO wa_saida_04-ort01,
                      wa_lfa1-regio TO wa_saida_04-regio,
                      wa_lfa1-stcd1 TO wa_saida_04-stcd1,
                      wa_lfa1-stras TO wa_saida_04-stras.

                MODIFY it_saida_04 FROM wa_saida_04 INDEX es_row_no-row_id.
              ENDIF.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
          MESSAGE 'Favor selecione um tipo "C" Cliente ou "F" Fornecedor ' TYPE 'I'.
          EXIT.
      ENDCASE.
    ENDIF.

    CALL METHOD g_grid04->refresh_table_display
      EXPORTING
        is_stable = wa_stable04.

  ENDMETHOD.
ENDCLASS.


CLASS lcl_event_handler05 DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed05 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,

      on_data_changed_finished05 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells sender.
ENDCLASS.

CLASS lcl_event_handler05 IMPLEMENTATION.
  METHOD on_data_changed05.

    LOOP AT  er_data_changed->mt_good_cells INTO DATA(wa_good_cells)
      WHERE fieldname EQ 'BUSCA_INDEA' .

      LOOP AT it_saida_05 INTO wa_saida_05.

        CHECK wa_good_cells-row_id EQ sy-tabix.

        CASE wa_good_cells-fieldname.
          WHEN 'BUSCA_INDEA'.
            wa_saida_05-busca_indea = 'X'.
            MODIFY it_saida_05 FROM wa_saida_05 INDEX wa_good_cells-row_id.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_data_changed_finished05.
    CALL METHOD g_grid05->refresh_table_display
      EXPORTING
        is_stable = wa_stable05.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modifica_tela .
  LOOP AT SCREEN.

    IF p_aut = 'X'.
      IF screen-group1 = 'T1'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
      IF s_esp IS NOT INITIAL OR
         s_cult IS NOT INITIAL.
        IF screen-group1 = 'T5'.
          screen-invisible = 0.
          screen-input     = 1.
          screen-active    = 1.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.
      ENDIF.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

    ELSE.
      IF screen-group1 = 'T1'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.

*** Inicio - Rubenilson Pereira - 07.04.25 #168932
    IF screen-group1 = 'T5'.
      screen-invisible = 1.
      screen-input     = 0.
      screen-active    = 0.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.
*** Fim - Rubenilson Pereira - 07.04.25 #168932

    IF p_prop = 'X'.
      IF screen-group1 = 'T2'.
        screen-invisible = 0.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ELSE.
      IF screen-group1 = 'T2'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.

*-CS2021000218-01.09.2022-#89886-JT-inicio
    IF p_agriq = 'X'.
      IF screen-group1 = 'T4'.
        screen-invisible = 0.
        screen-required  = 2.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ELSE.
      IF screen-group1 = 'T4'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.
*-CS2021000218-01.09.2022-#89886-JT-fim
  ENDLOOP.

*-CS2021000218-01.09.2022-#89886-JT-inicio
  LOOP AT SCREEN.
    IF screen-name CS 'P_DATA'.
      IF p_prod1 = abap_true.
        screen-input     = 1.
        CLEAR p_mapa2.
      ELSE.
        screen-input     = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name CS 'P_MAPA2'.
      IF p_prod2 = abap_true.
        screen-input     = 1.
        CLEAR p_data.
      ELSE.
        screen-input     = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
*-CS2021000218-01.09.2022-#89886-JT-fim

    IF screen-name CS 'S_KUNNR'.
      IF p_aut IS INITIAL OR ( s_prod IS INITIAL AND s_propri IS INITIAL ).
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.
ENDFORM.
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

* PERFORM  z_cria_alv02.  "*-CS2021000218-27.12.2022-#99520-JT

  IF g_custom_container02 IS INITIAL.

    PERFORM z_cria_alv02.  "*-CS2021000218-27.12.2022-#99520-JT
    PERFORM f_sort.        "*-CS2021000218-27.12.2022-#99520-JT

    CREATE OBJECT g_custom_container02
      EXPORTING
        container_name              = 'CONTAINER02'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid02 IS INITIAL AND g_custom_container02 IS NOT  INITIAL.
      CREATE OBJECT g_grid02
        EXPORTING
          i_parent          = g_custom_container02
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    wl_function02  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function02 TO tl_function02.
    wl_function02  = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function02 TO tl_function02.


    gs_layout02-stylefname = 'CELLTAB'.
    gs_layout02-ctab_fname = 'CELLCOLOR'.  "*-CS2021000218-08.09.2022-#89493-JT-inicio
    gs_layout02-sel_mode   = 'A'.          "*-CS2021000218-08.09.2022-#89493-JT-inicio
    gs_variant02-report    = sy-repid.     "*-CS2021000218-08.09.2022-#89493-JT-inicio
*   gs_variant02-username  = sy-uname.     "*-CS2021000218-08.09.2022-#89493-JT-inicio

    wl_f402-fieldname  = 'ID_PRODUTO'.
    wl_f402-register   = 'X'.
    wl_f402-getbefore  = 'X'.
    APPEND wl_f402 TO  lt_f402.
    CLEAR wl_f402.

    wl_f402-fieldname  = 'MATNR'.
    wl_f402-register   = 'X'.
    wl_f402-getbefore  = 'X'.
    APPEND wl_f402 TO  lt_f402.
    CLEAR wl_f402.

*-CS2021000218-08.09.2022-#89493-JT-inicio
    wl_f402-fieldname  = 'PRODUTOID'.
    wl_f402-register   = 'X'.
    wl_f402-getbefore  = 'X'.
    APPEND wl_f402 TO  lt_f402.
    CLEAR wl_f402.
*-CS2021000218-08.09.2022-#89493-JT-fim

    CALL METHOD g_grid02->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant02
        is_layout            = gs_layout02
        it_toolbar_excluding = tl_function02
        i_save               = 'A'           "*-CS2021000218-08.09.2022-#89493-JT-inicio
      CHANGING
        it_outtab            = it_saida_02
        it_sort              = t_sort[]      "*-CS2021000218-27.12.2022-#99520-JT
        it_fieldcatalog      = it_fieldcatalog02.

    SET HANDLER: lcl_event_handler02=>on_data_changed02 FOR g_grid02,
                 lcl_event_handler02=>on_data_changed_finished02  FOR g_grid02.


    SET HANDLER lcl_event_f402=>on_f4_02 FOR g_grid02.


    CALL METHOD g_grid02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid02->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD g_grid02->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f402[].

  ELSE.

*-CS2021000218-27.12.2022-#99520-JT-inicio
*    CALL METHOD g_grid02->set_frontend_fieldcatalog
*      EXPORTING
*        it_fieldcatalog = it_fieldcatalog02.
*-CS2021000218-27.12.2022-#99520-JT-fim

    CALL METHOD g_grid02->refresh_table_display
      EXPORTING
        is_stable = wa_stable02.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_sort.

  FREE: t_sort.
  w_sort-fieldname = 'DESCRICAO'.
* w_sort-subtot    = 'X'.
  w_sort-spos      = 1.
  w_sort-up        = 'X'.
  APPEND w_sort   TO t_sort.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.
  DATA it_salvar_02  TYPE TABLE OF  zsdt0210.
  DATA wa_salvar_02  TYPE zsdt0210.
  DATA: msg TYPE char100.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SALVAR'.

      CALL METHOD g_grid02->check_changed_data. ""*-CS2021000218-12.09.2022-#89825-JT-inicio

      CLEAR:   wa_saida_02, l_erro.
      REFRESH it_salvar_02.

      LOOP AT it_saida_02 INTO wa_saida_02
        WHERE check  = ' ' OR
              editar = abap_true.  "*-CS2021000218-12.09.2022-#89825-JT-inicio

        CLEAR wa_salvar_02.
        CONDENSE: wa_saida_02-matnr,
                  wa_saida_02-produtoid.

        IF wa_saida_02-editar = abap_false.
          SELECT SINGLE *
            FROM zsdt0210
            INTO @DATA(wzt210)
           WHERE matnr  EQ @wa_saida_02-matnr.
        ELSE.
          sy-subrc = 4.
        ENDIF.

        IF sy-subrc <> 0.

*-CS2021000218-12.09.2022-#89825-JT-inicio
          IF wa_saida_02-produtoid IS INITIAL.
*           MESSAGE 'Favor informar o cód.Produto AgriQ!' TYPE 'I'.
*           EXIT.
          ELSE.
            SELECT SINGLE *
              INTO @DATA(w1_0299)
              FROM zsdt0299
             WHERE produtoid EQ @wa_saida_02-produtoid.
            IF sy-subrc <> 0.
              l_erro = abap_true.
              MESSAGE 'Código Material AgriQ não existe!' TYPE 'I'.
              EXIT.
            ENDIF.
            wa_salvar_02-id_matnr_agriq = wa_saida_02-produtoid.
          ENDIF.
*-CS2021000218-12.09.2022-#89825-JT-fim

          SELECT SINGLE *
            FROM mara INTO @DATA(wa_mara)
          WHERE matnr EQ @wa_saida_02-matnr
           AND  matkl IN ('700130', '700280', '700230', '700240', '700350','658445').

          IF sy-subrc = 0 .
            IF wa_saida_02-matnr IS INITIAL.
              MESSAGE 'Favor informar o código material' TYPE 'I'.
              EXIT.
            ELSE.
              SELECT SINGLE * FROM makt INTO @DATA(wmakt)
                WHERE matnr EQ @wa_saida_02-matnr
                 AND  spras EQ @sy-langu.

              IF sy-subrc = 0.
                wa_salvar_02-mandt   = sy-mandt.
                wa_salvar_02-matnr   = wa_saida_02-matnr.
              ELSE.
                l_erro = abap_true.
                MESSAGE 'Código Material não existe' TYPE 'I'.
                EXIT.
              ENDIF.
            ENDIF.

            IF wa_saida_02-id_produto IS INITIAL.
              l_erro = abap_true.
              MESSAGE 'Favor informar o cód.Produto Indea' TYPE 'I'.
              EXIT.
            ELSE.
              CASE wa_saida_02-matkl.
                WHEN '700130' OR '700280' OR '700230' OR '700240' OR '700350'.

                  SELECT SINGLE * FROM zsdt0198 INTO  @DATA(wa_0198)
                    WHERE id_cultivar EQ @wa_saida_02-id_produto.

                  IF sy-subrc = 0.
                    wa_salvar_02-id_matnr_idea = wa_saida_02-id_produto.
                  ELSE.
                    l_erro = abap_true.
                    MESSAGE 'Código Indea não existe!' TYPE 'I'.
                  ENDIF.

                WHEN '658445'.

                  SELECT SINGLE *  FROM zsdt0201 INTO @DATA(wa_0201)
                   WHERE id_produto EQ @wa_saida_02-id_produto.

                  IF sy-subrc = 0.
                    wa_salvar_02-id_matnr_idea = wa_saida_02-id_produto.
                  ELSE.
                    l_erro = abap_true.
                    MESSAGE 'Código Indea não existe!' TYPE 'I'.
                  ENDIF.
                WHEN OTHERS.
                  l_erro = abap_true.
                  MESSAGE 'Favor informar o código do materiaL!' TYPE 'I'.
              ENDCASE.
            ENDIF.
            IF wa_salvar_02-id_matnr_idea IS NOT INITIAL AND wa_salvar_02-matnr IS NOT INITIAL .
              APPEND wa_salvar_02 TO it_salvar_02.
            ENDIF.
          ELSE.
            l_erro = abap_true.
            MESSAGE 'Favor informar material que esteja em um dos grupos ( 700130, 700230, 700240, 700350 ou 658445 )' TYPE 'I'.
            EXIT.
          ENDIF.
        ELSEIF wzt210-matnr IS NOT INITIAL.
          CLEAR msg.
          l_erro = abap_true.
          CONCATENATE 'DE/PARA já realizado com o material ' wzt210-matnr INTO msg SEPARATED BY space.
          MESSAGE msg TYPE 'I'.
          EXIT.
        ENDIF.
        CLEAR: wa_saida_02, wa_salvar_02.
      ENDLOOP.

      IF it_salvar_02 IS NOT INITIAL AND l_erro = abap_false.  ""*-CS2021000218-12.09.2022-#89825-JT-inicio
*       INSERT zsdt0210 FROM TABLE it_salvar_02.
        MODIFY zsdt0210 FROM TABLE it_salvar_02.  "*-CS2021000218-12.09.2022-#89825-JT-inicio
        COMMIT WORK.
        MESSAGE 'Dados gravado com Sucesso!' TYPE 'S'.

        REFRESH it_saida_02.
        CLEAR wa_saida_02.

        PERFORM seleciona_dados_material.

*        LOOP AT IT_SAIDA_02 INTO WA_SAIDA_02.
*          FREE WA_SAIDA_02-CELLTAB.
*          GT_ESTILO02 =  VALUE #( ( FIELDNAME = 'MATNR'      STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED  )
*                                  ( FIELDNAME = 'ID_PRODUTO' STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED  )  ).
*          INSERT LINES OF GT_ESTILO02 INTO TABLE WA_SAIDA_02-CELLTAB.
*          MODIFY IT_SAIDA_02 FROM WA_SAIDA_02.
*        ENDLOOP.
      ENDIF.

*-CS2021000218-12.09.2022-#89825-JT-inicio
    WHEN '&EDIT'.
      CALL METHOD g_grid02->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF tg_selectedrow IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'I'.
        EXIT.
      ENDIF.

      LOOP AT tg_selectedrow INTO wg_selectedrow.
        READ TABLE it_saida_02 INTO wa_saida_02 INDEX  wg_selectedrow-index.
        l_tabix            = sy-tabix.

        SELECT SINGLE matkl
          INTO @DATA(l_matkl)
          FROM mara
         WHERE matnr = @wa_saida_02-matnr.
        IF sy-subrc = 0.
          wa_saida_02-matkl = l_matkl.
        ENDIF.
        wa_saida_02-editar  = abap_true.

        FREE wa_saida_02-celltab.
        gt_estilo02 =  VALUE #( ( fieldname = 'MATNR'      style = cl_gui_alv_grid=>mc_style_disabled )
                                ( fieldname = 'ID_PRODUTO' style = cl_gui_alv_grid=>mc_style_enabled  )
                                ( fieldname = 'PRODUTOID'  style = cl_gui_alv_grid=>mc_style_enabled  )  ).
        INSERT LINES OF gt_estilo02 INTO TABLE wa_saida_02-celltab.

*-CS2021000218-08.09.2022-#89493-JT-inicio
        FREE wa_saida_02-cellcolor.
        gt_color02  =  VALUE #( ( fname     = 'ID_PRODUTO' color-col = 1 color-int = 1 )
                                ( fname     = 'PRODUTOID'  color-col = 1 color-int = 1 ) ).
        wa_saida_02-cellcolor[] = gt_color02[].
*-CS2021000218-08.09.2022-#89493-JT-fim

        MODIFY it_saida_02          FROM wa_saida_02 INDEX l_tabix.
      ENDLOOP.

      wa_stable02 = 'XX'.
*-CS2021000218-12.09.2022-#89825-JT-fim

    WHEN '&INS'.
*     APPEND INITIAL LINE TO it_saida_02.
      INSERT INITIAL LINE INTO it_saida_02 INDEX 1. "*-CS2021000218-08.09.2022-#89493-JT-inicio

*     CLEAR wa_stable02.

      IF g_grid02 IS NOT INITIAL.
        CALL METHOD g_grid02->set_focus
          EXPORTING
            control = g_grid02.
      ENDIF.

      l_row           = 1.
      l_col-fieldname = 'MATNR'.

      CALL METHOD g_grid02->set_current_cell_via_id
        EXPORTING
          is_row_id    = l_row
          is_column_id = l_col.

    WHEN '&DEL'.

      CALL METHOD g_grid02->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF tg_selectedrow IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'I'.
        EXIT.
      ELSE.

        LOOP AT tg_selectedrow INTO wg_selectedrow.

          READ TABLE it_saida_02 INTO wa_saida_02 INDEX  wg_selectedrow-index.

          DELETE FROM zsdt0210 WHERE id_matnr_idea =  wa_saida_02-id_produto
                               AND   matnr         =  wa_saida_02-matnr.

        ENDLOOP.

        REFRESH it_saida_02.
        CLEAR wa_saida_02.

        PERFORM seleciona_dados_material.
      ENDIF.

    WHEN '&EXCEL'.

      PERFORM z_importa_excel CHANGING l_erro.

      CHECK l_erro IS INITIAL.

      REFRESH it_arq_material.
      CLEAR wa_arq_material.

*----------------------------------------
* upload excel
*----------------------------------------
      CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
        EXPORTING
          filename                = p_file
          i_begin_col             = 1
          i_begin_row             = 1
          i_end_col               = 100
          i_end_row               = 30000
*         i_end_col               = 256
*         i_end_row               = 65536
        TABLES
          intern                  = t_tab
        EXCEPTIONS
          inconsistent_parameters = 1
          upload_ole              = 2
          OTHERS                  = 3.

      IF sy-subrc <> 0.
        l_erro = abap_true.
        MESSAGE s024(sd) WITH 'Falha ao abrir Planilha Excel.' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

*----------------------------------------
* carrega tabela interna
*----------------------------------------
      FREE: l_erro, l_cols, l_rows.

      LOOP AT t_tab INTO w_tab.
        l_cols = l_cols + 1.
        ASSIGN COMPONENT w_tab-col OF STRUCTURE wa_arq_material TO <fs_fld>.
        <fs_fld> = w_tab-value.
        AT END OF row.
          l_rows          = l_rows + 1.
          APPEND wa_arq_material  TO it_arq_material.
          FREE: l_cols, wa_arq_material.
        ENDAT.
      ENDLOOP.

      READ TABLE it_arq_material INTO wa_arq_material INDEX 1.
      IF sy-subrc = 0.
        DELETE it_arq_material INDEX 1.
      ENDIF.

*      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
*        EXPORTING
*          i_line_header        = 'X'
*          i_tab_raw_data       = it_raw
*          i_filename           = p_file
*        TABLES
*          i_tab_converted_data = it_arq_material
*        EXCEPTIONS
*          conversion_failed    = 1
*          OTHERS               = 2.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.

      CHECK it_arq_material[] IS NOT INITIAL.

      REFRESH: it_salvar_02, it_zsdt0300.
      CLEAR wa_salvar_02.

      DELETE FROM zsdt0300 WHERE usuario = sy-uname.

      CLEAR l_erro.

      LOOP AT it_arq_material INTO wa_arq_material.

        DATA(l_linha) = sy-tabix.

        CLEAR: wa_salvar_02, wa_zsdt0300.

        wa_arq_material-matnr = |{ wa_arq_material-matnr ALPHA = IN }|.

        IF wa_arq_material-id_produto > '999999999'.
          wa_arq_material-id_produto = '999999999'.
        ENDIF.

        REPLACE ALL OCCURRENCES OF '.' IN wa_arq_material-id_produto WITH ''.

        l_id_produto               = wa_arq_material-id_produto.

        wa_zsdt0300-mandt          = sy-mandt.
        wa_zsdt0300-usuario        = sy-uname.
        wa_zsdt0300-linha          = l_linha.
        wa_zsdt0300-matnr          = wa_arq_material-matnr.
        wa_zsdt0300-id_matnr_idea  = wa_arq_material-id_produto.
        wa_zsdt0300-id_matnr_agriq = wa_arq_material-produtoid.

*       SELECT SINGLE *
*         FROM zsdt0210
*         INTO wzt210
*        WHERE id_matnr_idea  EQ wa_arq_material-id_produto
*          AND id_matnr_agriq EQ wa_arq_material-produtoid   "*-CS2021000218-12.09.2022-#89825-JT-inicio
*          AND matnr          EQ wa_arq_material-matnr.
*       IF sy-subrc <> 0.

*-CS2021000218-12.09.2022-#89825-JT-inicio
        IF wa_arq_material-produtoid IS NOT INITIAL.
          SELECT SINGLE *
            INTO @DATA(w2_0299)
            FROM zsdt0299
           WHERE produtoid EQ @wa_arq_material-produtoid.
          IF sy-subrc = 0.
            wa_salvar_02-id_matnr_agriq = wa_arq_material-produtoid.
          ELSE.
            l_erro = abap_true.
            wa_zsdt0300-mensagem = 'Cod.AgriQ não encontrado: ' && wa_arq_material-produtoid.
          ENDIF.
        ENDIF.
*-CS2021000218-12.09.2022-#89825-JT-fim

        SELECT SINGLE *
          FROM mara
          INTO wa_mara
         WHERE matnr EQ wa_arq_material-matnr
           AND matkl IN ('700130', '700280', '700230', '700240', '700350','658445').

        IF sy-subrc = 0 .

          IF wa_arq_material-matnr IS NOT INITIAL.
            SELECT SINGLE * FROM makt INTO wmakt
              WHERE matnr EQ wa_arq_material-matnr
               AND  spras EQ sy-langu.

            IF sy-subrc = 0.
              wa_salvar_02-mandt   = sy-mandt.
              wa_salvar_02-matnr   = wa_arq_material-matnr.
            ELSE.
              l_erro = abap_true.
              wa_zsdt0300-mensagem = 'Material não encontrado: ' && wa_arq_material-matnr.
            ENDIF.
          ENDIF.

          IF wa_arq_material-id_produto IS NOT INITIAL.

            CASE wa_mara-matkl.
              WHEN '700130' OR '700280' OR '700230' OR '700240' OR '700350'.

                SELECT SINGLE * FROM zsdt0198 INTO wa_0198
                  WHERE id_cultivar EQ l_id_produto. "wa_arq_material-id_produto.

                IF sy-subrc = 0.
                  wa_salvar_02-id_matnr_idea = wa_arq_material-id_produto.
                ELSE.
                  l_erro = abap_true.
                  wa_zsdt0300-mensagem = 'Cod.INDEA não encontrado: ' && wa_arq_material-id_produto.
                ENDIF.

              WHEN '658445'.
                SELECT SINGLE *  FROM zsdt0201 INTO wa_0201
                 WHERE id_produto EQ l_id_produto. "wa_arq_material-id_produto.

                IF sy-subrc = 0.
                  wa_salvar_02-id_matnr_idea = wa_arq_material-id_produto.
                ELSE.
                  l_erro = abap_true.
                  wa_zsdt0300-mensagem = 'Cod.INDEA não encontrado: ' && wa_arq_material-id_produto.
                ENDIF.
            ENDCASE.

*           IF wa_salvar_02-matnr IS NOT INITIAL AND wa_salvar_02-id_matnr_idea IS NOT INITIAL.
*             APPEND wa_salvar_02 TO it_salvar_02.
*           ENDIF.
          ENDIF.
        ELSE.
          l_erro = abap_true.
          wa_zsdt0300-mensagem = 'Material não encontrado: ' && wa_arq_material-matnr.
        ENDIF.

        IF wa_zsdt0300-mensagem IS INITIAL.
          APPEND wa_salvar_02 TO it_salvar_02.
        ELSE.
          APPEND wa_zsdt0300  TO it_zsdt0300.
        ENDIF.

        CLEAR: wa_arq_material, wa_saida_02.
*        ENDIF.
      ENDLOOP.

      IF l_erro = abap_true.
        MODIFY zsdt0300       FROM TABLE it_zsdt0300.
        DELETE FROM zsdt0300 WHERE usuario <> sy-uname.
        COMMIT WORK.

        l_mesg1 = 'Há linhas na Planilha que estão com erro.'.
        l_mesg2 = 'As linhas a serem apresentadas não foram importadas!'.

        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
          EXPORTING
            titel        = 'Aviso'
            textline1    = l_mesg1
            textline2    = l_mesg2
*           textline3    = l_mesg3
            start_column = 25
            start_row    = 6.

        CALL FUNCTION 'ZREGISTER_DATA'
          EXPORTING
            i_db_tab    = 'ZSDT0300'
            i_stcnam    = 'ZSDT0300'
            i_title     = 'Log de Erros'
            i_start_lin = 04
            i_start_col = 40
            i_end_lin   = 16
            i_end_col   = 140
          EXCEPTIONS
            error       = 1
            OTHERS      = 2.
      ELSE.
        MESSAGE 'Importação feita com sucesso!' TYPE 'I'.
      ENDIF.

      CHECK it_salvar_02[] IS NOT INITIAL.

      MODIFY zsdt0210 FROM TABLE it_salvar_02.

      PERFORM seleciona_dados_material.

*-US192246-06.10.2025-#1922468-JT-inicio
    WHEN '&LOG'.
      CALL METHOD g_grid02->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF tg_selectedrow IS INITIAL.
        MESSAGE s024(sd) WITH 'Favor selecione uma linha!' DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.

      IF lines( tg_selectedrow ) > 1.
        MESSAGE s024(sd) WITH 'Favor selecione Somente uma linha!' DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.

      READ TABLE tg_selectedrow INTO wg_selectedrow INDEX 1.
      READ TABLE it_saida_02 INTO wa_saida_02 INDEX  wg_selectedrow-index.

      CALL FUNCTION 'Z_ANALISE_LOGS_TABLE'
        EXPORTING
          cusobj     = 'ZSDT0210'
          tabfirst   = abap_true
          especifico = abap_true
          matnr      = wa_saida_02-matnr.
*-US192246-06.10.2025-#1922468-JT-fim

  ENDCASE.

  CALL METHOD g_grid02->refresh_table_display
    EXPORTING
      is_stable = wa_stable02.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0103 OUTPUT.

  DATA: lt_f4 TYPE lvc_t_f4,
        wl_f4 TYPE lvc_s_f4.


  SET PF-STATUS 'ST_0103'.
  SET TITLEBAR 'TL_0103'.


  PERFORM z_cria_alv03.

  IF g_custom_container03 IS INITIAL.

    CREATE OBJECT g_custom_container03
      EXPORTING
        container_name              = 'CONTAINER03'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid03 IS INITIAL AND g_custom_container03 IS NOT  INITIAL.
      CREATE OBJECT g_grid03
        EXPORTING
          i_parent          = g_custom_container03
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    wl_function03  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function03 TO tl_function03.
    wl_function03  = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function03 TO tl_function03.

    gs_layout03-stylefname = 'CELLTAB'.

    wl_f4-fieldname  = 'BUKRS'.
    wl_f4-register   = 'X'.
    wl_f4-getbefore  = 'X'.
    APPEND wl_f4 TO  lt_f4.


    CALL METHOD g_grid03->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant03
        is_layout            = gs_layout03
        it_toolbar_excluding = tl_function03
      CHANGING
        it_outtab            = it_saida_03
        it_fieldcatalog      = it_fieldcatalog03.

    SET HANDLER lcl_event_f403=>on_f4_03 FOR g_grid03.

    SET HANDLER: lcl_event_handler03=>on_data_changed03 FOR g_grid03,
                 lcl_event_handler03=>on_data_changed_finished03 FOR g_grid03.



    CALL METHOD g_grid03->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid03->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid03->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.


    CALL METHOD g_grid03->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

  ELSE.

    CALL METHOD g_grid03->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcatalog03.


    CALL METHOD g_grid03->refresh_table_display
      EXPORTING
        is_stable = wa_stable03.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.

  DATA: wa_salvar TYPE zsdt0211,
        it_salvar TYPE TABLE OF zsdt0211.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SALVAR'.
      LOOP AT it_saida_03 INTO wa_saida_03.

        SELECT SINGLE * FROM zsdt0211 INTO @DATA(wzt211)
          WHERE bukrs EQ @wa_saida_03-bukrs
           AND  lifnr EQ @wa_saida_03-lifnr.

        IF sy-subrc <> 0.

          IF wa_saida_03-bukrs IS INITIAL.
            MESSAGE 'Favor informar a empresa!' TYPE 'I'.
            EXIT.
          ELSE.
            SELECT SINGLE * FROM t001 INTO @DATA(wt001)
              WHERE bukrs EQ @wa_saida_03-bukrs.

            IF sy-subrc = 0.
              wa_salvar-bukrs  =  wa_saida_03-bukrs.
            ELSE.
              MESSAGE 'Empresa não existe!' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.

          IF wa_saida_03-lifnr IS INITIAL.
            MESSAGE 'Favor informar o Codigo Fornecedor!' TYPE 'I'.
            EXIT.
          ELSE.
            SELECT SINGLE * FROM lfa1 INTO @DATA(wlfa1)
              WHERE lifnr EQ @wa_saida_03-lifnr.

            IF sy-subrc = 0.
              wa_salvar-lifnr  =  wa_saida_03-lifnr.
            ELSE.
              MESSAGE 'Fornecedor não existe!' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.

          IF  wa_salvar-bukrs IS NOT INITIAL AND wa_salvar-lifnr IS NOT INITIAL.
            APPEND wa_salvar TO it_salvar.
          ENDIF.
        ENDIF.

        CLEAR: wa_saida_03, wa_salvar.
      ENDLOOP.

      IF it_salvar IS NOT INITIAL.

        INSERT zsdt0211 FROM TABLE it_salvar.
        COMMIT WORK.
        MESSAGE 'Dados gravado com Sucesso!' TYPE 'S'.

        LOOP AT it_saida_03 INTO wa_saida_03.
          FREE wa_saida_03-celltab.
          gt_estilo03 =  VALUE #( ( fieldname = 'BUKRS' style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'LIFNR' style = cl_gui_alv_grid=>mc_style_disabled  )  ).
          INSERT LINES OF gt_estilo03 INTO TABLE wa_saida_03-celltab.
          MODIFY it_saida_03 FROM wa_saida_03.
        ENDLOOP.
      ENDIF.

    WHEN '&INS'.
      APPEND INITIAL LINE TO it_saida_03.
    WHEN '&DEL'.

      CALL METHOD g_grid03->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF tg_selectedrow IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
        EXIT.
      ELSE.
        LOOP AT tg_selectedrow INTO wg_selectedrow.

          READ TABLE it_saida_03 INTO wa_saida_03 INDEX wg_selectedrow-index.

          DELETE FROM zsdt0211  WHERE lifnr = wa_saida_03-lifnr AND
                                      bukrs = wa_saida_03-bukrs.

        ENDLOOP.

        REFRESH it_saida_03.
        CLEAR wa_saida_03.

        PERFORM seleciona_dados_ure.

      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_ALV03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_cria_alv03.

  REFRESH it_fieldcatalog03.
  PERFORM preenche_cat03 USING:  "'T001'     'BUKRS'
        'BUKRS'    'Empresa'              '07'     ''     ''     ''     ''   'X'     ''         ''         'X',
        'BUTXT'    'Desc.Empresa'         '25'     ''     ''     ''     ''   ' '     ''         ''         '',
        'LIFNR'    'Cód.Fornecedor'       '12'     ''     ''     ''     ''   'X'     'LFA1'     'LIFNR'    '',
        'NAME1'    'Desc.Fornecedor'      '25'     ''     ''     ''     ''   ' '     ''         ''         '',
        'STCD1'    'CNPJ'                 '14'     ''     ''     ''     ''   ' '     ''         ''         '',
        'ORT01'    'Cidade'               '20'     ''     ''     ''     ''   ' '     ''         ''         '',
        'STRAS'    'Endereço'             '25'     ''     ''     ''     ''   ' '     ''         ''         '',
        'REGIO'    'Estado'               '07'     ''     ''     ''     ''   ' '     ''         ''         ''.

ENDFORM.


FORM preenche_cat03 USING VALUE(p_campo)
                          VALUE(p_desc)
                          VALUE(p_tam)
                          VALUE(p_zero)
                          VALUE(p_hot)
                          VALUE(p_sum)
                          VALUE(p_just)
                          VALUE(p_edit)
                          VALUE(p_table)
                          VALUE(p_fieldname)
                          VALUE(p_f4).

  wa_fieldcatalog03-fieldname   = p_campo.
  wa_fieldcatalog03-coltext     = p_desc.
  wa_fieldcatalog03-scrtext_l   = p_desc.
  wa_fieldcatalog03-scrtext_m   = p_desc.
  wa_fieldcatalog03-scrtext_s   = p_desc.
  wa_fieldcatalog03-outputlen   = p_tam.
  wa_fieldcatalog03-hotspot     = p_hot.
  wa_fieldcatalog03-no_zero     = p_zero.
  wa_fieldcatalog03-do_sum      = p_sum.
  wa_fieldcatalog03-just        = p_just.
  wa_fieldcatalog03-edit        = p_edit.
  wa_fieldcatalog03-ref_table   = p_table.
  wa_fieldcatalog03-ref_field   = p_fieldname.
  wa_fieldcatalog03-f4availabl  = p_f4.


  APPEND wa_fieldcatalog03 TO it_fieldcatalog03.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_URE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados_roteiro.

  DATA: lr_kunnr TYPE zde_kunnr_t.

*-US191683-25.09.2025-#191683-JT-inicio
  lr_kunnr[] = p_kunnr[].

  zcl_indea=>selecao_dados_roteiro( EXPORTING i_kunnr    = lr_kunnr
                                    IMPORTING e_saida    = it_saida_01
                                              e_zsdt0132 = it_zsdt0132
                                              e_zsdt0207 = it_zsdt0207 ).
*-US191683-25.09.2025-#191683-JT-fim

*-US191683-25.09.2025-#191683-JT-inicio
*  SELECT *
*    FROM zsdt0132 INTO TABLE it_zsdt0132
*  WHERE ( id_propriedade <> '0' )
*    AND ( kunnr IN p_kunnr )
*    AND   status = 'A'. "ATIVO
*
*  CHECK it_zsdt0132 IS NOT INITIAL.
*
*  SELECT *
*    FROM zsdt0207 INTO TABLE it_zsdt0207
*     FOR ALL ENTRIES IN it_zsdt0132
*   WHERE id_propriedade EQ  it_zsdt0132-id_propriedade.
*
*
*  LOOP AT it_zsdt0132 INTO wa_zsdt0132.
*
*    wa_saida_01-nr_rot     = wa_zsdt0132-nr_rot.
*    wa_saida_01-rot_desc   = wa_zsdt0132-rot_desc.
*    wa_saida_01-kunnr      = |{ wa_zsdt0132-kunnr ALPHA = IN }|.
*    wa_saida_01-city1      = wa_zsdt0132-city1.
*    wa_saida_01-uf         = wa_zsdt0132-uf.
*    wa_saida_01-tel_number = wa_zsdt0132-tel_number.
*    wa_saida_01-status = wa_zsdt0132-status.
*
*
*    SELECT SINGLE *
*      FROM kna1 INTO @DATA(wa_kna1)
*    WHERE kunnr EQ  @wa_saida_01-kunnr .
*
*    IF sy-subrc = 0.
*      wa_saida_01-name1 = wa_kna1-name1.
*    ENDIF.
*
*    READ TABLE it_zsdt0207 INTO wa_zsdt0207 WITH KEY  id_propriedade = wa_zsdt0132-id_propriedade.
*    IF sy-subrc = 0.
*
*      IF wa_zsdt0207-inativo IS NOT INITIAL.
*
*        wa_saida_01-color = 'C600'.
*
*      ENDIF.
*
*      wa_saida_01-id_propriedade   = wa_zsdt0207-id_propriedade.
*      wa_saida_01-nome             = wa_zsdt0207-nome.
*      wa_saida_01-via_acesso       = wa_zsdt0207-via_acesso.
*      wa_saida_01-municipio        = wa_zsdt0207-municipio.
*    ENDIF.
*
*    IF wa_zsdt0207-via_acesso IS INITIAL.
*      wa_saida_01-texto02    = '@1F@'.
*    ELSE.
*      wa_saida_01-texto02    = '@1E@'.
*    ENDIF.
*
*
*    wl_name =  wa_zsdt0132-nr_rot.
*
*    CALL FUNCTION 'READ_TEXT'
*      EXPORTING
*        id                      = 'ZROT'
*        language                = sy-langu
*        name                    = wl_name
*        object                  = 'ZSDROTEIRO'
*      TABLES
*        lines                   = it_texto
*      EXCEPTIONS
*        id                      = 1
*        language                = 2
*        name                    = 3
*        not_found               = 4
*        object                  = 5
*        reference_check         = 6
*        wrong_access_to_archive = 7
*        OTHERS                  = 8.
*
*    IF it_texto IS INITIAL.
*      wa_saida_01-texto01 = '@1F@'.
*    ELSE.
*      wa_saida_01-texto01 = '@1E@'.
*    ENDIF.
*
*    FREE wa_saida_01-celltab.
*    gt_estilo01 =  VALUE #( ( fieldname = 'KUNNR'           style = cl_gui_alv_grid=>mc_style_disabled  )
*                            ( fieldname = 'NR_ROT'          style = cl_gui_alv_grid=>mc_style_disabled  )
*                            ( fieldname = 'ID_PROPRIEDADE'  style = cl_gui_alv_grid=>mc_style_disabled  ) ).
*    INSERT LINES OF gt_estilo01 INTO TABLE wa_saida_01-celltab.
*
*    APPEND wa_saida_01 TO it_saida_01.
*    CLEAR: wa_saida_01, wa_zsdt0207, wa_zsdt0132.
*  ENDLOOP.
*-US191683-25.09.2025-#191683-JT-fim

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZA_TAB_INDEA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_atualiza_rtc_siagri.

  DATA: l_codmapa   TYPE string.

  FREE: l_erro.

*-CS2021000218-01.09.2022-#89886-JT-inicio
  CASE abap_true.

*-----------------------
*-- produtos AGRIQ a partir de data
*-----------------------
    WHEN p_prod1.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = sy-tabix
          text       = 'Atualizando Tabelas AgriQ...'.

      TRY .
          zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
             )->set_exec_agriq( EXPORTING i_metodo  = 'GET_ALL_PROD'
                                          i_ativo   = abap_true
                                          i_data    = p_data ).

        CATCH zcx_integracao INTO DATA(ex_integra).
          l_erro = abap_true.
          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_error INTO DATA(ex_error).    "  "
          l_erro = abap_true.
          ex_error->zif_error~published_erro(   i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

      IF l_erro = abap_false.
        MESSAGE s024(sd) WITH TEXT-100.
      ENDIF.

*-----------------------
*-- produtos AGRIQ dado um Cod.MAPA
*-----------------------
    WHEN p_prod2.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = sy-tabix
          text       = 'Atualizando Tabelas AgriQ...'.

      l_codmapa = p_mapa2.

      TRY .
          zcl_integracao_agriq=>zif_integracao_agriq~get_instance(
             )->set_exec_agriq( EXPORTING i_metodo  = 'GET_MAPA'
                                          i_codmapa = l_codmapa ).

        CATCH zcx_integracao INTO ex_integra.
          l_erro = abap_true.
          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_error INTO ex_error.    "  "
          l_erro = abap_true.
          ex_error->zif_error~published_erro(   i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

      IF l_erro = abap_false.
        MESSAGE s024(sd) WITH TEXT-100.
      ENDIF.

*-----------------------
*-- atualizar RTC Siagri
*-----------------------
    WHEN p_rtc.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = sy-tabix
          text       = 'Atualizando Tabelas RTC Siagri...'.

      CLEAR it_rsparams[].
      wa_rsparams-selname = 'P_EVENTO'.
      wa_rsparams-kind    = 'P'.
      wa_rsparams-sign    = 'I'.
      wa_rsparams-option  = 'EQ'.
      wa_rsparams-low     = '2'.
      wa_rsparams-high    = ' '.
      APPEND wa_rsparams TO it_rsparams.

      SUBMIT zsdr023 WITH SELECTION-TABLE it_rsparams AND RETURN.

*     MESSAGE 'Dados gravado com Sucesso!' TYPE 'S'.
  ENDCASE.
*-CS2021000218-01.09.2022-#89886-JT-fim

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZA_TAB_INDEA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_atualiza_tab_indea.

  DATA: lr_kunnr TYPE zde_kunnr_t,
        lr_espec TYPE zde_id_especie_t,
        lv_msg   TYPE string.

*-US191683-25.09.2025-#191683-JT-inicio
  lr_kunnr[] = s_kunnr[].
  lr_espec[] = s_espec[].

  zcl_indea=>atualizar_tabelas_indea( EXPORTING i_esp      = s_esp    i_cult = s_cult i_cat   = s_cat   i_tpem   = s_tpem   i_produt = s_produt
                                                i_tpapli   = s_tpapli i_unid = s_unid i_praga = s_praga i_p_auto = s_p_auto i_prod   = s_prod
                                                i_propri   = s_propri i_ure  = s_ure  i_saldo = s_saldo i_todos  = s_todos
                                                i_kunnr    = lr_kunnr
                                                i_espec    = lr_espec
                                      IMPORTING e_erro     = DATA(lv_erro) ).
  IF lv_erro = abap_true.
    RETURN.
  ENDIF.
*-US191683-25.09.2025-#191683-JT-fim

*-US191683-25.09.2025-#191683-JT-inicio
*  CLEAR it_rsparams[].
*
*  IF s_kunnr IS NOT INITIAL.
*
*    SELECT *
*      FROM kna1
*      INTO TABLE @DATA(lt_kna1)
*      WHERE kunnr IN @s_kunnr.
*    IF sy-subrc IS INITIAL.
*
*      SORT lt_kna1 BY kunnr.
*      LOOP AT s_kunnr.
*
*        READ TABLE lt_kna1 TRANSPORTING NO FIELDS
*        WITH KEY kunnr = s_kunnr-low
*        BINARY SEARCH.
*        IF sy-subrc IS NOT INITIAL.
*
*          MESSAGE s024(sd) WITH TEXT-e01 s_kunnr-low TEXT-e02 DISPLAY LIKE 'E'.
*          RETURN.
*        ELSE.
*
*          wa_rsparams-selname = 'S_KUNNR'.
*          wa_rsparams-kind    = 'S'.
*          wa_rsparams-sign    = 'I'.
*          wa_rsparams-option  = 'EQ'.
*          wa_rsparams-low     = s_kunnr-low.
*          APPEND wa_rsparams TO it_rsparams.
*          CLEAR wa_rsparams.
*
*        ENDIF.
*
*      ENDLOOP.
*    ELSE.
*
*      MESSAGE s024(sd) WITH TEXT-e03 DISPLAY LIKE 'E'.
*      RETURN.
*
*    ENDIF.
*
*  ENDIF.
*
**** Inicio - Rubenilson Pereira - 07.04.25 #168932
*  LOOP AT s_espec[] ASSIGNING FIELD-SYMBOL(<fs_espec>).
*    wa_rsparams-selname = 'S_ESPEC'.
*    wa_rsparams-kind    = 'S'.
*    wa_rsparams-sign    = 'I'.
*    wa_rsparams-option  = 'EQ'.
*    wa_rsparams-low     = <fs_espec>-low.
*    APPEND wa_rsparams TO it_rsparams.
*    CLEAR wa_rsparams.
*  ENDLOOP.
**** Fim - Rubenilson Pereira - 07.04.25 #168932
*
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      percentage = sy-tabix
*      text       = 'Atualizando Tabelas Indea...'.
*
*
*  wa_rsparams-selname = 'P_TABELA'.
*  wa_rsparams-kind    = 'S'.
*  wa_rsparams-sign    = 'I'.
*  wa_rsparams-option  = 'EQ'.
*
*  IF s_esp = true.    "Especie
*
*    wa_rsparams-low     = 'INDEA_CONSULTA_ESPECIE'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*
*  ELSEIF s_cult  = true.     "Cultivar
*    wa_rsparams-low     = 'INDEA_CONSULTA_CULTIVAR'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF s_cat  = true.     "Categoria Sementes
*    wa_rsparams-low     = 'INDEA_CONSULTA_CAT_SEMENTES'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF s_tpem  = true.   "Tipo Embalagam
*    wa_rsparams-low     = 'INDEA_CONSULTA_SITUACAO_EMBALAGEM'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF s_produt  = true.   "Produto Autorizado
*    wa_rsparams-low     = 'INDEA_CONSULTA_PRODUTO'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF s_tpapli  = true.  "Tipo Aplicação,
*    wa_rsparams-low     = 'INDEA_CONSULTA_TIPO_APLICACAO'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF s_unid  = true. "unidade de medida
*    wa_rsparams-low     = 'INDEA_CONSULTA_UNIDADE_MEDIDA'.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-high    = ' '.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF s_praga  = true.   "Praga
*    wa_rsparams-low     = 'INDEA_CONSULTA_PRAGA'.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-high    = ' '.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF s_p_auto = true.  "Pessoa autorizada
*    wa_rsparams-low     = 'INDEA_CONSULTA_PESSOA_AUTORIAZADA'.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-high    = ' '.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF s_prod  = true.  "Produtor
*    wa_rsparams-low     = 'INDEA_CONSULTA_PRODUTOR'.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-high    = ' '.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF s_propri  = true.  "Propriedade
*    wa_rsparams-low     = 'INDEA_CONSULTA_PROPRIEDADE'.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-high    = ' '.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF s_ure      = true.  "URE
*    wa_rsparams-low     = 'INDEA_CONSULTA_URE'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF  s_saldo    = true.  "Saldo Revenda
*    wa_rsparams-low     = 'INDEA_CONSULTA_SALDO_REVENDA'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*
*  ELSEIF s_todos  = true.
*    wa_rsparams-selname = 'P_TABELA'.
*    wa_rsparams-kind    = 'S'.
*    wa_rsparams-sign    = 'I'.
*    wa_rsparams-option  = 'EQ'.
*    wa_rsparams-low     = 'INDEA_CONSULTA_ESPECIE'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_CULTIVAR'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_CAT_SEMENTES'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_SITUACAO_EMBALAGEM'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_PRODUTO'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_TIPO_APLICACAO'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_UNIDADE_MEDIDA'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_PRAGA'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_PESSOA_AUTORIAZADA'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_PRODUTOR'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_PROPRIEDADE'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_URE'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    wa_rsparams-low     = 'INDEA_CONSULTA_SALDO_REVENDA'.
*    wa_rsparams-high    = ' '.
*    APPEND wa_rsparams TO it_rsparams.
*    SUBMIT zsdr019 WITH SELECTION-TABLE it_rsparams AND RETURN.
*  ENDIF.
*-US191683-25.09.2025-#191683-JT-fim

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_URE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados_ure .

  SELECT *
    FROM zsdt0211 INTO TABLE it_zsdt0211.

  CHECK it_zsdt0211 IS NOT INITIAL.

  SELECT *
    FROM t001 INTO TABLE @DATA(it_t001)
    FOR ALL ENTRIES IN @it_zsdt0211
   WHERE bukrs EQ @it_zsdt0211-bukrs.


  SELECT *
    FROM lfa1 INTO TABLE @DATA(it_lfa1)
   FOR ALL ENTRIES IN @it_zsdt0211
  WHERE lifnr EQ @it_zsdt0211-lifnr.


  LOOP AT it_zsdt0211 INTO wa_zsdt0211.

    READ TABLE it_t001 INTO DATA(wa_t001) WITH KEY bukrs = wa_zsdt0211-bukrs.
    IF sy-subrc = 0.
      wa_saida_03-bukrs  = wa_t001-bukrs.
      wa_saida_03-butxt  = wa_t001-butxt.
    ENDIF.

    READ TABLE it_lfa1 INTO DATA(wa_lfa1) WITH KEY lifnr = wa_zsdt0211-lifnr.
    IF sy-subrc = 0.
      wa_saida_03-lifnr  =  wa_lfa1-lifnr.
      wa_saida_03-name1  =  wa_lfa1-name1.
      wa_saida_03-stcd1  =  wa_lfa1-stcd1.
      wa_saida_03-ort01  =  wa_lfa1-ort01.
      wa_saida_03-stras  =  wa_lfa1-stras.
      wa_saida_03-regio  =  wa_lfa1-regio.
    ENDIF.

    FREE wa_saida_03-celltab.
    gt_estilo03 =  VALUE #( ( fieldname = 'BUKRS' style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'LIFNR' style = cl_gui_alv_grid=>mc_style_disabled  )  ).
    INSERT LINES OF gt_estilo03 INTO TABLE wa_saida_03-celltab.


    APPEND wa_saida_03 TO it_saida_03.
    CLEAR: wa_saida_03, wa_zsdt0211, wa_t001, wa_lfa1.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: wa_event    TYPE REF TO lcl_event_handler01.
  DATA: lt_f401  TYPE lvc_t_f4,
        wl_f401  TYPE lvc_s_f4,
        tl_fcode TYPE TABLE OF sy-ucomm, "*-US191683-25.09.2025-#191683-JT-inicio
        wl_fcode TYPE sy-ucomm.          "*-US191683-25.09.2025-#191683-JT-inicio

*-US191683-25.09.2025-#191683-JT-inicio
  FREE: tl_fcode.
  IF p_submit = abap_true.
    APPEND '&INS'   TO tl_fcode.
    APPEND '&DEL'   TO tl_fcode.
    APPEND '&EXCEL' TO tl_fcode.
  ENDIF.
*-US191683-25.09.2025-#191683-JT-fim

  SET  PF-STATUS 'ST_0100' EXCLUDING tl_fcode.  "*-US191683-25.09.2025-#191683-JT
  SET TITLEBAR 'TL_0100'.

  PERFORM z_cria_alv01.

  IF g_custom_container01 IS INITIAL.

    CREATE OBJECT g_custom_container01
      EXPORTING
        container_name              = 'CONTAINER01'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid01 IS INITIAL AND g_custom_container01 IS NOT  INITIAL.
      CREATE OBJECT g_grid01
        EXPORTING
          i_parent          = g_custom_container01
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

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

    gs_layout01-stylefname = 'CELLTAB'.
    gs_layout01-info_fname = 'COLOR'.


    wl_f401-fieldname  = 'ID_PROPRIEDADE'.
    wl_f401-register   = 'X'.
    wl_f401-getbefore  = 'X'.
    APPEND wl_f401 TO  lt_f401.
    CLEAR wl_f401.

    wl_f401-fieldname  = 'KUNNR'.
    wl_f401-register   = 'X'.
    wl_f401-getbefore  = 'X'.
    APPEND wl_f401 TO  lt_f401.
    CLEAR wl_f401.

    wl_f401-fieldname  = 'NR_ROT'.
    wl_f401-register   = 'X'.
    wl_f401-getbefore  = 'X'.
    APPEND wl_f401 TO  lt_f401.
    CLEAR wl_f401.


    IF wa_event IS INITIAL.
      CREATE OBJECT wa_event.
      SET HANDLER: wa_event->on_button_click01  FOR g_grid01.
    ENDIF.

    CALL METHOD g_grid01->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant01
        is_layout            = gs_layout01
        it_toolbar_excluding = tl_function01
      CHANGING
        it_outtab            = it_saida_01
        it_fieldcatalog      = it_fieldcatalog01.


    SET HANDLER lcl_event_f401=>on_f4_01 FOR g_grid01.

    SET HANDLER: lcl_event_handler01=>on_data_changed01 FOR g_grid01,
                 lcl_event_handler01=>on_data_changed_finished01  FOR g_grid01.


    CALL METHOD g_grid01->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid01->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid01->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD g_grid01->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f401[].

  ELSE.

    CALL METHOD g_grid01->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcatalog01.

    CALL METHOD g_grid01->refresh_table_display
      EXPORTING
        is_stable = wa_stable01.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA lv_erro      TYPE c.
  DATA wa_salvar01  TYPE zsdt0132.
  DATA it_salvar01  TYPE TABLE OF  zsdt0132.
  DATA wa_excluir01 TYPE zsdt0132.

  FREE: lv_erro.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SALVAR'.

      LOOP AT it_saida_01 INTO wa_saida_01.

        IF  wa_saida_01-nr_rot IS INITIAL.
          lv_erro = abap_true.
          MESSAGE 'Favor informar o Nr Roteiro!' TYPE 'I'.
          EXIT.
        ELSEIF  wa_saida_01-id_propriedade IS INITIAL.
          lv_erro = abap_true.
          MESSAGE 'Favor informar o Cód. Propriedade!' TYPE 'I'.
          EXIT.
        ELSE.
          SELECT SINGLE * FROM  zsdt0132 INTO @DATA(wa_0132)
             WHERE nr_rot  EQ @wa_saida_01-nr_rot
               AND kunnr   EQ @wa_saida_01-kunnr.
          IF sy-subrc <> 0.
            lv_erro = abap_true.
            MESSAGE 'Numero de roteiro não existe!' TYPE 'I'.
            EXIT.
          ELSE.
            wa_saida_01-status = wa_0132-status.
            wa_saida_01-tel_number = wa_0132-tel_number.

            SELECT SINGLE * FROM  zsdt0207 INTO  @DATA(wa_0207)
               WHERE id_propriedade EQ  @wa_saida_01-id_propriedade.

            IF sy-subrc = 0.
              MOVE-CORRESPONDING wa_0132     TO wa_salvar01.  "*-US191683-25.09.2025-#191683-JT-inicio
              MOVE-CORRESPONDING wa_saida_01 TO wa_salvar01.
              wa_salvar01-data_atual = sy-datum.
              wa_salvar01-usnam      = sy-uname.
              wa_salvar01-hora_atual = sy-uzeit.

              APPEND wa_salvar01 TO it_salvar01.
            ELSE.
              lv_erro = abap_true.
              MESSAGE 'Id Propriedade não existe' TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: wa_salvar01, wa_saida_01.
      ENDLOOP.

*     IF it_salvar01 IS NOT INITIAL.
      IF lv_erro = abap_false.
        MODIFY zsdt0132 FROM TABLE it_salvar01.
        MESSAGE 'Dados gravado com Sucesso!' TYPE 'S'.

        LOOP AT it_saida_01 INTO wa_saida_01.
          FREE wa_saida_01-celltab.
          gt_estilo01 =  VALUE #( ( fieldname = 'NR_ROT'         style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'KUNNR'          style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'ID_PROPRIEDADE' style = cl_gui_alv_grid=>mc_style_disabled  )  ).
          INSERT LINES OF gt_estilo01 INTO TABLE wa_saida_01-celltab.
          MODIFY it_saida_01 FROM wa_saida_01.
        ENDLOOP.
        LEAVE TO SCREEN 0.  "*-US191683-25.09.2025-#191683-JT
      ENDIF.

    WHEN '&INS'.
      CLEAR wa_saida_01.
      wa_saida_01-texto02 = '@1F@'.
      wa_saida_01-texto01 = '@1F@'.
      APPEND wa_saida_01 TO it_saida_01.

    WHEN '&DEL'.

      CALL METHOD g_grid01->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF tg_selectedrow IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
        EXIT.
      ELSE.
        LOOP AT tg_selectedrow INTO wg_selectedrow.
          READ TABLE  it_saida_01 INTO wa_saida_01 INDEX  wg_selectedrow-index.

          MOVE-CORRESPONDING wa_saida_01 TO wa_excluir01.

*-US191683-25.09.2025-#191683-JT-inicio
          UPDATE zsdt0132 SET id_propriedade  = 0
                              data_atual      = sy-datum
                              usnam           = sy-uname
                              hora_atual      = sy-uzeit
                        WHERE nr_rot          = wa_saida_01-nr_rot.

*         wa_excluir01-id_propriedade  = 0.
*         wa_excluir01-mandt           = sy-mandt.
*         wa_excluir01-data_atual      = sy-datum.
*         wa_excluir01-usnam           = sy-uname.
*         wa_excluir01-hora_atual      = sy-uzeit.
*         MODIFY zsdt0132 FROM  wa_excluir01.
*-US191683-25.09.2025-#191683-JT-fim

          DELETE it_saida_01 INDEX  wg_selectedrow-index.

          CLEAR wa_saida_01.
        ENDLOOP.

        CLEAR: wa_excluir01, wa_saida_01.
      ENDIF.

    WHEN '&EXCEL'.

      PERFORM z_importa_excel CHANGING l_erro.

      CHECK l_erro IS INITIAL.

      REFRESH it_arq_roteiro.
      CLEAR wa_arq_roteiro.

      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
        EXPORTING
          i_line_header        = 'X'
          i_tab_raw_data       = it_raw
          i_filename           = p_file
        TABLES
          i_tab_converted_data = it_arq_roteiro
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CHECK it_arq_roteiro IS NOT INITIAL.

      REFRESH it_salvar01.
      CLEAR wa_salvar01.


      LOOP AT it_arq_roteiro INTO wa_arq_roteiro.

        SELECT SINGLE * FROM  zsdt0132 INTO wa_0132
           WHERE nr_rot  EQ wa_arq_roteiro-nr_rot.

        IF sy-subrc <> 0.
          MESSAGE 'Numero de roteiro não existe!' TYPE 'I'.
        ELSE.

          SELECT SINGLE * FROM  zsdt0207 INTO wa_0207
             WHERE id_propriedade EQ wa_arq_roteiro-id_propriedade.

          IF sy-subrc = 0.
            MOVE-CORRESPONDING wa_arq_roteiro TO wa_salvar01.

            MOVE: wa_0132-rot_desc   TO wa_salvar01-rot_desc.
            MOVE: wa_0132-marca      TO wa_salvar01-marca.
            MOVE: wa_0132-tel_number TO wa_salvar01-tel_number.
            MOVE: wa_0132-city1      TO wa_salvar01-city1.
            MOVE: wa_0132-uf         TO wa_salvar01-uf.
            MOVE: sy-datum           TO wa_salvar01-data_atual.
            MOVE: sy-uzeit           TO wa_salvar01-hora_atual.
            MOVE: sy-uname           TO wa_salvar01-usnam.

            APPEND wa_salvar01 TO it_salvar01.
          ELSE.
            MESSAGE 'Id Propriedade não existe' TYPE 'I'.
          ENDIF.
        ENDIF.

        CLEAR: wa_arq_roteiro, wa_salvar01.
      ENDLOOP.

      CHECK it_salvar01 IS NOT INITIAL.
      MODIFY zsdt0132 FROM TABLE it_salvar01.
      MESSAGE 'Importação feita com sucesso!' TYPE 'I'.

      PERFORM seleciona_dados_roteiro.
  ENDCASE.
ENDMODULE.


FORM z_cria_alv01.

  REFRESH it_fieldcatalog01.

  PERFORM preenche_cat01 USING:
        'KUNNR'             'Cód.Cliente'              '10'     'X'    ''     ''     ''   'X'     ''    ''   'X',
        'NAME1'             'Desc.Cliente'             '25'     ''     ''     ''     ''   ''      ''    ''   '',
        'NR_ROT'            'Cód.Roteiro'              '10'     ''     ''     ''     ''   'X'     ''    ''   'X',
        'ROT_DESC'          'Descrição Roteiro'        '25'     ''     ''     ''     ''   ''      ''    ''   '',
        'TEXTO01'           'Roteiro'                  '06'     ''     ''     ''     ''   ''      ''    ''   '',
        'ID_PROPRIEDADE'    'Cód.Prop.Indea'           '12'     ''     ''     ''     ''   'X'     ''    ''   'X',
        'NOME'              'Descricao'                '40'     ''     ''     ''     ''   ''      ''    ''   '',
        'CITY1'             'Município Roteiro'        '25'     ''     ''     ''     ''   ''      ''    ''   '',
        'UF'                'UF'                       '05'     ''     ''     ''     ''   ''      ''    ''   '',
        'MUNICIPIO'         'Município Indea'          '50'     ''     ''     ''     ''   ''      ''    ''   '',
        'TEXTO02'           'Via Acesso'               '08'     ''     ''     ''     ''   ''      ''    ''   ''.
ENDFORM.

FORM preenche_cat01 USING VALUE(p_campo)
                          VALUE(p_desc)
                          VALUE(p_tam)
                          VALUE(p_zero)
                          VALUE(p_hot)
                          VALUE(p_sum)
                          VALUE(p_just)
                          VALUE(p_edit)
                          VALUE(p_table)
                          VALUE(p_fieldname)
                          VALUE(p_f4).

  wa_fieldcatalog01-fieldname   = p_campo.
  wa_fieldcatalog01-coltext     = p_desc.
  wa_fieldcatalog01-scrtext_l   = p_desc.
  wa_fieldcatalog01-scrtext_m   = p_desc.
  wa_fieldcatalog01-scrtext_s   = p_desc.
  wa_fieldcatalog01-outputlen   = p_tam.
  wa_fieldcatalog01-hotspot     = p_hot.
  wa_fieldcatalog01-no_zero     = p_zero.
  wa_fieldcatalog01-do_sum      = p_sum.
  wa_fieldcatalog01-just        = p_just.
  wa_fieldcatalog01-edit        = p_edit.
  wa_fieldcatalog01-ref_table   = p_table.
  wa_fieldcatalog01-ref_field   = p_fieldname.
  wa_fieldcatalog01-f4availabl  = p_f4.


  IF p_campo EQ 'TEXTO01' OR  p_campo EQ 'TEXTO02'.
    wa_fieldcatalog01-style = cl_gui_alv_grid=>mc_style_button.
  ELSE.
    wa_fieldcatalog01-style = ''.
  ENDIF.
  APPEND wa_fieldcatalog01 TO it_fieldcatalog01.

ENDFORM.



FORM z_cria_alv02.

  REFRESH it_fieldcatalog02.

  PERFORM preenche_cat02 USING:
        'MATNR'              'Material'               '10'     'X'    ''     ''     ''   'X'     'MARA' 'MATNR' 'X',
        'MAKTX'              'Descrição'              '40'     ''     ''     ''     ''   ''      ''     ''   '',
        'ID_PRODUTO'         'Id Cód.Indea'           '10'     ''     ''     ''     ''   'X'     ''     ''   'X',
        'CODMAPA'            'Cód.Mapa Indea'         '15'     ''     ''     ''     ''   ''      ''     ''   'X', "*-CS2021000218-08.09.2022-#89493-JT-inicio
        'DESCRICAO'          'Descrição'              '25'     ''     ''     ''     ''   ''      ''     ''   '',
        'FABRICANTE'         'Fabricante'             '20'     ''     ''     ''     ''   ''      ''     ''   '',
        'EMBALAGEM'          'Embalagem'              '20'     ''     ''     ''     ''   ''      ''     ''   '',
        'TIPOEMBALAGEM'      'Tipo Embalagem'         '20'     ''     ''     ''     ''   ''      ''     ''   '',
        'VOLUME'             'Volume'                 '06'     ''     ''     ''     ''   ''      ''     ''   '',
        'UNIDADE'            'Unidade'                '06'     ''     ''     ''     ''   ''      ''     ''   '',
        'NOMECIENTIFICO'     'Nome Cientifico'        '20'     ''     ''     ''     ''   ''      ''     ''   '',
        'NOMECOMUM'          'Nome Comum'             '20'     ''     ''     ''     ''   ''      ''     ''   '',
        'PRODUTOID'          'Id.Cód.AgriQ'           '20'     ''     ''     ''     ''   'X'     ''     ''   'X', "*-CS2021000218-08.09.2022-#89493-JT-inicio
*       'NUMERODERISCO'      'Cód.Map.AgriQ'          '15'     ''     ''     ''     ''   ''      ''     ''   '',  "*-CS2021000218-08.09.2022-#89493-JT-inicio
        'REGISTROMINISTERIO' 'Cód.Map.AgriQ'          '15'     ''     ''     ''     ''   ''      ''     ''   '',  "*-CS2021000218-08.09.2022-#89493-JT-inicio
        'DESCRICAO2'         'Descrição AgriQ'        '35'     ''     ''     ''     ''   ''      ''     ''   ''.  "*-CS2021000218-08.09.2022-#89493-JT-inicio

ENDFORM.

FORM preenche_cat02 USING VALUE(p_campo)
                          VALUE(p_desc)
                          VALUE(p_tam)
                          VALUE(p_zero)
                          VALUE(p_hot)
                          VALUE(p_sum)
                          VALUE(p_just)
                          VALUE(p_edit)
                          VALUE(p_table)
                          VALUE(p_fieldname)
                          VALUE(p_f4).

  wa_fieldcatalog02-fieldname   = p_campo.
  wa_fieldcatalog02-coltext     = p_desc.
  wa_fieldcatalog02-scrtext_l   = p_desc.
  wa_fieldcatalog02-scrtext_m   = p_desc.
  wa_fieldcatalog02-scrtext_s   = p_desc.
  wa_fieldcatalog02-outputlen   = p_tam.
  wa_fieldcatalog02-hotspot     = p_hot.
  wa_fieldcatalog02-no_zero     = p_zero.
  wa_fieldcatalog02-do_sum      = p_sum.
  wa_fieldcatalog02-just        = p_just.
  wa_fieldcatalog02-edit        = p_edit.
  wa_fieldcatalog02-ref_table   = p_table.
  wa_fieldcatalog02-ref_field   = p_fieldname.
  wa_fieldcatalog02-f4availabl  = p_f4.

  APPEND wa_fieldcatalog02 TO it_fieldcatalog02.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados_material.

*** Inicio - Rubenilson - 03.04.2025 #172934
  TYPES: BEGIN OF ty_0210,
           matnr          TYPE zsdt0210-matnr,
           id_matnr_idea  TYPE zsdt0198-id_cultivar,
           id_matnr_agriq TYPE zsdt0210-id_matnr_agriq,
         END OF ty_0210.

  DATA: lt_0210 TYPE TABLE OF ty_0210.
*** Fim - Rubenilson - 03.04.2025 #172934

  SELECT *
    FROM zsdt0210 INTO TABLE it_zsdt0210.

  CHECK it_zsdt0210 IS NOT INITIAL.

*** Inicio - Rubenilson - 03.04.2025 #172934
  LOOP AT it_zsdt0210 ASSIGNING FIELD-SYMBOL(<fs_0210>).

    APPEND INITIAL LINE TO lt_0210 ASSIGNING FIELD-SYMBOL(<fs_lt210>).
    MOVE-CORRESPONDING <fs_0210> TO <fs_lt210>.

    <fs_lt210>-matnr          = <fs_0210>-matnr.
    <fs_lt210>-id_matnr_idea  = <fs_0210>-id_matnr_idea.
    <fs_lt210>-id_matnr_agriq = <fs_0210>-id_matnr_agriq.
  ENDLOOP.
*** Fim - Rubenilson - 03.04.2025 #172934

  SELECT *
    FROM makt INTO TABLE @DATA(it_makt)
   FOR ALL ENTRIES IN @it_zsdt0210
 WHERE matnr EQ @it_zsdt0210-matnr.

  SELECT *
    FROM mara INTO TABLE @DATA(it_mara)
  FOR ALL ENTRIES IN @it_zsdt0210
 WHERE matnr EQ @it_zsdt0210-matnr.


  SELECT *
    FROM  zsdt0198 INTO TABLE it_zsdt0198
*    FOR ALL ENTRIES IN it_zsdt0210          " Rubenilson - 03.04.2025 #172934
    FOR ALL ENTRIES IN lt_0210               " Rubenilson - 03.04.2025 #172934
  WHERE id_cultivar EQ lt_0210-id_matnr_idea." Rubenilson - 03.04.2025 #172934


  SELECT *
    FROM   zsdt0201 INTO TABLE it_zsdt0201
    FOR ALL ENTRIES IN it_zsdt0210
  WHERE id_produto EQ it_zsdt0210-id_matnr_idea.

*-CS2021000218-08.09.2022-#89493-JT-inicio
  SELECT *
    FROM   zsdt0299 INTO TABLE it_zsdt0299
    FOR ALL ENTRIES IN it_zsdt0210
  WHERE produtoid   EQ it_zsdt0210-id_matnr_agriq.
*-CS2021000218-08.09.2022-#89493-JT-fim

  FREE: it_saida_02.

  LOOP AT it_zsdt0210 INTO wa_zsdt0210.

    wa_saida_02-matnr           = wa_zsdt0210-matnr.
    wa_saida_02-id_produto      = wa_zsdt0210-id_matnr_idea.
    wa_saida_02-produtoid       = wa_zsdt0210-id_matnr_agriq.  "*-CS2021000218-08.09.2022-#89493-JT-inicio

    READ TABLE it_makt INTO DATA(wa_makt) WITH KEY matnr =  wa_zsdt0210-matnr.
    wa_saida_02-maktx    = wa_makt-maktx.

    READ TABLE it_mara INTO DATA(wa_mara) WITH KEY matnr = wa_zsdt0210-matnr.
    IF wa_mara-spart = '03' OR
       wa_mara-spart = '01'.   "*-CS2021000218-08.09.2022-#89493-JT-inicio

      READ TABLE it_zsdt0201 INTO wa_zsdt0201 WITH KEY  id_produto = wa_zsdt0210-id_matnr_idea.
      IF sy-subrc = 0.
        wa_saida_02-codmapa            =  wa_zsdt0201-codmapa.         "*-CS2021000218-08.09.2022-#89493-JT-inicio
        wa_saida_02-descricao          =  wa_zsdt0201-nome.
        wa_saida_02-fabricante         =  wa_zsdt0201-fabricante.
        wa_saida_02-embalagem          =  wa_zsdt0201-embalagem.
        wa_saida_02-tipoembalagem      =  wa_zsdt0201-tipoembalagem.
        wa_saida_02-volume             =  wa_zsdt0201-volume.
        wa_saida_02-unidade            =  wa_zsdt0201-unidade.
      ENDIF.

    ELSEIF wa_mara-spart = '04'.

      READ TABLE it_zsdt0198 INTO wa_zsdt0198 WITH KEY id_cultivar = wa_zsdt0210-id_matnr_idea.
      IF sy-subrc = 0.

        wa_saida_02-descricao    = wa_zsdt0198-nome.
        wa_saida_02-id_especie   = wa_zsdt0198-id_especie.

        SELECT SINGLE *
          FROM  zsdt0197 INTO @DATA(wa_zsdt0197)
        WHERE id_especie EQ @wa_zsdt0198-id_especie.

        IF sy-subrc = 0.
          wa_saida_02-nomecientifico  = wa_zsdt0197-nomecientifico.
          wa_saida_02-nomecomum       = wa_zsdt0197-nomecomum.
        ENDIF.
      ENDIF.
    ENDIF.

*-CS2021000218-08.09.2022-#89493-JT-inicio
    READ TABLE it_zsdt0299 INTO wa_zsdt0299 WITH KEY produtoid = wa_zsdt0210-id_matnr_agriq.
    IF sy-subrc = 0.
      wa_saida_02-numeroderisco      =  wa_zsdt0299-numeroderisco.
      wa_saida_02-registroministerio =  wa_zsdt0299-registroministerio.
      wa_saida_02-descricao2         =  wa_zsdt0299-descricao.
    ENDIF.
    READ TABLE it_zsdt0201 INTO DATA(w_0201) WITH KEY id_produto = wa_zsdt0210-id_matnr_idea.
    IF sy-subrc = 0.
      wa_saida_02-codmapa            =  w_0201-codmapa.         "*-CS2021000218-08.09.2022-#89493-JT-inicio
    ENDIF.
*-CS2021000218-08.09.2022-#89493-JT-fim

    FREE wa_saida_02-celltab.
    gt_estilo02 =  VALUE #( ( fieldname = 'MATNR'      style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'ID_PRODUTO' style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'PRODUTOID'  style = cl_gui_alv_grid=>mc_style_disabled  )  ). "*-CS2021000218-08.09.2022-#89493-JT-inicio
    INSERT LINES OF gt_estilo02 INTO TABLE wa_saida_02-celltab.

*-CS2021000218-08.09.2022-#89493-JT-inicio
    FREE wa_saida_02-cellcolor.
    gt_color02  =  VALUE #( ( fname     = 'ID_PRODUTO'          color-col = 3 )
                            ( fname     = 'CODMAPA'             color-col = 3 )
                            ( fname     = 'DESCRICAO'           color-col = 3 )
                            ( fname     = 'FABRICANTE'          color-col = 3 )
                            ( fname     = 'EMBALAGEM'           color-col = 3 )
                            ( fname     = 'TIPOEMBALAGEM'       color-col = 3 )
                            ( fname     = 'VOLUME'              color-col = 3 )
                            ( fname     = 'UNIDADE'             color-col = 3 )
                            ( fname     = 'NOMECIENTIFICO'      color-col = 3 )
                            ( fname     = 'NOMECOMUM'           color-col = 3 )
                            ( fname     = 'PRODUTOID'           color-col = 5 )
*                           ( fname     = 'NUMERODERISCO'  color-col = 5 )
                            ( fname     = 'REGISTROMINISTERIO'  color-col = 5 )
                            ( fname     = 'DESCRICAO2'          color-col = 5 ) ).
    wa_saida_02-cellcolor[] = gt_color02[].
*-CS2021000218-08.09.2022-#89493-JT-fim

    wa_saida_02-check = 'X'.

    APPEND wa_saida_02 TO it_saida_02.
    CLEAR: wa_saida_02, wa_zsdt0201, wa_zsdt0197, wa_zsdt0198, wa_zsdt0210, wa_makt.
  ENDLOOP.

ENDFORM.

FORM z_cria_alv04.

  REFRESH it_fieldcatalog04.
  PERFORM preenche_cat04 USING:
        'TIPO'                'Tipo'                 '04'     ''     ''     ''     ''   'X'     'ZSDT0216' 'TIPO'              '',
        'TEXT'                'Descr.'               '10'     ''     ''     ''     ''   ''      ''         ''                  '',
        'SETOR_ATIVIDADE'     'Setor'                '06'     ''     ''     ''     ''   'X'     'ZSDT0216' 'SETOR_ATIVIDADE'   '',
        'TEXT01'              'Descr.'               '11'     ''     ''     ''     ''   ' '     ''         ''                  '',
        'REVENDA'             'Revenda'              '07'     ''     ''     ''     ''   'X'     'ZSDT0216' 'REVENDA'           '',
        'TEXT02'              'Descr.'               '08'     ''     ''     ''     ''   ''      ''         ''                  '',
        'BUKRS'               'Empresa'              '07'     ''     ''     ''     ''   'X'     'T001'     'BUKRS'             '',
        'BUTXT'               'Desc.Empresa'         '25'     ''     ''     ''     ''   ' '     ''         ''                  '',
        'KUNNR'               'Código'               '12'     'X'    ''     ''     ''   'X'     ''         ''                  'X',
        'NAME1'               'Descrição'            '25'     ''     ''     ''     ''   ' '     ''         ''                  '',
        'STCD1'               'CNPJ'                 '14'     ''     ''     ''     ''   ' '     ''         ''                  '',
        'ORT01'               'Cidade'               '20'     ''     ''     ''     ''   ' '     ''         ''                  '',
        'STRAS'               'Endereço'             '25'     ''     ''     ''     ''   ' '     ''         ''                  '',
        'REGIO'               'Estado'               '07'     ''     ''     ''     ''   ' '     ''         ''                  '',
        'RENASEM'             'Renasem'              '20'     ''     ''     ''     ''   'X'     ''         ''                  ''.

ENDFORM.


FORM preenche_cat04 USING VALUE(p_campo)
                          VALUE(p_desc)
                          VALUE(p_tam)
                          VALUE(p_zero)
                          VALUE(p_hot)
                          VALUE(p_sum)
                          VALUE(p_just)
                          VALUE(p_edit)
                          VALUE(p_table)
                          VALUE(p_fieldname)
                          VALUE(p_f4).

  wa_fieldcatalog04-fieldname   = p_campo.
  wa_fieldcatalog04-coltext     = p_desc.
  wa_fieldcatalog04-scrtext_l   = p_desc.
  wa_fieldcatalog04-scrtext_m   = p_desc.
  wa_fieldcatalog04-scrtext_s   = p_desc.
  wa_fieldcatalog04-outputlen   = p_tam.
  wa_fieldcatalog04-hotspot     = p_hot.
  wa_fieldcatalog04-no_zero     = p_zero.
  wa_fieldcatalog04-do_sum      = p_sum.
  wa_fieldcatalog04-just        = p_just.
  wa_fieldcatalog04-edit        = p_edit.
  wa_fieldcatalog04-ref_table   = p_table.
  wa_fieldcatalog04-ref_field   = p_fieldname.
  wa_fieldcatalog04-f4availabl  = p_f4.

  APPEND wa_fieldcatalog04 TO it_fieldcatalog04.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0104 OUTPUT.

  DATA: lt_f404 TYPE lvc_t_f4,
        wl_f404 TYPE lvc_s_f4.

  SET PF-STATUS 'ST_0104'.
  SET TITLEBAR 'TL_0104'.

  PERFORM z_cria_alv04.

  IF g_custom_container04 IS INITIAL.

    CREATE OBJECT g_custom_container04
      EXPORTING
        container_name              = 'CONTAINER04'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid04 IS INITIAL AND g_custom_container04 IS NOT  INITIAL.
      CREATE OBJECT g_grid04
        EXPORTING
          i_parent          = g_custom_container04
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    wl_function04  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function04 TO tl_function04.
    wl_function04  = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function04 TO tl_function04.

    gs_layout04-stylefname = 'CELLTAB'.

    wl_f404-fieldname  = 'KUNNR'.
    wl_f404-register   = 'X'.
    wl_f404-getbefore  = 'X'.
    APPEND wl_f404 TO  lt_f404.



    CALL METHOD g_grid04->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant04
        is_layout            = gs_layout04
        it_toolbar_excluding = tl_function04
      CHANGING
        it_outtab            = it_saida_04
        it_fieldcatalog      = it_fieldcatalog04.

    SET HANDLER lcl_event_f404=>on_f4_04 FOR g_grid04.


    SET HANDLER: lcl_event_handler04=>on_data_changed04 FOR g_grid04,
                 lcl_event_handler04=>on_data_changed_finished04 FOR g_grid04.


    CALL METHOD g_grid04->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid04->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid04->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.


    CALL METHOD g_grid04->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f404[].


  ELSE.

    CALL METHOD g_grid04->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcatalog04.


    CALL METHOD g_grid04->refresh_table_display
      EXPORTING
        is_stable = wa_stable04.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0104 INPUT.
  DATA wa_salvar_04 TYPE zsdt0216.
  DATA it_salvar_04 TYPE TABLE OF zsdt0216.
  DATA erro TYPE c.


  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SALVAR'.

      REFRESH it_salvar_04.
      CLEAR wa_salvar_04.

      LOOP AT it_saida_04 INTO wa_saida_04.

        SELECT SINGLE * FROM zsdt0216 INTO @DATA(wzt216)
          WHERE bukrs EQ @wa_saida_04-bukrs
          AND   kunnr EQ @wa_saida_04-kunnr
          AND   tipo  EQ @wa_saida_04-tipo
          AND   setor_atividade EQ @wa_saida_04-setor_atividade.

        IF sy-subrc <> 0 .
          erro =  abap_false.

          IF wa_saida_04-tipo IS INITIAL OR  ( wa_saida_04-tipo <> 'C' AND wa_saida_04-tipo <> 'F' )  .
            erro =  abap_true.
            MESSAGE |Favor informar se é Tipo "C" Cliente ou "F" Fornecedor.  Linha: { sy-tabix }|  TYPE 'I'.
            EXIT.
          ELSE.
            wa_salvar_04-tipo       =  wa_saida_04-tipo.
          ENDIF.

*"// US-168932 16/05/2025 WBARBOSA
          CASE wa_salvar_04-tipo.
            WHEN 'C' OR 'F'.
              IF wa_saida_04-setor_atividade IS INITIAL OR ( wa_saida_04-setor_atividade <> 'A' AND wa_saida_04-setor_atividade <> 'S' ).
                erro =  abap_true.
                MESSAGE |Favor informar Setor Atividade. "S" Sementes ou "A" Agrotóxico.  Linha: { sy-tabix }| TYPE 'I'.
                EXIT.
              ELSE.
                wa_salvar_04-setor_atividade =  wa_saida_04-setor_atividade.
              ENDIF.

              IF wa_saida_04-revenda IS INITIAL OR ( wa_saida_04-revenda <> 'N' AND wa_saida_04-revenda <> 'S' ).
                erro =  abap_true.
                MESSAGE |'Favor informar Revenda. "S" Sim ou "N" Não.  Linha: { sy-tabix }| TYPE 'I'.
                EXIT.
              ELSE.
                wa_salvar_04-revenda =  wa_saida_04-revenda.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.
*"// US-168932 16/05/2025 WBARBOSA

*** Inicio - Rubenilson Pereira - 11.04.25 #BUG173919
*          IF wa_salvar_04-tipo EQ 'F'.
*            wa_salvar_04-setor_atividade =  'S'.
*            wa_salvar_04-revenda =  ' '.
*
*            wa_saida_04-setor_atividade = 'S'.
*            wa_saida_04-text01 = 'Sementes'.
*            wa_saida_04-revenda = ' '.
*
*            MODIFY it_saida_04 FROM wa_saida_04.
*          ENDIF.
*** Fim - Rubenilson Pereira - 11.04.25 #BUG173919

          IF wa_saida_04-bukrs IS INITIAL AND wa_saida_04-tipo EQ 'C'.
            erro =  abap_true.
            MESSAGE |Favor infomar empresa.  Linha: { sy-tabix }| TYPE 'I'.
            EXIT.
          ELSE.
            SELECT SINGLE * FROM t001 INTO @DATA(wat001)
              WHERE bukrs EQ @wa_saida_04-bukrs.

            IF sy-subrc = 0.
              wa_salvar_04-mandt       =  sy-mandt.
              wa_salvar_04-bukrs       =  wa_saida_04-bukrs.
            ELSE.
              erro =  abap_true.
              MESSAGE |Empresa não existe.  Linha: { sy-tabix }| TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.

          IF wa_saida_04-kunnr IS INITIAL AND wa_saida_04-tipo EQ 'C'.
            erro =  abap_true.
            MESSAGE |Favor informar o Cód.Cliente. Linha: { sy-tabix }| TYPE 'I'.
            EXIT.

          ELSEIF wa_saida_04-kunnr IS INITIAL AND wa_saida_04-tipo EQ 'F'.
            erro =  abap_true.
            MESSAGE |Favor informar o Cód.Fornecedor.  Linha: { sy-tabix }| TYPE 'I'.
            EXIT.

          ELSEIF wa_saida_04-kunnr IS NOT INITIAL AND wa_saida_04-tipo EQ 'C'.
            SELECT SINGLE * FROM kna1 INTO @DATA(wkna1)
              WHERE kunnr EQ @wa_saida_04-kunnr.

            IF sy-subrc = 0.
              wa_salvar_04-kunnr       =  wa_saida_04-kunnr.
              wa_salvar_04-data_atual  =  sy-datum.
              wa_salvar_04-usnam       =  sy-uname.
              wa_salvar_04-hora_atual  =  sy-uzeit.
            ELSE.
              erro =  abap_true.
              MESSAGE |Cliente não existe. Linha: { sy-tabix }|  TYPE 'I'.
              EXIT.
            ENDIF.

          ELSEIF wa_saida_04-kunnr IS NOT INITIAL AND wa_saida_04-tipo EQ 'F'.
            SELECT SINGLE * FROM lfa1 INTO wlfa1
              WHERE lifnr EQ wa_saida_04-kunnr.

            IF sy-subrc = 0.
              wa_salvar_04-kunnr       =  wa_saida_04-kunnr.
              wa_salvar_04-data_atual  =  sy-datum.
              wa_salvar_04-usnam       =  sy-uname.
              wa_salvar_04-hora_atual  =  sy-uzeit.
            ELSE.
              erro =  abap_true.
              MESSAGE |Fornecedor não existe. Linha: { sy-tabix }| TYPE 'I'.
              EXIT.
            ENDIF.
          ENDIF.

          IF  wa_saida_04-tipo EQ 'C'.
            IF  wa_saida_04-revenda EQ 'S' AND wa_saida_04-setor_atividade EQ 'S'.
              IF wa_saida_04-renasem IS INITIAL.
                erro =  abap_true.
                MESSAGE |Favor informar o Renasem! Linha: { sy-tabix }| TYPE 'I'.
                EXIT.
              ELSE.
                wa_salvar_04-renasem       =  wa_saida_04-renasem.
              ENDIF.
            ELSE.
              wa_salvar_04-renasem       =  wa_saida_04-renasem.
            ENDIF.

*** Inicio - Rubenilson Pereira - 11.04.25 #BUG173919
          ELSEIF wa_saida_04-tipo EQ 'F'.
            IF wa_saida_04-renasem IS INITIAL AND wa_saida_04-setor_atividade NE 'A'.

              erro =  abap_true.
              MESSAGE |Favor informar o Renasem! Linha: { sy-tabix }| TYPE 'I'.
              EXIT.
            ELSE.
              wa_salvar_04-renasem       =  wa_saida_04-renasem.
            ENDIF.
*** Fim - Rubenilson Pereira - 11.04.25 #BUG173919
          ELSE.
            IF wa_saida_04-renasem IS INITIAL.
              erro =  abap_true.
              MESSAGE |Favor informar o Renasem! Linha: { sy-tabix }| TYPE 'I'.
              EXIT.
            ELSE.
              wa_salvar_04-renasem       =  wa_saida_04-renasem.
            ENDIF.
          ENDIF.

          CHECK erro =  abap_false.
          APPEND  wa_salvar_04 TO it_salvar_04.
        ENDIF.

        CLEAR: wa_saida_04, wa_salvar_04.
      ENDLOOP.

      CHECK erro =  abap_false.
      IF  it_salvar_04 IS NOT INITIAL.

        MODIFY zsdt0216 FROM TABLE it_salvar_04.
        MESSAGE 'Dados gravado com Sucesso!' TYPE 'S'.

        LOOP AT it_saida_04 INTO wa_saida_04.
          FREE wa_saida_04-celltab.
          gt_estilo04 =  VALUE #( ( fieldname = 'TIPO'             style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'BUKRS'            style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'REVENDA'          style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'SETOR_ATIVIDADE'  style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'KUNNR'            style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'RENASEM'          style = cl_gui_alv_grid=>mc_style_disabled  ) ).
          INSERT LINES OF gt_estilo04 INTO TABLE wa_saida_04-celltab.
          MODIFY it_saida_04 FROM wa_saida_04.
        ENDLOOP.
      ENDIF.

    WHEN '&INS'.
      APPEND INITIAL LINE TO it_saida_04.
    WHEN '&DEL'.

      CALL METHOD g_grid04->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF tg_selectedrow IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
        EXIT.
      ELSE.
        LOOP AT tg_selectedrow INTO wg_selectedrow.

          READ TABLE it_saida_04 INTO wa_saida_04 INDEX wg_selectedrow-index.

          DELETE FROM zsdt0216  WHERE bukrs           = wa_saida_04-bukrs   AND
                                      kunnr           = wa_saida_04-kunnr   AND
                                      setor_atividade = wa_saida_04-setor_atividade AND
                                      revenda         = wa_saida_04-revenda  AND
                                      tipo            = wa_saida_04-tipo    AND
                                      renasem         = wa_saida_04-renasem.                                     .

          CLEAR wa_saida_04.
        ENDLOOP.

        REFRESH it_saida_04.
        PERFORM seleciona_dados_cliente.
      ENDIF.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_CLIENTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados_cliente .

  SELECT *
    FROM zsdt0216 INTO TABLE it_zsdt0216.

  CHECK it_zsdt0216 IS NOT INITIAL.

  SELECT *
    FROM t001 INTO TABLE @DATA(it_t001)
    FOR ALL ENTRIES IN @it_zsdt0216
   WHERE bukrs EQ @it_zsdt0216-bukrs.

  SELECT *
    FROM kna1 INTO TABLE @DATA(it_kna1)
    FOR ALL ENTRIES IN @it_zsdt0216
   WHERE kunnr EQ @it_zsdt0216-kunnr.

  SELECT *
    FROM lfa1 INTO TABLE @DATA(it_lfa1)
    FOR ALL ENTRIES IN @it_zsdt0216
   WHERE lifnr EQ @it_zsdt0216-kunnr.


  LOOP AT it_zsdt0216 INTO wa_zsdt0216.

    wa_saida_04-bukrs    = wa_zsdt0216-bukrs.
    wa_saida_04-kunnr    = wa_zsdt0216-kunnr.
    wa_saida_04-renasem  = wa_zsdt0216-renasem.
    wa_saida_04-tipo     = wa_zsdt0216-tipo.


    READ TABLE it_t001 INTO DATA(wa_t001) WITH KEY bukrs =  wa_zsdt0216-bukrs.
    IF sy-subrc = 0.
      wa_saida_04-butxt  =  wa_t001-butxt.
    ENDIF.

    CASE wa_zsdt0216-tipo.
      WHEN 'C'.
        wa_saida_04-text = 'Cliente'.

        READ TABLE it_kna1 INTO DATA(wa_kna1) WITH KEY kunnr = wa_zsdt0216-kunnr.
        IF sy-subrc = 0.
          wa_saida_04-name1  = wa_kna1-name1.
          wa_saida_04-ort01  = wa_kna1-ort01.
          wa_saida_04-regio  = wa_kna1-regio.

          IF wa_kna1-stcd1 IS NOT INITIAL.
            wa_saida_04-stcd1 = wa_kna1-stcd1.
          ELSE.
            wa_saida_04-stcd1 = wa_kna1-stcd2.
          ENDIF.

          wa_saida_04-stras = wa_kna1-stras.
        ENDIF.
      WHEN 'F'.
        wa_saida_04-text = 'Fornecedor'.

        READ TABLE it_lfa1 INTO DATA(wa_lfa1) WITH KEY lifnr = wa_zsdt0216-kunnr.
        IF sy-subrc = 0.
          wa_saida_04-name1  = wa_lfa1-name1.
          wa_saida_04-ort01  = wa_lfa1-ort01.
          wa_saida_04-regio  = wa_lfa1-regio.

          IF wa_lfa1-stcd1 IS NOT INITIAL.
            wa_saida_04-stcd1 = wa_lfa1-stcd1.
          ELSE.
            wa_saida_04-stcd1 = wa_lfa1-stcd2.
          ENDIF.
          wa_saida_04-stras = wa_lfa1-stras.
        ENDIF.
    ENDCASE.


    wa_saida_04-setor_atividade =  wa_zsdt0216-setor_atividade.
    CASE wa_zsdt0216-setor_atividade.
      WHEN 'A'.
        wa_saida_04-text01 = 'Agrotóxico'.
      WHEN 'S'.
        wa_saida_04-text01 = 'Sementes'.
    ENDCASE.

    wa_saida_04-revenda = wa_zsdt0216-revenda.
    CASE wa_zsdt0216-revenda.
      WHEN 'S'.
        wa_saida_04-text02 = 'Sim'.
      WHEN 'N'.
        wa_saida_04-text02 = 'Não'.
    ENDCASE.


    FREE wa_saida_04-celltab.
    gt_estilo04 =  VALUE #( ( fieldname = 'TIPO'             style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'BUKRS'            style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'KUNNR'            style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'SETOR_ATIVIDADE'  style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'REVENDA'          style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'RENASEM'          style = cl_gui_alv_grid=>mc_style_disabled  ) ).
    INSERT LINES OF gt_estilo04 INTO TABLE wa_saida_04-celltab.

    APPEND wa_saida_04 TO it_saida_04.

    CLEAR: wa_saida_04, wa_kna1, wa_lfa1, wa_t001.
  ENDLOOP.

ENDFORM.


FORM z_cria_alv05.

  REFRESH it_fieldcatalog05.
  PERFORM preenche_cat05 USING:
        'BUSCA_INDEA'      'Busca Indea'           '09'     ''     ''     ''     ''   'X'   ''   ''    ''    'X',
        'NR_CAD'           'Nº Cadastro'           '10'     ''     ''     ''     ''   ''   ''   ''    ''    '',
        'NR_REG'           'Nº Registro'           '10'     ''     ''     ''     ''   ''   ''   ''    ''    '',
        'MARCA'            'Marca Comercial'       '45'     ''     ''     ''     ''   ''   ''   ''    ''    '',
        'ING_ATIVO'        'Ingrediente Ativo'     '45'     ''     ''     ''     ''   ''   ''   ''    ''    '',
        'CLASSE'           'Classe Toxicológica'   '20'     ''     ''     ''     ''   ''   ''   ''    ''    '',
        'REGISTRANTE'      'Registrante'           '45'     ''     ''     ''     ''   ''   ''   ''    ''    '',
        'VALIDADE'         'Validade'              '10'     ''     ''     ''     ''   ''   ''   ''    ''    ''.
ENDFORM.


FORM preenche_cat05 USING VALUE(p_campo)
                          VALUE(p_desc)
                          VALUE(p_tam)
                          VALUE(p_zero)
                          VALUE(p_hot)
                          VALUE(p_sum)
                          VALUE(p_just)
                          VALUE(p_edit)
                          VALUE(p_table)
                          VALUE(p_fieldname)
                          VALUE(p_f4)
                          VALUE(p_check).

  wa_fieldcatalog05-fieldname   = p_campo.
  wa_fieldcatalog05-coltext     = p_desc.
  wa_fieldcatalog05-scrtext_l   = p_desc.
  wa_fieldcatalog05-scrtext_m   = p_desc.
  wa_fieldcatalog05-scrtext_s   = p_desc.
  wa_fieldcatalog05-outputlen   = p_tam.
  wa_fieldcatalog05-hotspot     = p_hot.
  wa_fieldcatalog05-no_zero     = p_zero.
  wa_fieldcatalog05-do_sum      = p_sum.
  wa_fieldcatalog05-just        = p_just.
  wa_fieldcatalog05-edit        = p_edit.
  wa_fieldcatalog05-ref_table   = p_table.
  wa_fieldcatalog05-ref_field   = p_fieldname.
  wa_fieldcatalog05-f4availabl  = p_f4.
  wa_fieldcatalog05-checkbox    = p_check.

  APPEND wa_fieldcatalog05 TO it_fieldcatalog05.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Module  STATUS_0105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0105 OUTPUT.
  SET PF-STATUS 'ST_0105'.
  SET TITLEBAR 'TL_0105'.

  PERFORM z_cria_alv05.

  IF g_custom_container05 IS INITIAL.

    CREATE OBJECT g_custom_container05
      EXPORTING
        container_name              = 'CONTAINER05'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF g_grid05 IS INITIAL AND g_custom_container05 IS NOT  INITIAL.

      CREATE OBJECT g_grid05
        EXPORTING
          i_parent          = g_custom_container05
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
    ENDIF.

    wl_function05  = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function05 TO tl_function05.
    wl_function05  = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function05 TO tl_function05.


    gs_layout05-info_fname = 'COLOR'.
    gs_layout05-stylefname = 'CELLTAB'.

    CALL METHOD g_grid05->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant05
        is_layout            = gs_layout05
        it_toolbar_excluding = tl_function05
      CHANGING
        it_outtab            = it_saida_05
        it_fieldcatalog      = it_fieldcatalog05.


    SET HANDLER: lcl_event_handler05=>on_data_changed05 FOR g_grid05,
                 lcl_event_handler05=>on_data_changed_finished05 FOR g_grid05.


    CALL METHOD g_grid05->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid05->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid05->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD g_grid05->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcatalog05.


    CALL METHOD g_grid05->refresh_table_display
      EXPORTING
        is_stable = wa_stable05.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0105 INPUT.

  DATA: it_salvar05 TYPE TABLE OF zsdt0217,
        wa_salvar05 TYPE zsdt0217.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SALVAR'.
      REFRESH it_salvar05.
      CLEAR wa_salvar05.

      LOOP AT it_saida_05 INTO wa_saida_05.

        wa_salvar05-mandt        =  sy-mandt.
        wa_salvar05-busca_indea  =  wa_saida_05-busca_indea.
        wa_salvar05-nr_cad       =  wa_saida_05-nr_cad.
        wa_salvar05-nr_reg       =  wa_saida_05-nr_reg.
        wa_salvar05-marca        =  wa_saida_05-marca.
        wa_salvar05-ing_ativo    =  wa_saida_05-ing_ativo.
        wa_salvar05-classe       =  wa_saida_05-classe.
        wa_salvar05-registrante  =  wa_saida_05-registrante.
        wa_salvar05-validade     =  wa_saida_05-validade.
        wa_salvar05-usnam        =  sy-uname.
        wa_salvar05-data_atual   =  sy-datum.
        wa_salvar05-hora_atual   =  sy-uzeit.

        APPEND wa_salvar05 TO it_salvar05.
        CLEAR : wa_saida_05, wa_salvar05.
      ENDLOOP.

      IF  it_salvar05 IS NOT INITIAL.

        MODIFY zsdt0217 FROM TABLE it_salvar05.
        MESSAGE 'Dados gravado com Sucesso!' TYPE 'S'.

        LOOP AT it_saida_05 INTO wa_saida_05.
          FREE wa_saida_05-celltab.
          gt_estilo05 =  VALUE #( ( fieldname = 'NR_CAD'      style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'NR_REG'      style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'MARCA'       style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'ING_ATIVO'   style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'CLASSE'      style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'REGISTRANTE' style = cl_gui_alv_grid=>mc_style_disabled  )
                                  ( fieldname = 'VALIDADE'    style = cl_gui_alv_grid=>mc_style_disabled  ) ).
          INSERT LINES OF gt_estilo05 INTO TABLE wa_saida_05-celltab.
          MODIFY it_saida_05 FROM wa_saida_05.
        ENDLOOP.
      ENDIF.

    WHEN '&INS'.

      FREE wa_saida_05-celltab.
      gt_estilo05 =  VALUE #( ( fieldname = 'NR_CAD'      style = cl_gui_alv_grid=>mc_style_enabled  )
                              ( fieldname = 'NR_REG'      style = cl_gui_alv_grid=>mc_style_enabled  )
                              ( fieldname = 'MARCA'       style = cl_gui_alv_grid=>mc_style_enabled  )
                              ( fieldname = 'ING_ATIVO'   style = cl_gui_alv_grid=>mc_style_enabled  )
                              ( fieldname = 'CLASSE'      style = cl_gui_alv_grid=>mc_style_enabled  )
                              ( fieldname = 'REGISTRANTE' style = cl_gui_alv_grid=>mc_style_enabled  )
                              ( fieldname = 'VALIDADE'    style = cl_gui_alv_grid=>mc_style_enabled  ) ).
      INSERT LINES OF gt_estilo05 INTO TABLE wa_saida_05-celltab.
      APPEND wa_saida_05 TO  it_saida_05.
      SORT it_saida_05 BY nr_cad nr_reg DESCENDING.
    WHEN '&EDIT'.

      CALL METHOD g_grid05->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF tg_selectedrow IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'I'.
      ELSE.

        LOOP AT tg_selectedrow INTO wg_selectedrow.

          READ TABLE it_saida_05 INTO wa_saida_05 INDEX wg_selectedrow-index.

          FREE wa_saida_05-celltab.
          gt_estilo05 =  VALUE #( ( fieldname = 'MARCA'       style = cl_gui_alv_grid=>mc_style_enabled  )
                                  ( fieldname = 'ING_ATIVO'   style = cl_gui_alv_grid=>mc_style_enabled  )
                                  ( fieldname = 'CLASSE'      style = cl_gui_alv_grid=>mc_style_enabled  )
                                  ( fieldname = 'REGISTRANTE' style = cl_gui_alv_grid=>mc_style_enabled  )
                                  ( fieldname = 'VALIDADE'    style = cl_gui_alv_grid=>mc_style_enabled  ) ).
          INSERT LINES OF gt_estilo05 INTO TABLE wa_saida_05-celltab.

          MODIFY it_saida_05 FROM wa_saida_05 INDEX wg_selectedrow-index.

        ENDLOOP.
      ENDIF.

    WHEN '&EXCEL'.
      PERFORM z_importa_excel CHANGING l_erro.

      CHECK l_erro IS INITIAL.

      REFRESH it_arq_mapa.
      CLEAR wa_arq_mapa.

      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
        EXPORTING
          i_line_header        = 'X'
          i_tab_raw_data       = it_raw
          i_filename           = p_file
        TABLES
          i_tab_converted_data = it_arq_mapa
        EXCEPTIONS
          conversion_failed    = 1
          OTHERS               = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CHECK it_arq_mapa IS NOT INITIAL.

      REFRESH it_salvar05.
      CLEAR wa_salvar05.

      LOOP AT it_arq_mapa INTO wa_arq_mapa.

        wa_salvar05-mandt           =   sy-mandt.
        wa_salvar05-nr_cad          =   wa_arq_mapa-nr_cad.
        wa_salvar05-nr_reg          =   wa_arq_mapa-nr_reg.
        wa_salvar05-marca           =   wa_arq_mapa-marca.
        wa_salvar05-ing_ativo       =   wa_arq_mapa-ing_ativo.
        wa_salvar05-classe          =   wa_arq_mapa-classe.
        wa_salvar05-registrante     =   wa_arq_mapa-registrante.
        CONCATENATE wa_arq_mapa-validade+6(4) wa_arq_mapa-validade+3(2) wa_arq_mapa-validade+0(2) INTO wa_salvar05-validade.
        wa_salvar05-usnam           = sy-uname.
        wa_salvar05-data_atual      = sy-datum.
        wa_salvar05-hora_atual      = sy-uzeit.

        APPEND wa_salvar05 TO it_salvar05.
        CLEAR: wa_arq_mapa, wa_salvar05.

      ENDLOOP.

      CHECK it_salvar05 IS NOT INITIAL.

      MODIFY zsdt0217 FROM TABLE it_salvar05.
      MESSAGE 'Importação feita com sucesso!' TYPE 'I'.

      PERFORM seleciona_dados_mapa.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Z_IMPORTA_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_importa_excel CHANGING p_erro.

  DATA: lt_filetable TYPE filetable,
        ls_filetable LIKE LINE OF lt_filetable,
        l_rc         TYPE i.

  CLEAR: ls_filetable, p_file, p_erro.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Selecione o arquivo para Upload'
      default_extension       = 'XLSX'
      file_filter             = 'Arquivos do Excel (*.XLS)|*.XLS| Excel files (*.XLSX)|*.XLSX|'
    CHANGING
      file_table              = lt_filetable
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE lt_filetable INTO ls_filetable INDEX 1.
    IF sy-subrc <> 0.
      p_erro = abap_true.
    ENDIF.
    p_file = ls_filetable-filename.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_MAPA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados_mapa.

  SELECT * FROM zsdt0217 INTO TABLE it_zsdt0217.


  SELECT * FROM zsdt0201 INTO TABLE it_zsdt0201
    FOR ALL ENTRIES IN it_zsdt0217
   WHERE codmapa EQ it_zsdt0217-nr_reg.


  CHECK it_zsdt0217 IS NOT INITIAL.


  LOOP AT it_zsdt0217  INTO wa_zsdt0217.

    wa_saida_05-busca_indea    =  wa_zsdt0217-busca_indea.
    wa_saida_05-nr_cad         =  wa_zsdt0217-nr_cad.
    wa_saida_05-nr_reg         =  wa_zsdt0217-nr_reg.
    wa_saida_05-marca          =  wa_zsdt0217-marca.
    wa_saida_05-ing_ativo      =  wa_zsdt0217-ing_ativo.
    wa_saida_05-classe         =  wa_zsdt0217-classe.
    wa_saida_05-registrante    =  wa_zsdt0217-registrante.
    wa_saida_05-validade       =  wa_zsdt0217-validade.

    FREE wa_saida_05-celltab.
    gt_estilo05 =  VALUE #( ( fieldname = 'NR_CAD'      style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'NR_REG'      style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'MARCA'       style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'ING_ATIVO'   style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'CLASSE'      style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'REGISTRANTE' style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'VALIDADE'    style = cl_gui_alv_grid=>mc_style_disabled  ) ).
    INSERT LINES OF gt_estilo05 INTO TABLE wa_saida_05-celltab.


    READ TABLE it_zsdt0201 INTO wa_zsdt0201 WITH KEY codmapa = wa_zsdt0217-nr_reg.
    IF sy-subrc = 0.
      wa_saida_05-color = 'C510'.
    ENDIF.

    APPEND wa_saida_05 TO it_saida_05.
    CLEAR: wa_zsdt0217, wa_saida_05.
  ENDLOOP.

ENDFORM.

*-US191683-25.09.2025-#191683-JT-inicio
***************************************************************
* preencher dados_roteiro
***************************************************************
FORM f_preenche_dados_roteiro.

  DATA: gt_estilo01 TYPE lvc_t_styl.

  DATA: lr_kunnr TYPE zde_kunnr_t.

  lr_kunnr[] = p_kunnr[].

  zcl_indea=>selecao_dados_roteiro( EXPORTING i_kunnr           = lr_kunnr
                                              i_nro_rot         = p_nr_rot
                                              i_sem_propriedade = abap_true
                                    IMPORTING e_saida           = it_saida_01 ).

  LOOP AT it_saida_01 ASSIGNING FIELD-SYMBOL(<fs_saida>).
    DATA(lv_tabix) = sy-tabix.

    SELECT SINGLE *
      FROM zsdt0132
      INTO @DATA(wa_0132)
     WHERE nr_rot = @<fs_saida>-nr_rot.

    FREE: <fs_saida>-celltab, gt_estilo01.
    gt_estilo01 =  VALUE #( ( fieldname = 'KUNNR'           style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'NR_ROT'          style = cl_gui_alv_grid=>mc_style_disabled  )
                            ( fieldname = 'ID_PROPRIEDADE'  style = cl_gui_alv_grid=>mc_style_enabled   ) ).
    INSERT LINES OF gt_estilo01 INTO TABLE <fs_saida>-celltab.

    PERFORM f_move_dados_roteiro USING <fs_saida> wa_0132 lv_tabix.
  ENDLOOP.

ENDFORM.

***************************************************************
* mover dados saida01
***************************************************************
FORM f_move_dados_roteiro USING p_saida  TYPE zsde0408
                                p_0132   TYPE zsdt0132
                                p_row_id TYPE int4.

  MOVE: p_saida           TO wa_saida_01.
  MOVE: p_0132-nr_rot     TO wa_saida_01-nr_rot.
  MOVE: p_0132-rot_desc   TO wa_saida_01-rot_desc.
  MOVE: p_0132-city1      TO wa_saida_01-city1.
  MOVE: p_0132-uf         TO wa_saida_01-uf.
  MOVE: p_0132-tel_number TO wa_saida_01-tel_number.
  MOVE: p_0132-status     TO wa_saida_01-status.

  CLEAR wl_name.
  wl_name =  wa_saida_01-nr_rot.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id       = 'ZROT'
      language = sy-langu
      name     = wl_name
      object   = 'ZSDROTEIRO'
    TABLES
      lines    = it_texto
    EXCEPTIONS
      OTHERS   = 1.

  IF it_texto IS INITIAL.
    wa_saida_01-texto01 = '@1F@'.
  ELSE.
    wa_saida_01-texto01 = '@1E@'.
  ENDIF.

*  SELECT SINGLE * FROM zsdt0206 INTO @DATA(wa206)
*    WHERE kunnr EQ @wa_saida_01-kunnr.
*
*  SELECT SINGLE * FROM  zsdt0207 INTO  @DATA(wa207)
*   WHERE id_produtor EQ @wa206-id_produtor.
*
*  wa_saida_01-id_propriedade = wa207-id_propriedade.
*  wa_saida_01-nome           = wa207-nome.
*
*  IF wa207-via_acesso IS INITIAL.
*    wa_saida_01-texto02 = '@1F@'.
*  ELSE.
*    wa_saida_01-texto02 = '@1E@'.
*    wa_saida_01-via_acesso     = wa207-via_acesso.
*  ENDIF.

  MODIFY it_saida_01 FROM wa_saida_01 INDEX p_row_id.

ENDFORM.
*-US191683-25.09.2025-#191683-JT-fim

***************************************************************
***************************************************************
