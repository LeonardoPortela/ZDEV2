*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Rodrigo C.                                              &*
*& Data.....: 10/01/2024                                              &*
*& Descrição: Consulta NCM                                            &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zpmr0083 MESSAGE-ID zcarga.

"Tabelas
TABLES: mara, marc, t134t, t141t, t023t, t604n, ibrmatuset, ibrmatorgt, tvkmt.

TYPES: BEGIN OF ty_data_material,
         matnr                 TYPE mara-matnr,  "Material
         maktx                 TYPE makt-maktx,  "DESC.MATERIAL
         matkl                 TYPE mara-matkl,  "GrpMercadoria
         bwkey                 TYPE mbew-bwkey,  "Centro
         name1                 TYPE t001w-name1, "Descrição do centro
         steuc                 TYPE marc-steuc,  "Cód.controle
         mtart                 TYPE mara-mtart,  "TMaterial
         mstae                 TYPE mara-mstae,  "Status material especifico para centro
         extwg                 TYPE mara-extwg,
         indus                 TYPE marc-indus,
         mtuse                 TYPE mbew-mtuse,
         mtorg                 TYPE mbew-mtorg,
         ownpr                 TYPE mbew-ownpr,
         vtweg                 TYPE mvke-vtweg,
         ktgrm                 TYPE mvke-ktgrm,
         mtbez                 TYPE t134t-mtbez,
         mtstb                 TYPE t141t-mtstb,
         wgbez                 TYPE t023t-wgbez,
         text1                 TYPE t604n-text1,
         br_mtuse_text         TYPE ibrmatuset-br_materialusagedesc,
         br_indus_text         TYPE ibrmatuset-br_materialusagedesc,
         br_materialorigindesc TYPE ibrmatorgt-br_materialorigindesc,
         vtext                 TYPE tvkmt-vtext,
       END OF ty_data_material,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         mtart TYPE mara-mtart,
         matkl TYPE mara-matkl,
         lvorm TYPE mara-lvorm,
         extwg TYPE mara-extwg,
         mstae TYPE mara-mstae,
       END OF ty_mara,

       BEGIN OF ty_mvke,
         matnr TYPE mvke-matnr,
         vtweg TYPE mvke-vtweg,
         ktgrm TYPE mvke-ktgrm,
       END OF ty_mvke,

       BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt,

       BEGIN OF ty_marc,
         matnr TYPE marc-matnr,
         werks TYPE marc-werks,
         steuc TYPE marc-steuc,
         indus TYPE marc-indus,
       END OF ty_marc,

       BEGIN OF ty_mbew,
         matnr TYPE mbew-matnr,
         bwkey TYPE mbew-bwkey,
         lvorm TYPE mbew-lvorm,
         mtuse TYPE mbew-mtuse,
         mtorg TYPE mbew-mtorg,
         ownpr TYPE mbew-ownpr,
       END OF ty_mbew,

       BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
       END OF ty_t001w.


TYPES: BEGIN OF ty_editor,
         line(72),
       END   OF ty_editor.

"Tabela Interna Global
DATA: gt_data_material TYPE TABLE OF ty_data_material,
      gt_mara          TYPE TABLE OF ty_mara,
      gt_makt          TYPE TABLE OF ty_makt,
      gt_marc          TYPE TABLE OF ty_marc,
      gt_mbew          TYPE TABLE OF ty_mbew,
      gt_t001w         TYPE TABLE OF ty_t001w,
      gt_mvke          TYPE TABLE OF ty_mvke,
      git_filtro       TYPE zif_screen_linha_filtro_t.


DATA: gt_t134t      TYPE TABLE OF t134t,
      gt_t141t      TYPE TABLE OF t141t,
      gt_t023t      TYPE TABLE OF t023t,
      gt_t604n      TYPE TABLE OF t604n,
      gt_dd07v      TYPE TABLE OF dd07v,
      gt_ibrmatorgt TYPE TABLE OF ibrmatorgt,
      gt_tvkmt      TYPE TABLE OF tvkmt.


"Work Área
DATA: wa_data_material TYPE ty_data_material.



"Objetos
DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,

      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,
      git_fcat                    TYPE lvc_t_fcat,
      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid,
      lines                       TYPE sy-tabix,
      wa_selected_rows            TYPE lvc_s_row,
      it_selected_rows            TYPE lvc_t_row.


" Classe
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA:  event_receiver   TYPE REF TO lcl_event_receiver.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:

      hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no .


    CLASS-METHODS:
      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD hotspot_click.
*    CASE e_column_id.
*      WHEN 'STATUS_PROCESSAMENTO'.
*        PERFORM fm_log_erros USING e_row_id
*                             e_column_id.
*    ENDCASE.
*
*
*
*    CALL METHOD gob_gui_alv_grid->refresh_table_display.

  ENDMETHOD.

  METHOD get_ucomm.

    CASE sy-ucomm.
      WHEN 'PROCESSAR'.

        CALL METHOD gob_gui_alv_grid->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


*&---------------------------------------------------------------------*
*&      Tela de seleção
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_werks  FOR marc-werks,  "Centro
                  s_matnr  FOR mara-matnr,  "Nº do material:
                  s_matkl  FOR mara-matkl,  "Grupo mercadoria
                  s_mtart  FOR mara-mtart,  "Tipo de material
                  s_mstae  FOR mara-mstae, "Status do material
                  s_steuc  FOR marc-steuc.

SELECTION-SCREEN: END OF BLOCK b1.



INITIALIZATION.


AT SELECTION-SCREEN. "PAI


START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.


*&---------------------------------------------------------------------*
*& Form fm_start_of_selection
*&---------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_dados_seleciona.
  PERFORM fm_dados_processa.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_dados_seleciona
*&---------------------------------------------------------------------*
FORM fm_dados_seleciona .

  " Seleciona dados de material
  IF s_steuc IS INITIAL.
    SELECT matnr mtart matkl lvorm extwg mstae
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE gt_mara
    WHERE matnr  IN s_matnr
    AND   mtart  IN s_mtart
    AND   matkl  IN s_matkl
    AND   mstae  IN s_mstae.
  ELSE.
    SELECT *
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE @gt_mara
    WHERE matnr  IN @s_matnr
    AND   mtart  IN @s_mtart
    AND   matkl  IN @s_matkl
    AND   mstae  IN @s_mstae
    AND ( EXISTS (
    SELECT *
    FROM marc
    WHERE steuc IN @s_steuc ) ).
  ENDIF.

  IF gt_mara[] IS INITIAL .
    MESSAGE 'Registros não encontrados !' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT gt_mara BY matnr.

  SELECT mtart, mtbez
  FROM t134t INTO CORRESPONDING FIELDS OF TABLE @gt_t134t
  FOR ALL ENTRIES IN @gt_mara
 WHERE mtart EQ @gt_mara-mtart
   AND spras EQ @sy-langu.

  SELECT mmsta, mtstb
    FROM t141t INTO CORRESPONDING FIELDS OF TABLE @gt_t141t
    FOR ALL ENTRIES IN @gt_mara
   WHERE mmsta EQ @gt_mara-mstae
     AND spras EQ @sy-langu.

  SELECT matkl, wgbez
  FROM t023t INTO CORRESPONDING FIELDS OF TABLE @gt_t023t
  FOR ALL ENTRIES IN @gt_mara
 WHERE matkl EQ @gt_mara-matkl
   AND spras EQ @sy-langu.

  SELECT matnr vtweg ktgrm
         FROM mvke
         INTO CORRESPONDING FIELDS OF TABLE gt_mvke
         FOR ALL ENTRIES IN gt_mara
         WHERE matnr EQ gt_mara-matnr.

  SORT gt_mvke BY matnr.

  IF gt_mvke IS NOT INITIAL.
    SELECT ktgrm, vtext
    FROM tvkmt INTO CORRESPONDING FIELDS OF TABLE @gt_tvkmt
    FOR ALL ENTRIES IN @gt_mvke
    WHERE ktgrm EQ @gt_mvke-ktgrm
    AND spras EQ @sy-langu.
  ENDIF.

  " Seleciona Textos breves de material
  SELECT matnr maktx
         FROM makt
         INTO CORRESPONDING FIELDS OF TABLE gt_makt
         FOR ALL ENTRIES IN gt_mara
         WHERE matnr EQ gt_mara-matnr.

  SORT gt_makt BY matnr.

  " Seleciona Dados de centro para material
  SELECT matnr werks indus steuc
  FROM marc
  INTO CORRESPONDING FIELDS OF TABLE gt_marc
  FOR ALL ENTRIES IN gt_mara
  WHERE matnr EQ gt_mara-matnr
  AND   werks IN s_werks.

  IF NOT gt_marc[] IS INITIAL.

    SORT gt_marc BY matnr werks.

    SELECT steuc, text1
    FROM t604n INTO CORRESPONDING FIELDS OF TABLE @gt_t604n
    FOR ALL ENTRIES IN @gt_marc
    WHERE steuc EQ @gt_marc-steuc
    AND spras EQ @sy-langu.

    " Seleciona Avaliação do material
    SELECT matnr bwkey lvorm mtuse mtorg ownpr
           FROM mbew
           INTO CORRESPONDING FIELDS OF TABLE gt_mbew
           FOR ALL ENTRIES IN gt_marc
           WHERE matnr EQ gt_marc-matnr
           AND   bwkey EQ gt_marc-werks.

    SORT gt_mbew BY matnr bwkey.

    IF gt_mbew IS NOT INITIAL.

      SELECT br_materialorigin, br_materialorigindesc
      FROM ibrmatorgt INTO CORRESPONDING FIELDS OF TABLE @gt_ibrmatorgt
      FOR ALL ENTRIES IN @gt_mbew
      WHERE br_materialorigin EQ @gt_mbew-mtorg
      AND language EQ @sy-langu.
    ENDIF.

    " Seleciona Centros/filiais
    SELECT werks name1
           FROM t001w
           INTO CORRESPONDING FIELDS OF TABLE gt_t001w
           FOR ALL ENTRIES IN gt_marc
           WHERE werks EQ gt_marc-werks.

    SORT gt_t001w BY werks.

  ENDIF.


  SELECT *
  FROM dd07v INTO CORRESPONDING FIELDS OF TABLE @gt_dd07v
  WHERE domname EQ 'J_1BINDUS3'
  AND ddlanguage EQ @sy-langu.

  SELECT *
  FROM dd07v APPENDING CORRESPONDING FIELDS OF TABLE @gt_dd07v
  WHERE domname EQ 'J_1BMATUSE'
  AND ddlanguage EQ @sy-langu.

  SELECT *
  FROM dd07v APPENDING CORRESPONDING FIELDS OF TABLE @gt_dd07v
  WHERE domname EQ 'J_1BMATORG'
  AND ddlanguage EQ @sy-langu.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_dados_processa
*&---------------------------------------------------------------------*
FORM fm_dados_processa .

  LOOP AT gt_mbew INTO DATA(wa_mbew).

    READ TABLE gt_marc INTO DATA(wa_marc) WITH KEY matnr = wa_mbew-matnr werks = wa_mbew-bwkey BINARY SEARCH.
    IF sy-subrc EQ 0.

      READ TABLE gt_t604n INTO DATA(wa_t604n) WITH KEY steuc = wa_marc-steuc.
      IF sy-subrc EQ 0.
        wa_data_material-text1 = wa_t604n-text1.
      ENDIF.

      READ TABLE gt_mara INTO DATA(wa_mara) WITH KEY matnr = wa_mbew-matnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE gt_t134t INTO DATA(wa_t134t) WITH KEY mtart = wa_mara-mtart.
        IF sy-subrc EQ 0.
          wa_data_material-mtbez = wa_t134t-mtbez.
        ENDIF.

        READ TABLE gt_t141t INTO DATA(wa_t141t) WITH KEY mmsta = wa_mara-mstae.
        IF sy-subrc EQ 0.
          wa_data_material-mtstb = wa_t141t-mtstb.
        ENDIF.

        READ TABLE gt_t023t INTO DATA(wa_t023t) WITH KEY matkl = wa_mara-matkl.
        IF sy-subrc EQ 0.
          wa_data_material-wgbez = wa_t023t-wgbez.
        ENDIF.


        READ TABLE gt_makt INTO DATA(wa_makt) WITH KEY matnr = wa_mbew-matnr BINARY SEARCH.
        IF sy-subrc EQ 0.

          wa_data_material-matnr = wa_mbew-matnr.
          wa_data_material-mtuse = wa_mbew-mtuse.
          wa_data_material-mtorg = wa_mbew-mtorg.
          wa_data_material-ownpr = wa_mbew-ownpr.
          wa_data_material-maktx = wa_makt-maktx.
          wa_data_material-matkl = wa_mara-matkl.
          wa_data_material-bwkey = wa_mbew-bwkey.

          READ TABLE gt_t001w INTO DATA(wa_t001w) WITH KEY werks = wa_marc-werks BINARY SEARCH.
          IF sy-subrc EQ 0.
            wa_data_material-name1 = wa_t001w-name1.
          ENDIF.

          READ TABLE gt_mvke INTO DATA(wa_mvke) WITH KEY matnr = wa_mara-matnr BINARY SEARCH.
          IF sy-subrc EQ 0.
            wa_data_material-vtweg = wa_mvke-vtweg.
            wa_data_material-ktgrm = wa_mvke-ktgrm.
          ENDIF.

          READ TABLE gt_dd07v INTO DATA(wa_dd07v) WITH KEY domvalue_l = wa_marc-indus domname = 'J_1BINDUS3'.
          IF sy-subrc EQ 0.
            wa_data_material-br_indus_text = wa_dd07v-ddtext.
          ENDIF.

          CLEAR: wa_dd07v.
          READ TABLE gt_dd07v INTO wa_dd07v WITH KEY domvalue_l = wa_mbew-mtuse domname = 'J_1BMATUSE'.
          IF sy-subrc EQ 0.
            wa_data_material-br_mtuse_text = wa_dd07v-ddtext.
          ENDIF.

*          READ TABLE gt_dd07v INTO DATA(wa_dd07v) WITH KEY domvalue_l = wa_marc-indus domname = 'J_1BMATORG'.
*          IF sy-subrc EQ 0.
*            wa_data_material-br_materialusagedesc = wa_dd07v-ddtext.
*          ENDIF.

          READ TABLE gt_ibrmatorgt INTO DATA(wa_ibrmatorgt) WITH KEY br_materialorigin = wa_mbew-mtorg.
          IF sy-subrc EQ 0.
            wa_data_material-br_materialorigindesc = wa_ibrmatorgt-br_materialorigindesc.
          ENDIF.

          READ TABLE gt_tvkmt INTO DATA(wa_tvkmt) WITH KEY ktgrm = wa_mvke-ktgrm.
          IF sy-subrc EQ 0.
            wa_data_material-vtext = wa_tvkmt-vtext.
          ENDIF.


          wa_data_material-indus = wa_marc-indus.
          wa_data_material-steuc = wa_marc-steuc.
          wa_data_material-mtart = wa_mara-mtart.
          wa_data_material-mstae = wa_mara-mstae.
          wa_data_material-extwg = wa_mara-extwg.
          APPEND wa_data_material TO gt_data_material.
          CLEAR: wa_data_material, wa_ibrmatorgt, wa_marc, wa_tvkmt, wa_mara, wa_mvke, wa_t001w, wa_mbew, wa_makt, wa_t023t, wa_t134t, wa_t604n, wa_t141t, wa_dd07v.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_end_of_selection
*&---------------------------------------------------------------------*
FORM fm_end_of_selection .
  PERFORM fm_filtros.
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_filtros
*&---------------------------------------------------------------------*
FORM fm_filtros .
  DATA vl_text TYPE TABLE OF textpool.

  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname = sy-repid
      action     = 'SHOW'
      language   = sy-langu
    TABLES
      tpool      = vl_text.

  FREE: git_filtro.

*  LOOP AT SCREEN.
*    git_filtro = VALUE #(
*      ( parametro = '' valor = p_bukrs )
*      ( parametro = '' valor = p_werks )
*    ).
*  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'PFS0100'.
  SET TITLEBAR 'STB0100' WITH 'Consulta NCM'.

  PERFORM fm_criar_objetos.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form fm_criar_objetos
*&---------------------------------------------------------------------*
FORM fm_criar_objetos .

  DATA: lva_data(22) TYPE c,
        w_layout     TYPE lvc_s_layo.

  DATA: gs_variant  TYPE disvariant.
  gs_variant-report      = sy-repid.

  PERFORM fm_cria_fieldcat.

  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Consulta NCM'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


*    CREATE OBJECT event_receiver.
*    SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.
*    SET HANDLER event_receiver->get_ucomm  FOR gob_gui_alv_grid.

*    w_layout-cwidth_opt = abap_true.
    w_layout-zebra      = 'X'.
    w_layout-sel_mode   = 'A'.
    w_layout-col_opt    = abap_true.



    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        is_variant                    = gs_variant
      CHANGING
        it_outtab                     = gt_data_material
        it_fieldcatalog               = git_fcat
*       IT_SORT                       =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_cria_fieldcat
*&---------------------------------------------------------------------*
FORM fm_cria_fieldcat .


  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

  git_fcat = VALUE lit_fieldcat_aux(
( fieldname ='MATNR'                              coltext = 'Material'                                    no_zero = '' outputlen = 20    ref_table = 'MARA'  ref_field = 'MATNR' ) "col_opt = 'X'
( fieldname ='MAKTX'                              coltext = 'Desc. material'                              no_zero = '' outputlen = 30    ref_table = 'MAKT'  ref_field = 'MAKTX' )
( fieldname ='MATKL'                              coltext = 'Grp. mercadoria'                             no_zero = '' outputlen = 15    ref_table = 'MARA'  ref_field = 'MATKL' )
( fieldname ='BWKEY'                              coltext = 'Centro'                                      no_zero = '' outputlen = 15    ref_table = 'MARC'  ref_field = 'BWKEY' )
( fieldname ='NAME1'                              coltext = 'Desc. centro'                                no_zero = '' outputlen = 40    ref_table = ''  ref_field = '' )
( fieldname ='STEUC'                              coltext = 'Cod. NCM'                                    no_zero = '' outputlen = 15    ref_table = 'MARC'  ref_field = 'STEUC' )
( fieldname ='TEXT1'                              coltext = 'Desc.NCM'                                    no_zero = '' outputlen = 30    ref_table = ''  ref_field = '' )
( fieldname ='MTART'                              coltext = 'Tipo de material'                            no_zero = '' outputlen = 15    ref_table = 'MARA'  ref_field = 'MTART' )
( fieldname ='MTBEZ'                              coltext = 'Desc.tipo de material'                       no_zero = '' outputlen = 30    ref_table = ''  ref_field = '' )
( fieldname ='MSTAE'                              coltext = 'Status material'                             no_zero = '' outputlen = 15    ref_table = 'MARA'  ref_field = 'MSTAE' )
( fieldname ='MTSTB'                              coltext = 'Desc.status material'                        no_zero = '' outputlen = 30    ref_table = ''  ref_field = '' )
( fieldname ='EXTWG'                              coltext = 'Grp. de merc externo'                        no_zero = '' outputlen = 15    ref_table = ''  ref_field = '' )
( fieldname ='WGBEZ'                              coltext = 'Desc.grp. merc'                              no_zero = '' outputlen = 30    ref_table = ''  ref_field = '' )
( fieldname ='INDUS'                              coltext = 'Material: categoria CFOP'                    no_zero = '' outputlen = 15     ref_table = ''  ref_field = '' )
( fieldname ='BR_INDUS_TEXT'                      coltext = 'Desc.categoria CFOP'                         no_zero = '' outputlen = 30     ref_table = ''  ref_field = '' )
( fieldname ='MTUSE'                              coltext = 'Utilização de material '                    	no_zero = '' outputlen = 15     ref_table = ''  ref_field = '' )
( fieldname ='BR_MTUSE_TEXT'                      coltext = 'Desc.utilização material'                    no_zero = '' outputlen = 30     ref_table = ''  ref_field = '' )
( fieldname ='MTORG'                              coltext = 'Origem de material '                         no_zero = '' outputlen = 15     ref_table = ''  ref_field = '' )
( fieldname ='BR_MATERIALORIGINDESC'              coltext = 'Desc.origem de material '                    no_zero = '' outputlen = 30     ref_table = ''  ref_field = '' )
( fieldname ='OWNPR'                              coltext = 'Produção interna'                            no_zero = '' outputlen = 15     ref_table = ''  ref_field = '' )
( fieldname ='VTWEG'                              coltext = 'Canal de distribuição'                       no_zero = '' outputlen = 15     ref_table = ''  ref_field = '' )
( fieldname ='KTGRM'                              coltext = 'Grupo de Classificação Contábil de Material' no_zero = '' outputlen = 15     ref_table = ''  ref_field = '' )
( fieldname ='VTEXT'                              coltext = 'Desc.classificação Material'                 no_zero = '' outputlen = 30     ref_table = ''  ref_field = '' )
).
*  DATA: lc_col_pos  TYPE lvc_colpos.
*  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.
*  CLEAR: git_fcat.
*
*  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_structure_name = 'ZDE_DATA_COMBOIO_ABASTECIMENTO'
*    CHANGING
*      ct_fieldcat      = git_fcat.
*
*  LOOP AT git_fcat ASSIGNING <fs_cat>.
*
*
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.
