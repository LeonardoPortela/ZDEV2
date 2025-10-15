*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: CAMILA BRAND                                            &*
*& Data.....: 19.01.2022                                              &*
*& Descrição: Customização - " DE X PARA " - Filial 0160 | RS         &*
*& Transação:  ZLES0194                                               &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zlesr0153.

TABLES: zlest0041.

*---------------------------------------------------------------------*
* TYPES                                                               *
*---------------------------------------------------------------------*
TYPE-POOLS: slis, icon.

TYPES: BEGIN OF ty_zlest0041,
         data_emissao     TYPE zlest0041-data_emissao,
         nr_nf            TYPE zlest0041-nr_nf,
         nr_nf_doc        TYPE j_1bnfdoc-nfnum,
         centro_comprador TYPE zlest0041-centro_comprador,
         serie            TYPE zlest0041-serie,
         cod_cliente      TYPE zlest0041-cod_cliente,
         nr_nf_propria    TYPE zlest0041-nr_nf_propria,
         serie_propria    TYPE zlest0041-serie_propria,
         docnum           TYPE zlest0041-docnum,
         quantidade       TYPE zlest0041-quantidade,
         cod_material     TYPE zlest0041-cod_material,
         ch_referencia    TYPE zlest0041-ch_referencia,
         serie_sz         TYPE zlest0041-serie,
       END OF ty_zlest0041.

TYPES: BEGIN OF ty_estrutura.
*         INCLUDE TYPE slis_fieldcat_main.
*         INCLUDE TYPE slis_fieldcat_alv_spec.
         INCLUDE TYPE slis_fieldcat_alv.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_saida,
         centro_comprador TYPE zlest0041-centro_comprador,
         cod_cliente      TYPE zlest0041-cod_cliente,
         name1            TYPE lfa1-name1,
         nr_nf            TYPE zlest0041-nr_nf,
         chv_nota_doc(44) TYPE c,
         docdat_doc       TYPE j_1bnfdoc-docdat,
         quantidade       TYPE zlest0041-quantidade,
         docnum_doc       TYPE j_1bnfdoc-docnum,
         nr_nf_propria    TYPE zlest0041-nr_nf_propria,
         chv_nota_41(44)  TYPE c,
         docdat_41        TYPE j_1bnfdoc-docdat,
         doc_num_41       TYPE zlest0041-docnum,
         cod_material     TYPE zlest0041-cod_material.
TYPES: END OF ty_saida.

*---------------------------------------------------------------------*
* DATA                                                                *
*---------------------------------------------------------------------*
DATA: git_zlest0041_aux     TYPE TABLE OF zlest0041,
      git_zlest0041         TYPE TABLE OF ty_zlest0041,
      git_j_1bnfdoc         TYPE TABLE OF j_1bnfdoc,
      git_j_1bnfdoc_41      TYPE TABLE OF j_1bnfdoc,
      git_j_1bnfe_active    TYPE TABLE OF j_1bnfe_active,
      git_j_1bnfe_active_41 TYPE TABLE OF j_1bnfe_active,
      git_lfa1              TYPE TABLE OF lfa1,
      git_saida             TYPE TABLE OF ty_saida,
      git_sort              TYPE lvc_t_sort,
      git_exclude_fcode     TYPE ui_functions,
      gs_layout             TYPE lvc_s_layo,
      gs_variant            TYPE disvariant,
      git_filtro            TYPE zif_screen_linha_filtro_t,
      git_fcat              TYPE lvc_t_fcat.


DATA: gwa_zlest0041 TYPE ty_zlest0041,
      gwa_saida     TYPE ty_saida,
      gwa_j_1bnfdoc TYPE j_1bnfdoc.

RANGES: s_datum FOR sy-datum.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*

DATA: gob_gui_alv_grid   TYPE REF TO cl_gui_alv_grid,
      dg_parent_alv      TYPE REF TO cl_gui_container,
      ls_stable          TYPE lvc_s_stbl,
      g_custom_container TYPE REF TO cl_gui_custom_container.

*----------------------------------------------------------------------*
* Tela de seleção *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: pdtem   FOR sy-datum OBLIGATORY, "NO-EXTENSION,
                pccomp  FOR zlest0041-centro_comprador ,
                pnrnf   FOR zlest0041-nr_nf,
                nr_nf   FOR zlest0041-nr_nf_propria,
                pccli   FOR zlest0041-cod_cliente,
                pdocn   FOR zlest0041-docnum.
SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.

*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_seleciona_dados .
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM fm_seleciona_dados .

  DATA: r_pnrnf TYPE RANGE OF j_1bnfnum9,
        r_nr_nf TYPE RANGE OF j_1bnfnum9.


  IF pdtem-high IS NOT INITIAL.
    LOOP AT pdtem.
      MOVE: 'I'  TO s_datum-sign,
            'BT' TO s_datum-option,
            pdtem-low TO s_datum-low,
            pdtem-high TO s_datum-high.
      APPEND s_datum.
    ENDLOOP.
  ELSE.
    LOOP AT pdtem.
      MOVE: 'I'  TO s_datum-sign,
            'EQ' TO s_datum-option,
            pdtem-low TO s_datum-low.
      APPEND s_datum.
    ENDLOOP.
  ENDIF.

  "Ajustar zeros a esquerda.
  IF pnrnf IS NOT INITIAL.
    r_pnrnf = VALUE #( FOR l IN pnrnf ( sign = 'I' option = 'EQ' low = |{ l-low ALPHA = IN }| ) ).
  ENDIF.

  IF nr_nf IS NOT INITIAL.
    r_nr_nf = VALUE #( FOR t IN nr_nf ( sign = 'I' option = 'EQ' low = |{ t-low ALPHA = IN }| ) ).
  ENDIF.


  SELECT * INTO TABLE git_zlest0041_aux
    FROM zlest0041
   WHERE centro_comprador IN pccomp
      AND nr_nf           IN r_pnrnf
      AND nr_nf_propria   IN r_nr_nf
      AND cod_cliente     IN pccli
      AND docnum          IN pdocn
      AND data_emissao    IN s_datum.


  IF git_zlest0041_aux IS NOT INITIAL.

    LOOP AT git_zlest0041_aux INTO DATA(lwa_zlest0041_aux).
      MOVE-CORRESPONDING lwa_zlest0041_aux TO gwa_zlest0041.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = lwa_zlest0041_aux-serie
        IMPORTING
          output = gwa_zlest0041-serie_sz.

      gwa_zlest0041-nr_nf_doc = gwa_zlest0041-nr_nf.

      APPEND gwa_zlest0041 TO git_zlest0041.
      CLEAR gwa_zlest0041.

    ENDLOOP.

    SELECT *
      FROM  j_1bnfdoc
      APPENDING CORRESPONDING FIELDS OF TABLE git_j_1bnfdoc
       FOR ALL ENTRIES IN git_zlest0041
     WHERE branch   = git_zlest0041-centro_comprador
       AND parid    = git_zlest0041-cod_cliente
       AND nfenum   = git_zlest0041-nr_nf
       AND series   = git_zlest0041-serie_sz.  "sem os zeros a esquerda.

* Se não encontrar , realizar uma segunda seleção, para verificar se é nota não eletrônica
    SELECT *
      FROM  j_1bnfdoc
      APPENDING CORRESPONDING FIELDS OF TABLE git_j_1bnfdoc
       FOR ALL ENTRIES IN git_zlest0041
     WHERE branch   = git_zlest0041-centro_comprador
       AND parid    = git_zlest0041-cod_cliente
       AND nfnum    = git_zlest0041-nr_nf_doc
       AND series   = git_zlest0041-serie_sz.  "sem os zeros a esquerda.

    IF git_j_1bnfdoc IS NOT INITIAL.
      LOOP AT git_j_1bnfdoc INTO DATA(lwa_j_1bnfdoc) WHERE nfe = 'X'.
        SELECT *
         FROM  j_1bnfe_active APPENDING CORRESPONDING FIELDS OF TABLE git_j_1bnfe_active
        WHERE docnum = lwa_j_1bnfdoc-docnum.
        CLEAR: lwa_j_1bnfdoc.
      ENDLOOP.
    ENDIF.

    SELECT *
      FROM  j_1bnfdoc
      INTO TABLE git_j_1bnfdoc_41
       FOR ALL ENTRIES IN git_zlest0041
      WHERE docnum = git_zlest0041-docnum.

    IF git_j_1bnfdoc_41 IS NOT INITIAL.
      LOOP AT git_j_1bnfdoc_41 INTO DATA(lwa_j_1bnfdoc_41) WHERE nfe = 'X'.
        SELECT *
         FROM  j_1bnfe_active APPENDING CORRESPONDING FIELDS OF TABLE git_j_1bnfe_active_41
        WHERE docnum = lwa_j_1bnfdoc_41-docnum.
        CLEAR: lwa_j_1bnfdoc_41.
      ENDLOOP.
    ENDIF.

    SELECT *
      FROM  lfa1
      INTO TABLE git_lfa1
       FOR ALL ENTRIES IN git_zlest0041
      WHERE lifnr = git_zlest0041-cod_cliente
        AND land1 = 'BR'.


    PERFORM fm_organiza_dados.

  ELSE.
    MESSAGE s000(z_les) WITH 'Dados não encontrados'(002) DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM fm_organiza_dados .

  DATA: lva_chave  TYPE c LENGTH 44.

  LOOP AT git_zlest0041 INTO gwa_zlest0041.

    gwa_saida-centro_comprador = gwa_zlest0041-centro_comprador.
    gwa_saida-cod_cliente      = gwa_zlest0041-cod_cliente.

    READ TABLE git_lfa1 INTO DATA(lwa_lfa1) WITH KEY lifnr = gwa_zlest0041-cod_cliente.
    gwa_saida-name1 = lwa_lfa1-name1.
    gwa_saida-nr_nf = gwa_zlest0041-nr_nf.

    READ TABLE git_j_1bnfdoc INTO DATA(lwa_j_1bnfdoc) WITH KEY branch = gwa_zlest0041-centro_comprador
                                                               parid  = gwa_zlest0041-cod_cliente
                                                               nfnum  = gwa_zlest0041-nr_nf_doc
                                                               series = gwa_zlest0041-serie_sz.

    IF sy-subrc = 0.
* Não Eletronica
*      READ TABLE git_j_1bnfe_active INTO DATA(lwa_j_1bnfe_active) WITH KEY docnum = lwa_j_1bnfdoc-docnum.
*
*      IF sy-subrc = 0.
*        DATA: lva_chave  TYPE c LENGTH 44.
*
*
*        CONCATENATE lwa_j_1bnfe_active-regio   "Região do emissor NF-e
*                    lwa_j_1bnfe_active-nfyear  "Ano da data do documento da NF-e
*                    lwa_j_1bnfe_active-nfmonth "Mês da data do documento da NF-e
*                    lwa_j_1bnfe_active-stcd1   "Nº CNPJ do emissor da NF-e
*                    lwa_j_1bnfe_active-model   "Modelo da nota fiscal
*                    lwa_j_1bnfe_active-serie   "SERIE
*                    lwa_j_1bnfe_active-nfnum9  "Nº NF-e de nove posições
*                    lwa_j_1bnfe_active-docnum9 "NF-e: nº aleatório
*                    lwa_j_1bnfe_active-cdv     "Dígito controle p/chave de acesso NF-e
*     INTO lva_chave.
*
*        gwa_saida-chv_nota_doc = lva_chave.
*        CLEAR: lva_chave.
*      ENDIF.

      gwa_saida-docdat_doc = lwa_j_1bnfdoc-docdat.

    ELSE.

      READ TABLE git_j_1bnfdoc INTO lwa_j_1bnfdoc WITH KEY branch = gwa_zlest0041-centro_comprador
                                                           parid  = gwa_zlest0041-cod_cliente
                                                           nfenum   = gwa_zlest0041-nr_nf
                                                           series = gwa_zlest0041-serie_sz.
      IF sy-subrc = 0.
        READ TABLE git_j_1bnfe_active INTO DATA(lwa_j_1bnfe_active) WITH KEY docnum = lwa_j_1bnfdoc-docnum.

        IF sy-subrc = 0.
          CONCATENATE lwa_j_1bnfe_active-regio   "Região do emissor NF-e
                      lwa_j_1bnfe_active-nfyear  "Ano da data do documento da NF-e
                      lwa_j_1bnfe_active-nfmonth "Mês da data do documento da NF-e
                      lwa_j_1bnfe_active-stcd1   "Nº CNPJ do emissor da NF-e
                      lwa_j_1bnfe_active-model   "Modelo da nota fiscal
                      lwa_j_1bnfe_active-serie   "SERIE
                      lwa_j_1bnfe_active-nfnum9  "Nº NF-e de nove posições
                      lwa_j_1bnfe_active-docnum9 "NF-e: nº aleatório
                      lwa_j_1bnfe_active-cdv     "Dígito controle p/chave de acesso NF-e
       INTO lva_chave.

          gwa_saida-chv_nota_doc = lva_chave.
          CLEAR: lva_chave.
        ENDIF.

        gwa_saida-docdat_doc = lwa_j_1bnfdoc-docdat.
      ENDIF.


    ENDIF.


    gwa_saida-quantidade    = gwa_zlest0041-quantidade.
    gwa_saida-docnum_doc    = lwa_j_1bnfdoc-docnum.


    READ TABLE git_j_1bnfdoc_41 INTO DATA(lwa_j_1bnfdoc_41) WITH KEY docnum = gwa_zlest0041-docnum.

    IF sy-subrc = 0.
      READ TABLE git_j_1bnfe_active_41 INTO DATA(lwa_j_1bnfe_active_41) WITH KEY docnum = lwa_j_1bnfdoc_41-docnum.

      IF sy-subrc = 0.
        CLEAR: lva_chave.
        CONCATENATE lwa_j_1bnfe_active_41-regio   "Região do emissor NF-e
                    lwa_j_1bnfe_active_41-nfyear  "Ano da data do documento da NF-e
                    lwa_j_1bnfe_active_41-nfmonth "Mês da data do documento da NF-e
                    lwa_j_1bnfe_active_41-stcd1   "Nº CNPJ do emissor da NF-e
                    lwa_j_1bnfe_active_41-model   "Modelo da nota fiscal
                    lwa_j_1bnfe_active_41-serie   "SERIE
                    lwa_j_1bnfe_active_41-nfnum9  "Nº NF-e de nove posições
                    lwa_j_1bnfe_active_41-docnum9 "NF-e: nº aleatório
                    lwa_j_1bnfe_active_41-cdv     "Dígito controle p/chave de acesso NF-e
     INTO lva_chave.

        gwa_saida-chv_nota_41 = lva_chave.

      ENDIF.

      gwa_saida-docdat_41    = lwa_j_1bnfdoc_41-docdat.
    ENDIF.

    gwa_saida-nr_nf_propria = gwa_zlest0041-nr_nf_propria.
    gwa_saida-doc_num_41   = gwa_zlest0041-docnum.
    gwa_saida-cod_material = gwa_zlest0041-cod_material.


    APPEND gwa_saida TO git_saida.

    CLEAR:gwa_saida,
          lwa_lfa1,
          lwa_j_1bnfdoc,
          lwa_j_1bnfdoc_41,
          lwa_j_1bnfe_active,
          lwa_j_1bnfe_active_41.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
FORM fm_end_of_selection .
  PERFORM fm_filtros.
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
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
*      ( parametro = '' valor = pbukrs )
*    ).
*  ENDLOOP.

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
  DATA: isave TYPE char01 VALUE 'A'.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Notas de terceiros X Notas Próprias'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( )"parametro = 'Empresa' valor = lwa_bukrs )
     ( parametro = '' ) )
     CHANGING
       alv = gob_gui_alv_grid )
     EQ abap_true.


    PERFORM fm_cria_fieldcat.

*    PERFORM exclude_tb_functions CHANGING git_exclude_fcode.
    gs_layout-sel_mode   = 'A'.
    gs_layout-cwidth_opt = 'X'.
    CLEAR: git_exclude_fcode, git_exclude_fcode[].

    gs_variant = VALUE #( report = sy-repid ).

    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = gs_variant
        is_layout                     = gs_layout
        it_toolbar_excluding          = git_exclude_fcode
        i_save                        = 'A'
      CHANGING
        it_fieldcatalog               = git_fcat
        it_outtab                     = git_saida
        it_sort                       = git_sort
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CALL METHOD gob_gui_alv_grid->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ELSE.
    CALL METHOD gob_gui_alv_grid->refresh_table_display
      EXPORTING
        is_stable = ls_stable
      EXCEPTIONS
        finished  = 1
        OTHERS    = 2.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
FORM fm_cria_fieldcat.

  git_fcat =  VALUE lvc_t_fcat(
    ( col_pos = 1   fieldname = 'CENTRO_COMPRADOR'   coltext =  'Cód.Filial'                emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'ZLEST0041       '    ref_field = 'CENTRO_COMPRADOR'     outputlen = '10' )
    ( col_pos = 2   fieldname = 'COD_CLIENTE'        coltext =  'Cód.Forn'                  emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'ZLEST0041       '    ref_field = 'COD_CLIENTE'          outputlen = '10' )
    ( col_pos = 3   fieldname = 'NAME1'              coltext =  'Nome Fornec.'              emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'LFA1            '    ref_field = 'NAME1'                outputlen = '25' )
    ( col_pos = 4   fieldname = 'NR_NF'              coltext =  'Nr. NF Fornec.'            emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'ZLEST0041       '    ref_field = 'NR_NF'                outputlen = '15' no_zero = 'X' )
    ( col_pos = 5   fieldname = 'CHV_NOTA_DOC'       coltext =  'Chave NFe Fornecedor'      emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'ZIB_NFE_DIST_TER'    ref_field = 'CHAVE_NFE'            outputlen = '30' )
    ( col_pos = 6   fieldname = 'DOCDAT_DOC'         coltext =  'Data emissão NF Fornec.'   emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'J_LBNFDOC       '    ref_field = 'DOCDAT'               outputlen = '30' )
    ( col_pos = 7   fieldname = 'QUANTIDADE'         coltext =  'Quantidade NF Forn.'       emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'ZLEST0041       '    ref_field = 'QUANTIDADE'           outputlen = '20' )
    ( col_pos = 8   fieldname = 'DOCNUM_DOC'         coltext =  'Docnum Fornec.'            emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'J_LBNFDOC       '    ref_field = 'DOCNUM'               outputlen = '15' )
    ( col_pos = 9   fieldname = 'NR_NF_PROPRIA'      coltext =  'Nr. NF RFL'                emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'ZLEST0041       '    ref_field = 'NR_NF_PROPRIA'        outputlen = '10'  no_zero = 'X')
    ( col_pos = 10  fieldname = 'CHV_NOTA_41'        coltext =  'Chave NFe RFL'             emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'ZIB_NFE_DIST_TER'    ref_field = 'CHAVE_NFE'            outputlen = '30' )
    ( col_pos = 11  fieldname = 'DOCDAT_41'          coltext =  'Data emissão RFL'          emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'J_LBNFDOC       '    ref_field = 'DOCDAT'               outputlen = '15' )
    ( col_pos = 12  fieldname = 'DOC_NUM_41'         coltext =  'Docnum RFL'                emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'ZLEST0041       '    ref_field = 'DOCNUM'               outputlen = '15' )
    ( col_pos = 13  fieldname = 'COD_MATERIAL'       coltext =  'Cód. Material'             emphasize = ''  tabname = 'GIT_SAIDA'   ref_table = 'ZLEST0041       '    ref_field = 'COD_MATERIAL'         outputlen = '25' )
      ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  FM_STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fm_status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100' WITH 'Notas de terceiros X Notas Próprias'.
  PERFORM fm_criar_objetos.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FM_USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fm_user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  FM_USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE fm_user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_EXCLUDE  text
*----------------------------------------------------------------------*
FORM exclude_tb_functions CHANGING pt_exclude TYPE ui_functions.
  DATA ls_exclude TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO pt_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO pt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO pt_exclude.
ENDFORM.
