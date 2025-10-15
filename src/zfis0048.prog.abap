*--------------------------------------------------------------------*
*                         Consultoria                                *
*--------------------------------------------------------------------*
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 03.09.2024                                              *
* Descrição: Cockpit de Frete de Subcontratação                      *
* Report   : ZFIS0048                                                *
*--------------------------------------------------------------------*
* Projeto  : CS2022001028
**********************************************************************
REPORT zfis0048.

**********************************************************************
* tabelas
**********************************************************************
TABLES: zib_cte_dist_ter.

**********************************************************************
* Types
**********************************************************************
TYPES: BEGIN OF ty_zib_cte_dist_ter.
         INCLUDE TYPE zib_cte_dist_ter.
TYPES:   tabix TYPE sy-tabix.
TYPES: END OF ty_zib_cte_dist_ter.

TYPES: BEGIN OF ty_saida_normal,
         status           TYPE char20,
         chave_cte_origem TYPE zib_cte_dist_ter-cd_chave_cte,
         emp_cte_orig     TYPE zib_cte_dist_ter-e_emissor, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         filial_cte_orig  TYPE zib_cte_dist_ter-f_emissor, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         dt_cte_orig      TYPE zib_cte_dist_ter-dt_emissao,
         nr_cte_orig      TYPE zib_cte_dist_ter-numr_cte,
         serie_cte_orig   TYPE zib_cte_dist_ter-numr_serie,
         vlr_cte_orig     TYPE zib_cte_dist_ter-valor_prestacao,
         qt_cte_origem    TYPE zib_cte_dist_ter-qt_carga_cte,
         cod_prod_transp  TYPE zcte_info_nota-material,
         desc_prod_transp TYPE makt-maktx,
         docnum_entrada   TYPE zib_cte_dist_ter-docnum_cte,
         docnum_origem    TYPE zib_cte_dist_ter-docnum_cte_p,
         doc_transp       TYPE zcte_ciot-tknum,
         doc_custo        TYPE vfkp-fknum,
         cond_zfre        TYPE zib_cte_dist_ter-zvlr_frete,
         perc_dif         TYPE zib_cte_dist_ter-zvlr_frete,
         chave_cte_sub    TYPE zib_cte_dist_ter-cd_chave_cte,
         nr_cte_sub       TYPE zib_cte_dist_ter-numr_cte,
         serie_cte_sub    TYPE zib_cte_dist_ter-numr_serie,
         cnpj_fornec_sub  TYPE char20, "zib_cte_dist_ter-emit_cnpj,
         fornec_sub       TYPE zib_cte_dist_ter-p_emissor,
         vlr_cte_sub      TYPE zib_cte_dist_ter-valor_prestacao,
         qt_cte_sub       TYPE zib_cte_dist_ter-qt_carga_cte,
         prod_pred_sub    TYPE zcte_info_nota-material,
         docnum_sub       TYPE zib_cte_dist_ter-docnum_cte_subcont,
         emp_sub          TYPE j_1bnfdoc-bukrs, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         filial_sub       TYPE j_1bnfdoc-branch. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
TYPES: END   OF ty_saida_normal.

TYPES: BEGIN OF ty_saida_subcon,
         status           TYPE char25,
         exibe_log        TYPE char05,
         chave_cte_sub    TYPE zib_cte_dist_ter-cd_chave_cte,
         nr_cte_sub       TYPE zib_cte_dist_ter-numr_cte,
         serie_cte_sub    TYPE zib_cte_dist_ter-numr_serie,
         emp_sub          TYPE j_1bnfdoc-bukrs, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         filial_sub       TYPE j_1bnfdoc-branch, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         cnpj_fornec_sub  TYPE char20, "zib_cte_dist_ter-emit_cnpj,
         fornec_sub       TYPE zib_cte_dist_ter-p_emissor,
         vlr_cte_sub      TYPE zib_cte_dist_ter-valor_prestacao,
         qt_cte_sub       TYPE zib_cte_dist_ter-qt_carga_cte,
         prod_pred_sub    TYPE zcte_info_nota-material,
         docnum_sub       TYPE zib_cte_dist_ter-docnum_cte_subcont,
         chave_cte_origem TYPE zib_cte_dist_ter-cd_chave_cte,
         nr_cte_orig      TYPE zib_cte_dist_ter-numr_cte,
         serie_cte_orig   TYPE zib_cte_dist_ter-numr_serie,
         vlr_cte_orig     TYPE zib_cte_dist_ter-valor_prestacao,
         qt_cte_orig      TYPE zib_cte_dist_ter-qt_carga_cte,
         cod_prod_transp  TYPE zcte_info_nota-material,
         desc_prod_transp TYPE makt-maktx,
         docnum_origem    TYPE zib_cte_dist_ter-docnum_cte,
         emp_cte_orig     TYPE zib_cte_dist_ter-e_emissor, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         filial_cte_orig  TYPE zib_cte_dist_ter-f_emissor, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
         doc_transp       TYPE zcte_ciot-tknum,
         doc_custo        TYPE vfkp-fknum,
         cond_zfre        TYPE zib_cte_dist_ter-zvlr_frete,
         perc_dif         TYPE zib_cte_dist_ter-zvlr_frete.
TYPES: END   OF ty_saida_subcon.

TYPES: BEGIN OF ty_doctos,
         chave_cte TYPE zib_cte_dist_ter-cd_chave_cte,
         docnum    TYPE zib_cte_dist_ter-docnum_cte.
TYPES: END   OF ty_doctos.

TYPES: BEGIN OF ty_total_normal,
         docnum_subcont TYPE zib_cte_dist_ter-docnum_cte_subcont,
         valor_normal   TYPE zib_cte_dist_ter-valor_prestacao.
TYPES: END   OF ty_total_normal.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END   OF ty_estrutura.

**********************************************************************
* Variaveis
**********************************************************************
DATA: t_zib_cte_dist_ter   TYPE TABLE OF ty_zib_cte_dist_ter,
      t_zib_cte_aux        TYPE TABLE OF ty_zib_cte_dist_ter,
      t_zib_cte_normal     TYPE TABLE OF ty_zib_cte_dist_ter,
      t_J_1BNFDOC          TYPE TABLE OF j_1bnfdoc, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
      t_zib_cte_normal_aux TYPE TABLE OF ty_zib_cte_dist_ter,
      t_zib_cte_subcon     TYPE TABLE OF ty_zib_cte_dist_ter,
      t_zcte_ciot          TYPE TABLE OF zcte_ciot,
      t_zcte_info_nota     TYPE TABLE OF zcte_info_nota,
      t_total_normal       TYPE TABLE OF ty_total_normal,
      t_vfkp               TYPE TABLE OF vfkp,
      t_makt               TYPE TABLE OF makt,
      t_saida_normal       TYPE TABLE OF ty_saida_normal,
      t_saida_subcon       TYPE TABLE OF ty_saida_subcon,
      t_doctos             TYPE TABLE OF ty_doctos,
      t_value              TYPE TABLE OF rgsb4,
      t_zlest0246          TYPE TABLE OF zlest0246,
      w_zib_cte_dist_ter   TYPE ty_zib_cte_dist_ter,
      w_zib_cte_subcon     TYPE ty_zib_cte_dist_ter,
      w_j_1bnfdoc          TYPE j_1bnfdoc, "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
      w_zib_cte_normal     TYPE ty_zib_cte_dist_ter,
      w_zib_cte_normal_aux TYPE ty_zib_cte_dist_ter,
      w_value              TYPE rgsb4,
      w_total_normal       TYPE ty_total_normal,
      w_saida_normal       TYPE ty_saida_normal,
      w_saida_subcon       TYPE ty_saida_subcon,
      w_zcte_ciot          TYPE zcte_ciot,
      w_zcte_info_nota     TYPE zcte_info_nota,
      w_vfkp               TYPE vfkp,
      w_makt               TYPE makt,
      w_doctos             TYPE ty_doctos,
      w_zlest0246          TYPE zlest0246,
      l_docnum_new         TYPE j_1bdocnum,
      l_check_tolera       TYPE char6,
      l_tipo_servico       TYPE zib_cte_dist_ter-cd_tipo_servico,
      ok_code              TYPE sy-ucomm,
      zcl_util             TYPE REF TO zcl_util,
*
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      picture              TYPE REF TO cl_gui_picture,
      l_graphic_conv       TYPE i,
      l_graphic_offs       TYPE i,
      graphic_size         TYPE i,
      l_graphic_xstr       TYPE xstring,
      url(255)             TYPE c,
      graphic_url(255),
      t_function           TYPE ui_functions,
      w_function           TYPE ui_func,
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_sort               TYPE lvc_t_sort,
      w_sort               TYPE lvc_s_sort,
      t_color              TYPE lvc_t_scol,
      w_color              TYPE lvc_s_scol,
      t_ucomm              TYPE TABLE OF syst_ucomm,
      w_ucomm              TYPE syst_ucomm,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_variant            TYPE disvariant,
      w_stable             TYPE lvc_s_stbl    VALUE 'XX',
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
*
      w_header_nfe         TYPE j_1bnfe_active,
      it_fieldcat          TYPE TABLE OF ty_estrutura,
      wa_estrutura         TYPE ty_estrutura,
      wa_fieldcat          TYPE ty_estrutura,
      ls_variant           TYPE disvariant,
      l_grid_title         TYPE lvc_title,

*
      BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

**********************************************************************
* tela seleção
**********************************************************************
SELECTION-SCREEN BEGIN OF   BLOCK b1 WITH FRAME TITLE TEXT-002.
  PARAMETERS    : p_normal RADIOBUTTON GROUP g3 USER-COMMAND us1,
                  p_subcon RADIOBUTTON GROUP g3.
SELECTION-SCREEN END   OF   BLOCK b1.

SELECTION-SCREEN BEGIN OF   BLOCK b2 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs     FOR zib_cte_dist_ter-e_emissor,
                  s_filial    FOR zib_cte_dist_ter-f_emissor,
                  s_dtemi     FOR zib_cte_dist_ter-dt_emissao,
                  s_cnpj      FOR zib_cte_dist_ter-emit_cnpj    NO INTERVALS,
                  s_chave     FOR zib_cte_dist_ter-cd_chave_cte NO INTERVALS.
SELECTION-SCREEN END   OF   BLOCK b2.

SELECTION-SCREEN BEGIN OF   BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS    : p_layout   TYPE disvariant-variant.
SELECTION-SCREEN END OF     BLOCK b3.

SELECTION-SCREEN BEGIN OF   BLOCK b4 WITH FRAME TITLE TEXT-004.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS: p_st01 RADIOBUTTON GROUP gpst DEFAULT 'X' USER-COMMAND sel.
    SELECTION-SCREEN COMMENT 5(10) TEXT-401 FOR FIELD p_st01.
    SELECTION-SCREEN POSITION 20.
    PARAMETERS: p_st02 RADIOBUTTON GROUP gpst.
    SELECTION-SCREEN COMMENT 24(10) TEXT-402 FOR FIELD p_st02.
    SELECTION-SCREEN POSITION 40.
    PARAMETERS: p_st03 RADIOBUTTON GROUP gpst.
    SELECTION-SCREEN COMMENT 44(10) TEXT-403 FOR FIELD p_st03.
    SELECTION-SCREEN POSITION 60.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF     BLOCK b4.

**********************************************************************
* p_st01 = Todos p_st02 Pendente p_st03 = Completo
**********************************************************************

**********************************************************************
* classes
**********************************************************************
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no,

      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      toolbar               FOR EVENT toolbar OF  cl_gui_alv_grid
        IMPORTING e_object.
ENDCLASS.

**********************************************************************
* classes implementacao
**********************************************************************
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
  ENDMETHOD.

  METHOD on_hotspot_click.

    DATA: l_docnum TYPE j_1bnfdoc-docnum.

    CASE abap_true.
      WHEN p_normal.
        READ TABLE t_saida_normal INTO w_saida_normal INDEX e_row_id-index.

        CASE e_column_id.
          WHEN 'CHAVE_CTE_ORIGEM'.
            DATA(t_documentos) = zcl_util->get_docnum( w_saida_normal-chave_cte_origem ).
            DELETE t_documentos WHERE form = abap_off.
            READ TABLE t_documentos INTO DATA(w_documentos) INDEX 1.
            IF sy-subrc = 0.
              l_docnum = w_documentos-docnum.
            ENDIF.

          WHEN 'CHAVE_CTE_SUB'.
            t_documentos = zcl_util->get_docnum( w_saida_normal-chave_cte_sub ).
            READ TABLE t_documentos INTO w_documentos INDEX 1.
            IF sy-subrc = 0.
              l_docnum = w_documentos-docnum.
            ENDIF.

          WHEN 'DOCNUM_ENTRADA'.
            l_docnum = w_saida_normal-docnum_entrada.

          WHEN 'DOCNUM_ORIGEM'.
            l_docnum = w_saida_normal-docnum_origem.

          WHEN 'DOCNUM_SUB'.
            l_docnum = w_saida_normal-docnum_sub.

          WHEN 'DOC_TRANSP'.
            CHECK w_saida_normal-doc_transp IS NOT INITIAL.
            SET PARAMETER ID 'TNR' FIELD w_saida_normal-doc_transp.
            CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.

          WHEN 'DOC_CUSTO'.
            CHECK w_saida_normal-doc_custo IS NOT INITIAL.
            SET PARAMETER ID 'FKK'  FIELD w_saida_normal-doc_custo.
            CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.

        ENDCASE.

        CHECK l_docnum IS NOT INITIAL.

        SET PARAMETER ID 'JEF'  FIELD l_docnum.
        CALL TRANSACTION 'J1B3N'  AND SKIP FIRST SCREEN.

      WHEN p_subcon.
        READ TABLE t_saida_subcon INTO w_saida_subcon INDEX e_row_id-index.

        CASE e_column_id.
          WHEN 'CHAVE_CTE_ORIGEM'.
            t_documentos = zcl_util->get_docnum( w_saida_subcon-chave_cte_origem ).
            DELETE t_documentos WHERE form = abap_off.
            READ TABLE t_documentos INTO w_documentos INDEX 1.
            IF sy-subrc = 0.
              l_docnum = w_documentos-docnum.
            ENDIF.

          WHEN 'CHAVE_CTE_SUB'.
            t_documentos = zcl_util->get_docnum( w_saida_subcon-chave_cte_sub ).
            READ TABLE t_documentos INTO w_documentos INDEX 1.
            IF sy-subrc = 0.
              l_docnum = w_documentos-docnum.
            ENDIF.

          WHEN 'DOCNUM_ORIGEM'.
            l_docnum = w_saida_subcon-docnum_origem.

          WHEN 'DOCNUM_SUB'.
            l_docnum = w_saida_subcon-docnum_sub.

          WHEN 'DOC_TRANSP'.
            CHECK w_saida_subcon-doc_transp IS NOT INITIAL.
            SET PARAMETER ID 'TNR' FIELD w_saida_subcon-doc_transp.
            CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.

          WHEN 'DOC_CUSTO'.
            CHECK w_saida_subcon-doc_custo IS NOT INITIAL.
            SET PARAMETER ID 'FKK'  FIELD w_saida_subcon-doc_custo.
            CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.

          WHEN 'EXIBE_LOG'.
            PERFORM f_exibir_log USING w_saida_subcon-chave_cte_sub.

        ENDCASE.

        CHECK l_docnum IS NOT INITIAL.

        SET PARAMETER ID 'JEF'  FIELD l_docnum.
        CALL TRANSACTION 'J1B3N'  AND SKIP FIRST SCREEN.

    ENDCASE.

  ENDMETHOD.

  METHOD data_changed_finished.
  ENDMETHOD.

  METHOD user_command.
  ENDMETHOD.

  METHOD toolbar.
  ENDMETHOD.

ENDCLASS.

**********************************************************************
* SELECTION-SCREEN OUTPUT
**********************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'S_DTEMI-LOW'.
      screen-required = 2.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

**********************************************************************
* SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
  PERFORM f_alv_variant_f4 CHANGING p_layout.

**********************************************************************
* start
**********************************************************************
START-OF-SELECTION.

  IF s_dtemi[] IS INITIAL.
    MESSAGE s024(sd) WITH TEXT-100 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  "PERFORM f_selecao_dados_geral.(Inutilizado!) 170396 CS2022001028 Questão Legal parte 3 PSA
  "********************************************************************** Inicio 170396 CS2022001028 Questão Legal parte 3 PSA
  FREE: t_saida_normal, t_saida_subcon, t_doctos, t_total_normal.

  CREATE OBJECT zcl_util.

*-------------------------
* tolerancia para gerar registro fiscal
*-------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'ZLESR0181_TOLERANCIA'
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  READ TABLE t_value INTO w_value INDEX 1.

  l_check_tolera = COND #( WHEN sy-subrc = 0 THEN w_value-from
                                             ELSE 0 ).


  CASE abap_true.
    WHEN p_normal.
      PERFORM f_selecao_dados_geral.

      IF t_zib_cte_dist_ter[] IS INITIAL.
        MESSAGE s024(sd) WITH TEXT-101 DISPLAY LIKE 'E'.
        STOP.
      ELSE.

        PERFORM f_selecao_dados_normal.
        PERFORM f_processa_normal.
      ENDIF.

    WHEN p_subcon.
      PERFORM f_selecao_dados_geral.

      IF t_zib_cte_dist_ter[] IS INITIAL.
        MESSAGE s024(sd) WITH TEXT-101 DISPLAY LIKE 'E'.
        STOP.
      ELSE.
        PERFORM f_selecao_dados_subcontratado.
        PERFORM f_processa_subcontratado.
      ENDIF.

  ENDCASE.
  "********************************************************************** Fim 170396 CS2022001028 Questão Legal parte 3 PSA
  CALL SCREEN 0100.

**********************************************************************
* selecao dados geral
**********************************************************************
FORM f_selecao_dados_geral.

  DATA: lv_where TYPE string.
  CLEAR: lv_where.

  lv_where = | cd_chave_cte IN @s_chave AND emit_cnpj IN @s_cnpj AND dt_emissao IN @s_dtemi AND e_emissor IN @s_bukrs AND f_emissor IN @s_filial AND cd_modal = '01' AND docsta = '1' |.

  CASE abap_true.
    WHEN p_normal.

      IF p_st01 = abap_true. "todos
        lv_where = |{ lv_where } and cd_tipo_servico =  '0' AND E_EMISSOR IS NOT INITIAL AND F_EMISSOR IS NOT INITIAL |.
      ELSEIF p_st02 = abap_true. "Pendente
        lv_where = |{ lv_where } and cd_tipo_servico =  '0' AND E_EMISSOR IS NOT INITIAL AND F_EMISSOR IS NOT INITIAL and docnum_cte <> '0000000000' and docnum_cte_subcont = '0000000000'|.
      ELSEIF p_st03 = abap_true. "Completo
        lv_where = |{ lv_where } and cd_tipo_servico =  '0' AND E_EMISSOR IS NOT INITIAL AND F_EMISSOR IS NOT INITIAL and docnum_cte <> '0000000000' and docnum_cte_subcont <>'0000000000'|.
      ENDIF.

      SELECT *
    FROM zib_cte_dist_ter
   WHERE (lv_where)
      INTO TABLE @t_zib_cte_dist_ter.

    WHEN p_subcon.

      IF p_st01 = abap_true. "todos
        lv_where = |{ lv_where } and cd_tipo_servico =  '1' |.
      ELSEIF p_st02 = abap_true. "Pendente
        lv_where = |{ lv_where } and cd_tipo_servico =  '1' and docnum_cte = '0000000000'|.
      ELSEIF       p_st03 = abap_true. "Completo.
        lv_where = |{ lv_where } and cd_tipo_servico =  '1' and docnum_cte <> '0000000000'|.
      ENDIF.

      SELECT *
FROM zib_cte_dist_ter
WHERE (lv_where)
      INTO TABLE @t_zib_cte_dist_ter.

  ENDCASE.

*  FREE: t_saida_normal, t_saida_subcon, t_doctos, t_total_normal.
*
*  CREATE OBJECT zcl_util.
*
**-------------------------
** tolerancia para gerar registro fiscal
**-------------------------
*  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*    EXPORTING
*      setnr           = 'ZLESR0181_TOLERANCIA'
*      class           = '0000'
*      no_descriptions = ''
*    TABLES
*      set_values      = t_value
*    EXCEPTIONS
*      set_not_found   = 1
*      OTHERS          = 2.
*
*  READ TABLE t_value INTO w_value INDEX 1.
*
*  l_check_tolera = COND #( WHEN sy-subrc = 0 THEN w_value-from
*                                             ELSE 0 ).
*
**-------------------------
** selecao
**-------------------------
*
*  l_tipo_servico = COND #( WHEN p_normal = abap_true THEN '0'
*                                                     ELSE '1' ).
*
*  SELECT *
*    FROM zib_cte_dist_ter
*   WHERE cd_chave_cte   IN @s_chave
*     AND emit_cnpj      IN @s_cnpj
*     AND dt_emissao     IN @s_dtemi
*     AND e_emissor      IN @s_bukrs
*     AND f_emissor      IN @s_filial
*     AND cd_tipo_servico = @l_tipo_servico
*     AND cd_modal        = '01'   "Rodoviário
*     AND docsta          = '1'    "Autorizado
*    INTO TABLE @t_zib_cte_dist_ter.

ENDFORM.

**********************************************************************
* selecao dados CTe normal
**********************************************************************
FORM f_selecao_dados_normal.

  t_zib_cte_aux[] = t_zib_cte_dist_ter[].

  DELETE t_zib_cte_aux WHERE docnum_cte_subcont IS INITIAL.

  IF t_zib_cte_aux[] IS NOT INITIAL.
    SELECT *
      FROM zib_cte_dist_ter
      INTO TABLE t_zib_cte_subcon
       FOR ALL ENTRIES IN t_zib_cte_aux
    WHERE docnum_cte = t_zib_cte_aux-docnum_cte_subcont.
  ENDIF.

  IF t_zib_cte_subcon[] IS NOT INITIAL.
    SELECT *
      FROM zib_cte_dist_ter
      INTO TABLE t_zib_cte_normal
       FOR ALL ENTRIES IN t_zib_cte_subcon
    WHERE docnum_cte_subcont = t_zib_cte_subcon-docnum_cte.

    SELECT *      "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
    FROM j_1bnfdoc
    INTO TABLE t_J_1BNFDOC
     FOR ALL ENTRIES IN t_zib_cte_subcon
    WHERE docnum = t_zib_cte_subcon-docnum_cte .
  ENDIF.

  LOOP AT t_zib_cte_normal     INTO w_zib_cte_normal.
    w_total_normal-docnum_subcont = w_zib_cte_normal-docnum_cte_subcont.
    w_total_normal-valor_normal   = w_zib_cte_normal-valor_prestacao.
    COLLECT w_total_normal     INTO t_total_normal.
  ENDLOOP.

  LOOP AT t_zib_cte_dist_ter INTO w_zib_cte_dist_ter.
    DATA(t_documentos)  = zcl_util->get_docnum( w_zib_cte_dist_ter-cd_chave_cte ).
    DELETE t_documentos WHERE form = abap_off.

    READ TABLE t_documentos  INTO DATA(w_documentos) INDEX 1.
    IF sy-subrc = 0.
      w_doctos-chave_cte = w_zib_cte_dist_ter-cd_chave_cte.
      w_doctos-docnum    = w_documentos-docnum.
      APPEND w_doctos   TO t_doctos.
    ENDIF.
  ENDLOOP.

  IF t_doctos[] IS NOT INITIAL.
    SELECT *
      FROM zcte_ciot
      INTO TABLE t_zcte_ciot
       FOR ALL ENTRIES IN t_doctos
    WHERE docnum = t_doctos-docnum.

    SELECT *
      FROM zcte_info_nota
      INTO TABLE t_zcte_info_nota
       FOR ALL ENTRIES IN t_doctos
    WHERE docnum = t_doctos-docnum.

  ENDIF.

  IF t_zcte_ciot[] IS NOT INITIAL.
    SELECT *
      FROM vfkp
      INTO TABLE t_vfkp
       FOR ALL ENTRIES IN t_zcte_ciot
     WHERE rebel = t_zcte_ciot-tknum
    AND fkpos = '000001'.
  ENDIF.

  IF t_zcte_info_nota[] IS NOT INITIAL.
    SELECT *
      FROM makt
      INTO TABLE t_makt
       FOR ALL ENTRIES IN t_zcte_info_nota
     WHERE matnr = t_zcte_info_nota-material
    AND spras = sy-langu.
  ENDIF.

ENDFORM.

**********************************************************************
* selecao dados CTe subcontratado
**********************************************************************
FORM f_selecao_dados_subcontratado.

  t_zib_cte_aux[] = t_zib_cte_dist_ter[].

  DELETE t_zib_cte_aux WHERE docnum_cte IS INITIAL.

  IF t_zib_cte_aux[] IS NOT INITIAL.
    SELECT *
      FROM zib_cte_dist_ter
      INTO TABLE t_zib_cte_normal
       FOR ALL ENTRIES IN t_zib_cte_aux
    WHERE docnum_cte_subcont = t_zib_cte_aux-docnum_cte.

    SELECT *      "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
FROM j_1bnfdoc
INTO TABLE t_J_1BNFDOC
FOR ALL ENTRIES IN t_zib_cte_normal
    WHERE docnum = t_zib_cte_normal-docnum_cte .
  ENDIF.

  SELECT *
    FROM zlest0246
    INTO TABLE t_zlest0246
     FOR ALL ENTRIES IN t_zib_cte_dist_ter
  WHERE cd_chave_cte = t_zib_cte_dist_ter-cd_chave_cte.

* IF t_zib_cte_normal[] IS NOT INITIAL.
*   SELECT *
*     FROM zib_cte_dist_ter
*     INTO TABLE t_zib_cte_subcon
*      FOR ALL ENTRIES IN t_zib_cte_normal
*    WHERE docnum_cte = t_zib_cte_normal-docnum_cte_subcont.
* ENDIF.

  LOOP AT t_zib_cte_normal     INTO w_zib_cte_normal.
    w_total_normal-docnum_subcont = w_zib_cte_normal-docnum_cte_subcont.
    w_total_normal-valor_normal   = w_zib_cte_normal-valor_prestacao.
    COLLECT w_total_normal     INTO t_total_normal.
  ENDLOOP.

  LOOP AT t_zib_cte_normal     INTO w_zib_cte_normal.
    DATA(t_documentos)  = zcl_util->get_docnum( w_zib_cte_normal-cd_chave_cte ).
    DELETE t_documentos WHERE form = abap_off.

    READ TABLE t_documentos  INTO DATA(w_documentos) INDEX 1.
    IF sy-subrc = 0.
      w_doctos-chave_cte = w_zib_cte_normal-cd_chave_cte.
      w_doctos-docnum    = w_documentos-docnum.
      APPEND w_doctos   TO t_doctos.
    ENDIF.
  ENDLOOP.

  IF t_doctos[] IS NOT INITIAL.
    SELECT *
      FROM zcte_ciot
      INTO TABLE t_zcte_ciot
       FOR ALL ENTRIES IN t_doctos
    WHERE docnum = t_doctos-docnum.

    SELECT *
      FROM zcte_info_nota
      INTO TABLE t_zcte_info_nota
       FOR ALL ENTRIES IN t_doctos
    WHERE docnum = t_doctos-docnum.
  ENDIF.

  IF t_zcte_ciot[] IS NOT INITIAL.
    SELECT *
      FROM vfkp
      INTO TABLE t_vfkp
       FOR ALL ENTRIES IN t_zcte_ciot
     WHERE rebel = t_zcte_ciot-tknum
    AND fkpos = '000001'.
  ENDIF.

  IF t_zcte_info_nota[] IS NOT INITIAL.
    SELECT *
      FROM makt
      INTO TABLE t_makt
       FOR ALL ENTRIES IN t_zcte_info_nota
     WHERE matnr = t_zcte_info_nota-material
    AND spras = sy-langu.
  ENDIF.

ENDFORM.

**********************************************************************
* processamento
**********************************************************************
FORM f_processa_normal.

  LOOP AT t_zib_cte_dist_ter      INTO w_zib_cte_dist_ter.
    CLEAR: w_saida_normal, w_doctos, w_zcte_ciot, w_total_normal, w_zcte_info_nota, w_vfkp, w_makt, w_zib_cte_subcon, w_j_1bnfdoc.

    READ TABLE t_doctos           INTO w_doctos         WITH KEY chave_cte  = w_zib_cte_dist_ter-cd_chave_cte.
    READ TABLE t_zcte_ciot        INTO w_zcte_ciot      WITH KEY docnum     = w_doctos-docnum.
    READ TABLE t_zcte_info_nota   INTO w_zcte_info_nota WITH KEY docnum     = w_doctos-docnum.
    READ TABLE t_vfkp             INTO w_vfkp           WITH KEY rebel      = w_zcte_ciot-tknum.
    READ TABLE t_makt             INTO w_makt           WITH KEY matnr      = w_zcte_info_nota-material.
    READ TABLE t_zib_cte_subcon   INTO w_zib_cte_subcon WITH KEY docnum_cte = w_zib_cte_dist_ter-docnum_cte_subcont.
    READ TABLE t_j_1bnfdoc        INTO w_j_1bnfdoc  WITH KEY docnum = w_zib_cte_dist_ter-docnum_cte_subcont. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA


    w_saida_normal-chave_cte_origem  = w_zib_cte_dist_ter-cd_chave_cte.
    w_saida_normal-dt_cte_orig       = w_zib_cte_dist_ter-dt_emissao.
    w_saida_normal-nr_cte_orig       = w_zib_cte_dist_ter-numr_cte.
    w_saida_normal-serie_cte_orig    = w_zib_cte_dist_ter-numr_serie.
    w_saida_normal-vlr_cte_orig      = w_zib_cte_dist_ter-valor_prestacao.
    w_saida_normal-qt_cte_origem     = w_zib_cte_dist_ter-qt_carga_cte.
    w_saida_normal-cod_prod_transp   = w_zcte_info_nota-material.
    w_saida_normal-desc_prod_transp  = w_makt-maktx.
    w_saida_normal-docnum_entrada    = w_zib_cte_dist_ter-docnum_cte.
    w_saida_normal-docnum_origem     = w_zib_cte_dist_ter-docnum_cte_p.
    w_saida_normal-doc_transp        = w_zcte_ciot-tknum.
    w_saida_normal-doc_custo         = w_vfkp-fknum.
    "w_saida_normal-cond_zfre         = w_zib_cte_dist_ter-zvlr_frete."170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
    SELECT SINGLE kwert FROM prcd_elements WHERE knumv = @w_vfkp-knumv  AND kposn  = @w_vfkp-fkpos AND kappl   = 'F' AND kschl   = 'ZFRE' INTO @w_saida_normal-cond_zfre. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
    w_saida_normal-chave_cte_sub     = zcl_util->get_chave_nfe( w_zib_cte_dist_ter-docnum_cte_subcont ).
    w_saida_normal-nr_cte_sub        = w_zib_cte_subcon-numr_cte.
    w_saida_normal-serie_cte_sub     = w_zib_cte_subcon-numr_serie.

    w_saida_normal-emp_cte_orig       = w_zib_cte_dist_ter-e_emissor."170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
    w_saida_normal-filial_cte_orig    = w_zib_cte_dist_ter-f_emissor."170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
    w_saida_normal-emp_sub            = w_j_1bnfdoc-bukrs."170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
    w_saida_normal-filial_sub         = w_j_1bnfdoc-branch."170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA

    IF w_zib_cte_subcon-emit_cnpj IS NOT INITIAL.
      WRITE w_zib_cte_subcon-emit_cnpj USING EDIT MASK  'RR__.___.___/____-__'  TO w_saida_normal-cnpj_fornec_sub.
*     w_saida_normal-cnpj_fornec_sub   = w_zib_cte_subcon-emit_cnpj.
    ENDIF.

    w_saida_normal-fornec_sub        = w_zib_cte_subcon-p_emissor.
    w_saida_normal-vlr_cte_sub       = w_zib_cte_subcon-valor_prestacao.
    w_saida_normal-qt_cte_sub        = w_zib_cte_subcon-qt_carga_cte.
    w_saida_normal-prod_pred_sub     = w_zib_cte_subcon-ds_prod_pred.
    w_saida_normal-docnum_sub        = w_zib_cte_dist_ter-docnum_cte_subcont.

    PERFORM f_calc_percentual    USING w_zib_cte_dist_ter
                                       w_zib_cte_subcon
                                       w_saida_subcon-cond_zfre "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
                              CHANGING w_saida_normal-perc_dif.

    PERFORM f_status_normal      USING w_zib_cte_dist_ter
                              CHANGING w_saida_normal-status.

    APPEND w_saida_normal           TO t_saida_normal.
  ENDLOOP.

ENDFORM.

**********************************************************************
* status
**********************************************************************
FORM f_status_normal    USING p_zib_dist_ter STRUCTURE zib_cte_dist_ter
                     CHANGING p_status.

  p_status = icon_dummy.

  IF     p_zib_dist_ter-docnum_cte IS NOT INITIAL AND p_zib_dist_ter-docnum_cte_subcont IS NOT INITIAL.
    p_status = icon_okay && ' Completo'.
  ELSEIF p_zib_dist_ter-docnum_cte IS NOT INITIAL AND p_zib_dist_ter-docnum_cte_subcont IS     INITIAL.
    p_status = icon_led_yellow && ' Pendente'.
  ENDIF.

ENDFORM.

**********************************************************************
* calcula percentual
**********************************************************************
FORM f_calc_percentual    USING p_zib_cte_normal STRUCTURE zib_cte_dist_ter
                                p_zib_cte_subcon STRUCTURE zib_cte_dist_ter
                                p_zfre TYPE kwert
                       CHANGING p_perc_var.

  DATA: l_menor_vlr  TYPE j_1bnftot,
        l_maior_vlr  TYPE j_1bnftot,
        l_tolerancia TYPE p DECIMALS 0.

  FREE: p_perc_var, w_total_normal, l_tolerancia.

  READ TABLE t_total_normal INTO w_total_normal  WITH KEY docnum_subcont = p_zib_cte_normal-docnum_cte_subcont.
  CHECK sy-subrc = 0.

  l_menor_vlr  = COND #( WHEN p_zfre <  p_zib_cte_subcon-valor_prestacao THEN p_zfre  "w_total_normal-valor_normal "w_total_normal-valor_normal "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
                                                                                              ELSE p_zib_cte_subcon-valor_prestacao ).
  l_maior_vlr  = COND #( WHEN p_zfre >= p_zib_cte_subcon-valor_prestacao THEN p_zfre "w_total_normal-valor_normal w_total_normal-valor_normal "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
                                                                                              ELSE p_zib_cte_subcon-valor_prestacao ).
  TRY.
      l_tolerancia = ( ( l_maior_vlr / l_menor_vlr ) - 1 ) * 100.
    CATCH cx_sy_zerodivide INTO DATA(l_zerodiv).
  ENDTRY.

  p_perc_var       = l_tolerancia.

ENDFORM.

**********************************************************************
* processamento
**********************************************************************
FORM f_processa_subcontratado.

  FREE: t_zib_cte_normal_aux.

  SORT t_zib_cte_normal        BY docnum_cte_subcont.

  LOOP AT t_zib_cte_normal   INTO w_zib_cte_normal.
    MOVE sy-tabix              TO w_zib_cte_normal-tabix.
    APPEND w_zib_cte_normal    TO t_zib_cte_normal_aux.
  ENDLOOP.

  SORT t_zib_cte_normal_aux    BY docnum_cte_subcont tabix.
  DELETE ADJACENT DUPLICATES FROM t_zib_cte_normal_aux
                        COMPARING docnum_cte_subcont.

  LOOP AT t_zib_cte_dist_ter        INTO w_zib_cte_dist_ter.
    CLEAR: w_saida_subcon, w_zib_cte_normal_aux, w_zib_cte_normal, w_zlest0246.

    READ TABLE t_zlest0246 INTO w_zlest0246 WITH KEY cd_chave_cte = w_zib_cte_dist_ter-cd_chave_cte.

    w_saida_subcon-exibe_log           = COND #( WHEN w_zlest0246-cd_chave_cte IS INITIAL THEN icon_dummy
                                                                                          ELSE icon_history ).
    w_saida_subcon-chave_cte_sub       = w_zib_cte_dist_ter-cd_chave_cte.
    w_saida_subcon-nr_cte_sub          = w_zib_cte_dist_ter-numr_cte.
    w_saida_subcon-serie_cte_sub       = w_zib_cte_dist_ter-numr_serie.

    IF w_zib_cte_dist_ter-emit_cnpj IS NOT INITIAL.
      WRITE w_zib_cte_dist_ter-emit_cnpj USING EDIT MASK  'RR__.___.___/____-__'  TO w_saida_subcon-cnpj_fornec_sub.
*     w_saida_subcon-cnpj_fornec_sub   = w_zib_cte_dist_ter-emit_cnpj.
    ENDIF.

    w_saida_subcon-fornec_sub          = w_zib_cte_dist_ter-p_emissor.
    w_saida_subcon-vlr_cte_sub         = w_zib_cte_dist_ter-valor_prestacao.
    w_saida_subcon-qt_cte_sub          = w_zib_cte_dist_ter-qt_carga_cte.
    w_saida_subcon-prod_pred_sub       = w_zib_cte_dist_ter-ds_prod_pred.
    w_saida_subcon-docnum_sub          = w_zib_cte_dist_ter-docnum_cte.

    READ TABLE t_zib_cte_normal_aux INTO w_zib_cte_normal_aux WITH KEY docnum_cte_subcont = w_zib_cte_dist_ter-docnum_cte
                                                              BINARY SEARCH.
    IF sy-subrc = 0.
      LOOP AT t_zib_cte_normal      INTO w_zib_cte_normal FROM w_zib_cte_normal_aux-tabix.
        IF w_zib_cte_normal-docnum_cte_subcont <> w_zib_cte_dist_ter-docnum_cte.
          EXIT.
        ENDIF.

        CLEAR: w_doctos, w_zcte_ciot, w_total_normal, w_zcte_info_nota, w_vfkp, w_makt, w_zib_cte_subcon,w_j_1bnfdoc.

        READ TABLE t_doctos         INTO w_doctos         WITH KEY chave_cte  = w_zib_cte_normal-cd_chave_cte.
        READ TABLE t_zcte_ciot      INTO w_zcte_ciot      WITH KEY docnum     = w_doctos-docnum.
        READ TABLE t_zcte_info_nota INTO w_zcte_info_nota WITH KEY docnum     = w_doctos-docnum.
        READ TABLE t_vfkp           INTO w_vfkp           WITH KEY rebel      = w_zcte_ciot-tknum.
        READ TABLE t_makt           INTO w_makt           WITH KEY matnr      = w_zcte_info_nota-material.
        READ TABLE t_j_1bnfdoc      INTO w_j_1bnfdoc      WITH KEY docnum = w_zib_cte_normal-docnum_cte. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA

        w_saida_subcon-chave_cte_origem  = w_zib_cte_normal-cd_chave_cte.
        w_saida_subcon-nr_cte_orig       = w_zib_cte_normal-numr_cte.
        w_saida_subcon-serie_cte_orig    = w_zib_cte_normal-numr_serie.
        w_saida_subcon-vlr_cte_orig      = w_zib_cte_normal-valor_prestacao.
        w_saida_subcon-qt_cte_orig       = w_zib_cte_normal-qt_carga_cte.
        w_saida_subcon-cod_prod_transp   = w_zcte_info_nota-material.
        w_saida_subcon-desc_prod_transp  = w_makt-maktx.
        w_saida_subcon-docnum_origem     = w_zib_cte_normal-docnum_cte.
        w_saida_subcon-doc_transp        = w_zcte_ciot-tknum.
        w_saida_subcon-doc_custo         = w_vfkp-fknum.
        w_saida_subcon-cond_zfre         = w_zib_cte_normal-zvlr_frete.

        w_saida_subcon-emp_cte_orig       = w_zib_cte_normal-e_emissor."170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
        w_saida_subcon-filial_cte_orig    = w_zib_cte_normal-f_emissor."170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
        w_saida_subcon-emp_sub            = w_j_1bnfdoc-bukrs."170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
        w_saida_subcon-filial_sub         = w_j_1bnfdoc-branch."170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA

        SELECT SINGLE kwert FROM prcd_elements WHERE knumv = @w_vfkp-knumv  AND kposn  = @w_vfkp-fkpos AND kappl   = 'F' AND kschl   = 'ZFRE' INTO @w_saida_subcon-cond_zfre. "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA


        PERFORM f_calc_percentual    USING w_zib_cte_normal
                                           w_zib_cte_dist_ter
                                           w_saida_subcon-cond_zfre "170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
                                  CHANGING w_saida_subcon-perc_dif.

        PERFORM f_status_subcontra   USING w_zib_cte_normal
                                           w_zib_cte_dist_ter
                                           w_saida_subcon-perc_dif
                                  CHANGING w_saida_subcon-status.

        APPEND w_saida_subcon           TO t_saida_subcon.
      ENDLOOP.
    ELSE.
      PERFORM f_status_subcontra     USING w_zib_cte_normal
                                           w_zib_cte_dist_ter
                                           0
                                  CHANGING w_saida_subcon-status.

      APPEND w_saida_subcon             TO t_saida_subcon.
    ENDIF.
  ENDLOOP.

ENDFORM.

**********************************************************************
* Estornar NF
**********************************************************************
FORM f_estorna_nf.

  DATA: w_saida    TYPE ty_saida_subcon,
        l_mensagem TYPE string.

  FREE: l_docnum_new.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ELSEIF lines( t_rows[] ) > 1.
    MESSAGE s024(sd) WITH 'Selecione apenas uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE t_rows INTO w_rows INDEX 1.

  w_saida = t_saida_subcon[ w_rows-index ].

  IF w_saida-docnum_sub IS INITIAL.
    MESSAGE s024(sd) WITH 'Estorno não pode ser concluído!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Processando...'.

*---------------------------------------------
* tabelas a estornar docnum
*---------------------------------------------
  SELECT *
    FROM zlest0032
    INTO TABLE @DATA(t_0032)
  WHERE docnum_sub = @w_saida-docnum_sub.
  IF sy-subrc <> 0.
    FREE: t_0032.
  ENDIF.

  SELECT *
    FROM zib_cte_dist_ter
    INTO TABLE @DATA(t_zib_cte_sub)
  WHERE docnum_cte_subcont = @w_saida-docnum_sub.
  IF sy-subrc <> 0.
    FREE: t_zib_cte_sub.
  ENDIF.

  SELECT *
    FROM zib_cte_dist_ter
    INTO TABLE @DATA(t_zib_cte_nor)
  WHERE docnum_cte = @w_saida-docnum_sub.
  IF sy-subrc <> 0.
    FREE: t_zib_cte_nor.
  ENDIF.

*---------------------------------------------
* status j_1bnfe_Active
*---------------------------------------------
  SELECT SINGLE *
    INTO w_header_nfe
    FROM j_1bnfe_active
  WHERE docnum = w_saida-docnum_sub.

  IF sy-subrc = 0.
    w_header_nfe-docsta = '2'.  "Recusado
    w_header_nfe-scssta = '2'.  "Autorização & cancelamento autorizado

    CALL FUNCTION 'J_1B_NFE_UPDATE_ACTIVE'
      EXPORTING
        i_acttab  = w_header_nfe
        i_updmode = 'U'.
  ENDIF.

*---------------------------------------------
* cancelar NF
*---------------------------------------------
  CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
    EXPORTING
      doc_number               = w_saida-docnum_sub
      ref_type                 = space
      ref_key                  = space
      can_dat                  = sy-datum
    IMPORTING
      doc_number               = l_docnum_new
    EXCEPTIONS
      document_not_found       = 1
      cancel_not_possible      = 2
      nf_cancel_type_not_found = 3
      database_problem         = 4
      docum_lock               = 5
      nfe_cancel_simulation    = 6
      OTHERS                   = 7.

  IF l_docnum_new IS INITIAL.
    MESSAGE s024(sd) WITH 'Não foi possível gerar estorno ' 'do Registro Fiscal!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*  l_mensagem = 'Registro Fiscal Criado: ' && l_docnum_new.

*---------------------------------------------
* atualiazr tabelas
*---------------------------------------------
  LOOP AT t_0032           INTO DATA(w_0032).
    UPDATE zlest0032        SET docnum_sub         = abap_off
                          WHERE tknum              = w_0032-tknum
                            AND fknum              = w_0032-fknum
                            AND ebeln              = w_0032-ebeln
                            AND ebelp              = w_0032-ebelp
                            AND lblni              = w_0032-lblni.
  ENDLOOP.

  LOOP AT t_zib_cte_sub    INTO DATA(w_zib_cte_sub).
    UPDATE zib_cte_dist_ter SET docnum_cte_subcont = abap_off
                          WHERE cd_chave_cte       = w_zib_cte_sub-cd_chave_cte.
  ENDLOOP.

  LOOP AT t_zib_cte_nor    INTO DATA(w_zib_cte_nor).
    UPDATE zib_cte_dist_ter SET docnum_cte         = abap_off
                          WHERE cd_chave_cte       = w_zib_cte_nor-cd_chave_cte.
  ENDLOOP.

  UPDATE zlest0246          SET docnum_est         = l_docnum_new
                                data_est           = sy-datum
                                hora_est           = sy-uzeit
                                user_est           = sy-uname
                          WHERE cd_chave_cte       = w_saida-chave_cte_sub
                            AND docnum             = w_saida-docnum_sub.

*---------------------------------------------
* log registro estorno
*---------------------------------------------
* PERFORM f_grava_log  USING w_saida-chave_cte_sub l_docnum_new 'S' l_mensagem.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  MESSAGE s024(sd) WITH 'Estorno Efetuado com Sucesso!'.

ENDFORM.

************************************************************************
* gravar log
************************************************************************
FORM f_grava_log USING p_chave_cte
                       p_docnum
                       p_status
                       p_mensagem.

  DATA: l_timestampl TYPE timestampl,
        w_zlest0246  TYPE zlest0246.

  DO.
    GET TIME STAMP FIELD l_timestampl.

    SELECT SINGLE cd_chave_cte
      INTO @DATA(_cd_chave_cte)
      FROM zlest0246
     WHERE cd_chave_cte = @p_chave_cte
    AND seq          = @l_timestampl.

    CHECK sy-subrc <> 0.

    w_zlest0246-mandt         = sy-mandt.
    w_zlest0246-cd_chave_cte  = p_chave_cte.
    w_zlest0246-seq           = l_timestampl.
    w_zlest0246-docnum        = p_docnum.
    w_zlest0246-status_msg    = p_status.
    w_zlest0246-mensagem      = p_mensagem.
    w_zlest0246-data_reg      = sy-datum.
    w_zlest0246-hora_reg      = sy-uzeit.
    w_zlest0246-user_reg      = sy-uname.
    MODIFY zlest0246       FROM w_zlest0246.
    EXIT.
  ENDDO.

ENDFORM.

**********************************************************************
* status
**********************************************************************
FORM f_status_subcontra    USING p_zib_cte_normal STRUCTURE zib_cte_dist_ter
                                 p_zib_cte_subcon STRUCTURE zib_cte_dist_ter
                                 p_perc_dif
                        CHANGING p_status.

  DATA: l_perc_var TYPE char10.

  p_status = icon_dummy.

  IF p_zib_cte_subcon-docnum_cte IS NOT INITIAL AND p_zib_cte_normal-docnum_cte_subcont IS NOT INITIAL.
    p_status   = icon_okay && ' Completo'.

    l_perc_var = p_perc_dif.
    CONDENSE l_perc_var.
    IF l_perc_var > l_check_tolera.
      p_status = icon_message_error_small && ' Dentro Tolerância'.
    ENDIF.
  ELSE.
    p_status   = icon_led_yellow && ' Pendente'.
  ENDIF.

ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

* PERFORM f_funcoes.
  PERFORM f_fieldcatalog.
  PERFORM f_sort.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
* w_layout-no_rowmark   = abap_true.
  w_layout-zebra        = abap_true.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_false.
  w_layout-no_totexp    = abap_false.
  w_layout-no_totline   = abap_false.
  w_layout-no_toolbar   = abap_false.
* w_layout-stylefname   = 'CELLSTYLES'.
* w_layout-ctab_fname   = 'CELLCOLOR'.
* w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-no_rowmark   = abap_false.
*
  w_variant-report      = sy-repid.
  w_variant-username    = sy-uname.
  w_variant-variant     = p_layout.
  w_variant-handle      = '0100'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  IF g_grid IS INITIAL. " AND  g_custom_container IS NOT INITIAL.
    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = 'X'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'HEADER'.

    PERFORM f_alv_header .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = g_custom_container
      EXCEPTIONS
        html_display_error = 1.

    "Grafico 1
    CALL METHOD cl_gui_cfw=>flush.

    CREATE OBJECT: g_custom_container
       EXPORTING
         container_name = 'CC_IMG',
         picture
       EXPORTING
         parent = g_custom_container.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = cl_container_95.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid,
                 lcl_event_handler=>on_data_changed  FOR g_grid,
                 lcl_event_handler=>user_command     FOR g_grid,
                 lcl_event_handler=>toolbar          FOR g_grid.

    CASE abap_true.
      WHEN p_normal.
        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            is_variant                    = w_variant
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_saida_normal[]
*           it_sort                       = t_sort[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

      WHEN p_subcon.
        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            is_variant                    = w_variant
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_saida_subcon[]
            it_sort                       = t_sort[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
    ENDCASE.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

**********************************************************************
*  exibir log
**********************************************************************
FORM f_exibir_log USING p_cd_chave_cte.

  TYPES: BEGIN OF ty_alv,
           status         TYPE char05,
           seq            TYPE zlest0246-seq,
           mensagem(1000) TYPE c,
           data_reg       TYPE zlest0246-data_reg,
           hora_reg       TYPE zlest0246-hora_reg,
           user_reg       TYPE zlest0246-user_reg,
           docnum_est     TYPE zlest0246-docnum_est,
           data_est       TYPE zlest0246-data_est,
           hora_est       TYPE zlest0246-hora_est,
           user_est       TYPE zlest0246-user_est.
  TYPES: END OF ty_alv.

  DATA: t_0246 TYPE TABLE OF zlest0246,
        w_0246 TYPE zlest0246,
        t_alv  TYPE TABLE OF ty_alv,
        w_alv  TYPE ty_alv.

  FREE: it_fieldcat.

  SELECT *
    FROM zlest0246
    INTO TABLE t_0246
  WHERE cd_chave_cte = p_cd_chave_cte.

  LOOP AT t_0246             INTO w_0246.
    MOVE-CORRESPONDING w_0246  TO w_alv.

    IF w_alv-docnum_est IS NOT INITIAL.
      w_alv-status              = icon_led_yellow.
    ELSE.
      IF w_0246-status_msg = 'S' .
        w_alv-status            = icon_led_green.
      ELSE.
        w_alv-status            = icon_led_red.
      ENDIF.
    ENDIF.

    w_alv-mensagem              = w_0246-mensagem.
    APPEND w_alv               TO t_alv.
  ENDLOOP.

  SORT t_alv BY seq DESCENDING.

*-----------------------------
* colunas alv
*-----------------------------
  PERFORM f_preenche_fcat USING :
   '01' ''          ''            'T_ALV'    'STATUS'     'Status'         '07'     ''    ''     'C'    '' '' 'X',
   '02' ''          ''            'T_ALV'    'MENSAGEM'   'Mensagem'       '50'    ''    ''     ' '    '' '' ' ',
   '03' ''          ''            'T_ALV'    'DATA_REG'   'Data'           '10'     ''    ''     ' '    '' '' ' ',
   '04' ''          ''            'T_ALV'    'HORA_REG'   'Hora'           '10'     ''    ''     ' '    '' '' ' ',
   '05' ''          ''            'T_ALV'    'USER_REG'   'Usuario'        '20'     ''    ''     ' '    '' '' ' ',
   '06' ''          ''            'T_ALV'    'DOCNUM_EST' 'Docnum Estorno' '15'     ''    ''     ' '    '' '' ' ',
   '07' ''          ''            'T_ALV'    'DATA_EST'   'Data Estorno'   '12'     ''    ''     ' '    '' '' ' ',
   '08' ''          ''            'T_ALV'    'HORA_EST'   'Hora Estorno'   '12'     ''    ''     ' '    '' '' ' ',
   '09' ''          ''            'T_ALV'    'USER_EST'   'User Estorno'   '20'     ''    ''     ' '    '' '' ' '.

*-----------------------------
* layout
*-----------------------------
  ls_variant-report = sy-repid && 'XXX'.
  l_grid_title      = 'Log de Ocorrências'.

*-----------------------------
* exibe alvv
*-----------------------------
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = sy-repid
      it_fieldcat           = it_fieldcat[]
*     it_sort               = t_sort[]
*     i_callback_user_command = 'USER_COMMAND_COMPRO'
      i_grid_title          = l_grid_title
      i_save                = 'X'
      is_variant            = ls_variant
      i_screen_start_column = 40
      i_screen_start_line   = 08
      i_screen_end_column   = 145
      i_screen_end_line     = 18
    TABLES
      t_outtab              = t_alv.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  CASE abap_true.
    WHEN p_normal.
      PERFORM f_estrutura_alv USING:
        01  ''          ''       'T_SAIDA_NORMAL' 'STATUS'            'Status'                '12'  ' ' ' ' 'C' ' ' ' ' ' ' ' ' ' ' 'X' 'X',
        02  ''          ''       'T_SAIDA_NORMAL' 'CHAVE_CTE_ORIGEM'  'Chave CTE Origem'      '45'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' 'X',
        03  ''          ''       'T_SAIDA_NORMAL' 'EMP_CTE_ORIG'      'Emp. CTE Orig.'        '10'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',"170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
        04  ''          ''       'T_SAIDA_NORMAL' 'FILIAL_CTE_ORIG'   'Filial. CTE Orig.'     '10'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',"170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
        05  ''          ''       'T_SAIDA_NORMAL' 'DT_CTE_ORIG'       'Dt Cte Origem'         '13'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
        06  ''          ''       'T_SAIDA_NORMAL' 'NR_CTE_ORIG'       'Nr Cte Origem'         '13'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
        07  ''          ''       'T_SAIDA_NORMAL' 'SERIE_CTE_ORIG'    'Serie CTE Origem'      '16'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
        08  ''          ''       'T_SAIDA_NORMAL' 'VLR_CTE_ORIG'      'Valor CTE Origem'      '16'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        09  ''          ''       'T_SAIDA_NORMAL' 'QT_CTE_ORIGEM'     'Qtde CTE Origem'       '16'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        10  'MARA'      'MATNR'  'T_SAIDA_NORMAL' 'COD_PROD_TRANSP'   'Produto'               '10'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        11  ''          ''       'T_SAIDA_NORMAL' 'DESC_PROD_TRANSP'  'Descrição'             '25'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        12  'J_1BNFDOC' 'DOCNUM' 'T_SAIDA_NORMAL' 'DOCNUM_ORIGEM'     'Docnum Origem'         '13'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
        13  'J_1BNFDOC' 'DOCNUM' 'T_SAIDA_NORMAL' 'DOCNUM_ENTRADA'    'Docnum Entrada'        '13'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
        14  'VTTK'      'TKNUM'  'T_SAIDA_NORMAL' 'DOC_TRANSP'        'Doc.Transporte'        '14'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
        15  'VFKP'      'FKNUM'  'T_SAIDA_NORMAL' 'DOC_CUSTO'         'Doc.Custo'             '14'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
        16  ''          ''       'T_SAIDA_NORMAL' 'COND_ZFRE'         'Condição ZFRE'         '15'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        17  ''          ''       'T_SAIDA_NORMAL' 'PERC_DIF'          'Perc.Diferença'        '14'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        18  ''          ''       'T_SAIDA_NORMAL' 'CHAVE_CTE_SUB'     'Chave CTE Subcontr'    '45'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
        19  ''          ''       'T_SAIDA_NORMAL' 'NR_CTE_SUB'        'Nr.CTE Subcontr'       '13'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        20  ''          ''       'T_SAIDA_NORMAL' 'SERIE_CTE_SUB'     'Serie CTE Subcontr'    '16'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
*       19  'J_1BNFDOC' 'CGC'    'T_SAIDA_NORMAL' 'CNPJ_FORNEC_SUB'   'CNPJ Fornec.Subcontr'  '20'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        21  '         ' '   '    'T_SAIDA_NORMAL' 'CNPJ_FORNEC_SUB'   'CNPJ Fornec.Subcontr'  '20'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        22  'LFA1'      'LIFNR'  'T_SAIDA_NORMAL' 'FORNEC_SUB'        'Fornecedor Subcontr'   '20'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        23  ''          ''       'T_SAIDA_NORMAL' 'VLR_CTE_SUB'       'Valor CTE Subcontr'    '18'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        24  ''          ''       'T_SAIDA_NORMAL' 'QT_CTE_SUB'        'Qtde CTE Subcontr'     '18'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        25  ''          ''       'T_SAIDA_NORMAL' 'PROD_PRED_SUB'     'Prod.Pred.Subcontr'    '20'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        26  'J_1BNFDOC' 'DOCNUM' 'T_SAIDA_NORMAL' 'DOCNUM_SUB'        'Docnum Subcontr'       '13'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
        27  ''          'DOCNUM' 'T_SAIDA_NORMAL' 'EMP_SUB'           'Emp. Subcontr'         '13'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',"170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
        28  ''          'DOCNUM' 'T_SAIDA_NORMAL' 'FILIAL_SUB'        'Filial Subcontr'       '13'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '."170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA

    WHEN p_subcon.
      PERFORM f_estrutura_alv USING:
        01  ''          ''       'T_SAIDA_SUBCON' 'STATUS'           'Status'               '21'  ' ' ' ' 'C' ' ' ' ' ' ' ' ' ' ' 'X' 'X',
        02  ''          ''       'T_SAIDA_SUBCON' 'EXIBE_LOG'        'Log Proc'             '10'  ' ' ' ' 'C' 'X' ' ' ' ' ' ' ' ' 'X' 'X',
        03  ''          ''       'T_SAIDA_SUBCON' 'CHAVE_CTE_SUB'    'Chave CTE Subcont'    '45'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' 'X',
        04  ''          ''       'T_SAIDA_SUBCON' 'NR_CTE_SUB'       'Nr Cte Subcontr'      '15'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
        05  ''          ''       'T_SAIDA_SUBCON' 'SERIE_CTE_SUB'    'Serie CTE Subcontr'   '19'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
        06  ''          'DOCNUM' 'T_SAIDA_SUBCON' 'EMP_SUB'          'Emp. Subcontr'        '13'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',"170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
        07  ''          'DOCNUM' 'T_SAIDA_SUBCON' 'FILIAL_SUB'       'Filial Subcontr'      '13'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',"170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
*       06  'J_1BNFDOC' 'CGC'    'T_SAIDA_SUBCON' 'CNPJ_FORNEC_SUB'  'CNPJ Fornec.Subcontr' '20'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        08  '         ' '   '    'T_SAIDA_SUBCON' 'CNPJ_FORNEC_SUB'  'CNPJ Fornec.Subcontr' '20'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        09  'LFA1'      'LIFNR'  'T_SAIDA_SUBCON' 'FORNEC_SUB'       'Fornecedor Subcontr'  '20'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        10  ''          ''       'T_SAIDA_SUBCON' 'VLR_CTE_SUB'      'Valor CTE Subcontr'   '18'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        11  ''          ''       'T_SAIDA_SUBCON' 'QT_CTE_SUB'       'Qtde CTE Subcontr'    '18'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        12  ''          ''       'T_SAIDA_SUBCON' 'PROD_PRED_SUB'    'Prod.Pred.Subcontr'   '20'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        13  'J_1BNFDOC' 'DOCNUM' 'T_SAIDA_SUBCON' 'DOCNUM_SUB'       'Docnum Subcontr'      '16'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
        14  ''          ''       'T_SAIDA_SUBCON' 'CHAVE_CTE_ORIGEM' 'Chave CTE Origem'     '45'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
        15  ''          ''       'T_SAIDA_SUBCON' 'NR_CTE_ORIG'      'Nr.CTE Origem'        '13'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        16  ''          ''       'T_SAIDA_SUBCON' 'SERIE_CTE_ORIG'   'Serie CTE Origem'     '16'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        17  ''          ''       'T_SAIDA_SUBCON' 'VLR_CTE_ORIG'     'Valor CTE Origem'     '16'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        18  ''          ''       'T_SAIDA_SUBCON' 'QT_CTE_ORIG'      'Qtde CTE Origem'      '16'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        19  'MARA'      'MATNR'  'T_SAIDA_SUBCON' 'COD_PROD_TRANSP'  'Produto'              '10'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        20  ''          ''       'T_SAIDA_SUBCON' 'DESC_PROD_TRANSP' 'Descrição'            '25'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        21  'J_1BNFDOC' 'DOCNUM' 'T_SAIDA_SUBCON' 'DOCNUM_ORIGEM'    'Docnum Origem'        '13'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
        22  ''          ''       'T_SAIDA_SUBCON' 'EMP_CTE_ORIG'     'Emp. CTE Orig.'       '10'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',"170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
        23  ''          ''       'T_SAIDA_SUBCON' 'FILIAL_CTE_ORIG'  'Filial. CTE Orig.'    '10'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',"170396 CS2022001028 QUESTÃO LEGAL PARTE 3 PSA
        24  'VTTK'      'TKNUM'  'T_SAIDA_SUBCON' 'DOC_TRANSP'       'Doc.Transporte'       '14'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
        25  'VFKP'      'FKNUM'  'T_SAIDA_SUBCON' 'DOC_CUSTO'        'Doc.Custo'            '14'  ' ' ' ' ' ' 'X' ' ' ' ' ' ' ' ' ' ' ' ',
        26  ''          ''       'T_SAIDA_SUBCON' 'COND_ZFRE'        'Condição ZFRE'        '15'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
        27  ''          ''       'T_SAIDA_SUBCON' 'PERC_DIF'         'Perc.Diferença'       '14'  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '.
  ENDCASE.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
* w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-coltext     = p_scrtext_l.
* w_fieldcat-scrtext_s   = p_scrtext_l.
* w_fieldcat-scrtext_m   = p_scrtext_l.
* w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
  w_fieldcat-no_out      = p_no_out.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

**********************************************************************
*  catalogo
**********************************************************************
FORM f_sort.

  FREE: t_sort.

  CASE abap_true.
    WHEN p_subcon.
      w_sort-fieldname = 'CHAVE_CTE_SUB'.
      w_sort-subtot    = abap_off.
      w_sort-spos      = 1.
      w_sort-up        = 'X'.
      APPEND w_sort   TO t_sort.
*
      w_sort-fieldname = 'CHAVE_CTE_ORIGEM'.
      w_sort-subtot    = abap_off.
      w_sort-spos      = 2.
      w_sort-up        = 'X'.
      APPEND w_sort   TO t_sort.
  ENDCASE.

ENDFORM.

**********************************************************************
*  cabecalho
**********************************************************************
FORM f_alv_header .

  DATA: wl_data1(10),
        wl_data2(10),
        wl_data(10),
        wl_hora(8),
        wl_linha(60),
  wl_text TYPE sdydo_text_element.

  wl_linha = 'Cockpit Frete de Suncontratação'.
  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->initialize_document.
  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>large
      sap_color    = cl_dd_area=>list_heading_int.

  wl_linha = COND #( WHEN p_normal = abap_true THEN 'Tipo CT-e: Normal'
                                               ELSE 'Tipo CT-e: Subcontratação' ).
  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->initialize_document.
  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>large
      sap_color    = cl_dd_area=>list_heading_int.

  CALL METHOD obj_dyndoc_id->new_line.

  IF s_dtemi IS NOT INITIAL.
    WRITE s_dtemi-low TO wl_data.
    CONCATENATE  'Data Emissão.......:' wl_data INTO wl_linha SEPARATED BY space.
    IF s_dtemi-high IS NOT INITIAL.
      WRITE s_dtemi-high TO wl_data.
      CONCATENATE  wl_linha 'a' wl_data INTO wl_linha SEPARATED BY space.
    ENDIF.

    CALL METHOD obj_dyndoc_id->new_line.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.

*  CALL METHOD obj_dyndoc_id->new_line.

*  wl_text = wl_linha.
*
*  CALL METHOD obj_dyndoc_id->add_text
*    EXPORTING
*      text         = wl_text
*      sap_style    = cl_dd_area=>heading
*      sap_fontsize = cl_dd_area=>list_normal
*      sap_color    = cl_dd_area=>list_heading_int.

*  CALL METHOD obj_dyndoc_id->new_line.
*
*  wl_text = wl_linha.
*
*  CALL METHOD obj_dyndoc_id->add_text
*    EXPORTING
*      text         = wl_text
*      sap_style    = cl_dd_area=>heading
*      sap_fontsize = cl_dd_area=>list_normal
*      sap_color    = cl_dd_area=>list_heading_int.

ENDFORM.

**********************************************************************
*  imagen
**********************************************************************
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

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

ENDFORM.                    " F_PEGA_IMAGEM

*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
FORM f_alv_variant_f4 CHANGING pa_vari.

  DATA: rs_variant LIKE disvariant.

  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.
  rs_variant-handle   = '0100'.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = rs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = rs_variant
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc = 0.
    pa_vari = rs_variant-variant.
  ENDIF.

ENDFORM.                               " ALV_VARIANT_F4

**********************************************************************
*      Module  STATUS_0100  OUTPUT
**********************************************************************
MODULE status_0100 OUTPUT.

  DATA: fcode TYPE TABLE OF sy-ucomm.

  IF p_normal = abap_true.
    APPEND 'ESTORNO'       TO fcode.
  ENDIF.

  SET PF-STATUS 'ZFIS0048' EXCLUDING fcode.
  SET TITLEBAR 'ZFIS0048'.

  PERFORM f_init_alv.

ENDMODULE.

**********************************************************************
*      Module  USER_COMMAND_0100  INPUT
**********************************************************************
MODULE user_command_0100 INPUT.

  CASE ok_code.
    WHEN 'REFRESH'.
      CASE abap_true.
          PERFORM f_selecao_dados_geral.
        WHEN p_normal.
          "PERFORM f_selecao_dados_geral.
          PERFORM f_selecao_dados_normal.
          PERFORM f_processa_normal.

        WHEN p_subcon.
          "PERFORM f_selecao_dados_geral.
          PERFORM f_selecao_dados_subcontratado.
          PERFORM f_processa_subcontratado.
      ENDCASE.

    WHEN 'ESTORNO'.
      PERFORM f_estorna_nf.
      PERFORM f_selecao_dados_geral.
      PERFORM f_selecao_dados_subcontratado.
      PERFORM f_processa_subcontratado.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'VOLTAR'.
      CALL METHOD g_custom_container->free.
      CALL METHOD cl_container_95->free.
      FREE: g_grid, g_custom_container, obj_dyndoc_id, cl_container_95.
*     CALL METHOD obj_dyndoc_id->custom_container->free( ).
      LEAVE TO SCREEN 0.

  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.

ENDMODULE.

**********************************************************************
* CATALOGO POPUP
**********************************************************************
FORM f_preenche_fcat   USING  VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit)
                              VALUE(p_do_sum)
                              VALUE(p_just)
                              VALUE(p_hotspot)
                              VALUE(p_checkbox)
                              VALUE(p_icon).

  CLEAR: wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-do_sum        = p_do_sum.
  wa_estrutura-just          = p_just.
  wa_estrutura-icon          = p_icon.
  wa_estrutura-hotspot       = p_hotspot.
  wa_estrutura-checkbox      = p_checkbox.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO it_fieldcat.

ENDFORM.

**********************************************************************
**********************************************************************
