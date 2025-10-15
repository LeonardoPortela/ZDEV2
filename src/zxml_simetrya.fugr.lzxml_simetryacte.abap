*&---------------------------------------------------------------------*
*&  Include           LZXML_SIMETRYACTE
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tipos de Dados
*----------------------------------------------------------------------*
TYPE-POOLS: zctet.

TABLES: zcte_info_nota,
        zcte_trans,
        zcte_ciot,
        zcte_parceiros,
        zcte_identifica,
        zcte_motorista,
        zcte_obs_gerais,
        zcte_seguro.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: event_handler      TYPE REF TO lcl_event_handler.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


DATA: cte_j_1bnfdoc TYPE j_1bnfdoc,
      cte_active    TYPE j_1bnfe_active,
      cte_form      TYPE j_1baqt,
      cte_catalog   TYPE lvc_s_fcat,
      cte_wa_fcode  TYPE sy-ucomm.

DATA: cte_dynnr_000       LIKE sy-dynnr,
      cte_dynnr_mod       LIKE sy-dynnr,
      cte_dynnr_ciot      LIKE sy-dynnr,
      cte_dynnr_ifn       LIKE sy-dynnr,
      cte_ok_code         LIKE sy-ucomm,
      cte_it_fcode        LIKE TABLE OF cte_wa_fcode,
      cte_bloqueado       TYPE c LENGTH 1,
      cte_autorizado_uso  TYPE c LENGTH 1,
      cte_alterando       TYPE c LENGTH 1,
      cte_alterando_modal TYPE c LENGTH 1,
      cte_alterado        TYPE c LENGTH 1,
      cte_cancelado       TYPE c LENGTH 1,
      st_ret              TYPE ddshretval,
      t_dynpfields        TYPE STANDARD TABLE OF dynpread   INITIAL SIZE 1 WITH HEADER LINE,
      t_ret               TYPE TABLE OF ddshretval.

CONTROLS: info_cte_tab      TYPE TABSTRIP,
          ctetb01           TYPE TABLEVIEW USING SCREEN 0100,
          ctetb02           TYPE TABLEVIEW USING SCREEN 0100,

          inf_nota_cte_tab  TYPE TABSTRIP,
          tbif01            TYPE TABLEVIEW USING SCREEN 0101,
          tbif02            TYPE TABLEVIEW USING SCREEN 0101,

          inf_trans_cte_tab TYPE TABSTRIP,
          tbts01            TYPE TABLEVIEW USING SCREEN 0102,
          tbts02            TYPE TABLEVIEW USING SCREEN 0102,

          inf_moto_cte_tab  TYPE TABSTRIP,
          tbmt01            TYPE TABLEVIEW USING SCREEN 0102,
          tbmt02            TYPE TABLEVIEW USING SCREEN 0102,

          inf_ciot_cte_tab  TYPE TABSTRIP,
          tbto01            TYPE TABLEVIEW USING SCREEN 0103,
          tbto02            TYPE TABLEVIEW USING SCREEN 0103,

          inf_ciot_detalhe  TYPE TABSTRIP,
          tbts06_01         TYPE TABLEVIEW USING SCREEN 2106,
          tbts06_02         TYPE TABLEVIEW USING SCREEN 2106,

          inf_seg_cte_tab   TYPE TABSTRIP,
          tbsg01            TYPE TABLEVIEW USING SCREEN 0107,
          tbsg02            TYPE TABLEVIEW USING SCREEN 0107,

          info_ciot         TYPE TABSTRIP,
          tb9970ciot01      TYPE TABLEVIEW USING SCREEN 9970,
          tb9970ciot02      TYPE TABLEVIEW USING SCREEN 9970.

CONSTANTS: cte_c_57         TYPE c LENGTH 2 VALUE '57',
           cte_c_back       TYPE c LENGTH 4 VALUE 'BACK',
           cte_c_exit       TYPE c LENGTH 4 VALUE 'EXIT',
           cte_c_save       TYPE c LENGTH 4 VALUE 'SAVE',
           cte_c_cancel     TYPE c LENGTH 6 VALUE 'CANCEL',
           cte_incluir      TYPE c LENGTH 7 VALUE 'INCLUIR',
           cte_alterar      TYPE c LENGTH 7 VALUE 'ALTERAR',
           cte_apagar       TYPE c LENGTH 6 VALUE 'APAGAR',
           cte_back         TYPE c LENGTH 6 VALUE 'BACK',
           cte_confirma     TYPE c LENGTH 8 VALUE 'CONFIRMA',
           cte_cancelar     TYPE c LENGTH 8 VALUE 'CANCELAR',
           cte_canc_viagem  TYPE c LENGTH 10 VALUE 'CANCVIAGEM', "US 71237
           cte_atualiza     TYPE c LENGTH 8 VALUE 'ATUALIZA',
           cte_todos        TYPE c LENGTH 5 VALUE 'TODOS',
           cte_imprimir     TYPE c LENGTH 8 VALUE 'IMPRIMIR',
           cte_impresumo    TYPE sy-ucomm VALUE 'IMPRESUMO',
           cte_impedagio    TYPE sy-ucomm VALUE 'PEDAGIO',
           cte_cargapedagio TYPE sy-ucomm VALUE 'CARGAPED',
           cte_todoscred    TYPE c LENGTH 9 VALUE 'TODOSCRED',
           cte_todoscanc    TYPE c LENGTH 9 VALUE 'TODOSCANC',
           cte_btn01        TYPE c LENGTH 5 VALUE 'BTN01',
           cte_btn02        TYPE c LENGTH 5 VALUE 'BTN02',
           cte_btn03        TYPE c LENGTH 5 VALUE 'BTN03',
           cte_btn04        TYPE c LENGTH 5 VALUE 'BTN04',
           cte_btn05        TYPE c LENGTH 5 VALUE 'BTN05',
           cte_cnfe         TYPE c LENGTH 4 VALUE 'CNFE',

           cte_c_0001       LIKE sy-dynnr VALUE '0001',
           cte_c_0101       LIKE sy-dynnr VALUE '0101',
           cte_c_1101       LIKE sy-dynnr VALUE '1101',
           cte_c_1102       LIKE sy-dynnr VALUE '1102',
           cte_c_0102       LIKE sy-dynnr VALUE '0102',
           cte_c_0104       LIKE sy-dynnr VALUE '0104',
           cte_c_0105       LIKE sy-dynnr VALUE '0105',
           cte_c_0106       LIKE sy-dynnr VALUE '0106',
           cte_c_0107       LIKE sy-dynnr VALUE '0107',
           cte_c_1701       LIKE sy-dynnr VALUE '1701',
           cte_c_1702       LIKE sy-dynnr VALUE '1702',
           cte_c_2101       LIKE sy-dynnr VALUE '2101',
           cte_c_2102       LIKE sy-dynnr VALUE '2102',
           cte_c_2001       LIKE sy-dynnr VALUE '2001',
           cte_c_2002       LIKE sy-dynnr VALUE '2002',
           cte_c_2003       LIKE sy-dynnr VALUE '2003',
           cte_c_2004       LIKE sy-dynnr VALUE '2004',
           cte_c_2005       LIKE sy-dynnr VALUE '2005',
           cte_c_2103       LIKE sy-dynnr VALUE '2103',
           cte_c_2104       LIKE sy-dynnr VALUE '2104',
           cte_c_2105       LIKE sy-dynnr VALUE '2105',
           cte_c_2106       LIKE sy-dynnr VALUE '2106',
           cte_c_6001       LIKE sy-dynnr VALUE '6001',
           cte_c_6002       LIKE sy-dynnr VALUE '6002',
           cte_c_6003       LIKE sy-dynnr VALUE '6003',
           cte_c_6004       LIKE sy-dynnr VALUE '6004',
           cte_c_6005       LIKE sy-dynnr VALUE '6005',
           cte_c_6006       LIKE sy-dynnr VALUE '6006',
           cte_c_6007       LIKE sy-dynnr VALUE '6007',
           cte_c_6008       LIKE sy-dynnr VALUE '6008',
           cte_c_6009       LIKE sy-dynnr VALUE '6009',
           cte_c_6010       LIKE sy-dynnr VALUE '6010',
           cte_c_6011       LIKE sy-dynnr VALUE '6011',
           cte_c_6012       LIKE sy-dynnr VALUE '6012',
           cte_ctetb01      TYPE c LENGTH 7 VALUE 'CTETB01',
           cte_ctetb02      TYPE c LENGTH 7 VALUE 'CTETB02',
           cte_ctetb03      TYPE c LENGTH 7 VALUE 'CTETB03',
           cte_ctetb04      TYPE c LENGTH 7 VALUE 'CTETB04',
           cte_ctetb05      TYPE c LENGTH 7 VALUE 'CTETB05',
           cte_ctetb06      TYPE c LENGTH 7 VALUE 'CTETB06',
           cte_ctetb07      TYPE c LENGTH 7 VALUE 'CTETB07',
           cte_tbif01       TYPE c LENGTH 6 VALUE 'TBIF01',
           cte_tbif02       TYPE c LENGTH 6 VALUE 'TBIF02',
           cte_tbts01       TYPE c LENGTH 6 VALUE 'TBTS01',
           cte_tbts02       TYPE c LENGTH 6 VALUE 'TBTS02',
           cte_tbts03       TYPE c LENGTH 6 VALUE 'TBTS03',
           cte_tbts04       TYPE c LENGTH 6 VALUE 'TBTS04',
           cte_tbts05       TYPE c LENGTH 6 VALUE 'TBTS05',
           cte_tbts06       TYPE c LENGTH 6 VALUE 'TBTS06',
           cte_tbsg01       TYPE c LENGTH 6 VALUE 'TBSG01',
           cte_tbsg02       TYPE c LENGTH 6 VALUE 'TBSG02',
           cte_tbts0601     TYPE c LENGTH 9 VALUE 'TBTS06_01',
           cte_tbts0602     TYPE c LENGTH 9 VALUE 'TBTS06_02',
           cte_tbts0603     TYPE c LENGTH 9 VALUE 'TBTS06_03',
           cte_tbts0604     TYPE c LENGTH 9 VALUE 'TBTS06_04',
           cte_tbts0605     TYPE c LENGTH 9 VALUE 'TBTS06_05',
           cte_tbts0606     TYPE c LENGTH 9 VALUE 'TBTS06_06',
           cte_tbts0607     TYPE c LENGTH 9 VALUE 'TBTS06_07',
           cte_tbts0608     TYPE c LENGTH 9 VALUE 'TBTS06_08',
           cte_tbts0609     TYPE c LENGTH 9 VALUE 'TBTS06_09',
           cte_tbts0610     TYPE c LENGTH 9 VALUE 'TBTS06_10',
           cte_tbts0611     TYPE c LENGTH 9 VALUE 'TBTS06_11',
           cte_tbts0612     TYPE c LENGTH 9 VALUE 'TBTS06_12'.


CONSTANTS: gc_text_line_length TYPE i VALUE 72.

"Informações de identificaçao
DATA: copia_identifica       TYPE zcte_identifica.

"Informações dos parceiros do conhecimento
DATA: wa_cte_parceiros TYPE zctet_info_parceiro,
      copia_parceiros  TYPE zcte_parceiros.

"Informações de Nota Fiscal de Mercadoria da CT-e
DATA: it_cte_info_nota       TYPE TABLE OF zcte_info_nota INITIAL SIZE 0 WITH HEADER LINE,
      it_cte_if_nt_alv       TYPE TABLE OF zcte_info_nota INITIAL SIZE 0 WITH HEADER LINE,
      cte_info_nota          TYPE zcte_info_nota,
      cte_if_nt_alv          TYPE zcte_info_nota,
      cte_prim_info_nota     TYPE c LENGTH 1,
      it_cte_catalog_inf_nt  TYPE lvc_t_fcat,
      cte_alv_inf_nota       TYPE REF TO cl_gui_alv_grid,
      cte_container_inf_nota TYPE REF TO cl_gui_custom_container,
      cte_gs_layout          TYPE lvc_s_layo,
      cte_text_modelo        TYPE c LENGTH 60,
      cte_text_cliente_nome  TYPE c LENGTH 35,
      cte_text_cliente_cgc   TYPE c LENGTH 18,
      cte_text_cfop          TYPE j_1bcfotxt,
      cte_text_numero        TYPE j_1bnfnum9,
      cte_text_cv_uf         TYPE j_1bregio,
      cte_text_cv_ano        TYPE j_1byear,
      cte_text_cv_mes        TYPE j_1bmonth,
      cte_text_cv_cnpj       TYPE j_1bstcd1,
* ---> S4 Migration - 07/07/2023 - JP
*      cte_text_cv_mod        TYPE char02,
      cte_text_cv_mod        TYPE c LENGTH 2,
* <--- S4 Migration - 07/07/2023 - JP
      cte_text_cv_serie      TYPE j_1bseries,
      cte_text_motorista_cpf TYPE c LENGTH 14,
      cte_text_produto       TYPE maktx.



"Modal Rodoviário
DATA: vg_transp_emissora   TYPE lfa1-name1.

"Informações de Veículos da CT-e
DATA: it_cte_trans         TYPE TABLE OF zcte_trans INITIAL SIZE 0 WITH HEADER LINE,
      it_cte_trans_alv     TYPE TABLE OF zcte_trans INITIAL SIZE 0 WITH HEADER LINE,
      cte_trans            TYPE zcte_trans,
      cte_trans_alv        TYPE zcte_trans,
      cte_prim_trans       TYPE c LENGTH 1,
      it_cte_catalog_trans TYPE lvc_t_fcat,
      cte_alv_trans        TYPE REF TO cl_gui_alv_grid,
      cte_container_trans  TYPE REF TO cl_gui_custom_container.

"Informações de Motoristas
DATA: it_cte_moto         TYPE TABLE OF zcte_motorista INITIAL SIZE 0 WITH HEADER LINE,
      it_cte_moto_alv     TYPE TABLE OF zcte_motorista INITIAL SIZE 0 WITH HEADER LINE,
      cte_moto            TYPE zcte_motorista,
      cte_moto_alv        TYPE zcte_motorista,
      cte_prim_moto       TYPE c LENGTH 1,
      it_cte_catalog_moto TYPE lvc_t_fcat,
      cte_alv_moto        TYPE REF TO cl_gui_alv_grid,
      cte_container_moto  TYPE REF TO cl_gui_custom_container.

"Informações de Observações Gerais
DATA: it_cte_obsg         TYPE TABLE OF zcte_obs_gerais INITIAL SIZE 0 WITH HEADER LINE,
      cte_obsg            TYPE zcte_obs_gerais,
      gv_custom_container TYPE REF TO cl_gui_custom_container,
      gv_text_editor      TYPE REF TO cl_gui_textedit.


"Informações de Seguro
TYPES: BEGIN OF ty_seguro_alv.
         INCLUDE STRUCTURE zcte_seguro.
TYPES:   txtresponsavel TYPE c LENGTH 60.
TYPES: END OF ty_seguro_alv.

DATA: it_cte_seguro         TYPE TABLE OF zcte_seguro INITIAL SIZE 0 WITH HEADER LINE,
      cte_seguro            TYPE zcte_seguro,
      it_cte_seguro_alv     TYPE TABLE OF ty_seguro_alv INITIAL SIZE 0 WITH HEADER LINE,
      cte_seguro_alv        TYPE ty_seguro_alv,
      cte_prim_seguro       TYPE c LENGTH 1,
      cte_alv_seguro        TYPE REF TO cl_gui_alv_grid,
      cte_container_seguro  TYPE REF TO cl_gui_custom_container,
      it_cte_catalog_seguro TYPE lvc_t_fcat.

TYPES: text_table_type(gc_text_line_length) TYPE c OCCURS 0.

DATA: BEGIN OF tlinetab OCCURS 10.     "Zeilen Langtext
        INCLUDE STRUCTURE tline.
DATA: END OF tlinetab.

"Informações CIOT

TYPES: BEGIN OF ty_ciot_alv.
TYPES: si_ciot        TYPE char04,
       pc_codigo      TYPE j_1bparid,
       pc_nome        TYPE char_60,
       pc_razao       TYPE char_60,
       pc_cnpj        TYPE stcd1,
       pc_cpf         TYPE stcd2,
       pc_logradouro  TYPE ad_street,
       pc_numero      TYPE ad_hsnm1,
       pc_complemento TYPE char_60,
       pc_bairro      TYPE ad_city2,
       pc_uf          TYPE regio,
       pc_municipio   TYPE zmunic_ibge,
       pc_cep         TYPE pstlz_bas,
       pc_fone        TYPE telf1,
       le_codigo      TYPE j_1bparid,
       le_nome        TYPE char_60,
       le_razao       TYPE char_60,
       le_cnpj        TYPE stcd1,
       le_cpf         TYPE stcd2,
       le_logradouro  TYPE ad_street,
       le_numero      TYPE ad_hsnm1,
       le_complemento TYPE char_60,
       le_bairro      TYPE ad_city2,
       le_uf          TYPE regio,
       le_municipio   TYPE zmunic_ibge,
       le_cep         TYPE pstlz_bas,
       le_fone        TYPE telf1.
       INCLUDE STRUCTURE zcte_ciot.
TYPES: END OF ty_ciot_alv.

DATA: it_cte_ciot          TYPE TABLE OF zcte_ciot INITIAL SIZE 0 WITH HEADER LINE,
      it_prt_ciot          TYPE TABLE OF zcte_viagem_obs INITIAL SIZE 0 WITH HEADER LINE,
      it_cte_ciot_alv      TYPE TABLE OF ty_ciot_alv INITIAL SIZE 0 WITH HEADER LINE,
      cte_ciot             TYPE zcte_ciot,
      cte_ciot_parceiros   TYPE zcte_ciot_parce_t,
      cte_ciot_alv         TYPE ty_ciot_alv,
      it_zcl_ciot          TYPE TABLE OF REF TO zcl_ciot WITH HEADER LINE,
      cte_obj_ciot         TYPE REF TO zcl_ciot,
      cte_prim_ciot        TYPE c LENGTH 1,
      it_cte_catalog_ciot  TYPE lvc_t_fcat,
      it_cte_catalog_cioto TYPE lvc_t_fcat,
      cte_alv_ciot         TYPE REF TO cl_gui_alv_grid,
      cte_alv_cioto        TYPE REF TO cl_gui_alv_grid,
      cte_container_ciot   TYPE REF TO cl_gui_custom_container,
      cte_container_cioto  TYPE REF TO cl_gui_custom_container,
      txt_municipio_origem TYPE text60,
      txt_municipio_termin TYPE text60,
      txt_nm_ct_ciot       TYPE text60,
      txt_nm_sb_ciot       TYPE text60,
      txt_nm_cs_ciot       TYPE text60,
      txt_nm_rm_ciot       TYPE text60,
      txt_nm_pc_ciot       TYPE text60,
      txt_nm_le_ciot       TYPE text60,
      txt_nm_mt_ciot       TYPE text60,
      txt_nm_dt_ciot       TYPE text60,
      txt_nm_tr_ciot       TYPE text60,
      vg_tx_status_ciot    TYPE text60,
      txt_material         TYPE maktx,
      txt_cnpj_cpf         TYPE c LENGTH 18,
      p_emite_ciot         TYPE char01.


*&---------------------------------------------------------------------*
*&      Module  CTE_STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE cte_status_0100 OUTPUT.

  SET TITLEBAR  'TLAVULSO'.

  IF cte_dynnr_000 IS INITIAL.
    cte_dynnr_000 = cte_c_0105.
  ENDIF.

  CLEAR: cte_it_fcode.

  CASE cte_dynnr_000.
      "Tela de Nota Fiscal
    WHEN cte_c_0101.
      info_cte_tab-activetab = cte_ctetb01.
      IF cte_dynnr_ifn IS INITIAL.
        cte_dynnr_ifn = cte_c_1101.
      ENDIF.
      IF cte_dynnr_ifn EQ cte_c_1102.
        "Remove botoes de inclusão/alteração
        IF ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).
          cte_alterando = 'C'.
        ENDIF.
        PERFORM tira_botoes_saida.
      ELSE.
        PERFORM tira_botoes_tela USING cte_alterando.
      ENDIF.
      "Tela de Modais
    WHEN cte_c_0102.
      info_cte_tab-activetab = cte_ctetb02.
      IF cte_dynnr_ifn IS INITIAL.
        cte_dynnr_ifn = cte_c_2101.
      ENDIF.
      IF ( cte_dynnr_ifn EQ cte_c_2102 ) OR ( cte_dynnr_ifn EQ cte_c_2104 ) OR ( cte_dynnr_ifn EQ cte_c_2106 ).
        "Remove botoes de inclusão/alteração
        IF ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).
          cte_alterando = 'C'.
        ENDIF.
        PERFORM tira_botoes_saida.
      ELSE.
        PERFORM tira_botoes_tela USING cte_alterando.
      ENDIF.
      "Tela de Parceiros
    WHEN cte_c_0104.
      info_cte_tab-activetab = cte_ctetb04.
      IF ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).
        cte_alterando = 'C'.
      ELSE.
        PERFORM tira_botoes_saida.
      ENDIF.
      PERFORM tira_botoes_tela USING cte_alterando.
      "Tela de Identicicação
    WHEN cte_c_0105.
      info_cte_tab-activetab = cte_ctetb05.
      IF ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).
        cte_alterando = 'C'.
      ELSE.
        PERFORM tira_botoes_saida.
      ENDIF.
      PERFORM tira_botoes_tela USING cte_alterando.
      "Tela de Observações
    WHEN cte_c_0106.
      info_cte_tab-activetab = cte_ctetb06.
      IF ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).
        cte_alterando = 'C'.
      ELSE.
        PERFORM tira_botoes_saida.
      ENDIF.
      PERFORM tira_botoes_tela USING cte_alterando.
      "Tela de Seguro
    WHEN cte_c_0107.
      info_cte_tab-activetab = cte_ctetb07.
      IF cte_dynnr_ifn IS INITIAL.
        cte_dynnr_ifn = cte_c_1701.
      ENDIF.
      IF cte_dynnr_ifn EQ cte_c_1702.
        "Remove botoes de inclusão/alteração
        IF ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).
          cte_alterando = 'C'.
        ENDIF.
        PERFORM tira_botoes_saida.
      ELSE.
        PERFORM tira_botoes_tela USING cte_alterando.
      ENDIF.
  ENDCASE.

  SET PF-STATUS 'PFAVULSO' EXCLUDING cte_it_fcode.

ENDMODULE.                 " CTE_STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE cte_command_0100 INPUT.

  CHECK cte_alterado IS INITIAL.
  CHECK ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).

  IF cte_obj_ciot IS NOT INITIAL.
    CLEAR: cte_obj_ciot.
  ENDIF.

  CASE cte_ok_code.
    WHEN cte_ctetb01.
      cte_dynnr_000 = cte_c_0101.
      CLEAR: cte_dynnr_ifn.
    WHEN cte_ctetb02.
      IF zcte_identifica-modal IS NOT INITIAL.
        cte_dynnr_000 = cte_c_0102.
      ELSE.
        MESSAGE s023 WITH 'Deve ser definido um modal!'.
      ENDIF.
      CLEAR: cte_dynnr_ifn, cte_dynnr_ciot.
    WHEN cte_ctetb04.
      cte_dynnr_000 = cte_c_0104.
      CLEAR: cte_dynnr_ifn.
    WHEN cte_ctetb05.
      cte_dynnr_000 = cte_c_0105.
      CLEAR: cte_dynnr_ifn.
    WHEN cte_ctetb06.
      cte_dynnr_000 = cte_c_0106.
      CLEAR: cte_dynnr_ifn.
    WHEN cte_ctetb07.
      cte_dynnr_000 = cte_c_0107.
      CLEAR: cte_dynnr_ifn.
  ENDCASE.

  IF ( cte_ok_code NE cte_incluir  ) AND
     ( cte_ok_code NE cte_alterar  ) AND
     ( cte_ok_code NE cte_apagar   ) AND
     ( cte_ok_code NE cte_confirma ) AND
     ( cte_ok_code NE cte_cancelar ).
    CLEAR: cte_alterando.
  ENDIF.

ENDMODULE.                 " CTE_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_APLICATIVO_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE cte_aplicativo_exit INPUT.

  CASE cte_ok_code.
    WHEN cte_c_back OR cte_c_exit OR cte_c_cancel.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " CTE_APLICATIVO_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_CRIA_INFO_NOTA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_cria_info_nota OUTPUT.

  PERFORM cte_cria_inf_nota_alv.

  MOVE: it_cte_info_nota[] TO it_cte_if_nt_alv[].

  CALL METHOD cte_alv_inf_nota->refresh_table_display.

ENDMODULE.                 " CTE_CRIA_INFO_NOTA  OUTPUT

*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
FORM z_estrutura_fieldcat TABLES it_catalogo TYPE lvc_t_fcat
                           USING p_tab_name
                                 p_fieldname
                                 p_texto_grande
                                 p_hot
                                 p_posicao
                                 p_outputlen
                                 p_fix_column
                                 p_convexit.
  CLEAR cte_catalog.
  cte_catalog-tabname     = p_tab_name.
  cte_catalog-fieldname   = p_fieldname.
  cte_catalog-scrtext_l   = p_texto_grande.
  cte_catalog-scrtext_m   = p_texto_grande.
  cte_catalog-scrtext_s   = p_texto_grande.
  cte_catalog-hotspot     = p_hot.
  cte_catalog-col_pos     = p_posicao.
  cte_catalog-outputlen   = p_outputlen.
  cte_catalog-fix_column  = p_fix_column.
  cte_catalog-convexit    = p_convexit.
  APPEND cte_catalog TO it_catalogo.
ENDFORM.                    " Z_ESTRUTURA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Module  CTE_STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
MODULE cte_status_0101 OUTPUT.

  IF cte_dynnr_ifn IS INITIAL.
    cte_dynnr_ifn = cte_c_1101.
  ENDIF.

  CASE cte_dynnr_ifn.
    WHEN cte_c_1101.
      inf_nota_cte_tab-activetab = cte_tbif01.
    WHEN cte_c_1102.
      inf_nota_cte_tab-activetab = cte_tbif02.
  ENDCASE.

ENDMODULE.                 " CTE_STATUS_0101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0101 INPUT.

  CHECK cte_alterado IS INITIAL.
  CHECK ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).

  CASE cte_ok_code.
    WHEN cte_tbif01 OR cte_back.
      cte_dynnr_ifn = cte_c_1101.
    WHEN cte_tbif02.
      PERFORM seleciona_info_nota.
      cte_dynnr_ifn = cte_c_1102.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0101  INPUT

*&---------------------------------------------------------------------*
*&      Form  TIRA_BOTOES_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tira_botoes_tela USING p_altera TYPE c.

  IF ( p_altera EQ 'A' ) OR ( p_altera EQ 'I' ).

    cte_wa_fcode = cte_incluir.
    APPEND cte_wa_fcode TO cte_it_fcode.
    cte_wa_fcode = cte_apagar.
    APPEND cte_wa_fcode TO cte_it_fcode.
    cte_wa_fcode = cte_back.
    APPEND cte_wa_fcode TO cte_it_fcode.

    IF ( NOT ( ( cte_dynnr_ifn EQ cte_c_2101 ) OR ( cte_dynnr_ifn EQ cte_c_2103 ) OR ( cte_dynnr_ifn EQ cte_c_2105 ) ) ) OR
      cte_alterando_modal EQ 'A'.
      cte_wa_fcode = cte_alterar.
      APPEND cte_wa_fcode TO cte_it_fcode.
    ENDIF.

    IF cte_alterando_modal EQ 'A'.
      cte_wa_fcode = cte_c_cancel.
      APPEND cte_wa_fcode TO cte_it_fcode.
    ENDIF.

  ELSEIF p_altera EQ 'C'.

    IF ( cte_bloqueado EQ c_x ) OR ( cte_dynnr_ifn NE cte_c_1102 ).
      cte_wa_fcode = cte_alterar.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_apagar.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_incluir.
      APPEND cte_wa_fcode TO cte_it_fcode.
    ENDIF.

    cte_wa_fcode = cte_confirma.
    APPEND cte_wa_fcode TO cte_it_fcode.

    "Cadastro sem ABA - Somente Cadastro
    IF ( cte_dynnr_000 EQ cte_c_0104 ) OR ( cte_dynnr_000 EQ cte_c_0105 ) OR ( cte_dynnr_000 EQ cte_c_0106 ).
      cte_wa_fcode = cte_incluir.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_back.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_cancelar.
      APPEND cte_wa_fcode TO cte_it_fcode.

      CASE cte_dynnr_000.
        WHEN cte_c_0106.
          IF it_cte_obsg[] IS INITIAL.
            cte_wa_fcode = cte_apagar.
            APPEND cte_wa_fcode TO cte_it_fcode.
          ENDIF.
        WHEN OTHERS.
          cte_wa_fcode = cte_apagar.
          APPEND cte_wa_fcode TO cte_it_fcode.
      ENDCASE.
    ENDIF.

    "Cadastros com ABA de Consulta e Cadastro
    IF ( cte_dynnr_ifn EQ cte_c_1101 ) OR ( cte_dynnr_ifn EQ cte_c_1102 )
    OR ( cte_dynnr_ifn EQ cte_c_1701 ) OR ( cte_dynnr_ifn EQ cte_c_1702 )
    OR ( cte_dynnr_ifn EQ cte_c_2101 ) OR ( cte_dynnr_ifn EQ cte_c_2102 )
    OR ( cte_dynnr_ifn EQ cte_c_2103 ) OR ( cte_dynnr_ifn EQ cte_c_2104 )
    OR ( cte_dynnr_ifn EQ cte_c_2105 ) OR ( cte_dynnr_ifn EQ cte_c_2106 ).

      cte_wa_fcode = cte_cancelar.
      APPEND cte_wa_fcode TO cte_it_fcode.

      CASE cte_dynnr_ifn.
          "Nota Fiscal
        WHEN cte_c_1102.
          IF it_cte_info_nota[] IS INITIAL.
            cte_wa_fcode = cte_apagar.
            APPEND cte_wa_fcode TO cte_it_fcode.
            cte_wa_fcode = cte_alterar.
            APPEND cte_wa_fcode TO cte_it_fcode.
          ENDIF.
          "Veículos
        WHEN cte_c_2102.
          cte_wa_fcode = cte_alterar.
          APPEND cte_wa_fcode TO cte_it_fcode.
          IF it_cte_trans[] IS INITIAL.
            cte_wa_fcode = cte_apagar.
            APPEND cte_wa_fcode TO cte_it_fcode.
          ENDIF.
          "Motoristas
        WHEN cte_c_2104.
          cte_wa_fcode = cte_alterar.
          APPEND cte_wa_fcode TO cte_it_fcode.
          IF it_cte_moto[] IS INITIAL.
            cte_wa_fcode = cte_apagar.
            APPEND cte_wa_fcode TO cte_it_fcode.
          ENDIF.
          "CIOT
        WHEN cte_c_2106.
          IF it_cte_ciot[] IS INITIAL.
            cte_wa_fcode = cte_apagar.
            APPEND cte_wa_fcode TO cte_it_fcode.
            cte_wa_fcode = cte_alterar.
            APPEND cte_wa_fcode TO cte_it_fcode.
          ENDIF.
          "Seguro
        WHEN cte_c_1702.
          IF it_cte_seguro[] IS INITIAL.
            cte_wa_fcode = cte_apagar.
            APPEND cte_wa_fcode TO cte_it_fcode.
            cte_wa_fcode = cte_alterar.
            APPEND cte_wa_fcode TO cte_it_fcode.
          ENDIF.
      ENDCASE.

    ENDIF.

  ELSE.
    PERFORM tira_botoes_tela USING 'C'.
    PERFORM tira_botoes_tela USING 'A'.
  ENDIF.

ENDFORM.                    " TIRA_BOTOES_TELA

*&---------------------------------------------------------------------*
*&      Form  TIRA_BOTOES_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tira_botoes_saida .

  cte_wa_fcode = cte_c_save.
  APPEND cte_wa_fcode TO cte_it_fcode.
  cte_wa_fcode = cte_c_cancel.
  APPEND cte_wa_fcode TO cte_it_fcode.

  PERFORM tira_botoes_tela USING cte_alterando.

ENDFORM.                    " TIRA_BOTOES_SAIDA

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1102 INPUT.

  DATA: answer_nota   TYPE c LENGTH 1,
        validado_nota TYPE c LENGTH 1,
        wa_info_part  TYPE lfa1,
        p_parid       TYPE j_1bparid,
        p_tp_forne    TYPE ztp_fornecimento,
        p_dc_forne    TYPE zdc_fornecimento,
        it_lips       TYPE TABLE OF lips WITH HEADER LINE.

  CASE cte_ok_code.
    WHEN cte_incluir.
      CLEAR: zcte_info_nota.

      zcte_info_nota-partyp = 'V'.

      CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
        EXPORTING
          p_docnum   = cte_j_1bnfdoc-docnum
        CHANGING
          p_parid    = p_parid
          p_tp_forne = p_tp_forne
          p_dc_forne = p_dc_forne.

      zcte_info_nota-cliente    = p_parid.

      CALL FUNCTION 'Z_PARCEIRO_INFO'
        EXPORTING
          p_parceiro   = p_parid
          p_partype    = zcte_info_nota-partyp
        CHANGING
          wa_info_part = wa_info_part.

      zcte_info_nota-name1      = wa_info_part-name1.
      zcte_info_nota-stcd1      = wa_info_part-stcd1.
      zcte_info_nota-stcd2      = wa_info_part-stcd2.
      zcte_info_nota-pfisica    = wa_info_part-stkzn.
      zcte_info_nota-docnum     = cte_j_1bnfdoc-docnum.
      zcte_info_nota-quantidade = 0.
      zcte_info_nota-peso_fiscal = 0.

      CLEAR: it_lips[].

      SELECT * INTO TABLE it_lips
        FROM lips
       WHERE vbeln EQ p_dc_forne.

      LOOP AT it_lips.
        zcte_info_nota-material = it_lips-matnr.
        ADD it_lips-brgew TO zcte_info_nota-quantidade.
        ADD it_lips-lfimg TO zcte_info_nota-peso_fiscal.
        zcte_info_nota-unidade = it_lips-gewei.
      ENDLOOP.

      CLEAR: cte_alterado.
      cte_alterando = 'I'.
    WHEN cte_alterar.
      CLEAR: cte_alterado.
      cte_alterando = 'A'.
    WHEN cte_apagar.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-004
        IMPORTING
          answer    = answer_nota.

      CASE answer_nota.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.

      DELETE FROM zcte_info_nota WHERE nfe     EQ zcte_info_nota-nfe
                                   AND modelo  EQ zcte_info_nota-modelo
                                   AND serie   EQ zcte_info_nota-serie
                                   AND numero  EQ zcte_info_nota-numero
                                   AND cliente EQ zcte_info_nota-cliente.

      DELETE it_cte_info_nota WHERE nfe     EQ zcte_info_nota-nfe
                                AND modelo  EQ zcte_info_nota-modelo
                                AND serie   EQ zcte_info_nota-serie
                                AND numero  EQ zcte_info_nota-numero
                                AND cliente EQ zcte_info_nota-cliente.

      CLEAR: zcte_info_nota.

    WHEN cte_confirma.

      PERFORM verificar_insert_inf_nf USING zcte_info_nota validado_nota.

      IF validado_nota EQ 'X'.
        zcte_info_nota-docnum = cte_j_1bnfdoc-docnum.
        MODIFY zcte_info_nota.
        PERFORM atualiza_viagem USING zcte_info_nota.

        IF cte_alterando EQ 'I'.
          MOVE-CORRESPONDING zcte_info_nota TO cte_if_nt_alv.
          APPEND cte_if_nt_alv TO it_cte_info_nota.
        ELSEIF cte_alterando EQ 'A'.
          READ TABLE it_cte_info_nota INTO cte_if_nt_alv WITH KEY nfe = zcte_info_nota-nfe
                                    modelo  = zcte_info_nota-modelo
                                    serie   = zcte_info_nota-serie
                                    numero  = zcte_info_nota-numero
                                    cliente = zcte_info_nota-cliente.
          MOVE-CORRESPONDING zcte_info_nota TO cte_if_nt_alv.

          MODIFY it_cte_info_nota INDEX sy-tabix FROM cte_if_nt_alv.
        ENDIF.

        PERFORM ajusta_valor_frete_empresa USING cte_j_1bnfdoc-docnum.

        cte_alterando = 'C'.
        CLEAR: cte_alterado.
      ENDIF.

    WHEN cte_cancelar.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-002
          textline2 = TEXT-003
        IMPORTING
          answer    = answer_nota.

      CASE answer_nota.
        WHEN 'J'.
          CLEAR: cte_alterado, cte_alterado.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.

      READ TABLE it_cte_info_nota INTO cte_if_nt_alv WITH KEY nfe = zcte_info_nota-nfe
                                modelo  = zcte_info_nota-modelo
                                serie   = zcte_info_nota-serie
                                numero  = zcte_info_nota-numero
                                cliente = zcte_info_nota-cliente.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING cte_if_nt_alv TO zcte_info_nota.
      ELSE.
        CLEAR: zcte_info_nota.
      ENDIF.
      cte_alterando = 'C'.
    WHEN cte_cnfe.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1102  INPUT


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_INFO_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_info_nota .

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CALL METHOD cte_alv_inf_nota->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  IF it_selected_rows IS NOT INITIAL.
    READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
    READ TABLE it_cte_if_nt_alv INDEX wa_selected_rows-index INTO cte_if_nt_alv.
    MOVE-CORRESPONDING cte_if_nt_alv TO zcte_info_nota.
  ENDIF.

ENDFORM.                    " SELECIONA_INFO_NOTA

*&---------------------------------------------------------------------*
*&      Module  SET_UPDATE_FLAG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_update_flag INPUT.
  cte_alterado = 'X'.
ENDMODULE.                 " SET_UPDATE_FLAG  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_1102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1102 OUTPUT.

  DATA: it_doc_model TYPE TABLE OF dd07v WITH KEY domvalue_l,
        wa_doc_text  TYPE dd07v,
        wa_info_c    TYPE kna1,
        wa_info_k    TYPE lfa1,
        wa_j_1bagt   TYPE j_1bagt.

  CLEAR: cte_text_modelo,
         cte_text_cliente_nome,
         cte_text_cliente_cgc,
         cte_text_cfop,
         cte_text_numero,
         cte_text_cv_uf,
         cte_text_cv_ano,
         cte_text_cv_mes,
         cte_text_cv_cnpj,
         cte_text_cv_mod,
         cte_text_cv_serie,
         cte_text_produto.

  IF zcte_info_nota-cfop IS NOT INITIAL.

    SELECT SINGLE * INTO wa_j_1bagt
      FROM j_1bagt
     WHERE spras EQ sy-langu
       AND cfop  EQ zcte_info_nota-cfop.

    IF sy-subrc IS INITIAL.
      cte_text_cfop = wa_j_1bagt-cfotxt.
    ENDIF.

  ENDIF.

  IF ( zcte_info_nota-material IS NOT INITIAL ) .
    SELECT SINGLE maktx INTO cte_text_produto
      FROM makt
     WHERE matnr EQ zcte_info_nota-material
       AND spras EQ sy-langu.
  ENDIF.

  IF ( zcte_info_nota-cliente IS NOT INITIAL ) AND ( ( cte_alterando EQ 'A' ) OR ( cte_alterando EQ 'I' ) ).

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = zcte_info_nota-cliente
        p_partype    = zcte_info_nota-partyp
      CHANGING
        wa_info_part = wa_info_k
        wa_info_c    = wa_info_c.

    zcte_info_nota-stcd1   = wa_info_k-stcd1.
    zcte_info_nota-stcd2   = wa_info_k-stcd2.
    zcte_info_nota-name1   = wa_info_k-name1.
    zcte_info_nota-pfisica = wa_info_k-stkzn.

  ENDIF.

  IF ( zcte_info_nota-pfisica IS INITIAL ) AND ( zcte_info_nota-stcd1 IS NOT INITIAL ).
    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
      EXPORTING
        input  = zcte_info_nota-stcd1
      IMPORTING
        output = cte_text_cliente_cgc.
  ELSEIF ( zcte_info_nota-pfisica IS NOT INITIAL ) AND ( zcte_info_nota-stcd2 IS NOT INITIAL ).
    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = zcte_info_nota-stcd2
      IMPORTING
        output = cte_text_cliente_cgc.
  ENDIF.

  cte_text_cliente_nome = zcte_info_nota-name1.

  IF zcte_info_nota-modelo IS NOT INITIAL.

    CALL FUNCTION 'DDIF_DOMA_GET'
      EXPORTING
        name          = 'J_1BMODEL'
        state         = 'A'
        langu         = sy-langu
      TABLES
        dd07v_tab     = it_doc_model
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    READ TABLE it_doc_model INTO wa_doc_text WITH KEY domvalue_l = zcte_info_nota-modelo.
    cte_text_modelo = wa_doc_text-ddtext.

    IF zcte_info_nota-modelo EQ '55'.
      zcte_info_nota-nfe = 'X'.
    ENDIF.

  ENDIF.

  IF zcte_info_nota-nfe IS NOT INITIAL.

    PERFORM completa_chave.

    LOOP AT SCREEN.
      IF ( screen-name EQ 'ZCTE_INFO_NOTA-NFNUM9' ) OR ( screen-name EQ 'ZCTE_INFO_NOTA-CDV' )
        OR ( screen-name EQ 'ZCTE_INFO_NOTA-CHAVE' ).
        screen-output = '1'.
        screen-input  = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSE.
    CLEAR: zcte_info_nota-docnum9,
           zcte_info_nota-cdv,
           zcte_info_nota-chave.

    LOOP AT SCREEN.
      IF ( screen-name EQ 'ZCTE_INFO_NOTA-NFNUM9' ) OR ( screen-name EQ 'ZCTE_INFO_NOTA-CDV' )
        OR ( screen-name EQ 'ZCTE_INFO_NOTA-CHAVE' ).
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

  IF zcte_info_nota-peso_fiscal IS INITIAL.
    zcte_info_nota-peso_fiscal = zcte_info_nota-quantidade.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_1102  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_INSERT_INF_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verificar_insert_inf_nf  USING  p_zcte_info_nota  TYPE zcte_info_nota
                                     p_validado        TYPE c.

  DATA: wa_info_c TYPE kna1,
        wa_info_p TYPE lfa1.

  DATA: regex   TYPE REF TO cl_abap_regex,
        matcher TYPE REF TO cl_abap_matcher,
        match   TYPE c LENGTH 1.

  CLEAR: p_validado.

  IF p_zcte_info_nota-modelo IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado o modelo da' 'nota fiscal!'.
    EXIT.
  ENDIF.

  IF p_zcte_info_nota-serie IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado a série da' 'nota fiscal!'.
    EXIT.
  ENDIF.

  IF p_zcte_info_nota-numero IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado o número da' 'nota fiscal!'.
    EXIT.
  ENDIF.

  IF p_zcte_info_nota-cliente IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado o cliente da' 'nota fiscal!'.
    EXIT.
  ENDIF.

  IF p_zcte_info_nota-dtemissao IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado a data de emissão da' 'nota fiscal!'.
    EXIT.
  ENDIF.

  IF p_zcte_info_nota-cfop IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado o CFOP da' 'nota fiscal!'.
    EXIT.
  ENDIF.

  IF p_zcte_info_nota-material IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado o Material da' 'nota fiscal!'.
    EXIT.
  ENDIF.

  IF p_zcte_info_nota-unidade IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado a Unidade do Material da' 'nota fiscal!'.
    EXIT.
  ENDIF.

  IF p_zcte_info_nota-quantidade IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado a Quantidade de Material da' 'nota fiscal!'.
    EXIT.
  ENDIF.

  IF p_zcte_info_nota-vl_produtos IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado o valor' 'dos produtos da nota fiscal!'.
    EXIT.
  ENDIF.

  IF p_zcte_info_nota-vl_nota_fiscal IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado o valor da nota fiscal!'.
    EXIT.
  ENDIF.

  IF p_zcte_info_nota-nfe IS NOT INITIAL.

    PERFORM completa_chave.

    IF p_zcte_info_nota-modelo NE '55'.
      MESSAGE i023 WITH 'Para NF-e modelo deve ser 55!'.
      EXIT.
    ENDIF.

    IF p_zcte_info_nota-docnum9 IS INITIAL.
      MESSAGE i023 WITH 'Deve ser informado o número' 'aleatório da nota fiscal!'.
      EXIT.
    ENDIF.

    IF p_zcte_info_nota-cdv IS INITIAL.
      MESSAGE i023 WITH 'Deve ser informado o dígito' 'verificador da nota fiscal!'.
      EXIT.
    ENDIF.

** Alteração validação para terceiros
    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = zcte_info_nota-cliente
        p_partype    = zcte_info_nota-partyp
      CHANGING
        wa_info_c    = wa_info_c
        wa_info_part = wa_info_p.

    IF wa_info_c IS NOT INITIAL.
      IF wa_info_c-txjcd+3(2) IS INITIAL.
        MESSAGE i023 WITH 'Cliente não possui domicílio' 'fiscal em seu cadastro!'.
        EXIT.
      ENDIF.

    ELSEIF wa_info_p IS NOT INITIAL.
      IF wa_info_p-txjcd+3(2) IS INITIAL.
        MESSAGE i023 WITH 'Cliente não possui domicílio' 'fiscal em seu cadastro!'.
        EXIT.
      ENDIF.

    ENDIF.

    CREATE OBJECT regex
      EXPORTING
        pattern = '[^0-9]'.

    matcher = regex->create_matcher( text = zcte_info_nota-numero ).
    match = matcher->match( ).

    IF match IS NOT INITIAL.
      MESSAGE i023 WITH 'Número da nota fiscal está inválido!'.
      EXIT.
    ENDIF.

  ENDIF.

  p_validado = 'X'.

ENDFORM.                    " VERIFICAR_INSERT_INF_NF

*&---------------------------------------------------------------------*
*&      Module  CTE_STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_status_0102 OUTPUT.

  CASE zcte_identifica-modal.
      "Rodoviário
    WHEN c_01.
      cte_dynnr_mod = cte_c_2001.
      "Aéreo
    WHEN c_02.
      cte_dynnr_mod = cte_c_2002.
      "Aquaviário
    WHEN c_03.
      cte_dynnr_mod = cte_c_2003.
      "Ferroviário
    WHEN c_04.
      cte_dynnr_mod = cte_c_2004.
      "Dutoviário
    WHEN c_05.
      cte_dynnr_mod = cte_c_2005.
  ENDCASE.

  IF zcte_identifica-modal EQ c_01.

    IF ( p_emite_ciot EQ space ).
      LOOP AT SCREEN.
        IF ( screen-name EQ 'TBTS05' ) OR ( screen-name EQ 'TBTS06' ).
          screen-active    = '0'.
          screen-invisible = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF cte_dynnr_ifn IS INITIAL.
      cte_dynnr_ifn  = cte_c_2101.
      cte_dynnr_ciot = cte_c_6001.
    ENDIF.

    CASE cte_dynnr_ifn.
      WHEN cte_c_2101.
        inf_trans_cte_tab-activetab = cte_tbts01.
      WHEN cte_c_2102.
        inf_trans_cte_tab-activetab = cte_tbts02.
      WHEN cte_c_2103.
        inf_trans_cte_tab-activetab = cte_tbts03.
      WHEN cte_c_2104.
        inf_trans_cte_tab-activetab = cte_tbts04.
      WHEN cte_c_2105.
        inf_trans_cte_tab-activetab = cte_tbts05.
      WHEN cte_c_2106.
        inf_trans_cte_tab-activetab = cte_tbts06.
    ENDCASE.

  ENDIF.

ENDMODULE.                 " CTE_STATUS_0102  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.

  IF zcte_identifica-modal EQ c_01.

    CHECK cte_alterado IS INITIAL.
    CHECK ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).

    CASE cte_ok_code.
      WHEN cte_tbts01.
        cte_dynnr_ifn = cte_c_2101.
      WHEN cte_tbts02.
        PERFORM seleciona_info_veiculos.
        cte_dynnr_ifn = cte_c_2102.
      WHEN cte_tbts03.
        cte_dynnr_ifn = cte_c_2103.
      WHEN cte_tbts04.
        PERFORM seleciona_info_motoristas.
        cte_dynnr_ifn = cte_c_2104.
      WHEN cte_tbts05.
        cte_dynnr_ifn = cte_c_2105.
      WHEN cte_tbts06.
        PERFORM seleciona_info_ciot.
        cte_dynnr_ifn = cte_c_2106.
      WHEN cte_back.
        IF cte_dynnr_ifn EQ cte_c_2102.
          cte_dynnr_ifn = cte_c_2101.
        ELSEIF cte_dynnr_ifn EQ cte_c_2104.
          cte_dynnr_ifn = cte_c_2103.
        ELSEIF cte_dynnr_ifn EQ cte_c_2106.
          cte_dynnr_ifn = cte_c_2105.
        ENDIF.
    ENDCASE.

  ENDIF.

ENDMODULE.                 " USER_COMMAND_0102  INPUT

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_INFO_VEICULOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_info_veiculos .

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  PERFORM cte_cria_trans_alv.

  CALL METHOD cte_alv_trans->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  IF it_selected_rows IS NOT INITIAL.
    READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
    READ TABLE it_cte_trans_alv INDEX wa_selected_rows-index INTO cte_trans_alv.
    MOVE-CORRESPONDING cte_trans_alv TO zcte_trans.
  ENDIF.

ENDFORM.                    " SELECIONA_INFO_VEICULOS

*&---------------------------------------------------------------------*
*&      Module  CTE_CRIA_TRANS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_cria_trans OUTPUT.

  PERFORM cte_cria_trans_alv.

  MOVE: it_cte_trans[] TO it_cte_trans_alv[].

  CALL METHOD cte_alv_trans->refresh_table_display.

ENDMODULE.                 " CTE_CRIA_TRANS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CTE_CRIA_TRANS_ALV
*&---------------------------------------------------------------------*
FORM cte_cria_trans_alv .

  CONSTANTS: tabela_trans TYPE string VALUE 'IT_CTE_TRANS_ALV'.

  IF cte_prim_trans IS INITIAL.

*   Create object for container
    CREATE OBJECT cte_container_trans
      EXPORTING
        container_name = 'CTE_TRANS'.

    CREATE OBJECT cte_alv_trans
      EXPORTING
        i_parent = cte_container_trans.

    PERFORM z_estrutura_fieldcat TABLES it_cte_catalog_trans USING:
        tabela_trans 'PC_VEICULO'     TEXT-c00 ' ' 01 07 space space,
        tabela_trans 'PROPRIETARIO'   TEXT-c01 ' ' 02 10 space 'ALPHA',
        tabela_trans 'CD_CIDADE'      TEXT-c02 ' ' 03 25 space space,
        tabela_trans 'CD_UF'          TEXT-c03 ' ' 04 03 space space,
        tabela_trans 'PROP_CNPJ'      TEXT-c04 ' ' 05 18 space 'CGCBR',
        tabela_trans 'PROP_CPF'       TEXT-c05 ' ' 06 14 space 'CPFBR',
        tabela_trans 'PROP_NOME'      TEXT-c06 ' ' 07 35 space space,
        tabela_trans 'PROP_RNTRC'     TEXT-c07 ' ' 08 10 space space,
        tabela_trans 'PROP_UF'        TEXT-c08 ' ' 09 03 space space,
        tabela_trans 'PROP_IE'        TEXT-c09 ' ' 10 18 space space.

    cte_gs_layout-zebra    = c_x.
    cte_gs_layout-sel_mode = space.

    CALL METHOD cte_alv_trans->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = cte_gs_layout
      CHANGING
        it_fieldcatalog = it_cte_catalog_trans
        it_outtab       = it_cte_trans_alv[].

    cte_prim_trans = c_x.

  ENDIF.

ENDFORM.                    " CTE_CRIA_TRANS_ALV

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2102 INPUT.

  DATA: answer_trans   TYPE c LENGTH 1,
        validado_trans TYPE c LENGTH 1.

  CASE cte_ok_code.
    WHEN cte_incluir.
      CLEAR: zcte_trans.
      zcte_trans-docnum = cte_j_1bnfdoc-docnum.
      CLEAR: cte_alterado.
      cte_alterando = 'I'.
    WHEN cte_alterar.
      CLEAR: cte_alterado.
      cte_alterando = 'A'.
    WHEN cte_apagar.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-004
        IMPORTING
          answer    = answer_trans.

      CASE answer_trans.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.

      DELETE FROM zcte_trans WHERE docnum EQ zcte_trans-docnum
                               AND pc_veiculo EQ zcte_trans-pc_veiculo.

      DELETE it_cte_trans WHERE docnum EQ zcte_trans-docnum
                            AND pc_veiculo EQ zcte_trans-pc_veiculo.

      CLEAR: zcte_trans.

    WHEN cte_confirma.
      PERFORM verificar_insert_trans USING zcte_trans validado_trans.
      IF validado_trans EQ c_x.
        MODIFY zcte_trans.

        IF cte_alterando EQ 'I'.
          MOVE-CORRESPONDING zcte_trans TO cte_trans_alv.
          APPEND cte_trans_alv TO it_cte_trans.
        ELSEIF cte_alterando EQ 'A'.
          READ TABLE it_cte_trans INTO cte_trans_alv WITH KEY docnum = zcte_trans-docnum
                                                              pc_veiculo = zcte_trans-pc_veiculo.
          MOVE-CORRESPONDING zcte_trans TO cte_trans_alv.
          MODIFY it_cte_trans INDEX sy-tabix FROM cte_trans_alv.
        ENDIF.

        cte_alterando = 'C'.
        CLEAR: cte_alterado.
      ENDIF.
    WHEN cte_cancelar.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-002
          textline2 = TEXT-003
        IMPORTING
          answer    = answer_trans.

      CASE answer_trans.
        WHEN 'J'.
          CLEAR: cte_alterado, cte_alterado.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.

      READ TABLE it_cte_trans INTO cte_trans_alv WITH KEY docnum     = zcte_trans-docnum
                                                          pc_veiculo = zcte_trans-pc_veiculo.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING cte_trans_alv TO zcte_trans.
      ELSE.
        CLEAR: zcte_trans.
      ENDIF.
      cte_alterando = 'C'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_2102  INPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_INSERT_TRANS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZCTE_TRANS  text
*      -->P_VALIDADO_TRANS  text
*----------------------------------------------------------------------*
FORM verificar_insert_trans  USING    p_zcte_trans TYPE zcte_trans
                                      p_validado   TYPE c.

  CLEAR: p_validado.

  IF p_zcte_trans-pc_veiculo IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado a placa do veículo!'.
    EXIT.
  ENDIF.

  IF p_zcte_trans-cd_renavam IS INITIAL.
    MESSAGE i023 WITH 'Ajustar cadastro de veículo' 'Informar RENAVAM!'.
    EXIT.
  ENDIF.

  IF p_zcte_trans-tara IS INITIAL.
    MESSAGE i023 WITH 'Ajustar cadastro de veículo' 'Informar Peso Tara!'.
    EXIT.
  ENDIF.

  IF p_zcte_trans-cap_kg IS INITIAL.
    MESSAGE i023 WITH 'Ajustar cadastro de veículo' 'Informar Capacidade em KG!'.
    EXIT.
  ENDIF.

  IF p_zcte_trans-cap_m3 IS INITIAL.
    MESSAGE i023 WITH 'Ajustar cadastro de veículo' 'Informar Capacidade em M3!'.
    EXIT.
  ENDIF.

  IF p_zcte_trans-tp_veiculo IS INITIAL.
    MESSAGE i023 WITH 'Ajustar cadastro de veículo' 'Informar Tipo de veículo!'.
    EXIT.
  ENDIF.

  IF p_zcte_trans-tp_rodado IS INITIAL.
    MESSAGE i023 WITH 'Ajustar cadastro de veículo' 'Informar Tipo de Rodado!'.
    EXIT.
  ENDIF.

  IF p_zcte_trans-tp_carroceria2 IS INITIAL.
    MESSAGE i023 WITH 'Ajustar cadastro de veículo' 'Informar Tipo de Carroceria!'.
    EXIT.
  ENDIF.

  IF ( p_zcte_trans-prop_cnpj IS INITIAL ) AND ( p_zcte_trans-prop_cpf IS INITIAL ).
    MESSAGE i023 WITH 'Ajustar cadastro de fornecedor' 'Informar CNPF/CPF!'.
    EXIT.
  ENDIF.

  IF p_zcte_trans-prop_nome IS INITIAL.
    MESSAGE i023 WITH 'Ajustar cadastro de fornecedor' 'Informar o Nome!'.
    EXIT.
  ENDIF.

  IF p_zcte_trans-prop_rntrc IS INITIAL.
    MESSAGE i023 WITH 'Ajustar cadastro de fornecedor' 'Informar o RNTRC!'.
    EXIT.
  ENDIF.

  IF p_zcte_trans-prop_uf IS INITIAL.
    MESSAGE i023 WITH 'Ajustar cadastro de fornecedor' 'Informar o Domicílio Fiscal!'.
    EXIT.
  ENDIF.

  READ TABLE it_cte_trans WITH KEY docnum     = zcte_trans-docnum
                                   pc_veiculo = zcte_trans-pc_veiculo.
  IF sy-subrc IS INITIAL.
    MESSAGE i023 WITH 'Placa já cadastrada para este transporte!'.
    EXIT.
  ENDIF.

  p_validado = 'X'.

ENDFORM.                    " VERIFICAR_INSERT_TRANS

*&---------------------------------------------------------------------*
*&      Module  STATUS_2102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2102 OUTPUT.

  DATA: wa_zlest0002 TYPE zlest0002.

  IF ( zcte_trans-pc_veiculo IS NOT INITIAL ) AND ( ( cte_alterando EQ 'A' ) OR ( cte_alterando EQ 'I' ) ).

    CLEAR: zcte_trans-proprietario,
           zcte_trans-cd_cidade,
           zcte_trans-cd_uf,
           zcte_trans-agregado,
           zcte_trans-cd_renavam,
           zcte_trans-tp_veiculo,
           zcte_trans-tp_rodado,
           zcte_trans-tp_carroceria2,
           zcte_trans-tara,
           zcte_trans-cap_kg,
           zcte_trans-cap_m3,
           zcte_trans-prop_cnpj,
           zcte_trans-prop_cpf,
           zcte_trans-prop_nome,
           zcte_trans-prop_rntrc,
           zcte_trans-prop_uf,
           zcte_trans-prop_ie,
           cte_text_cliente_cgc.

    SELECT SINGLE * INTO wa_zlest0002
      FROM zlest0002
     WHERE pc_veiculo EQ zcte_trans-pc_veiculo.

    IF sy-subrc IS INITIAL.

      MOVE-CORRESPONDING wa_zlest0002 TO zcte_trans.

      IF zcte_trans-proprietario IS NOT INITIAL.

        CALL FUNCTION 'Z_PARCEIRO_INFO'
          EXPORTING
            p_parceiro   = zcte_trans-proprietario
            p_partype    = 'V'
          CHANGING
            wa_info_part = wa_info_part.

        zcte_trans-prop_nome  = wa_info_part-name1.
        zcte_trans-prop_rntrc = wa_info_part-bahns.
        zcte_trans-prop_ie    = wa_info_part-stcd3.
        zcte_trans-pfisica    = wa_info_part-stkzn.

        IF zcte_trans-pfisica IS INITIAL.
          zcte_trans-prop_cnpj  = wa_info_part-stcd1.
        ELSE.
          zcte_trans-prop_cpf   = wa_info_part-stcd2.
        ENDIF.

        IF wa_info_part-txjcd IS NOT INITIAL.
          zcte_trans-prop_uf = wa_info_part-txjcd(2).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ( zcte_trans-pfisica IS INITIAL ) AND ( zcte_trans-prop_cnpj IS NOT INITIAL ).
    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
      EXPORTING
        input  = zcte_trans-prop_cnpj
      IMPORTING
        output = cte_text_cliente_cgc.

  ELSEIF ( zcte_trans-pfisica IS NOT INITIAL ) AND ( zcte_trans-prop_cpf IS NOT INITIAL ).
    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = zcte_trans-prop_cpf
      IMPORTING
        output = cte_text_cliente_cgc.

  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_2102  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  COMPLETA_CHAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM completa_chave .

  DATA: wa_info_c    TYPE kna1,
        wa_info_part TYPE lfa1,
        t_kna1       TYPE TABLE OF kna1 WITH HEADER LINE,
        t_lfa1       TYPE TABLE OF lfa1 WITH HEADER LINE,
        p_stcd1      TYPE j_1bstcd1,
        vg_active    TYPE j_1bnfe_active.

  IF ( zcte_info_nota-chave IS NOT INITIAL ) AND ( zcte_info_nota-cliente IS INITIAL ).

    zcte_info_nota-stcd1   = zcte_info_nota-chave+06(14).
    zcte_info_nota-modelo  = zcte_info_nota-chave+20(2).
    zcte_info_nota-serie   = zcte_info_nota-chave+22(3).
    zcte_info_nota-numero  = zcte_info_nota-chave+25(9).
    zcte_info_nota-docnum9 = zcte_info_nota-chave+34(9).
    zcte_info_nota-cdv     = zcte_info_nota-chave+43(1).

    p_stcd1 = zcte_info_nota-chave+06(14).

    IF zcte_info_nota-partyp NE 'V'.

      CALL FUNCTION 'Z_PARCEIRO_CNPJ'
        EXPORTING
          p_stcd1        = p_stcd1
          p_verifica_cad = 'X'
        TABLES
          t_kna1         = t_kna1.

      IF t_kna1[] IS NOT INITIAL.
        READ TABLE t_kna1 INDEX 1.
        zcte_info_nota-cliente = t_kna1-kunnr.
      ENDIF.

    ELSE.

      CALL FUNCTION 'Z_PARCEIRO_CNPJ'
        EXPORTING
          p_stcd1        = p_stcd1
          p_verifica_cad = 'X'
          p_partyp       = 'X'
        TABLES
          t_lfa1         = t_lfa1.

      IF t_lfa1[] IS NOT INITIAL.
        READ TABLE t_lfa1 INDEX 1.
        zcte_info_nota-cliente = t_lfa1-lifnr.
      ENDIF.

    ENDIF.

  ENDIF.

  IF zcte_info_nota-cliente IS NOT INITIAL.

    CALL FUNCTION 'Z_PARCEIRO_INFO'
      EXPORTING
        p_parceiro   = zcte_info_nota-cliente
        p_partype    = zcte_info_nota-partyp
      CHANGING
        wa_info_part = wa_info_part
        wa_info_c    = wa_info_c.

    IF zcte_info_nota-partyp EQ 'V'.
      cte_text_cliente_nome = wa_info_part-name1.
      IF ( wa_info_part-stkzn IS INITIAL ) AND ( wa_info_part-stcd1 IS NOT INITIAL ).

        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = wa_info_part-stcd1
          IMPORTING
            output = cte_text_cliente_cgc.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_info_part-stcd1
          IMPORTING
            output = cte_text_cv_cnpj.

        zcte_info_nota-stcd1 = wa_info_part-stcd1.

      ELSEIF ( wa_info_part-stkzn IS NOT INITIAL ) AND ( wa_info_part-stcd2 IS NOT INITIAL ).

        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            input  = wa_info_part-stcd2
          IMPORTING
            output = cte_text_cliente_cgc.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_info_part-stcd2
          IMPORTING
            output = cte_text_cv_cnpj.

        zcte_info_nota-stcd1 = wa_info_part-stcd2.

      ENDIF.
      IF wa_info_part-txjcd IS NOT INITIAL.
        cte_text_cv_uf = wa_info_part-txjcd+3(2).
      ENDIF.

    ELSE.
      cte_text_cliente_nome = wa_info_c-name1.
      IF ( wa_info_c-stkzn IS INITIAL ) AND ( wa_info_c-stcd1 IS NOT INITIAL ).

        CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
          EXPORTING
            input  = wa_info_c-stcd1
          IMPORTING
            output = cte_text_cliente_cgc.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_info_c-stcd1
          IMPORTING
            output = cte_text_cv_cnpj.

        zcte_info_nota-stcd1 = wa_info_c-stcd1.

      ELSEIF ( wa_info_c-stkzn IS NOT INITIAL ) AND ( wa_info_c-stcd2 IS NOT INITIAL ).

        CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
          EXPORTING
            input  = wa_info_c-stcd2
          IMPORTING
            output = cte_text_cliente_cgc.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_info_c-stcd2
          IMPORTING
            output = cte_text_cv_cnpj.

        zcte_info_nota-stcd1 = wa_info_c-stcd2.

      ENDIF.
      IF wa_info_c-txjcd IS NOT INITIAL.
        cte_text_cv_uf = wa_info_c-txjcd+3(2).
      ENDIF.

    ENDIF.

  ENDIF.

  zcte_info_nota-modelo = '55'.

  IF zcte_info_nota-dtemissao IS NOT INITIAL.
    cte_text_cv_ano = zcte_info_nota-dtemissao+2(2).
    cte_text_cv_mes = zcte_info_nota-dtemissao+4(2).
  ENDIF.

  IF zcte_info_nota-modelo IS NOT INITIAL.
    WRITE zcte_info_nota-modelo TO cte_text_cv_mod.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = cte_text_cv_mod
      IMPORTING
        output = cte_text_cv_mod.
  ENDIF.

  IF zcte_info_nota-serie IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = zcte_info_nota-serie
      IMPORTING
        output = cte_text_cv_serie.
  ENDIF.

  IF zcte_info_nota-numero IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = zcte_info_nota-numero
      IMPORTING
        output = cte_text_numero.
  ENDIF.

  IF ( NOT zcte_info_nota-docnum_nf IS INITIAL ) AND ( NOT zcte_info_nota-nfe IS INITIAL ).

    SELECT SINGLE * INTO vg_active
      FROM j_1bnfe_active
     WHERE docnum EQ zcte_info_nota-docnum_nf.

    IF sy-subrc IS INITIAL.

      CONCATENATE vg_active-regio
                  vg_active-nfyear
                  vg_active-nfmonth
                  vg_active-stcd1
                  vg_active-model
                  vg_active-serie
                  vg_active-nfnum9
                  vg_active-docnum9
                  vg_active-cdv INTO zcte_info_nota-chave.
      cte_text_cv_uf = vg_active-regio.

    ENDIF.
  ELSE.
    sy-subrc = 4.
  ENDIF.

  IF ( zcte_info_nota-stcd1 IS NOT INITIAL ) AND ( NOT sy-subrc IS INITIAL ).
    CONCATENATE cte_text_cv_uf       cte_text_cv_ano        cte_text_cv_mes
                zcte_info_nota-stcd1 zcte_info_nota-modelo  cte_text_cv_serie
                cte_text_numero      zcte_info_nota-docnum9 zcte_info_nota-cdv
           INTO zcte_info_nota-chave.
  ENDIF.

ENDFORM.                    " COMPLETA_CHAVE

*&---------------------------------------------------------------------*
*&      Module  CTE_CRIA_CIOT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_cria_ciot OUTPUT.

  PERFORM cte_cria_ciot_alv.

  CLEAR: it_cte_ciot_alv[].

  LOOP AT it_cte_ciot INTO cte_ciot.

    CLEAR: cte_ciot_alv.
    MOVE-CORRESPONDING cte_ciot TO cte_ciot_alv.

    CALL FUNCTION 'Z_SD_ICON_STATUS_CIOT'
      EXPORTING
        p_st_ciot = cte_ciot_alv-st_ciot
      IMPORTING
        icone     = cte_ciot_alv-si_ciot
      CHANGING
        texto     = vg_tx_status_ciot.

    READ TABLE cte_ciot_parceiros INTO DATA(wa_ciot_parceiros) WITH KEY cd_ciot = cte_ciot_alv-cd_ciot tipo = '06'.
    IF sy-subrc IS INITIAL.
      cte_ciot_alv-pc_codigo      = wa_ciot_parceiros-codigo.
      cte_ciot_alv-pc_nome        = wa_ciot_parceiros-nome.
      cte_ciot_alv-pc_razao       = wa_ciot_parceiros-razao.
      cte_ciot_alv-pc_cnpj        = wa_ciot_parceiros-cnpj.
      cte_ciot_alv-pc_cpf         = wa_ciot_parceiros-cpf.
      cte_ciot_alv-pc_logradouro  = wa_ciot_parceiros-logradouro.
      cte_ciot_alv-pc_numero      = wa_ciot_parceiros-numero.
      cte_ciot_alv-pc_complemento = wa_ciot_parceiros-complemento.
      cte_ciot_alv-pc_bairro      = wa_ciot_parceiros-bairro.
      cte_ciot_alv-pc_uf          = wa_ciot_parceiros-uf.
      cte_ciot_alv-pc_municipio   = wa_ciot_parceiros-municipio.
      cte_ciot_alv-pc_cep         = wa_ciot_parceiros-cep.
      cte_ciot_alv-pc_fone        = wa_ciot_parceiros-fone.
    ENDIF.

    READ TABLE cte_ciot_parceiros INTO wa_ciot_parceiros WITH KEY cd_ciot = cte_ciot_alv-cd_ciot tipo = '07'.
    IF sy-subrc IS INITIAL.
      cte_ciot_alv-le_codigo      = wa_ciot_parceiros-codigo.
      cte_ciot_alv-le_nome        = wa_ciot_parceiros-nome.
      cte_ciot_alv-le_razao       = wa_ciot_parceiros-razao.
      cte_ciot_alv-le_cnpj        = wa_ciot_parceiros-cnpj.
      cte_ciot_alv-le_cpf         = wa_ciot_parceiros-cpf.
      cte_ciot_alv-le_logradouro  = wa_ciot_parceiros-logradouro.
      cte_ciot_alv-le_numero      = wa_ciot_parceiros-numero.
      cte_ciot_alv-le_complemento = wa_ciot_parceiros-complemento.
      cte_ciot_alv-le_bairro      = wa_ciot_parceiros-bairro.
      cte_ciot_alv-le_uf          = wa_ciot_parceiros-uf.
      cte_ciot_alv-le_municipio   = wa_ciot_parceiros-municipio.
      cte_ciot_alv-le_cep         = wa_ciot_parceiros-cep.
      cte_ciot_alv-le_fone        = wa_ciot_parceiros-fone.
    ENDIF.

    APPEND cte_ciot_alv TO it_cte_ciot_alv.

  ENDLOOP.

  CALL METHOD cte_alv_ciot->refresh_table_display.
  CALL METHOD cte_alv_cioto->refresh_table_display.

ENDMODULE.                 " CTE_CRIA_CIOT  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CTE_CRIA_CIOT_ALV
*&---------------------------------------------------------------------*
FORM cte_cria_ciot_alv .

  CONSTANTS: tabela_ciot  TYPE string VALUE 'IT_CTE_CIOT_ALV',
             tabela_cioto TYPE string VALUE 'IT_PRT_CIOT'.

  IF cte_prim_ciot IS INITIAL.

*   Create object for container
    CREATE OBJECT cte_container_ciot
      EXPORTING
        container_name = 'CTE_CIOT'.

    CREATE OBJECT cte_container_cioto
      EXPORTING
        container_name = 'CTE_CIOT_OBS'.

    CREATE OBJECT cte_alv_ciot
      EXPORTING
        i_parent = cte_container_ciot.

    CREATE OBJECT cte_alv_cioto
      EXPORTING
        i_parent = cte_container_cioto.

    PERFORM z_estrutura_fieldcat TABLES it_cte_catalog_ciot USING:
        tabela_ciot 'NR_CIOT'           TEXT-o00 ' ' 01 12 space 'ALPHA',
        tabela_ciot 'SI_CIOT'           TEXT-o22 'X' 02 02 space space,
        tabela_ciot 'RNTRC'             TEXT-o01 ' ' 03 10 space space,
        tabela_ciot 'NUCONTRATO'        TEXT-025 ' ' 04 13 space 'ALPHA',
        tabela_ciot 'UF_ORIGEM'         TEXT-o19 ' ' 05 02 space space,
        tabela_ciot 'MUNICIPIO_ORIGEM'  TEXT-o02 ' ' 06 07 space space,
        tabela_ciot 'DT_ORIGEM'         TEXT-o23 ' ' 07 10 space space,
        tabela_ciot 'UF_TERMIN'         TEXT-o20 ' ' 08 02 space space,
        tabela_ciot 'MUNICIPIO_TERMIN'  TEXT-o21 ' ' 09 07 space space,
        tabela_ciot 'DT_TERMIN'         TEXT-o24 ' ' 10 10 space space,
        tabela_ciot 'QUANTIDADE'        TEXT-o03 ' ' 11 16 space space,
        tabela_ciot 'UNIDADE'           TEXT-o04 ' ' 12 03 space space,
        tabela_ciot 'PROP_RNTRC'        TEXT-o07 ' ' 13 10 space space,
        tabela_ciot 'VLR_FRETE'         TEXT-o08 ' ' 14 15 space space,
        tabela_ciot 'VLR_ADIANTAMENTO'  TEXT-o09 ' ' 15 15 space space,
        tabela_ciot 'VLR_SALDO'         TEXT-o10 ' ' 16 15 space space,
        tabela_ciot 'VLR_IMPOSTOS'      TEXT-o11 ' ' 17 15 space space,
        tabela_ciot 'VLR_PEDAGIO'       TEXT-o12 ' ' 18 15 space space,
        tabela_ciot 'PERC_TOLERANCIA'   TEXT-o13 ' ' 19 15 space space,
        tabela_ciot 'VLR_UNIT_MERC'     TEXT-o14 ' ' 20 15 space space,
        tabela_ciot 'VLR_UNIT_FRETE'    TEXT-o15 ' ' 21 15 space space,
        tabela_ciot 'NR_BC_BANCO'       TEXT-o16 ' ' 22 15 space space,
        tabela_ciot 'NR_BC_AGENCIA'     TEXT-o17 ' ' 23 15 space space,
        tabela_ciot 'NR_BC_CONTA'       TEXT-o18 ' ' 24 15 space space,
        tabela_ciot 'DISTANCIA'         TEXT-o25 ' ' 25 10 space space.

    PERFORM z_estrutura_fieldcat TABLES it_cte_catalog_cioto USING:
        tabela_ciot 'DS_PROTOCOLO'      'Observações' ' ' 01 100 space ''.

    cte_gs_layout-zebra    = c_x.
    cte_gs_layout-sel_mode = space.

    CALL METHOD cte_alv_ciot->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = cte_gs_layout
      CHANGING
        it_fieldcatalog = it_cte_catalog_ciot
        it_outtab       = it_cte_ciot_alv[].

    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_hotspot_click FOR cte_alv_ciot.

    cte_gs_layout-no_toolbar = abap_true.

    CALL METHOD cte_alv_cioto->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = cte_gs_layout
      CHANGING
        it_fieldcatalog = it_cte_catalog_cioto
        it_outtab       = it_prt_ciot[].

    cte_prim_ciot = c_x.

  ENDIF.

ENDFORM.                    " CTE_CRIA_CIOT_ALV

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_INFO_CIOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_info_ciot .

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  PERFORM cte_cria_ciot_alv.

  CALL METHOD cte_alv_ciot->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  IF it_selected_rows IS NOT INITIAL.
    READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
    READ TABLE it_cte_ciot_alv INDEX wa_selected_rows-index INTO cte_ciot_alv.
    MOVE-CORRESPONDING cte_ciot_alv TO zcte_ciot.
  ENDIF.

ENDFORM.                    " SELECIONA_INFO_CIOT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0104 INPUT.

  CASE cte_ok_code.
    WHEN cte_alterar.
      CLEAR: cte_alterado.
      cte_alterando = 'A'.
      MOVE-CORRESPONDING zcte_parceiros TO copia_parceiros.
    WHEN cte_cancelar.
      CLEAR: cte_alterado.
      cte_alterando = 'C'.
      MOVE-CORRESPONDING copia_parceiros TO zcte_parceiros.
    WHEN cte_confirma.
      CLEAR: cte_alterado.
      MODIFY zcte_parceiros FROM zcte_parceiros.
      cte_alterando = 'C'.
    WHEN cte_btn01.
      PERFORM copia_parceiro USING '1'.
      CALL SCREEN 4101 STARTING AT 10 10 ENDING AT 73 25.
    WHEN cte_btn02.
      PERFORM copia_parceiro USING '2'.
      CALL SCREEN 4101 STARTING AT 10 10 ENDING AT 73 25.
    WHEN cte_btn03.
      PERFORM copia_parceiro USING '3'.
      CALL SCREEN 4101 STARTING AT 10 10 ENDING AT 73 25.
    WHEN cte_btn04.
      PERFORM copia_parceiro USING '4'.
      CALL SCREEN 4101 STARTING AT 10 10 ENDING AT 73 25.
    WHEN cte_btn05.
      PERFORM copia_parceiro USING '5'.
      CALL SCREEN 4101 STARTING AT 10 10 ENDING AT 73 25.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0104  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_status_0104 OUTPUT.

  LOOP AT SCREEN.

    IF ( cte_alterando EQ 'A' ).
      IF screen-group1 EQ 'INS'.
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).
      IF ( screen-group1 EQ 'INS' ) OR ( screen-group1 EQ 'ALT' ).
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDMODULE.                 " CTE_STATUS_0104  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  COPIA_PARCEIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4020   text
*----------------------------------------------------------------------*
FORM copia_parceiro  USING tp TYPE c.

  CASE tp.
    WHEN c_1.
      wa_cte_parceiros-codigo  = zcte_parceiros-emit_codigo.
      wa_cte_parceiros-cnpj    = zcte_parceiros-emit_cnpj.
      wa_cte_parceiros-ie      = zcte_parceiros-emit_ie.
      wa_cte_parceiros-xnome   = zcte_parceiros-emit_xnome.
      wa_cte_parceiros-xfant   = zcte_parceiros-emit_xfant.
      wa_cte_parceiros-xlgr    = zcte_parceiros-emit_xlgr.
      wa_cte_parceiros-nro     = zcte_parceiros-emit_nro.
      wa_cte_parceiros-xcpl    = zcte_parceiros-emit_xcpl.
      wa_cte_parceiros-xbairro = zcte_parceiros-emit_xbairro.
      wa_cte_parceiros-cmun    = zcte_parceiros-emit_cmun.
      wa_cte_parceiros-xmun    = zcte_parceiros-emit_xmun.
      wa_cte_parceiros-cep     = zcte_parceiros-emit_cep.
      wa_cte_parceiros-uf      = zcte_parceiros-emit_uf.
    WHEN c_2.
      wa_cte_parceiros-codigo  = zcte_parceiros-reme_codigo.
      wa_cte_parceiros-cnpj    = zcte_parceiros-reme_cnpj.
      wa_cte_parceiros-ie      = zcte_parceiros-reme_ie.
      wa_cte_parceiros-xnome   = zcte_parceiros-reme_xnome.
      wa_cte_parceiros-xfant   = zcte_parceiros-reme_xfant.
      wa_cte_parceiros-xlgr    = zcte_parceiros-reme_xlgr.
      wa_cte_parceiros-nro     = zcte_parceiros-reme_nro.
      wa_cte_parceiros-xcpl    = zcte_parceiros-reme_xcpl.
      wa_cte_parceiros-xbairro = zcte_parceiros-reme_xbairro.
      wa_cte_parceiros-cmun    = zcte_parceiros-reme_cmun.
      wa_cte_parceiros-xmun    = zcte_parceiros-reme_xmun.
      wa_cte_parceiros-cep     = zcte_parceiros-reme_cep.
      wa_cte_parceiros-uf      = zcte_parceiros-reme_uf.
    WHEN c_3.
      wa_cte_parceiros-codigo  = zcte_parceiros-dest_codigo.
      wa_cte_parceiros-cnpj    = zcte_parceiros-dest_cnpj.
      wa_cte_parceiros-ie      = zcte_parceiros-dest_ie.
      wa_cte_parceiros-xnome   = zcte_parceiros-dest_xnome.
      wa_cte_parceiros-xfant   = zcte_parceiros-dest_xfant.
      wa_cte_parceiros-xlgr    = zcte_parceiros-dest_xlgr.
      wa_cte_parceiros-nro     = zcte_parceiros-dest_nro.
      wa_cte_parceiros-xcpl    = zcte_parceiros-dest_xcpl.
      wa_cte_parceiros-xbairro = zcte_parceiros-dest_xbairro.
      wa_cte_parceiros-cmun    = zcte_parceiros-dest_cmun.
      wa_cte_parceiros-xmun    = zcte_parceiros-dest_xmun.
      wa_cte_parceiros-cep     = zcte_parceiros-dest_cep.
      wa_cte_parceiros-uf      = zcte_parceiros-dest_uf.
    WHEN c_4.
      wa_cte_parceiros-codigo  = zcte_parceiros-cole_codigo.
      wa_cte_parceiros-cnpj    = zcte_parceiros-cole_cnpj.
      wa_cte_parceiros-ie      = zcte_parceiros-cole_ie.
      wa_cte_parceiros-xnome   = zcte_parceiros-cole_xnome.
      wa_cte_parceiros-xfant   = zcte_parceiros-cole_xfant.
      wa_cte_parceiros-xlgr    = zcte_parceiros-cole_xlgr.
      wa_cte_parceiros-nro     = zcte_parceiros-cole_nro.
      wa_cte_parceiros-xcpl    = zcte_parceiros-cole_xcpl.
      wa_cte_parceiros-xbairro = zcte_parceiros-cole_xbairro.
      wa_cte_parceiros-cmun    = zcte_parceiros-cole_cmun.
      wa_cte_parceiros-xmun    = zcte_parceiros-cole_xmun.
      wa_cte_parceiros-cep     = zcte_parceiros-cole_cep.
      wa_cte_parceiros-uf      = zcte_parceiros-cole_uf.
    WHEN c_5.
      wa_cte_parceiros-codigo  = zcte_parceiros-toma_codigo.
      wa_cte_parceiros-cnpj    = zcte_parceiros-toma_cnpj.
      wa_cte_parceiros-ie      = zcte_parceiros-toma_ie.
      wa_cte_parceiros-xnome   = zcte_parceiros-toma_xnome.
      wa_cte_parceiros-xfant   = zcte_parceiros-toma_xfant.
      wa_cte_parceiros-xlgr    = zcte_parceiros-toma_xlgr.
      wa_cte_parceiros-nro     = zcte_parceiros-toma_nro.
      wa_cte_parceiros-xcpl    = zcte_parceiros-toma_xcpl.
      wa_cte_parceiros-xbairro = zcte_parceiros-toma_xbairro.
      wa_cte_parceiros-cmun    = zcte_parceiros-toma_cmun.
      wa_cte_parceiros-xmun    = zcte_parceiros-toma_xmun.
      wa_cte_parceiros-cep     = zcte_parceiros-toma_cep.
      wa_cte_parceiros-uf      = zcte_parceiros-toma_uf.
  ENDCASE.

ENDFORM.                    " COPIA_PARCEIRO

*&---------------------------------------------------------------------*
*&      Module  STATUS_4101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_4101 OUTPUT.

  SET TITLEBAR  'TLPARCEIRO'.
  SET PF-STATUS 'PFPARCEIRO'.

ENDMODULE.                 " STATUS_4101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_STATUS_0105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_status_0105 OUTPUT.

  LOOP AT SCREEN.

    IF ( cte_alterando EQ 'A' ).
      IF screen-group1 EQ 'INS'.
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).
      IF ( screen-group1 EQ 'INS' ) OR ( screen-group1 EQ 'ALT' ).
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDMODULE.                 " CTE_STATUS_0105  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0105  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0105 INPUT.

  CASE cte_ok_code.
    WHEN cte_alterar.
      CLEAR: cte_alterado.
      cte_alterando = 'A'.
      MOVE-CORRESPONDING zcte_identifica TO copia_identifica.
    WHEN cte_cancelar.
      CLEAR: cte_alterado.
      cte_alterando = 'C'.
      MOVE-CORRESPONDING copia_identifica TO zcte_identifica.
    WHEN cte_confirma.
      CLEAR: cte_alterado.
      zcte_identifica-vrec = zcte_identifica-vtprest.
      cte_alterando = 'C'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0105  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_MUDA_REMETENTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_muda_remetente INPUT.

  DATA: cidade    TYPE j_1btxjurt,
        wa_lfa1_m TYPE lfa1,
        wa_adrc_m TYPE adrc.

  CLEAR: zcte_parceiros-reme_cnpj   ,
         zcte_parceiros-reme_ie     ,
         zcte_parceiros-reme_xnome  ,
         zcte_parceiros-reme_xfant  ,
         zcte_parceiros-reme_xlgr   ,
         zcte_parceiros-reme_nro    ,
         zcte_parceiros-reme_xbairro,
         zcte_parceiros-reme_cmun   ,
         zcte_parceiros-reme_cep    ,
         zcte_parceiros-reme_uf     ,
         zcte_parceiros-reme_fone   .

  "Busca Remetente de Mercadoria
  SELECT SINGLE * INTO wa_lfa1_m
    FROM lfa1
   WHERE lifnr EQ zcte_parceiros-reme_codigo.

  IF sy-subrc IS INITIAL.

    SELECT SINGLE * INTO wa_adrc_m
      FROM adrc
     WHERE addrnumber = wa_lfa1_m-adrnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = zcte_parceiros-reme_codigo
      IMPORTING
        output = zcte_parceiros-reme_codigo.

    zcte_parceiros-reme_cnpj    = wa_lfa1_m-stcd1.
    zcte_parceiros-reme_ie      = wa_lfa1_m-stcd3.
    zcte_parceiros-reme_xnome   = wa_lfa1_m-name1.
    zcte_parceiros-reme_xfant   = wa_lfa1_m-name1.
    zcte_parceiros-reme_xlgr    = wa_adrc_m-street(25).
    IF wa_adrc_m-house_num1 IS INITIAL.
      zcte_parceiros-reme_nro     = c_sn.
    ELSE.
      zcte_parceiros-reme_nro     = wa_adrc_m-house_num1.
    ENDIF.
    "p_parceiros-emit_xcpl    = emitente-.
    zcte_parceiros-reme_xbairro = wa_adrc_m-city2.
    zcte_parceiros-reme_cmun    = wa_adrc_m-taxjurcode+3(7).

    SELECT SINGLE * INTO cidade
      FROM j_1btxjurt
     WHERE spras      = wa_adrc_m-langu
       AND country    = wa_adrc_m-country
       AND taxjurcode = wa_adrc_m-taxjurcode.

    IF sy-subrc IS INITIAL.
      zcte_parceiros-reme_xmun    = cidade-text.
    ENDIF.
    zcte_parceiros-reme_cep     = wa_adrc_m-post_code1.
    zcte_parceiros-reme_uf      = wa_adrc_m-region.
    zcte_parceiros-reme_fone    = wa_adrc_m-tel_number.
  ENDIF.

ENDMODULE.                 " CTE_MUDA_REMETENTE  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_MUDA_DESTINATARIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_muda_destinatario INPUT.

  DATA: emitente_d TYPE zctet_endereco,
        cidade_d   TYPE j_1btxjurt.

  CLEAR: zcte_parceiros-dest_cnpj   ,
         zcte_parceiros-dest_ie     ,
         zcte_parceiros-dest_xnome  ,
         zcte_parceiros-dest_xfant  ,
         zcte_parceiros-dest_xlgr   ,
         zcte_parceiros-dest_nro    ,
         zcte_parceiros-dest_xbairro,
         zcte_parceiros-dest_cmun   ,
         zcte_parceiros-dest_cep    ,
         zcte_parceiros-dest_uf     ,
         zcte_parceiros-dest_fone   .

  "Busca Remetente de Mercadoria
  SELECT SINGLE a~name1   j~stcd3      j~stcd1  j~stcd2
                a~city2   a~post_code1 a~street a~house_num1
                a~country a~langu      a~region a~taxjurcode
                a~tel_number
           INTO emitente_d
    FROM kna1 AS j
   INNER JOIN adrc AS a ON a~addrnumber = j~adrnr
   WHERE kunnr = zcte_parceiros-dest_codigo.

  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = zcte_parceiros-dest_codigo
      IMPORTING
        output = zcte_parceiros-dest_codigo.

    zcte_parceiros-dest_cnpj    = emitente_d-stcd1.
    zcte_parceiros-dest_ie      = emitente_d-state_insc.
    zcte_parceiros-dest_xnome   = emitente_d-name1.
    zcte_parceiros-dest_xfant   = emitente_d-name1.
    zcte_parceiros-dest_xlgr    = emitente_d-street(25).
    IF emitente_d-house_num1 IS INITIAL.
      zcte_parceiros-dest_nro     = c_sn.
    ELSE.
      zcte_parceiros-dest_nro     = emitente_d-house_num1.
    ENDIF.
    "p_parceiros-emit_xcpl    = emitente-.
    zcte_parceiros-dest_xbairro = emitente_d-city2.
    zcte_parceiros-dest_cmun    = emitente_d-taxjurcode+3(7).

    SELECT SINGLE * INTO cidade_d
      FROM j_1btxjurt
     WHERE spras      = emitente_d-langu
       AND country    = emitente_d-country
       AND taxjurcode = emitente_d-taxjurcode.

    IF sy-subrc IS INITIAL.
      zcte_parceiros-dest_xmun    = cidade_d-text.
    ENDIF.
    zcte_parceiros-dest_cep     = emitente_d-post_code1.
    zcte_parceiros-dest_uf      = emitente_d-region.
    zcte_parceiros-dest_fone    = emitente_d-tel_number.
  ENDIF.

ENDMODULE.                 " CTE_MUDA_DESTINATARIO  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_BUSCA_MUN_INICIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_busca_mun_inicio INPUT.

  TYPES:
    BEGIN OF ty_mun_ini,
      taxjurcode TYPE j_1btxjcd,
      text       TYPE text60,
    END OF ty_mun_ini.

  DATA: ti_mun_ini TYPE STANDARD TABLE OF ty_mun_ini INITIAL SIZE 0 WITH HEADER LINE,
        uf_ini     TYPE string.

  CLEAR: st_ret, t_dynpfields[], t_ret.

  MOVE: 'ZCTE_IDENTIFICA-UFINI' TO t_dynpfields-fieldname.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.

  READ TABLE t_dynpfields INDEX 1.

  IF t_dynpfields-fieldvalue IS INITIAL.

    SELECT taxjurcode text INTO TABLE ti_mun_ini
      FROM j_1btxjurt
     WHERE spras   EQ zcte_identifica-spras
       AND country EQ zcte_identifica-country.

  ELSE.

    CONCATENATE t_dynpfields-fieldvalue '%' INTO uf_ini.

    SELECT taxjurcode text INTO TABLE ti_mun_ini
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode LIKE uf_ini.

  ENDIF.

  CHECK NOT ti_mun_ini[] IS INITIAL.

  CLEAR: t_dynpfields[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_mun_ini[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.
*---> 05/07/2023 - Migração S4 - DL
  SORT ti_mun_ini BY taxjurcode.
*<--- 05/07/2023 - Migração S4 - DL
  READ TABLE ti_mun_ini WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZCTE_IDENTIFICA-UFINI'    TO t_dynpfields-fieldname,
        ti_mun_ini-taxjurcode(3)   TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZCTE_IDENTIFICA-CMUNINI'  TO t_dynpfields-fieldname,
        ti_mun_ini-taxjurcode+3(7) TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZCTE_IDENTIFICA-NMUNINI'  TO t_dynpfields-fieldname,
        ti_mun_ini-text            TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " CTE_BUSCA_MUN_INICIO  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_BUSCA_MUN_FIM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_busca_mun_fim INPUT.

  TYPES:
    BEGIN OF ty_mun_fim,
      taxjurcode TYPE j_1btxjcd,
      text       TYPE text60,
    END OF ty_mun_fim.

  DATA: ti_mun_fim TYPE STANDARD TABLE OF ty_mun_fim INITIAL SIZE 0 WITH HEADER LINE,
        uf_fim     TYPE string.

  CLEAR: st_ret, t_dynpfields[], t_ret.

  MOVE: 'ZCTE_IDENTIFICA-UFFIM'    TO t_dynpfields-fieldname.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.

  READ TABLE t_dynpfields INDEX 1.

  IF t_dynpfields-fieldvalue IS INITIAL.

    SELECT taxjurcode text INTO TABLE ti_mun_fim
      FROM j_1btxjurt
     WHERE spras   EQ zcte_identifica-spras
       AND country EQ zcte_identifica-country.

  ELSE.

    CONCATENATE t_dynpfields-fieldvalue '%' INTO uf_fim.

    SELECT taxjurcode text INTO TABLE ti_mun_fim
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode LIKE uf_fim.

  ENDIF.

  CLEAR: t_dynpfields[].

  CHECK NOT ti_mun_fim[] IS INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_mun_fim[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.
*---> 06/07/2023 - Migração S4 - DL
  SORT ti_mun_fim BY taxjurcode.
*<--- 06/07/2023 - Migração S4 - DL
  READ TABLE ti_mun_fim WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZCTE_IDENTIFICA-UFFIM'    TO t_dynpfields-fieldname,
        ti_mun_fim-taxjurcode(3)   TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZCTE_IDENTIFICA-CMUNFIM'  TO t_dynpfields-fieldname,
        ti_mun_fim-taxjurcode+3(7) TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZCTE_IDENTIFICA-NMUNFIM'  TO t_dynpfields-fieldname,
        ti_mun_fim-text            TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " CTE_BUSCA_MUN_FIM  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_MUDA_MUNI_INICIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_muda_muni_inicial INPUT.

  DATA: vl_uf_ini TYPE string.

  CONCATENATE zcte_identifica-ufini zcte_identifica-cmunini INTO vl_uf_ini SEPARATED BY space.

  CLEAR: zcte_identifica-nmunini.

  SELECT SINGLE text INTO zcte_identifica-nmunini
    FROM j_1btxjurt
   WHERE spras      EQ zcte_identifica-spras
     AND country    EQ zcte_identifica-country
     AND taxjurcode EQ vl_uf_ini.

ENDMODULE.                 " CTE_MUDA_MUNI_INICIAL  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_MUDA_MUNI_FINAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_muda_muni_final INPUT.

  DATA: vl_uf_fim TYPE string.

  CONCATENATE zcte_identifica-uffim zcte_identifica-cmunfim INTO vl_uf_fim SEPARATED BY space.

  CLEAR: zcte_identifica-nmunfim.

  SELECT SINGLE text INTO zcte_identifica-nmunfim
    FROM j_1btxjurt
   WHERE spras      EQ zcte_identifica-spras
     AND country    EQ zcte_identifica-country
     AND taxjurcode EQ vl_uf_fim.

ENDMODULE.                 " CTE_MUDA_MUNI_FINAL  INPUT

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_INFO_MOTORISTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM seleciona_info_motoristas .

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  PERFORM cte_cria_moto_alv.

  CALL METHOD cte_alv_moto->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  IF it_selected_rows IS NOT INITIAL.
    READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
    READ TABLE it_cte_moto_alv INDEX wa_selected_rows-index INTO cte_moto_alv.
    MOVE-CORRESPONDING cte_moto_alv TO zcte_motorista.
  ENDIF.

ENDFORM.                    " SELECIONA_INFO_MOTORISTAS

*&---------------------------------------------------------------------*
*&      Module  CTE_CRIA_MOTO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_cria_moto OUTPUT.

  CONSTANTS: tabela_moto TYPE string VALUE 'IT_CTE_MOTO_ALV'.

  PERFORM cte_cria_moto_alv.

  MOVE: it_cte_moto[] TO it_cte_moto_alv[].

  CALL METHOD cte_alv_moto->refresh_table_display.

ENDMODULE.                 " CTE_CRIA_MOTO  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CTE_CRIA_MOTO_ALV
*&---------------------------------------------------------------------*
FORM cte_cria_moto_alv .

  IF cte_prim_moto IS INITIAL.

*   Create object for container
    CREATE OBJECT cte_container_moto
      EXPORTING
        container_name = 'CTE_MOTO'.

    CREATE OBJECT cte_alv_moto
      EXPORTING
        i_parent = cte_container_moto.

    PERFORM z_estrutura_fieldcat TABLES it_cte_catalog_moto USING:
        tabela_moto 'CPF'   TEXT-m00 ' ' 01 11 space space,
        tabela_moto 'XNOME' TEXT-m01 ' ' 02 60 space space,
        tabela_moto 'LIFNR' TEXT-m02 ' ' 03 10 space space.

    cte_gs_layout-zebra    = c_x.
    cte_gs_layout-sel_mode = space.

    CALL METHOD cte_alv_moto->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = cte_gs_layout
      CHANGING
        it_fieldcatalog = it_cte_catalog_moto
        it_outtab       = it_cte_moto_alv[].

    cte_prim_moto = c_x.
  ENDIF.

ENDFORM.                    " CTE_CRIA_MOTO_ALV

*&---------------------------------------------------------------------*
*&      Module  STATUS_2104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2104 OUTPUT.

  IF zcte_motorista-cpf IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_OUTPUT'
      EXPORTING
        input  = zcte_motorista-cpf
      IMPORTING
        output = cte_text_motorista_cpf.

  ELSE.
    CLEAR: cte_text_motorista_cpf .
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_2104  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_DADOS_LIFNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_dados_lifnr INPUT.

  DATA: wa_info_moto TYPE lfa1.

  CALL FUNCTION 'Z_PARCEIRO_INFO'
    EXPORTING
      p_parceiro   = zcte_motorista-lifnr
      p_partype    = 'V'
    CHANGING
      wa_info_part = wa_info_moto.

  IF wa_info_moto IS NOT INITIAL.
    PERFORM lct USING wa_info_moto-name1.
    zcte_motorista-xnome  = vg_limpo.
    zcte_motorista-cpf    = wa_info_moto-stcd2.
    zcte_motorista-lifnr  = wa_info_moto-lifnr.
  ENDIF.

ENDMODULE.                 " SET_DADOS_LIFNR  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2104 INPUT.

  DATA: answer_moto   TYPE c LENGTH 1,
        validado_moto TYPE c LENGTH 1.

  CASE cte_ok_code.
    WHEN cte_incluir.
      CLEAR: zcte_trans.
      zcte_motorista-docnum = cte_j_1bnfdoc-docnum.
      CLEAR: cte_alterado.
      cte_alterando = 'I'.
    WHEN cte_alterar.
      CLEAR: cte_alterado.
      cte_alterando = 'A'.
    WHEN cte_apagar.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-004
        IMPORTING
          answer    = answer_moto.

      CASE answer_moto.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.

      DELETE FROM zcte_motorista WHERE docnum EQ zcte_motorista-docnum
                                   AND lifnr  EQ zcte_motorista-lifnr.

      DELETE it_cte_moto WHERE docnum EQ zcte_motorista-docnum
                            AND lifnr  EQ zcte_motorista-lifnr.

      CLEAR: zcte_motorista.

    WHEN cte_confirma.
      PERFORM verificar_insert_moto USING zcte_motorista validado_moto.
      IF validado_moto EQ c_x.
        MODIFY zcte_trans.

        IF cte_alterando EQ 'I'.
          MOVE-CORRESPONDING zcte_motorista TO cte_moto_alv.
          APPEND cte_moto_alv TO it_cte_moto.
          MODIFY zcte_motorista.
        ELSEIF cte_alterando EQ 'A'.
          READ TABLE it_cte_moto INTO cte_moto_alv WITH KEY docnum = zcte_motorista-docnum
                                                            lifnr  = zcte_motorista-lifnr.
          MOVE-CORRESPONDING zcte_motorista TO cte_moto_alv.
          MODIFY it_cte_moto INDEX sy-tabix FROM cte_moto_alv.
        ENDIF.

        cte_alterando = 'C'.
        CLEAR: cte_alterado.
      ENDIF.
    WHEN cte_cancelar.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-002
          textline2 = TEXT-003
        IMPORTING
          answer    = answer_moto.

      CASE answer_moto.
        WHEN 'J'.
          CLEAR: cte_alterado, cte_alterado.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.

      READ TABLE it_cte_moto INTO cte_moto_alv WITH KEY docnum = zcte_motorista-docnum
                                                        lifnr  = zcte_motorista-lifnr.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING cte_moto_alv TO zcte_motorista.
      ELSE.
        CLEAR: zcte_motorista.
      ENDIF.
      cte_alterando = 'C'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_2104  INPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_INSERT_MOTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verificar_insert_moto  USING    p_zcte_mototista TYPE zcte_motorista
                                     p_validado       TYPE c.

  CLEAR: p_validado.

  IF p_zcte_mototista-lifnr IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado um código de fornecedor!'.
    EXIT.
  ENDIF.

  p_validado = 'X'.

ENDFORM.                    " VERIFICAR_INSERT_MOTO

*&---------------------------------------------------------------------*
*&      Module  CRIA_EDITOR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_editor_obs_geral OUTPUT.

  DATA: da_rc LIKE sy-subrc VALUE 0.

  DATA: lt_text_table                         TYPE text_table_type,
        ls_text_table_wa(gc_text_line_length) TYPE c.

  IF gv_text_editor IS INITIAL.

    CREATE OBJECT gv_custom_container
      EXPORTING
        container_name              = 'TEXT_CONTROL'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CHECK sy-subrc EQ 0.

    CREATE OBJECT gv_text_editor
      EXPORTING
        parent                     = gv_custom_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_windowborder
        wordwrap_to_linebreak_mode = cl_gui_textedit=>false
      EXCEPTIONS
        error_cntl_create          = 1
        error_cntl_init            = 2
        error_cntl_link            = 3
        error_dp_create            = 4
        gui_type_not_supported     = 5.

    CHECK sy-subrc EQ 0.

    REFRESH tlinetab.

  ENDIF.

  IF ( tlinetab[] IS INITIAL ) AND ( it_cte_obsg[] IS NOT INITIAL ) AND ( cte_alterando EQ 'C' ) .
    LOOP AT it_cte_obsg INTO cte_obsg.
      tlinetab-tdformat = '*'.
      tlinetab-tdline   = cte_obsg-texto.
      APPEND tlinetab.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
    TABLES
      itf_text    = tlinetab
      text_stream = lt_text_table.

* ... und den assoziierten Text dem Control bekanntmachen
  CALL METHOD gv_text_editor->set_text_as_stream
    EXPORTING
      text            = lt_text_table
    EXCEPTIONS
      error_dp        = 1
      error_dp_create = 2
      OTHERS          = 3.

  IF cte_alterando EQ 'A'.
    CALL METHOD gv_text_editor->set_readonly_mode
      EXPORTING
        readonly_mode          = gv_text_editor->false
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3.
  ELSE.
    CALL METHOD gv_text_editor->set_readonly_mode
      EXPORTING
        readonly_mode          = gv_text_editor->true
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3.
  ENDIF.

ENDMODULE.                 " CRIA_EDITOR  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0106  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0106 INPUT.

  DATA: answer_obsg   TYPE c LENGTH 1,
        validado_obsg TYPE c LENGTH 1.

  CASE cte_ok_code.
    WHEN cte_alterar.
      CLEAR: cte_alterado.
      cte_alterando = 'A'.
    WHEN cte_apagar.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-004
        IMPORTING
          answer    = answer_obsg.

      CASE answer_obsg.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.

      DELETE FROM zcte_obs_gerais WHERE docnum EQ cte_j_1bnfdoc-docnum.
      CLEAR: it_cte_obsg[], tlinetab[].
    WHEN cte_confirma.
      PERFORM verificar_insert_obsgera USING validado_obsg.
      IF validado_obsg EQ c_x.
        CLEAR: it_cte_obsg[].
        DELETE FROM zcte_obs_gerais WHERE docnum EQ cte_j_1bnfdoc-docnum.
        cte_obsg-linha = 0.
        LOOP AT tlinetab.
          ADD 1 TO cte_obsg-linha.
          cte_obsg-texto = tlinetab-tdline.
          APPEND cte_obsg TO it_cte_obsg.
          MODIFY zcte_obs_gerais FROM cte_obsg.
        ENDLOOP.
        cte_alterando = 'C'.
        CLEAR: cte_alterado.
      ENDIF.
    WHEN cte_cancelar.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-002
          textline2 = TEXT-003
        IMPORTING
          answer    = answer_obsg.

      CASE answer_obsg.
        WHEN 'J'.
          CLEAR: cte_alterado, cte_alterado.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.

      REFRESH tlinetab.

      SELECT * INTO cte_obsg
        FROM zcte_obs_gerais
       WHERE docnum EQ cte_j_1bnfdoc-docnum.
        tlinetab-tdformat = '*'.
        tlinetab-tdline   = cte_obsg-texto.
        APPEND tlinetab.
      ENDSELECT.

      cte_alterando = 'C'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0106  INPUT

*&---------------------------------------------------------------------*
*&      Module  TEXT_UPDATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE text_update_obs_geral INPUT.

  DATA: gv_xthead_updkz TYPE i.

  DATA: lt_text_tabler                         TYPE text_table_type,
        ls_text_table_war(gc_text_line_length) TYPE c.

  gv_xthead_updkz = 0.

* Texttabelle aus Control abholen
  CALL METHOD gv_text_editor->get_text_as_stream
    IMPORTING
      text                   = lt_text_tabler
      is_modified            = gv_xthead_updkz
    EXCEPTIONS
      error_dp               = 1
      error_cntl_call_method = 2
      OTHERS                 = 3.

  IF gv_xthead_updkz <> 0 AND NOT lt_text_tabler IS INITIAL.
    cte_alterado = 'X'.
    CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
      TABLES
        text_stream = lt_text_tabler
        itf_text    = tlinetab.
  ENDIF.

ENDMODULE.                 " TEXT_UPDATE  INPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_INSERT_OBSGERA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VALIDADO_OBSG  text
*----------------------------------------------------------------------*
FORM verificar_insert_obsgera  USING    p_validado TYPE c.

  CLEAR: p_validado.

  IF tlinetab[] IS INITIAL.
    MESSAGE i023 WITH 'Não foi digitado texto para a Obs. Geral'.
    EXIT.
  ENDIF.

  p_validado = 'X'.

ENDFORM.                    " VERIFICAR_INSERT_OBSGERA

*&---------------------------------------------------------------------*
*&      Module  STATUS_2001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2001 OUTPUT.

  CLEAR: vg_transp_emissora.

  IF zcte_identifica-emissor IS NOT INITIAL.
    SELECT SINGLE name1 INTO vg_transp_emissora
      FROM lfa1
     WHERE lifnr EQ zcte_identifica-emissor.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_2001  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2001 INPUT.

  CHECK cte_dynnr_ifn NE cte_c_2102 AND
        cte_dynnr_ifn NE cte_c_2104 AND
        cte_dynnr_ifn NE cte_c_2106.

  CASE cte_ok_code.
    WHEN cte_alterar.
      MOVE-CORRESPONDING zcte_identifica TO copia_identifica.
      cte_alterando       = 'A'.
      cte_alterando_modal = 'A'.
    WHEN cte_confirma.
      MODIFY zcte_identifica FROM zcte_identifica.
      CLEAR: cte_alterando,
             cte_alterando_modal.
    WHEN cte_cancelar.
      CLEAR: cte_alterando,
             cte_alterando_modal.
      MOVE-CORRESPONDING copia_identifica TO zcte_identifica.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_2001  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_STATUS_2106  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_status_2106 OUTPUT.

  IF cte_dynnr_ciot IS INITIAL.
    cte_dynnr_ciot = cte_c_6001.
  ENDIF.

  CASE cte_dynnr_ciot.
    WHEN cte_c_6001.
      inf_ciot_detalhe-activetab = cte_tbts0601.
    WHEN cte_c_6002.
      inf_ciot_detalhe-activetab = cte_tbts0602.
    WHEN cte_c_6010.
      inf_ciot_detalhe-activetab = cte_tbts0610.
    WHEN cte_c_6003.
      inf_ciot_detalhe-activetab = cte_tbts0603.
    WHEN cte_c_6004.
      inf_ciot_detalhe-activetab = cte_tbts0604.
    WHEN cte_c_6005.
      inf_ciot_detalhe-activetab = cte_tbts0605.
    WHEN cte_c_6006.
      inf_ciot_detalhe-activetab = cte_tbts0606.
    WHEN cte_c_6007.
      inf_ciot_detalhe-activetab = cte_tbts0607.
    WHEN cte_c_6008.
      inf_ciot_detalhe-activetab = cte_tbts0608.
    WHEN cte_c_6009.
      inf_ciot_detalhe-activetab = cte_tbts0609.
    WHEN cte_c_6011.
      inf_ciot_detalhe-activetab = cte_tbts0611.
    WHEN cte_c_6012.
      inf_ciot_detalhe-activetab = cte_tbts0612.
  ENDCASE.

ENDMODULE.                 " CTE_STATUS_2106  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_BUSCA_MUN_ORIGEM  INPUT
*&---------------------------------------------------------------------*
MODULE cte_busca_mun_origem INPUT.

  TYPES:
    BEGIN OF ty_mun_org,
      taxjurcode TYPE j_1btxjcd,
      text       TYPE text60,
    END OF ty_mun_org.

  DATA: ti_mun_org TYPE STANDARD TABLE OF ty_mun_org INITIAL SIZE 0 WITH HEADER LINE,
        uf_org     TYPE string.

  CLEAR: st_ret, t_dynpfields[], t_ret.

  MOVE: 'ZCTE_CIOT-UF_ORIGEM' TO t_dynpfields-fieldname.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.

  READ TABLE t_dynpfields INDEX 1.

  IF t_dynpfields-fieldvalue IS INITIAL.
    SELECT taxjurcode text INTO TABLE ti_mun_org
      FROM j_1btxjurt
     WHERE spras   EQ zcte_identifica-spras
       AND country EQ zcte_identifica-country.
  ELSE.
    CONCATENATE t_dynpfields-fieldvalue '%' INTO uf_org.

    SELECT taxjurcode text INTO TABLE ti_mun_org
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode LIKE uf_org.
  ENDIF.

  CHECK NOT ti_mun_org[] IS INITIAL.

  CLEAR: t_dynpfields[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_mun_org[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.
*---> 06.07.2023 11:37:59 - Migração S4 - DL
  SORT ti_mun_org BY taxjurcode.
*<--- 06.07.2023 11:37:59 - Migração S4 - DL
  READ TABLE ti_mun_org WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZCTE_CIOT-UF_ORIGEM'        TO t_dynpfields-fieldname,
        ti_mun_org-taxjurcode(3)     TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.
  MOVE: 'ZCTE_CIOT-MUNICIPIO_ORIGEM' TO t_dynpfields-fieldname,
        ti_mun_org-taxjurcode+3(7)   TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " CTE_BUSCA_MUN_ORIGEM  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_BUSCA_MUN_TERMIN  INPUT
*&---------------------------------------------------------------------*
MODULE cte_busca_mun_termin INPUT.

  TYPES:
    BEGIN OF ty_mun_tem,
      taxjurcode TYPE j_1btxjcd,
      text       TYPE text60,
    END OF ty_mun_tem.

  DATA: ti_mun_tem TYPE STANDARD TABLE OF ty_mun_tem INITIAL SIZE 0 WITH HEADER LINE,
        uf_tem     TYPE string.

  CLEAR: st_ret, t_dynpfields[], t_ret.

  MOVE: 'ZCTE_CIOT-UF_TERMIN' TO t_dynpfields-fieldname.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.

  READ TABLE t_dynpfields INDEX 1.

  IF t_dynpfields-fieldvalue IS INITIAL.
    SELECT taxjurcode text INTO TABLE ti_mun_tem
      FROM j_1btxjurt
     WHERE spras   EQ zcte_identifica-spras
       AND country EQ zcte_identifica-country.
  ELSE.
    CONCATENATE t_dynpfields-fieldvalue '%' INTO uf_tem.

    SELECT taxjurcode text INTO TABLE ti_mun_tem
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode LIKE uf_tem.
  ENDIF.

  CHECK NOT ti_mun_tem[] IS INITIAL.

  CLEAR: t_dynpfields[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_mun_tem[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.
*---> 06.07.2023 11:36:21 - Migração S4 - DL
  SORT ti_mun_tem BY taxjurcode.
*<--- 06.07.2023 11:36:21 - Migração S4 - DL
  READ TABLE ti_mun_tem WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZCTE_CIOT-UF_TERMIN'        TO t_dynpfields-fieldname,
        ti_mun_tem-taxjurcode(3)     TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.
  MOVE: 'ZCTE_CIOT-MUNICIPIO_TERMIN' TO t_dynpfields-fieldname,
        ti_mun_tem-taxjurcode+3(7)   TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " CTE_BUSCA_MUN_TERMIN  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2106  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2106 INPUT.

  DATA: answer_ciot   TYPE c LENGTH 1,
        validado_ciot TYPE c LENGTH 1,
        zciot         TYPE REF TO zcl_ciot.

  CASE cte_ok_code.
    WHEN cte_tbts0601.
      cte_dynnr_ciot = cte_c_6001.
    WHEN cte_tbts0602.
      cte_dynnr_ciot = cte_c_6002.
    WHEN cte_tbts0603.
      cte_dynnr_ciot = cte_c_6003.
    WHEN cte_tbts0604.
      cte_dynnr_ciot = cte_c_6004.
    WHEN cte_tbts0605.
      cte_dynnr_ciot = cte_c_6005.
    WHEN cte_tbts0606.
      cte_dynnr_ciot = cte_c_6006.
    WHEN cte_tbts0607.
      cte_dynnr_ciot = cte_c_6007.
    WHEN cte_tbts0608.
      cte_dynnr_ciot = cte_c_6008.
    WHEN cte_tbts0609.
      cte_dynnr_ciot = cte_c_6009.
    WHEN cte_tbts0610.
      cte_dynnr_ciot = cte_c_6010.
    WHEN cte_tbts0611.
      cte_dynnr_ciot = cte_c_6011.
    WHEN cte_tbts0612.
      cte_dynnr_ciot = cte_c_6012.
    WHEN cte_incluir.
      CLEAR: zcte_ciot.
      zcte_ciot-docnum = cte_j_1bnfdoc-docnum.
      CLEAR: cte_alterado.
      cte_alterando = 'I'.
    WHEN cte_alterar.
      CLEAR: cte_alterado.
      cte_alterando = 'A'.
    WHEN cte_apagar.
      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-004
        IMPORTING
          answer    = answer_ciot.

      CASE answer_ciot.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.
      DELETE FROM zcte_ciot WHERE cd_ciot EQ zcte_ciot-cd_ciot.
      DELETE it_cte_ciot WHERE cd_ciot EQ zcte_ciot-cd_ciot.
      CLEAR: zcte_ciot.
    WHEN cte_confirma.
      PERFORM verificar_insert_ciot USING zcte_ciot validado_ciot.
      IF validado_ciot EQ c_x.

        CREATE OBJECT zciot.

        CALL METHOD zciot->set_zcte_ciot
          EXPORTING
            p_cte_ciot = zcte_ciot.

        IF cte_alterando EQ 'I'.
          MOVE-CORRESPONDING zcte_ciot TO cte_ciot.
          APPEND cte_ciot TO it_cte_ciot.
        ELSEIF cte_alterando EQ 'A'.
          READ TABLE it_cte_ciot INTO cte_ciot WITH KEY cd_ciot = zcte_ciot-cd_ciot.
          MOVE-CORRESPONDING zcte_ciot TO cte_ciot.
          MODIFY it_cte_ciot INDEX sy-tabix FROM cte_ciot.
        ENDIF.

        CALL METHOD zciot->gravar.

        cte_alterando = 'C'.
        CLEAR: cte_alterado.
      ENDIF.
    WHEN cte_cancelar.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-002
          textline2 = TEXT-003
        IMPORTING
          answer    = answer_ciot.

      CASE answer_ciot.
        WHEN 'J'.
          CLEAR: cte_alterado, cte_alterado.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.

      READ TABLE it_cte_ciot INTO cte_ciot WITH KEY cd_ciot = zcte_ciot-cd_ciot.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING cte_ciot TO zcte_ciot.
      ELSE.
        CLEAR: zcte_ciot.
      ENDIF.
      cte_alterando = 'C'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_2106  INPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_INSERT_CIOT
*&---------------------------------------------------------------------*
FORM verificar_insert_ciot  USING    p_zcte_ciot TYPE zcte_ciot
                                     p_validado  TYPE c.

  CLEAR: p_validado.

*  IF p_zcte_ciot-lifnr IS INITIAL.
*    MESSAGE i023 WITH 'Deve ser informado um código de fornecedor!'.
*    EXIT.
*  ENDIF.

  p_validado = 'X'.

ENDFORM.                    " VERIFICAR_INSERT_CIOT

*&---------------------------------------------------------------------*
*&      Module  CTE_STATUS_6001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_status_6001 OUTPUT.

  DATA: vl_uf_ini_ciot TYPE string.
  DATA: vl_uf_tem_ciot TYPE string.

  CLEAR: txt_municipio_origem,
         txt_municipio_termin,
         txt_material,
         txt_cnpj_cpf.

  IF ( zcte_ciot-uf_origem IS NOT INITIAL ) AND ( zcte_ciot-municipio_origem IS NOT INITIAL ).
    CONCATENATE zcte_ciot-uf_origem zcte_ciot-municipio_origem INTO vl_uf_ini_ciot SEPARATED BY space.
    SELECT SINGLE text INTO txt_municipio_origem
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode EQ vl_uf_ini_ciot.
  ENDIF.

  IF ( zcte_ciot-uf_origem IS NOT INITIAL ) AND ( zcte_ciot-municipio_origem IS NOT INITIAL ).
    CONCATENATE zcte_ciot-uf_termin zcte_ciot-municipio_termin INTO vl_uf_tem_ciot SEPARATED BY space.
    SELECT SINGLE text INTO txt_municipio_termin
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode EQ vl_uf_tem_ciot.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " CTE_STATUS_6001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_6002  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_6002 OUTPUT.

  DATA: vl_uf_ct TYPE string.

  CLEAR: txt_nm_ct_ciot.

  IF ( zcte_ciot-ct_uf IS NOT INITIAL ) AND ( zcte_ciot-ct_municipio IS NOT INITIAL ).
    CONCATENATE zcte_ciot-ct_uf zcte_ciot-ct_municipio INTO vl_uf_ct SEPARATED BY space.
    SELECT SINGLE text INTO txt_nm_ct_ciot
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode EQ vl_uf_ct.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_6002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_BUSCA_MUN_CT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_busca_mun_ct INPUT.

  TYPES:
    BEGIN OF ty_mun_ct,
      taxjurcode TYPE j_1btxjcd,
      text       TYPE text60,
    END OF ty_mun_ct.

  DATA: ti_mun_ct TYPE STANDARD TABLE OF ty_mun_ct INITIAL SIZE 0 WITH HEADER LINE,
        uf_ct     TYPE string.

  CLEAR: st_ret, t_dynpfields[], t_ret.

  MOVE: 'ZCTE_CIOT-CT_UF' TO t_dynpfields-fieldname.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.

  READ TABLE t_dynpfields INDEX 1.

  IF t_dynpfields-fieldvalue IS INITIAL.
    SELECT taxjurcode text INTO TABLE ti_mun_ct
      FROM j_1btxjurt
     WHERE spras   EQ zcte_identifica-spras
       AND country EQ zcte_identifica-country.
  ELSE.
    CONCATENATE t_dynpfields-fieldvalue '%' INTO uf_ct.

    SELECT taxjurcode text INTO TABLE ti_mun_ct
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode LIKE uf_ct.
  ENDIF.

  CHECK NOT ti_mun_ct[] IS INITIAL.

  CLEAR: t_dynpfields[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_mun_ct[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  READ TABLE ti_mun_ct WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZCTE_CIOT-CT_UF'         TO t_dynpfields-fieldname,
        ti_mun_ct-taxjurcode(3)   TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZCTE_CIOT-CT_MUNICIPIO'  TO t_dynpfields-fieldname,
        ti_mun_ct-taxjurcode+3(7) TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " CTE_BUSCA_MUN_CT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_6003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_6003 OUTPUT.

  DATA: vl_uf_dt TYPE string.

  CLEAR: txt_nm_dt_ciot.

  IF ( zcte_ciot-dt_uf IS NOT INITIAL ) AND ( zcte_ciot-dt_municipio IS NOT INITIAL ).
    CONCATENATE zcte_ciot-dt_uf zcte_ciot-dt_municipio INTO vl_uf_dt SEPARATED BY space.
    SELECT SINGLE text INTO txt_nm_dt_ciot
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode EQ vl_uf_dt.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_6003  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_BUSCA_MUN_DT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_busca_mun_dt INPUT.

  TYPES:
    BEGIN OF ty_mun_dt,
      taxjurcode TYPE j_1btxjcd,
      text       TYPE text60,
    END OF ty_mun_dt.

  DATA: ti_mun_dt TYPE STANDARD TABLE OF ty_mun_dt INITIAL SIZE 0 WITH HEADER LINE,
        uf_dt     TYPE string.

  CLEAR: st_ret, t_dynpfields[], t_ret.

  MOVE: 'ZCTE_CIOT-DT_UF' TO t_dynpfields-fieldname.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.

  READ TABLE t_dynpfields INDEX 1.

  IF t_dynpfields-fieldvalue IS INITIAL.
    SELECT taxjurcode text INTO TABLE ti_mun_dt
      FROM j_1btxjurt
     WHERE spras   EQ zcte_identifica-spras
       AND country EQ zcte_identifica-country.
  ELSE.
    CONCATENATE t_dynpfields-fieldvalue '%' INTO uf_dt.

    SELECT taxjurcode text INTO TABLE ti_mun_dt
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode LIKE uf_dt.
  ENDIF.

  CHECK NOT ti_mun_dt[] IS INITIAL.

  CLEAR: t_dynpfields[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_mun_dt[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.
*---> 05/07/2023 - Migração S4 - DL
  SORT ti_mun_dt BY taxjurcode.
*<--- 05/07/2023 - Migração S4 - DL
  READ TABLE ti_mun_dt WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZCTE_CIOT-DT_UF'         TO t_dynpfields-fieldname,
        ti_mun_dt-taxjurcode(3)   TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZCTE_CIOT-DT_MUNICIPIO'  TO t_dynpfields-fieldname,
        ti_mun_dt-taxjurcode+3(7) TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " CTE_BUSCA_MUN_DT  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_6004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_6004 OUTPUT.

  IF cte_alterando EQ 'A'.
    zcte_ciot-vlr_saldo = zcte_ciot-vlr_frete - zcte_ciot-vlr_adiantamento.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_6004  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_6005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_6005 OUTPUT.

  IF cte_alterando EQ 'A'.
    zcte_ciot-vlr_impostos = 0.
    ADD zcte_ciot-vlr_inss TO zcte_ciot-vlr_impostos.
    ADD zcte_ciot-vlr_sest TO zcte_ciot-vlr_impostos.
    ADD zcte_ciot-vlr_irpf TO zcte_ciot-vlr_impostos.
    ADD zcte_ciot-vlr_iof  TO zcte_ciot-vlr_impostos.
    ADD zcte_ciot-vlr_iss  TO zcte_ciot-vlr_impostos.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_6005  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_BUSCA_MUN_SB  INPUT
*&---------------------------------------------------------------------*
MODULE cte_busca_mun_sb INPUT.

  TYPES:
    BEGIN OF ty_mun_sb,
      taxjurcode TYPE j_1btxjcd,
      text       TYPE text60,
    END OF ty_mun_sb.

  DATA: ti_mun_sb TYPE STANDARD TABLE OF ty_mun_ct INITIAL SIZE 0 WITH HEADER LINE,
        uf_sb     TYPE string.

  CLEAR: st_ret, t_dynpfields[], t_ret.

  MOVE: 'ZCTE_CIOT-SB_UF' TO t_dynpfields-fieldname.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.

  READ TABLE t_dynpfields INDEX 1.

  IF t_dynpfields-fieldvalue IS INITIAL.
    SELECT taxjurcode text INTO TABLE ti_mun_sb
      FROM j_1btxjurt
     WHERE spras   EQ zcte_identifica-spras
       AND country EQ zcte_identifica-country.
  ELSE.
    CONCATENATE t_dynpfields-fieldvalue '%' INTO uf_sb.

    SELECT taxjurcode text INTO TABLE ti_mun_sb
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode LIKE uf_sb.
  ENDIF.

  CHECK NOT ti_mun_sb[] IS INITIAL.

  CLEAR: t_dynpfields[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_mun_sb[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.
*---> 06.07.2023 11:35:41 - Migração S4 - DL
  SORT ti_mun_sb BY taxjurcode.
*<--- 06.07.2023 11:35:41 - Migração S4 - DL
  READ TABLE ti_mun_sb WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZCTE_CIOT-SB_UF'         TO t_dynpfields-fieldname,
        ti_mun_sb-taxjurcode(3)   TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZCTE_CIOT-SB_MUNICIPIO'  TO t_dynpfields-fieldname,
        ti_mun_sb-taxjurcode+3(7) TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " CTE_BUSCA_MUN_SB  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_6006  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_6006 OUTPUT.

  DATA: vl_uf_sb TYPE string.

  CLEAR: txt_nm_sb_ciot.

  IF ( zcte_ciot-sb_uf IS NOT INITIAL ) AND ( zcte_ciot-sb_municipio IS NOT INITIAL ).
    CONCATENATE zcte_ciot-sb_uf zcte_ciot-sb_municipio INTO vl_uf_sb SEPARATED BY space.
    SELECT SINGLE text INTO txt_nm_sb_ciot
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode EQ vl_uf_sb.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_6006  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_6007  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_6007 OUTPUT.

  DATA: vl_uf_cs TYPE string.

  CLEAR: txt_nm_cs_ciot.

  IF ( zcte_ciot-cs_uf IS NOT INITIAL ) AND ( zcte_ciot-cs_municipio IS NOT INITIAL ).
    CONCATENATE zcte_ciot-cs_uf zcte_ciot-cs_municipio INTO vl_uf_cs SEPARATED BY space.
    SELECT SINGLE text INTO txt_nm_cs_ciot
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode EQ vl_uf_cs.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_6007  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_BUSCA_MUN_CS  INPUT
*&---------------------------------------------------------------------*
MODULE cte_busca_mun_cs INPUT.

  TYPES:
    BEGIN OF ty_mun_cs,
      taxjurcode TYPE j_1btxjcd,
      text       TYPE text60,
    END OF ty_mun_cs.

  DATA: ti_mun_cs TYPE STANDARD TABLE OF ty_mun_cs INITIAL SIZE 0 WITH HEADER LINE,
        uf_cs     TYPE string.

  CLEAR: st_ret, t_dynpfields[], t_ret.

  MOVE: 'ZCTE_CIOT-CS_UF' TO t_dynpfields-fieldname.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.

  READ TABLE t_dynpfields INDEX 1.

  IF t_dynpfields-fieldvalue IS INITIAL.
    SELECT taxjurcode text INTO TABLE ti_mun_cs
      FROM j_1btxjurt
     WHERE spras   EQ zcte_identifica-spras
       AND country EQ zcte_identifica-country.
  ELSE.
    CONCATENATE t_dynpfields-fieldvalue '%' INTO uf_cs.

    SELECT taxjurcode text INTO TABLE ti_mun_cs
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode LIKE uf_cs.
  ENDIF.

  CHECK NOT ti_mun_cs[] IS INITIAL.

  CLEAR: t_dynpfields[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_mun_cs[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  READ TABLE ti_mun_cs WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZCTE_CIOT-CS_UF'         TO t_dynpfields-fieldname,
        ti_mun_cs-taxjurcode(3)   TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZCTE_CIOT-CS_MUNICIPIO'  TO t_dynpfields-fieldname,
        ti_mun_cs-taxjurcode+3(7) TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " CTE_BUSCA_MUN_CS  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_BUSCA_MUN_RM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_busca_mun_rm INPUT.

  TYPES:
    BEGIN OF ty_mun_rm,
      taxjurcode TYPE j_1btxjcd,
      text       TYPE text60,
    END OF ty_mun_rm.

  DATA: ti_mun_rm TYPE STANDARD TABLE OF ty_mun_rm INITIAL SIZE 0 WITH HEADER LINE,
        uf_rm     TYPE string.

  CLEAR: st_ret, t_dynpfields[], t_ret.

  MOVE: 'ZCTE_CIOT-RM_UF' TO t_dynpfields-fieldname.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.

  READ TABLE t_dynpfields INDEX 1.

  IF t_dynpfields-fieldvalue IS INITIAL.
    SELECT taxjurcode text INTO TABLE ti_mun_rm
      FROM j_1btxjurt
     WHERE spras   EQ zcte_identifica-spras
       AND country EQ zcte_identifica-country.
  ELSE.
    CONCATENATE t_dynpfields-fieldvalue '%' INTO uf_rm.

    SELECT taxjurcode text INTO TABLE ti_mun_rm
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode LIKE uf_rm.
  ENDIF.

  CHECK NOT ti_mun_rm[] IS INITIAL.

  CLEAR: t_dynpfields[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_mun_rm[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.
*---> 05/07/2023 - Migração S4 - DL
  SORT ti_mun_rm BY taxjurcode.
*<--- 05/07/2023 - Migração S4 - DL
  READ TABLE ti_mun_rm WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZCTE_CIOT-RM_UF'         TO t_dynpfields-fieldname,
        ti_mun_rm-taxjurcode(3)   TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZCTE_CIOT-RM_MUNICIPIO'  TO t_dynpfields-fieldname,
        ti_mun_rm-taxjurcode+3(7) TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " CTE_BUSCA_MUN_RM  INPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_BUSCA_MUN_MT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_busca_mun_mt INPUT.

  TYPES:
    BEGIN OF ty_mun_mt,
      taxjurcode TYPE j_1btxjcd,
      text       TYPE text60,
    END OF ty_mun_mt.

  DATA: ti_mun_mt TYPE STANDARD TABLE OF ty_mun_mt INITIAL SIZE 0 WITH HEADER LINE,
        uf_mt     TYPE string.

  CLEAR: st_ret, t_dynpfields[], t_ret.

  MOVE: 'ZCTE_CIOT-MT_UF' TO t_dynpfields-fieldname.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.

  READ TABLE t_dynpfields INDEX 1.

  IF t_dynpfields-fieldvalue IS INITIAL.
    SELECT taxjurcode text INTO TABLE ti_mun_mt
      FROM j_1btxjurt
     WHERE spras   EQ zcte_identifica-spras
       AND country EQ zcte_identifica-country.
  ELSE.
    CONCATENATE t_dynpfields-fieldvalue '%' INTO uf_mt.

    SELECT taxjurcode text INTO TABLE ti_mun_mt
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode LIKE uf_mt.
  ENDIF.

  CHECK NOT ti_mun_mt[] IS INITIAL.

  CLEAR: t_dynpfields[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_mun_mt[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.
*---> 05/07/2023 - Migração S4 - DL
  SORT ti_mun_mt BY taxjurcode.
*<--- 05/07/2023 - Migração S4 - DL
  READ TABLE ti_mun_mt WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZCTE_CIOT-MT_UF'         TO t_dynpfields-fieldname,
        ti_mun_mt-taxjurcode(3)   TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZCTE_CIOT-MT_MUNICIPIO'  TO t_dynpfields-fieldname,
        ti_mun_mt-taxjurcode+3(7) TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " CTE_BUSCA_MUN_RM  INPUT



*&---------------------------------------------------------------------*
*&      Module  CTE_STATUS_0107  OUTPUT
*&---------------------------------------------------------------------*
MODULE cte_status_0107 OUTPUT.

  IF cte_dynnr_ifn IS INITIAL.
    cte_dynnr_ifn = cte_c_1701.
  ENDIF.

  CASE cte_dynnr_ifn.
    WHEN cte_c_1701.
      inf_seg_cte_tab-activetab = cte_tbsg01.
    WHEN cte_c_1702.
      inf_seg_cte_tab-activetab = cte_tbsg02.
  ENDCASE.

ENDMODULE.                 " CTE_STATUS_0107  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0107  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0107 INPUT.

  CHECK cte_alterado IS INITIAL.
  CHECK ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).

  CASE cte_ok_code.
    WHEN cte_tbsg01 OR cte_back.
      cte_dynnr_ifn = cte_c_1701.
    WHEN cte_tbsg02.
      PERFORM seleciona_info_seguro.
      cte_dynnr_ifn = cte_c_1702.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0107  INPUT

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_INFO_SEGURO
*&---------------------------------------------------------------------*
FORM seleciona_info_seguro .

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CALL METHOD cte_alv_seguro->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  IF it_selected_rows IS NOT INITIAL.
    READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
    READ TABLE it_cte_seguro_alv INDEX wa_selected_rows-index INTO cte_seguro_alv.
    MOVE-CORRESPONDING cte_seguro_alv TO zcte_seguro.
  ENDIF.

ENDFORM.                    " SELECIONA_INFO_SEGURO

*&---------------------------------------------------------------------*
*&      Module  CTE_CRIA_SEGURO  OUTPUT
*&---------------------------------------------------------------------*
MODULE cte_cria_seguro OUTPUT.

  DATA: it_text TYPE TABLE OF dd07v WITH KEY domvalue_l,
        wa_text TYPE dd07v.

  PERFORM cte_cria_seguro_alv.

  CALL FUNCTION 'Z_VALORES_DOMINIO'
    EXPORTING
      name      = 'ZRESPSEG'
    TABLES
      dd07v_tab = it_text.

  CLEAR: it_cte_seguro_alv[].

  LOOP AT it_cte_seguro INTO cte_seguro.
    MOVE-CORRESPONDING cte_seguro TO cte_seguro_alv.

    READ TABLE it_text INTO wa_text WITH KEY domvalue_l = cte_seguro-respseg.
    IF sy-subrc IS INITIAL.
      cte_seguro_alv-txtresponsavel = wa_text-ddtext.
    ENDIF.

    APPEND cte_seguro_alv TO it_cte_seguro_alv.
  ENDLOOP.

  CALL METHOD cte_alv_seguro->refresh_table_display.

ENDMODULE.                 " CTE_CRIA_SEGURO  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CTE_CRIA_INF_NOTA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cte_cria_inf_nota_alv .

  CONSTANTS: tabela_inf_nota TYPE string VALUE 'IT_CTE_IF_NT_ALV'.

  IF cte_prim_info_nota IS INITIAL.

*   Create object for container
    CREATE OBJECT cte_container_inf_nota
      EXPORTING
        container_name = 'CTE_INFO_NOTA'.

    CREATE OBJECT cte_alv_inf_nota
      EXPORTING
        i_parent = cte_container_inf_nota.

    PERFORM z_estrutura_fieldcat TABLES it_cte_catalog_inf_nt USING:
        tabela_inf_nota 'NFE'            TEXT-i00 ' ' 01 04 space space,
        tabela_inf_nota 'MODELO'         TEXT-i01 ' ' 02 02 space space,
        tabela_inf_nota 'SERIE'          TEXT-i02 ' ' 03 03 space space,
        tabela_inf_nota 'NUMERO'         TEXT-i03 ' ' 04 10 space space,
        tabela_inf_nota 'CLIENTE'        TEXT-i04 ' ' 05 10 space 'ALPHA',
        tabela_inf_nota 'DOCNUM_CTE'     TEXT-i05 ' ' 06 10 space space,
        tabela_inf_nota 'DTEMISSAO'      TEXT-i06 ' ' 07 10 space space,
        tabela_inf_nota 'VL_BC'          TEXT-i07 ' ' 08 14 space space,
        tabela_inf_nota 'VL_ICMS'        TEXT-i08 ' ' 09 14 space space,
        tabela_inf_nota 'VL_BC_ST'       TEXT-i09 ' ' 10 14 space space,
        tabela_inf_nota 'VL_ST'          TEXT-i10 ' ' 11 14 space space,
        tabela_inf_nota 'VL_PRODUTOS'    TEXT-i11 ' ' 12 14 space space,
        tabela_inf_nota 'VL_NOTA_FISCAL' TEXT-i12 ' ' 13 14 space space,
        tabela_inf_nota 'CFOP'           TEXT-i13 ' ' 14 10 space 'CFOBR',
        tabela_inf_nota 'LC_RETIRADA'    TEXT-i14 ' ' 15 10 space 'ALPHA',
        tabela_inf_nota 'PIN_SUFRAMA'    TEXT-i15 ' ' 16 10 space space,
        tabela_inf_nota 'UNIDADE'        TEXT-i16 ' ' 17 03 space space,
        tabela_inf_nota 'QUANTIDADE'     TEXT-i17 ' ' 18 16 space space.

    cte_gs_layout-zebra    = c_x.
    cte_gs_layout-sel_mode = space.

    CALL METHOD cte_alv_inf_nota->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = cte_gs_layout
      CHANGING
        it_fieldcatalog = it_cte_catalog_inf_nt
        it_outtab       = it_cte_if_nt_alv[].

    cte_prim_info_nota = c_x.

  ENDIF.


ENDFORM.                    " CTE_CRIA_INF_NOTA_ALV

*&---------------------------------------------------------------------*
*&      Form  CTE_CRIA_SEGURO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cte_cria_seguro_alv .

  CONSTANTS: tabela_seguro TYPE string VALUE 'IT_CTE_SEGURO_ALV'.

  IF cte_prim_seguro IS INITIAL.

*   Create object for container
    CREATE OBJECT cte_container_seguro
      EXPORTING
        container_name = 'CTE_SEGURO'.

    CREATE OBJECT cte_alv_seguro
      EXPORTING
        i_parent = cte_container_seguro.

    PERFORM z_estrutura_fieldcat TABLES it_cte_catalog_seguro USING:
        tabela_seguro 'CD_SEGURO'      TEXT-g00 ' ' 01 10 space space,
        tabela_seguro 'TXTRESPONSAVEL' TEXT-g01 ' ' 02 30 space space,
        tabela_seguro 'XSEG'           TEXT-g02 ' ' 03 35 space space.

    cte_gs_layout-zebra    = c_x.
    cte_gs_layout-sel_mode = space.

    CALL METHOD cte_alv_seguro->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = cte_gs_layout
      CHANGING
        it_fieldcatalog = it_cte_catalog_seguro
        it_outtab       = it_cte_seguro_alv[].

    cte_prim_seguro = c_x.

  ENDIF.

ENDFORM.                    " CTE_CRIA_SEGURO_ALV

*&---------------------------------------------------------------------*
*&      Module  STATUS_1702  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1702 OUTPUT.

  IF ( cte_alterando EQ 'A' ) OR ( cte_alterando EQ 'I' ).

    IF zcte_seguro-resp_codigo IS NOT INITIAL.
      SELECT SINGLE name1 INTO zcte_seguro-xseg
        FROM lfa1
       WHERE lifnr EQ zcte_seguro-resp_codigo.
    ENDIF.

  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_1702  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1702  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_1702 INPUT.

  DATA: answer_seguro   TYPE c LENGTH 1,
        validado_seguro TYPE c LENGTH 1.

  CASE cte_ok_code.
    WHEN cte_incluir.
      CLEAR: zcte_seguro.
      zcte_seguro-docnum = cte_j_1bnfdoc-docnum.
      CLEAR: cte_alterado.
      cte_alterando = 'I'.
    WHEN cte_alterar.
      CLEAR: cte_alterado.
      cte_alterando = 'A'.
    WHEN cte_apagar.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-004
        IMPORTING
          answer    = answer_moto.

      CASE answer_seguro.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.

      DELETE FROM zcte_seguro WHERE cd_seguro EQ zcte_seguro-cd_seguro.
      DELETE it_cte_seguro    WHERE cd_seguro EQ zcte_seguro-cd_seguro.

      CLEAR: zcte_seguro.

    WHEN cte_confirma.
      PERFORM verificar_insert_seguro USING zcte_seguro validado_seguro.
      IF validado_seguro EQ c_x.
        MODIFY zcte_seguro.

        IF cte_alterando EQ 'I'.

          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'ZCTESEG'
            IMPORTING
              number                  = zcte_seguro-cd_seguro
            EXCEPTIONS
              interval_not_found      = 1
              number_range_not_intern = 2
              object_not_found        = 3
              quantity_is_0           = 4
              quantity_is_not_1       = 5
              interval_overflow       = 6
              buffer_overflow         = 7
              OTHERS                  = 8.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          MOVE-CORRESPONDING zcte_seguro TO cte_seguro.
          APPEND cte_seguro TO it_cte_seguro.
          MODIFY zcte_seguro.
        ELSEIF cte_alterando EQ 'A'.
          READ TABLE it_cte_seguro INTO cte_seguro WITH KEY cd_seguro = zcte_seguro-cd_seguro.
          MOVE-CORRESPONDING zcte_seguro TO cte_seguro.
          MODIFY it_cte_seguro INDEX sy-tabix FROM cte_seguro.
        ENDIF.

        cte_alterando = 'C'.
        CLEAR: cte_alterado.
      ENDIF.
    WHEN cte_cancelar.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-002
          textline2 = TEXT-003
        IMPORTING
          answer    = answer_seguro.

      CASE answer_seguro.
        WHEN 'J'.
          CLEAR: cte_alterado, cte_alterado.
        WHEN 'N'.
          EXIT.
        WHEN 'A'.
          EXIT.
      ENDCASE.

      READ TABLE it_cte_seguro INTO cte_seguro WITH KEY cd_seguro = zcte_seguro-cd_seguro.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING cte_seguro TO zcte_seguro.
      ELSE.
        CLEAR: zcte_seguro.
      ENDIF.
      cte_alterando = 'C'.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1702  INPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICAR_INSERT_SEGURO
*&---------------------------------------------------------------------*
FORM verificar_insert_seguro  USING    p_zcte_seguro TYPE zcte_seguro
                                       p_validado    TYPE c.

  CLEAR: p_validado.

  IF p_zcte_seguro-respseg IS INITIAL.
    MESSAGE i023 WITH 'Deve ser informado o responsável pelo seguro!'.
    EXIT.
  ENDIF.

  p_validado = 'X'.

ENDFORM.                    " VERIFICAR_INSERT_SEGURO


*&---------------------------------------------------------------------*
*&      Form  XML_CTE_ABRE_XML
*&---------------------------------------------------------------------*
FORM xml_cte_abre_xml  USING    p_cte.
  PERFORM ctnab USING ctepadrao p_cte.
  PERFORM ctnab USING intgcte   p_cte.
ENDFORM.                    " XML_CTE_ABRE_XML

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_FECHA_XML
*&---------------------------------------------------------------------*
FORM xml_cte_fecha_xml  USING    p_cte.
  PERFORM ctnfe USING intgcte p_cte.
ENDFORM.                    " XML_CTE_FECHA_XML

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_IDE
*&---------------------------------------------------------------------*
FORM xml_cte_ide  USING    p_cte
                           p_identifica TYPE zcte_identifica
                           p_parceiros  TYPE zcte_parceiros.

  DATA: vg_dt_hora_emi TYPE string,
        p_iniciada     TYPE char01.

  PERFORM ctnab USING cteide     p_cte.

  PERFORM ctnav USING ctecfop    p_identifica-cfop(4) p_cte.
  PERFORM ctnav USING ctenatop   p_identifica-cfotxt  p_cte.

  IF vg_versao_cte < 3. "CS2017002143 CT-e 3.0 Ini
    PERFORM ctnav USING cteforpag  p_identifica-forpag  p_cte.
  ENDIF. "CS2017002143 CT-e 3.0 - Fim

  PERFORM ctnav USING cteserie   p_identifica-serie   p_cte.
  PERFORM ctnav USING ctenct     p_identifica-nct     p_cte.

  CONCATENATE p_identifica-dhemi(4) '-' p_identifica-dhemi+4(2) '-' p_identifica-dhemi+6(2) '*'
              p_identifica-hremi(2) ':' p_identifica-hremi+2(2) ':' p_identifica-hremi+4(2) INTO vg_dt_hora_emi.
  REPLACE '*' WITH space INTO vg_dt_hora_emi.

  PERFORM ctnav USING ctedhemi   vg_dt_hora_emi       p_cte.
  PERFORM ctnav USING ctetpcte   p_identifica-tpcte   p_cte.
  PERFORM ctnaf USING cterefcte  ''                   p_cte.
  PERFORM ctnav USING ctemodal   p_identifica-modal   p_cte.
  PERFORM ctnav USING ctetpserv  p_identifica-tpserv  p_cte.

  IF NOT xml_cte_1_04 IS INITIAL.
    PERFORM ctnav USING ctecmunenv p_identifica-cmunenv p_cte.
  ENDIF.

  PERFORM ctnav USING ctecmunini p_identifica-cmunini p_cte.
  PERFORM ctnav USING ctecmunfim p_identifica-cmunfim p_cte.
  PERFORM ctnaf USING cteretira  p_identifica-retira  p_cte.

  IF vg_versao_cte >= 3. "CS2017002143 CT-e 3.0 Ini
    CASE p_identifica-toma.
      WHEN c_0. "Remetenete
        SELECT SINGLE * INTO @DATA(wa_remetente) FROM lfa1 WHERE lifnr EQ @p_parceiros-reme_codigo.
        PERFORM ctnaf USING indietoma  '1'  p_cte.
        IF wa_remetente-stcd3 IS INITIAL OR wa_remetente-stcd3 EQ 'ISENTO'.
          MESSAGE e134(zles) WITH p_parceiros-reme_codigo.
          "RAISE EXCEPTION TYPE ZCX_CTE.
        ENDIF.
        "WHEN C_1. "Expedidor
        "WHEN C_2. "Recebedor
      WHEN c_3. "Destinatario
        SELECT SINGLE * INTO @DATA(wa_destinatario) FROM kna1 WHERE kunnr EQ @p_parceiros-dest_codigo.
        PERFORM ctnaf USING indietoma  '1'  p_cte.
        IF wa_destinatario-stcd3 IS INITIAL OR wa_destinatario-stcd3 EQ 'ISENTO'.
          MESSAGE e134(zles) WITH p_parceiros-dest_codigo.
          "RAISE EXCEPTION TYPE ZCX_CTE.
        ENDIF.
      WHEN c_4. "Outros
        SELECT SINGLE * INTO @DATA(wa_tomador) FROM lfa1 WHERE lifnr EQ @p_parceiros-toma_codigo.
        PERFORM ctnaf USING indietoma  '1'  p_cte.
        IF wa_tomador-stcd3 IS INITIAL OR wa_tomador-stcd3 EQ 'ISENTO'.
          MESSAGE e134(zles) WITH p_parceiros-toma_codigo.
          "RAISE EXCEPTION TYPE ZCX_CTE.
        ENDIF.
    ENDCASE.
  ENDIF. "CS2017002143 CT-e 3.0 - Fim

  PERFORM ctnav USING ctetoma    p_identifica-toma    p_cte.

  IF p_identifica-toma EQ c_4.
    PERFORM ctnab USING ctetomaoutros     p_cte.

    PERFORM ctnaf USING ctetomacnpj       p_parceiros-toma_cnpj  p_cte.
    PERFORM ctnaf USING ctetomacpf        p_parceiros-toma_cpf   p_cte.
    PERFORM ctnav USING ctetomaie         p_parceiros-toma_ie    p_cte.
    PERFORM ctnav USING ctetomaxnome      p_parceiros-toma_xnome p_cte.
    PERFORM ctnaf USING ctetomaxfan       p_parceiros-toma_xfant p_cte.
    IF p_parceiros-toma_fone IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN p_parceiros-toma_fone WITH ''.
    ENDIF.
    PERFORM ctnaf USING ctetomafone       p_parceiros-toma_fone  p_cte.

    "Endereço do Tom
    PERFORM ctnab USING ctetomaendertoma  p_cte.
    PERFORM ctnav USING ctetomaxlgr       p_parceiros-toma_xlgr    p_cte.
    PERFORM ctnav USING ctetomaxnum       p_parceiros-toma_nro     p_cte.
    PERFORM ctnaf USING ctetomaxcpl       p_parceiros-toma_xcpl    p_cte.
    PERFORM ctnav USING ctetomaxbairro    p_parceiros-toma_xbairro p_cte.
    PERFORM ctnav USING ctetomacmun       p_parceiros-toma_cmun    p_cte.
    IF p_parceiros-toma_cep IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN p_parceiros-toma_cep WITH ''.
    ENDIF.
    PERFORM ctnav USING ctetomacep        p_parceiros-toma_cep     p_cte.

    PERFORM ctnfe USING ctetomaendertoma  p_cte.
    PERFORM ctnfe USING ctetomaoutros     p_cte.

  ENDIF.

  PERFORM ctnfe USING cteide     p_cte.

ENDFORM.                    " XML_CTE_IDE

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_OBSCONT
*&---------------------------------------------------------------------*
FORM xml_cte_compl  TABLES  p_it_obs_geral STRUCTURE zcte_obs_gerais
                            p_it_ciot      STRUCTURE zcte_ciot
                     USING  p_identifica   TYPE zcte_identifica
                            p_cte.

  DATA: wa_it_obs_geral    TYPE zcte_obs_gerais,
        tx_obs_geral       TYPE string,
        tx_ciot            TYPE string,
        wa_it_ciot         TYPE zcte_ciot,
        vg_contrato_viagem TYPE znucontrato,
        vg_kinak           TYPE kinak,
        vg_kwert           TYPE kwert,
        valor_pedagio      TYPE c LENGTH 20,
        texto_pedagio      TYPE string,
        "CS2017002728  - Ini
        it_cte_trans       TYPE TABLE OF zcte_trans INITIAL SIZE 0 WITH HEADER LINE,
        it_cte_moto        TYPE TABLE OF zcte_motorista INITIAL SIZE 0 WITH HEADER LINE,
        wl_doc_transp      TYPE vttk,
        vl_xtexto          TYPE string,
        "CS2017002728  - Fim
        it_vfkp            TYPE TABLE OF vfkp WITH HEADER LINE,
        it_konv            TYPE TABLE OF konv WITH HEADER LINE,
        it_vttp            TYPE TABLE OF vttp WITH HEADER LINE,
        it_vbfa            TYPE TABLE OF vbfa WITH HEADER LINE,
        it_vbkd            TYPE TABLE OF vbkd WITH HEADER LINE,
        wl_vbkd            TYPE vbkd.



  CLEAR: tx_obs_geral.

  SORT p_it_obs_geral BY linha.

  LOOP AT p_it_obs_geral INTO wa_it_obs_geral.
    IF tx_obs_geral IS INITIAL.
      tx_obs_geral = wa_it_obs_geral-texto.
    ELSE.
      CONCATENATE tx_obs_geral wa_it_obs_geral-texto INTO tx_obs_geral SEPARATED BY space.
    ENDIF.
  ENDLOOP.

  CLEAR: texto_pedagio, vg_kwert.

  vg_kwert = 0.

  SELECT *
    FROM vfkp
    INTO CORRESPONDING FIELDS OF TABLE it_vfkp
   WHERE refty EQ c_8
     AND rebel EQ p_identifica-tknum.

  IF NOT it_vfkp[] IS INITIAL.

* ---> S4 Migration - 07/07/2023 - JP
*    SELECT *
*      FROM konv
*      INTO CORRESPONDING FIELDS OF TABLE it_konv
*      FOR ALL ENTRIES IN it_vfkp
*     WHERE knumv EQ it_vfkp-knumv
*       AND kschl EQ c_zped
*       AND kinak EQ vg_kinak
*       AND kwert GT 0.

    SELECT *
      FROM v_konv
      INTO TABLE @DATA(it_konv_aux)
      FOR ALL ENTRIES IN @it_vfkp
     WHERE knumv EQ @it_vfkp-knumv
       AND kschl EQ @c_zseg
       AND kinak EQ @vg_kinak
       AND kwert GT 0.

    MOVE-CORRESPONDING it_konv_aux[] TO it_konv[].

* <--- S4 Migration - 07/07/2023 - JP

    IF sy-subrc EQ 0.
      LOOP AT it_konv.
        IF it_konv-kwert GT 0.
          vg_kwert = vg_kwert + it_konv-kwert.
        ENDIF.
      ENDLOOP.
      IF vg_kwert GT 0.
        WRITE vg_kwert TO valor_pedagio.
        SHIFT valor_pedagio LEFT DELETING LEADING space.
        CONCATENATE 'Valor do pedágio:' valor_pedagio INTO texto_pedagio SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDIF.

  IF NOT texto_pedagio IS INITIAL.
    IF tx_obs_geral IS INITIAL.
      tx_obs_geral = texto_pedagio.
    ELSE.
      CONCATENATE tx_obs_geral texto_pedagio INTO tx_obs_geral SEPARATED BY space.
    ENDIF.
  ENDIF.

  CLEAR: vg_contrato_viagem.

  LOOP AT p_it_ciot INTO wa_it_ciot.

    vg_contrato_viagem = wa_it_ciot-nucontrato.
    IF NOT wa_it_ciot-nr_ciot IS INITIAL.
      CONCATENATE 'Nr. CIOT:' wa_it_ciot-nr_ciot INTO tx_ciot SEPARATED BY space.

      IF tx_obs_geral IS INITIAL.
        tx_obs_geral = tx_ciot.
      ELSE.
        CONCATENATE tx_obs_geral tx_ciot INTO tx_obs_geral SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF NOT vg_contrato_viagem IS INITIAL.
    CONCATENATE 'Nr. Contrato de Viagem Administradora:' vg_contrato_viagem INTO tx_ciot SEPARATED BY space.

    IF tx_obs_geral IS INITIAL.
      tx_obs_geral = tx_ciot.
    ELSE.
      CONCATENATE tx_obs_geral tx_ciot INTO tx_obs_geral SEPARATED BY space.
    ENDIF.
  ENDIF.

  "Recuperar o Número do Pedido (DCO/Aviso) digitado pelo usuário - inicio
  SELECT * FROM vttp
    INTO CORRESPONDING FIELDS OF TABLE it_vttp
   WHERE tknum EQ p_identifica-tknum.

  "ALRS 06.01.2015
  IF it_vttp[] IS NOT INITIAL.
    SELECT * FROM vbfa
      INTO CORRESPONDING FIELDS OF TABLE it_vbfa
      FOR ALL ENTRIES IN it_vttp
    WHERE vbeln EQ it_vttp-vbeln.

    IF it_vbfa[] IS NOT INITIAL.
      SELECT * FROM vbkd
       INTO CORRESPONDING FIELDS OF TABLE it_vbkd
       FOR ALL ENTRIES IN it_vbfa
      WHERE vbeln EQ it_vbfa-vbelv
        AND posnr EQ '000010'.
    ENDIF.

    IF NOT it_vbkd[] IS INITIAL.
      LOOP AT it_vbkd INTO wl_vbkd.
        CONCATENATE tx_obs_geral wl_vbkd-bstkd_e INTO tx_obs_geral SEPARATED BY space.
      ENDLOOP.
    ENDIF.
  ENDIF.
  "Recuperar o Número do Pedido (DCO/Aviso) digitado pelo usuário - fim

  "CS2017002728  - Ini
  CLEAR: it_cte_trans[], it_cte_moto[], wl_doc_transp.

  CALL FUNCTION 'Z_SD_INFO_CTE_TRANS'
    EXPORTING
      p_cte_avulso = p_identifica-docnum
    TABLES
      it_cte_trans = it_cte_trans.

  CALL FUNCTION 'Z_SD_INFO_CTE_MOTO'
    EXPORTING
      p_cte_avulso = p_identifica-docnum
    TABLES
      it_cte_moto  = it_cte_moto.

  SELECT SINGLE *
    FROM vttk INTO wl_doc_transp
   WHERE tknum = p_identifica-tknum.
  "CS2017002728  - Fim

  IF tx_obs_geral IS NOT INITIAL.
    PERFORM ctnab USING ctecompl      p_cte.
    PERFORM ctnab USING ctexobs       p_cte.
    PERFORM ctnao USING tx_obs_geral  p_cte.
    PERFORM ctnfe USING ctexobs       p_cte.


    "CS2017002728  - Ini
    IF ( it_cte_trans[] IS NOT INITIAL ) AND ( wl_doc_transp IS NOT INITIAL ) AND ( p_identifica-docnum IS NOT INITIAL ).

      "Placa do cavalo
      IF wl_doc_transp-text1 IS NOT INITIAL.
        READ TABLE it_cte_trans INTO DATA(_wl_cte_trans) WITH KEY pc_veiculo = wl_doc_transp-text1.
        IF sy-subrc = 0.
          PERFORM ctnab2 USING cteobscont ctexcampo 'Placa Cavalo' p_cte.
          PERFORM formata_texto_veiculo USING _wl_cte_trans CHANGING vl_xtexto.
          PERFORM ctnav USING ctextexto vl_xtexto  p_cte.
          PERFORM ctnfe USING cteobscont    p_cte.
        ENDIF.
      ENDIF.

      "Placa Carreta 1
      IF wl_doc_transp-text2 IS NOT INITIAL.
        READ TABLE it_cte_trans INTO _wl_cte_trans WITH KEY pc_veiculo = wl_doc_transp-text2.
        IF sy-subrc = 0.
          PERFORM ctnab2 USING cteobscont ctexcampo 'Placa Carreta 1' p_cte.
          PERFORM formata_texto_veiculo USING _wl_cte_trans CHANGING vl_xtexto.
          PERFORM ctnav USING ctextexto vl_xtexto  p_cte.
          PERFORM ctnfe USING cteobscont    p_cte.
        ENDIF.
      ENDIF.

      "Placa Carreta 2
      IF wl_doc_transp-text3 IS NOT INITIAL.
        READ TABLE it_cte_trans INTO _wl_cte_trans WITH KEY pc_veiculo = wl_doc_transp-text3.
        IF sy-subrc = 0.
          PERFORM ctnab2 USING cteobscont ctexcampo 'Placa Carreta 2' p_cte.
          PERFORM formata_texto_veiculo USING _wl_cte_trans CHANGING vl_xtexto.
          PERFORM ctnav USING ctextexto vl_xtexto  p_cte.
          PERFORM ctnfe USING cteobscont    p_cte.
        ENDIF.
      ENDIF.

      "Placa Carreta 3
      IF wl_doc_transp-text4 IS NOT INITIAL.
        READ TABLE it_cte_trans INTO _wl_cte_trans WITH KEY pc_veiculo = wl_doc_transp-text4.
        IF sy-subrc = 0.
          PERFORM ctnab2 USING cteobscont ctexcampo 'Placa Carreta 3' p_cte.
          PERFORM formata_texto_veiculo USING _wl_cte_trans CHANGING vl_xtexto.
          PERFORM ctnav USING ctextexto vl_xtexto  p_cte.
          PERFORM ctnfe USING cteobscont    p_cte.
        ENDIF.
      ENDIF.

      LOOP AT it_cte_moto INTO DATA(_wl_cte_moto).
        PERFORM ctnab2 USING cteobscont ctexcampo 'Motorista' p_cte.
        CONCATENATE _wl_cte_moto-xnome '- CPF:' _wl_cte_moto-cpf INTO vl_xtexto.
        PERFORM ctnav USING ctextexto vl_xtexto  p_cte.
        PERFORM ctnfe USING cteobscont    p_cte.
      ENDLOOP.

    ENDIF.
    "CS2017002728  - Fim

    PERFORM ctnfe USING ctecompl      p_cte.
  ENDIF.

ENDFORM.                    " XML_CTE_OBSCONT

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_EMIT
*&---------------------------------------------------------------------*
FORM xml_cte_emit  USING    p_cte
                            p_parceiros TYPE zcte_parceiros.

  PERFORM ctnab USING cteemit     p_cte.
  PERFORM ctnav USING cteemitcnpj p_parceiros-emit_cnpj p_cte.
  PERFORM limpa_numero USING p_parceiros-emit_ie.
  PERFORM ctnav USING cteemitie   vg_limpo p_cte.
  PERFORM ctnfe USING cteemit     p_cte.

ENDFORM.                    " XML_CTE_EMIT

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_REME
*&---------------------------------------------------------------------*
FORM xml_cte_reme TABLES p_it_notas_info STRUCTURE zcte_info_nota
                   USING p_cte
                         p_parceiros TYPE zcte_parceiros.




  PERFORM ctnab USING cterem      p_cte.
  PERFORM ctnaf USING cteremcnpj  p_parceiros-reme_cnpj  p_cte.
  PERFORM ctnaf USING cteremcpf   p_parceiros-reme_cpf   p_cte.
  PERFORM ctnav USING cteremie    p_parceiros-reme_ie    p_cte.

  IF vg_ambiente = 'QAS'.
    PERFORM ctnav USING cteremxnome 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL' p_cte.
  ELSE.
    PERFORM ctnav USING cteremxnome p_parceiros-reme_xnome p_cte.
  ENDIF.

  PERFORM ctnav USING cteremxnome p_parceiros-reme_xnome p_cte.
  PERFORM ctnaf USING cteremxfant p_parceiros-reme_xfant p_cte.
  IF p_parceiros-reme_fone IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN p_parceiros-reme_fone WITH ''.
  ENDIF.
  PERFORM ctnaf USING cteremfone  p_parceiros-reme_fone  p_cte.

  PERFORM ctnab USING cteremenderreme  p_cte.
  PERFORM ctnav USING cteremxlgr       p_parceiros-reme_xlgr    p_cte.
  PERFORM ctnav USING cteremnro        p_parceiros-reme_nro     p_cte.
  PERFORM ctnaf USING cteremxcpl       p_parceiros-reme_xcpl    p_cte.
  PERFORM ctnav USING cteremxbairro    p_parceiros-reme_xbairro p_cte.
  PERFORM ctnav USING cteremcmun       p_parceiros-reme_cmun    p_cte.
  IF p_parceiros-reme_cep IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN p_parceiros-reme_cep WITH ''.
  ENDIF.
  PERFORM ctnaf USING cteremcep        p_parceiros-reme_cep     p_cte.
  PERFORM ctnfe USING cteremenderreme  p_cte.

  PERFORM xml_cte_reme_nf TABLES p_it_notas_info USING p_cte p_parceiros.

  PERFORM ctnfe USING cterem           p_cte.

ENDFORM.                    " XML_CTE_REME

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_REME_NF
*&---------------------------------------------------------------------*
FORM xml_cte_reme_nf  TABLES   p_it_notas_info STRUCTURE zcte_info_nota
                      USING    p_cte
                               p_parceiros TYPE zcte_parceiros.

  DATA: wa_notas_info TYPE zcte_info_nota,
        tx_data       TYPE c LENGTH 10.

  LOOP AT p_it_notas_info INTO wa_notas_info.
    CLEAR: tx_data.
    IF wa_notas_info-modelo EQ '55'.
      PERFORM ctnab USING cteinfnfe   p_cte.
      PERFORM ctnav USING cteinfchave wa_notas_info-chave       p_cte.
      PERFORM ctnaf USING cteinfpin   wa_notas_info-pin_suframa p_cte.
      PERFORM ctnfe USING cteinfnfe   p_cte.
    ELSE.
      CONCATENATE wa_notas_info-dtemissao(4) '-' wa_notas_info-dtemissao+4(2) '-' wa_notas_info-dtemissao+6(2) INTO tx_data.

      IF wa_notas_info-unidade NE 'KG'.
        CALL FUNCTION 'ME_CONVERSION_MEINS'
          EXPORTING
            i_matnr             = wa_notas_info-material
            i_mein1             = wa_notas_info-unidade
            i_meins             = 'KG'
            i_menge             = wa_notas_info-quantidade
          IMPORTING
            menge               = wa_notas_info-quantidade
          EXCEPTIONS
            error_in_conversion = 1
            no_success          = 2
            OTHERS              = 3.
      ENDIF.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF ( wa_notas_info-unidade NE 'KG' ) AND ( wa_notas_info-peso_fiscal > 0 ).
        CALL FUNCTION 'ME_CONVERSION_MEINS'
          EXPORTING
            i_matnr             = wa_notas_info-material
            i_mein1             = wa_notas_info-unidade
            i_meins             = 'KG'
            i_menge             = wa_notas_info-peso_fiscal
          IMPORTING
            menge               = wa_notas_info-peso_fiscal
          EXCEPTIONS
            error_in_conversion = 1
            no_success          = 2
            OTHERS              = 3.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.

      PERFORM ctnab  USING cteinfnf      p_cte.
      PERFORM ctnaf  USING cteinfnfnroma space p_cte.
      PERFORM ctnaf  USING cteinfnfnped  space p_cte.
      IF NOT xml_cte_1_04 IS INITIAL.
        PERFORM ctnav  USING cteinfnfmod   wa_notas_info-modelo       p_cte.
      ENDIF.
      PERFORM ctnav  USING cteinfnfserie wa_notas_info-serie          p_cte.
      PERFORM ctnav  USING cteinfnfndoc  wa_notas_info-numero         p_cte.
      PERFORM ctnav  USING cteinfnfdemi  tx_data                      p_cte.
      PERFORM ctnavn USING cteinfnfvbc   wa_notas_info-vl_bc          p_cte.
      PERFORM ctnavn USING cteinfnfvicms wa_notas_info-vl_icms        p_cte.
      PERFORM ctnavn USING cteinfnfvbcst wa_notas_info-vl_bc_st       p_cte.
      PERFORM ctnavn USING cteinfnfvst   wa_notas_info-vl_st          p_cte.
      PERFORM ctnavn USING cteinfnfvprod wa_notas_info-vl_produtos    p_cte.
      PERFORM ctnavn USING cteinfnfvnf   wa_notas_info-vl_nota_fiscal p_cte.
      PERFORM ctnav  USING cteinfnfncfop wa_notas_info-cfop(4)        p_cte.

      IF wa_notas_info-peso_fiscal > 0.
        PERFORM ctnafv USING cteinfnfnpeso  wa_notas_info-peso_fiscal p_cte.
      ELSE.
        PERFORM ctnafv USING cteinfnfnpeso  wa_notas_info-quantidade  p_cte.
      ENDIF.

      PERFORM ctnaf  USING cteinfnfpin   wa_notas_info-pin_suframa    p_cte.

      IF ( p_parceiros-reme_codigo NE p_parceiros-cole_codigo ) AND ( NOT p_parceiros-cole_codigo IS INITIAL ).
        PERFORM ctnab USING ctelocret         p_cte.
        PERFORM ctnaf USING ctelocretcnpj     p_parceiros-cole_cnpj    p_cte.
        PERFORM ctnaf USING ctelocretcpf      p_parceiros-cole_cpf     p_cte.
        PERFORM ctnav USING ctelocretxnome    p_parceiros-cole_xnome   p_cte.
        PERFORM ctnav USING ctelocretxlgr     p_parceiros-cole_xlgr    p_cte.
        PERFORM ctnav USING ctelocretnro      p_parceiros-cole_nro     p_cte.
        PERFORM ctnaf USING ctelocretxcpl     p_parceiros-cole_xcpl    p_cte.
        PERFORM ctnav USING ctelocretxxbairro p_parceiros-cole_xbairro p_cte.
        PERFORM ctnav USING ctelocretxcmun    p_parceiros-cole_cmun    p_cte.
        PERFORM ctnfe USING ctelocret         p_cte.
      ENDIF.

      PERFORM ctnfe  USING cteinfnf      p_cte.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " XML_CTE_REME_NF

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_DEST
*&---------------------------------------------------------------------*
FORM xml_cte_dest  USING    p_cte
                            p_parceiros TYPE zcte_parceiros.

  PERFORM ctnab USING ctedestremdest p_cte.

  PERFORM ctnaf USING ctedestremcnpj  p_parceiros-dest_cnpj  p_cte.
  PERFORM ctnaf USING ctedestremcpf   p_parceiros-dest_cpf   p_cte.
  PERFORM ctnav USING ctedestremie    p_parceiros-dest_ie    p_cte.

  IF vg_ambiente = 'QAS'.
    PERFORM ctnav USING ctedestremxnome 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL' p_cte.
  ELSE.
    PERFORM ctnav USING ctedestremxnome p_parceiros-dest_xnome p_cte.
  ENDIF.

  IF p_parceiros-dest_fone IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN p_parceiros-dest_fone WITH ''.
  ENDIF.
  PERFORM ctnaf USING ctedestremfone  p_parceiros-dest_fone  p_cte.
  PERFORM ctnaf USING ctedestremisuf  space                  p_cte.

  PERFORM ctnab USING cteenddestenderdest  p_cte.
  PERFORM ctnav USING cteenddestxlgr    p_parceiros-dest_xlgr    p_cte.
  PERFORM ctnav USING cteenddestnro     p_parceiros-dest_nro     p_cte.
  PERFORM ctnaf USING cteenddestxcpl    p_parceiros-dest_xcpl    p_cte.
  PERFORM ctnav USING cteenddestxbairro p_parceiros-dest_xbairro p_cte.
  PERFORM ctnav USING cteenddestcmun    p_parceiros-dest_cmun    p_cte.
  IF p_parceiros-dest_cep IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN p_parceiros-dest_cep WITH ''.
  ENDIF.
  PERFORM ctnav USING cteenddestcep     p_parceiros-dest_cep     p_cte.
  PERFORM ctnfe USING cteenddestenderdest p_cte.

  PERFORM ctnfe USING ctedestremdest p_cte.

ENDFORM.                    " XML_CTE_DEST

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_VPREST
*&---------------------------------------------------------------------*
FORM xml_cte_vprest  USING    p_cte
                              p_identifica TYPE zcte_identifica.

  PERFORM ctnab USING  ctevlrpserv        p_cte.

  IF ( p_identifica-vtprest EQ p_identifica-vrec ).
    PERFORM ctnavn USING ctevlrpservvtprest p_identifica-vtprest p_cte.
    PERFORM ctnavn USING ctevlrpservvrec    p_identifica-vrec    p_cte.
  ELSE.
    PERFORM ctnavn USING ctevlrpservvtprest p_identifica-vrec    p_cte.
    PERFORM ctnavn USING ctevlrpservvrec    p_identifica-vrec    p_cte.

  ENDIF.

  PERFORM ctnab  USING ctecompcomp        p_cte.
  PERFORM ctnav  USING ctecompxnome       c_frete_peso         p_cte.
  IF ( p_identifica-vtprest EQ p_identifica-vrec ).
    PERFORM ctnavn USING ctecompvcomp       p_identifica-vtprest p_cte.
  ELSE.
    PERFORM ctnavn USING ctecompvcomp       p_identifica-vrec p_cte.
  ENDIF.
  PERFORM ctnfe  USING ctecompcomp        p_cte.

  PERFORM ctnfe  USING ctevlrpserv        p_cte.

ENDFORM.                    " XML_CTE_VPREST

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_IMP
*&---------------------------------------------------------------------*
FORM xml_cte_imp  USING    p_cte
                           p_identifica TYPE zcte_identifica.

  PERFORM ctnab USING cteimpimp           p_cte.
  PERFORM ctnab USING cteimpicms          p_cte.

  PERFORM ctnav  USING cteimpcst          p_identifica-cst   p_cte.
  PERFORM ctnafv USING cteimppicms        p_identifica-picms p_cte.
  PERFORM ctnafv USING cteimpvbc          p_identifica-vbc   p_cte.
  PERFORM ctnafv USING cteimpvicms        p_identifica-vicms p_cte.

  PERFORM ctnafv USING cteimppredbc       p_identifica-predbc  p_cte.
  PERFORM ctnafv USING cteimpvcred        p_identifica-vcred   p_cte.

  PERFORM ctnfe USING cteimpicms          p_cte.
  PERFORM ctnfe USING cteimpimp           p_cte.

ENDFORM.                    " XML_CTE_IMP

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_ABRE_NORM
*&---------------------------------------------------------------------*
FORM xml_cte_abre_norm  USING    p_cte.

  PERFORM ctnab USING cteinfinfctenorm  p_cte.

ENDFORM.                    " XML_CTE_ABRE_NORM

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_FECHA_NORM
*&---------------------------------------------------------------------*
FORM xml_cte_fecha_norm  USING    p_cte.

  PERFORM ctnfe USING cteinfinfctenorm    p_cte.

ENDFORM.                    " XML_CTE_FECHA_NORM

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_INFCARGA
*&---------------------------------------------------------------------*
FORM xml_cte_infcarga TABLES p_it_notas_info STRUCTURE zcte_info_nota
                       USING p_cte.

  DATA: wa_notas_info TYPE zcte_info_nota,
        vl_tot_merc   LIKE zcte_info_nota-vl_nota_fiscal,
        vl_tot_qtde   LIKE zcte_info_nota-quantidade,
        vl_maktx      LIKE makt-maktx.

  CLEAR: vl_tot_merc, vl_tot_qtde.
  LOOP AT p_it_notas_info INTO wa_notas_info.
    ADD wa_notas_info-vl_nota_fiscal TO vl_tot_merc.
    ADD wa_notas_info-quantidade     TO vl_tot_qtde.
  ENDLOOP.

  PERFORM ctnab USING ctecargainfcarga              p_cte.
  IF NOT xml_cte_1_04 IS INITIAL.
    PERFORM ctnavn USING ctecargavcarga vl_tot_merc p_cte.
  ELSE.
    PERFORM ctnavn USING ctecargavmerc vl_tot_merc p_cte.
  ENDIF.

  CHECK wa_notas_info-material IS NOT INITIAL.

  SELECT SINGLE maktx INTO vl_maktx
    FROM makt
   WHERE matnr = wa_notas_info-material
     AND spras = sy-langu.

  PERFORM ctnavn USING ctecargapropred  vl_maktx    p_cte.

  PERFORM ctnab USING cteqtdeinfq         p_cte.

  LOOP AT p_it_notas_info INTO wa_notas_info.

    CASE wa_notas_info-unidade.
      WHEN c_kg.
        PERFORM ctnav USING cteqtdecunid    c_01              p_cte.
        PERFORM ctnav USING cteqtdetpmed    c_peso_bruto      p_cte.
      WHEN c_to.
        PERFORM ctnav USING cteqtdecunid    c_02              p_cte.
        PERFORM ctnav USING cteqtdetpmed    c_peso_bruto      p_cte.
      WHEN c_un.
        PERFORM ctnav USING cteqtdecunid    c_03              p_cte.
        PERFORM ctnav USING cteqtdetpmed    c_caixa           p_cte.
      WHEN c_lt.
        PERFORM ctnav USING cteqtdecunid    c_04              p_cte.
        PERFORM ctnav USING cteqtdetpmed    c_litragem        p_cte.
    ENDCASE.

    PERFORM ctnavn USING cteqtdeqcarga  vl_tot_qtde p_cte.

    EXIT.
  ENDLOOP.

  PERFORM ctnfe USING cteqtdeinfq      p_cte.
  PERFORM ctnfe USING ctecargainfcarga p_cte.

ENDFORM.                    " XML_CTE_INFCARGA

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_SEG
*&---------------------------------------------------------------------*
FORM xml_cte_seg  TABLES   p_it_seguro STRUCTURE zcte_seguro
                  USING    p_cte.

  DATA: wa_seguro TYPE zcte_seguro,
        nm_seguro TYPE c LENGTH 30.

  CHECK vg_versao_cte < 3. "CS2017002143 CT-e 3.0

  LOOP AT p_it_seguro INTO wa_seguro.
    PERFORM ctnab USING cteseg     p_cte.
    PERFORM ctnav USING cterespseg wa_seguro-respseg p_cte.
    MOVE wa_seguro-xseg TO nm_seguro.
    PERFORM ctnaf USING ctexseg    nm_seguro         p_cte.
    PERFORM ctnaf USING ctenapol   wa_seguro-napol   p_cte.
    PERFORM ctnfe USING cteseg     p_cte.
  ENDLOOP.

ENDFORM.                    " XML_CTE_SEG

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_ABRE_RODO
*&---------------------------------------------------------------------*
FORM xml_cte_abre_rodo  USING    p_cte.

  PERFORM ctnab USING ctemodalrodo p_cte.

ENDFORM.                    " XML_CTE_ABRE_RODO

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_FECHA_RODO
*&---------------------------------------------------------------------*
FORM xml_cte_fecha_rodo  USING    p_cte.

  PERFORM ctnfe USING ctemodalrodo p_cte.

ENDFORM.                    " XML_CTE_FECHA_RODO

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_RODO
*&---------------------------------------------------------------------*
FORM xml_cte_rodo  TABLES p_it_trans STRUCTURE zcte_trans
                    USING p_cte
                          p_identifica TYPE zcte_identifica.
  DATA: tx_dprev TYPE c LENGTH 10,
        wa_ciot  TYPE zcte_ciot,
        wa_trans TYPE zcte_trans.

  CONCATENATE p_identifica-rodo_dt_prev(4) '-' p_identifica-rodo_dt_prev+4(2) '-' p_identifica-rodo_dt_prev+6(2) INTO tx_dprev.
  PERFORM ctnav USING ctemodalrntrc  p_identifica-rodo_rntrc      p_cte.

  IF vg_versao_cte < 3. "CS2017002143 CT-e 3.0 Ini
    PERFORM ctnav USING ctemodaldprev  tx_dprev                     p_cte.
    PERFORM ctnav USING ctemodallota   p_identifica-rodo_frete_lota p_cte.

    "CIOT do Veículo de Tração
    READ TABLE p_it_trans INTO wa_trans WITH KEY tp_veiculo = c_0.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE * INTO wa_ciot
      FROM zcte_ciot
     WHERE docnum    EQ p_identifica-docnum
       AND rntrc     EQ wa_trans-prop_rntrc
       AND cancelado NE c_x.

    CHECK sy-subrc IS INITIAL.

    PERFORM ctnav USING ctemodalciot  wa_ciot-nr_ciot  p_cte.
  ENDIF. "CS2017002143 CT-e 3.0 - Fim


ENDFORM.                    " XML_CTE_RODO

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_VALEPED
*&---------------------------------------------------------------------*
FORM xml_cte_valeped  USING    p_cte
                               p_identifica TYPE zcte_identifica.

  CHECK vg_versao_cte < 3. "CS2017002143 CT-e 3.0

  IF xml_cte_1_04 IS INITIAL.
    IF p_identifica-vlr_pedagio GE 0.
      PERFORM ctnab  USING ctevaleped   p_cte.
      PERFORM ctnavn USING ctevtvaleped p_identifica-vlr_pedagio  p_cte.
      PERFORM ctnav  USING cteresppg    p_identifica-rsp_pedagio  p_cte.
      PERFORM ctnfe  USING ctevaleped   p_cte.
    ENDIF.
  ENDIF.

ENDFORM.                    " XML_CTE_VALEPED

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_VEIC
*&---------------------------------------------------------------------*
FORM xml_cte_veic  TABLES p_it_trans STRUCTURE zcte_trans
                   USING  p_cte
                          p_identifica TYPE zcte_identifica.

  DATA: wa_trans TYPE zcte_trans.

  CHECK vg_versao_cte < 3. "CS2017002143 CT-e 3.0

  LOOP AT p_it_trans INTO wa_trans.
    PERFORM ctnab  USING cteveicveic       p_cte.
    PERFORM ctnav  USING cteveicrenavam    wa_trans-cd_renavam     p_cte.
    PERFORM ctnav  USING cteveicplaca      wa_trans-pc_veiculo     p_cte.
    PERFORM ctnavn USING cteveictara       wa_trans-tara           p_cte.
    PERFORM ctnav  USING cteveiccapkg      wa_trans-cap_kg         p_cte.
    PERFORM ctnav  USING cteveiccapm3      wa_trans-cap_m3         p_cte.

    IF wa_trans-prop_rntrc EQ p_identifica-rodo_rntrc.
      PERFORM ctnav  USING cteveictpprop  c_p  p_cte.
    ELSE.
      PERFORM ctnav  USING cteveictpprop  c_t  p_cte.
    ENDIF.

    PERFORM ctnav  USING cteveictpveic     wa_trans-tp_veiculo     p_cte.
    PERFORM ctnav  USING cteveictprod      wa_trans-tp_rodado      p_cte.
    PERFORM ctnav  USING cteveictpcar      wa_trans-tp_carroceria2 p_cte.
    PERFORM ctnav  USING cteveicuf         wa_trans-cd_uf          p_cte.

    PERFORM ctnab  USING ctepropprop       p_cte.
    PERFORM ctnaf  USING ctepropcpf        wa_trans-prop_cpf       p_cte.
    PERFORM ctnaf  USING ctepropcnpj       wa_trans-prop_cnpj      p_cte.
    PERFORM ctnav  USING ctepropcrntrc     wa_trans-prop_rntrc     p_cte.
    PERFORM ctnav  USING ctepropxnome      wa_trans-prop_nome      p_cte.

    IF ( wa_trans-prop_ie IS NOT INITIAL ) AND ( wa_trans-prop_ie NE 'ISENTO' ).
      PERFORM ctnaf  USING ctepropie       wa_trans-prop_ie        p_cte.
      IF ( wa_trans-prop_uf IS NOT INITIAL ).
        PERFORM ctnav  USING ctepropuf     wa_trans-prop_uf        p_cte.
      ENDIF.
    ENDIF.

    PERFORM ctnav  USING cteproptpprop     c_0              p_cte.
    PERFORM ctnafv USING cteemail          wa_trans-e_mail  p_cte.

    PERFORM ctnfe  USING ctepropprop       p_cte.

    PERFORM ctnfe USING cteveicveic        p_cte.
  ENDLOOP.

ENDFORM.                    " XML_CTE_VEIC

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_MOTO
*&---------------------------------------------------------------------*
FORM xml_cte_moto  TABLES   p_it_motorista STRUCTURE zcte_motorista
                   USING    p_cte.

  DATA: wa_motorista TYPE zcte_motorista.

  CHECK vg_versao_cte < 3. "CS2017002143 CT-e 3.0

  LOOP AT p_it_motorista INTO wa_motorista.
    PERFORM ctnab  USING ctemotomoto  p_cte.
    PERFORM lct    USING wa_motorista-xnome.
    PERFORM ctnav  USING ctemotoxnome vg_limpo          p_cte.
    PERFORM ctnav  USING ctemotocpf   wa_motorista-cpf  p_cte.
    PERFORM ctnfe  USING ctemotomoto  p_cte.
  ENDLOOP.

ENDFORM.                    " XML_CTE_MOTO

*&---------------------------------------------------------------------*
*&      Module  STATUS_9970  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9970 OUTPUT.

  DATA: it_zcte_ciot_imp TYPE TABLE OF zcte_ciot_imp,
        ck_carga_pedagio TYPE char01,
        ck_pedagio       TYPE char01.

  IF cte_dynnr_000 IS INITIAL.
    cte_dynnr_000 = cte_c_2105.
  ENDIF.

  CLEAR: cte_it_fcode[].

  ck_pedagio       = abap_false.
  ck_carga_pedagio = abap_false.

  LOOP AT it_cte_ciot INTO cte_ciot.
    IF cte_ciot-st_ciot NE '0' AND cte_ciot-st_ciot NE '3'.
      cte_bloqueado = c_x.
    ENDIF.
    IF cte_ciot-link_pedagio IS NOT INITIAL.
      ck_pedagio = abap_true.
    ENDIF.

    IF cte_ciot-link_carga_pedagio IS NOT INITIAL.
      ck_carga_pedagio = abap_true.
    ENDIF.

  ENDLOOP.

  IF ck_pedagio EQ abap_false.
    cte_wa_fcode = cte_impedagio.
    APPEND cte_wa_fcode TO cte_it_fcode.
  ENDIF.

  IF ck_carga_pedagio EQ abap_false.
    cte_wa_fcode = cte_cargapedagio.
    APPEND cte_wa_fcode TO cte_it_fcode.
  ENDIF.

  CASE cte_dynnr_000.
    WHEN cte_c_2105.

      IF cte_bloqueado EQ c_x.
        IF cte_ciot-st_ciot NE '0' AND cte_ciot-st_ciot NE '1'.
          cte_wa_fcode = cte_todos.
          APPEND cte_wa_fcode TO cte_it_fcode.
        ENDIF.
*        cte_wa_fcode = cte_atualiza.
*        append cte_wa_fcode to cte_it_fcode.
      ENDIF.

      IF ( cte_ciot-st_ciot NE '2' ) AND NOT ( cte_ciot-st_ciot EQ '8' AND cte_autorizado_uso EQ abap_true ).
        cte_wa_fcode = cte_todoscred.
        APPEND cte_wa_fcode TO cte_it_fcode.
      ENDIF.

      IF ( cte_ciot-st_ciot NE '5' ) AND ( cte_ciot-st_ciot NE '6' ).
        cte_wa_fcode = cte_imprimir.
        APPEND cte_wa_fcode TO cte_it_fcode.
        cte_wa_fcode = cte_impresumo.
        APPEND cte_wa_fcode TO cte_it_fcode.
      ENDIF.

      IF NOT ( cte_ciot-st_ciot EQ '2' OR cte_ciot-st_ciot EQ '5' ).
        cte_wa_fcode = cte_todoscanc.
        APPEND cte_wa_fcode TO cte_it_fcode.
      ENDIF.

      info_ciot-activetab = 'TB9970CIOT01'.
      cte_wa_fcode = cte_incluir.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_alterar.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_confirma.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_cancelar.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_apagar.
      APPEND cte_wa_fcode TO cte_it_fcode.

      SELECT * INTO TABLE it_zcte_ciot_imp
        FROM zcte_ciot_imp
       WHERE cd_ciot      EQ cte_ciot-cd_ciot
         AND tp_impressao EQ 'C'.

      IF NOT sy-subrc IS INITIAL.
        cte_wa_fcode = cte_impresumo.
        APPEND cte_wa_fcode TO cte_it_fcode.
        cte_wa_fcode = cte_impedagio.
        APPEND cte_wa_fcode TO cte_it_fcode.
        cte_wa_fcode = cte_cargapedagio.
        APPEND cte_wa_fcode TO cte_it_fcode.
      ENDIF.

    WHEN cte_c_2106.

      info_ciot-activetab = 'TB9970CIOT02'.
      cte_wa_fcode = cte_imprimir.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_impresumo.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_impedagio.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_cargapedagio.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_todos.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_atualiza.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_todoscred.
      APPEND cte_wa_fcode TO cte_it_fcode.
      cte_wa_fcode = cte_todoscanc.
      APPEND cte_wa_fcode TO cte_it_fcode.

      IF ( ( cte_alterando EQ 'A' ) OR ( cte_alterando EQ 'I' ) ) OR ( cte_bloqueado EQ c_x ).
        cte_wa_fcode = cte_incluir.
        APPEND cte_wa_fcode TO cte_it_fcode.
        cte_wa_fcode = cte_alterar.
        APPEND cte_wa_fcode TO cte_it_fcode.
        cte_wa_fcode = cte_apagar.
        APPEND cte_wa_fcode TO cte_it_fcode.
        IF cte_bloqueado NE c_x.
          cte_wa_fcode = cte_c_exit.
          APPEND cte_wa_fcode TO cte_it_fcode.
        ENDIF.
      ENDIF.

      IF ( NOT ( ( cte_alterando EQ 'A' ) OR ( cte_alterando EQ 'I' ) ) ) OR ( cte_bloqueado EQ c_x ).
        cte_wa_fcode = cte_confirma.
        APPEND cte_wa_fcode TO cte_it_fcode.
        cte_wa_fcode = cte_cancelar.
        APPEND cte_wa_fcode TO cte_it_fcode.
      ENDIF.

  ENDCASE.

  IF cte_ciot-st_ciot EQ '9'.
    APPEND cte_todos     TO cte_it_fcode.
    APPEND cte_todoscred TO cte_it_fcode.
    APPEND cte_todoscanc TO cte_it_fcode.
    APPEND cte_imprimir  TO cte_it_fcode.
    APPEND cte_impresumo TO cte_it_fcode.
    APPEND cte_atualiza  TO cte_it_fcode.
    APPEND cte_confirma  TO cte_it_fcode.
    APPEND cte_cancelar  TO cte_it_fcode.
    APPEND cte_incluir   TO cte_it_fcode.
    APPEND cte_alterar   TO cte_it_fcode.
    APPEND cte_apagar    TO cte_it_fcode.
    APPEND cte_impedagio TO cte_it_fcode.
    APPEND cte_cargapedagio TO cte_it_fcode.
  ENDIF.

                                                            "US71237
  AUTHORITY-CHECK OBJECT 'ZAC_CAN_VG'
    ID 'ZCANCEL' FIELD '1'.

  IF sy-subrc IS NOT INITIAL.
    APPEND cte_canc_viagem TO cte_it_fcode.
  ENDIF.
                                                            "US71237

  SET PF-STATUS 'PFCIOT' EXCLUDING cte_it_fcode.
  SET TITLEBAR 'TLCIOT'.

ENDMODULE.                 " STATUS_9970  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9970  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9970 INPUT.

  CHECK cte_alterado IS INITIAL.
  CHECK ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ).

  CASE cte_ok_code.
    WHEN 'TB9970CIOT01'.
      cte_dynnr_000 = cte_c_2105.
    WHEN 'TB9970CIOT02'.
      cte_dynnr_000 = cte_c_2106.
      PERFORM seleciona_info_ciot.
    WHEN cte_todos.
      PERFORM solicita_ciot.
      PERFORM atualiza_ciot.
    WHEN cte_atualiza.
      PERFORM solicita_reposta_solicitacao.
      PERFORM atualiza_ciot.
    WHEN cte_todoscred.
      PERFORM solicita_credito.
      PERFORM atualiza_ciot.
    WHEN cte_todoscanc.
      PERFORM solicita_resisao.
      PERFORM atualiza_ciot.
    WHEN cte_imprimir.
      PERFORM imprimir_contrato.
    WHEN cte_impresumo.
      PERFORM imprimir_resumo.
    WHEN cte_impedagio.
      PERFORM imprimir_pedagio.
    WHEN cte_cargapedagio.
      PERFORM carga_pedagio.
    WHEN cte_canc_viagem.
      PERFORM canc_viagem.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_9970  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9970_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9970_exit INPUT.

  IF cte_ok_code EQ cte_c_exit.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_9970_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Form  SOLICITA_CIOT
*&---------------------------------------------------------------------*
FORM solicita_ciot .

  DATA: wa_cte_ciot TYPE zcte_ciot,
        p_protocolo TYPE zprotocolo.

  IF it_cte_ciot[] IS NOT INITIAL.

    READ TABLE it_cte_ciot INTO wa_cte_ciot INDEX 1.

    CALL FUNCTION 'Z_SD_EMITE_CIOT'
      EXPORTING
        p_cte_avulso        = cte_j_1bnfdoc-docnum
        p_tela_visualiza    = space
        emitir_viagem_adm   = c_x
        p_faturamento_autom = vg_faturamento_autom  "*-#133089-21.02.2024-JT
        p_ch_referencia     = vg_ch_referencia      "*-#133089-21.02.2024-JT
      CHANGING
        p_protocolo         = p_protocolo
      EXCEPTIONS
        sem_dados_ciot      = 1
        nao_ciot            = 2
        erro_status         = 3
        erro_web_service    = 4
        erro_solicitacao    = 5
        OTHERS              = 6.

    IF sy-subrc <> 0.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        WHEN abap_true.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_mesg).
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ELSE.
      IF NOT p_protocolo IS INITIAL.
        MESSAGE s034 WITH p_protocolo.
      ENDIF.
    ENDIF.

    CLEAR: it_prt_ciot[].

    SELECT * INTO TABLE it_prt_ciot
      FROM zcte_viagem_obs
     WHERE cd_protocolo EQ p_protocolo.

  ENDIF.

ENDFORM.                    " SOLICITA_CIOT

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_CIOT
*&---------------------------------------------------------------------*
FORM atualiza_ciot .

  CLEAR: it_cte_ciot[], it_prt_ciot[].

*-#133089-21.02.2024-JT-inicio
  CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
    EXPORTING
      p_cte_avulso       = cte_j_1bnfdoc-docnum
    TABLES
      it_cte_ciot        = it_cte_ciot
    EXCEPTIONS
      inf_docnum         = 1
      inf_propveiculo    = 2
      nao_docnum         = 3
      nao_rtrc           = 4
      nao_conta_corrente = 5
      nao_ciot           = 6
      n_placa_cad        = 7
      restricoes_veiculo = 8
      OTHERS             = 9.

  IF sy-subrc <> 0.
    CASE vg_faturamento_autom.
      WHEN abap_off.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      WHEN abap_true.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_mesg).
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
    ENDCASE.
  ENDIF.

* CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
*   EXPORTING
*     p_cte_avulso = cte_j_1bnfdoc-docnum
*   TABLES
*     it_cte_ciot  = it_cte_ciot.
*-#133089-21.02.2024-JT-fim

ENDFORM.                    " ATUALIZA_CIOT

*&---------------------------------------------------------------------*
*&      Form  VISIBILIDADE_CAMPOS
*&---------------------------------------------------------------------*
FORM visibilidade_campos .

  LOOP AT SCREEN.
    IF ( ( cte_alterando NE 'A' ) AND ( cte_alterando NE 'I' ) ) OR ( cte_bloqueado EQ c_x ).
      IF ( screen-group1 EQ 'INS' ) OR ( screen-group1 EQ 'ALT' ).
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF cte_alterando EQ 'A'.
      IF screen-group1 EQ 'INS'.
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " VISIBILIDADE_CAMPOS


*&---------------------------------------------------------------------*
*&      Form  SOLICITA_CREDITO
*&---------------------------------------------------------------------*
FORM solicita_credito .

  DATA: wa_cte_ciot TYPE zcte_ciot,
        p_protocolo TYPE zprotocolo.

  IF it_cte_ciot[] IS NOT INITIAL.

    READ TABLE it_cte_ciot INTO wa_cte_ciot INDEX 1.

    CALL FUNCTION 'Z_SD_CREDITA_CIOT'
      EXPORTING
        p_cte_avulso       = cte_j_1bnfdoc-docnum
      IMPORTING
        p_protocolo        = p_protocolo
      EXCEPTIONS
        cte_nao_autorizado = 1
        sem_dados_ciot     = 2
        erro_status_cred   = 3
        erro_web_service   = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
      IF p_protocolo IS INITIAL.
        MESSAGE s034 WITH p_protocolo.
      ENDIF.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR: it_cte_ciot[].

    CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
      EXPORTING
        p_cte_avulso = cte_j_1bnfdoc-docnum
      TABLES
        it_cte_ciot  = it_cte_ciot.

  ENDIF.


ENDFORM.                    " SOLICITA_CREDITO

*&---------------------------------------------------------------------*
*&      Form  SOLICITA_RESISAO
*&---------------------------------------------------------------------*
FORM solicita_resisao .

  DATA: wa_cte_ciot TYPE zcte_ciot,
        p_protocolo TYPE zprotocolo.

  IF it_cte_ciot[] IS NOT INITIAL.

    READ TABLE it_cte_ciot INTO wa_cte_ciot INDEX 1.

    CALL FUNCTION 'Z_SD_RESCISAO_CIOT'
      EXPORTING
        p_cte_avulso     = cte_j_1bnfdoc-docnum
      IMPORTING
        p_protocolo      = p_protocolo
      EXCEPTIONS
        sem_dados_ciot   = 1
        erro_status_canc = 2
        erro_web_service = 3
        erro_solicitacao = 4
        OTHERS           = 5.

    IF sy-subrc <> 0.
      IF NOT p_protocolo IS INITIAL.
        MESSAGE s034 WITH p_protocolo.
      ENDIF.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR: it_cte_ciot[].

    CALL FUNCTION 'Z_SD_INFO_CTE_CIOT'
      EXPORTING
        p_cte_avulso = cte_j_1bnfdoc-docnum
      TABLES
        it_cte_ciot  = it_cte_ciot.

  ENDIF.

ENDFORM.                    " SOLICITA_RESISAO

*&---------------------------------------------------------------------*
*&      Module  STATUS_6008  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_6008 OUTPUT.

  DATA: vl_uf_rm TYPE string.

  CLEAR: txt_nm_rm_ciot.

  IF ( zcte_ciot-rm_uf IS NOT INITIAL ) AND ( zcte_ciot-rm_municipio IS NOT INITIAL ).
    CONCATENATE zcte_ciot-rm_uf zcte_ciot-rm_municipio INTO vl_uf_rm SEPARATED BY space.
    SELECT SINGLE text INTO txt_nm_rm_ciot
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode EQ vl_uf_rm.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_6008  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_6009  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_6009 OUTPUT.

  DATA: vl_uf_mt TYPE string.

  CLEAR: txt_nm_mt_ciot.

  IF ( zcte_ciot-mt_uf IS NOT INITIAL ) AND ( zcte_ciot-mt_municipio IS NOT INITIAL ).
    CONCATENATE zcte_ciot-mt_uf zcte_ciot-mt_municipio INTO vl_uf_mt SEPARATED BY space.
    SELECT SINGLE text INTO txt_nm_mt_ciot
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode EQ vl_uf_mt.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_6009  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  AJUSTA_VALOR_FRETE_EMPRESA
*&---------------------------------------------------------------------*
*       Ajusta Valor Frete Empresa
*----------------------------------------------------------------------*
FORM ajusta_valor_frete_empresa  USING p_docnum TYPE j_1bdocnum.

  DATA: qtd LIKE zcte_info_nota-quantidade.

  CHECK zcte_identifica-docnum EQ p_docnum.

  LOOP AT it_cte_info_nota INTO cte_info_nota.
    IF cte_info_nota-unidade NE 'KG'.
      CALL FUNCTION 'ME_CONVERSION_MEINS'
        EXPORTING
          i_matnr             = cte_info_nota-material
          i_mein1             = cte_info_nota-unidade
          i_meins             = 'KG'
          i_menge             = cte_info_nota-quantidade
        IMPORTING
          menge               = cte_info_nota-quantidade
        EXCEPTIONS
          error_in_conversion = 1
          no_success          = 2
          OTHERS              = 3.
    ENDIF.
    qtd = qtd + cte_info_nota-quantidade.
  ENDLOOP.

  IF qtd GT 0.
    qtd = qtd / 1000.
    zcte_identifica-vlr_unit_frete   = zcte_identifica-vtprest / qtd.
    zcte_identifica-unid_vlr_frete   = 'TO'.
  ELSE.
    zcte_identifica-vlr_unit_frete   = 0.
    zcte_identifica-unid_vlr_frete   = space.
  ENDIF.
  MODIFY zcte_identifica.

ENDFORM.                    " AJUSTA_VALOR_FRETE_EMPRESA


*&---------------------------------------------------------------------*
*&      Form  SOLICITA_REPOSTA_SOLICITACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM solicita_reposta_solicitacao .

  DATA: wa_cte_ciot    TYPE zcte_ciot,
        p_protocolo    TYPE zprotocolo,
        wa_zcte_viagem TYPE zcte_viagem,
        msg_texto      TYPE string,
        msg_var1       LIKE balm-msgv1,
        msg_var2       LIKE balm-msgv2,
        msg_var3       LIKE balm-msgv3,
        msg_var4       LIKE balm-msgv4.

  IF it_cte_ciot[] IS NOT INITIAL.

    READ TABLE it_cte_ciot INTO wa_cte_ciot INDEX 1.

    CALL FUNCTION 'Z_SD_EMITE_CIOT'
      EXPORTING
        p_cte_avulso            = cte_j_1bnfdoc-docnum
        p_tela_visualiza        = space
        consultar_status_viagem = c_x
        p_faturamento_autom     = vg_faturamento_autom  "*-#133089-21.02.2024-JT
        p_ch_referencia         = vg_ch_referencia      "*-#133089-21.02.2024-JT
      CHANGING
        p_protocolo             = p_protocolo
      EXCEPTIONS
        sem_dados_ciot          = 1
        nao_ciot                = 2
        erro_status             = 3
        erro_web_service        = 4
        consultar_status_viagem = 5
        erro_solicitacao        = 6
        OTHERS                  = 7.

    IF sy-subrc <> 0.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
        WHEN abap_true.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_mesg).
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ELSE.
      IF NOT p_protocolo IS INITIAL.
        SELECT SINGLE * INTO wa_zcte_viagem
          FROM zcte_viagem
         WHERE cd_protocolo EQ p_protocolo.

        IF ( sy-subrc IS INITIAL ) AND ( NOT wa_zcte_viagem-ds_msg IS INITIAL ).

          msg_texto = wa_zcte_viagem-ds_msg.

          CALL FUNCTION 'ZMESSAGE_PREPARE'
            EXPORTING
              msg_completa = space
            CHANGING
              msg_text     = msg_texto
              msg_var1     = msg_var1
              msg_var2     = msg_var2
              msg_var3     = msg_var3
              msg_var4     = msg_var4.

*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE s023 WITH msg_var1 msg_var2 msg_var3 msg_var4.
            WHEN abap_true.
              MESSAGE s023 WITH msg_var1 msg_var2 msg_var3 msg_var4 INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = vg_ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'SVIA' ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.

      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " SOLICITA_REPOSTA_SOLICITACAO


*&---------------------------------------------------------------------*
*&      Module  CTE_MUDA_MUNI_ENVIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_muda_muni_envio INPUT.

  DATA: vl_uf_env TYPE string.

  CONCATENATE zcte_identifica-ufenv zcte_identifica-cmunenv INTO vl_uf_env SEPARATED BY space.

  CLEAR: zcte_identifica-nmunenv.

  SELECT SINGLE text INTO zcte_identifica-nmunenv
    FROM j_1btxjurt
   WHERE spras      EQ zcte_identifica-spras
     AND country    EQ zcte_identifica-country
     AND taxjurcode EQ vl_uf_env.

ENDMODULE.                 " CTE_MUDA_MUNI_ENVIO  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_6010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_6010 OUTPUT.

  DATA: vl_uf_tr TYPE string.

  CLEAR: txt_nm_tr_ciot.

  IF ( zcte_ciot-tr_uf IS NOT INITIAL ) AND ( zcte_ciot-tr_municipio IS NOT INITIAL ).
    CONCATENATE zcte_ciot-tr_uf zcte_ciot-tr_municipio INTO vl_uf_tr SEPARATED BY space.
    SELECT SINGLE text INTO txt_nm_tr_ciot
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode EQ vl_uf_tr.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.                 " STATUS_6010  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CTE_BUSCA_MUN_TR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cte_busca_mun_tr INPUT.

  TYPES:
    BEGIN OF ty_mun_tr,
      taxjurcode TYPE j_1btxjcd,
      text       TYPE text60,
    END OF ty_mun_tr.

  DATA: ti_mun_tr TYPE STANDARD TABLE OF ty_mun_tr INITIAL SIZE 0 WITH HEADER LINE,
        uf_tr     TYPE string.

  CLEAR: st_ret, t_dynpfields[], t_ret.

  MOVE: 'ZCTE_CIOT-TR_UF' TO t_dynpfields-fieldname.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = sy-dynnr
      translate_to_upper = 'X'
    TABLES
      dynpfields         = t_dynpfields.

  READ TABLE t_dynpfields INDEX 1.

  IF t_dynpfields-fieldvalue IS INITIAL.
    SELECT taxjurcode text INTO TABLE ti_mun_tr
      FROM j_1btxjurt
     WHERE spras   EQ zcte_identifica-spras
       AND country EQ zcte_identifica-country.
  ELSE.
    CONCATENATE t_dynpfields-fieldvalue '%' INTO uf_tr.

    SELECT taxjurcode text INTO TABLE ti_mun_tr
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode LIKE uf_tr.
  ENDIF.

  CHECK NOT ti_mun_tr[] IS INITIAL.

  CLEAR: t_dynpfields[].

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'TAXJURCODE'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_mun_tr[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.
*---> 05/07/2023 - Migração S4 - DL
  SORT ti_mun_tr BY taxjurcode.
*<--- 05/07/2023 - Migração S4 - DL
  READ TABLE ti_mun_tr WITH KEY taxjurcode = st_ret-fieldval BINARY SEARCH.

  MOVE: 'ZCTE_CIOT-TR_UF'         TO t_dynpfields-fieldname,
        ti_mun_tr-taxjurcode(3)   TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'ZCTE_CIOT-TR_MUNICIPIO'  TO t_dynpfields-fieldname,
        ti_mun_tr-taxjurcode+3(7) TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " CTE_BUSCA_MUN_TR  INPUT

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_CONTRATO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imprimir_contrato .

  DATA: wa_cte_ciot TYPE zcte_ciot,
        p_protocolo TYPE zprotocolo.

  IF it_cte_ciot[] IS NOT INITIAL.

    READ TABLE it_cte_ciot INTO wa_cte_ciot INDEX 1.

    CALL FUNCTION 'Z_SD_IMPRIMIR_CTR_CIOT'
      EXPORTING
        p_cte_avulso = wa_cte_ciot-docnum
      EXCEPTIONS
        nao_ciot     = 1
        erro_status  = 2
        error        = 3
        OTHERS       = 4.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.                    " IMPRIMIR_CONTRATO

*&---------------------------------------------------------------------*
*&      Form  CANCELAR_VIAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cancelar_viagem  USING    p_p_docnum TYPE j_1bdocnum
                               p_reason   TYPE j_1bnfe_cancel_reason
                               p_reason1  TYPE j_1bnfe_cancel_text
                               p_reason2  TYPE j_1bnfe_cancel_text
                               p_reason3  TYPE j_1bnfe_cancel_text
                               p_reason4  TYPE j_1bnfe_cancel_text.

  CLEAR: p_reason, p_reason1, p_reason2, p_reason3, p_reason4,
         zcte_identifica, cte_cancelado.

  CALL SCREEN 9960 STARTING AT 02 02.

  IF cte_cancelado IS NOT INITIAL.
    MOVE: zcte_identifica-reason  TO p_reason ,
          zcte_identifica-reason1 TO p_reason1,
          zcte_identifica-reason2 TO p_reason2,
          zcte_identifica-reason3 TO p_reason3,
          zcte_identifica-reason4 TO p_reason4.
  ENDIF.

ENDFORM.                    " CANCELAR_VIAGEM

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9960_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9960_exit INPUT.
  CLEAR: zcte_identifica.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_9960_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  REASON_INPUT_9960  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE reason_input_9960 INPUT.
  DATA: wa_j_1bnfe_cancelrt TYPE j_1bnfe_cancelrt.

  SELECT SINGLE * INTO wa_j_1bnfe_cancelrt
    FROM j_1bnfe_cancelrt
   WHERE spras  EQ sy-langu
     AND reason EQ zcte_identifica-reason.

  IF NOT sy-subrc IS INITIAL.
    CLEAR: zcte_identifica.
    MESSAGE e027(zciot).
  ELSE.
    zcte_identifica-reason1 = wa_j_1bnfe_cancelrt-reason1.
    zcte_identifica-reason2 = wa_j_1bnfe_cancelrt-reason2.
    zcte_identifica-reason3 = wa_j_1bnfe_cancelrt-reason3.
    zcte_identifica-reason4 = wa_j_1bnfe_cancelrt-reason4.
  ENDIF.

ENDMODULE.                 " REASON_INPUT_9960  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9960  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9960 OUTPUT.
  SET PF-STATUS 'PF9960'.
  SET TITLEBAR 'TL9960'.

  DATA: wa_cancelrt TYPE j_1bnfe_cancelrt.
  IF NOT zcte_identifica-reason IS INITIAL.
    SELECT SINGLE * INTO wa_cancelrt
      FROM j_1bnfe_cancelrt
     WHERE spras  EQ sy-langu
       AND reason EQ zcte_identifica-reason.
    IF NOT sy-subrc IS INITIAL.
      CLEAR: zcte_identifica.
    ELSE.
      zcte_identifica-reason1 = wa_j_1bnfe_cancelrt-reason1.
      zcte_identifica-reason2 = wa_j_1bnfe_cancelrt-reason2.
      zcte_identifica-reason3 = wa_j_1bnfe_cancelrt-reason3.
      zcte_identifica-reason4 = wa_j_1bnfe_cancelrt-reason4.
    ENDIF.
  ENDIF.

ENDMODULE.                 " STATUS_9960  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9960  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9960 INPUT.

  DATA: answer TYPE c LENGTH 1.

  IF cte_ok_code EQ 'OK_OK'.

    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        titel     = 'Atenção!'
        textline1 = 'Será cancelado o documento.'
        textline2 = 'Deseja cancelar?'
      IMPORTING
        answer    = answer.

    IF answer EQ 'J'.
      cte_cancelado = c_x.
      LEAVE TO SCREEN 0.
    ENDIF.

  ENDIF.

ENDMODULE.                 " USER_COMMAND_9960  INPUT

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_VIAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CTE_IF_NT_ALV  text
*----------------------------------------------------------------------*
FORM atualiza_viagem  USING  cte_nota TYPE zcte_info_nota.

  DATA: p_dc_forne   TYPE  zdc_fornecimento,
        it_lips      TYPE TABLE OF lips WITH HEADER LINE,
        wa_marc      TYPE marc,
        wa_zcte_ciot TYPE zcte_ciot.

  SELECT SINGLE * INTO wa_zcte_ciot
    FROM zcte_ciot
   WHERE docnum EQ cte_nota-docnum.

  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'Z_REMETENTE_MERCADORIA_CTE'
      EXPORTING
        p_docnum   = cte_nota-docnum
      CHANGING
        p_dc_forne = p_dc_forne.

    CLEAR: it_lips[].

    SELECT * INTO TABLE it_lips
      FROM lips
     WHERE vbeln EQ p_dc_forne.

    IF sy-subrc IS INITIAL.
      READ TABLE it_lips INDEX 1.

      CLEAR: wa_marc.

      SELECT SINGLE * INTO wa_marc
        FROM marc
       WHERE matnr EQ it_lips-matnr
         AND werks EQ it_lips-werks.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE w023 WITH 'Deve ser configurado o material' it_lips-matnr 'no centro' it_lips-werks.
      ENDIF.

      wa_zcte_ciot-natureza      = wa_marc-steuc(4).
      wa_zcte_ciot-unidade       = it_lips-gewei.
      wa_zcte_ciot-quantidade    = 0.

      LOOP AT it_lips.
        ADD it_lips-brgew TO wa_zcte_ciot-quantidade.
      ENDLOOP.

      IF ( cte_nota-vl_nota_fiscal GT 0 ) AND ( wa_zcte_ciot-quantidade NE 0 ).
        wa_zcte_ciot-vlr_unit_merc = cte_nota-vl_nota_fiscal / wa_zcte_ciot-quantidade.
      ELSE.
        wa_zcte_ciot-vlr_unit_merc = 0.
      ENDIF.

      wa_zcte_ciot-unid_vlr_merc = wa_zcte_ciot-unidade.

      MODIFY zcte_ciot FROM wa_zcte_ciot.

    ENDIF.

    CLEAR: it_cte_ciot[].

    SELECT * INTO TABLE it_cte_ciot
      FROM zcte_ciot
     WHERE docnum EQ cte_nota-docnum.

  ENDIF.

ENDFORM.                    " ATUALIZA_VIAGEM

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_RESUMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_resumo .
  DATA: wa_cte_ciot TYPE zcte_ciot,
        p_protocolo TYPE zprotocolo.

  IF it_cte_ciot[] IS NOT INITIAL.

    READ TABLE it_cte_ciot INTO wa_cte_ciot INDEX 1.

    CALL FUNCTION 'Z_SD_IMPRIMIR_CTR_CIOT'
      EXPORTING
        p_cte_avulso = wa_cte_ciot-docnum
        p_resumo     = c_x
      EXCEPTIONS
        nao_ciot     = 1
        erro_status  = 2
        error        = 3
        OTHERS       = 4.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
ENDFORM.                    " IMPRIMIR_RESUMO

**&---------------------------------------------------------------------*
**&      Form  XML_CTE_ABRE_AQUA
**&---------------------------------------------------------------------*
**       Aquaviario
**----------------------------------------------------------------------*
FORM xml_cte_ide_aquav  USING    p_cte
                                 p_zlest0066 STRUCTURE zlest0066.

  PERFORM ctnab USING cteide     p_cte.

  PERFORM ctnav USING ctecfop    p_zlest0066-cfop    p_cte.
  PERFORM ctnav USING ctenatop   p_zlest0066-natop   p_cte.
  IF vg_versao_cte < 3. "CS2017002143 CT-e 3.0 Ini
    PERFORM ctnav USING cteforpag  p_zlest0066-forpag  p_cte.
  ENDIF. "CS2017002143 CT-e 3.0 - Fim
  PERFORM ctnav USING cteserie   p_zlest0066-serie   p_cte.
  PERFORM ctnav USING ctenct     p_zlest0066-nct     p_cte.
  PERFORM ctnav USING ctedhemi   p_zlest0066-dhemi   p_cte.
  PERFORM ctnav USING ctetpcte   p_zlest0066-tpcte   p_cte.
  PERFORM ctnaf USING cterefcte  ''                  p_cte.
  PERFORM ctnav USING ctemodal   p_zlest0066-modal   p_cte.
  PERFORM ctnav USING ctetpserv  p_zlest0066-tpserv  p_cte.

  PERFORM ctnav USING ctecmunenv p_zlest0066-cmunenv p_cte.
  PERFORM ctnav USING ctecmunini p_zlest0066-cmunini p_cte.
  PERFORM ctnav USING ctecmunfim p_zlest0066-cmunfim p_cte.
  PERFORM ctnaf USING cteretira  p_zlest0066-retira  p_cte.

  IF vg_versao_cte >= 3. "CS2017002143 CT-e 3.0 Ini
    CASE p_zlest0066-toma.
      WHEN c_0. "Remetenete
        PERFORM ctnaf USING indietoma  '1'  p_cte.
        IF p_zlest0066-ie_reme IS INITIAL OR p_zlest0066-ie_reme EQ 'ISENTO'.
          MESSAGE e134(zles). "WITH P_PARCEIROS-REME_CODIGO.
          "RAISE EXCEPTION TYPE ZCX_CTE.
        ENDIF.
        "WHEN C_1. "Expedidor
        "WHEN C_2. "Recebedor
      WHEN c_3. "Destinatario
        PERFORM ctnaf USING indietoma  '1'  p_cte.
        IF p_zlest0066-ie_dest IS INITIAL OR p_zlest0066-ie_dest EQ 'ISENTO'.
          MESSAGE e134(zles). "WITH P_PARCEIROS-DEST_CODIGO.
          "RAISE EXCEPTION TYPE ZCX_CTE.
        ENDIF.
    ENDCASE.
  ENDIF. "CS2017002143 CT-e 3.0 - Fim

  PERFORM ctnav USING ctetoma    p_zlest0066-toma    p_cte.



  PERFORM ctnfe USING cteide     p_cte.

ENDFORM.                    " XML_CTE_IDE_AQUAV
*&---------------------------------------------------------------------*
*&      Form  XML_CTE_COMPL_AQUAV
*&---------------------------------------------------------------------*
FORM xml_cte_compl_aquav  TABLES   p_it_obs_geral STRUCTURE zcte_obs_gerais
                          USING    p_cte
                                   p_zlest0066 STRUCTURE zlest0066
                                   p_docref TYPE j_1bnfdoc-docref.

  DATA: tx_obs_geral TYPE string,
        wa_obs_geral TYPE zcte_obs_gerais,
        vlr_dolar    TYPE c LENGTH 20,
        vlr_taxa     TYPE c LENGTH 20.

  LOOP AT p_it_obs_geral INTO wa_obs_geral.
    CONCATENATE tx_obs_geral wa_obs_geral-texto INTO tx_obs_geral SEPARATED BY space.
    CLEAR: wa_obs_geral.
  ENDLOOP.

  IF tx_obs_geral IS NOT INITIAL.
    PERFORM ctnab USING ctecompl      p_cte.
    PERFORM ctnab USING ctexobs       p_cte.
    PERFORM ctnao USING tx_obs_geral  p_cte.
    PERFORM ctnfe USING ctexobs       p_cte.
    PERFORM ctnfe USING ctecompl      p_cte.
  ENDIF.

ENDFORM.                    " XML_CTE_COMPL_AQUAV
*&---------------------------------------------------------------------*
*&      Form  XML_CTE_EMIT_AQUAV
*&---------------------------------------------------------------------*
FORM xml_cte_emit_aquav  USING    p_cte
                                  p_zlest0066 STRUCTURE zlest0066.

  PERFORM ctnab        USING cteemit      p_cte.
  PERFORM ctnav        USING cteemitcnpj  p_zlest0066-cnpj_emite p_cte.
  PERFORM limpa_numero USING              p_zlest0066-ie_emite.
  PERFORM ctnav        USING cteemitie    vg_limpo p_cte.
  PERFORM ctnfe        USING cteemit      p_cte.

ENDFORM.                    " XML_CTE_EMIT_AQUAV
*&---------------------------------------------------------------------*
*&      Form  XML_CTE_REME_AQUAV
*&---------------------------------------------------------------------*
FORM xml_cte_reme_aquav  TABLES   p_it_zlest0067 STRUCTURE zlest0067
                         USING    p_cte
                                  p_zlest0066 STRUCTURE zlest0066.


  PERFORM ctnab USING cterem      p_cte.
  IF strlen( p_zlest0066-cnpj_reme ) = 11.
    PERFORM ctnaf USING cteremcpf   p_zlest0066-cnpj_reme  p_cte.
  ELSE.
    PERFORM ctnaf USING cteremcnpj  p_zlest0066-cnpj_reme  p_cte.
  ENDIF.

  PERFORM ctnav USING cteremie    p_zlest0066-ie_reme    p_cte.

  IF vg_ambiente = 'QAS'.
    PERFORM ctnav USING cteremxnome 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL' p_cte.
  ELSE.
    PERFORM ctnav USING cteremxnome  p_zlest0066-xnome_reme p_cte.
  ENDIF.

  PERFORM ctnaf USING cteremxfant p_zlest0066-xfant_reme p_cte.

  PERFORM ctnab USING cteremenderreme  p_cte.
  PERFORM ctnav USING cteremxlgr       p_zlest0066-xlgr_reme    p_cte.
  PERFORM ctnav USING cteremnro        p_zlest0066-nro_reme     p_cte.
  PERFORM ctnav USING cteremxbairro    p_zlest0066-xbairro_reme p_cte.
  PERFORM ctnav USING cteremcmun       p_zlest0066-cmun_reme    p_cte.
  IF p_zlest0066-cep_reme IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN p_zlest0066-cep_reme WITH ''.
  ENDIF.
  PERFORM ctnaf USING cteremcep        p_zlest0066-cep_reme     p_cte.
  PERFORM ctnfe USING cteremenderreme  p_cte.

  PERFORM xml_cte_reme_nf_aquav TABLES p_it_zlest0067 USING p_cte p_zlest0066.

  PERFORM ctnfe USING cterem           p_cte.


ENDFORM.                    " XML_CTE_REME_AQUAV
*&---------------------------------------------------------------------*
*&      Form  XML_CTE_REME_NF_AQUAV
*&---------------------------------------------------------------------*
FORM xml_cte_reme_nf_aquav  TABLES   p_it_zlest0067 STRUCTURE zlest0067
                            USING    p_cte
                                     p_zlest0066 STRUCTURE zlest0066.

  DATA: wl_zlest0067 TYPE zlest0067.
  DATA: v_qtdrat     TYPE p DECIMALS 2.
  DATA: vdata(10).

  CLEAR: wl_zlest0067.

  DATA(_rateio_nf) = 'X'.
  LOOP AT p_it_zlest0067 INTO wl_zlest0067 WHERE chave IS NOT INITIAL.

    SELECT SINGLE *
      FROM zlest0060 INTO @DATA(_wl_0060)
     WHERE chave_nfe = @wl_zlest0067-chave.

    CHECK sy-subrc = 0.

    SELECT SINGLE *
      FROM zlest0063 INTO @DATA(_wl_0063)
     WHERE bukrs       =  @_wl_0060-bukrs
       AND werks       =  @_wl_0060-werks
       AND nr_viagem   =  @_wl_0060-nr_viagem
       AND ano_viagem  =  @_wl_0060-ano_viagem
       AND embarcacao  =  @_wl_0060-embarcacao
       AND nome_emb    =  @_wl_0060-nome_emb.

    CHECK sy-subrc = 0.

    "Insumos
    SELECT SINGLE *
      FROM setleaf INTO @DATA(_wl_setleaf)
     WHERE setname EQ 'MAGGI_ZLES0077_GR_MAT'
       AND valfrom EQ @_wl_0063-gr_material.

    CHECK sy-subrc = 0.

    _rateio_nf = ''.

    EXIT.
  ENDLOOP.

  LOOP AT p_it_zlest0067 INTO wl_zlest0067.
    IF wl_zlest0067-chave+0(1) = 'F'.
      SELECT SINGLE *
        FROM zlest0060 INTO @DATA(_wa_0060)
        WHERE chave_nfe = @wl_zlest0067-chave.

      PERFORM ctnab USING cteinfnf   p_cte.
      PERFORM ctnav USING ctenmod       '01' p_cte.
      PERFORM ctnav USING cteinfserie   _wa_0060-series p_cte.
      PERFORM ctnav USING cteinfndoc    _wa_0060-nfnum p_cte.
*      V_QTDRAT = WL_ZLEST0067-QCARGA / 1000.
*      PERFORM CTNAVN USING CTEINFUNIDRAT         V_QTDRAT P_CTE.
      CONCATENATE _wa_0060-docdat+0(4) '-' _wa_0060-docdat+4(2) '-' _wa_0060-docdat+6(2) INTO vdata.
      PERFORM ctnav  USING ctedemi    vdata p_cte.
      PERFORM ctnavn USING ctevbc     _wa_0060-netwr p_cte.
      PERFORM ctnavn USING ctevicms   '0' p_cte.
      PERFORM ctnavn USING ctevbcst   '0' p_cte.
      PERFORM ctnavn USING ctevst     '0' p_cte.
      PERFORM ctnavn USING ctevprod   _wa_0060-netwr p_cte.
      PERFORM ctnavn USING ctevnf     _wa_0060-netwr p_cte.

      SELECT SINGLE cfop
        INTO @DATA(_cfop)
        FROM zsdt0001
        WHERE ch_referencia = @_wa_0060-ch_referencia.

      REPLACE '/' WITH space INTO _cfop.
      CONDENSE _cfop NO-GAPS.

      PERFORM ctnavn USING ctencfop  _cfop p_cte.
      IF _rateio_nf IS NOT INITIAL.
        PERFORM ctnab  USING cteinfunidtransp  p_cte.
        PERFORM ctnav  USING ctetpunidtransp  '4' p_cte. "4 = Balsa

        REPLACE ALL OCCURRENCES OF  '-' IN wl_zlest0067-nome_emb WITH ''.
        CONDENSE wl_zlest0067-nome_emb NO-GAPS.
        PERFORM ctnav  USING cteidunidtransp  wl_zlest0067-nome_emb p_cte.

        v_qtdrat = wl_zlest0067-qcarga / 1000.

        PERFORM ctnavn USING cteqtdrat         v_qtdrat p_cte.
        PERFORM ctnfe  USING cteinfunidtransp  p_cte.
      ENDIF.
      PERFORM ctnfe  USING cteinfnf   p_cte.
    ELSE.
      PERFORM ctnab USING cteinfnfe   p_cte.
      PERFORM ctnav USING cteinfchave wl_zlest0067-chave       p_cte.
      "     PERFORM CTNAF USING CTEINFPIN   WA_NOTAS_INFO-PIN_SUFRAMA P_CTE.

      IF _rateio_nf IS NOT INITIAL.
        PERFORM ctnab  USING cteinfunidtransp  p_cte.
        PERFORM ctnav  USING ctetpunidtransp  '4' p_cte. "4 = Balsa

        REPLACE ALL OCCURRENCES OF  '-' IN wl_zlest0067-nome_emb WITH ''.
        CONDENSE wl_zlest0067-nome_emb NO-GAPS.
        PERFORM ctnav  USING cteidunidtransp  wl_zlest0067-nome_emb p_cte.

        v_qtdrat = wl_zlest0067-qcarga / 1000.

        PERFORM ctnavn USING cteqtdrat         v_qtdrat p_cte.
        PERFORM ctnfe  USING cteinfunidtransp  p_cte.
      ENDIF.

      PERFORM ctnfe USING cteinfnfe   p_cte.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " XML_CTE_REME_NF_AQUAV
*&---------------------------------------------------------------------*
*&      Form  XML_CTE_DEST_AQUAV
*&---------------------------------------------------------------------*
FORM xml_cte_dest_aquav  USING    p_cte
                                  p_zlest0066 STRUCTURE zlest0066.


  PERFORM ctnab USING ctedestremdest p_cte.

  PERFORM ctnaf USING ctedestremcnpj  p_zlest0066-cnpj_dest  p_cte.
  PERFORM ctnav USING ctedestremie    p_zlest0066-ie_dest    p_cte.

  IF vg_ambiente = 'QAS'.
    PERFORM ctnav USING ctedestremxnome 'CT-E EMITIDO EM AMBIENTE DE HOMOLOGACAO - SEM VALOR FISCAL' p_cte.
  ELSE.
    PERFORM ctnav USING ctedestremxnome p_zlest0066-xnome_dest p_cte.
  ENDIF.

  PERFORM ctnab USING cteenddestenderdest  p_cte.
  PERFORM ctnav USING cteenddestxlgr    p_zlest0066-xlgr_dest    p_cte.
  PERFORM ctnav USING cteenddestnro     p_zlest0066-nro_dest     p_cte.
  PERFORM ctnav USING cteenddestxbairro p_zlest0066-xbairro_dest p_cte.
  PERFORM ctnav USING cteenddestcmun    p_zlest0066-cmun_dest    p_cte.
  IF p_zlest0066-cep_dest IS NOT INITIAL.
    REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN p_zlest0066-cep_dest WITH ''.
  ENDIF.
  PERFORM ctnav USING cteenddestcep     p_zlest0066-cep_dest     p_cte.
  PERFORM ctnfe USING cteenddestenderdest p_cte.

  PERFORM ctnfe USING ctedestremdest p_cte.



ENDFORM.                    " XML_CTE_DEST_AQUAV
*&---------------------------------------------------------------------*
*&      Form  XML_CTE_VPREST_AQUAV
*&---------------------------------------------------------------------*
FORM xml_cte_vprest_aquav  USING    p_cte
                                    p_zlest0066 STRUCTURE zlest0066.

  PERFORM ctnab USING  ctevlrpserv        p_cte.
  PERFORM ctnavn USING ctevlrpservvtprest p_zlest0066-vtprest p_cte.
  PERFORM ctnavn USING ctevlrpservvrec    p_zlest0066-vrec    p_cte.

  PERFORM ctnab  USING ctecompcomp        p_cte.
  PERFORM ctnav  USING ctecompxnome       c_frete_peso         p_cte.
  PERFORM ctnavn USING ctecompvcomp       p_zlest0066-vcomp p_cte.
  PERFORM ctnfe  USING ctecompcomp        p_cte.

  PERFORM ctnfe  USING ctevlrpserv        p_cte.


ENDFORM.                    " XML_CTE_VPREST_AQUAV
*&---------------------------------------------------------------------*
*&      Form  XML_CTE_IMP_AQUAV
*&---------------------------------------------------------------------*
FORM xml_cte_imp_aquav  USING    p_cte
                                 p_zlest0066 STRUCTURE zlest0066.


  PERFORM ctnab USING cteimpimp           p_cte.
  PERFORM ctnab USING cteimpicms          p_cte.

  PERFORM ctnav  USING cteimpcst         p_zlest0066-cst   p_cte.
  PERFORM ctnafv USING cteimppicms       p_zlest0066-picms p_cte.
  PERFORM ctnafv USING cteimpvbc         p_zlest0066-vbc   p_cte.
  PERFORM ctnafv USING cteimpvicms       p_zlest0066-vicms p_cte.


  PERFORM ctnfe USING cteimpicms          p_cte.
  PERFORM ctnfe USING cteimpimp           p_cte.



ENDFORM.                    " XML_CTE_IMP_AQUAV

*&---------------------------------------------------------------------*
*&      Form  XML_CTE_INFCARGA_AQUAV
*&---------------------------------------------------------------------*
FORM xml_cte_infcarga_aquav  USING    p_zlest0066 STRUCTURE zlest0066
                                      p_cte.

  PERFORM ctnab USING  ctecargainfcarga        p_cte.

  PERFORM ctnavn USING ctecargavcarga   p_zlest0066-vcarga   p_cte.
  PERFORM ctnavn USING ctecargapropred  p_zlest0066-propred  p_cte.

  PERFORM ctnab  USING cteqtdeinfq                           p_cte.
  PERFORM ctnav  USING cteqtdecunid   p_zlest0066-cunid      p_cte.
  PERFORM ctnav  USING cteqtdetpmed   p_zlest0066-tpmed      p_cte.
  PERFORM ctnavn USING cteqtdeqcarga  p_zlest0066-qcarga     p_cte.
  PERFORM ctnfe  USING cteqtdeinfq             p_cte.
  PERFORM ctnfe  USING ctecargainfcarga        p_cte.


ENDFORM.                    " XML_CTE_INFCARGA_AQUAV
*&---------------------------------------------------------------------*
*&      Form  XML_CTE_ABRE_AQUAV
*&---------------------------------------------------------------------*
FORM xml_cte_abre_aquav  USING    p_cte.
  PERFORM ctnab USING ctemodalaquav p_cte.
ENDFORM.                    " XML_CTE_ABRE_AQUAV
*&---------------------------------------------------------------------*
*&      Form  XML_CTE_AQUAV
*&---------------------------------------------------------------------*
FORM xml_cte_aquav  USING    p_cte
                             p_zlest0066 STRUCTURE zlest0066.

  PERFORM ctnavn USING ctevprestaquav p_zlest0066-vprest   p_cte.
  PERFORM ctnavn USING ctevafrmm      p_zlest0066-vafrmm   p_cte.

  IF vg_versao_cte < 3. "CS2017002143 CT-e 3.0 Ini
    PERFORM ctnavn USING ctenbooking    p_zlest0066-nbooking p_cte.
  ENDIF. "CS2017002143 CT-e 3.0 - Fim
  "PERFORM CTNAV  USING CTENCTRL       P_ZLEST0066-NCTRL    P_CTE.
  PERFORM ctnav  USING ctexnavio      p_zlest0066-xnavio   p_cte.

  PERFORM ctnab USING ctebalsa p_cte.
  PERFORM ctnav  USING ctexbalsa      p_zlest0066-xbalsa   p_cte.
  PERFORM ctnfe USING ctebalsa p_cte.

  PERFORM ctnavn USING ctenviag     p_zlest0066-nviag    p_cte.
  PERFORM ctnav  USING ctedirec     p_zlest0066-direc    p_cte.
  IF vg_versao_cte < 3. "CS2017002143 CT-e 3.0 Ini
    PERFORM ctnav  USING cteprtemb    p_zlest0066-prtemb   p_cte.
    PERFORM ctnav  USING cteprtdest   p_zlest0066-prtdest  p_cte.
    PERFORM ctnav  USING ctetpnav     p_zlest0066-tpnav    p_cte.
  ENDIF. "CS2017002143 CT-e 3.0 - Fim
  PERFORM ctnav  USING cteirin      p_zlest0066-irin     p_cte.


ENDFORM.                    " XML_CTE_AQUAV
*&---------------------------------------------------------------------*
*&      Form  XML_CTE_FECHA_AQUAV
*&---------------------------------------------------------------------*
FORM xml_cte_fecha_aquav  USING    p_cte.
  PERFORM ctnfe USING ctemodalaquav p_cte.
ENDFORM.                    " XML_CTE_FECHA_AQUAV


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ES_ROW_NO_ROW_ID  text
*      -->P_E_COLUMN_ID_FIELDNAME  text
*----------------------------------------------------------------------*
FORM handle_hotspot_click
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  DATA: ciot   TYPE ty_ciot_alv,
        viagem TYPE zcte_viagem.

  READ TABLE it_cte_ciot_alv INDEX row_id INTO ciot.

  CLEAR: it_prt_ciot[].

  SELECT SINGLE * INTO viagem FROM zcte_viagem WHERE docnum EQ ciot-docnum AND st_ultimo EQ abap_true.
  IF sy-subrc IS INITIAL.
    SELECT * INTO TABLE it_prt_ciot
      FROM zcte_viagem_obs
     WHERE cd_protocolo EQ viagem-cd_protocolo
      ORDER BY id_seq.
  ENDIF.

  CALL METHOD cte_alv_cioto->refresh_table_display.
  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_PEDAGIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_pedagio .
  DATA: wa_cte_ciot TYPE zcte_ciot,
        p_protocolo TYPE zprotocolo.

  IF it_cte_ciot[] IS NOT INITIAL.

    READ TABLE it_cte_ciot INTO wa_cte_ciot INDEX 1.

*-CS2024001181-16.12.2024-#160717-JT-inicio
    IF wa_cte_ciot-tp_card_ped <> 'O'.
      MESSAGE s024(sd) WITH 'Impressão só possivel para o tipo - Visa Cargo (TipBank)' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
*-CS2024001181-16.12.2024-#160717-JT-fim

    CALL FUNCTION 'Z_SD_IMPRIMIR_CTR_CIOT'
      EXPORTING
        p_cte_avulso = wa_cte_ciot-docnum
        p_pedagio    = c_x
      EXCEPTIONS
        nao_ciot     = 1
        erro_status  = 2
        error        = 3
        OTHERS       = 4.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
ENDFORM.                    " IMPRIMIR_PEDAGIO

FORM carga_pedagio .
  DATA: wa_cte_ciot TYPE zcte_ciot,
        p_protocolo TYPE zprotocolo.

  IF it_cte_ciot[] IS NOT INITIAL.

    READ TABLE it_cte_ciot INTO wa_cte_ciot INDEX 1.

    CALL FUNCTION 'Z_SD_IMPRIMIR_CTR_CIOT'
      EXPORTING
        p_cte_avulso    = wa_cte_ciot-docnum
        p_carga_pedagio = c_x
      EXCEPTIONS
        nao_ciot        = 1
        erro_status     = 2
        error           = 3
        OTHERS          = 4.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
ENDFORM.                    " IMPRIMIR_PEDAGIO

FORM xml_cte_infctecomp  USING p_cte
                               p_docref TYPE j_1bnfdoc-docref.

  DATA: vl_refcte     TYPE j1b_nf_xml_b12-refcte,
        ls_acckey     TYPE j_1b_nfe_access_key,
        ls_nfe_active TYPE j_1bnfe_active.

  CLEAR: vl_refcte.
  CHECK p_docref IS NOT INITIAL.

  SELECT SINGLE * FROM j_1bnfe_active INTO ls_nfe_active
   WHERE docnum = p_docref.

  CHECK sy-subrc = 0.

  MOVE-CORRESPONDING ls_nfe_active TO ls_acckey.
  vl_refcte = ls_acckey.
  CHECK strlen( vl_refcte ) EQ 44.

  PERFORM ctnab USING cteinfctecomp p_cte.
  PERFORM ctnaf USING ctechave  vl_refcte p_cte.
  PERFORM ctnfe USING cteinfctecomp p_cte.

ENDFORM.

FORM formata_texto_veiculo USING p_wl_cte_trans TYPE zcte_trans
                       CHANGING c_texto.

  CLEAR: c_texto.

  CONCATENATE p_wl_cte_trans-pc_veiculo '-' p_wl_cte_trans-cd_cidade INTO DATA(_vl_xtexto1).
  CONCATENATE p_wl_cte_trans-cd_uf '-' p_wl_cte_trans-prop_nome '- CNPJ:' p_wl_cte_trans-prop_cnpj INTO DATA(_vl_xtexto2).
  CONCATENATE _vl_xtexto1 '/' _vl_xtexto2 INTO c_texto SEPARATED BY space.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_6011  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_6011 OUTPUT.

  DATA: vl_uf_pc TYPE string.

  CLEAR: txt_nm_pc_ciot.

  IF ( cte_ciot_alv-pc_uf IS NOT INITIAL ) AND ( cte_ciot_alv-pc_municipio IS NOT INITIAL ).
    CONCATENATE cte_ciot_alv-pc_uf cte_ciot_alv-pc_municipio INTO vl_uf_pc SEPARATED BY space.
    SELECT SINGLE text INTO txt_nm_pc_ciot
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode EQ vl_uf_pc.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_6011  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_6012 OUTPUT.

  DATA: vl_uf_le TYPE string.

  CLEAR: txt_nm_le_ciot.

  IF ( cte_ciot_alv-le_uf IS NOT INITIAL ) AND ( cte_ciot_alv-le_municipio IS NOT INITIAL ).
    CONCATENATE cte_ciot_alv-le_uf cte_ciot_alv-le_municipio INTO vl_uf_le SEPARATED BY space.
    SELECT SINGLE text INTO txt_nm_le_ciot
      FROM j_1btxjurt
     WHERE spras      EQ zcte_identifica-spras
       AND country    EQ zcte_identifica-country
       AND taxjurcode EQ vl_uf_le.
  ENDIF.

  PERFORM visibilidade_campos.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CANC_VIAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM canc_viagem .

  DATA: it_canc    TYPE STANDARD TABLE OF sval,
        wa_canc    TYPE sval,
        lv_retorno TYPE c.

  DATA: wa_cte_ciot TYPE zcte_ciot.

  CLEAR: it_canc[], lv_retorno,wa_cte_ciot.

  wa_canc-tabname = 'ZCTE_CIOT'.
  wa_canc-fieldname = 'MOTIVO_CANC_SAP'.
  wa_canc-fieldtext =  'Motivo'.

  APPEND wa_canc TO it_canc.

  IF it_cte_ciot[] IS NOT INITIAL.

    READ TABLE it_cte_ciot INTO wa_cte_ciot INDEX 1.

*    IF wa_cte_ciot-st_ciot NE 6.
*
*      MESSAGE 'Viagem Não Autorizada' TYPE 'S'.
*    ELSE.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title = 'Cancelamento de Viagem(Cancelar TIPBank primeiramente)'
      IMPORTING
        returncode  = lv_retorno
      TABLES
        fields      = it_canc
      EXCEPTIONS
        OTHERS      = 4.

    IF lv_retorno = 'A'.
      MESSAGE 'Cancelamento interrompido' TYPE 'S'.
    ELSE.

      READ TABLE it_canc INDEX 1 INTO wa_canc.
      IF strlen( wa_canc-value ) >= 15.

        "IF it_cte_ciot[] IS NOT INITIAL.

        " READ TABLE it_cte_ciot INTO wa_cte_ciot INDEX 1.
        wa_cte_ciot-cancelado = 'X'.
        wa_cte_ciot-st_ciot = '8'.
        wa_cte_ciot-motivo_canc_sap = wa_canc-value.
        wa_cte_ciot-usuario_canc_sap = sy-uname.
        wa_cte_ciot-dt_canc_sap = sy-datum.
        wa_cte_ciot-hr_canc_sap = sy-uzeit.

        MODIFY zcte_ciot FROM wa_cte_ciot.

        COMMIT WORK.
        "  ENDIF.
      ELSE.
        MESSAGE 'Motivo de Cancelamento deve Conter 15 Caracteres' TYPE 'E'.

      ENDIF .

    ENDIF.
    " ENDIF.
  ENDIF.

ENDFORM.

*-#133089-12.02.2024-JT-inicio
*********************************************************************
* gerar soicitacao faturamento automatico
*********************************************************************
FORM gerar_solicitacao_fatauto.

* PERFORM solicita_ciot.          "*-US 172686-01.04.2025-JT-inicio
* PERFORM atualiza_ciot.          "*-US 172686-01.04.2025-JT-inicio

  DO 10 TIMES.  "*-#133089-21.02.2024-JT
    PERFORM solicita_ciot.        "*-US 172686-01.04.2025-JT-inicio
    PERFORM atualiza_ciot.        "*-US 172686-01.04.2025-JT-inicio
    WAIT UP TO 5 SECONDS.
    PERFORM solicita_reposta_solicitacao.
    PERFORM atualiza_ciot.
    WAIT UP TO 5 SECONDS.

    SELECT *
      INTO @DATA(_zcte)
      FROM zcte_viagem
     UP TO 1 ROWS
     WHERE docnum    = @cte_j_1bnfdoc-docnum
       AND st_ciot   = '1'
       AND st_ultimo = @abap_true.
    ENDSELECT.

    SELECT *
      INTO @DATA(_zcte_ciot)
      FROM zcte_ciot
     UP TO 1 ROWS
     WHERE docnum = @cte_j_1bnfdoc-docnum.
    ENDSELECT.

*-US 172686-01.04.2025-JT-inicio
    IF sy-subrc = 0.
      IF _zcte_ciot-nr_ciot IS INITIAL.
        IF     _zcte_ciot-st_ciot = '0' OR  "Pendente
               _zcte_ciot-st_ciot = '1'.    "Enviado
          WAIT UP TO 10 SECONDS.
        ELSEIF _zcte_ciot-st_ciot = '3'.    "Rejeitado
          IF _zcte-ds_msg CS 'TENTE CONSULTAR NOVAMENTE'.
            WAIT UP TO 10 SECONDS.
          ELSE.
            EXIT.
          ENDIF.
        ELSEIF _zcte_ciot-st_ciot = '9'.    "Não gera contrato - frota Amaggi
          EXIT.
        ENDIF.
      ELSE.
        IF    _zcte_ciot-st_ciot = '2'.     "Autorizado
          EXIT.
        ELSE.
          WAIT UP TO 10 SECONDS.
        ENDIF.
      ENDIF.
    ELSE.
      WAIT UP TO 10 SECONDS.
    ENDIF.
*-US 172686-01.04.2025-JT-fim
  ENDDO.

ENDFORM.
*-#133089-12.02.2024-JT-fim

*********************************************************************
*********************************************************************
