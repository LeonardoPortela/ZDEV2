REPORT  zlesr0082 MESSAGE-ID zles.
**********************************************************************
* TABLES
**********************************************************************
TABLES: zlest0084, j_1btxjur, zlest0090.

**********************************************************************
* TYPES
**********************************************************************
TYPES: BEGIN OF ty_zlest0084,
         id_rota          TYPE zlest0084-id_rota,
         cat_veiculo      TYPE zlest0084-cat_veiculo,
         munic_origem     TYPE zlest0084-munic_origem,
         munic_destino    TYPE zlest0084-munic_destino,
         distancia        TYPE zlest0084-distancia,
         vlr_pedagio      TYPE zlest0084-vlr_pedagio,
         descr_rota       TYPE zlest0084-descr_rota,
         text_origem      TYPE j_1btxjurt-text,
         text_destino     TYPE j_1btxjurt-text,
         qtd_eixo         TYPE zlest0091-qtd_eixo,
         cat_veiculo_icon TYPE c LENGTH 15,
       END OF ty_zlest0084,

       BEGIN OF ty_itinerario,
         id_rota       TYPE zlest0084-id_rota,
         munic_origem  TYPE zlest0084-munic_origem,
         munic_destino TYPE zlest0084-munic_destino,
         route         TYPE trolz-route,
         bezei         TYPE tvrot-bezei,
         lzone_origem  TYPE lfa1-lzone,
         lzone_destino TYPE kna1-lzone,
       END OF ty_itinerario,

       BEGIN OF ty_zlest0090,
         mandt          TYPE zlest0090-mandt,
         bukrs          TYPE zlest0090-bukrs,
         werks          TYPE zlest0090-werks,
         tp_op          TYPE zlest0090-tp_op,
         tp_servico     TYPE zlest0090-tp_servico,
         agenteped      TYPE zlest0090-agenteped,
         nr_vr_xml_tipf TYPE zde_xml_ver_tip,
         tp_op_desc     TYPE c LENGTH 15,
         opcao          TYPE c LENGTH 4,
       END OF ty_zlest0090,

       BEGIN OF ty_zlest0091,
         categoria      TYPE zlest0091-categoria,
         qtd_eixo       TYPE zlest0091-qtd_eixo,
         desc_categoria TYPE zlest0091-desc_categoria,
         opcao          TYPE c LENGTH 4,
       END OF ty_zlest0091,

       BEGIN OF ty_zlest0193,
         matkl      TYPE zlest0193-matkl,
         wgbez60    TYPE t023t-wgbez60,
         tipo_carga TYPE zlest0193-tipo_carga,
         descricao  TYPE char50,
         celltab    TYPE lvc_t_styl,
       END OF ty_zlest0193.

**********************************************************************
* INTERNAL TABLE
**********************************************************************
DATA: gt_zlest0084  TYPE TABLE OF ty_zlest0084,
      gt_itinerario TYPE TABLE OF ty_itinerario,
      gt_zlest0090  TYPE TABLE OF ty_zlest0090,
      gt_zlest0091  TYPE TABLE OF ty_zlest0091,
      gt_zlest0193  TYPE TABLE OF ty_zlest0193,
      gt_lfa1       TYPE TABLE OF lfa1,
      gt_kna1       TYPE TABLE OF kna1,
      gt_trolz      TYPE TABLE OF trolz,
      gt_tvrot      TYPE TABLE OF tvrot.

DATA: gt_fcat_rota  TYPE lvc_t_fcat,
      gt_fcat_iti   TYPE lvc_t_fcat,
      gt_fcat_ag    TYPE lvc_t_fcat,
      gt_fcat_cat   TYPE lvc_t_fcat,
      gt_fcat_carga TYPE lvc_t_fcat.

DATA: tg_selectedrow TYPE lvc_t_row,
      wg_selectedrow TYPE lvc_s_row.
DATA: gt_estilo TYPE lvc_t_styl,
      gs_layout TYPE lvc_s_layo.

**********************************************************************
* WORK AREA
**********************************************************************
DATA: gw_zlest0084  TYPE ty_zlest0084,
      gw_itinerario TYPE ty_itinerario,
      gw_zlest0090  TYPE ty_zlest0090,
      gw_zlest0091  TYPE ty_zlest0091,
      gw_zlest0193  TYPE ty_zlest0193,
      gw_lfa1       TYPE lfa1,
      gw_kna1       TYPE kna1,
      gw_trolz      TYPE trolz,
      gw_tvrot      TYPE tvrot.

DATA: gw_fcat_rota  TYPE lvc_s_fcat,
      gw_fcat_iti   TYPE lvc_s_fcat,
      gw_fcat_ag    TYPE lvc_s_fcat,
      gw_fcat_cat   TYPE lvc_s_fcat,
      gw_fcat_carga TYPE lvc_s_fcat,
      gw_stable     TYPE lvc_s_stbl.
**********************************************************************
* CONSTANTES
**********************************************************************
CONSTANTS: tela_0100 TYPE sy-dynnr VALUE '0100',
           tela_0200 TYPE sy-dynnr VALUE '0200',
           tela_0300 TYPE sy-dynnr VALUE '0301',
           tela_0400 TYPE sy-dynnr VALUE '0400',
           tela_0500 TYPE sy-dynnr VALUE '0500',
           tela_0510 TYPE sy-dynnr VALUE '0510',
           tela_0512 TYPE sy-dynnr VALUE '0512',
           tela_0513 TYPE sy-dynnr VALUE '0513'.

"DATA: TELA_AG_PEDAGIO TYPE N LENGTH 4 VALUE TELA_0300.

**********************************************************************
* OBJETOS
**********************************************************************
DATA: obj_custom_rota  TYPE REF TO cl_gui_custom_container,
      obj_grid_rota    TYPE REF TO cl_gui_alv_grid,
      obj_custom_iti   TYPE REF TO cl_gui_custom_container,
      obj_grid_iti     TYPE REF TO cl_gui_alv_grid,
      obj_custom_ag    TYPE REF TO cl_gui_custom_container,
      obj_grid_ag      TYPE REF TO cl_gui_alv_grid,
      obj_custom_cat   TYPE REF TO cl_gui_custom_container,
      obj_grid_cat     TYPE REF TO cl_gui_alv_grid,
      obj_custom_carga TYPE REF TO cl_gui_custom_container,
      obj_grid_carga   TYPE REF TO cl_gui_alv_grid.



**********************************************************************
* VARIABLES
**********************************************************************
DATA: title_popup    TYPE c LENGTH 100,
      var_edit       TYPE c,
      var_index_edit TYPE sy-tabix.

**********************************************************************
* VARIAVEIS
**********************************************************************
DATA: radio_manual TYPE c,
      radio_eletr  TYPE c,
      ok_code      TYPE sy-ucomm. "Código de função que acionou o PAI

**********************************************************************
* CAMPOS DA TELA.
**********************************************************************
DATA: p_empresa  TYPE bukrs,
      p_werks    TYPE werks,
      p_ag_ped   TYPE zagenteped,
      p_xml_tipf TYPE zde_xml_ver_tip.

DATA: p_categoria TYPE zlest0091-categoria,
      p_qtd_eixo  TYPE zlest0091-qtd_eixo,
      p_desc_cat  TYPE zlest0091-desc_categoria.

"Campos para Objeto de Autorização
DATA: ck_autorizacao_01 TYPE c LENGTH 1,  "01  Agente de Pedágio
      ck_autorizacao_02 TYPE c LENGTH 1,  "02  Categoria de Veículo
      ck_autorizacao_03 TYPE c LENGTH 1,  "03  Cadastro de Rotas
      ck_autorizacao_04 TYPE c LENGTH 1,  "04  Margem de Adiantamento
      ck_autorizacao_05 TYPE c LENGTH 1,  "05  Cadastro de Rotas - Inserir
      ck_autorizacao_06 TYPE c LENGTH 1,  "06  Cadastro de Rotas - Deletar
      ck_autorizacao_07 TYPE c LENGTH 1,  "07 Cadastro de Rotas - Solicitar Rota Administradora
      ck_autorizacao_08 TYPE c LENGTH 1,  "08 Cadastro de Rotas - Consutar Rota Administradora
      ck_autorizacao_09 TYPE c LENGTH 1,  "09 Cadastro de Rotas - Atualizar Rota Administradora (Carga)
      ck_autorizacao_10 TYPE c LENGTH 1,  "10 Cadastro de Rotas - Marcar Rota Prioritária
      ck_autorizacao_11 TYPE c LENGTH 1.  "11 Habilita/Desabilita Consulta Situaçao de RNTRC TipFrete

**********************************************************************
* SELECTION
**********************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: p_rota  FOR zlest0084-id_rota NO INTERVALS,
                p_mun_o FOR j_1btxjur-taxjurcode NO INTERVALS NO-EXTENSION,
                p_mun_d FOR j_1btxjur-taxjurcode NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK b1.

INITIALIZATION.

  IF sy-batch EQ abap_true.
    PERFORM atualizar_rota_adm.
    LEAVE PROGRAM.
  ENDIF.

  ck_autorizacao_01 = abap_false.
  ck_autorizacao_02 = abap_false.
  ck_autorizacao_03 = abap_false.
  ck_autorizacao_04 = abap_false.
  ck_autorizacao_05 = abap_false.
  ck_autorizacao_06 = abap_false.
  ck_autorizacao_07 = abap_false.
  ck_autorizacao_08 = abap_false.
  ck_autorizacao_09 = abap_false.
  ck_autorizacao_10 = abap_false.
  ck_autorizacao_11 = abap_false.

  "01  Agente de Pedágio
  AUTHORITY-CHECK OBJECT 'Z_AC_ADMFR' ID 'Z_AC_ADMFR' FIELD '01'.
  IF sy-subrc = 0.
    ck_autorizacao_01 = abap_true.
  ENDIF.

  "02  Categoria de Veículo
  AUTHORITY-CHECK OBJECT 'Z_AC_ADMFR' ID 'Z_AC_ADMFR' FIELD '02'.
  IF sy-subrc = 0.
    ck_autorizacao_02 = abap_true.
  ENDIF.

  "03  Cadastro de Rotas
  AUTHORITY-CHECK OBJECT 'Z_AC_ADMFR' ID 'Z_AC_ADMFR' FIELD '03'.
  IF sy-subrc = 0.
    ck_autorizacao_03 = abap_true.
  ENDIF.

  "04  Margem de Adiantamento
  AUTHORITY-CHECK OBJECT 'Z_AC_ADMFR' ID 'Z_AC_ADMFR' FIELD '04'.
  IF sy-subrc = 0.
    ck_autorizacao_04 = abap_true.
  ENDIF.

  "05  Cadastro de Rotas - Inserir
  AUTHORITY-CHECK OBJECT 'Z_AC_ADMFR' ID 'Z_AC_ADMFR' FIELD '05'.
  IF sy-subrc = 0.
    ck_autorizacao_05 = abap_true.
  ENDIF.

  "06  Cadastro de Rotas - Deletar
  AUTHORITY-CHECK OBJECT 'Z_AC_ADMFR' ID 'Z_AC_ADMFR' FIELD '06'.
  IF sy-subrc = 0.
    ck_autorizacao_06 = abap_true.
  ENDIF.

  "07 Cadastro de Rotas - Solicitar Rota Administradora
  AUTHORITY-CHECK OBJECT 'Z_AC_ADMFR' ID 'Z_AC_ADMFR' FIELD '07'.
  IF sy-subrc = 0.
    ck_autorizacao_07 = abap_true.
  ENDIF.

  "08 Cadastro de Rotas - Consutar Rota Administradora
  AUTHORITY-CHECK OBJECT 'Z_AC_ADMFR' ID 'Z_AC_ADMFR' FIELD '08'.
  IF sy-subrc = 0.
    ck_autorizacao_08 = abap_true.
  ENDIF.

  "09 Cadastro de Rotas - Atualizar Rota Administradora (Carga)
  AUTHORITY-CHECK OBJECT 'Z_AC_ADMFR' ID 'Z_AC_ADMFR' FIELD '09'.
  IF sy-subrc = 0.
    ck_autorizacao_09 = abap_true.
  ENDIF.

  "10 Cadastro de Rotas - Marcar Rota Prioritária
  AUTHORITY-CHECK OBJECT 'Z_AC_ADMFR' ID 'Z_AC_ADMFR' FIELD '09'.
  IF sy-subrc = 0.
    ck_autorizacao_10 = abap_true.
  ENDIF.

  "11 Habilita/Desabilita Consulta Situaçao de RNTRC TipFrete
  AUTHORITY-CHECK OBJECT 'Z_AC_ADMFR' ID 'Z_AC_ADMFR' FIELD '11'.
  IF sy-subrc = 0.
    ck_autorizacao_11 = abap_true.
  ENDIF.

  SELECT SINGLE * INTO @DATA(wa_zlest0101)
    FROM zlest0102
   WHERE bukrs EQ @space.

  IF sy-subrc IS INITIAL.

    "Cadastro de Rotas Solicitadas TipFrete
    UPDATE zlest0101
       SET bukrs  = '0001'
           branch = '0116'
     WHERE bukrs EQ space.

    "Cadastro de Rotas Solicitadas TipFrete - Praças
    UPDATE zlest0102
       SET bukrs  = '0001'
           branch = '0116'
     WHERE bukrs EQ space.

    "Ajustar Praças que não tem a chave de empresa/filial agrupadora
    UPDATE zlest0102
       SET bukrs  = '0001'
           branch = '0116'
     WHERE bukrs EQ space.

    "Ajustar Municípios Intermediários que não tem a chave de empresa/filial agrupadora
    UPDATE zlest0107
       SET bukrs  = '0001'
           branch = '0116'
     WHERE bukrs EQ space.

    UPDATE zlest0084
       SET bukrs  = '0001'
           branch = '0116'
     WHERE bukrs EQ space.

    COMMIT WORK.

  ENDIF.

START-OF-SELECTION.

**********************************************************************
* PERFORMS
**********************************************************************
  PERFORM: selecionar_dados,
           criar_alv.

  CALL SCREEN tela_0100.
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

  DATA: it_code TYPE TABLE OF syucomm.

  "01  Agente de Pedágio
  IF ck_autorizacao_01 EQ abap_false.
    APPEND 'AG_PEDAGIO' TO it_code.
  ENDIF.

  "02  Categoria de Veículo
  IF ck_autorizacao_02 EQ abap_false.
    APPEND 'CAT_VEIC' TO it_code.
  ENDIF.

  "03  Cadastro de Rotas
  IF ck_autorizacao_03 EQ abap_false.
    APPEND 'CAD_ROTAS' TO it_code.
  ENDIF.

  "04  Margem de Adiantamento
  IF ck_autorizacao_04 EQ abap_false.
    APPEND 'CAD_ADIANT' TO it_code.
  ENDIF.

  "11 Habilita/Desabilita Consulta Situaçao de RNTRC TipFrete
  IF ck_autorizacao_11 EQ abap_false.
    APPEND 'CS_RNTRC' TO it_code.
  ENDIF.

  SET PF-STATUS 'PF0100' EXCLUDING it_code.
  SET TITLEBAR  'TB0100'.

ENDMODULE.                 " PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'AG_PEDAGIO'.
      PERFORM verifica_tabela_90.
      CALL SCREEN tela_0300.
    WHEN 'CAT_VEIC'.
      CALL SCREEN tela_0400.
    WHEN 'CAD_ROTAS'.
      CALL SCREEN tela_0500.
    WHEN 'CAD_ADIANT'.
      CALL SCREEN tela_0510.
    WHEN 'CS_RNTRC'.
      CALL SCREEN tela_0512 STARTING AT 05 05.
    WHEN 'CAD_CARGA'.
      CALL SCREEN tela_0513.

  ENDCASE.
ENDMODULE.                 " PAI_0100  INPUT

*----------------------------------------------------------------------*
*       CLASS zcl_evento DEFINITION
*----------------------------------------------------------------------*
CLASS zcl_evento DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      zm_handle_hotspot_itinerario FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no,
      zm_handle_hotspot_agente     FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no,
      zm_handle_hotspot_categoria  FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "zcl_evento DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_evento IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS zcl_evento IMPLEMENTATION.

  METHOD: zm_handle_hotspot_itinerario.
    PERFORM z_hotspot_itinerario USING  e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

  METHOD: zm_handle_hotspot_agente.
    PERFORM z_hotspot_agente USING  e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

  METHOD: zm_handle_hotspot_categoria.
    PERFORM z_hotspot_categoria USING  e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot

ENDCLASS.                    "zcl_evento IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
FORM selecionar_dados.

  DATA: var_like_cod  TYPE c LENGTH 12.
  DATA: gw_j_1btxjur  TYPE j_1btxjur,
        gw_j_1btxjurt TYPE j_1btxjurt.

  DATA: lt_zlest0091 TYPE TABLE OF zlest0091,
        ls_zlest0091 TYPE zlest0091.

  REFRESH: lt_zlest0091[].

  FIELD-SYMBOLS: <fs_zlest0084> TYPE ty_zlest0084.

  SELECT id_rota cat_veiculo munic_origem munic_destino distancia vlr_pedagio descr_rota
    FROM zlest0084
    INTO TABLE gt_zlest0084
   WHERE id_rota       IN p_rota
     AND munic_origem  IN p_mun_o
     AND munic_destino IN p_mun_d.

  IF NOT ( gt_zlest0084[] IS INITIAL ).
    SELECT * FROM zlest0091
      INTO TABLE lt_zlest0091
      FOR ALL ENTRIES IN gt_zlest0084
   WHERE categoria EQ gt_zlest0084-cat_veiculo.
  ENDIF.

  LOOP AT gt_zlest0084 ASSIGNING <fs_zlest0084>.


    READ TABLE lt_zlest0091 INTO ls_zlest0091 WITH KEY categoria = <fs_zlest0084>-cat_veiculo.
    IF ( sy-subrc EQ 0 ).
      <fs_zlest0084>-qtd_eixo = ls_zlest0091-qtd_eixo.
    ENDIF.

    CLEAR: var_like_cod.
    var_like_cod = <fs_zlest0084>-munic_origem.
    CONCATENATE '%' var_like_cod INTO var_like_cod.

    SELECT SINGLE * FROM j_1btxjur as a INTO gw_j_1btxjur WHERE taxjurcode LIKE var_like_cod
                                                            AND country    EQ 'BR'
                                                            and exists ( SELECT taxjurcode
                                                                           FROM J_1Btreg_city as b
                                                                          where b~taxjurcode = a~taxjurcode ).

    IF ( sy-subrc EQ 0 ).
      SELECT SINGLE * FROM  j_1btxjurt INTO gw_j_1btxjurt WHERE taxjurcode = gw_j_1btxjur-taxjurcode.
      <fs_zlest0084>-text_origem = gw_j_1btxjurt-text.
    ENDIF.

    CLEAR: var_like_cod.
    var_like_cod = <fs_zlest0084>-munic_destino.
    CONCATENATE '%' var_like_cod INTO var_like_cod.

    SELECT SINGLE * FROM j_1btxjur as a INTO gw_j_1btxjur WHERE taxjurcode LIKE var_like_cod
                                                            AND country    EQ 'BR'
                                                            and exists ( SELECT taxjurcode
                                                                                FROM J_1Btreg_city as b
                                                                               where b~taxjurcode = a~taxjurcode ).
    IF ( sy-subrc EQ 0 ).
      SELECT SINGLE * FROM  j_1btxjurt INTO gw_j_1btxjurt WHERE taxjurcode = gw_j_1btxjur-taxjurcode.
      <fs_zlest0084>-text_destino = gw_j_1btxjurt-text.
    ENDIF.

    CONCATENATE icon_import_all_requests '-' <fs_zlest0084>-cat_veiculo INTO <fs_zlest0084>-cat_veiculo_icon SEPARATED BY space.


    CLEAR: gw_j_1btxjur, gw_j_1btxjurt, var_like_cod, ls_zlest0091.
  ENDLOOP.

  UNASSIGN: <fs_zlest0084>.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV
*&---------------------------------------------------------------------*
FORM criar_alv .

  CREATE OBJECT obj_custom_rota
    EXPORTING
      container_name              = 'CONTAINER_ROTA'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM: criar_catalog.

  CREATE OBJECT obj_grid_rota
    EXPORTING
      i_parent          = obj_custom_rota
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SET HANDLER: zcl_evento=>zm_handle_hotspot_itinerario FOR obj_grid_rota.

  CALL METHOD obj_grid_rota->set_table_for_first_display
    CHANGING
      it_outtab                     = gt_zlest0084[]
      it_fieldcatalog               = gt_fcat_rota[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.


ENDFORM.                    " CRIAR_ALV
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG
*&---------------------------------------------------------------------*
FORM criar_catalog .

  REFRESH: gt_fcat_rota[].

  PERFORM montar_catalog_rota USING:
        'ID_ROTA'           'Rota'             '15'   '' 'X' '' 'C' '' '' ''  '' '',
        'CAT_VEICULO_ICON'  'Cat.Veiculo'      '10'   '' '' '' 'C' '' ''  'ZLEST0084'  'CAT_VEICULO'   'GT_ZLEST0084',
        'QTD_EIXO'          'Qtd. de Eixo'     '10'   'X' '' '' 'C' '' ''  'ZLEST0091'  'QT_EIXO'       'LT_ZLEST0091',
        'VLR_PEDAGIO'       'Vlr.Pedágio R$'   '8'    '' '' '' '' '' ''  'ZLEST0084'  'VLR_PEDAGIO'   'GT_ZLEST0084',
        'MUNIC_ORIGEM'      'Mun.Origem'       '12'   '' '' '' '' '' ''  'ZLEST0084'  'MUNIC_ORIGEM'  'GT_ZLEST0084',
        'TEXT_ORIGEM'       'Descr.Origem'     '25'   '' '' '' '' '' ''  'J_1BTXJURT' 'TEXT'          'GT_ZLEST0084',
        'MUNIC_DESTINO'     'Mun.Destino.'     '12'   '' '' '' '' '' ''  'ZLEST0084'  'MUNIC_DESTINO' 'GT_ZLEST0084',
        'TEXT_DESTINO'      'Descr.Destino'    '25'   '' '' '' '' '' ''  'J_1BTXJURT' 'TEXT'          'GT_ZLEST0084',
        'DISTANCIA'         'Distância'        '10'   '' '' '' '' '' ''  'ZLEST0084'  'DISTANCIA'     'GT_ZLEST0084',
        'DESCR_ROTA '       'Descr.Rota'       '90'   '' '' '' '' '' ''  'ZLEST0084'  'DESCR_ROTA'    'GT_ZLEST0084'.

ENDFORM.                    " CRIAR_CATALOG

*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_ROTA
*&---------------------------------------------------------------------*
FORM montar_catalog_rota  USING       VALUE(p_fieldname)
                                      VALUE(p_desc)
                                      VALUE(p_tam)
                                      VALUE(p_no_zero)
                                      VALUE(p_hotspot)
                                      VALUE(p_cor)
                                      VALUE(p_just)
                                      VALUE(p_sum)
                                      VALUE(p_edit)
                                      VALUE(p_ref_tabname)   LIKE dd02d-tabname
                                      VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                                      VALUE(p_tabname)       LIKE dd02d-tabname.


  CLEAR: gw_fcat_rota.

  gw_fcat_rota-fieldname = p_fieldname.
  gw_fcat_rota-ref_table = p_ref_tabname..
  gw_fcat_rota-ref_field = p_ref_fieldname.
  gw_fcat_rota-tabname   = p_tabname.
  gw_fcat_rota-scrtext_l = p_desc.
  gw_fcat_rota-scrtext_m = p_desc.
  gw_fcat_rota-scrtext_s = p_desc.
  gw_fcat_rota-outputlen = p_tam.
  gw_fcat_rota-no_zero   = p_no_zero.
  gw_fcat_rota-hotspot   = p_hotspot.
  gw_fcat_rota-emphasize = p_cor.
  gw_fcat_rota-just      = p_just.
  gw_fcat_rota-do_sum    = p_sum.
  gw_fcat_rota-edit      = p_edit.

  APPEND gw_fcat_rota TO gt_fcat_rota.


ENDFORM.                    " MONTAR_CATALOG_ROTA
*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_ITINERARIO
*&---------------------------------------------------------------------*
FORM z_hotspot_itinerario  USING p_row_id
                                 p_column_id
                                 p_row_no.

  DEFINE conc.
    CONCATENATE TITLE_POPUP &1 INTO TITLE_POPUP SEPARATED BY SPACE.
  END-OF-DEFINITION.

  CLEAR: title_popup, gw_zlest0084.

  READ TABLE gt_zlest0084 INTO gw_zlest0084 INDEX p_row_id.

  IF ( sy-subrc EQ 0 ).

    conc gw_zlest0084-id_rota.
    conc '-'.
    conc 'Origem: '.
    conc gw_zlest0084-munic_origem.
    conc '-'.
    conc 'Destino: '.
    conc gw_zlest0084-munic_destino.

    PERFORM: selecionar_itinerario USING gw_zlest0084.

    CALL SCREEN tela_0200 STARTING AT 010 2 ENDING   AT 110 15.
  ENDIF.

ENDFORM.                    " Z_HOTSPOT_ITINERARIO
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_ITINERARIO
*&---------------------------------------------------------------------*
FORM selecionar_itinerario USING p_zlest0084 TYPE ty_zlest0084.

  DATA: var_like_cod  TYPE c LENGTH 12.
  DATA: range_origem  TYPE TABLE OF selopt WITH HEADER LINE,
        range_destino TYPE TABLE OF selopt WITH HEADER LINE.

  FIELD-SYMBOLS: <fs_lfa1> TYPE lfa1,
                 <fs_kna1> TYPE kna1.

  REFRESH: gt_lfa1[], gt_kna1[].

  CLEAR: var_like_cod.
  var_like_cod = p_zlest0084-munic_origem.
  CONCATENATE '%' var_like_cod INTO var_like_cod.

  SELECT * FROM lfa1 INTO gw_lfa1  WHERE txjcd LIKE var_like_cod.

    IF ( sy-subrc EQ 0 ) AND NOT ( gw_lfa1-lzone IS INITIAL ).
      APPEND gw_lfa1 TO gt_lfa1.
    ENDIF.
  ENDSELECT.

  CLEAR: var_like_cod.
  var_like_cod = p_zlest0084-munic_destino.
  CONCATENATE '%' var_like_cod INTO var_like_cod.

  SELECT * FROM kna1 INTO gw_kna1 WHERE txjcd LIKE var_like_cod.

    IF ( sy-subrc EQ 0 ) AND NOT ( gw_kna1-lzone IS INITIAL ).
      APPEND gw_kna1 TO gt_kna1.
    ENDIF.
  ENDSELECT.

  "Retirar a UF do código do município.
  "E criar um range para selecionar o itinerario.
  LOOP AT gt_lfa1 ASSIGNING <fs_lfa1>.
    <fs_lfa1>-txjcd = <fs_lfa1>-txjcd+3(12).
    range_origem-sign   = 'I'.
    range_origem-option = 'EQ'.
    range_origem-low    = <fs_lfa1>-lzone.
    range_origem-high   = <fs_lfa1>-lzone.
    APPEND range_origem.
  ENDLOOP.
  UNASSIGN <fs_lfa1>.

  "Retirar a UF do código do município.
  "E criar um range para selecionar o itinerario.
  LOOP AT gt_kna1 ASSIGNING <fs_kna1>.
    <fs_kna1>-txjcd = <fs_kna1>-txjcd+3(12).
    range_destino-sign   = 'I'.
    range_destino-option = 'EQ'.
    range_destino-low    = <fs_kna1>-lzone.
    range_destino-high   = <fs_kna1>-lzone.
    APPEND range_destino.
  ENDLOOP.
  UNASSIGN <fs_kna1>.

  IF NOT ( range_origem IS INITIAL ) AND NOT ( range_destino IS INITIAL ).

    SELECT * FROM trolz
      INTO TABLE gt_trolz
    WHERE azone IN range_origem
      AND aland EQ 'BR'
      AND lland EQ 'BR'
      AND lzone IN range_destino.

    SELECT * FROM tvrot
      INTO TABLE gt_tvrot
      FOR ALL ENTRIES IN gt_trolz
    WHERE route EQ gt_trolz-route.


    LOOP AT gt_trolz INTO gw_trolz.

      gw_itinerario-id_rota       = p_zlest0084-id_rota.
      gw_itinerario-munic_origem  = p_zlest0084-munic_origem.
      gw_itinerario-munic_destino = p_zlest0084-munic_destino.
      gw_itinerario-route         = gw_trolz-route.
      gw_itinerario-lzone_origem  = gw_trolz-azone.
      gw_itinerario-lzone_destino = gw_trolz-lzone.

      READ TABLE gt_tvrot INTO gw_tvrot WITH KEY route = gw_trolz-route.
      gw_itinerario-bezei = gw_tvrot-bezei.

      APPEND gw_itinerario TO gt_itinerario.

      CLEAR: gw_trolz, gw_itinerario, gw_tvrot.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " SELECIONAR_ITINERARIO
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.
  SET PF-STATUS 'PF0200'.
  SET TITLEBAR  'TB0200' WITH title_popup.
  PERFORM: criar_alv_itinerario.
ENDMODULE.                 " PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0200  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0200 INPUT.

  CASE sy-ucomm.
    WHEN: 'OK'.

      CLEAR: gw_itinerario.
      REFRESH: gt_itinerario[].

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " PAI_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_ITINERARIO
*&---------------------------------------------------------------------*
FORM criar_alv_itinerario .

  DATA: wa_stable       TYPE lvc_s_stbl.

  IF ( obj_custom_iti IS INITIAL ).
    CREATE OBJECT obj_custom_iti
      EXPORTING
        container_name              = 'CONTAINER_ITINERARIO'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    PERFORM: criar_catalog_iti.

    CREATE OBJECT obj_grid_iti
      EXPORTING
        i_parent          = obj_custom_iti
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CALL METHOD obj_grid_iti->set_table_for_first_display
      CHANGING
        it_outtab                     = gt_itinerario[]
        it_fieldcatalog               = gt_fcat_iti[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.
    CALL METHOD obj_grid_iti->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.                    " CRIAR_ALV_ITINERARIO
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_ITI
*&---------------------------------------------------------------------*
FORM criar_catalog_iti .

  REFRESH: gt_fcat_iti[].

  PERFORM montar_catalog_iti USING:
      'ID_ROTA'       'Rota'               '10'   '' '' '' '' '' '' '' '' '',
      'ROUTE'         'Itinerário'         '10'   '' '' '' '' '' '' 'TROLZ'     'ROUTE'     'GT_ITINERARIO',
      'BEZEI'         'Descr.Itinerário'   '30'   '' '' '' '' '' '' 'TVROT'     'BEZEI'     'GT_ITINERARIO',
      'LZONE_ORIGEM'  'Zona Origem'        '10'   '' '' '' '' '' '' 'LFA1'      'LZONE'     'GT_ITINERARIO',
      'LZONE_DESTINO' 'Zona Destino'       '10'   '' '' '' '' '' '' 'KNA1'      'LZONE'     'GT_ITINERARIO',
      'MUNIC_ORIGEM'  'Munic.Origem'       '10'   '' '' '' '' '' '' 'KNA1'      'LZONE'     'GT_ITINERARIO',
      'MUNIC_DESTINO' 'Munic.Destino'      '10'   '' '' '' '' '' '' ''      ''     ''.

ENDFORM.                    " CRIAR_CATALOG_ITI
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_ITI
*&---------------------------------------------------------------------*
FORM montar_catalog_iti  USING       VALUE(p_fieldname)
                                     VALUE(p_desc)
                                     VALUE(p_tam)
                                     VALUE(p_no_zero)
                                     VALUE(p_hotspot)
                                     VALUE(p_cor)
                                     VALUE(p_just)
                                     VALUE(p_sum)
                                     VALUE(p_edit)
                                     VALUE(p_ref_tabname)   LIKE dd02d-tabname
                                     VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                                     VALUE(p_tabname)       LIKE dd02d-tabname.


  CLEAR: gw_fcat_iti.

  gw_fcat_iti-fieldname = p_fieldname.
  gw_fcat_iti-ref_table = p_ref_tabname..
  gw_fcat_iti-ref_field = p_ref_fieldname.
  gw_fcat_iti-tabname   = p_tabname.
  gw_fcat_iti-scrtext_l = p_desc.
  gw_fcat_iti-scrtext_m = p_desc.
  gw_fcat_iti-scrtext_s = p_desc.
  gw_fcat_iti-outputlen = p_tam.
  gw_fcat_iti-no_zero   = p_no_zero.
  gw_fcat_iti-hotspot   = p_hotspot.
  gw_fcat_iti-emphasize = p_cor.
  gw_fcat_iti-just      = p_just.
  gw_fcat_iti-do_sum    = p_sum.
  gw_fcat_iti-edit      = p_edit.

  APPEND gw_fcat_iti TO gt_fcat_iti.

ENDFORM.                    " MONTAR_CATALOG_ITI
*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.

  SET PF-STATUS 'PF0300'.
  SET TITLEBAR  'TB0300'.

  IF ( var_edit EQ 'X' ).
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN: 'P_EMPRESA'.
          screen-input     = '0'.
          screen-invisible = '0'.
        WHEN: 'P_WERKS'.
          screen-input     = '0'.
          screen-invisible = '0'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.

    LOOP AT SCREEN.
      CASE screen-name.
        WHEN: 'P_EMPRESA'.
          screen-input     = '1'.
          screen-invisible = '0'.
        WHEN: 'P_WERKS'.
          screen-input     = '1'.
          screen-invisible = '0'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  PERFORM: criar_alv_ag_frete.

ENDMODULE.                 " PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0300 INPUT.

  CASE sy-ucomm.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'SAVE_AGE'.
      PERFORM: salvar_agente_pedagio.
    WHEN: 'CONS_AGE'.
      PERFORM: selecionar_agente_pedagio.
  ENDCASE.

ENDMODULE.                 " PAI_0300  INPUT

*&---------------------------------------------------------------------*
*&      Form  SALVAR_AGENTE_PEDAGIO
*&---------------------------------------------------------------------*
FORM salvar_agente_pedagio .

  DATA: wl_t001k TYPE t001k.

  SELECT SINGLE *
    FROM t001k INTO wl_t001k
   WHERE bwkey EQ p_werks
     AND bukrs EQ p_empresa.

  IF ( sy-subrc NE 0 ).
    MESSAGE e000(fi) DISPLAY LIKE 'W' WITH 'Centro não pertence a empresa informada.'.
  ELSE.

    IF ( var_edit IS INITIAL ).

      DATA: gw_zlest0090_insr TYPE zlest0090.

      CLEAR: gw_zlest0090_insr, gw_stable.

      gw_zlest0090_insr-mandt          = sy-mandt.
      gw_zlest0090_insr-bukrs          = p_empresa.
      gw_zlest0090_insr-werks          = p_werks.
      gw_zlest0090_insr-tp_servico     = zlest0090-tp_servico.
      gw_zlest0090_insr-nr_vr_xml_tipf = p_xml_tipf.

      IF ( radio_manual EQ 'X' ).
        gw_zlest0090_insr-tp_op = 'M'.
      ELSEIF ( radio_eletr EQ 'X' ).
        gw_zlest0090_insr-tp_op = 'E'.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = p_ag_ped
        IMPORTING
          output = gw_zlest0090_insr-agenteped.

      INSERT INTO zlest0090 VALUES gw_zlest0090_insr.

      COMMIT WORK.

      CLEAR: gw_zlest0090.

      gw_zlest0090-bukrs      = p_empresa.
      gw_zlest0090-werks      = p_werks.
      gw_zlest0090-tp_servico = zlest0090-tp_servico.
      gw_zlest0090-nr_vr_xml_tipf = p_xml_tipf.

      IF ( radio_manual EQ 'X' ).
        gw_zlest0090-tp_op = 'M'.
        gw_zlest0090-tp_op_desc = 'Manual'.
      ELSEIF ( radio_eletr EQ 'X' ).
        gw_zlest0090-tp_op = 'E'.
        gw_zlest0090-tp_op_desc = 'Eletrônica'.
      ENDIF.

      gw_zlest0090-agenteped = p_ag_ped.
      gw_zlest0090-opcao     = icon_change.
      APPEND gw_zlest0090 TO gt_zlest0090.

    ELSEIF ( var_edit EQ 'X' ) AND NOT ( var_index_edit IS INITIAL ).

      IF ( radio_manual EQ 'X' ).
        gw_zlest0090-tp_op = 'M'.
        gw_zlest0090-tp_op_desc = 'Manual'.
      ELSEIF ( radio_eletr EQ 'X' ).
        gw_zlest0090-tp_op = 'E'.
        gw_zlest0090-tp_op_desc = 'Eletrônica'.
      ENDIF.

      UPDATE zlest0090
         SET agenteped      = p_ag_ped
             tp_op          = gw_zlest0090-tp_op
             nr_vr_xml_tipf = p_xml_tipf
       WHERE bukrs      EQ p_empresa
         AND werks      EQ p_werks
         AND tp_servico EQ zlest0090-tp_servico.

      gw_zlest0090-agenteped      = p_ag_ped.
      gw_zlest0090-nr_vr_xml_tipf = p_xml_tipf.
      MODIFY gt_zlest0090 INDEX var_index_edit FROM gw_zlest0090 TRANSPORTING agenteped tp_servico tp_op nr_vr_xml_tipf tp_op_desc.

      CLEAR: gw_zlest0090.

    ENDIF.

    CLEAR: p_empresa, p_werks, p_ag_ped, var_edit, var_index_edit, zlest0090-tp_servico.

    LEAVE TO SCREEN 0300.

  ENDIF.
ENDFORM.                    " SALVAR_AGENTE_PEDAGIO

*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_AGENTE
*&---------------------------------------------------------------------*
FORM z_hotspot_agente  USING    p_row_id
                                p_column_id
                                p_row_no.

  CLEAR: gw_zlest0090.
  READ TABLE gt_zlest0090 INTO gw_zlest0090 INDEX p_row_id.

  IF ( sy-subrc EQ 0 ).

    p_empresa             = gw_zlest0090-bukrs.
    p_werks               = gw_zlest0090-werks.
    p_ag_ped              = gw_zlest0090-agenteped.
    zlest0090-tp_servico  = gw_zlest0090-tp_servico.

    var_edit       = 'X'.
    var_index_edit = p_row_id.

    LEAVE TO SCREEN 0300.

*    DELETE FROM ZLEST0090 WHERE BUKRS     EQ GW_ZLEST0090-BUKRS
*                            AND WERKS     EQ GW_ZLEST0090-WERKS
*                            AND TP_OP     EQ GW_ZLEST0090-TP_OP
*                            AND AGENTEPED EQ GW_ZLEST0090-AGENTEPED.
*
*
*    DELETE GT_ZLEST0090 INDEX P_ROW_ID.
*
*    COMMIT WORK.
*
*    CALL METHOD OBJ_GRID_AG->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = GW_STABLE.

  ENDIF.


ENDFORM.                    " Z_HOTSPOT_AGENTE

*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_AGENTE_PEDAGIO
*&---------------------------------------------------------------------*
FORM selecionar_agente_pedagio .

  DATA: lt_zlest0090_aux TYPE TABLE OF ty_zlest0090,
        wl_zlest0090_aux TYPE ty_zlest0090.


  REFRESH: lt_zlest0090_aux[].
  lt_zlest0090_aux[] = gt_zlest0090[].
  REFRESH: gt_zlest0090[].

  SELECT * FROM zlest0090
    INTO TABLE lt_zlest0090_aux.


  LOOP AT lt_zlest0090_aux INTO wl_zlest0090_aux.

    gw_zlest0090-bukrs      = wl_zlest0090_aux-bukrs.
    gw_zlest0090-werks      = wl_zlest0090_aux-werks.
    gw_zlest0090-tp_servico = wl_zlest0090_aux-tp_servico.
    gw_zlest0090-tp_op      = wl_zlest0090_aux-tp_op.
    gw_zlest0090-nr_vr_xml_tipf = wl_zlest0090_aux-nr_vr_xml_tipf.

    CASE gw_zlest0090-tp_op.
      WHEN: 'M'.
        gw_zlest0090-tp_op_desc = 'Manual'.
      WHEN: 'E'.
        gw_zlest0090-tp_op_desc = 'Eletrônica'.
    ENDCASE.

    gw_zlest0090-agenteped = wl_zlest0090_aux-agenteped.
    gw_zlest0090-opcao     = icon_change.

    APPEND gw_zlest0090 TO gt_zlest0090.

  ENDLOOP.

  LEAVE TO SCREEN 0300.

ENDFORM.                    " SELECIONAR_AGENTE_PEDAGIO
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_AG_FRETE
*&---------------------------------------------------------------------*
FORM criar_alv_ag_frete .

  DATA: zcl_evento TYPE REF TO zcl_evento.

  IF ( obj_custom_ag  IS INITIAL ).

    CREATE OBJECT obj_custom_ag
      EXPORTING
        container_name              = 'CONTAINER_AG'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    PERFORM: criar_catalog_ag.

    CREATE OBJECT obj_grid_ag
      EXPORTING
        i_parent          = obj_custom_ag
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT zcl_evento.
    SET HANDLER: zcl_evento=>zm_handle_hotspot_agente FOR obj_grid_ag.

    CALL METHOD obj_grid_ag->set_table_for_first_display
      CHANGING
        it_outtab                     = gt_zlest0090[]
        it_fieldcatalog               = gt_fcat_ag[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.

    CALL METHOD obj_grid_ag->refresh_table_display
      EXPORTING
        is_stable = gw_stable.
  ENDIF.


ENDFORM.                    " CRIAR_ALV_AG_FRETE
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_AG
*&---------------------------------------------------------------------*
FORM criar_catalog_ag .

  REFRESH: gt_fcat_ag[].

  PERFORM montar_catalog_ag USING:
      'OPCAO'       'Opção'                 '10'   '' 'X' '' 'C' '' '' '' '' '',
      'BUKRS'       'Empresa'               '10'   '' '' '' '' '' '' '' '' '',
      'WERKS'       'Centro'                '10'   '' '' '' '' '' '' '' '' '',
      'TP_SERVICO'  'Serviço'               '10'   '' '' '' '' '' '' '' '' '',
      'TP_OP_DESC'  'Tipo Operação'         '10'   '' '' '' '' '' '' '' '' '',
      'AGENTEPED'   'Ag. Pedágio.'          '10'   '' '' '' '' '' '' '' '' '',
      'NR_VR_XML_TIPF'   'Versão TipFrete'  '10'   '' '' '' '' '' '' '' '' ''.

ENDFORM.                    " CRIAR_CATALOG_AG
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_AG
*&---------------------------------------------------------------------*
FORM montar_catalog_ag  USING  VALUE(p_fieldname)
                               VALUE(p_desc)
                               VALUE(p_tam)
                               VALUE(p_no_zero)
                               VALUE(p_hotspot)
                               VALUE(p_cor)
                               VALUE(p_just)
                               VALUE(p_sum)
                               VALUE(p_edit)
                               VALUE(p_ref_tabname)   LIKE dd02d-tabname
                               VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                               VALUE(p_tabname)       LIKE dd02d-tabname.


  CLEAR: gw_fcat_ag.

  gw_fcat_ag-fieldname = p_fieldname.
  gw_fcat_ag-ref_table = p_ref_tabname..
  gw_fcat_ag-ref_field = p_ref_fieldname.
  gw_fcat_ag-tabname   = p_tabname.
  gw_fcat_ag-scrtext_l = p_desc.
  gw_fcat_ag-scrtext_m = p_desc.
  gw_fcat_ag-scrtext_s = p_desc.
  gw_fcat_ag-outputlen = p_tam.
  gw_fcat_ag-no_zero   = p_no_zero.
  gw_fcat_ag-hotspot   = p_hotspot.
  gw_fcat_ag-emphasize = p_cor.
  gw_fcat_ag-just      = p_just.
  gw_fcat_ag-do_sum    = p_sum.
  gw_fcat_ag-edit      = p_edit.

  APPEND gw_fcat_ag TO gt_fcat_ag.

ENDFORM.                    " MONTAR_CATALOG_AG
*&---------------------------------------------------------------------*
*&      Module  PAI_0400  OUTPUT
*&---------------------------------------------------------------------*
MODULE pai_0400 OUTPUT.

  SET PF-STATUS 'PF0400'.
  SET TITLEBAR  'TB0400'.

  PERFORM: criar_alv_categoria.

ENDMODULE.                 " PAI_0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0400  INPUT
*&---------------------------------------------------------------------*
MODULE pbo_0400 INPUT.

  CASE sy-ucomm.
    WHEN: 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN: 'SAVE_CAT'.

      PERFORM: salvar_categoria_veiculo.

    WHEN: 'CONS_CAT'.

      PERFORM: consulta_categoria_veiculo.

  ENDCASE.

ENDMODULE.                 " PBO_0400  INPUT
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_CATEGORIA
*&---------------------------------------------------------------------*
FORM criar_alv_categoria .

  DATA: zcl_evento TYPE REF TO zcl_evento.

  IF ( obj_custom_cat  IS INITIAL ).

    CREATE OBJECT obj_custom_cat
      EXPORTING
        container_name              = 'CONTAINER_CAT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    PERFORM: criar_catalog_cat.

    CREATE OBJECT obj_grid_cat
      EXPORTING
        i_parent          = obj_custom_cat
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT zcl_evento.
    SET HANDLER: zcl_evento=>zm_handle_hotspot_categoria FOR obj_grid_cat.

    CALL METHOD obj_grid_cat->set_table_for_first_display
      CHANGING
        it_outtab                     = gt_zlest0091[]
        it_fieldcatalog               = gt_fcat_cat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.

    CALL METHOD obj_grid_cat->refresh_table_display
      EXPORTING
        is_stable = gw_stable.
  ENDIF.

ENDFORM.                    " CRIAR_ALV_CATEGORIA
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_CAT
*&---------------------------------------------------------------------*
FORM criar_catalog_cat .

  REFRESH: gt_fcat_cat[].

  PERFORM montar_catalog_cat USING:
      'OPCAO'           'Opção'             '10'   '' 'X' '' 'C' '' '' '' '' '',
      'CATEGORIA'       'Categoria'         '10'   '' '' '' '' '' '' '' '' '',
      'QTD_EIXO'        'Qtd. Eixos'        '10'   '' '' '' '' '' '' '' '' '',
      'DESC_CATEGORIA'  'Descr. Categoria'  '45'   '' '' '' '' '' '' '' '' ''.

ENDFORM.                    " CRIAR_CATALOG_CAT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_CAT
*&---------------------------------------------------------------------*
FORM montar_catalog_cat   USING  VALUE(p_fieldname)
                                 VALUE(p_desc)
                                 VALUE(p_tam)
                                 VALUE(p_no_zero)
                                 VALUE(p_hotspot)
                                 VALUE(p_cor)
                                 VALUE(p_just)
                                 VALUE(p_sum)
                                 VALUE(p_edit)
                                 VALUE(p_ref_tabname)   LIKE dd02d-tabname
                                 VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                                 VALUE(p_tabname)       LIKE dd02d-tabname.


  CLEAR: gw_fcat_cat.

  gw_fcat_cat-fieldname = p_fieldname.
  gw_fcat_cat-ref_table = p_ref_tabname..
  gw_fcat_cat-ref_field = p_ref_fieldname.
  gw_fcat_cat-tabname   = p_tabname.
  gw_fcat_cat-scrtext_l = p_desc.
  gw_fcat_cat-scrtext_m = p_desc.
  gw_fcat_cat-scrtext_s = p_desc.
  gw_fcat_cat-outputlen = p_tam.
  gw_fcat_cat-no_zero   = p_no_zero.
  gw_fcat_cat-hotspot   = p_hotspot.
  gw_fcat_cat-emphasize = p_cor.
  gw_fcat_cat-just      = p_just.
  gw_fcat_cat-do_sum    = p_sum.
  gw_fcat_cat-edit      = p_edit.

  APPEND gw_fcat_cat TO gt_fcat_cat.

ENDFORM.                    " MONTAR_CATALOG_CAT
*&---------------------------------------------------------------------*
*&      Form  SALVAR_CATEGORIA_VEICULO
*&---------------------------------------------------------------------*
FORM salvar_categoria_veiculo .

  DATA: lt_zlest0091 TYPE zlest0091.

  CLEAR: lt_zlest0091,
         gw_zlest0091.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_categoria
    IMPORTING
      output = lt_zlest0091-categoria.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_qtd_eixo
    IMPORTING
      output = lt_zlest0091-qtd_eixo.

  lt_zlest0091-desc_categoria = p_desc_cat.

  INSERT INTO zlest0091 VALUES lt_zlest0091.
  COMMIT WORK.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_categoria
    IMPORTING
      output = gw_zlest0091-categoria.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_qtd_eixo
    IMPORTING
      output = gw_zlest0091-qtd_eixo.

  gw_zlest0091-desc_categoria = p_desc_cat.
  gw_zlest0091-opcao          = icon_delete.

  APPEND gw_zlest0091 TO gt_zlest0091.

  CLEAR: p_categoria, p_qtd_eixo, p_desc_cat.
  LEAVE TO SCREEN 0400.


ENDFORM.                    " SALVAR_CATEGORIA_VEICULO
*&---------------------------------------------------------------------*
*&      Form  CONSULTA_CATEGORIA_VEICULO
*&---------------------------------------------------------------------*
FORM consulta_categoria_veiculo.

  DATA: lt_zlest0091 TYPE TABLE OF zlest0091,
        wl_zlest0091 TYPE zlest0091.

  SELECT * FROM zlest0091
    INTO TABLE lt_zlest0091.

  REFRESH: gt_zlest0091[].

  LOOP AT lt_zlest0091 INTO wl_zlest0091.

    gw_zlest0091-categoria      = wl_zlest0091-categoria.
    gw_zlest0091-qtd_eixo       = wl_zlest0091-qtd_eixo.
    gw_zlest0091-desc_categoria = wl_zlest0091-desc_categoria.
    gw_zlest0091-opcao          = icon_delete.

    APPEND gw_zlest0091 TO gt_zlest0091.
  ENDLOOP.

  LEAVE TO SCREEN 0400.

ENDFORM.                    " CONSULTA_CATEGORIA_VEICULO
*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_CATEGORIA
*&---------------------------------------------------------------------*
FORM z_hotspot_categoria  USING    p_row_id
                                   p_column_id
                                   p_row_no.

  READ TABLE gt_zlest0091 INTO gw_zlest0091 INDEX p_row_id.

  IF ( sy-subrc EQ 0 ).

    DELETE FROM zlest0091 WHERE categoria EQ gw_zlest0091-categoria
                            AND qtd_eixo  EQ gw_zlest0091-qtd_eixo.

    COMMIT WORK.

    DELETE gt_zlest0091 INDEX p_row_id.

    LEAVE TO SCREEN 0400.
  ENDIF.

ENDFORM.                    " Z_HOTSPOT_CATEGORIA

INCLUDE zlesr0082_0500.

INCLUDE zlesr0082_0510.

INCLUDE zlesr0082_0301.

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_TABELA_90
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_tabela_90 .

  DATA: it_zlest0090 TYPE TABLE OF zlest0090 WITH HEADER LINE.

  SELECT * INTO TABLE it_zlest0090
    FROM zlest0090.

  READ TABLE it_zlest0090 INDEX 1.

  IF it_zlest0090-tp_servico EQ abap_false.
    UPDATE zlest0090 SET tp_servico = '1'.
    COMMIT WORK.
  ENDIF.

ENDFORM.

INCLUDE zlesr0082_0512.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0513  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0513 OUTPUT.
  SET PF-STATUS 'PF0513'.
  SET TITLEBAR 'TL0513'.

  PERFORM cria_alv_carga.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0513  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0513 INPUT.
  DATA: gt_gravar TYPE TABLE OF zlest0193,
        gw_gravar TYPE  zlest0193.


  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'SALVAR'.

      REFRESH gt_gravar.
      CLEAR gw_gravar.

      LOOP AT gt_zlest0193 INTO gw_zlest0193.
        gw_gravar-mandt      = sy-mandt.
        gw_gravar-matkl      = gw_zlest0193-matkl.
        gw_gravar-tipo_carga = gw_zlest0193-tipo_carga.
        APPEND gw_gravar TO gt_gravar.
        CLEAR: gw_gravar, gw_zlest0193.
      ENDLOOP.

      CHECK gt_gravar IS NOT INITIAL.

      MODIFY zlest0193 FROM TABLE gt_gravar.
      COMMIT WORK.
      MESSAGE 'Dados gravado com Sucesso!' TYPE 'S'.

      REFRESH gt_zlest0193.
      PERFORM busca_dados_carga.
      CALL METHOD obj_grid_carga->refresh_table_display.

    WHEN '&INS'.
      APPEND INITIAL LINE TO gt_zlest0193.

    WHEN  '&DEL'.
      CALL METHOD obj_grid_carga->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF tg_selectedrow IS INITIAL.
        MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
        EXIT.
      ELSE.

        READ TABLE tg_selectedrow INTO wg_selectedrow INDEX 1.

        READ TABLE gt_zlest0193 INTO gw_zlest0193 INDEX wg_selectedrow-index.
        IF sy-subrc = 0.
          DELETE FROM zlest0193 WHERE matkl      EQ gw_zlest0193-matkl
                                  AND tipo_carga EQ gw_zlest0193-tipo_carga.
        ENDIF.

        REFRESH gt_zlest0193.
        CLEAR gw_zlest0193.

        PERFORM busca_dados_carga.

        CALL METHOD obj_grid_carga->refresh_table_display.
      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cria_alv_carga .

  IF ( obj_custom_carga  IS INITIAL ).

    CREATE OBJECT obj_custom_carga
      EXPORTING
        container_name              = 'CONTAINER_CARGA'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    PERFORM: busca_dados_carga,
             criar_catalog_carga.


    CREATE OBJECT obj_grid_carga
      EXPORTING
        i_parent          = obj_custom_carga
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    gs_layout-stylefname = 'CELLTAB'.

    CALL METHOD obj_grid_carga->set_table_for_first_display
      EXPORTING
        is_layout                     = gs_layout
      CHANGING
        it_outtab                     = gt_zlest0193[]
        it_fieldcatalog               = gt_fcat_carga[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CALL METHOD obj_grid_carga->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.


    CALL METHOD obj_grid_carga->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD obj_grid_carga->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD obj_grid_carga->refresh_table_display
      EXPORTING
        is_stable = gw_stable.
  ENDIF.

ENDFORM.

FORM criar_catalog_carga .

  REFRESH: gt_fcat_carga[].

  PERFORM montar_catalog_carga USING:
      'MATKL'           'Grupo Mercadoria'     '12'   '' '' '' '' '' 'X' 'ZLEST0193' 'MATKL' '',
      'WGBEZ60'         'Descrição'            '50'   '' '' '' '' '' '' '' '' '',
      'TIPO_CARGA'      'Tipo Carga'           '12'   '' '' '' '' '' 'X' 'ZLEST0193' 'TIPO_CARGA' '',
      'DESCRICAO'       'Descrição'            '20'   '' '' '' '' '' ''  '' '' ''.

ENDFORM.                    " CRIAR_CATALOG_CAT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_CAT
*&---------------------------------------------------------------------*
FORM montar_catalog_carga   USING   VALUE(p_fieldname)
                                    VALUE(p_desc)
                                    VALUE(p_tam)
                                    VALUE(p_no_zero)
                                    VALUE(p_hotspot)
                                    VALUE(p_cor)
                                    VALUE(p_just)
                                    VALUE(p_sum)
                                    VALUE(p_edit)
                                    VALUE(p_ref_tabname)   LIKE dd02d-tabname
                                    VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                                    VALUE(p_tabname)       LIKE dd02d-tabname.


  CLEAR: gw_fcat_carga.

  gw_fcat_carga-fieldname = p_fieldname.
  gw_fcat_carga-ref_table = p_ref_tabname..
  gw_fcat_carga-ref_field = p_ref_fieldname.
  gw_fcat_carga-tabname   = p_tabname.
  gw_fcat_carga-scrtext_l = p_desc.
  gw_fcat_carga-scrtext_m = p_desc.
  gw_fcat_carga-scrtext_s = p_desc.
  gw_fcat_carga-outputlen = p_tam.
  gw_fcat_carga-no_zero   = p_no_zero.
  gw_fcat_carga-hotspot   = p_hotspot.
  gw_fcat_carga-emphasize = p_cor.
  gw_fcat_carga-just      = p_just.
  gw_fcat_carga-do_sum    = p_sum.
  gw_fcat_carga-edit      = p_edit.

  APPEND gw_fcat_carga TO gt_fcat_carga.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados_carga .

  DATA: it_carga_des TYPE TABLE OF dd07v,
        wa_carga_des TYPE  dd07v.


  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZTIPO_CARGA'
      text            = 'X'
    TABLES
      values_tab      = it_carga_des
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.


  SELECT *  FROM zlest0193 INTO TABLE @DATA(gt_193).

  LOOP AT gt_193 INTO DATA(gw_193).

    gw_zlest0193-matkl       = gw_193-matkl.
    gw_zlest0193-tipo_carga  = gw_193-tipo_carga.

    READ TABLE it_carga_des INTO wa_carga_des WITH KEY domvalue_l = gw_zlest0193-tipo_carga
                                                       ddlanguage = sy-langu.
    IF sy-subrc = 0.
      gw_zlest0193-descricao = wa_carga_des-ddtext.
    ENDIF.


    SELECT SINGLE * FROM t023t INTO @DATA(wa_t023t)
      WHERE matkl EQ @gw_zlest0193-matkl
      AND  spras  EQ @sy-langu.

    gw_zlest0193-wgbez60 = wa_t023t-wgbez60.

    FREE gw_zlest0193-celltab.
    gt_estilo =  VALUE #( ( fieldname = 'MATKL'      style = cl_gui_alv_grid=>mc_style_disabled )
                          ( fieldname = 'TIPO_CARGA' style = cl_gui_alv_grid=>mc_style_disabled ) ).
    INSERT LINES OF gt_estilo INTO TABLE gw_zlest0193-celltab.
    APPEND gw_zlest0193 TO gt_zlest0193.
    CLEAR: gw_zlest0193, gw_193, wa_carga_des.
  ENDLOOP.
ENDFORM.
