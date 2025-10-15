*&--------------------------------------------------------------------&*
*&                         Grupo André Maggi                          &*
*&--------------------------------------------------------------------&*
*& Projeto..: Aquaviário                                              &*
*& Data.....: 08/07/2013                                              &*
*& Descrição: Frete Aquaviário                                        &*
*&--------------------------------------------------------------------&*
REPORT  zlesr0073 MESSAGE-ID zles.

TYPE-POOLS: vrm,rmdi, zaqty.
*----------------------------------------------------------------------*
* includes
*----------------------------------------------------------------------*
INCLUDE: <icon>.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
"Tabelas do Frete Aquaviário
" ZLEST0056 - Frete Aquaviário     - Viagem - Tabela para Gravar Viagem
" ZLEST0058 - Sequencia de Viagens - Tabela para Gravar o Número da Viagem
TABLES: zlest0056, zlest0058, zsdt0001, zlest0074.


*----------------------------------------------------------------------*
* ESTRUTURA para SHDB
*----------------------------------------------------------------------*
DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.
DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      t_messtab  TYPE TABLE OF bdcmsgcoll,
      wa_bdcdata LIKE LINE OF ti_bdcdata.

*----------------------------------------------------------------------*
* ESTRUTURA
*----------------------------------------------------------------------*


DATA it_zlest0166 TYPE TABLE OF zlest0166.

*----------------------------------------------------------------------*
* Tabelas para executar a BAPI
*----------------------------------------------------------------------*
DATA: BEGIN OF it_headerdata OCCURS 0 .
        INCLUDE STRUCTURE bapisdhd1 .
DATA: END OF it_headerdata.

DATA: BEGIN OF it_itemdata OCCURS 0.
        INCLUDE STRUCTURE bapisditm.
DATA: END OF it_itemdata.
DATA: it_items_inx    TYPE TABLE OF bapisditmx WITH HEADER LINE,
      wl_items_inx    TYPE bapisditmx,
      tl_schedules_in TYPE TABLE OF bapischdl  WITH HEADER LINE,
      wl_schedules_in TYPE bapischdl,
      tl_vbuv         TYPE  TABLE OF vbuv WITH HEADER LINE,
      wl_vbuv         TYPE  vbuv,
      wl_fieldname    TYPE rmdi_name,
      wl_text         TYPE rmdi_ddtxt,
      tl_bapiparex    TYPE TABLE OF bapiparex WITH HEADER LINE,
      wl_bape_vbak    TYPE bape_vbak,
      wl_bape_vbakx   TYPE bape_vbakx.

DATA: BEGIN OF it_condition OCCURS 0.
        INCLUDE STRUCTURE bapicond .
DATA: END OF it_condition.

DATA: BEGIN OF it_partner OCCURS 0.
        INCLUDE STRUCTURE bapiparnr.
DATA: END OF it_partner.

DATA: BEGIN OF it_return OCCURS 0.
        INCLUDE STRUCTURE bapiret2.
DATA: END OF it_return.
DATA: wl_header_in   TYPE bapisdhd1,
      wl_header_inx2 TYPE bapisdh1x,
      wl_header_inx  TYPE bapisdhd1x,
      wl_return      TYPE bapiret2.

*S4H - ZLES0077 - Contingencia-Data Fatura-09.10.2023-JT-inicio
DATA: t_value       TYPE TABLE OF rgsb4,
      w_value       TYPE rgsb4,
      l_data_fatura TYPE sy-datum.
*S4H - ZLES0077 - Contingencia-Data Fatura-09.10.2023-JT-fim

*----------------------------------------------------------------------*
* INTERNAL TABLES / WORK AREA
*----------------------------------------------------------------------*
DATA: gt_saida_viagem            TYPE TABLE OF zaqty_saida_viagem WITH HEADER LINE, "Estrutura de Saida do ALV Viagem
      gw_saida_viagem            TYPE          zaqty_saida_viagem, "Work Area da Estrutura da Saida do ALV Viagem
      gw_popup_comboio           TYPE zaqty_envio_comboio,
      gt_saida_comboio           TYPE TABLE OF zaqty_saida_comboio, "Estrutura de Saida do ALV Comboio
      gw_saida_comboio           TYPE          zaqty_saida_comboio, "Work Area da Estrutura de Saida do ALV Comboio
      gt_saida_vinc_nf           TYPE TABLE OF zaqty_saida_vinc_nf, "Estrutura de Saida do ALV Vincular NF Geral
      gw_saida_vinc_nf           TYPE          zaqty_saida_vinc_nf, "Work Area de Saida do ALV Vincular NF Geral
      gt_saida_vinc_nf_nome      TYPE TABLE OF zaqty_saida_vinc_nf, "Estrutura de Saida do ALV Vincular NF por nome
      gw_saida_vinc_nf_nome      TYPE          zaqty_saida_vinc_nf, "Work Area de Saida do ALV Vincular NF por Nome
      gt_saida_vinc_romaneio     TYPE TABLE OF zaqty_saida_vinc_romaneio, "tabela de romaneio geral
      gw_saida_vinc_romaneio     TYPE          zaqty_saida_vinc_romaneio, "Work Area de romaneio geral

      gt_saida_vinc_rom_nome     TYPE TABLE OF zaqty_saida_vinc_romaneio, "tabela de romaneio nome
      gw_saida_vinc_rom_nome     TYPE          zaqty_saida_vinc_romaneio, "Work Area de romaneio nome
      gt_saida_vinc_rom_nome_aux TYPE TABLE OF zaqty_saida_vinc_romaneio, "tabela de romaneio nome
      gw_saida_vinc_rom_nome_aux TYPE          zaqty_saida_vinc_romaneio, "Work Area de romaneio nome

      gt_saida_ger_ov            TYPE TABLE OF zaqty_saida_ger_ov, "Estrutura de Saida do ALV Faturamento
      gw_saida_ger_ov            TYPE          zaqty_saida_ger_ov, "Work Area de Saida do ALV Faturamento

      gt_saida_viagens           TYPE TABLE OF zaqty_saida_viagens,
      gw_saida_viagens           TYPE zaqty_saida_viagens,

      gt_saida_arquivo           TYPE TABLE OF zaqty_saida_arquivo,
      gw_saida_arquivo           TYPE zaqty_saida_arquivo,

      gt_saida_import            TYPE TABLE OF zaqty_saida_import,
      gw_saida_import            TYPE zaqty_saida_import,

      gt_error_import            TYPE TABLE OF zaqty_error_import,
      gw_error_import            TYPE zaqty_error_import,
      gw_setleaf                 TYPE setleaf,

      gt_cfops_rfl               TYPE TABLE OF setleaf WITH HEADER LINE,
      gt_cfops_rin               TYPE TABLE OF setleaf WITH HEADER LINE,
      gt_cfops_rtr               TYPE TABLE OF setleaf WITH HEADER LINE,
      gt_cfops_imp               TYPE TABLE OF setleaf WITH HEADER LINE.


"TAbelas para o Buscar Arquivo.
DATA: gt_table_arquivo LIKE sdokpath OCCURS 0 WITH HEADER LINE,
      gw_table_arquivo TYPE sdokpath,
      gt_table_dir     LIKE sdokpath OCCURS 0 WITH HEADER LINE.
*----------------------------------------------------------------------*
* ALV CLASSES
*----------------------------------------------------------------------*
*Class definition for ALV toolbar
CLASS: lcl_alv_toolbar      DEFINITION DEFERRED.
CLASS: lcl_alv_toolbar_vn   DEFINITION DEFERRED.
CLASS: lcl_alv_toolbar_ov   DEFINITION DEFERRED.
CLASS: lcl_alv_toolbar_ro   DEFINITION DEFERRED.

DATA: obj_custom_viagem    TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da subtela 0500 - Viagem MAX
      obj_grid_viagem      TYPE REF TO cl_gui_alv_grid, "Classe referente a grid da subtela 0500 - Viagem MAX

      obj_custom_comboio   TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da subtela TAB 0800 - Comboio
      obj_grid_comboio     TYPE REF TO cl_gui_alv_grid, "Classe referente a grid da subtela TAB 0800 - Comboio
      obj_custom_vinc_nf   TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da subtela TAB 0900 - Vincular NF
      obj_grid_vinc_nf     TYPE REF TO cl_gui_alv_grid, "Classe referente a grid da subtela TAB 0900 - Vincular NF

      obj_custom_vinc_ge   TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da subtela TAB 0900 - Vincular NF
      obj_grid_vinc_ge     TYPE REF TO cl_gui_alv_grid, "Classe referente a grid da subtela TAB 0900 - Vincular NF

      obj_custom_ger_ov    TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da subtela TAB 0900 - Vincular NF
      obj_grid_ger_ov      TYPE REF TO cl_gui_alv_grid, "Classe referente a grid da subtela TAB 0900 - Vincular NF

      obj_custom_romaneio  TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da subtela TAB 1200 - Vincular Romaneio/NFs
      obj_grid_romaneio    TYPE REF TO cl_gui_alv_grid, "Classe referente a grid da subtela TAB 1200 - Vincular Romaneio/NFs

      obj_custom_viagens   TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da subtela TAB 1200 - Vincular Romaneio/NFs
      obj_grid_viagens     TYPE REF TO cl_gui_alv_grid, "Classe referente a grid da subtela TAB 1200 - Vincular Romaneio/NFs

      obj_custom_protocolo TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da subtela TAB 1800 - Protocolos
      obj_grid_protocolo   TYPE REF TO cl_gui_alv_grid, "Classe referente a grid da subtela TAB 1800 - Protocolos

      obj_custom_arquivo   TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da tela 1500
      obj_grid_arquivo     TYPE REF TO cl_gui_alv_grid, "Classe referente a grid da tela 1500

      obj_custom_import    TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da tela 1500
      obj_grid_import      TYPE REF TO cl_gui_alv_grid, "Classe referente a grid da tela 1500

      obj_custom_error     TYPE REF TO cl_gui_custom_container, "Classe referente ao Container da tela 1500
      obj_grid_error       TYPE REF TO cl_gui_alv_grid, "Classe referente a grid da tela 1500


      obj_toolbar_cb       TYPE REF TO lcl_alv_toolbar,
      obj_toolbar_vn       TYPE REF TO lcl_alv_toolbar_vn,
      obj_toolbar_ov       TYPE REF TO lcl_alv_toolbar_ov,
      obj_toolbar_ro       TYPE REF TO lcl_alv_toolbar_ro,


      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.

*Declaration for toolbar buttons
DATA : ty_toolbar      TYPE stb_button,
       wa_stable       TYPE lvc_s_stbl,
       tg_selectedcell TYPE lvc_t_cell,
       wg_selectedcell TYPE lvc_s_cell.

"DATA: OBJ_EVENT TYPE REF TO LCL_EVENT_RECEIVER. "Classe para eventos dos ALV
*----------------------------------------------------------------------*
* CATALOG
*----------------------------------------------------------------------*
DATA: gt_fcat_viagem    TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Viagem
      gw_fcat_viagem    TYPE lvc_s_fcat, "Controle VLA: catálogo de campos - Para Viagem

      gt_fcat_viagens   TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Viagem
      gw_fcat_viagens   TYPE lvc_s_fcat, "Catálogo de campos para controle visor de listas - Viagem

      gt_fcat_protocolo TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Protocolo
      gw_fcat_protocolo TYPE lvc_s_fcat, "Catálogo de campos para controle visor de listas - Protocolo

      gt_fcat_comboio   TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Comboio
      gw_fcat_comboio   TYPE lvc_s_fcat, "Controle VLA: catálogo de campos - Para Comboio

      gt_fcat_vinc_nf   TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Comboio
      gw_fcat_vinc_nf   TYPE lvc_s_fcat, "Controle VLA: catálogo de campos - Para Comboio

      gt_fcat_vinc_ge   TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Comboio
      gw_fcat_vinc_ge   TYPE lvc_s_fcat, "Controle VLA: catálogo de campos - Para Comboio

      gt_fcat_romaneio  TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Romaneio /Nfs
      gw_fcat_romaneio  TYPE lvc_s_fcat, "Controle VLA: catálogo de campos -  Romaneio /Nfs

      gt_fcat_ger_ov    TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Faturamento
      gw_fcat_ger_ov    TYPE lvc_s_fcat, "Controle VLA: catálogo de campos - Para Comboio - Faturamanento OV

      gt_fcat_arquivo   TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Arquivos XML
      gw_fcat_arquivo   TYPE lvc_s_fcat, "Controle VLA: catálogo de campos - Arquivos XML

      gt_fcat_import    TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Arquivos XML
      gw_fcat_import    TYPE lvc_s_fcat, "Controle VLA: catálogo de campos - Arquivos XML

      gt_fcat_error     TYPE lvc_t_fcat, "Catálogo de campos para controle visor de listas - Arquivos XML
      gw_fcat_error     TYPE lvc_s_fcat, "Controle VLA: catálogo de campos - Arquivos XML



      gt_f4             TYPE lvc_t_f4 WITH HEADER LINE. "Acionar F4

DATA: tl_bdc TYPE TABLE OF bdcdata,
      wl_bdc TYPE bdcdata.

DATA: tl_estilo_ov_aux TYPE TABLE OF zaqty_styl_tmp, "Estilo para Ordem de Venda
      wl_estilo_ov_aux TYPE zaqty_styl_tmp, "Estilo para Ordem de Venda
      tl_estilo_ov     TYPE lvc_t_styl WITH HEADER LINE, "Estilo para Ordem de Venda
      wl_estilo_ov     TYPE lvc_s_styl, "Estilo para Ordem de Venda


      tl_estilo_comb   TYPE lvc_t_styl WITH HEADER LINE, "Estilo para Comboio
      wl_estilo_comb   TYPE lvc_s_styl, "Estilo para Comboio

      tl_estilo_cell   TYPE lvc_t_scol WITH HEADER LINE, "Estilo para celula do Comboio
      wl_estilo_cell   TYPE lvc_s_scol. "Estilo para celula do Comboio.

*----------------------------------------------------------------------*
* TEXTFIELDS
*----------------------------------------------------------------------*
DATA: w_bukrs      TYPE t001-bukrs,  "Empresa
      w_desc_bukrs TYPE t001-butxt,  "Descrição da Empresa
      w_werks      TYPE t001w-werks, "Centro Emissor
      w_desc_werks TYPE t001w-name1, "Descrição do Centro
      w_ano        TYPE zlest0056-ano_viagem, "Ano da Viagem
      w_viagem     TYPE zlest0058-nr_viagem.  "Número da Viagem

"TextFields Pop-up da Tela de Vinculação
DATA: wp_bukrs          TYPE t001-bukrs, "Empresa
      wp_werks          TYPE t001w-werks, "Centro Emissor
      wp_ano            TYPE zlest0056-ano_viagem, "Ano da Viagem
      wp_viagem         TYPE zlest0058-nr_viagem, "Número da Viagem
      wp_frete_id       TYPE zlest0061-id_frete_aqua,
      wp_embarcacao(30) , "Barcaça
      wp_peso_previsto  TYPE zlest0057-peso_vinculado, "Peso Previsto
      wp_matnr(20)      , "Material
      wp_tp_class       TYPE c LENGTH 15, "Tipo de Classificação
      wp_eudr(10). "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
*----------------------------------------------------------------------*
* VARIAVEIS
*----------------------------------------------------------------------*
DATA: ok_code             TYPE sy-ucomm, "Código de função que acionou o PAI
      filtro              TYPE c, "Variável para configurar o botão Filtro (PBO - PBO_0100)
      novo                TYPE c, "Variável para configurar o botão Novo. (PBO - PBO_0100)
      busca               TYPE c, "Variável para configurar o botão Novo. (PBO - PBO_0100)
      valida              TYPE c, "Variável para validar os campos se estão preenchidos.
      wg_mensagem(30), "Mensagem de erro
      wg_acao(30),     "Controla ação do usuario
      wg_acao_cb(30),  "Controla ação do usuario
      wl_erro(1),
      wg_documento(10),
      x_field(30),     "Campo com erro
      vl_form             TYPE tdsfname,
      vl_name             TYPE rs38l_fnam,
      w_cont_geral        TYPE i,
      index_comboio       TYPE sy-tabix,
      vg_remetente        TYPE lfa1-lifnr,
      vg_destinatario     TYPE kna1-kunnr,
      opcao               TYPE c,
      gv_import           TYPE c,
      gv_edit             TYPE c,
      "VG_CALC_RETENCAO TYPE C,
      vg_vinc_nf_parc     TYPE c,
      vg_peso_retido      TYPE zlest0060-peso_retido,
      vg_peso_retido_utlz TYPE zlest0060-peso_retido_utlz,
      vg_desc_ret_acum    TYPE zlest0060-desc_ret_acum,
      vg_peso_aux         TYPE zlest0060-peso_retido,
      vg_sugere_dest      TYPE c,
      vg_bloq_viagem      TYPE c,
      vg_ok_aux           TYPE c,
      existe_protocolo    TYPE c,
      op_modo             TYPE c LENGTH 30,
      wl_werks            TYPE lfa1,         "*-BUG 154845-10.10.2024-JT-#154845-inicio
      lc_werks            TYPE lfa1-lifnr.   "*-BUG 154845-10.10.2024-JT-#154845-inicio

*----------------------------------------------------------------------*
* Tabela para erros de digitação
*----------------------------------------------------------------------*
DATA: tg_msg_ret TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      wl_msg_ret TYPE zfiwrs0002.
DATA: tl_zsdt0001       TYPE TABLE OF  zsdt0001,
      tl_zsdt0001_aux   TYPE TABLE OF zsdt0001,
      tl_0060_ret_ant   TYPE TABLE OF zlest0060 WITH HEADER LINE,
      tl_0060_ret_atual TYPE TABLE OF zlest0060 WITH HEADER LINE,
      tl_0060_retencao  TYPE TABLE OF zlest0060 WITH HEADER LINE,
      tl_forn_vinc_fis  TYPE TABLE OF zaqty_forn_vinc_fiscal WITH HEADER LINE,
      wl_zsdt0001       TYPE           zsdt0001,
      tl_zlest0057      TYPE TABLE OF zlest0057, "tabela com o resumo de notas fiscais vinculadas
      wl_zlest0057      TYPE          zlest0057, "Work Area com o resumo de notas fiscais vinculadas
      tl_zlest0060      TYPE TABLE OF zlest0060, "tabela com notas fiscais vinculadas
      wl_zlest0060      TYPE          zlest0060, "Work Area notas fiscais vinculadas,
      vpeso_vinc        TYPE          zlest0057-peso_vinculado,
      vl_parid          TYPE zsdt0001-parid,
      gw_zparametros    TYPE zparametros.


DATA: gt_estilo_romaneio TYPE lvc_t_styl WITH HEADER LINE,
      gw_estilo_romaneio TYPE lvc_s_styl.

*----------------------------------------------------------------------*
* TAB CONTROLS
*----------------------------------------------------------------------*
CONTROLS tabstrip TYPE TABSTRIP. "Controle do Tabstrip para Vinculações

*----------------------------------------------------------------------*
* TELAS
*----------------------------------------------------------------------*
CONSTANTS: tela_0100 TYPE sy-dynnr VALUE '0100', "Constante referente a TELA 0100 - Principal
           tela_0400 TYPE sy-dynnr VALUE '0400', "Constante referente a TELA 0400 - Subtela Viagem - MINIMIZADA (MIN)
           tela_0500 TYPE sy-dynnr VALUE '0500', "Constante referente a TELA 0500 - Subtela Viagem - MAXIMINIZADA (MAX)
           tela_0600 TYPE sy-dynnr VALUE '0600', "Constante referente a tela 0600 - Subtela Vinculações - MINIMIZADA (MIN)
           tela_0700 TYPE sy-dynnr VALUE '0700', "Constante referente a tela 0600 - Subtela Vinculações - MAXIMINIZADA (MAX)
           tela_0800 TYPE sy-dynnr VALUE '0800', "Constante referente a tela 0800 - Subtela TAB do Comboio
           tela_0900 TYPE sy-dynnr VALUE '0900', "Constante referente a tela 0900 - Subtela TAB da Vinculação de Notas Fiscais.
           tela_1000 TYPE sy-dynnr VALUE '1000', "Constante referente a tela 1000 - Subtela TAB das Notas Fiscais Vinculadas
           tela_1100 TYPE sy-dynnr VALUE '1100', "Constante referente a tela 1100 - Subtela TAB do Faturamento
           tela_1700 TYPE sy-dynnr VALUE '1700', "Constante referente a tela 1700 - Subtela Protocolo - MINIMIZADA (MIN)
           tela_1800 TYPE sy-dynnr VALUE '1800'. "Constante referente a tela 1800 - Subtela Protocolo - MAXIMINIZADA (MAX)

DATA: tela_viagem     TYPE n LENGTH 4 VALUE tela_0400, "Configuração da TELA_VIAGEM  Iniciando ela MINIMIZADA (MIN)
      tela_vinculacao TYPE n LENGTH 4 VALUE tela_0600, "Configuração da TELA_VINCULACAO Iniciando ela MINIMIZADA (MIN)
      tela_protocolo  TYPE n LENGTH 4 VALUE tela_1700. "Configuração da TELA_PROTOCOLO Iniciando ela MINIMIZADA (MIN)

DATA:  wg_tela1200           TYPE zaqty_tela1200.

"Constantes referente aos valores das TAB
CONSTANTS: tab_comboio TYPE c LENGTH 8  VALUE 'TAB_COMB',   "Tab do Comboio - Subtela 0700
           tab_nf      TYPE c LENGTH 6  VALUE 'TAB_NF',     "Tab das Notas Fiscais - Subtela 0700
           tab_nf_vinc TYPE c LENGTH 10 VALUE 'TAB_NFVINC', "Tab da Vinculação de Notas Fiscais - Subtela 0700
           tab_fatura  TYPE c LENGTH 10 VALUE 'TAB_FATU'.   "Tab do Faturamento - Subtela 0700

"Constantes referente LAYOUT
CONSTANTS: c_x              TYPE c VALUE 'X',
           c_add(3)         TYPE c VALUE 'ADD',
           c_del(3)         TYPE c VALUE 'DEL',
           c_save(15)       TYPE c VALUE 'BTN_SAVE_VIAGEM',
           c_edit(15)       TYPE c VALUE 'BTN_EDIT_VIAGEM',
           c_show_msgre(10) TYPE c VALUE 'SHOW_MSGRE',
           c_clos_msg(8)    TYPE c VALUE 'CLOS_MSG',
           c_exit(4)        TYPE c VALUE 'EXIT',
           c_back(4)        TYPE c VALUE 'BACK',
           c_cancel(6)      TYPE c VALUE 'CANCEL',
           c_email_c(13)    TYPE c VALUE 'EMAIL_COMBOIO',
           c_qtdv(4)        TYPE c VALUE 'QTDV',
           c_imp_xml(7)     TYPE c VALUE 'IMP_XML',
           c_del_emb(7)     TYPE c VALUE 'DEL_EMB'.

"Retorno da BAPI_SALESORDER_CREATEFROMDAT2
DATA vbeln_return     TYPE bapivbeln-vbeln.
"DATA: TOTAL_VINCULADO TYPE ZLEST0057-PESO_VINCULADO.
DATA: total_vinculado TYPE brgew,
      total_vlr_vinc  TYPE j_1bnflin-netwr.

"Objetos
DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd, "Classe de Utilitário SD
      obj_zcl_util    TYPE REF TO zcl_util. "Classe para Utilidade Geral.

DATA: area_parametro_filial_eudr TYPE c. " Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328

*-----------------------------------------------------------------------
* Classe Eventos das GRID´s
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    " Viagem
    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
    CLASS-METHODS:
      on_f4_vg                      FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

    " Comboio
    CLASS-METHODS:
      on_data_changed_cb FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished_cb FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_f4_cb                      FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

    CLASS-METHODS:
      on_data_changed_ro FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished_ro FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


    " O.V faturamento
    CLASS-METHODS:
      on_data_changed_ov FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

*    CLASS-METHODS:
*     ON_DATA_CHANGED_FINISHED_OV FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
*                      IMPORTING E_MODIFIED ET_GOOD_CELLS.

    CLASS-METHODS:
      on_f4_ov                      FOR EVENT onf4                 OF cl_gui_alv_grid
        IMPORTING e_fieldname
                  es_row_no
                  er_event_data
                  et_bad_cells
                  e_display.

    CLASS-METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.


  METHOD on_data_changed.
    DATA: ls_good      TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          vl_value     TYPE lvc_value,
          wl_lfa1      TYPE lfa1,
          wl_kna1      TYPE kna1,
          wl_zlest0064 TYPE zlest0064,

          opt          TYPE ctu_params,
          vl_lifnr     TYPE lifnr.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'PO_EMBARQUE'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE *
        FROM lfa1
        INTO wl_lfa1
        WHERE lifnr = lv_value.

      CLEAR: vl_lifnr.
      vl_lifnr = wl_lfa1-lifnr.

      IF sy-subrc EQ 0.
        lv_value = wl_lfa1-name1.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'NAME1_EMB'
            i_value     = lv_value.

        lv_value = wl_lfa1-ort01.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'ORT01_EMB'
            i_value     = lv_value.

        lv_value = wl_lfa1-regio.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'REGIO_EMB'
            i_value     = lv_value.
      ENDIF.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'PO_DESTINO'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE *
        FROM kna1
        INTO wl_kna1
        WHERE kunnr = lv_value.

      IF sy-subrc EQ 0.

        lv_value = wl_kna1-name1.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'NAME1_DEST'
            i_value     = lv_value.

        lv_value = wl_kna1-ort01.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'ORT01_DEST'
            i_value     = lv_value.

        lv_value = wl_kna1-regio.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'REGIO_DEST'
            i_value     = lv_value.

      ENDIF.

      IF NOT ( vl_lifnr IS INITIAL ).

        lv_value = ls_good-value.

        CONDENSE lv_value NO-GAPS.
        SELECT SINGLE * FROM zlest0064 INTO wl_zlest0064 WHERE po_embarque EQ vl_lifnr
                                                           AND po_destino  EQ lv_value.

        IF ( sy-subrc EQ 0 ).
          CASE wl_zlest0064-direcao.
            WHEN: 'N'.
              lv_value = 'Norte'.
            WHEN: 'S'.
              lv_value = 'Sul'.
            WHEN: 'L'.
              lv_value = 'Leste'.
            WHEN: 'O'.
              lv_value = 'Oeste'.
          ENDCASE.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'DIRECAO'
              i_value     = lv_value.
        ENDIF.
      ENDIF.
      CLEAR: vl_lifnr.
    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.

    DATA: ls_good    TYPE lvc_s_modi,
          lw_setleaf TYPE setleaf,
          tl_estilo  TYPE lvc_t_styl,
          wl_estilo  TYPE lvc_s_styl.


    IF et_good_cells[] IS NOT INITIAL.
      READ TABLE et_good_cells INTO ls_good INDEX 1.

      READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX ls_good-row_id.
      IF sy-subrc = 0.
        CLEAR: gw_saida_viagem-estilo, tl_estilo[].

        SELECT SINGLE * FROM setleaf
          INTO lw_setleaf
         WHERE setname EQ 'MAGGI_ZLES0077_CALADO'
           AND valfrom EQ gw_saida_viagem-po_embarque.

        IF sy-subrc NE 0.
          wl_estilo-fieldname = 'CALADO'.
          wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
          APPEND wl_estilo TO tl_estilo.
          CLEAR: gw_saida_viagem-calado, gw_saida_viagem-und_medida.
        ENDIF.

        INSERT LINES OF tl_estilo INTO TABLE gw_saida_viagem-estilo.
        MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX ls_good-row_id.
      ENDIF.

    ENDIF.

*** Método de atualização de dados na Tela
    CALL METHOD obj_grid_viagem->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '100'
        i_show        = space
        i_repid       = sy-repid
        i_pressed_tab = 'TABSTRIP-ACTIVETAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.

  ENDMETHOD.                    "on_data_changed_finishe

  " comboio
  METHOD on_data_changed_cb.
    DATA: ls_good      TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          vl_value     TYPE lvc_value,
          wl_zlest0053 TYPE zlest0053,
          wl_zlest0054 TYPE zlest0054,
          wl_makt      TYPE makt,
          wl_mara      TYPE mara,
          vcalado      TYPE zlest0054-calado,
          vl_checked   TYPE c.


    READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX 1.

    "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 --->>>
    IF area_parametro_filial_eudr IS NOT INITIAL.
      LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'EUDR'.

        DATA(lva_update)  = abap_false.

        READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX ls_good-row_id.

        IF gw_saida_comboio-matnr IS INITIAL.
          CLEAR: lv_value.
          lva_update = abap_true.
        ELSE.

          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.

          IF lv_value IS NOT INITIAL.
            DATA(lva_eudr) = zcl_eudr_utils=>check_material_eudr( i_matnr = gw_saida_comboio-matnr ).
            IF lva_eudr EQ abap_false.
              lva_update = abap_true.
              CLEAR: lv_value.
              MESSAGE 'Material não é EUDR!' TYPE 'S'.
            ENDIF.
          ENDIF.
        ENDIF.

        IF lva_update EQ abap_true.
          gw_saida_comboio-eudr = lv_value.
          MODIFY gt_saida_comboio INDEX ls_good-row_id FROM gw_saida_comboio TRANSPORTING eudr.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'EUDR'
              i_value     = lv_value.
        ENDIF.

      ENDLOOP.

      LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'MATNR'.

        lva_update = abap_false.

        READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX ls_good-row_id.

        DATA(lv_value_matnr) = ls_good-value.

        IF lv_value_matnr IS INITIAL.
          CLEAR: lv_value.
          lva_update = abap_true.
        ELSE.
          CONDENSE lv_value_matnr NO-GAPS.

          lva_eudr = zcl_eudr_utils=>check_material_eudr( i_matnr = CONV #( lv_value_matnr ) ).
          IF lva_eudr EQ abap_false.
            CLEAR: lv_value.
            lva_update = abap_true.
          ENDIF.

        ENDIF.

        IF lva_update EQ abap_true.
          gw_saida_comboio-eudr = lv_value.
          MODIFY gt_saida_comboio INDEX ls_good-row_id FROM gw_saida_comboio TRANSPORTING eudr.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'EUDR'
              i_value     = lv_value.
        ENDIF.

      ENDLOOP.
    ENDIF.
    "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 <<<---


    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'MATNR'.

      READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX ls_good-row_id.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      CLEAR: wl_mara, wl_makt.

      SELECT SINGLE * FROM makt
        INTO wl_makt
        WHERE matnr EQ lv_value
        AND spras   EQ 'P'.

      IF sy-subrc EQ 0.

        SELECT SINGLE * FROM mara
          INTO wl_mara
          WHERE matnr EQ lv_value.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_mara-matkl
          IMPORTING
            output = gw_saida_comboio-matkl.

        MODIFY gt_saida_comboio INDEX ls_good-row_id FROM gw_saida_comboio TRANSPORTING matkl.

        lv_value = wl_makt-maktx.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'MAKTX'
            i_value     = lv_value.

      ELSE.

        CLEAR lv_value.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'MAKTX'
            i_value     = lv_value.
      ENDIF.


    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'EMBARCACAO'.

      READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX ls_good-row_id.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE *
        FROM zlest0053
        INTO wl_zlest0053
        WHERE embarcacao = lv_value+0(1)
        AND   nome       = gw_saida_comboio-nome.


      IF sy-subrc EQ 0.
        CLEAR lv_value.
        IF wl_zlest0053-tp_barcaca = 'A'.
          lv_value = 'A-Acoplável'.
        ELSEIF wl_zlest0053-tp_barcaca = 'B'.
          lv_value = 'B-Box'.
        ELSEIF wl_zlest0053-tp_barcaca = 'P'.
          lv_value = 'P-Proa'.
        ELSEIF wl_zlest0053-tp_barcaca = 'M'.
          lv_value = 'M-Mineralizadora'.
        ENDIF.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'TP_BARCACA'
            i_value     = lv_value.

        CLEAR: wl_zlest0054.

      ELSE.

        CLEAR lv_value.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'TP_BARCACA'
            i_value     = lv_value.
      ENDIF.


    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'NOME'.

      READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX ls_good-row_id.
      lv_value = ls_good-value.

      SELECT SINGLE *
        FROM zlest0053
        INTO wl_zlest0053
        WHERE embarcacao = gw_saida_comboio-embarcacao+0(1)
        AND   nome       = lv_value.

      IF sy-subrc EQ 0.
        CLEAR lv_value.
        IF wl_zlest0053-tp_barcaca = 'A'.
          lv_value = 'A-Acoplável'.
        ELSEIF wl_zlest0053-tp_barcaca = 'B'.
          lv_value = 'B-Box'.
        ELSEIF wl_zlest0053-tp_barcaca = 'P'.
          lv_value = 'P-Proa'.
        ELSEIF wl_zlest0053-tp_barcaca = 'M'.
          lv_value = 'M-Mineralizadora'.
        ENDIF.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'TP_BARCACA'
            i_value     = lv_value.

        SELECT SINGLE * FROM zlest0054 INTO wl_zlest0054 WHERE calado     EQ gw_saida_viagem-calado
                                                           AND tp_barcaca EQ lv_value(1).

        CLEAR: lv_value.
        lv_value = wl_zlest0054-und_medida.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'UNID_MEDIDA'
            i_value     = lv_value.

        CLEAR: lv_value.
        lv_value = wl_zlest0054-capacidade.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'PESO_PREVISTO'
            i_value     = lv_value.

      ELSE.

        CLEAR lv_value.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'TP_BARCACA'
            i_value     = lv_value.


        CLEAR: lv_value.
        lv_value = wl_zlest0054-und_medida.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'UNID_MEDIDA'
            i_value     = lv_value.

        CLEAR: lv_value.
        lv_value = wl_zlest0054-capacidade.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'PESO_PREVISTO'
            i_value     = lv_value.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.                    "ON_DATA_CHANGED_CB

  METHOD on_data_changed_finished_cb.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_CB

  METHOD on_f4_vg.

    TYPES: BEGIN OF t_f4_structure,
             fieldtext TYPE dfies-fieldtext,
             fieldname TYPE dfies-fieldname,
           END OF t_f4_structure.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA: ls_modi TYPE lvc_s_modi.

    CASE e_fieldname.

      WHEN 'CALADO'.

        TYPES : BEGIN OF ty_zlest0054,
                  calado     TYPE zlest0054-calado,
                  und_medida TYPE zlest0054-und_medida,
                END OF ty_zlest0054.


        DATA: wl_return_chv TYPE  ddshretval,
              wl_dselcchv   TYPE  dselc,
              tl_zlest0054  TYPE TABLE OF ty_zlest0054,
              wl_zlest0054  TYPE ty_zlest0054,
              tl_return_chv TYPE TABLE OF ddshretval,
              tl_dselcchv   TYPE TABLE OF dselc.

        SELECT DISTINCT calado und_medida
          FROM zlest0054
          INTO TABLE tl_zlest0054
          ORDER BY calado ASCENDING.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'CALADO'
            value_org       = 'S'
          TABLES
            value_tab       = tl_zlest0054
            return_tab      = tl_return_chv
            dynpfld_mapping = tl_dselcchv.

        READ TABLE tl_return_chv INTO wl_return_chv INDEX 1.

        IF sy-subrc = 0 AND wl_return_chv-fieldval <> ''.

          ASSIGN er_event_data->m_data->* TO <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'CALADO'.
          ls_modi-value     = wl_return_chv-fieldval.
          APPEND ls_modi TO <itab>.

          er_event_data->m_event_handled = 'X'.
          CLEAR: wl_zlest0054.

          READ TABLE tl_zlest0054 INTO wl_zlest0054 INDEX es_row_no-row_id.
          gw_saida_viagem-und_medida = wl_zlest0054-und_medida.
          MODIFY gt_saida_viagem INDEX  es_row_no-row_id FROM gw_saida_viagem TRANSPORTING und_medida.

        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "ON_F4_VG

  METHOD on_f4_cb.
    TYPES: BEGIN OF t_f4_structure,
             fieldtext TYPE dfies-fieldtext,
             fieldname TYPE dfies-fieldname,
           END OF t_f4_structure.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA: ls_modi TYPE lvc_s_modi.

    READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX es_row_no-row_id.
    CASE e_fieldname.
      WHEN 'NOME'.
        IF gw_saida_comboio-embarcacao+0(1) IS INITIAL OR
           NOT 'B_E_R' CS gw_saida_comboio-embarcacao+0(1).
          MESSAGE s005(zaquaviario) DISPLAY LIKE 'W'.
        ELSE.
          TYPES : BEGIN OF ty_zlest0053,
                    nome       TYPE zlest0053-nome,
                    tp_barcaca TYPE zlest0053-tp_barcaca,
                  END OF ty_zlest0053.


          DATA: wl_return_chv TYPE  ddshretval,
                wl_dselcchv   TYPE  dselc,
                tl_zlest0053  TYPE TABLE OF ty_zlest0053,
                wl_zlest0053  TYPE ty_zlest0053,
                tl_return_chv TYPE TABLE OF ddshretval,
                tl_dselcchv   TYPE TABLE OF dselc.

          SELECT nome tp_barcaca
            FROM zlest0053
            INTO TABLE tl_zlest0053
            WHERE embarcacao = gw_saida_comboio-embarcacao+0(1)
            AND bukrs EQ w_bukrs.

          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              retfield        = 'NOME'
              value_org       = 'S'
            TABLES
              value_tab       = tl_zlest0053
              return_tab      = tl_return_chv
              dynpfld_mapping = tl_dselcchv.

          READ TABLE tl_return_chv INTO wl_return_chv INDEX 1.
          IF sy-subrc = 0 AND wl_return_chv-fieldval <> ''.
            ASSIGN er_event_data->m_data->* TO <itab>.
            ls_modi-row_id    = es_row_no-row_id.
            ls_modi-fieldname = 'NOME'.
            ls_modi-value     = wl_return_chv-fieldval.
            APPEND ls_modi TO <itab>.

            er_event_data->m_event_handled = 'X'.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "ON_F4_CB

  "Romaneio
  METHOD on_data_changed_ro.

    DATA: ls_good      TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          vl_value     TYPE lvc_value,
          "WL_ZLEST0065  TYPE ZLEST0065,
          wl_zlest0073 TYPE zlest0073,
          wl_zlest0060 TYPE zlest0060,
          wl_lfa1_nf   TYPE lfa1,
          lw_setleaf   TYPE setleaf,
          vl_vlr_vinc  TYPE brgew.

*    DATA: vl_peso_vinculado  TYPE zlest0073-peso_vinculado,
*          vl_valor_vinculado TYPE zlest0073-valor_vinculado.

    DATA: tabix TYPE sy-tabix.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'CHECK'.

      CHECK gt_saida_vinc_rom_nome[] IS NOT INITIAL.

      CLEAR: gw_saida_vinc_rom_nome.
      READ TABLE gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome INDEX ls_good-row_id.

      CASE gw_saida_vinc_rom_nome-check.
        WHEN: 'X'.
          PERFORM f_desmarcar_rom USING ls_good-row_id 'X' 'X'
                               CHANGING vg_ok_aux.
        WHEN OTHERS.
          PERFORM f_marcar_rom USING ls_good-row_id 0 'X'.
      ENDCASE.

      PERFORM f_atualiza_saldo_retencao USING ''.

      IF gw_saida_vinc_rom_nome-desc_ret_acum IS NOT INITIAL.
        PERFORM f_action_1200_enter_nf.
      ENDIF.

      LEAVE TO SCREEN 1200.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'PESO_VINC'.


      DATA: valor_vinculado TYPE zlest0060-netwr.

      CLEAR: gw_saida_vinc_rom_nome.
      READ TABLE gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome INDEX ls_good-row_id.

      CONDENSE ls_good-value NO-GAPS.

      IF ( sy-subrc EQ 0 ).

        IF ( ls_good-value > gw_saida_vinc_rom_nome-peso_fiscal ).
          MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Peso à vincular maior que o Peso Total da Nota'.
          CLEAR: lv_value.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'PESO_VINC'
              i_value     = lv_value.
        ELSE.

          valor_vinculado = ( ( gw_saida_vinc_rom_nome-netwr / gw_saida_vinc_rom_nome-peso_fiscal ) * ls_good-value ).

          total_vinculado = total_vinculado - gw_saida_vinc_rom_nome-peso_fiscal.
          total_vinculado = total_vinculado + ls_good-value.

          gw_saida_vinc_rom_nome-peso_vinc = ls_good-value.
          MODIFY gt_saida_vinc_rom_nome FROM gw_saida_vinc_rom_nome INDEX ls_good-row_id TRANSPORTING peso_vinc.

          CLEAR: lv_value.
          lv_value =  valor_vinculado.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'VALOR_VINCULADO'
              i_value     = lv_value.

          gv_edit = 'X'.
          LEAVE TO SCREEN 1200.

        ENDIF.

      ENDIF.


    ENDLOOP.


  ENDMETHOD.                    "ON_DATA_CHANGED_RO

  METHOD on_data_changed_finished_ro.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_RO

  "O.V. Faturamento
  METHOD on_data_changed_ov.


    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value.

    DATA: wl_zlest0055     TYPE zlest0055, "Work Area Cadastro de Preço
          wl_zlest0055_aux TYPE zlest0055,
          wl_zlest0059     TYPE zlest0059,
          wl_mara          TYPE mara,
          wl_kna1          TYPE kna1,
          wl_lfa1          TYPE lfa1,
          tl_kna1          TYPE TABLE OF kna1,
          kunnr1           TYPE kna1-kunnr,
          stcd_str         TYPE c LENGTH 9,
          stcd_conc        TYPE c LENGTH 4,
          v_vlr_usd        TYPE zaqty_saida_ger_ov-vlr_usd,
          v_vlr_brl        TYPE zaqty_saida_ger_ov-vlr_brl,
          v_tax_dolar      TYPE zaqty_saida_ger_ov-tax_dolar,
          wl_tcurr         TYPE tcurr,
          vl_tabix         TYPE sy-tabix,
          v_data           TYPE c LENGTH 10,
          v_data_aux       TYPE c LENGTH 10,
          cont_moeda       TYPE sy-tabix,
          dia              TYPE c LENGTH 2,
          dia_aux          TYPE c LENGTH 2,
          mes              TYPE c LENGTH 2,
          ano              TYPE c LENGTH 4,
          vl_ukurs         TYPE ukurs_curr,
          vl_gdatu         TYPE gdatu_inv.



    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'TAX_DOLAR'.

      READ TABLE gt_saida_ger_ov INTO gw_saida_ger_ov INDEX ls_good-row_id.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      v_tax_dolar = lv_value.
      lv_value    = gw_saida_ger_ov-vlr_usd * v_tax_dolar.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_BRL'
          i_value     = lv_value.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'WAERK'.

      CALL METHOD obj_grid_vinc_nf->get_selected_rows
        IMPORTING
          et_index_rows = DATA(tl_rows_vinc_nf).

      READ TABLE tl_rows_vinc_nf INDEX 1 INTO DATA(sl_rows_vinc_nf).
      READ TABLE gt_saida_vinc_nf_nome INTO DATA(gw_saida_vinc_nf) INDEX sl_rows_vinc_nf-index.


      READ TABLE gt_saida_ger_ov INTO gw_saida_ger_ov INDEX ls_good-row_id.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE * FROM mara
        INTO wl_mara
        WHERE matnr = gw_saida_ger_ov-matnr.

      " Ordem de Venda x Material
      SELECT SINGLE * FROM zlest0059
        INTO wl_zlest0059
        WHERE bukrs       EQ w_bukrs
          AND auart       EQ gw_saida_ger_ov-auart
          AND operacao    EQ gw_saida_vinc_nf-operacao "RJF
          AND po_embarque EQ gw_saida_viagem-po_embarque
          AND po_destino  EQ gw_saida_viagem-po_destino.

      IF sy-subrc NE 0.
        " Ordem de Venda x Material
        SELECT SINGLE * FROM zlest0059
          INTO wl_zlest0059
          WHERE bukrs       EQ w_bukrs
            AND auart       EQ gw_saida_ger_ov-auart
            AND po_embarque EQ gw_saida_viagem-po_embarque
            AND po_destino  EQ gw_saida_viagem-po_destino.

        IF sy-subrc NE 0.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Material não encontrado para Tipo Venda'.
          EXIT.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_mara-matkl
        IMPORTING
          output = wl_mara-matkl.

*------------------------------------------------------------------------------------*
*     Buscar Cadastro de preço
*------------------------------------------------------------------------------------*

      CLEAR: wl_zlest0055.

      "Buscar por Tp Transgenia.
      SELECT SINGLE * FROM zlest0055
        INTO wl_zlest0055
      WHERE kunnr       EQ gw_saida_ger_ov-cl_codigo
        AND matkl       EQ wl_mara-matkl
        AND auart       EQ gw_saida_ger_ov-auart
        AND dt_fim      GE gw_saida_ger_ov-dt_fatura
        AND waerk       EQ lv_value
        AND status      EQ '1'
        AND vkorg       EQ w_bukrs
        AND po_embarque EQ gw_saida_viagem-po_embarque
        AND po_destino  EQ gw_saida_viagem-po_destino
        AND tp_transgenia EQ gw_saida_comboio-tp_class(2)
        AND operacao      EQ gw_saida_vinc_nf-operacao.

      IF sy-subrc NE 0.
        SELECT SINGLE * FROM zlest0055
          INTO wl_zlest0055
        WHERE kunnr       EQ gw_saida_ger_ov-cl_codigo
          AND matkl       EQ wl_mara-matkl
          AND auart       EQ gw_saida_ger_ov-auart
          AND dt_fim      GE gw_saida_ger_ov-dt_fatura
          AND waerk       EQ lv_value
          AND status      EQ '1'
          AND vkorg       EQ w_bukrs
          AND po_embarque EQ gw_saida_viagem-po_embarque
          AND po_destino  EQ gw_saida_viagem-po_destino
          AND operacao    EQ gw_saida_vinc_nf-operacao.
      ENDIF.



*        MOVE: wl_zlest0055-vkorg TO gw_saida_ger_ov-vkorg,
*              wl_zlest0055-vtweg TO gw_saida_ger_ov-vtweg,
*              wl_zlest0055-spart TO gw_saida_ger_ov-spart,
*              wl_zlest0055-kunnr TO gw_saida_ger_ov-kunnr,
*              "WL_ZLEST0055-ZTERM TO GW_SAIDA_GER_OV-ZTERM,
*              wl_zlest0059-matnr TO gw_saida_ger_ov-matnr_ov,
*              wl_zlest0055-kurst TO gw_saida_ger_ov-kurst,
*              wl_zlest0055-waerk_fatura TO gw_saida_ger_ov-waerk_fatura.
*
*        PERFORM f_atrib_zterm_ov USING wl_zlest0055
*                              CHANGING gw_saida_ger_ov.
*
*        MODIFY gt_saida_ger_ov INDEX ls_good-row_id FROM gw_saida_ger_ov.
*
*        lv_value = wl_zlest0055-netpr.
*
*        CALL METHOD er_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'NETPR'
*            i_value     = lv_value.
*
*        v_vlr_usd =  ( ( gw_saida_ger_ov-peso_vinculado / 1000 ) * wl_zlest0055-netpr ).
*
*        lv_value = v_vlr_usd.
*
*        CALL METHOD er_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'VLR_USD'
*            i_value     = lv_value.
*
*        IF gw_saida_ger_ov-tax_dolar GT 0.
*          lv_value = v_vlr_usd * gw_saida_ger_ov-tax_dolar.
*          CALL METHOD er_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = ls_good-row_id
*              i_fieldname = 'VLR_BRL'
*              i_value     = lv_value.
*        ENDIF.

      IF sy-subrc NE 0. "Caso o preço não seja encontrado na tabela ZLEST0059 fazer a busca do mesmo em um CNPJ raiz.

        CLEAR: wl_kna1, wl_lfa1.
        CASE gw_saida_ger_ov-tomador_serv.
          WHEN 'D'.
            SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ gw_saida_ger_ov-cl_codigo.
          WHEN 'R'.
            SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ gw_saida_ger_ov-cl_codigo.
            IF sy-subrc NE 0.
              SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ gw_saida_ger_ov-cl_codigo.
              SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ wl_lfa1-kunnr.
            ENDIF.
        ENDCASE.

        IF ( sy-subrc EQ 0 ).

          CONCATENATE wl_kna1-stcd1(8) '%' INTO stcd_str.

          SELECT * FROM kna1
            INTO TABLE tl_kna1
          WHERE stcd1 LIKE stcd_str.

          CHECK NOT tl_kna1[] IS INITIAL.
          CLEAR: wl_kna1.

          LOOP AT tl_kna1 INTO wl_kna1.

            vl_tabix = sy-tabix.

            CLEAR: stcd_conc.
            CONCATENATE wl_kna1-stcd1+8(1) wl_kna1-stcd1+9(1) wl_kna1-stcd1+10(1) wl_kna1-stcd1+11(1) INTO stcd_conc.

            IF ( stcd_conc NE '0001' ).
              DELETE tl_kna1 INDEX vl_tabix.
            ELSE.

              "Busca por Transgenia
              SELECT SINGLE *
               FROM zlest0055
               INTO wl_zlest0055
               WHERE kunnr EQ wl_kna1-kunnr
               AND matkl   EQ wl_mara-matkl
               AND auart   EQ gw_saida_ger_ov-auart
               AND dt_fim  GE gw_saida_ger_ov-dt_fatura
               AND waerk   EQ lv_value
               AND status  EQ '1'
               AND vkorg   EQ w_bukrs
               AND po_embarque EQ gw_saida_viagem-po_embarque
               AND po_destino  EQ gw_saida_viagem-po_destino
               AND tp_transgenia EQ gw_saida_comboio-tp_class(2)
               AND operacao      EQ gw_saida_vinc_nf-operacao.

              IF sy-subrc NE 0.

                SELECT SINGLE *
                 FROM zlest0055
                 INTO wl_zlest0055
                 WHERE kunnr EQ wl_kna1-kunnr
                 AND matkl   EQ wl_mara-matkl
                 AND auart   EQ gw_saida_ger_ov-auart
                 AND dt_fim  GE gw_saida_ger_ov-dt_fatura
                 AND waerk   EQ lv_value
                 AND status  EQ '1'
                 AND vkorg   EQ w_bukrs
                 AND po_embarque EQ gw_saida_viagem-po_embarque
                 AND po_destino  EQ gw_saida_viagem-po_destino
                 AND operacao    EQ gw_saida_vinc_nf-operacao.

                IF sy-subrc EQ 0.
                  EXIT.
                ENDIF.

              ELSE.
                EXIT.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF wl_zlest0055 IS NOT INITIAL.

        MOVE: wl_zlest0055-vkorg TO gw_saida_ger_ov-vkorg,
              wl_zlest0055-vtweg TO gw_saida_ger_ov-vtweg,
              wl_zlest0055-spart TO gw_saida_ger_ov-spart,
              wl_zlest0055-kunnr TO gw_saida_ger_ov-kunnr,
              "WL_ZLEST0055-ZTERM TO GW_SAIDA_GER_OV-ZTERM,
              wl_zlest0059-matnr TO gw_saida_ger_ov-matnr_ov,
              wl_zlest0055-kurst TO gw_saida_ger_ov-kurst,
              wl_zlest0055-waerk_fatura TO gw_saida_ger_ov-waerk_fatura.

        PERFORM f_atrib_zterm_ov USING wl_zlest0055
                              CHANGING gw_saida_ger_ov.

        MODIFY gt_saida_ger_ov INDEX ls_good-row_id FROM gw_saida_ger_ov.

        lv_value = wl_zlest0055-netpr.
        CONDENSE lv_value NO-GAPS.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'NETPR'
            i_value     = lv_value.

        CASE wl_zlest0055-waerk.

          WHEN: 'BRL'.

            v_vlr_brl = ( ( gw_saida_ger_ov-peso_vinculado / 1000 )  * wl_zlest0055-netpr ).
            lv_value  = v_vlr_brl.
            CONDENSE lv_value NO-GAPS.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'VLR_BRL'
                i_value     = lv_value.

            CLEAR: obj_zcl_util_sd.
            CREATE OBJECT obj_zcl_util_sd.

            vl_gdatu =  gw_saida_ger_ov-dt_fatura.
            obj_zcl_util_sd->set_data( EXPORTING i_data = vl_gdatu ).
            obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = wl_zlest0055-kurst ).
            obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = wl_zlest0055-waerk ).
            obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'USD' ).
            obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = vl_ukurs ).

            IF NOT (  vl_ukurs IS INITIAL ).

              obj_zcl_util_sd->get_data_ukurs( RECEIVING e_data_ukurs = gw_saida_ger_ov-data_taxa ).

              MODIFY gt_saida_ger_ov INDEX ls_good-row_id FROM gw_saida_ger_ov.

              CLEAR: lv_value.
              lv_value = ( vl_ukurs * -1 ).
              CONDENSE lv_value NO-GAPS.
              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'TAX_DOLAR'
                  i_value     = lv_value.

              CLEAR: v_vlr_usd, lv_value.

              v_vlr_usd = ( v_vlr_brl / (  vl_ukurs * -1 ) ).

              lv_value = v_vlr_usd.
              CONDENSE lv_value NO-GAPS.
              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'VLR_USD'
                  i_value     = lv_value.

            ELSE.

              CLEAR: lv_value.

              CONDENSE lv_value NO-GAPS.
              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'TAX_DOLAR'
                  i_value     = lv_value.

              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'VLR_USD'
                  i_value     = lv_value.

              MESSAGE i000(zwrm001) DISPLAY LIKE 'E' WITH 'Taxa do câmbio não cadastrada.'.

            ENDIF.

          WHEN: 'USD'.

            v_vlr_usd = ( ( gw_saida_ger_ov-peso_vinculado / 1000 )  * wl_zlest0055-netpr ).
            lv_value  = v_vlr_usd.
            CONDENSE lv_value NO-GAPS.

            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'VLR_USD'
                i_value     = lv_value.

            CLEAR: vl_ukurs, vl_gdatu.

            CLEAR: obj_zcl_util_sd.
            CREATE OBJECT obj_zcl_util_sd.

            vl_gdatu =  gw_saida_ger_ov-dt_fatura.
            obj_zcl_util_sd->set_data( EXPORTING i_data = vl_gdatu ).
            obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = wl_zlest0055-kurst ).
            obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = wl_zlest0055-waerk ).
            obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'BRL' ).
            obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = vl_ukurs ).

            IF NOT ( vl_ukurs IS INITIAL ).

              obj_zcl_util_sd->get_data_ukurs( RECEIVING e_data_ukurs = gw_saida_ger_ov-data_taxa ).

              MODIFY gt_saida_ger_ov INDEX ls_good-row_id FROM gw_saida_ger_ov.

              lv_value = vl_ukurs.
              CONDENSE lv_value NO-GAPS.
              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'TAX_DOLAR'
                  i_value     = lv_value.

              CLEAR: v_vlr_brl.

              v_vlr_brl = v_vlr_usd * vl_ukurs.

              lv_value = v_vlr_brl.
              CONDENSE lv_value NO-GAPS.
              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'VLR_BRL'
                  i_value     = lv_value.

            ELSE.

              CLEAR: lv_value.

              CONDENSE lv_value NO-GAPS.
              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'TAX_DOLAR'
                  i_value     = lv_value.

              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'VLR_BRL'
                  i_value     = lv_value.

              MESSAGE i000(zwrm001) DISPLAY LIKE 'E' WITH 'Taxa do câmbio não cadastrada.'.

            ENDIF.

        ENDCASE.

        MODIFY gt_saida_ger_ov INDEX ls_good-row_id FROM gw_saida_ger_ov TRANSPORTING kunnr.

      ELSE.

        CLEAR: lv_value.
        CONDENSE lv_value NO-GAPS.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'NETPR'
            i_value     = lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_USD'
            i_value     = lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_BRL'
            i_value     = lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'TAX_DOLAR'
            i_value     = lv_value.

        MESSAGE i000(zwrm001) DISPLAY LIKE 'W' WITH 'Preço da moeda não cadastrado.'.

      ENDIF.


      REFRESH: tl_estilo_ov_aux.
      tl_estilo_ov[] = gw_saida_ger_ov-estilo.
      LOOP AT tl_estilo_ov INTO wl_estilo_ov.
        wl_estilo_ov_aux-fieldname = wl_estilo_ov-fieldname.
        wl_estilo_ov_aux-style     = wl_estilo_ov-style.
        APPEND wl_estilo_ov_aux TO tl_estilo_ov_aux.
      ENDLOOP.

      IF ( gw_saida_ger_ov-waerk_fatura IS NOT INITIAL ) AND
         ( gw_saida_ger_ov-waerk_fatura NE 'USD' ).
        CLEAR: wl_estilo_ov_aux.
        wl_estilo_ov_aux-fieldname = 'TAX_DOLAR'.
        wl_estilo_ov_aux-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo_ov_aux TO tl_estilo_ov_aux.
      ENDIF.

      SORT tl_estilo_ov_aux BY fieldname.
      DELETE ADJACENT DUPLICATES FROM tl_estilo_ov_aux COMPARING fieldname.

      REFRESH: tl_estilo_ov[].
      LOOP AT tl_estilo_ov_aux INTO wl_estilo_ov_aux.
        CLEAR: wl_estilo_ov.
        wl_estilo_ov-fieldname = wl_estilo_ov_aux-fieldname.
        wl_estilo_ov-style     = wl_estilo_ov_aux-style.
        APPEND wl_estilo_ov TO tl_estilo_ov.
      ENDLOOP.

      CLEAR: gw_saida_ger_ov-estilo.
      INSERT LINES OF tl_estilo_ov INTO TABLE gw_saida_ger_ov-estilo.
      MODIFY gt_saida_ger_ov INDEX ls_good-row_id FROM gw_saida_ger_ov.

      CALL METHOD obj_grid_ger_ov->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'AUART'.

    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED_OV

*  METHOD ON_DATA_CHANGED_FINISHED_OV.
*
*    DATA: LS_GOOD  TYPE LVC_S_MODI,
*          LV_VALUE TYPE LVC_VALUE,
*          VL_VALUE TYPE LVC_VALUE.
*    LOOP AT E_MODIFIED->MT_GOOD_CELLS INTO LS_GOOD WHERE FIELDNAME = 'WAERK'.
*    ENDLOOP.
*
*  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_OV


  METHOD on_f4_ov.


    TYPES: BEGIN OF t_f4_structure,
             fieldtext TYPE dfies-fieldtext,
             fieldname TYPE dfies-fieldname,
           END OF t_f4_structure.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA: ls_modi TYPE lvc_s_modi.

    READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX es_row_no-row_id.
    CASE e_fieldname.
      WHEN 'AUART'.
        TYPES : BEGIN OF ty_tvak,
                  auart TYPE tvak-auart,
                  bezei TYPE tvakt-bezei,
                END OF ty_tvak.

        DATA: wl_return_chv TYPE  ddshretval,
              wl_dselcchv   TYPE  dselc,
              tl_tvak       TYPE TABLE OF ty_tvak,
              wl_tvak       TYPE ty_tvak,
              tl_return_chv TYPE TABLE OF ddshretval,
              tl_dselcchv   TYPE TABLE OF dselc.

        SELECT auart bezei
          FROM tvakt
          INTO TABLE tl_tvak
          WHERE spras = 'P'.

        DELETE tl_tvak  WHERE auart+0(1) NE 'Z'.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'AUART'
            value_org       = 'S'
          TABLES
            value_tab       = tl_tvak
            return_tab      = tl_return_chv
            dynpfld_mapping = tl_dselcchv.

        READ TABLE tl_return_chv INTO wl_return_chv INDEX 1.
        IF sy-subrc = 0 AND wl_return_chv-fieldval <> ''.
          ASSIGN er_event_data->m_data->* TO <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'AUART'.
          ls_modi-value     = wl_return_chv-fieldval.
          APPEND ls_modi TO <itab>.

          er_event_data->m_event_handled = 'X'.
        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "ON_F4_OV

  METHOD handle_hotspot_click.

    DATA: wl_j_1bnfdoc TYPE j_1bnfdoc,
          opt          TYPE ctu_params.


    CLEAR: wl_j_1bnfdoc.

    CASE e_column_id.

      WHEN: 'DOCNUM'.

        REFRESH: tl_bdc.
        CLEAR: gw_saida_ger_ov, wl_bdc.

        READ TABLE gt_saida_ger_ov INTO DATA(gw_saida_ger_ov_tmp) INDEX e_row_id-index.
        CHECK sy-subrc EQ 0.

        SELECT SINGLE * FROM j_1bnfdoc INTO wl_j_1bnfdoc WHERE docnum EQ gw_saida_ger_ov_tmp-docnum.

        IF ( sy-subrc EQ 0 ).

          CASE wl_j_1bnfdoc-model.
            WHEN: '57'.

              SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wl_j_1bnfdoc-docnum.
              SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wl_j_1bnfdoc-bukrs.
              CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.

*              PERFORM F_PREENCHER_DYNPRO USING:
*                 'X' 'Z_1BNFE_MONITOR'       '1000',
*                 ' ' 'DOCNUM-LOW'            WL_J_1BNFDOC-DOCNUM,
*                 ' ' 'USER-LOW'              SPACE,
*                 ' ' 'DATE0-LOW'             SPACE,
*                 ' ' 'BUKRS-LOW'             WL_J_1BNFDOC-BUKRS,
*                 ' ' 'BDC_OKCODE'            'ONLI'.
*
*              OPT-DISMODE  = 'E'.
*              OPT-UPDMODE  = 'A'.
*              OPT-DEFSIZE  = 'X'.
*              OPT-RACOMMIT = 'X'.
*
*              CALL TRANSACTION 'ZCTE' USING TL_BDC OPTIONS FROM OPT.

            WHEN: '55'.

              PERFORM f_preencher_dynpro USING:
                 'X' 'Z_1BNFE_MONITOR'       '1000',
                 ' ' 'DOCNUM-LOW'            wl_j_1bnfdoc-docnum,
                 ' ' 'USER-LOW'              space,
                 ' ' 'DATE0-LOW'             space,
                 ' ' 'BUKRS-LOW'             wl_j_1bnfdoc-bukrs,
                 ' ' 'BDC_OKCODE'            'ONLI'.

              opt-dismode = 'E'.
              opt-defsize = ' '.
              opt-racommit = 'X'.

              CALL TRANSACTION 'ZCTE' USING tl_bdc OPTIONS FROM opt.

              CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
            WHEN: '01'.
              SET PARAMETER ID 'JEF' FIELD wl_j_1bnfdoc-docnum.
              CALL TRANSACTION 'J1B2N' AND SKIP FIRST SCREEN.

          ENDCASE.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION (Vinculo Viagem)
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_ro DEFINITION.
  PUBLIC SECTION.

    DATA: wl_toolbar  TYPE stb_button,
          c_separator TYPE i VALUE 3.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.


ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR_ro IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_ro IMPLEMENTATION.

  METHOD constructor.
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

*    MOVE   C_SEPARATOR TO WL_TOOLBAR-BUTN_TYPE.
*    APPEND WL_TOOLBAR  TO E_OBJECT->MT_TOOLBAR.
*
*    TY_TOOLBAR-ICON      =  ICON_DESELECT_ALL.
*    TY_TOOLBAR-FUNCTION  =  'DESMARCAR_ROMANEIOS'.
*    "TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
*    TY_TOOLBAR-TEXT      = 'Desmarcar Romaneios.'.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.


  ENDMETHOD.                    "ON_TOOLBAR

  METHOD handle_user_command.

    DATA: wl_tabix TYPE sy-tabix.

    CASE e_ucomm.
      WHEN: 'DESMARCAR_ROMANEIOS'.

        IF NOT ( gt_saida_vinc_rom_nome[] IS INITIAL ).

          CLEAR: gw_saida_vinc_rom_nome.

          LOOP AT gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome.

            wl_tabix = sy-tabix.
            IF ( gw_saida_vinc_rom_nome-check EQ 'X' ) AND ( gw_saida_vinc_rom_nome-estilo IS INITIAL ).

              CLEAR: gw_saida_vinc_rom_nome-check.
              MODIFY gt_saida_vinc_rom_nome FROM gw_saida_vinc_rom_nome INDEX wl_tabix TRANSPORTING check.

              DELETE gt_saida_vinc_nf_nome WHERE rm_codigo = wg_tela1200-rm_codigo
                                             AND nr_dco    = gw_saida_vinc_rom_nome-nr_dco
                                             AND safra     = gw_saida_vinc_rom_nome-safra.

              total_vinculado = total_vinculado - gw_saida_vinc_rom_nome-peso_vinc.

            ENDIF.
            CLEAR: wl_tabix.
          ENDLOOP.

          wa_stable-row = 'X'.
          wa_stable-col = 'X'.
          CALL METHOD obj_grid_romaneio->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

          CALL METHOD obj_grid_vinc_nf->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

          LEAVE TO SCREEN 1200.


        ENDIF.

    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND
ENDCLASS.                    "LCL_ALV_TOOLBAR_ro IMPLEMENTATION


*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION (Vinculo Viagem)
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_vn DEFINITION.
  PUBLIC SECTION.

*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_vn IMPLEMENTATION.
  METHOD constructor.


    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.


    DATA: wl_desactive.

    CLEAR: gw_setleaf.
    SELECT SINGLE * FROM setleaf
      INTO gw_setleaf
    WHERE setname EQ 'MAGGI_ZLES0077_IMPORT'
     AND valfrom EQ w_werks.

    IF gw_setleaf IS INITIAL.
      ty_toolbar-icon      =  icon_insert_multiple_lines.
      ty_toolbar-function  =  'VINCULAR_NF'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-text      = 'Vincular NF.'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    ty_toolbar-icon      =  icon_display.
    ty_toolbar-function  =  'VISUAL_NF'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-text      = 'Visualizar NF.'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    CASE e_ucomm.

      WHEN 'VINCULAR_NF'.

        DATA: tl_rows TYPE lvc_t_row,
              sl_rows TYPE lvc_s_row.

        REFRESH gt_saida_vinc_rom_nome. " tabela individual Embarcacao/Nome
        CLEAR: wg_tela1200,  wg_acao.
        DATA: wl_zlest0057 TYPE zlest0057.

        CLEAR: tl_rows, sl_rows.

        CALL METHOD obj_grid_comboio->get_selected_rows
          IMPORTING
            et_index_rows = tl_rows.

        CLEAR: gw_saida_comboio.
        READ TABLE tl_rows INDEX 1 INTO sl_rows.
        IF ( sy-subrc NE 0 ).
          READ TABLE gt_saida_comboio INDEX index_comboio INTO gw_saida_comboio.
        ELSE.
          READ TABLE gt_saida_comboio INDEX sl_rows-index INTO gw_saida_comboio.
        ENDIF.

* Ini - RJF - CS2023000946 Ajuste determinação de material OV (Aquaviário) #129189

        SELECT SINGLE * FROM zlest0059
          INTO @DATA(wl_zlest0059)
          WHERE bukrs       EQ @w_bukrs
            AND auart       EQ @gw_saida_ger_ov-auart
            AND operacao    EQ @gw_saida_vinc_nf-operacao "RJF
            AND po_embarque EQ @gw_saida_viagem-po_embarque
            AND po_destino  EQ @gw_saida_viagem-po_destino.
        IF sy-subrc EQ 0 AND wl_zlest0059-operacao IS NOT INITIAL.
          wg_tela1200-operacao   = wl_zlest0059-operacao.
        ELSE.

          SELECT SINGLE * FROM zlest0059
            INTO @wl_zlest0059
            WHERE bukrs       EQ @w_bukrs
              AND auart       EQ @gw_saida_ger_ov-auart
              AND po_embarque EQ @gw_saida_viagem-po_embarque
              AND po_destino  EQ @gw_saida_viagem-po_destino.

          wg_tela1200-operacao   = 'RF'.
        ENDIF.
* Fim - RJF - CS2023000946 Ajuste determinação de material OV (Aquaviário) #129189

        wp_bukrs         = w_bukrs.
        wp_werks         = w_werks.
        wp_ano           = w_ano.
        wp_viagem        = w_viagem.
        wp_embarcacao    = gw_saida_comboio-nome.
        wp_peso_previsto = gw_saida_comboio-peso_previsto.
        wp_matnr         = gw_saida_comboio-maktx.
        wp_tp_class      = gw_saida_comboio-tp_class.
        "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - INICIO
        CASE  gw_saida_comboio-eudr(1).
          WHEN 'S'.
            wp_eudr = 'S-Atende'.
          WHEN 'N'.
            wp_eudr = 'N-Não Atende'.
          WHEN OTHERS.
            CLEAR: wp_eudr.
        ENDCASE.
        "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - FIM
        wg_tela1200-dt_movimento = sy-datum.
        wg_tela1200-peso_avinc = gw_saida_comboio-peso_previsto.
        wg_tela1200-ex_codigo  = gw_saida_viagem-po_embarque.
        wg_tela1200-rc_codigo  = gw_saida_viagem-po_destino.
*        wg_tela1200-operacao   = 'RF'.

        SELECT s~* INTO TABLE @DATA(it_zlest0104)
          FROM zlest0104 AS s
         WHERE s~emissor EQ @w_werks
           AND EXISTS ( SELECT * FROM zlest0206 AS t WHERE t~local_descarga EQ s~local_descarga AND t~po_embarque EQ @gw_saida_viagem-po_embarque ).

        DESCRIBE TABLE it_zlest0104 LINES wg_tela1200-nm_qtd_local.

        READ TABLE it_zlest0104 INDEX 1 INTO DATA(wa_zlest0104).
        wg_tela1200-id_local_descarga = wa_zlest0104-local_descarga.
        wg_tela1200-ds_local_descarga = wa_zlest0104-descricao.

        CLEAR: vg_sugere_dest.

        CALL SCREEN 1200 STARTING AT 010 2 ENDING   AT 180 30.


      WHEN 'VISUAL_NF'.

        DATA: tl_estilo_romaneio TYPE lvc_t_styl,
              wl_estilo_romaneio TYPE lvc_s_styl.

        DATA: tl_zlest0060 TYPE TABLE OF zlest0060,
              wl_zlest0060 TYPE zlest0060.


        REFRESH: tl_zlest0060.
        CLEAR: wl_zlest0060.

        REFRESH: tl_rows.
        CLEAR: sl_rows.

        CALL METHOD obj_grid_vinc_nf->get_selected_rows
          IMPORTING
            et_index_rows = tl_rows.

        READ TABLE tl_rows INDEX 1 INTO sl_rows.

        IF ( sy-subrc EQ 0 ).

          w_cont_geral     = 0.
          wp_bukrs         = w_bukrs.
          wp_werks         = w_werks.
          wp_ano           = w_ano.
          wp_viagem        = w_viagem.
          wg_tela1200-ex_codigo  = gw_saida_viagem-po_embarque.
          wg_tela1200-rc_codigo  = gw_saida_viagem-po_destino.
          wg_tela1200-series     = ''.
          wg_tela1200-nr_safra   = ''.

          READ TABLE gt_saida_vinc_nf_nome INDEX sl_rows-index INTO gw_saida_vinc_nf_nome.

          CLEAR: tl_rows, sl_rows.

          CALL METHOD obj_grid_comboio->get_selected_rows
            IMPORTING
              et_index_rows = tl_rows.

          READ TABLE tl_rows INDEX 1 INTO sl_rows.
          IF ( sy-subrc NE 0 ).
            sl_rows-index = index_comboio.
            READ TABLE gt_saida_comboio INDEX sl_rows-index INTO gw_saida_comboio.
          ELSE.
            READ TABLE gt_saida_comboio INDEX sl_rows-index INTO gw_saida_comboio.
          ENDIF.


          wp_peso_previsto = gw_saida_comboio-peso_previsto.
          wp_matnr         = gw_saida_comboio-maktx.
          wp_tp_class      = gw_saida_comboio-tp_class.
          wp_frete_id      = gw_saida_vinc_nf_nome-id_frete_aqua.
          wg_tela1200-id_frete_aqua = gw_saida_vinc_nf_nome-id_frete_aqua.

          REFRESH: tl_zlest0060, gt_saida_vinc_rom_nome.
          SELECT * FROM zlest0060
            INTO TABLE tl_zlest0060
           WHERE bukrs       EQ w_bukrs
             AND werks       EQ w_werks
             AND ano_viagem  EQ w_ano
             AND nr_viagem   EQ w_viagem
             AND nr_dco      EQ gw_saida_vinc_nf_nome-nr_dco
             AND safra       EQ gw_saida_vinc_nf_nome-safra
             AND rm_codigo   EQ gw_saida_vinc_nf_nome-rm_codigo
             "AND cl_codigo   EQ gw_saida_vinc_nf_nome-cl_codigo
             AND nome_emb      EQ gw_saida_vinc_nf_nome-nome
             AND operacao      EQ gw_saida_vinc_nf_nome-operacao
             AND id_frete_aqua EQ gw_saida_vinc_nf_nome-id_frete_aqua.

          IF ( sy-subrc EQ 0 ).

            CLEAR: wl_zlest0060, gw_saida_vinc_rom_nome, total_vinculado.

            CLEAR: wg_tela1200-id_local_descarga,
                   wg_tela1200-ds_local_descarga.

            IF gw_saida_vinc_nf_nome-local_descarga IS NOT INITIAL.
              SELECT SINGLE s~* INTO @wa_zlest0104
                FROM zlest0104 AS s
               WHERE local_descarga EQ @gw_saida_vinc_nf_nome-local_descarga.

              IF sy-subrc IS INITIAL.
                wg_tela1200-id_local_descarga = wa_zlest0104-local_descarga.
                wg_tela1200-ds_local_descarga = wa_zlest0104-descricao.
              ENDIF.
            ENDIF.

            LOOP AT tl_zlest0060 INTO wl_zlest0060 .

              CLEAR: wl_estilo_romaneio.
              REFRESH: tl_estilo_romaneio.

              wp_embarcacao              = wl_zlest0060-nome_emb.
              wg_tela1200-rm_codigo      = wl_zlest0060-rm_codigo.
              wg_tela1200-dt_codigo      = wl_zlest0060-dt_codigo.
              wg_tela1200-tomador_cod    = wl_zlest0060-tomador_serv.
              wg_tela1200-dt_movimento   = wl_zlest0060-dt_movimento.
              wg_tela1200-operacao       = wl_zlest0060-operacao.
              wg_tela1200-id_frete_aqua  = wl_zlest0060-id_frete_aqua.
              wp_frete_id                = wl_zlest0060-id_frete_aqua.
              total_vinculado            = gw_saida_vinc_nf_nome-peso_vinculado.

              gw_saida_vinc_rom_nome-id_frete_aqua_nf = wl_zlest0060-id_frete_aqua_nf.
              gw_saida_vinc_rom_nome-id_frete_aqua    = wl_zlest0060-id_frete_aqua.
              gw_saida_vinc_rom_nome-embarcacao       = wl_zlest0060-embarcacao.
              gw_saida_vinc_rom_nome-nome             = wl_zlest0060-nome_emb.
              gw_saida_vinc_rom_nome-nr_romaneio      = wl_zlest0060-nr_romaneio.
              gw_saida_vinc_rom_nome-dt_movimento = wl_zlest0060-dt_movimento.
              gw_saida_vinc_rom_nome-docdat       = wl_zlest0060-docdat.
              gw_saida_vinc_rom_nome-nfnum        = wl_zlest0060-nfnum.
              gw_saida_vinc_rom_nome-series       = wl_zlest0060-series.
              gw_saida_vinc_rom_nome-peso_fiscal  = wl_zlest0060-peso_fiscal.
              gw_saida_vinc_rom_nome-netwr        = wl_zlest0060-netwr.
              gw_saida_vinc_rom_nome-peso_subtotal = wl_zlest0060-peso_subtotal.
              gw_saida_vinc_rom_nome-peso_liq_ret  = wl_zlest0060-peso_liq_ret.
              gw_saida_vinc_rom_nome-rm_codigo    = wl_zlest0060-rm_codigo.
              gw_saida_vinc_rom_nome-dt_codigo    = wl_zlest0060-dt_codigo.
              gw_saida_vinc_rom_nome-tomador_serv = wl_zlest0060-tomador_serv.
              gw_saida_vinc_rom_nome-nr_dco       = wl_zlest0060-nr_dco.
              gw_saida_vinc_rom_nome-safra        = wl_zlest0060-safra.
              gw_saida_vinc_rom_nome-operacao     = wl_zlest0060-operacao.

              IF ( wl_zlest0060-peso_liq_ret IS NOT INITIAL ).
                gw_saida_vinc_rom_nome-peso_util_vinc   = wl_zlest0060-peso_liq_ret.
                gw_saida_vinc_rom_nome-valor_util_vinc  = wl_zlest0060-vlr_liq_ret.
              ELSE.
                gw_saida_vinc_rom_nome-peso_util_vinc   = wl_zlest0060-peso_fiscal.
                gw_saida_vinc_rom_nome-valor_util_vinc  = wl_zlest0060-netwr.
              ENDIF.

              wl_estilo_romaneio-fieldname = 'CHECK'.
              wl_estilo_romaneio-style     = cl_gui_alv_grid=>mc_style_disabled.
              APPEND wl_estilo_romaneio TO tl_estilo_romaneio.
              INSERT LINES OF tl_estilo_romaneio INTO TABLE gw_saida_vinc_rom_nome-estilo.

              APPEND gw_saida_vinc_rom_nome TO gt_saida_vinc_rom_nome.

              CLEAR: gw_saida_vinc_rom_nome .
            ENDLOOP.
          ENDIF.

          wg_acao = 'VIS_ROMANEIO'.

          CLEAR: vg_sugere_dest.

          CALL SCREEN 1200 STARTING AT 010 2 ENDING   AT 180 30.

        ELSE.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Selecionar uma linha vinculada.'.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

**---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION (Faturamento)
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_ov DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar_ov IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: wl_desactive.

*    TY_TOOLBAR-ICON      =  ICON_REFRESH.
*    TY_TOOLBAR-FUNCTION  =  'ATUALIZA'.
*    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
*    "TY_TOOLBAR-TEXT      = 'Recarrega/Atualiza Notas'.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

    ty_toolbar-icon      =  icon_transport.
    ty_toolbar-function  =  'GERAR_OV'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-text      = 'Gerar O.V.'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*    TY_TOOLBAR-ICON      =  ICON_LIST.
*    TY_TOOLBAR-FUNCTION  =  'RELA_NF'.
*    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
*    TY_TOOLBAR-TEXT      = 'NF.Vinculadas'.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

    ty_toolbar-icon      =  icon_storno.
    ty_toolbar-function  =  'ESTORNO_DOC'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-text      = 'Estornar'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: wa_headerdata       TYPE  bapisdhd1,

          wa_itemdata         TYPE  bapisditm,
          wa_condition        TYPE  bapicond,
          wa_partner          TYPE  bapiparnr,
          wa_return           TYPE  bapiret2,
          tabix               TYPE  sy-tabix,
          vbfa_vbeln          TYPE  vbfa-vbeln,
          vdocnum             TYPE  j_1bnflin-docnum,
          nnota               TYPE  j_1bnfdoc-nfenum,
          wa_j_1bnflin        TYPE  j_1bnflin,
          wa_j_1bnfdoc        TYPE  j_1bnfdoc,
          tl_input_zlest0061  TYPE TABLE OF zlest0061 , " Ordem de Venda - Frete Aquaviario
          wl_input_zlest0061  TYPE          zlest0061 , " Ordem de Venda - Frete Aquaviario
          gt_saida_ger_ov_aux TYPE TABLE OF zaqty_saida_ger_ov, "Estrutura de Saida do ALV Faturamento
          gw_saida_ger_ov_aux TYPE          zaqty_saida_ger_ov, "Estrutura de Saida do ALV Faturamento
          vauart              TYPE          zaqty_saida_ger_ov-auart,
          lt_zlest0055        TYPE TABLE OF zlest0055,
          wl_zlest0055        TYPE zlest0055,
          w_linha(6),
          msg_erro(50),
          werks_fat           TYPE werks_d,
          wl_zsdt0001         TYPE zsdt0001.

    CASE e_ucomm.

      WHEN 'ATUALIZA'.
        PERFORM carregar_fatura.
      WHEN: 'ESTORNO_DOC'.
        PERFORM: estornar_documentos.
      WHEN 'GERAR_OV'.

        DATA: tl_rows        TYPE lvc_t_row,
              sl_rows        TYPE lvc_s_row,
              qtd_s_ov       TYPE sy-tabix,
              qtd_l_ov       TYPE sy-tabix,
              wl_j_1bnflin   TYPE j_1bnflin,
              vbeln_ov       TYPE bapivbeln-vbeln,
              vbeln_fatura   TYPE c LENGTH 10,
              wl_header_inx2 TYPE  bapisdh1x,
              wl_fieldname   TYPE rmdi_name,
              wl_text        TYPE rmdi_ddtxt,
              wl_lfa1        TYPE lfa1,
              wl_kna1        TYPE kna1,
              wl_knvv        TYPE knvv,
              "*-CS2025000025-#164218-27.01.2025-JT-inicio
              lt_zsdt0008    TYPE TABLE OF zsdt0008,
              wl_zsdt0008    TYPE zsdt0008,
*              lc_dados       TYPE zsde0183,
*              lc_retorno     TYPE zsdt0370_t,
*              wc_retorno     TYPE zsdt0370,
              "*-CS2025000025-#164218-27.01.2025-JT-Fim

              lt_j_1btxsdc   TYPE TABLE OF j_1btxsdc,
              wl_j_1btxsdc   TYPE j_1btxsdc,
              lt_j_1bsdica   TYPE TABLE OF j_1bsdica,
              wl_j_1bsdica   TYPE j_1bsdica,

              wl_j_1btxiss   TYPE j_1btxiss,
              wl_j_1btxic1   TYPE j_1btxic1,
              wl_j_1btxpis   TYPE j_1btxpis,
              wl_j_1btxcof   TYPE j_1btxcof,
              wl_zlest0030   TYPE zlest0030.

        DATA: vl_lifnr        TYPE lfa1-lifnr,
              vlr_icms        TYPE zlest0061-vlr_usd,
              vlr_pis         TYPE zlest0061-vlr_usd,
              vlr_cofins      TYPE zlest0061-vlr_usd,
              vlr_iss         TYPE zlest0061-vlr_usd,
              vlr_liquido     TYPE zlest0061-vlr_usd,
* SKM - RIM - IR098248 - INICIO
              vlr_icms1       TYPE kurrf,
              vlr_pis1        TYPE kurrf,
              vlr_cofins1     TYPE kurrf,
              vlr_iss1        TYPE kurrf,
              vlr_pis_confins TYPE kurrf,
              vlr_liquido1    TYPE zlest0061-vlr_usd,
              vlr_base1       TYPE zlest0061-vlr_usd,
              vlr_calc1       TYPE kurrf,
              vlr_calc2       TYPE kurrf,
              vlr_calc3       TYPE kurrf,
              vlr_calc4       TYPE kurrf,
* SKM - RIM - IR098248 - FIM
              vl_validto      TYPE j_1btxiss-validto,
              vl_validfrom    TYPE j_1btxiss-validfrom,
              v_msg_aux       TYPE string,
              vl_data         TYPE c LENGTH 10.

        DATA: tl_texto   TYPE catsxt_longtext_itab,
              wl_texto   TYPE LINE OF catsxt_longtext_itab,
              tl_text_in TYPE TABLE OF bapisdtext,
              wl_text_in TYPE bapisdtext.


        " Verifica Seleção de Linhas
        CALL METHOD obj_grid_ger_ov->get_selected_rows
          IMPORTING
            et_index_rows = tl_rows.

        DESCRIBE TABLE tl_rows LINES qtd_s_ov.

        CASE qtd_s_ov.

          WHEN: 1.

            werks_fat = w_werks.
            SELECT SINGLE * FROM zlest0104 INTO @DATA(_wl_0104_tmp) WHERE emissor = @w_werks.
            IF ( sy-subrc = 0 ) AND ( _wl_0104_tmp-centro_fat IS NOT INITIAL ).
              werks_fat = _wl_0104_tmp-centro_fat.
            ENDIF.

            READ TABLE gt_saida_ger_ov INTO gw_saida_ger_ov INDEX 1.
            vauart = gw_saida_ger_ov-auart.
            CLEAR: gw_saida_ger_ov.
            READ TABLE gt_saida_ger_ov INTO gw_saida_ger_ov INDEX 2.

            IF ( vauart EQ gw_saida_ger_ov-auart ).
              MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Tipo de O.V iguais,' ' não é possivel a geração da O.V.'.
            ELSE.

              CLEAR: gw_saida_ger_ov.
              READ TABLE tl_rows INTO sl_rows INDEX 1.
              READ TABLE gt_saida_ger_ov INTO gw_saida_ger_ov INDEX sl_rows-index.

              IF gw_saida_ger_ov-waerk_fatura IS INITIAL. " 12.12.16
                MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Moeda Fatura não encontrada!'.
                RETURN.
              ENDIF.

              CLEAR : wl_header_in,wl_header_inx,wl_items_inx,wa_itemdata,wa_condition,wa_partner,it_return,tl_bapiparex,wl_schedules_in.
              REFRESH: it_itemdata,it_condition,it_partner,it_return,it_items_inx,tl_schedules_in.

              "*-CS2025000025-#164218-27.01.2025-JT-inicio
*             REFRESH: lt_zlest0055[], lc_retorno, "lt_zsdt0008[],
              REFRESH: lt_zlest0055[], lt_zsdt0008[],
              "*-CS2025000025-#164218-27.01.2025-JT-Fim
                       lt_j_1btxsdc[], lt_j_1bsdica[], lt_j_1btxsdc[].
              CLEAR: wl_j_1btxic1, wl_j_1btxpis, wl_j_1btxiss, wl_j_1btxcof.

              CLEAR: gw_saida_comboio, gw_saida_vinc_nf_nome.


              CLEAR: tl_rows, sl_rows.

              CALL METHOD obj_grid_comboio->get_selected_rows
                IMPORTING
                  et_index_rows = tl_rows.

              READ TABLE tl_rows INDEX 1 INTO sl_rows.
              IF ( sy-subrc NE 0 ).
                sl_rows-index = index_comboio.
                READ TABLE gt_saida_comboio INDEX sl_rows-index INTO gw_saida_comboio.
              ELSE.
                READ TABLE gt_saida_comboio INDEX sl_rows-index INTO gw_saida_comboio.
              ENDIF.


              CLEAR: tl_rows, sl_rows.

              CALL METHOD obj_grid_vinc_nf->get_selected_rows
                IMPORTING
                  et_index_rows = tl_rows.

              READ TABLE tl_rows INDEX 1 INTO sl_rows.
              READ TABLE gt_saida_vinc_nf_nome INTO gw_saida_vinc_nf_nome INDEX sl_rows-index.


              IF gw_saida_vinc_nf_nome-id_frete_aqua IS NOT INITIAL.
                SELECT SINGLE *
                  FROM zlest0061 INTO @DATA(wl_0061)
                 WHERE id_frete_aqua EQ @gw_saida_vinc_nf_nome-id_frete_aqua.
              ELSE.

                "Verificar se já foi gerara uma OV. de faturamento.......
                SELECT SINGLE *
                  FROM zlest0061 INTO @wl_0061
                 WHERE bukrs           EQ @w_bukrs
                   AND werks           EQ @w_werks
                   AND ano_viagem      EQ @w_ano
                   AND nr_viagem       EQ @w_viagem
                   AND embarcacao      EQ @gw_saida_vinc_nf_nome-embarcacao
                   AND nome_emb        EQ @gw_saida_vinc_nf_nome-nome
                   AND rm_codigo       EQ @gw_saida_vinc_nf_nome-rm_codigo
                   AND nr_dco          EQ @gw_saida_vinc_nf_nome-nr_dco
                   AND safra           EQ @gw_saida_vinc_nf_nome-safra
                   AND operacao        EQ @gw_saida_vinc_nf_nome-operacao.


                IF sy-subrc NE 0.
                  SELECT SINGLE *
                    FROM zlest0061 INTO wl_0061
                   WHERE bukrs           EQ w_bukrs
                     AND werks           EQ w_werks
                     AND ano_viagem      EQ w_ano
                     AND nr_viagem       EQ w_viagem
                     AND embarcacao      EQ gw_saida_vinc_nf_nome-embarcacao
                     AND nome_emb        EQ gw_saida_vinc_nf_nome-nome
                     AND cl_codigo       EQ gw_saida_vinc_nf_nome-cl_codigo
                     AND rm_codigo       EQ ''
                     AND nr_dco          EQ gw_saida_vinc_nf_nome-nr_dco
                     AND safra           EQ gw_saida_vinc_nf_nome-safra
                     AND operacao        EQ gw_saida_vinc_nf_nome-operacao.
                ENDIF.
              ENDIF.

              IF sy-subrc = 0.
                MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'O.V de faturamento já gerada!' 'Atualizar dados da tela!'.
                RETURN.
              ENDIF.

              "Calculo do Imposto - INICIO
              CLEAR: lt_zlest0055[].
              SELECT * FROM zlest0055
                INTO TABLE lt_zlest0055
                WHERE kunnr  EQ gw_saida_ger_ov-kunnr
                  AND auart  EQ gw_saida_ger_ov-auart
                  AND matkl  EQ gw_saida_comboio-matkl
                  AND dt_fim >= gw_saida_ger_ov-dt_fatura
                  AND status EQ 1
                  AND vkorg  EQ w_bukrs
                  AND po_embarque EQ gw_saida_viagem-po_embarque
                  AND po_destino  EQ gw_saida_viagem-po_destino
                  AND tp_transgenia EQ gw_saida_comboio-tp_class(2)
                  AND operacao      EQ gw_saida_vinc_nf_nome-operacao.

              IF lt_zlest0055[] IS INITIAL.
                SELECT * FROM zlest0055
                  INTO TABLE lt_zlest0055
                  WHERE kunnr  EQ gw_saida_ger_ov-kunnr
                    AND auart  EQ gw_saida_ger_ov-auart
                    AND matkl  EQ gw_saida_comboio-matkl
                    AND dt_fim >= gw_saida_ger_ov-dt_fatura
                    AND status EQ 1
                    AND vkorg  EQ w_bukrs
                    AND po_embarque EQ gw_saida_viagem-po_embarque
                    AND po_destino  EQ gw_saida_viagem-po_destino
                    AND operacao    EQ gw_saida_vinc_nf_nome-operacao.
              ENDIF.

              IF  lt_zlest0055[] IS INITIAL..
                MESSAGE 'Nenhum cadastro de preço encontrado na transação ZLES0075' TYPE 'I'.
                RETURN.
              ENDIF.

              READ TABLE lt_zlest0055 INTO wl_zlest0055 INDEX 1.
              CLEAR: wl_lfa1, wl_kna1.

              SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ gw_saida_viagem-po_embarque.
              SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ gw_saida_viagem-po_destino.


              "T1 - Remetente
              wa_partner-partn_role  = 'T1'.
              wa_partner-partn_numb  = gw_saida_vinc_nf_nome-rm_codigo.
              APPEND wa_partner TO it_partner.

              "T2 - Expedidor
              wa_partner-partn_role  = 'T2'.
              wa_partner-partn_numb  = gw_saida_viagem-po_embarque.
              APPEND wa_partner TO it_partner.

              "T3 - Receberdor
              wa_partner-partn_role  = 'T3'.
              wa_partner-partn_numb  = gw_saida_viagem-po_destino.
              APPEND wa_partner TO it_partner.

              "T4 - Destinatário do CTe
              wa_partner-partn_role  = 'T4'.
              wa_partner-partn_numb  = gw_saida_vinc_nf_nome-dt_codigo.
              APPEND wa_partner TO it_partner.

              "*-CS2025000025-#164218-27.01.2025-JT-inicio
              REFRESH: lt_zsdt0008.
              "REFRESH: lc_retorno.
              "*-CS2025000025-#164218-27.01.2025-JT-Fim

*-CS2025000025-#164218-27.01.2025-JT-inicio
              SELECT * FROM zsdt0008
                INTO TABLE lt_zsdt0008
              WHERE auart      EQ gw_saida_ger_ov-auart
                AND vkaus      EQ wl_zlest0055-vkaus
                AND uf_centro  EQ wl_lfa1-regio
                AND uf_cliente EQ wl_kna1-regio
                AND mwsk1      EQ 'SD'
                AND ownpr      NE 'X'.

*              lc_dados-auart-valor      = gw_saida_ger_ov-auart.
*              lc_dados-vkaus-valor      = wl_zlest0055-vkaus.
*              lc_dados-mwsk1-valor      = 'SD'.
*              lc_dados-uf_centro-valor  = wl_lfa1-regio.
*              lc_dados-uf_cliente-valor = wl_kna1-regio.
*              lc_dados-ownpr-regra      = 'NE'.
*              lc_dados-ownpr-valor      = 'X'.
*              lc_dados-bukrs_emit-valor = w_bukrs.
*              lc_dados-kunnr-valor      = gw_saida_ger_ov-kunnr.
*              lc_dados-werks-valor      = w_werks.
*              lc_dados-matnr-valor      = gw_saida_comboio-matnr.
*
*              lc_retorno = zcl_impostos=>get_tax_imposto( i_dados = lc_dados i_todos = abap_true ).
*
*              READ TABLE lc_retorno INTO wc_retorno INDEX 1.
*-CS2025000025-#164218-27.01.2025-JT-fim

              "*-CS2025000025-#164218-27.01.2025-JT-inicio
              IF ( lt_zsdt0008[] IS INITIAL ).
*            IF ( lc_retorno[] IS INITIAL ).
                "*-CS2025000025-#164218-27.01.2025-JT-Fim
                v_msg_aux = 'Não encontrado parâmetro ZSDT0011!'.
                v_msg_aux = v_msg_aux && | Tp.OV.: { gw_saida_ger_ov-auart }|.
                v_msg_aux = v_msg_aux && | / Utilização:  { wl_zlest0055-vkaus } |.
                v_msg_aux = v_msg_aux && | / UF Emissor:  { wl_lfa1-regio } |.
                v_msg_aux = v_msg_aux && | / UF Receptor: { wl_kna1-regio } |.
                v_msg_aux = v_msg_aux && | / Tp.IVA: SD |.
                v_msg_aux = v_msg_aux && | / Produção: "Desabilitada" ! |.

                MESSAGE v_msg_aux TYPE 'I'.
                RETURN.
              ENDIF.

              "*-CS2025000025-#164218-27.01.2025-JT-inicio
              IF ( lt_zsdt0008[] IS NOT INITIAL ).
*            IF ( lc_retorno[] IS NOT INITIAL ).
                "*-CS2025000025-#164218-27.01.2025-JT-Fim


                "*-CS2025000025-#164218-27.01.2025-JT-inicio
                SELECT * FROM j_1btxsdc
                  INTO TABLE lt_j_1btxsdc
                  FOR ALL ENTRIES IN lt_zsdt0008
                WHERE taxcode   EQ lt_zsdt0008-j_1btxsdc
                  AND custusage EQ 1.

*                SELECT * FROM j_1btxsdc
*                  INTO TABLE lt_j_1btxsdc
*                  FOR ALL ENTRIES IN lc_retorno
*                WHERE taxcode   EQ lc_retorno-j_1btxsdc
*                  AND custusage EQ 1.
                "*-CS2025000025-#164218-27.01.2025-JT-Fim

                IF ( sy-subrc EQ 0 ).

** SKM - RIM - IR098248 - INICIO
*                    DATA vlr_set_uname TYPE setleaf.
*                    SELECT *
*                      FROM setleaf
*                      INTO vlr_set_uname UP TO 1 ROWS
*                     WHERE setname = 'ZLESR0073_NP_USER'
*                     AND   valfrom = sy-uname.
*                    ENDSELECT.
** SKM - RIM - IR098248 - FIM

                  LOOP AT lt_j_1btxsdc INTO wl_j_1btxsdc.

                    CLEAR:vlr_icms, vlr_pis, vlr_cofins.

                    "ICMS
                    IF ( wl_j_1btxsdc-icms EQ 'X' ).

                      SELECT SINGLE * FROM j_1btxic1
                        INTO wl_j_1btxic1
                        WHERE land1    EQ 'BR'
                          AND shipfrom EQ wl_lfa1-regio
                          AND shipto   EQ wl_kna1-regio.

                      CASE gw_saida_ger_ov-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
                        WHEN: 'BRL'.
                          vlr_icms   = (  gw_saida_ger_ov-vlr_brl * wl_j_1btxic1-rate ) / 100.
                        WHEN: 'USD'.
                          vlr_icms    = ( gw_saida_ger_ov-vlr_usd * wl_j_1btxic1-rate ) / 100.
                      ENDCASE.

                    ENDIF.

                    "PIS
                    IF ( wl_j_1btxsdc-pis EQ 'X' ).

                      SELECT SINGLE * FROM j_1btxpis
                        INTO wl_j_1btxpis
                        WHERE country   EQ 'BR'
                          AND gruop     EQ '72'
                          AND value     EQ  gw_saida_vinc_nf_nome-werks
                          AND validto   <=  gw_saida_ger_ov-dt_fatura
                          AND validfrom >=  gw_saida_ger_ov-dt_fatura.

                      CASE gw_saida_ger_ov-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
                        WHEN: 'BRL'.
                          vlr_pis     = ( gw_saida_ger_ov-vlr_brl * wl_j_1btxpis-rate ) / 100.
                        WHEN: 'USD'.
                          vlr_pis     = ( gw_saida_ger_ov-vlr_usd * wl_j_1btxpis-rate ) / 100.
                      ENDCASE.
                    ENDIF.

                    "Cofins
                    IF ( wl_j_1btxsdc-cofins EQ 'X' ).

                      SELECT SINGLE * FROM j_1btxcof
                        INTO wl_j_1btxcof
                        WHERE country EQ 'BR'
                          AND gruop   EQ '71'
                          AND value   EQ   gw_saida_vinc_nf_nome-werks
                          AND validto   <= gw_saida_ger_ov-dt_fatura
                          AND validfrom >= gw_saida_ger_ov-dt_fatura.

                      CASE gw_saida_ger_ov-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
                        WHEN: 'BRL'.
                          vlr_cofins  = ( gw_saida_ger_ov-vlr_brl * wl_j_1btxcof-rate ) / 100.
                        WHEN: 'USD'.
                          vlr_cofins  = ( gw_saida_ger_ov-vlr_usd * wl_j_1btxcof-rate ) / 100.
                      ENDCASE.

                    ENDIF.

                    CASE gw_saida_ger_ov-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
                      WHEN: 'BRL'.
                        vlr_liquido = gw_saida_ger_ov-vlr_brl - vlr_pis - vlr_cofins - vlr_icms.
                      WHEN: 'USD'.
                        vlr_liquido = gw_saida_ger_ov-vlr_usd - vlr_pis - vlr_cofins - vlr_icms.
                    ENDCASE.
* SKM - RIM - IR098248 - INICIO
*                    IF NOT vlr_set_uname IS INITIAL.
                    IF ( wl_j_1btxsdc-icms EQ 'X' ) AND ( wl_j_1btxsdc-pis EQ 'X' ) AND ( wl_j_1btxsdc-cofins EQ 'X' ).
                      vlr_icms1   = ( wl_j_1btxic1-rate / 100 ) / ( 1 - ( wl_j_1btxic1-rate / 100 ) ).
                      vlr_cofins1 = wl_j_1btxcof-rate / 100.
                      vlr_pis1    = wl_j_1btxpis-rate / 100.
                      vlr_calc1 = 1 - vlr_cofins1 - vlr_pis1.
                      vlr_calc2 =  vlr_icms1 + vlr_cofins1 + vlr_pis1.
                      vlr_calc3 = ( vlr_calc2 / vlr_calc1 ) + 1.
                      CASE gw_saida_ger_ov-waerk_fatura.
                        WHEN: 'BRL'.
                          vlr_liquido = gw_saida_ger_ov-vlr_brl / vlr_calc3.
                        WHEN: 'USD'.
                          vlr_liquido = gw_saida_ger_ov-vlr_usd / vlr_calc3.
                      ENDCASE.
                    ENDIF.
*                    endif.
* SKM - RIM - IR098248 FIM
                  ENDLOOP.
                ENDIF.

              ELSE.

                REFRESH: lt_j_1bsdica, lt_j_1btxsdc.

                SELECT * FROM j_1bsdica
                  INTO TABLE lt_j_1bsdica
                 WHERE auart EQ gw_saida_ger_ov-auart.

                SELECT * FROM j_1btxsdc
                  INTO TABLE lt_j_1btxsdc
                  FOR ALL ENTRIES IN lt_j_1bsdica
                WHERE taxcode   EQ lt_j_1bsdica-txsdc
                  AND custusage EQ 1.

                IF ( sy-subrc EQ 0 ).

                  CLEAR: wl_j_1btxsdc, wl_j_1btxiss, wl_j_1btxpis, wl_j_1btxcof, vl_data, vl_validto, vl_validfrom.

                  CONCATENATE gw_saida_ger_ov-dt_fatura+6(2) '.'  gw_saida_ger_ov-dt_fatura+4(2) '.' gw_saida_ger_ov-dt_fatura(4) INTO vl_data.

                  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
                    EXPORTING
                      input  = vl_data
                    IMPORTING
                      output = vl_validto.

                  vl_validfrom  = vl_validto.


                  LOOP AT lt_j_1btxsdc INTO wl_j_1btxsdc.

                    IF ( wl_j_1btxsdc-iss EQ 'X' ).

                      SELECT SINGLE * FROM j_1btxiss
                        INTO wl_j_1btxiss
                       WHERE country    EQ 'BR'
                         AND gruop      EQ '73'
                         AND taxjurcode EQ wl_kna1-txjcd
                         AND value      EQ wl_lfa1-txjcd
                         AND validto   <=   vl_validto
                         AND validfrom >=   vl_validfrom.

                      CASE gw_saida_ger_ov-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
                        WHEN: 'BRL'.
                          vlr_iss   = (  gw_saida_ger_ov-vlr_brl * wl_j_1btxiss-rate ) / 100.
                        WHEN: 'USD'.
                          vlr_iss    = ( gw_saida_ger_ov-vlr_usd * wl_j_1btxiss-rate ) / 100.
                      ENDCASE.

                    ENDIF.

                    IF ( wl_j_1btxsdc-pis EQ 'X' ).
                      SELECT SINGLE * FROM j_1btxpis
                        INTO wl_j_1btxpis
                        WHERE country   EQ 'BR'
                          AND gruop     EQ '72'
                          AND value     EQ  gw_saida_vinc_nf_nome-werks
                         AND validto   <=   vl_validto
                         AND validfrom >=   vl_validfrom.

                      CASE gw_saida_ger_ov-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
                        WHEN: 'BRL'.
                          vlr_pis     = ( gw_saida_ger_ov-vlr_brl * wl_j_1btxpis-rate ) / 100.
                        WHEN: 'USD'.
                          vlr_pis     = ( gw_saida_ger_ov-vlr_usd * wl_j_1btxpis-rate ) / 100.
                      ENDCASE.


                    ENDIF.

                    IF ( wl_j_1btxsdc-cofins EQ 'X' ).
                      SELECT SINGLE * FROM j_1btxcof
                        INTO wl_j_1btxcof
                        WHERE country EQ 'BR'
                          AND gruop   EQ '71'
                          AND value   EQ   gw_saida_vinc_nf_nome-werks
                         AND validto   <=   vl_validto
                         AND validfrom >=   vl_validfrom.

                      CASE gw_saida_ger_ov-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
                        WHEN: 'BRL'.
                          vlr_cofins  = ( gw_saida_ger_ov-vlr_brl * wl_j_1btxcof-rate ) / 100.
                        WHEN: 'USD'.
                          vlr_cofins  = ( gw_saida_ger_ov-vlr_usd * wl_j_1btxcof-rate ) / 100.
                      ENDCASE.
                    ENDIF.
                  ENDLOOP.

                  CASE gw_saida_ger_ov-waerk_fatura. "gw_saida_ger_ov-waerk 12.12.16
                    WHEN: 'BRL'.
                      vlr_liquido = gw_saida_ger_ov-vlr_brl - vlr_iss - vlr_pis - vlr_cofins.
                    WHEN: 'USD'.
                      vlr_liquido = gw_saida_ger_ov-vlr_usd - vlr_iss - vlr_pis - vlr_cofins.
                  ENDCASE.
                ENDIF.
              ENDIF.
              "Calculo do Imposto - FIM

              "Cabeçalho BAPI
              wl_header_in-sales_org  = gw_saida_ger_ov-vkorg. "(org. de venda)
              wl_header_in-distr_chan = gw_saida_ger_ov-vtweg. "(canal distr.)
              wl_header_in-currency   = gw_saida_ger_ov-waerk_fatura. "(moeda.) " gw_saida_ger_ov-waerk 12.12.16
              wl_header_in-pymt_meth  = 'P'.
              wl_header_in-division   = gw_saida_ger_ov-spart. "(setor atividade)
              wl_header_in-doc_type   = gw_saida_ger_ov-auart. "(tipo de ordem)
              wl_header_in-pmnttrms   = gw_saida_ger_ov-zterm. "ZTERM.
              wl_header_in-exrate_fi  = gw_saida_ger_ov-tax_dolar."(taxa dolar)
              wl_header_in-bill_date  = sy-datum.

              CONCATENATE 'VIAGEM:' w_viagem '–' 'ANO:' w_ano INTO wl_header_in-purch_no_c SEPARATED BY space.
              wl_header_in-fix_val_dy =  gw_saida_ger_ov-dt_fatura. "VALDT Data efetiva fixa
              wl_header_in-pymt_meth  =  ''. "ZLSCH. Forma de pagamento

              wl_header_in-dlvschduse =  wl_zlest0055-vkaus. "VKAUS. Código de utilização

              "Item BAPI
              wa_itemdata-itm_number  = 10.                                    "(item)
              wa_itemdata-material    = gw_saida_ger_ov-matnr_ov.              "(código material p/ OV)
              wa_itemdata-plant       = werks_fat.                             "(centro emissor).
              wa_itemdata-target_qty  = 1.                                     "(quant)
              wa_itemdata-target_qu   = 'UN'.                                  "(unidade medida)
              wa_itemdata-sales_unit  = 'UN'.                                  "(unidade medida)
              wa_itemdata-gross_wght  = gw_saida_ger_ov-peso_vinculado.        "(quantidade)
              wa_itemdata-net_weight  = gw_saida_ger_ov-peso_vinculado.        "(quantidade)
              wa_itemdata-untof_wght  = 'KG'.                                  "(unidade medida)
              wa_itemdata-fix_val_dy  = gw_saida_ger_ov-dt_fatura.             "(data criação ov)
              wa_itemdata-price_date  = gw_saida_ger_ov-dt_fatura.             "(data criação ov)
              wa_itemdata-ex_rate_fi  = gw_saida_ger_ov-tax_dolar.             "(taxa dolar)
              wa_itemdata-dlvschduse   = wl_zlest0055-vkaus.

              CLEAR: wl_zlest0030.

              READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX 1.
              IF ( sy-subrc EQ 0 ).

*-BUG 154845-10.10.2024-JT-#154845-inicio
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = w_werks
                  IMPORTING
                    output = lc_werks.

                CLEAR wl_werks.
                SELECT SINGLE * FROM lfa1 INTO wl_werks WHERE lifnr EQ lc_werks.
*-BUG 154845-10.10.2024-JT-#154845-fim

                SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ gw_saida_viagem-po_embarque.
                SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ gw_saida_viagem-po_destino.

                SELECT SINGLE * FROM knvv INTO wl_knvv WHERE kunnr EQ gw_saida_ger_ov-cl_codigo
                                                         AND vkorg EQ gw_saida_ger_ov-vkorg
                                                         AND vtweg EQ gw_saida_ger_ov-vtweg
                                                         AND spart EQ gw_saida_ger_ov-spart.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = wl_knvv-kdgrp
                  IMPORTING
                    output = wl_knvv-kdgrp.

                IF ( wl_lfa1-regio EQ  wl_kna1-regio ).

                  DATA(_dstcat) = '0'.

                  SELECT SINGLE * FROM zlest0030
                    INTO wl_zlest0030
                   WHERE direct     EQ '2'
                     AND dstcat     EQ '0'
                     AND industry   EQ wl_knvv-kdgrp
                     AND tpparceiro EQ '0'
                     AND vkaus      EQ wl_zlest0055-vkaus
                     AND tdlnr      EQ wl_werks-lifnr  "gw_saida_ger_ov-cl_codigo *-BUG 154845-10.10.2024-JT-#154845-inicio
                     AND bukrs      EQ w_bukrs.

                  IF sy-subrc IS NOT INITIAL.
                    SELECT SINGLE * FROM zlest0030
                      INTO wl_zlest0030
                     WHERE direct     EQ '2'
                       AND dstcat     EQ '0'
                       AND industry   EQ wl_knvv-kdgrp
                       AND tpparceiro EQ '0'
                       AND vkaus      EQ wl_zlest0055-vkaus
                       AND tdlnr      EQ wl_werks-lifnr  "gw_saida_ger_ov-cl_codigo *-BUG 154845-10.10.2024-JT-#154845-inicio
                       AND bukrs      EQ space.

                    IF sy-subrc IS NOT INITIAL.

                      SELECT SINGLE * FROM zlest0030
                        INTO wl_zlest0030
                       WHERE direct     EQ '2'
                         AND dstcat     EQ '0'
                         AND industry   EQ wl_knvv-kdgrp
                         AND tpparceiro EQ '0'
                         AND vkaus      EQ wl_zlest0055-vkaus
                         AND tdlnr      EQ space
                         AND bukrs      EQ w_bukrs.

                      IF sy-subrc IS NOT INITIAL.
                        SELECT SINGLE * FROM zlest0030
                          INTO wl_zlest0030
                         WHERE direct     EQ '2'
                           AND dstcat     EQ '0'
                           AND industry   EQ wl_knvv-kdgrp
                           AND tpparceiro EQ '0'
                           AND vkaus      EQ wl_zlest0055-vkaus
                           AND tdlnr      EQ space
                           AND bukrs      EQ space.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                ELSE.

                  _dstcat = '1'.

                  SELECT SINGLE * FROM zlest0030
                    INTO wl_zlest0030
                   WHERE direct     EQ '2'
                     AND dstcat     EQ '1'
                     AND industry   EQ wl_knvv-kdgrp
                     AND tpparceiro EQ '0'
                     AND vkaus      EQ wl_zlest0055-vkaus
                     AND tdlnr      EQ wl_werks-lifnr  "gw_saida_ger_ov-cl_codigo *-BUG 154845-10.10.2024-JT-#154845-inicio
                     AND bukrs      EQ w_bukrs.

                  IF sy-subrc IS NOT INITIAL.

                    SELECT SINGLE * FROM zlest0030
                      INTO wl_zlest0030
                     WHERE direct     EQ '2'
                       AND dstcat     EQ '1'
                       AND industry   EQ wl_knvv-kdgrp
                       AND tpparceiro EQ '0'
                       AND vkaus      EQ wl_zlest0055-vkaus
                       AND tdlnr      EQ wl_werks-lifnr  "gw_saida_ger_ov-cl_codigo *-BUG 154845-10.10.2024-JT-#154845-inicio
                       AND bukrs      EQ space.

                    IF sy-subrc IS NOT INITIAL.

                      SELECT SINGLE * FROM zlest0030
                        INTO wl_zlest0030
                       WHERE direct     EQ '2'
                         AND dstcat     EQ '1'
                         AND industry   EQ wl_knvv-kdgrp
                         AND tpparceiro EQ '0'
                         AND vkaus      EQ wl_zlest0055-vkaus
                         AND tdlnr      EQ space
                         AND bukrs      EQ w_bukrs.

                      IF sy-subrc IS NOT INITIAL.
                        SELECT SINGLE * FROM zlest0030
                          INTO wl_zlest0030
                         WHERE direct     EQ '2'
                           AND dstcat     EQ '1'
                           AND industry   EQ wl_knvv-kdgrp
                           AND tpparceiro EQ '0'
                           AND vkaus      EQ wl_zlest0055-vkaus
                           AND tdlnr      EQ space
                           AND bukrs      EQ space.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.


              IF wl_zlest0030-cfop IS INITIAL .
                v_msg_aux = 'Não encontrado parâmetro de CFOP na ZLES0028!'.
                v_msg_aux = v_msg_aux && | Direção: 2 |.
                v_msg_aux = v_msg_aux && | Categoria Dest. / :  { _dstcat } |.
                v_msg_aux = v_msg_aux && | / Loc.Neg.Cat:  { wl_knvv-kdgrp } |.
                v_msg_aux = v_msg_aux && | / Tp.Parc.: 0 |.
                v_msg_aux = v_msg_aux && | / Utilização: {  wl_zlest0055-vkaus } |.

                MESSAGE v_msg_aux TYPE 'I'.
                RETURN.
              ENDIF.

*-BUG 154845-10.10.2024-JT-#154845-inicio
*             wa_itemdata-cfop_long = wl_zlest0030-cfop.
              wa_itemdata-cfop_long = COND #( WHEN wl_werks-regio = wl_lfa1-regio THEN wl_zlest0030-cfop
                                                                                  ELSE wl_zlest0030-cfop_uf_emit_dif_prest ).
*-BUG 154845-10.10.2024-JT-#154845-fim

              APPEND wa_itemdata TO it_itemdata.

              wl_items_inx-itm_number = 10.
              wl_items_inx-target_qty = 'X'.
              APPEND wl_items_inx TO it_items_inx.

              wl_schedules_in-itm_number = 10.
              wl_schedules_in-req_qty    = 1.
              APPEND wl_schedules_in TO  tl_schedules_in.

              "ORDER_CONDITIONS_IN
              wa_condition-itm_number  = '10'.                  "(item)
              wa_condition-cond_type  = 'PR00'.

              IF NOT ( vlr_liquido IS INITIAL ).
                wa_condition-cond_value  = vlr_liquido. "(valor)


              ELSE.
                IF gw_saida_ger_ov-waerk_fatura = 'BRL'. "gw_saida_ger_ov-waerk 12.12.16
                  wa_condition-cond_value  = gw_saida_ger_ov-vlr_brl. "(valor)
                ELSE.
                  wa_condition-cond_value  = gw_saida_ger_ov-vlr_usd. "(valor)
                ENDIF.
              ENDIF.

              wa_condition-currency   = gw_saida_ger_ov-waerk_fatura. "(moeda) "gw_saida_ger_ov-waerk 12.12.16
              wa_condition-cond_unit  = 'UN'.
              APPEND wa_condition TO it_condition.

              "ORDER_PARTNERS
              wa_partner-partn_role  = 'AG'.
              wa_partner-partn_numb  = gw_saida_ger_ov-cl_codigo. "(CLIENTE EMISSOR OV).
              APPEND wa_partner TO it_partner.

              wa_partner-partn_role  = 'RE'.
              wa_partner-partn_numb  = gw_saida_ger_ov-cl_codigo.
              APPEND wa_partner TO it_partner.

              wa_partner-partn_role  = 'RG'.
              wa_partner-partn_numb  = gw_saida_ger_ov-cl_codigo.
              APPEND wa_partner TO it_partner.

              wa_partner-partn_role  = 'WE'.
              wa_partner-partn_numb  = gw_saida_ger_ov-cl_codigo.
              APPEND wa_partner TO it_partner.

              CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
                EXPORTING
                  sales_header_in     = wl_header_in
                  sales_header_inx    = wl_header_inx
                IMPORTING
                  salesdocument_ex    = vbeln_ov
                TABLES
                  return              = it_return
                  sales_items_in      = it_itemdata
                  sales_items_inx     = it_items_inx
                  sales_partners      = it_partner
                  sales_schedules_in  = tl_schedules_in
                  sales_conditions_in = it_condition.                  "SALES_TEXT = TL_TEXT_IN

              READ TABLE it_return INTO wa_return WITH KEY type = 'E'.
              IF sy-subrc IS INITIAL.
                CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

                CLEAR: wa_return, wl_msg_ret.

                LOOP AT it_return INTO wa_return.
                  wl_msg_ret-msg = wa_return-message.
                  APPEND wl_msg_ret TO tg_msg_ret.
                ENDLOOP.

                CHECK NOT tg_msg_ret[] IS INITIAL.

                CALL FUNCTION 'Z_DOC_CHECK_NEW'
                  EXPORTING
                    i_screen      = '100'
                    i_show        = c_x
                    i_repid       = sy-repid
                    i_pressed_tab = 'TABSTRIP-ACTIVETAB'
                    i_set_field   = 'X_FIELD'
                  IMPORTING
                    e_messagem    = wg_mensagem
                  TABLES
                    it_msgs       = tg_msg_ret.

              ELSE.

                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.

                SELECT * FROM vbuv INTO TABLE tl_vbuv
                  WHERE vbeln EQ vbeln_ov.

                IF ( sy-subrc IS INITIAL ).

                  LOOP AT tl_vbuv INTO wl_vbuv.

                    CLEAR: wl_fieldname, wl_text.
                    wl_fieldname = wl_vbuv-fdnam.

                    CALL FUNCTION 'RM_DDIC_TEXTS_GET'
                      EXPORTING
                        i_name                = wl_fieldname
                        i_type                = 'DTEL'
                        i_langu               = sy-langu
                      IMPORTING
                        e_ddtxt               = wl_text
                      EXCEPTIONS
                        objtype_not_supported = 1
                        illegal_input         = 2
                        OTHERS                = 3.

                    IF ( sy-subrc NE 0 ).
                      CONCATENATE 'Dados incompletos na O.V:' wl_vbuv-fdnam INTO wl_msg_ret-msg SEPARATED BY space.
                    ELSE.
                      CONCATENATE 'Dados incompletos na O.V:' wl_text INTO wl_msg_ret-msg SEPARATED BY space.
                    ENDIF.

                    APPEND wl_msg_ret TO tg_msg_ret.
                  ENDLOOP.

                  CHECK NOT tg_msg_ret[] IS INITIAL.

                  CALL FUNCTION 'Z_DOC_CHECK_NEW'
                    EXPORTING
                      i_screen      = '100'
                      i_show        = c_x
                      i_repid       = sy-repid
                      i_pressed_tab = 'TABSTRIP-ACTIVETAB'
                      i_set_field   = 'X_FIELD'
                    IMPORTING
                      e_messagem    = wg_mensagem
                    TABLES
                      it_msgs       = tg_msg_ret.

                  wl_erro = 'X'.

                ENDIF.

                CASE wl_erro.

                  WHEN: 'X'.

                    REFRESH: it_return.
                    CLEAR: wl_header_inx.
                    wl_header_inx2-updateflag = 'D'.
                    CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
                      EXPORTING
                        salesdocument    = vbeln_ov
                        order_header_inx = wl_header_inx2
                      TABLES
                        return           = it_return.

                    READ TABLE it_return INTO wa_return WITH KEY type = 'E'.

                    IF ( sy-subrc NE 0 ).

                      REFRESH: tl_rows.

                      " Verifica Seleção de Linhas
                      CALL METHOD obj_grid_ger_ov->get_selected_rows
                        IMPORTING
                          et_index_rows = tl_rows.

                      CLEAR: gw_saida_ger_ov, sl_rows.
                      READ TABLE tl_rows INTO sl_rows INDEX 1.
                      READ TABLE gt_saida_ger_ov INTO gw_saida_ger_ov INDEX sl_rows-index.

                      CLEAR: gw_saida_ger_ov-auart,
                             gw_saida_ger_ov-tax_dolar,
                             gw_saida_ger_ov-vlr_usd,
                             gw_saida_ger_ov-vlr_brl,
                             gw_saida_ger_ov-netpr,
                             gw_saida_ger_ov-waerk,
                             gw_saida_ger_ov-waerk_fatura.

                      MODIFY gt_saida_ger_ov FROM gw_saida_ger_ov INDEX sl_rows-index TRANSPORTING auart
                                                                                                   tax_dolar
                                                                                                   vlr_usd
                                                                                                   vlr_brl
                                                                                                   netpr
                                                                                                   waerk
                                                                                                   waerk_fatura.
                      CALL METHOD obj_grid_ger_ov->refresh_table_display
                        EXPORTING
                          is_stable = wa_stable.

                      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                        EXPORTING
                          wait = 'X'.

                    ELSE.

                    ENDIF.

                  WHEN OTHERS.

                    vbeln_fatura = vbeln_ov.

                    DATA: t_success TYPE TABLE OF bapivbrksuccess,
                          t_billing TYPE TABLE OF bapivbrk,
                          t_return  TYPE TABLE OF bapireturn1.

                    CLEAR: t_success[].

                    t_billing = VALUE #( (
                                          ref_doc       = vbeln_fatura
                                          ref_doc_ca    = 'C'
                                          bill_date     = l_data_fatura "sy-datum "*S4H - ZLES0077 - Contingencia-Data Fatura-09.10.2023-JT
                                       ) ).

                    CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE' "#EC CI_USAGE_OK[2438131]
                      TABLES
                        billingdatain = t_billing
                        return        = t_return
                        success       = t_success.

                    IF t_success IS NOT INITIAL.

                      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                        EXPORTING
                          wait = abap_true.

                      TRY.
                          vbeln_fatura = t_success[ 1 ]-bill_doc.
                        CATCH cx_sy_itab_line_not_found.
                      ENDTRY.
                    ELSE.
                      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
                      wl_erro = abap_true.
                    ENDIF.

*                    PERFORM F_SHDB_VF01 CHANGING VBELN_FATURA WL_ERRO.

                    IF t_success[] IS NOT INITIAL.
                      DO 200 TIMES.
                        CLEAR: wl_j_1bnflin.
                        SELECT SINGLE * FROM j_1bnflin INTO wl_j_1bnflin WHERE refkey EQ vbeln_fatura.
                        IF sy-subrc NE 0.
                          WAIT UP TO 1 SECONDS.
                        ELSE.
                          EXIT.
                        ENDIF.
                      ENDDO.
                    ENDIF.

                    IF ( t_success[] IS NOT INITIAL ) AND ( wl_j_1bnflin IS NOT INITIAL ).

                      "Dados para Gravar na ZLEST0061 - Geração da OV.
                      wl_input_zlest0061-mandt          = sy-mandt.
                      wl_input_zlest0061-bukrs          = w_bukrs.
                      wl_input_zlest0061-werks          = w_werks.
                      wl_input_zlest0061-nr_viagem      = w_viagem.
                      wl_input_zlest0061-ano_viagem     = w_ano.

                      wl_input_zlest0061-embarcacao     = gw_saida_ger_ov-embarcacao.
                      wl_input_zlest0061-nome_emb       = gw_saida_ger_ov-nome.
                      wl_input_zlest0061-cod_material   = gw_saida_ger_ov-matnr.
                      wl_input_zlest0061-tp_class       = gw_saida_ger_ov-tp_class(2).
                      wl_input_zlest0061-dt_movimento   = gw_saida_ger_ov-dt_movimento.
                      wl_input_zlest0061-peso_vinculado = gw_saida_ger_ov-peso_vinculado.
                      wl_input_zlest0061-cl_codigo      = gw_saida_ger_ov-cl_codigo.
                      wl_input_zlest0061-rm_codigo      = gw_saida_ger_ov-rm_codigo.
                      wl_input_zlest0061-auart          = gw_saida_ger_ov-auart.
                      wl_input_zlest0061-tax_dolar      = gw_saida_ger_ov-tax_dolar.
                      wl_input_zlest0061-vkorg          = gw_saida_ger_ov-vkorg.
                      wl_input_zlest0061-vtweg          = gw_saida_ger_ov-vtweg.
                      wl_input_zlest0061-spart          = gw_saida_ger_ov-spart.
                      wl_input_zlest0061-matnr_ov       = gw_saida_ger_ov-matnr_ov.
                      wl_input_zlest0061-zterm          = gw_saida_ger_ov-zterm.
                      wl_input_zlest0061-waerk_fatura   = gw_saida_ger_ov-waerk_fatura.
                      wl_input_zlest0061-waerk          = gw_saida_ger_ov-waerk.
                      wl_input_zlest0061-netpr          = gw_saida_ger_ov-netpr.
                      wl_input_zlest0061-vlr_usd        = gw_saida_ger_ov-vlr_usd.
                      wl_input_zlest0061-vlr_brl        = gw_saida_ger_ov-vlr_brl.
                      wl_input_zlest0061-tomador_serv   = gw_saida_ger_ov-tomador_serv.
                      wl_input_zlest0061-nr_ov          = vbeln_ov.
                      wl_input_zlest0061-fatura         = vbeln_fatura.
                      wl_input_zlest0061-docnum         = wl_j_1bnflin-docnum.
                      wl_input_zlest0061-usuario        = sy-uname.
                      wl_input_zlest0061-data_registro  = sy-datum.
                      wl_input_zlest0061-hora_registro  = sy-uzeit.
                      wl_input_zlest0061-nr_dco         = gw_saida_ger_ov-nr_dco.
                      wl_input_zlest0061-safra          = gw_saida_ger_ov-safra.
                      wl_input_zlest0061-dt_fatura      = gw_saida_ger_ov-dt_fatura.
                      wl_input_zlest0061-data_taxa      = gw_saida_ger_ov-data_taxa.
                      wl_input_zlest0061-centro_fat     = werks_fat.
                      wl_input_zlest0061-operacao       = gw_saida_vinc_nf_nome-operacao.
                      wl_input_zlest0061-id_frete_aqua  = gw_saida_vinc_nf_nome-id_frete_aqua.
                      wl_input_zlest0061-eudr           = gw_saida_ger_ov-eudr(1). "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328

                      APPEND wl_input_zlest0061 TO tl_input_zlest0061.
                      MODIFY zlest0061 FROM TABLE  tl_input_zlest0061.

                      COMMIT WORK.

                      IF ( sy-subrc EQ 0 ).

                        IF gw_saida_vinc_nf_nome-id_frete_aqua IS NOT INITIAL.
                          UPDATE zlest0060 SET docnum = wl_input_zlest0061-docnum WHERE id_frete_aqua EQ gw_saida_vinc_nf_nome-id_frete_aqua.
                        ELSE.
                          UPDATE zlest0060 SET docnum = wl_input_zlest0061-docnum WHERE bukrs      EQ w_bukrs
                                                                                    AND werks      EQ w_werks
                                                                                    AND nr_viagem  EQ w_viagem
                                                                                    AND ano_viagem EQ w_ano
                                                                                    AND embarcacao EQ gw_saida_comboio-embarcacao(1)
                                                                                    AND nome_emb   EQ gw_saida_comboio-nome
                                                                                    AND rm_codigo  EQ gw_saida_vinc_nf_nome-rm_codigo
                                                                                    AND cl_codigo  EQ gw_saida_vinc_nf_nome-cl_codigo
                                                                                    AND nr_dco     EQ gw_saida_vinc_nf_nome-nr_dco
                                                                                    AND safra      EQ gw_saida_vinc_nf_nome-safra
                                                                                    AND operacao   EQ gw_saida_vinc_nf_nome-operacao.
                        ENDIF.
                        COMMIT WORK.

                        REFRESH: tl_estilo_ov, tl_rows.
                        CLEAR: wl_estilo_ov, sl_rows.

                        " Verifica Seleção de Linhas
                        CALL METHOD obj_grid_ger_ov->get_selected_rows
                          IMPORTING
                            et_index_rows = tl_rows.
                        READ TABLE tl_rows INTO sl_rows INDEX 1.

                        gw_saida_ger_ov-nr_ov  = vbeln_ov.
                        gw_saida_ger_ov-fatura = vbeln_fatura.
                        gw_saida_ger_ov-docnum = wl_j_1bnflin-docnum.
                        gw_saida_ger_ov-nfnum  = icon_warning.
                        CLEAR: gw_saida_ger_ov-estilo.
                        MODIFY gt_saida_ger_ov FROM gw_saida_ger_ov INDEX sl_rows-index TRANSPORTING nr_ov fatura docnum.

                        wl_estilo_ov-fieldname = 'AUART'.
                        wl_estilo_ov-style = cl_gui_alv_grid=>mc_style_disabled.
                        APPEND wl_estilo_ov TO tl_estilo_ov.

                        wl_estilo_ov-fieldname = 'KURSF'.
                        wl_estilo_ov-style = cl_gui_alv_grid=>mc_style_disabled.
                        APPEND wl_estilo_ov TO tl_estilo_ov.

                        IF ( gw_saida_ger_ov-waerk_fatura IS NOT INITIAL ) AND
                           ( gw_saida_ger_ov-waerk_fatura NE 'USD' ).
                          CLEAR: wl_estilo_ov.
                          wl_estilo_ov-fieldname = 'TAX_DOLAR'.
                          wl_estilo_ov-style = cl_gui_alv_grid=>mc_style_disabled.
                          APPEND wl_estilo_ov TO tl_estilo_ov.
                        ENDIF.

                        wl_estilo_ov-fieldname = 'WAERK'.
                        wl_estilo_ov-style = cl_gui_alv_grid=>mc_style_disabled.
                        APPEND wl_estilo_ov TO tl_estilo_ov.


                        INSERT LINES OF tl_estilo_ov INTO TABLE gw_saida_ger_ov-estilo.
                        MODIFY gt_saida_ger_ov FROM gw_saida_ger_ov INDEX sl_rows-index.

                        CALL METHOD obj_grid_ger_ov->refresh_table_display
                          EXPORTING
                            is_stable = wa_stable.

                        "Bloquear Comboio
                        IF ( gw_saida_comboio-status NE 'X' ) AND ( gw_saida_comboio-estilo IS INITIAL ).
                          REFRESH: tl_estilo_comb, tl_rows.
                          CLEAR: wl_estilo_comb, sl_rows.

                          wl_estilo_comb-fieldname = 'EMBARCACAO'.
                          wl_estilo_comb-style = cl_gui_alv_grid=>mc_style_disabled.
                          APPEND wl_estilo_comb TO tl_estilo_comb.

                          CLEAR: wl_estilo_comb.
                          wl_estilo_comb-fieldname = 'NOME'.
                          wl_estilo_comb-style = cl_gui_alv_grid=>mc_style_disabled.
                          APPEND wl_estilo_comb TO tl_estilo_comb.

                          CLEAR: wl_estilo_comb.
                          wl_estilo_comb-fieldname = 'TP_CLASS'.
                          wl_estilo_comb-style = cl_gui_alv_grid=>mc_style_disabled.
                          APPEND wl_estilo_comb TO tl_estilo_comb.

                          CLEAR: gw_saida_comboio-estilo.
                          MODIFY gt_saida_comboio FROM gw_saida_comboio INDEX index_comboio.
                          INSERT LINES OF tl_estilo_comb INTO TABLE gw_saida_comboio-estilo.
                          MODIFY gt_saida_comboio FROM gw_saida_comboio INDEX index_comboio.

                          REFRESH: tl_estilo_comb, tl_rows.
                          CLEAR: wl_estilo_comb, sl_rows.

                          CLEAR: wl_estilo_comb.
                          wl_estilo_comb-fieldname = 'MATNR'.
                          wl_estilo_comb-style = cl_gui_alv_grid=>mc_style_disabled.
                          APPEND wl_estilo_comb TO tl_estilo_comb.

                          INSERT LINES OF tl_estilo_comb INTO TABLE gw_saida_comboio-estilo.
                          MODIFY gt_saida_comboio FROM gw_saida_comboio INDEX index_comboio.

                          CALL METHOD obj_grid_comboio->refresh_table_display
                            EXPORTING
                              is_stable = wa_stable.

                          IF NOT ( index_comboio IS INITIAL ).
                            READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX index_comboio.

                            UPDATE zlest0063 SET status = 'X' WHERE bukrs      EQ w_bukrs
                                                                AND werks      EQ w_werks
                                                                AND nr_viagem  EQ w_viagem
                                                                AND ano_viagem EQ w_ano
                                                                AND embarcacao EQ gw_saida_comboio-embarcacao
                                                                AND nome_emb   EQ gw_saida_comboio-nome.

                            IF ( sy-subrc EQ 0 ).
                              COMMIT WORK.
                            ELSE.
                              ROLLBACK WORK.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                ENDCASE.
              ENDIF.
            ENDIF.
          WHEN: 2.
            MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Selecionar somente uma ' 'linha para Geração da O.V.'.
          WHEN OTHERS.
            IF ( tl_rows IS INITIAL ).
              MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Linha não selecionada.'.
            ENDIF.
        ENDCASE.
    ENDCASE.

    CLEAR: wl_erro.

    CLEAR:
           vl_lifnr, wl_lfa1, wl_kna1, wl_j_1btxsdc, wl_j_1btxic1, wl_j_1btxpis, wl_j_1btxcof,vlr_liquido, vlr_pis, vlr_cofins, vl_data, vl_validto, vl_validfrom,
           wl_j_1btxiss, wl_j_1btxpis, wl_j_1btxcof, wl_zlest0030.

    "*-CS2025000025-#164218-27.01.2025-JT-inicio
    "REFRESH: lt_zlest0055, lc_retorno,
    REFRESH: lt_zlest0055, lt_zsdt0008,
    "*-CS2025000025-#164218-27.01.2025-JT-Fim
                           lt_j_1btxsdc, lt_j_1bsdica, lt_j_1btxsdc, tl_vbuv.

  ENDMETHOD.                    "zm_handle_user_command
ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: wl_desactive.
    PERFORM verifica_erros_viagem.

    CLEAR: gw_setleaf.

    SELECT SINGLE * FROM setleaf
      INTO gw_setleaf
    WHERE setname EQ 'MAGGI_ZLES0077_IMPORT'
      AND valfrom EQ w_werks.



*    IF TG_MSG_RET[] IS NOT INITIAL OR WG_ACAO_CB = 'OK'.
*      WL_DESACTIVE = 1.
*    ENDIF.
    IF gw_setleaf IS INITIAL.

      ty_toolbar-icon      =  icon_insert_row.
      ty_toolbar-function  =  c_add.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.


      ty_toolbar-icon      =  icon_delete_row.
      ty_toolbar-function  =  c_del.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_system_save.
      ty_toolbar-function  =  c_save.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_change.
      ty_toolbar-function  =  c_edit.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

    ENDIF.
*    TY_TOOLBAR-ICON      = ICON_MAIL.
*    TY_TOOLBAR-FUNCTION  = C_EMAIL_C.
*    TY_TOOLBAR-DISABLED  = WL_DESACTIVE.
*    TY_TOOLBAR-TEXT      = 'Enviar E-mail'.
*    TY_TOOLBAR-BUTN_TYPE = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

    ty_toolbar-icon      = icon_insert_relation.
    ty_toolbar-function  = c_qtdv.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-text      = 'Qtd. Vincu.'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = c_del_emb.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-text      = 'Excluir Embarcações'.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    IF NOT gw_setleaf IS INITIAL.
      ty_toolbar-icon      = icon_xml_doc.
      ty_toolbar-function  = c_imp_xml.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-text      = 'Importar XML'.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
    DATA: tl_itens_aux TYPE TABLE OF zaqty_saida_comboio,
          wl_itens     TYPE zaqty_saida_comboio,
          wl_lines     TYPE sy-tabix,
          wl_zlest0060 TYPE zlest0060.

    REFRESH: tl_itens_aux.

    CASE e_ucomm.

      WHEN c_save.

        PERFORM verifica_erros_comboio.

        IF tg_msg_ret[] IS INITIAL.
          CLEAR wg_acao.
          PERFORM grava_dados_comboio.
          PERFORM: criar_alv_viagem.

          op_modo = c_save.

          PERFORM: selecionar_dados_comboio,
                   criar_alv_comboio.

        ELSE.

          MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.

          CALL FUNCTION 'Z_DOC_CHECK_NEW'
            EXPORTING
              i_screen      = '100'
              i_show        = c_x
              i_repid       = sy-repid
              i_pressed_tab = 'TABSTRIP-ACTIVETAB'
              i_set_field   = 'X_FIELD'
            IMPORTING
              e_messagem    = wg_mensagem
            TABLES
              it_msgs       = tg_msg_ret.

        ENDIF.

      WHEN c_edit.

        op_modo = c_edit.

        PERFORM: selecionar_dados_comboio,
                 criar_alv_comboio.

      WHEN c_add.

        IF op_modo NE c_edit.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Modo edição não habilitado!'.
          EXIT.
        ENDIF.

        READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX 1.
        tl_itens_aux[] = gt_saida_comboio[].
        REFRESH: gt_saida_comboio.
        LOOP AT tl_itens_aux INTO wl_itens.
          APPEND wl_itens TO gt_saida_comboio.
        ENDLOOP.
        CLEAR: wl_itens.
        wl_itens-unid_medida    = gw_saida_viagem-und_medida.
        APPEND wl_itens TO gt_saida_comboio.

        CALL METHOD obj_grid_comboio->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_del.

        CALL METHOD obj_grid_comboio->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.

          READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX wg_selectedcell-row_id-index.
          IF sy-subrc = 0.

            SELECT SINGLE *
              FROM zlest0060 INTO wl_zlest0060
             WHERE bukrs       EQ w_bukrs
               AND werks       EQ w_werks
               AND ano_viagem  EQ w_ano
               AND nr_viagem   EQ w_viagem
               AND nome_emb    EQ gw_saida_comboio-nome.

            IF sy-subrc = 0.
              MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Existem notas vinculadas! Operação não permitida!'.
              EXIT.
            ENDIF.

            DELETE gt_saida_comboio INDEX wg_selectedcell-row_id-index.
          ENDIF.
        ENDLOOP.

        CALL METHOD obj_grid_comboio->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN: c_qtdv.
        CALL SCREEN 1300 STARTING AT 020 2 ENDING AT 200 17.
      WHEN: c_imp_xml.

        CALL SCREEN 1500 STARTING AT 010 2 ENDING   AT 150 30.
      WHEN: c_del_emb.

        SELECT SINGLE *
          FROM zlest0060 INTO wl_zlest0060
         WHERE bukrs       EQ w_bukrs
           AND werks       EQ w_werks
           AND ano_viagem  EQ w_ano
           AND nr_viagem   EQ w_viagem.

        IF sy-subrc = 0.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Existem notas vinculadas! Operação não permitida!'.
          EXIT.
        ENDIF.

        DELETE FROM zlest0063 WHERE bukrs      = w_bukrs
                                AND werks      = w_werks
                                AND nr_viagem  = w_viagem
                                AND ano_viagem = w_ano.

        CLEAR: gt_saida_comboio[].

        PERFORM: criar_alv_comboio.
        PERFORM: criar_alv_viagem.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Dados do comboio excluídos com sucesso!'.

    ENDCASE.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*----------------------------------------------------------------------*
* ICONES
*----------------------------------------------------------------------*
DATA: icon_viagem     TYPE c LENGTH 4, "Variável para configurar o ICONE da Subtela 0400/0500 da Viagem
      icon_vinculacao TYPE c LENGTH 4, "Variável para configurar o ICONE da Subtela 0600/0700 da Vinculação
      icon_protocolo  TYPE c LENGTH 4. "Variável para configurar o ICONE da Subtela 1700/1800 do Protocolo

INITIALIZATION.

  PERFORM atualizar_tabelas_aquaviario.

START-OF-SELECTION.

*S4H - ZLES0077 - Contingencia-Data Fatura-09.10.2023-JT-inicio
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'ZLES0077_DT_FAT'
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  READ TABLE t_value INTO w_value WITH KEY from = sy-uname.
  l_data_fatura = COND #( WHEN sy-subrc = 0 THEN w_value-title+6(4) && w_value-title+3(2) && w_value-title
                                            ELSE sy-datum ).
*S4H - ZLES0077 - Contingencia-Data Fatura-09.10.2023-JT-fim

  "Perform para preencher icones das descrições da Subtela 0400/0500/0600/0700.
  PERFORM: descricao_icones.
  CALL SCREEN tela_0100. "Chamar Tela Principal

*&---------------------------------------------------------------------*
*&      Form  DESCRICAO_ICONES
*&---------------------------------------------------------------------*
FORM descricao_icones.
  icon_viagem     = icon_ws_ship. "Atribuição do Icone ICON_WS_SHIP (Transporte de Barco)
  icon_vinculacao = icon_insert_relation. "Atribuição do Icone ICON_INSERT_RELATION (Relação).
  icon_protocolo  = icon_protocol. "Atribuição do Icone ICON_PROTOCOL (Log).
ENDFORM.                    " DESCRICAO_ICONES
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  DATA buttons TYPE TABLE OF sy-ucomm.

  IF gt_saida_viagem[] IS INITIAL.
    APPEND 'REG_COMBO' TO buttons.
    APPEND 'REG_COMBOB' TO buttons.
  ELSE.
    CLEAR buttons.
  ENDIF.

  SET PF-STATUS 'PF0100' EXCLUDING buttons. "Status da Tela 0100.
  SET TITLEBAR  'TB0100'. "Title da Tela 0100

*  " Controle da Tela

  CASE novo.

    WHEN: 'X'.
      LOOP AT SCREEN.
        CASE screen-name.
          WHEN: 'W_BUKRS' OR
                'W_WERKS' OR
                'W_ANO'   .
            screen-input  = '0'.
            MODIFY SCREEN.
          WHEN: 'W_VIAGEM'.
            screen-input     = '0'.
            screen-invisible = '0'.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.

    WHEN 'C'.

      REFRESH: gt_saida_viagens,
               gt_fcat_viagens,
               gt_saida_viagem,
               gt_saida_comboio[].

      LOOP AT SCREEN.
        CASE screen-name.
          WHEN: 'W_BUKRS' OR
                'W_WERKS' OR
                'W_ANO'.
            screen-input     = '1'.
          WHEN: 'W_VIAGEM'.
            screen-input     = '1'.
            screen-invisible = '1'.
          WHEN: 'TXT_VIAGEM'.
            screen-input     = '1'.
            screen-invisible = '1'.
          WHEN: 'BTN_BUSCA'.
            screen-input     = '1'.
            screen-invisible = '1'.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.

      tela_viagem = tela_0400. "Atribuição da SUBTELA 0400

      IF obj_grid_comboio IS NOT INITIAL.
        CALL METHOD obj_grid_comboio->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      ENDIF.

      IF obj_grid_viagem IS NOT INITIAL.
        CALL METHOD obj_grid_viagem->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      ENDIF.

    WHEN: 'B'.

      LOOP AT SCREEN.
        CASE screen-name.
          WHEN: 'W_BUKRS' OR
                'W_WERKS' OR
                'W_ANO'   .
            screen-input     = '0'.
          WHEN: 'W_VIAGEM'.
            screen-input     = '0'.
            screen-invisible = '0'.
          WHEN: 'TXT_VIAGEM'.
            screen-invisible = '0'.
          WHEN: 'BTN_BUSCA'.
            screen-invisible = '0'.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.


    WHEN OTHERS.
      IF w_viagem IS INITIAL.
        LOOP AT SCREEN.
          CASE screen-name.
            WHEN: 'W_BUKRS' OR
                  'W_WERKS' OR
                  'W_ANO'.
              screen-input     = '1'.
            WHEN: 'W_VIAGEM'.
              screen-input     = '1'.
              screen-invisible = '1'.
            WHEN: 'TXT_VIAGEM'.
              screen-input     = '1'.
              screen-invisible = '1'.
            WHEN: 'BTN_BUSCA'.
              screen-input     = '1'.
              screen-invisible = '1'.
          ENDCASE.
          MODIFY SCREEN.
        ENDLOOP.
      ENDIF.
  ENDCASE.

  CLEAR: novo.

ENDMODULE.                 " PBO_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
MODULE pai_0100 INPUT.

  DATA: tl_rows   TYPE lvc_t_row,
        sl_rows   TYPE lvc_s_row,
        qtd_linha TYPE sy-tabix.

  tela_protocolo = tela_1700. "Atribuição da SUBTELA 1700

  CASE ok_code.

    WHEN: tab_comboio.

      novo = 'B'.

      CALL METHOD obj_grid_comboio->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

      IF NOT ( obj_grid_vinc_nf IS INITIAL ).
        CALL METHOD obj_grid_vinc_nf->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      ENDIF.

      tabstrip-activetab = tab_comboio.
      CLEAR: index_comboio.

    WHEN: tab_nf.

      REFRESH: gt_saida_ger_ov[].
      novo = 'B'.

      CLEAR: qtd_linha, gw_saida_comboio.

      IF op_modo EQ c_edit .
        MESSAGE s000(zaquaviario) WITH 'Dados do Comboio em modo de edição!' DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.

      CALL METHOD obj_grid_comboio->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.

      DESCRIBE TABLE tl_rows LINES qtd_linha.

      IF NOT ( index_comboio IS INITIAL ).
        qtd_linha = 1.
      ENDIF.

      IF ( qtd_linha EQ 1 ).

        READ TABLE tl_rows INDEX 1 INTO sl_rows.
        IF ( sy-subrc NE 0 ).
          sl_rows-index = index_comboio.
          READ TABLE gt_saida_comboio INDEX sl_rows-index INTO gw_saida_comboio.
        ELSE.
          READ TABLE gt_saida_comboio INDEX sl_rows-index INTO gw_saida_comboio.
        ENDIF.

        IF ( gw_saida_comboio-embarcacao(1) EQ 'E').
          MESSAGE s000(zaquaviario) WITH 'A linha selecionada não é uma Barcaça!' DISPLAY LIKE 'W'.
        ELSE.

          index_comboio = sl_rows-index.

          PERFORM: seleciona_notas_fiscais," USING GW_SAIDA_COMBOIO,
                   criar_alv_vinc_nf.
          tabstrip-activetab = tab_nf.

        ENDIF.

      ELSE.
        MESSAGE s000(zaquaviario) WITH 'Selecionar uma linha do Comboio' DISPLAY LIKE 'W'.
      ENDIF.

    WHEN: tab_nf_vinc.
      tabstrip-activetab = tab_nf_vinc.
    WHEN: tab_fatura.

      novo = 'B'.

      IF op_modo EQ c_edit .
        MESSAGE s000(zaquaviario) WITH 'Dados do Comboio em modo de edição!' DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.

      CLEAR: tl_rows, sl_rows.
      IF NOT ( obj_grid_vinc_nf IS INITIAL ).
        CALL METHOD obj_grid_vinc_nf->get_selected_rows
          IMPORTING
            et_index_rows = tl_rows.

        DESCRIBE TABLE tl_rows LINES qtd_linha.

        IF ( qtd_linha EQ 1 ).
          PERFORM carregar_fatura.
          tabstrip-activetab = tab_fatura.
        ELSE.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Selecionar somente uma linha das vinculações.'.
        ENDIF.
      ELSE.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Selecionar somente uma linha das vinculações.'.
      ENDIF.

    WHEN: 'BACK'. "Botão para Voltar a Tela anterior.
      LEAVE TO SCREEN 0.
    WHEN: 'CANC'. "Botão para Voltar a Tela anterior.
      LEAVE TO SCREEN 0.
    WHEN: 'EXIT'. "Botão para sair do programa corrente.
      LEAVE PROGRAM.
    WHEN: 'ENTER'.

      PERFORM: validacao_campos.

      CASE valida.
        WHEN: 'X'.

        WHEN OTHERS.
          PERFORM: preencher_campos,    "Perform para preencher campos de descrição da tela.
                   solicitar_nova_viagem. "Verifica Viagens Existentes e  Solicita novas.
      ENDCASE.

      CLEAR: valida.

    WHEN: 'BTNV_EXP'. "Botão para Expandir a Subtela 0400 para 0500 da Viagem

      PERFORM: validacao_campos.
      IF ( valida NE 'X' ).
        tela_viagem = tela_0500. "Atribuição da SUBTELA 0500
        PERFORM: criar_alv_viagem.
      ENDIF.
      CLEAR: valida.

    WHEN: 'BTNV_MIN'. "Botão para comprimir a Subtela 0400 para 0500 da Viagem
      tela_viagem = tela_0400. "Atribuição da SUBTELA 0400

    WHEN: 'BTNP_PRO'.

      PERFORM: validacao_campos.
      IF ( valida NE 'X' ).
        tela_protocolo = tela_1800. "Atribuição da SUBTELA 1800
        PERFORM get_protocolo.
      ENDIF.
      CLEAR: valida.

    WHEN: 'BTNP_MIN'.
      tela_protocolo = tela_1700.

    WHEN: 'BTN_NOVO'.

      CLEAR: valida.
      PERFORM: validacao_campos.

      IF ( valida NE 'X' ).
        PERFORM: limpar_tabelas.
        PERFORM: gerar_numero_viagem CHANGING w_viagem.
        PERFORM: adicionar_linha_viagem,
                 criar_alv_viagem.
        tela_viagem = tela_0500.

      ENDIF.

    WHEN: 'BTNEX_VINC'.

      "Perform Criada para adicionar 30 linhas iniciais no ALV do Comboio.
      READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX 1.
      IF sy-subrc NE 0.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Informe dados da viagem.'.
        REFRESH gt_saida_comboio.
        EXIT.
      ENDIF.

      IF ( w_viagem EQ '0000' ).
        MESSAGE s000(zaquaviario) WITH 'Viagem não cadastrada.' DISPLAY LIKE 'W'.

      ELSE.
        novo = 'B'.
        tela_vinculacao = tela_0700.
        tela_viagem     = tela_0400.

        PERFORM:
                selecionar_dados_comboio,
                adicionar_linha_comboio,
                criar_alv_comboio.
      ENDIF.
    WHEN: 'BTNM_VINC'.
      novo = 'B'.
      tela_vinculacao = tela_0600.

    WHEN: 'BTN_BUSCA'.

      tabstrip-activetab = tab_comboio.

      REFRESH: gt_saida_viagens.
      CLEAR: gw_saida_viagens.

      PERFORM: validacao_campos.

      CASE valida.
        WHEN: 'X'.
        WHEN OTHERS.
          PERFORM: solicitar_nova_viagem .
      ENDCASE.

    WHEN 'REG_COMBO' OR 'REG_COMBOB'.

      tela_protocolo = tela_1700. "Atribuição da SUBTELA 1700

      CLEAR valida.
      PERFORM validacao_campos.

      IF ( valida <> 'X' ).
        gw_popup_comboio-data_previsao_saida   = gw_saida_viagem-dt_prevista.
        gw_popup_comboio-data_previsao_chegada = gw_saida_viagem-dt_prevista + 4.
        gw_popup_comboio-hora_previsao_saida   = gw_saida_viagem-hr_prevista.
        gw_popup_comboio-hora_previsao_chegada = gw_saida_viagem-hr_prevista.

        CALL SCREEN 1600 STARTING AT 5 5.
      ENDIF.

      CLEAR valida.
    WHEN: c_save.

      PERFORM verifica_erros_viagem.

      IF tg_msg_ret[] IS INITIAL.
        CLEAR wg_acao.
        PERFORM grava_dados_viagem.
      ELSE.

        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'TABSTRIP-ACTIVETAB'
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ENDIF.

  ENDCASE.

ENDMODULE.                 " PAI_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  PREENCHER_CAMPOS
*&---------------------------------------------------------------------*
FORM preencher_campos .

  DATA: wl_t001  TYPE t001,  " Work Area para Informações da Empresa
        wl_t001w TYPE t001w,
        vl_werks TYPE lfa1-lifnr. " Work Area para Informações do Centro
  CLEAR: wl_t001, wl_t001w. "Limpar Work Area

  "Recuperar Descrição da Empresa
  SELECT SINGLE * FROM t001 INTO  wl_t001  WHERE bukrs EQ w_bukrs.
  w_desc_bukrs = wl_t001-butxt. "Descrição da Empresa
  "Recuperar Descrição do Centro Emissor
  SELECT SINGLE * FROM t001w INTO wl_t001w WHERE werks EQ w_werks.
  w_desc_werks = wl_t001w-name1. "Descrição do Centro

*Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - INICIO
  CLEAR: area_parametro_filial_eudr.
  vl_werks              = |{ w_werks ALPHA = IN }|.

  SELECT SINGLE *
    FROM zlest0104 INTO @DATA(lwa_zlest0104)
    WHERE centro_fat EQ @w_werks.

  IF sy-subrc EQ 0.
    SELECT SINGLE *
       FROM zlest0206 AS a INTO @DATA(lwa_zsdt0206)
      WHERE local_descarga EQ @lwa_zlest0104-local_descarga
        AND EXISTS (
          SELECT b~kunnr
            FROM zsdt0351 AS b
           WHERE b~kunnr EQ a~po_embarque ).
    IF sy-subrc IS INITIAL.
      area_parametro_filial_eudr = abap_true.
    ENDIF.
  ENDIF.

*Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - FIM

ENDFORM.                    " PREENCHER_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  VALIDACAO_CAMPOS
*&---------------------------------------------------------------------*
FORM validacao_campos .

  DATA: wl_t001k TYPE t001k.

  CLEAR: valida.

  IF ( w_bukrs IS INITIAL ). "Caso o campo Empresa não esteja preenchido.
    valida = 'X'.
    MESSAGE s001(zaquaviario) DISPLAY LIKE 'W'.
  ELSEIF ( w_werks IS INITIAL ). "Caso o campo Centro não esteja preenchido.
    valida = 'X'.
    MESSAGE s002(zaquaviario) DISPLAY LIKE 'W'.
  ELSEIF ( w_ano IS INITIAL ). "Caso o campo Ano não esteja preenchido.
    valida = 'X'.
    MESSAGE s003(zaquaviario) DISPLAY LIKE 'W'.
  ELSE.
    "Verifica se o centro informado pertence a empresa informada.
    SELECT SINGLE *
      FROM t001k INTO wl_t001k
    WHERE bwkey EQ w_werks
      AND bukrs EQ w_bukrs.

    IF ( sy-subrc NE 0 ).
      CLEAR: w_bukrs, w_werks, w_ano, w_viagem.
      valida = 'X'.
      MESSAGE s012(zaquaviario) DISPLAY LIKE 'W'.
    ENDIF.
  ENDIF.
ENDFORM.                    " VALIDACAO_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  SOLICITAR_NOVA_VIAGEM
*&---------------------------------------------------------------------*
FORM solicitar_nova_viagem .

  DATA: v_msg        TYPE c LENGTH 255,
        v_ans        TYPE c,
        tl_zlest0068 TYPE TABLE OF zlest0068.

  valida = 'X'.

  SELECT * FROM zlest0068
    INTO TABLE tl_zlest0068
        WHERE bukrs    EQ w_bukrs
        AND werks      EQ w_werks
        AND ano_viagem EQ w_ano.

  IF ( sy-subrc EQ 0 ) AND  NOT ( tl_zlest0068 IS INITIAL ).

    PERFORM: visualizar_viagens TABLES tl_zlest0068.
    CALL SCREEN 1400 STARTING AT 15 1 ENDING AT 95 10.

    IF ( gt_saida_viagem[] IS INITIAL ) AND ( novo NE 'C' ).
      PERFORM: adicionar_linha_viagem,
               criar_alv_viagem.
    ELSE.
      IF ( novo NE 'C' ).
        PERFORM: criar_alv_viagem.
      ENDIF.

    ENDIF.

    IF ( novo NE 'C' ).
      tela_viagem     = tela_0500.
      tela_vinculacao = tela_0600.
      LEAVE TO SCREEN 0100.
    ENDIF.

  ELSE.

    CONCATENATE 'Empresa: ' w_bukrs 'e Centro: ' w_werks 'não possui viagens, deseja criar ?' INTO v_msg SEPARATED BY space.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Criação da Viagem'
        text_question         = v_msg
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ''
      IMPORTING
        answer                = v_ans.

    CASE v_ans.

      WHEN: '1'.
        PERFORM: gerar_numero_viagem CHANGING w_viagem.
        IF NOT ( w_viagem IS INITIAL ).

          novo = 'X'.

          PERFORM: adicionar_linha_viagem,
                   criar_alv_viagem.
          tela_viagem = tela_0500.
          LEAVE TO SCREEN 0100.
        ENDIF.

      WHEN: '2'.

    ENDCASE.
  ENDIF.

ENDFORM.                    " SOLICITAR_NOVA_VIAGEM
*&---------------------------------------------------------------------*
*&      Form  GERAR_NUMERO_VIAGEM
*&---------------------------------------------------------------------*
FORM gerar_numero_viagem  CHANGING p_viagem TYPE zlest0058-nr_viagem.
  " Função para Verificar o número da viagem
  CALL FUNCTION 'Z_LES_NR_VIAGEM'
    EXPORTING
      p_bukrs      = w_bukrs
      p_werks      = w_werks
      p_ano_viagem = w_ano
    IMPORTING
      p_nr_viagem  = p_viagem.
ENDFORM.                    " GERAR_NUMERO_VIAGEM
*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_VIAGENS
*&---------------------------------------------------------------------*
FORM visualizar_viagens TABLES p_zlest0068 STRUCTURE zlest0068.

  DATA: wl_zlest0068 TYPE zlest0068.

  REFRESH: gt_saida_viagens.

  LOOP AT p_zlest0068 INTO wl_zlest0068.
    gw_saida_viagens-bukrs      = wl_zlest0068-bukrs.
    gw_saida_viagens-werks      = wl_zlest0068-werks.
    gw_saida_viagens-ano_viagem = wl_zlest0068-ano_viagem.
    gw_saida_viagens-nr_viagem  = wl_zlest0068-nr_viagem.
    APPEND gw_saida_viagens TO gt_saida_viagens.
    CLEAR: gw_saida_viagens, wl_zlest0068.
  ENDLOOP.

  SORT: gt_saida_viagens BY nr_viagem DESCENDING.

ENDFORM.                    " VISUALIZAR_VIAGENS
*&---------------------------------------------------------------------*
*&      Module  PBO_1400  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_1400 OUTPUT.
  SET PF-STATUS 'PF1400'. "Status da Tela 1400
  SET TITLEBAR  'TB1400'. "Title da Tela 1400
  PERFORM: alv_viagens_existentes.
ENDMODULE.                 " PBO_1400  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI_1400  INPUT
*&---------------------------------------------------------------------*
MODULE pai_1400 INPUT.
  CASE ok_code.
    WHEN: 'CANCELAR'. "Botão para Voltar a Tela anterior.
      novo = 'C'.
      LEAVE TO SCREEN 0.
    WHEN: 'SELECIONAR'. "Botão para selecionar uma viagem existente.
      PERFORM: selecionar_viagem_existente.
    WHEN: 'NOVA_VIAG'.

      PERFORM: gerar_numero_viagem CHANGING w_viagem.
      IF NOT ( w_viagem IS INITIAL ).
        REFRESH: gt_saida_viagem[].
      ENDIF.
      LEAVE TO SCREEN 0.
  ENDCASE.

  CLEAR valida.
ENDMODULE.                 " PAI_1400  INPUT
*&---------------------------------------------------------------------*
*&      Form  ALV_VIAGENS_EXISTENTES
*&---------------------------------------------------------------------*
FORM alv_viagens_existentes .

  DATA: wl_layout   TYPE lvc_s_layo.


  CLEAR: wl_layout.
  REFRESH: gt_fcat_viagens.
  CLEAR: gw_fcat_viagens.

  CHECK NOT gt_saida_viagens[] IS INITIAL.

  PERFORM montar_catalog_viagens USING:
        'BUKRS'      'Empresa'    '15'   '' '' '' '' '' '' '' '' '' ,
        'WERKS'      'Centro'     '15'    '' '' '' '' '' '' '' '' '' ,
        'ANO_VIAGEM' 'Ano Viagem' '15'    '' '' '' '' '' '' '' '' '' ,
        'NR_VIAGEM'  'Nr. Viagem' '15'   '' '' '' '' '' '' '' '' ''.

  CHECK NOT gt_fcat_viagens[] IS INITIAL.

  IF ( obj_custom_viagens IS INITIAL ).

    CREATE OBJECT obj_custom_viagens
      EXPORTING
        container_name              = 'CONTAINER_VIAGENS'
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


    CREATE OBJECT obj_grid_viagens
      EXPORTING
        i_parent          = obj_custom_viagens
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

    wl_layout-sel_mode   = 'A'.
    wl_layout-no_toolbar = 'X'.

    CALL METHOD obj_grid_viagens->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
        i_save                        = 'U'
      CHANGING
        it_outtab                     = gt_saida_viagens[]
        it_fieldcatalog               = gt_fcat_viagens[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD obj_grid_viagens->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.                    " ALV_VIAGENS_EXISTENTES
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_VIAGENS
*&---------------------------------------------------------------------*
FORM montar_catalog_viagens USING  VALUE(p_fieldname)
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



  CLEAR: gw_fcat_viagens.

  gw_fcat_viagens-fieldname = p_fieldname.
  gw_fcat_viagens-ref_table = p_ref_tabname..
  gw_fcat_viagens-ref_field = p_ref_fieldname.
  gw_fcat_viagens-tabname   = p_tabname.
  gw_fcat_viagens-scrtext_l = p_desc.
  gw_fcat_viagens-scrtext_m = p_desc.
  gw_fcat_viagens-scrtext_s = p_desc.
  gw_fcat_viagens-outputlen = p_tam.
  gw_fcat_viagens-no_zero   = p_no_zero.
  gw_fcat_viagens-hotspot   = p_hotspot.
  gw_fcat_viagens-emphasize = p_cor.
  gw_fcat_viagens-just      = p_just.
  gw_fcat_viagens-do_sum    = p_sum.
  gw_fcat_viagens-edit      = p_edit.

  APPEND gw_fcat_viagens TO gt_fcat_viagens.

ENDFORM.                    " MONTAR_CATALOG_VIAGENS
*&      Form  SELECIONAR_VIAGEM_EXISTENTE
*&---------------------------------------------------------------------*
FORM selecionar_viagem_existente .

  DATA: tl_lfa1         TYPE TABLE OF lfa1,
        wl_lfa1         TYPE lfa1,
        tl_kna1         TYPE TABLE OF kna1,
        wl_kna1         TYPE kna1,
        tl_zlest0056    TYPE TABLE OF zlest0056,
        wl_zlest0056    TYPE zlest0056,
        tabix           TYPE sy-tabix,
        tl_estilo       TYPE lvc_t_styl WITH HEADER LINE,
        wl_estilo       TYPE lvc_s_styl,
        wl_zlest0063    TYPE zlest0063,
        vl_vinc_comboio TYPE c,
        lw_setleaf      TYPE setleaf.


  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  CALL METHOD obj_grid_viagens->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  READ TABLE tl_rows INDEX 1 INTO sl_rows.

  IF NOT ( sl_rows IS INITIAL ).

    READ TABLE gt_saida_viagens INTO gw_saida_viagens INDEX sl_rows-index.

    SELECT * FROM zlest0056
        INTO TABLE tl_zlest0056
       WHERE bukrs      EQ gw_saida_viagens-bukrs
         AND werks      EQ gw_saida_viagens-werks
         AND ano_viagem EQ gw_saida_viagens-ano_viagem
         AND nr_viagem  EQ gw_saida_viagens-nr_viagem.

    SELECT * FROM lfa1
      INTO TABLE tl_lfa1
      FOR ALL ENTRIES IN tl_zlest0056
    WHERE lifnr EQ tl_zlest0056-po_embarque.

    SELECT * FROM kna1
      INTO TABLE tl_kna1
      FOR ALL ENTRIES IN tl_zlest0056
    WHERE kunnr EQ tl_zlest0056-po_destino.


    REFRESH: gt_saida_viagem[].
    CLEAR: gw_saida_viagem.
    LOOP AT tl_zlest0056 INTO wl_zlest0056.

      READ TABLE tl_lfa1 INTO wl_lfa1 WITH KEY lifnr = wl_zlest0056-po_embarque.

      MOVE: wl_lfa1-name1 TO gw_saida_viagem-name1_emb,
            wl_lfa1-ort01 TO gw_saida_viagem-ort01_emb,
            wl_lfa1-regio TO gw_saida_viagem-regio_emb,
            wl_lfa1-lifnr TO gw_saida_viagem-po_embarque.


      READ TABLE tl_kna1 INTO wl_kna1 WITH KEY kunnr = wl_zlest0056-po_destino.

      MOVE: wl_kna1-name1 TO gw_saida_viagem-name1_dest,
            wl_kna1-ort01 TO gw_saida_viagem-ort01_dest,
            wl_kna1-regio TO gw_saida_viagem-regio_dest,
            wl_kna1-kunnr TO gw_saida_viagem-po_destino.


      IF wl_zlest0056-und_medida = 'TO' .
        gw_saida_viagem-und_medida = 'TO-Tonelada'.
      ELSE.
        gw_saida_viagem-und_medida = 'KG-Quilograma'.
      ENDIF.

      CASE wl_zlest0056-direcao.
        WHEN: 'N'.
          gw_saida_viagem-direcao = 'Norte'.
        WHEN: 'S'.
          gw_saida_viagem-direcao = 'Sul'.
        WHEN: 'L'.
          gw_saida_viagem-direcao = 'Leste'.
        WHEN: 'O'.
          gw_saida_viagem-direcao = 'Oeste'.
      ENDCASE.

      MOVE: wl_zlest0056-dt_prevista TO gw_saida_viagem-dt_prevista,
            wl_zlest0056-hr_prevista TO gw_saida_viagem-hr_prevista,
            wl_zlest0056-calado      TO gw_saida_viagem-calado,
            wl_zlest0056-und_medida  TO gw_saida_viagem-und_medida,
            wl_zlest0056-bukrs       TO gw_saida_viagem-bukrs,
            wl_zlest0056-werks       TO gw_saida_viagem-werks,
            wl_zlest0056-ano_viagem  TO gw_saida_viagem-ano_viagem,
            wl_zlest0056-nr_viagem   TO gw_saida_viagem-nr_viagem.

      APPEND gw_saida_viagem TO gt_saida_viagem.
    ENDLOOP.

    CLEAR: op_modo.

    IF ( gt_saida_viagem[] IS INITIAL ).
      CLEAR: novo.
      novo = 'B'.
      w_viagem = gw_saida_viagens-nr_viagem.
      LEAVE TO SCREEN 0.

    ELSEIF NOT (  gt_saida_viagem[] IS INITIAL ).
      CLEAR: novo.
      novo = 'B'.
      w_viagem = gw_saida_viagens-nr_viagem.

      CLEAR: gw_saida_viagem.

      LOOP AT gt_saida_viagem INTO gw_saida_viagem.

        CLEAR: tl_estilo[], wl_estilo,vl_vinc_comboio, gw_saida_viagem-estilo.

        tabix = sy-tabix.

        SELECT SINGLE *
          FROM zlest0063 INTO wl_zlest0063
         WHERE bukrs       = gw_saida_viagem-bukrs
           AND werks       = gw_saida_viagem-werks
           AND ano_viagem  = gw_saida_viagem-ano_viagem
           AND nr_viagem   = gw_saida_viagem-nr_viagem.
        IF sy-subrc = 0.
          vl_vinc_comboio = 'X'.
        ENDIF.

        SELECT SINGLE * FROM setleaf
          INTO lw_setleaf
         WHERE setname EQ 'MAGGI_ZLES0077_CALADO'
           AND valfrom EQ gw_saida_viagem-po_embarque.
        IF ( sy-subrc NE 0 ) OR ( vl_vinc_comboio IS NOT INITIAL ).
          wl_estilo-fieldname = 'CALADO'.
          wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
          APPEND wl_estilo TO tl_estilo.
        ENDIF.

        IF vl_vinc_comboio IS NOT INITIAL. "NOT ( gw_saida_viagem-dt_prevista IS INITIAL ).
          wl_estilo-fieldname = 'DT_PREVISTA'.
          wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
          APPEND wl_estilo TO tl_estilo.
        ENDIF.

        IF vl_vinc_comboio IS NOT INITIAL.
          wl_estilo-fieldname = 'HR_PREVISTA'.
          wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
          APPEND wl_estilo TO tl_estilo.
        ENDIF.

        IF vl_vinc_comboio IS NOT INITIAL. "NOT ( gw_saida_viagem-po_destino IS INITIAL ).
          wl_estilo-fieldname = 'PO_DESTINO'.
          wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
          APPEND wl_estilo TO tl_estilo.
        ENDIF.

        IF vl_vinc_comboio IS NOT INITIAL. "NOT ( gw_saida_viagem-po_embarque IS INITIAL ).
          wl_estilo-fieldname = 'PO_EMBARQUE'.
          wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
          APPEND wl_estilo TO tl_estilo.
        ENDIF.

        INSERT LINES OF tl_estilo INTO TABLE gw_saida_viagem-estilo.
        MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX tabix.

      ENDLOOP.


      LEAVE TO SCREEN 0.
    ENDIF.

  ELSE.
    MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Selecionar uma linha'.
  ENDIF.

ENDFORM.                    " SELECIONAR_VIAGEM_EXISTENTE
*&---------------------------------------------------------------------*
*&      Form  ADICIONAR_LINHA_VIAGEM
*&---------------------------------------------------------------------*
FORM adicionar_linha_viagem .

  DATA: wl_saida_viagem TYPE zaqty_saida_viagem.
  REFRESH  gt_saida_viagem.
  CLEAR: wl_saida_viagem.

  "WL_SAIDA_VIAGEM-STATUS_ICON   = ICON_LED_YELLOW.
  wl_saida_viagem-usuario       = sy-uname.
  wl_saida_viagem-data_registro = sy-datum.
  wl_saida_viagem-hora_registro = sy-uzeit.
  APPEND wl_saida_viagem TO gt_saida_viagem.
  novo = 'X'.
ENDFORM.                    " ADICIONAR_LINHA_VIAGEM
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_VIAGEM
*&---------------------------------------------------------------------*
FORM criar_alv_viagem .

  DATA: tl_function     TYPE  ui_functions,
        wl_function     LIKE  tl_function WITH HEADER LINE,
        wl_layout       TYPE  lvc_s_layo,
        tl_estilo       TYPE lvc_t_styl WITH HEADER LINE,
        wl_estilo       TYPE lvc_s_styl,
        tabix           TYPE  sy-tabix,
        lw_setleaf      TYPE setleaf,
        vl_vinc_comboio TYPE c,
        wl_zlest0063    TYPE zlest0063.

  DATA: lt_dropdown TYPE lvc_t_drop,
        ls_dropdown TYPE lvc_s_drop.

  wl_layout-no_toolbar = c_x.

  IF ( obj_custom_viagem IS INITIAL ).

    "Perform Para Criar o Catalog do ALV da Viagem


    PERFORM: catalog_viagem.

    CREATE OBJECT obj_custom_viagem
      EXPORTING
        container_name              = 'CONTAINER_VIAGEM'
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

    CREATE OBJECT obj_grid_viagem
      EXPORTING
        i_parent          = obj_custom_viagem
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

    REFRESH gt_f4.

    gt_f4-fieldname  = 'CALADO'.
    gt_f4-register   = 'X'.
    gt_f4-getbefore  = 'X'.
    gt_f4-chngeafter = 'X'.

    APPEND gt_f4.
    CALL METHOD obj_grid_viagem->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4[].

    wl_layout-stylefname = 'ESTILO'.


    CALL METHOD obj_grid_viagem->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
      CHANGING
        it_outtab                     = gt_saida_viagem[]
        it_fieldcatalog               = gt_fcat_viagem[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL METHOD obj_grid_viagem->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
              lcl_event_handler=>on_data_changed_finished FOR obj_grid_viagem,
              lcl_event_handler=>on_f4_vg FOR obj_grid_viagem,
              lcl_event_handler=>on_data_changed FOR obj_grid_viagem.

  ELSE.

    CLEAR: gw_saida_viagem.


    LOOP AT gt_saida_viagem INTO gw_saida_viagem.
      CLEAR: tabix.
      tabix = sy-tabix.
      REFRESH: gw_saida_viagem-estilo.
      MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX tabix TRANSPORTING estilo.
    ENDLOOP.


    LOOP AT gt_saida_viagem INTO gw_saida_viagem.
      CLEAR: tabix, vl_vinc_comboio, gw_saida_viagem-estilo.
      tabix = sy-tabix.

      SELECT SINGLE *
        FROM zlest0063 INTO wl_zlest0063
       WHERE bukrs       = gw_saida_viagem-bukrs
         AND werks       = gw_saida_viagem-werks
         AND ano_viagem  = gw_saida_viagem-ano_viagem
         AND nr_viagem   = gw_saida_viagem-nr_viagem.
      IF sy-subrc = 0.
        vl_vinc_comboio = 'X'.
      ENDIF.

      SELECT SINGLE * FROM setleaf
         INTO lw_setleaf
       WHERE setname EQ 'MAGGI_ZLES0077_CALADO'
         AND valfrom EQ gw_saida_viagem-po_embarque.

      IF ( sy-subrc NE 0 ) OR ( vl_vinc_comboio IS NOT INITIAL ).
        wl_estilo-fieldname = 'CALADO'.
        wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO tl_estilo.
      ENDIF.

      IF vl_vinc_comboio IS NOT INITIAL. "NOT ( gw_saida_viagem-dt_prevista IS INITIAL ).
        wl_estilo-fieldname = 'DT_PREVISTA'.
        wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO tl_estilo.
      ENDIF.

      IF vl_vinc_comboio IS NOT INITIAL.
        wl_estilo-fieldname = 'HR_PREVISTA'.
        wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO tl_estilo.
      ENDIF.

      IF vl_vinc_comboio IS NOT INITIAL. "NOT ( gw_saida_viagem-po_destino IS INITIAL ).
        wl_estilo-fieldname = 'PO_DESTINO'.
        wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO tl_estilo.
      ENDIF.

      IF vl_vinc_comboio IS NOT INITIAL. "NOT ( gw_saida_viagem-po_embarque IS INITIAL ).
        wl_estilo-fieldname = 'PO_EMBARQUE'.
        wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO tl_estilo.
      ENDIF.

      INSERT LINES OF tl_estilo INTO TABLE gw_saida_viagem-estilo.
      MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX tabix.

    ENDLOOP.

    CALL METHOD obj_grid_viagem->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDFORM.                    " CRIAR_ALV_VIAGEM
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_VIAGEM
*&---------------------------------------------------------------------*
FORM catalog_viagem .

  REFRESH: gt_fcat_viagem[].

  "Catalogo dos campos existentes no ALV da Viagem e definição de suas propriedades
  PERFORM montar_catalog_viagem USING:

        'DT_PREVISTA' 'Data. Prev.'   '10'   '' '' '' '' '' 'X' 'ZLEST0056' 'DT_PREVISTA' 'GT_SAIDA_VIAGEM' ,
        'HR_PREVISTA' 'Hora. Prev.'   '8'    '' '' '' '' '' 'X' 'ZLEST0056' 'HR_PREVISTA' 'GT_SAIDA_VIAGEM' ,
        'PO_EMBARQUE' 'Porto Emb.'    '8'    '' '' '' '' '' 'X' 'LFA1' 'LIFNR' 'GT_SAIDA_VIAGEM' ,
        'NAME1_EMB'   'Desc.  Emb.'   '15'   '' '' '' '' '' ''  '' '' '' ,
        'ORT01_EMB'   'Cidade Emb.'   '8'    '' '' '' '' '' ''  '' '' '' ,
        'REGIO_EMB'   'UF Emb.'       '6'    '' '' '' '' '' ''  '' '' '' ,
        'PO_DESTINO'  'Porto Dest.'   '8'    '' '' '' '' '' 'X' 'KNA1' 'KUNNR' 'GT_SAIDA_VIAGEM' ,
        'NAME1_DEST'  'Desc Dest.'    '15'   '' '' '' '' '' ''  '' '' '' ,
        'ORT01_DEST'  'Cidade Dest.'  '8'    '' '' '' '' '' ''  '' '' '' ,
        'REGIO_DEST'  'UF Dest.'      '8'    '' '' '' '' '' ''  '' '' '' ,
        'DIRECAO'     'Direção'       '6'    '' '' '' '' '' ''  '' '' '' ,
        'CALADO'      'Calado'        '5'    '' '' '' '' '' 'X' '' '' '' ,
        'UND_MEDIDA'  'Un.Calado'     '10'   '' '' '' '' '' '' '' '' '' .


ENDFORM.                    " CRIAR_CATALOG_VIAGEM
*&----------------------------- ---------------------------------------*
*&      Form  MONTAR_CATALOG_VIAGEM
*&---------------------------------------------------------------------*
FORM montar_catalog_viagem  USING    VALUE(p_fieldname)
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


  CLEAR: gw_fcat_viagem.

  gw_fcat_viagem-fieldname = p_fieldname.
  gw_fcat_viagem-ref_table = p_ref_tabname..
  gw_fcat_viagem-ref_field = p_ref_fieldname.
  gw_fcat_viagem-tabname   = p_tabname.
  gw_fcat_viagem-scrtext_l = p_desc.
  gw_fcat_viagem-scrtext_m = p_desc.
  gw_fcat_viagem-scrtext_s = p_desc.
  gw_fcat_viagem-outputlen = p_tam.
  gw_fcat_viagem-no_zero   = p_no_zero.
  gw_fcat_viagem-hotspot   = p_hotspot.
  gw_fcat_viagem-emphasize = p_cor.
  gw_fcat_viagem-just      = p_just.
  gw_fcat_viagem-do_sum    = p_sum.
  gw_fcat_viagem-edit      = p_edit.

  IF p_fieldname = 'CALADO'.
    gw_fcat_viagem-f4availabl = 'X'.
  ENDIF.
*  IF P_FIELDNAME = 'UND_MEDIDA'.
*    GW_FCAT_VIAGEM-DRDN_HNDL = '1'.
*    GW_FCAT_VIAGEM-CHECKTABLE = '!'.
*  ENDIF.

  APPEND gw_fcat_viagem TO gt_fcat_viagem.

ENDFORM.                    " MONTAR_CATALOG_VIAGEM
*&---------------------------------------------------------------------*
*&      Form  ADICIONAR_LINHA_COMBOIO
*&---------------------------------------------------------------------*
FORM adicionar_linha_comboio .

  DATA: wl_saida_comboio TYPE zaqty_saida_comboio.

  "Perform Criada para adicionar 30 linhas iniciais no ALV do Comboio.
  "Para a programação do comboio
  READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX 1.
  IF sy-subrc = 0.
    IF novo = 'X'.
      MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Gravar dados da viagem.'.
      REFRESH gt_saida_comboio.
    ELSE.
      PERFORM verifica_erros_viagem.
      IF tg_msg_ret[] IS NOT INITIAL.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Viagem contém erros.'.
        REFRESH gt_saida_comboio.
      ELSEIF gt_saida_comboio[] IS INITIAL.

        op_modo = c_edit.

        DO 4 TIMES.
          wl_saida_comboio-usuario        = sy-uname.
          wl_saida_comboio-data_registro  = sy-datum.
          wl_saida_comboio-hora_registro  = sy-uzeit.
          "          WL_SAIDA_COMBOIO-UNID_MEDIDA    = GW_SAIDA_VIAGEM-UND_MEDIDA.

          APPEND wl_saida_comboio TO gt_saida_comboio.
          CLEAR: wl_saida_comboio.
        ENDDO.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Informe dados da viagem.'.
    REFRESH gt_saida_comboio.
  ENDIF.

ENDFORM.                    " ADICIONAR_LINHA_COMBOIO
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_COMBOIO
*&---------------------------------------------------------------------*
FORM selecionar_dados_comboio .

  DATA: tl_zlest0063 TYPE TABLE OF zlest0063, "tabela de Comboios - Frete Aquaviário - Viagem
        wl_zlest0063 TYPE          zlest0063, "Work Area tabela de Comboios - Frete Aquaviário - Viagem
        tl_zlest0060 TYPE TABLE OF zlest0060,
        wl_zlest0060 TYPE zlest0060,
        wl_makt      TYPE makt,
        tabix        TYPE sy-tabix.

  DATA: total TYPE zlest0057-peso_vinculado.

  "PERFORM F_CENTRO_CALC_RET USING W_WERKS. "Adequações Retenção CS2016001199

  REFRESH: tl_zlest0063[], tl_zlest0060[].

  SELECT *
    FROM zlest0063
    INTO TABLE tl_zlest0063
     WHERE bukrs      EQ w_bukrs
       AND werks      EQ w_werks
       AND ano_viagem EQ w_ano
       AND nr_viagem  EQ w_viagem.

  SELECT * FROM zlest0060
    INTO TABLE tl_zlest0060
    FOR ALL ENTRIES IN tl_zlest0063
  WHERE bukrs      EQ tl_zlest0063-bukrs
    AND werks      EQ tl_zlest0063-werks
    AND ano_viagem EQ tl_zlest0063-ano_viagem
    AND nome_emb   EQ tl_zlest0063-nome_emb
    AND nr_viagem  EQ tl_zlest0063-nr_viagem.


  REFRESH  gt_saida_comboio.

  LOOP AT tl_zlest0063 INTO wl_zlest0063.

    CLEAR: gw_saida_comboio.

    tabix = sy-tabix.

    wg_acao_cb = 'OK'.

    CASE wl_zlest0063-embarcacao.
      WHEN: 'B'.
        gw_saida_comboio-embarcacao   = 'B-Barcaça'.
      WHEN: 'E'.
        gw_saida_comboio-embarcacao   = 'E-Empurrador'.
      WHEN: 'R'.
        gw_saida_comboio-embarcacao   = 'R-Rebocador'.
    ENDCASE.

    gw_saida_comboio-nome       = wl_zlest0063-nome_emb.

    CASE wl_zlest0063-tp_barcaca.
      WHEN: 'A'.
        gw_saida_comboio-tp_barcaca   = 'A-Acoplável'.
      WHEN: 'B'.
        gw_saida_comboio-tp_barcaca   = 'B-Box'.
      WHEN: 'P'.
        gw_saida_comboio-tp_barcaca   = 'P-Proa'.
      WHEN: 'M'.
        gw_saida_comboio-tp_barcaca   = 'M-Mineralizadora'.
    ENDCASE.

    gw_saida_comboio-matnr          = wl_zlest0063-cod_material.

*Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - INICIO
    IF area_parametro_filial_eudr IS NOT INITIAL.

      CASE wl_zlest0063-eudr.
        WHEN: 'S'.
          gw_saida_comboio-eudr = 'S-Atende EUDR'.
        WHEN: 'N'.
          gw_saida_comboio-eudr = 'N-Não atende EUDR'.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
*Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - FIM

    SELECT SINGLE *
      FROM makt
      INTO wl_makt
      WHERE matnr = wl_zlest0063-cod_material
        AND spras EQ sy-langu.

    gw_saida_comboio-maktx          = wl_makt-maktx.

    CASE wl_zlest0063-tp_class.
      WHEN: 'CO'.
        gw_saida_comboio-tp_class = 'CO-Convencional'.
      WHEN: 'R1'.
        gw_saida_comboio-tp_class = 'R1-R2-Transgênico'.
        "WHEN: 'R2'.
        "  GW_SAIDA_COMBOIO-TP_CLASS = 'R2-RR2'.
    ENDCASE.

    CASE wl_zlest0063-un_medida.
      WHEN: 'KG'.
        IF ( wl_zlest0063-embarcacao(1) EQ 'B' ).
          gw_saida_comboio-unid_medida = 'KG-Quilograma'.
        ENDIF.
      WHEN: 'TO'.
        IF ( wl_zlest0063-embarcacao(1) EQ 'B' ).
          gw_saida_comboio-unid_medida = 'To-Tonelada'.
        ENDIF.
    ENDCASE.
    gw_saida_comboio-matkl = wl_zlest0063-gr_material.
    gw_saida_comboio-peso_previsto   =  wl_zlest0063-peso_previsto.



    IF ( wl_zlest0063-embarcacao(1) EQ 'B' ).

      LOOP AT tl_zlest0060 INTO wl_zlest0060 WHERE bukrs      EQ wl_zlest0063-bukrs
                                               AND werks      EQ wl_zlest0063-werks
                                               AND ano_viagem EQ wl_zlest0063-ano_viagem
                                               AND nome_emb   EQ wl_zlest0063-nome_emb.

        IF wl_zlest0060-peso_liq_ret IS NOT INITIAL.
          total = total + wl_zlest0060-peso_liq_ret.
        ELSE.
          total = total + wl_zlest0060-peso_fiscal.
        ENDIF.

      ENDLOOP.

      gw_saida_comboio-peso_vinculado  = total.
      CLEAR: total.

    ENDIF.



    "Status do Peso Previsto vs Peso Vinculado
    REFRESH: tl_estilo_cell.
    CLEAR: wl_estilo_cell.

    IF ( gw_saida_comboio-peso_vinculado EQ gw_saida_comboio-peso_previsto ) AND ( wl_zlest0063-embarcacao(1) EQ 'B' ).

      wl_estilo_cell-fname = 'STATUS_ICON'.
      wl_estilo_cell-color-col   = '5'.
      wl_estilo_cell-color-int   = '0'.
      wl_estilo_cell-color-inv   = '0'.
      APPEND wl_estilo_cell TO tl_estilo_cell.
      INSERT LINES OF tl_estilo_cell INTO TABLE gw_saida_comboio-estilo_cell.

      gw_saida_comboio-status_icon = icon_unspecified_four.

    ELSEIF ( gw_saida_comboio-peso_vinculado < gw_saida_comboio-peso_previsto ) AND ( wl_zlest0063-embarcacao(1) EQ 'B' ).

      wl_estilo_cell-fname = 'STATUS_ICON'.
      wl_estilo_cell-color-col   = '3'.
      wl_estilo_cell-color-int   = '0'.
      wl_estilo_cell-color-inv   = '0'.
      APPEND wl_estilo_cell TO tl_estilo_cell.
      INSERT LINES OF tl_estilo_cell INTO TABLE gw_saida_comboio-estilo_cell.

      gw_saida_comboio-status_icon = icon_unspecified_three.


    ELSEIF ( gw_saida_comboio-peso_vinculado > gw_saida_comboio-peso_previsto ) AND ( wl_zlest0063-embarcacao(1) EQ 'B' ).

      wl_estilo_cell-fname = 'STATUS_ICON'.
      wl_estilo_cell-color-col   = '6'.
      wl_estilo_cell-color-int   = '0'.
      wl_estilo_cell-color-inv   = '0'.
      APPEND wl_estilo_cell TO tl_estilo_cell.
      INSERT LINES OF tl_estilo_cell INTO TABLE gw_saida_comboio-estilo_cell.

      gw_saida_comboio-status_icon = icon_unspecified_one.


    ENDIF.

    SELECT SINGLE *
      FROM zlest0060 INTO @DATA(wl_0060)
     WHERE bukrs       EQ @wl_zlest0063-bukrs
       AND werks       EQ @wl_zlest0063-werks
       AND ano_viagem  EQ @wl_zlest0063-ano_viagem
       AND nr_viagem   EQ @wl_zlest0063-nr_viagem
       AND nome_emb    EQ @wl_zlest0063-nome_emb.

    IF ( sy-subrc = 0 ) OR ( op_modo NE c_edit ).

      REFRESH: tl_estilo_comb, tl_rows.
      CLEAR: wl_estilo_comb, sl_rows.

      wl_estilo_comb-fieldname = 'EMBARCACAO'.
      wl_estilo_comb-style = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wl_estilo_comb TO tl_estilo_comb.

      CLEAR: wl_estilo_comb.
      wl_estilo_comb-fieldname = 'NOME'.
      wl_estilo_comb-style = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wl_estilo_comb TO tl_estilo_comb.

      CLEAR: wl_estilo_comb.
      wl_estilo_comb-fieldname = 'TP_CLASS'.
      wl_estilo_comb-style = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wl_estilo_comb TO tl_estilo_comb.

      INSERT LINES OF tl_estilo_comb INTO TABLE gw_saida_comboio-estilo.

      REFRESH: tl_estilo_comb.

      "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - INICIO
      CLEAR: wl_estilo_comb.
      wl_estilo_comb-fieldname = 'EUDR'.
      wl_estilo_comb-style = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wl_estilo_comb TO tl_estilo_comb.
      "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - FIM

      CLEAR: wl_estilo_comb.
      wl_estilo_comb-fieldname = 'MATNR'.
      wl_estilo_comb-style = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wl_estilo_comb TO tl_estilo_comb.

      INSERT LINES OF tl_estilo_comb INTO TABLE gw_saida_comboio-estilo.

      gw_saida_comboio-status = wl_zlest0063-status.

    ENDIF.

    APPEND gw_saida_comboio TO gt_saida_comboio.
    CLEAR: gw_saida_comboio, wl_makt, wl_zlest0057, wl_zlest0063, wl_estilo_comb.
    REFRESH: tl_estilo_comb.
  ENDLOOP.


ENDFORM.                    " SELECIONAR_DADOS_COMBOIO

*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_COMBOIO
*&---------------------------------------------------------------------*
FORM criar_alv_comboio .

  DATA: tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        wl_layout   TYPE lvc_s_layo,
        wl_stable   TYPE lvc_s_stbl.

  DATA: lt_dropdown TYPE lvc_t_drop,
        ls_dropdown TYPE lvc_s_drop.

  CLEAR: tl_function, wl_function, wl_layout.

  IF obj_custom_comboio IS INITIAL.
    PERFORM: criar_catalog_comboio.

    CREATE OBJECT obj_custom_comboio
      EXPORTING
        container_name              = 'CONTAINER_COMBOIO'
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

    CREATE OBJECT obj_grid_comboio
      EXPORTING
        i_parent          = obj_custom_comboio
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

    CREATE OBJECT obj_toolbar_cb
      EXPORTING
        io_alv_grid = obj_grid_comboio.

*      * Register event handler
    SET HANDLER obj_toolbar_cb->on_toolbar FOR obj_grid_comboio.
    SET HANDLER obj_toolbar_cb->handle_user_command FOR obj_grid_comboio.

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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_mb_filter.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.


** First listbox (handle '1').
    ls_dropdown-handle = '1'.
    ls_dropdown-value = 'B-Barcaça'.
    APPEND ls_dropdown TO lt_dropdown.

    ls_dropdown-handle = '1'.
    ls_dropdown-value = 'E-Empurrador'.
    APPEND ls_dropdown TO lt_dropdown.

    ls_dropdown-handle = '1'.
    ls_dropdown-value = 'R-Rebocador'.
    APPEND ls_dropdown TO lt_dropdown.

    "Tipo Class
    ls_dropdown-handle = '2'.
    ls_dropdown-value = 'CO-Convencional'.
    APPEND ls_dropdown TO lt_dropdown.

    ls_dropdown-handle = '2'.
    ls_dropdown-value = 'R1 - RR'.
    APPEND ls_dropdown TO lt_dropdown.

    ls_dropdown-handle = '2'.
    ls_dropdown-value  = 'R2 - RR2'.
    APPEND ls_dropdown TO lt_dropdown.

    " Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - INICIO
    "EUDR
    ls_dropdown-handle = '3'.
    ls_dropdown-value  = 'S - Atende'.
    APPEND ls_dropdown TO lt_dropdown.

    ls_dropdown-handle = '3'.
    ls_dropdown-value  = 'N - Não Atende'.
    APPEND ls_dropdown TO lt_dropdown.
    "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - FIM

    CALL METHOD obj_grid_comboio->set_drop_down_table
      EXPORTING
        it_drop_down = lt_dropdown.

    wl_layout-ctab_fname = 'ESTILO_CELL'.
    wl_layout-stylefname = 'ESTILO'.

    CALL METHOD obj_grid_comboio->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
        "I_SAVE                        = 'A'
        it_toolbar_excluding          = tl_function
      CHANGING
        it_outtab                     = gt_saida_comboio[]
        it_fieldcatalog               = gt_fcat_comboio[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD obj_grid_comboio->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    " Register event handler
    REFRESH gt_f4.
    gt_f4-fieldname  = 'NOME'.
    gt_f4-register   = 'X'.
    gt_f4-getbefore  = 'X'.
    gt_f4-chngeafter = 'X'.
    APPEND gt_f4.

    CALL METHOD obj_grid_comboio->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4[].

    SET HANDLER:
              lcl_event_handler=>on_data_changed_cb FOR obj_grid_comboio,
              lcl_event_handler=>on_f4_cb FOR obj_grid_comboio,
              lcl_event_handler=>on_data_changed_finished_cb FOR obj_grid_comboio.

  ELSE.
*    PERFORM: CRIAR_CATALOG_COMBOIO.
*    CALL METHOD OBJ_GRID_COMBOIO->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = GT_FCAT_COMBOIO[].
    CALL METHOD obj_grid_comboio->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.                    " CRIAR_ALV_COMBOIO
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG
*&---------------------------------------------------------------------*
FORM criar_catalog_comboio .

  REFRESH gt_fcat_comboio.
  "  Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - INICIO
  IF area_parametro_filial_eudr IS NOT INITIAL.
    PERFORM montar_catalog_comboio USING:
        'EMBARCACAO'     'Tipo. Emb.'      '12'   '' '' '' '' '' 'X' '' ''  '' '' ,
        'NOME'           'Nome Emb.'       '15'   '' '' '' '' '' 'X' ''  ''  '' '' ,
        'TP_BARCACA'     'Tipo Barcaça.'   '8'    '' '' '' '' '' ' ' '' ''  '' '' ,
        'MATNR'          'Cód. Material'   '6'    'X' '' '' '' '' 'X' 'MAKT' 'MATNR'  '' '' ,
        'MAKTX'          'Desc. Material'  '15'   '' '' '' '' '' ''  '' '' '' '' ,
        'TP_CLASS'       'Class. Produto'  '15'   '' '' '' '' '' 'X' '' '' '' '' ,
        'EUDR'           'Atende EUDR'            '15'   '' '' '' '' '' 'X' '' '' '' '' ,
        'UNID_MEDIDA'    'Unid.Medida'     '10'   '' '' '' '' '' ' ' '' '' '' '' ,
        'PESO_PREVISTO'  'Peso Previsto.'  '15'   '' '' '' '' 'X' '' '' ''  '' '' ,
        'PESO_VINCULADO' 'Peso Vinculado.' '15'   '' '' '' '' 'X' ''  '' '' '' '' ,
        'STATUS_ICON'     'Status'          '7'    '' '' '' 'C' '' ''  '' '' '' ''.
  ELSE.
    PERFORM montar_catalog_comboio USING:
          'EMBARCACAO'     'Tipo. Emb.'      '12'   '' '' '' '' '' 'X' '' ''  '' '' ,
          'NOME'           'Nome Emb.'       '15'   '' '' '' '' '' 'X' ''  ''  '' '' ,
          'TP_BARCACA'     'Tipo Barcaça.'   '8'    '' '' '' '' '' ' ' '' ''  '' '' ,
          'MATNR'          'Cód. Material'   '6'    'X' '' '' '' '' 'X' 'MAKT' 'MATNR'  '' '' ,
          'MAKTX'          'Desc. Material'  '15'   '' '' '' '' '' ''  '' '' '' '' ,
          'TP_CLASS'       'Class. Produto'  '15'   '' '' '' '' '' 'X' '' '' '' '' ,
          'UNID_MEDIDA'    'Unid.Medida'     '10'   '' '' '' '' '' ' ' '' '' '' '' ,
          'PESO_PREVISTO'  'Peso Previsto.'  '15'   '' '' '' '' 'X' '' '' ''  '' '' ,
          'PESO_VINCULADO' 'Peso Vinculado.' '15'   '' '' '' '' 'X' ''  '' '' '' '' ,
          'STATUS_ICON'     'Status'          '7'    '' '' '' 'C' '' ''  '' '' '' ''.
  ENDIF.
  " Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - fim
ENDFORM.                    " CRIAR_CATALOG
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_COMBOIO
*&---------------------------------------------------------------------*
FORM montar_catalog_comboio  USING   VALUE(p_fieldname)
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
                                     VALUE(p_tabname)       LIKE dd02d-tabname
                                     VALUE(p_check).


  CLEAR: gw_fcat_comboio.

  gw_fcat_comboio-fieldname = p_fieldname.
  gw_fcat_comboio-ref_table = p_ref_tabname..
  gw_fcat_comboio-ref_field = p_ref_fieldname.
  gw_fcat_comboio-tabname   = p_tabname.
  gw_fcat_comboio-scrtext_l = p_desc.
  gw_fcat_comboio-scrtext_m = p_desc.
  gw_fcat_comboio-scrtext_s = p_desc.
  gw_fcat_comboio-outputlen = p_tam.
  gw_fcat_comboio-no_zero   = p_no_zero.
  gw_fcat_comboio-hotspot   = p_hotspot.
  gw_fcat_comboio-emphasize = p_cor.
  gw_fcat_comboio-just      = p_just.
  gw_fcat_comboio-do_sum    = p_sum.
  gw_fcat_comboio-edit      = p_edit.
  gw_fcat_comboio-checkbox  = p_check.

  CASE p_fieldname.
    WHEN: 'EMBARCACAO'.
      gw_fcat_comboio-drdn_hndl = '1'.
      gw_fcat_comboio-checktable = '!'.
    WHEN: 'TP_CLASS'.
      gw_fcat_comboio-drdn_hndl = '2'.
      gw_fcat_comboio-checktable = '!'.
    WHEN: 'NOME' OR 'MATNR'.
      gw_fcat_comboio-f4availabl = 'X'.

      "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
    WHEN: 'EUDR'.
      gw_fcat_comboio-drdn_hndl = '3'.
      gw_fcat_comboio-checktable = '!'.
      "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
  ENDCASE.

  APPEND gw_fcat_comboio TO gt_fcat_comboio.

ENDFORM.                    " MONTAR_CATALOG_COMBOIO



*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_ROMANEIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM criar_catalog_romaneio.

  DATA: gw_setleaf TYPE setleaf.

  CLEAR: gw_setleaf.

  SELECT SINGLE * FROM setleaf
    INTO gw_setleaf
  WHERE setname EQ 'MAGGI_ZLES0077_IMPORT'
    AND valfrom EQ w_werks.

  REFRESH: gt_fcat_romaneio[].

  PERFORM montar_catalog_romaneio USING:
       'DT_MOVIMENTO'   'Dt.Entrada'      '10'   '' '' '' '' '' ''  '' ''  '' '',
       'NR_ROMANEIO'    'Romaneio'        '08'   '' '' '' '' '' ''  '' ''  '' '' ,
       'DOCDAT'         'Dt.Nf.'          '10'   '' '' '' '' '' ''  '' ''  '' '',
       'NFNUM'          'Nr.Nf.'          '9'    '' '' '' '' '' ''  '' ''  '' '',
       'SERIES'         'Série Nf.'       '3'    '' '' '' '' '' ''  '' ''  '' '',
       'NR_DCO'         'Nr.DCO.'         '9'    'X' '' '' '' '' '' '' ''  '' '',
       'SAFRA'          'Safra'           '5'    '' '' '' '' '' ''  '' ''  '' '',
       'XML'            'XML'             '3'   '' '' '' 'C' '' '' '' ''  '' ''.

  "IF WG_ACAO NE 'VIS_ROMANEIO'.
  PERFORM montar_catalog_romaneio USING:
          'CHECK'          'Check'           '5'    '' '' '' '' '' 'X' '' ''  '' 'X'.
  "ENDIF.

  PERFORM montar_catalog_romaneio USING:
    'NETWR'             'Valor Nf.'       '15'   '' '' '' '' 'X' '' '' ''  '' '',
    'PESO_FISCAL'       'Peso Nf.'        '15'   '' '' '' '' 'X' '' '' ''  '' '',
    'PESO_SUBTOTAL'     'Peso.Liq.'       '15'   '' '' '' '' 'X' '' '' ''  '' '',
    'PESO_LIQ_RET'      'Peso.Útil'       '15'   '' '' '' '' 'X' '' '' ''  '' ''.

  "17.01.2018 - CS201800007
  PERFORM montar_catalog_romaneio USING:
     'PESO_UTIL_VINC '  'Peso Vinc.'      '15'   '' '' '' '' 'X' ''  '' ''  '' '',
     'VALOR_UTIL_VINC'  'Valor Vinc.'     '15'   '' '' '' '' 'X' ''  '' ''  '' ''.

  "Adequações Retenção CS2016001199
  "IF VG_CALC_RETENCAO IS NOT INITIAL.
  "  PERFORM MONTAR_CATALOG_ROMANEIO USING:
  "    'VALOR_LIQ_RET_VINC' 'Valor Vinc.'       '15'   '' '' '' '' 'X' '' '' ''  '' ''.
  "ELSE.
  "  PERFORM MONTAR_CATALOG_ROMANEIO USING:
  "    'VALOR_VINCULADO' 'Valor Vinc.'       '15'   '' '' '' '' 'X' '' '' ''  '' ''.
  "ENDIF.

  "Adequações Retenção CS2016001199
*  IF VG_CALC_RETENCAO IS NOT INITIAL. "17.01.2018 - CS2018000076
*    PERFORM MONTAR_CATALOG_ROMANEIO USING:
*             'PESO_LIQ_RET_VINC'       'Peso Vinc.'      '15'   '' '' '' '' 'X' ' ' '' ''  '' ''.
  "ELSE.
*    IF NOT GW_SETLEAF IS INITIAL.
*      PERFORM MONTAR_CATALOG_ROMANEIO USING:
*          'PESO_VINC'       'Peso Vinc.'      '15'   '' '' '' '' 'X' 'X' '' ''  '' ''.
*    ELSE.
*      PERFORM MONTAR_CATALOG_ROMANEIO USING:
*          'PESO_VINC'       'Peso Vinc.'      '15'   '' '' '' '' 'X' '' '' ''  '' ''.
*
*    ENDIF.
*  ENDIF.


ENDFORM.                    "CRIAR_CATALOG_ROMANEIO

*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_COMBOIO
*&---------------------------------------------------------------------*
FORM montar_catalog_romaneio USING   VALUE(p_fieldname)
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
                                     VALUE(p_tabname)       LIKE dd02d-tabname
                                     VALUE(p_check).

  CLEAR: gw_fcat_romaneio.

  gw_fcat_romaneio-fieldname = p_fieldname.
  gw_fcat_romaneio-ref_table = p_ref_tabname..
  gw_fcat_romaneio-ref_field = p_ref_fieldname.
  gw_fcat_romaneio-tabname   = p_tabname.
  gw_fcat_romaneio-scrtext_l = p_desc.
  gw_fcat_romaneio-scrtext_m = p_desc.
  gw_fcat_romaneio-scrtext_s = p_desc.
  gw_fcat_romaneio-outputlen = p_tam.
  gw_fcat_romaneio-no_zero   = p_no_zero.
  gw_fcat_romaneio-hotspot   = p_hotspot.
  gw_fcat_romaneio-emphasize = p_cor.
  gw_fcat_romaneio-just      = p_just.
  gw_fcat_romaneio-do_sum    = p_sum.
  gw_fcat_romaneio-edit      = p_edit.
  gw_fcat_romaneio-checkbox  = p_check.

  APPEND gw_fcat_romaneio  TO gt_fcat_romaneio.


ENDFORM.                    " MONTAR_CATALOG_COMBOIO

*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_VINC_NF
*&---------------------------------------------------------------------*
FORM criar_alv_vinc_nf .

  DATA: tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        wl_layout   TYPE lvc_s_layo,
        wl_stable   TYPE lvc_s_stbl,
        tl_rows     TYPE lvc_t_row,
        sl_rows     TYPE lvc_s_row.

  CLEAR: tl_function, wl_function, wl_layout.

  "Recupera qual é a linha selecionada do comboio
  CLEAR: gw_saida_comboio.

  CALL METHOD obj_grid_comboio->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.
  READ TABLE tl_rows INDEX 1 INTO sl_rows.
  IF ( sy-subrc NE 0 ).
    sl_rows-index = index_comboio.
    READ TABLE gt_saida_comboio INDEX sl_rows-index INTO gw_saida_comboio.
  ELSE.
    READ TABLE gt_saida_comboio INDEX sl_rows-index INTO gw_saida_comboio.
  ENDIF.

  CONCATENATE gw_saida_comboio-nome  '-' gw_saida_comboio-maktx INTO wl_layout-grid_title.

  wl_layout-sel_mode   = 'A'.

  IF obj_custom_vinc_nf IS INITIAL.

    PERFORM: criar_catalog_vinc_nf.

    CREATE OBJECT obj_custom_vinc_nf
      EXPORTING
        container_name              = 'CONTAINER_VINC_NF'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT obj_grid_vinc_nf
      EXPORTING
        i_parent          = obj_custom_vinc_nf
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT obj_toolbar_vn
      EXPORTING
        io_alv_grid = obj_grid_vinc_nf.

    " Register event handler
    SET HANDLER obj_toolbar_vn->on_toolbar FOR obj_grid_vinc_nf.
    SET HANDLER obj_toolbar_vn->handle_user_command FOR obj_grid_vinc_nf.


    CALL METHOD obj_grid_vinc_nf->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
      CHANGING
        it_outtab                     = gt_saida_vinc_nf_nome[]
        it_fieldcatalog               = gt_fcat_vinc_nf[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.

    CALL METHOD obj_grid_vinc_nf->set_frontend_layout
      EXPORTING
        is_layout = wl_layout.

    CALL METHOD obj_grid_vinc_nf->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDFORM.                    " CRIAR_ALV_VINC_NF

*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_VINC_NF
*&---------------------------------------------------------------------*
FORM criar_catalog_vinc_ge .

  REFRESH: gt_fcat_vinc_ge[].
  "Catalogo dos campos existentes no ALV da Vincular Nota Fiscal e definição de suas propriedades
  PERFORM montar_catalog_vinc_ge USING:

        'NOME'           'Nome Embarcação' '12'   '' '' '' '' '' ''  '' '' '' '' ,
        'DT_MOVIMENTO'   'Dt. Movimento'   '10'   '' '' '' '' '' ''  '' '' '' '' ,
        'DOCNUM'          'Docnum'          '9'   '' '' '' '' '' ''  '' '' '' '',
        'NFENUM'          'CT-e/NF'         '9'   'X' '' '' '' '' ''  '' '' '' '',
        'SERIE'           'Serie'           '4'   '' '' '' '' '' ''  '' '' '' '',
        'DT_FATURA'       'Dt.Fatura'       '10'   '' '' '' '' '' ''  '' '' '' '',
        'PESO_VINCULADO'  'Peso Vinc.'      '10'   '' '' '' '' '' ''  '' '' '' '',
        'VALOR_VINCULADO' 'Valor Vinc.'     '10'   '' '' '' '' '' ''  '' '' '' '',
        'CL_CODIGO'       'Cliente'         '10'   'X' '' '' '' '' ''  '' '' '' '',
        'NAME1'           'Desc. Cliente'   '10'   '' '' '' '' '' ''  '' '' '' '',
        'STCD1'           'CNPJ Cliente'    '10'   '' '' '' '' '' ''  '' '' '' '',
        'TP_CLASS'        'Class. Produto'  '10'   '' '' '' '' '' ''  '' '' '' '',
        'MAKTX'           'Material'        '10'   '' '' '' '' '' ''  '' '' '' '',
        'SAFRA'           'Safra'           '10'   '' '' '' '' '' ''  '' '' '' '',
        'NR_DCO'          'Nr. DCO'         '10'   '' '' '' '' '' ''  '' '' '' '',
        'VLR_USD'         'Vlr. USD'        '10'   '' '' '' '' '' ''  '' '' '' '',
        'VLR_BRL'         'Vlr. BRL'        '10'   '' '' '' '' '' ''  '' '' '' '',
        'TAX_DOLAR'       'Taxa'            '10'   '' '' '' '' '' ''  '' '' '' ''.

ENDFORM.                    " CRIAR_CATALOG_VINC_GE
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_VINC_NF
*&---------------------------------------------------------------------*
FORM montar_catalog_vinc_ge   USING  VALUE(p_fieldname)
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
                                     VALUE(p_tabname)       LIKE dd02d-tabname
                                     VALUE(p_check).

  CLEAR: gw_fcat_vinc_ge.

  gw_fcat_vinc_ge-fieldname = p_fieldname.
  gw_fcat_vinc_ge-ref_table = p_ref_tabname..
  gw_fcat_vinc_ge-ref_field = p_ref_fieldname.
  gw_fcat_vinc_ge-tabname   = p_tabname.
  gw_fcat_vinc_ge-scrtext_l = p_desc.
  gw_fcat_vinc_ge-scrtext_m = p_desc.
  gw_fcat_vinc_ge-scrtext_s = p_desc.
  gw_fcat_vinc_ge-outputlen = p_tam.
  gw_fcat_vinc_ge-no_zero   = p_no_zero.
  gw_fcat_vinc_ge-hotspot   = p_hotspot.
  gw_fcat_vinc_ge-emphasize = p_cor.
  gw_fcat_vinc_ge-just      = p_just.
  gw_fcat_vinc_ge-do_sum    = p_sum.
  gw_fcat_vinc_ge-edit      = p_edit.
  gw_fcat_vinc_ge-checkbox  = p_check.

  APPEND gw_fcat_vinc_ge TO gt_fcat_vinc_ge.
ENDFORM.                    " MONTAR_CATALOG_VINC_ge


*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_GER_OV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM criar_alv_ger_ov .

  DATA: tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        wl_layout   TYPE lvc_s_layo,
        wl_stable   TYPE lvc_s_stbl.

  CLEAR: tl_function, wl_function, wl_layout.

  IF obj_grid_ger_ov IS INITIAL.

    PERFORM: criar_catalog_ger_ov.

    IF obj_custom_ger_ov IS INITIAL.
      CREATE OBJECT obj_custom_ger_ov
        EXPORTING
          container_name              = 'CONTAINER_GER_OV'
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
    ENDIF.

    CREATE OBJECT obj_grid_ger_ov
      EXPORTING
        i_parent          = obj_custom_ger_ov
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

    CREATE OBJECT obj_toolbar_ov
      EXPORTING
        io_alv_grid = obj_grid_ger_ov.

*      * Register event handler
    SET HANDLER obj_toolbar_ov->on_toolbar FOR obj_grid_ger_ov.
    SET HANDLER obj_toolbar_ov->handle_user_command FOR obj_grid_ger_ov.


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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_mb_filter.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.

    wl_layout-stylefname = 'ESTILO'.

    CALL METHOD obj_grid_ger_ov->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
      CHANGING
        it_outtab                     = gt_saida_ger_ov[]
        it_fieldcatalog               = gt_fcat_ger_ov[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD obj_grid_ger_ov->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_grid_ger_ov->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    REFRESH gt_f4.
    gt_f4-fieldname  = 'AUART'.
    gt_f4-register   = 'X'.
    gt_f4-getbefore  = 'X'.
    gt_f4-chngeafter ='X'.
    APPEND gt_f4.

    CALL METHOD obj_grid_ger_ov->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4[].

    SET HANDLER:
              lcl_event_handler=>on_data_changed_ov FOR obj_grid_ger_ov,
              lcl_event_handler=>on_f4_ov FOR obj_grid_ger_ov,
              lcl_event_handler=>handle_hotspot_click FOR obj_grid_ger_ov.


  ELSE.
*    PERFORM: CRIAR_CATALOG_GER_OV.
*    CALL METHOD OBJ_GRID_GER_OV->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = GT_FCAT_GER_OV[].
    CALL METHOD obj_grid_ger_ov->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.                    " CRIAR_ALV_VINC_NF
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_ROmaneio
*&---------------------------------------------------------------------*
FORM criar_alv_romaneio .

  DATA: tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        wl_layout   TYPE lvc_s_layo,
        wl_stable   TYPE lvc_s_stbl,
        wl_variant  TYPE disvariant.

  DATA: vpeso_vinculado     TYPE zlest0057-peso_vinculado,
        speso_vinculado(20).

  CLEAR: tl_function, wl_function, wl_layout.
  wl_layout-zebra       = c_x.
  wl_layout-no_rowmark  = c_x.
  wl_stable-row         = c_x.
  wl_layout-sel_mode    = 'A'.
  "WL_LAYOUT-CWIDTH_OPT  = 'X'.

  IF obj_custom_romaneio IS INITIAL.
    PERFORM: criar_catalog_romaneio.

    wl_stable = 'X'.

    CREATE OBJECT obj_custom_romaneio
      EXPORTING
        container_name              = 'CC_VINC_NF'
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

    CREATE OBJECT obj_grid_romaneio
      EXPORTING
        i_parent          = obj_custom_romaneio
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

    CREATE OBJECT obj_toolbar_ro
      EXPORTING
        io_alv_grid = obj_grid_romaneio.

    SET HANDLER:
              lcl_event_handler=>on_data_changed_ro          FOR obj_grid_romaneio,
              lcl_event_handler=>on_data_changed_finished_ro FOR obj_grid_romaneio,
              obj_toolbar_ro->on_toolbar                     FOR obj_grid_romaneio,
              obj_toolbar_ro->handle_user_command            FOR obj_grid_romaneio.

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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_mb_filter.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.

    "WL_LAYOUT-NO_TOOLBAR = 'X'.

    wl_variant-report   = sy-repid.
    wl_variant-username = sy-uname.
    wl_layout-stylefname = 'ESTILO'.
    wl_layout-ctab_fname = 'COLOR'.

    CALL METHOD obj_grid_romaneio->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
        "I_SAVE                        = 'A'
        it_toolbar_excluding          = tl_function
        "IS_VARIANT                    = WL_VARIANT
      CHANGING
        it_outtab                     = gt_saida_vinc_rom_nome[]
        it_fieldcatalog               = gt_fcat_romaneio[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD obj_grid_romaneio->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_grid_romaneio->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.

*    PERFORM: CRIAR_CATALOG_ROMANEIO.
*
*    CALL METHOD OBJ_GRID_ROMANEIO->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = GT_FCAT_ROMANEIO[].

    wa_stable-row = 'X'.
    wa_stable-col = 'X'.
    CALL METHOD obj_grid_romaneio->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
    "        I_SOFT_REFRESH = 'X'.

    CLEAR: wa_stable.
  ENDIF.

ENDFORM.                    " CRIAR_ALV_VINC_NF
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_NOTAS_FISCAIS
*&---------------------------------------------------------------------*
FORM seleciona_notas_fiscais." USING P_SAIDA_COMBOIO TYPE TY_SAIDA_COMBOIO.

  DATA: tl_zlest0060      TYPE TABLE OF zlest0060,
        wl_zlest0060      TYPE zlest0060,
        tl_zlest0060_aux  TYPE TABLE OF zlest0060,
        wl_zlest0060_aux  TYPE zlest0060,
        wl_zlest0061      TYPE zlest0061,
        wl_j_1bnfe_active TYPE j_1bnfe_active.

  DATA: tabix TYPE sy-tabix.

  DATA: wl_kna1 TYPE kna1,
        wl_lfa1 TYPE lfa1.

  REFRESH: tl_zlest0060[], tl_zlest0060_aux[], gt_saida_vinc_nf_nome[].
  CLEAR: gw_saida_vinc_nf_nome, wl_zlest0061.

  "PERFORM F_CENTRO_CALC_RET USING W_WERKS. "Adequações Retenção CS2016001199

  IF NOT ( index_comboio IS INITIAL ).

    READ TABLE gt_saida_comboio INDEX index_comboio INTO gw_saida_comboio.

    SELECT * FROM zlest0060
      INTO TABLE tl_zlest0060
    WHERE bukrs      EQ w_bukrs
      AND werks      EQ w_werks
      AND ano_viagem EQ w_ano
      AND nr_viagem  EQ w_viagem
      AND embarcacao EQ gw_saida_comboio-embarcacao
      AND nome_emb   EQ gw_saida_comboio-nome.

    CHECK NOT tl_zlest0060[] IS INITIAL.

    tl_zlest0060_aux[] = tl_zlest0060[].

    LOOP AT tl_zlest0060_aux INTO wl_zlest0060_aux .

      tabix = sy-tabix.

      LOOP AT tl_zlest0060 INTO wl_zlest0060 WHERE rm_codigo      = wl_zlest0060_aux-rm_codigo "cl_codigo = wl_zlest0060_aux-cl_codigo
                                               AND safra          = wl_zlest0060_aux-safra
                                               AND nr_dco         = wl_zlest0060_aux-nr_dco
                                               AND operacao       = wl_zlest0060_aux-operacao
                                               AND id_frete_aqua  = wl_zlest0060_aux-id_frete_aqua
                                               AND matnr          = wl_zlest0060_aux-matnr.

        IF wl_zlest0060-peso_liq_ret IS NOT INITIAL.
          gw_saida_vinc_nf_nome-peso_vinculado = gw_saida_vinc_nf_nome-peso_vinculado + wl_zlest0060-peso_liq_ret.
        ELSE.
          gw_saida_vinc_nf_nome-peso_vinculado = gw_saida_vinc_nf_nome-peso_vinculado + wl_zlest0060-peso_fiscal.
        ENDIF.


        CLEAR: wl_zlest0060.
      ENDLOOP.


      gw_saida_vinc_nf_nome-id_frete_aqua  = wl_zlest0060_aux-id_frete_aqua.
      gw_saida_vinc_nf_nome-embarcacao     = wl_zlest0060_aux-embarcacao.
      gw_saida_vinc_nf_nome-nome           = wl_zlest0060_aux-nome_emb.
      gw_saida_vinc_nf_nome-dt_movimento   = wl_zlest0060_aux-dt_movimento.
      gw_saida_vinc_nf_nome-rm_codigo      = wl_zlest0060_aux-rm_codigo.
      gw_saida_vinc_nf_nome-local_descarga = wl_zlest0060_aux-local_descarga.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_zlest0060_aux-dt_codigo
        IMPORTING
          output = gw_saida_vinc_nf_nome-dt_codigo.


      SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr = gw_saida_vinc_nf_nome-dt_codigo.

      gw_saida_vinc_nf_nome-name1_dest = wl_kna1-name1.
      gw_saida_vinc_nf_nome-stcd1_dest = wl_kna1-stcd1.

      SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr = wl_zlest0060_aux-rm_codigo.

      gw_saida_vinc_nf_nome-name1_reme = wl_lfa1-name1.
      gw_saida_vinc_nf_nome-stcd1_reme = wl_lfa1-stcd1.

      gw_saida_vinc_nf_nome-tomador_serv   = wl_zlest0060_aux-tomador_serv.
      gw_saida_vinc_nf_nome-werks          = wl_zlest0060_aux-werks.
      gw_saida_vinc_nf_nome-nr_dco         = wl_zlest0060_aux-nr_dco.
      gw_saida_vinc_nf_nome-safra          = wl_zlest0060_aux-safra.
      gw_saida_vinc_nf_nome-operacao       = wl_zlest0060_aux-operacao.
      gw_saida_vinc_nf_nome-matnr          = wl_zlest0060_aux-matnr.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_zlest0060_aux-cl_codigo
        IMPORTING
          output = gw_saida_vinc_nf_nome-cl_codigo.


      IF gw_saida_vinc_nf_nome-id_frete_aqua IS NOT INITIAL.
        SELECT SINGLE *
          FROM zlest0061 INTO @wl_zlest0061
         WHERE id_frete_aqua EQ @gw_saida_vinc_nf_nome-id_frete_aqua.
      ELSE.
        SELECT SINGLE * FROM zlest0061 INTO wl_zlest0061 WHERE bukrs         EQ w_bukrs
                                                           AND werks         EQ w_werks
                                                           AND ano_viagem    EQ w_ano
                                                           AND nr_viagem     EQ w_viagem
                                                           AND embarcacao    EQ gw_saida_vinc_nf_nome-embarcacao
                                                           AND nome_emb      EQ gw_saida_vinc_nf_nome-nome
                                                           AND rm_codigo     EQ gw_saida_vinc_nf_nome-rm_codigo
                                                           AND nr_dco        EQ gw_saida_vinc_nf_nome-nr_dco
                                                           AND safra         EQ gw_saida_vinc_nf_nome-safra
                                                           AND operacao      EQ gw_saida_vinc_nf_nome-operacao.
        IF sy-subrc NE 0.
          SELECT SINGLE * FROM zlest0061 INTO wl_zlest0061 WHERE bukrs         EQ w_bukrs
                                                             AND werks         EQ w_werks
                                                             AND ano_viagem    EQ w_ano
                                                             AND nr_viagem     EQ w_viagem
                                                             AND embarcacao    EQ gw_saida_vinc_nf_nome-embarcacao
                                                             AND nome_emb      EQ gw_saida_vinc_nf_nome-nome
                                                             AND rm_codigo     EQ ''
                                                             AND cl_codigo     EQ gw_saida_vinc_nf_nome-cl_codigo
                                                             AND nr_dco        EQ gw_saida_vinc_nf_nome-nr_dco
                                                             AND safra         EQ gw_saida_vinc_nf_nome-safra
                                                             AND operacao      EQ gw_saida_vinc_nf_nome-operacao.
        ENDIF.
      ENDIF.

      IF ( sy-subrc EQ 0 ).

        SELECT SINGLE * FROM j_1bnfe_active INTO wl_j_1bnfe_active WHERE docnum EQ wl_zlest0061-docnum.

        IF ( sy-subrc EQ 0 ).
          CASE wl_j_1bnfe_active-docsta.
            WHEN: '1'.
              gw_saida_vinc_nf_nome-status_ov = icon_complete.
            WHEN: '2' OR '3'.
              gw_saida_vinc_nf_nome-status_ov = icon_defect.
            WHEN OTHERS.
              IF ( wl_j_1bnfe_active-docsta IS INITIAL ) AND ( wl_j_1bnfe_active-scssta IS INITIAL ).
                gw_saida_vinc_nf_nome-status_ov = icon_warning.
              ELSEIF ( wl_j_1bnfe_active-docsta IS INITIAL ) AND ( wl_j_1bnfe_active-scssta EQ '0' ).
                gw_saida_vinc_nf_nome-status_ov = icon_activity.
              ENDIF.
          ENDCASE.
        ENDIF.

      ENDIF.

      APPEND gw_saida_vinc_nf_nome TO gt_saida_vinc_nf_nome.

      DELETE tl_zlest0060_aux WHERE rm_codigo      = wl_zlest0060_aux-rm_codigo "cl_codigo = wl_zlest0060_aux-cl_codigo
                                AND safra          = wl_zlest0060_aux-safra
                                AND nr_dco         = wl_zlest0060_aux-nr_dco
                                AND operacao       = wl_zlest0060_aux-operacao
                                AND id_frete_aqua  = wl_zlest0060_aux-id_frete_aqua
                                AND matnr          = wl_zlest0060_aux-matnr.

      CLEAR: wl_zlest0060_aux, gw_saida_vinc_nf_nome, wl_kna1, wl_lfa1, tabix.

    ENDLOOP.

    IF NOT ( obj_grid_vinc_nf IS INITIAL ).
      CALL METHOD obj_grid_vinc_nf->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.
  ENDIF.

ENDFORM.                    " SELECIONA_NOTAS_FISCAIS
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_VINC_NF
*&---------------------------------------------------------------------*
FORM criar_catalog_vinc_nf .

  REFRESH: gt_fcat_vinc_nf[].

  "Catalogo dos campos existentes no ALV da Vincular Nota Fiscal e definição de suas propriedades
  PERFORM montar_catalog_vinc_nf USING:
        "'CHECK'          'Check'           '5'    '' '' '' '' '' 'X' '' ''  '' 'X',
        'ID_FRETE_AQUA'  'Id.Frete'        '10'    '' '' '' '' '' ''  '' '' '' '',
        'DT_MOVIMENTO'   'Dt. Movimento'   '10'   '' '' '' '' '' ''  '' '' '' '' ,
        'PESO_VINCULADO' 'Peso Vinc.'      '12'   '' '' '' '' 'X' ''  '' '' '' '',
        'NR_DCO'         'Nr.DCO'          '8'    'X' '' '' '' 'X' ''  '' '' '' '',
        'SAFRA'          'Safra'           '5'    '' '' '' '' 'X' ''  '' '' '' '',
        'RM_CODIGO'      'Código Remet.'   '7 '   'X' '' '' '' '' '' '' ''  '' '',
        'NAME1_REME'     'Desc. Remet.'    '25'   '' '' '' '' '' ''  '' '' '' '',
        'STCD1_REME'     'CNPJ Remet.'     '15'   '' '' '' '' '' ''  '' '' '' '',
        'DT_CODIGO'      'Código Dest.'    '7'    'X' '' '' '' '' ''  '' '' '' '',
        'NAME1_DEST'     'Desc. Dest.'     '25'   '' '' '' '' '' ''  '' '' '' '',
        'STCD1_DEST'     'CNPJ Dest.'      '15'   '' '' '' '' '' ''  '' '' '' '',
        'STATUS_OV'      'O.V Ger.'        '4'    '' '' '' '' '' ''  '' '' '' '',
        'OPERACAO'       'Operação'        '4'    '' '' '' 'C' '' ''  '' '' '' ''.

ENDFORM.                    " CRIAR_CATALOG_VINC_NF
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_VINC_NF
*&---------------------------------------------------------------------*
FORM montar_catalog_vinc_nf   USING  VALUE(p_fieldname)
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
                                     VALUE(p_tabname)       LIKE dd02d-tabname
                                     VALUE(p_check).

  CLEAR: gw_fcat_vinc_nf.

  gw_fcat_vinc_nf-fieldname = p_fieldname.
  gw_fcat_vinc_nf-ref_table = p_ref_tabname..
  gw_fcat_vinc_nf-ref_field = p_ref_fieldname.
  gw_fcat_vinc_nf-tabname   = p_tabname.
  gw_fcat_vinc_nf-scrtext_l = p_desc.
  gw_fcat_vinc_nf-scrtext_m = p_desc.
  gw_fcat_vinc_nf-scrtext_s = p_desc.
  gw_fcat_vinc_nf-outputlen = p_tam.
  gw_fcat_vinc_nf-no_zero   = p_no_zero.
  gw_fcat_vinc_nf-hotspot   = p_hotspot.
  gw_fcat_vinc_nf-emphasize = p_cor.
  gw_fcat_vinc_nf-just      = p_just.
  gw_fcat_vinc_nf-do_sum    = p_sum.
  gw_fcat_vinc_nf-edit      = p_edit.
  gw_fcat_vinc_nf-checkbox  = p_check.

  APPEND gw_fcat_vinc_nf TO gt_fcat_vinc_nf.
ENDFORM.                    " MONTAR_CATALOG_VINC_NF

*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG_VINC_NF
*&---------------------------------------------------------------------*
FORM criar_catalog_ger_ov .

  DATA: lw_setleaf TYPE setleaf.

  REFRESH: gt_fcat_ger_ov[].

  "Catalogo dos campos existentes no ALV
  "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - INICIO
  IF area_parametro_filial_eudr IS INITIAL.
    PERFORM montar_catalog_ger_ov USING:
          'ID_FRETE_AQUA'  'Id.Frete'        '10'  '' '' '' '' '' ''  '' '' '' '',
          'EMBARCACAO'     'Embarcação'      '5'   '' '' '' '' '' ''  '' '' '' '',
          'NOME'           'Nome.Emb.'       '15'  '' '' '' '' '' ''  '' '' '' '',
          'MAKTX'          'Produto'         '10'  '' '' '' '' '' '' '' ''  '' '',
          'TP_CLASS'       'Tipo Prod'       '8'   '' '' '' '' '' ''  '' '' '' '' ,
          'PESO_VINCULADO' 'Peso Vinc'       '8'   '' '' '' '' '' ''  '' '' '' '' ,
          'CL_CODIGO'      'Cliente'         '8'   'X' '' '' '' '' ''  '' '' '' '',
          'AUART'          'Tipo O.V.'       '8'   '' '' '' '' '' 'X' '' '' '' '' ,
          'WAERK'          'Moeda'           '5'   '' '' '' '' '' 'X' '' '' '' '' ,
          'NETPR'          'Vlr Unit.'       '8'   '' '' '' '' '' ''  '' '' '' '' .
  ELSE.
    PERFORM montar_catalog_ger_ov USING:
          'ID_FRETE_AQUA'  'Id.Frete'        '10'  '' '' '' '' '' ''  '' '' '' '',
          'EMBARCACAO'     'Embarcação'      '5'   '' '' '' '' '' ''  '' '' '' '',
          'NOME'           'Nome.Emb.'       '15'  '' '' '' '' '' ''  '' '' '' '',
          'MAKTX'          'Produto'         '10'  '' '' '' '' '' '' '' ''  '' '',
          'TP_CLASS'       'Tipo Prod'       '8'   '' '' '' '' '' ''  '' '' '' '' ,
          'EUDR'           'Atende EUDR'     '10'  '' '' '' '' '' ''  '' '' '' '' ,
          'PESO_VINCULADO' 'Peso Vinc'       '8'   '' '' '' '' '' ''  '' '' '' '' ,
          'CL_CODIGO'      'Cliente'         '8'   'X' '' '' '' '' ''  '' '' '' '',
          'AUART'          'Tipo O.V.'       '8'   '' '' '' '' '' 'X' '' '' '' '' ,
          'WAERK'          'Moeda'           '5'   '' '' '' '' '' 'X' '' '' '' '' ,
          'NETPR'          'Vlr Unit.'       '8'   '' '' '' '' '' ''  '' '' '' '' .
  ENDIF.
  "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328  - FIM
  SELECT SINGLE * FROM setleaf
    INTO lw_setleaf
  WHERE setname EQ 'AQUAV_CENTRO_TAXA'
    AND valfrom EQ w_werks.

  IF ( sy-subrc EQ 0 ).
    PERFORM montar_catalog_ger_ov USING:
          'TAX_DOLAR'      'TX O.V. Serv'    '8'   '' '' '' '' '' 'X' 'BKPF' 'KURSF' '' ''.
  ELSE.
    PERFORM montar_catalog_ger_ov USING:
    'TAX_DOLAR'      'TX O.V. Serv'    '8'   '' '' '' '' '' '' 'BKPF' 'KURSF' '' '' .
  ENDIF.

  PERFORM montar_catalog_ger_ov USING:

  'VLR_USD'        'VLR USD'         '7'   '' '' '' '' '' ''  '' '' '' '' ,
  'VLR_BRL'        'VLR BRL'         '8'   '' '' '' '' '' ''  '' '' '' '' ,
  'NR_OV'          'NR. OV'          '8'   'X' '' ''  '' '' ''  '' '' '' '',
  'FATURA'         'Fatura'          '8'   'X' '' ''  '' '' ''  '' '' '' '',
  'DT_FATURA'      'Dt.Fatura'       '10'  '' '' ''  '' '' ''  '' '' '' '',
  'NFNUM'          'NF/CT-e'         '7'   'X' '' '' '' '' ''  '' '' '' '',
  'DOCNUM'         'DocNum'          '8'   'X' 'X' '' '' '' ''  '' '' '' ''.

ENDFORM.                    " CRIAR_CATALOG_GER_OV

*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_VINC_NF
*&---------------------------------------------------------------------*
FORM montar_catalog_ger_ov   USING  VALUE(p_fieldname)
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
                                     VALUE(p_tabname)       LIKE dd02d-tabname
                                     VALUE(p_check).

  CLEAR: gw_fcat_ger_ov.

  gw_fcat_ger_ov-fieldname = p_fieldname.
  gw_fcat_ger_ov-ref_table = p_ref_tabname..
  gw_fcat_ger_ov-ref_field = p_ref_fieldname.
  gw_fcat_ger_ov-tabname   = p_tabname.
  gw_fcat_ger_ov-scrtext_l = p_desc.
  gw_fcat_ger_ov-scrtext_m = p_desc.
  gw_fcat_ger_ov-scrtext_s = p_desc.
  gw_fcat_ger_ov-outputlen = p_tam.
  gw_fcat_ger_ov-no_zero   = p_no_zero.
  gw_fcat_ger_ov-hotspot   = p_hotspot.
  gw_fcat_ger_ov-emphasize = p_cor.
  gw_fcat_ger_ov-just      = p_just.
  gw_fcat_ger_ov-do_sum    = p_sum.
  gw_fcat_ger_ov-edit      = p_edit.
  gw_fcat_ger_ov-checkbox  = p_check.

  IF p_fieldname = 'AUART'.
    gw_fcat_ger_ov-f4availabl = 'X'.
  ENDIF.
  APPEND gw_fcat_ger_ov TO gt_fcat_ger_ov.
ENDFORM.                    " MONTAR_CATALOG_VINC_NF
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS_VIAGEM
*&---------------------------------------------------------------------*
FORM verifica_erros_viagem .


  DATA: wl_t001      TYPE t001,  " Work Area para Informações da Empresa
        wl_t001w     TYPE t001w, " Work Area para Informações do Centro
        wl_lfa1      TYPE lfa1,
        wl_kna1      TYPE kna1,
        wl_zlest0064 TYPE zlest0064,
        wl_zlest0054 TYPE zlest0054,
        wl_linha(6),
        lw_setleaf   TYPE setleaf.

  REFRESH: tg_msg_ret.
  CLEAR: tg_msg_ret.

  IF w_bukrs IS INITIAL.
    MOVE: TEXT-e01          TO tg_msg_ret-msg,
          'W_BUKRS'         TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Empresa' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
    FROM t001
    INTO wl_t001
     WHERE  bukrs EQ w_bukrs.
    IF sy-subrc NE 0.
      CONCATENATE TEXT-e02 ' Empresa' INTO  tg_msg_ret-msg.
      MOVE 'W_BUKRS'         TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF w_werks IS INITIAL.
    MOVE: TEXT-e01          TO tg_msg_ret-msg,
          'W_WERKS'         TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Centro' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
    FROM t001w
    INTO wl_t001w
     WHERE  werks EQ w_werks.
    IF sy-subrc NE 0.
      CONCATENATE TEXT-e02 ' Centro' INTO  tg_msg_ret-msg.
      MOVE 'W_WERKS'         TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF w_ano IS INITIAL.
    MOVE: TEXT-e01          TO tg_msg_ret-msg,
          'W_ANO'         TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Ano' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  LOOP AT gt_saida_viagem INTO gw_saida_viagem.
    wl_linha = sy-tabix.
    IF gw_saida_viagem-dt_prevista IS INITIAL.
      MOVE:  'OBJ_GRID_VIAGEM'  TO tg_msg_ret-aba.
      CONCATENATE TEXT-e01 ' Data Prevista, LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF gw_saida_viagem-po_embarque IS INITIAL.
      MOVE: TEXT-e01          TO tg_msg_ret-msg.
      CONCATENATE  tg_msg_ret-msg 'Porto de embarque' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      SELECT SINGLE *
      FROM lfa1
      INTO wl_lfa1
       WHERE  lifnr EQ gw_saida_viagem-po_embarque.
      IF sy-subrc NE 0.
        CONCATENATE TEXT-e02 ' Porto de embarque' INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF gw_saida_viagem-po_destino IS INITIAL.
      MOVE: TEXT-e01          TO tg_msg_ret-msg.
      CONCATENATE  tg_msg_ret-msg 'Porto de destino' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      SELECT SINGLE *
      FROM kna1
      INTO wl_kna1
       WHERE  kunnr EQ gw_saida_viagem-po_destino.
      IF sy-subrc NE 0.
        CONCATENATE TEXT-e02 ' Porto de destino' INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM setleaf
      INTO lw_setleaf
     WHERE setname EQ 'MAGGI_ZLES0077_CALADO'
       AND valfrom EQ gw_saida_viagem-po_embarque.

    IF sy-subrc = 0.

      IF gw_saida_viagem-calado IS INITIAL.
        MOVE:  'OBJ_GRID_VIAGEM'  TO tg_msg_ret-aba.
        CONCATENATE TEXT-e01 ' Calado, LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
        SELECT SINGLE *
          FROM zlest0054
          INTO wl_zlest0054
          WHERE calado = gw_saida_viagem-calado
          AND  und_medida = gw_saida_viagem-und_medida+0(2).
        IF sy-subrc NE 0.
          CONCATENATE TEXT-e02 ' Calado' INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

      IF gw_saida_viagem-und_medida IS INITIAL.
        MOVE:  'OBJ_GRID_VIAGEM'  TO tg_msg_ret-aba.
        CONCATENATE TEXT-e01 ' Medida, LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

    ENDIF.

    IF gw_saida_viagem-direcao IS INITIAL.
      MOVE: TEXT-e01          TO tg_msg_ret-msg.
      CONCATENATE  tg_msg_ret-msg 'Direção' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      SELECT SINGLE *
        FROM zlest0064 INTO wl_zlest0064
        WHERE po_embarque EQ gw_saida_viagem-po_embarque
          AND po_destino  EQ gw_saida_viagem-po_destino.

      IF sy-subrc NE 0.
        CONCATENATE TEXT-e02 ' Direção' INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.


  ENDLOOP.

ENDFORM.                    " VERIFICA_ERROS_VIAGEM
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS_VIAGEM
*&---------------------------------------------------------------------*
FORM grava_dados_viagem .

  DATA: wl_input_zlest0058 TYPE zlest0058,
        tl_input_zlest0056 TYPE TABLE OF zlest0056 WITH HEADER LINE,
        wl_zlest0064       TYPE zlest0064,
        wl_zlest0063       TYPE zlest0063,
        tabix              TYPE sy-tabix,
        tl_estilo          TYPE lvc_t_styl WITH HEADER LINE,
        wl_estilo          TYPE lvc_s_styl,
        lw_setleaf         TYPE setleaf,
        vl_vinc_comboio    TYPE c.

  "SELECT SINGLE * FROM zlest0056 WHERE bukrs      EQ w_bukrs
  "                                 AND werks      EQ w_werks
  "                                 AND nr_viagem  EQ w_viagem
  "                                 AND ano_viagem EQ w_ano.
  "CASE sy-subrc.

  "WHEN: '0'.
  "  MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Viagem já cadastrada!'.
  "  WHEN OTHERS.

  LOOP AT gt_saida_viagem INTO gw_saida_viagem.

    MOVE: sy-mandt                        TO  tl_input_zlest0056-mandt,
          w_bukrs                         TO  tl_input_zlest0056-bukrs,
          w_werks                         TO  tl_input_zlest0056-werks,
          w_viagem                        TO  tl_input_zlest0056-nr_viagem,
          w_ano                           TO  tl_input_zlest0056-ano_viagem,
          ''                              TO  tl_input_zlest0056-status,
          gw_saida_viagem-dt_prevista     TO  tl_input_zlest0056-dt_prevista,
          gw_saida_viagem-hr_prevista     TO  tl_input_zlest0056-hr_prevista,
          gw_saida_viagem-po_embarque     TO  tl_input_zlest0056-po_embarque,
          gw_saida_viagem-po_destino      TO  tl_input_zlest0056-po_destino,
          gw_saida_viagem-calado          TO  tl_input_zlest0056-calado,
          gw_saida_viagem-und_medida+0(2) TO  tl_input_zlest0056-und_medida,
          sy-uname                        TO  tl_input_zlest0056-usuario,
          sy-datum                        TO  tl_input_zlest0056-data_registro,
          sy-uzeit                        TO  tl_input_zlest0056-hora_registro.

    SELECT SINGLE * FROM zlest0064 INTO wl_zlest0064 WHERE po_embarque EQ tl_input_zlest0056-po_embarque
                                                       AND po_destino  EQ tl_input_zlest0056-po_destino.

    IF ( sy-subrc EQ 0 ).
      tl_input_zlest0056-direcao = wl_zlest0064-direcao.
    ENDIF.


    APPEND tl_input_zlest0056.
  ENDLOOP.

  CLEAR novo.
  filtro = 'X'.
  MODIFY zlest0056 FROM TABLE tl_input_zlest0056.

  IF ( sy-subrc EQ 0 ).

    novo = 'X'.

    REFRESH: tl_estilo.
    CLEAR: gw_saida_viagem,wl_estilo.

    LOOP AT gt_saida_viagem INTO gw_saida_viagem.

      CLEAR: vl_vinc_comboio, gw_saida_viagem-estilo.

      tabix = sy-tabix.

      gw_saida_viagem-bukrs       = w_bukrs.
      gw_saida_viagem-werks       = w_werks.
      gw_saida_viagem-nr_viagem   = w_viagem.
      gw_saida_viagem-ano_viagem  = w_ano.

      SELECT SINGLE *
        FROM zlest0063 INTO wl_zlest0063
       WHERE bukrs       = gw_saida_viagem-bukrs
         AND werks       = gw_saida_viagem-werks
         AND ano_viagem  = gw_saida_viagem-ano_viagem
         AND nr_viagem   = gw_saida_viagem-nr_viagem.
      IF sy-subrc = 0.
        vl_vinc_comboio = 'X'.
      ENDIF.

      SELECT SINGLE * FROM setleaf
        INTO lw_setleaf
       WHERE setname EQ 'MAGGI_ZLES0077_CALADO'
         AND valfrom EQ gw_saida_viagem-po_embarque.

      IF ( sy-subrc NE 0 ) OR ( vl_vinc_comboio IS NOT INITIAL )."IF NOT ( gw_saida_viagem-calado IS INITIAL ).
        wl_estilo-fieldname = 'CALADO'.
        wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO tl_estilo.
      ENDIF.

      IF vl_vinc_comboio IS NOT INITIAL."IF NOT ( gw_saida_viagem-dt_prevista IS INITIAL ).
        wl_estilo-fieldname = 'DT_PREVISTA'.
        wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO tl_estilo.
      ENDIF.

      IF vl_vinc_comboio IS NOT INITIAL.
        wl_estilo-fieldname = 'HR_PREVISTA'.
        wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO tl_estilo.
      ENDIF.

      IF vl_vinc_comboio IS NOT INITIAL. "IF NOT ( gw_saida_viagem-po_destino IS INITIAL ).
        wl_estilo-fieldname = 'PO_DESTINO'.
        wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO tl_estilo.
      ENDIF.

      IF vl_vinc_comboio IS NOT INITIAL. "IF NOT ( gw_saida_viagem-po_embarque IS INITIAL ).
        wl_estilo-fieldname = 'PO_EMBARQUE'.
        wl_estilo-style = cl_gui_alv_grid=>mc_style_disabled.
        APPEND wl_estilo TO tl_estilo.
      ENDIF.

      INSERT LINES OF tl_estilo INTO TABLE gw_saida_viagem-estilo.
      MODIFY gt_saida_viagem FROM gw_saida_viagem INDEX tabix.

    ENDLOOP.

    CALL METHOD obj_grid_viagem->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    MESSAGE s000(zwrm001) DISPLAY LIKE 'S' WITH 'Dados da viagem gravado com sucesso.'.
  ENDIF.

  "ENDCASE.

ENDFORM.                    " GRAVA_DADOS_VIAGEM
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS_COMBOIO
*&---------------------------------------------------------------------*
FORM verifica_erros_comboio .

  DATA: wl_linha(6),
        wl_cont              TYPE i,
        wl_cont_emp          TYPE i,
        wl_zlest0053         TYPE zlest0053,
        wl_makt              TYPE makt,
        wl_mara              TYPE mara,
        wl_setleaf           TYPE setleaf,
        gt_saida_comboio_aux TYPE TABLE OF zaqty_saida_comboio,
        gw_saida_comboio_aux TYPE          zaqty_saida_comboio.

  REFRESH: tg_msg_ret.
  CLEAR: tg_msg_ret.

  DELETE gt_saida_comboio WHERE embarcacao IS INITIAL.

  CALL METHOD obj_grid_comboio->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  gt_saida_comboio_aux[] = gt_saida_comboio[].
  SORT gt_saida_comboio_aux BY  embarcacao nome  .

  LOOP AT gt_saida_comboio INTO gw_saida_comboio.

    wl_linha = sy-tabix.
    wl_cont = 0.


    LOOP AT gt_saida_comboio_aux INTO gw_saida_comboio_aux WHERE embarcacao = gw_saida_comboio-embarcacao
                                                           AND   nome       = gw_saida_comboio-nome.
      ADD 1 TO wl_cont.
    ENDLOOP.

    IF wl_cont GT 1.

      MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
      CONCATENATE TEXT-e03 ', LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
    IF gw_saida_comboio-embarcacao IS INITIAL.
      MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
      CONCATENATE TEXT-e01 ' Tipo. Emb., LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSEIF NOT 'B_E_R' CS gw_saida_comboio-embarcacao+0(1) .
      MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
      CONCATENATE TEXT-e02 ' Tipo. Emb., LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF gw_saida_comboio-nome IS INITIAL.
      MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
      CONCATENATE TEXT-e01 ' Nome da Embarcação, LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      SELECT SINGLE *
        FROM zlest0053
        INTO wl_zlest0053
        WHERE embarcacao = gw_saida_comboio-embarcacao+0(1)
        AND   nome       = gw_saida_comboio-nome.
      IF sy-subrc NE 0.
        MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
        CONCATENATE TEXT-e02 ' Nome da Embarcação, LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF gw_saida_comboio-matnr IS INITIAL AND gw_saida_comboio-embarcacao(1) EQ 'B'.
      MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
      CONCATENATE TEXT-e01 ' Material, LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      SELECT SINGLE *
        FROM makt
        INTO wl_makt
        WHERE matnr EQ gw_saida_comboio-matnr
          AND spras EQ sy-langu.
      IF sy-subrc NE 0 AND gw_saida_comboio-embarcacao(1) EQ 'B'.
        MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
        CONCATENATE TEXT-e02 ' Material, LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF gw_saida_comboio-tp_class IS INITIAL AND gw_saida_comboio-embarcacao(1) EQ 'B'.

      CLEAR: wl_mara.

      SELECT SINGLE *
        FROM mara
        INTO wl_mara
        WHERE matnr = gw_saida_comboio-matnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_mara-matkl
        IMPORTING
          output = wl_mara-matkl.

      "LES - US 165779 - Transp. Aquav. Algodao - WPP -->>
      SELECT SINGLE *
        FROM tvarvc INTO @DATA(lwa_tvarvc)
       WHERE name EQ 'MAGGI_GR_GRAOS'
         AND low  EQ @wl_mara-matkl.

      IF ( sy-subrc EQ 0 ).
        MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
        CONCATENATE TEXT-e01 ' Tipo de Classificação, LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
*      SELECT SINGLE * FROM setleaf
*        INTO wl_setleaf
*       WHERE setname EQ 'MAGGI_ZLES0077_GR_MAT'
*         AND valfrom EQ wl_mara-matkl.
*
*      IF ( sy-subrc NE 0 ).
*
*        MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
*        CONCATENATE TEXT-e01 ' Tipo de Classificação, LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
*        APPEND tg_msg_ret.
*        CLEAR: tg_msg_ret.
*
*      ENDIF.
      "LES - US 165779 - Transp. Aquav. Algodao - WPP <<--

    ENDIF.

    "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
    IF area_parametro_filial_eudr IS NOT INITIAL.
      DATA(lva_eudr) = zcl_eudr_utils=>check_material_eudr( i_matnr = gw_saida_comboio-matnr ).
      IF lva_eudr EQ abap_true AND gw_saida_comboio-eudr IS INITIAL.
        MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
        CONCATENATE TEXT-e01 ' Classificação EUDR não informada! LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSEIF lva_eudr EQ abap_false.
        CLEAR: gw_saida_comboio-eudr.
        MODIFY gt_saida_comboio FROM gw_saida_comboio INDEX wl_linha TRANSPORTING eudr.
      ENDIF.
    ENDIF.
    "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328

    READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX 1.
    IF sy-subrc = 0.
      SELECT SINGLE * FROM setleaf
        INTO wl_setleaf
       WHERE setname EQ 'MAGGI_ZLES0077_CALADO'
         AND valfrom EQ gw_saida_viagem-po_embarque.
      IF sy-subrc = 0.

        IF gw_saida_comboio-unid_medida IS INITIAL  AND gw_saida_comboio-embarcacao(1) NE 'E'.
          MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
          CONCATENATE TEXT-e01 ' Unid. Medida, LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

        IF gw_saida_comboio-peso_previsto IS INITIAL AND gw_saida_comboio-embarcacao(1) EQ 'B'. "Se o valor do Comboio estiver zerado e a embarcação for igual a barcaça devera ser obrigatorio.
          MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
          CONCATENATE TEXT-e01 ' Peso Previsto, LINHA: ' wl_linha  INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

      ENDIF.
    ENDIF.

    IF ( gw_saida_comboio-embarcacao(1) EQ 'E' ).
      wl_cont_emp = wl_cont_emp + 1.
    ENDIF.

  ENDLOOP.

  IF ( wl_cont_emp IS INITIAL ).
    MOVE:  'TAB_COMB'   TO tg_msg_ret-aba.
    MOVE: 'É necessário um empurrador!' TO  tg_msg_ret-msg.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret, wl_cont_emp.
  ENDIF.

ENDFORM.                    " VERIFICA_ERROS_COMBOIO
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS_COMBOIO
*&---------------------------------------------------------------------*
FORM grava_dados_comboio .

  DATA: tl_input_zlest0063 TYPE TABLE OF zlest0063 WITH HEADER LINE,
        wl_mara            TYPE mara.

  LOOP AT gt_saida_comboio INTO gw_saida_comboio.

    SELECT SINGLE * FROM mara INTO wl_mara WHERE matnr EQ gw_saida_comboio-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_mara-matkl
      IMPORTING
        output = wl_mara-matkl.


    MOVE: sy-mandt                          TO  tl_input_zlest0063-mandt,
          w_bukrs                           TO  tl_input_zlest0063-bukrs,
          w_werks                           TO  tl_input_zlest0063-werks,
          w_viagem                          TO  tl_input_zlest0063-nr_viagem,
          w_ano                             TO  tl_input_zlest0063-ano_viagem,
          gw_saida_comboio-embarcacao+0(1)  TO  tl_input_zlest0063-embarcacao,
          gw_saida_comboio-nome             TO  tl_input_zlest0063-nome_emb,
          gw_saida_comboio-tp_barcaca+0(1)  TO  tl_input_zlest0063-tp_barcaca,
          gw_saida_comboio-matnr            TO  tl_input_zlest0063-cod_material,
          gw_saida_comboio-eudr(1)          TO  tl_input_zlest0063-eudr, "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 --->>>

          wl_mara-matkl                     TO  tl_input_zlest0063-gr_material,

          gw_saida_comboio-tp_class         TO  tl_input_zlest0063-tp_class,
          gw_saida_comboio-unid_medida+0(2) TO  tl_input_zlest0063-un_medida,
          gw_saida_comboio-peso_previsto    TO  tl_input_zlest0063-peso_previsto,
          sy-uname                          TO  tl_input_zlest0063-usuario,
          sy-datum                          TO  tl_input_zlest0063-data_registro,
          sy-uzeit                          TO  tl_input_zlest0063-hora_registro.

    APPEND tl_input_zlest0063.
    CLEAR: wl_mara.
  ENDLOOP.

  DELETE FROM zlest0063 WHERE bukrs      = w_bukrs
                   AND   werks      = w_werks
                   AND   nr_viagem  = w_viagem
                   AND   ano_viagem = w_ano.

  MODIFY zlest0063 FROM TABLE tl_input_zlest0063.
  wg_acao_cb = 'OK'.
  novo = 'V'.

  PERFORM: criar_alv_comboio.
  MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Dados de comboio gravado com sucesso.'.

ENDFORM.                    "GRAVA_DADOS_COMBOIO
" GRAVA_DADOS_COMBOIO
*&---------------------------------------------------------------------*
*&      Module  STATUS_1200  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_1200 OUTPUT.

  DATA: wl_lfa1     TYPE lfa1,
        wl_kna1     TYPE kna1,
        wl_kna1_aux TYPE kna1.

  DATA: wl_zlest0104 TYPE zlest0104.

  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.

  "PERFORM F_CENTRO_CALC_RET USING W_WERKS. "Adequações Retenção CS2016001199

  IF wg_acao = 'VIS_ROMANEIO'.

    APPEND '&NOVO'  TO fcode.
    APPEND '&MODIF' TO fcode.
    APPEND '&SAVE'  TO fcode.


    LOOP AT SCREEN.
      IF  ( screen-name EQ 'WG_TELA1200-RM_CODIGO'    ) OR
          ( screen-name EQ 'WG_TELA1200-DT_CODIGO'    ) OR
          ( screen-name EQ 'WG_TELA1200-DT_MOVIMENTO' ) OR
          ( screen-name EQ 'WG_TELA1200-SERIES'       ) OR
          ( screen-name EQ 'WG_TELA1200-NR_SAFRA'     ) OR
          ( screen-name EQ 'WG_TELA1200-OPERACAO'     ) OR
          ( screen-name EQ 'WG_TELA1200-TOMADOR_COD'  ).
        screen-output = '1'.
        screen-input  = '0'.
      ENDIF.

      IF ( screen-name EQ 'BTN_ATRIB_PESO'             ) OR
         ( screen-name EQ 'BTN_DESELECT_ALL'           ) OR
         ( screen-name EQ 'WG_TELA1200-PESO_AVINC'     ) OR
         ( screen-name EQ 'TXT_PESO_AVINC'             ) OR
         ( screen-name EQ 'TXT_RET_ANTERIOR'           ) OR
         ( screen-name EQ 'WG_TELA1200-RET_ANTERIOR'   ) OR
         ( screen-name EQ 'TXT_RET_SEL'                ) OR
         ( screen-name EQ 'WG_TELA1200-RETENCAO_SEL'   ) OR
         ( screen-name EQ 'TXT_TOT_RET'                ) OR
         ( screen-name EQ 'WG_TELA1200-TOT_RET_ACUM'   ) OR
         ( screen-name EQ 'BTN_DESC_RET'               ) OR
         ( screen-name EQ 'BTN_TROCAR_LOCAL'           ) .
        screen-active = '0'.
      ENDIF.

      IF ( screen-name EQ 'WG_TELA1200-PESO_VINCULADO' ) OR
         ( screen-name EQ 'TXTPESO' ).
        screen-active = '0'.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.

  ENDIF.

  SET PF-STATUS 'Z001'  EXCLUDING fcode.
  SET TITLEBAR  '1200'.

  LOOP AT SCREEN.

    IF screen-name EQ 'WG_TELA1200-NAME1_DEST'.
      IF wg_tela1200-tomador_cod IS NOT INITIAL.
        IF wg_tela1200-tomador_cod = 'D'.
          wg_tela1200-tomador_serv = 'DESTINATARIO'.
        ELSEIF wg_tela1200-tomador_cod = 'R'.
          wg_tela1200-tomador_serv = 'REMETENTE'.
        ELSE.
          CLEAR  wg_tela1200-tomador_serv.
        ENDIF.
      ELSE.
        CLEAR  wg_tela1200-tomador_serv.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'WG_TELA1200-NAME1_DEST'.

      IF wg_tela1200-dt_codigo IS NOT INITIAL.

        "DATA: dt_codigo TYPE kna1-kunnr.

        CLEAR wl_kna1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wg_tela1200-dt_codigo
          IMPORTING
            output = wg_tela1200-dt_codigo.

        SELECT SINGLE *
          FROM kna1
          INTO wl_kna1
          WHERE kunnr = wg_tela1200-dt_codigo.

        wg_tela1200-name1_dest = wl_kna1-name1.
        wg_tela1200-ort01_dest = wl_kna1-ort01.
        wg_tela1200-stcd1_dest = wl_kna1-stcd1.


      ELSE.
        CLEAR:  wg_tela1200-name1_dest, wg_tela1200-ort01_dest,wg_tela1200-stcd1_dest.
      ENDIF.
    ELSEIF screen-name EQ 'WG_TELA1200-NAME1_REME'.
      IF wg_tela1200-rm_codigo IS NOT INITIAL.
        CLEAR wl_lfa1.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wg_tela1200-rm_codigo
          IMPORTING
            output = wg_tela1200-rm_codigo.
        SELECT SINGLE *
        FROM lfa1
        INTO wl_lfa1
        WHERE lifnr = wg_tela1200-rm_codigo.
        wg_tela1200-name1_reme = wl_lfa1-name1.
        wg_tela1200-ort01_reme = wl_lfa1-ort01.
        wg_tela1200-stcd1_reme = wl_lfa1-stcd1.

        IF ( vg_sugere_dest        IS INITIAL        ) AND
           ( wg_tela1200-dt_codigo IS INITIAL        ) AND
           ( wl_lfa1-kunnr         IS NOT INITIAL    ) AND
           ( wg_acao               NE 'VIS_ROMANEIO' ).
          wg_tela1200-dt_codigo = wl_lfa1-kunnr.
          vg_sugere_dest = 'X'.
        ENDIF.

      ELSE.
        CLEAR:  wg_tela1200-name1_reme, wg_tela1200-ort01_reme,wg_tela1200-stcd1_reme.
      ENDIF.
    ELSEIF screen-name EQ 'WG_TELA1200-NAME1_EXPE'.

      IF wg_tela1200-ex_codigo IS NOT INITIAL.
        CLEAR wl_lfa1.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wg_tela1200-ex_codigo
          IMPORTING
            output = wg_tela1200-ex_codigo.
        SELECT SINGLE *
          FROM lfa1 INTO wl_lfa1
         WHERE lifnr = wg_tela1200-ex_codigo.
        wg_tela1200-name1_expe = wl_lfa1-name1.
        wg_tela1200-ort01_expe = wl_lfa1-ort01.
        wg_tela1200-stcd1_expe = wl_lfa1-stcd1.
      ELSE.
        CLEAR:  wg_tela1200-name1_expe, wg_tela1200-ort01_expe,wg_tela1200-stcd1_expe.
      ENDIF.

    ELSEIF screen-name EQ 'WG_TELA1200-NAME1_RECB'.

      IF wg_tela1200-rc_codigo IS NOT INITIAL.
        CLEAR wl_kna1.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wg_tela1200-rc_codigo
          IMPORTING
            output = wg_tela1200-rc_codigo.
        SELECT SINGLE *
          FROM kna1 INTO wl_kna1
         WHERE kunnr = wg_tela1200-rc_codigo.
        wg_tela1200-name1_recb = wl_kna1-name1.
        wg_tela1200-ort01_recb = wl_kna1-ort01.
        wg_tela1200-stcd1_recb = wl_kna1-stcd1.
      ELSE.
        CLEAR:  wg_tela1200-name1_recb, wg_tela1200-ort01_recb,wg_tela1200-stcd1_recb.
      ENDIF.


    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

  "Buscar informações para condições
  CLEAR: wl_zlest0104, wg_tela1200-bukrs_rom, wg_tela1200-branch_rom.

  SELECT SINGLE * FROM zlest0104 INTO wl_zlest0104 WHERE emissor = w_werks.
  IF sy-subrc = 0.
    wg_tela1200-bukrs_rom  = wl_zlest0104-bukrs.
    wg_tela1200-branch_rom = wl_zlest0104-branch.
  ENDIF.


  IF ( gv_edit IS INITIAL ).
    PERFORM criar_alv_romaneio.
  ELSE.
    CLEAR: gv_edit.
  ENDIF.

ENDMODULE.                 " STATUS_1200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit INPUT.
  CASE ok_code.
    WHEN c_back.
      SET SCREEN 0.
    WHEN c_cancel.
      SET SCREEN 0.
    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1200 INPUT.

  TYPES: BEGIN OF ty_vbfa,
           vbelv        TYPE vbfa-vbelv,
           vbtyp_n      TYPE vbfa-vbtyp_n,
           vbeln        TYPE vbfa-vbeln,
           mjahr        TYPE vbfa-mjahr,
           vbeln_refkey TYPE j_1bnflin-refkey,
         END OF ty_vbfa.


  DATA: tl_zlest0060_nf_vinc     TYPE TABLE OF zlest0060,
        wl_zlest0060_nf_vinc     TYPE zlest0060,
        wl_zlest0060_nf_vinc_aux TYPE zlest0060,
        "TL_ZLEST0065          TYPE TABLE OF ZLEST0065,
        "WL_ZLEST0065          TYPE ZLEST0065,
        tl_zib_nfe_dist_ter      TYPE TABLE OF zib_nfe_dist_ter,
        wl_zib_nfe_dist_ter      TYPE zib_nfe_dist_ter,
        wl_zib_nfe_dist_itm      TYPE zib_nfe_dist_itm,
        wl_input_zlest0057       TYPE zlest0057,
        wl_verifica_zlest0057    TYPE zlest0057,
        wl_zsdt0001_x            TYPE zsdt0001,
        tl_input_zlest0060       TYPE TABLE OF zlest0060 WITH HEADER LINE,
        tl_input_zlest0073       TYPE TABLE OF zlest0073 WITH HEADER LINE,
        total_peso               TYPE zlest0057-peso_vinculado,
        total_valor              TYPE zlest0057-valor_vinculado,
        total_peso_vinculado     TYPE zlest0057-peso_vinculado,
        nr_dco                   TYPE zdoc_rem,
        dt_codigo_aux            TYPE c LENGTH 10,
        tabix                    TYPE sy-tabix,
        tl_t001w                 TYPE TABLE OF t001w,
        wl_t001w                 TYPE t001w,
        v_werks                  TYPE t001w-werks,
        wl_lfa1_nf               TYPE lfa1,
        tp_notas                 TYPE c LENGTH 8,
        tp_romane                TYPE c LENGTH 8,
        tl_zdco_vinculo          TYPE TABLE OF zdco_vinculo,
        wl_zdco_vinculo          TYPE zdco_vinculo,
        tg_zlest0158             TYPE TABLE OF zlest0158 WITH HEADER LINE,
        v_gravado                TYPE c LENGTH 1,
        vl_kunnr                 TYPE c LENGTH 10,
        vl_tomador               TYPE c LENGTH 10,
        v_peso_sgramas           TYPE p DECIMALS 0,
        "Tabelas para Verificar a chave da nota fiscal.
        tl_vbfa                  TYPE TABLE OF ty_vbfa,
        wl_vbfa                  TYPE ty_vbfa,
        tl_j_1bnflin             TYPE TABLE OF j_1bnflin,
        wl_j_1bnflin             TYPE j_1bnflin,
        tl_j_1bnfe_active        TYPE TABLE OF j_1bnfe_active,
        wl_j_1bnfe_active        TYPE j_1bnfe_active,
        wl_j_1bnfdoc             TYPE j_1bnfdoc,
        tl_zlest0073             TYPE TABLE OF zlest0073,
        wl_zlest0073             TYPE zlest0073,
        wl_setleaf               TYPE setleaf,
        vl_ok                    TYPE c,
        vl_break_vinc            TYPE c,
        vl_docnum                TYPE j_1bnfe_active-docnum,
        gt_operacao_vinc         TYPE TABLE OF zlest0055-operacao.

  DATA:  ret_rollback  LIKE bapiret2 OCCURS 0 WITH HEADER LINE.


  CASE ok_code.

    WHEN 'ENTER_NF'.

      PERFORM f_action_1200_enter_nf.

    WHEN '&SAVE'.

      IF wg_tela1200-dt_codigo IS INITIAL.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Informe o Destinatário.'.
        EXIT.
      ELSEIF wg_tela1200-rm_codigo IS INITIAL.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Informe o Remetente.'.
        EXIT.
      ENDIF.

      IF wg_tela1200-tomador_serv IS INITIAL.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Informe o Tomador de Serviço.'.
        EXIT.
      ENDIF.

      IF wg_tela1200-dt_movimento IS INITIAL.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Informe a data de movimento.'.
        EXIT.
      ENDIF.

      IF wg_tela1200-peso_avinc NE total_vinculado.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Total vinculado divergente do Peso a vincular!'.
        EXIT.
      ENDIF.

      "17.01.2018 - CS2018000076
*      IF VG_CALC_RETENCAO IS NOT INITIAL.
*        CLEAR: VL_BREAK_VINC.
*        LOOP AT GT_SAIDA_VINC_ROM_NOME INTO GW_SAIDA_VINC_ROM_NOME WHERE ESTILO IS INITIAL
*                                                                     AND XML    EQ ICON_LED_GREEN.
*
*          IF ( VL_BREAK_VINC IS NOT INITIAL ) AND ( GW_SAIDA_VINC_ROM_NOME-CHECK IS NOT INITIAL ).
*            MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Romaneios devem ser vinculados na sequência!'.
*            RETURN.
*          ENDIF.
*
*          IF ( GW_SAIDA_VINC_ROM_NOME-CHECK IS INITIAL ) AND
*             ( GW_SAIDA_VINC_ROM_NOME-PESO_FISCAL   IS NOT INITIAL AND
*               GW_SAIDA_VINC_ROM_NOME-PESO_SUBTOTAL IS NOT INITIAL ).
*            VL_BREAK_VINC = 'X'.
*            CONTINUE.
*          ENDIF.
*
*        ENDLOOP.
*      ENDIF.

*      CLEAR: V_GRAVADO, TOTAL_VINCULADO.


      DATA(_vinc_peso_gramas) = ''.
      CONCATENATE wp_werks wg_tela1200-rm_codigo INTO DATA(_centro_remet).
      SELECT SINGLE *
        FROM setleaf INTO @DATA(_wl_set_sgramas)
       WHERE setname = 'MAGGI_ZLES0077_VINC_GRAM'
         AND valfrom = @_centro_remet.
      IF ( sy-subrc = 0 ).
        _vinc_peso_gramas = 'X'.
      ENDIF.

      REFRESH: tl_input_zlest0060, tl_input_zlest0073.

      READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX 1.

      "Validações NF's vinculadas
      DATA(lit_saida_vinc) = gt_saida_vinc_rom_nome[].

      CLEAR: gt_operacao_vinc[], lit_saida_vinc[].

      LOOP AT gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome.

        CHECK gw_saida_vinc_rom_nome-check = 'X' AND gw_saida_vinc_rom_nome-estilo IS INITIAL.

        APPEND gw_saida_vinc_rom_nome-operacao TO gt_operacao_vinc.
        APPEND gw_saida_vinc_rom_nome TO lit_saida_vinc[].
      ENDLOOP.

      "Valida Safras diferentes
      DATA(lit_saida_vinc_safra) = lit_saida_vinc[].
      SORT lit_saida_vinc_safra BY safra.
      DELETE ADJACENT DUPLICATES FROM lit_saida_vinc_safra COMPARING safra.
      IF lines( lit_saida_vinc_safra[] ) > 1.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Não é possível vinculadar notas com safra diferente'.
        EXIT.
      ENDIF.

*      SORT GT_OPERACAO_VINC.
*      DELETE ADJACENT DUPLICATES FROM GT_OPERACAO_VINC.
*      IF LINES( GT_OPERACAO_VINC[] ) > 1.
*        MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Não é possível vinculadas notas com operações diferentes'.
*        EXIT.
*      ENDIF.

      LOOP AT gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome.

        CLEAR: tl_input_zlest0060.

        CLEAR: tabix.
        tabix = sy-tabix.

        IF gw_saida_vinc_rom_nome-check = 'X' AND gw_saida_vinc_rom_nome-estilo IS INITIAL.

          IF wg_tela1200-id_frete_aqua IS INITIAL.

            "Verificar se Existe outro lançamento
            SELECT SINGLE * INTO @DATA(wa_zlest0061)
              FROM zlest0060
             WHERE bukrs      EQ @wp_bukrs
               AND werks      EQ @wp_werks
               AND nr_viagem  EQ @wp_viagem
               AND ano_viagem EQ @wp_ano
               AND embarcacao EQ @gw_saida_comboio-embarcacao+0(1)
               AND nome_emb   EQ @gw_saida_comboio-nome
               AND rm_codigo  EQ @wg_tela1200-rm_codigo
               AND dt_codigo  EQ @wg_tela1200-dt_codigo
               AND nr_dco     EQ @gw_saida_vinc_rom_nome-nr_dco
               AND safra      EQ @gw_saida_vinc_rom_nome-safra.

            IF sy-subrc IS INITIAL.
              MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Já existe um agrupamento com essas condições!'.
              EXIT.
            ENDIF.

            TRY .
                zcl_fatura_frete_aquaviario=>zif_fatura_frete_aquaviario~get_instance(
                  )->get_new_id_fatura_frete(
                  IMPORTING
                    e_id_fatura_frete = wg_tela1200-id_frete_aqua    " Id. Frete Aquaviário
                  ).
              CATCH zcx_fatura_frete_aquaviario INTO DATA(zcx_erro_ft).    " .
                zcx_erro_ft->zif_error~published_erro( EXPORTING i_msgty         = 'S'
                                                                 i_msgty_display = 'E'
                                                                ).
                EXIT.
            ENDTRY.
          ENDIF.
          MOVE :  sy-mandt                               TO tl_input_zlest0060-mandt,
                  wp_bukrs                               TO tl_input_zlest0060-bukrs,
                  wp_werks                               TO tl_input_zlest0060-werks,
                  wp_viagem                              TO tl_input_zlest0060-nr_viagem,
                  wp_ano                                 TO tl_input_zlest0060-ano_viagem,
                  wp_eudr(1)                             TO tl_input_zlest0060-eudr, "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
                  gw_saida_comboio-embarcacao+0(1)       TO tl_input_zlest0060-embarcacao,
                  gw_saida_comboio-nome                  TO tl_input_zlest0060-nome_emb,
                  wg_tela1200-rm_codigo                  TO tl_input_zlest0060-rm_codigo,
                  wg_tela1200-dt_codigo                  TO tl_input_zlest0060-dt_codigo,
                  gw_saida_vinc_rom_nome-nr_romaneio     TO tl_input_zlest0060-nr_romaneio,
                  "GW_SAIDA_VINC_ROM_NOME-DT_MOVIMENTO    TO TL_INPUT_ZLEST0060-DT_MOVIMENTO,
                  sy-datum                                TO tl_input_zlest0060-dt_movimento,
                  wg_tela1200-id_frete_aqua               TO tl_input_zlest0060-id_frete_aqua,
                  gw_saida_vinc_rom_nome-id_frete_aqua_nf TO tl_input_zlest0060-id_frete_aqua_nf,
                  gw_saida_vinc_rom_nome-docdat           TO tl_input_zlest0060-docdat,
                  gw_saida_vinc_rom_nome-nfnum            TO tl_input_zlest0060-nfnum,
                  gw_saida_vinc_rom_nome-series           TO tl_input_zlest0060-series,
                  gw_saida_vinc_rom_nome-chave_nfe        TO tl_input_zlest0060-chave_nfe,
                  gw_saida_vinc_rom_nome-peso_vinc        TO tl_input_zlest0060-peso_fiscal,
                  gw_saida_vinc_rom_nome-valor_vinculado  TO tl_input_zlest0060-netwr,
                  gw_saida_vinc_rom_nome-nr_dco           TO tl_input_zlest0060-nr_dco,
                  gw_saida_vinc_rom_nome-safra            TO tl_input_zlest0060-safra,
                  gw_saida_vinc_rom_nome-doc_rem          TO tl_input_zlest0060-doc_rem,
                  gw_saida_vinc_rom_nome-docnum_rem       TO tl_input_zlest0060-docnum_rem,

                  "Adequações Retenção CS2016001199
                  gw_saida_vinc_rom_nome-peso_subtotal_vinc   TO tl_input_zlest0060-peso_subtotal,
                  gw_saida_vinc_rom_nome-peso_liq_ret_vinc    TO tl_input_zlest0060-peso_liq_ret,
                  gw_saida_vinc_rom_nome-valor_liq_ret_vinc   TO tl_input_zlest0060-vlr_liq_ret,
                  gw_saida_vinc_rom_nome-peso_retido          TO tl_input_zlest0060-peso_retido,
                  gw_saida_vinc_rom_nome-peso_retido_utlz     TO tl_input_zlest0060-peso_retido_utlz,
                  gw_saida_vinc_rom_nome-peso_retido_sld      TO tl_input_zlest0060-peso_retido_sld,
                  gw_saida_vinc_rom_nome-perc_ret             TO tl_input_zlest0060-perc_ret,
                  gw_saida_vinc_rom_nome-ch_referencia        TO tl_input_zlest0060-ch_referencia,
                  gw_saida_vinc_rom_nome-bukrs_rom            TO tl_input_zlest0060-bukrs_rom,
                  gw_saida_vinc_rom_nome-branch_rom           TO tl_input_zlest0060-branch_rom,
                  gw_saida_vinc_rom_nome-ret_acumulada        TO tl_input_zlest0060-ret_acumulada,
                  gw_saida_vinc_rom_nome-desc_ret_acum        TO tl_input_zlest0060-desc_ret_acum,
                  gw_saida_vinc_rom_nome-matnr                TO tl_input_zlest0060-matnr,
                  gw_saida_vinc_rom_nome-tp_transgenia        TO tl_input_zlest0060-tp_transgenia,
                  gw_saida_vinc_rom_nome-local_descarga       TO tl_input_zlest0060-local_descarga,
                  "Fim

                  gw_saida_vinc_rom_nome-operacao        TO tl_input_zlest0060-operacao,

                  sy-uname                               TO tl_input_zlest0060-usuario,
                  sy-datum                               TO tl_input_zlest0060-data_registro,
                  sy-uzeit                               TO tl_input_zlest0060-hora_registro,
                  wg_tela1200-tomador_cod                TO tl_input_zlest0060-tomador_serv.

          IF _vinc_peso_gramas IS INITIAL.
            CLEAR: v_peso_sgramas.
            IF tl_input_zlest0060-peso_liq_ret IS NOT INITIAL.
              v_peso_sgramas = tl_input_zlest0060-peso_liq_ret.
              IF v_peso_sgramas <> tl_input_zlest0060-peso_liq_ret.
                MESSAGE 'Vinculação de peso com gramas não permitida!' TYPE 'S'.
                RETURN.
              ENDIF.
            ELSE.
              v_peso_sgramas = tl_input_zlest0060-peso_fiscal.
              IF v_peso_sgramas <> tl_input_zlest0060-peso_fiscal.
                MESSAGE 'Vinculação de peso com gramas não permitida!' TYPE 'S'.
                RETURN.
              ENDIF.
            ENDIF.
          ENDIF.

          IF gw_saida_vinc_rom_nome-vinc_fiscal IS NOT INITIAL . "17.01.2018 - CS2018000076
            CLEAR: tl_input_zlest0060-peso_liq_ret.
            "       TL_INPUT_ZLEST0060-PESO_RETIDO,
            "       TL_INPUT_ZLEST0060-PERC_RET.
          ENDIF.

          CASE wg_tela1200-tomador_cod.
            WHEN: 'R'.
              tl_input_zlest0060-cl_codigo =   wg_tela1200-rm_codigo.

              SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr = tl_input_zlest0060-cl_codigo.
              SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr = wl_lfa1-kunnr.

              IF ( sy-subrc EQ 0 ).
                tl_input_zlest0060-cl_codigo  = wl_kna1-kunnr.
              ENDIF.

            WHEN: 'D'.
              tl_input_zlest0060-cl_codigo =   wg_tela1200-dt_codigo.
          ENDCASE.



*          IF ( TL_INPUT_ZLEST0060-PESO_RETIDO      > 0  ) AND
*             ( TL_INPUT_ZLEST0060-PESO_RETIDO_UTLZ = 0  ) AND
*             ( TL_INPUT_ZLEST0060-PESO_RETIDO_SLD  = 0  ).
*            TL_INPUT_ZLEST0060-PESO_RETIDO_SLD =  TL_INPUT_ZLEST0060-PESO_RETIDO.
*          ENDIF.

          tl_input_zlest0060-peso_retido_sld =  tl_input_zlest0060-peso_retido - tl_input_zlest0060-peso_retido_utlz.

          "Adequações Retenção CS2016001199
*          "Gravar na tabela ZLEST0073 - Controle de Saldo
*          SELECT SINGLE * FROM zlest0073 INTO wl_zlest0073 WHERE chave_nfe EQ gw_saida_vinc_rom_nome-chave_nfe.
*
*          IF ( sy-subrc EQ 0 ).
*
*            tl_input_zlest0073-peso_vinculado  = wl_zlest0073-peso_vinculado  + gw_saida_vinc_rom_nome-peso_vinc.
*            tl_input_zlest0073-valor_vinculado = wl_zlest0073-valor_vinculado + gw_saida_vinc_rom_nome-valor_vinculado.
*            tl_input_zlest0073-chave_nfe       = gw_saida_vinc_rom_nome-chave_nfe.
*            tl_input_zlest0073-po_embarque     = gw_saida_viagem-po_embarque.
*            tl_input_zlest0073-po_destino      = gw_saida_viagem-po_destino.
*
*            MOVE:   wl_zlest0073-valor_origem     TO tl_input_zlest0073-valor_origem,
*                    wl_zlest0073-peso_origem      TO tl_input_zlest0073-peso_origem.
*
*
*          ELSE.
*            MOVE: gw_saida_vinc_rom_nome-chave_nfe       TO tl_input_zlest0073-chave_nfe,
*                  gw_saida_vinc_rom_nome-netwr           TO tl_input_zlest0073-valor_origem,
*                  gw_saida_vinc_rom_nome-peso_fiscal     TO tl_input_zlest0073-peso_origem,
*                  gw_saida_vinc_rom_nome-peso_vinc       TO tl_input_zlest0073-peso_vinculado,
*                  gw_saida_vinc_rom_nome-valor_vinculado TO tl_input_zlest0073-valor_vinculado,
*                  gw_saida_viagem-po_embarque            TO tl_input_zlest0073-po_embarque,
*                  gw_saida_viagem-po_destino             TO tl_input_zlest0073-po_destino.
*
*          ENDIF.
          "Fim

          IF tl_input_zlest0060-id_frete_aqua_nf IS INITIAL.
            TRY .
                zcl_fatura_frete_aquaviario=>zif_fatura_frete_aquaviario~get_instance(
                  )->get_new_id_fatura_frete_item( IMPORTING e_id_fatura_frete_item = tl_input_zlest0060-id_frete_aqua_nf
                  ).
              CATCH zcx_fatura_frete_aquaviario INTO zcx_erro_ft.    " .
                zcx_erro_ft->zif_error~published_erro( EXPORTING i_msgty         = 'S'
                                                                 i_msgty_display = 'E'
                                                                ).
                EXIT.
            ENDTRY.
          ENDIF.

          APPEND tl_input_zlest0060.
          "APPEND tl_input_zlest0073.

        ENDIF.

        CLEAR: gw_saida_vinc_rom_nome, wl_lfa1, wl_kna1.
      ENDLOOP.

      IF NOT ( tl_input_zlest0060[] IS INITIAL ).
        "Gravar as notas selecionadas na ZLEST0060
        MODIFY zlest0060 FROM TABLE tl_input_zlest0060.

        "Validação Aplicação Retenção.
        DATA(_error) = ''.
        LOOP AT gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome WHERE check IS NOT INITIAL
                                                                     AND xml  = icon_led_green
                                                                     AND desc_ret_acum > 0.

          CHECK ( gw_saida_vinc_rom_nome-desc_ret_acum_grv IS INITIAL ). " Aplicar desconto se retenção ainda não foi distribuida

          PERFORM f_retencao_nf USING '' "Aplicar Desconto
                                      gw_saida_vinc_rom_nome-desc_ret_acum
                             CHANGING _error.

          IF _error IS NOT INITIAL.
            ROLLBACK WORK.
            RETURN.
          ENDIF.

        ENDLOOP.

        "Gravar na tabela ZLEST0073 controle de saldo
        LOOP AT gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome WHERE check = 'X'
                                                                     AND estilo IS INITIAL.
          "Atualizar o romaneio na ZSDT0001
          IF gw_saida_vinc_rom_nome-check_complemento IS NOT INITIAL.
            UPDATE zlest0205 SET ct_aquav = 'X'
             WHERE chave_nfe EQ gw_saida_vinc_rom_nome-chave_nfe
               AND chave_nfe NE space.
          ELSE.
            UPDATE zsdt0001 SET ct_aquav = 'X' WHERE tp_movimento EQ 'E'
                                                 AND nr_romaneio  EQ gw_saida_vinc_rom_nome-nr_romaneio
                                                 AND nr_safra     EQ gw_saida_vinc_rom_nome-safra
                                                 AND bukrs        EQ wg_tela1200-bukrs_rom
                                                 AND branch       EQ wg_tela1200-branch_rom
                                                 AND parid        EQ wg_tela1200-rm_codigo.
          ENDIF.

          PERFORM f_atualiza_saldo_nf USING gw_saida_vinc_rom_nome.


          SELECT SINGLE *
            FROM zlest0073 INTO @DATA(lwa_0073)
           WHERE chave_nfe EQ @gw_saida_vinc_rom_nome-chave_nfe.

          IF ( sy-subrc EQ 0 ) AND ( lwa_0073-peso_vinculado > lwa_0073-peso_origem ).
            ROLLBACK WORK.
            MESSAGE |Vinculação da Nota fiscal Chave: { gw_saida_vinc_rom_nome-chave_nfe } com peso excedido! Atualizar Consulta!| TYPE 'I'.
            RETURN.
          ENDIF.

        ENDLOOP.

        COMMIT WORK.

        CLEAR: v_gravado, total_vinculado.

        PERFORM: seleciona_notas_fiscais.

        IF NOT ( gt_saida_vinc_nf_nome[] IS INITIAL ).
          REFRESH: tl_input_zlest0060, tl_input_zlest0073.

          "Deletar Registro no Controle de Remetente e Destinatario.
          vg_remetente    = wg_tela1200-rm_codigo.
          vg_destinatario = wg_tela1200-dt_codigo.
          opcao = 'D'.
          PERFORM: controle_remet_destin USING vg_remetente vg_destinatario opcao.

          CLEAR: wp_frete_id, wg_tela1200-id_frete_aqua, wg_tela1200-id_local_descarga, wg_tela1200-ds_local_descarga.

          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.


    WHEN: '&EXIT'.
      REFRESH: gt_saida_vinc_rom_nome.
      CLEAR:   gw_saida_vinc_rom_nome.

      "Deletar Registro no Controle de Remetente e Destinatario.
      vg_remetente    = wg_tela1200-rm_codigo.
      vg_destinatario = wg_tela1200-dt_codigo.
      opcao = 'D'.
      PERFORM: controle_remet_destin USING vg_remetente vg_destinatario opcao.

      CLEAR: total_vinculado.

      CLEAR: wp_frete_id, wg_tela1200-id_frete_aqua, wg_tela1200-id_local_descarga, wg_tela1200-ds_local_descarga.
      LEAVE TO SCREEN 0.

    WHEN: 'ATRIB_PESO'. "Adequações Retenção CS2016001199

      DATA: vl_saldo_vinc TYPE brgew,
            vl_vlr_vinc   TYPE brgew.

      CHECK wg_tela1200-peso_avinc > 0.

      LOOP AT gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome WHERE check IS INITIAL
                                                                   AND xml = icon_led_green
                                                                   AND estilo IS INITIAL.

        CLEAR: vl_saldo_vinc, tabix, vl_vlr_vinc.

        tabix = sy-tabix.

        vl_saldo_vinc = wg_tela1200-peso_avinc - total_vinculado.

        IF vl_saldo_vinc <= 0.
          EXIT.
        ENDIF.

        gw_saida_vinc_rom_nome-check           = 'X'.

        IF gw_saida_vinc_rom_nome-vinc_fiscal IS INITIAL. "17.01.2018 - CS2018000076

          IF vl_saldo_vinc < gw_saida_vinc_rom_nome-peso_liq_ret.

            "Verifica se centro permitire vinculação parcial da NF. Caso não, encerra vinculação.
*            IF VG_VINC_NF_PARC IS INITIAL.
*              RETURN.
*            ENDIF.

            PERFORM f_atrib_peso USING gw_saida_vinc_rom_nome
                                       vl_saldo_vinc
                                       ' '  "Vinculo Total
                                       '2'. "Peso Retido
          ELSE.
            PERFORM f_atrib_peso USING gw_saida_vinc_rom_nome
                                       gw_saida_vinc_rom_nome-peso_liq_ret
                                       'X'  "Vinculo Total
                                       '2'. "Peso Retido
          ENDIF.

          vl_vlr_vinc = gw_saida_vinc_rom_nome-peso_liq_ret_vinc.

        ELSE.
          IF vl_saldo_vinc < gw_saida_vinc_rom_nome-peso_fiscal.

            "Verifica se centro permitire vinculação parcial da NF. Caso não, encerra vinculação.
*            IF VG_VINC_NF_PARC IS INITIAL.
*              RETURN.
*            ENDIF.

            PERFORM f_atrib_peso USING gw_saida_vinc_rom_nome
                                       vl_saldo_vinc
                                       ' '  "Vinculo Total
                                       '1'. "Peso Fiscal
          ELSE.
            PERFORM f_atrib_peso USING gw_saida_vinc_rom_nome
                                       gw_saida_vinc_rom_nome-peso_fiscal
                                       'X'  "Vinculo Total
                                       '1'. "Peso Fiscal
          ENDIF.

          vl_vlr_vinc = gw_saida_vinc_rom_nome-peso_vinc.

        ENDIF.

        ADD vl_vlr_vinc TO total_vinculado.

        IF vl_vlr_vinc IS INITIAL.
          gw_saida_vinc_rom_nome-check = ''.
          CONTINUE.
        ENDIF.

        MODIFY gt_saida_vinc_rom_nome FROM gw_saida_vinc_rom_nome INDEX tabix.
        "TRANSPORTING CHECK ESTILO PESO_VINC VALOR_VINCULADO PESO_LIQ_RET_VINC VALOR_LIQ_RET_VINC PESO_SUBTOTAL_VINC PERC_RET PESO_RETIDO.

        CLEAR: gw_saida_comboio.
        READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX index_comboio.
        gw_saida_comboio-peso_vinculado = total_vinculado.

        MODIFY gt_saida_comboio FROM gw_saida_comboio INDEX index_comboio TRANSPORTING peso_vinculado.

      ENDLOOP.

      PERFORM f_atualiza_saldo_retencao USING ''.

*        wa_stable-row = 'X'.
*        wa_stable-col = 'X'.
*        CALL METHOD obj_grid_romaneio->refresh_table_display
*          EXPORTING
*            is_stable = wa_stable.


      LEAVE TO SCREEN 1200.

    WHEN 'DESELECT_ALL'.

      PERFORM f_deselect_all USING ''.

      PERFORM f_atualiza_saldo_retencao USING ''.

      CLEAR: wp_frete_id, wg_tela1200-id_frete_aqua, wg_tela1200-id_local_descarga, wg_tela1200-ds_local_descarga.
      LEAVE TO SCREEN 1200.

    WHEN 'DESC_RETENCAO'.

      PERFORM f_desconta_retencao.

      CLEAR: wp_frete_id, wg_tela1200-id_frete_aqua, wg_tela1200-id_local_descarga, wg_tela1200-ds_local_descarga.
      LEAVE TO SCREEN 1200.

    WHEN 'TRLC'.

      PERFORM trocar_local_descarga USING wp_werks  gw_saida_viagem-po_embarque CHANGING wg_tela1200-id_local_descarga wg_tela1200-ds_local_descarga sy-subrc.
      IF wg_tela1200-id_local_descarga IS NOT INITIAL.
        PERFORM f_action_1200_enter_nf.
      ENDIF.

  ENDCASE.


ENDMODULE.                 " USER_COMMAND_1200  INPUT
*&---------------------------------------------------------------------*
*&      Module  LIST_BOX  INPUT
*&---------------------------------------------------------------------*
MODULE list_tomador INPUT.

  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_tomador OCCURS 0,
          codigo       TYPE zlest0057-embarcacao,
          tomador_serv TYPE zlest0057-tomador_serv,
        END OF tl_tomador.

  REFRESH  tl_tomador.
  tl_tomador-codigo = 'R'.
  tl_tomador-tomador_serv   = 'Remetente'.
  APPEND tl_tomador.

  tl_tomador-codigo = 'D'.
  tl_tomador-tomador_serv   = 'Destinatário'.
  APPEND tl_tomador.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CODIGO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WG_TELA1200-TOMADOR_COD'
      value_org       = 'S'
    TABLES
      value_tab       = tl_tomador
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDMODULE.                 " LIST_BOX  INPUT
*&---------------------------------------------------------------------*
*&      Form  CARREGAR_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM carregar_fatura .

  DATA: tl_zlest0057      TYPE TABLE OF zlest0057, "tabela com o resumo de notas fiscais vinculadas
        wl_zlest0057      TYPE          zlest0057, "Work Area com o resumo de notas fiscais vinculadas
        tl_zlest0060      TYPE TABLE OF zlest0060, "
        wl_zlest0060      TYPE          zlest0060, "
        tl_zlest0061      TYPE TABLE OF zlest0061, "
        wl_zlest0061      TYPE          zlest0061, "
        tl_zlest0063      TYPE TABLE OF zlest0063, "tabela de Comboios - Frete Aquaviário - Viagem
        wl_zlest0063      TYPE          zlest0063, "Work Area tabela de Comboios - Frete Aquaviário - Viagem
        tl_zlest0055      TYPE TABLE OF zlest0055,
        tl_zlest0059      TYPE TABLE OF zlest0059,
        wl_zlest0059      TYPE zlest0059,
        wl_zlest0055      TYPE zlest0055,
        tl_kna1           TYPE TABLE OF kna1,
        wl_kna1           TYPE kna1,
        tl_lfa1           TYPE TABLE OF lfa1,
        wl_lfa1           TYPE lfa1,
        tl_makt           TYPE TABLE OF makt,
        wl_makt           TYPE makt,

        wl_j_1bnfe_active TYPE j_1bnfe_active,
        wa_j_1bnflin      TYPE  j_1bnflin,
        wa_j_1bnfdoc      TYPE  j_1bnfdoc,
        wl_setleaf        TYPE setleaf,

        w_cont61          TYPE i,
        vbfa_vbeln        TYPE  vbfa-vbeln,
        vdocnum           TYPE  j_1bnflin-docnum,
        nnota             TYPE  j_1bnfdoc-nfenum,
        tl_j_1bnfe_active TYPE TABLE OF j_1bnfe_active,
        tl_j_1bnfdoc      TYPE TABLE OF j_1bnfdoc,
        tabix             TYPE sy-tabix,
        stcd1_str         TYPE c  LENGTH 9,
        stcd1_conc        TYPE c  LENGTH 4,
        qtd_linha         TYPE sy-tabix.

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  REFRESH: gt_saida_ger_ov, tl_lfa1, tl_kna1.
  CLEAR: wl_lfa1, wl_kna1.

  CLEAR: tl_rows, sl_rows.

  CALL METHOD obj_grid_vinc_nf->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  DESCRIBE TABLE tl_rows LINES qtd_linha.

  IF ( qtd_linha EQ 1 ).

    READ TABLE tl_rows INDEX 1 INTO sl_rows.
    READ TABLE gt_saida_vinc_nf_nome INDEX sl_rows-index INTO gw_saida_vinc_nf_nome.

    IF ( sy-subrc EQ 0 ).

      IF gw_saida_vinc_nf_nome-id_frete_aqua IS NOT INITIAL.
        SELECT * FROM zlest0061
          INTO TABLE @tl_zlest0061
           WHERE id_frete_aqua EQ @gw_saida_vinc_nf_nome-id_frete_aqua.
      ELSE.

        SELECT * FROM zlest0061
          INTO TABLE tl_zlest0061
           WHERE bukrs           EQ w_bukrs
             AND werks           EQ w_werks
             AND ano_viagem      EQ w_ano
             AND nr_viagem       EQ w_viagem
             AND embarcacao      EQ gw_saida_vinc_nf_nome-embarcacao
             AND nome_emb        EQ gw_saida_vinc_nf_nome-nome
             AND rm_codigo       EQ gw_saida_vinc_nf_nome-rm_codigo
             AND nr_dco          EQ gw_saida_vinc_nf_nome-nr_dco
             AND safra           EQ gw_saida_vinc_nf_nome-safra
             AND operacao        EQ gw_saida_vinc_nf_nome-operacao.

        IF sy-subrc NE 0.
          SELECT * FROM zlest0061
            INTO TABLE tl_zlest0061
             WHERE bukrs           EQ w_bukrs
               AND werks           EQ w_werks
               AND ano_viagem      EQ w_ano
               AND nr_viagem       EQ w_viagem
               AND embarcacao      EQ gw_saida_vinc_nf_nome-embarcacao
               AND nome_emb        EQ gw_saida_vinc_nf_nome-nome
               AND cl_codigo       EQ gw_saida_vinc_nf_nome-cl_codigo
               AND rm_codigo       EQ ''
               AND nr_dco          EQ gw_saida_vinc_nf_nome-nr_dco
               AND safra           EQ gw_saida_vinc_nf_nome-safra
               AND operacao        EQ gw_saida_vinc_nf_nome-operacao.
        ENDIF.
      ENDIF.

      CASE sy-subrc.

        WHEN: '0'.

          REFRESH: tl_j_1bnfe_active, tl_makt.
          CLEAR: wl_j_1bnfe_active, wl_makt.

          SELECT * FROM j_1bnfe_active
            INTO TABLE tl_j_1bnfe_active
            FOR ALL ENTRIES IN tl_zlest0061
          WHERE docnum EQ tl_zlest0061-docnum.

          SELECT * FROM j_1bnfdoc
            INTO TABLE tl_j_1bnfdoc
            FOR ALL ENTRIES IN tl_zlest0061
          WHERE docnum EQ tl_zlest0061-docnum.

          SELECT * FROM makt
            INTO TABLE tl_makt
            FOR ALL ENTRIES IN tl_zlest0061
          WHERE matnr EQ tl_zlest0061-cod_material
            AND spras EQ sy-langu.

          SELECT * FROM lfa1
            INTO TABLE tl_lfa1
            FOR ALL ENTRIES IN tl_zlest0061
          WHERE lifnr EQ tl_zlest0061-cl_codigo.

          SELECT * FROM kna1
            INTO TABLE tl_kna1
            FOR ALL ENTRIES IN tl_lfa1
          WHERE kunnr EQ tl_lfa1-kunnr.

          REFRESH: gt_saida_ger_ov.
          CLEAR: gw_saida_ger_ov.

          LOOP AT tl_zlest0061 INTO wl_zlest0061.

            MOVE: wl_zlest0061-embarcacao    TO gw_saida_ger_ov-embarcacao,
                  wl_zlest0061-nome_emb      TO gw_saida_ger_ov-nome,
                  wl_zlest0061-id_frete_aqua TO gw_saida_ger_ov-id_frete_aqua.

            READ TABLE tl_makt INTO wl_makt WITH KEY matnr = wl_zlest0061-cod_material.
            MOVE: wl_makt-maktx TO gw_saida_ger_ov-maktx.

            CASE wl_zlest0061-tp_class.
              WHEN: 'CO'.
                gw_saida_ger_ov-tp_class = 'CO-Convencional'.
              WHEN: 'R1'.
                gw_saida_ger_ov-tp_class = 'R1-RR'.
              WHEN: 'R2'.
                gw_saida_ger_ov-tp_class = 'R2-RR2'.
            ENDCASE.

            MOVE: wl_zlest0061-peso_vinculado TO gw_saida_ger_ov-peso_vinculado,
                  wl_zlest0061-auart          TO gw_saida_ger_ov-auart,
                  wl_zlest0061-waerk          TO gw_saida_ger_ov-waerk,
                  wl_zlest0061-waerk_fatura   TO gw_saida_ger_ov-waerk_fatura,
                  wl_zlest0061-tomador_serv   TO gw_saida_ger_ov-tomador_serv,
                  wl_zlest0061-netpr          TO gw_saida_ger_ov-netpr,
                  wl_zlest0061-tax_dolar      TO gw_saida_ger_ov-tax_dolar,
                  wl_zlest0061-vlr_usd        TO gw_saida_ger_ov-vlr_usd,
                  wl_zlest0061-vlr_brl        TO gw_saida_ger_ov-vlr_brl,
                  wl_zlest0061-nr_ov          TO gw_saida_ger_ov-nr_ov,
                  wl_zlest0061-fatura         TO gw_saida_ger_ov-fatura,
                  wl_zlest0061-docnum         TO gw_saida_ger_ov-docnum,
                  wl_zlest0061-dt_fatura      TO gw_saida_ger_ov-dt_fatura,
                  wl_zlest0061-id_frete_aqua  TO gw_saida_ger_ov-id_frete_aqua.

            "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - inicio
            CASE wl_zlest0061-eudr .
              WHEN 'S'.
                gw_saida_ger_ov-eudr = 'S-Atende'.
              WHEN 'N'.
                gw_saida_ger_ov-eudr = 'N-Não Atende'.
              WHEN OTHERS.
            ENDCASE.
            "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328 - fim

            READ TABLE tl_makt INTO wl_makt WITH KEY matnr = wl_zlest0061-cod_material.
            gw_saida_ger_ov-maktx = wl_makt-maktx.

            gw_saida_ger_ov-cl_codigo  = wl_zlest0061-cl_codigo.
            gw_saida_ger_ov-rm_codigo  = wl_zlest0061-rm_codigo.

            READ TABLE tl_j_1bnfe_active INTO wl_j_1bnfe_active WITH KEY docnum = wl_zlest0061-docnum.

            IF ( sy-subrc EQ 0 ).

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = wl_j_1bnfe_active-nfnum9
                IMPORTING
                  output = wl_j_1bnfe_active-nfnum9.


              CASE wl_j_1bnfe_active-docsta.
                WHEN: '1'.
                  CONCATENATE icon_complete '-' wl_j_1bnfe_active-nfnum9 INTO gw_saida_ger_ov-nfnum SEPARATED BY space.
                WHEN: '2' OR '3'.
                  CONCATENATE icon_defect '-' wl_j_1bnfe_active-nfnum9 INTO gw_saida_ger_ov-nfnum SEPARATED BY space.
                WHEN OTHERS.
                  IF ( wl_j_1bnfe_active-docsta IS INITIAL ) AND ( wl_j_1bnfe_active-scssta IS INITIAL ).
                    CONCATENATE icon_warning '-' wl_j_1bnfe_active-nfnum9 INTO gw_saida_ger_ov-nfnum SEPARATED BY space.
                  ELSEIF ( wl_j_1bnfe_active-docsta IS INITIAL ) AND ( wl_j_1bnfe_active-scssta EQ 0 ).
                    CONCATENATE icon_activity '-' wl_j_1bnfe_active-nfnum9 INTO gw_saida_ger_ov-nfnum SEPARATED BY space.
                  ENDIF.
              ENDCASE.


            ELSE.
              CLEAR: wa_j_1bnfdoc.
              READ TABLE tl_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = wl_zlest0061-docnum.

              IF ( wa_j_1bnfdoc-cancel EQ 'X' ).
                CONCATENATE icon_defect '-' wa_j_1bnfdoc-nfnum INTO gw_saida_ger_ov-nfnum SEPARATED BY space.

              ELSE.
                CONCATENATE icon_complete '-' wa_j_1bnfdoc-nfnum INTO gw_saida_ger_ov-nfnum SEPARATED BY space.
              ENDIF.

            ENDIF.


            APPEND gw_saida_ger_ov TO gt_saida_ger_ov.
            CLEAR: gw_saida_ger_ov, wl_j_1bnfe_active.

          ENDLOOP.

          CHECK NOT gt_saida_ger_ov[] IS INITIAL.

          REFRESH: gt_fcat_ger_ov.
          CLEAR: gw_fcat_ger_ov.

          "Desabilita os campos de edição caso já tenho sido preenchidos.
          LOOP AT gt_saida_ger_ov INTO gw_saida_ger_ov.

            REFRESH: tl_estilo_ov.
            CLEAR: tabix, wl_estilo_ov.

            tabix = sy-tabix.

            IF NOT ( gw_saida_ger_ov-auart IS INITIAL ) AND NOT ( gw_saida_ger_ov-waerk IS INITIAL ).

              CLEAR: wl_estilo_ov.
              wl_estilo_ov-fieldname = 'AUART'.
              wl_estilo_ov-style = cl_gui_alv_grid=>mc_style_disabled.
              APPEND wl_estilo_ov TO tl_estilo_ov.

              IF ( gw_saida_ger_ov-waerk_fatura IS NOT INITIAL ) AND
                 ( gw_saida_ger_ov-waerk_fatura NE 'USD' ).
                CLEAR: wl_estilo_ov.
                wl_estilo_ov-fieldname = 'TAX_DOLAR'.
                wl_estilo_ov-style = cl_gui_alv_grid=>mc_style_disabled.
                APPEND wl_estilo_ov TO tl_estilo_ov.
              ENDIF.

              CLEAR: wl_estilo_ov.
              wl_estilo_ov-fieldname = 'WAERK'.
              wl_estilo_ov-style = cl_gui_alv_grid=>mc_style_disabled.
              APPEND wl_estilo_ov TO tl_estilo_ov.

              INSERT LINES OF tl_estilo_ov INTO TABLE gw_saida_ger_ov-estilo.
              MODIFY gt_saida_ger_ov FROM gw_saida_ger_ov INDEX tabix.
            ENDIF.

            CLEAR: wl_estilo_ov, tabix.
          ENDLOOP.

          PERFORM: criar_alv_ger_ov.

          "Verifica quantas linhas tem no GW_SAIDA_GER_OV
          CLEAR: qtd_linha.
          DESCRIBE TABLE tl_zlest0061 LINES qtd_linha.

          IF ( qtd_linha EQ 1 ).

            "Verifica se o centro emite nota fiscal de serviço
            SELECT SINGLE * FROM setleaf INTO wl_setleaf
               WHERE setname EQ 'MAGGI_ZLES0077_NFPS'
                 AND valfrom EQ w_werks.

            IF ( sy-subrc NE 0 ).

              REFRESH: tl_zlest0057, tl_zlest0063, tl_zlest0060, tl_makt.
              CLEAR: wl_zlest0057, gw_saida_ger_ov, wl_zlest0060, wl_zlest0063, wl_makt, wl_setleaf, wl_lfa1, wl_kna1.


              SELECT  *
                 FROM zlest0057
                 INTO  TABLE  tl_zlest0057
                WHERE bukrs           EQ w_bukrs
                  AND werks           EQ w_werks
                  AND ano_viagem      EQ w_ano
                  AND nr_viagem       EQ w_viagem
                  AND embarcacao      EQ gw_saida_vinc_nf_nome-embarcacao
                  AND rm_codigo       EQ gw_saida_vinc_nf_nome-rm_codigo
                  AND dt_codigo       EQ gw_saida_vinc_nf_nome-dt_codigo
                  AND dt_movimento    EQ gw_saida_vinc_nf_nome-dt_movimento
                  AND peso_vinculado  EQ gw_saida_vinc_nf_nome-peso_vinculado
                  AND nome_emb        EQ gw_saida_vinc_nf_nome-nome
                  AND nr_dco          EQ gw_saida_vinc_nf_nome-nr_dco
                  AND safra           EQ gw_saida_vinc_nf_nome-safra.

              CHECK tl_zlest0057[] IS NOT INITIAL.

              SELECT * FROM zlest0063
                INTO TABLE tl_zlest0063
                FOR ALL ENTRIES IN tl_zlest0057
              WHERE embarcacao  EQ tl_zlest0057-embarcacao
                AND ano_viagem  EQ tl_zlest0057-ano_viagem
                AND nr_viagem   EQ tl_zlest0057-nr_viagem
                AND nome_emb    EQ tl_zlest0057-nome_emb.


              SELECT * FROM zlest0060
                INTO TABLE tl_zlest0060
                FOR ALL ENTRIES IN tl_zlest0057
              WHERE bukrs      EQ tl_zlest0057-bukrs
                AND werks      EQ tl_zlest0057-werks
                AND nr_viagem  EQ tl_zlest0057-nr_viagem
                AND ano_viagem EQ tl_zlest0057-ano_viagem
                AND embarcacao EQ tl_zlest0057-embarcacao
                AND nome_emb   EQ tl_zlest0057-nome_emb
                AND rm_codigo  EQ tl_zlest0057-rm_codigo
                AND dt_codigo  EQ tl_zlest0057-dt_codigo
                AND nr_dco     EQ tl_zlest0057-nr_dco
                AND safra      EQ tl_zlest0057-safra.

              SELECT * FROM makt
                INTO TABLE tl_makt
                FOR ALL ENTRIES IN tl_zlest0063
              WHERE matnr EQ tl_zlest0063-cod_material
                AND spras EQ sy-langu.


              LOOP AT tl_zlest0057 INTO wl_zlest0057.

                MOVE: wl_zlest0057-embarcacao           TO gw_saida_ger_ov-embarcacao,
                      wl_zlest0057-nome_emb             TO gw_saida_ger_ov-nome,
                      wl_zlest0057-peso_vinculado       TO gw_saida_ger_ov-peso_vinculado,
                      wl_zlest0057-dt_movimento         TO gw_saida_ger_ov-dt_movimento,
                      wl_zlest0057-nr_dco               TO gw_saida_ger_ov-nr_dco,
                      wl_zlest0057-safra                TO gw_saida_ger_ov-safra,
                      sy-datum                          TO gw_saida_ger_ov-dt_fatura.

                IF wl_zlest0057-tomador_serv = 'REMETENTE'.
                  gw_saida_ger_ov-cl_codigo = wl_zlest0057-rm_codigo.
                ELSE.
                  gw_saida_ger_ov-cl_codigo = wl_zlest0057-dt_codigo.
                ENDIF.

                SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ gw_saida_ger_ov-cl_codigo.
                SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ wl_lfa1-kunnr.
                IF ( sy-subrc EQ 0 ).
                  gw_saida_ger_ov-cl_codigo = wl_kna1-kunnr.

                ENDIF.

                READ TABLE tl_zlest0060 INTO wl_zlest0060 WITH KEY bukrs      = wl_zlest0057-bukrs
                                                                   werks      = wl_zlest0057-werks
                                                                   nr_viagem  = wl_zlest0057-nr_viagem
                                                                   ano_viagem = wl_zlest0057-ano_viagem
                                                                   embarcacao = wl_zlest0057-embarcacao
                                                                   nome_emb   = wl_zlest0057-nome_emb
                                                                   rm_codigo  = wl_zlest0057-rm_codigo
                                                                   dt_codigo  = wl_zlest0057-dt_codigo
                                                                   nr_dco     = wl_zlest0057-nr_dco
                                                                   safra      = wl_zlest0057-safra.

                gw_saida_ger_ov-nr_romaneio = wl_zlest0060-nr_romaneio.


                READ TABLE tl_zlest0063 INTO wl_zlest0063 WITH KEY embarcacao = wl_zlest0057-embarcacao
                                                                   ano_viagem = wl_zlest0057-ano_viagem
                                                                   nr_viagem  = wl_zlest0057-nr_viagem
                                                                   nome_emb   = wl_zlest0057-nome_emb.

                READ TABLE tl_makt INTO wl_makt WITH KEY matnr = wl_zlest0063-cod_material
                                                         spras = 'P'.


                gw_saida_ger_ov-matnr        = wl_zlest0063-cod_material.
                gw_saida_ger_ov-maktx        = wl_makt-maktx.


                CASE wl_zlest0063-tp_class.
                  WHEN: 'CO'.
                    gw_saida_ger_ov-tp_class = 'CO-Convencional'.
                  WHEN: 'R1'.
                    gw_saida_ger_ov-tp_class = 'R1-RR'.
                  WHEN: 'R2'.
                    gw_saida_ger_ov-tp_class = 'R2-RR2'.
                ENDCASE.

                APPEND gw_saida_ger_ov TO gt_saida_ger_ov.

              ENDLOOP.
            ENDIF.

          ENDIF.

          CALL METHOD obj_grid_ger_ov->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        WHEN  OTHERS.

          IF NOT ( gw_saida_vinc_nf_nome IS INITIAL ).
            CLEAR: gw_saida_comboio, gw_saida_ger_ov.

            MOVE: gw_saida_vinc_nf_nome-id_frete_aqua  TO gw_saida_ger_ov-id_frete_aqua,
                  gw_saida_vinc_nf_nome-embarcacao     TO gw_saida_ger_ov-embarcacao,
                  gw_saida_vinc_nf_nome-nome           TO gw_saida_ger_ov-nome,
                  gw_saida_vinc_nf_nome-peso_vinculado TO gw_saida_ger_ov-peso_vinculado,
                  gw_saida_vinc_nf_nome-dt_movimento   TO gw_saida_ger_ov-dt_movimento,
                  gw_saida_vinc_nf_nome-nr_dco         TO gw_saida_ger_ov-nr_dco,
                  gw_saida_vinc_nf_nome-safra          TO gw_saida_ger_ov-safra,
                  gw_saida_vinc_nf_nome-tomador_serv   TO gw_saida_ger_ov-tomador_serv,
                  gw_saida_vinc_nf_nome-matnr          TO gw_saida_ger_ov-matnr.
*S4H - ZLES0077 - Contingencia-Data Fatura-09.10.2023-JT-inicio
            MOVE: l_data_fatura                        TO gw_saida_ger_ov-dt_fatura.
*                 sy-datum                             TO gw_saida_ger_ov-dt_fatura.
*S4H - ZLES0077 - Contingencia-Data Fatura-09.10.2023-JT-inicio



            IF gw_saida_vinc_nf_nome-tomador_serv = 'R'.
              gw_saida_ger_ov-cl_codigo = gw_saida_vinc_nf_nome-rm_codigo.
              SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ gw_saida_ger_ov-cl_codigo.
              IF sy-subrc EQ 0.
                SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ wl_lfa1-kunnr.
                IF ( sy-subrc EQ 0 ).
                  gw_saida_ger_ov-cl_codigo = wl_kna1-kunnr.
                ENDIF.
              ENDIF.
            ELSE.
              gw_saida_ger_ov-cl_codigo = gw_saida_vinc_nf_nome-dt_codigo.
            ENDIF.

            gw_saida_ger_ov-rm_codigo = gw_saida_vinc_nf_nome-rm_codigo.

            READ TABLE gt_saida_comboio INDEX index_comboio INTO gw_saida_comboio.

            gw_saida_ger_ov-tp_class = gw_saida_comboio-tp_class.
            gw_saida_ger_ov-eudr     = gw_saida_comboio-eudr. "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328

            IF gw_saida_vinc_nf_nome-matnr IS NOT INITIAL.

              SELECT SINGLE * FROM makt INTO wl_makt WHERE matnr EQ gw_saida_vinc_nf_nome-matnr
                                                       AND spras EQ 'P'.
            ELSE.

              SELECT SINGLE * FROM makt INTO wl_makt WHERE matnr EQ gw_saida_comboio-matnr
                                                           AND spras EQ 'P'.
            ENDIF.

            gw_saida_ger_ov-matnr        = wl_makt-matnr.
            gw_saida_ger_ov-maktx        = wl_makt-maktx.

            SELECT SINGLE * FROM setleaf INTO wl_setleaf
               WHERE setname EQ 'MAGGI_ZLES0077_NFPS'
                 AND valfrom EQ w_werks.

            IF ( sy-subrc NE 0 ).
              APPEND gw_saida_ger_ov TO gt_saida_ger_ov.
            ELSE.
              APPEND gw_saida_ger_ov TO gt_saida_ger_ov.
              APPEND gw_saida_ger_ov TO gt_saida_ger_ov.
            ENDIF.

          ENDIF.

          REFRESH: gt_fcat_ger_ov.
          CLEAR: gw_fcat_ger_ov.
          PERFORM: criar_alv_ger_ov.

      ENDCASE.
    ENDIF.
  ELSE.
    MESSAGE s000(zwrm001) DISPLAY LIKE 'W' WITH 'Selecionar somente uma linha.'.
  ENDIF.
ENDFORM.                    " CARREGAR_FATURA
*&---------------------------------------------------------------------*
*&      Form  F_SHDB_VF01
*&---------------------------------------------------------------------*
FORM f_shdb_vf01 CHANGING p_vbeln LIKE bapivbeln-vbeln p_erro.

  DATA: t_success TYPE TABLE OF bapivbrksuccess,
        t_billing TYPE TABLE OF bapivbrk,
        t_return  TYPE TABLE OF bapireturn1.

  t_billing = VALUE #( (
                        ref_doc       = p_vbeln
                        ref_doc_ca    = 'C'
                     ) ).

  CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE' "#EC CI_USAGE_OK[2438131]
    TABLES
      billingdatain = t_billing
      return        = t_return
      success       = t_success.

  IF t_success IS NOT INITIAL.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

*    WAIT UP TO 2 SECONDS.

    TRY.
        p_vbeln = t_success[ 1 ]-bill_doc.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    p_erro = abap_true.
  ENDIF.

*  DATA: WL_DATA_FATURA TYPE C LENGTH 10,
*        WL_MSG         TYPE BDCMSGCOLL,
*        VL_OK          TYPE C.
*
*  DATA OPT TYPE CTU_PARAMS.
*
*  CLEAR: WL_DATA_FATURA.
*
*  CONCATENATE SY-DATUM+6(2) '.' SY-DATUM+4(2) '.' SY-DATUM(4) INTO WL_DATA_FATURA.
*  REFRESH TI_BDCDATA.
*
*  PERFORM F_BDC_DATA USING:
*      'SAPMV60A'  '0102'  'X'  ''                 ' ',
*      ''          ''      ''   'BDC_OKCODE'        '/00',
*      ''          ''      ''   'RV60A-FKDAT'      WL_DATA_FATURA,
*      ''          ''      ''   'KOMFK-VBELN(01)'  P_VBELN,
*      'SAPMV60A'  '0104'  'X'  ''                 ' ',
*      ''          ''      ''   'BDC_OKCODE'        '=SICH'.
*
*  CLEAR P_ERRO.
*
*  REFRESH: IT_MSG.
*
*  CALL TRANSACTION 'VF01' USING TI_BDCDATA MODE 'S' MESSAGES INTO IT_MSG.

*  CLEAR: VL_OK.
*  LOOP AT IT_MSG INTO WL_MSG WHERE MSGTYP EQ 'S'.
*    IF ( WL_MSG-MSGV1(1) EQ '9').
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = WL_MSG-MSGV1
*        IMPORTING
*          OUTPUT = P_VBELN.
*      VL_OK = 'X'.
*    ENDIF.
*    CLEAR: WL_MSG.
*  ENDLOOP.

*  CASE VL_OK.
*    WHEN: 'X'.
*      COMMIT WORK.
*    WHEN OTHERS.
*      ROLLBACK WORK.
*      P_ERRO = 'X'.
*  ENDCASE.
ENDFORM.                    " F_SHDB_VF01

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_1300  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_1300 OUTPUT.

  SET PF-STATUS 'PF1300'.
  SET TITLEBAR  'TB1300'.

  PERFORM: seleciona_quantidade_comboio,
           criar_alv_vinc_geral.

ENDMODULE.                 " STATUS_1300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1300  INPUT
*&---------------------------------------------------------------------*
MODULE pai_1300 INPUT.

  DATA: tipo_sf TYPE c LENGTH 3.

  CASE ok_code.
    WHEN: 'FECHAR'.
      LEAVE TO SCREEN 0.

    WHEN: 'SF_CTE'.
      CLEAR: tipo_sf.
      tipo_sf = 'SFC'.
      PERFORM: z_smartform USING tipo_sf.
    WHEN: 'SF_BALSA'.
      CLEAR: tipo_sf.
      tipo_sf = 'SFB'.
      PERFORM: z_smartform USING tipo_sf.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1300  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_QUANTIDADE_COMBOIO
*&---------------------------------------------------------------------*
FORM seleciona_quantidade_comboio .

  DATA: tl_zlest0061      TYPE TABLE OF zlest0061,
        wl_zlest0061      TYPE zlest0061,
        tl_zlest0060      TYPE TABLE OF zlest0060,
        wl_zlest0060      TYPE zlest0060,
        tl_j_1bnfdoc      TYPE TABLE OF j_1bnfdoc,
        wl_j_1bnfdoc      TYPE j_1bnfdoc,
        tl_j_1bfne_active TYPE TABLE OF j_1bnfe_active,
        wl_j_1bnfe_active TYPE j_1bnfe_active,
        tl_makt           TYPE TABLE OF makt,
        wl_makt           TYPE makt,
        tl_zlest0063      TYPE TABLE OF zlest0063,
        wl_zlest0063      TYPE zlest0063.


  "PERFORM F_CENTRO_CALC_RET USING W_WERKS. "Adequações Retenção CS2016001199

  REFRESH: gt_saida_vinc_nf, tl_zlest0061, tl_j_1bnfdoc, tl_makt.
  CLEAR: gw_saida_vinc_nf, wl_zlest0061, wl_j_1bnfdoc, wl_makt.

  SELECT * FROM zlest0061
    INTO TABLE tl_zlest0061
    WHERE bukrs      EQ w_bukrs
      AND werks      EQ w_werks
      AND ano_viagem EQ w_ano
      AND nr_viagem  EQ w_viagem.

  CHECK NOT tl_zlest0061[] IS INITIAL.

  SELECT * FROM zlest0063
    INTO TABLE tl_zlest0063
    FOR ALL ENTRIES IN tl_zlest0061
  WHERE bukrs      EQ tl_zlest0061-bukrs
    AND werks      EQ tl_zlest0061-werks
    AND ano_viagem EQ tl_zlest0061-ano_viagem
    AND nr_viagem  EQ tl_zlest0061-nr_viagem
    AND nome_emb   EQ tl_zlest0061-nome_emb
    AND tp_class   EQ tl_zlest0061-tp_class.

  SELECT * FROM zlest0060
    INTO TABLE tl_zlest0060
    FOR ALL ENTRIES IN tl_zlest0061
  WHERE bukrs         EQ tl_zlest0061-bukrs
    AND werks         EQ tl_zlest0061-werks
    AND nr_viagem     EQ tl_zlest0061-nr_viagem
    AND ano_viagem    EQ tl_zlest0061-ano_viagem
    AND embarcacao    EQ tl_zlest0061-embarcacao
    AND nome_emb      EQ tl_zlest0061-nome_emb
    AND cl_codigo     EQ tl_zlest0061-cl_codigo
    AND nr_dco        EQ tl_zlest0061-nr_dco
    AND safra         EQ tl_zlest0061-safra
    AND id_frete_aqua EQ tl_zlest0061-id_frete_aqua.


  IF NOT ( tl_zlest0061[] IS INITIAL ).
    SELECT * FROM j_1bnfdoc
      INTO TABLE tl_j_1bnfdoc
      FOR ALL ENTRIES IN tl_zlest0061
    WHERE docnum EQ tl_zlest0061-docnum.

    SELECT * FROM j_1bnfe_active
      INTO TABLE tl_j_1bfne_active
      FOR ALL ENTRIES IN tl_j_1bnfdoc
    WHERE docnum EQ tl_j_1bnfdoc-docnum
      AND docsta EQ '1'
      AND cancel NE 'X'.
  ENDIF.

  SELECT * FROM makt
    INTO TABLE tl_makt
    FOR ALL ENTRIES IN tl_zlest0063
  WHERE matnr EQ tl_zlest0063-cod_material
    AND spras EQ sy-langu.

  LOOP AT tl_zlest0061 INTO wl_zlest0061.

    "Somar o Peso Vinculado / Valor Vinculado

    LOOP AT tl_zlest0060 INTO wl_zlest0060 WHERE cl_codigo     EQ wl_zlest0061-cl_codigo
                                             AND nr_dco        EQ wl_zlest0061-nr_dco
                                             AND safra         EQ wl_zlest0061-safra
                                             AND nome_emb      EQ wl_zlest0061-nome_emb
                                             AND id_frete_aqua EQ wl_zlest0061-id_frete_aqua.

      IF wl_zlest0061-rm_codigo IS NOT INITIAL.
        CHECK wl_zlest0060-rm_codigo EQ wl_zlest0061-rm_codigo.
      ENDIF.

      CHECK wl_zlest0060-operacao  EQ wl_zlest0061-operacao.

      IF wl_zlest0060-peso_liq_ret IS NOT INITIAL.

        gw_saida_vinc_nf-peso_vinculado    =  gw_saida_vinc_nf-peso_vinculado  + wl_zlest0060-peso_liq_ret.

        IF ( wl_zlest0060-netwr > 0 ) AND ( wl_zlest0060-peso_fiscal > 0 ).
          gw_saida_vinc_nf-valor_vinculado   =  gw_saida_vinc_nf-valor_vinculado +
          ( ( wl_zlest0060-netwr / wl_zlest0060-peso_fiscal ) * wl_zlest0060-peso_liq_ret ).
        ENDIF.

      ELSE.

        gw_saida_vinc_nf-peso_vinculado    =  gw_saida_vinc_nf-peso_vinculado  + wl_zlest0060-peso_fiscal.
        gw_saida_vinc_nf-valor_vinculado   =  gw_saida_vinc_nf-valor_vinculado + wl_zlest0060-netwr.
      ENDIF.

    ENDLOOP.

    MOVE: wl_zlest0060-rm_codigo            TO gw_saida_vinc_nf-rm_codigo,
          wl_zlest0060-dt_codigo            TO gw_saida_vinc_nf-dt_codigo,
          wl_zlest0060-id_frete_aqua        TO gw_saida_vinc_nf-id_frete_aqua.

    MOVE: wl_zlest0061-embarcacao           TO gw_saida_vinc_nf-embarcacao,
          wl_zlest0061-nome_emb             TO gw_saida_vinc_nf-nome,
          wl_zlest0061-dt_movimento         TO gw_saida_vinc_nf-dt_movimento,
          wl_zlest0061-cl_codigo            TO gw_saida_vinc_nf-cl_codigo,
          wl_zlest0061-safra                TO gw_saida_vinc_nf-safra,
          wl_zlest0061-nr_dco               TO gw_saida_vinc_nf-nr_dco.

    gw_saida_vinc_nf-dt_fatura = wl_zlest0061-dt_fatura.
    gw_saida_vinc_nf-vlr_usd   = wl_zlest0061-vlr_usd.
    gw_saida_vinc_nf-vlr_brl   = wl_zlest0061-vlr_brl.
    gw_saida_vinc_nf-tax_dolar = wl_zlest0061-tax_dolar.
    gw_saida_vinc_nf-docnum    = wl_zlest0061-docnum.

    READ TABLE tl_j_1bnfdoc INTO wl_j_1bnfdoc WITH KEY docnum = wl_zlest0061-docnum.
    IF ( wl_j_1bnfdoc-nfenum IS INITIAL ).
      gw_saida_vinc_nf-nfenum = wl_j_1bnfdoc-nfnum.
    ELSE.
      gw_saida_vinc_nf-nfenum = wl_j_1bnfdoc-nfenum.
    ENDIF.
    gw_saida_vinc_nf-serie  = wl_j_1bnfdoc-series.


    READ TABLE tl_j_1bfne_active INTO wl_j_1bnfe_active WITH KEY docnum = wl_j_1bnfdoc-docnum.
    gw_saida_vinc_nf-docsta = wl_j_1bnfe_active-docsta.



    READ TABLE tl_zlest0063 INTO wl_zlest0063 WITH KEY bukrs      = wl_zlest0061-bukrs
                                                       werks      = wl_zlest0061-werks
                                                       ano_viagem = wl_zlest0061-ano_viagem
                                                       nr_viagem  = wl_zlest0061-nr_viagem
                                                       nome_emb   = wl_zlest0061-nome_emb
                                                       tp_class   = wl_zlest0061-tp_class.

    CASE wl_zlest0061-tp_class.
      WHEN: 'CO'.
        gw_saida_vinc_nf-tp_class = 'CO-Convencional'.
      WHEN: 'R1'.
        gw_saida_vinc_nf-tp_class = 'R1-RR'.
      WHEN: 'R2'.
        gw_saida_vinc_nf-tp_class = 'R2-RR2'.
    ENDCASE.

    READ TABLE tl_makt INTO wl_makt WITH KEY matnr = wl_zlest0063-cod_material.
    gw_saida_vinc_nf-maktx = wl_makt-maktx.

    SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr = wl_zlest0061-cl_codigo.
    gw_saida_vinc_nf-name1 = wl_kna1-name1.
    gw_saida_vinc_nf-stcd1 = wl_kna1-stcd1.

    SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr = wl_zlest0060-dt_codigo.

    gw_saida_vinc_nf-name1_dest = wl_kna1-name1.
    gw_saida_vinc_nf-stcd1_dest = wl_kna1-stcd1.

    SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr = wl_zlest0060-rm_codigo.
    gw_saida_vinc_nf-name1_reme = wl_lfa1-name1.
    gw_saida_vinc_nf-stcd1_reme = wl_lfa1-stcd1.


    APPEND gw_saida_vinc_nf TO gt_saida_vinc_nf.

    CLEAR: gw_saida_vinc_nf, gw_saida_vinc_nf, wl_j_1bnfdoc,  wl_j_1bnfdoc, wl_j_1bnfe_active, wl_zlest0063, wl_makt, wl_kna1, wl_zlest0060.
  ENDLOOP.

ENDFORM.                    " SELECIONA_QUANTIDADE_COMBOIO
*&---------------------------------------------------------------------*
*&      Form  CRIAR_ALV_VINC_NF
*&---------------------------------------------------------------------*
FORM criar_alv_vinc_geral.

  DATA: tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        wl_layout   TYPE lvc_s_layo,
        wl_stable   TYPE lvc_s_stbl,
        wl_variant  TYPE disvariant.

  CLEAR: tl_function, wl_function, wl_layout.

  IF obj_custom_vinc_ge IS INITIAL.

    PERFORM: criar_catalog_vinc_ge.

    "WL_STABLE = 'X'.

    CREATE OBJECT obj_custom_vinc_ge
      EXPORTING
        container_name              = 'CONTAINER_VINC_GE'
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

    CREATE OBJECT obj_grid_vinc_ge
      EXPORTING
        i_parent          = obj_custom_vinc_ge
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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.
*    WL_FUNCTION = CL_GUI_ALV_GRID=>MC_MB_FILTER.
*    APPEND WL_FUNCTION TO TL_FUNCTION.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.

    wl_variant-report   = sy-repid.
    wl_variant-username = sy-uname.

    CALL METHOD obj_grid_vinc_ge->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = wl_variant
      CHANGING
        it_outtab                     = gt_saida_vinc_nf[]
        it_fieldcatalog               = gt_fcat_vinc_ge[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.

    CALL METHOD obj_grid_vinc_ge->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.                    " CRIAR_ALV_VINC_NF
*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_DOCUMENTOS
*&---------------------------------------------------------------------*
FORM estornar_documentos .

  DATA: tl_rows          TYPE lvc_t_row,
        sl_rows          TYPE lvc_s_row,
        opt              TYPE ctu_params,
        tl_msg           TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
        wl_msg           TYPE bdcmsgcoll,
        tabix            TYPE sy-tabix,
        estornado        TYPE c,
        data_ov          TYPE c LENGTH 10,
        tl_t100          TYPE TABLE OF t100,
        wl_t100          TYPE t100,
        txt_bstkd        TYPE vbkd-bstkd,
        wl_zlest0104_aux TYPE zlest0104,
        tg_zlest0060_aux TYPE TABLE OF zlest0060 WITH HEADER LINE.

  DATA: t_success    TYPE TABLE OF bapivbrksuccess,
        t_billing    TYPE TABLE OF bapivbrk,
        t_return     TYPE TABLE OF bapireturn1,
        vdocnum_est  TYPE j_1bdocnum,
        is_cancelled TYPE bapivbrkout-cancelled.

  REFRESH: tl_bdc.

  CALL METHOD obj_grid_ger_ov->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.

  READ TABLE tl_rows INDEX 1 INTO sl_rows.
  READ TABLE gt_saida_ger_ov INTO gw_saida_ger_ov INDEX sl_rows-index.

  IF ( sy-subrc EQ 0 ).

*>--- S4 MIGRATION 07/07/2023 - LM
    "//Cancela fatura
*    CALL FUNCTION 'BAPI_BILLINGDOC_IS_CANCELLED'
*      EXPORTING
*        billingdoc_number       = gw_saida_ger_ov-fatura
*      IMPORTING
*        billingdoc_is_cancelled = is_cancelled.

    DATA: vl_bill_doc              TYPE bapivbrksuccess-bill_doc,
          ls_billingdocumentdetail TYPE bapivbrkout.

    vl_bill_doc = CONV #( gw_saida_ger_ov-fatura ).

    CALL FUNCTION 'BAPI_BILLINGDOC_GETDETAIL'
      EXPORTING
        billingdocument       = vl_bill_doc
      IMPORTING
        billingdocumentdetail = ls_billingdocumentdetail.

    is_cancelled = ls_billingdocumentdetail-cancelled.
*<--- S4 MIGRATION 07/07/2023 - LM

    IF ( is_cancelled IS INITIAL ).
      "Cancela fatura
      CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1'
        EXPORTING
          billingdocument = gw_saida_ger_ov-fatura
        TABLES
          return          = t_return
          success         = t_success.
    ENDIF.

    IF ( t_success[] IS NOT INITIAL ) OR ( is_cancelled IS NOT INITIAL ).

      SELECT SINGLE docnum
        FROM j_1bnflin
        INTO @DATA(_docnum)
        WHERE refkey EQ @gw_saida_ger_ov-fatura.

      SELECT SINGLE candat
        FROM j_1bnfdoc
        INTO  @DATA(_vcandat)
       WHERE docnum EQ @_docnum.

      IF _vcandat IS INITIAL. "Documento Fiscal não está estornado ainda....
*
        "Verificar se documento esta autorizado na SEFAZ
        SELECT SINGLE docnum
         FROM j_1bnfe_active
         INTO @DATA(v_docnum)
         WHERE docnum     EQ @_docnum
           AND docsta     EQ '1'
           AND cancel     EQ ''.

        IF sy-subrc NE 0. "Caso não esteja, forçar o cancelamento do documento fiscal, serviço que a bapi deveria ter feito e não fez.
          CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
            EXPORTING
              doc_number               = _docnum
              ref_type                 = space
              ref_key                  = space
              can_dat                  = sy-datum
            IMPORTING
              doc_number               = vdocnum_est
            EXCEPTIONS
              document_not_found       = 1
              cancel_not_possible      = 2
              nf_cancel_type_not_found = 3
              database_problem         = 4
              docum_lock               = 5
              nfe_cancel_simulation    = 6
              OTHERS                   = 7.

          IF sy-subrc EQ 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.
          ELSE.
            DATA(w_erro) = abap_true.
          ENDIF.
        ELSE.
          w_erro = abap_true.
        ENDIF.

        IF w_erro IS NOT INITIAL. "Não houve êxito na tentativa do cancelamento do Doc. Fiscal, e prosseguir para gravar o log. de erro.
          APPEND VALUE #( msg = |Impossível estorno de fatura { gw_saida_ger_ov-fatura }. Danfe, não estornada| ) TO tg_msg_ret.
        ENDIF.

      ENDIF.
    ENDIF.

*    PERFORM F_PREENCHER_DYNPRO USING:
*       'X' 'SAPMV60A'        '0102',
*       ' ' 'BDC_CURSOR'      'KOMFK-VBELN(01)',
*       ' ' 'BDC_OKCODE'      '/00',
*       ' ' 'KOMFK-VBELN(01)' GW_SAIDA_GER_OV-FATURA,
*       'X' 'SAPMV60A'        '0103',
*       ' ' 'BDC_CURSOR'      '*TVFKT-VTEXT(01)',
*       ' ' 'BDC_OKCODE'      '=SICH'.
*
*    CALL TRANSACTION 'VF11' USING  TL_BDC MODE 'S' MESSAGES INTO TL_MSG.
    WAIT UP TO 2 SECONDS.

*    DELETE TL_MSG WHERE MSGTYP NE 'E'.
    DELETE t_return WHERE type NE 'E'.
*    IF NOT ( TL_MSG[] IS INITIAL ).
    IF NOT ( t_return[] IS INITIAL ) OR ( NOT w_erro IS INITIAL ).

      LOOP AT t_return INTO DATA(wa).

*      SELECT * FROM T100
*        INTO TABLE TL_T100
*        FOR ALL ENTRIES IN TL_MSG
*     WHERE SPRSL  EQ 'PT'
*       AND ARBGB  EQ TL_MSG-MSGID
*       AND MSGNR  EQ TL_MSG-MSGNR.

        SELECT * FROM t100
          INTO TABLE tl_t100
       WHERE sprsl  EQ 'PT'
         AND arbgb  EQ wa-id
         AND msgnr  EQ wa-number.

        CHECK NOT tl_t100[] IS INITIAL.

        LOOP AT tl_t100 INTO wl_t100.
          CONCATENATE gw_saida_ger_ov-fatura '-' wl_t100-text INTO wl_msg_ret-msg SEPARATED BY space.
          APPEND wl_msg_ret TO tg_msg_ret.
        ENDLOOP.

        IF NOT ( tg_msg_ret[] IS INITIAL ).

          CALL FUNCTION 'Z_DOC_CHECK_NEW'
            EXPORTING
              i_screen      = '100'
              i_show        = c_x
              i_repid       = sy-repid
              i_pressed_tab = 'TABSTRIP-ACTIVETAB'
              i_set_field   = 'X_FIELD'
            IMPORTING
              e_messagem    = wg_mensagem
            TABLES
              it_msgs       = tg_msg_ret.

        ELSE.
          estornado = 'X'.
        ENDIF.
      ENDLOOP.

    ELSE.
      estornado = 'X'.
    ENDIF.

    CASE estornado.
      WHEN: 'X'.
        REFRESH: tl_msg, tl_bdc.
        CLEAR: wl_msg,data_ov, estornado.

        CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) INTO data_ov.
        CONCATENATE 'VIAGEM:' w_viagem '–' 'ANO:' w_ano INTO txt_bstkd SEPARATED BY space.

        PERFORM f_preencher_dynpro USING:
                'X' 'SAPMV45A'    '0102',
                ''  'BDC_CURSOR'  'VBAK-VBELN',
                ''  'BDC_OKCODE'  '/00',
                ''  'VBAK-VBELN'  gw_saida_ger_ov-nr_ov,

                'X' 'SAPMV45A'  '4001',
                '' 'BDC_OKCODE'  '/00',
                '' 'BDC_SUBSCR'  'SAPMV45A',
                '' 'VBKD-BSTKD'	txt_bstkd,
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'KUWEV-KUNNR'  gw_saida_ger_ov-cl_codigo,
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_CURSOR'	'VBAK-FAKSK',
                '' 'RV45A-KETDAT'	data_ov,
                '' 'RV45A-KPRGBZ'	'D',
                '' 'VBAK-FAKSK'	'03',
                '' 'VBKD-PRSDT'	data_ov,
                '' 'VBKD-ZTERM'	'Z150',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_SUBSCR'	'SAPLV45W',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_SUBSCR'	'SAPMV45A',

                'X' 'SAPMV45A'  '4001',
                '' 'BDC_OKCODE'	'=SICH',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'VBKD-BSTKD'	txt_bstkd,
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'KUWEV-KUNNR'  gw_saida_ger_ov-cl_codigo,
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'RV45A-KETDAT'	data_ov,
                '' 'RV45A-KPRGBZ' 'D',
                '' 'VBAK-FAKSK'	'03',
                '' 'VBKD-PRSDT'	data_ov,
                '' 'VBKD-ZTERM'	'Z150',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_SUBSCR'	'SAPLV45W',
                '' 'BDC_SUBSCR'	'SAPMV45A',
                '' 'BDC_CURSOR'	'RV45A-MABNR(02)',
                '' 'BDC_SUBSCR' 'SAPMV45A'.

        CALL TRANSACTION 'VA02' USING  tl_bdc MODE 'S' MESSAGES INTO tl_msg.
        WAIT UP TO 2 SECONDS.


        DELETE FROM zlest0061 WHERE fatura EQ gw_saida_ger_ov-fatura
                                AND nr_ov  EQ gw_saida_ger_ov-nr_ov
                                AND docnum EQ gw_saida_ger_ov-docnum.
        IF ( sy-subrc EQ 0 ).
          "Adequações Retenção CS2016001199
          CLEAR: wl_zlest0104_aux, tg_zlest0060_aux[].
          SELECT *
            FROM zlest0060 INTO TABLE tg_zlest0060_aux
           WHERE docnum EQ gw_saida_ger_ov-docnum.

          LOOP AT tg_zlest0060_aux.
            SELECT SINGLE * FROM zlest0104 INTO wl_zlest0104_aux WHERE emissor = tg_zlest0060_aux-werks.
            IF sy-subrc = 0.

              IF tg_zlest0060_aux-chave_nfe IS NOT INITIAL.
                UPDATE zlest0205 SET cte_emit_aquav = space
                 WHERE chave_nfe = tg_zlest0060_aux-chave_nfe.
              ENDIF.

              UPDATE zsdt0001 SET cte_emit_aquav = space
               WHERE tp_movimento  = 'E'
                 AND bukrs         = wl_zlest0104_aux-bukrs
                 AND branch        = wl_zlest0104_aux-branch
                 AND nr_romaneio   = tg_zlest0060_aux-nr_romaneio
                 AND nr_safra      = tg_zlest0060_aux-safra
                 AND parid         = tg_zlest0060_aux-rm_codigo.
            ENDIF.
          ENDLOOP.
          "Fim

          UPDATE zlest0060 SET docnum       = space WHERE docnum EQ gw_saida_ger_ov-docnum.
          UPDATE zsdt0001  SET docnum_aquav = space
                          WHERE docnum_aquav EQ gw_saida_ger_ov-docnum.
        ENDIF.


        REFRESH: tl_estilo_ov.
        CLEAR: wl_estilo_ov.
        CLEAR:  gw_saida_ger_ov-auart,
                gw_saida_ger_ov-waerk,
                gw_saida_ger_ov-waerk_fatura,
                gw_saida_ger_ov-netpr,
                gw_saida_ger_ov-tax_dolar,
                gw_saida_ger_ov-vlr_usd,
                gw_saida_ger_ov-vlr_brl,
                gw_saida_ger_ov-nr_ov,
                gw_saida_ger_ov-fatura,
                gw_saida_ger_ov-nfnum,
                gw_saida_ger_ov-nfenum,
                gw_saida_ger_ov-docnum,
                gw_saida_ger_ov-estilo.

        MODIFY gt_saida_ger_ov INDEX sl_rows-index FROM gw_saida_ger_ov TRANSPORTING auart
                                                                                     waerk
                                                                                     waerk_fatura
                                                                                     netpr
                                                                                     tax_dolar
                                                                                     vlr_usd
                                                                                     vlr_brl
                                                                                     nr_ov
                                                                                     fatura
                                                                                     nfnum
                                                                                     nfenum
                                                                                     docnum.

        wl_estilo_ov-fieldname = 'AUART'.
        wl_estilo_ov-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND wl_estilo_ov TO tl_estilo_ov.

        wl_estilo_ov-fieldname = 'WAERK'.
        wl_estilo_ov-style = cl_gui_alv_grid=>mc_style_enabled.
        APPEND wl_estilo_ov TO tl_estilo_ov.


        INSERT LINES OF tl_estilo_ov INTO TABLE gw_saida_ger_ov-estilo.
        MODIFY gt_saida_ger_ov FROM gw_saida_ger_ov INDEX sl_rows-index.

        CALL METHOD obj_grid_ger_ov->refresh_table_display
          EXPORTING
            is_stable = wa_stable.



    ENDCASE.

  ENDIF.

ENDFORM.                    " ESTORNAR_DOCUMENTOS


*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.

ENDFORM.                    " F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TABELAS
*&---------------------------------------------------------------------*
FORM limpar_tabelas .
  REFRESH: gt_saida_viagem.
  CLEAR: gw_saida_viagem,
         obj_custom_viagem,
         obj_grid_viagem.
ENDFORM.                    " LIMPAR_TABELAS
*&---------------------------------------------------------------------*
*&      Form  Z_SMARTFORM
*&---------------------------------------------------------------------*
FORM z_smartform  USING    p_tipo TYPE any.

  DATA: ls_control        TYPE ssfctrlop,
        ls_options        TYPE ssfcompop,
        job_output_info   TYPE ssfcrescl,
        ls_xsfparam_line  TYPE ssfxsfp,
        v_bin_filesize    TYPE i,
        it_docs           TYPE STANDARD TABLE OF docs,
        it_lines          TYPE STANDARD TABLE OF tline,
        lv_fname          TYPE rs38l_fnam,
        lv_mail_recipient TYPE swotobjid,
        lv_mail_sender    TYPE swotobjid,
        lv_control        TYPE ssfctrlop,
        lv_name           TYPE so_name,
        lv_output         TYPE ssfcompop,
        wl_zmeng(20),
        wl_dmbtr(20),
        wl_vlrtot(20).

  DATA: i_otf       TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline     TYPE TABLE OF tline WITH HEADER LINE,
        i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
        i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist   LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
* Work Area declarations
        wa_objhead  TYPE soli_tab,
        w_ctrlop    TYPE ssfctrlop,
        w_compop    TYPE ssfcompop,
        w_return    TYPE ssfcrescl,
        wa_doc_chng TYPE sodocchgi1,
        w_data      TYPE sodocchgi1,
        wa_buffer   TYPE string, "To convert from 132 to 255
* Variables declarations
        v_form_name TYPE rs38l_fnam,
        v_len_in    LIKE sood-objlen,
        v_len_out   LIKE sood-objlen,
        v_len_outn  TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i.


  CASE p_tipo.

    WHEN: 'SFC'.
      vl_form = 'ZLES_SF_0002'.
    WHEN: 'SFB'.
      vl_form = 'ZLES_SF_0003'.

  ENDCASE.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_form
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*  Impresora
  ls_control-no_dialog = ' '. "Evita la pantalla de opciones de salida del formulario
  ls_options-tddest   = 'LOCL'.
  ls_options-tdimmed  = c_x.
  ls_options-tdnewid  = c_x.
  ls_options-tdnoarch = c_x.

  ls_control-preview = space.
  ls_control-device  = 'PRINTER'.
  ls_control-getotf  = ' '.

  CLEAR:job_output_info.
  CALL FUNCTION vl_name
    EXPORTING
      user_settings      = ' '
      control_parameters = ls_control
      output_options     = ls_options
      i_bukrs            = w_bukrs
      i_werks            = w_werks
      i_ano_viagem       = w_ano
      i_nr_viagem        = w_viagem
    IMPORTING
      job_output_info    = job_output_info
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_SMARTFORM
*&---------------------------------------------------------------------*
*&      Form  VALIDACAO_OV_GERADO
*&---------------------------------------------------------------------*
FORM validacao_romaneio.

  DATA: tl_estilo_romaneio TYPE lvc_t_styl WITH HEADER LINE,
        wl_estilo_romaneio TYPE lvc_s_styl.

  DATA: tl_zlest0060 TYPE TABLE OF zlest0060,
        wl_zlest0060 TYPE zlest0060.

  DATA: tl_zlest0060_aux TYPE TABLE OF zlest0060,
        wl_zlest0060_aux TYPE zlest0060.

  DATA: tl_zlest0061 TYPE TABLE OF zlest0061,
        wl_zlest0061 TYPE zlest0061.


  DATA: total_peso_fiscal   TYPE zlest0060-peso_fiscal,
        total_peso_subtotal TYPE zlest0060-peso_subtotal,
        total_netwr         TYPE zlest0060-netwr.

  DATA: ok         TYPE c,
        tabix      TYPE sy-tabix,
        vl_docnum  TYPE zlest0061-docnum,
        tg_idx_del TYPE TABLE OF sy-tabix WITH HEADER LINE.

  DATA: wl_color  TYPE kkblo_specialcol.

  REFRESH: tl_j_1bnfe_active[].


  "Validação para Travar a Linha caso já tenha sido vinculada.
  "Controlar o Saldo da Nota Fiscal
  IF NOT ( gt_saida_vinc_rom_nome[] IS INITIAL ).

    CLEAR: wl_zlest0073.

    SELECT * FROM zlest0073
      INTO TABLE tl_zlest0073
      FOR ALL ENTRIES IN gt_saida_vinc_rom_nome
    WHERE chave_nfe EQ gt_saida_vinc_rom_nome-chave_nfe.

    IF ( sy-subrc EQ 0 ).

      SELECT * FROM zlest0060
        INTO TABLE tl_zlest0060
        FOR ALL ENTRIES IN tl_zlest0073
     WHERE chave_nfe EQ tl_zlest0073-chave_nfe
       AND nr_viagem  EQ wp_viagem
       AND ano_viagem EQ wp_ano
       AND nome_emb   EQ wp_embarcacao.

      SELECT * FROM zlest0060
        INTO TABLE tl_zlest0060_aux
        FOR ALL ENTRIES IN tl_zlest0073
     WHERE chave_nfe EQ tl_zlest0073-chave_nfe.

      LOOP AT gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome.

        CLEAR: tabix.
        tabix = sy-tabix.

        READ TABLE tl_zlest0073 INTO wl_zlest0073 WITH KEY chave_nfe = gw_saida_vinc_rom_nome-chave_nfe.

        IF ( sy-subrc EQ 0 ).

          READ TABLE tl_zlest0060 INTO wl_zlest0060 WITH KEY chave_nfe  = wl_zlest0073-chave_nfe
                                                             nr_viagem  = wp_viagem
                                                             ano_viagem = wp_ano
                                                             nome_emb   = wp_embarcacao.
          IF ( sy-subrc EQ 0 ).

            gw_saida_vinc_rom_nome-check         = 'X'.
            gw_saida_vinc_rom_nome-id_frete_aqua    = wl_zlest0060-id_frete_aqua.
            gw_saida_vinc_rom_nome-id_frete_aqua_nf = wl_zlest0060-id_frete_aqua_nf.

            wg_tela1200-id_frete_aqua  = wl_zlest0060-id_frete_aqua.
            wp_frete_id                = wl_zlest0060-id_frete_aqua.

            IF wl_zlest0060-local_descarga IS NOT INITIAL AND wg_tela1200-id_local_descarga NE wl_zlest0060-local_descarga.
              SELECT SINGLE * INTO @DATA(wa_zlest0104)
                FROM zlest0104
               WHERE local_descarga EQ @wl_zlest0060-local_descarga.
              IF sy-subrc IS INITIAL.
                wg_tela1200-id_local_descarga = wa_zlest0104-local_descarga.
                wg_tela1200-ds_local_descarga = wa_zlest0104-descricao.
              ENDIF.
            ENDIF.

            gw_saida_vinc_rom_nome-peso_fiscal   = wl_zlest0073-peso_origem  - wl_zlest0073-peso_vinculado.
            gw_saida_vinc_rom_nome-netwr         = wl_zlest0073-valor_origem - wl_zlest0073-valor_vinculado.

            "Adequações Retenção CS2016001199
            gw_saida_vinc_rom_nome-peso_subtotal = wl_zlest0073-peso_subtotal - wl_zlest0073-peso_subtot_vinc.

            IF wl_zlest0060-peso_liq_ret IS NOT INITIAL.

              CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
                EXPORTING
                  i_bukrs        = gw_saida_vinc_rom_nome-bukrs_rom
                  i_branch       = gw_saida_vinc_rom_nome-branch_rom
                  i_lifnr        = gw_saida_vinc_rom_nome-parid_rom
                  i_peso         = gw_saida_vinc_rom_nome-peso_subtotal
                  i_peso_fiscal  = gw_saida_vinc_rom_nome-peso_fiscal
                IMPORTING
                  e_peso_liquido = gw_saida_vinc_rom_nome-peso_liq_ret.

              gw_saida_vinc_rom_nome-peso_vinc          = wl_zlest0060-peso_fiscal.
              gw_saida_vinc_rom_nome-valor_vinculado    = wl_zlest0060-netwr.
              gw_saida_vinc_rom_nome-peso_subtotal_vinc = wl_zlest0060-peso_subtotal.
              gw_saida_vinc_rom_nome-peso_liq_ret_vinc  = wl_zlest0060-peso_liq_ret.
              gw_saida_vinc_rom_nome-valor_liq_ret_vinc = gw_saida_vinc_rom_nome-netpr * gw_saida_vinc_rom_nome-peso_liq_ret_vinc.
              gw_saida_vinc_rom_nome-peso_retido        = wl_zlest0060-peso_retido.
              gw_saida_vinc_rom_nome-perc_ret           = wl_zlest0060-perc_ret.
              gw_saida_vinc_rom_nome-desc_ret_acum      = wl_zlest0060-desc_ret_acum.

              IF gw_saida_vinc_rom_nome-desc_ret_acum > 0.
                gw_saida_vinc_rom_nome-desc_ret_acum_grv  = 'X'.
              ENDIF.
            ELSE.
              gw_saida_vinc_rom_nome-peso_vinc       = wl_zlest0060-peso_fiscal.
              gw_saida_vinc_rom_nome-valor_vinculado = wl_zlest0060-netwr.
            ENDIF.

            gw_saida_vinc_rom_nome-peso_retido_utlz = wl_zlest0060-peso_retido_utlz.
            gw_saida_vinc_rom_nome-peso_retido_sld  = wl_zlest0060-peso_retido_sld.

            IF ( gw_saida_vinc_rom_nome-peso_liq_ret_vinc IS NOT INITIAL ).
              gw_saida_vinc_rom_nome-peso_util_vinc   = gw_saida_vinc_rom_nome-peso_liq_ret_vinc.
              gw_saida_vinc_rom_nome-valor_util_vinc  = gw_saida_vinc_rom_nome-valor_liq_ret_vinc.
            ELSE.
              gw_saida_vinc_rom_nome-peso_util_vinc   = gw_saida_vinc_rom_nome-peso_vinc.
              gw_saida_vinc_rom_nome-valor_util_vinc  = gw_saida_vinc_rom_nome-valor_vinculado.
            ENDIF.

            MODIFY gt_saida_vinc_rom_nome INDEX tabix FROM gw_saida_vinc_rom_nome TRANSPORTING check peso_fiscal netwr
                                                                                               peso_subtotal peso_liq_ret
                                                                                               peso_vinc valor_vinculado
                                                                                               peso_subtotal_vinc
                                                                                               peso_liq_ret_vinc
                                                                                               valor_liq_ret_vinc
                                                                                               peso_util_vinc
                                                                                               valor_util_vinc
                                                                                               desc_ret_acum
                                                                                               desc_ret_acum_grv
                                                                                               peso_retido_utlz
                                                                                               peso_retido_sld
                                                                                               peso_retido
                                                                                               perc_ret
                                                                                               id_frete_aqua
                                                                                               id_frete_aqua_nf.

*            WL_ESTILO_ROMANEIO-FIELDNAME = 'PESO_VINC'.
*            WL_ESTILO_ROMANEIO-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*            APPEND WL_ESTILO_ROMANEIO TO TL_ESTILO_ROMANEIO.
*
*            INSERT LINES OF TL_ESTILO_ROMANEIO INTO TABLE GW_SAIDA_VINC_ROM_NOME-ESTILO.
*
*            MODIFY GT_SAIDA_VINC_ROM_NOME FROM GW_SAIDA_VINC_ROM_NOME INDEX TABIX.


          ELSE.

            IF NOT ( tl_zlest0060_aux[] IS INITIAL  ).

              READ TABLE tl_zlest0060_aux INTO wl_zlest0060_aux WITH KEY chave_nfe = wl_zlest0073-chave_nfe.

              IF ( sy-subrc EQ 0 ).

                CLEAR: total_peso_fiscal,total_peso_subtotal,total_netwr.

                LOOP AT tl_zlest0060_aux INTO wl_zlest0060_aux WHERE chave_nfe EQ wl_zlest0073-chave_nfe.
                  total_peso_fiscal   = total_peso_fiscal   + wl_zlest0060_aux-peso_fiscal.
                  total_peso_subtotal = total_peso_subtotal + wl_zlest0060_aux-peso_subtotal. "Adequações Retenção CS2016001199
                  total_netwr  = total_netwr + wl_zlest0060_aux-netwr.
                  CLEAR: wl_zlest0060_aux.
                ENDLOOP.

                gw_saida_vinc_rom_nome-peso_fiscal   = wl_zlest0073-peso_origem   - total_peso_fiscal.
                gw_saida_vinc_rom_nome-peso_subtotal = wl_zlest0073-peso_subtotal - total_peso_subtotal. "Adequações Retenção CS2016001199
                gw_saida_vinc_rom_nome-netwr         = wl_zlest0073-valor_origem  - total_netwr.


              ELSE.

                gw_saida_vinc_rom_nome-peso_fiscal   = wl_zlest0073-peso_origem.
                gw_saida_vinc_rom_nome-peso_subtotal = wl_zlest0073-peso_subtotal. "Adequações Retenção CS2016001199
                gw_saida_vinc_rom_nome-netwr         = wl_zlest0073-valor_origem.

              ENDIF.

            ELSE.

              gw_saida_vinc_rom_nome-peso_fiscal   = wl_zlest0073-peso_origem.
              gw_saida_vinc_rom_nome-peso_subtotal = wl_zlest0073-peso_subtotal. "Adequações Retenção CS2016001199
              gw_saida_vinc_rom_nome-netwr         = wl_zlest0073-valor_origem.

            ENDIF.

            "Adequações Retenção CS2016001199
            "IF ( VG_CALC_RETENCAO IS NOT INITIAL ).  "17.01.2018 - CS2018000076
            CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
              EXPORTING
                i_bukrs         = gw_saida_vinc_rom_nome-bukrs_rom
                i_branch        = gw_saida_vinc_rom_nome-branch_rom
                i_lifnr         = gw_saida_vinc_rom_nome-parid_rom
                i_peso          = gw_saida_vinc_rom_nome-peso_subtotal
                i_peso_fiscal   = gw_saida_vinc_rom_nome-peso_fiscal
              IMPORTING
                e_perc_retencao = gw_saida_vinc_rom_nome-perc_ret
                e_peso_retido   = gw_saida_vinc_rom_nome-peso_retido
                e_peso_liquido  = gw_saida_vinc_rom_nome-peso_liq_ret.
            "ENDIF.

            MODIFY gt_saida_vinc_rom_nome INDEX tabix FROM gw_saida_vinc_rom_nome TRANSPORTING peso_fiscal netwr
                                                                                               peso_subtotal peso_liq_ret
                                                                                               peso_vinc valor_vinculado
                                                                                               peso_subtotal_vinc
                                                                                               peso_liq_ret_vinc
                                                                                               valor_liq_ret_vinc
                                                                                               peso_util_vinc
                                                                                               valor_util_vinc
                                                                                               peso_retido
                                                                                               perc_ret.
          ENDIF.


        ELSE.
          gw_saida_vinc_rom_nome-peso_fiscal   = gw_saida_vinc_rom_nome-peso_fiscal.
          gw_saida_vinc_rom_nome-peso_subtotal = gw_saida_vinc_rom_nome-peso_subtotal. "Adequações Retenção CS2016001199
          gw_saida_vinc_rom_nome-netwr         = gw_saida_vinc_rom_nome-netwr.

          "Adequações Retenção CS2016001199
          "IF ( VG_CALC_RETENCAO IS NOT INITIAL ). "17.01.2018 - CS2018000076
          CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
            EXPORTING
              i_bukrs         = gw_saida_vinc_rom_nome-bukrs_rom
              i_branch        = gw_saida_vinc_rom_nome-branch_rom
              i_lifnr         = gw_saida_vinc_rom_nome-parid_rom
              i_peso          = gw_saida_vinc_rom_nome-peso_subtotal
              i_peso_fiscal   = gw_saida_vinc_rom_nome-peso_fiscal
            IMPORTING
              e_perc_retencao = gw_saida_vinc_rom_nome-perc_ret
              e_peso_retido   = gw_saida_vinc_rom_nome-peso_retido
              e_peso_liquido  = gw_saida_vinc_rom_nome-peso_liq_ret.
          "ENDIF.

          MODIFY gt_saida_vinc_rom_nome INDEX tabix FROM gw_saida_vinc_rom_nome TRANSPORTING peso_fiscal netwr
                                                                                             peso_subtotal peso_liq_ret
                                                                                             peso_vinc valor_vinculado
                                                                                             peso_subtotal_vinc
                                                                                             peso_liq_ret_vinc
                                                                                             valor_liq_ret_vinc
                                                                                             peso_util_vinc
                                                                                             valor_util_vinc
                                                                                             peso_retido
                                                                                             perc_ret.

        ENDIF.

        CLEAR: gw_saida_vinc_rom_nome, wl_zlest0073, wl_estilo_romaneio, tabix.
        REFRESH: tl_estilo_romaneio.
      ENDLOOP.

    ENDIF.

  ENDIF.

  REFRESH: tl_zlest0061[], tl_zlest0060[].

  "Recupera todas as vinculações geradas OV.
  IF gt_saida_vinc_rom_nome[] IS NOT INITIAL.

    SELECT * FROM zlest0061
      INTO TABLE tl_zlest0061
      FOR ALL ENTRIES IN gt_saida_vinc_rom_nome
    WHERE nr_dco        EQ gt_saida_vinc_rom_nome-nr_dco
      AND safra         EQ gt_saida_vinc_rom_nome-safra
      AND embarcacao    EQ gt_saida_vinc_rom_nome-embarcacao
      AND nome_emb      EQ gt_saida_vinc_rom_nome-nome
      AND id_frete_aqua EQ gt_saida_vinc_rom_nome-id_frete_aqua
      AND nr_viagem   EQ wp_viagem
      AND ano_viagem  EQ wp_ano.

    IF tl_zlest0061[] IS NOT INITIAL.
      SELECT * FROM zlest0060
        INTO TABLE tl_zlest0060
        FOR ALL ENTRIES IN tl_zlest0061
     WHERE docnum        EQ tl_zlest0061-docnum
       AND nr_viagem     EQ tl_zlest0061-nr_viagem
       AND ano_viagem    EQ tl_zlest0061-ano_viagem
       AND nome_emb      EQ tl_zlest0061-nome_emb
       AND id_frete_aqua EQ tl_zlest0061-id_frete_aqua.
    ENDIF.

  ENDIF.


  LOOP AT gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome.

    CLEAR: wl_zlest0061, wl_zlest0060.

    tabix = sy-tabix.

    "Caso não tenha mais Saldo, descartar registro.
    IF ( gw_saida_vinc_rom_nome-peso_fiscal       EQ 0 ) AND
       ( gw_saida_vinc_rom_nome-peso_liq_ret_vinc EQ 0 ) AND
       ( gw_saida_vinc_rom_nome-peso_vinc         EQ 0 ).

      tg_idx_del = tabix.
      APPEND tg_idx_del.

      "Verifica Todos Ct-es Autorizados.
      PERFORM f_check_cte_autorizado USING gw_saida_vinc_rom_nome-chave_nfe.

      CONTINUE.

    ENDIF.

    PERFORM f_color_row_romaneio USING gw_saida_vinc_rom_nome.

    MODIFY gt_saida_vinc_rom_nome FROM gw_saida_vinc_rom_nome INDEX tabix.

    IF gw_saida_vinc_rom_nome-id_frete_aqua IS NOT INITIAL.
      READ TABLE tl_zlest0061 INTO wl_zlest0061 WITH KEY id_frete_aqua = gw_saida_vinc_rom_nome-id_frete_aqua.
    ELSE.
      READ TABLE tl_zlest0061 INTO wl_zlest0061 WITH KEY nr_dco        = gw_saida_vinc_rom_nome-nr_dco
                                                         safra         = gw_saida_vinc_rom_nome-safra
                                                         rm_codigo     = wg_tela1200-rm_codigo
                                                         embarcacao    = gw_saida_vinc_rom_nome-embarcacao
                                                         nome_emb      = gw_saida_vinc_rom_nome-nome
                                                         nr_viagem     = wp_viagem
                                                         ano_viagem    = wp_ano.
      IF sy-subrc NE 0.
        READ TABLE tl_zlest0061 INTO wl_zlest0061 WITH KEY nr_dco      = gw_saida_vinc_rom_nome-nr_dco
                                                           safra       = gw_saida_vinc_rom_nome-safra
                                                           cl_codigo   = wg_tela1200-rm_codigo
                                                           rm_codigo   = ''
                                                           embarcacao  = gw_saida_vinc_rom_nome-embarcacao
                                                           nome_emb    = gw_saida_vinc_rom_nome-nome
                                                           nr_viagem   = wp_viagem
                                                           ano_viagem  = wp_ano.
      ENDIF.

    ENDIF.

    READ TABLE tl_zlest0060 INTO wl_zlest0060 WITH KEY docnum        = wl_zlest0061-docnum
                                                       nr_viagem     = wl_zlest0061-nr_viagem
                                                       ano_viagem    = wl_zlest0061-ano_viagem
                                                       nome_emb      = wl_zlest0061-nome_emb
                                                       id_frete_aqua = wl_zlest0061-id_frete_aqua.


    IF ( sy-subrc EQ 0 ).

      CLEAR: wl_estilo_romaneio.
      REFRESH: tl_estilo_romaneio.

      IF wg_tela1200-id_frete_aqua EQ wl_zlest0061-id_frete_aqua.
        CLEAR: wg_tela1200-id_frete_aqua,
               wp_frete_id.
      ENDIF.

      wl_estilo_romaneio-fieldname = 'CHECK'.
      wl_estilo_romaneio-style = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wl_estilo_romaneio TO tl_estilo_romaneio.

      INSERT LINES OF tl_estilo_romaneio INTO TABLE gw_saida_vinc_rom_nome-estilo.
      MODIFY gt_saida_vinc_rom_nome FROM gw_saida_vinc_rom_nome INDEX tabix.

      IF gw_saida_vinc_rom_nome-check EQ abap_true.
        IF gw_saida_vinc_rom_nome-peso_liq_ret_vinc IS NOT INITIAL.
          total_vinculado = total_vinculado - gw_saida_vinc_rom_nome-peso_liq_ret_vinc.
        ELSE.
          total_vinculado = total_vinculado - gw_saida_vinc_rom_nome-peso_vinc.
        ENDIF.
      ENDIF.

      ok = 'X'.

*      "Verificar se Documento já foi autorizado
*      SELECT SINGLE docnum
*        FROM j_1bnfe_active INTO vl_docnum
*       WHERE docnum     = wl_zlest0061-docnum
*         AND cancel     = ''
*         AND docsta     = '1'.
*
*      IF sy-subrc = 0.
*        tg_idx_del = tabix.
*        APPEND tg_idx_del.
*      ENDIF.

    ELSE.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  "Deletar Registros descartados.
  SORT tg_idx_del DESCENDING.
  LOOP AT tg_idx_del.
    DELETE gt_saida_vinc_rom_nome INDEX tg_idx_del.
    ok = 'X'.
  ENDLOOP.

  IF ( ok EQ 'X' ).
    wa_stable-row = 'X'.
    wa_stable-col = 'X'.
    CALL METHOD obj_grid_romaneio->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  CLEAR: ok.



ENDFORM.                    " VALIDACAO_OV_GERADO
*&---------------------------------------------------------------------*
*&      Form  CONTROLE_REMET_DESTIN
*&---------------------------------------------------------------------*
* Perform Responsavel pelo o controle do Uso de Remetente/Destinatario.
* I = Incluir
* D = Deletar
* C = Consultar se já existe
FORM controle_remet_destin  USING    p_vg_remetente    TYPE lfa1-lifnr
                                     p_vg_destinatario TYPE kna1-kunnr
                                     opcao             TYPE c.

  DATA: wl_zlest0074 TYPE zlest0074.
  DATA: texto TYPE string.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_vg_remetente
    IMPORTING
      output = p_vg_remetente.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_vg_destinatario
    IMPORTING
      output = p_vg_destinatario.

  CASE opcao.

    WHEN: 'I'.


      wl_zlest0074-remetente    = p_vg_remetente.
      wl_zlest0074-destinatario = p_vg_destinatario.
      wl_zlest0074-usuario      = sy-uname.

      INSERT INTO zlest0074 VALUES wl_zlest0074.

      COMMIT WORK.

      "DELETE FROM ZLEST0074 WHERE REMETENTE EQ P_VG_REMETENTE
      "                  AND DESTINATARIO    EQ P_VG_DESTINATARIO
      "                  AND USUARIO         EQ SY-UNAME.

    WHEN: 'C'.

      SELECT SINGLE *
        FROM zlest0074
        INTO wl_zlest0074
      WHERE remetente    EQ p_vg_remetente
        AND destinatario EQ p_vg_destinatario.

      IF ( sy-subrc EQ 0 ) AND ( wl_zlest0074-usuario NE sy-uname ).

        CLEAR: texto.

        CONCATENATE 'Remet. '    wl_zlest0074-remetente
                    'e Destin. ' wl_zlest0074-destinatario INTO texto SEPARATED BY space.

        MESSAGE s000(zaquaviario)
        DISPLAY LIKE 'W'
        WITH texto 'está sendo utilizado pelo usuário ' wl_zlest0074-usuario.

        CLEAR: wl_zlest0074.
        EXIT.

      ELSE.
        opcao = 'I'.

        wl_zlest0074-remetente    = p_vg_remetente.
        wl_zlest0074-destinatario = p_vg_destinatario.
        wl_zlest0074-usuario      = sy-uname.

        INSERT INTO zlest0074 VALUES wl_zlest0074.

        COMMIT WORK.

      ENDIF.

    WHEN: 'D'.

      DELETE FROM zlest0074 WHERE usuario      EQ sy-uname.

      COMMIT WORK.

  ENDCASE.

ENDFORM.                    " CONTROLE_REMET_DESTIN
*&---------------------------------------------------------------------*
*&      Module  PBO_1500  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo_1500 OUTPUT.

  SET PF-STATUS 'PF1500'.
  SET TITLEBAR 'TB1500'.

  PERFORM: buscar_arquivos_xml.
ENDMODULE.                 " PBO_1500  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_1500  INPUT
*&---------------------------------------------------------------------*
MODULE pai_1500 INPUT.

  CASE sy-ucomm.
    WHEN: 'EXIT'. "Botão para Voltar a Tela anterior.
      CLEAR: gv_import. " OBJ_CUSTOM_ARQUIVO, OBJ_CUSTOM_IMPORT.
      REFRESH: gt_saida_arquivo[],
               gt_saida_import[],
               gt_table_arquivo[],
               gt_table_dir[],
               gt_error_import[].

      LEAVE TO SCREEN 0.
    WHEN: 'BTN_IMPORT'.
      PERFORM: importar_arquivo.
  ENDCASE.

ENDMODULE.                 " PAI_1500  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUSCAR_ARQUIVOS_XML
*&---------------------------------------------------------------------*
FORM buscar_arquivos_xml .

  DATA: vl_diretorio      TYPE rlgrap-filename,
        wl_zlest0076      TYPE zlest0076,
        tl_zlest0076      TYPE TABLE OF zlest0076,
        vl_tipo_arquivo   TYPE c,
        vl_status_arquivo TYPE c.


  CLEAR: gw_zparametros.

  SELECT SINGLE * FROM zparametros
    INTO gw_zparametros
   WHERE nome_parametro EQ 'INPUT_AQUAVIARIO'
    AND status EQ 'A'.

  IF ( sy-subrc NE 0 ) OR ( gw_zparametros-valor IS INITIAL ) .

    MESSAGE s000(zaquaviario)
    DISPLAY LIKE 'W'
    WITH  'Caminho não parametrizado. '.

  ELSE.

    IF ( gv_import  IS INITIAL ).

      REFRESH: gt_table_arquivo, gt_saida_arquivo.
      CLEAR: gw_table_arquivo, gw_saida_arquivo.

      CONCATENATE gw_zparametros-valor 'input' INTO vl_diretorio.


      CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
        EXPORTING
          directory  = vl_diretorio
          filter     = '*.XML'
        TABLES
          file_table = gt_table_arquivo
          dir_table  = gt_table_dir
        EXCEPTIONS
          cntl_error = 1
          OTHERS     = 2.


      LOOP AT gt_table_arquivo INTO gw_table_arquivo.

        CLEAR: vl_status_arquivo.
        SELECT SINGLE * FROM zlest0076 INTO wl_zlest0076 WHERE arquivo EQ gw_table_arquivo-pathname.

        IF ( sy-subrc NE 0 ).

          PERFORM: validar_arquivo_import USING gw_table_arquivo-pathname 0 CHANGING vl_tipo_arquivo vl_status_arquivo.

          CASE vl_status_arquivo.

            WHEN: 'E'.

              gw_saida_arquivo-tipo          = vl_tipo_arquivo.
              gw_saida_arquivo-status        = vl_status_arquivo.
              gw_saida_arquivo-status_icon   = icon_incomplete.
              gw_saida_arquivo-pathname      = gw_table_arquivo-pathname.

              APPEND gw_saida_arquivo TO gt_saida_arquivo.
              CLEAR: gw_saida_arquivo, gw_table_arquivo.

            WHEN OTHERS.
              gw_saida_arquivo-tipo          = vl_tipo_arquivo.
              gw_saida_arquivo-status        = vl_status_arquivo.
              gw_saida_arquivo-status_icon   = icon_checked.
              gw_saida_arquivo-pathname      = gw_table_arquivo-pathname.

              APPEND gw_saida_arquivo TO gt_saida_arquivo.
              CLEAR: gw_saida_arquivo, gw_table_arquivo.

          ENDCASE.


        ELSEIF ( sy-subrc EQ 0 ).

          PERFORM: validar_arquivo_import USING gw_table_arquivo-pathname 0 CHANGING vl_tipo_arquivo vl_status_arquivo.

          CASE vl_status_arquivo.

            WHEN: 'E'.

              gw_saida_arquivo-tipo          = vl_tipo_arquivo.
              gw_saida_arquivo-status        = vl_status_arquivo.
              gw_saida_arquivo-status_icon   = icon_incomplete.
              gw_saida_arquivo-pathname      = gw_table_arquivo-pathname.

              APPEND gw_saida_arquivo TO gt_saida_arquivo.
              CLEAR: gw_saida_arquivo, gw_table_arquivo.
            WHEN OTHERS.

              gw_saida_import-pathname      = gw_table_arquivo-pathname.

              APPEND gw_saida_import TO gt_saida_import.
              CLEAR: gw_saida_import, gw_table_arquivo.

          ENDCASE.


        ENDIF.

      ENDLOOP.

      IF NOT ( gt_table_arquivo[] IS INITIAL ) OR NOT ( gt_saida_arquivo IS INITIAL ).
        PERFORM: alv_arquivos_xml,
                 alv_arquivos_import,
                 alv_arquivos_erros.
      ELSE.

        REFRESH: tl_zlest0076[].
        CLEAR: wl_zlest0076.

        SELECT * FROM zlest0076
          INTO TABLE tl_zlest0076
        WHERE bukrs      EQ w_bukrs
          AND werks      EQ w_werks
          AND nr_viagem  EQ w_viagem
          AND ano_viagem EQ w_ano.

        CHECK NOT tl_zlest0076[] IS INITIAL.

        LOOP AT tl_zlest0076 INTO wl_zlest0076.
          gw_saida_import-pathname = wl_zlest0076-arquivo.
          APPEND gw_saida_import TO gt_saida_import.
        ENDLOOP.

        CHECK NOT gt_saida_import IS INITIAL.
        PERFORM: alv_arquivos_import.

      ENDIF.

    ELSE.

      CALL METHOD obj_grid_arquivo->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

      CALL METHOD obj_grid_import->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    ENDIF.


  ENDIF.
ENDFORM.                    " BUSCAR_ARQUIVOS_XML
*&---------------------------------------------------------------------*
*&      ARQUIVOS_XML
*&---------------------------------------------------------------------**&---------------------------------------------------------------------*
*&      ARQUIVOS_XML
*&---------------------------------------------------------------------**&---------------------------------------------------------------------*
*&      ARQUIVOS_XML
*&---------------------------------------------------------------------*
CLASS lcl_event_handler_arquivo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      zm_handle_hotspot_error FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER_ARQUIVO IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler_arquivo IMPLEMENTATION.
  METHOD: zm_handle_hotspot_error.
    PERFORM z_hotspot_error USING  e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot
ENDCLASS.                    "LCL_EVENT_HANDLER_ARQUIVO IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  ALV_ARQUIVOS_XML
*&---------------------------------------------------------------------*
FORM alv_arquivos_xml .


  DATA: wl_layout TYPE lvc_s_layo,
        wl_stable TYPE lvc_s_stbl.

  IF ( obj_custom_arquivo IS INITIAL ).
    PERFORM: criar_catalog_arquivo.

    CREATE OBJECT obj_custom_arquivo
      EXPORTING
        container_name              = 'CC_ARQUIVO_PEND'
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

    CREATE OBJECT obj_grid_arquivo
      EXPORTING
        i_parent          = obj_custom_arquivo
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

    "Retira o Toolbar do ALV
    wl_layout-no_toolbar = 'X'.
    wl_layout-sel_mode   = 'A'.

    SET HANDLER: lcl_event_handler_arquivo=>zm_handle_hotspot_error FOR obj_grid_arquivo.

    CALL METHOD obj_grid_arquivo->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
      CHANGING
        it_outtab                     = gt_saida_arquivo[]
        it_fieldcatalog               = gt_fcat_arquivo[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.



  ELSE.
    CALL METHOD obj_grid_arquivo->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDFORM.                    " ALV_ARQUIVOS_XML
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_ARQUIVO
*&---------------------------------------------------------------------*
FORM criar_catalog_arquivo .
  REFRESH: gt_fcat_arquivo.
  PERFORM montar_catalog_arquivo USING:
        'PATHNAME'     'Arquivo'      '35'    '' '' '' '' '' ''  '' '' '' '',
        'STATUS_ICON'  'Status'       '5'     '' 'X' '' 'C' '' ''  '' '' '' ''.
ENDFORM.                    " MONTAR_CATALOG_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  ALV_ARQUIVOS_XML
*&---------------------------------------------------------------------*
FORM alv_arquivos_import .

  DATA: wl_layout TYPE lvc_s_layo,
        wl_stable TYPE lvc_s_stbl.

  IF ( obj_custom_import IS INITIAL ).
    PERFORM: criar_catalog_import.

    CREATE OBJECT obj_custom_import
      EXPORTING
        container_name              = 'CC_ARQUIVO_IMPORT'
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

    CREATE OBJECT obj_grid_import
      EXPORTING
        i_parent          = obj_custom_import
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

    "Retira o Toolbar do ALV
    wl_layout-no_toolbar = 'X'.
    wl_layout-sel_mode   = 'A'.

    CALL METHOD obj_grid_import->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
      CHANGING
        it_outtab                     = gt_saida_import[]
        it_fieldcatalog               = gt_fcat_import[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.
    CALL METHOD obj_grid_import->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDFORM.                    " ALV_ARQUIVOS_XML
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_ARQUIVO
*&---------------------------------------------------------------------*
FORM criar_catalog_import .
  REFRESH: gt_fcat_import.
  PERFORM montar_catalog_import USING:
        'PATHNAME'     'Arquivo'      '5'    '' '' '' '' '' ''  '' '' '' ''.
ENDFORM.                    " MONTAR_CATALOG_ARQUIVO


*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_ARQUIVO
*&---------------------------------------------------------------------*
FORM montar_catalog_arquivo USING    VALUE(p_fieldname)
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
                                     VALUE(p_tabname)       LIKE dd02d-tabname
                                     VALUE(p_check).

  CLEAR: gw_fcat_arquivo.

  gw_fcat_arquivo-fieldname = p_fieldname.
  gw_fcat_arquivo-ref_table = p_ref_tabname..
  gw_fcat_arquivo-ref_field = p_ref_fieldname.
  gw_fcat_arquivo-tabname   = p_tabname.
  gw_fcat_arquivo-scrtext_l = p_desc.
  gw_fcat_arquivo-scrtext_m = p_desc.
  gw_fcat_arquivo-scrtext_s = p_desc.
  gw_fcat_arquivo-outputlen = p_tam.
  gw_fcat_arquivo-no_zero   = p_no_zero.
  gw_fcat_arquivo-hotspot   = p_hotspot.
  gw_fcat_arquivo-emphasize = p_cor.
  gw_fcat_arquivo-just      = p_just.
  gw_fcat_arquivo-do_sum    = p_sum.
  gw_fcat_arquivo-edit      = p_edit.
  gw_fcat_arquivo-checkbox  = p_check.

  APPEND gw_fcat_arquivo TO gt_fcat_arquivo.

ENDFORM.                    " MONTAR_CATALOG_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_ARQUIVO
*&---------------------------------------------------------------------*
FORM montar_catalog_import USING     VALUE(p_fieldname)
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
                                     VALUE(p_tabname)       LIKE dd02d-tabname
                                     VALUE(p_check).

  CLEAR: gw_fcat_import.

  gw_fcat_import-fieldname = p_fieldname.
  gw_fcat_import-ref_table = p_ref_tabname..
  gw_fcat_import-ref_field = p_ref_fieldname.
  gw_fcat_import-tabname   = p_tabname.
  gw_fcat_import-scrtext_l = p_desc.
  gw_fcat_import-scrtext_m = p_desc.
  gw_fcat_import-scrtext_s = p_desc.
  gw_fcat_import-outputlen = p_tam.
  gw_fcat_import-no_zero   = p_no_zero.
  gw_fcat_import-hotspot   = p_hotspot.
  gw_fcat_import-emphasize = p_cor.
  gw_fcat_import-just      = p_just.
  gw_fcat_import-do_sum    = p_sum.
  gw_fcat_import-edit      = p_edit.
  gw_fcat_import-checkbox  = p_check.

  APPEND gw_fcat_import TO gt_fcat_import.

ENDFORM.                    " MONTAR_CATALOG_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  IMPORTAR_ARQUIVO
*&---------------------------------------------------------------------*
FORM importar_arquivo .

  DATA: tl_rows TYPE lvc_t_row,
        sl_rows TYPE lvc_s_row.

  TYPES: BEGIN OF ty_xml_line,
           data(256) TYPE x,
         END OF ty_xml_line.

  DATA: vl_ixml          TYPE REF TO if_ixml,
        vl_streamfactory TYPE REF TO if_ixml_stream_factory,
        vl_parser        TYPE REF TO if_ixml_parser,
        vl_istream       TYPE REF TO if_ixml_istream,
        vl_document      TYPE REF TO if_ixml_document,
        vl_node          TYPE REF TO if_ixml_node,
        vl_xmldata       TYPE REF TO if_ixml_node,
        vl_elem          TYPE REF TO if_ixml_element,
        vl_root_node     TYPE REF TO if_ixml_node,
        vl_next_node     TYPE REF TO if_ixml_node,
        vl_name          TYPE string,
        vl_iterator      TYPE REF TO if_ixml_node_iterator,
        tl_xml_table     TYPE TABLE OF ty_xml_line,
        wl_xml_line      TYPE ty_xml_line,
        vl_xml_size      TYPE i,
        vl_arquivo       TYPE string,
        vl_arquivoteste  TYPE string,
        vl_string        TYPE string,
        vl_content       TYPE string,
        tl_tab           TYPE tsfixml,
        tl_itab          TYPE TABLE OF string,
        vl_tamanho       TYPE i,
        cl_converte      TYPE REF TO cl_abap_conv_in_ce,
        vl_tipo_arquivo  TYPE c,
        vl_vbeln         TYPE vbfa-vbeln,
        vl_series        TYPE zlest0060-series,
        vl_tabix         TYPE sy-tabix.


  "Parser Error
  DATA: vl_parsererror   TYPE REF TO if_ixml_parse_error,
        vl_str_error     TYPE string,
        vl_i_error       TYPE i,
        vl_i_error_c     TYPE c,
        vl_count_error   TYPE i,
        vl_count_error_c TYPE c,
        vl_index_error   TYPE i.

  "Dom-Tree
  DATA: vl_node_dom     TYPE REF TO if_ixml_node,
        vl_iterator_dom TYPE REF TO if_ixml_node_iterator,
        vl_nodemap_dom  TYPE REF TO if_ixml_named_node_map,
        vl_attr_dom     TYPE REF TO if_ixml_node,
        vl_name_dom     TYPE string,
        vl_prefix_dom   TYPE string,
        vl_value_dom    TYPE string,
        vl_indent_dom   TYPE i,
        vl_count_dom    TYPE i,
        vl_index_dom    TYPE i.


  DATA: porto_origem  TYPE lfa1-stcd1,
        porto_destino TYPE kna1-stcd1,
        nr_comboio    TYPE num4,
        empurrador    TYPE char30,
        calado        TYPE char4,
        descricao     TYPE char30,
        peso_real     TYPE brgew_ap,
        peso_total    TYPE j_1bnfdoc-nftot,
        data_carreg   TYPE char20,
        produto       TYPE c,
        tipo_produto  TYPE c LENGTH 2.

  "Nota Fiscal
  TYPES: BEGIN OF ty_nota,
           nf_numero         TYPE j_1bnfe_active-nfnum9,
           serie             TYPE j_1bnfe_active-serie,
           modelo            TYPE j_1bnfe_active-model,
           valor             TYPE j_1bnflin-netwr,
           peso              TYPE j_1bnfdoc-brgew,
           data_emissao      TYPE char10,
           uf_remetente      TYPE lfa1-spras,
           cnpj_remetente    TYPE lfa1-stcd1,
           insc_est_remet    TYPE lfa1-stcd2,
           uf_destinatario   TYPE lfa1-spras,
           cnpj_destinatario TYPE lfa1-stcd1,
           insc_est_destin   TYPE lfa1-stcd2,
           peso_total_nf     TYPE j_1bnfdoc-nftot,
           chave_nfe         TYPE char45,
           safra             TYPE char4,
           dco               TYPE char20,

         END OF ty_nota.

  DATA: tl_nota TYPE TABLE OF ty_nota,
        wl_nota TYPE ty_nota.

  "Tabelas para Inserir
  DATA: wl_input_zlest0063 TYPE zlest0063.

  "Tabela para Buscar Informações
  DATA: wl_zlest0053     TYPE zlest0053,
        wl_zlest0075     TYPE zlest0075,
        wl_zlest0054     TYPE zlest0054,
        wl_zlest0073     TYPE zlest0073,
        wl_zlest0060     TYPE zlest0060,
        wl_zlest0060_aux TYPE zlest0060,
        wl_j_1bnfdoc     TYPE j_1bnfdoc,
        wl_j_1bnflin     TYPE j_1bnflin,
        wl_vbfa          TYPE vbfa,
        wl_lfa1          TYPE lfa1,
        wl_kna1          TYPE kna1,
        wl_mara          TYPE mara.



  CLEAR: wl_zlest0053,
         wl_zlest0075,
         wl_zlest0054,
         wl_zlest0073,
         wl_zlest0060,
         wl_mara.

  FIELD-SYMBOLS: <fs_nota> TYPE ty_nota.

  REFRESH: tl_rows.
  CLEAR: sl_rows.

  CALL METHOD obj_grid_arquivo->get_selected_rows
    IMPORTING
      et_index_rows = tl_rows.



  LOOP AT tl_rows INTO sl_rows.


    "Verifica a Linha selecionada e recupera as informações como nome do arquivo.
    READ TABLE gt_saida_arquivo INTO gw_saida_arquivo INDEX sl_rows-index.

    IF ( sy-subrc EQ 0 ) AND ( gw_saida_arquivo-status NE 'E' ).

      vl_ixml          = cl_ixml=>create( ).
      vl_streamfactory = vl_ixml->create_stream_factory( ).


      "Concatena o nome do arquivo que foi selecionado na linha para montar o seu caminho completo.
      CONCATENATE gw_zparametros-valor 'input\' gw_saida_arquivo-pathname INTO vl_arquivo.

      "Recupera os dados do arquivo XML selecionado.
      CALL METHOD cl_gui_frontend_services=>gui_upload
        EXPORTING
          filename   = vl_arquivo
          filetype   = 'BIN'
        IMPORTING
          filelength = vl_xml_size
        CHANGING
          data_tab   = tl_xml_table
        EXCEPTIONS
          OTHERS     = 19.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


      CLEAR: vl_string.

      LOOP AT tl_xml_table INTO wl_xml_line.
        cl_converte = cl_abap_conv_in_ce=>create( input       = wl_xml_line-data
                                                  replacement = space ).
        cl_converte->read( IMPORTING data = vl_content
                                     len  = vl_tamanho ).
        CONCATENATE vl_string vl_content INTO vl_string.
      ENDLOOP.

      vl_string = vl_string+0(vl_xml_size).
      SPLIT vl_string AT cl_abap_char_utilities=>cr_lf INTO TABLE tl_itab.

      LOOP AT tl_itab INTO vl_string.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN vl_string WITH space.
      ENDLOOP.



      vl_istream = vl_streamfactory->create_istream_itable( table = tl_xml_table
                                                            size  = vl_xml_size ).

      vl_document = vl_ixml->create_document( ).
      vl_parser = vl_ixml->create_parser( stream_factory = vl_streamfactory
                                          istream        = vl_istream
                                          document       = vl_document ).


      "Valida a Estrutura (SCHEMA) do Arquivo XML.
      IF ( vl_parser->parse( ) NE 0 ).

        IF ( vl_parser->num_errors( ) NE 0 ).

          CLEAR: gw_error_import.
          gw_error_import-pathname = gw_saida_arquivo-pathname.

          vl_count_error = vl_parser->num_errors( ).
          vl_count_error_c = vl_count_error.
          CONCATENATE vl_count_error_c 'Erro na Estrutura do Arquivo: ' INTO gw_error_import-texto SEPARATED BY space.
          vl_index_error = 0.

          WHILE vl_index_error < vl_count_error.

            vl_parsererror = vl_parser->get_error( index = vl_index_error ).
            vl_i_error     = vl_parsererror->get_line( ).
            vl_i_error_c   = vl_i_error.
            CONCATENATE gw_error_import-texto 'Linha: ' vl_i_error_c INTO gw_error_import-texto SEPARATED BY space.

            vl_i_error     =   vl_parsererror->get_column( ).
            vl_i_error_c   = vl_i_error.
            CONCATENATE gw_error_import-texto 'Coluna: ' vl_i_error_c INTO gw_error_import-texto SEPARATED BY space.

            vl_str_error = vl_parsererror->get_reason( ).

            CONCATENATE gw_error_import-texto vl_str_error INTO gw_error_import-texto SEPARATED BY space.


            vl_index_error = vl_index_error + 1.

            APPEND gw_error_import TO gt_error_import.

          ENDWHILE.

          gv_import = 'X'.

        ENDIF.
      ENDIF.


      IF vl_parser->is_dom_generating( ) EQ 'X'.

        vl_node_dom ?= vl_document.

        IF NOT ( vl_node_dom IS INITIAL  ).

          vl_iterator_dom = vl_node_dom->create_iterator( ).
          vl_node_dom     = vl_iterator_dom->get_next( ).

          WHILE NOT vl_node_dom IS INITIAL.

            vl_indent_dom = vl_node_dom->get_height( ) * 2.
            vl_indent_dom = vl_indent_dom + 20.

            CASE vl_node_dom->get_type( ).
              WHEN if_ixml_node=>co_node_element.

                vl_name_dom    = vl_node_dom->get_name( ).
                vl_nodemap_dom = vl_node_dom->get_attributes( ).

                IF NOT vl_nodemap_dom IS INITIAL.

                  vl_count_dom = vl_nodemap_dom->get_length( ).

                  DO vl_count_dom TIMES.

                    vl_index_dom  = sy-index - 1.
                    vl_attr_dom   = vl_nodemap_dom->get_item( vl_index_dom ).
                    vl_name_dom   = vl_attr_dom->get_name( ).
                    vl_prefix_dom = vl_attr_dom->get_namespace_prefix( ).
                    vl_value_dom  = vl_attr_dom->get_value( ).

                    wl_nota-chave_nfe = vl_value_dom.

                    IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                      APPEND wl_nota TO tl_nota.
                    ENDIF.

                  ENDDO.

                  CLEAR: vl_value_dom.
                  CASE vl_name_dom.
                    WHEN: 'PORTO_ORIGEM'.
                      porto_origem = vl_node_dom->get_value( ).

                    WHEN: 'PORTO_DESTINO'.
                      porto_destino = vl_node_dom->get_value( ).

                    WHEN: 'NR_COMBOIO'.
                      nr_comboio = vl_node_dom->get_value( ).

                    WHEN: 'EMPURRADOR'.
                      empurrador = vl_node_dom->get_value( ).

                    WHEN: 'CALADO'.
                      calado = vl_node_dom->get_value( ).

                    WHEN: 'DESCRICAO'.
                      descricao = vl_node_dom->get_value( ).

                    WHEN: 'PESO_REAL'.
                      peso_real = vl_node_dom->get_value( ).

                    WHEN: 'DATA_CARREG'.
                      data_carreg = vl_node_dom->get_value( ).

                    WHEN: 'PRODUTO'.
                      produto = vl_node_dom->get_value( ).

                    WHEN: 'TIPO_PRODUTO'.
                      tipo_produto = vl_node_dom->get_value( ).

                      "Nota Fiscal.
                    WHEN: 'NF_NUMERO'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.

                          <fs_nota>-nf_numero = vl_node_dom->get_value( ).

                          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                            EXPORTING
                              input  = <fs_nota>-nf_numero
                            IMPORTING
                              output = <fs_nota>-nf_numero.

                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'SERIE'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.

                          "<FS_NOTA>-SERIE = VL_NODE_DOM->GET_VALUE( ).
                          <fs_nota>-serie = wl_nota-chave_nfe+22(3).
                          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                            EXPORTING
                              input  = <fs_nota>-serie
                            IMPORTING
                              output = <fs_nota>-serie.


                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'MODELO'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-modelo = vl_node_dom->get_value( ).
                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'VALOR'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-valor = vl_node_dom->get_value( )..
                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'PESO'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-peso = vl_node_dom->get_value( ).
                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'DATA_EMISSAO'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-data_emissao = vl_node_dom->get_value( ).
                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'UF_REMETENTE'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-uf_remetente = vl_node_dom->get_value( ).
                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'CNPJ_REMETENTE'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-cnpj_remetente = vl_node_dom->get_value( ).
                        ENDLOOP.

                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'INSC_EST_REMETENTE'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-insc_est_remet = vl_node_dom->get_value( ).
                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'UF_DESTINATARIO'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-uf_destinatario = vl_node_dom->get_value( ).


                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'CNPJ_DESTINATARIO'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          "<FS_NOTA>-CNPJ_DESTINATARIO = VL_NODE_DOM->GET_VALUE( ).
                          <fs_nota>-cnpj_destinatario = porto_destino.
                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'INSC_EST_DESTINATARIO'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-insc_est_destin = vl_node_dom->get_value( ).
                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'PESO_TOTAL'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-peso_total_nf = vl_node_dom->get_value( ).
                        ENDLOOP.
                        UNASSIGN <fs_nota>.

                      ELSE.

                        peso_total = vl_node_dom->get_value( ).

                      ENDIF.

                    WHEN: 'SAFRA'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-safra = vl_node_dom->get_value( ).
                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                    WHEN: 'DCO'.

                      IF NOT ( wl_nota-chave_nfe IS INITIAL ).
                        LOOP AT tl_nota ASSIGNING <fs_nota> WHERE chave_nfe EQ wl_nota-chave_nfe.
                          <fs_nota>-dco = vl_node_dom->get_value( ).
                        ENDLOOP.
                        UNASSIGN <fs_nota>.
                      ENDIF.

                  ENDCASE.

                ENDIF.

            ENDCASE.

            vl_node_dom = vl_iterator_dom->get_next( ).
          ENDWHILE.

          "Validação do número da Viagem.
          IF ( nr_comboio NE w_viagem ).
            gw_error_import-pathname = gw_saida_arquivo-pathname.
            CONCATENATE vl_count_error_c 'O número da viagem' nr_comboio 'é diferente da selecionada' w_viagem INTO gw_error_import-texto SEPARATED BY space.
            APPEND gw_error_import TO gt_error_import.
            gv_import = 'X'.
          ENDIF.


          "Validar o Empurrador.
          CLEAR: wl_zlest0053.
          SELECT SINGLE * FROM zlest0053 INTO wl_zlest0053 WHERE nome       EQ empurrador
                                                             AND bukrs      EQ w_bukrs
                                                             AND embarcacao EQ 'E'.
          IF ( sy-subrc NE 0 ).

            gw_error_import-pathname = gw_saida_arquivo-pathname.
            CONCATENATE vl_count_error_c 'Empurrador não cadastrado: ' empurrador INTO gw_error_import-texto SEPARATED BY space.
            APPEND gw_error_import TO gt_error_import.
            gv_import = 'X'.
            CLEAR: wl_zlest0053.
          ENDIF.


          "Validar a Barcaça
          CLEAR: wl_zlest0053.
          SELECT SINGLE * FROM zlest0053 INTO wl_zlest0053 WHERE nome       EQ descricao
                                                            AND bukrs      EQ w_bukrs
                                                            AND embarcacao EQ 'B'.
          IF ( sy-subrc NE 0 ).

            gw_error_import-pathname = gw_saida_arquivo-pathname.
            CONCATENATE vl_count_error_c 'Barcaça não cadastrada: ' descricao INTO gw_error_import-texto SEPARATED BY space.
            APPEND gw_error_import TO gt_error_import.
            gv_import = 'X'.

            CLEAR: wl_zlest0053.
          ENDIF.


          "Verifica se o Tipo de Produto esta cadastrado e pega o código do mesmo para demais informações.
          SELECT SINGLE * FROM zlest0075 INTO wl_zlest0075 WHERE produto EQ produto
                                                             AND tipo    EQ tipo_produto.
          IF ( sy-subrc EQ 0 ).

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wl_zlest0075-matnr
              IMPORTING
                output = wl_zlest0075-matnr.

            wl_input_zlest0063-cod_material = wl_zlest0075-matnr.

            "Pega O código do grupo de material
            SELECT SINGLE * FROM mara INTO wl_mara WHERE matnr EQ wl_zlest0075-matnr.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wl_mara-matkl
              IMPORTING
                output = wl_input_zlest0063-gr_material.

            wl_input_zlest0063-tp_class  = tipo_produto.
            wl_input_zlest0063-un_medida = 'KG'.

          ELSE.

            gw_error_import-pathname = gw_saida_arquivo-pathname.
            CONCATENATE vl_count_error_c 'Produto não parametrizado: ' produto INTO gw_error_import-texto SEPARATED BY space.
            APPEND gw_error_import TO gt_error_import.
            gv_import = 'X'.
          ENDIF.

          "Validação do Porto de Origem
          CLEAR: gw_saida_viagem.
          READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX 1.

          CLEAR: wl_lfa1.
          SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE stcd1 EQ porto_origem.
          IF ( sy-subrc NE 0 ).

            gw_error_import-pathname = gw_saida_arquivo-pathname.
            CONCATENATE vl_count_error_c 'Porto de Embarque não encontrado.' porto_origem INTO gw_error_import-texto SEPARATED BY space.
            APPEND gw_error_import TO gt_error_import.
            gv_import = 'X'.

          ELSE.

            IF ( gw_saida_viagem-po_embarque NE wl_lfa1-lifnr ).

              gw_error_import-pathname = gw_saida_arquivo-pathname.
              CONCATENATE vl_count_error_c 'Código do Porto Emb.' wl_lfa1-lifnr 'diferente do selecionado' gw_saida_viagem-po_embarque INTO gw_error_import-texto SEPARATED BY space.
              APPEND gw_error_import TO gt_error_import.
              gv_import = 'X'.

            ENDIF.
          ENDIF.

          "Validação do Porto de Destino
          CLEAR: wl_kna1.
          SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE stcd1 EQ porto_destino.
          IF ( sy-subrc NE 0 ).
            gw_error_import-pathname = gw_saida_arquivo-pathname.
            CONCATENATE vl_count_error_c 'Porto de Destino não encontrado.' porto_destino INTO gw_error_import-texto SEPARATED BY space.
            APPEND gw_error_import TO gt_error_import.
            gv_import = 'X'.
          ELSE.

            IF ( gw_saida_viagem-po_destino NE wl_kna1-kunnr  ).

              gw_error_import-pathname = gw_saida_arquivo-pathname.
              CONCATENATE vl_count_error_c 'Código do Porto Dest.' wl_kna1-kunnr  'diferente do selecionado.' gw_saida_viagem-po_destino INTO gw_error_import-texto SEPARATED BY space.
              APPEND gw_error_import TO gt_error_import.
              gv_import = 'X'.

            ENDIF.
          ENDIF.




          "Validação do Calado
          IF ( calado NE gw_saida_viagem-calado ).

            gw_error_import-pathname = gw_saida_arquivo-pathname.
            CONCATENATE vl_count_error_c 'O calado' calado 'é diferente do selecionado' gw_saida_viagem-calado INTO gw_error_import-texto SEPARATED BY space.
            APPEND gw_error_import TO gt_error_import.
            gv_import = 'X'.
          ENDIF.


          "Validação das Notas Vinculadas
          CLEAR: wl_nota.
          LOOP AT tl_nota INTO wl_nota.

            CLEAR: vl_tamanho.
            vl_tamanho = strlen( wl_nota-chave_nfe ).

            IF ( vl_tamanho < 44 ).

              gw_error_import-pathname = gw_saida_arquivo-pathname.
              CONCATENATE vl_count_error_c 'A chave' wl_nota-chave_nfe 'é menor que 44 caracteres.' INTO gw_error_import-texto SEPARATED BY space.
              APPEND gw_error_import TO gt_error_import.

              gv_import = 'X'.

            ELSEIF ( vl_tamanho > 44 ).

              gw_error_import-pathname = gw_saida_arquivo-pathname.
              CONCATENATE vl_count_error_c 'A chave' wl_nota-chave_nfe 'é maior que 44 caracteres.' INTO gw_error_import-texto SEPARATED BY space.
              APPEND gw_error_import TO gt_error_import.
              gv_import = 'X'.

            ELSE.

              "Validar o CNPJ comparando com a CHAVE da nota fiscal de rateio.
              IF ( wl_nota-chave_nfe+6(14) NE  wl_nota-cnpj_remetente ) .
                gw_error_import-pathname = gw_saida_arquivo-pathname.
                CONCATENATE vl_count_error_c 'O CNPJ da chave ' wl_nota-chave_nfe  'é diferente do remetente da nota' wl_nota-cnpj_remetente INTO gw_error_import-texto SEPARATED BY space.
                APPEND gw_error_import TO gt_error_import.
                gv_import = 'X'.
              ENDIF.
            ENDIF.

            "Validar Número da Nota referente a chave da nota.
            IF ( wl_nota-chave_nfe+25(9) NE wl_nota-nf_numero ).
              gw_error_import-pathname = gw_saida_arquivo-pathname.
              CONCATENATE vl_count_error_c 'Número da NF da chave ' wl_nota-chave_nfe 'diferente da nota'  wl_nota-nf_numero INTO gw_error_import-texto SEPARATED BY space.
              APPEND gw_error_import TO gt_error_import.

              gv_import = 'X'.
            ENDIF.

            "Validar Serie da Nota referente a chave da nota.
            IF ( wl_nota-chave_nfe+22(3) NE wl_nota-serie ).
              gw_error_import-pathname = gw_saida_arquivo-pathname.
              CONCATENATE vl_count_error_c 'Serie da NF da chave ' wl_nota-chave_nfe 'diferente da nota'  wl_nota-serie INTO gw_error_import-texto SEPARATED BY space.
              APPEND gw_error_import TO gt_error_import.
              gv_import = 'X'.
            ENDIF.

            SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE stcd1 EQ wl_nota-cnpj_remetente.
            IF ( sy-subrc EQ 0 ).
              IF ( wl_lfa1-kunnr IS INITIAL ).
                gw_error_import-pathname = gw_saida_arquivo-pathname.
                CONCATENATE vl_count_error_c 'Cliente não informado no cadastro de fornecedor' wl_nota-cnpj_remetente  INTO gw_error_import-texto SEPARATED BY space.
                APPEND gw_error_import TO gt_error_import.
                gv_import = 'X'.
              ENDIF.
            ELSE.
              gw_error_import-pathname = gw_saida_arquivo-pathname.
              CONCATENATE vl_count_error_c 'Fornecedor ' wl_nota-cnpj_remetente 'não cadastrado.' INTO gw_error_import-texto SEPARATED BY space.
              APPEND gw_error_import TO gt_error_import.
              gv_import = 'X'.
            ENDIF.
            CLEAR: wl_lfa1.
          ENDLOOP.

          IF ( gv_import EQ 'X' ).
            gw_saida_arquivo-status = 'E'.
            gw_saida_arquivo-status_icon = icon_incomplete.
            MODIFY gt_saida_arquivo FROM gw_saida_arquivo INDEX sl_rows-index TRANSPORTING status status_icon.
          ENDIF.

          "Validação da SAFRA
          IF ( wl_nota-safra IS INITIAL ).
            gw_error_import-pathname = gw_saida_arquivo-pathname.
            CONCATENATE vl_count_error_c 'A safra não foi informado.' w_viagem INTO gw_error_import-texto SEPARATED BY space.
            APPEND gw_error_import TO gt_error_import.
            gv_import = 'X'.
          ENDIF.


        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.


  "Alimentar as tabelas
  "Caso o arquivo não seja de cancelamento.
  IF ( gw_saida_arquivo-pathname+30(1) NE 'C' ) AND ( gv_import NE 'X' ).

    IF NOT ( tl_nota[] IS INITIAL ).

      "Log de Importação de Arquivo XML
      vl_tipo_arquivo = 'I'.
      PERFORM: log_arquivo_import USING gw_saida_arquivo-pathname sl_rows-index vl_tipo_arquivo.

      "Gravar na Tabela do Comboio ZLEST0063.
      wl_input_zlest0063-mandt      = sy-mandt. "Mandante do Sistema
      wl_input_zlest0063-bukrs      = w_bukrs. "Empresa da Viagem
      wl_input_zlest0063-werks      = w_werks. "Centro da Viagem
      wl_input_zlest0063-nr_viagem  = w_viagem. "Número da Viagem
      wl_input_zlest0063-ano_viagem = w_ano. "Ano da Viagem

      "Verifica se a Barcaça foi cadastrada.
      SELECT SINGLE * FROM zlest0053 INTO wl_zlest0053 WHERE nome EQ descricao.
      IF ( sy-subrc EQ 0 ).
        wl_input_zlest0063-embarcacao = wl_zlest0053-embarcacao. "Pega o Tipo da Embarcação.
        wl_input_zlest0063-nome_emb   = descricao. "Descrição da Barcaça no XML
        wl_input_zlest0063-tp_barcaca = wl_zlest0053-tp_barcaca. "Tipo de Barcaça

        SELECT SINGLE * FROM zlest0054 INTO wl_zlest0054 WHERE tp_barcaca EQ wl_zlest0053-tp_barcaca
                                                           AND calado     EQ calado.

        IF ( sy-subrc EQ 0 ).
          wl_input_zlest0063-peso_previsto = wl_zlest0054-capacidade.
        ENDIF.

      ENDIF.

      wl_input_zlest0063-status = 'X'.
      wl_input_zlest0063-usuario = sy-uname.
      wl_input_zlest0063-data_registro = sy-datum.
      wl_input_zlest0063-hora_registro = sy-uzeit.

      IF NOT ( wl_input_zlest0063 IS INITIAL ).
        INSERT INTO zlest0063 VALUES wl_input_zlest0063.
        COMMIT WORK.

        CLEAR: wl_input_zlest0063.

        wl_input_zlest0063-mandt      = sy-mandt. "Mandante do Sistema
        wl_input_zlest0063-bukrs      = w_bukrs. "Empresa da Viagem
        wl_input_zlest0063-werks      = w_werks. "Centro da Viagem
        wl_input_zlest0063-nr_viagem  = w_viagem. "Número da Viagem
        wl_input_zlest0063-ano_viagem = w_ano. "Ano da Viagem
        wl_input_zlest0063-embarcacao = 'E'.
        wl_input_zlest0063-nome_emb   = empurrador.

        wl_input_zlest0063-status = 'X'.
        wl_input_zlest0063-usuario = sy-uname.
        wl_input_zlest0063-data_registro = sy-datum.
        wl_input_zlest0063-hora_registro = sy-uzeit.

        INSERT INTO zlest0063 VALUES wl_input_zlest0063.
        COMMIT WORK.

      ENDIF.

      CLEAR: wl_nota.

      LOOP AT tl_nota INTO wl_nota.

        CLEAR: wl_zlest0073.

        READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX 1.
        SELECT SINGLE * FROM zlest0073 INTO wl_zlest0073 WHERE chave_nfe   EQ wl_nota-chave_nfe
                                                           AND po_embarque EQ gw_saida_viagem-po_embarque
                                                           AND po_destino  EQ gw_saida_viagem-po_destino.

        IF ( sy-subrc EQ 0 ).

          wl_nota-peso = wl_nota-peso * 1000.
          wl_zlest0073-peso_vinculado  = ( wl_zlest0073-peso_vinculado + wl_nota-peso ).
          wl_zlest0073-valor_vinculado = ( wl_zlest0073-valor_vinculado + ( ( wl_nota-valor / wl_nota-peso_total_nf ) * wl_nota-peso ) ).

          UPDATE zlest0073 SET peso_vinculado  = wl_zlest0073-peso_vinculado
                               valor_vinculado = wl_zlest0073-valor_vinculado
                           WHERE chave_nfe   EQ wl_nota-chave_nfe
                             AND po_embarque EQ gw_saida_viagem-po_embarque
                             AND po_destino  EQ gw_saida_viagem-po_destino.


        ELSE.


          wl_zlest0073-mandt           = sy-mandt.
          wl_zlest0073-chave_nfe       = wl_nota-chave_nfe.
          wl_zlest0073-po_embarque     = gw_saida_viagem-po_embarque.
          wl_zlest0073-po_destino      = gw_saida_viagem-po_destino.
          wl_zlest0073-valor_origem    = wl_nota-valor.
          wl_zlest0073-peso_origem     = wl_nota-peso_total_nf.
          wl_nota-peso = wl_nota-peso * 1000.
          wl_zlest0073-peso_vinculado  = wl_nota-peso.
          wl_zlest0073-valor_vinculado = ( ( wl_nota-valor / wl_nota-peso_total_nf ) * wl_zlest0073-peso_vinculado ).

          INSERT INTO zlest0073 VALUES wl_zlest0073.

          COMMIT WORK.

          CLEAR: gw_saida_viagem.

        ENDIF.

        wl_zlest0060-mandt      = sy-mandt.
        wl_zlest0060-bukrs      = w_bukrs.
        wl_zlest0060-werks      = w_werks.
        wl_zlest0060-nr_viagem  = w_viagem.
        wl_zlest0060-ano_viagem = w_ano.
        wl_zlest0060-embarcacao = wl_zlest0053-embarcacao.
        wl_zlest0060-nome_emb   = descricao.

        SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE stcd1 EQ wl_nota-cnpj_remetente.
        wl_zlest0060-rm_codigo = wl_lfa1-lifnr.

        SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ wl_lfa1-kunnr.

        wl_zlest0060-cl_codigo = wl_kna1-kunnr.

        CLEAR: wl_kna1.
        SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE stcd1 EQ wl_nota-cnpj_destinatario.
        wl_zlest0060-dt_codigo = wl_kna1-kunnr.

        wl_zlest0060-tomador_serv = 'R'.

        "CONCATENATE WL_NOTA-DATA_EMISSAO+6(4) WL_NOTA-DATA_EMISSAO+3(2) WL_NOTA-DATA_EMISSAO(2) INTO WL_ZLEST0060-DT_MOVIMENTO.
        wl_zlest0060-dt_movimento = sy-datum.
        CONCATENATE wl_nota-data_emissao+6(4) wl_nota-data_emissao+3(2) wl_nota-data_emissao(2) INTO wl_zlest0060-docdat.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_nota-nf_numero
          IMPORTING
            output = wl_zlest0060-nfnum.

        wl_zlest0060-series      = wl_nota-serie.
        wl_zlest0060-peso_fiscal = wl_nota-peso.
        wl_zlest0060-netwr       = ( ( wl_nota-valor / wl_nota-peso_total_nf ) * wl_zlest0060-peso_fiscal ).

        wl_zlest0060-chave_nfe = wl_nota-chave_nfe.
        wl_zlest0060-safra     = wl_nota-safra.
        wl_zlest0060-nr_dco    = wl_nota-dco.

        wl_zlest0060-usuario = sy-uname.
        wl_zlest0060-data_registro = sy-datum.
        wl_zlest0060-hora_registro = sy-uzeit.

        "alrs
        vl_series = wl_zlest0060-series+2(1).
        SELECT SINGLE * FROM j_1bnfdoc INTO wl_j_1bnfdoc
        WHERE doctyp      = '1'
        AND   direct      = '2'
        AND   nfe         = 'X'
        AND   parid       = wl_zlest0060-rm_codigo
        AND   nfenum      = wl_zlest0060-nfnum
        AND   series      = vl_series.

        IF sy-subrc = 0.
          SELECT SINGLE * FROM j_1bnflin INTO wl_j_1bnflin WHERE docnum    = wl_j_1bnfdoc-docnum.
          IF sy-subrc = 0.
            vl_vbeln = wl_j_1bnflin-refkey+0(10).
            SELECT SINGLE *
             FROM vbfa
             INTO wl_vbfa
             WHERE vbeln    = vl_vbeln
             AND   vbtyp_n  = 'M'
             AND   vbtyp_v  = 'J'.
            IF sy-subrc = 0.
              wl_zlest0060-doc_rem = wl_vbfa-vbelv.
            ENDIF.
          ENDIF.
        ENDIF.


        INSERT INTO zlest0060 VALUES wl_zlest0060.

        COMMIT WORK.

        CLEAR: wl_nota, wl_zlest0073, wl_zlest0060, wl_lfa1, wl_kna1, gw_saida_viagem.
      ENDLOOP.

      CLEAR: wl_input_zlest0063, wl_zlest0053, wl_zlest0054, wl_zlest0075, wl_mara,
             wl_zlest0073, wl_nota, wl_zlest0073, wl_zlest0060, wl_lfa1, wl_kna1.
    ENDIF.

  ELSE. "Caso seja um Arquivo de Cancelamento Desvincular todas as notas.

    vl_tipo_arquivo = 'C'.
    PERFORM: log_arquivo_import USING gw_saida_arquivo-pathname sl_rows-index vl_tipo_arquivo.

    "Desvincular Notas Fiscais
    LOOP AT tl_nota INTO wl_nota.

      CLEAR: wl_zlest0073, wl_zlest0060, gw_saida_viagem.

      READ TABLE gt_saida_viagem INTO gw_saida_viagem INDEX 1.
      SELECT SINGLE * FROM zlest0073 INTO wl_zlest0073 WHERE chave_nfe   EQ wl_nota-chave_nfe
                                                         AND po_embarque EQ gw_saida_viagem-po_embarque
                                                         AND po_destino  EQ gw_saida_viagem-po_destino.

      IF ( sy-subrc EQ 0 ).

        SELECT SINGLE * FROM zlest0053 INTO wl_zlest0053 WHERE nome EQ descricao.
        SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE stcd1 EQ wl_nota-cnpj_remetente.
        SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ wl_lfa1-kunnr.

        SELECT SINGLE * FROM zlest0060
          INTO wl_zlest0060
          WHERE bukrs      EQ w_bukrs
            AND werks      EQ w_werks
            AND nr_viagem  EQ w_viagem
            AND ano_viagem EQ w_ano
            AND embarcacao EQ wl_zlest0053-embarcacao
            AND nome_emb   EQ descricao
            AND rm_codigo  EQ wl_lfa1-lifnr
            AND cl_codigo  EQ wl_kna1-kunnr
           AND tomador_serv EQ 'R'.

        "Caso encontre a nota na ZLEST0060 (vinculação de notas fiscais).
        IF ( sy-subrc EQ 0 ).


          wl_zlest0073-peso_vinculado  = wl_zlest0073-peso_vinculado  - wl_zlest0060-peso_fiscal.
          wl_zlest0073-valor_vinculado = wl_zlest0073-valor_vinculado - wl_zlest0060-netwr.

          IF ( wl_zlest0073-peso_vinculado  >= 0 ) AND ( wl_zlest0073-valor_vinculado >= 0 ).

            UPDATE zlest0073 SET peso_vinculado  = wl_zlest0073-peso_vinculado
                                 valor_vinculado = wl_zlest0073-valor_vinculado
                             WHERE chave_nfe EQ wl_nota-chave_nfe.

            COMMIT WORK.

            DELETE FROM zlest0060
                  WHERE bukrs      EQ w_bukrs
                    AND werks      EQ w_werks
                    AND nr_viagem  EQ w_viagem
                    AND ano_viagem EQ w_ano
                    AND embarcacao EQ wl_zlest0053-embarcacao
                    AND nome_emb   EQ descricao
                    AND rm_codigo  EQ wl_lfa1-lifnr
                    AND cl_codigo  EQ wl_kna1-kunnr
                    AND tomador_serv EQ 'R'.

            COMMIT WORK.

          ENDIF.
        ENDIF.

        CLEAR: wl_zlest0073, wl_zlest0060, wl_lfa1, wl_kna1, wl_zlest0053.

      ENDIF.
    ENDLOOP.



  ENDIF.


  PERFORM: remover_arquivo USING vl_arquivo
                                 gw_zparametros-valor
                                 gw_saida_arquivo-pathname
                                 gv_import.


  PERFORM: selecionar_dados_comboio,
           criar_alv_comboio .


ENDFORM.                    " IMPORTAR_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  REMOVER_ARQUIVO
*&---------------------------------------------------------------------*
FORM remover_arquivo  USING p_caminho TYPE string
                            p_origem  TYPE zparametros-valor
                            p_arquivo TYPE sdokpath-pathname
                            p_import  TYPE c.


  DATA: vl_destino TYPE string.
  DATA: rc TYPE i.

  CASE p_import.
    WHEN: 'X'.

      CONCATENATE p_origem 'error\' p_arquivo INTO vl_destino.

      CALL METHOD cl_gui_frontend_services=>file_copy
        EXPORTING
          source               = p_caminho
          destination          = vl_destino
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          disk_full            = 4
          access_denied        = 5
          file_not_found       = 6
          destination_exists   = 7
          unknown_error        = 8
          path_not_found       = 9
          disk_write_protect   = 10
          drive_not_ready      = 11
          not_supported_by_gui = 12.

      CASE sy-subrc.
        WHEN: 1.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Controle Error.'.
        WHEN: 2.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro no Gui'.
        WHEN: 3.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Parâmetro Errado.'.
        WHEN: 4.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Disco insuficiente.'.
        WHEN: 5.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Acesso negado.'.
        WHEN: 6.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Arquivo não encontrado.'.
        WHEN: 7.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Destino não existente.'.
        WHEN: 8.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro não reconhecido.'.
        WHEN: 9.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Pasta nãoe encontrada.'.
        WHEN: 10.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Acesso negado para escrita no disco.'.
        WHEN: 11.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Acesso negado para leitura.'.
        WHEN: 12.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Não suportado pelo GUI'.
      ENDCASE.

      CALL METHOD cl_gui_frontend_services=>file_delete
        EXPORTING
          filename             = p_caminho
        CHANGING
          rc                   = rc
        EXCEPTIONS
          file_delete_failed   = 1
          cntl_error           = 2
          error_no_gui         = 3
          file_not_found       = 4
          access_denied        = 5
          unknown_error        = 6
          not_supported_by_gui = 7
          wrong_parameter      = 8.


      CASE sy-subrc.
        WHEN: 1.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro ao deletar o arquivo.'.
        WHEN: 2.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro de Controle.'.
        WHEN: 3.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro na GUI.'.
        WHEN: 4.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Arquivo não encontrado.'.
        WHEN: 5.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Acesso negado.'.
        WHEN: 6.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro não reconhecido.'.
        WHEN: 7.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Não suportado pelo GUI'.
        WHEN: 8.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Parâmetro errado..'.
      ENDCASE.


    WHEN OTHERS.


      CONCATENATE p_origem 'processed\' p_arquivo INTO vl_destino.

      CALL METHOD cl_gui_frontend_services=>file_copy
        EXPORTING
          source      = p_caminho
          destination = vl_destino.

      CASE sy-subrc.
        WHEN: 1.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Controle Error.'.
        WHEN: 2.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro no Gui'.
        WHEN: 3.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Parâmetro Errado.'.
        WHEN: 4.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Disco insuficiente.'.
        WHEN: 5.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Acesso negado.'.
        WHEN: 6.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Arquivo não encontrado.'.
        WHEN: 7.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Destino não existente.'.
        WHEN: 8.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro não reconhecido.'.
        WHEN: 9.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Pasta nãoe encontrada.'.
        WHEN: 10.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Acesso negado para escrita no disco.'.
        WHEN: 11.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Acesso negado para leitura.'.
        WHEN: 12.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Não suportado pelo GUI'.
      ENDCASE.


      CALL METHOD cl_gui_frontend_services=>file_delete
        EXPORTING
          filename             = p_caminho
        CHANGING
          rc                   = rc
        EXCEPTIONS
          file_delete_failed   = 1
          cntl_error           = 2
          error_no_gui         = 3
          file_not_found       = 4
          access_denied        = 5
          unknown_error        = 6
          not_supported_by_gui = 7
          wrong_parameter      = 8.


      CASE sy-subrc.
        WHEN: 1.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro ao deletar o arquivo.'.
        WHEN: 2.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro de Controle.'.
        WHEN: 3.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro na GUI.'.
        WHEN: 4.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Arquivo não encontrado.'.
        WHEN: 5.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Acesso negado.'.
        WHEN: 6.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Erro não reconhecido.'.
        WHEN: 7.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Não suportado pelo GUI'.
        WHEN: 8.
          MESSAGE s000(zaquaviario) DISPLAY LIKE 'W' WITH 'Parâmetro errado..'.
      ENDCASE.


  ENDCASE.
ENDFORM.                    " REMOVER_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  VALIDAR_ARQUIVO_IMPORT
*&---------------------------------------------------------------------*
FORM validar_arquivo_import  USING p_pathname TYPE sdokpath-pathname
                                   p_index    TYPE lvc_index
                          CHANGING p_tipo_arquivo p_status_arquivo.

  DATA: vl_tamanho TYPE i,
        texto      TYPE string,
        xml        TYPE abap_bool,
        wl_lfa1    TYPE lfa1,
        vl_werks   TYPE lfa1-lifnr.

  vl_tamanho = strlen( p_pathname ).

  CLEAR: vl_werks.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = w_werks
    IMPORTING
      output = vl_werks.


  IF ( vl_tamanho EQ 33 ).

    CASE p_pathname+29(4).
      WHEN: '.XML' OR '.xml'.
        CLEAR: vl_tamanho.

        IF ( p_pathname+14(1) EQ '_' ).
          vl_tamanho = vl_tamanho + 1.
        ENDIF.

        IF ( p_pathname+19(1) EQ '_' ).
          vl_tamanho = vl_tamanho + 1.
        ENDIF.

        IF ( p_pathname+24(1) EQ '_' ).
          vl_tamanho = vl_tamanho + 1.
        ENDIF.

        IF ( vl_tamanho EQ 3 ).

          IF ( p_pathname+15(4) NE w_viagem ).

            CLEAR: gw_error_import.

            p_tipo_arquivo   = 'I'. "C = Arquivo de Cancelamento
            p_status_arquivo = 'E'. "A = Arquivo Ativo.
            gw_error_import-pathname = p_pathname.
            CONCATENATE 'Este arquivo não é da viagem ' w_viagem INTO gw_error_import-texto SEPARATED BY space.
            APPEND gw_error_import TO gt_error_import.

          ELSE.

            CLEAR: wl_lfa1.

            SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE stcd1 EQ p_pathname(14)
                                                     AND sperr NE 'X'.
            IF ( wl_lfa1-lifnr EQ vl_werks ).


              IF ( p_pathname+20(4) EQ w_ano ).

                p_tipo_arquivo   = 'I'. "I = Arquivo Inicial da Importação
                p_status_arquivo = 'A'. "A = Arquivo Ativo.


              ELSE.
                CLEAR: gw_error_import.

                p_tipo_arquivo   = 'I'. "C = Arquivo de Cancelamento
                p_status_arquivo = 'E'. "A = Arquivo Ativo.
                gw_error_import-pathname = p_pathname.
                CONCATENATE 'Este arquivo não é do ano ' w_ano INTO gw_error_import-texto SEPARATED BY space.
                APPEND gw_error_import TO gt_error_import.
              ENDIF.



            ELSE.
              CLEAR: gw_error_import.

              p_tipo_arquivo   = 'I'. "C = Arquivo de Cancelamento
              p_status_arquivo = 'E'. "A = Arquivo Ativo.
              gw_error_import-pathname = p_pathname.
              CONCATENATE 'CNPJ do arquivo inválido' p_pathname(14) INTO gw_error_import-texto SEPARATED BY space.
              APPEND gw_error_import TO gt_error_import.

            ENDIF.

          ENDIF.

        ELSE.
          CLEAR: gw_error_import.
          p_tipo_arquivo   = 'I'. "I = Arquivo Inicial da Importação
          p_status_arquivo = 'A'. "A = Arquivo Ativo.
          gw_error_import-pathname = p_pathname.
          gw_error_import-texto    = 'Nome do Arquivo XML inválido'.
          APPEND gw_error_import TO gt_error_import.
        ENDIF.

      WHEN OTHERS.
        xml = ' '.
    ENDCASE.

  ELSEIF ( vl_tamanho EQ 35 ).

    CASE p_pathname+31(4).
      WHEN: '.XML' OR '.xml'.

        CLEAR: vl_tamanho.

        IF ( p_pathname+14(1) EQ '_' ).
          vl_tamanho = vl_tamanho + 1.
        ENDIF.

        IF ( p_pathname+19(1) EQ '_' ).
          vl_tamanho = vl_tamanho + 1.
        ENDIF.

        IF ( p_pathname+24(1) EQ '_' ).
          vl_tamanho = vl_tamanho + 1.
        ENDIF.

        IF ( p_pathname+29(1) EQ '_' ).
          vl_tamanho = vl_tamanho + 1.
        ENDIF.

        IF ( vl_tamanho EQ 4 ).


          IF ( p_pathname+15(4) NE w_viagem ).

            CLEAR: gw_error_import.

            p_tipo_arquivo   = 'C'. "C = Arquivo de Cancelamento
            p_status_arquivo = 'E'. "A = Arquivo Ativo.
            gw_error_import-pathname = p_pathname.
            CONCATENATE 'Este arquivo não é da viagem ' w_viagem INTO gw_error_import-texto SEPARATED BY space.
            APPEND gw_error_import TO gt_error_import.

          ELSE.

            CLEAR: wl_lfa1.

            SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE stcd1 EQ p_pathname(14)
                                                     AND sperr NE 'X'.

            IF ( wl_lfa1-lifnr EQ  vl_werks ).


              IF ( p_pathname+20(4) EQ w_ano ).
                p_tipo_arquivo   = 'C'. "C = Arquivo de Cancelamento
                p_status_arquivo = 'A'. "A = Arquivo Ativo.


              ELSE.
                CLEAR: gw_error_import.

                p_tipo_arquivo   = 'I'. "C = Arquivo de Cancelamento
                p_status_arquivo = 'E'. "A = Arquivo Ativo.
                gw_error_import-pathname = p_pathname.
                CONCATENATE 'Este arquivo não é do ano ' w_ano INTO gw_error_import-texto SEPARATED BY space.
                APPEND gw_error_import TO gt_error_import.
              ENDIF.



            ELSE.
              CLEAR: gw_error_import.

              p_tipo_arquivo   = 'I'. "C = Arquivo de Cancelamento
              p_status_arquivo = 'E'. "A = Arquivo Ativo.
              gw_error_import-pathname = p_pathname.
              CONCATENATE 'CNPJ do arquivo inválido' p_pathname(14) INTO gw_error_import-texto SEPARATED BY space.
              APPEND gw_error_import TO gt_error_import.

            ENDIF.


          ENDIF.

        ELSE.

          CLEAR: gw_error_import.

          p_tipo_arquivo   = 'C'. "C = Arquivo de Cancelamento
          p_status_arquivo = 'E'. "A = Arquivo Ativo.
          gw_error_import-pathname = p_pathname.
          gw_error_import-texto    = 'Nome do Arquivo XML inválido'.
          APPEND gw_error_import TO gt_error_import.
        ENDIF.

      WHEN OTHERS.
        xml = ' '.
    ENDCASE.


  ELSE.

    CLEAR: gw_error_import.
    p_status_arquivo = 'E'.
    gw_error_import-pathname = p_pathname.
    gw_error_import-texto    = 'Nome do Arquivo XML inválido'.
    APPEND gw_error_import TO gt_error_import.
    xml = ' '.
  ENDIF.

ENDFORM.                    " VALIDAR_ARQUIVO_IMPORT
*&---------------------------------------------------------------------*
*&      Form  LOG_ARQUIVO_IMPORT
*&---------------------------------------------------------------------*
FORM log_arquivo_import  USING    p_pathname      TYPE sdokpath-pathname
                                  p_index         TYPE lvc_index
                                  p_tipo_arquivo  TYPE c.

  DATA: wl_zlest0076 TYPE zlest0076,
        vl_arquivo   TYPE sdokpath-pathname.

  CLEAR: gv_import.

  "Escolhe qual é o tipo de arquivo para ser tratado.
  "I = Importação Inicial
  "C = Cancelamento do Arquivo Inicial.

  CASE p_tipo_arquivo.
    WHEN: 'I'.

      wl_zlest0076-arquivo    = p_pathname.
      wl_zlest0076-status     = 'A'.
      wl_zlest0076-bukrs      = w_bukrs.
      wl_zlest0076-werks      = w_werks.
      wl_zlest0076-nr_viagem  = w_viagem.
      wl_zlest0076-ano_viagem = w_ano.

      INSERT INTO zlest0076 VALUES wl_zlest0076.

      IF ( sy-subrc EQ 0 ).
        COMMIT WORK.
        gw_saida_import-pathname = p_pathname.
        APPEND gw_saida_import TO gt_saida_import.
      ENDIF.

    WHEN: 'C'.

      CLEAR: wl_zlest0076, vl_arquivo.

      CONCATENATE p_pathname(29) '.XML' INTO vl_arquivo.

      "Buscar se existe o Arquivo de Importação Inicial.
      SELECT SINGLE * FROM zlest0076 INTO wl_zlest0076 WHERE arquivo    EQ vl_arquivo
                                                         AND status     EQ 'A'
                                                         AND bukrs      EQ w_bukrs
                                                         AND werks      EQ w_werks
                                                         AND nr_viagem  EQ w_viagem
                                                         AND ano_viagem EQ w_ano.

      IF ( sy-subrc EQ 0 ). "Se encontrar o Arquivo Inicial de Importação deixar continuar o cancelamento ads notas vinculadas.


        UPDATE zlest0076 SET status = 'C'
                                       WHERE arquivo    EQ vl_arquivo
                                         AND status     EQ 'A'
                                         AND bukrs      EQ w_bukrs
                                         AND werks      EQ w_werks
                                         AND nr_viagem  EQ w_viagem
                                         AND ano_viagem EQ w_ano.

        COMMIT WORK.

        wl_zlest0076-arquivo    = p_pathname.
        wl_zlest0076-status     = 'C'.
        wl_zlest0076-bukrs      = w_bukrs.
        wl_zlest0076-werks      = w_werks.
        wl_zlest0076-nr_viagem  = w_viagem.
        wl_zlest0076-ano_viagem = w_ano.

        INSERT INTO zlest0076 VALUES wl_zlest0076.

        COMMIT WORK.

      ELSE. "Senão mostrar uma mensagem de erro para o usuário indicando que não existe arquivo para ser cancelado.

        CLEAR: gw_error_import.
        gw_error_import-pathname = p_pathname.
        gw_error_import-texto    = 'Não existe arquivo para cancelar.'.
        APPEND gw_error_import TO gt_error_import.

        gw_saida_arquivo-status = 'E'.
        gw_saida_arquivo-status_icon = icon_incomplete.
        MODIFY gt_saida_arquivo FROM gw_saida_arquivo INDEX p_index TRANSPORTING status status_icon.

        gv_import = 'X'.

      ENDIF.


  ENDCASE.

ENDFORM.                    " LOG_ARQUIVO_IMPORT
*&---------------------------------------------------------------------*
*&      Form  Z_HOTSPOT_ERROR
*&---------------------------------------------------------------------*
FORM z_hotspot_error  USING  p_e_row_id    TYPE  lvc_s_row
                             p_e_column_id TYPE  lvc_s_col
                             p_es_row_no   TYPE  lvc_s_roid.


  READ TABLE gt_saida_arquivo INTO gw_saida_arquivo INDEX  p_e_row_id.

  IF ( sy-subrc EQ 0 ).

    CALL METHOD obj_grid_error->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDFORM.                    " Z_HOTSPOT_ERROR
*&---------------------------------------------------------------------*
*&      Form  ALV_ARQUIVOS_ERROS
*&---------------------------------------------------------------------*
FORM alv_arquivos_erros.

  DATA: wl_layout TYPE lvc_s_layo,
        wl_stable TYPE lvc_s_stbl.

  IF ( obj_custom_error IS INITIAL ).
    PERFORM: criar_catalog_error.

    CREATE OBJECT obj_custom_error
      EXPORTING
        container_name              = 'CONTAINER_ERROR'
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

    CREATE OBJECT obj_grid_error
      EXPORTING
        i_parent          = obj_custom_error
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

    "Retira o Toolbar do ALV
    wl_layout-no_toolbar = 'X'.
    wl_layout-sel_mode   = 'A'.

    CALL METHOD obj_grid_error->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
      CHANGING
        it_outtab                     = gt_error_import[]
        it_fieldcatalog               = gt_fcat_error[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.
    CALL METHOD obj_grid_error->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.                    " ALV_ARQUIVOS_ERROS

*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_ERROR
*&---------------------------------------------------------------------*
FORM criar_catalog_error .
  REFRESH: gt_fcat_error.
  PERFORM montar_catalog_error USING:
        'PATHNAME'  'Arquivo'       '35'    '' '' '' '' '' ''  '' '' '' '',
        'TEXTO'     'Mensagem'      '70'    '' '' '' '' '' ''  '' '' '' ''.
ENDFORM.                    "CRIAR_CATALOG_ERROR

*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG_ERROR
*&---------------------------------------------------------------------*
FORM montar_catalog_error USING      VALUE(p_fieldname)
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
                                     VALUE(p_tabname)       LIKE dd02d-tabname
                                     VALUE(p_check).

  CLEAR: gw_fcat_error.

  gw_fcat_error-fieldname = p_fieldname.
  gw_fcat_error-ref_table = p_ref_tabname..
  gw_fcat_error-ref_field = p_ref_fieldname.
  gw_fcat_error-tabname   = p_tabname.
  gw_fcat_error-scrtext_l = p_desc.
  gw_fcat_error-scrtext_m = p_desc.
  gw_fcat_error-scrtext_s = p_desc.
  gw_fcat_error-outputlen = p_tam.
  gw_fcat_error-no_zero   = p_no_zero.
  gw_fcat_error-hotspot   = p_hotspot.
  gw_fcat_error-emphasize = p_cor.
  gw_fcat_error-just      = p_just.
  gw_fcat_error-do_sum    = p_sum.
  gw_fcat_error-edit      = p_edit.
  gw_fcat_error-checkbox  = p_check.

  APPEND gw_fcat_error TO gt_fcat_error.

ENDFORM.                    " MONTAR_CATALOG_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  PROCESSO_INSUMOS
*&---------------------------------------------------------------------*
FORM processo_insumos USING p_comboio TYPE zaqty_saida_comboio.

  TYPES: BEGIN OF ty_zfiwrt0008,
           parid      TYPE zfiwrt0008-parid,
           branch     TYPE zfiwrt0008-branch,
           operacao   TYPE zfiwrt0008-operacao,
           docnum     TYPE zfiwrt0008-docnum,
           nfenum     TYPE zfiwrt0008-nfenum,
           series     TYPE zfiwrt0008-series,
           nfenum_aux TYPE zlest0060-nfnum,
         END OF ty_zfiwrt0008.


  DATA: tl_zfiwrt0008     TYPE TABLE OF ty_zfiwrt0008,
        wl_zfiwrt0008     TYPE ty_zfiwrt0008,
        tl_j_1bnfdoc      TYPE TABLE OF j_1bnfdoc,
        wl_j_1bnfdoc      TYPE j_1bnfdoc,
        tl_j_1bnflin      TYPE TABLE OF j_1bnflin,
        wl_j_1bnflin      TYPE j_1bnflin,
        tl_j_1bnfe_active TYPE TABLE OF j_1bnfe_active,
        wl_j_1bnfe_active TYPE j_1bnfe_active,
        vl_branch         TYPE c LENGTH 10,
        vl_werks          TYPE werks_d.


  DATA: tl_zlest0060_nf_vinc     TYPE TABLE OF zlest0060,
        wl_zlest0060_nf_vinc     TYPE zlest0060,
        wl_zlest0060_nf_vinc_aux TYPE zlest0060.

  DATA: var_tp_operacao TYPE zfiwed001 VALUE '0454'.

  "DATA: OBJ_ZCL_UTIL_SD TYPE REF TO ZCL_UTIL_SD. "Classe de Utilitário SD

  DATA: tabix TYPE sy-tabix.

  REFRESH: tl_zfiwrt0008[], tl_j_1bnfdoc[], tl_j_1bnflin[], tl_j_1bnfe_active[], gt_saida_vinc_rom_nome[].
  CLEAR: wl_zfiwrt0008, wl_j_1bnfdoc, wl_j_1bnflin, wl_j_1bnfe_active.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wg_tela1200-rm_codigo
    IMPORTING
      output = wg_tela1200-rm_codigo.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wg_tela1200-dt_codigo
    IMPORTING
      output = vl_branch.

  vl_branch = vl_branch+6(4).

  SELECT parid branch operacao docnum nfenum series
    FROM zfiwrt0008
    INTO TABLE tl_zfiwrt0008
  WHERE parid    EQ wg_tela1200-rm_codigo
    AND branch   EQ vl_branch
    AND operacao EQ var_tp_operacao.

**================================== Inicio ajuste #IR177057 / AOENNING / Processo frete armazenagem do Insumos / 12-04-2024 =============================================
  IF sy-subrc NE 0.
    wg_tela1200-rm_codigo = |{ wg_tela1200-rm_codigo ALPHA = OUT }|.
    vl_werks              = |{ wg_tela1200-rm_codigo ALPHA = IN }|.
    vl_branch             = |{ vl_branch ALPHA = IN }|.
    var_tp_operacao       = '0961'.

    SELECT parid branch operacao docnum nfenum series
    FROM zfiwrt0008
    INTO TABLE tl_zfiwrt0008
  WHERE parid    EQ vl_branch      "Remetente
    AND branch   EQ vl_werks       "Destinatario
    AND operacao EQ var_tp_operacao.
  ENDIF.
**==================================Fim ajuste #IR177057 / AOENNING =============================================

  IF tl_zfiwrt0008[] IS NOT INITIAL.

    "Buscar Notas Vinculadas.
    REFRESH: tl_zlest0060_nf_vinc[].
    CLEAR: wl_zlest0060_nf_vinc.

    SELECT * FROM j_1bnfdoc
      INTO TABLE tl_j_1bnfdoc
      FOR ALL ENTRIES IN tl_zfiwrt0008
    WHERE docnum EQ tl_zfiwrt0008-docnum.

    CHECK NOT tl_j_1bnfdoc[] IS INITIAL.

    SELECT * FROM j_1bnflin
      INTO TABLE tl_j_1bnflin
      FOR ALL ENTRIES IN tl_j_1bnfdoc
    WHERE docnum EQ tl_j_1bnfdoc-docnum.


    "Agrupar Itens com mesmo Material, CFOP, Lote
    DATA(lit_lin_aux) = tl_j_1bnflin[].
    SORT tl_j_1bnflin BY docnum matnr cfop charg.
    DELETE ADJACENT DUPLICATES FROM tl_j_1bnflin COMPARING docnum matnr cfop charg.

    LOOP AT tl_j_1bnflin ASSIGNING FIELD-SYMBOL(<fs_lin_ajuste>).
      CLEAR: <fs_lin_ajuste>-menge, <fs_lin_ajuste>-netwr, <fs_lin_ajuste>-netpr.

      LOOP AT lit_lin_aux INTO DATA(lwa_lin_aux) WHERE docnum = <fs_lin_ajuste>-docnum
                                                   AND matnr  = <fs_lin_ajuste>-matnr
                                                   AND cfop   = <fs_lin_ajuste>-cfop
                                                   AND charg  = <fs_lin_ajuste>-charg.

        ADD lwa_lin_aux-menge TO <fs_lin_ajuste>-menge.
        ADD lwa_lin_aux-netwr TO <fs_lin_ajuste>-netwr.

        IF <fs_lin_ajuste>-menge <> 0.
          <fs_lin_ajuste>-netpr = <fs_lin_ajuste>-netwr / <fs_lin_ajuste>-menge.
        ENDIF.
      ENDLOOP.

    ENDLOOP.


    CHECK NOT tl_j_1bnflin[] IS INITIAL.

    SELECT * FROM j_1bnfe_active
      INTO TABLE tl_j_1bnfe_active
      FOR ALL ENTRIES IN tl_j_1bnflin
    WHERE docnum EQ tl_j_1bnflin-docnum
      AND docsta EQ '1'
      AND cancel NE 'X'.

    CHECK NOT tl_j_1bnfe_active[] IS INITIAL.

    LOOP AT tl_zfiwrt0008 INTO wl_zfiwrt0008.


      CLEAR: tabix, obj_zcl_util.

      tabix = sy-tabix.

      READ TABLE tl_j_1bnfdoc      INTO wl_j_1bnfdoc       WITH KEY docnum  = wl_zfiwrt0008-docnum.
      READ TABLE tl_j_1bnflin      INTO wl_j_1bnflin       WITH KEY docnum  = wl_j_1bnfdoc-docnum.
      READ TABLE tl_j_1bnfe_active INTO wl_j_1bnfe_active  WITH KEY docnum  = wl_j_1bnflin-docnum.

      IF ( sy-subrc EQ 0 ) AND NOT ( wl_j_1bnfe_active-nfnum9 IS INITIAL ).

        wl_zfiwrt0008-nfenum = wl_j_1bnfe_active-nfnum9.
        wl_zfiwrt0008-series = wl_j_1bnfe_active-serie.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_j_1bnfe_active-nfnum9
          IMPORTING
            output = wl_zfiwrt0008-nfenum_aux.



        MODIFY tl_zfiwrt0008 FROM wl_zfiwrt0008 INDEX tabix TRANSPORTING nfenum series nfenum_aux.

      ELSE.
        DELETE tl_zfiwrt0008 INDEX tabix.
      ENDIF.

      CLEAR:   wl_j_1bnfdoc,
               wl_j_1bnflin,
               wl_j_1bnfe_active,
               wl_zfiwrt0008.
    ENDLOOP.


    SELECT * FROM zlest0060
      INTO TABLE tl_zlest0060_nf_vinc
      FOR ALL ENTRIES IN tl_zfiwrt0008
    WHERE nfnum       EQ tl_zfiwrt0008-nfenum_aux
      AND series      EQ tl_zfiwrt0008-series
      AND bukrs       EQ wp_bukrs
      AND werks       EQ wp_werks
      AND nr_viagem   EQ w_viagem
      AND ano_viagem  EQ w_ano
      AND nome_emb    EQ p_comboio-nome.


    LOOP AT tl_zfiwrt0008 INTO wl_zfiwrt0008.

      CLEAR: wl_j_1bnfdoc, wl_j_1bnflin, wl_j_1bnfe_active, wl_zsdt0001, wl_zlest0073, wl_zlest0060_nf_vinc_aux, gw_saida_vinc_rom_nome.


      gw_saida_vinc_rom_nome-xml           = icon_led_green.
      gw_saida_vinc_rom_nome-vinc_fiscal   = 'X'.
      gw_saida_vinc_rom_nome-docnum_znfw   = wl_zfiwrt0008-docnum.
      gw_saida_vinc_rom_nome-embarcacao    = p_comboio-embarcacao+0(1).
      gw_saida_vinc_rom_nome-nome          = p_comboio-nome.
      gw_saida_vinc_rom_nome-rm_codigo     = wg_tela1200-rm_codigo.
      gw_saida_vinc_rom_nome-dt_codigo     = wg_tela1200-dt_codigo.

      READ TABLE tl_j_1bnfdoc INTO wl_j_1bnfdoc WITH KEY docnum = wl_zfiwrt0008-docnum.
      gw_saida_vinc_rom_nome-docdat        = wl_j_1bnfdoc-docdat.
      gw_saida_vinc_rom_nome-dt_movimento  = wl_j_1bnfdoc-pstdat.
      gw_saida_vinc_rom_nome-nfnum         = wl_j_1bnfdoc-nfenum.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wl_j_1bnfdoc-series
        IMPORTING
          output = gw_saida_vinc_rom_nome-series.


      READ TABLE tl_j_1bnflin INTO wl_j_1bnflin WITH KEY docnum = wl_j_1bnfdoc-docnum.
      gw_saida_vinc_rom_nome-safra         = wl_j_1bnflin-charg.

      READ TABLE tl_j_1bnfe_active INTO wl_j_1bnfe_active WITH KEY docnum = wl_j_1bnflin-docnum.

      IF ( sy-subrc EQ 0 ).

        CHECK wl_j_1bnflin-cfop IS NOT INITIAL.

        PERFORM f_set_operacao_nf USING wl_j_1bnflin-cfop(4)
                               CHANGING gw_saida_vinc_rom_nome-operacao.

        CHECK gw_saida_vinc_rom_nome-operacao IS NOT INITIAL.

        CHECK gw_saida_vinc_rom_nome-operacao EQ wg_tela1200-operacao.

        IF ( wl_j_1bnflin-netwr  > 0 ) AND
           ( wl_j_1bnflin-menge  > 0 ).
          gw_saida_vinc_rom_nome-netpr = wl_j_1bnflin-netwr / wl_j_1bnflin-menge.
        ENDIF.

        "Criar objeto da classe ZCL_UTIL para chamar o método MONTA_CHAVE_NFE.
        CREATE OBJECT obj_zcl_util.
        "Chama o método para montar a chave da Nf-e.
        obj_zcl_util->monta_chave_nfe( EXPORTING i_docnum = wl_j_1bnflin-docnum
                                       RECEIVING e_chave  = gw_saida_vinc_rom_nome-chave_nfe ).


        SELECT SINGLE * FROM zlest0073 INTO wl_zlest0073 WHERE chave_nfe EQ gw_saida_vinc_rom_nome-chave_nfe.

        IF ( sy-subrc NE 0 ).
          gw_saida_vinc_rom_nome-peso_fiscal   = wl_j_1bnflin-menge.
          gw_saida_vinc_rom_nome-netwr         = wl_j_1bnflin-netwr.
          gw_saida_vinc_rom_nome-matnr         = wl_j_1bnflin-matnr.
        ENDIF.
*        ELSE.
*          gw_saida_vinc_rom_nome-peso_fiscal     = wl_zlest0073-peso_origem.
*          gw_saida_vinc_rom_nome-netwr           = wl_zlest0073-valor_origem.
*          gw_saida_vinc_rom_nome-peso_vinc       = wl_zlest0073-peso_vinculado.
*          gw_saida_vinc_rom_nome-valor_vinculado = wl_zlest0073-valor_vinculado.
*        ENDIF.

        LOOP AT tl_zlest0060_nf_vinc INTO wl_zlest0060_nf_vinc WHERE nfnum       EQ wl_zfiwrt0008-nfenum_aux
                                                                 AND bukrs       EQ wp_bukrs
                                                                 AND werks       EQ wp_werks
                                                                 AND nr_viagem   EQ w_viagem
                                                                 AND ano_viagem  EQ w_ano
                                                                 AND nome_emb    EQ p_comboio-nome
                                                                 AND safra       EQ wl_j_1bnflin-charg.

          total_vinculado = total_vinculado + wl_zlest0060_nf_vinc-peso_fiscal.

        ENDLOOP.


        "Verifica se o romaneio esta selecionado para aquela barcaça, caso não esteja não mostrar para o usuário.
        SELECT SINGLE * FROM zlest0060 INTO  wl_zlest0060_nf_vinc_aux WHERE nfnum       EQ wl_zfiwrt0008-nfenum_aux
                                                                        AND bukrs       EQ wp_bukrs
                                                                        AND werks       EQ wp_werks
                                                                        AND nr_viagem   EQ w_viagem
                                                                        AND ano_viagem  EQ w_ano
                                                                        AND nome_emb    NE p_comboio-nome
                                                                        AND safra       EQ wl_j_1bnflin-charg.


        IF ( sy-subrc EQ 0 ).
          CLEAR: wl_j_1bnflin, wl_j_1bnfe_active, wl_zsdt0001, wl_zlest0073, wl_zlest0060_nf_vinc_aux, gw_saida_vinc_rom_nome.
          CONTINUE.

        ELSE.
          APPEND gw_saida_vinc_rom_nome TO gt_saida_vinc_rom_nome.
          CLEAR: gw_saida_vinc_rom_nome, wl_zdco_vinculo, wl_zlest0060_nf_vinc. "WL_ZLEST0065.

        ENDIF.
        CLEAR: wl_j_1bnflin, wl_j_1bnfe_active, wl_zsdt0001, wl_zlest0073, wl_zlest0060_nf_vinc_aux.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " PROCESSO_INSUMOS

*FORM F_CENTRO_CALC_RET USING P_CENTRO TYPE T001W-WERKS.
*
*
*  DATA: LW_SETLEAF   TYPE SETLEAF.
*
*  CLEAR: VG_CALC_RETENCAO.
*
*  SELECT SINGLE *
*    FROM ZLEST0152 INTO @DATA(_0152)
*   WHERE WERKS EQ @P_CENTRO
*     AND LOEKZ EQ ''.
*
*  IF SY-SUBRC = 0.
*    VG_CALC_RETENCAO = 'X'.
*  ENDIF.
*
*  VG_VINC_NF_PARC = 'X'.
*
*  SELECT SINGLE *
*    FROM SETLEAF INTO LW_SETLEAF
*   WHERE SETNAME EQ 'MAGGI_ZLES0077_NF_PARC'
*     AND VALFROM EQ P_CENTRO.
*
*  IF SY-SUBRC = 0.
*    CLEAR: VG_VINC_NF_PARC.
*  ENDIF.
*
*
*ENDFORM.

FORM f_atrib_peso  USING p_saida_vinc_rom_nome TYPE zaqty_saida_vinc_romaneio
                         p_peso_vinc           TYPE brgew
                         p_vinc_tot            TYPE c
                         p_tipo_peso           TYPE c.

*------------------------------------------------------------*
*   p_tipo_peso => 1 = Fiscal / 2 = Retido
*   p_vinc_tot  => Vinculo Total
*------------------------------------------------------------*

  DATA: vl_perc_vinc TYPE p DECIMALS 10.

  CLEAR: p_saida_vinc_rom_nome-peso_vinc,
         p_saida_vinc_rom_nome-valor_vinculado,

         p_saida_vinc_rom_nome-peso_liq_ret_vinc,
         p_saida_vinc_rom_nome-valor_liq_ret_vinc,

         p_saida_vinc_rom_nome-peso_subtotal_vinc,

         p_saida_vinc_rom_nome-perc_ret,
         p_saida_vinc_rom_nome-peso_retido.

  CHECK ( p_saida_vinc_rom_nome-netpr  > 0 ).
  CHECK ( p_peso_vinc > 0 ) OR ( p_vinc_tot IS NOT INITIAL ).

  IF p_tipo_peso = '1'. "Fiscal
    CHECK p_saida_vinc_rom_nome-peso_fiscal > 0 .
  ELSEIF p_tipo_peso = '2'. "Retido.
    CHECK p_saida_vinc_rom_nome-peso_liq_ret > 0.
  ELSE.
    EXIT.
  ENDIF.

  IF p_vinc_tot IS NOT INITIAL.

    "Atribuição Fiscal( Peso/ Valor)
    p_saida_vinc_rom_nome-peso_vinc          = p_saida_vinc_rom_nome-peso_fiscal.
    p_saida_vinc_rom_nome-valor_vinculado    = p_saida_vinc_rom_nome-netwr.

    "Atribuição Peso Subtotal
    p_saida_vinc_rom_nome-peso_subtotal_vinc = p_saida_vinc_rom_nome-peso_subtotal.

  ELSE.

    "Percentual Vinculação
    IF p_tipo_peso = '1'. "Fiscal

      CHECK p_peso_vinc <= p_saida_vinc_rom_nome-peso_fiscal.

      vl_perc_vinc = p_peso_vinc / p_saida_vinc_rom_nome-peso_fiscal.
    ELSEIF p_tipo_peso = '2'. "Retido.

      CHECK p_peso_vinc <= p_saida_vinc_rom_nome-peso_liq_ret.

      vl_perc_vinc = p_peso_vinc / p_saida_vinc_rom_nome-peso_liq_ret.
    ENDIF.

    "Atribuição Fiscal(Peso/Valor)
    p_saida_vinc_rom_nome-peso_vinc       = p_saida_vinc_rom_nome-peso_fiscal * vl_perc_vinc.
    p_saida_vinc_rom_nome-valor_vinculado = p_saida_vinc_rom_nome-peso_vinc   * p_saida_vinc_rom_nome-netpr.

    "Atribuição Peso Subtotal
    p_saida_vinc_rom_nome-peso_subtotal_vinc = p_saida_vinc_rom_nome-peso_subtotal * vl_perc_vinc.

  ENDIF.

  "Atribuição Retenção ( Peso/ Valor)
  "IF ( VG_CALC_RETENCAO IS NOT INITIAL ). "17.01.2018 - CS2018000076

  CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
    EXPORTING
      i_bukrs         = p_saida_vinc_rom_nome-bukrs_rom
      i_branch        = p_saida_vinc_rom_nome-branch_rom
      i_lifnr         = p_saida_vinc_rom_nome-parid_rom
      i_peso          = p_saida_vinc_rom_nome-peso_subtotal_vinc
      i_peso_fiscal   = p_saida_vinc_rom_nome-peso_vinc
      i_peso_ret_desc = p_saida_vinc_rom_nome-desc_ret_acum
    IMPORTING
      e_perc_retencao = p_saida_vinc_rom_nome-perc_ret
      e_peso_retido   = p_saida_vinc_rom_nome-peso_retido
      e_peso_liquido  = p_saida_vinc_rom_nome-peso_liq_ret_vinc.


  p_saida_vinc_rom_nome-valor_liq_ret_vinc = p_saida_vinc_rom_nome-peso_liq_ret_vinc *
                                             p_saida_vinc_rom_nome-netpr.
  "ENDIF.

  IF ( p_saida_vinc_rom_nome-peso_liq_ret_vinc IS NOT INITIAL ). "17.01.2018 - CS2018000076
    p_saida_vinc_rom_nome-peso_util_vinc   = p_saida_vinc_rom_nome-peso_liq_ret_vinc.
    p_saida_vinc_rom_nome-valor_util_vinc  = p_saida_vinc_rom_nome-valor_liq_ret_vinc.
  ELSE.
    p_saida_vinc_rom_nome-peso_util_vinc   = p_saida_vinc_rom_nome-peso_vinc.
    p_saida_vinc_rom_nome-valor_util_vinc  = p_saida_vinc_rom_nome-valor_vinculado.
  ENDIF.

ENDFORM.

FORM f_atualiza_saldo_nf  USING p_vinc_saida_rom TYPE zaqty_saida_vinc_romaneio.

  DATA: tg_0060      TYPE TABLE OF zlest0060 WITH HEADER LINE,
        wl_zlest0073 TYPE zlest0073,
        wl_zlest0056 TYPE zlest0056,
        wl_zlest0104 TYPE zlest0104,
        wl_zsdt0001  TYPE zsdt0001,
        vl_chave     TYPE zlest0073,
        wl_j_1bnflin TYPE j_1bnflin.

  DATA: lit_lin_znfw TYPE TABLE OF j_1bnflin.


  vl_chave = p_vinc_saida_rom-chave_nfe.

  DELETE FROM zlest0073 WHERE chave_nfe EQ vl_chave.

  SELECT *
    FROM zlest0060 INTO TABLE tg_0060
   WHERE chave_nfe EQ vl_chave.

  CHECK tg_0060[] IS NOT INITIAL.

  CLEAR: wl_zlest0073, wl_zsdt0001, wl_zlest0056.

  wl_zlest0073-chave_nfe = vl_chave.

  LOOP AT tg_0060.

    IF ( wl_zlest0073-peso_origem  IS INITIAL ) OR
       ( wl_zlest0073-valor_origem IS INITIAL ).

      CLEAR: wl_zlest0104.
      SELECT SINGLE * FROM zlest0104 INTO wl_zlest0104 WHERE emissor = tg_0060-werks.

      SELECT SINGLE *
        FROM zsdt0001 INTO wl_zsdt0001
       WHERE tp_movimento  = 'E'
         AND bukrs         = wl_zlest0104-bukrs
         AND branch        = wl_zlest0104-branch
         AND nr_romaneio   = tg_0060-nr_romaneio
         AND nr_safra      = tg_0060-safra
         AND parid         = tg_0060-rm_codigo.

      IF sy-subrc = 0.
        wl_zlest0073-valor_origem  = wl_zsdt0001-netwr.
        wl_zlest0073-peso_origem   = wl_zsdt0001-peso_fiscal.
        wl_zlest0073-peso_subtotal = wl_zsdt0001-peso_subtotal.

      ELSEIF p_vinc_saida_rom-check_complemento IS NOT INITIAL AND p_vinc_saida_rom-chave_nfe IS NOT INITIAL.

        SELECT SINGLE * INTO @DATA(wa_complemento)
          FROM zlest0205
         WHERE chave_nfe EQ @p_vinc_saida_rom-chave_nfe.

        IF sy-subrc = 0.
          wl_zlest0073-valor_origem  = wa_complemento-netwr.
          wl_zlest0073-peso_origem   = wa_complemento-peso_fiscal.
          wl_zlest0073-peso_subtotal = wa_complemento-peso_subtotal.
        ENDIF.

      ELSEIF p_vinc_saida_rom-docnum_znfw IS NOT INITIAL.

        CLEAR: lit_lin_znfw[].

        SELECT *
          FROM j_1bnflin INTO TABLE lit_lin_znfw
         WHERE docnum = p_vinc_saida_rom-docnum_znfw.

        READ TABLE lit_lin_znfw INTO DATA(lwa_lin_znfw_filter) INDEX 1.

        LOOP AT lit_lin_znfw INTO DATA(lwa_lin_znfw) WHERE docnum = lwa_lin_znfw_filter-docnum
                                                       AND matnr  = lwa_lin_znfw_filter-matnr
                                                       AND cfop   = lwa_lin_znfw_filter-cfop
                                                       AND charg  = lwa_lin_znfw_filter-charg.
          ADD lwa_lin_znfw-menge TO wl_zlest0073-peso_origem.
          ADD lwa_lin_znfw-netwr TO wl_zlest0073-valor_origem.
        ENDLOOP.

      ENDIF.
    ENDIF.

    ADD tg_0060-peso_fiscal   TO  wl_zlest0073-peso_vinculado.
    ADD tg_0060-netwr         TO  wl_zlest0073-valor_vinculado.
    ADD tg_0060-peso_subtotal TO  wl_zlest0073-peso_subtot_vinc.
    ADD tg_0060-peso_liq_ret  TO  wl_zlest0073-peso_liqret_vinc.
    ADD tg_0060-peso_retido   TO  wl_zlest0073-peso_retido_vinc.
  ENDLOOP.

  INSERT zlest0073 FROM wl_zlest0073.

  IF wl_zsdt0001 IS NOT INITIAL.
    CLEAR: wl_zsdt0001-vinc_tot_aquav, wl_zsdt0001-cte_emit_aquav.
    wl_zsdt0001-peso_retido_real = wl_zlest0073-peso_retido_vinc.
    wl_zsdt0001-peso_liqret_real = wl_zlest0073-peso_liqret_vinc.

    IF ( wl_zlest0073-peso_origem - wl_zlest0073-peso_vinculado ) <= 0.
      wl_zsdt0001-vinc_tot_aquav = 'X'.
    ENDIF.

    MODIFY zsdt0001 FROM wl_zsdt0001.
  ENDIF.

  IF wa_complemento IS NOT INITIAL.
    CLEAR: wa_complemento-vinc_tot_aquav, wa_complemento-cte_emit_aquav.
    wa_complemento-peso_retido_real = wl_zlest0073-peso_retido_vinc.
    wa_complemento-peso_liqret_real = wl_zlest0073-peso_liqret_vinc.

    IF ( wl_zlest0073-peso_origem - wl_zlest0073-peso_vinculado ) <= 0.
      wa_complemento-vinc_tot_aquav = 'X'.
    ENDIF.

    MODIFY zlest0205 FROM wa_complemento.
  ENDIF.

*    if ( wl_zlest0073-po_embarque is INITIAL ) or
*       ( wl_zlest0073-po_destino  is INITIAL ).
*      SELECT SINGLE *
*        FROM zlest0056 INTO wl_zlest0056
*       WHERE bukrs      EQ tg_0060-bukrs
*         AND werks      EQ tg_0060-werks
*         AND ano_viagem EQ tg_0060-ano_viagem
*         AND nr_viagem  EQ tg_0060-nr_viagem.
*      IF sy-subrc = 0.
*        wl_zlest0073-po_embarque = wl_zlest0056-po_embarque.
*        wl_zlest0073-po_destino  = wl_zlest0056-po_destino.
*      ENDIF.
*    endif.

ENDFORM.

FORM f_check_cte_autorizado  USING p_chave_nfe TYPE zlest0073-chave_nfe.

  DATA: tg_0060   TYPE TABLE OF zlest0060 WITH HEADER LINE,
        tg_0061   TYPE TABLE OF zlest0061 WITH HEADER LINE,
        wl_0073   TYPE zlest0073,
        wl_0104   TYPE zlest0104,
        vl_docnum TYPE zlest0061-docnum,
        vl_ok     TYPE c.

  CHECK p_chave_nfe IS NOT INITIAL.

  SELECT SINGLE *
    FROM zlest0073 INTO wl_0073
   WHERE chave_nfe = p_chave_nfe.

  CHECK ( sy-subrc = 0 ) AND ( wl_0073-peso_origem > 0 ).

  CHECK ( wl_0073-peso_origem - wl_0073-peso_vinculado ) <= 0.

  SELECT *
    FROM zlest0060 INTO TABLE tg_0060
   WHERE chave_nfe EQ p_chave_nfe.

  CHECK tg_0060[] IS NOT INITIAL.

  vl_ok = 'X'.
  LOOP AT tg_0060.

    SELECT SINGLE *
      FROM zlest0061 INTO tg_0061
     WHERE docnum = tg_0060-docnum.

    IF ( sy-subrc NE 0 ) OR ( tg_0060-docnum IS INITIAL ).
      CLEAR: vl_ok.
      RETURN.
    ENDIF.

    "Verificar se Documento já foi autorizado
    SELECT SINGLE docnum
      FROM j_1bnfe_active INTO vl_docnum
     WHERE docnum     = tg_0061-docnum
       AND cancel     = ''
       AND docsta     = '1'.

    IF sy-subrc NE 0.
      CLEAR: vl_ok.
      RETURN.
    ENDIF.

  ENDLOOP.

  IF vl_ok IS NOT INITIAL. "Todos Ct-es Autorizados.
    READ TABLE tg_0060 INDEX 1.

    CHECK sy-subrc = 0.

    SELECT SINGLE * FROM zlest0104 INTO wl_zlest0104 WHERE emissor = tg_0060-werks.

    IF ( sy-subrc = 0 ) AND
       ( wl_zlest0104-bukrs  IS NOT INITIAL ) AND
       ( wl_zlest0104-branch IS NOT INITIAL ).

      IF tg_0060-chave_nfe IS NOT INITIAL.
        UPDATE zlest0205 SET cte_emit_aquav = 'X'
         WHERE chave_nfe EQ tg_0060-chave_nfe.
      ENDIF.

      UPDATE zsdt0001 SET cte_emit_aquav = 'X'
       WHERE tp_movimento  = 'E'
         AND bukrs         = wl_zlest0104-bukrs
         AND branch        = wl_zlest0104-branch
         AND nr_romaneio   = tg_0060-nr_romaneio
         AND nr_safra      = tg_0060-safra
         AND parid         = tg_0060-rm_codigo.

    ENDIF.

  ENDIF.

ENDFORM.

FORM f_desmarcar_rom USING p_index TYPE sy-tabix
                           p_not_leave_screen
                           p_check_desc_ret
                  CHANGING c_ok.

  CLEAR: c_ok.

  READ TABLE gt_saida_vinc_rom_nome ASSIGNING FIELD-SYMBOL(<fs_saida_vinc_rom_nome>) INDEX p_index.

  CHECK sy-subrc = 0.

  CHECK <fs_saida_vinc_rom_nome>-chave_nfe IS NOT INITIAL.

  "Verifica se tem alguma outra nota com desconto de retenção aplicado. Caso tenha, obrigar a desvincular primeiro.
  IF ( <fs_saida_vinc_rom_nome>-desc_ret_acum IS INITIAL ) AND ( p_check_desc_ret IS NOT INITIAL ).
    LOOP AT gt_saida_vinc_rom_nome INTO DATA(gw_saida_vinc_nome_aux) WHERE check IS NOT INITIAL
                                                                       AND xml  = icon_led_green
                                                                       AND desc_ret_acum > 0.

      MESSAGE 'Retenção já aplicada! Desvincular notas com desconto(em vermelho) para continuar!' TYPE 'S'.
      RETURN.
    ENDLOOP.
  ENDIF.

  CLEAR: wl_zlest0073,
         wl_zlest0060.

  SELECT SINGLE * FROM zlest0073 INTO wl_zlest0073 WHERE chave_nfe EQ <fs_saida_vinc_rom_nome>-chave_nfe.

  SELECT SINGLE * FROM zlest0060 AS a INTO wl_zlest0060 WHERE chave_nfe  EQ wl_zlest0073-chave_nfe
                                                          AND nr_viagem  EQ wp_viagem
                                                          AND ano_viagem EQ wp_ano
                                                          AND nome_emb   EQ wp_embarcacao.
  IF ( sy-subrc EQ 0 ).

    IF wl_zlest0060-desc_ret_acum > 0.
      PERFORM f_retencao_nf USING 'X' "Remover
                                  wl_zlest0060-desc_ret_acum
                         CHANGING _error.

*      IF _ERROR IS NOT INITIAL.
*        ROLLBACK WORK.
*        RETURN.
*      ENDIF.
    ENDIF.

    DELETE FROM zlest0060 WHERE chave_nfe  EQ wl_zlest0060-chave_nfe
                            AND nr_viagem  EQ wl_zlest0060-nr_viagem
                            AND ano_viagem EQ wl_zlest0060-ano_viagem
                            AND nome_emb   EQ wl_zlest0060-nome_emb.
    COMMIT WORK.

    "Adequações Retenção CS2016001199
    CLEAR: wl_zlest0060.
    SELECT SINGLE * FROM zlest0060 AS a INTO wl_zlest0060
     WHERE chave_nfe  EQ wl_zlest0073-chave_nfe
       AND NOT EXISTS ( SELECT * FROM zlest0061 AS b WHERE b~docnum EQ a~docnum AND b~ck_anulado EQ abap_true ).

    IF sy-subrc NE 0. "Caso não exista mais nenhuma vinculação para a chave
      "Remover o X (utilização) da ZSDT0001.

      IF wl_zlest0073-chave_nfe IS NOT INITIAL.
        UPDATE zlest0205 SET ct_aquav         = space
                             vinc_tot_aquav   = space
                             cte_emit_aquav   = space
                             peso_retido_real = 0
                             peso_liqret_real = 0
                       WHERE chave_nfe EQ wl_zlest0073-chave_nfe.
      ENDIF.

      UPDATE zsdt0001 SET ct_aquav         = space
                          vinc_tot_aquav   = space
                          cte_emit_aquav   = space
                          peso_retido_real = 0
                          peso_liqret_real = 0
                    WHERE tp_movimento EQ 'E'
                      AND nr_romaneio  EQ <fs_saida_vinc_rom_nome>-nr_romaneio
                      AND nr_safra     EQ <fs_saida_vinc_rom_nome>-safra
                      AND bukrs        EQ wg_tela1200-bukrs_rom
                      AND branch       EQ wg_tela1200-branch_rom
                      AND parid        EQ wg_tela1200-rm_codigo.
    ENDIF.

    "Atualizar Saldo Table zlest0073
    PERFORM f_atualiza_saldo_nf USING <fs_saida_vinc_rom_nome>.


    "Retira o Valor do Peso Desvinculado.
    IF NOT ( gt_saida_vinc_nf_nome[] IS INITIAL ).


      IF ( total_vinculado > 0 ).
        IF ( <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc IS NOT INITIAL ). "Adequações Retenção CS2016001199 - 17.01.2018 - CS2018000076
          total_vinculado = total_vinculado - <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc.
        ELSE.
          total_vinculado = total_vinculado - <fs_saida_vinc_rom_nome>-peso_vinc.
        ENDIF.

        CLEAR: gw_saida_comboio.
        READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX index_comboio.

        IF ( <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc IS NOT INITIAL ). "Adequações Retenção CS2016001199 - "17.01.2018 - CS2018000076
          gw_saida_comboio-peso_vinculado = gw_saida_comboio-peso_vinculado - <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc.
        ELSE.
          gw_saida_comboio-peso_vinculado = gw_saida_comboio-peso_vinculado - <fs_saida_vinc_rom_nome>-peso_vinc.
        ENDIF.

        MODIFY gt_saida_comboio FROM gw_saida_comboio INDEX index_comboio TRANSPORTING peso_vinculado.
      ENDIF.


      CLEAR: gw_saida_vinc_nf_nome, tabix.

      READ TABLE gt_saida_vinc_nf_nome INTO gw_saida_vinc_nf_nome WITH KEY rm_codigo = wg_tela1200-rm_codigo
                                                                           nr_dco    = <fs_saida_vinc_rom_nome>-nr_dco
                                                                           safra     = <fs_saida_vinc_rom_nome>-safra.
      tabix = sy-tabix.

      IF ( sy-subrc EQ 0 ).
        IF ( <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc IS NOT INITIAL ). "Adequações Retenção CS2016001199 - "17.01.2018 - CS2018000076
          gw_saida_vinc_nf_nome-peso_vinculado  = gw_saida_vinc_nf_nome-peso_vinculado - <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc.
        ELSE.
          gw_saida_vinc_nf_nome-peso_vinculado  = gw_saida_vinc_nf_nome-peso_vinculado - <fs_saida_vinc_rom_nome>-peso_vinc.
        ENDIF.

        IF ( gw_saida_vinc_nf_nome-peso_vinculado EQ 0 ).
          DELETE gt_saida_vinc_nf_nome INDEX tabix.
        ELSE.
          MODIFY gt_saida_vinc_nf_nome FROM gw_saida_vinc_nf_nome INDEX tabix TRANSPORTING peso_vinculado.
        ENDIF.

        CALL METHOD obj_grid_vinc_nf->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      ENDIF.
    ENDIF.

    REFRESH: gt_estilo_romaneio.
    CLEAR: gw_estilo_romaneio.

    CLEAR: <fs_saida_vinc_rom_nome>-estilo,
           <fs_saida_vinc_rom_nome>-peso_vinc,
           <fs_saida_vinc_rom_nome>-valor_vinculado,
           <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc,
           <fs_saida_vinc_rom_nome>-valor_liq_ret_vinc,
           <fs_saida_vinc_rom_nome>-peso_subtotal_vinc,
           <fs_saida_vinc_rom_nome>-perc_ret,
           <fs_saida_vinc_rom_nome>-peso_retido,
           <fs_saida_vinc_rom_nome>-desc_ret_acum,
           <fs_saida_vinc_rom_nome>-desc_ret_acum_grv,
           <fs_saida_vinc_rom_nome>-peso_util_vinc,
           <fs_saida_vinc_rom_nome>-valor_util_vinc.

    CLEAR: wl_zlest0073.
    SELECT SINGLE * FROM zlest0073 INTO wl_zlest0073 WHERE chave_nfe EQ <fs_saida_vinc_rom_nome>-chave_nfe.

    IF ( wl_zlest0073-peso_vinculado EQ 0 ) AND ( wl_zlest0073-valor_vinculado EQ 0 ).
      <fs_saida_vinc_rom_nome>-peso_fiscal   = wl_zlest0073-peso_origem.
      <fs_saida_vinc_rom_nome>-netwr         = wl_zlest0073-valor_origem.
      <fs_saida_vinc_rom_nome>-peso_subtotal = wl_zlest0073-peso_subtotal. "Adequações Retenção CS2016001199

    ELSE.
      <fs_saida_vinc_rom_nome>-peso_fiscal   = wl_zlest0073-peso_origem  - wl_zlest0073-peso_vinculado.
      <fs_saida_vinc_rom_nome>-netwr         = wl_zlest0073-valor_origem - wl_zlest0073-valor_vinculado.
      <fs_saida_vinc_rom_nome>-peso_subtotal = wl_zlest0073-peso_subtotal - wl_zlest0073-peso_subtot_vinc. "Adequações Retenção CS2016001199
    ENDIF.

    "Adequações Retenção CS2016001199
    "IF ( VG_CALC_RETENCAO IS NOT INITIAL ). "17.01.2018 - CS2018000076
    CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
      EXPORTING
        i_bukrs         = <fs_saida_vinc_rom_nome>-bukrs_rom
        i_branch        = <fs_saida_vinc_rom_nome>-branch_rom
        i_lifnr         = <fs_saida_vinc_rom_nome>-parid_rom
        i_peso          = <fs_saida_vinc_rom_nome>-peso_subtotal
        i_peso_fiscal   = <fs_saida_vinc_rom_nome>-peso_fiscal
      IMPORTING
        e_perc_retencao = <fs_saida_vinc_rom_nome>-perc_ret
        e_peso_retido   = <fs_saida_vinc_rom_nome>-peso_retido
        e_peso_liquido  = <fs_saida_vinc_rom_nome>-peso_liq_ret.
    "ENDIF.

    CLEAR: <fs_saida_vinc_rom_nome>-check.

    CLEAR: gw_setleaf.
    SELECT SINGLE *
      FROM setleaf INTO gw_setleaf
     WHERE setname EQ 'MAGGI_ZLES0077_IMPORT'
       AND valfrom EQ w_werks.

    IF NOT ( gw_setleaf IS INITIAL ).
      gw_estilo_romaneio-fieldname = 'PESO_VINC'.
      gw_estilo_romaneio-style = cl_gui_alv_grid=>mc_style_enabled.
      APPEND gw_estilo_romaneio TO gt_estilo_romaneio.

      INSERT LINES OF gt_estilo_romaneio INTO TABLE <fs_saida_vinc_rom_nome>-estilo.
    ELSE.
      CLEAR: <fs_saida_vinc_rom_nome>-peso_vinc, <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc, <fs_saida_vinc_rom_nome>-peso_subtotal_vinc.
      CLEAR: gw_setleaf.
    ENDIF.

  ELSE.

    IF ( <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc IS NOT INITIAL ). "17.01.2018 - CS2018000076
      total_vinculado = total_vinculado - <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc.
    ELSE.
      total_vinculado = total_vinculado - <fs_saida_vinc_rom_nome>-peso_vinc.
    ENDIF.

    CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
      EXPORTING
        i_bukrs         = <fs_saida_vinc_rom_nome>-bukrs_rom
        i_branch        = <fs_saida_vinc_rom_nome>-branch_rom
        i_lifnr         = <fs_saida_vinc_rom_nome>-parid_rom
        i_peso          = <fs_saida_vinc_rom_nome>-peso_subtotal
        i_peso_fiscal   = <fs_saida_vinc_rom_nome>-peso_fiscal
      IMPORTING
        e_perc_retencao = <fs_saida_vinc_rom_nome>-perc_ret
        e_peso_retido   = <fs_saida_vinc_rom_nome>-peso_retido
        e_peso_liquido  = <fs_saida_vinc_rom_nome>-peso_liq_ret.

    CLEAR: <fs_saida_vinc_rom_nome>-check,
           <fs_saida_vinc_rom_nome>-peso_vinc,
           <fs_saida_vinc_rom_nome>-valor_vinculado,
           <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc,
           <fs_saida_vinc_rom_nome>-valor_liq_ret_vinc,
           <fs_saida_vinc_rom_nome>-peso_subtotal_vinc,
           <fs_saida_vinc_rom_nome>-perc_ret,
           <fs_saida_vinc_rom_nome>-peso_retido,
           <fs_saida_vinc_rom_nome>-desc_ret_acum,
           <fs_saida_vinc_rom_nome>-desc_ret_acum_grv,
           <fs_saida_vinc_rom_nome>-peso_util_vinc,
           <fs_saida_vinc_rom_nome>-valor_util_vinc.

  ENDIF.

  PERFORM f_color_row_romaneio USING <fs_saida_vinc_rom_nome>.

  wa_stable-row = 'X'.
  wa_stable-col = 'X'.
  CALL METHOD obj_grid_romaneio->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  IF p_not_leave_screen IS INITIAL.
    LEAVE TO SCREEN 1200.
  ENDIF.

  c_ok = 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  PBO_1600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_1600 OUTPUT.
  SET PF-STATUS 'PF1600'.
  SET TITLEBAR 'TB1600'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI_1600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_1600 INPUT.

* > 07/07/2023 - Migração S4 - LM
*  DATA: tg_cd_destino TYPE TABLE OF char02 WITH HEADER LINE.
  DATA: tg_cd_destino TYPE TABLE OF char2 WITH HEADER LINE.
* > 07/07/2023 - Migração S4 - LM

  DATA funcao TYPE string.

  CASE sy-ucomm.
    WHEN 'BTN_SEND'.
      IF gw_popup_comboio-data_previsao_saida   IS INITIAL OR gw_popup_comboio-hora_previsao_saida IS INITIAL
      OR gw_popup_comboio-data_previsao_chegada IS INITIAL OR gw_popup_comboio-hora_previsao_chegada IS INITIAL.
        MESSAGE TEXT-e04 TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.

*        FUNCAO = COND #( WHEN OK_CODE EQ 'REG_COMBO'
*                          THEN 'Z_LES_REGISTRAR_COMBOIO_LOGONE'
*                          ELSE 'Z_LES_REGISTRAR_COMBOIO_BUNGE'
*                       ).

        CASE ok_code.
          WHEN 'REG_COMBO'. "Logone

            CLEAR: tg_cd_destino[].

            tg_cd_destino = '01'. "Barcarena
            APPEND tg_cd_destino.

            tg_cd_destino = '02'. "Itaituba
            APPEND tg_cd_destino.

            LOOP AT tg_cd_destino.

              CALL FUNCTION 'Z_LES_REGISTRAR_COMBOIO_LOGONE'
                EXPORTING
                  empresa                       = w_bukrs
                  filial                        = w_werks
                  ano                           = w_ano
                  viagem                        = w_viagem
                  data_saida_origem             = gw_popup_comboio-data_previsao_saida
                  hora_saida_origem             = gw_popup_comboio-hora_previsao_saida
                  data_previsao_chegada         = gw_popup_comboio-data_previsao_chegada
                  hora_previsao_chegada         = gw_popup_comboio-hora_previsao_chegada
                  cd_destino                    = tg_cd_destino
                EXCEPTIONS
                  cte_unauthorized              = 1
                  barcaca_without_nf_vinculadas = 2
                  comboio_not_found             = 3
                  comboio_already_sended        = 5
                  unknown_error                 = 6
                  OTHERS                        = 7.

              IF ( sy-subrc <> 0 ).
                MESSAGE ID sy-msgid
                      TYPE 'I'
                    NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                DISPLAY LIKE 'E'.
              ELSE.
                CASE tg_cd_destino.
                  WHEN '01'. "Barcarena
                    MESSAGE i140(zles) DISPLAY LIKE 'S'.
                  WHEN '02'. "Itaituba
                    MESSAGE i139(zles) DISPLAY LIKE 'S'.
                ENDCASE.

              ENDIF.

            ENDLOOP.

          WHEN 'REG_COMBOB'. "Bunge

            CALL FUNCTION 'Z_LES_REGISTRAR_COMBOIO_BUNGE'
              EXPORTING
                empresa                       = w_bukrs
                filial                        = w_werks
                ano                           = w_ano
                viagem                        = w_viagem
                data_saida_origem             = gw_popup_comboio-data_previsao_saida
                hora_saida_origem             = gw_popup_comboio-hora_previsao_saida
                data_previsao_chegada         = gw_popup_comboio-data_previsao_chegada
                hora_previsao_chegada         = gw_popup_comboio-hora_previsao_chegada
              EXCEPTIONS
                cte_unauthorized              = 1
                barcaca_without_nf_vinculadas = 2
                comboio_not_found             = 3
                comboio_already_sended        = 5
                unknown_error                 = 6
                OTHERS                        = 7.

            IF ( sy-subrc <> 0 ).
              MESSAGE ID sy-msgid
                    TYPE 'I'
                  NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            DISPLAY LIKE 'E'.
            ELSE.
              MESSAGE i109(zles) DISPLAY LIKE 'S'.
            ENDIF.

        ENDCASE.

        PERFORM clear_screen_comboio.

        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'EXIT'.
      PERFORM clear_screen_comboio.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.

FORM clear_screen_comboio.
  CLEAR: gw_popup_comboio-data_previsao_saida,
         gw_popup_comboio-data_previsao_chegada,
         gw_popup_comboio-hora_previsao_saida,
         gw_popup_comboio-hora_previsao_chegada.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  F4_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_date_saida INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month = sy-datum
    IMPORTING
      select_date          = gw_popup_comboio-data_previsao_saida.
ENDMODULE.

MODULE f4_date_chegada INPUT.
  CALL FUNCTION 'F4_DATE'
    EXPORTING
      date_for_first_month = sy-datum
    IMPORTING
      select_date          = gw_popup_comboio-data_previsao_chegada.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_DATE_CHEGADA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_time_saida INPUT.
  CALL FUNCTION 'F4_CLOCK'
    EXPORTING
      start_time    = sy-uzeit
      display       = ' '
    IMPORTING
      selected_time = gw_popup_comboio-hora_previsao_saida.
ENDMODULE.

MODULE f4_time_chegada INPUT.
  CALL FUNCTION 'F4_CLOCK'
    EXPORTING
      start_time    = sy-uzeit
      display       = ' '
    IMPORTING
      selected_time = gw_popup_comboio-hora_previsao_chegada.
ENDMODULE.

FORM f_saldo_ret_anterior USING p_saldo TYPE brgew.

  CLEAR: p_saldo.

  "Recuperar saldo geral não utilizado na ZLEST0060
  PERFORM f_sel_saldo_ret TABLES tl_0060_retencao
                           USING ''
                                 ''
                        CHANGING vg_peso_retido
                                 vg_peso_retido_utlz
                                 vg_desc_ret_acum.

  p_saldo = vg_peso_retido - vg_peso_retido_utlz.

  "Recuperar saldo não utilizado somente da viagem, para desconsiderar do saldo geral anterior.
  PERFORM f_sel_saldo_ret TABLES tl_0060_retencao
                          USING ''
                                'X'
                       CHANGING vg_peso_retido
                                vg_peso_retido_utlz
                                vg_desc_ret_acum.

  CLEAR: vg_peso_aux.
  vg_peso_aux = vg_peso_retido - vg_peso_retido_utlz.
  SUBTRACT vg_peso_aux FROM p_saldo.

  "Verificar saldo já utilizado x saldo descontado.
  "Caso o Saldo descontado for superior ao saldo utilizado,
  "pegar a diferença ultrapassada, e descontar do saldo anterior(Prevendo casos de cancelamento extemporaneo)

  PERFORM f_sel_saldo_ret TABLES tl_0060_retencao
                           USING ''
                                 ''
                        CHANGING vg_peso_retido
                                 vg_peso_retido_utlz
                                 vg_desc_ret_acum.

  IF vg_desc_ret_acum > vg_peso_retido_utlz.
    vg_peso_aux = vg_desc_ret_acum - vg_peso_retido_utlz.

    "Tenta distribuir diferença excedida
    PERFORM f_retencao_nf USING '' "Aplicar Desconto
                                vg_peso_aux
                      CHANGING _error.

    IF _error IS NOT INITIAL. "Se não conseguir, tirar diferença do saldo anterior
      SUBTRACT vg_peso_aux FROM p_saldo.
    ELSE. "Senão recalcular saldo
      MESSAGE 'Retenção excedida distribuída!' TYPE 'S'.
      PERFORM f_action_1200_enter_nf.
      LEAVE TO SCREEN 1200.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_sel_saldo_ret TABLES p_zlest0060
                      USING p_ret_registros
                            p_viagem_atual
                   CHANGING c_peso_retido       TYPE zlest0060-peso_retido
                            c_peso_retido_utlz  TYPE zlest0060-peso_retido_utlz
                            c_desc_ret_acum     TYPE zlest0060-desc_ret_acum.

  CLEAR: c_peso_retido, c_peso_retido_utlz, c_desc_ret_acum, p_zlest0060[].

  "Buscar informações para condições de Portochuelo.
  SELECT SINGLE * FROM zlest0104 INTO wl_zlest0104 WHERE emissor = w_werks.

  RANGES: r_bukrs        FOR zlest0060-bukrs,
          r_werks        FOR zlest0060-werks,
          r_nr_viagem    FOR zlest0060-nr_viagem,
          r_ano_viagem   FOR zlest0060-ano_viagem,
          r_embarcacao   FOR zlest0060-embarcacao,
          r_nome_emb     FOR zlest0060-nome_emb,
          r_rm_codigo    FOR zlest0060-rm_codigo.

  IF p_viagem_atual IS NOT INITIAL.
    r_bukrs-sign    = 'I'.
    r_bukrs-option  = 'EQ'.
    r_bukrs-low     = wp_bukrs.
    APPEND r_bukrs.

    r_werks-sign    = 'I'.
    r_werks-option  = 'EQ'.
    r_werks-low     = wp_werks.
    APPEND r_werks.

    r_nr_viagem-sign    = 'I'.
    r_nr_viagem-option  = 'EQ'.
    r_nr_viagem-low     = wp_viagem.
    APPEND r_nr_viagem.

    r_ano_viagem-sign    = 'I'.
    r_ano_viagem-option  = 'EQ'.
    r_ano_viagem-low     = wp_ano.
    APPEND r_ano_viagem.

    r_embarcacao-sign    = 'I'.
    r_embarcacao-option  = 'EQ'.
    r_embarcacao-low     =  gw_saida_comboio-embarcacao+0(1).
    APPEND r_embarcacao.

    r_nome_emb-sign    = 'I'.
    r_nome_emb-option  = 'EQ'.
    r_nome_emb-low     = gw_saida_comboio-nome.
    APPEND r_nome_emb.
  ENDIF.

  r_rm_codigo-sign    = 'I'.
  r_rm_codigo-option  = 'EQ'.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wg_tela1200-rm_codigo
    IMPORTING
      output = r_rm_codigo-low.

  APPEND r_rm_codigo.

  IF p_ret_registros IS INITIAL.

    "Classificação da Soja
    CASE gw_saida_comboio-tp_class(2).

      WHEN: 'CO'.

        CASE tp_romane.
          WHEN: 'PROPRIO'.

            SELECT SUM( peso_retido )
                   SUM( peso_retido_utlz )
                   SUM( desc_ret_acum )
              FROM zlest0060 INTO
                      ( c_peso_retido,
                        c_peso_retido_utlz ,
                        c_desc_ret_acum )
             WHERE bukrs_rom       EQ wl_zlest0104-bukrs
               AND branch_rom      EQ wl_zlest0104-branch
               AND rm_codigo       IN r_rm_codigo
               AND matnr           EQ gw_saida_comboio-matnr
               AND tp_transgenia   EQ 'CO'
               AND local_descarga  EQ wl_zlest0104-local_descarga
               AND ret_acumulada   EQ 'X'
               AND bukrs           IN r_bukrs
               AND werks           IN r_werks
               AND nr_viagem       IN r_nr_viagem
               AND ano_viagem      IN r_ano_viagem
               AND embarcacao      IN r_embarcacao
               AND nome_emb        IN r_nome_emb.

          WHEN: 'TERCEIRO'.

            SELECT SUM( peso_retido )
                   SUM( peso_retido_utlz )
                   SUM( desc_ret_acum )
              FROM zlest0060 INTO
                      ( c_peso_retido,
                        c_peso_retido_utlz ,
                        c_desc_ret_acum )
            WHERE bukrs_rom       EQ wl_zlest0104-bukrs
              AND branch_rom      EQ wl_zlest0104-branch
              AND rm_codigo       IN r_rm_codigo
              AND matnr           EQ gw_saida_comboio-matnr
              AND tp_transgenia   EQ 'CO'
              AND local_descarga  EQ wl_zlest0104-local_descarga
              AND ret_acumulada   EQ 'X'
              AND bukrs           IN r_bukrs
              AND werks           IN r_werks
              AND nr_viagem       IN r_nr_viagem
              AND ano_viagem      IN r_ano_viagem
              AND embarcacao      IN r_embarcacao
              AND nome_emb        IN r_nome_emb.

        ENDCASE.

      WHEN: 'R1'.

        CASE tp_romane.
          WHEN: 'PROPRIO'.

            SELECT SUM( peso_retido )
                   SUM( peso_retido_utlz )
                   SUM( desc_ret_acum )
              FROM zlest0060 INTO
                       ( c_peso_retido,
                         c_peso_retido_utlz ,
                         c_desc_ret_acum )
             WHERE bukrs_rom       EQ wl_zlest0104-bukrs
               AND branch_rom      EQ wl_zlest0104-branch
               AND rm_codigo       IN r_rm_codigo
               AND matnr           EQ gw_saida_comboio-matnr
               AND tp_transgenia   IN ('P1','T1','D1')
               AND local_descarga  EQ wl_zlest0104-local_descarga
               AND ret_acumulada   EQ 'X'
               AND bukrs           IN r_bukrs
               AND werks           IN r_werks
               AND nr_viagem       IN r_nr_viagem
               AND ano_viagem      IN r_ano_viagem
               AND embarcacao      IN r_embarcacao
               AND nome_emb        IN r_nome_emb.

          WHEN: 'TERCEIRO'.

            SELECT SUM( peso_retido )
                   SUM( peso_retido_utlz )
                   SUM( desc_ret_acum )
              FROM zlest0060 INTO
                      ( c_peso_retido,
                        c_peso_retido_utlz ,
                        c_desc_ret_acum )
              WHERE bukrs_rom      EQ wl_zlest0104-bukrs
                AND branch_rom     EQ wl_zlest0104-branch
                AND rm_codigo       IN r_rm_codigo
                AND matnr          EQ gw_saida_comboio-matnr
                AND tp_transgenia  IN ('P1','T1','D1')
                AND local_descarga EQ wl_zlest0104-local_descarga
                AND ret_acumulada  EQ 'X'
                AND bukrs           IN r_bukrs
                AND werks           IN r_werks
                AND nr_viagem       IN r_nr_viagem
                AND ano_viagem      IN r_ano_viagem
                AND embarcacao      IN r_embarcacao
                AND nome_emb        IN r_nome_emb.

        ENDCASE.

      WHEN: 'R2'.

        CASE tp_romane.

          WHEN: 'PROPRIO'.
            SELECT SUM( peso_retido )
                   SUM( peso_retido_utlz )
                   SUM( desc_ret_acum )
              FROM zlest0060 INTO
                      ( c_peso_retido,
                        c_peso_retido_utlz ,
                        c_desc_ret_acum )
             WHERE bukrs_rom       EQ wl_zlest0104-bukrs
               AND branch_rom      EQ wl_zlest0104-branch
               AND rm_codigo       IN r_rm_codigo
               AND matnr           EQ gw_saida_comboio-matnr
               AND tp_transgenia   IN ('P2','T2','D2')
               AND local_descarga  EQ wl_zlest0104-local_descarga
               AND ret_acumulada   EQ 'X'
               AND bukrs           IN r_bukrs
               AND werks           IN r_werks
               AND nr_viagem       IN r_nr_viagem
               AND ano_viagem      IN r_ano_viagem
               AND embarcacao      IN r_embarcacao
               AND nome_emb        IN r_nome_emb.

          WHEN: 'TERCEIRO'.

            SELECT SUM( peso_retido )
                   SUM( peso_retido_utlz )
                   SUM( desc_ret_acum )
              FROM zlest0060 INTO
                      ( c_peso_retido,
                        c_peso_retido_utlz ,
                        c_desc_ret_acum )
             WHERE bukrs_rom       EQ wl_zlest0104-bukrs
               AND branch_rom      EQ wl_zlest0104-branch
               AND rm_codigo       IN r_rm_codigo
               AND matnr           EQ gw_saida_comboio-matnr
               AND tp_transgenia   IN ('P2','T2','D2')
               AND local_descarga  EQ wl_zlest0104-local_descarga
               AND ret_acumulada   EQ 'X'
               AND bukrs           IN r_bukrs
               AND werks           IN r_werks
               AND nr_viagem       IN r_nr_viagem
               AND ano_viagem      IN r_ano_viagem
               AND embarcacao      IN r_embarcacao
               AND nome_emb        IN r_nome_emb.

        ENDCASE.
    ENDCASE.

  ELSE. "Retorna Registros

    "Classificação da Soja
    CASE gw_saida_comboio-tp_class(2).

      WHEN: 'CO'.

        CASE tp_romane.
          WHEN: 'PROPRIO'.

            SELECT *
              FROM zlest0060 INTO TABLE p_zlest0060
             WHERE bukrs_rom       EQ wl_zlest0104-bukrs
               AND branch_rom      EQ wl_zlest0104-branch
               AND rm_codigo       IN r_rm_codigo
               AND matnr           EQ gw_saida_comboio-matnr
               AND tp_transgenia   EQ 'CO'
               AND local_descarga  EQ wl_zlest0104-local_descarga
               AND ret_acumulada   EQ 'X'
               AND bukrs           IN r_bukrs
               AND werks           IN r_werks
               AND nr_viagem       IN r_nr_viagem
               AND ano_viagem      IN r_ano_viagem
               AND embarcacao      IN r_embarcacao
               AND nome_emb        IN r_nome_emb.

          WHEN: 'TERCEIRO'.

            SELECT *
              FROM zlest0060 INTO TABLE p_zlest0060
             WHERE bukrs_rom       EQ wl_zlest0104-bukrs
               AND branch_rom      EQ wl_zlest0104-branch
               AND rm_codigo       IN r_rm_codigo
               AND matnr           EQ gw_saida_comboio-matnr
               AND tp_transgenia   EQ 'CO'
               AND local_descarga  EQ wl_zlest0104-local_descarga
               AND ret_acumulada   EQ 'X'
               AND bukrs           IN r_bukrs
               AND werks           IN r_werks
               AND nr_viagem       IN r_nr_viagem
               AND ano_viagem      IN r_ano_viagem
               AND embarcacao      IN r_embarcacao
               AND nome_emb        IN r_nome_emb.

        ENDCASE.

      WHEN: 'R1'.

        CASE tp_romane.
          WHEN: 'PROPRIO'.

            SELECT *
              FROM zlest0060 INTO TABLE p_zlest0060
             WHERE bukrs_rom       EQ wl_zlest0104-bukrs
               AND branch_rom      EQ wl_zlest0104-branch
               AND rm_codigo       IN r_rm_codigo
               AND matnr           EQ gw_saida_comboio-matnr
               AND tp_transgenia   IN ('P1','T1','D1')
               AND local_descarga  EQ wl_zlest0104-local_descarga
               AND ret_acumulada   EQ 'X'
               AND bukrs           IN r_bukrs
               AND werks           IN r_werks
               AND nr_viagem       IN r_nr_viagem
               AND ano_viagem      IN r_ano_viagem
               AND embarcacao      IN r_embarcacao
               AND nome_emb        IN r_nome_emb.

          WHEN: 'TERCEIRO'.

            SELECT *
               FROM zlest0060 INTO TABLE p_zlest0060
              WHERE bukrs_rom      EQ wl_zlest0104-bukrs
                AND branch_rom     EQ wl_zlest0104-branch
                AND rm_codigo       IN r_rm_codigo
                AND matnr          EQ gw_saida_comboio-matnr
                AND tp_transgenia  IN ('P1','T1','D1')
                AND local_descarga EQ wl_zlest0104-local_descarga
                AND ret_acumulada  EQ 'X'
                AND bukrs           IN r_bukrs
                AND werks           IN r_werks
                AND nr_viagem       IN r_nr_viagem
                AND ano_viagem      IN r_ano_viagem
                AND embarcacao      IN r_embarcacao
                AND nome_emb        IN r_nome_emb.

        ENDCASE.

      WHEN: 'R2'.

        CASE tp_romane.

          WHEN: 'PROPRIO'.

            SELECT *
              FROM zlest0060 INTO TABLE p_zlest0060
             WHERE bukrs_rom       EQ wl_zlest0104-bukrs
               AND branch_rom      EQ wl_zlest0104-branch
               AND rm_codigo       IN r_rm_codigo
               AND matnr           EQ gw_saida_comboio-matnr
               AND tp_transgenia   IN ('P2','T2','D2')
               AND local_descarga  EQ wl_zlest0104-local_descarga
               AND ret_acumulada   EQ 'X'
               AND bukrs           IN r_bukrs
               AND werks           IN r_werks
               AND nr_viagem       IN r_nr_viagem
               AND ano_viagem      IN r_ano_viagem
               AND embarcacao      IN r_embarcacao
               AND nome_emb        IN r_nome_emb.

          WHEN: 'TERCEIRO'.

            SELECT *
              FROM zlest0060 INTO TABLE p_zlest0060
             WHERE bukrs_rom       EQ wl_zlest0104-bukrs
               AND branch_rom      EQ wl_zlest0104-branch
               AND rm_codigo       IN r_rm_codigo
               AND matnr           EQ gw_saida_comboio-matnr
               AND tp_transgenia   IN ('P2','T2','D2')
               AND local_descarga  EQ wl_zlest0104-local_descarga
               AND ret_acumulada   EQ 'X'
               AND bukrs           IN r_bukrs
               AND werks           IN r_werks
               AND nr_viagem       IN r_nr_viagem
               AND ano_viagem      IN r_ano_viagem
               AND embarcacao      IN r_embarcacao
               AND nome_emb        IN r_nome_emb.

        ENDCASE.
    ENDCASE.

  ENDIF.


ENDFORM.

FORM f_atualiza_saldo_retencao USING p_atualiza_anterior.

  DATA: vl_peso_vinc_ret TYPE brgew.

  FIELD-SYMBOLS: <fs_saida_vinc_rom_nome> TYPE zaqty_saida_vinc_romaneio.

  "PERFORM F_DESELECT_ALL USING 'X'. "Retira seleção de NF's com retençao descontada

  IF p_atualiza_anterior IS NOT INITIAL.
    "Buscar Saldo Anterior Acumulado de Retenção
    PERFORM f_saldo_ret_anterior CHANGING wg_tela1200-ret_anterior.
  ENDIF.

  CLEAR: wg_tela1200-retencao_sel, wg_tela1200-tot_ret_acum, wg_tela1200-sld_ret_acum.
  LOOP AT gt_saida_vinc_rom_nome ASSIGNING <fs_saida_vinc_rom_nome> WHERE check  = 'X'
                                                                      AND estilo IS INITIAL
                                                                      AND xml    = icon_led_green.

    vl_peso_vinc_ret = <fs_saida_vinc_rom_nome>-peso_retido - <fs_saida_vinc_rom_nome>-peso_retido_utlz.

    IF vl_peso_vinc_ret > 0.
      ADD <fs_saida_vinc_rom_nome>-peso_retido TO wg_tela1200-retencao_sel.
    ENDIF.

    <fs_saida_vinc_rom_nome>-ret_acumulada = 'X'.
    "<FS_SAIDA_VINC_ROM_NOME>-DESC_RET_ACUM = 0.
  ENDLOOP.

  wg_tela1200-tot_ret_acum = wg_tela1200-ret_anterior + wg_tela1200-retencao_sel.
  wg_tela1200-sld_ret_acum = wg_tela1200-tot_ret_acum.

ENDFORM.

FORM f_desconta_retencao.

  DATA: vl_peso_ret_desc          TYPE brgew,
        vl_peso_retido            TYPE brgew,
        vl_desc_ret_acum          TYPE brgew,
        vl_peso_liq_vinc          TYPE brgew,
        vl_peso_subtotal_vinc_aux TYPE brgew,
        vl_peso_vinc_aux          TYPE brgew.

  FIELD-SYMBOLS: <fs_saida_vinc_rom_nome> TYPE zaqty_saida_vinc_romaneio.

  DATA(_vinc_fiscal) = 'X'.
  LOOP AT gt_saida_vinc_rom_nome INTO DATA(gw_saida_vinc_nome_aux) WHERE estilo      IS INITIAL
                                                                     AND xml         EQ icon_led_green
                                                                     AND vinc_fiscal IS INITIAL.
    _vinc_fiscal = ''.
    EXIT.
  ENDLOOP.

  IF _vinc_fiscal IS NOT INITIAL.
    MESSAGE 'Desconto de retenção não permitido para o remetente informado!' TYPE 'S'.
    RETURN.
  ENDIF.

  LOOP AT gt_saida_vinc_rom_nome INTO gw_saida_vinc_nome_aux WHERE check IS NOT INITIAL
                                                               AND xml  = icon_led_green
                                                               AND desc_ret_acum > 0.

    MESSAGE 'Retenção já aplicada! Desvincular notas com desconto(em vermelho) para continuar!' TYPE 'S'.
    RETURN.
  ENDLOOP.

  PERFORM f_atualiza_saldo_retencao USING 'X'.

  "Aplica Saldo Retenção ultimas notas selecionadas
  DATA(_lines) = lines( gt_saida_vinc_rom_nome[] ).
  WHILE ( _lines > 0 ) AND ( wg_tela1200-sld_ret_acum > 0 ).
    READ TABLE gt_saida_vinc_rom_nome ASSIGNING <fs_saida_vinc_rom_nome> INDEX _lines.

    CHECK sy-subrc = 0.

    IF ( <fs_saida_vinc_rom_nome>-vinc_fiscal IS INITIAL ) AND
       ( <fs_saida_vinc_rom_nome>-estilo IS INITIAL      ) AND
       ( <fs_saida_vinc_rom_nome>-check  IS NOT INITIAL  ) AND
       ( <fs_saida_vinc_rom_nome>-xml = icon_led_green   ) AND
       ( <fs_saida_vinc_rom_nome>-peso_liq_ret       > 0 ).

      IF wg_tela1200-sld_ret_acum < <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc.
        vl_peso_ret_desc = wg_tela1200-sld_ret_acum.
      ELSE.
        vl_peso_ret_desc = <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc - 1. "Fazer desconto e deixa 1 kg para fazer a vinculação da nota
      ENDIF.

      vl_peso_liq_vinc = <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc.

      PERFORM f_desmarcar_rom USING _lines 'X' ''
                            CHANGING vg_ok_aux.

      <fs_saida_vinc_rom_nome>-desc_ret_acum  = vl_peso_ret_desc.
      SUBTRACT vl_peso_ret_desc FROM wg_tela1200-sld_ret_acum.

      CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
        EXPORTING
          i_bukrs         = <fs_saida_vinc_rom_nome>-bukrs_rom
          i_branch        = <fs_saida_vinc_rom_nome>-branch_rom
          i_lifnr         = <fs_saida_vinc_rom_nome>-parid_rom
          i_peso          = <fs_saida_vinc_rom_nome>-peso_subtotal
          i_peso_fiscal   = <fs_saida_vinc_rom_nome>-peso_fiscal
        IMPORTING
          e_perc_retencao = <fs_saida_vinc_rom_nome>-perc_ret
          e_peso_retido   = <fs_saida_vinc_rom_nome>-peso_retido
          e_peso_liquido  = <fs_saida_vinc_rom_nome>-peso_liq_ret.

      PERFORM f_marcar_rom USING _lines vl_peso_liq_vinc 'X'.

    ENDIF.

    SUBTRACT 1 FROM _lines.
  ENDWHILE.

  "Verifica se Retenção foi Aplicada.
  IF wg_tela1200-sld_ret_acum > 0.
    MESSAGE 'Retenção acumulada não foi totalmente aplicada!' TYPE 'S'.
  ENDIF.


ENDFORM.

FORM f_deselect_all  USING p_desc_ret_acum TYPE c.

  DATA: tl_estilo_aux TYPE lvc_t_styl WITH HEADER LINE,
        wl_estilo_aux TYPE lvc_s_styl,
        vl_index      TYPE i.

  IF p_desc_ret_acum IS INITIAL.

    LOOP AT gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome WHERE check IS NOT INITIAL
                                                                 AND xml = icon_led_green.

      CLEAR: vl_index.
      vl_index = sy-tabix.

      tl_estilo_aux[] = gw_saida_vinc_rom_nome-estilo[].

      READ TABLE tl_estilo_aux INTO wl_estilo_aux WITH KEY fieldname = 'CHECK'.
      CHECK sy-subrc NE 0.

      PERFORM f_desmarcar_rom USING vl_index 'X' 'X'
                           CHANGING vg_ok_aux.

      IF vg_ok_aux IS INITIAL.
        EXIT.
      ENDIF.

    ENDLOOP.

  ELSE.

    LOOP AT gt_saida_vinc_rom_nome INTO gw_saida_vinc_rom_nome WHERE check IS NOT INITIAL
                                                                 AND xml  = icon_led_green
                                                                 AND desc_ret_acum > 0.

      CLEAR: vl_index.
      vl_index = sy-tabix.

      tl_estilo_aux[] = gw_saida_vinc_rom_nome-estilo[].

      READ TABLE tl_estilo_aux INTO wl_estilo_aux WITH KEY fieldname = 'CHECK'.
      CHECK sy-subrc NE 0.

      PERFORM f_desmarcar_rom USING vl_index 'X' 'X'
                           CHANGING vg_ok_aux.
    ENDLOOP.

  ENDIF.



ENDFORM.

FORM f_retencao_nf USING p_remover
                         p_peso TYPE brgew
                CHANGING p_error.

  DATA: vl_retencao_aplic TYPE brgew,
        vl_desc_ret_acum  TYPE brgew,
        vl_ret_vinc       TYPE brgew,
        vl_peso_base      TYPE brgew.

  CLEAR: p_error, vl_retencao_aplic, vl_desc_ret_acum..

  IF p_remover IS INITIAL.

    vl_peso_base = p_peso. "Retenção Aplicar.

    CHECK vl_peso_base > 0.

    PERFORM f_sel_saldo_ret TABLES tl_0060_retencao
                             USING 'X'
                                   ''
                          CHANGING vg_peso_retido
                                   vg_peso_retido_utlz
                                   vg_desc_ret_acum.

    LOOP AT tl_0060_retencao WHERE peso_retido_sld > 0.
      IF vl_peso_base  <= 0.
        EXIT.
      ENDIF.

      CLEAR: vl_ret_vinc.
      IF vl_peso_base < tl_0060_retencao-peso_retido_sld.
        vl_ret_vinc = vl_peso_base.
      ELSE.
        vl_ret_vinc = tl_0060_retencao-peso_retido_sld.
      ENDIF.

      ADD vl_ret_vinc TO tl_0060_retencao-peso_retido_utlz.

      IF tl_0060_retencao-peso_retido_utlz > tl_0060_retencao-peso_retido.
        p_error = 'X'.
        MESSAGE |Peso de Retenção Excedido! Total Peso Retido: { tl_0060_retencao-peso_retido } - Peso Retido Utlz.: { tl_0060_retencao-peso_retido_utlz } - Peso Distrib.:{ vl_ret_vinc } | TYPE 'S'.
        ROLLBACK WORK.
        RETURN.
      ENDIF.

      tl_0060_retencao-peso_retido_sld = tl_0060_retencao-peso_retido - tl_0060_retencao-peso_retido_utlz.
      MODIFY zlest0060 FROM tl_0060_retencao.

      SUBTRACT vl_ret_vinc FROM vl_peso_base.
      ADD vl_ret_vinc TO vl_retencao_aplic.

*      IF ( TL_0060_RETENCAO-BUKRS       EQ WP_BUKRS  ) AND
*         ( TL_0060_RETENCAO-WERKS       EQ WP_WERKS  ) AND
*         ( TL_0060_RETENCAO-NR_VIAGEM   EQ WP_VIAGEM ) AND
*         ( TL_0060_RETENCAO-ANO_VIAGEM  EQ WP_ANO    ) AND
*         ( TL_0060_RETENCAO-EMBARCACAO  EQ GW_SAIDA_COMBOIO-EMBARCACAO+0(1) ) AND
*         ( TL_0060_RETENCAO-NOME_EMB    EQ GW_SAIDA_COMBOIO-NOME ) AND
*         ( TL_0060_RETENCAO-RM_CODIGO   EQ WG_TELA1200-RM_CODIGO ).
*        ADD TL_0060_RETENCAO-DESC_RET_ACUM TO VL_DESC_RET_ACUM.
*      ENDIF.
    ENDLOOP.

    IF ( vl_retencao_aplic <> p_peso ).
      p_error = 'X'.
      MESSAGE 'Não foi possível descontar a retenção!' TYPE 'S'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

  ELSE.

    vl_peso_base = p_peso. "Retenção Remover

    CHECK vl_peso_base > 0.

    PERFORM f_sel_saldo_ret TABLES tl_0060_retencao
                             USING 'X'
                                   ''
                          CHANGING vg_peso_retido
                                   vg_peso_retido_utlz
                                   vg_desc_ret_acum.

    LOOP AT tl_0060_retencao WHERE peso_retido_utlz > 0.
      IF vl_peso_base <= 0.
        EXIT.
      ENDIF.

      CLEAR: vl_ret_vinc.

      IF vl_peso_base < tl_0060_retencao-peso_retido_utlz.
        vl_ret_vinc = vl_peso_base.
      ELSE.
        vl_ret_vinc = tl_0060_retencao-peso_retido_utlz.
      ENDIF.

      SUBTRACT vl_ret_vinc FROM tl_0060_retencao-peso_retido_utlz.

      IF tl_0060_retencao-peso_retido_utlz > tl_0060_retencao-peso_retido.
        p_error = 'X'.
        MESSAGE |Peso de Retenção Excedido! Total Peso Retido: { tl_0060_retencao-peso_retido } - Peso Retido Utlz.: { tl_0060_retencao-peso_retido_utlz } - Peso Distrib.:{ vl_ret_vinc } | TYPE 'S'.
        ROLLBACK WORK.
        RETURN.
      ENDIF.

      tl_0060_retencao-peso_retido_sld = tl_0060_retencao-peso_retido - tl_0060_retencao-peso_retido_utlz.
      MODIFY zlest0060 FROM tl_0060_retencao.

      SUBTRACT vl_ret_vinc FROM vl_peso_base.
      ADD vl_ret_vinc TO vl_retencao_aplic.
    ENDLOOP.

    IF ( vl_retencao_aplic <> p_peso ) .
      p_error = 'X'.
      "MESSAGE 'Houve um erro ao remover a retenção!' TYPE 'S'.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

  ENDIF.

ENDFORM.

FORM f_marcar_rom  USING p_index          TYPE sy-tabix
                         p_peso_vinc      TYPE brgew
                         p_not_leave_screen.

  DATA: wl_color  TYPE kkblo_specialcol.

  READ TABLE gt_saida_vinc_rom_nome ASSIGNING FIELD-SYMBOL(<fs_saida_vinc_rom_nome>) INDEX p_index.

  CHECK ( sy-subrc = 0 ).

  CHECK ( <fs_saida_vinc_rom_nome>-xml = icon_led_green ) AND ( <fs_saida_vinc_rom_nome>-chave_nfe IS NOT INITIAL ).

  REFRESH: gt_estilo_romaneio.
  CLEAR: gw_estilo_romaneio.

  CLEAR: <fs_saida_vinc_rom_nome>-estilo.

  <fs_saida_vinc_rom_nome>-check           = 'X'.

*  <FS_SAIDA_VINC_ROM_NOME>-PESO_VINC       = <FS_SAIDA_VINC_ROM_NOME>-PESO_FISCAL.
*  <FS_SAIDA_VINC_ROM_NOME>-VALOR_VINCULADO = <FS_SAIDA_VINC_ROM_NOME>-NETWR.

  IF <fs_saida_vinc_rom_nome>-vinc_fiscal IS INITIAL.

    IF ( p_peso_vinc > 0 ) AND ( p_peso_vinc <= <fs_saida_vinc_rom_nome>-peso_liq_ret ).
      PERFORM f_atrib_peso USING <fs_saida_vinc_rom_nome>
                                 p_peso_vinc
                                 ''  "Vinculo Total
                                 '2'. "Peso Retido
    ELSE.
      PERFORM f_atrib_peso USING <fs_saida_vinc_rom_nome>
                                 <fs_saida_vinc_rom_nome>-peso_liq_ret
                                 'X'  "Vinculo Total
                                 '2'. "Peso Retido
    ENDIF.

  ELSE.
    PERFORM f_atrib_peso USING <fs_saida_vinc_rom_nome>
                               <fs_saida_vinc_rom_nome>-peso_fiscal
                               'X'  "Vinculo Total
                               '1'. "Peso Fiscal
  ENDIF.

  CLEAR: vl_vlr_vinc.

  "Adequações Retenção CS2016001199
  IF ( <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc IS NOT INITIAL ). "17.01.2018 - CS2018000076
    vl_vlr_vinc = <fs_saida_vinc_rom_nome>-peso_liq_ret_vinc.
  ELSE.
    vl_vlr_vinc = <fs_saida_vinc_rom_nome>-peso_vinc.
  ENDIF.

  ADD vl_vlr_vinc TO total_vinculado.

  IF vl_vlr_vinc IS INITIAL.
    MESSAGE 'Nenhum peso encontrado para vinculação!' TYPE 'S'.
    <fs_saida_vinc_rom_nome>-check = ''.

    wa_stable-row = 'X'.
    wa_stable-col = 'X'.
    CALL METHOD obj_grid_romaneio->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    LEAVE TO SCREEN 1200.
    EXIT.
  ENDIF.

  PERFORM f_color_row_romaneio USING <fs_saida_vinc_rom_nome>.

  CLEAR: gw_saida_comboio.
  READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX index_comboio.

  IF ( gw_saida_comboio-peso_vinculado EQ 0 ).
    gw_saida_comboio-peso_vinculado = total_vinculado.
  ELSE.
    gw_saida_comboio-peso_vinculado = total_vinculado.
  ENDIF.

  MODIFY gt_saida_comboio FROM gw_saida_comboio INDEX index_comboio TRANSPORTING peso_vinculado.

  wa_stable-row = 'X'.
  wa_stable-col = 'X'.
  CALL METHOD obj_grid_romaneio->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  IF p_not_leave_screen IS INITIAL.
    LEAVE TO SCREEN 1200.
  ENDIF.


ENDFORM.

FORM f_color_row_romaneio  USING  p_saida_vinc_romaneio TYPE zaqty_saida_vinc_romaneio.

  DATA: wl_color  TYPE kkblo_specialcol.

  CLEAR: p_saida_vinc_romaneio-color.

  IF p_saida_vinc_romaneio-desc_ret_acum IS NOT INITIAL.
    CLEAR: wl_color.
    wl_color-fieldname = 'PESO_UTIL_VINC'.
    wl_color-color-col = 6.
    wl_color-color-inv = 6.
    APPEND wl_color TO p_saida_vinc_romaneio-color.

    CLEAR: wl_color.
    wl_color-fieldname = 'VALOR_UTIL_VINC'.
    wl_color-color-col = 6.
    wl_color-color-inv = 6.
    APPEND wl_color TO p_saida_vinc_romaneio-color.
  ENDIF.

ENDFORM.

FORM f_action_1200_enter_nf .

  DATA: wl_zlest0152 TYPE zlest0152.

  DATA: v_kunnr_branch TYPE kna1-kunnr.

  CLEAR: total_vinculado.

  PERFORM f_get_cfop_operacao.

  IF wg_tela1200-dt_codigo IS INITIAL.
    MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Informe o Destinatário.'.
    EXIT.
  ELSEIF wg_tela1200-rm_codigo IS INITIAL.
    MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Informe o Remetente.'.
    EXIT.
  ELSEIF ( wg_tela1200-tomador_cod IS INITIAL ).
    MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Informe o Tomador de Serviço.'.
    EXIT.
  ELSEIF ( wg_tela1200-operacao IS INITIAL ).
    MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Informe a Operação'.
    EXIT.
  ENDIF.

  READ TABLE gt_saida_comboio INTO gw_saida_comboio INDEX index_comboio.

  "Seleciona para saber se o grupo de material é de insumos.
  "LES - US 165779 - Transp. Aquav. Algodao - WPP -->>
  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_tvarvc_insumos)
   WHERE name EQ 'ZLES0077_MATKL_INSUMOS'
     AND low  EQ @gw_saida_comboio-matkl.

*  SELECT SINGLE * FROM setleaf INTO wl_setleaf
*          WHERE setname EQ 'MAGGI_ZLES0077_GR_MAT'
*            AND valfrom EQ gw_saida_comboio-matkl.
  "LES - US 165779 - Transp. Aquav. Algodao - WPP <<--

  IF ( sy-subrc NE 0 ).

    "Consultar Controle de Remetente e Destinatario.
    CLEAR: opcao, vg_remetente, vg_destinatario, vl_parid.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_tela1200-rm_codigo
      IMPORTING
        output = vl_parid.

    vg_remetente    = vl_parid.
    vg_destinatario = wg_tela1200-dt_codigo.
    opcao = 'C'.
    PERFORM: controle_remet_destin USING vg_remetente vg_destinatario opcao.

    IF ( opcao EQ 'I' ).

      "Verificar se o remetente é terceiro ou proprio.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wg_tela1200-rm_codigo
        IMPORTING
          output = vl_parid.

      SELECT SINGLE * FROM lfa1 INTO wl_lfa1_nf WHERE lifnr EQ vl_parid.

      "ZFIC = INTERCOMPNAY (PROPRIO).
      "OUTROS = TERCEIRO
      CASE wl_lfa1_nf-ktokk.
        WHEN: 'ZFIC'.
          tp_notas  = 'PROPRIO'.
          tp_romane = 'PROPRIO'.
        WHEN OTHERS.
          tp_notas  = 'TERCEIRO'.
          tp_romane = 'TERCEIRO'.
      ENDCASE.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wg_tela1200-dt_codigo
        IMPORTING
          output = vl_kunnr.


      SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ vl_kunnr.
      IF ( sy-subrc NE 0 ).
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Destinatário não encontrado.'.
        EXIT.
      ENDIF.

      REFRESH: tl_zdco_vinculo, tl_t001w, tl_zlest0060_nf_vinc, tl_zsdt0001, gt_saida_vinc_rom_nome, tl_t001w.
      CLEAR: tl_t001w.

      "Seleção de Romaneios.
      PERFORM f_selecao_romaneio TABLES tl_zsdt0001 USING '1'.
      "PERFORM processo_complemento PLANTÃO 21/09 - Leila: IR199538
      PERFORM processo_complemento.

      " IF tl_zsdt0001[] IS INITIAL.   " AJUSTE LEILA 21/09/2024 -VERIFICAR Leila: IR199538
      IF tl_zsdt0001[] IS INITIAL AND gt_saida_vinc_rom_nome IS INITIAL.
        "Gravar Controle de Remetente e Destinatario.
        vg_remetente    = vl_parid.
        vg_destinatario = wg_tela1200-dt_codigo.
        opcao = 'D'.
        PERFORM: controle_remet_destin USING vg_remetente vg_destinatario opcao.

        CLEAR: wp_frete_id, wg_tela1200-id_frete_aqua.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Nenhum romaneio encontrado!'.
        EXIT.
      ENDIF.

      DATA(wg_tela1200_id_local_descarga) = wg_tela1200-id_local_descarga.
      DATA(wg_tela1200_ds_local_descarga) = wg_tela1200-ds_local_descarga.

      CLEAR: wp_frete_id, wg_tela1200-id_frete_aqua, wg_tela1200-id_local_descarga, wg_tela1200-ds_local_descarga.

      SORT tl_zsdt0001 BY dt_movimento nr_romaneio.

      IF tl_zsdt0001[] IS NOT INITIAL. "LES - Seleção NFs COmplemento ZLES0077 - Issue #154726 - WPP --->>>

        CASE tp_romane.

          WHEN: 'PROPRIO'.

            SELECT vbelv vbtyp_n vbeln mjahr
               FROM vbfa
               INTO TABLE tl_vbfa
               FOR ALL ENTRIES IN tl_zsdt0001
             WHERE vbelv   EQ tl_zsdt0001-doc_rem
               AND vbtyp_n IN ( 'M', 'R' ).

            CHECK NOT tl_vbfa[] IS INITIAL.
            FIELD-SYMBOLS: <fs_vbfa> TYPE ty_vbfa.

            LOOP AT tl_vbfa ASSIGNING <fs_vbfa>.
              <fs_vbfa>-vbelv                     = <fs_vbfa>-vbelv.
              <fs_vbfa>-vbtyp_n                   = <fs_vbfa>-vbtyp_n.
              <fs_vbfa>-vbeln                     = <fs_vbfa>-vbeln.
              IF <fs_vbfa>-vbtyp_n = 'R'.
                CONCATENATE  <fs_vbfa>-vbeln <fs_vbfa>-mjahr INTO  <fs_vbfa>-vbeln_refkey.
              ELSE.
                <fs_vbfa>-vbeln_refkey              = <fs_vbfa>-vbeln.
              ENDIF.
            ENDLOOP.

            UNASSIGN <fs_vbfa>.

            SELECT * FROM j_1bnflin
              INTO TABLE tl_j_1bnflin
              FOR ALL ENTRIES IN tl_vbfa
            WHERE refkey EQ tl_vbfa-vbeln_refkey.

            SELECT * FROM j_1bnfe_active
              INTO TABLE tl_j_1bnfe_active
              FOR ALL ENTRIES IN tl_j_1bnflin
            WHERE docnum EQ tl_j_1bnflin-docnum
              AND cancel NE 'X'.

            "Buscar Informações do DCO
            CASE tp_notas.
              WHEN: 'PROPRIO'.
                CHECK NOT tl_zsdt0001[] IS INITIAL.
                "Recupera o DCO das Notas.
                SELECT * FROM zdco_vinculo
                  INTO TABLE tl_zdco_vinculo
                  FOR ALL ENTRIES IN tl_zsdt0001
                WHERE vbeln EQ tl_zsdt0001-doc_rem.
            ENDCASE.

          WHEN: 'TERCEIRO'.


        ENDCASE.

      ENDIF. "LES - Seleção NFs COmplemento ZLES0077 - Issue #154726 - WPP <<----

      "Buscar Notas Baixadas por Quebra
      CLEAR: tg_zlest0158[].
      IF tl_zsdt0001[] IS NOT INITIAL.
        SELECT *
          FROM zlest0158 INTO TABLE tg_zlest0158
           FOR ALL ENTRIES IN tl_zsdt0001
         WHERE ch_referencia = tl_zsdt0001-ch_referencia.

        SORT tg_zlest0158 BY ch_referencia.
      ENDIF.

      "Buscar Notas Vinculadas.
      REFRESH: tl_zlest0060_nf_vinc[].
      CLEAR: wl_zlest0060_nf_vinc.

      IF tl_zsdt0001[] IS NOT INITIAL. "LES - Seleção NFs COmplemento ZLES0077 - Issue #154726 - WPP <<----

        SELECT * FROM zlest0060 AS a
          INTO TABLE tl_zlest0060_nf_vinc
          FOR ALL ENTRIES IN tl_zsdt0001
        WHERE nr_romaneio    EQ tl_zsdt0001-nr_romaneio
          AND bukrs          EQ wp_bukrs
          AND werks          EQ wp_werks
          AND nr_viagem      EQ w_viagem
          AND ano_viagem     EQ w_ano
          AND nome_emb       EQ gw_saida_comboio-nome
          AND local_descarga EQ tl_zsdt0001-local_descarga
          AND NOT EXISTS ( SELECT * FROM zlest0061 AS b WHERE b~docnum EQ a~docnum AND b~ck_anulado EQ abap_true ).

      ENDIF. "LES - Seleção NFs COmplemento ZLES0077 - Issue #154726 - WPP <<----

      "Adequações Retenção CS2016001199
*      "Deletar os Romaneios já utilizados e gerados CT-e.
*      CLEAR: wl_zsdt0001.
*      LOOP AT tl_zsdt0001 INTO wl_zsdt0001.
*        tabix = sy-tabix.
*        IF ( wl_zsdt0001-docnum_aquav NE '0000000000' ) AND ( wl_zsdt0001-ct_aquav EQ 'X').
*          DELETE tl_zsdt0001 INDEX tabix.
*        ENDIF.
*      ENDLOOP.

      "Verifica fornecedores com vinculação fiscal.
      tl_zsdt0001_aux[] = tl_zsdt0001[].
      SORT tl_zsdt0001_aux BY bukrs branch parid.
      DELETE ADJACENT DUPLICATES FROM tl_zsdt0001_aux COMPARING bukrs branch parid.
      CLEAR: tl_forn_vinc_fis[].
      DATA(_vinc_fiscal) = ''.
      LOOP AT tl_zsdt0001_aux INTO DATA(wl_zsdt0001_aux).
        CALL FUNCTION 'ZLES_PARAM_FORN_AQUA'
          EXPORTING
            i_bukrs_rom  = wl_zsdt0001_aux-bukrs
            i_branch_rom = wl_zsdt0001_aux-branch
            i_lifnr      = wl_zsdt0001_aux-parid
          IMPORTING
            e_zlest0152  = wl_zlest0152.

        IF ( wl_zlest0152 IS NOT INITIAL ) AND ( wl_zlest0152-vinc_fiscal IS NOT INITIAL ).
          tl_forn_vinc_fis-bukrs   = wl_zsdt0001_aux-bukrs.
          tl_forn_vinc_fis-branch  = wl_zsdt0001_aux-branch.
          tl_forn_vinc_fis-parid   = wl_zsdt0001_aux-parid.
          APPEND tl_forn_vinc_fis.
        ENDIF.
      ENDLOOP.
      SORT tl_forn_vinc_fis BY bukrs branch parid.
      "Fim

      LOOP AT tl_zsdt0001 INTO wl_zsdt0001.

        "Não pode estar baixada
        READ TABLE tg_zlest0158 WITH KEY ch_referencia = wl_zsdt0001-ch_referencia BINARY SEARCH.
        CHECK sy-subrc NE 0.

        CLEAR: wl_vbfa, wl_j_1bnflin, wl_j_1bnfdoc , wl_j_1bnfe_active, wl_zlest0073, gw_saida_vinc_rom_nome.


        READ TABLE tl_forn_vinc_fis WITH KEY bukrs  = wl_zsdt0001-bukrs
                                             branch = wl_zsdt0001-branch
                                             parid  = wl_zsdt0001-parid BINARY SEARCH.
        IF sy-subrc = 0.
          gw_saida_vinc_rom_nome-vinc_fiscal = 'X'.
        ENDIF.

        CASE tp_romane.

          WHEN: 'PROPRIO'.

            "READ TABLE TL_VBFA INTO WL_VBFA WITH KEY VBELV = WL_ZSDT0001-DOC_REM.

            "READ TABLE TL_J_1BNFLIN INTO WL_J_1BNFLIN WITH KEY REFKEY = WL_VBFA-VBELN_REFKEY.

            "READ TABLE TL_J_1BNFE_ACTIVE INTO WL_J_1BNFE_ACTIVE WITH KEY DOCNUM = WL_J_1BNFLIN-DOCNUM.

            LOOP AT tl_vbfa INTO wl_vbfa WHERE vbelv = wl_zsdt0001-doc_rem.

              READ TABLE tl_j_1bnflin INTO wl_j_1bnflin WITH KEY refkey = wl_vbfa-vbeln_refkey.

              CHECK sy-subrc EQ 0.

              READ TABLE tl_j_1bnfe_active INTO wl_j_1bnfe_active WITH KEY docnum = wl_j_1bnflin-docnum.

              IF ( sy-subrc EQ 0 ) AND ( wl_j_1bnfe_active-nfnum9 = wl_zsdt0001-nfnum ). "Adequações Retenção CS2016001199
                EXIT.
              ENDIF.

            ENDLOOP.

            IF ( sy-subrc EQ 0 ).

              gw_saida_vinc_rom_nome-embarcacao    = gw_saida_comboio-embarcacao+0(1).
              gw_saida_vinc_rom_nome-nome          = gw_saida_comboio-nome.
              gw_saida_vinc_rom_nome-nr_romaneio   = wl_zsdt0001-nr_romaneio.
              gw_saida_vinc_rom_nome-dt_movimento  = wl_zsdt0001-dt_movimento.
              gw_saida_vinc_rom_nome-docdat        = wl_zsdt0001-docdat.
              gw_saida_vinc_rom_nome-nfnum         = wl_zsdt0001-nfnum.

              IF wl_j_1bnflin-cfop IS INITIAL.
                CONTINUE.
              ELSE.
                PERFORM f_set_operacao_nf USING wl_j_1bnflin-cfop(4)
                                       CHANGING gw_saida_vinc_rom_nome-operacao.

                IF gw_saida_vinc_rom_nome-operacao IS INITIAL.
                  CONTINUE.
                ENDIF.
              ENDIF.

              IF gw_saida_vinc_rom_nome-operacao NE wg_tela1200-operacao.
                CONTINUE.
              ENDIF.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wl_zsdt0001-series
                IMPORTING
                  output = gw_saida_vinc_rom_nome-series.

              gw_saida_vinc_rom_nome-rm_codigo     = wg_tela1200-rm_codigo.
              gw_saida_vinc_rom_nome-dt_codigo     = wg_tela1200-dt_codigo.
              gw_saida_vinc_rom_nome-id_frete_aqua = wg_tela1200-id_frete_aqua.

              SELECT SINGLE *
                FROM j_1bnfdoc INTO wl_j_1bnfdoc
               WHERE docnum = wl_j_1bnflin-docnum.

              CHECK ( sy-subrc EQ 0 ).

              IF wl_j_1bnfdoc-partyp EQ 'B'.
                v_kunnr_branch = wl_j_1bnfdoc-parid.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = v_kunnr_branch
                  IMPORTING
                    output = v_kunnr_branch.

                v_kunnr_branch = v_kunnr_branch+6(4).

                wl_j_1bnfdoc-parid = v_kunnr_branch.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = wl_j_1bnfdoc-parid
                  IMPORTING
                    output = wl_j_1bnfdoc-parid.

                wl_j_1bnfdoc-partyp = 'C'.
              ENDIF.

              CHECK ( vl_kunnr EQ wl_j_1bnfdoc-parid ).

              gw_saida_vinc_rom_nome-safra         = wl_zsdt0001-nr_safra.
              gw_saida_vinc_rom_nome-doc_rem       = wl_zsdt0001-doc_rem.

              "Adequações Retenção CS2016001199
              IF ( wl_zsdt0001-netwr        > 0 ) AND
                 ( wl_zsdt0001-peso_fiscal  > 0 ).
                gw_saida_vinc_rom_nome-netpr = wl_zsdt0001-netwr / wl_zsdt0001-peso_fiscal.
              ENDIF.

              gw_saida_vinc_rom_nome-ch_referencia   = wl_zsdt0001-ch_referencia.
              gw_saida_vinc_rom_nome-bukrs_rom       = wl_zsdt0001-bukrs.
              gw_saida_vinc_rom_nome-branch_rom      = wl_zsdt0001-branch.
              gw_saida_vinc_rom_nome-parid_rom       = wl_zsdt0001-parid.
              gw_saida_vinc_rom_nome-matnr           = wl_zsdt0001-matnr.
              gw_saida_vinc_rom_nome-tp_transgenia   = wl_zsdt0001-tp_transgenia.
              gw_saida_vinc_rom_nome-local_descarga  = wl_zsdt0001-local_descarga.
              "Fim

              "Concatenar a chave da nota fiscal
              CONCATENATE
                  wl_j_1bnfe_active-regio
                  wl_j_1bnfe_active-nfyear
                  wl_j_1bnfe_active-nfmonth
                  wl_j_1bnfe_active-stcd1
                  wl_j_1bnfe_active-model
                  wl_j_1bnfe_active-serie
                  wl_j_1bnfe_active-nfnum9
                  wl_j_1bnfe_active-docnum9
                  wl_j_1bnfe_active-cdv
             INTO gw_saida_vinc_rom_nome-chave_nfe.

              gw_saida_vinc_rom_nome-docnum_rem = wl_j_1bnfe_active-docnum.

              PERFORM f_check_saldo_nfe USING gw_saida_vinc_rom_nome.

              READ TABLE tl_zdco_vinculo INTO wl_zdco_vinculo WITH KEY vbeln = wl_zsdt0001-doc_rem.
              gw_saida_vinc_rom_nome-nr_dco = wl_zdco_vinculo-nr_dco.

              IF NOT ( wl_zsdt0001-doc_rem IS INITIAL ).
                gw_saida_vinc_rom_nome-xml = icon_led_green.
              ELSE.

                "SELECT SINGLE * FROM ZLEST0065 INTO WL_ZLEST0065 WHERE NFENUM EQ WL_ZSDT0001-NFNUM
                "                                                  AND STCD1  EQ WL_LFA1_NF-STCD1.

                CLEAR: wl_zib_nfe_dist_ter.
                SELECT SINGLE *
                  INTO wl_zib_nfe_dist_ter
                  FROM zib_nfe_dist_ter
                 WHERE forne_cnpj = wl_lfa1_nf-stcd1
                   AND numero     = wl_zsdt0001-nfnum
                   AND serie      = wl_zsdt0001-series.

                IF ( sy-subrc EQ 0 ).
                  gw_saida_vinc_rom_nome-xml = icon_led_green.
                ELSE.
                  gw_saida_vinc_rom_nome-xml = icon_led_red.
                ENDIF.
              ENDIF.


              SELECT SINGLE * FROM zlest0073 INTO wl_zlest0073 WHERE chave_nfe EQ gw_saida_vinc_rom_nome-chave_nfe.
              IF ( sy-subrc NE 0 ).
                gw_saida_vinc_rom_nome-peso_fiscal   = wl_zsdt0001-peso_fiscal.
                gw_saida_vinc_rom_nome-netwr         = wl_zsdt0001-netwr.

                "Adequações Retenção CS2016001199
                gw_saida_vinc_rom_nome-peso_subtotal = wl_zsdt0001-peso_subtotal.

                "IF ( VG_CALC_RETENCAO IS NOT INITIAL ). "17.01.2018 - CS2018000076
                CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
                  EXPORTING
                    i_bukrs         = wl_zsdt0001-bukrs
                    i_branch        = wl_zsdt0001-branch
                    i_lifnr         = wl_zsdt0001-parid
                    i_peso          = wl_zsdt0001-peso_subtotal
                    i_peso_fiscal   = wl_zsdt0001-peso_fiscal
                  IMPORTING
                    e_perc_retencao = gw_saida_vinc_rom_nome-perc_ret
                    e_peso_retido   = gw_saida_vinc_rom_nome-peso_retido
                    e_peso_liquido  = gw_saida_vinc_rom_nome-peso_liq_ret.
                "ENDIF.
                "Fim
              ENDIF.

              READ TABLE tl_zlest0060_nf_vinc INTO wl_zlest0060_nf_vinc WITH KEY nr_romaneio = wl_zsdt0001-nr_romaneio
                                                                                 safra       = wl_zsdt0001-nr_safra
                                                                                 nr_dco      = wl_zdco_vinculo-nr_dco.
              IF sy-subrc = 0.
                "Verificar se CT-e das NF's já foi autorizado
                SELECT SINGLE docnum
                  FROM j_1bnfe_active INTO vl_docnum
                 WHERE docnum     = wl_zlest0060_nf_vinc-docnum
                   AND cancel     = ''
                   AND docsta     = '1'.
                IF sy-subrc = 0.
                  CONTINUE.
                ENDIF.
              ENDIF.

              LOOP AT tl_zlest0060_nf_vinc INTO wl_zlest0060_nf_vinc WHERE nr_romaneio EQ wl_zsdt0001-nr_romaneio
                                                                       AND bukrs       EQ wp_bukrs
                                                                       AND werks       EQ wp_werks
                                                                       AND nr_viagem   EQ w_viagem
                                                                       AND ano_viagem  EQ w_ano
                                                                       AND nome_emb    EQ gw_saida_comboio-nome
                                                                       AND safra       EQ wl_zsdt0001-nr_safra
                                                                       AND nr_dco      EQ wl_zdco_vinculo-nr_dco.

                IF wl_zlest0060_nf_vinc-peso_liq_ret IS NOT INITIAL.
                  total_vinculado = total_vinculado + wl_zlest0060_nf_vinc-peso_liq_ret.
                ELSE.
                  total_vinculado = total_vinculado + wl_zlest0060_nf_vinc-peso_fiscal.
                ENDIF.
              ENDLOOP.

              "Adequações Retenção CS2016001199
              "Verifica se o romaneio esta selecionado para aquela barcaça, caso não esteja não mostrar para o usuário.
*              SELECT SINGLE * FROM zlest0060 INTO  wl_zlest0060_nf_vinc_aux WHERE nr_romaneio EQ wl_zsdt0001-nr_romaneio
*                                                                              AND bukrs       EQ wp_bukrs
*                                                                              AND werks       EQ wp_werks
*                                                                              AND nr_viagem   EQ w_viagem
*                                                                              AND ano_viagem  EQ w_ano
*                                                                              AND nome_emb    NE gw_saida_comboio-nome
*                                                                              AND safra       EQ wl_zsdt0001-nr_safra
*                                                                              AND nr_dco      EQ wl_zdco_vinculo-nr_dco.
*
*              IF ( sy-subrc EQ 0 ).
*                CLEAR: wl_vbfa, wl_j_1bnflin, wl_j_1bnfe_active, wl_zsdt0001, wl_zlest0073, wl_zlest0060_nf_vinc_aux.
*                CONTINUE.
*
*              ELSE.
*                APPEND gw_saida_vinc_rom_nome TO gt_saida_vinc_rom_nome.
*                CLEAR: gw_saida_vinc_rom_nome, wl_zdco_vinculo, wl_zlest0060_nf_vinc, wl_zib_nfe_dist_ter.  "WL_ZLEST0065.
*
*              ENDIF.

              APPEND gw_saida_vinc_rom_nome TO gt_saida_vinc_rom_nome.
              CLEAR: gw_saida_vinc_rom_nome, wl_zdco_vinculo, wl_zlest0060_nf_vinc, wl_zib_nfe_dist_ter.  "WL_ZLEST0065.
              "Fim

            ELSE.
              CLEAR: wl_vbfa, wl_j_1bnflin, wl_j_1bnfe_active, wl_zsdt0001, wl_zlest0073.
              CONTINUE.
            ENDIF.

          WHEN: 'TERCEIRO'.

            "CLEAR: WL_ZLEST0065.

            READ TABLE tl_zlest0060_nf_vinc INTO wl_zlest0060_nf_vinc WITH KEY nr_romaneio  = wl_zsdt0001-nr_romaneio.

            IF sy-subrc = 0.
              "Verificar se CT-e das NF's já foi autorizado
              SELECT SINGLE docnum
                FROM j_1bnfe_active INTO vl_docnum
               WHERE docnum     = wl_zlest0060_nf_vinc-docnum
                 AND cancel     = ''
                 AND docsta     = '1'.
              IF sy-subrc = 0.
                CONTINUE.
              ENDIF.
            ENDIF.

            LOOP AT tl_zlest0060_nf_vinc INTO wl_zlest0060_nf_vinc WHERE nr_romaneio EQ wl_zsdt0001-nr_romaneio
                                                                     AND bukrs       EQ wp_bukrs
                                                                     AND werks       EQ wp_werks
                                                                     AND nr_viagem   EQ w_viagem
                                                                     AND ano_viagem  EQ w_ano
                                                                     AND nome_emb    EQ gw_saida_comboio-nome
                                                                     AND safra       EQ wl_zsdt0001-nr_safra
                                                                     AND nr_dco      EQ wl_zdco_vinculo-nr_dco.

              IF wl_zlest0060_nf_vinc-peso_liq_ret IS NOT INITIAL.
                total_vinculado = total_vinculado + wl_zlest0060_nf_vinc-peso_liq_ret.
              ELSE.
                total_vinculado = total_vinculado + wl_zlest0060_nf_vinc-peso_fiscal.
              ENDIF.
            ENDLOOP.

            gw_saida_vinc_rom_nome-embarcacao    = gw_saida_comboio-embarcacao+0(1).
            gw_saida_vinc_rom_nome-nome          = gw_saida_comboio-nome.
            gw_saida_vinc_rom_nome-nr_romaneio   = wl_zsdt0001-nr_romaneio.
            gw_saida_vinc_rom_nome-dt_movimento  = wl_zsdt0001-dt_movimento.
            gw_saida_vinc_rom_nome-docdat        = wl_zsdt0001-docdat.
            gw_saida_vinc_rom_nome-nfnum         = wl_zsdt0001-nfnum.


            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wl_zsdt0001-series
              IMPORTING
                output = gw_saida_vinc_rom_nome-series.


            gw_saida_vinc_rom_nome-rm_codigo     = wg_tela1200-rm_codigo.
            gw_saida_vinc_rom_nome-dt_codigo     = wg_tela1200-dt_codigo.
            gw_saida_vinc_rom_nome-safra         = wl_zsdt0001-nr_safra.
            gw_saida_vinc_rom_nome-doc_rem       = wl_zsdt0001-doc_rem.

            "Adequações Retenção CS2016001199
            IF ( wl_zsdt0001-netwr        > 0 ) AND
               ( wl_zsdt0001-peso_fiscal  > 0 ).
              gw_saida_vinc_rom_nome-netpr = wl_zsdt0001-netwr / wl_zsdt0001-peso_fiscal.
            ENDIF.

            gw_saida_vinc_rom_nome-ch_referencia   = wl_zsdt0001-ch_referencia.
            gw_saida_vinc_rom_nome-bukrs_rom       = wl_zsdt0001-bukrs.
            gw_saida_vinc_rom_nome-branch_rom      = wl_zsdt0001-branch.
            gw_saida_vinc_rom_nome-parid_rom       = wl_zsdt0001-parid.
            gw_saida_vinc_rom_nome-matnr           = wl_zsdt0001-matnr.
            gw_saida_vinc_rom_nome-tp_transgenia   = wl_zsdt0001-tp_transgenia.
            gw_saida_vinc_rom_nome-local_descarga  = wl_zsdt0001-local_descarga.
            "Fim

            IF wl_zsdt0001-nfe = ' '.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wg_tela1200-rm_codigo
                IMPORTING
                  output = vl_parid.

              SELECT SINGLE *
               FROM lfa1 INTO wl_lfa1
               WHERE lifnr = vl_parid.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wg_tela1200-dt_codigo
                IMPORTING
                  output = vl_parid.

              SELECT SINGLE *
               FROM kna1 INTO wl_kna1_aux
              WHERE kunnr = vl_parid.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wl_zsdt0001-series
                IMPORTING
                  output = wl_zsdt0001-series.

              CONCATENATE
                 'F'
                 wl_lfa1-regio
                 wl_zsdt0001-docdat+2(2)
                 wl_zsdt0001-docdat+4(2)
                 '000'
                 wl_lfa1-stcd2
                 '04'
                 wl_zsdt0001-series
                 wl_zsdt0001-nfnum
              INTO gw_saida_vinc_rom_nome-chave_nfe.

              gw_saida_vinc_rom_nome-xml = icon_led_green.

              IF wl_zsdt0001-cfop IS NOT INITIAL.
                PERFORM f_set_operacao_nf USING wl_zsdt0001-cfop CHANGING gw_saida_vinc_rom_nome-operacao.
                IF gw_saida_vinc_rom_nome-operacao IS INITIAL.
                  CONTINUE.
                ENDIF.
              ELSEIF wl_zsdt0001-id_carga IS NOT INITIAL AND wl_zsdt0001-id_nota IS NOT INITIAL.

                SELECT SINGLE * INTO @DATA(wa_zsdt0001nt)
                  FROM zsdt0001nt
                 WHERE id_carga EQ @wl_zsdt0001-id_carga
                   AND id_nota  EQ @wl_zsdt0001-id_nota.
                IF sy-subrc IS INITIAL.
                  PERFORM f_set_operacao_nf USING wa_zsdt0001nt-cfop CHANGING gw_saida_vinc_rom_nome-operacao.
                  IF gw_saida_vinc_rom_nome-operacao IS INITIAL.
                    CONTINUE.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF gw_saida_vinc_rom_nome-operacao NE wg_tela1200-operacao.
                CONTINUE.
              ENDIF.

            ELSE.
              CLEAR: wl_zib_nfe_dist_ter, wl_zib_nfe_dist_itm.
              "IR112431  - Verificar se nota é emitida por PF/PJ
              IF wl_lfa1_nf-stcd1 IS INITIAL.
                SELECT SINGLE *
                 INTO wl_zib_nfe_dist_ter
                 FROM zib_nfe_dist_ter
                WHERE forne_cpf  = wl_lfa1_nf-stcd2
                  AND forne_ie   = wl_lfa1_nf-stcd3
                  AND numero     = wl_zsdt0001-nfnum
                  AND serie      = wl_zsdt0001-series.
              ELSE.
                SELECT SINGLE *
                  INTO wl_zib_nfe_dist_ter
                  FROM zib_nfe_dist_ter
                 WHERE forne_cnpj = wl_lfa1_nf-stcd1
                   AND numero     = wl_zsdt0001-nfnum
                   AND serie      = wl_zsdt0001-series.
              ENDIF.
              "IR112431  - Verificar se nota é emitida por PF/PJ
              IF sy-subrc EQ 0.
                SELECT SINGLE *
                  INTO wl_zib_nfe_dist_itm
                  FROM zib_nfe_dist_itm
                 WHERE chave_nfe = wl_zib_nfe_dist_ter-chave_nfe.
              ENDIF.

              IF ( sy-subrc EQ 0 ).

                IF wl_zib_nfe_dist_itm-prod_cfop IS INITIAL.
                  CONTINUE.
                ELSE.
                  PERFORM f_set_operacao_nf USING wl_zib_nfe_dist_itm-prod_cfop(4)
                                         CHANGING gw_saida_vinc_rom_nome-operacao.

                  IF gw_saida_vinc_rom_nome-operacao IS INITIAL.
                    CONTINUE.
                  ENDIF.
                ENDIF.

                IF gw_saida_vinc_rom_nome-operacao NE wg_tela1200-operacao.
                  CONTINUE.
                ENDIF.

                "GW_SAIDA_VINC_ROM_NOME-CHAVE_NFE    = WL_ZLEST0065-CH_ACESSO.
                gw_saida_vinc_rom_nome-chave_nfe    = wl_zib_nfe_dist_ter-chave_nfe.
                gw_saida_vinc_rom_nome-xml = icon_led_green.

                vl_tomador = |{ wl_zib_nfe_dist_ter-f_tomadora ALPHA = IN }|.

                IF vl_tomador IS INITIAL.
                  SELECT SINGLE *
                    FROM kna1 INTO wl_kna1_aux
                   WHERE stcd1 = wl_zib_nfe_dist_ter-destino_cnpj
                    AND  sperr NE 'X'.

                  IF ( sy-subrc NE 0 ) OR ( vl_kunnr NE wl_kna1_aux-kunnr ).
                    CONTINUE.
                  ENDIF.
                ELSE.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = vl_tomador
                    IMPORTING
                      output = vl_tomador.
                  SELECT SINGLE *
                    FROM kna1 INTO wl_kna1_aux
                      WHERE kunnr EQ vl_tomador.
                  IF sy-subrc IS NOT INITIAL.
                    CONTINUE.
                  ENDIF.
                ENDIF.
              ELSE.
                gw_saida_vinc_rom_nome-xml = icon_led_red.

              ENDIF.
            ENDIF.

            PERFORM f_check_saldo_nfe USING gw_saida_vinc_rom_nome.

            SELECT SINGLE * FROM zlest0073 INTO wl_zlest0073 WHERE chave_nfe EQ gw_saida_vinc_rom_nome-chave_nfe.

            IF ( sy-subrc NE 0 ).

              gw_saida_vinc_rom_nome-peso_fiscal   = wl_zsdt0001-peso_fiscal.
              gw_saida_vinc_rom_nome-netwr         = wl_zsdt0001-netwr.

              "Adequações Retenção CS2016001199
              gw_saida_vinc_rom_nome-peso_subtotal = wl_zsdt0001-peso_subtotal.

              "IF ( VG_CALC_RETENCAO IS NOT INITIAL ). "17.01.2018 - CS2018000076
              CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
                EXPORTING
                  i_bukrs         = wl_zsdt0001-bukrs
                  i_branch        = wl_zsdt0001-branch
                  i_lifnr         = wl_zsdt0001-parid
                  i_peso          = wl_zsdt0001-peso_subtotal
                  i_peso_fiscal   = wl_zsdt0001-peso_fiscal
                IMPORTING
                  e_perc_retencao = gw_saida_vinc_rom_nome-perc_ret
                  e_peso_retido   = gw_saida_vinc_rom_nome-peso_retido
                  e_peso_liquido  = gw_saida_vinc_rom_nome-peso_liq_ret.
              "ENDIF.
              "Fim

            ENDIF.

            "SELECT SINGLE *
            "  FROM ZLEST0065 INTO WL_ZLEST0065
            " WHERE NFENUM EQ WL_ZSDT0001-NFNUM
            "   AND STCD1  EQ WL_LFA1_NF-STCD1.

            APPEND gw_saida_vinc_rom_nome TO gt_saida_vinc_rom_nome.

            CLEAR: gw_saida_vinc_rom_nome, wl_zdco_vinculo, wl_zlest0060_nf_vinc, wl_zib_nfe_dist_ter. "WL_ZLEST0065.

        ENDCASE.

        CLEAR: wl_vbfa, wl_j_1bnflin, wl_j_1bnfe_active, wl_zsdt0001, wl_zlest0073, gw_saida_vinc_rom_nome.
      ENDLOOP.

      "PERFORM processo_complemento  LES - Seleção NFs COmplemento ZLES0077 - Issue #154726 - WPP
      "USING GW_SAIDA_COMBOIO
      "      WP_BUKRS
      "      WP_WERKS
      "      WP_ANO
      "      WP_VIAGEM
      "CHANGING OPCAO
      "         WG_TELA1200
      "         TOTAL_VINCULADO
      .

      IF NOT ( gt_saida_vinc_rom_nome[] IS INITIAL ).

*  "Validação da O.V Gerado
*  "Desabilitar Quando a O.V Estiver Gerada.
        PERFORM: validacao_romaneio.

        "Calcula Saldos de Retenção
        PERFORM: f_atualiza_saldo_retencao USING 'X'.

      ENDIF.

      "Gravar Controle de Remetente e Destinatario.
      vg_remetente    = vl_parid.
      vg_destinatario = wg_tela1200-dt_codigo.
      opcao = 'I'.
      PERFORM: controle_remet_destin USING vg_remetente vg_destinatario opcao.

    ENDIF.

    IF wg_tela1200-id_local_descarga IS INITIAL.
      wg_tela1200-id_local_descarga = wg_tela1200_id_local_descarga.
      wg_tela1200-ds_local_descarga = wg_tela1200_ds_local_descarga.
    ENDIF.

  ELSE.

*&--------------------------------------------------------------------&*
*& Data.....: 26.02.2014 10:48:42
*& Descrição: INSUMOS - INICIO
*&--------------------------------------------------------------------&*
    PERFORM: processo_insumos USING gw_saida_comboio.
*&--------------------------------------------------------------------&*
*& Data.....: 26.02.2014 10:48:42
*& Descrição: INSUMOS - FIM
*&--------------------------------------------------------------------&*

    "Vincular Notas Terceiro
    opcao = 'I'.
    PERFORM processo_complemento.


    IF ( gt_saida_vinc_rom_nome[] IS NOT INITIAL ).

*      "Validação da O.V Gerado
*      "Desabilitar Quando a O.V Estiver Gerada.
      PERFORM: validacao_romaneio.

      "Gravar Controle de Remetente e Destinatario.
      vg_remetente    = vl_parid.
      vg_destinatario = wg_tela1200-dt_codigo.
      opcao = 'I'.
      PERFORM: controle_remet_destin USING vg_remetente vg_destinatario opcao.
    ENDIF.

  ENDIF.


ENDFORM.

FORM f_selecao_romaneio TABLES p_zsdt0001  STRUCTURE zsdt0001
                         USING p_cte_emit_aquav TYPE c.

  IF wg_tela1200-id_local_descarga IS INITIAL.
    MESSAGE i151 WITH wp_werks  gw_saida_viagem-po_embarque.
    EXIT.
  ENDIF.

  CALL FUNCTION 'ZLES_SELECAO_ROM_AQUA'
    EXPORTING
      i_tp_romaneio    = tp_romane
      i_series         = wg_tela1200-series
      i_nr_safra       = wg_tela1200-nr_safra
      i_cte_emit_aquav = p_cte_emit_aquav
      i_centro_emissor = w_werks
      i_matnr          = gw_saida_comboio-matnr
      i_tp_class       = gw_saida_comboio-tp_class(2)
      i_parid          = vl_parid
      i_local_descarga = wg_tela1200-id_local_descarga
      i_eudr           = gw_saida_comboio-eudr(1) "WP_EUDR(1) "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
    TABLES
      e_zsdt0001       = p_zsdt0001.

*  RANGES: R_SERIES         FOR ZSDT0001-SERIES,
*          R_CTE_EMIT_AQUAV FOR ZSDT0001-CTE_EMIT_AQUAV.
*
*  CLEAR: R_SERIES,         R_SERIES[],
*         R_CTE_EMIT_AQUAV, R_CTE_EMIT_AQUAV[],
*         P_ZSDT0001[].
*
*  IF WG_TELA1200-SERIES IS NOT INITIAL.
*    R_SERIES-SIGN   = 'I'.
*    R_SERIES-OPTION = 'EQ'.
*    R_SERIES-LOW    = WG_TELA1200-SERIES.
*    APPEND R_SERIES.
*  ENDIF.
*
*  CASE P_CTE_EMIT_AQUAV.
*    WHEN '1'.
*      R_CTE_EMIT_AQUAV-SIGN   = 'I'.
*      R_CTE_EMIT_AQUAV-OPTION = 'EQ'.
*      R_CTE_EMIT_AQUAV-LOW    = ''.
*      APPEND R_CTE_EMIT_AQUAV.
*    WHEN '2'.
*      R_CTE_EMIT_AQUAV-SIGN   = 'I'.
*      R_CTE_EMIT_AQUAV-OPTION = 'EQ'.
*      R_CTE_EMIT_AQUAV-LOW    = 'X'.
*      APPEND R_CTE_EMIT_AQUAV.
*    WHEN '3'.
*  ENDCASE.
*
*  "Buscar informações para condições de Portochuelo.
*  SELECT SINGLE * FROM ZLEST0104 INTO WL_ZLEST0104 WHERE EMISSOR = W_WERKS.
*
*  "Classificação da Soja
*  CASE GW_SAIDA_COMBOIO-TP_CLASS(2).
*
*    WHEN: 'CO'.
*
*      CASE TP_ROMANE.
*        WHEN: 'PROPRIO'.
*
*          SELECT *
*          FROM ZSDT0001
*            INTO TABLE P_ZSDT0001
*          WHERE TP_MOVIMENTO  = 'E'
*            "Adequações Retenção CS2016001199
*            "AND branch        = wg_tela1200-dt_codigo
*            AND BUKRS         = WL_ZLEST0104-BUKRS
*            AND BRANCH        = WL_ZLEST0104-BRANCH
*            AND CTE_EMIT_AQUAV IN R_CTE_EMIT_AQUAV
*            "Fim
*            AND PARID         = VL_PARID
*            AND MATNR         = GW_SAIDA_COMBOIO-MATNR
*            AND TP_TRANSGENIA = 'CO'
*            AND DOC_REM       NE SPACE
*            AND SERIES        IN R_SERIES
*            AND LOCAL_DESCARGA = WL_ZLEST0104-LOCAL_DESCARGA.
*
*
*
*        WHEN: 'TERCEIRO'.
*
*          SELECT *
*          FROM ZSDT0001
*            INTO TABLE P_ZSDT0001
*          WHERE TP_MOVIMENTO  = 'E'
*            "Adequações Retenção CS2016001199
*            "AND branch        = wg_tela1200-dt_codigo
*            AND BUKRS         = WL_ZLEST0104-BUKRS
*            AND BRANCH        = WL_ZLEST0104-BRANCH
*            AND CTE_EMIT_AQUAV IN R_CTE_EMIT_AQUAV
*            "Fim
*            AND PARID         = VL_PARID
*            AND MATNR         = GW_SAIDA_COMBOIO-MATNR
*            AND TP_TRANSGENIA = 'CO'
*            AND DOC_REM       EQ SPACE
*            AND SERIES        IN R_SERIES
*            AND LOCAL_DESCARGA = WL_ZLEST0104-LOCAL_DESCARGA.
*
*
*      ENDCASE.
*
*    WHEN: 'R1'.
*
*      CASE TP_ROMANE.
*        WHEN: 'PROPRIO'.
*          SELECT *
*          FROM ZSDT0001
*            INTO TABLE P_ZSDT0001
*          WHERE TP_MOVIMENTO  = 'E'
*            "Adequações Retenção CS2016001199
*            "AND branch        = wg_tela1200-dt_codigo
*            AND BUKRS         = WL_ZLEST0104-BUKRS
*            AND BRANCH        = WL_ZLEST0104-BRANCH
*            AND CTE_EMIT_AQUAV IN R_CTE_EMIT_AQUAV
*            "Fim
*            AND PARID         = VL_PARID
*            AND MATNR         = GW_SAIDA_COMBOIO-MATNR
*            AND TP_TRANSGENIA  IN ('P1','T1','D1')
*            AND DOC_REM        NE SPACE
*            AND SERIES         IN R_SERIES
*            AND LOCAL_DESCARGA = WL_ZLEST0104-LOCAL_DESCARGA.
*
*
*
*        WHEN: 'TERCEIRO'.
*          SELECT *
*          FROM ZSDT0001
*            INTO TABLE P_ZSDT0001
*          WHERE TP_MOVIMENTO  = 'E'
*            "Adequações Retenção CS2016001199
*            "AND branch        = wg_tela1200-dt_codigo
*            AND BUKRS         = WL_ZLEST0104-BUKRS
*            AND BRANCH        = WL_ZLEST0104-BRANCH
*            AND CTE_EMIT_AQUAV IN R_CTE_EMIT_AQUAV
*            "Fim
*            AND PARID         = VL_PARID
*            AND MATNR         = GW_SAIDA_COMBOIO-MATNR
*            AND TP_TRANSGENIA  IN ('P1','T1','D1')
*            AND DOC_REM        EQ SPACE
*            AND SERIES         IN R_SERIES
*            AND LOCAL_DESCARGA = WL_ZLEST0104-LOCAL_DESCARGA.
*
*
*      ENDCASE.
*
*    WHEN: 'R2'.
*
*      CASE TP_ROMANE.
*
*        WHEN: 'PROPRIO'.
*          SELECT *
*          FROM ZSDT0001
*            INTO TABLE P_ZSDT0001
*           WHERE TP_MOVIMENTO  = 'E'
*             "Adequações Retenção CS2016001199
*             "AND branch        = wg_tela1200-dt_codigo
*             AND BUKRS         = WL_ZLEST0104-BUKRS
*             AND BRANCH        = WL_ZLEST0104-BRANCH
*             AND CTE_EMIT_AQUAV IN R_CTE_EMIT_AQUAV
*             "Fim
*             AND PARID         = VL_PARID
*             AND MATNR         = GW_SAIDA_COMBOIO-MATNR
*             AND TP_TRANSGENIA  IN ('P2','T2','D2')
*             AND DOC_REM        NE SPACE
*             AND SERIES         IN R_SERIES
*            AND LOCAL_DESCARGA = WL_ZLEST0104-LOCAL_DESCARGA.
*
*
*        WHEN: 'TERCEIRO'.
*
*          SELECT *
*          FROM ZSDT0001
*            INTO TABLE P_ZSDT0001
*           WHERE TP_MOVIMENTO  = 'E'
*             "Adequações Retenção CS2016001199
*             "AND branch        = wg_tela1200-dt_codigo
*             AND BUKRS         = WL_ZLEST0104-BUKRS
*             AND BRANCH        = WL_ZLEST0104-BRANCH
*             AND CTE_EMIT_AQUAV IN R_CTE_EMIT_AQUAV
*             "Fim
*             AND PARID         = VL_PARID
*             AND MATNR         = GW_SAIDA_COMBOIO-MATNR
*             AND TP_TRANSGENIA  IN ('P2','T2','D2')
*             AND DOC_REM        EQ SPACE
*             AND SERIES         IN R_SERIES
*            AND LOCAL_DESCARGA = WL_ZLEST0104-LOCAL_DESCARGA.
*
*
*      ENDCASE.
*  ENDCASE.

ENDFORM.

*FORM F_ADD_PESO_VINC  USING P_SAIDA_VINC_ROM_NOME TYPE TY_SAIDA_VINC_ROMANEIO
*                   CHANGING P_ERROR.
*
*  CLEAR: P_ERROR.
*
*  DATA: VL_VLR_VINC TYPE BRGEW.
*
*  CLEAR: VL_VLR_VINC.
*
*  "Adequações Retenção CS2016001199
*  IF ( P_SAIDA_VINC_ROM_NOME-PESO_LIQ_RET_VINC IS NOT INITIAL ). "17.01.2018 - CS2018000076
*    VL_VLR_VINC = P_SAIDA_VINC_ROM_NOME-PESO_LIQ_RET_VINC.
*  ELSE.
*    VL_VLR_VINC = P_SAIDA_VINC_ROM_NOME-PESO_VINC.
*  ENDIF.
*
*  ADD VL_VLR_VINC TO TOTAL_VINCULADO.
*
*  IF VL_VLR_VINC IS INITIAL.
*    P_ERROR = 'X'.
*    MESSAGE 'Nenhum peso encontrado para vinculação!' TYPE 'S'.
*    P_SAIDA_VINC_ROM_NOME-CHECK = ''.
*
*    WA_STABLE-ROW = 'X'.
*    WA_STABLE-COL = 'X'.
*    CALL METHOD OBJ_GRID_ROMANEIO->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WA_STABLE.
*
*    LEAVE TO SCREEN 1200.
*    EXIT.
*  ENDIF.
*
*  CLEAR: GW_SAIDA_COMBOIO.
*  READ TABLE GT_SAIDA_COMBOIO INTO GW_SAIDA_COMBOIO INDEX INDEX_COMBOIO.
*
*  IF ( GW_SAIDA_COMBOIO-PESO_VINCULADO EQ 0 ).
*    GW_SAIDA_COMBOIO-PESO_VINCULADO = TOTAL_VINCULADO.
*  ELSE.
*    GW_SAIDA_COMBOIO-PESO_VINCULADO = TOTAL_VINCULADO.
*  ENDIF.
*
*  MODIFY GT_SAIDA_COMBOIO FROM GW_SAIDA_COMBOIO INDEX INDEX_COMBOIO TRANSPORTING PESO_VINCULADO.
*
*
*ENDFORM.

FORM f_check_saldo_nfe  USING p_vinc_saida_rom TYPE zaqty_saida_vinc_romaneio.

  CHECK p_vinc_saida_rom-chave_nfe IS NOT INITIAL.

  SELECT SINGLE *
    FROM zlest0060 INTO @DATA(_wl_0060)
   WHERE chave_nfe = @p_vinc_saida_rom-chave_nfe.

  CHECK sy-subrc = 0.

  SELECT SINGLE *
    FROM zlest0073 INTO @DATA(_wl_0073)
   WHERE chave_nfe = @p_vinc_saida_rom-chave_nfe.

  IF sy-subrc NE 0.
    PERFORM f_atualiza_saldo_nf USING p_vinc_saida_rom.
  ENDIF.

ENDFORM.

FORM f_atrib_zterm_ov  USING p_zlest0055    TYPE zlest0055
                    CHANGING p_saida_ger_ov TYPE zaqty_saida_ger_ov.

  DATA: v_dia TYPE i.

  v_dia = p_saida_ger_ov-dt_fatura+06(2).

  IF ( v_dia >= 01 ) AND ( v_dia <= 15 ).
    p_saida_ger_ov-zterm = p_zlest0055-zterm.
  ELSEIF ( v_dia >= 16 ) AND ( v_dia <= 31 ).
    p_saida_ger_ov-zterm = p_zlest0055-zterm2.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_PROTOCOLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_protocolo .

  FREE it_zlest0166.

  SELECT *
    FROM zlest0166
    INTO TABLE it_zlest0166
    WHERE empresa EQ w_bukrs
      AND centro EQ w_werks
      AND ano EQ w_ano
      AND viagem EQ w_viagem.


  PERFORM alv_protocolo.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_PROTOCOLO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_protocolo .

  DATA: wl_layout   TYPE lvc_s_layo.


  CLEAR: wl_layout.

  REFRESH: gt_fcat_protocolo.
  CLEAR: gw_fcat_protocolo.

  PERFORM montar_catalog_protocolo USING:
  'EMPRESA'     'Empresa'    '05' '' '' '' '' '' '' '' '' '' ,
  'CENTRO'      'Centro'     '05' '' '' '' '' '' '' '' '' '' ,
  'ANO'         'Ano'        '05' '' '' '' '' '' '' '' '' '' ,
  'VIAGEM'      'Viagem'     '02' '' '' '' '' '' '' '' '' '' ,
  'NOME_EMB'    'Embarcação' '15' '' '' '' '' '' '' '' '' '' ,
  'RM_CODIGO'   'Remetente'  '07' '' '' '' '' '' '' '' '' '' ,
  'DT_CODIGO'   'Destino'    '07' '' '' '' '' '' '' '' '' '' ,
  'PROTOCOLO'   'Protocolo'  '20' '' '' '' '' '' '' '' '' '' .

  CHECK NOT gt_fcat_protocolo[] IS INITIAL.

  IF ( obj_custom_protocolo IS INITIAL ).

    CREATE OBJECT obj_custom_protocolo
      EXPORTING
        container_name              = 'CONTAINER_PROTOCOLO'
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


    CREATE OBJECT obj_grid_protocolo
      EXPORTING
        i_parent          = obj_custom_protocolo
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

    wl_layout-sel_mode   = 'A'.
    wl_layout-no_toolbar = 'X'.

    CALL METHOD obj_grid_protocolo->set_table_for_first_display
      EXPORTING
        is_layout                     = wl_layout
        i_save                        = 'U'
      CHANGING
        it_outtab                     = it_zlest0166
        it_fieldcatalog               = gt_fcat_protocolo[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD obj_grid_protocolo->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.

FORM montar_catalog_protocolo USING    VALUE(p_fieldname)
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

  APPEND VALUE #(
                  fieldname = p_fieldname
                  ref_table = p_ref_tabname
                  ref_field = p_ref_fieldname
                  tabname   = p_tabname
                  scrtext_l = p_desc
                  scrtext_m = p_desc
                  scrtext_s = p_desc
                  outputlen = p_tam
                  no_zero   = p_no_zero
                  hotspot   = p_hotspot
                  emphasize = p_cor
                  just      = p_just
                  do_sum    = p_sum
                  edit      = p_edit
                ) TO gt_fcat_protocolo.

ENDFORM.

FORM f_get_cfop_operacao.

  CLEAR: gt_cfops_rfl[], gt_cfops_rin[], gt_cfops_rtr[].

  "CFOP's de Remessa de Formação de Lote
  SELECT *
    FROM setleaf INTO TABLE gt_cfops_rfl
   WHERE setname EQ 'MAGGI_CFOP_RFL'.

  "CFOP's de Remessa para Industrialização
  SELECT *
    FROM setleaf INTO TABLE gt_cfops_rin
   WHERE setname EQ 'MAGGI_CFOP_RIN'.

  "CFOP's de Remessa Transferencia
  SELECT *
    FROM setleaf INTO TABLE gt_cfops_rtr
   WHERE setname EQ 'MAGGI_CFOP_RTR'.

  "CFOP's de Entrada Importação
  SELECT *
    FROM setleaf INTO TABLE gt_cfops_imp
   WHERE setname EQ 'MAGGI_CFOP_IMP'.

ENDFORM.

FORM f_set_operacao_nf  USING p_cfop
                     CHANGING c_operacao.

  CLEAR: c_operacao.

  CHECK p_cfop IS NOT INITIAL.

  READ TABLE gt_cfops_rfl WITH KEY valfrom = p_cfop.
  IF sy-subrc EQ 0.
    c_operacao = 'RF'.
    RETURN.
  ENDIF.

  READ TABLE gt_cfops_rin WITH KEY valfrom = p_cfop.
  IF sy-subrc EQ 0.
    c_operacao = 'RI'.
    RETURN.
  ENDIF.

  READ TABLE gt_cfops_rtr WITH KEY valfrom = p_cfop.
  IF sy-subrc EQ 0.
    c_operacao = 'RT'.
    RETURN.
  ENDIF.

  READ TABLE gt_cfops_imp WITH KEY valfrom = p_cfop.
  IF sy-subrc EQ 0.
    c_operacao = 'IM'.
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TROCAR_LOCAL_DESCARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trocar_local_descarga
  USING
        p_centro TYPE werks_d
        p_porto_embarque TYPE lifnr
  CHANGING id_local TYPE zde_local_descarga_opus
           ds_local TYPE zde_desc_loc_desc_opus
           p_subrc  TYPE sy-subrc.

  DATA: it_local TYPE  TABLE OF zde_psq_local_descarga,
        it_ret   TYPE TABLE OF ddshretval,
        it_fmap  TYPE STANDARD TABLE OF dselc.

  SELECT s~* INTO TABLE @DATA(it_zlest0104)
    FROM zlest0104 AS s
   WHERE emissor EQ @p_centro
     AND EXISTS ( SELECT * FROM zlest0206 AS t WHERE t~local_descarga EQ s~local_descarga AND t~po_embarque EQ @p_porto_embarque )
    ORDER BY local_descarga.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE i151 WITH p_centro p_porto_embarque DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE it_zlest0104 LINES DATA(lc_qtd_locais).

  IF lc_qtd_locais GT 1.
    CLEAR: it_local[].
    LOOP AT it_zlest0104 INTO DATA(wa_zlest0104).
      APPEND VALUE #( id_local = wa_zlest0104-local_descarga ds_local = wa_zlest0104-descricao ) TO it_local.
    ENDLOOP.
    SORT it_local BY id_local.

    CLEAR: it_fmap[].
    APPEND VALUE #( fldname = 'ID_LOCAL' dyfldname = 'Id.' ) TO it_fmap.
    APPEND VALUE #( fldname = 'DS_LOCAL' dyfldname = 'Desrição do Local de Descarga' ) TO it_fmap.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        ddic_structure  = 'ZDE_PSQ_LOCAL_DESCARGA'
        retfield        = 'ID_LOCAL'
        window_title    = 'Selecionar Local de Descarga do(s) Romaneio(s)'
        value_org       = 'S'
      TABLES
        value_tab       = it_local[]
        return_tab      = it_ret
        dynpfld_mapping = it_fmap
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc IS INITIAL AND it_ret[] IS NOT INITIAL.
      READ TABLE it_local INTO DATA(wa_local) WITH KEY id_local = it_ret[ 1 ]-fieldval BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        id_local = wa_local-id_local.
        ds_local = wa_local-ds_local.
        p_subrc  = 0.
      ELSE.
        p_subrc  = 1.
      ENDIF.
    ENDIF.

  ELSEIF lc_qtd_locais EQ 1.
    id_local = it_zlest0104[ 1 ]-local_descarga.
    ds_local = it_zlest0104[ 1 ]-descricao.
    p_subrc  = 0.
  ELSE.
    p_subrc  = 1.
  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_TABELAS_AQUAVIARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualizar_tabelas_aquaviario .

  CONSTANTS: pqtdpadrao TYPE i VALUE 100.

  DATA: p_total   TYPE i,
        p_posicao TYPE i,
        p_commit  TYPE i.

*  DELETE FROM ZLEST0061 WHERE ID_FRETE_AQUA NE @SPACE.
*  DELETE FROM ZLEST0060 WHERE ID_FRETE_AQUA NE @SPACE.
*  COMMIT WORK AND WAIT.
*
*  BREAK-POINT.
*
*  EXIT.

  SELECT SINGLE * INTO @DATA(wa_zlest0061)
    FROM zlest0061
   WHERE id_frete_aqua EQ @space.

  CHECK sy-subrc IS INITIAL.

  SELECT * INTO TABLE @DATA(it_zlest0061)
    FROM zlest0061
   WHERE id_frete_aqua EQ @space.

  CHECK sy-subrc IS INITIAL.

  DESCRIBE TABLE it_zlest0061 LINES p_total.

  CALL FUNCTION 'ZENQUEUE_CPROG'
    EXPORTING
      cprog          = sy-cprog
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.
    LEAVE PROGRAM.
  ENDIF.


  CALL FUNCTION 'Z_01_DRE_STATUS'
    EXPORTING
      texto     = 'Atualizando tabelas de Aquaviário'
      p_total   = p_total
      p_posicao = p_posicao.

  p_commit = pqtdpadrao.

  LOOP AT it_zlest0061 INTO wa_zlest0061.

    p_posicao = sy-tabix.

    TRY .

        zcl_fatura_frete_aquaviario=>zif_fatura_frete_aquaviario~get_instance(
          )->get_new_id_fatura_frete( IMPORTING e_id_fatura_frete = DATA(e_id_fatura_frete)
          ).

        IF wa_zlest0061-rm_codigo IS NOT INITIAL.
          SELECT * INTO TABLE @DATA(it_zlest0060)
            FROM zlest0060
           WHERE bukrs      EQ @wa_zlest0061-bukrs
             AND werks      EQ @wa_zlest0061-werks
             AND ano_viagem EQ @wa_zlest0061-ano_viagem
             AND nr_viagem  EQ @wa_zlest0061-nr_viagem
             AND embarcacao EQ @wa_zlest0061-embarcacao
             AND nome_emb   EQ @wa_zlest0061-nome_emb
             AND cl_codigo  EQ @wa_zlest0061-cl_codigo
             AND rm_codigo  EQ @wa_zlest0061-rm_codigo
             AND nr_dco     EQ @wa_zlest0061-nr_dco
             AND safra      EQ @wa_zlest0061-safra.
        ELSE.
          SELECT * INTO TABLE @it_zlest0060
            FROM zlest0060
           WHERE bukrs      EQ @wa_zlest0061-bukrs
             AND werks      EQ @wa_zlest0061-werks
             AND ano_viagem EQ @wa_zlest0061-ano_viagem
             AND nr_viagem  EQ @wa_zlest0061-nr_viagem
             AND embarcacao EQ @wa_zlest0061-embarcacao
             AND nome_emb   EQ @wa_zlest0061-nome_emb
             AND cl_codigo  EQ @wa_zlest0061-cl_codigo
             AND nr_dco     EQ @wa_zlest0061-nr_dco
             AND safra      EQ @wa_zlest0061-safra.
        ENDIF.

        IF sy-subrc IS INITIAL.
          LOOP AT it_zlest0060 INTO  DATA(wa_zlest0060).
            zcl_fatura_frete_aquaviario=>zif_fatura_frete_aquaviario~get_instance(
              )->get_new_id_fatura_frete_item( IMPORTING e_id_fatura_frete_item = DATA(e_id_fatura_frete_item)    " Id. Frete Aquaviário
              ).

            UPDATE zlest0060
               SET id_frete_aqua    = e_id_fatura_frete
                   id_frete_aqua_nf = e_id_fatura_frete_item
             WHERE bukrs       = wa_zlest0060-bukrs
               AND werks       = wa_zlest0060-werks
               AND nr_viagem   = wa_zlest0060-nr_viagem
               AND ano_viagem  = wa_zlest0060-ano_viagem
               AND embarcacao  = wa_zlest0060-embarcacao
               AND nome_emb    = wa_zlest0060-nome_emb
               AND rm_codigo   = wa_zlest0060-rm_codigo
               AND dt_codigo   = wa_zlest0060-dt_codigo
               AND nr_romaneio = wa_zlest0060-nr_romaneio
               AND safra       = wa_zlest0060-safra
               AND chave_nfe   = wa_zlest0060-chave_nfe .
          ENDLOOP.

        ELSE.
          CLEAR: it_zlest0060[].
        ENDIF.

        UPDATE zlest0061
           SET id_frete_aqua = e_id_fatura_frete
         WHERE bukrs        = wa_zlest0061-bukrs
           AND werks        = wa_zlest0061-werks
           AND ano_viagem   = wa_zlest0061-ano_viagem
           AND nr_viagem    = wa_zlest0061-nr_viagem
           AND embarcacao   = wa_zlest0061-embarcacao
           AND nome_emb     = wa_zlest0061-nome_emb
           AND tp_class     = wa_zlest0061-tp_class
           AND dt_movimento = wa_zlest0061-dt_movimento
           AND cl_codigo    = wa_zlest0061-cl_codigo
           AND rm_codigo    = wa_zlest0061-rm_codigo
           AND auart        = wa_zlest0061-auart
           AND nr_dco       = wa_zlest0061-nr_dco
           AND safra        = wa_zlest0061-safra
           AND operacao     = wa_zlest0061-operacao.

        CLEAR: it_zlest0060[].

      CATCH zcx_fatura_frete_aquaviario INTO DATA(ex_fatura_frete_aquaviario).
        ex_fatura_frete_aquaviario->zif_error~published_erro( EXPORTING i_msgty = 'I' i_msgty_display = 'E' ).
        EXIT.
    ENDTRY.

    p_commit = p_commit - 1.
    IF p_commit = 0.
      COMMIT WORK AND WAIT.
      p_commit = pqtdpadrao.
    ENDIF.

    CALL FUNCTION 'Z_01_DRE_STATUS'
      EXPORTING
        texto     = 'Atualizando tabelas de Aquaviário'
        p_total   = p_total
        p_posicao = p_posicao.

  ENDLOOP.

  COMMIT WORK AND WAIT.

  CALL FUNCTION 'ZDENQUEUE_CPROG'
    EXPORTING
      cprog = sy-cprog.


ENDFORM.

FORM processo_complemento
   "USING GW_SAIDA_COMBOIO TYPE ZAQTY_SAIDA_COMBOIO
         "WP_BUKRS TYPE T001-BUKRS
         "WP_WERKS TYPE T001W-WERKS
         "WP_ANO TYPE ZLEST0056-ANO_VIAGEM
         "WP_VIAGEM TYPE ZLEST0058-NR_VIAGEM
   " CHANGING OPCAO TYPE C
   "          WG_TELA1200 TYPE ZAQTY_TELA1200
   "          TOTAL_VINCULADO TYPE BRGEW
  .

  TYPES: BEGIN OF ty_vbfa,
           vbelv        TYPE vbfa-vbelv,
           vbtyp_n      TYPE vbfa-vbtyp_n,
           vbeln        TYPE vbfa-vbeln,
           mjahr        TYPE vbfa-mjahr,
           vbeln_refkey TYPE j_1bnflin-refkey,
         END OF ty_vbfa.

  DATA: vl_parid               TYPE zsdt0001-parid,
        tp_notas               TYPE c LENGTH 8,
        tp_romane              TYPE c LENGTH 8,
        vl_kunnr               TYPE c LENGTH 10,
        it_zlest0205           TYPE zde_zlest0205_t,
        vg_remetente           TYPE lfa1-lifnr,
        vg_destinatario        TYPE kna1-kunnr,
        wp_frete_id            TYPE zlest0061-id_frete_aqua,
        it_vbfa                TYPE TABLE OF ty_vbfa,
        it_zlest0060_nf_vinc   TYPE TABLE OF zlest0060,
        it_forn_vinc_fis       TYPE TABLE OF zaqty_forn_vinc_fiscal,
        wa_zlest0152           TYPE zlest0152,
        gw_saida_vinc_rom_nome TYPE zaqty_saida_vinc_romaneio,
        lc_kunnr_branch        TYPE kna1-kunnr,
        lc_tomador             TYPE c LENGTH 10.

  DATA: zcl_util          TYPE REF TO zcl_util,             "*-US 159346-25.11.2024-JT-inicio
        t_documentos      TYPE j_1b_tt_nfe_active,          "*-US 159346-25.11.2024-JT-inicio
        it_j_1bnflin      TYPE TABLE OF j_1bnflin,          "*-US 159346-25.11.2024-JT-inicio
        it_j_1bnfe_active TYPE TABLE OF j_1bnfe_active.     "*-US 159346-25.11.2024-JT-inicio

  CREATE OBJECT zcl_util.  "*-US 159346-25.11.2024-JT-inicio

  IF ( opcao EQ 'I' ).

    "Verificar se o remetente é terceiro ou proprio.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_tela1200-rm_codigo
      IMPORTING
        output = vl_parid.

    SELECT SINGLE * FROM lfa1 INTO @DATA(wa_lfa1_nf) WHERE lifnr EQ @vl_parid.

    "ZFIC = INTERCOMPNAY (PROPRIO).
    "OUTROS = TERCEIRO
    CASE wa_lfa1_nf-ktokk.
      WHEN: 'ZFIC'.
        tp_notas  = 'PROPRIO'.
        tp_romane = 'PROPRIO'.
      WHEN OTHERS.
        tp_notas  = 'TERCEIRO'.
        tp_romane = 'TERCEIRO'.
    ENDCASE.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_tela1200-dt_codigo
      IMPORTING
        output = vl_kunnr.


    SELECT SINGLE * FROM kna1 INTO @DATA(wl_kna1) WHERE kunnr EQ @vl_kunnr.
    IF ( sy-subrc NE 0 ).
      MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Destinatário não encontrado.'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'ZLES_AQUA_NF_COMPLEMENTO'
      EXPORTING
        i_tp_romaneio    = tp_romane
        i_series         = wg_tela1200-series
        i_nr_safra       = wg_tela1200-nr_safra
        i_cte_emit_aquav = '1'
        i_centro_emissor = wp_werks
        i_matnr          = gw_saida_comboio-matnr
        i_tp_class       = gw_saida_comboio-tp_class(2)
        i_parid          = vl_parid
        i_local_descarga = wg_tela1200-id_local_descarga
        i_eudr           = gw_saida_comboio-eudr(1) "Ajustes ZLES0077 - Frete Aquaviario EUDR - BG #153328
      CHANGING
        it_zlest0205     = it_zlest0205.

    IF it_zlest0205[] IS INITIAL.
*      "Gravar Controle de Remetente e Destinatario.
*      VG_REMETENTE    = VL_PARID.
*      VG_DESTINATARIO = WG_TELA1200-DT_CODIGO.
*      OPCAO = 'D'.
*      PERFORM: CONTROLE_REMET_DESTIN IN PROGRAM ZLESR0073 USING VG_REMETENTE VG_DESTINATARIO OPCAO.
*
*      CLEAR: WP_FRETE_ID, WG_TELA1200-ID_FRETE_AQUA.
*      MESSAGE S000(ZWRM001) DISPLAY LIKE 'E' WITH 'Nenhum Complemento encontrado!'.
      EXIT.
    ENDIF.

    DATA(wg_tela1200_id_local_descarga) = wg_tela1200-id_local_descarga.
    DATA(wg_tela1200_ds_local_descarga) = wg_tela1200-ds_local_descarga.

    CLEAR: wp_frete_id, wg_tela1200-id_frete_aqua.", WG_TELA1200-ID_LOCAL_DESCARGA, WG_TELA1200-DS_LOCAL_DESCARGA.

    SORT it_zlest0205 BY chave_nfe.

    CASE tp_romane.

      WHEN: 'PROPRIO'.

*-US 159346-25.11.2024-JT-inicio
        DATA(t_zlest0205_prop) = it_zlest0205[].
        DELETE t_zlest0205_prop WHERE propria_emissao = abap_off.

        LOOP AT t_zlest0205_prop INTO DATA(w_zlest0205_prop).
          t_documentos = zcl_util->get_docnum( w_zlest0205_prop-chave_nfe ).

          DELETE t_documentos WHERE form = abap_off.

          READ TABLE t_documentos INTO DATA(w_documentos) INDEX 1.
          IF sy-subrc = 0.
            SELECT *
              FROM j_1bnflin
         APPENDING TABLE it_j_1bnflin
             WHERE docnum = w_documentos-docnum.

            SELECT *
              FROM j_1bnfe_active
         APPENDING TABLE it_j_1bnfe_active
             WHERE docnum  = w_documentos-docnum
               AND cancel <> abap_true.
          ENDIF.
        ENDLOOP.

        DATA(t_zlest0205_naoprop) = it_zlest0205[].
        DELETE t_zlest0205_naoprop WHERE doc_rem = abap_off.
*-US 159346-25.11.2024-JT-fim

        IF t_zlest0205_naoprop[] IS NOT INITIAL.               "*-US 159346-25.11.2024-JT
          SELECT vbelv, vbtyp_n, vbeln, mjahr
             FROM vbfa
             INTO CORRESPONDING FIELDS OF TABLE @it_vbfa
             FOR ALL ENTRIES IN @t_zlest0205_naoprop   "*-US 159346-25.11.2024-JT
           WHERE vbelv   EQ @t_zlest0205_naoprop-doc_rem      "*-US 159346-25.11.2024-JT
             AND vbtyp_n IN ( 'M', 'R' ).
        ENDIF.

*-US 159346-25.11.2024-JT-inicio
*       CHECK NOT it_vbfa[] IS INITIAL.
        IF it_vbfa[] IS NOT INITIAL.
          LOOP AT it_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>).
            <fs_vbfa>-vbelv                     = <fs_vbfa>-vbelv.
            <fs_vbfa>-vbtyp_n                   = <fs_vbfa>-vbtyp_n.
            <fs_vbfa>-vbeln                     = <fs_vbfa>-vbeln.
            IF <fs_vbfa>-vbtyp_n = 'R'.
              CONCATENATE  <fs_vbfa>-vbeln <fs_vbfa>-mjahr INTO <fs_vbfa>-vbeln_refkey.
            ELSE.
              <fs_vbfa>-vbeln_refkey              = <fs_vbfa>-vbeln.
            ENDIF.
          ENDLOOP.

          UNASSIGN <fs_vbfa>.

          SELECT * FROM j_1bnflin
            APPENDING TABLE @it_j_1bnflin     "*-US 159346-25.11.2024-JT-inicio
            FOR ALL ENTRIES IN @it_vbfa
          WHERE refkey EQ @it_vbfa-vbeln_refkey.

          SELECT * FROM j_1bnfe_active
            APPENDING TABLE @it_j_1bnfe_active  "*-US 159346-25.11.2024-JT-inicio
            FOR ALL ENTRIES IN @it_j_1bnflin
          WHERE docnum EQ @it_j_1bnflin-docnum
            AND cancel NE 'X'.
        ENDIF.
*-US 159346-25.11.2024-JT-fim

        "Buscar Informações do DCO
        CASE tp_notas.
          WHEN: 'PROPRIO'.
*-US 159346-25.11.2024-JT-inicio
            t_zlest0205_naoprop = it_zlest0205[].
            DELETE t_zlest0205_naoprop WHERE doc_rem = abap_off.
*           CHECK NOT it_zlest0205[] IS INITIAL.
*-US 159346-25.11.2024-JT-fim

*-US 159346-25.11.2024-JT-inicio
            "Recupera o DCO das Notas.
            IF t_zlest0205_naoprop[] IS NOT INITIAL.
              SELECT * FROM zdco_vinculo
                INTO TABLE @DATA(tl_zdco_vinculo)
                FOR ALL ENTRIES IN @t_zlest0205_naoprop  "@it_zlest0205
              WHERE vbeln EQ @t_zlest0205_naoprop-doc_rem.
            ENDIF.
*-US 159346-25.11.2024-JT-fim
        ENDCASE.

      WHEN: 'TERCEIRO'.


    ENDCASE.

    "Buscar Notas Vinculadas.
    REFRESH: it_zlest0060_nf_vinc[].

    SELECT * FROM zlest0060 AS a
      INTO TABLE @it_zlest0060_nf_vinc
      FOR ALL ENTRIES IN @it_zlest0205
    WHERE chave_nfe      EQ @it_zlest0205-chave_nfe
      AND bukrs          EQ @wp_bukrs
      AND werks          EQ @wp_werks
      AND nr_viagem      EQ @wp_viagem
      AND ano_viagem     EQ @wp_ano
      AND nome_emb       EQ @gw_saida_comboio-nome
      AND local_descarga EQ @it_zlest0205-local_descarga
      AND NOT EXISTS ( SELECT * FROM zlest0061 AS b
                        WHERE b~docnum EQ a~docnum
                          AND b~ck_anulado EQ @abap_true ).

    "Verifica fornecedores com vinculação fiscal.
    DATA(it_zlest0205_aux) = it_zlest0205[].
    SORT it_zlest0205_aux BY bukrs branch parid.
    DELETE ADJACENT DUPLICATES FROM it_zlest0205_aux COMPARING bukrs branch parid.
    CLEAR: it_forn_vinc_fis[].
    DATA(_vinc_fiscal) = ''.
    LOOP AT it_zlest0205_aux INTO DATA(wa_zlest0205).
      CALL FUNCTION 'ZLES_PARAM_FORN_AQUA'
        EXPORTING
          i_bukrs_rom  = wa_zlest0205-bukrs
          i_branch_rom = wa_zlest0205-branch
          i_lifnr      = wa_zlest0205-parid
        IMPORTING
          e_zlest0152  = wa_zlest0152.

      IF ( wa_zlest0152 IS NOT INITIAL ) AND ( wa_zlest0152-vinc_fiscal IS NOT INITIAL ).
        APPEND VALUE #( bukrs = wa_zlest0205-bukrs
                        branch  = wa_zlest0205-branch
                        parid   = wa_zlest0205-parid ) TO it_forn_vinc_fis.
      ENDIF.
    ENDLOOP.
    SORT it_forn_vinc_fis BY bukrs branch parid.
    "Fim

    LOOP AT it_zlest0205 INTO wa_zlest0205.

      CLEAR: gw_saida_vinc_rom_nome.
      READ TABLE it_forn_vinc_fis WITH KEY bukrs  = wa_zlest0205-bukrs
                                           branch = wa_zlest0205-branch
                                           parid  = wa_zlest0205-parid
      BINARY SEARCH TRANSPORTING NO FIELDS.

      IF sy-subrc IS INITIAL.
        gw_saida_vinc_rom_nome-vinc_fiscal = 'X'.
      ENDIF.

      CASE tp_romane.

        WHEN: 'PROPRIO'.

*-US 159346-25.11.2024-JT-inicio
          IF wa_zlest0205-propria_emissao = abap_true.
            t_documentos = zcl_util->get_docnum( wa_zlest0205-chave_nfe ).

            DELETE t_documentos WHERE form = abap_off.

            READ TABLE t_documentos INTO w_documentos INDEX 1.
            IF sy-subrc = 0.
              READ TABLE it_j_1bnflin INTO DATA(wa_j_1bnflin) WITH KEY docnum = w_documentos-docnum.
              IF sy-subrc = 0.
                READ TABLE it_j_1bnfe_active INTO DATA(wa_j_1bnfe_active) WITH KEY docnum = w_documentos-docnum.
              ENDIF.
            ENDIF.
          ELSE.
*-US 159346-25.11.2024-JT-fim
            LOOP AT it_vbfa INTO DATA(wa_vbfa) WHERE vbelv = wa_zlest0205-doc_rem.

              READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY refkey = wa_vbfa-vbeln_refkey. "*-US 159346-25.11.2024-JT-inicio
              IF sy-subrc IS NOT INITIAL.
                CONTINUE.
              ENDIF.

              READ TABLE it_j_1bnfe_active INTO wa_j_1bnfe_active WITH KEY docnum = wa_j_1bnflin-docnum. "*-US 159346-25.11.2024-JT-inicio
              DATA(lc_chave) = wa_j_1bnfe_active-regio &&
                               wa_j_1bnfe_active-nfyear &&
                               wa_j_1bnfe_active-nfmonth &&
                               wa_j_1bnfe_active-stcd1 &&
                               wa_j_1bnfe_active-model &&
                               wa_j_1bnfe_active-serie &&
                               wa_j_1bnfe_active-nfnum9 &&
                               wa_j_1bnfe_active-docnum9 &&
                               wa_j_1bnfe_active-cdv.
              IF ( sy-subrc EQ 0 ) AND ( lc_chave = wa_zlest0205-chave_nfe ).
                CONTINUE.
              ENDIF.

            ENDLOOP.
          ENDIF.  "*-US 159346-25.11.2024-JT

          IF ( sy-subrc EQ 0 ).

            gw_saida_vinc_rom_nome-embarcacao    = gw_saida_comboio-embarcacao+0(1).
            gw_saida_vinc_rom_nome-nome          = gw_saida_comboio-nome.
            gw_saida_vinc_rom_nome-chave_nfe     = wa_zlest0205-chave_nfe.
            gw_saida_vinc_rom_nome-safra         = wa_zlest0205-nr_safra.
            gw_saida_vinc_rom_nome-dt_movimento  = wa_zlest0205-docdat.
            gw_saida_vinc_rom_nome-docdat        = wa_zlest0205-docdat.
            gw_saida_vinc_rom_nome-nfnum         = wa_zlest0205-nfnum.
            gw_saida_vinc_rom_nome-ch_referencia = wa_zlest0205-ch_referencia. " "*-BUG 154845-10.10.2024-JT-#154845-inicio
            gw_saida_vinc_rom_nome-check_complemento = abap_true.

            IF wa_zlest0205-cfop IS INITIAL.
              CONTINUE.
            ELSE.
              PERFORM f_set_operacao_nf IN PROGRAM zlesr0073 USING wa_zlest0205-cfop CHANGING gw_saida_vinc_rom_nome-operacao.

              IF gw_saida_vinc_rom_nome-operacao IS INITIAL.
                CONTINUE.
              ENDIF.
            ENDIF.

            IF gw_saida_vinc_rom_nome-operacao NE wg_tela1200-operacao.
              CONTINUE.
            ENDIF.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_zlest0205-series
              IMPORTING
                output = gw_saida_vinc_rom_nome-series.

            gw_saida_vinc_rom_nome-rm_codigo     = wg_tela1200-rm_codigo.
            gw_saida_vinc_rom_nome-dt_codigo     = wg_tela1200-dt_codigo.
            gw_saida_vinc_rom_nome-id_frete_aqua = wg_tela1200-id_frete_aqua.

            SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(wa_j_1bnfdoc) WHERE docnum EQ @wa_j_1bnflin-docnum.

            CHECK ( sy-subrc EQ 0 ).

            IF wa_j_1bnfdoc-partyp EQ 'B'.
              lc_kunnr_branch = wa_j_1bnfdoc-parid.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = lc_kunnr_branch
                IMPORTING
                  output = lc_kunnr_branch.

              lc_kunnr_branch = lc_kunnr_branch+6(4).

              wa_j_1bnfdoc-parid = lc_kunnr_branch.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = wa_j_1bnfdoc-parid
                IMPORTING
                  output = wa_j_1bnfdoc-parid.

              wa_j_1bnfdoc-partyp = 'C'.
            ENDIF.

            CHECK ( vl_kunnr EQ wa_j_1bnfdoc-parid ).

            "GW_SAIDA_VINC_ROM_NOME-SAFRA         = WA_ZLEST0205-NR_SAFRA.
            gw_saida_vinc_rom_nome-doc_rem       = wa_zlest0205-doc_rem.

            "Adequações Retenção CS2016001199
            IF ( wa_zlest0205-netwr        > 0 ) AND
               ( wa_zlest0205-peso_fiscal  > 0 ).
              gw_saida_vinc_rom_nome-netpr = wa_zlest0205-netwr / wa_zlest0205-peso_fiscal.
            ENDIF.

            "GW_SAIDA_VINC_ROM_NOME-CH_REFERENCIA   = WL_ZSDT0001-CH_REFERENCIA.
            gw_saida_vinc_rom_nome-bukrs_rom       = wa_zlest0205-bukrs.
            gw_saida_vinc_rom_nome-branch_rom      = wa_zlest0205-branch.
            gw_saida_vinc_rom_nome-parid_rom       = wa_zlest0205-parid.
            gw_saida_vinc_rom_nome-matnr           = wa_zlest0205-matnr.
            gw_saida_vinc_rom_nome-tp_transgenia   = wa_zlest0205-tp_transgenia.
            gw_saida_vinc_rom_nome-local_descarga  = wa_zlest0205-local_descarga.
            "Fim

            "Concatenar a chave da nota fiscal
            gw_saida_vinc_rom_nome-chave_nfe       = wa_zlest0205-chave_nfe.
            gw_saida_vinc_rom_nome-docnum_rem      = wa_j_1bnfe_active-docnum.

            PERFORM f_check_saldo_nfe IN PROGRAM zlesr0073 USING gw_saida_vinc_rom_nome .

            IF NOT ( wa_zlest0205-doc_rem IS INITIAL ).
              gw_saida_vinc_rom_nome-xml = icon_led_green.
            ELSE.

              SELECT SINGLE *
                INTO @DATA(wa_zib_nfe_dist_ter)
                FROM zib_nfe_dist_ter
               WHERE forne_cnpj EQ @wa_lfa1_nf-stcd1
                 AND numero     EQ @wa_zlest0205-nfnum
                 AND serie      EQ @wa_zlest0205-series.

              IF ( sy-subrc EQ 0 ).
                gw_saida_vinc_rom_nome-xml = icon_led_green.
              ELSE.
                gw_saida_vinc_rom_nome-xml = icon_led_red.
              ENDIF.
            ENDIF.


            SELECT SINGLE * FROM zlest0073 INTO @DATA(wa_zlest0073) WHERE chave_nfe EQ @gw_saida_vinc_rom_nome-chave_nfe.
            IF ( sy-subrc NE 0 ).
              gw_saida_vinc_rom_nome-peso_fiscal   = wa_zlest0205-peso_fiscal.
              gw_saida_vinc_rom_nome-netwr         = wa_zlest0205-netwr.

              "Adequações Retenção CS2016001199
              gw_saida_vinc_rom_nome-peso_subtotal = wa_zlest0205-peso_subtotal.

              "IF ( VG_CALC_RETENCAO IS NOT INITIAL ). "17.01.2018 - CS2018000076
              CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
                EXPORTING
                  i_bukrs         = wa_zlest0205-bukrs
                  i_branch        = wa_zlest0205-branch
                  i_lifnr         = wa_zlest0205-parid
                  i_peso          = wa_zlest0205-peso_subtotal
                  i_peso_fiscal   = wa_zlest0205-peso_fiscal
                IMPORTING
                  e_perc_retencao = gw_saida_vinc_rom_nome-perc_ret
                  e_peso_retido   = gw_saida_vinc_rom_nome-peso_retido
                  e_peso_liquido  = gw_saida_vinc_rom_nome-peso_liq_ret.

            ENDIF.

            READ TABLE it_zlest0060_nf_vinc INTO DATA(wa_zlest0060_nf_vinc) WITH KEY chave_nfe = wa_zlest0205-chave_nfe.
            IF sy-subrc IS INITIAL.
              "Verificar se CT-e das NF's já foi autorizado
              SELECT SINGLE docnum
                FROM j_1bnfe_active INTO @DATA(vl_docnum)
               WHERE docnum     EQ @wa_zlest0060_nf_vinc-docnum
                 AND cancel     EQ @space
                 AND docsta     EQ '1'.
              IF sy-subrc = 0.
                CONTINUE.
              ENDIF.
            ENDIF.

            LOOP AT it_zlest0060_nf_vinc INTO wa_zlest0060_nf_vinc WHERE chave_nfe   EQ wa_zlest0205-chave_nfe
                                                                     AND bukrs       EQ wp_bukrs
                                                                     AND werks       EQ wp_werks
                                                                     AND nr_viagem   EQ wp_viagem
                                                                     AND ano_viagem  EQ wp_ano
                                                                     AND nome_emb    EQ gw_saida_comboio-nome
                                                                     "AND SAFRA       EQ WL_ZSDT0001-NR_SAFRA
                                                                     "AND NR_DCO      EQ WL_ZDCO_VINCULO-NR_DCO
                                                                     .

              IF wa_zlest0060_nf_vinc-peso_liq_ret IS NOT INITIAL.
                total_vinculado = total_vinculado + wa_zlest0060_nf_vinc-peso_liq_ret.
              ELSE.
                total_vinculado = total_vinculado + wa_zlest0060_nf_vinc-peso_fiscal.
              ENDIF.
            ENDLOOP.

            APPEND gw_saida_vinc_rom_nome TO gt_saida_vinc_rom_nome.
            CLEAR: gw_saida_vinc_rom_nome, wa_zlest0060_nf_vinc, wa_zib_nfe_dist_ter.  "WL_ZLEST0065.
            "Fim

          ELSE.
            CLEAR: wa_vbfa, wa_j_1bnflin, wa_j_1bnfe_active, wa_zlest0205, wa_zlest0073.
            CONTINUE.
          ENDIF.

        WHEN: 'TERCEIRO'.

          "CLEAR: WL_ZLEST0065.

          READ TABLE it_zlest0060_nf_vinc INTO wa_zlest0060_nf_vinc
          WITH KEY chave_nfe = wa_zlest0205-chave_nfe.

          IF sy-subrc IS INITIAL.
            "Verificar se CT-e das NF's já foi autorizado
            SELECT SINGLE docnum FROM j_1bnfe_active INTO vl_docnum
             WHERE docnum     = wa_zlest0060_nf_vinc-docnum
               AND cancel     = ''
               AND docsta     = '1'.
            IF sy-subrc IS INITIAL.
              CONTINUE.
            ENDIF.
          ENDIF.

          LOOP AT it_zlest0060_nf_vinc INTO wa_zlest0060_nf_vinc
            WHERE chave_nfe   EQ wa_zlest0205-chave_nfe
             AND bukrs        EQ wp_bukrs
             AND werks        EQ wp_werks
             AND nr_viagem    EQ wp_viagem
             AND ano_viagem   EQ wp_ano
             AND nome_emb     EQ gw_saida_comboio-nome
             "AND SAFRA        EQ WL_ZSDT0001-NR_SAFRA
             "AND NR_DCO       EQ WL_ZDCO_VINCULO-NR_DCO
            .

            IF wa_zlest0060_nf_vinc-peso_liq_ret IS NOT INITIAL.
              total_vinculado = total_vinculado + wa_zlest0060_nf_vinc-peso_liq_ret.
            ELSE.
              total_vinculado = total_vinculado + wa_zlest0060_nf_vinc-peso_fiscal.
            ENDIF.
          ENDLOOP.

          gw_saida_vinc_rom_nome-embarcacao    = gw_saida_comboio-embarcacao+0(1).
          gw_saida_vinc_rom_nome-nome          = gw_saida_comboio-nome.
          gw_saida_vinc_rom_nome-chave_nfe     = wa_zlest0205-chave_nfe.
          gw_saida_vinc_rom_nome-safra         = wa_zlest0205-nr_safra.
          gw_saida_vinc_rom_nome-dt_movimento  = wa_zlest0205-docdat.
          gw_saida_vinc_rom_nome-docdat        = wa_zlest0205-docdat.
          gw_saida_vinc_rom_nome-nfnum         = wa_zlest0205-nfnum.
          gw_saida_vinc_rom_nome-ch_referencia = wa_zlest0205-ch_referencia. " "*-BUG 154845-10.10.2024-JT-#154845-inicio
          gw_saida_vinc_rom_nome-check_complemento = abap_true.


          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_zlest0205-series
            IMPORTING
              output = gw_saida_vinc_rom_nome-series.


          gw_saida_vinc_rom_nome-rm_codigo     = wg_tela1200-rm_codigo.
          gw_saida_vinc_rom_nome-dt_codigo     = wg_tela1200-dt_codigo.
          gw_saida_vinc_rom_nome-doc_rem       = wa_zlest0205-doc_rem.

          IF ( wa_zlest0205-netwr        > 0 ) AND
             ( wa_zlest0205-peso_fiscal  > 0 ).
            gw_saida_vinc_rom_nome-netpr = wa_zlest0205-netwr / wa_zlest0205-peso_fiscal.
          ENDIF.

          gw_saida_vinc_rom_nome-bukrs_rom       = wa_zlest0205-bukrs.
          gw_saida_vinc_rom_nome-branch_rom      = wa_zlest0205-branch.
          gw_saida_vinc_rom_nome-parid_rom       = wa_zlest0205-parid.
          gw_saida_vinc_rom_nome-matnr           = wa_zlest0205-matnr.
          gw_saida_vinc_rom_nome-tp_transgenia   = wa_zlest0205-tp_transgenia.
          gw_saida_vinc_rom_nome-local_descarga  = wa_zlest0205-local_descarga.

          IF wa_zlest0205-nfe = ' '.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wg_tela1200-rm_codigo
              IMPORTING
                output = vl_parid.

            SELECT SINGLE * FROM lfa1 INTO @DATA(wa_lfa1) WHERE lifnr EQ @vl_parid.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wg_tela1200-dt_codigo
              IMPORTING
                output = vl_parid.

            SELECT SINGLE * FROM kna1 INTO @DATA(wa_kna1_aux) WHERE kunnr EQ @vl_parid.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wa_zlest0205-series
              IMPORTING
                output = wa_zlest0205-series.

            gw_saida_vinc_rom_nome-chave_nfe = wa_zlest0205-chave_nfe.

            gw_saida_vinc_rom_nome-xml = icon_led_green.

            IF wa_zlest0205-cfop IS NOT INITIAL.
              PERFORM f_set_operacao_nf IN PROGRAM zlesr0073
                USING wa_zlest0205-cfop
              CHANGING gw_saida_vinc_rom_nome-operacao.
              IF gw_saida_vinc_rom_nome-operacao IS INITIAL.
                CONTINUE.
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.

          ELSE.

            SELECT SINGLE *
              INTO @wa_zib_nfe_dist_ter
              FROM zib_nfe_dist_ter
             WHERE chave_nfe EQ @wa_zlest0205-chave_nfe.

            IF sy-subrc IS INITIAL.
              SELECT SINGLE *
                INTO @DATA(wa_zib_nfe_dist_itm)
                FROM zib_nfe_dist_itm
               WHERE chave_nfe EQ @wa_zib_nfe_dist_ter-chave_nfe.
            ENDIF.

            IF sy-subrc IS INITIAL.

              IF wa_zib_nfe_dist_itm-prod_cfop IS INITIAL.
                CONTINUE.
              ELSE.
                PERFORM f_set_operacao_nf IN PROGRAM zlesr0073
                  USING wa_zib_nfe_dist_itm-prod_cfop(4)
               CHANGING gw_saida_vinc_rom_nome-operacao.

                IF gw_saida_vinc_rom_nome-operacao IS INITIAL.
                  CONTINUE.
                ENDIF.
              ENDIF.

              IF gw_saida_vinc_rom_nome-operacao NE wg_tela1200-operacao.
                CONTINUE.
              ENDIF.

              gw_saida_vinc_rom_nome-xml = icon_led_green.

              IF wa_zib_nfe_dist_itm-prod_cfop NE '3949'.
                lc_tomador = |{ wa_zib_nfe_dist_ter-f_tomadora ALPHA = IN }|.

                IF lc_tomador IS INITIAL.

                  SELECT SINGLE * FROM kna1 INTO @wa_kna1_aux
                   WHERE stcd1 EQ @wa_zib_nfe_dist_ter-destino_cnpj.

                  IF ( sy-subrc IS NOT INITIAL ) OR ( vl_kunnr NE wa_kna1_aux-kunnr ).
                    CONTINUE.
                  ENDIF.

                ELSE.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = lc_tomador
                    IMPORTING
                      output = lc_tomador.

                  SELECT SINGLE * FROM kna1 INTO @wa_kna1_aux WHERE kunnr EQ @lc_tomador.
                  IF sy-subrc IS NOT INITIAL.
                    CONTINUE.
                  ENDIF.
                ENDIF.
              ENDIF.

            ELSE.
              gw_saida_vinc_rom_nome-xml = icon_led_red.

            ENDIF.
          ENDIF.

          PERFORM f_check_saldo_nfe  IN PROGRAM zlesr0073 USING gw_saida_vinc_rom_nome.

          SELECT SINGLE *
            FROM zlest0073 INTO @wa_zlest0073
           WHERE chave_nfe EQ @gw_saida_vinc_rom_nome-chave_nfe.

          IF ( sy-subrc NE 0 ).

            gw_saida_vinc_rom_nome-peso_fiscal   = wa_zlest0205-peso_fiscal.
            gw_saida_vinc_rom_nome-netwr         = wa_zlest0205-netwr.
            gw_saida_vinc_rom_nome-peso_subtotal = wa_zlest0205-peso_subtotal.

            CALL FUNCTION 'Z_LES_RETENCAO_AQUA'
              EXPORTING
                i_bukrs         = wa_zlest0205-bukrs
                i_branch        = wa_zlest0205-branch
                i_lifnr         = wa_zlest0205-parid
                i_peso          = wa_zlest0205-peso_subtotal
                i_peso_fiscal   = wa_zlest0205-peso_fiscal
              IMPORTING
                e_perc_retencao = gw_saida_vinc_rom_nome-perc_ret
                e_peso_retido   = gw_saida_vinc_rom_nome-peso_retido
                e_peso_liquido  = gw_saida_vinc_rom_nome-peso_liq_ret.

          ENDIF.

          APPEND gw_saida_vinc_rom_nome TO gt_saida_vinc_rom_nome.
          CLEAR: gw_saida_vinc_rom_nome, wa_zlest0060_nf_vinc, wa_zib_nfe_dist_ter. "WL_ZLEST0065.

      ENDCASE.

      CLEAR: wa_vbfa, wa_j_1bnflin, wa_j_1bnfe_active, wa_zlest0205, wa_zlest0073, gw_saida_vinc_rom_nome.
    ENDLOOP.

  ENDIF.

ENDFORM.
