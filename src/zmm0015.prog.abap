*&---------------------------------------------------------------------*
*& Report  ZMM0015
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmm0015.
*----------------------------------------------------------------------*
* TYPES                                                                *
*----------------------------------------------------------------------*
TABLES: sscrfields.

TYPES: BEGIN OF type_msn,
*         NR_ROMANEIO TYPE ZNR_ROMANEIO,
         tp_msn   TYPE bapi_mtype,
         messagem TYPE bapi_msg,
       END   OF type_msn.

*----------------------------------------------------------------------*
*   data definition
*----------------------------------------------------------------------*
*       Batchinputdata of single transaction
DATA:   bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
*       messages of call transaction
DATA: messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
      t_msn   TYPE TABLE OF type_msn.

*       error session opened (' ' or 'X')
DATA:   e_group_opened.
*       message texts
TABLES: t100, rmmg1.

DATA: BEGIN OF dynpfields OCCURS 0. "Hilfsstruktur zum auslesen des akt.
        INCLUDE STRUCTURE dynpread. "Feldwertes vom Dynpro bei >F4<
DATA: END OF   dynpfields.

TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
         mtart TYPE mara-mtart,
         mbrsh TYPE mara-mbrsh,
         iprkz TYPE mara-iprkz,
         spart TYPE mara-spart,
         tragr TYPE mara-tragr,
         xchpf TYPE mara-xchpf,
       END OF ty_mara,

       BEGIN OF ty_marc,
         matnr TYPE marc-matnr,
         werks TYPE marc-werks,
         steuc TYPE marc-steuc,
         indus TYPE marc-indus,
         kautb TYPE marc-kautb,
         dismm TYPE marc-dismm,
         ladgr TYPE marc-ladgr,
         mfrgr TYPE marc-mfrgr,
         mtvfp TYPE marc-mtvfp,
         sauft TYPE marc-sauft,
         sfepr TYPE marc-sfepr,
       END OF ty_marc,

       BEGIN OF ty_mard,
         matnr TYPE mard-matnr,
         werks TYPE mard-werks,
         lgort TYPE mard-lgort,
       END OF ty_mard,


       BEGIN OF ty_mbew,
         matnr TYPE mbew-matnr,
         bwkey TYPE mbew-bwkey,
         bwtar TYPE mbew-bwtar,
         bklas TYPE mbew-bklas,
         mtuse TYPE mbew-mtuse,
         mtorg TYPE mbew-mtorg,
         ownpr TYPE mbew-ownpr,
         vprsv TYPE mbew-vprsv,
         stprs TYPE mbew-stprs,
       END OF ty_mbew,

       BEGIN OF ty_t001l,
         werks TYPE t001l-werks,
         lgort TYPE t001l-lgort,
         lgobe TYPE t001l-lgobe,
       END OF ty_t001l,

       BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
       END OF ty_t001w,

       BEGIN OF ty_t023t,
         spras TYPE t023t-spras,
         matkl TYPE t023t-matkl,
         wgbez TYPE t023t-wgbez,
       END OF ty_t023t,

       BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         spras TYPE makt-spras,
         maktx TYPE makt-maktx,
       END OF ty_makt,

       BEGIN OF ty_saida,
         matkl   TYPE mara-matkl,
         mbrsh   TYPE mara-mbrsh,
         iprkz   TYPE mara-iprkz,
         matnr   TYPE marc-matnr,
         werks   TYPE marc-werks,
         maktx   TYPE makt-maktx,
         lgort   TYPE mard-lgort,
         name1   TYPE t001w-name1,
         lgobe   TYPE t001l-lgobe,
         wgbez   TYPE t023t-wgbez,
         bklas   TYPE mbew-bklas,
         steuc   TYPE marc-steuc,
         indus   TYPE marc-indus,
         kautb   TYPE marc-kautb,
         mtuse   TYPE mbew-mtuse,
         mtorg   TYPE mbew-mtorg,
         mark(1) TYPE c,
         mtart   TYPE mara-mtart,
         spart   TYPE mara-spart, "Aqui
         tragr   TYPE mara-tragr,
         xchpf   TYPE mara-xchpf,
         dismm   TYPE marc-dismm,
         ladgr   TYPE marc-ladgr,
         mfrgr   TYPE marc-mfrgr,
         mtvfp   TYPE marc-mtvfp,
         sauft   TYPE marc-sauft,
         sfepr   TYPE marc-sfepr,
         ownpr   TYPE mbew-ownpr,
         mlast   TYPE ckmlhd-mlast,
         vprsv   TYPE mbew-vprsv,
         stprs   TYPE mbew-stprs,
         ktgrm   TYPE mvke-ktgrm,
         mtpos   TYPE mvke-mtpos,
       END OF ty_saida,

*
*       BEGIN OF TY_MRP,
*         CENTRO        TYPE RMMG1-WERKS,
*         MATERIAL      TYPE RMMG1-MATNR,
*         DISMM         TYPE MARC-DISMM, "Tipo de MRP MRP1
*         BSTMI(15), "      TYPE MARC-BSTMI, "Tamanho mínimo do lote MRP1
*         PLIFZ(3),  "      TYPE MARC-PLIFZ, "Prazo de entrega previsto em dias MRP2
*         SHZET(2),  "     TYPE MARC-SHZET, "Período de segurança (em dias de trabalho)  MRP2
*         MTVFP         TYPE MARC-MTVFP, "verificação para verificação de disponibilidade MRP3
*         WEBAZ(3),
*         PLANEJADOR    TYPE MARC-DISPO, "MRP1
*         LGORT         TYPE LGORT_D,
*         LGPBE         TYPE MARD-LGPBE,
*         CENTRO_A      TYPE RMMG1-WERKS,
*         MESSAGE       TYPE BAPI_MSG,
*         SEQ           TYPE I,
*         CHARG         TYPE CHARG_D,
*         DT_VENCIMENTO TYPE SY-DATUM,
*       END OF TY_MRP,

       BEGIN OF ty_mrp,
         centro        TYPE rmmg1-werks,
         material      TYPE rmmg1-matnr,
         dismm         TYPE marc-dismm, "Tipo de MRP
         minbe(15),                     "Ponto de reabastecimento
         dispo         TYPE marc-dispo, "Planejador MRP
         disls         TYPE marc-disls, "Chave de cálculo do tamanho do lote no MRP
         mabst(15),                     "Estoque máximo
         bstmi(15),                     "Tamanho mínimo do lote
         webaz(3),                      "Tempo de processamento (em dias) da entrada de mercadorias
         plifz(3),  "                   "Prazo de entrega previsto em dias MRP2
         mtvfp         TYPE marc-mtvfp, "verificação para verificação de disponibilidade MRP3
         "
         lgort         TYPE lgort_d,
         lgpbe         TYPE mard-lgpbe,
         centro_a      TYPE rmmg1-werks,
         message       TYPE bapi_msg,
         seq           TYPE i,
         charg         TYPE charg_d,
         dt_vencimento TYPE sy-datum,
       END OF ty_mrp,

*-US 164011-04-02-2025-#164011-RJF-Inicio
*Declaração.
       BEGIN OF ty_mrp2,
         mark(1)      TYPE c,
         status       TYPE icon-id,
         material     TYPE mara-matnr,           "Código material
         mbrsh        TYPE mara-mbrsh,           "Setor industrial
         mtart        TYPE mara-mtart,           "Tipo de material
         matkl        TYPE mara-matkl,           "Grupo de mercadorias
         spart        TYPE mara-spart,           "Setor de atividade
         gewei        TYPE mara-gewei,           "Unidade de peso
         meins        TYPE mara-meins,           "Unidade de medida básica
         maktx        TYPE makt-maktx,           "Texto breve de material
         werks        TYPE marc-werks,           "Centro
         mtvfp        TYPE marc-mtvfp,           "Grupo de verificação para verificação de disponibilidade
         steuc        TYPE marc-steuc,           "Código de controle p/imposto seletivo em comércio exterior
         kautb        TYPE marc-kautb,           "Dados de centro para material
         bklas        TYPE mbew-bklas,           "Classe de avaliação
         mlast        TYPE mbew-mlast,           "Apropriação custos do ledger de materiais: controle
         peinh        TYPE mbew-peinh,           "Unidade preço
         mtuse        TYPE mbew-mtuse,           "Utilização de material
         mtorg        TYPE mbew-mtorg,           "Origem de material
         vkorg        TYPE tvko-vkorg,           "Organização de vendas
         vtweg        TYPE mvke-vtweg,           "Canal de distribuição
*         sktof        TYPE mvke-sktof,           "Dados de venda para material
         mtpos        TYPE mvke-mtpos,           "Grupo de categorias de item do mestre de materiais
         message(255) TYPE c,
         check        TYPE c,
       END OF ty_mrp2,

*Declaração.
       BEGIN OF ty_arq,
         mbrsh TYPE mara-mbrsh,           "Setor industrial
         mtart TYPE mara-mtart,           "Tipo de material
         matkl TYPE mara-matkl,           "Grupo de mercadorias
         spart TYPE mara-spart,           "Setor de atividade
         gewei TYPE mara-gewei,           "Unidade de peso
         meins TYPE mara-meins,           "Unidade de medida básica
         maktx TYPE makt-maktx,           "Texto breve de material
         werks TYPE marc-werks,           "Centro
         mtvfp TYPE marc-mtvfp,           "Grupo de verificação para verificação de disponibilidade
         steuc TYPE marc-steuc,           "Código de controle p/imposto seletivo em comércio exterior
         kautb TYPE marc-kautb,           "Dados de centro para material
         bklas TYPE mbew-bklas,           "Classe de avaliação
         mlast TYPE mbew-mlast,           "Apropriação custos do ledger de materiais: controle
         peinh TYPE mbew-peinh,           "Unidade preço
         mtuse TYPE mbew-mtuse,           "Utilização de material
         mtorg TYPE mbew-mtorg,           "Origem de material
         vkorg TYPE mvke-vkorg,           "Organização de vendas
         vtweg TYPE mvke-vtweg,           "Canal de distribuição
       END OF ty_arq,
*-US 164011-04-02-2025-#164011-RJF-Fim

       BEGIN OF ty_erro,
         msg(100),
       END OF ty_erro,

*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Início de Alteração
       BEGIN OF ty_zmmt0184,
         matnr TYPE zmmt0184-matnr,
         werks TYPE zmmt0184-werks,
       END OF ty_zmmt0184,

       BEGIN OF ty_mara_mtart,
         matnr TYPE mara-matnr,
         mtart TYPE mara-mtart,
       END OF ty_mara_mtart.
*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Fim de Alteração


CONSTANTS: c_x TYPE c LENGTH 1 VALUE 'X'.

DATA: BEGIN OF t_06 OCCURS 0.
        INCLUDE STRUCTURE zmmt0007.
DATA: END OF t_06.


DATA: kzall            LIKE sy-marky,
      gob_gui_alv_grid TYPE REF TO cl_gui_alv_grid,
      kzbew_werk       LIKE sy-marky,
      hlgort           LIKE mard-lgort,
      hwerk            LIKE t001w-werks.

*&---------------------------------------------------------------------*
*& Importação do arquivo EXCEL
*&---------------------------------------------------------------------*
DATA: t_excel  LIKE alsmex_tabline OCCURS 18 WITH HEADER LINE,
      t_excel2 LIKE alsmex_tabline OCCURS 18 WITH HEADER LINE.


DATA:
  wa_mrp             TYPE ty_mrp,
  t_mrp              TYPE TABLE OF ty_mrp,
  t_mrp_aux          TYPE TABLE OF ty_mrp,
  wa_mrp2            TYPE ty_mrp2,
  gt_mat_description TYPE TABLE OF bapi_makt,
  gt_mat_longtext    TYPE TABLE OF bapi_mltx,
  gs_mat_description TYPE bapi_makt,
  gt_unitsofmeasure  TYPE TABLE OF bapi_marm,
  gt_unitsofmeasurex TYPE TABLE OF bapi_marmx,
  gs_unitsofmeasure  TYPE bapi_marm,
  gs_unitsofmeasurex TYPE bapi_marmx,
  t_mrp2             TYPE TABLE OF ty_mrp2,
  t_mrp_aux2         TYPE TABLE OF ty_mrp2,
  gv_umren           TYPE umren,
  gv_umrez           TYPE umrez,
  wa_erro            TYPE ty_erro.

DATA: el_headdata             LIKE bapimathead,
      el_clientdata           LIKE bapi_mara,
      el_clientdatax          LIKE bapi_marax,
      el_plantdata            LIKE bapi_marc,
      el_plantdatax           LIKE bapi_marcx,
      el_valdata              LIKE bapi_mbew,
      el_valdatax             LIKE bapi_mbewx,
      el_salesdata            LIKE bapi_mvke,
      el_salesdatax           LIKE bapi_mvkex,
      el_storagelocationdata  LIKE bapi_mard,
      el_storagelocationdatax LIKE bapi_mardx,
      gs_mat_longtext         TYPE bapi_mltx,
      gt_ret_messages         TYPE TABLE OF bapi_matreturn2,
      e_return                LIKE bapiret2.
DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.
DATA: wl_mode(1),
      wg_documento(10),
* ---> S4 Migration - 19/06/2023 - MA
*      vmsg(50),
      vmsg(70),
* <--- S4 Migration - 19/06/2023 - MA
      wl_erro(1),
      wl_seq TYPE i.
.
*&---------------------------------------------------------------------*
*& Definições para ALV
*&---------------------------------------------------------------------*

TYPE-POOLS: slis,
            kkblo.

DATA: repid           LIKE sy-repid.
DATA: fieldcat        TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA: layout          TYPE slis_layout_alv.
DATA: print           TYPE slis_print_alv.
DATA: sort        TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      events      TYPE slis_t_event,
      xs_events   TYPE slis_alv_event,
      gt_exclude  TYPE           slis_t_extab,
      gt_exclude1 TYPE           ui_functions.
DATA: w_tit(70).


*&---------------------------------------------------------------------*
*& Declaração de Tabelas Internas
*&---------------------------------------------------------------------*

DATA: t_mara               TYPE TABLE OF ty_mara,
      t_marc               TYPE TABLE OF ty_marc,
      t_marc2              TYPE TABLE OF ty_marc,
      t_mard               TYPE TABLE OF ty_mard,
      t_mbew               TYPE TABLE OF ty_mbew,
      t_ckmlhd             TYPE TABLE OF ckmlhd,
      t_t001l              TYPE TABLE OF ty_t001l,
      t_t001w              TYPE TABLE OF ty_t001w,
      t_t023t              TYPE TABLE OF ty_t023t,
      t_makt               TYPE TABLE OF ty_makt,
      w_mvke               TYPE mvke,
      t_saida              TYPE TABLE OF ty_saida,
*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Início de Alteração
      t_zmmt0184_processar TYPE TABLE OF zmmt0184,
      t_zmmt0184_mod       TYPE TABLE OF zmmt0184,
      t_zmmt0184_aux       TYPE TABLE OF zmmt0184,
      t_zmmt0184_pendente  TYPE TABLE OF ty_zmmt0184,
      t_mara_mtart         TYPE TABLE OF ty_mara_mtart.
*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Fim de Alteração

*&---------------------------------------------------------------------*
*& Declaração de WORK ÁREA
*&---------------------------------------------------------------------*

DATA: wa_mara       TYPE  ty_mara,
      wa_marc       TYPE  ty_marc,
      wa_marc2      TYPE  ty_marc,
      wa_mard       TYPE  ty_mard,
      wa_mard2      TYPE  ty_mard,
      wa_mbew       TYPE  ty_mbew,
      wa_ckmlhd     TYPE  ckmlhd,
      wa_t001l      TYPE  ty_t001l,
      wa_t001w      TYPE  ty_t001w,
      wa_t023t      TYPE  ty_t023t,
      wa_makt       TYPE  ty_makt,
      wa_saida      TYPE  ty_saida,
      vr_check_erro TYPE char01,
*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Início de Alteração
      wa_zmmt0184   TYPE zmmt0184.
*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Fim de Alteração

*-US 164011-03-02-2025-#164011-RJF-Inicio
DATA:
  l_sel_button                TYPE smp_dyntxt,
  l_sel_button_param          TYPE smp_dyntxt,
  "Objetos
  gob_custom_container        TYPE REF TO cl_gui_custom_container,
  gob_dd_document             TYPE REF TO cl_dd_document,
  gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
  gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,

  gob_gui_container_topo      TYPE REF TO cl_gui_container,
  gob_gui_container_filtro    TYPE REF TO cl_gui_container,
  gob_gui_container_logo      TYPE REF TO cl_gui_container,
  gob_gui_container_grid      TYPE REF TO cl_gui_container,
  gob_gui_picture             TYPE REF TO cl_gui_picture,
  git_fcat                    TYPE lvc_t_fcat,
*      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid,
  lines                       TYPE sy-tabix,
  wa_selected_rows            TYPE lvc_s_row,
  it_selected_rows            TYPE lvc_t_row,
  t_file                      TYPE TABLE OF ty_arq. "zpp_modelo_plan.

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



    CALL METHOD gob_gui_alv_grid->refresh_table_display.

  ENDMETHOD.

  METHOD get_ucomm.

    CASE sy-ucomm.
      WHEN 'PROC'.

        CALL METHOD gob_gui_alv_grid->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

*        IF ( lines IS INITIAL ).
*          MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.
*
*        ELSE.
*          LOOP AT it_selected_rows INTO wa_selected_rows.
*            READ TABLE git_zpm0058 INTO DATA(wa_saida) INDEX wa_selected_rows-index.
*            APPEND wa_saida TO it_zpm0058.
*            CLEAR: wa_saida.
*          ENDLOOP.
*        ENDIF.
*
*        IF it_zpm0058 IS NOT INITIAL.
*          FREE: it_zpm0058.
*          it_zpm0058 = git_zpm0058[].
*          PERFORM fm_processar.
*        ENDIF.


      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


*-US 164011-03-02-2025-#164011-RJF-Fim
*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN SKIP.
*  SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-007.
  SELECTION-SCREEN: BEGIN OF LINE.
    PARAMETERS: p_expan  RADIOBUTTON GROUP gr1 DEFAULT 'X' USER-COMMAND muda_tela." MODIF ID prc USER-COMMAND muda_tela .
    SELECTION-SCREEN COMMENT 5(20) TEXT-005." MODIF ID prc.   "Expandir Material
    PARAMETERS: p_matba RADIOBUTTON GROUP gr1." MODIF ID prc.
    SELECTION-SCREEN COMMENT 30(25) TEXT-006. "MODIF ID prc.   "Cadastrar Material Básico
*  SELECTION-SCREEN END OF BLOCK b5.
  SELECTION-SCREEN: END OF LINE.
  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: p_matnr TYPE rmmg1-matnr MODIF ID prc.
    SELECT-OPTIONS: s_matkl  FOR wa_mara-matkl NO INTERVALS NO-EXTENSION NO-DISPLAY MODIF ID prc,
                    s_werks  FOR wa_marc-werks NO INTERVALS NO-EXTENSION MODIF ID prc.
    PARAMETERS: p_vkorg LIKE mvke-vkorg MODIF ID prc,
                p_vtweg LIKE mvke-vtweg MODIF ID prc.
  SELECTION-SCREEN END OF BLOCK b2.
  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
    PARAMETERS: p_centro LIKE rmmg1-werks MODIF ID prc,
                p_lgort  LIKE rmmg1-lgort MODIF ID prc,
                p_vkorgd LIKE mvke-vkorg MODIF ID prc,
                p_vtwegd LIKE mvke-vtweg MODIF ID prc.
  SELECTION-SCREEN END OF BLOCK b3.

  SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
    PARAMETER: p_file   TYPE rlgrap-filename DEFAULT '',
               r_mrp    LIKE bsid-umskz AS CHECKBOX DEFAULT ' ' USER-COMMAND chk MODIF ID prc,
               r_dep    LIKE bsid-umskz AS CHECKBOX DEFAULT ' ' USER-COMMAND chk MODIF ID prc,
               r_lote   LIKE bsid-umskz AS CHECKBOX DEFAULT ' ' USER-COMMAND chk MODIF ID prc,
               r_centro LIKE bsid-umskz AS CHECKBOX DEFAULT ' ' USER-COMMAND chk MODIF ID prc. "RJF - 150191
  SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN END OF BLOCK b1.

*-US 164011-03-02-2025-#164011-RJF-Inicio
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2. "MM Ajuste expansão materiais #169622 / RG

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'FC01'.
      PERFORM gera_modelo_planillha.
    WHEN 'FC02'."MM Ajuste expansão materiais #169622 / RG
      PERFORM busca_parametros.

  ENDCASE.
*-US 164011-03-02-2025-#164011-RJF-Fim
*&---------------------------------------------------------------------*
*& START-OF-SELECTION
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_centro.
  PERFORM busca_centro_material.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LGORT.
*  PERFORM BUSCA_DEPOSITO_MATERIAL.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = 'C:\'
      mask             = ',*.xls*,'
      mode             = 'O'
      title            = 'Busca de Arquivo'
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

AT SELECTION-SCREEN OUTPUT.
*-US 164011-03-02-2025-#164011-RJF-Inicio
  LOOP AT SCREEN.
    IF screen-name CS 'P_CENTRO' OR screen-name CS 'P_MATNR' OR screen-name CS 'S_MATKL'
      OR screen-name CS 'S_WERKS' OR screen-name CS 'P_VKORG' OR screen-name CS 'P_VTWEG'
      OR screen-name CS 'P_LGORT' OR screen-name CS 'P_VKORGD' OR screen-name CS 'P_VTWEGD'.
      IF p_matba IS NOT INITIAL.
        screen-input = 0.
        MODIFY SCREEN.
      ELSE.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF screen-group1 = 'PRC'.
      IF p_expan IS NOT INITIAL.
        screen-invisible = 0.
        MODIFY SCREEN.
      ELSEIF p_matba IS NOT INITIAL.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
*-US 164011-03-02-2025-#164011-RJF-Fim

  CASE abap_true.
    WHEN r_mrp.
      CLEAR: r_dep, r_lote.
    WHEN r_dep.
      CLEAR: r_mrp, r_lote.
    WHEN r_lote.
      CLEAR: r_mrp, r_dep.
  ENDCASE.

  "IF  R_MRP = ''.
  IF  r_dep = 'X'.
    READ TABLE s_werks[] INTO s_werks INDEX 1.
    IF s_werks-low  IS INITIAL.
      MESSAGE 'Informe o Centro Atual' TYPE 'I'.
      SET CURSOR FIELD 'S_WERKS-LOW' .
    ENDIF.
    IF p_centro  IS INITIAL.
      MESSAGE 'Informe o Centro' TYPE 'I'.
      SET CURSOR FIELD 'P_CENTRO' .
    ENDIF.
*    IF p_vkorg IS NOT INITIAL AND p_vkorgd IS INITIAL.
*      MESSAGE 'Informe a Org. Vendas Destino' TYPE 'I'.
*      SET CURSOR FIELD 'P_VKORGD' .
*    ENDIF.
*    IF p_vtweg IS NOT INITIAL AND p_vtwegd IS INITIAL.
*      MESSAGE 'Informe o canal de Distribuição Destino' TYPE 'I'.
*      SET CURSOR FIELD 'P_VTWEGD' .
*    ENDIF.
  ENDIF.

  IF p_matnr IS NOT INITIAL AND s_werks IS NOT INITIAL.
    "Valida tipo de material.
    PERFORM valida_tp_material.
  ENDIF.

INITIALIZATION.

  l_sel_button-icon_id   = '@8H@'. ""icon_dangerous_goods. "
  l_sel_button-icon_text = 'Download Planilha Modelo Carga'.
  sscrfields-functxt_01  = l_sel_button.

  " MM Ajuste expansão materiais #169622 / RG
  DATA: lt_param  TYPE ustyp_t_parameters.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = lt_param
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.

  IF line_exists( lt_param[ parid = 'ZMM0015_PAR_GERAL' ] ).
    IF sy-subrc EQ 0.
      l_sel_button-icon_id   = '@BQ@'.
      l_sel_button-icon_text = 'Parâmetros Gerais'.
      l_sel_button-quickinfo = 'teste'.
      sscrfields-functxt_02 = l_sel_button.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  PERFORM valida_tp_material.


*-US 164011-03-02-2025-#164011-RJF-Inicio
  CASE abap_true.
    WHEN p_expan.
*-US 164011-03-02-2025-#164011-RJF-Fim
      IF vr_check_erro IS INITIAL.
        IF r_lote IS INITIAL.
          PERFORM valida_usuario.
        ENDIF.

        IF r_mrp = 'X'.
          PERFORM processa_arquivo.
        ELSEIF r_dep = 'X'.
          PERFORM processa_arquivo.
          PERFORM alv_erro.
        ELSEIF r_lote = 'X'.
          PERFORM processa_arquivo.
          PERFORM alv_erro.
*-US 150191-30-01-2025-#150191-RJF-Inicio
        ELSEIF r_centro = 'X'.
          PERFORM processa_arquivo.
          PERFORM seleciona_dados.
          PERFORM organiza_dados.
          PERFORM processar_multi.
*      PERFORM alv_erro.
*-US 150191-30-01-2025-#150191-RJF-Fim
        ELSEIF s_werks-low IS NOT INITIAL AND p_centro IS NOT INITIAL.
          PERFORM seleciona_dados.
          PERFORM organiza_dados.
          PERFORM chama_alv.
        ENDIF.
      ENDIF.
*-US 164011-03-02-2025-#164011-RJF-Inicio
    WHEN p_matba.
      PERFORM carregamento_arquivo.
      PERFORM chama_alv_cmb.
*-US 164011-03-02-2025-#164011-RJF-Fim
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados .
  DATA: w1(55).

  IF NOT p_matnr IS INITIAL.
    SELECT matnr matkl mtart mbrsh iprkz spart tragr xchpf
      FROM mara
      INTO CORRESPONDING FIELDS OF TABLE t_mara
     WHERE matnr EQ p_matnr
    AND matkl IN s_matkl.
  ELSE.
    SELECT matnr matkl mtart mbrsh iprkz spart tragr xchpf
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE t_mara
    WHERE matkl IN s_matkl.
  ENDIF.

  IF sy-subrc IS INITIAL.
    SELECT * FROM zmmt0007 INTO TABLE t_06.
    LOOP AT t_mara INTO wa_mara.
      READ TABLE t_06 WITH KEY mtart = wa_mara-mtart.
      IF sy-subrc <> 0.
*        Tipo de Material ZSSO não permitido nesta transação. Iniciar expansão via workflow de cadastro de materiais pela transação ZWFMAT004.
        CONCATENATE   'Tipo Material' wa_mara-mtart 'não permitido nesta transação' INTO w1 SEPARATED BY space.
        CONCATENATE  w1 '.' INTO w1.
        MESSAGE i000(z01) WITH  w1
                               'Iniciar expansão via workflow de cadastro'
                               ' de materiais pela transação ZWFMAT004'.
        STOP.
      ENDIF.
*** BUG 63479 - Inicio - CSB
*      IF wa_mara-mtart = 'ZDIE'.
*        IF p_vkorg IS INITIAL OR p_vtweg IS INITIAL.
*          MESSAGE i000(z01) WITH
*                          'Informe Org. Vendas e Canal distribuição'
*                          ' para pretação de serviços'.
*          STOP.
*        ENDIF.
*      ENDIF.
*** BUG 63479 - Fim - CSB
*      IF ( p_vtweg IS NOT  INITIAL ) AND        "p_vkorg IS NOT INITIAL AND
*         ( p_vtwegd IS INITIAL ).
*        MESSAGE i000(z01) WITH
*                        'Informe Org. Vendas e Canal distribuição DESTINO'
*                        ' para pretação de serviços'.
*        STOP.
*      ENDIF.
    ENDLOOP.


    SELECT matnr spras maktx
      FROM makt
      INTO TABLE t_makt
      FOR ALL ENTRIES IN t_mara
      WHERE matnr EQ t_mara-matnr
    AND   spras EQ sy-langu.

    SELECT  matnr werks steuc indus kautb dismm ladgr mfrgr mtvfp sauft sfepr
      FROM marc
      INTO TABLE t_marc
      FOR ALL ENTRIES IN t_mara
      WHERE matnr EQ t_mara-matnr
    AND   werks IN s_werks.

    IF sy-subrc NE 0.
      MESSAGE i000(z01) WITH
                         'Material não expandido'
                         ' para este centro'.
      STOP.
    ENDIF.
    SELECT *
      FROM ckmlhd
      INTO TABLE t_ckmlhd
      FOR ALL ENTRIES IN t_mara
      WHERE matnr EQ t_mara-matnr
    AND   bwkey IN s_werks.

    SELECT spras matkl wgbez
      FROM t023t
      INTO TABLE t_t023t
      FOR ALL ENTRIES IN t_mara
      WHERE matkl EQ t_mara-matkl
    AND   spras EQ sy-langu.

    IF NOT t_marc[] IS INITIAL.
      SELECT matnr bwkey bwtar bklas mtuse mtorg ownpr vprsv stprs
        FROM mbew
        INTO TABLE t_mbew
        FOR ALL ENTRIES IN t_marc
        WHERE matnr EQ t_marc-matnr
      AND   bwkey EQ t_marc-werks.

      SELECT werks name1
        FROM t001w
        INTO TABLE t_t001w
        FOR ALL ENTRIES IN t_marc
      WHERE werks EQ t_marc-werks.

      IF p_vkorg IS NOT INITIAL.
        SELECT SINGLE *
          FROM mvke
          INTO w_mvke
          WHERE matnr  = p_matnr
          AND   vkorg  = p_vkorg
        AND   vtweg  = p_vtweg.
        IF sy-subrc NE 0.
          MESSAGE i000(z01) WITH
                          'Org.Vendas/Canal distribuição'
                          'não existe neste centro '.
          STOP.
        ENDIF.
      ENDIF.

      IF p_lgort IS NOT INITIAL.
        SELECT matnr werks lgort
         FROM mard
         INTO TABLE t_mard
         FOR ALL ENTRIES IN t_marc
         WHERE matnr EQ t_marc-matnr
         AND   werks EQ t_marc-werks
        AND   lgort EQ p_lgort.

        IF sy-subrc IS INITIAL.

          SELECT werks lgort lgobe
            FROM t001l
            INTO TABLE t_t001l
            FOR ALL ENTRIES IN t_mard
            WHERE werks EQ t_mard-werks
          AND   lgort EQ t_mard-lgort.
*        ELSE.
*          MESSAGE I000(Z01) WITH
*                            'Depósito não existe na origem'
*                            ' '.
*          STOP.
        ENDIF.
      ENDIF.

    ENDIF.
  ELSE.
    MESSAGE 'Material não encontrado!' TYPE 'I'.
    STOP.
  ENDIF.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM organiza_dados .

  SORT: t_marc  BY matnr werks,
        t_marc2 BY matnr werks,
        t_mbew  BY matnr bwkey,
        t_ckmlhd  BY matnr bwkey,
        t_makt  BY matnr,
        t_t023t BY matkl spras,
        t_t001w BY werks,
        t_mard  BY matnr werks lgort,
        t_t001l BY werks lgort.

  LOOP AT t_mara INTO wa_mara.

    READ TABLE t_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl
                                              spras = sy-langu
                                            BINARY SEARCH.

    READ TABLE t_makt INTO wa_makt WITH KEY matnr = wa_mara-matnr
                                           BINARY SEARCH.

    READ TABLE t_marc INTO wa_marc WITH KEY matnr = wa_mara-matnr
                                            BINARY SEARCH.
    LOOP AT t_marc INTO wa_marc WHERE matnr = wa_mara-matnr.


      READ TABLE t_mbew INTO wa_mbew WITH KEY matnr = wa_marc-matnr
                                              bwkey = wa_marc-werks
                                              BINARY SEARCH.

      READ TABLE t_ckmlhd  INTO wa_ckmlhd  WITH KEY matnr = wa_marc-matnr
                                                    bwkey = wa_marc-werks
                                                    BINARY SEARCH.
      READ TABLE t_t001w INTO wa_t001w WITH KEY werks = wa_marc-werks
                                                BINARY SEARCH.


      CLEAR wa_mard.
      IF p_lgort IS NOT INITIAL.
        READ TABLE t_mard INTO wa_mard  WITH KEY matnr = wa_marc-matnr
                                                 werks = wa_marc-werks BINARY SEARCH.

      ENDIF.
      wa_saida-matkl    = wa_mara-matkl.
      wa_saida-mbrsh    = wa_mara-mbrsh.
      wa_saida-iprkz    = wa_mara-iprkz.
      wa_saida-wgbez    = wa_t023t-wgbez.
      wa_saida-maktx    = wa_makt-maktx.
      wa_saida-matnr    = wa_marc-matnr.
      wa_saida-steuc    = wa_marc-steuc.
      wa_saida-indus    = wa_marc-indus.
      wa_saida-werks    = wa_marc-werks.
      wa_saida-kautb    = wa_marc-kautb.
      wa_saida-name1    = wa_t001w-name1.
      wa_saida-lgort    = wa_mard-lgort.
      wa_saida-bklas    = wa_mbew-bklas.
      wa_saida-mtorg    = wa_mbew-mtorg.
      wa_saida-mtuse    = wa_mbew-mtuse.
      wa_saida-lgobe    = wa_t001l-lgobe.
      wa_saida-mtart    = wa_mara-mtart.
      "
      wa_saida-spart    = wa_mara-spart. "Aqui
      wa_saida-tragr    = wa_mara-tragr.
      wa_saida-xchpf    = wa_mara-xchpf.
      wa_saida-dismm    = wa_marc-dismm.
      wa_saida-ladgr    = wa_marc-ladgr.
      wa_saida-mfrgr    = wa_marc-mfrgr.
      wa_saida-mtvfp    = wa_marc-mtvfp.
      wa_saida-sauft    = wa_marc-sauft.
      wa_saida-sfepr    = wa_marc-sfepr.
      wa_saida-ownpr    = wa_mbew-ownpr.
      wa_saida-stprs    = wa_mbew-stprs.
      wa_saida-vprsv    = wa_mbew-vprsv.
      wa_saida-mlast    = wa_ckmlhd-mlast.

      IF p_vkorg IS NOT INITIAL.
        SELECT SINGLE *
          FROM mvke
          INTO w_mvke
          WHERE matnr  = p_matnr
          AND   vkorg  = p_vkorg
        AND   vtweg  = p_vtweg.
        wa_saida-ktgrm = w_mvke-ktgrm.
        wa_saida-mtpos = w_mvke-mtpos.
      ENDIF.

      APPEND wa_saida TO t_saida.
      CLEAR wa_saida.

    ENDLOOP. "

  ENDLOOP.
ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CHAMA_ALV
*&---------------------------------------------------------------------*
FORM chama_alv .

  REFRESH: fieldcat.

  PERFORM monta_fieldcat USING:
      'MATNR'  'T_SAIDA' 'MARC' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'MAKTX'  'T_SAIDA' 'MAKT' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'WERKS'  'T_SAIDA' 'MARC' ' ' ' ' ' ' ' ' ' ' '' ' ',
      'NAME1'  'T_SAIDA' 'T001W ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'LGORT'  'T_SAIDA' 'MARD  ' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'LGOBE'  'T_SAIDA' 'T001L' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'MATKL'  'T_SAIDA' 'MARA' ' ' ' ' ' ' ' ' ' ' ' ' ' ',
      'WGBEZ'  'T_SAIDA' 'T023T' ' ' ' ' ' ' ' ' ' ' ' ' ' '.

  w_tit ='Expansão de Materiais'.

  layout-box_fieldname     = 'MARK'.
  layout-box_tabname       = 'T_SAIDA'.
  layout-zebra = 'X'.
  print-no_print_listinfos = 'X'.

  repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = repid
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'COMANDO'
      it_fieldcat              = fieldcat[]
*     it_sort                  = sort[]
      is_layout                = layout
      i_grid_title             = w_tit
*     i_default                = 'X'
*     i_save                   = 'A'
*     it_events                = events
      is_print                 = print
    TABLES
      t_outtab                 = t_saida
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



ENDFORM.                    " CHAMA_ALV


*----------------------------------------------------------------------*
FORM monta_fieldcat USING
               x_field x_tab x_ref x_text x_sum x_just x_qfield
               x_hotspot x_key x_zero.
*----------------------------------------------------------------------*
*

  fieldcat-fieldname     = x_field.
  fieldcat-tabname       = x_tab.
  fieldcat-ref_tabname   = x_ref.
  fieldcat-do_sum        = x_sum.
  fieldcat-just          = x_just.
  fieldcat-qfieldname    = x_qfield.
  fieldcat-hotspot       = x_hotspot.
  fieldcat-key           = x_key.
  fieldcat-no_zero       = x_zero.

  APPEND fieldcat.
  CLEAR fieldcat.
*
ENDFORM.                               " MONTA_FIELDCAT

*----------------------------------------------------------------------*
FORM monta_fieldcat_2 USING
               x_field x_tab x_ref x_text x_sum x_just x_qfield
               x_hotspot x_key x_zero x_seltext_l x_outputlen.
*----------------------------------------------------------------------*
*

  fieldcat-fieldname     = x_field.
  fieldcat-tabname       = x_tab.
  fieldcat-ref_tabname   = x_ref.
  fieldcat-do_sum        = x_sum.
  fieldcat-just          = x_just.
  fieldcat-qfieldname    = x_qfield.
  fieldcat-hotspot       = x_hotspot.
  fieldcat-key           = x_key.
  fieldcat-no_zero       = x_zero.
  fieldcat-seltext_l     = x_seltext_l.
  fieldcat-outputlen     = x_outputlen.

  APPEND fieldcat.
  CLEAR fieldcat.
*
ENDFORM.                               " MONTA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  comando
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*
FORM comando USING ucomm LIKE sy-ucomm
                        selfield TYPE slis_selfield.

  CLEAR: t_msn[].

  CASE sy-ucomm.
    WHEN 'PROC'.

*-US 164011-04-02-2025-#164011-RJF-Inicio
      IF t_mrp2 IS NOT INITIAL.

        READ TABLE t_mrp2 INTO wa_mrp2 WITH KEY mark = 'X'.

        IF sy-subrc <> 0.
*          MESSAGE e000(zfi) WITH 'Marcar ao menos uma linha'.
          MESSAGE 'Marcar ao menos uma linha!' TYPE 'E'.
        ELSE.

          IF wa_mrp2-status IS NOT INITIAL.
*            MESSAGE e000(zfi) WITH 'Processamento já realizado"'.
            MESSAGE 'Processamento já realizado!' TYPE 'E'.
          ENDIF.

          LOOP AT t_mrp2 INTO wa_mrp2 WHERE mark = 'X'.
            DATA(lv_tabix) = sy-tabix.
            PERFORM f_bapi_cmb USING wa_mrp2.
            READ TABLE t_msn INTO DATA(wa_msn) INDEX 1.
            IF sy-subrc IS INITIAL.
              IF wa_msn-tp_msn EQ 'E'.
                wa_mrp2-status = icon_defect.
                wa_mrp2-message = wa_msn-messagem.
                MODIFY t_mrp2 FROM wa_mrp2 INDEX lv_tabix TRANSPORTING status message.
              ELSE.
                wa_mrp2-status = icon_complete.
                wa_mrp2-material = wa_msn-messagem.
                MODIFY t_mrp2 FROM wa_mrp2 INDEX lv_tabix TRANSPORTING status material.
              ENDIF.
              COMMIT WORK AND WAIT.
              DATA(lv_ok) = abap_true.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF lv_ok IS NOT INITIAL.
          PERFORM chama_alv_cmb.
        ENDIF.

      ELSE.
*-US 164011-04-02-2025-#164011-RJF-Fim

        READ TABLE t_saida INTO wa_saida WITH KEY mark = 'X'.

        IF sy-subrc <> 0.
          MESSAGE i000(zfi) WITH 'Marcar ao menos uma linha'.
        ELSE.
          LOOP AT t_saida INTO wa_saida WHERE mark = 'X'.
            PERFORM f_bapi.
            IF NOT t_msn[] IS INITIAL.

            ENDIF.
          ENDLOOP.
        ENDIF.

*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Início de Alteração
        PERFORM iniciar_processo_coupa.
*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Fim de Alteração

        IF NOT t_msn[] IS INITIAL.
          CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
            TABLES
              table    = t_msn
            EXCEPTIONS
              fb_error = 1
              OTHERS   = 2.

          IF NOT sy-subrc IS INITIAL.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.

*-US 164011-04-02-2025-#164011-RJF-Inicio
      ENDIF.
*-US 164011-04-02-2025-#164011-RJF-Fim
  ENDCASE.
ENDFORM.                    "comando

*---------------------------------------------------------------------*
*       FORM SET_STATUS                                               *
*---------------------------------------------------------------------*
FORM set_status USING pf_tab TYPE slis_t_extab.

  SET PF-STATUS 'ALV'.

ENDFORM.                    "SET_STATUS
*&---------------------------------------------------------------------*
*&      Form  VALIDA_USUARIO
*&---------------------------------------------------------------------*
FORM valida_usuario .

  DATA: wl_usnam TYPE zmmt0005-usnam.

  SELECT SINGLE usnam FROM zmmt0005 INTO wl_usnam
    WHERE werks EQ p_centro AND
  usnam EQ sy-uname.

*  IF sy-subrc NE 0.
*    MESSAGE i000(z01) WITH 'Usuário  sem autorização para expansão de material'.
*    STOP.
*  ENDIF.
ENDFORM.                    " VALIDA_USUARIO
*&---------------------------------------------------------------------*
*&      Form  BUSCA_CENTRO_MATERIAL
*&---------------------------------------------------------------------*
FORM busca_centro_material .

  IF p_matnr IS INITIAL.
    PERFORM z_busca_matnr.
  ENDIF.

  CALL FUNCTION 'EINGABEWERTE_WERK'
    EXPORTING
      kzall      = kzall
      kzbew_werk = kzbew_werk
      matnr      = p_matnr
    IMPORTING
      werks      = hwerk.

  IF NOT ( hwerk IS INITIAL )." AND DISPLAY IS INITIAL.
    p_centro = hwerk.
  ENDIF.
*  ENDIF.

ENDFORM.                    " BUSCA_CENTRO_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DEPOSITO_MATERIAL
*&---------------------------------------------------------------------*
FORM busca_deposito_material .

  IF p_matnr IS INITIAL.
    PERFORM z_busca_matnr.
  ENDIF.

  IF p_centro IS INITIAL.
    PERFORM z_busca_werks.
  ENDIF.

  CALL FUNCTION 'EINGABEWERTE_LGORT'
    EXPORTING
      kzall    = kzall
      matnr    = p_matnr
      werks    = p_centro                        "note 991663
    IMPORTING
      werks    = hwerk
      lagerort = hlgort.

  IF NOT ( hlgort IS INITIAL ).
    p_centro = hwerk.
    p_lgort  = hlgort.
  ENDIF.

ENDFORM.                    " BUSCA_DEPOSITO_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_MATNR
*&---------------------------------------------------------------------*
FORM z_busca_matnr .

  CLEAR dynpfields.
  REFRESH dynpfields.

  DATA: set_disp_field LIKE t130r-fname.
  DATA: set_disp_line  LIKE sy-tabix.

* IF T130M-AKTYP EQ AKTYPA OR T130M-AKTYP EQ AKTYPZ.  " ausge*nt für
*   DISPLAY = X.                                      " Keyfelder
* ELSE.                                               " //br101195
  GET CURSOR FIELD set_disp_field LINE set_disp_line.
  CLEAR dynpfields. REFRESH dynpfields.
  dynpfields-fieldname = 'P_MATNR'.
  dynpfields-stepl     = set_disp_line.
  APPEND dynpfields.
*   Lesen des akt. Wertes von Dynpro


  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = dynpfields
    EXCEPTIONS
      OTHERS     = 01.
  READ TABLE dynpfields INDEX 1.
*    p_matnr =   dynpfields-fieldvalue.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = dynpfields-fieldvalue
    IMPORTING
      output = p_matnr.


ENDFORM.                    " Z_BUSCA_MATNR
*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_WERKS
*&---------------------------------------------------------------------*
FORM z_busca_werks .

  CLEAR dynpfields.
  REFRESH dynpfields.

*  DATA: set_disp_field LIKE t130r-fname.
*  DATA: set_disp_line  LIKE sy-tabix.

* IF T130M-AKTYP EQ AKTYPA OR T130M-AKTYP EQ AKTYPZ.  " ausge*nt für
*   DISPLAY = X.                                      " Keyfelder
* ELSE.                                               " //br101195
*  GET CURSOR FIELD set_disp_field LINE set_disp_line.
  CLEAR dynpfields. REFRESH dynpfields.
  dynpfields-fieldname = 'P_WERKS'.
  dynpfields-stepl     = '0'.
  APPEND dynpfields.
*   Lesen des akt. Wertes von Dynpro


  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = dynpfields
    EXCEPTIONS
      OTHERS     = 01.
  READ TABLE dynpfields INDEX 1.
  p_centro =   dynpfields-fieldvalue.


*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = dynpfields-fieldvalue
*    IMPORTING
*      output = p_matnr.




ENDFORM.                    " Z_BUSCA_WERKS


**&---------------------------------------------------------------------*
*&      Form  PROCESSA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processa_arquivo .

  DATA vmatnr18 TYPE matnr18.
  CLEAR t_excel.
  REFRESH: t_excel, t_excel2.
  DATA: dt_vencimento TYPE char10,
        _return       TYPE bapiret2.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 15
      i_end_row               = 10000
    TABLES
      intern                  = t_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Atualizando Dados'.

  t_excel2[] = t_excel[].
  SORT t_excel2 BY row col.
  CLEAR: t_excel2, t_mrp.
  LOOP AT t_excel.
    IF t_excel-row = t_excel2-row.
      CONTINUE.
    ENDIF.
    LOOP AT t_excel2 WHERE row = t_excel-row.
      CASE t_excel2-col.
        WHEN 1.
          wa_mrp-centro   = t_excel2-value.
        WHEN 2.
          wa_mrp-material = t_excel2-value.
        WHEN 3.
          wa_mrp-dismm    = t_excel2-value.
        WHEN 4.
          wa_mrp-minbe    = t_excel2-value.
        WHEN 5.
          wa_mrp-dispo    = t_excel2-value.
        WHEN 6.
          wa_mrp-disls    = t_excel2-value.
        WHEN 7.
          wa_mrp-mabst    = t_excel2-value.
        WHEN 8.
          wa_mrp-bstmi    = t_excel2-value.
        WHEN 9.
          wa_mrp-webaz    = t_excel2-value.
        WHEN 10.
          wa_mrp-plifz    = t_excel2-value.
        WHEN 11.
          wa_mrp-mtvfp    = t_excel2-value.
        WHEN 12.
          wa_mrp-lgort = |{ t_excel2-value CASE = UPPER }|.
        WHEN 13.
          wa_mrp-lgpbe = t_excel2-value.
        WHEN 14.
          wa_mrp-centro_a = t_excel2-value.
        WHEN 15.
          wa_mrp-charg = t_excel2-value.
      ENDCASE.
    ENDLOOP.
    CONCATENATE 'Linha ' t_excel-row 'Material '  wa_mrp-material INTO vmsg SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = vmsg.
    "SHDB
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_mrp-centro
      IMPORTING
        output = wa_mrp-centro.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_mrp-material
      IMPORTING
        output = vmatnr18.

    wa_mrp-material = vmatnr18.
    SELECT SINGLE *
      FROM mara
      INTO @DATA(w_mara)
    WHERE matnr = @wa_mrp-material.

    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF r_mrp = 'X'.

      IF wa_mrp-dispo IS INITIAL.
        wa_mrp-dispo = '001'.
      ENDIF.
      "
      "Somente aba compras
      CLEAR:
        el_headdata,
        el_plantdata,
        el_plantdatax,
        el_valdata,
        el_valdatax,
        el_salesdata,
        el_salesdatax.

      el_headdata-material   = wa_mrp-material.
      el_headdata-matl_type  = w_mara-mtart.
      el_headdata-ind_sector = w_mara-mbrsh.
      "
      el_plantdata-plant           = wa_mrp-centro.
      el_plantdata-mrp_type        = wa_mrp-dismm.
      el_plantdata-reorder_pt      = wa_mrp-minbe.
      el_plantdata-mrp_ctrler      = wa_mrp-dispo.
      el_plantdata-lotsizekey      = wa_mrp-disls.
      el_plantdata-max_stock       = wa_mrp-mabst.
      el_plantdata-minlotsize      = wa_mrp-bstmi.
      el_plantdata-gr_pr_time      = wa_mrp-webaz.
      el_plantdata-plnd_delry      = wa_mrp-plifz.
      el_plantdata-availcheck      = wa_mrp-mtvfp.

      "
      el_plantdatax-plant       = wa_mrp-centro.
      el_plantdatax-mrp_type    = 'X'.
      el_plantdatax-reorder_pt  = 'X'.
      el_plantdatax-mrp_ctrler  = 'X'.
      el_plantdatax-lotsizekey  = 'X'.
      el_plantdatax-max_stock   = 'X'.
      el_plantdatax-minlotsize  = 'X'.
      el_plantdatax-gr_pr_time  = 'X'.
      el_plantdatax-plnd_delry  = 'X'.
      el_plantdatax-availcheck  = 'X'.

    ELSEIF r_dep = 'X'.

*      SELECT COUNT(*)
*        FROM MARD
*        WHERE MATNR EQ WA_MRP-MATERIAL
*        AND WERKS EQ WA_MRP-CENTRO
*        AND LGORT EQ WA_MRP-LGORT.
*
*      IF SY-SUBRC IS INITIAL.
*        CONTINUE.
*      ENDIF.

      IF wa_mrp-centro_a IS NOT INITIAL.

        "*---> 04/07/2023 - Migração S4 - LO
*        el_headdata =
*        VALUE #(
*                  material     = wa_mrp-material
*                  matl_type    = w_mara-mtart
*                  ind_sector   = w_mara-mbrsh
*                  purchase_view = abap_true
*                  sales_view    = abap_true
*                  account_view  = abap_true
*                  mrp_view      = abap_true
*                  storage_view = abap_true
*               ).
        DATA(v_len) = strlen( wa_mrp-material ).

        IF v_len > 18.
          el_headdata =
          VALUE #(
                    material_long   = wa_mrp-material
                    matl_type       = w_mara-mtart
                    ind_sector      = w_mara-mbrsh
                    purchase_view   = abap_true
                    sales_view      = abap_true
                    account_view    = abap_true
                    mrp_view        = abap_true
                    storage_view    = abap_true
                 ).
        ELSE.
          el_headdata =
          VALUE #(
                    material     = wa_mrp-material
                    matl_type    = w_mara-mtart
                    ind_sector   = w_mara-mbrsh
                    purchase_view = abap_true
                    sales_view    = abap_true
                    account_view  = abap_true
                    mrp_view      = abap_true
                    storage_view = abap_true
                 ).
        ENDIF.
        "*---> 04/07/2023 - Migração S4 - LO

      ELSE.
        "*---> 04/07/2023 - Migração S4 - LO
*        el_headdata =
*              VALUE #(
*                        material     = wa_mrp-material
*                        matl_type    = w_mara-mtart
*                        ind_sector   = w_mara-mbrsh
*                        purchase_view = abap_true
*                        storage_view = abap_true
*                     ).
        DATA(v_len2) = strlen( wa_mrp-material ).

        IF v_len2 > 18.
          el_headdata =
                VALUE #(
                          material_long     = wa_mrp-material
                          matl_type    = w_mara-mtart
                          ind_sector   = w_mara-mbrsh
                          purchase_view = abap_true
                          storage_view = abap_true
                       ).
        ELSE.
          el_headdata =
                VALUE #(
                          material     = wa_mrp-material
                          matl_type    = w_mara-mtart
                          ind_sector   = w_mara-mbrsh
                          purchase_view = abap_true
                          storage_view = abap_true
                       ).
        ENDIF.
        "*---> 04/07/2023 - Migração S4 - LO
      ENDIF.

      el_storagelocationdata =
      VALUE #(
               plant      = wa_mrp-centro
               stge_loc   = wa_mrp-lgort
               stge_bin   = wa_mrp-lgpbe
             ).

      el_storagelocationdatax =
      VALUE #(
               plant      = wa_mrp-centro
               stge_loc   = wa_mrp-lgort
               stge_bin   = 'X'
             ).

      CLEAR el_plantdata.
      CLEAR el_plantdatax.
      CLEAR el_valdata.
      CLEAR el_valdatax.
      IF wa_mrp-centro_a IS NOT INITIAL.
        "Compras
        SELECT SINGLE *
          FROM marc
          INTO @DATA(wmarc)
          WHERE matnr = @wa_mrp-material
        AND   werks = @wa_mrp-centro_a.
        el_plantdata-plant       = wa_mrp-centro.
        el_plantdata-auto_p_ord  = wmarc-kautb.
        el_plantdata-ctrl_code   = wmarc-steuc.
        el_plantdata-mat_cfop    = wmarc-indus.

        el_plantdatax-plant      = wa_mrp-centro.
        el_plantdatax-auto_p_ord = 'X'.
        el_plantdatax-ctrl_code  = 'X'.
        el_plantdatax-mat_cfop   = 'X'.

* Início - 02/05/2024 - 2000006453/IR179065 - Stefanini - Problemas com a Transação SAP ZMM0015 - PRB
        "MRP
        "04/06/2024 - excecao MRP - ALRS
        IF 'ZROH_ZHAW_ZHAL_ZFER' CS w_mara-mtart.
          el_plantdata-mrp_type    = 'ND'.
          el_plantdatax-mrp_type   = 'X'.
        ELSE.
          CLEAR el_headdata-mrp_view.
        ENDIF.
        "
* Fim    - 02/05/2024 - 2000006453/IR179065 - Stefanini - Problemas com a Transação SAP ZMM0015 - PRB

        "SD
        el_plantdata-loadinggrp  = wmarc-ladgr.
        el_plantdata-matfrgtgrp  = wmarc-mfrgr.
        el_plantdata-availcheck  = 'KP'. ".
        el_plantdata-rep_manuf   = wmarc-sauft.
        el_plantdata-repmanprof  = wmarc-sfepr.
        "
        el_plantdatax-loadinggrp  = 'X'.
        el_plantdatax-matfrgtgrp  = 'X'.
        el_plantdatax-availcheck  = 'X'.
        el_plantdatax-rep_manuf   = 'X'.
        el_plantdatax-repmanprof  = 'X'.
        "
        "contabilidade
        SELECT SINGLE *
          FROM mbew
          INTO @DATA(wmbew)
          WHERE matnr = @wa_mrp-material
        AND   bwkey = @wa_mrp-centro_a.
        el_valdata-val_area       = wa_mrp-centro.
        el_valdata-in_house       = wmbew-ownpr.
        el_valdata-matl_usage     = wmbew-mtuse.
        el_valdata-mat_origin	    = wmbew-mtorg.
        el_valdata-price_ctrl     = wmbew-vprsv.
        el_valdata-std_price      = wmbew-stprs.
        el_valdata-ml_settle      = wmbew-mlast.
        el_valdata-val_class      = wmbew-bklas.
        el_valdata-in_house       = wmbew-ownpr.
        "
        el_valdatax-val_area      = wa_mrp-centro.
        el_valdatax-in_house      = 'X'.
        el_valdatax-matl_usage    = 'X'.
        el_valdatax-mat_origin    = 'X'.
        el_valdatax-price_ctrl    = 'X'.
        el_valdatax-std_price     = 'X'.
        el_valdatax-ml_settle     = 'X'.
        el_valdatax-val_class     = 'X'.
        el_valdatax-in_house      = 'X'.
      ENDIF.

    ELSEIF r_lote = 'X'.

      DATA: new_batch TYPE TABLE OF mcha,
            l_return  TYPE TABLE OF bapiret2.
      DATA(_header)
            = VALUE mcha(
                          matnr = wa_mrp-material
                          werks = wa_mrp-centro
                          charg = wa_mrp-charg
                          hsdat = sy-datum
                        ).

      CALL FUNCTION 'VB_CREATE_BATCH'
        EXPORTING
          ymcha                        = _header
          new_lgort                    = wa_mrp-lgort
          kzcla                        = '2'
          no_cfc_calls                 = 'X'
        IMPORTING
          ymcha                        = _header
        TABLES
          new_batch                    = new_batch
          return                       = l_return
        EXCEPTIONS
          no_material                  = 1
          no_batch                     = 2
          no_plant                     = 3
          material_not_found           = 4
          plant_not_found              = 5
          stoloc_not_found             = 6
          lock_on_material             = 7
          lock_on_plant                = 8
          lock_on_batch                = 9
          lock_system_error            = 10
          no_authority                 = 11
          batch_exist                  = 12
          stoloc_exist                 = 13
          illegal_batch_number         = 14
          no_batch_handling            = 15
          no_valuation_area            = 16
          valuation_type_not_found     = 17
          no_valuation_found           = 18
          error_automatic_batch_number = 19
          cancelled                    = 20
          wrong_status                 = 21
          interval_not_found           = 22
          number_range_not_extern      = 23
          object_not_found             = 24
          error_check_batch_number     = 25
          no_external_number           = 26
          no_customer_number           = 27
          no_class                     = 28
          error_in_classification      = 29
          inconsistency_in_key         = 30
          region_of_origin_not_found   = 31
          country_of_origin_not_found  = 32
          OTHERS                       = 33.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ELSE.

        CALL FUNCTION 'BALW_BAPIRETURN_GET2'
          EXPORTING
            type   = sy-msgty
            cl     = sy-msgid
            number = sy-msgno
            par1   = sy-msgv1
            par2   = sy-msgv2
            par3   = sy-msgv3
            par4   = sy-msgv4
          IMPORTING
            return = _return.

        wa_mrp-message = _return-message.

*        LOOP AT L_RETURN INTO DATA(RETURN).
*
*          WA_MRP-MESSAGE = RETURN-MESSAGE.
*
        APPEND wa_mrp TO t_mrp_aux.
*        ENDLOOP.


*      WRITE: / 'Material ', WA_MRP-MATERIAL, E_RETURN-MESSAGE.

      ENDIF.


    ENDIF.

    "Quando na planilha estiver sem deposito expandir somente para centro
    IF wa_mrp-lgort IS INITIAL.
      CLEAR:el_storagelocationdata ,el_storagelocationdatax.
    ENDIF.


    IF r_lote IS INITIAL AND r_centro IS INITIAL.
      "*---> 01/07/2023 - Migração S4 - LO
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          headdata             = el_headdata
          plantdata            = el_plantdata
          plantdatax           = el_plantdatax
          valuationdata        = el_valdata
          valuationdatax       = el_valdatax
          salesdata            = el_salesdata
          salesdatax           = el_salesdatax
          storagelocationdata  = el_storagelocationdata
          storagelocationdatax = el_storagelocationdatax
        IMPORTING
          return               = e_return.

      IF e_return-type <> 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Início de Alteração
        PERFORM preencher_zmmt0184 USING wa_mrp-material
                                         wa_mrp-centro.
*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Fim de Alteração

      ELSE.
        APPEND wa_mrp TO t_mrp.
*      WRITE: / 'Material ', WA_MRP-MATERIAL, E_RETURN-MESSAGE.
      ENDIF.
    ELSEIF r_centro IS NOT INITIAL.
      APPEND wa_mrp TO t_mrp.
    ENDIF.

  ENDLOOP.

  IF r_dep = 'X'.

    LOOP AT t_mrp INTO wa_mrp.
      CLEAR: el_headdata,el_storagelocationdata, el_storagelocationdatax.

      SELECT COUNT(*)
        FROM mard
        WHERE matnr EQ wa_mrp-material
          AND werks EQ wa_mrp-centro
      AND lgort EQ wa_mrp-lgort.

      IF sy-subrc IS INITIAL.
        CONTINUE.
      ENDIF.

      "*---> 04/07/2023 - Migração S4 - LO
*      el_headdata =
*        VALUE #(
*                  material     = wa_mrp-material
*                  matl_type    = w_mara-mtart
*                  ind_sector   = w_mara-mbrsh
*                  purchase_view = abap_true
*                  storage_view = abap_true
*               ).
      DATA(v_len3) = strlen( wa_mrp-material ).

      IF v_len3 > 18.
        el_headdata =
          VALUE #(
                    material_long     = wa_mrp-material
                    matl_type         = w_mara-mtart
                    ind_sector        = w_mara-mbrsh
                    purchase_view     = abap_true
                    storage_view      = abap_true
                    ).
      ELSE.
        el_headdata =
          VALUE #(
                    material     = wa_mrp-material
                    matl_type    = w_mara-mtart
                    ind_sector   = w_mara-mbrsh
                    purchase_view = abap_true
                    storage_view = abap_true
                    ).
      ENDIF.
      "*---> 04/07/2023 - Migração S4 - LO


      el_storagelocationdata =
      VALUE #(
               plant      = wa_mrp-centro
               stge_loc   = wa_mrp-lgort
             ).

      el_storagelocationdatax =
      VALUE #(
               plant      = wa_mrp-centro
               stge_loc   = wa_mrp-lgort
             ).

      "*---> 01/07/2023 - Migração S4 - LO
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          headdata             = el_headdata
          plantdata            = el_plantdata
          plantdatax           = el_plantdatax
          valuationdata        = el_valdata
          valuationdatax       = el_valdatax
          salesdata            = el_salesdata
          salesdatax           = el_salesdatax
          storagelocationdata  = el_storagelocationdata
          storagelocationdatax = el_storagelocationdatax
        IMPORTING
          return               = e_return.

      IF e_return-type <> 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Início de Alteração
        PERFORM preencher_zmmt0184 USING wa_mrp-material
                                         wa_mrp-centro.
*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Fim de Alteração

      ELSE.
        wa_mrp-message = e_return-message.
        APPEND wa_mrp TO t_mrp_aux.
*        WRITE: / 'Material ', WA_MRP-MATERIAL, E_RETURN-MESSAGE.
      ENDIF.
    ENDLOOP.

  ENDIF.

*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Início de Alteração
  PERFORM iniciar_processo_coupa.
*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Fim de Alteração

  MESSAGE 'Fim atualização' TYPE 'I'.
ENDFORM.                    " PROCESSA_ARQUIVO

*&---------------------------------------------------------------------*
*&      Form  F_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_bapi .
  DATA:  w_msn     TYPE type_msn.

  DATA t_mlan      TYPE STANDARD TABLE OF  bapi_mlan   WITH HEADER LINE INITIAL SIZE 0.

*  IF WA_SAIDA-MTART = 'ZDIE'.
*    SELECT SINGLE *
*      FROM MLAN
*      INTO @DATA(_MLAN)
*
*    T_MLAN-DEPCOUNTRY =  'DE'.
*    T_MLAN-TAX_TYPE_1   =  'MWST'.      "  Tax category
*    T_MLAN-TAXCLASS_1   =  '0' .             "Tax classification material
*    APPEND T_MLAN.
*  ENDIF.
  "Somente aba compras
  CLEAR:
    el_headdata,
    el_clientdata,
    el_clientdatax,
    el_plantdata,
    el_plantdatax,
    el_valdata,
    el_valdatax,
    el_salesdata,
    el_salesdatax,
    el_storagelocationdata,
    el_storagelocationdatax .

  el_headdata-material      = wa_saida-matnr.
  "*---> 05/07/2023 - Migração S4 - LO
*  el_headdata-material      = wa_saida-matnr.

  DATA(v_len) = strlen( wa_saida-matnr ).

  IF v_len > 18.
    el_headdata-material_long      = wa_saida-matnr.
  ELSE.
    el_headdata-material           = wa_saida-matnr.
  ENDIF.
  "*---> 05/07/2023 - Migração S4 - LO

  el_headdata-matl_type     = wa_saida-mtart.
  el_headdata-ind_sector    = wa_saida-mbrsh.
  el_headdata-purchase_view = 'X'.
  el_headdata-sales_view    = 'X'.
  el_headdata-account_view  = 'X'.
  el_headdata-mrp_view      = 'X'.
  IF p_lgort IS NOT INITIAL.
    el_headdata-storage_view = 'X'.
  ENDIF.

  el_clientdata-division   = wa_saida-spart.
  el_clientdata-trans_grp  = wa_saida-tragr.
  el_clientdata-batch_mgmt = wa_saida-xchpf.

  el_clientdatax-division   =  'X'.
  el_clientdatax-trans_grp  =  'X'.
  el_clientdatax-batch_mgmt =  'X'.

  "Compras
  el_plantdata-plant       = p_centro.
  el_plantdata-auto_p_ord  = wa_saida-kautb.
  el_plantdata-ctrl_code   = wa_saida-steuc.

  "========================Ajuste realizado USER STORY 115211 / AOENNING
  el_plantdata-mat_cfop    = wa_saida-indus.
  el_plantdatax-mat_cfop   = 'X'.
  "========================Ajuste realizado USER STORY 115211 / AOENNING

  el_plantdatax-plant      = p_centro.
  el_plantdatax-auto_p_ord = 'X'.
  el_plantdatax-ctrl_code  = 'X'.

* Início - 23/05/2024 - 2000008417/IR182014 - Stefanini - Problemas com a Transação SAP ZMM0015 - PRB
  "MRP
  "04/06/2024 - excecao MRP - ALRS
  IF 'ZROH_ZHAW_ZHAL_ZFER' CS wa_saida-mtart.
    el_plantdata-mrp_type    = 'ND'.
    el_plantdatax-mrp_type   = 'X'.
  ELSE.
    CLEAR el_headdata-mrp_view.
  ENDIF.
  "

* Fim    - 23/05/2024 - 2000008417/IR182014 - Stefanini - Problemas com a Transação SAP ZMM0015 - PRB

  "SD
  el_plantdata-loadinggrp  = wa_saida-ladgr.
  el_plantdata-matfrgtgrp  = wa_saida-mfrgr.
  el_plantdata-availcheck  = 'KP'. ".
  el_plantdata-rep_manuf   = wa_saida-sauft.
  el_plantdata-repmanprof  = wa_saida-sfepr.
  "
  el_plantdatax-loadinggrp  = 'X'.
  el_plantdatax-matfrgtgrp  = 'X'.
  el_plantdatax-availcheck  = 'X'.
  el_plantdatax-rep_manuf   = 'X'.
  el_plantdatax-repmanprof  = 'X'.

  SELECT SINGLE *
    FROM t024d
    INTO @DATA(_t024d)
    WHERE werks = @p_centro
  AND   dispo = '020'.

* INICIO - RBRIBEIRO - 09/10/2025 - IR255452 - STEFANINI

  IF p_centro EQ wa_saida-werks AND p_lgort IS NOT INITIAL.
    IF sy-subrc = 0.                                          "US159953
      IF 'ZHIB_ZEPI_ERSA' CS wa_saida-mtart.
        el_headdata-mrp_view         = 'X'.
        el_plantdata-mrp_type        = 'PD'.
        el_plantdata-mrp_ctrler      = '020'.
        el_plantdata-lotsizekey      = 'EX'.
        el_plantdata-availcheck      = 'KP'.

        el_plantdatax-mrp_type       = 'X'.
        el_plantdatax-mrp_ctrler     = 'X'.
        el_plantdatax-lotsizekey     = 'X'.
        el_plantdatax-availcheck     = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

* FIM - RBRIBEIRO - 09/10/2025 - IR255452 - STEFANINI
                                                            "US159953
*

  el_valdata-val_area       = p_centro.
  el_valdata-in_house       = wa_saida-ownpr.
  el_valdata-matl_usage     = wa_saida-mtuse.
  el_valdata-mat_origin	    = wa_saida-mtorg.
  el_valdata-price_ctrl     = wa_saida-vprsv.
  el_valdata-std_price      = wa_saida-stprs.
  el_valdata-ml_settle      = wa_saida-mlast.
  el_valdata-val_class      = wa_saida-bklas.

  el_valdatax-val_area      = p_centro.
  el_valdatax-in_house      = 'X'.
  el_valdatax-matl_usage    = 'X'.
  el_valdatax-mat_origin    = 'X'.
  el_valdatax-price_ctrl    = 'X'.
  el_valdatax-std_price     = 'X'.
  el_valdatax-ml_settle     = 'X'.
  el_valdatax-val_class     = 'X'.
  "
  IF wa_saida-mtart = 'ZDIE' OR p_vkorgd IS NOT INITIAL .
    el_salesdata-sales_org   = p_vkorgd.
    el_salesdata-distr_chan  = p_vtwegd.
    el_salesdata-acct_assgt  = wa_saida-ktgrm.
    el_salesdata-item_cat    = wa_saida-mtpos.
    "
    el_salesdatax-sales_org   = p_vkorgd.
    el_salesdatax-distr_chan  = p_vtwegd.
    el_salesdatax-acct_assgt  = 'X'.
    el_salesdatax-item_cat    = 'X'.
  ENDIF.
  "
  IF p_lgort IS NOT INITIAL.
    el_storagelocationdata-plant      = p_centro.
    el_storagelocationdata-stge_loc   = p_lgort.

    el_storagelocationdatax-plant      = p_centro.
    el_storagelocationdatax-stge_loc   = p_lgort.

  ENDIF.

*******************************************************************************
*Início 169622 CS2025000249 - M - ZMM0015 - Avaliar ajustes Expansão Materiais
*******************************************************************************
  IF p_file IS INITIAL. "se processamento individual
    PERFORM f_consistencias.
  ENDIF.


  "*---> 01/07/2023 - Migração S4 - LO
  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      headdata             = el_headdata
      clientdata           = el_clientdata
      clientdatax          = el_clientdatax
      plantdata            = el_plantdata
      plantdatax           = el_plantdatax
      valuationdata        = el_valdata
      valuationdatax       = el_valdatax
      salesdata            = el_salesdata
      salesdatax           = el_salesdatax
      storagelocationdata  = el_storagelocationdata
      storagelocationdatax = el_storagelocationdatax
    IMPORTING
      return               = e_return.

  IF e_return-type <> 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    w_msn-messagem = e_return-message.
    w_msn-tp_msn   = e_return-type.
    APPEND w_msn TO t_msn.

*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Início de Alteração
    PERFORM preencher_zmmt0184 USING wa_saida-matnr
                                     p_centro.
*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Fim de Alteração

  ELSE.
    w_msn-messagem = e_return-message.
    w_msn-tp_msn   = e_return-type.
    APPEND w_msn TO t_msn.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_ERRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_erro .

  CHECK t_mrp_aux IS NOT INITIAL.

  FREE: fieldcat.

  PERFORM monta_fieldcat USING:
      'MATERIAL'   'T_MRP_AUX' 'MARC' ' ' ' ' ' ' ' ' ' ' ' ' 'X',
      'CENTRO'     'T_MRP_AUX' 'MARC' ' ' ' ' ' ' ' ' ' ' ' ' '',
      'LGORT'      'T_MRP_AUX' 'MARD  ' ' ' ' ' ' ' ' ' ' ' ' ' '',
      'MESSAGE'    'T_MRP_AUX' 'BSANLY_BCACT_LOG'    ' ' ' ' ' ' ' ' ' ' ' ' ''.

  w_tit ='Erros na Expansão de Materiais'.

*  LAYOUT-BOX_FIELDNAME     = 'MARK'.
  layout-box_tabname       = 'T_MRP_AUX'.
  layout-zebra = 'X'.
  print-no_print_listinfos = 'X'.

  repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = repid
      i_callback_pf_status_set = 'SET_STATUS'
      it_fieldcat              = fieldcat[]
      is_layout                = layout
      i_grid_title             = w_tit
      is_print                 = print
    TABLES
      t_outtab                 = t_mrp_aux
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDA_TP_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM valida_tp_material .
  CLEAR: vr_check_erro.

  SELECT SINGLE mtart FROM mara INTO @DATA(zmtart) WHERE matnr EQ @p_matnr.
  IF sy-subrc EQ 0.
    SELECT SINGLE *
    FROM setleaf
    INTO @DATA(i_data)
      WHERE setname EQ 'MAGI_MM_ZMM0015'
    AND valfrom EQ @zmtart.

    IF sy-subrc EQ 0.
*    ELSE.
      "Verificar se o campo Org.Vendas e Canal distribuição foi preenchido
*      IF p_vkorg IS INITIAL.
**        MESSAGE i000(z01) WITH 'É obrigatório preenchimento Org.Vendas'.
*        MESSAGE 'É obrigatório preenchimento Org.Vendas' TYPE 'I'.
*        SET CURSOR FIELD 'p_vkorg'.
*        vr_check_erro = 'X'.

*      ELSEIF p_vtweg IS INITIAL.
*        MESSAGE 'É obrigatório preenchimento Canal distribuição' TYPE 'I'.
*        SET CURSOR FIELD 'p_vtweg'.
*        vr_check_erro = 'X'.
*      ELSEIF p_vkorgd IS INITIAL.
*        MESSAGE 'É obrigatório preenchimento Org.Vendas destino' TYPE 'I'.
*        SET CURSOR FIELD 'p_vkorgd'.
*        vr_check_erro = 'X'.
*      ELSEIF p_vtwegd IS INITIAL.
*        MESSAGE 'É obrigatório preenchimento Canal distribuição destino' TYPE 'I'.
*        SET CURSOR FIELD 'p_vtwegd'.
*        vr_check_erro = 'X'.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Início de Alteração
FORM preencher_zmmt0184 USING i_v_matnr TYPE matnr
                              i_v_werks TYPE werks_d.

  CLEAR wa_zmmt0184.

  GET TIME STAMP FIELD DATA(lv_time_stamp).
  wa_zmmt0184-time_stamp    = lv_time_stamp.
  wa_zmmt0184-matnr         = i_v_matnr.
  wa_zmmt0184-werks         = i_v_werks.
  wa_zmmt0184-fg_processado = 'N'.
  wa_zmmt0184-data_processo = sy-datum.
  wa_zmmt0184-hora_processo = sy-timlo.

  APPEND wa_zmmt0184 TO t_zmmt0184_processar.

ENDFORM.

FORM buscar_dados_coupa.

  CLEAR t_zmmt0184_aux.

  t_zmmt0184_aux = t_zmmt0184_processar.
  SORT t_zmmt0184_aux BY matnr.
  DELETE ADJACENT DUPLICATES FROM t_zmmt0184_aux COMPARING matnr.

  IF t_zmmt0184_aux IS NOT INITIAL.

    SELECT matnr mtart
      FROM mara
      INTO TABLE t_mara_mtart
      FOR ALL ENTRIES IN t_zmmt0184_aux
    WHERE matnr = t_zmmt0184_aux-matnr.

    IF sy-subrc IS INITIAL.
      SORT t_mara_mtart BY matnr.
    ENDIF.

  ENDIF.

  t_zmmt0184_aux = t_zmmt0184_processar.
  SORT t_zmmt0184_aux BY matnr werks.
  DELETE ADJACENT DUPLICATES FROM t_zmmt0184_aux COMPARING matnr werks.

  IF t_zmmt0184_aux IS NOT INITIAL.

    SELECT matnr werks
      FROM zmmt0184
      INTO TABLE t_zmmt0184_pendente
      FOR ALL ENTRIES IN t_zmmt0184_aux
      WHERE matnr          = t_zmmt0184_aux-matnr
        AND werks          = t_zmmt0184_aux-werks
    AND fg_processado  = 'N'.

    IF sy-subrc IS INITIAL.
      SORT t_zmmt0184_pendente BY matnr werks.
    ENDIF.

  ENDIF.

ENDFORM.

FORM processar_dados_coupa.

  DATA: vl_processar TYPE boolean.

  SORT t_zmmt0184_processar BY matnr werks.
  DELETE ADJACENT DUPLICATES FROM t_zmmt0184_processar COMPARING matnr werks.

  LOOP AT t_zmmt0184_processar INTO wa_zmmt0184.

    vl_processar = abap_true.

    PERFORM verificar_tp_material USING wa_zmmt0184-matnr
                                  CHANGING vl_processar.

    PERFORM verificar_pendencia USING wa_zmmt0184-matnr wa_zmmt0184-werks
                        CHANGING vl_processar.

    IF vl_processar IS NOT INITIAL.
      APPEND wa_zmmt0184 TO t_zmmt0184_mod.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM verificar_tp_material USING i_v_matnr TYPE matnr
                           CHANGING c_v_processar TYPE boolean.

  READ TABLE t_mara_mtart INTO DATA(w_mara_mtart)
                          WITH KEY matnr = i_v_matnr
                          BINARY SEARCH.

  IF sy-subrc IS INITIAL.

    IF NOT ( 'ZHIB_ERSA_ZEPI_ZLAG_ZREC' CS w_mara_mtart-mtart ).
      CLEAR c_v_processar.
    ENDIF.

  ELSE.

    CLEAR c_v_processar.

  ENDIF.

ENDFORM.

FORM verificar_pendencia USING i_v_matnr TYPE matnr
                               i_v_werks TYPE werks_d
                         CHANGING c_v_processar TYPE boolean.

  READ TABLE t_zmmt0184_pendente TRANSPORTING NO FIELDS
                                 WITH KEY matnr = i_v_matnr
                                          werks = i_v_werks.
  IF sy-subrc IS INITIAL.
    CLEAR c_v_processar.
  ENDIF.

ENDFORM.

FORM preencher_tbl_coupa.

  " Os dados da tabela ZMMT0184 serão usado no programa ZMMR201.
  " Nesse programa, ele irá processar esssa tabela para disparar JOBs
  " que irão realizar a integração com o COUPA

  IF t_zmmt0184_mod IS NOT INITIAL.

    MODIFY zmmt0184 FROM TABLE t_zmmt0184_mod.

    IF sy-subrc IS INITIAL.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDIF.

ENDFORM.

FORM iniciar_processo_coupa.

  PERFORM buscar_dados_coupa.
  PERFORM processar_dados_coupa.
  PERFORM preencher_tbl_coupa.

ENDFORM.
*** Stefanini - IR230283 - 07/11/2024 - LAZAROSR - Fim de Alteração
*-US 164011-04-02-2025-#164011-RJF-Inicio
*&---------------------------------------------------------------------*
*& Form processar_multi
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM processar_multi .

  LOOP AT t_saida INTO wa_saida.
    LOOP AT t_mrp INTO wa_mrp WHERE material EQ wa_saida-matnr.
      PERFORM f_bapi_m USING wa_mrp-centro.
    ENDLOOP.
  ENDLOOP.

  IF NOT t_msn[] IS INITIAL.
    CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
      TABLES
        table    = t_msn
      EXCEPTIONS
        fb_error = 1
        OTHERS   = 2.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_bapi_m
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_bapi_m USING pf_centro TYPE werks_d.

  DATA:  w_msn     TYPE type_msn.

  DATA t_mlan      TYPE STANDARD TABLE OF  bapi_mlan   WITH HEADER LINE INITIAL SIZE 0.

  "Somente aba compras
  CLEAR:
    el_headdata,
    el_clientdata,
    el_clientdatax,
    el_plantdata,
    el_plantdatax,
    el_valdata,
    el_valdatax,
    el_salesdata,
    el_salesdatax,
    el_storagelocationdata,
    el_storagelocationdatax .

  el_headdata-material      = wa_saida-matnr.

  DATA(v_len) = strlen( wa_saida-matnr ).

  IF v_len > 18.
    el_headdata-material_long      = wa_saida-matnr.
  ELSE.
    el_headdata-material           = wa_saida-matnr.
  ENDIF.

  el_headdata-matl_type     = wa_saida-mtart.
  el_headdata-ind_sector    = wa_saida-mbrsh.
  el_headdata-purchase_view = 'X'.
  el_headdata-sales_view    = 'X'.
  el_headdata-account_view  = 'X'.
  el_headdata-mrp_view      = 'X'.
  IF p_lgort IS NOT INITIAL.
    el_headdata-storage_view = 'X'.
  ENDIF.

  el_clientdata-division   = wa_saida-spart.
  el_clientdata-trans_grp  = wa_saida-tragr.
  el_clientdata-batch_mgmt = wa_saida-xchpf.

  el_clientdatax-division   =  'X'.
  el_clientdatax-trans_grp  =  'X'.
  el_clientdatax-batch_mgmt =  'X'.

  "Compras
  el_plantdata-plant       = pf_centro.
  el_plantdata-auto_p_ord  = wa_saida-kautb.
  el_plantdata-ctrl_code   = wa_saida-steuc.
  el_plantdata-mat_cfop    = wa_saida-indus.
  el_plantdatax-mat_cfop   = 'X'.

  el_plantdatax-plant      = pf_centro.
  el_plantdatax-auto_p_ord = 'X'.
  el_plantdatax-ctrl_code  = 'X'.

  IF 'ZROH_ZHAW_ZHAL_ZFER' CS wa_saida-mtart.
    el_plantdata-mrp_type    = 'ND'.
    el_plantdatax-mrp_type   = 'X'.
  ELSE.
    CLEAR el_headdata-mrp_view.
  ENDIF.
  "SD
  el_plantdata-loadinggrp  = wa_saida-ladgr.
  el_plantdata-matfrgtgrp  = wa_saida-mfrgr.
  el_plantdata-availcheck  = 'KP'. ".
  el_plantdata-rep_manuf   = wa_saida-sauft.
  el_plantdata-repmanprof  = wa_saida-sfepr.
  "
  el_plantdatax-loadinggrp  = 'X'.
  el_plantdatax-matfrgtgrp  = 'X'.
  el_plantdatax-availcheck  = 'X'.
  el_plantdatax-rep_manuf   = 'X'.
  el_plantdatax-repmanprof  = 'X'.

  SELECT SINGLE *
    FROM t024d
    INTO @DATA(_t024d)
    WHERE werks = @pf_centro
  AND   dispo = '020'.

  IF sy-subrc = 0.                                          "US159953
    IF 'ZHIB_ZEPI_ERSA' CS wa_saida-mtart.
      el_headdata-mrp_view         = 'X'.
      el_plantdata-mrp_type        = 'PD'.
      el_plantdata-mrp_ctrler      = '020'.
      el_plantdata-lotsizekey      = 'EX'.
      el_plantdata-availcheck      = 'KP'.

      el_plantdatax-mrp_type       = 'X'.
      el_plantdatax-mrp_ctrler     = 'X'.
      el_plantdatax-lotsizekey     = 'X'.
      el_plantdatax-availcheck     = 'X'.
    ENDIF.
  ENDIF.
                                                            "US159953
*

  el_valdata-val_area       = pf_centro.
  el_valdata-in_house       = wa_saida-ownpr.
  el_valdata-matl_usage     = wa_saida-mtuse.
  el_valdata-mat_origin	    = wa_saida-mtorg.
  el_valdata-price_ctrl     = wa_saida-vprsv.
  el_valdata-std_price      = wa_saida-stprs.
  el_valdata-ml_settle      = wa_saida-mlast.
  el_valdata-val_class      = wa_saida-bklas.

  el_valdatax-val_area      = pf_centro.
  el_valdatax-in_house      = 'X'.
  el_valdatax-matl_usage    = 'X'.
  el_valdatax-mat_origin    = 'X'.
  el_valdatax-price_ctrl    = 'X'.
  el_valdatax-std_price     = 'X'.
  el_valdatax-ml_settle     = 'X'.
  el_valdatax-val_class     = 'X'.
  "
  IF wa_saida-mtart = 'ZDIE' OR p_vkorgd IS NOT INITIAL .
    el_salesdata-sales_org   = p_vkorgd.
    el_salesdata-distr_chan  = p_vtwegd.
    el_salesdata-acct_assgt  = wa_saida-ktgrm.
    el_salesdata-item_cat    = wa_saida-mtpos.
    "
    el_salesdatax-sales_org   = p_vkorgd.
    el_salesdatax-distr_chan  = p_vtwegd.
    el_salesdatax-acct_assgt  = 'X'.
    el_salesdatax-item_cat    = 'X'.
  ENDIF.
  "
  IF p_lgort IS NOT INITIAL.
    el_storagelocationdata-plant      = pf_centro.
    el_storagelocationdata-stge_loc   = p_lgort.

    el_storagelocationdatax-plant      = pf_centro.
    el_storagelocationdatax-stge_loc   = p_lgort.

  ENDIF.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata             = el_headdata
      clientdata           = el_clientdata
      clientdatax          = el_clientdatax
      plantdata            = el_plantdata
      plantdatax           = el_plantdatax
      valuationdata        = el_valdata
      valuationdatax       = el_valdatax
      salesdata            = el_salesdata
      salesdatax           = el_salesdatax
      storagelocationdata  = el_storagelocationdata
      storagelocationdatax = el_storagelocationdatax
    IMPORTING
      return               = e_return.

  IF e_return-type <> 'E'.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    w_msn-messagem = e_return-message.
    w_msn-tp_msn   = e_return-type.
    APPEND w_msn TO t_msn.

    PERFORM preencher_zmmt0184 USING wa_saida-matnr
                                     pf_centro.

  ELSE.

    w_msn-messagem = e_return-message.
    w_msn-tp_msn   = e_return-type.
    APPEND w_msn TO t_msn.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form gera_modelo_planillha
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM gera_modelo_planillha .

  DATA valor(30).
  DATA: p_local   TYPE string,
        path(250).
  DATA: t_alvdata      TYPE REF TO data,
        v_nome_arquivo TYPE char50.

  DATA:
    BEGIN OF t_fieldnames OCCURS 0,
      name(50) TYPE c,
    END OF t_fieldnames.

  CONCATENATE 'PlanilhaCargaModelo_' sy-datum+6(2) sy-datum+4(2) sy-datum(4) '_' sy-uzeit INTO v_nome_arquivo.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = v_nome_arquivo
      def_path         = 'C:\'
      mask             = ',*.XLS,'
      mode             = 'S'
      title            = 'Local de Gravação'
    IMPORTING
      filename         = path
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF sy-subrc IS INITIAL.

    CONCATENATE path '.xls' INTO p_local.

    t_fieldnames-name    = 'Setor industrial'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Tipo de material'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Grupo de mercadorias'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Setor de atividade'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Unidade de peso'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Unidade de medida básica'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Texto breve de material'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Centro'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Grupo de verificação de disponibilidade'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'C. controle p/imposto sel. em com ext'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Dados de centro para material'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Classe de avaliacao'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Aprop. custos ledger mat contr.'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Unidade preco'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Utilizacao material'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Origem material'.
    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Organizacao de vendas'.
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Canal de distribuicao'.
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Gr. categ item mestre de mat'.
*    APPEND t_fieldnames.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = p_local
        filetype              = 'ASC'
        write_field_separator = 'X'
        "CODEPAGE            = '8404'
      TABLES
        data_tab              = t_file
        fieldnames            = t_fieldnames
      EXCEPTIONS
        file_open_error       = 1
        file_write_error      = 2
        invalid_filesize      = 3
        invalid_table_width   = 4
        invalid_type          = 5
        no_batch              = 6
        unknown_error         = 7
        OTHERS                = 8.

    IF sy-subrc = 0.
      MESSAGE 'Arquivos gerados com sucesso' TYPE 'S'.
    ELSE.
      MESSAGE 'Arquivo processado com erro' TYPE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form cad_Material_basico
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM cad_material_basico .

  DATA vmatnr18 TYPE matnr18.
  CLEAR t_excel.
  REFRESH: t_excel, t_excel2.
  DATA: dt_vencimento TYPE char10,
        _return       TYPE bapiret2.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 15
      i_end_row               = 10000
    TABLES
      intern                  = t_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Atualizando Dados'.

  t_excel2[] = t_excel[].
  SORT t_excel2 BY row col.
  CLEAR: t_excel2, t_mrp.
  LOOP AT t_excel.
    IF t_excel-row = t_excel2-row.
      CONTINUE.
    ENDIF.
    LOOP AT t_excel2 WHERE row = t_excel-row.
      CASE t_excel2-col.
        WHEN 1.
          wa_mrp2-mbrsh    = t_excel2-value. "Setor industrial
        WHEN 2.
          wa_mrp2-mtart    = t_excel2-value. "Tipo de material
        WHEN 3.
          wa_mrp2-matkl    = t_excel2-value.
        WHEN 4.
          wa_mrp2-spart    = t_excel2-value. "Setor de atividade
        WHEN 5.
          wa_mrp2-gewei    = t_excel2-value. "Unidade de peso
        WHEN 6.
          wa_mrp2-meins    = t_excel2-value.
        WHEN 7.
          wa_mrp2-maktx    = t_excel2-value.
        WHEN 8.
          wa_mrp2-werks    = t_excel2-value.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_mrp2-werks
            IMPORTING
              output = wa_mrp2-werks.

        WHEN 9.
          wa_mrp2-mtvfp    = t_excel2-value.
        WHEN 10.
          wa_mrp2-steuc = t_excel2-value.
        WHEN 11.
          wa_mrp2-kautb = t_excel2-value.
        WHEN 12.
          wa_mrp2-bklas = t_excel2-value.
        WHEN 13.
          wa_mrp2-mlast = t_excel2-value.
        WHEN 14.
          wa_mrp2-peinh    = t_excel2-value. "Setor de atividade
        WHEN 15.
          wa_mrp2-mtuse    = t_excel2-value.
        WHEN 16.
          wa_mrp2-mtorg    = t_excel2-value.
        WHEN 17.
          wa_mrp2-vkorg    = t_excel2-value.
        WHEN 18.
          wa_mrp2-vtweg    = t_excel2-value.
*        WHEN 19.
*          wa_mrp2-sktof    = t_excel2-value.
*        WHEN 20.
*          wa_mrp2-mtpos = |{ t_excel2-value CASE = UPPER }|.
      ENDCASE.
    ENDLOOP.
*    CONCATENATE 'Linha ' t_excel2-row 'Tipo de material '  wa_mrp2-mtart INTO vmsg SEPARATED BY space.
*    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*      EXPORTING
*        text = vmsg.
    "SHDB
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = wa_mrp2-werks "wa_mrp2-centro
*      IMPORTING
*        output = wa_mrp2-werks. "wa_mrp2-centro.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = wa_mrp2-material
*      IMPORTING
*        output = vmatnr18.

*    wa_mrp2-material = vmatnr18.
*    SELECT SINGLE *
*      FROM mara
*      INTO @DATA(w_mara)
*    WHERE matnr = @wa_mrp2-material.

*    IF sy-subrc NE 0.
*      CONTINUE.
*    ENDIF.

    IF r_mrp = 'X'.

*      IF wa_mrp2-dispo IS INITIAL.
*        wa_mrp2-dispo = '001'.
*      ENDIF.
      "
      "Somente aba compras
      CLEAR:
        el_headdata,
        el_plantdata,
        el_plantdatax,
        el_valdata,
        el_valdatax,
        el_salesdata,
        el_salesdatax.

*      el_headdata-material   = wa_mrp2-material.
*      el_headdata-matl_type  = w_mara-mtart.
*      el_headdata-ind_sector = w_mara-mbrsh.

      el_clientdata-division   = wa_mrp2-spart.
      el_clientdata-matl_group = wa_mrp2-matkl.
      el_clientdata-unit_of_wt = wa_mrp2-gewei.
      el_clientdata-base_uom   = wa_mrp2-meins.

      el_clientdatax-division   = abap_true.
      el_clientdatax-matl_group = abap_true.
      el_clientdatax-unit_of_wt = abap_true.
      el_clientdata-base_uom    = abap_true.
      "
      el_plantdata-plant           = wa_mrp2-werks.
      el_plantdata-ctrl_code       = wa_mrp2-steuc.
      el_plantdata-auto_p_ord      = wa_mrp2-kautb.
*      el_plantdata-mrp_type        = wa_mrp2-dismm.
*      el_plantdata-reorder_pt      = wa_mrp2-minbe.
*      el_plantdata-mrp_ctrler      = wa_mrp2-dispo.
*      el_plantdata-lotsizekey      = wa_mrp2-disls.
*      el_plantdata-max_stock       = wa_mrp2-mabst.
*      el_plantdata-minlotsize      = wa_mrp2-bstmi.
*      el_plantdata-gr_pr_time      = wa_mrp2-webaz.
*      el_plantdata-plnd_delry      = wa_mrp2-plifz.
      el_plantdata-availcheck      = wa_mrp2-mtvfp.

      "
      el_plantdatax-plant       = wa_mrp2-werks.
      el_plantdatax-ctrl_code    = abap_true.
      el_plantdatax-auto_p_ord   = abap_true.
*      el_plantdatax-mrp_type    = 'X'.
*      el_plantdatax-reorder_pt  = 'X'.
*      el_plantdatax-mrp_ctrler  = 'X'.
*      el_plantdatax-lotsizekey  = 'X'.
*      el_plantdatax-max_stock   = 'X'.
*      el_plantdatax-minlotsize  = 'X'.
*      el_plantdatax-gr_pr_time  = 'X'.
*      el_plantdatax-plnd_delry  = 'X'.
      el_plantdatax-availcheck  = 'X'.

      el_salesdata-sales_org   = wa_mrp2-vkorg. "p_vkorgd.
      el_salesdata-distr_chan  = wa_mrp2-vtweg. "p_vtwegd.
*      el_salesdata-cash_disc   = wa_mrp2-sktof.
*    el_salesdata-acct_assgt  = wa_saida-ktgrm.
*      el_salesdata-item_cat    = wa_mrp2-mtpos. "wa_saida-mtpos.
      "
      el_salesdatax-sales_org   = wa_mrp2-vkorg. "p_vkorgd.
      el_salesdatax-distr_chan  = wa_mrp2-vtweg. "p_vtwegd.
      el_salesdatax-cash_disc   = abap_true.
*    el_salesdatax-acct_assgt  = 'X'.
      el_salesdatax-item_cat    = 'X'.

    ELSEIF r_dep = 'X'.

*      SELECT COUNT(*)
*        FROM MARD
*        WHERE MATNR EQ WA_MRP-MATERIAL
*        AND WERKS EQ WA_MRP-CENTRO
*        AND LGORT EQ WA_MRP-LGORT.
*
*      IF SY-SUBRC IS INITIAL.
*        CONTINUE.
*      ENDIF.

      IF wa_mrp2-werks IS NOT INITIAL.

*        el_headdata =
*        VALUE #(
*                  material     = wa_mrp-material
*                  matl_type    = w_mara-mtart
*                  ind_sector   = w_mara-mbrsh
*                  purchase_view = abap_true
*                  sales_view    = abap_true
*                  account_view  = abap_true
*                  mrp_view      = abap_true
*                  storage_view = abap_true
*               ).
*        DATA(v_len) = strlen( wa_mrp2-material ).

*        IF v_len > 18.
*          el_headdata =
*          VALUE #(
*                    material_long   = wa_mrp2-material
**                    matl_type       = w_mara-mtart
**                    ind_sector      = w_mara-mbrsh
*                    purchase_view   = abap_true
*                    sales_view      = abap_true
*                    account_view    = abap_true
*                    mrp_view        = abap_true
*                    storage_view    = abap_true
*                 ).
*        ELSE.
*          el_headdata =
*          VALUE #(
*                    material     = wa_mrp2-material
**                    matl_type    = w_mara-mtart
**                    ind_sector   = w_mara-mbrsh
*                    purchase_view = abap_true
*                    sales_view    = abap_true
*                    account_view  = abap_true
*                    mrp_view      = abap_true
*                    storage_view = abap_true
*                 ).
*        ENDIF.

      ELSE.

*        el_headdata =
*              VALUE #(
*                        material     = wa_mrp-material
*                        matl_type    = w_mara-mtart
*                        ind_sector   = w_mara-mbrsh
*                        purchase_view = abap_true
*                        storage_view = abap_true
*                     ).
*        DATA(v_len2) = strlen( wa_mrp2-material ).

*        IF v_len2 > 18.
*          el_headdata =
*                VALUE #(
*                          material_long     = wa_mrp2-material
**                          matl_type    = w_mara-mtart
**                          ind_sector   = w_mara-mbrsh
*                          purchase_view = abap_true
*                          storage_view = abap_true
*                       ).
*        ELSE.
*          el_headdata =
*                VALUE #(
*                          material     = wa_mrp2-material
*                          matl_type    = w_mara-mtart
*                          ind_sector   = w_mara-mbrsh
*                          purchase_view = abap_true
*                          storage_view = abap_true
*                       ).
*        ENDIF.

      ENDIF.

      el_storagelocationdata =
      VALUE #(
               plant      = wa_mrp2-werks
*               stge_loc   = wa_mrp2-lgort
*               stge_bin   = wa_mrp2-lgpbe
             ).

      el_storagelocationdatax =
      VALUE #(
               plant      = wa_mrp2-werks
*               stge_loc   = wa_mrp2-lgort
               stge_bin   = 'X'
             ).

      CLEAR el_plantdata.
      CLEAR el_plantdatax.
      CLEAR el_valdata.
      CLEAR el_valdatax.
      IF wa_mrp2-werks IS NOT INITIAL.
*        "Compras
*        SELECT SINGLE *
*          FROM marc
*          INTO @DATA(wmarc)
*          WHERE matnr = @wa_mrp-material
*        AND   werks = @wa_mrp2-werks.
        el_plantdata-plant       = wa_mrp2-werks.
        el_plantdata-auto_p_ord  = wa_mrp2-kautb.
        el_plantdata-ctrl_code   = wa_mrp2-steuc.
*        el_plantdata-mat_cfop    = wa_mrp2-indus.

        el_plantdatax-plant      = wa_mrp2-werks.
        el_plantdatax-auto_p_ord = 'X'.
        el_plantdatax-ctrl_code  = 'X'.
*        el_plantdatax-mat_cfop   = 'X'.

*        IF 'ZROH_ZHAW_ZHAL_ZFER' CS w_mara-mtart.
*          el_plantdata-mrp_type    = 'ND'.
*          el_plantdatax-mrp_type   = 'X'.
*        ELSE.
*          CLEAR el_headdata-mrp_view.
*        ENDIF.

*        "SD
*        el_plantdata-loadinggrp  = wmarc-ladgr.
*        el_plantdata-matfrgtgrp  = wmarc-mfrgr.
*        el_plantdata-availcheck  = 'KP'. ".
*        el_plantdata-rep_manuf   = wmarc-sauft.
*        el_plantdata-repmanprof  = wmarc-sfepr.
*        "
*        el_plantdatax-loadinggrp  = 'X'.
*        el_plantdatax-matfrgtgrp  = 'X'.
*        el_plantdatax-availcheck  = 'X'.
*        el_plantdatax-rep_manuf   = 'X'.
*        el_plantdatax-repmanprof  = 'X'.
        "
*        "contabilidade
*        SELECT SINGLE *
*          FROM mbew
*          INTO @DATA(wmbew)
*          WHERE matnr = @wa_mrp-material
*        AND   bwkey = @wa_mrp2-werks.
        el_valdata-val_area       = wa_mrp2-werks.
*        el_valdata-in_house       = wmbew-ownpr.
        el_valdata-matl_usage     = wa_mrp2-mtuse.
        el_valdata-mat_origin	    = wa_mrp2-mtorg.
*        el_valdata-price_ctrl     = wmbew-vprsv.
*        el_valdata-std_price      = wmbew-stprs.
        el_valdata-ml_settle      = wa_mrp2-mlast. "wmbew-mlast.
        el_valdata-val_class      = wa_mrp2-bklas. "wmbew-bklas.
*        el_valdata-in_house       = wmbew-ownpr.
        el_valdata-price_unit     = wa_mrp2-peinh.
*        el_valdata-in_house       = wmbew-mtuse.
*        el_valdata-in_house       = wmbew-mtorg.
        "
        el_valdatax-val_area      = wa_mrp2-werks.
        el_valdatax-in_house      = 'X'.
        el_valdatax-matl_usage    = 'X'.
        el_valdatax-mat_origin    = 'X'.
        el_valdatax-price_ctrl    = 'X'.
        el_valdatax-std_price     = 'X'.
        el_valdatax-ml_settle     = 'X'.
        el_valdatax-val_class     = 'X'.
        el_valdatax-in_house      = 'X'.
        el_valdatax-price_unit    = 'X'.
      ENDIF.

*    ELSEIF r_lote = 'X'.
*
*      DATA: new_batch TYPE TABLE OF mcha,
*            l_return  TYPE TABLE OF bapiret2.
*      DATA(_header)
*            = VALUE mcha(
*                          matnr = wa_mrp2-material
*                          werks = wa_mrp2-werks
**                          charg = wa_mrp2-charg
*                          hsdat = sy-datum
*                        ).
*
*      CALL FUNCTION 'VB_CREATE_BATCH'
*        EXPORTING
*          ymcha                        = _header
**         new_lgort                    = wa_mrp2-lgort
*          kzcla                        = '2'
*          no_cfc_calls                 = 'X'
*        IMPORTING
*          ymcha                        = _header
*        TABLES
*          new_batch                    = new_batch
*          return                       = l_return
*        EXCEPTIONS
*          no_material                  = 1
*          no_batch                     = 2
*          no_plant                     = 3
*          material_not_found           = 4
*          plant_not_found              = 5
*          stoloc_not_found             = 6
*          lock_on_material             = 7
*          lock_on_plant                = 8
*          lock_on_batch                = 9
*          lock_system_error            = 10
*          no_authority                 = 11
*          batch_exist                  = 12
*          stoloc_exist                 = 13
*          illegal_batch_number         = 14
*          no_batch_handling            = 15
*          no_valuation_area            = 16
*          valuation_type_not_found     = 17
*          no_valuation_found           = 18
*          error_automatic_batch_number = 19
*          cancelled                    = 20
*          wrong_status                 = 21
*          interval_not_found           = 22
*          number_range_not_extern      = 23
*          object_not_found             = 24
*          error_check_batch_number     = 25
*          no_external_number           = 26
*          no_customer_number           = 27
*          no_class                     = 28
*          error_in_classification      = 29
*          inconsistency_in_key         = 30
*          region_of_origin_not_found   = 31
*          country_of_origin_not_found  = 32
*          OTHERS                       = 33.
*
*      IF sy-subrc IS INITIAL.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*      ELSE.
*
*        CALL FUNCTION 'BALW_BAPIRETURN_GET2'
*          EXPORTING
*            type   = sy-msgty
*            cl     = sy-msgid
*            number = sy-msgno
*            par1   = sy-msgv1
*            par2   = sy-msgv2
*            par3   = sy-msgv3
*            par4   = sy-msgv4
*          IMPORTING
*            return = _return.
*
*        wa_mrp-message = _return-message.
*
**        LOOP AT L_RETURN INTO DATA(RETURN).
**
**          WA_MRP-MESSAGE = RETURN-MESSAGE.
**
*        APPEND wa_mrp2 TO t_mrp_aux2.
**        ENDLOOP.
*
**      WRITE: / 'Material ', WA_MRP-MATERIAL, E_RETURN-MESSAGE.
*
*      ENDIF.

    ENDIF.

*    IF wa_mrp2-lgort IS INITIAL.
*      CLEAR:el_storagelocationdata ,el_storagelocationdatax.
*    ENDIF.

***    IF r_lote IS INITIAL AND r_centro IS INITIAL.
***
***      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA' "#EC CI_USAGE_OK[2438131]
***        EXPORTING
***          headdata             = el_headdata
***          plantdata            = el_plantdata
***          plantdatax           = el_plantdatax
***          valuationdata        = el_valdata
***          valuationdatax       = el_valdatax
***          salesdata            = el_salesdata
***          salesdatax           = el_salesdatax
***          storagelocationdata  = el_storagelocationdata
***          storagelocationdatax = el_storagelocationdatax
***        IMPORTING
***          return               = e_return.
***
***      IF e_return-type <> 'E'.
***        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
***          EXPORTING
***            wait = 'X'.
***
****        PERFORM preencher_zmmt0184 USING wa_mrp2-material
****                                         wa_mrp2-werks.
***
***      ELSE.
***        APPEND wa_mrp2 TO t_mrp2.
****      WRITE: / 'Material ', WA_MRP-MATERIAL, E_RETURN-MESSAGE.
***      ENDIF.
****    ELSEIF r_centro IS NOT INITIAL.
    APPEND wa_mrp2 TO t_mrp2.
***    ENDIF.

  ENDLOOP.


  IF r_dep = 'X'.

    LOOP AT t_mrp2 INTO wa_mrp2.
      CLEAR: el_headdata,el_storagelocationdata, el_storagelocationdatax.

*      SELECT COUNT(*)
*        FROM mard
*        WHERE matnr EQ wa_mrp2-material
*          AND werks EQ wa_mrp2-werks
*      AND lgort EQ wa_mrp2-lgort.

*      IF sy-subrc IS INITIAL.
*        CONTINUE.
*      ENDIF.

*      el_headdata =
*        VALUE #(
*                  material     = wa_mrp-material
*                  matl_type    = w_mara-mtart
*                  ind_sector   = w_mara-mbrsh
*                  purchase_view = abap_true
*                  storage_view = abap_true
*               ).
*      DATA(v_len3) = strlen( wa_mrp-material ).

*      IF v_len3 > 18.
*        el_headdata =
*          VALUE #(
*                    material_long     = wa_mrp2-material
*                    matl_type         = w_mara-mtart
*                    ind_sector        = w_mara-mbrsh
*                    purchase_view     = abap_true
*                    storage_view      = abap_true
*                    ).
*      ELSE.
*        el_headdata =
*          VALUE #(
*                    material     = wa_mrp2-material
*                    matl_type    = w_mara-mtart
*                    ind_sector   = w_mara-mbrsh
*                    purchase_view = abap_true
*                    storage_view = abap_true
*                    ).
*      ENDIF.

      el_storagelocationdata =
      VALUE #(
               plant      = wa_mrp2-werks
*               stge_loc   = wa_mrp2-lgort
             ).

      el_storagelocationdatax =
      VALUE #(
               plant      = wa_mrp2-werks
*               stge_loc   = wa_mrp2-lgort
             ).

      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING
          headdata             = el_headdata
          plantdata            = el_plantdata
          plantdatax           = el_plantdatax
          valuationdata        = el_valdata
          valuationdatax       = el_valdatax
          salesdata            = el_salesdata
          salesdatax           = el_salesdatax
          storagelocationdata  = el_storagelocationdata
          storagelocationdatax = el_storagelocationdatax
        IMPORTING
          return               = e_return.

      IF e_return-type <> 'E'.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.

*        PERFORM preencher_zmmt0184 USING wa_mrp2-material
*                                         wa_mrp2-centro.

      ELSE.
        wa_mrp2-message = e_return-message.
        APPEND wa_mrp2 TO t_mrp_aux2.
*        WRITE: / 'Material ', WA_MRP-MATERIAL, E_RETURN-MESSAGE.
      ENDIF.
    ENDLOOP.

  ENDIF.

*  MESSAGE 'Fim carregamento arquivo!' TYPE 'I'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form seleciona_dados_cmb
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM seleciona_dados_cmb .
  DATA: w1(55).

  IF NOT p_matnr IS INITIAL.
    SELECT matnr matkl mtart mbrsh iprkz spart tragr xchpf
      FROM mara
      INTO CORRESPONDING FIELDS OF TABLE t_mara
      FOR ALL ENTRIES IN t_mrp2
     WHERE matnr EQ t_mrp2-material
       AND matkl EQ t_mrp2-matkl.
  ELSE.
    SELECT matnr matkl mtart mbrsh iprkz spart tragr xchpf
    FROM mara
    INTO CORRESPONDING FIELDS OF TABLE t_mara
    FOR ALL ENTRIES IN t_mrp2
    WHERE matkl EQ t_mrp2-matkl.
  ENDIF.

  IF sy-subrc IS INITIAL.
    SELECT * FROM zmmt0007 INTO TABLE t_06.
    LOOP AT t_mara INTO wa_mara.
      READ TABLE t_06 WITH KEY mtart = wa_mara-mtart.
      IF sy-subrc <> 0.
*        Tipo de Material ZSSO não permitido nesta transação. Iniciar expansão via workflow de cadastro de materiais pela transação ZWFMAT004.
        CONCATENATE   'Tipo Material' wa_mara-mtart 'não permitido nesta transação' INTO w1 SEPARATED BY space.
        CONCATENATE  w1 '.' INTO w1.
        MESSAGE i000(z01) WITH  w1
                               'Iniciar expansão via workflow de cadastro'
                               ' de materiais pela transação ZWFMAT004'.
        STOP.
      ENDIF.
*** BUG 63479 - Inicio - CSB
*      IF wa_mara-mtart = 'ZDIE'.
*        IF p_vkorg IS INITIAL OR p_vtweg IS INITIAL.
*          MESSAGE i000(z01) WITH
*                          'Informe Org. Vendas e Canal distribuição'
*                          ' para pretação de serviços'.
*          STOP.
*        ENDIF.
*      ENDIF.
*** BUG 63479 - Fim - CSB
*      IF ( p_vtweg IS NOT  INITIAL ) AND        "p_vkorg IS NOT INITIAL AND
*         ( p_vtwegd IS INITIAL ).
*        MESSAGE i000(z01) WITH
*                        'Informe Org. Vendas e Canal distribuição DESTINO'
*                        ' para pretação de serviços'.
*        STOP.
*      ENDIF.
    ENDLOOP.


    SELECT matnr spras maktx
      FROM makt
      INTO TABLE t_makt
      FOR ALL ENTRIES IN t_mara
      WHERE matnr EQ t_mara-matnr
    AND   spras EQ sy-langu.

    SELECT  matnr werks steuc indus kautb dismm ladgr mfrgr mtvfp sauft sfepr
      FROM marc
      INTO TABLE t_marc
      FOR ALL ENTRIES IN t_mara
      WHERE matnr EQ t_mara-matnr
    AND   werks IN s_werks.

    IF sy-subrc NE 0.
      MESSAGE i000(z01) WITH
                         'Material não expandido'
                         ' para este centro'.
      STOP.
    ENDIF.
    SELECT *
      FROM ckmlhd
      INTO TABLE t_ckmlhd
      FOR ALL ENTRIES IN t_mara
      WHERE matnr EQ t_mara-matnr
    AND   bwkey IN s_werks.

    SELECT spras matkl wgbez
      FROM t023t
      INTO TABLE t_t023t
      FOR ALL ENTRIES IN t_mara
      WHERE matkl EQ t_mara-matkl
    AND   spras EQ sy-langu.

    IF NOT t_marc[] IS INITIAL.
      SELECT matnr bwkey bwtar bklas mtuse mtorg ownpr vprsv stprs
        FROM mbew
        INTO TABLE t_mbew
        FOR ALL ENTRIES IN t_marc
        WHERE matnr EQ t_marc-matnr
      AND   bwkey EQ t_marc-werks.

      SELECT werks name1
        FROM t001w
        INTO TABLE t_t001w
        FOR ALL ENTRIES IN t_marc
      WHERE werks EQ t_marc-werks.

      IF p_vkorg IS NOT INITIAL.
        SELECT SINGLE *
          FROM mvke
          INTO w_mvke
          WHERE matnr  = p_matnr
          AND   vkorg  = p_vkorg
        AND   vtweg  = p_vtweg.
        IF sy-subrc NE 0.
          MESSAGE i000(z01) WITH
                          'Org.Vendas/Canal distribuição'
                          'não existe neste centro '.
          STOP.
        ENDIF.
      ENDIF.

      IF p_lgort IS NOT INITIAL.
        SELECT matnr werks lgort
         FROM mard
         INTO TABLE t_mard
         FOR ALL ENTRIES IN t_marc
         WHERE matnr EQ t_marc-matnr
         AND   werks EQ t_marc-werks
        AND   lgort EQ p_lgort.

        IF sy-subrc IS INITIAL.

          SELECT werks lgort lgobe
            FROM t001l
            INTO TABLE t_t001l
            FOR ALL ENTRIES IN t_mard
            WHERE werks EQ t_mard-werks
          AND   lgort EQ t_mard-lgort.
*        ELSE.
*          MESSAGE I000(Z01) WITH
*                            'Depósito não existe na origem'
*                            ' '.
*          STOP.
        ENDIF.
      ENDIF.

    ENDIF.
  ELSE.
    MESSAGE 'Material não encontrado!' TYPE 'I'.
    STOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form organiza_dados_cmb
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM organiza_dados_cmb .


  SORT: t_marc  BY matnr werks,
        t_marc2 BY matnr werks,
        t_mbew  BY matnr bwkey,
        t_ckmlhd  BY matnr bwkey,
        t_makt  BY matnr,
        t_t023t BY matkl spras,
        t_t001w BY werks,
        t_mard  BY matnr werks lgort,
        t_t001l BY werks lgort.

  LOOP AT t_mara INTO wa_mara.

    READ TABLE t_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl
                                              spras = sy-langu
                                            BINARY SEARCH.

    READ TABLE t_makt INTO wa_makt WITH KEY matnr = wa_mara-matnr
                                           BINARY SEARCH.

    READ TABLE t_marc INTO wa_marc WITH KEY matnr = wa_mara-matnr
                                            BINARY SEARCH.
    LOOP AT t_marc INTO wa_marc WHERE matnr = wa_mara-matnr.


      READ TABLE t_mbew INTO wa_mbew WITH KEY matnr = wa_marc-matnr
                                              bwkey = wa_marc-werks
                                              BINARY SEARCH.

      READ TABLE t_ckmlhd  INTO wa_ckmlhd  WITH KEY matnr = wa_marc-matnr
                                                    bwkey = wa_marc-werks
                                                    BINARY SEARCH.
      READ TABLE t_t001w INTO wa_t001w WITH KEY werks = wa_marc-werks
                                                BINARY SEARCH.


      CLEAR wa_mard.
      IF p_lgort IS NOT INITIAL.
        READ TABLE t_mard INTO wa_mard  WITH KEY matnr = wa_marc-matnr
                                                 werks = wa_marc-werks BINARY SEARCH.

      ENDIF.
      wa_mrp2-matkl    = wa_mara-matkl.
      wa_mrp2-mbrsh    = wa_mara-mbrsh.
      wa_mrp2-maktx    = wa_makt-maktx.
      wa_mrp2-material    = wa_marc-matnr.
      wa_mrp2-steuc    = wa_marc-steuc.
      wa_mrp2-werks    = wa_marc-werks.
      wa_mrp2-kautb    = wa_marc-kautb.
      wa_mrp2-bklas    = wa_mbew-bklas.
      wa_mrp2-mtorg    = wa_mbew-mtorg.
      wa_mrp2-mtuse    = wa_mbew-mtuse.

      wa_mrp2-mtart    = wa_mara-mtart.
      "
      wa_mrp2-spart    = wa_mara-spart.
      wa_mrp2-mtvfp    = wa_marc-mtvfp.
      wa_mrp2-mlast    = wa_ckmlhd-mlast.

*      IF p_vkorg IS NOT INITIAL.
*        SELECT SINGLE *
*          FROM mvke
*          INTO w_mvke
*          WHERE matnr  = p_matnr
*          AND   vkorg  = p_vkorg
*        AND   vtweg  = p_vtweg.
**        wa_mrp2-ktgrm = w_mvke-ktgrm.
*        wa_mrp2-mtpos = w_mvke-mtpos.
*      ENDIF.

      APPEND wa_mrp2 TO t_mrp2.
      CLEAR wa_mrp2.

    ENDLOOP. "

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form chama_alv_cmb
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM chama_alv_cmb .
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form carregamento_arquivo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM carregamento_arquivo .
  DATA vmatnr18 TYPE matnr18.
  CLEAR t_excel.
  REFRESH: t_excel, t_excel2.
  DATA: dt_vencimento TYPE char10,
        _return       TYPE bapiret2.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 16
      i_end_row               = 10000
    TABLES
      intern                  = t_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Atualizando Dados'.

  t_excel2[] = t_excel[].
  SORT t_excel2 BY row col.
  CLEAR: t_excel2, t_mrp.
  LOOP AT t_excel.
    IF t_excel-row = t_excel2-row.
      CONTINUE.
    ENDIF.
    LOOP AT t_excel2 WHERE row = t_excel-row.
      CASE t_excel2-col.
        WHEN 1.
          wa_mrp2-mbrsh    = t_excel2-value. "Setor industrial
        WHEN 2.
          wa_mrp2-mtart    = t_excel2-value. "Tipo de material
        WHEN 3.
          wa_mrp2-matkl    = t_excel2-value.
        WHEN 4.
          wa_mrp2-spart    = t_excel2-value. "Setor de atividade
        WHEN 5.
          wa_mrp2-gewei    = t_excel2-value. "Unidade de peso
        WHEN 6.
          wa_mrp2-meins    = t_excel2-value.
        WHEN 7.
          wa_mrp2-maktx    = t_excel2-value.
        WHEN 8.
          wa_mrp2-werks    = t_excel2-value.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_mrp2-werks
            IMPORTING
              output = wa_mrp2-werks.

        WHEN 9.
          wa_mrp2-mtvfp    = t_excel2-value.
        WHEN 10.
          wa_mrp2-steuc = t_excel2-value.
        WHEN 11.
          wa_mrp2-kautb = t_excel2-value.
        WHEN 12.
          wa_mrp2-bklas = t_excel2-value.
        WHEN 13.
          wa_mrp2-mlast = t_excel2-value.
        WHEN 14.
          wa_mrp2-peinh    = t_excel2-value. "Setor de atividade
        WHEN 15.
          wa_mrp2-mtuse    = t_excel2-value.
        WHEN 16.
          wa_mrp2-mtorg    = t_excel2-value.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wa_mrp2-mtorg
            IMPORTING
              output = wa_mrp2-mtorg.

*        WHEN 17.
*          wa_mrp2-vkorg    = t_excel2-value.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = wa_mrp2-vkorg
*            IMPORTING
*              output = wa_mrp2-vkorg.
*
*        WHEN 18.
*          wa_mrp2-vtweg    = t_excel2-value.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = wa_mrp2-vtweg
*            IMPORTING
*              output = wa_mrp2-vtweg.
*        WHEN 19.
*          wa_mrp2-mtpos    = t_excel2-value.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = wa_mrp2-mtpos
*            IMPORTING
*              output = wa_mrp2-mtpos.

*        WHEN 19.
*          wa_mrp2-sktof    = t_excel2-value.
*        WHEN 20.
*          wa_mrp2-mtpos = |{ t_excel2-value CASE = UPPER }|.
      ENDCASE.
    ENDLOOP.
    APPEND wa_mrp2 TO t_mrp2.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_bapi_cmb USING wa_mrp2 TYPE ty_mrp2.
  DATA:  w_msn     TYPE type_msn.
  DATA t_mlan      TYPE STANDARD TABLE OF  bapi_mlan   WITH HEADER LINE INITIAL SIZE 0.

  "Somente aba compras
  CLEAR:
    el_headdata,
    el_clientdata,
    el_clientdatax,
    el_plantdata,
    el_plantdatax,
    el_valdata,
    el_valdatax,
    el_salesdata,
    el_salesdatax,
    el_storagelocationdata,
    el_storagelocationdatax .

  FREE: w_msn, t_msn.

*  DATA(v_len) = strlen( wa_saida-matnr ).
*
*  IF v_len > 18.
*    el_headdata-material_long      = wa_saida-matnr.
*  ELSE.
*    el_headdata-material           = wa_saida-matnr.
*  ENDIF.

**==================================================================
  "Preenchimento dos dados HEADER material.
  el_headdata-matl_type     = wa_mrp2-mtart.
  el_headdata-ind_sector    = wa_mrp2-mbrsh.
  el_headdata-basic_view    = abap_true.
  el_headdata-purchase_view = abap_true.
  el_headdata-sales_view    = abap_true.
  el_headdata-account_view  = abap_true.
  el_headdata-mrp_view      = abap_true.

**==================================================================
  "Preenchimento dos dados basicos.
  el_clientdata-division   = wa_mrp2-spart.
  el_clientdata-matl_group = wa_mrp2-matkl.
  el_clientdata-unit_of_wt = wa_mrp2-gewei.
  el_clientdata-base_uom   = wa_mrp2-meins.

  el_clientdatax-matl_group = abap_true.
  el_clientdatax-unit_of_wt = abap_true.
  el_clientdatax-base_uom   = abap_true.
  el_clientdatax-division   = abap_true.
**==================================================================

  "Compras
  el_plantdata-plant       = wa_mrp2-werks.
  el_plantdata-mrp_type    = 'ND'.
  el_plantdata-availcheck  = wa_mrp2-mtvfp.
  el_plantdata-auto_p_ord  = wa_mrp2-kautb.
  el_plantdata-ctrl_code   = wa_mrp2-steuc.
  el_plantdata-auto_p_ord  = wa_mrp2-kautb.
  el_plantdata-ctrl_code   = wa_mrp2-steuc.

  el_plantdatax-plant      = wa_mrp2-werks.
  el_plantdatax-mrp_type   = abap_true.
  el_plantdatax-availcheck = abap_true.
  el_plantdatax-auto_p_ord = abap_true.
  el_plantdatax-ctrl_code  = abap_true.
  el_plantdatax-auto_p_ord = abap_true.
  el_plantdatax-ctrl_code  = abap_true.


**==================================================================
  el_valdata-val_area       = wa_mrp2-werks.
  el_valdata-matl_usage     = wa_mrp2-mtuse.
  el_valdata-mat_origin	    = wa_mrp2-mtorg.
  el_valdata-ml_settle      = wa_mrp2-mlast.
  el_valdata-val_class      = wa_mrp2-bklas.
  el_valdata-price_unit     = wa_mrp2-peinh.

  el_valdatax-val_area      = wa_mrp2-werks.
  el_valdatax-in_house      = abap_true.
  el_valdatax-matl_usage    = abap_true.
  el_valdatax-mat_origin    = abap_true.
  el_valdatax-ml_settle     = abap_true.
  el_valdatax-val_class     = abap_true.
  el_valdatax-price_unit    = abap_true.
  "

**==================================================================

  gs_mat_description-langu     = sy-langu.
  gs_mat_description-langu_iso = sy-langu.
  gs_mat_description-matl_desc = wa_mrp2-maktx.
  APPEND gs_mat_description TO gt_mat_description.
  CLEAR gs_mat_description.

**==================================================================
*  if sy-subrc is not initial.
*    gs_unitsofmeasure-alt_unit   = wa_mrp2-meins.
*  endif.
*
*  gs_unitsofmeasure-denominatr = gv_umren.
*  gs_unitsofmeasure-numerator  = gv_umrez.
*
*  append gs_unitsofmeasure to gt_unitsofmeasure.
*  clear gs_unitsofmeasure.
*
*  gs_unitsofmeasurex-alt_unit   = gs_unitsofmeasure-alt_unit.
*  gs_unitsofmeasurex-denominatr = abap_true.
*  gs_unitsofmeasurex-numerator  = abap_true.
*
*  append gs_unitsofmeasurex to gt_unitsofmeasurex.
*  clear gs_unitsofmeasurex.
*
*  gs_mat_longtext-applobject = 'MATERIAL'.
*  gs_mat_longtext-text_name  = wa_mrp2-maktx.
*  gs_mat_longtext-langu      = SY-langu.
*  gs_mat_longtext-text_id    = 'GRUN'.  "Textos básicos
**
***  loop at lt_texto into ls_texto.
*    gs_mat_longtext-format_col = '*'.
*    gs_mat_longtext-text_line  = wa_mrp2-maktx.
*    append gs_mat_longtext to gt_mat_longtext.
***  endloop.

  PERFORM f_define_codigo_material.

  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
    EXPORTING
      headdata            = el_headdata
      clientdata          = el_clientdata
      clientdatax         = el_clientdatax
      plantdata           = el_plantdata
      plantdatax          = el_plantdatax
      valuationdata       = el_valdata
      valuationdatax      = el_valdatax
      salesdata           = el_salesdata
      salesdatax          = el_salesdatax
    IMPORTING
      return              = e_return
    TABLES
      unitsofmeasure      = gt_unitsofmeasure
      unitsofmeasurex     = gt_unitsofmeasurex
      materialdescription = gt_mat_description
      materiallongtext    = gt_mat_longtext
      returnmessages      = gt_ret_messages.
  IF e_return-type <> 'E'.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    w_msn-messagem = e_return-message_v1.
    w_msn-tp_msn   = e_return-type.
    APPEND w_msn TO t_msn.

  ELSE.
    w_msn-messagem = e_return-message.
    w_msn-tp_msn   = e_return-type.
    APPEND w_msn TO t_msn.

  ENDIF.

ENDFORM.
*-US 164011-04-02-2025-#164011-RJF-Fim
*&---------------------------------------------------------------------*
*& Form f_define_codigo_material
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_define_codigo_material .
  DATA:  lt_mat_number   TYPE STANDARD TABLE OF bapimatinr.
  DATA: ls_mat_number TYPE bapimatinr,
        ls_return     TYPE bapireturn1.

  REFRESH: lt_mat_number.

  CALL FUNCTION 'BAPI_MATERIAL_GETINTNUMBER'
    EXPORTING
      material_type    = el_headdata-matl_type
      industry_sector  = el_headdata-ind_sector
      required_numbers = 1
    IMPORTING
      return           = ls_return
    TABLES
      material_number  = lt_mat_number.

  IF lt_mat_number[] IS NOT INITIAL.

    CLEAR ls_mat_number.
    READ TABLE lt_mat_number INTO ls_mat_number INDEX 1.

    el_headdata-material      = ls_mat_number-material.

  ELSE.
    CLEAR: el_headdata-material, el_headdata-material_long.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.

  DATA: _param  TYPE ustyp_t_parameters,
        r_parid TYPE RANGE OF memoryid,
        zproc   TYPE char01.

  DATA fcode TYPE TABLE OF sy-ucomm.

  FREE: r_parid, _param.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = _param
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.

  IF _param IS NOT INITIAL.
    LOOP AT _param ASSIGNING FIELD-SYMBOL(<ws_param_nep>).
      IF <ws_param_nep>-parid = 'ZPARAM_CREATE_MAT'.
        zproc = abap_true.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF zproc EQ abap_false.
    APPEND 'PROC' TO fcode.
  ENDIF.

  SET PF-STATUS 'ALV' EXCLUDING fcode.
  SET TITLEBAR 'ALV0100' WITH 'Cadastro Material Básico'.

  PERFORM fm_criar_objetos.

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

    WHEN 'PROC'.
      PERFORM fm_processar.

  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form fm_criar_objetos
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
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
       i_titulo  = 'Cadastro Material Básico'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.
    SET HANDLER event_receiver->get_ucomm  FOR gob_gui_alv_grid.

    w_layout-cwidth_opt = abap_true.
    w_layout-zebra      = 'X'.
    w_layout-sel_mode   = 'A'.
    w_layout-col_opt    = abap_true.

    DATA: wa_fcat TYPE lvc_s_fcat.

    LOOP AT fieldcat ASSIGNING FIELD-SYMBOL(<fs_field>).
      wa_fcat = CORRESPONDING #( <fs_field> ).
      wa_fcat-coltext = <fs_field>-seltext_l.
      APPEND wa_fcat TO git_fcat.
      CLEAR wa_fcat.
    ENDLOOP.

    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        is_variant                    = gs_variant
      CHANGING
        it_outtab                     = t_mrp2
        it_fieldcatalog               = git_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_processar
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_processar .

  CALL METHOD gob_gui_alv_grid->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  DESCRIBE TABLE it_selected_rows LINES lines.

  IF ( lines IS INITIAL ).
    MESSAGE 'Marcar ao menos uma linha!' TYPE 'E'.

  ENDIF.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    CLEAR: wa_mrp2.
    LOOP AT t_mrp2 INTO wa_mrp2 FROM wa_selected_rows-index.

      IF wa_mrp2-status IS NOT INITIAL.
        MESSAGE 'Processamento já realizado!' TYPE 'E'.
      ENDIF.

      DATA(lv_tabix) = sy-tabix.
      PERFORM f_bapi_cmb USING wa_mrp2.
      READ TABLE t_msn INTO DATA(wa_msn) INDEX 1.
      IF sy-subrc IS INITIAL.
        IF wa_msn-tp_msn EQ 'E'.
          wa_mrp2-status = icon_defect.
          wa_mrp2-message = wa_msn-messagem.
          MODIFY t_mrp2 FROM wa_mrp2 INDEX lv_tabix TRANSPORTING status message.
        ELSE.
          wa_mrp2-status = icon_complete.
          wa_mrp2-material = wa_msn-messagem.
          MODIFY t_mrp2 FROM wa_mrp2 INDEX lv_tabix TRANSPORTING status material.
        ENDIF.
        COMMIT WORK AND WAIT.
        DATA(lv_ok) = abap_true.
      ENDIF.
      EXIT.
    ENDLOOP.
  ENDLOOP.

  CALL METHOD gob_gui_alv_grid->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form fm_cria_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_cria_fieldcat .

  REFRESH: fieldcat.

  PERFORM monta_fieldcat_2 USING:

'STATUS'   'T_MRP2'   ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Status' '10',
'MATERIAL'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Material' '20',
'MBRSH'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Setor industrial' '20',
'MTART'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Tipo de material' '20',
'MATKL'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Grupo de mercadorias' '20',
'SPART'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Setor de atividade' '20',
'GEWEI'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Unidade de peso' '20',
'MEINS'   'T_MRP2'   'MAKT ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Unidade de medida básica' '20',
'MAKTX'   'T_MRP2'   'MARA ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Texto breve de material' '30',
'WERKS'   'T_MRP2'   'MARC ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Centro' '20',
'MTVFP'   'T_MRP2'   'MARC ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Grupo de verificação' '20',
'STEUC'   'T_MRP2'   'MARC ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Código de controle p/imposto' '20',
'KAUTB'   'T_MRP2'   'MARC ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Dados de centro para material' '20',
'BKLAS'   'T_MRP2'   'MBEW ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Classe de avaliação' '20',
'MLAST'   'T_MRP2'   'MBEW ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Apropriação custos do ledger' '20',
'PEINH'   'T_MRP2'   'MBEW ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Unidade preço' '20',
'MTUSE'   'T_MRP2'   'MBEW ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Utilização de material' '20',
'MTORG'   'T_MRP2'   'MBEW ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Origem de material' '20',
*'VKORG'   'T_MRP2'   'TVKO ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Organização de vendas' '20',
*'VTWEG'   'T_MRP2'   'MVKE ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Canal de distribuição' '20',
*'SKTOF'   'T_MRP2'   'MVKE ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Dados de venda para material' '20',
*'MTPOS'   'T_MRP2'   'MVKE ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Grupo de categorias de item' '20',
'MESSAGE'   'T_MRP2'   ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' 'Mensagem Erro' '40'.


ENDFORM.


"SCREEN 0200 - "MM Ajuste expansão materiais #169622 / RG

*&---------------------------------------------------------------------*
*& Form busca_parametros
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM busca_parametros .

  CALL SCREEN '0200'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'PF_0200'.
  SET TITLEBAR  'TITLE_0200'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  DATA: rb1, rb2.

  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.


    WHEN 'PROC'.

      IF rb1 EQ abap_true.  "call zmmt0197


        SUBMIT zregister_data WITH p_db_tab = 'ZMMT0197'
                           WITH p_stcnam = 'ZMMT0197'
                           WITH p_scmant = '0289'
                           WITH p_title  = 'Parâmetros Centro Fornecedores - ZMM0015'
                           AND RETURN .

        SET SCREEN 0.

      ELSEIF rb2 EQ abap_true.

        SUBMIT zregister_data WITH p_db_tab = 'ZMMT0198'
                            WITH p_stcnam = 'ZMMT0198'
                            WITH p_scmant = '0291'
                            WITH p_title  = 'Parâmetros Centro Fornecedores - ZMM0015'
                            AND RETURN.
        SET SCREEN 0.
      ENDIF.

    WHEN OTHERS.

  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_consistencias
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_consistencias .


  SELECT *
    FROM zmmt0197
     INTO TABLE @DATA(lt_werks_vend).

  SELECT *
    FROM zmmt0198
     INTO TABLE @DATA(lt_mtart).

*  IF line_exists( lt_werks_vend[ werks  = p_centro ] ).

  IF wa_saida-matnr IS NOT INITIAL AND
     wa_saida-werks IS NOT INITIAL.


    "marial sem categoria CFOP
    IF wa_saida-indus IS INITIAL .
      MESSAGE e020(z01) WITH wa_saida-matnr wa_saida-werks.
    ENDIF.

    "material sem NCM
    IF wa_saida-steuc IS INITIAL.
      MESSAGE e019(z01) WITH wa_saida-matnr wa_saida-werks.
    ENDIF.

    IF line_exists( lt_mtart[ mtart  = wa_saida-mtart ] ).
    ELSE.
      "MESSAGE e023(z01) WITH wa_saida-mtart.
    ENDIF.

    IF p_vkorg NE p_vkorgd AND
      p_vkorg IS NOT INITIAL AND
      p_vkorgd IS NOT INITIAL.
      MESSAGE e021(z01) WITH p_centro p_vkorgd.
    ENDIF.

  ENDIF.

*  ELSE.
*
*    MESSAGE i022(z01) WITH p_centro.
*    "se não tiver o centro cadastrado como vendedor segue o processo sem ampliar dados de vendas
*    FREE: el_salesdata, el_salesdatax, el_headdata-sales_view.

*  ENDIF.


*******************************************************************************
*Fim 169622 CS2025000249 - M - ZMM0015 - Avaliar ajustes Expansão Materiais
*******************************************************************************

ENDFORM.
