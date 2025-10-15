*&---------------------------------------------------------------------*
*&  Include           ZMMR_0041_N_TOP
*&---------------------------------------------------------------------*
TYPE-POOLS: slis, abap, icon.

TABLES: zibs_nfse_001, sscrfields, /tcsr/t_act,
        /tcsr/t_hd,zibt_nfse_001,j_1bnfdoc.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZIBS_NFSE_001'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.
CONSTANTS c_x               TYPE c VALUE 'X'.

DATA gv_first_display TYPE c VALUE 'X'.
DATA gv_erro TYPE c.

DATA go_cccontainer        TYPE REF TO cl_gui_custom_container.
DATA go_splitter_container TYPE REF TO cl_gui_splitter_container.
DATA go_container2         TYPE REF TO cl_gui_container.
"DATA go_container3         TYPE REF TO cl_gui_container.
"DATA go_container_dock     TYPE REF TO cl_gui_docking_container.
DATA go_alv_grid1          TYPE REF TO cl_gui_alv_grid.
DATA go_alv_grid2          TYPE REF TO cl_gui_alv_grid.
"DATA go_alv_grid3          TYPE REF TO cl_gui_alv_grid.

DATA go_part1 TYPE REF TO  cl_gui_container .
DATA go_part2 TYPE REF TO  cl_gui_container .

DATA gt_status TYPE TABLE OF dd07v.
DATA zde_se_ns_miro TYPE zde_se_ns_miro.
DATA gt_nfse_001 TYPE TABLE OF zibt_nfse_001.
DATA gt_nfse_002 TYPE TABLE OF zibt_nfse_002.
DATA gt_nfse_005 TYPE TABLE OF zibs_nfse_005.

DATA gt_lfa1 TYPE TABLE OF lfa1.

* ALV layout
DATA: gw_layout1 TYPE lvc_s_layo,
      gw_layout2 TYPE lvc_s_layo,
      gw_layout3 TYPE lvc_s_layo.

DATA:  gw_variant1 TYPE disvariant.

DATA: gt_fieldcatalog1 TYPE lvc_t_fcat,
      gt_fieldcatalog2 TYPE lvc_t_fcat.

DATA: g_custom_conta0200 TYPE REF TO cl_gui_custom_container,
      splitter           TYPE REF TO cl_gui_splitter_container,
      g_container2       TYPE scrfname VALUE 'CC_DOCS',
      container_2        TYPE REF TO cl_gui_container,
      grid2              TYPE REF TO cl_gui_alv_grid,
      wa_fcat_lvc        TYPE lvc_s_fcat,
      lt_fcat_lvc2       TYPE lvc_t_fcat.
"dados alv
DATA gt_dados_alv TYPE STANDARD TABLE OF zibs_nfse_001.
DATA gt_dados_alv_aux TYPE STANDARD TABLE OF zibs_nfse_001.
DATA gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.

DATA: event        TYPE cntl_simple_event,
      events       TYPE cntl_simple_events,
      tl_filter    TYPE lvc_t_filt,
      wl_filter    TYPE lvc_s_filt,
      tl_function  TYPE ui_functions,
      wl_function  LIKE tl_function  WITH HEADER LINE,
      gs_variant_2 TYPE disvariant.

DATA: wa_layout TYPE lvc_s_layo,
      wa_stable TYPE lvc_s_stbl.

DATA gw_line TYPE zibs_nfse_001.

TYPES:
  BEGIN OF ty_docs,
    lblni TYPE essr-lblni,
    mblnr TYPE mseg-mblnr,
    mjahr TYPE mseg-gjahr,
  END OF ty_docs.

DATA it_docs_alv   TYPE TABLE OF ty_docs.
DATA wa_docs       TYPE ty_docs.
DATA: tg_msg_ret      TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      wg_mensagem(30).

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01. "text-s01: Dados básicos

  SELECT-OPTIONS s_bukrs  FOR /tcsr/t_act-bukrs OBLIGATORY. "Empresa
  SELECT-OPTIONS s_branch FOR /tcsr/t_act-branch OBLIGATORY. "Local de negócios

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02. "text-s02: Dados NFS-e

  SELECT-OPTIONS s_dtemis FOR /tcsr/t_hd-dtemissao OBLIGATORY. "Data Emissão NFS-e
  SELECT-OPTIONS s_nfsenu FOR /tcsr/t_hd-nfse_numero.          "Número NFS-e

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-s03. "text-s03: Dados fornecedor

  SELECT-OPTIONS: s_lifnr FOR /tcsr/t_act-lifnr,        "Fornecedor
                  s_cpf   FOR /tcsr/t_hd-p_cpf,  "CPF
                  s_cnpj  FOR /tcsr/t_hd-p_cnpj. "CNPJ

SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-s04. "text-s04: Documentos

  SELECT-OPTIONS: s_ebeln  FOR /tcsr/t_act-ebeln,       "Pedido de Compras
                  s_mblnr  FOR zibt_nfse_001-mblnr,       "Documento de Material
                  s_lblni  FOR zibt_nfse_001-lblni,       "Folha reg.serviços
                  s_belnr  FOR zibt_nfse_001-belnr,       "Nº doc.faturamento
                  s_docnum FOR j_1bnfdoc-docnum, "Nº documento
                  s_nsmiro FOR zde_se_ns_miro.   "Nº solicitação MIRO(SE).

SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-s05. "text-s05: Informações de Status de Documentos

  PARAMETERS: p_all   RADIOBUTTON GROUP g1,             "Todos
              p_pend  RADIOBUTTON GROUP g1 DEFAULT 'X', "Pendentes
              p_final RADIOBUTTON GROUP g1.             "Finalizados

SELECTION-SCREEN END OF BLOCK b5.

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.



CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no,

      catch_hotspot FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

ENDCLASS.                    "lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_double_click.
    PERFORM f_on_click USING e_row e_column es_row_no.
  ENDMETHOD.

  METHOD catch_hotspot.
    IF e_row GT 0.
      READ TABLE it_docs_alv INTO wa_docs INDEX e_row-index.
      IF  e_column = 'LBLNI' AND wa_docs-lblni IS NOT INITIAL.
        SET PARAMETER ID 'BES' FIELD ' '.
        SET PARAMETER ID 'LBL' FIELD wa_docs-lblni.
        SET PARAMETER ID 'LBD' FIELD 'X'.
        CALL TRANSACTION 'ML81N' AND SKIP FIRST SCREEN.
      ELSEIF e_column = 'MBLNR' AND wa_docs-mblnr IS NOT INITIAL.
* ---> S4 Migration - 19/07/2023 - LO
*        SET PARAMETER ID 'MBN' FIELD wa_docs-mblnr.
*        SET PARAMETER ID 'MJA' FIELD wa_docs-mjahr.
*        CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ''
            i_skip_first_screen = 'X'
            i_deadend           = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = wa_docs-mblnr
            i_mjahr             = wa_docs-mjahr
          EXCEPTIONS
            illegal_combination = 1
            OTHERS              = 2.
* <--- S4 Migration - 19/07/2023 - LO
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.                    "lcl_handle_events DEFINITION
