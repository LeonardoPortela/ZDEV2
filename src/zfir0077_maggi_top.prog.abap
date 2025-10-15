*&---------------------------------------------------------------------*
*& Include          ZFIR0077_MAGGI_TOP
*&---------------------------------------------------------------------*

TABLES: bsak, lfa1, t001, adr6,somlreci1,zfit0091.

*&---------------------------------------------------------------------*
*& TABELAS INTERNAS
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_saida,
         empresa        TYPE zfit0077_monitor-empresa,
         fornecedor     TYPE zfit0077_monitor-fornecedor,
         lifnr          TYPE zfit0077_monitor-lifnr,
         divisao        TYPE t001w-werks,
         referencia     TYPE bsak-xblnr,
         doc_compensa   TYPE zfit0077_monitor-doc_compensa,
         belnr          TYPE bsik-belnr,
         vencimento     TYPE BSEG-zfbdt,
         moeda          TYPE zfit0077_monitor-moeda,
         montante       TYPE bsak-dmbtr,
         ptax           TYPE p LENGTH 8 DECIMALS 5, " 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
         vlrdoc         TYPE bsak-wrbtr, " 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
         receiver_email TYPE zfit0077_monitor-receiver_email,
         receiver_qtd   TYPE zfit0077_monitor-receiver_qtd,
         uf_filial      TYPE lfa1-regio,
         blart          TYPE blart,
         tipo           TYPE zfit0077_monitor-tipo,
         bukrs          TYPE zfit0077_monitor-bukrs,
         compensacao    TYPE zfit0077_monitor-compensacao,
         assunto        TYPE zfit0077_monitor-assunto,
         pdf            TYPE zfit0077_monitor-pdf,
         dt_send        TYPE zfit0077_monitor-dt_send,
         HR_send        TYPE zfit0077_monitor-HR_send,
         usuario        TYPE zfit0077_monitor-usuario,
         execucao       TYPE zfit0077_monitor-execucao,
         html           TYPE zfit0077_monitor-html,
         nmfor          TYPE lfa1-name1,
       END OF ty_saida,

       BEGIN OF ty_filtro,
         empresa        TYPE zfit0077_monitor-empresa,
         fornecedor     TYPE zfit0077_monitor-fornecedor,
         lifnr          TYPE zfit0077_monitor-lifnr,
         doc_compensa   TYPE zfit0077_monitor-doc_compensa,
         moeda          TYPE zfit0077_monitor-moeda,
         receiver_email TYPE zfit0077_monitor-receiver_email,
         receiver_qtd   TYPE zfit0077_monitor-receiver_qtd,
         tipo           TYPE zfit0077_monitor-tipo,
         bukrs          TYPE zfit0077_monitor-bukrs,
         nmfor          TYPE lfa1-name1,
         compensacao    TYPE zfit0077_monitor-compensacao,
       END OF ty_filtro,

       BEGIN OF ty_dados,
         bukrs         TYPE bsak-bukrs,
         lifnr         TYPE bsak-lifnr,
         gsber         TYPE bsak-gsber,
         augdt         TYPE bsak-augdt,
         dmbtr         TYPE bsak-dmbtr,
         wrbtr         TYPE bsak-wrbtr,
         augbl         TYPE bsak-augbl,
         gjahr         TYPE bsak-gjahr,
         belnr         TYPE bsak-belnr,
         waers         TYPE bsak-waers,
         DTVENC         TYPE BSAK-zfbdt,
         xblnr         TYPE bsak-xblnr,
         blart         TYPE bsak-blart,
         doc_bkpf      TYPE bkpf-belnr,
         doc_estornado TYPE bkpf-stblg,
         regio         TYPE t001w-regio,
       END OF ty_dados.

DATA tg_saida_bsak      TYPE TABLE OF ty_saida.
DATA tg_saida_bsik      TYPE TABLE OF ty_saida.
DATA tg_envia           TYPE TABLE OF ty_saida.
DATA tg_envia_filtro           TYPE TABLE OF ty_filtro.
DATA tg_envia_salv      TYPE TABLE OF zfit0077_monitor.
DATA wa_envia_salv      TYPE TABLE OF zfit0077_monitor.
DATA tg_envia_comp      TYPE TABLE OF ty_saida.
DATA tg_envia_aux       TYPE TABLE OF ty_saida.
DATA tg_faturas         TYPE TABLE OF ty_saida.
DATA tg_faturas_aux     TYPE TABLE OF ty_saida.
DATA wg_saida_bsik      LIKE LINE OF tg_saida_bsik.
DATA wg_saida_bsak      LIKE LINE OF tg_saida_bsak.
DATA wg_envia           LIKE LINE OF tg_envia.
DATA wg_envia_comp      LIKE LINE OF tg_envia.
DATA aux_doc_compensa   TYPE bsak-augbl.
DATA aux_doc_empresa    TYPE t001-bukrs.
DATA v_executa          TYPE btcjob.
DATA v_stepc            TYPE btcstepcnt.
DATA tg_bsak_aux        TYPE STANDARD TABLE OF bsak INITIAL SIZE 0.
DATA tg_bsik_aux        TYPE STANDARD TABLE OF bsik INITIAL SIZE 0.
DATA tg_bsak            TYPE STANDARD TABLE OF ty_dados INITIAL SIZE 0.
DATA tg_bsik            TYPE STANDARD TABLE OF ty_dados INITIAL SIZE 0.
DATA vl_assunto         TYPE c LENGTH 100.
DATA vl_tipo            TYPE c LENGTH 40.
DATA menssagem_email1   TYPE string.
DATA menssagem_email11  TYPE string.
DATA menssagem_email2   TYPE string.
DATA menssagem_email3   TYPE string.
DATA p_not_file         TYPE c LENGTH 1.

" --------Data for sending e-mail:
DATA it_packing_list    TYPE TABLE OF sopcklsti1.
DATA it_header          TYPE TABLE OF solisti1.
DATA it_contents_txt    TYPE TABLE OF solisti1.
DATA it_html            TYPE TABLE OF solisti1.
DATA it_comprovante_txt TYPE TABLE OF solisti1.
DATA it_tmp_txt         TYPE TABLE OF solisti1.
DATA l_comprovante      TYPE zfit0091.
DATA it_anexo           TYPE TABLE OF sopcklsti1.
DATA wa_anexo           TYPE sopcklsti1.
DATA wa_comprovante_txt TYPE solisti1.
DATA titulo_anexo       TYPE string.
DATA it_contents_bin    TYPE TABLE OF solisti1 INITIAL SIZE 0.
DATA it_contents_hex    TYPE solisti1.
DATA it_anexo_bin       TYPE TABLE OF solisti1   WITH HEADER LINE.
DATA it_receivers       TYPE TABLE OF somlreci1.
DATA enviado_sucesso    TYPE c LENGTH 1.
DATA: data_container TYPE REF TO cl_gui_container.

CONSTANTS c_smart_forms TYPE tdsfname VALUE 'ZFIS31'.

DATA wa_packing_list     TYPE sopcklsti1.
DATA v_func_name         TYPE rs38l_fnam.
DATA wa_contents_txt     TYPE solisti1.
DATA wa_html             TYPE solisti1.
DATA wa_receivers        TYPE somlreci1.
DATA wa_head             TYPE solisti1.
DATA gw_record           LIKE solisti1 OCCURS 0 WITH HEADER LINE.
DATA gw_tline            TYPE STANDARD TABLE OF tline WITH HEADER LINE.

DATA vl_data_venc        TYPE c LENGTH 15.
DATA vl_data             TYPE c LENGTH 10.
DATA vl_montante         TYPE c LENGTH 20.
DATA vl_montante_final   TYPE c LENGTH 20.
DATA vl_montante_final_doc TYPE c LENGTH 20.
DATA vl_ptax             TYPE c LENGTH 20.           " 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
DATA vl_vlrdoc           TYPE c LENGTH 20.           " 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
DATA vlr_ptax            TYPE p LENGTH 8 DECIMALS 5.

DATA aux_ptax            TYPE p LENGTH 8 DECIMALS 5. " 115227 - CS2023000355 Melhoria Job Composição de Pagamento - PSA
DATA aux_vlrTot          TYPE bsak-dmbtr.
DATA aux_vlrdocTot       TYPE bsak-dmbtr.



DATA it_ZFIT0077_MONITOR TYPE STANDARD TABLE OF zfit0077_monitor WITH HEADER LINE.
DATA lr_container      TYPE REF TO cl_gui_custom_container.
DATA container_main TYPE REF TO cl_gui_custom_container.
DATA painel_control  TYPE REF TO cl_gui_splitter_container.
DATA painel_1        TYPE REF TO cl_gui_container.
DATA painel_2        TYPE REF TO cl_gui_container.
DATA container       TYPE REF TO cl_gui_container.
DATA gr_table TYPE REF TO cl_salv_table.
DATA: lr_functions TYPE REF TO cl_salv_functions_list.
DATA lr_columns           TYPE REF TO cl_salv_columns.
DATA lr_columns_TB        TYPE REF TO cl_salv_columns_table.
DATA lr_column            TYPE REF TO cl_salv_column.
DATA lr_column_TB         TYPE REF TO cl_salv_column_table.
DATA lr_events TYPE REF TO cl_salv_events_table.

DATA : g_html_container TYPE REF TO cl_gui_custom_container,
       g_html_control   TYPE REF TO cl_gui_html_viewer.
DATA: lt_pdf     TYPE TABLE OF tline,
      ls_pdf     LIKE LINE OF lt_pdf,
      lv_url     TYPE char255,
      pdf_fsize  TYPE  i,
      lv_content TYPE xstring,
      lt_data    TYPE STANDARD TABLE OF x255.


DEFINE new_line.
  CLEAR wa_html.
  wa_html = &1.
  APPEND wa_html TO it_html.
END-OF-DEFINITION.

DEFINE add_receiver.
  CLEAR wa_receivers.
  wa_receivers-receiver   = &1.
  wa_receivers-rec_type   = 'U'.
  wa_receivers-blind_copy = &2.
  APPEND wa_receivers TO it_receivers[].
END-OF-DEFINITION.

TYPES: BEGIN OF tY_list_send,
         lifnr     TYPE lfa1-lifnr,
         name1     TYPE lfa1-name1,
         adrnr     TYPE lfa1-adrnr,
         smtp_addr TYPE adr6-smtp_addr,
       END OF ty_list_send.

DATA tg_lfa1      TYPE STANDARD TABLE OF ty_list_send INITIAL SIZE 0.
DATA it_list_send TYPE STANDARD TABLE OF ty_list_send INITIAL SIZE 0.
DATA wa_list_send TYPE ty_list_send.
DATA lista        TYPE string.
DATA it_lista     TYPE STANDARD TABLE OF string INITIAL SIZE 0.
DATA vl_acao      TYPE c LENGTH 1.
DATA vl_batch     TYPE sy-batch.

DATA r_forn   TYPE RANGE OF ty_saida-fornecedor WITH HEADER LINE.
DATA r_lifnr  TYPE RANGE OF lfa1-lifnr WITH HEADER LINE.
DATA vl_trin  TYPE sy-datum.
DATA vl_ano   TYPE gjahr.
DATA vl_augdt TYPE zfit0091-dt_pgto.
DATA cdfor    TYPE lifnr.
DATA vl_hoje  TYPE sydatum.
DATA vl_horas  TYPE syuzeit.
DATA add_receivers_list(1) TYPE c.
DATA p_manual(1)    TYPE c.
DATA p_envia(1)    TYPE c.

START-OF-SELECTION.

**********************************************************************
  DATA lr_frete TYPE RANGE OF bsak-blart.
  DATA w_frete LIKE LINE OF lr_frete.

  w_frete-sign   = 'I'.
  w_frete-option = 'EQ'.
  w_frete-low    = 'FT'.
  w_frete-high   = 'FT' .
  APPEND w_frete TO lr_frete.

  w_frete-sign   = 'I'.
  w_frete-option = 'EQ'.
  w_frete-low    = 'ME'.
  w_frete-high   = 'ME' .
  APPEND w_frete TO lr_frete.

  CLEAR: w_frete.

  DATA wl_control    TYPE ssfctrlop.
  wl_control-no_dialog = 'X'.
  wl_control-preview   = space.
  wl_control-device    = 'PRINTER'.
  wl_control-getotf    = 'X'.
  DATA wl_output_opt TYPE ssfcompop.
  wl_output_opt-tddest   = 'LOCL'.
  wl_output_opt-tdimmed  = 'X'.
  wl_output_opt-tdnewid  = 'X'.
  wl_output_opt-tdnoarch = 'X'.

**********************************************************************

  PERFORM seleciona_dados.

  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""Pega Variavel Stvarvc
  "146847 CS2023000355 Correção de disparo - PSA
  SELECT SINGLE low FROM tvarvc
  WHERE name = 'Z_CONFIG_ENVIO_ZFIR0077_MAGGI'
  INTO @vl_acao.
  "Dicionário
  "T - TABLE -> Grava somente em Tabela o Registro de envio.
  "S - SEND -> Envia Direto sem Registar na Tabela.
  "A - ALL -> Grava em tabela e envia os E-mails.
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF tg_envia_salv IS NOT INITIAL.

    IF p_manual = abap_true.
      CALL SCREEN 0200.
    ELSEIF p_envia = abap_true.
      PERFORM valida_envio.
      IF p_manual = abap_true.
        gr_table->refresh( ).
      ENDIF.
    ENDIF.

  ELSE.
    MESSAGE 'E-mail não cadastrado para o fornecedor!' TYPE 'I' DISPLAY LIKE 'I'.
    EXIT.
  ENDIF.
