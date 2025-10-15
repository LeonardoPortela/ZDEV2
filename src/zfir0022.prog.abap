*&--------------------------------------------------------------------*&
*& Report Name    : Lançamento de Adto. de Clientes Insumos           *&
*& Author         : Victor Hugo                                       *&
*& Date           : 17.04.2012                                        *&
*& Funcional Area : Fiscal                                            *&
*&                                                                    *&
*&--------------------------------------------------------------------*&
*& Histórico de Alterações:                                           *&
*&--------------------------------------------------------------------*&
*&  Data      | Request    | Autor      | Alteração                   *&
*&--------------------------------------------------------------------*&
*&--------------------------------------------------------------------*&
*& 15/01/2025 | DEVK9A1R40 | NSEGATIN   |Incluir um campo onde vincula*&
*&                                      |Doc. Cont. com saldo residual*&
*&                                      | - Chamado 163316.           *&
*---------------------------------------------------------------------*&
REPORT  zfir0022 MESSAGE-ID sabapdocu.

*&--------------------------------------------------------------------&*
*& Tabelas
*&--------------------------------------------------------------------&*
TABLES: vbak, zfit0026, zib_contabil, zfit0027, bdcmsgcoll.

*&--------------------------------------------------------------------&*
*& Variaveis Globalf
*&--------------------------------------------------------------------&*
DATA: ok_code LIKE sy-ucomm,
      ver_tab TYPE c,
      edit    TYPE c,
      block   TYPE c,
      vblock  TYPE c,
      txtopen TYPE c.

*&--------------------------------------------------------------------&*
*& Constantes
*&--------------------------------------------------------------------&*
CONSTANTS:

  " Tela
  c_tela01         TYPE c LENGTH 4  VALUE '0100',
  c_tela02         TYPE c LENGTH 4  VALUE '0200',
  c_tela03         TYPE c LENGTH 4  VALUE '0300',
  c_tela32         TYPE c LENGTH 4  VALUE '0302',
  c_tela33         TYPE c LENGTH 4  VALUE '0303',
  c_tela04         TYPE c LENGTH 4  VALUE '0400',


  " User Command
  c_back           TYPE c LENGTH 4  VALUE 'BACK',
  c_cancel         TYPE c LENGTH 6  VALUE 'CANCEL',
  c_exit           TYPE c LENGTH 4  VALUE 'EXIT',

  c_x              TYPE c VALUE 'X',
  c_a              TYPE c VALUE 'A',

  " Catalog
  c_edit           TYPE c LENGTH 10  VALUE 'Visualizar',
  c_bukrs          TYPE c LENGTH 7  VALUE 'Empresa',
  c_auart          TYPE c LENGTH 14 VALUE 'Tipo de Ordem',
  c_kunnr          TYPE c LENGTH 8  VALUE 'Cliente',
  c_name1          TYPE c LENGTH 16 VALUE 'Nome do Cliente',
  c_vkbur          TYPE c LENGTH 20 VALUE 'Escritório de venda',
  c_werks          TYPE c LENGTH 6  VALUE 'Centro',
  c_vbeln          TYPE c LENGTH 15 VALUE 'Ordem de venda',
  c_valdt          TYPE c LENGTH 10 VALUE 'Vencimento',
  c_status         TYPE c LENGTH 15 VALUE 'Status da OV',
  c_erdat          TYPE c LENGTH 18 VALUE 'Data Criação O.V.',
  c_waerk          TYPE c LENGTH 16 VALUE 'Moeda Documento',
  c_netwr          TYPE c LENGTH 21 VALUE 'Valor Ordem de Venda',
  "C_ZLSCH              TYPE C LENGTH 16 VALUE 'Forma Pagamento',
  c_zterm          TYPE c LENGTH 30 VALUE 'Cond. de Pagamento',
  c_text1          TYPE c LENGTH 50 VALUE 'Desc. de Pagamento',
  c_meio_pgmto     TYPE c LENGTH 50 VALUE 'Meio Pagamento',

  " Catalog Lançamento
  c_edit_l         TYPE c LENGTH 18 VALUE 'Editar',
  c_excl_l         TYPE c LENGTH 18 VALUE 'Excluir',
  c_gera_l         TYPE c LENGTH 5  VALUE 'Gerar',
  c_estor_l        TYPE c LENGTH 15 VALUE 'Estornar',
  c_status_l       TYPE c LENGTH 18 VALUE 'Status',

  c_seq_l          TYPE c LENGTH 15 VALUE 'Seq. Lançamento',
  c_bukrs_l        TYPE c LENGTH 15 VALUE 'Empresa',
  c_vbeln_l        TYPE c LENGTH 15 VALUE 'Ordem de Venda',
  c_nfenum         TYPE c LENGTH 15 VALUE 'Referência',
  c_data_venc_l    TYPE c LENGTH 20 VALUE 'Data Vencimento',
  c_moeda_l        TYPE c LENGTH 8  VALUE 'Moeda',
  c_mont_moeda_l   TYPE c LENGTH 20 VALUE 'Mont. Moeda',
  c_taxa_l         TYPE c LENGTH 20 VALUE 'Taxa',
  c_multa_rbdo     TYPE c LENGTH 20 VALUE 'Multa Recebida',
  c_juros_rbdo     TYPE c LENGTH 20 VALUE 'Juros Recebido',
  c_mont_rbdo      TYPE c LENGTH 20 VALUE 'Montante Recebido',

  c_mont_mi_l      TYPE c LENGTH 15 VALUE 'Montante MI',
  c_forma_pag_l    TYPE c LENGTH 20 VALUE 'Forma de Pagto.',
  c_uname_l        TYPE c LENGTH 20 VALUE 'Usuário',
  c_dta_registro_l TYPE c LENGTH 30 VALUE 'Data Lançamento',
  c_doc_contabil_l TYPE c LENGTH 30 VALUE 'Doc. Contabil',
  c_data_pgto      TYPE c LENGTH 30 VALUE 'Data Pagamento',
  c_razao_especial TYPE c LENGTH 30 VALUE 'Razão Especial',
  c_observacao     TYPE c LENGTH 30 VALUE 'Observação',
  c_condicao       TYPE c LENGTH 30 VALUE 'Condiçãi de Pagamento',
  c_pgto_ant       TYPE c LENGTH 30 VALUE 'Tipo Pgto',
  " Catalog de Status de Lançamentos
  c_seq_s          TYPE c LENGTH 15 VALUE 'Seq. Lançamento',
  c_vbeln_s        TYPE c LENGTH 15 VALUE 'Ordem de Venda',
  c_status_s       TYPE c LENGTH 18 VALUE 'Status',
  c_data_venc_s    TYPE c LENGTH 20 VALUE 'Data de Vencimento',
  c_mont_moeda_s   TYPE c LENGTH 20 VALUE 'Mont. Moeda',
  c_uname_s        TYPE c LENGTH 20 VALUE 'Usuário',
  c_dta_registro_s TYPE c LENGTH 30 VALUE 'Data Lançamento',
  c_mensagem       TYPE c LENGTH 10 VALUE 'Mensagem',

  msg_tab          TYPE c LENGTH 100 VALUE 'Por favor clique na opção de visualizar'.

DATA: rbutton_acerto   TYPE c,
      rbutton_deposito TYPE c,
      txtzterm         TYPE char30.

DATA: r_devo_recu     TYPE RANGE OF auart,
      vlr_multa       TYPE vbrk-netwr,
      vlr_juros       TYPE vbrk-netwr,
      mont_rbdo_anter TYPE vbrk-netwr,
      dt_pgto_anter   TYPE datum.
*&--------------------------------------------------------------------&*
*& Estrutura
*&--------------------------------------------------------------------&*
DATA:

  BEGIN OF wa_vbak,
    bukrs_vf TYPE  vbak-bukrs_vf,
    auart    TYPE  vbak-auart,
    kunnr    TYPE  vbak-kunnr,
    vkbur    TYPE  vbak-vkbur,
    vbeln    TYPE  vbak-vbeln,
    erdat    TYPE  vbak-erdat,
    waerk    TYPE  vbak-waerk,
    knumv    TYPE  vbak-knumv,
    netwr    TYPE  vbak-netwr,
  END OF wa_vbak,

  BEGIN OF wa_vbkd,
    vbeln TYPE vbkd-vbeln,
    zlsch TYPE vbkd-zlsch,
    kurrf TYPE vbkd-kurrf,
    valdt TYPE vbkd-valdt,
    zterm TYPE vbkd-zterm,
  END OF wa_vbkd,

  BEGIN OF wa_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END OF wa_kna1,

  BEGIN OF wa_vbap,
    vbeln TYPE vbap-vbeln,
    posnr TYPE vbap-posnr,
    werks TYPE vbap-werks,
    netwr TYPE vbap-netwr,
    mwsbp TYPE vbap-mwsbp,
  END OF wa_vbap,

  BEGIN OF wa_edit,
    vbeln           TYPE zfit0026-vbeln,
    seq             TYPE zfit0026-seq,
    data_venc       TYPE zfit0026-data_venc,
    moeda           TYPE zfit0026-moeda,
    mont_moeda      TYPE zfit0026-mont_moeda,
    taxa            TYPE zfit0026-taxa,
    mont_mi         TYPE zfit0026-mont_mi,
    forma_pag       TYPE zfit0026-forma_pag,
    status          TYPE zfit0026-status,
    bukrs           TYPE zfit0026-bukrs,
    razao_especial  TYPE zfit0026-razao_especial,
    zterm           TYPE zfit0026-zterm,
    nfenum          TYPE j_1bnfdoc-nfenum,
    data_pgto       TYPE zfit0026-data_pgto,
    mont_rbdo       TYPE zfit0026-mont_rbdo,
    vlr_multa_calc  TYPE p DECIMALS 2,
    vlr_juros_calc  TYPE p DECIMALS 2,
*    VLR_MULTA_CALC  TYPE ZFIT0026-VLR_MULTA_CALC,
*    VLR_JUROS_CALC  TYPE ZFIT0026-VLR_JUROS_CALC,
    vlr_multa_rbdo  TYPE zfit0026-vlr_multa_rbdo,
    vlr_juros_rbdo  TYPE zfit0026-vlr_juros_rbdo,
    doc_fatura      TYPE zfit0026-doc_fatura,
    observacao(255) TYPE c,
    ajuste          TYPE c,
    rec_vlr_total   TYPE zfit0026-rec_vlr_total,
    vlr_desc_mult   TYPE zfit0026-vlr_desc_mult,
    vlr_desc_jros   TYPE zfit0026-vlr_desc_jros,
    valor           TYPE vbrk-netwr,
    total_recebido  TYPE vbrk-netwr,
    num_comp_adiant TYPE zfit0026-num_comp_adiant, "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
    doc_cont        TYPE belnr_d,                    "<<<------"163316 - NMS------>>>
  END OF wa_edit,


  BEGIN OF wa_lanc,
    vbeln           TYPE zfit0026-vbeln,
    seq             TYPE zfit0026-seq,
    data_venc       TYPE zfit0026-data_venc,
    moeda           TYPE zfit0026-moeda,
    mont_moeda      TYPE zfit0026-mont_moeda,
    taxa            TYPE zfit0026-taxa,
    forma_pag       TYPE zfit0026-forma_pag,
    status          TYPE zfit0026-status,
    uname           TYPE zfit0026-uname,
    data_registro   TYPE zfit0026-data_registro,
    obj_key         TYPE zfit0026-obj_key,
    docnum          TYPE zfit0026-docnum,
    razao_especial  TYPE zfit0026-razao_especial,
    mont_moeda_fix  TYPE zfit0026-mont_moeda,
    mont_moeda_parc TYPE zfit0026-mont_moeda,
    zterm           TYPE zfit0026-zterm,
    razao           TYPE c LENGTH 8,
    nfenum          TYPE j_1bnfdoc-nfenum,
    data_pgto       TYPE zfit0026-data_pgto,
    vlr_multa_rbdo  TYPE zfit0026-vlr_multa_rbdo,
    vlr_juros_rbdo  TYPE zfit0026-vlr_juros_rbdo,
    mont_mi         TYPE zfit0026-mont_mi,
    mont_rbdo       TYPE zfit0026-mont_rbdo,
    vlr_multa_calc  TYPE zfit0026-vlr_multa_calc,
    vlr_juros_calc  TYPE zfit0026-vlr_juros_calc,
    doc_fatura      TYPE zfit0026-doc_fatura,
    bukrs_vf        TYPE vbak-bukrs_vf,
    edit(4)         TYPE c,
    gera(4)         TYPE c,
    estor(4)        TYPE c,
    status_doc(4)   TYPE c,
    excluir(4)      TYPE c,
    observacao(255) TYPE c,
    ajuste(1)       TYPE c,
    rec_vlr_total   TYPE zfit0026-rec_vlr_total,
    vlr_desc_mult   TYPE zfit0026-vlr_desc_mult,
    vlr_desc_jros   TYPE zfit0026-vlr_desc_jros,
    pgto_ant(15)    TYPE c,
    num_comp_adiant TYPE zfit0026-num_comp_adiant, "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
    mass_flag(1)    TYPE c,
    saldo_finan     TYPE netwr,
  END OF wa_lanc,

  BEGIN OF wa_lanc_ver,
    vbeln           TYPE zfit0026-vbeln,
    seq             TYPE zfit0026-seq,
    data_venc       TYPE zfit0026-data_venc,
    moeda           TYPE zfit0026-moeda,
    mont_moeda      TYPE zfit0026-mont_moeda,
    taxa            TYPE zfit0026-taxa,
    mont_mi         TYPE zfit0026-mont_mi,
    forma_pag       TYPE zfit0026-forma_pag,
    status          TYPE zfit0026-status,
    uname           TYPE zfit0026-uname,
    data_registro   TYPE zfit0026-data_registro,
    bukrs           TYPE zfit0026-bukrs,
    obj_key         TYPE zfit0026-obj_key,
    docnum          TYPE zfit0026-docnum,
    zterm           TYPE zfit0026-zterm,
    doc_fatura      TYPE zfit0026-doc_fatura,
    data_pgto       TYPE zfit0026-data_pgto,
    vlr_multa_rbdo  TYPE zfit0026-vlr_multa_rbdo,
    vlr_juros_rbdo  TYPE zfit0026-vlr_juros_rbdo,
    mont_rbdo       TYPE zfit0026-mont_rbdo,
    vlr_multa_calc  TYPE zfit0026-vlr_multa_calc,
    vlr_juros_calc  TYPE zfit0026-vlr_juros_calc,
    razao_especial  TYPE zfit0026-razao_especial,
    observacao(255) TYPE c,
    ajuste          TYPE zfit0026-ajuste,
    mont_moeda_fix  TYPE zfit0026-mont_moeda,
    nfenum          TYPE j_1bnfdoc-nfenum,
    edit(4)         TYPE c,
    gera(4)         TYPE c,
    status_doc(4)   TYPE c,
    excluir(4)      TYPE c,
    rec_vlr_total   TYPE zfit0026-rec_vlr_total,
    vlr_desc_mult   TYPE zfit0026-vlr_desc_mult,
    vlr_desc_jros   TYPE zfit0026-vlr_desc_jros,
    num_comp_adiant TYPE zfit0026-num_comp_adiant,
  END OF wa_lanc_ver,


  BEGIN OF wa_cont_seq,
    vbeln TYPE zfit0026-vbeln,
    seq   TYPE zfit0026-seq,
  END OF wa_cont_seq,


  BEGIN OF wa_saida,
    bukrs_vf        TYPE  vbak-bukrs_vf,
    auart           TYPE  vbak-auart,
    kunnr           TYPE  vbak-kunnr,
    vkbur           TYPE  vbak-vkbur,
    vbeln           TYPE  vbak-vbeln,
    erdat           TYPE  vbak-erdat,
    waerk           TYPE  vbak-waerk,
    knumv           TYPE  vbak-knumv,
    netwr           TYPE  vbak-netwr,
    netwr_l         TYPE  vbap-netwr,
    mwsbp           TYPE  vbap-mwsbp,
    zlsch           TYPE  vbkd-zlsch,
    kurrf           TYPE  vbkd-kurrf,
    valdt           TYPE  vbkd-valdt,
    name1           TYPE  kna1-name1,
    werks           TYPE  vbap-werks,
    zterm           TYPE  vbkd-zterm,
    text1           TYPE t052u-text1,
    meio_pgmto      TYPE dd07v-ddtext,
    edit(4)         TYPE  c,
    visual(4)       TYPE  c,
    status(4)       TYPE  c,
    observacao(255) TYPE c,
    rec_vlr_total   TYPE zfit0026-rec_vlr_total,
    pgto_ant(15)    TYPE c,
  END OF wa_saida.

*&--------------------------------------------------------------------&*
*& Types
*&--------------------------------------------------------------------&*
TYPES:

  BEGIN OF ty_zib_contabil,
    obj_key TYPE zib_contabil-obj_key,
    seqitem TYPE zib_contabil-seqitem,
  END OF ty_zib_contabil,


  BEGIN OF ty_t052u,
    spras TYPE t052u-spras,
    zterm TYPE t052u-zterm,
    text1 TYPE t052u-text1,
  END OF ty_t052u,


  BEGIN OF ty_zib_contabil_chv,
    obj_key TYPE zib_contabil_chv-obj_key,
    belnr   TYPE zib_contabil_chv-belnr,
    bukrs   TYPE zib_contabil_chv-bukrs,
    gjahr   TYPE zib_contabil_chv-gjahr,
  END OF ty_zib_contabil_chv,

  BEGIN OF ty_zib_contabil_err,
    obj_key        TYPE zib_contabil_err-obj_key,
    nr_item        TYPE zib_contabil_err-nr_item,
    interface      TYPE zib_contabil_err-interface,
    dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
    hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
    type           TYPE zib_contabil_err-type,
    id             TYPE zib_contabil_err-id,
    num            TYPE zib_contabil_err-num,
    message        TYPE zib_contabil_err-message,
    message_v1     TYPE zib_contabil_err-message_v1,
    message_v2     TYPE zib_contabil_err-message_v2,
    message_v3     TYPE zib_contabil_err-message_v3,
    message_v4     TYPE zib_contabil_err-message_v4,
    seqitem_aux    TYPE zfit0026-seq,
  END OF ty_zib_contabil_err,

  BEGIN OF ty_editor,
    line(255),
  END   OF ty_editor,

  BEGIN OF ty_status,
    vbeln          TYPE zfit0026-vbeln,
    seq            TYPE zfit0026-seq,
    data_venc      TYPE zfit0026-data_venc,
    moeda          TYPE zfit0026-moeda,
    mont_moeda     TYPE zfit0026-mont_moeda,
    taxa           TYPE zfit0026-taxa,
    mont_mi        TYPE zfit0026-mont_mi,
    forma_pag      TYPE zfit0026-forma_pag,
    status         TYPE zfit0026-status,
    uname          TYPE zfit0026-uname,
    data_registro  TYPE zfit0026-data_registro,
    obj_key        TYPE zfit0026-obj_key,
    docnum         TYPE zfit0026-docnum,
    mont_moeda_fix TYPE zfit0026-mont_moeda,
    edit(4)        TYPE c,
    gera(4)        TYPE c,
    status_doc(4)  TYPE c,
    excluir(4)     TYPE c,
    seqitem_aux    TYPE zib_contabil_err-nr_item,
    mensagem       TYPE zib_contabil_err-message,
  END OF ty_status,


  BEGIN OF ty_fatu,
    nfenum         TYPE j_1bnfdoc-nfenum,
    doc_fatura     TYPE zfit0026-doc_fatura,
    belnr          TYPE bkpf-belnr,
    valdt          TYPE vbrk-valdt,
    valor          TYPE vbrk-netwr,
    total_recebido TYPE vbrk-netwr,
    saldo_finan    TYPE vbrk-netwr,
    refkey         TYPE j_1bnflin-refkey,
  END OF ty_fatu,

*** US #181883 - MMSILVA - 10.06.2025 - Ini ***
  BEGIN OF TY_LEGENDA,
    ICONE      TYPE CHAR6,
    DESCR(150) TYPE C,
  END OF TY_LEGENDA.
*** US #181883 - MMSILVA - 10.06.2025 - Fim ***

*&--------------------------------------------------------------------&*
*& Tabela Internal
*&--------------------------------------------------------------------&*
DATA: it_edit             LIKE STANDARD TABLE OF wa_edit             WITH HEADER LINE,
      it_lanc             LIKE STANDARD TABLE OF wa_lanc             WITH HEADER LINE,
      it_lanc_ver         LIKE STANDARD TABLE OF wa_lanc_ver         WITH HEADER LINE,
      it_cont_seq         LIKE STANDARD TABLE OF wa_cont_seq         WITH HEADER LINE,
      it_status           TYPE TABLE OF ty_status,
      wa_status           TYPE ty_status,
      it_status_display   TYPE TABLE OF ty_status,
      wa_status_display   TYPE ty_status,
      it_status_aux       TYPE TABLE OF ty_status,
      wa_status_aux       TYPE ty_status,
      it_vbak             LIKE STANDARD TABLE OF wa_vbak             WITH HEADER LINE,
      it_vbkd             LIKE STANDARD TABLE OF wa_vbkd             WITH HEADER LINE,
      it_t052             TYPE TABLE OF t052,
      wa_t052             TYPE t052,
      it_t052u            TYPE TABLE OF ty_t052u,
      it_kna1             LIKE STANDARD TABLE OF wa_kna1             WITH HEADER LINE,
      it_vbap             LIKE STANDARD TABLE OF wa_vbap             WITH HEADER LINE,
*     IT_VBAP             LIKE TABLE OF WA_VBAP                      WITH HEADER LINE,
      it_zsdt0090         TYPE STANDARD TABLE OF zsdt0090,
      it_zsdt0041         TYPE TABLE OF zsdt0041,
      wa_zsdt0041         TYPE zsdt0041,
      it_zsdt0040         TYPE TABLE OF zsdt0040,
      wa_zsdt0040         TYPE zsdt0040,
      it_editor           TYPE TABLE OF ty_editor,
      wa_editor           TYPE ty_editor,
      it_zib_contabil     TYPE TABLE OF ty_zib_contabil,
      it_zib_contabil_chv TYPE TABLE OF ty_zib_contabil_chv,
      it_zib_contabil_err TYPE TABLE OF ty_zib_contabil_err,
      t_zib_contabil_err  TYPE TABLE OF ty_zib_contabil_err, "// INICIO WBARBOSA 11-12-2024 US-115811
      wa_zib_contabil     TYPE ty_zib_contabil,
      wa_zib_contabil_chv TYPE ty_zib_contabil_chv,
      wa_zib_contabil_err TYPE ty_zib_contabil_err,
      wa_t052u            TYPE ty_t052u,
      wa_zsdt0090         TYPE zsdt0090,
      it_bdc              TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      opt                 TYPE ctu_params,
      zib_contabil_wa     TYPE zib_contabil,
      it_saida            LIKE STANDARD TABLE OF wa_saida WITH HEADER LINE,
      it_dd07v_tab        TYPE TABLE OF dd07v,
      wa_dd07v            TYPE dd07v,
      it_fatu             TYPE TABLE OF ty_fatu,
      wa_fatu             TYPE ty_fatu,
      it_legenda          TYPE STANDARD TABLE OF ty_legenda. "US #181883 - MMSILVA - 10.06.2025

*&--------------------------------------------------------------------&*
*& Estrutura ALV
*&--------------------------------------------------------------------&*
DATA: wa_fieldcatalog             TYPE lvc_s_fcat,
      it_fieldcatalog             TYPE lvc_t_fcat,

      wa_fieldcatalog_f           TYPE lvc_s_fcat,
      it_fieldcatalog_f           TYPE lvc_t_fcat,

      tl_function                 TYPE ui_functions,
      wl_function                 LIKE tl_function WITH HEADER LINE,
      tg_selectedrow              TYPE lvc_t_row,
      wg_selectedrow              TYPE lvc_s_row,


      cl_custom                   TYPE REF TO cl_gui_custom_container,
      cl_custom_l                 TYPE REF TO cl_gui_custom_container,
      cl_custom_s                 TYPE REF TO cl_gui_custom_container,
      cl_custom_f                 TYPE REF TO cl_gui_custom_container,

      grid_principal              TYPE REF TO cl_gui_alv_grid,
      grid_lancamento             TYPE REF TO cl_gui_alv_grid,
      grid_status                 TYPE REF TO cl_gui_alv_grid,
      grid_faturamento            TYPE REF TO cl_gui_alv_grid,

      cl_timer_l                  TYPE REF TO cl_gui_timer,

      editor                      TYPE REF TO cl_gui_textedit,
      c_editor                    TYPE REF TO cl_gui_custom_container,

      wa_layout                   TYPE lvc_s_layo,
      wa_layout_l                 TYPE lvc_s_layo,
      wa_layout_s                 TYPE lvc_s_layo,
      wa_layout_f                 TYPE lvc_s_layo,
      wa_stable                   TYPE lvc_s_stbl,
      wa_stable_l                 TYPE lvc_s_stbl,
      wa_stable_s                 TYPE lvc_s_stbl,
      wa_stable_f                 TYPE lvc_s_stbl VALUE 'XX',
      gs_variant_c                TYPE disvariant,

*** US #181883 - MMSILVA - 10.06.2025 - Ini ***
      g_custom_container_pop_6001 TYPE REF TO cl_gui_custom_container,
      ctl_alv1_pop_6001           TYPE REF TO cl_gui_alv_grid,
      gs_layout_pop_6001          TYPE lvc_s_layo,
      it_fieldcatalog_pop_6001    TYPE lvc_t_fcat,
      _stable                     TYPE lvc_s_stbl VALUE 'XX'.



DATA: vbeln_aux     TYPE vbak-vbeln,
      l_status(1)   TYPE c,
      l_message(64) TYPE c.

DATA: it_value LIKE rgsb4 OCCURS 0 WITH HEADER LINE,
      p_spart  TYPE RANGE OF vbak-spart,
      w_spart  LIKE LINE OF p_spart,
      calc     TYPE c.


*&--------------------------------------------------------------------&*
*& SELEÇÃO
*&--------------------------------------------------------------------&*
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME  TITLE TEXT-005.
  PARAMETERS: p_ins RADIOBUTTON GROUP g1,
              p_mi  RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_bukrs FOR vbak-bukrs_vf OBLIGATORY,
                  "p_auart FOR vbak-auart,
                  p_vkbur FOR vbak-vkbur OBLIGATORY,
                  p_kunnr FOR vbak-kunnr,
                  p_vbeln FOR vbak-vbeln,
                  p_erdat FOR vbak-erdat.
SELECTION-SCREEN: END OF BLOCK b1.


************************************
*
*  CLASSE CHECK ESTORNO DOCNUM.
*
************************************
CLASS check_estor_docnum DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS check_estorn_docnum IMPORTING i_vbeln TYPE vbeln.

ENDCLASS.


CLASS check_estor_docnum IMPLEMENTATION.

  METHOD check_estorn_docnum.

    SELECT *
      FROM zfit0026
      INTO TABLE @DATA(t_zfit0026)
    WHERE vbeln EQ  @i_vbeln.


    IF t_zfit0026 IS NOT INITIAL.
      LOOP AT t_zfit0026 ASSIGNING FIELD-SYMBOL(<w_zfit0026>).
        IF <w_zfit0026>-docnum IS NOT INITIAL.

***      Verifica se o documento esta estornado.
          SELECT SINGLE *
          FROM bkpf
          INTO @DATA(w_bkpf)
            WHERE belnr EQ @<w_zfit0026>-docnum
              AND stgrd NE ' '.

          IF w_bkpf IS NOT INITIAL.
*****     Documento foi estornado e a ZFIT0026 sera atualizada.

****      Atualizar a zfit0026 documento contabíl.
            UPDATE zfit0026 SET docnum        = ''
                                status        = ''
*                               DATA_VENC     = ''
*                               TAXA          = ''
*                               DATA_REGISTRO = ''
*                               FORMA_PAG     = ''
                                WHERE docnum EQ <w_zfit0026>-docnum.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    FREE: t_zfit0026.
    CLEAR: w_bkpf.
  ENDMETHOD.
ENDCLASS.

*&--------------------------------------------------------------------&*
*& START OF SELECTION
*&--------------------------------------------------------------------&*
START-OF-SELECTION.



  PERFORM busca_set.

  PERFORM: seleciona_dados,
           display_catalog.

  CONTROLS tabstrip TYPE TABSTRIP.
  CALL SCREEN c_tela01.

  IF ( ok_code EQ 'TAB_P' ).
    CALL SCREEN c_tela02.
    CALL SCREEN c_tela03.

  ELSEIF ( ok_code EQ 'TAB_F' ).
    CONTROLS tabstrip300 TYPE TABSTRIP.
    CALL SCREEN c_tela32.
    CALL SCREEN c_tela33.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  IF p_ins IS NOT INITIAL.
    SET TITLEBAR  'TB0100'.
  ELSE.
    SET TITLEBAR  'TB0200'.
  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  SELECT COUNT( * )
    FROM zsdt0090
    WHERE vbelv EQ wa_edit-vbeln
    AND categoria EQ 'C'
    AND estorno EQ abap_false.

  IF sy-subrc IS INITIAL.
    DATA(ok_) = abap_true.
  ELSE.
    ok_ = abap_false.
  ENDIF.

  DATA  valor TYPE c LENGTH 20.

  valor = wa_lanc-mont_moeda_parc.
  TRANSLATE valor USING '. '.
  CONDENSE valor NO-GAPS .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN "'WA_EDIT-DATA_VENC'        OR  'WA_EDIT-MONT_MOEDA'
           'WA_EDIT-TAXA'             OR 'WA_EDIT-FORMA_PAG'
        OR 'WA_EDIT-ZTERM'            OR 'WA_EDIT-AJUSTE'
        OR 'WA_EDIT-DATA_PGTO'        OR 'WA_EDIT-MONT_RBDO'
        OR 'WA_EDIT-VLR_MULTA_RBDO'   OR 'WA_EDIT-VLR_JUROS_RBDO'
        OR 'SELEC'                    OR 'WA_EDIT-REC_VLR_TOTAL'
        OR 'WA_EDIT-NUM_COMP_ADIANT'.

*        OR 'WA_EDIT-NFENUM'

        IF NOT block IS INITIAL.
          screen-input = 0. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.


        IF NOT txtopen IS INITIAL.

          IF ( 'P' NE wa_edit-status ) OR ( 'G' NE wa_edit-status ).
            IF ( edit EQ 'X' ).
*              IF ( WA_LANC-DOCNUM EQ 0 ).
              IF ( wa_edit-status EQ ' ' OR wa_edit-status EQ 'A' ).

                IF ( screen-name EQ 'WA_EDIT-TAXA' ) AND ( wa_edit-moeda EQ 'BRL' ) .
                  screen-input = 0. "Campo Fechado
                ELSE.

                  IF screen-name EQ 'WA_EDIT-TAXA'.

                    IF NOT ok_ IS INITIAL.
                      screen-input = 0.
                    ELSE.
                      screen-input = 1.
                    ENDIF.

                  ELSE.
                    screen-input = 1.
                  ENDIF.

                ENDIF.

*                IF SCREEN-NAME EQ 'WA_EDIT-NFENUM'.
*                  IF VBLOCK IS NOT INITIAL.
*                    SCREEN-INPUT = 0. "Campo Fechado
*                  ELSE.
*                    SCREEN-INPUT = 1.
*                  ENDIF.
*                ENDIF.

*              ELSE.
*              IF VALOR > 0.
*                IF OK_CODE EQ 'NOVO'.
*                  SCREEN-INPUT = 1. "Campo Aberto
*                ELSE.
*                  SCREEN-INPUT = 0. "Campo Fechado
*                ENDIF.
              ELSE.
                screen-input = 0. "Campo Fechado
*              ENDIF.
              ENDIF.
            ELSE.


              IF ( screen-name EQ 'WA_EDIT-TAXA' ) AND ( wa_edit-moeda EQ 'BRL' ).
                screen-input = 0. "Campo Fechado
              ELSE.

                IF screen-name EQ 'WA_EDIT-TAXA'.

                  IF NOT ok_ IS INITIAL.
                    screen-input = 0.
                  ELSE.
                    screen-input = 1.
                  ENDIF.

                ELSE.
                  screen-input = 1.
                ENDIF.
              ENDIF.

*              IF SCREEN-NAME EQ 'WA_EDIT-NFENUM'.
*                IF VBLOCK IS NOT INITIAL.
*                  SCREEN-INPUT = 0. "Campo Fechado
*                ELSE.
*                  SCREEN-INPUT = 1.
*                ENDIF.
*              ENDIF.

              IF screen-name EQ 'SELEC'.
                IF vblock IS NOT INITIAL.
                  screen-input = 0. "Campo Fechado
                ELSE.
                  screen-input = 1.
                ENDIF.
              ENDIF.

            ENDIF.
          ELSE.
            screen-input = 0. "Campo Fechado
          ENDIF.
          IF wa_edit IS INITIAL.
            screen-input = 0. "Campo Fechado
          ENDIF.
          MODIFY SCREEN.
        ENDIF.

*      WHEN 'NOVO'.
*
*        IF VALOR > 0.
*          SCREEN-INPUT = 1. "Campo Aberto
*        ELSE.
*          SCREEN-INPUT = 0. "Campo Fechado
*        ENDIF.
*
*        MODIFY SCREEN.
      WHEN 'OBS'.

        IF wa_edit-num_comp_adiant IS NOT INITIAL. "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt3- BG #115338
          screen-input = 0. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
**<<<------"163316 - NMS - INI------>>>
      WHEN 'WA_EDIT-DOC_CONT'.
* Verifica se Lançamento de Ajuste não está marcado.
        IF wa_edit-ajuste IS INITIAL.
          screen-input    = 0. "Campo Fechado
          screen-required = 0. "Campo Obrigatório

        ELSE.
          screen-input    = 1. "Campo Aberto
*          screen-required = 2. "Campo Obrigatório "#171608 24-03-2025 SMC--> retirado validação pq nao é necessário ser obrigatorio preenchimento qdo ajuste = X. Manter apenas validação da existencia do doc contabil caso preenchido no campo.

        ENDIF.

        MODIFY SCREEN.
**<<<------"163316 - NMS - FIM------>>>
    ENDCASE.
  ENDLOOP.

  PERFORM inat_check_vlr_total.

  block = abap_false.

ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.                 " STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0400 INPUT.

ENDMODULE.                 " USER_COMMAND_0400  INPUT


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.


  CLEAR: ver_tab.

*  IF ( WA_LANC-MONT_MOEDA_PARC NE 0 ) AND ( OK_CODE EQ 'TAB_P' ).

*    MESSAGE S888(SABAPDOCU) WITH 'Distribuição pendente, valor: ' WA_LANC-MONT_MOEDA_PARC  DISPLAY LIKE 'E'.

*  ELSE.

  CASE ok_code.
    WHEN: 'TAB_P'.
      tabstrip-activetab = 'TAB_P'.
      CLEAR: it_lanc_ver[],
             it_lanc[],
             it_edit[],
             it_editor[], "BUG - 174094 - CSB
             it_status[],
             it_status_display[],
             wa_lanc_ver,
             wa_lanc,
             wa_edit,
             wa_status,
             wa_status_display.

      ver_tab = 'X'.
    WHEN: 'TAB_S'.
      tabstrip-activetab = 'TAB_S'.
      PERFORM: seleciona_status.
    WHEN: c_back.
      LEAVE TO SCREEN 0.
    WHEN: c_cancel.
      "LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0.
      LEAVE TO SCREEN 0.
    WHEN: c_exit.
      LEAVE PROGRAM.
    WHEN: 'NOVO'.
**********************************************************************PSA
      "FI 115811 - CS2023000317 ZFIS26 - Recebimento Vendas Mercado Interno - PSA

      IF wa_saida-vbeln IS INITIAL.
        READ TABLE it_saida[] INTO wa_saida INDEX 1.
      ENDIF.

      PERFORM z_busca_dados_fatu.
      IF it_fatu IS NOT INITIAL.
        DATA: qtd_nf TYPE i.

        IF it_fatu IS NOT INITIAL.
          DESCRIBE TABLE it_fatu LINES qtd_nf.
        ENDIF.

      ENDIF.

*** BUG - 178407 - CBRAND - Inicio
      FIND 'DDL' IN wa_saida-text1.
      IF sy-subrc = 0.
        DATA(lva_ddl) = 'X'.
      ENDIF.
*** BUG - 178407 - CBRAND - FIM

*** BUG - 174094 - CBRAND - Inicio
*** BUG - 178407 - CBRAND - Inicio
      IF  ( qtd_nf > 0 AND p_mi = 'X' AND lva_ddl = 'X').
*      IF  ( qtd_nf > 0 AND p_mi = 'X' AND wa_saida-zterm = 'Z015' ).
*** BUG - 178407 - CBRAND - FIM
*      IF qtd_nf > 0.
*** BUG - 174094 - CBRAND - Fim
        DATA:lv_answer.
        CLEAR: lv_answer.
        DATA(vtextline) = |Existe { qtd_nf } Nf a vincular. Deseja realizar lançamento em massa ?|.
        DATA(vtitel) = |Lançamentos em Massa|.
        CALL FUNCTION 'POPUP_CONTINUE_YES_NO'
          EXPORTING
            textline1 = vtextline
            titel     = vtitel
          IMPORTING
            answer    = lv_answer.

        "J Sim
        "N Não

        IF lv_answer = 'J'.
          PERFORM: lancamento_massa.
        ELSE.

        ENDIF.
      ENDIF.

**********************************************************************PSA
      FREE: it_editor.
      calc = abap_false.
      edit = abap_false.
      txtopen = abap_true.
      CLEAR txtzterm.
      PERFORM: novo_lancamento.
    WHEN: 'GRAVAR'.
      block = abap_true.
      CLEAR: txtzterm, txtopen.
      PERFORM z_validacoes .
      PERFORM: gravar_lancamento.
      PERFORM f_seleciona_zfis.
    WHEN: 'CANCELAR'.
      block   = abap_true.
      CLEAR: txtzterm, txtopen.
      FREE: it_editor.
      PERFORM atua_campo.
      CLEAR: wa_edit.
      LEAVE TO SCREEN 0100.
    WHEN: 'EXECUTE'.
      block = abap_true.
      txtopen = abap_true.
      PERFORM z_validacoes.

    WHEN 'CHECK_TOT'.
      block = abap_true.
      txtopen = abap_true.
      PERFORM z_validacoes.

    WHEN 'CHECK_AJUST'.
      block = abap_true.
      txtopen = abap_true.
      PERFORM z_validacoes.
*      PERFORM Z_ATUALIZA_VALORES.
    WHEN: 'SELEC'.
      txtopen = abap_true.
      PERFORM z_busca_dados_fatu.
      CALL SCREEN 0301 STARTING AT 5 5 ENDING AT 100 20.
*** US #181883 - MMSILVA - 10.06.2025 - Ini ***
    WHEN 'LEG'.
      CALL SCREEN 6001 STARTING AT 5 5 ENDING AT 56 12.
*** US #181883 - MMSILVA - 10.06.2025 - Fim ***

  ENDCASE.
  CLEAR: lva_ddl. "BUG - 178407 - CBRAND
*  ENDIF.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

ENDMODULE.                 " USER_COMMAND_0200  INPUT


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados .

  DATA(obj_auart) = NEW zcl_taxa_curva( ).
  r_devo_recu = obj_auart->get_auart( 'ZHEDGEDEVO/RECU' ). " Get SET de AUART de Devolução/Recusa

  " Documento de vendas: dados de cabeçalho
  SELECT bukrs_vf auart kunnr vkbur vbeln erdat waerk knumv netwr
    FROM vbak
    INTO TABLE it_vbak
  WHERE bukrs_vf IN p_bukrs
    "AND auart    IN p_auart
    AND spart    IN p_spart
    AND vkbur    IN p_vkbur
    AND kunnr    IN p_kunnr
    AND vbeln    IN p_vbeln
    AND erdat    IN p_erdat.

  CHECK NOT it_vbak[] IS INITIAL.

  " Documento de vendas: dados comerciais
  SELECT vbeln zlsch kurrf valdt zterm
    FROM vbkd
    INTO TABLE it_vbkd
  FOR ALL ENTRIES IN it_vbak
    WHERE vbeln EQ it_vbak-vbeln.

  SELECT *
    FROM t052 INTO TABLE it_t052
   FOR ALL ENTRIES IN  it_vbkd
    WHERE zterm EQ it_vbkd-zterm.


  " Dados de Taxa Travada
  CLEAR: it_zsdt0090.
  SELECT *
    FROM zsdt0090
    INTO TABLE it_zsdt0090
    FOR ALL ENTRIES IN it_vbak
    WHERE vbelv EQ it_vbak-vbeln
    AND estorno   NE 'X'. "NAO~TRAZER ESTORNO

  IF it_zsdt0090[] IS NOT INITIAL.

    SELECT *
      FROM zsdt0040 INTO TABLE it_zsdt0040
      FOR ALL ENTRIES IN it_zsdt0090
     WHERE doc_simulacao EQ it_zsdt0090-doc_simulacao.

  ELSE.

    SELECT *
      FROM zsdt0041 INTO TABLE it_zsdt0041
      FOR ALL ENTRIES IN it_vbak
      WHERE vbeln EQ it_vbak-vbeln.

    IF it_zsdt0041[] IS NOT INITIAL.

      SELECT *
        FROM zsdt0040 INTO TABLE it_zsdt0040
        FOR ALL ENTRIES IN it_zsdt0041
       WHERE doc_simulacao EQ it_zsdt0041-doc_simulacao.

    ENDIF.
  ENDIF.


  " Explicações próprias relativas às condições de pagamento
  SELECT  spras zterm text1
    FROM t052u
    INTO TABLE it_t052u
   FOR ALL ENTRIES IN it_vbkd
   WHERE zterm  EQ it_vbkd-zterm
    AND spras   = 'PT'.

  " Mestre de clientes (parte geral)
  SELECT kunnr name1
    FROM kna1
    INTO TABLE it_kna1
  FOR ALL ENTRIES IN it_vbak
    WHERE kunnr EQ it_vbak-kunnr.

  SELECT vbeln posnr werks netwr mwsbp
   FROM vbap
    INTO TABLE it_vbap
  FOR ALL ENTRIES IN it_vbak
    WHERE vbeln EQ it_vbak-vbeln
       AND NOT EXISTS ( SELECT *
                         FROM vbep
                        WHERE vbeln EQ vbap~vbeln
                          AND posnr EQ vbap~posnr
                          AND lifsp EQ '12' ).

  PERFORM: saida_grid.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  SAIDA_GRID
*&---------------------------------------------------------------------*
FORM saida_grid .

  SORT: it_vbkd BY vbeln,
        it_kna1 BY kunnr,
        it_vbap BY vbeln.


  CALL FUNCTION 'DDIF_DOMA_GET'
    EXPORTING
      name          = 'ZSDD023'
      state         = 'A'
      langu         = sy-langu
    TABLES
      dd07v_tab     = it_dd07v_tab
    EXCEPTIONS
      illegal_input = 1
      OTHERS        = 2.


  LOOP AT it_vbak INTO wa_vbak.

    SELECT SINGLE * FROM zfit0026 INTO @DATA(wa_zfit0026) WHERE vbeln EQ @wa_vbak-vbeln AND status IN ('E', 'S').
    IF sy-subrc IS INITIAL.
      wa_saida-status = icon_led_red.
    ENDIF.


    wa_saida-visual   =  icon_display_text.
    wa_saida-bukrs_vf =  wa_vbak-bukrs_vf.
    wa_saida-auart    =  wa_vbak-auart.
    wa_saida-kunnr    =  wa_vbak-kunnr.
    wa_saida-vkbur    =  wa_vbak-vkbur.
    wa_saida-vbeln    =  wa_vbak-vbeln.
    wa_saida-erdat    =  wa_vbak-erdat.
    wa_saida-waerk    =  wa_vbak-waerk.
    wa_saida-knumv    =  wa_vbak-knumv.

    READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
    wa_saida-zlsch = wa_vbkd-zlsch.
*    WA_SAIDA-KURRF = WA_VBKD-KURRF.
    wa_saida-zterm = wa_vbkd-zterm.

    READ TABLE it_t052 INTO wa_t052 WITH KEY zterm = wa_vbkd-zterm.
    IF wa_t052-zdart = 'D'.
      wa_saida-valdt = wa_vbkd-valdt.
    ENDIF.

    READ TABLE it_zsdt0090 INTO wa_zsdt0090 WITH KEY  vbelv = wa_vbak-vbeln.
    IF sy-subrc = 0.

      READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0090-doc_simulacao.

    ELSE.

      READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_vbak-vbeln.

      IF sy-subrc = 0.
        READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao.

      ENDIF.
    ENDIF.


    IF p_ins IS NOT INITIAL.

      READ TABLE it_dd07v_tab INTO wa_dd07v WITH KEY domvalue_l = wa_zsdt0040-meio_pago.
      IF sy-subrc = 0.
        wa_saida-meio_pgmto = wa_dd07v-ddtext.
      ENDIF.

      READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_vbak-vbeln.

      IF sy-subrc = 0.
        READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao.
        IF sy-subrc EQ 0.
          "PAGAMENTO ANTECIPADO
          CASE wa_zsdt0040-meio_pago .
            WHEN 'D' .
              wa_saida-pgto_ant = 'Deposito em Conta'.
            WHEN 'A' .
              wa_saida-pgto_ant = 'Acerto'.
            WHEN 'B' .
              wa_saida-pgto_ant = 'Boleto Bancário'.
            WHEN ' ' .
              wa_saida-pgto_ant = 'Não Atencipado'.
          ENDCASE.
        ENDIF.
      ENDIF.
    ELSE.

      SELECT SINGLE *  FROM t042zt INTO @DATA(wa_t042zt)
      WHERE spras EQ @sy-langu
        AND zlsch EQ @wa_vbkd-zlsch.

      wa_saida-meio_pgmto =   wa_t042zt-text2.

      SELECT SINGLE *
      FROM zsdt0053 INTO @DATA(wa_zsdt0053)
      WHERE vbeln = @vbeln_aux.

      "PAGAMENTO ANTECIPADO
      SELECT SINGLE *
        FROM zsdt0052 INTO @DATA(wa_zsdt0052)
        WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.

      CASE wa_zsdt0052-pgto_ant .

        WHEN 'X' .
          wa_saida-pgto_ant = ' Com Boleto '.

        WHEN 'N' .
          wa_saida-pgto_ant = ' Sem Boleto '.

        WHEN ' ' .
          wa_saida-pgto_ant = ' Não Antecipado '.
      ENDCASE.

    ENDIF.


    READ TABLE it_zsdt0090 INTO wa_zsdt0090 WITH KEY vbelv     = wa_vbak-vbeln
                                                     categoria = 'C'
                                                     estorno   = space.
    IF sy-subrc IS INITIAL.
      wa_saida-kurrf = wa_zsdt0090-kurrf.
    ELSE.
      wa_saida-kurrf = wa_vbkd-kurrf.
    ENDIF.


    READ TABLE it_t052u INTO wa_t052u WITH KEY zterm = wa_vbkd-zterm
                                               spras = 'PT'.
    wa_saida-text1  = wa_t052u-text1.


    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
    wa_saida-name1 = wa_kna1-name1.

*    READ TABLE IT_VBAP INTO WA_VBAP WITH KEY VBELN = WA_VBAK-VBELN BINARY SEARCH.

    LOOP AT it_vbap INTO wa_vbap WHERE vbeln EQ wa_vbak-vbeln.

      wa_saida-werks = wa_vbap-werks.

*      IF WA_VBAK-AUART IN R_DEVO_RECU.
*        MULTIPLY WA_VBAP-NETWR BY -1.
*        MULTIPLY WA_VBAP-MWSBP BY -1.
*      ENDIF.

      ADD wa_vbap-netwr TO wa_saida-netwr.
      ADD wa_vbap-mwsbp TO wa_saida-netwr.

      ADD wa_vbap-netwr TO wa_saida-netwr_l.
      ADD wa_vbap-mwsbp TO wa_saida-mwsbp.
    ENDLOOP.

    APPEND wa_saida TO it_saida.

    CLEAR: wa_vbak, wa_vbkd, wa_kna1, wa_vbap, wa_t052u, wa_saida, wa_t052.

  ENDLOOP.

ENDFORM.                    " SAIDA_GRID
*&---------------------------------------------------------------------*
*&      CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no,
      handle_user_command  FOR EVENT user_command  OF cl_gui_alv_grid IMPORTING e_ucomm,
      handle_toolbar       FOR EVENT toolbar       OF cl_gui_alv_grid IMPORTING e_object e_interactive.


ENDCLASS.                    "lcl_event_handler DEFINITION

*&---------------------------------------------------------------------*
*&      CLASS IMPLMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no.

  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_user_command.
    PERFORM handle_user_command USING e_ucomm .

  ENDMETHOD.                    "handle_user_command


  METHOD handle_toolbar.
    PERFORM handle_toolbar USING e_object e_interactive.

  ENDMETHOD.                    "HANDLE_TOOLBAR

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM handle_hotspot_click  USING    i_row_id     TYPE lvc_s_row
                                    i_column_id  TYPE lvc_s_col
                                    is_row_no    TYPE lvc_s_roid.
  CASE i_column_id.

    WHEN: 'VISUAL'.
      block = abap_true.
*      READ TABLE IT_SAIDA INTO WA_SAIDA INDEX IS_ROW_NO-ROW_ID.
*      PERFORM CHECK_BLOQ_ORDEM USING WA_SAIDA-VBELN CHANGING SY-SUBRC.
*      IF SY-SUBRC EQ 0.
      PERFORM: visualizar_lancamento  USING is_row_no-row_id.
*      ELSE.
**        MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH 'Ordem 'WA_SAIDA-VBELN ' Bloqueada pelo usuario ' sy-.
*      ENDIF.
    WHEN: 'EDIT'.
*      IF WA_T052-ZDART NE 'B'.
*        BLOCK = ABAP_TRUE.
*      ENDIF.

      calc = abap_true.
      edit = abap_true.
      txtopen = abap_true.
      PERFORM: editar_lancamento           USING is_row_no-row_id.
    WHEN: 'EXCLUIR'.
      block = abap_true.
      PERFORM: excluir_lancamento          USING is_row_no-row_id.
    WHEN: 'GERA'.
      block = abap_true.
      PERFORM: gerar_documento_contabil    USING is_row_no-row_id.
      PERFORM: select_data_refresh.
    WHEN: 'ESTOR'.
      PERFORM: estorno_lancamento_contabil USING is_row_no-row_id.
*"// INICIO WBARBOSA 11-12-2024 US-115811
    WHEN: 'STATUS_DOC'.
      PERFORM: f_exibir_erro USING is_row_no-row_id.
*"// FIM WBARBOSA 11-12-2024 US-115811
    WHEN: 'VBELN'.
      PERFORM: call_transaction_va03       USING is_row_no-row_id.
    WHEN: 'DOCNUM'.
      PERFORM: call_transaction_fb03       USING is_row_no-row_id.
    WHEN: 'STATUS_DOC'.
      PERFORM: reinicia_erro               USING is_row_no-row_id.

  ENDCASE.
ENDFORM.                    " HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
FORM handle_user_command  USING    p_e_ucomm TYPE syucomm.

  CASE  p_e_ucomm.
    WHEN: 'ATUALIZAR'.
      PERFORM refresh_data_lancamento.
    WHEN: 'NOVO'.
      PERFORM z_busca_dados_fatu.

      DATA: lt_return TYPE TABLE OF ddshretval.
      DATA: ls_return TYPE ddshretval.

      IF it_fatu IS NOT INITIAL.

        DATA: it_popup_fatu TYPE TABLE OF ty_fatu.

        "DELETE it_popup_fatu WHERE
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'NFENUM'
            value_org       = 'S'
          TABLES
            value_tab       = it_fatu
            return_tab      = lt_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF lt_return IS NOT INITIAL.

          READ TABLE lt_return INTO ls_return INDEX 1.
          READ TABLE it_fatu INTO wa_fatu WITH KEY nfenum = ls_return-fieldval.
          CLEAR: wa_vbak.

          READ TABLE it_vbak INTO wa_vbak .

          CLEAR: wa_lanc.

          wa_lanc-vbeln = wa_vbak-vbeln.
          wa_lanc-bukrs_vf = wa_vbak-bukrs_vf.

          FREE: it_editor.
          calc = abap_false.
          edit = abap_false.
          txtopen = abap_true.
          CLEAR txtzterm.

          DATA: soma     TYPE zfit0026-mont_moeda,
                c_vbeln  TYPE zfit0026-vbeln,
                c_bukrs  TYPE zfit0026-bukrs,
                texto_01 TYPE c LENGTH 100 VALUE 'Valor do Montante menor do',
                texto_02 TYPE c LENGTH 100 VALUE 'que a soma total dos lançamentos'.

          c_vbeln = wa_lanc-vbeln.
          c_bukrs = wa_lanc-bukrs_vf.

*  CLEAR: WA_LANC.
** Inicio de alteração - CS0982743 - FMartins - 25/04/2022
*    clear:  wa_lanc-doc_fatura, wa_edit-doc_fatura.
** Fim de alteração - CS0982743 - FMartins - 25/04/2022

          CLEAR: it_lanc_ver[], wa_lanc_ver.

          SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag
                 status uname data_registro bukrs obj_key docnum zterm doc_fatura
                 data_pgto vlr_multa_rbdo vlr_juros_rbdo mont_rbdo vlr_multa_calc
                 vlr_juros_calc observacao ajuste rec_vlr_total vlr_desc_mult vlr_desc_jros num_comp_adiant
            FROM zfit0026
            INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
          WHERE vbeln EQ  c_vbeln.

          LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln = c_vbeln.
            soma = soma + wa_lanc_ver-mont_moeda.
          ENDLOOP.

*  READ TABLE IT_LANC INTO WA_LANC WITH KEY VBELN = C_VBELN.
          PERFORM preenche_edit.

          IF wa_saida-auart IN r_devo_recu.
            wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - abs( soma ).
            MULTIPLY wa_lanc-mont_moeda_parc BY -1.
          ELSE.
            wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - soma.
          ENDIF.

          wa_lanc-observacao = ''.

          SELECT SINGLE * FROM vbkd INTO @DATA(wa_vbkd)
           WHERE vbeln EQ @c_vbeln.

          IF  wa_vbkd IS NOT INITIAL.

            SELECT SINGLE *  FROM t052 INTO @DATA(wa_t052)
             WHERE zterm EQ  @wa_vbkd-zterm.

            CLEAR: vblock.
            IF wa_t052-zdart NE 'B'.
              vblock = abap_true.
            ENDIF.
          ENDIF.


***** CS2016001313>>>>>>>>
*  IF ( SOMA >= WA_LANC-MONT_MOEDA_FIX ).
***** CS2016001313<<<<<<<<
          "IF ( SOMA > WA_LANC-MONT_MOEDA_FIX ).
          " MESSAGE E888(SABAPDOCU) WITH TEXTO_01 TEXTO_02.
          "ELSE.
          wa_edit-vbeln = wa_lanc-vbeln.
          wa_edit-moeda = wa_lanc-moeda.
          wa_edit-bukrs = c_bukrs.
          wa_edit-status = 'N'.

        ENDIF.

      ENDIF.

    WHEN 'ESTORNAR_MASSA' OR 'GERAR_MASSA' OR 'DELETAR_MASSA'. "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA

      FREE: tg_selectedrow.
      DATA: ls_row TYPE i.
      CLEAR: ls_row.

      CALL METHOD grid_lancamento->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow[].

      IF tg_selectedrow[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!'    TYPE 'S'.
        EXIT.
      ELSE.

        block = abap_true.
        sy-ucomm = p_e_ucomm.

        IF sy-ucomm = 'ESTORNAR_MASSA'.
          LOOP AT tg_selectedrow[] ASSIGNING FIELD-SYMBOL(<_estornar_row>).
            CLEAR: ls_row.
            CONDENSE <_estornar_row>-index NO-GAPS.
            ls_row = <_estornar_row>-index.
            PERFORM: estorno_lancamento_contabil USING ls_row.
          ENDLOOP.
        ELSEIF sy-ucomm = 'GERAR_MASSA' .
          LOOP AT tg_selectedrow[] ASSIGNING FIELD-SYMBOL(<_gerar_row>).
            CLEAR: ls_row.
            ls_row = <_gerar_row>-index.
            CONDENSE <_gerar_row>-index NO-GAPS.
            PERFORM: gerar_documento_contabil    USING ls_row.
          ENDLOOP.
        ELSEIF sy-ucomm = 'DELETAR_MASSA' .
          LOOP AT tg_selectedrow[] ASSIGNING FIELD-SYMBOL(<_deletar_row>).
            CLEAR: ls_row.
            CONDENSE <_deletar_row>-index NO-GAPS.
            ls_row = <_deletar_row>-index.
            PERFORM: excluir_lancamento USING ls_row.
          ENDLOOP.
          DELETE it_lanc[] WHERE mass_flag = 'X'.
        ENDIF.
      ENDIF.
      PERFORM refresh_data_lancamento.

      IF it_lanc[] IS INITIAL.
        LEAVE TO SCREEN 0.
      ENDIF.

  ENDCASE.

ENDFORM.                    " HANDLE_USER_COMMAND


*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*

FORM handle_toolbar  USING    e_object TYPE REF TO cl_alv_event_toolbar_set
                              e_interactive TYPE char1.


  CONSTANTS: c_separador TYPE i VALUE 3.

  DATA: sl_toolbar TYPE stb_button.

  CLEAR: sl_toolbar.
  MOVE c_separador TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO e_object->mt_toolbar.

  CLEAR: sl_toolbar.
  MOVE: 'ATUALIZAR'  TO sl_toolbar-function,
        icon_refresh TO sl_toolbar-icon,
        'Atualizar'  TO sl_toolbar-quickinfo,
        'Atualizar'  TO sl_toolbar-text,
        space        TO sl_toolbar-disabled.
  APPEND sl_toolbar TO e_object->mt_toolbar.

  CLEAR: sl_toolbar.
  MOVE: 'GERAR_MASSA'  TO sl_toolbar-function,
        icon_activity TO sl_toolbar-icon,
        'Gerar'  TO sl_toolbar-quickinfo,
        'Gerar'  TO sl_toolbar-text,
        space        TO sl_toolbar-disabled.
  APPEND sl_toolbar TO e_object->mt_toolbar.

  CLEAR: sl_toolbar.
  MOVE: 'ESTORNAR_MASSA'  TO sl_toolbar-function,
        icon_reject TO sl_toolbar-icon,
        'Estornar'  TO sl_toolbar-quickinfo,
        'Estornar'  TO sl_toolbar-text,
        space        TO sl_toolbar-disabled.
  APPEND sl_toolbar TO e_object->mt_toolbar.

  CLEAR: sl_toolbar.
  MOVE: 'DELETAR_MASSA'  TO sl_toolbar-function,
        icon_delete TO sl_toolbar-icon,
        'Deletar'  TO sl_toolbar-quickinfo,
        'Deletar'  TO sl_toolbar-text,
        space        TO sl_toolbar-disabled.
  APPEND sl_toolbar TO e_object->mt_toolbar.

ENDFORM.                    " HANDLE_TOOLBAR

*----------------------------------------------------------------------*
*       CLASS lcl_timer DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_timer DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_finished FOR EVENT finished OF cl_gui_timer.
ENDCLASS.                    "lcl_receiver DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_timer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_timer IMPLEMENTATION.
  METHOD handle_finished.
    PERFORM refresh_data_lancamento.
    MESSAGE s003(z001) WITH 'Relatório atualizado' sy-uzeit.
    CALL METHOD cl_timer_l->run.
  ENDMETHOD.                    "handle_finished
ENDCLASS.                    "lcl_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_LANCAMENTO
*&---------------------------------------------------------------------*
FORM visualizar_lancamento  USING  p_is_row_no.

  DATA: tl_enq     TYPE TABLE OF seqg3 WITH HEADER LINE,
        wl_num_enq TYPE sy-tabix,
        wl_arg     TYPE seqg3-garg,
        coruf      TYPE coruf,
        tco01      TYPE tco01,
        aufnr      TYPE seqg3-garg.

  CLEAR sy-subrc.

  DATA: cont      TYPE numc10,
        soma_parc TYPE zfit0026-mont_moeda.

  DATA: it_zib_contabil_chv TYPE STANDARD TABLE OF zib_contabil_chv,
        wa_zib_contabil_chv TYPE zib_contabil_chv,
        it_z0159            TYPE STANDARD TABLE OF zsdt0159,
        wa_z0159            TYPE zsdt0159.

  READ TABLE it_saida INTO wa_saida INDEX p_is_row_no.

****  Verificar bloqueio.
*  WL_ARG  = |{ SY-MANDT }{ V_ORDEM }|.
*  V_ORDEM = |{ V_ORDEM ALPHA = IN }|.
*  TCO01-AUTYP = '30'.
  coruf-aufnr = |{ wa_saida-vbeln ALPHA = IN }|.

  CALL FUNCTION 'CO_RU_ORDER_LOCK'
    EXPORTING
      aufnr_imp            = coruf-aufnr
*     AUTYP_IMP            = TCO01-AUTYP
    EXCEPTIONS
      order_already_locked = 1.

  IF sy-subrc NE 0.



    MESSAGE | Ordem | && coruf-aufnr && | bloqueada pelo usuario | && sy-msgv2 TYPE 'I' DISPLAY LIKE 'E'.

*    MESSAGE E469(CO) DISPLAY LIKE 'I' WITH CORUF-AUFNR SY-MSGV2
*                     RAISING ORDER_ALREADY_LOCKED.
    EXIT.
  ENDIF.
*****

  vbeln_aux = wa_saida-vbeln.

  SELECT COUNT(*)
    INTO cont
    FROM zfit0026
  WHERE vbeln EQ wa_saida-vbeln.

  IF cont NE 0.
****  Verificar se o docnum foi estornado. (AOENNING)
    check_estor_docnum=>check_estorn_docnum( EXPORTING i_vbeln = wa_saida-vbeln ).
  ENDIF.

  SELECT vbeln seq data_venc moeda mont_moeda
         taxa mont_mi forma_pag status uname
         data_registro bukrs obj_key docnum
         zterm  doc_fatura data_pgto
         vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo
         vlr_multa_calc vlr_juros_calc
         razao_especial  observacao ajuste rec_vlr_total vlr_desc_mult vlr_desc_jros num_comp_adiant
    FROM zfit0026
    INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
  WHERE vbeln EQ  wa_saida-vbeln.

  SORT: it_lanc_ver BY seq.
  IF ( ( sy-subrc NE 0 ) AND ( cont EQ 0 ) ) .

    CLEAR: wa_lanc.

    wa_lanc-edit          = icon_change.
    wa_lanc-excluir       = icon_delete.
    wa_lanc-gera          = icon_activity.
    wa_lanc-estor         = icon_reject.
    wa_lanc-vbeln         = wa_saida-vbeln.
    wa_lanc-seq           = cont + 1.
    wa_lanc-data_venc     = wa_saida-valdt.
    wa_lanc-moeda         = wa_saida-waerk.
    wa_lanc-mont_moeda    = wa_saida-netwr.
    wa_lanc-zterm         = wa_saida-zterm.
    wa_lanc-bukrs_vf      = wa_saida-bukrs_vf.
*    WA_LANC-PGTO_ANT     =

    IF ( wa_lanc-moeda EQ 'BRL' ).
      wa_lanc-taxa = 1.
      wa_lanc-mont_mi    = wa_saida-netwr * wa_lanc-taxa.
    ELSE.
      wa_lanc-taxa          = wa_saida-kurrf.
      wa_lanc-mont_mi    = wa_saida-netwr * wa_lanc-taxa.
    ENDIF.

    wa_lanc-forma_pag     = wa_saida-zlsch.

    CASE wa_lanc_ver-status.
      WHEN: 'P'.
        wa_lanc-status_doc = icon_generate.
      WHEN: 'A'.
        wa_lanc-status_doc = icon_budget_update.
      WHEN: 'S'.
        wa_lanc-status_doc = icon_alarm.
      WHEN OTHERS.
        wa_lanc-status_doc = icon_led_yellow.
    ENDCASE.

    wa_lanc-uname         = sy-uname.
    wa_lanc-data_registro = sy-datum.

    wa_lanc-mont_moeda_fix  = wa_saida-netwr.

    IF p_ins IS NOT INITIAL.

      READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_saida-vbeln.

      IF sy-subrc = 0.
        READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao.
        IF sy-subrc EQ 0.
          "PAGAMENTO ANTECIPADO
          CASE wa_zsdt0040-meio_pago .
            WHEN 'D' .
              wa_lanc-pgto_ant = 'Deposito em Conta'.
            WHEN 'A' .
              wa_lanc-pgto_ant = 'Acerto'.
            WHEN 'B' .
              wa_lanc-pgto_ant = 'Boleto Bancário'.
            WHEN ' ' .
              wa_lanc-pgto_ant = 'Não Atencipado'.
          ENDCASE.
        ENDIF.
      ENDIF.
    ELSE.


      SELECT SINGLE *
      FROM zsdt0053 INTO @DATA(wa_zsdt0053)
      WHERE vbeln = @wa_saida-vbeln.

      "PAGAMENTO ANTECIPADO
      SELECT SINGLE *
        FROM zsdt0052 INTO @DATA(wa_zsdt0052)
        WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.

      CASE wa_zsdt0052-pgto_ant .

        WHEN 'X' .
          wa_lanc-pgto_ant = ' Com Boleto '.

        WHEN 'N' .
          wa_lanc-pgto_ant = ' Sem Boleto '.

        WHEN ' ' .
          wa_lanc-pgto_ant = ' Não Antecipado '.
      ENDCASE.

    ENDIF.


*** insere a OBS na caixa
    wa_editor-line = wa_lanc-observacao.
    APPEND wa_editor TO  it_editor.
    CALL METHOD editor->set_text_as_stream( EXPORTING text = it_editor ).

    PERFORM preenche_edit.

*    APPEND WA_LANC TO IT_LANC.

  ELSE.

    SELECT *
      FROM zib_contabil_chv
      INTO TABLE it_zib_contabil_chv
      FOR ALL ENTRIES IN it_lanc_ver
      WHERE obj_key EQ it_lanc_ver-obj_key
      AND NOT EXISTS ( SELECT *
                         FROM bkpf
                        WHERE belnr EQ zib_contabil_chv~belnr
                          AND bukrs EQ zib_contabil_chv~bukrs
                          AND gjahr EQ zib_contabil_chv~gjahr
                          AND stblg NE space ).

*"// INICIO WBARBOSA 11-12-2024 US-115811
    SELECT *
      FROM zib_contabil_err
      INTO CORRESPONDING FIELDS OF TABLE t_zib_contabil_err
      FOR ALL ENTRIES IN it_lanc_ver
      WHERE obj_key EQ it_lanc_ver-obj_key.
*"// FIM WBARBOSA 11-12-2024 US-115811

    SELECT *
      FROM zsdt0159
      INTO TABLE it_z0159
      FOR ALL ENTRIES IN it_lanc_ver
      WHERE obj_key EQ it_lanc_ver-obj_key.

    LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln = wa_saida-vbeln.

      SELECT SINGLE * FROM j_1bnflin INTO @DATA(wa_lin)
       WHERE refkey  EQ @wa_lanc_ver-doc_fatura.

      SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(wa_doc)
       WHERE docnum EQ @wa_lin-docnum.


      wa_lanc-nfenum         = wa_doc-nfenum.
      wa_lanc-edit           = icon_change.
      wa_lanc-excluir        = icon_delete.
      IF wa_lanc_ver-ajuste NE 'X'.
        wa_lanc-gera           = icon_activity.
        wa_lanc-estor          = icon_reject.
      ELSE.
        CLEAR: wa_lanc-gera, wa_lanc-estor.
      ENDIF.
      wa_lanc-vbeln           = wa_lanc_ver-vbeln.
      wa_lanc-seq             = wa_lanc_ver-seq.
      wa_lanc-data_venc       = wa_lanc_ver-data_venc."wa_saida-valdt ."WA_LANC_VER-DATA_VENC. "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA
      wa_lanc-moeda           = wa_lanc_ver-moeda.
      wa_lanc-mont_moeda      = wa_lanc_ver-mont_moeda.
      wa_lanc-taxa            = wa_lanc_ver-taxa.
      wa_lanc-mont_mi         = wa_lanc_ver-mont_mi.
      wa_lanc-forma_pag       = wa_lanc_ver-forma_pag.
      wa_lanc-status          = wa_lanc_ver-status.
      wa_lanc-uname           = wa_lanc_ver-uname.
      wa_lanc-data_registro   = wa_lanc_ver-data_registro.
      wa_lanc-ajuste          = wa_lanc_ver-ajuste.

      IF wa_lanc_ver-ajuste IS NOT INITIAL.
        wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_desc_mult.
        wa_lanc-vlr_juros_rbdo  = wa_lanc_ver-vlr_desc_jros.
      ELSE.
        wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_multa_rbdo.
        wa_lanc-vlr_juros_rbdo  = wa_lanc_ver-vlr_juros_rbdo.
      ENDIF.

      wa_lanc-data_pgto       = wa_lanc_ver-data_pgto.
      wa_lanc-vlr_multa_calc  = wa_lanc_ver-vlr_multa_calc.
      wa_lanc-vlr_juros_calc  = wa_lanc_ver-vlr_juros_calc.
      wa_lanc-mont_rbdo       = wa_lanc_ver-mont_rbdo.
      wa_lanc-doc_fatura      = wa_lanc_ver-doc_fatura.
      wa_lanc-rec_vlr_total   = wa_lanc_ver-rec_vlr_total.

      IF p_ins IS NOT INITIAL.

        READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_lanc_ver-vbeln.

        IF sy-subrc = 0.
          READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao.
          IF sy-subrc EQ 0.
            "PAGAMENTO ANTECIPADO
            CASE wa_zsdt0040-meio_pago .
              WHEN 'D' .
                wa_lanc-pgto_ant = 'Deposito em Conta'.
              WHEN 'A' .
                wa_lanc-pgto_ant = 'Acerto'.
              WHEN 'B' .
                wa_lanc-pgto_ant = 'Boleto Bancário'.
              WHEN ' ' .
                wa_lanc-pgto_ant = 'Não Atencipado'.
            ENDCASE.
          ENDIF.
        ENDIF.
      ELSE.

        CLEAR: wa_zsdt0053.
        SELECT SINGLE *
        FROM zsdt0053 INTO wa_zsdt0053
        WHERE vbeln = wa_lanc_ver-vbeln.

        "PAGAMENTO ANTECIPADO
        CLEAR: wa_zsdt0052.
        SELECT SINGLE *
          FROM zsdt0052 INTO wa_zsdt0052
          WHERE nro_sol_ov = wa_zsdt0053-nro_sol_ov.

        CASE wa_zsdt0052-pgto_ant .

          WHEN 'X' .
            wa_lanc-pgto_ant = ' Com Boleto '.

          WHEN 'N' .
            wa_lanc-pgto_ant = ' Sem Boleto '.

          WHEN ' ' .
            wa_lanc-pgto_ant = ' Não Antecipado '.
        ENDCASE.

      ENDIF.

******Alteração CS2017000894 Início

      "WA_LANC-DOCNUM         = WA_LANC_VER-DOCNUM.
      READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_lanc_ver-obj_key.

      IF sy-subrc IS INITIAL.
        wa_lanc-docnum       = wa_zib_contabil_chv-belnr.
        "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt3- BG #115338 - fim
        SELECT SINGLE * FROM zfit0026 INTO @DATA(wa_zfit0026) WHERE  obj_key EQ @wa_zib_contabil_chv-obj_key.
        IF  sy-subrc IS INITIAL.
          IF wa_zfit0026-objkey_sigam IS NOT INITIAL AND wa_zfit0026-docnum_forn IS INITIAL .
            wa_lanc_ver-status = 'S'.
            wa_lanc-status = wa_lanc_ver-status.
          ELSE.
            wa_lanc_ver-status = 'G'.
            wa_lanc-status = wa_lanc_ver-status.
          ENDIF.
        ENDIF.
        "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt3- BG #115338 - FIM

      ELSE.
        "IR167273 - BG - 19/08/2024
        IF NOT wa_lanc_ver-obj_key IS INITIAL.           "<< RIM-SKM-IR118149-12.05.2022
          READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_lanc_ver-obj_key.
          IF sy-subrc IS INITIAL.
            wa_lanc-docnum         = wa_z0159-adiant.
          ELSE.
**<<<------"163316 - NMS - INI------>>>
* Verifica se Lançamento de Ajuste não está marcado.
            IF wa_lanc-ajuste IS INITIAL.
**<<<------"163316 - NMS - FIM------>>>
              CLEAR: wa_lanc-docnum.
**<<<------"163316 - NMS - INI------>>>
            ELSE.
              wa_lanc-docnum = wa_lanc_ver-docnum.

            ENDIF.
**<<<------"163316 - NMS - FIM------>>>
          ENDIF.
        ELSE.                                            "<< RIM-SKM-IR118149-12.05.2022
          wa_lanc-docnum         = wa_lanc_ver-docnum.   "<< RIM-SKM-IR118149-12.05.2022
        ENDIF.                                         "<< RIM-SKM-IR118149-12.05.2022
      ENDIF.

******Alteração CS2017000894 Fim

      wa_lanc-razao_especial = wa_lanc_ver-razao_especial.
*-BUG 49186 - inicio
      IF wa_lanc_ver-bukrs IS NOT INITIAL.
        wa_lanc-bukrs_vf       = wa_lanc_ver-bukrs.
      ELSE.
        wa_lanc-bukrs_vf       = wa_saida-bukrs_vf.
      ENDIF.
*     wa_lanc-bukrs_vf       = wa_saida-bukrs_vf.
*-BUG 49186 - fim

      wa_lanc-zterm          = wa_lanc_ver-zterm. "WA_SAIDA-ZTERM.
      wa_lanc-observacao     = wa_lanc_ver-observacao.

      CASE wa_lanc-razao_especial.
        WHEN: 'G'.
          wa_lanc-razao = 'Acerto'.
        WHEN: 'L'.
          wa_lanc-razao = 'Depósito'.
      ENDCASE.


      CASE wa_lanc_ver-status.
        WHEN: 'P'.
          wa_lanc-status_doc = icon_generate.
        WHEN: 'E'.
          wa_lanc-status_doc = icon_led_red.
        WHEN: 'G'.
          wa_lanc-status_doc = icon_led_green.
        WHEN: 'X'.
          wa_lanc-status_doc = icon_booking_stop.
        WHEN: 'A'.
          wa_lanc-status_doc = icon_budget_update.
        WHEN: 'S'.
          wa_lanc-status_doc = icon_alarm.
        WHEN OTHERS.
          wa_lanc-status_doc = icon_led_yellow.
      ENDCASE.

*"// INICIO WBARBOSA 11-12-2024 US-115811
      READ TABLE t_zib_contabil_err INTO DATA(ls_zib_contabil_err) WITH KEY obj_key = wa_lanc_ver-obj_key.
      IF sy-subrc IS INITIAL.
        wa_lanc-status_doc = icon_led_red.
      ENDIF.
*"// FIM 11-12-2024 US-115811

      wa_lanc-mont_moeda_fix  = wa_saida-netwr.

      soma_parc = soma_parc + wa_lanc_ver-mont_moeda.

      APPEND wa_lanc TO it_lanc.

    ENDLOOP.


*** BUG - 174094 - CBRAND - Inicio
*** insere a OBS na caixa
    wa_editor-line = wa_lanc-observacao.
    APPEND wa_editor TO  it_editor.
    CALL METHOD editor->set_text_as_stream( EXPORTING text = it_editor ).
*** BUG - 174094 - CBRAND - Fim

    PERFORM preenche_edit.

    IF wa_saida-auart IN r_devo_recu.
      wa_lanc-mont_moeda_parc = wa_saida-netwr - abs( soma_parc ).
      MULTIPLY wa_lanc-mont_moeda_parc BY -1.
    ELSE.
      wa_lanc-mont_moeda_parc = wa_saida-netwr - soma_parc.
    ENDIF.

    CLEAR: soma_parc.

  ENDIF.

***** CS2016001313>>>>>>>>
***** REMOVER REGRA DE MONT_MOEDA INFERIOR A ZERO
*  IF ( WA_LANC-MONT_MOEDA_PARC < 0 ).
*    MESSAGE E888(SABAPDOCU) WITH 'Número da OV menor do que o montante'.
*  ELSE.

*  IF NOT ( IT_LANC[] IS INITIAL ).

  FREE: it_fieldcatalog.

  PERFORM: display_catalog_lancamento.
  PERFORM: create_container_lancamento USING wa_saida-vbeln.

  LEAVE TO SCREEN 0100.

  CLEAR wa_edit.


*  ENDIF.
***** CS2016001313<<<<<<<<
ENDFORM.                    " VISUALIZAR_LANCAMENTO
*&---------------------------------------------------------------------*
*&      Form  NOVO_LANCAMENTO
*&---------------------------------------------------------------------*
FORM novo_lancamento.

  DATA: soma     TYPE zfit0026-mont_moeda,
        c_vbeln  TYPE zfit0026-vbeln,
        c_bukrs  TYPE zfit0026-bukrs,
        texto_01 TYPE c LENGTH 100 VALUE 'Valor do Montante menor do',
        texto_02 TYPE c LENGTH 100 VALUE 'que a soma total dos lançamentos'.

  c_vbeln = wa_lanc-vbeln.
  c_bukrs = wa_lanc-bukrs_vf.

*  CLEAR: WA_LANC.
** Inicio de alteração - CS0982743 - FMartins - 25/04/2022
*    clear:  wa_lanc-doc_fatura, wa_edit-doc_fatura.
** Fim de alteração - CS0982743 - FMartins - 25/04/2022

  CLEAR: it_lanc_ver[], wa_lanc_ver.

  SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag
         status uname data_registro bukrs obj_key docnum zterm doc_fatura
         data_pgto vlr_multa_rbdo vlr_juros_rbdo mont_rbdo vlr_multa_calc
         vlr_juros_calc observacao ajuste rec_vlr_total vlr_desc_mult vlr_desc_jros num_comp_adiant
    FROM zfit0026
    INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
  WHERE vbeln EQ  c_vbeln.

  LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln = c_vbeln.
    soma = soma + wa_lanc_ver-mont_moeda.
  ENDLOOP.

*  READ TABLE IT_LANC INTO WA_LANC WITH KEY VBELN = C_VBELN.
  PERFORM preenche_edit.

  IF wa_saida-auart IN r_devo_recu.
    wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - abs( soma ).
    MULTIPLY wa_lanc-mont_moeda_parc BY -1.
  ELSE.
    wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - soma.
  ENDIF.

  wa_lanc-observacao = ''.

  SELECT SINGLE * FROM vbkd INTO @DATA(wa_vbkd)
   WHERE vbeln EQ @c_vbeln.

  IF  wa_vbkd IS NOT INITIAL.

    SELECT SINGLE *  FROM t052 INTO @DATA(wa_t052)
     WHERE zterm EQ  @wa_vbkd-zterm.

    CLEAR: vblock.
    IF wa_t052-zdart NE 'B'.
      vblock = abap_true.
    ENDIF.
  ENDIF.


***** CS2016001313>>>>>>>>
*  IF ( SOMA >= WA_LANC-MONT_MOEDA_FIX ).
***** CS2016001313<<<<<<<<
  "IF ( SOMA > WA_LANC-MONT_MOEDA_FIX ).
  " MESSAGE E888(SABAPDOCU) WITH TEXTO_01 TEXTO_02.
  "ELSE.
  wa_edit-vbeln = wa_lanc-vbeln.
  wa_edit-moeda = wa_lanc-moeda.
  wa_edit-bukrs = c_bukrs.
  wa_edit-status = 'N'.
  LEAVE TO SCREEN 0100.
  "ENDIF.

ENDFORM.                    " NOVO_LANCAMENTO


*&---------------------------------------------------------------------*
*&      Form  EDITAR_LANCAMENTO
*&---------------------------------------------------------------------*
FORM editar_lancamento USING p_is_row_no.

  DATA: wa_verifica TYPE zfit0026,
        wa_0159     TYPE zsdt0159.

  CLEAR: wa_edit, wa_0159, wa_editor, it_editor.

  READ TABLE it_lanc INTO wa_lanc INDEX p_is_row_no.

  IF  sy-subrc EQ 0 .
    "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt3- BG #115338 SMC
    IF wa_lanc-NUM_COMP_ADIANT IS INITIAL.
      SELECT SINGLE NUM_COMP_ADIANT FROM ZFIT0026 WHERE
        VBELN = @WA_LANC-VBELN AND
        SEQ = @WA_LANC-SEQ AND
        DATA_VENC = @WA_LANC-DATA_VENC
       INTO @WA_LANC-NUM_COMP_ADIANT .
    ENDIF.
    "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt3- BG #115338 SMC
    SELECT SINGLE * FROM zsdt0159
      INTO  wa_0159
      WHERE vbeln EQ  wa_lanc-vbeln.

    IF  wa_lanc IS NOT INITIAL.
      SELECT SINGLE * FROM vbkd INTO @DATA(wa_vbkd)
   WHERE vbeln EQ @wa_lanc-vbeln.

      IF  wa_vbkd IS NOT INITIAL.

        SELECT SINGLE *  FROM t052 INTO @DATA(wa_t052)
         WHERE zterm EQ  @wa_vbkd-zterm.

        CLEAR: vblock.
        IF wa_t052-zdart NE 'B'.
          vblock = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

*==============================================================Comentado ajuste (AOENNING)
*    IF SY-SUBRC EQ 0 .
*      MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Lançamento é referente à Boleto Bancário,|
*                                   | Simulador| WA_0159-DOC_SIMULACAO |, editar pela Transação ZSDT0044.|.
*    ELSE.

*      SELECT SINGLE * FROM ZSDT0053 INTO @DATA(WA_ZSDT0053)
*        WHERE VBELN EQ @WA_LANC-VBELN.
*
*      IF SY-SUBRC = 0.
*
*        SELECT SINGLE  * FROM ZSDT0054 INTO @DATA(WA_ZSDT0054)
*          WHERE NRO_SOL_OV EQ @WA_ZSDT0053-NRO_SOL_OV.
*
*        IF SY-SUBRC = 0.
*
*          MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |O Documento para essa OV deve ser gerado pela |
*                                 | Solicitação de venda | WA_ZSDT0053-NRO_SOL_OV |, através da Transação ZSDT0062.|.
*          LEAVE TO SCREEN 0100.
*
*        ENDIF.
*
*      ENDIF.
*================================================================================
    SELECT SINGLE *
       FROM zfit0026
       INTO wa_verifica
     WHERE vbeln EQ wa_lanc-vbeln
       AND seq   EQ wa_lanc-seq.

    CASE wa_verifica-status.
      WHEN: 'G' OR 'E' OR 'P'.
        CLEAR: wa_editor, it_editor.

        MOVE-CORRESPONDING wa_lanc TO wa_edit.
**<<<------"163316 - NMS - INI------>>>
        wa_edit-doc_cont = wa_lanc-docnum.
**<<<------"163316 - NMS - FIM------>>>
        IF wa_edit-bukrs IS INITIAL. " bug 60934
          wa_edit-bukrs  =  wa_lanc-bukrs_vf.
        ENDIF.

        IF wa_lanc-docnum IS NOT INITIAL.

          wa_editor-line     = wa_lanc-observacao.
          APPEND wa_editor TO it_editor.

          CALL METHOD editor->set_text_as_stream
            EXPORTING
              text = it_editor.

          LEAVE TO SCREEN 0100.

        ELSE.

          PERFORM desc_zterm.

          IF wa_verifica-status EQ 'E'.
            wa_edit-status = ''.
          ENDIF.

****     Caso for fatura, pegar o saldo total e parcial para demostrar na tela para o usuario.
          CLEAR: wa_edit-total_recebido.
          IF wa_lanc-doc_fatura IS NOT INITIAL.
            zcl_dados_ov=>i_vlr_referencia_ov(
              EXPORTING
                i_vbeln       = wa_lanc-vbeln
                i_vbelnn      = wa_lanc-doc_fatura
              IMPORTING
                e_vlr_total   = wa_edit-valor
                e_vlr_parcial = wa_edit-total_recebido ).
            wa_edit-total_recebido  = wa_edit-total_recebido + wa_lanc-mont_moeda.
          ENDIF.

          IF wa_lanc-moeda EQ 'BRL'.
            wa_edit-taxa           = 0.
          ELSE.
            wa_edit-taxa           = wa_lanc-taxa.
          ENDIF.

          LOOP AT it_lanc WHERE seq NE wa_edit-seq.
            ADD it_lanc-mont_moeda TO wa_lanc-mont_moeda_parc.
          ENDLOOP.

          IF wa_saida-auart IN r_devo_recu.
            wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - abs( wa_lanc-mont_moeda_parc ).
            MULTIPLY wa_lanc-mont_moeda_parc BY -1.
          ELSE.
            wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - wa_lanc-mont_moeda_parc.
          ENDIF.

          wa_editor-line     = wa_lanc-observacao.
          CONDENSE wa_editor-line.
          FREE: it_editor.
          APPEND wa_editor TO it_editor.

          CALL METHOD editor->set_text_as_stream
            EXPORTING
              text = it_editor.

          LEAVE TO SCREEN 0100.

          CLEAR wa_edit.
        ENDIF.

      WHEN: 'S'.
        MESSAGE s888(sabapdocu) DISPLAY LIKE 'W' WITH 'Este registro não pode ser editado.'.
        LEAVE TO SCREEN 0100.
      WHEN OTHERS.
        CLEAR: wa_editor, it_editor.
****     Caso for fatura, pegar o saldo total e parcial para demostrar na tela para o usuario.
        CLEAR: wa_edit-total_recebido.
        IF wa_lanc-doc_fatura IS NOT INITIAL.
          zcl_dados_ov=>i_vlr_referencia_ov(
            EXPORTING
              i_vbeln       = wa_lanc-vbeln
              i_vbelnn      = wa_lanc-doc_fatura
            IMPORTING
              e_vlr_total   = wa_edit-valor
              e_vlr_parcial = wa_edit-total_recebido ).
          wa_edit-total_recebido  = wa_edit-total_recebido + wa_lanc-mont_moeda.
        ENDIF.

        wa_edit-ajuste          = wa_lanc-ajuste.
        wa_edit-vbeln           = wa_lanc-vbeln.
        wa_edit-data_venc       = wa_lanc-data_venc.
        wa_edit-moeda           = wa_lanc-moeda.
        wa_edit-mont_moeda      = wa_lanc-mont_moeda.
        wa_edit-data_pgto       = wa_lanc-data_pgto.
        CLEAR: dt_pgto_anter.
        dt_pgto_anter = wa_lanc-data_pgto.

        CLEAR: mont_rbdo_anter.
        mont_rbdo_anter         = wa_lanc-mont_rbdo.
        wa_edit-mont_rbdo       = wa_lanc-mont_rbdo.
        wa_edit-vlr_multa_rbdo  = wa_lanc-vlr_multa_rbdo.
        wa_edit-vlr_juros_rbdo  = wa_lanc-vlr_juros_rbdo.
        wa_edit-vlr_multa_calc  = wa_lanc-vlr_multa_calc.
        wa_edit-vlr_juros_calc  = wa_lanc-vlr_juros_calc.
        wa_edit-nfenum          = wa_lanc-nfenum.
        wa_edit-doc_fatura      = wa_lanc-doc_fatura.
        wa_edit-rec_vlr_total   = wa_lanc-rec_vlr_total.

        IF wa_edit-ajuste EQ 'X'.
          wa_edit-status       = wa_lanc-status.
        ENDIF.

        wa_edit-zterm          = wa_lanc-zterm.

        PERFORM desc_zterm.

        IF wa_lanc-moeda EQ 'BRL'.
          wa_edit-taxa           = 0.
        ELSE.
          wa_edit-taxa           = wa_lanc-taxa.
        ENDIF.

        wa_edit-seq            = wa_lanc-seq.
        wa_edit-bukrs          = wa_lanc-bukrs_vf.
        wa_edit-razao_especial = wa_lanc-razao_especial.

        LOOP AT it_lanc WHERE seq NE wa_edit-seq.
          ADD it_lanc-mont_moeda TO wa_lanc-mont_moeda_parc.
        ENDLOOP.

        IF wa_saida-auart IN r_devo_recu.
          wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - abs( wa_lanc-mont_moeda_parc ).
          MULTIPLY wa_lanc-mont_moeda_parc BY -1.
        ELSE.
          wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - wa_lanc-mont_moeda_parc.
        ENDIF.

        CASE wa_edit-razao_especial.
          WHEN: 'G'.
            rbutton_acerto = 'X'.
          WHEN: 'L'.
            rbutton_deposito = 'X'.
        ENDCASE.

        IF ( wa_lanc-moeda EQ 'USD' ).
          wa_edit-mont_mi    = wa_lanc-mont_moeda * wa_lanc-taxa.
        ELSE.
          wa_edit-mont_mi    = wa_lanc-mont_moeda.
        ENDIF.

        wa_edit-forma_pag  = wa_lanc-forma_pag.
**<<<------"163316 - NMS - INI------>>>
        wa_edit-doc_cont = wa_lanc-docnum.
**<<<------"163316 - NMS - FIM------>>>
        wa_edit-observacao = wa_lanc-observacao.
        wa_editor-line     = wa_edit-observacao.
        CONDENSE wa_editor-line.
        FREE: it_editor.
        APPEND wa_editor TO it_editor.

        CALL METHOD editor->set_text_as_stream
          EXPORTING
            text = it_editor.

        LEAVE TO SCREEN 0100.

        CLEAR wa_edit.
    ENDCASE.
  ENDIF.
  "ENDIF.
ENDFORM.                    " EDITAR_LANCAMENTO

*&---------------------------------------------------------------------*
*&      Form  GRAVAR_LANCAMENTO
*&---------------------------------------------------------------------*
FORM gravar_lancamento .


  DATA: wa_vbak_valor TYPE vbak.
  DATA: p_erro(1) TYPE c.

  DATA: p_zid       TYPE numc10,
        cont        TYPE numc5,
        zfit0026_wa TYPE zfit0026,
        soma        TYPE zfit0026-mont_moeda,
        obj_key     TYPE zfit0026-obj_key,

        texto_01    TYPE c LENGTH 100 VALUE 'Valor do montante menor do que o valor',
        texto_02    TYPE c LENGTH 100 VALUE 'do lançamento!',

        texto_03    TYPE c LENGTH 100 VALUE 'Não existe montante suficiente',
        texto_04    TYPE c LENGTH 100 VALUE 'para esse lançamento'.

  DATA: razao_especial TYPE c.

  DATA: it_zib_contabil_chv TYPE STANDARD TABLE OF zib_contabil_chv,
        wa_zib_contabil_chv TYPE zib_contabil_chv,
        it_z0159            TYPE STANDARD TABLE OF zsdt0159,
        wa_z0159            TYPE zsdt0159.

  DATA: vl_gdatu     TYPE gdatu_inv.


  DATA: vsoma           TYPE zfit0026-mont_moeda,
        juros_calculado TYPE netwr, " BG "115337
        c_vbeln         TYPE zfit0026-vbeln,
        c_bukrs         TYPE zfit0026-bukrs,
        texto_05        TYPE c LENGTH 100 VALUE 'Valor do Montante menor do',
        texto_06        TYPE c LENGTH 100 VALUE 'que a soma total dos lançamentos',
        texto_07        TYPE c LENGTH 100 VALUE 'Ordem de venda não possue saldo para o lançamento',
        texto_08        TYPE c LENGTH 100 VALUE 'Referencia não possue saldo para o lançamento'.
  CLEAR: p_erro.
  c_vbeln = wa_lanc-vbeln.
  c_bukrs = wa_lanc-bukrs_vf.

  FREE: l_status,
        l_message.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.
*  IF WA_LANC-DOC_FATURA IS NOT INITIAL.
*    SELECT VBELN SEQ DATA_VENC MOEDA  MONT_MOEDA TAXA MONT_MI FORMA_PAG
*         STATUS UNAME DATA_REGISTRO BUKRS OBJ_KEY DOCNUM ZTERM DOC_FATURA DATA_PGTO
*         VLR_MULTA_RBDO  VLR_JUROS_RBDO MONT_RBDO VLR_MULTA_CALC VLR_JUROS_CALC
*         OBSERVACAO AJUSTE REC_VLR_TOTAL VLR_DESC_JROS VLR_DESC_MULT
*        FROM ZFIT0026
*        INTO CORRESPONDING FIELDS OF TABLE IT_LANC_VER
*      WHERE VBELN EQ  C_VBELN
*        AND DOC_FATURA EQ WA_LANC-DOC_FATURA.
*
*  ELSE.
*    SELECT VBELN SEQ DATA_VENC MOEDA  MONT_MOEDA TAXA MONT_MI FORMA_PAG
*           STATUS UNAME DATA_REGISTRO BUKRS OBJ_KEY DOCNUM ZTERM DOC_FATURA DATA_PGTO
*           VLR_MULTA_RBDO  VLR_JUROS_RBDO MONT_RBDO VLR_MULTA_CALC VLR_JUROS_CALC
*           OBSERVACAO AJUSTE REC_VLR_TOTAL VLR_DESC_JROS VLR_DESC_MULT
*          FROM ZFIT0026
*          INTO CORRESPONDING FIELDS OF TABLE IT_LANC_VER
*        WHERE VBELN EQ  C_VBELN.
*  ENDIF.

  CLEAR: vl_gdatu.
****Selecionar
** Inicio de alteração - CS0982743 - FMartins - 25/04/2022
  IF wa_lanc-doc_fatura IS NOT INITIAL.
*   IF wa_edit-doc_fatura IS NOT INITIAL.
** Fim de alteração - CS0982743 - FMartins - 25/04/2022
    SELECT vbeln seq data_venc moeda  mont_moeda taxa mont_mi forma_pag
         status uname data_registro bukrs obj_key docnum zterm doc_fatura data_pgto
         vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo vlr_multa_calc vlr_juros_calc
         observacao ajuste rec_vlr_total vlr_desc_jros vlr_desc_mult num_comp_adiant
        FROM zfit0026
        INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
      WHERE vbeln EQ  wa_edit-vbeln
        AND doc_fatura EQ wa_edit-doc_fatura.

  ELSE.
    SELECT vbeln seq data_venc moeda  mont_moeda taxa mont_mi forma_pag
           status uname data_registro bukrs obj_key docnum zterm doc_fatura data_pgto
           vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo vlr_multa_calc vlr_juros_calc
           observacao ajuste rec_vlr_total vlr_desc_jros vlr_desc_mult num_comp_adiant
          FROM zfit0026
          INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
        WHERE vbeln EQ  wa_edit-vbeln.
  ENDIF.

****Totalizar montante recebido.
  LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln = c_vbeln.
    vsoma = vsoma + wa_lanc_ver-mont_moeda.
    IF wa_lanc_ver-docnum IS NOT INITIAL.
      IF wa_lanc_ver-vlr_juros_calc IS INITIAL .
        juros_calculado = juros_calculado -  wa_lanc_ver-vlr_juros_rbdo.
      ELSE.
        juros_calculado = juros_calculado + wa_lanc_ver-vlr_juros_calc.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF wa_edit-doc_fatura IS NOT INITIAL.
    wa_lanc-mont_moeda_fix = wa_edit-total_recebido.
  ELSE.
    wa_lanc-mont_moeda_fix = wa_lanc-mont_moeda_parc.
  ENDIF.

******  Consultar saldo total e parcial da ordem de venda e da referencia.
*  IF WA_LANC_VER-DOC_FATURA IS INITIAL.
*****   Buscar o saldo total e parcial da OV.
*    ZCL_DADOS_OV=>I_VLR_OV(
*                 EXPORTING
*           I_VBELN = WA_LANC_VER-VBELN
*                 IMPORTING
**           E_VLR_TOTAL = TOTAL_OV
*           E_VLR_PARCIAL = WA_LANC-MONT_MOEDA_FIX ).
*  ELSE.
*****     Buscar o saldo total e parcial da referencia.
*    ZCL_DADOS_OV=>I_VLR_REFERENCIA_OV(
*           EXPORTING
*     I_VBELN = WA_LANC_VER-VBELN
*     I_VBELNN = WA_LANC_VER-DOC_FATURA
*           IMPORTING
**     E_VLR_TOTAL = TOTAL_OV
*     E_VLR_PARCIAL = WA_LANC-MONT_MOEDA_FIX ).
*  ENDIF.
******************************************************************

*  IF WA_EDIT-AJUSTE NE 'X' AND WA_LANC-DOCNUM EQ 0.     "Comentado Aoenning
*    IF ( VSOMA > WA_LANC-MONT_MOEDA_FIX ).
*      MESSAGE E888(SABAPDOCU) WITH TEXTO_05 TEXTO_06.
*      EXIT.
*    ENDIF.
*  ENDIF.


**** Se status igula a green o processo ja esta encerrado então não deve gravar nem fazer as validações:
** BUG - 174094 - CBRAND - Inicio
  IF wa_edit-status <> 'G'.
** BUG - 174094 - CBRAND - Fim

    IF wa_edit-ajuste NE 'X' AND wa_lanc-docnum EQ 0.     "Comentado Aoenning
      IF ( wa_edit-mont_moeda > wa_lanc-mont_moeda_fix ).
        IF wa_edit-doc_fatura IS INITIAL.
          MESSAGE e888(sabapdocu) WITH texto_07.
          EXIT.
        ELSE.
          MESSAGE e888(sabapdocu) WITH texto_08.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.


    "Só pode ser negativo, quando for lançamento de ajuste.
    IF ( wa_edit-mont_moeda < 0 ).
      IF wa_edit-ajuste NE 'X'.
        MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Valor não pode ser Negativo!'.
        txtopen = abap_true.
        p_erro = abap_true.
        PERFORM editor_text.
        EXIT.
      ENDIF.
    ENDIF.

    IF ( wa_edit-data_pgto EQ ' ' ).
      MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Falta preencher o campo data de pagamento!'.
      txtopen = abap_true.
      p_erro = abap_true.
      PERFORM editor_text.
      EXIT.
    ENDIF.

    IF wa_edit-rec_vlr_total IS NOT INITIAL.
*    IF WA_EDIT-MONT_MOEDA NE WA_SAIDA-NETWR.

      IF wa_edit-valor IS NOT INITIAL.
        IF wa_edit-total_recebido NE wa_edit-mont_moeda.
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Montante da OV deve ser igual ao montante parcial da referencia'.
          p_erro = abap_true.
          txtopen = abap_true.
          PERFORM editor_text.
          EXIT.
        ENDIF.

        IF ( wa_edit-mont_moeda > wa_edit-total_recebido ).
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Montante é maior que o valor da referencia'.
          p_erro = abap_true.
          txtopen = abap_true.
          PERFORM editor_text.
          EXIT.
        ENDIF.

      ELSE.

        IF ( wa_edit-mont_moeda > wa_lanc-mont_moeda_parc ).
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Montante é maior que o valor da OV'.
          p_erro = abap_true.
          txtopen = abap_true.
          PERFORM editor_text.
          EXIT.
        ENDIF.

        IF wa_edit-mont_moeda NE wa_lanc-mont_moeda_parc.
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Montante da OV deve ser igual ao montante parcial'.
          p_erro = abap_true.
          txtopen = abap_true.
          PERFORM editor_text.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.


*-CS2021000297 - 29.07.2021 - JT - inicio
    IF wa_edit-data_pgto IS NOT INITIAL.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          i_bukrs  = wa_edit-bukrs
          i_data   = wa_edit-data_pgto
        IMPORTING
          e_status = l_status
          e_messa  = l_message
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF l_status = 'E'.
        DATA(l_mess1) = l_message(30).
        DATA(l_mess2) = l_message+30(34).
        MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH l_mess1 l_mess2.
        txtopen = abap_true.
        p_erro = abap_true.
        PERFORM editor_text.
        EXIT.
      ENDIF.
    ENDIF.
*-CS2021000297 - 29.07.2021 - JT - fim

*  ELSE.

    IF ( ( wa_edit IS INITIAL )          OR
       ( wa_edit-data_venc IS INITIAL )  OR
       ( wa_edit-moeda IS INITIAL )      OR
       ( wa_edit-forma_pag IS INITIAL  ) OR
       ( wa_edit-taxa IS INITIAL  )      OR
       ( wa_edit-mont_rbdo IS INITIAL )  OR
       ( wa_edit-zterm IS INITIAL ) )    AND
       ( wa_edit-moeda NE 'BRL').

      MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Obrigatório preencher todos os campos!'.
      txtopen = abap_true.
      p_erro = abap_true.
      PERFORM editor_text.
      EXIT.
*      IF WA_EDIT-MONT_MOEDA IS INITIAL AND WA_EDIT-VLR_JUROS_CALC IS INITIAL.

*        MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH 'Obrigatório preencher todos os campos!'.

*      ENDIF.

    ELSE.



      IF wa_t052-zdart EQ 'B'.
        IF  ( wa_edit-nfenum IS INITIAL ).
          block = abap_false.
          MESSAGE 'Favor informar  Referencia(Nfe)!' TYPE 'E'.
          p_erro = abap_true.
          PERFORM editor_text.
          EXIT.
        ENDIF.
      ENDIF.

      IF ( wa_edit-data_pgto IS INITIAL ) .
        block = abap_false.
        MESSAGE 'Favor informar Data Pagamento!' TYPE 'E'.
        p_erro = abap_true.
        PERFORM editor_text.
        EXIT.
      ENDIF.

      IF  ( wa_edit-mont_rbdo IS INITIAL ).
        block = abap_false.
        MESSAGE 'Favor informar Montante Recebido!' TYPE 'E'.
        p_erro = abap_true.
        PERFORM editor_text.
        EXIT.
      ENDIF.
* " BG - 115337 - SE FOR RECEBIMENTO DE JUROS TEM QUE TER SALDO DE JUROS CALCULADO - inicio
*  IF WA_LANC-VLR_JUROS_RBDO IS NOT INITIAL .
*    IF JUROS_CALCULADO IS INITIAL.
*     MESSAGE 'Não existe amortização para esta OV. Não há juros calculado.' TYPE 'E'.
*     P_ERRO = ABAP_TRUE.
*    ELSEIF WA_LANC-VLR_JUROS_RBDO > JUROS_CALCULADO.
*
*      MESSAGE 'Valor do juros em aberto é menor que o valor do lançamento!' TYPE 'E'.
*      P_ERRO = ABAP_TRUE.
*    ENDIF.
*  ENDIF.
*" BG - 115337 - SE FOR RECEBIMENTO DE JUROS TEM QUE TER SALDO DE JUROS CALCULADO - inicio

      IF ( wa_edit-vbeln IS INITIAL ).
        MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Clicar em "Novo"'.
      ELSEIF NOT ( wa_edit-taxa IS INITIAL ) AND ( wa_lanc-moeda EQ 'BRL').
        MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Moeda BRL, não é necessário a TAXA'.
        p_erro = abap_true.
        CLEAR: wa_edit-mont_moeda.
      ELSE.

*        CLEAR WA_EDITOR.
*        CALL METHOD EDITOR->GET_TEXT_AS_STREAM( IMPORTING TEXT = IT_EDITOR ).
*        CLEAR: WA_EDIT-OBSERVACAO.
*        LOOP AT IT_EDITOR INTO WA_EDITOR.
*          WA_EDIT-OBSERVACAO = |{ WA_EDIT-OBSERVACAO } { WA_EDITOR-LINE }|.
*        ENDLOOP.
**        READ TABLE IT_EDITOR INTO WA_EDITOR INDEX 1.

        SELECT COUNT(*)
          INTO cont
          FROM zfit0026
        WHERE vbeln EQ wa_edit-vbeln.

        IF ( edit EQ 'X' ).

          IF ( cont EQ 0 ).

            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                nr_range_nr = '01'
                object      = 'ZID_LANC'
              IMPORTING
                number      = p_zid.

            CLEAR: obj_key.

            zfit0026_wa-zid_lanc         = p_zid.
**<<<------"163316 - NMS - INI------>>>
* Verifica se Lançamento de Ajuste está marcado.
            IF NOT wa_edit-ajuste IS INITIAL.
              zfit0026_wa-docnum = wa_edit-doc_cont.

            ENDIF.
**<<<------"163316 - NMS - FIM------>>>
            zfit0026_wa-seq              = cont + 1.
            zfit0026_wa-vbeln            = wa_edit-vbeln.
            zfit0026_wa-data_venc        = wa_edit-data_venc.
            zfit0026_wa-moeda            = wa_edit-moeda.
            zfit0026_wa-bukrs            = wa_edit-bukrs.
            zfit0026_wa-zterm            = wa_edit-zterm.
            zfit0026_wa-status           = wa_edit-status.
            zfit0026_wa-data_pgto        = wa_edit-data_pgto.
            zfit0026_wa-mont_rbdo        = wa_edit-mont_rbdo.
            zfit0026_wa-rec_vlr_total    = wa_edit-rec_vlr_total.

            IF wa_edit-ajuste IS NOT INITIAL AND wa_edit-mont_moeda EQ 0.
              zfit0026_wa-vlr_desc_jros   = wa_edit-vlr_juros_rbdo.
              zfit0026_wa-vlr_desc_mult   = wa_edit-vlr_multa_rbdo.
            ELSE.
              zfit0026_wa-vlr_juros_rbdo   = wa_edit-vlr_juros_rbdo.
              zfit0026_wa-vlr_multa_rbdo   = wa_edit-vlr_multa_rbdo.
            ENDIF.


            IF wa_edit-ajuste IS INITIAL AND wa_edit-mont_moeda NE 0. "
              zfit0026_wa-vlr_juros_calc   = wa_edit-vlr_juros_calc.
              zfit0026_wa-vlr_multa_calc   = wa_edit-vlr_multa_calc.
            ELSE.
              zfit0026_wa-vlr_juros_calc   = ' '.
              zfit0026_wa-vlr_multa_calc   = ' '.
            ENDIF.

            zfit0026_wa-doc_fatura       = wa_edit-doc_fatura.


            zfit0026_wa-mont_moeda    = wa_edit-mont_moeda.

            IF ( wa_edit-moeda EQ 'BRL' ) AND ( wa_edit-taxa IS INITIAL ).
              IF wa_edit-ajuste IS INITIAL.
                zfit0026_wa-taxa          = 1.
              ELSE.
                "BUSCA A TAXA
                obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'G' ).
                obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'USD' ).
                obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'BRL' ).

                MOVE  wa_edit-data_pgto TO vl_gdatu.
                obj_zcl_util_sd->set_data( vl_gdatu ).
                zfit0026_wa-taxa = obj_zcl_util_sd->taxa_cambio_dia( ).

                IF zfit0026_wa-taxa IS INITIAL.
                  zfit0026_wa-taxa          = 1.
                ENDIF.

              ENDIF.

              zfit0026_wa-mont_mi       = wa_edit-mont_moeda * zfit0026_wa-taxa.


            ELSE.
              zfit0026_wa-taxa          = wa_edit-taxa.
              zfit0026_wa-mont_mi       = wa_edit-mont_moeda * wa_edit-taxa.
            ENDIF.

            zfit0026_wa-forma_pag     = wa_edit-forma_pag.
            zfit0026_wa-uname         = sy-uname.
            zfit0026_wa-data_registro = sy-datum.
            zfit0026_wa-observacao    = wa_editor-line.

            CONCATENATE zfit0026_wa-vbeln zfit0026_wa-seq sy-datum(4) INTO obj_key.

            zfit0026_wa-obj_key = obj_key.

            IF ( rbutton_acerto EQ 'X' ).
              zfit0026_wa-razao_especial = 'G'.
            ELSEIF ( rbutton_deposito EQ 'X' ).
              zfit0026_wa-razao_especial = 'L'.
            ENDIF.

            INSERT INTO zfit0026 VALUES zfit0026_wa.

            CLEAR: it_lanc_ver[], wa_lanc_ver.

            SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname
                   data_registro bukrs obj_key docnum  zterm doc_fatura data_pgto
                   vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo vlr_multa_calc vlr_juros_calc
                   razao_especial observacao ajuste rec_vlr_total vlr_desc_mult vlr_desc_jros num_comp_adiant
                FROM zfit0026
                INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
                WHERE vbeln EQ  wa_edit-vbeln.

            IF it_lanc_ver[] IS NOT INITIAL.

              SELECT *
                FROM zib_contabil_chv
                INTO TABLE it_zib_contabil_chv
                FOR ALL ENTRIES IN it_lanc_ver
                WHERE obj_key EQ it_lanc_ver-obj_key
                  AND NOT EXISTS ( SELECT *
                                     FROM bkpf
                                    WHERE belnr EQ zib_contabil_chv~belnr
                                      AND bukrs EQ zib_contabil_chv~bukrs
                                      AND gjahr EQ zib_contabil_chv~gjahr
                                      AND stblg NE space ).

              SELECT *
                FROM zsdt0159
                INTO TABLE it_z0159
                FOR ALL ENTRIES IN it_lanc_ver
                WHERE obj_key EQ it_lanc_ver-obj_key.

            ENDIF.
* ---> S4 Migration - 04/07/2023 - FTM - Inicio
            SORT: it_lanc_ver BY vbeln.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
            READ TABLE it_lanc_ver INTO wa_lanc_ver WITH KEY vbeln = wa_edit-vbeln BINARY SEARCH.

            IF ( sy-subrc EQ 0 ).

              CLEAR: it_lanc[].

              wa_lanc-edit           = icon_change.
              wa_lanc-excluir        = icon_delete.

              IF wa_lanc_ver-ajuste NE 'X'.
                wa_lanc-gera           = icon_activity.
                wa_lanc-estor          = icon_reject.
              ELSE.
                CLEAR: wa_lanc-gera, wa_lanc-estor.
              ENDIF.

              wa_lanc-vbeln          = wa_lanc_ver-vbeln.
              wa_lanc-seq            = wa_lanc_ver-seq.
              wa_lanc-data_venc      = wa_lanc_ver-data_venc.
              wa_lanc-moeda          = wa_lanc_ver-moeda.
              wa_lanc-mont_moeda     = wa_lanc_ver-mont_moeda.
              wa_lanc-taxa           = wa_lanc_ver-taxa.
              wa_lanc-mont_mi        = wa_lanc_ver-mont_mi.
              wa_lanc-zterm          = wa_lanc_ver-zterm.

              wa_lanc-taxa           = wa_lanc_ver-taxa.

              wa_lanc-forma_pag      = wa_lanc_ver-forma_pag.
              wa_lanc-uname          = wa_lanc_ver-uname.
              wa_lanc-data_registro  = wa_lanc_ver-data_registro.

              wa_lanc-data_pgto       = wa_lanc_ver-data_pgto.
              wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_multa_rbdo.
              wa_lanc-vlr_juros_rbdo  = wa_lanc_ver-vlr_juros_rbdo.
              wa_lanc-mont_rbdo       = wa_lanc_ver-mont_rbdo.

              IF wa_lanc_ver-ajuste IS NOT INITIAL.
                wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_desc_mult.
                wa_lanc-vlr_juros_rbdo  = wa_lanc_ver-vlr_desc_jros.
              ELSE.
                wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_multa_rbdo.
                wa_lanc-vlr_juros_rbdo  = wa_lanc_ver-vlr_juros_rbdo.
              ENDIF.
**<<<------"163316 - NMS - INI------>>>
* Verifica se Lançamento de Ajuste está marcado.
              IF NOT wa_lanc_ver-ajuste IS INITIAL.
                wa_lanc-docnum = wa_edit-doc_cont.

              ENDIF.
**<<<------"163316 - NMS - FIM------>>>
              IF p_ins IS NOT INITIAL.

                READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_lanc_ver-vbeln.

                IF sy-subrc = 0.
                  READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao.
                  IF sy-subrc EQ 0.
                    "PAGAMENTO ANTECIPADO
                    CASE wa_zsdt0040-meio_pago .
                      WHEN 'D' .
                        wa_lanc-pgto_ant = 'Deposito em Conta'.
                      WHEN 'A' .
                        wa_lanc-pgto_ant = 'Acerto'.
                      WHEN 'B' .
                        wa_lanc-pgto_ant = 'Boleto Bancário'.
                      WHEN ' ' .
                        wa_lanc-pgto_ant = 'Não Atencipado'.
                    ENDCASE.
                  ENDIF.
                ENDIF.
              ELSE.


                SELECT SINGLE *
                FROM zsdt0053 INTO @DATA(wa_zsdt0053)
                WHERE vbeln = @wa_lanc_ver-vbeln.

                "PAGAMENTO ANTECIPADO

                SELECT SINGLE *
                  FROM zsdt0052 INTO @DATA(wa_zsdt0052)
                  WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.

                CASE wa_zsdt0052-pgto_ant .

                  WHEN 'X' .
                    wa_lanc-pgto_ant = ' Com Boleto '.

                  WHEN 'N' .
                    wa_lanc-pgto_ant = ' Sem Boleto '.

                  WHEN ' ' .
                    wa_lanc-pgto_ant = ' Não Antecipado '.
                ENDCASE.

              ENDIF.

******Alteração CS2017000894 Início

              "WA_LANC-DOCNUM         = WA_LANC_VER-DOCNUM.
              "IR167273 - BG - 19/08/2024 - INICIO
*            READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_lanc_ver-obj_key.
*            IF sy-subrc IS INITIAL.
*              wa_lanc-docnum       = wa_zib_contabil_chv-belnr.
**                WA_LANC-STATUS_DOC  = 'P'.
*            ELSE.
*              READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_lanc_ver-obj_key.
*              IF sy-subrc IS INITIAL.
*                wa_lanc-docnum         = wa_z0159-adiant.
*              ELSE.
*                CLEAR: wa_lanc-docnum.
*              ENDIF.
*            ENDIF.
              READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_lanc_ver-obj_key.

              IF sy-subrc IS INITIAL.
                wa_lanc-docnum       = wa_zib_contabil_chv-belnr.
              ELSE.
                IF NOT wa_lanc_ver-obj_key IS INITIAL.           "<< RIM-SKM-IR118149-12.05.2022
                  READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_lanc_ver-obj_key.
                  IF sy-subrc IS INITIAL.
                    wa_lanc-docnum         = wa_z0159-adiant.
                  ELSE.
                    CLEAR: wa_lanc-docnum.
                  ENDIF.
                ELSE.                                            "<< RIM-SKM-IR118149-12.05.2022
                  wa_lanc-docnum         = wa_lanc_ver-docnum.   "<< RIM-SKM-IR118149-12.05.2022
                ENDIF.                                         "<< RIM-SKM-IR118149-12.05.2022
              ENDIF.
              "IR167273 - BG - 19/08/2024 -  FIM
******Alteração CS2017000894 Fim
              CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
              CLEAR: wa_edit-observacao,wa_editor.
              LOOP AT it_editor INTO wa_editor.
                wa_lanc-observacao = |{ wa_lanc-observacao } { wa_editor-line }|.
              ENDLOOP.

*              WA_LANC-OBSERVACAO     = WA_EDITOR-LINE.

              CASE wa_lanc_ver-status.
                WHEN: 'P'.
                  wa_lanc-status_doc = icon_generate.
                WHEN: 'E'.
                  wa_lanc-status_doc = icon_led_red.
                WHEN: 'G'.
                  wa_lanc-status_doc = icon_led_green.
                WHEN: 'X'.
                  wa_lanc-status_doc = icon_booking_stop.
                WHEN: 'A'.
                  wa_lanc-status_doc = icon_budget_update.
                WHEN: 'S'.
                  wa_lanc-status_doc = icon_alarm.
                WHEN OTHERS.
                  wa_lanc-status_doc = icon_led_yellow.
              ENDCASE.

              IF wa_saida-auart IN r_devo_recu.
                wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix + abs( wa_lanc-mont_moeda ).
                MULTIPLY wa_lanc-mont_moeda_parc BY -1.
              ELSE.
                wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - wa_lanc-mont_moeda.
              ENDIF.

              APPEND wa_lanc TO it_lanc.

              CALL METHOD grid_lancamento->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.

              CLEAR: wa_edit, edit.
              MESSAGE s888(sabapdocu) WITH 'Lançamento cadastrado!'.
*              LEAVE TO SCREEN 0100.
            ENDIF.

            CLEAR: wa_edit.

*            LEAVE TO SCREEN 0100.

          ELSE.

            IF it_lanc[ vbeln = wa_edit-vbeln
                        seq = wa_edit-seq ]-docnum
              IS NOT INITIAL.

              CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
              CLEAR: wa_edit-observacao,wa_editor.
              LOOP AT it_editor INTO wa_editor.
                wa_edit-observacao = |{ wa_edit-observacao } { wa_editor-line }|.
              ENDLOOP.

              UPDATE zfit0026 SET observacao     = wa_edit-observacao
              WHERE vbeln        = wa_edit-vbeln
                AND seq          = wa_edit-seq.
              COMMIT WORK.

            ELSE.


              IF wa_edit-ajuste IS NOT INITIAL AND wa_edit-mont_moeda EQ 0.
                wa_edit-vlr_desc_jros   = wa_edit-vlr_juros_rbdo.
                wa_edit-vlr_desc_mult   = wa_edit-vlr_multa_rbdo.

                wa_edit-vlr_juros_rbdo  = ' '.
                wa_edit-vlr_multa_rbdo  = ' '.
                wa_edit-vlr_juros_calc  = ' '.
                wa_edit-vlr_multa_calc  = ' '.
              ELSE.
                wa_edit-vlr_desc_jros   = ' '.
                wa_edit-vlr_desc_mult   = ' '.
              ENDIF.

              IF ( wa_edit-moeda EQ 'BRL' ) AND ( wa_edit-taxa IS INITIAL ) .
                IF wa_edit-ajuste IS INITIAL.
                  wa_edit-taxa = 1.
                ELSE.
                  "BUSCA A TAXA
*                CLEAR: VL_UKURS.
                  obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'G' ).
                  obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'USD' ).
                  obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'BRL' ).

                  MOVE  wa_edit-data_pgto TO vl_gdatu.
                  obj_zcl_util_sd->set_data( vl_gdatu ).
                  wa_edit-taxa = obj_zcl_util_sd->taxa_cambio_dia( ).

                  IF wa_edit-taxa IS INITIAL.
                    wa_edit-taxa = 1.
                  ENDIF.
                ENDIF.

*** Stefanini - IR255111 - 10/09/2025 - LAZAROSR - Início de Alteração
                TRY.
                  wa_edit-mont_mi = wa_edit-mont_moeda / wa_edit-taxa.
                CATCH cx_root.
                  " Ocorreu algum erro ao realizar o cálculo...
                ENDTRY.

*                wa_edit-mont_mi = wa_edit-mont_moeda * wa_edit-taxa.
*** Stefanini - IR255111 - 10/09/2025 - LAZAROSR - Fim de Alteração

              ELSE.
                CLEAR: wa_edit-mont_mi.
                wa_edit-mont_mi = wa_edit-mont_moeda * wa_edit-taxa.
              ENDIF.

              CLEAR: razao_especial.

              IF NOT ( rbutton_acerto IS INITIAL ).
                razao_especial = 'G'.
              ELSEIF NOT ( rbutton_deposito IS INITIAL ).
                razao_especial = 'L'.
              ENDIF.

              CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
              CLEAR: wa_edit-observacao,wa_editor.
              LOOP AT it_editor INTO wa_editor.
                wa_edit-observacao = |{ wa_edit-observacao } { wa_editor-line }|.
              ENDLOOP.
*              READ TABLE IT_EDITOR INTO WA_EDITOR INDEX 1.
*              WA_EDIT-OBSERVACAO = WA_EDITOR-LINE.

              IF wa_edit-ajuste EQ 'X'.
                wa_edit-status = 'A'.
              ELSE.
                wa_edit-status = ' '.
              ENDIF.

              UPDATE zfit0026 SET data_venc         = wa_edit-data_venc
                                  mont_moeda        = wa_edit-mont_moeda
                                  taxa              = wa_edit-taxa
                                  mont_mi           = wa_edit-mont_mi
                                  forma_pag         = wa_edit-forma_pag
                                  status            = wa_edit-status
                                  data_registro     = sy-datum
                                  razao_especial    = razao_especial
                                  observacao        = wa_edit-observacao
                                  zterm             = wa_edit-zterm
                                  ajuste            = wa_edit-ajuste
                                  data_pgto         = wa_edit-data_pgto
                                  mont_rbdo         = wa_edit-mont_rbdo
                                  vlr_juros_rbdo    = wa_edit-vlr_juros_rbdo
                                  vlr_multa_rbdo    = wa_edit-vlr_multa_rbdo
                                  vlr_juros_calc    = wa_edit-vlr_juros_calc
                                  vlr_multa_calc    = wa_edit-vlr_multa_calc
                                  vlr_desc_jros     = wa_edit-vlr_desc_jros
                                  vlr_desc_mult     = wa_edit-vlr_desc_mult
                                  rec_vlr_total     = wa_edit-rec_vlr_total
                                  doc_fatura        = wa_edit-doc_fatura
                                  docnum            = wa_edit-doc_cont      ""<<<------"163316 - NMS------>>>
              WHERE vbeln        = wa_edit-vbeln
                AND seq          = wa_edit-seq.
              COMMIT WORK.

            ENDIF.

            IF sy-subrc IS INITIAL.

              MESSAGE s888(sabapdocu) WITH 'Registro atualizado.'.

              CLEAR: it_lanc[],
                     wa_lanc,
                     it_lanc_ver[],
                     wa_lanc_ver.

              SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname
                     data_registro bukrs obj_key docnum   zterm doc_fatura data_pgto
                     vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo vlr_multa_calc
                     razao_especial observacao ajuste vlr_juros_calc rec_vlr_total vlr_desc_mult vlr_desc_jros num_comp_adiant
               FROM zfit0026
               INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
               WHERE vbeln EQ  wa_edit-vbeln.

              SORT: it_lanc_ver BY seq.
              IF ( sy-subrc EQ 0 ).

                IF it_lanc_ver[] IS NOT INITIAL.

                  SELECT *
                    FROM zib_contabil_chv
                    INTO TABLE it_zib_contabil_chv
                    FOR ALL ENTRIES IN it_lanc_ver
                    WHERE obj_key EQ it_lanc_ver-obj_key
                      AND NOT EXISTS ( SELECT *
                                         FROM bkpf
                                        WHERE belnr EQ zib_contabil_chv~belnr
                                          AND bukrs EQ zib_contabil_chv~bukrs
                                          AND gjahr EQ zib_contabil_chv~gjahr
                                          AND stblg NE space ).

                  SELECT *
                   FROM zsdt0159
                   INTO TABLE it_z0159
                   FOR ALL ENTRIES IN it_lanc_ver
                   WHERE obj_key EQ it_lanc_ver-obj_key.

                ENDIF.

                CLEAR: soma.

                LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln EQ wa_edit-vbeln.

                  wa_lanc-edit           = icon_change.
                  wa_lanc-excluir        = icon_delete.

                  IF wa_lanc_ver-ajuste NE 'X'.
                    wa_lanc-gera           = icon_activity.
                    wa_lanc-estor          = icon_reject.
                  ELSE.
                    CLEAR:  wa_lanc-gera, wa_lanc-estor .
                  ENDIF.

                  wa_lanc-vbeln           = wa_lanc_ver-vbeln.
                  wa_lanc-seq             = wa_lanc_ver-seq.
                  wa_lanc-data_venc       = wa_lanc_ver-data_venc.
                  wa_lanc-moeda           = wa_lanc_ver-moeda.
                  wa_lanc-mont_moeda      = wa_lanc_ver-mont_moeda.
                  wa_lanc-taxa            = wa_lanc_ver-taxa.
                  wa_lanc-mont_mi         = wa_lanc_ver-mont_mi.
                  wa_lanc-forma_pag       = wa_lanc_ver-forma_pag.
                  wa_lanc-uname           = wa_lanc_ver-uname.
                  wa_lanc-data_registro   = wa_lanc_ver-data_registro.
                  wa_lanc-ajuste          = wa_lanc_ver-ajuste.
                  wa_lanc-data_pgto       = wa_lanc_ver-data_pgto.
*                  WA_LANC-VLR_MULTA_RBDO  = WA_LANC_VER-VLR_MULTA_RBDO.
*                  WA_LANC-VLR_JUROS_RBDO  = WA_LANC_VER-VLR_JUROS_RBDO.
*                  WA_LANC-VLR_MULTA_CALC  = WA_LANC_VER-VLR_MULTA_CALC.
*                  WA_LANC-VLR_JUROS_CALC  = WA_LANC_VER-VLR_JUROS_CALC.
                  wa_lanc-mont_rbdo       = wa_lanc_ver-mont_rbdo.
                  wa_lanc-rec_vlr_total   = wa_lanc_ver-rec_vlr_total.
                  wa_lanc-doc_fatura      = wa_lanc_ver-doc_fatura.
                  wa_lanc-num_comp_adiant = wa_lanc_ver-num_comp_adiant.

                  IF wa_edit-ajuste IS NOT INITIAL AND wa_edit-mont_moeda EQ 0.
                    zfit0026_wa-vlr_desc_jros   = wa_edit-vlr_juros_rbdo.
                    zfit0026_wa-vlr_desc_mult   = wa_edit-vlr_multa_rbdo.
                  ELSE.
                    zfit0026_wa-vlr_juros_rbdo   = wa_edit-vlr_juros_rbdo.
                    zfit0026_wa-vlr_multa_rbdo   = wa_edit-vlr_multa_rbdo.
                  ENDIF.

                  IF wa_edit-ajuste IS INITIAL AND wa_edit-mont_moeda NE 0. "
                    zfit0026_wa-vlr_juros_calc   = wa_edit-vlr_juros_calc.
                    zfit0026_wa-vlr_multa_calc   = wa_edit-vlr_multa_calc.
                  ELSE.
                    zfit0026_wa-vlr_juros_calc   = ' '.
                    zfit0026_wa-vlr_multa_calc   = ' '.
                  ENDIF.

                  IF p_ins IS NOT INITIAL.

                    READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_lanc_ver-vbeln.

                    IF sy-subrc = 0.
                      READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao.
                      IF sy-subrc EQ 0.
                        "PAGAMENTO ANTECIPADO
                        CASE wa_zsdt0040-meio_pago .
                          WHEN 'D' .
                            wa_lanc-pgto_ant = 'Deposito em Conta'.
                          WHEN 'A' .
                            wa_lanc-pgto_ant = 'Acerto'.
                          WHEN 'B' .
                            wa_lanc-pgto_ant = 'Boleto Bancário'.
                          WHEN ' ' .
                            wa_lanc-pgto_ant = 'Não Atencipado'.
                        ENDCASE.
                      ENDIF.
                    ENDIF.
                  ELSE.


                    CLEAR: wa_zsdt0053.
                    SELECT SINGLE *
                    FROM zsdt0053 INTO wa_zsdt0053
                    WHERE vbeln = wa_lanc_ver-vbeln.

                    "PAGAMENTO ANTECIPADO

                    CLEAR: wa_zsdt0052.
                    SELECT SINGLE *
                      FROM zsdt0052 INTO wa_zsdt0052
                      WHERE nro_sol_ov = wa_zsdt0053-nro_sol_ov.

                    CASE wa_zsdt0052-pgto_ant .

                      WHEN 'X' .
                        wa_lanc-pgto_ant = ' Com Boleto '.

                      WHEN 'N' .
                        wa_lanc-pgto_ant = ' Sem Boleto '.

                      WHEN ' ' .
                        wa_lanc-pgto_ant = ' Não Antecipado '.
                    ENDCASE.

                  ENDIF.

******Alteração CS2017000894 Início

                  "WA_LANC-DOCNUM         = WA_LANC_VER-DOCNUM.

                  READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_lanc_ver-obj_key.
                  IF sy-subrc IS INITIAL.
                    wa_lanc-docnum       = wa_zib_contabil_chv-belnr.
                  ELSE.
                    "IR167273 - BG - 19/08/2024
                    IF NOT wa_lanc_ver-obj_key IS INITIAL.
                      READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_lanc_ver-obj_key.
                      IF sy-subrc IS INITIAL.
                        wa_lanc-docnum         = wa_z0159-adiant.
                      ELSE.
                        CLEAR: wa_lanc-docnum.
                      ENDIF.
                    ELSE.
                      wa_lanc-docnum = wa_lanc_ver-docnum.
                    ENDIF.
                  ENDIF.

******Alteração CS2017000894 Fim

                  wa_lanc-bukrs_vf       = wa_lanc_ver-bukrs.
                  wa_lanc-razao_especial = wa_lanc_ver-razao_especial.
                  wa_lanc-observacao     = wa_lanc_ver-observacao.
                  wa_lanc-zterm          = wa_lanc_ver-zterm.

                  CASE wa_lanc_ver-status.
                    WHEN: 'P'.
                      wa_lanc-status_doc = icon_generate.
                    WHEN: 'E'.
                      wa_lanc-status_doc = icon_led_red.
                    WHEN: 'G'.
                      wa_lanc-status_doc = icon_led_green.
                    WHEN: 'X'.
                      wa_lanc-status_doc = icon_booking_stop.
                    WHEN: 'A'.
                      wa_lanc-status_doc = icon_budget_update.
                    WHEN: 'S'.
                      wa_lanc-status_doc = icon_alarm.
                    WHEN OTHERS.
                      wa_lanc-status_doc = icon_led_yellow.
                  ENDCASE.

                  CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
                  CLEAR: wa_edit-observacao,wa_editor.
                  LOOP AT it_editor INTO wa_editor.
                    wa_lanc-observacao = |{ wa_lanc-observacao } { wa_editor-line }|.
                  ENDLOOP.


                  wa_lanc-mont_moeda_fix = wa_saida-netwr.

                  soma      = soma + wa_lanc_ver-mont_moeda.
**<<<------"163316 - NMS - INI------>>>
* No momento que se está editando um lançamento e o Documento Contábil da tela
* for diferente do Documento Contábil do registro em edição, substituir pelo da
* tela.
                  IF wa_lanc_ver-docnum NE wa_edit-doc_cont AND
                     wa_lanc_ver-seq    EQ wa_edit-seq.
                    wa_lanc-docnum = wa_edit-doc_cont.

                    UPDATE zfit0026
                       SET docnum = wa_lanc-docnum
                     WHERE vbeln EQ wa_edit-vbeln
                       AND seq   EQ wa_edit-seq.

                    IF sy-subrc IS INITIAL.
                      COMMIT WORK.

                    ENDIF.

                  ENDIF.
**<<<------"163316 - NMS - FIM------>
                  APPEND wa_lanc TO it_lanc.
                ENDLOOP.

                IF wa_saida-auart IN r_devo_recu.
                  wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - abs( soma ).
                  MULTIPLY wa_lanc-mont_moeda_parc BY -1.
                ELSE.
                  wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - soma.
                ENDIF.

                CALL METHOD grid_lancamento->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable.


                CLEAR: wa_edit, it_edit[], edit.
*                FREE IT_EDITOR.

*                LEAVE TO SCREEN 0100.

              ENDIF.
            ENDIF.
          ENDIF.

          CLEAR: edit.

        ELSE.


          IF ( wa_edit-ajuste NE 'X' ) AND ( wa_edit-mont_moeda > wa_lanc-mont_moeda_fix ).
            CLEAR: wa_edit.
            MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH texto_01 texto_02.
          ELSEIF ( wa_edit-ajuste <> 'X' ) AND  ( wa_edit-mont_moeda > wa_lanc-mont_moeda_parc ) AND ( cont NE 0 ).
            CLEAR: wa_edit.
            MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH texto_03 texto_04.
          ELSE.

            CLEAR: obj_key.

            CALL FUNCTION 'NUMBER_GET_NEXT'
              EXPORTING
                nr_range_nr = '01'
                object      = 'ZID_LANC'
              IMPORTING
                number      = p_zid.

            SELECT vbeln seq
              FROM zfit0026
              INTO TABLE it_cont_seq
            WHERE vbeln EQ wa_saida-vbeln
              ORDER BY seq DESCENDING.

            READ TABLE it_cont_seq INTO wa_cont_seq INDEX 1.

            zfit0026_wa-zid_lanc      = p_zid.
**<<<------"163316 - NMS - INI------>>>
* Verifica se Lançamento de Ajuste está marcado.
            IF NOT wa_edit-ajuste IS INITIAL.
              zfit0026_wa-docnum = wa_edit-doc_cont.

            ENDIF.
**<<<------"163316 - NMS - FIM------>>>
            zfit0026_wa-seq           = wa_cont_seq-seq + 1.
            zfit0026_wa-vbeln         = wa_edit-vbeln.
            zfit0026_wa-data_venc     = wa_edit-data_venc.
            zfit0026_wa-bukrs         = wa_edit-bukrs.
            zfit0026_wa-zterm         = wa_edit-zterm.

            zfit0026_wa-moeda         = wa_edit-moeda.

            zfit0026_wa-mont_moeda    = wa_edit-mont_moeda.
            zfit0026_wa-ajuste        = wa_edit-ajuste.

            IF ( zfit0026_wa-moeda EQ 'BRL' ).
              IF wa_edit-ajuste IS INITIAL.
                zfit0026_wa-taxa          = 1.
              ELSE.
                "BUSCA A TAXA
*                CLEAR: VL_UKURS.
                obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'G' ).
                obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'USD' ).
                obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'BRL' ).

                MOVE  wa_edit-data_pgto TO vl_gdatu.
                obj_zcl_util_sd->set_data( vl_gdatu ).
                zfit0026_wa-taxa = obj_zcl_util_sd->taxa_cambio_dia( ).

                IF ( zfit0026_wa-taxa IS INITIAL ) OR ( zfit0026_wa-moeda EQ 'BRL' ). "alteracao feita por Alexandre Rimini 04.04.2023 - antes era  IF zfit0026_wa-taxa IS INITIAL.
                  zfit0026_wa-taxa = 1.
                ENDIF.
              ENDIF.
              zfit0026_wa-mont_mi       = wa_edit-mont_moeda * zfit0026_wa-taxa.
            ELSE.
              zfit0026_wa-taxa          = wa_edit-taxa.
              zfit0026_wa-mont_mi       = wa_edit-mont_moeda * zfit0026_wa-taxa.
            ENDIF.

            IF ( rbutton_acerto EQ 'X' ).
              zfit0026_wa-razao_especial = 'G'.
            ELSEIF ( rbutton_deposito EQ 'X' ).
              zfit0026_wa-razao_especial = 'L'.
            ENDIF.

            zfit0026_wa-forma_pag     = wa_edit-forma_pag.
            IF zfit0026_wa-ajuste EQ 'X'.
              zfit0026_wa-status        = 'A'.
            ELSE.
              zfit0026_wa-status        = space.
            ENDIF.
            zfit0026_wa-uname         = sy-uname.
            zfit0026_wa-data_registro = sy-datum.
            zfit0026_wa-observacao    = wa_editor-line.

            zfit0026_wa-data_pgto        = wa_edit-data_pgto.
            zfit0026_wa-mont_rbdo        = wa_edit-mont_rbdo.
*            ZFIT0026_WA-VLR_JUROS_RBDO   = WA_EDIT-VLR_JUROS_RBDO.
*            ZFIT0026_WA-VLR_MULTA_RBDO   = WA_EDIT-VLR_MULTA_RBDO.
*            ZFIT0026_WA-VLR_JUROS_CALC   = WA_EDIT-VLR_JUROS_CALC.
*            ZFIT0026_WA-VLR_MULTA_CALC   = WA_EDIT-VLR_MULTA_CALC.
            zfit0026_wa-doc_fatura       = wa_edit-doc_fatura.
            zfit0026_wa-rec_vlr_total    = wa_edit-rec_vlr_total.

            IF wa_edit-ajuste IS NOT INITIAL AND wa_edit-mont_moeda EQ 0.
              zfit0026_wa-vlr_desc_jros   = wa_edit-vlr_juros_rbdo.
              zfit0026_wa-vlr_desc_mult   = wa_edit-vlr_multa_rbdo.
            ELSE.
              zfit0026_wa-vlr_juros_rbdo   = wa_edit-vlr_juros_rbdo.
              zfit0026_wa-vlr_multa_rbdo   = wa_edit-vlr_multa_rbdo.
            ENDIF.

            IF wa_edit-ajuste IS INITIAL AND wa_edit-mont_moeda NE 0. "
              zfit0026_wa-vlr_juros_calc   = wa_edit-vlr_juros_calc.
              zfit0026_wa-vlr_multa_calc   = wa_edit-vlr_multa_calc.
            ELSE.
              zfit0026_wa-vlr_juros_calc   = ' '.
              zfit0026_wa-vlr_multa_calc   = ' '.
            ENDIF.

            CONCATENATE zfit0026_wa-vbeln zfit0026_wa-seq sy-datum(4) INTO obj_key.


******Alteração CS2017000894 Fim
            FREE it_editor.
            CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
            CLEAR: zfit0026_wa-observacao,wa_editor.
            LOOP AT it_editor INTO wa_editor.
              zfit0026_wa-observacao = |{ zfit0026_wa-observacao } { wa_editor-line }|.
            ENDLOOP.

            zfit0026_wa-obj_key = obj_key.
            " BG - 115337 - SE FOR RECEBIMENTO DE JUROS TEM QUE TER SALDO DE JUROS CALCULADO - inicio
            IF zfit0026_wa-vlr_juros_rbdo IS NOT INITIAL AND  zfit0026_wa-vlr_juros_rbdo EQ zfit0026_wa-mont_rbdo.
              IF juros_calculado IS INITIAL.
                MESSAGE 'Não existe amortização para esta OV. Não há juros calculado.' TYPE 'E'.
                p_erro = abap_true.
              ELSEIF zfit0026_wa-vlr_juros_rbdo > juros_calculado.
                MESSAGE 'Valor do juros em aberto é menor que o valor do lançamento!' TYPE 'E'.
                p_erro = abap_true.
              ENDIF.
            ENDIF.
*" BG - 115337 - SE FOR RECEBIMENTO DE JUROS TEM QUE TER SALDO DE JUROS CALCULADO - in
            IF p_erro IS INITIAL.
              INSERT INTO zfit0026 VALUES zfit0026_wa.
              "ENDIF.

              CLEAR: it_lanc[],
                     wa_lanc,
                     it_lanc_ver[],
                     wa_lanc_ver.


              SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname
                     data_registro bukrs obj_key docnum zterm doc_fatura data_pgto
                     vlr_multa_rbdo vlr_juros_rbdo mont_rbdo vlr_multa_calc vlr_juros_calc
                     razao_especial observacao ajuste rec_vlr_total vlr_desc_mult vlr_desc_jros num_comp_adiant
               FROM zfit0026
               INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
                WHERE vbeln EQ  wa_edit-vbeln.

              SORT: it_lanc_ver BY seq.

              IF ( sy-subrc EQ 0 ).

                CLEAR: soma.

                LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln EQ wa_edit-vbeln.

                  "                MOVE-CORRESPONDING WA_LANC TO WA_LANC_VER.
                  MOVE-CORRESPONDING wa_lanc_ver TO wa_lanc.

                  wa_lanc-edit           = icon_change.
                  wa_lanc-excluir        = icon_delete.
                  IF wa_lanc_ver-ajuste NE 'X'.
                    wa_lanc-gera           = icon_activity.
                    wa_lanc-estor          = icon_reject.
                  ELSE.
                    CLEAR: wa_lanc-gera, wa_lanc-estor.
                  ENDIF.
*                WA_LANC-VBELN          = WA_LANC_VER-VBELN.
*                WA_LANC-SEQ            = WA_LANC_VER-SEQ.
*                WA_LANC-DATA_VENC      = WA_LANC_VER-DATA_VENC.
*                WA_LANC-MOEDA          = WA_LANC_VER-MOEDA.
*                WA_LANC-MONT_MOEDA     = WA_LANC_VER-MONT_MOEDA.
*                WA_LANC-TAXA           = WA_LANC_VER-TAXA.
*                WA_LANC-MONT_MI        = WA_LANC_VER-MONT_MI.
*                WA_LANC-FORMA_PAG      = WA_LANC_VER-FORMA_PAG.
*                WA_LANC-UNAME          = WA_LANC_VER-UNAME.
*                WA_LANC-DATA_REGISTRO  = WA_LANC_VER-DATA_REGISTRO.
*                WA_LANC-DOCNUM         = WA_LANC_VER-DOCNUM.
                  wa_lanc-bukrs_vf       = wa_lanc_ver-bukrs.
                  wa_lanc-ajuste         = wa_lanc_ver-ajuste.
*                WA_LANC-RAZAO_ESPECIAL = WA_LANC_VER-RAZAO_ESPECIAL.
*                WA_LANC-OBSERVACAO     = WA_LANC_VER-OBSERVACAO.

                  CASE wa_lanc-razao_especial.
                    WHEN: 'G'.
                      wa_lanc-razao = 'Acerto'.
                    WHEN: 'L'.
                      wa_lanc-razao = 'Depósito'.
                  ENDCASE.

                  CASE wa_lanc_ver-status.
                    WHEN: 'P'.
                      wa_lanc-status_doc = icon_generate.
                    WHEN: 'E'.
                      wa_lanc-status_doc = icon_led_red.
                    WHEN: 'G'.
                      wa_lanc-status_doc = icon_led_green.
                    WHEN: 'X'.
                      wa_lanc-status_doc = icon_booking_stop.
                    WHEN: 'A'.
                      wa_lanc-status_doc = icon_budget_update.  " ICON_MONEY   /  ICON_BUDGET_UPDATE / ICON_INSERT_RELATION /  ICON_PRICE /  ICON_VARIABLE  / ICON_TE_COSTS_ASSIGN
                    WHEN: 'S'.
                      wa_lanc-status_doc = icon_alarm.
                    WHEN OTHERS.
                      wa_lanc-status_doc = icon_led_yellow.
                  ENDCASE.

                  wa_lanc-mont_moeda_fix = wa_saida-netwr.

                  wa_lanc-observacao = wa_lanc_ver-observacao.

                  soma = soma + wa_lanc_ver-mont_moeda.

                  APPEND wa_lanc TO it_lanc.

                ENDLOOP.

                IF wa_saida-auart IN r_devo_recu.
                  wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - abs( soma ).
                  MULTIPLY wa_lanc-mont_moeda_parc BY -1.
                ELSE.
                  wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - soma.
                ENDIF.

                CALL METHOD grid_lancamento->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable.

                CLEAR: wa_edit, it_edit[].

*              LEAVE TO SCREEN 0100.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*  ENDIF.

    " 03.04.2023 - RAMON - 97568 -->
    IF p_erro IS INITIAL.

      DATA lt_zsdt0315 TYPE TABLE OF zsdt0315. " ---> Chamado MG-5804 - 25.07.2023 - LM

      CALL FUNCTION 'ZSDMF_GRAVA_REG_ZSDT0315'
        EXPORTING
          iv_vbeln             = zfit0026_wa-vbeln
*         IV_VBELV             =
          iv_waers             = zfit0026_wa-moeda
          iv_valor_ov          = wa_lanc-mont_moeda_fix
          iv_liqui             = wa_lanc-mont_moeda
*         IV_VLR_DESM          =
          iv_commit            = 'X'
*      TABLES
*         et_zsdt0315          =
* ---> Chamado MG-5804 - 25.07.2023 - LM
        TABLES
          et_zsdt0315          = lt_zsdt0315
* <--- Chamado MG-5804 - 25.07.2023 - LM
        EXCEPTIONS
          ov_100_liquidada     = 1
          vlr_desm_maior_ov    = 2
          desm_e_liqui         = 3
          vlr_liqui_maior_ov   = 4
          informar_ov_desmem   = 5
          vlr_ov_desatualizado = 6
          ov_nova_existente    = 7
          OTHERS               = 8.

      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

    ENDIF.

    " 03.04.2023 - RAMON - 97568 --<
** BUG - 174094 - CBRAND - Inicio
  ENDIF.
** BUG - 174094 - CBRAND - Fim

  IF p_erro IS INITIAL.
    CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
    CLEAR: wa_edit-observacao,wa_editor.
    LOOP AT it_editor INTO wa_editor.
      wa_edit-observacao = |{ wa_edit-observacao } { wa_editor-line }|.
    ENDLOOP.
*  READ TABLE IT_EDITOR INTO WA_EDITOR INDEX 1.
*  WA_EDIT-OBSERVACAO = WA_EDITOR-LINE.

    IF NOT wa_edit-observacao IS INITIAL.
      UPDATE zfit0026 SET   observacao     = wa_edit-observacao
                      WHERE vbeln        = wa_edit-vbeln
                        AND seq          = wa_edit-seq.

    ENDIF.


*    FREE: it_editor. "BUG - 174094 - CBRABD
  ENDIF.

  CALL METHOD editor->set_text_as_stream
    EXPORTING
      text = it_editor.

  PERFORM refresh_data_lancamento.

ENDFORM.                    " GRAVAR_LANCAMENTO

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_LANCAMENTO
*&---------------------------------------------------------------------*
FORM excluir_lancamento USING p_is_row_no.

  DATA: wa_verifica TYPE zfit0026,
        wa_0159     TYPE zsdt0159,
        mont_parc   TYPE zfit0026-mont_moeda,
        cont_lines  TYPE sy-tabix.

  mont_parc = wa_lanc-mont_moeda_parc.

  "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA
  CLEAR: wa_lanc.
  READ TABLE it_lanc[] INTO wa_lanc INDEX p_is_row_no.

  IF ( sy-subrc EQ 0 ).
*    SELECT SINGLE * FROM ZSDT0159
*        INTO  WA_0159
*        WHERE VBELN EQ  WA_LANC-VBELN.

*    IF SY-SUBRC EQ 0 .
*      MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Lançamento é referente à Boleto Bancário,|
*                                           | Simulador| WA_0159-DOC_SIMULACAO |, excluir pela Transação ZSDT0044.|.
*
*    ELSE.
**<<<------"163316 - NMS - INI------>>>
*    IF NOT wa_lanc-docnum IS INITIAL OR wa_lanc-status EQ 'P'.
    IF ( NOT wa_lanc-docnum IS INITIAL OR wa_lanc-status EQ 'P' ) AND wa_lanc-ajuste IS INITIAL.
**<<<------"163316 - NMS - FIM------>>>
      MESSAGE s888(sabapdocu) DISPLAY LIKE 'W' WITH 'Este registro só pode ser editado.'.
    ELSE.
      SELECT SINGLE *
         FROM zfit0026
         INTO wa_verifica
       WHERE vbeln EQ wa_lanc-vbeln
         AND seq   EQ wa_lanc-seq.

      CASE wa_verifica-status.

        WHEN: 'P' OR 'S' .
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Lançamento não pode ser excluido!'.
        WHEN: 'G'.

        WHEN: 'E'.

        WHEN OTHERS.

          wa_lanc-mont_moeda_parc = mont_parc + wa_lanc-mont_moeda.

          DELETE FROM zfit0026 WHERE vbeln EQ wa_lanc-vbeln
                                 AND  seq  EQ wa_lanc-seq.

          IF ( sy-subrc EQ 0 ).

            " 09.05.2023 - RAMON - 97568 -->
            CALL FUNCTION 'ZSDMF_GRAVA_REG_ZSDT0315'
              EXPORTING
                iv_vbeln   = wa_lanc-vbeln
                iv_estorno = 'X'.
            " 09.05.2023 - RAMON - 97568 --<

          ENDIF.

          IF sy-ucomm <> 'DELETAR_MASSA'."152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA
*            BREAK-POINT.
            DELETE it_lanc WHERE vbeln EQ wa_lanc-vbeln
                             AND seq   EQ wa_lanc-seq.

            CALL METHOD grid_lancamento->refresh_table_display
              EXPORTING
                is_stable = wa_stable.
            CLEAR:sy-ucomm.
            LEAVE TO SCREEN 0100.

          ELSE.

            wa_lanc-mass_flag = 'X'.

            MODIFY it_lanc INDEX p_is_row_no.

          ENDIF.

      ENDCASE.

    ENDIF.

*    ENDIF.

  ENDIF.

ENDFORM.                    " EXCLUIR_LANCAMENTO
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TRAVA_LANCAMENTO
*&---------------------------------------------------------------------*
FORM update_trava_lancamento  USING    p_vbeln
                                       p_seg.


  DATA: soma        TYPE zfit0026-mont_moeda,
        p_zid       TYPE numc10,
        zfit0026_wa TYPE zfit0026,
        cont        TYPE numc10,
        c_vbeln     TYPE zfit0026-vbeln.

  DATA: it_zib_contabil_chv TYPE STANDARD TABLE OF zib_contabil_chv,
        wa_zib_contabil_chv TYPE zib_contabil_chv,
        it_z0159            TYPE STANDARD TABLE OF zsdt0159,
        wa_z0159            TYPE zsdt0159.

  c_vbeln = p_vbeln.
  " Status
  " P = Pendencia

  IF NOT ( p_vbeln IS INITIAL ).

    SELECT COUNT(*)
      INTO cont
      FROM zfit0026
    WHERE vbeln EQ c_vbeln.


    IF ( cont > 0 ).

      UPDATE zfit0026 SET status     = 'P'
                      WHERE vbeln    = c_vbeln
                        AND seq      = p_seg.

      CLEAR: it_lanc[],  wa_lanc."it_lanc_ver[],wa_lanc_ver. "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA

      SELECT vbeln seq data_venc moeda mont_moeda  taxa mont_mi forma_pag status uname
             data_registro bukrs obj_key docnum  zterm doc_fatura data_pgto
             vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo  vlr_multa_calc vlr_juros_calc
             razao_especial  observacao ajuste rec_vlr_total num_comp_adiant
            FROM zfit0026
            INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
          WHERE vbeln EQ  c_vbeln.

      SORT: it_lanc_ver BY seq.

      IF ( sy-subrc EQ 0 ).

        IF it_lanc_ver[] IS NOT INITIAL.

          SELECT *
            FROM zib_contabil_chv
            INTO TABLE it_zib_contabil_chv
            FOR ALL ENTRIES IN it_lanc_ver
            WHERE obj_key EQ it_lanc_ver-obj_key
              AND NOT EXISTS ( SELECT *
                                 FROM bkpf
                                WHERE belnr EQ zib_contabil_chv~belnr
                                  AND bukrs EQ zib_contabil_chv~bukrs
                                  AND gjahr EQ zib_contabil_chv~gjahr
                                  AND stblg NE space ).

          SELECT *
           FROM zsdt0159
           INTO TABLE it_z0159
           FOR ALL ENTRIES IN it_lanc_ver
           WHERE obj_key EQ it_lanc_ver-obj_key.

        ENDIF.

        CLEAR: soma.

        LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln EQ c_vbeln.

          wa_lanc-edit            = icon_change.
          wa_lanc-excluir         = icon_delete.
          wa_lanc-gera            = icon_activity.
          wa_lanc-estor           = icon_reject.
          wa_lanc-vbeln           = wa_lanc_ver-vbeln.
          wa_lanc-seq             = wa_lanc_ver-seq.
          wa_lanc-data_venc       = wa_lanc_ver-data_venc.
          wa_lanc-moeda           = wa_lanc_ver-moeda.
          wa_lanc-mont_moeda      = wa_lanc_ver-mont_moeda.
          wa_lanc-taxa            = wa_lanc_ver-taxa.
          wa_lanc-mont_mi         = wa_lanc_ver-mont_mi.
          wa_lanc-forma_pag       = wa_lanc_ver-forma_pag.
          wa_lanc-uname           = wa_lanc_ver-uname.
          wa_lanc-data_registro   = wa_lanc_ver-data_registro.
          wa_lanc-data_pgto       = wa_lanc_ver-data_pgto.
          wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_multa_rbdo.
          wa_lanc-vlr_juros_rbdo  = wa_lanc_ver-vlr_juros_rbdo.
          wa_lanc-mont_rbdo       = wa_lanc_ver-mont_rbdo.
          wa_lanc-vlr_multa_calc  = wa_lanc_ver-vlr_multa_calc.
          wa_lanc-vlr_juros_calc  = wa_lanc_ver-vlr_juros_calc.
          wa_lanc-doc_fatura      = wa_lanc_ver-doc_fatura.
          wa_lanc-rec_vlr_total   = wa_lanc_ver-rec_vlr_total.
**<<<------"163316 - NMS - INI------>>>
          wa_lanc-ajuste          = wa_lanc_ver-ajuste.
**<<<------"163316 - NMS - FIM------>>>
          SELECT SINGLE * FROM j_1bnflin INTO  @DATA(wa_lin)
          WHERE refkey EQ @wa_lanc_ver-doc_fatura.

          IF sy-subrc = 0.
            SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(wa_doc)
              WHERE docnum EQ @wa_lin-docnum.

            wa_lanc-nfenum = wa_doc-nfenum.
          ENDIF.


******Alteração CS2017000894 Início

          "WA_LANC-DOCNUM         = WA_LANC_VER-DOCNUM.

          READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_lanc_ver-obj_key.
          IF sy-subrc IS INITIAL.
            wa_lanc-docnum       = wa_zib_contabil_chv-belnr.
          ELSE.
            "IR167273 - BG - 19/08/2024
            IF NOT wa_lanc_ver-obj_key IS INITIAL.
              READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_lanc_ver-obj_key.
              IF sy-subrc IS INITIAL.
                wa_lanc-docnum         = wa_z0159-adiant.
              ELSE.
**<<<------"163316 - NMS - INI------>>>
* Verifica se o o registro não é um ajuste.
                IF wa_lanc-ajuste IS INITIAL.
**<<<------"163316 - NMS - FIM------>>>
                  CLEAR: wa_lanc-docnum.
**<<<------"163316 - NMS - INI------>>>
                ENDIF.
**<<<------"163316 - NMS - FIM------>>>
              ENDIF.
            ELSE.
              wa_lanc-docnum = wa_lanc_ver-docnum.
            ENDIF.
          ENDIF.

******Alteração CS2017000894 Fim

          CASE wa_lanc_ver-status.
            WHEN: 'P'.
              wa_lanc-status_doc = icon_generate.
            WHEN: 'E'.
              wa_lanc-status_doc = icon_led_red.
            WHEN: 'G'.
              wa_lanc-status_doc = icon_led_green.
            WHEN: 'X'.
              wa_lanc-status_doc = icon_booking_stop.
            WHEN: 'A'.
              wa_lanc-status_doc = icon_budget_update.
            WHEN: 'S'.
              wa_lanc-status_doc = icon_alarm.
            WHEN OTHERS.
              wa_lanc-status_doc = icon_led_yellow.
          ENDCASE.

          wa_lanc-mont_moeda_fix = wa_saida-netwr.
          wa_lanc-status = wa_lanc_ver-status.

          soma      = soma + wa_lanc_ver-mont_moeda.

          APPEND wa_lanc TO it_lanc.

        ENDLOOP.

        wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - soma.

        "Grava a taxa na ordem
        IF wa_lanc-moeda EQ 'USD'.
          PERFORM: update_taxa USING wa_lanc-taxa
                                     wa_lanc-vbeln.
        ENDIF.

        CALL METHOD grid_lancamento->refresh_table_display
          EXPORTING
            is_stable = wa_stable.


        CLEAR: wa_edit, it_edit[].

        IF sy-ucomm <> 'GERAR_MASSA'. "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA
          LEAVE TO SCREEN 0100.
        ENDIF.

      ENDIF.

    ELSE.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZID_LANC'
        IMPORTING
          number      = p_zid.

      SELECT COUNT(*)
        INTO cont
        FROM zfit0026
      WHERE vbeln EQ c_vbeln.

      zfit0026_wa-zid_lanc        = p_zid.
      zfit0026_wa-seq             = cont + 1.
      zfit0026_wa-vbeln           = wa_lanc-vbeln.
      zfit0026_wa-data_venc       = wa_lanc-data_venc.
      zfit0026_wa-moeda           = wa_lanc-moeda.
      zfit0026_wa-mont_moeda      = wa_lanc-mont_moeda.
      zfit0026_wa-taxa            = wa_lanc-taxa.
      zfit0026_wa-mont_mi         = wa_lanc-mont_moeda * wa_lanc-taxa.
      zfit0026_wa-forma_pag       = wa_lanc-forma_pag.
      zfit0026_wa-status          = 'P'.
      zfit0026_wa-uname           = sy-uname.
      zfit0026_wa-data_registro   = sy-datum.
      zfit0026_wa-doc_fatura      = wa_lanc-doc_fatura.
      zfit0026_wa-data_pgto       = wa_lanc-data_pgto.
      zfit0026_wa-mont_rbdo       = wa_lanc-mont_rbdo.
      zfit0026_wa-vlr_multa_calc  = wa_lanc-vlr_multa_calc.
      zfit0026_wa-vlr_juros_calc  = wa_lanc-vlr_juros_calc.
      zfit0026_wa-vlr_multa_rbdo  = wa_lanc-vlr_multa_rbdo.
      zfit0026_wa-vlr_juros_rbdo  = wa_lanc-vlr_juros_rbdo.
      zfit0026_wa-rec_vlr_total   = wa_lanc-rec_vlr_total.
**<<<------"163316 - NMS - INI------>>>
      zfit0026_wa-ajuste         = wa_lanc_ver-ajuste.
**<<<------"163316 - NMS - FIM------>>>

      INSERT INTO zfit0026 VALUES zfit0026_wa.

      CLEAR: it_lanc_ver[],
             wa_lanc_ver.

      SELECT vbeln seq data_venc moeda mont_moeda  taxa mont_mi forma_pag status uname
             data_registro bukrs obj_key docnum  zterm doc_fatura data_pgto
             vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo  vlr_multa_calc vlr_juros_calc
             razao_especial  observacao ajuste rec_vlr_total num_comp_adiant
        FROM zfit0026
        INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
      WHERE vbeln EQ  c_vbeln.

      SORT: it_lanc_ver BY seq.

      IF ( sy-subrc EQ 0 ).

        IF it_lanc_ver[] IS NOT INITIAL.

          SELECT *
            FROM zib_contabil_chv
            INTO TABLE it_zib_contabil_chv
            FOR ALL ENTRIES IN it_lanc_ver
            WHERE obj_key EQ it_lanc_ver-obj_key
            AND NOT EXISTS ( SELECT *
                               FROM bkpf
                              WHERE belnr EQ zib_contabil_chv~belnr
                                AND bukrs EQ zib_contabil_chv~bukrs
                                AND gjahr EQ zib_contabil_chv~gjahr
                                AND stblg NE space ).

          SELECT *
              FROM zsdt0159
              INTO TABLE it_z0159
              FOR ALL ENTRIES IN it_lanc_ver
              WHERE obj_key EQ it_lanc_ver-obj_key.


        ENDIF.

        CLEAR: soma, wa_lanc, it_lanc[].

        LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln EQ c_vbeln.

          wa_lanc-edit           = icon_change.
          wa_lanc-excluir        = icon_delete.
          wa_lanc-gera           = icon_activity.
          wa_lanc-estor          = icon_reject.
          wa_lanc-vbeln          = wa_lanc_ver-vbeln.
          wa_lanc-seq            = wa_lanc_ver-seq.
          wa_lanc-data_venc      = wa_lanc_ver-data_venc.
          wa_lanc-moeda          = wa_lanc_ver-moeda.
          wa_lanc-mont_moeda     = wa_lanc_ver-mont_moeda.
          wa_lanc-taxa           = wa_lanc_ver-taxa.
          wa_lanc-mont_mi        = wa_lanc_ver-mont_mi.
          wa_lanc-forma_pag      = wa_lanc_ver-forma_pag.
          wa_lanc-uname          = wa_lanc_ver-uname.
          wa_lanc-data_registro  = wa_lanc_ver-data_registro.
          wa_lanc-data_pgto       = wa_lanc_ver-data_pgto.
          wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_multa_rbdo.
          wa_lanc-vlr_juros_rbdo  = wa_lanc_ver-vlr_juros_rbdo.
          wa_lanc-mont_rbdo       = wa_lanc_ver-mont_rbdo.
          wa_lanc-vlr_multa_calc  = wa_lanc_ver-vlr_multa_calc.
          wa_lanc-vlr_juros_calc  = wa_lanc_ver-vlr_juros_calc.
          wa_lanc-doc_fatura      = wa_lanc_ver-doc_fatura.
          wa_lanc-rec_vlr_total   = wa_lanc_ver-rec_vlr_total.
**<<<------"163316 - NMS - INI------>>>
          wa_lanc-ajuste          = wa_lanc_ver-ajuste.
**<<<------"163316 - NMS - FIM------>>>
          SELECT SINGLE * FROM j_1bnflin INTO  wa_lin
          WHERE refkey EQ wa_lanc_ver-doc_fatura.

          IF sy-subrc = 0.

            SELECT SINGLE * FROM j_1bnfdoc INTO wa_doc
              WHERE docnum EQ wa_lin-docnum.

            wa_lanc-nfenum = wa_doc-nfenum.
          ENDIF.

******Alteração CS2017000894 Início

          "WA_LANC-DOCNUM         = WA_LANC_VER-DOCNUM.

          READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_lanc_ver-obj_key.
          IF sy-subrc IS INITIAL.
            wa_lanc-docnum       = wa_zib_contabil_chv-belnr.
          ELSE.
            "IR167273 - BG - 19/08/2024
            IF NOT wa_lanc_ver-obj_key IS INITIAL.
              READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_lanc_ver-obj_key.
              IF sy-subrc IS INITIAL.
                wa_lanc-docnum         = wa_z0159-adiant.
              ELSE.
**<<<------"163316 - NMS - INI------>>>
* Verifica se o o registro não é um ajuste.
                IF wa_lanc-ajuste IS INITIAL.
**<<<------"163316 - NMS - FIM------>>>
                  CLEAR: wa_lanc-docnum.
**<<<------"163316 - NMS - INI------>>>
                ENDIF.
**<<<------"163316 - NMS - FIM------>>>
              ENDIF.
            ELSE.
              wa_lanc-docnum  = wa_lanc_ver-docnum.
            ENDIF.
          ENDIF.

******Alteração CS2017000894 Fim

          "WA_LANC-RAZAO_ESPECIAL = WA_LANC_VER-RAZAO_ESPECIAL.

          CASE wa_lanc_ver-status.
            WHEN: 'P'.
              wa_lanc-status_doc = icon_generate.
            WHEN: 'E'.
              wa_lanc-status_doc = icon_led_red.
            WHEN: 'G'.
              wa_lanc-status_doc = icon_led_green.
            WHEN: 'X'.
              wa_lanc-status_doc = icon_booking_stop.
            WHEN: 'A'.
              wa_lanc-status_doc = icon_budget_update.
            WHEN: 'S'.
              wa_lanc-status_doc = icon_alarm.
            WHEN OTHERS.
              wa_lanc-status_doc = icon_led_yellow.
          ENDCASE.


          wa_lanc-mont_moeda_fix = wa_saida-netwr.

          soma      = soma + wa_lanc_ver-mont_moeda.

          APPEND wa_lanc TO it_lanc.

        ENDLOOP.

        wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - soma.

        "Grava a taxa na ordem
*        TRAVAR CAMBIO SOMENTE NA ZSDT0087
        IF wa_lanc-moeda EQ 'USD'.
*          PERFORM: UPDATE_TAXA USING WA_LANC-TAXA
*                                     WA_LANC-VBELN.
        ENDIF.

        CALL METHOD grid_lancamento->refresh_table_display
          EXPORTING
            is_stable = wa_stable.


        IF sy-ucomm <> 'GERAR_MASSA'. "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA
          LEAVE TO SCREEN 0100.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " UPDATE_TRAVA_LANCAMENTO
*&---------------------------------------------------------------------*
*&      Form  GERAR_DOCUMENTO_CONTABIL
*&---------------------------------------------------------------------*
FORM gerar_documento_contabil  USING  p_is_row_no.

  DATA: "zib_contabil_wa type zib_contabil,
    cont        TYPE numc10,
    obj_key     TYPE awkey,
    sgtxt       TYPE sgtxt,
    data(10)    TYPE c,
    dia(2)      TYPE c,
    mes(2)      TYPE c,
    ano(4)      TYPE c,
    wa_zib_cont TYPE zib_contabil,
    wa_0159     TYPE zsdt0159,
    wa_kna1     TYPE kna1.

  DATA: data_sum TYPE datum,
        taxa     TYPE zfit0026-taxa.

  "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA
  CLEAR: wa_lanc.
  READ TABLE it_lanc[] INTO wa_lanc INDEX p_is_row_no.

  IF wa_lanc-bukrs_vf IS INITIAL.
    wa_lanc-bukrs_vf = wa_lanc_ver-bukrs.
  ENDIF.

  taxa = 1.

  FREE: l_status,
        l_message.

  IF wa_lanc-ajuste <> 'X'.

    IF sy-subrc EQ 0 .

      SELECT SINGLE * FROM zsdt0159
        INTO  wa_0159
        WHERE vbeln EQ  wa_lanc-vbeln.

*      IF SY-SUBRC EQ 0 .
*        MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Lançamento é referente à Boleto Bancário,|
*                                     | Simulador| WA_0159-DOC_SIMULACAO |, gerar pela Transação ZSDT0044.|.
*      ELSE.

      SELECT SINGLE * FROM zsdt0053 INTO @DATA(wa_zsdt0053)
       WHERE vbeln EQ @wa_lanc-vbeln.

*      IF SY-SUBRC = 0.
*        SELECT SINGLE * FROM ZSDT0054 INTO @DATA(WA_ZSDT0054)
*         WHERE NRO_SOL_OV EQ @WA_ZSDT0053-NRO_SOL_OV.
*
*          IF SY-SUBRC = 0.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |O Documento para essa OV deve ser gerado pela |
*                                         | Solicitação de venda | WA_ZSDT0053-NRO_SOL_OV |, através da Transação ZSDT0062.|.
*            LEAVE TO SCREEN 0100.
*          ENDIF.
*        ENDIF.

*-CS2021000297 - 29.07.2021 - JT - inicio
      IF wa_lanc-data_pgto IS NOT INITIAL.
        CALL FUNCTION 'Z_CONTROLE_FECHAMES'
          EXPORTING
            i_bukrs  = wa_lanc-bukrs_vf "WA_SAIDA-BUKRS_VF "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA
            i_data   = wa_lanc-data_pgto
          IMPORTING
            e_status = l_status
            e_messa  = l_message
          EXCEPTIONS
            error    = 1
            OTHERS   = 2.

        IF l_status = 'E'.
          DATA(l_mess1) = l_message(30).
          DATA(l_mess2) = l_message+30(34).
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH l_mess1 l_mess2.
          LEAVE TO SCREEN 0100.
        ENDIF.
      ENDIF.
*-CS2021000297 - 29.07.2021 - JT - fim

      data_sum = sy-datum + 3.
      IF ( wa_lanc-taxa IS INITIAL ).
        MESSAGE s888(sabapdocu) WITH 'Não é possível fazer um lançamento sem a taxa.' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0100.
      ELSEIF ( wa_lanc-data_venc = '00000000' OR wa_lanc-data_venc IS INITIAL OR wa_lanc-data_venc = '' OR wa_lanc-data_venc = ' ' )."IS INITIAL. "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA
        MESSAGE s888(sabapdocu) WITH 'Não é possivel fazer um lançamento' 'sem data de vencimento.' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0100.
      ELSEIF ( wa_lanc-forma_pag IS INITIAL ).
        MESSAGE s888(sabapdocu) WITH 'Não é possivel fazer um lançamento' 'sem forma de pagamento.' DISPLAY LIKE 'E'.
        LEAVE TO SCREEN 0100.
*      ELSEIF ( WA_LANC-DATA_VENC > DATA_SUM ).
*        MESSAGE S888(SABAPDOCU) WITH 'Data de vencimento maior do que o permitido.' DISPLAY LIKE 'E'. "Comentado AOENNING - 02/04/2020.
*        LEAVE TO SCREEN 0100.
      ELSE.
*        IF ( wa_lanc-moeda EQ 'BRL' ) AND ( wa_lanc-taxa > taxa ).
        IF ( wa_lanc-moeda EQ 'BRL' OR wa_lanc-moeda EQ 'ARS' ) AND ( wa_lanc-taxa > taxa ). "US 170226 - PQ

          MESSAGE s888(sabapdocu) WITH 'Taxa incorreta para moeda' DISPLAY LIKE 'E'.
          LEAVE TO SCREEN 0100.
        ELSE.
          CASE wa_lanc-status.

            WHEN: 'P' OR 'S'.
              MESSAGE s888(sabapdocu) WITH |Lançamento já está gerado, aguarde o retorno|
                                           | do número do Doc. Contábil!| DISPLAY LIKE 'E'.
            WHEN: 'G'.

            WHEN: 'E'.
              MESSAGE s888(sabapdocu) WITH 'Este lançamento contem erro' DISPLAY LIKE 'E'.
            WHEN OTHERS.

              CLEAR: wa_saida.

*                DIA = WA_LANC-DATA_VENC+6(2).
*                MES = WA_LANC-DATA_VENC+4(2).
*                ANO = WA_LANC-DATA_VENC(4).

              dia = wa_lanc-data_pgto+6(2).
              mes = wa_lanc-data_pgto+4(2).
              ano = wa_lanc-data_pgto(4).

              CONCATENATE dia '.' mes '.' ano INTO data.
              CONCATENATE wa_lanc-vbeln wa_lanc-seq sy-datum(4) INTO obj_key.

              SELECT SINGLE * FROM zib_contabil INTO wa_zib_cont WHERE obj_key EQ obj_key.

              IF ( sy-subrc EQ 0 ).

                PERFORM: gerar_contabil USING wa_lanc
                                        CHANGING obj_key .
              ELSE.

                READ TABLE it_saida INTO wa_saida WITH KEY vbeln  = wa_lanc-vbeln.

                zib_contabil_wa-obj_key     = obj_key.
                zib_contabil_wa-seqitem     = '0001'.

                "US 170226 - PQ  - INICIO
                IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA"
                  zib_contabil_wa-gsber = 'T001'.
                ELSE.
                  "De => para centro virtual.
                  SELECT SINGLE centro_real
                  FROM zsdt_depara_cen
                  INTO @DATA(w_werks)
                  WHERE centrov_1 EQ @wa_saida-werks.
                  IF w_werks IS NOT INITIAL.
                    zib_contabil_wa-gsber       = w_werks.
                  ELSE.
                    zib_contabil_wa-gsber       = wa_saida-werks.
                  ENDIF.
                ENDIF.

                "De => para centro virtual.
*                SELECT SINGLE centro_real
*                FROM zsdt_depara_cen
*                INTO @DATA(w_werks)
*                WHERE centrov_1 EQ @wa_saida-werks.
*                IF w_werks IS NOT INITIAL.
*                  zib_contabil_wa-gsber       = w_werks.
*                ELSE.
*                  zib_contabil_wa-gsber       = wa_saida-werks.
*                ENDIF.
                "US 170226 - PQ - FIM
                CLEAR: w_werks.


                zib_contabil_wa-bukrs       = wa_saida-bukrs_vf.
                zib_contabil_wa-interface   = '96'.
                IF p_ins IS NOT INITIAL.
                  zib_contabil_wa-bktxt       = 'Recbto Venda Insumos'.
                ELSEIF p_mi IS NOT INITIAL.
                  zib_contabil_wa-bktxt       = 'Recbto Venda M.I.'.
                ENDIF.
                zib_contabil_wa-bldat       = data.
                zib_contabil_wa-budat       = data.
                zib_contabil_wa-gjahr       = ano.
                zib_contabil_wa-monat       = mes.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = wa_lanc-vbeln
                  IMPORTING
                    output = zib_contabil_wa-xblnr.

                IF p_ins IS NOT INITIAL.

                  IF ( wa_saida-zlsch EQ 'P' ).
                    zib_contabil_wa-blart       = 'ND'.
                  ELSE.
                    zib_contabil_wa-blart       = 'NC'.
                  ENDIF.

                ELSEIF p_mi IS NOT INITIAL.
                  IF ( wa_saida-zlsch EQ 'P' ).
                    zib_contabil_wa-blart       = 'NM'.
                  ELSE.
                    zib_contabil_wa-blart       = 'NL'.
                  ENDIF.
                ENDIF.


                " Lançamento - Conta de Débito
                IF p_ins IS NOT INITIAL.
                  CONCATENATE 'Ordem de Venda Insumos' wa_lanc-vbeln INTO sgtxt SEPARATED BY space.
                ELSEIF p_mi IS NOT INITIAL.
                  CONCATENATE 'Ordem de Venda M.I' wa_lanc-vbeln INTO sgtxt SEPARATED BY space.
                ENDIF.


                zib_contabil_wa-bschl          = '09'.
                zib_contabil_wa-hkont          = wa_saida-kunnr.
                zib_contabil_wa-waers          = wa_lanc-moeda.
                "ZIB_CONTABIL_WA-ZFBDT          = SPACE.
                zib_contabil_wa-zfbdt          = data.
                zib_contabil_wa-zlspr          = ' '.
                zib_contabil_wa-zlsch          = wa_lanc-forma_pag.
                zib_contabil_wa-kidno          = space.
                zib_contabil_wa-sgtxt          = sgtxt.
                zib_contabil_wa-xref1          = space.
                zib_contabil_wa-xref2          = space.
                zib_contabil_wa-xref3          = space.

                "US 170226 - PQ  - INICIO
                IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA"
                  zib_contabil_wa-bupla = ''.
                ELSE.
                  "De => para centro virtual.
                  SELECT SINGLE centro_real
                  FROM zsdt_depara_cen
                  INTO w_werks
                  WHERE centrov_1 EQ wa_saida-werks.
                  IF w_werks IS NOT INITIAL.
                    zib_contabil_wa-bupla       = w_werks.
                  ELSE.
                    zib_contabil_wa-bupla       = wa_saida-werks.
                  ENDIF.
                ENDIF.

*                "De => para centro virtual.
*                SELECT SINGLE centro_real
*                FROM zsdt_depara_cen
*                INTO w_werks
*                WHERE centrov_1 EQ wa_saida-werks.
*                IF w_werks IS NOT INITIAL.
*                  zib_contabil_wa-bupla       = w_werks.
*                ELSE.
*                  zib_contabil_wa-bupla       = wa_saida-werks.
*                ENDIF.
                "US 170226 - PQ  - INICIO
                CLEAR: w_werks.

*                zib_contabil_wa-bupla          = wa_saida-vkbur. "Escritório de vendas "Comentado AOENNING 28/04/2020
                zib_contabil_wa-zterm          = wa_lanc-zterm.
                IF wa_lanc-moeda EQ 'USD'.
                  zib_contabil_wa-kurrf          = wa_lanc-taxa.
                ENDIF.


                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = wa_lanc-vbeln
                  IMPORTING
                    output = zib_contabil_wa-zuonr.

                IF p_ins IS NOT INITIAL.
                  zib_contabil_wa-umskz  = 'L'.
                ELSEIF p_mi IS NOT INITIAL.
                  zib_contabil_wa-umskz  = 'A'.
                ENDIF.

                "ZIB_CONTABIL_WA-UMSKZ = WA_LANC-RAZAO_ESPECIAL.


                zib_contabil_wa-kostl          = space.
                zib_contabil_wa-aufnr          = space.
                zib_contabil_wa-prctr          = space.
                "US 170226 - PQ  - INICIO
                IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA"
                  zib_contabil_wa-waers_i        = 'ARS'.
                ELSE.
                  zib_contabil_wa-waers_i        = 'BRL'.
                ENDIF.
*                zib_contabil_wa-waers_i        = 'BRL'.
                "US 170226 - PQ  - FIM

                zib_contabil_wa-waers_f        = wa_lanc-moeda.

                IF ( wa_lanc-moeda EQ 'USD' ).
                  zib_contabil_wa-wrbtr          = wa_lanc-mont_moeda + ( wa_lanc-vlr_multa_rbdo + wa_lanc-vlr_juros_rbdo ).
                  zib_contabil_wa-dmbtr          = wa_lanc-mont_mi    + ( ( wa_lanc-vlr_multa_rbdo + wa_lanc-vlr_juros_rbdo ) * wa_lanc-taxa ).
                  zib_contabil_wa-dmbe2          = wa_lanc-mont_moeda + ( wa_lanc-vlr_multa_rbdo + wa_lanc-vlr_juros_rbdo ).
                ELSE.
                  zib_contabil_wa-wrbtr          = wa_lanc-mont_moeda + ( wa_lanc-vlr_multa_rbdo + wa_lanc-vlr_juros_rbdo ).
                  zib_contabil_wa-dmbtr          = wa_lanc-mont_mi    + ( wa_lanc-vlr_multa_rbdo + wa_lanc-vlr_juros_rbdo ).
                  zib_contabil_wa-dmbe2          = space.
                ENDIF.

                zib_contabil_wa-bvtyp          = space.
                zib_contabil_wa-hbkid          = space.
                zib_contabil_wa-rg_atualizado  = 'N'.
                zib_contabil_wa-bankl          = space.
                zib_contabil_wa-bankn          = space.
                zib_contabil_wa-newbw          = space.
                zib_contabil_wa-anln1          = space.
                zib_contabil_wa-anln2          = space.


                INSERT INTO zib_contabil VALUES zib_contabil_wa.

                CLEAR: zib_contabil_wa.


                zib_contabil_wa-obj_key     = obj_key.
                zib_contabil_wa-seqitem     =  '0002'.

                "US 170226 - PQ  - INICIO
                IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA"
                  zib_contabil_wa-gsber = 'T001'.
                ELSE.
                  "De => para centro virtual.
                  SELECT SINGLE centro_real
                  FROM zsdt_depara_cen
                  INTO w_werks
                  WHERE centrov_1 EQ wa_saida-werks.
                  IF w_werks IS NOT INITIAL.
                    zib_contabil_wa-gsber       = w_werks.
                  ELSE.
                    zib_contabil_wa-gsber       = wa_saida-werks.
                  ENDIF.
                ENDIF.

                "De => para centro virtual.
*                SELECT SINGLE centro_real
*                FROM zsdt_depara_cen
*                INTO w_werks
*                WHERE centrov_1 EQ wa_saida-werks.
*                IF w_werks IS NOT INITIAL.
*                  zib_contabil_wa-gsber       = w_werks.
*                ELSE.
*                  zib_contabil_wa-gsber       = wa_saida-werks.
*                ENDIF.
                "US 170226 - PQ  - FIM
                CLEAR: w_werks.

                zib_contabil_wa-bukrs       = wa_saida-bukrs_vf.
                zib_contabil_wa-interface   = '96'.
                IF p_ins IS NOT INITIAL.
                  zib_contabil_wa-bktxt       = 'Recbto Venda Insumos'.
                ELSEIF p_mi IS NOT INITIAL.
                  zib_contabil_wa-bktxt       = 'Recbto Venda M.I.'.
                ENDIF.
                zib_contabil_wa-bldat       = data.
                zib_contabil_wa-budat       = data.
                zib_contabil_wa-gjahr       = ano.
                zib_contabil_wa-monat       = mes.

                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = wa_lanc-vbeln
                  IMPORTING
                    output = zib_contabil_wa-xblnr.

                IF wa_lanc-mont_moeda NE 0.
                  zib_contabil_wa-bschl          = '19'.
                  zib_contabil_wa-hkont          = wa_saida-kunnr.

                  IF p_ins IS NOT INITIAL.
                    IF ( wa_lanc-forma_pag EQ 'P' ).
                      zib_contabil_wa-blart       = 'ND'.
                    ELSE.
                      zib_contabil_wa-blart       = 'NC'.
                    ENDIF.
                  ELSEIF p_mi IS NOT INITIAL.

                    IF ( wa_lanc-forma_pag EQ 'P' ).
                      zib_contabil_wa-blart       = 'NM'.
                    ELSE.
                      zib_contabil_wa-blart       = 'NL'.
                    ENDIF.
                  ENDIF.


                  zib_contabil_wa-waers          = wa_lanc-moeda.
                  zib_contabil_wa-zfbdt          = data.
                  zib_contabil_wa-zlspr          = ' '.
                  zib_contabil_wa-zlsch          = wa_lanc-forma_pag.
                  zib_contabil_wa-kidno          = space.
                  zib_contabil_wa-sgtxt          = sgtxt.
                  zib_contabil_wa-xref1          = space.
                  zib_contabil_wa-xref2          = space.
                  zib_contabil_wa-xref3          = space.


                  "US 170226 - PQ  - INICIO
                  IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA"
                    zib_contabil_wa-bupla = ''.
                  ELSE.
                    "De => para centro virtual.
                    SELECT SINGLE centro_real
                    FROM zsdt_depara_cen
                    INTO w_werks
                    WHERE centrov_1 EQ wa_saida-werks.
                    IF w_werks IS NOT INITIAL.
                      zib_contabil_wa-bupla   = w_werks.
                    ELSE.
                      zib_contabil_wa-bupla   = wa_saida-werks.
                    ENDIF.
                  ENDIF.

*                  "De => para centro virtual.
*                  SELECT SINGLE centro_real
*                  FROM zsdt_depara_cen
*                  INTO w_werks
*                  WHERE centrov_1 EQ wa_saida-werks.
*                  IF w_werks IS NOT INITIAL.
*                    zib_contabil_wa-bupla   = w_werks.
*                  ELSE.
*                    zib_contabil_wa-bupla   = wa_saida-werks.
*                  ENDIF.
                  "US 170226 - PQ  - FIM
                  CLEAR: w_werks.


                  IF wa_lanc-moeda EQ 'USD'.
                    zib_contabil_wa-kurrf          = wa_lanc-taxa.
                  ENDIF.
*              ZIB_CONTABIL_WA-ZTERM          = WA_LANC-ZTERM.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                    EXPORTING
                      input  = wa_lanc-vbeln
                    IMPORTING
                      output = zib_contabil_wa-zuonr.

                  IF p_ins IS NOT INITIAL.
                    zib_contabil_wa-umskz          = 'L'.
                  ELSEIF p_mi IS NOT INITIAL.
                    zib_contabil_wa-umskz          = 'A'.
                  ENDIF.

                  "ZIB_CONTABIL_WA-UMSKZ          = WA_LANC-RAZAO_ESPECIAL.
                  zib_contabil_wa-kostl          = space.
                  zib_contabil_wa-aufnr          = space.
                  zib_contabil_wa-prctr          = space.
                  "US 170226 - PQ  - INICIO
                  IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA"
                    zib_contabil_wa-waers_i        = 'ARS'.
                  ELSE.
                    zib_contabil_wa-waers_i        = 'BRL'.
                  ENDIF.
*                  zib_contabil_wa-waers_i        = 'BRL'.
                  "US 170226 - PQ  - FIM
                  zib_contabil_wa-waers_f        = wa_lanc-moeda.

                  IF ( wa_lanc-moeda EQ 'USD' ).
                    zib_contabil_wa-wrbtr          = wa_lanc-mont_moeda.
                    zib_contabil_wa-dmbtr          = wa_lanc-mont_mi.
                    zib_contabil_wa-dmbe2          = wa_lanc-mont_moeda.
                  ELSE.
                    zib_contabil_wa-wrbtr          = wa_lanc-mont_moeda.
                    zib_contabil_wa-dmbtr          = wa_lanc-mont_mi.
                    zib_contabil_wa-dmbe2          = space.
                  ENDIF.

                  zib_contabil_wa-bvtyp          = space.
                  zib_contabil_wa-hbkid          = space.
                  zib_contabil_wa-rg_atualizado  = 'N'.
                  zib_contabil_wa-bankl          = space.
                  zib_contabil_wa-bankn          = space.
                  zib_contabil_wa-newbw          = space.
                  zib_contabil_wa-anln1          = space.
                  zib_contabil_wa-anln2          = space.


                  INSERT INTO zib_contabil VALUES zib_contabil_wa.
                ENDIF.

                CLEAR: zib_contabil_wa, sgtxt.

                DATA(x_total) = ( wa_lanc-vlr_juros_rbdo + wa_lanc-vlr_multa_rbdo ).
                IF x_total > 0.

                  IF p_ins IS NOT INITIAL.
                    CONCATENATE 'Receita de Juros sobre OV. Insumos' wa_lanc-vbeln INTO sgtxt SEPARATED BY space.
                  ELSEIF p_mi IS NOT INITIAL.
                    CONCATENATE 'Receita de Juros sobre OV. M.I.' wa_lanc-vbeln INTO sgtxt SEPARATED BY space.
                  ENDIF.

                  zib_contabil_wa-obj_key     =  obj_key.
                  zib_contabil_wa-seqitem     = '0003'.

                  "US 170226 - PQ  - INICIO
                  IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA"
                    zib_contabil_wa-gsber = 'T001'.
                  ELSE.
                    "De => para centro virtual.
                    SELECT SINGLE centro_real
                    FROM zsdt_depara_cen
                    INTO w_werks
                    WHERE centrov_1 EQ wa_saida-werks.
                    IF w_werks IS NOT INITIAL.
                      zib_contabil_wa-gsber       = w_werks.
                    ELSE.
                      zib_contabil_wa-gsber       = wa_saida-werks.
                    ENDIF.
                  ENDIF.

                  "De => para centro virtual.
*                  SELECT SINGLE centro_real
*                  FROM zsdt_depara_cen
*                  INTO w_werks
*                  WHERE centrov_1 EQ wa_saida-werks.
*                  IF w_werks IS NOT INITIAL.
*                    zib_contabil_wa-gsber       = w_werks.
*                  ELSE.
*                    zib_contabil_wa-gsber       = wa_saida-werks.
*                  ENDIF.
                  "US 170226 - PQ  - FIM
                  CLEAR: w_werks.

                  zib_contabil_wa-bukrs       = wa_saida-bukrs_vf.
                  zib_contabil_wa-interface   = '96'.
                  IF p_ins IS NOT INITIAL.
                    zib_contabil_wa-bktxt = 'Recbto Juros/multa Insumos'.
                  ELSEIF p_mi IS NOT INITIAL.
                    zib_contabil_wa-bktxt = 'Recbto Juros/multa M.I'.
                  ENDIF.

                  zib_contabil_wa-bldat = data.
                  zib_contabil_wa-budat = data.
                  zib_contabil_wa-gjahr = ano.
                  zib_contabil_wa-monat = mes.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = wa_lanc-vbeln
                    IMPORTING
                      output = zib_contabil_wa-xblnr.

                  zib_contabil_wa-bschl          = '50'.

                  SELECT SINGLE * FROM kna1
                       INTO  wa_kna1
                     WHERE kunnr EQ  wa_saida-kunnr.

                  zib_contabil_wa-vbund = wa_kna1-vbund.

                  IF zib_contabil_wa-vbund EQ 'SOCIOS'.
                    zib_contabil_wa-hkont          = '331004'.
                  ELSE.
                    zib_contabil_wa-hkont          = '331002'.
                  ENDIF.

                  IF p_ins IS NOT INITIAL.
                    IF ( wa_lanc-forma_pag EQ 'P' ).
                      zib_contabil_wa-blart       = 'ND'.
                    ELSE.
                      zib_contabil_wa-blart       = 'NC'.
                    ENDIF.
                  ELSEIF p_mi IS NOT INITIAL.
                    IF ( wa_lanc-forma_pag EQ 'P' ).
                      zib_contabil_wa-blart       = 'NM'.
                    ELSE.
                      zib_contabil_wa-blart       = 'NL'.
                    ENDIF.
                  ENDIF.

                  zib_contabil_wa-waers    = wa_lanc-moeda.
                  zib_contabil_wa-zfbdt    = data.
                  zib_contabil_wa-zlspr    = ' '.
                  zib_contabil_wa-zlsch    = wa_lanc-forma_pag.
                  zib_contabil_wa-kidno    = space.
                  zib_contabil_wa-sgtxt    = sgtxt.
                  zib_contabil_wa-xref1    = space.
                  zib_contabil_wa-xref2    = space.
                  zib_contabil_wa-xref3    = space.

                  "US 170226 - PQ  - INICIO
                  IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA"
                    zib_contabil_wa-bupla = ''.
                  ELSE.
                    "De => para centro virtual.
                    SELECT SINGLE centro_real
                    FROM zsdt_depara_cen
                    INTO w_werks
                    WHERE centrov_1 EQ wa_saida-werks.
                    IF w_werks IS NOT INITIAL.
                      zib_contabil_wa-bupla    = w_werks.
                    ELSE.
                      zib_contabil_wa-bupla    = wa_saida-werks.
                    ENDIF.
                  ENDIF.

*                  "De => para centro virtual.
*                  SELECT SINGLE centro_real
*                  FROM zsdt_depara_cen
*                  INTO w_werks
*                  WHERE centrov_1 EQ wa_saida-werks.
*                  IF w_werks IS NOT INITIAL.
*                    zib_contabil_wa-bupla    = w_werks.
*                  ELSE.
*                    zib_contabil_wa-bupla    = wa_saida-werks.
*                  ENDIF.
                  "US 170226 - PQ  - FIM
                  CLEAR: w_werks.


                  IF wa_lanc-moeda EQ 'USD'.
                    zib_contabil_wa-kurrf    = wa_lanc-taxa.
                  ENDIF.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                    EXPORTING
                      input  = wa_lanc-vbeln
                    IMPORTING
                      output = zib_contabil_wa-zuonr.

*                    IF P_INS IS NOT INITIAL.
*                      ZIB_CONTABIL_WA-UMSKZ = 'L'.
*                    ELSEIF P_MI IS NOT INITIAL.
*                      ZIB_CONTABIL_WA-UMSKZ = 'A'.
*                    ENDIF.

                  zib_contabil_wa-kostl   = space.
                  zib_contabil_wa-aufnr   = space.
                  zib_contabil_wa-prctr   = space.
                  IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA"
                    zib_contabil_wa-waers_i = 'ARS'.
                  ELSE.
                    zib_contabil_wa-waers_i = 'BRL'.
                  ENDIF.
*                 zib_contabil_wa-waers_i = 'BRL'.
                  "US 170226 - PQ  - FIM
                  zib_contabil_wa-waers_f = wa_lanc-moeda.

                  IF wa_lanc-moeda EQ 'USD'.
                    zib_contabil_wa-wrbtr = ( wa_lanc-vlr_multa_rbdo + wa_lanc-vlr_juros_rbdo ).
                    zib_contabil_wa-dmbtr = ( ( wa_lanc-vlr_multa_rbdo + wa_lanc-vlr_juros_rbdo ) * wa_lanc-taxa ).
                    zib_contabil_wa-dmbe2 = ( wa_lanc-vlr_multa_rbdo + wa_lanc-vlr_juros_rbdo ).
                  ELSE.
                    zib_contabil_wa-wrbtr  = ( wa_lanc-vlr_multa_rbdo + wa_lanc-vlr_juros_rbdo ).
                    zib_contabil_wa-dmbtr  = ( wa_lanc-vlr_multa_rbdo + wa_lanc-vlr_juros_rbdo ).
                    zib_contabil_wa-dmbe2  = space.
                  ENDIF.

                  zib_contabil_wa-bvtyp          = space.
                  zib_contabil_wa-hbkid          = space.
                  zib_contabil_wa-rg_atualizado  = 'N'.
                  zib_contabil_wa-bankl          = space.
                  zib_contabil_wa-bankn          = space.
                  zib_contabil_wa-newbw          = space.
                  zib_contabil_wa-anln1          = space.
                  zib_contabil_wa-anln2          = space.

                  INSERT INTO zib_contabil VALUES zib_contabil_wa.
                  CLEAR: zib_contabil_wa.
                ENDIF.

                IF ( sy-subrc EQ 0 ).
                  PERFORM: update_trava_lancamento USING wa_lanc-vbeln
                                                         wa_lanc-seq.

                ELSE.
                  MESSAGE s888(sabapdocu) WITH 'Lançamento não foi gerado'.
                ENDIF.
              ENDIF.
          ENDCASE.
          CLEAR: zib_contabil_wa, wa_lanc, wa_saida.
        ENDIF.
      ENDIF.
*      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH |Este Lançamento é apenas de Ajuste|.

  ENDIF.

  FREE: it_editor.
ENDFORM.                    " GERAR_DOCUMENTO_CONTABIL

*&---------------------------------------------------------------------*
*&      Form  ESTORNO_LANCAMENTO_CONTABIL
*&---------------------------------------------------------------------*
FORM estorno_lancamento_contabil  USING    p_row_id.

  DATA: messtab TYPE bdcmsgcoll OCCURS 0,
        wa_mess TYPE bdcmsgcoll,
        ok      TYPE c,
        data    TYPE n LENGTH 10.

  DATA: wa_zfit26   TYPE zfit0026,
        wa_0159     TYPE zsdt0159,
        wa_zib_chv  TYPE zib_contabil_chv,
        wa_zib      TYPE zib_contabil,
        vl_budat    TYPE char10,
        vl_data_doc TYPE char6,
        vl_erro     TYPE char1.

  CLEAR: ok.

  data = sy-datum.
  CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4)   INTO data SEPARATED BY '.'.

  "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA
  CLEAR: wa_lanc.
  READ TABLE it_lanc[] INTO wa_lanc INDEX p_row_id.
  IF wa_lanc-ajuste  <> 'X'.

    IF ( sy-subrc EQ 0 ) AND NOT ( wa_lanc-docnum IS INITIAL ).
      IF NOT wa_lanc-docnum IS INITIAL .

        SELECT SINGLE * FROM zfit0026 INTO wa_zfit26 WHERE   vbeln  EQ wa_lanc-vbeln
                                                         AND docnum EQ wa_lanc-docnum
                                                         AND bukrs  EQ wa_lanc-bukrs_vf
                                                         AND seq    EQ wa_lanc-seq.


        SELECT SINGLE * FROM zib_contabil_chv INTO wa_zib_chv WHERE obj_key EQ wa_zfit26-obj_key.

        IF ( sy-subrc EQ 0 ).

          SELECT SINGLE * FROM zsdt0159
            INTO  wa_0159
            WHERE vbeln EQ  wa_lanc-vbeln.

          IF sy-subrc EQ 0 .
            MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH |Doc. Contábil é referente à Boleto Bancário,|
                                         | Simulador| wa_0159-doc_simulacao |, estornar pela Transação ZSDT0044.|.

          ELSE.

            SELECT SINGLE * FROM zsdt0053 INTO @DATA(wa_zsdt0053)
              WHERE vbeln EQ @wa_lanc-vbeln.

*            IF SY-SUBRC = 0.
*              SELECT SINGLE  * FROM ZSDT0054 INTO @DATA(WA_ZSDT0054)
*                WHERE NRO_SOL_OV EQ @WA_ZSDT0053-NRO_SOL_OV.
*
*              IF SY-SUBRC = 0.
*                MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |O Documento para essa OV deve ser gerado pela |
*                                       | Solicitação de venda | WA_ZSDT0053-NRO_SOL_OV |, através da Transação ZSDT0062.|.
*                LEAVE TO SCREEN 0100.
*              ENDIF.
*            ENDIF.

            "CS2017001985
            SELECT SINGLE * FROM zib_contabil INTO wa_zib WHERE obj_key EQ wa_zfit26-obj_key.
            IF sy-subrc IS INITIAL.
              FREE: it_bdc.
              CLEAR: vl_data_doc, vl_budat.

              CONCATENATE wa_zib-bldat+6(4) wa_zib-bldat+3(2) '01' INTO vl_data_doc.

              IF vl_data_doc EQ sy-datum(6).
                "mesmo mes
                PERFORM batch_input USING: 'X' 'SAPMF05A'     '0105',
                                           ''  'BDC_CURSOR'   'UF05A-STGRD',
                                           ''  'BDC_OKCODE'   '=BU',
                                           ''  'RF05A-BELNS'  wa_lanc-docnum,
                                           ''  'BKPF-BUKRS'   wa_lanc-bukrs_vf,
                                           ''  'RF05A-GJAHS'  wa_zib_chv-gjahr(4),
                                           ''  'UF05A-STGRD'  '01'.
              ELSE.
                "mes retroativo
                CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) INTO vl_budat.

                PERFORM batch_input USING: 'X' 'SAPMF05A'     '0105',
                                           ''  'BDC_CURSOR'   'UF05A-STGRD',
                                           ''  'BDC_OKCODE'   '=BU',
                                           ''  'RF05A-BELNS'  wa_lanc-docnum,
                                           ''  'BKPF-BUKRS'   wa_lanc-bukrs_vf,
                                           ''  'RF05A-GJAHS'  wa_zib_chv-gjahr(4),
                                           ''  'UF05A-STGRD'  '02',
                                           ''  'BSIS-BUDAT'   vl_budat,
                                           ''  'BSIS-MONAT'   sy-datum+4(2).
              ENDIF.
            ENDIF.

*      FREE: IT_BDC.
*      PERFORM BATCH_INPUT USING: 'X' 'SAPMF05A'   '0105',
*                                 ''  'BDC_CURSOR'  'UF05A-STGRD',
*                                 ''  'BDC_OKCODE'  '=BU',
*                                 ''  'RF05A-BELNS' WA_LANC-DOCNUM,
*                                 ''  'BKPF-BUKRS'  WA_LANC-BUKRS_VF,
*                                 ''  'RF05A-GJAHS' WA_ZIB_CHV-GJAHR(4),
*                                 ''  'UF05A-STGRD' '01'.

*      OPT-DISMODE = 'E'.
            CALL TRANSACTION 'FB08' USING it_bdc MODE 'N' MESSAGES INTO messtab."OPTIONS FROM OPT.

*      CALL TRANSACTION 'FB08' USING it_bdc MODE 'N' MESSAGES INTO messtab.

            IF ( sy-subrc EQ 0 ) .

              LOOP AT messtab INTO wa_mess.
                IF ( wa_mess-msgtyp EQ 'S' ) AND NOT ( wa_mess-msgv1 IS INITIAL ) .
                  ok = 'X'.
                ENDIF.
              ENDLOOP.


              CASE ok.
                WHEN: 'X'.

                  CLEAR: vl_erro.

                  UPDATE zfit0026 SET docnum        = ''
                                      status        = ''
                                      data_venc     = wa_lanc-data_venc "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA
*                                      TAXA          = ''
*                                      DATA_REGISTRO = ''
*                                      FORMA_PAG     = ''
                                  WHERE docnum EQ wa_lanc-docnum.

                  "Estorna a taxa da ordem e adiciona a última existente (docnum mais recente)
                  PERFORM estorna_taxa USING wa_lanc-vbeln
                                       CHANGING vl_erro.

                  PERFORM: select_data_refresh .

                  CALL METHOD grid_lancamento->refresh_table_display
                    EXPORTING
                      is_stable = wa_stable.

                  IF vl_erro EQ abap_true.
                    MESSAGE s888(sabapdocu) WITH 'Taxa não pode ser atualizada'.
                  ELSE.
                    READ TABLE messtab INTO wa_mess WITH KEY msgtyp = 'S'.
                    IF sy-subrc IS INITIAL.
                      MESSAGE ID wa_mess-msgid TYPE 'S' NUMBER wa_mess-msgnr WITH wa_mess-msgv1 wa_mess-msgv2 wa_mess-msgv3 wa_mess-msgv4
                      DISPLAY LIKE wa_mess-msgtyp.
                    ENDIF.
                  ENDIF.

              ENDCASE.

            ELSE.

              READ TABLE messtab INTO wa_mess WITH KEY msgtyp = 'E'.
              IF sy-subrc IS INITIAL.
                MESSAGE ID wa_mess-msgid TYPE 'S' NUMBER wa_mess-msgnr WITH wa_mess-msgv1 wa_mess-msgv2 wa_mess-msgv3 wa_mess-msgv4
                DISPLAY LIKE wa_mess-msgtyp.
              ELSE.
                MESSAGE s888(sabapdocu) WITH 'Documento já compensado.'.
              ENDIF.

            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH | Não há Doc. Contábil a ser estornado!|.
    ENDIF.
  ENDIF.
ENDFORM.                    " ESTORNO_LANCAMENTO_CONTABIL

*&---------------------------------------------------------------------*
*&      Form  REINICIA_ERRO
*&---------------------------------------------------------------------*
FORM reinicia_erro  USING p_row_id.

  IF NOT ( p_row_id IS INITIAL ).

    READ TABLE it_status_display INTO wa_status_display  INDEX p_row_id.

    IF ( sy-subrc EQ 0 ).

      DELETE FROM zfit0026 WHERE seq EQ wa_status_display-seq
                             AND vbeln EQ wa_status_display-vbeln.

      IF ( sy-subrc EQ 0 ).

        CALL METHOD grid_status->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

        MESSAGE s888(sabapdocu) WITH 'Lançamento deletado!'.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " REINICIA_ERRO


*&---------------------------------------------------------------------*
*&      Form  BATCH_INPUT
*&---------------------------------------------------------------------*
FORM batch_input  USING     VALUE(p_flag)
                            VALUE(p_fnam)
                            VALUE(p_fval).

  CLEAR it_bdc.

  IF NOT p_flag IS INITIAL.
    it_bdc-program  = p_fnam.
    it_bdc-dynpro   = p_fval.
    it_bdc-dynbegin = 'X'.
  ELSE.
    it_bdc-fnam = p_fnam.
    it_bdc-fval = p_fval.
  ENDIF.

  APPEND it_bdc.

ENDFORM.                    "batch_input



*&---------------------------------------------------------------------*
*&      Form  SELECIONA_STATUS
*&---------------------------------------------------------------------*
FORM seleciona_status.


  DATA: it_zib_contabil_err_aux TYPE TABLE OF ty_zib_contabil_err,
        wa_zib_contabil_err_aux TYPE ty_zib_contabil_err.

  DATA: mensagem TYPE text240.


  CLEAR: it_status[], wa_status, wa_lanc-mont_moeda_parc.

  SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname data_registro obj_key docnum
    FROM zfit0026
    INTO TABLE it_status_aux
  WHERE status EQ 'E'.

  IF it_status_aux IS NOT INITIAL.
    LOOP AT it_status_aux INTO wa_status_aux.

      wa_status-vbeln          =  wa_status_aux-vbeln.
      wa_status-seqitem_aux    =  wa_status_aux-seq.
      wa_status-data_venc      =  wa_status_aux-data_venc.
      wa_status-moeda          =  wa_status_aux-moeda.
      wa_status-mont_moeda     =  wa_status_aux-mont_moeda.
      wa_status-taxa           =  wa_status_aux-taxa.
      wa_status-mont_mi        =  wa_status_aux-mont_mi.
      wa_status-forma_pag      =  wa_status_aux-forma_pag.
      wa_status-status         =  wa_status_aux-status.
      wa_status-uname          =  wa_status_aux-uname.
      wa_status-data_registro  =  wa_status_aux-data_registro.
      wa_status-obj_key        =  wa_status_aux-obj_key.
      wa_status-mont_moeda_fix =  wa_status_aux-mont_moeda_fix.

      APPEND wa_status TO it_status.

    ENDLOOP.


    SELECT obj_key nr_item interface dt_atualizacao hr_atualizacao type id num message message_v1 message_v2 message_v3 message_v4
      FROM zib_contabil_err
      INTO TABLE it_zib_contabil_err
    FOR ALL ENTRIES IN it_status
      WHERE obj_key EQ it_status-obj_key.
  ENDIF.


  REFRESH: it_zib_contabil_err_aux[].
  it_zib_contabil_err_aux[] = it_zib_contabil_err[].

  CHECK it_status IS NOT INITIAL.

  LOOP AT it_status INTO wa_status.

    READ TABLE it_zib_contabil_err INTO wa_zib_contabil_err WITH KEY obj_key = wa_status-obj_key.


    IF ( sy-subrc EQ 0 ).

      wa_status_display-vbeln          = wa_status-vbeln.
      wa_status_display-seqitem_aux    = wa_status-seqitem_aux.
      wa_status_display-data_venc      = wa_status-data_venc.
      wa_status_display-moeda          = wa_status-moeda.
      wa_status_display-mont_moeda     = wa_status-mont_moeda.
      wa_status_display-taxa           = wa_status-taxa.
      wa_status_display-mont_mi        = wa_status-mont_mi.
      wa_status_display-forma_pag      = wa_status-forma_pag.


      wa_status_display-seq =  wa_status_display-seqitem_aux .

      CASE wa_status-status.
        WHEN: 'P'.
          wa_status_display-status_doc = icon_generate.
        WHEN: 'E'.
          wa_status_display-status_doc = icon_led_red.
        WHEN: 'G'.
          wa_status_display-status_doc = icon_led_green.
        WHEN: 'A'.
          wa_status_display-status_doc = icon_budget_update.
        WHEN: 'S'.
          wa_status_display-status_doc = icon_alarm.
        WHEN OTHERS.
          wa_status_display-status_doc = icon_led_yellow.
      ENDCASE.

      wa_status_display-uname          = wa_status-uname.
      wa_status_display-data_registro  = wa_status-data_registro.
      wa_status_display-obj_key        = wa_status-obj_key.
      wa_status_display-mont_moeda_fix = wa_status-mont_moeda_fix.



      LOOP AT it_zib_contabil_err_aux INTO wa_zib_contabil_err_aux  WHERE obj_key EQ  wa_zib_contabil_err-obj_key.
        CONCATENATE  mensagem wa_zib_contabil_err_aux-message  INTO mensagem SEPARATED BY space.
        CLEAR: wa_zib_contabil_err-message.
      ENDLOOP.

      wa_status_display-mensagem = mensagem.

      APPEND wa_status_display TO it_status_display.

      DELETE it_status WHERE obj_key EQ wa_zib_contabil_err-obj_key.

      CLEAR: wa_status_display, wa_zib_contabil_err, wa_zib_contabil_err_aux.

    ENDIF.
  ENDLOOP.



  IF NOT ( it_status_display[] IS INITIAL ).


    CLEAR: it_fieldcatalog[], wa_fieldcatalog.


    SORT: it_status_display ASCENDING.
    PERFORM: display_catalog_status,
             create_object_status.

  ENDIF.

ENDFORM.                    " SELECIONA_STATUS
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_VA03
*&---------------------------------------------------------------------*
FORM call_transaction_va03  USING    p_row_id.

  DATA: vbeln_var TYPE zfit0026-vbeln.

  CLEAR: wa_lanc, wa_status_display, vbeln_var.

  IF NOT ( it_status_display[] IS INITIAL ).
    READ TABLE it_status_display INTO wa_status_display INDEX p_row_id.
    vbeln_var = wa_status_display-vbeln.
  ELSE.
    READ TABLE it_lanc INTO wa_lanc INDEX p_row_id .
    vbeln_var = wa_lanc-vbeln.
  ENDIF.

  IF NOT ( vbeln_var IS INITIAL ).
    SET PARAMETER ID 'AUN' FIELD vbeln_var.
    CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.                    " CALL_TRANSACTION_VA03

*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_FB03
*&---------------------------------------------------------------------*
FORM call_transaction_fb03  USING    p_row_id.

  CLEAR: wa_lanc.

  READ TABLE it_lanc INTO wa_lanc INDEX p_row_id .

  CHECK NOT wa_lanc-docnum IS INITIAL.
  CHECK NOT wa_lanc-bukrs_vf IS INITIAL.

  SET PARAMETER ID 'BLN' FIELD wa_lanc-docnum.
  SET PARAMETER ID 'BUK' FIELD wa_lanc-bukrs_vf.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.


ENDFORM.                    " CALL_TRANSACTION_FB03


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CATALOG
*&---------------------------------------------------------------------*
FORM display_catalog .

  FREE: it_fieldcatalog, wa_fieldcatalog.

  DEFINE alv_l.

    wa_fieldcatalog-fieldname = &1.
    wa_fieldcatalog-scrtext_l = &2.
    wa_fieldcatalog-scrtext_m = &2.
    wa_fieldcatalog-scrtext_s = &2.
    wa_fieldcatalog-outputlen = &3.
    wa_fieldcatalog-no_zero   = &4.
    wa_fieldcatalog-hotspot   = &5.
    wa_fieldcatalog-emphasize = &6.
    wa_fieldcatalog-just      = &7.
    wa_fieldcatalog-do_sum    = &8.

    APPEND wa_fieldcatalog TO it_fieldcatalog.

  END-OF-DEFINITION.

  alv_l:
        'VISUAL'      c_edit        '7'   ' ' 'X' ' '    'C' ' ',
        'BUKRS_VF'    c_bukrs       '4'   'X' ' ' ' '    ' ' ' ',
        'KUNNR'       c_kunnr       '8'   'X' ' ' ' '    ' ' ' ',
        'NAME1'       c_name1       '28'  ' ' ' ' ' '    ' ' ' ',
        'VKBUR'       c_vkbur       '4'   ' ' ' ' ' '    ' ' ' ',
        "'WERKS'       C_WERKS       '4'   ' ' ' ' ' '    ' ' ' ',
        'VBELN'       c_vbeln       '9'   'X' ' ' ' '    ' ' ' ',
        "'ERDAT'       C_ERDAT       '10'  ' ' ' ' ' '    ' ' ' ',
        'VALDT'       c_valdt       '10'  ' ' ' ' ' '    ' ' ' ',
        'ZTERM'       c_zterm       '4'   ' ' ' ' ' '    ' ' ' ',
        'TEXT1'       c_text1       '15'  ' ' ' ' ' '    ' ' ' ',
        'MEIO_PGMTO'  c_meio_pgmto  '20'  ' ' ' ' ' '    ' ' ' ',
        'PGTO_ANT'    c_pgto_ant    '15'  ' ' ' ' ' '    ' ' ' ',
        'WAERK'       c_waerk       '4'   ' ' ' ' ' '    ' ' ' ',
        'NETWR_L'     TEXT-002      '18'  ' ' ' ' ' '    ' ' 'X',
        'MWSBP'       TEXT-003      '18'  ' ' ' ' ' '    ' ' 'X',
        'NETWR'       TEXT-004      '18'  ' ' ' ' 'C500' ' ' 'X',
        'STATUS'     c_status_l    '3'   ' ' ' ' ' '    'C' ' '. " API lanc.Aut. Acerto Insumos SIGAM pt3- BG #115338
  "'AUART'       C_AUART       '5'   ' ' ' ' ' '    ' ' ' ',


ENDFORM.                    " DISPLAY_CATALOG

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CATALOG_LANCAMENTO
*&---------------------------------------------------------------------*
FORM display_catalog_lancamento.

  PERFORM fieldcatalog USING:

        'EDIT'           c_edit_l          '5'   ' ' 'X' ' '  'C' ' ',
        'EXCLUIR'        c_excl_l          '5'   ' ' 'X' ' '  'C' ' ',
        'GERA'           c_gera_l          '5'   ' ' 'X' ' '  'C' ' ',
        'ESTOR'          c_estor_l         '5'   ' ' 'X' ' '  'C' ' ',
        'STATUS_DOC'     c_status_l        '3'   ' ' 'X' ' '  'C' ' ',  "// INICIO WBARBOSA 11-12-2024 US-115811
        'SEQ'            c_seq_l           '7'   ' ' ' ' ' '  ' ' ' ',
        'BUKRS_VF'       c_bukrs_l         '5'   ' ' ' ' ' '  ' ' ' ',
        'VBELN'          c_vbeln_l         '10'   ' ' 'X' ' '  ' ' ' ',
        'NFENUM'         c_nfenum          '10'  ' ' ' ' ' '  ' ' ' ',
        'DOCNUM'         c_doc_contabil_l  '10'  ' ' 'X' ' '  ' ' ' ',
        'DATA_PGTO'      c_data_pgto       '10'   ' ' ' ' ' '  ' ' ' ',
        'DATA_VENC'      c_data_venc_l     '10'  ' ' ' ' ' '  ' ' ' ',"152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA
        'MOEDA'          c_moeda_l         '4'   ' ' ' ' ' '  ' ' ' ',
        'MONT_MOEDA'     c_mont_moeda_l    '13'  ' ' ' ' ' '  ' ' 'X',
        'TAXA'           c_taxa_l          '8'   ' ' ' ' ' '  ' ' 'X',
        'VLR_MULTA_RBDO' c_multa_rbdo      '14'  ' ' ' ' ' '  ' ' ' ',
        'VLR_JUROS_RBDO' c_juros_rbdo      '14'  ' ' ' ' ' '  ' ' ' ',
        'MONT_MI'        c_mont_mi_l       '13'  ' ' ' ' ' '  ' ' 'X',
        'MONT_RBDO'      c_mont_rbdo       '14'  ' ' ' ' ' '  ' ' ' ',
        'FORMA_PAG'      c_forma_pag_l     '4'  ' ' ' ' ' '  ' ' ' ',
        'ZTERM'          c_condicao        '05'  ' ' ' ' ' '  ' ' ' ',
*        'PGTO_ANT '      C_PGTO_ANT        '10'   ' ' ' ' ' '  ' ' ' ',
        'UNAME'          c_uname_l         '10'  ' ' ' ' ' '  ' ' ' ',
        'DATA_REGISTRO'  c_dta_registro_l  '10'  ' ' ' ' ' '  ' ' ' ',
*        'RAZAO'          C_RAZAO_ESPECIAL  '10'  ' ' ' ' ' '  ' ' ' ',
        'OBSERVACAO'     c_observacao      '30'  ' ' ' ' ' '  ' ' ' '.

ENDFORM.                    " DISPLAY_CATALOG_LANCAMENTO
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_CATALOG_STATUS
*&---------------------------------------------------------------------*
FORM display_catalog_status .

  PERFORM fieldcatalog USING:

  'SEQ'            c_seq_s           '7'   '' '' ''  '' '',
  'VBELN'          c_vbeln_s         '12'  '' 'X' ''  '' '',
  'STATUS_DOC'     c_status_s        '5'   '' 'X' ''  '' '', "// INICIO WBARBOSA 11-12-2024 US-115811
  'DATA_VENC'      c_data_venc_s     '10'  '' '' ''  '' '',
  'MONT_MOEDA'     c_mont_moeda_s    '16'  '' '' ''  '' '',
  'UNAME'          c_uname_s         '15'  '' '' ''  '' '',
  'DATA_REGISTRO'  c_dta_registro_s  '15'  '' '' ''  '' '',
  'MENSAGEM'       c_mensagem        '100' '' '' ''  '' ''.

ENDFORM.                    " DISPLAY_CATALOG_STATUS

*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fieldcatalog  USING    VALUE(p_fieldname)
                            VALUE(p_desc)
                            VALUE(p_tam)
                            VALUE(p_no_zero)
                            VALUE(p_hotspot)
                            VALUE(p_cor)
                            VALUE(p_just)
                            VALUE(p_sum).

  wa_fieldcatalog-fieldname = p_fieldname.
  wa_fieldcatalog-scrtext_l = p_desc.
  wa_fieldcatalog-scrtext_m = p_desc.
  wa_fieldcatalog-scrtext_s = p_desc.
  wa_fieldcatalog-outputlen = p_tam.
  wa_fieldcatalog-no_zero   = p_no_zero.
  wa_fieldcatalog-hotspot   = p_hotspot.
  wa_fieldcatalog-emphasize = p_cor.
  wa_fieldcatalog-just      = p_just.
  wa_fieldcatalog-do_sum    = p_sum.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

  CLEAR: wa_fieldcatalog.

ENDFORM.                    " FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
MODULE create_object OUTPUT.

  DATA: wl_repid         TYPE sy-repid,
        gr_event_handler TYPE REF TO lcl_event_handler.

  wl_repid = sy-repid.

  IF ( cl_custom IS INITIAL ).

    wa_layout-zebra      = c_x.
    wa_stable-row        = c_x.

    CREATE OBJECT cl_custom
      EXPORTING
        container_name              = 'CONTAINER_P'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT grid_principal
      EXPORTING
        i_parent          = cl_custom
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT gr_event_handler.
    SET HANDLER gr_event_handler->handle_hotspot_click FOR grid_principal.


    CALL METHOD grid_principal->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        is_variant                    = gs_variant_c
        i_save                        = c_a
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = it_fieldcatalog[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.
ENDMODULE.                 " CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_LANCAMENTO
*&---------------------------------------------------------------------*
FORM  create_container_lancamento USING p_vbeln.

  DATA: t_fun  TYPE ui_functions,
        wa_fun TYPE ui_func.

  "DATA: CL_RECEV_L  TYPE REF TO LCL_TIMER.

*  CREATE OBJECT CL_TIMER_L.
*  CREATE OBJECT CL_RECEV_L.

  "  SET HANDLER CL_RECEV_L->HANDLE_FINISHED FOR CL_TIMER_L.

  " CL_TIMER_L->INTERVAL = 60.

  "CALL METHOD CL_TIMER_L->RUN.

  CONCATENATE 'Número da O.V ' p_vbeln INTO wa_layout-grid_title SEPARATED BY space.

  CREATE OBJECT cl_custom_l
    EXPORTING
      container_name = 'CONTAINER_L'.

  CREATE OBJECT grid_lancamento
    EXPORTING
      i_parent = cl_custom_l.

  CREATE OBJECT gr_event_handler.
  SET HANDLER   gr_event_handler->handle_hotspot_click FOR grid_lancamento.
  SET HANDLER   gr_event_handler->handle_user_command  FOR grid_lancamento.
  SET HANDLER   gr_event_handler->handle_toolbar  FOR grid_lancamento.

  "Habilita o box de seleção!
  wa_layout-sel_mode = 'A'. "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA

  CALL METHOD grid_lancamento->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding          = t_fun
      is_layout                     = wa_layout
      is_variant                    = gs_variant_c
      i_save                        = c_a
    CHANGING
      it_outtab                     = it_lanc[]
      it_fieldcatalog               = it_fieldcatalog[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.


  IF ( sy-subrc EQ 0 ).
    tabstrip-activetab = 'TAB_L'.
  ENDIF.

ENDFORM.                    " CREATE_CONTAINER_LANCAMENTO
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATA_LANCAMENTO
*&---------------------------------------------------------------------*
FORM refresh_data_lancamento .

  IF ( grid_lancamento IS INITIAL ).

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = grid_lancamento.

  ENDIF.


  IF grid_lancamento IS NOT INITIAL.
    PERFORM select_data_refresh .

    IF NOT ( it_lanc_ver[] IS INITIAL ).
      CALL METHOD grid_lancamento->refresh_table_display.
    ENDIF.
  ENDIF.

ENDFORM.                    " REFRESH_DATA_LANCAMENTO
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_REFRESH
*&---------------------------------------------------------------------*
FORM select_data_refresh .

**** É executado o JOB LANC_CLIENTE_INSUMOS com REPORT ZFISJ0001 para atualizar
**** o documento da ZFIT0026

  CLEAR: it_lanc[].
*  FREE: IT_EDITOR.

  DATA: cont      TYPE numc10,
        soma_parc TYPE zfit0026-mont_moeda.

  DATA: it_zib_contabil_chv TYPE STANDARD TABLE OF zib_contabil_chv,
        wa_zib_contabil_chv TYPE zib_contabil_chv,
        it_z0159            TYPE STANDARD TABLE OF zsdt0159,
        wa_z0159            TYPE zsdt0159.


****  Verificar se o docnum foi estornado. (AOENNING)
  check_estor_docnum=>check_estorn_docnum( EXPORTING i_vbeln = vbeln_aux ).
********************************************************************************

  SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname
         data_registro bukrs obj_key docnum  zterm doc_fatura data_pgto
         vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo
         vlr_multa_calc vlr_juros_calc razao_especial observacao ajuste rec_vlr_total vlr_desc_mult vlr_desc_jros num_comp_adiant
     FROM zfit0026
     INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
   WHERE vbeln EQ  vbeln_aux.

  IF it_lanc_ver[] IS NOT INITIAL.

    CLEAR: wa_lanc.

    SELECT *
      FROM zib_contabil_chv
      INTO TABLE it_zib_contabil_chv
      FOR ALL ENTRIES IN it_lanc_ver
      WHERE obj_key EQ it_lanc_ver-obj_key
        AND NOT EXISTS ( SELECT *
                           FROM bkpf
                          WHERE belnr EQ zib_contabil_chv~belnr
                            AND bukrs EQ zib_contabil_chv~bukrs
                            AND gjahr EQ zib_contabil_chv~gjahr
                            AND stblg NE space ).


    IF it_zib_contabil_chv IS INITIAL.
      SELECT *
       FROM zib_contabil_err
       INTO TABLE @DATA(t_zib_contabil_err)
       FOR ALL ENTRIES IN @it_lanc_ver
       WHERE obj_key EQ @it_lanc_ver-obj_key.
    ENDIF.

    SELECT *
        FROM zsdt0159
        INTO TABLE it_z0159
        FOR ALL ENTRIES IN it_lanc_ver
        WHERE obj_key EQ it_lanc_ver-obj_key.

  ENDIF.

  SORT: it_lanc_ver BY seq.

  LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln = vbeln_aux.

    wa_lanc-edit          = icon_change.
    wa_lanc-excluir       = icon_delete.
    IF wa_lanc_ver-ajuste NE 'X'.
      wa_lanc-gera          = icon_activity.
      wa_lanc-estor         = icon_reject.
    ELSE.
      CLEAR: wa_lanc-gera, wa_lanc-estor.
    ENDIF.
    wa_lanc-vbeln           = wa_lanc_ver-vbeln.
    wa_lanc-seq             = wa_lanc_ver-seq.
    wa_lanc-data_venc       = wa_lanc_ver-data_venc.
    wa_lanc-moeda           = wa_lanc_ver-moeda.
    wa_lanc-mont_moeda      = wa_lanc_ver-mont_moeda.
    wa_lanc-taxa            = wa_lanc_ver-taxa.
    wa_lanc-mont_mi         = wa_lanc_ver-mont_mi.
    wa_lanc-forma_pag       = wa_lanc_ver-forma_pag.
    wa_lanc-status          = wa_lanc_ver-status.
    wa_lanc-uname           = wa_lanc_ver-uname.
    wa_lanc-data_registro   = wa_lanc_ver-data_registro.
    wa_lanc-docnum          = wa_lanc_ver-docnum.
    wa_lanc-data_pgto       = wa_lanc_ver-data_pgto.
    wa_lanc-vlr_multa_calc  = wa_lanc_ver-vlr_multa_calc.
    wa_lanc-vlr_juros_calc  = wa_lanc_ver-vlr_juros_calc.
    wa_lanc-mont_rbdo       = wa_lanc_ver-mont_rbdo.
    wa_lanc-doc_fatura      = wa_lanc_ver-doc_fatura.
    wa_lanc-rec_vlr_total   = wa_lanc_ver-rec_vlr_total.

    IF wa_lanc_ver-ajuste IS NOT INITIAL.
      wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_desc_mult.
      wa_lanc-vlr_juros_rbdo   = wa_lanc_ver-vlr_desc_jros.
    ELSE.
      wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_multa_rbdo.
      wa_lanc-vlr_juros_rbdo  = wa_lanc_ver-vlr_juros_rbdo.
    ENDIF.

    IF wa_lanc_ver-doc_fatura IS NOT INITIAL.
      SELECT SINGLE * FROM j_1bnflin INTO  @DATA(wa_lin)
      WHERE refkey EQ @wa_lanc_ver-doc_fatura.

      IF sy-subrc = 0.
        SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(wa_doc)
          WHERE docnum EQ @wa_lin-docnum.

        wa_lanc-nfenum = wa_doc-nfenum.
      ENDIF.
    ENDIF.
******Alteração CS2017000894 Início

    "WA_LANC-DOCNUM         = WA_LANC_VER-DOCNUM.


    IF p_ins IS NOT INITIAL.

      READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_lanc_ver-vbeln.

      IF sy-subrc = 0.
        READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao.
        IF sy-subrc EQ 0.
          "PAGAMENTO ANTECIPADO
          CASE wa_zsdt0040-meio_pago .
            WHEN 'D' .
              wa_lanc-pgto_ant = 'Deposito em Conta'.
            WHEN 'A' .
              wa_lanc-pgto_ant = 'Acerto'.
            WHEN 'B' .
              wa_lanc-pgto_ant = 'Boleto Bancário'.
            WHEN ' ' .
              wa_lanc-pgto_ant = 'Não Atencipado'.
          ENDCASE.
        ENDIF.
      ENDIF.
    ELSE.



      SELECT SINGLE *
      FROM zsdt0053 INTO @DATA(wa_zsdt0053)
      WHERE vbeln = @wa_lanc_ver-vbeln.

      "PAGAMENTO ANTECIPADO


      SELECT SINGLE *
        FROM zsdt0052 INTO @DATA(wa_zsdt0052)
        WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.

      CASE wa_zsdt0052-pgto_ant .

        WHEN 'X' .
          wa_lanc-pgto_ant = ' Com Boleto '.

        WHEN 'N' .
          wa_lanc-pgto_ant = ' Sem Boleto '.

        WHEN ' ' .
          wa_lanc-pgto_ant = ' Não Antecipado '.
      ENDCASE.

    ENDIF.


    READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_lanc_ver-obj_key.
    IF sy-subrc IS INITIAL.
      IF wa_lanc-docnum IS NOT INITIAL.
        IF wa_zib_contabil_chv-belnr NE wa_lanc-docnum.
          wa_lanc-docnum = wa_zib_contabil_chv-belnr.
        ENDIF.
      ELSE.
        wa_lanc-docnum       = wa_zib_contabil_chv-belnr.
      ENDIF.

      IF wa_zib_contabil_chv-belnr IS NOT INITIAL.
        "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt3- BG #115338 - fim
        SELECT SINGLE * FROM zfit0026 INTO @DATA(wa_zfit0026) WHERE  obj_key EQ @wa_zib_contabil_chv-obj_key.
        IF  sy-subrc IS INITIAL.
          IF wa_zfit0026-objkey_sigam IS NOT INITIAL AND wa_zfit0026-docnum_forn IS INITIAL .
            wa_lanc_ver-status = 'S'.
            wa_lanc-status = wa_lanc_ver-status.
          ELSE.
            wa_lanc_ver-status = 'G'.
            wa_lanc-status = wa_lanc_ver-status.
          ENDIF.
        ENDIF.
        "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt3- BG #115338 - FIM
      ELSE.
        wa_lanc_ver-status = 'P'.
        wa_lanc-status = wa_lanc_ver-status.
      ENDIF.

    ELSE.
      "IR167273 - BG - 19/08/2024
      IF NOT wa_lanc_ver-obj_key IS INITIAL.
        READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_lanc_ver-obj_key.
        IF sy-subrc IS INITIAL.
          wa_lanc-docnum         = wa_z0159-adiant.

          IF wa_z0159-adiant IS NOT INITIAL.
            wa_lanc_ver-status = 'G'.
            wa_lanc-status = wa_lanc_ver-status.
          ELSE.
            wa_lanc_ver-status = 'P'.
            wa_lanc-status = wa_lanc_ver-status.
          ENDIF.

        ELSE.
**<<<------"163316 - NMS - INI------>>>
* Verifica se Lançamento de Ajuste não está marcado.
          IF wa_lanc_ver-ajuste IS INITIAL.
**<<<------"163316 - NMS - FIM------>>>
            CLEAR: wa_lanc-docnum.
**<<<------"163316 - NMS - INI------>>>
          ENDIF.
**<<<------"163316 - NMS - FIM------>>>
        ENDIF.
      ELSE.
        wa_lanc-docnum = wa_lanc_ver-docnum.
      ENDIF.

    ENDIF.

*    READ TABLE T_ZIB_CONTABIL_ERR INTO DATA(W_ZIB_ERR) WITH KEY OBJ_KEY = WA_LANC_VER-OBJ_KEY. "Comentado AOENNING 25/03/2020
*    IF SY-SUBRC EQ 0.
*      WA_LANC_VER-STATUS = 'E'.
*      WA_LANC-STATUS = WA_LANC_VER-STATUS.
*    ENDIF.

    UPDATE zfit0026
        SET
          docnum = @wa_lanc-docnum,
          status = @wa_lanc-status
     WHERE vbeln        = @wa_lanc-vbeln
       AND seq          = @wa_lanc-seq.

******Alteração CS2017000894 Fim

*-BUG 49186 - inicio
    IF wa_lanc_ver-bukrs IS NOT INITIAL.
      wa_lanc-bukrs_vf       = wa_lanc_ver-bukrs.
    ELSE.
      wa_lanc-bukrs_vf       = wa_saida-bukrs_vf.
    ENDIF.
*   wa_lanc-bukrs_vf      = wa_lanc_ver-bukrs.
*-BUG 49186 - fim
    wa_lanc-zterm         = wa_lanc_ver-zterm.
    wa_lanc-observacao    = wa_lanc_ver-observacao.
    wa_lanc-ajuste        = wa_lanc_ver-ajuste.

    CASE wa_lanc_ver-razao_especial.
      WHEN: 'G'.
        wa_lanc-razao = 'Acerto'.
      WHEN: 'L'.
        wa_lanc-razao = 'Depósito'.
    ENDCASE.

    CASE wa_lanc_ver-status.
      WHEN: 'P'.
        wa_lanc-status_doc = icon_generate.
      WHEN: 'E'.
        wa_lanc-status_doc = icon_led_red.
      WHEN: 'G'.
        wa_lanc-status_doc = icon_led_green.
      WHEN: 'X'.
        wa_lanc-status_doc = icon_booking_stop.
      WHEN: 'A'.
        wa_lanc-status_doc = icon_budget_update.
      WHEN: 'S'.
        wa_lanc-status_doc = icon_alarm.
      WHEN OTHERS.
        wa_lanc-status_doc = icon_led_yellow.
    ENDCASE.

    wa_lanc-mont_moeda_fix  = wa_saida-netwr.

    soma_parc = soma_parc + wa_lanc_ver-mont_moeda.
    APPEND wa_lanc TO it_lanc.

  ENDLOOP.

  IF wa_lanc_ver-ajuste EQ abap_true.
    soma_parc = abs( soma_parc ).
  ENDIF.

  wa_lanc-mont_moeda_parc = wa_saida-netwr - soma_parc.

  IF wa_saida-auart IN r_devo_recu.
    MULTIPLY wa_lanc-mont_moeda_parc BY -1.
  ENDIF.

  CLEAR: soma_parc.

  CALL METHOD grid_lancamento->refresh_table_display.

ENDFORM.                    " SELECT_DATA_REFRESH

*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT_STATUS
*&---------------------------------------------------------------------*
FORM create_object_status .

  DATA: t_fun  TYPE ui_functions,
        wa_fun TYPE ui_func.

  CREATE OBJECT cl_custom_s
    EXPORTING
      container_name = 'CONTAINER_S'.

  CREATE OBJECT grid_status
    EXPORTING
      i_parent = cl_custom_s.

  CREATE OBJECT gr_event_handler.
  SET HANDLER   gr_event_handler->handle_hotspot_click FOR grid_status.
  SET HANDLER   gr_event_handler->handle_user_command  FOR grid_status.

  CALL METHOD grid_status->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding          = t_fun
      is_layout                     = wa_layout_s
      is_variant                    = gs_variant_c
      i_save                        = c_a
    CHANGING
      it_outtab                     = it_status_display[]
      it_fieldcatalog               = it_fieldcatalog[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

ENDFORM.                    " CREATE_OBJECT_STATUS
*&---------------------------------------------------------------------*
*&      Form  GERAR_CONTABIL
*&---------------------------------------------------------------------*
FORM gerar_contabil  USING    wa_lanc STRUCTURE wa_lanc
                     CHANGING obj_key.

  TYPES: BEGIN OF ty_zfit0026_gera,
           vbeln   TYPE zfit0026-vbeln,
           seq     TYPE zfit0026-seq,
           obj_key TYPE zfit0026-obj_key,
         END OF ty_zfit0026_gera,

         BEGIN OF ty_zib_contabil_cont,
           obj_key   TYPE zib_contabil-obj_key,
           seqitem   TYPE zib_contabil-seqitem,
           bukrs     TYPE zib_contabil-bukrs,
           interface TYPE zib_contabil-interface,
           xblnr     TYPE zib_contabil-xblnr,
         END OF ty_zib_contabil_cont.

  DATA: it_zfit0026_gera    TYPE TABLE OF ty_zfit0026_gera,
        wa_zfit0026_gera    TYPE ty_zfit0026_gera,
        tabix               TYPE sy-tabix,
        seq_novo            TYPE zfit0026-seq,
        wa_verifica_zib     TYPE zib_contabil,
        wa_zib_contabil_aux TYPE zib_contabil,
        it_zib_cont         TYPE TABLE OF ty_zib_contabil_cont,
        wa_zib_cont         TYPE ty_zib_contabil_cont,
        vbeln_aux_p         TYPE zib_contabil-xblnr,
        wa_kna1             TYPE kna1.


  DATA: data(10)   TYPE c,
        dia(2)     TYPE c,
        mes(2)     TYPE c,
        ano(4)     TYPE c,
        sgtxt      TYPE sgtxt,
        w_zfit0026 TYPE zfit0026,
        qtd        TYPE sy-tabix.



  SELECT vbeln seq obj_key
    FROM zfit0026
    INTO TABLE it_zfit0026_gera
  WHERE vbeln EQ wa_lanc-vbeln.

  IF ( sy-subrc EQ 0 ).

    SELECT SINGLE * FROM zsdt0053 INTO @DATA(wa_zsdt0053)
    WHERE vbeln EQ @wa_lanc-vbeln.
*
*    IF SY-SUBRC = 0.
*      SELECT SINGLE * FROM ZSDT0054 INTO @DATA(WA_ZSDT0054)
*      WHERE NRO_SOL_OV EQ @WA_ZSDT0053-NRO_SOL_OV.
*
*      IF SY-SUBRC = 0.
*        MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |O Documento para essa OV deve ser gerado pela |
*                               | Solicitação de venda | WA_ZSDT0053-NRO_SOL_OV |, através da Transação ZSDT0062.|.
*        LEAVE TO SCREEN 0100.
*      ENDIF.
*    ENDIF.

    DESCRIBE TABLE it_zfit0026_gera LINES tabix.

    READ TABLE it_zfit0026_gera INTO wa_zfit0026_gera INDEX tabix.

    IF ( sy-subrc EQ 0 ).

      seq_novo = wa_zfit0026_gera-seq + 1.

*      DIA = WA_LANC-DATA_VENC+6(2).
*      MES = WA_LANC-DATA_VENC+4(2).
*      ANO = WA_LANC-DATA_VENC(4).

      dia = wa_lanc-data_pgto+6(2).
      mes = wa_lanc-data_pgto+4(2).
      ano = wa_lanc-data_pgto(4).

      CONCATENATE dia '.' mes '.' ano INTO data.
      CONCATENATE wa_zfit0026_gera-vbeln seq_novo sy-datum(4) INTO obj_key.

      SELECT SINGLE * FROM zib_contabil INTO wa_verifica_zib WHERE obj_key EQ obj_key.


      IF ( sy-subrc EQ 0 ).
        CLEAR: wa_verifica_zib.

** =====================================================Comentado Aoenning
*        CLEAR: SEQ_NOVO, WA_VERIFICA_ZIB.

*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*          EXPORTING
*            INPUT  = WA_LANC-VBELN
*          IMPORTING
*            OUTPUT = VBELN_AUX_P.
*
*        SELECT OBJ_KEY SEQITEM BUKRS INTERFACE XBLNR
*          FROM ZIB_CONTABIL
*          INTO TABLE IT_ZIB_CONT
*        WHERE XBLNR     EQ VBELN_AUX_P
*          AND BUKRS     EQ WA_LANC-BUKRS_VF
*          AND INTERFACE EQ '96'.

*        DESCRIBE TABLE IT_ZIB_CONT LINES QTD.
**=======================================================Comentado Aoenning

        DATA: w TYPE i.

        w = 0.

*        SEQ_NOVO = QTD + 1. "Comentado Aoenning
        seq_novo = seq_novo + 1.

        CONCATENATE wa_zfit0026_gera-vbeln seq_novo sy-datum(4) INTO obj_key.

        WHILE w EQ 0.

          CLEAR: wa_verifica_zib.
          SELECT SINGLE * FROM zib_contabil INTO wa_verifica_zib WHERE obj_key EQ obj_key.
          CLEAR: w_zfit0026.
          SELECT SINGLE * FROM zfit0026 INTO w_zfit0026 WHERE seq EQ seq_novo AND vbeln  EQ wa_lanc-vbeln.


*          IF ( SY-SUBRC NE 0 ).
***       Verifica sequencia na ZIB e tambem na ZFIT0026 caso tenha retorna e adiciona outra seguencia.
          IF wa_verifica_zib IS INITIAL AND w_zfit0026 IS INITIAL.

            w = 1.
          ELSE.

            CLEAR: obj_key.
            seq_novo = seq_novo + 1.
            CONCATENATE wa_zfit0026_gera-vbeln seq_novo sy-datum(4) INTO obj_key.

          ENDIF.
        ENDWHILE.
      ENDIF.

      CLEAR: w_zfit0026.
      READ TABLE it_saida INTO wa_saida WITH KEY vbeln  = wa_lanc-vbeln.

      zib_contabil_wa-obj_key     = obj_key.
      zib_contabil_wa-seqitem     = '0001'.

      "US 170226 - PQ - INICIO
      IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA
        zib_contabil_wa-gsber = 'T001'.
      ELSE.
        "De => para centro virtual.
        SELECT SINGLE centro_real
        FROM zsdt_depara_cen
        INTO @DATA(w_werks)
        WHERE centrov_1 EQ @wa_saida-werks.
        IF w_werks IS NOT INITIAL.
          zib_contabil_wa-gsber       = w_werks.
        ELSE.
          zib_contabil_wa-gsber       = wa_saida-werks.
        ENDIF.
      ENDIF.

*      "De => para centro virtual.
*      SELECT SINGLE centro_real
*      FROM zsdt_depara_cen
*      INTO @DATA(w_werks)
*      WHERE centrov_1 EQ @wa_saida-werks.
*      IF w_werks IS NOT INITIAL.
*        zib_contabil_wa-gsber       = w_werks.
*      ELSE.
*        zib_contabil_wa-gsber       = wa_saida-werks.
*      ENDIF.
      "US 170226 - PQ - INICIO
      CLEAR: w_werks.

      zib_contabil_wa-bukrs       = wa_saida-bukrs_vf.
      zib_contabil_wa-interface   = '96'.
      IF p_ins IS NOT INITIAL.
        zib_contabil_wa-bktxt       = 'Recbto Venda Insumos'.
      ELSEIF p_mi IS NOT INITIAL.
        zib_contabil_wa-bktxt       = 'Recbto Venda M.I'.
      ENDIF.
      zib_contabil_wa-bldat       = data.
      zib_contabil_wa-budat       = data.
      zib_contabil_wa-gjahr       = ano.
      zib_contabil_wa-monat       = mes.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_lanc-vbeln
        IMPORTING
          output = zib_contabil_wa-xblnr.

      IF p_ins IS NOT INITIAL.
        IF ( wa_lanc-forma_pag EQ 'P' ).
          zib_contabil_wa-blart       = 'ND'.
        ELSE.
          zib_contabil_wa-blart       = 'NC'.
        ENDIF.
      ELSEIF p_mi IS NOT INITIAL.
        IF ( wa_lanc-forma_pag EQ 'P' ).
          zib_contabil_wa-blart       = 'NM'.
        ELSE.
          zib_contabil_wa-blart       = 'NL'.
        ENDIF.
      ENDIF.

      " Lançamento - Conta de Débito
      IF p_ins IS NOT INITIAL.
        CONCATENATE 'Ordem de Venda Insumos' wa_lanc-vbeln INTO sgtxt SEPARATED BY space.
      ELSEIF p_mi IS NOT INITIAL.
        CONCATENATE 'Ordem de Venda M.I.' wa_lanc-vbeln INTO sgtxt SEPARATED BY space.
      ENDIF.

      zib_contabil_wa-bschl          = '09'.
      zib_contabil_wa-hkont          = wa_saida-kunnr.
      zib_contabil_wa-waers          = wa_lanc-moeda.
      zib_contabil_wa-zfbdt          = data.
      zib_contabil_wa-zlspr          = ' '.
      zib_contabil_wa-zlsch          = wa_lanc-forma_pag.
      zib_contabil_wa-kidno          = space.
      zib_contabil_wa-sgtxt          = sgtxt.
      zib_contabil_wa-xref1          = space.
      zib_contabil_wa-xref2          = space.
      zib_contabil_wa-xref3          = space.

      "US 170226 - PQ - INICIO
      IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA
        zib_contabil_wa-bupla  = ''.
      ELSE.
        "De => para centro virtual.
        SELECT SINGLE centro_real
        FROM zsdt_depara_cen
        INTO w_werks
        WHERE centrov_1 EQ wa_saida-werks.
        IF w_werks IS NOT INITIAL.
          zib_contabil_wa-bupla       = w_werks.
        ELSE.
          zib_contabil_wa-bupla       = wa_saida-werks.
        ENDIF.
      ENDIF.

*      "De => para centro virtual.
*      SELECT SINGLE centro_real
*      FROM zsdt_depara_cen
*      INTO w_werks
*      WHERE centrov_1 EQ wa_saida-werks.
*      IF w_werks IS NOT INITIAL.
*        zib_contabil_wa-bupla       = w_werks.
*      ELSE.
*        zib_contabil_wa-bupla       = wa_saida-werks.
*      ENDIF.
      "US 170226 - PQ - FIM
      CLEAR: w_werks.

*      ZIB_CONTABIL_WA-BUPLA          = WA_SAIDA-VKBUR.
      zib_contabil_wa-zterm          = wa_lanc-zterm.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_lanc-vbeln
        IMPORTING
          output = zib_contabil_wa-zuonr.

      IF p_ins IS NOT INITIAL.
        zib_contabil_wa-umskz          = 'L'.
      ELSEIF p_mi IS NOT INITIAL.
        zib_contabil_wa-umskz          = 'A'.
      ENDIF.

      zib_contabil_wa-kostl          = space.
      zib_contabil_wa-aufnr          = space.
      zib_contabil_wa-prctr          = space.
      "US 170226 - PQ - INICIO
      IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA
        zib_contabil_wa-waers_i   = 'ARS'.
      ELSE.
        zib_contabil_wa-waers_i   = 'BRL'.
      ENDIF.
*        zib_contabil_wa-waers_i   = 'BRL'.
      "US 170226 - PQ - FIM

      zib_contabil_wa-waers_f        = wa_lanc-moeda.

      IF ( wa_lanc-moeda EQ 'USD' ).
        zib_contabil_wa-wrbtr          = wa_lanc-mont_moeda + ( wa_lanc-vlr_juros_rbdo + wa_lanc-vlr_multa_rbdo ).
        zib_contabil_wa-dmbtr          = wa_lanc-mont_mi    + ( ( wa_lanc-vlr_juros_rbdo + wa_lanc-vlr_multa_rbdo ) * wa_lanc-taxa ).
        zib_contabil_wa-dmbe2          = wa_lanc-mont_moeda + ( wa_lanc-vlr_juros_rbdo + wa_lanc-vlr_multa_rbdo ).
      ELSE.
        zib_contabil_wa-wrbtr          = wa_lanc-mont_moeda + ( wa_lanc-vlr_juros_rbdo + wa_lanc-vlr_multa_rbdo ).
        zib_contabil_wa-dmbtr          = wa_lanc-mont_mi    + ( wa_lanc-vlr_juros_rbdo + wa_lanc-vlr_multa_rbdo ).
        zib_contabil_wa-dmbe2          = space.
      ENDIF.

      zib_contabil_wa-bvtyp          = space.
      zib_contabil_wa-hbkid          = space.
      zib_contabil_wa-rg_atualizado  = 'N'.
      zib_contabil_wa-bankl          = space.
      zib_contabil_wa-bankn          = space.
      zib_contabil_wa-newbw          = space.
      zib_contabil_wa-anln1          = space.
      zib_contabil_wa-anln2          = space.

      INSERT INTO zib_contabil VALUES zib_contabil_wa.

      CLEAR: zib_contabil_wa.

      zib_contabil_wa-obj_key     = obj_key.
      zib_contabil_wa-seqitem     = '0002'.

      "US 170226 - PQ - INICIO
      IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA
        zib_contabil_wa-gsber = 'T001'.
      ELSE.
        "De => para centro virtual.
        SELECT SINGLE centro_real
        FROM zsdt_depara_cen
        INTO w_werks
        WHERE centrov_1 EQ wa_saida-werks.
        IF w_werks IS NOT INITIAL.
          zib_contabil_wa-gsber       = w_werks.
        ELSE.
          zib_contabil_wa-gsber       = wa_saida-werks.
        ENDIF.
      ENDIF.


*      "De => para centro virtual.
*      SELECT SINGLE centro_real
*      FROM zsdt_depara_cen
*      INTO w_werks
*      WHERE centrov_1 EQ wa_saida-werks.
*      IF w_werks IS NOT INITIAL.
*        zib_contabil_wa-gsber       = w_werks.
*      ELSE.
*        zib_contabil_wa-gsber       = wa_saida-werks.
*      ENDIF.
      "US 170226 - PQ - FIM
      CLEAR: w_werks.

      zib_contabil_wa-bukrs       = wa_saida-bukrs_vf.
      zib_contabil_wa-interface   = '96'.
      IF p_ins IS NOT INITIAL.
        zib_contabil_wa-bktxt       = 'Recbto Venda Insumos'.
      ELSEIF p_mi IS NOT INITIAL.
        zib_contabil_wa-bktxt       = 'Recbto Venda M.I'.
      ENDIF.
      zib_contabil_wa-bldat       = data.
      zib_contabil_wa-budat       = data.
      zib_contabil_wa-gjahr       = ano.
      zib_contabil_wa-monat       = mes.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_lanc-vbeln
        IMPORTING
          output = zib_contabil_wa-xblnr.

      IF wa_lanc-mont_moeda NE 0.
        zib_contabil_wa-bschl          = '19'.
        zib_contabil_wa-hkont          = wa_saida-kunnr.

        IF p_ins IS NOT INITIAL.
          IF ( wa_lanc-forma_pag EQ 'P' ).
            zib_contabil_wa-blart       = 'ND'.
          ELSE.
            zib_contabil_wa-blart       = 'NC'.
          ENDIF.
        ELSEIF p_mi IS NOT INITIAL.
          IF ( wa_lanc-forma_pag EQ 'P' ).
            zib_contabil_wa-blart       = 'NM'.
          ELSE.
            zib_contabil_wa-blart       = 'NL'.
          ENDIF.
        ENDIF.


        zib_contabil_wa-waers          = wa_lanc-moeda.
        zib_contabil_wa-zfbdt          = data.
        zib_contabil_wa-zlspr          = ' '.
        zib_contabil_wa-zlsch          = wa_lanc-forma_pag.
        zib_contabil_wa-kidno          = space.
        zib_contabil_wa-sgtxt          = sgtxt.
        zib_contabil_wa-xref1          = space.
        zib_contabil_wa-xref2          = space.
        zib_contabil_wa-xref3          = space.

        "US 170226 - PQ - INICIO
        IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA
          zib_contabil_wa-bupla  = ''.
        ELSE.
          "De => para centro virtual.
          SELECT SINGLE centro_real
          FROM zsdt_depara_cen
          INTO w_werks
          WHERE centrov_1 EQ wa_saida-werks.
          IF w_werks IS NOT INITIAL.
            zib_contabil_wa-bupla       = w_werks.
          ELSE.
            zib_contabil_wa-bupla       = wa_saida-werks.
          ENDIF.
        ENDIF.

*        "De => para centro virtual.
*        SELECT SINGLE centro_real
*        FROM zsdt_depara_cen
*        INTO w_werks
*        WHERE centrov_1 EQ wa_saida-werks.
*        IF w_werks IS NOT INITIAL.
*          zib_contabil_wa-bupla       = w_werks.
*        ELSE.
*          zib_contabil_wa-bupla       = wa_saida-werks.
*        ENDIF.
        "US 170226 - PQ - FIM
        CLEAR: w_werks.

*      ZIB_CONTABIL_WA-BUPLA          = WA_SAIDA-VKBUR.
*      ZIB_CONTABIL_WA-ZTERM          = WA_LANC-ZTERM.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_lanc-vbeln
          IMPORTING
            output = zib_contabil_wa-zuonr.

        IF p_ins IS NOT INITIAL.
          zib_contabil_wa-umskz          = 'L'.
        ELSEIF p_mi IS NOT INITIAL.
          zib_contabil_wa-umskz          = 'A'.
        ENDIF.

        zib_contabil_wa-kostl          = space.
        zib_contabil_wa-aufnr          = space.
        zib_contabil_wa-prctr          = space.

        "US 170226 - PQ - INICIO
        IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA
          zib_contabil_wa-waers_i   = 'ARS'.
        ELSE.
          zib_contabil_wa-waers_i   = 'BRL'.
        ENDIF.
*        zib_contabil_wa-waers_i   = 'BRL'.
        "US 170226 - PQ - FIM

        zib_contabil_wa-waers_f        = wa_lanc-moeda.

        IF ( wa_lanc-moeda EQ 'USD' ).
          zib_contabil_wa-wrbtr          = wa_lanc-mont_moeda.
          zib_contabil_wa-dmbtr          = wa_lanc-mont_mi.
          zib_contabil_wa-dmbe2          = wa_lanc-mont_moeda.
        ELSE.
          zib_contabil_wa-wrbtr          = wa_lanc-mont_moeda.
          zib_contabil_wa-dmbtr          = wa_lanc-mont_mi.
          zib_contabil_wa-dmbe2          = space.
        ENDIF.

        zib_contabil_wa-bvtyp          = space.
        zib_contabil_wa-hbkid          = space.
        zib_contabil_wa-rg_atualizado  = 'N'.
        zib_contabil_wa-bankl          = space.
        zib_contabil_wa-bankn          = space.
        zib_contabil_wa-newbw          = space.
        zib_contabil_wa-anln1          = space.
        zib_contabil_wa-anln2          = space.

        INSERT INTO zib_contabil VALUES zib_contabil_wa.

      ENDIF.
      CLEAR zib_contabil_wa.

      DATA x_total TYPE netwr.

      x_total =  wa_lanc-vlr_multa_rbdo + wa_lanc-vlr_juros_rbdo.

      IF x_total > 0.
        IF p_ins IS NOT INITIAL.
          CONCATENATE 'Receita de Juros sobre OV. Insumos' wa_lanc-vbeln INTO sgtxt SEPARATED BY space.
        ELSEIF p_mi IS NOT INITIAL.
          CONCATENATE 'Receita de Juros sobre OV. M.I.' wa_lanc-vbeln INTO sgtxt SEPARATED BY space.
        ENDIF.

        zib_contabil_wa-obj_key   = obj_key.
        zib_contabil_wa-seqitem   = '0003'.

        "US 170226 - PQ - INICIO
        IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA
          zib_contabil_wa-gsber = 'T001'.
        ELSE.
          "De => para centro virtual.
          SELECT SINGLE centro_real
          FROM zsdt_depara_cen
          INTO w_werks
          WHERE centrov_1 EQ wa_saida-werks.
          IF w_werks IS NOT INITIAL.
            zib_contabil_wa-gsber     = w_werks.
          ELSE.
            zib_contabil_wa-gsber     = wa_saida-werks.
          ENDIF.
        ENDIF.

*        "De => para centro virtual.
*        SELECT SINGLE centro_real
*        FROM zsdt_depara_cen
*        INTO w_werks
*        WHERE centrov_1 EQ wa_saida-werks.
*        IF w_werks IS NOT INITIAL.
*          zib_contabil_wa-gsber     = w_werks.
*        ELSE.
*          zib_contabil_wa-gsber     = wa_saida-werks.
*        ENDIF.
        "US 170226 - PQ - FIM
        CLEAR: w_werks.


        zib_contabil_wa-bukrs     = wa_saida-bukrs_vf.
        zib_contabil_wa-interface = '96'.

        IF p_ins IS NOT INITIAL.
          zib_contabil_wa-bktxt = 'Recbto Juros/multa Insumos'.
        ELSEIF p_mi IS NOT INITIAL.
          zib_contabil_wa-bktxt = 'Recbto Juros/multa M.I'.
        ENDIF.

        zib_contabil_wa-bldat = data.
        zib_contabil_wa-budat = data.
        zib_contabil_wa-gjahr = ano.
        zib_contabil_wa-monat = mes.
        zib_contabil_wa-xblnr = |{ wa_lanc-vbeln ALPHA = OUT }|.

        zib_contabil_wa-bschl = '50'.

        SELECT SINGLE * FROM kna1
             INTO  wa_kna1
           WHERE kunnr EQ  wa_saida-kunnr.

        zib_contabil_wa-vbund = wa_kna1-vbund.

        IF zib_contabil_wa-vbund EQ 'SOCIOS'.
          zib_contabil_wa-hkont          = '331004'.
        ELSE.
          zib_contabil_wa-hkont          = '331002'.
        ENDIF.

        IF p_ins IS NOT INITIAL.
          IF ( wa_lanc-forma_pag EQ 'P').
            zib_contabil_wa-blart  = 'ND'.
          ELSE.
            zib_contabil_wa-blart  = 'NC'.
          ENDIF.
        ELSEIF p_mi IS NOT INITIAL.
          IF ( wa_lanc-forma_pag EQ 'P').
            zib_contabil_wa-blart  = 'NM'.
          ELSE.
            zib_contabil_wa-blart  = 'NL'.
          ENDIF.
        ENDIF.

        zib_contabil_wa-waers     = wa_lanc-moeda.
        zib_contabil_wa-zfbdt     = data.
        zib_contabil_wa-zlspr     = ' '.
        zib_contabil_wa-zlsch     = wa_lanc-forma_pag.
        zib_contabil_wa-kidno     = space.
        zib_contabil_wa-sgtxt     = sgtxt.
        zib_contabil_wa-xref1     = space.
        zib_contabil_wa-xref2     = space.
        zib_contabil_wa-xref3     = space.

        "US 170226 - PQ - INICIO
        IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA
          zib_contabil_wa-bupla  = ''.
        ELSE.
          "De => para centro virtual.
          SELECT SINGLE centro_real
          FROM zsdt_depara_cen
          INTO w_werks
          WHERE centrov_1 EQ wa_saida-werks.
          IF w_werks IS NOT INITIAL.
            zib_contabil_wa-bupla     = w_werks.
          ELSE.
            zib_contabil_wa-bupla     = wa_saida-werks.
          ENDIF.
        ENDIF.

*        "De => para centro virtual.
*        SELECT SINGLE centro_real
*        FROM zsdt_depara_cen
*        INTO w_werks
*        WHERE centrov_1 EQ wa_saida-werks.
*        IF w_werks IS NOT INITIAL.
*          zib_contabil_wa-bupla     = w_werks.
*        ELSE.
*          zib_contabil_wa-bupla     = wa_saida-werks.
*        ENDIF.
        "US 170226 - PQ - INICIO
        CLEAR: w_werks.

*        ZIB_CONTABIL_WA-BUPLA     = WA_SAIDA-VKBUR.

        zib_contabil_wa-zuonr = |{ wa_lanc-vbeln ALPHA = OUT }|.

*        IF P_INS IS NOT INITIAL.
*          ZIB_CONTABIL_WA-UMSKZ = 'L'.
*        ELSEIF P_MI IS NOT INITIAL.
*          ZIB_CONTABIL_WA-UMSKZ = 'A'.
*        ENDIF.

        zib_contabil_wa-kostl     = space.
        zib_contabil_wa-aufnr     = space.
        zib_contabil_wa-prctr     = space.
        "US 170226 - PQ - INICIO
        IF wa_saida-bukrs_vf EQ '0100'. "ARGENTINA
          zib_contabil_wa-waers_i   = 'ARS'.
        ELSE.
          zib_contabil_wa-waers_i   = 'BRL'.
        ENDIF.
*        zib_contabil_wa-waers_i   = 'BRL'.
        "US 170226 - PQ - FIM

        zib_contabil_wa-waers_f   = wa_lanc-moeda.

        IF ( wa_lanc-moeda EQ 'USD' ).
          zib_contabil_wa-wrbtr = ( wa_lanc-vlr_juros_rbdo + wa_lanc-vlr_multa_rbdo ).
          zib_contabil_wa-dmbtr = ( ( wa_lanc-vlr_juros_rbdo + wa_lanc-vlr_multa_rbdo ) * wa_lanc-taxa ).
          zib_contabil_wa-dmbe2 = ( wa_lanc-vlr_juros_rbdo + wa_lanc-vlr_multa_rbdo ).
        ELSE.
          zib_contabil_wa-wrbtr = ( wa_lanc-vlr_juros_rbdo + wa_lanc-vlr_multa_rbdo ).
          zib_contabil_wa-dmbtr = ( wa_lanc-vlr_juros_rbdo + wa_lanc-vlr_multa_rbdo ).
          zib_contabil_wa-dmbe2 = space.
        ENDIF.

        zib_contabil_wa-bvtyp          = space.
        zib_contabil_wa-hbkid          = space.
        zib_contabil_wa-rg_atualizado  = 'N'.
        zib_contabil_wa-bankl          = space.
        zib_contabil_wa-bankn          = space.
        zib_contabil_wa-newbw          = space.
        zib_contabil_wa-anln1          = space.
        zib_contabil_wa-anln2          = space.

        INSERT INTO zib_contabil VALUES zib_contabil_wa.
        CLEAR zib_contabil_wa.

      ENDIF.

      IF ( sy-subrc EQ 0 ).

        UPDATE zfit0026 SET seq     = seq_novo
                            obj_key = obj_key
                            status  = 'P'
                       WHERE vbeln  EQ wa_lanc-vbeln
                         AND seq    EQ wa_lanc-seq.


        IF ( sy-subrc EQ 0 ).

          "Grava a taxa na ordem
          IF wa_lanc-moeda EQ 'USD'.
            PERFORM: update_taxa USING wa_lanc-taxa
                                       wa_lanc-vbeln.
          ENDIF.

          "PERFORM: select_data_refresh.  "152772 - CS2023000317 - ZFIS26 - AJUSTE GERAR DOC EM MASSA E CALC JUROS/MULTA PSA

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " GERAR_CONTABIL
*&---------------------------------------------------------------------*
*&      Form  CAIXA_TXT_OBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM caixa_txt_obs .

  CHECK ok_code NE 'EXECUTE'.

  IF NOT c_editor IS INITIAL AND NOT editor IS INITIAL.
    CALL METHOD c_editor->free( ).
    CALL METHOD editor->free( ).
  ENDIF.

  CREATE OBJECT: c_editor EXPORTING container_name = 'C_EDITOR', "CONTAINER
                   editor EXPORTING parent         = c_editor.

  CALL METHOD editor->set_toolbar_mode( toolbar_mode = editor->false ).
  CALL METHOD editor->set_statusbar_mode( statusbar_mode = editor->false ).

  CASE txtopen.
    WHEN 'X'.
      CALL METHOD editor->set_readonly_mode( readonly_mode = editor->false ).
    WHEN OTHERS.
      CALL METHOD editor->set_readonly_mode( readonly_mode = editor->true ).
  ENDCASE.

  cl_gui_cfw=>flush( ). "BUG - 174094 - CBRAND

  CALL METHOD editor->set_text_as_stream
    EXPORTING
      text = it_editor.


  FREE: txtopen.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  PERFORM caixa_txt_obs.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_EDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LANC  text
*----------------------------------------------------------------------*
FORM preenche_edit.

  DATA kurrf TYPE vbkd-kurrf.

  MOVE-CORRESPONDING wa_lanc TO wa_edit.

  IF wa_edit-moeda EQ 'USD'.
    CLEAR: kurrf.
    SELECT SINGLE kurrf
      FROM zsdt0090
      INTO  kurrf
      WHERE vbelv     EQ wa_edit-vbeln
        AND categoria EQ 'C'
        AND estorno   NE 'X'
      AND kurrf IS NOT NULL.


    IF sy-subrc IS NOT INITIAL.

      SELECT SINGLE kurrf
        FROM vbkd
        INTO  kurrf
        WHERE vbeln EQ wa_edit-vbeln.

    ENDIF.

    wa_edit-taxa = kurrf.

  ELSE.
    wa_edit-taxa = ''.
  ENDIF.

  wa_edit-mont_moeda      = ''.
  wa_edit-mont_mi         = ''.
  wa_edit-forma_pag       = ''.
  wa_edit-zterm           = ''.
  wa_edit-ajuste          = ''.
  wa_edit-data_pgto       = ''.
  wa_edit-mont_rbdo       = ''.
  wa_edit-vlr_multa_calc  = ''.
  wa_edit-vlr_juros_calc  = ''.
  wa_edit-vlr_multa_rbdo  = ''.
  wa_edit-vlr_juros_rbdo  = ''.
  wa_edit-nfenum          = ''.
*  wa_edit-NUM_COMP_ADIANT = ''. "SMC ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337


*  IF WA_SAIDA-AUART IN R_DEVO_RECU .
*    WA_LANC-MONT_MOEDA_PARC = WA_LANC-MONT_MOEDA * -1 .
*  ELSE.
  wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  UPDATE_TAXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LANC_TAXA  text
*----------------------------------------------------------------------*
FORM update_taxa  USING    p_wa_lanc_taxa
                           p_wa_lanc_vbeln.

  DATA: wl_orderheaderin  TYPE bapisdh1,
        wl_orderheaderinx TYPE bapisdh1x,
        wl_return         TYPE bapiret2,
        tl_return         TYPE STANDARD TABLE OF bapiret2.

  wl_orderheaderinx-updateflag = 'U'.
  wl_orderheaderin-exrate_fi = p_wa_lanc_taxa.
  wl_orderheaderinx-exrate_fi = abap_true.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      salesdocument    = p_wa_lanc_vbeln
      order_header_in  = wl_orderheaderin
      order_header_inx = wl_orderheaderinx
    TABLES
      return           = tl_return.

  CLEAR:wl_return.
  READ TABLE tl_return INTO wl_return WITH KEY type = 'E'.

  IF sy-subrc NE 0.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

  ELSE.
    MESSAGE s888(sabapdocu) WITH 'Taxa não pode ser atualizada'.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ESTORNA_TAXA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM estorna_taxa USING p_wa_lanc_vbeln
                  CHANGING vl_erro TYPE char1.

  DATA: wl_orderheaderin  TYPE bapisdh1,
        wl_orderheaderinx TYPE bapisdh1x,
        wl_return         TYPE bapiret2,
        tl_return         TYPE STANDARD TABLE OF bapiret2,
        it_zfit0026       TYPE STANDARD TABLE OF zfit0026,
        wa_zfit0026       TYPE zfit0026,
        v_taxa            TYPE zfit0026-taxa.

  SELECT *
    FROM zfit0026
    INTO TABLE it_zfit0026
    WHERE vbeln EQ wa_lanc-vbeln
    AND seq NE wa_lanc-seq
    AND docnum NE space.

  IF it_zfit0026 IS INITIAL.
    MOVE 0 TO v_taxa.
  ELSE.
    SORT it_zfit0026 BY docnum DESCENDING.
    READ TABLE it_zfit0026 INTO wa_zfit0026 INDEX 1.
    MOVE wa_zfit0026-taxa TO v_taxa.
  ENDIF.

  wl_orderheaderinx-updateflag = 'U'.
  wl_orderheaderin-exrate_fi = v_taxa.
  wl_orderheaderinx-exrate_fi = abap_true.

  CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      salesdocument    = p_wa_lanc_vbeln
      order_header_in  = wl_orderheaderin
      order_header_inx = wl_orderheaderinx
    TABLES
      return           = tl_return.

  CLEAR:wl_return.
  READ TABLE tl_return INTO wl_return WITH KEY type = 'E'.

  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ELSE.
    MESSAGE s888(sabapdocu) WITH 'Taxa não pode ser atualizada'.
    MOVE abap_true TO vl_erro.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  F4_FORM_PGTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_form_pgto INPUT.

  TYPES: BEGIN OF ty_f4_form_pgto,
           zlsch TYPE t042z-zlsch,
           text1 TYPE t042z-text1.
  TYPES: END OF ty_f4_form_pgto.

  DATA: it_form_pgto  TYPE STANDARD TABLE OF ty_f4_form_pgto.

  SELECT zlsch text1
    FROM t042z
    INTO TABLE it_form_pgto
  WHERE land1 EQ 'BR'.

  SORT it_form_pgto BY zlsch.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ZLSCH'
      dynpprog        = sy-repid               " Program name
      dynpnr          = sy-dynnr               " Screen number
      dynprofield     = 'WA_EDIT-FORMA_PAG'    " F4 help need field
      value_org       = 'S'
    TABLES
      value_tab       = it_form_pgto           " F4 help values
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

ENDMODULE.

FORM desc_zterm.

  CLEAR txtzterm.

  FREE: it_form_pgto.
  IF wa_edit-forma_pag IS NOT INITIAL.
    SELECT zlsch text1
     FROM t042z
     INTO TABLE it_form_pgto
   WHERE land1 EQ 'BR'
     AND zlsch EQ wa_edit-forma_pag.

    IF it_form_pgto IS INITIAL.
      MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Forma de pagamento não cadastrado.'.
      CLEAR: wa_edit-forma_pag.
    ENDIF.
  ENDIF.

  IF wa_edit-zterm IS NOT INITIAL.
    SELECT SINGLE vtext
     FROM tvzbt
     INTO txtzterm
      WHERE spras EQ sy-langu
        AND zterm EQ wa_edit-zterm.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Condição de pagamento não cadastrado.'.
      CLEAR: wa_edit-zterm, txtzterm.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_TELA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atualiza_tela INPUT.
  PERFORM desc_zterm.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_set .

  FREE: it_value, p_spart.

  IF  p_ins IS NOT INITIAL.
    DATA(vsetnr) = 'ZFIS26_SA_IN'.
  ELSE.
    vsetnr  = 'ZFIS26_SA_MI'.
  ENDIF.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr         = vsetnr    " Set ID
      class         = '0000'    " Obsolete, Use ID in SETNR Parameter
    TABLES
      set_values    = it_value   " Set Value Table
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.


  CHECK it_value IS NOT INITIAL.

  CLEAR p_spart.
  LOOP AT it_value.
    w_spart-sign    = 'I'.
    w_spart-option  = 'EQ'.
    w_spart-low     =  it_value-from.
    APPEND w_spart TO p_spart.
    CLEAR w_spart.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0301 OUTPUT.

  SET PF-STATUS 'PF0301'.
  SET TITLEBAR 'TL_0301'.

  PERFORM alv_f.

  IF ( cl_custom_f IS INITIAL ).

    CREATE OBJECT cl_custom_f
      EXPORTING
        container_name              = 'CONTAINER_F'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT grid_faturamento
      EXPORTING
        i_parent          = cl_custom_f
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

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
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    "    CREATE OBJECT GR_EVENT_HANDLER.
    "    SET HANDLER GR_EVENT_HANDLER->HANDLE_HOTSPOT_CLICK FOR GRID_PRINCIPAL.


    CALL METHOD grid_faturamento->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout_f
        i_save                        = c_a
        it_toolbar_excluding          = tl_function
      CHANGING
        it_outtab                     = it_fatu[]
        it_fieldcatalog               = it_fieldcatalog_f[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    CALL METHOD grid_faturamento->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD grid_faturamento->refresh_table_display
      EXPORTING
        is_stable = wa_stable_f.


  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0301 INPUT.
  CASE sy-ucomm.
    WHEN 'OK'.

      CALL METHOD grid_faturamento->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF tg_selectedrow[] IS INITIAL.
        MESSAGE 'Favor selecione uma linha!'    TYPE 'S'.
        EXIT.
      ELSE.

        READ TABLE tg_selectedrow INTO wg_selectedrow INDEX 1.

        READ TABLE it_fatu INTO wa_fatu INDEX wg_selectedrow-index.

        wa_edit-nfenum             = wa_fatu-nfenum.
        wa_edit-total_recebido     = wa_fatu-total_recebido.
        wa_edit-valor              = wa_fatu-valor.
        wa_edit-data_venc  = wa_fatu-valdt.
        wa_edit-doc_fatura = wa_fatu-doc_fatura.

        LEAVE TO SCREEN 0.

        CALL METHOD grid_faturamento->refresh_table_display
          EXPORTING
            is_stable = wa_stable_f.
      ENDIF.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  Z_BUSCA_DADOS_FATU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_busca_dados_fatu .

  DATA: x_total_recebido TYPE zfit0026-mont_rbdo,
        j_m_calculado    TYPE zfit0026-mont_rbdo,
        j_m_recebido     TYPE zfit0026-mont_rbdo.
*        VAR_DATA          TYPE DATUM,
*        VAR_DATA_COMPLETA TYPE DATUM,
*        VAR_MES_AUX       TYPE C LENGTH 2,
*        VAR_ANO           TYPE C LENGTH 4,
*        VAR_MES           TYPE I.

  FREE: it_fatu[].

  SELECT  *  FROM vbfa INTO TABLE @DATA(it_vbfa)
      WHERE vbelv   EQ @wa_saida-vbeln
      AND   vbtyp_n EQ 'M'
      AND   vbtyp_v EQ 'C'.

  SELECT  *  FROM vbfa APPENDING TABLE it_vbfa
      WHERE vbelv   EQ wa_saida-vbeln
      AND   vbtyp_n EQ 'P'
      AND   vbtyp_v EQ 'L'.

  IF it_vbfa IS NOT INITIAL.

    SELECT *  FROM vbrk INTO TABLE @DATA(it_vbrk)
     FOR ALL ENTRIES IN @it_vbfa
      WHERE vbeln EQ @it_vbfa-vbeln
      AND   fksto NE 'X'.

  ENDIF.
  DATA: vlr_desc_juros TYPE zfit0026-vlr_desc_mult.
  DATA: vlr_desc_multa TYPE zfit0026-vlr_desc_mult.

  LOOP AT it_vbfa INTO DATA(wa_vbfa).
    LOOP AT it_lanc_ver INTO wa_lanc_ver
      WHERE doc_fatura EQ wa_vbfa-vbeln.
      x_total_recebido  = x_total_recebido + wa_lanc_ver-mont_moeda.
      j_m_calculado     = ( j_m_calculado + wa_lanc_ver-vlr_multa_calc + wa_lanc_ver-vlr_juros_calc ).
      j_m_recebido      = ( j_m_recebido + wa_lanc_ver-vlr_multa_rbdo + wa_lanc_ver-vlr_juros_rbdo ).
      vlr_desc_juros    = ( vlr_desc_juros + wa_lanc_ver-vlr_desc_jros ).
      vlr_desc_multa    = ( vlr_desc_multa + wa_lanc_ver-vlr_desc_mult ).
    ENDLOOP.


    READ TABLE it_vbrk INTO DATA(wa_vbrk) WITH KEY vbeln = wa_vbfa-vbeln.
    IF sy-subrc = 0.

      wa_fatu-doc_fatura = wa_vbrk-vbeln.
      wa_fatu-valdt      = wa_vbrk-valdt.
      wa_fatu-valor      = ( wa_vbrk-netwr + wa_vbrk-mwsbk ).

      SELECT SINGLE * FROM j_1bnflin INTO @DATA(wa_lin)
      WHERE refkey EQ @wa_vbfa-vbeln.

      SELECT SINGLE * FROM j_1bnfdoc INTO @DATA(wa_doc)
        WHERE docnum EQ @wa_lin-docnum.

      wa_fatu-nfenum = wa_doc-nfenum.
      wa_fatu-refkey = wa_lin-refkey.

      wa_fatu-total_recebido = ( wa_vbrk-netwr + wa_vbrk-mwsbk ) - x_total_recebido.
      wa_fatu-saldo_finan    = ( j_m_calculado - j_m_recebido - vlr_desc_juros - vlr_desc_multa ).

*       WA_SAIDA-VLR_SALD_FIN = ( WA_ZFIT0026-VLR_JUROS_CALC - WA_ZFIT0026-VLR_JUROS_RBDO - WA_ZFIT0026-VLR_DESC_JROS ) + ( WA_ZFIT0026-VLR_MULTA_CALC - WA_ZFIT0026-VLR_MULTA_RBDO - WA_ZFIT0026-VLR_DESC_MULT ).


      SELECT SINGLE * FROM bkpf INTO @DATA(wa_bkpf)
         WHERE awkey EQ @wa_vbfa-vbeln.

      wa_fatu-belnr = wa_bkpf-belnr.

*      ***   Pegando a data de vencimento.
* ---> S4 Migration - 15/06/2023 - MA
*      SELECT SINGLE * FROM bseg INTO @DATA(w_bseg) WHERE belnr EQ @wa_bkpf-belnr
*      AND bukrs EQ @wa_bkpf-bukrs AND gjahr EQ @wa_bkpf-gjahr AND bschl = '01'.
      DATA: lt_bseg TYPE fagl_t_bseg,
            w_bseg  TYPE bseg.

      CALL FUNCTION 'FAGL_GET_BSEG'
        EXPORTING
          i_bukrs   = wa_bkpf-bukrs
          i_belnr   = wa_bkpf-belnr
          i_gjahr   = wa_bkpf-gjahr
        IMPORTING
          et_bseg   = lt_bseg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.

      DELETE lt_bseg WHERE bschl NE '01'.

      READ TABLE lt_bseg INTO DATA(ls_bseg) INDEX 1.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_bseg TO w_bseg.
      ENDIF.
*<--- S4 Migration - 15/06/2023 - MA

      IF sy-subrc EQ 0.
*        WA_FATU-VALDT = W_BSEG-FDTAG.
        "Condições de pagamento

        CALL METHOD zcl_solicitacao_ov=>get_t052_calc
          EXPORTING
            data_in  = w_bseg-zfbdt
            zterm    = w_bseg-zterm
          RECEIVING
            data_out = wa_fatu-valdt.

*        SELECT SINGLE * FROM T052
*          INTO @DATA(GW_T052)
*        WHERE ZTERM EQ @W_BSEG-ZTERM.
*
*        CASE GW_T052-ZDART.
*          WHEN: 'B'.
*            IF NOT ( GW_T052-ZTAG1 IS INITIAL ).
*              WA_FATU-VALDT = W_BSEG-ZFBDT + GW_T052-ZTAG1.
*            ELSE.
*
*              VAR_MES = ( ( VAR_MES + W_BSEG-ZFBDT+4(2) ) + GW_T052-ZMONA ).
*              IF ( VAR_MES > 12 ).
*                VAR_MES_AUX =  GW_T052-ZMONA.
*                VAR_ANO = W_BSEG-ZFBDT(4) + 1.
*              ELSE.
*                VAR_MES_AUX = VAR_MES.
*                VAR_ANO     = W_BSEG-ZFBDT(4).
*              ENDIF.
*
*              CONCATENATE VAR_ANO VAR_MES_AUX VAR_DATA+6(2) INTO VAR_DATA_COMPLETA.
*              VAR_DATA_COMPLETA = VAR_DATA_COMPLETA +  GW_T052-ZFAEL.
*              WA_FATU-VALDT      = VAR_DATA_COMPLETA.
*
*            ENDIF.
*          WHEN OTHERS.
*            WA_FATU-VALDT = W_BSEG-ZFBDT.
*        ENDCASE.

      ENDIF.

      APPEND wa_fatu TO it_fatu.

    ENDIF.

    CLEAR: wa_fatu, x_total_recebido, j_m_calculado, j_m_recebido, vlr_desc_juros, vlr_desc_multa.

  ENDLOOP.

  SORT it_fatu BY doc_fatura.
  DELETE ADJACENT DUPLICATES FROM it_fatu.
* DELETE it_fatu WHERE total_recebido = 0. "// WBARBOSA 24/12/2024 US-115811 BUG-162073

ENDFORM.

FORM alv_f.
  PERFORM fieldcatalog_f USING:
        'NFENUM'          'Nfe'                '09'    ''    ''   ''   ''    '',
        'BELNR'           'Doc.Contábil'       '10'    ''    ''   ''   ''    '',
        'VALDT'           'Data Vencimento'    '16'    ''    ''   ''   ''    '',
        'VALOR'           'Valor'              '10'    ''    ''   ''   ''    '',
        'TOTAL_RECEBIDO'  'Saldo à Receber'    '20'    ''    ''   ''   ''    '',
        'SALDO_FINAN'     'Saldo Financeiro'   '20'    ''    ''   ''   ''    ''.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fieldcatalog_f  USING  VALUE(p_fieldname)
                            VALUE(p_desc)
                            VALUE(p_tam)
                            VALUE(p_no_zero)
                            VALUE(p_hotspot)
                            VALUE(p_cor)
                            VALUE(p_just)
                            VALUE(p_sum).

  wa_fieldcatalog_f-fieldname = p_fieldname.
  wa_fieldcatalog_f-scrtext_l = p_desc.
  wa_fieldcatalog_f-scrtext_m = p_desc.
  wa_fieldcatalog_f-scrtext_s = p_desc.
  wa_fieldcatalog_f-outputlen = p_tam.
  wa_fieldcatalog_f-no_zero   = p_no_zero.
  wa_fieldcatalog_f-hotspot   = p_hotspot.
  wa_fieldcatalog_f-emphasize = p_cor.
  wa_fieldcatalog_f-just      = p_just.
  wa_fieldcatalog_f-do_sum    = p_sum.

  APPEND wa_fieldcatalog_f TO it_fieldcatalog_f.

  CLEAR: wa_fieldcatalog_f.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_VALIDAÇOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_validacoes .

  DATA: t_day_attributes TYPE TABLE OF  casdayattr,
        w_day_attributes TYPE casdayattr,
        dt_ini           TYPE sy-datum,
        dt_fim           TYPE sy-datum,
        v_data_aux       TYPE datum,
        v_mont_aux       TYPE zfit0026-vlr_multa_rbdo.

  DATA: vlr_jros TYPE p DECIMALS 2.
  DATA: vlr_mult TYPE p DECIMALS 2.
  DATA: fat_prop TYPE p DECIMALS 9.
  DATA: prop_juros TYPE p DECIMALS 9.
  DATA: prop_multa TYPE p DECIMALS 9.


  IF wa_edit-mont_rbdo EQ ' '.
    CLEAR: wa_edit-vlr_juros_rbdo, wa_edit-vlr_multa_rbdo, wa_edit-mont_moeda.
  ENDIF.

  IF wa_edit-mont_rbdo NE ' ' AND wa_edit-data_pgto EQ ' '.
    MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Preencher a data de pagamento.'.

    wa_edit-mont_rbdo      = ' '.
    wa_edit-mont_moeda     = ' '.
    wa_edit-vlr_multa_rbdo = ' '.
    wa_edit-vlr_juros_rbdo = ' '.
  ENDIF.

  IF ( wa_edit-data_pgto EQ ' ' ).
    wa_edit-mont_rbdo      = ' '.
    wa_edit-mont_moeda     = ' '.
    wa_edit-vlr_multa_rbdo = ' '.
    wa_edit-vlr_juros_rbdo = ' '.
  ENDIF.

  PERFORM desc_zterm.
**<<<------"163316 - NMS - INI------>>>
* Valida a existencia do Documento Contábil.
  PERFORM zf_doc_cont_check.
**<<<------"163316 - NMS - FIM------>>>
  IF ( wa_edit-data_pgto <> '00000000' AND wa_edit-data_venc <> '00000000' AND wa_edit-mont_rbdo <> ' ' ).

    "Mercado Interno
    SELECT SINGLE * FROM zsdt0053 INTO @DATA(wa_zsdt0053)
     WHERE vbeln EQ @wa_edit-vbeln.

    IF sy-subrc = 0.
      SELECT SINGLE *  FROM zsdt0051 INTO @DATA(wa_zsdt0051)
       WHERE nro_sol_ov EQ @wa_zsdt0053-nro_sol_ov.
    ENDIF.

    "Insumos
    SELECT  SINGLE * FROM zsdt0090 INTO wa_zsdt0090
    WHERE vbeln EQ wa_edit-vbeln.

    IF sy-subrc = 0.
      SELECT SINGLE * FROM zsdt0040 INTO wa_zsdt0040
        WHERE doc_simulacao EQ wa_zsdt0090-doc_simulacao.
    ELSE.

      SELECT SINGLE * FROM zsdt0041  INTO wa_zsdt0041
      WHERE vbeln EQ wa_edit-vbeln.

      IF sy-subrc = 0.
        SELECT SINGLE * FROM zsdt0040 INTO wa_zsdt0040
        WHERE doc_simulacao EQ wa_zsdt0041-doc_simulacao.
      ENDIF.
    ENDIF.

    vlr_multa = 0.
    vlr_juros = 0.

    " Essa função Retorna o próximo dia util...
    " Caso a data entrada seja dia útil, a função retornar ela mesma.

    zcl_miro=>get_proximo_dia_util(
      EXPORTING
        i_data_base        = wa_edit-data_venc
        i_signum           = '+'
        i_ck_data_zles0145 = abap_true
      RECEIVING
        r_data             = v_data_aux
      EXCEPTIONS
        erro               = 1
        OTHERS             = 2 ).

*    IF V_DATA_AUX <> WA_EDIT-DATA_VENC.
*      DATA(DT_VENC_UTIL) = ABAP_TRUE.
*    ENDIF.
*
*    IF V_DATA_AUX EQ WA_EDIT-DATA_PGTO.
*      DATA(DIA_UTIL) = ABAP_TRUE.
*    ENDIF.


    IF p_mi IS NOT INITIAL.

*      IF DT_VENC_UTIL EQ ABAP_FALSE AND DIA_UTIL EQ ABAP_FALSE.

      IF wa_edit-data_pgto > v_data_aux OR v_mont_aux > wa_edit-mont_rbdo AND wa_edit-data_pgto > v_data_aux.
        IF  calc = abap_false.

          DATA(d_atraso) = ( wa_edit-data_pgto - wa_edit-data_venc ).

*          VLR_MULTA = ( WA_EDIT-MONT_RBDO * ( WA_ZSDT0051-TX_MULTA  / 100 )  ).
*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).

***=====================================================================================================
          CLEAR: fat_prop.
          fat_prop = ( wa_zsdt0051-tx_juros / 360 ) * d_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

          IF wa_edit-rec_vlr_total EQ abap_true. "Se o campo recebimento total for marcado, considerar o juros com base no total da OV.
            IF wa_edit-total_recebido IS NOT INITIAL. "Se o valor da fatura estiver preenchido, fazer o calculo valor da referencia.
*              VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).

              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_edit-total_recebido * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_mult = ( wa_edit-total_recebido * wa_zsdt0051-tx_multa ) / 100.

              vlr_juros = vlr_jros.
              vlr_multa =  vlr_mult.
            ELSE.
*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_lanc-mont_moeda_parc * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_mult = ( wa_lanc-mont_moeda_parc * wa_zsdt0051-tx_multa ) / 100.

              vlr_juros = vlr_jros.
              vlr_multa =  vlr_mult.
            ENDIF.

          ELSE.
            "Se o campo recebimento total não for marcado, considerar o juros com base no recebimento parcial.

*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
            CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
            vlr_jros = ( wa_lanc-mont_moeda_fix * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
            vlr_mult = ( wa_lanc-mont_moeda_fix * wa_zsdt0051-tx_multa ) / 100.


            DATA(total_ov) = ( vlr_jros + wa_lanc-mont_moeda_fix +  vlr_mult ). "Total da OV + juros.
*          DATA(TOTAL_OV_MULT) = ( VLR_MULT + WA_ZSDT0053-VLRTOT ). "Total da OV + juros.

            prop_juros = ( vlr_jros / total_ov )  * 100. " Porcentagem proporcional ao valor do juros
            prop_multa = ( vlr_mult / total_ov )  * 100. " Porcentagem proporcional da multa.

            CLEAR: total_ov.

            vlr_juros = ( prop_juros * wa_edit-mont_rbdo ) / 100.
            vlr_multa =  ( prop_multa * wa_edit-mont_rbdo ) / 100.
          ENDIF.

***=======================================================================================================

          wa_edit-vlr_multa_rbdo = vlr_multa.
          wa_edit-vlr_juros_rbdo = vlr_juros.

          wa_edit-vlr_multa_calc = vlr_multa.
          wa_edit-vlr_juros_calc = vlr_juros.

*          IF WA_EDIT-VLR_MULTA_RBDO > WA_EDIT-VLR_MULTA_CALC.
**            MESSAGE 'Multa recebido  (' && WA_EDIT-VLR_MULTA_RBDO && ' ) não pode ser maior que multa calculado (' && WA_EDIT-VLR_MULTA_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Multa recebido | WA_EDIT-VLR_MULTA_RBDO |, não pode ser maior que multa calculado | WA_EDIT-VLR_MULTA_CALC.
*            CLEAR: WA_EDIT-VLR_MULTA_RBDO.
*            EXIT.
*          ENDIF.

*          IF WA_EDIT-VLR_JUROS_RBDO > WA_EDIT-VLR_JUROS_CALC.
**            MESSAGE 'Juros recebido  ( ' && WA_EDIT-VLR_JUROS_RBDO && ' ) não pode ser maior que juros calculado (' && WA_EDIT-VLR_JUROS_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Juros recebido | WA_EDIT-VLR_JUROS_RBDO |, não pode ser maior que juros calculado | WA_EDIT-VLR_JUROS_CALC.
*            CLEAR: WA_EDIT-VLR_JUROS_RBDO.
*            EXIT.
*          ENDIF.

          wa_edit-mont_moeda = ( wa_edit-mont_rbdo -   wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo  ).
          calc = abap_true.
        ELSE.

          d_atraso  = ( wa_edit-data_pgto - wa_edit-data_venc ).
*          VLR_MULTA = ( WA_EDIT-MONT_RBDO * ( WA_ZSDT0051-TX_MULTA  / 100 )  ).
*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).

***=====================================================================================================
          CLEAR: fat_prop.
          fat_prop = ( wa_zsdt0051-tx_juros / 360 ) * d_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

          IF wa_edit-rec_vlr_total EQ abap_true.
            IF wa_edit-total_recebido IS NOT INITIAL. "Se o valor da fatura estiver preenchido, fazer o calculo com base neste valor.
*              VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_edit-total_recebido * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_mult = ( wa_edit-total_recebido * wa_zsdt0051-tx_multa ) / 100.

              vlr_juros = vlr_jros.
              vlr_multa =  vlr_mult.
            ELSE.

*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_lanc-mont_moeda_parc * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_mult = ( wa_lanc-mont_moeda_parc * wa_zsdt0051-tx_multa ) / 100.

              vlr_juros = vlr_jros.
              vlr_multa =  vlr_mult.

*            WA_EDIT-VLR_JUROS_RBDO  = VLR_JUROS.
*            WA_EDIT-VLR_MULTA_RBDO  = VLR_MULTA.
            ENDIF.

          ELSE.
*         VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
            CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
            vlr_jros = ( wa_lanc-mont_moeda_fix * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
            vlr_mult = ( wa_lanc-mont_moeda_fix * wa_zsdt0051-tx_multa ) / 100.

            total_ov = ( vlr_jros + wa_lanc-mont_moeda_fix + vlr_mult ). "Total da OV + juros.
**          TOTAL_OV_MULT = ( VLR_MULT + WA_ZSDT0053-VLRTOT ). "Total da OV + multa.
            prop_juros = ( vlr_jros / total_ov )  * 100. " Porcentagem proporcional ao valor do juros
            prop_multa = ( vlr_mult / total_ov )  * 100. " Porcentagem proporcional da multa.
            CLEAR: total_ov.

            vlr_juros = ( prop_juros * wa_edit-mont_rbdo ) / 100.
            vlr_multa =  ( prop_multa * wa_edit-mont_rbdo ) / 100.

*            WA_EDIT-VLR_JUROS_RBDO  = VLR_JUROS.
*            WA_EDIT-VLR_MULTA_RBDO  = VLR_MULTA.

          ENDIF.

***=======================================================================================================
          "Se for ajuste não calcular juros nem multa.
          IF wa_edit-ajuste IS NOT INITIAL.
            vlr_juros = ' '.
            vlr_multa = ' '.
          ENDIF.

          "Alterando valor do Montante Recebido ou executar a opção recebimento total.
          IF mont_rbdo_anter NE wa_edit-mont_rbdo OR sy-ucomm EQ 'CHECK_AJUST'.
            mont_rbdo_anter         = wa_edit-mont_rbdo.
            wa_edit-vlr_juros_calc  = vlr_juros.
            wa_edit-vlr_multa_calc  = vlr_multa.
            wa_edit-vlr_juros_rbdo  = vlr_juros.
            wa_edit-vlr_multa_rbdo  = vlr_multa.

          ENDIF.

          "Alterando valor do juros
          IF wa_edit-vlr_juros_rbdo EQ  wa_edit-vlr_juros_calc.
            wa_edit-vlr_juros_rbdo = vlr_juros.
          ENDIF.

          "Alterando data de pagamento
          IF dt_pgto_anter <> wa_edit-data_pgto.
            dt_pgto_anter      = wa_edit-data_pgto.

            wa_edit-vlr_juros_rbdo = vlr_juros.
            wa_edit-vlr_juros_calc = vlr_juros.

            wa_edit-vlr_multa_rbdo = vlr_multa.
            wa_edit-vlr_multa_calc = vlr_multa.
          ENDIF.
          wa_edit-mont_moeda = ( wa_edit-mont_rbdo -   wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo  ).
        ENDIF.

*        IF WA_EDIT-VLR_MULTA_RBDO > WA_EDIT-VLR_MULTA_CALC.
**          MESSAGE 'Multa recebido  (' && WA_EDIT-VLR_MULTA_RBDO && ' ) não pode ser maior que multa calculado (' && WA_EDIT-VLR_MULTA_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*           MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Multa recebido | WA_EDIT-VLR_MULTA_RBDO |, não pode ser maior que multa calculado | WA_EDIT-VLR_MULTA_CALC.
*          CLEAR: WA_EDIT-VLR_MULTA_RBDO.
*          EXIT.
*        ENDIF.

*        IF WA_EDIT-VLR_JUROS_RBDO > WA_EDIT-VLR_JUROS_CALC.
**          MESSAGE 'Juros recebido  ( ' && WA_EDIT-VLR_JUROS_RBDO && ' ) não pode ser maior que juros calculado (' && WA_EDIT-VLR_JUROS_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*          MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Juros recebido | WA_EDIT-VLR_JUROS_RBDO |, não pode ser maior que juros calculado | WA_EDIT-VLR_JUROS_CALC.
*          CLEAR: WA_EDIT-VLR_JUROS_RBDO.
*          EXIT.
*        ENDIF.

      ELSE.
        wa_edit-mont_moeda = ( wa_edit-mont_rbdo -   wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo  ).

        wa_edit-vlr_juros_calc = vlr_multa.
        wa_edit-vlr_multa_calc = vlr_multa.
      ENDIF.

      CLEAR v_mont_aux.
      v_mont_aux = wa_edit-mont_rbdo.

    ELSEIF p_ins IS NOT INITIAL.

*      IF DT_VENC_UTIL EQ ABAP_FALSE AND DIA_UTIL EQ ABAP_FALSE.

      IF wa_edit-data_pgto > v_data_aux OR v_mont_aux > wa_edit-mont_rbdo AND wa_edit-data_pgto > v_data_aux.
        IF  calc = abap_false.

          d_atraso = ( wa_edit-data_pgto - wa_edit-data_venc ).
          vlr_multa = wa_edit-vlr_multa_rbdo.

          DATA(dias_ano) = 360.     "Quantidade de dias no ano.
          CLEAR: fat_prop.
          fat_prop = ( wa_zsdt0040-juros_ano / 360 ) * d_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0040-JUROS_ANO / 360 ) ) / 100 ).

          IF wa_edit-rec_vlr_total EQ abap_true. "Se o campo recebimento total for marcado, considerar o juros com base no total da OV.
            CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
            IF wa_edit-total_recebido IS NOT INITIAL. "Se o valor da fatura estiver preenchido, fazer o calculo com base neste valor.
              vlr_jros = ( wa_edit-total_recebido * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_juros = vlr_jros.
            ELSE.
              vlr_jros = ( wa_lanc-mont_moeda_parc * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_juros = vlr_jros.
*          VLR_MULTA =  ' '.
            ENDIF.

          ELSE.
            CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
            vlr_jros = ( wa_lanc-mont_moeda_fix * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.

            DATA(tot_ov) = ( vlr_jros + wa_lanc-mont_moeda_fix ). "Total da OV + juros.
            prop_juros = ( vlr_jros / tot_ov )  * 100. " Porcentagem proporcional ao valor do juros
            prop_multa = ( vlr_multa / tot_ov )  * 100. " Porcentagem proporcional da multa.
            CLEAR tot_ov.

            vlr_juros = ( prop_juros * wa_edit-mont_rbdo ) / 100.
            vlr_multa =  ( prop_multa * wa_edit-mont_rbdo ) / 100.


          ENDIF.

          wa_edit-vlr_juros_rbdo = vlr_juros.
          wa_edit-vlr_multa_rbdo = vlr_multa.
          wa_edit-vlr_juros_rbdo = vlr_juros.

          wa_edit-vlr_multa_calc = vlr_multa.
          wa_edit-vlr_juros_calc = vlr_juros.
          wa_edit-mont_moeda = ( wa_edit-mont_rbdo - wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo ).
          calc = abap_true.
          dt_pgto_anter   = wa_edit-data_pgto.
          mont_rbdo_anter = wa_edit-mont_rbdo.

*          IF WA_EDIT-VLR_MULTA_RBDO > WA_EDIT-VLR_MULTA_CALC.
**            MESSAGE 'Multa recebido  (' && WA_EDIT-VLR_MULTA_RBDO && ' ) não pode ser maior que multa calculado (' && WA_EDIT-VLR_MULTA_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Multa recebido | WA_EDIT-VLR_MULTA_RBDO |, não pode ser maior que multa calculado | WA_EDIT-VLR_MULTA_CALC.
*            CLEAR: WA_EDIT-VLR_MULTA_RBDO.
*            EXIT.
*          ENDIF.

*          IF WA_EDIT-VLR_JUROS_RBDO > WA_EDIT-VLR_JUROS_CALC.
**            MESSAGE 'Juros recebido  ( ' && WA_EDIT-VLR_JUROS_RBDO && ' ) não pode ser maior que juros calculado (' && WA_EDIT-VLR_JUROS_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Juros recebido | WA_EDIT-VLR_JUROS_RBDO |, não pode ser maior que juros calculado | WA_EDIT-VLR_JUROS_CALC.
*            CLEAR: WA_EDIT-VLR_JUROS_RBDO.
*            EXIT.
*          ENDIF.

        ELSE.

          d_atraso = ( wa_edit-data_pgto - wa_edit-data_venc ).
          vlr_multa = wa_edit-vlr_multa_rbdo.

*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0040-JUROS_ANO / 360 ) ) / 100 ).

          CLEAR: fat_prop.
          fat_prop = ( wa_zsdt0040-juros_ano / 360 ) * d_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

          IF wa_edit-rec_vlr_total EQ abap_true. "Se o campo recebimento total for marcado, considerar o juros com base no total da OV.
            IF wa_edit-total_recebido IS NOT INITIAL. "Se o valor da fatura estiver preenchido, fazer o calculo com base neste valor.
*              VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0040-JUROS_ANO / 360 ) ) / 100 ).
              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_edit-total_recebido * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_juros = vlr_jros.
            ELSE.
*              VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0040-JUROS_ANO / 360 ) ) / 100 ).
              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_lanc-mont_moeda_parc * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.

              vlr_juros = vlr_jros.
*          VLR_MULTA =  ''.

*            WA_EDIT-VLR_JUROS_RBDO  = VLR_JUROS.
*            WA_EDIT-VLR_MULTA_RBDO  = VLR_MULTA.
            ENDIF.
          ELSE.
*            VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0040-JUROS_ANO / 360 ) ) / 100 ).
            CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
            vlr_jros = ( wa_lanc-mont_moeda_fix * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.

            total_ov = ( vlr_jros + wa_lanc-mont_moeda_fix ). "Total da OV + juros.
            prop_juros = ( vlr_jros / total_ov )  * 100. " Porcentagem proporcional ao valor do juros
            prop_multa = ( vlr_multa / total_ov )  * 100. " Porcentagem proporcional da multa.

            vlr_juros = ( prop_juros * wa_edit-mont_rbdo ) / 100.
            vlr_multa =  ( prop_multa * wa_edit-mont_rbdo ) / 100.

*            WA_EDIT-VLR_JUROS_RBDO  = VLR_JUROS.
*            WA_EDIT-VLR_MULTA_RBDO  = VLR_MULTA.
          ENDIF.

          "Se for ajuste não calcular juros nem multa.
          IF wa_edit-ajuste IS NOT INITIAL.
            vlr_juros = ' '.
            vlr_multa = ' '.
          ENDIF.

          "Alterando valor do Montante Recebido
          IF mont_rbdo_anter <> wa_edit-mont_rbdo OR sy-ucomm EQ 'CHECK_AJUST'.
            vlr_multa = 0. "Atribuido o valor, pois nao tem campo na tabela ZSDT0040 p/ calculo
            mont_rbdo_anter         = wa_edit-mont_rbdo.
            wa_edit-vlr_juros_calc  = vlr_juros.
            wa_edit-vlr_multa_calc  = vlr_multa.
            wa_edit-vlr_juros_rbdo  = vlr_juros.
            wa_edit-vlr_multa_rbdo  = vlr_multa.
          ENDIF.

          "Alterando valor do juros
          IF wa_edit-vlr_juros_rbdo EQ  wa_edit-vlr_juros_calc.
            wa_edit-vlr_juros_rbdo = vlr_juros.
          ENDIF.

          "Alterando data de pagamento
          IF dt_pgto_anter <> wa_edit-data_pgto.
            dt_pgto_anter          = wa_edit-data_pgto.
            vlr_multa = 0.  "Atribuido o valor, pois nao tem campo na tabela ZSDT0040 p/ calculo
            wa_edit-vlr_juros_rbdo = vlr_juros.
            wa_edit-vlr_juros_calc = vlr_juros.
            wa_edit-vlr_multa_rbdo = vlr_multa.
            wa_edit-vlr_multa_calc = vlr_multa.
          ENDIF.

*          IF WA_EDIT-VLR_MULTA_RBDO > WA_EDIT-VLR_MULTA_CALC.
**            MESSAGE 'Multa recebido  (' && WA_EDIT-VLR_MULTA_RBDO && ' ) não pode ser maior que multa calculado (' && WA_EDIT-VLR_MULTA_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Multa recebido | WA_EDIT-VLR_MULTA_RBDO |, não pode ser maior que multa calculado | WA_EDIT-VLR_MULTA_CALC.
*            CLEAR: WA_EDIT-VLR_MULTA_RBDO.
*            EXIT.
*          ENDIF.

*          IF WA_EDIT-VLR_JUROS_RBDO > WA_EDIT-VLR_JUROS_CALC.
**            MESSAGE 'Juros recebido  ( ' && WA_EDIT-VLR_JUROS_RBDO && ' ) não pode ser maior que juros calculado (' && WA_EDIT-VLR_JUROS_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Juros recebido | WA_EDIT-VLR_JUROS_RBDO |, não pode ser maior que juros calculado | WA_EDIT-VLR_JUROS_CALC.
*            CLEAR: WA_EDIT-VLR_JUROS_RBDO.
*            EXIT.
*          ENDIF.

          wa_edit-mont_moeda = ( wa_edit-mont_rbdo - wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo ).
        ENDIF.

      ELSE.
        wa_edit-mont_moeda = ( wa_edit-mont_rbdo - wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo ).

        wa_edit-vlr_juros_calc = vlr_multa.
        wa_edit-vlr_multa_calc = vlr_multa.

      ENDIF.

*      IF WA_EDIT-DATA_PGTO EQ WA_EDIT-DATA_VENC.
*        WA_EDIT-VLR_MULTA_RBDO = ' '.
*        WA_EDIT-VLR_JUROS_RBDO = ' '.
*      ENDIF.

      IF wa_edit-data_pgto EQ ' '.
        wa_edit-mont_rbdo      = ' '.
        wa_edit-mont_moeda     = ' '.
        wa_edit-vlr_multa_rbdo = ' '.
        wa_edit-vlr_juros_rbdo = ' '.
      ENDIF.

      CLEAR v_mont_aux.
      v_mont_aux = wa_edit-mont_rbdo.
*      " BG - 115337 - SE FOR RECEBIMENTO DE JUROS TEM QUE TER SALDO DE JUROS CALCULADO - inicio
*  IF WA_EDIT-VLR_JUROS_RBDO IS NOT INITIAL AND  WA_EDIT-VLR_JUROS_RBDO EQ WA_EDIT-MONT_RBDO.
*    IF JUROS_CALCULADO IS INITIAL.
*     MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH 'Não existe amortização para esta OV. Não há juros calculado.'.
*
*    ELSEIF WA_LANC-VLR_JUROS_RBDO > JUROS_CALCULADO.
*
*      MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH 'Valor do juros em aberto é menor que o valor do lançamento!'.
*
*    ENDIF.
*  ENDIF.
*" BG - 115337 - SE FOR RECEBIMENTO DE JUROS TEM QUE TER SALDO DE JUROS CALCULADO - inicio
    ENDIF.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INAT_CHECK_VLR_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inat_check_vlr_total .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'WA_EDIT-REC_VLR_TOTAL'.
        IF wa_edit-ajuste IS NOT INITIAL.
          screen-input = 0. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.

      WHEN 'WA_EDIT-AJUSTE'.
        IF wa_edit-rec_vlr_total IS NOT INITIAL.
          screen-input = 0. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.

*      WHEN 'WA_EDIT-NFENUM'.           "AOENNING
*        IF VBLOCK IS NOT INITIAL.
*          SCREEN-INPUT = 0. "Campo Fechado
*          MODIFY SCREEN.
*        ENDIF.

      WHEN  'SELEC'.
        IF vblock IS NOT INITIAL.
          screen-input = 0. "Campo Fechado
          screen-active    = 1.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'WA_EDIT-VALOR'.
        IF wa_edit-nfenum IS INITIAL.
*        SCREEN-INPUT = 0. "Campo Fechado
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'TXT_FATURA'.
        IF wa_edit-nfenum IS INITIAL.
*        SCREEN-INPUT = 0. "Campo Fechado
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'TXT_MONT_FATURA'.
        IF wa_edit-nfenum IS INITIAL.
*        SCREEN-INPUT = 0. "Campo Fechado
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

      WHEN 'WA_EDIT-TOTAL_RECEBIDO'.
        IF wa_edit-nfenum IS INITIAL.
*        SCREEN-INPUT = 0. "Campo Fechado
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

      WHEN OTHERS.

    ENDCASE.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EDITOR_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM editor_text .
  CLEAR wa_editor.
  CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
  CLEAR: wa_edit-observacao,wa_editor.
  LOOP AT it_editor INTO wa_editor.
    wa_edit-observacao = |{ wa_edit-observacao } { wa_editor-line }|.
  ENDLOOP.
*        READ TABLE IT_EDITOR INTO WA_EDITOR INDEX 1.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ATUA_CAMPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atua_campo .

  DATA: vlr_ov TYPE netwr.

  SELECT *
    FROM zfit0026
    INTO TABLE @DATA(t_lanc)
  WHERE vbeln EQ  @wa_edit-vbeln.

  IF t_lanc IS NOT INITIAL.
    LOOP AT t_lanc ASSIGNING FIELD-SYMBOL(<w_lanc>).
      ADD <w_lanc>-mont_moeda TO vlr_ov.
    ENDLOOP.

    wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - vlr_ov.
    CLEAR: vlr_ov.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZA_VALORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_atualiza_valores .

  DATA: t_day_attributes TYPE TABLE OF  casdayattr,
        w_day_attributes TYPE casdayattr,
        dt_ini           TYPE sy-datum,
        dt_fim           TYPE sy-datum,
        v_data_aux       TYPE datum,
        v_mont_aux       TYPE zfit0026-vlr_multa_rbdo.

  DATA: vlr_jros TYPE p DECIMALS 2.
  DATA: vlr_mult TYPE p DECIMALS 2.
  DATA: fat_prop TYPE p DECIMALS 9.
  DATA: prop_juros TYPE p DECIMALS 9.
  DATA: prop_multa TYPE p DECIMALS 9.


  IF wa_edit-mont_rbdo EQ ' '.
    CLEAR: wa_edit-vlr_juros_rbdo, wa_edit-vlr_multa_rbdo, wa_edit-mont_moeda.
  ENDIF.

  IF wa_edit-mont_rbdo NE ' ' AND wa_edit-data_pgto EQ ' '.
    MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Preencher a data de pagamento.'.

    wa_edit-mont_rbdo      = ' '.
    wa_edit-mont_moeda     = ' '.
    wa_edit-vlr_multa_rbdo = ' '.
    wa_edit-vlr_juros_rbdo = ' '.
  ENDIF.

  IF ( wa_edit-data_pgto EQ ' ' ).
    wa_edit-mont_rbdo      = ' '.
    wa_edit-mont_moeda     = ' '.
    wa_edit-vlr_multa_rbdo = ' '.
    wa_edit-vlr_juros_rbdo = ' '.
  ENDIF.

  PERFORM desc_zterm.

  IF ( wa_edit-data_pgto <> '00000000' AND wa_edit-data_venc <> '00000000' AND wa_edit-mont_rbdo <> ' ' ).

    "Mercado Interno
    SELECT SINGLE * FROM zsdt0053 INTO @DATA(wa_zsdt0053)
     WHERE vbeln EQ @wa_edit-vbeln.

    IF sy-subrc = 0.
      SELECT SINGLE *  FROM zsdt0051 INTO @DATA(wa_zsdt0051)
       WHERE nro_sol_ov EQ @wa_zsdt0053-nro_sol_ov.
    ENDIF.

    "Insumos
    SELECT  SINGLE * FROM zsdt0090 INTO wa_zsdt0090
    WHERE vbeln EQ wa_edit-vbeln.

    IF sy-subrc = 0.
      SELECT SINGLE * FROM zsdt0040 INTO wa_zsdt0040
        WHERE doc_simulacao EQ wa_zsdt0090-doc_simulacao.
    ELSE.

      SELECT SINGLE * FROM zsdt0041  INTO wa_zsdt0041
      WHERE vbeln EQ wa_edit-vbeln.

      IF sy-subrc = 0.
        SELECT SINGLE * FROM zsdt0040 INTO wa_zsdt0040
        WHERE doc_simulacao EQ wa_zsdt0041-doc_simulacao.
      ENDIF.
    ENDIF.

    vlr_multa = 0.
    vlr_juros = 0.

    " Essa função Retorna o próximo dia util...
    " Caso a data entrada seja dia útil, a função retornar ela mesma.

    zcl_miro=>get_proximo_dia_util(
      EXPORTING
        i_data_base        = wa_edit-data_venc
        i_signum           = '+'
        i_ck_data_zles0145 = abap_true
      RECEIVING
        r_data             = v_data_aux
      EXCEPTIONS
        erro               = 1
        OTHERS             = 2 ).

*    IF V_DATA_AUX <> WA_EDIT-DATA_VENC.
*      DATA(DT_VENC_UTIL) = ABAP_TRUE.
*    ENDIF.
*
*    IF V_DATA_AUX EQ WA_EDIT-DATA_PGTO.
*      DATA(DIA_UTIL) = ABAP_TRUE.
*    ENDIF.


    IF p_mi IS NOT INITIAL.

*      IF DT_VENC_UTIL EQ ABAP_FALSE AND DIA_UTIL EQ ABAP_FALSE.

      IF wa_edit-data_pgto > v_data_aux OR v_mont_aux > wa_edit-mont_rbdo AND wa_edit-data_pgto > v_data_aux.
        IF  calc = abap_false.

          DATA(d_atraso) = ( wa_edit-data_pgto - wa_edit-data_venc ).

*          VLR_MULTA = ( WA_EDIT-MONT_RBDO * ( WA_ZSDT0051-TX_MULTA  / 100 )  ).
*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).

***=====================================================================================================
          CLEAR: fat_prop.
          fat_prop = ( wa_zsdt0051-tx_juros / 360 ) * d_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

          IF wa_edit-rec_vlr_total EQ abap_true. "Se o campo recebimento total for marcado, considerar o juros com base no total da OV.
            IF wa_edit-total_recebido IS NOT INITIAL. "Se o valor da fatura estiver preenchido, fazer o calculo valor da referencia.
*              VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).

              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_edit-total_recebido * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_mult = ( wa_edit-total_recebido * wa_zsdt0051-tx_multa ) / 100.

              vlr_juros = vlr_jros.
              vlr_multa =  vlr_mult.
            ELSE.
*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_lanc-mont_moeda_parc * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_mult = ( wa_lanc-mont_moeda_parc * wa_zsdt0051-tx_multa ) / 100.

              vlr_juros = vlr_jros.
              vlr_multa =  vlr_mult.
            ENDIF.

          ELSE.
            "Se o campo recebimento total não for marcado, considerar o juros com base no recebimento parcial.

*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
            CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
            vlr_jros = ( wa_lanc-mont_moeda_fix * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
            vlr_mult = ( wa_lanc-mont_moeda_fix * wa_zsdt0051-tx_multa ) / 100.


            DATA(total_ov) = ( vlr_jros + wa_lanc-mont_moeda_fix +  vlr_mult ). "Total da OV + juros.
*          DATA(TOTAL_OV_MULT) = ( VLR_MULT + WA_ZSDT0053-VLRTOT ). "Total da OV + juros.

            prop_juros = ( vlr_jros / total_ov )  * 100. " Porcentagem proporcional ao valor do juros
            prop_multa = ( vlr_mult / total_ov )  * 100. " Porcentagem proporcional da multa.

            CLEAR: total_ov.

            vlr_juros = ( prop_juros * wa_edit-mont_rbdo ) / 100.
            vlr_multa =  ( prop_multa * wa_edit-mont_rbdo ) / 100.
          ENDIF.

***=======================================================================================================

          wa_edit-vlr_multa_rbdo = vlr_multa.
          wa_edit-vlr_juros_rbdo = vlr_juros.

          wa_edit-vlr_multa_calc = vlr_multa.
          wa_edit-vlr_juros_calc = vlr_juros.

*          IF WA_EDIT-VLR_MULTA_RBDO > WA_EDIT-VLR_MULTA_CALC.
**            MESSAGE 'Multa recebido  (' && WA_EDIT-VLR_MULTA_RBDO && ' ) não pode ser maior que multa calculado (' && WA_EDIT-VLR_MULTA_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Multa recebido | WA_EDIT-VLR_MULTA_RBDO |, não pode ser maior que multa calculado | WA_EDIT-VLR_MULTA_CALC.
*            CLEAR: WA_EDIT-VLR_MULTA_RBDO.
*            EXIT.
*          ENDIF.

*          IF WA_EDIT-VLR_JUROS_RBDO > WA_EDIT-VLR_JUROS_CALC.
**            MESSAGE 'Juros recebido  ( ' && WA_EDIT-VLR_JUROS_RBDO && ' ) não pode ser maior que juros calculado (' && WA_EDIT-VLR_JUROS_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Juros recebido | WA_EDIT-VLR_JUROS_RBDO |, não pode ser maior que juros calculado | WA_EDIT-VLR_JUROS_CALC.
*            CLEAR: WA_EDIT-VLR_JUROS_RBDO.
*            EXIT.
*          ENDIF.

          wa_edit-mont_moeda = ( wa_edit-mont_rbdo -   wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo  ).
          calc = abap_true.
        ELSE.

          d_atraso  = ( wa_edit-data_pgto - wa_edit-data_venc ).
*          VLR_MULTA = ( WA_EDIT-MONT_RBDO * ( WA_ZSDT0051-TX_MULTA  / 100 )  ).
*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).

***=====================================================================================================
          CLEAR: fat_prop.
          fat_prop = ( wa_zsdt0051-tx_juros / 360 ) * d_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

          IF wa_edit-rec_vlr_total EQ abap_true.
            IF wa_edit-total_recebido IS NOT INITIAL. "Se o valor da fatura estiver preenchido, fazer o calculo com base neste valor.
*              VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_edit-total_recebido * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_mult = ( wa_edit-total_recebido * wa_zsdt0051-tx_multa ) / 100.

              vlr_juros = vlr_jros.
              vlr_multa =  vlr_mult.
            ELSE.

*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_lanc-mont_moeda_parc * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_mult = ( wa_lanc-mont_moeda_parc * wa_zsdt0051-tx_multa ) / 100.

              vlr_juros = vlr_jros.
              vlr_multa =  vlr_mult.

*            WA_EDIT-VLR_JUROS_RBDO  = VLR_JUROS.
*            WA_EDIT-VLR_MULTA_RBDO  = VLR_MULTA.
            ENDIF.

          ELSE.
*         VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
            CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
            vlr_jros = ( wa_lanc-mont_moeda_fix * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
            vlr_mult = ( wa_lanc-mont_moeda_fix * wa_zsdt0051-tx_multa ) / 100.

            total_ov = ( vlr_jros + wa_lanc-mont_moeda_fix + vlr_mult ). "Total da OV + juros.
**          TOTAL_OV_MULT = ( VLR_MULT + WA_ZSDT0053-VLRTOT ). "Total da OV + multa.
            prop_juros = ( vlr_jros / total_ov )  * 100. " Porcentagem proporcional ao valor do juros
            prop_multa = ( vlr_mult / total_ov )  * 100. " Porcentagem proporcional da multa.
            CLEAR: total_ov.

            vlr_juros = ( prop_juros * wa_edit-mont_rbdo ) / 100.
            vlr_multa =  ( prop_multa * wa_edit-mont_rbdo ) / 100.

*            WA_EDIT-VLR_JUROS_RBDO  = VLR_JUROS.
*            WA_EDIT-VLR_MULTA_RBDO  = VLR_MULTA.

          ENDIF.

***=======================================================================================================


          "Alterando valor do Montante Recebido ou executar a opção recebimento total.
          IF mont_rbdo_anter <> wa_edit-mont_rbdo OR sy-ucomm EQ 'CHECK_AJUST'.
            mont_rbdo_anter         = wa_edit-mont_rbdo.
            wa_edit-vlr_juros_calc  = vlr_juros.
            wa_edit-vlr_juros_rbdo  = vlr_juros.
            wa_edit-vlr_multa_calc  = vlr_multa.
            wa_edit-vlr_multa_rbdo  = vlr_multa.
          ENDIF.

          "Alterando valor do juros
          IF wa_edit-vlr_juros_rbdo EQ  wa_edit-vlr_juros_calc.
            wa_edit-vlr_juros_rbdo = vlr_juros.
          ENDIF.

          "Alterando data de pagamento
          IF dt_pgto_anter <> wa_edit-data_pgto.
            dt_pgto_anter      = wa_edit-data_pgto.

            wa_edit-vlr_juros_rbdo = vlr_juros.
            wa_edit-vlr_juros_calc = vlr_juros.

            wa_edit-vlr_multa_rbdo = vlr_multa.
            wa_edit-vlr_multa_calc = vlr_multa.
          ENDIF.
          wa_edit-mont_moeda = ( wa_edit-mont_rbdo -   wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo  ).
        ENDIF.

*        IF WA_EDIT-VLR_MULTA_RBDO > WA_EDIT-VLR_MULTA_CALC.
**          MESSAGE 'Multa recebido  (' && WA_EDIT-VLR_MULTA_RBDO && ' ) não pode ser maior que multa calculado (' && WA_EDIT-VLR_MULTA_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*           MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Multa recebido | WA_EDIT-VLR_MULTA_RBDO |, não pode ser maior que multa calculado | WA_EDIT-VLR_MULTA_CALC.
*          CLEAR: WA_EDIT-VLR_MULTA_RBDO.
*          EXIT.
*        ENDIF.

*        IF WA_EDIT-VLR_JUROS_RBDO > WA_EDIT-VLR_JUROS_CALC.
**          MESSAGE 'Juros recebido  ( ' && WA_EDIT-VLR_JUROS_RBDO && ' ) não pode ser maior que juros calculado (' && WA_EDIT-VLR_JUROS_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*          MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Juros recebido | WA_EDIT-VLR_JUROS_RBDO |, não pode ser maior que juros calculado | WA_EDIT-VLR_JUROS_CALC.
*          CLEAR: WA_EDIT-VLR_JUROS_RBDO.
*          EXIT.
*        ENDIF.

      ELSE.
        wa_edit-mont_moeda = ( wa_edit-mont_rbdo -   wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo  ).

        wa_edit-vlr_juros_calc = vlr_juros.
        wa_edit-vlr_multa_calc = vlr_multa.
      ENDIF.

      CLEAR v_mont_aux.
      v_mont_aux = wa_edit-mont_rbdo.

    ELSEIF p_ins IS NOT INITIAL.

*      IF DT_VENC_UTIL EQ ABAP_FALSE AND DIA_UTIL EQ ABAP_FALSE.

      IF wa_edit-data_pgto > v_data_aux OR v_mont_aux > wa_edit-mont_rbdo AND wa_edit-data_pgto > v_data_aux.
        IF  calc = abap_false.

          d_atraso = ( wa_edit-data_pgto - wa_edit-data_venc ).
          vlr_multa = wa_edit-vlr_multa_rbdo.

          DATA(dias_ano) = 360.     "Quantidade de dias no ano.
          CLEAR: fat_prop.
          fat_prop = ( wa_zsdt0040-juros_ano / 360 ) * d_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0040-JUROS_ANO / 360 ) ) / 100 ).

          IF wa_edit-rec_vlr_total EQ abap_true. "Se o campo recebimento total for marcado, considerar o juros com base no total da OV.
            CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
            IF wa_edit-total_recebido IS NOT INITIAL. "Se o valor da fatura estiver preenchido, fazer o calculo com base neste valor.
              vlr_jros = ( wa_edit-total_recebido * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_juros = vlr_jros.
            ELSE.
              vlr_jros = ( wa_lanc-mont_moeda_parc * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_juros = vlr_jros.
*          VLR_MULTA =  ' '.
            ENDIF.

          ELSE.
            CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
            vlr_jros = ( wa_lanc-mont_moeda_fix * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.

            DATA(tot_ov) = ( vlr_jros + wa_lanc-mont_moeda_fix ). "Total da OV + juros.
            prop_juros = ( vlr_jros / tot_ov )  * 100. " Porcentagem proporcional ao valor do juros
            prop_multa = ( vlr_multa / tot_ov )  * 100. " Porcentagem proporcional da multa.
            CLEAR tot_ov.

            vlr_juros = ( prop_juros * wa_edit-mont_rbdo ) / 100.
            vlr_multa =  ( prop_multa * wa_edit-mont_rbdo ) / 100.


          ENDIF.

          wa_edit-vlr_juros_rbdo = vlr_juros.
          wa_edit-vlr_multa_rbdo = vlr_multa.
          wa_edit-vlr_juros_rbdo = vlr_juros.

          wa_edit-vlr_multa_calc = vlr_multa.
          wa_edit-vlr_juros_calc = vlr_juros.
          wa_edit-mont_moeda = ( wa_edit-mont_rbdo - wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo ).
          calc = abap_true.
          dt_pgto_anter   = wa_edit-data_pgto.
          mont_rbdo_anter = wa_edit-mont_rbdo.

*          IF WA_EDIT-VLR_MULTA_RBDO > WA_EDIT-VLR_MULTA_CALC.
**            MESSAGE 'Multa recebido  (' && WA_EDIT-VLR_MULTA_RBDO && ' ) não pode ser maior que multa calculado (' && WA_EDIT-VLR_MULTA_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Multa recebido | WA_EDIT-VLR_MULTA_RBDO |, não pode ser maior que multa calculado | WA_EDIT-VLR_MULTA_CALC.
*            CLEAR: WA_EDIT-VLR_MULTA_RBDO.
*            EXIT.
*          ENDIF.

*          IF WA_EDIT-VLR_JUROS_RBDO > WA_EDIT-VLR_JUROS_CALC.
**            MESSAGE 'Juros recebido  ( ' && WA_EDIT-VLR_JUROS_RBDO && ' ) não pode ser maior que juros calculado (' && WA_EDIT-VLR_JUROS_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Juros recebido | WA_EDIT-VLR_JUROS_RBDO |, não pode ser maior que juros calculado | WA_EDIT-VLR_JUROS_CALC.
*            CLEAR: WA_EDIT-VLR_JUROS_RBDO.
*            EXIT.
*          ENDIF.

        ELSE.

          d_atraso = ( wa_edit-data_pgto - wa_edit-data_venc ).
          vlr_multa = wa_edit-vlr_multa_rbdo.

*          VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0040-JUROS_ANO / 360 ) ) / 100 ).

          CLEAR: fat_prop.
          fat_prop = ( wa_zsdt0040-juros_ano / 360 ) * d_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

          IF wa_edit-rec_vlr_total EQ abap_true. "Se o campo recebimento total for marcado, considerar o juros com base no total da OV.
            IF wa_edit-total_recebido IS NOT INITIAL. "Se o valor da fatura estiver preenchido, fazer o calculo com base neste valor.
*              VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0040-JUROS_ANO / 360 ) ) / 100 ).
              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_edit-total_recebido * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.
              vlr_juros = vlr_jros.
            ELSE.
*              VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0040-JUROS_ANO / 360 ) ) / 100 ).
              CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
              vlr_jros = ( wa_lanc-mont_moeda_parc * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.

              vlr_juros = vlr_jros.
*          VLR_MULTA =  ''.

*            WA_EDIT-VLR_JUROS_RBDO  = VLR_JUROS.
*            WA_EDIT-VLR_MULTA_RBDO  = VLR_MULTA.
            ENDIF.
          ELSE.
*            VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0040-JUROS_ANO / 360 ) ) / 100 ).
            CLEAR: vlr_juros, vlr_jros, prop_multa, vlr_mult.
            vlr_jros = ( wa_lanc-mont_moeda_fix * fat_prop ) / 100.  "Valor do juros com base no valor total da OV.

            total_ov = ( vlr_jros + wa_lanc-mont_moeda_fix ). "Total da OV + juros.
            prop_juros = ( vlr_jros / total_ov )  * 100. " Porcentagem proporcional ao valor do juros
            prop_multa = ( vlr_multa / total_ov )  * 100. " Porcentagem proporcional da multa.

            vlr_juros = ( prop_juros * wa_edit-mont_rbdo ) / 100.
            vlr_multa =  ( prop_multa * wa_edit-mont_rbdo ) / 100.

*            WA_EDIT-VLR_JUROS_RBDO  = VLR_JUROS.
*            WA_EDIT-VLR_MULTA_RBDO  = VLR_MULTA.
          ENDIF.

          "Alterando valor do Montante Recebido
          IF mont_rbdo_anter <> wa_edit-mont_rbdo OR sy-ucomm EQ 'CHECK_AJUST'.
            vlr_multa = 0. "Atribuido o valor, pois nao tem campo na tabela ZSDT0040 p/ calculo
            mont_rbdo_anter         = wa_edit-mont_rbdo.
            wa_edit-vlr_juros_calc  = vlr_juros.
            wa_edit-vlr_juros_rbdo  = vlr_juros.
            wa_edit-vlr_multa_calc  = vlr_multa.
            wa_edit-vlr_multa_rbdo  = vlr_multa.
          ENDIF.

          "Alterando valor do juros
          IF wa_edit-vlr_juros_rbdo EQ  wa_edit-vlr_juros_calc.
            wa_edit-vlr_juros_rbdo = vlr_juros.
          ENDIF.

          "Alterando data de pagamento
          IF dt_pgto_anter <> wa_edit-data_pgto.
            dt_pgto_anter          = wa_edit-data_pgto.
            vlr_multa = 0.  "Atribuido o valor, pois nao tem campo na tabela ZSDT0040 p/ calculo

            wa_edit-vlr_juros_rbdo = vlr_juros.
            wa_edit-vlr_juros_calc = vlr_juros.

            wa_edit-vlr_multa_rbdo = vlr_multa.
            wa_edit-vlr_multa_calc = vlr_multa.
          ENDIF.

*          IF WA_EDIT-VLR_MULTA_RBDO > WA_EDIT-VLR_MULTA_CALC.
**            MESSAGE 'Multa recebido  (' && WA_EDIT-VLR_MULTA_RBDO && ' ) não pode ser maior que multa calculado (' && WA_EDIT-VLR_MULTA_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Multa recebido | WA_EDIT-VLR_MULTA_RBDO |, não pode ser maior que multa calculado | WA_EDIT-VLR_MULTA_CALC.
*            CLEAR: WA_EDIT-VLR_MULTA_RBDO.
*            EXIT.
*          ENDIF.

*          IF WA_EDIT-VLR_JUROS_RBDO > WA_EDIT-VLR_JUROS_CALC.
**            MESSAGE 'Juros recebido  ( ' && WA_EDIT-VLR_JUROS_RBDO && ' ) não pode ser maior que juros calculado (' && WA_EDIT-VLR_JUROS_CALC && ')' TYPE 'I' DISPLAY LIKE 'E'.
*            MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH |Juros recebido | WA_EDIT-VLR_JUROS_RBDO |, não pode ser maior que juros calculado | WA_EDIT-VLR_JUROS_CALC.
*            CLEAR: WA_EDIT-VLR_JUROS_RBDO.
*            EXIT.
*          ENDIF.

          wa_edit-mont_moeda = ( wa_edit-mont_rbdo - wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo ).
        ENDIF.

      ELSE.
        wa_edit-mont_moeda = ( wa_edit-mont_rbdo - wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo ).

        wa_edit-vlr_juros_calc = vlr_juros.
        wa_edit-vlr_multa_calc = vlr_multa.
      ENDIF.

      IF wa_edit-data_pgto EQ wa_edit-data_venc.
        wa_edit-vlr_multa_rbdo = ' '.
        wa_edit-vlr_juros_rbdo = ' '.
      ENDIF.



      IF wa_edit-data_pgto EQ ' '.
        wa_edit-mont_rbdo      = ' '.
        wa_edit-mont_moeda     = ' '.
        wa_edit-vlr_multa_rbdo = ' '.
        wa_edit-vlr_juros_rbdo = ' '.
      ENDIF.

      CLEAR v_mont_aux.
      v_mont_aux = wa_edit-mont_rbdo.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_BLOQ_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_ORDEM  text
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM check_bloq_ordem USING v_ordem CHANGING sy-subrc.

  DATA: tl_enq     TYPE TABLE OF seqg3 WITH HEADER LINE,
        wl_num_enq TYPE sy-tabix,
        wl_arg     TYPE seqg3-garg,
        coruf      TYPE coruf,
        tco01      TYPE tco01,
        aufnr      TYPE seqg3-garg.

  CLEAR sy-subrc.
*  WL_ARG  = |{ SY-MANDT }{ V_ORDEM }|.
*  V_ORDEM = |{ V_ORDEM ALPHA = IN }|.
*  TCO01-AUTYP = '30'.
*  CORUF-AUFNR = V_ORDEM.
*
*  CALL FUNCTION 'CO_RU_ORDER_LOCK'
*    EXPORTING
*      AUFNR_IMP            = CORUF-AUFNR
**     AUTYP_IMP            = TCO01-AUTYP
*    EXCEPTIONS
*      ORDER_ALREADY_LOCKED = 1.
*
*  CASE SY-SUBRC.
*    WHEN 1.
*      CLEAR SY-SUBRC.
*      SY-SUBRC = 1.
*
*
*      MESSAGE E469(CO) WITH CORUF-AUFNR SY-MSGV2
*                       RAISING ORDER_ALREADY_LOCKED.
*  ENDCASE.


  CALL FUNCTION 'SD_SALES_DOCUMENT_ENQUEUE'
    EXPORTING
      vbeln              = v_ordem "US_VBELN
      i_check_scenario_a = abap_true "CHARX
*     I_VBAK             = VBAK
*     I_XVBAK_UPDKZ      = XVBAK_UPDKZ
    EXCEPTIONS
      foreign_lock       = 2
      system_failure     = 3
      no_change          = 4.

  IF sy-subrc = 0.
    EXIT.
  ENDIF.

  DATA(da_uname) = sy-msgv1.


  CASE sy-subrc.
    WHEN 2.
*      DA_VBELN = VBAK-VBELN.
*      IF US_INIT NE SPACE.
*        PERFORM BELEG_INITIALISIEREN.
*      ENDIF.
*      VBAK-VBELN = DA_VBELN.
*      PERFORM BELEG_ENTSPERREN_BTCH USING VBAK-VBELN.
      MESSAGE e042 DISPLAY LIKE 'I' WITH v_ordem da_uname.
*      MESSAGE 'Ordem' V_ORDEM

      sy-subrc = 1.
    WHEN 3.
*      MESSAGE A043.
    WHEN 4.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  LIMPA_JUROS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE limpa_juros INPUT.
  CHECK p_ins IS NOT INITIAL.
  CLEAR: wa_edit-vlr_multa_rbdo,  wa_edit-vlr_juros_rbdo.
ENDMODULE.

FORM lancamento_massa. "FI 115811 - CS2023000317 ZFIS26 - Recebimento Vendas Mercado Interno - PSA

  IF wa_lanc-vbeln IS INITIAL.
    READ TABLE it_saida[] INTO wa_saida INDEX 1.
  ENDIF.

  DATA: soma     TYPE zfit0026-mont_moeda,
        texto_01 TYPE c LENGTH 100 VALUE 'Valor do Montante menor do',
        texto_02 TYPE c LENGTH 100 VALUE 'que a soma total dos lançamentos'.

  CLEAR: it_lanc_ver[], wa_lanc_ver.

  SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag
         status uname data_registro bukrs obj_key docnum zterm doc_fatura
         data_pgto vlr_multa_rbdo vlr_juros_rbdo mont_rbdo vlr_multa_calc
         vlr_juros_calc observacao ajuste rec_vlr_total vlr_desc_mult vlr_desc_jros num_comp_adiant
    FROM zfit0026
    INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
  WHERE vbeln EQ wa_lanc-vbeln.

  LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln = wa_lanc-vbeln.
    soma = soma + wa_lanc_ver-mont_moeda.
  ENDLOOP.

**********************************************************************

  DATA kurrf TYPE vbkd-kurrf.

  MOVE-CORRESPONDING wa_lanc TO wa_edit.

  IF wa_edit-moeda EQ 'USD'.
    CLEAR: kurrf.
    SELECT SINGLE kurrf
      FROM zsdt0090
      INTO  kurrf
      WHERE vbelv     EQ wa_edit-vbeln
        AND categoria EQ 'C'
        AND estorno   NE 'X'
      AND kurrf IS NOT NULL.


    IF sy-subrc IS NOT INITIAL.

      SELECT SINGLE kurrf
        FROM vbkd
        INTO  kurrf
        WHERE vbeln EQ wa_edit-vbeln.

    ENDIF.

    wa_edit-taxa = kurrf.

  ENDIF.

  wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda.

**********************************************************************

  IF wa_saida-auart IN r_devo_recu.
    wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - abs( soma ).
    MULTIPLY wa_lanc-mont_moeda_parc BY -1.
  ELSE.
    wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - soma.
  ENDIF.


  DATA:  it_lanc_mass TYPE STANDARD TABLE OF zfie0026 INITIAL SIZE 0.
  CLEAR: it_lanc_mass.
  MOVE-CORRESPONDING it_fatu TO it_lanc_mass.

  IF  wa_lanc-bukrs_vf IS INITIAL.
    wa_lanc-bukrs_vf = wa_saida-bukrs_vf.
  ENDIF.

  LOOP AT it_lanc_mass ASSIGNING FIELD-SYMBOL(<_it_lanc_mass>).
    CLEAR:<_it_lanc_mass>-data_venc.
    <_it_lanc_mass>-data_venc = <_it_lanc_mass>-valdt.
    READ TABLE it_lanc_ver INTO DATA(_get_lanc) WITH KEY doc_fatura = <_it_lanc_mass>-refkey.
    IF _get_lanc IS NOT INITIAL.
      <_it_lanc_mass>-data_venc = <_it_lanc_mass>-valdt.
      IF _get_lanc-taxa > 1.
        <_it_lanc_mass>-taxa = _get_lanc-taxa.
      ELSE.
        <_it_lanc_mass>-taxa = '0.00'.
      ENDIF.
      <_it_lanc_mass>-vlr_multa_calc = _get_lanc-vlr_multa_calc.
      <_it_lanc_mass>-vlr_juros_calc = _get_lanc-vlr_juros_calc.
    ENDIF.
    <_it_lanc_mass>-vbeln = wa_saida-vbeln.
    <_it_lanc_mass>-seq = wa_lanc-seq.
    <_it_lanc_mass>-bukrs = wa_lanc-bukrs_vf.
    <_it_lanc_mass>-mont_moeda_parc = wa_lanc-mont_moeda_parc.
    <_it_lanc_mass>-mont_moeda_fix = wa_lanc-mont_moeda_fix.
    <_it_lanc_mass>-valid = icon_led_yellow.
    <_it_lanc_mass>-auart = wa_saida-auart.
    <_it_lanc_mass>-netwr = wa_saida-netwr.
    <_it_lanc_mass>-moeda = wa_lanc-moeda.
    <_it_lanc_mass>-taxa = wa_lanc-taxa.
*    <_it_lanc_mass>-saldo_finan = wa_lanc-saldo_finan.
  ENDLOOP.

  EXPORT it_lanc_mass = it_lanc_mass TO MEMORY ID 'ZFIR0022_LM'.
  SUBMIT zfir0022_lm AND RETURN .

  PERFORM refresh_data_lancamento.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibir_erro
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IS_ROW_NO_ROW_ID
*&---------------------------------------------------------------------*
FORM f_exibir_erro  USING p_row_id. "// INICIO WBARBOSA 11-12-2024 US-115811

  DATA: lv_msg TYPE string.

  CLEAR: wa_lanc.
  READ TABLE it_lanc[] INTO wa_lanc INDEX p_row_id.
  CHECK sy-subrc IS INITIAL.

  READ TABLE it_lanc_ver INTO DATA(ls_lanc_ver) WITH KEY vbeln = wa_lanc-vbeln seq = wa_lanc-seq.
  CHECK sy-subrc IS INITIAL.

  READ TABLE t_zib_contabil_err INTO DATA(ls_zib_contabil_err) WITH KEY obj_key = ls_lanc_ver-obj_key.
  CHECK sy-subrc IS INITIAL.

  LOOP AT t_zib_contabil_err INTO ls_zib_contabil_err WHERE obj_key = ls_lanc_ver-obj_key.
    IF lv_msg IS INITIAL.
      lv_msg = |Msg{ sy-tabix }: { ls_zib_contabil_err-message }|.
    ELSE.
      lv_msg = |{ lv_msg }, Msg{ sy-tabix }:{ ls_zib_contabil_err-message }|.
    ENDIF.
  ENDLOOP.

  CHECK lv_msg IS NOT INITIAL.

  MESSAGE lv_msg TYPE 'I'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_seleciona_zfis
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_seleciona_zfis .

  SELECT vbeln seq data_venc moeda mont_moeda  taxa mont_mi forma_pag status uname
       data_registro bukrs obj_key docnum  zterm doc_fatura data_pgto
       vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo  vlr_multa_calc vlr_juros_calc
       razao_especial  observacao ajuste rec_vlr_total num_comp_adiant
      FROM zfit0026
      INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
    WHERE vbeln EQ  wa_lanc-vbeln.

ENDFORM.
**<<<------"163316 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*&      Module  ZM_DOC_CONT  INPUT
*&---------------------------------------------------------------------*
*       Valida a existencia do Documento Contábil
*----------------------------------------------------------------------*
MODULE zm_doc_cont INPUT.

* Valida a existencia do Documento Contábil.
  PERFORM zf_doc_cont_check.

ENDMODULE.
*&---------------------------------------------------------------------*
*& FORM ZF_DOC_CONT_CHECK
*&---------------------------------------------------------------------*
*& Valida a existencia do Documento Contábil
*&---------------------------------------------------------------------*
FORM zf_doc_cont_check.

  TABLES bkpf.

* Verifica se Lançamento de Ajuste está marcado.
  CHECK NOT   wa_edit-ajuste IS INITIAL    AND
            ( sy-ucomm       EQ 'GRAVAR'   OR
              sy-ucomm       EQ 'EXECUTE' ).

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_edit-doc_cont
    IMPORTING
      output = wa_edit-doc_cont.

  SELECT SINGLE * FROM bkpf WHERE bukrs EQ wa_edit-bukrs
                              AND belnr EQ wa_edit-doc_cont.

  IF NOT sy-subrc         IS INITIAL AND
     NOT wa_edit-doc_cont IS INITIAL AND
         sy-ucomm         EQ 'GRAVAR'.
    MESSAGE e888(sabapdocu) DISPLAY LIKE 'E' WITH 'Doc Contábil não existe.'.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_OBLIGATORY_CHECK  INPUT
*&---------------------------------------------------------------------*
*       Verifica o Documento Contábil se está preenchido
*----------------------------------------------------------------------*
*---#171608 24-03-2025 SMC--Inicio--> retirado validação pq nao é necessário ser obrigatorio preenchimento qdo ajuste = X. Manter apenas validação da existencia do doc contabil caso preenchido no campo.
*MODULE zm_obligatory_check INPUT.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = wa_edit-doc_cont
*    IMPORTING
*      output = wa_edit-doc_cont.
*
*  IF NOT wa_edit-ajuste   IS INITIAL AND
*         wa_edit-doc_cont IS INITIAL AND
*         sy-ucomm         EQ 'GRAVAR'.
*    MESSAGE e055(00).
*
*  ENDIF.
*
*ENDMODULE.
*---#171608 24-03-2025 SMC--Fim--> retirado validação pq nao é necessário ser obrigatorio preenchimento qdo ajuste = X. Manter apenas validação da existencia do doc contabil caso preenchido no campo.
**<<<------"163316 - NMS - FIM------>>>

*** US #181883 - MMSILVA - 10.06.2025 - Ini ***
*&---------------------------------------------------------------------*
*& Module STATUS_6001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_6001 OUTPUT.
  SET PF-STATUS 'PF6001'.
  SET TITLEBAR 'TB6001'.

  PERFORM monta_pop_6001.
  PERFORM mostra_pop_6001.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_6001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_6001 INPUT.
  CASE sy-ucomm.
    WHEN 'CANCELAR'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_POP_6001
*&---------------------------------------------------------------------*
FORM monta_pop_6001.

  DATA: wa_legenda TYPE ty_legenda.

  CLEAR: it_legenda.

  IF tabstrip-activetab = 'TAB_P'.
    CLEAR wa_legenda.
    wa_legenda-icone = '@5C@'.
    wa_legenda-descr = 'Erro gerado pela ZIB'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5C@'.
    wa_legenda-descr = 'Aguardando Retorno Doc Fornecedor SIGAM'.
    APPEND wa_legenda TO it_legenda.

  ELSEIF tabstrip-activetab = 'TAB_L'.
    CLEAR wa_legenda.
    wa_legenda-icone = '@39@'.
    wa_legenda-descr = 'Aguardando Retorno da ZIB'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5C@'.
    wa_legenda-descr = 'Erro gerado pela ZIB'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5B@'.
    wa_legenda-descr = 'Processo Concluído'.
    APPEND wa_legenda TO it_legenda.

*    CLEAR wa_legenda.
*    wa_legenda-icone = '@B2@'.
*    wa_legenda-descr = 'Não definido'.
*    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@H6@'.
    wa_legenda-descr = 'Lançamento de ajuste'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@1V@'.
    wa_legenda-descr = 'Aguardando Retorno Doc Fornecedor SIGAM'.
    APPEND wa_legenda TO it_legenda.

    CLEAR wa_legenda.
    wa_legenda-icone = '@5D@'.
    wa_legenda-descr = 'Processo Iniciado'.
    APPEND wa_legenda TO it_legenda.

  ELSEIF tabstrip-activetab = 'TAB_S'.


  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP_6001
*&---------------------------------------------------------------------*
FORM mostra_pop_6001.

  IF g_custom_container_pop_6001 IS INITIAL.

    CREATE OBJECT g_custom_container_pop_6001
      EXPORTING
        container_name              = 'CONTAINER6001'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    FREE: it_fieldcatalog.
    PERFORM fieldcatalog USING:
          'ICONE'           'Icone'          '10'   ' ' '' ' '  'X' ' ',
          'DESCR'           'Descrição'      '10'   ' ' '' ' '  'X' ' '.

    gs_layout_pop_6001-cwidth_opt = 'X'.

    CREATE OBJECT ctl_alv1_pop_6001
      EXPORTING
        i_parent = g_custom_container_pop_6001.           "alv lote

    CALL METHOD ctl_alv1_pop_6001->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_pop_6001
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_legenda.

  ELSE.
    CALL METHOD ctl_alv1_pop_6001->refresh_table_display
      EXPORTING
        is_stable = _stable.
  ENDIF.

ENDFORM.
*** US #181883 - MMSILVA - 10.06.2025 - Fim ***
