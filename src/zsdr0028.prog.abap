*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio Luiz R. da Silva                                &*
*& Data.....: 29/04/2013                                              &*
*& Descrição: Solicitações de Venda	                                  &*
*& Transação: ZSDT0067                                                &*
*---------------------------------------------------------------------&*


REPORT  zsdr0028.

TABLES: zsdt0051,zsdt0053.
*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
"tables: zimp_lanc_impost.

TYPES: z_vkorg  TYPE RANGE OF zsdt0051-vkorg,
       z_vkbur  TYPE RANGE OF zsdt0051-vkbur,
       z_vtweg  TYPE RANGE OF zsdt0051-vtweg,
       z_spart  TYPE RANGE OF zsdt0051-spart,
       z_tp_ve  TYPE RANGE OF zsdt0051-tp_venda,
       z_nroov  TYPE RANGE OF zsdt0051-nro_sol_ov,
       z_data   TYPE RANGE OF zsdt0051-data_atual,
       z_kunnr  TYPE RANGE OF zsdt0051-kunnr,
       z_matnr  TYPE RANGE OF zsdt0053-matnr,
       z_charg  TYPE RANGE OF zsdt0053-charg,
       z_status TYPE RANGE OF zsdt0051-status.

TYPES:
  BEGIN OF ty_zsdt0051,
    nro_sol_ov       TYPE zsdt0051-nro_sol_ov,
    tp_venda         TYPE zsdt0051-tp_venda,
    auart            TYPE zsdt0051-auart,
    vkorg            TYPE zsdt0051-vkorg,
    vtweg            TYPE zsdt0051-vtweg,
    spart            TYPE zsdt0051-spart,
    vkbur            TYPE zsdt0051-vkbur,
    vkgrp            TYPE zsdt0051-vkgrp,
    kunnr            TYPE zsdt0051-kunnr,
    bstkd            TYPE zsdt0051-bstkd,
    inco1            TYPE zsdt0051-inco1,
    inco2            TYPE zsdt0051-inco2,
    vkaus            TYPE zsdt0051-vkaus,
    waerk            TYPE zsdt0051-waerk,
    dtde_logist      TYPE zsdt0051-dtde_logist,
    dtate_logist     TYPE zsdt0051-dtate_logist,
    observacao       TYPE zsdt0051-observacao,
    coment_logistica TYPE zsdt0051-coment_logistica,
    correto          TYPE zsdt0051-correto,
    status           TYPE zsdt0051-status,
    data_venda       TYPE zsdt0051-data_venda,
    data_atual       TYPE zsdt0051-data_atual,
    num_fixacao      TYPE zsdt0051-num_fixacao,
    taxa_curva       TYPE zsdt0051-taxa_curva,
  END OF ty_zsdt0051,

  BEGIN OF ty_zsdt0052,
    nro_sol_ov TYPE zsdt0052-nro_sol_ov,
    zlsch      TYPE zsdt0052-zlsch,
    pgto_ant   TYPE zsdt0052-pgto_ant,
    zterm      TYPE zsdt0052-zterm,
    qte_venc   TYPE zsdt0052-qte_venc,
    hbkid      TYPE zsdt0052-hbkid,
    valdt      TYPE zsdt0052-valdt,
  END OF ty_zsdt0052,

  BEGIN OF ty_zsdt0069,
    nro_sol_ov   TYPE zsdt0069-nro_sol_ov,
    id_historico TYPE zsdt0069-id_historico,
    data_atual   TYPE zsdt0069-data_atual,
  END OF ty_zsdt0069,

  BEGIN OF ty_zsdt0073,
    nro_sol_ov TYPE zsdt0073-nro_sol_ov,
    fixacao    TYPE zsdt0073-fixacao,
    zterm      TYPE zsdt0073-zterm,
    qte_venc   TYPE zsdt0073-qte_venc,
  END OF ty_zsdt0073,

  BEGIN OF ty_zsdt0059,
    nro_sol_ov TYPE zsdt0059-nro_sol_ov,
    posnr      TYPE zsdt0059-posnr,
    cod_fp     TYPE zsdt0059-cod_fp,
    field      TYPE zsdt0059-field,
    bezei      TYPE zsdt0059-bezei,
    formula2   TYPE zsdt0059-formula2,
    cbot       TYPE zsdt0059-cbot,
    ocbot      TYPE zsdt0059-ocbot,
    valdt      TYPE zsdt0059-valdt,
    monat      TYPE zsdt0059-monat,
    c_decimais TYPE zsdt0059-c_decimais,
  END OF ty_zsdt0059,

  BEGIN OF ty_zsdt0053,
    nro_sol_ov    TYPE zsdt0053-nro_sol_ov,
    status        TYPE zsdt0053-status,
    posnr         TYPE zsdt0053-posnr,
    fixacao       TYPE zsdt0053-fixacao,
    matnr         TYPE zsdt0053-matnr,
    werks         TYPE zsdt0053-werks,
    lgort         TYPE zsdt0053-lgort,
    charg         TYPE zsdt0053-charg,
    zmeng         TYPE zsdt0053-zmeng,
    zieme         TYPE zsdt0053-zieme,
    dmbtr         TYPE zsdt0053-dmbtr,
    pmein         TYPE zsdt0053-pmein,
    vlrtot        TYPE zsdt0053-vlrtot,
    valdt         TYPE zsdt0053-valdt,
    vbeln         TYPE zsdt0053-vbeln,
    kunnr         TYPE zsdt0053-kunnr,
    ponto_c       TYPE zsdt0053-ponto_c,
    terminal      TYPE zsdt0053-terminal,
    brgew         TYPE zsdt0053-brgew,
    volum         TYPE zsdt0053-volum,
    voleh         TYPE zsdt0053-voleh,
    kursf         TYPE zsdt0053-kursf,
    contrato      TYPE zsdt0053-contrato,
    classificacao TYPE zsdt0053-classificacao,
    navio         TYPE zsdt0053-navio,
    porto         TYPE zsdt0053-porto,
    p_porto       TYPE zsdt0053-p_porto,
    vlt_porto     TYPE zsdt0053-vlt_porto,
    instrucao     TYPE zsdt0053-instrucao,
  END OF ty_zsdt0053,

  BEGIN OF ty_zsdt0054,
    nro_sol_ov TYPE zsdt0054-nro_sol_ov,
    posnr      TYPE zsdt0054-posnr,
    valdt      TYPE zsdt0054-valdt,
    dmbtr      TYPE zsdt0054-dmbtr,
    adiant     TYPE zsdt0054-adiant,
    kursf      TYPE zsdt0054-kursf,
    vlr_real   TYPE zsdt0054-vlr_real,

  END OF ty_zsdt0054,

  BEGIN OF ty_zsdt0056,
    cod_fp TYPE zsdt0056-cod_fp,
    bezei  TYPE zsdt0056-bezei,
  END OF ty_zsdt0056,

  BEGIN OF ty_zsdt0070,
    cod_fp     TYPE zsdt0070-cod_fp,
    field      TYPE zsdt0070-field,
    c_decimais TYPE zsdt0070-c_decimais,
  END OF ty_zsdt0070,

  BEGIN OF ty_zsdt0057,
    tp_venda    TYPE zsdt0057-tp_venda,
    bezei       TYPE zsdt0057-bezei,
    param_espec TYPE zsdt0057-param_espec,
  END OF ty_zsdt0057,

  BEGIN OF ty_zsdt0084,
    nro_sol_ov      TYPE zsdt0084-nro_sol_ov,
    vlr_vencido_brl TYPE zsdt0084-vlr_vencido_brl,
    vlr_avencer_brl TYPE zsdt0084-vlr_avencer_brl,
    total_brl       TYPE zsdt0084-total_brl,
    vlr_adiant_brl  TYPE zsdt0084-vlr_adiant_brl,
    total_mov_brl   TYPE zsdt0084-total_mov_brl,
    limite_credito  TYPE zsdt0084-limite_credito,
    saldo_disp_brl  TYPE zsdt0084-saldo_disp_brl,
    util_limit_brl  TYPE zsdt0084-util_limit_brl,
    sdo_ov_emit     TYPE zsdt0084-sdo_ov_emit,
  END OF ty_zsdt0084,


  BEGIN OF ty_kna1,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
    ort01 TYPE kna1-ort01,
    regio TYPE kna1-regio,
  END OF ty_kna1,

  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
    ort01 TYPE lfa1-ort01,
    regio TYPE lfa1-regio,
  END OF ty_lfa1,

  BEGIN OF ty_makt,
    matnr TYPE makt-matnr,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  BEGIN OF ty_vbap,
    vbeln  TYPE vbap-vbeln,
    kwmeng TYPE vbap-kwmeng,
  END OF ty_vbap,

  BEGIN OF ty_vbfa,
    vbelv    TYPE vbfa-vbelv,
    vbeln    TYPE vbfa-vbeln,
    vbtyp_n  TYPE vbfa-vbtyp_n,
    vbtyp_v  TYPE vbfa-vbtyp_v,
    rfmng    TYPE vbfa-rfmng,
    rfwrt    TYPE vbfa-rfwrt,
    vbeln_fk TYPE bkpf-awkey,
  END OF ty_vbfa,

  BEGIN OF ty_zsdt0063,
    nro_sol_ov TYPE zsdt0063-nro_sol_ov,
    posnr      TYPE zsdt0063-posnr,
    valdt      TYPE zsdt0063-valdt,
    bukrs      TYPE zsdt0063-bukrs,
    dmbtr      TYPE zsdt0063-dmbtr,
    waers      TYPE zsdt0063-waers,
    lifnr      TYPE zsdt0063-lifnr,
    banks      TYPE zsdt0063-banks,
    bankl      TYPE zsdt0063-bankl,
    swift      TYPE zsdt0063-swift,
    bankn      TYPE zsdt0063-bankn,
    adiant     TYPE zsdt0063-adiant,
  END OF ty_zsdt0063,

  BEGIN OF ty_saida,
    data_atual       TYPE zsdt0051-data_atual,
    data_venda       TYPE zsdt0051-data_venda,
    dif(5),
    nro_sol_ov       TYPE zsdt0051-nro_sol_ov,
    fixacao          TYPE zsdt0053-fixacao,
    tp_venda         TYPE zsdt0051-tp_venda,
    de_venda         TYPE zsdt0057-bezei,
    auart            TYPE zsdt0051-auart,
    vkorg            TYPE zsdt0051-vkorg,
    vtweg            TYPE zsdt0051-vtweg,
    spart            TYPE zsdt0051-spart,
    vkbur            TYPE zsdt0051-vkbur,
    vkgrp            TYPE zsdt0051-vkgrp,
    kunnr            TYPE zsdt0051-kunnr,
    name1_c          TYPE kna1-name1,
    bstkd            TYPE zsdt0051-bstkd,
    inco1            TYPE zsdt0051-inco1,
    inco2            TYPE zsdt0051-inco2,
    vkaus            TYPE zsdt0051-vkaus,
    waerk            TYPE zsdt0051-waerk,
    dtde_logist      TYPE zsdt0051-dtde_logist,
    dtate_logist     TYPE zsdt0051-dtate_logist,
    num_fixacao      TYPE zsdt0051-num_fixacao,
    observacao       TYPE zsdt0051-observacao,
    coment_logistica TYPE zsdt0051-coment_logistica,
    zlsch            TYPE zsdt0052-zlsch,
    pgto_ant(18), "          TYPE ZSDT0052-PGTO_ANT,
    zterm            TYPE zsdt0052-zterm,
    qte_venc         TYPE zsdt0052-qte_venc,
    hbkid            TYPE zsdt0052-hbkid,
    valdt            TYPE zsdt0052-valdt,
    status           TYPE zsdt0053-status,
    posnr            TYPE zsdt0053-posnr,
    matnr            TYPE zsdt0053-matnr,
    maktx            TYPE makt-maktx,
    werks            TYPE zsdt0053-werks,
    lgort            TYPE zsdt0053-lgort,
    charg            TYPE zsdt0053-charg,
    zmeng            TYPE zsdt0053-zmeng,
    zieme            TYPE zsdt0053-zieme,
    dmbtr            TYPE zsdt0053-dmbtr,
    pmein            TYPE zsdt0053-pmein,
    vlrtot           TYPE zsdt0053-vlrtot,
    vbeln            TYPE zsdt0053-vbeln,
    valdt_ant        TYPE zsdt0054-valdt,
    dmbtr_ant        TYPE zsdt0054-dmbtr,
    adiant           TYPE zsdt0054-adiant,
    posnr63          TYPE zsdt0063-posnr,
    valdt63          TYPE zsdt0063-valdt,
    bukrs63          TYPE zsdt0063-bukrs,
    dmbtr63          TYPE zsdt0063-dmbtr,
    waers63          TYPE zsdt0063-waers,
    lifnr63          TYPE zsdt0063-lifnr,
    name163          TYPE lfa1-name1,
    banks63          TYPE zsdt0063-banks,
    bankl63          TYPE zsdt0063-bankl,
    swift63          TYPE zsdt0063-swift,
    bankn63          TYPE zsdt0063-bankn,
    adiant63         TYPE zsdt0063-adiant,
    correto          TYPE zsdt0051-correto,
    name1_f          TYPE lfa1-name1,
    sd_ordem         TYPE vbfa-rfmng,
    qt_dev           TYPE vbfa-rfmng,
    qt_compl         TYPE vbfa-rfmng,
    qt_ord           TYPE vbap-kwmeng,
    status51         TYPE zsdt0053-status,
    ponto_c          TYPE zsdt0053-ponto_c,
    terminal         TYPE zsdt0053-terminal,
    brgew            TYPE zsdt0053-brgew,
    volum            TYPE zsdt0053-volum,
    voleh            TYPE zsdt0053-voleh,
    kursf            TYPE zsdt0053-kursf,
    kursf54          TYPE zsdt0054-kursf,
    vlr_real         TYPE zsdt0054-vlr_real,
    posnr66          TYPE zsdt0066-posnr,
    matnr66          TYPE zsdt0066-matnr,
    werks66          TYPE zsdt0066-werks,
    lgort66          TYPE zsdt0066-lgort,
    charg66          TYPE zsdt0066-charg,
    zmeng66          TYPE zsdt0066-zmeng,
    volum66          TYPE zsdt0066-volum,
    dmbtr66          TYPE zsdt0066-dmbtr,
    vlrtot66         TYPE zsdt0066-vlrtot,
    kunnr66          TYPE zsdt0066-kunnr,
    instrucao66      TYPE zsdt0066-instrucao,
    terminal66       TYPE zsdt0066-terminal,
    lentrega66       TYPE zsdt0066-lentrega,
    inco166          TYPE zsdt0066-inco1,
    inco266          TYPE zsdt0066-inco2,
    vbeln66          TYPE zsdt0066-vbeln,
    libra_to         TYPE zsdt0066-libra_to,
    usd_to           TYPE zsdt0066-usd_to,
    vlr_tot_frm_usd  TYPE zsdt0066-vlr_tot_frm_usd,
    vlr_vencido_brl  TYPE zsdt0084-vlr_vencido_brl,
    vlr_avencer_brl  TYPE zsdt0084-vlr_avencer_brl,
    total_brl        TYPE zsdt0084-total_brl,
    vlr_adiant_brl   TYPE zsdt0084-vlr_adiant_brl,
    total_mov_brl    TYPE zsdt0084-total_mov_brl,
    limite_credito   TYPE zsdt0084-limite_credito,
    saldo_disp_brl   TYPE zsdt0084-saldo_disp_brl,
    util_limit_brl   TYPE zsdt0084-util_limit_brl,
    sdo_ov_emit      TYPE zsdt0084-sdo_ov_emit,
    data_atual69     TYPE zsdt0069-data_atual,
    valdt52          TYPE zsdt0052-valdt,
    taxa_curva       TYPE zsdt0051-taxa_curva,
    color_cell       TYPE lvc_t_scol,  " Cell color
    contrato         TYPE zsdt0053-contrato,
    classificacao    TYPE zsdt0053-classificacao,
    navio            TYPE zsdt0053-navio,
    porto            TYPE zsdt0053-porto,
    p_porto          TYPE zsdt0053-p_porto,
    vlt_porto        TYPE zsdt0053-vlt_porto,
    instrucao        TYPE zsdt0053-instrucao,
    ort01            TYPE kna1-ort01,
    regio            TYPE kna1-regio,
    qtd_fatur        TYPE zsdt0084-sdo_ov_emit, " Rubenilson - 24.09.24 - #153088
  END OF ty_saida.


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_tables,
         name TYPE c LENGTH 11,
         qtd  TYPE n LENGTH 3,
         ov   TYPE zsdt0051-nro_sol_ov,
       END OF ty_tables,

       BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         awkey TYPE bkpf-awkey,
       END OF ty_bkpf,

       BEGIN OF ty_bseg,
         bukrs TYPE bseg-bukrs,
         belnr TYPE bseg-belnr,
         gjahr TYPE bseg-gjahr,
         wrbtr TYPE bseg-wrbtr,
         koart TYPE bseg-koart,
       END OF ty_bseg.
*----------------------------------------------------------------------*
* Field-symbols
*----------------------------------------------------------------------*
* <fs_data> p/ser a tabela dinâmica onde constaram os dados de exibição
FIELD-SYMBOLS: <fs_data>  TYPE ANY TABLE,

*work-área p/ trabalhar os dados antes de inclui <fs_data>
               <wa_data>  TYPE any,
               <wa_data2> TYPE any,

*campo que recebera dados e apontara p/ os campos dinâmicos da wa.
               <fs_campo> TYPE any.

*----------------------------------------------------------------------*
* Tabelas Interna
*----------------------------------------------------------------------*
*Tabela dinâmica de exibição do ALV
DATA: t_data TYPE REF TO data.

*----------------------------------------------------------------------*
* Estrutura de dados
*----------------------------------------------------------------------*
* Work-Área p/ montar dados dos campos
DATA: wa_fcat_lvc TYPE lvc_s_fcat,

* Tabela sem cabeçalho p/ receber dados da wa acima e passar informações
* de campos p/ gerar a tabela dinâmica
      lt_fcat_lvc TYPE lvc_t_fcat.



*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: t_bdc           TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      t_messtab       TYPE TABLE OF bdcmsgcoll,

      it_zsdt0051     TYPE TABLE OF ty_zsdt0051,
      it_zsdt0051_1   TYPE TABLE OF ty_zsdt0051,
      it_zsdt0052     TYPE TABLE OF ty_zsdt0052,
      it_zsdt0073     TYPE TABLE OF ty_zsdt0073,
      it_zsdt0053     TYPE TABLE OF ty_zsdt0053,
      it_zsdt53_aux   TYPE TABLE OF ty_zsdt0053,
      it_zsdt0069     TYPE TABLE OF ty_zsdt0069,
      it_zsdt0162     TYPE TABLE OF zsdt0162,
      it_zsdt0054     TYPE TABLE OF ty_zsdt0054,
      it_zsdt0056     TYPE TABLE OF ty_zsdt0056,
      it_zsdt0070     TYPE TABLE OF ty_zsdt0070,
      it_zsdt0057     TYPE TABLE OF ty_zsdt0057,
      it_zsdt0059     TYPE TABLE OF ty_zsdt0059,
      it_zsdt0059_aux TYPE TABLE OF ty_zsdt0059,
      it_zsdt0059_dec TYPE TABLE OF ty_zsdt0059,
      it_zsdt0059_au2 TYPE TABLE OF ty_zsdt0059,
      it_zsdt0059_spr TYPE TABLE OF ty_zsdt0059,
      it_zsdt0063     TYPE TABLE OF ty_zsdt0063,
      it_zsdt0066     TYPE TABLE OF zsdt0066,
      it_zsdt0084     TYPE TABLE OF ty_zsdt0084,
      it_kna1         TYPE TABLE OF ty_kna1,
      it_kna1_aux     TYPE TABLE OF ty_kna1,
      it_lfa1         TYPE TABLE OF ty_lfa1,
      it_makt         TYPE TABLE OF ty_makt,
      it_bseg         TYPE TABLE OF ty_bseg,
      it_vbfa         TYPE TABLE OF ty_vbfa,
      it_vbfa_c       TYPE TABLE OF ty_vbfa,
      it_vbfa_c_e     TYPE TABLE OF ty_vbfa,
      it_vbfa_l       TYPE TABLE OF ty_vbfa,
      it_vbfa_l_e     TYPE TABLE OF ty_vbfa,
      it_vbfa_h       TYPE TABLE OF ty_vbfa,
      it_vbfa_h_e     TYPE TABLE OF ty_vbfa,
      it_bkpf         TYPE TABLE OF ty_bkpf,
      it_vbap         TYPE TABLE OF ty_vbap,
      it_saida        TYPE TABLE OF ty_saida,
      it_saida_aux    TYPE TABLE OF ty_saida,
      it_tables       TYPE TABLE OF ty_tables,

      it_color        TYPE TABLE OF lvc_s_scol.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_cont         TYPE REF TO cl_gui_custom_container,
  wa_alv          TYPE REF TO cl_gui_alv_grid,
  wa_layout       TYPE lvc_s_layo,

  wa_zsdt0051     TYPE ty_zsdt0051,
  wa_zsdt0051_1   TYPE ty_zsdt0051,
  wa_zsdt0052     TYPE ty_zsdt0052,
  wa_zsdt0073     TYPE ty_zsdt0073,
  wa_zsdt0053     TYPE ty_zsdt0053,
  wa_zsdt53_aux   TYPE ty_zsdt0053,
  wa_zsdt0069     TYPE ty_zsdt0069,
  wa_zsdt0162     TYPE zsdt0162,
  wa_zsdt0054     TYPE ty_zsdt0054,
  wa_zsdt0056     TYPE ty_zsdt0056,
  wa_zsdt0070     TYPE ty_zsdt0070,
  wa_zsdt0057     TYPE ty_zsdt0057,
  wa_zsdt0059     TYPE ty_zsdt0059,
  wa_zsdt0059_aux TYPE ty_zsdt0059,
  wa_zsdt0059_dec TYPE ty_zsdt0059,
  wa_zsdt0063     TYPE ty_zsdt0063,
  wa_zsdt0066     TYPE zsdt0066,
  wa_zsdt0084     TYPE ty_zsdt0084,
  wa_kna1         TYPE ty_kna1,
  wa_lfa1         TYPE ty_lfa1,
  wa_makt         TYPE ty_makt,
  wa_bseg         TYPE ty_bseg,
  wa_vbfa         TYPE ty_vbfa,
  wa_vbfa_c       TYPE ty_vbfa,
  wa_vbfa_c_e     TYPE ty_vbfa,
  wa_vbfa_l       TYPE ty_vbfa,
  wa_vbfa_l_e     TYPE ty_vbfa,
  wa_vbfa_h       TYPE ty_vbfa,
  wa_vbfa_h_e     TYPE ty_vbfa,
  wa_bkpf         TYPE ty_bkpf,
  wa_vbap         TYPE ty_vbap,
  wa_saida        TYPE ty_saida,
  wa_saida2       TYPE ty_saida,
  wa_saida3       TYPE ty_saida,
  wa_saida_aux    TYPE ty_saida,
  wa_tables       TYPE ty_tables,
  lines1          TYPE sy-tabix,
  lines_max       TYPE sy-tabix,
  wa_color        TYPE lvc_s_scol.

FIELD-SYMBOLS <fs_vbfa>  TYPE ty_vbfa.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  it_fcat       TYPE TABLE OF ty_estrutura,
  it_fcat2      TYPE TABLE OF ty_estrutura,
  s_variant     TYPE disvariant           , " Tabela Estrutura co
  t_top         TYPE slis_t_listheader,
  xs_events     TYPE slis_alv_event,
  events        TYPE slis_t_event,
  gd_layout     TYPE slis_layout_alv,
  t_print       TYPE slis_print_alv,
  v_report      LIKE sy-repid,
  t_sort        TYPE slis_t_sortinfo_alv WITH HEADER LINE,
  it_setleaf    LIKE TABLE OF setleaf INITIAL SIZE 0 WITH HEADER LINE,
  estrutura     TYPE TABLE OF ty_estrutura,
  vg_i          TYPE i,
  v_repid       LIKE sy-repid,
  v_camp(7),      " variável p/ montar campo dinâmico
  v_camp1(10),      " variável p/ montar campo dinâmico
  v_camp2(10),      " variável p/ montar campo dinâmico
  v_camp3(10),      " variável p/ montar campo dinâmico
  v_text(100),    " variável p/ montar texto dinâmico
  v_continua(1),
  v_nro_sol_ov  TYPE zsdt0051-nro_sol_ov,
  vtotal        TYPE zimp_lanc_imp_ct-valor_imp VALUE 0,
  tabix         TYPE sy-tabix.

DATA: repid            LIKE sy-repid.
DATA: s_fieldcat       TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA: s_layout         TYPE slis_layout_alv.
DATA: s_print          TYPE slis_print_alv.
DATA: s_sort           TYPE slis_t_sortinfo_alv WITH HEADER LINE.
DATA: variante         LIKE disvariant.
DATA: def_variante     LIKE disvariant.
DATA: s_selfield       TYPE slis_selfield.
DATA: list_top_of_page TYPE slis_t_listheader.

DATA: gs_variant_c TYPE disvariant.

DATA: ref1            TYPE REF TO cl_gui_alv_grid,
      ls_sel_hide     TYPE slis_sel_hide_alv,
      is_table        TYPE lvc_s_stbl,
      it_fieldcatalog TYPE lvc_t_fcat,
      wa_fieldcatalog TYPE lvc_s_fcat.

FIELD-SYMBOLS: <wa_fieldcatalog> TYPE lvc_s_fcat.


DEFINE mc_preenche_class.
  vg_i = vg_i + 1.
  CLEAR t_sort.
  t_sort-spos      = vg_i.
  t_sort-fieldname = &1.
  t_sort-group     = &2.
  IF &3 = 'D'.
    t_sort-down        = 'X'.
  ELSE.
    t_sort-up          = &3.
  ENDIF.
  t_sort-subtot    = &4.
  APPEND t_sort.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
"DATA   DYFIELDS LIKE DYNPREAD OCCURS 1 WITH HEADER LINE.
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:  p_vkorg   FOR zsdt0051-vkorg OBLIGATORY,
                   p_vkbur   FOR zsdt0051-vkbur ,
                   p_vtweg   FOR zsdt0051-vtweg ,
                   p_spart   FOR zsdt0051-spart ,
                   p_tp_ve   FOR zsdt0051-tp_venda OBLIGATORY,
                   p_nroov   FOR zsdt0051-nro_sol_ov,
                   p_data    FOR zsdt0051-data_atual,
                   p_daven   FOR zsdt0051-data_atual,
                   p_dalib   FOR zsdt0051-data_atual,
                   p_kunnr   FOR zsdt0051-kunnr,
                   p_matnr   FOR zsdt0053-matnr,
                   p_charg   FOR zsdt0053-charg,
                   p_status  FOR zsdt0051-status.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-000.
  PARAMETER: p_varia TYPE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b5.

INITIALIZATION.
  gs_variant_c-report      = sy-repid.
  p_data-low   = sy-datum - 1.
  p_data-high  = sy-datum.
  p_data-sign  = 'I'.
  p_data-option = 'BT'.
  APPEND p_data.

*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
*
  DATA: vg_repid   LIKE sy-repid,
        vg_variant TYPE disvariant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

  vg_repid          = sy-repid.
  variante-report = vg_repid.

  IF ( NOT p_varia IS INITIAL ).
    vg_variant-variant = p_varia.

  ENDIF.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = variante
      i_save        = 'A'
    IMPORTING
      es_variant    = variante
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE variante-variant TO p_varia.
    MOVE variante-variant TO gs_variant_c-variant.
  ENDIF.


*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
            f_iniciar_variaves, " Cabeçalho
            f_seleciona_dados USING p_vkorg[]
                                    p_vkbur[]
                                    p_vtweg[]
                                    p_spart[]
                                    p_tp_ve[]
                                    p_nroov[]
                                    p_data[]
                                    p_daven[]
                                    p_kunnr[]
                                    p_matnr[]
                                    p_charg[]
                                    p_status[], " Form seleciona dados
*            F_SAIDA, " Form de saida
            f_atribuir_dados,
  f_imprime_dados.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves .
  DATA:
                 w_texto1(10),
                 w_texto2(10),
                 w_texto3(40),
                 w_texto(40),
                 textocab     TYPE c LENGTH 99.


  v_report = sy-repid.

  w_texto3 = 'Solicitação de Ordem de Vendas'.
  PERFORM f_construir_cabecalho USING 'H' w_texto3.

  IF p_vkorg IS NOT INITIAL.
    w_texto = 'Org. de Vendas    :'.
    CONCATENATE w_texto p_vkorg-low  INTO textocab SEPARATED BY space.
    IF p_vkorg-high IS NOT INITIAL.
      CONCATENATE 'a' p_vkorg-high INTO textocab SEPARATED BY space.
    ENDIF.
    PERFORM f_construir_cabecalho USING 'S' textocab.
  ENDIF.

  IF  p_tp_ve IS NOT INITIAL.
    w_texto = 'Tipo de Venda   :'.
    CONCATENATE w_texto p_tp_ve-low  INTO textocab SEPARATED BY space.
    IF p_tp_ve-high IS NOT INITIAL.
      CONCATENATE 'a' p_tp_ve-high INTO textocab SEPARATED BY space.
    ENDIF.
    PERFORM f_construir_cabecalho USING 'S' textocab.
  ENDIF.


ENDFORM.                    " F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados USING s_vkorg TYPE z_vkorg
                             s_vkbur TYPE z_vkbur
                             s_vtweg TYPE z_vtweg
                             s_spart TYPE z_spart
                             s_tp_ve TYPE z_tp_ve
                             s_nroov TYPE z_nroov
                             s_data  TYPE z_data
                             s_daven TYPE z_data
                             s_kunnr TYPE z_kunnr
                             s_matnr TYPE z_matnr
                             s_charg TYPE z_charg
                             s_status TYPE z_status.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Preparando dados'.

  IF gs_variant_c-variant IS INITIAL.
    IF p_varia IS NOT INITIAL.
      MOVE p_varia TO gs_variant_c-variant.
    ENDIF.
  ENDIF.

  IF NOT p_dalib IS INITIAL.

    SELECT nro_sol_ov id_historico data_atual
      FROM zsdt0069
      INTO TABLE it_zsdt0069
      WHERE data_atual IN p_dalib.

    SORT it_zsdt0069 BY nro_sol_ov.
    DELETE ADJACENT DUPLICATES FROM it_zsdt0069 COMPARING nro_sol_ov.

    SELECT * FROM zsdt0162
      INTO TABLE it_zsdt0162
      WHERE data_atual IN p_dalib.
*        AND STATUS EQ 'L'
*        AND CK_RECUSA NE 'S'
*        AND ULTIMO_NIVEL EQ 'X'.

    SORT it_zsdt0162 BY vbeln ASCENDING id_log DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_zsdt0162 COMPARING vbeln.

*    CHECK NOT IT_ZSDT0069 IS INITIAL.

    IF NOT it_zsdt0069 IS INITIAL.

      SELECT nro_sol_ov tp_venda status
        FROM zsdt0051
        INTO TABLE it_zsdt0051_1
        FOR ALL ENTRIES IN it_zsdt0069
        WHERE nro_sol_ov EQ it_zsdt0069-nro_sol_ov
        AND   vkorg       IN s_vkorg
        AND   vkbur       IN s_vkbur
        AND   vtweg       IN s_vtweg
        AND   spart       IN s_spart
        AND   tp_venda    IN s_tp_ve
        AND   nro_sol_ov  IN s_nroov
        AND   kunnr       IN s_kunnr
        AND   status      IN s_status
        AND   data_atual  IN s_data
        AND   data_venda  IN s_daven.

    ENDIF.

    IF NOT it_zsdt0162 IS INITIAL.

      SELECT nro_sol_ov tp_venda status
        FROM zsdt0051
        APPENDING TABLE it_zsdt0051_1
        FOR ALL ENTRIES IN it_zsdt0162
        WHERE nro_sol_ov EQ it_zsdt0162-vbeln
        AND   vkorg       IN s_vkorg
        AND   vkbur       IN s_vkbur
        AND   vtweg       IN s_vtweg
        AND   spart       IN s_spart
        AND   tp_venda    IN s_tp_ve
        AND   nro_sol_ov  IN s_nroov
        AND   kunnr       IN s_kunnr
        AND   status      IN s_status
        AND   data_atual  IN s_data
        AND   data_venda  IN s_daven.

    ENDIF.

  ELSE.

    SELECT nro_sol_ov tp_venda
    FROM zsdt0051
    INTO TABLE it_zsdt0051_1
    WHERE vkorg       IN s_vkorg
    AND   vkbur       IN s_vkbur
    AND   vtweg       IN s_vtweg
    AND   spart       IN s_spart
    AND   tp_venda    IN s_tp_ve
    AND   nro_sol_ov  IN s_nroov
    AND   kunnr       IN s_kunnr
    AND   status      IN s_status
    AND   data_atual  IN s_data
    AND   data_venda  IN s_daven.

  ENDIF.


  CHECK NOT it_zsdt0051_1 IS INITIAL.

  SORT: it_zsdt0051_1 BY nro_sol_ov.

  LOOP AT it_zsdt0051_1 INTO wa_zsdt0051_1.

    SELECT nro_sol_ov tp_venda auart vkorg vtweg spart vkbur vkgrp kunnr bstkd inco1
     inco2 vkaus waerk dtde_logist dtate_logist observacao coment_logistica
     correto status data_venda data_atual num_fixacao taxa_curva
    FROM zsdt0051
    INTO TABLE it_zsdt0051
    WHERE nro_sol_ov  EQ  wa_zsdt0051_1-nro_sol_ov.

    CHECK it_zsdt0051[] IS NOT INITIAL.

    SELECT kunnr name1  ort01 regio
      FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_zsdt0051
      WHERE kunnr EQ it_zsdt0051-kunnr.

    SELECT   nro_sol_ov zlsch pgto_ant zterm qte_venc hbkid valdt
      FROM zsdt0052
      INTO TABLE it_zsdt0052
      FOR ALL ENTRIES IN it_zsdt0051
      WHERE nro_sol_ov EQ it_zsdt0051-nro_sol_ov.

    SELECT   nro_sol_ov id_historico  data_atual
      FROM zsdt0069
      INTO TABLE it_zsdt0069
      FOR ALL ENTRIES IN it_zsdt0051
      WHERE nro_sol_ov EQ it_zsdt0051-nro_sol_ov
      AND   status     EQ 'L'.

    SELECT * FROM zsdt0162
      INTO TABLE it_zsdt0162
      FOR ALL ENTRIES IN it_zsdt0051
      WHERE vbeln EQ it_zsdt0051-nro_sol_ov
        AND status EQ 'L'
        AND ck_recusa NE 'S'
        AND ultimo_nivel EQ 'X'.

    SELECT   nro_sol_ov fixacao zterm qte_venc
     FROM zsdt0073
     INTO TABLE it_zsdt0073
     FOR ALL ENTRIES IN it_zsdt0051
     WHERE nro_sol_ov EQ it_zsdt0051-nro_sol_ov.

    SELECT   *
      FROM zsdt0066
      INTO TABLE it_zsdt0066
      FOR ALL ENTRIES IN it_zsdt0051
      WHERE nro_sol_ov EQ it_zsdt0051-nro_sol_ov.

    SELECT nro_sol_ov status posnr fixacao matnr werks lgort charg zmeng zieme dmbtr pmein vlrtot
      valdt vbeln kunnr ponto_c terminal brgew volum voleh kursf contrato classificacao navio porto p_porto vlt_porto instrucao
     FROM zsdt0053
     INTO TABLE it_zsdt0053
     FOR ALL ENTRIES IN it_zsdt0051
     WHERE nro_sol_ov EQ it_zsdt0051-nro_sol_ov.

    MOVE it_zsdt0053 TO it_zsdt53_aux.

    DELETE it_zsdt0053 WHERE status EQ 'Y'.
    DELETE it_zsdt0053 WHERE status EQ 'W'.
    DELETE it_zsdt0053 WHERE status EQ 'C'.

    DELETE it_zsdt53_aux WHERE status EQ 'C'.

    IF it_zsdt0053[] IS NOT INITIAL.
      SELECT  vbelv vbeln vbtyp_n vbtyp_v rfmng rfwrt
        FROM vbfa
        INTO TABLE it_vbfa
        FOR ALL ENTRIES IN it_zsdt0053
        WHERE vbelv     EQ  it_zsdt0053-vbeln
        AND   vbtyp_n   IN  ('J', 'H', 'C', 'L' )
        AND   vbtyp_v   EQ  'C'.

      IF it_vbfa[] IS NOT INITIAL.

        SELECT  vbelv vbeln vbtyp_n vbtyp_v rfmng rfwrt
          FROM vbfa
          INTO TABLE it_vbfa_c
          FOR ALL ENTRIES IN it_vbfa
          WHERE vbelv     EQ    it_vbfa-vbeln
          AND   vbtyp_n   EQ    'M'
          AND   vbtyp_v   EQ    'C'.

        IF it_vbfa_c IS NOT INITIAL.
          LOOP AT it_vbfa_c ASSIGNING <fs_vbfa>.
            <fs_vbfa>-vbeln_fk = <fs_vbfa>-vbeln.
          ENDLOOP.

          SELECT bukrs belnr gjahr awkey
          FROM bkpf
            APPENDING TABLE it_bkpf
            FOR ALL ENTRIES IN it_vbfa_c
            WHERE awkey EQ it_vbfa_c-vbeln_fk.
        ENDIF.

        SELECT  vbelv vbeln vbtyp_n vbtyp_v rfmng rfwrt
          FROM vbfa
          INTO TABLE it_vbfa_l
          FOR ALL ENTRIES IN it_vbfa
          WHERE vbelv     EQ    it_vbfa-vbeln
          AND   vbtyp_n   EQ    'P'
          AND   vbtyp_v   EQ    'L'.

        IF it_vbfa_l  IS NOT INITIAL.
          LOOP AT it_vbfa_l ASSIGNING <fs_vbfa>.
            <fs_vbfa>-vbeln_fk = <fs_vbfa>-vbeln.
          ENDLOOP.

          SELECT bukrs belnr gjahr awkey
          FROM bkpf
            APPENDING TABLE it_bkpf
            FOR ALL ENTRIES IN it_vbfa_l
            WHERE awkey EQ it_vbfa_l-vbeln_fk.
        ENDIF.

        SELECT  vbelv vbeln vbtyp_n vbtyp_v rfmng rfwrt
          FROM vbfa
          INTO TABLE it_vbfa_h
          FOR ALL ENTRIES IN it_vbfa
          WHERE vbelv     EQ    it_vbfa-vbeln
          AND   vbtyp_n   EQ    'O'
          AND   vbtyp_v   EQ    'H'.

        IF it_vbfa_h IS NOT INITIAL.
          LOOP AT it_vbfa_h ASSIGNING <fs_vbfa>.
            <fs_vbfa>-vbeln_fk = <fs_vbfa>-vbeln.
          ENDLOOP.

          SELECT bukrs belnr gjahr awkey
          FROM bkpf
            APPENDING TABLE it_bkpf
            FOR ALL ENTRIES IN it_vbfa_h
            WHERE awkey EQ it_vbfa_h-vbeln_fk.
        ENDIF.

        IF it_bkpf IS NOT INITIAL.
          DATA etl947c10r6545 TYPE TABLE OF bseg.
          DATA lt_fields_l947c10r2926 TYPE fagl_t_field.
          lt_fields_l947c10r2926 = VALUE #( ( line = 'BUKRS' )
           ( line = 'BELNR' )
           ( line = 'GJAHR' )
           ( line = 'WRBTR' )
           ( line = 'KOART' )
           ).

          CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
            EXPORTING
              it_for_all_entries = it_bkpf
              i_where_clause     = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR AND KOART EQ 'D'|
              it_fieldlist       = lt_fields_l947c10r2926
            IMPORTING
              et_bseg            = etl947c10r6545
            EXCEPTIONS
              not_found          = 1.
          IF sy-subrc = 0 AND lines( etl947c10r6545 ) > 0.
            MOVE-CORRESPONDING etl947c10r6545 TO it_bseg.
            sy-dbcnt = lines( etl947c10r6545 ).
          ELSE.
            sy-subrc = 4.
            sy-dbcnt = 0.
          ENDIF.

        ENDIF.
        IF it_vbfa_c[] IS NOT INITIAL.
          SELECT  vbelv vbeln vbtyp_n vbtyp_v rfmng rfwrt
            FROM vbfa
            INTO TABLE it_vbfa_c_e
            FOR ALL ENTRIES IN it_vbfa_c
            WHERE vbelv     EQ    it_vbfa_c-vbeln
            AND   vbtyp_n   EQ    'N'
            AND   vbtyp_v   EQ    'M'.
        ENDIF.
        IF it_vbfa_l[] IS NOT INITIAL.
          SELECT  vbelv vbeln vbtyp_n vbtyp_v rfmng rfwrt
            FROM vbfa
            INTO TABLE it_vbfa_l_e
            FOR ALL ENTRIES IN it_vbfa_l
            WHERE vbelv     EQ    it_vbfa_l-vbeln
            AND   vbtyp_n   EQ    'N'
            AND   vbtyp_v   EQ    'P'.
        ENDIF.
        IF it_vbfa_h[] IS NOT INITIAL.
          SELECT  vbelv vbeln vbtyp_n vbtyp_v rfmng rfwrt
            FROM vbfa
            INTO TABLE it_vbfa_h_e
            FOR ALL ENTRIES IN it_vbfa_h
            WHERE vbelv     EQ    it_vbfa_h-vbeln
            AND   vbtyp_n   EQ    'S'
            AND   vbtyp_v   EQ    'O'.
        ENDIF.

      ENDIF.

      SELECT vbeln kwmeng
        FROM vbap
        INTO TABLE it_vbap
        FOR ALL ENTRIES IN it_zsdt0053
        WHERE vbeln EQ it_zsdt0053-vbeln.

    ENDIF.

    SELECT nro_sol_ov posnr valdt dmbtr adiant kursf vlr_real
     FROM zsdt0054
     INTO TABLE it_zsdt0054
     FOR ALL ENTRIES IN it_zsdt0051
     WHERE nro_sol_ov EQ it_zsdt0051-nro_sol_ov.

    SELECT    nro_sol_ov posnr valdt bukrs dmbtr waers lifnr banks bankl swift bankn adiant
      FROM zsdt0063
      INTO TABLE it_zsdt0063
      FOR ALL ENTRIES IN it_zsdt0051
      WHERE nro_sol_ov EQ it_zsdt0051-nro_sol_ov.


    SELECT nro_sol_ov vlr_vencido_brl vlr_avencer_brl total_brl vlr_adiant_brl
           total_mov_brl limite_credito saldo_disp_brl util_limit_brl sdo_ov_emit
      FROM zsdt0084
      INTO TABLE it_zsdt0084
      FOR ALL ENTRIES IN it_zsdt0051
      WHERE nro_sol_ov EQ it_zsdt0051-nro_sol_ov.

    SELECT kunnr name1  ort01 regio
      FROM kna1
      INTO TABLE it_kna1_aux
      FOR ALL ENTRIES IN it_zsdt0053
      WHERE kunnr EQ it_zsdt0053-kunnr.

    LOOP AT it_kna1_aux INTO wa_kna1.
      APPEND wa_kna1 TO it_kna1.
    ENDLOOP.

    SELECT lifnr name1
      FROM lfa1
      INTO TABLE it_lfa1
      FOR ALL ENTRIES IN it_zsdt0051
      WHERE lifnr EQ it_zsdt0051-correto.

    IF it_zsdt0063[] IS NOT INITIAL.
      SELECT lifnr name1 ort01 regio
        FROM lfa1
        APPENDING TABLE it_lfa1
        FOR ALL ENTRIES IN it_zsdt0063
        WHERE lifnr EQ it_zsdt0063-lifnr.
    ENDIF.

    SELECT matnr maktx
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_zsdt0053
      WHERE matnr EQ it_zsdt0053-matnr
        AND spras EQ sy-langu.

    PERFORM f_saida.
    CLEAR: it_saida, it_tables.
  ENDLOOP.

* 59
  SELECT     nro_sol_ov posnr cod_fp field bezei formula2 cbot ocbot valdt monat c_decimais
    FROM zsdt0059
    INTO TABLE it_zsdt0059
    FOR ALL ENTRIES IN it_zsdt0051_1
    WHERE nro_sol_ov EQ it_zsdt0051_1-nro_sol_ov.
* 56
  SELECT cod_fp bezei
    FROM zsdt0056
    INTO TABLE it_zsdt0056
    FOR ALL ENTRIES IN it_zsdt0059
    WHERE cod_fp EQ it_zsdt0059-cod_fp.
* 70
  SELECT cod_fp field c_decimais
    FROM zsdt0070
    INTO TABLE it_zsdt0070
    FOR ALL ENTRIES IN it_zsdt0059
    WHERE cod_fp EQ it_zsdt0059-cod_fp.
* 57
  SELECT tp_venda bezei param_espec
   FROM zsdt0057
   INTO TABLE it_zsdt0057
   FOR ALL ENTRIES IN it_zsdt0051_1
   WHERE tp_venda EQ it_zsdt0051_1-tp_venda.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida.

  DATA vkunnr TYPE zsdt0053-kunnr.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Gerando relatório'.

  DATA: w_flag53(1),
        w_flag54(1),
        w_flag63(1),
        w_flag66(1),
        it_aux      TYPE TABLE OF zsdt0051,
        vsd_ordem   TYPE vbfa-rfmng,
        vsd_total   TYPE vbfa-rfwrt,
        qt_dev      TYPE vbfa-rfmng,
        qt_com      TYPE vbfa-rfwrt,
        w_flag53_c  TYPE n LENGTH 3,
        w_flag54_c  TYPE n LENGTH 3,
        w_flag63_c  TYPE n LENGTH 3,
        w_flag66_c  TYPE n LENGTH 3.

  DATA:
    cont_53   TYPE n LENGTH 3,
    cont_54   TYPE n LENGTH 3,
    cont_53_1 TYPE n LENGTH 3,
    cont_63   TYPE n LENGTH 3,
    cont_66   TYPE n LENGTH 3,
    cont_s    TYPE n LENGTH 3,
    cont      TYPE n LENGTH 3,
    tab       TYPE TABLE OF zsdt0051.


  SORT: it_zsdt0051 BY nro_sol_ov,
        it_zsdt0051_1 BY nro_sol_ov,
        it_zsdt0052 BY nro_sol_ov,
        it_zsdt0073 BY nro_sol_ov fixacao,
        it_zsdt0053 BY nro_sol_ov posnr,
        it_zsdt0054 BY nro_sol_ov posnr valdt,
        it_zsdt0057 BY tp_venda,
        it_zsdt0063 BY nro_sol_ov posnr valdt,
        it_zsdt0059 BY nro_sol_ov,
        it_zsdt0066 BY nro_sol_ov posnr,
        it_zsdt0069 BY nro_sol_ov ASCENDING id_historico DESCENDING,
        it_zsdt0162 BY vbeln ASCENDING id_log DESCENDING,
        it_zsdt0084 BY nro_sol_ov,
        it_kna1     BY kunnr,
        it_lfa1     BY lifnr,
        it_makt     BY matnr,
        it_vbfa     BY vbelv,
        it_saida    BY nro_sol_ov posnr.

  LOOP AT it_zsdt0051 INTO wa_zsdt0051.

    DATA: tabix51 TYPE sy-tabix.
    tabix51 = sy-tabix.

    wa_saida-nro_sol_ov = wa_zsdt0051-nro_sol_ov.

    wa_saida-tp_venda             = wa_zsdt0051-tp_venda.
    READ TABLE it_zsdt0057 INTO wa_zsdt0057 WITH KEY tp_venda = wa_zsdt0051-tp_venda BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-de_venda             = wa_zsdt0057-bezei.
    ELSE.
      CLEAR wa_saida-de_venda.
    ENDIF.

    wa_saida-auart                = wa_zsdt0051-auart.
    wa_saida-vkorg                = wa_zsdt0051-vkorg.
    wa_saida-vtweg                = wa_zsdt0051-vtweg.
    wa_saida-spart                = wa_zsdt0051-spart.
    wa_saida-vkbur                = wa_zsdt0051-vkbur.
    wa_saida-vkgrp                = wa_zsdt0051-vkgrp.
    wa_saida-status51             = wa_zsdt0051-status.
    wa_saida-data_atual           = wa_zsdt0051-data_atual.
    wa_saida-data_venda           = wa_zsdt0051-data_venda.
    wa_saida-num_fixacao          = wa_zsdt0051-num_fixacao.
    wa_saida-taxa_curva           = wa_zsdt0051-taxa_curva.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0051-correto
      IMPORTING
        output = wa_saida-correto.

    READ TABLE it_zsdt0084 INTO wa_zsdt0084 WITH KEY nro_sol_ov = wa_zsdt0051-nro_sol_ov BINARY SEARCH.
    wa_saida-vlr_vencido_brl = wa_zsdt0084-vlr_vencido_brl.
    wa_saida-vlr_avencer_brl = wa_zsdt0084-vlr_avencer_brl.
    wa_saida-total_brl       = wa_zsdt0084-total_brl.
    wa_saida-vlr_adiant_brl  = wa_zsdt0084-vlr_adiant_brl.
    wa_saida-total_mov_brl   = wa_zsdt0084-total_mov_brl.
    wa_saida-limite_credito  = wa_zsdt0084-limite_credito.
    wa_saida-saldo_disp_brl  = wa_zsdt0084-saldo_disp_brl.
    wa_saida-util_limit_brl  = wa_zsdt0084-util_limit_brl.
    wa_saida-sdo_ov_emit     = wa_zsdt0084-sdo_ov_emit.


    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zsdt0051-correto BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-name1_f              = wa_lfa1-name1.
      wa_saida-ort01                = wa_lfa1-ort01.
      wa_saida-regio                = wa_lfa1-regio.
    ELSE.
      CLEAR wa_saida-name1_f.
    ENDIF.

    wa_saida-bstkd                = wa_zsdt0051-bstkd.
    wa_saida-inco1                = wa_zsdt0051-inco1.
    wa_saida-inco2                = wa_zsdt0051-inco2.
    wa_saida-vkaus                = wa_zsdt0051-vkaus.
    wa_saida-waerk                = wa_zsdt0051-waerk.
    wa_saida-dtde_logist          = wa_zsdt0051-dtde_logist.
    wa_saida-dtate_logist         = wa_zsdt0051-dtate_logist.
    wa_saida-observacao           = wa_zsdt0051-observacao.
    wa_saida-coment_logistica     = wa_zsdt0051-coment_logistica.

    wa_saida-pgto_ant = '  Não Antencipado'.
    READ TABLE it_zsdt0052 INTO wa_zsdt0052 WITH KEY nro_sol_ov = wa_zsdt0051-nro_sol_ov BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-zlsch      = wa_zsdt0052-zlsch.
      IF wa_zsdt0052-pgto_ant = 'X'.
        wa_saida-pgto_ant   = 'X-Com Boleto'.
      ENDIF.
      IF wa_zsdt0052-pgto_ant = 'N'.
        wa_saida-pgto_ant   = 'N-Sem Boleto'.
      ENDIF.

      wa_saida-zterm      = wa_zsdt0052-zterm.
      wa_saida-qte_venc   = wa_zsdt0052-qte_venc.
      wa_saida-hbkid      = wa_zsdt0052-hbkid.
      wa_saida-valdt52    = wa_zsdt0052-valdt.
    ENDIF.

    READ TABLE it_zsdt0069 INTO wa_zsdt0069 WITH KEY nro_sol_ov = wa_zsdt0051-nro_sol_ov BINARY SEARCH.
    IF sy-subrc = 0.
      IF p_dalib IS NOT INITIAL.
        IF wa_zsdt0069-data_atual NOT IN p_dalib.
          CONTINUE.
        ENDIF.
      ENDIF.
      wa_saida-data_atual69  = wa_zsdt0069-data_atual.
*    ELSEIF P_DALIB IS NOT INITIAL.
*      CONTINUE.
    ENDIF.

    IF wa_saida-data_atual69 IS INITIAL.
      READ TABLE it_zsdt0162 INTO wa_zsdt0162 WITH KEY vbeln = wa_zsdt0051-nro_sol_ov BINARY SEARCH.
      IF sy-subrc = 0.
        IF p_dalib IS NOT INITIAL.
          IF wa_zsdt0162-data_atual NOT IN p_dalib.
            CONTINUE.
          ENDIF.
        ENDIF.
        wa_saida-data_atual69  = wa_zsdt0162-data_atual.
      ELSEIF p_dalib IS NOT INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0051-kunnr
      IMPORTING
        output = wa_saida-kunnr.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_zsdt0051-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-name1_c              = wa_kna1-name1.
      wa_saida-ort01                = wa_kna1-ort01.
      wa_saida-regio                = wa_kna1-regio.
    ELSE.
      CLEAR wa_saida-name1_c.
    ENDIF.
    MOVE-CORRESPONDING  wa_saida TO wa_saida2.
    MOVE-CORRESPONDING  wa_saida TO wa_saida3.

    w_flag53 = ''.
    w_flag54 = ''.
    w_flag63 = ''.

    CLEAR cont_53.
    LOOP AT it_zsdt0053 INTO wa_zsdt0053 WHERE nro_sol_ov = wa_zsdt0051-nro_sol_ov. cont_53 = cont_53 + 1. ENDLOOP.
    wa_tables-name = 'IT_ZSDT0053'. wa_tables-qtd = cont_53. wa_tables-ov = wa_zsdt0051-nro_sol_ov. APPEND wa_tables TO it_tables.
    LOOP AT it_zsdt0054 INTO wa_zsdt0054 WHERE nro_sol_ov = wa_zsdt0051-nro_sol_ov. cont_54 = cont_54 + 1. ENDLOOP.
    wa_tables-name = 'IT_ZSDT0054'. wa_tables-qtd = cont_54. wa_tables-ov = wa_zsdt0051-nro_sol_ov. APPEND wa_tables TO it_tables.
    LOOP AT it_zsdt0063 INTO wa_zsdt0063 WHERE nro_sol_ov = wa_zsdt0051-nro_sol_ov. cont_63 = cont_63 + 1. ENDLOOP.
    wa_tables-name = 'IT_ZSDT0063'. wa_tables-qtd = cont_63. wa_tables-ov = wa_zsdt0051-nro_sol_ov. APPEND wa_tables TO it_tables.
    LOOP AT it_zsdt0066 INTO wa_zsdt0066 WHERE nro_sol_ov = wa_zsdt0051-nro_sol_ov. cont_66 = cont_66 + 1. ENDLOOP.
    wa_tables-name = 'IT_ZSDT0066'. wa_tables-qtd = cont_66. wa_tables-ov = wa_zsdt0051-nro_sol_ov. APPEND wa_tables TO it_tables.
    SORT it_tables BY qtd DESCENDING.

    CLEAR: cont_53_1, w_flag54_c.
    LOOP AT it_zsdt0053 INTO wa_zsdt0053 WHERE nro_sol_ov = wa_zsdt0051-nro_sol_ov.
      w_flag53 = 'X'.
      w_flag53_c = sy-tabix.
      cont_53_1 = cont_53_1 + 1.

      CLEAR wa_saida-fixacao.
      READ TABLE it_zsdt0073 INTO wa_zsdt0073 WITH KEY nro_sol_ov = wa_zsdt0053-nro_sol_ov
                                                       fixacao    = wa_zsdt0053-fixacao BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida-zterm      = wa_zsdt0073-zterm.
        wa_saida-qte_venc   = wa_zsdt0073-qte_venc.
        wa_saida-fixacao    = wa_zsdt0053-fixacao.
      ENDIF.

      IF p_matnr IS NOT INITIAL.
        IF  wa_zsdt0053-matnr NOT IN p_matnr.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF p_charg IS NOT INITIAL.
        IF  wa_zsdt0053-charg NOT IN p_charg.
          CONTINUE.
        ENDIF.
      ENDIF.

      wa_saida-status    = wa_zsdt0053-status.
      wa_saida-posnr     = wa_zsdt0053-posnr.

      READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_zsdt0053-matnr BINARY SEARCH.
      wa_saida-maktx     = wa_makt-maktx.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_zsdt0053-matnr
        IMPORTING
          output = wa_saida-matnr.

      wa_saida-werks     = wa_zsdt0053-werks.
      wa_saida-lgort     = wa_zsdt0053-lgort.
      wa_saida-charg     = wa_zsdt0053-charg.
      wa_saida-zmeng     = wa_zsdt0053-zmeng.
      wa_saida-zieme     = wa_zsdt0053-zieme.
      wa_saida-dmbtr     = wa_zsdt0053-dmbtr.
      wa_saida-pmein     = wa_zsdt0053-pmein.
      wa_saida-valdt     = wa_zsdt0053-valdt.
      wa_saida-ponto_c   = wa_zsdt0053-ponto_c.
      wa_saida-terminal  = wa_zsdt0053-terminal.
      wa_saida-brgew     = wa_zsdt0053-brgew.
      wa_saida-volum     = wa_zsdt0053-volum.
      wa_saida-voleh     = wa_zsdt0053-voleh.
      wa_saida-kursf     = wa_zsdt0053-kursf.

      wa_saida-contrato       =  wa_zsdt0053-contrato.
      wa_saida-classificacao  =  wa_zsdt0053-classificacao.
      wa_saida-navio          =  wa_zsdt0053-navio.
      wa_saida-porto          =  wa_zsdt0053-porto.
      wa_saida-p_porto        =  wa_zsdt0053-p_porto.
      wa_saida-vlt_porto      =  wa_zsdt0053-vlt_porto.
      wa_saida-instrucao      =  wa_zsdt0053-instrucao.


      READ TABLE it_vbap INTO wa_vbap
        WITH KEY vbeln = wa_zsdt0053-vbeln.

      IF sy-subrc IS INITIAL.
        wa_saida-qt_ord = wa_vbap-kwmeng.
      ELSE.
        wa_saida-qt_ord = 0.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_zsdt0053-vbeln
        IMPORTING
          output = wa_saida-vbeln.

      IF wa_zsdt0053-kunnr IS INITIAL.
        vkunnr = wa_zsdt0051-kunnr.
      ELSE.
        vkunnr = wa_zsdt0053-kunnr.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = vkunnr
        IMPORTING
          output = wa_saida-kunnr.

      READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = vkunnr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_saida-name1_c = wa_kna1-name1.
        wa_saida-ort01                = wa_kna1-ort01.
        wa_saida-regio                = wa_kna1-regio.
      ELSE.
        CLEAR wa_saida-name1_c.
      ENDIF.

      vsd_ordem = 0.
      vsd_total = 0.
      qt_dev    = 0.
      qt_com    = 0.
*---> 15/06/2023 - Migração S4 - JS
*        VSD_TOTAL = WA_ZSDT0053-VLRTOT.
      vsd_total = CONV #( wa_zsdt0053-vlrtot ).
*<--- 15/06/2023 - Migração S4 - JS

      LOOP AT it_vbfa INTO wa_vbfa WHERE vbelv = wa_zsdt0053-vbeln.
        IF wa_vbfa-vbtyp_n EQ 'J'.
          ADD wa_vbfa-rfmng TO vsd_ordem.
**        ELSEIF WA_VBFA-VBTYP_N EQ 'C' OR WA_VBFA-VBTYP_N EQ 'L'.
**          ADD WA_VBFA-RFMNG TO QT_COM.
**          ADD WA_VBFA-RFWRT TO VSD_TOTAL.
**        ELSE.
**          WA_VBFA-RFWRT = WA_VBFA-RFWRT * -1.
**          ADD WA_VBFA-RFMNG TO QT_DEV.
**          ADD WA_VBFA-RFWRT TO VSD_TOTAL.
        ENDIF.
**      ENDLOOP.


        LOOP AT it_vbfa_c INTO wa_vbfa_c WHERE vbelv = wa_vbfa-vbeln.
          READ TABLE it_vbfa_c_e TRANSPORTING NO FIELDS WITH KEY vbelv = wa_vbfa_c-vbeln.
          IF sy-subrc IS NOT INITIAL.

            READ TABLE it_bkpf INTO wa_bkpf WITH KEY awkey = wa_vbfa_c-vbeln_fk.
            READ TABLE it_bseg INTO wa_bseg WITH KEY bukrs = wa_bkpf-bukrs
                                                     belnr = wa_bkpf-belnr
                                                     gjahr = wa_bkpf-gjahr
                                                     koart = 'D'.
            ADD wa_bseg-wrbtr TO vsd_total.
            ADD wa_vbfa_c-rfmng TO qt_com.
            CLEAR: wa_bseg, wa_bkpf.

          ENDIF.
        ENDLOOP.

        LOOP AT it_vbfa_l INTO wa_vbfa_l WHERE vbelv = wa_vbfa-vbeln.
          READ TABLE it_vbfa_l_e TRANSPORTING NO FIELDS WITH KEY vbelv = wa_vbfa_l-vbeln.
          IF sy-subrc IS NOT INITIAL.

            READ TABLE it_bkpf INTO wa_bkpf WITH KEY awkey = wa_vbfa_l-vbeln_fk.
            READ TABLE it_bseg INTO wa_bseg WITH KEY bukrs = wa_bkpf-bukrs
                                                     belnr = wa_bkpf-belnr
                                                     gjahr = wa_bkpf-gjahr
                                                     koart = 'D'.
            ADD wa_bseg-wrbtr TO vsd_total.
            ADD wa_vbfa_l-rfmng TO qt_com.
            CLEAR: wa_bseg, wa_bkpf.
          ENDIF.
        ENDLOOP.

        LOOP AT it_vbfa_h INTO wa_vbfa_h WHERE vbelv = wa_vbfa-vbeln.
          READ TABLE it_vbfa_h_e TRANSPORTING NO FIELDS WITH KEY vbelv = wa_vbfa_h-vbeln.
          IF sy-subrc IS NOT INITIAL.
            wa_vbfa_h-rfwrt = wa_vbfa_h-rfwrt * -1.

            READ TABLE it_bkpf INTO wa_bkpf WITH KEY awkey = wa_vbfa_h-vbeln_fk.
            READ TABLE it_bseg INTO wa_bseg WITH KEY bukrs = wa_bkpf-bukrs
                                                     belnr = wa_bkpf-belnr
                                                     gjahr = wa_bkpf-gjahr
                                                     koart = 'D'.
            wa_bseg-wrbtr = wa_bseg-wrbtr * -1.
            ADD wa_bseg-wrbtr TO vsd_total.
            ADD wa_vbfa_h-rfmng TO qt_dev.
            CLEAR: wa_bseg, wa_bkpf.
          ENDIF.
        ENDLOOP.
      ENDLOOP.


      qt_dev = qt_dev * -1.
      wa_saida-sd_ordem =  wa_zsdt0053-zmeng - vsd_ordem. "Saldo da Ordem
      wa_saida-qt_dev   =  qt_dev.                        " Quantidade de Devolução
      wa_saida-qt_compl = qt_com.                         " Quantidade de Complemento
      wa_saida-vlrtot   = vsd_total.                      " Valor total da Ordem

      w_flag54 = ''.
      w_flag63 = ''.
      w_flag66 = ''.
      LOOP AT it_zsdt0063 INTO wa_zsdt0063 WHERE nro_sol_ov = wa_zsdt0051-nro_sol_ov.
        w_flag63 = 'X'.
      ENDLOOP.
      LOOP AT it_zsdt0066 INTO wa_zsdt0066 WHERE nro_sol_ov = wa_zsdt0051-nro_sol_ov.
        w_flag66 = 'X'.
      ENDLOOP.

      READ TABLE it_zsdt0054 TRANSPORTING NO FIELDS WITH KEY nro_sol_ov = wa_zsdt0053-nro_sol_ov
                                                             posnr      = wa_zsdt0053-posnr.
      w_flag53_c = sy-subrc.
      IF w_flag53_c IS INITIAL.
        LOOP AT it_zsdt0054 INTO wa_zsdt0054 WHERE nro_sol_ov = wa_zsdt0053-nro_sol_ov
                                               AND posnr      = wa_zsdt0053-posnr.
          w_flag54 = 'X'.
          w_flag54_c = sy-tabix.

          wa_saida3-valdt_ant  =    wa_saida-valdt_ant  = wa_zsdt0054-valdt.
          wa_saida3-dmbtr_ant  =    wa_saida-dmbtr_ant  = wa_zsdt0054-dmbtr.
          wa_saida3-adiant     =    wa_saida-adiant     = wa_zsdt0054-adiant.
          wa_saida3-kursf54    =    wa_saida-kursf54    = wa_zsdt0054-kursf.
          wa_saida3-vlr_real   =    wa_saida-vlr_real   = wa_zsdt0054-vlr_real.

          IF cont_53_1 EQ w_flag54_c.
            APPEND wa_saida TO it_saida.
            PERFORM limpa_campo_53.
          ELSE.
            APPEND wa_saida TO it_saida.
            PERFORM limpa_campo_53.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF  w_flag54 = ''.
        CLEAR: wa_saida-valdt_ant, wa_saida-dmbtr_ant, wa_saida-adiant, wa_saida-kursf54, wa_saida-vlr_real.
        APPEND wa_saida TO it_saida.
      ENDIF.

    ENDLOOP.

    IF wa_saida-pgto_ant+0(1) = 'X' OR wa_saida-pgto_ant+0(1) = 'N'.
      IF  w_flag53 = '' OR w_flag54_c EQ 0.
        LOOP AT it_zsdt0054 INTO wa_zsdt0054 WHERE nro_sol_ov = wa_zsdt0051-nro_sol_ov.
          w_flag53 = 'X'.
          w_flag54_c = sy-tabix.

          wa_saida3-valdt_ant  =    wa_saida-valdt_ant  = wa_zsdt0054-valdt.
          wa_saida3-dmbtr_ant  =    wa_saida-dmbtr_ant  = wa_zsdt0054-dmbtr.
          wa_saida3-adiant     =    wa_saida-adiant     = wa_zsdt0054-adiant.
          wa_saida3-kursf54    =    wa_saida-kursf54    = wa_zsdt0054-kursf.
          wa_saida3-vlr_real   =    wa_saida-vlr_real   = wa_zsdt0054-vlr_real.

          IF cont_53 >= w_flag54_c.
            MODIFY it_saida FROM wa_saida INDEX tabix51 TRANSPORTING valdt_ant
                                                                          dmbtr_ant
                                                                          adiant
                                                                          kursf54
                                                                          vlr_real.

            CLEAR: wa_saida-zmeng,wa_saida-dmbtr, wa_saida-vlrtot,wa_saida-sd_ordem,wa_saida-brgew,wa_saida-volum,wa_saida-kursf.     "zera nivel
          ELSE.
            APPEND wa_saida3 TO it_saida.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    w_flag63 = ''.
    LOOP AT it_zsdt0063 INTO wa_zsdt0063 WHERE nro_sol_ov = wa_zsdt0051-nro_sol_ov.
      tabix = sy-tabix.
      w_flag63 = 'X'.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zsdt0063-lifnr BINARY SEARCH.
      wa_saida-posnr63  = wa_zsdt0063-posnr.
      wa_saida-valdt63  = wa_zsdt0063-valdt.
      wa_saida-bukrs63  = wa_zsdt0063-bukrs.
      wa_saida-dmbtr63  = wa_zsdt0063-dmbtr.
      wa_saida-waers63  = wa_zsdt0063-waers.
      wa_saida-lifnr63  = wa_zsdt0063-lifnr.
      wa_saida-name163  = wa_lfa1-name1.
      wa_saida-banks63  = wa_zsdt0063-banks.
      wa_saida-bankl63  = wa_zsdt0063-bankl.
      wa_saida-swift63  = wa_zsdt0063-swift.
      wa_saida-bankn63  = wa_zsdt0063-bankn.
      wa_saida-adiant63 = wa_zsdt0063-adiant.

      READ TABLE it_zsdt0063 INTO wa_zsdt0063 WITH KEY nro_sol_ov = wa_zsdt0051-nro_sol_ov BINARY SEARCH.
      IF sy-subrc = 0 AND wa_zsdt0063-nro_sol_ov EQ wa_zsdt0051-nro_sol_ov.
        tabix = sy-tabix.
        MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING  posnr63
                                                                valdt63
                                                                bukrs63
                                                                dmbtr63
                                                                waers63
                                                                lifnr63
                                                                name163
                                                                banks63
                                                                bankl63
                                                                swift63
                                                                bankn63
                                                                taxa_curva
                                                                adiant63.
      ELSE.
        APPEND wa_saida TO it_saida.
      ENDIF.

      CLEAR: wa_saida-zmeng,wa_saida-dmbtr, wa_saida-vlrtot,wa_saida-sd_ordem,wa_saida-brgew,wa_saida-volum,wa_saida-kursf.     "zera nivel
    ENDLOOP.

    w_flag66 = ''.
    LOOP AT it_zsdt0066 INTO wa_zsdt0066 WHERE nro_sol_ov = wa_zsdt0051-nro_sol_ov.

      DELETE it_tables WHERE name EQ 'IT_ZSDT0066'.
      LOOP AT it_tables INTO wa_tables.
        IF sy-tabix EQ 1.
          cont_s = wa_tables-qtd.
        ENDIF.
      ENDLOOP.

      w_flag66 = 'X'.
      w_flag66_c = sy-tabix.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_zsdt0066-matnr
        IMPORTING
          output = wa_saida-matnr66.
      wa_saida2-matnr66           =   wa_saida-matnr66.

      wa_saida2-posnr66           =	  wa_saida-posnr66         = wa_zsdt0066-posnr.
      wa_saida2-werks66           =   wa_saida-werks66         = wa_zsdt0066-werks.
      wa_saida2-lgort66           =   wa_saida-lgort66         = wa_zsdt0066-lgort.
      wa_saida2-charg66           =   wa_saida-charg66         = wa_zsdt0066-charg.
      wa_saida2-zmeng66           =   wa_saida-zmeng66         = wa_zsdt0066-zmeng.
      wa_saida2-volum66           =   wa_saida-volum66         = wa_zsdt0066-volum.
      wa_saida2-dmbtr66           =   wa_saida-dmbtr66         = wa_zsdt0066-dmbtr.
      wa_saida2-vlrtot66          =   wa_saida-vlrtot66        = wa_zsdt0066-vlrtot.
      wa_saida2-libra_to          =   wa_saida-libra_to        = wa_zsdt0066-libra_to.
      wa_saida2-usd_to            =   wa_saida-usd_to          = wa_zsdt0066-usd_to.
      wa_saida2-vlr_tot_frm_usd   =   wa_saida-vlr_tot_frm_usd = wa_zsdt0066-vlr_tot_frm_usd.



      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_zsdt0066-kunnr
        IMPORTING
          output = wa_saida-kunnr66.
      wa_saida2-kunnr66 = wa_saida-kunnr66.

      wa_saida-instrucao66    = wa_zsdt0066-instrucao.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_zsdt0066-terminal
        IMPORTING
          output = wa_saida-terminal66.
      wa_saida2-terminal66 = wa_saida-terminal66.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_zsdt0066-lentrega
        IMPORTING
          output = wa_saida-lentrega66.
      wa_saida2-lentrega66 = wa_saida-lentrega66.

      wa_saida2-inco166           = wa_saida-inco166        = wa_zsdt0066-inco1.
      wa_saida2-inco266           = wa_saida-inco266        = wa_zsdt0066-inco2.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_zsdt0066-vbeln
        IMPORTING
          output = wa_saida-vbeln66.
      wa_saida2-vbeln66 = wa_saida-vbeln66.

      cont = lines( it_saida ).
      IF wa_zsdt0066-nro_sol_ov EQ wa_zsdt0051-nro_sol_ov.

        IF  cont_s GE sy-tabix AND w_flag53 EQ 'X' OR w_flag54 EQ 'X'.
          MODIFY it_saida FROM wa_saida2 INDEX w_flag66_c TRANSPORTING       matnr66
                                                                       posnr66
                                                                       werks66
                                                                       lgort66
                                                                       charg66
                                                                       zmeng66
                                                                       volum66
                                                                       dmbtr66
                                                                       vlrtot66
                                                                       libra_to
                                                                       usd_to
                                                                       vlr_tot_frm_usd
                                                                       kunnr66
                                                                       instrucao66
                                                                       terminal66
                                                                       lentrega66
                                                                       inco166
                                                                       inco266
                                                                       vbeln66.
        ELSE.
*          clear wa_saida.
          APPEND wa_saida2 TO it_saida.
        ENDIF.
      ENDIF.
      CLEAR: wa_saida-zmeng,wa_saida-dmbtr, wa_saida-vlrtot,wa_saida-sd_ordem,wa_saida-brgew,wa_saida-volum,wa_saida-kursf.     "zera nivel
    ENDLOOP.


    IF w_flag53 = '' AND w_flag63 = '' AND w_flag66 = ''.
      APPEND wa_saida TO it_saida.
    ENDIF.
    IF w_flag63 = '' OR w_flag66 = ''.
      PERFORM limpa_campo_63_66.
    ENDIF.
    CLEAR wa_saida.
  ENDLOOP. " FIM 51

  LOOP AT it_saida INTO  wa_saida.
    APPEND wa_saida TO it_saida_aux.
  ENDLOOP.
  CLEAR: it_saida, wa_saida.

ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .

  PERFORM f_definir_eventos.
  PERFORM f_alv_sort.
  PERFORM f_alv.

* Impressão do ALV passando tabela dinâmica
  PERFORM f_imprime_dados_alv USING <fs_data>.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING:
                                     slis_ev_top_of_page  'XTOP_OF_PAGE'.

ENDFORM.                    " F_DEFINIR_EVENTOS

*----------------------------------------------------------------------*
*       Form  f_monta_top_of_page
*----------------------------------------------------------------------*
FORM top_of_page.

* Cabeçalho Logo
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = list_top_of_page[].
  "I_LOGO             = 'WELLA_LOGO'.

ENDFORM.        " top_of_page.

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_0621   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                      " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_sort .
  CLEAR vg_i.
  REFRESH t_sort.

  mc_preenche_class:
                      'NRO_SOL_OV'       '' 'X' 'X',
                      'DATA_ATUAL'       '' 'X' ' ',
                      'DATA_VENDA'       '' 'X' ' ',
                      'DIF'              '' 'X' ' ',
                      'DATA_ATUAL69'     '' 'X' ' ',
                      'TP_VENDA'         '' 'X' ' ',
                      'AUART'            '' 'X' ' ',
                      'VKORG'            '' 'X' ' ',
                      'VTWEG'            '' 'X' ' ',
                      'SPART'            '' 'X' ' ',
                      'VKBUR'            '' 'X' ' ',
                      'VKGRP'            '' 'X' ' ',
                      'KUNNR'            '' 'X' ' ',
                      'NAME1_C'          '' 'X' ' ',
                      'ORT01'            '' 'X' ' ',
                      'REGIO'            '' 'X' ' ',
                      'CORRETO'          '' 'X' ' ',
                      'NAME1_F'          '' 'X' ' ',
                      'BSTKD'            '' 'X' ' ',
                      'INCO1'            '' 'X' ' ',
                      'INCO2'            '' 'X' ' ',
                      'VKAUS'            '' 'X' ' ',
                      'WAERK'            '' 'X' ' ',
                      'DTDE_LOGIST'      '' 'X' ' ',
                      'DTATE_LOGIST'     '' 'X' ' ',
                      'NUM_FIXACAO'      '' 'X' ' ',
                      'OBSERVACAO'       '' 'X' ' ',
                      'COMENT_LOGISTICA' '' 'X' ' ',
                      'ZLSCH'            '' 'X' ' ',
                      'PGTO_ANT'         '' 'X' ' ',
                      'ZTERM'            '' 'X' ' ',
                      'QTE_VENC'         '' 'X' ' ',
                      'VALDT52'          '' 'X' ' ',
                      'HBKID'            '' 'X' ' ',
                      'STATUS'           '' 'X' ' ',
                      'POSNR'            '' 'X' ' ',
                      'MATNR'            '' 'X' ' ',
                      'MAKTX'            '' 'X' ' ',
                      'WERKS'            '' 'X' ' ',
                      'PONTO_C'          '' 'X' ' ',
                      'TERMINAL'         '' 'X' ' ',
                      'LGORT'            '' 'X' ' ',
                      'CHARG'            '' 'X' ' ',
                      'ZMENG'            '' 'D' ' ',
                      'ZIEME'            '' 'D' ' ',
                      'QT_DEV'           '' 'D' ' ',
                      'QT_COMPL'         '' 'D' ' ',
                      'QT_ORD'           '' 'D' ' ',
                      'DMBTR'            '' 'D' ' ',
                      'PMEIN'            '' 'X' ' ',
                      'VLRTOT'           '' 'D' ' ',
                      'VALDT'            '' 'D' ' ',
                      'BRGEW'            '' 'D' ' ',
                      'VOLUM'            '' 'D' ' ',
                      'VOLEH'            '' 'D' ' ',
                      'KURSF'            '' 'D' ' ',
                      'VBELN'            '' 'X' ' ',
                      'SD_ORDEM'         '' 'X' ' '.

ENDFORM.                    " F_ALV_SORT
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0683   text
*      -->P_TEXTOCAB  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho    USING typ text.
  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.
ENDFORM.                    " F_CONSTRUIR_CABECALHO

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv .
  REFRESH it_fcat.
  PERFORM alv_preenche_cat USING:
               'STATUS51'         'Status Solic.'  '05'       ' '     ' '    ' ' , "Status Solic.
               'NRO_SOL_OV'       TEXT-002        '15'       ' '     ' '    ' ' , "Solicitação Nº
               'DATA_ATUAL'       TEXT-066        '15'       ' '     ' '    ' ' , "Data Solicitação
               'DATA_VENDA'       TEXT-086        '15'       ' '     ' '    ' ' , "Data Venda
               'DIF'              TEXT-087        '10'       ' '     ' '    ' ' , "Dif
               'DATA_ATUAL69'     TEXT-101        '12'       ' '     ' '    ' ' , "Data da Liberação
               'TP_VENDA'         TEXT-003        '15'       ' '     ' '    ' ' , "Tipo de Venda.
               'AUART'            TEXT-004        '15'       ' '     ' '    ' ' , "Tipo  O.V.
               'VKORG'            TEXT-005        '15'       ' '     ' '    ' ' , "Org. de Vendas
               'VTWEG'            TEXT-006        '15'       ' '     ' '    ' ' , "Canal Dist.
               'SPART'            TEXT-007        '15'       ' '     ' '    ' ' , "Setor Atividade
               'VKBUR'            TEXT-008        '15'       ' '     ' '    ' ' , "Esc. de vendas
               'VKGRP'            TEXT-009        '15'       ' '     ' '    ' ' , "Eq. de vendas
               'KUNNR'            TEXT-010        '15'       ' '     ' '    ' ' , "Cliente
               'NAME1_C'          TEXT-041        '30'       ' '     ' '    ' ' , "Nome Cliente
               'ORT01'            TEXT-111        '15'       ' '     ' '    ' ' , "Cidade
               'REGIO'            TEXT-112        '05'       ' '     ' '    ' ' , "Estado
               'CORRETO'          TEXT-042        '15'       ' '     ' '    ' ' , "Corretor
               'NAME1_F'          TEXT-043        '30'       ' '     ' '    ' ' , "Nome Corretor
               'BSTKD'            TEXT-011        '15'       ' '     ' '    ' ' , "Nº Pedido Cliente
               'INCO1'            TEXT-012        '15'       ' '     ' '    ' ' , "Frete
               'INCO2'            TEXT-013        '15'       ' '     ' '    ' ' , "Frete 2
               'VKAUS'            TEXT-014        '15'       ' '     ' '    ' ' , "Utilização
               'WAERK'            TEXT-015        '15'       ' '     ' '    ' ' , "Moeda Documento
               'TAXA_CURVA'       TEXT-102        '15'       ' '     ' '    ' ' , "Taxa curva
               'DTDE_LOGIST'      TEXT-016        '15'       ' '     ' '    ' ' , "Prog. Logistica Inicio
               'DTATE_LOGIST'     TEXT-017        '15'       ' '     ' '    ' ' , "Prog. Logistica Fim
               'NUM_FIXACAO'      TEXT-069        '15'       ' '     ' '    ' ' ,
               'OBSERVACAO'       TEXT-018        '15'       ' '     ' '    ' ' , "Observações
               'COMENT_LOGISTICA' TEXT-019        '15'       ' '     ' '    ' ' , "Comt. Logistica
               'ZLSCH'            TEXT-020        '15'       ' '     ' '    ' ' , "Forma Pag.
               'PGTO_ANT'         TEXT-021        '15'       ' '     ' '    ' ' , "Pag. Antecipado
               'ZTERM'            TEXT-022        '15'       ' '     ' '    ' ' , "Cond. Pag
               'QTE_VENC'         TEXT-023        '15'       ' '     ' '    ' ' , "Qtd Vencimentos
               'VALDT52'          TEXT-100        '12'       ' '     ' '    ' ' , "Data Vencimento
               'HBKID'            TEXT-024        '15'       ' '     ' '    ' ' , "Banco
               'STATUS'           TEXT-025        '15'       ' '     ' '    ' ' , "Status Item
               'POSNR'            TEXT-026        '15'       ' '     ' '    ' ' , "Item
               'FIXACAO'          'Fixação'       '15'       ' '     ' '    ' ' , "Fixação
               'MATNR'            TEXT-027        '15'       ' '     ' '    ' ' , "Material
               'MAKTX'            TEXT-056        '15'       ' '     ' '    ' ' , "Descr. Material
               'WERKS'            TEXT-028        '15'       ' '     ' '    ' ' , "Centro
               'PONTO_C'          TEXT-063        '15'       ' '     ' '    ' ' , "Ponto Coleta
               'TERMINAL'         TEXT-058        '15'       ' '     ' '    ' ' , "Terminal
               'LGORT'            TEXT-029        '15'       ' '     ' '    ' ' , "Depósito
               'CHARG'            TEXT-030        '15'       ' '     ' '    ' ' , "Lote
               'ZMENG'            TEXT-031        '15'       ' '     ' '    'X' , "Qtd Prevista
               'ZIEME'            TEXT-032        '15'       ' '     ' '    ' ' , "UM
               'QT_DEV'           TEXT-067        '15'       ' '     ' '    ' ' , "Qt.Dev.
               'QT_COMPL'         TEXT-110        '15'       ' '     ' '    ' ' , "Qt.Compl.
               'QT_ORD'           TEXT-068        '15'       ' '     ' '    ' ' , "Qte.Ordem
               'DMBTR'            TEXT-033        '15'       ' '     ' '    ' ' , "Preço
               'PMEIN'            TEXT-034        '15'       ' '     ' '    ' ' , "UM Preço
               'VLRTOT'           TEXT-035        '15'       ' '     ' '    'X' , "Valor Total
               'VALDT'            TEXT-036        '15'       ' '     ' '    ' ' , "Data Vencimento
               'BRGEW'            TEXT-059        '15'       ' '     ' '    ' ' , "Peso bruto
               'VOLUM'            TEXT-060        '15'       ' '     ' '    ' ' , "Volume
               'VOLEH'            TEXT-061        '15'       ' '     ' '    ' ' , "UM Volume
               'KURSF'            TEXT-062        '15'       ' '     ' '    ' ' , "Câmbio Item
               'CONTRATO'         TEXT-103        '15'       ' '     ' '    ' ' , "CONTRATO,
               'CLASSIFICACAO'    TEXT-104        '15'       ' '     ' '    ' ' , "Classificação
               'NAVIO'            TEXT-105        '15'       ' '     ' '    ' ' , "Navio
               'PORTO'            TEXT-106        '15'       ' '     ' '    ' ' , "POrto
               'P_PORTO'          TEXT-107        '15'       ' '     ' '    ' ' , "Preço Porto
               'VLT_PORTO'        TEXT-108        '15'       ' '     ' '    'X' , "Total Porto
               'INSTRUCAO'        TEXT-109        '15'       ' '     ' '    ' ' , "Instrução
               'VBELN'            TEXT-037        '15'       'X'     ' '    ' ' , "Ordem Venda
               'SD_ORDEM'         TEXT-057        '15'       ' '     ' '    'X' , "Sdo.Ordem
               'VALDT_ANT'        TEXT-038        '15'       ' '     ' '    ' ' , "Data Venc. Pag. Ant.
               'DMBTR_ANT'        TEXT-039        '15'       ' '     ' '    ' ' , "Valor pag. Ant.
               'ADIANT'           TEXT-040        '15'       ' '     ' '    ' ' , "Doc Pag. Ant.
               'KURSF54'          TEXT-064        '15'       ' '     ' '    ' ' , "Câmbio Pag. Ant.
               'VLR_REAL'         TEXT-065        '15'       ' '     ' '    ' ' , "Valor Real Pag. Ant.
               'BUKRS63'          TEXT-044        '15'       ' '     ' '    ' ' , "Emp. Ad. Ext.
               'POSNR63'          TEXT-045        '15'       ' '     ' '    ' ' , "
               'VALDT63'          TEXT-046        '15'       ' '     ' '    ' ' , "
               'DMBTR63'          TEXT-047        '15'       ' '     ' '    ' ' , "
               'WAERS63'          TEXT-048        '15'       ' '     ' '    ' ' , ".
               'LIFNR63'          TEXT-049        '15'       ' '     ' '    ' ' , "
               'NAME163'          TEXT-050        '30'       ' '     ' '    ' ' , "
               'ADIANT63'         TEXT-055        '15'       ' '     ' '    ' ' , "
               'POSNR66'          TEXT-085        '15'       ' '     ' '    ' ' , "Item FRM
               'MATNR66'          TEXT-070        '15'       ' '     ' '    ' ' , "Material FRM
               'WERKS66'          TEXT-071        '15'       ' '     ' '    ' ' , "Centro FRM
               'LGORT66'          TEXT-072        '15'       ' '     ' '    ' ' , "Deposito FRM
               'CHARG66'          TEXT-073        '15'       ' '     ' '    ' ' , "Lote FRM
               'ZMENG66'          TEXT-074        '15'       ' '     ' '    ' ' , "Qtd Prevista FRM
               'VOLUM66'          TEXT-075        '15'       ' '     ' '    ' ' , "Volume FRM
               'DMBTR66'          TEXT-076        '15'       ' '     ' '    ' ' , "Valor FRM

               'LIBRA_TO'         TEXT-097        '15'       ' '     ' '    ' ' , "Valor Libra/TO
               'USD_TO'           TEXT-098        '15'       ' '     ' '    ' ' , "Valor Dólar/TO

               'VLRTOT66'         TEXT-077        '15'       ' '     ' '    ' ' , "Total FRM
               'VLR_TOT_FRM_USD'  TEXT-099        '15'       ' '     ' '    ' ' , "Total FRM Dólar

               'KUNNR66'          TEXT-078        '15'       ' '     ' '    ' ' , "Cliente FRM
               'INSTRUCAO66'      TEXT-079        '15'       ' '     ' '    ' ' , "Instrução FRM
               'TERMINAL66'       TEXT-080        '15'       ' '     ' '    ' ' , "Terminal FRM
               'INCO166'          TEXT-081        '15'       ' '     ' '    ' ' , "Frete FRM
               'INCO266'          TEXT-082        '15'       ' '     ' '    ' ' , "Frete 2 FRM
               'LENTREGA66'       TEXT-083        '15'       ' '     ' '    ' ' , "Local Entrega FRM
               'VBELN66'          TEXT-084        '15'       'X'     ' '    ' ' , "Ordem FRM

               'VLR_VENCIDO_BRL'  TEXT-088        '15'       ''     ' '     ' ' , "
               'VLR_AVENCER_BRL'  TEXT-089        '15'       ''     ' '     ' ' , "
               'TOTAL_BRL'        TEXT-090        '15'       ''     ' '     ' ' , "
               'VLR_ADIANT_BRL'   TEXT-091        '15'       ''     ' '     ' ' , "
               'TOTAL_MOV_BRL'    TEXT-092        '15'       ''     ' '     ' ' , "
               'LIMITE_CREDITO'   TEXT-093        '15'       ''     ' '     ' ' , "
               'SALDO_DISP_BRL'   TEXT-094        '15'       ''     ' '     ' ' , "
               'UTIL_LIMIT_BRL'   TEXT-095        '15'       ''     ' '     ' ' , "
               'SDO_OV_EMIT'      TEXT-096        '15'       ''     ' '     ' ' , "
               'QTD_FATUR'        TEXT-113        '15'       ''     ' '     ' ' . " Rubenilson - 24.09.24 - #145367

  REFRESH lt_fcat_lvc.
  PERFORM monta_fieldcat USING:
               'STATUS51'         'ZSDT0051' 'Status Solic.' '05'       'STATUS'            , "Status Solic.
               'NRO_SOL_OV'       'ZSDT0051' TEXT-002        '15'       'NRO_SOL_OV'        , "Solicitação Nº
               'DATA_ATUAL'       'ZSDT0051' TEXT-066        '15'       'DATA_ATUAL'        , "Data Solicitação
               'DATA_VENDA'       'ZSDT0051' TEXT-086        '15'       'DATA_VENDA'        , "Data Venda
               'DIF'              ''         TEXT-087        '10'       ''                  , "Dif
               'DATA_ATUAL69'     'ZSDT0069' TEXT-101        '12'       'DATA_ATUAL'        , "Data da Liberação
               'TP_VENDA'         'ZSDT0051' TEXT-003        '15'       'TP_VENDA'          , "Tipo de Venda.
               'AUART'            'ZSDT0051' TEXT-004        '15'       'AUART'             , "Tipo  O.V.
               'VKORG'            'ZSDT0051' TEXT-005        '15'       'VKORG'             , "Org. de Vendas
               'VTWEG'            'ZSDT0051' TEXT-006        '15'       'VTWEG'             , "Canal Dist.
               'SPART'            'ZSDT0051' TEXT-007        '15'       'SPART'             , "Setor Atividade
               'VKBUR'            'ZSDT0051' TEXT-008        '15'       'VKBUR'             , "Esc. de vendas
               'VKGRP'            'ZSDT0051' TEXT-009        '15'       'VKGRP'             , "Eq. de vendas
               'KUNNR'            'ZSDT0051' TEXT-010        '15'       'KUNNR'             , "Cliente
               'NAME1_C'          'KNA1'     TEXT-041        '30'       'NAME1'             , "Nome Cliente
               'ORT01'            'KNA1'     TEXT-111        '15'       'ORT01'             , "Cidade
               'REGIO'            'KNA1'     TEXT-112        '05'       'REGIO'             , "Estado
               'CORRETO'          'ZSDT0051' TEXT-042        '15'       'CORRETO'           , "Corretor
               'NAME1_F'          'LFA1'     TEXT-043        '30'       'NAME1'             , "Nome Corretor
               'BSTKD'            'ZSDT0051' TEXT-011        '15'       'BSTKD'             , "Nº Pedido Cliente
               'INCO1'            'ZSDT0051' TEXT-012        '15'       'INCO1'             , "Frete
               'INCO2'            'ZSDT0051' TEXT-013        '15'       'INCO2'             , "Frete 2
               'VKAUS'            'ZSDT0051' TEXT-014        '15'       'VKAUS'             , "Utilização
               'WAERK'            'ZSDT0051' TEXT-015        '15'       'WAERK'             , "Moeda Documento
               'TAXA_CURVA'       'ZSDT0051' TEXT-102        '15'       'TAXA_CURVA'        , "Taxa curva
               'DTDE_LOGIST'      'ZSDT0051' TEXT-016        '15'       'DTDE_LOGIST'       , "Prog. Logistica Inicio
               'DTATE_LOGIST'     'ZSDT0051' TEXT-017        '15'       'DTATE_LOGIST'      , "Prog. Logistica Fim
               'NUM_FIXACAO'      'ZSDT0051' TEXT-069        '15'       'NUM_FIXACAO'       ,
               'OBSERVACAO'       'ZSDT0051' TEXT-018        '15'       'OBSERVACAO'        , "Observações
               'COMENT_LOGISTICA' 'ZSDT0051' TEXT-019        '15'       'COMENT_LOGISTICA'  , "Comt. Logistica
               'ZLSCH'            'ZSDT0052' TEXT-020        '15'       'ZLSCH'             , "Forma Pag.
               'PGTO_ANT'         ''         TEXT-021        '15'       ''                  , "Pag. Antecipado
               'ZTERM'            'ZSDT0052' TEXT-022        '15'       'ZTERM'             , "Cond. Pag
               'QTE_VENC'         'ZSDT0052' TEXT-023        '15'       'QTE_VENC'          , "Qtd Vencimentos
               'VALDT52'          'ZSDT0052' TEXT-101        '15'       'VALDT'             , "Data Vencimento
               'HBKID'            'ZSDT0052' TEXT-024        '15'       'HBKID'             , "Banco
               'STATUS'           'ZSDT0053' TEXT-025        '15'       'STATUS'            , "Status Item
               'POSNR'            'ZSDT0053' TEXT-026        '15'       'POSNR'             , "Item
               'FIXACAO'          'ZSDT0053' 'Fixação'       '15'       'FIXACAO'           , "Fixação
               'MATNR'            'ZSDT0053' TEXT-027        '15'       'MATNR'             , "Material
               'MAKTX'            'MAKT'     TEXT-056        '30'       'MAKTX'             , "Descr. Material
               'WERKS'            'ZSDT0053' TEXT-028        '15'       'WERKS'             , "Centro
               'PONTO_C'          'ZSDT0053' TEXT-063        '15'       'PONTO_C'           , "Ponto Coleta
               'TERMINAL'         'ZSDT0053' TEXT-058        '15'       'TERMINAL'          , "Terminal
               'LGORT'            'ZSDT0053' TEXT-029        '15'       'LGORT'             , "Depósito
               'CHARG'            'ZSDT0053' TEXT-030        '15'       'CHARG'             , "Lote
               'ZMENG'            'ZSDT0053' TEXT-031        '15'       'ZMENG'             , "Qtd Prevista
               'ZIEME'            'ZSDT0053' TEXT-032        '15'       'ZIEME'             , "UM
*               'QT_DEV'           'VBFA'     TEXT-067        '15'       'RFMNG'             , "Qt.Dev.\Compl.
               'QT_DEV'           'VBFA'     TEXT-067        '15'       'RFMNG'             , "Qt.Dev.
               'QT_COMPL'         'VBFA'     TEXT-110        '15'       'RFMNG'             , "Qt.Compl.
               'QT_ORD'           'VBAP'     TEXT-068        '15'       'KWMENG'            , "Qte.Ordem
               'DMBTR'            'ZSDT0053' TEXT-033        '15'       'DMBTR'             , "Preço
               'PMEIN'            'ZSDT0053' TEXT-034        '15'       'PMEIN'             , "UM Preço
               'VLRTOT'           'ZSDT0053' TEXT-035        '15'       'VLRTOT'            , "Valor Total
               'VALDT'            'ZSDT0053' TEXT-036        '15'       'VALDT'             , "Data Vencimento
               'BRGEW'            'ZSDT0053' TEXT-059        '15'       'BRGEW'             , "Peso bruto
               'VOLUM'            'ZSDT0053' TEXT-060        '15'       'VOLUM'             , "Volume
               'VOLEH'            'ZSDT0053' TEXT-061        '15'       'VOLEH'             , "UM Volume
               'KURSF'            'ZSDT0053' TEXT-062        '15'       'KURSF'             , "Câmbio Item

               'CONTRATO'         'ZSDT0053' TEXT-103        '15'       'CONTRATO'          , "CONTRATO,
               'CLASSIFICACAO'    'ZSDT0053' TEXT-104        '15'       'CLASSIFICACAO'     , "Classificação
               'NAVIO'            'ZSDT0053' TEXT-105        '15'       'NAVIO'             , "Navio
               'PORTO'            'ZSDT0053' TEXT-106        '15'       'PORTO'             , "POrto
               'P_PORTO'          'ZSDT0053' TEXT-107        '15'       'P_PORTO'           , "Preço Porto
               'VLT_PORTO'        'ZSDT0053' TEXT-108        '15'       'VLT_PORTO'         , "Total Porto
               'INSTRUCAO'        'ZSDT0053' TEXT-109        '15'       'INSTRUCAO'         , "Instrução

               'VBELN'            'ZSDT0053' TEXT-037        '15'       'VBELN'             , "Ordem Venda
               'SD_ORDEM'         'VBFA'     TEXT-057        '15'       'RFMNG'             , "Sdo.Ordem
               'VALDT_ANT'        'ZSDT0054' TEXT-038        '15'       'VALDT'             , "Data Venc. Pag. Ant.
               'DMBTR_ANT'        'ZSDT0054' TEXT-039        '15'       'DMBTR'             , "Valor pag. Ant.
               'ADIANT'           'ZSDT0054' TEXT-040        '15'       'ADIANT'            , "Doc Pag. Ant.
               'KURSF54'          'ZSDT0054' TEXT-064        '15'       'KURSF'             , "Câmbio Pag. Ant.
               'VLR_REAL'         'ZSDT0054' TEXT-065        '15'       'VLR_REAL'          , "Valor Real Pag. Ant.
               'BUKRS63'          'ZSDT0063' TEXT-044        '15'       'BUKRS'             , "Emp. Ad. Ext.
               'POSNR63'          'ZSDT0063' TEXT-045        '15'       'POSNR'             , "
               'VALDT63'          'ZSDT0063' TEXT-046        '15'       'VALDT'             , "
               'DMBTR63'          'ZSDT0063' TEXT-047        '15'       'DMBTR'             , "
               'WAERS63'          'ZSDT0063' TEXT-048        '15'       'WAERS'             , ".
               'LIFNR63'          'ZSDT0063' TEXT-049        '15'       'LIFNR'             , "
               'NAME163'          'LFA1'     TEXT-050        '30'       'NAME1'             , "
               'ADIANT63'         'ZSDT0063' TEXT-055        '15'       'ADIANT'            , "
               'POSNR66'          'ZSDT0066' TEXT-085        '15'       'POSNR'             , "Item FRM
               'MATNR66'          'ZSDT0066' TEXT-070        '15'       'MATNR'             , "Material FRM
               'WERKS66'          'ZSDT0066' TEXT-071        '15'       'WERKS'             , "Centro FRM
               'LGORT66'          'ZSDT0066' TEXT-072        '15'       'LGORT'             , "Deposito FRM
               'CHARG66'          'ZSDT0066' TEXT-073        '15'       'CHARG'             , "Lote FRM
               'ZMENG66'          'ZSDT0066' TEXT-074        '15'       'ZMENG'             , "Qtd Prevista FRM
               'VOLUM66'          'ZSDT0066' TEXT-075        '15'       'VOLUM'             , "Volume FRM
               'DMBTR66'          'ZSDT0066' TEXT-076        '15'       'DMBTR'             , "Valor FRM

               'LIBRA_TO'         'ZSDT0066' TEXT-097        '15'       'LIBRA_TO'           ,
               'USD_TO'           'ZSDT0066' TEXT-098        '15'       'USD_TO'             ,

               'VLRTOT66'         'ZSDT0066' TEXT-077        '15'       'VLRTOT'            , "Total FRM
               'VLR_TOT_FRM_USD'  'ZSDT0066' TEXT-099        '15'       'VLR_TOT_FRM_USD'   ,

               'KUNNR66'          'ZSDT0066' TEXT-078        '15'       'KUNNR'             , "Cliente FRM
               'INSTRUCAO66'      'ZSDT0066' TEXT-079        '15'       'INSTRUCAO'         , "Instrução FRM
               'TERMINAL66'       'ZSDT0066' TEXT-080        '15'       'TERMINAL'          , "Terminal FRM
               'INCO166'          'ZSDT0066' TEXT-081        '15'       'INCO1'             , "Frete FRM
               'INCO266'          'ZSDT0066' TEXT-082        '15'       'INCO2'             , "Frete 2 FRM
               'LENTREGA66'       'ZSDT0066' TEXT-083        '15'       'LENTREGA'          , "Local Entrega FRM
               'VBELN66'          'ZSDT0066' TEXT-084        '15'       'VBELN'             , "Ordem FRM

               'VLR_VENCIDO_BRL'  'ZSDT0084' TEXT-088        '15'       'VLR_VENCIDO_BRL'   , "
               'VLR_AVENCER_BRL'  'ZSDT0084' TEXT-089        '15'       'VLR_AVENCER_BRL'   , "
               'TOTAL_BRL'        'ZSDT0084' TEXT-090        '15'       'TOTAL_BRL'          , "
               'VLR_ADIANT_BRL'   'ZSDT0084' TEXT-091        '15'       'VLR_ADIANT_BRL'    , "
               'TOTAL_MOV_BRL'    'ZSDT0084' TEXT-092        '15'       'TOTAL_MOV_BRL'     , "
               'LIMITE_CREDITO'   'ZSDT0084' TEXT-093        '15'       'LIMITE_CREDITO'    , "
               'SALDO_DISP_BRL'   'ZSDT0084' TEXT-094        '15'       'SALDO_DISP_BRL'    , "
               'UTIL_LIMIT_BRL'   'ZSDT0084' TEXT-095        '15'       'UTIL_LIMIT_BRL'    , "
               'SDO_OV_EMIT'      'ZSDT0084' TEXT-096        '15'       'SDO_OV_EMIT'       , "
               'QTD_FATUR'        'ZSDT0084' TEXT-113        '15'       'SDO_OV_EMIT'. "Rubenilson - 24.09.24 - #145367


  it_zsdt0059_dec[] = it_zsdt0059[].
  it_zsdt0059_aux[] = it_zsdt0059[].
  it_zsdt0059_au2[] = it_zsdt0059[].
*  IT_ZSDT0059_SPR[] = IT_ZSDT0059[].

  SORT it_zsdt0059_dec BY cod_fp field c_decimais.
  SORT it_zsdt0059_aux BY cod_fp field.
  SORT it_zsdt0059_au2 BY cod_fp ocbot.
  SORT it_zsdt0059_spr BY cod_fp.
  SORT it_zsdt0056     BY cod_fp.

  DELETE it_zsdt0059_au2 WHERE ocbot NE 'X'.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0059_dec COMPARING cod_fp field c_decimais.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0059_aux COMPARING cod_fp field.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0059_au2 COMPARING cod_fp ocbot.

  LOOP AT it_zsdt0059 INTO wa_zsdt0059 .
    IF wa_zsdt0059-bezei EQ 'SPREAD' OR wa_zsdt0059-bezei EQ 'SPRED' OR wa_zsdt0059-bezei EQ 'PREMIO'.
      APPEND wa_zsdt0059 TO it_zsdt0059_spr.
    ENDIF.
  ENDLOOP.
  CLEAR wa_zsdt0059.

  SORT: it_zsdt0070 BY cod_fp field ,
        it_zsdt0059 BY cod_fp.
  LOOP AT it_zsdt0059_aux INTO wa_zsdt0059_aux.
    CLEAR: v_camp,
         v_text.
    CONCATENATE  wa_zsdt0059_aux-field+0(3) "'VAL'
                 wa_zsdt0059_aux-cod_fp
                 INTO v_camp.
    READ TABLE it_zsdt0056 INTO wa_zsdt0056 WITH KEY cod_fp =  wa_zsdt0059_aux-cod_fp BINARY SEARCH.
    "V_TEXT = WA_ZSDT0056-BEZEI.
    IF wa_zsdt0059_aux-field IS NOT INITIAL.
      CONCATENATE wa_zsdt0056-bezei '-' wa_zsdt0059_aux-field INTO  v_text.
    ELSE.
      v_text = wa_zsdt0056-bezei.
    ENDIF.
    LOOP AT it_zsdt0059_dec INTO wa_zsdt0059_dec WHERE cod_fp = wa_zsdt0059_aux-cod_fp
                                                 AND   field  = wa_zsdt0059_aux-field.
      " Pega o ultimo
    ENDLOOP.
    IF wa_zsdt0059_dec-c_decimais = '4'.
      PERFORM monta_fieldcat USING
             v_camp   'ZFIT0036' v_text            '15' 'TX_CAMBIO'.
    ELSE.
      PERFORM monta_fieldcat USING
             v_camp   'ZSDT0053' v_text            '15' 'VLRTOT'.
    ENDIF.

    PERFORM alv_preenche_cat USING:
            v_camp   v_text       '15'       ' '     ' '    'X' .

*############# INCLUIR DUAS COLUNAS DE MES DE FIXACAO MONAT PARA SPRED E PREMIO #####################
    READ TABLE it_zsdt0059_spr INTO wa_zsdt0059 WITH KEY bezei = wa_zsdt0059_aux-bezei.
    IF sy-subrc EQ 0.
      CLEAR: v_camp, v_text.

      CONCATENATE  'M-' wa_zsdt0059_aux-bezei+0(2)
                   wa_zsdt0059_aux-cod_fp+2(2)
                   INTO v_camp.

      CONCATENATE  wa_zsdt0059_aux-bezei 'MÊS'
                   INTO v_text SEPARATED BY space.

      PERFORM monta_fieldcat USING
             v_camp   'ZSDT0059' v_text            '02' 'MONAT'.

      PERFORM alv_preenche_cat USING:
              v_camp   v_text       '02'       ' '     ' '    ' '.

      DELETE it_zsdt0059_spr WHERE bezei = wa_zsdt0059_aux-bezei.

    ENDIF.
*############# INCLUIR DUAS COLUNAS DE MES DE FIXACAO MONAT PARA SPRED E PREMIO #####################

    READ TABLE it_zsdt0059_au2 INTO wa_zsdt0059 WITH KEY cod_fp = wa_zsdt0059_aux-cod_fp BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_zsdt0059-ocbot = 'X'.
        CLEAR: v_camp,
        v_text.
        CONCATENATE  'CBO'
                     wa_zsdt0059_aux-cod_fp
                     INTO v_camp.
        v_text = 'CBOT'.
        PERFORM monta_fieldcat USING
               v_camp   'ZSDT0059' v_text            '15' 'CBOT'.

        PERFORM alv_preenche_cat USING:
                v_camp   v_text       '15'       ' '     ' '    ' ' .
        DELETE it_zsdt0059_au2 WHERE cod_fp = wa_zsdt0059_aux-cod_fp .
      ENDIF.
    ENDIF.

    IF wa_zsdt0059_aux-field = 'QTDFIXADA'.
      CLEAR: v_camp, v_text.

      CONCATENATE  'DAT' wa_zsdt0059_aux-cod_fp INTO v_camp.
      CONCATENATE  wa_zsdt0059_aux-bezei+0(3) '-Dt Fix' INTO v_text.
      PERFORM monta_fieldcat USING v_camp 'ZFIT0036' v_text '10' 'DT_PGTO'.
      PERFORM alv_preenche_cat USING: v_camp v_text '10' '' '' 'X'.

      CONCATENATE  'MES' wa_zsdt0059_aux-cod_fp INTO v_camp.
      CONCATENATE  wa_zsdt0059_aux-bezei+0(3) '-Mês Fix' INTO v_text.
      PERFORM monta_fieldcat USING v_camp 'ZFIT0036' v_text '10' 'BUZEI'.
      PERFORM alv_preenche_cat USING: v_camp v_text '10' '' '' 'X'.

    ENDIF.
  ENDLOOP.

*  TABELA DINAMICA
  DATA: t_alvdata TYPE REF TO data.

* Monta tabela dinâmica
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      i_style_table   = ' '
*                     tab com as informações de campo
      it_fieldcatalog = lt_fcat_lvc
    IMPORTING
*                     retorna tab dinâmica com campos informados
      ep_table        = t_data.

*  free lt_fcat_lvc.

  IF <fs_data> IS ASSIGNED.
    UNASSIGN <fs_data>.
    UNASSIGN <wa_data>.
    UNASSIGN <fs_campo> .
  ENDIF.

* Carrega <fs_data> com a estrutura dos campos passados para o metodo
  ASSIGN t_data->* TO <fs_data>.
  CREATE DATA t_alvdata LIKE LINE OF <fs_data>.
  ASSIGN t_alvdata->* TO <wa_data>.

  REFRESH <fs_data>.

  SORT  it_zsdt0059 BY nro_sol_ov posnr.

  "DATA vformula2 TYPE zsdt0053-vlrtot.
  DATA: vformula2 TYPE  p DECIMALS 5,
        flag59(1),
        ndif      TYPE i,
        vdif(5).


  LOOP AT it_saida INTO wa_saida.
    IF wa_saida-data_venda IS NOT INITIAL.
      ndif  =  wa_saida-data_venda -  wa_saida-data_atual.
      vdif = ndif.
    ELSE.
      CLEAR vdif.
    ENDIF.

    wa_saida-qtd_fatur = wa_saida-zmeng - wa_saida-sd_ordem. " Rubenilson - 24.09.24 - #145367

* Campos fixos
    PERFORM f_carrega_dados USING:
        wa_saida-status51      'STATUS51'         ,
        wa_saida-nro_sol_ov    'NRO_SOL_OV'       ,
        wa_saida-data_atual    'DATA_ATUAL'       ,
        wa_saida-data_venda    'DATA_VENDA'       ,
        vdif                   'DIF'              ,
        wa_saida-data_atual69  'DATA_ATUAL69'     ,
        wa_saida-tp_venda      'TP_VENDA'         ,
        wa_saida-auart         'AUART'            ,
        wa_saida-vkorg         'VKORG'            ,
        wa_saida-vtweg         'VTWEG'            ,
        wa_saida-spart         'SPART'            ,
        wa_saida-vkbur         'VKBUR'            ,
        wa_saida-vkgrp         'VKGRP'            ,
        wa_saida-kunnr         'KUNNR'            ,
        wa_saida-name1_c       'NAME1_C'          ,
        wa_saida-ort01         'ORT01'            ,
        wa_saida-regio         'REGIO'            ,
        wa_saida-bstkd         'BSTKD'            ,
        wa_saida-inco1         'INCO1'            ,
        wa_saida-inco2         'INCO2'            ,
        wa_saida-vkaus         'VKAUS'            ,
        wa_saida-waerk         'WAERK'            ,
        wa_saida-taxa_curva    'TAXA_CURVA'       ,
        wa_saida-dtde_logist   'DTDE_LOGIST'      ,
        wa_saida-dtate_logist  'DTATE_LOGIST'     ,
        wa_saida-num_fixacao   'NUM_FIXACAO'      ,
        wa_saida-observacao    'OBSERVACAO'       ,
        wa_saida-coment_logistica 'COMENT_LOGISTICA',
        wa_saida-zlsch         'ZLSCH'            ,
        wa_saida-pgto_ant      'PGTO_ANT'         ,
        wa_saida-zterm         'ZTERM'            ,
        wa_saida-qte_venc      'QTE_VENC'         ,
        wa_saida-valdt52       'VALDT52'          ,
        wa_saida-hbkid         'HBKID'            ,
        wa_saida-status        'STATUS'           ,
        wa_saida-posnr         'POSNR'            ,
        wa_saida-fixacao       'FIXACAO'          ,
        wa_saida-matnr         'MATNR'            ,
        wa_saida-maktx         'MAKTX'            ,
        wa_saida-werks         'WERKS'            ,
        wa_saida-lgort         'LGORT'            ,
        wa_saida-charg         'CHARG'            ,
        wa_saida-zmeng         'ZMENG'            ,
        wa_saida-zieme         'ZIEME'            ,
        wa_saida-qt_dev        'QT_DEV'           ,
        wa_saida-qt_compl      'QT_COMPL'         ,
        wa_saida-qt_ord        'QT_ORD'           ,
        wa_saida-dmbtr         'DMBTR'            ,
        wa_saida-pmein         'PMEIN'            ,
        wa_saida-vlrtot        'VLRTOT'           ,
        wa_saida-valdt         'VALDT'            ,
        wa_saida-ponto_c       'PONTO_C'          ,
        wa_saida-terminal      'TERMINAL'         ,
        wa_saida-brgew         'BRGEW'            ,
        wa_saida-volum         'VOLUM'            ,
        wa_saida-voleh         'VOLEH'            ,
        wa_saida-kursf         'KURSF'            ,

        wa_saida-contrato          'CONTRATO'       ,
        wa_saida-classificacao     'CLASSIFICACAO'  ,
        wa_saida-navio             'NAVIO'          ,
        wa_saida-porto             'PORTO'          ,
        wa_saida-p_porto           'P_PORTO'        ,
        wa_saida-vlt_porto         'VLT_PORTO'      ,
        wa_saida-instrucao         'INSTRUCAO'      ,

        wa_saida-vbeln         'VBELN'            ,
        wa_saida-valdt_ant     'VALDT_ANT'        ,
        wa_saida-dmbtr_ant     'DMBTR_ANT'        ,
        wa_saida-adiant        'ADIANT'           ,
        wa_saida-kursf54       'KURSF54'          ,
        wa_saida-vlr_real      'VLR_REAL'         ,
        wa_saida-posnr63       'POSNR63'          ,
        wa_saida-valdt63       'VALDT63'          ,
        wa_saida-bukrs63       'BUKRS63'          ,
        wa_saida-dmbtr63       'DMBTR63'          ,
        wa_saida-waers63       'WAERS63'          ,
        wa_saida-lifnr63       'LIFNR63'          ,
        wa_saida-name163       'NAME163'          ,
        wa_saida-banks63       'BANKS63'          ,
        wa_saida-bankl63       'BANKL63'          ,
        wa_saida-swift63       'SWIFT63'          ,
        wa_saida-bankn63       'BANKN63'          ,
        wa_saida-adiant63      'ADIANT63'         ,
        wa_saida-correto       'CORRETO'          ,
        wa_saida-name1_f       'NAME1_F'          ,
        wa_saida-sd_ordem      'SD_ORDEM'         ,
        wa_saida-posnr66       'POSNR66'          ,
        wa_saida-matnr66       'MATNR66'          ,
        wa_saida-werks66       'WERKS66'          ,
        wa_saida-lgort66       'LGORT66'          ,
        wa_saida-charg66       'CHARG66'          ,
        wa_saida-zmeng66       'ZMENG66'          ,
        wa_saida-volum66       'VOLUM66'          ,
        wa_saida-dmbtr66       'DMBTR66'          ,
        wa_saida-libra_to      'LIBRA_TO'         ,
        wa_saida-usd_to        'USD_TO'           ,

        wa_saida-vlrtot66      'VLRTOT66'         ,
        wa_saida-vlr_tot_frm_usd 'VLR_TOT_FRM_USD',

        wa_saida-kunnr66       'KUNNR66'          ,
        wa_saida-instrucao66   'INSTRUCAO66'      ,
        wa_saida-terminal66    'TERMINAL66'       ,
        wa_saida-lentrega66    'LENTREGA66'       ,
        wa_saida-inco166       'INCO166'          ,
        wa_saida-inco266       'INCO266'          ,
        wa_saida-vbeln66       'VBELN66'          ,
        wa_saida-vlr_vencido_brl 'VLR_VENCIDO_BRL',
        wa_saida-vlr_avencer_brl 'VLR_AVENCER_BRL',
        wa_saida-total_brl       'TOTAL_BRL',
        wa_saida-vlr_adiant_brl  'VLR_ADIANT_BRL',
        wa_saida-total_mov_brl   'TOTAL_MOV_BRL',
        wa_saida-limite_credito  'LIMITE_CREDITO',
        wa_saida-saldo_disp_brl  'SALDO_DISP_BRL',
        wa_saida-util_limit_brl  'UTIL_LIMIT_BRL',
        wa_saida-sdo_ov_emit     'SDO_OV_EMIT',
        wa_saida-qtd_fatur       'QTD_FATUR'." Rubenilson - 24.09.24 - #145367


    REFRESH it_color.

    CLEAR flag59.
    SORT  it_zsdt0057 BY tp_venda.

    READ TABLE it_zsdt0057 INTO wa_zsdt0057 WITH KEY tp_venda = wa_saida-tp_venda BINARY SEARCH.
    IF wa_zsdt0057-param_espec NE 'M' .
      LOOP AT it_zsdt0059 INTO wa_zsdt0059 WHERE nro_sol_ov = wa_saida-nro_sol_ov
                                           AND   posnr      = wa_saida-posnr.
        flag59 = 'X'.
        CONDENSE wa_zsdt0059-formula2 NO-GAPS.
        vformula2 = wa_zsdt0059-formula2.
        CLEAR v_camp.
        CONCATENATE  wa_zsdt0059-field+0(3) "'VAL'
                   wa_zsdt0059-cod_fp
                   INTO v_camp.
        PERFORM f_carrega_dados USING:
            vformula2     v_camp.

        IF wa_zsdt0059-field = 'QTDFIXADA' .
          CONCATENATE  'DAT' wa_zsdt0059-cod_fp INTO v_camp.
          PERFORM f_carrega_dados USING:
          wa_zsdt0059-valdt     v_camp.

          CONCATENATE  'MES' wa_zsdt0059-cod_fp INTO v_camp.
          PERFORM f_carrega_dados USING:
           wa_zsdt0059-monat     v_camp.
        ENDIF.
        IF wa_zsdt0059-bezei EQ 'SPREAD' OR wa_zsdt0059-bezei EQ 'PREMIO' OR wa_zsdt0059-bezei EQ 'SPRED'.
          CLEAR v_camp.
          CONCATENATE  'M-' wa_zsdt0059-bezei+0(2)
                   wa_zsdt0059-cod_fp+2(2)
                   INTO v_camp.

          PERFORM f_carrega_dados USING:
                wa_zsdt0059-monat     v_camp.
        ENDIF.

        IF wa_zsdt0059-ocbot = 'X'.
          CLEAR v_camp.
          CONCATENATE 'CBO'
                      wa_zsdt0059-cod_fp
                   INTO v_camp.
          PERFORM f_carrega_dados USING:
              wa_zsdt0059-cbot     v_camp.
        ENDIF.

      ENDLOOP.
    ENDIF.
    IF flag59 NE 'X'.
      LOOP AT it_zsdt0059 INTO wa_zsdt0059 WHERE nro_sol_ov = wa_saida-nro_sol_ov
                                       AND   posnr      = wa_saida-fixacao.
        flag59 = 'X'.
        CONDENSE wa_zsdt0059-formula2 NO-GAPS.
        vformula2 = wa_zsdt0059-formula2.
        CLEAR v_camp.
        CONCATENATE  wa_zsdt0059-field+0(3) "'VAL'
                   wa_zsdt0059-cod_fp
                   INTO v_camp.

        v_camp1 = 'NRO_SOL_OV'.
        v_camp2 = 'FIXACAO'.
        v_camp3 = 'POSNR'.
        " Pega conteudo anterior
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_saida-nro_sol_ov
          IMPORTING
            output = v_nro_sol_ov.

        READ TABLE <fs_data> ASSIGNING <wa_data2> WITH KEY (v_camp1) = v_nro_sol_ov
                                                          (v_camp2) = wa_saida-fixacao
                                                          (v_camp3) = wa_saida-posnr.
        IF  sy-subrc = 0.
          ASSIGN COMPONENT v_camp  OF STRUCTURE <wa_data2> TO <fs_campo>.
          ADD <fs_campo> TO vformula2.
        ENDIF.

        PERFORM f_carrega_dados USING:
            vformula2     v_camp.

        IF wa_zsdt0059-field = 'QTDFIXADA' .
          CONCATENATE  'DAT' wa_zsdt0059-cod_fp INTO v_camp.
          PERFORM f_carrega_dados USING:
          wa_zsdt0059-valdt     v_camp.

          CONCATENATE  'MES' wa_zsdt0059-cod_fp INTO v_camp.
          PERFORM f_carrega_dados USING:
           wa_zsdt0059-monat     v_camp.
        ENDIF.

        IF wa_zsdt0059-bezei EQ 'SPREAD' OR wa_zsdt0059-bezei EQ 'PREMIO' OR wa_zsdt0059-bezei EQ 'SPRED'.
          CLEAR v_camp.
          CONCATENATE  'M-' wa_zsdt0059-bezei+0(2)
                   wa_zsdt0059-cod_fp+2(2)
                   INTO v_camp.

          PERFORM f_carrega_dados USING:
                wa_zsdt0059-monat     v_camp.
        ENDIF.


        IF wa_zsdt0059-ocbot = 'X'.
          CLEAR v_camp.
          CONCATENATE 'CBO'
                      wa_zsdt0059-cod_fp
                   INTO v_camp.
          PERFORM f_carrega_dados USING:
              wa_zsdt0059-cbot     v_camp.
        ENDIF.

      ENDLOOP.
    ENDIF.
    IF flag59 NE 'X'.
      LOOP AT it_zsdt0059 INTO wa_zsdt0059 WHERE nro_sol_ov = wa_saida-nro_sol_ov.
        CONDENSE wa_zsdt0059-formula2 NO-GAPS.
        vformula2 = wa_zsdt0059-formula2.
        CLEAR v_camp.
        CONCATENATE  wa_zsdt0059-field+0(3) "'VAL'
                   wa_zsdt0059-cod_fp
                   INTO v_camp.
        PERFORM f_carrega_dados USING:
            vformula2     v_camp.

        IF wa_zsdt0059-field = 'QTDFIXADA' .
          CONCATENATE  'DAT' wa_zsdt0059-cod_fp INTO v_camp.
          PERFORM f_carrega_dados USING:
          wa_zsdt0059-valdt     v_camp.

          CONCATENATE  'MES' wa_zsdt0059-cod_fp INTO v_camp.
          PERFORM f_carrega_dados USING:
           wa_zsdt0059-monat     v_camp.
        ENDIF.

        IF wa_zsdt0059-bezei EQ 'SPREAD' OR wa_zsdt0059-bezei EQ 'PREMIO' OR wa_zsdt0059-bezei EQ 'SPRED'.
          CLEAR v_camp.
          CONCATENATE  'M-' wa_zsdt0059-bezei+0(2)
                   wa_zsdt0059-cod_fp+2(2)
                   INTO v_camp.

          PERFORM f_carrega_dados USING:
                wa_zsdt0059-monat     v_camp.
        ENDIF.


        IF wa_zsdt0059-ocbot = 'X'.
          CLEAR v_camp.
          CONCATENATE 'CBO'
                      wa_zsdt0059-cod_fp
                   INTO v_camp.
          PERFORM f_carrega_dados USING:
              wa_zsdt0059-cbot     v_camp.
        ENDIF.

      ENDLOOP.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_saida-nro_sol_ov
      IMPORTING
        output = wa_saida-nro_sol_ov.

    PERFORM f_carrega_dados USING:
        wa_saida-nro_sol_ov    'NRO_SOL_OV'       .

*
*    PERFORM F_CARREGA_DADOS USING:
*              IT_COLOR[]      'COLOR_CELL'.

*  Inclui dados da work-área dinâmica na tabela dinâmica
    PERFORM f_carrega_alv USING <fs_data>
                                <wa_data>.
    CLEAR           <wa_data>.
  ENDLOOP.

ENDFORM.                    " F_AL

*&---------------------------------------------------------------------*
*&      Form  f_carrega_alv
*&---------------------------------------------------------------------*
*      -->P_TAB  tabela
*      -->P_WA   work-área
*----------------------------------------------------------------------*
FORM f_carrega_alv USING    p_tab TYPE table
                            p_wa.
*  Inclui dados da work-área dinâmica na tabela dinâmica
  APPEND p_wa TO p_tab.

ENDFORM.                    " f_carrega_alv
*&---------------------------------------------------------------------*
*&      Form  f_imprime_dados
*&---------------------------------------------------------------------*
FORM f_imprime_dados_alv USING p_itab_output TYPE table.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = v_report
      is_layout                = gd_layout
      is_variant               = gs_variant_c
      i_callback_pf_status_set = 'PF_STATUS_001'
      i_callback_user_command  = 'USER_COMMAND'
      it_fieldcat              = it_fcat[]
      it_sort                  = t_sort[]
      i_save                   = 'X'
      it_events                = events
      is_print                 = t_print
*     IS_VARIANT               = VG_VARIANT
    TABLES
      t_outtab                 = p_itab_output.

ENDFORM.                    " f_imprime_dados

*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PF_TAB     text
*----------------------------------------------------------------------*
FORM pf_status_001 USING pf_tab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'.
ENDFORM.                    "z_pf_status_001

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UCOMM      text
*      -->SELFIELD   text
*----------------------------------------------------------------------*

FORM user_command USING r_ucomm     LIKE sy-ucomm           "#EC CALLED
                        rs_selfield TYPE slis_selfield.

  DATA: lv_file     TYPE string.

  CASE r_ucomm.
    WHEN '&IC1'.
      "READ TABLE <FS_DATA> INTO <WA_DATA> INDEX RS_SELFIELD-TABINDEX.
      IF sy-subrc EQ 0.
        IF rs_selfield-fieldname = 'VBELN'
        AND rs_selfield-value IS NOT INITIAL.
          SET PARAMETER ID 'AUN' FIELD rs_selfield-value.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ELSEIF rs_selfield-fieldname = 'VBELN66'
        AND rs_selfield-value IS NOT INITIAL.
          SET PARAMETER ID 'AUN' FIELD rs_selfield-value.
          CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
    WHEN 'EXCEL'.
      PERFORM export_to_xlsx USING <fs_data>.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.  "User_command

*----------------------------------------------------------------------*
*       Form  f_monta_top_of_page
*----------------------------------------------------------------------*
FORM f_monta_top_of_page USING p_list_top_of_page TYPE
                               slis_t_listheader.

  DATA: t_header   TYPE slis_listheader,
        v_data(10) TYPE c.

  t_header-typ  = 'H'.
  t_header-info = 'Solicitações de Venda '(t01).
  APPEND t_header TO p_list_top_of_page.
  CLEAR t_header.
  WRITE sy-datum USING EDIT  MASK '__.__.____' TO v_data.
  CONCATENATE 'Data : '(023)  v_data INTO t_header-key SEPARATED BY
  space.
  t_header-typ  = 'S'.
  APPEND t_header TO p_list_top_of_page.

ENDFORM.                    " f_monta_top_of_page


*----------------------------------------------------------------------*
*       Form  f_carrega_dados
*----------------------------------------------------------------------*
*   Carrega dados para work-área dinâmica
*----------------------------------------------------------------------*
*      -->P_valor   valor
*      -->P_campo   campo
*----------------------------------------------------------------------*
FORM f_carrega_dados USING    p_valor
                              p_campo.
*Aponta <fs_campo> para <wa_data>-campo montado
  ASSIGN COMPONENT p_campo  OF STRUCTURE <wa_data> TO <fs_campo>.

*Move valor para <fs_campo> que esta apontando p/<wa_data>-campo montado
  MOVE p_valor TO <fs_campo>.


ENDFORM.                    " f_carrega_dados
*&---------------------------------------------------------------------*
*&      Form  MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FIELD      text
*      -->P_TABREF     text
*      -->P_TEXT       text
*      -->P_OUT        text
*      -->P_REF_FIELD  text
*----------------------------------------------------------------------*
FORM monta_fieldcat USING p_field
*                          p_tab
                          p_tabref
                          p_text
                          p_out
                          p_ref_field.
**** Se o programa for um ALV, pode aproveitar p/ carregar fieldcat com
* os atributos necessários, caso não se trate de um ALV basta informar o
* campo de referencia, a tabela de referência, o campo  e a tabela.

  CLEAR: s_fieldcat, wa_fcat_lvc.
  wa_fcat_lvc-fieldname   = s_fieldcat-fieldname   = p_field.
  wa_fcat_lvc-tabname     = s_fieldcat-tabname     = '<FS_DATA>'.
  wa_fcat_lvc-ref_table   = s_fieldcat-ref_tabname = p_tabref.
  wa_fcat_lvc-seltext     = s_fieldcat-seltext_l   = p_text.

  s_fieldcat-seltext_m    = p_text.
  s_fieldcat-seltext_l    = p_text.
  s_fieldcat-seltext_s    = p_text.

  wa_fcat_lvc-outputlen   = s_fieldcat-outputlen   = p_out.
  wa_fcat_lvc-ref_field   = s_fieldcat-ref_fieldname   = p_ref_field.

* carrega fieldcat do alv
  APPEND s_fieldcat.

*inclui dados da work-área p/ tabela sem cab.
  APPEND wa_fcat_lvc TO lt_fcat_lvc.

ENDFORM.                    " monta_fieldcat
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0649   text
*      -->P_TEXT_003  text
*      -->P_0651   text
*      -->P_0652   text
*      -->P_0653   text
*      -->P_0654   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo  TYPE c
                               p_desc   TYPE c
                               p_tam    TYPE c
                               p_hot    TYPE c
                               p_zero   TYPE c
                               p_soma   TYPE c.


  DATA: wl_fcat TYPE ty_estrutura.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-seltext_s = p_desc.
  wl_fcat-seltext_m = p_desc.
  wl_fcat-seltext_l = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-outputlen = p_tam.

  IF p_campo NE 'PRE0001'.
    wl_fcat-do_sum    = p_soma.
  ENDIF.

  IF p_campo = 'ICON'.
    wl_fcat-icon      = 'X'.
  ENDIF.


  APPEND wl_fcat TO it_fcat.
ENDFORM.                    " ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPO_53
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM limpa_campo_53.

  CLEAR: wa_saida-zterm,wa_saida-qte_venc,wa_saida-fixacao,wa_saida-status,wa_saida-posnr,wa_saida-maktx,wa_saida-matnr,
         wa_saida-werks,wa_saida-lgort,wa_saida-charg,wa_saida-zmeng,wa_saida-zieme,wa_saida-dmbtr,wa_saida-pmein,wa_saida-vlrtot,
         wa_saida-valdt,wa_saida-ponto_c,wa_saida-terminal,wa_saida-brgew,wa_saida-volum,wa_saida-voleh,wa_saida-kursf,
         wa_saida-qt_ord,wa_saida-vbeln,wa_saida-kunnr,wa_saida-name1_c,wa_saida-sd_ordem,wa_saida-qt_dev,
         wa_saida-zmeng,wa_saida-dmbtr, wa_saida-vlrtot,wa_saida-sd_ordem,wa_saida-brgew,wa_saida-volum,wa_saida-kursf.     "zera nivel
ENDFORM.                    "LIMPA_CAMPO_53
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPO_63_66
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM limpa_campo_63_66.

  CLEAR:  wa_saida-status, wa_saida-posnr, wa_saida-matnr, wa_saida-werks, wa_saida-lgort, wa_saida-charg, wa_saida-zmeng, wa_saida-zieme,
          wa_saida-dmbtr, wa_saida-pmein, wa_saida-vlrtot, wa_saida-valdt, wa_saida-vbeln, wa_saida-sd_ordem, wa_saida-brgew,
          wa_saida-volum, wa_saida-kursf, wa_saida-valdt_ant, wa_saida-dmbtr_ant, wa_saida-adiant, wa_saida-posnr63, wa_saida-valdt63,
          wa_saida-bukrs63, wa_saida-dmbtr63, wa_saida-waers63, wa_saida-lifnr63, wa_saida-banks63, wa_saida-bankl63, wa_saida-swift63,
          wa_saida-bankn63, wa_saida-adiant63, wa_saida-posnr63, wa_saida-valdt63, wa_saida-bukrs63, wa_saida-dmbtr63, wa_saida-waers63,
          wa_saida-lifnr63, wa_saida-banks63, wa_saida-bankl63, wa_saida-swift63, wa_saida-bankn63, wa_saida-adiant63, wa_saida-matnr66,
          wa_saida-werks66, wa_saida-lgort66, wa_saida-charg66, wa_saida-zmeng66, wa_saida-volum66, wa_saida-dmbtr66, wa_saida-vlrtot66,
          wa_saida-kunnr66, wa_saida-instrucao66, wa_saida-terminal66, wa_saida-lentrega66, wa_saida-inco166, wa_saida-inco266,
          wa_saida-vbeln66.

ENDFORM.                    " LIMPA53
*&---------------------------------------------------------------------*
*&      Form  F_ATRIBUIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_atribuir_dados .
  MOVE it_saida_aux TO it_saida.
ENDFORM.                    " F_ATRIBUIR_DADOS
*&---------------------------------------------------------------------*
*&      Form  EXPORT_TO_XLSX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_DATA>  text
*----------------------------------------------------------------------*
FORM export_to_xlsx USING t_export_excel TYPE table.
  DATA valor(30).
  DATA: p_local   TYPE string,
        path(250).
  DATA: t_alvdata TYPE REF TO data.

  DATA:
    BEGIN OF t_fieldnames OCCURS 0,
      name TYPE char20,
    END OF t_fieldnames.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
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

    CONCATENATE path '.XLS' INTO p_local.

    LOOP AT lt_fcat_lvc INTO wa_fcat_lvc.
      t_fieldnames-name = wa_fcat_lvc-seltext.
      APPEND t_fieldnames.
    ENDLOOP.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename   = p_local
        filetype   = 'DAT'
        codepage   = '8404'
      TABLES
        data_tab   = t_export_excel
        fieldnames = t_fieldnames.
  ENDIF.

ENDFORM.                    " EXPORT_TO_XLSX
