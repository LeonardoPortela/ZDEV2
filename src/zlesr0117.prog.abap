*&---------------------------------------------------------------------*
*& Report  ZLESR0117
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zlesr0117.

TABLES: zlest0155.
TYPE-POOLS: slis, kkblo.

*======================================================================*
* TYPES
*======================================================================*
TYPES:
  BEGIN OF ty_zlest0155,
    id_ordem          TYPE zlest0155-id_ordem,
    nr_ordem          TYPE zlest0155-nr_ordem,
    vbeln             TYPE zlest0155-vbeln,
    id_bukrs          TYPE zlest0155-id_bukrs,
    id_branch         TYPE zlest0155-id_branch,
    id_bukrs_ag       TYPE zlest0155-id_bukrs_ag,
    id_branch_ag      TYPE zlest0155-id_branch_ag,
    id_local_coleta   TYPE zlest0155-id_local_coleta,
    id_local_destino  TYPE zlest0155-id_local_destino,
    id_local_descarga TYPE zlest0155-id_local_descarga,
    id_produto        TYPE zlest0155-id_produto,
    id_motorista      TYPE zlest0155-id_motorista,
    ds_placa_trator   TYPE zlest0155-ds_placa_trator,
    nr_peso_alvo      TYPE zlest0155-nr_peso_alvo,
    kbetr             TYPE zlest0155-kbetr,
    vlr_frete_neg     TYPE zlest0155-vlr_frete_neg,
    motivo            TYPE zlest0155-motivo,
    bname             TYPE zlest0155-bname,
    dt_mod            TYPE zlest0155-dt_mod,
    hr_mod            TYPE zlest0155-hr_mod,
    status_aprov      TYPE zlest0155-status_aprov,
    vlr_frete_tk11    TYPE zlest0155-vlr_frete_tk11,
    ch_referencia     TYPE zlest0155-ch_referencia,
  END OF ty_zlest0155,

  BEGIN OF ty_vbpa,   "PARCEIROS ORDEM DE VENDA"
    vbeln TYPE vbpa-vbeln,  "Nº documento de vendas e distribuição
    parvw TYPE vbpa-parvw,  "Função do parceiro
    kunnr TYPE vbpa-kunnr,  "Nº cliente
    lifnr TYPE vbpa-lifnr,  "Nº conta do fornecedor
  END OF ty_vbpa,

  BEGIN OF ty_lfa1,   "FORNECEDOR"
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
    lzone TYPE lfa1-lzone,
  END OF ty_lfa1,

  BEGIN OF ty_kna1,   "CLIENTE"
    name1 TYPE kna1-name1,
    lzone TYPE kna1-lzone,
    kunnr TYPE kna1-kunnr,
  END OF ty_kna1,

  BEGIN OF ty_t001w,  "FILIAL"
    werks TYPE t001w-werks,
    name1 TYPE t001w-name1,
  END OF ty_t001w,

  BEGIN OF ty_makt,   "MATERIAL"
    matnr TYPE makt-matnr,
    spras TYPE makt-spras,
    maktx TYPE makt-maktx,
  END OF ty_makt,

  BEGIN OF ty_vbak,   "ORDEM DE VENDA - CABEÇALHO"
    vbeln TYPE vbak-vbeln,
    auart TYPE vbak-auart,
  END OF ty_vbak,

  BEGIN OF ty_tvak,   "ORDEM DE VENDA - TIPO"
    auart TYPE tvak-auart,
    bezob TYPE tvak-bezob,
  END OF ty_tvak,

  BEGIN OF ty_select,
    v_vbeln  TYPE vbpa-vbeln,
    v_parvw  TYPE vbpa-parvw,
    v_kunnr  TYPE vbpa-kunnr,
    v_lifnr  TYPE vbpa-lifnr,
    l_lifnr  TYPE lfa1-lifnr,
    l_name1  TYPE lfa1-name1,
    l_lzone  TYPE lfa1-lzone,
    k_kunnr  TYPE kna1-kunnr,
    k_name1  TYPE kna1-name1,
    k_lzone  TYPE kna1-lzone,
    t_werks  TYPE t001w-werks,
    t_name1  TYPE t001w-name1,
    m_matnr  TYPE makt-matnr,
    m_spras  TYPE makt-spras,
    m_maktx  TYPE makt-maktx,
    vb_vbeln TYPE vbak-vbeln,
    vb_auart TYPE vbak-auart,
    t_auart  TYPE tvak-auart,
    t_bezei  TYPE tvak-bezob,
  END OF ty_select,

  BEGIN OF ty_zlest0157,
    ordem      TYPE zlest0157-ordem,
    id_ordem   TYPE zlest0157-id_ordem,
    nivel      TYPE zlest0157-nivel,
    aprovador  TYPE zlest0157-aprovador,
    data_atual TYPE zlest0157-data_atual,
    hora_atual TYPE zlest0157-hora_atual,
  END OF ty_zlest0157,


  BEGIN OF ty_saida,
    status_aprov       TYPE c LENGTH 4,
    filial             TYPE t001w-werks, "TYPE c LENGTH 45,
    desc_filial        TYPE t001w-name1,
    ordem_id           TYPE zlest0155-id_ordem,
    ordem_carregamento TYPE zlest0155-nr_ordem,
    ordem_venda        TYPE zlest0155-vbeln,
    tipo_ordem         TYPE c LENGTH 45,
    emissor            TYPE c LENGTH 60,
* ---> S4 Migration - 19/06/2023 - MA
*    produto            TYPE c LENGTH 60,
    produto            TYPE c LENGTH 90,
* <--- S4 Migration - 19/06/2023 - MA
    ponto_coleta       TYPE c LENGTH 60,
    destino            TYPE c LENGTH 60,
    motorista          TYPE c LENGTH 60,
    placa              TYPE zlest0155-ds_placa_trator,
    peso_estimado      TYPE zlest0155-nr_peso_alvo,
    valor_frete        TYPE zlest0155-kbetr,
    valor_f_negociado  TYPE zlest0155-vlr_frete_neg,
    motivo_alteracao   TYPE zlest0155-motivo,
    aprovador          TYPE zlest0156-aprovador,
    data               TYPE zlest0155-dt_mod,
    hora               TYPE zlest0155-hr_mod,
    usuario            TYPE zlest0155-bname,
    status_texto       TYPE c LENGTH 20,
    dt_aprov           TYPE zlest0157-data_atual,
    hr_aprov           TYPE zlest0157-hora_atual,
    vlr_frete_tk11     TYPE zlest0155-vlr_frete_tk11,
    ch_referencia      TYPE zsdt0001-ch_referencia,
    nr_romaneio        TYPE zsdt0001-nr_romaneio,
    doc_rem            TYPE zsdt0001-doc_rem,
    fatura_prod        TYPE zsdt0001-fatura_prod,
    doc_transp         TYPE zsdt0001-doc_transp,
    fknum              TYPE zsdt0001-fknum,
    dt_movimento       TYPE zsdt0001-dt_movimento,
  END OF ty_saida,

  BEGIN OF ty_zsdt0001,
    ch_referencia TYPE zsdt0001-ch_referencia,
    nr_romaneio   TYPE zsdt0001-nr_romaneio,
    doc_rem       TYPE zsdt0001-doc_rem,
    fatura_prod   TYPE zsdt0001-fatura_prod,
    doc_transp    TYPE zsdt0001-doc_transp,
    fknum         TYPE zsdt0001-fknum,
    dt_movimento  TYPE zsdt0001-dt_movimento,

  END OF ty_zsdt0001.


*======================================================================*
* WORKAREA
*======================================================================*
DATA:
  wa_zlest0155 TYPE ty_zlest0155,
  wa_zlest0157 TYPE ty_zlest0157,
  wa_vbpa      TYPE ty_vbpa,
  wa_lfa1      TYPE ty_lfa1,
  wa_kna1      TYPE ty_kna1,
  wa_t001w     TYPE ty_t001w,
  wa_makt      TYPE ty_makt,
  wa_vbak      TYPE ty_vbak,
  wa_tvak      TYPE ty_tvak,
  wa_select    TYPE ty_select,
  wa_saida     TYPE ty_saida,
  wa_zsdt0001  TYPE ty_zsdt0001.

*======================================================================*
* TABELAS INTERNAS
*======================================================================*
DATA:
  it_zlest0155 TYPE TABLE OF ty_zlest0155,
  it_vbpa      TYPE TABLE OF ty_vbpa,
  it_lfa1      TYPE TABLE OF ty_lfa1,
  it_kna1      TYPE TABLE OF ty_kna1,
  it_t001w     TYPE TABLE OF ty_t001w,
  it_makt      TYPE TABLE OF ty_makt,
  it_vbak      TYPE TABLE OF ty_vbak,
  it_tvak      TYPE TABLE OF ty_tvak,
  it_select    TYPE TABLE OF ty_select,
  it_saida     TYPE TABLE OF ty_saida,
  it_zlest0157 TYPE TABLE OF ty_zlest0157,
  it_zsdt0001  TYPE TABLE OF ty_zsdt0001.

*======================================================================*
* VARIÁVEIS
*======================================================================*
DATA:
  xfil      TYPE t001w-name1,
  xcclr     TYPE vbpa-kunnr,
  xlr       TYPE kna1-name1,
  xccag     TYPE vbpa-kunnr,
  xag       TYPE kna1-name1,
  xcfpc     TYPE vbpa-lifnr,
  xpc       TYPE lfa1-name1,
  xcfag     TYPE vbpa-lifnr,
  xfag      TYPE lfa1-name1,
  xmot      TYPE lfa1-name1,
  xdescid   TYPE kna1-kunnr,
  xdescname TYPE kna1-name1.

DATA: ok-code         TYPE sy-ucomm.

**======================================================================*
* ALV
*======================================================================*
DATA: obj_container    TYPE REF TO cl_gui_custom_container,
      obj_alv          TYPE REF TO cl_gui_alv_grid,
      dg_splitter_1    TYPE REF TO cl_gui_splitter_container,
      dg_parent_1      TYPE REF TO cl_gui_container,
      dg_splitter_2    TYPE REF TO cl_gui_splitter_container,
      dg_parent_2      TYPE REF TO cl_gui_container,
      dg_parent_2a     TYPE REF TO cl_gui_container,
      dg_parent_alv    TYPE REF TO cl_gui_container,
      picture          TYPE REF TO cl_gui_picture,
      dg_dyndoc_id     TYPE REF TO cl_dd_document,
      table_element    TYPE REF TO cl_dd_table_element,
      column           TYPE REF TO cl_dd_area,
      table_element2   TYPE REF TO cl_dd_table_element,
      column_1         TYPE REF TO cl_dd_area,
      dg_html_cntrl    TYPE REF TO cl_gui_html_viewer,
      it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode,
      gs_layout        TYPE lvc_s_layo,
      gs_variant       TYPE disvariant,
      it_fieldcatalog  TYPE lvc_t_fcat,
      wa_fieldcatalog  TYPE lvc_s_fcat,
      it_sort          TYPE lvc_t_sort,
      ls_stable        TYPE lvc_s_stbl,
      c_x              TYPE c VALUE 'X',
      w_sort           TYPE lvc_t_sort WITH HEADER LINE.

DATA: gt_fieldcatalog TYPE lvc_t_fcat,
      gw_fieldcatalog TYPE lvc_s_fcat.

DATA: gob_gui_alv_grid TYPE REF TO cl_gui_alv_grid.


*======================================================================*
* TELA DE SELEÇÃO
*======================================================================*

SELECTION-SCREEN: BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-000.
  SELECT-OPTIONS: filial         FOR zlest0155-id_branch,
                  ordemcar       FOR zlest0155-nr_ordem,
                  ordemven       FOR zlest0155-vbeln,
                  datasoli       FOR zlest0155-dt_mod,
                  solicita       FOR zlest0155-bname,
                  status         FOR zlest0155-status_aprov.
SELECTION-SCREEN: END OF BLOCK b0.

START-OF-SELECTION.

  PERFORM: f_seleciona_dados,
           f_organiza_dados,
           criar_alv.

  "CALL SCREEN 0100.

*======================================================================*
* FORM F_SELECIONA_DADOS
*======================================================================*
FORM f_seleciona_dados.

  SELECT * FROM zlest0155
    INTO CORRESPONDING FIELDS OF TABLE it_zlest0155
    WHERE nr_ordem      IN ordemcar
    AND   id_branch     IN filial
    AND   vbeln         IN ordemven
    AND   dt_mod        IN datasoli
    AND   bname         IN solicita
    AND   status_aprov  IN status.


  IF ( it_zlest0155 IS INITIAL ).
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não existem registros para os parâmetros informados!'.
    STOP.

  ELSE.

    SELECT lifnr               "Seleção Nome do Fornecedor
           name1
           lzone FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_zlest0155
    WHERE lifnr = it_zlest0155-id_local_coleta.

    SELECT lifnr               "Seleção Nome do Cliente
           name1 FROM lfa1
    APPENDING TABLE it_lfa1
    FOR ALL ENTRIES IN it_zlest0155
    WHERE lifnr = it_zlest0155-id_motorista.

    SELECT name1               "Seleção Nome do Cliente
           lzone
           kunnr FROM kna1
    INTO TABLE it_kna1
    FOR ALL ENTRIES IN it_zlest0155
    WHERE kunnr = it_zlest0155-id_local_destino.

    SELECT name1               "Seleção Nome do Cliente / Descarga
           lzone
           kunnr
      FROM kna1
      APPENDING TABLE it_kna1
      FOR ALL ENTRIES IN it_zlest0155
      WHERE kunnr = it_zlest0155-id_local_descarga.

    SELECT werks               "Seleção Nome da Filial
           name1 FROM t001w
    INTO TABLE it_t001w
    FOR ALL ENTRIES IN it_zlest0155
    WHERE werks = it_zlest0155-id_branch_ag.

    SELECT werks               "Seleção Nome da Filial AG
           name1 FROM t001w
    APPENDING TABLE it_t001w
    FOR ALL ENTRIES IN it_zlest0155
    WHERE werks = it_zlest0155-id_branch.

    SELECT matnr               "Seleção Nome do Material
           spras
           maktx FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_zlest0155
    WHERE matnr = it_zlest0155-id_produto
    AND spras = 'PT'.

    SELECT ch_referencia
           nr_romaneio
           doc_rem
           fatura_prod
           doc_transp
           fknum
           dt_movimento
      FROM zsdt0001
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0001
      FOR ALL ENTRIES IN  it_zlest0155
      WHERE ch_referencia = it_zlest0155-ch_referencia.


  ENDIF.
ENDFORM.

*======================================================================*
* FORM F_ORGANIZA_DADOS
*======================================================================*

FORM f_organiza_dados.

  CLEAR wa_saida.

  LOOP AT it_zlest0155 INTO wa_zlest0155.

    CLEAR wa_saida. CLEAR wa_zlest0157. CLEAR wa_t001w.
    CLEAR wa_vbak.  CLEAR wa_tvak.      CLEAR wa_lfa1.
    CLEAR wa_makt.  CLEAR wa_kna1.      CLEAR wa_zsdt0001.

    wa_saida-ordem_id            = wa_zlest0155-id_ordem.
    wa_saida-ordem_carregamento  = wa_zlest0155-nr_ordem.
    wa_saida-ordem_venda         = wa_zlest0155-vbeln.
    wa_saida-placa               = wa_zlest0155-ds_placa_trator.
    wa_saida-peso_estimado       = wa_zlest0155-nr_peso_alvo.
    wa_saida-valor_frete         = wa_zlest0155-vlr_frete_tk11. "KBETR.
    wa_saida-valor_f_negociado   = wa_zlest0155-vlr_frete_neg.
    wa_saida-motivo_alteracao    = wa_zlest0155-motivo.
    wa_saida-data                = wa_zlest0155-dt_mod.
    wa_saida-hora                = wa_zlest0155-hr_mod.
    wa_saida-usuario             = wa_zlest0155-bname.


    CASE wa_zlest0155-status_aprov.
      WHEN 1.
        wa_saida-status_texto = icon_green_light.    "'Aprovado'.

        SELECT ordem  id_ordem  nivel aprovador data_atual hora_atual
          FROM zlest0157
          INTO TABLE it_zlest0157
          WHERE ordem    = wa_zlest0155-nr_ordem
            AND id_ordem = wa_zlest0155-id_ordem.

        IF wa_zlest0155-status_aprov = '1' OR wa_zlest0155-status_aprov = '2'.
          READ TABLE it_zlest0157 INTO wa_zlest0157 WITH KEY ordem = wa_zlest0155-nr_ordem id_ordem = wa_zlest0155-id_ordem.
          wa_saida-aprovador = wa_zlest0157-aprovador.
          wa_saida-dt_aprov  = wa_zlest0157-data_atual.
          wa_saida-hr_aprov  = wa_zlest0157-hora_atual.
        ENDIF.

      WHEN 2.
        wa_saida-status_texto = icon_red_light. "'Reprovado'.
      WHEN 3.
        wa_saida-status_texto = icon_light_out. "'Bloqueado'.
      WHEN 9.
        wa_saida-status_texto = icon_yellow_light. "'Aguardando Aprovação'.
    ENDCASE.

    "FILIAL
    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_zlest0155-id_branch.
    xfil = wa_t001w-name1.
    wa_saida-filial = wa_zlest0155-id_branch. "|{ WA_ZLEST0155-ID_BRANCH } { XFIL }|.
    wa_saida-desc_filial =  xfil.
    CLEAR wa_t001w.

    "TIPO DE ORDEM
    READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = wa_zlest0155-vbeln.
    READ TABLE it_tvak INTO wa_tvak WITH KEY auart = wa_vbak-auart.
    wa_saida-tipo_ordem = |{ wa_vbak-auart } { wa_tvak-bezob }|.

    "EMISSOR
    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_zlest0155-id_branch_ag.
    xcclr = wa_zlest0155-id_branch_ag.
    xlr   = wa_t001w-name1.
    wa_saida-emissor = |{ xcclr }-{ xlr }|.


    "PRODUTO
    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_zlest0155-id_produto.
    wa_saida-produto = |{ wa_zlest0155-id_produto }-{ wa_makt-maktx }|.

    "PONTO DE COLETA
    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zlest0155-id_local_coleta.
    xcfpc = wa_zlest0155-id_local_coleta.
    xpc   = wa_lfa1-name1.
    SHIFT xcfpc LEFT DELETING LEADING '0'.
    wa_saida-ponto_coleta = |{ xcfpc } - { xpc }|.

    "MOTORISTA:
    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zlest0155-id_motorista.
    xmot = wa_lfa1-name1.
    SHIFT wa_zlest0155-id_motorista LEFT DELETING LEADING '0'.
    wa_saida-motorista = |{ wa_zlest0155-id_motorista } - { xmot }|.


    "DESTINO
    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_zlest0155-id_local_destino.
    xcclr = wa_zlest0155-id_local_destino.
    xlr   = wa_kna1-name1.
    SHIFT xcclr LEFT DELETING LEADING '0'.
    wa_saida-destino = |{ xcclr } - { xlr }|.

    "DESCARGA
    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_zlest0155-id_local_descarga.
    xdescid   = wa_zlest0155-id_local_descarga.
    xdescname = wa_kna1-name1.

    "DADOS DO FATURAMENTO
    READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY ch_referencia = wa_zlest0155-ch_referencia.
    wa_saida-nr_romaneio = wa_zsdt0001-nr_romaneio.
    wa_saida-doc_rem     = wa_zsdt0001-doc_rem.
    wa_saida-fatura_prod = wa_zsdt0001-fatura_prod.
    wa_saida-doc_transp  = wa_zsdt0001-doc_transp.
    wa_saida-fknum       = wa_zsdt0001-fknum.

    SHIFT wa_saida-produto LEFT DELETING LEADING '0'.

    APPEND wa_saida TO it_saida.

  ENDLOOP.
ENDFORM.

*======================================================================*
* FORM CRIAR_ALV
*======================================================================*
FORM criar_alv.

  CALL SCREEN 0100.

ENDFORM.

*======================================================================*
* FORM FIELDCATALOG
*======================================================================*
FORM fieldcatalog USING   VALUE(p_fieldname)
                          VALUE(p_desc)
                          VALUE(p_tam)
                          VALUE(p_no_zero)
                          VALUE(p_cor)
                          VALUE(p_just)
                          VALUE(p_sum)
                          VALUE(p_col).

  gw_fieldcatalog-fieldname = p_fieldname.
  gw_fieldcatalog-scrtext_l = p_desc.
  gw_fieldcatalog-scrtext_m = p_desc.
  gw_fieldcatalog-scrtext_s = p_desc.
  gw_fieldcatalog-outputlen = p_tam.
  gw_fieldcatalog-no_zero   = p_no_zero.
  gw_fieldcatalog-emphasize = p_cor.
  gw_fieldcatalog-just      = p_just.
  gw_fieldcatalog-do_sum    = p_sum.
  gw_fieldcatalog-col_opt   = p_col.

  CASE p_fieldname.
    WHEN 'ORDEM_VENDA'.
      IF wa_saida-ordem_venda IS NOT INITIAL.
        gw_fieldcatalog-hotspot   = c_x.
      ENDIF.
    WHEN 'DOC_REM'.
      IF wa_saida-doc_rem IS NOT INITIAL.
        gw_fieldcatalog-hotspot   = c_x.
      ENDIF.
    WHEN 'FATURA_PROD'.
      IF wa_saida-fatura_prod IS NOT INITIAL.
        gw_fieldcatalog-hotspot   = c_x.
      ENDIF.
    WHEN 'DOC_TRANSP'.
      IF wa_saida-doc_transp IS NOT INITIAL.
        gw_fieldcatalog-hotspot   = c_x.
      ENDIF.
    WHEN 'FKNUM'.
      IF wa_saida-fknum IS NOT INITIAL.
        gw_fieldcatalog-hotspot   = c_x.
      ENDIF.
  ENDCASE.

  APPEND gw_fieldcatalog TO gt_fieldcatalog.

  CLEAR: gw_fieldcatalog.

ENDFORM.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_click.
    DATA: wl_saida      LIKE LINE OF it_saida,
          vg_ordem      TYPE zlest0155-vbeln,
          v_doc_rem     TYPE zsdt0001-doc_rem,
          v_fatura_prod TYPE zsdt0001-fatura_prod,
          v_doc_transp  TYPE zsdt0001-doc_transp,
          v_fknum       TYPE zsdt0001-fknum.

    IF e_row_id GT 0.
      IF e_column_id = 'ORDEM_VENDA'.
        CLEAR vg_ordem.
        READ TABLE it_saida INTO wl_saida INDEX e_row_id.
        vg_ordem = wl_saida-ordem_venda.
        SET PARAMETER ID 'AUN' FIELD vg_ordem.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

      ELSEIF e_column_id = 'DOC_REM'.
        CLEAR v_doc_rem.
        READ TABLE it_saida INTO wl_saida INDEX e_row_id.
        v_doc_rem = wl_saida-doc_rem.
        SET PARAMETER ID 'VL' FIELD v_doc_rem.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.

      ELSEIF e_column_id = 'FATURA_PROD'.
        CLEAR v_fatura_prod.
        READ TABLE it_saida INTO wl_saida INDEX e_row_id.
        v_fatura_prod = wl_saida-fatura_prod.
        SET PARAMETER ID 'VF' FIELD v_fatura_prod.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.

      ELSEIF e_column_id = 'DOC_TRANSP'.
        CLEAR v_doc_transp.
        READ TABLE it_saida INTO wl_saida INDEX e_row_id.
        v_doc_transp = wl_saida-doc_transp.
        SET PARAMETER ID 'TNR' FIELD v_doc_transp.
        CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.

      ELSEIF e_column_id = 'FKNUM'.
        CLEAR v_fknum.
        READ TABLE it_saida INTO wl_saida INDEX e_row_id.
        v_fknum = wl_saida-fknum.
        SET PARAMETER ID 'FKK' FIELD v_fknum.
        CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.

      ENDIF.


    ENDIF.
  ENDMETHOD.
ENDCLASS.


"FORM USER_COMAND USING UCOMM LIKE SY-UCOMM SELFIELD TYPE KKBLO_SELFIELD "SLIS_SELFIED


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: dia_inicio(10) TYPE c,
        dia_fim(10)    TYPE c.

  SET PF-STATUS 'PS0100'.
  SET TITLEBAR  'TB0100'.

  DATA:
        lst_layout TYPE lvc_s_layo.

  DATA: url(255)                TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table,
        vl_cont                 TYPE i,
        vl_butxt                TYPE t001-butxt,
        vl_dates1               TYPE char10,
        vl_dates2               TYPE char10.

  gs_layout-zebra      = 'X'.

* PBI - 60625 - Inicio - CSB
*  CREATE OBJECT obj_container
*    EXPORTING
*      container_name              = 'CONTAINER'
*    EXCEPTIONS
*      cntl_error                  = 1
*      cntl_system_error           = 2
*      create_error                = 3
*      lifetime_error              = 4
*      lifetime_dynpro_dynpro_link = 5
*      OTHERS                      = 6.
*
*  """"""""""""""""""""""""""""""""""""""""
*  IF sy-subrc <> 0.
*    MESSAGE a000(tree_control_msg).
*  ENDIF.
*
*  CREATE OBJECT dg_splitter_1
*    EXPORTING
*      parent  = obj_container
*      rows    = 2
*      columns = 1.
*
*  CALL METHOD dg_splitter_1->get_container
*    EXPORTING
*      row       = 1
*      column    = 1
*    RECEIVING
*      container = dg_parent_1.
*
*  CALL METHOD dg_splitter_1->get_container
*    EXPORTING
*      row       = 2
*      column    = 1
*    RECEIVING
*      container = dg_parent_alv.
*
*  CREATE OBJECT dg_splitter_2
*    EXPORTING
*      parent  = dg_parent_1
*      rows    = 1
*      columns = 2.
*
*  CALL METHOD dg_splitter_2->get_container
*    EXPORTING
*      row       = 1
*      column    = 1
*    RECEIVING
*      container = dg_parent_2.
*
*  CALL METHOD dg_splitter_2->get_container
*    EXPORTING
*      row       = 1
*      column    = 2
*    RECEIVING
*      container = dg_parent_2a.
*
*  CALL METHOD dg_splitter_1->set_row_height
*    EXPORTING
*      id     = 1
*      height = 28.
*
*  CALL METHOD dg_splitter_2->set_column_width
*    EXPORTING
*      id    = 1
*      width = 65.
*
*  CREATE OBJECT picture
*    EXPORTING
*      parent = dg_parent_2a.
*
*  PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.
*
*  CALL METHOD picture->load_picture_from_url
*    EXPORTING
*      url = url.
*
*  CALL METHOD picture->set_display_mode
*    EXPORTING
*      display_mode = picture->display_mode_fit_center.
* PBI - 60625 - Fim - CSB


  IF it_saida IS NOT INITIAL.
    "PERFORM CATALOG.
    PERFORM fieldcatalog USING:
      'STATUS_TEXTO'        'Status Aprovação'         '04'   'X' ''  'C' ' ' 'X',
      'FILIAL'              'Filial'                   '10'   ''  ''' ' 'C' ' ' 'X',
      'DESC_FILIAL'         'Descrição Filial'         '45'   ''  ''' ' '' ' ' 'X',
      'ORDEM_CARREGAMENTO'  'Ordem Carregamento'       '10'   ''  ' ' '' ' ' 'X',
      'ORDEM_VENDA'         'Ordem Venda'              '10'   ''  ' ' '' ' ' 'X',
      'EMISSOR'             'Emissor'                  '20'   ''  ' ' '' ' ' 'X',
      'PRODUTO'             'Produto'                  '20'   ''  ' ' '' ' ' 'X',
      'PONTO_COLETA'        'Ponto de Coleta'          '15'   ''  ' ' '' ' ' 'X',
      'DESTINO'             'Destino'                  '15'   ''  ' ' '' ' ' 'X',
      'MOTORISTA'           'Motorista'                '20'   ''  ' ' '' ' ' 'X',
      'PLACA'               'Placa'                    '10'   ''  ' ' '' ' ' 'X',
      'PESO_ESTIMADO'       'Peso Estimado'            '10'   ''  ' ' '' ' ' 'X',
      'VALOR_FRETE'         'Valor Frete'              '10'   ''  ' ' '' ' ' 'X',
      'VALOR_F_NEGOCIADO'   'Valor Negociado'          '10'   ''  ' ' '' ' ' 'X',
      'MOTIVO_ALTERACAO'    'Motivo Alteração'         '20'   ''  ' ' '' ' ' 'X',
      'NR_ROMANEIO'         'Romaneio'                 '10'   ''  ' ' 'C' ' ' 'X',
      'DOC_REM'             'Remessa'                  '10'   ''  ' ' 'C' ' ' 'X',
      'FATURA_PROD'         'Fatura'                   '10'   ''  ' ' 'C' ' ' 'X',
      'DOC_TRANSP'          'Doc.Transp.'              '10'   ''  ' ' 'C' ' ' 'X',
      'FKNUM'               'Doc.Custo'                '10'   ''  ' ' 'C' ' ' 'X',
      'APROVADOR'           'Aprovador'                '20'   ''  ' ' '' ' ' 'X',
      'DT_APROV'            'Data Aprovação'           '10'   ''  ' ' '' ' ' 'X',
      'HR_APROV'            'Hora Aprovação'           '10'   ''  ' ' '' ' ' 'X',
      'USUARIO'             'Solicitante'              '20'   ''  ' ' '' ' ' 'X',
      'DATA'                'Data Mod'                 '10'   ''  ' ' '' ' ' 'X',
      'HORA'                'Hora Mod'                 '10'   ''  ' ' '' ' ' 'X'.



    """"""""""""""""""""""""""""""""""""""""
* PBI - 60625 - Inicio - CSB
*    CREATE OBJECT obj_alv
*      EXPORTING
*        i_parent          = dg_parent_alv
*      EXCEPTIONS
*        error_cntl_create = 1
*        error_cntl_init   = 2
*        error_cntl_link   = 3
*        error_dp_create   = 4
*        OTHERS            = 5.
* PBI - 60625 - Fim - CSB

* PBI - 60625 - Inicio - CSB
    DATA: i_filtros	TYPE zif_screen_linha_filtro_t.


    PERFORM organiza_cabecalho.

    IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      EXPORTING
         i_titulo  = CONV #( p_text )
         i_filtros = i_filtros
       CHANGING
         alv = gob_gui_alv_grid
       )
       EQ abap_true.


      SET HANDLER: lcl_event_handler=>on_click FOR gob_gui_alv_grid.

      CALL METHOD gob_gui_alv_grid->set_table_for_first_display
        EXPORTING
          is_layout                     = gs_layout
          is_variant                    = gs_variant
          i_save                        = 'A'
        CHANGING
          it_outtab                     = it_saida[]
          it_fieldcatalog               = gt_fieldcatalog[]
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

* PBI - 60625 - Fim - CSB

* PBI - 60625 - Inicio - CSB
*      CALL METHOD obj_alv->set_table_for_first_display
*        EXPORTING
*          is_layout                     = gs_layout
*          is_variant                    = gs_variant
*          i_save                        = 'A'
*        CHANGING
*          it_outtab                     = it_saida[]
*          it_fieldcatalog               = gt_fieldcatalog[]
*        EXCEPTIONS
*          invalid_parameter_combination = 1
*          program_error                 = 2
*          too_many_lines                = 3
*          OTHERS                        = 4.


*      CREATE OBJECT dg_dyndoc_id
*        EXPORTING
*          style = 'ALV_GRID'.
*
*      CALL METHOD dg_dyndoc_id->initialize_document.
*
*      CALL METHOD dg_dyndoc_id->add_table
*        EXPORTING
*          no_of_columns = 1
*          border        = '0'
*          width         = '100%'
*        IMPORTING
*          table         = table_element.
*
*      CALL METHOD table_element->add_column
*        IMPORTING
*          column = column.
*
*      CALL METHOD table_element->set_column_style
*        EXPORTING
*          col_no    = 1
*          sap_align = 'CENTER'
*          sap_style = cl_dd_document=>heading.
*
*      CALL METHOD column->add_text
*        EXPORTING
*          text      = p_text
*          sap_style = 'HEADING'.
*
*      CALL METHOD dg_dyndoc_id->add_table
*        EXPORTING
*          no_of_columns = 2
*          border        = '0'
*          width         = '100'
*        IMPORTING
*          table         = table_element2.
*
*      CALL METHOD table_element2->add_column
*        EXPORTING
*          sap_style   = 'SAP_BOLD'
*          style_class = 'SAP_BOLD'
*        IMPORTING
*          column      = column_1.
*
*      SET HANDLER:
*          lcl_event_handler=>on_click FOR obj_alv.
*
*      PERFORM organiza_cabecalho.
*
*      CALL METHOD column_1->add_text
*        EXPORTING
*          text_table = p_text_table
*          fix_lines  = 'X'.
*
*      CALL METHOD dg_dyndoc_id->merge_document.
*
*      CREATE OBJECT dg_html_cntrl
*        EXPORTING
*          parent = dg_parent_2.
*
*      dg_dyndoc_id->html_control = dg_html_cntrl.
*
*      CALL METHOD dg_dyndoc_id->display_document
*        EXPORTING
*          reuse_control      = 'X'
*          parent             = dg_parent_2
*        EXCEPTIONS
*          html_display_error = 1.
*
*      IF sy-subrc <> 0.
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      ENDIF.
* PBI - 60625 - Fim - CSB
    ELSE.
      gob_gui_alv_grid->refresh_table_display( ).
    ENDIF.

  ENDIF.

ENDMODULE.


FORM organiza_cabecalho.

  dia_inicio = |{ datasoli-low+6(2) }.{ datasoli-low+4(2) }.{ datasoli-low+0(4) }|.
  dia_fim = |{ datasoli-high+6(2) }.{ datasoli-high+4(2) }.{ datasoli-high+0(4) }|.

  sdydo_text_element = 'Relatório de Solicitação de Alteração de Preço de Frete: ____________________________________________________'.
  APPEND sdydo_text_element TO p_text_table.
  CLEAR: sdydo_text_element.

  PERFORM forma_cabecalho USING filial   filial-option    filial-low    filial-high TEXT-001.
  PERFORM forma_cabecalho USING ordemcar ordemcar-option  ordemcar-low  ordemcar-high TEXT-002.
  PERFORM forma_cabecalho USING ordemven ordemven-option  ordemven-low  ordemven-high TEXT-003.
  PERFORM forma_cabecalho USING datasoli datasoli-option  dia_inicio    dia_fim TEXT-004.
  PERFORM forma_cabecalho USING solicita solicita-option  solicita-low  solicita-high TEXT-005.
  PERFORM forma_cabecalho USING status   status-option    status-low    status-high TEXT-006.

ENDFORM.


FORM forma_cabecalho  USING    p_status
                               p_status_option
                               p_status_low
                               p_status_high
                               p_text_006.

  IF p_status IS NOT INITIAL.
    IF p_status_option NE 'EQ' AND p_status_option NE 'BT'.
      sdydo_text_element = p_text_006.
      APPEND VALUE #( parametro =  p_text_006 ) TO i_filtros.
      EXIT.
    ELSEIF p_status_option EQ 'BT'.
      sdydo_text_element = | { p_text_006 } { p_status_low } - { p_status_high } |.
      APPEND VALUE #( parametro  = | { p_text_006 } { p_status_low } - { p_status_high } | ) TO i_filtros.
    ELSE.
      sdydo_text_element = | { p_text_006 } { p_status_low } |.
      APPEND VALUE #( parametro = | { p_text_006 } { p_status_low } | ) TO i_filtros.
    ENDIF.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR sdydo_text_element.
  ELSE.
    sdydo_text_element = p_text_006.
    APPEND VALUE #( parametro =  p_text_006 ) TO i_filtros.
    APPEND sdydo_text_element TO p_text_table.
    CLEAR: sdydo_text_element.
  ENDIF.

ENDFORM.

*Busca a logo Marca e adiciona no cabeçario.
FORM f_pega_imagem  USING    nome_logo
                  CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.

  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

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
*&      Module  EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_screen INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'OTHERS'.
  ENDCASE.
ENDMODULE.
