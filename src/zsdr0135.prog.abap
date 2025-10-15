*&---------------------------------------------------------------------*
*& Report  ZSDR0135                                                    *
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Reenvio de Ordem de venda ao OPUS                       *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
REPORT zsdr0135.

TYPES: BEGIN OF ty_outtab,
         icon   TYPE iconname,
         ordped TYPE vbak-vbeln,
         text1  TYPE bapi_msg,
         text2  TYPE bapi_msg,
       END   OF ty_outtab.

DATA: git_outtab TYPE STANDARD TABLE OF ty_outtab,
      gwa_outtab TYPE ty_outtab,
      gr_table   TYPE REF TO cl_salv_table.

DATA: git_vbak     TYPE TABLE OF vbak,
      git_ekko     TYPE TABLE OF ekko,
      git_zmmt0196 TYPE TABLE OF zmmt0196.  "*-CS2025000249-26.03.2025-#170726-JT

DATA: lit_ekpo  TYPE TABLE OF uekpo,
      lit_eket  TYPE TABLE OF ueket,
      lit_ekpa  TYPE TABLE OF ekpa,
      lwa_ekpo  TYPE uekpo,
      lwa_eket  TYPE ueket,
      lwa_ekpa  TYPE ekpa,
      lwa_uekpa TYPE uekpa.

DATA: lva_zauart   TYPE zsdt0010-zauart,
      lva_aux      TYPE char01,
      lva_local    TYPE ekpa-lifn2,
      lva_mtorg    TYPE mbew-mtorg,
      lva_erro(1),
      lva_atua     TYPE char01,
      lva_qt_ordem TYPE vbap-kwmeng,
      lva_kunnr    TYPE kna1-kunnr,
      lva_werks    TYPE uekpo-werks.

DATA: lit_matkl_fert     TYPE TABLE OF tvarvc,
      matkl_fertilizante TYPE RANGE OF matkl,
      i_ordem_venda      TYPE zde_cargueiro_ov,
      lwa_mara           TYPE mara.


DATA: l_header        TYPE mepoheader.
DATA: w_pedido_compra    TYPE zde_cargueiro_pc.
DATA: lt_item      TYPE TABLE OF mepoitem,
      lt_erro      TYPE TABLE OF zmms004,
      it_itens     TYPE TABLE OF ekpo, "purchase_order_items,
      ls_itens     TYPE ekpo, "purchase_order_item,
      lt_partners  TYPE mmpur_partner_all_tab,
      ls_partner   LIKE LINE OF lt_partners,
      lt_ekpa      TYPE TABLE OF ekpa,
      ls_ekpa      TYPE ekpa,
      lo_po_doc    TYPE REF TO if_purchasing_document,
      l_erro       TYPE zmms004,
      wa_itens     TYPE purchase_order_item,
      wa_item_if   TYPE REF TO if_purchase_order_item_mm,
      l_item       TYPE mepoitem,
      l_nome_tvarv TYPE tvarvc-name,
      l_lifn2      TYPE ekpa-lifn2,
      r_ekpa       TYPE RANGE OF ekpa-lifn2,
      r_vbpa       TYPE RANGE OF vbpa-lifnr,
      r_matkl      TYPE RANGE OF ekpo-matkl,
      l_mtext      TYPE char100,
      l_mtext1     TYPE char100,
      l_mtext2     TYPE char100.

DATA: lc_solicitacao TYPE zde_cargueiro_sl. "*-CS2025000249-26.03.2025-#170726-JT

DATA: lwa_auart       TYPE vbak-auart,
      lva_atualizacao TYPE char1,
      lwa_vbak        TYPE vbak,
      lwa_vbap        TYPE vbapvb,
      lwa_vbuk        TYPE vbak, "vbuk,
      lwa_vbkd        TYPE vbkd,
      lwa_vbpa        TYPE vbpa,
      lit_values      TYPE TABLE OF rgsb4,
      lit_range       TYPE RANGE OF vbap-werks,
      lwa_range       LIKE LINE  OF lit_range,
      lit_vbkd        TYPE TABLE OF vbkd,
      lit_vbuk        TYPE TABLE OF vbak, "vbuk,
      lit_vbpa        TYPE TABLE OF vbpa,
      lwa_status      TYPE vbuk-gbstk.


DATA: BEGIN OF lit_vbap OCCURS 125.
        INCLUDE STRUCTURE vbapvb.
DATA: END OF lit_vbap.

DATA lva_mtart TYPE mtart.
DATA lva_matkl TYPE matkl.

*FIELD-SYMBOLS: <fs1_vbak> TYPE vbak,
*               <fs1_vbuk> TYPE vbuk,
*               <fs1_vbkd> TYPE vbkd.

FIELD-SYMBOLS: <fs_data> LIKE LINE OF lit_vbpa.


DATA: e_erro_mesg TYPE bapi_msg.


CONSTANTS: lc_000000 TYPE vbkd-posnr VALUE '000000',
           lc_14     TYPE char2      VALUE '14',
           lc_01     TYPE char2      VALUE '01',
           lc_02     TYPE char2      VALUE '02',
           lc_a      TYPE char1      VALUE 'A',
           lc_i      TYPE char1      VALUE 'I',
           lc_0      TYPE char1      VALUE '0',
           lc_d      TYPE char1      VALUE 'D'.

SELECTION-SCREEN: BEGIN OF BLOCK b1.
  PARAMETERS: p_vbeln TYPE vbeln.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: r_opus RADIOBUTTON GROUP g1 USER-COMMAND rad1 DEFAULT 'X',
              r_crg  RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b2.


START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.


*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM fm_start_of_selection.
  PERFORM fm_dados_seleciona.
  PERFORM fm_dados_processa.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona .

  SELECT * FROM vbak
  INTO TABLE git_vbak
 WHERE vbeln  =  p_vbeln.

  IF git_vbak IS INITIAL.
    SELECT * FROM ekko
      INTO TABLE git_ekko
      WHERE ebeln =  p_vbeln.

*-CS2025000249-26.03.2025-#170726-JT-inicio
    IF git_ekko IS INITIAL.
      SELECT *
        FROM zmmt0196
        INTO TABLE git_zmmt0196
       WHERE nro_sol =  p_vbeln.
    ENDIF.

    "IF git_zmmt0196 IS INITIAL.
    IF git_ekko IS INITIAL AND git_zmmt0196[] IS INITIAL.
*-CS2025000249-26.03.2025-#170726-JT-fim
      MESSAGE 'Dados não encontrados.' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_processa .

  CASE abap_true.
    WHEN r_opus.
      PERFORM fm_processa_opus.
    WHEN r_crg.
      PERFORM fm_processa_carguero.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .

  IF git_outtab IS NOT INITIAL.

    DATA: lr_content    TYPE REF TO cl_salv_form_element,
          lr_selections TYPE REF TO cl_salv_selections.

    DATA: r_columns TYPE REF TO cl_salv_columns_table,
          r_column  TYPE REF TO cl_salv_column.

    cl_salv_table=>factory(
      EXPORTING
        list_display = 'X'
      IMPORTING
        r_salv_table = gr_table
      CHANGING
        t_table      = git_outtab ).

    gr_table->set_screen_popup(
      start_column = 1
      end_column   = 100
      start_line   = 1
      end_line     = 20 ).

    lr_selections = gr_table->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    r_columns = gr_table->get_columns( ).
    r_columns->set_optimize( abap_true ).

    r_column = r_columns->get_column( 'ICON' ).
    r_column->set_short_text( 'Status' ).

    r_column = r_columns->get_column( 'ORDPED' ).
    r_column->set_short_text( 'Ord/Ped' ).

    r_column = r_columns->get_column( 'TEXT1' ).
    r_column->set_short_text( 'Texto 1' ).

    r_column = r_columns->get_column( 'TEXT2' ).
    r_column->set_short_text( 'Texto 2' ).



    gr_table->display( ).

  ENDIF.

ENDFORM.

FORM fm_processa_opus.

  PERFORM: fm_processa_opus_ordem_venda,
           fm_processa_opus_pedido.
  "Não precisa enviar solicitação para o OPUS, por isso foi comentado fm_processa_opus_solicitacao.     "*-CS2025000249-26.03.2025-#170726-JT

ENDFORM.

FORM fm_processa_carguero.

  PERFORM: fm_processa_carguero_ov,
           fm_processa_carguero_ped,
           fm_processa_carguero_solicit.     "*-CS2025000249-26.03.2025-#170726-JT

ENDFORM.

FORM fm_processa_opus_ordem_venda.

*** Implementação Envio Ordem Opus

  READ TABLE  git_vbak INTO lwa_vbak INDEX 1.

  CHECK sy-subrc EQ 0 AND git_vbak[] IS NOT INITIAL.

  SELECT SINGLE zauart
    FROM zsdt0010
    INTO lwa_auart
  WHERE  cenario EQ 'SD'
    AND  zauart  EQ lwa_vbak-auart.

  CHECK lwa_auart IS NOT INITIAL.

  CLEAR: lva_atualizacao,
         lwa_vbap,
         lwa_vbuk,
         lwa_vbkd,
         lit_vbap[],
         lit_vbkd[],
         lit_vbuk[].

  SELECT *
    INTO TABLE lit_vbap
    FROM vbap
     FOR ALL ENTRIES IN git_vbak
   WHERE vbeln = git_vbak-vbeln.


  SELECT *
    INTO TABLE lit_vbkd
    FROM vbkd
     FOR ALL ENTRIES IN lit_vbap
    WHERE vbeln = lit_vbap-vbeln
      AND posnr = lit_vbap-posnr.
**=====================================Ajuste para atender IR169573 / 06/02/2024 - ABAP - AOENNING.
  IF sy-subrc NE 0.
    SELECT * FROM vbkd
    INTO TABLE lit_vbkd
      FOR ALL ENTRIES IN lit_vbap
    WHERE vbeln = lit_vbap-vbeln.
  ENDIF.
**=====================================Ajuste para atender IR169573 / 06/02/2024 - ABAP - AOENNING.
  SELECT *
    INTO TABLE lit_vbuk
    FROM vbak "vbuk
     FOR ALL ENTRIES IN lit_vbap
    WHERE vbeln = lit_vbap-vbeln.

  SELECT *
    INTO TABLE lit_vbpa
    FROM vbpa
     FOR ALL ENTRIES IN lit_vbap
    WHERE vbeln = lit_vbap-vbeln.
  "AND posnr = lit_vbap-posnr.


  lva_atualizacao = 'A'.

*excluir os itens que foram eliminados = d
  DELETE lit_vbap WHERE updkz EQ 'D'.

*// Inicio Wsb
  SELECT sign AS sign,
         opti AS option,
         low  AS low
    FROM tvarvc
      INTO CORRESPONDING FIELDS OF TABLE @r_matkl
     WHERE name EQ 'MAGGI_GR_FERTILIZANTES'.
*// Fim Wsb

*verificar se o centro está entre exceção
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr      = '0000MV45AFZZ_WERKS'
    TABLES
      set_values = lit_values.

  lwa_range-option = 'EQ'.
  lwa_range-sign   = 'I'.

  LOOP AT lit_values INTO DATA(_values).
    lwa_range-low = _values-from.
    APPEND lwa_range TO lit_range.
  ENDLOOP.

*não precisa passar o vbeln porque a ordem é a mesma
  LOOP AT lit_vbap ASSIGNING FIELD-SYMBOL(<fs1_vbap>).

    lwa_vbap = <fs1_vbap>.

*// Inicio Wsb
    IF lwa_vbap-werks IN lit_range AND lit_range IS NOT INITIAL.
      SELECT SINGLE mtart, matkl
        FROM mara
        INTO ( @lva_mtart, @lva_matkl )
       WHERE matnr EQ @lwa_vbap-matnr.

      IF lva_mtart NE 'ZHAW'.
        IF r_matkl IS NOT INITIAL AND lva_matkl IN r_matkl.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
*// Fim Wsb

    CLEAR lwa_vbuk.
    READ TABLE lit_vbuk ASSIGNING FIELD-SYMBOL(<fs1_vbuk>) INDEX 1.

    IF sy-subrc IS INITIAL.
      lwa_vbuk = <fs1_vbuk>.
    ENDIF.

    CLEAR lwa_vbkd.
    READ TABLE lit_vbkd ASSIGNING FIELD-SYMBOL(<fs1_vbkd>)
                             WITH KEY posnr = <fs1_vbap>-posnr.

    IF sy-subrc IS INITIAL.
      lwa_vbkd = <fs1_vbkd>.
    ELSE.  " nivel de Cabeçalho
      READ TABLE lit_vbkd
             ASSIGNING <fs1_vbkd>
                       WITH KEY  posnr = '000000'.

      IF sy-subrc IS INITIAL.
        lwa_vbkd = <fs1_vbkd>.
      ENDIF.
    ENDIF.

    SORT lit_vbpa BY parvw ASCENDING.
    READ TABLE lit_vbpa INTO lwa_vbpa
      WITH KEY parvw = 'LR'
      BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      lwa_vbak-kunnr = lwa_vbpa-kunnr.
    ENDIF.

*&-------------------------------------------------------BUG SOLTO 144473/IR144473 /AOENNING
    SELECT SINGLE low FROM tvarvc INTO @DATA(lwa_stvarv_lote_matnr)
    WHERE name EQ 'ZSDT0191_OV_MAT_LOTE'
      AND low  EQ @lwa_vbap-matnr.

    IF sy-subrc EQ 0 AND strlen( lwa_vbap-charg ) > 4.
      lwa_vbap-charg = lwa_vbap-charg+0(4).
    ENDIF.
*&-------------------------------------------------------BUG SOLTO 144473/IR144473 /AOENNING


    "Verificar material e lote da ov.

    "tratamento necessário para forçar a atualização do numero da OV quando inclui,
    "pois na tabela interna de parceiro o VBELN ainda não esta preenchido
    LOOP AT lit_vbpa ASSIGNING <fs_data>.
      <fs_data>-vbeln = lwa_vbak-vbeln.
    ENDLOOP.

    IF NOT ( lwa_vbak-lifsk IS INITIAL ).
      lwa_status     = 'B'.
    ELSE.
      lwa_status     = 'A'.
    ENDIF.

*--> 24.08.2023 18:35:24 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_SD_OUTBOUND_ORD_VENDA'
*      IN BACKGROUND TASK DESTINATION 'XI_ORDEM_VENDA'
*      EXPORTING
*        nu_ordem_venda = lwa_vbak-vbeln
*        tp_ordem_venda = lwa_vbak-auart
*        nu_item        = lwa_vbap-posnr
*        dt_ordem_venda = lwa_vbak-erdat
*        tp_frete       = lwa_vbkd-inco1
*        id_cliente     = lwa_vbak-kunnr
*        qt_ordem_venda = lwa_vbap-kwmeng
*        cd_material    = lwa_vbap-matnr
*        vr_unitario    = lwa_vbap-netpr
*        cd_safra       = lwa_vbap-charg
*        cd_empresa     = lwa_vbak-vkorg
*        cd_centro      = lwa_vbap-werks
*        cd_moeda       = lwa_vbap-waerk
*        st_atualizacao = lva_atualizacao
*        status         = lwa_status
*        dt_atualizacao = sy-datum
*        hr_atualizacao = sy-uzeit
*        rg_atualizado  = lc_0
*        id_interface   = lc_14
*        transgenia     = lwa_vbak-kvgr3
*        id_lote_frete  = lwa_vbap-id_lote_frete
*      TABLES
*        it_vbpa        = lit_vbpa.

*    "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - INICIO
    zcl_eudr_utils=>check_ov_pedido_eudr(
      EXPORTING
        i_vbeln = lwa_vbak-vbeln     " Nº do documento de compras
      RECEIVING
        r_eudr  = DATA(v_eudr)               " Atende critérios Europeu
    ).
*    VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - FIM
**<<<------"188424 - NMS - INI------>>>
* Verifica se o Tipo da OV é de Prestação de Serviço de frete.
    IF lwa_vbak-auart EQ 'ZTER'.
      lwa_vbap-charg = lwa_vbak-aedat(4).

    ENDIF.
**<<<------"188424 - NMS - FIM------>>>
    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_SD_OUTBOUND_ORD_VENDA'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        EXPORTING
          nu_ordem_venda = lwa_vbak-vbeln
          tp_ordem_venda = lwa_vbak-auart
          nu_item        = lwa_vbap-posnr
          dt_ordem_venda = lwa_vbak-erdat
          tp_frete       = lwa_vbkd-inco1
          id_cliente     = lwa_vbak-kunnr
          qt_ordem_venda = lwa_vbap-kwmeng
          cd_material    = lwa_vbap-matnr
          vr_unitario    = lwa_vbap-netpr
          cd_safra       = lwa_vbap-charg
          cd_empresa     = lwa_vbak-vkorg
          cd_centro      = lwa_vbap-werks
          cd_moeda       = lwa_vbap-waerk
          st_atualizacao = lva_atualizacao
          status         = lwa_status
          dt_atualizacao = sy-datum
          hr_atualizacao = sy-uzeit
          rg_atualizado  = lc_0
          id_interface   = lc_14
          transgenia     = lwa_vbak-kvgr3
          id_lote_frete  = lwa_vbap-id_lote_frete
          tipo_documento = 'OV'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
          eudr           = v_eudr "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
        TABLES
          it_vbpa        = lit_vbpa.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        EXPORTING
          nu_ordem_venda = lwa_vbak-vbeln
          tp_ordem_venda = lwa_vbak-auart
          nu_item        = lwa_vbap-posnr
          dt_ordem_venda = lwa_vbak-erdat
          tp_frete       = lwa_vbkd-inco1
          id_cliente     = lwa_vbak-kunnr
          qt_ordem_venda = lwa_vbap-kwmeng
          cd_material    = lwa_vbap-matnr
          vr_unitario    = lwa_vbap-netpr
          cd_safra       = lwa_vbap-charg
          cd_empresa     = lwa_vbak-vkorg
          cd_centro      = lwa_vbap-werks
          cd_moeda       = lwa_vbap-waerk
          st_atualizacao = lva_atualizacao
          status         = lwa_status
          dt_atualizacao = sy-datum
          hr_atualizacao = sy-uzeit
          rg_atualizado  = lc_0
          id_interface   = lc_14
          transgenia     = lwa_vbak-kvgr3
          id_lote_frete  = lwa_vbap-id_lote_frete
          tipo_documento = 'OV'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
          eudr           = v_eudr "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
        TABLES
          it_vbpa        = lit_vbpa.
    ENDIF.

    COMMIT WORK.
*<-- 24.08.2023 18:35:24 - Migração S4 – ML – Fim

    CLEAR: git_outtab.

    IF sy-subrc IS  INITIAL.
      COMMIT WORK AND WAIT.
      gwa_outtab-icon   = icon_okay  .
      gwa_outtab-ordped = lwa_vbak-vbeln.
      gwa_outtab-text1  = 'Sucesso de Envio de Ordem para OPUS'.

      APPEND gwa_outtab TO git_outtab.
      CLEAR: gwa_outtab.

    ELSE.

      gwa_outtab-icon   = icon_led_red  .
      gwa_outtab-ordped = lwa_vbak-vbeln.
      gwa_outtab-text1  = 'Erro de Envio de Ordem para OPUS'.

      APPEND gwa_outtab TO git_outtab.
      CLEAR: gwa_outtab.

    ENDIF.

    CLEAR: lit_vbap,
           lit_vbuk,
           lit_vbkd,
           lwa_vbak.

  ENDLOOP.


ENDFORM.

FORM fm_processa_opus_pedido .

  DATA: lt_split_safra  TYPE TABLE OF char40.


*** Implementação Envio Ordem Opus

* Implementação Pedido Opus
  READ TABLE git_ekko INTO DATA(lwa_ekko) INDEX 1.

  CHECK sy-subrc EQ 0 AND ( git_ekko[] IS NOT INITIAL ).

  CLEAR: lit_vbpa,
         lwa_vbpa.


  SELECT SINGLE zauart
    FROM zsdt0010
    INTO lva_zauart
   WHERE  zauart EQ lwa_ekko-bsart.

  CHECK lva_zauart IS NOT INITIAL.

  lva_aux = 'A'.

  IF lit_ekpo[] IS INITIAL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE lit_ekpo
      FROM ekpo
     WHERE ebeln EQ lwa_ekko-ebeln.

    SELECT *
      INTO TABLE lit_eket
      FROM eket
       FOR ALL ENTRIES IN lit_ekpo
      WHERE ebeln = lit_ekpo-ebeln
      AND  ebelp = lit_ekpo-ebelp.


  ENDIF.

  CLEAR: lva_local, lva_erro.
  LOOP AT lit_ekpo INTO lwa_ekpo.
    IF 'YFTE' CS lva_zauart.                            "
      lva_local = lwa_ekpo-werks.
    ENDIF.
    IF 'ZNB_ZFTE' CS lva_zauart.                            "US172998
      lva_local = lwa_ekpo-werks.
      SELECT SINGLE mtorg
        FROM mbew
        INTO lva_mtorg
       WHERE matnr = lwa_ekpo-matnr
       AND   bwkey = lwa_ekpo-werks.
      IF lva_mtorg NE 1 AND lva_mtorg NE 6.
        lva_erro = 'X'.
      ENDIF.

    ENDIF.
  ENDLOOP.

  CHECK lva_erro IS INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lit_ekpa
    FROM ekpa
   WHERE ebeln EQ lwa_ekko-ebeln.

  LOOP AT lit_ekpa INTO lwa_ekpa.
    lwa_vbpa-vbeln = lwa_ekpa-ebeln.
    lwa_vbpa-parvw = lwa_ekpa-parvw.
    lwa_vbpa-lifnr = lwa_ekpa-lifn2.
    APPEND lwa_vbpa TO lit_vbpa.
  ENDLOOP.

  LOOP AT lit_ekpa INTO lwa_uekpa.
    lwa_vbpa-vbeln = lwa_uekpa-ebeln.
    lwa_vbpa-parvw = lwa_uekpa-parvw.
    lwa_vbpa-lifnr = lwa_uekpa-lifn2.
    APPEND lwa_vbpa TO lit_vbpa.
  ENDLOOP.

  IF lva_local IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lva_local
      IMPORTING
        output = lva_local.

    lwa_vbpa-vbeln = lwa_ekko-ebeln.
    lwa_vbpa-parvw = 'LR'.
    lwa_vbpa-kunnr = lva_local.
    APPEND lwa_vbpa TO lit_vbpa.
  ENDIF.

  LOOP AT lit_ekpo INTO lwa_ekpo.

    IF lit_eket[] IS INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lit_eket
        FROM eket
       WHERE ebeln EQ lwa_ekpo-ebeln.
    ENDIF.

*---> 05/07/2023 - Migração S4 - DL
    SORT lit_eket BY ebeln.
*<--- 05/07/2023 - Migração S4 - DL

    READ TABLE lit_eket INTO lwa_eket
      WITH KEY ebeln = lwa_ekpo-ebeln
      BINARY SEARCH.

    IF lwa_ekpo-loekz NE space.
      lva_atua = 'D'.
    ELSE.
      lva_atua = lva_aux.
    ENDIF.

    lva_qt_ordem = lwa_ekpo-menge.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_ekpo-werks
      IMPORTING
        output = lva_kunnr.

    IF lwa_ekko-bsart = 'ZUB'.
      lva_werks = lwa_ekko-reswk.
    ELSE.
      lva_werks = lwa_ekpo-werks.
    ENDIF.

    DATA: lva_charg_ret TYPE eket-charg.

    CALL FUNCTION 'ZMM_GET_SAFRA_PEDIDO_FOR_OPUS'
      EXPORTING
        i_ebeln = lwa_ekpo-ebeln
      IMPORTING
        e_safra = lva_charg_ret.

    lwa_eket-charg = lva_charg_ret.

*    DATA(_fertilizantes) = ABAP_FALSE.
*    SELECT SINGLE *
*      FROM tvarvc INTO @DATA(lwa_tvarvc)
*     WHERE name = 'MAGGI_GR_FERTILIZANTES'
*       AND LOW  = @lwa_ekpo-MATKL.
*
*    IF SY-SUBRC EQ 0.
*      _fertilizantes = abap_true.
*    ENDIF.
*
*    IF ( 'ZNB_ZFTE' CS lva_zauart ) OR ( _fertilizantes EQ ABAP_TRUE AND lwa_ekko-UNSEZ IS NOT INITIAL ).
*      lwa_eket-charg = lwa_ekko-unsez.
*    ENDIF.
*
*    "Ajuste Determinação Safra Para Fertilizantes
*    IF ( _fertilizantes EQ ABAP_TRUE ) AND ( lwa_ekko-UNSEZ IS INITIAL ).
*      CLEAR: LT_SPLIT_SAFRA[].
*      SPLIT lwa_eket-charg AT '/' INTO TABLE LT_SPLIT_SAFRA.
*
*      IF LINES( LT_SPLIT_SAFRA ) EQ 2.
*        READ TABLE LT_SPLIT_SAFRA INTO DATA(LWA_SAFRA) INDEX 2.
*        IF STRLEN( LWA_SAFRA ) EQ 4.
*          lwa_eket-charg = LWA_SAFRA.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    "Ajuste Determinação Safra Para Fertilizantes

    IF lwa_eket-charg IS INITIAL AND 'ZNB_ZFTE_YFTE' CS lva_zauart.
      CONTINUE.
    ENDIF.

                                                            "US163737
    IF lwa_eket-charg IS INITIAL AND lva_zauart = 'ZUB' AND lwa_ekko-submi IS NOT INITIAL.
      SELECT SINGLE zsdt0053~charg
        FROM zsdt0051
        INNER JOIN zsdt0053 ON zsdt0051~nro_sol_ov = zsdt0053~nro_sol_ov
       INTO lwa_eket-charg
       WHERE zsdt0051~nro_sol_ov = lwa_ekko-submi
       AND zsdt0051~auart = 'ZUB'.

    ENDIF.
                                                            "US163737

    "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - INICIO
    zcl_eudr_utils=>check_ov_pedido_eudr(
      EXPORTING
        i_ebeln = lwa_ekko-ebeln     " Nº do documento de compras
      RECEIVING
        r_eudr  = DATA(v_eudr)               " Atende critérios Europeu
    ).
    "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - FIM


    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_SD_OUTBOUND_ORD_VENDA'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        EXPORTING
          nu_ordem_venda = lwa_ekko-ebeln
          tp_ordem_venda = lwa_ekko-bsart
          nu_item        = lwa_ekpo-ebelp
          dt_ordem_venda = lwa_ekko-aedat
          tp_frete       = lwa_ekpo-inco1
          id_cliente     = lva_kunnr
          qt_ordem_venda = lva_qt_ordem
          cd_material    = lwa_ekpo-matnr
          vr_unitario    = lwa_ekpo-netpr
          cd_safra       = lwa_eket-charg
          cd_empresa     = lwa_ekko-bukrs
          cd_centro      = lva_werks "im_ekko-reswk
          cd_moeda       = lwa_ekko-waers
          st_atualizacao = lva_atua
          status         = lwa_ekko-statu
          dt_atualizacao = sy-datum
          hr_atualizacao = sy-uzeit
          rg_atualizado  = lc_0
          id_interface   = lc_14
          tipo_documento = 'PD'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
          eudr           = v_eudr "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
        TABLES
          it_vbpa        = lit_vbpa.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        EXPORTING
          nu_ordem_venda = lwa_ekko-ebeln
          tp_ordem_venda = lwa_ekko-bsart
          nu_item        = lwa_ekpo-ebelp
          dt_ordem_venda = lwa_ekko-aedat
          tp_frete       = lwa_ekpo-inco1
          id_cliente     = lva_kunnr
          qt_ordem_venda = lva_qt_ordem
          cd_material    = lwa_ekpo-matnr
          vr_unitario    = lwa_ekpo-netpr
          cd_safra       = lwa_eket-charg
          cd_empresa     = lwa_ekko-bukrs
          cd_centro      = lva_werks "im_ekko-reswk
          cd_moeda       = lwa_ekko-waers
          st_atualizacao = lva_atua
          status         = lwa_ekko-statu
          dt_atualizacao = sy-datum
          hr_atualizacao = sy-uzeit
          rg_atualizado  = lc_0
          id_interface   = lc_14
          tipo_documento = 'PD'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
          eudr           = v_eudr "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
        TABLES
          it_vbpa        = lit_vbpa.
    ENDIF.

    COMMIT WORK.


    CLEAR: git_outtab.

    IF sy-subrc IS  INITIAL.
      COMMIT WORK AND WAIT.
      gwa_outtab-icon   = icon_okay  .
      gwa_outtab-ordped = lwa_ekko-ebeln.
      gwa_outtab-text1  = 'Sucesso de Envio de Pedido para OPUS'.

      APPEND gwa_outtab TO git_outtab.
      CLEAR: gwa_outtab.
    ELSE.
      gwa_outtab-icon   = icon_led_red  .
      gwa_outtab-ordped = lwa_ekko-ebeln.
      gwa_outtab-text1  = 'Erro de Envio de Pedido para OPUS'.

      APPEND gwa_outtab TO git_outtab.
      CLEAR: gwa_outtab.
    ENDIF.

    CLEAR: lwa_ekpo,
           lwa_eket.
  ENDLOOP.



ENDFORM.

*-CS2025000249-26.03.2025-#170726-JT-inicio
*FORM fm_processa_opus_solicitacao.
*
*  DATA: lt_split_safra  TYPE TABLE OF char40.
*
**** Implementação Envio Ordem Opus
*
** Implementação Pedido Opus
*  READ TABLE git_zmmt0196 INTO DATA(lwa_zmmt0196) INDEX 1.
*
*  CHECK sy-subrc EQ 0 AND ( git_zmmt0196[] IS NOT INITIAL ).
*
*  CLEAR: lit_vbpa,
*         lwa_vbpa.
*
*  SELECT SINGLE *
*    FROM ekko
*    INTO @DATA(lwa_ekko)
*   WHERE ebeln EQ @lwa_zmmt0196-ebeln.
*
*  CHECK sy-subrc = 0.
*
*  SELECT SINGLE zauart
*    FROM zsdt0010
*    INTO lva_zauart
*   WHERE  zauart EQ lwa_ekko-bsart.
*
*  CHECK lva_zauart IS NOT INITIAL.
*
*  lva_aux = 'A'.
*
*  IF lit_ekpo[] IS INITIAL.
*
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE lit_ekpo
*      FROM ekpo
*     WHERE ebeln EQ lwa_ekko-ebeln.
*
*    SELECT *
*      INTO TABLE lit_eket
*      FROM eket
*       FOR ALL ENTRIES IN lit_ekpo
*      WHERE ebeln = lit_ekpo-ebeln
*      AND  ebelp = lit_ekpo-ebelp.
*  ENDIF.
*
*  CLEAR: lva_local, lva_erro.
*  LOOP AT lit_ekpo INTO lwa_ekpo.
*    IF 'ZNB_ZFTE_YFTE' CS lva_zauart.
*      lva_local = lwa_ekpo-werks.
*      SELECT SINGLE mtorg
*        FROM mbew
*        INTO lva_mtorg
*       WHERE matnr = lwa_ekpo-matnr
*       AND   bwkey = lwa_ekpo-werks.
*      IF lva_mtorg NE 1 AND lva_mtorg NE 6.
*        lva_erro = 'X'.
*      ENDIF.
*
*    ENDIF.
*  ENDLOOP.
*
*  CHECK lva_erro IS INITIAL.
*
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE lit_ekpa
*    FROM ekpa
*   WHERE ebeln EQ lwa_ekko-ebeln.
*
*  LOOP AT lit_ekpa INTO lwa_ekpa.
*    lwa_vbpa-vbeln = lwa_ekpa-ebeln.
*    lwa_vbpa-parvw = lwa_ekpa-parvw.
*    lwa_vbpa-lifnr = lwa_ekpa-lifn2.
*    APPEND lwa_vbpa TO lit_vbpa.
*  ENDLOOP.
*
*  LOOP AT lit_ekpa INTO lwa_uekpa.
*    lwa_vbpa-vbeln = lwa_uekpa-ebeln.
*    lwa_vbpa-parvw = lwa_uekpa-parvw.
*    lwa_vbpa-lifnr = lwa_uekpa-lifn2.
*    APPEND lwa_vbpa TO lit_vbpa.
*  ENDLOOP.
*
*  IF lva_local IS NOT INITIAL.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lva_local
*      IMPORTING
*        output = lva_local.
*
*    lwa_vbpa-vbeln = lwa_ekko-ebeln.
*    lwa_vbpa-parvw = 'LR'.
*    lwa_vbpa-kunnr = lva_local.
*    APPEND lwa_vbpa TO lit_vbpa.
*  ENDIF.
*
*  LOOP AT lit_ekpo INTO lwa_ekpo.
*
*    IF lit_eket[] IS INITIAL.
*      SELECT * INTO CORRESPONDING FIELDS OF TABLE lit_eket
*        FROM eket
*       WHERE ebeln EQ lwa_ekpo-ebeln.
*    ENDIF.
*
**---> 05/07/2023 - Migração S4 - DL
*    SORT lit_eket BY ebeln.
**<--- 05/07/2023 - Migração S4 - DL
*
*    READ TABLE lit_eket INTO lwa_eket
*      WITH KEY ebeln = lwa_ekpo-ebeln
*      BINARY SEARCH.
*
*    IF lwa_ekpo-loekz NE space.
*      lva_atua = 'D'.
*    ELSE.
*      lva_atua = lva_aux.
*    ENDIF.
*
*    lva_qt_ordem = lwa_ekpo-menge.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lwa_ekpo-werks
*      IMPORTING
*        output = lva_kunnr.
*
*    IF lwa_ekko-bsart = 'ZUB'.
*      lva_werks = lwa_ekko-reswk.
*    ELSE.
*      lva_werks = lwa_ekpo-werks.
*    ENDIF.
*
*    DATA: lva_charg_ret TYPE eket-charg.
*
*    CALL FUNCTION 'ZMM_GET_SAFRA_PEDIDO_FOR_OPUS'
*      EXPORTING
*        i_ebeln = lwa_ekpo-ebeln
*      IMPORTING
*        e_safra = lva_charg_ret.
*
*    lwa_eket-charg = lva_charg_ret.
*
**    "Ajuste Determinação Safra Para Fertilizantes
*    IF lwa_eket-charg IS INITIAL AND 'ZNB_ZFTE_YFTE' CS lva_zauart.
*      CONTINUE.
*    ENDIF.
*
*                                                            "US163737
*    IF lwa_eket-charg IS INITIAL AND lva_zauart = 'ZUB' AND lwa_ekko-submi IS NOT INITIAL.
*      SELECT SINGLE zsdt0053~charg
*        FROM zsdt0051
*        INNER JOIN zsdt0053 ON zsdt0051~nro_sol_ov = zsdt0053~nro_sol_ov
*       INTO lwa_eket-charg
*       WHERE zsdt0051~nro_sol_ov = lwa_ekko-submi
*       AND zsdt0051~auart = 'ZUB'.
*
*    ENDIF.
*                                                            "US163737
*
*    "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - INICIO
*    zcl_eudr_utils=>check_ov_pedido_eudr(
*      EXPORTING
*        i_ebeln = lwa_ekko-ebeln     " Nº do documento de compras
*      RECEIVING
*        r_eudr  = DATA(v_eudr)               " Atende critérios Europeu
*    ).
*    "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223 - FIM
*
*
*    DATA: lv_rfc TYPE rfcdest.
*
*    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_SD_OUTBOUND_ORD_VENDA'.
*
*    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
*      EXPORTING
*        i_fm          = c_fm
*      IMPORTING
*        e_rfc         = lv_rfc
*      EXCEPTIONS
*        no_rfc        = 1
*        no_rfc_config = 2
*        OTHERS        = 3.
*
*    IF sy-subrc EQ 0.
*      CALL FUNCTION c_fm IN BACKGROUND TASK
*        DESTINATION lv_rfc
*        EXPORTING
*          nu_ordem_venda = lwa_ekko-ebeln
*          tp_ordem_venda = lwa_ekko-bsart
*          nu_item        = lwa_ekpo-ebelp
*          dt_ordem_venda = lwa_ekko-aedat
*          tp_frete       = lwa_ekpo-inco1
*          id_cliente     = lva_kunnr
*          qt_ordem_venda = lva_qt_ordem
*          cd_material    = lwa_ekpo-matnr
*          vr_unitario    = lwa_ekpo-netpr
*          cd_safra       = lwa_eket-charg
*          cd_empresa     = lwa_ekko-bukrs
*          cd_centro      = lva_werks "im_ekko-reswk
*          cd_moeda       = lwa_ekko-waers
*          st_atualizacao = lva_atua
*          status         = lwa_ekko-statu
*          dt_atualizacao = sy-datum
*          hr_atualizacao = sy-uzeit
*          rg_atualizado  = lc_0
*          id_interface   = lc_14
*          tipo_documento = 'PD'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
*          eudr           = v_eudr "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
*        TABLES
*          it_vbpa        = lit_vbpa.
*    ELSE.
*      CALL FUNCTION c_fm IN BACKGROUND TASK
*        EXPORTING
*          nu_ordem_venda = lwa_ekko-ebeln
*          tp_ordem_venda = lwa_ekko-bsart
*          nu_item        = lwa_ekpo-ebelp
*          dt_ordem_venda = lwa_ekko-aedat
*          tp_frete       = lwa_ekpo-inco1
*          id_cliente     = lva_kunnr
*          qt_ordem_venda = lva_qt_ordem
*          cd_material    = lwa_ekpo-matnr
*          vr_unitario    = lwa_ekpo-netpr
*          cd_safra       = lwa_eket-charg
*          cd_empresa     = lwa_ekko-bukrs
*          cd_centro      = lva_werks "im_ekko-reswk
*          cd_moeda       = lwa_ekko-waers
*          st_atualizacao = lva_atua
*          status         = lwa_ekko-statu
*          dt_atualizacao = sy-datum
*          hr_atualizacao = sy-uzeit
*          rg_atualizado  = lc_0
*          id_interface   = lc_14
*          tipo_documento = 'PD'   "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
*          eudr           = v_eudr "VA01/ME21N - Identif EUDR - Int. OV/Pedido - BG #545223
*        TABLES
*          it_vbpa        = lit_vbpa.
*    ENDIF.
*
*    COMMIT WORK.
*
*    CLEAR: git_outtab.
*
*    IF sy-subrc IS  INITIAL.
*      COMMIT WORK AND WAIT.
*      gwa_outtab-icon   = icon_okay  .
*      gwa_outtab-ordped = lwa_zmmt0196-nro_sol.
*      gwa_outtab-text1  = 'Sucesso de Envio de Solicitação para OPUS'.
*
*      APPEND gwa_outtab TO git_outtab.
*      CLEAR: gwa_outtab.
*    ELSE.
*      gwa_outtab-icon   = icon_led_red  .
*      gwa_outtab-ordped = lwa_zmmt0196-nro_sol.
*      gwa_outtab-text1  = 'Erro de Envio de Solicitação para OPUS'.
*
*      APPEND gwa_outtab TO git_outtab.
*      CLEAR: gwa_outtab.
*    ENDIF.
*
*    CLEAR: lwa_ekpo,
*           lwa_eket.
*  ENDLOOP.
*
*ENDFORM.
*-CS2025000249-26.03.2025-#170726-JT-fim

FORM fm_processa_carguero_ov.

  DATA: lit_zsdt0082  TYPE TABLE OF zsdt0082.
  DATA: l_tot_brgew   TYPE vbap-brgew.

  CHECK git_vbak[] IS NOT  INITIAL.

  CLEAR: lit_vbap, lwa_vbak.

  SELECT *
    INTO TABLE lit_vbap
    FROM vbap
     FOR ALL ENTRIES IN git_vbak
   WHERE vbeln = git_vbak-vbeln.

  SELECT *
    FROM tvarvc
    INTO TABLE lit_matkl_fert
   WHERE name = 'MAGGI_GR_FERTILIZANTES'.

  IF sy-subrc = 0.
    LOOP AT lit_matkl_fert INTO DATA(lwa_matkl_fert).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_matkl_fert-low ) TO matkl_fertilizante.
    ENDLOOP.
  ENDIF.

  CHECK matkl_fertilizante[] IS NOT INITIAL.

  LOOP AT lit_vbap ASSIGNING FIELD-SYMBOL(<fs_xvbap>).
    CLEAR: i_ordem_venda.

    SELECT SINGLE *
      FROM vbpa INTO @DATA(w_vbpa_pc)
     WHERE vbeln = @<fs_xvbap>-vbeln
       AND parvw = 'PC'.

    CHECK sy-subrc = 0.

    CLEAR lwa_mara.
    SELECT SINGLE *
      INTO lwa_mara
      FROM mara
     WHERE matnr = <fs_xvbap>-matnr.

    CHECK sy-subrc = 0.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lwa_mara-matkl
      IMPORTING
        output = lwa_mara-matkl.

    IF lwa_mara-matkl IN matkl_fertilizante[].

      l_nome_tvarv = 'PARCEIRO_PC_CENTRO_' && <fs_xvbap>-werks.

      SELECT *
        FROM tvarvc
        INTO TABLE @DATA(t_parceiro)
       WHERE name = @l_nome_tvarv.

      IF sy-subrc = 0.
        LOOP AT t_parceiro INTO DATA(w_parceiro).
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = w_parceiro-low
            IMPORTING
              output = l_lifn2.

          APPEND VALUE #( sign = 'I' option = 'EQ' low = l_lifn2 ) TO r_vbpa.
        ENDLOOP.
      ENDIF.

      CHECK w_vbpa_pc-lifnr IN r_vbpa[] AND r_vbpa[] IS NOT INITIAL.

      CHECK ( lwa_mara-mtart = 'ZHAW' ) OR ( lwa_mara-mtart = 'ZFER' ).

      "Verificar quantidasde liberada...

      CLEAR: lit_zsdt0082[], l_tot_brgew.

      SELECT *
        FROM zsdt0082
        INTO TABLE @lit_zsdt0082
       WHERE   vbeln  = @<fs_xvbap>-vbeln
         AND   posnr  = @<fs_xvbap>-posnr
         AND ( status = '2'
          OR   status = '5' ).

      "peso total
      FREE l_tot_brgew.

      LOOP AT lit_zsdt0082 INTO DATA(w_0082_tab).
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = <fs_xvbap>-matnr
            i_in_me              = <fs_xvbap>-gewei
            i_out_me             = 'KG'
            i_menge              = w_0082_tab-qte_lib
          IMPORTING
            e_menge              = w_0082_tab-qte_lib
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.

        l_tot_brgew = l_tot_brgew + w_0082_tab-qte_lib.
      ENDLOOP.

      <fs_xvbap>-brgew         = l_tot_brgew.

    ENDIF.

    READ TABLE git_vbak INTO lwa_vbak INDEX 1.

    MOVE-CORRESPONDING  lwa_vbak TO i_ordem_venda.
    i_ordem_venda-item = <fs_xvbap>.

*-----------------------------------------------
*---obtem patrceiros
*-----------------------------------------------
    SELECT *
     FROM vbpa
     INTO TABLE @DATA(t_vbpa)
    WHERE vbeln = @lwa_vbak-vbeln.
*-----------------------------------------------
*---obtem dados comerciais
*-----------------------------------------------
    SELECT *
      FROM vbkd
      INTO TABLE @DATA(t_vbkd)
     WHERE vbeln = @lwa_vbak-vbeln.

*-----------------------------------------------
*---preenche tables
*-----------------------------------------------
    DATA: xvbpa TYPE TABLE OF vbpavb,
          wvbpa TYPE vbpavb,
          xvbkd TYPE TABLE OF vbkdvb,
          wvbkd TYPE vbkdvb.

    LOOP AT t_vbpa             INTO DATA(w_vbpa).
      MOVE-CORRESPONDING w_vbpa  TO wvbpa.
      APPEND wvbpa               TO xvbpa.
    ENDLOOP.

    LOOP AT t_vbkd             INTO DATA(w_vbkd).
      MOVE-CORRESPONDING w_vbkd  TO wvbkd.
      APPEND wvbkd               TO xvbkd.
    ENDLOOP.


    i_ordem_venda-parceiros = xvbpa[].
    i_ordem_venda-comercial = xvbkd[].

    CLEAR: git_outtab.

    TRY .
        zcl_integracao_lote_frete=>zif_integracao_lote_frete~set_gerencia_lote( EXPORTING i_ordem_venda = i_ordem_venda IMPORTING e_id_lote_frete = <fs_xvbap>-id_lote_frete ).

        gwa_outtab-icon   = icon_okay  .
        gwa_outtab-ordped = lwa_vbak-vbeln.
        gwa_outtab-text1  = 'Sucesso de Envio ao Carguero'.
        APPEND gwa_outtab TO git_outtab.
        CLEAR: gwa_outtab.

*-----------------------------------------------
*-----Atualiza ID LOTE
*-----------------------------------------------
*        UPDATE vbap
*           SET id_lote_frete = <fs_xvbap>-id_lote_frete
*         WHERE vbeln         = <fs_xvbap>-vbeln
*           AND posnr         = <fs_xvbap>-posnr.
*
*        COMMIT WORK AND WAIT.


      CATCH zcx_integracao INTO DATA(ex_integracao).
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = ex_integracao->msgid
            msgnr               = ex_integracao->msgno
            msgv1               = ex_integracao->msgv1
            msgv2               = ex_integracao->msgv2
            msgv3               = ex_integracao->msgv3
            msgv4               = ex_integracao->msgv4
          IMPORTING
            message_text_output = e_erro_mesg.

        gwa_outtab-icon   = icon_led_red.
        gwa_outtab-ordped = lwa_vbak-vbeln.
        gwa_outtab-text1  = ex_integracao->msgv1.
        gwa_outtab-text2  = e_erro_mesg.

        APPEND gwa_outtab TO git_outtab.
        CLEAR: gwa_outtab.

        EXIT.
      CATCH zcx_error      INTO DATA(ex_error).
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = ex_error->msgid
            msgnr               = ex_error->msgno
            msgv1               = ex_error->msgv1
            msgv2               = ex_error->msgv2
            msgv3               = ex_error->msgv3
            msgv4               = ex_error->msgv4
          IMPORTING
            message_text_output = e_erro_mesg.

        gwa_outtab-icon   = icon_led_red.
        gwa_outtab-ordped = lwa_vbak-vbeln.
        gwa_outtab-text1  = ex_error->msgv1.
        gwa_outtab-text2  = e_erro_mesg.

        APPEND gwa_outtab TO git_outtab.
        CLEAR: gwa_outtab.

    ENDTRY.
  ENDLOOP.


ENDFORM.

FORM fm_processa_carguero_ped.

  FREE: lt_ekpa, r_ekpa, r_matkl.
  FREE: w_pedido_compra.

  CHECK git_ekko[] IS NOT INITIAL.

*----------------------------------------------------
*   Obtém dados do Item
*----------------------------------------------------
  READ TABLE git_ekko INTO DATA(lwa_ekko) INDEX 1.

  CHECK sy-subrc EQ 0.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itens
    FROM ekpo
   WHERE ebeln EQ lwa_ekko-ebeln.

*----------------------------------------------------
*---Obtener Header Structure
*----------------------------------------------------

  MOVE-CORRESPONDING lwa_ekko TO l_header.

*----------------------------------------------------
*---PARCEIROS
*----------------------------------------------------

  SELECT * INTO CORRESPONDING FIELDS OF TABLE lit_ekpa
    FROM ekpa
   WHERE ebeln EQ lwa_ekko-ebeln.

  IF lit_ekpa IS NOT INITIAL.
    LOOP AT lit_ekpa INTO lwa_ekpa.

      MOVE lwa_ekko-ebeln   TO ls_ekpa-ebeln.
      APPEND lwa_ekpa       TO lt_ekpa.
      APPEND lwa_ekpa       TO w_pedido_compra-parceiros.
    ENDLOOP.
  ELSE.
*    lo_po_doc ?= im_header.  " casting to purchase document
*    lt_partners = lo_po_doc->get_partners( ).
*
*    LOOP AT lt_partners                INTO ls_partner .
*      MOVE-CORRESPONDING ls_partner-data TO ls_ekpa.
*      MOVE im_ebeln                      TO ls_ekpa-ebeln.
*      MOVE ls_partner-data-parvw         TO ls_ekpa-parvw.
*      MOVE ls_partner-data-parno         TO ls_ekpa-lifn2.
*      APPEND ls_ekpa                     TO lt_ekpa.
*      APPEND ls_ekpa                     TO w_pedido_compra-parceiros.
*    ENDLOOP.
  ENDIF.

*----------------------------------------------------
*---Itens da PO
*----------------------------------------------------
  SELECT *
    FROM tvarvc
    INTO TABLE @DATA(t_grp_mat)
   WHERE name = 'MAGGI_GR_FERTILIZANTES'.

  LOOP AT t_grp_mat INTO DATA(w_grp_mat).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_grp_mat-low ) TO r_matkl.
  ENDLOOP.

  LOOP AT it_itens INTO ls_itens.
*--------------------
*-----TVARV
*--------------------
    IF ls_itens-matkl IN r_matkl[] AND r_matkl[] IS NOT INITIAL.
      l_nome_tvarv = 'PARCEIRO_PC_CENTRO_' && ls_itens-werks.

      SELECT *
        FROM tvarvc
        INTO TABLE @DATA(t_parceiro)
       WHERE name = @l_nome_tvarv.

      IF sy-subrc = 0.
        LOOP AT t_parceiro INTO DATA(w_parceiro).
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = w_parceiro-low
            IMPORTING
              output = l_lifn2.

          APPEND VALUE #( sign = 'I' option = 'EQ' low = l_lifn2 ) TO r_ekpa.
        ENDLOOP.
      ENDIF.

      CLEAR ls_ekpa.
      READ TABLE lt_ekpa INTO ls_ekpa WITH KEY parvw = 'PR'.

      CHECK ls_ekpa-lifn2 IN r_ekpa[] AND r_ekpa[] IS NOT INITIAL.

    ELSEIF l_header-bsart EQ 'ZUB'.

      "Se for pedido de transferencia, só enviar para o carguero se for do grupo de mercadoria de grãos
      SELECT SINGLE *
        FROM tvarvc INTO @DATA(lwa_stvarv_graos)
       WHERE name EQ 'MAGGI_GR_GRAOS'
        AND  low  EQ @ls_itens-matkl.

      CHECK sy-subrc EQ 0.

    ELSE.
      CONTINUE.
    ENDIF.


    MOVE ls_itens-ebeln               TO l_header-ebeln.
    MOVE ls_itens-ebeln               TO l_item-ebeln.
    MOVE-CORRESPONDING l_header       TO w_pedido_compra.
    MOVE-CORRESPONDING ls_itens       TO w_pedido_compra-item.


    CLEAR: git_outtab, ls_itens.
    TRY .
        zcl_integracao_lote_frete=>zif_integracao_lote_frete~set_gerencia_lote(
          EXPORTING
            i_pedido_compra = w_pedido_compra
          IMPORTING
            e_id_lote_frete = l_item-id_lote_frete ).


        gwa_outtab-icon   = icon_okay  .
        gwa_outtab-ordped = l_item-ebeln.
        gwa_outtab-text1  = 'Sucesso de Envio ao Carguero'.

        APPEND gwa_outtab TO git_outtab.
        CLEAR: gwa_outtab.
*
**------------------------------------------------
** ----atualiza ID_LOTE
**------------------------------------------------
*        UPDATE ekpo
*           SET id_lote_frete = l_item-id_lote_frete
*         WHERE ebeln         = w_pedido_compra-ebeln
*           AND ebelp         = w_pedido_compra-item-ebelp.
*        COMMIT WORK AND WAIT.


      CATCH zcx_integracao INTO DATA(zcx_integracao).

        MESSAGE ID zcx_integracao->msgid
              TYPE zcx_integracao->msgty
            NUMBER zcx_integracao->msgno
              INTO l_mtext
              WITH zcx_integracao->msgv1
                   zcx_integracao->msgv2
                   zcx_integracao->msgv3
                   zcx_integracao->msgv4.

        l_mtext1 = l_mtext(50).
        l_mtext2 = l_mtext+50(50).

        gwa_outtab-icon   = icon_led_red .
        gwa_outtab-ordped = l_item-ebeln.
        gwa_outtab-text1  = l_mtext1.
        gwa_outtab-text2  = l_mtext2.

        APPEND gwa_outtab TO git_outtab.
        CLEAR: gwa_outtab.

      CATCH zcx_error INTO DATA(zcx_error).

        MESSAGE ID zcx_error->msgid
              TYPE zcx_error->msgty
            NUMBER zcx_error->msgno
              INTO l_mtext
              WITH zcx_error->msgv1
                   zcx_error->msgv2
                   zcx_error->msgv3
                   zcx_error->msgv4.

        l_mtext1 = l_mtext(50).
        l_mtext2 = l_mtext+50(50).

        gwa_outtab-icon   = icon_led_red .
        gwa_outtab-ordped = l_item-ebeln.
        gwa_outtab-text1  = l_mtext1.
        gwa_outtab-text2  = l_mtext2.

        APPEND gwa_outtab TO git_outtab.
        CLEAR: gwa_outtab.

    ENDTRY.


  ENDLOOP.

ENDFORM.

*-CS2025000249-26.03.2025-#170726-JT-inicio
FORM fm_processa_carguero_solicit.

  FREE: lc_solicitacao.

  CHECK git_zmmt0196[] IS NOT INITIAL.

*----------------------------------------------------
*   Obtém dados do Item
*----------------------------------------------------
  READ TABLE git_zmmt0196 INTO DATA(lwa_zmmt0196) INDEX 1.

  CHECK sy-subrc EQ 0.

  MOVE-CORRESPONDING lwa_zmmt0196 TO lc_solicitacao.
  LOOP AT git_zmmt0196          INTO lwa_zmmt0196.
    APPEND lwa_zmmt0196           TO lc_solicitacao-item.
  ENDLOOP.

  CLEAR: git_outtab, ls_itens.

  TRY .
      zcl_integracao_lote_frete=>zif_integracao_lote_frete~set_gerencia_lote(
        EXPORTING
          i_solicitacao   = lc_solicitacao
        IMPORTING
          e_id_lote_frete = l_item-id_lote_frete ).

      gwa_outtab-icon   = icon_okay  .
      gwa_outtab-ordped = lc_solicitacao-nro_sol.
      gwa_outtab-text1  = 'Sucesso de Envio Solicitacao ao Carguero'.

      APPEND gwa_outtab TO git_outtab.
      CLEAR: gwa_outtab.

    CATCH zcx_integracao INTO DATA(zcx_integracao).

      MESSAGE ID zcx_integracao->msgid
            TYPE zcx_integracao->msgty
          NUMBER zcx_integracao->msgno
            INTO l_mtext
            WITH zcx_integracao->msgv1
                 zcx_integracao->msgv2
                 zcx_integracao->msgv3
                 zcx_integracao->msgv4.

      l_mtext1 = l_mtext(50).
      l_mtext2 = l_mtext+50(50).

      REPLACE ALL OCCURRENCES OF REGEX '&' IN l_mtext1 WITH ''.
      REPLACE ALL OCCURRENCES OF REGEX '&' IN l_mtext2 WITH ''.

      gwa_outtab-icon   = icon_led_red .
      gwa_outtab-ordped = lc_solicitacao-nro_sol.
      gwa_outtab-text1  = l_mtext1.
      gwa_outtab-text2  = l_mtext2.

      APPEND gwa_outtab TO git_outtab.
      CLEAR: gwa_outtab.

    CATCH zcx_error INTO DATA(zcx_error).

      MESSAGE ID zcx_error->msgid
            TYPE zcx_error->msgty
          NUMBER zcx_error->msgno
            INTO l_mtext
            WITH zcx_error->msgv1
                 zcx_error->msgv2
                 zcx_error->msgv3
                 zcx_error->msgv4.

      l_mtext1 = l_mtext(50).
      l_mtext2 = l_mtext+50(50).

      REPLACE ALL OCCURRENCES OF REGEX '&' IN l_mtext1 WITH ''.
      REPLACE ALL OCCURRENCES OF REGEX '&' IN l_mtext2 WITH ''.

      gwa_outtab-icon   = icon_led_red .
      gwa_outtab-ordped = lc_solicitacao-nro_sol.
      gwa_outtab-text1  = l_mtext1.
      gwa_outtab-text2  = l_mtext2.

      APPEND gwa_outtab TO git_outtab.
      CLEAR: gwa_outtab.

  ENDTRY.

ENDFORM.
*-CS2025000249-26.03.2025-#170726-JT-fim
