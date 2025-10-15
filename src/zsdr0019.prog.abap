*&--------------------------------------------------------------------&*
*& Report Name    : Instruções de embarque de algodão                 *&
*& Author         : Victor Hugo                                       *&
*& Date           : 02.05.2012                                        *&
*& Funcional Area : SD                                                *&
*&                                                                    *&
*&--------------------------------------------------------------------&*
REPORT  zsdr0019.
*&--------------------------------------------------------------------&*
*& TABLES
*&--------------------------------------------------------------------&*
TABLES: t001, t001w, zsdt0045, vbak.

*&--------------------------------------------------------------------&*
*& Estrutura
*&--------------------------------------------------------------------&*
TYPES:
  BEGIN OF ty_t001,
    bukrs TYPE t001-bukrs,
    butxt TYPE t001-butxt,
    ort01 TYPE t001-ort01,
  END OF ty_t001,

  BEGIN OF ty_t001w,
    werks TYPE t001w-werks,
    name1 TYPE t001w-name1,
  END OF ty_t001w,

  BEGIN OF ty_zsdt0045.
    INCLUDE TYPE zsdt0045.
TYPES:
    seq(3),
  END OF ty_zsdt0045,

  BEGIN OF ty_0045,
    objek            TYPE zsdt0045-objek,
    bukrs            TYPE zsdt0045-bukrs,
    bukrs_d          TYPE kna1-name1,
    instrucao        TYPE zsdt0045-instrucao,

    instrucao_ant    TYPE zsdt0053-instrucao_ant, "INSTRUÇÃO ANTERIOR

    contrato         TYPE zsdt0045-contrato,
    list_contrato    TYPE char255,
    kunnr            TYPE zsdt0051-kunnr,
    kunnr_d          TYPE kna1-name1,
    data_instr       TYPE zsdt0045-data_instr,
    data_retirada    TYPE zsdt0045-data_retirada,
    deadline_draft   TYPE zsdt0045-deadline_draft,
    data_container   TYPE zsdt0045-data_container,
    deadline_documen TYPE zsdt0045-deadline_documen,
    data_in_porto    TYPE zsdt0045-data_in_porto,
    data_porto       TYPE zsdt0045-data_porto,
    porto_embarque   TYPE zsdt0045-porto_embarque,
    porto_embarque_d TYPE lfa1-name1,
    quantidade       TYPE zsdt0045-quantidade,
    qtd_ctners       TYPE zsdt0045-qtd_ctners,
    quantidade1      TYPE p LENGTH 16,

    qtd_fardos_exp   TYPE zsdt0053-volum,        "QTD FARDOS EXPORTADOS
    saldo_fardos     TYPE zsdt0053-volum,        "SALDO FARDOS
    nfe_fl           TYPE c LENGTH '5',          "NFe F.L.

    qtd_ctners1      TYPE p LENGTH 16,
    charg            TYPE zsdt0045-charg,
    btgew            TYPE zsdt0045-btgew,

    peso_total_exp   TYPE zsdt0053-zmeng,         "PESO TOTAL EXPORTADO
    saldo_peso       TYPE zsdt0053-zmeng,         "SALDO PESO

    booking          TYPE zsdt0045-booking,
    mapa             TYPE zsdt0045-mapa,
    fumigacao        TYPE zsdt0045-fumigacao,
    hrs_fgacao       TYPE zsdt0045-hrs_fgacao,
    armador          TYPE zsdt0045-armador,
    free_time        TYPE zsdt0045-free_time,
    terminal         TYPE zsdt0045-terminal,
    terminal_d       TYPE lfa1-name1,
    cod_despach      TYPE zsdt0045-cod_despach,
    cod_despach_d    TYPE lfa1-name1,
    cod_transp       TYPE zsdt0045-cod_transp,
    cod_transp_d     TYPE lfa1-name1,
    vlr_frete        TYPE zsdt0045-vlr_frete,
    pais_des         TYPE zsdt0045-pais_des,
    pais_ext         TYPE landx,
    terminal_estuf   TYPE zsdt0045-terminal_estuf,
    terminal_estuf_d TYPE lfa1-name1,
    controladora     TYPE zsdt0045-controladora,
    controladora_d   TYPE lfa1-name1,
    navio            TYPE zsdt0045-navio,
    limite_peso      TYPE zsdt0045-limite_peso,
    peso_max         TYPE zsdt0045-peso_max,
    stras            TYPE lfa1-stras,
    ort02            TYPE lfa1-ort02,
    ort01            TYPE lfa1-ort01,
    pstlz            TYPE lfa1-pstlz,
    regio            TYPE lfa1-regio,
    id_prog_embq     TYPE zsdt0236-id_prog_embq,
    carga            TYPE zsdt0236-carga,
    estufagem        TYPE zsdt0236-estufagem,
    pesagem          TYPE zsdt0236-pesagem,
    planilha_peso    TYPE zsdt0236-planilha_peso,
    nf_export        TYPE zsdt0236-nf_export,
    liberacao        TYPE zsdt0236-liberacao,
    deposito         TYPE zsdt0236-deposito,

    observacao       TYPE zsdt0236-observacao,           "OBSERVAÇÕES
    observacao_ico   TYPE c LENGTH 5,           "OBSERVAÇÕES
    embarque         TYPE zsdt0236-embarque,      "EMBARQUE

    pctgem_ant       TYPE zsdt0143-pctgem_ant,
    erdat            TYPE vbak-erdat,
    data_bl          TYPE vbak-erdat,

    dt_docs_completo TYPE zsdt0236-dt_docs_completo, "DT. DOCS COMPLETOS
    dt_recebimento   TYPE zsdt0236-dt_recebimento, "DT. RECEBIMENTO
    dt_apresentacao  TYPE zsdt0236-dt_apresentacao, "DT. APRESENTAÇÃO

    previsao_receb   TYPE vbak-erdat,
    data_pagto       TYPE vbak-erdat,
    dias_receb       TYPE p LENGTH 16,

    dias_rec_bl      TYPE c LENGTH 4              , "DIAS REC X BL
    dias_rec_docs    TYPE c LENGTH 4              , "DIAS REC X BL

    data_eta         TYPE vbak-erdat,
    tamanho_fardo    TYPE zsdt0045-tamanho_fardo,
    tipo             TYPE zsdt0166-tipo,
    dmbtr            TYPE zsdt0045-dmbtr,
    fazenda          TYPE lfa1-name1,
    safra            TYPE zsdt0045-safra,
    matnr            TYPE zsdt0045-matnr,
    werks            TYPE zsdt0045-werks,
    ponto_c          TYPE zsdt0045-ponto_c,
    ponto_c_d        TYPE lfa1-name1,
    cellcolor        TYPE lvc_t_scol,
  END OF ty_0045,

  BEGIN OF ty_saida,
    icon                TYPE c LENGTH 4,
    status              TYPE c LENGTH 1,
    zseq_inst           TYPE zsdt0045-zseq_inst,
    objek               TYPE zsdt0045-objek,
    objecttable         TYPE zsdt0045-objecttable,
    bukrs               TYPE zsdt0045-bukrs,
    werks               TYPE zsdt0045-werks,
    contrato            TYPE zsdt0045-contrato,

    observacao_ico      TYPE c LENGTH 5,            "OBSERVAÇÕES
    embarque            TYPE zsdt0236-embarque,     "EMBARQUE

    list_contrato       TYPE char255,
    instrucao           TYPE zsdt0045-instrucao,

    instrucao_ant       TYPE zsdt0053-instrucao_ant, "INSTRUÇÃO ANTERIOR

    data_instr          TYPE zsdt0045-data_instr,
    data_retirada       TYPE zsdt0045-data_retirada,
    status_retirada     TYPE c LENGTH 4,
    data_porto          TYPE zsdt0045-data_porto,
    status_porto        TYPE c LENGTH 4,
    deadline_draft      TYPE zsdt0045-deadline_draft,
    status_draft        TYPE c LENGTH 4,
    deadline_documen    TYPE zsdt0045-deadline_documen,
    status_docum        TYPE c LENGTH 4,
    porto_embarque      TYPE zsdt0045-porto_embarque,
    safra               TYPE zsdt0045-safra,
    quantidade          TYPE p LENGTH 16,
    qtd_ctners          TYPE p LENGTH 16,
    voleh               TYPE zsdt0045-voleh,
    sep(1),
    data_in_porto       TYPE zsdt0045-data_in_porto,
    status_in_porto     TYPE c LENGTH 4,
    data_eta            TYPE zsdt0045-data_eta,
    qtd_fardos_exp      TYPE zsdt0053-volum,         "QTD FARDOS EXPORTADOS
    saldo_fardos        TYPE zsdt0053-volum,         "SALDO FARDOS
    nfe_fl              TYPE c LENGTH '5', "NFe F.L.
    name1               TYPE kna1-name1,
    charg               TYPE zsdt0045-charg,
    tipo                TYPE zsdt0166-tipo,
    btgew               TYPE zsdt0045-btgew,
    peso_total_exp      TYPE zsdt0053-zmeng,         "PESO TOTAL EXPORTADO
    saldo_peso          TYPE zsdt0053-zmeng,         "SALDO PESO
    algodoeira          TYPE zsdt0166-algodoeira,
    tamanho_fardo       TYPE zsdt0166-tamanho_fardo,
    dmbtr               TYPE zsdt0045-dmbtr,
    data_container      TYPE zsdt0045-data_container,
    terminal            TYPE zsdt0045-terminal,
    terminal_desc       TYPE kna1-name1,
    ponto_c             TYPE zsdt0045-ponto_c,
    ponto_c_desc        TYPE kna1-name1,
    cod_despach         TYPE zsdt0045-cod_despach,
    cod_despach_desc    TYPE kna1-name1,
    cod_transp          TYPE zsdt0045-cod_transp,
    cod_transp_desc     TYPE kna1-name1,
    terminal_estuf      TYPE zsdt0045-terminal_estuf,
    terminal_estuf_desc TYPE kna1-name1,
    controladora        TYPE zsdt0045-controladora,
    controladora_desc   TYPE kna1-name1,

    booking             TYPE zsdt0045-booking,
    mapa                TYPE zsdt0045-mapa,
    fumigacao           TYPE zsdt0045-fumigacao,
    hrs_fgacao          TYPE zsdt0045-hrs_fgacao,
    armador             TYPE zsdt0045-armador,
    free_time           TYPE zsdt0045-free_time,
*    QTD_CTNERS          TYPE ZSDT0045-QTD_CTNERS,
    vlr_frete           TYPE zsdt0045-vlr_frete,
    pais_des            TYPE zsdt0045-pais_des,
    pais_ext            TYPE landx,
    navio               TYPE zsdt0045-navio,
    previsao_receb      TYPE vbak-erdat,

    dt_docs_completo    TYPE zsdt0236-dt_docs_completo, "DT. DOCS COMPLETOS
    dt_recebimento      TYPE zsdt0236-dt_recebimento, "DT. RECEBIMENTO
    dt_apresentacao     TYPE zsdt0236-dt_apresentacao, "DT. APRESENTAÇÃO

    dias_rec_bl         TYPE c LENGTH 4              , "DIAS REC X BL
    dias_rec_docs       TYPE c LENGTH 4              , "DIAS REC X BL

    dias_receb          TYPE p LENGTH 16,
    data_pagto          TYPE vbak-erdat,
  END OF ty_saida.

TYPES:
  BEGIN OF ty_controle,
    seq(3),
    zseq_inst TYPE zsdt0045-zseq_inst,
    objek     TYPE zsdt0045-objek,
    status    TYPE zsdt0045-status,
  END OF ty_controle.

TYPES: BEGIN OF ty_zsdt0053_col,
         instrucao TYPE zsdt0053-instrucao,
*         vbeln     TYPE zsdt0053-vbeln,
         zmeng     TYPE zsdt0053-zmeng,
         volum     TYPE zsdt0053-volum,
       END OF ty_zsdt0053_col.

TYPES: BEGIN OF ty_zsdt0053_inst,
         nro_sol_ov    TYPE zsdt0053-nro_sol_ov,
         posnr         TYPE zsdt0053-posnr,
         zmeng         TYPE zsdt0053-zmeng,
         volum         TYPE zsdt0053-volum,
         vbeln         TYPE zsdt0053-vbeln,
         instrucao     TYPE zsdt0053-instrucao,
         instrucao_ant TYPE zsdt0053-instrucao_ant,
       END OF ty_zsdt0053_inst.

TYPES: BEGIN OF ty_zsdt0053,
         nro_sol_ov    TYPE zsdt0053-nro_sol_ov,
         posnr         TYPE zsdt0053-posnr,
         zmeng         TYPE zsdt0053-zmeng,
         volum         TYPE zsdt0053-volum,
         vbeln         TYPE zsdt0053-vbeln,
         instrucao     TYPE zsdt0053-instrucao,
         instrucao_ant TYPE zsdt0053-instrucao_ant,
       END OF ty_zsdt0053.

DATA: it_controle TYPE TABLE OF ty_controle,
      wa_controle TYPE ty_controle.

*&--------------------------------------------------------------------&*
*& Variaveis
*&--------------------------------------------------------------------&*
DATA: edit  TYPE c,
      inser TYPE c,
      seq   TYPE numc10.

DATA : ty_toolbar TYPE stb_button.


DATA: l_dt_dif TYPE c.

*&--------------------------------------------------------------------&*
*& Internal Table
*&--------------------------------------------------------------------&*
DATA: it_t001          TYPE TABLE OF ty_t001,
      it_t001w         TYPE TABLE OF ty_t001w,
      it_zsdt0045      TYPE TABLE OF ty_zsdt0045,
      it_0045          TYPE TABLE OF ty_0045,
      it_contrato      TYPE TABLE OF ty_0045,
      r_0045           TYPE RANGE OF zsded030,
      it_fcat          TYPE lvc_t_fcat,
      it_0236          TYPE TABLE OF zsdt0236,
      it_0045_aux      TYPE TABLE OF ty_0045,
      it_color         TYPE lvc_t_scol,
*      IT_STATUS   TYPE TABLE OF ZSDT0045,

      it_status        TYPE TABLE OF zsdt0045,
      "IT_EDICAO   TYPE TABLE OF TY_EDICAO,
      it_saida         TYPE TABLE OF ty_saida,
      it_saida_aux     TYPE TABLE OF ty_saida,

      it_zsdt0053_col  TYPE TABLE OF ty_zsdt0053_col,
      it_zsdt0053_inst TYPE TABLE OF ty_zsdt0053_inst,
      it_zsdt0053      TYPE TABLE OF ty_zsdt0053.

DATA: tg_texto TYPE TABLE OF tline WITH HEADER LINE,
      wg_texto TYPE tline.

DATA: _str   TYPE REF TO data,
      campo  TYPE char20,
      vl_cor TYPE n.

DATA: vg_ok_0110         TYPE sy-ucomm.

FIELD-SYMBOLS: <fs_campo>  TYPE any,
               <fs_campo1> TYPE any.


*&--------------------------------------------------------------------&*
*& Work Area
*&--------------------------------------------------------------------&*
DATA: wa_t001     TYPE ty_t001,
      wa_t001w    TYPE ty_t001w,
      wa_zsdt0045 TYPE ty_zsdt0045,
      wa_status   TYPE zsdt0045,
      "WA_EDICAO   TYPE TY_EDICAO,
      wa_saida    TYPE ty_saida,
      r_seq       TYPE RANGE OF zsdt0045-zseq_inst,
      block       TYPE char1,
      wa_seq      LIKE LINE OF r_seq.

DATA currency_field TYPE vbap-waerk.

*&--------------------------------------------------------------------&*
*& Constants
*&--------------------------------------------------------------------&*
CONSTANTS:
  tela_001(4) TYPE c VALUE '0100',
  tela_003(4) TYPE c VALUE '0300'.

*&--------------------------------------------------------------------&*
*& GUI
*&--------------------------------------------------------------------&*
DATA: cl_custom       TYPE REF TO cl_gui_custom_container,
      grid_principal  TYPE REF TO cl_gui_alv_grid,
*      CL_CUSTOM1      TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
*      GRID_PRINCIPAL TYPE REF TO CL_GUI_ALV_GRID,
      wa_fieldcatalog TYPE lvc_s_fcat,
      it_fieldcatalog TYPE lvc_t_fcat,
      gs_variant_c    TYPE disvariant,
      gs_variant_r    TYPE disvariant,
      wa_layout       TYPE lvc_s_layo,
      wa_stable       TYPE lvc_s_stbl VALUE 'XX'.

TYPES: BEGIN OF ty_nfe_fl,
         docnum      TYPE j_1bnfdoc-docnum,
         nfenum      TYPE     j_1bnfdoc-nfenum,
         docdat      TYPE    j_1bnfdoc-docdat,
         menge       TYPE   j_1bnflin-menge,
         branch      TYPE   j_1bnfdoc-branch,
         placa_cav   TYPE  zlest0039-placa_cav,
         datachegada TYPE   zlest0039-datachegada,
       END OF ty_nfe_fl.

DATA: it_nfe_fl TYPE TABLE OF ty_nfe_fl.

*----------------------------------------------------------------------*
* Grid
*----------------------------------------------------------------------*
DATA: v_grid_110 TYPE REF TO cl_gui_alv_grid.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*       Definição da classe lcl_event_receiver
*----------------------------------------------------------------------*
CLASS lcl_event_receiver_110 DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object
                  e_interactive.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*       Implementação da classe lcl_event_receiver
*----------------------------------------------------------------------*
CLASS lcl_event_receiver_110 IMPLEMENTATION.

  METHOD handle_toolbar.
    PERFORM elimina_botoes_nfe_fl  USING e_object.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
* Declaração de Instância de Métodos
*&---------------------------------------------------------------------*
DATA: v_event_receiver_110 TYPE REF TO lcl_event_receiver_110.

*&--------------------------------------------------------------------&*
*& SELEÇÃO
*&--------------------------------------------------------------------&*
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-004.
  PARAMETERS: rb1 RADIOBUTTON GROUP rad1 USER-COMMAND gb2  DEFAULT 'X',
              rb2 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: p_bukrs  FOR zsdt0045-bukrs MODIF ID b,
                  p_werks  FOR zsdt0045-werks MODIF ID a,
                  p_safra  FOR zsdt0045-safra MODIF ID a,
                  p_contr  FOR zsdt0045-contrato MODIF ID b,
                  p_instr  FOR zsdt0045-instrucao NO INTERVALS MODIF ID b,
                  p_booki  FOR zsdt0045-booking NO INTERVALS MODIF ID b,
                  dt_instr FOR vbak-erdat MODIF ID b,
                  dt_retir FOR vbak-erdat MODIF ID b,
                  dt_porto FOR vbak-erdat MODIF ID b,
                  dt_deadd FOR vbak-erdat MODIF ID b,
                  dt_dedoc FOR vbak-erdat MODIF ID b.

SELECTION-SCREEN: END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-s02.
  PARAMETERS: p_varia LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.
  PERFORM variant_f4 CHANGING p_varia.

*&---------------------------------------------------------------------*
*& Form VARIANT_F4
*&---------------------------------------------------------------------*
FORM variant_f4 CHANGING pv_varnt TYPE slis_vari.
  DATA :ls_varnt TYPE disvariant.


  ls_varnt-report = sy-repid.

  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      is_variant    = ls_varnt
      i_save        = 'A'
    IMPORTING
      es_variant    = ls_varnt
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    pv_varnt = ls_varnt-variant.
  ENDIF.

ENDFORM. " VARIANT_F4


AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF rb1 IS NOT INITIAL.
      IF screen-name EQ 'P_BUKRS-LOW'.
        screen-required = 2.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ELSE.
      IF screen-group1 EQ 'A'.
        IF rb2 IS NOT INITIAL.
          screen-active = '0'.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.
      ELSE.
*        IF screen-name EQ 'DT_INSTR-LOW'.
*          screen-required = 2.
*          MODIFY SCREEN.
*          CONTINUE.
*        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

*&--------------------------------------------------------------------&*
*& Telas
*&--------------------------------------------------------------------&*
START-OF-SELECTION.

  IF ( p_contr IS INITIAL AND p_booki IS INITIAL AND  p_instr IS INITIAL ).

    IF dt_retir IS INITIAL AND dt_porto IS INITIAL AND dt_deadd IS INITIAL
        AND dt_dedoc IS INITIAL AND dt_instr IS INITIAL.
      MESSAGE |Por Favor, preencher um dos campos de data!| TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

  ENDIF.

  IF rb1 IS INITIAL.
*    IF ( dt_instr IS INITIAL ).
*      MESSAGE |Por Favor, informe a data da Instrução!| TYPE 'S' DISPLAY LIKE 'E'.
*      EXIT.
*    ENDIF.
  ELSE.
    IF ( p_bukrs IS INITIAL ).
      MESSAGE |Por Favor, informe a empresa!| TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.
  ENDIF.

  PERFORM: seleciona_dados.

  IF rb1 IS INITIAL.
    PERFORM enqueue_ezsdt0053.
    PERFORM: agrupa_dados.
  ENDIF.
  PERFORM: display_fieldcatalog.
  PERFORM: create_object.

  CALL SCREEN: tela_001.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN: 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
    WHEN: 'CREATE'.
      IF ( edit EQ 'X' ).
        PERFORM: update_instrucao.
      ELSE.
        "CLEAR: WA_EDICAO.
        PERFORM: tela_edicao.
      ENDIF.
    WHEN 'SAVE'.
      PERFORM salvar_status.

    WHEN 'REFRESH'.
      PERFORM: seleciona_dados.
      IF rb1 IS INITIAL.
        PERFORM: agrupa_dados.
      ENDIF.
      PERFORM: display_fieldcatalog.
      PERFORM: create_object.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      CLASS DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no,
      handle_user_command  FOR EVENT user_command  OF cl_gui_alv_grid IMPORTING e_ucomm,
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells,
      handle_toolbar       FOR EVENT toolbar       OF cl_gui_alv_grid IMPORTING  e_object,
      on_but_clk            FOR EVENT button_click  OF cl_gui_alv_grid IMPORTING es_col_id es_row_no.
ENDCLASS.                    "lcv_event_handler DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcv_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING e_row_id e_column_id es_row_no.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_toolbar.

    FREE: ty_toolbar.
    DEFINE toobar.
      ty_toolbar-icon      = &1.
      ty_toolbar-function  = &2.
      ty_toolbar-quickinfo = &3.
      ty_toolbar-text      = &4.
      ty_toolbar-butn_type = &5.
      ty_toolbar-disabled = &6.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    END-OF-DEFINITION.

    IF rb1 IS NOT INITIAL.
      toobar:
        ''     ''      ''              ''              3 '',
        '@3J@' 'STATUS' 'Mudar Status' 'Mudar Status'  0 '',
        ''     ''      ''              ''              3 ''.
    ELSE.
      toobar:
        ''     ''      ''              ''              3 block,
        '@3J@' 'EMAIL' 'Gerar Documento' 'Gerar Documento'  0 block,
        ''     ''      ''              ''              3 block.
    ENDIF.


  ENDMETHOD.                    "ON_TOOLBAR_INS

  METHOD handle_user_command.

    DATA: it_sel_rows TYPE lvc_t_row,
          wa_sel_rows TYPE lvc_s_row,
          linhas      TYPE TABLE OF zsdt0045.

    FIELD-SYMBOLS <saida> TYPE ty_saida.

    DATA: p_resp.

*    FREE: IT_SAIDA_AUX."IT_STATUS, .

    CASE e_ucomm.
      WHEN 'STATUS'.

        CALL METHOD grid_principal->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        CHECK NOT it_sel_rows IS INITIAL.

        LOOP AT it_sel_rows INTO wa_sel_rows.
          READ TABLE it_saida ASSIGNING <saida> INDEX wa_sel_rows-index.
          APPEND <saida> TO it_saida_aux.

          CASE <saida>-status.
            WHEN 'L'.
              MOVE '@Q3@' TO <saida>-icon.
              MOVE ''    TO <saida>-status.
            WHEN ''.
              MOVE '@5Y@' TO <saida>-icon.
              MOVE 'L'    TO <saida>-status.
          ENDCASE.

          IF it_status IS INITIAL.
            MOVE-CORRESPONDING <saida> TO wa_status.
            APPEND wa_status TO it_status.
            CLEAR: wa_status.
          ELSE.
            READ TABLE it_status TRANSPORTING NO FIELDS WITH KEY status = <saida>-status.
            IF sy-subrc IS INITIAL.
              MOVE-CORRESPONDING <saida> TO wa_status.
              APPEND wa_status TO it_status.
              CLEAR: wa_status.
            ELSE.
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-002 TEXT-003.

              CASE <saida>-status.
                WHEN 'L'.
                  MOVE '@Q3@' TO <saida>-icon.
                  MOVE ''    TO <saida>-status.
                WHEN ''.
                  MOVE '@5Y@' TO <saida>-icon.
                  MOVE 'L'    TO <saida>-status.
              ENDCASE.

            ENDIF.
          ENDIF.
        ENDLOOP.

        IF <saida>-status EQ ''.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              text_question         = 'Deseja retornar para o Status de Liberada?'
              text_button_1         = 'Sim'
              text_button_2         = 'Não'
              display_cancel_button = ' '
            IMPORTING
              answer                = p_resp.

          IF p_resp EQ 2.
            FREE: it_status.
            MOVE it_saida_aux TO it_saida.
            FREE it_saida_aux.
            CHECK p_resp EQ 1.
          ENDIF.
        ENDIF.

      WHEN 'EMAIL'.

        CALL METHOD grid_principal->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        CHECK NOT it_sel_rows IS INITIAL.

        LOOP AT it_sel_rows INTO wa_sel_rows.
          READ TABLE it_0045_aux ASSIGNING FIELD-SYMBOL(<w_0045>) INDEX wa_sel_rows-index.
          PERFORM gera_doc CHANGING <w_0045>.
        ENDLOOP.

        PERFORM enqueue_ezsdt0053.

    ENDCASE.

    CALL METHOD grid_principal->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "HANDLE_USER_COMMAND

  METHOD on_data_changed_finished.
    CHECK e_modified IS NOT INITIAL.
    CALL METHOD grid_principal->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: lv_datediff TYPE  p,
          lv_timediff TYPE  p,
          lv_earliest TYPE  c.

    FIELD-SYMBOLS: <fs_value> TYPE any.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_good)
      WHERE fieldname EQ 'CARGA'         OR
            fieldname EQ 'ESTUFAGEM'     OR
            fieldname EQ 'PESAGEM'       OR
            fieldname EQ 'PLANILHA_PESO' OR
            fieldname EQ 'NF_EXPORT'     OR
            fieldname EQ 'LIBERACAO'     OR
            fieldname EQ 'DEPOSITO'.

      DATA(vl_instrucao) = it_0045_aux[ ls_good-row_id ]-instrucao.

      SELECT SINGLE *
        FROM zsdt0236
        INTO @DATA(w_0236)
      WHERE instrucao EQ @vl_instrucao.

      IF sy-subrc IS INITIAL.
        DATA(w_0236_) = w_0236.

        w_0236 =
        VALUE #(
                 instrucao     = w_0236_-instrucao
                 id_prog_embq  = w_0236_-id_prog_embq
                 carga         = COND #( WHEN ls_good-fieldname EQ 'CARGA'         THEN ls_good-value ELSE w_0236_-carga )
                 estufagem     = COND #( WHEN ls_good-fieldname EQ 'ESTUFAGEM'     THEN ls_good-value ELSE w_0236_-estufagem )
                 pesagem       = COND #( WHEN ls_good-fieldname EQ 'PESAGEM'       THEN ls_good-value ELSE w_0236_-pesagem )
                 planilha_peso = COND #( WHEN ls_good-fieldname EQ 'PLANILHA_PESO' THEN ls_good-value ELSE w_0236_-planilha_peso )
                 nf_export     = COND #( WHEN ls_good-fieldname EQ 'NF_EXPORT'     THEN ls_good-value ELSE w_0236_-nf_export )
                 liberacao     = COND #( WHEN ls_good-fieldname EQ 'LIBERACAO'     THEN ls_good-value ELSE w_0236_-liberacao )
                 deposito      = COND #( WHEN ls_good-fieldname EQ 'DEPOSITO'      THEN ls_good-value ELSE w_0236_-deposito )

                ).
      ELSE.

        w_0236 =
        VALUE #(
                 instrucao     = vl_instrucao
                 carga         = COND #( WHEN ls_good-fieldname EQ 'CARGA' THEN ls_good-value  )
                 estufagem     = COND #( WHEN ls_good-fieldname EQ 'ESTUFAGEM' THEN ls_good-value  )
                 pesagem       = COND #( WHEN ls_good-fieldname EQ 'PESAGEM' THEN ls_good-value  )
                 planilha_peso = COND #( WHEN ls_good-fieldname EQ 'PLANILHA_PESO' THEN ls_good-value  )
                 nf_export     = COND #( WHEN ls_good-fieldname EQ 'NF_EXPORT' THEN ls_good-value  )
                 liberacao     = COND #( WHEN ls_good-fieldname EQ 'LIBERACAO' THEN ls_good-value  )
                 deposito      = COND #( WHEN ls_good-fieldname EQ 'DEPOSITO' THEN ls_good-value  )

                ).

      ENDIF.

      MODIFY zsdt0236 FROM w_0236.
      COMMIT WORK.

    ENDLOOP.

    CLEAR l_dt_dif.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good
          WHERE fieldname EQ 'EMBARQUE'         OR
                fieldname EQ 'DT_DOCS_COMPLETO' OR
                fieldname EQ 'DT_RECEBIMENTO'   OR
                fieldname EQ 'DT_APRESENTACAO'  OR
                fieldname EQ 'OBSERVACAO'.

      READ TABLE it_0045_aux ASSIGNING FIELD-SYMBOL(<fs_0045_aux>) INDEX ls_good-row_id.

      CHECK <fs_0045_aux> IS ASSIGNED.

      ASSIGN COMPONENT ls_good-fieldname OF STRUCTURE <fs_0045_aux> TO <fs_value>.
      CHECK <fs_value> IS ASSIGNED.
      <fs_value>      = ls_good-value.

      IF <fs_0045_aux>-dt_docs_completo IS NOT INITIAL.

        CLEAR: lv_datediff, lv_timediff, lv_earliest.

        CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
          EXPORTING
            date1            = <fs_0045_aux>-dt_docs_completo
            date2            = <fs_0045_aux>-dt_recebimento
          IMPORTING
            datediff         = lv_datediff
            timediff         = lv_timediff
            earliest         = lv_earliest
          EXCEPTIONS
            invalid_datetime = 1
            OTHERS           = 2.
        IF sy-subrc = 0.
          <fs_0045_aux>-dias_rec_docs =  lv_datediff.
        ENDIF.

      ENDIF.

      IF <fs_0045_aux>-data_bl IS NOT INITIAL.

        CLEAR: lv_datediff, lv_timediff, lv_earliest.

        CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
          EXPORTING
            date1            = <fs_0045_aux>-data_bl
            date2            = <fs_0045_aux>-dt_recebimento
          IMPORTING
            datediff         = lv_datediff
            timediff         = lv_timediff
            earliest         = lv_earliest
          EXCEPTIONS
            invalid_datetime = 1
            OTHERS           = 2.
        IF sy-subrc = 0.
          <fs_0045_aux>-dias_rec_bl =  lv_datediff.
        ENDIF.

      ENDIF.

      IF <fs_0045_aux>-dt_docs_completo IS NOT INITIAL AND <fs_0045_aux>-dt_recebimento IS NOT INITIAL
        AND <fs_0045_aux>-dt_docs_completo(4) <>  <fs_0045_aux>-dt_recebimento(4).
        l_dt_dif = abap_true.
      ENDIF.

      IF <fs_0045_aux>-dt_docs_completo IS NOT INITIAL AND <fs_0045_aux>-dt_apresentacao IS NOT INITIAL
       AND <fs_0045_aux>-dt_docs_completo(4) <> <fs_0045_aux>-dt_apresentacao(4).
        l_dt_dif = abap_true.
      ENDIF.

      IF <fs_0045_aux>-dt_recebimento IS NOT INITIAL AND <fs_0045_aux>-dt_apresentacao IS NOT INITIAL
        AND <fs_0045_aux>-dt_recebimento(4) <> <fs_0045_aux>-dt_apresentacao(4).
        l_dt_dif = abap_true.
      ENDIF.

      IF <fs_0045_aux>-observacao IS INITIAL.
        MOVE icon_display_more TO <fs_0045_aux>-observacao_ico.
      ELSE.
        MOVE icon_enter_more TO <fs_0045_aux>-observacao_ico.
      ENDIF.

      IF l_dt_dif IS NOT INITIAL.
        er_data_changed->add_protocol_entry( EXPORTING i_msgid     = 'Z_LES'
                                                       i_msgty     = 'W'
                                                       i_msgno     = '000'
                                                       i_msgv1     = 'Data com exercício divergente!'
                                                       i_fieldname = ls_good-fieldname ).
      ENDIF.

    ENDLOOP.

    CHECK sy-subrc IS INITIAL AND vl_instrucao IS NOT INITIAL.
    PERFORM cor_grid USING vl_instrucao.

  ENDMETHOD.

  METHOD on_but_clk.

    TYPES: BEGIN OF y_vbfa_aux,
             vbelv   TYPE vbfa-vbelv,
             posnv   TYPE vbfa-posnv,
             vbeln   TYPE j_1bnflin-refkey,
             posnn   TYPE vbfa-posnn,
             vbtyp_n TYPE  vbfa-vbtyp_n,
             vbtyp_v TYPE vbfa-vbtyp_v,
           END OF  y_vbfa_aux.

    DATA: it_vbfa_aux TYPE TABLE OF y_vbfa_aux,
          w_nfe_fl    LIKE LINE OF it_nfe_fl.

    DATA: tl_texto TYPE catsxt_longtext_itab,
          wl_texto TYPE LINE OF catsxt_longtext_itab,
          wl_field TYPE lvc_s_col,
          wl_name  TYPE thead-tdname,
          title    TYPE sytitle,
          v_cont   TYPE i.

    DATA: ls_sel_hide TYPE slis_sel_hide_alv,
          is_table    TYPE lvc_s_stbl.

    FREE: tl_texto, tg_texto.
    CLEAR:wl_texto.

    READ TABLE it_0045_aux ASSIGNING FIELD-SYMBOL(<fs_0045_aux>) INDEX es_row_no-row_id.

    CASE es_col_id.
      WHEN 'OBSERVACAO_ICO'.

        title = 'Observaçôes'.
        PERFORM read_text CHANGING <fs_0045_aux>.

        IF <fs_0045_aux>-observacao IS INITIAL.
          MOVE icon_enter_more TO <fs_0045_aux>-observacao_ico.
        ELSE.
          MOVE icon_display_more TO <fs_0045_aux>-observacao_ico.
        ENDIF.

      WHEN 'NFE_FL'.

        REFRESH: it_nfe_fl.

        "Tabela de Solicitação de Ordem de Venda – Formação de Lote
        SELECT nro_sol_ov, posnr, instrucao, vbeln, status FROM zsdt0066
          INTO TABLE @DATA(it_zsdt0066)
          WHERE instrucao  = @<fs_0045_aux>-instrucao
            AND vbeln     <> @space
            AND status    = 'L'.

        IF it_zsdt0066[] IS NOT INITIAL.

          "Fluxo de documentos de vendas e distribuição
          SELECT vbelv, posnv, vbeln, posnn, vbtyp_n, vbtyp_v FROM vbfa
            INTO TABLE @DATA(it_vbfa)
            FOR ALL ENTRIES IN @it_zsdt0066
            WHERE vbelv = @it_zsdt0066-vbeln
              AND vbtyp_n       = 'M'
              AND vbtyp_v       = 'C'.

          IF it_vbfa[] IS NOT INITIAL.

            MOVE-CORRESPONDING it_vbfa TO it_vbfa_aux EXPANDING NESTED TABLES
                                                  KEEPING TARGET LINES.

            SELECT docnum, itmnum, menge, refkey, refitm FROM  j_1bnflin
              INTO TABLE @DATA(it_lin)
              FOR ALL ENTRIES IN @it_vbfa_aux
              WHERE refkey = @it_vbfa_aux-vbeln.

          ENDIF.

          IF it_lin[] IS NOT INITIAL.

            SELECT docnum, docdat, cancel, branch, nfenum, candat FROM  j_1bnfdoc
              INTO TABLE @DATA(it_doc)
              FOR ALL ENTRIES IN @it_lin
              WHERE docnum = @it_lin-docnum
               AND cancel  = @space
               AND candat = '00000000'.

          ENDIF.

          IF it_doc[] IS NOT INITIAL.

            "Electronic Nota Fiscal: Actual Status
            SELECT docnum, docsta, scssta  FROM j_1bnfe_active
              INTO TABLE @DATA(it_active)
              FOR ALL ENTRIES IN @it_doc
              WHERE docnum = @it_doc-docnum
              AND docsta = 1
              AND scssta <> 2.

          ENDIF.

          IF it_active[] IS NOT INITIAL.

            "Comparativo de saidas e chegadas
            SELECT docnum, placa_cav, datatransb, datachegada FROM zlest0039
              INTO TABLE @DATA(it_zlest0039)
              FOR ALL ENTRIES IN @it_active
                    WHERE docnum = @it_active-docnum.


            LOOP AT it_active INTO DATA(w_active).

              w_nfe_fl-docnum = w_active-docnum.

              READ TABLE it_doc INTO DATA(w_doc) WITH KEY docnum = w_active-docnum.
              IF sy-subrc IS INITIAL.
                w_nfe_fl-nfenum = w_doc-nfenum.
                w_nfe_fl-docdat = w_doc-docdat.
                w_nfe_fl-branch = w_doc-branch.
              ENDIF.

              READ TABLE it_lin INTO DATA(w_lin) WITH KEY docnum = w_active-docnum.
              IF sy-subrc IS INITIAL.
                w_nfe_fl-menge = w_lin-menge.
              ENDIF.

              READ TABLE it_zlest0039 INTO DATA(w_zlest0039) WITH KEY docnum = w_active-docnum.
              IF sy-subrc IS INITIAL.
                w_nfe_fl-placa_cav = w_zlest0039-placa_cav.
              ENDIF.

              IF w_zlest0039-datatransb <> space.
                w_nfe_fl-datachegada = w_zlest0039-datatransb.
                APPEND w_nfe_fl TO it_nfe_fl.
              ELSE.
                w_nfe_fl-datachegada = w_zlest0039-datachegada.
                APPEND w_nfe_fl TO it_nfe_fl.

              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDIF.

        IF it_nfe_fl[] IS NOT INITIAL.
          CALL SCREEN 0110 STARTING AT 02 02 ENDING AT 118 15.
        ELSE.
          MESSAGE s000(z_les) WITH 'Nenhuma nota encontrada!'.
          RETURN.
        ENDIF.

      WHEN OTHERS.

    ENDCASE.

    CALL METHOD grid_principal->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

ENDCLASS.                    "lcv_event_handler IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM handle_hotspot_click  USING   i_row_id     TYPE lvc_s_row
                                   i_column_id  TYPE lvc_s_col
                                   is_row_no    TYPE lvc_s_roid.
  CASE i_column_id.
    WHEN: 'EDIT'. PERFORM: editar_instrucao USING is_row_no-row_id.
    WHEN: 'EXCLUIR'. PERFORM: excluir_instrucao USING is_row_no-row_id.
    WHEN: 'STATUS'. PERFORM: bloquear_instrucao USING is_row_no-row_id.
  ENDCASE.
ENDFORM.                    " HANDLE_HOTSPOT_CLICK


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados .

  DATA: w_zsdt0053_col LIKE LINE OF it_zsdt0053_col.

  REFRESH: it_zsdt0053_col.

  IF rb1 IS NOT INITIAL.
    FREE: it_saida, it_t001, it_t001w, it_zsdt0045.

    SELECT bukrs butxt ort01
      FROM t001
      INTO TABLE it_t001
     WHERE bukrs IN p_bukrs.

    CHECK NOT it_t001[] IS INITIAL.

    IF p_werks[] IS NOT INITIAL.
      SELECT werks name1
        FROM t001w
        INTO TABLE it_t001w
      WHERE werks IN p_werks.

      CHECK NOT it_t001w[] IS INITIAL.
    ENDIF.

    SELECT *
      FROM zsdt0045
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0045
    WHERE bukrs            IN p_bukrs
      AND werks            IN p_werks
      AND safra            IN p_safra
      AND instrucao        IN p_instr
      AND contrato         IN p_contr
      AND booking          IN p_booki
      AND data_instr       IN dt_instr
      AND data_retirada    IN dt_retir
      AND data_porto       IN dt_porto
      AND deadline_draft   IN dt_deadd
      AND deadline_documen IN dt_dedoc.

    LOOP AT it_zsdt0045 INTO wa_zsdt0045.

      MOVE-CORRESPONDING wa_zsdt0045 TO wa_saida.

      wa_saida-quantidade = CONV #( wa_zsdt0045-quantidade ).
      wa_saida-qtd_ctners = CONV #( wa_zsdt0045-qtd_ctners ).

      wa_saida-icon = SWITCH #( wa_saida-status WHEN ''  THEN '@Q3@'
                                                WHEN 'L' THEN '@5Y@'
                                                WHEN 'A' THEN '@K4@'
                                                WHEN 'F' THEN '@DF@'
                              ).

      PERFORM status USING 1 CHANGING wa_saida-status_retirada wa_saida-data_retirada.
      PERFORM status USING 1 CHANGING wa_saida-status_porto    wa_saida-data_porto.
      PERFORM status USING 1 CHANGING wa_saida-status_in_porto wa_saida-data_in_porto.
      PERFORM status USING 2 CHANGING wa_saida-status_draft    wa_saida-deadline_draft.
      PERFORM status USING 2 CHANGING wa_saida-status_docum    wa_saida-deadline_documen.

      PERFORM lfa1 USING wa_zsdt0045-terminal CHANGING wa_saida-terminal_desc.
      PERFORM lfa1 USING wa_zsdt0045-ponto_c CHANGING wa_saida-ponto_c_desc.
      PERFORM lfa1 USING wa_zsdt0045-cod_despach CHANGING wa_saida-cod_despach_desc.
      PERFORM lfa1 USING wa_zsdt0045-cod_transp CHANGING wa_saida-cod_transp_desc.
      PERFORM lfa1 USING wa_zsdt0045-terminal_estuf CHANGING wa_saida-terminal_estuf_desc.
      PERFORM lfa1 USING wa_zsdt0045-controladora CHANGING wa_saida-controladora_desc.

      SELECT SINGLE *
         FROM zsdt0166
        INTO @DATA(wa_zsdt0166)
         WHERE id EQ ( SELECT MAX( id )
                              FROM zsdt0166
                          WHERE safra  EQ @wa_saida-safra
                            AND werks  EQ @wa_saida-werks
                            AND lote   EQ @wa_saida-charg
                            AND status EQ 'A'
                      ).

      IF sy-subrc IS INITIAL.
        PERFORM kna1 USING wa_zsdt0166-kunnr CHANGING wa_saida-name1.
        wa_saida-tipo           = wa_zsdt0166-tipo.
        wa_saida-algodoeira     = wa_zsdt0166-algodoeira.
        wa_saida-tamanho_fardo = wa_zsdt0166-tamanho_fardo.
      ELSE.

        SELECT SINGLE *
          FROM zsdt0051
          INTO @DATA(w_0051)
          WHERE nro_sol_ov EQ @wa_saida-objek.

        IF sy-subrc IS INITIAL.
          PERFORM kna1 USING w_0051-kunnr CHANGING wa_saida-name1.
          PERFORM mara USING wa_zsdt0045-matnr CHANGING wa_saida-tipo.
          wa_saida-algodoeira     = ''.
          wa_saida-tamanho_fardo  = wa_zsdt0045-tamanho_fardo.
        ENDIF.

      ENDIF.

      APPEND wa_saida TO it_saida.
      CLEAR: wa_zsdt0045, wa_saida, wa_zsdt0166.

    ENDLOOP.
  ELSE.

    ASSIGN 'ZSDT0236' TO FIELD-SYMBOL(<fs_str>).
    CREATE DATA _str TYPE (<fs_str>).

    it_fcat = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( _str ) ) ) ).

    SELECT
            'I' AS sign,
            'EQ' AS option,
            instrucao AS low
      FROM zsdt0045
      INTO TABLE @r_0045
    WHERE bukrs            IN @p_bukrs
      AND data_instr       IN @dt_instr
      AND instrucao        IN @p_instr
      AND data_retirada    IN @dt_retir
      AND data_porto       IN @dt_porto
      AND deadline_draft   IN @dt_deadd
      AND deadline_documen IN @dt_dedoc.

    SORT r_0045 BY low.
    DELETE ADJACENT DUPLICATES FROM r_0045 COMPARING low.

    CHECK r_0045 IS NOT INITIAL.

    SELECT *
      FROM zsdt0045
      INTO CORRESPONDING FIELDS OF TABLE it_0045
    WHERE instrucao IN r_0045
      ORDER BY instrucao.

    LOOP AT it_0045 ASSIGNING FIELD-SYMBOL(<f0045>).
      SHIFT <f0045>-instrucao LEFT DELETING LEADING ' '.
    ENDLOOP.

    SELECT bukrs butxt ort01
      FROM t001
      INTO TABLE it_t001
          FOR ALL ENTRIES IN it_0045
     WHERE bukrs EQ it_0045-bukrs.

    SELECT *
      FROM zsdt0236
      INTO TABLE it_0236
      FOR ALL ENTRIES IN it_0045
      WHERE instrucao EQ it_0045-instrucao.

  ENDIF.

  IF it_0045[] IS NOT INITIAL.

    "Tabela de Solicitação de ordem de venda - MATERIAIS

    REFRESH: it_zsdt0053, it_zsdt0053_col, it_zsdt0053_inst.

    SELECT nro_sol_ov posnr zmeng volum vbeln instrucao instrucao_ant FROM zsdt0053
      INTO TABLE it_zsdt0053
      FOR ALL ENTRIES IN it_0045
      WHERE instrucao = it_0045-instrucao.

    SORT it_zsdt0053 BY instrucao.

    it_zsdt0053_inst = it_zsdt0053.
    DELETE ADJACENT DUPLICATES FROM it_zsdt0053_inst COMPARING instrucao.
    DELETE  it_zsdt0053_inst WHERE instrucao_ant IS INITIAL.

    LOOP AT it_zsdt0053 INTO DATA(w_zsdt0053).

      IF  w_zsdt0053-vbeln <> space.

*        w_zsdt0053_col-vbeln       = w_zsdt0053-vbeln.
        w_zsdt0053_col-instrucao   = w_zsdt0053-instrucao.
        w_zsdt0053_col-zmeng       = w_zsdt0053-zmeng.
        w_zsdt0053_col-volum       = w_zsdt0053-volum.

        COLLECT w_zsdt0053_col INTO it_zsdt0053_col.
        CLEAR: w_zsdt0053_col.

      ENDIF.

      "preenche inscrição anterior
      READ TABLE it_zsdt0053_inst ASSIGNING FIELD-SYMBOL(<fs_zsdt0053_inst>) WITH KEY instrucao =  w_zsdt0053-instrucao.

      IF <fs_zsdt0053_inst> IS ASSIGNED
                      AND w_zsdt0053-instrucao_ant IS NOT INITIAL
                              AND ( <fs_zsdt0053_inst>-instrucao_ant NS w_zsdt0053-instrucao_ant ).

        IF <fs_zsdt0053_inst>-instrucao_ant IS INITIAL.
          <fs_zsdt0053_inst>-instrucao_ant = w_zsdt0053-instrucao_ant.
        ELSE.
          CONCATENATE <fs_zsdt0053_inst>-instrucao_ant w_zsdt0053-instrucao_ant INTO <fs_zsdt0053_inst>-instrucao_ant SEPARATED BY ','.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'PF0300'.
  SET TITLEBAR  'TB0300'.
ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE sy-ucomm.
    WHEN: 'CANC' OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN: 'OK'.
      IF ( edit EQ 'X' ).
        PERFORM: update_instrucao.
      ELSE.
        PERFORM: inserir_dados.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0300  INPUT

*&---------------------------------------------------------------------*
*&      Form  TELA_EDICAO
*&---------------------------------------------------------------------*
FORM tela_edicao.

  CALL SCREEN tela_003 STARTING AT 15 10 ENDING AT 90 20.

ENDFORM.                    " TELA_EDICAO

*&---------------------------------------------------------------------*
*&      Form  INSERIR_DADOS
*&---------------------------------------------------------------------*
FORM inserir_dados .
*
*  DATA: P_ZID TYPE NUMC10.
*
*  IF ( WA_EDICAO IS INITIAL ).
*    MESSAGE E888(SABAPDOCU) WITH 'Preencher os campos!'.
*  ELSE.
*
*    CALL FUNCTION 'NUMBER_GET_NEXT'
*      EXPORTING
*        NR_RANGE_NR = '01'
*        OBJECT      = 'ZSEQ_INST'
*      IMPORTING
*        NUMBER      = P_ZID.
*
*    IF ( SY-SUBRC EQ 0 ).
*
*      INSER = 'X'.
*
*      WA_SEQ-SIGN   = 'I'.
*      WA_SEQ-OPTION = 'EQ'.
*      WA_SEQ-LOW    = P_ZID.
*
*      APPEND WA_SEQ TO R_SEQ.
*
*      CLEAR: WA_SEQ.
*
*
*      WA_EDICAO-ZSEQ_INST    = P_ZID.
*      WA_EDICAO-STATUS       = SPACE.
*      WA_EDICAO-USUARIO      = SY-UNAME.
*      WA_EDICAO-DATA_CRIACAO = SY-DATUM.
*
*      "INSERT INTO ZSDT0045 VALUES WA_EDICAO.
*
*      CLEAR: IT_SAIDA[], IT_ZSDT0045[], WA_SAIDA, WA_ZSDT0045.
*      PERFORM: SELECIONA_DADOS.
*
*      CALL METHOD GRID_PRINCIPAL->REFRESH_TABLE_DISPLAY
*        EXPORTING
*          IS_STABLE = WA_STABLE.
*
*      LEAVE TO SCREEN 0.
*
*    ENDIF.
*  ENDIF.
ENDFORM.                    " INSERIR_DADOS

*&---------------------------------------------------------------------*
*&      Form  EDITAR_INSTRUCAO
*&---------------------------------------------------------------------*
FORM editar_instrucao  USING    p_is_row_no.


*  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_IS_ROW_NO.
*
*  IF ( SY-SUBRC EQ 0 ).
*
*    IF ( WA_SAIDA-STATUS_T EQ 'B' ).
*      MESSAGE S888(SABAPDOCU) WITH 'Instrução bloqueada, não pode ser editada!' DISPLAY LIKE 'W'.
*    ELSE.
*      CLEAR: WA_EDICAO.
*
*      WA_EDICAO-BUKRS            = WA_SAIDA-BUKRS.
*      WA_EDICAO-WERKS            = WA_SAIDA-WERKS.
*      WA_EDICAO-INSTRUCAO        = WA_SAIDA-INSTRUCAO.
*      WA_EDICAO-CONTRATO         = WA_SAIDA-CONTRATO.
*      WA_EDICAO-DATA_RETIRADA    = WA_SAIDA-DATA_RETIRADA.
*      WA_EDICAO-DEADLINE_DRAFT   = WA_SAIDA-DEADLINE_DRAFT.
*      WA_EDICAO-DEADLINE_DOCUMEN = WA_SAIDA-DEADLINE_DOCUMEN.
*      WA_EDICAO-PORTO_EMBARQUE   = WA_SAIDA-PORTO_EMBARQUE.
*      WA_EDICAO-SAFRA            = WA_SAIDA-SAFRA.
*      WA_EDICAO-ZSEQ_INST        = WA_SAIDA-ZSEQ_INST.
*      WA_EDICAO-QUANTIDADE       = WA_SAIDA-QUANTIDADE.
*
*      EDIT = 'X'.
*
*      PERFORM: TELA_EDICAO.
*
*    ENDIF.
  "ENDIF.

ENDFORM.                    " EDITAR_INSTRUCAO
*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_INSTRUCAO
*&---------------------------------------------------------------------*
FORM excluir_instrucao  USING    p_is_row_no.

  READ TABLE it_saida INTO wa_saida INDEX p_is_row_no.

  IF ( sy-subrc EQ 0 ).

    DELETE FROM zsdt0045 WHERE zseq_inst EQ wa_saida-zseq_inst.

    CLEAR: it_saida[], it_zsdt0045[], wa_saida, wa_zsdt0045.
    PERFORM: seleciona_dados.

    CALL METHOD grid_principal->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    MESSAGE s888(sabapdocu) WITH 'Instrução excluída'.
    LEAVE TO SCREEN tela_001.
  ENDIF.

ENDFORM.                    " EXCLUIR_INSTRUCAO
*&---------------------------------------------------------------------*
*&      Form  UPDATE_INSTRUCAO
*&---------------------------------------------------------------------*
FORM update_instrucao .
*  IF ( EDIT EQ 'X' ).
*
*    UPDATE ZSDT0045 SET BUKRS            = WA_EDICAO-BUKRS
*                        WERKS            = WA_EDICAO-WERKS
*                        INSTRUCAO        = WA_EDICAO-INSTRUCAO
*                        CONTRATO         = WA_EDICAO-CONTRATO
*                        DATA_RETIRADA    = WA_EDICAO-DATA_RETIRADA
*                        DEADLINE_DRAFT   = WA_EDICAO-DEADLINE_DRAFT
*                        DEADLINE_DOCUMEN = WA_EDICAO-DEADLINE_DOCUMEN
*                        PORTO_EMBARQUE   = WA_EDICAO-PORTO_EMBARQUE
*                        SAFRA            = WA_EDICAO-SAFRA
*                        DATA_CRIACAO     = SY-DATUM
*                        USUARIO          = SY-UNAME
*                        QUANTIDADE       = WA_EDICAO-QUANTIDADE
*    WHERE  ZSEQ_INST EQ WA_EDICAO-ZSEQ_INST.
*
*    CLEAR: WA_SAIDA.
*
*    CLEAR: IT_SAIDA[], IT_ZSDT0045[], WA_SAIDA, WA_ZSDT0045.
*    PERFORM: SELECIONA_DADOS.
*
*    CALL METHOD GRID_PRINCIPAL->REFRESH_TABLE_DISPLAY
*      EXPORTING
*        IS_STABLE = WA_STABLE.
*
*
*    LEAVE TO SCREEN 0.
*  ENDIF.
ENDFORM.                    " UPDATE_INSTRUCAO
*&--------------------------------------
*&---------------------------------------------------------------------*
*&      Form  BLOQUEAR_INSTRUCAO
*&---------------------------------------------------------------------*
FORM bloquear_instrucao  USING    p_is_row_no.
*
*  READ TABLE IT_SAIDA INTO WA_SAIDA INDEX P_IS_ROW_NO.
*
*  IF ( SY-SUBRC EQ 0 ).
*
*    IF ( WA_SAIDA-STATUS_T EQ 'B' ).
*
*      UPDATE ZSDT0045 SET STATUS = ''
*                          DATA_CRIACAO = SY-DATUM
*                          USUARIO      = SY-UNAME
*            WHERE  ZSEQ_INST EQ WA_SAIDA-ZSEQ_INST.
*
*      CLEAR: WA_SAIDA.
*
*      CLEAR: IT_SAIDA[], IT_ZSDT0045[], WA_SAIDA, WA_ZSDT0045.
*      PERFORM: SELECIONA_DADOS.
*
*      CALL METHOD GRID_PRINCIPAL->REFRESH_TABLE_DISPLAY
*        EXPORTING
*          IS_STABLE = WA_STABLE.
*
*      MESSAGE S888(SABAPDOCU) WITH 'Instrução desbloqueada'.
*      LEAVE TO SCREEN TELA_001.
*
*    ELSE.
*
*
*      UPDATE ZSDT0045 SET STATUS = 'B'
*                          DATA_CRIACAO = SY-DATUM
*                          USUARIO      = SY-UNAME
*            WHERE  ZSEQ_INST EQ WA_SAIDA-ZSEQ_INST.
*
*      CLEAR: WA_SAIDA.
*
*      CLEAR: IT_SAIDA[], IT_ZSDT0045[], WA_SAIDA, WA_ZSDT0045.
*      PERFORM: SELECIONA_DADOS.
*
*      CALL METHOD GRID_PRINCIPAL->REFRESH_TABLE_DISPLAY
*        EXPORTING
*          IS_STABLE = WA_STABLE.
*
*      MESSAGE S888(SABAPDOCU) WITH 'Instrução bloqueada'.
*
*      LEAVE TO SCREEN TELA_001.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " BLOQUEAR_INSTRUCAO


*&---------------------------------------------------------------------*
*&      Form  VALIDACAO
*&---------------------------------------------------------------------*
FORM validacao .

  IF ( p_bukrs IS INITIAL ).
    MESSAGE |Por Favor, informe a empresa!| TYPE 'S' DISPLAY LIKE 'E'.
*    MESSAGE W888(SABAPDOCU) WITH 'Por favor informe a empresa!'.
  ELSE.
*    IF ( T001W-WERKS IS INITIAL ).
*    MESSAGE W888(SABAPDOCU) WITH 'Por favor informe o centro!'.
  ENDIF.

ENDFORM.                    " VALIDACAO



*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_object.

  DATA: gr_event_handler TYPE REF TO lcl_event_handler,
        it_dropdown      TYPE lvc_t_dral, "lvc_t_drop,
        w_dropdown       LIKE LINE OF it_dropdown.

  IF cl_custom IS INITIAL.

    CREATE OBJECT cl_custom
      EXPORTING
        container_name = 'CONTAINER_EMB'.

    CREATE OBJECT grid_principal
      EXPORTING
        i_parent = cl_custom.

    wa_layout-zebra      = 'X'.
    wa_layout-sel_mode   = 'C'.

    wa_layout-ctab_fname  = 'CELLCOLOR'.

    CREATE OBJECT gr_event_handler.
    SET HANDLER: gr_event_handler->handle_hotspot_click FOR grid_principal,
    gr_event_handler->handle_toolbar       FOR grid_principal,
    gr_event_handler->handle_user_command  FOR grid_principal,
    gr_event_handler->on_but_clk           FOR grid_principal.


    IF rb1 IS NOT INITIAL.

      gs_variant_c-report = sy-repid. "Enable users save own LAYOUTs
      gs_variant_c-variant = p_varia.
      CALL METHOD grid_principal->set_table_for_first_display
        EXPORTING
          is_layout                     = wa_layout
          is_variant                    = gs_variant_c
          i_save                        = 'A'
        CHANGING
          it_outtab                     = it_saida
          it_fieldcatalog               = it_fieldcatalog
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ELSE.

      SET HANDLER: gr_event_handler->on_data_changed          FOR grid_principal,
                   gr_event_handler->on_data_changed_finished FOR grid_principal,
                   gr_event_handler->on_but_clk               FOR grid_principal.

      gs_variant_r-report = sy-repid. "Enable users save own LAYOUTs
      gs_variant_r-variant = p_varia.

      " DD: textos p/valores fixos dom.(depend.idioma)
      SELECT * FROM dd07t
        INTO TABLE @DATA(it_dd07t)
        WHERE domname = 'ZDOSD_EMB'
         AND ddlanguage = @sy-langu(1).

* Drop-Down-Liste definieren und an das ALV-Gitter übergeben
      REFRESH: it_dropdown.
      LOOP AT it_dd07t INTO DATA(w_dd07t).
        w_dropdown-handle = 1.
        w_dropdown-value  = w_dd07t-ddtext.
        w_dropdown-int_value = w_dd07t-ddtext.
        APPEND w_dropdown TO it_dropdown.
      ENDLOOP.

      IF grid_principal IS NOT INITIAL.
*        CALL METHOD grid_principal->set_drop_down_table( it_drop_down = it_dropdown ).
        CALL METHOD grid_principal->set_drop_down_table( it_drop_down_alias = it_dropdown ).
      ENDIF.

      CALL METHOD grid_principal->set_table_for_first_display
        EXPORTING
          is_layout                     = wa_layout
          is_variant                    = gs_variant_r
          i_save                        = 'A'
        CHANGING
          it_outtab                     = it_0045_aux
          it_fieldcatalog               = it_fieldcatalog
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL METHOD grid_principal->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    ENDIF.

  ELSE.
    CALL METHOD grid_principal->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDFORM.                    "validacao

*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
FORM display_fieldcatalog .
  FREE: it_fieldcatalog.

  IF rb1 IS NOT INITIAL.

    PERFORM fieldcatalog USING:
    'ICON'                'Status'                  '04' '' '' '' '' '' '',
    'ZSEQ_INST'           'ID Instrução'            ''   '' '' '' '' '' '',
    'OBJEK'               'Nº Solicitação'          ''   '' '' '' '' '' '',
    'BUKRS'               'Empresa'                 '04' '' '' '' '' '' '',
    'NAME1'               'Desc. Cliente'           ''   '' '' '' '' '' '',
    'WERKS'               'Centro'                  '04' '' '' '' '' '' '',
    'CONTRATO'            'Contrato'                ''   '' '' '' '' '' '',
    'INSTRUCAO'           'Instrução'               ''   '' '' '' '' '' '',
    'CHARG'               'Lote'                    '05' '' '' '' '' '' '',
    'BTGEW'               'Peso Lote'               ''   '' '' '' '' '' '',
    'TIPO'                'Tipo'                    '04' '' '' '' '' '' '',
    'ALGODOEIRA'          'Desc.Fazenda'            ''   '' '' '' '' '' '',
    'TAMANHO_FARDO'       'Tam.Fardos'              '03' '' '' '' '' '' '',
    'DMBTR'               'Preço (U$LB)'            ''   '' '' '' '' '' '',
    'DATA_INSTR'          'DT Instrução'            '10' '' '' '' '' '' '',
    'SEP'                 ''                        '1'  '' '' '' '' '' '',
    'DATA_RETIRADA'       'DT Retirada'             '10' '' '' '' '' '' '',
    'STATUS_RETIRADA'     ''                        '05' '' '' '' 'C' '' '',
    'SEP'                 ''                        '01' '' '' '' '' '' '',
    'DATA_IN_PORTO'       'DT In Porto'             '10' '' '' '' '' '' '',
    'STATUS_IN_PORTO'     ''                        '05' '' '' '' 'C' '' '',
    'SEP'                 ''                        '01' '' '' '' '' '' '',
    'DATA_PORTO'          'DT Fim Porto'            '10' '' '' '' '' '' '',
    'STATUS_PORTO'        ''                        '05' '' '' '' 'C' '' '',
    'SEP'                 ''                        '01' '' '' '' '' '' '',
    'PREVISAO_RECEB'      'Previsão Recebimento'    '10' '' '' '' '' '' '',
    'DEADLINE_DRAFT'      'DDline Draft'            '10' '' '' '' '' '' '',
    'STATUS_DRAFT'        ''                         '5' '' '' '' 'C' '' '',
    'SEP'                 ''                        '01' '' '' '' '' '' '',
    'DEADLINE_DOCUMEN'    'DDline Carga'            '10' '' '' '' '' '' '',
    'STATUS_DOCUM'        ''                        '05' '' '' '' 'C' '' '',
    'SEP'                 ''                        '01' '' '' '' '' '' '',
    'DATA_ETA'            'DT do ETA'               '10' '' '' '' '' '' '',
    'DATA_CONTAINER'      'Data Ret. Container'     '10' '' '' '' '' '' '',
    'PORTO_EMBARQUE'      'Porto Embarque'          ''   '' '' '' '' '' '',
    'SAFRA'               'Safra'                   '05' '' '' '' '' '' '',
    'QUANTIDADE'          'Quantidade'              '10'   '' '' '' '' '' '',
    'VOLEH'               'UM'                      '03' '' '' '' '' '' '',
    'TERMINAL'            'Cod. Terminal'           ''   '' '' '' '' '' '',
    'TERMINAL_DESC'       'Desc. Terminal'          ''   '' '' '' '' '' '',
    'PONTO_C'             'Cod. Pto Coleta'         ''   '' '' '' '' '' '',
    'PONTO_C_DESC'        'Desc Pto Coleta'         ''   '' '' '' '' '' '',
    'BOOKING'             'Booking'                 ''   '' '' '' '' '' '',
    'MAPA'                'Mapa'                    ''   '' '' '' '' '' '',
    'FUMIGACAO'           'Fumigação'               ''   '' '' '' '' '' '',
    'HRS_FGACAO'          'HorasFumigação'          ''   '' '' '' '' '' '',
    'ARMADOR'             'Armador'                 ''   '' '' '' '' '' '',
    'FREE_TIME'           'FreeTime'                ''   '' '' '' '' '' '',
    'QTD_CTNERS'          'Qtd.Containers'          '10'   '' '' '' '' '' '',
    'COD_DESPACH'         'Cod.Despachante'         ''   '' '' '' '' '' '',
    'COD_DESPACH_DESC'    'Desc.Despachante'        ''   '' '' '' '' '' '',
    'COD_TRANSP'          'Cod.Transportadora'      ''   '' '' '' '' '' '',
    'COD_TRANSP_DESC'     'Desc.Transportadora'     ''   '' '' '' '' '' '',
    'VLR_FRETE'           'ValorFrete'              ''   '' '' '' '' '' '',
*    'PAIS_DES'            'PaísDestino'             ''   '' '' '' '' '' '',
    'PAIS_EXT'            'PaísDestino'             ''   '' '' '' '' '' '',
    'TERMINAL_ESTUF'      'Cód.TerminalEstufagem'   ''   '' '' '' '' '' '',
    'TERMINAL_ESTUF_DESC' 'Desc.TerminalEstufagem'  ''   '' '' '' '' '' '',
    'CONTROLADORA'        'Cod.Controladora'        ''   '' '' '' '' '' '',
    'CONTROLADORA_DESC'   'Desc.Controladora'       ''   '' '' '' '' '' '',
    'NAVIO'               'Navio'                   ''   '' '' '' '' '' ''.

  ELSE.
    PERFORM fieldcatalog USING:
    'BUKRS'                 'Empresa'                   ''  ''  ''  ''  '' '' '',
    'INSTRUCAO'             'Instrução'                 ''  ''  ''  ''  '' '' '',

    'INSTRUCAO_ANT'         'Instrução Anterior'        '20'  ''  ''  ''  '' '' '',

    'ID_PROG_EMBQ'          'Id. Prog. Embarque'        ''  ''  ''  ''  '' '' '',
    'CARGA'                 'Carga'                     ''  ''  ''  ''  '' '' 'X',
    'ESTUFAGEM'             'Estufagem'                 ''  ''  ''  ''  '' '' 'X',
    'PESAGEM'               'Pesagem'                   ''  ''  ''  ''  '' '' 'X',
    'PLANILHA_PESO'         'PlanilhaPeso'              ''  ''  ''  ''  '' '' 'X',
    'NF_EXPORT'             'NFe Exportação'            ''  ''  ''  ''  '' '' 'X',
    'LIBERACAO'             'Liberação'                 ''  ''  ''  ''  '' '' 'X',
    'DEPOSITO'              'Depósito'                  ''  ''  ''  ''  '' '' 'X',

    'OBSERVACAO_ICO'       'Observações'               ''  ''  ''  ''  '' '' '',
    'EMBARQUE'              'Embarque'                  '12'  ''  ''  ''  '' '' '',

    'LIST_CONTRATO'         'Contrato'                  ''  ''  ''  ''  '' '' '',
    'KUNNR'                 'Cód. Cliente'              ''  ''  ''  ''  '' '' '',
    'KUNNR_D'               'Desc. Cliente'             ''  ''  ''  ''  '' '' '',
    'DATA_INSTR'            'Dt. Instrução'             ''  ''  ''  ''  '' '' '',
    'DATA_RETIRADA'         'Dt. Retirada'              ''  ''  ''  ''  '' '' '',
    'DEADLINE_DRAFT'        'Dt. DD Draft'              ''  ''  ''  ''  '' '' '',
    'DATA_CONTAINER'        'Dt. Ret. Container'        ''  ''  ''  ''  '' '' '',
    'DEADLINE_DOCUMEN'      'Dt. DD Carga'              ''  ''  ''  ''  '' '' '',
    'DATA_IN_PORTO'         'Dt. Inicial Porto'         ''  ''  ''  ''  '' '' '',
    'DATA_PORTO'            'Dt. Final Porto'           ''  ''  ''  ''  '' '' '',
    'DATA_ETA'              'DT do ETA'                 '10' '' ''  ''  '' '' '',
    'QUANTIDADE1'           'Quantidade Fardos'         '10'  ''  ''  ''  '' '' '',

    'QTD_FARDOS_EXP'        'Qtd Fardos Exportados'     '10'  ''  ''  ''  '' '' '',
    'SALDO_FARDOS'          'Saldo Fardos'              '10'  ''  ''  ''  '' '' '',
    'NFE_FL'                'NFe F.L.'                  ''  ''  ''  ''  '' '' '',

    'QTD_CTNERS'            'Quantidade Containers'     '10'  ''  ''  ''  '' '' '',
    'BTGEW'                 'Peso Total'                ''  ''  ''  ''  '' '' '',

    'PESO_TOTAL_EXP'        'Peso Total Exportado'      ''  ''  ''  ''  '' '' '',
    'SALDO_PESO'            'Saldo Peso'                ''  ''  ''  ''  '' '' '',

    'BOOKING'               'Booking'                   ''  ''  ''  ''  '' '' '',
    'MAPA'                  'Mapa'                      ''  ''  ''  ''  '' '' '',
    'FUMIGACAO'             'Fumigação'                 ''  ''  ''  ''  '' '' '',
    'HRS_FGACAO'            'HorasFumigação'            ''  ''  ''  ''  '' '' '',
    'ARMADOR'               'Armador'                   ''  ''  ''  ''  '' '' '',
    'FREE_TIME'             'Free Time'                 ''  ''  ''  ''  '' '' '',
    'TERMINAL'              'Terminal Embarque'         ''  ''  ''  ''  '' '' '',
    'TERMINAL_D'            'Desc. Terminal'            ''  ''  ''  ''  '' '' '',
    'COD_DESPACH'           'Cód. Despachante'          ''  ''  ''  ''  '' '' '',
    'COD_DESPACH_D'         'Desc. Despachante'         ''  ''  ''  ''  '' '' '',
    'COD_TRANSP'            'Cód. Transportadora'       ''  ''  ''  ''  '' '' '',
    'COD_TRANSP_D'          'Desc. Transportadora'      ''  ''  ''  ''  '' '' '',
    'VLR_FRETE'             'Valor Frete'               ''  ''  ''  ''  '' '' '',
*    'PAIS_DES'             'Pais Destino'              ''  ''  ''  ''  '' '' '',
    'PAIS_EXT'              'PaísDestino'               ''  ''  ''  ''  '' '' '',
    'TERMINAL_ESTUF'        'Terminal Estufagem'        ''  ''  ''  ''  '' '' '',
    'TERMINAL_ESTUF_D'      'Desc. Terminal Estufagem'  ''  ''  ''  ''  '' '' '',
    'CONTROLADORA'          'Cód. Controladora'         ''  ''  ''  ''  '' '' '',
    'CONTROLADORA_D'        'Desc. Controladora'        ''  ''  ''  ''  '' '' '',
    'NAVIO'                 'Navio'                     ''  ''  ''  ''  '' '' '',
    'PCTGEM_ANT'            'Pré-pagamento(%)'          ''  ''  ''  ''  '' '' '',
    'ERDAT'                 'Previsão Recebimento'      '20'  ''  ''  ''  '' '' '',
    'DATA_BL'               'Dt. BL'                    ''  ''  ''  ''  '' '' '',

    'DT_DOCS_COMPLETO'      'Dt. Docs Completos'         '20'  ''  ''  ''  '' '' '',
    'DT_RECEBIMENTO'        'Dt. Recebimento'           '20'  ''  ''  ''  '' '' '',
    'DT_APRESENTACAO'       'Dt. Apresentação'          '20'  ''  ''  ''  '' '' '',

    'DIAS_REC_BL'           'Dias REC x BL'             '20'  ''  ''  ''  '' '' '',
    'DIAS_REC_DOCS'         'Dias REC x DOCS'           '20'  ''  ''  ''  '' '' '',

    'DIAS_RECEB'            'Dias de Receb.'            '15'  ''  ''  ''  '' '' '',
    'DATA_PAGTO'            'Pagamento Realizado'       '20'  ''  ''  ''  '' '' ''.


    LOOP AT it_fieldcatalog ASSIGNING FIELD-SYMBOL(<fcat>).
      CASE <fcat>-fieldname.
        WHEN 'OBSERVACAO_ICO'.

          <fcat>-style = cl_gui_alv_grid=>mc_style_button.

        WHEN 'EMBARQUE'.

          <fcat>-edit = abap_true.
          <fcat>-drdn_alias = 'X'.
          <fcat>-drdn_hndl = 1.
          <fcat>-outputlen = 10.

        WHEN 'DT_DOCS_COMPLETO'.

          <fcat>-edit = abap_true.
          <fcat>-f4availabl = 'X'.
          <fcat>-ref_table  = 'ZSDT0236'.
          <fcat>-ref_field  = 'DT_DOCS_COMPLETO'.
          <fcat>-tabname    = 'IT_ZSDT0045_AUX'.
          <fcat>-edit_mask  = '__.__.____'.

        WHEN 'DT_RECEBIMENTO'.

          <fcat>-edit = abap_true.
          <fcat>-f4availabl = 'X'.
          <fcat>-ref_table  = 'ZSDT0236'.
          <fcat>-ref_field  = 'DT_RECEBIMENTO'.
          <fcat>-tabname    = 'IT_ZSDT0045_AUX'.
          <fcat>-edit_mask  = '__.__.____'.

        WHEN 'DT_APRESENTACAO'.

          <fcat>-edit = abap_true.
          <fcat>-f4availabl = 'X'.
          <fcat>-ref_table  = 'ZSDT0236'.
          <fcat>-ref_field  = 'DT_APRESENTACAO'.
          <fcat>-tabname    = 'IT_ZSDT0045_AUX'.
          <fcat>-edit_mask  = '__.__.____'.

        WHEN 'NFE_FL'.

          <fcat>-style = cl_gui_alv_grid=>mc_style_button.

      ENDCASE.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " FIELDCATALOG
**&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fieldcatalog  USING    VALUE(p_fieldname)
                            VALUE(p_desc)
                            VALUE(p_tam)
                            VALUE(p_no_zero)
                            VALUE(p_hotspot)
                            VALUE(p_cor)
                            VALUE(p_just)
                            VALUE(p_sum)
                            VALUE(p_checkbox).

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
  wa_fieldcatalog-checkbox  = p_checkbox.

  wa_fieldcatalog-edit = COND #( WHEN block IS INITIAL THEN p_checkbox ELSE abap_false ).

  APPEND wa_fieldcatalog TO it_fieldcatalog.

  CLEAR: wa_fieldcatalog.

ENDFORM.                    " FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  SALVAR_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_status .

  DATA p_status TYPE zsdt0045-status.

  CALL METHOD grid_principal->check_changed_data.

  LOOP AT it_status INTO wa_status.

    UPDATE zsdt0045 SET status = wa_status-status
                        usnam_lt = sy-uname
                        data_lt  = sy-datum
                        hora_lt  = sy-uzeit
              WHERE zseq_inst   EQ wa_status-zseq_inst
                AND objek       EQ wa_status-objek
                AND objecttable EQ wa_status-objecttable.

  ENDLOOP.

  SELECT instrucao FROM zsdt0236
    INTO TABLE @DATA(t_zsdt0236_upd)
    FOR ALL ENTRIES IN @it_0045_aux
    WHERE instrucao = @it_0045_aux-instrucao.

  LOOP AT it_0045_aux INTO DATA(w_zsdt0045_aux).

    READ TABLE t_zsdt0236_upd ASSIGNING FIELD-SYMBOL(<fs_zsdt0236_upd>)
                                                  WITH KEY instrucao = w_zsdt0045_aux-instrucao.
    IF <fs_zsdt0236_upd> IS ASSIGNED.

      UPDATE zsdt0236 SET embarque          = w_zsdt0045_aux-embarque
                          dt_docs_completo  = w_zsdt0045_aux-dt_docs_completo
                          dt_recebimento    = w_zsdt0045_aux-dt_recebimento
                          dt_apresentacao   = w_zsdt0045_aux-dt_apresentacao
                          observacao        = w_zsdt0045_aux-observacao
                   WHERE instrucao = w_zsdt0045_aux-instrucao.

    ENDIF.

  ENDLOOP.

  IF t_zsdt0236_upd[] IS NOT INITIAL.
    COMMIT WORK.
    MESSAGE s000(z_les) WITH 'Dados atualizados com sucesso!'.
  ENDIF.

  PERFORM: seleciona_dados.
  PERFORM: display_fieldcatalog.
  PERFORM: create_object.

  FREE: it_status, it_saida_aux.

ENDFORM.                    " SALVAR_STATUS
*&---------------------------------------------------------------------*
*&      Form  STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_QTD  text
*      <--P_WA_SAIDA_STATUS_RETIRADA  text
*----------------------------------------------------------------------*
FORM status  USING    p_dir
             CHANGING p_status p_data.

  DATA: qtd TYPE i.

  CHECK p_data IS NOT INITIAL.

  qtd =  p_data - sy-datum.
  p_status = abap_false.

  p_status = SWITCH icon_l4( p_dir
       WHEN 1 THEN COND icon_l4( WHEN qtd LT 0 THEN icon_green_light
                                 WHEN qtd LE 2 THEN icon_red_light
                                 WHEN qtd BETWEEN 2 AND 7 THEN icon_yellow_light
                               )
      WHEN 2 THEN COND icon_l4(  WHEN qtd EQ 0 THEN icon_red_light
                                 WHEN qtd EQ 1 THEN icon_yellow_light
                                 WHEN qtd EQ 2 THEN ''
                                 WHEN qtd < 0 THEN icon_green_light
                               )
                            ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  KNA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZSDT0166_KUNNR  text
*      <--P_WA_SAIDA_NAME1  text
*----------------------------------------------------------------------*
FORM kna1  USING    p_kunnr
           CHANGING p_name.

  SELECT SINGLE name1
    FROM kna1
    INTO p_name
   WHERE kunnr EQ p_kunnr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MARA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZSDT0045_MATNR  text
*      <--P_WA_SAIDA_TIPO  text
*----------------------------------------------------------------------*
FORM mara  USING    p_matnr
           CHANGING p_tipo.
  SELECT SINGLE normt
    FROM mara
    INTO p_tipo
    WHERE matnr EQ p_matnr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  LFA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZSDT0045_TERMINAL  text
*      <--P_WA_SAIDA_TERMINAL_DESC  text
*----------------------------------------------------------------------*
FORM lfa1 USING p_lifnr CHANGING p_name.
  SELECT SINGLE name1 FROM lfa1 INTO p_name WHERE lifnr EQ p_lifnr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FCAT>_FIELDNAME  text
*      -->P_6      text
*----------------------------------------------------------------------*
FORM cor USING p_fieldname p_cor CHANGING t_cor TYPE lvc_t_scol.
  APPEND VALUE #( fname = p_fieldname color-col = p_cor color-int = '1' color-inv = '1' nokeycol = 'X' ) TO t_cor.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  T0143
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_0051_BSTKD  text
*      <--P_<W_0045>_PCTGEM_ANT  text
*----------------------------------------------------------------------*
FORM t0143 USING p_bstkd CHANGING p_pctgem_ant.
  SELECT SINGLE pctgem_ant
    FROM zsdt0143
    INTO p_pctgem_ant
    WHERE contrato EQ p_bstkd.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AGRUPA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM agrupa_dados .

  DATA v_cor TYPE n.

  DATA _quantidade TYPE zsdt0045-quantidade.
  DATA _qtd_ctners TYPE zsdt0045-qtd_ctners.
  DATA _btgew TYPE zsdt0045-btgew.
  DATA lv_vbeln TYPE vbrp-vbeln.
  DATA lv_posnr TYPE vbrp-posnr.

  DATA lv_nro_sol_ov TYPE  zsded013.

  DATA: lv_datediff TYPE  p,
        lv_timediff TYPE  p,
        lv_earliest TYPE  c.

  DATA: ls_comwa TYPE vbco6.

  "*---> 19/07/2023 - Migração S4 - LO
*  DATA: lt_vbfa_tab TYPE TABLE OF vbfa.
  DATA: lt_vbfa_tab TYPE TABLE OF vbfas.
*  DATA: ls_vbfa_tab TYPE vbfa.
  DATA: ls_vbfa_tab TYPE vbfas.
  "*<--- 19/07/2023 - Migração S4 - LO

  DATA: lt_bseg TYPE TABLE OF bseg.
  DATA: ls_bseg TYPE bseg.

  DATA: lv_belegtyp_back TYPE  vbuk-vbtyp.

  SELECT *
    FROM zsdt0236
    INTO TABLE it_0236
      FOR ALL ENTRIES IN it_0045
      WHERE instrucao EQ it_0045-instrucao.

  it_contrato = it_0045.
  SORT it_contrato BY instrucao contrato.
  DELETE ADJACENT DUPLICATES FROM it_contrato COMPARING instrucao contrato.

  LOOP AT it_0045 ASSIGNING FIELD-SYMBOL(<w_0045>).

* 3 AMARELO
* 5 VERDE
* 6 VERMELHO

    READ TABLE it_0236 ASSIGNING FIELD-SYMBOL(<f_0236>) WITH KEY instrucao = <w_0045>-instrucao.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING <f_0236> TO <w_0045>.

      LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>) WHERE outputlen EQ 1.
        vl_cor = 0.

        campo = <fcat>-fieldname.
        ASSIGN COMPONENT campo OF STRUCTURE <w_0045> TO <fs_campo1>.

        CASE campo.
          WHEN 'NF_EXPORT'.
            SELECT COUNT(*)
              FROM zsdt0053
              WHERE instrucao EQ <w_0045>-instrucao
                AND vbeln IS NOT NULL.

            IF sy-subrc IS INITIAL.
              IF <fs_campo1> IS NOT INITIAL.
                vl_cor = 5.
              ELSE.
                vl_cor = 3.
              ENDIF.
            ELSE.
              IF <fs_campo1> IS NOT INITIAL.
                vl_cor = 5.
              ELSE.
                vl_cor = 6.
              ENDIF.
            ENDIF.

            PERFORM cor USING <fcat>-fieldname vl_cor CHANGING <w_0045>-cellcolor.

          WHEN OTHERS.
            IF <fs_campo1> IS NOT INITIAL.
              vl_cor = 5.
            ELSE.
              vl_cor = 6.
            ENDIF.

            PERFORM cor USING <fcat>-fieldname vl_cor CHANGING <w_0045>-cellcolor.
        ENDCASE.

      ENDLOOP.
    ELSE.
      LOOP AT it_fcat ASSIGNING <fcat> WHERE outputlen EQ 1.
        vl_cor = 0.

        campo = <fcat>-fieldname.
        ASSIGN COMPONENT campo OF STRUCTURE <w_0045> TO <fs_campo1>.

        CASE campo.
          WHEN 'NF_EXPORT'.
            SELECT COUNT(*)
              FROM zsdt0053
              WHERE instrucao EQ <w_0045>-instrucao
                AND vbeln IS NOT NULL.

            IF sy-subrc IS INITIAL.
              IF <fs_campo1> IS NOT INITIAL.
                vl_cor = 5.
              ELSE.
                vl_cor = 3.
              ENDIF.
            ELSE.
              IF <fs_campo1> IS NOT INITIAL.
                vl_cor = 5.
              ELSE.
                vl_cor = 6.
              ENDIF.
            ENDIF.

            PERFORM cor USING <fcat>-fieldname vl_cor CHANGING <w_0045>-cellcolor.

          WHEN OTHERS.
            IF <fs_campo1> IS NOT INITIAL.
              vl_cor = 5.
            ELSE.
              vl_cor = 6.
            ENDIF.

            PERFORM cor USING <fcat>-fieldname vl_cor CHANGING <w_0045>-cellcolor.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0051
      INTO @DATA(w_0051)
      WHERE nro_sol_ov EQ @<w_0045>-objek.

    IF sy-subrc IS INITIAL.
      <w_0045>-kunnr = w_0051-kunnr.
      PERFORM kna1 USING <w_0045>-kunnr CHANGING <w_0045>-kunnr_d.
      PERFORM t0143 USING w_0051-bstkd CHANGING <w_0045>-pctgem_ant.
    ENDIF.

    lv_nro_sol_ov = <w_0045>-objek.

    CALL FUNCTION 'ZDUE_CONSULTA_NOMEACAO_SOL_OV'
      EXPORTING
        i_nro_sol_ov = lv_nro_sol_ov
      IMPORTING
        e_data_bl    = <w_0045>-data_bl.

*    SELECT MIN( erdat )
*      FROM vbak
*      INTO <w_0045>-data_bl
*      WHERE vbeln IN ( SELECT vbeln FROM zsdt0053 WHERE instrucao EQ <w_0045>-instrucao ).
*    IF sy-subrc IS INITIAL AND <w_0045>-data_bl IS NOT INITIAL.
*      ADD 30 TO <w_0045>-data_bl.
*    ENDIF.

    IF <w_0045>-data_bl IS NOT INITIAL.
      <w_0045>-erdat = <w_0045>-data_bl.
      ADD 2 TO <w_0045>-erdat.
    ENDIF.
    IF <w_0045>-erdat IS NOT INITIAL AND <w_0045>-data_bl IS NOT INITIAL.
      CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
        EXPORTING
          date1            = <w_0045>-data_bl
          date2            = <w_0045>-erdat
        IMPORTING
          datediff         = lv_datediff
          timediff         = lv_timediff
          earliest         = lv_earliest
        EXCEPTIONS
          invalid_datetime = 1
          OTHERS           = 2.
      IF sy-subrc = 0.
        <w_0045>-dias_receb = lv_datediff.
      ENDIF.

    ENDIF.

    CLEAR lv_vbeln.
    SELECT MAX( vbeln ) MAX( posnr )
      FROM vbap
      INTO (lv_vbeln, lv_posnr)
      WHERE vbeln = ( SELECT MAX( vbeln ) FROM zsdt0053 WHERE instrucao EQ <w_0045>-instrucao ).
    IF lv_vbeln IS NOT INITIAL.
      ls_comwa-mandt = sy-mandt.
      ls_comwa-vbeln = lv_vbeln.
      ls_comwa-posnr = lv_posnr.

      REFRESH lt_vbfa_tab.

      CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION' "#EC CI_USAGE_OK[2198647]
        EXPORTING
          comwa         = ls_comwa
        IMPORTING
          belegtyp_back = lv_belegtyp_back
        TABLES
          vbfa_tab      = lt_vbfa_tab
        EXCEPTIONS
          no_vbfa       = 1
          no_vbuk_found = 2
          OTHERS        = 3.
      IF lt_vbfa_tab[] IS NOT INITIAL.

        CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
          EXPORTING
            it_for_all_entries = lt_vbfa_tab
            i_where_clause     = |BELNR = IT_FOR_ALL_ENTRIES-VBELN|
          IMPORTING
            et_bseg            = lt_bseg
          EXCEPTIONS
            not_found          = 1.
        IF sy-subrc <> 0 OR lines( lt_bseg ) = 0.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ELSE.
          sy-dbcnt = lines( lt_bseg ).
        ENDIF.


        IF sy-subrc = 0.
          DELETE lt_bseg WHERE vbel2 NE lv_vbeln.
          IF lt_bseg[] IS NOT INITIAL.
            READ TABLE lt_bseg INTO ls_bseg INDEX 1.
            <w_0045>-data_pagto = ls_bseg-augdt.
            IF <w_0045>-erdat IS INITIAL AND <w_0045>-data_bl IS INITIAL.
              <w_0045>-erdat =  ls_bseg-zfbdt.
              IF <w_0045>-erdat IS NOT INITIAL AND <w_0045>-data_pagto IS NOT INITIAL.
                CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
                  EXPORTING
                    date1            = <w_0045>-erdat
                    date2            = <w_0045>-data_pagto
                  IMPORTING
                    datediff         = lv_datediff
                    timediff         = lv_timediff
                    earliest         = lv_earliest
                  EXCEPTIONS
                    invalid_datetime = 1
                    OTHERS           = 2.
                IF sy-subrc = 0.
                  <w_0045>-dias_receb = lv_datediff.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

    TRY .
        <w_0045>-bukrs_d = it_t001[ bukrs = <w_0045>-bukrs ]-butxt.
      CATCH cx_sy_itab_line_not_found.
        <w_0045>-bukrs_d = ''.
    ENDTRY.

    PERFORM lfa1          USING <w_0045>-porto_embarque CHANGING <w_0045>-porto_embarque_d.
    PERFORM lfa1          USING <w_0045>-ponto_c        CHANGING <w_0045>-ponto_c_d.
    PERFORM lfa1          USING <w_0045>-terminal       CHANGING <w_0045>-terminal_d.
    PERFORM lfa1          USING <w_0045>-cod_despach    CHANGING <w_0045>-cod_despach_d.
    PERFORM lfa1          USING <w_0045>-cod_transp     CHANGING <w_0045>-cod_transp_d.
    PERFORM lfa1          USING <w_0045>-terminal_estuf CHANGING <w_0045>-terminal_estuf_d.
    PERFORM lfa1          USING <w_0045>-controladora   CHANGING <w_0045>-controladora_d.
    PERFORM lfa1_endereco USING <w_0045>-terminal_estuf CHANGING <w_0045>-stras <w_0045>-ort02 <w_0045>-ort01 <w_0045>-pstlz <w_0045>-regio.


    SELECT SINGLE landx
      FROM t005t
      INTO <w_0045>-pais_ext
      WHERE spras EQ sy-langu
      AND land1 EQ <w_0045>-pais_des.

    SELECT SINGLE *
       FROM zsdt0166
      INTO @DATA(wa_zsdt0166)
       WHERE id EQ ( SELECT MAX( id )
                            FROM zsdt0166
                        WHERE safra  EQ @<w_0045>-safra
                          AND werks  EQ @<w_0045>-werks
                          AND lote   EQ @<w_0045>-charg
                          AND status EQ 'A'
                    ).

    IF sy-subrc IS INITIAL.
      <w_0045>-tipo          = wa_zsdt0166-tipo.
      <w_0045>-tamanho_fardo = COND #( WHEN <w_0045>-bukrs EQ '0015' THEN wa_zsdt0166-tamanho_fardo ELSE <w_0045>-tamanho_fardo ).
      <w_0045>-fazenda       = wa_zsdt0166-algodoeira.
    ELSE.

      SELECT SINGLE *
        FROM zsdt0051
        INTO w_0051
        WHERE nro_sol_ov EQ <w_0045>-objek.

      IF sy-subrc IS INITIAL.
        PERFORM mara USING <w_0045>-matnr CHANGING <w_0045>-tipo.
      ENDIF.

      <w_0045>-fazenda = <w_0045>-ponto_c_d.

    ENDIF.

    PERFORM lfa1 USING <w_0045>-ponto_c CHANGING <w_0045>-ponto_c_d.
  ENDLOOP.

  it_0045_aux = it_0045.

  LOOP AT it_0045_aux ASSIGNING <w_0045>.

    _quantidade = 0.
    _qtd_ctners = 0.
    _btgew = 0.

    LOOP AT it_0045 INTO DATA(w_0045) WHERE instrucao EQ <w_0045>-instrucao.
      ADD w_0045-quantidade TO _quantidade.
      ADD w_0045-btgew TO _btgew.
    ENDLOOP.

    LOOP AT it_contrato INTO DATA(w_contrato) WHERE instrucao EQ <w_0045>-instrucao.
      IF <w_0045>-list_contrato IS NOT INITIAL.
        <w_0045>-list_contrato = |{ <w_0045>-list_contrato }, { w_contrato-contrato }|.
      ELSE.
        <w_0045>-list_contrato = |{ w_contrato-contrato }|.
      ENDIF.
    ENDLOOP.

    <w_0045>-quantidade1 = _quantidade.
    <w_0045>-btgew      = _btgew.

    READ TABLE it_zsdt0053_col INTO DATA(w_zsdt0053_col) WITH KEY  instrucao = <w_0045>-instrucao.
    IF sy-subrc IS INITIAL.
      <w_0045>-peso_total_exp  = w_zsdt0053_col-zmeng.
      <w_0045>-qtd_fardos_exp =  w_zsdt0053_col-volum.
    ENDIF.

    <w_0045>-saldo_peso = ( <w_0045>-btgew - <w_0045>-peso_total_exp ).

    "preenche inscrição anterior
    READ TABLE  it_zsdt0053_inst INTO DATA(w_zsdt0053_inst) WITH KEY instrucao = <w_0045>-instrucao.
    IF sy-subrc IS INITIAL.
      <w_0045>-instrucao_ant = w_zsdt0053_inst-instrucao_ant.
    ENDIF.

    IF <w_0045>-observacao IS INITIAL.
      MOVE icon_display_more TO <w_0045>-observacao_ico.
    ELSE.
      MOVE icon_enter_more TO <w_0045>-observacao_ico.
    ENDIF.

    IF <w_0045>-data_bl IS NOT INITIAL.

      CLEAR: lv_datediff, lv_timediff, lv_earliest.

      CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
        EXPORTING
          date1            = <w_0045>-data_bl
          date2            = <w_0045>-dt_recebimento
        IMPORTING
          datediff         = lv_datediff
          timediff         = lv_timediff
          earliest         = lv_earliest
        EXCEPTIONS
          invalid_datetime = 1
          OTHERS           = 2.
      IF sy-subrc = 0.
        <w_0045>-dias_rec_bl =  lv_datediff.
      ENDIF.

    ENDIF.

    IF <w_0045>-dt_docs_completo IS NOT INITIAL.

      CLEAR: lv_datediff, lv_timediff, lv_earliest.

      CALL FUNCTION '/SDF/CMO_DATETIME_DIFFERENCE'
        EXPORTING
          date1            = <w_0045>-dt_docs_completo
          date2            = <w_0045>-dt_recebimento
        IMPORTING
          datediff         = lv_datediff
          timediff         = lv_timediff
          earliest         = lv_earliest
        EXCEPTIONS
          invalid_datetime = 1
          OTHERS           = 2.
      IF sy-subrc = 0.
        <w_0045>-dias_rec_docs =  lv_datediff.
      ENDIF.

    ENDIF.

    <w_0045>-nfe_fl = icon_select_detail.

    SELECT SINGLE instrucao, embarque, dt_docs_completo, dt_recebimento,
      dt_apresentacao, observacao FROM zsdt0236
      INTO @DATA(w_zsdt0236_upd)
      WHERE instrucao = @<w_0045>-instrucao.

    IF sy-subrc IS INITIAL.
      <w_0045>-embarque          = w_zsdt0236_upd-embarque.
      <w_0045>-dt_docs_completo = w_zsdt0236_upd-dt_docs_completo.
      <w_0045>-dt_recebimento    = w_zsdt0236_upd-dt_recebimento.
      <w_0045>-dt_apresentacao   = w_zsdt0236_upd-dt_apresentacao.
      <w_0045>-observacao       = w_zsdt0236_upd-observacao.
    ENDIF.

    IF <w_0045>-observacao IS INITIAL.
      MOVE icon_enter_more TO <w_0045>-observacao_ico.
    ELSE.
      MOVE icon_display_more TO <w_0045>-observacao_ico.
    ENDIF.

    <w_0045>-saldo_fardos = <w_0045>-quantidade1 - <w_0045>-qtd_fardos_exp .

  ENDLOOP.

  SORT it_0045_aux BY instrucao.
  SORT it_0045 BY instrucao.
  DELETE ADJACENT DUPLICATES FROM it_0045_aux COMPARING instrucao.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GERA_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_doc CHANGING p_0045 TYPE ty_0045.
  DATA: html             TYPE string,
        mandt            TYPE char50,
        vl_peso_maixmo   TYPE string,
        vl_peso_maixmo_d TYPE char30.


  mandt = | #Ambiente de { sy-sysid }#|.

  IF sy-sysid EQ 'PRD'.
    CLEAR mandt.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0236
    INTO @DATA(w_0236)
    WHERE instrucao EQ @p_0045-instrucao.

  IF w_0236-id_prog_embq IS INITIAL.

    SELECT MAX( id_prog_embq )
    FROM zsdt0236
    INTO @DATA(id).

    ADD 1 TO id.

    w_0236-id_prog_embq = id.
    w_0236-instrucao = p_0045-instrucao.

    MODIFY zsdt0236 FROM w_0236.
    COMMIT WORK.

  ENDIF.

  p_0045-id_prog_embq = w_0236-id_prog_embq.

  DATA(titulo) = |Programação de embarque { w_0236-id_prog_embq } { mandt }|.

  IF p_0045-limite_peso EQ 'S'.
    vl_peso_maixmo_d = 'Peso Máximo:'.
    vl_peso_maixmo   = p_0045-peso_max.
  ENDIF.

  html =
    |<!DOCTYPE html> | &&
    |<html lang="pt"> | &&
    |<head> | &&
    |<title>{ titulo }</title> | &&
    |<meta charset="utf-8"> | &&
    |<meta name="viewport" content="width=device-width, initial-scale=1"> | &&
    |<link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.4.0/css/bootstrap.min.css"> | &&
    |<link href="https://fonts.googleapis.com/css?family=Montserrat" rel="stylesheet" type="text/css"> | &&
    |<link href="https://fonts.googleapis.com/css?family=Lato" rel="stylesheet" type="text/css"> | &&
    |<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js"></script> | &&
    |<script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.4.0/js/bootstrap.min.js"></script> | &&
    '<style> .table { border-style: solid; } </style>' &&
    |</head> | &&
    |<body id="myPage" data-spy="scroll" data-target=".navbar" data-offset="60"> | &&
    |<div class="container"> | &&
        |<img src="https://www.amaggi.com.br/wp-content/uploads/2018/08/4-2014-thumb.jpg" width="150" height="150"> | &&
        |<p class="text-right">{ titulo }</p><br> | &&
        |<p>Cuiabá, { sy-datum+6(2) } de { sy-datum+4(2) } de { sy-datum+0(4) }.</p> | &&
        |<br> | &&
        |<p>Cliente: { p_0045-kunnr_d } <p> | &&
        |<br> | &&
        |<p>Para: { p_0045-cod_despach_d }, { p_0045-bukrs_d }, { p_0045-controladora_d }, { p_0045-cod_despach_d }, { p_0045-terminal_estuf_d } </p> | &&
        |<p>Programação de Estufagem da Instrução { p_0045-instrucao }</p> | &&
        |<p>A Instrução documentária segue no anexo.</p> | &&
        |<p>Notar informações da instrução abaixo:</p> | &&
        |<br> | &&
        |Quantidade de containers: { p_0045-qtd_ctners } x 40’HC <br> | &&
        |Porto de Embarque: { p_0045-porto_embarque }<br> | &&
        |Terminal de Embarque: { p_0045-terminal_d }<br> | &&
        |Terminal de Estufagem: { p_0045-terminal_estuf_d }<br> | &&
        |Destino: { p_0045-pais_ext }<br> | &&
        |Navio: { p_0045-navio }<br> | &&
        |Armador: { p_0045-armador }<br> | &&
        |ETA: { p_0045-data_eta+6(2) }/{ p_0045-data_eta+4(2) }/{ p_0045-data_eta+0(4) }<br> | &&
        |Booking: { p_0045-booking }<br> | &&
        |Draft Deadline: { p_0045-deadline_draft+6(2) }/{ p_0045-deadline_draft+4(2) }/{ p_0045-deadline_draft+0(4) }<br> | &&
        |Carga Deadline: { p_0045-deadline_documen+6(2) }/{ p_0045-deadline_documen+4(2) }/{ p_0045-deadline_documen+0(4) }<br><br> | &&
        |Limite de peso: { p_0045-limite_peso } { vl_peso_maixmo_d } { vl_peso_maixmo } <br> | &&
        |Fumigação: { p_0045-fumigacao } Horas Fumigação: { p_0045-hrs_fgacao } <br> | &&
        |Mapa: { p_0045-mapa } <br> | &&

        |<p style="color: #ff0000"> <strong>|.

  IF p_0045-free_time IS INITIAL .
    html = |{ html } Ø FREE TIME DE: não informado<br> |.
  ELSE.
    html = |{ html } Ø FREE TIME DE: { p_0045-free_time } DIAS<br> |.
  ENDIF.

  html = |{ html }| &&
|</strong></p>| &&
|Segue abaixo lotes instruídos: | &&
|<table class="table" style="border-color: black white black white  ">| &&
*|<table class="table"> | &&
  |<thead class="thead-dark"> | &&
      |<tr> | &&
          |<th scope="col">Cliente</th> | &&
          |<th scope="col">Lote</th> | &&
          |<th scope="col">Fardos</th> | &&
          |<th scope="col">Tipo</th> | &&
          |<th scope="col">Peso</th> | &&
          |<th scope="col">Fazenda</th> | &&
          |<th scope="col">Contrato</th> | &&
          |<th scope="col">Tamanho do Fardo</th> | &&
          |<th scope="col">Preço LB USD</th> | &&
          |<th scope="col">Instrução</th> | &&
      |</tr> | &&
  |</thead> |.

*  DATA AMOUNT_FIELD TYPE VBAP-NETWR.
  DATA amount_field TYPE zsdt0045-btgew.

  DATA currency_field TYPE zsdt0045-gewei.
  DATA: character_string TYPE string.

  LOOP AT it_0045 INTO DATA(w_0045) WHERE instrucao EQ p_0045-instrucao.

    CLEAR amount_field.
    amount_field = w_0045-btgew.

*    CHARACTER_STRING = |{ AMOUNT_FIELD CURRENCY = CURRENCY_FIELD  NUMBER = USER }|.
    character_string = |{ amount_field COUNTRY = 'BR ' }|.

    html = |{ html }| &&
                |<tbody> | &&
                |<tr> | &&
                    |<td>{ w_0045-kunnr_d }</td> | &&
                    |<td>{ w_0045-charg }</td> | &&
                    |<td>{ w_0045-quantidade ALPHA = OUT }</td> | &&
                    |<td>{ w_0045-tipo }</td> | &&
*                    |<td>{ W_0045-BTGEW }</td> | &&
                    |<td>{ character_string }</td> | &&
                    |<td>{ w_0045-fazenda }</td> | &&
                    |<td>{ w_0045-contrato }</td> | &&
                    |<td>{ w_0045-tamanho_fardo }</td> | &&
                    |<td>{ w_0045-dmbtr }</td> | &&
                    |<td>{ w_0045-instrucao }</td> | &&
                |</tr> | &&
             |</tbody> |.
  ENDLOOP.

  html = |{ html }| &&
           |</table> | &&
                |<br>| &&
                |<p>{ p_0045-bukrs_d }</p> | &&
                |<p>Notar que as carretas devem chegar a partir do dia | &&
                |{ p_0045-data_in_porto+6(2) }/{ p_0045-data_in_porto+4(2) }/{ p_0045-data_in_porto+0(4) } | &&
                |até { p_0045-data_porto+6(2) }/{ p_0045-data_porto+4(2) }/{ p_0045-data_porto+0(4) } | &&
                |{ p_0045-terminal_estuf_d }</p>| &&
                |<p>Data da liberação dos containers a partir do dia { p_0045-data_container+6(2) }/{ p_0045-data_container+4(2) }/{ p_0045-data_container+0(4) } , levando em consideração | &&
                |o DEADLINE no dia { p_0045-deadline_documen+6(2) }/{ p_0045-deadline_documen+4(2) }/{ p_0045-deadline_documen+0(4) }.</p> | &&
                |<p>Retirada de Vazios conforme o carregamento. (Acompanhar Rondonline) | &&
                |<p>Segue abaixo endereço do terminal:</p> | &&
                |<br> | &&
                |<p>Endereço: { p_0045-stras }, { p_0045-ort02 }, { p_0045-ort01 }, { p_0045-pstlz }, { p_0045-regio } </p> | &&
                |</div> | &&
                |</body> | &&
                |</html> |.

  PERFORM converte_envio USING html p_0045-bukrs p_0045-pais_ext p_0045-instrucao p_0045-booking .

ENDFORM.

FORM monta_envio USING v_html v_bukrs v_pais_ext p_0045-instrucao p_0045-booking .

  DATA: ls_type        TYPE sood-objtp,
        lv_date        TYPE char10,
        wl_email       TYPE adr6-smtp_addr,
       " lv_sub         TYPE so_obj_des,
        lv_sub         TYPE SO_OBJ_DES,
        lo_document    TYPE REF TO cl_document_bcs,
        lo_bcs         TYPE REF TO cl_bcs,
        lo_sapuser_bcs TYPE REF TO cl_sapuser_bcs,
        lo_recipient   TYPE REF TO if_recipient_bcs,
        lo_ex_bcs      TYPE REF TO cx_bcs,
        lv_message     TYPE string,
        mandt          TYPE char20,
        vuser          TYPE sy-uname.

  SELECT *
    FROM zmail
    INTO TABLE @DATA(t_zmail)
    WHERE tcode EQ @sy-tcode
      AND bukrs EQ @v_bukrs.

  LOOP AT t_zmail INTO DATA(w_zmail).

    MOVE: 'HTML'          TO ls_type,
           w_zmail-email TO wl_email.

    CLEAR: lo_document, vuser.

    mandt = | #Ambiente de { sy-sysid }#|.

    IF sy-sysid EQ 'PRD'.
      CLEAR mandt.
    ENDIF.




*    lv_sub = |Programação de embarque { mandt }|.

    CONCATENATE 'Nomeação –' p_0045-instrucao  '/' p_0045-booking  '-'  v_pais_ext
    INTO lv_sub SEPARATED BY space.


    lo_document =
      cl_document_bcs=>create_document(
      i_type    = 'HTM'
      i_subject = lv_sub
      i_text    = v_html
    ).

    lo_bcs = cl_bcs=>create_persistent( ).
    lo_bcs->set_document( lo_document ).
    lo_recipient = cl_cam_address_bcs=>create_internet_address( wl_email ).
    lo_bcs->set_message_subject( ip_subject = CONV #( lv_sub ) ).

    TRY.
        CALL METHOD lo_bcs->add_recipient
          EXPORTING
            i_recipient = lo_recipient
            i_express   = 'X'.
      CATCH cx_send_req_bcs.
    ENDTRY.

    vuser = sy-uname.
    sy-uname = 'JOBADM'.

    lo_sapuser_bcs = cl_sapuser_bcs=>create( sy-uname ).
    lo_bcs->set_sender( i_sender = lo_sapuser_bcs ).
    lo_bcs->set_send_immediately( 'X' ).

    TRY.

        CALL METHOD lo_bcs->send( ).
        COMMIT WORK.

        sy-uname = vuser.
        MESSAGE 'Mensagem enviada com Sucesso!' TYPE 'S'.

      CATCH cx_bcs INTO lo_ex_bcs.
        sy-uname = vuser.
        lv_message = lo_ex_bcs->get_text( ).
    ENDTRY.

  ENDLOOP.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Email não cadastrado!' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CONVERTE_ENVIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_HTML  text
*----------------------------------------------------------------------*
FORM converte_envio USING p_html p_bukrs p_pais_ext p_0045_instrucao p_0045_booking .

  DATA: len1    TYPE i,
        len2    TYPE i,
        ofst1   TYPE i,
        ofst2   TYPE i,
        ofst3   TYPE i,
        vl_html TYPE string,
        str_aux TYPE string VALUE 'a b',
        str_c   TYPE string.

  DATA z_html TYPE zstring.

  vl_html = p_html.

  len1 = strlen( vl_html ).

  DO.

    IF ofst2 EQ len1.
      EXIT.
    ENDIF.

    IF strlen( vl_html+ofst2 ) <= 255.
      APPEND VALUE #( line = vl_html+ofst2 ) TO z_html.
      CLEAR str_c.
      EXIT.
    ENDIF.

    str_c = |{ str_c }{ vl_html+ofst2(255) }|.

    ofst3 = strlen( str_c ).
    SUBTRACT 1 FROM ofst3.

    DO.
      IF str_c+ofst3(1) EQ str_aux+1(1).
        APPEND VALUE #( line = str_c+0(ofst3) ) TO z_html.
        CLEAR str_c.
        EXIT.
      ELSE.
        SUBTRACT 1 FROM ofst3.
      ENDIF.
    ENDDO.
    ADD ofst3 TO ofst2.

  ENDDO.

  PERFORM monta_envio USING z_html p_bukrs p_pais_ext p_0045_instrucao p_0045_booking .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  T001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<W_0045>_KUNNR  text
*      <--P_<W_0045>_KUNNR_D  text
*----------------------------------------------------------------------*
FORM t001 USING p_kunnr CHANGING p_kunnr_d.
  SELECT SINGLE butxt FROM t001 INTO p_kunnr_d WHERE bukrs EQ p_kunnr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COR_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cor_grid USING l_instrucao.

  DATA v_cor TYPE n.

  DELETE it_0236 WHERE instrucao EQ l_instrucao.

  SELECT *
    FROM zsdt0236
    APPENDING TABLE it_0236
    WHERE instrucao EQ l_instrucao.

  LOOP AT it_0045_aux ASSIGNING FIELD-SYMBOL(<w_0045>) WHERE instrucao EQ l_instrucao.

* 3 AMARELO
* 5 VERDE
* 6 VERMELHO

    READ TABLE it_0236 ASSIGNING FIELD-SYMBOL(<f_0236>) WITH KEY instrucao = <w_0045>-instrucao.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING <f_0236> TO <w_0045>.

      FREE <w_0045>-cellcolor.
      LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>) WHERE outputlen EQ 1.
        vl_cor = 0.

        campo = <fcat>-fieldname.
        ASSIGN COMPONENT campo OF STRUCTURE <w_0045> TO <fs_campo1>.

        CASE campo.
          WHEN 'NF_EXPORT'.
            SELECT COUNT(*)
              FROM zsdt0053
              WHERE instrucao EQ <w_0045>-instrucao
                AND vbeln IS NOT NULL.

            IF sy-subrc IS INITIAL.
              IF <fs_campo1> IS NOT INITIAL.
                vl_cor = 5.
              ELSE.
                vl_cor = 3.
              ENDIF.
            ELSE.
              IF <fs_campo1> IS NOT INITIAL.
                vl_cor = 5.
              ELSE.
                vl_cor = 6.
              ENDIF.
            ENDIF.

            PERFORM cor USING <fcat>-fieldname vl_cor CHANGING <w_0045>-cellcolor.

          WHEN OTHERS.
            IF <fs_campo1> IS NOT INITIAL.
              vl_cor = 5.
            ELSE.
              vl_cor = 6.
            ENDIF.

            PERFORM cor USING <fcat>-fieldname vl_cor CHANGING <w_0045>-cellcolor.
        ENDCASE.

      ENDLOOP.
    ELSE.
      FREE <w_0045>-cellcolor.
      LOOP AT it_fcat ASSIGNING <fcat> WHERE outputlen EQ 1.
        vl_cor = 0.

        campo = <fcat>-fieldname.
        ASSIGN COMPONENT campo OF STRUCTURE <w_0045> TO <fs_campo1>.

        CASE campo.
          WHEN 'NF_EXPORT'.
            SELECT COUNT(*)
              FROM zsdt0053
              WHERE instrucao EQ <w_0045>-instrucao
                AND vbeln IS NOT NULL.

            IF sy-subrc IS INITIAL.
              IF <fs_campo1> IS NOT INITIAL.
                vl_cor = 5.
              ELSE.
                vl_cor = 3.
              ENDIF.
            ELSE.
              IF <fs_campo1> IS NOT INITIAL.
                vl_cor = 5.
              ELSE.
                vl_cor = 6.
              ENDIF.
            ENDIF.

            PERFORM cor USING <fcat>-fieldname vl_cor CHANGING <w_0045>-cellcolor.

          WHEN OTHERS.
            IF <fs_campo1> IS NOT INITIAL.
              vl_cor = 5.
            ELSE.
              vl_cor = 6.
            ENDIF.

            PERFORM cor USING <fcat>-fieldname vl_cor CHANGING <w_0045>-cellcolor.
        ENDCASE.
      ENDLOOP.
    ENDIF.


* 3 AMARELO
* 5 VERDE
* 6 VERMELHO

*    VL_COR = 6.
*
*    SELECT COUNT(*)
*      FROM ZSDT0053
*      WHERE INSTRUCAO EQ <W_0045>-INSTRUCAO
*        AND VBELN IS NOT NULL.
*
*    IF SY-SUBRC IS INITIAL.
*      V_COR = 3.
*    ENDIF.
*
*    LOOP AT IT_0236 ASSIGNING FIELD-SYMBOL(<F_0236>) WHERE INSTRUCAO EQ <W_0045>-INSTRUCAO.
*      MOVE-CORRESPONDING <F_0236> TO <W_0045>.
*      FREE <W_0045>-CELLCOLOR.
*      LOOP AT IT_FCAT ASSIGNING FIELD-SYMBOL(<FCAT>) WHERE OUTPUTLEN EQ 1.
*        CAMPO = <FCAT>-FIELDNAME.
*        ASSIGN COMPONENT CAMPO OF STRUCTURE <F_0236> TO <FS_CAMPO1>.
*        IF <FCAT>-FIELDNAME NE 'NF_EXPORT'.
*          VL_COR = COND #( WHEN <FS_CAMPO1> IS INITIAL THEN '6' ELSE '5' ).
*        ELSE.
*          IF V_COR NE 3.
*            VL_COR = COND #( WHEN <FS_CAMPO1> IS INITIAL THEN '6' ELSE '5' ).
*          ELSE.
*            VL_COR = COND #( WHEN <FS_CAMPO1> IS INITIAL THEN V_COR ELSE '5' ).
*          ENDIF.
*          CLEAR V_COR.
*        ENDIF.
*        PERFORM COR USING <FCAT>-FIELDNAME VL_COR CHANGING <W_0045>-CELLCOLOR.
*        VL_COR = 6.
*      ENDLOOP.
*    ENDLOOP.
*
*    IF SY-SUBRC IS NOT INITIAL.
*      FREE <W_0045>-CELLCOLOR.
*      LOOP AT IT_FCAT ASSIGNING <FCAT> WHERE OUTPUTLEN EQ 1.
*        IF <FCAT>-FIELDNAME EQ 'NF_EXPORT'.
*          VL_COR = V_COR.
*        ELSE.
*          VL_COR = 6.
*        ENDIF.
*        PERFORM COR USING <FCAT>-FIELDNAME VL_COR CHANGING <W_0045>-CELLCOLOR.
*      ENDLOOP.
*    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_EZSDT0053
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enqueue_ezsdt0053 .

  CALL FUNCTION 'ENQUEUE_EZSDT0053'
    EXPORTING
      name           = 'ZSDT0053'
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc NE 0.
    IF sy-msgv1 NE sy-uname.
      MESSAGE |Usuário { sy-msgv1 } já está em modo de edição, será aberto no modo de Exibição.| TYPE 'S' DISPLAY LIKE 'E'.
      block = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2219   text
*      -->P_WL_NAME  text
*----------------------------------------------------------------------*
FORM read_text USING p_0045 TYPE ty_0045.

  DATA:
    it_text_tab	TYPE STANDARD TABLE OF string,
    it_text1    TYPE TABLE OF txw_note,
    wa_text_tab	LIKE LINE OF it_text_tab.

  "populate fields of struture and append to itab
  APPEND wa_text_tab TO it_text_tab.

  CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
    EXPORTING
      text                = p_0045-observacao
      flag_no_line_breaks = ''
      line_length         = 72
      langu               = sy-langu
    TABLES
      text_tab            = it_text1. "it_text_tab.

  CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
    TABLES
      t_txwnote = it_text1. "it_text_tab.

  CALL FUNCTION 'SOTR_SERV_TABLE_TO_STRING'
    EXPORTING
      flag_no_line_breaks = ' '
      line_length         = 72
      langu               = sy-langu
    IMPORTING
      text                = p_0045-observacao
    TABLES
      text_tab            = it_text1. "it_text_tab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0110 OUTPUT.
  SET PF-STATUS 'PF0110'.
  SET TITLEBAR 'T0110'.

  PERFORM zf_preparar_alv_list_vsr.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.

  CASE vg_ok_0110.

    WHEN 'CONS'.

*      "Tabela Detalhe Resumo Fluxo Caixa Prev. - Fluxo Financeiro
*      IF s_bukvrs IS NOT INITIAL OR s_dtvrs  IS NOT INITIAL.
*
*        SELECT dt_base_versao bukrs versao hora_versao FROM zfit0111
*          APPENDING CORRESPONDING FIELDS OF TABLE t_nfe_fl
*          WHERE bukrs         IN s_bukvrs
*          AND dt_base_versao  IN s_dtvrs.
*
*        SORT t_nfe_fl BY bukrs dt_base_versao versao.
*        DELETE ADJACENT DUPLICATES FROM t_nfe_fl COMPARING bukrs dt_base_versao versao.
*
*        "Descrição da empresa
*        SELECT bukrs, butxt FROM t001
*         INTO TABLE  @DATA(t_t001)
*          FOR ALL ENTRIES IN @t_nfe_fl
*          WHERE bukrs = @t_nfe_fl-bukrs.
*
*        LOOP AT t_nfe_fl ASSIGNING FIELD-SYMBOL(<fs_nfe_fl>).
*
*          READ TABLE t_t001 INTO DATA(w_t001) WITH KEY bukrs = <fs_nfe_fl>-bukrs.
*          IF sy-subrc IS INITIAL.
*            <fs_nfe_fl>-butxt = w_t001-butxt.
*          ENDIF.
*
*        ENDLOOP.

*      ENDIF.
*
*      CALL METHOD v_grid_110->refresh_table_display
*        EXPORTING
*          is_stable = wa_stable.

    WHEN 'CANC'.

      SET SCREEN 0.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_ALV_LIST_VSR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preparar_alv_list_vsr .


*&---------------------------------------------------------------------*
*&  Declaração de Tabela Interna
*&---------------------------------------------------------------------*
  DATA: t_sort_110 TYPE lvc_t_sort,
        t_fcat_110 TYPE lvc_t_fcat.

*&---------------------------------------------------------------------*
*&  Declaração de Work Área
*&---------------------------------------------------------------------*
  DATA: w_variant_110 TYPE disvariant,
        w_layout_110  TYPE lvc_s_layo.

*&---------------------------------------------------------------------*
*&  Declaração de GRID - 0109
*&---------------------------------------------------------------------*
  DATA:
    v_docking  TYPE REF TO cl_gui_docking_container,
    v_splitter TYPE REF TO cl_gui_splitter_container.

*&---------------------------------------------------------------------*
*&  Declaração de Container
*&---------------------------------------------------------------------*
  DATA: v_container_110 TYPE REF TO cl_gui_custom_container.

  w_layout_110-cwidth_opt = 'X'.
  w_layout_110-sel_mode   = 'D'.
  w_layout_110-sel_mode   = 'D'.

  w_variant_110-report    = sy-repid.

  CREATE OBJECT v_container_110
    EXPORTING
      container_name              = 'CONTAINER'
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

  IF v_grid_110 IS INITIAL.

    PERFORM zf_montar_fieldcat  CHANGING it_nfe_fl t_fcat_110.
    PERFORM zf_descricao_campos CHANGING t_fcat_110.

    CREATE OBJECT v_grid_110
      EXPORTING
        i_parent          = v_container_110
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT v_event_receiver_110.

*----------------------------------------------------------------------*
*** Define eventos
*----------------------------------------------------------------------*
    SET HANDLER v_event_receiver_110->handle_toolbar FOR v_grid_110.

    w_layout_110-sel_mode = 'A'.

    CALL METHOD v_grid_110->set_table_for_first_display
      EXPORTING
        is_variant                    = w_variant_110
        i_save                        = 'A'
        is_layout                     = w_layout_110
      CHANGING
        it_fieldcatalog               = t_fcat_110
        it_outtab                     = it_nfe_fl
        it_sort                       = t_sort_110
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD v_grid_110->set_toolbar_interactive.

  ELSE.
    CALL METHOD v_grid_110->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_nfe_fl  text
*      <--P_T_FCAT_110  text
*----------------------------------------------------------------------*
FORM zf_montar_fieldcat   CHANGING pt_tabela   TYPE ANY TABLE
                                  pt_fieldcat TYPE lvc_t_fcat.

  DATA:
    l_columns      TYPE REF TO cl_salv_columns_table,
    l_aggregations TYPE REF TO cl_salv_aggregations,
    l_salv_table   TYPE REF TO cl_salv_table,
    l_data         TYPE REF TO data.
  FIELD-SYMBOLS:
    <f_table>      TYPE STANDARD TABLE.

* Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA l_data LIKE pt_tabela.
  ASSIGN l_data->* TO <f_table>.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Monta a estrutura dinâmica no objeto l_salv_table
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = l_salv_table
        CHANGING
          t_table      = <f_table> ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* Recupera as colunas e dados internos
  l_columns      = l_salv_table->get_columns( ).
  l_aggregations = l_salv_table->get_aggregations( ).

* Monta o fieldcat
  pt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = l_columns
                                                                   r_aggregations = l_aggregations ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_DESCRICAO_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FCAT_110  text
*----------------------------------------------------------------------*
FORM zf_descricao_campos CHANGING pt_fcat TYPE lvc_t_fcat.

  LOOP AT pt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    CASE <fs_fcat>-fieldname.
      WHEN 'DOCNUM'.
        <fs_fcat>-scrtext_l = 'DocNum'.
      WHEN 'NFENUM'.
        <fs_fcat>-scrtext_l = 'Nº NFe'.
      WHEN 'NFENUM'.
        <fs_fcat>-scrtext_l = 'Data NFe'.
      WHEN 'MENGE'.
        <fs_fcat>-scrtext_l = 'Data NFe'.
      WHEN 'MENGE'.
        <fs_fcat>-scrtext_l = 'Peso Nfe'.
      WHEN 'BRANCH'.
        <fs_fcat>-scrtext_l = 'Filial'.
      WHEN 'PLACA_CAV'.
        <fs_fcat>-scrtext_l = 'Placa Cavalo'.
      WHEN 'DATACHEGADA'.
        <fs_fcat>-scrtext_l = 'Dt Recep. Terminal'.
      WHEN OTHERS.
    ENDCASE.

    <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
    <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-reptext.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ELIMINA_BOTOES_nfe_fl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM elimina_botoes_nfe_fl    USING  e_object TYPE REF TO cl_alv_event_toolbar_set.

*    elimina itens desnecessarios da barra do container
  DELETE e_object->mt_toolbar WHERE function = '&LOCAL&APPEND'
                                 OR function = '&LOCAL&INSERT_ROW'
                                 OR function = '&LOCAL&DELETE_ROW'
                                 OR function = '&LOCAL&COPY_ROW'
                                 OR function = '&LOCAL&CUT'
                                 OR function = '&LOCAL&COPY'
                                 OR function = '&LOCAL&PASTE'
                                 OR function = '&REFRESH'
                                 OR function = '&CHECK'
                                 OR function = '&GRAPH'
                                 OR function = '&INFO'
                                 OR function = '&LOCAL&UNDO'
                                 OR function = '&MB_VIEW'
                                 OR function = '&MB_VARIANT'
                                 OR function =  '&MB_EXPORT'
                                 OR function =  '&MB_SUM'
                                 OR function =  '&MB_SUBTOT'
                                 OR function =  '&PRINT_BACK'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form lfa1_endereco
*&---------------------------------------------------------------------*
FORM lfa1_endereco  USING p_lifnr CHANGING p_stras p_ort02 p_ort01 p_pstlz p_regio.
  SELECT SINGLE ort01 ort02 pstlz regio stras FROM lfa1 INTO ( p_ort01, p_ort02, p_pstlz, p_regio, p_stras ) WHERE lifnr EQ p_lifnr.
ENDFORM.
