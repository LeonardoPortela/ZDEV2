**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|  Funcional:                                                               |*
**|    + Leonardo Portela ( leonardo.portela@amaggi.com.br )                  |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Manutenção - Fluxo de Exportação                                          |*
**/===========================================================================\*
*&----------------------------------------------------------------------------&*
*&                    Histórico de Modificações                               &*
*& Autor ABAP |Request    |Data       |Descrição                              &*
*&----------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A1XAW |18/02/2025 |Ordenar o Relatório por data e quando  &*
*&                                    |Status cadeado fechado, se NF cancelada&*
*&                                    |não mostrar o cancelamento no relatório&*
*&                                    |e implantação do vínculo manual.       &*
*&                                    |Chamado - Bug Solto 167326 da História &*
*&                                    |156950.                                &*
*&----------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A1XAW |19/03/2025 |Implementação dos Botões de chamada dos&*
*&                                    |programas de "Cadastro Aprovador" e de &*
*&                                    |"Cockpit Aprovação". Chamado - 169312. &*
*&----------------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A1XAW |27/03/2025 |Ajsute da exibição do conteúdo do campo&*
*&                                    |de marcação Vinculo Manual e demais    &*
*&                                    |campos do Relatório Manutenção da Fila.&*
*&                                    |BUG 172407.                            &*
*-----------------------------------------------------------------------------&*
REPORT zsdr0195.

TABLES: t001w, j_1bnfdoc.
**<<<------"169312 - NMS - INI------>>>
TABLES: sscrfields.
**<<<------"169312 - NMS - FIM------>>>
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.

  PARAMETERS: pmfila RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND rd1,
              pmnota RADIOBUTTON GROUP rad1.

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
**<<<------"169312 - NMS - INI------>>>
SELECTION-SCREEN: FUNCTION KEY 1, "Declaração do Botão Cadastro Aprovador
                  FUNCTION KEY 2. "Declaração do Botão Cockpit Aprovação
**<<<------"169312 - NMS - FIM------>>>
  SELECT-OPTIONS: scentro  FOR t001w-werks NO INTERVALS,
                  scentror FOR t001w-werks NO INTERVALS,
                  sdt_emi  FOR j_1bnfdoc-pstdat,
                  sdocnum  FOR j_1bnfdoc-docnum NO INTERVALS,
                  s_flote  FOR j_1bnfdoc-docnum NO INTERVALS,
                  s_prod   FOR j_1bnfdoc-docnum NO INTERVALS,
                  s_emis   FOR j_1bnfdoc-docdat NO INTERVALS.

  SELECTION-SCREEN SKIP.
  PARAMETERS: ckb_vinc TYPE c AS CHECKBOX USER-COMMAND vin. "<<<------"167326 - NMS ------>>>
  PARAMETERS: p_ins LIKE h99cwtr-arch_read DEFAULT space USER-COMMAND us1.
  PARAMETERS: p_lsn LIKE h99cwtr-shownullrecords DEFAULT space.

  SELECTION-SCREEN SKIP.

SELECTION-SCREEN: END OF BLOCK b2.

TYPES: BEGIN OF ty_saida.
TYPES: block                  TYPE c LENGTH 4,
       mark                   TYPE c LENGTH 1,
       saldo_nao_disp_icon    TYPE c LENGTH 4, "<<<------"172407 - NMS ------>>>
       compra_fim_es_icon     TYPE c LENGTH 4,
       romaneio_completo_icon TYPE c LENGTH 4,
       eudr_icon              TYPE c LENGTH 4,
       entrada_transf_icon    TYPE c LENGTH 4,
       manual_icon            TYPE c LENGTH 4, "<<<------"172407 - NMS ------>>>
       cancel_icon            TYPE c LENGTH 4.
       INCLUDE TYPE zsdtprod_flote.
TYPES: ln_color               TYPE char4.      "<<<------"167326 - NMS ------>>>
TYPES: END OF ty_saida.

TYPES: BEGIN OF ty_ucomm,
         ucomm TYPE  sy-ucomm,
       END OF ty_ucomm,
**<<<------"167326 - NMS - INI------>>>
       BEGIN OF ty_docs,
         docnum_flote TYPE zdocnum_flote,
         docnum_eprod TYPE zdocnum_eprod,
         row_id       TYPE int4,
       END OF ty_docs,
**<<<------"167326 - NMS - FIM------>>>
       BEGIN OF ty_vinc,
         celltab       TYPE lvc_t_styl,
         qtd_vinc_orig TYPE zqtd_vinc.
         INCLUDE TYPE zsdtvinc_p_flote.
TYPES END OF ty_vinc.

TYPES:BEGIN OF ty_vinc_aux,
        celltab       TYPE lvc_t_styl,
        doc_eprod_ant TYPE j_1bdocnum,
        qtd_vinc_ant  TYPE zqtd_vinc.
        INCLUDE TYPE zsdtvinc_p_flote.
TYPES END OF ty_vinc_aux.

DATA: gt_saida         TYPE TABLE OF ty_saida,
      ls_formacao_lote TYPE zsdtprod_flote,
      lv_docnum        TYPE j_1bdocnum,
      lv_formacao_lote TYPE c LENGTH 01,
      lt_formacao_lote TYPE zdesd_formacao_lote,
      lt_ucomm         TYPE TABLE OF ty_ucomm,
      str              TYPE REF TO data,
      gt_fcat          TYPE lvc_t_fcat,
      grid             TYPE REF TO cl_gui_alv_grid,
      gs_variant       TYPE disvariant,
      gt_exclude       TYPE ui_functions,
      gs_exclude       TYPE ui_func,
      container        TYPE REF TO cl_gui_docking_container,
      lo_alv           TYPE REF TO cl_salv_table,   " Objeto ALV
      lt_columns       TYPE REF TO cl_salv_columns_table, " Colunas do ALV
      lo_functions     TYPE REF TO cl_salv_functions_list, " Funções do ALV
      lr_selections    TYPE REF TO cl_salv_selections,
      gt_vinc          TYPE TABLE OF ty_vinc,
      gt_vinc_aux      TYPE TABLE OF ty_vinc_aux,
      gt_canceladas    TYPE TABLE OF ty_vinc,
      gt_novos_doc     TYPE TABLE OF ty_vinc_aux,
      tg_docs          TYPE TABLE OF ty_docs, "<<<------"167326 - NMS------>>>
      go_api           TYPE REF TO if_salv_gui_om_extend_grid_api,
      go_edit          TYPE REF TO if_salv_gui_om_edit_restricted,
      gv_qtd_tot_vinc  TYPE zqtd_vinc.
**<<<------"167326 - NMS - INI------>>>
DATA: r_docnum  TYPE RANGE OF j_1bdocnum.

DATA: vg_alt_vinc TYPE c,
      vg_err_save TYPE c.
**<<<------"167326 - NMS - FIM------>>>
INCLUDE zsdr0195_cls.

DATA: "r_docnum  TYPE RANGE OF j_1bdocnum, "<<<------"167326 - NMS------>>>
      gr_events TYPE REF TO lcl_handle_events.

INCLUDE zsdr0195_f01.
**<<<------"169312 - NMS - INI------>>>
*---------------------------------------------
* Monta o botão no evento Initialization     *
*---------------------------------------------
INITIALIZATION.
* Monta o boptão da tela de seleção
 PERFORM zf_cria_botao.
**<<<------"169312 - NMS - FIM------>>>
AT SELECTION-SCREEN.
**<<<------"169312 - NMS - INI------>>>
  CASE sscrfields-ucomm.
    WHEN'FC01'.
      CALL TRANSACTION 'ZSDT0345' AND SKIP FIRST SCREEN.
      LEAVE SCREEN.

    WHEN'FC02'.
      CALL TRANSACTION 'ZSDT0346' AND SKIP FIRST SCREEN.
      LEAVE SCREEN.

    WHEN OTHERS.
* Do nothing.

  ENDCASE.
**<<<------"169312 - NMS - FIM------>>>
  IF sy-ucomm NE 'RD1'.
    IF p_ins IS INITIAL AND pmnota IS INITIAL.
      IF sdocnum[] IS INITIAL AND
        scentro[] IS INITIAL AND
        scentror[] IS INITIAL AND
        sdt_emi[] IS INITIAL AND
        sdocnum[] IS INITIAL.
        MESSAGE s000(z01) WITH 'Preencha ao menos um Campo do Filtro!' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

    ELSEIF pmnota IS NOT INITIAL.
**<<<------"167326 - NMS - INI------>>>
      CHECK ckb_vinc IS INITIAL.
**<<<------"167326 - NMS - FIM------>>>
      IF s_flote IS INITIAL AND
         s_prod  IS INITIAL AND
         s_emis  IS INITIAL.
        CHECK sy-ucomm NE 'VIN'."<<<------"167326 - NMS------>>>
        MESSAGE s000(z01) WITH 'Preencha ao menos um Campo do Filtro!' DISPLAY LIKE 'E'.
        STOP.
      ENDIF.

    ENDIF.

  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF p_ins IS NOT INITIAL.
      CASE screen-name.
        WHEN 'SCENTRO-LOW' OR
             'SCENTROR-LOW' OR
             'SDT_EMI-LOW' OR
             'SDOCNUM-LOW'.
          screen-required = 0.
        WHEN OTHERS.
      ENDCASE.
**<<<------"167326 - NMS - INI------>>>
      IF screen-name CS 'CKB_VINC'  AND
         pmnota      IS INITIAL.
        screen-active = 0.
      ENDIF.
**<<<------"167326 - NMS - FIM------>>>
    ELSE.
      IF screen-name EQ 'SDOCNUM-LOW' OR
         screen-name EQ 'SCENTRO-LOW' OR
         screen-name EQ 'SCENTROR-LOW' OR
         screen-name EQ 'SDT_EMI-LOW'.
        screen-required = 2.
      ENDIF.
**<<<------"167326 - NMS - INI------>>>
      IF screen-name CS 'CKB_VINC'  AND
         pmnota      IS INITIAL.
        screen-active = 0.
      ENDIF.
**<<<------"167326 - NMS - FIM------>>>
    ENDIF.

    IF pmnota IS NOT INITIAL.
      IF screen-name NS 'S_FLOTE'  AND
         screen-name NS 'S_PROD'   AND
         screen-name NS 'S_EMIS'   AND
         screen-name NS 'PMFILA'   AND
         screen-name NS 'PMNOTA'   AND
         screen-name NS 'CKB_VINC' AND "<<<------"167326 - NMS ------>>>
         screen-name NS 'BLOCK' .
        screen-active = 0.
**<<<------"167326 - NMS - INI------>>>
      ELSE.
        IF NOT ckb_vinc IS INITIAL.
          IF screen-name CS 'S_FLOTE' OR
             screen-name CS 'S_PROD' OR
             screen-name CS 'S_EMIS'.
            screen-active = 0.

          ENDIF.

        ENDIF.
**<<<------"167326 - NMS - FIM------>>>
      ENDIF.

    ELSE.
      IF NOT ckb_vinc IS INITIAL.
        CLEAR ckb_vinc.

      ENDIF.

      IF screen-name CS 'S_FLOTE' OR
         screen-name CS 'S_PROD' OR
         screen-name CS 'S_EMIS'.
        screen-active = 0.
      ENDIF.

    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

  IF pmfila IS NOT INITIAL.

    PERFORM f_seleciona_dados.

    CALL SCREEN 0100.

  ELSEIF pmnota IS NOT INITIAL.

    PERFORM f_altera_docnum_vinculado.

  ENDIF.

END-OF-SELECTION.


*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

**<<<------"167326 - NMS - INI------>>>
  DATA: el_layout TYPE lvc_s_layo.
**<<<------"167326 - NMS - FIM------>>>

  gt_exclude =
  VALUE #(
            ( cl_gui_alv_grid=>mc_fc_check )
            ( cl_gui_alv_grid=>mc_fc_loc_append_row )
            ( cl_gui_alv_grid=>mc_fc_loc_copy )
            ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
            ( cl_gui_alv_grid=>mc_fc_loc_cut )
            ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
            ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
            ( cl_gui_alv_grid=>mc_fc_loc_paste )
            ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
            ( cl_gui_alv_grid=>mc_fc_refresh )
         ).

  SET PF-STATUS 'PF0100' EXCLUDING lt_ucomm.
  SET TITLEBAR 'TL0100'.

  FREE gt_fcat.

  ASSIGN 'ty_saida' TO FIELD-SYMBOL(<fs_str>).
  CREATE DATA str TYPE (<fs_str>).

  gt_fcat =
  CORRESPONDING lvc_t_fcat(
  cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr(
  cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

  LOOP AT gt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

    CASE <fs_fcat>-fieldname.
**<<<------"172407 - NMS - INI------>>>
*      WHEN 'COMPRA_FIM_ES_ICON' OR
      WHEN 'SALDO_NAO_DISP_ICON' OR
           'COMPRA_FIM_ES_ICON_ICON' OR
**<<<------"172407 - NMS - FIM------>>>
           'ROMANEIO_COMPLETO_ICON' OR
           'EUDR_ICON' OR
           'ENTRADA_TRANSF_ICON' OR
           'MANUAL_ICON' OR "<<<------"172407 - NMS------>>>
           'CANCEL_ICON'.
        <fs_fcat>-col_pos = sy-tabix + 40.
      WHEN OTHERS.
        <fs_fcat>-col_pos = sy-tabix.
    ENDCASE.

    CASE <fs_fcat>-fieldname.
      WHEN 'MANDT' OR
           'COMPRA_FIM_ES' OR
           'ROMANEIO_COMPLETO' OR
           'EUDR' OR
           'ENTRADA_TRANSF' OR
**<<<------"172407 - NMS - INI------>>>
           'MANUAL' OR
           'SALDO_NAO_DISPONIVEL' OR
**<<<------"172407 - NMS - FIM------>>>
           'US_CRIACAO' OR
           'DT_CRIACAO' OR
           'HR_CRIACAO' OR
           'CANCEL' OR
           'US_CANCEL' OR
           'DT_CANCEL' OR
           'HR_CANCEL'.

        <fs_fcat>-no_out = abap_true.
**<<<------"172407 - NMS - INI------>>>
* Inibe o campo na sele;áo de campos de modifica;áo de layout.
        IF <fs_fcat>-fieldname EQ 'MANDT'                OR
           <fs_fcat>-fieldname EQ 'COMPRA_FIM_ES'        OR
           <fs_fcat>-fieldname EQ 'SALDO_NAO_DISPONIVEL' OR
           <fs_fcat>-fieldname EQ 'ROMANEIO_COMPLETO'    OR
           <fs_fcat>-fieldname EQ 'EUDR'                 OR
           <fs_fcat>-fieldname EQ 'MANUAL'               OR
           <fs_fcat>-fieldname EQ 'CANCEL'               OR
           <fs_fcat>-fieldname EQ 'ENTRADA_TRANSF'.
            <fs_fcat>-tech = abap_on.
**<<<------"172407 - NMS - FIM------>>>
        ENDIF.

      WHEN 'COMPRA_FIM_ES_ICON' OR
           'ROMANEIO_COMPLETO_ICON' OR
           'EUDR_ICON' OR
           'ENTRADA_TRANSF_ICON' OR
           'BLOCK' OR
**<<<------"172407 - NMS - INI------>>>
           'SALDO_NAO_DISP_ICON' OR
           'MANUAL_ICON' OR "<<<------"172407 - NMS------>>>
**<<<------"172407 - NMS - FIM------>>>
           'CANCEL_ICON'.

        <fs_fcat>-icon = abap_true.

        CASE <fs_fcat>-fieldname.
          WHEN 'CANCEL_ICON'.
            <fs_fcat>-reptext   = 'Cancelado'.
            <fs_fcat>-scrtext_l = 'Cancelado'.
            <fs_fcat>-scrtext_m = 'Cancelado'.
            <fs_fcat>-scrtext_s = 'Cancelado'.
          WHEN 'COMPRA_FIM_ES_ICON'.
            <fs_fcat>-reptext   = 'Fim Especifico'.
            <fs_fcat>-scrtext_l = 'Fim Especifico'.
            <fs_fcat>-scrtext_m = 'Fim Especifico'.
            <fs_fcat>-scrtext_s = 'Fim Esp.'.
          WHEN 'ROMANEIO_COMPLETO_ICON'.
            <fs_fcat>-reptext   = 'Romaneio Completo'.
            <fs_fcat>-scrtext_l = 'Romaneio Completo'.
            <fs_fcat>-scrtext_m = 'Romaneio Completo'.
            <fs_fcat>-scrtext_s = 'Rom. Comp.'.
          WHEN 'ENTRADA_TRANSF_ICON'.
            <fs_fcat>-reptext   = 'Transferencia'.
            <fs_fcat>-scrtext_l = 'Transferencia'.
            <fs_fcat>-scrtext_m = 'Transferencia'.
            <fs_fcat>-scrtext_s = 'Transf.'.
          WHEN 'EUDR_ICON'.
            <fs_fcat>-reptext   = 'EDUR'.
            <fs_fcat>-scrtext_l = 'EDUR'.
            <fs_fcat>-scrtext_m = 'EDUR'.
            <fs_fcat>-scrtext_s = 'EDUR'.
          WHEN 'BLOCK'.
            <fs_fcat>-reptext   = 'Status'.
            <fs_fcat>-scrtext_l = 'Status'.
            <fs_fcat>-scrtext_m = 'Status'.
            <fs_fcat>-scrtext_s = 'Status'.
**<<<------"172407 - NMS - INI------>>>
          WHEN 'MANUAL_ICON'.
            <fs_fcat>-reptext   = 'Manual'.
            <fs_fcat>-scrtext_l = 'Manual'.
            <fs_fcat>-scrtext_m = 'Manual'.
            <fs_fcat>-scrtext_s = 'Manual'.
          WHEN 'SALDO_NAO_DISP_ICON'.
            <fs_fcat>-reptext   = 'Sld. não Disp.'.
            <fs_fcat>-scrtext_l = 'Sld. não Disp.'.
            <fs_fcat>-scrtext_m = 'Sld. não Disp.'.
            <fs_fcat>-scrtext_s = 'Sld. não Disp.'.
**<<<------"172407 - NMS - FIM------>>>
        ENDCASE.

      WHEN 'MARK'.
        <fs_fcat>-checkbox = abap_true.
        <fs_fcat>-outputlen = '03'.
        <fs_fcat>-edit = COND #( WHEN p_ins IS NOT INITIAL THEN abap_false ELSE abap_true ).
        <fs_fcat>-scrtext_l = 'Check'.
        <fs_fcat>-scrtext_m = 'Check'.
        <fs_fcat>-scrtext_s = 'Check'.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

  IF container IS INITIAL.

    CREATE OBJECT container
      EXPORTING
        repid     = sy-repid
        dynnr     = '0100'
        extension = 2000.

    CREATE OBJECT grid
      EXPORTING
        i_parent = container.

    gs_variant-report = sy-repid.
**<<<------"167326 - NMS - INI------>>>
    el_layout-info_fname = 'LN_COLOR'.
**<<<------"167326 - NMS - FIM------>>>
    CALL METHOD grid->set_table_for_first_display
      EXPORTING
        is_variant           = gs_variant
        i_default            = abap_true
**<<<------"167326 - NMS - INI------>>>
        is_layout            = el_layout
**<<<------"167326 - NMS - FIM------>>>
        i_save               = 'A'
        it_toolbar_excluding = gt_exclude
      CHANGING
        it_fieldcatalog      = gt_fcat
        it_outtab            = gt_saida.

    CALL METHOD grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  ELSE.

    CALL METHOD grid->refresh_table_display
      EXPORTING
        is_stable = CONV #( 'XX' ).

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: is_ok     TYPE c,
        lv_linhas TYPE i.

  CLEAR ls_formacao_lote.

  CASE sy-ucomm.
******************************RETORNA******************************
    WHEN zcl_im_cl_fluxo_exportacao=>lc_contante-acao-back.

      LEAVE TO SCREEN 0.

******************************CANCELA******************************
    WHEN zcl_im_cl_fluxo_exportacao=>lc_contante-acao-cancel.

      LEAVE LIST-PROCESSING AND RETURN TO SCREEN 0100.

******************************SAIDA******************************
    WHEN zcl_im_cl_fluxo_exportacao=>lc_contante-acao-exit.

      LEAVE PROGRAM.

*"// Bloqueia a Linha Marcada
******************************BLOQUEIAR******************************
    WHEN zcl_im_cl_fluxo_exportacao=>lc_contante-acao-block.

      LOOP AT gt_saida INTO DATA(ls_saida) WHERE mark IS NOT INITIAL.

        MOVE-CORRESPONDING ls_saida TO ls_formacao_lote.

        zcl_im_cl_fluxo_exportacao=>manut_fila(
          i_docnum        = ls_formacao_lote-docnum
          i_operacao      = zcl_im_cl_fluxo_exportacao=>lc_contante-operacao-bloqueiar
          i_formacao_lote = ls_formacao_lote
        ).

      ENDLOOP.

*"// Atualiza os dados da GRID
******************************ATUALIZA******************************
    WHEN zcl_im_cl_fluxo_exportacao=>lc_contante-acao-change.

      PERFORM f_seleciona_dados.

*"// Marca como Cancelado a Linha Marcada
******************************DELETA******************************
    WHEN zcl_im_cl_fluxo_exportacao=>lc_contante-acao-delete.

      READ TABLE gt_saida INTO ls_saida WITH KEY mark = abap_true.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE 'Nenhuma Linha Selecionada!' TYPE 'I'.
        EXIT.
      ENDIF.

      lv_linhas = REDUCE i( INIT x = 0 FOR _saida IN gt_saida WHERE ( mark = abap_true ) NEXT x = x + 1 ).
      IF lv_linhas NE 1.
        MESSAGE 'Para EXCLUIR, Selecione apenas uma LINHA!' TYPE 'I'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Deseja EXCLUIR o Documento?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = is_ok
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF is_ok NE '1'.
        EXIT.
      ENDIF.

      FREE: r_docnum.
      APPEND VALUE #(
               sign    = zcl_les_utils=>if_stab_constants~mc_sign_include
               option  = zcl_les_utils=>if_stab_constants~mc_option_equal
               low     = ls_saida-docnum
             ) TO r_docnum.

      DATA(ls_vinculo) = zcl_im_cl_fluxo_exportacao=>check_nfe_fila_vinculo(  i_docnum = r_docnum[] ).
      IF ls_vinculo-zsdtvinc_p_flote IS NOT INITIAL.
        CLEAR ls_formacao_lote.
        MESSAGE 'NF-e já Vinculada, operação não permitida' TYPE 'I'.
        EXIT.
      ENDIF.

      MOVE-CORRESPONDING ls_saida TO ls_formacao_lote.

      zcl_im_cl_fluxo_exportacao=>manut_fila(
        i_docnum        = ls_formacao_lote-docnum
        i_operacao      = zcl_im_cl_fluxo_exportacao=>lc_contante-operacao-excluir
        i_formacao_lote = ls_formacao_lote
      ).

*"// Inserir um novo Registro
******************************INSERE******************************
    WHEN zcl_im_cl_fluxo_exportacao=>lc_contante-acao-insert.

      CALL SCREEN 0110 STARTING AT 07 05 ENDING AT 72 11.

*"// Atualiza o SALDO da linha selecionada
    WHEN zcl_im_cl_fluxo_exportacao=>lc_contante-acao-upbalance.

      READ TABLE gt_saida INTO ls_saida WITH KEY mark = abap_true.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE 'Nenhuma Linha Selecionada!' TYPE 'I'.
        CLEAR: ls_formacao_lote.
        EXIT.
      ENDIF.

      MOVE-CORRESPONDING ls_saida TO ls_formacao_lote.

      ls_formacao_lote-docnum = ls_saida-docnum.
      ls_formacao_lote-itmnum = ls_saida-itmnum.
      ls_formacao_lote-seq = ls_saida-seq.
      ls_formacao_lote-material = ls_saida-material.
      ls_formacao_lote-werks = ls_saida-werks.
      ls_formacao_lote-werks_real = ls_saida-werks_real.
      ls_formacao_lote-qtd_nf = ls_saida-qtd_nf.
      ls_formacao_lote-data_emissao = ls_saida-data_emissao.
      ls_formacao_lote-compra_fim_es = ls_saida-compra_fim_es_icon.
      ls_formacao_lote-eudr = ls_saida-eudr.
      ls_formacao_lote-entrada_transf = ls_saida-entrada_transf.
      ls_formacao_lote-romaneio_completo = ls_saida-romaneio_completo.
      ls_formacao_lote-cancel = ls_saida-cancel.

      CALL SCREEN 0111 STARTING AT 07 05 ENDING AT 72 11.

    WHEN OTHERS.
  ENDCASE.

  PERFORM f_seleciona_dados.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0110 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0110 OUTPUT.

  SET PF-STATUS 'PF0110'.
  SET TITLEBAR 'TL0110'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0110 INPUT.
  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      FREE: r_docnum.
      APPEND VALUE #(
               sign    = zcl_les_utils=>if_stab_constants~mc_sign_include
               option  = zcl_les_utils=>if_stab_constants~mc_option_equal
               low     = lv_docnum
             ) TO r_docnum.

      DATA(vinculo_2) = zcl_im_cl_fluxo_exportacao=>check_nfe_fila_vinculo(  i_docnum = r_docnum[] ).
      IF vinculo_2-zsdtprod_flote IS NOT INITIAL.
        CLEAR ls_formacao_lote.
        MESSAGE 'NF-e já incluido na Fila, operação não permitida' TYPE 'I'.
        EXIT.
      ENDIF.

      IF ls_formacao_lote-saldo_disponivel IS INITIAL.
        MESSAGE 'Informar Valor no Campo SALDO!' TYPE 'I'.
        EXIT.
      ENDIF.

      IF ls_formacao_lote-qtd_nf < ls_formacao_lote-saldo_disponivel.
        MESSAGE 'Valor Superior ao Permitido!' TYPE 'I'.
        EXIT.
      ENDIF.

      zcl_im_cl_fluxo_exportacao=>manut_fila(
        i_docnum        = lv_docnum
        i_operacao      = zcl_im_cl_fluxo_exportacao=>lc_contante-operacao-incluir
        i_formacao_lote = ls_formacao_lote
      ).

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  GET_DADOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_dados INPUT.

  FREE: r_docnum.
  APPEND VALUE #(
           sign    = zcl_les_utils=>if_stab_constants~mc_sign_include
           option  = zcl_les_utils=>if_stab_constants~mc_option_equal
           low     = lv_docnum
         ) TO r_docnum.

  DATA(ls_vinculo_1) = zcl_im_cl_fluxo_exportacao=>check_nfe_fila_vinculo(  i_docnum = r_docnum[] ).
  IF ls_vinculo_1-zsdtvinc_p_flote IS NOT INITIAL.
    CLEAR ls_formacao_lote.
    MESSAGE 'NF-e já Vinculada, operação não permitida' TYPE 'I'.
    EXIT.
  ENDIF.

  zcl_im_cl_fluxo_exportacao=>get_nfe_e_f(
    CHANGING
      c_docnum = r_docnum[]
    RECEIVING
      r_notas  = DATA(lt_notas)
  ).

  READ TABLE lt_notas INTO DATA(ls_notas) WITH KEY docnum = lv_docnum.
  IF sy-subrc IS INITIAL.

    ls_formacao_lote-docnum = ls_notas-docnum.
    ls_formacao_lote-itmnum = ls_notas-itmnum.
    ls_formacao_lote-material = ls_notas-matnr.
    ls_formacao_lote-werks = ls_notas-werks.
    ls_formacao_lote-werks_real = ls_notas-branch.
    ls_formacao_lote-qtd_nf = ls_notas-menge.
    ls_formacao_lote-data_emissao = ls_notas-docdat.

    ls_formacao_lote-compra_fim_es = ls_notas-comprafimespecifico.
    ls_formacao_lote-eudr = ls_notas-eudr.
    ls_formacao_lote-entrada_transf = ls_notas-entradatransferencia.
    lv_formacao_lote = ls_notas-formacaolote.
    ls_formacao_lote-romaneio_completo = ls_notas-romaneiocompleto.
    ls_formacao_lote-cancel = ls_notas-cancel.

  ELSE.
    CLEAR: ls_formacao_lote.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0111 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0111 OUTPUT.
  SET PF-STATUS 'PF0111'.
  SET TITLEBAR 'TL0111'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0111  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0111 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF ls_formacao_lote-saldo_disponivel IS INITIAL.
        MESSAGE 'Informar Valor no Campo SALDO!' TYPE 'I'.
        EXIT.
      ENDIF.

      IF ls_formacao_lote-qtd_nf < ls_formacao_lote-saldo_disponivel.
        MESSAGE 'Valor Superior ao Permitido!' TYPE 'I'.
        EXIT.
      ENDIF.

      zcl_im_cl_fluxo_exportacao=>manut_fila(
        i_docnum        = ls_formacao_lote-docnum
        i_operacao      = zcl_im_cl_fluxo_exportacao=>lc_contante-operacao-alterar_saldo
        i_formacao_lote = ls_formacao_lote
      ).

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .

  CHECK p_ins IS INITIAL.

  zcl_im_cl_fluxo_exportacao=>check_nfe_fila_vinculo(
    EXPORTING
      i_docnum        = sdocnum[]
      i_werks         = scentro[]
      i_werks_real    = scentror[]
      i_data_emisssao = sdt_emi[]
    RECEIVING
      r_formacao_lote = DATA(lt_formacao_lote)
  ).

  IF p_lsn IS NOT INITIAL.
    DELETE lt_formacao_lote-zsdtprod_flote WHERE saldo_disponivel EQ 0.
  ENDIF.

  FREE gt_saida.
  LOOP AT lt_formacao_lote-zsdtprod_flote INTO DATA(ls_flote).

    APPEND
    VALUE #(
                  block                = COND #( WHEN ls_flote-us_cancel EQ 'BLOQUEADO' THEN '@06@' ELSE '@07@' )
                  mark                 = abap_false
                  docnum               = ls_flote-docnum
                  itmnum               = ls_flote-itmnum
                  werks                = ls_flote-werks
                  seq                  = ls_flote-seq
                  matkl                = ls_flote-matkl
                  werks_real           = ls_flote-werks_real
                  data_emissao         = ls_flote-data_emissao
                  material             = ls_flote-material
                  qtd_nf               = ls_flote-qtd_nf
                  saldo_vinc           = ls_flote-saldo_vinc
                  saldo_nao_disponivel = ls_flote-saldo_nao_disponivel
                  saldo_disponivel     = ls_flote-saldo_disponivel"172407 SMC - AJUSTADO 03-04-2025
**<<<------"172407 - NMS - INI------>>>
                  saldo_nao_disp_icon  = COND #( WHEN ls_flote-saldo_nao_disponivel IS NOT INITIAL THEN '@01@' ELSE '' )
                  manual_icon          = COND #( WHEN ls_flote-manual IS NOT INITIAL THEN '@01@' ELSE '' )
**<<<------"172407 - NMS - FIM------>>>
                  compra_fim_es_icon   = COND #( WHEN ls_flote-compra_fim_es     IS NOT INITIAL THEN '@01@' ELSE '' )
                  romaneio_completo_icon = COND #( WHEN ls_flote-romaneio_completo IS NOT INITIAL THEN '@01@' ELSE '' )
                  eudr_icon            = COND #( WHEN ls_flote-eudr              IS NOT INITIAL THEN '@01@' ELSE '' )
                  entrada_transf_icon  = COND #( WHEN ls_flote-entrada_transf    IS NOT INITIAL THEN '@01@' ELSE '' )
                  us_criacao           = ls_flote-us_criacao
                  dt_criacao           = ls_flote-dt_criacao
                  hr_criacao           = ls_flote-hr_criacao
                  cancel               = ls_flote-cancel
**<<<------"167326 - NMS - INI------>>>
*                  cancel_icon          = COND #( WHEN ls_flote-cancel IS NOT INITIAL THEN '@01@' ELSE '' )
                  cancel_icon          = COND #( WHEN ls_flote-cancel IS NOT INITIAL
                                                 THEN
                                                   COND #( WHEN ls_flote-us_cancel EQ 'BLOQUEADO' THEN abap_off ELSE abap_on )
                                                 ELSE
                                                   abap_off
                                                )
                  ln_color             = COND #( WHEN ls_flote-us_cancel EQ 'BLOQUEADO' THEN 'C600' ELSE abap_off )
**<<<------"167326 - NMS - FIM------>>>
                  us_cancel            = ls_flote-us_cancel
                  dt_cancel            = ls_flote-dt_cancel
                  hr_cancel            = ls_flote-hr_cancel
            ) TO gt_saida.

  ENDLOOP.

  DELETE gt_saida WHERE us_cancel NE 'BLOQUEADO' AND cancel IS NOT INITIAL.
**<<<------"167326 - NMS - INI------>>>
  SORT gt_saida BY data_emissao werks_real docnum itmnum seq.
**<<<------"167326 - NMS - FIM------>>>
ENDFORM.
