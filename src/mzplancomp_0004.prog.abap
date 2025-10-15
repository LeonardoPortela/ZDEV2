*----------------------------------------------------------------------*
***INCLUDE MZPLANCOMP_0004 .
*----------------------------------------------------------------------*

DATA: vg_remessa2       TYPE likp,
      vg_total_vincular TYPE j_1bnetqty.

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_PROGRAMA_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_VERIFICA_SELECAO_PR  text
*----------------------------------------------------------------------*
FORM verifica_selecao_programa_plan  USING vg_verifica_selecao TYPE sy-subrc.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row,
        vg_quant_recusa  TYPE ntgew_ap,
        saldo            TYPE ntgew_ap.

  CLEAR: wa_znom_programacao_alv.

  CALL METHOD plan_alv_programa->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_znom_programacao_alv INTO wa_znom_programacao_alv INDEX wa_selected_rows-index.
    READ TABLE it_znom_programacao     INTO wa_znom_programacao
      WITH KEY id_nomeacao_tran = wa_znom_programacao_alv-id_nomeacao_tran
               id_empresa       = wa_znom_programacao_alv-id_empresa
               id_filial        = wa_znom_programacao_alv-id_filial
               id_material      = wa_znom_programacao_alv-id_material.
    IF sy-subrc IS INITIAL.
      SELECT SUM( r~nm_quantidade ) INTO vg_quant_recusa
        FROM zdoc_exp_recusa AS r
       INNER JOIN znom_prog_reme AS e ON e~id_remessa EQ r~vbeln_re_exp
       WHERE e~id_nomeacao_tran EQ wa_znom_programacao-id_nomeacao_tran
         AND e~id_empresa       EQ wa_znom_programacao-id_empresa
         AND e~id_filial        EQ wa_znom_programacao-id_filial
         AND e~id_material      EQ wa_znom_programacao-id_material.
    ENDIF.
  ENDLOOP.

  saldo = wa_znom_programacao_alv-nr_qtde_saldo_pla - vg_quant_recusa.

  IF ( NOT wa_znom_programacao_alv IS INITIAL ) AND ( ( wa_znom_programacao_alv-nr_qtde_saldo_pla EQ 0 ) OR
                                                      ( saldo EQ 0 ) ) .
    vg_verifica_selecao = 0.
*    IF VG_QUANT_RECUSA GT 0.
*      MESSAGE W048 WITH VG_QUANT_RECUSA WA_ZNOM_PROGRAMACAO_ALV-ID_UNIDADE.
*    ENDIF.
  ELSE.
    vg_verifica_selecao = 1.
  ENDIF.

ENDFORM.                    " VERIFICA_SELECAO_PROGRAMA_PLAN

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_REMESSAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM consulta_remessas USING p_todas_remessas_nom.

  DATA: it_zdoc_exp     TYPE TABLE OF zdoc_exp WITH HEADER LINE,
        it_likp         TYPE TABLE OF likp WITH HEADER LINE,
        it_vbfa         TYPE TABLE OF vbfa WITH HEADER LINE,
        it_vbfa_or      TYPE TABLE OF vbfa WITH HEADER LINE,
        it_vbrk         TYPE TABLE OF vbrk WITH HEADER LINE,
        it_zdoc_recusa  TYPE TABLE OF zdoc_exp_recusa WITH HEADER LINE,
        vg_fksto        TYPE fksto,
        vg_sfakn        TYPE sfakn,
        wa_j_1bnflin    TYPE j_1bnflin,
        wa_dde_aplicado TYPE zdde_aplicacao,
        wa_dde          TYPE zdde.

  CLEAR: it_znom_prog_reme_alv[].

  IF p_todas_remessas_nom IS NOT INITIAL.

    SELECT * INTO TABLE it_znom_prog_reme
      FROM znom_prog_reme
     WHERE id_nomeacao_tran EQ wa_znom_programacao-id_nomeacao_tran.

  ELSE.
    SELECT * INTO TABLE it_znom_prog_reme
      FROM znom_prog_reme
     WHERE id_nomeacao_tran EQ wa_znom_programacao-id_nomeacao_tran
       AND id_empresa       EQ wa_znom_programacao-id_empresa
       AND id_filial        EQ wa_znom_programacao-id_filial
       AND id_material      EQ wa_znom_programacao-id_material.
  ENDIF.

  IF sy-subrc IS INITIAL.

    SELECT * INTO TABLE it_zdoc_recusa
      FROM zdoc_exp_recusa
       FOR ALL ENTRIES IN it_znom_prog_reme
     WHERE vbeln_re_exp EQ it_znom_prog_reme-id_remessa.

    SELECT * INTO TABLE it_zdoc_exp
      FROM zdoc_exp
       FOR ALL ENTRIES IN it_znom_prog_reme
     WHERE id_nomeacao_tran EQ it_znom_prog_reme-id_nomeacao_tran
       AND vbeln            EQ it_znom_prog_reme-id_remessa
       AND id_registro_expo EQ it_znom_prog_reme-id_registro_expo.

    SELECT * APPENDING TABLE it_zdoc_exp
      FROM zdoc_exp
       FOR ALL ENTRIES IN it_znom_prog_reme
     WHERE id_nomeacao_tran EQ it_znom_prog_reme-id_nomeacao_tran
       AND vbeln            EQ it_znom_prog_reme-id_remessa
       AND id_due           EQ it_znom_prog_reme-id_due.

    SORT it_zdoc_exp                                   BY id_doc_exp vbeln id_registro_expo id_due.
    DELETE ADJACENT DUPLICATES FROM it_zdoc_exp COMPARING id_doc_exp vbeln id_registro_expo id_due.

    SELECT * INTO TABLE it_likp
      FROM likp
       FOR ALL ENTRIES IN it_znom_prog_reme
     WHERE vbeln EQ it_znom_prog_reme-id_remessa.

    SELECT * INTO TABLE it_vbfa
      FROM vbfa
       FOR ALL ENTRIES IN it_znom_prog_reme
     WHERE vbelv   EQ it_znom_prog_reme-id_remessa
       AND vbtyp_n EQ 'M'
       AND vbtyp_v EQ 'J'.

*-CS2019001113 - correcao performance - 09.12.2020 - inicio
    IF sy-subrc IS INITIAL.
      SELECT * INTO TABLE it_vbrk
        FROM vbrk
         FOR ALL ENTRIES IN it_vbfa
       WHERE vbeln EQ it_vbfa-vbeln.
*        AND FKSTO EQ VG_FKSTO
*        AND SFAKN EQ VG_SFAKN.

      DELETE it_vbrk WHERE fksto <> vg_fksto
                        OR sfakn <> vg_sfakn.
    ENDIF.
*-CS2019001113 - correcao performance - 09.12.2020 - fim

**  Dados do campo ordem
    SELECT vbelv vbeln
      FROM vbfa
      INTO CORRESPONDING FIELDS OF TABLE it_vbfa_or
   FOR ALL ENTRIES IN it_znom_prog_reme
     WHERE vbeln = it_znom_prog_reme-id_remessa
       AND vbtyp_n = 'J'
       AND vbtyp_v = 'C'.

  ENDIF.

  LOOP AT it_znom_prog_reme INTO wa_znom_prog_reme.
    CLEAR: wa_znom_prog_reme_alv.

    MOVE-CORRESPONDING wa_znom_prog_reme TO wa_znom_prog_reme_alv.

    IF wa_znom_prog_reme-id_registro_expo IS NOT INITIAL.
      SELECT SINGLE nr_registro_expo INTO wa_znom_prog_reme_alv-nr_registro_expo
        FROM zreg_exportacao
       WHERE id_registro_expo EQ wa_znom_prog_reme-id_registro_expo.

      SELECT SINGLE * INTO wa_dde_aplicado
        FROM zdde_aplicacao
       WHERE id_registro_expo EQ wa_znom_prog_reme-id_registro_expo.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE * INTO wa_dde
          FROM zdde
         WHERE id_dde EQ wa_dde_aplicado-id_dde.

        IF sy-subrc IS INITIAL.
          wa_znom_prog_reme_alv-id_dde  = wa_dde-id_dde.
          wa_znom_prog_reme_alv-nr_dde  = wa_dde-nr_dde.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_znom_prog_reme-id_due IS NOT INITIAL.
      SELECT SINGLE numero_due INTO wa_znom_prog_reme_alv-numero_due
        FROM zsdt0170
       WHERE id_due EQ wa_znom_prog_reme-id_due.
    ENDIF.

    READ TABLE it_likp WITH KEY vbeln = wa_znom_prog_reme-id_remessa.
    IF sy-subrc IS INITIAL.
      wa_znom_prog_reme_alv-kunnr = it_likp-kunnr.

      IF it_likp-gewei NE 'KG'.

        CALL FUNCTION 'ME_CONVERSION_MEINS'
          EXPORTING
            i_matnr             = wa_znom_prog_reme-id_material
            i_mein1             = it_likp-gewei
            i_meins             = 'KG'
            i_menge             = it_likp-ntgew
          IMPORTING
            menge               = it_likp-ntgew
          EXCEPTIONS
            error_in_conversion = 1
            no_success          = 2
            OTHERS              = 3.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          it_likp-gewei = 'KG'.
        ENDIF.

      ENDIF.

      wa_znom_prog_reme_alv-ntgew = it_likp-ntgew.
      wa_znom_prog_reme_alv-gewei = it_likp-gewei.
      SELECT SINGLE name1 INTO wa_znom_prog_reme_alv-name1
        FROM kna1
       WHERE kunnr = it_likp-kunnr.
    ENDIF.

    wa_znom_prog_reme_alv-ntgew_rec = 0.
    LOOP AT it_zdoc_recusa WHERE vbeln_re_exp EQ wa_znom_prog_reme-id_remessa.
      wa_znom_prog_reme_alv-ntgew_rec = wa_znom_prog_reme_alv-ntgew_rec + it_zdoc_recusa-nm_quantidade.
    ENDLOOP.

    CLEAR: wa_j_1bnflin.

    LOOP AT it_vbfa WHERE vbelv EQ wa_znom_prog_reme-id_remessa.
      READ TABLE it_vbrk WITH KEY vbeln = it_vbfa-vbeln.
      IF sy-subrc IS INITIAL.
        wa_znom_prog_reme_alv-vbeln = it_vbrk-vbeln.
        wa_znom_prog_reme_alv-waerk = it_vbrk-waerk.
        wa_znom_prog_reme_alv-netwr = it_vbrk-netwr.
        wa_znom_prog_reme_alv-kurrf = it_vbrk-kurrf.
        wa_j_1bnflin-refkey         = it_vbrk-vbeln.
      ENDIF.
    ENDLOOP.

    IF wa_j_1bnflin-refkey IS NOT INITIAL.
      SELECT SINGLE * INTO wa_j_1bnflin
        FROM j_1bnflin
       WHERE reftyp EQ 'BI'
         AND refkey EQ wa_j_1bnflin-refkey.
      IF sy-subrc IS INITIAL.
        wa_znom_prog_reme_alv-docnum = wa_j_1bnflin-docnum.
      ENDIF.
    ENDIF.

    READ TABLE it_vbfa_or WITH KEY vbeln = wa_znom_prog_reme-id_remessa.
    IF sy-subrc IS INITIAL.
      wa_znom_prog_reme_alv-vbelv = it_vbfa_or-vbelv. "Campo: Ordem
    ENDIF.

    APPEND wa_znom_prog_reme_alv TO it_znom_prog_reme_alv.

  ENDLOOP.

  CLEAR: wa_znom_prog_reme,
         wa_znom_prog_reme_alv,
         vg_qtd_vinc_conh,
         it_zdoc_nf_produtor_alv[],
         it_zdoc_nf_produtor[],
         it_zdoc_rem_bl[],
         it_zdoc_rem_bl_alv[],
         it_znom_conhec_alv[],
         it_znom_conhec[].

ENDFORM.                    " CONSULTA_REMESSAS

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_PROG_REME  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_alv_prog_reme OUTPUT.

  PERFORM plan_cria_prog_remessas_alv.

ENDMODULE.                 " CRIA_ALV_PROG_REME  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_PROG_REMESSAS_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM plan_cria_prog_remessas_alv .

  CONSTANTS: tabela_prog_reme TYPE string VALUE 'IT_ZNOM_PROG_REME_ALV'.

  DATA: text_n001 TYPE c LENGTH 50 VALUE 'Empresa',
        text_n002 TYPE c LENGTH 50 VALUE 'Filial',
        text_n003 TYPE c LENGTH 50 VALUE 'Produto',
        text_n004 TYPE c LENGTH 50 VALUE 'Remessa',
        text_n005 TYPE c LENGTH 50 VALUE 'Id. RE',
        text_n006 TYPE c LENGTH 50 VALUE 'Nr. RE',
        text_n007 TYPE c LENGTH 50 VALUE 'Id. DDE',
        text_n008 TYPE c LENGTH 50 VALUE 'Nr. DDE',
        text_n009 TYPE c LENGTH 50 VALUE 'Cliente',
        text_n010 TYPE c LENGTH 50 VALUE 'Nome Cliente',
        text_n011 TYPE c LENGTH 50 VALUE 'Fatura',
        text_n012 TYPE c LENGTH 50 VALUE 'Moeda',
        text_n013 TYPE c LENGTH 50 VALUE 'Valor',
        text_n014 TYPE c LENGTH 50 VALUE 'Tx.Câmbio',
        text_n015 TYPE c LENGTH 50 VALUE 'Nota Fiscal',
        text_n016 TYPE c LENGTH 50 VALUE 'Quantidade',
        text_n017 TYPE c LENGTH 50 VALUE 'Und.',
        text_n018 TYPE c LENGTH 50 VALUE 'Qtd. Rec./Dev.',
        text_n019 TYPE c LENGTH 50 VALUE 'N. Ordem',
        text_n020 TYPE c LENGTH 50 VALUE 'Id. DU-e',
        text_n021 TYPE c LENGTH 50 VALUE 'Nr. DU-e'.

  IF plan_prim_prog_reme IS INITIAL.

    CREATE OBJECT plan_container_prog_reme
      EXPORTING
        container_name = 'CTN_PROG_REME'.

    CREATE OBJECT plan_alv_prog_reme
      EXPORTING
        i_parent = plan_container_prog_reme.

    CREATE OBJECT toolbar_prog_reme_alv
      EXPORTING
        io_alv_grid = plan_alv_prog_reme.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_prog_reme USING:
        tabela_prog_reme 'ID_EMPRESA'       text_n001 space 01 06 space space   space 'X'   space space             space,
        tabela_prog_reme 'ID_FILIAL'        text_n002 space 02 05 space space   space space space space             space,
        tabela_prog_reme 'ID_MATERIAL'      text_n003 space 03 18 space 'ALPHA' space space space space             space,
        tabela_prog_reme 'VBELV'            text_n019 space 03 18 space 'ALPHA' space space space space             space,
        tabela_prog_reme 'ID_REMESSA'       text_n004 'X'   04 10 space 'ALPHA' space space space c_grid_color_c200 space,
        tabela_prog_reme 'NTGEW'            text_n016 space 05 15 space space   'X'   space space c_grid_color_c400 space,
        tabela_prog_reme 'NTGEW_REC'        text_n018 space 06 15 space space   'X'   space space c_grid_color_recu space,
        tabela_prog_reme 'GEWEI'            text_n017 space 07 04 space space   space space space c_grid_color_c400 space,
        tabela_prog_reme 'VBELN'            text_n011 'X'   08 10 space 'ALPHA' space space space c_grid_color_c200 space,
        tabela_prog_reme 'DOCNUM'           text_n015 'X'   09 10 space 'ALPHA' space space space c_grid_color_c200 space,
        tabela_prog_reme 'ID_DUE'           text_n020 space 09 10 space space   space space space space             space,
        tabela_prog_reme 'NUMERO_DUE'       text_n021 space 09 15 space space   space space space space             space,
        tabela_prog_reme 'ID_REGISTRO_EXPO' text_n005 space 10 10 space space   space space space space             space,
        tabela_prog_reme 'NR_REGISTRO_EXPO' text_n006 space 11 15 space space   space space space space             space,
        tabela_prog_reme 'ID_DDE'           text_n007 space 12 10 space space   space space space space             space,
        tabela_prog_reme 'NR_DDE'           text_n008 space 13 13 space space   space space space space             space,
        tabela_prog_reme 'KUNNR'            text_n009 space 14 10 space 'ALPHA' space space space space             space,
        tabela_prog_reme 'NAME1'            text_n010 space 15 36 space space   space space space space             space,
        tabela_prog_reme 'WAERK'            text_n012 space 16 05 space space   space space space space             space,
        tabela_prog_reme 'NETWR'            text_n013 space 17 15 space space   space space space space             space,
        tabela_prog_reme 'KURRF'            text_n014 space 18 09 space space   space space space space             space.

    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra    = c_x.
    plan_gs_layout-sel_mode = space.

    CREATE OBJECT plan_event_handler_prog_reme.
    SET HANDLER plan_event_handler_prog_reme->handle_hotspot_click_prog_reme FOR plan_alv_prog_reme.
    SET HANDLER plan_event_handler_prog_reme->on_double_prog_reme FOR plan_alv_prog_reme.

    SET HANDLER toolbar_prog_reme_alv->on_toolbar          FOR plan_alv_prog_reme.
    SET HANDLER toolbar_prog_reme_alv->handle_user_command FOR plan_alv_prog_reme.

    CALL METHOD plan_alv_prog_reme->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = plan_gs_layout
      CHANGING
        it_fieldcatalog = plan_catalogo_prog_reme
        it_outtab       = it_znom_prog_reme_alv[].

    plan_prim_prog_reme = c_x.

  ENDIF.

  CALL METHOD plan_alv_prog_reme->refresh_table_display.

  CALL METHOD plan_alv_prog_reme->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_PROG_REMESSAS_ALV

*&---------------------------------------------------------------------*
*&      Form  INCLUIR_REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM incluir_remessa .

  DATA: vg_remessa_add TYPE sy-subrc.

  PERFORM verifica_execucao_filial USING wa_znom_programacao-id_filial vg_remessa_add.
  IF vg_remessa_add IS INITIAL.
    CLEAR: wa_vinc_remessa, it_prod_remessa[], vg_total_vincular.
    CALL SCREEN 0042 STARTING AT 07 05 ENDING AT 130 25.
    PERFORM consulta_remessas USING abap_false.
  ELSE.
    MESSAGE s022 WITH wa_znom_programacao-id_filial.
  ENDIF.

ENDFORM.                    " INCLUIR_REMESSA

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excluir_remessa .

  DATA: vg_verifica_selecao_reme TYPE sy-subrc,
        wa_j_1bnfdoc             TYPE j_1bnfdoc,
        vg_qtd_produtor          TYPE ntgew_15,
        vg_valida_nfe            TYPE sy-subrc.

  PERFORM verifica_selecao_prog_reme USING vg_verifica_selecao_reme.

  IF vg_verifica_selecao_reme IS INITIAL.

    PERFORM verifica_execucao_filial USING wa_znom_programacao-id_filial vg_verifica_selecao_reme.
    IF vg_verifica_selecao_reme IS INITIAL.

      PERFORM verifica_alterar USING wa_znom_prog_reme_alv vg_valida_nfe.

      IF NOT vg_valida_nfe IS INITIAL.
        EXIT.
      ENDIF.

      IF NOT wa_znom_prog_reme_alv-docnum IS INITIAL.
        SELECT SINGLE * INTO wa_j_1bnfdoc
          FROM j_1bnfdoc
         WHERE docnum EQ wa_znom_prog_reme_alv-docnum.

        IF ( sy-subrc IS INITIAL ) AND ( wa_j_1bnfdoc-cancel IS INITIAL ).
          MESSAGE s026 WITH wa_znom_prog_reme_alv-docnum.
          EXIT.
        ENDIF.
      ENDIF.

      SELECT SUM( menge ) INTO vg_qtd_produtor
        FROM zdoc_nf_produtor
       WHERE vbeln EQ wa_znom_prog_reme_alv-id_remessa.

      IF vg_qtd_produtor LT wa_znom_prog_reme_alv-ntgew.

        vg_qtd_produtor = wa_znom_prog_reme_alv-ntgew - vg_qtd_produtor.

        UPDATE znom_remetente
           SET nr_parte_empresa = nr_parte_empresa - vg_qtd_produtor
         WHERE id_nomeacao_tran EQ wa_znom_prog_reme_alv-id_nomeacao_tran
           AND id_empresa       EQ wa_znom_prog_reme_alv-id_empresa
           AND id_filial        EQ wa_znom_prog_reme_alv-id_filial
           AND id_material      EQ wa_znom_prog_reme_alv-id_material
           AND id_remetente     EQ space.

      ENDIF.

      DELETE FROM znom_prog_reme WHERE id_remessa EQ wa_znom_prog_reme_alv-id_remessa.
      DELETE FROM zdoc_rem_bl WHERE id_doc_exp EQ ( SELECT id_doc_exp FROM zdoc_exp WHERE vbeln EQ wa_znom_prog_reme_alv-id_remessa ).
      DELETE FROM zdoc_exp WHERE vbeln EQ wa_znom_prog_reme_alv-id_remessa.
      DELETE FROM zdoc_nf_produtor WHERE vbeln EQ wa_znom_prog_reme_alv-id_remessa.
**    Remove vinculação da remessa com as notas de retorno.
      UPDATE zsdt_retlote SET vbeln = '' WHERE vbeln = wa_znom_prog_reme_alv-id_remessa.
      PERFORM consulta_remessas USING abap_false.
    ELSE.
      MESSAGE s022 WITH wa_znom_programacao-id_filial.
    ENDIF.
  ELSE.
    MESSAGE s010.
  ENDIF.

ENDFORM.                    " EXCLUIR_REMESSA

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0041  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0041 INPUT.
  DATA: t_fields TYPE TABLE OF sval WITH HEADER LINE,
        v_return TYPE znom_remetente-nr_ordem.

  CASE ok_code_0001.
    WHEN ok_bt_reme.
      CLEAR: ok_code_0001.
      PERFORM incluir_remessa.
    WHEN ok_ft_fatura.
      CLEAR: ok_code_0001.
      PERFORM gerar_faturamento.
    WHEN ok_ft_nota.
      CLEAR: ok_code_0001.
      PERFORM validar_nota_fiscal.
    WHEN ok_bt_refr.
      CLEAR: ok_code_0001.
      PERFORM atualizar_remesas.
    WHEN ok_ft_recusa.
      CLEAR: ok_code_0001.
      PERFORM recusar_remessa.
    WHEN ok_ft_memora.
      PERFORM memorando_nota_fiscal.
      CLEAR: ok_code_0001.
    WHEN ok_bt_rem2.
      CLEAR: t_fields, t_fields[].
      t_fields-tabname    = 'ZNOM_REMETENTE'.
*      t_fields-comp_tab   = 'ZNOM_REMETENTE'.
      t_fields-fieldname  = 'NR_ORDEM'.
*      t_fields-comp_field = 'NR_ORDEM'.
      t_fields-field_obl  = 'X'.
      APPEND t_fields.

*      CALL FUNCTION 'POPUP_GET_VALUES'
*        EXPORTING
*          popup_title     = 'Selecionar Ordem'
*        IMPORTING
*          returncode      = v_return
*        TABLES
*          fields          = t_fields
*        EXCEPTIONS
*          error_in_fields = 1
*          OTHERS          = 2.

      CALL FUNCTION 'POPUP_GET_VALUES_USER_HELP'
        EXPORTING
*         F1_FORMNAME     = ' '
*         F1_PROGRAMNAME  = ' '
          f4_formname     = 'CALL_F4_HELP'
          f4_programname  = sy-cprog
*         formname        = 'POPUP_EXIT'
          popup_title     = 'Selecionar Ordem'
          programname     = sy-cprog
*         START_COLUMN    = '5'
*         START_ROW       = '5'
*         NO_CHECK_FOR_FIXED_VALUES = ' '
        IMPORTING
          returncode      = v_return
        TABLES
          fields          = t_fields
        EXCEPTIONS
          error_in_fields = 1
          OTHERS          = 2.

      IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      READ TABLE tl_remet WITH KEY nr_ordem = t_fields-value.
      IF sy-subrc IS INITIAL.
        CLEAR: it_dta[].
        PERFORM f_bdc_data USING:
              ''          ''      'T' 'VL01N'         '',
              'SAPMV50A'  '4001'  'X' ''              '',
              ''          ''      ''  'BDC_CURSOR'    'LV50C-VBELN',
              ''          ''      ''  'LV50C-VBELN'   t_fields-value.

        CALL TRANSACTION 'VL01N' USING it_dta
          MODE 'E'
          UPDATE 'S'.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0041  INPUT

*&---------------------------------------------------------------------*
*&      Form  CALL_F4_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TABNAME    text
*      -->FIELDNAME  text
*      -->DISPLAY    text
*      -->RETURNCODE text
*      -->VALUE      text
*----------------------------------------------------------------------*
FORM call_f4_help USING tabname fieldname display           "#EC CALLED
                  CHANGING returncode value.

  CLEAR tl_remet.

  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  SELECT DISTINCT nr_ordem
    FROM znom_remetente
    INTO CORRESPONDING FIELDS OF TABLE tl_remet
   WHERE nr_ordem         <> ''
     AND id_nomeacao_tran =  wa_znom_transporte_alv-id_nomeacao_tran
     AND id_filial        =  wa_znom_programacao_alv-id_filial.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NR_ORDEM'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'NR_ORDEM'
      value_org       = 'S'
    TABLES
      value_tab       = tl_remet
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDFORM.                    "CALL_F4_HELP

*&---------------------------------------------------------------------*
*&      Module  STATUS_0042  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0042 OUTPUT.

  SET PF-STATUS 'PF0021'.
  SET TITLEBAR 'TL0042'.

  CLEAR: tx_ds_estado.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'RG1'.

        DATA(_input) = '1'.

        IF wa_vinc_remessa-numero_due IS NOT INITIAL.
          SELECT SINGLE *
            FROM zsdt0170 INTO @DATA(_wl_0170_ant)
           WHERE numero_due EQ @wa_vinc_remessa-numero_due
             AND tp_due     EQ '1'. "Sem NF-e

          IF ( sy-subrc EQ 0 ) AND ( _wl_0170_ant-regio IS NOT INITIAL ).
            _input = '0'.
          ENDIF.
        ENDIF.

        screen-input = _input.

    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

  IF wa_vinc_remessa-id_pais IS INITIAL.
    wa_vinc_remessa-id_pais = 'BR '.
  ENDIF.

  IF wa_vinc_remessa-numero_due IS NOT INITIAL.
    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170_aux)
     WHERE numero_due EQ @wa_vinc_remessa-numero_due.

    IF sy-subrc EQ 0.
      wa_vinc_remessa-id_pais   = _wl_0170_aux-land1.
      wa_vinc_remessa-id_estado = _wl_0170_aux-regio.
    ENDIF.
  ENDIF.

  IF NOT wa_vinc_remessa-id_estado IS INITIAL.
    SELECT SINGLE bezei INTO tx_ds_estado
      FROM t005u
     WHERE spras EQ sy-langu
       AND land1 EQ wa_vinc_remessa-id_pais
       AND bland EQ wa_vinc_remessa-id_estado.
  ENDIF.

ENDMODULE.                 " STATUS_0042  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0042  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0042 INPUT.
  DATA: vg_valida       TYPE sy-subrc,
        vg_remessa      TYPE likp,
        vg_zsdt0170     TYPE zsdt0170,
        vg_regis        TYPE zreg_exportacao,
        wa_dde_aplicado TYPE zdde_aplicacao,
        wa_dde          TYPE zdde,
        vl_id_doc       TYPE zid_doc,
        fg_erro(1),
        "IR218333 - 21.01.25 - PQ
*        lv_grp_ret      TYPE c LENGTH 1.
        lv_grp_ret      TYPE znom_reme_notas-grp_retorno.
  "IR218333 - 21.01.25 - PQ

  DATA: wa_lips                 TYPE lips,
        gs_cfop                 TYPE zmemo_cfop,
        gt_cfop                 LIKE TABLE OF gs_cfop,
        tl_retlote              TYPE TABLE OF zsdt_retlote WITH HEADER LINE,
        wl_retlote              TYPE zsdt_retlote,
        vg_item_ov              TYPE vbap,
        vg_menge_nf             TYPE zdoc_nf_produtor-menge,
        vg_menge_efetiva_tot    TYPE zdoc_nf_produtor-menge,
        it_zdoc_nf_produtor_tmp TYPE TABLE OF zdoc_nf_produtor WITH HEADER LINE,
        lv_tlvinc_ret           TYPE zsdt_retlote-quant_vinc,
        lv_docnum_ret           TYPE zsdt_retlote-docnum_ret,
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
        lt_doc_nf_produtor_mod  TYPE TABLE OF zdoc_nf_produtor,
        lv_inserir_nf_produtor  TYPE boolean.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

  DATA: BEGIN OF tl_vbfa1 OCCURS 0 ,
          vbelv TYPE vbfa-vbelv,
        END OF tl_vbfa1,

        BEGIN OF tl_remete OCCURS 0 ,
          docnum_rt TYPE znom_remetente-docnum_rt,
        END OF tl_remete.

  IF ok_code_0042 EQ ok_enter.
    PERFORM busca_produtores_remetentes.
    CALL METHOD plan_alv_remetente_re->refresh_table_display.
    CLEAR: ok_code_0042.
    EXIT.
  ENDIF.

  CLEAR: fg_erro, fg_propria.
  CALL FUNCTION 'Z_MEMO_CFOP_SAIDA'
    EXPORTING
      exp_propria = 'X'
    TABLES
      cfops       = gt_cfop.

  SELECT SINGLE * INTO wa_lips
    FROM lips
   WHERE vbeln EQ vg_remessa2-vbeln.

  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE * INTO vg_item_ov
    FROM vbap
   WHERE vbeln EQ wa_lips-vgbel
     AND posnr EQ wa_lips-vgpos.

  READ TABLE gt_cfop INTO gs_cfop WITH KEY low = vg_item_ov-j_1bcfop.

  IF sy-subrc NE 0.
    fg_propria = 'X'.
  ENDIF.
  IF it_prod_remessa[] IS INITIAL AND fg_propria IS  INITIAL.
    fg_erro = 'X'.
  ENDIF.

  IF fg_propria = 'X'.
    LOOP AT it_znom_remetente_alv.
      IF it_znom_remetente_alv-icone = icon_customer_warehouse.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF it_znom_remetente_alv-uf NE wa_vinc_remessa-id_estado.
      fg_erro = 'X'.
    ENDIF.
  ENDIF.

  IF ok_code_0042 EQ ok_salvar AND fg_erro IS INITIAL.
    CALL METHOD plan_alv_remetente_re->check_changed_data.

    IF wa_vinc_remessa-id_pais IS INITIAL.
      CLEAR: ok_code_0042.
      MESSAGE s057.
      EXIT.
    ENDIF.

    IF wa_vinc_remessa-id_estado IS INITIAL.
      CLEAR: ok_code_0042.
      MESSAGE s029.
      EXIT.
    ENDIF.

    PERFORM validar_vbeln USING vg_valida vg_remessa.
    IF NOT vg_valida IS INITIAL.
      PERFORM message_vbeln USING vg_valida.
      CLEAR: ok_code_0042.
      EXIT.
    ENDIF.

    CLEAR: wa_vinc_remessa-id_registro_expo, wa_vinc_remessa-id_due.

    PERFORM validar_inf_due_re USING vg_valida.
    IF vg_valida IS NOT INITIAL.
      CLEAR: ok_code_0042.
      EXIT.
    ENDIF.

    IF wa_vinc_remessa-nr_registro_expo IS NOT INITIAL.
      PERFORM validar_regis USING vg_valida vg_regis.
      IF NOT vg_valida IS INITIAL.
        PERFORM message_regis USING vg_valida.
        CLEAR: ok_code_0042.
        EXIT.
      ENDIF.
    ELSEIF wa_vinc_remessa-numero_due IS NOT INITIAL .
      PERFORM validar_due USING vg_valida vg_zsdt0170.
      IF vg_valida IS NOT INITIAL.
        CLEAR: ok_code_0042.
        EXIT.
      ENDIF.
    ENDIF.

    PERFORM validar_inf_due_re USING vg_valida.
    IF vg_valida IS NOT INITIAL.
      CLEAR: ok_code_0042.
      EXIT.
    ENDIF.

    PERFORM valida_produtores_remetents USING vg_valida.
    IF NOT vg_valida IS INITIAL.
      CASE vg_valida.
        WHEN 1.
          MESSAGE s030.
        WHEN 2.
          MESSAGE s031 WITH it_prod_remessa-id_remetente.
      ENDCASE.
      CLEAR: ok_code_0042.
      EXIT.
    ENDIF.

    CLEAR: wa_znom_prog_reme.
    wa_znom_prog_reme-id_nomeacao_tran = wa_znom_programacao-id_nomeacao_tran.
    wa_znom_prog_reme-id_empresa       = wa_znom_programacao-id_empresa.
    wa_znom_prog_reme-id_filial        = wa_znom_programacao-id_filial.
    wa_znom_prog_reme-id_material      = wa_znom_programacao-id_material.
    wa_znom_prog_reme-id_remessa       = wa_vinc_remessa-vbeln.

    IF wa_vinc_remessa-nr_registro_expo IS NOT INITIAL.
      wa_znom_prog_reme-id_registro_expo = wa_vinc_remessa-id_registro_expo.
    ELSEIF wa_vinc_remessa-numero_due IS NOT INITIAL.
      wa_znom_prog_reme-id_due           = wa_vinc_remessa-id_due.
    ENDIF.

    MODIFY znom_prog_reme FROM wa_znom_prog_reme.

    SELECT MAX( id_doc_exp ) INTO vl_id_doc
      FROM zdoc_exp.
    IF vl_id_doc IS INITIAL.
      vl_id_doc = 1.
    ELSE.
      ADD 1 TO vl_id_doc.
    ENDIF.

    wa_zdoc_exp-id_doc_exp       = vl_id_doc.
    wa_zdoc_exp-vbeln            = wa_vinc_remessa-vbeln.

    IF wa_vinc_remessa-id_registro_expo IS NOT INITIAL.
      wa_zdoc_exp-id_registro_expo = wa_vinc_remessa-id_registro_expo.
      wa_zdoc_exp-nr_registro_expo = vg_regis-nr_registro_expo.
    ELSEIF wa_vinc_remessa-numero_due IS NOT INITIAL.
      wa_zdoc_exp-id_due           = vg_zsdt0170-id_due.
      wa_zdoc_exp-numero_due       = vg_zsdt0170-numero_due.
    ENDIF.

    wa_zdoc_exp-id_nomeacao_tran = wa_vinc_remessa-id_nomeacao_tran.

    IF wa_vinc_remessa-id_registro_expo IS NOT INITIAL.
      SELECT SINGLE * INTO wa_dde_aplicado
        FROM zdde_aplicacao
       WHERE id_registro_expo EQ wa_vinc_remessa-id_registro_expo.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE * INTO wa_dde
          FROM zdde
         WHERE id_dde EQ wa_dde_aplicado-id_dde.

        IF sy-subrc IS INITIAL.
          wa_zdoc_exp-id_dde = wa_dde-id_dde.
          wa_zdoc_exp-nr_dde = wa_dde-nr_dde.
        ENDIF.
      ENDIF.
    ENDIF.

    MODIFY zdoc_exp FROM wa_zdoc_exp.

    "Vincula notas
    PERFORM consulta_remetentes.

    IF vg_remessa-gewei NE 'KG'.
      CALL FUNCTION 'ME_CONVERSION_MEINS'
        EXPORTING
          i_matnr             = wa_znom_programacao-id_material
          i_mein1             = vg_remessa-gewei
          i_meins             = 'KG'
          i_menge             = vg_remessa-ntgew
        IMPORTING
          menge               = vg_remessa-ntgew
        EXCEPTIONS
          error_in_conversion = 1
          no_success          = 2
          OTHERS              = 3.
      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

**  Encontrar grupo de retorno da remessa informada.
    SELECT SINGLE grp_retorno
      INTO lv_grp_ret
      FROM vbfa AS vb
     INNER JOIN znom_remetente AS re ON re~nr_ordem = vb~vbelv
     WHERE vbeln = wa_vinc_remessa-vbeln
       AND vbtyp_n = 'J'
       AND vbtyp_v = 'C'.

    LOOP AT it_prod_remessa.
      LOOP AT it_znom_reme_notas_alv INTO wa_znom_reme_notas_alv WHERE parid EQ it_prod_remessa-id_remetente
                                                                   AND nr_saldo_efetivar GT 0
                                                                   AND grp_retorno EQ lv_grp_ret.

        IF wa_vinc_remessa-numero_due IS NOT INITIAL.
          CHECK wa_vinc_remessa-numero_due EQ wa_znom_reme_notas_alv-numero_due.
        ENDIF.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
        CLEAR lv_inserir_nf_produtor.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

        IF vg_remessa-ntgew GT 0.
          CLEAR: wa_zdoc_nf_produtor.

          wa_zdoc_nf_produtor-vbeln       = wa_vinc_remessa-vbeln.
          wa_zdoc_nf_produtor-docnum_prod = wa_znom_reme_notas_alv-docnum.
          wa_zdoc_nf_produtor-itmnum_prod = wa_znom_reme_notas_alv-itmnum.

          IF vg_remessa-ntgew GE wa_znom_reme_notas_alv-nr_saldo_efetivar.
            IF it_prod_remessa-nr_qtd_vincular GT wa_znom_reme_notas_alv-nr_saldo_efetivar.
              wa_zdoc_nf_produtor-menge = wa_znom_reme_notas_alv-nr_saldo_efetivar.
            ELSE.
              wa_zdoc_nf_produtor-menge = it_prod_remessa-nr_qtd_vincular.
            ENDIF.
            wa_znom_reme_notas_alv-nr_efetivada      = wa_znom_reme_notas_alv-nr_efetivada + wa_zdoc_nf_produtor-menge.
            wa_znom_reme_notas_alv-nr_saldo_efetivar = 0.
            wa_znom_reme_notas_alv-nr_saldo_efetivar = 0.
          ELSE.
            IF it_prod_remessa-nr_qtd_vincular GT vg_remessa-ntgew.
              wa_zdoc_nf_produtor-menge = vg_remessa-ntgew.
            ELSE.
              wa_zdoc_nf_produtor-menge = it_prod_remessa-nr_qtd_vincular.
            ENDIF.
            wa_znom_reme_notas_alv-nr_efetivada      = wa_znom_reme_notas_alv-nr_efetivada + wa_zdoc_nf_produtor-menge.
            wa_znom_reme_notas_alv-nr_saldo_efetivar = wa_znom_reme_notas_alv-nr_saldo_efetivar - vg_remessa-ntgew.
          ENDIF.

          vg_remessa-ntgew =  vg_remessa-ntgew - wa_zdoc_nf_produtor-menge.
          wa_zdoc_nf_produtor-grp_retorno = lv_grp_ret.
          wa_zdoc_nf_produtor-id_nomeacao_tran = wa_vinc_remessa-id_nomeacao_tran.

          "Se tem volume desta nota fiscal.
          IF wa_zdoc_nf_produtor-menge GT 0.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*            MODIFY zdoc_nf_produtor FROM wa_zdoc_nf_produtor.
            lv_inserir_nf_produtor = abap_true.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração
          ENDIF.

          "Valida se quantidade efetiva foi excedida
          IF ( wa_zdoc_nf_produtor-docnum_prod IS NOT INITIAL ) AND
             ( wa_zdoc_nf_produtor-itmnum_prod IS NOT INITIAL ).

            CLEAR: it_zdoc_nf_produtor_tmp[], vg_menge_efetiva_tot, vg_menge_nf.

            SELECT SINGLE *
              FROM j_1bnflin INTO @DATA(_wl_lin_prod)
             WHERE docnum EQ @wa_zdoc_nf_produtor-docnum_prod
               AND itmnum EQ @wa_zdoc_nf_produtor-itmnum_prod.

            IF sy-subrc EQ 0.

              "Get Quantidade NF
              IF _wl_lin_prod-meins NE 'KG'.
                CALL FUNCTION 'ME_CONVERSION_MEINS'
                  EXPORTING
                    i_matnr             = _wl_lin_prod-matnr
                    i_mein1             = _wl_lin_prod-meins
                    i_meins             = 'KG'
                    i_menge             = _wl_lin_prod-menge
                  IMPORTING
                    menge               = vg_menge_nf
                  EXCEPTIONS
                    error_in_conversion = 1
                    no_success          = 2
                    OTHERS              = 3.
                IF NOT sy-subrc IS INITIAL.
                  ROLLBACK WORK.
                  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
                  RETURN.
                ENDIF.
              ELSE.
                vg_menge_nf = _wl_lin_prod-menge.
              ENDIF.

              "Get Quantidade Total Efetiva
              SELECT *
                FROM zdoc_nf_produtor INTO TABLE it_zdoc_nf_produtor_tmp
               WHERE docnum_prod = wa_zdoc_nf_produtor-docnum_prod
                 AND itmnum_prod = wa_zdoc_nf_produtor-itmnum_prod.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
              IF lv_inserir_nf_produtor IS NOT INITIAL.

                READ TABLE it_zdoc_nf_produtor_tmp[] ASSIGNING FIELD-SYMBOL(<fs_nf_prod>)
                                                     WITH KEY vbeln       = wa_zdoc_nf_produtor-vbeln
                                                              docnum_prod = wa_zdoc_nf_produtor-docnum_prod
                                                              itmnum_prod = wa_zdoc_nf_produtor-itmnum_prod
                                                              grp_retorno = wa_zdoc_nf_produtor-grp_retorno.
                IF sy-subrc IS INITIAL
                AND <fs_nf_prod> IS ASSIGNED.

                  <fs_nf_prod> = wa_zdoc_nf_produtor.

                ELSE.

                  APPEND wa_zdoc_nf_produtor TO it_zdoc_nf_produtor_tmp[].

                ENDIF.

              ENDIF.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

              LOOP AT it_zdoc_nf_produtor_tmp.
                SELECT SINGLE *
                  FROM zdoc_exp_rec_nf INTO @DATA(_wl_doc_exp_rec_nf)
                 WHERE vbeln_re_exp = @it_zdoc_nf_produtor_tmp-vbeln.

                CHECK sy-subrc NE 0.

                ADD it_zdoc_nf_produtor_tmp-menge TO vg_menge_efetiva_tot.
              ENDLOOP.

              IF vg_menge_efetiva_tot > vg_menge_nf.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*                ROLLBACK WORK.
                CLEAR lv_inserir_nf_produtor.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

                MESSAGE s058 WITH vg_menge_efetiva_tot vg_menge_nf wa_zdoc_nf_produtor-docnum_prod.
                RETURN.
              ENDIF.
            ENDIF.
          ENDIF. "Valida se quantidade efetiva foi excedida

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
          IF lv_inserir_nf_produtor IS NOT INITIAL.
            APPEND wa_zdoc_nf_produtor TO lt_doc_nf_produtor_mod.
          ENDIF.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

        ENDIF.
        it_prod_remessa-nr_qtd_vincular = it_prod_remessa-nr_qtd_vincular - wa_zdoc_nf_produtor-menge.
      ENDLOOP.
    ENDLOOP.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
    IF lt_doc_nf_produtor_mod IS NOT INITIAL.

      PERFORM modify_doc_nf_produtor TABLES lt_doc_nf_produtor_mod.
      CLEAR lt_doc_nf_produtor_mod.

    ELSE.

      MESSAGE s000(su) WITH 'Não foi gravado dados na ZDOC_NF_PRODUTOR'.

    ENDIF.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

    "Vincular Saldo Filial (Parte Empresa)
    IF ( vg_remessa-ntgew GT 0 ) AND ( it_prod_remessa[] IS INITIAL ).

      READ TABLE it_znom_remetente INTO wa_znom_remetente WITH KEY id_remetente = space
                                                                   grp_retorno  = lv_grp_ret.
      IF sy-subrc IS INITIAL.
        wa_znom_remetente-nr_parte_empresa = wa_znom_remetente-nr_parte_empresa + vg_remessa-ntgew.
        UPDATE znom_remetente
           SET nr_parte_empresa = wa_znom_remetente-nr_parte_empresa
         WHERE id_nomeacao_tran = wa_znom_remetente-id_nomeacao_tran
           AND id_empresa       = wa_znom_remetente-id_empresa
           AND id_filial        = wa_znom_remetente-id_filial
           AND id_material      = wa_znom_remetente-id_material
           AND grp_retorno      = lv_grp_ret
           AND id_remetente     = space.
      ENDIF.
    ENDIF.

**  Vinculação das notas de retorno
**  Procurando notas de retorno ainda não vinculadas
    REFRESH: tl_retlote, tl_vbfa1, tl_remete.

    CLEAR: lv_tlvinc_ret.

    SELECT DISTINCT vbelv
      FROM vbfa AS vb
      INTO CORRESPONDING FIELDS OF TABLE tl_vbfa1
      WHERE vb~vbeln   = wa_vinc_remessa-vbeln
       AND  vb~vbtyp_n = 'J'
       AND  vb~vbtyp_v = 'C'.

    IF sy-subrc IS INITIAL.
      SELECT DISTINCT docnum_rt
        FROM znom_remetente AS re
        INTO CORRESPONDING FIELDS OF TABLE tl_remete
        FOR ALL ENTRIES IN tl_vbfa1
        WHERE re~nr_ordem    = tl_vbfa1-vbelv
         AND  re~grp_retorno = lv_grp_ret.

      IF sy-subrc IS INITIAL.
        LOOP AT tl_remete.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = tl_remete-docnum_rt
            IMPORTING
              output = lv_docnum_ret.

          SELECT DISTINCT *
           FROM zsdt_retlote AS rt
           APPENDING CORRESPONDING FIELDS OF TABLE tl_retlote
           WHERE rt~docnum_ret = lv_docnum_ret
            AND  rt~vbeln = ''.
        ENDLOOP.
      ENDIF.
    ENDIF.

    SORT: tl_retlote BY docnum.

    DELETE ADJACENT DUPLICATES FROM tl_retlote COMPARING docnum.

    IF tl_retlote[] IS NOT INITIAL.

      LOOP AT tl_retlote INTO wl_retlote.
        wl_retlote-vbeln = wa_vinc_remessa-vbeln.
        MODIFY zsdt_retlote FROM wl_retlote.
        ADD wl_retlote-quant_vinc TO lv_tlvinc_ret.
**      Checa se a quantidade excedeu, caso tenha excedido encerra a processo de vinculação.
        IF lv_tlvinc_ret >= vg_total_vincular.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LEAVE TO SCREEN 0.
    CLEAR: ok_code_0042.
  ELSE.
    CLEAR: ok_code_0042.
    MESSAGE s052.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0042  INPUT


*&---------------------------------------------------------------------*
*&      Module  ZM_VBELN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_vbeln INPUT.
  DATA: vg_valida_beln TYPE sy-subrc.

  CLEAR: vg_remessa2.

  PERFORM validar_vbeln USING vg_valida_beln vg_remessa2.

  IF NOT vg_valida_beln IS INITIAL.
    PERFORM message_vbeln USING vg_valida_beln.
  ENDIF.

ENDMODULE.                 " ZM_VBELN  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_REGIS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_regis INPUT.

  DATA: vg_valida_regis TYPE sy-subrc.
  DATA: vg_registro TYPE zreg_exportacao.

  PERFORM validar_regis USING vg_valida_regis vg_registro.
  IF NOT vg_valida_regis IS INITIAL.
    PERFORM message_regis USING vg_valida_regis.
  ENDIF.

ENDMODULE.                 " ZM_REGIS  INPUT

MODULE zm_due INPUT.

  DATA: v_valida   TYPE sy-subrc,
        v_zsdt0170 TYPE zsdt0170.

  PERFORM validar_due USING v_valida
                            v_zsdt0170.

ENDMODULE.                 " ZM_REGIS  INPUT



*&---------------------------------------------------------------------*
*&      Form  VALIDAR_VBELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM validar_vbeln  USING  p_valida_beln TYPE sy-subrc vg_likp TYPE likp.

  DATA: wa_virtual         TYPE zsdt_depara_cen,
        it_lips            TYPE TABLE OF lips WITH HEADER LINE,
        it_zsdt0024        TYPE TABLE OF zsdt0024 WITH HEADER LINE,
        vg_total_possivel  TYPE j_1bnetqty,
        vg_total_vinculado TYPE j_1bnetqty,
        it_zdoc_exp_recusa TYPE TABLE OF zdoc_exp_recusa WITH HEADER LINE.

  p_valida_beln = 4.

  CHECK NOT wa_vinc_remessa-vbeln IS INITIAL.
  SELECT SINGLE * FROM likp INTO vg_likp WHERE vbeln EQ wa_vinc_remessa-vbeln.
  IF NOT sy-subrc IS INITIAL.
    p_valida_beln = 1.
  ELSE.

    p_valida_beln = 0.

    IF NOT ( vg_likp-vstel EQ wa_znom_programacao-id_filial ) AND ( vg_likp-vkorg EQ wa_znom_programacao-id_empresa ).

      SELECT SINGLE * INTO wa_j_1bbranch
        FROM j_1bbranch
       WHERE bukrs  EQ vg_likp-vkorg
         AND branch EQ vg_likp-vstel.

      IF sy-subrc IS INITIAL.
        p_valida_beln = 2.
        EXIT.
      ELSE.
        SELECT SINGLE * INTO wa_virtual
          FROM zsdt_depara_cen
         WHERE vkorg       EQ wa_znom_programacao-id_empresa
           AND centro_real EQ wa_znom_programacao-id_filial
           AND centrov_1   EQ vg_likp-vstel.

        IF NOT sy-subrc IS INITIAL.
          p_valida_beln = 3.
          EXIT.
        ENDIF.
      ENDIF.

    ENDIF.

    SELECT * INTO TABLE it_lips
      FROM lips
     WHERE vbeln EQ wa_vinc_remessa-vbeln.

    IF NOT it_lips[] IS INITIAL.

      READ TABLE it_lips INDEX 1.

      "Material diferente e Não Similar
      IF wa_znom_programacao-id_material NE it_lips-matnr.

        SELECT * INTO TABLE it_zsdt0024
          FROM zsdt0024
         WHERE matnr1 EQ wa_znom_programacao-id_material.

        READ TABLE it_zsdt0024 WITH KEY matnr2 = it_lips-matnr.
        IF NOT sy-subrc IS INITIAL.
          p_valida_beln = 5.
          EXIT.
        ENDIF.

      ENDIF.

      "Vincula notas
      PERFORM consulta_remetentes.

      vg_total_possivel = 0.
      LOOP AT it_znom_remetente INTO wa_znom_remetente.
        vg_total_possivel = vg_total_possivel + wa_znom_remetente-nr_programada.
      ENDLOOP.

      vg_total_vinculado = 0.
      LOOP AT it_znom_prog_reme_alv INTO wa_znom_prog_reme_alv.
        IF wa_znom_prog_reme_alv-gewei NE 'KG'.
          CALL FUNCTION 'ME_CONVERSION_MEINS'
            EXPORTING
              i_matnr             = wa_znom_prog_reme_alv-id_material
              i_mein1             = wa_znom_prog_reme_alv-gewei
              i_meins             = 'KG'
              i_menge             = wa_znom_prog_reme_alv-ntgew
            IMPORTING
              menge               = wa_znom_prog_reme_alv-ntgew
            EXCEPTIONS
              error_in_conversion = 1
              no_success          = 2
              OTHERS              = 3.
          IF NOT sy-subrc IS INITIAL.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
        vg_total_vinculado = vg_total_vinculado + wa_znom_prog_reme_alv-ntgew.

        SELECT * INTO TABLE it_zdoc_exp_recusa
          FROM zdoc_exp_recusa
         WHERE vbeln_re_exp EQ wa_znom_prog_reme_alv-id_remessa.

        LOOP AT it_zdoc_exp_recusa.
          vg_total_vinculado = vg_total_vinculado - it_zdoc_exp_recusa-nm_quantidade.
        ENDLOOP.

      ENDLOOP.

      IF it_lips-gewei NE 'KG'.
        CALL FUNCTION 'ME_CONVERSION_MEINS'
          EXPORTING
            i_matnr             = it_lips-matnr
            i_mein1             = it_lips-gewei
            i_meins             = 'KG'
            i_menge             = it_lips-ntgew
          IMPORTING
            menge               = it_lips-ntgew
          EXCEPTIONS
            error_in_conversion = 1
            no_success          = 2
            OTHERS              = 3.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.

      vg_total_vincular = it_lips-ntgew.

      vg_total_vinculado = vg_total_vinculado + vg_total_vincular.

      IF vg_total_possivel LT vg_total_vinculado.
        p_valida_beln = 6.
        EXIT.
      ENDIF.

    ELSE.
      p_valida_beln = 1.
      EXIT.
    ENDIF.

    CLEAR: wa_zdoc_exp.

    SELECT SINGLE *
      FROM zdoc_exp
      INTO wa_zdoc_exp
     WHERE vbeln EQ wa_vinc_remessa-vbeln.

    IF sy-subrc IS INITIAL.
      p_valida_beln = 4.
      EXIT.
    ENDIF.

  ENDIF.

ENDFORM.                    " VALIDAR_VBELN

FORM validar_due USING p_valida   TYPE sy-subrc
                       p_zsdt0170 TYPE zsdt0170.

  DATA: it_zsdt0024        TYPE TABLE OF zsdt0024 WITH HEADER LINE,
        it_likp            TYPE TABLE OF likp WITH HEADER LINE,
        vg_total_re        LIKE zreg_exportacao-nr_qtde,
        lt_zsdt0170        TYPE TABLE OF ty_zplac_due_antecipada WITH HEADER LINE,
        it_zdoc_exp_recusa TYPE TABLE OF zdoc_exp_recusa WITH HEADER LINE.

  CLEAR: p_zsdt0170.

  p_valida = 4.

  CHECK wa_vinc_remessa-numero_due IS NOT INITIAL.

  SELECT SINGLE *
    FROM zsdt0170 INTO p_zsdt0170
   WHERE numero_due  EQ wa_vinc_remessa-numero_due
     AND id_due_ref  EQ 0.

  "Não Localizado
  IF sy-subrc NE 0.
    p_valida = 1.
    MESSAGE s054.
    EXIT.
  ENDIF.

  PERFORM f_get_due_remessa TABLES lt_zsdt0170
                             USING wa_znom_transporte_alv-id_nomeacao_tran
                                   wa_vinc_remessa-vbeln.

  READ TABLE lt_zsdt0170 WITH KEY numero_due = wa_vinc_remessa-numero_due.
  IF sy-subrc NE 0.
    p_valida = 2.
    MESSAGE s055.
    EXIT.
  ENDIF.


*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      INPUT  = WA_ZNOM_PROGRAMACAO-ID_MATERIAL
*    IMPORTING
*      OUTPUT = WA_ZNOM_PROGRAMACAO-ID_MATERIAL.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      INPUT  = VG_REGIS-CD_MATERIAL
*    IMPORTING
*      OUTPUT = VG_REGIS-CD_MATERIAL.
*
*  "Material diferente e Não Similar
*  IF WA_ZNOM_PROGRAMACAO-ID_MATERIAL NE VG_REGIS-CD_MATERIAL.
*
*    SELECT * INTO TABLE IT_ZSDT0024
*      FROM ZSDT0024
*     WHERE MATNR1 EQ WA_ZNOM_PROGRAMACAO-ID_MATERIAL.
*
*    READ TABLE IT_ZSDT0024 WITH KEY MATNR2 = VG_REGIS-CD_MATERIAL.
*    IF NOT SY-SUBRC IS INITIAL.
*      P_VALIDA_REGIS = 3.
*      EXIT.
*    ENDIF.
*
*  ENDIF.

  "Valida Saldo de Registro de Exportação
*  SELECT * INTO TABLE IT_ZDOC_EXP
*    FROM ZDOC_EXP
*   WHERE ID_REGISTRO_EXPO EQ VG_REGIS-ID_REGISTRO_EXPO.
*
*  IF SY-SUBRC IS INITIAL.
*    SELECT * INTO TABLE IT_LIKP
*      FROM LIKP
*       FOR ALL ENTRIES IN IT_ZDOC_EXP
*     WHERE VBELN EQ IT_ZDOC_EXP-VBELN.
*  ENDIF.
*
*  SELECT * APPENDING TABLE IT_LIKP
*    FROM LIKP
*   WHERE VBELN EQ WA_VINC_REMESSA-VBELN.
*
*  VG_TOTAL_RE = 0.
*  LOOP AT IT_LIKP.
*
*    IF IT_LIKP-GEWEI NE 'KG'.
*
*      CALL FUNCTION 'ME_CONVERSION_MEINS'
*        EXPORTING
*          I_MATNR             = WA_ZNOM_PROGRAMACAO-ID_MATERIAL
*          I_MEIN1             = IT_LIKP-GEWEI
*          I_MEINS             = 'KG'
*          I_MENGE             = IT_LIKP-NTGEW
*        IMPORTING
*          MENGE               = IT_LIKP-NTGEW
*        EXCEPTIONS
*          ERROR_IN_CONVERSION = 1
*          NO_SUCCESS          = 2
*          OTHERS              = 3.
*      IF NOT SY-SUBRC IS INITIAL.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
*    ENDIF.
*
*    VG_TOTAL_RE = VG_TOTAL_RE + IT_LIKP-NTGEW.
*
*    SELECT * INTO TABLE IT_ZDOC_EXP_RECUSA
*      FROM ZDOC_EXP_RECUSA
*     WHERE VBELN_RE_EXP EQ IT_LIKP-VBELN.
*
*    LOOP AT IT_ZDOC_EXP_RECUSA.
*      VG_TOTAL_RE = VG_TOTAL_RE - IT_ZDOC_EXP_RECUSA-NM_QUANTIDADE.
*    ENDLOOP.
*
*  ENDLOOP.
*
*  IF VG_TOTAL_RE GT VG_REGIS-NR_QTDE.
*    P_VALIDA_REGIS = 4.
*    EXIT.
*  ENDIF.

  "Nomeação Correta
  p_valida = 0.
  wa_vinc_remessa-id_due           = p_zsdt0170-id_due.
  wa_vinc_remessa-id_nomeacao_tran = p_zsdt0170-id_nomeacao_tran.


ENDFORM.

FORM validar_inf_due_re USING p_valida TYPE sy-subrc.

  p_valida = 1.

  IF ( wa_vinc_remessa-nr_registro_expo IS NOT INITIAL AND
       wa_vinc_remessa-numero_due       IS NOT INITIAL ).
    MESSAGE s056.
    EXIT.
  ENDIF.

  IF ( wa_vinc_remessa-nr_registro_expo IS INITIAL AND
       wa_vinc_remessa-numero_due       IS INITIAL ).
    MESSAGE s056.
    EXIT.
  ENDIF.

  IF ( wa_vinc_remessa-id_registro_expo IS NOT INITIAL AND
       wa_vinc_remessa-id_due           IS NOT INITIAL ).
    MESSAGE s056.
    EXIT.
  ENDIF.

  p_valida = 0.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VALIDAR_REGIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM validar_regis  USING  p_valida_regis TYPE sy-subrc vg_regis TYPE zreg_exportacao.

  DATA: it_zsdt0024        TYPE TABLE OF zsdt0024 WITH HEADER LINE,
        it_likp            TYPE TABLE OF likp WITH HEADER LINE,
        vg_total_re        LIKE zreg_exportacao-nr_qtde,
        it_zdoc_exp_recusa TYPE TABLE OF zdoc_exp_recusa WITH HEADER LINE.

  p_valida_regis = 4.

  CHECK NOT wa_vinc_remessa-nr_registro_expo IS INITIAL.

  SELECT SINGLE *
    FROM zreg_exportacao
    INTO vg_regis
   WHERE nr_registro_expo EQ wa_vinc_remessa-nr_registro_expo
     AND in_status_comex  NE 'X'.

  "Não Localizado
  IF NOT sy-subrc IS INITIAL.
    p_valida_regis = 1.
    EXIT.
  ENDIF.

*  "Nomeação Diferente
*  if wa_znom_programacao-id_nomeacao_tran ne vg_regis-id_nomeacao_tran.
*    p_valida_regis = 2.
*    exit.
*  endif.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_znom_programacao-id_material
    IMPORTING
      output = wa_znom_programacao-id_material.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vg_regis-cd_material
    IMPORTING
      output = vg_regis-cd_material.

  "Material diferente e Não Similar
  IF wa_znom_programacao-id_material NE vg_regis-cd_material.

    SELECT * INTO TABLE it_zsdt0024
      FROM zsdt0024
     WHERE matnr1 EQ wa_znom_programacao-id_material.

    READ TABLE it_zsdt0024 WITH KEY matnr2 = vg_regis-cd_material.
    IF NOT sy-subrc IS INITIAL.
      p_valida_regis = 3.
      EXIT.
    ENDIF.

  ENDIF.

  "Valida Saldo de Registro de Exportação
  SELECT * INTO TABLE it_zdoc_exp
    FROM zdoc_exp
   WHERE id_registro_expo EQ vg_regis-id_registro_expo.

  IF sy-subrc IS INITIAL.
    SELECT * INTO TABLE it_likp
      FROM likp
       FOR ALL ENTRIES IN it_zdoc_exp
     WHERE vbeln EQ it_zdoc_exp-vbeln.
  ENDIF.

  SELECT * APPENDING TABLE it_likp
    FROM likp
   WHERE vbeln EQ wa_vinc_remessa-vbeln.

  vg_total_re = 0.
  LOOP AT it_likp.

    IF it_likp-gewei NE 'KG'.

      CALL FUNCTION 'ME_CONVERSION_MEINS'
        EXPORTING
          i_matnr             = wa_znom_programacao-id_material
          i_mein1             = it_likp-gewei
          i_meins             = 'KG'
          i_menge             = it_likp-ntgew
        IMPORTING
          menge               = it_likp-ntgew
        EXCEPTIONS
          error_in_conversion = 1
          no_success          = 2
          OTHERS              = 3.
      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDIF.

    vg_total_re = vg_total_re + it_likp-ntgew.

    SELECT * INTO TABLE it_zdoc_exp_recusa
      FROM zdoc_exp_recusa
     WHERE vbeln_re_exp EQ it_likp-vbeln.

    LOOP AT it_zdoc_exp_recusa.
      vg_total_re = vg_total_re - it_zdoc_exp_recusa-nm_quantidade.
    ENDLOOP.

  ENDLOOP.

  IF vg_total_re GT vg_regis-nr_qtde.
    p_valida_regis = 4.
    EXIT.
  ENDIF.

  "Nomeação Correta
  p_valida_regis = 0.
  wa_vinc_remessa-id_registro_expo = vg_regis-id_registro_expo.
  wa_vinc_remessa-id_nomeacao_tran = vg_regis-id_nomeacao_tran.

ENDFORM.                    " VALIDAR_REGIS

*&---------------------------------------------------------------------*
*&      Form  MESSAGE_REGIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM message_regis  USING  p_valida_regis TYPE sy-subrc.

  CASE p_valida_regis.
    WHEN 1.
      MESSAGE s007.
    WHEN 2.
      MESSAGE s014.
    WHEN 3.
      MESSAGE s015 WITH wa_vinc_remessa-nr_registro_expo wa_znom_programacao-id_material.
    WHEN 4.
      MESSAGE s017 WITH wa_vinc_remessa-nr_registro_expo.
  ENDCASE.

ENDFORM.                    " MESSAGE_REGIS

*&---------------------------------------------------------------------*
*&      Form  MESSAGE_VBELN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM message_vbeln  USING p_valida_vbeln TYPE sy-subrc.

  CASE p_valida_vbeln.
    WHEN 1.
      MESSAGE s006.
    WHEN 2.
      MESSAGE s008.
    WHEN 3.
      MESSAGE s009.
    WHEN 4.
      MESSAGE s012 WITH wa_vinc_remessa-vbeln wa_zdoc_exp-nr_registro_expo.
    WHEN 5.
      MESSAGE s016 WITH wa_vinc_remessa-vbeln wa_znom_programacao-id_material.
    WHEN 6.
      MESSAGE s028 WITH wa_vinc_remessa-vbeln.
  ENDCASE.

ENDFORM.                    " MESSAGE_VBELN

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_SELECAO_PROG_REME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VG_VERIFICA_SELECAO_REME  text
*----------------------------------------------------------------------*
FORM verifica_selecao_prog_reme  USING  p_verifica_selecao_reme TYPE sy-subrc.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CLEAR: wa_znom_prog_reme_alv,
         wa_znom_prog_reme.

  CALL METHOD plan_alv_prog_reme->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows WHERE index IS NOT INITIAL.
    READ TABLE it_znom_prog_reme_alv INTO wa_znom_prog_reme_alv INDEX wa_selected_rows-index.
    READ TABLE it_znom_prog_reme     INTO wa_znom_prog_reme
    WITH KEY id_nomeacao_tran = wa_znom_prog_reme_alv-id_nomeacao_tran
             id_empresa       = wa_znom_prog_reme_alv-id_empresa
             id_filial        = wa_znom_prog_reme_alv-id_filial
             id_material      = wa_znom_prog_reme_alv-id_material
             id_remessa       = wa_znom_prog_reme_alv-id_remessa
             id_registro_expo = wa_znom_prog_reme_alv-id_registro_expo.
  ENDLOOP.

  IF NOT wa_znom_prog_reme_alv IS INITIAL.
    p_verifica_selecao_reme = 0.
  ELSE.
    p_verifica_selecao_reme = 1.
  ENDIF.

ENDFORM.                    " VERIFICA_SELECAO_PROG_REME

*&---------------------------------------------------------------------*
*&      Form  GERAR_FATURAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_faturamento .

  DATA : vg_valida                TYPE char01,
         vg_verifica_selecao_reme TYPE sy-subrc,
         vg_nota_fat              TYPE sy-subrc.
  DATA : lt_msg LIKE bdcmsgcoll  OCCURS 1 WITH HEADER LINE.
  DATA : lt_dta LIKE bdcdata     OCCURS 1 WITH HEADER LINE.
  DATA : lt_ret TYPE TABLE OF bapireturn1 WITH HEADER LINE.

  PERFORM verifica_selecao_prog_reme USING vg_verifica_selecao_reme.

  IF vg_verifica_selecao_reme IS INITIAL.

    PERFORM verifica_execucao_filial USING wa_znom_programacao-id_filial vg_nota_fat.

    IF vg_nota_fat IS INITIAL.

      IF wa_znom_prog_reme_alv-vbeln IS INITIAL.
        PERFORM authority_check USING 'VF01' vg_valida.
        IF vg_valida IS INITIAL.
          CLEAR: lt_dta[].
          PERFORM f_bdc_field TABLES lt_dta  USING: 'X' 'SAPMV60A' '0102',
                                                    ' ' 'KOMFK-VBELN(01)'  wa_znom_prog_reme_alv-id_remessa,
                                                    ' ' 'BDC_OKCODE'       '=SICH'.
          CALL TRANSACTION 'VF01' USING lt_dta UPDATE 'S' MODE 'N'.
          PERFORM atualizar_remesas.
        ENDIF.
      ELSE.
        MESSAGE s018.
      ENDIF.

    ELSE.
      MESSAGE s022 WITH wa_znom_programacao-id_filial.
    ENDIF.
  ELSE.
    MESSAGE s010.
  ENDIF.

ENDFORM.                    " GERAR_FATURAMENTO

*&---------------------------------------------------------------------*
*&      Form  VALIDAR_NOTA_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM validar_nota_fiscal .

  DATA : vg_valida      TYPE char01,
         vg_verifica_nf TYPE sy-subrc,
         vg_nota_fil    TYPE sy-subrc.

  DATA : lt_dta LIKE bdcdata OCCURS 1 WITH HEADER LINE.

  PERFORM verifica_selecao_prog_reme USING vg_verifica_nf.

  IF vg_verifica_nf IS INITIAL.

    PERFORM verifica_conhecimentos_volume USING vg_verifica_nf.

    IF vg_verifica_nf IS INITIAL.

      PERFORM verifica_execucao_filial USING wa_znom_programacao-id_filial vg_nota_fil.

      IF vg_nota_fil IS INITIAL.

        IF NOT wa_znom_prog_reme_alv-docnum IS INITIAL.
          PERFORM authority_check USING 'ZNFE' vg_valida.
          IF vg_valida IS INITIAL.
*            CLEAR: lt_dta[].
*            PERFORM f_bdc_field TABLES lt_dta  USING: 'X' 'Z_1BNFE_MONITOR' '1000',
*                                                      ' ' 'DOCNUM-LOW'       wa_znom_prog_reme_alv-docnum,
*                                                      ' ' 'DATE0-LOW'       space,
*                                                      ' ' 'BUKRS-LOW'       wa_znom_prog_reme_alv-id_empresa,
*                                                      ' ' 'USER-LOW'        space.
*            CALL TRANSACTION 'ZNFE' USING lt_dta MODE 'A' UPDATE 'N'.

            SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_znom_prog_reme_alv-docnum.
            SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_znom_prog_reme_alv-id_empresa.
            CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
          ENDIF.
        ELSE.
          MESSAGE s024.
        ENDIF.
      ELSE.
        MESSAGE s022 WITH wa_znom_programacao-id_filial.
      ENDIF.
    ELSE.
      MESSAGE s023.
    ENDIF.
  ELSE.
    MESSAGE s010.
  ENDIF.

ENDFORM.                    " VALIDAR_NOTA_FISCAL

*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bdc_field  TABLES  it_bdc STRUCTURE bdcdata
                   USING  VALUE(p_flag)
                          VALUE(p_fnam)
                          VALUE(p_fval).

  DATA: wa_bdc TYPE bdcdata.
  IF NOT p_flag IS INITIAL.
    wa_bdc-program  = p_fnam.
    wa_bdc-dynpro   = p_fval.
    wa_bdc-dynbegin = 'X'.
  ELSE.
    wa_bdc-fnam = p_fnam.
    wa_bdc-fval = p_fval.
  ENDIF.
  APPEND wa_bdc TO it_bdc.

ENDFORM.                    " F_BDC_FIELD


*&---------------------------------------------------------------------*
*&      Form  TROCA_ABA_04A
*&---------------------------------------------------------------------*
*       Seleciona tabela de notas vinculadas e B/Ls
*----------------------------------------------------------------------*
FORM troca_aba_04a .

  DATA: vg_verifica TYPE sy-subrc.

  PERFORM verifica_selecao_prog_reme USING vg_verifica.

  IF vg_verifica IS INITIAL.
    PERFORM selecionar_notas_efetivadas.
    PERFORM consulta_conhecimentos_vinc.
  ELSE.
    MESSAGE s020.
  ENDIF.

ENDFORM.                    " TROCA_ABA_04A


*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_NOTAS_EFETIVADAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selecionar_notas_efetivadas .

  DATA: it_zdoc_nf_produtor_aux TYPE TABLE OF zdoc_nf_produtor WITH HEADER LINE,
        it_zdoc_nf_estorno      TYPE TABLE OF znom_reme_notase WITH HEADER LINE,
        it_j_1bnfdoc_aux        TYPE TABLE OF j_1bnfdoc WITH HEADER LINE,
        it_j_1bnfdoc            TYPE TABLE OF j_1bnfdoc WITH HEADER LINE,
        it_j_1bnflin            TYPE TABLE OF j_1bnflin WITH HEADER LINE,
        it_fornecedor           TYPE TABLE OF lfa1 WITH HEADER LINE.

  CLEAR: it_zdoc_nf_produtor[], it_zdoc_nf_produtor_alv[].

  "it_zdoc_nf_produtor_alv.
  SELECT * INTO TABLE it_zdoc_nf_produtor
    FROM zdoc_nf_produtor
   WHERE vbeln EQ wa_znom_prog_reme_alv-id_remessa.

  IF sy-subrc IS INITIAL.

    CLEAR it_zdoc_nf_produtor_aux[].
    MOVE it_zdoc_nf_produtor[] TO it_zdoc_nf_produtor_aux[].
    SORT it_zdoc_nf_produtor_aux BY docnum_prod.
    DELETE ADJACENT DUPLICATES FROM it_zdoc_nf_produtor_aux COMPARING docnum_prod.

    SELECT * INTO TABLE it_zdoc_nf_estorno
      FROM znom_reme_notase
       FOR ALL ENTRIES IN it_zdoc_nf_produtor_aux
     WHERE docnum EQ it_zdoc_nf_produtor_aux-docnum_prod.

    SELECT * INTO TABLE it_j_1bnfdoc
      FROM j_1bnfdoc
       FOR ALL ENTRIES IN it_zdoc_nf_produtor_aux
     WHERE docnum EQ it_zdoc_nf_produtor_aux-docnum_prod.

    LOOP AT it_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_nota_fiscal>) WHERE partyp EQ 'B'.
      PERFORM f_converte_parid_br_to_lf CHANGING <fs_nota_fiscal>-parid
                                                 <fs_nota_fiscal>-partyp
                                                 <fs_nota_fiscal>-parvw.
    ENDLOOP.

    "IF sy-subrc IS INITIAL.

    CLEAR: it_j_1bnfdoc_aux[].
    MOVE it_j_1bnfdoc[] TO it_j_1bnfdoc_aux[].
    SORT it_j_1bnfdoc_aux[] BY parid.
    DELETE ADJACENT DUPLICATES FROM it_j_1bnfdoc_aux COMPARING parid.

    SELECT * INTO TABLE it_fornecedor
      FROM lfa1
       FOR ALL ENTRIES IN it_j_1bnfdoc_aux
     WHERE lifnr EQ it_j_1bnfdoc_aux-parid.

    " ENDIF.

    CLEAR it_zdoc_nf_produtor_aux[].
    MOVE it_zdoc_nf_produtor[] TO it_zdoc_nf_produtor_aux[].
    SORT it_zdoc_nf_produtor_aux BY docnum_prod itmnum_prod.
    DELETE ADJACENT DUPLICATES FROM it_zdoc_nf_produtor_aux COMPARING docnum_prod itmnum_prod.

    SELECT * INTO TABLE it_j_1bnflin
      FROM j_1bnflin
       FOR ALL ENTRIES IN it_zdoc_nf_produtor_aux
     WHERE docnum EQ it_zdoc_nf_produtor_aux-docnum_prod
       AND itmnum EQ it_zdoc_nf_produtor_aux-itmnum_prod.

  ENDIF.

  LOOP AT it_zdoc_nf_produtor INTO wa_zdoc_nf_produtor.

    CLEAR: wa_zdoc_nf_produtor_alv.
    MOVE-CORRESPONDING wa_zdoc_nf_produtor TO wa_zdoc_nf_produtor_alv.

    READ TABLE it_j_1bnfdoc WITH KEY docnum = wa_zdoc_nf_produtor-docnum_prod.
    IF sy-subrc IS INITIAL.
      wa_zdoc_nf_produtor_alv-docdat = it_j_1bnfdoc-docdat.
      wa_zdoc_nf_produtor_alv-model  = it_j_1bnfdoc-model.
      wa_zdoc_nf_produtor_alv-series = it_j_1bnfdoc-series.
      wa_zdoc_nf_produtor_alv-branch = it_j_1bnfdoc-branch.
      wa_zdoc_nf_produtor_alv-parvw  = it_j_1bnfdoc-parvw.
      wa_zdoc_nf_produtor_alv-parid  = it_j_1bnfdoc-parid.
      IF it_j_1bnfdoc-nfe IS INITIAL.
        MOVE it_j_1bnfdoc-nfnum TO wa_zdoc_nf_produtor_alv-nfenum.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_zdoc_nf_produtor_alv-nfenum
          IMPORTING
            output = wa_zdoc_nf_produtor_alv-nfenum.
      ELSE.
        wa_zdoc_nf_produtor_alv-nfenum = it_j_1bnfdoc-nfenum.
      ENDIF.
    ENDIF.

    READ TABLE it_j_1bnflin WITH KEY docnum = wa_zdoc_nf_produtor-docnum_prod
                                     itmnum = wa_zdoc_nf_produtor-itmnum_prod.
    IF sy-subrc IS INITIAL.
      wa_zdoc_nf_produtor_alv-matnr = it_j_1bnflin-matnr.
      wa_zdoc_nf_produtor_alv-maktx = it_j_1bnflin-maktx.
      wa_zdoc_nf_produtor_alv-nbm   = it_j_1bnflin-nbm.
      wa_zdoc_nf_produtor_alv-charg = it_j_1bnflin-charg.
      wa_zdoc_nf_produtor_alv-cfop  = it_j_1bnflin-cfop.
    ENDIF.

    READ TABLE it_fornecedor WITH KEY lifnr = it_j_1bnfdoc-parid.
    IF sy-subrc IS INITIAL.
      wa_zdoc_nf_produtor_alv-name1 = it_fornecedor-name1.
    ENDIF.
    wa_zdoc_nf_produtor_alv-nr_quantidade2 = wa_zdoc_nf_produtor_alv-menge.

    "Verifica se documento está estornado pelo XI
    READ TABLE it_zdoc_nf_estorno WITH KEY docnum = wa_zdoc_nf_produtor_alv-docnum_prod
                                           itmnum = wa_zdoc_nf_produtor_alv-itmnum_prod.
    IF sy-subrc IS INITIAL.
      wa_zdoc_nf_produtor_alv-rowcolor = c_grid_color_c600.
    ENDIF.

    APPEND wa_zdoc_nf_produtor_alv TO it_zdoc_nf_produtor_alv.

  ENDLOOP.

ENDFORM.                    " SELECIONAR_NOTAS_EFETIVADAS

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_ZDOC_NF_PRODUTOR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_alv_zdoc_nf_produtor OUTPUT.

  PERFORM plan_cria_zdoc_nf_produtor_alv.

ENDMODULE.                 " CRIA_ALV_ZDOC_NF_PRODUTOR  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_ZDOC_NF_PRODUTOR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plan_cria_zdoc_nf_produtor_alv .

  CONSTANTS: tabela_zdoc_nf_produtor TYPE string VALUE 'IT_ZDOC_NF_PRODUTOR_ALV'.

  DATA: text_n001 TYPE c LENGTH 50 VALUE 'Nr.SAP',
        text_n002 TYPE c LENGTH 50 VALUE 'Nr.Item',
        text_n003 TYPE c LENGTH 50 VALUE 'Filial',
        text_n004 TYPE c LENGTH 50 VALUE 'Mod',
        text_n005 TYPE c LENGTH 50 VALUE 'Série',
        text_n006 TYPE c LENGTH 50 VALUE 'Número',
        text_n007 TYPE c LENGTH 50 VALUE 'Produtor',
        text_n008 TYPE c LENGTH 50 VALUE 'Nome Produtor',
        text_n009 TYPE c LENGTH 50 VALUE 'Produto',
        text_n010 TYPE c LENGTH 50 VALUE 'Nome Produto',
        text_n011 TYPE c LENGTH 50 VALUE 'Safra',
        text_n012 TYPE c LENGTH 50 VALUE 'CFOP',
        text_n013 TYPE c LENGTH 50 VALUE 'Quantidade',
        text_n016 TYPE c LENGTH 50 VALUE 'Qtd. Efetivada',
        text_n017 TYPE c LENGTH 50 VALUE 'Qtd. Efetivar',
        text_n014 TYPE c LENGTH 50 VALUE 'Dt. Emissão',
        text_n015 TYPE c LENGTH 50 VALUE 'NCM',
        text_n018 TYPE c LENGTH 50 VALUE 'Grp.Ret.'.

  IF plan_prim_nf_produtor IS INITIAL.

    CREATE OBJECT plan_container_nf_produtor
      EXPORTING
        container_name = 'CTN_NF_PRODUTOR'.

    CREATE OBJECT plan_alv_nf_produtor
      EXPORTING
        i_parent = plan_container_nf_produtor.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_nf_produtor USING:
        tabela_zdoc_nf_produtor 'GRP_RETORNO'    text_n018 ' ' 01 02 space space   space space space space             space,
        tabela_zdoc_nf_produtor 'DOCNUM_PROD'    text_n001 'X' 01 11 space 'ALPHA' space space space c_grid_color_c200 space,
        tabela_zdoc_nf_produtor 'ITMNUM_PROD'    text_n002 ' ' 02 06 space space   space space space space             space,
        tabela_zdoc_nf_produtor 'BRANCH'         text_n003 ' ' 03 04 space space   space space space c_grid_color_c200 space,
        tabela_zdoc_nf_produtor 'MODEL'          text_n004 ' ' 04 03 space space   space space space space             space,
        tabela_zdoc_nf_produtor 'SERIES'         text_n005 ' ' 05 03 space space   space space space c_grid_color_c200 space,
        tabela_zdoc_nf_produtor 'NFENUM'         text_n006 ' ' 06 09 space space   space space space c_grid_color_c200 space,
        tabela_zdoc_nf_produtor 'NR_QUANTIDADE2' text_n013 ' ' 07 15 space space   'X'   space space c_grid_color_c300 space,
        tabela_zdoc_nf_produtor 'DOCDAT'         text_n014 ' ' 08 10 space space   space space space space             space,
        tabela_zdoc_nf_produtor 'PARID'          text_n007 ' ' 09 10 space 'ALPHA' space space space space             space,
        tabela_zdoc_nf_produtor 'NAME1'          text_n008 ' ' 10 25 space space   space space space space             space,
        tabela_zdoc_nf_produtor 'MATNR'          text_n009 ' ' 11 10 space 'ALPHA' space space space space             space,
        tabela_zdoc_nf_produtor 'MAKTX'          text_n010 ' ' 12 25 space space   space space space space             space,
        tabela_zdoc_nf_produtor 'NBM'            text_n015 ' ' 13 10 space space   space space space space             space,
        tabela_zdoc_nf_produtor 'CHARG'          text_n011 ' ' 14 05 space space   space space space space             space,
        tabela_zdoc_nf_produtor 'CFOP'           text_n012 ' ' 15 07 space 'CFOBR' space space space space             space.

    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra      = c_x.
    plan_gs_layout-sel_mode   = space.
    plan_gs_layout-grid_title = 'Notas Fiscais - Compromisso Exportação'.
    plan_gs_layout-info_fname = 'ROWCOLOR'.

    CALL METHOD plan_alv_nf_produtor->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = plan_gs_layout
      CHANGING
        it_fieldcatalog = plan_catalogo_nf_produtor
        it_outtab       = it_zdoc_nf_produtor_alv[].

*   Create Object for Event Handler
    CREATE OBJECT plan_event_handler_produtor.
    SET HANDLER plan_event_handler_produtor->handle_hotspot_click_produtor FOR plan_alv_nf_produtor.

    plan_prim_nf_produtor = c_x.
  ENDIF.

  CALL METHOD plan_alv_nf_produtor->refresh_table_display.

  CALL METHOD plan_alv_nf_produtor->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_ZDOC_NF_PRODUTOR_ALV

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_CONHECIMENTOS_VINC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM consulta_conhecimentos_vinc.

  TYPES: BEGIN OF ty_pais,
           sign   TYPE char1,
           option TYPE  char2,
           low    TYPE land1,
           high   TYPE land1,
         END OF ty_pais.

  DATA: it_zdoc_rem_bl_aux TYPE TABLE OF zdoc_rem_bl WITH HEADER LINE,
        it_znom_conhec_aux TYPE TABLE OF znom_conhec WITH HEADER LINE,
        it_pais            TYPE TABLE OF ty_pais WITH HEADER LINE,
        it_t005t           TYPE TABLE OF t005t WITH HEADER LINE.

  CLEAR: it_zdoc_rem_bl_alv[], it_zdoc_rem_bl[].

  SELECT SINGLE * INTO wa_zdoc_exp
    FROM zdoc_exp
   WHERE vbeln            = wa_znom_prog_reme_alv-id_remessa
     AND id_registro_expo = wa_znom_prog_reme_alv-id_registro_expo.

  IF sy-subrc IS INITIAL.
    "Conhecimentos Vinculados
    SELECT * INTO TABLE it_zdoc_rem_bl
      FROM zdoc_rem_bl
     WHERE id_doc_exp EQ wa_zdoc_exp-id_doc_exp.

    "Informações dos conhecimentos
    IF sy-subrc IS INITIAL.
      MOVE it_zdoc_rem_bl[] TO it_zdoc_rem_bl_aux[].
      SORT it_zdoc_rem_bl_aux BY id_conhec.
      DELETE ADJACENT DUPLICATES FROM it_zdoc_rem_bl_aux COMPARING id_conhec.

      SELECT * INTO TABLE it_znom_conhec
        FROM znom_conhec
         FOR ALL ENTRIES IN it_zdoc_rem_bl_aux
       WHERE id_nomeacao_tran EQ wa_znom_prog_reme_alv-id_nomeacao_tran
         AND id_conhec        EQ it_zdoc_rem_bl_aux-id_conhec.

      CLEAR: it_znom_conhec_aux[].
      MOVE it_znom_conhec[] TO it_znom_conhec_aux[].
      SORT it_znom_conhec_aux BY sg_pais_destino.
      DELETE ADJACENT DUPLICATES FROM it_znom_conhec_aux COMPARING sg_pais_destino.

      LOOP AT it_znom_conhec_aux INTO wa_znom_conhec.
        it_pais-sign   = 'I'.
        it_pais-option = 'EQ'.
        it_pais-low    = wa_znom_conhec-sg_pais_destino.
        it_pais-high   = wa_znom_conhec-sg_pais_destino.
        APPEND it_pais.
      ENDLOOP.

      IF NOT it_pais[] IS INITIAL.
        SELECT * INTO TABLE it_t005t
          FROM t005t
         WHERE spras EQ sy-langu
           AND land1 IN it_pais.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT it_zdoc_rem_bl INTO wa_zdoc_rem_bl.
    CLEAR: wa_zdoc_rem_bl_alv.
    MOVE-CORRESPONDING wa_zdoc_rem_bl TO wa_zdoc_rem_bl_alv.

    READ TABLE it_znom_conhec WITH KEY id_conhec        = wa_zdoc_rem_bl-id_conhec
                                       id_nomeacao_tran = wa_zdoc_rem_bl-id_nomeacao_tran.
    IF sy-subrc IS INITIAL.
      wa_zdoc_rem_bl_alv-ds_tipo = it_znom_conhec-ds_tipo.

      it_pais-low = it_znom_conhec-sg_pais_destino.
      READ TABLE it_t005t WITH KEY land1 = it_pais-low.
      IF sy-subrc IS INITIAL.
        wa_zdoc_rem_bl_alv-pais = it_t005t-landx.
      ENDIF.

    ENDIF.

    APPEND wa_zdoc_rem_bl_alv TO it_zdoc_rem_bl_alv.
  ENDLOOP.

  PERFORM consulta_conhecimento_a_vinc.

ENDFORM.                    " CONSULTA_CONHECIMENTOS_VINC

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_CONHECIMENTO_VINC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_alv_conhecimento_vinc OUTPUT.

  PERFORM plan_cria_conhec_vinc_alv.

ENDMODULE.                 " CRIA_ALV_CONHECIMENTO_VINC  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_CONHEC_VINC_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plan_cria_conhec_vinc_alv .

  CONSTANTS: tabela_zdoc_conhec_vinc TYPE string VALUE 'IT_ZDOC_REM_BL_ALV'.

  DATA: text_n001 TYPE c LENGTH 50 VALUE 'Id.',
        text_n002 TYPE c LENGTH 50 VALUE 'Número',
        text_n003 TYPE c LENGTH 50 VALUE 'Tipo',
        text_n004 TYPE c LENGTH 50 VALUE 'Data.',
        text_n005 TYPE c LENGTH 50 VALUE 'Quantidade',
        text_n006 TYPE c LENGTH 50 VALUE 'País Dest.'.

  IF plan_prim_conhec_vinc IS INITIAL.

    CREATE OBJECT plan_container_conhec_vinc
      EXPORTING
        container_name = 'CTN_CONHEC_VINC'.

    CREATE OBJECT plan_alv_conhec_vinc
      EXPORTING
        i_parent = plan_container_conhec_vinc.

    CREATE OBJECT toolbar_conhec_vinc_alv
      EXPORTING
        io_alv_grid = plan_alv_conhec_vinc.

    SET HANDLER toolbar_conhec_vinc_alv->on_toolbar FOR plan_alv_conhec_vinc.
    SET HANDLER toolbar_conhec_vinc_alv->handle_user_command FOR plan_alv_conhec_vinc.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_conhec_vinc USING:
        tabela_zdoc_conhec_vinc 'ID_CONHEC'    text_n001 ' ' 01 05 space 'ALPHA' space space space space             space,
        tabela_zdoc_conhec_vinc 'NR_CONHEC'    text_n002 ' ' 02 06 space space   space space space space             space,
        tabela_zdoc_conhec_vinc 'DS_TIPO'      text_n003 ' ' 03 15 space space   space space space space             space,
        tabela_zdoc_conhec_vinc 'DT_DATA'      text_n004 ' ' 04 10 space space   space space space space             space,
        tabela_zdoc_conhec_vinc 'NR_QTDE_VINC' text_n005 ' ' 05 15 space space   'X'   space space c_grid_color_c300 space,
        tabela_zdoc_conhec_vinc 'PAIS'         text_n006 ' ' 06 20 space space   space space space space             space.

    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra      = c_x.
    plan_gs_layout-sel_mode   = space.
    plan_gs_layout-sel_mode   = c_a.

    CALL METHOD plan_alv_conhec_vinc->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = plan_gs_layout
      CHANGING
        it_fieldcatalog = plan_catalogo_conhec_vinc
        it_outtab       = it_zdoc_rem_bl_alv[].

    plan_prim_conhec_vinc = c_x.

  ENDIF.

  CALL METHOD plan_alv_conhec_vinc->refresh_table_display.

  CALL METHOD plan_alv_conhec_vinc->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_CONHEC_VINC_ALV

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_CONHECIMENTO_A_VINC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_alv_conhecimento_a_vinc OUTPUT.

  PERFORM plan_cria_conhec_a_vinc_alv.

ENDMODULE.                 " CRIA_ALV_CONHECIMENTO_A_VINC  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_CONHEC_A_VINC_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plan_cria_conhec_a_vinc_alv .

  CONSTANTS: tabela_zdoc_conhec_a_vinc TYPE string VALUE 'IT_ZNOM_CONHEC_ALV'.

  DATA: text_n001 TYPE c LENGTH 50 VALUE 'Id.',
        text_n002 TYPE c LENGTH 50 VALUE 'Número',
        text_n003 TYPE c LENGTH 50 VALUE 'Tipo',
        text_n004 TYPE c LENGTH 50 VALUE 'Data.',
        text_n005 TYPE c LENGTH 50 VALUE 'Quantidade',
        text_n006 TYPE c LENGTH 50 VALUE 'Qtd. Utilizada',
        text_n007 TYPE c LENGTH 50 VALUE 'Qtd. Saldo',
        text_n008 TYPE c LENGTH 50 VALUE 'País Dest.'.

  IF plan_prim_conhec_a_vinc IS INITIAL.

    CREATE OBJECT plan_container_conhec_a_vinc
      EXPORTING
        container_name = 'CTN_CONHEC_A_VINC'.

    CREATE OBJECT plan_alv_conhec_a_vinc
      EXPORTING
        i_parent = plan_container_conhec_a_vinc.

    CREATE OBJECT toolbar_conhec_a_vinc_alv
      EXPORTING
        io_alv_grid = plan_alv_conhec_a_vinc.

    SET HANDLER toolbar_conhec_a_vinc_alv->on_toolbar FOR plan_alv_conhec_a_vinc.
    SET HANDLER toolbar_conhec_a_vinc_alv->handle_user_command FOR plan_alv_conhec_a_vinc.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_conhec_a_vinc USING:
        tabela_zdoc_conhec_a_vinc 'ID_CONHEC'     text_n001 ' ' 01 05 space 'ALPHA' space space space space             space,
        tabela_zdoc_conhec_a_vinc 'NR_CONHEC'     text_n002 ' ' 02 06 space space   space space space space             space,
        tabela_zdoc_conhec_a_vinc 'DS_TIPO'       text_n003 ' ' 03 08 space space   space space space space             space,
        tabela_zdoc_conhec_a_vinc 'DT_DATA'       text_n004 ' ' 04 10 space space   space space space space             space,
        tabela_zdoc_conhec_a_vinc 'NR_QUANTIDADE' text_n005 ' ' 05 15 space space   'X'   space space c_grid_color_c300 space,
        tabela_zdoc_conhec_a_vinc 'NR_UTILIZADA'  text_n006 ' ' 05 15 space space   'X'   space space c_grid_color_c400 space,
        tabela_zdoc_conhec_a_vinc 'NR_SALDO'      text_n007 ' ' 05 15 space space   'X'   space space c_grid_color_c500 space,
        tabela_zdoc_conhec_a_vinc 'PAIS'          text_n008 ' ' 06 20 space space   space space space space             space.

    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra      = c_x.
    plan_gs_layout-sel_mode   = c_a.

    CALL METHOD plan_alv_conhec_a_vinc->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = plan_gs_layout
      CHANGING
        it_fieldcatalog = plan_catalogo_conhec_a_vinc
        it_outtab       = it_znom_conhec_alv[].

    plan_prim_conhec_a_vinc = c_x.

  ENDIF.

  CALL METHOD plan_alv_conhec_a_vinc->refresh_table_display.

  CALL METHOD plan_alv_conhec_a_vinc->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_CONHEC_A_VINC_ALV

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_CONHECIMENTO_A_VINC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM consulta_conhecimento_a_vinc .

  TYPES: BEGIN OF ty_pais,
           sign   TYPE char1,
           option TYPE  char2,
           low    TYPE land1,
           high   TYPE land1,
         END OF ty_pais.

  DATA: it_pais            TYPE TABLE OF ty_pais     WITH HEADER LINE,
        it_t005t           TYPE TABLE OF t005t       WITH HEADER LINE,
        it_znom_conhec_aux TYPE TABLE OF znom_conhec WITH HEADER LINE,
        it_zdoc_rem_bl_aux TYPE TABLE OF zdoc_rem_bl WITH HEADER LINE.

  CLEAR: it_znom_conhec_alv[], it_znom_conhec[].

  SELECT * INTO TABLE it_znom_conhec
    FROM znom_conhec AS n
   WHERE id_nomeacao_tran EQ wa_znom_prog_reme_alv-id_nomeacao_tran.

  IF NOT it_znom_conhec[] IS INITIAL.
    SELECT * INTO TABLE it_zdoc_rem_bl_aux
      FROM zdoc_rem_bl AS b
       FOR ALL ENTRIES IN it_znom_conhec
     WHERE b~id_conhec        EQ it_znom_conhec-id_conhec
       AND b~id_nomeacao_tran EQ it_znom_conhec-id_nomeacao_tran
       AND NOT EXISTS ( SELECT * FROM zdoc_exp_recusa AS e WHERE e~id_doc_exp EQ b~id_doc_exp ).
  ENDIF.

  SORT it_znom_conhec BY id_conhec.

  CLEAR: it_znom_conhec_aux[].
  MOVE it_znom_conhec[] TO it_znom_conhec_aux[].
  SORT it_znom_conhec_aux BY sg_pais_destino.
  DELETE ADJACENT DUPLICATES FROM it_znom_conhec_aux COMPARING sg_pais_destino.

  LOOP AT it_znom_conhec_aux INTO wa_znom_conhec.
    it_pais-sign   = 'I'.
    it_pais-option = 'EQ'.
    it_pais-low    = wa_znom_conhec-sg_pais_destino.
    it_pais-high   = wa_znom_conhec-sg_pais_destino.
    APPEND it_pais.
  ENDLOOP.

  IF NOT it_pais[] IS INITIAL.
    SELECT * INTO TABLE it_t005t
      FROM t005t
     WHERE spras EQ sy-langu
       AND land1 IN it_pais.
  ENDIF.

  LOOP AT it_znom_conhec INTO wa_znom_conhec.
    CLEAR: wa_zdoc_rem_bl_alv.
    MOVE-CORRESPONDING wa_znom_conhec TO wa_znom_conhec_alv.

    it_pais-low = wa_znom_conhec-sg_pais_destino.
    READ TABLE it_t005t WITH KEY land1 = it_pais-low.
    IF sy-subrc IS INITIAL.
      wa_znom_conhec_alv-pais = it_t005t-landx.
    ENDIF.

    wa_znom_conhec_alv-nr_quantidade = wa_znom_conhec-nr_qtde.
    wa_znom_conhec_alv-nr_saldo      = wa_znom_conhec-nr_qtde.
    wa_znom_conhec_alv-nr_utilizada  = 0.

    LOOP AT it_zdoc_rem_bl_aux INTO wa_zdoc_rem_bl WHERE id_conhec        EQ wa_znom_conhec-id_conhec
                                                     AND id_nomeacao_tran EQ wa_znom_conhec-id_nomeacao_tran.
      wa_znom_conhec_alv-nr_utilizada  = wa_znom_conhec_alv-nr_utilizada + wa_zdoc_rem_bl-nr_qtde_vinc.
      wa_znom_conhec_alv-nr_saldo      = wa_znom_conhec_alv-nr_saldo - wa_zdoc_rem_bl-nr_qtde_vinc.
    ENDLOOP.
    APPEND wa_znom_conhec_alv TO it_znom_conhec_alv.
  ENDLOOP.

ENDFORM.                    " CONSULTA_CONHECIMENTO_A_VINC

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_CONHECIMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM vincular_conhecimento .

  DATA: it_znom_conhec_alv_sel TYPE TABLE OF zplac_znom_conhec WITH HEADER LINE,
        it_selected_rows       TYPE lvc_t_row,
        wa_selected_rows       TYPE lvc_s_row,
        vl_id_item             TYPE zid_item,
        vg_nr_qtde_vinc        LIKE zdoc_rem_bl-nr_qtde_vinc,
        vg_nr_qtde_reme        LIKE zdoc_rem_bl-nr_qtde_vinc,
        vg_vinc_conh           TYPE sy-subrc,
        vg_valida_nfe          TYPE sy-subrc.

  PERFORM verifica_execucao_filial USING wa_znom_programacao-id_filial vg_vinc_conh.

  IF vg_vinc_conh IS INITIAL.

    PERFORM verifica_alterar USING wa_znom_prog_reme_alv vg_valida_nfe.

    IF NOT vg_valida_nfe IS INITIAL.
      EXIT.
    ENDIF.

    CALL METHOD plan_alv_conhec_a_vinc->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_rows.

    LOOP AT it_selected_rows INTO wa_selected_rows WHERE index IS NOT INITIAL.
      READ TABLE it_znom_conhec_alv INTO wa_znom_conhec_alv INDEX wa_selected_rows-index.
      APPEND wa_znom_conhec_alv TO it_znom_conhec_alv_sel.
    ENDLOOP.

    CHECK NOT it_znom_conhec_alv_sel[] IS INITIAL.

    vg_nr_qtde_vinc = 0.
    LOOP AT it_zdoc_rem_bl_alv INTO wa_zdoc_rem_bl_alv.
      vg_nr_qtde_vinc = vg_nr_qtde_vinc + wa_zdoc_rem_bl_alv-nr_qtde_vinc.
    ENDLOOP.

    IF wa_znom_prog_reme_alv-gewei NE 'KG'.
      CALL FUNCTION 'ME_CONVERSION_MEINS'
        EXPORTING
          i_matnr             = wa_znom_prog_reme_alv-id_material
          i_mein1             = wa_znom_prog_reme_alv-gewei
          i_meins             = 'KG'
          i_menge             = wa_znom_prog_reme_alv-ntgew
        IMPORTING
          menge               = vg_nr_qtde_reme
        EXCEPTIONS
          error_in_conversion = 1
          no_success          = 2
          OTHERS              = 3.
      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
      vg_nr_qtde_reme = wa_znom_prog_reme_alv-ntgew.
    ENDIF.

    vg_nr_qtde_vinc = vg_nr_qtde_reme - vg_nr_qtde_vinc.

    IF vg_qtd_vinc_conh GT 0.
      IF vg_nr_qtde_vinc GT vg_qtd_vinc_conh.
        vg_nr_qtde_vinc = vg_qtd_vinc_conh.
      ENDIF.
    ENDIF.

    SELECT SINGLE * INTO wa_zdoc_exp
      FROM zdoc_exp
     WHERE vbeln            EQ wa_znom_prog_reme_alv-id_remessa
       AND id_registro_expo EQ wa_znom_prog_reme_alv-id_registro_expo.

    IF sy-subrc IS INITIAL.

      CLEAR: vl_id_item.

      SELECT MAX( id_doc_item )
        FROM zdoc_rem_bl
        INTO vl_id_item
       WHERE id_doc_exp EQ wa_zdoc_exp-id_doc_exp.

      IF vl_id_item IS INITIAL.
        vl_id_item = 1.
      ELSE.
        ADD 1 TO vl_id_item.
      ENDIF.

      LOOP AT it_znom_conhec_alv_sel INTO wa_znom_conhec_alv.

        IF vg_nr_qtde_vinc GT 0.

          READ TABLE it_zdoc_rem_bl INTO wa_zdoc_rem_bl WITH KEY id_conhec = wa_znom_conhec_alv-id_conhec.
          IF sy-subrc IS INITIAL.
            IF vg_nr_qtde_vinc GT wa_znom_conhec_alv-nr_saldo.
              wa_zdoc_rem_bl-nr_qtde_vinc = wa_zdoc_rem_bl-nr_qtde_vinc + wa_znom_conhec_alv-nr_saldo.
              vg_nr_qtde_vinc = vg_nr_qtde_vinc - wa_znom_conhec_alv-nr_saldo.
            ELSE.
              wa_zdoc_rem_bl-nr_qtde_vinc = wa_zdoc_rem_bl-nr_qtde_vinc + vg_nr_qtde_vinc.
              vg_nr_qtde_vinc = 0.
            ENDIF.
          ELSE.
            wa_zdoc_rem_bl-id_doc_exp        = wa_zdoc_exp-id_doc_exp.
            wa_zdoc_rem_bl-id_doc_item       = vl_id_item.
            wa_zdoc_rem_bl-id_conhec         = wa_znom_conhec_alv-id_conhec.
            wa_zdoc_rem_bl-id_nomeacao_tran  = wa_znom_conhec_alv-id_nomeacao_tran.
            wa_zdoc_rem_bl-dt_data           = wa_znom_conhec_alv-dt_data.
            wa_zdoc_rem_bl-nr_qtde           = wa_znom_conhec_alv-nr_qtde.
            wa_zdoc_rem_bl-nr_conhec         = wa_znom_conhec_alv-nr_conhec.
            wa_zdoc_rem_bl-id_unidade        = 'KG'.
            IF vg_nr_qtde_vinc GT wa_znom_conhec_alv-nr_saldo.
              wa_zdoc_rem_bl-nr_qtde_vinc = wa_znom_conhec_alv-nr_saldo.
              vg_nr_qtde_vinc = vg_nr_qtde_vinc - wa_znom_conhec_alv-nr_saldo.
            ELSE.
              wa_zdoc_rem_bl-nr_qtde_vinc = vg_nr_qtde_vinc.
              vg_nr_qtde_vinc = 0.
            ENDIF.
            ADD 1 TO vl_id_item.
          ENDIF.

          wa_znom_conhec_alv-nr_saldo     = wa_znom_conhec_alv-nr_saldo     - wa_zdoc_rem_bl-nr_qtde_vinc.
          wa_znom_conhec_alv-nr_utilizada = wa_znom_conhec_alv-nr_utilizada + wa_zdoc_rem_bl-nr_qtde_vinc.

          MODIFY zdoc_rem_bl FROM wa_zdoc_rem_bl.
        ENDIF.
      ENDLOOP.

      PERFORM consulta_conhecimentos_vinc.

    ELSE.
      MESSAGE e019 WITH wa_znom_prog_reme_alv-id_remessa wa_znom_prog_reme_alv-nr_registro_expo.
    ENDIF.

  ELSE.
    MESSAGE s022 WITH wa_znom_programacao-id_filial.
  ENDIF.
ENDFORM.                    " VINCULAR_CONHECIMENTO

*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR_CONHECIMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM desvincular_conhecimento .

  DATA: it_zdoc_rem_bl_alv_sel TYPE TABLE OF zplac_zdoc_rem_bl WITH HEADER LINE,
        it_selected_rows       TYPE lvc_t_row,
        wa_selected_rows       TYPE lvc_s_row,
        vg_desv_conh           TYPE sy-subrc,
        vg_valida_nfe          TYPE sy-subrc.

  PERFORM verifica_execucao_filial USING wa_znom_programacao-id_filial vg_desv_conh.

  IF vg_desv_conh IS INITIAL.

    PERFORM verifica_alterar USING wa_znom_prog_reme_alv vg_valida_nfe.

    IF NOT vg_valida_nfe IS INITIAL.
      EXIT.
    ENDIF.

    CALL METHOD plan_alv_conhec_vinc->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_rows.

    LOOP AT it_selected_rows INTO wa_selected_rows WHERE index IS NOT INITIAL.
      READ TABLE it_zdoc_rem_bl_alv INTO wa_zdoc_rem_bl_alv INDEX wa_selected_rows-index.
      APPEND wa_zdoc_rem_bl_alv TO it_zdoc_rem_bl_alv_sel.
    ENDLOOP.

    CHECK NOT it_zdoc_rem_bl_alv_sel[] IS INITIAL.

    LOOP AT it_zdoc_rem_bl_alv_sel INTO wa_zdoc_rem_bl_alv.
      DELETE FROM zdoc_rem_bl WHERE id_doc_exp  EQ wa_zdoc_rem_bl_alv-id_doc_exp
                                AND id_doc_item EQ wa_zdoc_rem_bl_alv-id_doc_item.
    ENDLOOP.

    IF NOT it_zdoc_rem_bl_alv_sel[] IS INITIAL.
      COMMIT WORK.
    ENDIF.

    PERFORM consulta_conhecimentos_vinc.

  ELSE.
    MESSAGE s022 WITH wa_znom_programacao-id_filial.
  ENDIF.

ENDFORM.                    " DESVINCULAR_CONHECIMENTO

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ALTERAR
*&---------------------------------------------------------------------*
*      -->P_WA_ZNOM_PROG_REME_ALV  text
*----------------------------------------------------------------------*
FORM verifica_alterar  USING  p_znom_prog_reme_alv TYPE ty_zplac_nom_prog_reme p_valida TYPE sy-subrc.

  p_valida = 0.

  IF NOT p_znom_prog_reme_alv-docnum IS INITIAL.

    CALL FUNCTION 'Z_NFE_CTE_AUTORIZADO'
      EXPORTING
        p_docnum       = p_znom_prog_reme_alv-docnum
        p_uso          = 'N'
      EXCEPTIONS
        cancelado      = 1
        nao_cancelado  = 2
        pendente       = 3
        nao_concluido  = 4
        nao_existe     = 5
        autorizado_uso = 6
        denegado       = 7
        OTHERS         = 8.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      p_valida = 4.
    ENDIF.

  ENDIF.

ENDFORM.                    " VERIFICA_ALTERAR

*&---------------------------------------------------------------------*
*&      Form  LIMPA_ABA_04A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM limpa_aba_04a .
  CLEAR: wa_znom_prog_reme, wa_znom_prog_reme_alv, vg_qtd_vinc_conh,
         it_zdoc_nf_produtor[], it_zdoc_nf_produtor_alv[],
         it_znom_conhec_alv[], it_znom_conhec[],
         it_zdoc_rem_bl_alv[], it_zdoc_rem_bl[].
ENDFORM.                    " LIMPA_ABA_04A

*&---------------------------------------------------------------------*
*&      Module  STATUS_0045  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0045 OUTPUT.

  LOOP AT SCREEN.
    IF ( screen-name EQ 'VG_QTD_VINC_CONH' ).
      IF wa_znom_prog_reme IS INITIAL.
        screen-input  = 0.
        screen-output = 1.
      ELSE.
        screen-input  = 1.
        screen-output = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " STATUS_0045  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_CONHECIMENTOS_VOLUME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verifica_conhecimentos_volume  USING  p_vg_conhec TYPE sy-subrc.

  DATA: wa_zdoc_exp     TYPE zdoc_exp,
        it_zdoc_rem_aux TYPE TABLE OF zdoc_rem_bl WITH HEADER LINE,
        wa_zdoc_rem_aux TYPE zdoc_rem_bl,
        qtd_remessa     TYPE j_1bnetqty,
        qtd_conheci     TYPE j_1bnetqty.

  p_vg_conhec = 0. "4.

  SELECT SINGLE * INTO wa_zdoc_exp
    FROM zdoc_exp
   WHERE vbeln            = wa_znom_prog_reme_alv-id_remessa
     AND id_registro_expo = wa_znom_prog_reme_alv-id_registro_expo.

  IF sy-subrc IS INITIAL.
    "Conhecimentos Vinculados
    SELECT * INTO TABLE it_zdoc_rem_aux
      FROM zdoc_rem_bl
     WHERE id_doc_exp EQ wa_zdoc_exp-id_doc_exp.

    IF sy-subrc IS INITIAL.
      IF wa_znom_prog_reme_alv-gewei NE 'KG'.

        qtd_remessa = 0.
        qtd_conheci = 0.

        CALL FUNCTION 'ME_CONVERSION_MEINS'
          EXPORTING
            i_matnr             = wa_znom_prog_reme_alv-id_material
            i_mein1             = wa_znom_prog_reme_alv-gewei
            i_meins             = 'KG'
            i_menge             = wa_znom_prog_reme_alv-ntgew
          IMPORTING
            menge               = qtd_remessa
          EXCEPTIONS
            error_in_conversion = 1
            no_success          = 2
            OTHERS              = 3.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ELSE.
        qtd_remessa = wa_znom_prog_reme_alv-ntgew.
      ENDIF.

      LOOP AT it_zdoc_rem_aux INTO wa_zdoc_rem_aux.
        qtd_conheci = qtd_conheci + wa_zdoc_rem_aux-nr_qtde_vinc.
      ENDLOOP.

      IF qtd_conheci EQ qtd_remessa.
        p_vg_conhec = 0.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " VERIFICA_CONHECIMENTOS_VOLUME

*&---------------------------------------------------------------------*
*&      Form  ATUALIZAR_REMESAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM atualizar_remesas .
  PERFORM consulta_remessas USING abap_false.
ENDFORM.                    " ATUALIZAR_REMESAS

*&---------------------------------------------------------------------*
*&      Module  ZM_ESTADO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_estado INPUT.

ENDMODULE.                 " ZM_ESTADO  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUSCA_PRODUTORES_REMETENTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM busca_produtores_remetentes .

  DATA: wa_lips    TYPE lips,
        gs_cfop    TYPE zmemo_cfop,
        gt_cfop    LIKE TABLE OF gs_cfop,
        vg_item_ov TYPE vbap,
        saldo_nota TYPE j_1bnetqty,
        "IR218333 - 21.01.25 - PQ
*        lv_grp_ret TYPE c LENGTH 1.
        lv_grp_ret TYPE znom_reme_notas-grp_retorno.
  "IR218333 - 21.01.25 - PQ

  CLEAR: it_prod_remessa[].

  CALL FUNCTION 'Z_MEMO_CFOP_SAIDA'
    EXPORTING
      exp_propria = 'X'
    TABLES
      cfops       = gt_cfop.

  SELECT SINGLE * INTO wa_lips
    FROM lips
   WHERE vbeln EQ vg_remessa2-vbeln.

  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE * INTO vg_item_ov
    FROM vbap
   WHERE vbeln EQ wa_lips-vgbel
     AND posnr EQ wa_lips-vgpos.

  READ TABLE gt_cfop INTO gs_cfop WITH KEY low = vg_item_ov-j_1bcfop.

  "Ordem de venda não marcada para com compromisso de exportação

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*  CHECK sy-subrc IS INITIAL.
  IF sy-subrc IS INITIAL
  OR ok_code_0042 EQ ok_enter.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração


    "Vincula notas
    PERFORM consulta_remetentes.

    IF wa_lips-gewei NE 'KG'.
      CALL FUNCTION 'ME_CONVERSION_MEINS'
        EXPORTING
          i_matnr             = wa_lips-matnr
          i_mein1             = wa_lips-gewei
          i_meins             = 'KG'
          i_menge             = wa_lips-ntgew
        IMPORTING
          menge               = wa_lips-ntgew
        EXCEPTIONS
          error_in_conversion = 1
          no_success          = 2
          OTHERS              = 3.
      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

** Determinar de grupo de retorno da ordem
    SELECT SINGLE grp_retorno
      INTO lv_grp_ret
      FROM vbfa AS vb
     INNER JOIN znom_remetente AS re ON re~nr_ordem = vb~vbelv
     WHERE vb~vbeln       = wa_vinc_remessa-vbeln
      AND  re~id_empresa  = wa_filtro_remetente-empresa
      AND  re~id_filial   = wa_filtro_remetente-centro
      AND  re~id_material = wa_filtro_remetente-material
      AND  vb~vbtyp_n     = 'J'
      AND  vb~vbtyp_v     = 'C'.

    LOOP AT it_znom_remetente_alv INTO wa_znom_remetente_alv WHERE uf EQ wa_vinc_remessa-id_estado
                                                               AND nr_saldo_efetivar GT 0
                                                               AND grp_retorno EQ lv_grp_ret.

      MOVE-CORRESPONDING wa_znom_remetente_alv TO it_prod_remessa.

      saldo_nota = 0.

      LOOP AT it_znom_reme_notas_alv WHERE id_nomeacao_tran EQ wa_znom_remetente_alv-id_nomeacao_tran
                                       AND id_empresa       EQ wa_znom_remetente_alv-id_empresa
                                       AND id_filial        EQ wa_znom_remetente_alv-id_filial
                                       AND id_material      EQ wa_znom_remetente_alv-id_material
                                       AND id_remetente     EQ wa_znom_remetente_alv-id_remetente
                                       AND grp_retorno      EQ lv_grp_ret.

        saldo_nota = saldo_nota + it_znom_reme_notas_alv-nr_saldo_efetivar.
      ENDLOOP.

      it_prod_remessa-nr_qtd_possivel = saldo_nota.

      IF saldo_nota LE wa_lips-ntgew.
        it_prod_remessa-nr_qtd_vincular = saldo_nota.
        wa_lips-ntgew = wa_lips-ntgew - saldo_nota.
      ELSE.
        it_prod_remessa-nr_qtd_vincular = wa_lips-ntgew.
        wa_lips-ntgew = 0.
      ENDIF.
      APPEND it_prod_remessa.
    ENDLOOP.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
  ENDIF.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

ENDFORM.                    " BUSCA_PRODUTORES_REMETENTES

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_REMESSA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_alv_remessa OUTPUT.

  PERFORM plan_cria_remessa_alv.

ENDMODULE.                 " CRIA_ALV_REMESSA  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_REMESSA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plan_cria_remessa_alv .

  CONSTANTS: tabela_remetente_re TYPE string VALUE 'IT_PROD_REMESSA'.

  DATA: text_n000        TYPE c LENGTH 50 VALUE 'Remetente',
        text_n001        TYPE c LENGTH 50 VALUE 'Remetente',
        text_n002        TYPE c LENGTH 50 VALUE 'Qtd. Disponível',
        text_n003        TYPE c LENGTH 50 VALUE 'CNPJ/CPF',
        text_n004        TYPE c LENGTH 50 VALUE 'UF',
        text_n006        TYPE c LENGTH 50 VALUE 'Qtd. Vinculada',
        it_exclude_fcode TYPE ui_functions,
        wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

  IF plan_prim_remetente_re IS INITIAL.

    CREATE OBJECT plan_container_remetente_re
      EXPORTING
        container_name = 'CTN_REMETENTE'.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = plan_alv_remetente_re.

    CREATE OBJECT plan_alv_remetente_re
      EXPORTING
        i_parent = plan_container_remetente_re.

    CREATE OBJECT toolbar_prog_remessa_re
      EXPORTING
        io_alv_grid = plan_alv_remetente_re.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_remetente_re USING:
        tabela_remetente_re 'ICONE'           text_n000 'X'   01 03 space space space 'X'   space space             space,
        tabela_remetente_re 'NR_QTD_POSSIVEL' text_n002 space 02 15 space space space space space c_grid_color_c400 space,
        tabela_remetente_re 'NR_QTD_VINCULAR' text_n006 space 03 15 space space space space space c_grid_color_c300 'X',
        tabela_remetente_re 'NAME1'           text_n001 space 04 35 space space space space 'X'   space             space,
        tabela_remetente_re 'STCD1'           text_n003 space 05 18 space space space space 'X'   space             space,
        tabela_remetente_re 'UF'              text_n004 space 06 03 space space space space space space             space.
    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra      = c_x.
    plan_gs_layout-sel_mode   = space.
    plan_gs_layout-edit_mode  = c_x.

    wa_exclude_fcode = '&LOCAL&CUT'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&INSERT_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&MOVE_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&PASTE'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&PASTE_NEW_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&UNDO'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&VARI_ADMIN'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&APPEND'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&COPY'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&COPY_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&VLOTUS'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&AQW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    wa_exclude_fcode = '&PRINT'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&MB_SUM'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&AVERAGE'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&MB_VIEW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&MB_EXPORT'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&MB_FILTER'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&GRAPH'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&INFO'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&DELETE_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    SET HANDLER toolbar_prog_remessa_re->on_toolbar          FOR plan_alv_remetente_re.
    SET HANDLER toolbar_prog_remessa_re->handle_user_command FOR plan_alv_remetente_re.

    CALL METHOD plan_alv_remetente_re->set_table_for_first_display
      EXPORTING
        i_default            = space
        is_layout            = plan_gs_layout
        it_toolbar_excluding = it_exclude_fcode
      CHANGING
        it_fieldcatalog      = plan_catalogo_remetente_re
        it_outtab            = it_prod_remessa[].

*    call method plan_alv_remetente_re->register_edit_event
*      exporting
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    call method plan_alv_remetente_re->register_edit_event
*      exporting
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*    create object plan_event_remessa_re.
*    set handler plan_event_remessa_re->handle_data_changed_remessa_re for plan_alv_remetente_re.

    plan_prim_remetente_re = c_x.
  ENDIF.

  CALL METHOD plan_alv_remetente_re->refresh_table_display.

  CALL METHOD plan_alv_remetente_re->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.


ENDFORM.                    " PLAN_CRIA_REMESSA_ALV

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_PRODUTORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM excluir_produtores .

  DATA: it_selected_rows  TYPE lvc_t_row,
        wa_selected_rows  TYPE lvc_s_row,
        it_prod_remessa_c TYPE TABLE OF zplac_vinc_produtor WITH HEADER LINE.

  CLEAR: it_prod_remessa_c[].

  CALL METHOD plan_alv_remetente_re->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  MOVE it_prod_remessa[] TO it_prod_remessa_c[].

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_prod_remessa_c INDEX wa_selected_rows-index.
    IF sy-subrc IS INITIAL.
      DELETE it_prod_remessa INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
  CALL METHOD plan_alv_remetente_re->refresh_table_display.

ENDFORM.                    " EXCLUIR_PRODUTORES

*&---------------------------------------------------------------------*
*&      Form  VALIDA_PRODUTORES_REMETENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM valida_produtores_remetents  USING p_valida TYPE sy-subrc.
  DATA: wa_lips TYPE lips,
        vg_prod TYPE j_1bnetqty.

  p_valida = 0.

  IF NOT it_prod_remessa[] IS INITIAL.
    p_valida = 1.
    vg_prod  = 0.

    SELECT SINGLE * INTO wa_lips
      FROM lips
     WHERE vbeln EQ vg_remessa-vbeln.

    IF wa_lips-gewei NE 'KG'.
      CALL FUNCTION 'ME_CONVERSION_MEINS'
        EXPORTING
          i_matnr             = wa_lips-matnr
          i_mein1             = wa_lips-gewei
          i_meins             = 'KG'
          i_menge             = wa_lips-ntgew
        IMPORTING
          menge               = wa_lips-ntgew
        EXCEPTIONS
          error_in_conversion = 1
          no_success          = 2
          OTHERS              = 3.
      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    LOOP AT it_prod_remessa.
      vg_prod  = vg_prod + it_prod_remessa-nr_qtd_vincular.
      IF it_prod_remessa-nr_qtd_vincular GT it_prod_remessa-nr_qtd_possivel.
        p_valida = 2.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF NOT vg_prod NE wa_lips-ntgew.
      p_valida = 0.
    ENDIF.
  ENDIF.
ENDFORM.                    " VALIDA_PRODUTORES_REMETENTS

*&---------------------------------------------------------------------*
*&      Form  RECUSAR_REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM recusar_remessa .

  DATA: vg_remessa_add TYPE sy-subrc.

  PERFORM verifica_execucao_filial USING wa_znom_programacao-id_filial vg_remessa_add.
  IF vg_remessa_add IS INITIAL.
    PERFORM verifica_selecao_prog_reme USING vg_remessa_add.
    IF vg_remessa_add IS INITIAL.

      CALL FUNCTION 'ZPLANCOMP_REC_DEV_EXPORTACAO'
        EXPORTING
          p_docnum_exp       = wa_znom_prog_reme_alv-docnum
        EXCEPTIONS
          falta_docnum       = 1
          sem_fatura_ref     = 2
          direcao_nota       = 3
          tipo_nota          = 4
          fatura_estornada   = 5
          ordem_errada       = 6
          nota_fiscal_exp    = 7
          nao_planejada      = 8
          cancelado          = 9
          nota_fiscal_compro = 10
          OTHERS             = 11.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ELSE.
      MESSAGE s010.
    ENDIF.
  ELSE.
    MESSAGE s022 WITH wa_znom_programacao-id_filial.
  ENDIF.

ENDFORM.                    " RECUSAR_REMESSA

*&---------------------------------------------------------------------*
*&      Form  MEMORANDO_NOTA_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM memorando_nota_fiscal .
  DATA: vg_remessa_add TYPE sy-subrc.

  PERFORM verifica_execucao_filial USING wa_znom_programacao-id_filial vg_remessa_add.
  IF vg_remessa_add IS INITIAL.
    PERFORM verifica_selecao_prog_reme USING vg_remessa_add.
    IF vg_remessa_add IS INITIAL.
      PERFORM authority_check USING 'ZMEMO01' vg_remessa_add.
      IF vg_remessa_add IS INITIAL.

        CALL FUNCTION 'Z_MEMO_ADD_NFE'
          EXPORTING
            vg_docnum                = wa_znom_prog_reme_alv-docnum
            p_gravar                 = 'S'
          EXCEPTIONS
            docn_localizado          = 1
            docn_memorando           = 2
            docn_cancelado           = 3
            docn_cancelado_aut       = 4
            docn_neletronico         = 5
            docn_respres_legal       = 6
            docn_re_nao_encontrada   = 7
            docn_dde_nao_encontrada  = 8
            pais_nao_encontrado      = 9
            docn_dde_nao_encontrada2 = 10
            sem_saldo_comprovar      = 11
            OTHERS                   = 12.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE s010.
    ENDIF.
  ELSE.
    MESSAGE s022 WITH wa_znom_programacao-id_filial.
  ENDIF.


ENDFORM.                    " MEMORANDO_NOTA_FISCAL
*&---------------------------------------------------------------------*
*&      Module  BUSCA_REMESSA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_remessa INPUT.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_vbfa OCCURS 0,
          vbeln TYPE vbfa-vbeln,
          vbelv TYPE vbfa-vbelv,
          erdat TYPE vbfa-erdat,
        END OF tl_vbfa.

  SELECT DISTINCT *
    FROM vbfa
    INTO CORRESPONDING FIELDS OF TABLE tl_vbfa
   FOR ALL ENTRIES IN it_znom_remetente_alv
   WHERE vbelv = it_znom_remetente_alv-nr_ordem
    AND  vbtyp_n = 'J'
    AND  vbtyp_v = 'C'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VBELN'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'VBELN'
      value_org       = 'S'
    TABLES
      value_tab       = tl_vbfa
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " BUSCA_REMESSA  INPUT

*----------------------------------------------------------------------*
*  MODULE BUSCA_RE INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE busca_re INPUT.
  DATA: BEGIN OF tl_re OCCURS 0,
          nr_registro_expo TYPE zreg_exportacao-nr_registro_expo,
          nr_qtde          TYPE zreg_exportacao-nr_qtde,
        END OF tl_re,

        lv_re TYPE znom_transporte-id_nomeacao_tran.

  lv_re = wa_znom_transporte_alv-id_nomeacao_tran.
  SHIFT lv_re LEFT DELETING LEADING '0'.

  SELECT DISTINCT nr_registro_expo nr_qtde
    FROM zreg_exportacao
    INTO CORRESPONDING FIELDS OF TABLE tl_re
   WHERE id_nomeacao_tran = lv_re.

  SORT tl_re BY nr_registro_expo.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NR_REGISTRO_EXPO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'NR_REGISTRO_EXPO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_re
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " BUSCA_RE  INPUT

MODULE busca_due INPUT.

  DATA: lt_zsdt0170 TYPE TABLE OF ty_zplac_due_antecipada WITH HEADER LINE.

  DATA: BEGIN OF tl_due OCCURS 0,
          numero_due     TYPE zsdt0170-numero_due,
          tp_exportacao  TYPE zsdt0170-tp_exportacao,
          peso_liq_total TYPE zsdt0172-peso_liq_total,
        END OF tl_due.

  CLEAR: tl_due[].

  PERFORM f_get_due_remessa TABLES lt_zsdt0170
                             USING wa_znom_transporte_alv-id_nomeacao_tran
                                   wa_vinc_remessa-vbeln.
  LOOP AT lt_zsdt0170.
    CLEAR: tl_due.
    tl_due-numero_due         = lt_zsdt0170-numero_due.
    tl_due-tp_exportacao      = lt_zsdt0170-tp_exportacao.
    tl_due-peso_liq_total     = lt_zsdt0170-peso_liq_total.
    APPEND tl_due.
  ENDLOOP.

  SORT tl_due BY numero_due.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NUMERO_DUE'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'NUMERO_DUE'
      value_org       = 'S'
    TABLES
      value_tab       = tl_due
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " BUSCA_RE  INPUT


MODULE busca_due_antecipada INPUT.

  CLEAR: tl_due[], lt_zsdt0170[].

  "Selecionar o NCM do material.
  SELECT SINGLE steuc FROM marc INTO wa_znom_programacao-ncm
  WHERE matnr EQ wa_znom_programacao-id_material
  AND werks EQ wa_znom_programacao-id_filial.

  SELECT *
    FROM zsdt0170 AS a INTO CORRESPONDING FIELDS OF TABLE lt_zsdt0170
   WHERE a~id_nomeacao_tran  EQ wa_znom_programacao-id_nomeacao_tran
     AND tp_due            EQ '1' "Sem NF-e
     AND id_due_ref        EQ 0
     AND status            EQ '1'
     AND bloqueio_interno  EQ abap_false
     AND bukrs             EQ wa_znom_programacao-id_empresa
     AND EXISTS ( SELECT id_due
                    FROM zsdt0172 AS b
                   WHERE b~id_due = a~id_due
                      AND b~codigo_ncm = wa_znom_programacao-ncm ).
*                     AND b~matnr  = wa_znom_programacao-id_material ). "Ajuste realizado 11/06/2021


  LOOP AT lt_zsdt0170.
    CLEAR: tl_due.
    tl_due-numero_due         = lt_zsdt0170-numero_due.
    tl_due-tp_exportacao      = lt_zsdt0170-tp_exportacao.

    SELECT SUM( peso_liq_total )
      FROM zsdt0172 INTO tl_due-peso_liq_total
     WHERE id_due = lt_zsdt0170-id_due.

    APPEND tl_due.
  ENDLOOP.

  SORT tl_due BY numero_due.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NUMERO_DUE'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'NUMERO_DUE'
      value_org       = 'S'
    TABLES
      value_tab       = tl_due
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " BUSCA_RE  INPUT

FORM f_valida_qtde_exp  CHANGING p_validou.

  DATA: v_peso_liq_total TYPE zsdt0172-peso_liq_total,
        v_msg            TYPE string.

  CLEAR: p_validou.



  LOOP AT it_due_ret_conf_alv INTO wa_due_ret_conf_alv.

    CLEAR: v_peso_liq_total.

    "Selecionar o NCM do material.
    SELECT SINGLE steuc FROM marc INTO wa_due_ret_conf_alv-ncm
    WHERE matnr EQ wa_znom_programacao-id_material
    AND werks EQ wa_znom_programacao-id_filial.

    LOOP AT it_due_retificacao_alv INTO wa_due_retificacao_alv WHERE id_nomeacao_tran   = wa_due_ret_conf_alv-id_nomeacao_tran
                                                                 AND bukrs              = wa_due_ret_conf_alv-bukrs
                                                                 AND codigo_ra_embarque = wa_due_ret_conf_alv-codigo_ra_embarque.
*                                                                 AND matnr              = wa_due_ret_conf_alv-matnr.
      "Selecionar o NCM do material.
      SELECT SINGLE steuc FROM marc INTO wa_due_ret_conf_alv-ncm
      WHERE matnr EQ wa_znom_programacao-id_material
      AND werks EQ wa_znom_programacao-id_filial.

      IF  wa_due_ret_conf_alv-ncm EQ wa_due_ret_conf_alv-ncm.
        ADD wa_due_retificacao_alv-peso_liq_tot_ret TO v_peso_liq_total.
      ENDIF.
    ENDLOOP.

    IF v_peso_liq_total <> wa_due_ret_conf_alv-peso_liq_total.

      v_msg = 'Quantidade para a Empresa: ' && wa_due_ret_conf_alv-bukrs &&
              ', R.A Embarque: '            && wa_due_ret_conf_alv-codigo_ra_embarque &&
              ', Material: '                && wa_due_ret_conf_alv-matnr    &&
              ', não confere com a Qtde. à Retificar: ' && v_peso_liq_total && '!'.

      MESSAGE v_msg TYPE 'I'.
      RETURN.
    ENDIF.
  ENDLOOP.

  p_validou = abap_true.
  MESSAGE 'Quantidades de Exportação validadas com sucesso!' TYPE 'S'.


ENDFORM.

FORM f_montar_layout_nf_exp_due.
  REFRESH estrutura.
  PERFORM f_montar_estrutura USING:

     01  ''   ''            'IT_DUE_NF_EXP_ALV' 'BRANCH'  'Filial'      '06' '',
     02  ''   ''            'IT_DUE_NF_EXP_ALV' 'DOCNUM'  'Docnum'      '10' '',
     03  ''   ''            'IT_DUE_NF_EXP_ALV' 'NFENUM'  'Número'      '10' '',
     04  ''   ''            'IT_DUE_NF_EXP_ALV' 'MENGE'   'Quantidade'  '13' '',
     05  ''   ''            'IT_DUE_NF_EXP_ALV' 'MEINS'   'UM'          '05' '',
     06  ''   ''            'IT_DUE_NF_EXP_ALV' 'NETWR'   'Valor'       '13' ''.

ENDFORM.                    " MONTAR_LAYOUT


FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit).

  CLEAR: wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.


  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
FORM modify_doc_nf_produtor TABLES i_t_doc_nf_produtor_mod STRUCTURE zdoc_nf_produtor.

  DATA:
    lv_qtd_registro    TYPE i,
    lv_total_registros TYPE i,
    lv_qtd_limite_mod  TYPE i VALUE 500,
    lt_nf_prod_mod     TYPE TABLE OF zdoc_nf_produtor.

  lv_total_registros = lines( i_t_doc_nf_produtor_mod ).

  LOOP AT i_t_doc_nf_produtor_mod INTO DATA(ls_nf_prod).

    APPEND ls_nf_prod TO lt_nf_prod_mod.
    lv_qtd_registro = lv_qtd_registro + 1.

    IF lv_qtd_registro EQ lv_qtd_limite_mod
    OR lv_qtd_registro EQ lv_total_registros.

      MODIFY zdoc_nf_produtor FROM TABLE lt_nf_prod_mod.

      IF sy-subrc = 0.

        COMMIT WORK AND WAIT.
        WAIT UP TO 1 SECONDS.

        CLEAR lt_nf_prod_mod.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

MODULE busca_tipo INPUT.

  DATA: BEGIN OF tl_tipo OCCURS 0,
          auart TYPE vbak-auart,
        END OF tl_tipo.

*  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
*        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.
  FREE: tl_tipo[].
  CLEAR: tl_return_tab, tl_dselc,tl_tipo.

*** Inicio - Rubenilson Pereira - 08.10.25 #192273
  IF wa_filtro_remetente-fins_espec IS NOT INITIAL.
    tl_tipo-auart = 'ZEXI'.
    APPEND tl_tipo TO tl_tipo.
  ELSE.
    tl_tipo-auart = 'ZEXI'.
    APPEND tl_tipo TO tl_tipo.
    tl_tipo-auart = 'ZEXP'.
    APPEND tl_tipo TO tl_tipo.
    tl_tipo-auart = 'ZEXD'. "80543 -LP
    APPEND tl_tipo TO tl_tipo.
  ENDIF.
*** Fim - Rubenilson Pereira - 08.10.25 #192273

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'AUART'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'AUART'
      value_org       = 'S'
    TABLES
      value_tab       = tl_tipo
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_DEPOSITO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_deposito INPUT.

*** Inicio - Rubenilson Pereira - 09.10.2025 #192273
  TYPES: BEGIN OF ty_depo,
           werks   TYPE werks_ext,
           name1   TYPE name1,
           lgort   TYPE lgort_d,
           lgort_t TYPE lgort_d,
         END OF ty_depo.
*** Fim - Rubenilson Pereira - 09.10.2025 #192273

  DATA: BEGIN OF tl_depst OCCURS 0,
          werks TYPE werks_ext,
**<<<------"173808 - NMS - INI------>>>
          name1 TYPE name1,
          lgort TYPE lgort_d,
**<<<------"173808 - NMS - FIM------>>>
        END OF tl_depst.

  DATA: lt_depositos TYPE TABLE OF ty_depo." Rubenilson Pereira - 09.10.2025 #192273

*  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
*        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.
  FREE: tl_depst[].
  CLEAR: tl_return_tab, tl_dselc,tl_depst.

  IF wa_filtro_remetente-cvirt IS NOT INITIAL.
**<<<------"173808 - NMS - INI------>>>
*    SELECT DISTINCT lgort FROM mard_lgort
*      WHERE werks = @wa_filtro_remetente-cvirt INTO TABLE @tl_depst[].
    SELECT werks_v name1 lgort lgort_t
      FROM zsdt_depara_depo AS a
      INNER JOIN zsdt0168 AS b
       ON a~lifnr EQ b~lifnr
      INNER JOIN t001w AS c
       ON a~werks_v EQ c~werks
      INTO TABLE lt_depositos
    WHERE a~werks     EQ wa_filtro_remetente-centro
      AND a~operacao  EQ 'RF'
      AND a~eudr      EQ wa_filtro_remetente-due_eudr
      AND b~codigo_ra EQ wa_filtro_remetente-codigo_ra_embarque.
    IF sy-subrc IS INITIAL.
*** Inicio - Rubenilson Pereira - 09.10.2025 #192273
      LOOP AT lt_depositos ASSIGNING FIELD-SYMBOL(<fs_depositos>).
        APPEND INITIAL LINE TO tl_depst ASSIGNING FIELD-SYMBOL(<fs_desp>).
        <fs_desp>-werks = <fs_depositos>-werks.
        <fs_desp>-name1 = <fs_depositos>-name1.
        <fs_desp>-lgort = <fs_depositos>-lgort.

        IF <fs_depositos>-lgort_t IS NOT INITIAL AND
           <fs_depositos>-lgort_t <> <fs_depositos>-lgort.
          APPEND INITIAL LINE TO tl_depst ASSIGNING <fs_desp>.
          <fs_desp>-werks = <fs_depositos>-werks.
          <fs_desp>-name1 = <fs_depositos>-name1.
          <fs_desp>-lgort = <fs_depositos>-lgort_t.
        ENDIF.

      ENDLOOP.
    ENDIF.
*** Fim - Rubenilson Pereira - 09.10.2025 #192273
**<<<------"173808 - NMS - FIM------>>>
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
**<<<------"173808 - NMS - INI------>>>
*       retfield        = 'WERKS'
        retfield        = 'LGORT'
**<<<------"173808 - NMS - FIM------>>>
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
**<<<------"173808 - NMS - INI------>>>
*       dynprofield     = 'werks_ext'
        dynprofield     = 'lgort_d'
**<<<------"173808 - NMS - FIM------>>>
        value_org       = 'S'
      TABLES
        value_tab       = tl_depst
        return_tab      = tl_return_tab
        dynpfld_mapping = tl_dselc.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode = 'ENTER'.

  ELSE.
    CLEAR lv_msg.
    lv_msg = |Favor seleciona primeiro o centro!|.
    MESSAGE lv_msg TYPE 'I'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_CVIRT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_cvirt INPUT.

  IF NOT wa_filtro_remetente-cvirt IS INITIAL.

    SELECT SINGLE name1
           FROM t001w
           INTO @DATA(lv_name1)
           WHERE werks EQ @wa_filtro_remetente-cvirt.
    IF sy-subrc EQ 0.
      wa_filtro_remetente-desc_cvirt = lv_name1.
    ENDIF.

  ENDIF.


ENDMODULE.
