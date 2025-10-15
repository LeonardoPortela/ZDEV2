*----------------------------------------------------------------------*
***INCLUDE MZPLANCOMP_0003 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_REMETENTE  OUTPUT
*&---------------------------------------------------------------------*
MODULE cria_alv_remetente OUTPUT.

  PERFORM plan_cria_remetente_alv.

ENDMODULE.                 " CRIA_ALV_REMETENTE  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_REMETENTE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plan_cria_remetente_alv .

  CONSTANTS: tabela_remetente TYPE string VALUE 'IT_ZNOM_REMETENTE_ALV'.

  DATA: text_n000 TYPE c LENGTH 50 VALUE 'Remetente',
        text_n001 TYPE c LENGTH 50 VALUE 'Remetente',
        text_n002 TYPE c LENGTH 50 VALUE 'Qtd. Compromisso',
        text_n003 TYPE c LENGTH 50 VALUE 'CNPJ/CPF',
        text_n004 TYPE c LENGTH 50 VALUE 'UF',
        text_n005 TYPE c LENGTH 50 VALUE 'Município',
        text_n006 TYPE c LENGTH 50 VALUE 'Qtd. Efetivada',
        text_n008 TYPE c LENGTH 50 VALUE 'Qtd. Rec./Dev.',
        text_n007 TYPE c LENGTH 50 VALUE 'Qtd. Efetivar',
        text_n009 TYPE c LENGTH 50 VALUE 'DocNum Retorno',
        text_n010 TYPE c LENGTH 50 VALUE 'OV. Exportação',
        text_n011 TYPE c LENGTH 50 VALUE 'Grp.',
        text_n012 TYPE c LENGTH 50 VALUE 'Tipo de OV', "113637 - CS2023000378 - PSA
        text_n013 TYPE c LENGTH 50 VALUE 'Preço', "113637 - CS2023000378 - PSA
        text_n014 TYPE c LENGTH 50 VALUE 'Deposito', "113637 - CS2023000378 - PSA
        text_n015 TYPE c LENGTH 50 VALUE 'Safra', "113637 - CS2023000378 - PSA
        text_n016 TYPE c LENGTH 50 VALUE 'Centro Virtual', "113637 - CS2023000378 - PSA
        text_n017 TYPE c LENGTH 50 VALUE 'Retorno Com NF Terceiro', "<<<------"145379 - NMS - INI------>>>
        text_n018 TYPE c LENGTH 50 VALUE 'EUDR'.                    "<<<------"165835 - NMS - INI------>>>

  "it_exclude_fcode type ui_functions,
  "wa_exclude_fcode like line of it_exclude_fcode.

  IF plan_prim_remetente IS INITIAL.

    CREATE OBJECT plan_container_remetente
      EXPORTING
        container_name = 'CTN_REMETENTES'.

    CREATE OBJECT plan_alv_remetente
      EXPORTING
        i_parent = plan_container_remetente.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_remetente USING:
          tabela_remetente 'GRP_RETORNO'        text_n011 space 01 03 space space space space space space             space,
          tabela_remetente 'ICONE'              text_n000 'X'   02 03 space space space 'X'   space space             space,
          tabela_remetente 'NAME1'              text_n001 space 03 33 space space space space 'X'   space             space,
          tabela_remetente 'STCD1'              text_n003 space 04 17 space space space space 'X'   space             space,
          tabela_remetente 'UF'                 text_n004 space 05 03 space space space space space space             space,
          tabela_remetente 'MUNIC'              text_n005 space 06 20 space space space space space space             space,
          tabela_remetente 'NR_REMETENTE2'      text_n002 space 07 15 space space 'X'   space space c_grid_color_c400 space,
          tabela_remetente 'NR_EFETIVADA'       text_n006 space 08 15 space space 'X'   space space c_grid_color_c600 space,
          tabela_remetente 'NR_RECUSADO'        text_n008 space 09 15 space space 'X'   space space c_grid_color_recu space,
          tabela_remetente 'NR_SALDO_EFETIVAR'  text_n007 space 10 15 space space 'X'   space space c_grid_color_c500 space,
          tabela_remetente 'DOCNUM_RT'          text_n009 space 11 10 space space space space space space             space,
          tabela_remetente 'NR_ORDEM'           text_n010 space 12 10 space space space space space space             space,
          tabela_remetente 'tipov'              text_n012 space 12 10 space space space space space space             space, "113637 - CS2023000378 - PSA
          tabela_remetente 'preco'              text_n013 space 12 10 space space space space space space             space, "113637 - CS2023000378 - PSA
          tabela_remetente 'depst'              text_n014 space 12 10 space space space space space space             space, "113637 - CS2023000378 - PSA
          tabela_remetente 'safra'              text_n015 space 12 10 space space space space space space             space, "113637 - CS2023000378 - PSA
          tabela_remetente 'cvirt'              text_n016 space 12 10 space space space space space space             space, "113637 - CS2023000378 - PSA
          tabela_remetente 'RETORNO_COM_NF_DE_TERCEIRO' text_n017 ' ' 13 07 space space   space space space space space,     "<<<------"145379 - NMS------>>>
          tabela_remetente 'EUDR'               text_n018 space 14 05 space space space space space space             space. "**<<<------"165835 - NMS - FIM------>>>
    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra    = c_x.
    plan_gs_layout-sel_mode = c_a.

    CLEAR: wa_sort, it_sort[].
    wa_sort-spos      = 1.
    wa_sort-fieldname = 'GRP_RETORNO'.
    wa_sort-up        = 'X'.
    wa_sort-subtot    = space.
    wa_sort-expa      = space.
    APPEND wa_sort TO it_sort.

    CALL METHOD plan_alv_remetente->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = plan_gs_layout
      CHANGING
        it_fieldcatalog = plan_catalogo_remetente
        it_outtab       = it_znom_remetente_alv[]
        it_sort         = it_sort[].

    CREATE OBJECT plan_alv_remet.
    SET HANDLER: plan_alv_remet->on_double_click FOR plan_alv_remetente.

    plan_prim_remetente = c_x.
  ENDIF.

  CALL METHOD plan_alv_remetente->refresh_table_display.

  CALL METHOD plan_alv_remetente->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_REMETENTE_ALV

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0033  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0033 INPUT.
  CASE ok_code_0001.
    WHEN ok_btpsqn.
      CLEAR: ok_code_0001.
      IF wa_filtro_remetente-data_ini GT wa_filtro_remetente-data_fim.
        MESSAGE 'A Dt. Inicio deve ser menor que a Dt. Final!' TYPE 'S'.
        EXIT.
      ENDIF.
      PERFORM consulta_nota_fiscal_disp.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0033  INPUT


*&---------------------------------------------------------------------*
*&      Module  STATUS_0033  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0033 OUTPUT.

  CLEAR: wa_filtro_remetente-ds_centro,
         wa_filtro_remetente-ds_remetente,
         wa_filtro_remetente-regio_due,
         wa_filtro_remetente-id_due,
         wa_filtro_remetente-codigo_urf_embarque,
         wa_filtro_remetente-codigo_ra_embarque.


  IF NOT wa_filtro_remetente-centro IS INITIAL.
    SELECT SINGLE name INTO wa_filtro_remetente-ds_centro
      FROM j_1bbranch
     WHERE bukrs  EQ wa_filtro_remetente-empresa
       AND branch EQ wa_filtro_remetente-centro.
  ENDIF.

  IF NOT wa_filtro_remetente-remetente IS INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_filtro_remetente-remetente
      IMPORTING
        output = wa_filtro_remetente-remetente.

    SELECT SINGLE name1 INTO wa_filtro_remetente-ds_remetente
      FROM lfa1
     WHERE lifnr EQ wa_filtro_remetente-remetente.
  ENDIF.

  IF wa_filtro_remetente-numero_due IS NOT INITIAL.
    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_0170)
     WHERE numero_due   EQ @wa_filtro_remetente-numero_due
       AND id_due_ref   EQ 0
       AND tp_due       EQ '1'. "Sem NF-e

    IF sy-subrc NE 0.
      CLEAR: wa_filtro_remetente-numero_due.
      MESSAGE 'DU-e informada não existe!' TYPE 'I'.
**<<<------"145379 - NMS - INI------>>>
*      EXIT.
      DATA(lv_exit) = abap_on.
**<<<------"145379 - NMS - FIM------>>>
    ENDIF.
**<<<------"145379 - NMS - INI------>>>
    IF lv_exit IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
      SELECT SINGLE *
        FROM zsdt0170 INTO _wl_0170
       WHERE numero_due       EQ wa_filtro_remetente-numero_due
         AND id_due_ref       EQ 0
         AND id_nomeacao_tran EQ wa_znom_transporte-id_nomeacao_tran.

      IF sy-subrc NE 0.
        CLEAR: wa_filtro_remetente-numero_due.
        MESSAGE 'DU-e informada não pertence a Nomeação selecionada!' TYPE 'I'.
**<<<------"145379 - NMS - INI------>>>
*      EXIT.
        lv_exit = abap_on.
**<<<------"145379 - NMS - FIM------>>>
      ENDIF.
**<<<------"145379 - NMS - INI------>>>
    ENDIF.

    IF lv_exit IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
      IF _wl_0170-status NE '1'.
        CLEAR: wa_filtro_remetente-numero_due.
        MESSAGE 'DU-e informada não foi registrada no Portal Siscomex!' TYPE 'I'.
**<<<------"145379 - NMS - INI------>>>
*      EXIT.
        lv_exit = abap_on.
**<<<------"145379 - NMS - FIM------>>>
      ENDIF.
**<<<------"145379 - NMS - INI------>>>
    ENDIF.

    IF lv_exit IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
      IF _wl_0170-bloqueio_interno EQ abap_true.
        CLEAR: wa_filtro_remetente-numero_due.
        MESSAGE 'DU-e informada possui um bloqueio interno!' TYPE 'I'.
**<<<------"145379 - NMS - INI------>>>
*      EXIT.
        lv_exit = abap_on.
**<<<------"145379 - NMS - FIM------>>>
      ENDIF.
**<<<------"145379 - NMS - INI------>>>
    ENDIF.
**<<<------"145379 - NMS - FIM------>>>
*    IF ( _WL_0170-REGIO IS INITIAL ) AND ( _WL_0170-TP_EXPORTACAO NE 'D' ).
*      CLEAR: WA_FILTRO_REMETENTE-NUMERO_DUE.
*      MESSAGE 'Região da DU-e não encontrada!' TYPE 'I'.
*      EXIT.
*    ENDIF.
**<<<------"145379 - NMS - INI------>>>
    IF lv_exit IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
      IF _wl_0170-codigo_ra_embarque IS INITIAL.
        CLEAR: wa_filtro_remetente-numero_due.
        MESSAGE 'R.A de Embarque da DU-e não encontrado!' TYPE 'I'.
**<<<------"145379 - NMS - INI------>>>
*      EXIT.
        lv_exit = abap_on.
**<<<------"145379 - NMS - FIM------>>>
      ENDIF.
**<<<------"145379 - NMS - INI------>>>
    ENDIF.

    IF lv_exit IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
      IF _wl_0170-lib_leitura_opus IS INITIAL.
        CLEAR: wa_filtro_remetente-numero_due.
        MESSAGE 'Registro DU-e não foi liberado para Leitura do Comex!' TYPE 'I'.
**<<<------"145379 - NMS - INI------>>>
*      EXIT.
        lv_exit = abap_on.
**<<<------"145379 - NMS - FIM------>>>
      ENDIF.
**<<<------"145379 - NMS - INI------>>>
    ENDIF.

    IF lv_exit IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
      "================================================================CS2021000431 - 11/06/2021 - AOENNING
      "Selecionar o NCM do material.
      SELECT SINGLE steuc FROM marc INTO wa_znom_programacao-ncm
      WHERE matnr EQ wa_znom_programacao-id_material
      AND werks EQ wa_znom_programacao-id_filial.
*    wa_znom_programacao-ncm = |{ wa_znom_programacao-ncm(4) }{ wa_znom_programacao-ncm+5(2) }{ wa_znom_programacao-ncm+8(2) }|.


      SELECT SINGLE *
        FROM zsdt0172 INTO @DATA(_wl_0172)
       WHERE id_due = @_wl_0170-id_due
          AND codigo_ncm EQ @wa_znom_programacao-ncm
*       AND matnr  = @wa_znom_programacao-id_material.
          AND EXISTS ( SELECT *
                      FROM zsdt0170 AS b
                     WHERE b~bukrs = @wa_znom_programacao-id_empresa ).

      "==============================================================================
      IF sy-subrc NE 0.
        CLEAR: wa_filtro_remetente-numero_due.
        MESSAGE 'NCM do Material DU-e diferente do NCM do Material da programação!!' TYPE 'I'.
**<<<------"145379 - NMS - INI------>>>
*      EXIT.
        lv_exit = abap_on.
**<<<------"145379 - NMS - FIM------>>>
      ENDIF.
**<<<------"145379 - NMS - INI------>>>
      IF lv_exit IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
        wa_filtro_remetente-id_due              = _wl_0170-id_due.
        wa_filtro_remetente-regio_due           = _wl_0170-regio.
        wa_filtro_remetente-codigo_urf_embarque = _wl_0170-codigo_urf_embarque.
        wa_filtro_remetente-codigo_ra_embarque  = _wl_0170-codigo_ra_embarque.
        wa_filtro_remetente-codigo_ncm          = _wl_0172-codigo_ncm.
        wa_filtro_remetente-ue_exportada        = _wl_0172-ue_exportada.
        wa_filtro_remetente-peso_liq_due        = _wl_0172-peso_liq_total.
        wa_filtro_remetente-due_eudr            = _wl_0170-eudr. "WPP 23102024 - US-153330 --->>>

        REPLACE ALL OCCURRENCES OF '.' IN  wa_filtro_remetente-codigo_ncm WITH space.
        CONDENSE wa_filtro_remetente-codigo_ncm NO-GAPS.

        TRANSLATE wa_filtro_remetente-ue_exportada TO UPPER CASE.
**<<<------"145379 - NMS - INI------>>>
      ENDIF.

    ENDIF.
**<<<------"145379 - NMS - FIM------>>>
  ENDIF.

*WBARBOSA 12112024 US-156375
*  IF wa_filtro_remetente-tp_vinc2 IS NOT INITIAL.
*    wa_filtro_remetente-fins_espec = abap_true.
*    CLEAR wa_filtro_remetente-comerc.
*  ENDIF.
*WBARBOSA 12112024 US-156375

  LOOP AT SCREEN.

*    wbarbosa 06112024 US-153330
    IF screen-name EQ 'WA_FILTRO_REMETENTE-RETORNAR_EUDR'.
      CASE wa_filtro_remetente-due_eudr.
        WHEN 'S'.
          screen-invisible = '1'.
          CLEAR: wa_filtro_remetente-retornar_eudr.
        WHEN 'N'.
          screen-invisible = '0'.
        WHEN OTHERS.
          screen-invisible = '1'.
          CLEAR: wa_filtro_remetente-retornar_eudr.
      ENDCASE.
    ENDIF.
*    wbarbosa 06112024 US-153330

    CASE abap_true.
      WHEN wa_filtro_remetente-tp_vinc1.
        IF screen-group1 EQ 'CCT'.
          screen-input = '0'.
        ENDIF.
      WHEN wa_filtro_remetente-tp_vinc2.
        IF screen-group1 EQ 'CCT'.
          screen-input = '1'.
        ENDIF.

* "// wbarbosa 07112024 - US-156375 item 04 HABILITAR FUNCIONALIDADES
* "// Habilitar para ambos os tipos "Vinc.c/.RFL" e "Sem Vinc."
*        IF screen-group1 EQ 'FEC'.
*          screen-input = '0'.
*        ENDIF.
* "// wbarbosa 07112024 - US-156375 item 04 HABILITAR FUNCIONALIDADES

    ENDCASE.

    IF screen-group1 EQ 'RFL'.
      IF ( wa_filtro_remetente-tp_vinc1 EQ abap_true ) OR ( wa_filtro_remetente-s_cct EQ abap_false ).
        screen-input = '0'.
      ELSE.
        screen-input = '1'.
      ENDIF.
    ENDIF.

**<<<------"145379 - NMS - INI------>>>
* Habilita o campo Retorno será gerada com NF de RFL de Terceiro.
    IF screen-group1 EQ 'NFT'.


      IF wa_filtro_remetente-tp_vinc1 IS INITIAL.
        screen-input = '0'.
        CLEAR znom_remetente-retorno_com_nf_de_terceiro.
      ELSE.
        screen-input = '1'.
      ENDIF.

    ENDIF.
**<<<------"145379 - NMS - FIM------>>>
    MODIFY SCREEN.
  ENDLOOP.
**<<<------"145379 - NMS - INI------>>>
  IF lv_exit IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
    IF ( wa_filtro_remetente-tp_vinc1 EQ abap_true ) OR ( wa_filtro_remetente-s_cct EQ abap_false ).
      CLEAR: wa_filtro_remetente-show_rfl, wa_filtro_remetente-ck_cct_cp.
    ENDIF.

    CLEAR wa_znom_remetente_dados.
    IF NOT wa_znom_programacao_alv-id_nomeacao_tran IS INITIAL AND
       NOT wa_znom_programacao_alv-id_empresa       IS INITIAL AND
       NOT wa_znom_programacao_alv-id_filial        IS INITIAL AND
       NOT wa_znom_programacao_alv-id_material      IS INITIAL AND
       NOT wa_filtro_remetente-grp_retorno          IS INITIAL.
      SELECT * UP TO 1 ROWS
             FROM znom_remetente
             INTO wa_znom_remetente_dados
             WHERE id_nomeacao_tran EQ wa_znom_programacao_alv-id_nomeacao_tran
             AND   id_empresa       EQ wa_znom_programacao_alv-id_empresa
             AND   id_filial        EQ wa_znom_programacao_alv-id_filial
             AND   id_material      EQ wa_znom_programacao_alv-id_material
             AND   grp_retorno      EQ wa_filtro_remetente-grp_retorno.
      ENDSELECT.
    ENDIF.
    IF NOT wa_znom_remetente_dados-tipov IS INITIAL AND
       NOT wa_znom_remetente_dados-depst IS INITIAL AND
       NOT wa_znom_remetente_dados-safra IS INITIAL AND
       NOT wa_znom_remetente_dados-cvirt IS INITIAL.

      wa_filtro_remetente-tipov = wa_znom_remetente_dados-tipov.
      wa_filtro_remetente-preco = wa_znom_remetente_dados-preco.
      wa_filtro_remetente-depst = wa_znom_remetente_dados-depst.
      wa_filtro_remetente-safra = wa_znom_remetente_dados-safra.
      wa_filtro_remetente-cvirt = wa_znom_remetente_dados-cvirt.
**<<<------"145379 - NMS - INI------>>>
      znom_remetente-retorno_com_nf_de_terceiro = wa_znom_remetente_dados-retorno_com_nf_de_terceiro.
**<<<------"145379 - NMS - FIM------>>>
      SELECT SINGLE name1
            FROM t001w
            INTO @DATA(lv_name1_desc)
            WHERE werks EQ @wa_filtro_remetente-cvirt.
      IF sy-subrc EQ 0.
        wa_filtro_remetente-desc_cvirt = lv_name1_desc.
      ENDIF.

      LOOP AT SCREEN.
**<<<------"145379 - NMS - INI------>>>
*        IF screen-group1 EQ 'GR5'.
        IF screen-group1 EQ 'GR5' OR
           screen-group1 EQ 'NFT'.
**<<<------"145379 - NMS - FIM------>>>
          screen-input = '0'.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
    ELSE.

*** Inicio - Rubenilson Pereira - 13.10.25 #192273
      IF wa_filtro_remetente-fins_espec IS NOT INITIAL AND
         wa_filtro_remetente-tipov IS NOT INITIAL AND
         wa_filtro_remetente-tipov  <> 'ZEXI'.
        MESSAGE 'Para Fins Esp. apenas o Tipo Ov. ZEXI é permitido' TYPE 'S' DISPLAY LIKE 'E'.
        CLEAR wa_filtro_remetente-tipov.
        RETURN.
      ENDIF.
*** Fim - Rubenilson Pereira - 13.10.25 #192273

**<<<------"165835 - NMS - INI------>>>
* Carregamento automático do Centro Virtual e Depósito.
**<<<------"173808 - NMS - INI------>>>
      SELECT werks_v, lgort, name1, lgort_t" Rubenilson Pereira - 10.10.25 #192273
        FROM zsdt_depara_depo AS a
        INNER JOIN zsdt0168 AS b
         ON a~lifnr EQ b~lifnr
        INNER JOIN t001w AS c
         ON a~werks_v EQ c~werks
        INTO TABLE @DATA(tl_werks_lgort)
      WHERE a~werks     EQ @wa_filtro_remetente-centro
        AND a~operacao  EQ 'RF'
        AND a~eudr      EQ @wa_filtro_remetente-due_eudr
        AND b~codigo_ra EQ @wa_filtro_remetente-codigo_ra_embarque.

      IF sy-subrc IS INITIAL.
        IF lines( tl_werks_lgort ) EQ 1 .

          READ TABLE tl_werks_lgort INTO DATA(el_werks_lgort) INDEX 1.
          IF el_werks_lgort-lgort = el_werks_lgort-lgort_t OR el_werks_lgort-lgort_t IS INITIAL. " Rubenilson Pereira - 10.10.25 #192273
            wa_filtro_remetente-cvirt      = el_werks_lgort-werks_v.
            wa_filtro_remetente-desc_cvirt = el_werks_lgort-name1.
            wa_filtro_remetente-depst      = el_werks_lgort-lgort.
          ELSE." Rubenilson Pereira - 10.10.25 #192273
            wa_filtro_remetente-cvirt      = el_werks_lgort-werks_v." Rubenilson Pereira - 10.10.25 #192273
            wa_filtro_remetente-desc_cvirt = el_werks_lgort-name1." Rubenilson Pereira - 10.10.25 #192273
            DATA(lv_lgort_diferente) = abap_true.
          ENDIF." Rubenilson Pereira - 10.10.25 #192273

        ELSEIF lines( tl_werks_lgort ) EQ 0.
          CLEAR: wa_filtro_remetente-cvirt, wa_filtro_remetente-depst, wa_filtro_remetente-desc_cvirt.

        ELSE.
          DATA(vl_qtlin) = lines( tl_werks_lgort ).
          IF wa_filtro_remetente-depst IS INITIAL.
            READ TABLE tl_werks_lgort INTO el_werks_lgort INDEX 1.

          ELSE.
            READ TABLE tl_werks_lgort INTO el_werks_lgort WITH KEY lgort = wa_filtro_remetente-depst.

            IF NOT sy-subrc IS INITIAL.
              MESSAGE |Deposito { wa_filtro_remetente-depst } não existe para o Centro { wa_filtro_remetente-cvirt }.|  TYPE 'S' DISPLAY LIKE 'E'.
              DATA(vl_err_lgort) = abap_on.

            ENDIF.

          ENDIF.

          wa_filtro_remetente-cvirt      = el_werks_lgort-werks_v.
          wa_filtro_remetente-desc_cvirt = el_werks_lgort-name1.

        ENDIF.

      ENDIF.
**<<<------"173808 - NMS - FIM------>>>
**<<<------"165835 - NMS - FIM------>>>
      LOOP AT SCREEN.
**<<<------"145379 - NMS - INI------>>>
*        IF screen-group1 EQ 'GR5'.
        IF screen-group1 EQ 'GR5' OR
           screen-group1 EQ 'NFT'.
**<<<------"145379 - NMS - FIM------>>>
          screen-input = '1'.
        ENDIF.
**<<<------"173808 - NMS - INI------>>>
        IF screen-group1 EQ 'LGT' AND
           vl_qtlin      GT 1.
          screen-input = '1'.

        ENDIF.
**<<<------"173808 - NMS - FIM------>>>
        IF screen-name EQ 'WA_FILTRO_REMETENTE-DEPST' AND
           lv_lgort_diferente IS NOT INITIAL.
          screen-input = '1'.
        ENDIF.

        MODIFY SCREEN.
      ENDLOOP.
    ENDIF.
**<<<------"145379 - NMS - INI------>>>
  ENDIF.

  CLEAR lv_exit.
**<<<------"145379 - NMS - FIM------>>>
**<<<------"173808 - NMS - INI------>>>
  CLEAR vl_qtlin.
* Verifica se deu erro na valodação do Depósito com o Centro Virtual.
  IF NOT vl_err_lgort IS INITIAL.
    CLEAR vl_err_lgort.
    RETURN.

  ENDIF.
**<<<------"173808 - NMS - FIM------>>>
ENDMODULE.                 " STATUS_0033  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_REME_NOTAS_VINCULADAS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_alv_reme_notas_vinculadas OUTPUT.

  PERFORM plan_cria_reme_notas_vinc_alv.

ENDMODULE.                 " CRIA_ALV_REME_NOTAS_VINCULADAS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_REME_NOTAS_VINC_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plan_cria_reme_notas_vinc_alv .

  CONSTANTS: tabela_reme_notas TYPE string VALUE 'IT_ZNOM_REME_NOTAS_ALV'.

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
        text_n018 TYPE c LENGTH 50 VALUE 'Qtd. Rec./Dev.',
        text_n017 TYPE c LENGTH 50 VALUE 'Qtd. Efetivar',
        text_n014 TYPE c LENGTH 50 VALUE 'Dt. Emissão',
        text_n015 TYPE c LENGTH 50 VALUE 'NCM',
        text_n019 TYPE c LENGTH 50 VALUE 'Grp.',
        text_n020 TYPE c LENGTH 50 VALUE 'UF',
        text_n021 TYPE c LENGTH 50 VALUE 'Dt.Chegada',
        text_n022 TYPE c LENGTH 50 VALUE 'Número DU-e',
        text_n023 TYPE c LENGTH 50 VALUE 'R.A Emb.',
        text_n024 TYPE c LENGTH 50 VALUE 'Tipo',
        text_n025 TYPE c LENGTH 50 VALUE 'Entrada Prop.',
        text_n026 TYPE c LENGTH 50 VALUE 'EUDR'.

  IF plan_prim_reme_notas IS INITIAL.

    CREATE OBJECT plan_container_reme_notas
      EXPORTING
        container_name = 'CTN_REME_NOTAS_VINC'.

    CREATE OBJECT plan_alv_reme_notas
      EXPORTING
        i_parent = plan_container_reme_notas.

    CREATE OBJECT toolbar_reme_notas_alv
      EXPORTING
        io_alv_grid = plan_alv_reme_notas.

    SET HANDLER toolbar_reme_notas_alv->on_toolbar FOR plan_alv_reme_notas.
    SET HANDLER toolbar_reme_notas_alv->handle_user_command FOR plan_alv_reme_notas.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_reme_notas USING:
          tabela_reme_notas 'GRP_RETORNO'           text_n019 ' ' 01 03 space space   space space space space             space,
          tabela_reme_notas 'TP_NF_REM'             text_n024 ' ' 02 04 space space   space space space space             space,
          tabela_reme_notas 'NUMERO_DUE'            text_n022 ' ' 02 14 space space   space space space space             space,
          tabela_reme_notas 'CODIGO_RA_EMBARQUE'    text_n023 ' ' 02 08 space space   space space space space             space,
          tabela_reme_notas 'DOCNUM'                text_n001 'X' 02 10 space 'ALPHA' space 'X'   space c_grid_color_c200 space,
          tabela_reme_notas 'ITMNUM'                text_n002 ' ' 03 06 space space   space 'X'   'X'   space             space,
          tabela_reme_notas 'ID_FILIAL'             text_n003 ' ' 04 04 space space   space space space c_grid_color_c200 space,
          tabela_reme_notas 'MODEL'                 text_n004 ' ' 05 03 space space   space space space space             space,
          tabela_reme_notas 'SERIES'                text_n005 ' ' 06 03 space space   space space space c_grid_color_c200 space,
          tabela_reme_notas 'NFENUM'                text_n006 ' ' 07 09 space space   space space space c_grid_color_c200 space,
          tabela_reme_notas 'NR_QUANTIDADE2'        text_n013 ' ' 08 12 space space   'X'   space space c_grid_color_c300 space,
          tabela_reme_notas 'NR_EFETIVADA'          text_n016 ' ' 09 15 space space   'X'   space space c_grid_color_c600 space,
          tabela_reme_notas 'NR_RECUSADO'           text_n018 ' ' 10 15 space space   'X'   space space c_grid_color_recu space,
          tabela_reme_notas 'NR_SALDO_EFETIVAR'     text_n017 ' ' 11 15 space space   'X'   space space c_grid_color_c500 space,
          tabela_reme_notas 'DOCDAT'                text_n014 ' ' 12 10 space space   space space space space             space,
          tabela_reme_notas 'PARID'                 text_n007 ' ' 13 10 space 'ALPHA' space space space space             space,
          tabela_reme_notas 'NAME1'                 text_n008 ' ' 14 25 space space   space space space space             space,
          tabela_reme_notas 'REGIO'                 text_n020 ' ' 14 03 space space   space space space space             space,
          tabela_reme_notas 'MATNR'                 text_n009 ' ' 15 08 space 'ALPHA' space space space space             space,
          tabela_reme_notas 'MAKTX'                 text_n010 ' ' 16 25 space space   space space space space             space,
          tabela_reme_notas 'EUDR'                  text_n026 ' ' 17 04 space space   space space space space             space,
          tabela_reme_notas 'NBM'                   text_n015 ' ' 18 10 space space   space space space space             space,
          tabela_reme_notas 'CHARG'                 text_n011 ' ' 19 05 space space   space space space space             space,
          tabela_reme_notas 'CFOP'                  text_n012 ' ' 20 07 space 'CFOBR' space space space space             space,
          tabela_reme_notas 'ENTRAD'                text_n025 ' ' 21 07 space space   space space space space             space.

    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra      = c_x.
    plan_gs_layout-sel_mode   = c_a.
    plan_gs_layout-info_fname = 'ROWCOLOR'.

    CLEAR: wa_sort, it_sort[].
    wa_sort-spos      = 1.
    wa_sort-fieldname = 'GRP_RETORNO'.
    wa_sort-up        = 'X'.
    wa_sort-subtot    = space.
    wa_sort-expa      = space.
    APPEND wa_sort TO it_sort.

    CALL METHOD plan_alv_reme_notas->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = plan_gs_layout
      CHANGING
        it_fieldcatalog = plan_catalogo_reme_notas
        it_outtab       = it_znom_reme_notas_alv[]
        it_sort         = it_sort[].

*   Create Object for Event Handler
    CREATE OBJECT plan_event_handler_notas2.
    SET HANDLER: plan_event_handler_notas2->handle_hotspot_click_notas2 FOR plan_alv_reme_notas.

    plan_prim_reme_notas = c_x.
  ENDIF.

  CALL METHOD plan_alv_reme_notas->refresh_table_display.

  CALL METHOD plan_alv_reme_notas->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_REME_NOTAS_VINC_ALV

*&---------------------------------------------------------------------*
*&      Form  CONSULTA_NOTA_FISCAL_DISP
*&---------------------------------------------------------------------*
*       Consulta documentos fiscais disponíveis
*----------------------------------------------------------------------*
FORM consulta_nota_fiscal_disp .

  DATA vmaterial18  TYPE matnr18.

  DATA: r_docnum TYPE rsis_t_range.

  RANGES: r_data_emissao             FOR j_1bnfdoc-docdat,
          r_model                    FOR j_1bnfdoc-model.

* ---> S4 Migration - 10/06/2023 - DG
  TYPES: BEGIN OF ty_range_matnr,
           sign   TYPE  char1,
           option TYPE  char2,
           low    TYPE  matnr,
           high   TYPE  matnr,
         END OF ty_range_matnr.
* <--- S4 Migration - 10/06/2023 - DG

  DATA: it_zsdt0024                  TYPE TABLE OF zsdt0024 WITH HEADER LINE,
        wa_zsdt0024                  TYPE zsdt0024,
        it_centros                   TYPE TABLE OF lxhme_range_c4,
        it_cfops                     TYPE TABLE OF lxhme_range_c10,
        it_cfops_comercializacao     TYPE TABLE OF lxhme_range_c10, "// wbarbosa 07112024 - US-156375
        it_cfops_fim_espeficido      TYPE TABLE OF lxhme_range_c10, "// wbarbosa 07112024 - US-156375
        it_produtor                  TYPE TABLE OF lxhme_range_c10,
        it_produtos                  TYPE TABLE OF ty_range_matnr, "lxhme_range_c18, "---> S4 Migration - 10/06/2023 - DG
        wa_centros                   TYPE lxhme_range_c4,
        wa_produtos                  TYPE ty_range_matnr, "lxhme_range_c18,
        vg_cancel                    TYPE j_1bcancel,
        vg_tabix                     TYPE sy-tabix,
        it_zdoc_nf_produtor_utlz     TYPE TABLE OF zdoc_nf_produtor WITH HEADER LINE,
        it_zdoc_nf_produtor          TYPE TABLE OF zdoc_nf_produtor WITH HEADER LINE,
        it_znom_reme_notas           TYPE TABLE OF znom_reme_notas  WITH HEADER LINE,
        wa_zdoc_nf_produtor          TYPE zdoc_nf_produtor,
        wa_znom_reme_notas           TYPE znom_reme_notas,
        it_zdoc_nf_produtor_efetiv   TYPE TABLE OF zdoc_nf_produtor WITH HEADER LINE,
        it_znom_notasfiscais_alv_aux TYPE TABLE OF zplac_notasfiscais WITH HEADER LINE,
        it_lfa1_v                    TYPE TABLE OF lfa1 WITH HEADER LINE,
        it_kna1_v                    TYPE TABLE OF kna1 WITH HEADER LINE,
        it_j_1bbranch_b              TYPE TABLE OF j_1bbranch WITH HEADER LINE,
        it_makt                      TYPE TABLE OF makt WITH HEADER LINE,
        it_virtual                   TYPE TABLE OF zsdt_depara_cen WITH HEADER LINE,
        it_zlest0170                 TYPE TABLE OF zlest0170 WITH HEADER LINE,
        "IT_ZNOM_NOTASFISCAIS_AUX     TYPE TABLE OF ZPLAC_NOTASFISCAIS WITH HEADER LINE,
        "IT_ZLEST0146                 TYPE TABLE OF ZLEST0146 WITH HEADER LINE,
        "IT_ZLEST0147                 TYPE TABLE OF ZLEST0147 WITH HEADER LINE,
        "IT_ZLEST0168                 TYPE TABLE OF ZLEST0168 WITH HEADER LINE,
        it_zsdt0001_ro_vinc          TYPE zsdt0001_ro_vinc_t,
        vg_menge                     LIKE ekpo-menge,
        v_menge_efetivo              TYPE zdoc_nf_produtor-menge,
        wl_zlest0146                 TYPE zlest0146,
**<<<------"163355 - NMS - INI------>>>
        wl_zlest0146_s               TYPE zlest0146,
        vl_doc_rateio                TYPE c,
**<<<------"163355 - NMS - FIM------>>>
        lt_zlest0147                 TYPE zlest0147_t,
        lt_zlest0168                 TYPE zlest0168_t,
        wl_setleaf_nf_porto          TYPE setleaf,
        wl_setleaf_grv_nf_porto      TYPE setleaf,
        v_doc_rateio                 TYPE char01,
        lv_nfs_vinc                  TYPE c,
        lv_entrada_cct               TYPE c,
        lv_saida_cct                 TYPE c,
        lv_ambas_cct                 TYPE c.

  DATA: zcl_util TYPE REF TO zcl_util. "// WBARBOSA 07112024 US-156375
  CREATE OBJECT zcl_util. "// WBARBOSA 07112024 US-156375

  DATA: lv_vinc_flote_parcial TYPE c,
        lv_total_vinc_flote   TYPE j_1bnetqty.

*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
  DATA:
        ls_zlest0146 TYPE zlest0146.
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração

  FREE: it_baixas_nf.

  wa_produtos-sign   = 'I'.
  wa_produtos-option = 'EQ'.
*---> 07/06/2023 - Migração S4 - JS
  vmaterial18 = |{ wa_znom_programacao-id_material ALPHA = IN }|.
  wa_znom_programacao-id_material = vmaterial18.
  wa_produtos-low    = wa_znom_programacao-id_material.
  wa_produtos-high   = wa_znom_programacao-id_material.
*<--- 07/06/2023 - Migração S4 - JS
  APPEND wa_produtos TO it_produtos.

  SELECT * INTO TABLE it_zsdt0024
    FROM zsdt0024
   WHERE matnr1 EQ wa_znom_programacao-id_material.

  LOOP AT it_zsdt0024 INTO wa_zsdt0024.
    vmaterial18 = |{ wa_zsdt0024-matnr2 ALPHA = IN }|.
    wa_produtos-sign   = 'I'.
    wa_produtos-option = 'EQ'.
*---> 07/06/2023 - Migração S4 - JS
    wa_produtos-low    = vmaterial18.
    wa_produtos-high   = vmaterial18.

*<--- 07/06/2023 - Migração S4 - JS

    APPEND wa_produtos TO it_produtos.
  ENDLOOP.

  IF wa_filtro_remetente-fins_espec IS NOT INITIAL.

    CALL FUNCTION 'Z_MEMO_CFOP_ENTRADAS'
      TABLES
        cfops = it_cfops.

*** Inicio - Rubenilson - 22.07.24 - Projeto Execução
  ELSEIF wa_filtro_remetente-comerc IS NOT INITIAL.

    CALL FUNCTION 'Z_MEMO_CFOP_COMERCIALIZACAO'
      TABLES
        cfops = it_cfops.

  ENDIF.
*** Fim - Rubenilson - 22.07.24 - Projeto Execução

  IF NOT wa_filtro_remetente-remetente IS INITIAL.
    wa_produtos-sign   = 'I'.
    wa_produtos-option = 'EQ'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_filtro_remetente-remetente
      IMPORTING
        output = wa_filtro_remetente-remetente.

    wa_produtos-low    = wa_filtro_remetente-remetente.
    wa_produtos-high   = wa_filtro_remetente-remetente.

    APPEND wa_produtos TO it_produtor.
  ENDIF.

  wa_centros-sign   = 'I'.
  wa_centros-option = 'EQ'.
  wa_centros-low    = wa_filtro_remetente-centro.
  wa_centros-high   = wa_filtro_remetente-centro.
  APPEND wa_centros TO it_centros.

  SELECT * INTO TABLE it_virtual
    FROM zsdt_depara_cen
   WHERE vkorg       EQ wa_filtro_remetente-empresa
     AND centro_real EQ wa_filtro_remetente-centro.

  LOOP AT it_virtual.
    wa_centros-sign   = 'I'.
    wa_centros-option = 'EQ'.
    wa_centros-low    = it_virtual-centrov_1.
    wa_centros-high   = it_virtual-centrov_1.
    APPEND wa_centros TO it_centros.
  ENDLOOP.

  CLEAR: it_znom_notasfiscais_alv[], it_zlest0170[], it_kna1_v[], it_lfa1_v[],
         it_j_1bbranch_b[],  it_zdoc_nf_produtor[],  it_znom_reme_notas[],
         it_zdoc_nf_produtor_utlz[].

  CLEAR: wl_setleaf_nf_porto.
  SELECT SINGLE * FROM setleaf INTO wl_setleaf_nf_porto WHERE setname = 'ZMEMO00_DT_NF_PORTO'.

  CLEAR: wl_setleaf_grv_nf_porto.
  SELECT SINGLE * FROM setleaf INTO wl_setleaf_grv_nf_porto WHERE setname = 'ZMEMO00_GRV_DT_NF_PORTO'.

  IF NOT it_cfops[] IS INITIAL.

    CLEAR: r_data_emissao[].

    "Define data mínima de emissão
    SELECT SINGLE *
      FROM setleaf INTO @DATA(_wl_setleaf)
     WHERE setname EQ 'ZMEMO00_MIN_DATA_EMISSAO'.

    DATA(_dt_ini) = abap_false.
    IF ( sy-subrc = 0 ) AND ( _wl_setleaf-valfrom IS NOT INITIAL ) AND ( wa_filtro_remetente-data_ini IS INITIAL ).
      r_data_emissao-sign   = 'I'.
      r_data_emissao-option = 'BT'.
      r_data_emissao-low    = sy-datum - _wl_setleaf-valfrom.

      IF wa_filtro_remetente-data_fim IS NOT INITIAL.
        r_data_emissao-high = wa_filtro_remetente-data_fim.
      ELSE.
        r_data_emissao-high = sy-datum.
      ENDIF.

      APPEND r_data_emissao.
      _dt_ini = abap_true.
    ELSEIF wa_filtro_remetente-data_ini IS NOT INITIAL.

      IF wa_filtro_remetente-data_fim IS INITIAL.
        MESSAGE 'A Dt. Final é um campo obrigatório!' TYPE 'S'.
        RETURN.
      ENDIF.

      r_data_emissao-sign   = 'I'.
      r_data_emissao-option = 'BT'.
      r_data_emissao-low    = wa_filtro_remetente-data_ini.
      r_data_emissao-high   = wa_filtro_remetente-data_fim.

      APPEND r_data_emissao.
      _dt_ini = abap_true.

    ENDIF.

    IF _dt_ini EQ abap_false.
      MESSAGE 'A Dt. Inicio é um campo obrigatório!' TYPE 'S'.
      RETURN.
    ENDIF.

*** Inicio - Rubenilson - 22.07.24 - Projeto Execução
*    IF wa_filtro_remetente-tp_vinc1 EQ abap_true.
*      MESSAGE 'Opção "Vinc.c/RFL" não disponível!' TYPE 'S'.
*      RETURN.
*    ENDIF.
*** Fim - Rubenilson - 22.07.24 - Projeto Execução

    CHECK r_data_emissao[] IS NOT INITIAL.

    "Modelo
    CLEAR: r_model[].

    r_model-sign   = 'I'.
    r_model-option = 'EQ'.
    r_model-low    = '55'.
    APPEND r_model.

    r_model-low    = '01'.
    APPEND r_model.

    r_model-low    = '04'.
    APPEND r_model.

    "Check alteração em documentos Efetivados no periodo
    CLEAR: it_zdoc_nf_produtor_efetiv[].
    SELECT *
     FROM zdoc_nf_produtor AS a INTO TABLE it_zdoc_nf_produtor_efetiv
    WHERE a~branch_prod   IN it_centros
      AND a~docdat_prod   IN r_data_emissao
      AND a~matnr_prod    IN it_produtos
      AND a~doc_efetivado EQ abap_true
      AND a~menge_util    NE ( SELECT SUM( menge )
                                 FROM zdoc_nf_produtor AS b
                                WHERE b~docnum_prod = a~docnum_prod
                                  AND b~itmnum_prod = a~itmnum_prod
                                  AND NOT EXISTS ( SELECT x~vbeln_re_exp FROM zdoc_exp_recusa AS x WHERE x~vbeln_re_exp EQ b~vbeln ) ).

    SELECT *
     FROM zdoc_nf_produtor AS a APPENDING TABLE it_zdoc_nf_produtor_efetiv
    WHERE a~branch_prod   IN it_centros
      AND a~docdat_prod   IN r_data_emissao
      AND a~matnr_prod    IN it_produtos
      AND a~doc_efetivado EQ abap_true
      AND NOT EXISTS ( SELECT menge
                         FROM zdoc_nf_produtor AS b
                        WHERE b~docnum_prod = a~docnum_prod
                          AND b~itmnum_prod = a~itmnum_prod
                          AND NOT EXISTS ( SELECT x~vbeln_re_exp FROM zdoc_exp_recusa AS x WHERE x~vbeln_re_exp EQ b~vbeln ) ).

    LOOP AT it_zdoc_nf_produtor_efetiv.
      it_zdoc_nf_produtor_efetiv-doc_efetivado = abap_false.
      MODIFY zdoc_nf_produtor FROM it_zdoc_nf_produtor_efetiv.
    ENDLOOP.

    SELECT *
      INTO CORRESPONDING FIELDS OF TABLE it_znom_notasfiscais_alv
      FROM j_1bnfdoc AS dc
     INNER JOIN j_1bnflin AS li ON li~docnum EQ dc~docnum
*     INNER JOIN j_1bnfe_active AS ac ON ac~docnum EQ dc~docnum
     WHERE dc~bukrs  EQ wa_filtro_remetente-empresa
       AND dc~branch IN it_centros
       AND dc~docdat IN r_data_emissao
       "AND DC~DOCDAT GE WA_FILTRO_REMETENTE-DATA_INI
       "AND DC~DOCDAT LE WA_FILTRO_REMETENTE-DATA_FIM
       AND dc~direct EQ '1'
       AND dc~model  IN r_model
       AND dc~cancel EQ vg_cancel
       AND dc~doctyp NE '5'
       AND dc~parid  IN it_produtor
       AND li~cfop   IN it_cfops
       AND li~matnr  IN it_produtos
*       AND ac~cancel NE abap_true
       AND NOT EXISTS ( SELECT *
                         FROM zdoc_nf_produtor AS c
                        WHERE c~docnum_prod  = li~docnum
                          AND c~itmnum_prod  = li~itmnum
                          AND doc_efetivado  = abap_true ).

    IF it_znom_notasfiscais_alv[] IS NOT INITIAL.

      SELECT * FROM j_1bnfe_active INTO TABLE @DATA(gt_j_1bnfe_active)
        FOR ALL ENTRIES IN @it_znom_notasfiscais_alv
         WHERE docnum EQ @it_znom_notasfiscais_alv-docnum
           AND cancel  EQ @abap_true.
      IF sy-subrc EQ 0.
        r_docnum = VALUE #( FOR l IN gt_j_1bnfe_active ( sign = 'I' option = 'EQ' low = l-docnum ) ).
        IF r_docnum IS NOT INITIAL.
          DELETE it_znom_notasfiscais_alv WHERE docnum IN r_docnum.
        ENDIF.
      ENDIF.
    ENDIF.
    FREE: gt_j_1bnfe_active.


    LOOP AT it_znom_notasfiscais_alv ASSIGNING FIELD-SYMBOL(<fs_nota_fiscal>) WHERE partyp EQ 'B'.
      PERFORM f_converte_parid_br_to_lf CHANGING <fs_nota_fiscal>-parid
                                                 <fs_nota_fiscal>-partyp
                                                 <fs_nota_fiscal>-parvw.
    ENDLOOP.

*    LOOP AT IT_ZNOM_NOTASFISCAIS_ALV ASSIGNING FIELD-SYMBOL(<FS_NF_ALV>).
*
*
*    ENDLOOP.

*    DELETE IT_ZNOM_NOTASFISCAIS_ALV WHERE DEL_REG EQ ABAP_TRUE. "Deletar Registros Marcados

    CHECK it_znom_notasfiscais_alv[] IS NOT INITIAL.

    DELETE it_znom_notasfiscais_alv[] WHERE menge EQ 1. " Não mostra nota com valores = 1 Chamado 115585

    CHECK it_znom_notasfiscais_alv[] IS NOT INITIAL.

*"// WBARBOSA 07112024 US-156375 >>>>>>>>>>>>
    IF wa_filtro_remetente-tp_vinc1 EQ abap_true.
      SELECT *
      FROM zsdtvinc_p_flote
      INTO TABLE @DATA(lt_zsdtvinc_p_flote)
      FOR ALL ENTRIES IN @it_znom_notasfiscais_alv
      WHERE docnum_eprod EQ @it_znom_notasfiscais_alv-docnum
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
         AND cancel = ''.
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração
    ENDIF.
*"// WBARBOSA 07112024 US-156375 >>>>>>>>>>>>
*-CS2020000343 - 30.04.2021 - JT - inicio
    SELECT * FROM zsdt0276
             INTO TABLE it_zsdt0276
              FOR ALL ENTRIES IN it_znom_notasfiscais_alv
            WHERE docnum = it_znom_notasfiscais_alv-docnum
              AND itmnum = it_znom_notasfiscais_alv-itmnum.

    DELETE it_zsdt0276 WHERE baixar = ''.
    DELETE it_zsdt0276 WHERE status <> ''
                         AND status <> 'A'.

    LOOP AT it_zsdt0276    INTO wa_zsdt0276.
      ws_baixas_nf-docnum     = wa_zsdt0276-docnum.
      ws_baixas_nf-itmnum     = wa_zsdt0276-itmnum.
      ws_baixas_nf-menge      = wa_zsdt0276-menge.
      COLLECT ws_baixas_nf INTO it_baixas_nf.
    ENDLOOP.

    SORT it_baixas_nf BY docnum itmnum.
*-CS2020000343 - 30.04.2021 - JT - fim

*    IF SY-SUBRC IS NOT INITIAL.  " Adicionado NOT por causa das notas que já estão sem saldo
    IF it_znom_notasfiscais_alv[] IS NOT INITIAL.
********busca produtores selecionados *************************************************************
      "Vendor
      CLEAR: it_znom_notasfiscais_alv_aux[].
      MOVE it_znom_notasfiscais_alv[] TO it_znom_notasfiscais_alv_aux[].
      DELETE it_znom_notasfiscais_alv_aux[] WHERE partyp NE 'V'.
      SORT it_znom_notasfiscais_alv_aux[] BY parid.
      DELETE ADJACENT DUPLICATES FROM it_znom_notasfiscais_alv_aux COMPARING parid.

      IF it_znom_notasfiscais_alv_aux[] IS NOT INITIAL.
        SELECT * INTO TABLE it_lfa1_v
          FROM lfa1
           FOR ALL ENTRIES IN it_znom_notasfiscais_alv_aux
         WHERE lifnr EQ it_znom_notasfiscais_alv_aux-parid.
      ENDIF.

      "Cliente
      CLEAR: it_znom_notasfiscais_alv_aux[].
      MOVE it_znom_notasfiscais_alv[] TO it_znom_notasfiscais_alv_aux[].
      DELETE it_znom_notasfiscais_alv_aux[] WHERE partyp NE 'C'.
      SORT it_znom_notasfiscais_alv_aux[] BY parid.
      DELETE ADJACENT DUPLICATES FROM it_znom_notasfiscais_alv_aux COMPARING parid.

      IF it_znom_notasfiscais_alv_aux[] IS NOT INITIAL.
        SELECT * INTO TABLE it_kna1_v
          FROM kna1
           FOR ALL ENTRIES IN it_znom_notasfiscais_alv_aux
         WHERE kunnr EQ it_znom_notasfiscais_alv_aux-parid.
      ENDIF.

      "Local de Negócio
      CLEAR: it_znom_notasfiscais_alv_aux[].
      MOVE it_znom_notasfiscais_alv[] TO it_znom_notasfiscais_alv_aux[].
      DELETE it_znom_notasfiscais_alv_aux[] WHERE partyp NE 'B'.
      SORT it_znom_notasfiscais_alv_aux[] BY parid.
      DELETE ADJACENT DUPLICATES FROM it_znom_notasfiscais_alv_aux COMPARING parid.

      LOOP AT it_znom_notasfiscais_alv_aux INTO wa_znom_notasfiscais_alv.

        vg_tabix = sy-tabix.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_znom_notasfiscais_alv-parid
          IMPORTING
            output = wa_znom_notasfiscais_alv-parid.

        MODIFY it_znom_notasfiscais_alv INDEX vg_tabix FROM wa_znom_notasfiscais_alv TRANSPORTING parid.

      ENDLOOP.

      IF it_znom_notasfiscais_alv_aux[] IS NOT INITIAL.
        SELECT * INTO TABLE it_j_1bbranch_b
          FROM j_1bbranch
           FOR ALL ENTRIES IN it_znom_notasfiscais_alv_aux
         WHERE bukrs  EQ it_znom_notasfiscais_alv_aux-bukrs
           AND branch EQ it_znom_notasfiscais_alv_aux-parid+6(4).
      ENDIF.

********busca produtos selecionados ***************************************************************

      CLEAR: it_znom_notasfiscais_alv_aux[].
      MOVE it_znom_notasfiscais_alv[] TO it_znom_notasfiscais_alv_aux[].
      SORT it_znom_notasfiscais_alv_aux[] BY matnr.
      DELETE ADJACENT DUPLICATES FROM it_znom_notasfiscais_alv_aux COMPARING matnr.

      IF it_znom_notasfiscais_alv_aux[] IS NOT INITIAL.
        SELECT * INTO TABLE it_makt
          FROM makt
           FOR ALL ENTRIES IN it_znom_notasfiscais_alv_aux
         WHERE spras EQ sy-langu
           AND matnr EQ it_znom_notasfiscais_alv_aux-matnr.
      ENDIF.

********busca notas já vinculadas a partir das livres ( zdoc_nf_produtor ) - módulo antigo*********
      SELECT * INTO TABLE it_zdoc_nf_produtor
        FROM zdoc_nf_produtor AS p
         FOR ALL ENTRIES IN it_znom_notasfiscais_alv
       WHERE docnum_prod EQ it_znom_notasfiscais_alv-docnum
         AND itmnum_prod EQ it_znom_notasfiscais_alv-itmnum
         AND NOT EXISTS ( SELECT * FROM znom_prog_reme AS r WHERE r~id_remessa EQ p~vbeln ).

********busca notas já vinculadas a partir das livres ( znom_reme_notas  ) - módulo antigo*********
      SELECT * INTO TABLE it_znom_reme_notas
        FROM znom_reme_notas
         FOR ALL ENTRIES IN it_znom_notasfiscais_alv
       WHERE docnum EQ it_znom_notasfiscais_alv-docnum
         AND itmnum EQ it_znom_notasfiscais_alv-itmnum.

      SELECT * INTO TABLE it_zlest0170
        FROM zlest0170
         FOR ALL ENTRIES IN it_znom_notasfiscais_alv
       WHERE docnum EQ it_znom_notasfiscais_alv-docnum.

      SELECT * INTO TABLE it_zdoc_nf_produtor_utlz
        FROM zdoc_nf_produtor AS p
         FOR ALL ENTRIES IN it_znom_notasfiscais_alv
       WHERE docnum_prod EQ it_znom_notasfiscais_alv-docnum
         AND itmnum_prod EQ it_znom_notasfiscais_alv-itmnum
         AND NOT EXISTS ( SELECT x~vbeln_re_exp FROM zdoc_exp_recusa AS x WHERE x~vbeln_re_exp EQ p~vbeln ).

    ENDIF.
**<<<------"145379 - NMS - INI------>>>
    IF NOT wa_filtro_remetente-numero_due         IS INITIAL AND
           wa_filtro_remetente-codigo_ra_embarque IS INITIAL.
      SELECT SINGLE id_due codigo_ra_embarque
        FROM zsdt0170 INTO ( wa_filtro_remetente-id_due, wa_filtro_remetente-codigo_ra_embarque )
       WHERE numero_due       EQ wa_filtro_remetente-numero_due
         AND id_due_ref       EQ 0
         AND id_nomeacao_tran EQ wa_znom_transporte-id_nomeacao_tran.

    ENDIF.

    IF wa_filtro_remetente-codigo_ncm IS INITIAL.
      CLEAR it_due_antecipada_alv.
      READ TABLE it_due_antecipada_alv WITH KEY matnr      = wa_filtro_remetente-material
                                                numero_due = wa_filtro_remetente-numero_due.

      IF sy-subrc IS INITIAL.
        wa_filtro_remetente-ue_exportada = it_due_antecipada_alv-ue_exportada.
        wa_filtro_remetente-codigo_ncm   = it_due_antecipada_alv-codigo_ncm.
        TRANSLATE wa_filtro_remetente-codigo_ncm USING '. '.
        CONDENSE wa_filtro_remetente-codigo_ncm NO-GAPS.
        CLEAR it_due_antecipada_alv.

      ENDIF.

    ENDIF.
**<<<------"145379 - NMS - FIM------>>>
    SORT: it_lfa1_v        BY lifnr,
          it_kna1_v        BY kunnr,
          it_j_1bbranch_b  BY bukrs branch,
          it_zlest0170     BY docnum.

    LOOP AT it_znom_notasfiscais_alv ASSIGNING FIELD-SYMBOL(<fs_nf_alv>). "INTO WA_ZNOM_NOTASFISCAIS_ALV.

      vg_tabix = sy-tabix.

*----------------------------------------------------------------------------------------------------------*
*     Pré Validações Ini
*----------------------------------------------------------------------------------------------------------*
      <fs_nf_alv>-id_due = wa_filtro_remetente-id_due.
*** Inicio - Rubenilson - 22.07.24 - Projeto Execução
      "Busca Informaçõe referente ao CCT
      "IF ( WL_SETLEAF_NF_PORTO-VALFROM IS INITIAL ).
*      PERFORM f_atrib_inf_cct CHANGING <fs_nf_alv>.
      "ENDIF.

**** CS2019000714 / Inicio
*      PERFORM f_atrib_inf_cte_rfl CHANGING <fs_nf_alv>.
**** CS2019000714 / fim
**<<<------"145379 - NMS - INI------>>>
      CLEAR: lv_entrada_cct, lv_saida_cct, lv_ambas_cct, wl_zlest0146.
**<<<------"145379 - NMS - FIM------>>>
      CALL FUNCTION 'ZSD_VALIDA_NOTA_CCT'
        EXPORTING
          i_docnum      = <fs_nf_alv>-docnum
        IMPORTING
          e_entrada_cct = lv_entrada_cct
          e_saida_cct   = lv_saida_cct
          e_ambas_cct   = lv_ambas_cct
          e_zlest0146   = wl_zlest0146.
*** Fim - Rubenilson - 22.07.24 - Projeto Execução

      CASE abap_true.
        WHEN wa_filtro_remetente-tp_vinc1. "Com Vinculo RFL
**<<<------"163355 - NMS - INI------>>>
          DATA: tl_docnum            TYPE j_1bnfe_t_docnum,
                tl_consulta_terminal TYPE zsdct_consulta_terminal.
* Valida o Terminal de embarque com relação a NF de entrada e/ou saída.
          PERFORM zf_valida_terminal TABLES tl_consulta_terminal
                                      USING <fs_nf_alv>-docnum
                                            wa_filtro_remetente-codigo_ra_embarque
                                            <fs_nf_alv>-del_reg.

          IF NOT <fs_nf_alv>-del_reg IS INITIAL.
            CONTINUE.

          ENDIF.


* Verifica se é Nota EUDR, qual o tipo e valida as condições.
          PERFORM f_filter_eudr_documents TABLES tl_consulta_terminal
                                           USING <fs_nf_alv>-del_reg.

          IF NOT <fs_nf_alv>-del_reg IS INITIAL.
            CONTINUE.

          ELSE.
* Busca o campo EUDR da Nota Fiscal de entrada.
            zcl_eudr_utils=>check_doc_fiscal_eudr( EXPORTING
                                                     i_docnum =  <fs_nf_alv>-docnum
                                                   RECEIVING
                                                     r_eudr   = <fs_nf_alv>-eudr
                                                  ).
* Verifica se o flag "Listar somente EUDR" está marcado.
            IF wa_filtro_remetente-retornar_eudr EQ abap_on.
**<<<------"173808 - NMS - INI------>>>
** Verifica se a Nota de Entrada é EUDR diferente de S.
*              IF <fs_nf_alv>-eudr NE 'S'. "S - EUDR.
* Verifica se a Nota de Entrada é EUDR = X.
              IF <fs_nf_alv>-eudr NE abap_on. "X - EUDR / ' ' - Não EUDR.
**<<<------"173808 - NMS - FIM------>>>
                <fs_nf_alv>-del_reg = abap_on.
                CONTINUE.

              ENDIF.

            ELSE.
**<<<------"173808 - NMS - INI------>>>
** Verifica se o EUDR da Nota de Entrada é diferente do EUDR da DU-e.
*              IF wa_filtro_remetente-due_eudr NE <fs_nf_alv>-eudr.
*                <fs_nf_alv>-del_reg = abap_on.
*                CONTINUE.
*
*              ENDIF.
* Verifica se o EUDR da Nota de Entrada é compatível com o EUDR da DU-e.
              IF ( wa_filtro_remetente-due_eudr EQ 'S'        AND
                   <fs_nf_alv>-eudr             EQ abap_off ) OR
                 ( wa_filtro_remetente-due_eudr NE 'S'        AND
                   <fs_nf_alv>-eudr             EQ abap_on ).
                <fs_nf_alv>-del_reg = abap_on.
                CONTINUE.

              ENDIF.
**<<<------"173808 - NMS - FIM------>>>
            ENDIF.

          ENDIF.
**<<<------"163355 - NMS - FIM------>>>
          "---------------------------------------------------------------------------------------------------------------*
          " Só trazer Entradas que possuam vínculo com saídas de Remessa Form.Lote,
          " pois os mesmas terão que ser vinculadas na transação de Retorno Formação Lote(ZSDT0008) de forma obrigatória.
          "---------------------------------------------------------------------------------------------------------------*

*** Inicio - Rubenilson - 22.07.24 - Projeto Execução
*          "Check se Documento de Entrada possui vinculo com Rem.Form.Lote
*          CALL FUNCTION 'ZROMANEIO_VINCULADO_NF'
*            EXPORTING
*              i_docnum            = <fs_nf_alv>-docnum
*              i_ck_vinc_zmemo00   = abap_true
*              i_ck_cfop_e_zmemo00 = abap_true
*            IMPORTING
*              e_zsdt0001_ro_vinc  = it_zsdt0001_ro_vinc.
*
*          IF ( it_zsdt0001_ro_vinc[] IS INITIAL ) OR  "Sem Vinculo
*             ( <fs_nf_alv>-peso_cct <= 0        ).    "Sem Peso CCT
*            <fs_nf_alv>-del_reg = abap_true.
*            CONTINUE.
*          ENDIF.

          CALL FUNCTION 'ZSD_VALIDA_NF_FORMACAO_LOTE'
            EXPORTING
              i_docnum         = <fs_nf_alv>-docnum
              i_direcao        = 'E'
            IMPORTING
              e_nfs_vinculadas = lv_nfs_vinc.
          IF lv_nfs_vinc IS INITIAL.

            <fs_nf_alv>-del_reg = abap_true.

          ELSE.
***VALIDA DADOS REFERENTE AO CCT
            IF lv_ambas_cct IS NOT INITIAL.
              <fs_nf_alv>-del_reg = abap_true.
            ELSEIF lv_entrada_cct IS NOT INITIAL.
**<<<------"145379 - NMS - INI------>>>
* Valida o campo Retorno será gerada com NF de RFL de Terceiro.
              IF znom_remetente-retorno_com_nf_de_terceiro IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
                IF wa_filtro_remetente-numero_due IS NOT INITIAL.
                  IF wl_zlest0146-local_codigo_ra NE wa_filtro_remetente-codigo_ra_embarque.
                    <fs_nf_alv>-del_reg = abap_true.
                  ELSE.
                    <fs_nf_alv>-cct_prod         = abap_true.
                    <fs_nf_alv>-nf_chegada_porto = c_pro.

                    <fs_nf_alv>-dt_chegada       = wl_zlest0146-dt_recepcao.
                    <fs_nf_alv>-peso_cct         = wl_zlest0146-peso_disponivel_uso.
                    <fs_nf_alv>-peso_aferido_cct = wl_zlest0146-peso_aferido_recepcao.
                  ENDIF.

                ENDIF.
**<<<------"145379 - NMS - INI------>>>
              ELSE.
                <fs_nf_alv>-del_reg = abap_true.

              ENDIF.
**<<<------"145379 - NMS - FIM------>>>
            ELSEIF lv_saida_cct IS NOT INITIAL.
**<<<------"145379 - NMS - INI------>>>
* Valida o campo Retorno será gerada com NF de RFL de Terceiro.
              IF znom_remetente-retorno_com_nf_de_terceiro IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
                IF wa_filtro_remetente-numero_due IS NOT INITIAL.
**<<<------"163355 - NMS - INI------>>>
*                  IF wl_zlest0146-local_codigo_ra NE wa_filtro_remetente-codigo_ra_embarque.
*                    <fs_nf_alv>-del_reg = abap_true.
*                  ELSE.
**<<<------"163355 - NMS - FIM------>>>
                  <fs_nf_alv>-nf_chegada_porto = c_rfl.
                  <fs_nf_alv>-cct_rfl          = abap_true.
*                  ENDIF. **<<<------"163355 - NMS ------>>>
                ENDIF.
**<<<------"145379 - NMS - INI------>>>
              ELSE.
                <fs_nf_alv>-del_reg = abap_true.

              ENDIF.
**<<<------"145379 - NMS - FIM------>>>
            ELSE.
**<<<------"145379 - NMS - INI------>>>
* Valida o campo Retorno será gerada com NF de RFL de Terceiro.
              IF znom_remetente-retorno_com_nf_de_terceiro IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
                <fs_nf_alv>-del_reg = abap_true.
**<<<------"145379 - NMS - INI------>>>
              ENDIF.
**<<<------"145379 - NMS - FIM------>>>
            ENDIF.

          ENDIF.
*** Fim - Rubenilson - 22.07.24 - Projeto Execução

          <fs_nf_alv>-tp_nf_rem = c_f.  "Vinculado Remessa Formação Lote

        WHEN wa_filtro_remetente-tp_vinc2. "Sem Vinculo RFL
**<<<------"145379 - NMS - INI------>>>
* Verificar se a NF Entrada já está vinculada.
          SELECT SINGLE * FROM zsdtvinc_p_flote
            INTO @DATA(ls_vinc_p_flote)
          WHERE docnum_eprod EQ @<fs_nf_alv>-docnum
            AND cancel       EQ @abap_false.

          IF sy-subrc IS INITIAL.
            <fs_nf_alv>-del_reg = abap_true.
            CLEAR ls_vinc_p_flote.
            CONTINUE.

          ENDIF.
*** Inicio - Rubenilson - 22.07.24 - Projeto Execução
          PERFORM f_atrib_inf_cct CHANGING <fs_nf_alv>.
*** Fim - Rubenilson - 22.07.24 - Projeto Execução

**<<<------"145379 - NMS - FIM------>>>
          CASE abap_true.
            WHEN wa_filtro_remetente-c_cct."Com CCT

*** Inicio - Rubenilson - 22.07.24 - Projeto Execução
              IF lv_entrada_cct IS INITIAL.
                <fs_nf_alv>-del_reg = abap_true.
                CONTINUE.
              ENDIF.
              "Busca Informaçõe referente ao CCT
              "IF ( WL_SETLEAF_NF_PORTO-VALFROM IS INITIAL ).
              "PERFORM f_atrib_inf_cct CHANGING <fs_nf_alv>.
              "ENDIF.

              "Temp Ini.
*              IF ( <fs_nf_alv>-peso_cct <= 0 ).
*                <fs_nf_alv>-del_reg = abap_true.
*                CONTINUE.
*              ENDIF.
              "Temp Fim.
*** Fim - Rubenilson - 22.07.24 - Projeto Execução

              "Definitivo Ini.
*              IF ( <FS_NF_ALV>-PESO_CCT <= 0             ) OR
*                 ( <FS_NF_ALV>-NF_CHEGADA_PORTO NE C_PRO ). "Desconsiderar senão for a NF do Produtor
*                <FS_NF_ALV>-DEL_REG = ABAP_TRUE.
*                CONTINUE.
*              ENDIF.
*
*              "Check se Documento de Entrada possui vinculo com Rem.Form.Lote
*              CALL FUNCTION 'ZROMANEIO_VINCULADO_NF'
*                EXPORTING
*                  I_DOCNUM            = <FS_NF_ALV>-DOCNUM
*                  I_CK_VINC_ZMEMO00   = ABAP_TRUE
*               IMPORTING
*                 E_ZSDT0001_RO_VINC   = IT_ZSDT0001_RO_VINC.
*
*              IF ( IT_ZSDT0001_RO_VINC[] IS NOT INITIAL ). "Com Vinculo
*                <FS_NF_ALV>-DEL_REG = ABAP_TRUE.
*                CONTINUE.
*              ENDIF.
              "Definitivo Fim.

              <fs_nf_alv>-tp_nf_rem = c_c.  "Com Registro CCT

            WHEN wa_filtro_remetente-s_cct."Sem CCT


              "Temp Ini.
*** Inicio - Rubenilson - 22.07.24 - Projeto Execução
*              IF ( <fs_nf_alv>-peso_cct > 0 ) OR ( <fs_nf_alv>-cct_prod EQ abap_true ).
              IF ( lv_entrada_cct IS NOT INITIAL ) OR ( <fs_nf_alv>-cct_prod EQ abap_true ).
*** Fim - Rubenilson - 22.07.24 - Projeto Execução
                <fs_nf_alv>-del_reg = abap_true.
                CONTINUE.
              ENDIF.
              "Temp Fim.

*"// WBARBOSA 07112024 US-156375 >>>>>>>>>>>>
              DATA(lv_chave_nfe)  = zcl_util->get_chave_nfe( <fs_nf_alv>-docnum ).

              IF lv_chave_nfe IS NOT INITIAL.

                SELECT COUNT(*)
                FROM zlest0186
                WHERE chave EQ lv_chave_nfe.

                IF sy-subrc IS INITIAL.
                  <fs_nf_alv>-del_reg = abap_true.
                  CONTINUE.
                ENDIF.

              ENDIF.
*"// WBARBOSA 07112024 US-156375 <<<<<<<<<<<

              "Definitivo Ini.
*              IF ( <FS_NF_ALV>-NF_CHEGADA_PORTO EQ C_PRO ). "Desconsiderar se NF do Produtor chegou no porto
*                <FS_NF_ALV>-DEL_REG = ABAP_TRUE.
*                CONTINUE.
*              ENDIF.
              "Definitivo Fim.

              <fs_nf_alv>-tp_nf_rem = c_s.  "Sem Registro CCT
          ENDCASE.
      ENDCASE.

*** Inicio - Rubenilson - 22.07.24 - Projeto Execução
*** CS2019000714 / Inicio
      PERFORM f_atrib_inf_cte_rfl CHANGING <fs_nf_alv>.
*** CS2019000714 / fim
*** Fim - Rubenilson - 22.07.24 - Projeto Execução

*----------------------------------------------------------------------------------------------------------*
*     Pré Validações Fim
*----------------------------------------------------------------------------------------------------------*


      IF <fs_nf_alv>-nfe IS INITIAL.
        MOVE <fs_nf_alv>-nfnum TO <fs_nf_alv>-nfenum.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_nf_alv>-nfenum
          IMPORTING
            output = <fs_nf_alv>-nfenum.
      ELSE.
        wa_znom_reme_notas_alv-nfenum = <fs_nf_alv>-nfenum.
      ENDIF.

      IF <fs_nf_alv>-meins NE 'KG'.
        CALL FUNCTION 'ME_CONVERSION_MEINS'
          EXPORTING
            i_matnr             = <fs_nf_alv>-matnr
            i_mein1             = <fs_nf_alv>-meins
            i_meins             = 'KG'
            i_menge             = <fs_nf_alv>-menge
          IMPORTING
            menge               = vg_menge
          EXCEPTIONS
            error_in_conversion = 1
            no_success          = 2
            OTHERS              = 3.
        IF NOT sy-subrc IS INITIAL.
          MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          CLEAR: it_znom_notasfiscais_alv[].
          EXIT.
        ELSE.
          <fs_nf_alv>-nr_quantidade2 = vg_menge.
        ENDIF.
      ELSE.
        <fs_nf_alv>-nr_quantidade2 = <fs_nf_alv>-menge.
      ENDIF.

*-CS2020000343 - 30.04.2021 - JT - inicio
      CLEAR ws_baixas_nf.
      READ TABLE it_baixas_nf INTO ws_baixas_nf WITH KEY docnum = <fs_nf_alv>-docnum
                                                         itmnum = <fs_nf_alv>-itmnum
                                                BINARY SEARCH.
      <fs_nf_alv>-nr_quantidade2 = <fs_nf_alv>-nr_quantidade2 - ws_baixas_nf-menge.
*-CS2020000343 - 30.04.2021 - JT - fim

      <fs_nf_alv>-qtde_nf = <fs_nf_alv>-nr_quantidade2.

* BUG SOLTO 189734* - SMC - 09-09-2025 - CORREÇÃO ESPECIFICA PARA ALZ (trecho comentado até finalizar definição entre equipe Daniel e ALZ)
*        IF SY-SYSID = 'QAS' AND SY-UNAME = 'SMCABANA'.
*            IF WA_FILTRO_REMETENTE-EMPRESA = '0035' AND <FS_NF_ALV>-NF_CHEGADA_PORTO = 'RFL'. "AND <fs_nf_alv>-qtde_nf < WL_ZLEST0146-PESO_AFERIDO_RECEPCAO.
*                <fs_nf_alv>-nr_quantidade2 = WL_ZLEST0146-PESO_AFERIDO_RECEPCAO.
*                ENDIF.
**                 ENDIF.
* BUG SOLTO 189734* - SMC - 09-09-2025 - CORREÇÃO ESPECIFICA PARA ALZ (trecho comentado até finalizar definição entre equipe Daniel e ALZ)


      "Definir Quantidade Disponivel para vinculação
      CASE abap_true.
        WHEN wa_filtro_remetente-tp_vinc1.

          IF lv_entrada_cct IS NOT INITIAL.
            <fs_nf_alv>-nr_quantidade2 = <fs_nf_alv>-peso_cct. "Disponibilizar peso CCT
            <fs_nf_alv>-nr_quantidade2 = <fs_nf_alv>-nr_quantidade2 - ws_baixas_nf-menge.
          ENDIF.

        WHEN wa_filtro_remetente-tp_vinc2.
          CASE abap_true.
            WHEN wa_filtro_remetente-c_cct.
              <fs_nf_alv>-nr_quantidade2 = <fs_nf_alv>-peso_cct. "Disponibilizar peso CCT
              <fs_nf_alv>-nr_quantidade2 = <fs_nf_alv>-nr_quantidade2 - ws_baixas_nf-menge.
            WHEN wa_filtro_remetente-s_cct.
          ENDCASE.
      ENDCASE.

****************************************************************************************************************************************
********Processa Cliente/Fonecedor/Local de Negócio ************************************************************************************
****************************************************************************************************************************************

      CASE <fs_nf_alv>-partyp.
        WHEN 'V'.
          READ TABLE it_lfa1_v WITH KEY lifnr = <fs_nf_alv>-parid BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_nf_alv>-name1 = it_lfa1_v-name1.
            <fs_nf_alv>-regio = it_lfa1_v-regio.
          ENDIF.
        WHEN 'C'.
          READ TABLE it_kna1_v WITH KEY kunnr = <fs_nf_alv>-parid BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_nf_alv>-name1 = it_kna1_v-name1.
            <fs_nf_alv>-regio = it_kna1_v-regio.
          ENDIF.
        WHEN 'B'.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = <fs_nf_alv>-parid
            IMPORTING
              output = <fs_nf_alv>-parid.

          READ TABLE it_j_1bbranch_b WITH KEY bukrs  = <fs_nf_alv>-bukrs
                                              branch = <fs_nf_alv>-parid+6(4) BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <fs_nf_alv>-name1 = it_j_1bbranch_b-name.
            READ TABLE it_lfa1_v INTO DATA(_lfa1_aux) WITH KEY lifnr = <fs_nf_alv>-parid BINARY SEARCH.
            IF sy-subrc = 0.
              <fs_nf_alv>-regio = _lfa1_aux-regio.
            ENDIF.
          ENDIF.
      ENDCASE.

****************************************************************************************************************************************
********Processa Material item documento ***********************************************************************************************
****************************************************************************************************************************************

      READ TABLE it_makt WITH KEY matnr = <fs_nf_alv>-matnr.
      IF sy-subrc IS INITIAL.
        <fs_nf_alv>-maktx  = it_makt-maktx .
      ENDIF.

****************************************************************************************************************************************
********Processa saldo de documento fiscal *********************************************************************************************
****************************************************************************************************************************************
      <fs_nf_alv>-nr_utilizada     = 0.
      <fs_nf_alv>-nr_saldo         = 0.

      "Verifica se nota fiscal foi vinculada em exportação (efetiva).
      LOOP AT it_zdoc_nf_produtor INTO wa_zdoc_nf_produtor WHERE docnum_prod EQ <fs_nf_alv>-docnum
                                                             AND itmnum_prod EQ <fs_nf_alv>-itmnum.
        <fs_nf_alv>-nr_utilizada = <fs_nf_alv>-nr_utilizada + wa_zdoc_nf_produtor-menge.
      ENDLOOP.
      <fs_nf_alv>-nr_saldo = <fs_nf_alv>-nr_quantidade2 - <fs_nf_alv>-nr_utilizada.


      CLEAR: v_menge_efetivo.
      LOOP AT it_zdoc_nf_produtor_utlz INTO wa_zdoc_nf_produtor WHERE docnum_prod EQ <fs_nf_alv>-docnum
                                                                  AND itmnum_prod EQ <fs_nf_alv>-itmnum.
        ADD wa_zdoc_nf_produtor-menge TO v_menge_efetivo.
      ENDLOOP.

      IF ( ( <fs_nf_alv>-qtde_nf - v_menge_efetivo ) EQ 0 ) AND  "Se Documento Efetivado
         ( <fs_nf_alv>-branch IS NOT INITIAL ) AND
         ( <fs_nf_alv>-docdat IS NOT INITIAL ) AND
         ( <fs_nf_alv>-matnr  IS NOT INITIAL ) AND
         ( <fs_nf_alv>-docnum IS NOT INITIAL ) AND
         ( <fs_nf_alv>-itmnum IS NOT INITIAL ).

        UPDATE zdoc_nf_produtor SET branch_prod   = <fs_nf_alv>-branch
                                    docdat_prod   = <fs_nf_alv>-docdat
                                    matnr_prod    = <fs_nf_alv>-matnr
                                    doc_efetivado = abap_true
                                    menge_util    = <fs_nf_alv>-qtde_nf
                              WHERE docnum_prod   = <fs_nf_alv>-docnum
                                AND itmnum_prod   = <fs_nf_alv>-itmnum.
      ENDIF.


      "Se existe saldo de nota fiscal verificar planejamento.
      IF <fs_nf_alv>-nr_saldo GT 0.
        LOOP AT it_znom_reme_notas INTO wa_znom_reme_notas WHERE docnum EQ <fs_nf_alv>-docnum
                                                             AND itmnum EQ <fs_nf_alv>-itmnum.
**<<<------"163355 - NMS - INI------>>>
*          <fs_nf_alv>-nr_utilizada = <fs_nf_alv>-nr_utilizada + wa_znom_reme_notas-nr_quantidade.
**<<<------"174413 - NMS - INI------>>>
          CASE abap_on.
            WHEN wa_filtro_remetente-tp_vinc1. "Com Vinculo RFL
**<<<------"174413 - NMS - FIM------>>>
              LOOP AT lt_zsdtvinc_p_flote INTO DATA(el_zsdtvinc_p_flote) WHERE docnum_eprod EQ wa_znom_reme_notas-docnum.
                READ TABLE tl_consulta_terminal TRANSPORTING NO FIELDS WITH KEY docnum = el_zsdtvinc_p_flote-docnum_flote.

                IF sy-subrc                              IS INITIAL                                AND
                   wa_znom_reme_notas-codigo_ra_embarque EQ wa_filtro_remetente-codigo_ra_embarque.
                  ADD wa_znom_reme_notas-nr_quantidade TO <fs_nf_alv>-nr_utilizada.
                  EXIT.

                ENDIF.

              ENDLOOP.
**<<<------"174413 - NMS - INI------>>>
            WHEN wa_filtro_remetente-tp_vinc2. "Sem Vinculo
              CHECK ( wa_znom_reme_notas-tp_nf_rem NE c_f ) AND "Não pode ser Vinculo com RFL
                    ( wa_znom_reme_notas-tp_nf_rem NE c_c ).    "Não pode ser Com CCT
              <fs_nf_alv>-nr_utilizada = <fs_nf_alv>-nr_utilizada + wa_znom_reme_notas-nr_quantidade.

            WHEN OTHERS.
*           Do nothing
          ENDCASE.
**<<<------"174413 - NMS - FIM------>>>
**<<<------"163355 - NMS - FIM------>>>
        ENDLOOP.

*"// wbartbosa 07112024 US-156375 >>>>>>
*"// Verificar se a nota está totalmente vinculada a uma NF de Saida
        IF wa_filtro_remetente-tp_vinc1 IS NOT INITIAL.
          CLEAR lv_total_vinc_flote.
**<<<------"163355 - NMS - INI------>>>
* Nova variável do totalizador da Nota de entrada sem validar EUDR e RA.
          DATA(lv_total_vinc_flote2) = lv_total_vinc_flote.
**<<<------"163355 - NMS - FIM------>>>
          LOOP AT lt_zsdtvinc_p_flote INTO DATA(ls_zsdtvinc_p_flote) WHERE docnum_eprod EQ <fs_nf_alv>-docnum.

*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
            CLEAR ls_zlest0146.

            IF  wa_filtro_remetente-tp_vinc1 IS NOT INITIAL
            AND <fs_nf_alv>-nf_chegada_porto EQ 'RFL'.

              CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
                EXPORTING
                  i_docnum    = ls_zsdtvinc_p_flote-docnum_flote
                IMPORTING
                  e_zlest0146 = ls_zlest0146.

              IF ls_zlest0146-dt_recepcao IS INITIAL.
                CONTINUE.
              ENDIF.

            ENDIF.
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração

**<<<------"163355 - NMS - INI------>>>
*            ADD ls_zsdtvinc_p_flote-qtd_vinc TO lv_total_vinc_flote.
* Somatória do total de vinculo da Nota de Entrada sem validar EUDR e RA.
            ADD ls_zsdtvinc_p_flote-qtd_vinc TO lv_total_vinc_flote2.

            READ TABLE tl_consulta_terminal TRANSPORTING NO FIELDS WITH KEY docnum = ls_zsdtvinc_p_flote-docnum_flote.

            IF sy-subrc IS INITIAL.
* Somatória do total de vinculo da Nota de Entrada com validação de EUDR e RA.
              ADD ls_zsdtvinc_p_flote-qtd_vinc TO lv_total_vinc_flote.

            ENDIF.
**<<<------"163355 - NMS - FIM------>>>
          ENDLOOP.

          IF lv_total_vinc_flote < <fs_nf_alv>-nr_quantidade2.
            <fs_nf_alv>-nr_saldo  = lv_total_vinc_flote - <fs_nf_alv>-nr_utilizada.
**<<<------"163355 - NMS - INI------>>>
*            lv_vinc_flote_parcial = abap_true.
**<<<------"174413 - NMS - INI------>>>
* Verifica o tipo do CCT
            CASE abap_on.
              WHEN lv_entrada_cct. "CCT de Entrada
                IF lv_total_vinc_flote2 LT <fs_nf_alv>-qtde_nf.
                  lv_vinc_flote_parcial = abap_true.

                ELSE.
                  lv_vinc_flote_parcial = abap_false.

                ENDIF.

              WHEN lv_saida_cct.   "CCT de Saída
**<<<------"174413 - NMS - FIM------>>>
                IF lv_total_vinc_flote2 LT <fs_nf_alv>-nr_quantidade2.
                  lv_vinc_flote_parcial = abap_true.

                ELSE.
                  lv_vinc_flote_parcial = abap_false.

                ENDIF.
**<<<------"174413 - NMS - INI------>>>
              WHEN OTHERS.
* Do nothing
            ENDCASE.
**<<<------"174413 - NMS - FIM------>>>
**<<<------"163355 - NMS - FIM------>>>
          ELSE.
            <fs_nf_alv>-nr_saldo  = <fs_nf_alv>-nr_quantidade2 - <fs_nf_alv>-nr_utilizada.
            lv_vinc_flote_parcial = abap_false.
          ENDIF.
        ELSE.
          <fs_nf_alv>-nr_saldo  = <fs_nf_alv>-nr_quantidade2 - <fs_nf_alv>-nr_utilizada.
          lv_vinc_flote_parcial = abap_false.
        ENDIF.
      ELSE.
        <fs_nf_alv>-nr_saldo  = <fs_nf_alv>-nr_quantidade2 - <fs_nf_alv>-nr_utilizada.
        lv_vinc_flote_parcial = abap_false.
*"// wbartbosa 07112024 US-156375 <<<<<<
      ENDIF.
*    <fs_nf_alv>-nr_saldo  = <fs_nf_alv>-nr_quantidade2 - <fs_nf_alv>-nr_utilizada. "// wbartbosa 07112024 US-156375

*--------------------------------------------------------------------------------------------------------------*
*     Calculos com base em quantidades do CCT
*--------------------------------------------------------------------------------------------------------------*
      <fs_nf_alv>-nr_utilizada_cct = 0.
      <fs_nf_alv>-saldo_cct        = 0.
      <fs_nf_alv>-dif_peso_cct_nf  = 0.

      "Quantidade Utilizada
      LOOP AT it_znom_reme_notas INTO wa_znom_reme_notas WHERE docnum    EQ <fs_nf_alv>-docnum
                                                           AND itmnum    EQ <fs_nf_alv>-itmnum.

        CHECK ( wa_znom_reme_notas-tp_nf_rem EQ c_f ) OR "Vinculo com RFL
              ( wa_znom_reme_notas-tp_nf_rem EQ c_c ).    "Com CCT

        ADD wa_znom_reme_notas-nr_quantidade TO <fs_nf_alv>-nr_utilizada_cct.

      ENDLOOP.
**<<<------"174413 - NMS - INI------>>>
*      <fs_nf_alv>-saldo_cct  = <fs_nf_alv>-peso_cct - <fs_nf_alv>-nr_utilizada_cct.
* Verifica a Origem da CCT.


*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Início de Alteração
*      CASE abap_on.
*        WHEN lv_entrada_cct. "CCT Produtor
*          <fs_nf_alv>-saldo_cct  = <fs_nf_alv>-peso_cct - <fs_nf_alv>-nr_utilizada_cct.
*
*        WHEN lv_saida_cct.   "CCT RFL
*          <fs_nf_alv>-saldo_cct  = <fs_nf_alv>-qtde_nf - <fs_nf_alv>-nr_utilizada_cct.
*
*        WHEN OTHERS.
**       Do nothing
*      ENDCASE.

      CASE abap_on.
        WHEN lv_entrada_cct. "CCT Produtor
          <fs_nf_alv>-saldo_cct  = <fs_nf_alv>-peso_cct.

        WHEN lv_saida_cct.   "CCT RFL
          <fs_nf_alv>-saldo_cct  = <fs_nf_alv>-qtde_nf.

      ENDCASE.
*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Fim de Alteração

* Verifica se existe ou não CCT.
      IF NOT wa_filtro_remetente-c_cct IS INITIAL. "Com CCT
        CASE abap_on.

*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Início de Alteração
*          WHEN wa_filtro_remetente-tp_vinc1. "Com Vinculo RFL
*            IF <fs_nf_alv>-nr_saldo GE <fs_nf_alv>-qtde_nf.
*              <fs_nf_alv>-nr_saldo = <fs_nf_alv>-qtde_nf - <fs_nf_alv>-nr_utilizada_cct.
*
*            ELSE.
*              <fs_nf_alv>-nr_saldo = <fs_nf_alv>-nr_quantidade2 - <fs_nf_alv>-nr_utilizada_cct.
*
*            ENDIF.
*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Fim de Alteração

          WHEN wa_filtro_remetente-tp_vinc2. "Sem Vinculo
            IF <fs_nf_alv>-nr_saldo GT <fs_nf_alv>-qtde_nf.
              <fs_nf_alv>-nr_saldo = <fs_nf_alv>-qtde_nf - <fs_nf_alv>-nr_utilizada_cct.

            ELSE.
              <fs_nf_alv>-nr_saldo = <fs_nf_alv>-nr_quantidade2 - <fs_nf_alv>-nr_utilizada_cct.

            ENDIF.

          WHEN OTHERS.
*         Do nothing
        ENDCASE.

      ENDIF.
**<<<------"174413 - NMS - FIM------>>>
      IF <fs_nf_alv>-peso_cct > 0.
        <fs_nf_alv>-dif_peso_cct_nf = <fs_nf_alv>-qtde_nf   - <fs_nf_alv>-peso_cct.
      ENDIF.

*      DATA(_CALC_CCT) = ABAP_FALSE.
*      CASE ABAP_TRUE.
*        WHEN WA_FILTRO_REMETENTE-TP_VINC1. "Com Vinculo RFL
*          _CALC_CCT = ABAP_TRUE.
*        WHEN WA_FILTRO_REMETENTE-TP_VINC2. "Sem Vinculo RFL
*          CASE ABAP_TRUE.
*            WHEN WA_FILTRO_REMETENTE-C_CCT."Com CCT
*              _CALC_CCT = ABAP_TRUE.
*            WHEN WA_FILTRO_REMETENTE-S_CCT."Sem CCT
*          ENDCASE.
*      ENDCASE.

*      IF _CALC_CCT EQ ABAP_TRUE.
*
*      ENDIF.

*----------------------------------------------------------------------------------------------------------*
*     Pós Validações Ini
*----------------------------------------------------------------------------------------------------------*
      CASE abap_true.
        WHEN wa_filtro_remetente-tp_vinc1.

          IF lv_entrada_cct IS NOT INITIAL.

            IF ( <fs_nf_alv>-nr_saldo EQ 0 ) OR ( <fs_nf_alv>-saldo_cct EQ 0 ).
              <fs_nf_alv>-del_reg = abap_true.
            ENDIF.

          ELSEIF lv_saida_cct IS NOT INITIAL.

            IF ( <fs_nf_alv>-nr_saldo EQ 0 ).
              <fs_nf_alv>-del_reg = abap_true.
            ENDIF.

          ENDIF.

        WHEN wa_filtro_remetente-tp_vinc2.

          CASE abap_true.
            WHEN wa_filtro_remetente-c_cct. "Com Registro CCT
              IF <fs_nf_alv>-nr_saldo <= 0. "Definitivo Deixar só " = 0 "
                <fs_nf_alv>-del_reg = abap_true.
              ENDIF.
            WHEN wa_filtro_remetente-s_cct. "Sem Registro CCT.
              CASE <fs_nf_alv>-nf_chegada_porto.
                WHEN c_rfl.
                  "-------------------------------------------------------------------------------------------------------------------------------------------------*
                  " Notas de Produtor vinculada a uma Remessa Formação de Lote que chegou no Porto,
                  " só devem cair nessa opção, se já utilizou toda a quantidade do CCT.
                  " Nessa opção, é para liberar o saldo fiscal(Quantidade NF Prod. - Peso CCT RFL) da NF do Produtor para ser vinculada a uma outra RFL.
                  "----------------------------------------------------------------------------------------------------------------------------------*
                  "Definitivo Ini.
*                  IF <FS_NF_ALV>-SALDO_CCT > 0.
*                    <FS_NF_ALV>-DEL_REG = ABAP_TRUE.
*                  ENDIF.
                  "Definitivo Fim.
                  "CLEAR: <FS_NF_ALV>-SALDO_CCT, <FS_NF_ALV>-DIF_PESO_CCT_NF.
                WHEN c_pro.
                  "Não deve aparecer NF's de produtor que chegaram no porto
                WHEN OTHERS.
              ENDCASE.

              IF <fs_nf_alv>-nr_saldo = 0.
                <fs_nf_alv>-del_reg = abap_true.
              ENDIF.
          ENDCASE.
      ENDCASE.

      IF ( wa_filtro_remetente-numero_due IS NOT INITIAL ) AND ( wa_filtro_remetente-regio_due IS NOT INITIAL ).
        IF <fs_nf_alv>-regio NE wa_filtro_remetente-regio_due.
          <fs_nf_alv>-del_reg = abap_true.
        ENDIF.
      ENDIF.

      IF ( wa_filtro_remetente-regio IS NOT INITIAL ).
        IF <fs_nf_alv>-regio NE wa_filtro_remetente-regio.
          <fs_nf_alv>-del_reg = abap_true.
        ENDIF.
      ENDIF.

*----------------------------------------------------------------------------------------------------------*
*     Pós Validações Fim
*----------------------------------------------------------------------------------------------------------*

      "Ini - Busca Data Chegada
      IF ( <fs_nf_alv>-dt_chegada      IS INITIAL     ) AND
         ( <fs_nf_alv>-del_reg         EQ abap_false  ).
        "( WL_SETLEAF_NF_PORTO-VALFROM IS NOT INITIAL ).

        IF wl_setleaf_grv_nf_porto-valfrom IS NOT INITIAL.
          READ TABLE it_zlest0170 WITH KEY docnum = <fs_nf_alv>-docnum BINARY SEARCH.
          IF ( sy-subrc EQ 0 ) AND ( it_zlest0170-dt_chegada IS NOT INITIAL ).
            <fs_nf_alv>-dt_chegada = it_zlest0170-dt_chegada.
          ENDIF.
        ENDIF.

        IF <fs_nf_alv>-dt_chegada IS INITIAL.
          CALL FUNCTION 'Z_DATA_CHEGADA_NF_PORTO'
            EXPORTING
              i_bukrs       = <fs_nf_alv>-bukrs
              i_branch      = <fs_nf_alv>-branch
              i_parid       = <fs_nf_alv>-parid
              i_docdat      = <fs_nf_alv>-docdat
              i_nfenum      = <fs_nf_alv>-nfenum
              i_series      = <fs_nf_alv>-series
              i_nf_terceiro = 'X'
            IMPORTING
              e_dt_chegada  = <fs_nf_alv>-dt_chegada.

          IF ( <fs_nf_alv>-docnum              IS NOT INITIAL ) AND
             ( <fs_nf_alv>-dt_chegada          IS NOT INITIAL ) AND
             ( wl_setleaf_grv_nf_porto-valfrom IS NOT INITIAL ).
            CLEAR: it_zlest0170.
            it_zlest0170-docnum     = <fs_nf_alv>-docnum.
            it_zlest0170-dt_chegada = <fs_nf_alv>-dt_chegada.
            MODIFY zlest0170 FROM it_zlest0170.
          ENDIF.

        ENDIF.
      ENDIF.
      "Fim Busca Data Chegada

      IF <fs_nf_alv>-del_reg EQ abap_false.
        "Validações Contra Partida(RFL)
        PERFORM f_valida_nf_prod_rfl CHANGING <fs_nf_alv>.
      ENDIF.

      IF <fs_nf_alv>-del_reg EQ abap_false.
        "Validações Restrições de Vinculo
        PERFORM f_valida_restricoes_nf CHANGING <fs_nf_alv>.
        IF ( <fs_nf_alv>-restricao IS NOT INITIAL ) AND ( wa_filtro_remetente-ck_nf_restr IS NOT INITIAL ).
          <fs_nf_alv>-del_reg = abap_true.
        ENDIF.
      ENDIF.

      IF <fs_nf_alv>-dt_chegada IS INITIAL.
        <fs_nf_alv>-line_color  = 'C600'.
      ELSE.
        CLEAR: <fs_nf_alv>-line_color.
      ENDIF.

      IF ( <fs_nf_alv>-restricao IS NOT INITIAL ).
        <fs_nf_alv>-line_color  = 'C700'.
      ENDIF.

*"// wbarbosa 07112024 US-156375
      IF ( lv_vinc_flote_parcial IS NOT INITIAL ).
        <fs_nf_alv>-line_color  = 'C710'.
        <fs_nf_alv>-restricao   = TEXT-060.
      ENDIF.
*"// wbarbosa 07112024 US-156375

*** US #131067 - MMSILVA - 09.07.2025 - Ini ***

* COMENTADO EM 30-09-2025 PARA AVALIAÇÃO DO PROCESSO E TAMBEM DE PERFORMANCE DA TRANSAÇÃO QUANDO USA RANGE DE DATA MUITO LONGO - BUG SOLTO 191973 SMC
*          PERFORM f_consultar_devolucao_sigam using <fs_nf_alv> changing <fs_nf_alv>.


*** US #131067 - MMSILVA - 09.07.2025 - Fim ***

*      MODIFY IT_ZNOM_NOTASFISCAIS_ALV
*               INDEX VG_TABIX
*                FROM WA_ZNOM_NOTASFISCAIS_ALV
*        TRANSPORTING NAME1 REGIO MAKTX NR_QUANTIDADE2 NR_UTILIZADA NR_SALDO NFENUM DT_CHEGADA LINE_COLOR
*                     QTDE_NF PESO_CCT SALDO_CCT DIF_PESO_CCT_NF NF_CHEGADA_PORTO TP_NF_REM DEL_REG NR_UTILIZADA_CCT.

****************************************************************************************************************************************
****************************************************************************************************************************************
    ENDLOOP.

*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
    it_nf_doc = it_znom_notasfiscais_alv[].
    SORT it_nf_doc BY docnum.
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração

    DELETE it_znom_notasfiscais_alv WHERE del_reg EQ abap_true.
**<<<------"163355 - NMS - INI------>>>
*    PERFORM f_filter_eudr_documents TABLES it_znom_notasfiscais_alv. "// WBARBOSA 28102024 - US-153330
**<<<------"163355 - NMS - FIM------>>>
    "" Excluir as que estão com saldo zerado
    "IF ( WA_FILTRO_REMETENTE-TP_VINC1 EQ ABAP_TRUE ).
    "  DELETE IT_ZNOM_NOTASFISCAIS_ALV[] WHERE PESO_CCT NE 0 AND NR_SALDO EQ 0.
    "ELSE.
    "DELETE IT_ZNOM_NOTASFISCAIS_ALV[] WHERE NR_SALDO EQ 0.
    "ENDIF.

    SORT it_znom_notasfiscais_alv BY docdat.
  ENDIF.
ENDFORM.                    " CONSULTA_NOTA_FISCAL_DISP

"WBARBOSA 23102024 - US-153330 --->>>
*&---------------------------------------------------------------------*
*&      FORM F_FILTER_EUDR_DOCUMENTS
*&---------------------------------------------------------------------*
*       Verifica se é Nota EUDR, qual o tipo e valida as condições
*----------------------------------------------------------------------*
**<<<------"163355 - NMS - INI------>>>
*FORM f_filter_eudr_documents TABLES p_documentos STRUCTURE it_znom_notasfiscais_alv
FORM f_filter_eudr_documents TABLES p_documentos STRUCTURE zsde_consulta_terminal
                              USING uv_del_reg   TYPE      c.
**<<<------"163355 - NMS - FIM------>>>
  DATA: r_docnum TYPE rsis_t_range.

  DATA: r_eudr TYPE RANGE OF zeudr.

  CHECK p_documentos[] IS NOT INITIAL.

  FREE: r_eudr.

  CASE wa_filtro_remetente-due_eudr.
    WHEN zcl_eudr_utils=>lc_s_eudr.

      APPEND VALUE #( sign   = 'I'
                      option = 'EQ'
                      low    = zcl_eudr_utils=>lc_s_eudr ) TO r_eudr.
**<<<------"163355 - NMS - INI------>>>
*    WHEN zcl_eudr_utils=>lc_n_eudr.
    WHEN zcl_eudr_utils=>lc_n_eudr OR space.
**<<<------"163355 - NMS - FIM------>>>
      IF wa_filtro_remetente-retornar_eudr IS INITIAL. "Lista tambem notas EUDR?

        APPEND VALUE #( sign   = 'I'
                        option = 'NE'
                        low    = zcl_eudr_utils=>lc_s_eudr ) TO r_eudr.

      ENDIF.
  ENDCASE.

  r_docnum = VALUE #(
       FOR ws_lin IN p_documentos (
           sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
           option = zcl_les_utils=>if_stab_constants~mc_option_equal
           low    = ws_lin-docnum ) ).

  zcl_eudr_utils=>check_doc_fiscal_eudr(
    EXPORTING
      i_docnum_t    =  r_docnum
    IMPORTING
      e_docnum_eudr =  DATA(lit_docnum_eudr) ).

  DELETE lit_docnum_eudr WHERE eudr NOT IN r_eudr.

  FREE r_docnum.

  r_docnum =  VALUE #( FOR ws_docnum IN lit_docnum_eudr (
                           sign   = 'I'
                           option = 'EQ'
                           low    = ws_docnum-docnum ) ).

  DELETE p_documentos WHERE docnum NOT IN r_docnum.
**<<<------"163355 - NMS - INI------>>>
*  LOOP AT  p_documentos ASSIGNING FIELD-SYMBOL(<fs_documet>).
*    READ TABLE lit_docnum_eudr INTO DATA(lwa_docnum_eudr) WITH KEY docnum = <fs_documet>-docnum.
*    IF sy-subrc EQ 0.
*      <fs_documet>-eudr = lwa_docnum_eudr-eudr.
*    ENDIF.
*  ENDLOOP.
  IF p_documentos[] IS INITIAL.
    uv_del_reg = abap_on.

  ENDIF.
**<<<------"163355 - NMS - FIM------>>>
ENDFORM.
"WBARBOSA 23102024 - US-153330 <<----

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_REME_NOTAS_DISPONIVEL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_alv_reme_notas_disponivel OUTPUT.

  PERFORM plan_cria_notas_disponivel.

ENDMODULE.                 " CRIA_ALV_REME_NOTAS_DISPONIVEL  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_REME_NOTAS_DISPONIVEL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_alv_vinc_lotes OUTPUT.

  PERFORM: alv_vinc_lotes,
*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Início de Alteração
           ajustar_saldo_nf.
*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Fim de Alteração


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_NOTAS_DISPONIVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_vinc_lotes .

  TYPES:
    BEGIN OF ty_rem_notas,
      docnum        TYPE znom_reme_notas-docnum,
      nr_quantidade TYPE znom_reme_notas-nr_quantidade,
    END OF ty_rem_notas.

  TYPES:
    BEGIN OF ty_j_1bnflin,
      docnum TYPE j_1bnflin-docnum,
      docdat TYPE j_1bnfdoc-docdat,
      itmnum TYPE j_1bnflin-itmnum,
      werks  TYPE j_1bnflin-werks,
      cfop   TYPE j_1bnflin-cfop,
      menge  TYPE j_1bnflin-menge,
    END OF ty_j_1bnflin,

    BEGIN OF ty_retlote_sum,
      docnum TYPE zsdt_retlote-docnum,
      qtd    TYPE zsdt_retlote-quant_vinc,
    END OF ty_retlote_sum,

    BEGIN OF ty_rem_saldo,
      docnum TYPE j_1bnflin-docnum,
      saldo  TYPE menge_d,
    END OF ty_rem_saldo.

  DATA: lt_j_1bnflin   TYPE TABLE OF ty_j_1bnflin,
        lt_retlote_sum TYPE TABLE OF ty_retlote_sum,
        ls_retlote_sum TYPE ty_retlote_sum,
        lt_rem_saldo   TYPE TABLE OF ty_rem_saldo,
        ls_rem_saldo   TYPE ty_rem_saldo.
**<<<------"163355 - NMS - INI------>>>
  DATA: lr_docnum      TYPE rsis_t_range.
**<<<------"163355 - NMS - FIM------>>>
  CONSTANTS: tabela_vinc_lotes TYPE string VALUE 'IT_ALV_VINC_LOTES'.

  DATA: text_n000 TYPE c LENGTH 50 VALUE 'Nr.SAP',
        text_n001 TYPE c LENGTH 50 VALUE 'Nr.Item',
        text_n002 TYPE c LENGTH 50 VALUE 'Nr. RFL',
        text_n003 TYPE c LENGTH 50 VALUE 'Item RFL',
        text_n004 TYPE c LENGTH 50 VALUE 'Filial RFL',
        text_n005 TYPE c LENGTH 50 VALUE 'QTD RFL',
        text_n006 TYPE c LENGTH 50 VALUE 'Saldo RFL',
        text_n007 TYPE c LENGTH 50 VALUE 'Data RFL',
        text_n009 TYPE c LENGTH 50 VALUE 'EUDR',            "<<<------"163355 - NMS------>>>
        text_n010 TYPE c LENGTH 50 VALUE 'Dt. CHEGADA CCT', "<<<------"165835 - NMS------>>>
        text_n008 TYPE c LENGTH 50 VALUE 'CFOP RFL'.

  DATA: vg_ttb_button         TYPE ttb_button,
        ls_layout             TYPE lvc_s_layo,
        lt_fieldcat           TYPE lvc_t_fcat,
        lv_qtd                TYPE zsdt_retlote-quant_vinc,
        lv_saldo              TYPE zsdt_retlote-quant_vinc,
        lt_rem_notas          TYPE TABLE OF ty_rem_notas,
        ls_alv_vinc_lotes     TYPE ty_alv_vinc_lotes,
        ls_alv_vinc_lotes_aux TYPE ty_alv_vinc_lotes,
        lv_docnum_eprod_ant   TYPE j_1bnfdoc-docnum,
        lv_qtd_rem            TYPE zsdt_retlote-quant_vinc,
        lv_qtd_rem2           TYPE zsdt_retlote-quant_vinc,
        lv_pular              TYPE c,
        lv_sair               TYPE c,
        lt_docnum_prod        TYPE zsdt_itmnum.

  FREE: it_alv_vinc_lotes.

  IF it_znom_notasfiscais_alv[] IS NOT INITIAL.

    MOVE-CORRESPONDING it_znom_notasfiscais_alv[] TO lt_docnum_prod.

    CALL FUNCTION 'ZSDF_BUSCA_SALDO_RFL'
      EXPORTING
        i_docnum_prod     = lt_docnum_prod
**<<<------"163355 - NMS - INI------>>>
        i_cod_ra_embarque = wa_filtro_remetente-codigo_ra_embarque
        i_eudr            = wa_filtro_remetente-due_eudr
        i_retornar_eudr   = wa_filtro_remetente-retornar_eudr
**<<<------"163355 - NMS - FIM------>>>
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
        i_tp_vinc1        = wa_filtro_remetente-tp_vinc1
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração
      IMPORTING
        e_rfl             = it_alv_vinc_lotes.
**<<<------"163355 - NMS - INI------>>>
* Monta o Range de Consulta das Notas de Saídas para muscar os seus respectivos EUFR.
    lr_docnum = VALUE #( FOR el_lin IN it_alv_vinc_lotes ( sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
                                                           option = zcl_les_utils=>if_stab_constants~mc_option_equal
                                                           low    = el_lin-docnum_rfl
                                                          )
                       ).
* Consolta dos EUDRs das Notas de Saídas.
    zcl_eudr_utils=>check_doc_fiscal_eudr( EXPORTING
                                             i_docnum_t    = lr_docnum
                                           IMPORTING
                                             e_docnum_eudr = DATA(tl_docnum_eudr)
                                          ).
**<<<------"163355 - NMS - FIM------>>>

    LOOP AT it_alv_vinc_lotes ASSIGNING FIELD-SYMBOL(<fs_alv_vinc_lotes>).
**<<<------"163355 - NMS - INI------>>>
*      READ TABLE it_znom_notasfiscais_alv INTO DATA(ls_znom_notasfiscais_alv) WITH KEY docnum = <fs_alv_vinc_lotes>-docnum.
*      IF sy-subrc IS INITIAL.
*        <fs_alv_vinc_lotes>-eudr = ls_znom_notasfiscais_alv-eudr.
*      ENDIF.
      DATA(vl_tabix) = sy-tabix.
      READ TABLE tl_docnum_eudr INTO DATA(el_docnum_eudr) WITH KEY docnum = <fs_alv_vinc_lotes>-docnum_rfl.
      IF sy-subrc IS INITIAL.
        <fs_alv_vinc_lotes>-eudr = el_docnum_eudr-eudr.

      ELSE.
        CLEAR <fs_alv_vinc_lotes>-eudr.

      ENDIF.
**<<<------"163355 - NMS - FIM------>>>

    ENDLOOP.
*
*    DATA(lt_notas_fiscais_alv) = it_znom_notasfiscais_alv[].
*    SORT lt_notas_fiscais_alv BY docnum.
*    DELETE ADJACENT DUPLICATES FROM lt_notas_fiscais_alv COMPARING docnum.
*
*    SELECT *
*      FROM zsdtvinc_p_flote
*      INTO TABLE @DATA(lt_flote)
*      FOR ALL ENTRIES IN @lt_notas_fiscais_alv
*      WHERE docnum_eprod = @lt_notas_fiscais_alv-docnum.
*    IF sy-subrc IS INITIAL.
*      SORT lt_flote BY docnum_flote docnum_eprod ASCENDING.
*
*      DATA(lt_flote_aux) = lt_flote.
*      SORT lt_flote_aux BY docnum_flote.
*      DELETE ADJACENT DUPLICATES FROM lt_flote_aux COMPARING docnum_flote.
*
*      SELECT *
*        FROM zsdtvinc_p_flote
*        INTO TABLE @DATA(lt_flote2)
*        FOR ALL ENTRIES IN @lt_flote_aux
*        WHERE docnum_flote = @lt_flote_aux-docnum_flote.
*      IF sy-subrc IS INITIAL.
*        SORT lt_flote2 BY docnum_flote docnum_eprod ASCENDING.
*
*        DATA(lt_flote3_aux) = lt_flote2.
*        SORT lt_flote3_aux BY docnum_eprod.
*        DELETE ADJACENT DUPLICATES FROM lt_flote3_aux COMPARING docnum_eprod.
*        SELECT *
*          FROM zsdtvinc_p_flote
*          INTO TABLE @DATA(lt_flote3)
*          FOR ALL ENTRIES IN @lt_flote3_aux
*          WHERE docnum_eprod = @lt_flote3_aux-docnum_eprod.
*        IF sy-subrc IS INITIAL.
*          SORT lt_flote3 BY docnum_flote docnum_eprod ASCENDING.
*
*          lt_flote3_aux = lt_flote3.
*          SORT lt_flote3_aux BY docnum_eprod.
*          DELETE ADJACENT DUPLICATES FROM lt_flote3_aux COMPARING docnum_eprod.
*
*          SELECT docnum nr_quantidade
*            FROM znom_reme_notas AS a
*            INNER JOIN znom_remetente AS b
*            ON a~id_nomeacao_tran = b~id_nomeacao_tran
*           AND a~id_material = b~id_material
*           AND a~id_remetente = b~id_remetente
*           AND a~grp_retorno = b~grp_retorno
*            INTO TABLE lt_rem_notas
*            FOR ALL ENTRIES IN lt_flote3_aux
*            WHERE a~docnum = lt_flote3_aux-docnum_eprod
*              AND b~docnum_rt = space.
*          IF sy-subrc IS INITIAL.
*            SORT lt_rem_notas BY docnum.
*          ENDIF.
*
*          SELECT a~docnum a~docdat b~itmnum b~werks b~cfop b~menge
*            FROM j_1bnfdoc AS a
*            INNER JOIN j_1bnflin AS b
*            ON a~docnum = b~docnum
*            INTO TABLE lt_j_1bnflin
*            FOR ALL ENTRIES IN lt_flote3_aux
*            WHERE a~docnum = lt_flote3_aux-docnum_flote.
*
*        ENDIF.
*
*      ENDIF.
*
*      SELECT *
*        FROM zsdt_retlote
*        INTO TABLE @DATA(lt_retlote)
*        FOR ALL ENTRIES IN @lt_flote_aux
*        WHERE docnum = @lt_flote_aux-docnum_flote.
*      IF sy-subrc IS INITIAL.
*        SORT lt_retlote BY docnum.
*
*        LOOP AT lt_retlote ASSIGNING FIELD-SYMBOL(<fs_retlote>).
*
*          ls_retlote_sum-docnum = <fs_retlote>-docnum.
*          ls_retlote_sum-qtd    = <fs_retlote>-quant_vinc.
*
*          COLLECT ls_retlote_sum INTO lt_retlote_sum.
*          CLEAR ls_retlote_sum.
*
*        ENDLOOP.
*      ENDIF.
*
*      DATA(lt_flote_aux2) = lt_flote.
*      SORT lt_flote_aux2 BY docnum_eprod.
*      DELETE ADJACENT DUPLICATES FROM lt_flote_aux2 COMPARING docnum_eprod.
*
*      SELECT docnum nr_quantidade
*        FROM znom_reme_notas AS a
*        INNER JOIN znom_remetente AS b
*        ON a~id_nomeacao_tran = b~id_nomeacao_tran
*       AND a~id_material = b~id_material
*       AND a~id_remetente = b~id_remetente
*       AND a~grp_retorno = b~grp_retorno
*        APPENDING TABLE lt_rem_notas
*        FOR ALL ENTRIES IN lt_flote_aux2
*        WHERE a~docnum = lt_flote_aux2-docnum_eprod
*          AND b~docnum_rt = space.
*      IF sy-subrc IS INITIAL.
*        SORT lt_rem_notas BY docnum.
*      ENDIF.
*
*    ENDIF.
*
*    IF lt_flote_aux IS NOT INITIAL.
*
*      SELECT a~docnum a~docdat b~itmnum b~werks b~cfop b~menge
*        FROM j_1bnfdoc AS a
*        INNER JOIN j_1bnflin AS b
*        ON a~docnum = b~docnum
*        APPENDING TABLE lt_j_1bnflin
*        FOR ALL ENTRIES IN lt_flote_aux
*        WHERE a~docnum = lt_flote_aux-docnum_flote.
*      IF sy-subrc IS INITIAL.
*        SORT lt_j_1bnflin BY docnum.
*
*        LOOP AT lt_flote ASSIGNING FIELD-SYMBOL(<fs_flote>).
*
*          READ TABLE lt_j_1bnflin ASSIGNING FIELD-SYMBOL(<fs_j_1bnflin>)
*          WITH KEY docnum = <fs_flote>-docnum_flote
*          BINARY SEARCH.
*          IF sy-subrc IS INITIAL.
*
*            READ TABLE lt_notas_fiscais_alv ASSIGNING FIELD-SYMBOL(<fs_notas_fiscais_alv>)
*            WITH KEY docnum = <fs_flote>-docnum_eprod
*            BINARY SEARCH.
*            IF sy-subrc IS INITIAL.
*              ls_alv_vinc_lotes-docnum = <fs_notas_fiscais_alv>-docnum.
*              ls_alv_vinc_lotes-itmnum = <fs_notas_fiscais_alv>-itmnum.
*            ENDIF.
*
*            ls_alv_vinc_lotes-docnum_rfl = <fs_flote>-docnum_flote.
*            ls_alv_vinc_lotes-item_rfl = <fs_j_1bnflin>-itmnum.
*            ls_alv_vinc_lotes-filial_rfl = <fs_j_1bnflin>-werks.
*            ls_alv_vinc_lotes-cfop_rfl = <fs_j_1bnflin>-cfop.
*            ls_alv_vinc_lotes-qtd_rfl  = <fs_j_1bnflin>-menge.
*            ls_alv_vinc_lotes-data_rfl  = <fs_j_1bnflin>-docdat.
*
*            READ TABLE it_alv_vinc_lotes ASSIGNING FIELD-SYMBOL(<fs_alv_vinc_lotes>)
*            WITH KEY docnum_rfl = <fs_flote>-docnum_flote.
*            IF sy-subrc IS NOT INITIAL.
*
*              READ TABLE lt_retlote_sum ASSIGNING FIELD-SYMBOL(<fs_retlote_sum>)
*              WITH KEY docnum = <fs_flote>-docnum_flote
*              BINARY SEARCH.
*              IF sy-subrc IS INITIAL.
*                lv_saldo = <fs_j_1bnflin>-menge - <fs_retlote_sum>-qtd.
*              ELSE.
*                lv_saldo = <fs_j_1bnflin>-menge.
*              ENDIF.
*
*              IF lv_docnum_eprod_ant IS NOT INITIAL AND
*                 lv_docnum_eprod_ant EQ <fs_flote>-docnum_eprod
*                 AND lv_qtd_rem <= 0.
*
*                lv_pular = abap_true.
*                CLEAR lv_qtd_rem.
*
*              ENDIF.
*
*              IF lv_pular IS INITIAL.
*
*                IF lv_saldo > 0.
*
*                  READ TABLE lt_rem_notas ASSIGNING FIELD-SYMBOL(<fs_rem_notas>)
*                  WITH KEY docnum = <fs_flote>-docnum_eprod
*                  BINARY SEARCH.
*                  IF sy-subrc IS INITIAL.
*
*                    lv_saldo = lv_saldo - <fs_rem_notas>-nr_quantidade.
*                    IF lv_qtd_rem IS INITIAL.
*                      lv_qtd_rem = <fs_rem_notas>-nr_quantidade - <fs_j_1bnflin>-menge.
*                    ELSE.
*                      lv_qtd_rem = lv_qtd_rem - <fs_j_1bnflin>-menge.
*                    ENDIF.
*
*
*                    IF lv_saldo <= 0.
*
*                      CLEAR lv_saldo.
*                      CONTINUE.
*
*                    ENDIF.
*
*                  ENDIF.
*
*                ELSE.
*
*                  "Não exibir no alv
*                  CONTINUE.
*
*                ENDIF.
*
*              ENDIF.
*
**** Busca de quantidade para outros eprods
*              DATA(lt_vinc2) = lt_flote2.
*              DELETE lt_vinc2 WHERE docnum_flote <> <fs_flote>-docnum_flote OR
*                                    docnum_eprod EQ <fs_flote>-docnum_eprod.
*
*              LOOP AT lt_vinc2 ASSIGNING FIELD-SYMBOL(<fs_vinc2>).
*
*                READ TABLE lt_flote3 TRANSPORTING NO FIELDS
*                WITH KEY docnum_eprod = <fs_vinc2>-docnum_eprod
*                BINARY SEARCH.
*                IF sy-subrc IS INITIAL.
*                  LOOP AT lt_flote3 ASSIGNING FIELD-SYMBOL(<fs_flote3>) FROM sy-tabix.
*                    IF <fs_flote3>-docnum_eprod <> <fs_vinc2>-docnum_eprod OR
*                       lv_sair = abap_true.
*                      CLEAR lv_sair.
*                      EXIT.
*                    ENDIF.
*
*                    READ TABLE lt_j_1bnflin ASSIGNING <fs_j_1bnflin>
*                    WITH KEY docnum = <fs_flote3>-docnum_flote
*                    BINARY SEARCH.
*                    IF sy-subrc IS INITIAL.
*                      READ TABLE lt_rem_notas ASSIGNING <fs_rem_notas>
*                      WITH KEY docnum = <fs_flote3>-docnum_eprod
*                      BINARY SEARCH.
*                      IF sy-subrc IS INITIAL.
*                        IF lv_qtd_rem2 IS INITIAL.
*                          lv_qtd_rem2 = <fs_rem_notas>-nr_quantidade.
*                        ENDIF.
*
*                        ls_rem_saldo-docnum = <fs_flote>-docnum_flote.
*
*                        READ TABLE lt_rem_saldo ASSIGNING FIELD-SYMBOL(<fs_rem_saldo>)
*                        WITH KEY docnum =  <fs_flote>-docnum_flote.
*                        IF sy-subrc IS INITIAL.
*
*                          <fs_rem_saldo>-saldo = <fs_rem_saldo>-saldo - lv_qtd_rem2.
*
*                          lv_qtd_rem2 = lv_qtd_rem2 - <fs_rem_saldo>-saldo.
*
*                        ELSE.
*
*                          ls_rem_saldo-saldo = <fs_j_1bnflin>-menge - lv_qtd_rem2.
*                          APPEND ls_rem_saldo TO lt_rem_saldo.
*                          CLEAR ls_rem_saldo.
*
*                          lv_qtd_rem2 = lv_qtd_rem2 - <fs_j_1bnflin>-menge.
*
*                        ENDIF.
*
*                        IF lv_qtd_rem2 <= 0.
*                          lv_sair = abap_true.
*                        ENDIF.
*                      ENDIF.
*
*                    ENDIF.
*
*                  ENDLOOP.
*
*                  CLEAR lv_qtd_rem2.
*
*                ENDIF.
*
*              ENDLOOP.
*
*              READ TABLE lt_rem_saldo ASSIGNING <fs_rem_saldo>
*              WITH KEY docnum = <fs_flote>-docnum_flote.
*              IF sy-subrc IS INITIAL.
*                lv_saldo = <fs_rem_saldo>-saldo.
*              ENDIF.
*
*            ELSE.
*              lv_saldo = <fs_alv_vinc_lotes>-saldo_rfl.
*            ENDIF.
*
*          ENDIF.
*
*          IF lv_saldo <= 0.
*
*            CLEAR lv_saldo.
*            CONTINUE.
*
*          ENDIF.
*
*          ls_alv_vinc_lotes-saldo_rfl = lv_saldo.
*
*          APPEND ls_alv_vinc_lotes TO it_alv_vinc_lotes.
*          CLEAR: ls_alv_vinc_lotes,
*                 lv_saldo,
*                 lv_pular.
*
*          lv_docnum_eprod_ant = <fs_flote>-docnum_eprod.
*
*        ENDLOOP.
*
*      ENDIF.
*
*    ENDIF.

  ENDIF.

  IF plan_alv_vinc_lotes IS INITIAL.

    CREATE OBJECT go_cc_vinc_lotes
      EXPORTING
        container_name = 'CC_VINC_LOTES'.

    CREATE OBJECT go_alv_vinc_lotes
      EXPORTING
        i_parent = go_cc_vinc_lotes.

*  CREATE OBJECT toolbar_notasfiscais_alv
*    EXPORTING
*      io_alv_grid = plan_alv_notasfiscais.

*  SET HANDLER toolbar_notasfiscais_alv->on_toolbar FOR plan_alv_notasfiscais.
*  SET HANDLER toolbar_notasfiscais_alv->handle_user_command FOR plan_alv_notasfiscais.

    PERFORM z_estrutura_fieldcat TABLES lt_fieldcat USING:
        tabela_vinc_lotes 'DOCNUM'            text_n000 'X' 01 10 space space   space 'X'   space space space,
        tabela_vinc_lotes 'ITMNUM'            text_n001 'X' 02 06 space space   space 'X'   space space space,
        tabela_vinc_lotes 'DOCNUM_RFL'        text_n002 'X' 03 10 space space   space 'X'   'X'   space space,
        tabela_vinc_lotes 'ITEM_RFL'          text_n003 ' ' 04 06 space space   space space space space space,
        tabela_vinc_lotes 'FILIAL_RFL'        text_n004 ' ' 05 04 space space   space space space space space,
        tabela_vinc_lotes 'QTD_RFL'           text_n005 ' ' 06 15 space space   space space space space space,
        tabela_vinc_lotes 'SALDO_RFL'         text_n006 ' ' 07 15 space space   space space space space space,
        tabela_vinc_lotes 'DATA_RFL'          text_n007 ' ' 08 10 space space   space space space space space,
**<<<------"163355 - NMS - INI------>>>
*        tabela_vinc_lotes 'CFOP_RFL'          text_n008 ' ' 08 10 space space   space space space space space.
        tabela_vinc_lotes 'CFOP_RFL'          text_n008 ' ' 08 10 space space   space space space space space,
        tabela_vinc_lotes 'EUDR'              text_n009 ' ' 09 05 space space   space space space space space.
**<<<------"163355 - NMS - FIM------>>>
**<<<------"165835 - NMS - INI------>>>
    PERFORM z_estrutura_fieldcat TABLES lt_fieldcat USING:
        tabela_vinc_lotes 'DT_RECEPCAO'       text_n010 ' ' 10 10 space space   space space space space space.
**<<<------"165835 - NMS - FIM------>>>
    CLEAR: plan_gs_layout.
    ls_layout-zebra    = c_x.
    ls_layout-sel_mode = c_a.
    ls_layout-info_fname = 'LINE_COLOR'.

    CALL METHOD go_alv_vinc_lotes->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = ls_layout
      CHANGING
        it_fieldcatalog = lt_fieldcat
        it_outtab       = it_alv_vinc_lotes.

*   Create Object for Event Handler
    CREATE OBJECT plan_event_handler_notas.
    SET HANDLER plan_event_handler_notas->handle_hotspot_click_notas
            FOR go_alv_vinc_lotes.

    plan_alv_vinc_lotes = abap_true.

  ENDIF.

  CALL METHOD go_alv_vinc_lotes->refresh_table_display.

  CALL METHOD go_alv_vinc_lotes->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_NOTAS_DISPONIVEL

*&---------------------------------------------------------------------*
*&      Form  ALV_VINC_LOTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM plan_cria_notas_disponivel.

  CONSTANTS: tabela_notasfiscais TYPE string VALUE 'IT_ZNOM_NOTASFISCAIS_ALV'.

  DATA: text_n000 TYPE c LENGTH 50 VALUE 'CT-e RFL', "CS2019000714
        text_n001 TYPE c LENGTH 50 VALUE 'Nr.SAP',
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
        text_n013 TYPE c LENGTH 50 VALUE 'Qtd.Disponível',
        text_n014 TYPE c LENGTH 50 VALUE 'Qtd. Utilizada',
        text_n015 TYPE c LENGTH 50 VALUE 'Qtd. Saldo',
        text_n016 TYPE c LENGTH 50 VALUE 'Dt. Emissão',
        text_n017 TYPE c LENGTH 50 VALUE 'Dt. Chegada',
        text_n018 TYPE c LENGTH 50 VALUE 'UF',
        text_n019 TYPE c LENGTH 50 VALUE 'Saldo CCT',
        text_n020 TYPE c LENGTH 50 VALUE 'Dif.Peso.CCT x NF',
        text_n021 TYPE c LENGTH 50 VALUE 'Qtd.NF',
        text_n022 TYPE c LENGTH 50 VALUE 'Peso CCT',
        text_n023 TYPE c LENGTH 50 VALUE 'Qtd.Utlz.CCT',
        text_n024 TYPE c LENGTH 50 VALUE 'NF.Chegada Porto',
        text_n025 TYPE c LENGTH 50 VALUE 'Docnum RFL',
        text_n026 TYPE c LENGTH 50 VALUE 'CCT RFL',
        text_n027 TYPE c LENGTH 50 VALUE 'Rom.Compl.',
        text_n028 TYPE c LENGTH 50 VALUE 'Restrição Uso',
        text_n029 TYPE c LENGTH 50 VALUE 'Entrada Prop.',
        text_n030 TYPE c LENGTH 50 VALUE 'Ind.RFL.',
        text_n031 TYPE c LENGTH 50 VALUE 'Peso Disp.CCT',
        text_n032 TYPE c LENGTH 50 VALUE 'EUDR'.  "// WBARBOSA 28102024 US-153330

  DATA: vg_ttb_button TYPE ttb_button.

  IF plan_prim_notasfiscais IS INITIAL.

    CREATE OBJECT plan_container_notasfiscais
      EXPORTING
        container_name = 'CTN_REME_NOTAS_LIVRES'.

    CREATE OBJECT plan_alv_notasfiscais
      EXPORTING
        i_parent = plan_container_notasfiscais.

    CREATE OBJECT toolbar_notasfiscais_alv
      EXPORTING
        io_alv_grid = plan_alv_notasfiscais.

    SET HANDLER toolbar_notasfiscais_alv->on_toolbar FOR plan_alv_notasfiscais.
    SET HANDLER toolbar_notasfiscais_alv->handle_user_command FOR plan_alv_notasfiscais.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_notasfiscais USING:
        tabela_notasfiscais 'ICONE'            text_n000 'X' 01 11 space space   space 'X'   space space             space,
        tabela_notasfiscais 'DOCNUM'           text_n001 'X' 02 11 space 'ALPHA' space 'X'   space c_grid_color_c200 space,
        tabela_notasfiscais 'ITMNUM'           text_n002 ' ' 03 06 space space   space 'X'   'X'   space             space,
        tabela_notasfiscais 'BRANCH'           text_n003 ' ' 04 04 space space   space space space c_grid_color_c200 space,
        tabela_notasfiscais 'MODEL'            text_n004 ' ' 05 03 space space   space space space space             space,
        tabela_notasfiscais 'SERIES'           text_n005 ' ' 06 03 space space   space space space c_grid_color_c200 space,
        tabela_notasfiscais 'NFENUM'           text_n006 ' ' 07 09 space space   space space space c_grid_color_c200 space,
        tabela_notasfiscais 'QTDE_NF'          text_n021 ' ' 08 15 space space   'X'   space space c_grid_color_c500 space,
        tabela_notasfiscais 'PESO_CCT'         text_n031 ' ' 09 15 space space   'X'   space space c_grid_color_c500 space,
        tabela_notasfiscais 'NR_QUANTIDADE2'   text_n013 ' ' 10 15 space space   'X'   space space c_grid_color_c300 space,
        tabela_notasfiscais 'NR_UTILIZADA'     text_n014 ' ' 11 15 space space   'X'   space space c_grid_color_c400 space,
        tabela_notasfiscais 'NR_UTILIZADA_CCT' text_n023 ' ' 12 15 space space   'X'   space space c_grid_color_c400 space,
        tabela_notasfiscais 'SALDO_CCT'        text_n019 ' ' 13 15 space space   'X'   space space c_grid_color_c500 space,
        tabela_notasfiscais 'NR_SALDO'         text_n015 ' ' 14 15 space space   'X'   space space c_grid_color_c500 space,
        tabela_notasfiscais 'DIF_PESO_CCT_NF'  text_n020 ' ' 15 15 space space   'X'   space space c_grid_color_c500 space,
        tabela_notasfiscais 'NF_CHEGADA_PORTO' text_n024 ' ' 16 16 space space   space space space space             space,
        tabela_notasfiscais 'DOCDAT'           text_n016 ' ' 17 10 space space   space space space space             space,
        tabela_notasfiscais 'PARID'            text_n007 ' ' 18 10 space 'ALPHA' space space space space             space,
        tabela_notasfiscais 'NAME1'            text_n008 ' ' 19 25 space space   space space space space             space,
        tabela_notasfiscais 'EUDR'             text_n032 ' ' 20 04 space space   space space space space             space, "// WBARBOSA 28102024 US-153330
        tabela_notasfiscais 'REGIO'            text_n018 ' ' 21 03 space space   space space space space             space,
        tabela_notasfiscais 'MATNR'            text_n009 ' ' 22 10 space 'ALPHA' space space space space             space,
        tabela_notasfiscais 'MAKTX'            text_n010 ' ' 23 25 space space   space space space space             space,
        tabela_notasfiscais 'CHARG'            text_n011 ' ' 24 05 space space   space space space space             space,
        tabela_notasfiscais 'CFOP'             text_n012 ' ' 25 07 space 'CFOBR' space space space space             space,
        tabela_notasfiscais 'DT_CHEGADA'       text_n017 ' ' 26 10 space space   space space space space             space,
        tabela_notasfiscais 'ROM_COMPLETO'     text_n027 ' ' 27 10 space space   space space 'C'   space             space,
        tabela_notasfiscais 'DOCNUM_RFL'       text_n025 'X' 28 11 space 'ALPHA' space space space space             space,
        tabela_notasfiscais 'CCT_RFL'          text_n026 ' ' 29 08 space space   space space 'C'   space             space,
        tabela_notasfiscais 'IND_RFL'          text_n030 ' ' 30 08 space space   space space 'C'   space             space,
        tabela_notasfiscais 'RESTRICAO'        text_n028 ' ' 31 80 space space   space space space space             space,
        tabela_notasfiscais 'ENTRAD'           text_n029 ' ' 32 13 space space   space space 'C'   space             space,
        tabela_notasfiscais 'PESO_AFERIDO_CCT' text_n022 ' ' 33 15 space space   'X'   space space c_grid_color_c500 space.


    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra    = c_x.
    plan_gs_layout-sel_mode = c_a.
    plan_gs_layout-info_fname = 'LINE_COLOR'.

    CALL METHOD plan_alv_notasfiscais->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = plan_gs_layout
      CHANGING
        it_fieldcatalog = plan_catalogo_notasfiscais
        it_outtab       = it_znom_notasfiscais_alv[].

*   Create Object for Event Handler
    CREATE OBJECT plan_event_handler_notas.
    SET HANDLER plan_event_handler_notas->handle_hotspot_click_notas
            FOR plan_alv_notasfiscais.

    plan_prim_notasfiscais = c_x.

  ENDIF.

  CALL METHOD plan_alv_notasfiscais->refresh_table_display.

  CALL METHOD plan_alv_notasfiscais->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_NOTAS_DISPONIVEL
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0031  INPUT
*&---------------------------------------------------------------------*
*       Comandos de Vinculação e Desvinculação
*----------------------------------------------------------------------*
MODULE user_command_0031 INPUT.
  DATA:it_selected_rows    TYPE lvc_t_row,
       wa_selected_rows    TYPE lvc_s_row,
       lv_docnum           TYPE znom_remetente-docnum_rt,
       lv_fieldname        TYPE rmdi_name,
       lv_nr_ordem         TYPE znom_remetente-nr_ordem,
       lv_qtd              TYPE znom_remetente-nr_programada,
       wl_remetente        LIKE LINE OF it_znom_remetente_alv,
       wl_znom_remet       TYPE znom_remetente,
       lv_qtd2             TYPE c LENGTH 16, "j_1bnetqty,
       lv_data             TYPE c LENGTH 10,
       lv_erro             TYPE i,
       lv_auart            TYPE vbak-auart,
       v_erro_vinc         TYPE c,
       v_msg_vinc          TYPE string,
       v_qtde_dist         TYPE znom_reme_notas-nr_quantidade,
       it_rfl_zsdt0008     TYPE zsdt0001_ro_vinc_t WITH HEADER LINE,
       it_zsdt0001_ro_vinc TYPE zsdt0001_ro_vinc_t,
       v_comcct            TYPE c,
       vl_retorno_manual   TYPE c,
       v_semcct            TYPE c,
       v_allcct            TYPE c,
       it_lips_rec         TYPE TABLE OF lips WITH HEADER LINE,
       v_ov_not_recusada   TYPE c,
       v_tp_nf_rem         TYPE znom_reme_notas-tp_nf_rem,
       v_id_due            TYPE zsdt0170-id_due,
       v_tcode             TYPE syst-tcode,
       v_id_produtor       TYPE znom_remetente-id_remetente,
       lv_salesdocument    LIKE bapivbeln-vbeln.
**<<<------"145379 - NMS - INI------>>>
  TYPES: BEGIN OF ty_rnd_docnum,
           sg_opt TYPE char3,
           docnum TYPE j_1bdocnum,
         END   OF ty_rnd_docnum.

  DATA: tl_rfl_sld_fisico TYPE          zsdt_rfl_saldo_fisico,
        tl_docnum_prod    TYPE          zsdt_itmnum,
        tl_notas_rfl      TYPE          zde_nota_retorno_rfl_t,
        tl_nota_sel       TYPE          zde_nota_retorno_rfl_sel_t,
        tl_str            TYPE TABLE OF ty_rnd_docnum.

  DATA: rl_docnum         TYPE RANGE OF j_1bdocnum.

  DATA: el_itmnum   TYPE zsd_itmnum,
        el_nota_sel	TYPE zde_nota_retorno_rfl_sel,
        el_str      TYPE ty_rnd_docnum.

  CONSTANTS: cl_ieq TYPE char3 VALUE 'IEQ'.
**<<<------"145379 - NMS - FIM------>>>
  IF ok_code_0001 = 'ENTER'.

    IF wl_remetente-cvirt IS NOT INITIAL.

    ENDIF.

  ENDIF.


  CASE ok_code_0001.
    WHEN 'VER_LOG'.
      vg_troca_notas = 'X'.
    WHEN ok_vinc.
      CLEAR: ok_code_0001.
      PERFORM vincular_notas.
** Limpa campo tela
      CLEAR wa_filtro_remetente-grp_retorno.
      LEAVE TO SCREEN 0001.
    WHEN ok_desv.
      CLEAR: ok_code_0001.
      PERFORM desvincular_notas.
    WHEN ok_comp.
      CLEAR: ok_code_0001.
      PERFORM vincular_saldo_filial.
    WHEN ok_dlog. "Substituir notas
      CLEAR: ok_code_0001.
      PERFORM desvincular_notlog.
    WHEN ok_tab31.
      CLEAR: ok_code_0001.
      vg_dynnr_30xx    = vg_dynnr_0034.
      tabreme-activetab = ok_tab31.
    WHEN ok_tab32.
      CLEAR: ok_code_0001.
      vg_dynnr_30xx    = vg_dynnr_0032.
      tabreme-activetab = ok_tab32.
    WHEN ok_tab33.
      CLEAR: ok_code_0001.
      vg_dynnr_30xx    = vg_dynnr_0038.
      tabreme-activetab = ok_tab33.
    WHEN ok_lib_ov_rec.

      CHECK plan_alv_remetente IS NOT INITIAL.

      CALL METHOD plan_alv_remetente->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      CHECK it_selected_rows[] IS NOT INITIAL.

      IF lines( it_selected_rows[] ) NE 1.
        MESSAGE 'Selecione somente uma linha!' TYPE 'I'.
        RETURN.
      ENDIF.

      READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
      CHECK sy-subrc EQ 0.

      READ TABLE it_znom_remetente_alv INTO wa_znom_remetente_alv INDEX wa_selected_rows-index.

      CHECK ( sy-subrc EQ 0 ) AND ( wa_znom_remetente_alv-nr_ordem IS NOT INITIAL ).

      CLEAR: it_lips_rec[].

      v_ov_not_recusada = abap_false.

      SELECT *
        FROM lips INTO TABLE it_lips_rec
       WHERE vgbel EQ wa_znom_remetente_alv-nr_ordem.

      CHECK it_lips_rec[] IS NOT INITIAL.

      LOOP AT it_lips_rec.

        SELECT SINGLE *
          FROM zdoc_exp_recusa INTO @DATA(_wl_zdoc_exp_recusa)
         WHERE vbeln_re_exp   = @it_lips_rec-vbeln
           AND nm_quantidade  > 0.

        IF sy-subrc NE 0.
          v_ov_not_recusada = abap_true.
        ENDIF.

      ENDLOOP.

      IF v_ov_not_recusada EQ abap_true.
        MESSAGE 'O.V não foi recusada!' TYPE 'I'.
        RETURN.
      ENDIF.

      LOOP AT it_lips_rec WHERE vbeln IS NOT INITIAL.

        UPDATE zsdt_retlote SET vbeln = space
         WHERE vbeln = it_lips_rec-vbeln.

      ENDLOOP.

      UPDATE znom_remetente SET nr_ordem = space
       WHERE nr_ordem = wa_znom_remetente_alv-nr_ordem.

      wa_znom_remetente_alv-nr_ordem = space.

      MODIFY it_znom_remetente_alv FROM wa_znom_remetente_alv INDEX wa_selected_rows-index TRANSPORTING nr_ordem.

      CALL METHOD plan_alv_remetente->refresh_table_display.

      MESSAGE 'O.V de Recusa Liberada!' TYPE 'I'.

    WHEN ok_criar_ret.
      CLEAR: lv_erro, wl_remetente, lv_qtd, v_id_due,  v_tp_nf_rem, v_erro_vinc, v_msg_vinc, it_rfl_zsdt0008[], wl_znom_remet, lv_docnum, lv_nr_ordem, lv_salesdocument.

      IF plan_alv_remetente IS NOT INITIAL.
        CALL METHOD plan_alv_remetente->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        IF it_selected_rows[] IS NOT INITIAL.

          FREE: tl_nota_sel. "US 145379 - NMS

          LOOP AT it_selected_rows INTO wa_selected_rows WHERE index IS NOT INITIAL.
            READ TABLE it_znom_remetente_alv INTO wa_znom_remetente_alv INDEX wa_selected_rows-index.
            IF wl_remetente IS INITIAL.
              MOVE-CORRESPONDING wa_znom_remetente_alv TO wl_remetente.
              MOVE-CORRESPONDING wa_znom_remetente_alv TO wl_znom_remet.

              DATA(_change) = abap_false.
              PERFORM f_check_reg_rem_vinc USING wl_znom_remet
                                        CHANGING _change.
              IF _change EQ abap_true.
                MESSAGE 'Quantidades foram reajustadas! Revisar quantidades do Grupo selecionado' TYPE 'I'.
                RETURN.
              ENDIF.

            ELSE.
****      Valida se os rementente são equivalente, o retorno só pode ser gerado em grupos
****      onde todos possuem o campo id_rementente preenchido ou entre os que possuem o mesmo vazio.
              IF ( wl_remetente-id_remetente IS INITIAL
                   AND wa_znom_remetente_alv-id_remetente IS NOT INITIAL )
              OR ( wl_remetente-id_remetente IS NOT INITIAL
                   AND wa_znom_remetente_alv-id_remetente IS INITIAL ).
                ADD 1 TO lv_erro.
              ENDIF.
            ENDIF.
***       Valida se remetente já possui retorno
            IF wa_znom_remetente_alv-docnum_rt IS NOT INITIAL.
              ADD 1 TO lv_erro.
            ENDIF.
***         Valida Grupo de retorno
            IF wl_remetente-grp_retorno <> wa_znom_remetente_alv-grp_retorno.
              ADD 1 TO lv_erro.
            ENDIF.
**        Soma o quantidade dos remetente selecionados para gerar o retorno.
            ADD wa_znom_remetente_alv-nr_programada TO lv_qtd.

            "Pré seleciona formações de lote que devem ser selecionadas
            IF ( v_erro_vinc EQ abap_false ) AND ( wa_znom_remetente_alv-id_remetente IS NOT INITIAL ).

              LOOP AT it_znom_reme_notas_alv WHERE id_nomeacao_tran EQ wa_znom_remetente_alv-id_nomeacao_tran
                                               AND id_empresa       EQ wa_znom_remetente_alv-id_empresa
                                               AND id_filial        EQ wa_znom_remetente_alv-id_filial
                                               AND id_material      EQ wa_znom_remetente_alv-id_material
                                               AND id_remetente     EQ wa_znom_remetente_alv-id_remetente
                                               AND grp_retorno      EQ wa_znom_remetente_alv-grp_retorno
                                               AND tp_nf_rem        EQ 'F'.  "Vinculado Remessa Formação Lote
**<<<------"145379 - NMS - INI------>>>
*                CALL FUNCTION 'ZROMANEIO_VINCULADO_NF'
*                  EXPORTING
*                    i_docnum            = it_znom_reme_notas_alv-docnum
*                    i_ck_vinc_zmemo00   = abap_true
*                    i_ck_cfop_e_zmemo00 = abap_true
*                  IMPORTING
*                    e_zsdt0001_ro_vinc  = it_zsdt0001_ro_vinc.
*
*                IF it_zsdt0001_ro_vinc[] IS INITIAL.
                IF sy-tabix EQ 1.
                  DATA(vl_data_ini) = CONV datum( |{ it_znom_reme_notas_alv-docdat(6) && '01' ALPHA = IN }| ).

                ENDIF.

                CLEAR: tl_docnum_prod, el_itmnum.
                el_itmnum-docnum = it_znom_reme_notas_alv-docnum.
                el_itmnum-itmnum = it_znom_reme_notas_alv-itmnum.

*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
                READ TABLE it_nf_doc INTO DATA(ls_znom_nf)
                                     WITH KEY docnum = it_znom_reme_notas_alv-docnum
                                                                       BINARY SEARCH.
                IF sy-subrc IS INITIAL.

                  el_itmnum-nf_chegada_porto = ls_znom_nf-nf_chegada_porto.

                ENDIF.
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração

                APPEND el_itmnum TO tl_docnum_prod.
* Função de Busca saldo de formação de lote - Físico.
                CALL FUNCTION 'ZSDF_BUSCA_SALDO_RFL'
                  EXPORTING
                    i_docnum_prod      = tl_docnum_prod
**<<<------"165835 - NMS - INI------>>>
                    i_cod_ra_embarque  = wa_znom_remetente_alv-codigo_ra_embarque
                    i_eudr             = wa_znom_remetente_alv-eudr
**<<<------"165835 - NMS - FIM------>>>
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
                    i_tp_vinc1         = wa_filtro_remetente-tp_vinc1
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração
                  IMPORTING
                    e_rfl_saldo_fisico = tl_rfl_sld_fisico.

                IF tl_rfl_sld_fisico[] IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
                  v_erro_vinc = abap_true.
                  v_msg_vinc  = 'Não foi possível identificar a Rem.Formação de Lote do Documento: ' && it_znom_reme_notas_alv-docnum.
                  EXIT.
**<<<------"145379 - NMS - INI------>>>
                ELSE.
                  LOOP AT tl_rfl_sld_fisico INTO DATA(el_rfl_sld_fisico) WHERE docnum EQ it_znom_reme_notas_alv-docnum.
                    el_nota_sel-docnum = el_rfl_sld_fisico-docnum_rfl.
* Verifica a Quantidade da do retorno com a quantidade do Saldo Vinculado da Vinculação.
                    IF it_znom_reme_notas_alv-nr_quantidade LE el_rfl_sld_fisico-qtd_vinc.
                      el_nota_sel-qtde_vinc = it_znom_reme_notas_alv-nr_quantidade.
                      COLLECT el_nota_sel INTO tl_nota_sel.
                      CLEAR: el_nota_sel.
                      EXIT.

                    ELSE.
                      el_nota_sel-qtde_vinc = el_rfl_sld_fisico-qtd_vinc.
                      it_znom_reme_notas_alv-nr_quantidade = it_znom_reme_notas_alv-nr_quantidade - el_rfl_sld_fisico-qtd_vinc.
                      COLLECT el_nota_sel INTO tl_nota_sel.
                      CLEAR: el_nota_sel.
                    ENDIF.
                  ENDLOOP.
**<<<------"145379 - NMS - FIM------>>>
                ENDIF.

                CLEAR: v_qtde_dist.
                v_qtde_dist = it_znom_reme_notas_alv-nr_quantidade.
**<<<------"145379 - NMS - INI------>>>
*                LOOP AT it_zsdt0001_ro_vinc INTO DATA(_wl_ro_vinc).
*                  CLEAR: it_rfl_zsdt0008.
*                  MOVE-CORRESPONDING _wl_ro_vinc TO it_rfl_zsdt0008.
*                  CLEAR: it_rfl_zsdt0008-peso_liq.
*                  it_rfl_zsdt0008-peso_liq = ( _wl_ro_vinc-prop_vinc * it_znom_reme_notas_alv-nr_quantidade ) / 100.
*                  APPEND it_rfl_zsdt0008.
*                ENDLOOP.
**<<<------"145379 - NMS - FIM------>>>
              ENDLOOP.


              LOOP AT it_znom_reme_notas_alv WHERE id_nomeacao_tran EQ wa_znom_remetente_alv-id_nomeacao_tran
                                               AND id_empresa       EQ wa_znom_remetente_alv-id_empresa
                                               AND id_filial        EQ wa_znom_remetente_alv-id_filial
                                               AND id_material      EQ wa_znom_remetente_alv-id_material
                                               AND id_remetente     EQ wa_znom_remetente_alv-id_remetente
                                               AND grp_retorno      EQ wa_znom_remetente_alv-grp_retorno
                                               AND id_remetente     IS NOT INITIAL.
                IF v_tp_nf_rem IS INITIAL.
                  v_tp_nf_rem  = it_znom_reme_notas_alv-tp_nf_rem.
                  v_id_due     = it_znom_reme_notas_alv-id_due.
                ELSE.
                  IF v_tp_nf_rem NE it_znom_reme_notas_alv-tp_nf_rem.
                    v_erro_vinc = abap_true.
                    v_msg_vinc  = 'Existem mais de um Tipo de NF Remente vinculado no grupo!'.
                    EXIT.
                  ENDIF.

                  IF v_id_due NE it_znom_reme_notas_alv-id_due.
                    v_erro_vinc = abap_true.
                    v_msg_vinc  = 'Existem mais de uma DU-e vinculada no grupo!'.
                    EXIT.
                  ENDIF.
                ENDIF.
              ENDLOOP.
            ENDIF.

          ENDLOOP.

          IF lv_erro IS NOT INITIAL.
            MESSAGE 'Os remententes/grupos selecionados não são equivalentes/ou já possui retorno.' TYPE 'E'.
            EXIT.
          ENDIF.

          IF v_erro_vinc EQ abap_true.
            MESSAGE v_msg_vinc TYPE 'E'.
            EXIT.
          ENDIF.

          IF sy-subrc = 0.
            CLEAR: it_dta[].

            MOVE lv_qtd TO lv_qtd2.
            TRANSLATE lv_qtd2 USING '.,'.
            CONDENSE lv_qtd2 NO-GAPS.

            "Definições RFL
            CLEAR: v_comcct, v_semcct, v_allcct.

            IF wl_remetente-id_remetente IS INITIAL. "Proprio
              v_id_due = wl_remetente-id_due.
              v_comcct = abap_true. "Só selecionar Notas do RFL que estão no CCT
              vl_retorno_manual = abap_true. "// wbarbosa 07112024 US-156375
            ELSE.
              IF v_tp_nf_rem IS INITIAL.
                MESSAGE 'Tipo NF. Remetente não foi atribuido!' TYPE 'E'.
                RETURN.
              ENDIF.

              IF ( v_tp_nf_rem NE c_f ) AND
                 ( v_tp_nf_rem NE c_c ) AND
                 ( v_tp_nf_rem NE c_s ).
                MESSAGE 'Tipo NF. Remetente inválido!' TYPE 'E'.
                RETURN.
              ENDIF.

              CASE v_tp_nf_rem.
                WHEN c_f. "Vinculo Rem. Formação Lote
**<<<------"145379 - NMS - INI------>>>
*                  IF it_rfl_zsdt0008[] IS INITIAL.
                  IF tl_nota_sel[] IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
                    MESSAGE 'Não encontrado nenhuma Remessa Formação Lote vinculado as Entradas! Verifique a opção de Vinculo!' TYPE 'E'.
                    RETURN.
                  ENDIF.
**<<<------"145379 - NMS - INI------>>>
*                  "Exportar Documentos de RFL para memória
*                  EXPORT it_rfl_zsdt0008 TO MEMORY ID 'IT_RFL_ZSDT0008'.
*                IF znom_remetente-retorno_com_nf_de_terceiro IS INITIAL. "// WBARBOSA 07112024 - US-156375 Removido e incluido apos a seleção das notas
* Busca o terminal de embarque.
                  SELECT SINGLE lifnr FROM zsdt0168 INTO @DATA(vl_terminal) WHERE codigo_ra EQ @wa_znom_remetente_alv-codigo_ra_embarque.
* Consulta as NFs de entrada para carregar na classe de Retorno Simbolico para o prcessamento.
                  " Inicio - Rubenilson - 01.10.2025 #192188
                  DATA(lt_notas_reme) = it_znom_reme_notas_alv[].
                  SORT lt_notas_reme BY docdat.
                  READ TABLE lt_notas_reme ASSIGNING FIELD-SYMBOL(<fs_notas_reme>) INDEX 1.
                  IF sy-subrc IS INITIAL.
                    vl_data_ini = CONV datum( |{ <fs_notas_reme>-docdat(6) && '01' ALPHA = IN }| ).
                  ENDIF.
                  " Fim - Rubenilson - 01.10.2025 #192188

                  CALL METHOD zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->selecionar_notas(
                    EXPORTING
                      i_bukrs      = wa_znom_remetente_alv-id_empresa
                      i_fkart      = 'ZRFL'
                      i_werks      = wa_znom_remetente_alv-id_filial
                      i_charg      = wa_znom_remetente_alv-safra
                      i_kunnr      = CONV kunnr( |{ wa_znom_remetente_alv-id_filial ALPHA = IN }| )
                      i_terminal   = vl_terminal
                      i_matnr      = wa_znom_remetente_alv-id_material
                      i_status_cct = abap_off
                      i_dt_ini_emi = vl_data_ini
                      i_dt_fim_emi = sy-datum
                    IMPORTING
                      e_notas      = tl_notas_rfl ).

                  MOVE-CORRESPONDING tl_nota_sel TO tl_str.
                  el_str-sg_opt = cl_ieq.
                  MODIFY tl_str FROM el_str TRANSPORTING sg_opt WHERE sg_opt NE cl_ieq.
                  CLEAR el_str.
                  rl_docnum = tl_str.
                  SORT rl_docnum.

                  DELETE tl_notas_rfl WHERE docnum NOT IN rl_docnum.
                  CLEAR rl_docnum.

                  IF tl_notas_rfl[] IS INITIAL.
                    MESSAGE 'Não encontrado nenhuma Remessa Formação Lote vinculado as Entradas! Verifique a opção de Vinculo!' TYPE 'E'.
                    RETURN.

                  ELSE.

*"// Valida o campo Retorno será gerada com NF de RFL de Terceiro.
                    IF wa_znom_remetente_alv-retorno_com_nf_de_terceiro IS INITIAL. "// WBARBOSA 07112024 - US-156375
*"// Instancia a classe de Retorno Simbolico para o prcessamento.
                      zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->novo_lancamento( ).
                      CLEAR el_nota_sel.
                      LOOP AT tl_nota_sel INTO el_nota_sel.
                        READ TABLE tl_notas_rfl INTO DATA(el_notas_rfl) WITH KEY docnum = el_nota_sel-docnum.

                        IF sy-subrc IS INITIAL.
                          TRY.
                              zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->set_qtde_vinc_nf( i_nota_vinc = el_nota_sel ).
                            CATCH zcx_controle_retorno_rfl INTO DATA(zcxl_controle_rfl).
                              zcxl_controle_rfl->published_erro( EXPORTING i_msgty = 'E' i_msgty_display = 'W' ).
                              DATA(lv_text) = zcxl_controle_rfl->get_text( ).
                              MESSAGE lv_text TYPE 'E'.
                              RETURN.

                          ENDTRY.

                        ELSE.
                          lv_text = 'Documento Fiscal de RFL não esta disponivel para retorno simbolico! #' && el_nota_sel-docnum.
                          MESSAGE lv_text TYPE 'E'.
                          RETURN.

                        ENDIF.

                      ENDLOOP.

                      TRY.
                          CLEAR lv_docnum.
                          lv_docnum = zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->gerar_retorno(
                                                                EXPORTING i_check_exists_ret_finalidade = abap_false
                                                                          i_dt_retorno = sy-datum
                                                                          i_finalidade = sy-abcde+4(1) "E - Exportação
                                                                          i_parceiro = el_notas_rfl-lifnr_z1 ).

                          IF lv_docnum IS INITIAL.
                            MESSAGE 'Nota de retorno não gerada' TYPE 'I'.

                          ELSE.
                            PERFORM f_enviar_sefaz IN PROGRAM zsdr0112 USING lv_docnum.

                          ENDIF.

                        CATCH zcx_controle_retorno_rfl INTO zcxl_controle_rfl.
                          zcxl_controle_rfl->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).

                        CATCH zcx_nf_writer INTO DATA(zcxl_nf_writer).
                          zcxl_nf_writer->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).

                      ENDTRY.

                    ELSE.
**<<<------"145379 - NMS - FIM------>>>
                      v_allcct = abap_true.
**<<<------"145379 - NMS - INI------>>>

*"// wbarbosa 07112024 - US-156375
                      it_rfl_zsdt0008[] = VALUE #( FOR ls_notas_rfl IN tl_notas_rfl (
                      ch_referencia = sy-tabix
                      docnum_vinc   = ls_notas_rfl-docnum
                      peso_liq      = ls_notas_rfl-qtde_vinc
                       ) ).

                      EXPORT it_rfl_zsdt0008 TO MEMORY ID 'IT_RFL_ZSDT0008'.
*"// wbarbosa 07112024 - US-156375

                    ENDIF.

                  ENDIF.

**<<<------"145379 - NMS - FIM------>>>
                WHEN c_c. "Se Notas do Produtor estão no CCT
                  v_semcct = abap_true. "Só selecionar Notas do RFL que não estão no CCT
                  vl_retorno_manual = abap_true. "// WBARBOSA 07112024 US-156375
                WHEN c_s. " "Se Notas do Produtor não estão no CCT
                  v_comcct = abap_true. "Só selecionar Notas do RFL que estão no CCT
                  vl_retorno_manual = abap_true. "// WBARBOSA 07112024 US-156375
              ENDCASE.
            ENDIF.
**<<<------"145379 - NMS - INI------>>>
* Valida o campo Retorno será gerada com NF de RFL de Terceiro, se o o remetente está vazio e se não
* gerada a nota de retorno.
            IF ( NOT wa_znom_remetente_alv-retorno_com_nf_de_terceiro IS INITIAL   OR
*                    Comentado a condição para aplicar a variavel vl_retorno_manual                 "// WBARBOSA 07112024 US-156735
*                    wl_remetente-id_remetente                 IS INITIAL ) AND    "Não Proprio     "// WBARBOSA 07112024 US-156735
                     vl_retorno_manual IS NOT INITIAL ) AND                                         "// WBARBOSA 07112024 US-156735
                     lv_docnum                                 IS INITIAL.
**<<<------"145379 - NMS - FIM------>>>
              v_tcode        = sy-tcode.
              v_id_produtor  = wl_remetente-id_remetente.

              EXPORT v_comcct                   TO MEMORY ID 'P_COMCCT'.
              EXPORT v_semcct                   TO MEMORY ID 'P_SEMCCT'.
              EXPORT v_allcct                   TO MEMORY ID 'P_ALLCCT'.
              EXPORT v_id_due                   TO MEMORY ID 'P_ID_DUE'.
              EXPORT v_tcode                    TO MEMORY ID 'P_TCODE'.
              EXPORT v_id_produtor              TO MEMORY ID 'P_ID_PRODUTOR'.

              PERFORM f_bdc_data USING:
                    ''          ''      'T' 'ZSDT0008'      '',
                    'ZSDI0001'  '1000'  'X' ''              '',
                    ''          ''      ''  'P_BUKRS'       wa_filtro_remetente-empresa,
                    ''          ''      ''  'P_FKART'       'ZRFL',
                    ''          ''      ''  'P_WERKS'       wa_filtro_remetente-centro,
                    ''          ''      ''  'S_LGORT-LOW'   'ARMZ',
                    ''          ''      ''  'S_SAFRA-LOW'   wa_znom_remetente_alv-safra,
                    ''          ''      ''  'S_KUNNR-LOW'   wa_filtro_remetente-centro,
                    ''          ''      ''  'S_MATNR-LOW'   wa_filtro_remetente-material,
                    ''          ''      ''  'P_FINAL'       'E',
                    ''          ''      ''  'P_QUANT'       lv_qtd2.

              IF v_semcct EQ abap_true.
                PERFORM f_bdc_data USING:
                    ''          ''      ''  'P_CCT_CP'    abap_true.
              ENDIF.

              PERFORM f_call_transaction USING 'ZSDT0008'
                                               'E'
                                               'S'.
**<<<------"145379 - NMS - INI------>>>
*            DELETE FROM MEMORY ID 'IT_RFL_ZSDT0008'.
**<<<------"145379 - NMS - FIM------>>>
              READ TABLE it_msg WITH KEY msgtyp = 'I'
                                         msgnr  = '836'
                                         msgid  = 'SD'
                                         dynumb = '0100'
                                         dyname = 'ZSDI0001'.

              IF sy-subrc IS INITIAL.
                CLEAR wl_remetente.
                MOVE it_msg-msgv2 TO lv_docnum.

                SELECT COUNT(*)
                  FROM j_1bnfdoc
                  WHERE docnum = lv_docnum.

                IF sy-subrc IS NOT INITIAL.
                  DELETE FROM zsdt_export WHERE  docnum = lv_docnum.
                  DELETE FROM zsdt_retlote WHERE docnum_ret = lv_docnum.
                  MESSAGE |Nota de retorno não gerada, Tentar Novamente!| TYPE 'I'.
                  EXIT.
                ENDIF.
**<<<------"145379 - NMS - INI------>>>
*                LOOP AT it_selected_rows INTO wa_selected_rows WHERE index IS NOT INITIAL.
*                  READ TABLE it_znom_remetente_alv INTO wl_remetente INDEX wa_selected_rows-index.
*
*                  MOVE lv_docnum TO wl_remetente-docnum_rt.
*                  MODIFY it_znom_remetente_alv FROM wl_remetente INDEX wa_selected_rows-index TRANSPORTING docnum_rt.
*
***            Atualiza Tabela
*                  UPDATE znom_remetente SET docnum_rt = lv_docnum
*                   WHERE id_remetente     = wl_remetente-id_remetente AND
*                         id_material      = wl_remetente-id_material AND
*                         id_filial        = wl_remetente-id_filial AND
*                         id_empresa       = wl_remetente-id_empresa AND
*                         id_nomeacao_tran = wl_remetente-id_nomeacao_tran AND
*                         grp_retorno      = wl_remetente-grp_retorno.
*                ENDLOOP.
*
*                CALL METHOD plan_alv_remetente->refresh_table_display.
**<<<------"145379 - NMS - FIM------>>>
              ELSE.
                MESSAGE 'Nota de retorno não gerada' TYPE 'I'.

              ENDIF.
**<<<------"145379 - NMS - INI------>>>
            ENDIF.

            IF NOT lv_docnum IS INITIAL.
              LOOP AT it_selected_rows INTO wa_selected_rows WHERE index IS NOT INITIAL.
                READ TABLE it_znom_remetente_alv INTO wl_remetente INDEX wa_selected_rows-index.

                MOVE lv_docnum TO wl_remetente-docnum_rt.
                MODIFY it_znom_remetente_alv FROM wl_remetente INDEX wa_selected_rows-index TRANSPORTING docnum_rt.
* Atualiza Tabela de Remetente
                UPDATE znom_remetente SET docnum_rt = lv_docnum
                 WHERE id_remetente     = wl_remetente-id_remetente AND
                       id_material      = wl_remetente-id_material AND
                       id_filial        = wl_remetente-id_filial AND
                       id_empresa       = wl_remetente-id_empresa AND
                       id_nomeacao_tran = wl_remetente-id_nomeacao_tran AND
                       grp_retorno      = wl_remetente-grp_retorno.

                IF sy-subrc IS INITIAL.
                  CLEAR lv_docnum.

                ENDIF.

              ENDLOOP.

              CALL METHOD plan_alv_remetente->refresh_table_display.

            ENDIF.
**<<<------"145379 - NMS - FIM------>>>
          ELSE.
            MESSAGE e836(sd) WITH 'Por favor selecione um remetente.'.
          ENDIF.
        ELSE.
          MESSAGE 'Selecione um remetente.' TYPE 'E'.
        ENDIF.
      ENDIF.
    WHEN ok_nota_fis.
      IF plan_alv_remetente IS NOT INITIAL.
        CALL METHOD plan_alv_remetente->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        LOOP AT it_selected_rows INTO wa_selected_rows WHERE index IS NOT INITIAL.
          READ TABLE it_znom_remetente_alv INTO wa_znom_remetente_alv INDEX wa_selected_rows-index.
          IF sy-subrc = 0.
            CLEAR: it_dta[].

            SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD wa_znom_remetente_alv-docnum_rt.
            SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_znom_remetente_alv-id_empresa.
            CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.

          ELSE.
            MESSAGE e836(sd) WITH 'Por favor selecione um remetente.'.
          ENDIF.
        ENDLOOP.
      ENDIF.
    WHEN ok_criar_ord.
      CLEAR: ok_code_0001.
**    Seleciona o tipo da ordem
      "PERFORM f_sel_tipo_ordem CHANGING lv_auart. "Tipo OV Antigo - DEVK9A1V05 07.02.2024 113637 CS2023000378 ORDEM DE VENDA AUTOM

      " DEVK9A1V05 07.02.2024 113637 CS2023000378 ORDEM DE VENDA AUTOM
      CHECK plan_alv_remetente IS NOT INITIAL.

      CALL METHOD plan_alv_remetente->get_selected_rows
        IMPORTING
          et_index_rows = it_selected_rows.

      IF lines( it_selected_rows[] ) NE 1.
        MESSAGE 'Selecione pelo menos uma linha!' TYPE 'I'.
        RETURN.
      ENDIF.

      READ TABLE it_selected_rows INTO wa_selected_rows INDEX 1.
      CHECK sy-subrc EQ 0.

      READ TABLE it_znom_remetente_alv INTO wa_znom_remetente_alv INDEX wa_selected_rows-index.

      CHECK ( sy-subrc EQ 0 ).
      " DEVK9A1V05 07.02.2024 113637 CS2023000378 ORDEM DE VENDA AUTOM


      lv_auart = wa_znom_remetente_alv-tipov.
      IF lv_auart IS NOT INITIAL AND ( lv_auart = 'ZEXI' OR lv_auart = 'ZEXP'  OR lv_auart = 'ZEXD'  ).

        IF NOT wa_znom_remetente_alv-docnum_rt IS INITIAL.
          SELECT nf_retorno UP TO 1 ROWS
                 FROM zsdt_export
                 INTO @DATA(lv_nf_retorno)
                 WHERE docnum EQ @wa_znom_remetente_alv-docnum_rt.
          ENDSELECT.
        ENDIF.

        IF NOT wa_znom_remetente_alv-docnum_rt IS INITIAL AND NOT lv_nf_retorno IS INITIAL.

          " DEVK9A1V05 - 07.02.2024 113637 CS2023000378 ORDEM DE VENDA AUTOM PSA

          READ TABLE it_znom_reme_notas INTO DATA(st_znom_reme_notas) WITH KEY id_nomeacao_tran = wa_znom_remetente_alv-id_nomeacao_tran
                                                                               id_filial        = wa_znom_remetente_alv-id_filial
                                                                               id_material      = wa_znom_remetente_alv-id_material
                                                                               id_remetente     = wa_znom_remetente_alv-id_remetente
                                                                               grp_retorno      = wa_znom_remetente_alv-grp_retorno.
          IF sy-subrc EQ 0.
            DATA(vl_codigo_ra_embarque) = st_znom_reme_notas-codigo_ra_embarque.
            CLEAR: st_znom_reme_notas.
          ENDIF.


          IF sy-subrc NE  0."BUG GERAÇÃO ov AUTOMATICA -LP - ZEXP NÃO TEM NOTA DE PRODUTOR

            READ TABLE it_znom_remetente INTO DATA(st_znom_reme)    WITH KEY id_nomeacao_tran = wa_znom_remetente_alv-id_nomeacao_tran
                                                                              id_filial        = wa_znom_remetente_alv-id_filial
                                                                              id_material      = wa_znom_remetente_alv-id_material
                                                                              "id_remetente     = wa_znom_remetente_alv-id_remetente
                                                                              grp_retorno      = wa_znom_remetente_alv-grp_retorno.

            IF sy-subrc EQ 0.
              vl_codigo_ra_embarque = st_znom_reme-codigo_ra_embarque.
              CLEAR: st_znom_reme.
            ENDIF.

          ENDIF.


          IF NOT vl_codigo_ra_embarque IS INITIAL.

            SELECT SINGLE lifnr
                   FROM zsdt0168
                   INTO @DATA(vl_lifnr)
                   WHERE codigo_ra EQ @vl_codigo_ra_embarque.

          ENDIF.

          "====Comenado inicio - USER STORY 73032 - Anderson Oenning  16/05/2022
          CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO lv_data SEPARATED BY '.'.
          CLEAR: it_dta[].
*        PERFORM f_bdc_data USING:
*              ''          ''      'T' 'VA01'            '',
*              'SAPMV45A'  '0101'  'X' ''                '',
*              ''          ''      ''  'BDC_CURSOR'      'VBAK-AUART',
*              ''          ''      ''  'BDC_OKCODE'      '=COPY',
*              ''          ''      ''  'VBAK-AUART'      lv_auart,
*              'SAPLV45C'  '0100'  'X' ''                '',
*              ''          ''      ''  'BDC_OKCODE'      '=REF1',
*              ''          ''      ''  'BDC_SUBSCR'      'SAPLV45C          0302SUB1',
*              ''          ''      ''  'BDC_CURSOR'      'LV45C-VBELN',
*              ''          ''      ''  'LV45C-VBELN'     wa_filtro_remetente-contrato,
*              ''          ''      ''  'LV45C-KETDAT'    lv_data,
*              ''          ''      ''  'LV45C-KPRGBZ'    'D',
*              'SAPLV45C'  '0100'  'X' ''                '',
*              ''          ''      ''  'BDC_OKCODE'      '=UEBR',
*              ''          ''      ''  'BDC_SUBSCR'      'SAPLV45C          0302SUB1',
*              ''          ''      ''  'BDC_CURSOR'      'LV45C-VBELN',
*              ''          ''      ''  'LV45C-VBELN'     wa_filtro_remetente-contrato,
*              ''          ''      ''  'LV45C-KETDAT'    lv_data,
*              ''          ''      ''  'LV45C-KPRGBZ'    'D'.
*
*        PERFORM f_call_transaction USING 'VA01'
*                                         'E'
*                                         'S'.
*
*        READ TABLE it_msg WITH KEY msgtyp = 'S'
*                                   msgnr  = '311'.

*====Incluído estrutura BAPI.
**      Buscar parâmetros de contrato
          SELECT SINGLE *
            FROM znom_param_cont
            INTO wl_param
            WHERE bukrs = wa_znom_programacao_alv-id_empresa.

*        Busca valor do dólar para o dia
          CLEAR: obj_zcl_util_sd.
          CREATE OBJECT obj_zcl_util_sd.

          lv_data_atual = sy-datum.

          obj_zcl_util_sd->set_data(  EXPORTING i_data  = lv_data_atual ). "TCURR-GDATU

*        OBJ_ZCL_UTIL_SD->SET_KURST( EXPORTING I_KURST = 'B' ).

          obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'G' ).    " TCURR-KURST
          obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'USD' ).  " TCURR-FCURR
          obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'BRL' ).  " TCURR-TCURR
          obj_zcl_util_sd->taxa_cambio(  RECEIVING e_ukurs = lv_ukurs ).
**      Valor taxa de câmbio

          DATA: qtd              TYPE char15,
                lv_qtd_efetivada TYPE j_1bnetqty,
                lv_campos_inc    TYPE boolean.

          "DATA(material) = |{ wa_znom_programacao_alv-id_material ALPHA = OUT }|.
          DATA(material) = wa_znom_remetente_alv-id_material.

          " Soma Quantiade efetivada por Grupo de retorno
          CLEAR lv_qtd_efetivada.
          LOOP AT it_znom_remetente_alv INTO DATA(st_znom_remetente_alv) WHERE grp_retorno = wa_znom_remetente_alv-grp_retorno.
            lv_qtd_efetivada = lv_qtd_efetivada + st_znom_remetente_alv-nr_programada.
          ENDLOOP.
          CLEAR: qtd.
          "qtd =  wa_znom_transporte_alv-nr_qtde_programada.
          qtd = lv_qtd_efetivada.

          lv_ukurs_ = lv_ukurs.
          "REPLACE ALL OCCURRENCES OF '.' IN qtd WITH ','.
          "REPLACE ALL OCCURRENCES OF '.' IN lv_ukurs_ WITH ','.
          "CONDENSE: qtd, lv_ukurs_.


          TYPES: BEGIN OF ty_log_ov,
                   vbeln   TYPE vbuv-vbeln,
                   posnr   TYPE vbuv-posnr,
                   tbnam   TYPE char30,
                   fdnam   TYPE char30,
                   fehgr   TYPE char02,
                   statg   TYPE char02,
                   message TYPE char100,
                 END   OF ty_log_ov.


          DATA: wa_header_in      TYPE STANDARD TABLE OF bapisdhd1 WITH HEADER LINE,
                wa_bape_vbak      TYPE bape_vbak,
                wa_bape_vbakx     TYPE bape_vbakx,
                lt_items          TYPE STANDARD TABLE OF bapisditm WITH HEADER LINE,
                lt_parceiro       TYPE STANDARD TABLE OF bapiparnr WITH HEADER LINE,
                it_retorno        TYPE TABLE OF bapiret2 INITIAL SIZE 0,
                lt_conditions_in  TYPE TABLE OF bapicond   WITH HEADER LINE,
                lt_conditions_inx TYPE TABLE OF bapicondx  WITH HEADER LINE,
                lt_schedules_in   TYPE TABLE OF bapischdl  WITH HEADER LINE,
                lt_schedules_inx  TYPE TABLE OF bapischdlx WITH HEADER LINE,
                lt_bapiparex      TYPE TABLE OF bapiparex WITH HEADER LINE,
                wl_header_inx2    TYPE  bapisdh1x,
                wl_text           TYPE rmdi_ddtxt,
                tl_return_c       TYPE TABLE OF bapiret2   WITH HEADER LINE,
                tl_log_ov         TYPE TABLE OF ty_log_ov,
                wa_log_ov         TYPE ty_log_ov,
                linha_selecionada TYPE slis_selfield,
                _exit             TYPE c,
                tl_vbuv           TYPE  TABLE OF vbuv WITH HEADER LINE.


          DATA: g_container        TYPE scrfname VALUE 'CC_LOG_OV',
                g_custom_container TYPE REF TO cl_gui_custom_container,
                g_grid             TYPE REF TO cl_gui_alv_grid,
                ok_code            TYPE sy-ucomm,
                l_stable           TYPE lvc_s_stbl,
                w_layout           TYPE lvc_s_layo,
                t_function         TYPE ui_functions,
                w_function         TYPE ui_func,
                ls_fieldcatalog    TYPE lvc_s_fcat,
                tl_fieldcatalog    TYPE lvc_t_fcat.


          " Header
          wa_header_in-accnt_asgn           = '04'.
          wa_header_in-doc_type             = wa_znom_remetente_alv-tipov. "lv_auart.
          wa_header_in-sales_org            = wl_param-bukrs ."VKORG
          wa_header_in-distr_chan           = wl_param-vtweg  ."VTWEG
          wa_header_in-division             = wl_param-spart  ."SPART
          wa_header_in-incoterms1           = wl_param-inco1  ."INCO1
          wa_header_in-incoterms2           = wl_param-inco2  ."INCO2
          wa_header_in-pmnttrms             = 'Z030'  ."DZTERM - SMC
          wa_header_in-price_date           = sy-datum. "lv_data  ."PRSDT
          wa_header_in-purch_no_c           = wa_znom_transporte_alv-ds_nome_transpor."cliente
          wa_header_in-purch_no_s	          =	wa_znom_transporte_alv-ds_nome_transpor	."recebedor mercadoria
          wa_header_in-doc_date             = sy-datum. "lv_data  ."AUDAT
          wa_header_in-exchg_rate           = lv_ukurs_ ."KURSK_P
          wa_header_in-exrate_fi           = lv_ukurs_ ."KURRF_P SMC
          wa_header_in-currency             = wl_param-waerk.
          wa_header_in-pymt_meth            = 'P'.


          " Item
          CLEAR lt_items[].
          lt_items-material   = material.
          lt_items-usage_ind  = wl_param-vkaus.
          lt_items-itm_number = '000010'.
          lt_items-target_qty = qtd.
          lt_items-gross_wght = qtd.
          lt_items-net_weight = qtd.
          lt_items-target_qu  = 'KG'.
          lt_items-sales_unit = 'KG'.
          lt_items-untof_wght = 'KG'.
          lt_items-bill_date  = sy-datum. "lv_data.
          lt_items-batch      = wa_znom_remetente_alv-safra.
          lt_items-plant      = wa_znom_remetente_alv-cvirt.
          lt_items-store_loc  = wa_znom_remetente_alv-depst.
          lt_items-route      =  'FOB'.
          lt_items-volume     = ''.
          lt_items-volunit    = ''.
          APPEND lt_items.

          CLEAR: lt_schedules_in[], lt_schedules_inx[].
          lt_schedules_in-itm_number = lt_items-itm_number.
          lt_schedules_in-req_qty    = qtd.
          APPEND lt_schedules_in.


          " Conditions
          CLEAR: lt_conditions_in[], lt_conditions_inx[].
          lt_conditions_in-itm_number = lt_items-itm_number.
          lt_conditions_in-cond_type  = 'PR00'.
          lt_conditions_in-currency   = wl_param-waerk.
          lt_conditions_in-cond_value = wa_znom_remetente_alv-preco.
          lt_conditions_in-cond_unit  = 'TO'.
          APPEND lt_conditions_in.


          "Parceiros
          CLEAR lt_parceiro[].
          IF NOT vl_lifnr IS INITIAL.
            lt_parceiro-partn_role = 'Z1'.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = vl_lifnr
              IMPORTING
                output = lt_parceiro-partn_numb.
            lt_parceiro-partn_role = 'Z1'.
            APPEND lt_parceiro.
          ENDIF.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wl_param-kunnr
            IMPORTING
              output = lt_parceiro-partn_numb.
          lt_parceiro-partn_role = 'AG'.
          APPEND lt_parceiro.

          lt_parceiro-partn_role = 'RE'.
          APPEND lt_parceiro.

          lt_parceiro-partn_role = 'RG'.
          APPEND lt_parceiro.

          lt_parceiro-partn_role = 'WE'.
          APPEND lt_parceiro.


          " Extension - campo zpesagem
          FREE: tl_bapiparex.

          wa_bape_vbak =
          VALUE #(
                    zpesagem = SWITCH #( wa_header_in-doc_type
                                         WHEN 'ZPER' OR 'ZEXP' OR 'ZEXI' OR 'YV02'
                                         THEN '02'
                                         ELSE '01'
                                       )
                  ).

          wa_bape_vbakx = VALUE #( zpesagem = abap_true ).

          lt_bapiparex[] =
          VALUE #(
                   ( structure = 'BAPE_VBAK'  valuepart1 = wa_bape_vbak )
                   ( structure = 'BAPE_VBAKX' valuepart1 = wa_bape_vbakx )
                   ).


          CLEAR: tl_log_ov[], it_retorno[].

          " Cria Ordem de venda
          CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
            EXPORTING
              sales_header_in      = wa_header_in
            IMPORTING
              salesdocument_ex     = lv_salesdocument
            TABLES
              return               = it_retorno
              sales_items_in       = lt_items
              sales_schedules_in   = lt_schedules_in
              sales_schedules_inx  = lt_schedules_inx
              sales_conditions_in  = lt_conditions_in
              sales_conditions_inx = lt_conditions_inx
              sales_partners       = lt_parceiro
              extensionin          = lt_bapiparex.

          IF lv_salesdocument IS INITIAL.

            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

          ELSE.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            lv_nr_ordem = lv_salesdocument.

          ENDIF.

          CLEAR lv_campos_inc.
          IF NOT lv_nr_ordem IS INITIAL.
            REFRESH: tl_vbuv.
            SELECT *
              FROM vbuv
              INTO TABLE tl_vbuv
               WHERE vbeln EQ lv_nr_ordem.


            IF sy-subrc IS INITIAL.


              LOOP AT tl_vbuv.
                CLEAR wl_text.
                lv_fieldname = tl_vbuv-fdnam.
                CALL FUNCTION 'RM_DDIC_TEXTS_GET'
                  EXPORTING
                    i_name                = lv_fieldname
                    i_type                = 'DTEL'
                    i_langu               = sy-langu
                  IMPORTING
                    e_ddtxt               = wl_text
                  EXCEPTIONS
                    objtype_not_supported = 1
                    illegal_input         = 2
                    OTHERS                = 3.
                IF sy-subrc <> 0.
                  lv_campos_inc = abap_true.
                ELSE.
                  lv_campos_inc = abap_true.
                ENDIF.
                IF NOT wl_text IS INITIAL.
                  wa_log_ov-vbeln   = tl_vbuv-vbeln.
                  wa_log_ov-posnr   = tl_vbuv-posnr.
                  wa_log_ov-tbnam   = tl_vbuv-tbnam.
                  wa_log_ov-fdnam   = tl_vbuv-fdnam.
                  wa_log_ov-fehgr   = tl_vbuv-fehgr.
                  wa_log_ov-statg   = tl_vbuv-statg.
                  wa_log_ov-message = wl_text.
                  APPEND wa_log_ov TO tl_log_ov.
                ENDIF.
              ENDLOOP.
              REFRESH: tl_return_c.
              wl_header_inx2-updateflag = 'D'.
              CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
                EXPORTING
                  salesdocument    = lv_nr_ordem
*                 ORDER_HEADER_IN  = WL_ORDERHEADERIN
                  order_header_inx = wl_header_inx2
                TABLES
                  return           = tl_return_c.
              READ TABLE tl_return_c  WITH KEY type = 'E'.
              IF sy-subrc NE 0.
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.
              ENDIF.

            ENDIF.

          ELSE.
            LOOP AT it_retorno INTO DATA(wa_retorno).
*              wa_log_ov-vbeln   = tl_vbuv-vbeln.
*              wa_log_ov-posnr   = tl_vbuv-posnr.
*              wa_log_ov-tbnam   = tl_vbuv-tbnam.
*              wa_log_ov-fdnam   = tl_vbuv-fdnam.
*              wa_log_ov-fehgr   = tl_vbuv-fehgr.
*              wa_log_ov-statg   = tl_vbuv-statg.
              wa_log_ov-message = wa_retorno-message.
              APPEND wa_log_ov TO tl_log_ov.
              lv_campos_inc = abap_true.
            ENDLOOP.
          ENDIF.



          IF lv_campos_inc EQ abap_true AND NOT tl_log_ov[] IS INITIAL.

            " Log de processamento da OV.
            CALL SCREEN 0072 STARTING AT 34  4
                    ENDING AT 156 15.

            RETURN.
          ENDIF.

          "Nova estrutura de SHDB sem dados contrato.
*        PERFORM f_bdc_data USING:
*'        '  '0000'  'T'   'VA01            '   '                                         ',
*'SAPMV45A'  '0101'  'X'   '                '   '                                         ',
*'        '  '0000'  ' '   'BDC_CURSOR      '   'VBAK-AUART                               ',
*'        '  '0000'  ' '   'BDC_OKCODE      '   '/00                                      ',
*'        '  '0000'  ' '   'VBAK-AUART      '   wl_remetente-tipov,                          "PSA
*'        '  '0000'  ' '   'VBAK-VKORG      '   wl_param-bukrs,
*'        '  '0000'  ' '   'VBAK-VTWEG      '   wl_param-vtweg,
*'        '  '0000'  ' '   'VBAK-SPART      '   wl_param-spart,
*'SAPMV45A'  '4001'  'X'   '                '   '                                         ',
*'        '  '0000'  ' '   'BDC_OKCODE      '   '=HEAD                                    ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4021SUBSCREEN_HEADER  ',
*'        '  '0000'  ' '   'VBKD-BSTKD      '   wa_znom_transporte_alv-ds_nome_transpor,
*'        '  '0000'  ' '   'VBKD-BSTDK      '   lv_data,
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4701PART-SUB          ',
*'        '  '0000'  ' '   'KUAGV-KUNNR     '   wl_param-kunnr,
*'        '  '0000'  ' '   'KUWEV-KUNNR     '   wl_param-kunnr,
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4400SUBSCREEN_BODY    ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4440HEADER_FRAME      ',
*'        '  '0000'  ' '   'RV45A-KETDAT    '   lv_data,
*'        '  '0000'  ' '   'RV45A-KPRGBZ    '   'D                                        ',
*'        '  '0000'  ' '   'VBKD-PRSDT      '   lv_data,
*'        '  '0000'  ' '   'VBKD-ZTERM      '   wl_param-zterm,
*'        '  '0000'  ' '   'VBKD-INCO1      '   wl_param-inco1,
*'        '  '0000'  ' '   'VBKD-INCO2_L    '   wl_param-inco2,
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           8310HEAD_USER         ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPLV45W           0400SUBSCREEN_VERTRAG ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4900SUBSCREEN_TC      ',
*'        '  '0000'  ' '   'BDC_CURSOR      '   'VBAP-VKAUS(01)',
*'        '  '0000'  ' '   'RV45A-MABNR(01) '   material,
*'        '  '0000'  ' '   'RV45A-KWMENG(01)'   qtd,
*'        '  '0000'  ' '   'VBAP-VKAUS(01)  '   wl_param-vkaus,
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4050SUBSCREEN_BUTTONS ',
*'SAPMV45A'  '4002'  'X'   '                '   '                                         ',
*'        '  '0000'  ' '   'BDC_OKCODE      '   '/00                                      ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4012SUBSCREEN_HEADER  ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4301SUBSCREEN_BODY    ',
*'        '  '0000'  ' '   'BDC_CURSOR      '   'VBAK-WAERK',
*'        '  '0000'  ' '   'VBAK-AUDAT      '   lv_data,
*'        '  '0000'  ' '   'VBAK-WAERK      '   wl_param-waerk,
*'        '  '0000'  ' '   'VBKD-PRSDT      '   lv_data,
*'SAPMV45A'  '4002'  'X'   '                '   '                                         ',
*'        '  '0000'  ' '   'BDC_OKCODE      '   '/00                                      ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4012SUBSCREEN_HEADER  ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4301SUBSCREEN_BODY    ',
*'        '  '0000'  ' '   'BDC_CURSOR      '   'VBKD-KURSK',
*'        '  '0000'  ' '   'VBAK-AUDAT      '   lv_data,
*'        '  '0000'  ' '   'VBAK-WAERK      '   wl_param-waerk,
*'        '  '0000'  ' '   'VBKD-KURSK      '   lv_ukurs_,
*'        '  '0000'  ' '   'VBKD-PRSDT      '   lv_data,
*'SAPMV45A'  '4002'  'X'   '                '   '                                         ',
*'        '  '0000'  ' '   'BDC_OKCODE      '   '=T\05                                    ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4012SUBSCREEN_HEADER  ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4301SUBSCREEN_BODY    ',
*'        '  '0000'  ' '   'BDC_CURSOR      '   'VBAK-AUDAT',
*'        '  '0000'  ' '   'VBAK-AUDAT      '   lv_data,
*'        '  '0000'  ' '   'VBAK-WAERK      '   wl_param-waerk,
*'        '  '0000'  ' '   'VBKD-KURSK      '   lv_ukurs_,
*'        '  '0000'  ' '   'VBKD-PRSDT      '   lv_data,
*'SAPMV45A'  '4002'  'X'   '                '   '                                         ',
*'        '  '0000'  ' '   'BDC_OKCODE      '   '/00                                      ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4012SUBSCREEN_HEADER  ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4311SUBSCREEN_BODY    ',
**'        '  '0000'  ' '   'BDC_CURSOR      '   'VBKD-KURRF',
**'        '  '0000'  ' '   'VBKD-KTGRD      '   '04                                       ',
*'        '  '0000'  ' '   'VBKD-KURRF      '   lv_ukurs_,
*'SAPMV45A'  '4002'  'X'   '                '   '                                         ',
*'        '  '0000'  ' '   'BDC_OKCODE      '   '=T\13                                    ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4012SUBSCREEN_HEADER  ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4311SUBSCREEN_BODY    ',
*'        '  '0000'  ' '   'BDC_CURSOR      '   'VBKD-KTGRD',
**'        '  '0000'  ' '   'VBKD-KTGRD      '   '04                                       ',
*'        '  '0000'  ' '   'BDC_CURSOR      '   'VBKD-KURRF                               ',
*'        '  '0000'  ' '   'VBKD-KURRF      '   lv_ukurs_,
*'SAPMV45A'  '4002'  'X'   '                '   '                                         ',
*'        '  '0000'  ' '   'BDC_OKCODE      '   '/EBACK                                   ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4012SUBSCREEN_HEADER  ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           4312SUBSCREEN_BODY    ',
*'        '  '0000'  ' '   'BDC_SUBSCR      '   'SAPMV45A           83098309              ',
*'        '  '0000'  ' '   'BDC_CURSOR      '   'VBAK-ZPESAGEM',
*'        '  '0000'  ' '   'VBAK-ZPESAGEM   '   wl_param-zpesagem.
**'SAPMV45A'  '4001'  'X'   '                '   '                                         ',
**'        '  '0000'  ' '   'BDC_OKCODE      '   '/EABBA'.
*
*        DATA(p_mode) = 'E'.
*        DATA(p_upd)  = 'S'.
*
*        PERFORM f_call_transaction USING 'VA01'
*                                            p_mode
*                                            p_upd.
*
*        READ TABLE it_msg WITH KEY msgtyp = 'S'
*                                   msgnr  = '311'.
*
*
*        IF sy-subrc IS INITIAL.
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = it_msg-msgv2
*            IMPORTING
*              output = lv_nr_ordem.




**        Localiza ordem a vincular
          IF NOT lv_nr_ordem IS INITIAL.
            CLEAR  lv_docnum.
            SELECT SINGLE docnum
              INTO lv_docnum
              FROM zsdt_export
              WHERE ordem = lv_nr_ordem.
          ENDIF.

**        Atualiza tabela
          IF lv_docnum IS NOT INITIAL.
            UPDATE znom_remetente SET nr_ordem = lv_nr_ordem
            WHERE docnum_rt = lv_docnum.
          ENDIF.

**        Atualiza ALV
          CLEAR wl_remetente.
          LOOP AT it_znom_remetente_alv INTO wl_remetente FROM wa_selected_rows-index.
            IF wl_remetente-docnum_rt = lv_docnum.
              MOVE lv_nr_ordem TO wl_remetente-nr_ordem.
            ENDIF.
            MODIFY it_znom_remetente_alv FROM wl_remetente INDEX sy-tabix TRANSPORTING nr_ordem.
          ENDLOOP.
          CALL METHOD plan_alv_remetente->refresh_table_display.
*        ELSE.
*          MESSAGE 'A ordem não foi gerada.' TYPE 'E'.
*        ENDIF.
        ELSE.
          MESSAGE 'DocNum RT não gerado/determinado!' TYPE 'I'.
        ENDIF.
      ELSE.
        MESSAGE 'Tipo de OV. inválido.' TYPE 'E'.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0031  INPUT

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_NOTAS
*&---------------------------------------------------------------------*
*       Vincular notas selecionadas
*----------------------------------------------------------------------*
FORM vincular_notas .

  DATA: it_cfops_comercializacao TYPE TABLE OF lxhme_range_c10, "// wbarbosa 07112024 - US-156375
        it_cfops_fim_espeficido  TYPE TABLE OF lxhme_range_c10. "// wbarbosa 07112024 - US-156375

  DATA: it_selected_rows   TYPE lvc_t_row,
        wa_selected_rows   TYPE lvc_s_row,
        vg_tabix           TYPE sy-tabix,
        vg_tabix_2         TYPE sy-tabix,
        nr_qtd_vinc        LIKE wa_filtro_remetente-nr_qtd_vinc,
        saldo_retirado     LIKE wa_filtro_remetente-nr_qtd_vinc,
        saldo_nota         LIKE wa_filtro_remetente-nr_qtd_vinc,
        vg_verificar_saldo TYPE char01,
        v_tp_nf_rem        TYPE c,
        v_entrad           TYPE c,
        v_cont             TYPE i,
        v_msg              TYPE string,
        v_qtd_vinculada    TYPE zsdtvinc_p_flote-qtd_vinc. "US 156375 - PQ

  "// wbarbosa 13112024 - US-156375
  CALL FUNCTION 'Z_MEMO_CFOP_COMERCIALIZACAO'
    TABLES
      cfops = it_cfops_comercializacao.

  CALL FUNCTION 'Z_MEMO_CFOP_ENTRADAS'
    TABLES
      cfops = it_cfops_fim_espeficido.
  "// wbarbosa 13112024 - US-156375

  "Re-consulta DU-e
  DATA(_error) = abap_false.
  PERFORM f_check_due_filtro CHANGING _error.
  CHECK _error IS INITIAL.

  CLEAR: it_znom_notasfiscais_sel[].

  CALL METHOD plan_alv_notasfiscais->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.
  "V_cont = 0.
  LOOP AT it_selected_rows INTO wa_selected_rows WHERE index IS NOT INITIAL.
    READ TABLE it_znom_notasfiscais_alv INTO wa_znom_notasfiscais_alv INDEX wa_selected_rows-index.
    IF v_cont >= 1.

      IF v_entrad NE wa_znom_notasfiscais_alv-entrad.
        MESSAGE TEXT-057 TYPE 'I'.
        RETURN.
      ENDIF.


    ELSE.
      v_entrad = wa_znom_notasfiscais_alv-entrad.
    ENDIF.

    IF wa_znom_notasfiscais_alv-nr_saldo GT 0.
      APPEND wa_znom_notasfiscais_alv TO it_znom_notasfiscais_sel.
    ENDIF.

    IF wa_znom_notasfiscais_alv-id_due NE wa_filtro_remetente-id_due.
      MESSAGE TEXT-056 TYPE 'I'.
      RETURN.
    ENDIF.
    v_cont = 1.
  ENDLOOP.
  CLEAR: v_cont, v_entrad.

  nr_qtd_vinc = wa_filtro_remetente-nr_qtd_vinc.
  IF nr_qtd_vinc GT 0.
    vg_verificar_saldo = 'X'.
  ENDIF.

  REFRESH it_grp_retorno.

  IF it_znom_reme_dnotas_alv[] IS INITIAL. "ALRS
    REFRESH it_znom_reme_notas.
  ENDIF.

  CLEAR: v_tp_nf_rem.
  CASE abap_true.
    WHEN wa_filtro_remetente-tp_vinc1.
      v_tp_nf_rem = c_f.
    WHEN wa_filtro_remetente-tp_vinc2.
      IF wa_filtro_remetente-c_cct IS NOT INITIAL.
        v_tp_nf_rem = c_c.
      ELSE.
        v_tp_nf_rem = c_s.
      ENDIF.
    WHEN OTHERS.
      MESSAGE 'Tipo de vinculo não identificado!' TYPE 'I'.
      RETURN.
  ENDCASE.

  LOOP AT it_znom_notasfiscais_sel INTO wa_znom_notasfiscais_alv.

    "// wbarbosa 13112024 - US-156375
    IF wa_filtro_remetente-fins_espec IS NOT INITIAL.
      IF wa_znom_notasfiscais_alv-cfop IN it_cfops_comercializacao AND
         wa_filtro_remetente-tipov NE 'ZEXI'.
        MESSAGE TEXT-058  TYPE 'I'.
        RETURN.
      ENDIF.
    ENDIF.

    IF wa_filtro_remetente-comerc IS NOT INITIAL.
      IF ( wa_filtro_remetente-tipov NE 'ZEXP' AND
        wa_filtro_remetente-tipov NE 'ZEXI' ) OR
        wa_znom_notasfiscais_alv-cfop IN it_cfops_fim_espeficido.
        MESSAGE TEXT-059  TYPE 'I'.
        RETURN.
      ENDIF.
    ENDIF.
    "// wbarbosa 13112024 - US-156375

    IF wa_filtro_remetente-id_due IS NOT INITIAL.
      SELECT SINGLE *
        FROM znom_reme_notas INTO @DATA(_znom_reme_nf)
       WHERE id_nomeacao_tran  EQ @wa_znom_programacao_alv-id_nomeacao_tran
         AND id_empresa        EQ @wa_znom_programacao_alv-id_empresa
         AND id_filial         EQ @wa_znom_programacao_alv-id_filial
         AND id_material       EQ @wa_znom_programacao_alv-id_material
         AND id_remetente      EQ @wa_znom_notasfiscais_alv-parid
         AND grp_retorno       EQ @wa_filtro_remetente-grp_retorno
         AND id_due            NE @wa_filtro_remetente-id_due.

      IF sy-subrc = 0.
        MESSAGE 'Já existem notas vinculadas para essa Empresa/Filial/Material/Remetente/Grp.Retorno com DU-e diferente da informada!' TYPE 'I'.
        RETURN.
      ENDIF.

    ELSE.
      SELECT SINGLE *
        FROM znom_reme_notas INTO _znom_reme_nf
       WHERE id_nomeacao_tran  EQ wa_znom_programacao_alv-id_nomeacao_tran
         AND id_empresa        EQ wa_znom_programacao_alv-id_empresa
         AND id_filial         EQ wa_znom_programacao_alv-id_filial
         AND id_material       EQ wa_znom_programacao_alv-id_material
         AND id_remetente      EQ wa_znom_notasfiscais_alv-parid
         AND grp_retorno       EQ wa_filtro_remetente-grp_retorno
         AND id_due            NE 0.

      IF sy-subrc = 0.
        MESSAGE 'Já existem notas vinculadas para essa Empresa/Filial/Material/Remetente/Grp.Retorno com DU-e informada!' TYPE 'I'.
        RETURN.
      ENDIF.
    ENDIF.

    SELECT SINGLE *
      FROM znom_reme_notas INTO _znom_reme_nf
     WHERE id_nomeacao_tran  EQ wa_znom_programacao_alv-id_nomeacao_tran
       AND id_empresa        EQ wa_znom_programacao_alv-id_empresa
       AND id_filial         EQ wa_znom_programacao_alv-id_filial
       AND id_material       EQ wa_znom_programacao_alv-id_material
       AND id_remetente      EQ wa_znom_notasfiscais_alv-parid
       AND grp_retorno       EQ wa_filtro_remetente-grp_retorno
       AND tp_nf_rem         NE v_tp_nf_rem.

    IF sy-subrc = 0.
      MESSAGE |Já existem notas vinculadas com tipos diferentes!| TYPE 'I'.
      RETURN.
    ENDIF.

    SELECT *
     FROM znom_reme_notas INTO TABLE @DATA(t_znom_reme_notas)
    WHERE id_nomeacao_tran  EQ @wa_znom_programacao_alv-id_nomeacao_tran
      AND id_empresa        EQ @wa_znom_programacao_alv-id_empresa
      AND id_filial         EQ @wa_znom_programacao_alv-id_filial
      AND id_material       EQ @wa_znom_programacao_alv-id_material
      AND grp_retorno       EQ @wa_filtro_remetente-grp_retorno.

    IF sy-subrc = 0.

      LOOP AT t_znom_reme_notas INTO DATA(wa_znom_reme_nf).
        IF v_cont >= 1.

          IF v_entrad NE wa_znom_reme_nf-entrad.

            CONCATENATE 'No Grp de Retorno "' wa_znom_reme_nf-grp_retorno '" as notas devem ser ( "com Entrada Própria" )  ou  (" sem Entrada Própria" ).' INTO DATA(v_mensagem).
            MESSAGE v_mensagem TYPE 'I'.
            RETURN.
          ENDIF.
        ELSE.
          v_entrad = wa_znom_reme_nf-entrad.
        ENDIF.

        v_cont = 1.
**<<<------"165835 - NMS - INI------>>>
*        "WPP 23102024 - US-153330 --->>>
*        DATA(r_nf_eudr) = zcl_eudr_utils=>check_doc_fiscal_eudr( i_docnum =   wa_znom_reme_nf-docnum ).
*        CASE wa_filtro_remetente-due_eudr.
*          WHEN zcl_eudr_utils=>lc_s_eudr.
*            IF r_nf_eudr NE zcl_eudr_utils=>lc_s_eudr.
*              MESSAGE |DU-e atende EUDR, porém documento fiscal { wa_znom_reme_nf-docnum } do grupo { wa_filtro_remetente-grp_retorno } não atende EUDR! | TYPE 'I'.
*              RETURN.
*            ENDIF.
*          WHEN zcl_eudr_utils=>lc_n_eudr.
*            IF r_nf_eudr EQ zcl_eudr_utils=>lc_s_eudr.
*              MESSAGE |DU-e não atende EUDR, porém documento fiscal { wa_znom_reme_nf-docnum } do grupo { wa_filtro_remetente-grp_retorno } atende EUDR! | TYPE 'I'.
*              RETURN.
*            ENDIF.
*        ENDCASE.
*        "WPP 23102024 - US-153330 <<<---
        SELECT SINGLE * FROM znom_remetente
          INTO @DATA(el_remetente)
         WHERE id_nomeacao_tran = @wa_znom_programacao_alv-id_nomeacao_tran
           AND id_empresa       = @wa_znom_programacao_alv-id_empresa
           AND id_filial        = @wa_znom_programacao_alv-id_filial
           AND id_material      = @wa_znom_programacao_alv-id_material
           AND grp_retorno      = @wa_filtro_remetente-grp_retorno.

        IF NOT sy-subrc IS INITIAL.
          READ TABLE  it_znom_remetente_log INTO el_remetente
            WITH KEY id_nomeacao_tran = wa_znom_programacao_alv-id_nomeacao_tran
                     id_empresa       = wa_znom_programacao_alv-id_empresa
                     id_filial        = wa_znom_programacao_alv-id_filial
                     id_material      = wa_znom_programacao_alv-id_material
                     grp_retorno      = wa_filtro_remetente-grp_retorno.

        ENDIF.

        IF sy-subrc IS INITIAL.
* Valida o EUDR da DU-e com o Remetentes da Nomeações de Transporte.
          CASE wa_filtro_remetente-due_eudr.
            WHEN zcl_eudr_utils=>lc_s_eudr.
              IF el_remetente-eudr NE zcl_eudr_utils=>lc_s_eudr.
                MESSAGE |EUDR da DU-e diverge do EUDR do grupo { wa_filtro_remetente-grp_retorno } | TYPE 'I'.
                RETURN.

              ENDIF.

            WHEN zcl_eudr_utils=>lc_n_eudr OR space.
              IF NOT wa_filtro_remetente-retornar_eudr IS INITIAL                   AND
                     wa_filtro_remetente-due_eudr      EQ zcl_eudr_utils=>lc_n_eudr AND
                     el_remetente-eudr                 NE zcl_eudr_utils=>lc_s_eudr.
                MESSAGE |EUDR da DU-e diverge do EUDR do grupo { wa_filtro_remetente-grp_retorno } | TYPE 'I'.
                RETURN.

              ELSE.
                IF el_remetente-eudr EQ zcl_eudr_utils=>lc_s_eudr.
                  MESSAGE |EUDR da DU-e diverge do EUDR do grupo { wa_filtro_remetente-grp_retorno } | TYPE 'I'.
                  RETURN.

                ENDIF.

              ENDIF.

            WHEN OTHERS.
* Do nothing
          ENDCASE.

        ENDIF.
**<<<------"165835 - NMS - FIM------>>>
      ENDLOOP.

      IF v_entrad NE wa_znom_notasfiscais_alv-entrad.

        IF v_entrad EQ 'X'.
          CONCATENATE 'No Grp de Retorno "' wa_znom_reme_nf-grp_retorno '" as notas devem ser "Com Entrada Própria"' INTO v_msg.
        ELSE.
          CONCATENATE 'No Grp de Retorno "' wa_znom_reme_nf-grp_retorno '" as notas devem ser "Sem Entrada Própria"' INTO v_msg.
        ENDIF.

        MESSAGE v_msg TYPE 'I'.
        RETURN.
      ENDIF.


*
*        CONCATENATE 'No Grp de Retorno "' wa_znom_reme_nf-grp_retorno '" as notas devem ser ( "com Entrada Própria" )  ou  (" sem Entrada Própria" ).' INTO DATA(v_mensagem).
*        MESSAGE v_mensagem TYPE 'I'.
*        RETURN.
*      ENDIF.
    ENDIF.

    EXIT.
  ENDLOOP.


  LOOP AT it_znom_notasfiscais_sel INTO wa_znom_notasfiscais_alv.
    vg_tabix = sy-tabix.
    saldo_nota = wa_znom_notasfiscais_alv-nr_saldo.
    IF wa_znom_programacao_alv-nr_qtde_saldo_rem GT 0.
      saldo_retirado = 0.
      IF vg_verificar_saldo IS NOT INITIAL.
        IF wa_znom_notasfiscais_alv-nr_saldo GT nr_qtd_vinc.
          saldo_retirado = wa_znom_notasfiscais_alv-nr_saldo - nr_qtd_vinc.
          wa_znom_notasfiscais_alv-nr_saldo = nr_qtd_vinc.
        ENDIF.
        nr_qtd_vinc = nr_qtd_vinc - wa_znom_notasfiscais_alv-nr_saldo.
      ENDIF.

      "distribui a quantidade pelos grupos
      REFRESH it_grp_retorno.
      IF it_znom_reme_dnotas_alv[] IS NOT INITIAL. "ALRS
        LOOP AT it_znom_reme_dnotas_alv INTO wa_znom_reme_dnotas_alv.
          wa_grp_retorno-grp_retorno  = wa_znom_reme_dnotas_alv-grp_retorno.
          wa_grp_retorno-docnum_rt    = wa_znom_reme_dnotas_alv-docnum_rt.
          wa_grp_retorno-nr_ordem     = wa_znom_reme_dnotas_alv-nr_ordem.
          wa_grp_retorno-total        = wa_znom_reme_dnotas_alv-nr_quantidade.
          wa_grp_retorno-novos        = 0.
          wa_grp_retorno-saldo        = 0.
          wa_grp_retorno-quantidade   = 0.
          COLLECT wa_grp_retorno INTO it_grp_retorno.
        ENDLOOP.
        LOOP AT it_znom_reme_notas INTO wa_znom_reme_notas.  "Novos - Notas.
          IF wa_znom_reme_notas-mandt = 999.
            wa_grp_retorno-grp_retorno  = wa_znom_reme_notas-grp_retorno.
            wa_grp_retorno-docnum_rt    = wa_znom_reme_notas-docnum_rt.
            wa_grp_retorno-nr_ordem     = wa_znom_reme_notas-nr_ordem.
            wa_grp_retorno-total        = 0.
            wa_grp_retorno-novos        = wa_znom_reme_notas-nr_quantidade.
            wa_grp_retorno-saldo        = 0.
            wa_grp_retorno-quantidade   = 0.
            COLLECT wa_grp_retorno INTO it_grp_retorno.
          ENDIF.
        ENDLOOP.
        LOOP AT it_grp_retorno INTO wa_grp_retorno.
          wa_grp_retorno-saldo = wa_grp_retorno-total - wa_grp_retorno-novos.
          MODIFY it_grp_retorno FROM wa_grp_retorno INDEX sy-tabix TRANSPORTING saldo.
        ENDLOOP.
        DELETE it_grp_retorno WHERE saldo = 0.
      ENDIF.

      LOOP AT it_grp_retorno INTO wa_grp_retorno.
        IF wa_grp_retorno-saldo GE wa_znom_notasfiscais_alv-nr_saldo.
          wa_grp_retorno-quantidade = wa_znom_notasfiscais_alv-nr_saldo.
          MODIFY it_grp_retorno FROM wa_grp_retorno INDEX sy-tabix TRANSPORTING quantidade.
          EXIT.
        ELSE.
          wa_grp_retorno-quantidade = wa_grp_retorno-saldo.
          wa_znom_notasfiscais_alv-nr_saldo = wa_znom_notasfiscais_alv-nr_saldo - wa_grp_retorno-saldo.
          MODIFY it_grp_retorno FROM wa_grp_retorno INDEX sy-tabix TRANSPORTING quantidade.
        ENDIF.
      ENDLOOP.

      DATA(_stop_vinc) = abap_false.

      DELETE it_grp_retorno WHERE quantidade = 0.

      IF it_grp_retorno[] IS INITIAL.
        PERFORM vincula_nota USING wa_znom_notasfiscais_alv
                          CHANGING _stop_vinc.
      ELSE.

        LOOP AT it_grp_retorno INTO wa_grp_retorno.
          wa_filtro_remetente-grp_retorno = wa_grp_retorno-grp_retorno.
          wa_znom_notasfiscais_alv-nr_saldo = wa_grp_retorno-quantidade.
          PERFORM vincula_nota USING wa_znom_notasfiscais_alv
                            CHANGING _stop_vinc.
        ENDLOOP.
      ENDIF.

      IF _stop_vinc EQ abap_true.
        EXIT.
      ENDIF.

**inicio - "US 156375 - PQ
*      "ALRS
*      wa_znom_notasfiscais_alv-nr_saldo = wa_znom_notasfiscais_alv-nr_quantidade2 - wa_znom_notasfiscais_alv-nr_utilizada.

      IF wa_filtro_remetente-tp_vinc1 IS NOT INITIAL.
        CLEAR: v_qtd_vinculada.
**<<<------"163355 - NMS - INI------>>>
*        SELECT SUM( qtd_vinc )
*          FROM zsdtvinc_p_flote INTO v_qtd_vinculada
*         WHERE docnum_eprod = wa_znom_notasfiscais_alv-docnum.
        DATA: tl_consulta_terminal TYPE zsdct_consulta_terminal.
        DATA: vl_erro TYPE c.

* Valida o Terminal de embarque com relação a NF de entrada e/ou saída.
        PERFORM zf_valida_terminal TABLES tl_consulta_terminal
                                    USING wa_znom_notasfiscais_alv-docnum
                                          wa_filtro_remetente-codigo_ra_embarque
                                          vl_erro.

        IF NOT tl_consulta_terminal[] IS INITIAL.
* Verifica se é Nota EUDR, qual o tipo e valida as condições.
          PERFORM f_filter_eudr_documents TABLES tl_consulta_terminal
                                           USING vl_erro.

          SELECT docnum_flote, qtd_vinc FROM zsdtvinc_p_flote
            INTO TABLE @DATA(tl_vinc)
            FOR ALL ENTRIES IN @tl_consulta_terminal
          WHERE docnum_flote EQ @tl_consulta_terminal-docnum.

          IF sy-subrc IS INITIAL.
            LOOP AT tl_vinc INTO DATA(el_vinc).
              ADD el_vinc-qtd_vinc TO v_qtd_vinculada.

            ENDLOOP.

          ENDIF.

          CLEAR: tl_consulta_terminal, vl_erro.

        ELSE.
          CLEAR vl_erro.

        ENDIF.
*--------------------------------------------------------------------------------------------------------------*
*     Calculos da quantidades do CCT
*--------------------------------------------------------------------------------------------------------------*
        SELECT * FROM znom_reme_notas
          INTO TABLE it_znom_reme_notas
        WHERE id_nomeacao_tran EQ wa_znom_programacao_alv-id_nomeacao_tran
          AND id_empresa       EQ wa_znom_programacao_alv-id_empresa
          AND id_filial        EQ wa_znom_programacao_alv-id_filial
          AND id_material      EQ wa_znom_programacao_alv-id_material
          AND docnum           EQ wa_znom_notasfiscais_alv-docnum
          AND itmnum           EQ wa_znom_notasfiscais_alv-itmnum
          AND grp_retorno      EQ wa_filtro_remetente-grp_retorno.

        IF sy-subrc IS INITIAL.
* Quantidade Utilizada
          LOOP AT it_znom_reme_notas INTO wa_znom_reme_notas WHERE docnum EQ wa_znom_notasfiscais_alv-docnum
                                                               AND itmnum EQ wa_znom_notasfiscais_alv-itmnum.

            CHECK ( wa_znom_reme_notas-tp_nf_rem EQ c_f ) OR "Vinculo com RFL
                  ( wa_znom_reme_notas-tp_nf_rem EQ c_c ).   "Com CCT

            ADD wa_znom_reme_notas-nr_quantidade TO wa_znom_notasfiscais_alv-nr_utilizada_cct.

          ENDLOOP.

          wa_znom_notasfiscais_alv-saldo_cct  = wa_znom_notasfiscais_alv-peso_cct - wa_znom_notasfiscais_alv-nr_utilizada_cct.

          IF wa_znom_notasfiscais_alv-peso_cct > 0.
            wa_znom_notasfiscais_alv-dif_peso_cct_nf = wa_znom_notasfiscais_alv-qtde_nf - wa_znom_notasfiscais_alv-peso_cct.

          ENDIF.

          MODIFY it_znom_notasfiscais_sel INDEX vg_tabix FROM wa_znom_notasfiscais_alv TRANSPORTING nr_utilizada_cct saldo_cct dif_peso_cct_nf.

        ENDIF.
**<<<------"163355 - NMS - FIM------>>>
        "// Verificar se a nota está totalmente vinculada a uma NF de Saida
        IF v_qtd_vinculada < wa_znom_notasfiscais_alv-nr_quantidade2.
          wa_znom_notasfiscais_alv-nr_saldo = v_qtd_vinculada - wa_znom_notasfiscais_alv-nr_utilizada.
        ELSE.
          wa_znom_notasfiscais_alv-nr_saldo = wa_znom_notasfiscais_alv-nr_quantidade2 - wa_znom_notasfiscais_alv-nr_utilizada.
        ENDIF.
      ELSE.
        "ALRS
        wa_znom_notasfiscais_alv-nr_saldo = wa_znom_notasfiscais_alv-nr_quantidade2 - wa_znom_notasfiscais_alv-nr_utilizada.
      ENDIF.
**fim - "US 156375 - PQ

      MODIFY it_znom_notasfiscais_sel INDEX vg_tabix FROM wa_znom_notasfiscais_alv TRANSPORTING nr_utilizada nr_saldo.

      READ TABLE it_znom_notasfiscais_alv WITH KEY docnum = wa_znom_notasfiscais_alv-docnum
                                                   itmnum = wa_znom_notasfiscais_alv-itmnum.
      IF sy-subrc IS INITIAL.
        vg_tabix_2 = sy-tabix.
*        wa_znom_notasfiscais_alv-nr_saldo = wa_znom_notasfiscais_alv-nr_quantidade2 - wa_znom_notasfiscais_alv-nr_utilizada.
**<<<------"163355 - NMS - INI------>>>
**inicio - "US 156375 - PQ
*        IF wa_filtro_remetente-tp_vinc1 IS NOT INITIAL.
*          CLEAR: v_qtd_vinculada.
*          SELECT SUM( qtd_vinc )
*            FROM zsdtvinc_p_flote INTO v_qtd_vinculada
*           WHERE docnum_eprod = wa_znom_notasfiscais_alv-docnum.
*
*          "// Verificar se a nota está totalmente vinculada a uma NF de Saida
*          IF v_qtd_vinculada < wa_znom_notasfiscais_alv-nr_quantidade2.
*            wa_znom_notasfiscais_alv-nr_saldo = v_qtd_vinculada - wa_znom_notasfiscais_alv-nr_utilizada.
*          ELSE.
*            wa_znom_notasfiscais_alv-nr_saldo = wa_znom_notasfiscais_alv-nr_quantidade2 - wa_znom_notasfiscais_alv-nr_utilizada.
*
*          ENDIF.
*        ELSE.
*          wa_znom_notasfiscais_alv-nr_saldo = wa_znom_notasfiscais_alv-nr_quantidade2 - wa_znom_notasfiscais_alv-nr_utilizada.
*        ENDIF.
***fim - "US 156375 - PQ
*        MODIFY it_znom_notasfiscais_alv INDEX vg_tabix_2 FROM wa_znom_notasfiscais_alv TRANSPORTING nr_utilizada nr_saldo.
        MODIFY it_znom_notasfiscais_alv INDEX vg_tabix_2 FROM wa_znom_notasfiscais_alv TRANSPORTING nr_utilizada nr_saldo nr_utilizada_cct saldo_cct dif_peso_cct_nf.
        CLEAR wa_znom_notasfiscais_alv.
**<<<------"163355 - NMS - FIM------>>>
      ENDIF.
    ELSE.
      MESSAGE TEXT-058 TYPE 'S'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF it_znom_reme_dnotas_alv[] IS INITIAL. "ALRS
    PERFORM consulta_remetentes.
  ELSE.
    PERFORM consulta_remetentes_log USING 'N'.
  ENDIF.

ENDFORM.                    " VINCULAR_NOTAS

*&---------------------------------------------------------------------*
*&      Form  VINCULA_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM vincula_nota  USING  nota_fiscal TYPE zplac_notasfiscais
                 CHANGING c_stop_vinc.

  DATA: wa_notas                TYPE ty_znom_reme_notas,
        wa_remetente            TYPE ty_znom_remetente,
        wa_znom_remetente       TYPE znom_remetente,
        vg_tabix                TYPE sy-tabix,
        wa_znom_programacao_aux TYPE ty_zplac_nom_programacao,
        vg_qtd_vinculada        LIKE nota_fiscal-nr_saldo,
        v_nr_quantidade         TYPE znom_reme_notas-nr_quantidade,
        vg_quantidade_recusada  TYPE j_1bnetqty.

  CHECK nota_fiscal-nr_saldo GT 0.

  vg_qtd_vinculada = 0.

  c_stop_vinc = abap_false.

  READ TABLE it_znom_reme_notas_alv WITH KEY docnum      = nota_fiscal-docnum
                                             grp_retorno = wa_filtro_remetente-grp_retorno
                                             itmnum      = nota_fiscal-itmnum.
  IF sy-subrc IS INITIAL.

    MOVE-CORRESPONDING it_znom_reme_notas_alv TO wa_notas.

    "Verifica Quantidade Recusada.
    SELECT SUM( r~menge ) INTO vg_quantidade_recusada
      FROM zdoc_exp_rec_nf AS r
     INNER JOIN znom_prog_reme AS m ON m~id_remessa EQ r~vbeln_re_exp
     WHERE r~docnum_prod      EQ wa_notas-docnum
       AND r~itmnum_prod      EQ wa_notas-itmnum
       AND m~id_nomeacao_tran EQ wa_znom_programacao_alv-id_nomeacao_tran
       AND m~id_empresa       EQ wa_znom_programacao_alv-id_empresa
       AND m~id_filial        EQ wa_znom_programacao_alv-id_filial
       AND m~id_material      EQ wa_znom_programacao_alv-id_material
       AND NOT EXISTS ( SELECT *
                          FROM znom_reme_notas AS n
                         WHERE n~id_nomeacao_tran EQ m~id_nomeacao_tran
                           AND n~id_empresa       EQ m~id_empresa
                           AND n~id_filial        EQ m~id_filial
                           AND n~id_material      EQ m~id_material
                           AND n~id_remetente     EQ r~id_remetente
                           AND n~docnum           EQ r~docnum_prod
                           AND n~itmnum           EQ r~itmnum_prod ).

    wa_notas-nr_quantidade = wa_notas-nr_quantidade - vg_quantidade_recusada.

  ELSE.
    wa_notas-id_nomeacao_tran = wa_znom_programacao_alv-id_nomeacao_tran.
    wa_notas-id_empresa       = wa_znom_programacao_alv-id_empresa.
    wa_notas-id_filial        = wa_znom_programacao_alv-id_filial.
    wa_notas-id_material      = wa_znom_programacao_alv-id_material.
    wa_notas-id_unidade       = wa_znom_programacao_alv-id_unidade.
    wa_notas-id_remetente     = nota_fiscal-parid.
    wa_notas-docnum           = nota_fiscal-docnum.
    wa_notas-itmnum           = nota_fiscal-itmnum.
    wa_notas-tp_nf_rem        = nota_fiscal-tp_nf_rem.
    wa_notas-dt_registro      = sy-datum.
    wa_notas-hr_registro      = sy-uzeit.
    wa_notas-us_registro      = sy-uname.

    wa_notas-nr_quantidade    = 0.

    wa_notas-id_due               = wa_filtro_remetente-id_due.
    wa_notas-numero_due           = wa_filtro_remetente-numero_due.
    wa_notas-codigo_urf_embarque  = wa_filtro_remetente-codigo_urf_embarque.
    wa_notas-codigo_ra_embarque   = wa_filtro_remetente-codigo_ra_embarque.

  ENDIF.

  IF wa_znom_programacao_alv-nr_qtde_saldo_rem GE nota_fiscal-nr_saldo.
    vg_qtd_vinculada          = nota_fiscal-nr_saldo.
    wa_notas-nr_quantidade    = wa_notas-nr_quantidade + nota_fiscal-nr_saldo.
    nota_fiscal-nr_utilizada  = nota_fiscal-nr_utilizada + nota_fiscal-nr_saldo.
    nota_fiscal-nr_saldo      = 0.
  ELSE.
    vg_qtd_vinculada          = wa_znom_programacao_alv-nr_qtde_saldo_rem.
    wa_notas-nr_quantidade    = wa_notas-nr_quantidade + wa_znom_programacao_alv-nr_qtde_saldo_rem.
    nota_fiscal-nr_utilizada  = nota_fiscal-nr_utilizada + wa_znom_programacao_alv-nr_qtde_saldo_rem.
    nota_fiscal-nr_saldo      = nota_fiscal-nr_saldo - wa_znom_programacao_alv-nr_qtde_saldo_rem.
  ENDIF.

  wa_znom_programacao_alv-nr_qtde_remetente = wa_znom_programacao_alv-nr_qtde_remetente + wa_notas-nr_quantidade.
  wa_znom_programacao_alv-nr_qtde_saldo_rem = wa_znom_programacao_alv-nr_qtde_saldo_rem - wa_notas-nr_quantidade.
  wa_znom_programacao_alv-nr_qtde_planejada = wa_znom_programacao_alv-nr_qtde_remetente.
  wa_znom_programacao_alv-nr_qtde_saldo_pla = wa_znom_programacao_alv-nr_qtde_saldo_rem.

  MOVE-CORRESPONDING wa_znom_programacao_alv TO wa_znom_programacao_aux.

  READ TABLE it_znom_programacao_alv WITH KEY id_nomeacao_tran = wa_znom_programacao_aux-id_nomeacao_tran
                                              id_empresa       = wa_znom_programacao_aux-id_empresa
                                              id_filial        = wa_znom_programacao_aux-id_filial
                                              id_material      = wa_znom_programacao_aux-id_material.
  IF sy-subrc IS INITIAL.
    vg_tabix = sy-tabix.
    MODIFY it_znom_programacao_alv INDEX vg_tabix FROM wa_znom_programacao_aux
      TRANSPORTING nr_qtde_remetente nr_qtde_saldo_rem nr_qtde_planejada nr_qtde_saldo_pla.
  ENDIF.

  IF it_znom_reme_dnotas_alv[] IS INITIAL. "ALRS
    SELECT SINGLE * INTO wa_remetente
      FROM znom_remetente
     WHERE id_nomeacao_tran = wa_znom_programacao_alv-id_nomeacao_tran
       AND id_empresa       = wa_znom_programacao_alv-id_empresa
       AND id_filial        = wa_znom_programacao_alv-id_filial
       AND id_material      = wa_znom_programacao_alv-id_material
       AND id_remetente     = nota_fiscal-parid
       AND grp_retorno      = wa_filtro_remetente-grp_retorno.

  ELSE.
    READ TABLE  it_znom_remetente_log INTO wa_remetente
      WITH KEY id_nomeacao_tran = wa_znom_programacao_alv-id_nomeacao_tran
               id_empresa       = wa_znom_programacao_alv-id_empresa
               id_filial        = wa_znom_programacao_alv-id_filial
               id_material      = wa_znom_programacao_alv-id_material
               id_remetente     = nota_fiscal-parid
               grp_retorno      = wa_filtro_remetente-grp_retorno.

  ENDIF.

  IF sy-subrc IS INITIAL.
    IF wa_remetente-novo = 'D'.
      wa_remetente-nr_programada = vg_qtd_vinculada.
    ELSE.
      wa_remetente-nr_programada = wa_remetente-nr_programada + vg_qtd_vinculada.
    ENDIF.
  ELSE.
    wa_remetente-id_nomeacao_tran = wa_znom_programacao_alv-id_nomeacao_tran.
    wa_remetente-id_empresa       = wa_znom_programacao_alv-id_empresa.
    wa_remetente-id_filial        = wa_znom_programacao_alv-id_filial.
    wa_remetente-id_material      = wa_znom_programacao_alv-id_material.
    wa_remetente-id_remetente     = nota_fiscal-parid.
    wa_remetente-id_unidade       = wa_znom_programacao_alv-id_unidade.
    wa_remetente-nr_programada    = vg_qtd_vinculada.
**<<<------"145379 - NMS - INI------>>>
    wa_remetente-id_due               = wa_filtro_remetente-id_due.
    wa_remetente-numero_due           = wa_filtro_remetente-numero_due.
    wa_remetente-codigo_urf_embarque  = wa_filtro_remetente-codigo_urf_embarque.
    wa_remetente-codigo_ra_embarque   = wa_filtro_remetente-codigo_ra_embarque.
**<<<------"145379 - NMS - FIM------>>>
**<<<------"165835 - NMS - INI------>>>
* Valida se "Listar apenas notas EUDR = SIM".
    CASE wa_filtro_remetente-retornar_eudr.
      WHEN abap_on.
        wa_remetente-eudr = 'S'.

      WHEN abap_off.
        wa_remetente-eudr = wa_filtro_remetente-due_eudr.

      WHEN OTHERS.
* Do nothing
    ENDCASE.
**<<<------"165835 - NMS - FIM------>>>
  ENDIF.
** Grava Grupo de retorno
  wa_remetente-grp_retorno      = wa_filtro_remetente-grp_retorno.
  wa_notas-grp_retorno          = wa_filtro_remetente-grp_retorno.

  "tipo entrada
  wa_notas-entrad = nota_fiscal-entrad.
**<<<------"145379 - NMS - INI------>>>
* Carrega campo Retorno será gerada com NF de RFL de Terceiro.
  wa_remetente-retorno_com_nf_de_terceiro = znom_remetente-retorno_com_nf_de_terceiro.
**<<<------"145379 - NMS - FIM------>>>
  "DEVK9A1V05 - 07.02.2024 113637 CS2023000378 ORDEM DE VENDA AUTOM
  "Campos criados no cabeçalho para a ABA remetentes vinculados


  IF wa_filtro_remetente-tipov NE wa_remetente-tipov OR
     wa_filtro_remetente-preco NE wa_remetente-preco OR
     wa_filtro_remetente-depst NE wa_remetente-depst OR
     wa_filtro_remetente-safra NE wa_remetente-safra OR
     wa_filtro_remetente-cvirt NE wa_remetente-cvirt.
    wa_remetente-tipov            = wa_filtro_remetente-tipov.
    wa_remetente-preco            = wa_filtro_remetente-preco.
    wa_remetente-depst            = wa_filtro_remetente-depst.
    wa_remetente-safra            = wa_filtro_remetente-safra.
    wa_remetente-cvirt            = wa_filtro_remetente-cvirt.
  ENDIF.

  "DEVK9A1V05 - 07.02.2024 113637 CS2023000378 ORDEM DE VENDA AUTOM

  IF it_znom_reme_dnotas_alv[] IS INITIAL. "ALRS
    MODIFY znom_remetente FROM wa_remetente.
    MODIFY znom_reme_notas FROM wa_notas.

*-------------------------------------------------------------*
*   Check se quantidade vinculada foi excedida
*-------------------------------------------------------------*
    CLEAR: v_nr_quantidade.

    SELECT SUM( nr_quantidade )
      FROM znom_reme_notas INTO v_nr_quantidade
     WHERE docnum = wa_notas-docnum
       AND itmnum = wa_notas-itmnum.

    IF v_nr_quantidade > nota_fiscal-qtde_nf.
      ROLLBACK WORK.
      c_stop_vinc = abap_true.
      MESSAGE |Quantidade planejada do documento { wa_notas-docnum } foi excedida! Reconsultar NFs!| TYPE 'I'.
      RETURN.
    ENDIF.

    DATA(_error) = abap_false.
    PERFORM f_check_qtde_prog_due USING wa_filtro_remetente-id_due
                                        wa_znom_programacao_alv-id_nomeacao_tran
                               CHANGING _error.

    IF _error EQ abap_true.
      ROLLBACK WORK.
      c_stop_vinc = abap_true.
      RETURN.
    ENDIF.

    COMMIT WORK.

    CLEAR: wa_znom_remetente.
    MOVE-CORRESPONDING wa_remetente TO wa_znom_remetente.

    DATA(_change) = abap_false.
    PERFORM f_check_reg_rem_vinc USING wa_znom_remetente
                              CHANGING _change.
    "IF _CHANGE EQ ABAP_TRUE.
    "MESSAGE 'Quantidades foram ajustadas! Revisar quantidades do Grupo' TYPE 'E'.
    "RETURN.
    "ENDIF.

  ELSE.
    "Remetente
    MOVE-CORRESPONDING wa_remetente TO wa_znom_remetente_log.
    wa_znom_remetente_log-docnum_rt = wa_grp_retorno-docnum_rt.
    wa_znom_remetente_log-nr_ordem  = wa_grp_retorno-nr_ordem.
    READ TABLE it_znom_remetente_log
     WITH KEY  id_nomeacao_tran = wa_znom_remetente_log-id_nomeacao_tran
               id_empresa       = wa_znom_remetente_log-id_empresa
               id_filial        = wa_znom_remetente_log-id_filial
               id_material      = wa_znom_remetente_log-id_material
               id_remetente     = wa_znom_remetente_log-id_remetente
               grp_retorno      = wa_znom_remetente_log-grp_retorno.
    IF sy-subrc = 0.
      wa_znom_remetente_log-novo = 'A'.
      MODIFY it_znom_remetente_log FROM wa_znom_remetente_log INDEX sy-tabix.
    ELSE.
      wa_znom_remetente_log-novo = 'N'.
      APPEND wa_znom_remetente_log TO it_znom_remetente_log.
    ENDIF.

*    "Notas

    IF it_znom_reme_dnotas_alv[] IS NOT INITIAL. "ALRS
      wa_notas-mandt = 999.
      wa_notas-docnum_rt = wa_grp_retorno-docnum_rt.
      wa_notas-nr_ordem  = wa_grp_retorno-nr_ordem.
    ENDIF.

    DELETE it_znom_reme_notas
       WHERE id_nomeacao_tran = wa_notas-id_nomeacao_tran
         AND id_empresa       = wa_notas-id_empresa
         AND id_filial        = wa_notas-id_filial
         AND id_material      = wa_notas-id_material
         AND id_remetente     = wa_notas-id_remetente
         AND docnum           = wa_notas-docnum
         AND itmnum           = wa_notas-itmnum
         AND grp_retorno      = wa_notas-grp_retorno.

    APPEND wa_notas TO it_znom_reme_notas.
  ENDIF.

ENDFORM.                    " VINCULA_NOTA


*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM desvincular_notas .

  DATA: wa_znom_remetente  TYPE znom_remetente.

  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row,
        vg_tabix         TYPE sy-tabix,
        vg_tabix_2       TYPE sy-tabix.

  IF it_znom_reme_dnotas_alv[] IS NOT INITIAL. "ALRS
    MESSAGE 'Permitido apenas vincular nota para troca' TYPE 'I'.
    EXIT.
  ENDIF.

  CLEAR: it_znom_reme_notas_sel[].

  CALL METHOD plan_alv_reme_notas->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows WHERE index IS NOT INITIAL.
    READ TABLE it_znom_reme_notas_alv INTO wa_znom_reme_notas_alv INDEX wa_selected_rows-index.
    IF wa_znom_reme_notas_alv-docnum NE '999999999'.
      APPEND wa_znom_reme_notas_alv TO it_znom_reme_notas_sel.
    ENDIF.
  ENDLOOP.

  LOOP AT it_znom_reme_notas_sel INTO wa_znom_reme_notas_alv.
    vg_tabix = sy-tabix.
    PERFORM des_vincula_nota USING wa_znom_reme_notas_alv.

    CLEAR: wa_znom_remetente.
    MOVE-CORRESPONDING wa_znom_reme_notas_alv TO wa_znom_remetente.

    DATA(_change) = abap_false.
    PERFORM f_check_reg_rem_vinc USING wa_znom_remetente
                              CHANGING _change.
  ENDLOOP.

  PERFORM consulta_remetentes.

ENDFORM.                    " DESVINCULAR_NOTAS


*&---------------------------------------------------------------------*
*&      Form  DES_VINCULA_NOTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM des_vincula_nota USING nota_vinculada TYPE zplac_nom_reme_notas.

  DATA: wa_remetente            TYPE znom_remetente,
        vg_tabix                TYPE sy-tabix,
        wa_znom_programacao_aux TYPE ty_zplac_nom_programacao,
        it_reme_vinc            TYPE TABLE OF znom_prog_reme WITH HEADER LINE,
        it_prod_vinc            TYPE TABLE OF zdoc_nf_produtor WITH HEADER LINE,
        it_prod_recu            TYPE TABLE OF zdoc_exp_rec_nf WITH HEADER LINE,
        qtd_efetivada           TYPE j_1bnetqty,
        qtd_recusada            TYPE j_1bnetqty,
        znom_rem_nf_aux         TYPE znom_reme_notas,
        nota_recusada           TYPE znom_reme_notas.

  SELECT SINGLE * INTO nota_recusada
    FROM znom_reme_notas
   WHERE id_nomeacao_tran = nota_vinculada-id_nomeacao_tran
     AND id_empresa       = nota_vinculada-id_empresa
     AND id_filial        = nota_vinculada-id_filial
     AND id_material      = nota_vinculada-id_material
     AND id_remetente     = nota_vinculada-id_remetente
     AND docnum           = nota_vinculada-docnum
     AND itmnum           = nota_vinculada-itmnum
     AND grp_retorno      = nota_vinculada-grp_retorno.

  CHECK sy-subrc IS INITIAL.

  SELECT SINGLE * INTO wa_remetente
    FROM znom_remetente
   WHERE id_nomeacao_tran = nota_vinculada-id_nomeacao_tran
     AND id_empresa       = nota_vinculada-id_empresa
     AND id_filial        = nota_vinculada-id_filial
     AND id_material      = nota_vinculada-id_material
     AND id_remetente     = nota_vinculada-id_remetente
     AND grp_retorno      = nota_vinculada-grp_retorno.

  IF wa_remetente-docnum_rt IS NOT INITIAL
  OR wa_remetente-nr_ordem  IS NOT INITIAL.
    MESSAGE 'A nota não pode ser desvinculada pois já possui DocNum Ret./OV. Export.' TYPE 'E'.
    LEAVE TO SCREEN 0001.
  ENDIF.

  IF nota_vinculada-tp_nf_rem EQ 'F'. "Vinculo Rem.Form Lote
    SELECT SINGLE *
      FROM znom_reme_notas INTO znom_rem_nf_aux
     WHERE docnum           = nota_vinculada-docnum
       AND itmnum           = nota_vinculada-itmnum
       AND tp_nf_rem        = 'S'. "Sem Registro CCT

    IF sy-subrc EQ 0.
      MESSAGE 'A nota não pode ser desvinculada pois já possui um vinculo Tipo ''S'' ! ' TYPE 'I'.
      LEAVE TO SCREEN 0001.
    ENDIF.
  ENDIF.

  SELECT * INTO TABLE it_reme_vinc
    FROM znom_prog_reme
   WHERE id_nomeacao_tran = nota_vinculada-id_nomeacao_tran
     AND id_empresa       = nota_vinculada-id_empresa
     AND id_filial        = nota_vinculada-id_filial
     AND id_material      = nota_vinculada-id_material.

  IF NOT it_reme_vinc[] IS INITIAL.

    SELECT * INTO TABLE it_prod_vinc
      FROM zdoc_nf_produtor
       FOR ALL ENTRIES IN it_reme_vinc
     WHERE vbeln EQ it_reme_vinc-id_remessa
       AND docnum_prod EQ nota_vinculada-docnum
       AND itmnum_prod EQ nota_vinculada-itmnum.

    SELECT * INTO TABLE it_prod_recu
      FROM zdoc_exp_rec_nf
       FOR ALL ENTRIES IN it_reme_vinc
     WHERE vbeln_re_exp EQ it_reme_vinc-id_remessa
       AND docnum_prod  EQ nota_vinculada-docnum
       AND itmnum_prod  EQ nota_vinculada-itmnum.

  ENDIF.

  qtd_efetivada = 0.
  qtd_recusada  = 0.

  LOOP AT it_prod_vinc.
    qtd_efetivada = qtd_efetivada + it_prod_vinc-menge.
  ENDLOOP.

  LOOP AT it_prod_recu.
    qtd_recusada = qtd_recusada + it_prod_recu-menge.
  ENDLOOP.

  wa_remetente-nr_programada = wa_remetente-nr_programada - nota_vinculada-nr_quantidade2 - qtd_efetivada  + qtd_recusada.

  qtd_efetivada = qtd_efetivada - qtd_recusada.

  IF qtd_efetivada LE 0.
    DELETE FROM znom_reme_notas
     WHERE id_nomeacao_tran = nota_vinculada-id_nomeacao_tran
       AND id_empresa       = nota_vinculada-id_empresa
       AND id_filial        = nota_vinculada-id_filial
       AND id_material      = nota_vinculada-id_material
       AND id_remetente     = nota_vinculada-id_remetente
       AND docnum           = nota_vinculada-docnum
       AND itmnum           = nota_vinculada-itmnum
       AND grp_retorno      = nota_vinculada-grp_retorno.
  ELSE.

    IF ( qtd_efetivada LE 0 ) AND ( qtd_recusada GT 0 ).
      qtd_efetivada = qtd_recusada.
    ENDIF.

    UPDATE znom_reme_notas
       SET nr_quantidade    = qtd_efetivada
     WHERE id_nomeacao_tran = nota_vinculada-id_nomeacao_tran
       AND id_empresa       = nota_vinculada-id_empresa
       AND id_filial        = nota_vinculada-id_filial
       AND id_material      = nota_vinculada-id_material
       AND id_remetente     = nota_vinculada-id_remetente
       AND docnum           = nota_vinculada-docnum
       AND itmnum           = nota_vinculada-itmnum
       AND grp_retorno      = nota_vinculada-grp_retorno.
  ENDIF.

  IF wa_remetente-nr_programada LE 0.
    DELETE FROM znom_remetente
     WHERE id_nomeacao_tran = nota_vinculada-id_nomeacao_tran
       AND id_empresa       = nota_vinculada-id_empresa
       AND id_filial        = nota_vinculada-id_filial
       AND id_material      = nota_vinculada-id_material
       AND id_remetente     = nota_vinculada-id_remetente
       AND grp_retorno      = nota_vinculada-grp_retorno.
  ELSE.
    MODIFY znom_remetente FROM wa_remetente.
  ENDIF.

  wa_znom_programacao_alv-nr_qtde_remetente = wa_znom_programacao_alv-nr_qtde_remetente - ( nota_vinculada-nr_quantidade2 - qtd_efetivada ).
  wa_znom_programacao_alv-nr_qtde_saldo_rem = wa_znom_programacao_alv-nr_qtde_saldo_rem + ( nota_vinculada-nr_quantidade2 - qtd_efetivada ).
  wa_znom_programacao_alv-nr_qtde_planejada = wa_znom_programacao_alv-nr_qtde_remetente.
  wa_znom_programacao_alv-nr_qtde_saldo_pla = wa_znom_programacao_alv-nr_qtde_saldo_rem.
  MOVE-CORRESPONDING wa_znom_programacao_alv TO wa_znom_programacao_aux.

  READ TABLE it_znom_programacao_alv WITH KEY id_nomeacao_tran = wa_znom_programacao_aux-id_nomeacao_tran
                                              id_empresa       = wa_znom_programacao_aux-id_empresa
                                              id_filial        = wa_znom_programacao_aux-id_filial
                                              id_material      = wa_znom_programacao_aux-id_material.
  IF sy-subrc IS INITIAL.
    vg_tabix = sy-tabix.
    MODIFY it_znom_programacao_alv INDEX vg_tabix FROM wa_znom_programacao_aux
       TRANSPORTING nr_qtde_remetente nr_qtde_saldo_rem nr_qtde_planejada nr_qtde_saldo_pla.
  ENDIF.

  READ TABLE it_znom_notasfiscais_alv WITH KEY docnum = nota_vinculada-docnum
                                               itmnum = nota_vinculada-itmnum.
  IF sy-subrc IS INITIAL.
    vg_tabix = sy-tabix.
    it_znom_notasfiscais_alv-nr_utilizada = it_znom_notasfiscais_alv-nr_utilizada - ( nota_vinculada-nr_quantidade2 - qtd_efetivada ).
    it_znom_notasfiscais_alv-nr_saldo     = it_znom_notasfiscais_alv-nr_saldo     + ( nota_vinculada-nr_quantidade2 - qtd_efetivada ).
**<<<------"163355 - NMS - INI------>>>
    it_znom_notasfiscais_alv-saldo_cct    = it_znom_notasfiscais_alv-saldo_cct    + ( nota_vinculada-nr_quantidade2 - qtd_efetivada ).
**<<<------"163355 - NMS - FIM------>>>
    MODIFY it_znom_notasfiscais_alv INDEX vg_tabix.
**<<<------"163355 - NMS - INI------>>>
    CLEAR: it_znom_notasfiscais_alv.
**<<<------"163355 - NMS - FIM------>>>
  ENDIF.

ENDFORM.                    " DES_VINCULA_NOTA

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM atualiza_tela .

  DATA: t_dynpfields TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1 WITH HEADER LINE.

  MOVE: 'WA_ZNOM_PROGRAMACAO_ALV-NR_QTDE_REMETENTE' TO  t_dynpfields-fieldname,
         wa_znom_programacao_alv-nr_qtde_remetente  TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  MOVE: 'WA_ZNOM_PROGRAMACAO_ALV-NR_QTDE_SALDO_REM' TO  t_dynpfields-fieldname,
         wa_znom_programacao_alv-nr_qtde_saldo_rem  TO  t_dynpfields-fieldvalue.
  APPEND t_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = vg_dynnr_0035
    TABLES
      dynpfields = t_dynpfields.

ENDFORM.                    " ATUALIZA_TELA

*&---------------------------------------------------------------------*
*&      Module  STATUS_0031  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0031 OUTPUT.

  IF vg_dynnr_30xx IS INITIAL.
    vg_dynnr_30xx = vg_dynnr_0034.
    "vg_dynnr_36xx = vg_dynnr_0037.
  ENDIF.

  "Teste 09.07.2018
  IF vg_dynnr_36xx IS INITIAL.
    vg_dynnr_36xx = vg_dynnr_0036.
  ENDIF.

ENDMODULE.                 " STATUS_0031  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  VINCULAR_SALDO_FILIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM vincular_saldo_filial .

  DATA: wa_remetente            TYPE znom_remetente,
        wa_znom_programacao_aux TYPE ty_zplac_nom_programacao,
        vg_tabix                TYPE sy-tabix,
        v_lifnr_aux             TYPE lfa1-lifnr,
        vg_qtd_recusada         TYPE j_1bnetqty,
        v_nr_programada         TYPE znom_remetente-nr_programada.

  "Re-consulta DU-e
  DATA(_error) = abap_false.
  PERFORM f_check_due_filtro CHANGING _error.
  IF _error IS NOT INITIAL.
    LEAVE TO SCREEN 0001.
  ENDIF.

  IF wa_filtro_remetente-numero_due IS NOT INITIAL.
    v_lifnr_aux = wa_znom_programacao_alv-id_filial.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = v_lifnr_aux
      IMPORTING
        output = v_lifnr_aux.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(_wl_lfa1)
     WHERE lifnr = @v_lifnr_aux.

    IF ( sy-subrc NE 0 ) OR ( v_lifnr_aux IS INITIAL  ).
      MESSAGE |Fornecedor: { v_lifnr_aux } não encontrado! | TYPE 'S'.
      LEAVE TO SCREEN 0001.
    ENDIF.

    SELECT SINGLE *
      FROM adrc INTO @DATA(_wl_adrc)
     WHERE addrnumber EQ @_wl_lfa1-adrnr.

    IF sy-subrc NE 0.
      MESSAGE |Endereço Fornecedor: { v_lifnr_aux } não encontrado! | TYPE 'S'.
      LEAVE TO SCREEN 0001.
    ENDIF.

*    IF _WL_ADRC-REGION NE WA_FILTRO_REMETENTE-REGIO.
*      MESSAGE |UF DU-e é diferente da UF da Filial! | TYPE 'S'.
*      LEAVE TO SCREEN 0001.
*    ENDIF.
  ENDIF.

  IF wa_znom_programacao_alv-nr_qtde_saldo_rem GT 0.

** Valida se há nota de produtor para o grupo
    IF it_znom_reme_dnotas_alv[] IS INITIAL. "ALRS
      LOOP AT it_znom_remetente INTO wa_remetente WHERE grp_retorno = wa_filtro_remetente-grp_retorno.
        IF wa_remetente-id_remetente IS NOT INITIAL.
          MESSAGE 'Não é possível vincular saldo próprio em um grupo onde há notas fornecedores.' TYPE 'S'.
          LEAVE TO SCREEN 0001.
        ENDIF.

        "Se não existe na tabela de remetentes ignorar soliciração de desvinculação - 27.09.2018 Ini
        SELECT SINGLE * INTO @DATA(_wl_znom_reme_aux)
          FROM znom_remetente
         WHERE id_nomeacao_tran EQ @wa_remetente-id_nomeacao_tran
           AND id_empresa       EQ @wa_remetente-id_empresa
           AND id_filial        EQ @wa_remetente-id_filial
           AND id_material      EQ @wa_remetente-id_material
           AND grp_retorno      EQ @wa_remetente-grp_retorno
           AND id_remetente     EQ @space.

        IF sy-subrc EQ 0.
          IF ( _wl_znom_reme_aux-docnum_rt IS NOT INITIAL ) OR ( _wl_znom_reme_aux-nr_ordem IS NOT INITIAL ).
            MESSAGE 'A nota não pode ser desvinculada pois já possui DocNum Ret./OV. Export.' TYPE 'S'.
            LEAVE TO SCREEN 0001.
          ENDIF.
        ENDIF.
        "27.09.2018 Fim

      ENDLOOP.
    ENDIF.

    CLEAR wa_remetente.

    READ TABLE it_znom_remetente INTO wa_remetente WITH KEY id_remetente = space
                                                            grp_retorno  = wa_filtro_remetente-grp_retorno.
    IF sy-subrc IS INITIAL.
      "Retirar a parte recusada do volume sem compromisso
      vg_qtd_recusada = 0.

      SELECT SUM( r~nm_quantidade ) INTO vg_qtd_recusada
        FROM zdoc_exp_recusa AS r
        INNER JOIN znom_prog_reme AS e ON e~id_remessa EQ r~vbeln_re_exp
       WHERE e~id_nomeacao_tran EQ wa_remetente-id_nomeacao_tran
         AND e~id_empresa       EQ wa_remetente-id_empresa
         AND e~id_filial        EQ wa_remetente-id_filial
         AND e~id_material      EQ wa_remetente-id_material
         AND NOT EXISTS ( SELECT * FROM zdoc_exp_rec_nf AS n WHERE n~id_doc_exp EQ r~id_doc_exp ).

      IF wa_filtro_remetente-nr_qtd_vinc IS INITIAL.
        wa_remetente-nr_programada = wa_remetente-nr_programada + wa_znom_programacao_alv-nr_qtde_saldo_rem - vg_qtd_recusada.
        wa_filtro_remetente-nr_qtd_vinc = wa_znom_programacao_alv-nr_qtde_saldo_rem. "ALRS
      ELSEIF wa_filtro_remetente-nr_qtd_vinc > wa_znom_programacao_alv-nr_qtde_saldo_rem.
        MESSAGE 'Quantidade informada para vincular é maior que o saldo disponível.' TYPE 'S'.
      ELSE.
        wa_remetente-nr_programada = wa_remetente-nr_programada + wa_filtro_remetente-nr_qtd_vinc - vg_qtd_recusada.
      ENDIF.
      IF it_znom_reme_dnotas_alv[] IS INITIAL. "ALRS
        MODIFY znom_remetente FROM wa_remetente.

        _error = abap_false.
        PERFORM f_check_qtde_prog_due USING wa_filtro_remetente-id_due
                                            wa_znom_programacao_alv-id_nomeacao_tran
                                   CHANGING _error.

        IF _error EQ abap_true.
          ROLLBACK WORK.
          LEAVE TO SCREEN 0001.
        ENDIF.

      ELSE.
        "Remetente
        MOVE-CORRESPONDING wa_remetente TO wa_znom_remetente_log.
        READ TABLE it_znom_remetente_log
         WITH KEY  id_nomeacao_tran = wa_znom_remetente_log-id_nomeacao_tran
                   id_empresa       = wa_znom_remetente_log-id_empresa
                   id_filial        = wa_znom_remetente_log-id_filial
                   id_material      = wa_znom_remetente_log-id_material
                   id_remetente     = wa_znom_remetente_log-id_remetente
                   grp_retorno      = wa_znom_remetente_log-grp_retorno.
        IF sy-subrc = 0.
          wa_znom_remetente_log-novo = 'A'.
          MODIFY it_znom_remetente_log FROM wa_znom_remetente_log INDEX sy-tabix.
        ELSE.
          wa_znom_remetente_log-novo = 'N'.
          APPEND wa_znom_remetente_log TO it_znom_remetente_log.
        ENDIF.
      ENDIF.
      wa_remetente-nr_programada = wa_filtro_remetente-nr_qtd_vinc.
    ELSE.
      CLEAR: wa_remetente.
      wa_remetente-id_nomeacao_tran     = wa_znom_programacao_alv-id_nomeacao_tran.
      wa_remetente-id_empresa           = wa_znom_programacao_alv-id_empresa.
      wa_remetente-id_filial            = wa_znom_programacao_alv-id_filial.
      wa_remetente-id_material          = wa_znom_programacao_alv-id_material.
      wa_remetente-id_unidade           = wa_znom_programacao_alv-id_unidade.

      wa_remetente-id_due               = wa_filtro_remetente-id_due.
      wa_remetente-numero_due           = wa_filtro_remetente-numero_due.
      wa_remetente-codigo_urf_embarque  = wa_filtro_remetente-codigo_urf_embarque.
      wa_remetente-codigo_ra_embarque   = wa_filtro_remetente-codigo_ra_embarque.


**    Define a quantidade a ser vinculada de acordo com o informado na tela ou total disponível da nomeação
      IF wa_filtro_remetente-nr_qtd_vinc IS INITIAL.
        wa_remetente-nr_programada    = wa_znom_programacao_alv-nr_qtde_saldo_rem.
        wa_filtro_remetente-nr_qtd_vinc = wa_znom_programacao_alv-nr_qtde_saldo_rem.
      ELSEIF wa_filtro_remetente-nr_qtd_vinc > wa_znom_programacao_alv-nr_qtde_saldo_rem.
        MESSAGE 'Quantidade informada para vincular é maior que o saldo disponível.' TYPE 'S'.
      ELSE.
        wa_remetente-nr_programada    = wa_filtro_remetente-nr_qtd_vinc.
      ENDIF.

      wa_remetente-grp_retorno      = wa_filtro_remetente-grp_retorno.
      wa_remetente-nr_parte_empresa = 0.

      wa_remetente-tipov = wa_filtro_remetente-tipov. "113637 - CS2023000378 - PSA
      wa_remetente-preco = wa_filtro_remetente-preco. "113637 - CS2023000378 - PSA
      wa_remetente-depst = wa_filtro_remetente-depst. "113637 - CS2023000378 - PSA
      wa_remetente-safra = wa_filtro_remetente-safra. "113637 - CS2023000378 - PSA
      wa_remetente-cvirt = wa_filtro_remetente-cvirt. "113637 - CS2023000378 - PSA

      IF it_znom_reme_dnotas_alv[] IS INITIAL. "ALRS
        MODIFY znom_remetente FROM wa_remetente.

        _error = abap_false.
        PERFORM f_check_qtde_prog_due USING wa_filtro_remetente-id_due
                                            wa_znom_programacao_alv-id_nomeacao_tran
                                   CHANGING _error.

        IF _error EQ abap_true.
          ROLLBACK WORK.
          LEAVE TO SCREEN 0001.
        ENDIF.

      ELSE. "ALRS (saldo)
        "Remetente
        MOVE-CORRESPONDING wa_remetente TO wa_znom_remetente_log.
        wa_znom_remetente_log-docnum_rt = wa_grp_retorno-docnum_rt.
        wa_znom_remetente_log-nr_ordem  = wa_grp_retorno-nr_ordem.
        READ TABLE it_znom_remetente_log
         WITH KEY  id_nomeacao_tran = wa_znom_remetente_log-id_nomeacao_tran
                   id_empresa       = wa_znom_remetente_log-id_empresa
                   id_filial        = wa_znom_remetente_log-id_filial
                   id_material      = wa_znom_remetente_log-id_material
                   id_remetente     = wa_znom_remetente_log-id_remetente
                   grp_retorno      = wa_znom_remetente_log-grp_retorno.
        IF sy-subrc = 0.
          wa_znom_remetente_log-novo = 'A'.
          MODIFY it_znom_remetente_log FROM wa_znom_remetente_log INDEX sy-tabix.
        ELSE.
          wa_znom_remetente_log-novo = 'N'.
          APPEND wa_znom_remetente_log TO it_znom_remetente_log.
        ENDIF.
        "Notas
        CLEAR: wa_znom_reme_notas_alv.
        wa_znom_reme_notas_alv-grp_retorno       = wa_znom_remetente_log-grp_retorno.
        wa_znom_reme_notas_alv-docnum            = '9999999999'.
        wa_znom_reme_notas_alv-itmnum            = '999999'.
        wa_znom_reme_notas_alv-nr_quantidade2    = wa_znom_remetente_log-nr_programada.
        wa_znom_reme_notas_alv-id_unidade        = wa_znom_remetente_log-id_unidade.
        wa_znom_reme_notas_alv-nr_quantidade     = wa_znom_remetente_log-nr_programada.
        wa_znom_reme_notas_alv-parid             = wa_znom_remetente_log-id_filial.
        " WA_ZNOM_REME_NOTAS_ALV-NAME1             = P_J_1BBRANCH-NAME.
        wa_znom_reme_notas_alv-matnr             = wa_znom_programacao_alv-id_material.
        wa_znom_reme_notas_alv-maktx             = wa_znom_programacao_alv-ds_material.
        wa_znom_reme_notas_alv-nbm               = wa_znom_programacao_alv-nbm.
        wa_znom_reme_notas_alv-nr_efetivada      = wa_znom_remetente_log-nr_programada.
        wa_znom_reme_notas_alv-grp_retorno       = wa_znom_remetente_log-grp_retorno.
        wa_znom_reme_notas_alv-nr_saldo_efetivar = 0.
        wa_znom_reme_notas_alv-nr_recusado       = 0.
        APPEND wa_znom_reme_notas_alv TO it_znom_reme_notas_alv.
      ENDIF.
    ENDIF.

    wa_znom_programacao_alv-nr_qtde_remetente = wa_znom_programacao_alv-nr_qtde_remetente + wa_remetente-nr_programada.
    wa_znom_programacao_alv-nr_qtde_saldo_rem = wa_znom_programacao_alv-nr_qtde_saldo_rem - wa_filtro_remetente-nr_qtd_vinc.
    wa_znom_programacao_alv-nr_qtde_planejada = wa_znom_programacao_alv-nr_qtde_remetente.
    wa_znom_programacao_alv-nr_qtde_saldo_pla = wa_znom_programacao_alv-nr_qtde_saldo_rem.

    CLEAR wa_filtro_remetente-nr_qtd_vinc.

    MOVE-CORRESPONDING wa_znom_programacao_alv TO wa_znom_programacao_aux.

    READ TABLE it_znom_programacao_alv WITH KEY id_nomeacao_tran = wa_znom_programacao_aux-id_nomeacao_tran
                                                id_empresa       = wa_znom_programacao_aux-id_empresa
                                                id_filial        = wa_znom_programacao_aux-id_filial
                                                id_material      = wa_znom_programacao_aux-id_material.
    IF sy-subrc IS INITIAL.
      vg_tabix = sy-tabix.
      MODIFY it_znom_programacao_alv INDEX vg_tabix FROM wa_znom_programacao_aux
        TRANSPORTING nr_qtde_remetente nr_qtde_saldo_rem nr_qtde_planejada nr_qtde_saldo_pla.
    ENDIF.

    IF it_znom_reme_dnotas_alv[] IS INITIAL. "ALRS
      PERFORM consulta_remetentes.
    ELSE.
      PERFORM consulta_remetentes_log USING 'N'.
    ENDIF.
  ELSE.
    MESSAGE 'Não há saldo disponível.' TYPE 'S'.
  ENDIF.

ENDFORM.                    " VINCULAR_SALDO_FILIAL

*&---------------------------------------------------------------------*
*&      Form  DES_VINCULAR_SALDO_FILIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM des_vincular_saldo_filial .

  DATA: wa_znom_programacao_aux TYPE ty_zplac_nom_programacao,
        vg_tabix                TYPE sy-tabix,
        wa_znom_remetente_aux   TYPE znom_remetente,
        wa_znom_reme_notas_aux  LIKE it_znom_reme_notas_alv.

  IF it_znom_reme_dnotas_alv[] IS NOT INITIAL. "ALRS
    MESSAGE 'Permitido apenas vincular saldo para troca' TYPE 'I'.
    EXIT.
  ENDIF.
  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row.

  CALL METHOD plan_alv_reme_notas->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE it_znom_reme_notas_alv INTO wa_znom_reme_notas_aux INDEX wa_selected_rows-index.
    LOOP AT it_znom_remetente INTO wa_znom_remetente WHERE id_remetente EQ space
                                                      AND  grp_retorno  EQ wa_znom_reme_notas_aux-grp_retorno.

      "Se não existe na tabela de remetentes ignorar soliciração de desvinculação
      "******************************************************************************************************
      SELECT SINGLE * INTO wa_znom_remetente_aux
        FROM znom_remetente
       WHERE id_nomeacao_tran EQ wa_znom_remetente-id_nomeacao_tran
         AND id_empresa       EQ wa_znom_remetente-id_empresa
         AND id_filial        EQ wa_znom_remetente-id_filial
         AND id_material      EQ wa_znom_remetente-id_material
         AND grp_retorno      EQ wa_znom_remetente-grp_retorno
         AND id_remetente     EQ space.

      CHECK sy-subrc IS INITIAL.

      IF wa_znom_remetente_aux-docnum_rt IS NOT INITIAL
      OR wa_znom_remetente_aux-nr_ordem  IS NOT INITIAL.
        MESSAGE 'A nota não pode ser desvinculada pois já possui DocNum Ret./OV. Export.' TYPE 'E'.
        LEAVE TO SCREEN 0001.
      ENDIF.

      IF wa_znom_remetente-nr_parte_empresa IS INITIAL.
        wa_znom_remetente-nr_parte_empresa = 0.
      ENDIF.
      "******************************************************************************************************
      "******************************************************************************************************
      wa_znom_programacao_alv-nr_qtde_remetente = wa_znom_programacao_alv-nr_qtde_remetente - wa_znom_remetente-nr_programada + wa_znom_remetente-nr_parte_empresa.
      wa_znom_programacao_alv-nr_qtde_saldo_rem = wa_znom_programacao_alv-nr_qtde_saldo_rem + wa_znom_remetente-nr_programada - wa_znom_remetente-nr_parte_empresa.
      wa_znom_programacao_alv-nr_qtde_planejada = wa_znom_programacao_alv-nr_qtde_remetente.
      wa_znom_programacao_alv-nr_qtde_saldo_pla = wa_znom_programacao_alv-nr_qtde_saldo_rem.

      MOVE-CORRESPONDING wa_znom_programacao_alv TO wa_znom_programacao_aux.

      READ TABLE it_znom_programacao_alv WITH KEY id_nomeacao_tran = wa_znom_programacao_aux-id_nomeacao_tran
                                                  id_empresa       = wa_znom_programacao_aux-id_empresa
                                                  id_filial        = wa_znom_programacao_aux-id_filial
                                                  id_material      = wa_znom_programacao_aux-id_material.
      IF sy-subrc IS INITIAL.
        vg_tabix = sy-tabix.
        MODIFY it_znom_programacao_alv INDEX vg_tabix FROM wa_znom_programacao_aux
          TRANSPORTING nr_qtde_remetente nr_qtde_saldo_rem nr_qtde_planejada nr_qtde_saldo_pla.
      ENDIF.

      IF wa_znom_remetente-nr_parte_empresa LE 0.
        DELETE FROM znom_remetente WHERE id_nomeacao_tran = wa_znom_remetente-id_nomeacao_tran
                                     AND id_empresa       = wa_znom_remetente-id_empresa
                                     AND id_filial        = wa_znom_remetente-id_filial
                                     AND id_material      = wa_znom_remetente-id_material
                                     AND grp_retorno      = wa_znom_remetente-grp_retorno
                                     AND id_remetente     = space.
      ELSE.
        UPDATE znom_remetente
           SET nr_programada    = wa_znom_remetente-nr_parte_empresa
         WHERE id_nomeacao_tran = wa_znom_remetente-id_nomeacao_tran
           AND id_empresa       = wa_znom_remetente-id_empresa
           AND id_filial        = wa_znom_remetente-id_filial
           AND id_material      = wa_znom_remetente-id_material
           AND grp_retorno      = wa_znom_remetente-grp_retorno
           AND id_remetente     = space.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  PERFORM consulta_remetentes.

ENDFORM.                    " DES_VINCULAR_SALDO_FILIAL

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0037  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0037 INPUT.

  CASE ok_code_0001.
    WHEN ok_btn_expa.
      CLEAR: ok_code_0001.
      vg_dynnr_36xx = vg_dynnr_0036.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0037  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0036  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0036 INPUT.

  CASE ok_code_0001.
    WHEN ok_btn_mini.
      CLEAR: ok_code_0001.
      vg_dynnr_36xx = vg_dynnr_0037.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0036  INPUT

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_EXECUCAO_FILIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM verifica_execucao_filial  USING  p_filial    TYPE werks_d
                                      p_vg_filial TYPE sy-subrc.

  p_vg_filial = 4.

  AUTHORITY-CHECK OBJECT 'ZPLA_NFISC' ID 'ZPLA_NFISC' FIELD p_filial.
  IF sy-subrc IS INITIAL.
    p_vg_filial = sy-subrc.
  ELSE.
    p_vg_filial = 4.
  ENDIF.

ENDFORM.                    " VERIFICA_EXECUCAO_FILIAL

*&---------------------------------------------------------------------*
*&      Form  F_TIPO_ORDEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_sel_tipo_ordem CHANGING p_auart TYPE vbak-auart.
  DATA: t_fields TYPE TABLE OF sval WITH HEADER LINE.
  CLEAR: t_fields[].

  t_fields-tabname    = 'VBAK'.
*      t_fields-comp_tab   = 'ZNOM_REMETENTE'.
  t_fields-fieldname  = 'AUART'.
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
*     F1_FORMNAME     = ' '
*     F1_PROGRAMNAME  = ' '
      f4_formname     = 'LIST_ORDEM'
      f4_programname  = sy-cprog
*     formname        = 'POPUP_EXIT'
      popup_title     = 'Selecionar tipo'
      programname     = sy-cprog
*     START_COLUMN    = '5'
*     START_ROW       = '5'
*     NO_CHECK_FOR_FIXED_VALUES = ' '
    IMPORTING
      returncode      = p_auart
    TABLES
      fields          = t_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF t_fields-value IS NOT INITIAL.
    MOVE t_fields-value TO p_auart.
  ENDIF.

ENDFORM.                    " INCLUIR_REMESSA

*&---------------------------------------------------------------------*
*&      Form  list_ordem
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TABNAME    text
*      -->FIELDNAME  text
*      -->DISPLAY    text
*      -->RETURNCODE text
*      -->VALUE      text
*----------------------------------------------------------------------*
FORM list_ordem USING tabname fieldname display             "#EC CALLED
                  CHANGING returncode value.
  DATA: BEGIN OF tl_tipo OCCURS 0,
          auart TYPE vbak-auart,
        END OF tl_tipo.

  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  tl_tipo-auart = 'ZEXI'.
  APPEND tl_tipo TO tl_tipo.
  tl_tipo-auart = 'ZEXP'.
  APPEND tl_tipo TO tl_tipo.
  tl_tipo-auart = 'ZEXD'. "80543 -LP
  APPEND tl_tipo TO tl_tipo.

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

ENDFORM.                    "CALL_F4_HELP
*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_REME_NOTAS_DESV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_alv_reme_notas_desv OUTPUT.
  PERFORM plan_cria_reme_notas_dvinc_alv.
ENDMODULE.                 " CRIA_ALV_REME_NOTAS_DESV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  PLAN_CRIA_REME_NOTAS_DVINC_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM plan_cria_reme_notas_dvinc_alv .
  CONSTANTS: tabela_reme_notas TYPE string VALUE 'IT_ZNOM_REME_DNOTAS_ALV'.

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
        text_n018 TYPE c LENGTH 50 VALUE 'Qtd. Rec./Dev.',
        text_n017 TYPE c LENGTH 50 VALUE 'Qtd. Efetivar',
        text_n014 TYPE c LENGTH 50 VALUE 'Dt. Emissão',
        text_n015 TYPE c LENGTH 50 VALUE 'NCM',
        text_n019 TYPE c LENGTH 50 VALUE 'Grp.',
        text_n020 TYPE c LENGTH 50 VALUE 'Seq.',
        text_n021 TYPE c LENGTH 50 VALUE 'Data',
        text_n022 TYPE c LENGTH 50 VALUE 'Hora',
        text_n023 TYPE c LENGTH 50 VALUE 'Usuário'.

  IF plan_container_reme_notlog IS INITIAL.

    CREATE OBJECT plan_container_reme_notlog
      EXPORTING
        container_name = 'CTN_REME_NOTLOG_VINC'.

    CREATE OBJECT plan_alv_reme_notlog
      EXPORTING
        i_parent = plan_container_reme_notlog.

*    CREATE OBJECT TOOLBAR_REME_NOTAS_ALV
*      EXPORTING
*        IO_ALV_GRID = PLAN_ALV_REME_NOTLOG.
*
*    SET HANDLER TOOLBAR_REME_NOTAS_ALV->ON_TOOLBAR FOR PLAN_ALV_REME_NOTAS.
*    SET HANDLER TOOLBAR_REME_NOTAS_ALV->HANDLE_USER_COMMAND FOR PLAN_ALV_REME_NOTAS.

    PERFORM z_estrutura_fieldcat TABLES plan_catalogo_reme_notlog USING:
          tabela_reme_notas 'GRP_RETORNO'       text_n019 ' ' 01 03 space space   space space space space               space,
          tabela_reme_notas 'DOCNUM'            text_n001 'X' 02 10 space 'ALPHA' space 'X'   space c_grid_color_c200 space,
          tabela_reme_notas 'ITMNUM'            text_n002 ' ' 03 06 space space   space 'X'   'X'   space             space,
          tabela_reme_notas 'ID_FILIAL'         text_n003 ' ' 04 04 space space   space space space c_grid_color_c200 space,
          tabela_reme_notas 'MODEL'             text_n004 ' ' 05 03 space space   space space space space             space,
          tabela_reme_notas 'SERIES'            text_n005 ' ' 06 03 space space   space space space c_grid_color_c200 space,
          tabela_reme_notas 'NFENUM'            text_n006 ' ' 07 09 space space   space space space c_grid_color_c200 space,
          tabela_reme_notas 'NR_QUANTIDADE2'    text_n013 ' ' 08 12 space space   'X'   space space c_grid_color_c300 space,
          tabela_reme_notas 'NR_EFETIVADA'      text_n016 ' ' 09 15 space space   'X'   space space c_grid_color_c600 space,
          tabela_reme_notas 'NR_RECUSADO'       text_n018 ' ' 10 15 space space   'X'   space space c_grid_color_recu space,
          tabela_reme_notas 'NR_SALDO_EFETIVAR' text_n017 ' ' 11 15 space space   'X'   space space c_grid_color_c500 space,
          tabela_reme_notas 'DOCDAT'            text_n014 ' ' 12 10 space space   space space space space             space,
          tabela_reme_notas 'PARID'             text_n007 ' ' 13 10 space 'ALPHA' space space space space             space,
          tabela_reme_notas 'NAME1'             text_n008 ' ' 14 25 space space   space space space space             space,
          tabela_reme_notas 'MATNR'             text_n009 ' ' 15 08 space 'ALPHA' space space space space             space,
          tabela_reme_notas 'MAKTX'             text_n010 ' ' 16 25 space space   space space space space             space,
          tabela_reme_notas 'NBM'               text_n015 ' ' 17 10 space space   space space space space             space,
          tabela_reme_notas 'CHARG'             text_n011 ' ' 18 05 space space   space space space space             space,
          tabela_reme_notas 'CFOP'              text_n012 ' ' 19 07 space space   space space space space             space,
          tabela_reme_notas 'SEQ_LOG'           text_n020 ' ' 20 07 space space   space space space space             space,
          tabela_reme_notas 'DATA_ATUAL'        text_n021 ' ' 21 07 space space   space space space space             space,
          tabela_reme_notas 'HORA_ATUAL'        text_n022 ' ' 22 07 space space   space space space space             space,
          tabela_reme_notas 'USUARIO'           text_n023 ' ' 23 07 space space   space space space space             space.

    CLEAR: plan_gs_layout.
    plan_gs_layout-zebra      = c_x.
    plan_gs_layout-sel_mode   = c_a.
    plan_gs_layout-info_fname = 'ROWCOLOR'.

    CLEAR: wa_sort, it_sort[].
    wa_sort-spos      = 1.
    wa_sort-fieldname = 'GRP_RETORNO'.
    wa_sort-up        = 'X'.
    wa_sort-subtot    = space.
    wa_sort-expa      = space.
    APPEND wa_sort TO it_sort.

    CALL METHOD plan_alv_reme_notlog->set_table_for_first_display
      EXPORTING
        i_default       = space
        is_layout       = plan_gs_layout
      CHANGING
        it_fieldcatalog = plan_catalogo_reme_notlog
        it_outtab       = it_znom_reme_dnotas_alv[]
        it_sort         = it_sort[].

*   Create Object for Event Handler
*    CREATE OBJECT PLAN_EVENT_HANDLER_NOTAS2.
*    SET HANDLER: PLAN_EVENT_HANDLER_NOTAS2->HANDLE_HOTSPOT_CLICK_NOTAS2 FOR PLAN_ALV_REME_NOTAS.

  ENDIF.

  CALL METHOD plan_alv_reme_notlog->refresh_table_display.

  CALL METHOD plan_alv_reme_notas->set_scroll_info_via_id
    EXPORTING
      is_col_info = plan_scroll_col
      is_row_no   = plan_scroll_row.

ENDFORM.                    " PLAN_CRIA_REME_NOTAS_DVINC_ALV
*&---------------------------------------------------------------------*
*&      Form  DESVINCULAR_NOTLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM desvincular_notlog .
  DATA: it_selected_rows TYPE lvc_t_row,
        wa_selected_rows TYPE lvc_s_row,
        vg_tabix         TYPE sy-tabix,
        vg_tabix_2       TYPE sy-tabix.

  DATA: wl_znom_rem_aux TYPE znom_remetente.

  CLEAR: it_znom_reme_notas_sel[].

  CALL METHOD plan_alv_reme_notas->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  DELETE it_selected_rows WHERE rowtype IS NOT INITIAL.
  LOOP AT it_selected_rows INTO wa_selected_rows WHERE index IS NOT INITIAL.
    READ TABLE it_znom_reme_notas_alv INTO wa_znom_reme_notas_alv INDEX wa_selected_rows-index.
*    IF WA_ZNOM_REME_NOTAS_ALV-DOCNUM NE '999999999'.
    wa_znom_reme_notas_alv-index = wa_selected_rows-index.
    APPEND wa_znom_reme_notas_alv TO it_znom_reme_notas_sel.
*    ENDIF.
  ENDLOOP.
  IF it_znom_reme_dnotas_alv[] IS INITIAL. "ALRS.
    it_znom_remetente_log[] = it_znom_remetente[]. "Tabela de remetentes original e modifica
  ENDIF.
  LOOP AT it_znom_reme_notas_sel INTO wa_znom_reme_notas_alv.
    vg_tabix_sel = wa_znom_reme_notas_alv-index.
    PERFORM des_vincula_notlog USING wa_znom_reme_notas_alv.
  ENDLOOP.

  "Checa Divergencia em vinculações da nomeação
  CLEAR: wl_znom_rem_aux.
  wl_znom_rem_aux-id_nomeacao_tran = wa_znom_programacao_alv-id_nomeacao_tran.

  DATA(_change) = abap_false.
  PERFORM f_check_reg_rem_vinc USING wl_znom_rem_aux
                            CHANGING _change.

  PERFORM consulta_remetentes_log USING 'D'.

ENDFORM.                    " DESVINCULAR_NOTLOG
*&---------------------------------------------------------------------*
*&      Form  DES_VINCULA_NOTLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZNOM_REME_NOTAS_ALV  text
*----------------------------------------------------------------------*
FORM des_vincula_notlog  USING nota_vinculada TYPE zplac_nom_reme_notas.

  DATA: wa_remetente            TYPE ty_znom_remetente,
        vg_tabix                TYPE sy-tabix,
        vg_tabix_r              TYPE sy-tabix,
        wa_znom_programacao_aux TYPE ty_zplac_nom_programacao,
        it_reme_vinc            TYPE TABLE OF znom_prog_reme WITH HEADER LINE,
        it_prod_vinc            TYPE TABLE OF zdoc_nf_produtor WITH HEADER LINE,
        it_prod_recu            TYPE TABLE OF zdoc_exp_rec_nf WITH HEADER LINE,
        qtd_efetivada           TYPE j_1bnetqty,
        qtd_recusada            TYPE j_1bnetqty,
        nota_recusada           TYPE znom_reme_notas,
        vg_nr_memorando         TYPE zdoc_memorando-numero_memo.

  IF nota_vinculada-docnum NE '9999999999'. "Notas
    "Checar se pode desvincular
    SELECT  SINGLE zdoc_memorando~numero_memo
      FROM zdoc_nf_produtor
      INNER JOIN zdoc_memo_nota
            ON   zdoc_memo_nota~docnum = zdoc_nf_produtor~docnum_prod
            AND  zdoc_memo_nota~itmnum = zdoc_nf_produtor~itmnum_prod
      INNER JOIN znom_prog_reme
            ON   znom_prog_reme~id_nomeacao_tran = nota_vinculada-id_nomeacao_tran
            AND  znom_prog_reme~id_empresa       = nota_vinculada-id_empresa
            AND  znom_prog_reme~id_filial        = nota_vinculada-id_filial
            AND  znom_prog_reme~id_remessa       = zdoc_nf_produtor~vbeln
      INNER JOIN vbfa
            ON   vbfa~vbelv  =  zdoc_nf_produtor~vbeln
            AND  vbfa~vbtyp_n = 'M'
            AND  vbfa~vbtyp_v = 'J'
      INNER JOIN j_1bnflin
            ON   j_1bnflin~refkey = vbfa~vbeln
      INNER JOIN zdoc_memo_nf_exp
            ON   zdoc_memo_nf_exp~docnum = j_1bnflin~docnum
      INNER JOIN zdoc_memorando
            ON   zdoc_memorando~nr_nota_exp  = zdoc_memo_nf_exp~nr_nota_exp
            AND  zdoc_memorando~nr_memorando = zdoc_memo_nota~nr_memorando
      INTO vg_nr_memorando
      WHERE docnum_prod = nota_vinculada-docnum
      AND   itmnum_prod = nota_vinculada-itmnum.

    IF sy-subrc = 0.
      MESSAGE i000(z01) WITH
                             nota_vinculada-docnum
                             'com memorando gerado, eliminar memorando '
                             vg_nr_memorando
                             ' antes de continuar!'.
      EXIT.
    ENDIF.

    READ TABLE  it_znom_remetente_log INTO wa_remetente
    WITH KEY  id_nomeacao_tran = nota_vinculada-id_nomeacao_tran
       id_empresa       = nota_vinculada-id_empresa
       id_filial        = nota_vinculada-id_filial
       id_material      = nota_vinculada-id_material
       id_remetente     = nota_vinculada-id_remetente
       grp_retorno      = nota_vinculada-grp_retorno.

  ELSE. "Saldo
    READ TABLE  it_znom_remetente_log INTO wa_remetente
    WITH KEY  id_remetente     = ''
              grp_retorno      = nota_vinculada-grp_retorno.
*    IF WA_REMETENTE-DOCNUM_RT IS NOT INITIAL
*      OR WA_REMETENTE-NR_ORDEM  IS NOT INITIAL.
*      MESSAGE 'A nota não pode ser desvinculada pois já possui DocNum Ret./OV. Export.' TYPE 'I'.
*      LEAVE TO SCREEN 0001.
*      EXIT.
*    ENDIF.
  ENDIF.
  vg_tabix_r = sy-tabix.

************************************************************************************************
* move para tela de notas/saldo trocados
************************************************************************************************
  MOVE-CORRESPONDING nota_vinculada TO wa_znom_reme_dnotas_alv.
  wa_znom_reme_dnotas_alv-docnum_rt = wa_remetente-docnum_rt.
  wa_znom_reme_dnotas_alv-nr_ordem  = wa_remetente-nr_ordem.
  APPEND wa_znom_reme_dnotas_alv TO it_znom_reme_dnotas_alv.
************************************************************************************************

  IF nota_vinculada-docnum NE '9999999999'. "Notas
    wa_remetente-nr_programada =  wa_remetente-nr_programada - nota_vinculada-nr_quantidade2.

    DELETE it_znom_reme_notas_alv
     WHERE id_nomeacao_tran = nota_vinculada-id_nomeacao_tran
       AND id_empresa       = nota_vinculada-id_empresa
       AND id_filial        = nota_vinculada-id_filial
       AND id_material      = nota_vinculada-id_material
       AND id_remetente     = nota_vinculada-id_remetente
       AND docnum           = nota_vinculada-docnum
       AND itmnum           = nota_vinculada-itmnum
       AND grp_retorno      = nota_vinculada-grp_retorno.

    DELETE it_znom_reme_notas
       WHERE id_nomeacao_tran = nota_vinculada-id_nomeacao_tran
         AND id_empresa       = nota_vinculada-id_empresa
         AND id_filial        = nota_vinculada-id_filial
         AND id_material      = nota_vinculada-id_material
         AND id_remetente     = nota_vinculada-id_remetente
         AND docnum           = nota_vinculada-docnum
         AND itmnum           = nota_vinculada-itmnum
         AND grp_retorno      = nota_vinculada-grp_retorno.

    IF wa_remetente-nr_programada LE 0.
      wa_remetente-novo = 'D'. "Eliminado
      MODIFY it_znom_remetente_log FROM wa_remetente INDEX vg_tabix_r TRANSPORTING novo.
    ELSE.
      MOVE-CORRESPONDING wa_remetente TO wa_znom_remetente_log.
      wa_znom_remetente_log-novo = 'A'. "alterado valores
      MODIFY it_znom_remetente_log FROM wa_znom_remetente_log INDEX vg_tabix_r.
    ENDIF.

    wa_znom_programacao_alv-nr_qtde_remetente = wa_znom_programacao_alv-nr_qtde_remetente - ( nota_vinculada-nr_quantidade2  ).
    wa_znom_programacao_alv-nr_qtde_saldo_rem = wa_znom_programacao_alv-nr_qtde_saldo_rem + ( nota_vinculada-nr_quantidade2  ).
    wa_znom_programacao_alv-nr_qtde_planejada = wa_znom_programacao_alv-nr_qtde_remetente.
    wa_znom_programacao_alv-nr_qtde_saldo_pla = wa_znom_programacao_alv-nr_qtde_saldo_rem.
  ELSE.
    IF wa_remetente-nr_parte_empresa IS INITIAL.
      wa_remetente-nr_parte_empresa = 0.
    ENDIF.
    wa_remetente-nr_programada =  wa_remetente-nr_programada - nota_vinculada-nr_quantidade2.

    DELETE it_znom_reme_notas_alv
     WHERE id_nomeacao_tran = nota_vinculada-id_nomeacao_tran
       AND id_empresa       = nota_vinculada-id_empresa
       AND id_filial        = nota_vinculada-id_filial
       AND id_material      = nota_vinculada-id_material
       AND id_remetente     = nota_vinculada-id_remetente
       AND docnum           = nota_vinculada-docnum
       AND itmnum           = nota_vinculada-itmnum
       AND grp_retorno      = nota_vinculada-grp_retorno.

    IF wa_remetente-nr_programada LE 0.
      wa_remetente-novo = 'D'. "Eliminado
      MODIFY it_znom_remetente_log FROM wa_remetente INDEX vg_tabix_r TRANSPORTING novo.
    ELSE.
      MOVE-CORRESPONDING wa_remetente TO wa_znom_remetente_log.
      wa_znom_remetente_log-novo = 'A'. "alterado valores
      MODIFY it_znom_remetente_log FROM wa_znom_remetente_log INDEX vg_tabix_r.
    ENDIF.
    wa_znom_programacao_alv-nr_qtde_remetente = wa_znom_programacao_alv-nr_qtde_remetente - ( nota_vinculada-nr_quantidade2  ).
    wa_znom_programacao_alv-nr_qtde_saldo_rem = wa_znom_programacao_alv-nr_qtde_saldo_rem + ( nota_vinculada-nr_quantidade2  ).
    wa_znom_programacao_alv-nr_qtde_planejada = wa_znom_programacao_alv-nr_qtde_remetente.
    wa_znom_programacao_alv-nr_qtde_saldo_pla = wa_znom_programacao_alv-nr_qtde_saldo_rem.
  ENDIF.

  MOVE-CORRESPONDING wa_znom_programacao_alv TO wa_znom_programacao_aux.

  READ TABLE it_znom_programacao_alv WITH KEY id_nomeacao_tran = wa_znom_programacao_aux-id_nomeacao_tran
                                              id_empresa       = wa_znom_programacao_aux-id_empresa
                                              id_filial        = wa_znom_programacao_aux-id_filial
                                              id_material      = wa_znom_programacao_aux-id_material.
  IF sy-subrc IS INITIAL.
    vg_tabix = sy-tabix.
    MODIFY it_znom_programacao_alv INDEX vg_tabix FROM wa_znom_programacao_aux
       TRANSPORTING nr_qtde_remetente nr_qtde_saldo_rem nr_qtde_planejada nr_qtde_saldo_pla.
  ENDIF.

  IF nota_vinculada-docnum NE '9999999999'. "Notas
    READ TABLE it_znom_notasfiscais_alv WITH KEY docnum = nota_vinculada-docnum
                                                 itmnum = nota_vinculada-itmnum.
    IF sy-subrc IS INITIAL.
      vg_tabix = sy-tabix.
      it_znom_notasfiscais_alv-nr_utilizada = it_znom_notasfiscais_alv-nr_utilizada - ( nota_vinculada-nr_quantidade2 ).
      it_znom_notasfiscais_alv-nr_saldo     = it_znom_notasfiscais_alv-nr_saldo     + ( nota_vinculada-nr_quantidade2 ).
      MODIFY it_znom_notasfiscais_alv INDEX vg_tabix.
    ENDIF.
  ENDIF.

ENDFORM.                    " DES_VINCULA_NOTLOG
*&---------------------------------------------------------------------*
*&      Form  GRAVAR_NOTLOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gravar_notlog.
  DATA: it_znom_reme_notas_log    TYPE TABLE OF zplac_nom_reme_notas,
        wa_znom_reme_notas_log    TYPE          zplac_nom_reme_notas,

        it_znom_reme_dnotas_sld   TYPE TABLE OF zplac_nom_reme_notlog WITH HEADER LINE,


        it_znom_reme_notas_nov    TYPE TABLE OF znom_reme_notas,
        wa_znom_reme_notas_nov    TYPE          znom_reme_notas,
        it_znom_reme_notlog       TYPE TABLE OF znom_reme_notlog, "log
        wa_znom_reme_notlog       TYPE          znom_reme_notlog,

        it_zdoc_nf_prod_log       TYPE TABLE OF zdoc_nf_produtor,
        it_zdoc_nf_prod_log_aux   TYPE TABLE OF zdoc_nf_produtor,
        it_zdoc_nf_prod_nov       TYPE TABLE OF zdoc_nf_produtor,
        wa_zdoc_nf_prod_log       TYPE          zdoc_nf_produtor,
        wa_zdoc_nf_prod_nov       TYPE          zdoc_nf_produtor,
        it_zdoc_nf_prod_log1      TYPE TABLE OF zdoc_nf_prod_log, "Log
        wa_zdoc_nf_prod_log1      TYPE          zdoc_nf_prod_log,

        it_znom_remetente_nov     TYPE TABLE OF znom_remetente,
        wa_znom_remetente_nov     TYPE          znom_remetente,

        it_znom_remetente_log_aux TYPE TABLE OF ty_znom_remetente WITH HEADER LINE,

        it_znom_remetente_log1    TYPE TABLE OF znom_reme_log, "Log
        wa_znom_remetente_log1    TYPE          znom_reme_log,


        vg_menge                  TYPE zdoc_nf_produtor-menge,
        vg_nr_quantidade2         TYPE j_1bnetqty,
        vg_seq_log                TYPE znom_reme_notlog-seq_log,
        tabix1                    TYPE sy-tabix,
        tabix2                    TYPE sy-tabix.

  "versão de alterações
  READ TABLE it_znom_reme_dnotas_alv INTO wa_znom_reme_dnotas_alv INDEX 1.
  SELECT SINGLE MAX( seq_log )
    FROM znom_reme_notlog
    INTO vg_seq_log
   WHERE id_nomeacao_tran  = wa_znom_reme_dnotas_alv-id_nomeacao_tran
   AND   id_empresa        = wa_znom_reme_dnotas_alv-id_empresa
   AND   id_filial         = wa_znom_reme_dnotas_alv-id_filial.


  ADD 1 TO vg_seq_log.

  DELETE it_znom_reme_dnotas_alv WHERE seq_log NE 0. "eliminar log antigo
  it_znom_reme_notas_log[]   = it_znom_reme_notas_alv[].  "Novos - Notas
  it_znom_reme_dnotas_sld[]  = it_znom_reme_dnotas_alv[]. " Antigo - Saldo
  DELETE it_znom_reme_dnotas_sld WHERE docnum NE '9999999999'.

  DELETE it_znom_reme_notas_log   WHERE novo NE 'N'.
  DELETE it_znom_reme_notas_log   WHERE docnum EQ '9999999999'.

  IF it_znom_reme_dnotas_alv[] IS NOT INITIAL.
    SELECT *
      FROM zdoc_nf_produtor
      INTO TABLE it_zdoc_nf_prod_log
      FOR ALL ENTRIES IN it_znom_reme_dnotas_alv "Antigos
      WHERE docnum_prod = it_znom_reme_dnotas_alv-docnum
      AND   itmnum_prod = it_znom_reme_dnotas_alv-itmnum
      AND   grp_retorno = it_znom_reme_dnotas_alv-grp_retorno.
  ENDIF.

  SORT: it_znom_reme_notas_log  BY docnum itmnum, " Novos
        it_znom_reme_dnotas_alv BY docnum itmnum, " Antigos
        it_zdoc_nf_prod_log     BY docnum_prod itmnum_prod. " Antigos por remessa

  " Redistribui quantidades nas mesmas remessas
  REFRESH it_zdoc_nf_prod_nov.
  PERFORM troca_aba_04 USING space.

  " Elimina Remessas que não pertencem a nomeação
  LOOP AT it_zdoc_nf_prod_log INTO wa_zdoc_nf_prod_log.
    tabix1 = sy-tabix.
    READ TABLE it_znom_prog_reme_alv WITH KEY id_remessa = wa_zdoc_nf_prod_log-vbeln.
    IF sy-subrc NE 0.
      wa_zdoc_nf_prod_log-mandt = 999.
      MODIFY it_zdoc_nf_prod_log FROM wa_zdoc_nf_prod_log INDEX tabix1 TRANSPORTING mandt.
    ENDIF.
  ENDLOOP.
  DELETE it_zdoc_nf_prod_log WHERE mandt = 999.

  it_zdoc_nf_prod_log_aux[] = it_zdoc_nf_prod_log[].
  it_znom_remetente_log_aux[] = it_znom_remetente_log[].

  DELETE it_znom_remetente_log WHERE novo = 'D'. "elimina deletados
  DELETE it_znom_remetente_log WHERE id_remetente = ''.

  LOOP AT it_znom_reme_notas_log INTO wa_znom_reme_notas_log. "novos - Notas
    vg_nr_quantidade2 = wa_znom_reme_notas_log-nr_quantidade2.
    tabix1 = sy-tabix.
    " busca relacionamento com notas antigas para gerar as novas
    LOOP AT it_zdoc_nf_prod_log INTO wa_zdoc_nf_prod_log. "Antigos por remessa
      IF vg_nr_quantidade2 LT 0.
        EXIT.
      ENDIF.
      vg_menge = wa_zdoc_nf_prod_log-menge.
      IF vg_menge GT 0.
        tabix2 = sy-tabix.
        IF vg_menge LE vg_nr_quantidade2.
          wa_zdoc_nf_prod_nov-vbeln         = wa_zdoc_nf_prod_log-vbeln.
          wa_zdoc_nf_prod_nov-docnum_prod   = wa_znom_reme_notas_log-docnum. "novo.
          wa_zdoc_nf_prod_nov-itmnum_prod   = wa_znom_reme_notas_log-itmnum. "novo
          wa_zdoc_nf_prod_nov-grp_retorno   = wa_zdoc_nf_prod_log-grp_retorno.
          wa_zdoc_nf_prod_nov-menge         = vg_menge.  "novo
          COLLECT wa_zdoc_nf_prod_nov INTO it_zdoc_nf_prod_nov.
          "
          vg_nr_quantidade2 = vg_nr_quantidade2 - vg_menge.
          wa_znom_reme_notas_log-nr_quantidade2 = vg_nr_quantidade2.
          MODIFY it_znom_reme_notas_log FROM wa_znom_reme_notas_log INDEX tabix1 TRANSPORTING nr_quantidade2.
          wa_zdoc_nf_prod_log-menge = 0.
          MODIFY it_zdoc_nf_prod_log FROM wa_zdoc_nf_prod_log INDEX tabix2 TRANSPORTING menge.
        ELSE.
          vg_menge = vg_menge - vg_nr_quantidade2.
          wa_zdoc_nf_prod_nov-vbeln         = wa_zdoc_nf_prod_log-vbeln.
          wa_zdoc_nf_prod_nov-docnum_prod   = wa_znom_reme_notas_log-docnum. "novo.
          wa_zdoc_nf_prod_nov-itmnum_prod   = wa_znom_reme_notas_log-itmnum. "novo
          wa_zdoc_nf_prod_nov-grp_retorno   = wa_zdoc_nf_prod_log-grp_retorno.
          wa_zdoc_nf_prod_nov-menge         = vg_nr_quantidade2.  "novo
          COLLECT wa_zdoc_nf_prod_nov INTO it_zdoc_nf_prod_nov.
          "
          vg_nr_quantidade2 = 0.
          wa_znom_reme_notas_log-nr_quantidade2 = 0.
          MODIFY it_znom_reme_notas_log FROM wa_znom_reme_notas_log INDEX tabix1 TRANSPORTING nr_quantidade2.
          wa_zdoc_nf_prod_log-menge = vg_menge.
          MODIFY it_zdoc_nf_prod_log FROM wa_zdoc_nf_prod_log INDEX tabix2 TRANSPORTING menge.
        ENDIF.
      ENDIF.
    ENDLOOP.
    "
    " busca relacionamento com saldo da remessa para gerar novas
    READ TABLE it_znom_remetente_log INTO wa_znom_remetente_log
              WITH KEY  id_nomeacao_tran = wa_znom_reme_notas_log-id_nomeacao_tran
              id_empresa       = wa_znom_reme_notas_log-id_empresa
              id_filial        = wa_znom_reme_notas_log-id_filial
              id_material      = wa_znom_reme_notas_log-id_material
              id_remetente     = wa_znom_reme_notas_log-id_remetente
              grp_retorno      = wa_znom_reme_notas_log-grp_retorno.

    IF sy-subrc = 0.
      tabix2 = sy-tabix.

      READ TABLE it_znom_reme_dnotas_sld
            WITH KEY docnum_rt = wa_znom_reme_notas_log-docnum_rt
            nr_ordem           = wa_znom_reme_notas_log-nr_ordem
            grp_retorno        = wa_znom_reme_notas_log-grp_retorno.


      IF sy-subrc = 0. "se existir saldo para substituir por nota
        LOOP AT it_znom_prog_reme_alv WHERE id_nomeacao_tran EQ wa_znom_remetente_log-id_nomeacao_tran
                                     AND id_empresa        EQ wa_znom_remetente_log-id_empresa
                                     AND id_filial         EQ wa_znom_remetente_log-id_filial
                                     AND id_material       EQ wa_znom_remetente_log-id_material
                                     AND vbelv             EQ wa_znom_remetente_log-nr_ordem.
          IF vg_nr_quantidade2 LT 0.
            EXIT.
          ENDIF.
          vg_menge = wa_znom_remetente_log-nr_programada. "IT_ZNOM_PROG_REME_ALV-NTGEW.
          IF vg_menge GT 0.
            IF vg_menge LE vg_nr_quantidade2.
              wa_zdoc_nf_prod_nov-vbeln         = it_znom_prog_reme_alv-id_remessa.
              wa_zdoc_nf_prod_nov-docnum_prod   = wa_znom_reme_notas_log-docnum. "novo.
              wa_zdoc_nf_prod_nov-itmnum_prod   = wa_znom_reme_notas_log-itmnum. "novo
              wa_zdoc_nf_prod_nov-grp_retorno   = wa_znom_reme_notas_log-grp_retorno.
              wa_zdoc_nf_prod_nov-menge         = vg_menge.  "novo
              COLLECT wa_zdoc_nf_prod_nov INTO it_zdoc_nf_prod_nov.
              "
              vg_nr_quantidade2 = vg_nr_quantidade2 - vg_menge.
              wa_znom_reme_notas_log-nr_quantidade2 = vg_nr_quantidade2.
              MODIFY it_znom_reme_notas_log FROM wa_znom_reme_notas_log INDEX tabix1 TRANSPORTING nr_quantidade2.
              wa_znom_remetente_log-nr_programada = 0.
              MODIFY it_znom_remetente_log FROM wa_znom_remetente_log INDEX tabix2 TRANSPORTING nr_programada.
              EXIT.
            ELSE.
              vg_menge = vg_menge - vg_nr_quantidade2.
              wa_zdoc_nf_prod_nov-vbeln         = it_znom_prog_reme_alv-id_remessa.
              wa_zdoc_nf_prod_nov-docnum_prod   = wa_znom_reme_notas_log-docnum. "novo.
              wa_zdoc_nf_prod_nov-itmnum_prod   = wa_znom_reme_notas_log-itmnum. "novo
              wa_zdoc_nf_prod_nov-grp_retorno   = wa_znom_reme_notas_log-grp_retorno.
              wa_zdoc_nf_prod_nov-menge         = vg_nr_quantidade2.  "novo
              COLLECT wa_zdoc_nf_prod_nov INTO it_zdoc_nf_prod_nov.
              "
              vg_nr_quantidade2 = 0.
              wa_znom_reme_notas_log-nr_quantidade2 = 0.
              MODIFY it_znom_reme_notas_log FROM wa_znom_reme_notas_log INDEX tabix1 TRANSPORTING nr_quantidade2.
              wa_znom_remetente_log-nr_programada = vg_menge.
              MODIFY it_znom_remetente_log FROM wa_znom_remetente_log INDEX tabix2 TRANSPORTING nr_programada.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  it_znom_remetente_log[] = it_znom_remetente_log_aux[].

  DELETE it_zdoc_nf_prod_nov WHERE menge = 0.

************************************************************************************************
* ZNOM_REMETENTE
************************************************************************************************
  "Apaga ANTIGOS ZNOM_REMETENTE
  LOOP AT  it_znom_remetente_log INTO wa_znom_remetente_log.
    IF  wa_znom_remetente_log-novo EQ 'D'.
      DELETE FROM znom_remetente
        WHERE id_nomeacao_tran = wa_znom_remetente_log-id_nomeacao_tran
          AND id_empresa       = wa_znom_remetente_log-id_empresa
          AND id_filial        = wa_znom_remetente_log-id_filial
          AND id_material      = wa_znom_remetente_log-id_material
          AND id_remetente     = wa_znom_remetente_log-id_remetente
          AND grp_retorno      = wa_znom_remetente_log-grp_retorno.

      IF sy-subrc = 0.
        MOVE-CORRESPONDING wa_znom_remetente_log TO wa_znom_remetente_log1.
        wa_znom_remetente_log1-seq_log     = vg_seq_log.
        wa_znom_remetente_log1-data_atual  = sy-datum.
        wa_znom_remetente_log1-hora_atual  = sy-uzeit.
        wa_znom_remetente_log1-usuario     = sy-uname.
        APPEND wa_znom_remetente_log1 TO it_znom_remetente_log1.
      ENDIF.
    ELSEIF wa_znom_remetente_log-novo EQ 'A'.
      MOVE-CORRESPONDING wa_znom_remetente_log TO wa_znom_remetente_log1.
      wa_znom_remetente_log1-seq_log     = vg_seq_log.
      wa_znom_remetente_log1-data_atual  = sy-datum.
      wa_znom_remetente_log1-hora_atual  = sy-uzeit.
      wa_znom_remetente_log1-usuario     = sy-uname.
      APPEND wa_znom_remetente_log1 TO it_znom_remetente_log1.
    ENDIF.

  ENDLOOP.

  LOOP AT  it_znom_remetente_log INTO wa_znom_remetente_log. "novos
    IF  wa_znom_remetente_log-novo NE 'D'.
      MOVE-CORRESPONDING wa_znom_remetente_log  TO wa_znom_remetente_nov.
      APPEND wa_znom_remetente_nov TO it_znom_remetente_nov.
    ENDIF.
  ENDLOOP.

  "Grava novos
  IF it_znom_remetente_nov[] IS NOT INITIAL.
    MODIFY znom_remetente FROM TABLE it_znom_remetente_nov.
  ENDIF.

  "Grava Log Antigas
  IF it_znom_remetente_log1[] IS NOT INITIAL.
    MODIFY znom_reme_log FROM TABLE it_znom_remetente_log1.
  ENDIF.
************************************************************************************************
* ZDOC_NF_PRODUTOR
************************************************************************************************
  "Apaga antigos ZDOC_NF_PRODUTOR
  LOOP AT it_zdoc_nf_prod_log_aux INTO wa_zdoc_nf_prod_log. "Antigos por remessa
    DELETE FROM zdoc_nf_produtor
      WHERE vbeln       = wa_zdoc_nf_prod_log-vbeln
      AND   docnum_prod = wa_zdoc_nf_prod_log-docnum_prod
      AND   itmnum_prod = wa_zdoc_nf_prod_log-itmnum_prod
      AND   grp_retorno = wa_zdoc_nf_prod_log-grp_retorno.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_zdoc_nf_prod_log TO wa_zdoc_nf_prod_log1.
      wa_zdoc_nf_prod_log1-seq_log     = vg_seq_log.
      wa_zdoc_nf_prod_log1-data_atual  = sy-datum.
      wa_zdoc_nf_prod_log1-hora_atual  = sy-uzeit.
      wa_zdoc_nf_prod_log1-usuario     = sy-uname.
      APPEND wa_zdoc_nf_prod_log1 TO it_zdoc_nf_prod_log1.
    ENDIF.
  ENDLOOP.

  "Grava novos
  IF it_zdoc_nf_prod_nov[] IS NOT INITIAL.
    MODIFY zdoc_nf_produtor FROM TABLE it_zdoc_nf_prod_nov.
  ENDIF.

  "Grava Log Antigas
  IF it_zdoc_nf_prod_log1[] IS NOT INITIAL.
    MODIFY zdoc_nf_prod_log FROM TABLE it_zdoc_nf_prod_log1.
  ENDIF.

************************************************************************************************
* ZNOM_REME_NOTAS
************************************************************************************************
  "Apaga antigos ZNOM_REME_NOTAS
  LOOP AT it_znom_reme_dnotas_alv INTO wa_znom_reme_dnotas_alv.
    DELETE FROM znom_reme_notas
    WHERE id_nomeacao_tran  = wa_znom_reme_dnotas_alv-id_nomeacao_tran
    AND   id_empresa        = wa_znom_reme_dnotas_alv-id_empresa
    AND   id_filial         = wa_znom_reme_dnotas_alv-id_filial
    AND   id_material       = wa_znom_reme_dnotas_alv-id_material
    AND   id_remetente      = wa_znom_reme_dnotas_alv-id_remetente
    AND   docnum            = wa_znom_reme_dnotas_alv-docnum
    AND   itmnum            = wa_znom_reme_dnotas_alv-itmnum
    AND   grp_retorno       = wa_znom_reme_dnotas_alv-grp_retorno.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING wa_znom_reme_dnotas_alv TO wa_znom_reme_notlog.
      wa_znom_reme_notlog-seq_log     = vg_seq_log.
      wa_znom_reme_notlog-data_atual  = sy-datum.
      wa_znom_reme_notlog-hora_atual  = sy-uzeit.
      wa_znom_reme_notlog-usuario     = sy-uname.
      APPEND wa_znom_reme_notlog TO it_znom_reme_notlog.
    ENDIF.
  ENDLOOP.

  LOOP AT it_znom_reme_notas_log INTO wa_znom_reme_notas_log. "novos
    MOVE-CORRESPONDING wa_znom_reme_notas_log TO wa_znom_reme_notas_nov.
    APPEND wa_znom_reme_notas_nov TO it_znom_reme_notas_nov.
  ENDLOOP.

  "Grava Novo
  IF it_znom_reme_notas_nov[] IS NOT INITIAL.
    MODIFY znom_reme_notas FROM TABLE it_znom_reme_notas_nov.
  ENDIF.
  "Grava Log Antigas
  IF it_znom_reme_notlog[] IS NOT INITIAL.
    MODIFY znom_reme_notlog FROM TABLE it_znom_reme_notlog.
  ENDIF.

************************************************************************************************
* Fim Gravação
************************************************************************************************
  REFRESH it_znom_reme_dnotas_alv.
  PERFORM consulta_remetentes.
ENDFORM.                    " GRAVAR_NOTLOG
*&---------------------------------------------------------------------*
*&      Module  STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0050 OUTPUT.
  SET PF-STATUS 'PF0050'.
  SET TITLEBAR 'TL0050'.

ENDMODULE.                 " STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0050 INPUT.
  CASE ok_code_0050.
    WHEN 'SAIR'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_0050 OUTPUT.
  DATA: event     TYPE cntl_simple_event,
        tl_filter TYPE lvc_t_filt,
        wl_filter TYPE lvc_s_filt,

        wa_layout TYPE lvc_s_layo,
        wa_stable TYPE lvc_s_stbl.


  DATA: obg_conteiner_notas TYPE REF TO cl_gui_custom_container,
        obg_conteiner_rem   TYPE REF TO cl_gui_custom_container,
        obg_conteiner_prod  TYPE REF TO cl_gui_custom_container,
        grid1               TYPE REF TO cl_gui_alv_grid,
        grid2               TYPE REF TO cl_gui_alv_grid,
        grid3               TYPE REF TO cl_gui_alv_grid.

** Criação de tabela dinamica
  DATA: t_fieldcatalog TYPE lvc_t_fcat,
        w_fieldcatalog TYPE lvc_s_fcat.

  wa_layout-zebra      = c_x.
  wa_layout-no_rowmark = c_x.
  wa_stable-row        = c_x.
  wa_layout-no_toolbar = c_x.
  wa_layout-grid_title = ' '.
  "GRID1
  IF obg_conteiner_notas IS INITIAL.
    CREATE OBJECT obg_conteiner_notas
      EXPORTING
        container_name = 'CC_NOTAS'.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = obg_conteiner_notas.

    PERFORM montar_layout_notas.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        is_layout       = wa_layout
      CHANGING
        it_filter       = tl_filter
        it_fieldcatalog = t_fieldcatalog[]
        it_outtab       = tg_notas[].
  ELSE.
    PERFORM montar_layout_notas.
    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
  "
  "GRID2
  IF obg_conteiner_rem IS INITIAL.
    CREATE OBJECT obg_conteiner_rem
      EXPORTING
        container_name = 'CC_REM'.

    CREATE OBJECT grid2
      EXPORTING
        i_parent = obg_conteiner_rem.

    PERFORM montar_layout_reme.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout       = wa_layout
      CHANGING
        it_filter       = tl_filter
        it_fieldcatalog = t_fieldcatalog[]
        it_outtab       = tg_reme[].
  ELSE.
    PERFORM montar_layout_reme.
    CALL METHOD grid2->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  "GRID3
  IF obg_conteiner_prod IS INITIAL.
    CREATE OBJECT obg_conteiner_prod
      EXPORTING
        container_name = 'CC_PROD'.

    CREATE OBJECT grid3
      EXPORTING
        i_parent = obg_conteiner_prod.

    PERFORM montar_layout_prod.

    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout       = wa_layout
      CHANGING
        it_filter       = tl_filter
        it_fieldcatalog = t_fieldcatalog[]
        it_outtab       = tg_prod[].
  ELSE.
    PERFORM montar_layout_prod.
    CALL METHOD grid3->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS_0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_NOTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_notas .
  REFRESH t_fieldcatalog.

  PERFORM montar_estrutura USING:
          1 '' ' '        'TG_NOTAS'   'GRP_RETORNO'      'Grupo'           '05' ' ' ' ' ' ',
          1 '' ' '        'TG_NOTAS'   'ID_MATERIAL'      'Material'        '10' ' ' ' ' ' ',
          1 '' ' '        'TG_NOTAS'   'ID_REMETENTE'     'Remetente'       '10' ' ' ' ' ' ',
          1 '' ' '        'TG_NOTAS'   'DOCNUM'           'Nr. SAP'         '10' ' ' ' ' ' ',
          1 '' ' '        'TG_NOTAS'   'ITMNUM'           'Nr. Item'        '05' ' ' ' ' ' ',
          1 '' ' '        'TG_NOTAS'   'SEQ_LOG'          'Seq.Alt'         '05' ' ' ' ' ' ',
          1 '' ' '        'TG_NOTAS'   'NR_QUANTIDADE'    'Quantidade'      '10' ' ' ' ' ' ',
          1 '' ' '        'TG_NOTAS'   'DATA_ATUAL'       'Data'            '10' ' ' ' ' ' ',
          1 '' ' '        'TG_NOTAS'   'HORA_ATUAL'       'Hora'            '10' ' ' ' ' ' ',
          1 '' ' '        'TG_NOTAS'   'USUARIO'          'Usuário'         '10' ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_NOTAS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.

  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_REM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_reme .
  REFRESH t_fieldcatalog.

  PERFORM montar_estrutura USING:
          1 '' ' '        'TG_REME'   'GRP_RETORNO'      'Grupo'           '05' ' ' ' ' ' ',
          1 '' ' '        'TG_REME'   'ID_MATERIAL'      'Material'        '10' ' ' ' ' ' ',
          1 '' ' '        'TG_REME'   'ID_REMETENTE'     'Remetente'       '10' ' ' ' ' ' ',
          1 '' ' '        'TG_REME'   'SEQ_LOG'          'Seq.Alt'         '05' ' ' ' ' ' ',
          1 '' ' '        'TG_REME'   'NR_PROGRAMADA'    'Quantidade'      '10' ' ' ' ' ' ',
          1 '' ' '        'TG_REME'   'DOCNUM_RT'        'DocNum Ret'      '10' ' ' ' ' ' ',
          1 '' ' '        'TG_REME'   'NR_ORDEM'         'OV.Export'       '10' ' ' ' ' ' ',
          1 '' ' '        'TG_REME'   'DATA_ATUAL'       'Data'            '10' ' ' ' ' ' ',
          1 '' ' '        'TG_REME'   'HORA_ATUAL'       'Hora'            '10' ' ' ' ' ' ',
          1 '' ' '        'TG_REME'   'USUARIO'          'Usuário'         '10' ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_REM
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_prod .
  REFRESH t_fieldcatalog.

  PERFORM montar_estrutura USING:
          1 '' ' '        'TG_PROD'   'GRP_RETORNO'      'Grupo'           '05' ' ' ' ' ' ',
          1 '' ' '        'TG_PROD'   'VBELN'            'Remessa'         '10' ' ' ' ' ' ',
          1 '' ' '        'TG_PROD'   'DOCNUM_PROD'      'Nr.Sap'          '10' ' ' ' ' ' ',
          1 '' ' '        'TG_PROD'   'ITMNUM_PROD'      'Nr.Item'         '05' ' ' ' ' ' ',
          1 '' ' '        'TG_PROD'   'SEQ_LOG'          'Seq.Alt'         '05' ' ' ' ' ' ',
          1 '' ' '        'TG_PROD'   'MENGE'            'Quantidade'      '10' ' ' ' ' ' ',
          1 '' ' '        'TG_PROD'   'DATA_ATUAL'       'Data'            '10' ' ' ' ' ' ',
          1 '' ' '        'TG_PROD'   'HORA_ATUAL'       'Hora'            '10' ' ' ' ' ' ',
          1 '' ' '        'TG_PROD'   'USUARIO'          'Usuário'         '10' ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_PROD



FORM f_get_due_remessa  TABLES p_zsdt0170 STRUCTURE it_due_antecipada_alv
                        USING  p_id_nomeacao_tran
                               p_vbeln.

  DATA: gt_cfop TYPE TABLE OF zmemo_cfop WITH HEADER LINE.

  DATA: v_beln_inp         TYPE lips-vbeln,
        lt_zsdt0170        TYPE TABLE OF zsdt0170 WITH HEADER LINE,
        lt_id_due          TYPE TABLE OF zsdt0170-id_due WITH HEADER LINE,
        lt_znom_remetente  TYPE TABLE OF znom_remetente WITH HEADER LINE,
        lt_znom_reme_notas TYPE TABLE OF znom_reme_notas WITH HEADER LINE.

  CLEAR: p_zsdt0170[], lt_znom_remetente[], lt_znom_reme_notas[], lt_id_due[].

  CHECK ( p_vbeln IS NOT INITIAL ) AND (  p_id_nomeacao_tran IS NOT INITIAL ).

  CLEAR: v_beln_inp.

  CALL FUNCTION 'Z_MEMO_CFOP_SAIDA'
    EXPORTING
      exp_propria = 'X'
    TABLES
      cfops       = gt_cfop.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_vbeln
    IMPORTING
      output = v_beln_inp.

  SELECT SINGLE *
    FROM lips INTO @DATA(_wl_lips)
   WHERE vbeln EQ @v_beln_inp.

  CHECK ( sy-subrc EQ 0 ) AND ( _wl_lips-vgbel IS NOT INITIAL ).

  SELECT SINGLE *
    FROM vbap INTO @DATA(_wl_vbap)
   WHERE vbeln EQ @_wl_lips-vgbel
     AND posnr EQ @_wl_lips-vgpos.

  CHECK ( sy-subrc EQ 0 ).

  SELECT *
    FROM znom_remetente INTO TABLE lt_znom_remetente
   WHERE id_nomeacao_tran EQ p_id_nomeacao_tran
     AND nr_ordem         EQ _wl_lips-vgbel.

  CHECK lt_znom_remetente[] IS NOT INITIAL.

  DATA(_proprio) = abap_false.
  READ TABLE gt_cfop WITH KEY low = _wl_vbap-j_1bcfop.
  IF sy-subrc NE 0.
    _proprio = abap_true.
  ENDIF.

  IF _proprio EQ abap_true.
    ""WPP 23102024 - US-153330 --->>>
*    LOOP AT lt_znom_remetente WHERE ( id_remetente IS INITIAL     )
*                                AND ( id_due       IS NOT INITIAL ).
    LOOP AT lt_znom_remetente WHERE ( id_due IS NOT INITIAL ).
      "WPP 23102024 - US-153330 <<<----
      lt_id_due = lt_znom_remetente-id_due.
      APPEND lt_id_due.
    ENDLOOP.
  ELSE.
    SELECT *
      FROM znom_reme_notas INTO TABLE lt_znom_reme_notas
       FOR ALL ENTRIES IN lt_znom_remetente
     WHERE id_nomeacao_tran EQ lt_znom_remetente-id_nomeacao_tran
       AND id_empresa       EQ lt_znom_remetente-id_empresa
       AND id_filial        EQ lt_znom_remetente-id_filial
       AND id_material      EQ lt_znom_remetente-id_material
       AND id_remetente     EQ lt_znom_remetente-id_remetente
       AND grp_retorno      EQ lt_znom_remetente-grp_retorno.

    LOOP AT lt_znom_reme_notas WHERE id_due IS NOT INITIAL.
      lt_id_due = lt_znom_reme_notas-id_due.
      APPEND lt_id_due.
    ENDLOOP.
  ENDIF.

  LOOP AT lt_id_due.
    CLEAR: p_zsdt0170.

    SELECT SINGLE *
      FROM zsdt0170 INTO @DATA(_wl_zsdt0170)
     WHERE id_due EQ @lt_id_due.

    CHECK sy-subrc EQ 0.

    p_zsdt0170-id_due        = _wl_zsdt0170-id_due.
    p_zsdt0170-tp_exportacao = _wl_zsdt0170-tp_exportacao.
    p_zsdt0170-numero_due    = _wl_zsdt0170-numero_due.

    SELECT SUM( peso_liq_total )
      FROM zsdt0172 INTO p_zsdt0170-peso_liq_total
     WHERE id_due = _wl_zsdt0170-id_due.

    APPEND p_zsdt0170.
  ENDLOOP.

  SORT p_zsdt0170 BY id_due.
  DELETE ADJACENT DUPLICATES FROM p_zsdt0170 COMPARING id_due.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_DUE_FILTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P__ERROR  text
*----------------------------------------------------------------------*
FORM f_check_due_filtro  CHANGING p_error.

  CHECK wa_filtro_remetente-numero_due IS NOT INITIAL.

  SELECT SINGLE id_due
                numero_due
                codigo_urf_embarque
                codigo_ra_embarque
         INTO  ( wa_filtro_remetente-id_due,
                 wa_filtro_remetente-numero_due ,
                 wa_filtro_remetente-codigo_urf_embarque ,
                 wa_filtro_remetente-codigo_ra_embarque )
    FROM zsdt0170
   WHERE numero_due EQ wa_filtro_remetente-numero_due
     AND status     EQ '1'
     AND tp_due     EQ '1' "Sem NF-e
     AND id_due_ref EQ 0.
  "PSA
*     "RMI - CS1086413 - IR135677 - ajustado a consulta para que desconsidere registros que foram marcados para eliminação - inicio
*     AND loekz      NE 'X'.
*    "RMI - CS1086413 - IR135677 - fim

  IF ( sy-subrc NE 0 ) OR
     ( wa_filtro_remetente-id_due              IS INITIAL ) OR
     ( wa_filtro_remetente-numero_due          IS INITIAL ) OR
     ( wa_filtro_remetente-codigo_urf_embarque IS INITIAL ) OR
     ( wa_filtro_remetente-codigo_ra_embarque  IS INITIAL ).
    p_error = abap_true.
    MESSAGE TEXT-055 TYPE 'I'.
  ENDIF.

ENDFORM.

FORM f_atrib_inf_cct CHANGING p_nf_alv TYPE zplac_notasfiscais.

  DATA: wl_zlest0146 TYPE zlest0146,
        lt_zlest0147 TYPE zlest0147_t,
        lt_zlest0168 TYPE zlest0168_t,
        v_doc_rateio TYPE char01.

  CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
    EXPORTING
      i_docnum            = p_nf_alv-docnum
      i_itmnum            = p_nf_alv-itmnum
      i_set_peso_disp_uso = abap_true
    IMPORTING
      e_zlest0146         = wl_zlest0146
      e_zlest0147         = lt_zlest0147
      e_zlest0168         = lt_zlest0168
      e_doc_rateio        = v_doc_rateio.

  CHECK wl_zlest0146 IS NOT INITIAL.

  IF v_doc_rateio IS INITIAL.
    p_nf_alv-cct_prod = abap_true.
  ENDIF.

  IF wa_filtro_remetente-numero_due IS NOT INITIAL.
    CHECK ( wl_zlest0146-local_codigo_ra EQ wa_filtro_remetente-codigo_ra_embarque ).
  ENDIF.

  "Definir qual NF chegou no porto(Produtor ou RFL), quantidade do CCT, e data de chegada.
  IF v_doc_rateio IS INITIAL.
*-------------------------------------------------------------------------------------------------*
*   Nota Fiscal do Produtor chegou no porto.
*-------------------------------------------------------------------------------------------------*
    p_nf_alv-dt_chegada       = wl_zlest0146-dt_recepcao.
    p_nf_alv-peso_cct         = wl_zlest0146-peso_disponivel_uso.
    p_nf_alv-peso_aferido_cct = wl_zlest0146-peso_aferido_recepcao.
    p_nf_alv-nf_chegada_porto = c_pro. "Produtor
  ELSE.
*-------------------------------------------------------------------------------------------------*
*   Notas Fiscais do Produtor vinculada a uma Formação de Lote que chegou no porto.
*-------------------------------------------------------------------------------------------------*
    "Definitivo Ini.
*    READ TABLE LT_ZLEST0168 INTO DATA(_WL_0168) INDEX 1.
*    CHECK SY-SUBRC EQ 0.
*
*    P_NF_ALV-DT_CHEGADA       = WL_ZLEST0146-DT_RECEPCAO.
*    P_NF_ALV-PESO_CCT         = _WL_0168-PESO_AFERIDO.
*    P_NF_ALV-NF_CHEGADA_PORTO = C_RFL. "Remessa Formação Lote
    "Definitivo Fim.
  ENDIF.


ENDFORM.

FORM f_check_reg_rem_vinc  USING p_nom_remetente TYPE znom_remetente
                        CHANGING c_change.

  DATA: tg_znom_remetente_aux  TYPE TABLE OF znom_remetente  WITH HEADER LINE,
        tg_znom_reme_notas_aux TYPE TABLE OF znom_reme_notas WITH HEADER LINE,
        tg_znom_remetente_ck   TYPE TABLE OF znom_remetente  WITH HEADER LINE.

  DATA: v_nr_quantidade TYPE znom_reme_notas-nr_quantidade.

  CLEAR: c_change, tg_znom_remetente_ck[].

  "Valida nomeação inteira
  CHECK ( p_nom_remetente-id_nomeacao_tran IS NOT INITIAL ).

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_nom_remetente-id_nomeacao_tran
    IMPORTING
      output = p_nom_remetente-id_nomeacao_tran.

  "Deletar Registros Remetentes, onde não possui NF correspondente vinculada
  CLEAR: tg_znom_remetente_aux[], tg_znom_reme_notas_aux[].

  SELECT *
    FROM znom_remetente AS a INTO TABLE tg_znom_remetente_aux
   WHERE a~id_nomeacao_tran EQ p_nom_remetente-id_nomeacao_tran
     AND a~id_remetente     NE ' '
     AND NOT EXISTS (
       SELECT *
         FROM znom_reme_notas AS b
        WHERE b~id_nomeacao_tran = a~id_nomeacao_tran
          AND b~id_empresa       = a~id_empresa
          AND b~id_filial        = a~id_filial
          AND b~id_material      = a~id_material
          AND b~id_remetente     = a~id_remetente
          AND b~grp_retorno      = a~grp_retorno ).

  LOOP AT tg_znom_remetente_aux.
    c_change = abap_true.
    DELETE FROM znom_remetente WHERE id_nomeacao_tran EQ tg_znom_remetente_aux-id_nomeacao_tran
                                 AND id_empresa       EQ tg_znom_remetente_aux-id_empresa
                                 AND id_filial        EQ tg_znom_remetente_aux-id_filial
                                 AND id_material      EQ tg_znom_remetente_aux-id_material
                                 AND id_remetente     EQ tg_znom_remetente_aux-id_remetente
                                 AND grp_retorno      EQ tg_znom_remetente_aux-grp_retorno.
  ENDLOOP.

  "Deletar Registros Notas, onde não possui Remetentes correspondente vinculados
  SELECT *
    FROM znom_reme_notas AS a INTO TABLE tg_znom_reme_notas_aux
   WHERE a~id_nomeacao_tran EQ p_nom_remetente-id_nomeacao_tran
     AND a~id_remetente     NE ' '
     AND NOT EXISTS (
       SELECT *
         FROM znom_remetente AS b
        WHERE b~id_nomeacao_tran = a~id_nomeacao_tran
          AND b~id_empresa       = a~id_empresa
          AND b~id_filial        = a~id_filial
          AND b~id_material      = a~id_material
          AND b~id_remetente     = a~id_remetente
          AND b~grp_retorno      = a~grp_retorno ).

  LOOP AT tg_znom_reme_notas_aux.
    c_change = abap_true.
    DELETE FROM znom_reme_notas WHERE id_nomeacao_tran EQ tg_znom_reme_notas_aux-id_nomeacao_tran
                                  AND id_empresa        EQ tg_znom_reme_notas_aux-id_empresa
                                  AND id_filial         EQ tg_znom_reme_notas_aux-id_filial
                                  AND id_material       EQ tg_znom_reme_notas_aux-id_material
                                  AND id_remetente      EQ tg_znom_reme_notas_aux-id_remetente
                                  AND grp_retorno       EQ tg_znom_reme_notas_aux-grp_retorno.
  ENDLOOP.

  DATA(_ck_nomeacao) = abap_false.

  IF ( p_nom_remetente-id_nomeacao_tran IS NOT INITIAL ) AND
     ( p_nom_remetente-id_empresa       IS NOT INITIAL ) AND
     ( p_nom_remetente-id_filial        IS NOT INITIAL ) AND
     ( p_nom_remetente-id_material      IS NOT INITIAL ) AND
     ( p_nom_remetente-id_remetente     IS NOT INITIAL ).

    APPEND p_nom_remetente TO tg_znom_remetente_ck.
  ELSE.
    SELECT *
      FROM znom_remetente AS a INTO TABLE tg_znom_remetente_ck
     WHERE id_nomeacao_tran EQ p_nom_remetente-id_nomeacao_tran
       AND id_remetente     NE ' '
       AND nr_ordem         EQ space.

    _ck_nomeacao = abap_true.
  ENDIF.

  "Check Quantidades Grupos Remetente
  LOOP AT tg_znom_remetente_ck.

    SELECT SINGLE *
      FROM znom_remetente INTO @DATA(_wl_znom_remetente)
     WHERE id_nomeacao_tran = @tg_znom_remetente_ck-id_nomeacao_tran
       AND id_empresa       = @tg_znom_remetente_ck-id_empresa
       AND id_filial        = @tg_znom_remetente_ck-id_filial
       AND id_material      = @tg_znom_remetente_ck-id_material
       AND id_remetente     = @tg_znom_remetente_ck-id_remetente
       AND grp_retorno      = @tg_znom_remetente_ck-grp_retorno.

    CHECK sy-subrc EQ 0.

    CLEAR: v_nr_quantidade.
    SELECT SUM( nr_quantidade )
      FROM znom_reme_notas INTO v_nr_quantidade
     WHERE id_nomeacao_tran = _wl_znom_remetente-id_nomeacao_tran
       AND id_empresa       = _wl_znom_remetente-id_empresa
       AND id_filial        = _wl_znom_remetente-id_filial
       AND id_material      = _wl_znom_remetente-id_material
       AND id_remetente     = _wl_znom_remetente-id_remetente
       AND grp_retorno      = _wl_znom_remetente-grp_retorno.

    IF _wl_znom_remetente-nr_programada <> v_nr_quantidade.
      _wl_znom_remetente-nr_programada = v_nr_quantidade.
      MODIFY znom_remetente FROM _wl_znom_remetente.
      c_change = abap_true.
    ENDIF.

  ENDLOOP.

  IF _ck_nomeacao = abap_true.

    "Check Quantidade Programação Filial x Retorno
    CLEAR: tg_znom_remetente_ck[].
    SELECT *
       FROM znom_remetente AS a INTO TABLE tg_znom_remetente_ck
      WHERE id_nomeacao_tran EQ p_nom_remetente-id_nomeacao_tran
        AND id_remetente     EQ space
        AND nr_ordem         EQ space
        AND docnum_rt        NE ' '.

    LOOP AT tg_znom_remetente_ck WHERE docnum_rt IS NOT INITIAL.

      SELECT SINGLE *
        FROM znom_remetente INTO _wl_znom_remetente
       WHERE id_nomeacao_tran EQ tg_znom_remetente_ck-id_nomeacao_tran
         AND id_empresa       EQ tg_znom_remetente_ck-id_empresa
         AND id_filial        EQ tg_znom_remetente_ck-id_filial
         AND id_material      EQ tg_znom_remetente_ck-id_material
         AND id_remetente     EQ tg_znom_remetente_ck-id_remetente
         AND grp_retorno      EQ tg_znom_remetente_ck-grp_retorno.

      CHECK sy-subrc EQ 0.

      SELECT SINGLE *
        FROM j_1bnflin INTO @DATA(_lin_ret)
       WHERE docnum EQ @tg_znom_remetente_ck-docnum_rt.

      IF ( sy-subrc EQ 0 ).
        IF _wl_znom_remetente-nr_programada <> _lin_ret-menge.
          _wl_znom_remetente-nr_programada = _lin_ret-menge.
          MODIFY znom_remetente FROM _wl_znom_remetente.
          c_change = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.

FORM f_valida_nf_prod_rfl CHANGING p_nf_alv TYPE zplac_notasfiscais.

  DATA: v_rom_completo      TYPE char01,
        v_cct_cp            TYPE char01,
        wl_zlest0146_cp     TYPE zlest0146,
        it_zsdt0001_ro_vinc TYPE zsdt0001_ro_vinc_t.

  CHECK wa_filtro_remetente-tp_vinc2 EQ abap_true AND
        wa_filtro_remetente-s_cct EQ abap_true.    "Sem Registro CCT.

  CHECK wa_filtro_remetente-show_rfl EQ abap_true OR wa_filtro_remetente-ck_cct_cp EQ abap_true.

  CALL FUNCTION 'ZROMANEIO_VINCULADO_NF'
    EXPORTING
      i_docnum            = p_nf_alv-docnum
      i_ck_cct_cp         = abap_true
      i_ck_cfop_e_zmemo00 = abap_true
    IMPORTING
      e_zsdt0001_ro_vinc  = it_zsdt0001_ro_vinc
      e_cct_cp            = v_cct_cp
      e_zlest0146_cp      = wl_zlest0146_cp
      e_romaneio_completo = v_rom_completo.

  IF ( wa_filtro_remetente-ck_cct_cp EQ abap_true ) AND "Checar CCT Contrapartida
     ( v_rom_completo                EQ abap_true ) AND "É Romaneio Completo
     ( it_zsdt0001_ro_vinc[]         IS INITIAL   ).    "Não faturou romaneio de saída, e não achou documento fiscal da RFL.
    p_nf_alv-del_reg = abap_true.
    RETURN.
  ENDIF.

  IF v_rom_completo EQ abap_true.
    p_nf_alv-rom_completo = icon_okay.
  ENDIF.

  CHECK ( it_zsdt0001_ro_vinc[] IS NOT INITIAL ). "Romaneio Completo

  READ TABLE it_zsdt0001_ro_vinc INTO DATA(_wl_rom_vinc) INDEX 1.

  CHECK ( sy-subrc EQ 0 ) AND ( _wl_rom_vinc-docnum_vinc IS NOT INITIAL ).

  p_nf_alv-docnum_rfl = _wl_rom_vinc-docnum_vinc.

  IF p_nf_alv-docnum_rfl IS NOT INITIAL.
    SELECT SINGLE a~* INTO @DATA(_wl_ret)
      FROM zsdt_retlote AS a INNER JOIN zsdt_export AS b ON a~docnum_ret = b~docnum
     WHERE a~docnum     EQ @p_nf_alv-docnum_rfl
       AND b~finalidade EQ 'I'. "Industrialização

    IF ( sy-subrc EQ 0 ).
      p_nf_alv-ind_rfl = icon_okay.
    ENDIF.
  ENDIF.

  IF v_cct_cp EQ abap_true.
    p_nf_alv-cct_rfl    = icon_okay.
    p_nf_alv-dt_chegada = wl_zlest0146_cp-dt_recepcao.
  ENDIF.

  "Checar CCT Contrapartida
  CHECK wa_filtro_remetente-ck_cct_cp EQ abap_true.

  IF p_nf_alv-cct_rfl EQ abap_false.
    p_nf_alv-del_reg = abap_true.
    RETURN.
  ENDIF.

  IF wa_filtro_remetente-numero_due IS NOT INITIAL.
    IF ( wl_zlest0146_cp-local_codigo_ra NE wa_filtro_remetente-codigo_ra_embarque ).
      p_nf_alv-del_reg = abap_true.
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.


FORM f_valida_restricoes_nf CHANGING p_nf_alv TYPE zplac_notasfiscais.

  DATA: v_chave   TYPE zde_chave_nfe,
        v_retorno TYPE zde_retorno_proc.

  "Se entrada Propria e Form. Lote tem CCT, não aplicar restrição
  IF ( p_nf_alv-entrad IS NOT INITIAL ) AND ( p_nf_alv-cct_rfl EQ icon_okay ).
    RETURN.
  ENDIF.

*** CS2019000714 inicio
*  IF wa_filtro_remetente-tp_vinc2 EQ abap_true AND
*     wa_filtro_remetente-s_cct    EQ abap_true.    "Sem Registro CCT.
*    IF ( p_nf_alv-entrad IS NOT INITIAL ) AND ( p_nf_alv-cct_rfl NE icon_okay ).
*      p_nf_alv-restricao = 'Entrada própria não pode ser vinculada sem Registro CCT'.
*
*      RETURN.
*    ENDIF.
*  ENDIF.
*** CS2019000714 fim

  IF ( p_nf_alv-model EQ '55' ) AND ( p_nf_alv-entrad IS INITIAL ).

    CALL FUNCTION 'ZCCT_MONTA_CHAVE_DOCUMENTO'
      EXPORTING
        i_docnum    = p_nf_alv-docnum
      IMPORTING
        e_chave_nfe = v_chave
        e_retorno   = v_retorno.

    IF ( v_retorno-type EQ 'E' ).
      p_nf_alv-restricao = v_retorno-texto.
      RETURN.
    ENDIF.

    IF ( v_chave IS INITIAL ) OR ( strlen( v_chave ) NE 44 ).
      p_nf_alv-restricao = 'Não foi possível obter a chave NF-e do documento'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zib_nfe_dist_itm INTO @DATA(_wl_zib_nfe_dist_itm)
     WHERE chave_nfe EQ @v_chave.

    IF sy-subrc NE 0.
      p_nf_alv-restricao = 'Não foi possível obter o item do XML do documento'.
      RETURN.
    ENDIF.

    REPLACE ALL OCCURRENCES OF '.' IN  _wl_zib_nfe_dist_itm-prod_ncm WITH space.
    CONDENSE _wl_zib_nfe_dist_itm-prod_ncm NO-GAPS.

    TRANSLATE _wl_zib_nfe_dist_itm-prod_und_trib TO UPPER CASE.

    IF wa_filtro_remetente-codigo_ncm NE _wl_zib_nfe_dist_itm-prod_ncm.
      p_nf_alv-restricao = 'NCM DU-e: ' && wa_filtro_remetente-codigo_ncm && ' - NCM Xml: ' && _wl_zib_nfe_dist_itm-prod_ncm.
      RETURN.
    ENDIF.

    IF wa_filtro_remetente-ue_exportada NE _wl_zib_nfe_dist_itm-prod_und_trib.
      p_nf_alv-restricao = 'Un.Estatística DU-e: ' && wa_filtro_remetente-ue_exportada && ' - Un.Trib.Xml: ' && _wl_zib_nfe_dist_itm-prod_und_trib.
      RETURN.
    ENDIF.

  ENDIF.

ENDFORM.

FORM f_check_qtde_prog_due  USING p_id_due            TYPE zsdt0170-id_due
                                  p_id_nomeacao_tran  TYPE zsdt0170-id_nomeacao_tran
                         CHANGING c_error.

  DATA: v_nr_quantidade TYPE znom_reme_notas-nr_quantidade,
        v_nr_programada TYPE znom_remetente-nr_programada,
        v_tot_planejado TYPE znom_reme_notas-nr_quantidade.

  c_error = abap_false.

  CLEAR: v_nr_quantidade, v_nr_programada, v_tot_planejado.

  CHECK ( p_id_due IS NOT INITIAL ) AND ( p_id_nomeacao_tran IS NOT INITIAL ).

  SELECT SINGLE *
    FROM zsdt0172 INTO @DATA(_wl_0172)
   WHERE id_due EQ @p_id_due.

  IF sy-subrc NE 0.
    c_error = abap_true.
    MESSAGE |Item DU-e Id: { p_id_due } não encontrado!| TYPE 'I'.
    RETURN.
  ENDIF.

  SELECT SUM( nr_quantidade )
    FROM znom_reme_notas INTO v_nr_quantidade
   WHERE id_nomeacao_tran EQ p_id_nomeacao_tran
     AND id_due           EQ p_id_due.

  SELECT SUM( nr_programada )
    FROM znom_remetente INTO v_nr_programada
   WHERE id_nomeacao_tran EQ p_id_nomeacao_tran
     AND id_due           EQ p_id_due
     AND id_remetente     EQ space.

  v_tot_planejado = v_nr_quantidade + v_nr_programada.

  IF v_tot_planejado > _wl_0172-peso_liq_total.
    c_error = abap_true.
    MESSAGE |Quantidade planejada p/ a DU-e: foi excedida! Quantidade Planejada { v_tot_planejado } - Quantidade DU-e: { _wl_0172-peso_liq_total } !| TYPE 'I'.
    RETURN.
  ENDIF.

ENDFORM.

FORM f_converte_parid_br_to_lf CHANGING p_parid  TYPE j_1bnfdoc-parid
                                        p_partyp TYPE j_1bnfdoc-partyp
                                        p_parvw  TYPE j_1bnfdoc-parvw.

  CHECK p_partyp EQ 'B'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_parid
    IMPORTING
      output = p_parid.

  p_parid  = p_parid+6(4).

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_parid
    IMPORTING
      output = p_parid.

  p_partyp = 'V'.
  p_parvw  = 'LF'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATRIB_INF_CCE_RFL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<FS_NF_ALV>  text
*----------------------------------------------------------------------*
FORM f_atrib_inf_cte_rfl  CHANGING p_nf_alv TYPE zplac_notasfiscais.

*  CHECK 1 EQ 2.

  SELECT SINGLE obj_key  FROM zmmt_ee_zgr_docs
    INTO @DATA(v_obj_key)
    WHERE docnum   EQ @p_nf_alv-docnum.

  CHECK v_obj_key IS NOT INITIAL.

  SELECT SINGLE ch_referencia  FROM zmmt_ee_zgr
    INTO @DATA(v_ch_referencia)
    WHERE obj_key   EQ @v_obj_key.

  CHECK v_ch_referencia IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_zsdt0001_aux)
    FROM zsdt0001
   WHERE ch_referencia EQ @v_ch_referencia
    AND tp_movimento = 'E'.


  FREE: it_zsdt0001.
  LOOP AT it_zsdt0001_aux INTO DATA(wa_zsdt0001_aux).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = wa_zsdt0001_aux-nr_romaneio
      IMPORTING
        output = wa_zsdt0001-id_referencia.

    wa_zsdt0001-branch = wa_zsdt0001_aux-branch.
    wa_zsdt0001-nr_safra  = wa_zsdt0001_aux-nr_safra.

    APPEND wa_zsdt0001 TO it_zsdt0001.
    CLEAR: wa_zsdt0001 .

  ENDLOOP.

  CHECK it_zsdt0001[] IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_zsdt0001_doc)
     FROM zsdt0001
      FOR ALL ENTRIES IN @it_zsdt0001
    WHERE tp_movimento = 'S'
      AND branch = @it_zsdt0001-branch
      AND nr_safra = @it_zsdt0001-nr_safra
      AND id_referencia = @it_zsdt0001-id_referencia.

  READ TABLE it_zsdt0001_doc INTO DATA(wa_zsdt0001_doc) INDEX 1.

  IF sy-subrc = 0 AND wa_zsdt0001_doc-doc_transp IS NOT INITIAL.
    MOVE icon_okay TO p_nf_alv-icone.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form fields_catalog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> T_FIELDCATALOG
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
*FORM fields_catalog  TABLES   p_t_fieldcatalog STRUCTURE tl_fieldcatalog
*                               USING    fp_fieldname TYPE any
*                               fp_text TYPE any.
*  tl_fieldcatalog-tabname = 'TL_LOG_OV'.
*  tl_fieldcatalog-fieldname = fp_fieldname.
*  tl_fieldcatalog-seltext_l = fp_text.
*  APPEND tl_fieldcatalog.
*  CLEAR tl_fieldcatalog.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0072 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0072 OUTPUT.

  SET PF-STATUS 'ZPLAN0072'.
  SET TITLEBAR 'ZPLANT0072'.

  PERFORM init_alv_log_ov.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form init_alv_log_ov
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init_alv_log_ov .

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container EXPORTING container_name = g_container.
    CREATE OBJECT g_grid EXPORTING i_parent = g_custom_container.
  ENDIF.

  PERFORM build_fieldcatalog.
  PERFORM toolbar_alv.

  l_stable-row          = abap_true.
  l_stable-col          = abap_true.

  w_layout-zebra        = abap_false.
  w_layout-cwidth_opt   = abap_true.
  w_layout-no_totarr    = abap_true.
  w_layout-no_totexp    = abap_true.
  w_layout-no_totline   = abap_true.
  w_layout-no_toolbar   = abap_false.

  " SET_TABLE_FOR_FIRST_DISPLAY
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout            = w_layout
      it_toolbar_excluding = t_function
      i_save               = 'U' "abap_true
    CHANGING
      it_fieldcatalog      = t_fieldcatalog
*     it_sort              = lt_sort
      it_outtab            = tl_log_ov.

*  CALL METHOD g_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*  CALL METHOD g_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_enter.


*  IF m_event_handler IS INITIAL.
*    CREATE OBJECT m_event_handler.
*    SET HANDLER : m_event_handler->toolbar         FOR g_grid.
*    SET HANDLER : m_event_handler->user_command    FOR g_grid.
*    SET HANDLER : m_event_handler->on_data_changed FOR g_grid.
*  ENDIF.

*SET HANDLER: lcl_event_handler=>on_data_changed4 FOR g_grid.
*              lcl_event_handler=>on_double_click  FOR g_grid.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = l_stable.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_fieldcatalog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_fieldcatalog .

  FREE: t_fieldcatalog.


  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'TL_LOG_ALV'.
  ls_fieldcatalog-fieldname = 'VBELN'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 1.
  ls_fieldcatalog-outputlen = 15.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-coltext   = 'Ordem de venda'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'TL_LOG_ALV'.
  ls_fieldcatalog-fieldname = 'POSNR'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 2.
  ls_fieldcatalog-outputlen = 15.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-coltext   = 'Item OV'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'TL_LOG_ALV'.
  ls_fieldcatalog-fieldname = 'TBNAM'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 3.
  ls_fieldcatalog-outputlen = 15.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-coltext   = 'Tabela'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'TL_LOG_ALV'.
  ls_fieldcatalog-fieldname = 'FDNAM'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 4.
  ls_fieldcatalog-outputlen = 15.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-coltext   = 'Campo'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'TL_LOG_ALV'.
  ls_fieldcatalog-fieldname = 'FEHGR'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 5.
  ls_fieldcatalog-outputlen = 15.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-coltext   = 'Esq. dados incompletos'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'TL_LOG_ALV'.
  ls_fieldcatalog-fieldname = 'STATG'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 6.
  ls_fieldcatalog-outputlen = 15.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-coltext   = 'Grupo de status'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.

  CLEAR ls_fieldcatalog.
  ls_fieldcatalog-tabname   = 'TL_LOG_ALV'.
  ls_fieldcatalog-fieldname = 'MESSAGE'.
  ls_fieldcatalog-ref_table = ''.
  ls_fieldcatalog-ref_field = ''.
  ls_fieldcatalog-col_pos   = 7.
  ls_fieldcatalog-outputlen = 15.
  ls_fieldcatalog-dd_outlen = 15.
  ls_fieldcatalog-coltext   = 'Mensagem'.
  APPEND ls_fieldcatalog  TO t_fieldcatalog.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form toolbar_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM toolbar_alv .

  FREE: t_function.
  APPEND cl_gui_alv_grid=>mc_mb_export TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_sort TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_separator TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_call_more TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_find TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_find_more TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_average TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_auf TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_subtot TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_sum TO t_function.
  APPEND cl_gui_alv_grid=>mc_mb_view TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_print TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_info TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO t_function.

  APPEND cl_gui_alv_grid=>mc_fc_check          TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc       TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_refresh        TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut        TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy       TO t_function.
  APPEND cl_gui_alv_grid=>mc_mb_paste          TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo       TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row   TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO t_function.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO t_function.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0072  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0072 INPUT.

  CASE ok_code.

    WHEN '&CANCEL'.

      LEAVE TO SCREEN 0.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      "CALL METHOD g_custom_container->free.
      LEAVE TO SCREEN 0.

  ENDCASE.
  CLEAR ok_code.

ENDMODULE.
**<<<------"163355 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form zf_valida_terminal
*&---------------------------------------------------------------------*
*& Valida o Terminal de embarque com relação a NF de entrada e/ou saída
*&---------------------------------------------------------------------*
*&      --> PT_CONSULTA_TERMINAL Tabela de terminal de embarque
*&      --> UV_DOCNUM            Número do Documento da NF de entrada
*&      --> UV_CODIGO_RA         Código Recinto Alfandegado Embarque
*&      --> UV_ERRO              Erro
*&---------------------------------------------------------------------*
FORM zf_valida_terminal TABLES pt_consulta_terminal STRUCTURE zsde_consulta_terminal
                        USING  uv_docnum            TYPE      j_1bdocnum
                               uv_codigo_ra         TYPE      zde_codigo_ra_embarque
                               uv_erro              TYPE      c.

  DATA: tl_docnum      TYPE j_1bnfe_t_docnum.

  DATA: el_zlest0146_s TYPE zlest0146.

  DATA: vl_doc_rateio TYPE c,
        lv_saida_cct  TYPE c.

  CALL FUNCTION 'ZSD_VALIDA_NOTA_CCT'
    EXPORTING
      i_docnum    = uv_docnum
    IMPORTING
      e_saida_cct = lv_saida_cct.

* Verificar se a NF Entrada já está vinculada.
  SELECT docnum_flote FROM zsdtvinc_p_flote
    INTO TABLE tl_docnum
  WHERE docnum_eprod EQ uv_docnum
    AND cancel       EQ abap_false.

  IF sy-subrc IS INITIAL.
    CLEAR: pt_consulta_terminal.
* Consulta Terminal de Embarque NFe - Parceito Negócio Tp. Z1
    zcl_controle_retorno_rfl=>zif_controle_retorno_rfl~get_instance( )->consulta_terminar_nfe(
    EXPORTING
      it_docnum = tl_docnum
    IMPORTING
      et_consulta_terminal = pt_consulta_terminal[] ).
* Busca o terminal de embarque.
    SELECT SINGLE lifnr FROM zsdt0168 INTO @DATA(vl_terminal) WHERE codigo_ra EQ @uv_codigo_ra.

    IF sy-subrc IS INITIAL.
      LOOP AT pt_consulta_terminal.
        DATA(vl_tabix) = sy-tabix.
        IF pt_consulta_terminal-lifnr NE vl_terminal.
          DELETE pt_consulta_terminal INDEX vl_tabix.

        ELSE.
          CHECK NOT lv_saida_cct IS INITIAL.
          CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
            EXPORTING
              i_docnum            = pt_consulta_terminal-docnum
              i_set_peso_disp_uso = abap_false
            IMPORTING
              e_zlest0146         = el_zlest0146_s
              e_doc_rateio        = vl_doc_rateio.

          IF     vl_doc_rateio  IS INITIAL AND
             NOT el_zlest0146_s IS INITIAL.

            IF el_zlest0146_s-local_codigo_ra NE uv_codigo_ra.
              DELETE pt_consulta_terminal INDEX vl_tabix.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ELSE.
    uv_erro = abap_true.

  ENDIF.

*** Stefanini - IR239550 - 22/05/2025 - LAZAROSR - Início de Alteração
  IF pt_consulta_terminal[] IS INITIAL.
    uv_erro = abap_true.
  ENDIF.
*** Stefanini - IR239550 - 22/05/2025 - LAZAROSR - Fim de Alteração

ENDFORM.
**<<<------"163355 - NMS - FIM------>>>

*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Início de Alteração
FORM ajustar_saldo_nf.

  DATA:
    lv_saldo_rfl TYPE ty_alv_vinc_lotes-saldo_rfl,
    lv_encontrou TYPE boolean.

  IF  wa_filtro_remetente-c_cct    IS NOT INITIAL " Com CCT
  AND wa_filtro_remetente-tp_vinc1 IS NOT INITIAL." Com Vínculo

    it_vinc_lotes_ord = it_alv_vinc_lotes.
    SORT it_vinc_lotes_ord BY docnum itmnum.

    LOOP AT it_znom_notasfiscais_alv ASSIGNING FIELD-SYMBOL(<fs_nf_alv>).

      IF <fs_nf_alv>-nf_chegada_porto EQ 'RFL'.

        CLEAR <fs_nf_alv>-saldo_cct.

        IF <fs_nf_alv>-nr_saldo >= <fs_nf_alv>-qtde_nf.

          <fs_nf_alv>-nr_saldo = <fs_nf_alv>-qtde_nf.

          PERFORM zf_diminuir_qtd_utilizada USING <fs_nf_alv>.

        ENDIF.

      ELSE.

        IF <fs_nf_alv>-qtde_nf > <fs_nf_alv>-saldo_cct.

          <fs_nf_alv>-nr_saldo = <fs_nf_alv>-saldo_cct.

        ELSE.

          <fs_nf_alv>-nr_saldo = <fs_nf_alv>-qtde_nf.

        ENDIF.

        PERFORM zf_diminuir_qtd_utilizada USING <fs_nf_alv>.

      ENDIF.

      PERFORM zf_get_saldo_rfl USING <fs_nf_alv>
                               CHANGING lv_saldo_rfl
                                        lv_encontrou.

      IF  lv_encontrou IS NOT INITIAL
      AND <fs_nf_alv>-nr_saldo > lv_saldo_rfl.

        <fs_nf_alv>-nr_saldo = lv_saldo_rfl.

      ENDIF.

    ENDLOOP.

    DELETE it_znom_notasfiscais_alv WHERE del_reg = abap_true.

    IF plan_alv_notasfiscais IS NOT INITIAL.

      CALL METHOD plan_alv_notasfiscais->refresh_table_display.

    ENDIF.

  ENDIF.

ENDFORM.

FORM zf_diminuir_qtd_utilizada CHANGING c_s_nf_alv TYPE zplac_notasfiscais.

  TRY.

      c_s_nf_alv-nr_saldo = c_s_nf_alv-nr_saldo - c_s_nf_alv-nr_utilizada.

      IF c_s_nf_alv-nr_saldo <= 0.
        c_s_nf_alv-del_reg = abap_true.
      ENDIF.

    CATCH cx_root.

      c_s_nf_alv-del_reg = abap_true.

  ENDTRY.

ENDFORM.

FORM consultar_alv_notas.

  PERFORM: consulta_nota_fiscal_disp,
           alv_vinc_lotes,
           ajustar_saldo_nf.

ENDFORM.

FORM zf_get_saldo_rfl USING i_s_nf_alv TYPE zplac_notasfiscais
                      CHANGING c_v_saldo_rfl TYPE ty_alv_vinc_lotes-saldo_rfl
                               c_v_encontrou TYPE boolean.

  CLEAR: c_v_saldo_rfl, c_v_encontrou.

  READ TABLE it_vinc_lotes_ord TRANSPORTING NO FIELDS
                           WITH KEY docnum = i_s_nf_alv-docnum
                                    itmnum = i_s_nf_alv-itmnum
                                                 BINARY SEARCH.
  IF sy-subrc IS INITIAL.

    c_v_encontrou = abap_true.

    LOOP AT it_vinc_lotes_ord INTO DATA(ls_vinc_lotes) FROM sy-tabix.

      IF ls_vinc_lotes-docnum NE i_s_nf_alv-docnum
      OR ls_vinc_lotes-itmnum NE i_s_nf_alv-itmnum.
        EXIT.
      ENDIF.

      c_v_saldo_rfl = c_v_saldo_rfl + ls_vinc_lotes-saldo_rfl.

    ENDLOOP.

  ENDIF.

ENDFORM.
*** Stefanini - IR241122 - 06/06/2025 - LAZAROSR - Fim de Alteração

*** US #131067 - MMSILVA - 09.07.2025 - Ini ***
FORM f_consultar_devolucao_sigam USING wa_saida_devolucao TYPE zplac_notasfiscais
                                 CHANGING wa_saida_result TYPE zplac_notasfiscais.

  DATA: wa_consulta_nfe_sigam       TYPE zfie_consultar_nfe_sigam,
        wa_resultado_cons_nfe_sigam TYPE zfie_result_cons_nfe_sigam,
        it_resultado_cons_nfe_sigam TYPE TABLE OF zfie_result_cons_nfe_sigam,
        lt_docnum                   TYPE TABLE OF string. " Rubenilson Pereira - 08.10.25 #192273

  "US #131067 - SMCABANA - 22-08-2025
  "Adicione no início do PERFORM
  DATA: lv_simular_erro TYPE abap_bool.
  "US #131067 - SMCABANA - 22-08-2025

  APPEND INITIAL LINE TO lt_docnum ASSIGNING FIELD-SYMBOL(<fs_docnum>)." Rubenilson Pereira - 08.10.25 #192273
  <fs_docnum> = wa_saida_devolucao-docnum." Rubenilson Pereira - 08.10.25 #192273
*  wa_consulta_nfe_sigam = VALUE #( numerodocumentosap = wa_saida_devolucao-docnum" Rubenilson Pereira - 08.10.25 #192273
  wa_consulta_nfe_sigam = VALUE #( numerodocumentosaplista = lt_docnum" Rubenilson Pereira - 08.10.25 #192273
                                   item               = 'true'
                                   imposto            = 'true'
                                   devolucao          = 'true' ).

  "US #131067 - SMCABANA - 22-08-2025 - "SIMULAÇÃO DE ERRO - APENAS PARA TESTE
  "Defina quando simular
*lv_simular_erro = COND #( WHEN sy-uname = 'SMCABANA' AND sy-sysid = 'QAS' THEN abap_true ELSE abap_false ).
  "US #131067 - SMCABANA - 22-08-2025 - "SIMULAÇÃO DE ERRO - APENAS PARA TESTE

  TRY .
      "Chama API para consultar o documento
      zcl_int_ob_dados_nf_sigam=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = wa_consulta_nfe_sigam IMPORTING e_integracao = DATA(r_response) ).

      "US #131067 - SMCABANA - 22-08-2025
      "SIMULAÇÃO DE ERRO - APENAS PARA TESTE
*    IF lv_simular_erro = abap_true.
*      BREAK-POINT.
*      RAISE EXCEPTION TYPE zcx_integracao.
*    ENDIF.
      "US #131067 - SMCABANA - 22-08-2025


      IF r_response-nm_code EQ '0200'.
        CALL METHOD /ui2/cl_json=>deserialize
          EXPORTING
            json = r_response-ds_data_retorno
          CHANGING
            data = wa_resultado_cons_nfe_sigam.

        IF wa_resultado_cons_nfe_sigam IS NOT INITIAL.
          LOOP AT wa_resultado_cons_nfe_sigam-data INTO DATA(ls_data).
            IF ls_data-devolucao IS NOT INITIAL.
              LOOP AT ls_data-devolucao INTO DATA(ls_devolucao).
                wa_saida_result-nr_saldo = wa_saida_devolucao-nr_saldo.
*                 "US #131067 - SMCABANA     - 16-09-2025  - COMENTADO ESSE TRECHO PARA ANALISE E AJUSTE DA AREA ANTES DE SUBIR PARA PRD - FAVOR NAO DELETAR ESSE TRECHO DO CODIGO
*                wa_saida_result-nr_saldo = wa_saida_devolucao-nr_saldo - ls_devolucao-quantidade.
*                  IF wa_saida_result-nr_saldo < 0.
*                      wa_saida_result-nr_saldo = 0.
*                  ENDIF.
*              "US #131067 - SMCABANA     - 16-09-2025 - COMENTADO ESSE TRECHO PARA ANALISE E AJUSTE DA AREA ANTES DE SUBIR PARA PRD - FAVOR NAO DELETAR ESSE TRECHO DO CODIGO
              ENDLOOP.
            ENDIF.
          ENDLOOP.
        ENDIF.

      ENDIF.
    CATCH zcx_integracao INTO DATA(ex_integra).    "
      ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      RAISE RESUMABLE EXCEPTION TYPE zcx_integracao."US #131067 - SMCABANA - 22-08-2025 - QUANDO FALHAR O SERVIÇO API DAR ESSA MSG
    CATCH zcx_error INTO DATA(ex_error).    "  "
      ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      RAISE RESUMABLE EXCEPTION TYPE zcx_error. "US #131067 - SMCABANA - 22-08-2025 - QUANDO FALHAR O SERVIÇO API DAR ESSA MSG
  ENDTRY.

  CLEAR: wa_consulta_nfe_sigam, wa_resultado_cons_nfe_sigam, r_response.

ENDFORM.
*** US #131067 - MMSILVA - 09.07.2025 - Fim ***
