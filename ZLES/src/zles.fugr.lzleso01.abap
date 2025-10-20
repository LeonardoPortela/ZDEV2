*----------------------------------------------------------------------*
***INCLUDE LZLESO01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: it_ucomm TYPE TABLE OF sy-ucomm.

  CLEAR: it_ucomm.

  IF vg_solicita_pedagio EQ abap_false OR zlest0026-ck_credita_ped EQ abap_false OR zlest0026-tp_admim_ped NE '03'.
    APPEND 'PEDAGIO' TO it_ucomm.
  ENDIF.

  SET PF-STATUS 'LES0001' EXCLUDING it_ucomm.
  SET TITLEBAR  'LES0001'.

  zlest0101-country = 'BR'.

  IF zlest0101-cd_cid_origem IS NOT INITIAL.
    SELECT SINGLE text INTO lc_tx_cid_origem
      FROM j_1btxjurt
     WHERE spras      EQ sy-langu
       AND country    EQ 'BR'
       AND taxjurcode EQ zlest0101-cd_cid_origem.
  ENDIF.

  IF zlest0101-cd_cid_destino IS NOT INITIAL.
    SELECT SINGLE text INTO lc_tx_cid_destino
      FROM j_1btxjurt
     WHERE spras      EQ sy-langu
       AND country    EQ 'BR'
       AND taxjurcode EQ zlest0101-cd_cid_destino.
  ENDIF.

  IF zlest0026-id_rota IS NOT INITIAL OR vg_ck_alterou_cidade EQ abap_true.
    SELECT * INTO TABLE it_pracas
      FROM zlest0102
     WHERE id_rota_adm EQ zlest0026-id_rota
       AND branch      EQ zlest0101-branch.

    DELETE it_pracas WHERE st_praca = abap_false.

  ELSEIF zlest0026-id_rota IS INITIAL.
    CLEAR: it_pracas[].
  ENDIF.

  vg_ck_alterou_cidade = abap_false.
  vg_ck_admim_ped      = abap_false.

  ""*-CS2024001181-16.12.2024-#160717-JT-inicio
  IF ctl_con_0100 IS NOT INITIAL.
    CALL METHOD ctl_con_0100->free.
    FREE: ctl_con_0100.
  ENDIF.
  ""*-CS2024001181-16.12.2024-#160717-JT-fim

  IF ctl_con_0100 IS INITIAL.

    CREATE OBJECT ctl_con_0100
      EXPORTING
        container_name = 'ALV_PRACAS'.

    CREATE OBJECT ctl_alv_0100
      EXPORTING
        i_parent = ctl_con_0100.

    PERFORM fill_it_fieldcatalog_0100.
*   Fill info for layout variant

    PERFORM fill_gs_variant_0100.
*   Set layout parameters for ALV grid

    gs_lay_0100-grid_title = TEXT-001.
    gs_lay_0100-sel_mode   = space.
    gs_lay_0100-zebra      = abap_true.
    gs_lay_0100-no_toolbar = abap_true.

    CALL METHOD ctl_alv_0100->set_table_for_first_display
      EXPORTING
        is_layout            = gs_lay_0100
        is_variant           = gs_var_0100
        i_default            = space
        it_toolbar_excluding = it_exclude_0100        "I_SAVE = 'A'
      CHANGING
        it_fieldcatalog      = it_catalog_0100
        it_outtab            = it_pracas[].

    CALL METHOD ctl_alv_0100->refresh_table_display.

  ELSE.
    PERFORM fill_it_fieldcatalog_0100.  ""*-CS2024001181-16.12.2024-#160717-JT-inicio
    CALL METHOD ctl_alv_0100->refresh_table_display.
  ENDIF.

  CALL METHOD ctl_alv_0100->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col_0100
      es_row_no   = gs_scroll_row_0100.

  LOOP AT SCREEN.
    IF screen-name EQ 'ZLEST0026-ADTO'.
      CASE vg_ck_adiantamento.
        WHEN abap_true.
          screen-input = '1'.
        WHEN abap_false.
          screen-input = '0'.
      ENDCASE.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " STATUS_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  DISPLAY_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_0100 OUTPUT.

  PERFORM ajusta_tela USING abap_true.

ENDMODULE.                 " DISPLAY_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0100 .

  DATA: lc_col_pos TYPE lvc_colpos.

  DATA: q TYPE i.

  FIELD-SYMBOLS: <fs_cat_0100> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZLEST0102'
    CHANGING
      ct_fieldcat      = it_catalog_0100.

  LOOP AT it_catalog_0100 ASSIGNING <fs_cat_0100>.
    CASE <fs_cat_0100>-fieldname.
      WHEN 'NM_PRACA'.
        <fs_cat_0100>-outputlen = 30.
      WHEN 'ID_ROTA' OR 'ID_ROTA_ADM' OR 'ID_PRACA' OR 'ST_PRACA' OR 'ID_PRACA_SEM_PAR'.
        <fs_cat_0100>-no_out = abap_true.
    ENDCASE.
    CASE <fs_cat_0100>-fieldname(7).
      WHEN 'VL_EIXO'.
        IF zlest0026-tp_card_ped = 'G' OR "*-CS2024001181-16.12.2024-#160717-JT-inicio
           zlest0026-tp_card_ped = 'M' OR
           zlest0026-tp_card_ped = 'S'.
          <fs_cat_0100>-no_out = abap_true.
          CONTINUE.
        ENDIF.

        q = strlen( <fs_cat_0100>-fieldname ).
        q = q - 7.
        <fs_cat_0100>-outputlen = 15.
        <fs_cat_0100>-do_sum    = abap_true.
        CONCATENATE 'Qtd.Eixo' <fs_cat_0100>-fieldname+7(q) INTO <fs_cat_0100>-scrtext_l.
        CONCATENATE 'Qtd.Eixo' <fs_cat_0100>-fieldname+7(q) INTO <fs_cat_0100>-scrtext_m.
        CONCATENATE 'Qtd.Eixo' <fs_cat_0100>-fieldname+7(q) INTO <fs_cat_0100>-scrtext_s.
        CONCATENATE 'Qtd.Eixo' <fs_cat_0100>-fieldname+7(q) INTO <fs_cat_0100>-coltext.
        MOVE <fs_cat_0100>-fieldname+7(q) TO q.
        IF zlest0026-qtd_eixo EQ q.
          <fs_cat_0100>-no_out = abap_false.
        ELSE.
          <fs_cat_0100>-no_out = abap_true.
        ENDIF.

*-CS2024001181-16.12.2024-#160717-JT-inicio
      WHEN 'VL_TAGE'.
        IF zlest0026-tp_card_ped <> 'G' AND  "*-CS2024001181-16.12.2024-#160717-JT-inicio
           zlest0026-tp_card_ped <> 'M' AND
           zlest0026-tp_card_ped <> 'S'.
          <fs_cat_0100>-no_out = abap_true.
          CONTINUE.
        ENDIF.

        q = strlen( <fs_cat_0100>-fieldname ).
        q = q - 10.
        <fs_cat_0100>-outputlen = 15.
        <fs_cat_0100>-do_sum    = abap_true.
        CONCATENATE 'Qtd.Eixo' <fs_cat_0100>-fieldname+10(q) INTO <fs_cat_0100>-scrtext_l.
        CONCATENATE 'Qtd.Eixo' <fs_cat_0100>-fieldname+10(q) INTO <fs_cat_0100>-scrtext_m.
        CONCATENATE 'Qtd.Eixo' <fs_cat_0100>-fieldname+10(q) INTO <fs_cat_0100>-scrtext_s.
        CONCATENATE 'Qtd.Eixo' <fs_cat_0100>-fieldname+10(q) INTO <fs_cat_0100>-coltext.
        MOVE <fs_cat_0100>-fieldname+10(q) TO q.
        IF zlest0026-qtd_eixo EQ q.
          <fs_cat_0100>-no_out = abap_false.
        ELSE.
          <fs_cat_0100>-no_out = abap_true.
        ENDIF.
*-CS2024001181-16.12.2024-#160717-JT-fim

    ENDCASE.
  ENDLOOP.

ENDFORM.                    " FILL_IT_FIELDCATALOG_0100

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0100 .

  gs_var_0100-report      = sy-repid.
  gs_var_0100-handle      = '0100'.
  gs_var_0100-log_group   = abap_false.
  gs_var_0100-username    = abap_false.
  gs_var_0100-variant     = abap_false.
  gs_var_0100-text        = abap_false.
  gs_var_0100-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT_0100

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ajusta_tela USING pck_open_pedagio TYPE char01.

  DATA : wk_vtts           TYPE vtts,
         vl_laufk          TYPE tvtk-laufk,
         vl_shtyp          TYPE vttk-shtyp,
         vl_ctrl           TYPE c,
         vl_ped_e          TYPE c,
         vl_bezkz          TYPE tvknt-bezkz,
         wk_xvttk          TYPE vttkvb,
         wk_xvtts          TYPE vttsvb,
         vg_branch         TYPE j_1bbranc_,
         vg_tdlnr          TYPE tdlnr,
         wk_vttp           TYPE vttp,
         wk_lips           TYPE lips,
         wk_zlest0090      TYPE zlest0090,
         it_zlest0090      TYPE TABLE OF zlest0090,
         qt_zlest0090      TYPE i,
         lt_zlest0002      TYPE TABLE OF zlest0002,
         wa_zlest0002_card TYPE zlest0002_card,
         wk_zlest0002      TYPE zlest0002,
         var_qtd_eixo      TYPE i,
         wk_trolz          TYPE trolz,
         wk_lfa1           TYPE lfa1,
         wk_kna1           TYPE kna1,
         wk_vtpa           TYPE vtpa,
         lt_zlest0084      TYPE TABLE OF zlest0084,
         wk_zlest0084      TYPE zlest0084,
         wk_zlest0091      TYPE zlest0091,
         lc_ck_ped         TYPE zde_ck_pegagio,
         vg_tp_card_ped	   TYPE zde_tp_card_ped,
         vg_tp_admim_ped   TYPE zde_adm_pedagio,
         vg_tp_admim_frete TYPE zde_adm_pedagio,
         vg_credito_ped    TYPE zde_cred_pedagio,
         wa_zlest0026_aux  TYPE zlest0026.

  DATA: repom_roteiro      TYPE REF TO zcl_repom_roteiro_vlr_vpr,
        lc_ok              TYPE char01,
        lc_percurso        TYPE zlest0122,
        lc_erro            TYPE zde_repom_erros,
        p_retornou         TYPE c LENGTH 1,
        lc_branch          TYPE j_1bbranc_,
        lc_credita_pedagio TYPE zde_cred_pedagio, "*-CS2024000522-18.07.2024-JT-#143588-inicio
        var_versao_xml_tip TYPE char10.

  CLEAR: vg_credito_ped, vg_ck_credito_pedt.

  IF vg_ck_credito_ped EQ abap_true.
    vg_credito_ped = zlest0026-ck_credita_ped.
  ENDIF.

  IF vg_ck_admim_frete EQ abap_true.
    CLEAR: zlest0026-nr_vr_xml_tipf.
  ENDIF.

  IF vg_ck_admim_ped EQ abap_true.
    CLEAR: zlest0026-tp_card_ped,
           zlest0026-nr_card_ped,
           zlest0026-id_rota,
           zlest0026-qtd_eixo,
           zlest0026-ck_credita_ped,
           zlest0026-tx_obs_cred_mesm,
           vg_ck_admim_ped,
           vg_ck_admim_frete,
           vg_ck_adiantamento.
  ENDIF.

  vg_tp_card_ped     = zlest0026-tp_card_ped.
  vg_tp_admim_ped    = zlest0026-tp_admim_ped.
  vg_tp_admim_frete  = zlest0026-tp_admim_frete.

  CLEAR : vl_route, vl_tplst ,vl_bukrs, zlest0026, vl_ctrl, vl_ped_e, vl_laufk, vl_shtyp, wk_vtts, wk_xvttk, wk_xvtts,
          vg_pedi_pedagio, vg_cartao_pedagio.

  zlest0026-tp_card_ped     = vg_tp_card_ped.
  zlest0026-tp_admim_ped    = vg_tp_admim_ped.
  zlest0026-tp_admim_frete  = vg_tp_admim_frete.

* Cabeçalho do documento de transporte
  READ TABLE ti_xvttk INTO wk_xvttk INDEX 1.

  READ TABLE ti_xvtts INTO wk_xvtts INDEX 1.



  IF NOT wk_xvttk IS INITIAL.

    CALL METHOD zcl_repom_viagem_vpr=>get_id_proc_cliente_vt
      EXPORTING
        i_tknum           = wk_xvttk-tknum
      RECEIVING
        e_id_proc_cliente = zlest0026-id_proc_cliente
      EXCEPTIONS
        nao_encontrado    = 1
        OTHERS            = 2.

    IF sy-subrc IS INITIAL.
      vg_solicita_pedagio      = abap_true.
      vg_cartao_pedagio        = abap_true.
      zlest0026-tp_admim_ped   = '03'.
      zlest0026-ck_credita_ped = abap_true.

      CREATE OBJECT obj_pedagio EXPORTING i_id_proc_cliente = zlest0026-id_proc_cliente.
      obj_pedagio->get_registro( IMPORTING e_registro = wa_zlest0123 ).
      zlest0026-pedagio = wa_zlest0123-vlr_total_pedagio.
      CLEAR: obj_pedagio.
    ENDIF.

    " Origem e Destino """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF zlest0101-cd_cid_origem IS INITIAL.
      CLEAR: wk_vtpa.

*-#133089-12.02.2024-JT-inicio
      IF vg_faturauto_in IS NOT INITIAL.
        SELECT SINGLE * FROM lfa1 INTO wk_lfa1 WHERE lifnr EQ vg_faturauto_in-parid.
        zlest0101-cd_cid_origem = wk_lfa1-txjcd.
      ELSE.
*-#133089-12.02.2024-JT-fim
        SELECT SINGLE * FROM vtpa INTO wk_vtpa WHERE vbeln EQ wk_xvttk-tknum AND parvw EQ 'PC'.
        IF sy-subrc IS INITIAL.
          SELECT SINGLE * FROM lfa1 INTO wk_lfa1 WHERE lifnr EQ wk_vtpa-lifnr.
          zlest0101-cd_cid_origem = wk_lfa1-txjcd.
        ENDIF.
      ENDIF.
    ENDIF.

    IF zlest0101-cd_cid_destino IS INITIAL.
      CLEAR: wk_vtpa.

*-#133089-12.02.2024-JT-inicio
      IF vg_faturauto_in IS NOT INITIAL.
        SELECT SINGLE * FROM kna1 INTO wk_kna1 WHERE kunnr EQ vg_faturauto_in-kunnr.
        zlest0101-cd_cid_destino = wk_kna1-txjcd.
      ELSE.
*-#133089-12.02.2024-JT-fim
        SELECT SINGLE * FROM vtpa INTO wk_vtpa WHERE vbeln EQ wk_xvttk-tknum AND parvw EQ 'LR'.
        IF sy-subrc IS INITIAL.
          SELECT SINGLE * FROM kna1 INTO wk_kna1 WHERE kunnr EQ wk_vtpa-kunnr.
          zlest0101-cd_cid_destino = wk_kna1-txjcd.
        ENDIF.
      ENDIF.
    ENDIF.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

* Validação do código de percurso
    SELECT SINGLE laufk INTO vl_laufk FROM tvtk WHERE shtyp = wk_xvttk-shtyp.

    vg_ck_ped_param = abap_true.

    CASE vl_laufk.
* Percurso preliminar / Percurso Direto
      WHEN 1 OR 4.
        SELECT SINGLE ck_pedagio INTO lc_ck_ped FROM zlest0027 WHERE route = wk_xvttk-route.
        IF sy-subrc IS INITIAL.
          CASE lc_ck_ped.
            WHEN abap_true.
              vl_ctrl = abap_false.
            WHEN abap_false.
              vl_ctrl = abap_true.
          ENDCASE.
        ELSE.
          vg_ck_ped_param = abap_false.
        ENDIF.
* Principal, subsequente
      WHEN OTHERS.

        IF wk_xvtts IS INITIAL.
          vl_ctrl = 'X'.
        ELSE.
          IF wk_xvtts-vsart = 1.
            SELECT SINGLE bezkz INTO vl_bezkz FROM tvknt WHERE knote = wk_xvtts-knotz.
            IF NOT sy-subrc IS INITIAL OR vl_bezkz <> 'X'.
              vl_ctrl = 'X'.
            ENDIF.
          ELSE.
            SELECT SINGLE bezkz INTO vl_bezkz FROM tvknt WHERE knote = wk_xvtts-knota.
            IF NOT sy-subrc IS INITIAL OR vl_bezkz <> 'X'.
              vl_ctrl = 'X'.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.

    vg_branch = wk_xvttk-tplst.
    vg_tdlnr  = wk_xvttk-tdlnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = vg_tdlnr
      IMPORTING
        output = vg_tdlnr.

    lc_branch = vg_tdlnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lc_branch
      IMPORTING
        output = lc_branch.

    zlest0101-branch = lc_branch.

    IF NOT wk_netwr_all IS INITIAL.

* Local de organizaçao de transporte
      SELECT SINGLE bukrs INTO vl_bukrs FROM ttds WHERE tplst = wk_xvttk-tplst.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vg_branch
        IMPORTING
          output = vg_branch.

      CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
        EXPORTING
          centro               = vg_branch
        IMPORTING
          centro_out           = vg_branch
        EXCEPTIONS
          informar_centro      = 1
          nao_centro_r_virtual = 2
          informar_centro_out  = 3
          informar_centro_v    = 4
          OTHERS               = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro.
      ENDIF.

      CLEAR: wk_zlest0002.

      IF wk_xvttk-text1 IS NOT INITIAL.
        SELECT SINGLE * INTO wk_zlest0002
          FROM zlest0002
         WHERE pc_veiculo EQ wk_xvttk-text1(7).
        IF sy-subrc IS INITIAL.
          DATA(vl_proprietario) = wk_zlest0002-proprietario.
        ENDIF.
      ENDIF.

      TRY .
          DATA(r_margadto) =
            zcl_calc_frete=>get_valor_adiantamento(
            EXPORTING
              i_bukrs  = vl_bukrs
              i_branch = vg_branch
              i_lifnr  = vl_proprietario ).

          IF r_margadto IS NOT INITIAL.
            vg_ck_adiantamento = abap_true.
            zlest0026-adto = ( wk_netwr_all * r_margadto ) / 100.
          ELSE.
            zlest0026-adto = 0.
          ENDIF.

        CATCH zcx_calc_frete INTO DATA(ex_calc_frete).    "
          zlest0026-adto = 0.
          ex_calc_frete->published_erro( EXPORTING i_msgty = 'I' i_msgty_display = 'E' ).
      ENDTRY.
    ENDIF.

*-#133089-12.02.2024-JT-inicio
*  "Validação se o valor do pedagio precisa ser informado manualmente ou eletronico.
    IF vg_faturauto_in IS INITIAL.
      SELECT SINGLE * FROM vttp INTO wk_vttp WHERE tknum EQ wk_xvttk-tknum.
      SELECT SINGLE * FROM lips INTO wk_lips WHERE vbeln EQ wk_vttp-vbeln.

      CHECK sy-subrc IS INITIAL.
    ELSE.
      wk_lips-werks = vg_faturauto_in-branch.
    ENDIF.
*-#133089-12.02.2024-JT-inicio

*   CHECK sy-subrc IS INITIAL.

    CALL FUNCTION 'Z_CENTRO_REAL_VIRTUAL'
      EXPORTING
        centro               = wk_lips-werks
      IMPORTING
        centro_real          = wk_zlest0090-werks
      EXCEPTIONS
        informar_centro      = 1
        nao_centro_r_virtual = 2
        informar_centro_out  = 3
        informar_centro_v    = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "Administradora de Frete """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    IF zlest0026-tp_admim_frete IS INITIAL.
      SELECT SINGLE * FROM zlest0090 INTO wk_zlest0090 WHERE werks EQ wk_zlest0090-werks AND tp_servico EQ '0' AND ck_default EQ abap_true.
      IF sy-subrc IS INITIAL.
        zlest0026-tp_admim_frete = wk_zlest0090-tp_adm.
      ENDIF.
    ELSE.
      SELECT SINGLE * FROM zlest0090 INTO wk_zlest0090 WHERE werks EQ wk_zlest0090-werks AND tp_servico EQ '0' AND tp_adm EQ zlest0026-tp_admim_frete.
    ENDIF.

**=============================================Inicio LES - Ajuste montagem XML CTE #IR175016 AOENNING
    "Stvarv versão XML/TIP.
    IF wk_zlest0090-nr_vr_xml_tipf IS INITIAL.
      CLEAR: var_versao_xml_tip.
      SELECT SINGLE low FROM tvarvc
      INTO var_versao_xml_tip
      WHERE name = 'Z_VERSAO_XML_TIP'.
      IF sy-subrc EQ 0.
        zlest0026-tp_admim_frete = '09'.
        zlest0026-nr_vr_xml_tipf = var_versao_xml_tip.
      ELSE.
        zlest0026-tp_admim_frete = '09'.
        zlest0026-nr_vr_xml_tipf = '1.17'.
      ENDIF.
    ENDIF.


*    IF sy-subrc IS INITIAL.
*      zlest0026-nr_vr_xml_tipf = wk_zlest0090-nr_vr_xml_tipf.
*    ELSEIF zlest0026-tp_admim_frete IS INITIAL.
*      zlest0026-tp_admim_frete = '09'.
*      zlest0026-nr_vr_xml_tipf = '1.17'.
*    ENDIF.

**=============================================Fim LES - Ajuste montagem XML CTE #IR175016 AOENNING




    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    "Administradora de Pedágio
    IF vl_ctrl IS INITIAL AND zlest0026-id_proc_cliente IS INITIAL.

      vg_pedi_pedagio = abap_true.

      "Administradora de Pedágio """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      SELECT * INTO TABLE it_zlest0090 FROM zlest0090 WHERE werks EQ wk_zlest0090-werks AND tp_servico EQ '1'.
      IF sy-subrc IS INITIAL.
        IF zlest0026-tp_admim_ped IS INITIAL.
          READ TABLE it_zlest0090 INTO wk_zlest0090 WITH KEY ck_default = abap_true.
          IF sy-subrc IS INITIAL.
            zlest0026-tp_admim_ped = wk_zlest0090-tp_adm.
          ELSE.
            zlest0026-tp_admim_ped = '09'.
          ENDIF.
        ELSE.
          SELECT SINGLE * FROM zlest0090 INTO wk_zlest0090 WHERE werks EQ wk_zlest0090-werks AND tp_servico EQ '1' AND tp_adm EQ zlest0026-tp_admim_ped.
        ENDIF.
      ELSE.
        zlest0026-tp_admim_ped = '09'.
      ENDIF.
      DESCRIBE TABLE it_zlest0090 LINES qt_zlest0090.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      IF ( sy-subrc EQ 0 ).

        CASE wk_zlest0090-tp_op.
          WHEN 'E'. "Eletronico

            vg_cartao_pedagio = abap_true.

            CASE wk_zlest0090-tp_adm.
              WHEN '03'. "REPOM S.A.

                "Solicitar Pedágio REPOM
                vg_solicita_pedagio = abap_true.

                CLEAR: zlest0026-tp_card_ped,
                       zlest0026-nr_card_ped,
                       zlest0026-id_rota,
                       zlest0026-qtd_eixo.

                IF vg_ck_credito_ped NE abap_true.
                  zlest0026-ck_credita_ped = abap_true.
                ELSE.
                  zlest0026-ck_credita_ped = vg_credito_ped.
                ENDIF.

                lc_credita_pedagio = zlest0026-ck_credita_ped.  "*-CS2024000522-18.07.2024-JT-#143588-inicio

                SELECT * FROM zlest0002
                  INTO TABLE lt_zlest0002
                WHERE pc_veiculo IN (wk_xvttk-text1(7),wk_xvttk-text2(7),wk_xvttk-text3(7),wk_xvttk-text4(7) ).

                IF ( sy-subrc EQ 0 ).

                  zlest0026-placa_cav = wk_xvttk-text1(7).

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                    EXPORTING
                      input  = vg_tdlnr
                    IMPORTING
                      output = vg_tdlnr.

                  lc_branch = vg_tdlnr.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = lc_branch
                    IMPORTING
                      output = lc_branch.

                  i_veiculo_eixos = 0.
                  LOOP AT lt_zlest0002 INTO wk_zlest0002.
                    i_veiculo_eixos = i_veiculo_eixos + wk_zlest0002-qt_eixo.
                  ENDLOOP.

                  IF pck_open_pedagio EQ abap_true.

                    SELECT SINGLE * INTO @DATA(wa_j1_bbranch)
                      FROM j_1bbranch
                     WHERE branch EQ @lc_branch.

                    CALL FUNCTION 'Z_REPOM_INFORMA_PERCURSO'
                      EXPORTING
                        i_branch         = lc_branch
                        i_bukrs          = wa_j1_bbranch-bukrs
                        i_cd_cid_origem  = zlest0101-cd_cid_origem
                        i_cd_cid_destino = zlest0101-cd_cid_destino
*                       I_QTD_EIXOS      = I_VEICULO_EIXOS
                      IMPORTING
                        e_informado      = lc_ok
                        e_percurso       = lc_percurso
                      EXCEPTIONS
                        sem_percurso     = 1
                        OTHERS           = 2.

                    IF lc_ok EQ abap_true.
                      CREATE OBJECT repom_roteiro.
                      repom_roteiro->set_bukrs( EXPORTING i_bukrs = wa_j1_bbranch-bukrs ).
                      repom_roteiro->set_branch( EXPORTING i_branch = lc_branch ).
                      repom_roteiro->set_id_rota_repom( EXPORTING i_id_rota_repom = lc_percurso-id_rota_repom ).
                      repom_roteiro->set_id_percurso_repom( EXPORTING i_id_percurso_repom = lc_percurso-id_percurso_repom ).
                      repom_roteiro->set_id_rota( EXPORTING i_id_rota = lc_percurso-id_rota ).
                      repom_roteiro->set_veiculo_eixos( EXPORTING i_veiculo_eixos = i_veiculo_eixos ).
                      repom_roteiro->set_qtd_eixos_suspensos_ida( EXPORTING i_qtd_eixos_suspensos_ida = 0 ).
                      repom_roteiro->set_qtd_eixos_suspensos_volta( EXPORTING i_qtd_eixos_suspensos_volta = 0 ).

                      CALL METHOD repom_roteiro->consultar_valor
                        IMPORTING
                          e_erros                    = DATA(lc_erros)
                        RECEIVING
                          i_retornou                 = p_retornou
                        EXCEPTIONS
                          servico_nao_encontrado     = 1
                          http_communication_failure = 2
                          http_invalid_state         = 3
                          http_processing_failed     = 4
                          http_invalid_timeout       = 5
                          erro                       = 6
                          OTHERS                     = 7.

                      IF sy-subrc IS NOT INITIAL.
                        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING pedagio.
                      ENDIF.

                      IF p_retornou EQ abap_false.

                        LOOP AT lc_erros INTO lc_erro.
                          MESSAGE w017(zrepom) WITH lc_erro-erro_codigo lc_erro-erro_descricao.
                        ENDLOOP.

                      ELSE.

                        zlest0026-pedagio = repom_roteiro->get_valor_total_vpr( ).

*                      SELECT SINGLE * INTO @DATA(WA_ZLEST0123_ANTERIOR)
*                        FROM ZLEST0123
*                       WHERE VEICULO_PLACA      EQ @-PLACA_CAV
*                         AND DT_EMISSAO_PEDAGIO EQ @SY-DATUM
*                         AND ID_ROTA_REPOM      EQ @LC_PERCURSO-ID_ROTA_REPOM
*                         AND ID_PERCURSO_REPOM  EQ @LC_PERCURSO-ID_PERCURSO_REPOM
*                         AND TKNUM              NE @WK_XVTTK-TKNUM
*                         AND TP_STATUS_AUT      EQ '3'
*                         AND TP_STATUS_CAN      NE '3'.
*
*                      IF SY-SUBRC IS INITIAL.
*                        VG_PEDI_MESMO_VEICUL = ABAP_TRUE.
*                      ELSE.
*                        VG_PEDI_MESMO_VEICUL = ABAP_FALSE.
*                      ENDIF.

                        CALL FUNCTION 'Z_CK_PEDAGIO_MESMO_VEICULO'
                          EXPORTING
                            i_zlest0090      = wk_zlest0090
                            i_zlest0026      = zlest0026
                            i_zlest0122      = lc_percurso
                            i_tknum          = wk_xvttk-tknum
                          IMPORTING
                            e_mesmo_veiculo  = vg_pedi_mesmo_veicul
                            e_mesma_carga    = vg_pedi_mesma_carga
                          CHANGING
                            m_ck_credita_ped = lc_credita_pedagio. "*-#146705-25.07.2024-JT-#146705

                        IF vg_ck_credito_ped = abap_false. "*-#146705-25.07.2024-JT-#146705
                          zlest0026-ck_credita_ped = lc_credita_pedagio.
                        ENDIF.

                        IF vg_pedi_mesma_carga EQ abap_true OR zlest0026-ck_credita_ped = abap_false. "*-#146705-25.07.2024-JT-#146705
                          zlest0026-ck_credita_ped = abap_false.
                          CLEAR: zlest0026-pedagio.
                        ENDIF.

                      ENDIF.
                      CLEAR: repom_roteiro.
                    ELSE.
                      MESSAGE w040(zrepom) WITH zlest0101-cd_cid_origem zlest0101-cd_cid_destino.
                    ENDIF.
                  ENDIF.
                ENDIF.


              WHEN '09'. "(TipFrete) UNIK S.A.

                SELECT * FROM zlest0002
                  INTO TABLE lt_zlest0002
                WHERE pc_veiculo IN (wk_xvttk-text1(7),wk_xvttk-text2(7),wk_xvttk-text3(7),wk_xvttk-text4(7) ).

                IF ( sy-subrc EQ 0 ).

                  CLEAR: var_qtd_eixo, wk_zlest0091.

                  LOOP AT lt_zlest0002 INTO wk_zlest0002.
                    var_qtd_eixo = var_qtd_eixo + wk_zlest0002-qt_eixo.
                    CLEAR: wk_zlest0002.
                  ENDLOOP.

                  SELECT SINGLE * FROM zlest0091
                    INTO wk_zlest0091
                   WHERE qtd_eixo EQ var_qtd_eixo.

                  zlest0026-placa_cav = wk_xvttk-text1(7).

                  IF zlest0026-tp_card_ped IS INITIAL.
                    "Busca Padrão
                    SELECT SINGLE * FROM zlest0002_card INTO wa_zlest0002_card
                     WHERE pc_veiculo     EQ zlest0026-placa_cav
                       AND ck_card_padrao EQ 'X'.
                  ELSE.
                    "Busca Selecionado
                    IF zlest0026-tp_card_ped NE 'S'.
                      SELECT SINGLE * FROM zlest0002_card INTO wa_zlest0002_card
                       WHERE pc_veiculo  EQ zlest0026-placa_cav
                         AND tp_card_ped EQ zlest0026-tp_card_ped.
                    ENDIF.
                  ENDIF.

                  IF sy-subrc IS INITIAL AND wa_zlest0002_card IS NOT INITIAL.
                    zlest0026-tp_card_ped = wa_zlest0002_card-tp_card_ped.
                    zlest0026-nr_card_ped = wa_zlest0002_card-nr_card_ped.
                  ELSE.
                    IF zlest0026-tp_card_ped IS INITIAL.
                      IF wk_zlest0090-tp_card_ped_default IS NOT INITIAL.
                        zlest0026-tp_card_ped = wk_zlest0090-tp_card_ped_default.
                      ELSE.
                        zlest0026-tp_card_ped = 'S'.
                      ENDIF.
                      CLEAR: zlest0026-nr_card_ped.
                    ENDIF.
                  ENDIF.

                  SELECT *
                    FROM zlest0084
                    INTO TABLE lt_zlest0084
                   WHERE branch         EQ zlest0101-branch
                     AND munic_origem   EQ zlest0101-cd_cid_origem+3(7)
                     AND munic_destino  EQ zlest0101-cd_cid_destino+3(7)
                     AND cat_veiculo    EQ wk_zlest0091-categoria
                     AND prioridade     EQ 'X'.

                  IF ( sy-subrc EQ 0 ).

*-CS2024001181-16.12.2024-#160717-JT-inicio
                    IF zlest0026-tp_card_ped = 'G' OR
                       zlest0026-tp_card_ped = 'M' OR
                       zlest0026-tp_card_ped = 'S'.
                      LOOP AT lt_zlest0084    INTO DATA(wl_zlest0084).
                        wl_zlest0084-vlr_pedagio = wl_zlest0084-tag_vl_pedagio.
                        MODIFY lt_zlest0084   FROM wl_zlest0084 INDEX sy-tabix.
                      ENDLOOP.
                    ENDIF.
*-CS2024001181-16.12.2024-#160717-JT-fim

                    SORT lt_zlest0084 BY vlr_pedagio ASCENDING.
                    READ TABLE lt_zlest0084 INTO wk_zlest0084 INDEX 1.
                    IF ( wk_xvttk-dalen >= wk_zlest0084-dt_vigencia ).
                      IF NOT ( wk_zlest0084-vlr_pedagio  IS INITIAL ).

                        IF vg_ck_credito_ped NE abap_true.
                          zlest0026-ck_credita_ped = abap_true.
                        ELSE.
                          zlest0026-ck_credita_ped = vg_credito_ped.
                        ENDIF.

                        lc_credita_pedagio = zlest0026-ck_credita_ped.  "*-CS2024000522-18.07.2024-JT-#143588-inicio

*-#146705-25.07.2024-JT-#146705-inicio
                        CALL FUNCTION 'Z_CK_PEDAGIO_MESMO_VEICULO'
                          EXPORTING
                            i_zlest0090      = wk_zlest0090
                            i_zlest0026      = zlest0026
                            i_tknum          = wk_xvttk-tknum
                          IMPORTING
                            e_mesmo_veiculo  = vg_pedi_mesmo_veicul
                            e_mesma_carga    = vg_pedi_mesma_carga
                          CHANGING
                            m_ck_credita_ped = lc_credita_pedagio. "*-#146705-25.07.2024-JT-#146705
*-#146705-25.07.2024-JT-#146705-fim

                        IF vg_ck_credito_ped = abap_false. "*-#146705-25.07.2024-JT-#146705
                          zlest0026-ck_credita_ped = lc_credita_pedagio.
                          vg_pedi_pedagio          = lc_credita_pedagio. "*-CS2024001181-16.12.2024-#160717-JT-inicio
                        ENDIF.

                        IF zlest0026-ck_credita_ped EQ abap_true.
                          zlest0026-pedagio      = wk_zlest0084-vlr_pedagio.
                        ENDIF.
                        zlest0026-id_rota        = wk_zlest0084-id_rota.
                        zlest0026-qtd_eixo       = wk_zlest0091-qtd_eixo.

*                        SELECT SINGLE * INTO WA_ZLEST0026_AUX
*                          FROM ZLEST0026 AS Z
*                         WHERE Z~PLACA_CAV EQ ZLEST0026-PLACA_CAV
*                           AND Z~ERDAT     EQ SY-DATUM
*                           AND Z~ID_ROTA   EQ ZLEST0026-ID_ROTA
*                           AND EXISTS ( SELECT * FROM VTTK AS T WHERE T~TKNUM EQ Z~TKNUM AND T~TKNUM NE WK_XVTTK-TKNUM ).
*
*                        IF SY-SUBRC IS INITIAL.
*                          VG_PEDI_MESMO_VEICUL = ABAP_TRUE.
*                        ENDIF.

*-#146705-25.07.2024-JT-#146705-inicio-comentado
*                       CALL FUNCTION 'Z_CK_PEDAGIO_MESMO_VEICULO'
*                         EXPORTING
*                           i_zlest0090      = wk_zlest0090
*                           i_zlest0026      = zlest0026
*                           i_tknum          = wk_xvttk-tknum
*                         IMPORTING
*                           e_mesmo_veiculo  = vg_pedi_mesmo_veicul
*                           e_mesma_carga    = vg_pedi_mesma_carga
*                         CHANGING
*                           m_ck_credita_ped = zlest0026-ck_credita_ped. "*-#146705-25.07.2024-JT-#146705
*-#146705-25.07.2024-JT-#146705-fim-comentado

                        IF vg_pedi_mesma_carga EQ abap_true OR zlest0026-ck_credita_ped = abap_false. "*-#146705-25.07.2024-JT-#146705.
                          zlest0026-ck_credita_ped = abap_false.
                          CLEAR: zlest0026-pedagio.
                        ENDIF.

                      ENDIF.
                    ELSE.
                      MESSAGE e000(fi) DISPLAY LIKE 'W' WITH 'Data de Vigência menor que a do transporte.'.
                    ENDIF.
                  ENDIF.
                ENDIF.
            ENDCASE.

          WHEN 'M'. "Manual

            SELECT * FROM zlest0002
              INTO TABLE lt_zlest0002
            WHERE pc_veiculo IN (wk_xvttk-text1(7),wk_xvttk-text2(7),wk_xvttk-text3(7),wk_xvttk-text4(7) ).

            IF ( sy-subrc EQ 0 ).

              zlest0026-placa_cav = wk_xvttk-text1(7).

              CALL FUNCTION 'Z_CK_PEDAGIO_MESMO_VEICULO'
                EXPORTING
                  i_zlest0090     = wk_zlest0090
                  i_zlest0026     = zlest0026
                  i_tknum         = wk_xvttk-tknum
                IMPORTING
                  e_mesmo_veiculo = vg_pedi_mesmo_veicul
                  e_mesma_carga   = vg_pedi_mesma_carga.

              IF vg_pedi_mesma_carga EQ abap_true.
                CLEAR: zlest0026-pedagio.
              ENDIF.

            ENDIF.

        ENDCASE.

      ENDIF.
    ELSE.
      qt_zlest0090 = 0.
    ENDIF.

    IF wk_xvttk-tknum IS NOT INITIAL. "Faturamento Contingencia ECC
      SELECT SINGLE *
        FROM vttp INTO @DATA(lwa_vttp)
       WHERE tknum EQ @wk_xvttk-tknum.

      IF sy-subrc EQ 0 AND lwa_vttp-vbeln IS NOT INITIAL.

        DATA: lwa_faturamento_ecc TYPE zde_compare_faturamento.

        SELECT SINGLE *
          FROM zsdt0001 INTO @DATA(lwa_zsdt0001_s)
         WHERE doc_rem      = @lwa_vttp-vbeln
           AND tp_movimento = 'S'.

        IF sy-subrc EQ 0 AND lwa_zsdt0001_s-fat_contingencia_ecc EQ abap_true.
          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
            EXPORTING
              i_ch_referencia         = lwa_zsdt0001_s-ch_referencia
              i_get_dados_fat_ecc     = abap_true
            IMPORTING
              e_dados_faturamento_ecc = lwa_faturamento_ecc.

          zlest0026-adto = lwa_faturamento_ecc-kbetr_zadm.
        ELSE.

          SELECT SINGLE *
            FROM zlest0108 INTO @DATA(lwa_zlest0108)
           WHERE vbeln EQ @lwa_vttp-vbeln.

          IF sy-subrc EQ 0 AND lwa_zlest0108-fat_contingencia_ecc EQ abap_true.
            CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
              EXPORTING
                i_vbeln                 = lwa_zlest0108-vbeln
                i_get_dados_fat_ecc     = abap_true
              IMPORTING
                e_dados_faturamento_ecc = lwa_faturamento_ecc.

            zlest0026-adto = lwa_faturamento_ecc-kbetr_zadm.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDIF.

*-#133089-12.02.2024-JT-inicio
  IF vg_faturauto_in IS NOT INITIAL.

    DATA: t_pracas TYPE TABLE OF zlest0102.

*-- pracas
    IF zlest0026-id_rota IS NOT INITIAL.
      SELECT *
        INTO TABLE t_pracas
        FROM zlest0102
       WHERE id_rota_adm EQ zlest0026-id_rota
         AND branch      EQ zlest0101-branch.

      DELETE t_pracas WHERE st_praca = abap_false.
    ELSEIF zlest0026-id_rota IS INITIAL.
      FREE: t_pracas[].
    ENDIF.

    MOVE t_pracas[]               TO vg_faturauto_out-pracas[].
    MOVE-CORRESPONDING zlest0101  TO vg_faturauto_out.
    MOVE-CORRESPONDING zlest0026  TO vg_faturauto_out.

    "Valor do Pedágio (Mostrar/Informar)
    "Não pode alterar quando: Sem Pedágio ou Pedágio Eletrônico
    IF vg_pedi_pedagio EQ abap_false OR vg_cartao_pedagio EQ abap_true OR vg_pedi_mesma_carga EQ abap_true.
      vg_faturauto_out-edita_pedagio = abap_false.
    ELSE.
      vg_faturauto_out-edita_pedagio = abap_true.
    ENDIF.

    "Cartão de Pagamento (Mostrar)
    IF ( vg_cartao_pedagio EQ abap_false ) OR ( zlest0026-tp_admim_ped EQ '03'  ) OR
      "Processo de Pedágio REPOM Emitido (Bloqueia)
       ( zlest0026-id_proc_cliente IS NOT INITIAL ).
      vg_faturauto_out-edita_tp_card_ped = abap_false.
    ELSE.
      vg_faturauto_out-edita_tp_card_ped = abap_true.
    ENDIF.

    "Credita Pedágio (Mostrar)
    IF vg_pedi_mesmo_veicul EQ abap_false OR vg_cartao_pedagio EQ abap_false OR
       zlest0026-id_proc_cliente IS NOT INITIAL OR vg_pedi_mesma_carga IS NOT INITIAL.
      IF zlest0026-id_proc_cliente IS NOT INITIAL AND vg_pedi_mesmo_veicul EQ abap_true.
*-CS2024000522-18.07.2024-JT-#143588-inicio
        IF zlest0026-ck_credita_ped = abap_false.
          vg_faturauto_out-edita_ck_credita_ped = abap_true.
        ELSE.
          vg_faturauto_out-edita_ck_credita_ped = abap_false.
        ENDIF.
      ELSE.
        IF zlest0026-ck_credita_ped = abap_false.
          vg_faturauto_out-edita_ck_credita_ped = abap_true.
        ELSE.
          vg_faturauto_out-edita_ck_credita_ped = abap_false.
        ENDIF.
      ENDIF.
*-CS2024000522-18.07.2024-JT-#143588-fim
    ELSE.
      vg_faturauto_out-edita_ck_credita_ped = abap_true.
    ENDIF.

    "Escolher Administradora Pedágio
    IF vg_pedi_pedagio EQ abap_false OR zlest0026-id_proc_cliente IS NOT INITIAL OR qt_zlest0090 LE 1.
      vg_faturauto_out-edita_tp_admim_ped = abap_false.
    ELSE.
      vg_faturauto_out-edita_tp_admim_ped = abap_true.
    ENDIF.

    EXIT.
  ENDIF.
*-#133089-12.02.2024-JT-fim

  "Ajusta campos de valor de pedágio e de cartão de pedágio
  LOOP AT SCREEN.

    "Valor do Pedágio (Mostrar/Informar)
    "Não pode alterar quando: Sem Pedágio ou Pedágio Eletrônico
    IF screen-group1     = 'GR1'.
      screen-output      = '1'.
      IF vg_pedi_pedagio EQ abap_false OR vg_cartao_pedagio EQ abap_true OR vg_pedi_mesma_carga EQ abap_true.
        screen-input     = '0'.
      ELSE.
        screen-input     = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    "Cartão de Pagamento (Mostrar)
    IF screen-group1     = 'GR2'.
      screen-output      = '1'.
      IF ( vg_cartao_pedagio EQ abap_false ) OR
        "
         ( zlest0026-tp_admim_ped EQ '03' AND screen-name EQ 'ZLEST0026-TP_CARD_PED' ) OR
        "Processo de Pedágio REPOM Emitido (Bloqueia)
         ( zlest0026-id_proc_cliente IS NOT INITIAL ).
        screen-input     = '0'.
      ELSE.
        screen-input     = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    "Credita Pedágio (Mostrar)
    IF screen-group1     = 'GR3'.
      screen-output      = '1'.
      IF vg_pedi_mesmo_veicul EQ abap_false OR vg_cartao_pedagio EQ abap_false OR zlest0026-id_proc_cliente IS NOT INITIAL OR vg_pedi_mesma_carga IS NOT INITIAL.
        IF zlest0026-id_proc_cliente IS NOT INITIAL AND vg_pedi_mesmo_veicul EQ abap_true.
*-#146705-25.07.2024-JT-#146705-inicio
          IF screen-name EQ 'ZLEST0026-TX_OBS_CRED_MESM'.
            IF zlest0026-ck_credita_ped = abap_false.
              screen-input     = '1'.
            ELSE.
              screen-input     = '0'.
            ENDIF.
          ELSE.
            screen-input     = '0'.
          ENDIF.
        ELSE.
          IF screen-name EQ 'ZLEST0026-TX_OBS_CRED_MESM'.
            IF zlest0026-ck_credita_ped = abap_false.
              screen-input     = '1'.
            ELSE.
              screen-input     = '0'.
            ENDIF.
          ELSE.
            screen-input     = '0'.
          ENDIF.
        ENDIF.
*-#146705-25.07.2024-JT-#146705-fim
      ELSE.
        screen-input     = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

    "Escolher Administradora Pedágio
    IF screen-group1     = 'GR4'.
      screen-output      = '1'.
      IF vg_pedi_pedagio EQ abap_false OR zlest0026-id_proc_cliente IS NOT INITIAL OR qt_zlest0090 LE 1.
        screen-input     = '0'.
      ELSE.
        screen-input     = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.

*-#133089-12.02.2024-JT-inicio
****************************************************************
* ajustar dados faturamento automatico para tela
****************************************************************
FORM f_ajustar_fatur_auto_tela.

  zlest0026-tp_admim_frete   = vg_zlest0240_in-tp_admim_frete.
  zlest0026-tp_admim_ped     = vg_zlest0240_in-tp_admim_ped.
  zlest0026-adto             = vg_zlest0240_in-vl_adiantamento .
  zlest0026-id_rota          = vg_zlest0240_in-id_rota_tip_frete.
  zlest0026-qtd_eixo         = vg_zlest0240_in-nm_qtd_eixos.
  zlest0026-ck_credita_ped   = vg_zlest0240_in-ck_credita_ped.
  zlest0026-tx_obs_cred_mesm = vg_zlest0240_in-tx_obs_cred_mesm.
  zlest0101-cd_cid_origem    = vg_zlest0240_in-cd_cid_origem.
  zlest0101-cd_cid_destino   = vg_zlest0240_in-cd_cid_destino.

ENDFORM.

****************************************************************
* tratamento faturamento automatico
****************************************************************
FORM f_tratar_faturamento.

  SELECT SINGLE *
    FROM zlest0240
    INTO @DATA(w_zlest0240)
   WHERE ch_faturamento = @w_zlest0241-ch_faturamento
     AND cancelado      = @abap_false.

  CHECK sy-subrc = 0.

*-----------------------------------
* mover estrutura tela
*-----------------------------------
  zlest0026-tp_admim_frete   = w_zlest0240-tp_admim_frete.
  zlest0026-tp_admim_ped     = w_zlest0240-tp_admim_ped.
  zlest0026-tp_card_ped      = w_zlest0240-tp_card_ped.
* zlest0026-id_rota          = w_zlest0240-id_rota_tip_frete.
* zlest0026-qtd_eixo         = w_zlest0240-nm_qtd_eixos.
  zlest0101-cd_cid_origem    = w_zlest0240-cd_cid_origem.
  zlest0101-cd_cid_destino   = w_zlest0240-cd_cid_destino.
  zlest0026-adto             = w_zlest0240-vl_adiantamento.
* zlest0026-pedagio          = w_zlest0240-vl_pedagio.
  zlest0026-nr_card_ped      = w_zlest0240-nr_card_ped.
  zlest0026-ck_credita_ped   = w_zlest0240-ck_credita_ped.
  zlest0026-tx_obs_cred_mesm = w_zlest0240-tx_obs_cred_mesm.

*-----------------------------------
* gravar
*-----------------------------------
  PERFORM f_confirma.

ENDFORM.

****************************************************************
* confirmar pedagio
****************************************************************
FORM f_confirma.

  DATA: wa_xvttk  TYPE vttkvb.
  DATA: e_consultas  TYPE zlest0135_t,
        wa_zlest0135 TYPE zlest0135.

*-#133089-12.02.2024-JT-inicio
  IF vg_faturamento_autom = abap_true.
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.
*-#133089-12.02.2024-JT-fim

  READ TABLE ti_xvttk INTO wa_xvttk INDEX 1.

  IF zlest0026-placa_cav IS INITIAL.
    zlest0026-placa_cav = wa_xvttk-text1(7).
  ENDIF.

  IF zlest0026-placa_cav IS NOT INITIAL.
    CALL METHOD zcl_webservice_tipcard=>cons_situacao_transportador
      EXPORTING
        i_placa     = zlest0026-placa_cav
      RECEIVING
        e_consultas = e_consultas
      EXCEPTIONS
        erro        = 1
        webservice  = 2
        OTHERS      = 3.

    IF sy-subrc IS NOT INITIAL.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE i000(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        WHEN abap_true.
          MESSAGE i000(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(l_mesg).
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
          RAISE pedagio.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
      EXIT.
    ENDIF.

    READ TABLE e_consultas INDEX 1 INTO wa_zlest0135.
    IF wa_zlest0135-ck_rntrc_ativo EQ abap_false.
      sy-msgv1 = wa_zlest0135-ds_msg_transportador+000(50).
      sy-msgv2 = wa_zlest0135-ds_msg_transportador+050(50).
      sy-msgv3 = wa_zlest0135-ds_msg_transportador+100(50).
      sy-msgv4 = wa_zlest0135-ds_msg_transportador+150(50)..
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE i000(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        WHEN abap_true.
          MESSAGE i000(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
          RAISE pedagio.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
      EXIT.
    ENDIF.
  ENDIF.

  CHECK vg_ck_alterou_cidade EQ abap_false.

  CHECK vg_ck_admim_ped EQ abap_false.

  CHECK vg_ck_admim_frete EQ abap_false.

  CHECK vg_ck_credito_pedt EQ abap_false.

*-#146705-25.07.2024-JT-#146705-inicio
* IF zlest0026-ck_credita_ped EQ abap_true  AND vg_pedi_mesmo_veicul EQ abap_true AND zlest0026-tx_obs_cred_mesm IS INITIAL.
  IF zlest0026-ck_credita_ped EQ abap_false AND vg_pedi_mesmo_veicul EQ abap_true AND zlest0026-tx_obs_cred_mesm IS INITIAL.
*-#146705-25.07.2024-JT-#146705-fim
*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_off.
        MESSAGE i100(zles).
      WHEN abap_true.
        MESSAGE i100(zles) INTO l_mesg.
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
        RAISE pedagio.
    ENDCASE.
*-#133089-21.02.2024-JT-fim
    EXIT.
  ENDIF.

  IF vg_ck_ped_param = abap_false.
*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_off.
        MESSAGE i092(zles).
      WHEN abap_true.
        MESSAGE i092(zles) INTO l_mesg.
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
        RAISE pedagio.
    ENDCASE.
*-#133089-21.02.2024-JT-fim
    EXIT.
  ENDIF.

  IF zlest0026-pedagio LE 0 AND
     ( ( vg_pedi_pedagio EQ abap_true AND vg_cartao_pedagio = abap_false ) OR
       ( vg_pedi_pedagio EQ abap_true AND vg_cartao_pedagio = abap_true AND zlest0026-ck_credita_ped EQ abap_true ) ).

    IF ( vg_cartao_pedagio    EQ abap_false ) AND
       ( vg_pedi_mesmo_veicul EQ abap_false ) AND
       ( vg_pedi_mesma_carga  EQ abap_false ).
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE i070(zles) .
        WHEN abap_true.
          MESSAGE i070(zles) INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
          RAISE pedagio.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
      EXIT.
    ENDIF.

** Validação inserida fixa, pois o processo deve deixar de existir com a implementação da TIP
    "===================================================USER STORY 61743 / Anderson Oenning
*      ELSEIF zlest0026-pedagio GT 2200 AND  vg_pedi_pedagio EQ abap_true.
*        MESSAGE i079(zles) .
*        EXIT.
    "===================================================USER STORY 61743 / Anderson Oenning
  ENDIF.

  "Verificar Autorização de Viagem """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "Verificar Autorização de Viagem """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF zlest0026-ck_credita_ped EQ abap_true AND vg_cartao_pedagio = abap_true.
    CASE zlest0026-tp_admim_ped.
      WHEN '03'. "REPOM S.A.

        CALL METHOD zcl_repom_viagem_vpr=>get_id_proc_cliente_vt
          EXPORTING
            i_tknum           = vl_tknum
          RECEIVING
            e_id_proc_cliente = zlest0026-id_proc_cliente
          EXCEPTIONS
            nao_encontrado    = 1
            OTHERS            = 2.

        IF sy-subrc IS NOT INITIAL.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            WHEN abap_true.
              MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
              RAISE pedagio.
          ENDCASE.
*-#133089-21.02.2024-JT-fim
          EXIT.
        ENDIF.

        IF zcl_repom_viagem_vpr=>get_autorizado( EXPORTING i_id_proc_cliente = zlest0026-id_proc_cliente ) NE abap_true.
          CLEAR: zlest0026-pedagio.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE i037(zrepom) .
            WHEN abap_true.
              MESSAGE i037(zrepom) INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
              RAISE pedagio.
          ENDCASE.
*-#133089-21.02.2024-JT-fim
          EXIT.
        ELSE.
          CREATE OBJECT obj_pedagio EXPORTING i_id_proc_cliente = zlest0026-id_proc_cliente.
          obj_pedagio->get_registro( IMPORTING e_registro = wa_zlest0123 ).
          IF zlest0026-pedagio NE wa_zlest0123-vlr_total_pedagio.
            zlest0026-pedagio = wa_zlest0123-vlr_total_pedagio.
            CLEAR: obj_pedagio.
            EXIT.
          ELSE.
            CLEAR: obj_pedagio.
          ENDIF.
        ENDIF.
      WHEN '09'. "TipFrete
        IF zlest0026-placa_cav IS NOT INITIAL.
          IF wa_zlest0135-ck_sem_parar EQ abap_false AND zlest0026-tp_card_ped EQ 'S'. "Sem Para
            sy-msgv1 = wa_zlest0135-ds_msg_veiculo+000(50).
            sy-msgv2 = wa_zlest0135-ds_msg_veiculo+050(50).
            sy-msgv3 = wa_zlest0135-ds_msg_veiculo+100(50).
            sy-msgv4 = wa_zlest0135-ds_msg_veiculo+150(50)..
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE i000(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              WHEN abap_true.
                MESSAGE i000(zles) WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
                RAISE pedagio.
            ENDCASE.
*-#133089-21.02.2024-JT-fim
            EXIT.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDIF.
  "Verificar Autorização de Viagem """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  "Tip Frete """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  IF zlest0026-tp_admim_ped NE '03'.
    IF vg_cartao_pedagio EQ abap_true AND zlest0026-tp_card_ped IS INITIAL.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE i081(zles).
        WHEN abap_true.
          MESSAGE i081(zles) INTO l_mesg.
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
          RAISE pedagio.
      ENDCASE.
*-#133089-21.02.2024-JT-fim
      EXIT.
    ELSEIF ( vg_cartao_pedagio     EQ abap_true   ) AND
           ( zlest0026-tp_card_ped IS NOT INITIAL ) AND
           ( zlest0026-tp_card_ped NE 'S'         ) AND "S = Sem Parar
           ( zlest0026-tp_card_ped NE 'O'         ) AND "O = Visa Cargo
           ( zlest0026-tp_card_ped NE 'G'         ) AND "G = TAG Strada Pass "*-CS2024001181-16.12.2024-#160717-JT
           ( zlest0026-tp_card_ped NE 'M'         ).    "M = MoveMais        "*-CS2024001181-16.12.2024-#160717-JT

      SELECT SINGLE * INTO @DATA(wa_zlest0002_card)
        FROM zlest0002_card
       WHERE pc_veiculo  EQ @zlest0026-placa_cav
         AND tp_card_ped EQ @zlest0026-tp_card_ped.

      IF sy-subrc IS INITIAL.
        IF ( zlest0026-nr_card_ped IS INITIAL ) OR ( zlest0026-nr_card_ped NE wa_zlest0002_card-nr_card_ped ).
          zlest0026-nr_card_ped = wa_zlest0002_card-nr_card_ped.
          EXIT.
        ELSE.
          zlest0026-nr_card_ped = wa_zlest0002_card-nr_card_ped.
          IF zlest0026-ck_credita_ped EQ abap_false.
            CLEAR: zlest0026-pedagio.
          ENDIF.
        ENDIF.
      ELSE.
*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            MESSAGE i081(zles) .
          WHEN abap_true.
            MESSAGE i081(zles) INTO l_mesg .
            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
            RAISE pedagio.
        ENDCASE.
*-#133089-21.02.2024-JT-fim
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

* Documento de custo de frete
  SELECT SINGLE fknum
  INTO   vl_fknum
  FROM   vfkp
  WHERE  refty = '8' AND
         rebel = vl_tknum.

  IF sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_off.
        MESSAGE w024(zles) WITH vl_fknum.
      WHEN abap_true.
        MESSAGE w024(zles) WITH vl_fknum INTO l_mesg.
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
        RAISE pedagio.
    ENDCASE.
*-#133089-21.02.2024-JT-fim
    EXIT.
  ELSE.

* Cabeçalho do documento de transporte
    SELECT SINGLE tplst tdlnr
      INTO  (vl_tplst,vl_tdlnr)
      FROM vttk
     WHERE tknum = vl_tknum.

* Local de organizaçao de transporte
    SELECT SINGLE bukrs
    INTO   vl_bukrs
    FROM   ttds
    WHERE  tplst = vl_tplst.

    TRY .
        DATA(r_margadto) = zcl_calc_frete=>get_valor_adiantamento(
          EXPORTING
            i_bukrs  = vl_bukrs
            i_branch = vl_tplst
            i_lifnr  = wa_zlest0135-cd_transportador ).

        vl_adto = ( wk_netwr_all * r_margadto ) / 100.
        IF zlest0026-adto > vl_adto.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE i022(zles) WITH r_margadto.
            WHEN abap_true.
              MESSAGE i022(zles) WITH r_margadto INTO l_mesg.
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = w_zlest0241-ch_referencia i_type = 'E' i_msg = l_mesg i_status = 'TRAN' ).
              RAISE pedagio.
          ENDCASE.
*-#133089-21.02.2024-JT-fim
          EXIT.
        ENDIF.

      CATCH zcx_calc_frete.
    ENDTRY.

* Determinação do itinerário
    SELECT SINGLE route
    INTO   vl_route
    FROM   vttk
    WHERE  tknum = vl_tknum.

    IF sy-subrc IS INITIAL.
      IF ( NOT vg_pedi_pedagio IS INITIAL ) AND ( zlest0026-pedagio GT 0 ).
        vg_pedi_informado = 'X'.
      ELSE.
        CLEAR: vg_pedi_informado.
      ENDIF.
    ENDIF.

    DATA(ck_gravou) = abap_false.
    PERFORM gravar_registro CHANGING ck_gravou.
    IF ck_gravou EQ abap_true.
*-#133089-21.02.2024-JT-inicio
      IF vg_faturamento_autom = abap_true.
        EXIT.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
*-#133089-21.02.2024-JT-fim
    ENDIF.
  ENDIF.

ENDFORM.
*-#133089-12.02.2024-JT-fim
****************************************************************
****************************************************************
