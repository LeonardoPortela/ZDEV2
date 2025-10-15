************************************************************************
*&                        AMAGGI                                      &*
************************************************************************
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 13.03.2025                                              &*
*& Descrição: Cockpit Aprovação Boleta Compra - Comercial             &*
************************************************************************

************************************************************************
* includes
************************************************************************
INCLUDE zmmr0045_top.
INCLUDE zmmr0045_classe.
INCLUDE zmmr0045_status_0100o01.
INCLUDE zmmr0045_status_0200o01.
INCLUDE zmmr0045_status_0300o01.

************************************************************************
* SCREEN OUTPUT
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  IF p_submit IS INITIAL.
    APPEND VALUE #( sign = 'I'  option = 'BT'  low = sy-datum - 30 high = sy-datum ) TO s_dtsol.
    APPEND VALUE #( sign = 'I'  option = 'EQ'  low = 'LP'                          ) TO s_status.
  ENDIF.

************************************************************************
* start
************************************************************************
START-OF-SELECTION.

*-----------------------
* autorizacao
*-----------------------
  AUTHORITY-CHECK OBJECT 'ZMM0236'
                      ID 'BUKRS'      FIELD s_bukrs-low
                      ID 'ZTP_INSUMO' FIELD s_tpins-low.
  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Perfil usuário sem acesso à ' 'Empresa e Tipo de Insumo!' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

*-----------------------
* processamento
*-----------------------
  PERFORM f_selecao_dados.

  IF t_boleta[] IS INITIAL.
    MESSAGE s024(sd) WITH TEXT-002 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM f_processa_dados.

  IF p_submit = abap_off.
    CALL SCREEN 0100.
  ELSE.
    CALL SCREEN 0100 STARTING AT  02 01
                       ENDING AT 185 22.
  ENDIF.

************************************************************************
* selecao dados
************************************************************************
FORM f_selecao_dados.

  FREE: r_dtsol.

*-------------------------------------------------
*-converter datas
*-------------------------------------------------
  READ TABLE s_dtsol INDEX 1.
  IF s_dtsol-high IS INITIAL.
    s_dtsol-high = s_dtsol-low.
  ENDIF.

  r_dtsol-sign   = 'I'.
  r_dtsol-option = 'BT'.
  CONVERT DATE s_dtsol-low  TIME '000000'  INTO TIME STAMP r_dtsol-low  TIME ZONE sy-zonlo.
  CONVERT DATE s_dtsol-high TIME '235959'  INTO TIME STAMP r_dtsol-high TIME ZONE sy-zonlo.
  APPEND r_dtsol.

*-------------------------------------------------
*-selecao dados
*-------------------------------------------------
  SELECT *
    FROM zi_mm_boleta_compra_info
    INTO TABLE @t_boleta
   WHERE idboletacompra    IN @s_idbol
     AND bukrs             IN @s_bukrs
     AND createdat         IN @r_dtsol
     AND tipoinsumo        IN @s_tpins
     AND safra             IN @s_safra
     AND waers             IN @s_moeda
     AND statusworkflow    IN @s_status
     AND fornecedor        IN @s_fornec.

  CHECK t_boleta[] IS NOT INITIAL.

  SELECT *
    FROM zi_mm_boleta_comp_it_info
    INTO TABLE @t_boleta_item
     FOR ALL ENTRIES IN @t_boleta
   WHERE guidhd = @t_boleta-guid.

  SELECT *
    FROM zi_mm_boleta_comp_parc_info
    INTO TABLE @t_boleta_parc
     FOR ALL ENTRIES IN @t_boleta
   WHERE guidhd = @t_boleta-guid.

  SELECT *
    FROM zi_mm_boleta_comp_wf_info
    INTO TABLE @t_boleta_wf
     FOR ALL ENTRIES IN @t_boleta
   WHERE guidhd = @t_boleta-guid.

ENDFORM.

************************************************************************
* processa dados
************************************************************************
FORM f_processa_dados.

  DATA l_qtd_item TYPE i.

  FREE: t_saida.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZDO_TP_USUARIO_WF_BOLETA'
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = t_idd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  LOOP AT t_boleta          INTO w_boleta.
    FREE: l_qtd_item.

*   LOOP AT t_boleta_item   INTO w_boleta_item WHERE guidhd = w_boleta-guid.
*     l_qtd_item = l_qtd_item + 1.
*   ENDLOOP.

    CONVERT TIME STAMP w_boleta-createdat  TIME ZONE sy-zonlo INTO DATE w_saida-data_adiantamento.

    w_saida-guid               = w_boleta-guid.
    w_saida-id_boleta_compra   = w_boleta-idboletacompra.
    w_saida-royalties_valor    = w_boleta-valortotalit.
    w_saida-fornecedor         = w_boleta-fornecedor.
    w_saida-tipo_insumo        = w_boleta-desctpinsumo.
    w_saida-safra              = w_boleta-safra.
    w_saida-waers              = w_boleta-waers.
    w_saida-condicao_negocio   = w_boleta-desccondnegocio.
    w_saida-inco1              = w_boleta-inco1.

*----------------------------------------
* status Anexos
*----------------------------------------
    FREE: t_anexos.
    lv_obj_key = w_boleta-idboletacompra.

    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
      EXPORTING
        classname          = 'ZMMBOLCOMP'
        objkey             = lv_obj_key
      TABLES
        gos_connections    = t_anexos
      EXCEPTIONS
        no_objects_found   = 1
        internal_error     = 2
        internal_gos_error = 3
        OTHERS             = 4.

    w_saida-anexos             = COND #( WHEN t_anexos[] IS NOT INITIAL THEN icon_led_green
                                                                        ELSE icon_dummy ).
    CASE w_boleta-statusworkflow.
      WHEN 'LP'.
        lv_icon = icon_led_green.
      WHEN 'EL'.
        lv_icon = icon_led_yellow.
      WHEN 'RE'.
        lv_icon = icon_led_red.
      WHEN OTHERS.
        lv_icon = icon_dummy.
    ENDCASE.

    w_saida-st_workflow        = COND #( WHEN w_boleta-statusworkflow IS INITIAL THEN icon_dummy
                                                                                 ELSE lv_icon && w_boleta-descstwf ).
    CASE w_boleta-stsoliccrit.
      WHEN '3'.
        lv_icon = icon_led_green.
      WHEN '2'.
        lv_icon = icon_led_yellow.
      WHEN '1'.
        lv_icon = icon_led_red.
      WHEN OTHERS.
        lv_icon = icon_dummy.
    ENDCASE.

    w_saida-st_solic           = COND #( WHEN w_boleta-stsolic        IS INITIAL THEN icon_dummy
                                                                                 ELSE lv_icon && w_boleta-stsolic ).
    CASE w_boleta-stpedcrit.
      WHEN '3'.
        lv_icon = icon_led_green.
      WHEN '2'.
        lv_icon = icon_led_yellow.
      WHEN '1'.
        lv_icon = icon_led_red.
      WHEN OTHERS.
        lv_icon = icon_dummy.
    ENDCASE.

    w_saida-st_ped             = COND #( WHEN w_boleta-stped          IS INITIAL THEN icon_dummy
                                                                                 ELSE lv_icon && w_boleta-stped ).
    w_saida-user_solic         = w_boleta-createdby.
    w_saida-user_retorno       = w_boleta-usuarioretorno.
    w_saida-user_pedido        = w_boleta-usuariocriaped.
    w_saida-qtd_item           = w_boleta-qtditens. "l_qtd_item.

    APPEND w_saida            TO t_saida.
  ENDLOOP.

  SORT t_saida BY id_boleta_compra.

ENDFORM.

************************************************************************
* visualizar / editar boleta
************************************************************************
FORM f_manutencao_boleta USING p_editar.

  FREE: tl001, t_saida_parc, t_saida_wf, t_saida_item, t_saida_item_del.

  lv_editar = p_editar.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecionar uma linha!' DISPLAY LIKE 'W'.
    RETURN.
  ELSEIF lines( t_rows[] ) > 1.
    MESSAGE s024(sd) WITH 'Selecionar Somente uma linha!' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  READ TABLE t_rows     INTO w_rows        INDEX 1.
  READ TABLE t_saida    INTO w_saida       INDEX w_rows-index.
  READ TABLE t_boleta   INTO w_boleta       WITH KEY guid = w_saida-guid.

  CONVERT TIME STAMP w_boleta-createdat  TIME ZONE sy-zonlo INTO DATE tl001-dt_solicitacao.

  tl001-id_boleta_compra              = w_boleta-idboletacompra.
  tl001-usuario_sol                   = w_boleta-createdby.
  tl001-bukrs                         = w_boleta-bukrs.
  tl001-moeda                         = w_boleta-waers.
  tl001-dt_adiantamento               = w_boleta-dataadiantamento.
  tl001-kursf                         = w_boleta-kursf.
  tl001-perc_adiantamento             = w_boleta-percadiantamento.
  tl001-tipo_insumo                   = w_boleta-desctpinsumo.
  tl001-frete                         = w_boleta-inco1.
  tl001-safra                         = w_boleta-safra.
  tl001-cond_negocio                  = w_boleta-desccondnegocio.
  tl001-fornecedor                    = w_boleta-fornecedor.
  tl001-dt_pagto                      = w_boleta-datavencimento.
  tl001-royalties_data                = w_boleta-royaltiesdata.
  tl001-pagto_pos_embarque            = w_boleta-pagamentoposembarque.
  tl001-royalties_valor               = w_boleta-royaltiesvalor.
  tl001-observacao1                   = w_boleta-observacoes(128).
  tl001-observacao2                   = w_boleta-observacoes+128(127).

  LOOP AT t_boleta_parc            INTO w_boleta_parc WHERE guidhd = w_boleta-guid.
    CLEAR w_saida_parc.
    w_saida_parc-guid_hd              = w_boleta_parc-guidhd.
    w_saida_parc-guid                 = w_boleta_parc-guid.
    w_saida_parc-id_boleta_compra     = w_boleta_parc-idboletacompra.
    w_saida_parc-data_vencimento      = w_boleta_parc-datavencimento.
    w_saida_parc-waers                = w_boleta_parc-waers.
    w_saida_parc-valor                = w_boleta_parc-valor.
    APPEND w_saida_parc              TO t_saida_parc.
  ENDLOOP.

  LOOP AT t_boleta_item            INTO w_boleta_item WHERE guidhd = w_boleta-guid.
    CLEAR w_saida_item.
    MOVE-CORRESPONDING w_boleta_item TO w_saida_item.
    PERFORM f_edicao_itens        USING abap_false.
    APPEND w_saida_item              TO t_saida_item.
  ENDLOOP.

  LOOP AT t_boleta_wf              INTO w_boleta_wf   WHERE guidhd = w_boleta-guid.
    CLEAR: w_saida_wf, w_idd07v.

    READ TABLE t_idd07v            INTO w_idd07v WITH KEY domvalue_l = w_boleta_wf-tpusuario.

    CONVERT TIME STAMP w_boleta_wf-createdat  TIME ZONE sy-zonlo INTO DATE w_saida_wf-data.
    CONVERT TIME STAMP w_boleta_wf-createdat  TIME ZONE sy-zonlo INTO TIME w_saida_wf-hora.

    w_saida_wf-guid_hd                = w_boleta_wf-guidhd.
    w_saida_wf-guid                   = w_boleta_wf-guid.
    w_saida_wf-id_workflow            = w_boleta_wf-idworkflow.
    w_saida_wf-id_boleta_compra       = w_boleta_wf-idboletacompra.
    w_saida_wf-created_by             = w_boleta_wf-createdby.
    w_saida_wf-mensagem               = w_boleta_wf-mensagem.
    w_saida_wf-tp_usuario             = w_idd07v-ddtext.
    APPEND w_saida_wf                TO t_saida_wf.
  ENDLOOP.

  SORT t_saida_parc BY data_vencimento DESCENDING.
  SORT t_saida_wf   BY data            DESCENDING hora DESCENDING.

  CALL SCREEN 0210 STARTING AT 05  01
                     ENDING AT 185 23.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

ENDFORM.

************************************************************************
* EDICAO ITENS
************************************************************************
FORM f_manutencao_itens  USING p_ucomm.

  w_stable = 'XX'.

  CASE p_ucomm.
    WHEN 'DELETE' OR 'CHANGE'.
      CALL METHOD g_grid_item->get_selected_rows
        IMPORTING
          et_index_rows = t_rows.

      IF t_rows[] IS INITIAL.
        MESSAGE s024(sd) WITH 'Selecionar pelo menos uma linha!' DISPLAY LIKE 'W'.
        RETURN.
      ENDIF.
      LOOP AT t_rows INTO w_rows.
        READ TABLE t_saida_item    INTO w_saida_item INDEX w_rows-index.
        IF w_saida_item-inscockpitapro = abap_off.
          MESSAGE s024(sd) WITH 'Item não foi gerado pelo Cockpit! ' 'Não pode ser Ajustado/Eliminado!' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      ENDLOOP.

    WHEN 'COPIAR'.
      CALL METHOD g_grid_item->get_selected_rows
        IMPORTING
          et_index_rows = t_rows.

      IF t_rows[] IS INITIAL.
        MESSAGE s024(sd) WITH 'Selecionar uma linha para Copiar!' DISPLAY LIKE 'W'.
        RETURN.
      ELSEIF lines( t_rows[] ) > 1.
        MESSAGE s024(sd) WITH 'Selecionar Somente uma linha!' DISPLAY LIKE 'W'.
        RETURN.
      ENDIF.
  ENDCASE.

  CASE p_ucomm.
    WHEN 'DELETE'.
      LOOP AT t_rows            INTO w_rows.
        READ TABLE t_saida_item INTO w_saida_item INDEX w_rows-index.
        APPEND w_saida_item       TO t_saida_item_del.
        DELETE t_saida_item    INDEX w_rows-index.
      ENDLOOP.

    WHEN 'CHANGE'.
      LOOP AT t_rows            INTO w_rows.
        READ TABLE t_saida_item INTO w_saida_item INDEX w_rows-index.
        lv_tabix = sy-tabix.

        PERFORM f_edicao_itens USING abap_true.
        MODIFY t_saida_item     FROM w_saida_item INDEX lv_tabix.
      ENDLOOP.

    WHEN 'INSERT'.
      INSERT INITIAL LINE       INTO t_saida_item INDEX 1.
      READ TABLE t_saida_item   INTO w_saida_item INDEX 1.
      w_saida_item-novo            = abap_true.
      w_saida_item-inscockpitapro  = abap_true.
      PERFORM f_edicao_itens   USING abap_true.
      MODIFY t_saida_item       FROM w_saida_item INDEX 1.

    WHEN 'COPIAR'.
      CLEAR w_stable.
      READ TABLE t_rows         INTO w_rows INDEX 1.
      READ TABLE t_saida_item   INTO w_saida_item_cop INDEX w_rows-index.
      INSERT INITIAL LINE       INTO t_saida_item INDEX 1.
      READ TABLE t_saida_item   INTO w_saida_item INDEX 1.
      w_saida_item                 = w_saida_item_cop.
      w_saida_item-novo            = abap_true.
      w_saida_item-inscockpitapro  = abap_true.
      PERFORM f_edicao_itens   USING abap_true.
      MODIFY t_saida_item       FROM w_saida_item INDEX 1.

    WHEN 'SALVAR'.
      PERFORM f_validar_ajustes CHANGING lv_erro.
      IF lv_erro = abap_false.
        w_boleta_aux = w_boleta.
        PERFORM f_salvar_ajustes.
        PERFORM f_montar_tela_itens.
        PERFORM f_selecao_dados.
        PERFORM f_processa_dados.
        w_boleta     = w_boleta_aux.
      ENDIF.
  ENDCASE.

  CALL METHOD g_grid_item->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

ENDFORM.

************************************************************************
* validar ajustes itens
************************************************************************
FORM f_validar_ajustes CHANGING p_erro.

  FREE: p_erro.

  LOOP AT t_saida_item INTO w_saida_item WHERE editado = abap_true OR
                                               novo    = abap_true.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = w_saida_item-matnr
      IMPORTING
        output = w_saida_item-matnr.

    SELECT SINGLE matnr
      INTO @DATA(_matnr)
      FROM mara
     WHERE matnr = @w_saida_item-matnr.

    IF sy-subrc <> 0.
      MESSAGE s024(sd) WITH 'Cód. Material Incorreto!' DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

    SELECT SINGLE werks
      INTO @DATA(_werks)
      FROM t001w
     WHERE werks = @w_saida_item-werks.

    IF sy-subrc <> 0.
      MESSAGE s024(sd) WITH 'Centro informado Incorreto!' DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

    IF w_saida_item-maktx IS INITIAL.
      MESSAGE s024(sd) WITH 'Informar a Descrição do Material' DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

    IF w_saida_item-menge IS INITIAL.
      MESSAGE s024(sd) WITH 'Informar a Quantidade do Item!' DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

    IF w_saida_item-meins IS INITIAL.
      MESSAGE s024(sd) WITH 'Informar a Unidade de Medida!' DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

    IF w_saida_item-waers IS INITIAL.
      MESSAGE s024(sd) WITH 'Informar a Moeda!' DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

    IF w_saida_item-netwr IS INITIAL.
      MESSAGE s024(sd) WITH 'Informar o Valor Unitário!' DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

    IF w_saida_item-netwr IS INITIAL.
      MESSAGE s024(sd) WITH 'Informar o Valor Total!' DISPLAY LIKE 'E'.
      p_erro = abap_true.
      RETURN.
    ENDIF.

  ENDLOOP.

ENDFORM.

************************************************************************
* salvar ajustes itens
************************************************************************
FORM f_salvar_ajustes.

  DATA: lv_id_item    TYPE zmmt0193-id_item,
        lv_netpr      TYPE zmmt0193-netpr,
        lv_timestampl TYPE timestampl.

  GET TIME STAMP FIELD lv_timestampl.

  SORT t_saida_item BY iditem     DESCENDING.
  READ TABLE t_saida_item         INTO w_saida_item INDEX 1.
  lv_id_item = w_saida_item-iditem.

*----------------------
* tratar eliminados
*----------------------
  LOOP AT t_saida_item_del        INTO w_saida_item.
    DELETE FROM zmmt0193         WHERE guid_hd = w_saida_item-guidhd
                                   AND guid    = w_saida_item-guid.
  ENDLOOP.

*----------------------
* tratar alteracoes
*----------------------
  LOOP AT t_saida_item            INTO w_saida_item  WHERE novo    = abap_true
                                                        OR editado = abap_true.

    IF w_saida_item-novo = abap_true.
      lv_id_item = lv_id_item + 1.
      TRY.
          w_saida_item-guid   = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.
      ENDTRY.
      w_saida_item-guidhd     = w_boleta-guid.
      w_saida_item-iditem     = lv_id_item.
      w_saida_item-createdby  = sy-uname.
      w_saida_item-createdat  = lv_timestampl.
    ENDIF.

    MOVE-CORRESPONDING w_saida_item    TO w_zmmt0193.

    w_zmmt0193-id_boleta_compra         = w_boleta-idboletacompra.
    w_zmmt0193-guid_hd                  = w_saida_item-guidhd.
    w_zmmt0193-id_item                  = w_saida_item-iditem.
    w_zmmt0193-local_embarque           = w_saida_item-localembarque.
    w_zmmt0193-nro_sol_cp               = w_saida_item-nrosolcp.
    w_zmmt0193-insert_cockpit_aprovacao = abap_true.
    w_zmmt0193-created_by               = w_saida_item-createdby.
    w_zmmt0193-created_at               = w_saida_item-createdat.
    w_zmmt0193-last_changed_by          = COND #( WHEN w_saida_item-novo = abap_false THEN sy-uname
                                                                                      ELSE w_saida_item-lastchangedby ).
    w_zmmt0193-last_changed_at          = COND #( WHEN w_saida_item-novo = abap_false THEN lv_timestampl
                                                                                      ELSE w_saida_item-lastchangedat ).
    w_zmmt0193-local_last_changed_at    = COND #( WHEN w_saida_item-novo = abap_false THEN lv_timestampl
                                                                                      ELSE w_saida_item-locallastchangedat ).
    MODIFY zmmt0193                  FROM w_zmmt0193.
  ENDLOOP.

  MESSAGE s024(sd) WITH 'Alterações gravadas com Sucesso!'.

  COMMIT WORK AND WAIT.

* SELECT SUM( netpr )
*   INTO @lv_netpr
*   FROM zi_mm_boleta_comp_it_info
*  WHERE guidhd = @w_saida_item-guidhd.
*
* UPDATE zmmt0191 SET VALORTOTALIT    = lv_netpr
*               WHERE guid            = w_saida_item-guidhd.
*
* COMMIT WORK AND WAIT.

ENDFORM.

************************************************************************
* salvar ajustes itens
************************************************************************
FORM f_montar_tela_itens.

  FREE: t_saida_item, t_saida_item_del.

  SELECT *
    FROM zi_mm_boleta_comp_it_info
    INTO TABLE @t_boleta_item
     FOR ALL ENTRIES IN @t_boleta
   WHERE guidhd = @t_boleta-guid.

  SELECT *
    FROM zi_mm_boleta_comp_it_info
    INTO TABLE @DATA(t_bol_item)
   WHERE guidhd = @w_boleta-guid.

  CHECK sy-subrc = 0.

  LOOP AT t_bol_item               INTO DATA(w_bol_item) WHERE guidhd = w_boleta-guid.
    CLEAR w_saida_item.
    MOVE-CORRESPONDING w_bol_item    TO w_saida_item.
    PERFORM f_edicao_itens        USING abap_false.
    APPEND w_saida_item              TO t_saida_item.
  ENDLOOP.

ENDFORM.

************************************************************************
* RETORNO ELABORACAO
************************************************************************
FORM f_retorno_elaboracao.

  FREE: tl300.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecionar uma linha!' DISPLAY LIKE 'W'.
    RETURN.
  ELSEIF lines( t_rows[] ) > 1.
    MESSAGE s024(sd) WITH 'Selecionar Somente uma linha!' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  READ TABLE t_rows     INTO w_rows      INDEX 1.
  READ TABLE t_saida    INTO w_saida     INDEX w_rows-index.
  READ TABLE t_boleta   INTO w_boleta     WITH KEY guid = w_saida-guid.

  IF w_boleta-statusworkflow = 'RE'.
    MESSAGE s024(sd) WITH 'Status Workflow ja esta em RE!' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL SCREEN 0300 STARTING AT 28  10
                     ENDING AT 155 13.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

ENDFORM.

************************************************************************
* validar motivo retorno
************************************************************************
FORM f_validar_motivo_retorno CHANGING p_erro.

  DATA: lv_motivo TYPE zmmt0194-mensagem.

  FREE p_erro.

  IF w_boleta-statusworkflow = 'RE'.
    MESSAGE s024(sd) WITH 'Status Workflow ja esta em RE!' DISPLAY LIKE 'E'.
    p_erro = abap_true.
    RETURN.
  ENDIF.

  lv_motivo = tl300-motivo1 && tl300-motivo2.

  IF strlen( lv_motivo ) < 15.
    MESSAGE s024(sd) WITH 'Motivo do Retorno deve ter no mínimo 15 caracteres!!' DISPLAY LIKE 'E'.
    p_erro = abap_true.
    RETURN.
  ENDIF.

ENDFORM.

************************************************************************
* salvar motivo retorno
************************************************************************
FORM f_salvar_motivo_retorno.

  DATA: lv_id_workflow TYPE zmmt0194-id_workflow,
        lv_timestampl  TYPE timestampl.

  GET TIME STAMP FIELD lv_timestampl.

  SORT t_saida_wf BY id_workflow  DESCENDING.
  READ TABLE t_saida_wf           INTO w_saida_wf INDEX 1.
  lv_id_workflow = w_saida_wf-id_workflow + 1..

*----------------------
* ajusta worlflow
*----------------------
  CLEAR w_zmmt0194.

  TRY.
      w_zmmt0194-guid         = cl_system_uuid=>create_uuid_x16_static( ).
    CATCH cx_uuid_error.
  ENDTRY.

  w_zmmt0194-guid_hd          = w_boleta-guid.
  w_zmmt0194-id_boleta_compra = w_boleta-idboletacompra.
  w_zmmt0194-id_workflow      = lv_id_workflow.
  w_zmmt0194-mensagem         = tl300-motivo1 && tl300-motivo2.
  w_zmmt0194-tp_usuario       = 'AP'.
  w_zmmt0194-created_by       = sy-uname.
  w_zmmt0194-created_at       = lv_timestampl.

  MODIFY zmmt0194          FROM w_zmmt0194.

*----------------------
* status geader
*----------------------
  UPDATE zmmt0191 SET status_workflow = 'RE'
                WHERE guid            = w_boleta-guid.

  MESSAGE s024(sd) WITH 'Motivo Retorno gravado com Sucesso!'.

  COMMIT WORK AND WAIT.

ENDFORM.

************************************************************************
* configura linha itens
************************************************************************
FORM f_edicao_itens USING p_edita.

  DATA: lv_style TYPE raw4.

  FREE: w_saida_item-style.

  CASE p_edita.
    WHEN abap_false.
      lv_style = cl_gui_alv_grid=>mc_style_disabled.
    WHEN abap_true.
      lv_style = cl_gui_alv_grid=>mc_style_enabled.
  ENDCASE.

  PERFORM f_config_cell USING 'MATNR'         lv_style.
  PERFORM f_config_cell USING 'MAKTX'         cl_gui_alv_grid=>mc_style_disabled.
  PERFORM f_config_cell USING 'MENGE'         lv_style.
  PERFORM f_config_cell USING 'MEINS'         cl_gui_alv_grid=>mc_style_disabled.
  PERFORM f_config_cell USING 'NETWR'         cl_gui_alv_grid=>mc_style_disabled.
  PERFORM f_config_cell USING 'WAERS'         lv_style.
  PERFORM f_config_cell USING 'NETPR'         lv_style.
  PERFORM f_config_cell USING 'LOCALEMBARQUE' lv_style.
  PERFORM f_config_cell USING 'WERKS'         lv_style.
  PERFORM f_config_cell USING 'NROSOLCP'      cl_gui_alv_grid=>mc_style_disabled.

ENDFORM.

************************************************************************
* criar solicitacao
************************************************************************
FORM f_criar_solicitacao  USING p_modo.

  DATA: lc_itens TYPE zmme0251.

  FREE: lc_call.

  CASE p_modo.
    WHEN 'HEADER'.
      CALL METHOD g_grid->get_selected_rows
        IMPORTING
          et_index_rows = t_rows.

      IF t_rows[] IS INITIAL.
        MESSAGE s024(sd) WITH 'Selecionar uma linha!' DISPLAY LIKE 'W'.
        RETURN.
      ELSEIF lines( t_rows[] ) > 1.
        MESSAGE s024(sd) WITH 'Selecionar Somente uma linha!' DISPLAY LIKE 'W'.
        RETURN.
      ENDIF.

      READ TABLE t_rows        INTO w_rows         INDEX 1.
      READ TABLE t_saida       INTO w_saida        INDEX w_rows-index.
      READ TABLE t_boleta      INTO w_boleta       WITH KEY guid = w_saida-guid.

      READ TABLE t_boleta_item INTO w_boleta_item  WITH KEY guidhd = w_boleta-guid.
      IF sy-subrc <> 0.
        MESSAGE s024(sd) WITH 'Boleta não contém Itens!' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      LOOP AT t_boleta_item INTO DATA(w_item) WHERE guidhd = w_boleta-guid.
        IF w_item-werks <> w_boleta_item-werks.
          MESSAGE s024(sd) WITH 'Boleta contém Itens com ' 'Centros Diferentes!' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
        IF w_boleta_item-nrosolcp IS NOT INITIAL.
          MESSAGE s024(sd) WITH 'Solicitação já Criada' 'para esta Boleta!' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      ENDLOOP.

*---------------------------------
*---- montar estrutura itens
*---------------------------------
      LOOP AT t_boleta_item INTO w_boleta_item  WHERE guidhd = w_boleta-guid.
        lc_itens-guid_hd       = w_boleta_item-guidhd.
        lc_itens-guid          = w_boleta_item-guid.
        lc_itens-matnr         = w_boleta_item-matnr.
        lc_itens-maktx         = w_boleta_item-maktx.
        lc_itens-menge         = w_boleta_item-menge.
        lc_itens-meins         = w_boleta_item-meins.
        lc_itens-waers         = w_boleta_item-waers.
        lc_itens-netpr         = w_boleta_item-netpr.
        lc_itens-netwr         = w_boleta_item-netwr.
        lc_itens-werks         = w_boleta_item-werks.
        APPEND lc_itens       TO lc_call-itens.
      ENDLOOP.

    WHEN 'ITEM'.
      CALL METHOD g_grid_item->get_selected_rows
        IMPORTING
          et_index_rows = t_rows.

      IF t_rows[] IS INITIAL.
        MESSAGE s024(sd) WITH 'Selecionar uma linha!' DISPLAY LIKE 'W'.
        RETURN.
      ELSEIF lines( t_rows[] ) > 1.
        MESSAGE s024(sd) WITH 'Selecionar Somente uma linha!' DISPLAY LIKE 'W'.
        RETURN.
      ENDIF.

      READ TABLE t_rows        INTO w_rows         INDEX 1.
      READ TABLE t_saida_item  INTO w_saida_item   INDEX w_rows-index.
      READ TABLE t_boleta      INTO w_boleta       WITH KEY guid = w_saida_item-guid.

*---------------------------------
*---- montar estrutura itens
*---------------------------------
      LOOP AT t_boleta_item INTO w_boleta_item  WHERE guidhd = w_boleta-guid
                                                  AND werks  = w_saida_item-werks.
        IF w_boleta_item-nrosolcp IS NOT INITIAL.
          MESSAGE s024(sd) WITH 'Solicitação já Criada' 'para esta Boleta!' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        lc_itens-guid_hd       = w_boleta_item-guidhd.
        lc_itens-guid          = w_boleta_item-guid.
        lc_itens-matnr         = w_boleta_item-matnr.
        lc_itens-maktx         = w_boleta_item-maktx.
        lc_itens-menge         = w_boleta_item-menge.
        lc_itens-meins         = w_boleta_item-meins.
        lc_itens-waers         = w_boleta_item-waers.
        lc_itens-netpr         = w_boleta_item-netpr.
        lc_itens-netwr         = w_boleta_item-netwr.
        lc_itens-werks         = w_boleta_item-werks.
        APPEND lc_itens       TO lc_call-itens.
      ENDLOOP.
  ENDCASE.

*---------------------------------
*-montar estrutura header
*---------------------------------
  lc_call-guid                = w_boleta-guid.
  lc_call-id_boleta_compra    = w_boleta-idboletacompra.
  lc_call-safra               = w_boleta-safra.

  CASE w_boleta-tipoinsumo.
    WHEN 'FT'.   "Fertilizantes
      lc_call-bsart   = 'ZFTE'.
    WHEN 'SM'.   "Sementes
      lc_call-bsart   = 'ZSEM'.
    WHEN 'DF'.   "Defensivos
      lc_call-bsart   = 'ZDEF'.
  ENDCASE.

  lc_call-tipo_insumo = w_boleta-tipoinsumo.
  lc_call-ekgrp       = 'I01'.
  lc_call-werks       = w_boleta_item-werks.
  lc_call-lifnr       = abap_off.
  lc_call-ped_forn    = abap_off.

  CASE w_boleta-condicaonegocio.
    WHEN 'PM'.   "`Permuta
      lc_call-zterm   = 'I006'.
    WHEN 'BN'.   "`Bonificacao
      lc_call-zterm   = 'I007'.
    WHEN OTHERS.
      lc_call-zterm   = abap_off.
  ENDCASE.

  lc_call-ihran       = w_boleta-datavencimento.
  lc_call-inco1       = w_boleta-inco1.
  lc_call-waers       = w_boleta-waers.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lc_call-werks
    IMPORTING
      output = lc_call-werks.

  EXPORT lc_call TO MEMORY ID 'ZMMR149_BOLETA'.

*---------------------------------
*-executar ZMM0149 - botao criar
*---------------------------------
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...executando ZMM0149'.

  PERFORM f_insere_bdc USING: 'X' 'ZMMR149'     '0100',
                              ' ' 'BDC_CURSOR'  'WG_CADLAN-NRO_SOL_CP',
                              ' ' 'BDC_OKCODE'  '=ADD'.

  CALL TRANSACTION 'ZMM0149' USING t_bdc MODE 'E'.

  FREE MEMORY ID 'ZMMR149_BOLETA'.

*---------------------------------
*-Atalizar grid itens
*---------------------------------
  PERFORM f_montar_tela_itens.

  IF g_grid_item IS NOT INITIAL.
    CALL METHOD g_grid_item->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

************************************************************************
* montar BDC
************************************************************************
FORM f_insere_bdc USING p_dynbegin TYPE any
                        p_field    TYPE any
                        p_value    TYPE any.

  DATA: sl_bdc TYPE bdcdata.

  CLEAR sl_bdc.

  IF p_dynbegin = 'X'.
    sl_bdc-dynbegin = 'X'.
    sl_bdc-program  = p_field.
    sl_bdc-dynpro   = p_value.
  ELSE.
    sl_bdc-fnam     = p_field.
    sl_bdc-fval     = p_value.
  ENDIF.

  APPEND sl_bdc    TO t_bdc.

ENDFORM.                    " Z_INSERE_BDC
************************************************************************
* configura celulas para edicao
************************************************************************
FORM f_config_cell  USING p_fieldname TYPE lvc_s_styl-fieldname
                          p_style     TYPE lvc_s_styl-style.

  CLEAR: w_style.
  w_style-fieldname = p_fieldname.
  w_style-style     = p_style.
  INSERT w_style INTO TABLE w_saida_item-style .

ENDFORM.

************************************************************************
* ALV
************************************************************************
FORM f_init_alv.

* PERFORM f_funcoes.
  PERFORM f_fieldcatalog USING 'CABECALHO'.
  PERFORM f_sort.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
* w_layout-no_rowmark   = abap_true.
  w_layout-zebra        = abap_true.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_false.
  w_layout-no_totexp    = abap_false.
  w_layout-no_totline   = abap_false.
  w_layout-no_toolbar   = abap_false.
* w_layout-stylefname   = 'CELLSTYLES'.
* w_layout-ctab_fname   = 'CELLCOLOR'.
  w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-no_rowmark   = abap_false.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  IF g_grid IS INITIAL. " AND  g_custom_container IS NOT INITIAL.
    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = 'X'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'HEADER'.

    PERFORM f_alv_header .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = g_custom_container
      EXCEPTIONS
        html_display_error = 1.

    "Grafico 1
    CALL METHOD cl_gui_cfw=>flush.

    CREATE OBJECT: g_custom_container
       EXPORTING
         container_name = 'CC_IMG',
         picture
       EXPORTING
         parent = g_custom_container.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = cl_container_95.

    SET HANDLER: lcl_event_handler=>on_hotspot_click   FOR g_grid,
                 lcl_event_handler=>on_data_changed    FOR g_grid,
                 lcl_event_handler=>user_command       FOR g_grid,
                 lcl_event_handler=>toolbar            FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_saida[]
        it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

************************************************************************
* ALV PARCELAS
************************************************************************
FORM f_init_alv_parc.

  w_stable-row               = abap_true.
  w_stable-col               = abap_true.
*
* w_layout_PARC-no_rowmark   = abap_true.
  w_layout_parc-zebra        = abap_true.
  w_layout_parc-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout_parc-no_totarr    = abap_true.
  w_layout_parc-no_totexp    = abap_true.
  w_layout_parc-no_totline   = abap_true.
  w_layout_parc-no_toolbar   = abap_true.
* w_layout_PARC-stylefname   = 'CELLSTYLES'.
* w_layout_PARC-ctab_fname   = 'CELLCOLOR'.
  w_layout_parc-info_fname   = 'LINE_COLOR'.
  w_layout_parc-no_rowmark   = abap_true.

  IF g_grid_parc IS INITIAL.
    PERFORM f_fieldcatalog USING 'PARCELA'.

    CREATE OBJECT g_custom_container_parc
      EXPORTING
        container_name = 'CC_PAGAMENTO'.

    CREATE OBJECT g_grid_parc
      EXPORTING
        i_parent          = g_custom_container_parc
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid_parc,
                 lcl_event_handler=>on_data_changed  FOR g_grid_parc,
                 lcl_event_handler=>user_command     FOR g_grid_parc,
                 lcl_event_handler=>toolbar          FOR g_grid_parc.

    CALL METHOD g_grid_parc->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout_parc
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_saida_parc
*       it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD g_grid_parc->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

************************************************************************
* ALV workflow
************************************************************************
FORM f_init_alv_wf.

  w_stable-row               = abap_true.
  w_stable-col               = abap_true.
*
* w_layout_wf-no_rowmark   = abap_true.
  w_layout_wf-zebra        = abap_true.
  w_layout_wf-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout_wf-no_totarr    = abap_true.
  w_layout_wf-no_totexp    = abap_true.
  w_layout_wf-no_totline   = abap_true.
  w_layout_wf-no_toolbar   = abap_true.
* w_layout_wf-stylefname   = 'CELLSTYLES'.
* w_layout_wf-ctab_fname   = 'CELLCOLOR'.
  w_layout_wf-info_fname   = 'LINE_COLOR'.
  w_layout_wf-no_rowmark   = abap_true.

  IF g_grid_wf IS INITIAL.
    PERFORM f_fieldcatalog USING 'WORKFLOW'.

    CREATE OBJECT g_custom_container_wf
      EXPORTING
        container_name = 'CC_WORKFLOW'.

    CREATE OBJECT g_grid_wf
      EXPORTING
        i_parent          = g_custom_container_wf
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid_wf,
                 lcl_event_handler=>on_data_changed  FOR g_grid_wf,
                 lcl_event_handler=>user_command     FOR g_grid_wf,
                 lcl_event_handler=>toolbar          FOR g_grid_wf.

    CALL METHOD g_grid_wf->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout_wf
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_saida_wf
*       it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD g_grid_wf->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

************************************************************************
* ALV ITENS
************************************************************************
FORM f_init_alv_item.

  w_stable-row               = abap_true.
  w_stable-col               = abap_true.
*
* w_layout_wf-no_rowmark     = abap_true.
  w_layout_item-zebra        = abap_false.
  w_layout_item-sel_mode     = 'A'.
  w_layout_item-edit         = lv_editar.
  w_layout_item-no_totarr    = abap_true.
  w_layout_item-no_totexp    = abap_true.
  w_layout_item-no_totline   = abap_true.
  w_layout_item-no_toolbar   = COND #( WHEN lv_editar = abap_true THEN abap_false ELSE abap_true ).
  w_layout_item-stylefname   = 'STYLE'.
* w_layout_item-ctab_fname   = 'CELLCOLOR'.
* w_layout_item-info_fname   = 'LINE_COLOR'.
  w_layout_item-no_rowmark   = COND #( WHEN lv_editar = abap_true THEN abap_false ELSE abap_true ).

  IF g_grid_item IS INITIAL.
    PERFORM f_fieldcatalog USING 'ITEM'.

    CREATE OBJECT g_custom_container_item
      EXPORTING
        container_name = 'CC_ITENS'.

    CREATE OBJECT g_grid_item
      EXPORTING
        i_parent          = g_custom_container_item
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CALL METHOD g_grid_item->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_item->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid_item,
                 lcl_event_handler=>on_data_changed  FOR g_grid_item,
                 lcl_event_handler=>user_command     FOR g_grid_item,
                 lcl_event_handler=>toolbar_item     FOR g_grid_item.

    CALL METHOD g_grid_item->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout_item
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_saida_item
*       it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD g_grid_item->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

**********************************************************************
* exibir anexos
**********************************************************************
FORM f_exibir_anexos.

  CLEAR lc_object.
  lc_ip_mode        = 'E'.
  lc_object-objtype = 'ZMMBOLCOMP'.
  lc_object-objkey  = tl001-id_boleta_compra.

  CREATE OBJECT g_manager.

  g_manager->set_rw_mode( ip_mode = 'D' ).
  g_manager->start_service_direct(
    EXPORTING
      ip_service       = 'VIEW_ATTA'
      is_object        = lc_object
    EXCEPTIONS
      no_object        = 1
      object_invalid   = 2
      execution_failed = 3
      OTHERS           = 4 ).

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Boleta não contém Anexos!' DISPLAY LIKE 'W'.
  ENDIF.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_sort.

  FREE: t_sort.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog USING p_tipo.

  FREE t_fieldcat[].

  CASE p_tipo.

    WHEN 'CABECALHO'.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_SAIDA'      'ID_BOLETA_COMPRA'  'Id.Boleta Compra'       '16'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        02  ''      ''       'T_SAIDA'      'DATA_ADIANTAMENTO' 'Data Solicitaçao'       '16'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        03  ''      ''       'T_SAIDA'      'FORNECEDOR'        'Fornecedor'             '25'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        04  ''      ''       'T_SAIDA'      'ROYALTIES_VALOR'   'Total'                  '16'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        05  ''      ''       'T_SAIDA'      'TIPO_INSUMO'       'Tipo Insumo'            '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        06  ''      ''       'T_SAIDA'      'SAFRA'             'Safra'                  '06'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  ''      ''       'T_SAIDA'      'WAERS'             'Moeda'                  '06'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        08  ''      ''       'T_SAIDA'      'CONDICAO_NEGOCIO'  'Cond.Negócio'           '13'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        09  ''      ''       'T_SAIDA'      'INCO1'             'Frete'                  '06'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        10  ''      ''       'T_SAIDA'      'ANEXOS'            'Anexos'                 '06'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' 'X' ' ',
        11  ''      ''       'T_SAIDA'      'ST_WORKFLOW'       'Status Workflow'        '35'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' 'X' ' ',
        12  ''      ''       'T_SAIDA'      'ST_SOLIC'          'Status Solicitação'     '35'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' 'X' ' ',
        13  ''      ''       'T_SAIDA'      'ST_PED'            'Status Pedido'          '35'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' 'X' ' ',
        14  ''      ''       'T_SAIDA'      'USER_SOLIC'        'Usuário Solicitante'    '20'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        15  ''      ''       'T_SAIDA'      'USER_RETORNO'      'Usuário Retorno Ajuste' '25'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        16  ''      ''       'T_SAIDA'      'USER_PEDIDO'       'Usuário Criador Pedido' '25'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        17  ''      ''       'T_SAIDA'      'QTD_ITEM'          'Qtde.Itens Boleta'      '18'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

    WHEN 'PARCELA'.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_SAIDA_PARC' 'DATA_VENCIMENTO'   'Data Vencimento'        '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        02  ''      ''       'T_SAIDA_PARC' 'WAERS'             'Moeda'                  '08'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        03  ''      ''       'T_SAIDA_PARC' 'VALOR'             'Valor'                  '20'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

    WHEN 'WORKFLOW'.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_SAIDA_WF'   'CREATED_BY'        'Usuário'                '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        02  ''      ''       'T_SAIDA_WF'   'MENSAGEM'          'Mensagem'               '25'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        03  ''      ''       'T_SAIDA_WF'   'TP_USUARIO'        'Tp.Usuário'             '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        04  ''      ''       'T_SAIDA_WF'   'DATA'              'Data'                   '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        05  ''      ''       'T_SAIDA_WF'   'HORA'              'Hora'                   '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

    WHEN 'ITEM'.
      PERFORM f_estrutura_alv USING:
        01  'MARA'  'MATNR'  'T_SAIDA_ITEM' 'MATNR'             'Cód.Material'           '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        02  'MAKT'  'MAKTX'  'T_SAIDA_ITEM' 'MAKTX'             'Descrição Material'     '30'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        03  'VBAP'  'ZMENG'  'T_SAIDA_ITEM' 'MENGE'             'Quantidade'             '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        04  'MARA'  'MEINS'  'T_SAIDA_ITEM' 'MEINS'             'U.M/Embalagem'          '13'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        05  'VBAP'  'WAERK'  'T_SAIDA_ITEM' 'WAERS'             'Moeda'                  '06'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        06  'VBAP'  'NETPR'  'T_SAIDA_ITEM' 'NETPR'             'Valor Unitário'         '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  'VBAP'  'NETWR'  'T_SAIDA_ITEM' 'NETWR'             'Total'                  '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        08  ''      ''       'T_SAIDA_ITEM' 'LOCALEMBARQUE'     'Local Embarque'         '25'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        09  'MARC'  'WERKS'  'T_SAIDA_ITEM' 'WERKS'             'Centro'                 '06'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        10  ''      ''       'T_SAIDA_ITEM' 'NROSOLCP'          'Nro.Solicitação'        '15'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' '.

  ENDCASE.

ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
* w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-coltext     = p_scrtext_l.
* w_fieldcat-scrtext_s   = p_scrtext_l.
* w_fieldcat-scrtext_m   = p_scrtext_l.
* w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
  w_fieldcat-no_out      = p_no_out.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_alv_header .

  DATA: wl_data1(10),
        wl_data2(10),
        wl_hora(8),
        wl_linha(60),
        wl_text TYPE sdydo_text_element.

  CALL METHOD obj_dyndoc_id->initialize_document.

  wl_linha = 'Cockpit Aprovação Boleta Compra - Comercial'.
  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>large
      sap_color    = cl_dd_area=>list_heading_int.

  CALL METHOD obj_dyndoc_id->new_line.

  IF s_dtsol[] IS NOT INITIAL.
    READ TABLE s_dtsol INDEX 1.

    wl_data1 = s_dtsol-low+6(2) && '.'  && s_dtsol-low+4(2)  && '.' && s_dtsol-low(4).
    wl_data2 = s_dtsol-high+6(2) && '.' && s_dtsol-high+4(2) && '.' && s_dtsol-high(4).

    IF s_dtsol-high IS NOT INITIAL.
      CONCATENATE  'Data da Solicitação...:' wl_data1 'a' wl_data2
             INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE  'Data da Solicitação...:' wl_data1
             INTO wl_linha SEPARATED BY space.
    ENDIF.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.

  IF s_status[] IS NOT INITIAL.
    READ TABLE s_status INDEX 1.

    IF s_status-high IS NOT INITIAL.
      CONCATENATE  'Status Solicitação.....:' s_status-low 'a' s_status-high
             INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE  'Status Solicitação.....:' s_status-low
             INTO wl_linha SEPARATED BY space.
    ENDIF.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.

ENDFORM.

**********************************************************************
*  imagen
**********************************************************************
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

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

************************************************************************
************************************************************************
