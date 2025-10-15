*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Tavares                                         &*
*& Data.....: 24/12/2013                                              &*
*& Descrição: Autorização Liberação Embarque - INSUMOS                &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

************************************************************************
* efetuar distribuicao
************************************************************************
FORM f_efetuar_distribuicao    USING p_reprocessar
                            CHANGING p_sair_tela.

  DATA: w_zsds093 TYPE zsds093,
        t_zsds094 TYPE zsds094_tt.

  FREE: gt_dados_alv2_dado.

  READ TABLE gt_dados_alv1        INTO DATA(_dados_alv1) INDEX 1.
  CHECK sy-subrc = 0.

  LOOP AT gt_dados_alv2           INTO w_dados_alv2.
    MOVE-CORRESPONDING w_dados_alv2 TO w_dados_alv2_dado.
    APPEND w_dados_alv2_dado        TO gt_dados_alv2_dado.
  ENDLOOP.

  w_zsds093   = _dados_alv1.
  t_zsds094[] = gt_dados_alv2_dado[].

* DELETE t_zsds094 WHERE kwmeng IS INITIAL. "*-US191316-22.09.2025-#191316-JT-inicio

*-------------------------------------
* processamento distribuicao
*-------------------------------------
  CALL FUNCTION 'ZSD_EVENTOS_DISTRIBUICAO'
    EXPORTING
      i_zsds093       = w_zsds093
      i_reprocessar   = p_reprocessar
    IMPORTING
      e_abandona_tela = p_sair_tela
    TABLES
      t_zsds094       = t_zsds094
    EXCEPTIONS
      erro_validacao  = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

************************************************************************
* visualizar historico
************************************************************************
FORM f_visualizar_historico CHANGING p_sair_tela.

  DATA: w_zsds093 TYPE zsds093,
        t_zsds094 TYPE zsds094_tt.

  READ TABLE gt_dados_alv1 INTO DATA(_dados_alv1) INDEX 1.
  CHECK sy-subrc = 0.

  w_zsds093   = _dados_alv1.

*-------------------------------------
* processamento distribuicao
*-------------------------------------
  CALL FUNCTION 'ZSD_EVENTOS_DISTRIBUICAO'
    EXPORTING
      i_zsds093       = w_zsds093
      i_visualizar    = abap_true
    IMPORTING
      e_abandona_tela = p_sair_tela
    EXCEPTIONS
      erro_validacao  = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

************************************************************************
* criar itinerario
************************************************************************
FORM f_criar_itinerario USING p_dados_alv2 TYPE ty_dados_alv2.

  DATA: lv_route   TYPE vbap-route,
        lv_werks   TYPE werks_d,
        lv_lifnr   TYPE vbpa-lifnr,
        w_zsdt0132 TYPE zsdt0132.

  CLEAR: lv_route, lv_lifnr.

  READ TABLE gt_dados_alv1 INTO DATA(_dados_alv1) INDEX 1.
  CHECK sy-subrc = 0.

  IF p_dados_alv2-ebeln IS NOT INITIAL.
    SELECT SINGLE lifnr
      INTO lv_lifnr
      FROM ekko
     WHERE ebeln = p_dados_alv2-ebeln.
  ENDIF.

  IF lv_lifnr IS INITIAL.
    IF p_dados_alv2-lifnr IS NOT INITIAL.
      lv_lifnr = p_dados_alv2-lifnr.
    ENDIF.
  ENDIF.

  IF lv_lifnr IS INITIAL.
    lv_werks = p_dados_alv2-werks.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_werks
      IMPORTING
        output = lv_lifnr.
  ENDIF.

  SELECT SINGLE route
    INTO lv_route
    FROM vbap
   WHERE vbeln = _dados_alv1-vbeln
     AND posnr = _dados_alv1-posnr.

  SELECT SINGLE *
    FROM zsdt0132
    INTO w_zsdt0132
   WHERE nr_rot = _dados_alv1-nr_rot.

  IF _dados_alv1-nr_rot IS INITIAL OR w_zsdt0132-lzone IS INITIAL.
    MESSAGE s836(sd) WITH 'Cadastrar Roteiro/Zona Transp. p/criar itinerario.' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    INTO @DATA(_zlest0153)
    FROM zlest0153
  WHERE lifnr  = @lv_lifnr
    AND nr_rot = @p_dados_alv2-nr_rot_pc.

  IF sy-subrc = 0.
    SET PARAMETER ID 'ZONA_ORI' FIELD _zlest0153-lzone.
  ENDIF.

*-US191669-24.09.2025-#191669-JT-inicio
  SELECT SINGLE nr_rot, lifnr_rot, lzone
    INTO @DATA(_0312)
    FROM zsdt0132
   WHERE nr_rot = @p_dados_alv2-nr_rot_pc.

  IF sy-subrc = 0 AND _0312-lifnr_rot IS NOT INITIAL.
    lv_lifnr = _0312-lifnr_rot.
    SET PARAMETER ID 'ZONA_ORI' FIELD _0312-lzone.
  ENDIF.
*-US191669-24.09.2025-#191669-JT-fim

  SET PARAMETER   ID 'ZONA_DES' FIELD w_zsdt0132-lzone.
  SET PARAMETER   ID 'COD_CLI'  FIELD w_zsdt0132-kunnr.
  SET PARAMETER   ID 'COD_PC'   FIELD lv_lifnr.

  SUBMIT zlesr0162 WITH p_kunnr = w_zsdt0132-kunnr
                   WITH p_lifnr = lv_lifnr
                   WITH p_lzone = w_zsdt0132-lzone
               AND RETURN.

ENDFORM.

************************************************************************
* enviar contatos safra
************************************************************************
FORM f_enviar_safra.

  DATA: t_sel_rows  TYPE lvc_t_row,
        w_sel_rows  TYPE lvc_s_row,
        t_selection TYPE TABLE OF rsparams,
        w_selection TYPE rsparams,
        l_jobname   TYPE tbtcjob-jobname,
        l_name      TYPE tbtcjob-jobname,
        l_number    TYPE tbtcjob-jobcount,
        ref1        TYPE REF TO cl_gui_alv_grid.

  FREE: t_selection.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref1.

  CHECK ref1 IS NOT INITIAL.

  CALL METHOD ref1->get_selected_rows
    IMPORTING
      et_index_rows = t_sel_rows.

  DELETE t_sel_rows WHERE index IS INITIAL.

  IF t_sel_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione uma linha!'.
    RETURN.
  ENDIF.

  LOOP AT t_sel_rows INTO w_sel_rows.
    READ TABLE t_saida INTO wa_saida INDEX w_sel_rows-index.

    wa_saida-integrou_werks     = icon_dummy.
    wa_saida-integrou_vkbur     = icon_dummy.
    wa_saida-integrou_kunnr     = icon_dummy.
    wa_saida-integrou_nr_rot    = icon_dummy.
    wa_saida-integrou_nr_rot_pc = icon_dummy.
    MODIFY t_saida           FROM wa_saida     INDEX w_sel_rows-index.

    CLEAR w_selection.
    w_selection-selname = 'S_NROSOL'.
    w_selection-kind    = 'S'.
    w_selection-sign    = 'I'.
    w_selection-option  = 'EQ'.
    w_selection-low     = wa_saida-nro_sol.
    APPEND w_selection TO t_selection.
  ENDLOOP.

  CLEAR w_selection.
  w_selection-selname   = 'P_DATA'.
  w_selection-kind      = 'P'.
  w_selection-sign      = 'I'.
  w_selection-option    = 'EQ'.
  w_selection-low       = sy-datum.
  APPEND w_selection   TO t_selection.

  CLEAR w_selection.
  w_selection-selname   = 'P_DIAS'.
  w_selection-kind      = 'P'.
  w_selection-sign      = 'I'.
  w_selection-option    = 'EQ'.
  w_selection-low       = '999'.
  APPEND w_selection   TO t_selection.

  CLEAR w_selection.
  w_selection-selname   = 'P_CONTAC'.
  w_selection-kind      = 'P'.
  w_selection-sign      = 'I'.
  w_selection-option    = 'EQ'.
  w_selection-low       = abap_true.
  APPEND w_selection   TO t_selection.

  CLEAR w_selection.
  w_selection-selname   = 'P_NAOJOB'.
  w_selection-kind      = 'P'.
  w_selection-sign      = 'I'.
  w_selection-option    = 'EQ'.
  w_selection-low       = abap_true.
  APPEND w_selection   TO t_selection.

*-DEBUG ----------
  IF 1 = 2.
    SUBMIT zsdr0232_job WITH SELECTION-TABLE t_selection
                                  AND RETURN.
  ENDIF.

  MESSAGE s024(sd) WITH 'JOB em Execução. Atualize a tela' ' para verificar resultado!'.

*------------------------------------
*-criar job execucao
*------------------------------------
  l_jobname = |ZSDT0081|.
  l_name    = l_jobname && '_' && sy-datum && '_' && sy-uzeit.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = l_name
    IMPORTING
      jobcount         = l_number
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Não foi possivel criar JOB!' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  SUBMIT zsdr0232_job WITH SELECTION-TABLE t_selection
                                   VIA JOB l_name
                                    NUMBER l_number
                                AND RETURN.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = l_number
      jobname              = l_name
      strtimmed            = abap_true
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      OTHERS               = 8.

ENDFORM.

************************************************************************
* tratamento locks defensivos
************************************************************************
FORM f_tratar_defensivos USING p_mesmo_centro.

  DATA: lv_erro            TYPE char01,
        t_dados_alv2_werks TYPE TABLE OF ty_dados_alv2,
        lv_centros_lock    TYPE string,
        lv_user_lock       TYPE string,
        lv_mesg1           TYPE string,
        lv_mesg2           TYPE string,
        lv_mesg3           TYPE string.

  FREE: lv_centros_lock.

  g_mesmo_centro = p_mesmo_centro.

  READ TABLE gt_dados_alv1 INTO DATA(_dados_alv1) INDEX 1.

  IF p_mesmo_centro = abap_true.
    DELETE gt_dados_alv2 WHERE werks <> _dados_alv1-werks.
  ELSE.
    DELETE gt_dados_alv2 WHERE werks  = _dados_alv1-werks.
  ENDIF.

  t_dados_alv2_werks[] = gt_dados_alv2[].

  SORT t_dados_alv2_werks BY werks.
  DELETE ADJACENT DUPLICATES FROM t_dados_alv2_werks COMPARING werks.

  LOOP AT t_dados_alv2_werks INTO DATA(_dados_alv2).
    PERFORM f_lock_defensivo  USING _dados_alv2-werks _dados_alv2-matnr
                           CHANGING lv_erro
                                    lv_user_lock.
    IF lv_erro = abap_true.
      lv_centros_lock = lv_centros_lock && | { _dados_alv2-werks } |.
      PERFORM f_fechar_campos USING _dados_alv2-werks.
    ELSE.
      MOVE _dados_alv2-matnr     TO w_centros_defens-matnr.
      MOVE _dados_alv2-werks     TO w_centros_defens-werks.
      APPEND w_centros_defens    TO t_centros_defens.
    ENDIF.
  ENDLOOP.

  IF lv_centros_lock IS NOT INITIAL.
    lv_mesg1 = ' Centros: '   && | { lv_centros_lock } | .
    lv_mesg2 = 'Princ.Ativo:' && | { lv_obj_bloq ALPHA = OUT } | && | { 'usuario:' } |.
    lv_mesg3 = 'Usuário:'     && | { lv_user_lock } |.
    MESSAGE s024(sd) WITH 'Distribuições Bloqueadas:' lv_mesg1 lv_mesg2 lv_mesg3.
  ENDIF.

ENDFORM.

************************************************************************
* fechar campos para edicao
************************************************************************
FORM f_fechar_campos USING p_werks.

  LOOP AT gt_dados_alv2  INTO DATA(_dados_alv2) WHERE werks = p_werks.
    DATA(lv_tabix) = sy-tabix.

    FREE: t_style.
    wa_style-fieldname      = 'KWMENG'.
    wa_style-style          = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wa_style        TO t_style.
    wa_style-fieldname      = 'DT_ENTREGA'.
    wa_style-style          = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wa_style        TO t_style.
    wa_style-fieldname      = 'NR_ROT_PC'.
    wa_style-style          = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wa_style        TO t_style.
    wa_style-fieldname      = 'PRIORIDADE'.
    wa_style-style          = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wa_style        TO t_style.
    wa_style-fieldname      = 'FLEXIBILIDADE'.
    wa_style-style          = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wa_style        TO t_style.
    wa_style-fieldname      = 'CARGA_AUTO'.
    wa_style-style          = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wa_style        TO t_style.
    wa_style-fieldname      = 'TRANSF_NO_FORNECEDOR'.
    wa_style-style          = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wa_style        TO t_style.

    _dados_alv2-style[]     = t_style[].
    MODIFY gt_dados_alv2 FROM _dados_alv2 INDEX lv_tabix.
  ENDLOOP.

ENDFORM.

************************************************************************
* selecionar outros centros - defensivos
************************************************************************
FORM f_selecionar_centros USING p_mesmo_centro.

  PERFORM f_debloqueio_defensivos.
  PERFORM f_dados_alv2_108    USING abap_off.
  PERFORM f_tratar_defensivos USING p_mesmo_centro.

ENDFORM.

************************************************************************
* selecionar outros centros - defensivos
************************************************************************
FORM f_exibir_lotes USING p_vencidos.

  READ TABLE gt_dados_alv1 INTO DATA(_dados_alv1) INDEX 1.

  IF _dados_alv1-spart = '04'.  "sementes
    PERFORM f_dados_alv2_108 USING p_vencidos.
  ELSE.
    PERFORM f_debloqueio_defensivos.
    PERFORM f_dados_alv2_108    USING p_vencidos.
    PERFORM f_tratar_defensivos USING g_mesmo_centro.
  ENDIF.

ENDFORM.

************************************************************************
************************************************************************
