*&--------------------------------------------------------------------------------&*
*&                        AMAGGI                                                  &*
*&--------------------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                              &*
*& Autor....: Jaime Tassoni                                                       &*
*& Data.....: 19.02.2024                                                          &*
*& Descrição: Integracao CONTATOS com sistema SAFRA                               &*
*&--------------------------------------------------------------------------------&*
REPORT zsdr0232_job.

TABLES: zsdt0082.

************************************************************************
* variaveis globais
************************************************************************
CONSTANTS: c_nfe     TYPE char30 VALUE 'NFE',
           c_aut_emb TYPE char30 VALUE 'AUT_EMB'.


DATA: t_zsdt0082        TYPE TABLE OF zsdt0082,
      t_zsdt0133        TYPE TABLE OF zsdt0133,
      t_zsdt0133_transp TYPE TABLE OF zsdt0133,
      t_zsdt0133_anexos TYPE TABLE OF zsdt0133,
      t_zsdt0129_veicul TYPE TABLE OF zsdt0129,
      t_zsdt0129        TYPE TABLE OF zsdt0129,
      t_vbak            TYPE TABLE OF vbak,
      w_vbak            TYPE vbak,
      w_zsdt0082        TYPE zsdt0082,
      w_zsdt0129        TYPE zsdt0129,
      w_zsdt0133        TYPE zsdt0133,
      w_zsdt0133_transp TYPE zsdt0133,
      lv_lifnr          TYPE lifnr,
      lv_integrado      TYPE char01,
      lv_integrar       TYPE char01,
      lv_referencia     TYPE char50,
      lv_quant_werks    TYPE i,
      lv_quant_vkbur    TYPE i,
      lv_quant_kunnr    TYPE i,
      lv_quant_trans    TYPE i,
      lv_quant_motor    TYPE i,
      w_zsdt0422        TYPE zsdt0422,
      t_status          TYPE zde_btcstatus_t,
      lc_integra_safra  TYPE REF TO zcl_int_ob_safra_crt_contact.

************************************************************************
* ranges
************************************************************************
RANGES: r_data    FOR sy-datum.

************************************************************************
* parametros entrada
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS : s_nrosol  FOR zsdt0082-nro_sol.  "*-CS2025000249-01.09.2025-#189440-JT
  PARAMETERS     : p_data   TYPE datum  OBLIGATORY DEFAULT sy-datum,
                   p_dias   TYPE numc3  OBLIGATORY DEFAULT 7,
                   p_contac AS CHECKBOX,
                   p_tracki AS CHECKBOX,
                   p_veicul AS CHECKBOX,
                   p_naojob TYPE char01 NO-DISPLAY. "*-CS2025000249-01.09.2025-#189440-JT
SELECTION-SCREEN END   OF BLOCK b1.

************************************************************************
*  start
************************************************************************
START-OF-SELECTION.

  FREE: t_status.

  APPEND 'R' TO t_status.

*---------------------------------------------
* se tem Job ativo, abandona
*---------------------------------------------
  IF p_naojob = abap_false.  "*-CS2025000249-01.09.2025-#189440-JT
    TRY .
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = sy-cprog    " Nome de um programa em uma etapa (p.ex. report)
            i_sdldate    = sy-datum    " Data de escalonamento de job ou etapa
            i_status     = t_status    " Status de Jobs
          IMPORTING
            e_quantidade = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd > 1.
      EXIT.
    ENDIF.
  ENDIF.

*-selecao -------------------------------
  PERFORM f_selecao_dados.

*-processamento -------------------------
  PERFORM f_processamento_zsdt0082.
  PERFORM f_processamento_zsdt0133.
  PERFORM f_processamento_cargo_veicul.
  PERFORM f_processamento_cargo_transp.
  PERFORM f_processamento_cargo_anexos.
  PERFORM f_imprime_dados.

  MESSAGE s024(sd) WITH 'Processaento Concluído.'.

************************************************************************
* selecao dados
************************************************************************
FORM f_selecao_dados.

  DATA: lva_nr_rot_pc_null TYPE zsdt0082-nr_rot_pc.

  FREE: lv_quant_werks,
        lv_quant_vkbur,
        lv_quant_kunnr,
        lv_quant_trans,
        lv_quant_motor.

  CREATE OBJECT lc_integra_safra.

  FREE: r_data.
  r_data-sign   = 'I'.
  r_data-option = 'BT'.
  r_data-low    = p_data - p_dias.
  r_data-high   = p_data.
  APPEND r_data.

*-------------------------
* selecao
*-------------------------
  SELECT *
    FROM zsdt0082
    INTO TABLE @t_zsdt0082
   WHERE   nro_sol           IN @s_nrosol   "*-CS2025000249-01.09.2025-#189440-JT
     AND   dt_sol            IN @r_data
     AND   seq                = 001
     AND ( integrou_werks     = @abap_false
      OR   integrou_vkbur     = @abap_false
      OR   integrou_kunnr     = @abap_false
      OR   integrou_nr_rot    = @abap_false
      OR   ( nr_rot_pc NE @lva_nr_rot_pc_null AND integrou_nr_rot_pc = @abap_false )
          ).

  IF t_zsdt0082[] IS NOT INITIAL.
    SELECT vbeln, kunnr
      FROM vbak
      INTO CORRESPONDING FIELDS OF TABLE @t_vbak
       FOR ALL ENTRIES IN @t_zsdt0082
     WHERE vbeln = @t_zsdt0082-vbeln.
  ENDIF.

  SELECT *
    FROM zsdt0133
    INTO TABLE t_zsdt0133
   WHERE   data_atual           IN r_data
     AND ( int_transp_safra_ctrl = abap_false
      OR   int_mot_safra_ctrl    = abap_false ).

  DELETE t_zsdt0133 WHERE id_carga_safra_control IS INITIAL.

  IF t_zsdt0133[] IS NOT INITIAL.
    SELECT nro_lote, nro_cg, motorista
      FROM zsdt0129
      INTO CORRESPONDING FIELDS OF TABLE @t_zsdt0129
       FOR ALL ENTRIES IN @t_zsdt0133
     WHERE nro_cg = @t_zsdt0133-nro_cg.
  ENDIF.

  SELECT nro_cg, id_carga_safra_control
    FROM zsdt0133
   WHERE data_atual                 IN @r_data
     AND id_carga_safra_control     IS NOT INITIAL
     AND cod_transportadora         IS NOT INITIAL
     AND int_dados_transp_safra_ctrl = @abap_false
    INTO CORRESPONDING FIELDS OF TABLE @t_zsdt0133_transp.

  DELETE t_zsdt0133_transp WHERE id_carga_safra_control IS INITIAL.

  SELECT nro_cg, id_carga_safra_control
    FROM zsdt0133
   WHERE   data_atual                   IN @r_data
     AND   id_carga_safra_control       IS NOT INITIAL
     AND   dt_autorizacao_embarque      IS NOT INITIAL
     AND ( int_anexos_aut_emb_safra_ctrl = @abap_false
      OR   int_anexos_nfe_safra_ctrl     = @abap_false )
    INTO CORRESPONDING FIELDS OF TABLE @t_zsdt0133_anexos.

  DELETE t_zsdt0133_anexos WHERE id_carga_safra_control IS INITIAL.

  SELECT nro_lote, nro_cg, placa_cav
    FROM zsdt0129 AS a
   WHERE data_atual              IN @r_data
     AND placa_cav               IS NOT INITIAL
     AND int_veiculo_safra_ctrl   = @abap_false
     AND EXISTS ( SELECT nro_cg
                   FROM zsdt0133 AS b
                  WHERE b~nro_cg  = a~nro_cg
                    AND id_carga_safra_control IS NOT INITIAL )
    INTO CORRESPONDING FIELDS OF TABLE @t_zsdt0129_veicul.

ENDFORM.

************************************************************************
*  Processamento
************************************************************************
FORM f_processamento_zsdt0082.

  CHECK p_contac = abap_true.

  LOOP AT t_zsdt0082  INTO DATA(_zsdt0082).

    CLEAR w_vbak.
    READ TABLE t_vbak INTO w_vbak WITH KEY vbeln = _zsdt0082-vbeln.

*-----------------------------------------
*-- integrar WERKS - Fornecedor
*-----------------------------------------
    IF _zsdt0082-werks IS NOT INITIAL.
      lv_lifnr = |{ _zsdt0082-werks ALPHA = IN }|.

      PERFORM f_checa_integracao     USING lv_lifnr
                                           'C'
                                           0
                                  CHANGING lv_integrado
                                           lv_integrar.
      IF lv_integrar = abap_true.
        IF lv_integrado = abap_true.
          PERFORM f_atualizar_status   USING 'WERKS' 'C' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_true.
        ELSE.
          CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS'
            EXPORTING
              i_parceiro          = lv_lifnr
              i_tp_contato        = 'C'
              i_somente_cadastrar = abap_true
              i_outros_enderecos  = abap_false
            EXCEPTIONS
              erro_integracao     = 1
              OTHERS              = 2.

          IF sy-subrc = 0.
            lv_quant_werks = lv_quant_werks + 1.
            PERFORM f_atualizar_status USING 'WERKS' 'C' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_true.
          ELSE.
            PERFORM f_atualizar_status USING 'WERKS' 'C' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_false.
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM f_checa_integracao     USING lv_lifnr
                                           'F'
                                           0
                                  CHANGING lv_integrado
                                           lv_integrar.
      IF lv_integrar = abap_true.
        IF lv_integrado = abap_true.
          PERFORM f_atualizar_status   USING 'WERKS' 'F' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_true.
        ELSE.
          CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS'
            EXPORTING
              i_parceiro          = lv_lifnr
              i_tp_contato        = 'F'
              i_somente_cadastrar = abap_true
              i_outros_enderecos  = abap_false
            EXCEPTIONS
              erro_integracao     = 1
              OTHERS              = 2.

          IF sy-subrc = 0.
            lv_quant_werks = lv_quant_werks + 1.
            PERFORM f_atualizar_status USING 'WERKS' 'F' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_true.
          ELSE.
            PERFORM f_atualizar_status USING 'WERKS' 'F' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_false.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*-----------------------------------------
*-- integrar WERKS - Fornecedor
*-----------------------------------------
    IF _zsdt0082-vkbur IS NOT INITIAL.
      lv_lifnr = |{ _zsdt0082-vkbur ALPHA = IN }|.

      PERFORM f_checa_integracao     USING lv_lifnr
                                           'C'
                                           0
                                  CHANGING lv_integrado
                                           lv_integrar.
      IF lv_integrar = abap_true.
        IF lv_integrado = abap_true.
          PERFORM f_atualizar_status   USING 'VKBUR' 'C' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_true.
        ELSE.
          CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS'
            EXPORTING
              i_parceiro          = lv_lifnr
              i_tp_contato        = 'C'
              i_somente_cadastrar = abap_true
              i_outros_enderecos  = abap_false
            EXCEPTIONS
              erro_integracao     = 1
              OTHERS              = 2.

          IF sy-subrc = 0.
            lv_quant_vkbur = lv_quant_vkbur + 1.
            PERFORM f_atualizar_status USING 'VKBUR' 'C' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_true.
          ELSE.
            PERFORM f_atualizar_status USING 'VKBUR' 'C' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_false.
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM f_checa_integracao     USING lv_lifnr
                                           'F'
                                           0
                                  CHANGING lv_integrado
                                           lv_integrar.
      IF lv_integrar = abap_true.
        IF lv_integrado = abap_true.
          PERFORM f_atualizar_status   USING 'VKBUR' 'F' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_true.
        ELSE.
          CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS'
            EXPORTING
              i_parceiro          = lv_lifnr
              i_somente_cadastrar = abap_true
              i_tp_contato        = 'F'
              i_outros_enderecos  = abap_false
            EXCEPTIONS
              erro_integracao     = 1
              OTHERS              = 2.

          IF sy-subrc = 0.
            lv_quant_vkbur = lv_quant_vkbur + 1.
            PERFORM f_atualizar_status USING 'VKBUR' 'F' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_true.
          ELSE.
            PERFORM f_atualizar_status USING 'VKBUR' 'F' lv_lifnr 0 0 _zsdt0082 w_zsdt0133 abap_false.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*-----------------------------------------
*-- integrar KUNNR - OV Cliente
*-----------------------------------------
    IF w_vbak-kunnr IS NOT INITIAL.
      PERFORM f_checa_integracao     USING w_vbak-kunnr
                                           'C'
                                           0
                                  CHANGING lv_integrado
                                           lv_integrar.
      IF lv_integrar = abap_true.
        IF lv_integrado = abap_true.
          PERFORM f_atualizar_status   USING 'KUNNR' 'C' w_vbak-kunnr 0 0 _zsdt0082 w_zsdt0133 abap_true.
        ELSE.
          CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS'
            EXPORTING
              i_parceiro          = w_vbak-kunnr
              i_tp_contato        = 'C'
              i_somente_cadastrar = abap_true
              i_outros_enderecos  = abap_false
            EXCEPTIONS
              erro_integracao     = 1
              OTHERS              = 2.

          IF sy-subrc = 0.
            lv_quant_kunnr = lv_quant_kunnr + 1.
            PERFORM f_atualizar_status USING 'KUNNR' 'C' w_vbak-kunnr 0 0 _zsdt0082 w_zsdt0133 abap_true.
          ELSE.
            PERFORM f_atualizar_status USING 'KUNNR' 'C' w_vbak-kunnr 0 0 _zsdt0082 w_zsdt0133 abap_false.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*-----------------------------------------
*-- integrar NR_ROT - Roteiro
*-----------------------------------------
    IF _zsdt0082-nr_rot IS NOT INITIAL.
      PERFORM f_checa_integracao     USING abap_off
                                           abap_off
                                           _zsdt0082-nr_rot
                                  CHANGING lv_integrado
                                           lv_integrar.
      IF lv_integrar = abap_true.
        IF lv_integrado = abap_true.
          PERFORM f_atualizar_status   USING 'NR_ROT' '' abap_off 0 _zsdt0082-nr_rot _zsdt0082 w_zsdt0133 abap_true.
        ELSE.
          CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS'
            EXPORTING
              i_nr_rot            = _zsdt0082-nr_rot
              i_somente_cadastrar = abap_true
              i_outros_enderecos  = abap_false
            EXCEPTIONS
              erro_integracao     = 1
              OTHERS              = 2.

          IF sy-subrc = 0.
            PERFORM f_atualizar_status USING 'NR_ROT' '' abap_off 0 _zsdt0082-nr_rot _zsdt0082 w_zsdt0133 abap_true.
          ELSE.
            PERFORM f_atualizar_status USING 'NR_ROT' '' abap_off 0 _zsdt0082-nr_rot _zsdt0082 w_zsdt0133 abap_false.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*-----------------------------------------
*-- integrar NR_ROT_PC - Roteiro
*-----------------------------------------
    IF _zsdt0082-nr_rot_pc IS NOT INITIAL.
      PERFORM f_checa_integracao     USING abap_off
                                           abap_off
                                           _zsdt0082-nr_rot_pc
                                  CHANGING lv_integrado
                                           lv_integrar.
      IF lv_integrar = abap_true.
        IF lv_integrado = abap_true.
          PERFORM f_atualizar_status   USING 'NR_ROT_PC' '' abap_off 0 _zsdt0082-nr_rot_pc _zsdt0082 w_zsdt0133 abap_true.
        ELSE.
          CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS'
            EXPORTING
              i_nr_rot            = _zsdt0082-nr_rot_pc
              i_somente_cadastrar = abap_true
              i_outros_enderecos  = abap_false
            EXCEPTIONS
              erro_integracao     = 1
              OTHERS              = 2.

          IF sy-subrc = 0.
            PERFORM f_atualizar_status USING 'NR_ROT_PC' '' abap_off 0 _zsdt0082-nr_rot_pc _zsdt0082 w_zsdt0133 abap_true.
          ELSE.
            PERFORM f_atualizar_status USING 'NR_ROT_PC' '' abap_off 0 _zsdt0082-nr_rot_pc _zsdt0082 w_zsdt0133 abap_false.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.

************************************************************************
*  Processamento
************************************************************************
FORM f_processamento_zsdt0133.

  CHECK p_contac = abap_true AND p_naojob = abap_false.  "*-CS2025000249-01.09.2025-#189440-JT

  LOOP AT t_zsdt0133  INTO DATA(_zsdt0133).

    CLEAR w_zsdt0129.
    READ TABLE t_zsdt0129 INTO w_zsdt0129 WITH KEY nro_cg = _zsdt0133-nro_cg.

*-----------------------------------------
*-- integrar Transprtadora
*-----------------------------------------
    IF _zsdt0133-cod_transportadora IS NOT INITIAL.
      lv_lifnr = _zsdt0133-cod_transportadora.

      PERFORM f_checa_integracao     USING lv_lifnr
                                           'C'
                                           0
                                  CHANGING lv_integrado
                                           lv_integrar.
      IF lv_integrar = abap_true.
        IF lv_integrado = abap_true.
          PERFORM f_atualizar_status   USING 'COD_TRANSPORTADORA' 'C' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_true.
        ELSE.
          CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS'
            EXPORTING
              i_parceiro          = lv_lifnr
              i_tp_contato        = 'C'
              i_somente_cadastrar = abap_true
              i_outros_enderecos  = abap_false
            EXCEPTIONS
              erro_integracao     = 1
              OTHERS              = 2.

          IF sy-subrc = 0.
            lv_quant_trans = lv_quant_trans + 1.
            PERFORM f_atualizar_status   USING 'COD_TRANSPORTADORA' 'C' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_true.
          ELSE.
            PERFORM f_atualizar_status   USING 'COD_TRANSPORTADORA' 'C' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_false.
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM f_checa_integracao     USING lv_lifnr
                                           'F'
                                           0
                                  CHANGING lv_integrado
                                           lv_integrar.
      IF lv_integrar = abap_true.
        IF lv_integrado = abap_true.
          PERFORM f_atualizar_status   USING 'COD_TRANSPORTADORA' 'F' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_true.
        ELSE.
          CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS'
            EXPORTING
              i_parceiro          = lv_lifnr
              i_tp_contato        = 'F'
              i_somente_cadastrar = abap_true
              i_outros_enderecos  = abap_false
            EXCEPTIONS
              erro_integracao     = 1
              OTHERS              = 2.

          IF sy-subrc = 0.
            lv_quant_trans = lv_quant_trans + 1.
            PERFORM f_atualizar_status   USING 'COD_TRANSPORTADORA' 'F' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_true.
          ELSE.
            PERFORM f_atualizar_status   USING 'COD_TRANSPORTADORA' 'F' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_false.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*-----------------------------------------
*-- integrar Motorista
*-----------------------------------------
    IF w_zsdt0129-motorista IS NOT INITIAL.
      lv_lifnr = w_zsdt0129-motorista.

      PERFORM f_checa_integracao     USING lv_lifnr
                                           'C'
                                           0
                                  CHANGING lv_integrado
                                           lv_integrar.
      IF lv_integrar = abap_true.
        IF lv_integrado = abap_true.
          PERFORM f_atualizar_status   USING 'MOTORISTA' 'C' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_true.
        ELSE.
          CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS'
            EXPORTING
              i_parceiro          = lv_lifnr
              i_tp_contato        = 'C'
              i_somente_cadastrar = abap_true
              i_outros_enderecos  = abap_false
            EXCEPTIONS
              erro_integracao     = 1
              OTHERS              = 2.

          IF sy-subrc = 0.
            lv_quant_motor = lv_quant_motor + 1.
            PERFORM f_atualizar_status   USING 'MOTORISTA' 'C' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_true.
          ELSE.
            PERFORM f_atualizar_status   USING 'MOTORISTA' 'C' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_false.
          ENDIF.
        ENDIF.
      ENDIF.

      PERFORM f_checa_integracao     USING lv_lifnr
                                           'F'
                                           0
                                  CHANGING lv_integrado
                                           lv_integrar.
      IF lv_integrar = abap_true.
        IF lv_integrado = abap_true.
          PERFORM f_atualizar_status   USING 'MOTORISTA' 'F' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_true.
        ELSE.
          CALL FUNCTION 'ZSD_INT_OB_INTEGRA_CONTATOS'
            EXPORTING
              i_parceiro          = lv_lifnr
              i_tp_contato        = 'F'
              i_somente_cadastrar = abap_true
              i_outros_enderecos  = abap_false
            EXCEPTIONS
              erro_integracao     = 1
              OTHERS              = 2.

          IF sy-subrc = 0.
            lv_quant_motor = lv_quant_motor + 1.
            PERFORM f_atualizar_status   USING 'MOTORISTA' 'F' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_true.
          ELSE.
            PERFORM f_atualizar_status   USING 'MOTORISTA' 'F' lv_lifnr 0 0 w_zsdt0082 _zsdt0133 abap_false.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.

************************************************************************
*  Processamento
************************************************************************
FORM f_processamento_cargo_transp.

  CLEAR: lv_lifnr, w_zsdt0082, w_zsdt0133_transp.

  CHECK p_tracki = abap_true.

*------------------------------------------
*-dados transport data
*------------------------------------------
  LOOP AT t_zsdt0133_transp  INTO DATA(_zsdt0133_transp).

    SELECT SINGLE *
      FROM zsdt0129 INTO @DATA(lwa_zsdt0129)
     WHERE nro_cg = @_zsdt0133_transp-nro_cg.

    CHECK sy-subrc EQ 0 AND lwa_zsdt0129-placa_cav IS NOT INITIAL AND lwa_zsdt0129-motorista IS NOT INITIAL.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(lwa_lfa1_motorista)
     WHERE lifnr EQ @lwa_zsdt0129-motorista.

    CHECK sy-subrc EQ 0 AND lwa_lfa1_motorista-telf1 IS NOT INITIAL.

    SELECT SINGLE *
      FROM zsdt0422 INTO @DATA(_zsdt0422)
     WHERE placa     = @lwa_zsdt0129-placa_cav
       AND integrado = @abap_true.

    CHECK sy-subrc EQ 0.

    CALL FUNCTION 'ZSD_INT_OB_INTEGRA_TRACKING'
      EXPORTING
        i_nro_cg        = _zsdt0133_transp-nro_cg
        i_endpoint      = 'TRANSPORT'
      EXCEPTIONS
        erro_integracao = 1
        OTHERS          = 2.

    IF sy-subrc = 0.
      PERFORM f_atualizar_status   USING 'TRANSPORT' lv_lifnr 'F' _zsdt0133_transp-nro_cg 0 w_zsdt0082 _zsdt0133_transp abap_true.
    ELSE.
      PERFORM f_atualizar_status   USING 'TRANSPORT' lv_lifnr 'F' _zsdt0133_transp-nro_cg 0 w_zsdt0082 _zsdt0133_transp abap_false.
    ENDIF.
  ENDLOOP.

ENDFORM.

************************************************************************
*  Processamento
************************************************************************
FORM f_processamento_cargo_veicul.

  CLEAR: lv_lifnr, w_zsdt0082, w_zsdt0133_transp.

  CHECK p_veicul = abap_true.

*------------------------------------------
*-veiculos transport data
*------------------------------------------
  LOOP AT t_zsdt0129_veicul  INTO DATA(_zsdt0129_veicul).

    IF _zsdt0129_veicul-placa_cav IS NOT INITIAL.
      PERFORM f_checa_integra_veic   USING _zsdt0129_veicul-placa_cav
                                  CHANGING lv_integrado.

      IF lv_integrado = abap_true.
        PERFORM f_atualizar_status_veicul     USING _zsdt0129_veicul-nro_lote _zsdt0129_veicul-placa_cav abap_true.
      ELSE.
        CALL FUNCTION 'ZSD_INT_OB_INTEGRA_TRACKING'
          EXPORTING
            i_nro_cg        = _zsdt0129_veicul-nro_cg
            i_endpoint      = 'VEICULO'
          EXCEPTIONS
            erro_integracao = 1
            OTHERS          = 2.

        IF sy-subrc = 0.
          PERFORM f_atualizar_status_veicul   USING _zsdt0129_veicul-nro_lote _zsdt0129_veicul-placa_cav abap_true.
        ELSE.
          PERFORM f_atualizar_status_veicul   USING _zsdt0129_veicul-nro_lote _zsdt0129_veicul-placa_cav abap_false.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.

************************************************************************
*  Processamento
************************************************************************
FORM f_processamento_cargo_anexos.

  CLEAR: lv_lifnr, w_zsdt0082.

  CHECK p_tracki = abap_true.

  LOOP AT t_zsdt0133_anexos  INTO DATA(_zsdt0133_anexos).

    CALL FUNCTION 'ZSD_INT_OB_INTEGRA_TRACKING'
      EXPORTING
        i_nro_cg        = _zsdt0133_anexos-nro_cg
        i_endpoint      = 'ANEXOS'
      EXCEPTIONS
        erro_integracao = 1
        OTHERS          = 2.

    IF sy-subrc = 0.
      PERFORM f_atualizar_anexos   USING _zsdt0133_anexos-nro_cg c_aut_emb abap_true.
      PERFORM f_atualizar_anexos   USING _zsdt0133_anexos-nro_cg c_nfe     abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.

************************************************************************
* atualizar status
************************************************************************
FORM f_atualizar_status_veicul   USING p_nro_lote
                                       p_placa
                                       p_integrado.

  UPDATE zsdt0129 SET int_veiculo_safra_ctrl = p_integrado
                WHERE nro_lote               = p_nro_lote.

  CLEAR w_zsdt0422.

  w_zsdt0422-placa       = p_placa.
  w_zsdt0422-integrado   = p_integrado.
  w_zsdt0422-user_create = sy-uname.
  w_zsdt0422-date_create = sy-datum.
  w_zsdt0422-time_create = sy-uzeit.
  MODIFY zsdt0422     FROM w_zsdt0422.

  COMMIT WORK AND WAIT.

ENDFORM.

FORM f_checa_integra_veic   USING p_placa
                         CHANGING p_integrado.

  p_integrado = abap_false.

  SELECT SINGLE *
    INTO @DATA(_zsdt0422)
    FROM zsdt0422
   WHERE placa     = @p_placa
     AND integrado = @abap_true.

  IF sy-subrc = 0.
    p_integrado = abap_true.
  ENDIF.

ENDFORM.

************************************************************************
* atualizar status
************************************************************************
FORM f_atualizar_anexos   USING p_nro_cg
                                p_tipo_anexo
                                p_integrado.

  SELECT SINGLE *
    INTO w_zsdt0422
    FROM zsdt0422
   WHERE nro_cg            = p_nro_cg
     AND tipo_anexo        = p_tipo_anexo
     AND anexo_gerado      = abap_true
     AND integrado         = abap_false.

  IF sy-subrc = 0.
    w_zsdt0422-integrado   = p_integrado.
    w_zsdt0422-user_create = sy-uname.
    w_zsdt0422-date_create = sy-datum.
    w_zsdt0422-time_create = sy-uzeit.
    MODIFY zsdt0422     FROM w_zsdt0422.
  ELSE.
    RETURN.
  ENDIF.

  CASE p_tipo_anexo.
    WHEN c_aut_emb.
      UPDATE zsdt0133 SET int_anexos_aut_emb_safra_ctrl = p_integrado
                    WHERE nro_cg                        = p_nro_cg.
    WHEN c_nfe.
      UPDATE zsdt0133 SET int_anexos_nfe_safra_ctrl     = p_integrado
                    WHERE nro_cg                        = p_nro_cg.
  ENDCASE.

  COMMIT WORK AND WAIT.

ENDFORM.
************************************************************************
* atualizar status
************************************************************************
FORM f_atualizar_status   USING p_tipo     TYPE char50
                                p_tp_contat
                                p_parceiro
                                p_nro_cg
                                p_nr_rot
                                p_zsdt0082 TYPE zsdt0082
                                p_zsdt0133 TYPE zsdt0133
                       CHANGING p_integrado.

  DATA: lv_external_id TYPE string,
        lv_tipo_anexo  TYPE zsdt0422-tipo_anexo.

  CLEAR: lv_external_id, lv_tipo_anexo.

  CASE p_tipo.
    WHEN 'WERKS'.
      lv_external_id = lc_integra_safra->get_external_id( i_parceiro = p_parceiro i_tipo_parceiro = p_tp_contat ).
      UPDATE zsdt0082 SET integrou_werks              = p_integrado
                    WHERE nro_sol                     = p_zsdt0082-nro_sol
                      AND seq                         = p_zsdt0082-seq
                      AND vbeln                       = p_zsdt0082-vbeln
                      AND posnr                       = p_zsdt0082-posnr.

    WHEN 'VKBUR'.
      lv_external_id = lc_integra_safra->get_external_id( i_parceiro = p_parceiro i_tipo_parceiro = p_tp_contat ).
      UPDATE zsdt0082 SET integrou_vkbur              = p_integrado
                    WHERE nro_sol                     = p_zsdt0082-nro_sol
                      AND seq                         = p_zsdt0082-seq
                      AND vbeln                       = p_zsdt0082-vbeln
                      AND posnr                       = p_zsdt0082-posnr.

    WHEN 'KUNNR'.
      lv_external_id = lc_integra_safra->get_external_id( i_parceiro = p_parceiro i_tipo_parceiro = p_tp_contat ).
      UPDATE zsdt0082 SET integrou_kunnr              = p_integrado
                    WHERE nro_sol                     = p_zsdt0082-nro_sol
                      AND seq                         = p_zsdt0082-seq
                      AND vbeln                       = p_zsdt0082-vbeln
                      AND posnr                       = p_zsdt0082-posnr.

    WHEN 'NR_ROT'.
      UPDATE zsdt0082 SET integrou_nr_rot             = p_integrado
                    WHERE nro_sol                     = p_zsdt0082-nro_sol
                      AND seq                         = p_zsdt0082-seq
                      AND vbeln                       = p_zsdt0082-vbeln
                      AND posnr                       = p_zsdt0082-posnr.

    WHEN 'NR_ROT_PC'.
      UPDATE zsdt0082 SET integrou_nr_rot_pc          = p_integrado
                    WHERE nro_sol                     = p_zsdt0082-nro_sol
                      AND seq                         = p_zsdt0082-seq
                      AND vbeln                       = p_zsdt0082-vbeln
                      AND posnr                       = p_zsdt0082-posnr.

    WHEN 'COD_TRANSPORTADORA'.
      lv_external_id = lc_integra_safra->get_external_id( i_parceiro = p_parceiro i_tipo_parceiro = p_tp_contat ).
      UPDATE zsdt0133 SET int_transp_safra_ctrl       = p_integrado
                    WHERE nro_cg                      = p_zsdt0133-nro_cg.

    WHEN 'MOTORISTA'.
      lv_external_id = lc_integra_safra->get_external_id( i_parceiro = p_parceiro i_tipo_parceiro = p_tp_contat ).
      UPDATE zsdt0133 SET int_mot_safra_ctrl          = p_integrado
                    WHERE nro_cg                      = p_zsdt0133-nro_cg.

    WHEN 'TRANSPORT'.
      lv_tipo_anexo = p_tipo.
      UPDATE zsdt0133 SET int_dados_transp_safra_ctrl = p_integrado
                    WHERE nro_cg                      = p_zsdt0133-nro_cg.
    WHEN 'VEICULO'.
      lv_tipo_anexo = p_tipo.
      UPDATE zsdt0129 SET int_veiculo_safra_ctrl      = p_integrado
                    WHERE nro_lote                    = p_zsdt0133-nro_cg.

*   WHEN 'ANEXOS'.
*     UPDATE zsdt0133 SET int_dados_anexos_safra_ctrl = p_integrado
*                   WHERE nro_cg                      = p_zsdt0133-nro_cg.
  ENDCASE.

  CLEAR w_zsdt0422.

  w_zsdt0422-parceiro    = lv_external_id.
  w_zsdt0422-nro_cg      = p_nro_cg.
  w_zsdt0422-nr_rot      = p_nr_rot.
  w_zsdt0422-tipo_anexo  = lv_tipo_anexo.
  w_zsdt0422-integrado   = p_integrado.
  w_zsdt0422-user_create = sy-uname.
  w_zsdt0422-date_create = sy-datum.
  w_zsdt0422-time_create = sy-uzeit.
  MODIFY zsdt0422     FROM w_zsdt0422.

  COMMIT WORK AND WAIT.

ENDFORM.

************************************************************************
* checa se parceiro foi integrado
************************************************************************
FORM f_checa_integracao   USING p_parceiro
                                p_tp_contat
                                p_nr_rot
                       CHANGING p_integrado
                                p_integrar.

  DATA: lv_external_id TYPE string.

  FREE: p_integrado, p_integrar.

  IF p_parceiro IS NOT INITIAL.
    lv_external_id = lc_integra_safra->get_external_id( EXPORTING i_parceiro      = p_parceiro
                                                                  i_tipo_parceiro = p_tp_contat
                                                        IMPORTING e_integrar      = p_integrar ).

    CHECK p_integrar = abap_true.

    SELECT SINGLE *
      INTO @DATA(_zsdt0422)
      FROM zsdt0422
     WHERE parceiro = @lv_external_id.

    IF sy-subrc = 0 AND _zsdt0422-integrado = abap_true.
      p_integrado = abap_true.
    ENDIF.

  ELSEIF p_nr_rot IS NOT INITIAL.
    SELECT SINGLE *
      INTO @_zsdt0422
      FROM   zsdt0422
     WHERE nr_rot = @p_nr_rot.

    p_integrar    = abap_true.

    IF sy-subrc = 0 AND _zsdt0422-integrado = abap_true.
      p_integrado = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.

************************************************************************
* IMPEIMW DADOS
************************************************************************
FORM f_imprime_dados.

  CHECK p_naojob = abap_false.  "*-CS2025000249-01.09.2025-#189440-JT

  WRITE:/ 'Quantidade de Registros Integrados'.
  ULINE.
  WRITE:/ 'Centros         :', lv_quant_werks.
  WRITE:/ 'Filial          :', lv_quant_vkbur.
  WRITE:/ 'Clientes        :', lv_quant_kunnr.
  WRITE:/ 'Transportadoras :', lv_quant_trans.
  WRITE:/ 'Motoristas      :', lv_quant_motor.
  ULINE.

ENDFORM.

************************************************************************
************************************************************************
