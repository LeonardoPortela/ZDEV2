*----------------------------------------------------------------------*
***INCLUDE MZMEMORANDO_5051 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_5050 INPUT.

  CASE ok_code.
    WHEN c_apcone.
      CLEAR: ok_code.
      PERFORM pesquisa_expot_acomp_e.
    WHEN c_csnfmo.
      CLEAR: ok_code.
      PERFORM visualiza_notas USING c_x.
    WHEN c_csnfme.
      CLEAR: ok_code.
      PERFORM visualiza_notas USING space.
    WHEN c_lancne.
      CLEAR: ok_code.
      PERFORM lanca_nota_acomp_e.
    WHEN c_back OR c_exit OR c_cancel.
      CLEAR: ok_code.
      LEAVE PROGRAM.
    WHEN c_bcgrd.
      CLEAR: ok_code.
      PERFORM back_ground.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_5050  INPUT

*&---------------------------------------------------------------------*
*&      Form  PESQUISA_EXPOT_ACOMP_E
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pesquisa_expot_acomp_e .

  CLEAR: it_exportacoes[], it_notas[], it_export_acomp[].

  vg_quantidade = 0.
  vg_quantcompe = 0.
  vg_quantacomp = 0.

  p_direcao_1 = 1.

  CALL FUNCTION 'Z_EXPORT_TERCEIRO_ACOMP'
    EXPORTING
      p_bukrs          = t_empree
      p_werks          = t_centre
      p_matnr          = t_produe
      t_export         = t_remetp[]
      t_period         = t_perioe[]
      exportadores     = c_x
      notas_exportacao = c_x
      p_direcao        = '1'
    TABLES
      it_exportacoes   = it_exportacoes
      it_notas         = it_notas
    EXCEPTIONS
      cfops_saida      = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF it_notas[] IS NOT INITIAL.

    DATA: r_status       TYPE RANGE OF zsdt0276-status,
          l_qde_baixada  TYPE zsdt0276-menge,
          l_qde_baixada2 TYPE zsdt0276-menge.

    r_status = VALUE #( ( sign = 'I' option = 'EQ' low = '' )
                        ( sign = 'I' option = 'EQ' low = 'A' ) ).

    " Baixa de Notas Fiscais de Compra
    SELECT * FROM zsdt0276
      INTO TABLE @DATA(it_zsdt0276)
      FOR ALL ENTRIES IN @it_notas
      WHERE docnum = @it_notas-docnum
        AND itmnum = @it_notas-itmnum
        AND status IN @r_status.

  ENDIF.

  LOOP AT it_exportacoes ASSIGNING FIELD-SYMBOL(<fs_exportacoes>).

    CLEAR l_qde_baixada2.
    LOOP AT it_notas
        ASSIGNING FIELD-SYMBOL(<fs_notas>)  WHERE bukrs     =  <fs_exportacoes>-bukrs
                                              AND werks      = <fs_exportacoes>-werks
                                              AND exportador = <fs_exportacoes>-exportador
                                              AND produto    = <fs_exportacoes>-produto
                                              AND unidade    = <fs_exportacoes>-unidade.

      CLEAR l_qde_baixada.
      LOOP AT it_zsdt0276 INTO DATA(wa_zsdt0276) WHERE docnum = <fs_notas>-docnum
                                                   AND itmnum = <fs_notas>-itmnum.
        l_qde_baixada2 = l_qde_baixada = l_qde_baixada + wa_zsdt0276-menge.
      ENDLOOP.

*---------Planejar---------------------------------------------
      IF <fs_notas>-quantaplan > '0.000'.
        <fs_notas>-quantaplan = <fs_notas>-quantaplan - l_qde_baixada2.
      ENDIF.

      IF <fs_notas>-quantaplan < '0.000'.
        <fs_notas>-quantaplan =   '0.000'.
      ENDIF.

*---------SAldo---------------------------------------------
      IF <fs_notas>-saldo > '0.000'.
        <fs_notas>-saldo = <fs_notas>-saldo - l_qde_baixada2.
      ENDIF.

      IF <fs_notas>-saldo < '0.000'.
        <fs_notas>-saldo = '0.000'.
      ENDIF.

      <fs_notas>-qtde_baixadas = l_qde_baixada.

    ENDLOOP.

    IF <fs_exportacoes>-quantplane > '0.000'.
      <fs_exportacoes>-quantplane = <fs_exportacoes>-quantplane - l_qde_baixada2.
    ENDIF.

    IF <fs_exportacoes>-quantacomp > '0.000'.
      <fs_exportacoes>-quantacomp = <fs_exportacoes>-quantacomp - l_qde_baixada2.
    ENDIF.

    IF <fs_exportacoes>-quantplane < '0.000'.
      <fs_exportacoes>-quantplane =   '0.000'.
    ENDIF.

    IF <fs_exportacoes>-quantacomp < '0.000'.
      <fs_exportacoes>-quantacomp = '0.000'.
    ENDIF.

    <fs_exportacoes>-qtde_baixadas = l_qde_baixada2.
  ENDLOOP.


  SORT  it_notas BY dt_emissao ASCENDING quantaplan.
  it_notas3[] = it_notas[].
  DELETE it_notas3 WHERE quantaplan LE 0.
  LOOP AT it_exportacoes INTO wa_exportacoes.

    vg_quantidade = vg_quantidade + wa_exportacoes-quantidade.
    vg_quantcompe = vg_quantcompe + wa_exportacoes-quantcompe.
    vg_quantacomp = vg_quantacomp + wa_exportacoes-quantacomp.

    CLEAR wa_export_acomp-dt_emissao.
    "Pega a data da primeira nota com  saldo
    LOOP AT it_notas3 INTO wa_notas WHERE bukrs     = wa_exportacoes-bukrs
                                     AND werks      = wa_exportacoes-werks
                                     AND exportador = wa_exportacoes-exportador
                                     AND produto    = wa_exportacoes-produto
                                     AND unidade    = wa_exportacoes-unidade.
      IF wa_notas-quantaplan GT 0.
        wa_export_acomp-dt_emissao = wa_notas-dt_emissao.
        EXIT.
      ENDIF.
    ENDLOOP.

    MOVE-CORRESPONDING wa_exportacoes TO wa_export_acomp.
    APPEND wa_export_acomp TO it_export_acomp.
  ENDLOOP.

  DELETE it_export_acomp WHERE quantidade = 1.
  DELETE it_notas        WHERE quantidade = 1.
  REFRESH it_notas3.
ENDFORM.                    " PESQUISA_EXPOT_ACOMP_E

*&---------------------------------------------------------------------*
*&      Form  LANCA_NOTA_ACOMP_E
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM lanca_nota_acomp_e .

  CLEAR terceiro.
  DATA: vg_verifica_selecao TYPE sy-subrc.

  PERFORM verifica_selecao_acomp USING vg_verifica_selecao.

  IF vg_verifica_selecao EQ 0.
    PERFORM lancar_memorandos_e.
    zdoc_memo_nf_exp-material = wa_export_acomp-produto.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_export_acomp-werks
      IMPORTING
        output = zdoc_memorando-representante.
    zdoc_memorando-remetente     = wa_export_acomp-exportador.
    terceiro = ''.
    zdoc_memorando-direcao       = c_1.
  ENDIF.

ENDFORM.                    " LANCA_NOTA_ACOMP_E

*&---------------------------------------------------------------------*
*&      Form  LANCAR_MEMORANDOS_E
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM lancar_memorandos_e .

  DATA: wa_setleaf TYPE setleaf.

  "Opter Área de contabilidade de custos
  SELECT SINGLE * INTO wa_setleaf
    FROM setleaf
   WHERE setname EQ 'ZMEMORANDO_KOKRS'.

  IF sy-subrc NE 0.
    MESSAGE e022 DISPLAY LIKE c_s.
  ENDIF.

  "Opter Centro de custo
  SELECT SINGLE * INTO wa_setleaf
    FROM setleaf
   WHERE setname EQ 'ZMEMORANDO_KOSTL'.

  IF sy-subrc NE 0.
    MESSAGE e023 DISPLAY LIKE c_s.
  ENDIF.

  vg_dynnr_ant = vg_dynnr_000.
  vg_dynnr_000 = c_2000.
  vg_dynnr_tab = c_2003.
  vg_novo_lanc = c_x.
  CLEAR: vg_consul_memo, zdoc_memorando, zdoc_memo_nf_exp, it_memorandos[], it_memorando_tela[].

ENDFORM.                    " LANCAR_MEMORANDOS_E
*&---------------------------------------------------------------------*
*&      Form  BACK_GROUND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM back_ground .

  DATA: p_jobn(32),
        v_job_id   LIKE tbtcjob-jobcount,
        v_jobd     LIKE sy-datum,
        v_jobt     LIKE sy-uzeit.

  p_jobn = |ZMEMO03-{ sy-datum }-{ sy-uzeit }|.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = p_jobn
    IMPORTING
      jobcount         = v_job_id
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  SUBMIT zsdr0097 WITH t_empree EQ t_empree
                  WITH t_centre EQ t_centre
                  WITH t_produe EQ t_produe
                  WITH t_perioe IN t_perioe
                  WITH t_remetp IN t_remetp
                     VIA JOB p_jobn
                      NUMBER v_job_id
                  AND RETURN.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = v_job_id
      jobname              = p_jobn
      sdlstrtdt            = sy-datum
      sdlstrttm            = sy-uzeit
      strtimmed            = 'X'
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      OTHERS               = 8.

  MESSAGE |Escalonamento de job para programa { p_jobn } criado!| TYPE 'S'.

ENDFORM.
