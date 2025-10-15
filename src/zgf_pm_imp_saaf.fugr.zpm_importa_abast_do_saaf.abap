FUNCTION zpm_importa_abast_do_saaf.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  EXPORTING
*"     REFERENCE(EXPORTACAO_COMMIT_SAAF) TYPE  ZTPM_EXP_COMMIT_SAAF_T
*"  TABLES
*"      _ZTPM_IMP_DO_SAAF STRUCTURE  ZTPM_IMP_DO_SAAF
*"----------------------------------------------------------------------
*&                 AMAGGI - Projeto Abaco
*&---------------------------------------------------------------------*
*& Criado por:  José Godoy ( JAP ) - Ábaco Consultores
*& Data      : 18/05/2017
*& Pedido por: Cleudo Ferreira
*& Chamado/Descrição :xx  xxxxxxx - Interface Abastecimento SAAF
*& Request: DEVK970603 - PM 17.05.2017 - Interface SAAF x SAP [  ] - JAP
*&---------------------------------------------------------------------*
*& Histórico de Alterações:                                            *
*&---------------------------------------------------------------------*
*&  Data       | Request    | Autor         | Alteração                *
*&---------------------------------------------------------------------*
*&             |            |               |                          *
*&--------------------------------------------------------------------

* Verifica se tabela de entrada está vazia
  CLEAR et_return[].

  DATA: zimp_do_saaf TYPE TABLE OF ztpm_imp_do_saaf.
  DATA: _i_cod_transa_saaf TYPE ztpm_imp_do_saaf,
        lv_qtd_abastec     TYPE dec_16_02_s, " Rubenilson - 06.01.24 - #158521
        ls_equi            TYPE equi,        " Rubenilson - 06.01.24 - #158521
        lV_equi            TYPE equi-equnr.  " Rubenilson - 06.01.24 - #158521

  IF _ztpm_imp_do_saaf[] IS INITIAL.
    PERFORM zf_inserir_mensagem USING  c_e 025 space space space  space
                               CHANGING et_return.
  ELSE.
    CLEAR lv_ja_apontou.
  ENDIF.

*  SORT _ZTPM_IMP_DO_SAAF ASCENDING BY S_COD_EQUIPAMENTO DT_INICIO_MOV S_HORA_INICIO_MOV.
*
*  LOOP AT _ZTPM_IMP_DO_SAAF ASSIGNING FIELD-SYMBOL(<W_ZTPM_IMP_DO_SAAF>).
*    IF <W_ZTPM_IMP_DO_SAAF>-DT_ESTORNO IS INITIAL.
*      LOOP AT ZIMP_DO_SAAF ASSIGNING FIELD-SYMBOL(<W_ZIMP_DO_SAAF>) WHERE S_COD_EQUIPAMENTO EQ <W_ZTPM_IMP_DO_SAAF>-S_COD_EQUIPAMENTO
*                                                                       AND DT_INICIO_MOV    EQ <W_ZTPM_IMP_DO_SAAF>-DT_INICIO_MOV
*                                                                       AND F_HODOMETRO NE '0' AND F_HODOMETRO EQ <W_ZTPM_IMP_DO_SAAF>-F_HODOMETRO
*                                                                        OR F_HORIMETRO NE '0' AND F_HORIMETRO EQ <W_ZTPM_IMP_DO_SAAF>-F_HORIMETRO.
*
*
*        ADD <W_ZTPM_IMP_DO_SAAF>-F_QTDE_ABAST TO <W_ZIMP_DO_SAAF>-F_QTDE_ABAST.
*      ENDLOOP.
*    ENDIF.
*
*    IF SY-SUBRC EQ 4.
*      APPEND <W_ZTPM_IMP_DO_SAAF> TO ZIMP_DO_SAAF.
*    ENDIF.
*  ENDLOOP.

  LOOP AT _ztpm_imp_do_saaf INTO wa_input.
*  LOOP AT ZIMP_DO_SAAF INTO WA_INPUT.

    REPLACE ALL OCCURRENCES OF ':' IN wa_input-s_hora_inicio_mov   WITH ''.
    REPLACE ALL OCCURRENCES OF ':' IN wa_input-s_hora_fim_mov      WITH ''.
    REPLACE ALL OCCURRENCES OF ':' IN wa_input-s_hora_exportacao   WITH ''.
    REPLACE ALL OCCURRENCES OF ':' IN wa_input-s_hora_leitura_terc WITH ''.

    PERFORM convert_data CHANGING wa_input-dt_inicio_mov.
    PERFORM convert_data CHANGING wa_input-dt_data_fim_mov.
    PERFORM convert_data CHANGING wa_input-dt_exportacao.
    PERFORM convert_data CHANGING wa_input-dt_estorno.
    PERFORM convert_data CHANGING wa_input-dt_leitura_terceiro.

    IF wa_input-i_cod_transa_saaf IS INITIAL.
      PERFORM zf_inserir_mensagem USING c_e 026 space space space space CHANGING et_return.
      p_erro = abap_true.
      CONTINUE.
    ENDIF.

    IF wa_input-dt_estorno IS NOT INITIAL.
      PERFORM estorno USING  wa_input-i_cod_transa_saaf
                             wa_input-i_cod_transa_orig_alteracao.

      IF wa_input-i_cod_transa_orig_alteracao IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

*    Verifica se o movimento ja existe e ja foi importado com sucesso.

    FREE: _i_cod_transa_saaf.
    SELECT SINGLE *
    FROM ztpm_imp_do_saaf
    INTO _i_cod_transa_saaf
      WHERE i_cod_transa_saaf EQ wa_input-i_cod_transa_saaf.
    IF _i_cod_transa_saaf-i_cod_transa_saaf IS NOT INITIAL
      AND _i_cod_transa_saaf-doc_material IS NOT INITIAL
      AND wa_input-dt_estorno IS INITIAL.

      CLEAR ls_return-message.
      ls_return-message = 'Abastecimento ja foi exportado com sucesso.'.
      PERFORM zf_inserir_mensagem USING c_e 0 space space ls_return-message  space
                                  CHANGING et_return.
      p_erro = 'X'.
      CONTINUE.
    ENDIF.

* Verifica tipo de registro
    CASE wa_input-i_tipo_registro.
      WHEN c_2 OR c_3.
* Executando processo de transferencia de estoque ou aferição.
        IF wa_input-i_cod_planta_transa EQ wa_input-s_cod_ponto_abast.
          wa_input-s_id_tanque_origem = wa_input-s_cod_ponto_abast_sap.
        ENDIF.

        IF wa_input-s_id_tanque_destino IS NOT INITIAL.
          wa_input-s_id_tanque_destino = |{ wa_input-s_id_tanque_destino ALPHA = IN }|.
          SELECT SINGLE *
          FROM zpmt006
          INTO w_zpmt006
            WHERE id_tq EQ wa_input-s_id_tanque_destino.

          IF w_zpmt006 IS NOT INITIAL.
            wa_input-s_id_tanque_destino = w_zpmt006-id_deposito.
            wa_input-s_id_tanque_destino = |{ wa_input-s_id_tanque_destino ALPHA = OUT }|.
          ENDIF.
        ENDIF.

        wa_input-s_id_tanque_destino = |{ wa_input-s_id_tanque_destino ALPHA = OUT }|.
        PERFORM zf_transfere_tanque.
        IF NOT p_erro = abap_true.
          MODIFY ztpm_imp_do_saaf FROM wa_input.
          COMMIT WORK.
        ENDIF.

* Só faz a transferência
        CONTINUE.

      WHEN OTHERS.

**&--------Inicio / aoenning---& ajuste IR190960 ZPM_IMPORTA_ABAST_DO_SAAF
* Processo de baixa de consumo de combustivel realizada pelos equipamentos.
*        IF WA_INPUT-I_COD_PLANTA_TRANSA EQ WA_INPUT-S_COD_PONTO_ABAST.
*          WA_INPUT-S_COD_PONTO_ABAST = WA_INPUT-S_COD_PONTO_ABAST_SAP.
*        ELSE.
*          WA_INPUT-S_COD_PONTO_ABAST = WA_INPUT-S_ID_TANQUE_ORIGEM.
*        ENDIF.

*** Inicio - Rubenilson - 06.01.24 - US158521

        lv_equi = wa_input-s_cod_equipamento.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_equi
          IMPORTING
            output = lv_equi.

        CONDENSE lv_equi NO-GAPS.

        SELECT SINGLE *
          FROM equi
          INTO ls_equi
          WHERE equnr = lv_equi.

        IF sy-subrc = 0.

          IF (  ls_equi-eqtyp EQ '1'
             OR ls_equi-eqtyp EQ '2'
             OR ls_equi-eqtyp EQ '3'
             OR ls_equi-eqtyp EQ '4'
             OR ls_equi-eqtyp EQ 'A' ).

            DATA(lo_pm_data_equipament) = NEW zcl_pm_data_equipament( ).

            lv_qtd_abastec = wa_input-f_qtde_abast.
            lo_pm_data_equipament->zif_pm_data_equipament~valida_capacidade_combustivel(

              EXPORTING
                i_cod_classe           = ls_equi-eqart
                i_fabricante           = ls_equi-herst
                i_modelo               = ls_equi-typbz
                i_qtd_abastec          = lv_qtd_abastec
              IMPORTING
                e_tq_comb              = DATA(lv_tq_comb)
                e_dentro_da_tolerancia = DATA(lv_dentro_tolerancia) ).

            IF lv_dentro_tolerancia <> abap_true.
              PERFORM zf_inserir_mensagem USING c_e 043 space space space space CHANGING et_return.
              p_erro = abap_true.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
*** Fim - Rubenilson - 06.01.24 - US158521

        IF wa_input-s_cod_ponto_abast_sap IS NOT INITIAL.
          wa_input-s_cod_ponto_abast = wa_input-s_cod_ponto_abast_sap.
        ENDIF.
**&--------Fim ajuste IR190960 / aoenning---&

        IF wa_input-s_cod_equipamento IS INITIAL.
          PERFORM zf_inserir_mensagem USING c_e 027 space space space space CHANGING et_return.
          p_erro = abap_true.
          CONTINUE.
        ELSE.
          v_equip  = wa_input-s_cod_equipamento.
*          Validar informações do equipamento.
          PERFORM zf_verifica_equipamento.
        ENDIF.

    ENDCASE.

* Se registro estiver ok, será inserido na tabela.
    IF NOT p_erro = abap_true.
      FREE: t_doc_med.
      PERFORM zf_save.
      IF NOT p_erro = abap_true.
        MODIFY ztpm_imp_do_saaf FROM wa_input.
        COMMIT WORK.
      ENDIF.

    ELSE.
      CLEAR p_erro.
    ENDIF.

  ENDLOOP.

  APPEND LINES OF et_return[] TO it_return.

  SORT it_return.
  DELETE ADJACENT DUPLICATES FROM it_return COMPARING ALL FIELDS.

  it_erro = VALUE #( FOR ls IN it_return WHERE ( type = 'E' ) ( CORRESPONDING #( ls ) ) ).
  it_sucess = VALUE #( FOR ls IN it_return WHERE ( type <> 'E' ) ( CORRESPONDING #( ls ) ) ).

  SORT it_sucess BY field.
  DELETE ADJACENT DUPLICATES FROM it_sucess COMPARING field.

  LOOP AT it_sucess ASSIGNING FIELD-SYMBOL(<sucess>).

    CASE <sucess>-type.
      WHEN 'W'. <sucess>-message = |{ <sucess>-message } Processado com Avisos!|.
      WHEN 'S'. <sucess>-message = 'Processado com Sucesso!'. <sucess>-number = 0. "COND #( WHEN WA_INPUT-DT_ESTORNO IS INITIAL THEN 0 ELSE <SUCESS>-NUMBER ).
      WHEN OTHERS.
        <sucess>-message = 'Msg não Tratada Devidamente!'.
    ENDCASE.

  ENDLOOP.

  LOOP AT it_erro INTO DATA(wa_erro).
    IF line_exists( it_sucess[ field = wa_erro-field type = 'S' ] ).
      DELETE it_sucess WHERE field = wa_erro-field AND type = 'S'.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF it_erro TO it_sucess.

  IF it_sucess IS NOT INITIAL.

    CREATE OBJECT obj_imp.
    FREE exportacao_commit_saaf.

    exportacao_commit_saaf =  VALUE #( (
    m_transa_exp_terceiro = VALUE #( FOR ls1 IN  it_sucess (
    i_cod_transa_saaf   = obj_imp->get_msg( input = ls1-field  tabix = 1 )
    i_cod_d_transa_saaf = obj_imp->get_msg( input = ls1-field  tabix = 2 )
    i_cod_planta_transa = obj_imp->get_msg( input = ls1-field  tabix = 3 )
    i_cod_retorno_erp   = ls1-number
    s_desc_retorno_erp  = ls1-message
    )
    )
    ) ).
  ENDIF.

  FREE _ztpm_imp_do_saaf[].

ENDFUNCTION.
